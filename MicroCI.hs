{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Config
import Config (Config)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Exception (try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), withArray, eitherDecodeStrict)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Dhall
import GitHub.Data
import GitHub.Endpoints.Repos.Status
import Network.Wai.Handler.Warp as Warp
import Servant
import Servant.GitHub.Webhook
import Servant.Server
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import qualified System.Process as Process


-- Job

data Job = Job
  { jobRepo :: Repo
  , jobCommit :: Text.Text
  , jobAttr :: AttrPath
  }


doJob :: Config -> Job -> IO ()
doJob config job = do
  ensureRepository config (jobRepo job)

  checkoutRef config (jobRepo job) (jobCommit job)

  buildRes <-
    buildAttribute config (jobRepo job) (jobAttr job)

  case buildRes of
    Left e ->
      putStrLn e

    Right _ ->
      return ()

  createStatus
    (OAuth (fromString $ LT.unpack $ Config.oauth config))
    (simpleOwnerLogin $ repoOwner (jobRepo job))
    (repoName (jobRepo job))
    (mkName (Proxy @Commit)
            (jobCommit job))
    NewStatus { newStatusState =
                  case buildRes of
                    Right{} ->
                      Success

                    Left{} ->
                      Failure
              , newStatusTargetUrl = Nothing
              , newStatusDescription = Just $
                  case buildRes of
                    Right{}->
                      "Evaluation successful"

                    Left e ->
                      fromString e
              , newStatusContext =
                  "ci.nix: " <> fromString (encodeAttrPath (jobAttr job))
              }

  return ()


-- buildAttribute

buildAttribute :: Config -> Repo -> AttrPath -> IO (Either String ())
buildAttribute config repo path = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode
      (inGitRepository config repo
         (Process.proc "nix-build"
            ["ci.nix"
            , "-A"
            , encodeAttrPath path
            ]))
      ""

  case exitCode of
    ExitSuccess ->
      return (Right ())

    ExitFailure{} ->
      return (Left $ stdout ++ stderr) 
    


-- findJobAttrPaths


findJobAttrPaths :: Config -> Repo -> IO [AttrPath]
findJobAttrPaths config repo = do
  (exitCode, jobs, stderr) <-
    readCreateProcessWithExitCode
      (inGitRepository config repo
         (Process.proc
            "nix-instantiate"
            [ "--eval"
            , "--strict"
            , "--json"
            , "-E"
            , "import /home/ollie/work/micro-ci/test-repo/jobs.nix (import ./ci.nix)"
            ]))
      ""

  unless (exitCode == ExitSuccess) $
    fail $ unlines
      [ "Could not identify jobs in ci.nix:"
      , ""
      , stderr
      ]

  case eitherDecodeStrict (fromString jobs) of
    Left e ->
      fail $ unlines $
        [ "Could not parse jobs JSON"
        , ""
        , "Aeson reported:"
        , ""
        , show e
        , ""
        , "I'm trying to parse:"
        , ""
        , jobs
        ] 

    Right paths ->
      return paths


  
-- repoDir


repoDir :: Config -> Repo -> FilePath
repoDir config repo =
  LT.unpack (Config.repoRoot config)
    </> Text.unpack (untagName (simpleOwnerLogin (repoOwner repo)))
    </> Text.unpack (untagName (repoName repo))



-- ensureRepository

-- | Ensure that a repository has been cloned into the repoRoot directory, and that
-- it is up-to-date.

ensureRepository :: Config -> Repo -> IO ()
ensureRepository cfg repo = do 
  let
    dir =
      repoDir cfg repo
  
  exists <-
    doesDirectoryExist dir

  if exists then 
    void $
      readCreateProcess
        (inGitRepository cfg repo 
           (proc "git" [ "fetch" ]))
        ""
  else do 
    URL cloneUrl <-
      maybe
        (fail (Text.unpack (untagName (repoName repo)) ++ " does not have a clone URL."))
        return
        (repoCloneUrl repo)

    void $ readCreateProcess (proc "git" [ "clone", Text.unpack cloneUrl, dir ]) ""



checkoutRef :: Config -> Repo -> Text.Text -> IO ()
checkoutRef config repo ref =
  void $
    readCreateProcess
      (inGitRepository config repo 
         (proc "git" [ "checkout", Text.unpack ref ]))
      ""


-- inGitRepository


inGitRepository :: Config -> Repo -> CreateProcess -> CreateProcess
inGitRepository config repo a =
  a { cwd = Just (repoDir config repo) }



-- HttpApi
  

type HttpApi =
  "github"
    :> "web-hook"
    :> GitHubEvent '[ 'WebhookPullRequestEvent ]
    :> GitHubSignedReqBody '[JSON] PullRequestEvent
    :> Post '[JSON] ()


httpEndpoints :: TQueue PullRequestCommit -> Server HttpApi
httpEndpoints =
  gitHubWebHookHandler



-- AttrPath
  

-- | An attribute path.
newtype AttrPath =
  AttrPath [String]
  deriving (FromJSON)


encodeAttrPath :: AttrPath -> String
encodeAttrPath (AttrPath parts) =
  intercalate "." parts


-- gitHubWebHookHandler


-- | Top-level GitHub web hook handler. Ensures that a build is scheduled.

gitHubWebHookHandler :: TQueue PullRequestCommit -> RepoWebhookEvent -> ((), PullRequestEvent) -> Handler ()
gitHubWebHookHandler queue WebhookPullRequestEvent ((), ev) = do
  liftIO $ atomically $
    writeTQueue queue (pullRequestHead $ pullRequestEventPullRequest ev)

gitHubWebHookHandler _ _ _ =
  return ()



-- processPullRequest


processPullRequest :: Config -> TQueue Job -> PullRequestCommit ->  IO ()
processPullRequest config jobQueue pr = do
  let
    headRepo =
      pullRequestCommitRepo pr

  ensureRepository config headRepo

  checkoutRef config headRepo (pullRequestCommitSha pr)

  paths <-
    findJobAttrPaths config headRepo

  for_ paths $ \path ->
    atomically $
    writeTQueue
      jobQueue
      Job
        { jobRepo = headRepo
        , jobCommit = pullRequestCommitSha pr
        , jobAttr = path
        }



-- main


main :: IO ()
main = do
  config <-
    LT.readFile "config.dhall"
      >>= Dhall.input Dhall.auto
  
  jobQueue <-
    newTQueueIO

  prQueue <-
    newTQueueIO

  forkIO $ forever $ do
    job <-
      atomically $ fmap Left (readTQueue prQueue) <|> fmap Right (readTQueue jobQueue)

    --try $
    case job of
        Left pr ->
          processPullRequest config jobQueue pr

        Right job ->
          doJob config job
  
  Warp.run 8080
    (serveWithContext
       (Proxy @HttpApi)
       (gitHubKey (return (fromString $ LT.unpack $ Config.secret config)) :. EmptyContext)
       (httpEndpoints prQueue))
