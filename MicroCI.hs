{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Paths_micro_ci

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
import Data.Traversable
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

  createStatus
    (OAuth (fromString $ LT.unpack $ Config.oauth config))
    (simpleOwnerLogin $ repoOwner (jobRepo job))
    (repoName (jobRepo job))
    (mkName (Proxy @Commit)
            (jobCommit job))
    NewStatus { newStatusState =
                  if buildSuccess buildRes then
                    Success
                  else
                    Failure
              , newStatusTargetUrl =
                  Just $ URL $ Text.pack $
                  LT.unpack (Config.httpRoot config) ++ "/" ++ takeFileName (buildDerivation buildRes)
              , newStatusDescription =
                  Just $
                  if buildSuccess buildRes then
                    "nix-build successful"
                  else
                    "nix-build failed"
              , newStatusContext =
                  "ci.nix: " <> fromString (encodeAttrPath (jobAttr job))
              }

  return ()


-- buildAttribute

data BuildResult = BuildResult
  { buildSuccess :: Bool
  , buildDerivation :: String
  }


buildAttribute :: Config -> Repo -> AttrPath -> IO BuildResult
buildAttribute config repo path = do
  drv <- do
    (exitCode, stdout, stderr) <-
      readCreateProcessWithExitCode
        (inGitRepository config repo
           (Process.proc "nix-instantiate"
              ["ci.nix"
              , "-A"
              , encodeAttrPath path
              ]))
        ""

    case lines stdout of
      [drv] ->
        return drv

      _ ->
        fail $ unlines $
        [ "Colud not find .drv from nix-instantiate ci.nix:"
        , ""
        , stdout
        , stderr
        ]

  steps <- do 
    (exitCode, stdout, stderr) <-
      readCreateProcessWithExitCode
        (Process.proc "nix-store" [ "-qR", drv ])
        ""

    when (exitCode /= ExitSuccess)
      (fail stderr)

    return (lines stdout)

  createDirectoryIfMissing
    True
    (LT.unpack $ Config.logs config)

  exitCodes <-
    for steps $ \drv -> do
      putStrLn $ "Realise " ++ drv
    
      (exitCode, stdout, stderr) <-
        readCreateProcessWithExitCode
          (inGitRepository config repo
             (Process.proc "nix-store" [ "--realise", drv ]))
          "" 
  
      writeFile (LT.unpack (Config.logs config) </> takeFileName drv <.> "stdout") stdout
  
      writeFile (LT.unpack (Config.logs config) </> takeFileName drv <.> "stderr") stderr

      return exitCode

  return BuildResult
    { buildSuccess = all (== ExitSuccess) exitCodes
    , buildDerivation = drv
    }



-- findJobAttrPaths


findJobAttrPaths :: Config -> Repo -> IO [AttrPath]
findJobAttrPaths config repo = do
  jobsNixPath <-
    getDataFileName "jobs.nix"
    
  (exitCode, jobs, stderr) <-
    readCreateProcessWithExitCode
      (inGitRepository config repo
         (Process.proc
            "nix-instantiate"
            [ "--eval"
            , "--strict"
            , "--json"
            , "-E"
            , "import " ++ jobsNixPath ++ " (import ./ci.nix)"
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
  :<|>
  Capture "drv" Text.Text
    :> Get '[PlainText] Text.Text


httpEndpoints :: TQueue PullRequestCommit -> Config -> Server HttpApi
httpEndpoints q config =
  gitHubWebHookHandler q :<|> detailsHandler config


  
-- detailsHandler


detailsHandler :: Config-> Text.Text -> Handler Text.Text
detailsHandler config drvName = do
  stdout <-
    liftIO
      $ readFile (LT.unpack (Config.logs config) </> Text.unpack drvName <.> "stdout")

  stderr <-
    liftIO
      $ readFile (LT.unpack (Config.logs config) </> Text.unpack drvName <.> "stderr")

  return (Text.pack $ unlines [ stdout, "", stderr ])



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
       (httpEndpoints prQueue config))
