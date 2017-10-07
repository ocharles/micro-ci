pkgs:
{
  overrides =
    self: super: {
      github = self.callPackage /home/ollie/work/github {};
      servant-github-webhook = pkgs.haskell.lib.doJailbreak super.servant-github-webhook;  
    };
}
