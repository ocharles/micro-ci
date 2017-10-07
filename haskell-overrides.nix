pkgs:
{
  overrides =
    self: super: {
      servant-github-webhook = pkgs.haskell.lib.doJailbreak super.servant-github-webhook;  

      github = pkgs.haskell.lib.overrideCabal super.github (drv: {
        src = pkgs.fetchgit {
          url = "git://github.com/ocharles/github";
          rev = "8312ba0b2a90823a2548cab4f112726933f8072a";
          sha256 = "1k02hpls2cwrhp534w193g1ffzlsk3dlv42w3m6nf9x9xbnfvam4";
        };
        jailbreak = true;
      });
    }; 
}
