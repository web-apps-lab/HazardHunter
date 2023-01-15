{
  description = "HazardHunter";
  nixConfig.bash-prompt = "[nix(HazardHunter)] ";

  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/ba5d181089900f376f765e4a6889bd30c4f96993";
    flake-utils.url = "github:numtide/flake-utils";
    butler.url =
      "github:TristanCacqueray/haskell-butler/c0b75e7825dda37e50568fceda7852629fa2a98f";
  };

  outputs = { self, hspkgs, flake-utils, butler }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = hspkgs.pkgs;

        packageName = "HazardHunter";

        haskellExtend = hpFinal: hpPrev: {
          appPackage = hpPrev.callCabal2nix packageName self { };
          butlerPackage = hpPrev.callCabal2nix "butler" butler { };
          ebml = hpPrev.callCabal2nix "ebml" (pkgs.fetchFromGitHub {
            owner = "TristanCacqueray";
            repo = "haskell-ebml";
            rev = "aff25512b52e48e92d77cd59019a0291a8b43bf4";
            sha256 = "sha256-U2Mo83gr7dLm+rRKOLzS9LZUaZ90ECO6Zjbv6maflyc=";
          }) { };
        };

        hsPkgs = pkgs.hspkgs.extend haskellExtend;

        appExe = pkgs.haskell.lib.justStaticExecutables hsPkgs.takoyaki;

      in {
        defaultExe = appExe;
        defaultPackage = hsPkgs.appPackage;

        devShell = hsPkgs.shellFor {
          packages = p: [ p.appPackage p.butlerPackage ];

          buildInputs = with pkgs; [
            ghcid
            ormolu
            cabal-install
            hlint
            haskell-language-server
          ];
        };
      });
}
