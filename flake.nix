{
  description = "HazardHunter";
  nixConfig.bash-prompt = "[nix(HazardHunter)] ";

  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/ba5d181089900f376f765e4a6889bd30c4f96993";
    flake-utils.url = "github:numtide/flake-utils";
    butler.url =
      "github:TristanCacqueray/haskell-butler/acb47a5b2b3d913ec24439514f43baa0fbfde130";
  };

  outputs = { self, hspkgs, flake-utils, butler }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = hspkgs.pkgs;

        packageName = "HazardHunter";

        haskellExtend = hpFinal: hpPrev: {
          appPackage = hpPrev.callCabal2nix packageName self { };
          butler = pkgs.haskell.lib.dontHaddock hpPrev.butler;
        };

        hsPkgs = (pkgs.hspkgs.extend butler.haskellExtend).extend haskellExtend;

        appExe = pkgs.haskell.lib.justStaticExecutables hsPkgs.appPackage;

      in {
        defaultExe = appExe;
        defaultPackage = hsPkgs.appPackage;

        devShell = hsPkgs.shellFor {
          packages = p: [ p.appPackage ];

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
