{
  description = "HazardHunter";
  nixConfig.bash-prompt = "[nix(HazardHunter)] ";

  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/f6893161b29a8086c7e1232c886a99a5d815ffae";
    flake-utils.url = "github:numtide/flake-utils";
    butler.url =
      "github:TristanCacqueray/haskell-butler/16e6ffc28ede3191807b4207362cdaa86972b89c";
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

        containerHome = "var/lib/${packageName}";
        mkContainerHome = "mkdir -p -m 744 ${containerHome}";
        container = pkgs.dockerTools.buildLayeredImage {
          name = "localhost/hazard-hunter";
          contents = [ pkgs.coreutils pkgs.bash pkgs.openssl appExe ];
          extraCommands = "${mkContainerHome} && mkdir -p -m 777 tmp";
          tag = "latest";
          created = "now";
          config = {
            Env = [
              "HOME=/${containerHome}"
              # Use fakeroot to avoid `No user exists for uid` error
              "LD_PRELOAD=${pkgs.fakeroot}/lib/libfakeroot.so"
            ];
          };
        };

      in {
        defaultExe = appExe;
        defaultPackage = hsPkgs.appPackage;
        packages.container = container;
        devShell = hsPkgs.shellFor {
          packages = p: [ p.appPackage ];

          buildInputs = with pkgs; [
            ghcid
            fourmolu
            cabal-install
            hlint
            haskell-language-server
          ];
        };
      });
}
