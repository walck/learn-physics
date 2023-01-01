{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=haskell-updates";
    devenv.url = "github:cachix/devenv";
    nix-filter.url = "github:numtide/nix-filter";
    TypeCompose.url = "github:conal/TypeCompose?ref=master";
    TypeCompose.flake = false;
  };

  outputs = { self, nixpkgs, devenv, nix-filter, ... }@inputs:
    with nix-filter.lib;
    let
      systems = [
        "x86_64-linux"
        # "i686-linux"
        "x86_64-darwin"
        # "aarch64-linux"
        # "aarch64-darwin"
      ];
      config = { allowBroken = true; };
      overlays.default = final: previous: {
        haskellPackages = with final.haskell.lib;
          previous.haskellPackages.extend (hfinal: hprevious:
            with hfinal; {
              learn-physics = disableOptimization (dontHaddock
                (callCabal2nix "learn-physics" (filter {
                  root = self;
                  exclude = [ (matchExt "cabal") ];
                }) { }));
              TypeCompose = callCabal2nix "TypeCompose"
                (filter { root = inputs.TypeCompose; }) { };
            });
      };
      forAllSystems = f:
        builtins.listToAttrs (map (name: {
          inherit name;
          value = f name;
        }) systems);
    in {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit config system;
            overlays = [ overlays.default ];
          };
        in { default = pkgs.haskellPackages.learn-physics; });
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit config system;
            overlays = [ overlays.default ];
          };
        in {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = with pkgs.haskellPackages;
              with pkgs; [{
                env = { name = "learn-physics"; };
                enterShell = ''
                  setUp
                '';
                packages = [
                  ghcid
                  gnuplot
                  (ghcWithPackages
                    (p: with p; [ learn-physics haskell-language-server ]))
                ];
                pre-commit.hooks = { nixfmt.enable = true; };
                scripts = {
                  setUp.exec = ''
                    if [ -f package.yaml ]
                    then
                      ${hpack}/bin/hpack -f package.yaml
                    fi
                    if [ -f learn-physics.cabal ]
                    then
                      ${implicit-hie}/bin/gen-hie --cabal &> hie.yaml
                    fi
                  '';
                };
              }];
          };
        });
    };
}
