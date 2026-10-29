{
    description = "Vim like search for GNU Emacs";

    inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    outputs = { self, nixpkgs }:
    let
        systems = [
            "x86_64-linux"
            "aarch64-linux"
        ];

        forAllSystems = nixpkgs.lib.genAttrs systems;

    in {
        packages = forAllSystems (system:
        let
            pkgs = nixpkgs.legacyPackages.${system};
        in {
            default = pkgs.emacsPackages.trivialBuild {
                pname   = "nj-search";
                version = "0.1.0";
                src     = self;
            };
        }
        );
    };
}
