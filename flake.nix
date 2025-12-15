{
  description = "cl-mcp - Model Context Protocol server for Common Lisp";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/release-25.11";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];
      perSystem = { pkgs, ... }:
        let
          lisp = pkgs.sbcl;
        in {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              rlwrap
              lisp
            ];

            shellHook = ''
              export CL_SOURCE_REGISTRY="$PWD//:$CL_SOURCE_REGISTRY"
            '';
          };
        };
    };
}
