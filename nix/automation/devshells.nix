{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
in {
  default = nixpkgs.mkShell {
    EMACS_SRC = "${cell.packages.emacsSrcWithModule}";
    packages = [
      cell.packages.eldev
      nixpkgs.alejandra
      nixpkgs.gnulib
      nixpkgs.statix
    ];
    shellHook = ''
      export ELDEV_DIR=$PWD/.eldev
    '';
  };
}
