{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
in {
  eldev = nixpkgs.stdenv.mkDerivation {
    name = "eldev";
    src = inputs.eldev;
    dontUnpack = true;
    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;
    nativeBuildInputs = [nixpkgs.emacs];
    installPhase = ''
      mkdir -p $out/bin
      cp $src/bin/eldev $out/bin/
    '';
  };

  emacsSrcWithModule =
    nixpkgs.runCommand "emacs-src-with-module" {
      src = inputs.emacsSrc;
      nativeBuildInputs = [nixpkgs.autoconf nixpkgs.gcc nixpkgs.ncurses];
    } ''
      cp -r $src/* .
      chmod -R +w .
      ./autogen.sh
      ./configure --without-makeinfo --with-gnutls=no
      mkdir $out
      cp src/emacs-module.h $out/
    '';
}
