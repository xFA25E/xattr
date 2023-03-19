{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs self;
  l = inputs.nixpkgs.lib // builtins;
  meta = inputs.cells.automation.lib;
  epkgs = (nixpkgs.appendOverlays [inputs.emacs-overlay.overlay]).emacs.pkgs;
in {
  default = cell.packages."emacsPackages/${meta.name}";
  "emacsPackages/${meta.name}" = epkgs.melpaBuild {
    inherit (meta) version;
    pname = meta.name;
    src = self;
    commit = self.rev or "0000000000000000000000000000000000000000";
    recipe = nixpkgs.writeText "recipe" ''
      (${meta.name} :fetcher git :url "${meta.url}"
                    :files ("xattr-core.so" "xattr.el" "xattr-map.el"))
    '';
    packageRequires = l.attrsets.attrVals meta.deps epkgs;

    EMACS_SRC = "${inputs.cells.automation.packages.emacsSrcWithModule}";
    nativeBuildInputs = [nixpkgs.gnulib];
    preBuild = "make";
  };
}
