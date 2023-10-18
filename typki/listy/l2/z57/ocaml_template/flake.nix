{
  description = "A very basic flake";
  inputs = {
	nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  };

  outputs = { self, nixpkgs }: 
    let 
    	pkgs = import nixpkgs { system = "x86_64-linux"; }; 
      ocaml_pkgs = pkgs.ocaml-ng.ocamlPackages_5_0;
    in {
    	devShells.x86_64-linux.default = pkgs.mkShell {
		packages = with pkgs; [

		] ++ (with ocaml_pkgs; [
			ocaml
      ocamlformat
      ocaml-lsp
			dune_3
			utop
		]);
		OCAML_DEV = 1; # Set nev variable

	};


    };


}
