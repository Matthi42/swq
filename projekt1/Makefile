push: 
	nix build .#defaultPackage.x86_64-linux
	nix-store --export $$(readlink result) > swq.closure 
	scp swq.closure srv1:
