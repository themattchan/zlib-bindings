let
config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = haskellPackagesNew : haskellPackagesOld : {
                zlib-bindings-awake =
                    pkgs.haskell.lib.overrideCabal
                    (haskellPackagesNew.callPackage ./default.nix { })
                    (oldDerivation: rec {
                      shellHook =
                        ''
                         export LD_LIBRARY_PATH=${pkgs.zlib}/lib
                        '';
                      librarySystemDepends = [ pkgs.zlib.dev ];
                    });

                memory =
                    haskellPackagesNew.callPackage ./nix/memory.nix { };
            };
        };
    };
};


pkgs = import <nixpkgs> { inherit config; };

in
  { zlib-bindings-awake = pkgs.haskellPackages.zlib-bindings-awake;
  }
