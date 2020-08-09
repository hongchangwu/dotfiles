let
  nixpkgs = builtins.fetchTarball (import ./nixpkgs.nix);
  pkgs = import "${nixpkgs}" {};
in
rec {
  home-manager = pkgs.callPackage ./home-manager/home-manager {
    path = toString ./home-manager;
  };

  install = pkgs.callPackage ./home-manager/home-manager/install.nix {
    inherit home-manager;
  };
}
