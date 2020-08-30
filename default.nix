let
  nixpkgs = builtins.fetchTarball (import ./nixpkgs.nix);
  home-manager = builtins.fetchTarball (import ./home-manager.nix);
  pkgs = import "${nixpkgs}" {};
in
{
  install = (import "${home-manager}" { inherit pkgs; }).install;
}
