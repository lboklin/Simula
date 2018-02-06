{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "SimulaHS";
  inherit ghc;
  buildInputs = with pkgs; [
    (callPackage simula-openvr/openvr.nix { })
    dbus
    gcc7
    libxkbcommon
    libxml2
    mesa
    steam
    vulkan-loader
    wayland
    wayland-protocols
    weston
    xorg.libX11
    xorg.pixman
  ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
