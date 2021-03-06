{ bootstrap ? import <nixpkgs> { } }:
let
  # 18.09 (stable)
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };
in

import src { }
