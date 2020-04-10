{ useLocal }:
let
  local-src = ../../../nix;

  remote-src = builtins.fetchGit {
    url = "https://github.com/maybevoid/maybevoid-nix.git";
    rev = "253ba4ff3e3eef463a7961a3afaf56031fd401a8";
  };
in
if useLocal then local-src else remote-src
