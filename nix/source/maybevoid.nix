{ useLocal }:
let
  local-src = ../../../nix;

  remote-src = builtins.fetchGit {
    url = "https://github.com/maybevoid/maybevoid-nix.git";
    rev = "31f6ac82c778fcc7e3b23beb3322c935d290e2b5";
  };
in
if useLocal then local-src else remote-src
