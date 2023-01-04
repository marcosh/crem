# This file is taken from https://github.com/edolstra/flake-compat
# It probably never needs to change.

let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat = fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
  };
in
(import flake-compat { src = ./.; }).shellNix
