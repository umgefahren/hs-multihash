{
  pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/8b1cf100cd8badad6e1b6d4650b904b88aa870db.tar.gz) {}
}:
  with pkgs;
  haskell.lib.buildStackProject {
    name = "hs-multihash";
    buildInputs = [ zlib ];
    shellHook = ''
      echo 'Entering Multihash Environment'
      set -v

      alias stack='\stack --nix'

      set +v
    '';
  }
