if [ -z "$1" ]; then
  tipe="-Wno-missing-signatures -Wno-orphans -Wno-unused-matches -Wno-unused-local-binds -Wno-unused-top-binds -Wno-unused-imports -Wno-name-shadowing -Wmissing-methods"
else
  tipe="-Wall"
fi
#LD_LIBRARY_PATH=./foreign/lib:$LD_LIBRARY_PATH stack exec ghcid -- -c "stack ghci --main-is inline-fortran:test:i --only-main --ghci-options '$tipe'"
#LD_LIBRARY_PATH=./foreign/lib:$LD_LIBRARY_PATH nix-shell shell-stack.nix --run "stack exec ghcid -- -c \"stack ghci --main-is inline-fortran:test:i --only-main --ghci-options '$tipe'\""
ghcid --command 'stack ghci --ghci-options=fbyte-code --no-nix-pure inline-fortran:lib inline-fortran:test:i'  --restart inline-fortran.cabal --restart stack.yaml -W

