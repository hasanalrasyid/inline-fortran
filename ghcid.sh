if [ -z "$1" ]; then
  tipe="-Wno-missing-signatures -Wno-orphans -Wno-unused-matches -Wno-unused-local-binds -Wno-unused-top-binds -Wno-unused-imports -Wno-name-shadowing -Wmissing-methods"
else
  tipe="-Wall"
fi
LD_LIBRARY_PATH=./foreign/lib:$LD_LIBRARY_PATH ghcid -c "stack ghci --main-is inline-fortran:exe:x --only-main --ghci-options '$tipe'"
