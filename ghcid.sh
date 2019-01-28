if [ -z "$1" ]; then
  tipe="exe:t3"
else
  tipe=$1
fi
LD_LIBRARY_PATH=./foreign/lib:$LD_LIBRARY_PATH ghcid -c "stack ghci --only-main --ghci-options '-Wall -Wno-missing-signatures -Wno-orphans -Wno-unused-imports -Wno-unused-local-binds -Wno-unused-matches'"
