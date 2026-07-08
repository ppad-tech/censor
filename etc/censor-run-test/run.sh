#!/usr/bin/env bash
# Build the shim and run the two smoke checks against the `censor`
# executable. Assumes the executable has already been built via
# `cabal build -f+run` in the project root.
#
# Usage: bash etc/censor-run-test/run.sh [meter]
#   meter defaults to `wall`; on Linux, try `instructions` or
#   `cycles` for a tighter check.

set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
meter=${1:-wall}

case "$(uname -s)" in
  Darwin) ext=dylib ;;
  Linux)  ext=so    ;;
  *)      echo "unsupported platform" >&2; exit 1 ;;
esac

lib="$here/shim.$ext"
cc -O2 -shared -fPIC -o "$lib" "$here/shim.c"

censor=$(cd "$root" && cabal list-bin -f+run exe:censor)

pass=0
fail=0

check() {
  local name=$1 expected=$2 target=$3
  echo "=== $name ==="
  set +e
  "$censor" "$lib" --target "$target" --workspace 64 --secret 0:32 \
    --meter "$meter" --alpha 1e-3 --budget 4000 --attribute
  local ec=$?
  set -e
  if [ "$ec" -eq "$expected" ]; then
    echo "OK (exit $ec)"; pass=$((pass+1))
  else
    echo "FAIL: expected exit $expected, got $ec"; fail=$((fail+1))
  fi
  echo
}

check "ct_countdown (positive)"    0 ct_countdown
check "leaky_countdown (negative)" 1 leaky_countdown

echo "results: $pass passed, $fail failed"
exit "$fail"
