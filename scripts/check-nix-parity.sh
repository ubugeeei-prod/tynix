#!/bin/sh
# Syntax-parity regression check against nixpkgs.
#
# Every sampled nixpkgs file is compiled with `tynix compile --no-check` and
# the result must parse (`nix-instantiate --parse`) to the same expression as
# the original. Adjacent string literals are joined before comparing, since
# tynix may split an equal string value differently.
#
# usage: scripts/check-nix-parity.sh <tynix-binary> [stride] [limit]
set -eu

tynix=$1
stride=${2:-25}
limit=${3:-1000}
# Resolve the locked nixpkgs from flake.lock directly; evaluating the flake
# itself would copy the whole working tree into the store.
nixpkgs=$(nix eval --raw --impure --expr '
  let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  in (builtins.fetchTree lock.nodes.nixpkgs.locked).outPath
')
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

normalize() {
  sed 's/" + "//g' | perl -pe '1 while s/\("((?:[^"\\]|\\.)*)"\)/"$1"/g'
}

find "$nixpkgs" -name '*.nix' -not -path '*/tests/*' |
  sort |
  awk -v s="$stride" 'NR % s == 0' |
  head -n "$limit" > "$work/files"
total=0
failed=0
while IFS= read -r file; do
  total=$((total + 1))
  cat "$file" > "$work/input.tynix"
  if ! "$tynix" compile --no-check "$work/input.tynix" > "$work/output.nix" 2> "$work/error"; then
    failed=$((failed + 1))
    echo "compile failed: $file"
    head -n 3 "$work/error"
    continue
  fi
  expected=$(nix-instantiate --parse "$file" | normalize)
  actual=$(
    cd "$(dirname "$file")" &&
      nix-instantiate --parse - < "$work/output.nix" | normalize
  ) || actual="<parse error>"
  if [ "$expected" != "$actual" ]; then
    failed=$((failed + 1))
    echo "round-trip changed the expression: $file"
  fi
done < "$work/files"

echo "checked $total nixpkgs files, $failed failed"
[ "$failed" -eq 0 ]
