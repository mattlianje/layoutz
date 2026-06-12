#!/bin/sh
set -e

cd "$(dirname "$0")/.."

if ! command -v vhs >/dev/null 2>&1; then
  echo "error: vhs not found. Install it with: brew install vhs" >&2
  exit 1
fi

echo "Building layoutz (publishLocal)..."
./mill layoutz.jvm[3.3.7].publishLocal

if [ -n "$1" ]; then
  tapes="demos/$1.tape"
  if [ ! -f "$tapes" ]; then
    echo "error: no such tape '$tapes'" >&2
    exit 1
  fi
else
  tapes="demos/*.tape"
fi

for tape in $tapes; do
  name=$(basename "$tape" .tape)
  echo "Recording $name..."
  vhs "$tape"
done

echo "Done. GIFs in demos/"
ls -lh demos/*.gif
