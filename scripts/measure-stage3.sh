#!/bin/sh
set -eu

if [ "${READJSA_QUIET_MEASUREMENT_SLOT:-}" != "confirmed" ]; then
  echo "Refusing to benchmark outside an explicitly confirmed quiet slot." >&2
  echo "Set READJSA_QUIET_MEASUREMENT_SLOT=confirmed to opt in." >&2
  exit 64
fi

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
package_dir=$(dirname -- "$script_dir")

export READJSA_LIVE=true

cd "$package_dir"
exec Rscript scripts/measure-stage3.R
