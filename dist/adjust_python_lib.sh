#!/usr/bin/env bash
# adjust_python_lib.sh
# Usage: adjust_python_lib.sh <executable>
# Example: adjust_python_lib.sh target/release/journ

# This script provides an example of distributing journ to other Mac machines,
# as when journ is built, it is linked against the builders machine's python framework
# location. This can be made more generic by using 'Current' instead of the version number
# in the framework path.
# On Mac, journ will likely be installed with brew and a post install script should emulate this
# logic to ensure a suitable version of python exists and update the exe.

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <executable>" >&2
  exit 1
fi

exe="$1"
formula="${2:-python}"

if [ ! -f "$exe" ]; then
  echo "Error: '$exe' not found or not a file." >&2
  exit 2
fi

# 1) Find the old Python.framework install-name
old_path=$(otool -L "$exe" \
  | awk '/Python3?.framework\/Versions\// { print $1; exit }')

if [ -z "$old_path" ]; then
  echo "Error: no Python.framework dependency found in '$exe'." >&2
  exit 3
fi

echo "Found old framework path: $old_path"

# 2) Replace it with @rpath/…/Current/Python
new_id="@rpath/Python.framework/Versions/Current/Python"
echo "Patching install-name to: $new_id"
install_name_tool -change "$old_path" "$new_id" "$exe"
