#!/bin/bash
#
# Usage: ./make_tar_gz.sh <package_name>
#
# Creates an installation .tar.gz from a git repository hosted on GitHub
# (github.com/ccorrado71). The script clones the repository, builds its
# documentation site/PDF manual if present (docs/site and docs/*.pdf are
# generated files, not tracked in git — see docs/mkdocs.yml, requires
# pandoc + xelatex on this machine), configures it with cmake, builds the
# source package, copies the resulting .tar.gz to the current directory,
# and then cleans up.
#
# Examples:
#   ./make_tar_gz.sh sir
#   ./make_tar_gz.sh expo2
#   ./make_tar_gz.sh qualx3

# 1) Read package_name from the command-line argument
if [ -z "$1" ]; then
  echo "Error: no package name provided."
  echo "Usage: $0 <package_name>"
  echo "Examples: $0 sir | $0 expo2 | $0 qualx3"
  exit 1
fi
package_name="$1"

# 2) Execute a git clone
git clone git@github.com:ccorrado71/$package_name.git

# 2b) Build the documentation site + PDF manual, if this package has one.
# docs/site and docs/*.pdf are generated, gitignored files, so the fresh
# clone above never has them — build them now so they end up in the
# source package.
if [ -f "$package_name/docs/mkdocs.yml" ]; then
  echo "Building documentation for $package_name..."
  python3 -m venv "$package_name/docs/.venv"
  "$package_name/docs/.venv/bin/pip" install -q -r "$package_name/docs/requirements.txt"
  (cd "$package_name/docs" && "./.venv/bin/mkdocs" build)

  if [ -f "$package_name/docs/make_pdf.sh" ]; then
    if command -v pandoc >/dev/null 2>&1 && command -v xelatex >/dev/null 2>&1; then
      bash "$package_name/docs/make_pdf.sh"
    else
      echo "Warning: pandoc/xelatex not found, skipping PDF manual (docs/site will still be packaged)."
    fi
  fi
fi

# 3) Create a build folder named build + package_name
mkdir build_$package_name

# 4) Change directory to the one created in step 3
cd build_$package_name

# 5) Configure the project with cmake
cmake -B . -S ../$package_name

# 6) Create the .tar.gz file
cmake --build . --target package_source

# 7) Copy the .tar.gz file created under build to the parent directory
tar_file=$(ls *.tar.gz 2>/dev/null)
if [ -n "$tar_file" ]; then
  cp "$tar_file" ..
  echo "File copied to the parent directory: $tar_file"
else
  echo "No .tar.gz file found."
  exit 1
fi

# 8) Remove the package_name and build_$package_name directories
cd ..
rm -rf $package_name
rm -rf build_$package_name

echo "Cleanup completed."
