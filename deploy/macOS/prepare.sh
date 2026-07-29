#!/bin/bash
CURDIR="$(cd "$(dirname "$0")" && pwd)"
VERSION=$(grep "^project(" "$CURDIR/../../CMakeLists.txt" | sed -E 's/.*VERSION ([0-9]+\.[0-9]+\.[0-9]+).*/\1/')
DMG_NAME="qualx-${VERSION}-arm64"

rm -rf "$CURDIR/qualx.app"
rm -f "$CURDIR"/*.dmg
cp -r "$CURDIR/../../build_gfor_r/src/qualx.app" "$CURDIR/"

# Bundle the documentation (docs/site + docs/qualx_manual.pdf), mirroring the
# 'install(DIRECTORY docs/site ...)' / 'install(FILES docs/qualx_manual.pdf ...)'
# rules in CMakeLists.txt, which cmake --install would do but this script skips.
DOCS_DEST="$CURDIR/qualx.app/Contents/share/qualx/docs"
mkdir -p "$DOCS_DEST"
if [ -d "$CURDIR/../../docs/site" ]; then
    cp -r "$CURDIR/../../docs/site/." "$DOCS_DEST/"
else
    echo "WARNING: docs/site not found - run 'mkdocs build' inside docs/ first"
fi
if [ -f "$CURDIR/../../docs/qualx_manual.pdf" ]; then
    cp "$CURDIR/../../docs/qualx_manual.pdf" "$DOCS_DEST/"
else
    echo "WARNING: docs/qualx_manual.pdf not found - run 'bash docs/make_pdf.sh' first"
fi

echo "Building $DMG_NAME.dmg ..."
cd "$CURDIR"
./make_dmg.sh qualx "$DMG_NAME"
