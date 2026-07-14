#!/bin/bash
CURDIR="$(cd "$(dirname "$0")" && pwd)"
VERSION=$(grep "^project(" "$CURDIR/../../CMakeLists.txt" | sed -E 's/.*VERSION ([0-9]+\.[0-9]+\.[0-9]+).*/\1/')
DMG_NAME="qualx-${VERSION}-arm64"

rm -rf "$CURDIR/qualx.app"
rm -f "$CURDIR"/*.dmg
cp -r "$CURDIR/../../build_gfor_r/src/qualx.app" "$CURDIR/"

echo "Building $DMG_NAME.dmg ..."
cd "$CURDIR"
./make_dmg.sh qualx "$DMG_NAME"
