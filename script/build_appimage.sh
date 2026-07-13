#!/bin/bash
#
# Builds a qualx AppImage from the current source tree.
#
# Usage: ./build_appimage.sh [fortran_compiler]
#   fortran_compiler  gfortran (default), ifort, ifx
#
# Produces build_appimage/qualx-<version>-x86_64.AppImage (also copied
# next to this script's source tree root).
#
# Requires: cmake, a Qt6 dev install, patchelf, curl (to fetch linuxdeploy
# and appimagetool on first run — cached under build_appimage/tools/).
# For a native GTK/GNOME look (instead of Qt's generic Fusion style), also
# install qt6-gtk-platformtheme before building: its plugin is bundled only
# if present at build time.

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

FORTRAN_COMPILER=${1:-gfortran}
NUM_THREADS=$(nproc)

BUILD_DIR="$SRC_DIR/build_appimage"
APPDIR="$BUILD_DIR/AppDir"
TOOLS_DIR="$BUILD_DIR/tools"

echo -e "${BLUE}=========================================="
echo "QualX - AppImage Build"
echo -e "==========================================${NC}"
echo ""
echo "Fortran compiler   : ${FORTRAN_COMPILER}"
echo "Compilation threads: ${NUM_THREADS}"
echo ""

if ! command -v ${FORTRAN_COMPILER} &> /dev/null; then
    echo -e "${RED}ERROR: Compiler ${FORTRAN_COMPILER} not found${NC}"
    exit 1
fi
if ! command -v curl &> /dev/null; then
    echo -e "${RED}ERROR: curl not found (required to fetch linuxdeploy/appimagetool)${NC}"
    exit 1
fi

echo -e "${YELLOW}Cleaning previous build...${NC}"
rm -rf "$BUILD_DIR"
mkdir -p "$TOOLS_DIR"

# Step 1: configure + build + install into a portable AppDir/usr tree
# (same "bundle everything" layout as a manual /opt install, just
# skipping the per-user desktop-integration step — see BUILD_APPIMAGE
# in CMakeLists.txt).
echo -e "${YELLOW}Step 1/5: Configuration with CMake (AppImage mode)...${NC}"
cmake -B "$BUILD_DIR/cmake" -S "$SRC_DIR" \
    -DCMAKE_Fortran_COMPILER=${FORTRAN_COMPILER} \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_APPIMAGE=ON \
    -DCMAKE_INSTALL_PREFIX="$APPDIR/usr"
echo -e "${GREEN}✓ Configuration completed${NC}"
echo ""

echo -e "${YELLOW}Step 2/5: Compilation (using ${NUM_THREADS} threads)...${NC}"
cmake --build "$BUILD_DIR/cmake" -j${NUM_THREADS}
cmake --install "$BUILD_DIR/cmake"
echo -e "${GREEN}✓ Compilation and install completed${NC}"
echo ""

# Step 2: AppDir metadata (desktop file with a relative Exec=, + icon)
echo -e "${YELLOW}Step 3/5: Assembling AppDir metadata...${NC}"
install -Dm644 "$SRC_DIR/deploy/linux/qualx.appimage.desktop" \
    "$APPDIR/usr/share/applications/qualx.desktop"
install -Dm644 "$SRC_DIR/deploy/linux/icons/256x256/qualx.png" \
    "$APPDIR/usr/share/icons/hicolor/256x256/apps/qualx.png"
echo -e "${GREEN}✓ Metadata assembled${NC}"
echo ""

# Step 3: fetch linuxdeploy + appimagetool (cached across runs)
echo -e "${YELLOW}Step 4/5: Fetching linuxdeploy and appimagetool...${NC}"
LINUXDEPLOY="$TOOLS_DIR/linuxdeploy-x86_64.AppImage"
APPIMAGETOOL="$TOOLS_DIR/appimagetool-x86_64.AppImage"

if [ ! -x "$LINUXDEPLOY" ]; then
    curl -fsSL -o "$LINUXDEPLOY" \
        https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
    chmod +x "$LINUXDEPLOY"
fi
if [ ! -x "$APPIMAGETOOL" ]; then
    curl -fsSL -o "$APPIMAGETOOL" \
        https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-x86_64.AppImage
    chmod +x "$APPIMAGETOOL"
fi
echo -e "${GREEN}✓ Tools ready${NC}"
echo ""

# Containers (and many CI runners) have no /dev/fuse: run tools extracted.
export APPIMAGE_EXTRACT_AND_RUN=1

# Step 4: let linuxdeploy pull in non-Qt runtime deps not already bundled
# by the CMake install (libgfortran, libquadmath, ...), write AppRun, and
# wire the desktop file + icon into the AppDir root.
#
# Qt plugins (platforms/xcb, platformthemes/gtk3, sqldrivers/sqlite, ...)
# are loaded via dlopen(), so linuxdeploy never sees them by walking the
# qualx executable's own link graph. Pass each one explicitly via
# --library so linuxdeploy also discovers and bundles *their* transitive
# dependencies (e.g. libqgtk3.so pulls in GTK3/glib/atspi) instead of
# silently falling back to the host's — possibly incompatible — copies.
echo -e "${YELLOW}Step 5/5: Running linuxdeploy and appimagetool...${NC}"
linuxdeploy_args=(
    --appdir "$APPDIR"
    --executable "$APPDIR/usr/bin/qualx"
    --desktop-file "$APPDIR/usr/share/applications/qualx.desktop"
    --icon-file "$APPDIR/usr/share/icons/hicolor/256x256/apps/qualx.png"
)
while IFS= read -r -d '' plugin_lib; do
    linuxdeploy_args+=(--library "$plugin_lib")
done < <(find "$APPDIR/usr/plugins" -name '*.so' -print0)

"$LINUXDEPLOY" "${linuxdeploy_args[@]}"

VERSION="$(grep -m1 'project\s*(' "$SRC_DIR/CMakeLists.txt" | sed 's/.*VERSION[[:space:]]\+\([0-9][0-9.]*\).*/\1/')"
OUTPUT="$BUILD_DIR/qualx-${VERSION:-unknown}-x86_64.AppImage"
ARCH=x86_64 "$APPIMAGETOOL" "$APPDIR" "$OUTPUT"

cp "$OUTPUT" "$SRC_DIR/"
echo -e "${GREEN}=========================================="
echo "✓ AppImage created successfully!"
echo -e "==========================================${NC}"
echo ""
echo "File: $(basename "$OUTPUT")"
