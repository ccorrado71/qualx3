#!/bin/bash
#
# Builds a qualx3 AppImage from a source tarball using Docker, so the
# result doesn't depend on whatever glibc/Qt happens to be installed on
# the host machine.
#
# Usage: ./make_appimage_docker.sh <source.tar.gz> [ubuntu_version]
#
# Unlike make_debian_docker.sh, this does not loop over several Ubuntu
# versions: a .deb is built once per *target* distro because it links
# against that distro's own libraries, but a single AppImage is meant
# to run on virtually any modern distro, so it is built once against a
# single, deliberately old base (see Dockerfile.appimage) to maximize
# glibc/libstdc++ compatibility with newer host systems.
#
# Examples:
#   ./make_appimage_docker.sh qualx3-1.0.0-Source.tar.gz
#   ./make_appimage_docker.sh qualx3-1.0.0-Source.tar.gz 22.04
#
# The resulting .AppImage is written to ./appimages/ next to the
# working directory.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCKERFILE="$SCRIPT_DIR/Dockerfile.appimage"
UBUNTU_VERSION="22.04"

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <source.tar.gz> [ubuntu_version]"
    echo "Examples:"
    echo "  $0 qualx3-1.0.0-Source.tar.gz"
    echo "  $0 qualx3-1.0.0-Source.tar.gz 22.04"
    exit 1
fi

TAR_GZ="$1"
if [ "$#" -ge 2 ]; then
    UBUNTU_VERSION="$2"
fi

if [ ! -f "$TAR_GZ" ]; then
    echo "Error: source archive not found: $TAR_GZ"
    exit 1
fi
TAR_GZ="$(cd "$(dirname "$TAR_GZ")" && pwd)/$(basename "$TAR_GZ")"

OUT_DIR="$(pwd)/appimages"
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

echo "Extracting $TAR_GZ ..."
tar -xzf "$TAR_GZ" -C "$WORK_DIR"
SRC_DIR="$(find "$WORK_DIR" -mindepth 1 -maxdepth 1 -type d | head -n 1)"
if [ -z "$SRC_DIR" ]; then
    echo "Error: could not locate extracted source directory inside $TAR_GZ"
    exit 1
fi

echo ""
echo "=========================================="
echo " Building AppImage (base: Ubuntu $UBUNTU_VERSION)"
echo "=========================================="

mkdir -p "$OUT_DIR"

DOCKER_BUILDKIT=1 docker build \
    --build-arg UBUNTU_VERSION="$UBUNTU_VERSION" \
    --target export \
    -f "$DOCKERFILE" \
    -o "$OUT_DIR" \
    "$SRC_DIR"

echo ""
echo "AppImage(s) written to $OUT_DIR:"
ls -1 "$OUT_DIR"
