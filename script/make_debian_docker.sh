#!/bin/bash
#
# Builds qualx3 Debian packages for multiple Ubuntu LTS releases using Docker,
# replacing the manual per-distro virtual machine + make_debian.sh workflow.
#
# Usage: ./make_debian_docker.sh <source.tar.gz> [ubuntu_version ...]
#
# Examples:
#   ./make_debian_docker.sh qualx3-1.0.0-Source.tar.gz
#   ./make_debian_docker.sh qualx3-1.0.0-Source.tar.gz 24.04
#
# With no version arguments, packages are built for every entry in
# UBUNTU_VERSIONS below. Resulting .deb files are written to
# ./debs/<ubuntu_version>/ next to the working directory.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCKERFILE="$SCRIPT_DIR/Dockerfile.deb"
UBUNTU_VERSIONS=(24.04 26.04)

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <source.tar.gz> [ubuntu_version ...]"
    echo "Examples:"
    echo "  $0 qualx3-1.0.0-Source.tar.gz"
    echo "  $0 qualx3-1.0.0-Source.tar.gz 24.04"
    exit 1
fi

TAR_GZ="$1"
shift
if [ "$#" -gt 0 ]; then
    UBUNTU_VERSIONS=("$@")
fi

if [ ! -f "$TAR_GZ" ]; then
    echo "Error: source archive not found: $TAR_GZ"
    exit 1
fi
TAR_GZ="$(cd "$(dirname "$TAR_GZ")" && pwd)/$(basename "$TAR_GZ")"

OUT_DIR="$(pwd)/debs"
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

echo "Extracting $TAR_GZ ..."
tar -xzf "$TAR_GZ" -C "$WORK_DIR"
SRC_DIR="$(find "$WORK_DIR" -mindepth 1 -maxdepth 1 -type d | head -n 1)"
if [ -z "$SRC_DIR" ]; then
    echo "Error: could not locate extracted source directory inside $TAR_GZ"
    exit 1
fi

for version in "${UBUNTU_VERSIONS[@]}"; do
    echo ""
    echo "=========================================="
    echo " Building .deb package for Ubuntu $version"
    echo "=========================================="

    dest="$OUT_DIR/$version"
    mkdir -p "$dest"

    DOCKER_BUILDKIT=1 docker build \
        --build-arg UBUNTU_VERSION="$version" \
        --target export \
        -f "$DOCKERFILE" \
        -o "$dest" \
        "$SRC_DIR"

    echo "Package(s) for Ubuntu $version written to $dest:"
    ls -1 "$dest"
done

echo ""
echo "Done. All packages are available under $OUT_DIR"
