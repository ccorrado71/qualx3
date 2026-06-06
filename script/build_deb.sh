#!/bin/bash
# Script to build Debian package for QualX
# The package will install to /usr (standard Debian location)

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Fortran compiler (gfortran, ifort, or ifx — no MPI needed)
FORTRAN_COMPILER=${1:-gfortran}

# Number of threads for parallel compilation
NUM_THREADS=$(nproc)

echo -e "${BLUE}=========================================="
echo "QualX - Debian Package Build"
echo -e "==========================================${NC}"
echo ""
echo "Fortran compiler   : ${FORTRAN_COMPILER}"
echo "Compilation threads: ${NUM_THREADS}"
echo "Package will install to: /usr"
echo ""

# Check if compiler is available
if ! command -v ${FORTRAN_COMPILER} &> /dev/null; then
    echo -e "${RED}ERROR: Compiler ${FORTRAN_COMPILER} not found${NC}"
    echo ""
    echo "Install dependencies with:"
    echo "  sudo apt-get install cmake build-essential gfortran pkg-config qt6-base-dev libqt6sql6 libqt6printsupport6"
    exit 1
fi

# Check required tools for packaging
if ! command -v dpkg-deb &> /dev/null; then
    echo -e "${RED}ERROR: dpkg-deb not found (required for building .deb packages)${NC}"
    exit 1
fi

# Clean previous build
echo -e "${YELLOW}Cleaning previous build...${NC}"
rm -rf build_deb

# Step 1: Configuration for Debian package
echo -e "${YELLOW}Step 1/4: Configuration with CMake (Debian package mode)...${NC}"
cmake -B build_deb -S . \
    -DCMAKE_Fortran_COMPILER=${FORTRAN_COMPILER} \
    -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_DEB_PACKAGE=ON

echo -e "${GREEN}✓ Configuration completed${NC}"
echo ""

# Step 2: Compilation
echo -e "${YELLOW}Step 2/4: Compilation (using ${NUM_THREADS} threads)...${NC}"
cmake --build build_deb -j${NUM_THREADS}

echo -e "${GREEN}✓ Compilation completed${NC}"
echo ""

# Step 3: Create Debian package
echo -e "${YELLOW}Step 3/4: Creating Debian package...${NC}"
cd build_deb
cpack -G DEB

echo -e "${GREEN}✓ Package created${NC}"
echo ""

# Step 4: Show package info
echo -e "${YELLOW}Step 4/4: Package information...${NC}"
DEB_FILE=$(ls -1 *.deb | head -1)

if [ -f "$DEB_FILE" ]; then
    echo -e "${GREEN}=========================================="
    echo "✓ Debian package created successfully!"
    echo -e "==========================================${NC}"
    echo ""
    echo "Package file: build_deb/$DEB_FILE"
    echo ""
    echo "Package information:"
    dpkg-deb -I "$DEB_FILE" | grep -E "Package:|Version:|Architecture:|Maintainer:|Depends:"
    echo ""
    echo "Package contents:"
    dpkg-deb -c "$DEB_FILE" | head -20
    echo ""
    echo "To install the package:"
    echo "  sudo dpkg -i $DEB_FILE"
    echo "  sudo apt-get install -f  # Install missing dependencies if needed"
    echo ""
    PKG_NAME=$(dpkg-deb --field "$DEB_FILE" Package)
    echo "To uninstall:"
    echo "  sudo dpkg -r $PKG_NAME"
    echo ""
else
    echo -e "${RED}ERROR: Package file not found${NC}"
    exit 1
fi
