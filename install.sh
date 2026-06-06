#!/bin/bash
# Automatic installation script for QualX
# Usage: ./install.sh [fortran_compiler] [qt_prefix]
#   fortran_compiler  gfortran (default), ifort, ifx
#   qt_prefix         optional Qt6 installation path (e.g. /home/user/Qt/6.5.0/gcc_64)

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Arguments
FORTRAN_COMPILER=${1:-gfortran}
QT_PREFIX=${2:-}

# Number of threads for parallel compilation
NUM_THREADS=$(nproc)

# Read program info from CMakeLists.txt
PROG_NAME=$(grep -m1 'EXECUTABLE_NAME' CMakeLists.txt | sed 's/.*set(EXECUTABLE_NAME[[:space:]]*"\([^"]*\)").*/\1/')
PROG_VERSION=$(grep -m1 'project\s*(' CMakeLists.txt | sed 's/.*VERSION[[:space:]]\+\([0-9][0-9.]*\).*/\1/')
INSTALL_PREFIX="/opt/${PROG_NAME}-${PROG_VERSION}"

# Detect Linux distribution family
detect_distro() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        for val in "${ID:-}" ${ID_LIKE:-}; do
            case "$val" in
                debian|ubuntu|linuxmint|pop|elementary|kali|raspbian|mint)
                    echo "debian"; return ;;
                fedora|rhel|centos|rocky|almalinux|ol|redhat)
                    echo "redhat"; return ;;
            esac
        done
        echo "unknown"
    else
        echo "unknown"
    fi
}

DISTRO_FAMILY=$(detect_distro)

# Distribution-specific package info
case "$DISTRO_FAMILY" in
    debian)
        PKG_MANAGER="apt-get"
        PKG_UPDATE="sudo apt-get update"
        PKG_INSTALL="sudo apt-get install cmake build-essential gfortran pkg-config qt6-base-dev libqt6sql6 libqt6printsupport6"
        ;;
    redhat)
        if command -v dnf &> /dev/null; then
            PKG_MANAGER="dnf"
        else
            PKG_MANAGER="yum"
        fi
        PKG_UPDATE="sudo ${PKG_MANAGER} check-update || true"
        PKG_INSTALL="sudo ${PKG_MANAGER} install cmake gcc gcc-c++ make gcc-gfortran pkgconfig qt6-qtbase-devel"
        ;;
    *)
        PKG_MANAGER="unknown"
        PKG_UPDATE=""
        PKG_INSTALL=""
        ;;
esac

echo -e "${BLUE}=========================================="
echo "QualX Installation"
echo -e "==========================================${NC}"
echo ""
echo "Fortran compiler : ${FORTRAN_COMPILER}"
echo "Compile threads  : ${NUM_THREADS}"
if [ -n "${QT_PREFIX}" ]; then
    echo "Qt prefix        : ${QT_PREFIX}"
fi
echo ""

# Print install hint based on distro
print_install_hint() {
    if [ -n "$PKG_INSTALL" ]; then
        echo "Install dependencies with:"
        [ -n "$PKG_UPDATE" ] && echo "  ${PKG_UPDATE}"
        echo "  ${PKG_INSTALL}"
    else
        echo "Please install the following manually: cmake, make, gcc, gfortran, pkg-config, Qt6 (Widgets, Sql, PrintSupport, Network)"
    fi
}

# Check that compiler is available
if ! command -v ${FORTRAN_COMPILER} &> /dev/null; then
    echo -e "${RED}ERROR: Compiler ${FORTRAN_COMPILER} not found${NC}"
    echo ""
    echo "Available Fortran compilers:"
    echo "  gfortran  - GNU Fortran compiler"
    echo "  ifort     - Intel Fortran compiler (classic)"
    echo "  ifx       - Intel Fortran compiler (LLVM-based)"
    echo ""
    print_install_hint
    exit 1
fi

# Check mandatory build tools
echo -e "${YELLOW}Checking dependencies...${NC}"
MISSING_DEPS=()

command -v cmake      &> /dev/null || MISSING_DEPS+=("cmake")
command -v make       &> /dev/null || MISSING_DEPS+=("make")
command -v pkg-config &> /dev/null || MISSING_DEPS+=("pkg-config")

if [ ${#MISSING_DEPS[@]} -ne 0 ]; then
    echo -e "${RED}ERROR: Missing dependencies: ${MISSING_DEPS[*]}${NC}"
    echo ""
    print_install_hint
    exit 1
fi

echo -e "${GREEN}✓ All dependencies are present${NC}"
echo ""

# ── Step 1: Configuration ──────────────────────────────────────────────────────
echo -e "${YELLOW}Step 1/3: Configuration with CMake...${NC}"

CMAKE_ARGS="-B build -S . -DCMAKE_Fortran_COMPILER=${FORTRAN_COMPILER} -DCMAKE_BUILD_TYPE=Release"
if [ -n "${QT_PREFIX}" ]; then
    CMAKE_ARGS="${CMAKE_ARGS} -DCMAKE_PREFIX_PATH=${QT_PREFIX}"
fi

cmake ${CMAKE_ARGS}

echo ""
echo "Installation prefix: ${INSTALL_PREFIX}"
echo ""
echo -e "${GREEN}✓ Configuration completed${NC}"
echo ""

# ── Step 2: Compilation ────────────────────────────────────────────────────────
echo -e "${YELLOW}Step 2/3: Compilation (using ${NUM_THREADS} threads)...${NC}"
cmake --build build -j${NUM_THREADS}

echo -e "${GREEN}✓ Compilation completed${NC}"
echo ""

# ── Step 3: Installation ───────────────────────────────────────────────────────
echo -e "${YELLOW}Step 3/3: Installing the program...${NC}"
echo "Password required for 'sudo cmake --install build'"
sudo cmake --install build

echo -e "${GREEN}✓ Installation completed${NC}"
echo ""

# Verify installation
if [ -f "${INSTALL_PREFIX}/bin/${PROG_NAME}" ]; then
    echo -e "${GREEN}=========================================="
    echo "✓ Installation completed successfully!"
    echo -e "==========================================${NC}"
    echo ""
    echo "Program installed in : ${INSTALL_PREFIX}"
    echo "Executable           : ${INSTALL_PREFIX}/bin/${PROG_NAME}"
    echo ""
    echo "To run the program:"
    echo "  ${INSTALL_PREFIX}/bin/${PROG_NAME}"
    echo ""
    echo "Or add to PATH:"
    echo "  export PATH=${INSTALL_PREFIX}/bin:\$PATH"
    echo ""
else
    echo -e "${RED}ERROR: File ${INSTALL_PREFIX}/bin/${PROG_NAME} was not created${NC}"
    exit 1
fi
