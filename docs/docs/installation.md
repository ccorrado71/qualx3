# Installation

## Windows

1. Download the Windows installer (`qualx-<version>_install.exe`) from the project download page.
2. Run the installer and follow the on-screen instructions.
3. QualX will be available from the Start Menu and the desktop shortcut.

!!! note
    The installer includes all required runtime libraries (Qt6, Fortran runtime).
    No additional software needs to be installed separately.

---

## macOS

1. Download the macOS DMG file (`qualx-<version>_macos.dmg`) from the project download page.
2. Double-click the DMG to mount it.
3. Drag the **QualX** icon into the **Applications** folder.
4. Eject the mounted volume.
5. Launch QualX from the Applications folder or Launchpad.

!!! note
    On first launch macOS may warn about an unidentified developer.
    Open **System Settings → Privacy & Security** and click **Open Anyway**.

---

## Linux

### Option A — Debian/Ubuntu package (.deb)

This is the recommended method for Debian-based distributions (Ubuntu, Linux Mint, etc.).

**1. Install the runtime dependencies**

```bash
sudo apt-get update
sudo apt-get install libgfortran5 \
    libqt6core6 libqt6gui6 libqt6widgets6 \
    libqt6sql6 libqt6printsupport6 libqt6network6 \
    libqt6opengl6 libqt6openglwidgets6
```

!!! note
    On Ubuntu 24.04 and later the library names may have the `t64` suffix
    (e.g. `libqt6core6t64`). Run `apt-cache search libqt6core6` to check
    which variant is available on your system.

**2. Install the package**

```bash
sudo dpkg -i qualx-<version>-<distro>_amd64.deb
# or, to resolve missing dependencies automatically:
sudo apt install ./qualx-<version>-<distro>_amd64.deb
```

The executable is installed to `/usr/bin/qualx`.

**3. Uninstall**

```bash
sudo dpkg -r qualx
```

---

### Option B — Compilation from source (.tar.gz)

Use this method on any Linux distribution or when you need to choose a specific
Fortran compiler.

**Requirements**

| Tool | Minimum version |
|------|-----------------|
| CMake | 3.20 |
| C++ compiler (g++) | 9 |
| Fortran compiler | gfortran 9, ifort 2021, or ifx 2024 |
| Qt6 | 6.5 (Widgets, Sql, PrintSupport, Network) |

Install build dependencies on Debian/Ubuntu:

```bash
sudo apt-get install cmake build-essential gfortran pkg-config qt6-base-dev
```

On Fedora/RHEL:

```bash
sudo dnf install cmake gcc gcc-c++ make gcc-gfortran pkgconfig qt6-qtbase-devel
```

**Automated installation with `install.sh`**

```bash
tar xzf qualx-<version>.tar.gz
cd qualx-<version>
./install.sh
```

The script configures, compiles, and installs QualX to `/opt/qualx-<version>`.
To run it afterwards:

```bash
/opt/qualx-<version>/bin/qualx
# or add to PATH:
export PATH=/opt/qualx-<version>/bin:$PATH
```

By default `install.sh` uses `gfortran`. Pass a different compiler as the
first argument, and an optional Qt6 prefix as the second:

```bash
./install.sh ifx                                    # Intel Fortran (ifx)
./install.sh gfortran /home/user/Qt/6.5.0/gcc_64   # custom Qt path
```

**Manual CMake build**

```bash
tar xzf qualx-<version>.tar.gz
cd qualx-<version>

cmake -DCMAKE_Fortran_COMPILER=gfortran \
      -DCMAKE_BUILD_TYPE=Release \
      -S . -B build
cmake --build build -j$(nproc)
sudo cmake --install build
```

---

## Databases

QualX requires at least one crystallographic reference database.

| Database | Format | Notes |
|----------|--------|-------|
| COD (Crystallography Open Database) | COD/CIF | Free, included |
| PDF-2 | PDF-2 DAT | Commercial — ICDD license required |

Database files are managed through **Search → Manage Databases**.

---

## First Launch

On first launch QualX will ask you to configure the path to the database files.
See [Getting Started](introduction/getting-started.md) for a step-by-step guide.
