# Installation

## Windows

1. Download the Windows installer (`qualx-<version>_install.exe`) from the [download page](https://github.com/ccorrado71/qualx3/releases).
2. Run the installer and follow the on-screen instructions.
3. QualX will be available from the Start Menu and the desktop shortcut.

!!! note
    The installer includes all required runtime libraries (Qt6, Fortran runtime).
    No additional software needs to be installed separately.

---

## macOS

1. Download the macOS DMG file (`qualx-<version>_macos.dmg`) from the [download page](https://github.com/ccorrado71/qualx3/releases).
2. Double-click the DMG to mount it.
3. Drag the **QualX** icon into the **Applications** folder.
4. Eject the mounted volume.
5. Launch QualX from the Applications folder or Launchpad.

!!! note
    On first launch macOS may warn about an unidentified developer.
    Open **System Settings → Privacy & Security** and click **Open Anyway**.

---

## Linux

### Option A — AppImage (recommended)

The AppImage is a self-contained executable that bundles Qt6, the Fortran
runtime and all other dependencies. It runs on most recent x86_64 Linux
distributions without installing anything.

**1. Download**

Download `qualx-<version>-x86_64.AppImage` (e.g. `qualx-1.0.3-x86_64.AppImage`)
from the [download page](https://github.com/ccorrado71/qualx3/releases).

**2. Make it executable and run it**

```bash
chmod +x qualx-<version>-x86_64.AppImage
./qualx-<version>-x86_64.AppImage
```

!!! note
    AppImages require FUSE. If you get a "FUSE not found" error, either
    install `libfuse2` (`sudo apt-get install libfuse2` on Debian/Ubuntu) or
    extract and run the AppImage without FUSE:
    ```bash
    ./qualx-<version>-x86_64.AppImage --appimage-extract
    ./squashfs-root/AppRun
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
| COD (Crystallography Open Database) | COD/CIF | Free — download separately, not bundled with QualX |
| PDF-2 | PDF-2 DAT | Commercial — ICDD license required |

Database files are managed through **Search → Manage Databases**.

---

## First Launch

On first launch QualX will ask you to configure the path to the database files.
See [Getting Started](introduction/getting-started.md) for a step-by-step guide.
