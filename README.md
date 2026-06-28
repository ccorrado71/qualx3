# QualX

**Qualitative phase analysis software for X-ray powder diffraction patterns**, developed at the [Institute of Crystallography (IC-CNR), Bari](https://www.ic.cnr.it/).

QualX helps identify the crystalline phases present in a powder diffraction pattern by comparing it against large reference databases (PDF-2, COD) and crystallographic data imported from CIF files.

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](LICENSE)
![C++17](https://img.shields.io/badge/C%2B%2B-17-00599C)
![Qt6](https://img.shields.io/badge/Qt-6-41CD52)
![Fortran](https://img.shields.io/badge/Fortran-modern-734F96)

---

## Table of contents

- [About](#about)
- [Features](#features)
- [Reference databases](#reference-databases)
- [Requirements](#requirements)
- [Installation](#installation)
  - [Linux (quick install)](#linux-quick-install)
  - [Manual build](#manual-build)
- [Usage](#usage)
  - [Graphical interface](#graphical-interface)
  - [Command-line database creation](#command-line-database-creation)
- [Documentation](#documentation)
- [Repository structure](#repository-structure)
- [Citing QualX](#citing-qualx)
- [License](#license)
- [Authors and acknowledgments](#authors-and-acknowledgments)
- [Contributing](#contributing)
- [Support](#support)

## About

QualX is a Qt6/C++ desktop application, with a Fortran computational core, for **qualitative phase analysis (phase identification)** of powder X-ray diffraction data. It lets researchers search a diffraction pattern against crystallographic reference databases to identify which known phases are present in a sample.

The application can work with:
- **PDF-2** (Powder Diffraction File) reference data,
- **COD** (Crystallography Open Database) entries,
- custom databases built from a folder of **CIF** files.

## Features

- Qualitative phase identification against large crystallographic databases
- Built-in database creation/import tools (from PDF-2 or CIF files)
- Search engine based on top d-value indexing for fast pattern matching
- Cross-platform: Linux, Windows, macOS
- GUI workflow plus a scriptable command-line interface for database creation
- Fortran computational core for crystallographic calculations, interfaced with C++ via `iso_c_binding`

## Reference databases

QualX databases are composed of four files sharing a common base name:

| Extension       | Content                                   |
|------------------|--------------------------------------------|
| `.sq`            | Main data (id, chemical formula, subfiles) |
| `.sq.info`       | Crystallographic and bibliographic data    |
| `.sq.infostat`   | Statistical tables                         |
| `.sq.search`     | Search index (top d-values)                |

A reference database built from the [COD](https://www.crystallography.net/cod/) inorganic subset is included under `DB/cod/`. It is large (the bundled `cod_inorg.sq*` files total roughly **1 GB**), so cloning the repository may take a while and require a stable connection.

You can also build your own database from PDF-2 data or a folder of CIF files — see [Command-line database creation](#command-line-database-creation).

## Requirements

- CMake ≥ 3.20
- A C++17 compiler (GCC, Clang, or MSVC)
- A Fortran compiler: `gfortran`, `ifort`, or `ifx`
- Qt6 (components: `Widgets`, `Sql`, `PrintSupport`, `Network`)
- SQLite (used via Qt SQL driver)

## Installation

### Linux (quick install)

An automatic installation script is provided for Debian-based and RedHat-based distributions:

```bash
./install.sh [fortran_compiler] [qt_prefix]
```

Examples:

```bash
./install.sh                  # uses gfortran, auto-detects Qt
./install.sh gfortran /home/user/Qt/6.5.0/gcc_64
```

The script checks dependencies, configures the project with CMake, builds it, and installs it to `/opt/qualx-<version>` (requires `sudo` for the install step).

### Manual build

```bash
cmake -DCMAKE_PREFIX_PATH=/path/to/Qt/6.x.y/gcc_64 \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_Fortran_COMPILER=gfortran \
      -S . -B build

cmake --build build -j$(nproc)
sudo cmake --install build
```

Adjust `CMAKE_PREFIX_PATH` to point to your Qt6 installation, and `CMAKE_Fortran_COMPILER` to `gfortran`, `ifort`, or `ifx` as needed. Platform-specific packaging resources (icons, installer scripts) are available under `deploy/linux`, `deploy/Windows`, and `deploy/macOS`.

## Usage

### Graphical interface

Launch the installed executable (`qualx`) to open the GUI, load a diffraction pattern, select a reference database, and run a qualitative phase search.

### Command-line database creation

QualX can also build reference databases without launching the GUI:

```bash
# From a PDF-2 file:
qualx --createdb --pdf2 /path/to/pdf2.dat --dbout /path/to/output

# From a folder of CIF files:
qualx --createdb --cifdir /path/to/cifs [--recursive] --dbout /path/to/output
```

## Documentation

A user manual is available under `docs/` (built with [MkDocs](https://www.mkdocs.org/)):

```bash
pip install -r docs/requirements.txt
mkdocs build -f docs/mkdocs.yml
bash docs/make_pdf.sh   # optional: generates docs/qualx_manual.pdf
```

## Repository structure

```
src/           C++ and Fortran sources
src/CMakeLists.txt   list of all sources (PROJECT_SOURCES and FORTRAN_SOURCES)
share/qualx/   runtime data files (syminfo.lib, AtomProperties.xen, ...)
DB/            bundled SQLite reference databases (e.g. DB/cod/cod_inorg)
deploy/        platform-specific packaging resources (Linux, Windows, macOS)
docs/          user manual sources (MkDocs)
configured/    files generated by CMake (config.h with version info)
```

## Citing QualX

If you use QualX in your research, please cite one of the following works:

1. Altomare, A., Corriero, N., Cuocci, C., Falcicchio, A., Moliterni, A. and Rizzi, R.,
   *'Main features of QUALX2.0 software for qualitative phase analysis'* (2017). *Powder Diffr.*
   [https://doi.org/10.1017/S0885715617000240](https://doi.org/10.1017/S0885715617000240)

2. Altomare, A., Corriero, N., Cuocci, C., Falcicchio, A., Moliterni, A. and Rizzi, R.,
   *'QUALX2.0: a qualitative phase analysis software using the freely available database POW_COD'* (2015). *J. Appl. Cryst.* **48**, 598–603.
   [https://doi.org/10.1107/S1600576715002319](https://doi.org/10.1107/S1600576715002319)

3. Altomare, A., Cuocci, C., Giacovazzo, C., Moliterni, A. and Rizzi, R.,
   *'QUALX: A computer program for qualitative analysis using powder diffraction data'* (2008). *J. Appl. Cryst.* **41**(4), 815–817.
   [https://doi.org/10.1107/S0021889808016956](https://doi.org/10.1107/S0021889808016956)

## License

QualX is distributed under the **GNU Lesser General Public License v3.0 (LGPL-3.0)**. See [LICENSE](LICENSE) for the full text.

## Authors and acknowledgments

Developed and maintained at the **Institute of Crystallography, CNR (IC-CNR), Bari**, Italy.

## Contributing

Bug reports, feature requests, and merge requests are welcome. Please open an issue describing the problem or proposal before submitting larger changes.

## Support

For questions or issues, please use the project's issue tracker, or contact the maintainer (corrado.cuocci@cnr.it).
