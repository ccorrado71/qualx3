# Installation

## Requirements

- **Operating System**: Linux (64-bit), Windows 10/11
- **Qt**: 6.5 or later
- **Disk space**: ~50 MB (application) + database files

## Linux

### From binary package

1. Download the latest release from the project page.
2. Extract the archive:
   ```bash
   tar xzf qualx3-<version>-linux64.tar.gz
   cd qualx3-<version>
   ```
3. Run the installer or copy files manually:
   ```bash
   ./install.sh          # system-wide (requires root)
   # or
   ./install.sh --local  # install in ~/qualx3
   ```

### From source

```bash
git clone https://baltig.cnr.it/corrado.cuocci/qualx3.git
cd qualx3
cmake -DCMAKE_PREFIX_PATH=/path/to/Qt/6.5.0/gcc_64 \
      -DCMAKE_BUILD_TYPE=Release \
      -S . -B build
cmake --build build
```

## Windows

1. Download the Windows installer (`.exe`) from the project page.
2. Run the installer and follow the on-screen instructions.
3. QualX will be available from the Start Menu.

## Databases

QualX requires at least one crystallographic reference database.

| Database | Format | Notes |
|----------|--------|-------|
| COD (Crystallography Open Database) | COD/CIF | Free, bundled |
| PDF-2 | PDF-2 DAT | Commercial, ICDD license required |

Database files are managed through **Search → Manage Databases**.

## First Launch

On first launch, QualX will ask you to configure the path to the database files.
See [Getting Started](introduction/getting-started.md) for a step-by-step guide.
