# Database Installation

QualX requires at least one crystallographic reference database in its own
compiled format (`.sq` file). Database files are not included in the QualX
installation and must be downloaded separately.

## Available databases

Pre-built databases are available for download from the
[QualX Databases page](https://www.ba.ic.cnr.it/content/old/qualx3/Databases/).

| File | Contents | Size |
|------|----------|------|
| `cod_inorg.zip` | COD — inorganic phases only | 318.4 MB |
| `cod.zip` | COD — complete database | 2.7 GB |

Each archive contains a single folder (`cod_inorg` or `cod`) with all the
required files. Extract it into your `QualxDB` folder following the
instructions below.

## Default database folder

QualX looks for databases in a dedicated folder called **QualxDB** inside your
home directory:

| Platform | Default path |
|----------|-------------|
| Linux / macOS | `~/QualxDB` |
| Windows | `C:\Users\<username>\QualxDB` |

**At every launch** QualX scans this folder recursively for `.sq` files and
registers any database it finds automatically — no manual configuration needed.
You can organise the folder freely: subfolders are fully supported.

## Setting up the QualxDB folder

1. Create the `QualxDB` folder in your home directory.
2. Download one of the zip archives from the link above and extract it
   directly into `QualxDB`. After extraction the folder should look like:
   ```
   ~/QualxDB/
   └── cod_inorg/          (or cod/)
       ├── cod_inorg.sq
       ├── cod_inorg.sq.info
       ├── cod_inorg.sq.infostat
       └── cod_inorg.sq.search
   ```
3. Launch QualX — the database is detected and registered automatically.

## What happens if the folder is not found

If QualX starts without any configured database and the `QualxDB` folder does
not exist, a dialog is shown with two options:

- **Choose folder…** — lets you select a different folder that will be saved
  as the new default. QualX scans it immediately and registers all databases
  found inside.
- **Manage Databases later** — dismiss the dialog and configure databases
  manually via **Search → Manage Databases**.

## Adding a database manually

If you prefer not to use the default folder, or need to register an additional
database stored elsewhere:

1. Open **Search → Manage Databases**
2. Click **Add existing** and browse to your `.sq` database file
3. Click **OK** — the database is now active
