# CLAUDE.md — qualx3

## Progetto

Applicazione Qt6/C++ + Fortran per l'analisi qualitativa di spettri di polveri, con database di riferimento e funzionalità di creazione database da PDF-2 o CIF.

## Stack tecnologico

- **C++17** + **Qt6** (Widgets, Sql, PrintSupport, Network)
- **Fortran** (gfortran / ifort), con interoperabilità C++ via `iso_c_binding`
- **SQLite** per i database di riferimento cristallografico
- **CMake** ≥ 3.20

## Build

Configurazione (una sola volta):
```bash
cmake -DCMAKE_PREFIX_PATH=/home/corrado/Qt/6.5.0/gcc_64 \
      -DCMAKE_BUILD_TYPE=debug \
      -DCMAKE_Fortran_COMPILER=gfortran \
      -S . -B build_debug_gfortran
```

Build incrementale:
```bash
cmake --build build_debug_gfortran
# oppure semplicemente:
bash build.sh
```

Le cartelle di build sono:
- `build_gfor_d/` — gfortran debug
- `build_gfor_r/` — gfortran release
- `build_ifx_d/`  — ifx debug
- `build_ifx_r/`  — ifx release

## Struttura del progetto

```
src/           sorgenti C++ e Fortran
src/CMakeLists.txt   lista di tutti i sorgenti (PROJECT_SOURCES e FORTRAN_SOURCES)
share/qualx/   file dati runtime (syminfo.lib, AtomProperties.xen, ...)
DB/            database SQLite inclusi nell'installazione (es. DB/cod/cod_inorg)
configured/    file generati da CMake (config.h con versione)
```

## Convenzioni

### Aggiungere un file sorgente
Ogni nuovo `.cpp/.h/.f90/.F90` va aggiunto a `src/CMakeLists.txt`:
- File C++: in `set(PROJECT_SOURCES ...)`, in ordine alfabetico
- File Fortran: in `set(FORTRAN_SOURCES ...)`, in ordine alfabetico

### Interoperabilità Fortran ↔ C++
- Le subroutine Fortran chiamabili da C++ usano `bind(C, name='...')` e `use iso_c_binding`
- I tipi usati: `integer(C_INT)`, `real(C_FLOAT)`, `character(kind=C_CHAR)`
- Le stringhe C→Fortran si convertono con `toFortranString` (modulo `strutil`)
- Le stringhe Fortran→C si convertono con `copy_string_to_c_array` (modulo `strutil`)
- Le dichiarazioni `extern "C"` e i wrapper C++ stanno in `libfor.cpp`
- Le dichiarazioni pubbliche (struct, funzioni) stanno in `libcomune.h`

### Stato applicazione
- `AppState` (appstate.h/cpp): stato globale accessibile da GUI e CLI
  - `AppState::load()` — carica i database da QSettings (chiamato in `main.cpp`)
  - `AppState::db()` — restituisce il `QualxDbManager` aperto sul database attivo
  - `AppState::setDatabases(...)` — aggiorna la lista e riapre il DB se cambia quello attivo

### Database SQLite
Ogni database Qualx è composto da 4 file:
- `<name>.sq` — dati principali (id, chemical, subfiles, ...)
- `<name>.sq.info` — dati cristallografici e bibliografici
- `<name>.sq.infostat` — tabelle statistiche
- `<name>.sq.search` — indice di ricerca (top d-values)

Le classi coinvolte:
- `QualxDbCreator` — crea lo schema (due varianti: Pdf2, CifFiles)
- `QualxDbPopulator` — popola da file PDF-2
- `CifDbPopulator` — popola da file CIF (via Fortran `get_crystal_info_from_cif`)
- `QualxDbManager` — esegue query su un database aperto

### Creazione database da riga di comando
```bash
# Da file PDF-2:
qualx --createdb --pdf2 /path/to/pdf2.dat --dbout /path/to/output

# Da cartella CIF:
qualx --createdb --cifdir /path/to/cifs [--recursive] --dbout /path/to/output
```

## Convenzioni UI

- **Preferire il file `.ui`** per definire widget e layout. Evitare di costruire widget programmaticamente in C++ (es. `setupXxxTab()`) quando è possibile farlo nel file `.ui`. Il codice C++ deve solo leggere i widget tramite `ui->nomeWidget`.

## Note importanti

- Gli errori di diagnostica con codice **1696** sono **solo IntelliSense** (Qt headers non trovati nella configurazione IDE). Non sono errori di compilazione reali.
- `load_chemical_tables(exepath, err)` deve essere chiamata prima di qualsiasi lettura CIF con `get_crystal_info_from_cif`. In modalità CLI viene chiamata tramite `initQualxTables()` in `main.cpp`.
- I moduli Fortran copiati da `expo2` che mancavano in qualx3 si trovano in `src/` (asymfunc, calcpdp, cryutil, ecc.).
- `progtype.F90` in qualx3 non contiene `refine_condition_type` — problema noto, gestito separatamente.
