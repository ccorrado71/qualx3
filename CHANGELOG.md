# Changelog

All notable changes to qualx are documented in this file.

## [Unreleased]

## [1.0.5] - 2026-08-24

### Added
- HTML documentation is now built and published automatically to GitHub Pages (https://ccorrado71.github.io/qualx3/) via a GitHub Actions workflow; Help > Documentation (HTML) opens it directly instead of searching for a local build.
- Documentation homepage now links directly to the software and database download pages.
- Experimental peaks are now passed to Fortran and displayed on the pattern.

### Changed
- Unified the `info` table schema and location across database types: it now always lives in `.sq.info`, with the same columns (including `natoms`/`nreflections`) for both Pdf2 and CifFiles/COD databases. Pdf2 databases built with a previous version must be regenerated (`qualx --createdb --pdf2 ...` or Search > Manage Databases).

### Fixed
- Cell parameters (a, b, c, alpha, beta, gamma) and h/k/l reflections never shown in the card browser for CifFiles/COD-type databases: `QualxDbManager::queryCard` always queried the `info` table in the main `.sq` database, but for COD-type databases that table lives in `.sq.info`.
- Database open error message now includes the file path.
- macOS DMG packaging missing the HTML documentation and PDF manual.

## [1.0.4] - 2026-07-29

### Added
- "Recalculate FOMs" action to the Search menu.
- `qualx --search <file>` on the command line now runs the same peak-based Search & Match used by the GUI's Search > Match action, instead of a composition-only query.
- AppImage packaging for Linux, with bundled Qt plugins and platform theme for Qt < 6.5.

### Fixed
- qualx launched from the GNOME dash failing to start when built with Intel Fortran (ifx/ifort).
- Bug preventing search from running with `--nogui`.
- Bundled Qt plugins loading system Qt libraries instead of the ones shipped with the app.
- AppImage missing the GTK3 platform theme and its transitive dependencies.
- Packaged tarballs missing the HTML documentation.
- macOS app signing and DMG packaging broken on Apple Silicon.

## [1.0.3] - 2026-07-09

### Fixed
- Strongest-peaks screening in database search ignored the 2theta tolerance configured in `SearchOptionsDialog` (`spinDelta`) and used a hardcoded value converted with the wrong (inverse) formula, causing the tolerance to be wildly oversized and the screening to accept almost every card.

## [1.0.2] - 2026-07-09

### Changed
- Optimized the `computeFOM` hot path for full-database search.

### Fixed
- Background not respecting the selected number of coefficients: the coefficient slider in `BackgroundCoefWidget` had no effect on the computed background in manual mode.
- Search returning no results when "strongest peaks" is unchecked.

## [1.0.1] - 2026-07-08

### Added
- Peak deletion when "New" is selected.

### Fixed
- Maximum number of peaks after background subtraction.
- `peakCompareWidget` not syncing with result list phase changes.
- qualx aborting when no display is available (e.g. over SSH).
- Disabled menus in CLI mode; removed unused `fileout` parameter from `qualxmain`.

## [1.0.0] - 2026-07-02

### Added
- First public release of qualx.
