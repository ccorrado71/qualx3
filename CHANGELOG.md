# Changelog

All notable changes to qualx are documented in this file.

## [Unreleased]

## [1.0.8] - 2026-10-21

### Changed
- Help > Check for Updates on Linux and macOS now looks up the latest release on GitHub instead of the old ba.ic.cnr.it website, and reports a failed check (e.g. no connection) instead of claiming that the latest version is installed.

### Fixed
- Selected rows in the results list painted white on Linux and macOS (regression in 1.0.7).
- New, Import Diffraction Pattern, Save Project and Save Project As shown without icon (as text in the toolbar) in the Linux AppImage.
- Crash when importing pdCIF data given as d-spacings.
- Build failing on macOS (Clang).

## [1.0.7] - 2026-10-19

### Added
- "Export as CIF..." for COD entries.
- Experimental peaks and the reflections of the selected card are now shown as bars below the pattern.
- The main plot legend now shows the card ID.
- Background points are now saved in the project file (`.qxp`), so the background can still be edited after reopening a project.

### Changed
- Windows installer: no more Start menu folder page and no Uninstall shortcut (uninstall from Settings > Installed apps)
- The pie chart in the quantitative analysis now shows percentages as inline labels instead of a separate legend.
- Restraints dialog: for COD and user databases only the Inorganic and Organic subfiles can be selected; the other subfiles, available only in PDF-2 databases, are disabled.
- Database downloads now point to Zenodo (https://zenodo.org/records/23297507): `cod_inorg.zip` (inorganic phases) and `cod.zip` (whole COD).
- The database installation page now spells out the default `QualXDB` folder for each platform.
- Packages no longer bundle the HTML documentation (available online), only the PDF manual.

### Fixed
- Results table on Windows 11: the cell under the mouse and the last clicked cell looked selected
- Wavelength combo box not editable when the dialog opens, and pasted wavelengths with more than 8 decimals being discarded.
- Linux AppImage and source installation issues: the AppImage crashing with "Illegal instruction" on CPUs lacking the build machine's instruction sets (the Fortran code is now compiled for generic x86-64), failing with "undefined symbol" errors when `LD_LIBRARY_PATH` points to another Qt installation or when loading the host's GIO modules, and `cmake --install` failing with "No valid ELF RPATH or RUNPATH entry" when building with the distribution's Qt (a `/opt` install now uses the system Qt). Many thanks to Roberto de Avillez for testing and reporting these problems in detail.
- Windows installer never showing the license page; it now shows the GPLv3, replacing an outdated license agreement that was bundled with it.
- Build with Intel Fortran (ifx) failing at link time ("relocation R_X86_64_32S ... can not be used when making a PIE object").
- PDF manual not opening from the AppImage with sandboxed (e.g. snap) viewers.
- Crash when adding a background point after opening a project in which only the background had been computed.
- Experimental peak bars not updated when the peak search conditions change.
- Duplicate legend entry when an accepted phase is also selected.
- "Selected_Peaks" legend entry not removed when the peak selection is cleared.
- Search > Restraints "Load & Merge" emptying the peak comparison table and the quantitative report.
- Experimental peak bars overlapping the impulse-style peak lines.
- File > New not fully clearing the report and the card browser title.
- Reflection bars and pie chart not cleared or restored on New/Load project.
- Save Project asking for a file name after Open Project.
- Default toolbar order and Quantitative dock placement.

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
