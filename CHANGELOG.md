# Changelog

All notable changes to qualx are documented in this file.

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
