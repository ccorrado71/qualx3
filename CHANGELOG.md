# Changelog

All notable changes to qualx are documented in this file.

## [Unreleased]

### Fixed
- Background not respecting the selected number of coefficients: the coefficient slider in `BackgroundCoefWidget` had no effect on the computed background in manual mode.

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
