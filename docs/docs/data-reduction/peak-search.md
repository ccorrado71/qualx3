# Peak Search

## Overview

QualX locates peaks in the diffraction pattern and records their 2θ position,
intensity and FWHM. These peaks are used as input for the Search & Match.

## Running Peak Search

**Pattern → Peaks → Peak Search** runs the automatic peak search with the
current conditions. Detected peaks appear as vertical markers in the graphic area
and are listed in the **Peaks** dock.

## Peak Search Conditions

**Pattern → Peaks → Peak Search Conditions** opens the conditions dialog.

| Parameter | Description |
|-----------|-------------|
| **Min. 2θ / Max. 2θ** | Search range |
| **Threshold** | Minimum peak height relative to the pattern maximum |
| **Sensitivity** | Controls smoothing before peak detection |
| **Max. peaks** | Maximum number of peaks to detect |
| **Append** | Add new peaks to the existing list instead of replacing it |

## Manual editing

- **Add peak**: View → Selection Mode, then right-click on the pattern
- **Delete peak**: select a peak marker and press Delete
- **Load / Save**: Pattern → Peaks → Load Peaks / Save Peaks

!!! tip
    If too many or too few peaks are found, adjust **Threshold** and
    **Sensitivity** in the Peak Search Conditions dialog.
