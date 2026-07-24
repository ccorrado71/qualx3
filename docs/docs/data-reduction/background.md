# Background

## Overview

Background estimation is performed by fitting a polynomial (Chebyshev) or
filter-based curve to the diffraction pattern. The estimated background can
be visualised and subtracted before peak search.

## Controls

Open **Pattern → Background → Generate Background** to display the Background dialog.

| Parameter | Description |
|-----------|-------------|
| **Type** | Background model: Chebyshev polynomial or filter |
| **Coefficients** | Number of polynomial terms (Chebyshev mode) |
| **Range** | 2θ range to use for background fitting |

It is possible to choose the background function that best fits the selected
background points. The default function is the Chebyshev polynomial. The
other available background functions are: polynomial, cosine Fourier series,
cubic spline, Bézier spline, and a filter function based on the Brückner
algorithm (Brückner, 2000).

## Manual editing

Background points selected automatically can be added or deleted using the mouse.

## Subtraction

Click **Pattern → Subtract Background** (or press the button in the toolbar)
to subtract the estimated background from the pattern.
The original pattern is preserved and can be restored by reloading the project.

!!! note
    Background subtraction is recommended before running Peak Search, as it
    improves peak detection accuracy.
