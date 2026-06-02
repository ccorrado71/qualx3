# Getting Started

## Step 1 — Configure the database

Before running the first search, add at least one reference database:

1. Open **Search → Manage Databases**
2. Click **Add existing** and browse to your `.sq` database file
3. Click **OK** — the database is now active

## Step 2 — Import a diffraction pattern

1. **File → Import Diffraction Pattern** (or drag-and-drop a file onto the graphic area)
2. Supported formats: `.xy`, `.dat`, `.xrdml`, `.gda`, `.xye`, `.rtv`
3. The pattern appears in the graphic area

## Step 3 — Background

1. **Pattern → Background → Generate Background**
2. Adjust parameters in the Background dialog
3. Optionally subtract: **Pattern → Subtract Background**

## Step 4 — Peak Search

1. **Pattern → Peaks → Peak Search**
2. The detected peaks appear as vertical markers in the graphic area
3. Fine-tune with **Pattern → Peaks → Peak Search Conditions**

## Step 5 — Search & Match

1. **Search → Search & Match**
2. QualX queries the database and shows ranked results in the **Results List**
3. Select a card to view its reflections overlaid on the experimental pattern

## Step 6 — Accept a phase

1. Select a card in the Results List
2. Click **Accept** (✓ button) — the phase moves to the **Quantitative** panel
3. QualX automatically performs a residual search on remaining peaks

## Step 7 — Save the project

**File → Save Project As** saves a `.qxp` file containing the pattern data,
peaks, results and accepted phases.
