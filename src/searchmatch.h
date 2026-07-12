#pragma once

#include "dbquerybuilder.h"
#include "experimentalpeaks.h"

// Peak search + FOM-based database query, shared between the GUI's
// "Search > Match" action (MainWindow::onActionSearchMatchTriggered) and the
// --search command-line mode used when a data file is given without a GUI.

// Runs a peak search on the currently loaded pattern if no peaks exist yet.
// Returns the number of peaks found (0 if none, even after searching).
int ensurePeaksFound();

// Reads experimental peaks from Fortran and stores them in AppState::peaks().
// Returns the number of peaks (0 if none).
int loadExperimentalPeaks();

// Builds a DbQueryBuilder configured for a peak-based Search & Match query
// using the given experimental peaks and the persisted Search Options settings
// (min FOM, weights, delta 2theta). Composition/restraints are left to the
// caller to add afterwards.
DbQueryBuilder buildSearchMatchQuery(const ExperimentalPeaks &ep);
