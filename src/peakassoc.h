#pragma once

#include <QVector>

// Result of associating one experimental peak to the card's peak list.
struct PeakAssociation {
    int    dbPeakIndex = -1;  // 0-based index in the card array; -1 = no association found
    double quality     = 0.0; // association quality: (1 - Δ2θ/delta) * (1 - ΔI/1000)
};

// Associates each experimental peak to the closest database peak within a 2theta window.
//
// For each experimental peak i the function searches for the nearest database peak j
// such that |x1[i] - x2[j]| <= delta.  A conflict-resolution step checks whether
// another experimental peak is even closer to x2[j]; if so, peak i is associated only
// when that competing peak has a better match elsewhere.
//
// Parameters:
//   expTth      – 2theta positions of experimental peaks
//   expIntensity – intensities of experimental peaks
//   cardTth      – 2theta positions of the card's reflections
//   cardIntensity– intensities of the card's reflections (normalised to 0-1000 scale)
//   delta        – half-window tolerance in 2theta degrees
//
// Returns one PeakAssociation per experimental peak (same size as expTth).
QVector<PeakAssociation> associatePeaks(
    const QVector<double> &expTth,
    const QVector<double> &expIntensity,
    const QVector<double> &cardTth,
    const QVector<double> &cardIntensity,
    double delta);
