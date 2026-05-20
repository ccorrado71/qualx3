#include "peakassoc.h"

#include <cmath>
#include <limits>

// Returns the 0-based index of the element in 'arr' with the minimum value that is
// also <= threshold, or -1 if no element satisfies the condition.
static int minlocWithin(const QVector<double> &arr, double threshold)
{
    int    best = -1;
    double minVal = std::numeric_limits<double>::max();
    for (int j = 0; j < arr.size(); ++j) {
        if (arr[j] <= threshold && arr[j] < minVal) {
            minVal = arr[j];
            best   = j;
        }
    }
    return best;
}

QVector<PeakAssociation> associatePeaks(
    const QVector<double> &expTth,
    const QVector<double> &expIntensity,
    const QVector<double> &cardTth,
    const QVector<double> &cardIntensity,
    double delta)
{
    const int nsp = expTth.size();   // number of experimental peaks
    const int ndb = cardTth.size();  // number of database (card) peaks

    QVector<PeakAssociation> result(nsp); // default: dbPeakIndex = -1, quality = 0

    // Compute the association quality between experimental peak i and database peak j.
    // Formula: (1 - |Δ2θ|/delta) * (1 - |ΔI|/1000).
    // Intensities are assumed to be on a 0-1000 scale; quality can be negative if
    // the intensity difference exceeds 1000.
    auto quality = [&](int i, int j, double diffPos) {
        return (1.0 - diffPos / delta)
             * (1.0 - std::abs(expIntensity[i] - cardIntensity[j]) / 1000.0);
    };

    // Precomputed distance buffers (reused each iteration to avoid repeated allocations).
    QVector<double> diffDB(ndb);   // distances from exp peak i to all card peaks
    QVector<double> diffExp(nsp);  // distances from card peak to all exp peaks

    for (int i = 0; i < nsp; ++i) {
        // --- Step 1: find the closest card peak to experimental peak i ---
        for (int j = 0; j < ndb; ++j)
            diffDB[j] = std::abs(expTth[i] - cardTth[j]);

        const int locmin = minlocWithin(diffDB, delta);
        if (locmin < 0)
            continue; // no card peak within the tolerance window

        // --- Step 2: check which experimental peak is closest to cardTth[locmin] ---
        for (int k = 0; k < nsp; ++k)
            diffExp[k] = std::abs(expTth[k] - cardTth[locmin]);

        const int locmin1 = minlocWithin(diffExp, delta);

        if (locmin1 == i) {
            // Current experimental peak is the unique best match → associate directly.
            result[i] = {locmin, quality(i, locmin, diffDB[locmin])};
        } else {
            // Another experimental peak (locmin1) is closer to cardTth[locmin].
            // Check whether locmin1 has an even better match at a different card peak.
            for (int j = 0; j < ndb; ++j)
                diffDB[j] = std::abs(expTth[locmin1] - cardTth[j]);

            const int locmin2 = minlocWithin(diffDB, delta);

            // If locmin1's best card peak differs from locmin, peak i can claim locmin.
            if (locmin2 != locmin) {
                const double d = std::abs(expTth[i] - cardTth[locmin]);
                result[i] = {locmin, quality(i, locmin, d)};
            }
        }
    }

    return result;
}
