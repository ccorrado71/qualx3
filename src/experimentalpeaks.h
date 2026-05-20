#pragma once

#include <QVector>

struct ExperimentalPeaks {
    QVector<double> d;
    QVector<double> deltaD;
    QVector<double> tth;
    QVector<double> intensity;
    QVector<double> fwhm;
    double          wave  = 0.0;
    bool            valid = false;

    void clear()
    {
        d.clear(); deltaD.clear(); tth.clear(); intensity.clear(); fwhm.clear();
        wave = 0.0; valid = false;
    }
};
