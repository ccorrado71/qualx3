#pragma once

#include <QVector>

struct ExperimentalPeaks {
    QVector<double> d;
    QVector<double> deltaD;
    QVector<double> tth;
    QVector<double> intensity;     // current (residual) intensity, modified by performResidualSearch
    QVector<double> intensityOrig; // original intensity at load time, never subtracted
    QVector<double> fwhm;
    double          wave  = 0.0;
    bool            valid = false;

    void clear()
    {
        d.clear(); deltaD.clear(); tth.clear();
        intensity.clear(); intensityOrig.clear(); fwhm.clear();
        wave = 0.0; valid = false;
    }
};
