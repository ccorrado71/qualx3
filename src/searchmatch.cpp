#include "searchmatch.h"
#include "appstate.h"
#include "searchoptionsdialog.h"

extern "C" void apply_background_subtraction();
extern "C" void run_peaksearchwin();
extern "C" int peak_number();
extern "C" void get_d_delta_values(float dval[], float deltadval[], float tthval[], float intval[], float fwhmval[], double *wave, double delta2theta);

int ensurePeaksFound()
{
    apply_background_subtraction();
    int npeaks = peak_number();
    if (npeaks == 0) {
        run_peaksearchwin();
        npeaks = peak_number();
    }
    return npeaks;
}

int loadExperimentalPeaks()
{
    const int n = peak_number();
    if (n <= 0) {
        AppState::peaks().clear();
        return 0;
    }
    float *dval      = new float[n];
    float *deltadval = new float[n];
    float *tthval    = new float[n];
    float *intval    = new float[n];
    float *fwhmval   = new float[n];
    double wave;
    get_d_delta_values(dval, deltadval, tthval, intval, fwhmval, &wave, SearchOptionsDialog::savedDelta2theta());

    ExperimentalPeaks &ep = AppState::peaks();
    ep.d.resize(n); ep.deltaD.resize(n);
    ep.tth.resize(n); ep.intensity.resize(n); ep.intensityOrig.resize(n); ep.fwhm.resize(n);
    ep.wave  = wave;
    ep.valid = true;
    for (int i = 0; i < n; ++i) {
        ep.d[i]             = dval[i];
        ep.deltaD[i]        = deltadval[i];
        ep.tth[i]           = tthval[i];
        ep.intensity[i]     = intval[i];
        ep.intensityOrig[i] = intval[i]; // original value, never subtracted
        ep.fwhm[i]          = fwhmval[i];
    }
    delete[] dval; delete[] deltadval; delete[] tthval; delete[] intval; delete[] fwhmval;
    return n;
}

DbQueryBuilder buildSearchMatchQuery(const ExperimentalPeaks &ep)
{
    DbQueryBuilder builder;
    builder.setPrintEnabled(true);
    builder.setDValues(ep.d, ep.deltaD);
    builder.setWave(ep.wave);
    builder.setCalcFom(true);
    builder.setMinFom(SearchOptionsDialog::savedMinFom());
    builder.setWeight2thetaD(SearchOptionsDialog::savedWeight2thetaD());
    builder.setWeightIntensity(SearchOptionsDialog::savedWeightIntensity());
    builder.setWeightPhases(SearchOptionsDialog::savedWeightPhases());
    builder.setDelta2theta(SearchOptionsDialog::savedDelta2theta());
    return builder;
}
