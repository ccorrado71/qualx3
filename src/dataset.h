#ifndef DATASET_H
#define DATASET_H

typedef struct {
    float           zero;
    float           sdisp;
    float           stran;
    int             zcode,sdcode,stcode;
    int             bcode;
} dataset_t;

typedef struct {
    float par[10];
    int rcod[10];
} c_profile_function;

typedef struct {
    int btype;
    bool autob;
    int ncoef;
    int niterf;
    int nwinf;
    double minf;
    double maxf;
} BackgroundSettings;

#endif // DATASET_H
