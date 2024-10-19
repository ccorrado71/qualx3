#ifndef __LIBMATH_H
#define __LIBMATH_H
#include <QColor>
#include <cmath>
#include <algorithm>
//#define MRabs(x)                 ( (x >= 0) ? x : -(x))
#define MRsign(x,y)               ( ((y) >= (0)) ? (abs(x)) : -(abs(x)) )

#ifndef M_PI
#define  M_PI 3.14159265358979323846
#endif /* M_PI */
                                                                                    
#define DEGTORAD(a) (2.0*M_PI*a/360.0)
#define RADTODEG(a) (360.0*a/(2*M_PI))
#define IsZero(x)   ((abs(x) < 1e-9)?true:false)
#define Cos(th)     cos(M_PI/180*(th))
#define Sin(th)     sin(M_PI/180*(th))
static const float EPS=static_cast <float>(1e-6);


typedef union {
                struct { double x, y, z; };
                double v[3];
              } vet3;

typedef union {
                struct { double x, y, z, w; };
                double v[4];
              } vet4;

typedef union
{
    struct
    {
        double m00, m01, m02;
        double m10, m11, m12;
        double m20, m21, m22;
    };
    double m[9];
} mat3x3;

typedef union
{
    struct
    {
        double m00, m01, m02, m03;
        double m10, m11, m12, m13;
        double m20, m21, m22, m23;
        double m30, m31, m32, m33;
    };
    double m[16];
} mat4x4;

typedef struct {
                 double v[6];
               } cell_type;

typedef struct {
               float x, y, z;
               float c[4];
               } Vert;

static const vet3   MRVectorZero = {{0, 0, 0}};
static const vet4   MRVector4Identity = {{ 0, 0, 0, 1}};
static const mat3x3 MRMatrix3Identity = {{ 1, 0, 0,
                                          0, 1, 0,
                                          0, 0, 1 }};

static const mat4x4 MRMatrixIdentity = {{ 1, 0, 0, 0,
                                         0, 1, 0, 0,
                                         0, 0, 1, 0,
                                         0, 0, 0, 1 }};
void CalcMats(float cella[], mat3x3 *mat, mat3x3 *matI,
              float cells[], mat3x3 *matR, bool cInv,
              mat3x3 *om=NULL, mat3x3 *omI=NULL);

mat4x4 MRCylinderMatrix(double *l, vet3 p1, vet3 p2, vet3 p3);
mat4x4 MRCylinderMatrix(double *l, vet3 p1, vet3 p2);
vet4 MRTrackBall(double p1x, double  p1y, double p2x, double p2z);

float Distanza (const float *p1, const float *p2);
float Angle (const float *p1, const float *p2);
float Angolo (const float *p1, const float *p2, const float *p3);
float Torsione (const float *p1, const float *p2, const float *p3, const float *p4);
float calcDistAng (float **p, int op);
void print_vector(vet3 v, const char *nome);
Vert norm(Vert a, Vert b, Vert c);
void autov(double a[3][3], double eigen[3], double t[3][3], int *icod);
void Desaturate(float color[]);
void TuneHSV(float color[], float inten);
void TuneH(int nTot, int  frac, float color[]);
void TuneH(float nTot, float  frac, float color[]);
void TuneHSV(QColor color, float inten);
void TuneH(int nTot, int  frac, QColor color);
void TuneH(float nTot, float  frac, QColor color);
void Desaturate(QColor color);
QColor Darker(QColor color1);
QColor Lighter(QColor color2);
QColor Blend(QColor color1, QColor color2);
void BuildRect(Vert *v, float p1[], float p2[]);
#endif
