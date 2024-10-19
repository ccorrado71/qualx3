#include <cstdio>
#include <cstring>
#include "libmath.h"
#include "libmatrix.h"
#include "libvector.h"
using namespace std;

mat4x4 MRMatrixInvert(mat4x4 m, bool *isInvertible)
{
   mat4x4 im;
   *isInvertible = false;

   im.m[0] =   m.m[5]*m.m[10]*m.m[15] - m.m[5]*m.m[11]*m.m[14] - m.m[9]*m.m[6]*m.m[15]
            + m.m[9]*m.m[7]*m.m[14] + m.m[13]*m.m[6]*m.m[11] - m.m[13]*m.m[7]*m.m[10];
   im.m[4] =  -m.m[4]*m.m[10]*m.m[15] + m.m[4]*m.m[11]*m.m[14] + m.m[8]*m.m[6]*m.m[15]
            - m.m[8]*m.m[7]*m.m[14] - m.m[12]*m.m[6]*m.m[11] + m.m[12]*m.m[7]*m.m[10];
   im.m[8] =   m.m[4]*m.m[9]*m.m[15] - m.m[4]*m.m[11]*m.m[13] - m.m[8]*m.m[5]*m.m[15]
            + m.m[8]*m.m[7]*m.m[13] + m.m[12]*m.m[5]*m.m[11] - m.m[12]*m.m[7]*m.m[9];
   im.m[12] = -m.m[4]*m.m[9]*m.m[14] + m.m[4]*m.m[10]*m.m[13] + m.m[8]*m.m[5]*m.m[14]
            - m.m[8]*m.m[6]*m.m[13] - m.m[12]*m.m[5]*m.m[10] + m.m[12]*m.m[6]*m.m[9];
   im.m[1] =  -m.m[1]*m.m[10]*m.m[15] + m.m[1]*m.m[11]*m.m[14] + m.m[9]*m.m[2]*m.m[15]
            - m.m[9]*m.m[3]*m.m[14] - m.m[13]*m.m[2]*m.m[11] + m.m[13]*m.m[3]*m.m[10];
   im.m[5] =   m.m[0]*m.m[10]*m.m[15] - m.m[0]*m.m[11]*m.m[14] - m.m[8]*m.m[2]*m.m[15]
            + m.m[8]*m.m[3]*m.m[14] + m.m[12]*m.m[2]*m.m[11] - m.m[12]*m.m[3]*m.m[10];
   im.m[9] =  -m.m[0]*m.m[9]*m.m[15] + m.m[0]*m.m[11]*m.m[13] + m.m[8]*m.m[1]*m.m[15]
            - m.m[8]*m.m[3]*m.m[13] - m.m[12]*m.m[1]*m.m[11] + m.m[12]*m.m[3]*m.m[9];
   im.m[13] =  m.m[0]*m.m[9]*m.m[14] - m.m[0]*m.m[10]*m.m[13] - m.m[8]*m.m[1]*m.m[14]
            + m.m[8]*m.m[2]*m.m[13] + m.m[12]*m.m[1]*m.m[10] - m.m[12]*m.m[2]*m.m[9];
   im.m[2] =   m.m[1]*m.m[6]*m.m[15] - m.m[1]*m.m[7]*m.m[14] - m.m[5]*m.m[2]*m.m[15]
            + m.m[5]*m.m[3]*m.m[14] + m.m[13]*m.m[2]*m.m[7] - m.m[13]*m.m[3]*m.m[6];
   im.m[6] =  -m.m[0]*m.m[6]*m.m[15] + m.m[0]*m.m[7]*m.m[14] + m.m[4]*m.m[2]*m.m[15]
            - m.m[4]*m.m[3]*m.m[14] - m.m[12]*m.m[2]*m.m[7] + m.m[12]*m.m[3]*m.m[6];
   im.m[10] =  m.m[0]*m.m[5]*m.m[15] - m.m[0]*m.m[7]*m.m[13] - m.m[4]*m.m[1]*m.m[15]
            + m.m[4]*m.m[3]*m.m[13] + m.m[12]*m.m[1]*m.m[7] - m.m[12]*m.m[3]*m.m[5];
   im.m[14] = -m.m[0]*m.m[5]*m.m[14] + m.m[0]*m.m[6]*m.m[13] + m.m[4]*m.m[1]*m.m[14]
            - m.m[4]*m.m[2]*m.m[13] - m.m[12]*m.m[1]*m.m[6] + m.m[12]*m.m[2]*m.m[5];
   im.m[3] =  -m.m[1]*m.m[6]*m.m[11] + m.m[1]*m.m[7]*m.m[10] + m.m[5]*m.m[2]*m.m[11]
            - m.m[5]*m.m[3]*m.m[10] - m.m[9]*m.m[2]*m.m[7] + m.m[9]*m.m[3]*m.m[6];
   im.m[7] =   m.m[0]*m.m[6]*m.m[11] - m.m[0]*m.m[7]*m.m[10] - m.m[4]*m.m[2]*m.m[11]
            + m.m[4]*m.m[3]*m.m[10] + m.m[8]*m.m[2]*m.m[7] - m.m[8]*m.m[3]*m.m[6];
   im.m[11] = -m.m[0]*m.m[5]*m.m[11] + m.m[0]*m.m[7]*m.m[9] + m.m[4]*m.m[1]*m.m[11]
            - m.m[4]*m.m[3]*m.m[9] - m.m[8]*m.m[1]*m.m[7] + m.m[8]*m.m[3]*m.m[5];
   im.m[15] =  m.m[0]*m.m[5]*m.m[10] - m.m[0]*m.m[6]*m.m[9] - m.m[4]*m.m[1]*m.m[10]
            + m.m[4]*m.m[2]*m.m[9] + m.m[8]*m.m[1]*m.m[6] - m.m[8]*m.m[2]*m.m[5];

   double det = m.m[0]*im.m[0] + m.m[4]*im.m[1] + m.m[8]*im.m[2] + m.m[12]*im.m[3];
   if (IsZero(det))
       return MRMatrixIdentity;
   *isInvertible = true;
   return(MRMatrixDivideScalar(im, det));
}

mat3x3 MRMatrix3Invert(mat3x3 m, bool *result)
{
      *result = false;
      mat3x3 alfa = {{ m.m11*m.m22 - m.m12*m.m21,
                       m.m02*m.m21 - m.m01*m.m22,
                       m.m01*m.m12 - m.m02*m.m11,
                       m.m12*m.m20 - m.m10*m.m22,
                       m.m00*m.m22 - m.m02*m.m20,
                       m.m02*m.m10 - m.m00*m.m12,
                       m.m10*m.m21 - m.m11*m.m20,
                       m.m01*m.m20 - m.m00*m.m21,
                       m.m00*m.m11 - m.m01*m.m10 }};
      double det = m.m00*alfa.m00+m.m01*alfa.m01+m.m02*alfa.m02;
      if (IsZero(det))
         return MRMatrix3Identity;
      *result = true;
      return MRMatrix3DivideScalar(alfa, det);
}

void print_matrix(double Mat[16], const char *nome)
{
     printf("\n");
     printf("%s\n", nome);
     int k=0;
     for(int i=0; i<4; i++)
     {
         for(int j=0; j<4; j++)
             printf("%lf ", Mat[k++]);
         printf("\n");
     }
     printf("\n");
     printf("\n");
}

void print_vector(vet3 v, const char *nome)
{
     printf("%s\n", nome);
     for(int i=0; i<3; i++)
     {
         printf("%lf ", v.v[i]);
         printf("\n");
     }
}

double MRVectorAngle(vet3 a, vet3 b)
{
   vet3 ab = MRVectorCrossProduct(a, b);
   double psin = sqrt(MRVectorDotProduct(ab, ab));
   double pcos = MRVectorDotProduct(a, b);
   return 57.2958 * atan2(psin, pcos);
}

double MRVectorDihedral(vet3 a1,vet3 a2,vet3 a3,vet3 a4)
{
   vet3 r1 = MRVectorSub(a2, a1);
   vet3 r2 = MRVectorSub(a3, a2);
   vet3 r3 = MRVectorSub(a4, a3);
   vet3 n1 = MRVectorCrossProduct(r1, r2);
   vet3 n2 = MRVectorCrossProduct(r2, r3);

   double psin = MRVectorDotProduct(n1, r3) * sqrt(MRVectorDotProduct(r2, r2));
   double pcos = MRVectorDotProduct(n1, n2);

   return 57.2958 * atan2(psin, pcos);
}

mat3x3 MatriceMetrica(float Cell[])
{
     const double rad = M_PI / 180.0;
     mat3x3 m;
     for(int i=0; i<3; i++)
         m.m[4*i] = Cell[i] * Cell[i];
     m.m[1] = Cell[0] * Cell[1] * (float) cos(Cell[5] * rad);
     m.m[2] = Cell[0] * Cell[2] * (float) cos(Cell[4] * rad);
     m.m[5] = Cell[1] * Cell[2] * (float) cos(Cell[3] * rad);
     m.m[6] = m.m[2];
     m.m[3] = m.m[1];
     m.m[7] = m.m[5];
     return m;
}

double Det(mat3x3 m)
{
      double d = m.m[0]*m.m[4]*m.m[8] +
                 m.m[1]*m.m[5]*m.m[6] +
                 m.m[2]*m.m[3]*m.m[7] -
                 m.m[6]*m.m[4]*m.m[2] -
                 m.m[7]*m.m[5]*m.m[0] -
                 m.m[8]*m.m[3]*m.m[1];
      return d;
}

cell_type CellaReciproca(float Cell[])
{
     cell_type cells;
     const double rad = M_PI / 180.0;
     mat3x3  MM = MatriceMetrica(Cell);
     double vol = sqrt(Det(MM));
     double s1 = sin(Cell[3]*rad);
     double c1 = cos(Cell[3]*rad);
     double s2 = sin(Cell[4]*rad);
     double c2 = cos(Cell[4]*rad);
     double s3 = sin(Cell[5]*rad);
     double c3 = cos(Cell[5]*rad);
     cells.v[0] = Cell[1]*Cell[2]*s1 / vol;       // a*
     cells.v[1] = Cell[0]*Cell[2]*s2 / vol;       // b*
     cells.v[2] = Cell[0]*Cell[1]*s3 / vol;       // c*
     cells.v[3] = (c2*c3-c1)/(s2*s3);             // cos(alpha*)
     cells.v[4] = (c1*c3-c2)/(s1*s3);             // cos(beta*)
     cells.v[5] = (c1*c2-c3)/(s1*s2);             // cos(gamma*)
     return cells;
}

mat3x3 MatriceReciproca(float cell[])
{
     mat3x3 matR;
     cell_type cells = CellaReciproca(cell);
     for(int i=0; i<3; i++)
         matR.m[4*i] = cells.v[i] * cells.v[i];
     matR.m[1] = cells.v[0] * cells.v[1] * cells.v[5];
     matR.m[2] = cells.v[0] * cells.v[2] * cells.v[4];
     matR.m[5] = cells.v[1] * cells.v[2] * cells.v[3];
     matR.m[6] = matR.m[2];
     matR.m[3] = matR.m[1];
     matR.m[7] = matR.m[5];
     return  matR;
}

void CalcMats(float cella[], mat3x3 *mat, mat3x3 *matI,
              float cells[], mat3x3 *matR, bool cInv,
              mat3x3 *om, mat3x3 *omI)
{
      double  a  = (double) cella[0];
      double  b  = (double) cella[1];
      double  c  = (double) cella[2];
      double  alf = M_PI/180 * (double) cella[3];
      double  bet = M_PI/180 * (double) cella[4];
      double  gam = M_PI/180 * (double) cella[5];

      double sgs=(cos(alf)*cos(bet)-cos(gam))/(sin(alf)*sin(bet));

      if ( sgs >  1.0 ) sgs = 1.0;
      if ( sgs < -1.0 ) sgs =-1.0;
      if(cInv)
      {
         double sas=(cos(bet)*cos(gam)-cos(alf))/(sin(bet)*sin(gam));
         double sbs=(cos(alf)*cos(gam)-cos(bet))/(sin(alf)*sin(gam));
         if ( sas >  1.0 ) sas = 1.0;
         if ( sas < -1.0 ) sas =-1.0;
         double alfs=acos(sas);
         if ( sbs >  1.0 ) sbs = 1.0;
         if ( sbs < -1.0 ) sbs =-1.0;
         double bets=acos(sbs);
         double v = a * b * c;
         v = v*sqrt(sin(alf)*sin(alf) + sin(bet)*sin(bet) + sin(gam)*sin(gam)
                    - 2 +  2*cos(alf)*cos(bet)*cos(gam));
         double astar = b*c*sin(alf)/v;
         double bstar = a*c*sin(bet)/v;
         double cstar = a*b*sin(gam)/v;

         matI->m[0] = 1./a;
         matI->m[1] = -cos(gam)/(a*sin(gam));
         matI->m[2] = astar*cos(bets);
         matI->m[3] = 0;
         matI->m[4] = 1./(b*sin(gam));
         matI->m[5] = bstar*cos(alfs);
         matI->m[6] = 0;
         matI->m[7] = 0;
         matI->m[8] = cstar;
         bool result;
         *mat = MRMatrix3Invert(*matI, &result);
      }
      else
      {
         double sgs=(cos(alf)*cos(bet)-cos(gam))/(sin(alf)*sin(bet));
         if ( sgs >  1.0 ) sgs = 1.0;
         if ( sgs < -1.0 ) sgs =-1.0;
         double gams=acos(sgs);

         mat->m[0] = a*sin(bet);
         mat->m[1] = -b*sin(alf)*cos(gams);
         mat->m[2] = 0;
         mat->m[3] = 0;
         mat->m[4] = b*sin(alf)*sin(gams);
         mat->m[5] = 0;
         mat->m[6] = a*cos(bet);
         mat->m[7] = b*cos(alf);
         mat->m[8] = c;
         bool result;
         *matI = MRMatrix3Invert(*mat, &result);
      }
      if(cells != NULL)
      {
         cell_type cellr = CellaReciproca(cella);
         for(int i=0; i<6; i++)
             cells[i] = (float) cellr.v[i];
         if(om != NULL)
         {
            om->m[0] = a;
            om->m[1] = b*cos(gam);
            om->m[2] = c*cos(bet);
            om->m[3] = 0;
            om->m[4] = b*sin(gam);
            om->m[5] = -c*sin(bet)*cells[3]; 
            om->m[6] = 0;
            om->m[7] = 0;
            om->m[8] = 1./cells[2];
         }
         if(omI != NULL)
         {
            omI->m[0] = 1/a;
            omI->m[1] = -cos(gam)/(a*sin(gam));
            omI->m[2] = cells[0]*cells[4];
            omI->m[3] = 0;
            omI->m[4] = 1/(b*sin(gam));
            omI->m[5] = cells[1]*cells[3];
            omI->m[6] = 0;
            omI->m[7] = 0;
            omI->m[8] = cells[2];
         }
      }
      if(matR != NULL)
         *matR = MatriceReciproca(cella);
}

vet4 MRMatrixToQuat(mat4x4 a)
{
  vet4 q;
  double trace = a.m[0] + a.m[5] + a.m[10] + 1;
  if( trace > 0 )
  {
    double s = 0.5 / sqrt(trace);
    q.v[0] = ( a.m[9] - a.m[6] ) * s;
    q.v[1] = ( a.m[2] - a.m[8] ) * s;
    q.v[2] = ( a.m[4] - a.m[1] ) * s;
    q.v[3] = 0.25 / s;
  }
  else
  {
    if ( a.m[0] > a.m[5] && a.m[0] > a.m[10] )
    {
      double s = 2.0 * sqrt( 1.0 + a.m[0] - a.m[5] - a.m[10]);
      q.v[0] = 0.5 * s;
      q.v[1] = (a.m[1] + a.m[4]) / s;
      q.v[2] = (a.m[2] + a.m[8]) / s;
      q.v[3] = (a.m[6] + a.m[9]) / s;
    }
    else if (a.m[5] > a.m[10])
    {
      double s = 2.0 * sqrt( 1.0 + a.m[5] - a.m[0] - a.m[10]);
      q.v[0] = (a.m[1] + a.m[4]) / s;
      q.v[1] = 0.5 * s;
      q.v[2] = (a.m[6] + a.m[9]) / s;
      q.v[3] = (a.m[2] + a.m[8]) / s;
    }
    else
    {
      double s = 2.0 * sqrt( 1.0 + a.m[10] - a.m[0] - a.m[5]);
      q.v[0] = (a.m[2] + a.m[8]) / s;
      q.v[1] = (a.m[6] + a.m[9]) / s;
      q.v[2] = 0.5 * s;
      q.v[3] = (a.m[1] + a.m[4]) / s;
    }
  }
  return MRVectorNormalizeQuat(q);
}

mat4x4 MRCylinderMatrix(double *l, vet3 p1, vet3 p2, vet3 p3)
{
   vet3 v = MRVectorSub(p2, p1);
   vet3 V = MRVectorNormalize(v);
   vet3 firstPerp;
   if(IsZero(MRVectorLength(p3)))
       firstPerp  = MRVectorFirstPerp(V);
   else
   {
       vet3 v1 = MRVectorSub(p3, p1);
       firstPerp  =  MRVectorCrossProduct(V, v1);
   }
   vet3 secondPerp = MRVectorCrossProduct(V, firstPerp);
   firstPerp = MRVectorNormalize(firstPerp);
   secondPerp = MRVectorNormalize(secondPerp);
   mat4x4 Mat = {{firstPerp.x, firstPerp.y, firstPerp.z, 0,
                  secondPerp.x, secondPerp.y, secondPerp.z, 0,
                  V.x, V.y, V.z, 0,
                  0, 0, 0, 1}};
   *l = MRVectorLength(v);
   return Mat;
}

#define RENORMCOUNT 97
vet4 MRAddQuats(vet4 q1, vet4 q2)
{
    static int count=0;
    vet3 tq1 = {{q1.x, q1.y, q1.z}};
    vet3 tq2 = {{q2.x, q2.y, q2.z}};
    vet3 t1 = MRVectorMultiplyScalar(tq1, q2.w);
    vet3 t2 = MRVectorMultiplyScalar(tq2, q1.w);
    vet3 t3 = MRVectorCrossProduct(tq2, tq1);
    vet3 tf = MRVectorAdd(t1, t2);
    tf = MRVectorAdd(t3, tf);
    vet4 vect = {{tf.x, tf.y, tf.z, q1.w * q2.w - MRVectorDotProduct(tq1,tq2)}};
    if (++count > RENORMCOUNT) 
    {
        count = 0;
        vect = MRVectorNormalizeQuat(vect);
    }
    return vect;
}

static double tb_project_to_sphere(double r, double x, double y)
{
    double d, z;

    d = sqrt(x*x + y*y);
    if (d < r * 0.70710678118654752440)     // Inside sphere 
        z = sqrt(r*r - d*d);
    else                                    // On hyperbola 
        z = r * r / (2*d);
    return z;
}

vet4 MRTrackBall(double p1x, double  p1y, double p2x, double p2y)
{
    double tbs = 0.8;
    if((p1x == p2x) && (p1y == p2y) )  // no rotation
        return MRVector4Identity;

    double tbp = tb_project_to_sphere(tbs,p1x,p1y);
    vet3 p1 = {{p1x, p1y, tbp}};
    p1 = MRVectorNormalize(p1);

    tbp = tb_project_to_sphere(tbs,p2x,p2y);
    vet3 p2 = {{p2x, p2y, tbp}};
    p2 = MRVectorNormalize(p2);

    vet3 axis = MRVectorCrossProduct(p2, p1);
    double t = MRVectorDotProduct(p2, p1);
    if (t > 1.0)  t = 1.0;
    if (t < -1.0) t = -1.0;
    double phi = acos(t);

    return MRAxisToQuat(axis, phi);
}

/************************************************************/
/************************* Math utils ***********************/
/************************************************************/

float Distanza(const float *a, const float *b) 
{
   return (float) MRVectorDistance(MRVectorMake(a), MRVectorMake(b));
}

float Angolo (const float *p1, const float *p2, const float *p3)
{
   vet3 a = MRVectorSub(MRVectorMake(p1), MRVectorMake(p2));
   vet3 b = MRVectorSub(MRVectorMake(p3), MRVectorMake(p2));
   return (float) MRVectorAngle(a, b);
}

float Torsione (const float *p1, const float *p2, const float *p3, const float *p4)
{
   vet3 vp1 = MRVectorMake(p1);
   vet3 vp2 = MRVectorMake(p2);
   vet3 vp3 = MRVectorMake(p3);
   vet3 vp4 = MRVectorMake(p4);
   return (float) MRVectorDihedral(vp1, vp2, vp3, vp4);
}

float calcDistAng (float **p, int op)
{
// p[0:op][0:2]
   float val = 0;
   switch(op)
   {
   default:
        break;
   case 2:
        val = Distanza(p[0], p[1]);
        break;
   case 3:
        {
        float VV[10][3];
        int nvc = 0;
        for(int i=0; i<10; i++)
            for(int j=0; j<3; j++)
                VV[i][j] = 0;
        for(int j=0; j<3; j+=2)
        {
           for(int i=0; i<3; i++)
               VV[nvc][i] = p[j][i] - p[1][i];
           nvc++;
        }
        float rMod1 = sqrt(VV[0][0]*VV[0][0]
                          +VV[0][1]*VV[0][1]
                          +VV[0][2]*VV[0][2]);
        float rMod2 = sqrt(VV[1][0]*VV[1][0]
                          +VV[1][1]*VV[1][1]
                          +VV[1][2]*VV[1][2]);
        float rModuS = VV[0][0]*VV[1][0]+VV[0][1]*VV[1][1]+VV[0][2]*VV[1][2];
        float CosAng = rModuS/(rMod1*rMod2);
        if (CosAng > 0.999999f) 
            CosAng = 0.999999f;
        if (CosAng < -0.999999f) 
            CosAng =-0.999999f;
        val = (float) RADTODEG(acos(CosAng));
        }
        break;
   case 4:
        {
        float VV[10][3];
        for(int i=0; i<10; i++)
            for(int j=0; j<3; j++)
                VV[i][j] = 0;
        for(int i=0; i<3; i++)
            for(int j=0; j<3; j++)
                VV[i][j] = p[i+1][j] - p[i][j];
        float rModuA = sqrt(VV[0][0]*VV[0][0]
                           +VV[0][1]*VV[0][1]
                           +VV[0][2]*VV[0][2]);
        float rModuB = sqrt(VV[1][0]*VV[1][0]
                           +VV[1][1]*VV[1][1]
                           +VV[1][2]*VV[1][2]);
        float rModuS = VV[0][0]*VV[1][0] +VV[0][1]*VV[1][1] +VV[0][2]*VV[1][2];
        float CosAng = rModuS / (rModuA * rModuB);
        if (CosAng >  0.999999f) 
            CosAng = 0.999999f;
        if (CosAng < -0.999999f) 
            CosAng =-0.999999f;
        float AngG = acos(CosAng);
        float SinG = sin(AngG);

        float rModuC = sqrt(VV[2][0]*VV[2][0]
                           +VV[2][1]*VV[2][1]
                           +VV[2][2]*VV[2][2]);
        rModuS = VV[1][0]*VV[2][0]+VV[1][1]*VV[2][1]+VV[1][2]*VV[2][2];
        CosAng = rModuS/(rModuB*rModuC);
        if (CosAng > 0.999999f) 
            CosAng = 0.999999f;
        if (CosAng <-0.999999f) 
            CosAng =-0.999999f;
        float AngA = acos(CosAng);
        float SinA = sin(AngA);

        for(int i=1; i<=2; i++)
        {
            VV[2+i][0] = VV[i-1][1]*VV[i][2] - VV[i-1][2]*VV[i][1];
            VV[2+i][1] =-VV[i-1][0]*VV[i][2] + VV[i-1][2]*VV[i][0];
            VV[2+i][2] = VV[i-1][0]*VV[i][1] - VV[i-1][1]*VV[i][0];
        }
        VV[5][0] = VV[3][1]*VV[4][2] - VV[3][2]*VV[4][1];
        VV[5][1] =-VV[3][0]*VV[4][2] + VV[3][2]*VV[4][0];
        VV[5][2] = VV[3][0]*VV[4][1] - VV[3][1]*VV[4][0];

        float V = VV[0][0]*VV[4][0]+VV[0][1]*VV[4][1]+VV[0][2]*VV[4][2];
        float SinAng =  V / (rModuA*rModuB*rModuC*SinA*SinG);
        float pm = MRsign(1.f,SinAng);

        rModuS = VV[3][0]*VV[4][0]+VV[3][1]*VV[4][1]+VV[3][2]*VV[4][2];

        CosAng = rModuS / (rModuA*rModuB*rModuB*rModuC*SinA*SinG);
        if (CosAng > 0.999999f) 
            CosAng = 0.999999f;
        if (CosAng <-0.999999f) 
            CosAng =-0.999999f;
        val = pm * (float) RADTODEG(acos(CosAng));
        }
        break;
   }
   return val;
}

/************************************************************/
/************************Colors utils ***********************/
/************************************************************/
#define RETURN_HSV(h, s, v) {HSV.H = h; HSV.S = s; HSV.V = v; return HSV;}
#define RETURN_RGB(r, g, b) {RGB.R = r; RGB.G = g; RGB.B = b; return RGB;}

typedef struct {float R, G, B;} RGBType;
typedef struct {float H, S, V;} HSVType;

HSVType RGB_to_HSV( RGBType RGB )
{
// RGB are each on [0, 1]. S and V are returned on [0, 1] and H is
// returned on [0, 6]. H is returned 0 if S==0.
   float min1, max1, delta, h, s, v;
   HSVType HSV;
   float r = RGB.R;
   float g = RGB.G;
   float b = RGB.B;
   min1 = min(min(r, g), b);
   max1 = max(max(r, g), b);
   if(min1 == max1) RETURN_HSV(0, 0, max1);
   v = max1;
   delta = max1 - min1;
   if( max1 != 0 )
      s = delta / max1;
   else
     RETURN_HSV(0, 0, max1);
   if( r == max1 )
      h = ( g - b ) / delta;            // between yellow & magenta
   else if( g == max1 )
      h = 2 + ( b - r ) / delta;        // between cyan & yellow
   else
      h = 4 + ( r - g ) / delta;        // between magenta & cyan
   if( h < 0 )
      h += 6;
   RETURN_HSV(h, s, v);
}

RGBType HSV_to_RGB( HSVType HSV )
{
// H is given on [0, 6]. S and V are given on [0, 1].
// RGB are each returned on [0, 1].
     RGBType RGB;
     float h = HSV.H, s = HSV.S, v = HSV.V;
     if (s == 0) RETURN_RGB(v, v, v);
     int i = (int) floor(h);
     float f = h - i;
     float p = v * (1 - s);
     float q = v * (1 - s * f);
     float t = v * (1 - s * (1 - f));
     switch (i)
     {
     case 0:  RETURN_RGB(v, t, p);
     case 1:  RETURN_RGB(q, v, p);
     case 2:  RETURN_RGB(p, v, t)
     case 3:  RETURN_RGB(p, q, v);
     case 4:  RETURN_RGB(t, p, v);
     default: RETURN_RGB(v, p, q);
     }
}

void Desaturate(float color[])
{
     RGBType c1;
     HSVType c2;
     float Slim = 0.25;

     c1.R = color[0];
     c1.G = color[1];
     c1.B = color[2];
     c2 = RGB_to_HSV(c1);
     if (c2.S > Slim)
         c2.S = Slim;
     c1 = HSV_to_RGB(c2);
     color[0] = c1.R;
     color[1] = c1.G;
     color[2] = c1.B;
}

void TuneH(int nTot, int  frac, float color[])
{
     if(nTot == 0)
        return;
     HSVType c2 = {0.f, 1.0f, 1.0f};
     c2.H = 4.0f*(float)(frac)/(float(nTot));
     RGBType c1 = HSV_to_RGB(c2);
     color[0] = c1.R;
     color[1] = c1.G;
     color[2] = c1.B;
}

void TuneH(float nTot, float  frac, float color[])
{
     if(abs(nTot) < 1e-6)
        return;
     HSVType c2 = {0.f, 1.0f, 1.0f};
     c2.H = 4.0f*frac/nTot;
     RGBType c1 = HSV_to_RGB(c2);
     color[0] = c1.R;
     color[1] = c1.G;
     color[2] = c1.B;
}

void TuneHSV(float color[], float inten)
{
     RGBType c1;
     HSVType c2;
     float Vlim = 0.30f;

     c1.R = color[0];
     c1.G = color[1];
     c1.B = color[2];
     c2 = RGB_to_HSV(c1);
     c2.V = max(Vlim, inten);

     c1 = HSV_to_RGB(c2);
     color[0] = c1.R;
     color[1] = c1.G;
     color[2] = c1.B;
}

void Darker(float color[])
{
     RGBType c1;
     HSVType c2;

     c1.R = color[0];
     c1.G = color[1];
     c1.B = color[2];
     c2 = RGB_to_HSV(c1);
     c2.V *= 0.65f;

     c1 = HSV_to_RGB(c2);
     color[0] = c1.R;
     color[1] = c1.G;
     color[2] = c1.B;
}

void Lighter(float color[])
{
     RGBType c1;
     HSVType c2;

     c1.R = color[0];
     c1.G = color[1];
     c1.B = color[2];
     c2 = RGB_to_HSV(c1);
     c2.S *= 0.35f;

     c1 = HSV_to_RGB(c2);
     color[0] = c1.R;
     color[1] = c1.G;
     color[2] = c1.B;
}

void Desaturate(QColor color)
{
     RGBType c1;
     HSVType c2;
     float Slim = 0.25;

     c1.R = color.redF();
     c1.G = color.greenF();
     c1.B = color.blueF();
     c2 = RGB_to_HSV(c1);
     if (c2.S > Slim)
         c2.S = Slim;
     c1 = HSV_to_RGB(c2);
     color.setRedF(c1.R);
     color.setGreenF(c1.G);
     color.setBlueF(c1.B);
}

QColor Darker(QColor color)
{
     RGBType c1;
     HSVType c2;
     QColor color1 = QColor();

     c1.R = color.redF();
     c1.G = color.greenF();
     c1.B = color.blueF();
     c2 = RGB_to_HSV(c1);
     c2.V *= 0.65f;

     c1 = HSV_to_RGB(c2);
     color.setRedF(c1.R);
     color.setGreenF(c1.G);
     color.setBlueF(c1.B);
     return color1;
}

QColor Lighter(QColor color)
{
     RGBType c1;
     HSVType c2;
     QColor color1 = QColor();

     c1.R = color.redF();
     c1.G = color.greenF();
     c1.B = color.blueF();
     c2 = RGB_to_HSV(c1);
     c2.S *= 0.35f;

     c1 = HSV_to_RGB(c2);
     color1.setRedF(c1.R);
     color1.setGreenF(c1.G);
     color1.setBlueF(c1.B);
     return color1;
}

QColor Blend(QColor color1, QColor color2)
{
     RGBType c1;
     RGBType c2;
     QColor color = QColor();

     c1.R = color1.redF();
     c1.G = color1.greenF();
     c1.B = color1.blueF();
     c2.R = color2.redF();
     c2.G = color2.greenF();
     c2.B = color2.blueF();

     color.setRedF(min((c1.R+c2.R)/2.f, 1.f));
     color.setGreenF(min((c1.G+c2.G)/2.f, 1.f));
     color.setBlueF(min((c1.B+c2.B)/2.f, 1.f));
     return color;
}

void TuneH(int nTot, int  frac, QColor color)
{
     if(nTot == 0)
        return;
     HSVType c2 = {0.f, 1.0f, 1.0f};
     c2.H = 4.0f*(float)(frac)/(float(nTot));
     RGBType c1 = HSV_to_RGB(c2);
     color.setRedF(c1.R);
     color.setGreenF(c1.G);
     color.setBlueF(c1.B);
}

void TuneH(float nTot, float  frac, QColor color)
{
     if(abs(nTot) < 1e-6)
        return;
     HSVType c2 = {0.f, 1.0f, 1.0f};
     c2.H = 4.0f*frac/nTot;
     RGBType c1 = HSV_to_RGB(c2);
     color.setRedF(c1.R);
     color.setGreenF(c1.G);
     color.setBlueF(c1.B);
}

void TuneHSV(QColor color, float inten)
{
     RGBType c1;
     HSVType c2;
     float Vlim = 0.30f;

     c1.R = color.redF();
     c1.G = color.greenF();
     c1.B = color.blueF();
     c2 = RGB_to_HSV(c1);
     c2.V = max(Vlim, inten);

     c1 = HSV_to_RGB(c2);
     color.setRedF(c1.R);
     color.setGreenF(c1.G);
     color.setBlueF(c1.B);
}

Vert norm(Vert a, Vert b, Vert c)
{
        vet3 v1 = {{b.x - a.x, b.y - a.y, b.z - a.z}};
        vet3 v2 = {{c.x - a.x, c.y - a.y, c.z - a.z}};
        vet3 v3 = MRVectorCrossProduct(v1, v2);
        v3 = MRVectorNormalize(v3);
        Vert n = { (float) v3.x, (float) v3.y, (float) v3.z,
                   {a.c[0], a.c[1], a.c[2], a.c[3]}};
        return n;
}


void autov(double a[3][3], double eigen[3], double t[3][3], int *icod)
{
   double aik[3];
   double eps1 = .1e-10, eps2 = eps1, eps3 = .1e-9;
   double sigma1 = 0, offdsq = 0;
   int itmax = 50, n = 3, nm1 = n-1;
   for(int i=0; i<3; i++)
       for(int j=0; j<3; j++)
           t[i][j] = 0;
   for(int i=0; i<n; i++)
   {
      sigma1 = sigma1 + (double) (a[i][i] * a[i][i]);
      t[i][i] = 1.f;
      int ip1 = i+1;
      if(i >= (n-1))
         break;
      for(int j=ip1; j<n; j++)
          offdsq = offdsq + (double) (a[i][j]*a[i][j]);
   }

//    inizio iter. jacobi

   for(int iter=0; iter<itmax; iter++)
   {
       for(int i=0; i<nm1; i++)
       {
           int ip1 = i + 1;
           for(int j=ip1; j<n; j++)
           {
               double q = abs((double) (a[i][i] - a[j][j]));
               double csa, sna;
               if (q > eps1)
               {
                   if(abs((double) a[i][j]) <= eps2)
                   {
                      a[i][j] = 0.f;
                      continue;
                   }
                   double p = 2.0 * a[i][j] * q / (double) (a[i][i] - a[j][j]);
                   double spq = sqrt( p * p + q * q);
                   csa = sqrt( (1.0 + q / spq) / 2.0 );
                   sna = p / ( 2.0 * csa * spq );
               }
               else
               {
                   csa = 1./sqrt(2.0);
                   sna = csa;
               }
               for(int k=0; k<n; k++)
               {
                  double holdki = t[k][i];
                  t[k][i] = holdki * csa + t[k][j] * sna;
                  t[k][j] = holdki * sna - t[k][j] * csa;
               }
               for(int k=0; k<n; k++)
               {
                  if (k <= j) 
                  {
                      aik[k] = a[i][k];
                      a[i][k] = csa * aik[k] + sna * a[k][j];
                      if (k == j) 
                          a[j][k] = sna * aik[k] - csa * a[j][k];
                  }
                  else
                  {
                      double holdik = a[i][k];
                      a[i][k] = csa * holdik + sna * a[j][k];
                      a[j][k] = sna * holdik - csa * a[j][k];
                  }
               }
               aik[j] = sna * aik[i] - csa * aik[j];
               for(int k=0; k<=j; k++)
               {
                  if (k <= i) 
                  {
                     double holdki = a[k][i];
                     a[k][i] = csa * holdki + sna * a[k][j];
                     a[k][j] = sna * holdki - csa * a[k][j];
                  }
                  else
                     a[k][j] = sna * aik[k] - csa * a[k][j];
               }
               a[i][j] = 0.f;
           }
       }
       double sigma2 = 0.0;
       for(int i=0; i<n; i++)
       {
            eigen[i] = a[i][i];
            sigma2 = sigma2 + eigen[i]*eigen[i];
       }
       if (abs(sigma2) <= 1.e-07)
       {
            *icod = 0;
            return;
       }
       else
       {
            double aaa = 1.0 - sigma1 / sigma2;
            if (aaa >= eps3)
            {
                sigma1 = sigma2;
                continue;
            }
       }
       *icod = 1;
       return;
   }
   *icod = 0;
}


void BuildRect(Vert *v, float p1[], float p2[])
{
   v[0].x = p1[0];
   v[0].y = p1[1];
   v[1].x = p1[0];
   v[1].y = p2[1];
   v[2].x = p2[0];
   v[2].y = p2[1];
   v[3].x = p2[0];
   v[3].y = p1[1];
   float colors[4] = {1.f, 0.85f, 1.f, 0.65f};
   for(int i=0; i<4; i++)
   {
       v[i].z = 0.f;
       memcpy(v[i].c, colors, 4*sizeof(float));
   }
}
