#ifndef __MRMATRIX_H
#define __MRMATRIX_H

#include "libmath.h"
#include "libvector.h"

static inline mat3x3 MRMatrix3Make(float m00, float m01, float m02, 
                                    float m10, float m11, float m12,
                                    float m20, float m21, float m22)
{
    mat3x3 mm = {{ m00, m01, m02,
                   m10, m11, m12,
                   m20, m21, m22 }};
    return mm;
}

static inline mat3x3 MRMatrixMakeFromArray(float m[3][3])
{
    mat3x3 mm = {{ m[0][0], m[0][1], m[0][2],
                   m[1][0], m[1][1], m[1][2],
                   m[2][0], m[2][1], m[2][2] }};
    return mm;
}

static inline void MRMatrixToArray(mat3x3 mm, float m[3][3])
{
    for (int i=0; i<3; i++)
         for (int j=0; j<3; j++)
              m[i][j] = (float) mm.m[i*3+j];
}

static inline mat3x3 MRMatrixMakeAndTranspose(float m[3][3])
{
    mat3x3 mm = {{ m[0][0], m[1][0], m[2][0],
                   m[0][1], m[1][1], m[2][1],
                   m[0][2], m[1][2], m[2][2] }};
    return mm;
}

static inline mat3x3 MRMatrix3Transpose(mat3x3 matrix)
{
    mat3x3 m = {{ matrix.m[0], matrix.m[3], matrix.m[6],
                  matrix.m[1], matrix.m[4], matrix.m[7],
                  matrix.m[2], matrix.m[5], matrix.m[8] }};
    return m;
}

static inline  mat3x3 MRMatrix3Multiply(mat3x3 a, mat3x3 b)
{
    mat3x3 m;
    m.m00 = a.m00 * b.m00 + a.m01 * b.m10 + a.m02 * b.m20;
    m.m01 = a.m00 * b.m01 + a.m01 * b.m11 + a.m02 * b.m21;
    m.m02 = a.m00 * b.m02 + a.m01 * b.m12 + a.m02 * b.m22;

    m.m10 = a.m10 * b.m00 + a.m11 * b.m10 + a.m12 * b.m20;
    m.m11 = a.m10 * b.m01 + a.m11 * b.m11 + a.m12 * b.m21;
    m.m12 = a.m10 * b.m02 + a.m11 * b.m12 + a.m12 * b.m22;

    m.m20 = a.m20 * b.m00 + a.m21 * b.m10 + a.m22 * b.m20;
    m.m21 = a.m20 * b.m01 + a.m21 * b.m11 + a.m22 * b.m21;
    m.m22 = a.m20 * b.m02 + a.m21 * b.m12 + a.m22 * b.m22;
    return m;
}

static inline mat3x3 MRMatrix3DivideScalar(mat3x3 m, double value)
{
    mat3x3 m2 = {{ m.m[0]  / value,
                   m.m[1]  / value,
                   m.m[2]  / value,
                   m.m[3]  / value,
                   m.m[4]  / value,
                   m.m[5]  / value,
                   m.m[6]  / value,
                   m.m[7]  / value,
                   m.m[8]  / value }};
    return m2;
}

static inline vet3 MRMatrix3MultiplyVector3(mat3x3 mat, vet3 vect)
{
    vet3 v = {{mat.m[0] * vect.v[0] + mat.m[1] * vect.v[1] + mat.m[2] * vect.v[2],
               mat.m[3] * vect.v[0] + mat.m[4] * vect.v[1] + mat.m[5] * vect.v[2],
               mat.m[6] * vect.v[0] + mat.m[7] * vect.v[1] + mat.m[8] * vect.v[2] }};
    return v;
}

static inline mat4x4 MRMatrixMake(
         double m00, double m01, double m02, double m03,
         double m10, double m11, double m12, double m13,
         double m20, double m21, double m22, double m23,
         double m30, double m31, double m32, double m33)
{
    mat4x4 m = {{ m00, m01, m02, m03,
                  m10, m11, m12, m13,
                  m20, m21, m22, m23,
                  m30, m31, m32, m33 }};
    return m;
}

static inline mat4x4 MRMatrixMakeFromArray(const double values[16])
{
    mat4x4 m = {{ values[0], values[1], values[2], values[3],
                  values[4], values[5], values[6], values[7],
                  values[8], values[9], values[10], values[11],
                  values[12], values[13], values[14], values[15] }};
    return m;
}

static inline mat4x4 MRMatrixMakeFromArray(const float values[16])
{
    mat4x4 m = {{ values[0], values[1], values[2], values[3],
                  values[4], values[5], values[6], values[7],
                  values[8], values[9], values[10], values[11],
                  values[12], values[13], values[14], values[15] }};
    return m;
}

static inline mat4x4 MRMatrixTranspose(mat4x4 matrix)
{
    mat4x4 m = {{ matrix.m[0], matrix.m[4], matrix.m[8], matrix.m[12],
                  matrix.m[1], matrix.m[5], matrix.m[9], matrix.m[13],
                  matrix.m[2], matrix.m[6], matrix.m[10], matrix.m[14],
                  matrix.m[3], matrix.m[7], matrix.m[11], matrix.m[15] }};
    return m;
}

static inline  mat4x4 MRMatrixMultiplyGL(mat4x4 a, mat4x4 b)
{
    mat4x4 m;
    m.m[0]  = a.m[0]*b.m[0] +a.m[4]*b.m[1] +a.m[8]*b.m[2]  +a.m[12]*b.m[3];
    m.m[4]  = a.m[0]*b.m[4] +a.m[4]*b.m[5] +a.m[8]*b.m[6]  +a.m[12]*b.m[7];
    m.m[8]  = a.m[0]*b.m[8] +a.m[4]*b.m[9] +a.m[8]*b.m[10] +a.m[12]*b.m[11];
    m.m[12] = a.m[0]*b.m[12]+a.m[4]*b.m[13]+a.m[8]*b.m[14] +a.m[12]*b.m[15];

    m.m[1]  = a.m[1]*b.m[0] +a.m[5]*b.m[1] +a.m[9]*b.m[2]  +a.m[13]*b.m[3];
    m.m[5]  = a.m[1]*b.m[4] +a.m[5]*b.m[5] +a.m[9]*b.m[6]  +a.m[13]*b.m[7];
    m.m[9]  = a.m[1]*b.m[8] +a.m[5]*b.m[9] +a.m[9]*b.m[10] +a.m[13]*b.m[11];
    m.m[13] = a.m[1]*b.m[12]+a.m[5]*b.m[13]+a.m[9]*b.m[14] +a.m[13]*b.m[15];

    m.m[2]  = a.m[2]*b.m[0] +a.m[6]*b.m[1] +a.m[10]*b.m[2] +a.m[14]*b.m[3];
    m.m[6]  = a.m[2]*b.m[4] +a.m[6]*b.m[5] +a.m[10]*b.m[6] +a.m[14]*b.m[7];
    m.m[10] = a.m[2]*b.m[8] +a.m[6]*b.m[9] +a.m[10]*b.m[10]+a.m[14]*b.m[11];
    m.m[14] = a.m[2]*b.m[12]+a.m[6]*b.m[13]+a.m[10]*b.m[14]+a.m[14]*b.m[15];

    m.m[3]  = a.m[3]*b.m[0] +a.m[7]*b.m[1] +a.m[11]*b.m[2] +a.m[15]*b.m[3];
    m.m[7]  = a.m[3]*b.m[4] +a.m[7]*b.m[5] +a.m[11]*b.m[6] +a.m[15]*b.m[7];
    m.m[11] = a.m[3]*b.m[8] +a.m[7]*b.m[9] +a.m[11]*b.m[10]+a.m[15]*b.m[11];
    m.m[15] = a.m[3]*b.m[12]+a.m[7]*b.m[13]+a.m[11]*b.m[14]+a.m[15]*b.m[15];

    return m;
}

static inline  mat4x4 MRMatrixMultiply(mat4x4 a, mat4x4 b)
{
    mat4x4 m;

    m.m[0]  = a.m[0]*b.m[0] +a.m[1]*b.m[4] +a.m[2]*b.m[8]  +a.m[3]*b.m[12];
    m.m[1]  = a.m[0]*b.m[1] +a.m[1]*b.m[5] +a.m[2]*b.m[9]  +a.m[3]*b.m[13];
    m.m[2]  = a.m[0]*b.m[2] +a.m[1]*b.m[6] +a.m[2]*b.m[10] +a.m[3]*b.m[14];
    m.m[3]  = a.m[0]*b.m[3] +a.m[1]*b.m[7] +a.m[2]*b.m[11] +a.m[3]*b.m[15];

    m.m[4]  = a.m[4]*b.m[0] +a.m[5]*b.m[4] +a.m[6]*b.m[8]  +a.m[7] *b.m[12];
    m.m[5]  = a.m[4]*b.m[1] +a.m[5]*b.m[5] +a.m[6]*b.m[9]  +a.m[7] *b.m[13];
    m.m[6]  = a.m[4]*b.m[2] +a.m[5]*b.m[6] +a.m[6]*b.m[10] +a.m[7] *b.m[14];
    m.m[7]  = a.m[4]*b.m[3] +a.m[5]*b.m[7] +a.m[6]*b.m[11] +a.m[7] *b.m[15];

    m.m[8]  = a.m[8]*b.m[0] +a.m[9]*b.m[4] +a.m[10]*b.m[8] +a.m[11]*b.m[12];
    m.m[9]  = a.m[8]*b.m[1] +a.m[9]*b.m[5] +a.m[10]*b.m[9] +a.m[11]*b.m[13];
    m.m[10] = a.m[8]*b.m[2] +a.m[9]*b.m[6] +a.m[10]*b.m[10]+a.m[11]*b.m[14];
    m.m[11] = a.m[8]*b.m[3] +a.m[9]*b.m[7] +a.m[10]*b.m[11]+a.m[11]*b.m[15];

    m.m[12] = a.m[12]*b.m[0]+a.m[13]*b.m[4] +a.m[14]*b.m[8] +a.m[15]*b.m[12];
    m.m[13] = a.m[12]*b.m[1]+a.m[13]*b.m[5] +a.m[14]*b.m[9] +a.m[15]*b.m[13];
    m.m[14] = a.m[12]*b.m[2]+a.m[13]*b.m[6] +a.m[14]*b.m[10]+a.m[15]*b.m[14];
    m.m[15] = a.m[12]*b.m[3]+a.m[13]*b.m[7] +a.m[14]*b.m[11]+a.m[15]*b.m[15];

    return m;
}

static inline vet4 MRMatrixMultiplyVector(mat4x4 m1, vet4 v1)
{
    vet4 v = {{
         m1.m[0]*v1.v[0]+m1.m[4]*v1.v[1]+m1.m[8]*v1.v[2]+m1.m[12]*v1.v[3],
         m1.m[1]*v1.v[0]+m1.m[5]*v1.v[1]+m1.m[9]*v1.v[2]+m1.m[13]*v1.v[3],
         m1.m[2]*v1.v[0]+m1.m[6]*v1.v[1]+m1.m[10]*v1.v[2]+m1.m[14]*v1.v[3],
         m1.m[3]*v1.v[0]+m1.m[7]*v1.v[1]+m1.m[11]*v1.v[2]+m1.m[15]*v1.v[3] }};
    return v;
}

static inline mat4x4 MRMatrixTranslate(mat4x4 a, double tx, double ty, double tz)
{
    mat4x4 m = {{ a.m[0], a.m[1], a.m[2], a.m[3],
                  a.m[4], a.m[5], a.m[6], a.m[7],
                  a.m[8], a.m[9], a.m[10], a.m[11],
                  a.m[0] * tx + a.m[4] * ty + a.m[8]  * tz + a.m[12],
                  a.m[1] * tx + a.m[5] * ty + a.m[9]  * tz + a.m[13],
                  a.m[2] * tx + a.m[6] * ty + a.m[10] * tz + a.m[14],
                  a.m[15] }};
    return m;
}

static inline mat4x4 MRMatrixMakeLookAt(double eyeX, double eyeY, double eyeZ,
                                double centerX, double centerY, double centerZ,
                                double upX, double upY, double upZ)
{
    vet3 ev = {{ eyeX, eyeY, eyeZ }};
    vet3 cv = {{ centerX, centerY, centerZ }};
    vet3 uv = {{ upX, upY, upZ }};
    vet3 n = MRVectorNormalize(MRVectorAdd(ev, MRVectorNegate(cv)));
    vet3 u = MRVectorNormalize(MRVectorCrossProduct(uv, n));
    vet3 v = MRVectorCrossProduct(n, u);

    mat4x4 m = {{ u.v[0], v.v[0], n.v[0], 0.0f,
                  u.v[1], v.v[1], n.v[1], 0.0f,
                  u.v[2], v.v[2], n.v[2], 0.0f,
                  MRVectorDotProduct(MRVectorNegate(u), ev),
                  MRVectorDotProduct(MRVectorNegate(v), ev),
                  MRVectorDotProduct(MRVectorNegate(n), ev),
                  1.0f }};

    return m;
}

static inline mat4x4 MRMatrixMakePerspective(double fovy, double aspect, double nearZ, double farZ)
{
    double cotan = 1.0 / tan(DEGTORAD(fovy) / 2.0);

    mat4x4 m = {{ cotan / aspect, 0.0, 0.0, 0.0,
                     0.0, cotan, 0.0, 0.0,
                     0.0, 0.0, (farZ + nearZ) / (nearZ - farZ), -1.0,
                     0.0, 0.0, (2.0 * farZ * nearZ) / (nearZ - farZ), 0.0 }};

    return m;
}

static inline mat4x4 MRMatrixMakeOrtho(double left, double right,
                                       double bottom, double top,
                                       double nearZ, double farZ)
{
    double ral = right + left;
    double rsl = right - left;
    double tab = top + bottom;
    double tsb = top - bottom;
    double fan = farZ + nearZ;
    double fsn = farZ - nearZ;

    mat4x4 m = {{ 2.0 / rsl, 0.0, 0.0, 0.0,
                  0.0, 2.0 / tsb, 0.0, 0.0,
                  0.0, 0.0, -2.0 / fsn, 0.0,
                  -ral / rsl, -tab / tsb, -fan / fsn, 1.0 }};

    return m;
}

static inline mat4x4 MRMatrixDivideScalar(mat4x4 m, double value)
{
    mat4x4 m2 = {{ m.m[0]  / value,
                   m.m[1]  / value,
                   m.m[2]  / value,
                   m.m[3]  / value,
                   m.m[4]  / value,
                   m.m[5]  / value,
                   m.m[6]  / value,
                   m.m[7]  / value,
                   m.m[8]  / value,
                   m.m[9]  / value,
                   m.m[10] / value,
                   m.m[11] / value,
                   m.m[12] / value,
                   m.m[13] / value,
                   m.m[14] / value,
                   m.m[15] / value }};
    return m2;
}

static inline mat4x4 MRMatrix4MakeFromQuat(vet4 quaternion)
{
    quaternion = MRVector4Normalize(quaternion);

    double x = quaternion.v[0];
    double y = quaternion.v[1];
    double z = quaternion.v[2];
    double w = quaternion.v[3];

    double due_x = x + x;
    double due_y = y + y;
    double due_z = z + z;
    double due_w = w + w;

    mat4x4 m = {{1.0 - due_y * y - due_z * z,
                 due_x * y - due_w * z,
                 due_x * z + due_w * y,
                 0.0,
                 due_x * y + due_w * z,
                 1.0 - due_x * x - due_z * z,
                 due_y * z - due_w * x,
                 0.0,
                 due_x * z - due_w * y,
                 due_y * z + due_w * x,
                 1.0 - due_x * x - due_y * y,
                 0.0,
                 0.0,
                 0.0,
                 0.0,
                 1.0}};

    return m;
}

static inline void MRMatrixCopy(mat4x4 m1, mat4x4 *m2)
{
    for (int i=0; i<16; i++)
        m2->m[i] = m1.m[i];
}
vet4 MRMatrixToQuat(mat4x4 a);
mat4x4 MRMatrixInvert(mat4x4 matrix, bool *isInvertible);
mat3x3 MRMatrix3Invert(mat3x3 matrix, bool *isInvertible);

#endif
