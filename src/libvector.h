#ifndef __LIBVECTOR_H
#define __LIBVECTOR_H

static inline vet3 MRVectorMake(const float x[3])
{
    vet3 v = {{ (double)x[0], (double)x[1], (double)x[2]}};
    return v;
}

static inline vet3 MRVectorMake(const float x1, const float x2, const float x3)
{
    vet3 v = {{ (double)x1, (double)x2, (double)x3}};
    return v;
}

static inline vet4 MRVector4Make(double x, double y, double z, double w)
{
    vet4 v = {{ x, y, z, w }};
    return v;
}

static inline vet3 MRVectorNegate(vet3 vector)
{
    vet3 v = {{ -vector.v[0], -vector.v[1], -vector.v[2] }};
    return v;
}

static inline vet3 MRVectorAddScalar(vet3 vector, double value)
{
    vet3 v = {{ vector.v[0] + value,
               vector.v[1] + value,
               vector.v[2] + value}};
    return v;
}

static inline vet3 MRVectorMultiplyScalar(vet3 vector, double value)
{
    vet3 v = {{ vector.v[0] * value,
               vector.v[1] * value,
               vector.v[2] * value}};
    return v;
}

static inline vet3 MRVectorAdd(vet3 v1, vet3 v2)
{
    vet3 v = {{ v1.v[0] + v2.v[0],
               v1.v[1] + v2.v[1],
               v1.v[2] + v2.v[2] }};
    return v;
}

static inline vet3 MRVectorSub(vet3 v1, vet3 v2)
{
    vet3 v = {{ v1.v[0] - v2.v[0],
               v1.v[1] - v2.v[1],
               v1.v[2] - v2.v[2] }};
    return v;
}

static inline double MRVectorLength(vet3 vector)
{
    return sqrt(vector.v[0] * vector.v[0] + vector.v[1] * vector.v[1] +
                vector.v[2] * vector.v[2]);
}

static inline double MRVectorDotProduct(vet3 v1, vet3 v2)
{
    return v1.v[0] * v2.v[0] + v1.v[1] * v2.v[1] + v1.v[2] * v2.v[2];
}

static inline vet3 MRVectorNormalize(vet3 vector)
{
    double scale = 1.0f / MRVectorLength(vector);
    vet3 v = {{ vector.v[0] * scale, vector.v[1] * scale, vector.v[2] * scale }};
    return v;
}

static inline vet3 MRVectorMedium(float v1[], float v2[])
{
   vet3 v = {{(v1[0]+v2[0])*0.5, (v1[1]+v2[1])*0.5, (v1[2]+v2[2])*0.5}};
   return v;
}

static inline vet3 MRVectorMedium(vet3 v1, vet3 v2)
{
   vet3 v = {{(v1.x+v2.x)*0.5, (v1.y+v2.y)*0.5, (v1.z+v2.z)*0.5}};
   return v;
}

static inline vet3 MRVectorCrossProduct(vet3 v1, vet3 v2)
{
    vet3 v = {{ v1.v[1] * v2.v[2] - v1.v[2] * v2.v[1],
               v1.v[2] * v2.v[0] - v1.v[0] * v2.v[2],
               v1.v[0] * v2.v[1] - v1.v[1] * v2.v[0] }};
    return v;
}

static inline vet3 MRVectorFirstPerp(vet3 vector)
{
  vet3 result = {{0, 0, 0}};
  //modificato 24/10/2019 per legami deformati
#if 0
  if(IsZero(vector.x) || IsZero(vector.y) || IsZero(vector.z))
  {
    if (IsZero(vector.x))
    {
        if(IsZero(vector.z))
           result.z = 1.0;
        else
           result.x = 1.0;
    }
    else if (IsZero(vector.y))
      result.y = 1.0;
    else
      result.z = 1.0;
  }
  else
#endif
  {
    result.x = vector.z;
    result.y = vector.z;
    result.z = -(vector.x+vector.y);

    // aggiunte il 14/11/2022
    if(IsZero(result.x) && IsZero(result.y) && IsZero(result.z))
    {
          vet3 arb = {{0, 0, 0}};
          if (!IsZero(vector.y) || !IsZero(vector.z))
              arb.x = 1.0;
          else
              arb.y = 1.0;
          result = MRVectorCrossProduct(vector, arb);
    }
    //end

    result = MRVectorNormalize(result);
  }
  return result;
}

static inline void MRVectorCopy(vet3 v1, vet3 *v2)
{
    v2->x = v1.x;
    v2->y = v1.y;
    v2->z = v1.z;
}

static inline void MRQuatCopy(vet4 v1, vet4 *v2)
{
    v2->x = v1.x;
    v2->y = v1.y;
    v2->z = v1.z;
    v2->w = v1.w;
}

static inline double MRVectorDistance2(vet3 v1, vet3 v2)
{
    vet3 vector = MRVectorSub(v2, v1);
    return (vector.v[0] * vector.v[0] + vector.v[1] * vector.v[1] +
                vector.v[2] * vector.v[2]);
}

static inline double MRVectorDistance(vet3 v1, vet3 v2)
{
    return MRVectorLength(MRVectorSub(v2, v1));
}

static inline vet3 MRVectorMaximum(vet3 vector1, vet3 vector2)
{
    vet3 max = vector1;
    if (vector2.v[0] > vector1.v[0])
        max.v[0] = vector2.v[0];
    if (vector2.v[1] > vector1.v[1])
        max.v[1] = vector2.v[1];
    if (vector2.v[2] > vector1.v[2])
        max.v[2] = vector2.v[2];
    return max;
}

static inline vet3 MRVectorMinimum(vet3 vector1, vet3 vector2)
{
    vet3 min = vector1;
    if (vector2.v[0] < vector1.v[0])
        min.v[0] = vector2.v[0];
    if (vector2.v[1] < vector1.v[1])
        min.v[1] = vector2.v[1];
    if (vector2.v[2] < vector1.v[2])
        min.v[2] = vector2.v[2];
    return min;
}

static inline vet4 MRVector4AddScalar(vet4 vector, double value)
{
    vet4 v = {{ vector.v[0] + value,
               vector.v[1] + value,
               vector.v[2] + value,
               vector.v[3] + value }};
    return v;
}

static inline vet4 MRVector4MultiplyScalar(vet4 vector, double value)
{
    vet4 v = {{ vector.v[0] * value,
               vector.v[1] * value,
               vector.v[2] * value,
               vector.v[3] * value }};
    return v;
}

static inline vet4 MRVector4DivideScalar(vet4 vector, double value)
{
    vet4 v = {{ vector.v[0] / value,
               vector.v[1] / value,
               vector.v[2] / value,
               vector.v[3] / value }};
    return v;
}

static inline vet4 MRVector4MapToViewport(vet4 vector, const int viewport[])
{
    vet4 v = {{ vector.v[0] * viewport[2] + viewport[0],
               vector.v[1] * viewport[3] + viewport[1],
               vector.v[2],
               vector.v[3]}};
    return v;
}

static inline vet4 MRVector4MapFromViewport(vet4 vector, const int viewport[])
{
    vet4 v = {{ (vector.v[0] - viewport[0]) / viewport[2],
               (vector.v[1] - viewport[1]) / viewport[3],
               vector.v[2],
               vector.v[3]}};
    return v;
}

static inline double MRVector4Length(vet4 vector)
{
    return sqrt(vector.v[0] * vector.v[0] + vector.v[1] * vector.v[1] +
                vector.v[2] * vector.v[2] + vector.v[3] * vector.v[3]);
}

static inline vet4 MRVector4Normalize(vet4 vector)
{
    double scale = 1.0f / MRVector4Length(vector);
    vet4 v = {{ vector.v[0] * scale, vector.v[1] * scale, vector.v[2] * scale,
               vector.v[3] * scale }};
    return v;
}

static inline vet4 MRVectorNormalizeQuat(vet4 q)
{ 
    double scale = (q.x*q.x + q.y*q.y + q.z*q.z + q.w*q.w);
    vet4 v = {{ q.v[0]/scale, q.v[1]/scale, q.v[2]/scale, q.v[3]/scale }};
    return v;
}

static inline vet4 MRAxisToQuat(vet3 axis, double radians)
{
    axis = MRVectorNormalize(axis);
    double halfAngle = radians * 0.5f;
    double scale = sin(halfAngle);
    vet4 q = {{scale * axis.x, scale * axis.y, scale * axis.z, cos(halfAngle)}};
    return q;
}

double MRVectorAngle(vet3 a, vet3 b);
double MRVectorDihedral(vet3 a1,vet3 a2,vet3 a3,vet3 a4);
vet4 MRAddQuats(vet4 q1, vet4 q2);

#endif
