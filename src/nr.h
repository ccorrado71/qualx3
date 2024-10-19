#ifndef _NR_H_
#define _NR_H_

#include <QVector>

namespace NR {

typedef QVector<double> Vec_I_DP;
typedef double DP;

void locate(Vec_I_DP &xx, const DP x, int &j);
int locateClosest(Vec_I_DP &xx, const DP x);

}
#endif /* _NR_H_ */
