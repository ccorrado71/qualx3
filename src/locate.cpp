#include "nr.h"

#include <QDebug>

void NR::locate(Vec_I_DP &xx, const DP x, int &j)
{
	int ju,jm,jl;
	bool ascnd;

	int n=xx.size();
	jl=-1;
	ju=n;
	ascnd=(xx[n-1] >= xx[0]);
	while (ju-jl > 1) {
		jm=(ju+jl) >> 1;
		if (x >= xx[jm] == ascnd)
			jl=jm;
		else
			ju=jm;
	}
	if (x == xx[0]) j=0;
	else if (x == xx[n-1]) j=n-2;
	else j=jl;
}

int NR::locateClosest(Vec_I_DP &xx, const DP x)
{
    int j;
    locate(xx, x, j);

    if (j == -1) return 0; // out of range to the left
    if (j == xx.count() - 1) return j; // out of range to the right
    if (abs(x - xx[j]) < abs(x - xx[j+1])) return j;   // xx[j] < x < xx[j+1]
    return ++j;
}
