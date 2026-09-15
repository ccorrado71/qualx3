#ifndef XPDUTILS_H
#define XPDUTILS_H

#include "customplotzoom.h"

#include <QVector>

namespace xpdutils {

enum xAbscissaType {TTHETA, DVALUE, ONE_OVER_DVALUE, ONE_OVER_DVALUE2};
double dvalue(double ttheta, double wave);
double tthvalue(double d, double wave);
double tthvalue(double d, double wave, xAbscissaType dtype);
QVector<double> dvalue(const QVector<double> &ttheta, double wave);
QVector<double> tthvalue(const QVector<double> &d, double wave);
QVector<double> tthvalue_safe(const QVector<double> &d, double wave);
void tthetaToD(QVector<double> &xvet, QVector<double> &yvet, double wave);
void convertTtheta2D(CustomPlotZoom *plot, const QVector<double>& wave, xAbscissaType dType = DVALUE);
void convertD2Ttheta(CustomPlotZoom *plot, const QVector<double>& wave, xAbscissaType dType = DVALUE);
void convertAbscissa(CustomPlotZoom *plot, const QVector<double>& wave, xAbscissaType from, xAbscissaType to);
void convertD(CustomPlotZoom *plot, xAbscissaType from, xAbscissaType to);
QString abscissaString(xAbscissaType aType);

}

#endif // XPDUTILS_H
