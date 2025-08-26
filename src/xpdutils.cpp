#include "customplotzoom.h"
#include "graphitem.h"
#include "xpdutils.h"
#include <QVector>
#include <cmath>

#ifndef M_PI
#define  M_PI 3.14159265358979323846
#endif /* M_PI */

namespace xpdutils {

double dvalue(double ttheta, double wave) {
    const double DTOR = M_PI / 180;
    return wave / (2.0 * sin(ttheta * 0.5 * DTOR));
}

double tthvalue(double d, double wave) {
    const double RTOD = 180 / M_PI;
    return 2.0 * asin(0.5 * wave / d) * RTOD;
}

double tthvalue(double d, double wave, xAbscissaType dtype) {
    switch (dtype) {
    case TTHETA: return d; //dvalue(d, wave);
    case DVALUE: return tthvalue(d, wave);
    case ONE_OVER_DVALUE: return tthvalue(1/d, wave);
    case ONE_OVER_DVALUE2: return tthvalue(sqrt(1/d), wave);
    }
    return dvalue(d, wave);
}

QVector<double> dvalue(const QVector<double> &ttheta, double wave) {
    QVector<double> dvector(ttheta.size());
    for (int i = 0; i < ttheta.size(); i++) {
        int j = ttheta.size() - i - 1;
        dvector[i] = dvalue(ttheta.at(j), wave);
    }
    return dvector;
}

QVector<double> tthvalue(const QVector<double> &d, double wave)
{
    QVector<double> tthvector(d.size());
    for (int i = 0; i < d.size(); i++) {
        tthvector[i] = tthvalue(d.at(i), wave);
    }
    return tthvector;
}

void tthetaToD(QVector<double> &xvet, QVector<double> &yvet, double wave) {
    QVector<double> xvetc = xvet;
    QVector<double> yvetc = yvet;
    for (int i = 0; i < xvetc.size(); i++) {
        int j = xvetc.size() - i - 1;
        xvet[i] = dvalue(xvetc.at(j),wave);
        yvet[i] = yvetc.at(j);
    }
}

void convertTtheta2D(CustomPlotZoom *plot, const QVector<double>& wave, const QVector<graphItem>& refl, xAbscissaType dType) {
    //Covert all plots
    for (int i = 0; i < plot->graphCount(); i++) {
        for (auto it = plot->graph(i)->data()->begin(); it != plot->graph(i)->data()->end(); it++) {
            it->key = xpdutils::dvalue(it->key, wave.at(i));
            if (dType == xpdutils::ONE_OVER_DVALUE) {
                it->key = 1/it->key;
            } else if (dType == xpdutils::ONE_OVER_DVALUE2) {
                it->key = 1/(it->key * it->key);
            }
        }
        if (dType == xpdutils::DVALUE) {
            std::reverse(plot->graph(i)->data()->begin(),plot->graph(i)->data()->end());
        }
    }

    //Convert all reflection sets
    for (int i = 0; i < refl.count(); i++) {
        if (refl[i].xSize() > 0) {
            qInfo() << "Refl to convert with wave: " << wave.at(refl[i].getGraphIndex());
            double wavel = wave.at(refl[i].getGraphIndex());
            for (int ind = refl[i].itemIndexStart; ind <= refl[i].itemIndexEnd; ind++) {
                QCPItemLine *line = dynamic_cast<QCPItemLine *> (plot->item(ind));                
                double newkey = xpdutils::dvalue(line->start->key(),wavel);
                if (dType == xpdutils::ONE_OVER_DVALUE) {
                    newkey = 1/newkey;
                } else if (dType == xpdutils::ONE_OVER_DVALUE2) {
                    newkey = 1/(newkey * newkey);
                }
                line->start->setCoords(newkey,line->start->value());
                line->end->setCoords(newkey,line->end->value());
            }
        }
    }

    plot->xAxis->setLabel(abscissaString(dType));
}

void convertD2Ttheta(CustomPlotZoom *plot, const QVector<double>& wave, const QVector<graphItem>& refl, xAbscissaType dType) {
    //Convert all plots
    for (int i = 0; i < plot->graphCount(); i++) {
        for (auto it = plot->graph(i)->data()->begin(); it != plot->graph(i)->data()->end(); it++) {
            if (dType == ONE_OVER_DVALUE) {
                it->key = 1/it->key;
            } else if (dType == ONE_OVER_DVALUE2) {
                it->key = sqrt(1/(it->key));
            }
            it->key = xpdutils::tthvalue(it->key, wave.at(i));
        }
        if (dType == DVALUE) std::reverse(plot->graph(i)->data()->begin(),plot->graph(i)->data()->end());
    }

    //Convert all reflection sets
    for (int i = 0; i < refl.count(); i++) {
        if (refl[i].xSize() > 0) {
            qInfo() << "Refl to convert with wave: " << wave.at(refl[i].getGraphIndex());
            double wavel = wave.at(refl[i].getGraphIndex());
            for (int ind = refl[i].itemIndexStart; ind <= refl[i].itemIndexEnd; ind++) {
                QCPItemLine *line = dynamic_cast<QCPItemLine *> (plot->item(ind));
                double newkey;
                if (dType == ONE_OVER_DVALUE) {
                    newkey = xpdutils::tthvalue(1/line->start->key(),wavel);
                } else if (dType == ONE_OVER_DVALUE2) {
                    newkey = xpdutils::tthvalue(sqrt(1/line->start->key()),wavel);
                } else {
                    newkey = xpdutils::tthvalue(line->start->key(),wavel);
                }
                line->start->setCoords(newkey,line->start->value());
                line->end->setCoords(newkey,line->end->value());
            }
        }
    }

    plot->xAxis->setLabel(abscissaString(xpdutils::TTHETA));
}

void convertAbscissa(CustomPlotZoom *plot, const QVector<double> &wave, const QVector<graphItem> &refl, xpdutils::xAbscissaType from, xpdutils::xAbscissaType to)
{
    if (from == xpdutils::TTHETA) {
        convertTtheta2D(plot, wave, refl, to);
    } else {
        if (to == xpdutils::TTHETA) {
            convertD2Ttheta(plot, wave, refl, from);
        } else {
            convertD(plot, refl, from, to);
        }
    }    
}

QString abscissaString(xAbscissaType aType)
{
    switch (aType) {
    case TTHETA: return "2"+QString(QChar(0x03B8));
    case DVALUE: return "d";
    case ONE_OVER_DVALUE: return "1/d";
    case ONE_OVER_DVALUE2: return "q";
    default: return QString();
    }
}

void convertD(CustomPlotZoom *plot, const QVector<graphItem> &refl, xAbscissaType from, xAbscissaType to)
{
    if (from == DVALUE) {
        for (int i = 0; i < plot->graphCount(); i++) {
            for (auto it = plot->graph(i)->data()->begin(); it != plot->graph(i)->data()->end(); it++) {
                it->key = 1/it->key;
                if (to == ONE_OVER_DVALUE2) {
                    it->key *= it->key;
                }
            }
            std::reverse(plot->graph(i)->data()->begin(),plot->graph(i)->data()->end());
        }

        for (int i = 0; i < refl.count(); i++) {
            if (refl[i].xSize() > 0) {
                for (int ind = refl[i].itemIndexStart; ind <= refl[i].itemIndexEnd; ind++) {
                    QCPItemLine *line = dynamic_cast<QCPItemLine *> (plot->item(ind));
                    double newkey = 1/line->start->key();
                    if (to == ONE_OVER_DVALUE2) {
                        newkey *= newkey;
                    }
                    line->start->setCoords(newkey,line->start->value());
                    line->end->setCoords(newkey,line->end->value());
                }
            }
        }
    } else if (from == ONE_OVER_DVALUE) {
        for (int i = 0; i < plot->graphCount(); i++) {
            for (auto it = plot->graph(i)->data()->begin(); it != plot->graph(i)->data()->end(); it++) {
                if (to == DVALUE) {
                    it->key = 1/it->key;
                } else if (to == ONE_OVER_DVALUE2) {
                    it->key *= it->key;
                }
            }
            if (to == DVALUE) std::reverse(plot->graph(i)->data()->begin(),plot->graph(i)->data()->end());
        }

        for (int i = 0; i < refl.count(); i++) {
            if (refl[i].xSize() > 0) {
                for (int ind = refl[i].itemIndexStart; ind <= refl[i].itemIndexEnd; ind++) {
                    QCPItemLine *line = dynamic_cast<QCPItemLine *> (plot->item(ind));
                    double newkey = line->start->key();
                    if (to == DVALUE) {
                        newkey = 1/newkey;
                    } else if (to == ONE_OVER_DVALUE2) {
                        newkey *= newkey;
                    }
                    line->start->setCoords(newkey,line->start->value());
                    line->end->setCoords(newkey,line->end->value());
                }
            }
        }
    } else if (from == ONE_OVER_DVALUE2) {
        for (int i = 0; i < plot->graphCount(); i++) {
            for (auto it = plot->graph(i)->data()->begin(); it != plot->graph(i)->data()->end(); it++) {
                if (to == DVALUE) {
                    it->key = sqrt(1/it->key);
                } else if (to == ONE_OVER_DVALUE) {
                    it->key = sqrt(it->key);
                }
            }
            if (to == DVALUE) std::reverse(plot->graph(i)->data()->begin(),plot->graph(i)->data()->end());
        }

        for (int i = 0; i < refl.count(); i++) {
            if (refl[i].xSize() > 0) {
                for (int ind = refl[i].itemIndexStart; ind <= refl[i].itemIndexEnd; ind++) {
                    QCPItemLine *line = dynamic_cast<QCPItemLine *> (plot->item(ind));
                    double newkey = line->start->key();
                    if (to == DVALUE) {
                        newkey = sqrt(1/newkey);
                    } else if (to == ONE_OVER_DVALUE) {
                        newkey = sqrt(newkey);
                    }
                    line->start->setCoords(newkey,line->start->value());
                    line->end->setCoords(newkey,line->end->value());
                }
            }
        }
    }
    plot->xAxis->setLabel(abscissaString(to));
}

}

