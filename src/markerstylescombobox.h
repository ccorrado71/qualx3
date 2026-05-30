#ifndef MARKERSTYLESCOMBOBOX_H
#define MARKERSTYLESCOMBOBOX_H

#include "qcustomplot.h"

#include <QComboBox>

class MarkerStylesComboBox : public QComboBox
{
    Q_OBJECT
public:
    explicit MarkerStylesComboBox(QWidget *parent = nullptr);
    void setStyle(QCPScatterStyle::ScatterShape style);
    QCPScatterStyle::ScatterShape style() const;

signals:
    void markerStyleChanged(QCPScatterStyle::ScatterShape style);

private:
    QCPScatterStyle::ScatterShape mStyle;
    void populateComboBox();    
};

#endif // MARKERSTYLESCOMBOBOX_H
