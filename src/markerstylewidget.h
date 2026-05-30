#ifndef MARKERSTYLEWIDGET_H
#define MARKERSTYLEWIDGET_H

#include "qcustomplot.h"

#include <QWidget>

namespace Ui {
class MarkerStyleWidget;
}

class MarkerStyleWidget : public QWidget
{
    Q_OBJECT

public:
    explicit MarkerStyleWidget(QWidget *parent = nullptr);
    void setMarkerStyle(const QCPScatterStyle::ScatterShape &shape, const QColor &c, int w);
    void setMarkerStyle(const QCPScatterStyle &scatter);
    QCPScatterStyle::ScatterShape getShape() const;
    int getSize() const;
    QColor getColor() const;

    bool isDifferent(const QCPScatterStyle &scatter);
    ~MarkerStyleWidget();

signals:
    void markerColorChanged(QColor c);
    void markerSizeChanged(int size);
    void markerShapeChanged(QCPScatterStyle::ScatterShape shape);

private:
    Ui::MarkerStyleWidget *ui;
};

#endif // MARKERSTYLEWIDGET_H
