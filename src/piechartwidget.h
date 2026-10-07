#ifndef PIECHARTWIDGET_H
#define PIECHARTWIDGET_H

#include <QWidget>
#include <QVector>
#include <QColor>
#include <QString>

struct PieSlice {
    QColor  color;
    double  percentage = 0.0;
    QString label;
};

class PieChartWidget : public QWidget
{
    Q_OBJECT
public:
    explicit PieChartWidget(QWidget *parent = nullptr);
    void setSlices(const QVector<PieSlice> &slices);

protected:
    void paintEvent(QPaintEvent *event) override;

private:
    QVector<PieSlice> m_slices;
};

#endif // PIECHARTWIDGET_H
