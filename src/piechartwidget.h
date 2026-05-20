#ifndef PIECHARTWIDGET_H
#define PIECHARTWIDGET_H

#include <QWidget>
#include <QVector>
#include <QPair>
#include <QColor>

class PieChartWidget : public QWidget
{
    Q_OBJECT
public:
    explicit PieChartWidget(QWidget *parent = nullptr);
    void setSlices(const QVector<QPair<QColor, double>> &slices);

protected:
    void paintEvent(QPaintEvent *event) override;

private:
    QVector<QPair<QColor, double>> m_slices; // (color, percentage)
};

#endif // PIECHARTWIDGET_H
