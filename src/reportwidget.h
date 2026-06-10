#ifndef REPORTWIDGET_H
#define REPORTWIDGET_H

#include <QWidget>
#include <QVector>
#include <QPair>
#include <QColor>
#include "experimentalpeaks.h"
#include "cardtype.h"

class QPrinter;

QT_BEGIN_NAMESPACE
namespace Ui { class ReportWidget; }
QT_END_NAMESPACE

class ReportWidget : public QWidget
{
    Q_OBJECT

public:
    explicit ReportWidget(QWidget *parent = nullptr);
    ~ReportWidget();

    void updateReport(const ExperimentalPeaks &ep,
                      const QVector<CardType> &resultCards,
                      int maxCards = 100);
    void updateQuantitative(const QVector<CardType> &phases,
                            const QVector<double> &percentages);
    void clearQuantitative();
    void print(QPrinter *printer);

private:
    void generateHtml();
    static QPixmap renderPieChart(const QVector<QPair<QColor, double>> &slices, int size);

    Ui::ReportWidget  *ui;

    ExperimentalPeaks  m_ep;
    QVector<CardType>  m_cards;
    int                m_maxCards = 100;
    QVector<CardType>  m_phases;
    QVector<double>    m_quant;
};

#endif // REPORTWIDGET_H
