#ifndef PEAKCOMPAREWIDGET_H
#define PEAKCOMPAREWIDGET_H

#include <QWidget>
#include <QVector>
#include <QColor>
#include "experimentalpeaks.h"
#include "cardtype.h"

class QStandardItemModel;

QT_BEGIN_NAMESPACE
namespace Ui { class PeakCompareWidget; }
QT_END_NAMESPACE

class PeakCompareWidget : public QWidget
{
    Q_OBJECT

public:
    explicit PeakCompareWidget(QWidget *parent = nullptr);
    ~PeakCompareWidget();

    void setExperimentalPeaks(const ExperimentalPeaks &ep);
    void setSelectedCard(const CardType &card, const QString &cardId, double delta);
    void clearCard();
    void addAcceptedPhase(const CardType &card);
    void clearAcceptedPhases();
    void refresh();

    QStandardItemModel *m_model;

signals:
    void selectedComparePointsChanged(const QVector<double> &tth,
                                      const QVector<double> &intensity,
                                      const QVector<QColor> &colors);

private:
    void rebuild();
    void updateDelegates(int totalColumns);
    void emitSelectedComparePoints();

    Ui::PeakCompareWidget *ui;

    ExperimentalPeaks  m_ep;
    QVector<CardType>  m_acceptedPhases;
    CardType           m_card;
    QString            m_cardId;
    double             m_delta   = 0.0;
    bool               m_hasCard = false;
};

#endif // PEAKCOMPAREWIDGET_H
