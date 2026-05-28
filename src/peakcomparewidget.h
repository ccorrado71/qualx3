#ifndef PEAKCOMPAREWIDGET_H
#define PEAKCOMPAREWIDGET_H

#include <QWidget>
#include <QVector>
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

private:
    void rebuild();
    void updateDelegates(int totalColumns);

    Ui::PeakCompareWidget *ui;
    QStandardItemModel    *m_model;

    ExperimentalPeaks  m_ep;
    QVector<CardType>  m_acceptedPhases;
    CardType           m_card;
    QString            m_cardId;
    double             m_delta   = 0.0;
    bool               m_hasCard = false;
};

#endif // PEAKCOMPAREWIDGET_H
