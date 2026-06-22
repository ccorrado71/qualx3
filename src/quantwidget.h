#ifndef QUANTWIDGET_H
#define QUANTWIDGET_H

#include <QWidget>
#include <QVector>
#include "cardtype.h"

class QAction;
class QStandardItemModel;

QT_BEGIN_NAMESPACE
namespace Ui { class QuantWidget; }
QT_END_NAMESPACE

class QuantWidget : public QWidget
{
    Q_OBJECT

public:
    explicit QuantWidget(QWidget *parent = nullptr);
    ~QuantWidget();

signals:
    void cardSelected(const QString &id);
    void selectedPhasesChanged(const QVector<CardType> &cards);
    void phaseRemoved(const CardType &card);

public slots:
    void addPhase(const CardType &card);

public:
    void clearPhases();
    const QVector<CardType>  &phases()            const { return m_phases; }
    const QVector<double>    &quantPercentages()  const { return m_quant;  }

private slots:
    void removeSelectedPhase();

private:
    void updateQuant();

    Ui::QuantWidget      *ui;
    QStandardItemModel   *m_model;
    QVector<CardType>     m_phases;
    QVector<double>       m_quant;   // computed percentages (empty if RIR unavailable)
    QAction              *m_clearSelAction    = nullptr;
    QAction              *m_removePhaseAction = nullptr;
};

#endif // QUANTWIDGET_H
