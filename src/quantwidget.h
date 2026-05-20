#ifndef QUANTWIDGET_H
#define QUANTWIDGET_H

#include <QWidget>
#include <QVector>
#include "cardtype.h"

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

public slots:
    void addPhase(const CardType &card);

private:
    void updateQuant();

    Ui::QuantWidget      *ui;
    QStandardItemModel   *m_model;
    QVector<CardType>     m_phases;
};

#endif // QUANTWIDGET_H
