#ifndef DBRESULTSWIDGET_H
#define DBRESULTSWIDGET_H

#include <QWidget>
#include <QVector>
#include "cardtype.h"

class QStandardItemModel;
class TextFilterProxyModel;
class PaginationModel;

QT_BEGIN_NAMESPACE
namespace Ui { class DbResultsWidget; }
QT_END_NAMESPACE

class DbResultsWidget : public QWidget
{
    Q_OBJECT

public:
    explicit DbResultsWidget(QWidget* parent = nullptr);
    ~DbResultsWidget();

    void setResults(const QVector<CardType>& results);

private slots:
    void currentPageChanged(int page);
    void pageCountChanged(int count);
    void updateButtons();
    void onTableSelectionChanged();
    void onAscClicked();
    void onDesClicked();

private:
    Ui::DbResultsWidget* ui;

    QStandardItemModel* sourceModel;
    TextFilterProxyModel* filterModel;
    PaginationModel* pageModel;

    int selectedColumn{-1};
};

#endif // DBRESULTSWIDGET_H