#ifndef DBRESULTSWIDGET_H
#define DBRESULTSWIDGET_H

#include <QWidget>
#include <QVector>
#include <QColor>
#include <QToolBar>
#include "cardtype.h"

// Returns the display color associated with a card ID (same golden-ratio hue mapping
// used in DbResultsWidget and cardBrowser).
QColor cardColor(const QString &id);

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
    void mergeResults(const QVector<CardType>& newCards);
    void addCard(const CardType &card);
    bool hasResults() const;
    bool hasSelection() const;
    QVector<CardType> allCards() const;
    void selectFirstCard();
    void selectCard(const QString &id);
    void deleteSelectedCards();
    void acceptSelectedCards();
    void changeSelectedCardColor();
    void setEntryToolBar(QToolBar *tb);
    void setContextMenuActions(const QList<QAction *> &actions);

signals:
    void hasResultsChanged(bool hasResults);
    void cardSelected(const QString &id);
    void cardDataSelected(const CardType &card);
    void phaseAccepted(const CardType &card);
    void cardColorChanged(const QString &id);
    void entrySelectionChanged(bool hasSelection);

private slots:
    void currentPageChanged(int page);
    void pageCountChanged(int count);
    void updateButtons();
    void onHeaderSectionClicked(int column);
    void populateRow(int row, const CardType &card);
    void onAcceptClicked();

private:
    Ui::DbResultsWidget* ui;

    QStandardItemModel*   sourceModel;
    TextFilterProxyModel* filterModel;
    PaginationModel*      pageModel;

    int            m_sortColumn = -1;
    Qt::SortOrder  m_sortOrder  = Qt::AscendingOrder;
};

#endif // DBRESULTSWIDGET_H