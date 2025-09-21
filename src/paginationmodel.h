#ifndef PAGINATIONMODEL_H
#define PAGINATIONMODEL_H

#include <QSortFilterProxyModel>

class PaginationModel : public QSortFilterProxyModel
{
    Q_OBJECT

public:
    explicit PaginationModel(int maxRows = 50, QObject* parent = nullptr);

    int maxRows() const;
    void setMaxRows(int maxRows);

    bool canGoBack() const;
    bool canGoForward() const;

    int pageCount() const;
    int currentPage() const;
    void setCurrentPage(int page);

    void firstPage();
    void previousPage();
    void nextPage();
    void lastPage();

    void setSourceModel(QAbstractItemModel* model) override;

signals:
    void currentPageChanged(int page);
    void pageCountChanged(int count);
    void canGoBackChanged(bool canGoBack);
    void canGoForwardChanged(bool canGoForward);

protected:
    bool filterAcceptsRow(int source_row, const QModelIndex& source_parent) const override;

private:
    void _emitCanChange(bool oldBack, bool oldFwd);
    void _beginSourceChange();
    void _endSourceChange();
    void _setCurrentPage(int page, bool force = false);

    int _maxRows;
    int _currentPage;
    int _firstRow;
    int _rangeEnd;

    // For change tracking
    int _prePageCount;
    int _preCurrentPage;
    bool _preCanGoBack;
    bool _preCanGoForward;
};

#endif // PAGINATIONMODEL_H