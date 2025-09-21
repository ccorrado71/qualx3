#include "paginationmodel.h"
#include <QAbstractItemModel>
#include <algorithm>

PaginationModel::PaginationModel(int maxRows, QObject* parent)
    : QSortFilterProxyModel(parent),
      _maxRows(std::max(1, maxRows)),
      _currentPage(-1),
      _firstRow(0),
      _rangeEnd(0),
      _prePageCount(0),
      _preCurrentPage(-1),
      _preCanGoBack(false),
      _preCanGoForward(false)
{}

void PaginationModel::_emitCanChange(bool oldBack, bool oldFwd)
{
    bool newBack = canGoBack();
    if (oldBack != newBack) emit canGoBackChanged(newBack);

    bool newFwd = canGoForward();
    if (oldFwd != newFwd) emit canGoForwardChanged(newFwd);
}

void PaginationModel::_beginSourceChange()
{
    _prePageCount = pageCount();
    _preCurrentPage = _currentPage;
    _preCanGoBack = canGoBack();
    _preCanGoForward = canGoForward();
}

void PaginationModel::_endSourceChange()
{
    int newRowCount = sourceModel() ? sourceModel()->rowCount() : 0;
    int oldPageCount = _prePageCount;
    int oldPage = _preCurrentPage;
    bool oldBack = _preCanGoBack;
    bool oldFwd = _preCanGoForward;

    int newPageCount, firstRow, rangeEnd;
    if (!newRowCount) {
        newPageCount = firstRow = rangeEnd = 0;
        _currentPage = -1;
    } else {
        newPageCount = (newRowCount + _maxRows - 1) / _maxRows;
        if (oldPage < 0) {
            _currentPage = 0;
            firstRow = 0;
        } else {
            firstRow = oldPage * _maxRows;
        }

        if (firstRow >= newRowCount)
            firstRow = ((newRowCount - 1) / _maxRows) * _maxRows;

        rangeEnd = firstRow + _maxRows;
        if (rangeEnd > newRowCount) {
            rangeEnd = newRowCount;
            _currentPage = (newRowCount - 1) / _maxRows;
        }
    }

    _firstRow = firstRow;
    _rangeEnd = rangeEnd;
    invalidateFilter();

    if (oldPageCount != newPageCount)
        emit pageCountChanged(newPageCount);

    if (oldPage != _currentPage)
        emit currentPageChanged(_currentPage);

    _emitCanChange(oldBack, oldFwd);
}

void PaginationModel::_setCurrentPage(int page, bool force)
{
    if (!sourceModel()) return;

    int rowCount = sourceModel()->rowCount();
    int firstRow, rangeEnd;

    if (rowCount <= 0) {
        firstRow = rangeEnd = 0;
    } else {
        firstRow = page * _maxRows;
        if (firstRow >= rowCount)
            firstRow = ((rowCount - 1) / _maxRows) * _maxRows;
        rangeEnd = firstRow + _maxRows;
        if (rangeEnd > rowCount)
            rangeEnd = rowCount;
    }

    _firstRow = firstRow;
    _rangeEnd = rangeEnd;

    int newPage = (firstRow / _maxRows);
    if (_currentPage != newPage || force) {
        bool oldBack = canGoBack();
        bool oldFwd = canGoForward();
        _currentPage = newPage;
        invalidateFilter();
        emit currentPageChanged(newPage);
        _emitCanChange(oldBack, oldFwd);
    }
}

int PaginationModel::maxRows() const
{
    return _maxRows;
}

void PaginationModel::setMaxRows(int maxRows)
{
    if (maxRows <= 0 || _maxRows == maxRows) return;
    if (!sourceModel()) {
        _maxRows = maxRows;
        return;
    }

    int oldPageCount = pageCount();
    bool oldBack = canGoBack();
    bool oldFwd = canGoForward();

    _maxRows = maxRows;

    invalidateFilter();
    _setCurrentPage(std::max(0, _currentPage), true);

    int newPageCount = pageCount();
    if (oldPageCount != newPageCount) {
        emit pageCountChanged(newPageCount);
        _emitCanChange(oldBack, oldFwd);
    }
}

bool PaginationModel::canGoBack() const
{
    return _currentPage > 0;
}

bool PaginationModel::canGoForward() const
{
    return _currentPage < pageCount() - 1;
}

void PaginationModel::firstPage()
{
    setCurrentPage(0);
}

void PaginationModel::previousPage()
{
    setCurrentPage(_currentPage - 1);
}

void PaginationModel::nextPage()
{
    setCurrentPage(_currentPage + 1);
}

void PaginationModel::lastPage()
{
    if (sourceModel())
        setCurrentPage((sourceModel()->rowCount() - 1) / _maxRows);
}

int PaginationModel::pageCount() const
{
    if (sourceModel()) {
        int rows = sourceModel()->rowCount();
        return (rows + _maxRows - 1) / _maxRows;
    }
    return 0;
}

int PaginationModel::currentPage() const
{
    return _currentPage;
}

void PaginationModel::setCurrentPage(int page)
{
    if (sourceModel() && page >= 0 && _currentPage != page)
        _setCurrentPage(page);
}

bool PaginationModel::filterAcceptsRow(int source_row, const QModelIndex&) const
{
    return (source_row >= _firstRow) && (source_row < _rangeEnd);
}

void PaginationModel::setSourceModel(QAbstractItemModel* model)
{
    QAbstractItemModel* old = sourceModel();
    if (old == model) return;

    if (old) {
        disconnect(old, nullptr, this, nullptr);
        _currentPage = -1;
    }

    QSortFilterProxyModel::setSourceModel(model);

    if (!model) {
        _firstRow = _rangeEnd = 0;
    } else {
        connect(model, &QAbstractItemModel::rowsAboutToBeRemoved, this, &PaginationModel::_beginSourceChange);
        connect(model, &QAbstractItemModel::rowsAboutToBeInserted, this, &PaginationModel::_beginSourceChange);
        connect(model, &QAbstractItemModel::layoutAboutToBeChanged, this, &PaginationModel::_beginSourceChange);
        connect(model, &QAbstractItemModel::modelAboutToBeReset, this, &PaginationModel::_beginSourceChange);

        connect(model, &QAbstractItemModel::rowsRemoved, this, &PaginationModel::_endSourceChange);
        connect(model, &QAbstractItemModel::rowsInserted, this, &PaginationModel::_endSourceChange);
        connect(model, &QAbstractItemModel::layoutChanged, this, &PaginationModel::_endSourceChange);
        connect(model, &QAbstractItemModel::modelReset, this, &PaginationModel::_endSourceChange);

        _setCurrentPage(std::max(0, _currentPage));
    }
}