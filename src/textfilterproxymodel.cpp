#include "textfilterproxymodel.h"
#include <QAbstractItemModel>

TextFilterProxyModel::TextFilterProxyModel(QObject* parent)
    : QSortFilterProxyModel(parent)
{}

bool TextFilterProxyModel::filterAcceptsRow(int source_row, const QModelIndex& source_parent) const
{
    int cols = sourceModel()->columnCount();
    const auto& regExp = filterRegularExpression();
    for (int col = 0; col < cols; ++col) {
        QModelIndex idx = sourceModel()->index(source_row, col, source_parent);
        QString data = sourceModel()->data(idx).toString();
        if (data.contains(regExp))
            return true;
    }
    // Se il filtro è vuoto mostra tutte le righe
    return regExp.pattern().isEmpty();
}