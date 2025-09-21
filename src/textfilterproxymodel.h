#ifndef TEXTFILTERPROXYMODEL_H
#define TEXTFILTERPROXYMODEL_H

#include <QSortFilterProxyModel>

class TextFilterProxyModel : public QSortFilterProxyModel
{
    Q_OBJECT
public:
    explicit TextFilterProxyModel(QObject* parent = nullptr);

protected:
    bool filterAcceptsRow(int source_row, const QModelIndex& source_parent) const override;
};

#endif // TEXTFILTERPROXYMODEL_H