#ifndef FLOATDELEGATE_H
#define FLOATDELEGATE_H

#include <QStyledItemDelegate>
#include <QDebug>

class FloatDelegate : public QStyledItemDelegate
{
    Q_OBJECT
public:
    explicit FloatDelegate(QObject *parent = 0, int decimal = 3);
    void setDecimals(int n) {mDecimal = n;}

private:
    int mDecimal;
protected:
    QWidget *createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const override;
    void setEditorData(QWidget * editor, const QModelIndex & index) const override;
    void setModelData(QWidget * editor, QAbstractItemModel * model, const QModelIndex & index) const override;
    void updateEditorGeometry(QWidget * editor, const QStyleOptionViewItem & option, const QModelIndex & index) const override;
    void initStyleOption(QStyleOptionViewItem *option, const QModelIndex &index) const override
    {
            QStyledItemDelegate::initStyleOption(option, index);
            if (index.data().toString() == "-")
                option->displayAlignment = Qt::AlignCenter | Qt::AlignVCenter;
            else
                option->displayAlignment = Qt::AlignRight | Qt::AlignVCenter;
    }
    QString displayText(const QVariant &value, const QLocale &locale) const override
    {
        bool ok;
        float val = value.toFloat(&ok);
        if (!ok) return QString("-");
        return locale.toString(val,'f',mDecimal);
    }

signals:

public slots:

};

#endif // FLOATDELEGATE_H
