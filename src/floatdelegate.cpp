#include "floatdelegate.h"
#include <QLineEdit>
#include <QDoubleValidator>

FloatDelegate::FloatDelegate(QObject *parent, int decimal) :
    QStyledItemDelegate(parent),
    mDecimal(decimal)
{
}

QWidget *FloatDelegate::createEditor(QWidget *parent,
                                    const QStyleOptionViewItem &option,
                                    const QModelIndex &index) const
{
    Q_UNUSED(index);
    Q_UNUSED(option);
    QLineEdit *editor = new QLineEdit(parent);
    QDoubleValidator *doubleValidator = new QDoubleValidator();
    doubleValidator->setDecimals(mDecimal);
    editor->setValidator(doubleValidator);
    return editor;
}

void FloatDelegate::setEditorData(QWidget *editor,
                                 const QModelIndex &index) const
{
    QLocale locale = QLocale();
    QString value = locale.toString(index.model()->data(index, Qt::EditRole).toFloat(), 'f', mDecimal);
    QLineEdit *line = static_cast<QLineEdit*>(editor);
    line->setText(value);
}


void FloatDelegate::setModelData(QWidget *editor,
                                QAbstractItemModel *model,
                                const QModelIndex &index) const
{
    QLocale locale = QLocale();
    QLineEdit *line = static_cast<QLineEdit*>(editor);
    float value = locale.toFloat(line->text());
    model->setData(index, value, Qt::DisplayRole);
}

void FloatDelegate::updateEditorGeometry(QWidget *editor,
                                        const QStyleOptionViewItem &option,
                                        const QModelIndex &index) const
{
    Q_UNUSED(index);
    editor->setGeometry(option.rect);
}
