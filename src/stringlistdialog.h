#ifndef STRINGLISTDIALOG_H
#define STRINGLISTDIALOG_H

#include <QDialog>

namespace Ui {
class StringListDialog;
}

class StringListDialog : public QDialog
{
    Q_OBJECT

public:
    explicit StringListDialog(QWidget *parent = nullptr);
    void setLabel(const QString &label);
    void setTableSize(int nrow, int ncol);
    void setColumTitle(const QStringList &titles);
    void setTableItem(int row, int col, const QString &sitem);
    void setSelection(int selection);
    int getSelection();
    ~StringListDialog();

signals:
    void newRowSelected(int row);

private slots:
    void onCellClicked(int row);

private:
    Ui::StringListDialog *ui;
    int selectedRow;
};

#endif // STRINGLISTDIALOG_H
