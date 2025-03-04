#ifndef WAVEDIALOG_H
#define WAVEDIALOG_H

#include <QAbstractButton>
#include <QDialog>

namespace Ui {
class WaveDialog;
}

class WaveDialog : public QDialog
{
    Q_OBJECT

public:
    explicit WaveDialog(QWidget *parent = nullptr, int startIndex = 0);
    int radiationType() const;
    int nWaves() const;
    double wave1() const;
    double wave2() const;
    double ratio() const;
    bool isValidInput(QString &errMessage);

    ~WaveDialog();

private slots:
    void done(int result);

private:
    Ui::WaveDialog *ui;
};

#endif // WAVEDIALOG_H
