#ifndef WAVELENGTHSWIDGET_H
#define WAVELENGTHSWIDGET_H

#include <QWidget>

namespace Ui {
class WavelengthsWidget;
}

class WavelengthsWidget : public QWidget
{
    Q_OBJECT

public:
    explicit WavelengthsWidget(QWidget *parent = nullptr);
    int nWaves() const;
    QString wave1() const;
    QString wave2() const;
    QString ratio() const;
    bool hasWave2() const;
    QSize columnSize() const;
    bool isValidInput(QString &errMessage) const;
    bool isUserDefined() const;
    int currentIndex();
    int radiationIndex();

    void setComboIndex(int index);
    void setComboAsUserDefined(double wave = -1);
    void setUserWave(double wave);
    void setSeconWave(Qt::CheckState checkState=Qt::Unchecked, double wave = -1, double ratio = 0.5);

    ~WavelengthsWidget();

private slots:
    void on_wave2CheckBox_stateChanged(int arg1);

private:
    Ui::WavelengthsWidget *ui;
    void setWave1LineEditVisible(int index);
    void setWave2LineEdit();
    const QVector<QString> waveType2 = {"Cu-Ka2", "Cr-Ka2", "Fe-Ka2", "Co-Ka2", "Mo-Ka2"};
    const QVector<double> waveValue2 = { 1.54443,  2.29365,  1.93997,  1.79283,  0.71361};
};

#endif // WAVELENGTHSWIDGET_H
