#ifndef BACKGROUNDCOEFWIDGET_H
#define BACKGROUNDCOEFWIDGET_H

#include <QWidget>

namespace Ui {
class BackgroundCoefWidget;
}

class BackgroundCoefWidget : public QWidget
{
    Q_OBJECT

public:
    explicit BackgroundCoefWidget(QWidget *parent = nullptr);
    void setAutob(bool value);
    void setNcoef(int value);
    void setMaximumCoef(int value);
    bool getAutob() const;
    int getNcoef() const;
    ~BackgroundCoefWidget();

signals:
    void autoCoefChanged();
    void ncoefChanged();

private slots:
    //void on_checkBox_clicked(bool checked);
    //void on_coefSpinBox_valueChanged(int arg1);
    void onCheckBoxClicked(bool checked);
    void onCoefSpinBoxValueChanged(int arg1);

private:
    Ui::BackgroundCoefWidget *ui;
    int ncoef;
    bool autob;
};

#endif // BACKGROUNDCOEFWIDGET_H
