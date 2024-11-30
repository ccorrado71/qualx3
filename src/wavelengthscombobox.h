#ifndef WAVELENGTHSCOMBOBOX_H
#define WAVELENGTHSCOMBOBOX_H

#include <QComboBox>

class WavelengthsComboBox : public QComboBox
{
    Q_OBJECT
public:
    explicit WavelengthsComboBox(QWidget *parent = nullptr);    
    bool isUserDefined(int index) const;
    int getUserDefinedIndex() const;

    QString wave1() const;
    QString anode() const;

signals:
    void wavelComboBoxActivated(int index);

private:
    void populateComboBox();

    const QVector<QString> waveType = {"Cu-Ka1", "Cr-Ka1", "Fe-Ka1", "Co-Ka1", "Mo-Ka1"};
    const QVector<double> waveValue = { 1.54059,  2.28973,  1.93604,  1.78900,  0.70932};
    int userDefinedIndex;

};

#endif // WAVELENGTHSCOMBOBOX_H
