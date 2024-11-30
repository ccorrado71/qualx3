#include "wavelengthscombobox.h"

WavelengthsComboBox::WavelengthsComboBox(QWidget *parent) : QComboBox(parent)
{
    populateComboBox();    
    setEditable(true);

    this->setValidator(new QDoubleValidator(0.0, 5.0, 8, this));

    connect(this, QOverload<int>::of(&QComboBox::activated),[=](int index){
        emit wavelComboBoxActivated(index);
    });
}

bool WavelengthsComboBox::isUserDefined(int index) const
{
    return (index == userDefinedIndex);
}

// Not Qt6 compatible
//QString WavelengthsComboBox::wave1() const
//{
//    // match real number at the beginning, es: 123.5abc -> 123.5
//    QRegExp rx("^\\s*[0-9.]+");
//    int pos = rx.indexIn(this->currentText());
//    if (pos > -1) {
//        return rx.cap(0);
//    }
//    return QString();
//}

// Qt6/Qt5 compatible
QString WavelengthsComboBox::wave1() const
{
    // match real number at the beginning, es: 123.5abc -> 123.5
    static QRegularExpression rx("^\\s*[0-9.]+");
    auto match = rx.match(this->currentText());
    if (match.hasMatch()) {
        return match.captured(0);
    }
    return QString();
}

QString WavelengthsComboBox::anode() const
{
    int index = this->currentIndex();
    QString str = this->itemData(index).toString();
    if (str.isEmpty()) return QString();

    //Get anode from the first 2 valid character
    QString wave;
    if (str.at(0).isLetter()) wave = str.at(0);
    if (str.size() > 1 && str.at(1).isLetter()) wave += str.at(1);
    return wave;
}

void WavelengthsComboBox::populateComboBox()
{
    for (int i = 0; i < waveType.count(); i++) {
        QString text = QString("%1 (%2)").arg(waveValue.at(i),-7,'g',-1,'0').arg(waveType.at(i));
        this->addItem(text,waveType.at(i));
    }

    //Assign last position to 'User Defined' item
    userDefinedIndex = waveType.count();
    this->addItem("User defined");
}

int WavelengthsComboBox::getUserDefinedIndex() const
{
    return userDefinedIndex;
}
