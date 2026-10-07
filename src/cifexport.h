#ifndef CIFEXPORT_H
#define CIFEXPORT_H

#include <QString>

class QWidget;

// Downloads the CIF file for a COD entry (https://www.crystallography.net/cod/<id>.cif)
// and saves it locally, prompting the user for the destination file.
namespace cifexport {
void exportCardAsCif(QWidget *parent, const QString &cardId);
}

#endif // CIFEXPORT_H
