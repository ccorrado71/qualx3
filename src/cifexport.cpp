#include "cifexport.h"
#include "qtdownload.h"
#include "savedialog.h"
#include "progkeysettings.h"

#include <QDir>
#include <QFileInfo>
#include <QMessageBox>
#include <QSettings>
#include <QUrl>
#include <QWidget>

void cifexport::exportCardAsCif(QWidget *parent, const QString &cardId)
{
    if (cardId.isEmpty())
        return;

    QSettings settings;
    QString selectedFilter = "CIF file (*.cif)";
    QString fileName = SaveDialog::run(parent,
                                       QObject::tr("Export as CIF"),
                                       settings.value(DEFAULT_DIR_KEY).toString(),
                                       cardId,
                                       "CIF file (*.cif);;All Files (*.*)",
                                       "cif",
                                       selectedFilter);
    if (fileName.isEmpty())
        return;

    fileName = QDir::toNativeSeparators(fileName);
    settings.setValue(DEFAULT_DIR_KEY, QFileInfo(fileName).absolutePath());

    auto *downloader = new QtDownload(parent);
    QObject::connect(downloader, &QtDownload::downloadComplete, parent,
                      [parent, downloader](bool success, const QString &errMsg) {
        if (!success) {
            QMessageBox::warning(parent, QObject::tr("Export as CIF"),
                                  QObject::tr("Failed to download CIF file:\n%1").arg(errMsg));
        }
        downloader->deleteLater();
    });
    downloader->startRequest(QUrl("https://www.crystallography.net/cod/" + cardId + ".cif"), fileName);
}
