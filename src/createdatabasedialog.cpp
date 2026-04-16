#include "createdatabasedialog.h"
#include "ui_createdatabasedialog.h"
#include "folderselectorwidget.h"
#include "fileselectorwidget.h"

#include <QFileInfo>
#include <QMessageBox>
#include <QStandardPaths>
#include <QDir>

CreateDatabaseDialog::CreateDatabaseDialog(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::CreateDatabaseDialog)
{
    ui->setupUi(this);

    connect(ui->buttonBox,     &QDialogButtonBox::helpRequested,
            this, &CreateDatabaseDialog::onHelpRequested);
    connect(ui->checkAutoName, &QCheckBox::toggled,
            this, &CreateDatabaseDialog::onAutoNameToggled);
    connect(ui->checkAutoDir,  &QCheckBox::toggled,
            this, &CreateDatabaseDialog::onAutoDirToggled);

    // Update auto name whenever a source groupbox is toggled
    connect(ui->checkCod,  &QGroupBox::toggled, this, &CreateDatabaseDialog::onSourceChanged);
    connect(ui->checkPdf,  &QGroupBox::toggled, this, &CreateDatabaseDialog::onSourceChanged);
    connect(ui->checkUser, &QGroupBox::toggled, this, &CreateDatabaseDialog::onSourceChanged);

    ui->codSqFileSelector->setFilter(tr("QualX database (*.sq);;All files (*)"));
    ui->pdfFileSelector->setFilter(tr("PDF-2 files (*.dat);;All files (*)"));

    // Update visible widgets when radio changes
    connect(ui->radioCif, &QRadioButton::toggled,
            this, &CreateDatabaseDialog::onUserSourceTypeChanged);

    // Set filter for the user .sq file selector
    ui->sqFileSelector->setFilter(tr("QualX database (*.sq);;All files (*)"));

    // Set initial auto values
    ui->nameLineEdit->setText(generateAutoName());
    ui->folderSelector->setFolderPath(generateAutoDirectory());
}

CreateDatabaseDialog::~CreateDatabaseDialog()
{
    delete ui;
}

bool CreateDatabaseDialog::isCodSelected() const
{
    return ui->checkCod->isChecked();
}

bool CreateDatabaseDialog::isPdfSelected() const
{
    return ui->checkPdf->isChecked();
}

bool CreateDatabaseDialog::isUserSelected() const
{
    return ui->checkUser->isChecked();
}

QString CreateDatabaseDialog::codSqFile() const
{
    return ui->codSqFileSelector->filePath();
}

QString CreateDatabaseDialog::userSqFile() const
{
    return ui->sqFileSelector->filePath();
}

CreateDatabaseDialog::UserSource CreateDatabaseDialog::userSource() const
{
    return ui->radioCif->isChecked() ? UserSource::CifFiles : UserSource::SqDatabase;
}

bool CreateDatabaseDialog::isRecursive() const
{
    return ui->checkRecursive->isChecked();
}

QString CreateDatabaseDialog::userSourceFolder() const
{
    return ui->userFolderSelector->folderPath();
}

QString CreateDatabaseDialog::pdfFile() const
{
    return ui->pdfFileSelector->filePath();
}

QString CreateDatabaseDialog::databaseName() const
{
    return ui->nameLineEdit->text();
}

QString CreateDatabaseDialog::databaseDirectory() const
{
    return ui->folderSelector->folderPath();
}

// static
QStringList CreateDatabaseDialog::missingSqFiles(const QString &sqFile)
{
    QStringList missing;
    for (const QString &suffix : QStringList{".info", ".search", ".infostat"}) {
        const QString companion = sqFile + suffix;
        if (!QFileInfo::exists(companion))
            missing << companion;
    }
    return missing;
}

void CreateDatabaseDialog::accept()
{
    if (ui->checkCod->isChecked()) {
        const QString sqFile = ui->codSqFileSelector->filePath();
        if (sqFile.isEmpty()) {
            QMessageBox::warning(this, tr("Missing File"),
                tr("Please select a COD database file (.sq)."));
            return;
        }
        const QStringList missing = missingSqFiles(sqFile);
        if (!missing.isEmpty()) {
            QMessageBox::critical(this, tr("Missing Files"),
                tr("The following companion files are missing:\n%1").arg(missing.join('\n')));
            return;
        }
    }

    if (ui->checkUser->isChecked() && ui->radioSq->isChecked()) {
        const QString sqFile = ui->sqFileSelector->filePath();
        if (sqFile.isEmpty()) {
            QMessageBox::warning(this, tr("Missing File"),
                tr("Please select a QualX database file (.sq)."));
            return;
        }
        const QStringList missing = missingSqFiles(sqFile);
        if (!missing.isEmpty()) {
            QMessageBox::critical(this, tr("Missing Files"),
                tr("The following companion files are missing:\n%1").arg(missing.join('\n')));
            return;
        }
    }

    QDialog::accept();
}

void CreateDatabaseDialog::onAutoNameToggled(bool checked)
{
    ui->nameLineEdit->setEnabled(!checked);
    if (checked)
        ui->nameLineEdit->setText(generateAutoName());
}

void CreateDatabaseDialog::onAutoDirToggled(bool checked)
{
    ui->folderSelector->setEnabled(!checked);
    if (checked)
        ui->folderSelector->setFolderPath(generateAutoDirectory());
}

void CreateDatabaseDialog::onSourceChanged()
{
    if (ui->checkAutoName->isChecked())
        ui->nameLineEdit->setText(generateAutoName());
}

void CreateDatabaseDialog::onUserSourceTypeChanged()
{
    const bool isCif = ui->radioCif->isChecked();
    ui->recursiveWidget->setVisible(isCif);
    ui->cifFolderWidget->setVisible(isCif);
    ui->sqFileSelector->setVisible(!isCif);
}

QString CreateDatabaseDialog::generateAutoName() const
{
    QStringList parts;
    if (ui->checkCod->isChecked())  parts << "COD";
    if (ui->checkPdf->isChecked())  parts << "PDF";
    if (ui->checkUser->isChecked()) parts << "USR";
    return parts.isEmpty() ? QString("QualxDB") : parts.join("+");
}

QString CreateDatabaseDialog::generateAutoDirectory() const
{
    const QString base = QStandardPaths::writableLocation(QStandardPaths::HomeLocation)
                         + QDir::separator() + "QualxDB";
    int index = 1;
    while (true) {
        const QString candidate = base + QDir::separator()
                                  + QStringLiteral("DB%1").arg(index, 2, 10, QChar('0'));
        if (!QDir(candidate).exists())
            return candidate;
        ++index;
    }
}

void CreateDatabaseDialog::onHelpRequested()
{
    QMessageBox::information(this, tr("Help"),
        tr("<b>COD database</b>: Select an existing QualX database (.sq) built from the "
           "Crystallography Open Database.<br><br>"
           "<b>ICDD PDF</b>: International Centre for Diffraction Data Powder Diffraction File "
           "in NBS*AIDS83 format.<br><br>"
           "<b>User database (CIF files)</b>: A custom database created from user-supplied CIF files "
           "in a selected folder.<br><br>"
           "<b>User database (.sq file)</b>: Register an existing QualX database (.sq) as a "
           "user database."));
}
