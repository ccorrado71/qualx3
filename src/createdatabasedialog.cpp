#include "createdatabasedialog.h"
#include "ui_createdatabasedialog.h"
#include "folderselectorwidget.h"
#include "fileselectorwidget.h"

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

    // Update auto name whenever a source checkbox changes
    connect(ui->checkCod,  &QCheckBox::toggled, this, &CreateDatabaseDialog::onSourceChanged);
    connect(ui->checkPdf,  &QCheckBox::toggled, this, &CreateDatabaseDialog::onSourceChanged);
    connect(ui->checkUser, &QCheckBox::toggled, this, &CreateDatabaseDialog::onSourceChanged);

    // Enable/disable pdf source group when checkPdf changes
    connect(ui->checkPdf, &QCheckBox::toggled,
            ui->pdfSourceGroupBox, &QWidget::setEnabled);
    ui->pdfFileSelector->setFilter(tr("PDF-2 files (*.dat);;All files (*)"));

    // Enable/disable user source group when checkUser changes
    connect(ui->checkUser, &QCheckBox::toggled,
            ui->userSourceGroupBox, &QWidget::setEnabled);

    // Update folder label and recursive checkbox state when radio changes
    connect(ui->radioCif, &QRadioButton::toggled,
            this, &CreateDatabaseDialog::onUserSourceTypeChanged);

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
    updateUserSourceLabel();
    ui->checkRecursive->setEnabled(ui->radioCif->isChecked());
}

void CreateDatabaseDialog::updateUserSourceLabel()
{
    if (ui->radioCif->isChecked())
        ui->labelUserFolder->setText(tr("Folder containing CIF files:"));
    else
        ui->labelUserFolder->setText(tr("Folder containing .sq database files:"));
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
        tr("<b>COD database</b>: Crystallography Open Database, available free of charge.<br><br>"
           "<b>ICDD PDF</b>: International Centre for Diffraction Data Powder Diffraction File "
           "in NBS*AIDS83 format.<br><br>"
           "<b>User database (CIF files)</b>: A custom database created from user-supplied CIF files "
           "in a selected folder.<br><br>"
           "<b>User database (.sq file)</b>: A custom database created by merging an existing "
           "QualX database."));
}
