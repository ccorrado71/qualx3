#include "folderselectorwidget.h"
#include "ui_folderselectorwidget.h"
#include <QFileDialog>
#include <QDir>

FolderSelectorWidget::FolderSelectorWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::FolderSelectorWidget)
{
    ui->setupUi(this);
    
    connect(ui->browseButton, &QPushButton::clicked, this, &FolderSelectorWidget::onBrowseClicked);
    connect(ui->pathLineEdit, &QLineEdit::textChanged, this, &FolderSelectorWidget::onPathEdited);
}

FolderSelectorWidget::~FolderSelectorWidget()
{
    delete ui;
}

QString FolderSelectorWidget::folderPath() const
{
    return ui->pathLineEdit->text();
}

void FolderSelectorWidget::setFolderPath(const QString &path)
{
    ui->pathLineEdit->setText(path);
}

void FolderSelectorWidget::onBrowseClicked()
{
    QString currentPath = ui->pathLineEdit->text();
    if (currentPath.isEmpty()) {
        currentPath = QDir::homePath();
    }
    
    QString dir = QFileDialog::getExistingDirectory(
        this,
        tr("Select Folder"),
        currentPath,
        QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks
    );
    
    if (!dir.isEmpty()) {
        ui->pathLineEdit->setText(dir);
        emit folderChanged(dir);
    }
}

void FolderSelectorWidget::onPathEdited(const QString &text)
{
    emit folderChanged(text);
}
