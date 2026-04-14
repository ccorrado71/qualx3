#include "fileselectorwidget.h"
#include "ui_fileselectorwidget.h"
#include <QFileDialog>
#include <QDir>

FileSelectorWidget::FileSelectorWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::FileSelectorWidget),
    fileFilter("All Files (*.*)")
{
    ui->setupUi(this);
    
    connect(ui->browseButton, &QPushButton::clicked, this, &FileSelectorWidget::onBrowseClicked);
    connect(ui->filePathLineEdit, &QLineEdit::textChanged, this, &FileSelectorWidget::fileSelected);
}

FileSelectorWidget::~FileSelectorWidget()
{
    delete ui;
}

QString FileSelectorWidget::filePath() const
{
    return ui->filePathLineEdit->text();
}

void FileSelectorWidget::setFilePath(const QString &path)
{
    ui->filePathLineEdit->setText(path);
}

void FileSelectorWidget::setFilter(const QString &filter)
{
    fileFilter = filter;
}

void FileSelectorWidget::setPlaceholder(const QString &placeholder)
{
    ui->filePathLineEdit->setPlaceholderText(placeholder);
}

void FileSelectorWidget::setInitialDirectory(const QString &directory)
{
    initialDirectory = directory;
}

void FileSelectorWidget::onBrowseClicked()
{
    QString currentPath = ui->filePathLineEdit->text();
    QString startPath;
    
    if (!currentPath.isEmpty()) {
        startPath = currentPath;
    } else if (!initialDirectory.isEmpty()) {
        startPath = initialDirectory;
    } else {
        startPath = QDir::homePath();
    }
    
    QString selectedFile = QFileDialog::getOpenFileName(this, 
                                                        tr("Select File"), 
                                                        startPath, 
                                                        fileFilter);
    
    if (!selectedFile.isEmpty()) {
        ui->filePathLineEdit->setText(selectedFile);
    }
}
