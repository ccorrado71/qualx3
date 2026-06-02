#include "aboutdialog.h"
#include "ui_aboutdialog.h"
#include "qt_utils.h"

#include <QPainter>
#include <QSettings>

AboutDialog::AboutDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AboutDialog)
{
    ui->setupUi(this);
    this->setWindowTitle("About " + qApp->applicationName());

    QPixmap pix(":/images/images/about.png");
    QPainter painter(&pix);
    painter.setPen(QPen(Qt::black));

    QFont mFont;
    mFont.setPointSize(20);
    mFont.setBold(true);
    painter.setFont(mFont);
    int xpos = 250, ypos = 155, offset = 30, offset1 = 20;
    painter.drawText(QPoint(xpos, ypos),
                     QString("%1 Version %2").arg(qApp->applicationName(), qApp->applicationVersion()));
    mFont.setBold(false);
    mFont.setPointSize(11);
    painter.setFont(mFont);
    AppVersionInfo appVersion = getVersionInfo();
    painter.drawText(QPoint(xpos, ypos += offset),  QString("Created on %1").arg(appVersion.data));
    painter.drawText(QPoint(xpos, ypos += offset1), appVersion.compilersString);
    painter.drawText(QPoint(xpos, ypos += offset1), appVersion.qtVersionInfo);

    QPixmap cnrPix(":/images/images/logo_CNR_compatto.png");
    painter.drawPixmap(QRectF(270, 290, 179, 114), cnrPix, cnrPix.rect());

    QPixmap icPix(":/images/images/logo_IC.png");
    painter.drawPixmap(QRectF(440, 300, 208, 66), icPix, icPix.rect());

    ui->settingsFileLabel->setText(QSettings().fileName());
    ui->aboutLabel->setPixmap(pix);
    ui->aboutLabel->setFixedSize(pix.width(), pix.height());
}

AboutDialog::~AboutDialog()
{
    delete ui;
}

void AboutDialog::setWebsiteUrl(const QString &url, const QString &text)
{
    ui->websiteLabel->setText(linkHtml(url, text));
}

void AboutDialog::setContactsUrl(const QString &url, const QString &text)
{
    ui->contactsLabel->setText(linkHtml(url, text));
}

void AboutDialog::setCitationUrl(const QString &url, const QString &text)
{
    ui->citationLabel->setText(linkHtml(url, text));
}

QString AboutDialog::linkHtml(const QString &url, const QString &text)
{
    const QString display = text.isEmpty() ? url : text;
    return QString("<html><head/><body><p>"
                   "<a href=\"%1\"><span style=\" text-decoration: underline; color:#0000ff;\">%2</span></a>"
                   "</p></body></html>").arg(url, display);
}
