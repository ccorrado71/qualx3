#include "qtdownload.h"
#include <QFileInfo>

static const int MAX_REDIRECTS = 5;

QtDownload::QtDownload(QObject *parent, QProgressBar *progBar) :
    QObject(parent),
    m_progBar(progBar),
    m_manager(new QNetworkAccessManager(this)),
    m_reply(nullptr),
    m_file(nullptr),
    m_redirectCount(0)
{
}

QString QtDownload::getFileName() const
{
    return m_file ? m_file->fileName() : QString();
}

QtDownload::~QtDownload()
{
}

void QtDownload::startRequest(QUrl url, QString fileName)
{
    if (fileName.isEmpty()) {
        // create file name from url
        QFileInfo fileInfo(url.path());
        fileName = fileInfo.fileName();
        if (fileName.isEmpty()) fileName = "downloadedfile";
    }

    m_redirectCount = 0;

    // create the file and open it for writing
    m_file = new QFile(fileName);
    if (!m_file->open(QIODevice::WriteOnly)) {
        delete m_file;
        m_file = nullptr;
        emit downloadComplete(false, tr("Cannot open file for writing: %1").arg(fileName));
        return;
    }

    // create network request to download stuff
    m_reply = m_manager->get(QNetworkRequest(url));

    // connect the signals to monitor the download
    connect(m_reply, &QNetworkReply::readyRead, this, &QtDownload::downloadReadyRead);
    if (m_progBar) connect(m_reply, &QNetworkReply::downloadProgress, this, &QtDownload::updateDownloadProgress);
    connect(m_reply, &QNetworkReply::finished, this, &QtDownload::downloadFinished);
}

void QtDownload::downloadReadyRead()
{
    if (m_file) m_file->write(m_reply->readAll());
}

void QtDownload::updateDownloadProgress(qint64 bytesRead, qint64 totalBytes)
{
    if (!m_progBar) return;
    if (totalBytes < 1) {
        // unable to determine file size, so display a busy indicator
        m_progBar->setMinimum(0);
        m_progBar->setMaximum(0);
        return;
    }
    m_progBar->setMaximum(100);
    m_progBar->setValue(static_cast<int>(100.0 * bytesRead / totalBytes));
}

void QtDownload::downloadFinished()
{
    if (m_progBar) {
        m_progBar->setValue(0);
        m_progBar->setMaximum(100);
    }
    m_file->flush();
    m_file->close();

    // follow redirection, if any
    const QVariant redirectionTarget = m_reply->attribute(QNetworkRequest::RedirectionTargetAttribute);
    if (!redirectionTarget.isNull() && ++m_redirectCount <= MAX_REDIRECTS) {
        const QUrl newUrl = m_reply->url().resolved(redirectionTarget.toUrl());
        m_reply->deleteLater();
        m_reply = nullptr;

        m_file->open(QIODevice::WriteOnly);
        m_file->resize(0);

        m_reply = m_manager->get(QNetworkRequest(newUrl));
        connect(m_reply, &QNetworkReply::readyRead, this, &QtDownload::downloadReadyRead);
        if (m_progBar) connect(m_reply, &QNetworkReply::downloadProgress, this, &QtDownload::updateDownloadProgress);
        connect(m_reply, &QNetworkReply::finished, this, &QtDownload::downloadFinished);
        return;
    }

    bool success = true;
    QString errMsg;
    if (m_reply->error()) {
        success = false;
        m_file->remove();
        errMsg = m_reply->errorString();
    } else {
        errMsg = m_file->fileName();
    }
    m_reply->deleteLater();
    m_reply = nullptr;
    delete m_file;
    m_file = nullptr;

    emit downloadComplete(success, errMsg);
}
