#ifndef QTDOWNLOAD_H
#define QTDOWNLOAD_H

#include <QObject>
#include <QString>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QFile>
#include <QProgressBar>

class QtDownload : public QObject {
    Q_OBJECT
public:
    explicit QtDownload(QObject* parent = nullptr, QProgressBar* progBar = nullptr);
    QString getFileName() const;
    ~QtDownload();

signals:
    void downloadComplete(bool success, QString errMsg);

public slots:
    void startRequest(QUrl url, QString fileName);

private slots:
    void downloadReadyRead();
    void updateDownloadProgress(qint64 bytesRead, qint64 totalBytes);
    void downloadFinished();

private:
    QProgressBar *m_progBar;
    QNetworkAccessManager *m_manager;
    QNetworkReply *m_reply;
    QFile *m_file;
    int m_redirectCount;
};
#endif
