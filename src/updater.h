#ifndef UPDATER_H
#define UPDATER_H

#include <QObject>
#include <QNetworkReply>
#include <QNetworkAccessManager>

class Updater : public QObject
{
    Q_OBJECT
public:
    explicit Updater(QObject *parent = nullptr);
    ~Updater();

    QString url() const;
    QString openUrl() const;
    QString changelog() const;
    QString moduleName() const;
    QString downloadUrl() const;
    QString platformKey() const;
    QString moduleVersion() const;
    QString latestVersion() const;
    QString userAgentString() const;

    bool notifyOnUpdate() const;
    bool notifyOnFinish() const;
    bool updateAvailable() const;

    void checkForUpdates();
    void setUrl(const QString &url);
    void setModuleName(const QString &name);
    void setNotifyOnUpdate(const bool notify);
    void setNotifyOnFinish(const bool notify);
    void setUserAgentString(const QString &agent);
    void setModuleVersion(const QString &version);
    void setPlatformKey(const QString &platformKey);

signals:
    void checkingFinished(const QString &url);

private slots:
    void onReply(QNetworkReply *reply);
    void setUpdateAvailable(const bool available);

private:
    QString m_url;
    QString m_userAgentString;

    bool m_notifyOnUpdate;
    bool m_notifyOnFinish;
    bool m_updateAvailable;

    QString m_openUrl;
    QString m_platform;
    QString m_changelog;
    QString m_moduleName;
    QString m_downloadUrl;
    QString m_moduleVersion;
    QString m_latestVersion;

    QNetworkAccessManager *m_manager;

    bool compare(const QString &x, const QString &y);
};

#endif // UPDATER_H
