#include <QJsonValue>
#include <QJsonObject>
#include <QMessageBox>
#include <QApplication>
#include <QJsonDocument>
#include <QDesktopServices>

#include "updater.h"

Updater::Updater(QObject *parent)
    : QObject{parent}
{
    m_url = "";
    m_openUrl = "";
    m_changelog = "";
    m_downloadUrl = "";
    m_latestVersion = "";
    m_notifyOnUpdate = true;
    m_notifyOnFinish = false;
    m_updateAvailable = false;
    m_moduleName = qApp->applicationName();
    m_moduleVersion = qApp->applicationVersion();

    m_manager = new QNetworkAccessManager(this);

#if defined Q_OS_WIN
    m_platform = "windows";
#elif defined Q_OS_MAC
    m_platform = "osx";
#elif defined Q_OS_LINUX
    m_platform = "linux";
#elif defined Q_OS_ANDROID
    m_platform = "android";
#elif defined Q_OS_IOS
    m_platform = "ios";
#endif

    setUserAgentString(QString("%1/%2 (Qt; Updater)").arg(qApp->applicationName(), qApp->applicationVersion()));
    connect(m_manager, &QNetworkAccessManager::finished, this, &Updater::onReply);
}

Updater::~Updater()
{

}

QString Updater::url() const
{
    return m_url;
}

QString Updater::openUrl() const
{
    return m_openUrl;
}

QString Updater::changelog() const
{
    return m_changelog;
}

QString Updater::moduleName() const
{
    return m_moduleName;
}

QString Updater::platformKey() const
{
    return m_platform;
}

QString Updater::downloadUrl() const
{
    return m_downloadUrl;
}

QString Updater::latestVersion() const
{
    return m_latestVersion;
}

QString Updater::moduleVersion() const
{
    return m_moduleVersion;
}

QString Updater::userAgentString() const
{
    return m_userAgentString;
}

bool Updater::notifyOnUpdate() const
{
    return m_notifyOnUpdate;
}

bool Updater::notifyOnFinish() const
{
    return m_notifyOnFinish;
}

bool Updater::updateAvailable() const
{
    return m_updateAvailable;
}

void Updater::checkForUpdates()
{
    QNetworkRequest request(url());

    request.setAttribute(QNetworkRequest::RedirectPolicyAttribute, QNetworkRequest::NoLessSafeRedirectPolicy);

    if (!userAgentString().isEmpty())
        request.setRawHeader("User-Agent", userAgentString().toUtf8());

    m_manager->get(request);
}

void Updater::setUrl(const QString &url)
{
    m_url = url;
}

void Updater::setModuleName(const QString &name)
{
    m_moduleName = name;
}

void Updater::setNotifyOnUpdate(const bool notify)
{
    m_notifyOnUpdate = notify;
}

void Updater::setNotifyOnFinish(const bool notify)
{
    m_notifyOnFinish = notify;
}

void Updater::setUserAgentString(const QString &agent)
{
    m_userAgentString = agent;
}

void Updater::setModuleVersion(const QString &version)
{
    m_moduleVersion = version;
}

void Updater::setPlatformKey(const QString &platformKey)
{
    m_platform = platformKey;
}

void Updater::onReply(QNetworkReply *reply)
{
    /* Check if we need to redirect */
    QUrl redirect = reply->attribute(QNetworkRequest::RedirectionTargetAttribute).toUrl();
    if (!redirect.isEmpty())
    {
        setUrl(redirect.toString());
        checkForUpdates();
        return;
    }

    /* There was a network error */
    if (reply->error() != QNetworkReply::NoError)
    {
        setUpdateAvailable(false);
        emit checkingFinished(url());
        return;
    }

    /* Try to create a JSON document from downloaded data */
    QJsonDocument document = QJsonDocument::fromJson(reply->readAll());

    /* JSON is invalid */
    if (document.isNull())
    {
        setUpdateAvailable(false);
        emit checkingFinished(url());
        return;
    }

    /* Get the platform information */
    QJsonObject updates = document.object().value("updates").toObject();
    QJsonObject platform = updates.value(platformKey()).toObject();

    /* Get update information */
    m_openUrl = platform.value("open-url").toString();
    m_changelog = platform.value("changelog").toString();
    m_downloadUrl = platform.value("download-url").toString();
    m_latestVersion = platform.value("latest-version").toString();

    /* Compare latest and current version */
    setUpdateAvailable(compare(latestVersion(), moduleVersion()));
    emit checkingFinished(url());
}

void Updater::setUpdateAvailable(const bool available)
{
    m_updateAvailable = available;

    QMessageBox box;
    box.setTextFormat(Qt::RichText);
    box.setIcon(QMessageBox::Information);

    if (updateAvailable() && (notifyOnUpdate() || notifyOnFinish()))
    {
        QString text = tr("Would you like to download the update now?");

        QString title
            = "<h3>" + tr("Version %1 of %2 has been released!").arg(latestVersion(), moduleName()) + "</h3>";

        box.setText(title);
        box.setInformativeText(text);
        box.setStandardButtons(QMessageBox::No | QMessageBox::Yes);
        box.setDefaultButton(QMessageBox::Yes);

        if (box.exec() == QMessageBox::Yes)
        {
            if (!openUrl().isEmpty())
                QDesktopServices::openUrl(QUrl(openUrl()));
            else
                QDesktopServices::openUrl(QUrl(downloadUrl()));
        }
    }
    else if (notifyOnFinish())
    {
        box.setStandardButtons(QMessageBox::Close);
        box.setInformativeText(tr("No updates are available for the moment"));
        box.setText("<h3>"
                    + tr("Congratulations! You are running the "
                         "latest version of %1")
                          .arg(moduleName())
                    + "</h3>");

        box.exec();
    }

}

bool Updater::compare(const QString &x, const QString &y)
{
    QStringList versionsX = x.split(".");
    QStringList versionsY = y.split(".");

    int count = qMin(versionsX.count(), versionsY.count());

    for (int i = 0; i < count; ++i)
    {
        int a = QString(versionsX.at(i)).toInt();
        int b = QString(versionsY.at(i)).toInt();

        if (a > b)
            return true;

        else if (b > a)
            return false;
    }

    return versionsY.count() < versionsX.count();
}
