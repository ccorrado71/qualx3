#include "maintenancetool.h"

#include <QCoreApplication>
#include <QDir>
#include <QMessageBox>
#include <QDebug>
#include <QRegularExpression>

MaintenanceTool::MaintenanceTool(QObject *parent)
    : QObject{parent},
    m_state(MaintenanceTool::NotRunning),
    m_hasUpdate(false),
    m_latestVersion("")
{
    // Signal when the process starts
    connect(&m_process, &QProcess::started, this, &MaintenanceTool::processStarted);
    // Signal when the process finishes
    connect(&m_process, &QProcess::finished, this, &MaintenanceTool::processFinished);
    // Signal when the process encounters an error
    connect(&m_process, &QProcess::errorOccurred, this, &MaintenanceTool::processError);
}

void MaintenanceTool::checkUpdate()
{
    startMaintenanceTool(MaintenanceTool::CheckUpdate);
}

void MaintenanceTool::startMaintenanceTool(StartMode mode)
{
    // Create a full path (uncertain about the current location)
    QString toolName;
    QString path;
#if defined(Q_OS_WIN)
    toolName = "maintenancetool.exe";
#elif defined(Q_OS_MAC)
    toolName = "../../../maintenancetool.app/Contents/MacOS/maintenancetool";
#else
    toolName = "maintenancetool";
#endif
    QDir parentDir = QDir(QCoreApplication::applicationDirPath());
    parentDir.cdUp();
    qDebug() << "ParentDir: " << parentDir;
    //path = QDir(QCoreApplication::applicationDirPath()).absoluteFilePath(toolName);
    path = parentDir.absoluteFilePath(toolName);
    qDebug() << "PATH: " << path;

    QStringList args;
    if (mode == MaintenanceTool::CheckUpdate) {
        // Execute if the process is not running
        if (m_process.state() == QProcess::NotRunning) {
            setUpdateDetails(QString());
            setHasUpdate(false);
            // Set options for update check
            args.append("check-updates");
            // Execute
            m_process.start(path, args);
        } else {
            qDebug() << "Already started.";
        }
    } else {
        // Set options for update
        args.append("--start-updater");
        // Regular launch of maintenance tool without process management
        QProcess::startDetached(path, args);
    }
}

// State of the maintenance tool
MaintenanceTool::ProcessState MaintenanceTool::state() const
{
    return m_state;
}

void MaintenanceTool::setState(ProcessState state)
{
    if (m_state == state) return;
    m_state = state;
    emit stateChanged(m_state);
}

void MaintenanceTool::notifyAboutUpdate()
{
    QMessageBox box;
    box.setTextFormat(Qt::RichText);
    box.setIcon(QMessageBox::Information);

    if (hasUpdate()) {
        QString text = tr("Would you like to download the update now?");

        QString title
            = "<h3>" + tr("Version %1 of %2 has been released!").arg(m_latestVersion, qApp->applicationName()) + "</h3>";

        box.setText(title);
        box.setInformativeText(text);
        box.setStandardButtons(QMessageBox::No | QMessageBox::Yes);
        box.setDefaultButton(QMessageBox::Yes);

        if (box.exec() == QMessageBox::Yes)
        {

        }

    } else {
        box.setStandardButtons(QMessageBox::Close);
        box.setInformativeText(tr("No updates are available for the moment"));
        box.setText("<h3>"
                    + tr("Congratulations! You are running the "
                         "latest version of %1")
                          .arg(qApp->applicationName())
                    + "</h3>");

        box.exec();
    }
}

// Check if there's an update
bool MaintenanceTool::hasUpdate() const
{
    return m_hasUpdate;
}

void MaintenanceTool::setHasUpdate(bool hasUpdate)
{
    if (m_hasUpdate == hasUpdate) return;
    m_hasUpdate = hasUpdate;
    emit hasUpdateChanged(m_hasUpdate);
}

QString MaintenanceTool::updateDetails() const
{
    return m_updateDetails;
}

void MaintenanceTool::setUpdateDetails(const QString &updateDetails)
{
    if (m_updateDetails == updateDetails) return;
    m_updateDetails = updateDetails;
    emit updateDetailsChanged(m_updateDetails);
}

// Process started
void MaintenanceTool::processStarted()
{
    setState(MaintenanceTool::Running);
}

// Process finished
void MaintenanceTool::processFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
    qDebug() << "exitCode=" << exitCode << ", exitStatus=" << exitStatus;

    if (exitCode == 0) {

        // Retrieve standard output
        QByteArray stdOut = m_process.readAllStandardOutput();
        QString stdOutStr = QString::fromLocal8Bit(stdOut);
        qDebug() << "out>" << stdOutStr;

        if (stdOutStr.isEmpty()) {
            qDebug() << "No updates available.";
        } else if (stdOutStr.contains("no updates available")) {
            qDebug() << "No updates available.";
        } else if (stdOutStr.contains("Warning:")) {
            qDebug() << "No updates available.";
        } else  {
            // Get version number from xml lines:
            // <updates>
            //     <update name="Expo" version="2.0.1" size="50412966" id="Expo"/>
            // </updates>

            QStringList lines;
            bool enabled = false;
            lines = stdOutStr.split("\n");

            foreach (const QString &line, lines) {
                if (line.startsWith("<updates>")){
                    enabled = true;
                } else if (line.startsWith("</updates>")) {
                    break;
                } else if (enabled) {
                    static QRegularExpression regex("version=\"([^\"]+)\"");
                    QRegularExpressionMatch match = regex.match(line);

                    if (match.hasMatch()) {
                        m_latestVersion = match.captured(1);
                        qDebug() <<"line: " << line << "Version extracted:" << m_latestVersion;
                    } else {
                        qDebug() <<"line: " << line << "No match found.";
                    }
                }
            }

            setUpdateDetails(stdOutStr);
            setHasUpdate(true);
        }

        notifyAboutUpdate();

        // Change to stopped state
        setState(MaintenanceTool::NotRunning);

    } else if (exitCode == 1) {
        // No update available
        QByteArray stdErr = m_process.readAllStandardError();
        QString stdErrStr = QString::fromLocal8Bit(stdErr);
        qDebug() << "err>" << stdErrStr;

        QMessageBox::warning(nullptr, "Check for Update",
                             "An error occurred while checking for a software update");

        // Change to stopped state
        setState(MaintenanceTool::NotRunning);
    } else {
        // Change to stopped state
        setState(MaintenanceTool::NotRunning);
    }
}

// Process error
void MaintenanceTool::processError(QProcess::ProcessError error)
{
    qDebug() << error;
    setState(MaintenanceTool::NotRunning);
}
