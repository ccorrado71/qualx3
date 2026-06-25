#ifndef MAINTENANCETOOL_H
#define MAINTENANCETOOL_H

#include <QObject>
#include <QProcess>

class MaintenanceTool : public QObject
{
    Q_OBJECT
public:
    explicit MaintenanceTool(QObject *parent = nullptr);

    enum ProcessState {
        NotRunning,
        Running
    };

    enum StartMode {
        CheckUpdate,
        Updater
    };

    ProcessState state() const;                           // State of the maintenance tool
    bool hasUpdate() const;                               // Check for updates
    QString updateDetails() const;                        // Details about the update

signals:
    // Signals emitted when values change
    void stateChanged(ProcessState state);
    void hasUpdateChanged(bool hasUpdate);
    void updateDetailsChanged(const QString &updateDetails);

public slots:
    void checkUpdate();                                   // Start checking for updates
    void startMaintenanceTool(StartMode mode = Updater);

private slots:
    void setHasUpdate(bool hasUpdate);                    // Check for updates
    void setUpdateDetails(const QString &updateDetails);  // Details about the update
    // Slots related to the process operation
    void processStarted();
    void processFinished(int exitCode, QProcess::ExitStatus exitStatus);
    void processError(QProcess::ProcessError error);

private:
    ProcessState m_state;
    bool m_hasUpdate;
    QString m_updateDetails;
    QProcess m_process;
    QString m_latestVersion;

    void setState(ProcessState state);
    void notifyAboutUpdate();
};

#endif // MAINTENANCETOOL_H
