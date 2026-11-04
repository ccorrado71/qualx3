function Controller()
{
    installer.setDefaultPageVisible(QInstaller.ComponentSelection,false);
    // Skip the Start menu folder page: shortcuts go to the default folder
    installer.setDefaultPageVisible(QInstaller.StartMenuSelection,false);
}

function maintenanceToolPath(dir)
{
    return dir + "/maintenancetool.exe";
}

function containsQualx(dir)
{
    return installer.fileExists(maintenanceToolPath(dir))
        && installer.fileExists(dir + "/bin/qualx.exe");
}

// IFW refuses a target directory holding an installation, unless RemoveTargetDir is
// false. Turn the check off only for a previous QualX: any other directory keeps the
// standard checks (other installations, folders that are not empty).
function updateTargetDirCheck(dir)
{
    installer.setValue("RemoveTargetDir", containsQualx(dir) ? "false" : "true");
}

Controller.prototype.TargetDirectoryPageCallback = function()
{
    var page = gui.currentPageWidget();
    if (!page.targetDirCheckConnected) {
        page.targetDirCheckConnected = true;
        page.TargetDirectoryLineEdit.textChanged.connect(updateTargetDirCheck);
    }
    updateTargetDirCheck(page.TargetDirectoryLineEdit.text);
}

// Do not run this from installer.installationStarted: there the concurrent Extract
// operations randomly get corrupted arguments (seen with IFW 4.8.1), and the
// installation fails after the old version has been removed.
function removePreviousInstallation(targetDir)
{
    var tool = maintenanceToolPath(targetDir);
    var result = installer.execute(tool, ["purge", "--confirm-command", "--default-answer"]);

    // The command line of the tool cannot ask for admin rights by itself, and they are
    // needed in Program Files: run it again elevated, which shows the UAC prompt.
    if (result[1] !== 0) {
        installer.execute("powershell", ["-NoProfile", "-WindowStyle", "Hidden", "-Command",
            "Start-Process -FilePath '" + tool.replace(/'/g, "''") + "'"
            + " -ArgumentList 'purge','--confirm-command','--default-answer'"
            + " -Verb RunAs -Wait"]);
    }

    // After exiting, a script of the tool deletes the tool and then the whole folder:
    // wait for the folder, or that deletion hits the files of this installation. The
    // folder can survive if a file in it is locked, so give up soon after the tool has
    // gone (about one second per ping).
    var waitAfterToolRemoved = 5;
    for (var i = 0; i < 30 && installer.fileExists(targetDir) && waitAfterToolRemoved > 0; ++i) {
        installer.execute("cmd", ["/c", "ping -n 2 127.0.0.1 >nul"]);
        if (!installer.fileExists(tool))
            --waitAfterToolRemoved;
    }

    return !installer.fileExists(tool);
}

// The hidden Start menu page is what turns @StartMenuDir@ (just the folder name,
// e.g. "QualX") into a full path when it is left. Do the same here, right before
// installing, or the Start menu shortcuts are not created in the Start menu.
Controller.prototype.ReadyForInstallationPageCallback = function()
{
    if (!installer.isInstaller())
        return;
    var startMenuDir = installer.value("StartMenuDir");
    var programsPath = installer.value(installer.value("AllUsers") === "true"
                                       ? "AllUsersStartMenuProgramsPath"
                                       : "UserStartMenuProgramsPath");
    if (startMenuDir.indexOf(programsPath) !== 0)
        installer.setValue("StartMenuDir", programsPath + "/" + startMenuDir);

    var targetDir = installer.value("TargetDir");
    if (containsQualx(targetDir)) {
        var answer = QMessageBox.question("UninstallPreviousVersion", "QualX",
            "A previous version of QualX is installed in this folder.\n"
            + "It will be uninstalled and all the files in the folder will be deleted.\n\n"
            + "Do you want to continue?", QMessageBox.Yes | QMessageBox.No);
        if (answer !== QMessageBox.Yes) {
            gui.clickButton(buttons.BackButton);
            return;
        }
        // Back to the target directory page: interrupt() would not disable Install here
        if (!removePreviousInstallation(targetDir)) {
            QMessageBox.critical("PreviousInstallationNotRemoved", "QualX",
                                 "The previous version of QualX could not be removed.\n"
                                 + "Uninstall it from Settings > Installed apps and try again.");
            gui.clickButton(buttons.BackButton);
            return;
        }
    }

    // RemoveTargetDir is saved in the maintenance tool of this installation
    installer.setValue("RemoveTargetDir", "true");
}
