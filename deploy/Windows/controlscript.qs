function Controller()
{
    installer.setDefaultPageVisible(QInstaller.ComponentSelection,false);
    // Skip the Start menu folder page: shortcuts go to the default folder
    installer.setDefaultPageVisible(QInstaller.StartMenuSelection,false);
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
}
