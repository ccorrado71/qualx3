function Controller()
{
    installer.setDefaultPageVisible(QInstaller.ComponentSelection,false);
    // Skip the Start menu folder page: shortcuts go to the default folder
    installer.setDefaultPageVisible(QInstaller.StartMenuSelection,false);
}
