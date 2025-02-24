#include "commandline.h"

#include <QFileInfo>

CommandLineParseResult parseCommandLine(QCommandLineParser &parser, QString &filein, QString &fileout,
                                        ProgOptions &popt, QString &errorMessage, bool &test, QString &testFolder)
{
    parser.addPositionalArgument("file1",QCoreApplication::translate("main","Input file, optionally"),"[file1]");
    parser.addPositionalArgument("file2",QCoreApplication::translate("main","Output file, optionally"),"[file2]");
    const QCommandLineOption helpOption = parser.addHelpOption();
    const QCommandLineOption versionOption = parser.addVersionOption();
    const QCommandLineOption noguiOption("nogui", "Run the program without graphics");
    parser.addOption(noguiOption);
    const QCommandLineOption autoOption("auto", "Run the program in automatic mode");
    parser.addOption(autoOption);
    const QCommandLineOption testOption("test", "Run the program in test mode");
    parser.addOption(testOption);

    if (!parser.parse(QCoreApplication::arguments())) {
        errorMessage = parser.errorText();
        return CommandLineError;
    }

    if (parser.isSet(versionOption))
        return CommandLineVersionRequested;

    if (parser.isSet(helpOption))
        return CommandLineHelpRequested;

    popt = {0, 0, 0, -1};

    if (parser.isSet((noguiOption))) {
        popt.nogui = 1;
    }

    if (parser.isSet(autoOption)) {
        popt.autom = 1;
    }

    test = parser.isSet(testOption);

    const QStringList positionalArguments = parser.positionalArguments();
    if (!positionalArguments.isEmpty()) {
        if (positionalArguments.size() > 2) {
            errorMessage = "Several 'name' arguments specified.";
            return CommandLineError;
        }
        QString arg = positionalArguments.first();
        if (QFileInfo(arg).isDir()) {
            testFolder = arg;
            return CommandLineOk;
        }
        filein = arg;
        if (positionalArguments.size() == 2) fileout = positionalArguments.at(1);
    }

    return CommandLineOk;
}
