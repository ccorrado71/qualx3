#ifndef COMMANDLINE_H
#define COMMANDLINE_H

#include <QCommandLineParser>

struct ProgOptions
{
    int nogui;
    int autom;
    int indexing;
    float wavel;
};

enum CommandLineParseResult
{
    CommandLineOk,
    CommandLineError,
    CommandLineVersionRequested,
    CommandLineHelpRequested
};

CommandLineParseResult parseCommandLine(QCommandLineParser &parser, QString &filein, QString &fileout,
                                        ProgOptions &popt, QString &errorMessage, bool &test, QString &testFolder);

#endif // COMMANDLINE_H
