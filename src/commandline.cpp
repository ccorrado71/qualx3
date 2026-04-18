#include "commandline.h"

#include <QFileInfo>

CommandLineParseResult parseCommandLine(QCommandLineParser &parser, QString &filein, QString &fileout,
                                        ProgOptions &popt, DbBuildOptions &dbopt, SearchOptions &searchopt,
                                        QString &errorMessage, bool &test, QString &testFolder)
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

    // Database creation options
    const QCommandLineOption createDbOption("createdb", "Create a Qualx SQLite database");
    parser.addOption(createDbOption);
    const QCommandLineOption pdf2Option("pdf2", "Path to pdf2.dat file (source: PDF-2)", "pdf2file");
    parser.addOption(pdf2Option);
    const QCommandLineOption cifDirOption("cifdir", "Folder containing .cif files (source: CIF)", "folder");
    parser.addOption(cifDirOption);
    const QCommandLineOption recursiveOption("recursive", "Scan CIF folder recursively");
    parser.addOption(recursiveOption);
    const QCommandLineOption inorganicOption("inorganic", "Include only inorganic structures (CIF source only)");
    parser.addOption(inorganicOption);
    const QCommandLineOption dbOutOption("dbout", "Output database base path (without extension)", "dbpath");
    parser.addOption(dbOutOption);

    // Search options
    const QCommandLineOption searchOption("search", "Run a database search and show results");
    parser.addOption(searchOption);
    const QCommandLineOption compositionOption("composition",
        "Composition filter formula (e.g. \"Al AND Si\", \"Ti OR Fe\", \"Ca NOT O\"). "
        "Operators: AND, OR, NOT (case-insensitive).", "formula");
    parser.addOption(compositionOption);
    const QCommandLineOption exactOption("exact",
        "Exact composition: match structures containing exactly the listed elements and no others");
    parser.addOption(exactOption);
    const QCommandLineOption containsAnyOption("contains-any",
        "Contains-any composition: match structures whose elements are a subset of the listed ones");
    parser.addOption(containsAnyOption);

    if (!parser.parse(QCoreApplication::arguments())) {
        errorMessage = parser.errorText();
        return CommandLineError;
    }

    if (parser.isSet(versionOption))
        return CommandLineVersionRequested;

    if (parser.isSet(helpOption))
        return CommandLineHelpRequested;

    popt = {0, 0, 0, -1};
    dbopt = {};
    searchopt = {};

    if (parser.isSet((noguiOption))) {
        popt.nogui = 1;
    }

    if (parser.isSet(autoOption)) {
        popt.autom = 1;
    }

    test = parser.isSet(testOption);

    // Parse database creation options
    if (parser.isSet(createDbOption)) {
        dbopt.enabled = true;
        popt.nogui = 1;  // db creation runs without GUI

        if (parser.isSet(pdf2Option) && parser.isSet(cifDirOption)) {
            errorMessage = "Options --pdf2 and --cifdir are mutually exclusive.";
            return CommandLineError;
        }
        if (!parser.isSet(pdf2Option) && !parser.isSet(cifDirOption)) {
            errorMessage = "Option --createdb requires either --pdf2 <file> or --cifdir <folder>.";
            return CommandLineError;
        }
        if (!parser.isSet(dbOutOption)) {
            errorMessage = "Option --createdb requires --dbout <dbpath>.";
            return CommandLineError;
        }

        dbopt.outputDb = parser.value(dbOutOption);

        if (parser.isSet(pdf2Option)) {
            dbopt.source   = DbBuildOptions::Source::Pdf2;
            dbopt.pdf2File = parser.value(pdf2Option);
            if (!QFileInfo::exists(dbopt.pdf2File)) {
                errorMessage = QString("PDF-2 file not found: %1").arg(dbopt.pdf2File);
                return CommandLineError;
            }
        } else {
            dbopt.source    = DbBuildOptions::Source::Cif;
            dbopt.cifDir    = parser.value(cifDirOption);
            dbopt.recursive = parser.isSet(recursiveOption);
            dbopt.inorganic = parser.isSet(inorganicOption);
            if (!QFileInfo(dbopt.cifDir).isDir()) {
                errorMessage = QString("CIF folder not found: %1").arg(dbopt.cifDir);
                return CommandLineError;
            }
        }
    }

    // Parse search options
    if (parser.isSet(searchOption)) {
        searchopt.enabled = true;
        if (parser.isSet(compositionOption))
            searchopt.composition = parser.value(compositionOption).trimmed();
        if (parser.isSet(exactOption) && parser.isSet(containsAnyOption)) {
            errorMessage = "Options --exact and --contains-any are mutually exclusive.";
            return CommandLineError;
        }
        searchopt.exactComposition = parser.isSet(exactOption);
        searchopt.containsAny      = parser.isSet(containsAnyOption);
    }

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
