#! /bin/bash

error()
{
SHELLNAME=`basename $0`
if ! test -z "${1}"; then
   echo ""
   echo "${SHELLNAME}: $1"
   echo ""
   exit 1
else
   echo "${SHELLNAME}: Creates DMG file."
   echo "Usage: ${SHELLNAME} [OPTION] app dmg_name"
   echo ""
   echo "app is used as volume name (displayed in the Finder sidebar and window title)"
   echo "app is also used as icon file name (app.icns)"
   echo "and as dmg background file name (app.png)"
   echo "Example: ./make_dmg.sh qualx qualx-1.0.3-arm64"
   echo "Options:"
   echo "  --skip-deploy"
   echo "      does not run macdeployqt"
   echo "  --only-dmg"
   echo "      builds dmg file only, starting from directory 'app'"
   echo "  -h, --help        display this help"
   exit 1
fi
}

set_icon()
{
if test -d "${1}"; then
  cp "${2}" "${1}/.VolumeIcon.icns"
  SetFile -c icnC "${1}/.VolumeIcon.icns"
  SetFile -a C "${1}"
else
  ./fileicon set "${1}" "${2}"
fi
}

crea_dmg()
{
VOLUME_NAME=$1
APP_NAME=$1.app
VOLUME_ICON_FILE=$1.icns
IMAGE_NAME=$1w.dmg
echo "Creating volume $VOLUME_NAME"

DMG_NAME=$2
DEV_NAME=$(hdiutil info | egrep '^/dev/' | sed 1q | awk '{print $1}')
MOUNT_DIR="/Volumes/${VOLUME_NAME}"
test -d "${MOUNT_DIR}" && hdiutil detach "${DEV_NAME}"
rm -f $IMAGE_NAME
hdiutil create -fs HFS+ -format UDRW -volname $VOLUME_NAME -srcfolder ./$APP_NAME $IMAGE_NAME
DEV_NAME=$(hdiutil attach -readwrite -noverify -noautoopen "${IMAGE_NAME}" | egrep '^/dev/' | sed 1q | awk '{print $1}')
ln -s /Applications "$MOUNT_DIR/Applications"
set_icon "${MOUNT_DIR}" "${VOLUME_ICON_FILE}"

CURDIR=$PWD
backimg="${CURDIR}/${NAME}.png"
dir="${MOUNT_DIR}";
rm -rf "${dir}/.background"
echo "mkdir ${dir}/.background"
mkdir -p $dir/.background
echo "copying background"
cp $backimg $dir/.background/back_image.png

cat >tmpscript << EOF
   tell application "Finder"
     tell disk "${VOLUME_NAME}"
           open
           set theViewOptions to the icon view options of container window
           tell container window
                set current view to icon view
                set toolbar visible to false
                set statusbar visible to false
                set the bounds to {100, 100, 600, 460}
                set arrangement of theViewOptions to not arranged
                set icon size of theViewOptions to 48
EOF
if ! test -z $bottom; then
cat >>tmpscript << EOF
                set label position of theViewOptions to bottom
EOF
else
cat >>tmpscript << EOF
                set label position of theViewOptions to right
EOF
fi
cat >>tmpscript << EOF
                set background picture of theViewOptions to file ".background:back_image.png"
           end tell
           tell container window
                set sidebar width to 0
                set position of item "${APP_NAME}" to {$ICONP_X, $ICON_Y}
                set position of item "Applications" to {$ICONA_X, $ICON_Y}
EOF
cat >>tmpscript << EOF
                tell application "System Events" to tell process "Finder"
                   tell menu item "Show View Options" of menu of menu bar item "View" of menu bar 1 to if exists then click
                   tell checkbox "Always open in icon view" of window 1 to if (exists) and value is 0 then click
                   tell checkbox "Always open in list view" of window 1 to if (exists) and value is 1 then click
                   tell menu item "Mostra opzioni Vista" of menu of menu bar item "Vista" of menu bar 1 to if exists then click
                   tell checkbox "Apri sempre in vista icona" of window 1 to if (exists) and value is 0 then click
                   tell checkbox "Apri sempre in vista elenco" of window 1 to if (exists) and value is 1 then click
                   delay 2
                   tell menu item "Hide view options" of menu of menu bar item "View" of menu bar 1 to if exists then click
                   tell menu item "Nascondi opzioni vista" of menu of menu bar item "Vista" of menu bar 1 to if exists then click

		end tell
           end tell
           delay 5
           update without registering applications
           close
           eject
     end tell
   end tell
EOF

echo "Setting view options - please wait"
cat tmpscript | osascript
rm -f tmpscript
rm -f $DMG_NAME
hdiutil convert $IMAGE_NAME -format UDZO -o $DMG_NAME
set_icon "${DMG_NAME}" "${VOLUME_ICON_FILE}"
}

## main script

NAME=""
NAME2=""
ICON_Y="200"
ICONP_X="140"
ICONA_X="330"
SKIP_DEPLOY=""
ONLY_DMG=""

CURDIR=$PWD

openbabeldir=`pkg-config --variable=prefix openbabel-3 2>/dev/null`

#Check arguments
let nargs=$#
let arg=1
while test $# -gt 0
do
if test "${1:0:1}" = "-"; then
  case $1 in
    --skip-deploy)
      SKIP_DEPLOY="yes"
      shift;;
    --only-dmg)
      ONLY_DMG="yes"
      shift;;
    -h | --help)
      error ;;
    *)
      error "Invalid option $1";;
  esac
  let nargs--
else
  case $arg in
    1)
      NAME=`basename $1 .app`;;
    2)
      NAME2=`basename $1 .dmg`;;
  esac
  shift
  let arg++
fi
done

if test -z $NAME2; then
   NAME2=`basename $NAME .dmg`
fi

if test $nargs -lt 1; then
  error "Missing argument(s)"
fi

APPNAME="${NAME}.app"
DMGNAME="${NAME2}.dmg"

if ! test -d ${APPNAME}; then
   error "Missing ${APPNAME}. You must build your project, using Release option before run this shell"
fi
cd $CURDIR

#if macdeployqt already runned, skip other run
#else force run

dirFram="./${APPNAME}/Contents/Frameworks"
if ! test -d $dirFram; then
   SKIP_DEPLOY=""
else
   SKIP_DEPLOY="yes"
fi
if test -z  $SKIP_DEPLOY; then
    a=`which macdeployqt`
    if test -z $a; then
        error "macdeployqt directory must be in your path"
    fi
    macdeployqt ${APPNAME} -verbose=2
    cd $CURDIR
fi

# Fix openbabel plugin libraries (only if openbabel_formats directory exists)
OPENBABEL_FORMATS_DIR="./${APPNAME}/Contents/share/openbabel_formats"
FRAMEWORKS_DIR="./${APPNAME}/Contents/Frameworks"

if test -d "${OPENBABEL_FORMATS_DIR}" && ! test -z "${openbabeldir}"; then
    cd "${OPENBABEL_FORMATS_DIR}"
    echo "copy missing openbabel libraries in Frameworks"
    for file in *.so
    do
       b=`otool -L $file | grep "${openbabeldir}"|cut -f1 -d" " |sed s/^" "*//`
       for i in $b; do
         if test -f $i; then
           nome=`basename $i`
           if ! test -f ../../Frameworks/$nome; then
              echo "copying $nome"
              cp $i ../../Frameworks/$nome
           fi
           install_name_tool -change $i @executable_path/../Frameworks/$nome $file
         fi
       done
    done
    cd $CURDIR
fi

if test -d "${FRAMEWORKS_DIR}"; then
    echo "now fix all libraries in Frameworks"
    cd "${FRAMEWORKS_DIR}"

    if ! test -z "${openbabeldir}"; then
        for file in *.dylib
        do
           b=`otool -L $file | grep "${openbabeldir}"|cut -f1 -d" " |sed s/^" "*//`
           for i in $b; do
             if test -f $i; then
               nome=`basename $i`
               install_name_tool -change $i @executable_path/../Frameworks/$nome $file
             fi
           done
           install_name_tool -id @executable_path/../Frameworks/$file $file
        done
    fi

#    #fix problem with libgcc_s.1.dylib used by libgfortran.5.dylib
#    LIBGCC="/opt/homebrew/opt/gcc/lib/gcc/current/libgcc_s.1.1.dylib"
#    if test -f $LIBGCC; then
#       cp $LIBGCC .
#       install_name_tool -id @executable_path/../Frameworks/libgcc_s.1.1.dylib libgcc_s.1.1.dylib
#       install_name_tool -change $LIBGCC @executable_path/../Frameworks/libgcc_s.1.1.dylib libgfortran.5.dylib
#    fi

    #fix problem with libgcc_s.1.1.dylib used by libgfortran.5.dylib
    if test -f libgfortran.5.dylib; then
        LIBGCC_RPATH=$(otool -L libgfortran.5.dylib | grep libgcc_s | awk '{print $1}')
        LIBGCC_NAME=$(basename "$LIBGCC_RPATH")
        # otool returns @rpath/... not a real path; use gfortran to resolve the actual location
        LIBGCC_REAL=$(gfortran -print-file-name="$LIBGCC_NAME" 2>/dev/null)
        if test -f "$LIBGCC_REAL"; then
            cp "$LIBGCC_REAL" .
            install_name_tool -id "@executable_path/../Frameworks/$LIBGCC_NAME" "$LIBGCC_NAME"
            install_name_tool -change "$LIBGCC_RPATH" "@executable_path/../Frameworks/$LIBGCC_NAME" libgfortran.5.dylib
        else
            echo "WARNING: $LIBGCC_NAME not found via gfortran -print-file-name"
        fi
    fi

    cd $CURDIR
fi

# Sign the app bundle from the inside out (required on Apple Silicon / macOS 12+)
# install_name_tool invalidates existing signatures, so each component must be re-signed
echo "Signing frameworks and dylibs..."
find "./${APPNAME}/Contents/Frameworks" -name "*.dylib" | while read f; do
    codesign --force --sign - "$f"
done
find "./${APPNAME}/Contents/Frameworks" -name "*.framework" -type d | while read f; do
    codesign --force --sign - "$f"
done
find "./${APPNAME}/Contents/PlugIns" -name "*.dylib" 2>/dev/null | while read f; do
    codesign --force --sign - "$f"
done
echo "Signing app bundle..."
codesign --force --deep --sign - "./${APPNAME}"

# Remove quarantine/provenance attributes that prevent launch from Finder
echo "Removing quarantine and provenance attributes..."
xattr -r -d com.apple.quarantine "./${APPNAME}" 2>/dev/null
xattr -r -d com.apple.provenance "./${APPNAME}" 2>/dev/null

##Make dmg with background
crea_dmg $NAME $DMGNAME

