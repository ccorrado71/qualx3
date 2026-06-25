rem Tested with: oneAPI 2024, MSVC 2022, Qt 6.8.1
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"

"C:\Program Files\Microsoft Visual Studio\2022\Community\Common7\IDE\CommonExtensions\Microsoft\CMake\CMake\bin\cmake.exe" ^
-B "C:/qualx3/build_ifort_release" ^
-S "C:/qualx3" ^
-DCMAKE_GENERATOR:STRING=Ninja ^
-DCMAKE_BUILD_TYPE:STRING=Release ^
-DCMAKE_INSTALL_PREFIX="C:/qualx3/install" ^
-DCMAKE_Fortran_FLAGS_RELEASE="/O2" ^
-DCMAKE_PREFIX_PATH="C:/Qt/6.8.1/msvc2022_64" ^
-DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifx.exe" ^
-DIFORT_LIB_DIR="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/lib" ^
-DIFORT_DLL_DIR="C:/Program Files (x86)/Intel/oneAPI/compiler/2025.0/bin" ^
-DCMAKE_CXX_COMPILER:FILEPATH="C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.44.35207/bin/HostX64/x64/cl.exe"
