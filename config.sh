#-DCMAKE_PREFIX_PATH serve solo se hai più installazioni di Qt6
#compile with ifort for debug
cmake -DCMAKE_PREFIX_PATH=/home/corrado/Qt/6.5.0/gcc_64 -DCMAKE_BUILD_TYPE=debug -DCMAKE_Fortran_COMPILER=ifort -S . -B build_debug_ifort

#compile with gfortran for debug
#cmake -DCMAKE_PREFIX_PATH=/home/corrado/Qt/6.5.0/gcc_64 -DCMAKE_BUILD_TYPE=debug -DCMAKE_Fortran_COMPILER=gfortran -S . -B build_debug_gfortran
