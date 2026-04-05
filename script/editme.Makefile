#add -G Ninja  at configure step to change generator

NCPU := 10
INSTALL_FOLDER := $(HOME)/myapps/expo2/
#QT_FOLDER := "/usr"
#QT_FOLDER := $(HOME)/Qt/6.8.0/macos
QT_FOLDER := $(HOME)/Qt/6.8.0/gcc_64

error:
	@echo "Usage: make <target>"

prepare:
	rm -rf build_gfor_d
	rm -rf build_ifort_d
	rm -rf build_gfor_r
	rm -rf build_ifort_r
	rm -rf build_ifort_rd
	rm -rf build_ifx_r
	rm -rf build_ifx_d
	rm -rf build_mpiifort_r
	rm -rf build_mpifort_r
	rm -rf build_mpifort_d
	rm -rf build_mpiifx_r
	rm -rf build_mpiifx_d

prepare_gford:
	rm -rf build_gfor_d

prepare_gfor:
	rm -rf build_gfor_r

prepare_ifortd:
	rm -rf build_ifort_d

prepare_ifort:
	rm -rf build_ifort_r

prepare_ifortrd:
	rm -rf build_ifort_rd

prepare_ifx:
	rm -rf build_ifx_r

prepare_ifxd:
	rm -rf build_ifx_d

prepare_mpiifx:
	rm -rf build_mpiifx_r

prepare_mpiifxd:
	rm -rf build_mpiifx_d

prepare_mpiifort:
	rm -rf build_mpiifort_r

prepare_mpiifortd:
	rm -rf build_mpiifort_d

prepare_mpifort:
	rm -rf build_mpifort_r

config_gford:
	cmake -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=Debug -B build_gfor_d -S . -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER)

config_gfor:
	cmake -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=Release -B build_gfor_r -S . -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER)

config_ifortd:
	cmake -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_BUILD_TYPE=Debug -B build_ifort_d -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_ifort:
	cmake -DCMAKE_Fortran_COMPILER=ifort  -DCMAKE_BUILD_TYPE=Release -B build_ifort_r -S . -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_ifxd:
	cmake -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_BUILD_TYPE=Debug -B build_ifx_d -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_ifx:
	cmake -DCMAKE_Fortran_COMPILER=ifx  -DCMAKE_BUILD_TYPE=Release -B build_ifx_r -S . -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_ifortrd:
	cmake -DCMAKE_Fortran_COMPILER=ifort  -DCMAKE_BUILD_TYPE=RelWithDebInfo -B build_ifort_rd -S . -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_mpiifort:
	cmake -DCMAKE_Fortran_COMPILER=mpiifort -DCMAKE_CXX_COMPILER=mpiicpx -DCMAKE_BUILD_TYPE=Release -B build_mpiifort_r -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_mpiifortd:
	cmake -DCMAKE_Fortran_COMPILER=mpiifort -DCMAKE_CXX_COMPILER=mpiicpx -DCMAKE_BUILD_TYPE=Debug -B build_mpiifort_d -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_mpifort:
	cmake -DCMAKE_Fortran_COMPILER=mpifort  -DCMAKE_BUILD_TYPE=Release -B build_mpifort_r -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_mpifortd:
	cmake -DCMAKE_Fortran_COMPILER=mpifort  -DCMAKE_BUILD_TYPE=Debug -B build_mpifort_d -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_mpiifx:
	cmake -DCMAKE_Fortran_COMPILER=mpiifx -DCMAKE_CXX_COMPILER=mpiicpx -DCMAKE_BUILD_TYPE=Release -B build_mpiifx_r -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

config_mpiifxd:
	cmake -DCMAKE_Fortran_COMPILER=mpiifx -DCMAKE_CXX_COMPILER=mpiicpx -DCMAKE_BUILD_TYPE=Debug -B build_mpiifx_d -DCMAKE_INSTALL_PREFIX=$(INSTALL_FOLDER) -DCMAKE_PREFIX_PATH=$(QT_FOLDER) -S .

build_gford:
	cmake --build build_gfor_d --parallel $(NCPU) --target install

build_gfor:
	cmake --build build_gfor_r --parallel $(NCPU) --target install

build_ifortd:
	cmake --build build_ifort_d --parallel $(NCPU) --target install

build_ifort:
	cmake --build build_ifort_r --parallel $(NCPU) --target install

build_ifxd:
	cmake --build build_ifx_d --parallel $(NCPU) --target install

build_ifx:
	cmake --build build_ifx_r --parallel $(NCPU) --target install

build_ifortrd:
	cmake --build build_ifort_rd --parallel $(NCPU) --target install

build_mpiifortd:
	cmake --build build_mpiifort_d --parallel $(NCPU) --target install

build_mpiifort:
	cmake --build build_mpiifort_r --parallel $(NCPU) --target install

build_mpifortd:
	cmake --build build_mpifort_d --parallel $(NCPU) --target install

build_mpifort:
	cmake --build build_mpifort_r --parallel $(NCPU) --target install

build_mpiifxd:
	cmake --build build_mpiifx_d --parallel $(NCPU) --target install

build_mpiifx:
	cmake --build build_mpiifx_r --parallel $(NCPU) --target install
