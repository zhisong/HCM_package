*MISHIKA-1 file list

src : source file of MISHKA
lib : all other dependent libraries except libblas

namelist, output, mapping, plot : store input and output files


*Compile MISHIKA:

$ make

If successful, an executable 'mishika.exe' will be automatically generated

NOTE: gfortran and blas library is required. (Included in PRL_PTM virtual machines)
If you don't have them, you can install gfortran and blas library by

$ sudo apt-get install gfortran
$ sudo apt-get install libblas-dev


*Run MISHIKA:

1. copy mapping file from HELENA mapping directory into 'mapping' under this directory
2. make the namelist and store it into 'namelist' under this directory
3. run MISHIKA

$ ./runmis mappingfile namelistfile

for example,

$ ./runmis he18696_290_10a mi18696_290_10a_1

NOTE: This version of MISHIKA do not have any modification.




