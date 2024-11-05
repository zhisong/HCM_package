MISHKA-F / MISHKA-2

src : source file

namelist, output, mapping, plot : store input and output files


*Compile MISHKA-F:

$ make

If successful, an executable 'mishka.exe' will be automatically generated

NOTE: gfortran, blas and lapack library is required. (Included in PRL_PTM virtual machines)

*Run MISHKA:

1. copy mapping file from HELENA mapping directory into 'mapping' under this directory
2. make the namelist and store it into 'namelist' under this directory
3. run MISHKA

$ ./runmis mappingfile namelistfile

for example,

$ ./runmis he18696_290_10a mi18696_290_10a_1



