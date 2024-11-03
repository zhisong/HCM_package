Files

  he12a.f  - source code of HELENA
  ppplib.f - library for plotting
  runhel   - script to run HELENA

Directories

  namelist - input files
  output   - screen output of the code
  mapping  - mapping for stability codes (CSCAS, MISHKA)
  plot     - plot of the result
  documentation - official guide to HELENA

To compile

$ make

External Libraries : lapack blas


To run

$ ./runhel namelist_file

for example :

$ ./runhel solex


