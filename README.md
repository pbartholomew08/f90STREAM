# f90STREAM

Fortran90 implementation of STREAM with dynamic memory allocation.

This is a reimplementation of the [STREAM](http://www.cs.virginia.edu/stream/) benchmark by
Dr. McCalpin in Fortran90.
The purpose is to use dynamic, rather than static, memory allocation, and to compare the use of Cray
pointers with Fortran90 `allocatable` arrays.

## Building and running

To build, run `make` from the `f90STREAM` root directory, this requires a Fortran90 and compatible C
compiler.
You can specify the compiler suite by setting the `CMP` variable, for example `make CMP=gnu` will
build using `gfortran` and `gcc`.

After building the benchmark can be run by executing `./stream`, the run can be customised with the
following run-time arguments:
- `-n` sets the problem size (default value 1000000)
- `-r` sets the number of kernel executions (default value 10)
- `-c` uses Cray pointers, rather than Fortran90 `allocatable` arrays, for dynamic memory

On completion, the program will print the achieved bandwidth and floating point operation rates for
each kernel.
