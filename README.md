# f90STREAM

Fortran90 implementation of STREAM with dynamic memory allocation.

This is a reimplementation of the [STREAM][http://www.cs.virginia.edu/stream/] benchmark by
Dr. McCalpin in Fortran90.
The purpose is to use dynamic, rather than static, memory allocation, and to compare the use of Cray
pointers with Fortran90 `allocatable` arrays.
