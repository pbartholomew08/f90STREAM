!! Copyright 2020 University of Edinburgh
!!
!! Licensed under the Apache License, Version 2.0 (the "License");
!! you may not use this file except in compliance with the License.
!! You may obtain a copy of the License at
!!      http://www.apache.org/licenses/LICENSE-2.0
!! Unless required by applicable law or agreed to in writing, software
!! distributed under the License is distributed on an "AS IS" BASIS,
!! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!! See the License for the specific language governing permissions and
!! limitations under the License.
!!
!!=========================================================================
!!
!! This software was developed as part of the EPSRC funded project
!! ASiMoV (Project ID: EP/S005072/1)
!!
!!=========================================================================

module vars

  implicit none

  double precision, allocatable, dimension(:) :: a, b, c
  pointer (aptr, a77), (bptr, b77), (cptr, c77)
  double precision :: a77(1), b77(1), c77(1)
  double precision :: q
  
end module vars
  
program stream

  use vars
  
  implicit none
  
  logical :: crayalloc = .false.
  integer :: n = 1000000
  integer :: nrep = 10

  print *, "======================="
  print *, " f90STREAM benchmark"
  print *, "Runs the STREAM benchmark"
  print *, "using dynamically allocated"
  print *, "memory."
  print *, "======================="
  
  !! Input:
  call parse_args(n, nrep, crayalloc)
  
  !! Allocate
  if (.not.crayalloc) then
     call allocate_f90(n)
     call benchmark(a, b, c, q, n, nrep)
     call deallocate_f90()
  else
     call allocate_cray(n)
     call benchmark(a77, b77, c77, q, n, nrep)
     call deallocate_cray()
  endif
  
contains

  subroutine parse_args(n, nrep, crayalloc)

    implicit none

    integer, intent(inout) :: n, nrep
    logical, intent(inout) :: crayalloc

    character(len=:), allocatable :: arg
    integer :: arglen, argc
    integer :: i

    argc = command_argument_count()

    do i = 1, argc
       call get_command_argument(i, length=arglen)
       allocate(character(arglen) :: arg)
       call get_command_argument(i, value=arg)
       if (arg .eq. "-n") then
          deallocate(arg)
          call get_command_argument(i + 1, length=arglen)
          allocate(character(arglen) :: arg)
          call get_command_argument(i + 1, value=arg)
          read(arg, '(I9)') n
          deallocate(arg)
       else if (arg .eq. "-r") then
          deallocate(arg)
          call get_command_argument(i + 1, length=arglen)
          allocate(character(arglen) :: arg)
          call get_command_argument(i + 1, value=arg)
          read(arg, '(I9)') nrep
          deallocate(arg)
       else if (arg .eq. "-c") then
          deallocate(arg)
          crayalloc = .true.
       else
          deallocate(arg)
       endif
    enddo

    print *, "n             = ", n
    print *, "nreps         = ", nrep
    print *, "Cray pointers:  ", crayalloc
    print *, "======================="
    
  end subroutine parse_args
  
  subroutine allocate_f90(n)

    use vars

    implicit none

    integer, intent(in) :: n

    allocate(a(n), b(n), c(n))

  end subroutine allocate_f90

  subroutine allocate_cray(n)
    
    use vars

    implicit none

    integer, intent(in) :: n

    integer(8) cmalloc
    external cmalloc

    aptr = cmalloc(n)
    bptr = cmalloc(n)
    cptr = cmalloc(n)
    
  end subroutine allocate_cray

  subroutine deallocate_f90()

    use vars

    implicit none

    deallocate(a, b, c)

  end subroutine deallocate_f90

  subroutine deallocate_cray()

    use vars

    implicit none

  end subroutine deallocate_cray

  subroutine benchmark(a, b, c, q, n, nrep)

    implicit none

    double precision, intent(inout) :: a(:), b(:), c(:)
    double precision, intent(inout) :: q
    integer, intent(in) :: n, nrep

    real    :: tcopy = 0, tscale = 0, tsum = 0, ttriad = 0
    integer :: r
    
    do r = 1, nrep
       call init(a, b, c, n, q)
       call copy(a, b, n, tcopy)
       call scale(a, b, q, n, tscale)
       call sum(a, b, c, n, tsum)
       call triad(a, b, c, q, n, ttriad)
    enddo

    call report(tcopy, tscale, tsum, ttriad, n, nrep)
    
  end subroutine benchmark
  
  subroutine copy(a, b, n, t)

    implicit none

    double precision, intent(in)  :: b(:)
    integer, intent(in) :: n
    double precision, intent(out) :: a(:)
    real, intent(inout) :: t

    real :: t1, t2
    integer :: i

    call cpu_time(t1)
    do i = 1, n
       a(i) = b(i)
    enddo
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine copy

  subroutine scale(a, b, q, n, t)

    implicit none

    double precision, intent(in)  :: b(:)
    double precision, intent(in)  :: q
    integer, intent(in)  :: n
    double precision, intent(out) :: a(:)
    real, intent(inout) :: t
    real :: t1, t2
    integer :: i
    
    call cpu_time(t1)
    do i = 1, n
       a(i) = q * b(i)
    enddo
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine scale

  subroutine sum(a, b, c, n, t)

    implicit none

    double precision, intent(in)  :: b(:), c(:)
    integer, intent(in) :: n
    double precision, intent(out) :: a(:)
    real, intent(inout) :: t
    real :: t1, t2
    integer :: i

    call cpu_time(t1)
    do i = 1, n
       a(i) = b(i) + c(i)
    enddo
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine sum

  subroutine triad(a, b, c, q, n, t)

    implicit none

    double precision, intent(in)  :: b(:), c(:)
    double precision, intent(in)  :: q
    integer, intent(in) :: n
    double precision, intent(out) :: a(:)
    real, intent(inout) :: t
    real :: t1, t2
    integer :: i

    call cpu_time(t1)
    do i = 1, n
       a(i) = b(i) + q * c(i)
    enddo
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine triad

  subroutine init(a, b, c, n, q)

    implicit none

    double precision, intent(out) :: a(:), b(:), c(:)
    double precision, intent(out) :: q
    integer, intent(in) :: n
    integer :: i

    do i = 1, n
       a(i) = 0

       call random_number(b(i))
       call random_number(c(i))
    enddo
    call random_number(q)

  end subroutine init

  subroutine report(tcopy, tscale, tsum, ttriad, n, nrep)

    implicit none

    real, intent(in)    :: tcopy, tscale, tsum, ttriad
    integer, intent(in) :: n, nrep

    integer :: copyb = 16, copyf = 0
    integer :: scaleb = 16, scalef = 1
    integer :: sumb = 24, sumf = 1
    integer :: triadb = 24, triadf = 2

    copyb = n * copyb; copyf = n * copyf
    scaleb = n * scaleb; scalef = n * scalef
    sumb = n * sumb; sumf = n * sumf
    triadb = n * triadb; triadf = n * triadf
    
    print *, "Bandwidth [GB/s]:"
    print *, "-----------------------"
    print *, "COPY: ", copyb / (tcopy / nrep) / 1e9
    print *, "SCALE: ", scaleb / (tscale / nrep) / 1e9
    print *, "SUM: ", sumb / (tsum / nrep) / 1e9
    print *, "TRIAD: ", triadb / (ttriad / nrep) / 1e9
    print *, "======================="
    print *, "Compute [GFLOPs]:"
    print *, "-----------------------"
    print *, "COPY: ", copyf / (tcopy / nrep) / 1e9
    print *, "SCALE: ", scalef / (tscale / nrep) / 1e9
    print *, "SUM: ", sumf / (tsum / nrep) / 1e9
    print *, "TRIAD: ", triadf / (ttriad / nrep) / 1e9

  end subroutine report
  
end program
