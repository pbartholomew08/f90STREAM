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
  double precision :: q
  
end module vars
  
program stream

  use vars
  
  implicit none
  
  logical :: crayalloc = .false.
  integer :: n = 1000000
  integer :: nrep = 10
  
  !! Input:
  !! - problem size
  !! - allocator (Fortran90 or Cray pointers/C-malloc?)
  !! - repetitions (default 10)
  
  !! Allocate
  if (.not.crayalloc) then
     call allocate_f90(n)
  else
     !! Cray
  endif

  call benchmark(a, b, c, q, nrep)
  
  !! Tidy
  if (.not.crayalloc) then
     call deallocate_f90()
  else
     !! Cray
  endif
  
contains
  
  subroutine allocate_f90(n)

    use vars

    implicit none

    integer, intent(in) :: n

    allocate(a(n), b(n), c(n))

  end subroutine allocate_f90

  subroutine deallocate_f90()

    use vars

    implicit none

    deallocate(a, b, c)

  end subroutine deallocate_f90

  subroutine benchmark(a, b, c, q, nrep)

    implicit none

    double precision, intent(inout) :: a(:), b(:), c(:)
    double precision, intent(inout) :: q
    integer, intent(in) :: nrep

    integer :: n
    real    :: tcopy = 0, tscale = 0, tsum = 0, ttriad = 0
    integer :: r
    
    do r = 1, nrep
       call init(a, b, c, q)
       call copy(a, b, tcopy)
       call scale(a, b, q, tscale)
       call sum(a, b, c, tsum)
       call triad(a, b, c, q, ttriad)
    enddo

    n = size(a)
    call report(tcopy, tscale, tsum, ttriad, n, nrep)
    
  end subroutine benchmark
  
  subroutine copy(a, b, t)

    implicit none

    double precision, intent(in)  :: b(:)
    double precision, intent(out) :: a(:)
    real, intent(inout) :: t
    real :: t1, t2

    call cpu_time(t1)
    a(:) = b(:)
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine copy

  subroutine scale(a, b, q, t)

    implicit none

    double precision, intent(in)  :: b(:)
    double precision, intent(in)  :: q
    double precision, intent(out) :: a(:)
    real, intent(inout) :: t
    real :: t1, t2

    call cpu_time(t1)
    a(:) = q * b(:)
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine scale

  subroutine sum(a, b, c, t)

    implicit none

    double precision, intent(in)  :: b(:), c(:)
    double precision, intent(out) :: a(:)
    real, intent(inout) :: t
    real :: t1, t2

    call cpu_time(t1)
    a(:) = b(:) + c(:)
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine sum

  subroutine triad(a, b, c, q, t)

    implicit none

    double precision, intent(in)  :: b(:), c(:)
    double precision, intent(in)  :: q
    double precision, intent(out) :: a(:)
    real, intent(inout) :: t
    real :: t1, t2

    call cpu_time(t1)
    a(:) = b(:) + q * c(:)
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine triad

  subroutine init(a, b, c, q)

    implicit none

    double precision, intent(out) :: a(:), b(:), c(:)
    double precision, intent(out) :: q

    a(:) = 0

    call random_number(b)
    call random_number(c)
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
