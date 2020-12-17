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

  real, allocatable, dimension(:) :: a, b, c
  real :: q
  
end module vars
  
program stream

  use vars
  
  implicit none
  
  logical :: crayalloc = .false.
  integer :: n = 1000000
  integer :: nrep = 10
  integer :: r
  real    :: tcopy = 0, tscale = 0, tsum = 0, ttriad = 0
  
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

  !! Benchmark
  do r = 1, nrep
     call init(a, b, c, q)
     call copy(a, b, tcopy)
     call scale(a, b, q, tscale)
     call sum(a, b, c, tsum)
     call triad(a, b, c, q, ttriad)
  enddo

  !! Tidy
  if (.not.crayalloc) then
     call deallocate_f90()
  else
     !! Cray
  endif
  
  !! Report
  tcopy = tcopy / nrep
  tscale = tscale / nrep
  tsum = tsum / nrep
  ttriad = ttriad / nrep
  print *, "COPY: ", tcopy
  print *, "SCALE: ", tscale
  print *, "SUM: ", tsum
  print *, "TRIAD: ", ttriad

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
  
  subroutine copy(a, b, t)

    implicit none

    real, intent(in)    :: b(:)
    real, intent(out)   :: a(:)
    real, intent(inout) :: t
    real :: t1, t2

    call cpu_time(t1)
    a(:) = b(:)
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine copy

  subroutine scale(a, b, q, t)

    implicit none

    real, intent(in)    :: b(:)
    real, intent(in)    :: q
    real, intent(out)   :: a(:)
    real, intent(inout) :: t
    real :: t1, t2

    call cpu_time(t1)
    a(:) = q * b(:)
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine scale

  subroutine sum(a, b, c, t)

    implicit none

    real, intent(in)    :: b(:), c(:)
    real, intent(out)   :: a(:)
    real, intent(inout) :: t
    real :: t1, t2

    call cpu_time(t1)
    a(:) = b(:) + c(:)
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine sum

  subroutine triad(a, b, c, q, t)

    implicit none

    real, intent(in)    :: b(:), c(:)
    real, intent(in)    :: q
    real, intent(out)   :: a(:)
    real, intent(inout) :: t
    real :: t1, t2

    call cpu_time(t1)
    a(:) = b(:) + q * c(:)
    call cpu_time(t2)
    t = t + (t2 - t1)

  end subroutine triad

  subroutine init(a, b, c, q)

    implicit none

    real, intent(out) :: a(:), b(:), c(:)
    real, intent(out) :: q

    a(:) = 0

    call random_number(b)
    call random_number(c)
    call random_number(q)

  end subroutine init

end program
