module Utilities
  implicit none
contains

  character(len=80) function str(k)
    integer, intent(in) :: k
    write(str,*)k
    str = adjustl(str)
  end function

  subroutine writematrix(M)
  real(8) :: M(:,:)
  integer :: i,j
  do i=1,size(M,1)
    do j=1,size(M,2)
      write(*,'( f11.4)',advance="no") M(i,j)
    enddo
    write(*,*)
  enddo
end subroutine
subroutine writeVec(M)
  real(8) :: M(:)
  integer :: i
  do i=1,size(M,1)
      write(*,'( f11.4)') M(i)
  enddo
end subroutine
end module
