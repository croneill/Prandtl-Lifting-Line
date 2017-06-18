! Gauss Jordan Linear Equation Solver
!  Solve Ax-B=0 for x
!
! Copyright (c) 2011 Charles O'Neill
!
! Permission is hereby granted, free of charge, to any person
! obtaining a copy of this software and associated documentation
! files (the "Software"), to deal in the Software without
! restriction, including without limitation the rights to use,
! copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the
! Software is furnished to do so, subject to the following
! conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
! HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
! WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
! OTHER DEALINGS IN THE SOFTWARE.

module GaussElimination
  !use Precisions
  integer,parameter :: WP = 8
  !use Utilities
contains
  subroutine GaussElim(Ain,Bin,x,n)
    implicit none
    integer,intent(in) :: n
    real(WP),intent(in) :: Ain(n,n),Bin(n)
    real(WP),intent(inout) :: x(n)
    ! Gauss elimination
    real(WP) :: A(n,n), B(n)
    real(WP) :: BackSum
    real(WP) :: PivotValue, PivotRatio
    integer :: i,ii, j

    A = Ain
    B = Bin

    do i = 1,(n-1)
        PivotValue = A(i,i)
        do ii = (i+1),n
          PivotRatio = A(i,i)/A(ii,i)
          do j=1,n
            A(ii,j) = A(ii,j) * PivotRatio - A(i,j)
          enddo
          B(ii) = B(ii) * PivotRatio - B(i)
        enddo
    enddo

    ! Backsubstitute
    do i = n,1,-1
      BackSum = 0
      do j= (i+1),n
        BackSum = BackSum + A(i,j)*x(j)
      enddo
      x(i) = ( B(i) - BackSum) /A(i,i)
    enddo

  end subroutine

  subroutine testGaussElimination

    real(WP) :: A(3,3)
    real(WP) :: B(3)
    real(WP) :: x(3)
    integer :: n

    A = reshape([2,1,1,1,2,1,1,1,2],[3,3])
    B = [1,1,1]
    x = 0
    n = 3

    call GaussElim(A,B,x,n)

    !call print(x,'x')

  end subroutine

end module
