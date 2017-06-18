!
! Create SVG images with Fortran
!
!   Charles O'Neill 12 May 2011
!    charles.oneill@gmail.com
!    www.caselab.okstate.edu
!   CO, 10/10/15: Added colors for circles with svgCircleColor
!
! Useage:
!    1) call svgOpen(Name, CanvasLowerLeft,CanvasUpperRight, Resolution)
!    2) call svgHeader()
!    3) Place Objects
!      i) call svgLine([x,y], [x,y])
!     ii) call svgRectangle( [x,y], [x,y], [x,y], [x,y])
!    iii) call svgCircle([x,y], [x,y])
!    iii) call svgTriangle([x,y], [x,y], [x,y])
!    4) call svgFooter()
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


module forSVG
  !use SolutionParameters
  implicit none
  integer,parameter :: svgID = 245

  integer,private :: Resolution
  real(8),private :: CanvasSize ! x=y
  real(8),private :: CanvasLowLeft(2),CanvasUpRight(2)
contains

  subroutine svgOpen(Name,CLL,CUP,Res)
    character(len=*) :: Name
    real(8) :: CLL(2), CUP(2)
    integer :: Res
    open(svgID, file=trim(Name)//".svg", status='unknown')
    write(*,*) "Opening", trim(Name)//".svg"
    CanvasLowLeft = CLL
    CanvasUpRight = CUP
    CanvasSize = max( CUP(1)-CLL(1) , CUP(2)-CLL(2))
    Resolution = Res
  end subroutine svgOpen

  function CanvasX(x)
    real(8) :: x
    real(8) :: CanvasX
    CanvasX = (x-CanvasLowLeft(1))/CanvasSize*Resolution
  end function CanvasX

  function CanvasY(y)
    real(8) :: y
    real(8) :: CanvasY
    CanvasY = (1.0-(y-CanvasLowLeft(2))/CanvasSize)*Resolution
  end function CanvasY

  function ScaleR(r)
    real(8) :: r
    real(8) :: ScaleR
    ScaleR = r/CanvasSize*Resolution
  end function ScaleR

  subroutine svgHeader()
    write(svgID,'(1X,A)') '<svg version="1.1" xmlns="http://www.w3.org/2000/svg"> '
  end subroutine

  ! subroutine svgRectangle(xA,xB,xC,xD)
  !   real(8) :: xA(2), xB(2), xC(2), xD(2)
  !   call svgLine(xA,xB)
  !   call svgLine(xB,xC)
  !   call svgLine(xC,xD)
  !   call svgLine(xD,xA)
  ! end subroutine


  ! subroutine svgTriangle(xA,xB,xC)
  !   real(8) :: xA(2), xB(2), xC(2)
  !   call svgLine(xA,xB)
  !   call svgLine(xB,xC)
  !   call svgLine(xC,xA)
  ! end subroutine

  subroutine svgLine(xA,xB,rgb)
    real(8) :: xA(2), xB(2)
    integer :: rgb(3)
    character(len=7) :: rgbHex
    character(len=80) :: sA1,sA2, sB1,sB2
    write(rgbHex,'( "#", Z2.2,Z2.2,Z2.2)'), rgb(1),rgb(2),rgb(3)
    write(sA1,'( e15.8 )') CanvasX(xA(1))
    write(sA2,'( e15.8 )') Canvasy(xA(2))
    write(sB1,'( e15.8 )') Canvasx(xB(1))
    write(sB2,'( e15.8 )') Canvasy(xB(2))
    write(svgID,'(1X,A)') '<line'
    write(svgID,'(1X,A,A,A)') 'x1="', trim(ADJUSTL(sa1)),'" '
    write(svgID,'(1X,A,A,A)') 'y1="', trim(ADJUSTL(sa2)),'" '
    write(svgID,'(1X,A,A,A)') 'x2="', trim(ADJUSTL(sb1)),'" '
    write(svgID,'(1X,A,A,A)') 'y2="', trim(ADJUSTL(sb2)),'" '
    write(svgID,'(1X,A,A,A,A)') 'style="stroke:',rgbHex, '" />'
  end subroutine

  subroutine svgCircle(x,r)
    real(8) :: x(2), r
    character(len=80) :: stringA,StringB,StringC
    write(stringA,'( e15.8 )') CanvasX(x(1))
    write(stringB,'( e15.8 )') CanvasY(x(2))
    write(stringC,'( e15.8 )') ScaleR(r)
    write(svgID,'(1X,A,A,A,A,A,A,A)') ' <circle cx="', trim(ADJUSTL(stringA)), &
                                      '" cy="', trim(ADJUSTL(stringB)), &
                                      '" r="', trim(ADJUSTL(stringC)), &
                                      '"'
    write(svgID,'(1X,A,A,A,A)') 'style="stroke:#000000;fill:#ff0000" />'
  end subroutine

  subroutine svgCircleColor(x,r,rgb)
    real(8) :: x(2), r
    integer :: rgb(3)
    character(len=7) :: rgbHex
    character(len=80) :: stringA,StringB,StringC
    write(stringA,'( e15.8 )') CanvasX(x(1))
    write(stringB,'( e15.8 )') CanvasY(x(2))
    write(stringC,'( e15.8 )') ScaleR(r)
    write(rgbHex,'( "#", Z2.2,Z2.2,Z2.2)'), rgb(1),rgb(2),rgb(3)
    write(svgID,'(1X,A,A,A,A,A,A,A)') ' <circle cx="', trim(ADJUSTL(stringA)), &
                                      '" cy="', trim(ADJUSTL(stringB)), &
                                      '" r="', trim(ADJUSTL(stringC)), &
                                      '"'
    write(svgID,'(1X,A,A,A,A,A)') 'style="stroke:', 'none', ';fill:', rgbHex,'; fill-opacity: 1.0;" />'
  end subroutine

  subroutine svgQuadColor(xa,xb,xc,xd,rgb,alpha)
    real(8) :: xa(2),xb(2),xc(2),xd(2), alpha
    integer :: rgb(3)
    character(len=7) :: rgbHex
    character(len=80) :: stringA,StringB,StringC
    write(rgbHex,'( "#", Z2.2,Z2.2,Z2.2)'), rgb(1),rgb(2),rgb(3)
    write(svgID,'(1X,A)') ' <polygon points="'
    write(stringA,'( e15.8 )') CanvasX(xa(1))
    write(stringB,'( e15.8 )') CanvasY(xa(2))
    write(svgID,'(1X,A,",",A)') trim(ADJUSTL(stringA)),trim(ADJUSTL(stringB))
    write(stringA,'( e15.8 )') CanvasX(xb(1))
    write(stringB,'( e15.8 )') CanvasY(xb(2))
    write(svgID,'(1X,A,",",A)') trim(ADJUSTL(stringA)),trim(ADJUSTL(stringB))
    write(stringA,'( e15.8 )') CanvasX(xc(1))
    write(stringB,'( e15.8 )') CanvasY(xc(2))
    write(svgID,'(1X,A,",",A)') trim(ADJUSTL(stringA)),trim(ADJUSTL(stringB))
    write(stringA,'( e15.8 )') CanvasX(xd(1))
    write(stringB,'( e15.8 )') CanvasY(xd(2))
    write(svgID,'(1X,A,",",A)') trim(ADJUSTL(stringA)),trim(ADJUSTL(stringB))
    write(svgID,'(1X,A)')  '"'
    write(stringB,'( e15.8 )') alpha
    write(svgID,'(1X,A,A,A,A,A,A)') 'style="stroke:', 'none', ';fill:', rgbHex,'; fill-opacity: ', stringB ,';" />'
  end subroutine

  subroutine svgText(x,s,val)
    real(8) :: x(2), s
    real(8) :: val
    character(len=80) :: text
    character(len=80) :: stringA,StringB,StringC
    write(stringA,'( e15.8 )') CanvasX(x(1))
    write(stringB,'( e15.8 )') CanvasY(x(2))
    write(stringC,'( e15.8 )') ScaleR(s)
    write(text,'( f5.1 )') val
    write(svgID,'(1X,A,A,A,A,A,A,A)') ' <text x="', trim(ADJUSTL(stringA)), &
                                      '" y="', trim(ADJUSTL(stringB)), &
                                      '" font-size="', trim(ADJUSTL(stringC)), &
                                      '"'
    write(svgID,'(1X,A,A,A,A)') 'font-family="Verdana" fill="gray" >'
    write(svgID,'(1X,A,1X,A)') text, "</text>"

  end subroutine

  subroutine svgTextT(x,s,text)
    real(8) :: x(2), s
    character(len=80) :: text
    character(len=80) :: stringA,StringB,StringC
    write(stringA,'( e15.8 )') CanvasX(x(1))
    write(stringB,'( e15.8 )') CanvasY(x(2))
    write(stringC,'( e15.8 )') ScaleR(s)
    write(svgID,'(1X,A,A,A,A,A,A,A)') ' <text x="', trim(ADJUSTL(stringA)), &
                                      '" y="', trim(ADJUSTL(stringB)), &
                                      '" font-size="', trim(ADJUSTL(stringC)), &
                                      '"'
    write(svgID,'(1X,A,A,A,A)') 'font-family="Verdana" fill="gray" >'
    write(svgID,'(1X,A,1X,A)') text, "</text>"

  end subroutine

  subroutine svgFooter()
    write(svgID,'(1X,A)') ' </svg>'
    close(svgID)
  end subroutine

end module
