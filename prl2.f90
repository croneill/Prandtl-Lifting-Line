program prl2
  use forSVG
  use GaussElimination
  use Utilities
  implicit none

  real(8) :: AR,b,S
  real(8),allocatable :: spanY(:), chordY(:), twistY(:)
  real(8),allocatable :: spanTh(:), chordTh(:), twistTh(:)
  real(8),allocatable :: Gamm(:), dGamm(:), Cl(:), yPlot(:)
  integer :: N, Nplot, Nsol
  integer :: i,j
  real(8) :: v
  real(8),allocatable  :: a(:,:), r(:), An(:)

  real(8) :: CanvasLowerLeft(2),CanvasUpperRight(2)
  integer :: Resolution

  real(8),parameter :: Cla = 2.0d0*3.141592
  real(8) :: theta, yloc, chord, twist
  real(8) :: alfa
  character(len=80) :: text,filename

  write(*,*) "Prandtl Lifting Line v3 11/11/16"
  b = 2.0d0 ! span from -1 to 1

  ! SVG
  CanvasLowerLeft = [-2,-2]
  CanvasUpperRight = [2,2]
  Resolution = 1000
  call svgOpen("Geo", CanvasLowerLeft,CanvasUpperRight, Resolution)
  call svgHeader()

  Nplot = 400
  allocate(Gamm(Nplot))
  allocate(dGamm(Nplot))
  allocate(Cl(Nplot))
  allocate(yPlot(Nplot))

  ! geometery
  write(*,*) "Geometry Filename"
  read*, filename
  open(unit=101,file=trim(filename),status="old")

  read(101,*) ! Header Line
  read(101,*) ! Comment Line
  read(101,*) N

  allocate(spanY(N))
  allocate(chordY(N))
  allocate(twistY(N))

  read(101,*) ! Comment Line
  do i=1,N
    read(101,*) spanY(i), chordY(i), twistY(i)
    twistY(i) = twistY(i)*3.1415926/180.0d0
  enddo

  do i=1,N-1
    call svgQuadColor([spanY(i),0.25*chordY(i)],&
                      [spanY(i+1),0.25*chordY(i+1)],&
                      [spanY(i+1),-0.75*chordY(i+1)],&
                      [spanY(i),-0.75*chordY(i)],[150,150,150],1.0d0)
    call svgLine([spanY(i),-twistY(i)],[spanY(i+1),-twistY(i+1)],[0,0,0])
  enddo
  do i=-10,10,2
    call svgLine([1.0d0,i/57.30d0],[1.1d0,i/57.3d0],[0,0,0])
    call svgText([1.2d0,i/57.3d0],0.03d0,-i*1.0d0)
  enddo
  do i=1,N
        call svgLine([spanY(i),0.25*chordY(i)],[spanY(i),-0.75*chordY(i)],[255,255,255])
  enddo

  ! COmpute geometry
  S = 0
  do i=1,N-1
    S = S + 0.5*(chordY(i)+chordY(i+1))*(spanY(i+1)-spanY(i))
  enddo
  AR = b*b/S
  write(*,*) "Area = ", S, "AR =", AR

  ! Solution
  Nsol = 50
  allocate( a(Nsol,Nsol))
  allocate( r(Nsol))
  allocate( An(Nsol))

  ! Polar FIle
  open(unit=109,file="polar.txt",status='unknown')
  write(109,*) "Area = ", S, "AR =", AR
  write(109,*) "CL                 CD"
  
  do while(.true.)
  write(*,*) "Alpha [deg]"
  read*, alfa
  if(alfa.eq.-999) exit
  alfa = alfa/57.3


  do i=1,Nsol
    theta = gettheta(i,Nsol)
    yloc = Th2Y(b/2.0d0,theta)
    chord = interpolate(spanY,chordY,yloc)
    twist = interpolate(spanY,twistY,yloc)
    do j=1,Nsol
      a(i,j) = aic(b,theta,chord,Cla,j)
    enddo
    r(i)=raic(alfa,b,theta,chord,twist,Cla,Nsol)
  enddo

  write(*,*) "Solving...."
  call GaussElim(a,r,An,Nsol)

  open(unit=301,file="Coeffs.txt",status='unknown')
  write(301,'(f15.5)') An
  close(301)

  write(*,*) "Values...."
  write(*,*) "CL = ", getCL(AR,An)
  write(*,*) "CDi = ", getCD(AR,An)
  write(*,*) "Delta = ", getDelta(AR,An)
  write(109,*) getCL(AR,An), getCD(AR,An)

  write(*,*) "Plotting...."


  do i=1,Nplot
    yloc = -1 + b*(i-1.0d0)/(Nplot-1.0d0)
    chord = interpolate(spanY,chordY,yloc)
    yPlot(i) = yloc
    Gamm(i) = GammaCirc(An,b,Nsol,yloc)
    dGamm(i) = dGammaCirc(An,b,Nsol,yloc)
    Cl(i) = 2*Gamm(i)/chord
  enddo

  write(*,*) "SVG...."

  ! Gamma
  do i=1,Nplot-1
    call svgQuadColor([yPlot(i),-0.0d0],&
                      [yPlot(i+1),0.0d0],&
                      [yPlot(i+1),Gamm(i+1)/maxval(Gamm)],&
                      [yPlot(i),Gamm(i)/maxval(Gamm)],[0,255,0],0.1d0)
    call svgLine([yPlot(i),Gamm(i)/maxval(Gamm)],[yPlot(i+1),Gamm(i+1)/maxval(Gamm)],[0,255,0])
  enddo

  ! Cl Plotting
  do i=1,Nplot-1
    call svgLine([yPlot(i),Cl(i)],[yPlot(i+1),Cl(i+1)],[0,0,255])
  enddo
  text = "Cl"
  call svgTextT([-1.3d0,0.0d0],0.05d0,text)
  write(text,'( "AOA=", f5.1, " CL=", f6.3, " CD= ", f6.3)') alfa*57.3, getCL(AR,An), getCD(AR,An)
  call svgTextT([0.0d0,Cl(Nplot/2)+0.02],0.05d0,text)
  do i=-10,20
    call svgLine([-1.0d0,i/10.0d0],[-1.1d0,i/10.0d0],[0,0,0])
    call svgText([-1.2d0,i/10.0d0],0.05d0,i/10.0d0)
  enddo

  open(unit=200,file="output.txt",status='unknown')
  write(200,*) "#### ", filename, alfa, "CL ", getCL(AR,An), "CD ", getCD(AR,An)
  do i=1,Nplot-1
    write(200,*) yPlot(i), Gamm(i), Cl(i)
  enddo


  ! ! Trailing Vortex Sheet
  ! do i=2,Nplot-2
  !   call svgLine([yPlot(i),(dGamm(i))/maxval(dGamm)],&
  !               [yPlot(i+1),(dGamm(i+1))/maxval(dGamm)],[255,0,0])
  !   !call svgQuadColor([yPlot(i),-0.2d0],&
  !   !                  [yPlot(i+1),-0.2d0],&
  !   !                  [yPlot(i+1),-2.0d0],&
  !   !                  [yPlot(i),-2.0d0],[255,0,0],abs(dGamm(i))/maxval(dGamm))
  ! enddo
enddo
  call svgFooter()
contains

  function getCL(AR,An)
    real(8) :: getCL, AR
    real(8),allocatable :: An(:)
    getCL = 3.14159*AR*An(1)
  end function

  function getCD(AR,An)
    real(8) :: getCD, AR
    real(8),allocatable :: An(:)
    integer :: n
    n = size(An)
    getCD = 0.0
    do i=1,n
      getCD= getCD + 3.14159*AR*i*An(i)**2
    enddo
  end function

  function getDelta(AR,An)
    real(8) :: getDelta, AR
    real(8),allocatable :: An(:)
    integer :: n
    n = size(An)
    getDelta = 0.0
    do i=2,n
      getDelta= getDelta + i*(An(i)/An(1))**2
    enddo
  end function
  
  function GammaCirc(An,b,n,y)
    implicit none
    real(8) :: b,y
    real(8),allocatable :: An(:)
    integer :: n, i
    real(8) :: GammaCirc
    GammaCirc = 0
    do i=1,n
      GammaCirc = GammaCirc + 2*b*An(i)*sin(i*acos(2*y/b))
    enddo
  end function

  function dGammaCirc(An,b,n,y)
    implicit none
    real(8) :: b,y
    real(8),allocatable :: An(:)
    integer :: n, i
    real(8) :: dGammaCirc
    real(8),parameter :: deltay = 0.0001
    dGammaCirc =  -(GammaCirc(An,b,n,y+deltay)- GammaCirc(An,b,n,y-deltay))/deltay/2.0d0

  end function

  function gettheta(m,N)
    real(8) :: gettheta
    integer :: m,N
    gettheta = 3.1415926*m/(N+1.0d0)
  end function

  function aic(b,theta,chord,Cla,n)
    real(8) :: theta,chord,CLa,b
    integer :: n
    real(8) :: aic
    aic = sin(n*theta)+chord/4.0/b*Cla*n*sin(n*theta)/sin(theta)
  end function

  function raic(alfa,b,theta,chord,twist,Cla,n)
    real(8) :: theta,chord,CLa,twist,alfa,b
    integer :: n
    real(8) :: raic
    raic = chord/4.0/b*Cla*(alfa+twist)
  end function


  function Th2Y(halfspan,theta)
    real(8) :: Th2Y,theta,halfspan
    Th2Y = halfspan*cos(theta)
  end function

  function interpolate(x,y,x0)
    real(8) :: interpolate
    real(8) :: x(:)
    real(8) :: y(:)
    real(8) :: x0,deltax, deltay
    integer :: i,n
    ! Sizes
    n = size(x)

    if(x0 .LT. x(1))then
      stop "Interpolation Failed. Too small"
    elseif(x0 .GT. x(n))then
      stop "Interpolation Failed. Too LARGE"
    endif
    do i=1,n
      if( x0 < x(i+1) ) exit
    enddo

    ! interpolate
    deltax = x(i+1)-x(i)
    deltay = y(i+1)-y(i)

    interpolate = y(i)+deltay/deltax*(x0-x(i))
  end function

end program
