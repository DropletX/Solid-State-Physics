program main
  implicit none
  real(8), parameter :: PI=4.D0*datan(1.D0)
  real(8),dimension(3) :: a1,a2,a3,b1,b2,b3
  real(8) :: Va

  print *,"Enter x, y, z coordinates of a1:"
  read(*,*)a1(1),a1(2),a1(3)
  print *,"Enter x, y, z coordinates of a2:"
  read(*,*)a2(1),a2(2),a2(3)
  print *,"Enter x, y, z coordinates of a3:"
  read(*,*)a3(1),a3(2),a3(3)
  Va = dot_product(a1,cross_product(a2,a3))
  b1 = cross_product(a2,a3)*2*PI/Va
  b2 = cross_product(a3,a1)*2*PI/Va
  b3 = cross_product(a1,a2)*2*PI/Va
  print *,"b1(1)=",b1(1),"b1(2)=",b1(2),"b1(3)=",b1(3)
  print *,"b2(1)=",b2(1),"b2(2)=",b2(2),"b2(3)=",b2(3)
  print *,"b3(1)=",b3(1),"b3(2)=",b2(2),"b3(3)=",b2(3)
  print *,"Volume of Unit Cell=",Va
contains
  function cross_product(x,y)
    implicit none
    real(8),dimension(3) :: x, y, z, cross_product
    z(1)=x(2)*y(3)-x(3)*y(2)
    z(2)=x(3)*y(1)-x(1)*y(3)
    z(3)=x(1)*y(2)-x(2)*y(1)
    cross_product=z
  end function cross_product

  real(8) function dot_product(x,y)
    implicit none
    real(8),dimension(3) :: x,y
    dot_product = x(1)*y(1)+x(2)*y(2)+x(3)*y(3)
  end function dot_product
end program main
