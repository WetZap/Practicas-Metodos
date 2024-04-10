program Prueba
    implicit none
    real*8 :: valor
    real*8,external :: facto
    valor=6.d0
    print *,facto(7.d0)
end program Prueba

function facto(x) result(prod)
implicit none
real*8 :: x,prod
integer i
prod=1.d0
do i=1,int(x)
    prod=prod*i
end do

end function