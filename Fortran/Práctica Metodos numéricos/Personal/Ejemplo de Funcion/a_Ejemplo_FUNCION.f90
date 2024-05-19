program Prueba
    implicit none
    real*8 :: valor
    real*8,external :: facto
    valor=6.d0
    print *,facto(7)
    print*,'hola  \n peroo'
    print*,'pero'
    print*,10.d0**(-3)
end program Prueba

function facto(n) result(prod)
    implicit none
    real*8 prod
    integer i,n
    if (n==0) then
        prod=1.d0
    else
        prod=1.d0
        do i = 1, n
            prod=prod*i        
        end do
    end if
end function