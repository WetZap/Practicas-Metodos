function funcion(x) result(suma)
    implicit none
    real*8 x(0:3),suma
    integer i
    do i = 0, 3
        suma=suma+x(i)
    end do
end function
program H
    implicit none
    real*8 x(0:3),valor
    real*8,external::funcion
    integer i
    do i = 0, 3
        x(i)=(i+1)*2
    end do
    valor=funcion(x)
    print*,valor
end program H