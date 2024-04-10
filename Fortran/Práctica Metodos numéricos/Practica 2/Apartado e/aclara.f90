function funcion(x) result(valor)
    implicit none
    real*8 :: x,valor
    valor=1.d0/(1.d0+x)
end function

program Aclara
    implicit none
    real*8 x(0:19),h,suma,a,b
    real*8,external::funcion
    integer i,j,n
    a=0.d0
    b=1.d0
    n=8
    h=b-a
    x(0)=h/2*(funcion(a)+funcion(b))
    do i=1,n
        h=h/2
        suma=0.d0
        do j = 1, 2**(i-1)
            suma=suma+funcion(a+(2*j-1)*h)
        end do
        x(i)=x(i-1)/2+h*suma
    end do
    print*,"El valor de la integral es: ",x(n)
end program Aclara
