function funcion(x) result(valor)!Esta funcion es la funcion que nos piden integrar
    implicit none
    real*8 :: x,valor
    valor=1.d0/(1.d0+x)
end function

program Practica_2_2
    implicit none
    real*8 a,b,h,suma,integral,stop
    real*8 , external::funcion
    integer n,k
    !Declaramos las variables que tomaran valores
    a=0.d0
    b=1.d0
    print*,"Valor de n: "
    read(*,*)n
    h=(b-a)/n
    !Aplicamos la formula del calculo integral
    suma=0.d0

    do k=1,(n-1)
        suma=suma+funcion(a+k*h)
    end do 
    
    integral=h/2*(1.d0/(1.d0+a)+1.d0/(1.d0+b)+2*suma)

    print *,"La integral es: ",integral
    read(*,*) stop
end program Practica_2_2
