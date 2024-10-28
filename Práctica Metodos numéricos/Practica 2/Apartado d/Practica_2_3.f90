function funcion(x) result(valor)
    implicit none
    real*8 :: x,valor
    valor=1.d0/(1.d0+x)
end function

program Practica_2_3
    implicit none
    real*8 a,b,h,suma_1,suma_2,integral,stop
    real*8,external::funcion!Esto sirve para que el programa entienda las funciones
    integer n,k
    !Inicializo los limites
    a=0.d0
    b=1.d0
    !Pregunto el valor de n
    print*,"Valor n:"
    read(*,*)n
    !Defino h
    h=(b-a)/n
    suma_1=0.d0
    !Hacemos las dos sumas que nos dicen de acuerdo a las encontradas en un libro de que se encuentra en
    !https://images.app.goo.gl/mdy7it3dnyQ87srN8
    do k=2,(n/2)
        suma_1=suma_1+funcion(a+((2*k-2)*h))
    end do  

    suma_2=0.d0
    do k=1,(n/2)
        suma_2=suma_2+funcion(a+((2*k-1)*h))
    end do

    integral=h/3*( funcion(a) + funcion(b) + 2*suma_1 + 4*suma_2)
    print *,"La integral es: ",integral
    read(*,*) stop
    
end program Practica_2_3
