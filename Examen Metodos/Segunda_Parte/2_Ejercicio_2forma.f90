function funcion(x) result(valor)
    implicit none
    real*8 :: x,valor
    valor=x/((1+x**2)**3)
end function

program Ejer_2_Sim!El máximo valor que puede alcanzar n es algo mayor que 1000000000, pero, comienza a tardar una mayor cantidad de tiempo.
    implicit none
    real*8 a,b,h,suma_1,suma_2,integral,stop
    real*8,external::funcion!Esto sirve para que el programa entienda las funciones
    integer k,n
    !Inicializo los limites
    a=0.d0
    b=1.d0
    !Pregunto el valor de n
    !print*,"Valor n:"
    !read(*,*)n
    n=99999999
    !Defino h
    h=(b-a)/n
    !Inicializo las sumas 0.d0 para que no me den ningún tipo de problema.
    suma_1=0.d0
    suma_2=0.d0!In
    !Hacemos las dos sumas que nos dicen de acuerdo a las encontradas en un libro de formulas matemáticas 
    !Utilizo esta formula porque la otra me daba problemas y me estaba poniendo un poco nervioso de que no me saliese.
    do k=2,(n/2)!Esta formula comienza en 2 y acaba en n/2
        suma_1=suma_1+funcion(a+((2*k-2)*h))
    end do  

    do k=1,(n/2)
        suma_2=suma_2+funcion(a+((2*k-1)*h))
    end do

    integral=h/3*( funcion(a) + funcion(b) + 2*suma_1 + 4*suma_2)
    print*,"Para una n=",n,":"
    print *,"La integral es: ",integral
    print*,"La diferencia es: ",0.187500000-integral
    !read(*,*)stop !Este comando sirve para detener el ejecutable en caso de que al terminer se cierre.
  
end program Ejer_2_Sim