function funcion(x) result(valor)!Esta funcion es la funcion que nos piden integrar,( lo que hacemos es que la cambiamos para poner
    implicit none                !la funcion que nos piden en cada caso.
    real*8 :: x,valor
    valor=x/((1+x**2)**3)
end function

program Ejer_2!El programa puede tomar un n maximo de 999999999,con este n nos saldría que el resultado es 0.18749999999941425
    implicit none!Esta integral está hecha mediante el trapecio compuesto
    real*8 a,b,h,suma,integral,stop!Declaro las variables.
    real*8 , external::funcion!Esto sirve para que el programa entienda las funciones
    integer n,k
    !Declaramos las variables que tomaran valores
    a=0.d0
    b=1.d0
    !print*,"Valor de n: "
    !read(*,*)n
    n=99999998!A partir de este n  ya empieza a aumentar el tiempo de ejecucion.
    h=(b-a)/n
    !Aplicamos la formula del calculo integral
    suma=0.d0
    do k=1,(n-1)!Llegariamos hasta 
        suma=suma+funcion(a+k*h)!Es a + k*h, porque kh es el incremento del intervalo
    end do 
    
    integral=h/2*(funcion(a)+funcion(b)+2*suma)
    print*,"Para una n=",n,":"
    print *,"La integral es: ",integral
    print*,"La diferencia es: ",0.187500000-integral
    read(*,*)stop !Este comando sirve para detener el ejecutable en caso de que al terminer se cierre.
end program Ejer_2