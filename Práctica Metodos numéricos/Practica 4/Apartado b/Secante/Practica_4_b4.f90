function funcion(x) result(valor)!Funcion que toma el valor de la función deseada.
    implicit none
    real*8 x,valor
    valor= x**3- 5*x**2 + 7*x - 3
end function

program Practica_4_Secante
    implicit none
    real*8,external::funcion
    real*8 a,b,error,difere,x,x_1,x__1
    !Definimos los valores de los limites del intervalo.
    a=0.d0
    b=5.d0
    !Definimos el error.
    error=10.d0**(-3)
    difere=1.d0
    !Tomamos dos valor inicial para la x
    x=11.d0
    x__1=5.d0
    x_1=x
    do while(difere>=error)!Comenzamos nuestro bucle
        !Tomamos el valor de x como el anterior menos una expresion que tiene
        !que ver con la derivada.
        x=x-((funcion(x)*(x-x__1))/(funcion(x)-funcion(x__1)))
        difere=abs(abs(x)-abs(funcion(x_1)))
        x_1=x
    end do
    !Imprimimos el valor por pantalla.
    print*,"El valor ",x,"es un cero de la función."
end program Practica_4_Secante