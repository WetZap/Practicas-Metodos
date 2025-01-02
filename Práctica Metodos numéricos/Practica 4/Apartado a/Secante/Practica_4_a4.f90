function funcion(x) result(valor)!Funcion.
    implicit none
    real*8 x,valor
    valor=x+exp(x)
end function


program Practica_4_Secante
    implicit none
    real*8,external::funcion
    real*8 a,b,error,difere,x,x_0,x_1
    integer iteracion
    !Definimos los valores de los limites del intervalo.
    a=-1.d0
    b=0.d0
    !Definimos el error.
    error=10.d0**(-3)
    difere=1.d0
    !Tomamos dos valores inicial para la x
    x_0=0.7d0
    x_1=3.d0
    iteracion=0
    do while(difere>=error)!Comenzamos nuestro bucle
        !Tomamos el valor de x como el anterior menos una expresion que tiene
        x=x_0-((funcion(x_0)*(x_0-x_1))/(funcion(x_0)-funcion(x_1)))
        difere=abs(abs(x)-abs(x_1))!que ver con la derivada.
        x_1=x
        iteracion=iteracion+1!Contamos las iteraciones que se hacen
    end do
    !Imprimimos el valor por pantalla.
    print*,"El valor ",x,"es un cero de la función."
    print*,"El numero de iteraciones es: ",iteracion,"."

end program Practica_4_Secante