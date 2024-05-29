function funcion(x) result(valor)!Esta función se encarga de guardar el valor de la funcion a encontrar cero.
    implicit none
    real*8 x,valor
    valor=x+exp(x)
end function

function funcion_prima(x) result(valor)!Esta función se encarga de guardar el valor de la funcion prima a encontrar cero.
    implicit none
    real*8 x,valor
    valor=1+exp(x)
end function
program Prueba
    implicit none
    real*8,external::funcion,funcion_prima
    real*8 a,b,error,difere,x,x_1
    integer iteracion,j
    !Definimos los valores de los limites del intervalo.
    do j = 1, 100
        a=-1.d0
        b=0.d0
        !Definimos el error.
        error=10.d0**(-3)
        difere=1.d0
        !Tomamos un valor inicial para la x
        x=-1+((j*1.d0)/100)
        print*, x
        x_1=x
        iteracion=0
        do while(difere>=error)!Comenzamos el bucle.
            x=x-(funcion(x)/funcion_prima(x))!Realizamos el paso correspondiente.
            difere=abs(abs((x))-abs((x_1)))!Calculamos la diferencia con el valor anterior.
            x_1=x!Guardamos el valor anterior en una variable.
            iteracion=iteracion+1!Contamos el numero de iteraciones.
        end do
        !Imprimimos el resultado por pantalla.
        print*,"El valor ",x,"es un cero de la función."
        print*,"El numero de iteraciones es: ",iteracion,"."    
    end do

end program Prueba