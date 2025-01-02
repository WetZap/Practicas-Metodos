function funcion(x) result(valor)!Funcion que toma el valor de la funcion.
    implicit none
    real*8 x,valor
    valor=x+exp(x)
end function

program Practica_4_Regula
    implicit none
    real*8,external::funcion
    real*8 a,b,valor,error,c,difere,f_a,f_b,c_1
    integer iteracion
    !Definimos los valores de los limites del intervalo.
    a=-1.d0
    b=0.d0
    !Definimos el error.
    error=10.d0**(-3)
    difere=1.d0
    !Tomamos el valor de c como la formula descrita en clase.
    c=(a*funcion(b)-b*funcion(a))/(funcion(b)-funcion(a))
    c_1=c
    iteracion=0
    do while(difere>=error)!Comenzamos nuestro bucle.
        !Hacemos lo mismo que hicimos en Biseccion.
        if (funcion(c)*funcion(a)>0) then
            !Cambiamos el valor de a por el de c ya que nos interesa estudiar el intervalo [c,b]
            a=c
            !Volvemos a definir el valor de c y comprobamos la diferencia 
            !de este con el anterior.
            c=(a*funcion(b)-b*funcion(a))/(funcion(b)-funcion(a))
            difere=abs(abs((c))-abs((c_1)))
        else if(funcion(c)*funcion(a)<0)then
            b=c!lo mismo pero con el intervalo [a,c]
            c=(a*funcion(b)-b*funcion(a))/(funcion(b)-funcion(a))
            difere=abs(abs((c))-abs((c_1)))
        end if
        if(funcion(c)*funcion(a)==0) exit !Si encontramos el cero nos vamos.
        c_1=c
        iteracion=iteracion+1!Contamos el numero de iteraciones.
    end do
    !Imprimimos el resultado.
    print*,"El valor ",c,"es un cero de la función."
    print*,"El numero de iteraciones es: ",iteracion,"."
end program Practica_4_Regula