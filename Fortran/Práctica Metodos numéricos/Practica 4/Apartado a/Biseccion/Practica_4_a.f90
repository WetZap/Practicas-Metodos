function funcion(x) result(valor)!Funcion que toma el valor de la función deseada.
    implicit none
    real*8 x,valor
    valor=x+exp(x)
end function

program Practica_4_Biseccion
    implicit none
    real*8,external::funcion
    real*8 a,b,valor,error,c,difere,f_a,f_b,c_anterior,i!Definimos las variables
    !Definimos los valores de los limites del intervalo.
    a=-1.d0
    b=0.d0
    if ( funcion(a)*funcion(b)<0 ) then!Comprobamos que la funcion tiene un cero.
        !Definimos el error que consideraremos.
        i=0.d0
        error=10.d0**(-3)
        difere=1.d0
        !Definimos el valor de c como la SUMA de los intervalos entre 2.
        c=(b+a)/(2.d0)
        c_anterior=c
        !Comenzamos el bucle que se encargara de que llegar al error deseado.
        do while(difere>=error)
            if (funcion(c)*funcion(a)>0) then!Esta sería la primera condicion del método.
                a=c!Cambiamos el valor de a por el de c ya que nos interesa estudiar el intervalo [c,b]
                c=(b+a)/(2.d0)!Volvemos a definir el valor de c y comprobamos la diferencia de este con el anterior.
                difere=abs((c-c_anterior))
                c_anterior=c
            else if(funcion(c)*funcion(a)<0)then
                b=c!Lo mismo que arriba
                c=(b+a)/(2.d0)
                difere=abs(c-c_anterior)!
                c_anterior=c  
            end if
            if(funcion(c)*funcion(a)==0) exit !Cuando sea 0 se marcha.
            i=i+1
        end do
        !Imprimimos el valor del sitio del 0.
        print*,"El valor ",c,"es un cero de la función."
        print*,'Se han realizado ',i,' iteraciones para encontrar el cero.'
    else
        print*,'La funcion no pasa de postivo a negativo en el intervalo escogido.'
    end if

end program Practica_4_Biseccion