function funcion(x) result(valor)!Funcion que toma el valor de la función deseada.
    implicit none
    real*8 x,valor
    valor=x**3-0.5d0*x**2+2*x-1
end function
function funcion_prima(x) result(valor)!Esta función se encarga de guardar el valor de la funcion prima a encontrar cero.
    implicit none
    real*8 x,valor
    valor=3*x**2-x+2
end function



program Programa_EXAMEN_2
    implicit none
    real*8,external::funcion,funcion_prima
    real*8 a,b,valor,error,c_bisec,difere,f_a,f_b,c_anterior,x,x_1,c_regu,x_0s,xs,x_1s,c_1,x_1si!Definimos las variables
    integer iteracion_bisect,iteracion_newt,iteracion_reg,iteracion_sec

    !COMENZAMOS HACIENDOLO POR BISECCION
    !Definimos los valores de los limites del intervalo.
    a=0.d0
    b=1.d0
    !Definimos el error que consideraremos.
    error=10.d0**(-3)
    difere=1.d0
    !Definimos el valor de c como la SUMA de los intervalos entre 2.
    c_bisec=(b+a)/(2.d0)
    c_anterior=c_bisec
    !Iniciamos el valor de la iteracion a 0
    iteracion_bisect=0
    !Comenzamos el bucle que se encargara de que llegar al error deseado.
    do while(difere>=error)
        iteracion_bisect=iteracion_bisect+1
        if (funcion(c_bisec)*funcion(a)>0) then!Esta sería la primera condicion del método.
            a=c_bisec!Cambiamos el valor de a por el de c ya que nos interesa estudiar el intervalo [c,b]
            c_bisec=(b+a)/(2.d0)!Volvemos a definir el valor de c y comprobamos la diferencia de este con el anterior.
            difere=abs((c_bisec-c_anterior))
            c_anterior=c_bisec
        else if(funcion(c_bisec)*funcion(a)<0)then
            b=c_bisec!Lo mismo que arriba
            c_bisec=(b+a)/(2.d0)
            difere=abs(abs(c_bisec)-abs(c_anterior))!
            c_anterior=c_bisec  
        end if
        if(funcion(c_bisec)*funcion(a)==0) exit !Cuando sea 0 se marcha.
     end do

     !AHORA LO RESOLVEREMOS MEDIANTE EL MÉTODO DE NEWTON RAPHSON

     a=0.d0
     b=1.d0
     !Redefinimos la variable difiere.
     difere=1.d0
     !Tomamos un valor inicial para la x
     x=1.d0
     x_1=x
     !Iniciamos el valor de la iteracion a 0
     iteracion_newt=0
     do while(difere>=error)!Comenzamos el bucle.
         x=x-(funcion(x)/funcion_prima(x))!Realizamos el paso correspondiente.
         difere=abs(abs((x))-abs((x_1)))!Calculamos la diferencia con el valor anterior.
         x_1=x!Guardamos el valor anterior en una variable.
         iteracion_newt=iteracion_newt+1!Contamos el numero de iteraciones.
     end do
     
    !AHORA LO RESOLVEREMOS MEDIANTE EL MÉTODO DE REGULA FALSI
    a=0.d0
    b=1.d0
    !Redefinimos la variable difiere.
    difere=1.d0
    !Tomamos el valor de c como la formula descrita en clase.
    c_regu=(a*funcion(b)-b*funcion(a))/(funcion(b)-funcion(a))
    c_1=c_regu
    !Iniciamos el valor de la iteracion a 0
    iteracion_reg=0
    do while(difere>=error)!Comenzamos nuestro bucle.
        if (funcion(c_regu)*funcion(a)>0) then!Hacemos lo mismo que hicimos en Biseccion.
            a=c_regu!Cambiamos el valor de a por el de c ya que nos interesa estudiar el intervalo [c,b]
            c_regu=(a*funcion(b)-b*funcion(a))/(funcion(b)-funcion(a))!Volvemos a definir el valor de c y comprobamos la diferencia 
            difere=abs(abs((c_regu))-abs((c_1)))!de este con el anterior.
        else if(funcion(c_regu)*funcion(a)<0)then
            b=c_regu!lo mismo pero con el intervalo [a,c]
            c_regu=(a*funcion(b)-b*funcion(a))/(funcion(b)-funcion(a))
            difere=abs(abs((c_regu))-abs((c_1)))
        end if
        if(funcion(c_regu)*funcion(a)==0) exit !Si encontramos el cero nos vamos.
        c_1=c_regu
        iteracion_reg=iteracion_reg+1!Contamos el numero de iteraciones.
    end do

    !AHORA LO RESOLVEREMOS MEDIANTE EL MÉTODO DE SECANTE

    !Definimos los valores de los limites del intervalo.
    a=0.d0
    b=1.d0
    !Redefinimos la variable difiere.
    difere=1.d0
    !Tomamos dos valores inicial para la x
    x_0s=1.d0
    x_1s=2.2d0
    x_1si=x_1s
    !Iniciamos el valor de la iteracion a 0
    iteracion_sec=0
    do while(difere>=error)!Comenzamos nuestro bucle
        xs=x_0s-((funcion(x_0s)*(x_0s-x_1s))/(funcion(x_0s)-funcion(x_1s)))!Tomamos el valor de x como el anterior 
        difere=abs(abs(xs)-abs(x_1s))!menos una expresion que tieneque ver con la derivada.
        x_1s=xs
        iteracion_sec=iteracion_sec+1!Contamos las iteraciones que se hacen
    end do


    print*,"Ahora se Mostraran los valores de las c y las iteraciones que cada método ha usado:"
    print*,'___________________________________________________________________________________________'
    print*,"BISECCIÓN:"
    print*,"El valor ",c_bisec,"es un cero de la función."
    print*,'Se han realizado ',iteracion_bisect,' iteraciones para encontrar el cero.'
    print*,'___________________________________________________________________________________________'
    print*,"NEWTON RAPHSON:"
    print*,"El valor ",x,"es un cero de la función."
    print*,'Se han realizado ',iteracion_newt,' iteraciones para encontrar el cero.'
    print*,'Se ha escogido como valor inicial: ',x_1,"."
    print*,'___________________________________________________________________________________________'
    print*,"REGULA FALSI:"
    print*,"El valor ",c_regu,"es un cero de la función."
    print*,'Se han realizado ',iteracion_reg,' iteraciones para encontrar el cero.'
    print*,'___________________________________________________________________________________________'
    print*,'SECANTE: '
    print*,"El valor ",xs,"es un cero de la función."
    print*,'Se han realizado ',iteracion_sec,' iteraciones para encontrar el cero.'
    print*,'Se ha escogido como valores iniciales: ',x_0s,"y",x_1si,"."
    print*,'___________________________________________________________________________________________'



end program Programa_EXAMEN_2