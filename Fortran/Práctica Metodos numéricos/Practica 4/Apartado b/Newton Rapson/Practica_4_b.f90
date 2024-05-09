function funcion(x) result(valor)
    implicit none
    real*8 x,valor
    valor=x**3-5*x**2+7*x-3
end function
function funcion_prima(x) result(valor)
    implicit none
    real*8 x,valor
    valor=3*x**2-10*x+7
end function

program Practica_4_Newton
    implicit none
    real*8,external::funcion,funcion_prima
    real*8 a,b,error,difere,x,x_1
    a=0.d0
    b=5.d0
    error=10.d0**(-3)
    difere=1.d0
    x=0.d0
    x_1=x
    do while(difere>=error)
        x=x-(funcion(x)/funcion_prima(x))
        difere=abs(abs(funcion(x))-abs(funcion(x_1)))
        x_1=x
    end do

    print*,"El valor ",x,"es un cero de la función."
    100 print*,"El valor ",x,"es un cero de la función."
end program Practica_4_Newton