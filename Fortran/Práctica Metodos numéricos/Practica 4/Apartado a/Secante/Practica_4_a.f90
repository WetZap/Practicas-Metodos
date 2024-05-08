function funcion(x) result(valor)
    implicit none
    real*8 x,valor
    valor=x+exp(x)
end function


program Practica_4_Secante
    implicit none
    real*8,external::funcion
    real*8 a,b,error,difere,x,x_1,x__1
    a=-1.d0
    b=0.d0
    error=10.d0**(-3)
    difere=1.d0
    x=1.d0
    x__1=0.d0

    x_1=x
    do while(difere>=error)
        x=x-((funcion(x)*(x-x__1))/(funcion(x)-funcion(x__1)))
        difere=abs(abs(funcion(x))-abs(funcion(x_1)))
        x_1=x
        i=i+1
    end do
    print*,"El valor ",x,"es un cero de la función."
    100 print*,"El valor ",x,"es un cero de la función."
end program Practica_4_Secante