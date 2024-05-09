function funcion(x) result(valor)
    implicit none
    real*8 x,valor
    valor=x**3-5*x**2+7*x-3
end function

program Practica_4_Biseccion
    implicit none
    real*8,external::funcion
    real*8 a,b,valor,error,c,difere,f_a,f_b,c_anterior
    a=0.d0
    b=5.d0
    error=10.d0**(-3)
    difere=1.d0
    c=(b+a)/(2.d0)
    c_anterior=c
    do while(difere>=error)
        if (funcion(c)*funcion(a)>0) then
            a=c
            c=(b+a)/(2.d0)
            difere=abs((funcion(c))-(funcion(c_anterior)))
            c_anterior=c
        else if(funcion(c)*funcion(a)<0)then
            b=c
            c=(b+a)/(2.d0)
            print*,c
            difere=abs((funcion(c))-(funcion(c_anterior)))
            c_anterior=c
        end if
         if(funcion(c)*funcion(a)==0) exit 
    end do

    print*,"El valor ",c,"es un cero de la función."
end program Practica_4_Biseccion