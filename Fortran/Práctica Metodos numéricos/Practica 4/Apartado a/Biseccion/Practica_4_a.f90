function funcion(x) result(valor)
    implicit none
    real*8 x,valor
    valor=x+exp(x)
end function

program Practica_4_Biseccion
    implicit none
    real*8,external::funcion
    real*8 a,b,valor,error,c,difere,f_a,f_b,c_1
    a=-1.d0
    b=0.d0
    error=10.d0**(-3)
    difere=1.d0
    c=(b-a)/(2.d0)
    c_1=c
    do while(difere>=error)
        if (funcion(c)*funcion(a)>0) then
            a=c
            print*,difere
            c=(b-a)/(2.d0)
            difere=abs(abs(funcion(c))-abs(funcion(c_1)))
        else if(funcion(c)*funcion(a)<0)then
            b=c
            c=(b-a)/(2.d0)
            difere=abs(abs(funcion(c))-abs(funcion(c_1)))
        else if(funcion(c)*funcion(a)==0)then 
            go to 100
        end if
        c_1=c
    end do

    print*,"El valor ",c,"es un cero de la función."
    100 print*,"El valor ",c,"es un cero de la función."
end program Practica_4_Biseccion