function funcion(x) result(valor)!Esta funcion es la funcion que nos piden integrar
    implicit none
    real*8 :: x,valor
    valor=1.d0/(1.d0+x)
end function

function funcion_fi(x,h) result(valor)
    implicit none
    real*8 :: x,valor,h,funcion
    valor=(funcion(x+h)-funcion(x-h))/2*h
end function

program Practica_2_5
    implicit none
    real*8 x,h,f_prima_tay,f_prima_rich,stop
    real*8,external :: funcion, funcion_fi
    ! Inicializo las variables
    print*,"Valor de x: "
    read(*,*)x
    print*,"Valor de h: "
    read(*,*)h
   !Primera forma, desarrollo en serie de Taylor
    f_prima_tay=(funcion(x+h)-funcion(x-h))/2*h!rev

   !Segunda forma, extrapolacion de Richardson
    f_prima_rich=((4.d0/3.d0)*funcion_fi(x,h/2))-((1.d0/3.d0)*funcion_fi(x,h))
    
    !Expreso los valores
    print*,"El valor según el desarrollo en serie de Taylor es: ",f_prima_tay
    print*,"El valor según la extrapolacion de Richardson es: ",f_prima_rich
read(*,*)stop
end program Practica_2_5

