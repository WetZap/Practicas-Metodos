function funcion(x) result(valor)!Esta funcion es la funcion que nos piden integrar
    implicit none
    real*8 :: x,valor
    valor=1.d0/(1.d0+x)
end function

program Practica_2_4
    implicit none
    real*8 x(0:19),h,suma,a,b,stop
    real*8,external::funcion
    integer i,j,n
    !Declaramos las variables
    a=0.d0
    b=1.d0
    print*,"Valor de n: "
    read(*,*)n
    !Comprobamos que el valor introducido se un multiplo de 2.
    if ( mod(n,2)==0 ) then
        !Aplicamos la formula del calculo integral
        h=b-a
        !Declaramos la funcion en el 0 para iniciar el bucle desde 1 hasta n
        x(0)=h/2*(funcion(a)+funcion(b))
        do i=1,n
            !Conseguimos dividir el valor de h entre 2 para que sea el correspondiente a cada valor de h
            h=h/2
            suma=0.d0!Inicializamos la suma a 0 para que la suma resultante no se vea afectada
            do j = 1, 2**(i-1)
                suma=suma+funcion(a+(2*j-1)*h)
            end do!Escribimos el valor de x(i) como la formula empleando la anterior.
            x(i)=x(i-1)/2+h*suma
        end do
        print*,"El valor de la integral es: ",x(n)
    else
        print*,"El valor de n debe ser un multiplo de 2."
    end if
    read(*,*) stop
end program Practica_2_4
