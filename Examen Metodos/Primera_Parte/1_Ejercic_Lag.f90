program Ejer_1_La
    implicit none
    real*8 val_x(0:4),val_y(0:4),t,polinomio,coef,stop!Defino los tipos de las variables, aunque el profesor prefiera definirlo como 
    integer i,j,k,n                              !implicito
    open(11,file='valores.txt',status='old')!Abrimos el archivo correspondiente(es muy importante comprobar que los datos se han
    read (11,*)val_x,val_y                  !metido correctamente(lo que yo no hice))
    !Inciamos el bucle que se encargara de tomar el valor de la x que nosotros queremos interpolar
    do k=0,10!En este caso nos piden que lo interpolemos entre 0 y 10 diez veces
        polinomio=0.d0!Iniciamos el polinomio correspondiente a 0 para que no interfiera con los otros
        t=k*(1.d0/10)!Aqui yo no tuve cuidado y se me olvido poner el 1.d0, entonces hacía la división entera y no la real.
        do j=0,4!El segundo bucle solo llegara hasta el grado del polinomio menos uno
            coef=1.d0!Inicializo los coeficientes a 1.d0 para que no interfiera con los demas(es a 1.d0 y no a 0.0)
            do i = 0,4!Este bucle se encarga de tomar el valor del productorio
                if ( i.NE.j ) then!Esta condición es la que se expone en la formula del polinomio
                    coef=coef*((t-val_x(i))/(val_x(j)-val_x(i)))
                end if
            end do
            !Finalmente sumamos cada coeficiente con el polinomio anterior y lo repetimos hasta acabar el bucle.
            polinomio=polinomio+val_y(j)*coef
        end do!Imprimimos por pantalla.
         print *,t,polinomio
    end do
    read(*,*)stop !Este comando sirve para detener el ejecutable en caso de que al terminer se cierre.


!Nota: El algoritmo no es muy eficiente porque se pueden sacar funciones que agilicen las operaciones.
!En un principio el productorio iba a ser una función pero como me equivoque escribiendo los valores de x me salia mal y probe 
!haciendolo de la forma más básica.
end program Ejer_1_La