program Practica_1
    implicit none
    real*8 val_x(0:3),val_y(0:3),t,polinomio,coef
    integer i,j,k,n
    open(11,file='Tabla_Valores.txt',status='old')
    open(12,file='lagrange.dat',status='unknown')
    read (11,*)val_x,val_y
    n=4
    !Inciamos el bucle que se encargara de tomar el valor de la x
    do k=0,100
        polinomio=0.d0!Iniciamos el polinomio correspondiente a 0 para que no interfiera con los otros
        t=0.4 + k*(0.4/100)
        do j=0,(n-1)!El segundo bucle solo llegara hasta el grado del polinomio menos uno
            coef=1.d0
            do i = 0,(n-1)!Este bucle se encarga de tomar el valor del productorio
                if ( j.NE.i ) then
                    coef=coef*((t-val_x(i))/(val_x(j)-val_x(i)))
                end if
            end do
            !Finalmente lo escribimos en el archivo o lo imprimimos por pantalla
            polinomio=polinomio+val_y(j)*coef
           !print *,t,polinomio
            write(12,*)t,polinomio
        end do
    end do

end program Practica_1