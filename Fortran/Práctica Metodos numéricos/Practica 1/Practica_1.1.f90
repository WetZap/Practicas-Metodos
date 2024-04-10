program Practica_1
    implicit none
    real*8 val_x(0:3),val_y(0:3),t,polinomio,coef
    integer i,j,k,n
    open(11,file='Tabla_Valores.txt',status='old')
    open(12,file='mincuad.dat',status='unknown')
    read (11,*)val_x,val_y
    n=4

    do i=0,100
        polinomio=0.d0
        t=0.4 + i*(0.4/100)
        do j = 0,3
            coef=1.d0
            do k = 0, 3
                if ( k.NE.j ) then
                    coef=coef*((t-val_x(k))/val_x(j)-val_x(k))
                end if
            end do
         polinomio=polinomio+val_y(j)*coef
        end do
        print *,t,polinomio
    end do



end program Practica_1