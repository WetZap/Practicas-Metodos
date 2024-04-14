program Practica_1
    implicit none
    real*8 val_x(0:3),val_y(0:3),sx,sy,sxx,sxy, t, alfa_0,alfa_1
    integer i,n
    !Abro los dos archivos
    open(11,file='Tabla_Valores.txt',status='old')
    open(12,file='mincuad.dat',status='unknown')
    read (11,*)val_x,val_y
    n=4
    !Lo que hare ahora será calcular los sx,etc
    sx=0.d0
    sxy=0.d0
    sy=0.d0
    sxx=0.d0
    do i=0,3
        sx=sx+val_x(i)
        sxy=sxy+val_x(i)*val_y(i)
        sy=sy+val_y(i)
        sxx=sxx+val_x(i)*val_x(i)
    end do
    !Aqui expreso la formula de alfa según la encontrada en una pagina web
    alfa_0=(sxx*sy-sx*sxy)/(n*sxx-sx*sx)
    alfa_1=(n*sxy-sx*sy)/(n*sxx-sx*sx)
    !Imprimo los resultados en intervalos.
    do i=0,100
        t=0.4 + i*(0.4/100)
        write(12,*)t,alfa_0+t*alfa_1
    end do    
end program Practica_1