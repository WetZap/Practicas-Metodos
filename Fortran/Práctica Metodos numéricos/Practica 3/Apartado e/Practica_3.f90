program Apartado_e
    implicit none
    real*8 matriz(100,100),matriz_coef(100),pivote,x(100),suma,matriz_supl(100,100),matriz_coef_sup(100),t(100,100),v(100)
    integer i,j,k,fil,col,fil_1,n,l
    open(11,file='matriz.txt',status='old')!Abrimos los archivos que vamos a usar
    open(12,file='matriz_coef.txt',status='old')
    open(13,file='valo_resu.txt',status='unknown')
    open(14,file='Resultados.txt',status='unknown')

    read(11,*)fil,col
    read(12,*)fil_1

    if ( fil==fil_1 ) then
        

    do i = 1, fil
        read(11,*)(matriz(j,i),j=1,col)
    end do


    do i = 1, fil_1
        read(12,*) matriz_coef(i)
    end do
    n=fil
    matriz_supl=matriz
    matriz_coef_sup=matriz_coef
    do k = 1, n
        pivote=matriz(k,k)
        do i = 1, fil
            do j=1,col
                if (i/=k.and.j>=k) then!Igual pero hago ceros fuera de diagonal.
                    matriz_supl(j,i)=matriz(j,i)-((matriz(k,i)/pivote)*matriz(j,k))
                    matriz_coef_sup(i)=matriz_coef(i)-((matriz(k,i)/pivote)*matriz_coef(k))     
                end if
            end do
            matriz=matriz_supl
        end do
        matriz_coef=matriz_coef_sup

    end do

    do i = 1, col
        write(13,*)(matriz(j,i),j=1,fil),matriz_coef_sup(i)
    enddo

    do i=1,n
        x(i)=matriz_coef(i)/matriz(i,i)
    end do
    do i = 1, n
        write(14,*) x(i)
    end do

    else
        print*,"Para que se pueda resolver la matriz de coeficientes tiene que tener la misma dimension que la matriz."
    end if


end program Apartado_e