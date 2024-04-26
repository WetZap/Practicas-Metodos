program Apartado_c
    implicit none
    real*8 matriz(100,100),matriz_supl(100,100),matriz_coef(100),matriz_coef_sup(100),x(100)
    real*8,external::sumatorio
    integer i,j,col,fil,k,fil_1,n
    open(11,file='matriz.txt',status='old')
    open(12,file='matriz_coef.txt',status='old')
    open(13,file='valo_resu.txt',status='unknown')
    open(14,file='coef_resu.txt',status='unknown')


    read(11,*)col,fil
    read(12,*)fil_1
    n=fil
    if ( fil_1==fil ) then

        do i = 1, col
            read(11,*)(matriz(i,j),j=1,fil)
        end do

        do i = 1, fil_1
            read(12,*) matriz_coef(i)
        end do

        matriz_supl=matriz
        matriz_coef_sup=matriz_coef
        do i = 1, fil
            do j=1,col
                do k = 1, fil-1
                    if ( i<=k ) then
                        matriz_supl(j,i)=matriz_supl(j,i)
                        matriz_coef_sup(i)=matriz_coef_sup(i)
                    else
                        if ( j>=k ) then
                            matriz_supl(j,i)=matriz_supl(j,i)-((matriz_supl(k,i)/matriz_supl(k,k))*matriz_supl(j,k))
                            matriz_coef_sup(i)=matriz_coef_sup(i)-((matriz_supl(k,i)/matriz_supl(k,k))*matriz_coef_sup(k))
                        end if
                    end if
                end do
            enddo
        end do
        
        do i = 1, col
            write(13,*)(matriz_supl(i,j),j=1,fil)
        enddo

        x(n)=((matriz_coef_sup(n)*1.d0)/matriz_supl(n,n))
        do k =n-1,1,-1
            x(k)=(1.d0/matriz_supl(k,k))*(matriz_coef_sup(k)-sumatorio(k,n,matriz_supl,x))
        end do

        do i = 1, fil
            write(14,*)x(i)
        end do
  
    else    
        print*,"Lo siento pero las filas no coinciden."
    end if

end program Apartado_c

function sumatorio(i,n,matriz_supl,x) result(suma)
    implicit none
    integer i,n,j
    real*8 matriz_supl(100,100),x(100),suma
    suma=0.d0
    do j = i+1, n
        suma=suma+(matriz_supl(j,i)*x(j))
    end do


end function 
