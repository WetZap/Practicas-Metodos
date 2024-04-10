program Practica_0
    real*8 valoresn(1:11),valoresk(1:11),matriz_pascal(0:10,0:10)
    integer i,j

    do i=1,11
        valoresn(i)=i
    end do
    do j=1,11
        valoresk(j)=j
    end do
    do i=0,10
        do j=0,10
            matriz_pascal(i,j)=0
        end do
    end do
    do i=0,10
        do j=0,10
            if ((((j/2)*10)==0) && (i=6))
                matriz_pascal(i,j)=0
               
        end do
    end do 

end program Practica_0