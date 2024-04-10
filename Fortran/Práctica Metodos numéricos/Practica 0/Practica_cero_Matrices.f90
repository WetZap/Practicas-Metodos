program Matrices
    implicit none
    real*8 matriz_A(0:3,0:3), matriz_B(0:3,0:3),matriz_prod(0:3,0:3),valor
    integer i,j,k,l
    open(11,file='Matrices.txt',status="old")
    read(11,*)matriz_A,matriz_B
    do i = 0,2
        valor=0.d0
        do j = 0,2
                valor=valor+matriz_A(i,j)*matriz_B(j,i)
            
        end do
        
    end do

end program Matrices
