function ini_cero(matriz_X) result(matriz_X)
    implicit none
    real*8 matriz_X(0:10)
    integer i
    do i = 0, 10
        matriz_X(i)=0
    end do
end function

program Metodo de Jorge
    implicit none
    real*8 matriz_Q(0:10,0:10),matriz_A(0:10,0:10),matriz_B(0:10),matriz_X(0:10)
    real*8,external::ini_cero
    integer i
    
end program Metodo de Jorge