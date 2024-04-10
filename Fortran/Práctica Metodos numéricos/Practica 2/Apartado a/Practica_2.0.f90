program Practica_2_0
    implicit none
    real*8 Coeficientes_cotes(0:9,8)
    integer i,j,valor
    open(11,file="Coeficientes_cotes_nuevo.txt",status="unknown")
    do i = 0,9
        do j = 1,8
            read(*,*) valor
            Coeficientes_cotes(i,j)=valor
        end do
    end do
    write (11,*)Coeficientes_cotes
end program Practica_2_0