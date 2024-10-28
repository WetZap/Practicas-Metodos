program Practica_2_0
    implicit none
    real*8 Coeficientes_cotes(0:9,8)
    integer i,j,valor
    !Abrimos el archivo en que queremos guardar los coeficientes
    open(11,file="Coeficientes_cotes_nuevo.txt",status="unknown")
    !La variable i significa columnas y la j las filas
    do i = 0,9
        do j = 1,8
            read(*,*) valor
            Coeficientes_cotes(i,j)=valor!Le pregunto el valor que quiere introducir.
        end do
    end do
    !Escribo la matriz resultante en archivo.
    write (11,*)Coeficientes_cotes
end program Practica_2_0