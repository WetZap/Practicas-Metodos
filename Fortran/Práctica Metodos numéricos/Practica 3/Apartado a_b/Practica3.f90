program Apartado_a_b
    implicit none
    real*8 matriz(100,100)
    integer i,j,col,fil
    open(11,file='Matriz_Lectura.txt',status='old')
    open(12,file='Matriz_Escritura.txt',status='unknown')
    
    read(11,*)col,fil

    do i = 1, fil
            read(11,*)(matriz(j,i),j=1,col)
    end do
    do i = 1, fil
        write(12,*)(matriz(j,i),j=1,col)

end do
end program Apartado_a_b