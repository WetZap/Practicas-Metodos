program Apartado_a_b!Este programa soprta hasta matrices de orden 100x100
    implicit none
    real*8 matriz(100,100)
    integer i,j,col,fil
    open(11,file='Matriz_Lectura.txt',status='old')!Abro el archivo que contiene a la matriz, su formato sería en la primera fila 
    !se encontraria la dimension de la matriz y a continuacion los valores. 
    open(12,file='Matriz_Escritura.txt',status='unknown')!Abro el archivo en el que voy a escribir la matriz.
    
    read(11,*)col,fil!Leo las dimensiones de las mattrices.

    do i = 1, fil
            read(11,*)(matriz(j,i),j=1,col)!Leo la matriz con un bucle implicito
    end do
    do i = 1, fil
        write(12,*)(matriz(j,i),j=1,col)!Escribo la matriz con un bucle implicito

end do
end program Apartado_a_b