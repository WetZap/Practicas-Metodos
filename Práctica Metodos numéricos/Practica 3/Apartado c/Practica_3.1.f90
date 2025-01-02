program Practica_3_c
    implicit none
    real*8 matriz(100,100),matriz_coef(100),pivote,x(100),suma,matriz_supl(100,100),matriz_coef_sup(100)
    integer i,j,k,fil,col,fil_1,n
    !Abrimos los archivos que vamos a usar:
    !Este tendra los valores de la matriz.
    open(11,file='matriz.txt',status='old')
    !Este los coeficientes de la matriz de coeficientes
    open(12,file='matriz_coef.txt',status='old')
    !Este almacenara la matriz resultado que sale despues de hacer las operaciones.
    open(13,file='valo_resu.txt',status='unknown')
    !Este almacenara los valores de x tras resolver el sistema
    open(14,file='Resultados.txt',status='unknown').

    read(11,*)fil,col!Leemos los valores de las filas y columnas.
    read(12,*)fil_1!leemos el valor de la fila de la matriz de coeficientes.
    
    !Compruebo si tienen el mismo numero de filas
    if ( fil==fil_1 ) then

        do i = 1, fil!Leo la matriz que contiene a los valores
            read(11,*)(matriz(j,i),j=1,col)
        end do
    
    
        do i = 1, fil_1!Leo la matriz que contiene a los coeficientes.
            read(12,*) matriz_coef(i)
        end do
        !Defino n como el numero de filas, se podría haber hecho lo mismo pero con columnas.
        n=fil
        !Las matrices suplementarias toman los valores de las matrices originales.
        matriz_supl=matriz
        matriz_coef_sup=matriz_coef
        
        do k = 1, n-1!Comenzamos el bucle que tendra n-1 pasos.
            do i =2, fil
                pivote=matriz(k,k)!Elegimos el elemento de la diagonal como elemento pivote.
                do j = 1, col
                    if (i>k.and.j>=k) then!esta es la condicion para que se resuelva el sistema.
                        matriz_supl(j,i)=matriz(j,i)-(((matriz(k,i)*1.d0)/pivote)*matriz(j,k))
                        matriz_coef_sup(i)=matriz_coef(i)-(((matriz(k,i)*1.d0)/pivote)*matriz_coef(k))
                    end if
                end do
                !Copiamos en la matriz antigua los valores de la nueva para que en la siguiente
                ! vuelta no se entorpezcan los valores.
                matriz=matriz_supl
            end do
            matriz_coef=matriz_coef_sup!Lo mismo.
    
        end do
    
        do i = 1, col
            write(13,*)(matriz(j,i),j=1,fil),matriz_coef(i)
        enddo
    
        !De esta forma resolvemos el sistema, como 
        !el ultimo valor es igual x(n) lo despejamos y 
        !hacemos un bucle inverso que nos va resolviendo el sistema. 
        x(n)=(matriz_coef(n)/matriz(n,n))
    
        do k = n-1, 1,-1
            suma=0.d0
            do i = k+1, n
                suma=suma+(matriz(i,k)*x(i))
            end do
            x(k)=(matriz_coef(k)-suma)/matriz(k,k)
        end do
        !Escribimos la matriz de resultados en un archivo.
        do i = 1, n
            write(14,*) x(i)
        end do
    else
        print*,"Para que se pueda resolver la matriz de coeficientes tiene que tener la misma dimension que la matriz."
    end if
    
end program Practica_3_c