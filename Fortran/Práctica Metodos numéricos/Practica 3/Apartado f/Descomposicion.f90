program Descomposicion
    implicit none
    interface
        function Gauss(matriz,fil,col,L) result(matriz_supl)
            implicit none
            real*8::matriz(100,100),L(100,100)
            real*8 matriz_supl(100,100)
            integer::fil,col
        end function
    end interface
    real*8 matriz(100,100),L(100,100),U(100,100)
    !real*8,external::Gauss
    integer i,j,col,fil,n
    !Abrimos los archivos que vamos a usar.
    open(11,file='matriz.txt',status='old')!Este almacena los valores de la matriz a descomponer.
    open(12,file='Matriz_L.txt',status='unknown')!Este almacenara los valores de la matriz L descompuesta.
    open(13,file='Matriz_U.txt',status='unknown')!Este almacenara los valores de la matriz U descompuesta.
    !Leo los valores de las columnas y las filas.
    read(11,*)col,fil
    !Defino a n como el valor maximo, en este caso n
    n=fil

    do i = 1, col
        read(11,*)(matriz(j,i),j=1,fil)!Leo los valores de la matriz a descomponer.
    end do
    matriz=Gauss(matriz,fil,col,L)
    !Inicio el proceso de descomposicion de la matriz L
    L(1,1)=matriz(1,1)!Lo demas se resuelve en la funcion Gauss ya que necesita los valores que multiplican a los"pivote".
    !Iniciamos el proceso de resolucion de U
    do i = 1, fil
        do j= 1,col
            if ( j<i ) then
                U(j,i)=0
            else
                U(j,i)=matriz(j,i)
            end if
        end do
    end do
    !Escribimos las matrices descompuestas en sus respectivos archivos.
    do i = 1, fil
        write(12,*)(L(j,i),j=1,col)
    enddo
    do i = 1, fil
        write(13,*)(U(j,i),j=1,col)
    enddo
end program Descomposicion

function Gauss(matriz,fil,col,L) result(matriz_supl)
    implicit none
    real*8 matriz(100,100),matriz_supl(100,100),pivote,L(100,100)
    integer i,j,k,n,fil,col  
    n=fil
    matriz(1,1)=matriz(1,1)/matriz(1,1)
    matriz_supl=matriz
    
    do k = 1, n!Comenzamos el bucle que tendra n-1 pasos.
        do i =2, fil
            pivote=matriz(k,k)!Elegimos el elemento de la diagonal como elemento pivote.
            do j = 1, col
                if (i>k.and.j>=k) then!Esta es la condicion para que se resuelva el sistema.
                    matriz_supl(j,i)=matriz(j,i)-(((matriz(k,i)*1.d0)/pivote)*matriz(j,k))
                    if ( j==i ) then!El proceso de resolucion es este.
                        L(i,i)=1
                    elseif(j>i)then
                        L(j,i)=0  
                    elseif(j<i)then
                        L(j,i)=(matriz(j,i)/matriz(j,j))
                    end if
                end if
            end do
            matriz=matriz_supl!Copiamos en la matriz antigua los valores de la nueva para que en la siguiente vuelta no se
        end do!entorpezcan los valores.
    end do
end function Gauss