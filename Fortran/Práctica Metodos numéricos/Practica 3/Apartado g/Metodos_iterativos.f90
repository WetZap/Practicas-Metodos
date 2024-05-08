program Metodos_iterativos
    implicit none
    real*8 matriz(100,100),matriz_coef(100),x(100),x_auxi(100),matriz_pa_Rich(100,100),matriz_coef_pa_rich(100),pivote,p
    real*8,external::sumatorio,sumatorio_jac,sumatorio_sneide_1,sumatorio_sneide_2
    character,external::Menu,comprobacion_demas,comprobacion_richa
    character resp
    integer i,j,col,fil,k,n,max,fil_1
    open(11,file='matriz.txt',status='old')
    open(13,file='matriz_coef.txt',status='old')
    !PUERTOS USADOS HASTA EL 15
    !Leo las columnas de la matriz
    read(11,*)col,fil
    read(13,*)fil_1
    !Comprobamos que la dimension de ambas matrices es igual.
    if ( fil==fil_1 ) then  
        n=fil!Defino como n el numero maximo
        !Leo la matriz que queremos calcular sus soluciones
        do i = 1, fil
            read(11,*)(matriz(j,i),j=1,col)
        end do
        !Leo los valores de los coeficientes de la matriz
        do i = 1, fil
            read(13,*) matriz_coef(i)
        end do
        !Defino el valor que va a tomar N
        max=100000
        !Pregunto la opcion que se va a tomar.
        resp=Menu()
        do while(resp/='4')
            if ( resp=='1' ) then!Metodo de Richarsdson
                !Resolucion por Richardson
                if ( comprobacion_richa(matriz,fil,col) == 'n' ) then!Si la compro. manda n, no se puede resolver.
                    print*,'La matriz introducida no puede ser calculada mediante Richardson.'
                else if(comprobacion_richa(matriz,fil,col)=='m') then!Si la compro. manda m , debemos hacer uno la diagonal.
                    matriz_pa_Rich=matriz
                    matriz_coef_pa_rich=matriz_coef
                    do i = 1, n!Convierto la diagonal en 1
                        pivote=matriz_pa_Rich(i,i)
                        do j = 1, n
                            matriz_pa_Rich(j,i)=(matriz_pa_Rich(j,i)/(pivote))
                        end do
                        matriz_coef_pa_rich(i)=matriz_coef_pa_rich(i)/pivote
                    end do
                    !Inicializo los valores a 0
                    do i = 1, n
                        x(i)=0
                    end do
                    !Resuelvo mediante Richardson.
                    do k= 1, max
                        do i = 1, n
                            x(i)=x(i)-sumatorio(n,matriz_pa_Rich,x,i)+matriz_coef_pa_rich(i)
                        end do
                    end do
                    !Abro el arhcivo en el que se guardara y lo escribo dentro.
                    open(12,file='Resolucion_Rich.txt',status='unknown')
                    do i = 1, n
                        write(12,*) x(i)
                    end do
                elseif(comprobacion_richa(matriz,fil,col) == 's') then!Si la compro. manda s, ya es uno la diagonal y se puede.
                    matriz_pa_Rich=matriz
                    matriz_coef_pa_rich=matriz_coef
                    !Inicializo los valores a 0
                    do i = 1, n
                        x(i)=0
                    end do
                    !Resuelvo mediante RIchardson
                    do k= 1, max
                        do i = 1, n
                            x(i)=x(i)-sumatorio(n,matriz_pa_Rich,x,i)+matriz_coef_pa_rich(i)
                        end do
                    end do
                    !Abro el archivo y escribo las respuestas.
                    open(12,file='Resolucion_Rich.txt',status='unknown')
                    do i = 1, n
                        write(12,*) x(i)
                    end do
                endif
                resp=Menu()
            else if ( resp=='2' ) then!Metodo de Jacobi
                if ( comprobacion_demas(matriz,fil,col)=='n' ) then!Si la compro. manda n, no se puede resolver.
                    print*,'La matriz introducida no converge mediante este metodo.'
                else
                    !Inicializo los valores a 0
                    do i = 1, n
                        x(i)=0
                    end do
                    !Resuelvo por Jacobi
                    do k = 1, max
                        do i = 1, n
                            x(i)=(1.d0/matriz(i,i))*(-sumatorio_jac(i,n,x,matriz)+matriz_coef(i))
                        end do
                        
                    end do
                    !Abro y escribo las soluciones.
                    open(14,file='Resolucion_Jaco.txt',status='unknown')
                    do i = 1, n
                        write(14,*) x(i)
                    end do
                endif
                resp=Menu()
            else if (resp=='3') then!Metodo Gauss-Sneider
                if ( comprobacion_demas(matriz,fil,col)=='n' ) then!Si la compro. manda n, no se puede resolver.
                    print*,'La matriz introducida no converge mediante este metodo.'
                else
                    !Inicializo a 0
                    do i = 1, n
                        x(i)=0
                    end do
                    !Resuelvo por Sneider.
                    do k = 1, max
                        do i = 1, n
                            x(i)=(1.d0/matriz(i,i))*(matriz_coef(i)-sumatorio_sneide_1(i,n,matriz,x)-sumatorio_sneide_2&
                            &(i,n,matriz,x))
                        end do
                    end do
                    !Abro y escribo soluciones.
                    open(15,file='Resolucion_Snei.txt',status='unknown')
                    do i = 1, n
                        write(15,*) x(i)
                    end do
                endif
                resp=Menu()
            else!Me aseguro de que no escriba datos invalidos.
                print*,'Escogio un valor no valido.'
                resp=Menu()
            endif
        end do
    else
        print*,"Para que se pueda resolver la matriz de coeficientes tiene que tener la misma dimension que la matriz."
    end if

end program Metodos_iterativos

function sumatorio(n,matriz,x,i) result(suma)!Un sumatorio cualquiera, que llega hasta un valor dado.
    implicit none
    real*8 suma,matriz(100,100),x(100)
    integer n,i,j
    suma=0.d0
    do j = 1, n
        suma=suma+(matriz(j,i)*x(j))
    end do
end function sumatorio
function sumatorio_jac(i,n,x,matriz) result(suma)!Un sumatorio que excluye los elementos de la diagonal.
    implicit none
    real*8 matriz(100,100),x(100),suma
    integer i,j,n
    suma=0.d0
    do j = 1, n
        if ( j/=i ) then
            suma=suma+(matriz(j,i)*x(j))
        end if
    end do
end function
function sumatorio_sneide_1(i,n,matriz,x) result(suma)!Un sumatorio que empieza en un valor dado y acaba en otro dado.
    implicit none
    real*8 matriz(100,100),x(100),suma
    integer i,n,j
    suma=0.d0
    do  j= i+1, n
        suma=suma+(matriz(j,i)*x(j))
    end do
end function
function sumatorio_sneide_2(i,n,matriz,x) result(suma)!Un sumatorio que acaba en un dado dado menos 1
    real*8 matriz(100,100),x(100),suma
    integer i,n,j
    suma=0.d0
    do j = 1, i-1
        suma=suma+(matriz(j,i)*x(j))
    end do
end function
function comprobacion_richa(matriz,fil,col) result(respuesta)!funcion que se encarga de comprobar la convergencia de Richard.
    implicit none
    real*8 matriz(100,100),suma
    character respuesta
    integer i,j,fil,col
    respuesta='s'!Partimos de que sea verdadero.
    do i = 1, fil
        if (matriz(i,i)/=1) then
            respuesta='m'!Este valor lo que nos dice es si la matriz tiene unos en la diagonal, si se marca como m, no tiene.
        end if
    end do
    do i = 1, fil
        suma=0.d0
        do j = 1, col
            if ( j/=i ) then
                suma = suma + abs(matriz(j,i))
            end if
        end do
        if ( suma>matriz(i,i) ) then
            respuesta='n'!Lo que hacemos es comprobar si la suma de los elementos de una fila sin contar la diagonal, suman mas que
        end if!el propio elemento de la diiagonal.
    end do
end function
function Menu() result(res)!Funcion que se encarga de tomar el valor de la opcion del menu.
    implicit none
    character res
    print*,"Menu:"
    print*,'1.-Resolver por Richardson.'
    print*,'2.-Resolver por Jacobi.'
    print*,'3.-Resolver por Gauss-Sneider.'
    print*,'4.-Salir.'
    print*,"Digame su eleccion: "
    read(*,*)res

end function Menu
function comprobacion_demas(matriz,fil,col) result(respuesta)!funcion que se encarga de comprobar la convergencia de Sne. y Jaco..
    implicit none
    real*8 matriz(100,100),suma
    character respuesta
    integer i,j,fil,col
    respuesta='s'
    do i = 1, fil
        suma=0.d0
        do j = 1, col
            if ( j/=i ) then
                suma=suma+abs(matriz(j,i))
            end if
        end do
        if ( abs(matriz(i,i))<suma ) then
            respuesta='n'!Lo que hacemos es comprobar si la suma de los elementos de una fila sin contar la diagonal, suman mas que
        end if!el propio elemento de la diiagonal.
    end do
end function comprobacion_demas
