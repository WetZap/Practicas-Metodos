program Programa_EXAMEN_1
    implicit none
    real*8 matriz(100,100),matriz_coef(100),x(100),x_auxi(100),matriz_pa_Rich(100,100),matriz_coef_pa_rich(100),pivote,p,error,ant,&
    & matriz_coef_sup(100),matriz_supl(100,100),v(100),t(100,100),suma
    real*8,external::sumatorio,sumatorio_jac,sumatorio_sneide_1,sumatorio_sneide_2,gauss
    character,external::Menu,comprobacion_demas,comprobacion_richa
    character resp,usado
    integer i,j,col,fil,k,n,max,fil_1
    open(11,file='matriz.txt',status='old')
    open(13,file='matriz_coef.txt',status='old')
    !Leo las columnas de la matriz
    read(11,*)col,fil
    read(13,*)fil_1
    
    !Aqui describire la forma en la que se debe introducir la matriz, en la primera fila se debe introducir el valor de 
    !las dimensiones de la matriz, por ahora la matriz debe ser cuadrada, además los valores fraccionarios deberan ponerse en 
    !formato decimal con un punto como indicador de inicio de decimales.El programa no es capaz de interpretar letras y 
    !no se tiene en cuenta la posibilidad de que  el pivote fuese cero (la resolucion sería simplemente intercambiar filas).

    !Comprobamos que la dimension de ambas matrices es igual.
    print*,"DETALLES DEL PROGRAMA"
    print*,"El programa solo acepta matrices cuadradas."
    print*,"Como se debe introducir la matriz esta comentada en el código fuente (ciñanse al formato)."
    if ( fil==fil_1 ) then  
        !Defino el error que tendra el resultado
        error=10**(-5)
        n=fil!Defino como n el numero maximo
        !Leo la matriz que queremos calcular sus soluciones
        do i = 1, fil
            read(11,*)(matriz(j,i),j=1,col)
        end do
        !Leo los valores de los coeficientes de la matriz
        do i = 1, fil
            read(13,*) matriz_coef(i)
        end do
        !Defino el valor que va a tomar N (Esto solo sirve para los métodos iterativos)
        max=100000
        !Pregunto la opcion que se va a tomar.
        resp=Menu()
        do while(resp/='7')
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
                        if ( abs(x(n)-ant)<error ) exit
                        ant=x(n)
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
                        if ( abs(x(n)-ant)<error ) exit
                        ant=x(n)
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
                        if ( abs(x(n)-ant)<error ) exit
                        ant=x(n)
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
                        if ( abs(x(n)-ant)<error ) exit
                        ant=x(n)
                    end do
                    
                    !Abro y escribo soluciones.
                     open(15,file='Resolucion_Snei.txt',status='unknown')
                    do i = 1, n
                        write(15,*) x(i)
                    end do
                endif
                resp=Menu()
            else if (resp=='4') then
                open(22,file='Resolucion_Gauss.txt',status='unknown')
                open(23,file='Resolucion_Gauss_Matriz.txt',status='unknown')

                n=fil!Defino n como el numero de filas, se podría haber hecho lo mismo pero con columnas.
                matriz_supl=matriz!Las matrices suplementarias toman los valores de las matrices originales.
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
                        matriz=matriz_supl!Copiamos en la matriz antigua los valores de la nueva para que en la siguiente vuelta no se
                    end do!entorpezcan los valores.
                    matriz_coef=matriz_coef_sup!Lo mismo.
            
                end do
            
                do i = 1, col
                    write(23,*)(matriz(j,i),j=1,fil),matriz_coef(i)
                enddo
            
                !De esta forma resolvemos el sistema, como el ultimo valor es igual x(n) lo despejamos y hacemos un bucle inverso que nos 
                x(n)=(matriz_coef(n)/matriz(n,n))!va resolviendo el sistema.
            
                do k = n-1, 1,-1
                    suma=0.d0
                    do i = k+1, n
                        suma=suma+(matriz(i,k)*x(i))
                    end do
                    x(k)=(matriz_coef(k)-suma)/matriz(k,k)
                end do
                !Escribimos la matriz de resultados en un archivo.
                do i = 1, n
                    write(22,*) x(i)
                end do

                print*,'La matriz resultante y los valores de x se encuentran en un archivo.'
                resp=Menu()
            else if(resp=='5')then
                open(20,file='Resolucion_Gauss_Div_Uni.txt',status='unknown')
                open(21,file='Resolucion_Gauss_Div_Uni_Matriz.txt',status='unknown')

                n=fil
                matriz_supl=matriz
                matriz_coef_sup=matriz_coef
                do k = 1, n-1
                    pivote=matriz(k,k)
                    do i = 1, fil
                        do j=1,col
                            if (i>k.and.j>=k) then!La uncia diferencia respecto a Gauss es que hago 1 el elemento pivote 
                                t(j,k)=matriz(j,k)/pivote  
                                v(k)=matriz_coef(k)/pivote    
                                matriz_supl(j,i)=matriz(j,i)-(matriz(k,i)*t(j,k))
                                matriz_coef_sup(i)=matriz_coef(i)-(matriz(k,i)*v(k))     
                            end if
                        end do
                        matriz=matriz_supl
                    end do
                    matriz_coef=matriz_coef_sup
                end do
                do i = 1, col
                    write(21,*)(matriz(j,i),j=1,fil),matriz_coef(i)
                enddo
            
                x(n)=(matriz_coef(n)/matriz(n,n))
            
                do k = n-1, 1,-1
                    suma=0.d0
                    do i = k+1, n
                        suma=suma+(matriz(i,k)*x(i))
                    end do
                    x(k)=(matriz_coef(k)-suma)/matriz(k,k)
                end do
                do i = 1, n
                    write(20,*) x(i)
                end do
                print*,'La matriz resultante y los valores de x se encuentran en un archivo.'
                resp=Menu()
            else if(resp=='6') then
                open(31,file='Resolucion_Gauss_Jordan.txt',status='unknown')
                open(30,file='Resolucion_Gauss_Jordan_Matriz.txt',status='unknown')
                n=fil
                matriz_supl=matriz
                matriz_coef_sup=matriz_coef
                do k = 1, n
                    pivote=matriz(k,k)
                    do i = 1, fil
                        do j=1,col
                            if (i/=k.and.j>=k) then!Igual pero hago ceros fuera de diagonal.
                                matriz_supl(j,i)=matriz(j,i)-((matriz(k,i)/pivote)*matriz(j,k))
                                matriz_coef_sup(i)=matriz_coef(i)-((matriz(k,i)/pivote)*matriz_coef(k))     
                            end if
                        end do
                        matriz=matriz_supl
                    end do
                    matriz_coef=matriz_coef_sup
            
                end do
                do i = 1, fil
                    write(30,*)(matriz(j,i),j=1,col),matriz_coef_sup(i)
                enddo
            
                do i=1,n
                    x(i)=matriz_coef(i)/matriz(i,i)
                end do
                do i = 1, n
                    write(31,*) x(i)
                end do
                print*,'La matriz resultante y los valores de x se encuentran en un archivo.'
                resp=Menu()
            else!Me aseguro de que no escriba datos invalidos.
                print*,'Escogio un valor no valido.'
                resp=Menu()
            endif
        end do
    else
        print*,"Para que se pueda resolver la matriz de coeficientes tiene que tener la misma dimension que la matriz."
    end if

end program Programa_EXAMEN_1

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
    print*,'4.-Resolver por Gauss.'
    print*,'5.-Resolver por Gauss-Division-Unica.'
    print*,'6.-Resolver por Gauss-Jordan.'
    print*,'7.-Salir.'
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
