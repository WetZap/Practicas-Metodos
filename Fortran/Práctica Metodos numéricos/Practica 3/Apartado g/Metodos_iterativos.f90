program Metodos_iterativos
    implicit none
    real*8 Q_Jac(100,100),Q_Rich(100,100),Q_Seid(100,100),matriz(100,100),matriz_coef(100),x(100),x_auxi(100),&
    & matriz_pa_Rich(100,100),matriz_coef_pa_rich(100),pivote
    real*8,external::sumatorio,sumatorio_jac,sumatorio_sneide_1,sumatorio_sneide_2
    character,external::Menu,comprobacion_demas,comprobacion_richa
    character resp
    integer i,j,col,fil,k,n,max,fil_1
    open(11,file='matriz.txt',status='old')
    open(13,file='matriz_coef.txt',status='old')
    open(12,file='Resolucion_Rich.txt',status='unknown')
    open(14,file='Resolucion_Jaco.txt',status='unknown')
    open(15,file='Resolucion_Snei.txt',status='unknown')
    !Leo las columnas de la matriz
    read(11,*)col,fil
    read(13,*)fil_1
    n=fil
    !Leo la matriz
    do i = 1, col
        read(11,*)(matriz(i,j),j=1,fil)
    end do
    do i = 1, fil
        read(13,*) matriz_coef(i)
    end do
    !Defino el valor que va a tomar N
    max=100000
    !Defino las matrices Q respectivas a cada metodo.
    do i  = 1, n
        Q_Rich(i,i)=1
        Q_Jac(i,i)=matriz(i,i)
    end do
    resp=Menu()
    do while(resp/='5')
        if ( resp=='1' ) then!Metodo de Richarsdson
            !Resolucion por Richardson
            if ( comprobacion_richa(matriz,fil,col) == 'n' ) then
                print*,'La matriz introducida no puede ser calculada mediante Richardson.'
            else if(comprobacion_richa(matriz,fil,col)=='m') then
                matriz_pa_Rich=matriz
                matriz_coef_pa_rich=matriz_coef
                do i = 1, n
                    pivote=matriz_pa_Rich(i,i)
                    do j = 1, n
                        matriz_pa_Rich(j,i)=(matriz_pa_Rich(j,i)/(pivote))
                    end do
                    matriz_coef_pa_rich(i)=matriz_coef_pa_rich(i)/pivote
                end do

                do i = 1, n
                    x(i)=0
                end do

                do k= 1, max
                    do i = 1, n
                        x(i)=x(i)-sumatorio(n,matriz_pa_Rich,x,i)+matriz_coef_pa_rich(i)
                    end do
                end do

                do i = 1, n
                    write(12,*) x(i)
                end do
            elseif(comprobacion_richa(matriz,fil,col) == 's') then
                matriz_pa_Rich=matriz
                matriz_coef_pa_rich=matriz_coef

                do i = 1, n
                    x(i)=0
                end do
                do k= 1, max
                    do i = 1, n
                        x(i)=x(i)-sumatorio(n,matriz_pa_Rich,x,i)+matriz_coef_pa_rich(i)
                    end do
                end do
                do i = 1, n
                    write(12,*) x(i)
                end do
            endif
            resp=Menu()
        else if ( resp=='2' ) then!Metodo de Jacobi
            if ( comprobacion_demas(matriz,fil,col)=='n' ) then
                print*,'La matriz introducida no converge mediante este metodo.'
            else
                do i = 1, n
                    x(i)=0
                end do
                do k = 1, max
                    do i = 1, n
                        x(i)=(1.d0/matriz(i,i))*(-sumatorio_jac(i,n,matriz,x)+matriz_coef(i))
                    end do
                    
                end do
                do i = 1, n
                    write(14,*) x(i)
                end do
            endif
            resp=Menu()
        elseif(resp=='3') then!Metodo Gauss-Sneider
            if ( comprobacion_demas(matriz,fil,col)=='n' ) then
                print*,'La matriz introducida no converge mediante este metodo.'
            else
                do i = 1, n
                    x(i)=0
                end do

                do k = 1, max
                    do i = 1, n
                        x(i)=(1.d0/matriz(i,i))*(matriz_coef(i)-sumatorio_sneide_1(i,n,matriz,x)-sumatorio_sneide_2(i,n,matriz,x))
                    end do
                end do
                do i = 1, n
                    write(15,*) x(i)
                end do
            endif
            resp=Menu()
        elseif(resp=='4') then
            print*,'opc4'
            resp=Menu()
        else
            print*,'Escogio un valor no valido.'
            resp=Menu()
        endif
    end do
end program Metodos_iterativos

function sumatorio(n,matriz,x,i) result(suma)
    implicit none
    real*8 suma,matriz(100,100),x(100)
    integer n,i,j
    suma=0.d0
    do j = 1, n
        suma=suma+(matriz(j,i)*x(j))
    end do
end function sumatorio
function sumatorio_jac(i,n,x,matriz) result(suma)
    implicit none
    real*8 matriz(100,100),x(100),suma
    integer i,j,n
    suma=0.d0
    do j = 1, n
        if ( i/=j ) then
            suma=suma+(matriz(j,i)*x(j))
        end if
    end do
end function
function sumatorio_sneide_1(i,n,matriz,x) result(suma)
    implicit none
    real*8 matriz(100,100),x(100),suma
    integer i,n,j
    suma=0.d0
    do  j= i+1, n
        suma=suma+(matriz(j,i)*x(j))
    end do
end function
function sumatorio_sneide_2(i,n,matriz,x) result(suma)
    real*8 matriz(100,100),x(100),suma
    integer i,n,j
    suma=0.d0
    do j = 1, i-1
        suma=suma+(matriz(j,i)*x(j))
    end do
end function
function comprobacion_richa(matriz,fil,col) result(respuesta)
    implicit none
    real*8 matriz(100,100),suma
    character respuesta
    integer i,j,fil,col
    respuesta='s'
    do i = 1, fil
        if (matriz(i,i)/=1) then
            respuesta='m'
        end if
    end do
    do i = 1, fil
        suma=0.d0
        do j = 1, col
            if ( j/=i ) then
                suma = suma+ abs(matriz(j,i))
                print*,suma
            end if
        end do
        if ( suma>1 ) then
            respuesta='n'
        end if
    end do
end function
function Menu() result(res)
    implicit none
    character res
    print*,"Menu:"
    print*,'1.-Resolver por Richardson'
    print*,'2.-Resolver por Jacobi'
    print*,'3.-Resolver por Gauss-Sneider'
    print*,'4.-Op'
    print*,'5.-Salir'
    print*,"Digame su eleccion: "
    read(*,*)res

end function Menu
function comprobacion_demas(matriz,fil,col) result(respuesta)
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
            respuesta='n'
        end if
    end do
end function comprobacion_demas