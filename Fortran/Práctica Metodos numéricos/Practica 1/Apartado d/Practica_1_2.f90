!Funcion que recibe como argumento un entero y un array que tiene dimension 4
function Prod_x(j,x) result(prod)!Esta funcion lo que hace es el productorio que sale en la definicion de $c_k$
    implicit none
    integer j,k
    real*8 x(0:3),prod
    prod=1.d0
    do k = 0, j-1
        prod=prod*(x(j)-x(k))
    end do
end function
!Funcion que recibe como argumento un entero, un array que tiene dimension 4 y un valor real (en este caso toma valores entre
!0,4 y 0,8)
function Productorio(k,x,t) result(prod)!Esta funcion es la encargada de realizar el productorio de la formula $p_l(x)$ donde x toma
    implicit none                       !el valor de t
    integer k,j
    real*8 x(0:3),prod,t
    prod=1.d0
    do j = 0, k-1
        prod=prod*(t-x(j))
    end do
end function

program Practica_1
    implicit none!Definimos los tipos de variables y las funciones
    real*8 x(0:3),y(0:3),polinomio(0:3),coeficientes(0:3),resultado,t,stop
    real*8,external::Prod_x,Productorio
    integer i,j,k
    !Abrimos los archivos que vamos a usar.
    open(11,file='Tabla_Valores.txt',status='old')
    open(12,file='Newton.dat',status='unknown')
    !En esta linea leemos los valores que toman x e y que son tomados de la tabla
    read(11,*) x,y
    !Iniciamos el bucle que se encargara de tomar el valor de cada x que queremos interpolar
    do i=0,100
        t=0.4 + i*(0.4/100)
        !Inicializamos los valores correspondientes al primer elemento del array.
        coeficientes(0)=y(0)
        polinomio(0)=coeficientes(0)
        resultado=coeficientes(0)
        !Iniciamos el bucle que oscilara entre 1 y 3,correspondiendose a los grados del polinomio
        do j = 1, 3
            !Definimos el coeficiente de j como se expone en la formula
            coeficientes(j)=(y(j)-polinomio(j-1))/(Prod_x(j,x))
            !Aqui el polinomio de j toma el valor que le corresponde al anterior más el ultimo termino añadido, esto sirve para que 
            !se pueda usar polinomio(j-1) en la anterior sentencia, ya que toma el valor de sustituirse.
            polinomio(j)=polinomio(j-1)+coeficientes(j)*Productorio(j,x,t)
            !El resultado se escribe como la fomula expuesta en la teoria
            resultado=resultado+coeficientes(j)*Productorio(j,x,t)
        end do
        !Guardamos el valor en el archivo correspondiente.
        write(12,*) t, resultado
    end do
    read(*,*) stop
end program Practica_1