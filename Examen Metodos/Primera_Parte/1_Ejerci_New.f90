!Funcion que recibe como argumento un entero y un array que tiene dimension 5
function Prod_x(j,x) result(prod)
    implicit none
    integer j,k
    real*8 x(0:0),prod
    prod=1.d0
    do k = 0, j-1
        prod=prod*(x(j)-x(k))
    end do
end function
!Funcion que recibe como argumento un entero, un array que tiene dimension 5,
function Productorio(k,x,t) result(prod)!
    implicit none                       !
    integer k,j
    real*8 x(0:3),prod,t
    prod=1.d0
    do j = 0, k-1
        prod=prod*(t-x(j))
    end do
end function

program Ejer_1_Ne
    implicit none!Definimos los tipos de variables y las funciones
    real*8 x(0:4),y(0:4),polinomio(0:4),coeficientes(0:4),resultado,t,stop
    real*8,external::Prod_x,Productorio
    integer i,j,k
    !Abrimos los archivos que vamos a usar.
    open(11,file='valores.txt',status='old')
    !En esta linea leemos los valores que toman x e y que son tomados de la tabla expuesta en el examen.
    read(11,*) x,y
    !Iniciamos el bucle que se encargara de tomar el valor de cada x que queremos interpolar
    do i=0,10
        t=i*(1.d0/10)
        !Inicializamos los valores correspondientes al primer elemento del array.
        coeficientes(0)=y(0)
        polinomio(0)=coeficientes(0)
        resultado=coeficientes(0)!Este paso es sumamente importante porque si no iniciasemos los valores al primer valor de y
                                 !Este tomara valores erroneos
        !Iniciamos el bucle que oscilara entre 1 y 4,corresponde a los grados del polinomio
        do j = 1, 4
            !Definimos el coeficiente de j como se expone en la formula
            coeficientes(j)=(y(j)-polinomio(j-1))/(Prod_x(j,x))
            !Aqui el polinomio de j toma el valor que le corresponde al anterior más el ultimo termino añadido, esto sirve para que 
            !se pueda usar polinomio(j-1) en la anterior sentencia, ya que toma el valor que le corresponderia de sustituirse.
            !Esto lo hago así porque como no se usar recursividad, y además, no se si Fortran tiene recursividad siquiera.
            polinomio(j)=polinomio(j-1)+coeficientes(j)*Productorio(j,x,t)
            !El resultado se escribe como la fomula expuesta en la teoria
            resultado=resultado+coeficientes(j)*Productorio(j,x,t)
        end do
        !Imprimimos por pantala el archivo.
        print*,t,resultado
    end do
end program Ejer_1_Ne