function Prod_x(j,x) result(prod)
    implicit none
    integer j,k
    real*8 x(0:3),prod
    prod=1.d0
    do k = 0, j-1
        prod=prod*(x(j)-x(k))
    end do
end function

function Productorio(k,x,t) result(prod)
    implicit none
    integer k,j
    real*8 x(0:3),prod,t
    prod=1.d0
    do j = 0, k-1
        prod=prod*(t-x(j))
    end do
end function

program Practica_1
    implicit none
    real*8 x(0:3),y(0:3),polinomio(0:3),coeficientes(0:3),resultado,t
    real*8,external::Prod_x,Productorio
    integer i,j,k
    open(11,file='Tabla_Valores.txt',status='old')
    open(12,file='Newton.dat',status='unknown')
    read(11,*) x,y
    do i=0,100
        t=0.4 + i*(0.4/100)
        coeficientes(0)=y(0)
        polinomio(0)=coeficientes(0)
        resultado=coeficientes(0)
        do j = 1, 3
            coeficientes(j)=(y(j)-polinomio(j-1))/(Prod_x(j,x))
            polinomio(j)=polinomio(j-1)+coeficientes(j)*Productorio(j,x,t)
            resultado=resultado+coeficientes(j)*Productorio(j,x,t)
           
        end do
        write(12,*) t, resultado
    end do

end program Practica_1