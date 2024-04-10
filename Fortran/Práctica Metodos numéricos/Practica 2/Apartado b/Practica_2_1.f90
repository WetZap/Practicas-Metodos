function funcion(x) result(valor)!Esta funcion es la funcion que nos piden integrar
    implicit none
    real*8 :: x,valor
    valor=1.d0/(1.d0+x)
end function

program Practica_2_2_1
    implicit none
    real*8 Coeficientes_cotes(0:9,8),suma,a,b,integral,Coeficientes_cotes_fin(0:9),h,x
    real*8,external :: funcion
    integer n,i
    !Declaramos las variables que tomaran valores
    a=0.d0
    b=1.d0
    n=8
    h=(b-a)/n
    !Abrimos el archivo de texto y lo leemos
    open(11,file="Coeficientes_cotes.txt",status="old")
    read(11,*)Coeficientes_cotes
    !Lo que hacemos aqui es calcular el H que necesitamos recorriendo la matriz de coeficientes
    do i=0,8
        Coeficientes_cotes_fin(i)=Coeficientes_cotes(i,n)/Coeficientes_cotes(n+1,n)
    end do
    !Aplicamos la formula del calculo integral
    suma=0.d0
    do i=0,8
        x=a+i*(((b-a)*1.d0)/n)
        suma=suma+Coeficientes_cotes_fin(i)*funcion(x)
    end do
    integral=(b-a)*suma
    print *,"La integral es: ",integral
end program Practica_2_2_1