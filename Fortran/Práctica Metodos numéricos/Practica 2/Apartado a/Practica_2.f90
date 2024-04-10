program Practica_2_2
    implicit none
    real*8 valor_ini,valor_fin,valor_x(0:8),valor_inter(1:8),integral,incremento,suma_medio,suma_nor
    integer n,i
    n=8
    valor_fin=1.d0
    valor_ini=0.d0
    incremento=(valor_fin-valor_ini)/n
    do i=0,8
        valor_x(i)=valor_ini+i*incremento
    end do

    valor_inter(1)=(valor_x(1)-valor_x(0))/2
    do i = 2, 7
        valor_inter(i)=(valor_x(i+1)-valor_x(i))/2
    end do
    do i=1,8
        suma_medio=suma_medio+1.d0/1+valor_inter(i)
    end do
    do i = 1,7
        suma_nor=suma_nor+ 1.d0/(1+valor_x(i))  
    end do
    integral=(incremento/6)*(1.d0/(1+valor_ini)+4*suma_medio+2*suma_nor+1.d0/(1+valor_fin))
    print*, "La integral es: ",integral
end program Practica_2_2