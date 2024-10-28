program Practica_1
    implicit none
    real*8 valor_x(0:3),valor_y(0:3)
    integer i
    open(11,file='Valores.txt',status="unknown")
    
    print*,"Introduzcamos los datos: "
    do i=0,3
        print*,"Diga el valor",i+1,"de x: "
        read(*,*) valor_x(i)
        print*,"Diga el valor",i+1,"de y: "
        read(*,*) valor_y(i)
    end do
    write(11,*)valor_x,valor_y

end program Practica_1