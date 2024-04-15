program Practica_1
    implicit none
    real*8 min_c(2,0:100),lag(2,0:100),new(2,0:100),x(0:3),y(0:3)
    integer i
    open(11,file='mincuad.dat',status='old')
    open(12,file='lagrange.dat',status='old')
    open(13,file='Newton.dat',status='old')
    open(14,file='Tabla_Valores.txt',status='old')
    read(11,*) min_c
    read(12,*) lag
    read(13,*) new
    read(14,*)x,y
    print*,"  Min. Cua/Lagr-------------Min.Cua/Newt--------------Newt/Lagr--------------"
    do i=0,100
        print*,-min_c(2,i)-(-lag(2,i)),-min_c(2,i)-(-new(2,i)),-new(2,i)-(-lag(2,i))
    end do
    print*,"_____________________________________________________________________________"
    print*,"  Val.Real/Lagr-------------Val.Real/Newt-------------Val.Real/Min.Cua-------"
    do i=0,100
        if (i==1 ) then
            print*,-y(0)-(-lag(2,i)),-y(0)-(-new(2,i)),-y(0)-(-min_c(2,i))
        elseif(i==26) then
            print*,-y(1)-(-lag(2,i)),-y(1)-(-new(2,i)),-y(1)-(-min_c(2,i))
        elseif(i==76) then
            print*,-y(2)-(-lag(2,i)),-y(2)-(-new(2,i)),-y(2)-(-min_c(2,i))
        elseif(i==100) then
            print*,-y(3)-(-lag(2,i)),-y(3)-(-new(2,i)),-y(3)-(-min_c(2,i))
        endif
    end do

end program Practica_1