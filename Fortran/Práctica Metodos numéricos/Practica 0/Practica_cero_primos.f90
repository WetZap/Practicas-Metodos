program Practica_cero
    implicit none
    real*8 numeros_primos(0:1000)
    integer i,j,cont
    logical primo
    open (11,file="Numeros_primos.txt",status="unknown")
    cont=1
    i=1
    numeros_primos(0)=0
    do while (cont.LE.1001)
        do j = 2,i-1
           if  ((MOD(i,j)).EQ.0) then
                primo=.false.
           end if
        end do
        if (primo.EQV..TRUE.) then
            numeros_primos(cont)=i
            cont=cont+1
        end if
        i=i+1
        primo=.TRUE.
    end do 
    do i = 0, 1000
        write (11,*) numeros_primos(i)
    end do

end program Practica_cero