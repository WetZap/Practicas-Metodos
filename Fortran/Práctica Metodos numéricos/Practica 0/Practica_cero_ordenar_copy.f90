program practica_cero
    implicit none
    real*8 lista(0:9),lista_ord(0:9),menor,menor_ant
    logical acabo
    integer i,j
    open (11,file="Lista.txt",status="old")
    open (12,file="Lista_orde.txt",status="unknown")
    read(11,*) lista
    acabo=.false.
    j=0
    do while (acabo.eqv..false.) 
        menor=1000000
        do i=j,9
            if (lista(i)==menor_ant)then
                
            end if  
            if ( lista(i)<menor ) then
                menor = lista(i)
            end if
        end do
        print*,menor
        lista_ord(j)=menor
        if ( j==9 ) then
            acabo=.TRUE.
        end if
        j=j+1
        menor_ant=menor
    end do
    write(12,*)lista_ord
end program practica_cero