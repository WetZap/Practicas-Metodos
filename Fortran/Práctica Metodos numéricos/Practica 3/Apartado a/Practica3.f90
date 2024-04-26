program Apartado_a
    implicit none
    real*8 matriz(100,100)
   ! character(len=693)formato 
    integer i,j,col,fil
    open(11,file='matriz.txt',status='old')
    open(12,file='matriz_2.txt',status='unknown')
    read(11,*)col,fil
    !formato=''
    !do i = 0, fil
        !formato=formato+"1x,F6.3"
   ! end do
    do i = 1, col
            read(11,*)(matriz(i,j),j=1,fil)
    end do
    do i = 1, col
        write(12,*)(matriz(i,j),j=1,fil)

end do
    



!100 Format(1x,F6.3)
end program Apartado_a