program Descomposicion
    implicit none
    real*8 matriz(100,100),L(100,100),U(100,100)
    integer i,j,col,fil,n
    open(11,file='matriz.txt',status='old')
    open(12,file='Matriz_L.txt',status='unknown')
    open(13,file='Matriz_U.txt',status='unknown')


    read(11,*)col,fil
    n=fil
    do i = 1, col
        read(11,*)(matriz(i,j),j=1,fil)
    end do

    do i = 1, n
        do j=1,n
            if ( j==i ) then
                L(i,i)=1
            elseif(j>i)then
                L(i,j)=0  
            else
                L(i,j)=(matriz(i,j)/matriz(j,j))
            end if
        end do
    end do
    do i = 1, n
        do j=1,n
            if ( j<i ) then
                U(i,j)=0
            else
                U(i,j)=matriz(i,j)
            end if
        end do
    end do
    do i = 1, col
        write(12,*)(L(i,j),j=1,fil)
    enddo
    do i = 1, col
        write(13,*)(U(i,j),j=1,fil)
    enddo
end program Descomposicion