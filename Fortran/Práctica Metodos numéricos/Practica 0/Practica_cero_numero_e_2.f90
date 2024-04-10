program practica_cero
    real*8 valor
    integer i
valor=0.0d0
do i =0,1000
    valor=(1.d0+1.d0/i)**i
    print*,valor
end do


end program practica_cero