program Practica_0
    real*8 suma,produc
    integer i,j


    suma=1.d0
    do i=1,7000
        produc=1.d0
        do j=1,i
            produc=produc*j
            !print*,j
        end do
        !print*,suma
        suma=suma+(1/produc)
    end do
    print *,suma
end program Practica_0
