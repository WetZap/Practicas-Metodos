program Euler
    implicit none
    real*8 y_in,x,y,x_in,h,max_x,y_ant
    real*8,external::funcion,func_anali
    integer i,j,limite_inf,limite_sup
    x_in=0
    y_in=-2
    h=0.05d0
    limite_inf=0.d0
    limite_sup=2.d0
    max_x=(limite_sup-limite_inf)/h
    y_ant=y_in
    x=x_in
    print*,"||            x              || y(x) Analitica            || y(x) Numerica             ||  Error Relativo           ||"
    print*,"||",x,"||",func_anali(x),'||',y_in,"||",y_in-func_anali(x),'||'
    do i = 1,int(max_x)
        y=y_ant+h*funcion(x,y_ant)
        print*,"||",x+h,"||",func_anali(x+h),'||',y,"||",abs(abs(func_anali(x+h))-abs(y)),'||'
        x=x+h
        y_ant=y
    end do

end program Euler
function funcion(x,y) result(valor)
    implicit none
    real*8 x,y,valor
    valor=-2*x**2-3*y
end function funcion
function func_anali(x) result(valor)
    implicit none
    real*8 x,valor
    valor=-(50.d0/(27*1.d0))*exp(-3*x)-(2.d0/27.d0)*(9*x**2-6*x+2)
end function func_anali