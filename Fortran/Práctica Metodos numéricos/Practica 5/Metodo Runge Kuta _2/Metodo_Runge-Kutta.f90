program Euler
    implicit none
    real*8 y_in,x,y,x_in,h,max_x,y_ant,k_1,k_2,k_3,k_4
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
        k_1 =h*funcion(x,y_ant)
        k_2 =h*funcion(x+(h/2.d0),y_ant+(k_1/2.d0))
        k_3 =h*funcion(x+(h/2.d0),y_ant+(k_2/2.d0))
        k_4 =h*funcion(x+h,y_ant+k_3)

        y=y_ant+(k_1/6.d0)+(k_2/3.d0)+(k_3/3.d0)+(k_4/6.d0)
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
