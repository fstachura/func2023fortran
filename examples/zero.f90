program main
    a = -100.0
    b = 100.0

10  if (abs(a-b) - 0.00001) 100, 100, 20
20  x1 = (a+b)/2
    fx1 = (4.5*(x1**5)-2*(x1**4)+8.7*(x1**2)-x1+4)
    fa = (4.5*(a**5)-2*(a**4)+8.7*(a**2)-a+4)
    if (abs(fx1) <= 0.00001) then
        go to 100
    else if (fx1*fa < 0) then
        b = x1
    else
        a = x1
    end if
    goto 10

!20  x1 = (a+b)/2
!    fx1 = (4.5*(x1**5)-2*(x1**4)+8.7*(x1**2)-x1+4)
!    if (abs(fx1) - 0.00001) 100, 100, 21
!21  write (*,*), a, b, fx1, (abs(a-b) - 0.00001)
!    fa = (4.5*(a**5)-2*(a**4)+8.7*(a**2)-a+4)
!    if (fa*fx1) 30, 40, 40
!30  b = x1; goto 10
!40  a = x1; goto 10
!
100 c = 2
write (*,*), "result:", (a+b)/2, fx1, (abs(a-b) - 0.00001), (abs(fx1) - 0.00001) 



end program
