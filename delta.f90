program main
    read *,a,b,c
    delta = 4*a*b
    if (delta) 30, 20, 10
10  print *,((-b-sqrt(delta))/2*a),((-b+sqrt(delta))/2*a)
    goto 40
20  print *,(-b/2*a) 
    goto 40
30  print *,"no solutions"
40  end program main
