program main
    write (*,*), "a: "
    read (*,*), a
    write (*,*), "b: "
    read (*,*), b
    write (*,*), "c: "
    read (*,*), c
    
    delta = (b**2)-(4*a*c)

    if (delta > 0) then 
        write (*,*), (-b-sqrt(delta))/(2*a), (-b+sqrt(delta))/(2*a)
    else if (delta .EQ. 0) then
        write (*,*), -b/2*a
    else 
        write (*,*), "no roots"
    end if
end program
