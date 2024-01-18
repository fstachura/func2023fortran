program main
    write (*,*), "write a number:"
    read (*,*), i
    if (i-1) 30, 30, 50
    50 if (i-2) 30, 40, 10

    10 do j=2, sqrt(real(i))
        if (mod(i, j) .EQ. 0) then 
            30 write (*, *), "number is not prime"
            goto 20
        end if
    end do

    40 write (*, *), "number is prime"
    20 a = 0
end program
