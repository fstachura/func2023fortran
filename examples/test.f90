program main
    write (*,*), +(-(+(-(+4))))                 ! 4
    write (*,*), (4-2*2+3)                      ! 3
    write (*,*), (4+(-2*2)+3)                   ! 3
    write (*,*), 6/3-1                          ! 1
    write (*,*), (4 - ((2 * 2) + 3))            ! -3
    write (*,*), ((4 - (2 * 2)) + 3)            ! 3
    write (*,*), 100+200/10-3*10                ! 90
    write (*,*), 2.0 + 3.0 * 2.0 - 1.0 / 4.0    ! 7.75
    write (*,*), 2 + 3 * 2 - 1 / 4 ** 8         ! 8
    write (*,*), .NOT. .FALSE. .OR. .TRUE. .AND. .FALSE. .EQV. .TRUE. .EQV. .FALSE. .OR. .TRUE. .AND. (.NOT. .TRUE.) ! False
end program

