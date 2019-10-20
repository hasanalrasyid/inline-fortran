      program main
        implicit none
        integer :: isq, icub,i,j
        integer :: v(2,3)
        call square_cube(4,isq,icub,0)
        do 300 i = 1,2
        do 301 j = 1,3
            v(i,j) = 10*i + j
  301     continue
  300   continue
        print *, "i,i^2,i^3=",4,isq,icub
        do 400 i = 1,2
        do 401 j = 1,3
            print*,"test v:", v(i,j)
  401     continue
  400   continue

      end program main
