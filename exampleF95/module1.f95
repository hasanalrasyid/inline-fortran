module module1
  implicit none
contains
  real function calc_it(x,y)
    real , intent(in):: x,y
    calc_it = sqrt(x**2 + y**2)
  end function calc_it
end module module1
