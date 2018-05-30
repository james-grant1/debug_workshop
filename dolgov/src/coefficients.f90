!===================================================
! Functions for computing cross-section coefficients
! Auxiliary for create_matrices
!===================================================

function K1(x) result (v)
  real(kind=8), intent(in) :: x
  real(kind=8) :: v
  
  v = 1.176d-3 ! region 1
  if ((x>2d0/8).and.(x<=5d0/8)) v = 1.744d-3 ! region 2
  if ((x>5d0/8).and.(x<=7d0/8)) v = 1.758d-3 ! region 3
  if (x>7d0/8) v = 1.766d-3 ! region 4
end function K1
function K2(x) result (v)
  real(kind=8), intent(in) :: x
  real(kind=8) :: v
  
  v = 5.224d-4 ! region 1
  if ((x>2d0/8).and.(x<=5d0/8)) v = 5.576d-4 ! region 2
  if ((x>5d0/8).and.(x<=7d0/8)) v = 5.420d-4 ! region 3
  if (x>7d0/8) v = 5.380d-4 ! region 4
end function K2

function Sigma_a1(x) result (v)
  real(kind=8), intent(in) :: x
  real(kind=8) :: v
  
  v = 0.089 ! region 1
  if ((x>2d0/8).and.(x<=5d0/8)) v = 0.092 ! region 2
  if ((x>5d0/8).and.(x<=7d0/8)) v = 0.091 ! region 3
  if (x>7d0/8) v = 0.093 ! region 4
end function SIGMA_A1
function Sigma_a2(x) result (v)
  real(kind=8), intent(in) :: x
  real(kind=8) :: v
  
  v = 0.436 ! region 1
  if ((x>2d0/8).and.(x<=5d0/8)) v = 0.372 ! region 2
  if ((x>5d0/8).and.(x<=7d0/8)) v = 0.332 ! region 3
  if (x>7d0/8) v = 0.340 ! region 4
end function SIGMA_A2

function Sigma_s(x) result (v)
  real(kind=8), intent(in) :: x
  real(kind=8) :: v
  
  v = 0d0 ! region 1
  if ((x>2d0/8).and.(x<=5d0/8)) v = 0.0264 ! region 2
  if ((x>5d0/8).and.(x<=7d0/8)) v = 0.0228 ! region 3
  if (x>7d0/8) v = 0.0264 ! region 4
end function SIGMA_S

function Sigma_f1(x) result (v)
  real(kind=8), intent(in) :: x
  real(kind=8) :: v
  
  v = 0d0 ! region 1
  if ((x>2d0/8).and.(x<=5d0/8)) v = 0.140 ! region 2
  if ((x>5d0/8).and.(x<=7d0/8)) v = 0.109 ! region 3
  if (x>7d0/8) v = 0.107 ! region 4
end function SIGMA_F1
function Sigma_f2(x) result (v)
  real(kind=8), intent(in) :: x
  real(kind=8) :: v
  
  v = 0.0079 ! region 1
  if ((x>2d0/8).and.(x<=5d0/8)) v = 0.0156 ! region 2
  if ((x>5d0/8).and.(x<=7d0/8)) v = 0.0159 ! region 3
  if (x>7d0/8) v = 0.0157 ! region 4
end function SIGMA_F2
