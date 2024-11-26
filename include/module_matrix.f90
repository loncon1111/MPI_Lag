module matrix

implicit none

contains
!-- 
!--Calculate eigenvectors and eigenvalues of a matrix 
!--

! Subroutine to calculate determinant of a matrix

! Subroutine to calculate eigenvalues and eigenvectors

subroutine jacobian(ao,x,abserr,n)
! This subroutine performs the Jacobain
!==============================================================================
! Evaluate the eigenvalues and eigenvectors of a real symmetric maxtrix a(n,n):
! a*x = lambda*x
! method: Jacoby method for symmetric matrices
!-----------------------------------------------------------------------------
! input ...
! a(n,n) - array of coefficients for matrix A
! n      - number of equations
! abserr - abs tolerance [sum of (off-diagonal elements)^2]
! output ...
! a(i,j) - eigenvalues
! x(i,j) - eigenvectors
!==============================================================================

