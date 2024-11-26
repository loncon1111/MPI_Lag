! Main program for finding the eigenvalues and eigenvectors of 
! a symmetric matrix AO (N x N) with Jacobi's method. 
! ITMAX and EPS are the stopping criteria, namely the maximum number of
! iterations and error tolerance, respectively.

! eigenvalues o tren duong cheo chinh cua ma tran (diagonal of the matrix)  AO(i,i)
! eigenvectors ma tran A(i,j)

! Method: Calls Jacobi

! This subroutine performs the Jacobian
!==============================================================================
! Evaluate the eigenvalues and eigenvectors of a real symmetric maxtrix a(n,n):
! ao*a = lambda*a
! method: Jacoby method for symmetric matrices
!-----------------------------------------------------------------------------
! input ...
! ao(n,n) - array of coefficients for matrix A
! n      - number of equations
! abserr - abs tolerance [sum of (off-diagonal elements)^2]
! output ...
! ao(i,i) - eigenvalues
! a(i,j) - eigenvectors
!===========================================================================

subroutine jacobi (ao,n,a,eps,itmax)
        double precision, dimension:: ao(n,n), a(n,n)
        double precision :: eps
        integer          :: n, itmax

        iter = 0.0
        do 10 i = 1,n
        do 10 j = 1,n
           a(i,j) = 0.0
10         a(i,i) = 1.0

20      z = 0.
        nm1 = n - 1
        do 30 i = 1, nm1
           ip1 = i + 1
           do 30 j = ip1, n
              if (abs(ao(i,j)) .le. z) go to 30
              z = abs(ao(i,j)
              irow = i
              icol = j
30      continue

        if (iter .eq. 0) y = z*eps
        if (z .lt. y) go to 200
        dif  = ao(irow,irow) - ao(icol,icol)
        tang = (-dif+sqrt(dif**2 + 4.0*z**2))/(2.0*ao(irow,icol))
        cose = 1.0 /sqrt(1.0 + tang**2)
        sine = cose*tang

        do 40 i = 1,n
           zz = a(i,irow)
           a(i,irow) = cose*zz + sine*a(i,icol)
40         a(i,icol) = cose*a(i,icol) - sine*zz

        i = 1  
50      if (i .eq. irow) go to 60
           yy = ao(i,irow)
           ao(i,irow) = cose*yy + sine*ao(i,icol)
           ao(i,icol) = cose*ao(i,icol) - sine*yy
           i = i + 1
        go to 50
60      i = irow + 1

70      if (i .eq. icol) go to 80
           yy = ao(irow,i)
           ao(irow,i) = cose*yy + sine*ao(i,icol)
           ao(i,icol) = cose*ao(i,icol) - sine*yy
        i = i + 1
        go to 70

80      i = icol + 1
90      if (i .gt. n) go to 100
           zz = ao(irow,i)
           ao(irow,i) = cose*zz + sine*ao(icol,i)
           ao(icol,i) = cose*ao(icol,i) - sine*zz
        i = i + 1
        go to 90

100     yy = ao(irow,irow)
        ao(irow,irow) = yy*cose**2 + ao(irow,icol) * 2.0*cose*sine +
ao(icol,icol)* sine**2
        ao(icol,icol) = ao(icol,icol)*cose**2 + yy*sine**2 -
ao(irow,icol)*2.)*cose*sine

        iter = iter + 1
        if (iter .lt. itmax) go to 20

200     return
        end
  
