c     Main program for finding the eigenvalues and eigenvectors
c     of a symmetric matrix AO (NxN) with Jacobi's method. ITMAX
c     and EPS are the stopping criteria, namely the maximum number
c     of iterations and error tolerance,respectively.
      DIMENSION  AO(3,3), A(3,3)
      DATA N,ITMAX, EPS /3,100,1.0E-06/
      AO(1,1)=2.0
      AO(1,2)=8.0
      AO(1,3)=10.
      AO(2,1)=8.0
      AO(2,2)=3.0
      AO(2,3)=4.0
      AO(3,1)=10.
      AO(3,2)=4.
      AO(3,3)=7.0
      CALL JACOBI(AO,N,A,EPS,ITMAX)
      PRINT 10, (AO(I,I),I=1,N),((A(I,J), J=1,N), I=1,N)
10    FORMAT (1X,'EIGENVALUES ARE',/,3X,3E18.8,//3X,
     # 'EIGENVECTORS',/,8X,' FIRST',15X,' SECOND',13X,'THIRD',
     # /,(3X,3E18.8))
      STOP
      END
C **********************************
      SUBROUTINE JACOBI (AO,N,A,EPS,ITMAX)
      DIMENSION  AO(N,N), A(N,N)
      ITER=0
      DO 10 I=1,N
      DO 10 J=1,N
      A(I,J)=0.0
10    A(I,I)=1.0
20    Z=0.
      NM1=N-1
      DO 30 I=1,NM1
      IP1=I+1
      DO 30 J=IP1,N
      IF (ABS(AO(I,J)) .LE. Z) GO TO 30
      Z=ABS(AO(I,J))
      IROW=I
      ICOL=J
30    CONTINUE
      IF (ITER .EQ. 0) Y=Z*EPS
      IF (Z .LT. Y) GO TO 200
      DIF=AO(IROW,IROW)-AO(ICOL,ICOL)
      TANG= (-DIF+SQRT(DIF**2+4.0*Z**2))/(2.0*AO(IROW,ICOL))
      COSE=1.0/SQRT(1.0+TANG**2)
      SINE=COSE*TANG
      DO 40 I=1,N
      ZZ=A(I,IROW)
      A(I,IROW)=COSE*ZZ+SINE*A(I,ICOL)
40    A(I,ICOL)=COSE*A(I,ICOL)-SINE*ZZ
      I=1
50    IF (I .EQ. IROW) GO TO 60
      YY=AO(I,IROW)
      AO(I,IROW)=COSE*YY+SINE*AO(I,ICOL)
      AO(I,ICOL)=COSE*AO(I,ICOL)-SINE*YY
      I=I+1
      GO TO 50
60    I=IROW+1
70    IF (I .EQ. ICOL)  GO TO 80
      YY=AO(IROW,I)
      AO(IROW,I)=COSE*YY+SINE*AO(I,ICOL)
      AO(I,ICOL)=COSE*AO(I,ICOL)-SINE*YY
      I=I+1
      GO TO 70
80    I=ICOL+1
90    IF (I .GT. N) GO TO 100
      ZZ=AO(IROW,I)
      AO(IROW,I)=COSE*ZZ+SINE*AO(ICOL,I)
      AO(ICOL,I)=COSE*AO(ICOL,I)-SINE*ZZ
      I=I+1
      GO TO 90
100   YY=AO(IROW,IROW)
      AO(IROW,IROW)=YY*COSE**2+AO(IROW,ICOL)*2.0*COSE*SINE+
     # AO(ICOL,ICOL)*SINE**2
      AO(ICOL,ICOL)=AO(ICOL,ICOL)*COSE**2+YY*SINE**2-AO(IROW,ICOL)
     # *2.0*COSE*SINE
      AO(IROW,ICOL)=0.0
      ITER=ITER+1
      IF (ITER .LT. ITMAX) GO TO 20
200   RETURN
      END
