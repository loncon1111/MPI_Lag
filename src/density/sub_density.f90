real function lmstolm(phis,lams,polphi,pollam)

! LMSTOLM - calculate the true geographical length for a point with the
! coordinate (phis,alms) in the rotating system. The system's north pole
! true coordinate (polphi,pollam)

! call : LAM = lmstolm (phis,lams,polphi,pollam)

! purpose: calculating the true geographical length for a point with
! coordinate (phis,lams) in the rotating system. The system's north pole
! true coordinate (polphi,pollam)

! parameter:
!      PHIS   real   geographical width of the point in rotating system
!      LAMS   real   geographical length of the point in rotating system
!      POLSPHI real  true geogr. width of the north pole
!      POLLAM  real  true geogr. length of the north pole

! output:
!      parameter: true geographical length as the value of function
!      angle in degrees (north > 0, east > 0)

    real                :: lams,phis,polphi,pollam
    real,parameter      :: zrpi18 = 57.2957795
    real,parameter      :: zpir18 = 0.0174532925

    zsinpol = sin(zpir18*polphi)
    zcospol = cos(zpir18*polphi)
    zlampol = zpir18 * pollam
    zphis   = zpir18 * phis
    zlams   = lams

    if (zlams .gt. 180.) zlams = zlams - 360.
    zlams = zpir18 * zlams

    zarg1 = sin(zlampol)*(- zsinpol*cos(zlams)*cos(zphis) + &
                            zcospol*           sin(zphis) - &
                                    sin(zlams)*cos(zphis)) 

    zarg2 = cos(zlampol)*(- zsinpol*cos(zlams)*cos(zphis) + &
                            zcospol*           sin(zphis) - &
                                    sin(zlams)*cos(zphis))

    if ( abs(zarg2) .lt. 1.e-30 ) then
       if ( abs(zarg1) .lt. 1.d-30 ) then
          lmstolm = 0.
       else if ( zarg1 .gt. 0. ) then
          lmstolam = 90.
       else
          lmstolam = -90.
       end if
    else
       lmstolm = zrpi18*atan2(zarg1,zarg2)
    end if

    return
end function lmstolm

real function phstoph(phi,lam,polphi,pollam)

! PHSTOPH - calculate the true geographical width for a point with the
! coordinate (phi,lam) in the rotating system. The system's north pole
! true coordinate (polphi,pollam)

! call : PHI = phstoph (phi,lam,polphi,pollam)

! purpose: calculating the true geographical width for a point with
! coordinate (phi,lam) in the rotating system. The system's north pole
! true coordinate (polphi,pollam)

! parameter:
!      PHI   real   geographical width of the point in rotating system
!      LAM   real   geographical length of the point in rotating system
!      POLPHI real  true geogr. width of the north pole
!      POLLAM  real  true geogr. length of the north pole

! output:
!      parameter: true geographical length as the value of function
!      angle in degrees (north > 0, east > 0)
    real :: lam, phi, polphi, pollam

    real,parameter      :: zrpi18 = 57.2957795
    real,parameter      :: zpir18 = 0.0174532925

    zsinpol = sin(zpir18*polphi)
    zcospol = cos(zpir18*polphi)
    zlampol = zpir18 * pollam
    zphi    = zpir18 * phi
    zlam    = lam

    if (zlam .gt. 180.) zlam = zlam - 360.
    zlam    = zpir18 * zlam

    zarg1   = - sin(zlam-zlampol)*cos(zphi)
    zarg2   = - zsinpol*cos(zphi)*cos(zlam-zlampol)+zcospol*sin(zphi)

    if (abs(zarg2) .lt. 1.e-30) then
       if (abs(zarg1) .lt. 1.e-30) then
          lmtolms = 0.
       else if (zarg1.gt.0.( then
          lmtolms = 90.
       else
          lmtolms = -90.
       end if
    else
       lmtolms = zrpi18*atan2(zarg1,zarg2)
    end if
    return

end function phstoph

real function LMTOLMS (PHI, LAM, POLPHI, POLLAM)
      REAL        LAM,PHI,POLPHI,POLLAM

      DATA        ZRPI18 , ZPIR18  / 57.2957795 , 0.0174532925 /

      ZSINPOL = SIN(ZPIR18*POLPHI)
      ZCOSPOL = COS(ZPIR18*POLPHI)
      ZLAMPOL =     ZPIR18*POLLAM
      ZPHI    =     ZPIR18*PHI
      ZLAM    = LAM
      IF(ZLAM.GT.180.0) ZLAM = ZLAM - 360.0
      ZLAM    = ZPIR18*ZLAM

      ZARG1   = - SIN(ZLAM-ZLAMPOL)*COS(ZPHI)
      ZARG2   = - ZSINPOL*COS(ZPHI)*COS(ZLAM-ZLAMPOL)+ZCOSPOL*SIN(ZPHI)
      IF (ABS(ZARG2).LT.1.E-30) THEN
        IF (ABS(ZARG1).LT.1.E-30) THEN
          LMTOLMS =   0.0
        ELSEIF (ZARG1.GT.0.) THEN
              LMTOLMS =  90.0
            ELSE
              LMTOLMS = -90.0
            ENDIF
      ELSE
        LMTOLMS = ZRPI18*ATAN2(ZARG1,ZARG2)
      ENDIF

      RETURN
      END

REAL FUNCTION PHTOPHS (PHI, LAM, POLPHI, POLLAM)
      REAL        LAM,PHI,POLPHI,POLLAM

      DATA        ZRPI18 , ZPIR18  / 57.2957795 , 0.0174532925 /

      ZSINPOL = SIN(ZPIR18*POLPHI)
      ZCOSPOL = COS(ZPIR18*POLPHI)
      ZLAMPOL = ZPIR18*POLLAM
      ZPHI    = ZPIR18*PHI
      ZLAM    = LAM
      IF(ZLAM.GT.180.0) ZLAM = ZLAM - 360.0
      ZLAM    = ZPIR18*ZLAM
      ZARG    = ZCOSPOL*COS(ZPHI)*COS(ZLAM-ZLAMPOL) + ZSINPOL*SIN(ZPHI)

      PHTOPHS = ZRPI18*ASIN(ZARG)

      RETURN
      END

!------------------------------------------------------------------
!Compute Cos/Sin of an argument in Degree instead of Radian
! ------------------------------------------------------------------

      real function cosd(arg)

      real,intent(IN) :: arg
      real,parameter :: grad2rad=3.1415926/180.
      cosd=cos(arg*grad2rad)
      return
      end

      real function sind(arg)

      real,intent(IN) :: arg
      real,parameter :: grad2rad=3.1415926/180.
      sind=sin(arg*grad2rad)
      return
      end
