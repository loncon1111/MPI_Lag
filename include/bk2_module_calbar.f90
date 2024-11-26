module calvar
    contains
! ----------------------------------------------------------------------
! Potential temperature (TH)
! ----------------------------------------------------------------------

    subroutine calc_TH (pt,t,p)

        implicit none
        
        ! Argument declaration
        real    :: pt           ! Potential temperature (K)
        real    :: t            ! Temperature (either in C or K)
        real    :: p            ! Pressure (hPa)

        ! Physical parameters
        real    :: rdcp,tzero
        data    rdcp,tzero /0.286,273.15/

        ! Calculation - distinction between temperature in K and in C
        if ( t .lt. 100. ) then
             pt = (t + tzero) * ( (1000./p) ** rdcp )
        else
             pt = t * ( (1000./p) ** rdcp )
        end if

    end subroutine calc_TH


! ----------------------------------------------------------------------
! Density (RHO)
! ----------------------------------------------------------------------

    subroutine calc_RHO (rho,t,p)

        implicit none

        ! Argument declaration
        real    :: rho          ! Density [kg/m^3]
        real    :: t            ! Temperature (either in C or K(
        real    :: p            ! Pressure (hPa)

        ! Physical parameters
        real    :: rd,tzero
        data    rd,tzero /287.05,273.15/

        ! Auxiliary variables
        real    :: tk

        ! Calculation - distinction between temperature in K and in C
        if ( t .lt. 100. ) then
             tk = t + tzero
        else
             tk = t
        end if

        rho = 100. * p/ (tk * rd)

    end subroutine calc_RHO

! ----------------------------------------------------------------------
! Relative humidity (RH)
! ----------------------------------------------------------------------

    subroutine calc_RH (rh,t,p,q)

        implicit none
        
        ! Argument declaration
        real    :: rh           ! Relative humidity (%)
        real    :: t            ! Temperature (either in C or K)
        real    :: p            ! Pressure (hPa)
        real    :: q            ! Specific humidity

        ! Physical parameters
        real    :: rdcp,tzero
        data    rdcp,tzero /0.286,273.15/
        real    :: es0,lv,t0
        data    es0,lv,t0 /6.1078,2.5e06,273.16/
        real,parameter :: rd=287.05,rv=461.51,eps=rd/rv

        ! Auxiliary variables
        real    :: es
        real    :: qs,ws
        real    :: tk
        real    :: qk

        ! Calculation - distintion between temperature in K and in C
        if ( t .lt. 100. ) then
             tk = t + tzero
        else
             tk = t
        end if

        qk = q
        es = es0 * exp( lv/rv*(1./t0 - 1/tk) ) 
        ws = eps * es/(p-es)
        qs = ws/(1+ws)

        rh = 100. * q/qs

    end subroutine calc_RH

! ----------------------------------------------------------------------
! Equivalent potential temperature (THE)
! ----------------------------------------------------------------------

    subroutine calc_THE (the,t,p,q)

        implicit none

        ! Argument declaration
        real    :: the          ! Equivalent potential temperature (K)
        real    :: t            ! Temperature (either in K or C)
        real    :: p            ! Pressure (hPa)
        real    :: q            ! Specific humidity

        ! Physical parameters
        real    :: rdcp,tzero
        data    rdcp,tzero /0.2854,273.15/
        real,parameter :: rd=287.05, rv=461.51, eps = rd/rv

        ! Auxiliary variables
        real    :: tk,qk

        ! Calculation - distinction between temperature in C and in K
        if ( t .lt. 100 ) then
           tk = t + tzero
        else
           tk = t
        end if
        qk = q

      the = tk*(1000./p)                         &
     &      **(0.2854*(1.0-0.28*qk))*exp(        &
     &      (3.376/(2840.0/(3.5*alog(tk)-alog(   &
     &      100.*p*max(1.0E-10,qk)/(0.622+0.378* &
     &      q))-0.1998)+55.0)-0.00254)*1.0E3*    &
     &      max(1.0E-10,qk)*(1.0+0.81*qk))

    end subroutine calc_THE

! ----------------------------------------------------------------------
! Latent heating rate (LHR)
! ----------------------------------------------------------------------

    subroutine calc_LHR (lhr,t,p,q,omega,rh)

        implicit none

        ! Argument declaration
        real    :: lhr          ! Latent heating rate [K/6h]
        real    :: t            ! Temperature (either in K or in C)
        real    :: p            ! Pressure [hPa]
        real    :: q            ! Specific humidity
        real    :: omega        ! Vertical velocity [Pa/s] (in isobaric coordinates)
        real    :: rh                  

        ! Physical parameters
        real    :: p0,kappa,tzero
        data    p0,kappa,tzero /1000.,0.286,273.15/
        real    :: blog10,cp,r,lw,eps
        data    blog10,cp,r,lw,eps /.08006,1004.,287.,2.5E+6,0.622/

        ! Auxiliary variables
        real    :: tk,qk
        real    :: tt,c

        ! Calculation - distinction between temperature in C or in K
        if ( t .lt. 100. ) then
             tk = t + tzero
        else
             tk = t
        end if

        qk = q

        if ( rh .lt. 80. ) then
             lhr = 0.
        else if (omega .gt. 0.) then
             lhr = 0.
        else
             c   = lw/cp * eps * blog10 * esat(tk)/p
             tt  = tk*(p0/p)**kappa
             lhr = 21600. * (1.-exp(0.2*(80.-rh))) *    &
        &          (-c * kappa * tt * omega/(100.*p))/(1. + c)
        end if

    end subroutine calc_LHR

! ----------------------------------------------------------------------
! Wind Speed (VEL)
! ----------------------------------------------------------------------

    subroutine calc_VEL (vel,u,v)

        implicit none

        ! Argument declaration
        real    :: vel          ! Velocity [m/s]
        real    :: u,v          ! horizontal vel components [m/s]

        vel = sqrt( u**2 + v**2 )

    end subroutine calc_VEL 

! ----------------------------------------------------------------------
! Wind direction (DIR)
! ----------------------------------------------------------------------

    subroutine calc_DIR (dir,u,v)

        implicit none

        ! Argument declaration
        real    :: dir          ! Wind direction [deg]
        real    :: u,v          ! horizontal vel components [m/s]

        call getangle(1.,0.,u,v,dir)

    end subroutine calc_DIR

! ----------------------------------------------------------------------
! Zontal derivative of variable F (DFDX)
! ----------------------------------------------------------------------

    subroutine calc_DFDX (dfdx,f1,f0,lat,dx)

        implicit none

        ! argument declaration
        real :: dfdx         ! Zonal derivative of F component [m/s**2]
        real    :: f1           ! F @ LON + 1  DLON [m/s]
        real    :: f0           ! F @ LON - 1  DLON [m/s]
        real    :: lat          ! Latitude [deg]
        real    :: dx           ! in [deg] 

        ! physical parameters
        real,parameter :: pi180  = 4*atan(1.)/180.
        real,parameter :: deltay = 1.112E05     ! [m]
       

        dfdx = (f1 - f0) / (2. * dx * deltay * cos( lat * pi180 ))
        print*, 'SUCCESS',dfdx  ! check

    end subroutine calc_DFDX

! -------------------------------------------------------------------------
! Meridional derivative of variable F (DFDY)
! -------------------------------------------------------------------------

    subroutine calc_DFDY (dfdy,f1,f0,dy)

        implicit none

        ! argument declaration
        real    :: dfdy         ! Meridional derivative of F 
        real    :: f1           ! F @ LAT + 1 DLAT 
        real    :: f0           ! F @ LAT - 1 DLAT
        real    :: dy           ! in [deg]

        ! physical parameters
        real,parameter :: deltay = 1.112E05     ! [m]

        dfdy = (f1 - f0) / (2. * dy * deltay)

    end subroutine calc_DFDY

! -------------------------------------------------------------------------
! Vertical derivative of variable F (DFDP)
! -------------------------------------------------------------------------

    subroutine calc_DFDP (dfdp,f1,f0,p1,p0)

        implicit none
        
        ! argument declaration
        real    :: dfdp         ! Vertical derivative of F
        real    :: f1
        real    :: f0
        real    :: p0,p1

        dfdp = (f1 - f0) / ( 100 * (p1 - p0) )

    end subroutine calc_DFDP

! -------------------------------------------------------------------------
! Special handle for zonal derivative of theta (DTHDX)
! -------------------------------------------------------------------------
    subroutine calc_DTHDX (dthdx,t1,t0,p,lat,dx)

        implicit none

        ! argument declaration
        real    :: dthdx
        real    :: t1
        real    :: t0
        real    :: p
        real    :: lat
        real    :: dx
        
        ! physical parameter
        real,parameter :: pi180 = (4*atan(1.))/180.
        real,parameter :: deltay = 1.112E5 !km
        real           :: rdcp,pref
        data    rdcp,pref /0.286,1000./

        dthdx = (pref/p)**rdcp * (t1-t0) / (2. * dx * deltay * cos(pi180 * lat) )

        end

! -------------------------------------------------------------------------
! Special handle for meridional derivative of theta (DTHDY)
! -------------------------------------------------------------------------
    subroutine calc_DTHDY (dthdy,t1,t0,p,dy)

        implicit none

        ! argument declaration
        real    :: dthdy
        real    :: t1
        real    :: t0
        real    :: p
        real    :: dy

        ! physical parameter
        real,parameter :: pi180 = (4*atan(1.))/180.
        real,parameter :: deltay = 1.112E5 !km
        real           :: rdcp,pref
        data    rdcp,pref /0.286,1000./

        dthdy = (pref/p)**rdcp * (t1-t0) / (2. * dy * deltay)

        end

! -------------------------------------------------------------------------
! Special handle for vertical derivative of theta (DTHDP)
! -------------------------------------------------------------------------

    subroutine calc_DTHDP (dthdp,t1,t0,p1,p0,t,p)

        implicit none
        
        ! argument declaration
        real    :: dthdp        ! Vertical derivative of TH [K/Pa]
        real    :: t1           ! T @ P + 1 DP [K] or [C]
        real    :: t0           ! T @ P - 1 DP [K] or [C]
        real    :: t    
        real    :: p1           ! Pressure P + 1 DP [Pa]
        real    :: p0           ! P - 1 DP [Pa]
        real    :: p            

        ! Physical parameter
        real    :: rdcp,tzero,pref
        data    rdcp,tzero,pref /0.286,273.15,1000./

        ! Auxiliary variable
        real    :: tk,tk1,tk0

        ! Calculation - distinction between temperarture in K or in C
        if ( t .lt. 100. ) then
             tk = t + tzero
        else
             tk = t
        end if

        if ( t1 .lt. 100. ) then
             tk1 = t1 + tzero
        else
             tk1 = t1
        end if

        if ( t0 .lt. 100. ) then
             tk0 = t0 + tzero
        else
             tk0 = t0
        end if

        dthdp = (pref/p)**rdcp * 0.01 * ( (tk1 - tk0)/(p1 - p0) - rdcp * tk/p )

    end subroutine calc_DTHDP

! -------------------------------------------------------------------------
! Squared Brunt-Vaisäla frequency (NSQ)
! -------------------------------------------------------------------------

    subroutine calc_NSQ (nsq,dthdp,th,rho)

        implicit none
        
        ! argument declaration
        real    :: nsq          ! squared brunt-vaisäla frequency[s**-1]
        real    :: dthdp        ! vertical derivative of theta [K/Pa]
        real    :: th          ! theta [K]
        real    :: rho          ! density [kg/m**3]

        ! physical parameter
        real,parameter :: g=9.80665 ! gravitational acceleration [m/s**2]

        nsq = -g**2/th * rho * dthdp

    end subroutine calc_NSQ

! -------------------------------------------------------------------------
! Relative Vorticity (RELVORT)
! -------------------------------------------------------------------------

    subroutine calc_RELVORT (relvort,dudy,dvdx,u,lat)

        implicit none

        ! argument declaration
        real    :: relvort      ! relative vorticity [s**-1]
        real    :: dudy         ! meridional derivative of u component [s**-1]
        real    :: dvdx         ! zonal derivative of v component [s**-1]
        real    :: u            ! zonal vel. component u [m/s]
        real    :: lat          ! latitude [deg]

        ! physical parameter
        real,parameter :: pi180 = 4*atan(1.)/180.
        real,parameter :: deltay= 1.112E5

        relvort = dvdx - dudy + u * pi180/deltay * tan(pi180 * lat)
 
    end subroutine calc_RELVORT

! -------------------------------------------------------------------------
! Absolute vorticity (ABSVORT)
! -------------------------------------------------------------------------

    subroutine calc_ABSVORT (absvort,dudy,dvdx,u,lat)

        implicit none

        ! argument declaration
        real    :: absvort      ! absolute vorticity [s**-1]
        real    :: dudy         ! meridional derivative of u component [s**-1]
        real    :: dvdx         ! zonal derivative of v component [s**-1]
        real    :: u            ! zonal vel. component u [m/s]
        real    :: lat          ! latitude [deg]

        ! physical parameter 
        real,parameter :: pi180 = 4*atan(1.)/180.
        real,parameter :: deltay= 1.112E5
        real,parameter :: omega = 7.292E-5 ![s**-1]

        absvort = (dvdx-dudy + u*pi180/deltay*tan(pi180 * lat)) + &
        &         2*omega*sin(lat*pi180)

    end subroutine calc_ABSVORT

! -------------------------------------------------------------------------
! Divergence (DIV)
! -------------------------------------------------------------------------

    subroutine calc_DIV (div,dudx,dvdy,v,lat)

        implicit none

        ! argument declaration
        real    :: div          ! divergence [s**-1]
        real    :: dudx
        real    :: dvdy
        real    :: v
        real    :: lat

        ! physical parameter 
        real,parameter :: pi180 = 4*atan(1.)/180.
        real,parameter :: deltay= 1.112E5
   
        div = dudx + dvdy - v * pi180/deltay * tan(pi180 * lat)

    end subroutine calc_DIV

! -------------------------------------------------------------------------
! Deformation (DEF)
! -------------------------------------------------------------------------

    subroutine calc_DEF (def,dudx,dvdx,dudy,dvdy)

        implicit none
        
        ! argument declaration
        real    :: def          ! deformation [s**-1]
        real    :: dudx,dudy
        real    :: dvdx,dvdy

        ! physical parameter
        real,parameter :: pi180 = 4*atan(1.)/180.
        real,parameter :: deltay = 1.112E5

        def = sqrt ( (dvdx + dudy)**2 + (dudx - dvdy)**2 )

    end subroutine calc_DEF

! -------------------------------------------------------------------------
! Potential Vorticity (PV)
! -------------------------------------------------------------------------

    subroutine calc_PV (pv,absvort,dthdp,dudp,dvdp,dthdx,dthdy)
        
        implicit none
        
        ! argument declaration
        real    :: pv         ! Ertel-PV [PVU]
        real    :: absvort    ! Absolute vorticity [s^-1]
        real    :: dthdp      ! dth/dp [K/Pa]
        real    :: dudp       ! du/dp [m/s per Pa]
        real    :: dvdp       ! dv/dp [m/s per Pa]
        real    :: dthdx      ! dth/dx [K/m]
        real    :: dthdy      ! dth/dy [K/m]

        ! physical parameter
        real,parameter :: g=9.80665, scale=1.E6 ! convert to PVU

        pv = - scale * g * (absvort * dthdp + dudp * dthdy - dvdp * dthdx ) 
        
    end subroutine calc_PV

! -------------------------------------------------------------------------
! Richardson number (RI)
! -------------------------------------------------------------------------

    subroutine calc_RI (ri,dudp,dvdp,nsq,rho)

        implicit none

        !  Argument declaration
        real  ri         ! Richardson number
        real  dudp       ! Du/Dp [m/s per Pa]
        real  dvdp       ! Dv/Dp [m/s per Pa]
        real  nsq        ! Squared Brunt-Vailälä frequency [s^-1]
        real  rho        ! Density [kg/m^3]

        ! Physical and numerical parameters
        real      g
        parameter (g=9.80665)

        ri = nsq / ( dudp**2 + dvdp**2 ) / ( rho * g )**2

    end subroutine calc_RI

! -------------------------------------------------------------------------
! Distance from starting position
! -------------------------------------------------------------------------

    subroutine calc_DIST0 (dist0,lon0,lat0,lon1,lat1)

        implicit none

        ! argument declaration
        real    :: dist0        ! Distance in km 
        real    :: lon0,lat0    ! Coordinates of starting position
        real    :: lon1,lat1    ! Coordinates of ref position

    !    real,external :: sdis

        dist0 = sdis(lon0,lat0,lon1,lat1)

    end subroutine calc_DIST0

! -------------------------------------------------------------------------
! Heading of the trajectory (HEAD)
! -------------------------------------------------------------------------

    subroutine calc_HEAD (head,lon0,lat0,lon1,lat1)

        implicit none

        ! Argument declaration
        real  head       ! Heading angle (in deg) relativ to zonal direction
        real  lon0,lat0  ! Starting position
        real  lon1,lat1  ! New position 

        ! Physical parameters
        real      pi180
        parameter (pi180=3.14159/180.)

        !Auixiliary variables
        real     dx,dy
        real     dlon

        dlon = lon1-lon0
        if ( dlon.gt.180.  ) dlon = dlon - 360.
        if ( dlon.lt.-180. ) dlon = dlon + 360.

        dx = dlon * cos(pi180*0.5*(lat0+lat1))
        dy = lat1-lat0

        call getangle(1.,0.,dx,dy,head)

    end subroutine calc_HEAD

! -------------------------------------------------------------------------
! Directional change of the trajectory (HEAD)
! -------------------------------------------------------------------------

    subroutine calc_DANGLE (dangle,lon0,lat0,lon1,lat1,lon2,lat2)

        implicit none

        ! Argument declaration
        real  dangle     ! Directiopanl change
        real  lon0,lat0  ! t-1
        real  lon1,lat1  ! t 
        real  lon2,lat2  ! t+1

        !Physical parameters
        real      pi180
        parameter (pi180=3.14159/180.)
        real      eps
        parameter (eps=0.00001)

        ! Auixiliary variables
        real     dx1,dy1,dx2,dy2,norm,cross,dlon1,dlon2

        dlon1 = lon1 - lon0
        if ( dlon1.gt.180.  ) dlon1 = dlon1 - 360.
        if ( dlon1.lt.-180. ) dlon1 = dlon1 + 360.
        dlon2 = lon2 - lon1
        if ( dlon2.gt.180.  ) dlon2 = dlon2 - 360.
        if ( dlon2.lt.-180. ) dlon2 = dlon2 + 360.

        dx1 = dlon1 * cos(pi180*0.5*(lat1+lat0))
        dy1 = lat1 - lat0
        dx2 = dlon2 * cos(pi180*0.5*(lat2+lat1))
        dy2 = lat2 - lat1

        norm = sqrt( (dx1**2 + dy1**2) * (dx2**2 + dy2**2) )

        if ( norm.gt.eps ) then
             cross = ( dx1 * dy2 - dy1 * dx2 ) / norm
             if ( cross.ge.1. ) then
                dangle = 90.
             elseif (cross.le.-1.) then
                dangle = -90.
             else
                dangle = 1./pi180 * asin( cross )
             endif
        else
             dangle = -999.
        endif

    end subroutine calc_DANGLE

! -------------------------------------------------------------------------
! Finding Eigenvalues and Eigenvectors of a matrix A using LAPACK
! -------------------------------------------------------------------------
!     module load lapack/3.5.0_gnu_64
!     DGEEV from llapack
    subroutine calc_EIG(WR,WI,dudx,dvdx,dudy,dvdy)

        ! The routine computs for an n-by-n real nonsymmetric matrix A, the
        ! eigenvalues and, optionally, the left and/or right eigenvectors. The
        ! right eigenvector v(j) of A satisfies
        ! A*v(j) = lambda(j) * v(j)
        ! where lambda(j) is its eigenvalue. The left eigenvector u(j) of A
        ! satisfies
        ! u(j)H*A = lambda(j)*u(j)H
        ! where u(j)H denotes the conjugate transpose of u(j). The computed
        ! eigenvectors are normalized to have Euclidean norm equal to 1 and
        ! largest component real.

        implicit none

        ! argument declaration
        real    :: dudx, dudy, dvdx, dvdy

        integer, parameter :: N=2
        integer, parameter :: LDA=N, LDVL=N, LDVR=N, LWMAX=10000
        double precision  :: A(LDA,N), VL(LDVL,N), VR(LDVR,N), WR(N), WI(N), WORK(LWMAX)
        ! local scalars
        integer            :: info, lwork

        ! external subroutines
        external  :: dgeev
        ! intrinsic functions
        intrinsic :: int, min

        ! DEFINE A - THE VELOCITY GRADIENT TENSOR
        A(1,1) = dudx
        A(1,2) = dudy
        A(2,1) = dvdx
        A(2,2) = dvdy
        ! QUERY THE OPTIMAL WORKSPACE
        lwork = -1
        call DGEEV( 'V', 'V', N, A, LDA, WR, WI, VL, LDVL, VR, LDVR,    &
                    WORK,LWORK, INFO)
        LWORK = MIN( LWMAX, INT(WORK(1)))

        ! SOLVE EIGEN PROBLEM   
        call DGEEV( 'V', 'V', N, A, LDA, WR, WI, VL, LDVL, VR, LDVR,    &
                    WORK, LWORK, INFO)

        ! Check
        if ( info .gt. 0 ) then
           write(*,*) 'The algorithm failed to compute eigenvalues.'
           stop
        end if

    end subroutine calc_EIG

!     *************************************************************************
!     Auxiliary subroutines and functions
!     *************************************************************************

!     -------------------------------------------------------------------------
!     Saturation vapor pressure over water
!     -------------------------------------------------------------------------

    real function esat(t)

        ! This function returns the saturation vapor pressure over water
        ! (mb) given the temperature (Kelvin).
        ! The algorithm is due to Nordquist, W. S. ,1973: "Numerical
        ! Approximations of Selected Meteorological Parameters for Cloud
        ! Physics Problems" ECOM-5475, Atmospheric Sciences Laboratory,
        ! U. S. Army Electronics Command, White Sands Missile Range,
        ! New Mexico 88002.

        real p1,p2,c1,t

        p1  = 11.344  - 0.0303998 * t
        p2  = 3.49149 - 1302.8844 / t
        c1  = 23.832241 - 5.02808 * log10(t)
        esat= 10.**(c1 - 1.3816e-7*10.**p1 + 8.1328e-3 * 10.**p2 - 2949.076/t)

      end function esat

!     -------------------------------------------------------------------------
!     Angle between 2 vectors
!     -------------------------------------------------------------------------
   
    subroutine getangle (ux1,ux2,uy1,uy2,angle)

        ! given 2 vectors {ux1,uy1} and {ux2,uy2}, determine the angle in [deg]
        ! between 2 vectors
        
        implicit none

        ! argument declaration
        real    :: ux1,ux2
        real    :: uy1,uy2
        real    :: angle

        ! physical parameters
        real,parameter :: pi = 4*atan(1.)

        ! auxiliary varialbles
        real    :: len1,len2,len3
        real    :: val1,val2,val3
        real    :: vx1,vy1,vx2,vy2

        vx1 = ux1
        vx2 = ux2
        vy1 = uy1
        vy2 = uy2

        len1 = sqrt (vx1**2 + vy1**2)
        len2 = sqrt (vx2**2 + vy2**2)

        if ( (len1.gt.0) .and. (len2.gt.0) ) then

           vx1 = vx1/len1
           vy1 = vy1/len1
           vx2 = vx2/len2
           vy2 = vy2/len2

           val1 =   vx1*vx2 + vy1*vy2
           val2 = - vy1*vx2 + vx1*vy2

           len3 = sqrt (val1**2 + val2**2)

           if ( (val1.ge.0) .and. (val2.ge.0) ) then
                val3 = acos (val1/len3)
           else if ( (val1.lt.0) .and. (val2.ge.0) ) then
                val3 = pi - acos (abs(val1)/len3)
           else if ( (val1.ge.0) .and. (val2.lt.0) ) then
                val3 = -acos (val1/len3)
           else if ( (val1.lt.0) .and. (val2.lt.0) ) then
                val3 = -pi + acos (abs(val1)/len3)
           end if

        else

           val3 = 0.
        
        end if

        angle = 180./pi * val3

    end subroutine getangle

!    -------------------------------------------------------------------------
!    Spherical distance between lat/lon points
!    -------------------------------------------------------------------------

    real function sdis(xp,yp,xq,yq)

        ! calculate spherical distance between 2 lat/lon points
        ! given by their spherical coordinates xp;yp and xq;yq, respectively

        implicit none

        real    :: xp,yp,xq,yq
        real,parameter :: re = 6370.
        real,parameter :: pi180 = 4*atan(1.)/180.
        real    :: arg

        arg = sin(pi180*yp)*sin(pi180*yq) +     &
        &     cos(pi180*yp)*cos(pi180*yq)*cos(pi180*(xp-xq))

        if (arg.lt.-1.) arg=-1.
        if (arg.gt.1.) arg=1.

        sdis = re * acos(arg)

    end function sdis

end module calvar  
