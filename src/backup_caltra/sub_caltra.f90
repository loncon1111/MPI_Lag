module sub_caltra

   contains

!  ************************************************************
!  * Time step : either Euler or Runge-Kutta                  *
!  ************************************************************

!  time-step from (x0,y0,p0) to (x1,y1,p1)
!
!  (x0,y0,p0) input coordinates (long, lat, p) for starting point
!  (x1,y1,p1) output coordinates (long,lat, o) for ending point
!  deltat     input timestep in seconds
!  numit      input number of iterations
!  jump       input flag (=1 trajectories don't enter the ground)
!  left       output flag (=1 if trajectory leaves data domain)


!  ------------------------------------------------------------
!  Iterative Euler time step (KINEMATIC 3D TRAJECTORIES)
!  ------------------------------------------------------------

   subroutine euler_3d(x1,y1,p1,left,x0,y0,p0,reltpos0,reltpos1,&
                      &deltat,numit,jump,mdv,wfactor,fbflag,&
                      &spt0,spt1,p3d0,p3d1,uut0,uut1,vvt0,vvt1,wwt0,wwt1,&
                      &xmin,ymin,dx,dy,per,hem,nx,ny,nz)

      use interp
      implicit none

  !   declaration of subroutine parameters
      integer      ::nx,ny,nz             ! number of points
      real         ::x1,y1,p1             ! ending point
      integer      ::left                 ! output flag (left the domain or not)
      real         ::x0,y0,p0             ! starting point
      real         ::reltpos0,reltpos1    ! relative time position
      real         ::deltat               ! input timestep (seconds)
      integer      ::numit                ! input number of iterations
      integer      ::jump                 ! input flag
      real         ::wfactor              ! vertical wind (scale)
      integer      ::fbflag               ! backward or forward trajectory
      real         ::spt0(nx*ny),spt1(nx*ny)      ! surface pressure at t0 and t1
      real         ::uut0(nx*ny*nz),uut1(nx*ny*nz)! zonal wind at t0 and t1
      real         ::vvt0(nx*ny*nz),vvt1(nx*ny*nz)! meridional wind at t0 and t1
      real         ::wwt0(nx*ny*nz),wwt1(nx*ny*nz)! vertical wind at t0 and t1
      real         ::p3d0(nx*ny*nz),p3d1(nx*ny*nz)! 3D pressure at t0 and t1
      real         ::xmin, ymin, dx, dy   ! minimum lon(xmin),lat(ymin),dx,dy(grid spacing)
      real         ::per                  ! if data domain periodic and its periodicity
      integer      ::hem                  ! flag for hemispheric domain
      real         ::mdv                  ! missing data value

  !   Numerical and physical constants
      real,parameter::deltay=1.112E5     ! Distance in m between 2 lat circles
      real,parameter::pi=4*atan(1.0)        ! Pi

  !   Auxiliary variables
      real         ::xmax,ymax            ! maximum lon(xmax),lat(ymax)
      real         ::xind,yind,pind       !
      real         ::u0,v0,w0,u1,v1,w1,u,v,w,sp   ! 
      integer      ::icount
      character    ::ch

  !   Externals    
      !real,external::int_index4           !

  !   Reset the flag for domain-leaving
      left=0

  !   Set the east-north boundary of the domain
      xmax = xmin+real(nx-1)*dx
      ymax = ymin+real(ny-1)*dy

  !   Interpolate wind fields to starting position (x0,y0,p0)
      call get_index4 (xind,yind,pind,x0,y0,p0,reltpos0,&
                      &p3d0,p3d1,spt0,spt1,3,           &
                      &nx,ny,nz,xmin,ymin,dx,dy,mdv)
      u0 = int_index4(uut0,uut1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
      v0 = int_index4(vvt0,vvt1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
      w0 = int_index4(wwt0,wwt1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)

  !   Force the near-surface wind to zero
      if (pind.lt.1.) w0=w0*pind

  !   For first iteration take ending position equal to starting position
      x1=x0
      y1=y0
      p1=p0

  !   Iterative calculation of new position
      do icount = 1, numit

        !  calculate new winds for advection
        call get_index4(xind,yind,pind,x1,y1,p1,reltpos1,&
                    &    p3d0,p3d1,spt0,spt1,3, &
                    &    nx,ny,nz,xmin,ymin,dx,dy,mdv)

        u1 = int_index4(uut0,uut1,nx,ny,nz,xind,yind,pind,reltpos1,mdv)
        v1 = int_index4(vvt0,vvt1,nx,ny,nz,xind,yind,pind,reltpos1,mdv)
        w1 = int_index4(wwt0,wwt1,nx,ny,nz,xind,yind,pind,reltpos1,mdv)

        ! force the near-surface wind to zero
        if ( pind .lt. 1 ) w1 = pind*w1

        ! get the new velocity in between
        u = ( u0 + u1 )/2.
        v = ( v0 + v1 )/2.
        w = ( w0 + w1 )/2.

        ! calculate new positions
        x1 = x0 + fbflag*u*deltat/(deltay*cos(y0*pi/180.)) ! last term is to convert from meter to lat/long degree
        y1 = y0 + fbflag*v*deltat/deltay
        p1 = p0 + fbflag*w*deltat
        
        ! handle pole problems (crossing or near pole trajectory)
        if ( (hem.eq.1) .and. (y1.gt.90.) ) then
           y1 = 180. - y1
           x1 = x1 + per/2.
        endif

        if ( (hem.eq.1) .and. (y1.lt.-90.) ) then
          y1 = -180. - y1
          x1 = x1 + per/2.
        endif

        if (y1.gt.89.99) then
           y1 = 89.99
        end if

        ! handle crossings of the dateline (0 deg longitude)
        if ( (hem.eq.1) .and. (x1.gt.xmin+per-dx) ) then 
           x1 = xmin + amod(x1-xmin,per)
        end if

        if ( (hem.eq.1) .and. (x1.lt.xmin) ) then
           x1 = xmin + per + amod(x1-xmin,per)
        end if

        ! interpolate surface pressure to actual position
        call get_index4(xind,yind,pind,x1,y1,1050.,reltpos1,&
                       &p3d0,p3d1,spt0,spt1,3,nx,ny,nz,xmin,&
                       &ymin,dx,dy,mdv)

        sp = int_index4(spt0,spt1,nx,ny,1,xind,yind,1.,reltpos1,mdv)

        ! handle trajectories which cross the lower boundary (jump flag)
        if ((jump.eq.1).and.(p1.gt.sp)) p1=sp-10.
        
        ! check if trajectory leaves data domain
        if ( ( (hem.eq.0) .and. (x1.lt.xmin   ) ) .or.&
            &( (hem.eq.0) .and. (x1.gt.xmax-dx) ) .or.&
            &(y1.lt.ymin).or.(y1.gt.ymax).or.(p1.gt.sp) ) then
           go to 100
        end if      

      end do

100   continue

      return

   end subroutine euler_3d

!  --------------------------------------------------------------
!  Iterative Euler time step 2D (MODEL LEVELS)
!  --------------------------------------------------------------

   subroutine euler_2d(x1,y1,p1,left,x0,y0,p0,zindex,reltpos0,reltpos1,deltat,&
                      &numit,jump,mdv,wfactor,fbflag,spt0,spt1,p3d0,p3d1,uut0,&
                      &uut1,vvt0,vvt1,xmin,ymin,dx,dy,per,hem,nx,ny,nz)

      use interp
      implicit none
        
      ! delclaration of subroutine variables
      real      :: x1,y1,p1     ! output coordinates (lon,lat,p) for ending point
      real      :: x0,y0,p0     ! input coordinates (lon,lat,p) for starting point
      integer   :: left         ! flag if traj left the domain
      real      :: zindex       ! vertical index for model-level (2D) trajectories  
      real      :: reltpos0,reltpos1 ! relative position in time interpolation 
      real      :: deltat       ! timestep in seconds
      integer   :: jump         ! flag (=1 trajs don't enter the ground
      real      :: mdv          ! flag (<>0 if misvalue is set)
      real      :: wfactor      ! vertical wind (scale)
      integer   :: fbflag       ! forward(1) or backward(-1) flag
      integer   :: numit
      real      :: spt0(nx*ny),spt1(nx*ny)    ! surface pressure
      real      :: p3d0(nx*ny*nz),p3d1(nx*ny*nz) ! 3D pressure
      real      :: uut0(nx*ny*nz),uut1(nx*ny*nz),vvt0(nx*ny*nz),vvt1(nx*ny*nz) !horizontal wind components
      real      :: wwt0(nx*ny*nz),wwt1(nx*ny*nz) ! vertical wind component
      real      :: xmin,ymin    ! most south-west point of domain
      real      :: dx,dy        ! grid spacing
      real      :: per          ! check if domain is periodic(=0ifno) and its periodicity 
      integer   :: hem          ! hemispheric flag
      integer   :: nx,ny,nz     ! number of points

      ! numerical parameters
      real,parameter::deltay=1.112e5   ! distance in m between 2 lat circle
      real,parameter::pi=4*atan(1.0)   
      real,parameter::eps=0.001

      ! auxiliary variables
      real      :: xmax,ymax
      real      :: xind,yind,pind
      real      :: u1,u0,v1,v0,w1,w0,u,v,w
      real      :: sp
      integer   :: icount
      character :: ch

      ! externals
      !real,external:: int_index4

      ! reset the flag for domain-leaving
      left = 0

      ! set the east-north boundary of the domain
      xmax = xmin + real(nx-1)*dx
      ymax = ymin + real(ny-1)*dy

      ! interpolate wind fields to starting position (x0,y0,p0)
      call get_index4(xind,yind,pind,x0,y0,p0,reltpos0,p3d0,p3d1,&
                     &spt0,spt1,3,nx,ny,nz,xmin,ymin,dx,dy,mdv)

      u0 = int_index4(uut0,uut1,nx,ny,nz,xind,yind,zindex,reltpos0,mdv)
      v0 = int_index4(vvt0,vvt1,nx,ny,nz,xind,yind,zindex,reltpos0,mdv)

      ! for first iteration take ending position equal to starting position
      x1 = x0
      y1 = y0
      p1 = p0

      ! iterative calculation of new position
      do icount = 1, numit

        ! calculate new wind for advection
        call get_index4(xind,yind,pind,x1,y1,p1,reltpos1,p3d0,p3d1,&
                       &spt0,spt1,3,nx,ny,nz,xmin,ymin,dx,dy,mdv)
        u1 = int_index4(uut0,uut1,nx,ny,nz,xind,yind,zindex,reltpos1,mdv)
        v1 = int_index4(vvt0,vvt1,nx,ny,nz,xind,yind,zindex,reltpos1,mdv)
        
        if ( abs(u1-mdv).lt.eps ) then
           left = 1
           go to 100
        end if

        if ( abs(v1-mdv).lt.eps ) then
           left = 1
           go to 100
        end if

        ! get the new velocity in between
        u = (u0 + u1)/2.
        v = (v0 + v1)/2.        

        ! calculate new positions
        x1 = x0 + fbflag * u * deltat/(deltay * cos(y0*pi/180.))
        y1 = y0 + fbflag * u * deltat/deltay

        ! get the pressure on the model surface at the new position
        xind = (x1 - xmin)/dx + 1.
        yind = (y1 - ymin)/dy + 1.

        pind = int_index4(p3d0,p3d1,nx,ny,nz,xind,yind,zindex,reltpos1,mdv)
        if ( abs(p1-mdv).lt.eps ) then
           left = 1
           go to 100
        end if

        ! handle pole problems (crossing and near pole trajectory)
        if ( (hem.eq.1) .and. (y1.gt.90) ) then
           y1 = 180. - y1
           x1 = x1 + per/2.
        end if

        if ( (hem.eq.1) .and. (y1.lt.-90) ) then
           y1 = -180. - y1
           x1 = x1 + per/2.
        end if

        if ( y1.gt.89.99) then
           y1 = 89.99
        end if

        ! handle crossing of the dateline
        if ( (hem.eq.1) .and. (x1.gt.(xmin-dx+per)) ) then
           x1 = xmin + amod(x1-xmin,per)
        end if

        if ( (hem.eq.1) .and. (x1.lt.xmin) ) then
           x1 = xmin + per + amod(x1-xmin,per)
        end if

        ! interpolate surface pressure to actual position
        call get_index4(xind,yind,pind,x1,y1,1050.,reltpos1,p3d0,p3d1,spt0,&
                       &spt1,3,nx,ny,nz,xmin,ymin,dx,dy,mdv)

        sp = int_index4(spt0,spt1,nx,ny,1,xind,yind,1.,reltpos1,mdv)

        ! handle trajectories which cross the lower boundary (jump flag)
        if ((jump.eq.1).and.(p1.gt.sp)) p1 = sp - 10.

        ! check if the trajectory leaves data domain
        if ( ((hem.eq.0) .and. (x1.lt.xmin) ) .or. &
           &( (hem.eq.0) .and. (x1.gt.xmax-dx) ) .or. &
           &(y1.lt.ymin).or.(y1.gt.ymax).or.(p1.gt.sp) ) then

           left = 1
           go to 100
        end if

      end do

 100  continue

      return       
   end subroutine euler_2d

   subroutine runge(x1,y1,p1,left,x0,y0,p0,reltpos0,reltpos1,deltat,numit,jump,&
                   &mdv,wfactor,fbflag,spt0,spt1,p3d0,p3d1,uut0,uut1,vvt0,vvt1,&
                   &wwt0,wwt1,xmin,ymin,dx,dy,per,hem,nx,ny,nz)

      use interp
      implicit none

      ! declaration of subroutine variables
      real      :: x1, y1, p1   ! grid coordinates (lon,lat,p) of ending point
      real      :: x0, y0, p0   ! grid coordinates (lon,lat,p) of starting point
      integer   :: left         ! flag if point leaves the domain
      real      :: reltpos0,reltpos1    ! relative position in time interp
      real      :: deltat       ! time step
      integer   :: numit        ! number of iterations
      integer   :: jump         ! flag (=1 trajs don't enter the ground)
      real      :: mdv          ! missing value flag (<>0 is misdat)
      real      :: wfactor      ! vertical wind (scale)
      integer   :: fbflag       ! forward(1) and backward(-1) flag
      real      :: spt0(nx*ny),spt1(nx*ny)    ! surface pressure
      real      :: p3d0(nx*ny*nz),p3d1(nx*ny*nz)    ! 3D pressure
      real      :: uut0(nx*ny*nz),uut1(nx*ny*nz),vvt0(nx*ny*nz),vvt1(nx*ny*nz) ! horizontal wind components
      real      :: wwt0(nx*ny*nz),wwt1(nx*ny*nz)  ! vertical wind component
      real      :: xmin,ymin,dx,dy
      real      :: per
      integer   :: hem
      integer   :: nx,ny,nz      

      ! numerical parameters
      real,parameter :: deltay=1.112e5
      real,parameter :: pi=4*atan(1.0)

      ! auxiliary variables
      real      :: xmax,ymax
      real      :: xind,yind,pind
      real      :: u0,v0,w0,u1,v1,w1,u,v,w,sp
      integer   :: n,icount
      real      :: xs,ys,ps,xk(4),yk(4),pk(4)
      real      :: reltpos

      ! externals
      print*, "x0,y0,p0: ",x0,y0,p0     ! check


      ! reset the flags for domain-leaving
      left = 0

      ! set the east-north boundary of the domain
      xmax = xmin + real(nx-1)*dx
      ymax = ymin + real(ny-1)*dy

      ! apply the Runge Kutta scheme

      do n = 1,4
        
        ! get intermediate position and relative time
        if ( n.eq.1 ) then
           xs = 0.
           ys = 0.
           ps = 0.
           reltpos=reltpos0
        else if (n.eq.4) then
           xs = xk(3)
           ys = yk(3)
           ps = pk(3)
           reltpos = reltpos1
        else
           xs = xk(n-1)/2.
           ys = yk(n-1)/2.
           ps = pk(n-1)/2.
           reltpos = (reltpos0 + reltpos1)/2.
        end if
        !print*, "xs ", xs                      !"checkkkkkkk"
      ! calculate new winds for advection
        call get_index4(xind,yind,pind,x0+xs,y0+ys,p0+ps,reltpos,p3d0,p3d1,&
                       &spt0,spt1,3,nx,ny,nz,xmin,ymin,dx,dy,mdv)

        !print*, "check1"

        u = int_index4(uut0,uut1,nx,ny,nz,xind,yind,pind,reltpos,mdv)
        !print*, "check2"
        v = int_index4(vvt0,vvt1,nx,ny,nz,xind,yind,pind,reltpos,mdv)
        !print*, "check3"
        w = int_index4(wwt0,wwt1,nx,ny,nz,xind,yind,pind,reltpos,mdv)

        !print*, "(u,v,w)", u, v, w      ! check

      ! force the near-surface wind to zero
        if ( pind .lt. 1 ) w = w*pind

      ! update position and keep them
        xk(n) = fbflag * u * deltat / (deltay*cos(y0 * 180./pi))
        yk(n) = fbflag * v * deltat / deltay
        pk(n) = fbflag * w * deltat * wfactor/100.

      !  print*, "(deltat,deltay): ",deltat,deltay ! check
        !print*, "(update position: xk(n),yk(n),pk(n)):", xk(n),yk(n),pk(n) !check

        

      end do

      x1 = x0 + 1./6.*( xk(1) + 2*xk(2) + 2*xk(3) + xk(4) )
      y1 = y0 + 1./6.*( yk(1) + 2*yk(2) + 2*yk(3) + yk(4) )
      p1 = p0 + 1./6.*( pk(1) + 2*pk(2) + 2*pk(3) + pk(4) )

      !print*, "(x0,y0,p0): ",x0,y0,p0   ! checckkkkk
      print*, "(x1,y1,p1): ",x1,y1,p1   ! checckkkkk

      ! handle pole problems ( crossing and near pole trajectory )
      if (( hem.eq.1 ).and.( y1.gt.90 )) then
        y1 = 180. - y1
        x1 = x1 + per/2.
      end if
      if (( hem.eq.1 ).and.( y1.lt.-90 )) then
        y1 = -180. - y1
        x1 = x1 + per/2.
      end if
      if ( y1.gt.89.99 ) y1 = 89.99

      ! handle crossing of the dateline
      if (( hem.eq.1 ).and.(x1.gt.xmin-dx+per)) then
        x1 = xmin + amod(xmin-dx,per)
      end if
      if (( hem.eq.1 ).and.(x1.lt.xmin)) then
        x1 = xmin + per + amod(xmin-dx,per)
      end if

      ! interpolate surface pressure to actual position
      call get_index4(xind,yind,pind,x1,y1,1050.,reltpos1,p3d0,p3d1,spt0,&
                      &spt1,3,nx,ny,nz,xmin,ymin,dx,dy,mdv)

      sp = int_index4(spt0,spt1,nx,ny,1,xind,yind,1.,reltpos1,mdv)

      print*,"sp: ", sp ! checkkkk

      ! handle trajectories which crossed the lower boundary (when jump = 1)
      if ( (jump.eq.1).and.(p1.gt.sp) ) p1 = sp - 10.

      ! check if trajectories leave data domain
      if ( ( (hem.eq.0).and.(x1.lt.xmin) ) .or. &
        &  ( (hem.eq.0).and.(x1.gt.xmax) ) .or. &
        &  (y1.lt.ymin).or.(y1.gt.ymax).or.(p1.gt.sp) ) then
         left = 1
      end if
 
    end subroutine runge
end module sub_caltra
