Program caltra

! ========== Calculation of trajectories =============
! Inspired by major update of Michael Spenger, 2009
! Made by HoaDao, 2019
! ====================================================
    use sub_caltra
    use ncinput
    use caltime
    use interp
    use iotra

    implicit none

!   --------------------------------------------------
!   Declaration of parameters
!   --------------------------------------------------
    
    ! maximum number of levels in the input files
    integer,parameter:: nlevmax = 200 
    ! numerical epsilon (for float comparison)
    real,parameter   :: eps = 0.001 
    ! maximum number of input files (dates, length of trajs)
    integer,parameter:: ndatmax = 500
    ! distance in m between 2 lat circles
    real,parameter   :: deltay = 1.112E5 
    ! number of iterations for iterative Euler scheme
    integer,parameter:: numit = 3 

!   --------------------------------------------------
!   Declaration of variables
!   --------------------------------------------------

    !Input parameters
    integer     :: fbflag       ! flag for forward/backward mode
    integer     :: numdat       ! number of input files
    character*20:: dat(ndatmax) ! dates of input files
    real        :: timeinc      ! time increment between input files
    real        :: per          ! periodicity (=0 if none)
    integer     :: imethod      ! method to cal (=0 Euler iterative,=1 Runge-Kutta)


    !trajectories vars
    integer     :: ntra         ! number of trajectories
    character*80:: cdfname      ! name of output files
    real        :: ts           ! time step
    real        :: tst,ten      ! shift of start and end time relative to first data file
    integer     :: deltout      ! output time interval (in mins)
    integer     :: jflag        ! jump flag (if =1 ground-touching trajs re-enter the atmosphere)
    real        :: wfactor      ! factor for vertial velocity field
    character*80:: strname      ! file with start positions
    logical     :: timecheck    ! either 'yes' or 'no
    logical     :: modlev       ! 2D (model level) trajectories ('yes' or 'no')

    integer     :: ncol         ! number of columns for input trajectories
    real,allocatable,dimension(:,:,:) :: trainp ! input start coordinates (ntra,1,ncol)
    real,allocatable,dimension(:,:,:) :: traout ! output trajectories (ntra,ntim,4)
    integer     :: reftime(6)   ! reference date
    character*80:: vars(200)    ! field names
    real,allocatable,dimension (:)   :: xx0,yy0,pp0        ! position of input air parcels
    real        :: xx1,yy1,pp1        ! updated position of air parcels
    integer,allocatable,dimension(:) :: leftflag           ! flag for domain-leaving
    integer     :: leftcount    ! number of domain-leaving trajectories
    integer     :: ntim         ! number of output timesteps
    real,allocatable,dimension (:)   :: zindex             ! vertical index for model-level (2D) trajectories

    ! meteorological fields
    real,allocatable,dimension (:)   :: spt0,spt1          ! surface pressure
    real,allocatable,dimension (:)   :: uut0,uut1          ! zonal wind
    real,allocatable,dimension (:)   :: vvt0,vvt1          ! meridional wind
    real,allocatable,dimension (:)   :: wwt0,wwt1          ! vertical wind
    real,allocatable,dimension (:)   :: p3t0,p3t1          ! 3d-pressure

    ! grid description
    real        :: pollon, pollat       ! longitude/latitude of pole
    real        :: ak(nlevmax)          ! vertical layers and levels 
    real        :: bk(nlevmax)          
    real        :: xmin,xmax            ! zonal grid extension
    real        :: ymin,ymax            ! meridional grid extension
    integer     :: nx,ny,nz             ! grid dimensions
    real        :: dx,dy                ! horizontal grid resolution
    integer     :: hem                  ! flag for hemispheric domain
    real        :: mdv                  ! missing data value 

    ! input and output format for trajectories
    integer     :: inpmode
    integer     :: outmode 

    ! Auxiliary variables
    real        :: delta,rd
    real,allocatable :: r1d(:)             ! rd:temporary reading     
    integer     :: itm,iloop,i,j,k,filo,lalo
    integer     :: ierr,stat
    integer     :: cdfid,fid
    real        :: tstart,time0,time1,time
    real        :: reltpos0,reltpos1
    real        :: xind,yind,pind,pp,sp,stagz
    character*80:: filename,varname
    integer     :: reftmp(6)
    character   :: ch
    real        :: frac,tload
    integer     :: itim
    integer     :: wstep
    real        :: x1,y1,p1
    real        :: thetamin,thetamax
    real        :: zindexmin,zindexmax
    integer     :: year,month,day,hour,minute,time_range

    namelist /params/ fbflag,imethod,jflag,wfactor,timecheck,ts,tst,ten
    namelist /input/  year,month,day,hour,minute,time_range,modlev,&
                      &numdat,timeinc,per,strname,ntra,ncol
    namelist /output/ cdfname,deltout

!   -----------------------------------------------------------
!   Start of program, Read parameters
!   -----------------------------------------------------------


    ! write text message
    print*, "=================================================="
    print*, "         *** START OF PROGRAM CALTRA ***          "
    print* 

    ! open the parameter file
    open(100,file="namelist.caltra",status='old',form='formatted')
        read(100,params)
        read(100,input)
        read(100,output)
    close(100)

    ! handle if number of data files excesses ndatmax
    if ( numdat .gt. ndatmax ) then
        print*, "ERROR: too many input files ", numdat, ndatmax
        go to 1000
    end if

    ! read list of input dates (dat, sort depending on forward/backward mode)
    open(101,file="input.lst",status="old",form='formatted')
        if ( fbflag.eq.1 ) then
           do itm = 1, numdat
              read (101,*) dat(itm)
           end do
        else if ( fbflag.eq.-1 ) then
           do itm = numdat, 1, -1
              read (101,*) dat(itm)
           end do
        else
           print*, "ERROR: fbflag is not sufficient, -1/1 only", fbflag
           go to 1000
        end if
    close(101)

    ! convert time step for trajectory calculations from mins to hours
    ts = ts/60.

    ! read the reference time and time range
    reftime(1)  = year
    reftime(2)  = month
    reftime(3)  = day
    reftime(4)  = hour
    reftime(5)  = minute
    reftime(6)  = time_range

    ! close the namelist file
    close(101)

    ! calculate the number of output time steps
    ntim = abs(reftime(6)/deltout) + 1

    !Set the formats of the input and output files
    call mode_tra(inpmode,strname)
    call mode_tra(outmode,cdfname)
    if (outmode.eq.-1) outmode=1

    ! Write some status information
    print*, "-----------------INPUT PARAMETERS--------------------"
    print* 
    write(*,'(a30,i12)') " - Forward/Backward        : ", fbflag
    write(*,'(a30,i12)') " - #input files            : ", numdat
    print*, " - First/last input file   : ", trim(dat(1)),' ... ',trim(dat(numdat))
    write(*,'(a30,f14.2)') " - Time increment          : ", timeinc
    write(*,*) " - Output filename         : ", trim(cdfname)
    write(*,'(a30,f14.2)') " - Time step (min)         : ", 60.*ts
    write(*,'(a30,f7.2,f7.2)') " - Time shift (start,end)  : ",tst,ten
    write(*,'(a30,i12)') " - Output time interval    : ", deltout
    write(*,'(a30,i12)') " - Jump flag               : ", jflag
    print*, " - Vertical wind (scale)   : ", wfactor
    print*, " - Trajectory position file: ", trim(strname)
    print*, " - # of trajectories       : ", ntra
    print*, " - # of output timesteps   : ", ntim
    !print*, " - Input format            : ", inpmode
    if ( inpmode.eq.-1) then
         print*,'  Input format           : (lon,lat,p)-list'
    else
         print*,'  Input format           : ',inpmode
    endif
    print*,'  Output format          : ',outmode
    print*, " - Periodicity             : ", per
    print*, " - Time check              : ", timecheck 
    print*, " - Model-level trajs (2D)  : ", modlev 
    print*, " - Numerical scheme        : ", imethod, "(0- iterative Euler, 1- 4th Runge-Kutta)"

    ! init missing data value
    mdv = -999.
    print*, " - Missing Data value      : ", mdv

!   --------------------------------------------------------------------------
!   Read grid parameters. Checks and allocate memory
!   --------------------------------------------------------------------------

    ! read the constant grid parameters
    ! (nx,ny,nz,xmin,xmax,ymin,ymax,pollon,pollat)
    ! the negative <-fid> of the file identifier is used as a flag for parameter
    ! retrieval

    filename = dat(1)
    varname  = 'U'
    nx       = 1
    ny       = 1
    nz       = 1
    tload    = -tst
    print*, tload
    call input_open (fid, filename)
    call input_grid(-fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tload,pollon,&
        &           pollat,r1d,r1d,nz,r1d,r1d,rd,timecheck)
    call input_close(fid)

    ! check if the number of levels is too large
    if ( nz .gt. nlevmax ) go to 1000
    
    ! set logical flag for periodic data set (hemispheric or not)
    hem = 0
    if ( per .eq. 0. ) then
       delta = xmax-xmin-360.
       if ( abs(delta + dx) .lt. eps ) then     ! Program aborts: arrays must be closed
          print*, "ERROR: close arrays on files (prog. closear)"
          go to 1000
       else if ( abs(delta) .lt. eps ) then     ! Periodic and hemispheric
          hem = 1
          per = 360.
       end if
    else
       hem = 1
    end if

    ! allocate memory for some meteorological arrays
    allocate(spt0(nx*ny),stat = stat)           ! Surface pressure time0
    if (stat.ne.0) print*,"ERROR: allocating array spt0" 
    allocate(spt1(nx*ny),stat = stat)           ! Surface pressure time1
    if (stat.ne.0) print*,"ERROR: allocating array spt1"
    allocate(uut0(nx*ny*nz),stat = stat)        ! Zonal wind time0
    if (stat.ne.0) print*,"ERROR: allocating array uut0"
    allocate(uut1(nx*ny*nz),stat = stat)           ! Zonal wind time1
    if (stat.ne.0) print*,"ERROR: allocating array uut1"
    allocate(vvt0(nx*ny*nz),stat = stat)        ! Meridional wind time0
    if (stat.ne.0) print*,"ERROR: allocating array vvt0"
    allocate(vvt1(nx*ny*nz),stat = stat)           ! Meridional wind time1
    if (stat.ne.0) print*,"ERROR: allocating array vvt1"
    allocate(wwt0(nx*ny*nz),stat = stat)        ! Zonal wind time0
    if (stat.ne.0) print*,"ERROR: allocating array wwt0"
    allocate(wwt1(nx*ny*nz),stat = stat)           ! Zonal wind time1
    if (stat.ne.0) print*,"ERROR: allocating array wwt1"
    allocate(p3t0(nx*ny*nz),stat = stat)        ! 3D pressure time0
    if (stat.ne.0) print*,"ERROR: allocating array uut0"
    allocate(p3t1(nx*ny*nz),stat = stat)           ! 3D pressure time1
    if (stat.ne.0) print*,"ERROR: allocating array uut1"

    ! get memory for trajectory arrays
    allocate(trainp(ntra,1,ncol))
    allocate(traout(ntra,ntim,4))
    allocate(xx0(ntra)) ! X position (longitude)
    allocate(yy0(ntra)) ! Y position (latitude)
    allocate(pp0(ntra)) ! Pressure
    allocate(leftflag(ntra)) ! Leaving-domain flag
    allocate(zindex(ntra))   ! Vertical index for model-level trajectories

    ! Write some status information
    print*,"--------------CONSTANT GRID PARAMETERS------------------"
    print*
    print*," + xmin  /xmax        : ",xmin,xmax
    print*," + ymin  /ymax        : ",ymin,ymax
    print*," + dx    /dy          : ",dx,dy
    print*," + pollon/pollat      : ",pollon,pollat
    print*," + nx    /ny    /nz   : ",nx,ny,nz
    print*," + per   /hem         : ",per,hem

!   ----------------------------------------------------------------------
!   Initialized the trajectory calculation
!   ----------------------------------------------------------------------

    ! Read start coordinates from file - Format (lon,lat,lev)
    if (inpmode.eq.-1) then
         open(fid,file=strname)
          do i=1,ntra
             read(fid,*) xx0(i),yy0(i),pp0(i)
          enddo
         close(fid)

    !  Read start coordinates from trajectory file - check consistency of ref time
    else
         call ropen_tra(cdfid,strname,ntra,1,ncol,reftmp,vars,inpmode)
         call read_tra (cdfid,trainp,ntra,1,ncol,inpmode)
         do i=1,ntra
            time   = trainp(i,1,1)
            xx0(i) = trainp(i,1,2)
            yy0(i) = trainp(i,1,3)
            pp0(i) = trainp(i,1,4)
         enddo
         call close_tra(cdfid,inpmode)

         if ( ( reftime(1).ne.reftmp(1) ).or.   &
              ( reftime(2).ne.reftmp(2) ).or.   &
              ( reftime(3).ne.reftmp(3) ).or.   &
              ( reftime(4).ne.reftmp(4) ).or.   &
              ( reftime(5).ne.reftmp(5) ) )     &
         then
            print*,' WARNING: Inconsistent reference times'
            write(*,'(5i8)') (reftime(i),i=1,5)
            write(*,'(5i8)') (reftmp (i),i=1,5)
            print*,'Enter a key to proceed...'
            stop 
         endif
    endif 



    ! set sign of time range
    reftime(6) = fbflag * reftime(6)

    ! write some status information
    print*, '--------------------REFERENCE DATE----------------------'
    print*
    write(*,'(a30,i12)') ' Reference time (year)    :', reftime(1)
    write(*,'(a30,i12)') '                (month)   :', reftime(2)
    write(*,'(a30,i12)') '                (day)     :', reftime(3)
    write(*,'(a30,i12)') '                (hour)    :', reftime(4)
    write(*,'(a30,i12)') '                (min)     :', reftime(5)
    write(*,'(a30,i12,a10)') ' Time range               :', reftime(6),' min(s)'



    ! save starting positions
    itim = 1
    do i = 1,ntra
        traout(i,itim,1) = 0.
        traout(i,itim,2) = xx0(i)
        traout(i,itim,3) = yy0(i)
        traout(i,itim,4) = pp0(i)
    end do


    ! init the flag and the counter for trajectories leaving the domain
    leftcount = 0
    do i = 1, ntra
        leftflag(i) = 0
    end do

    ! convert time shifts [tst,ten] from hh.mm to frac
    call hhmm2frac(tst,frac)
    tst = frac
    call hhmm2frac(ten,frac)
    ten = frac

    ! get 3D and surface pressure from first data file
    filename = dat(1)
    call input_open(fid,filename)
    if ( modlev .eqv. .false. ) then
        varname = 'P'
        print*, varname
        call input_grid(fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tload, &
        &               pollon,pollat,p3t1,spt1,nz,ak,bk,stagz,timecheck)
        print*, "What is the prob"
    else
        varname = 'P.ML'
        call input_grid(fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tload, &
        &               pollon,pollat,p3t1,spt1,nz,ak,bk,stagz,timecheck)
    end if

    call input_close(fid)


    ! check that all starting positions are above topography
    do i = 1, ntra

       ! interpolate surface pressure to actual positions (from first input
       ! file)
       x1 = xx0(i)
       y1 = yy0(i)
       call get_index3(xind,yind,pind,x1,y1,1050.,3,p3t1,spt1,nx,ny,nz,xmin,&
      &                ymin,dx,dy,mdv)

       sp = int_index3(spt1,nx,ny,1,xind,yind,1.,mdv)
       ! decide whether to keep the trajectory
       if (pp0(i) .gt. sp ) then
            write(*,'(a30,3f10.2)') &
     &               'WARNING: starting point below topography ', &
     &               xx0(i),yy0(i),pp0(i)
            leftflag(i) = 1
       endif

    end do

    ! Special handling for model-level (2D) trajectories - get the vertical
    ! index for each trajectory - which will remain fixed.

    if ( modlev .eqv. .true. ) then
       do i = 1, ntra
          x1 = xx0(i)
          y1 = yy0(i)
          p1 = pp0(i)
          call get_index3(xind,yind,pind,x1,y1,p1,3,p3t1,spt1,nx,ny,nz,xmin,&
     &                    ymin,dx,dy,mdv)
          zindex(i) = pind
       end do

       do i = 1, nz
          print*, i, p3t1( 189 + (141 - 1)*nx + ( i - 1 )*nx*ny )
       end do

       ! write info about zindex range of starting positions
       zindexmin = zindex(1)
       zindexmax = zindex(1)

       do i = 2, ntra
          if ( zindex(i) .lt. zindexmin ) zindexmin = zindex(i)
          if ( zindex(i) .gt. zindexmax ) zindexmax = zindex(i)
       end do

       ! write some status information
       print*
       print*, '---- INDEX RANGE OF MODEL-LEVEL TRAJECTORIES ------------'
       print*
       print*, ' Zindex(min)    :', zindexmin
       print*, ' Zindex(max)    :', zindexmax 
    end if

!   --------------------------------------------------------------------
!   Loop to calculate trajectories
!   --------------------------------------------------------------------

    ! Write some status information
    print*
    print*, ' ---------------TRAJECTORIES------------------- '
    print*

    ! Set the time for first data file (depending on forward/backward mode)
    if ( fbflag .eq. 1 ) then
       tstart = -tst
    else if ( fbflag .eq. -1 ) then
       tstart = tst
    end if

    ! Set the minute counter for input
    wstep = 0

    ! Read wind fields and vertical grid from files
    filename = dat(1)

    call frac2hhmm(tstart,tload)

    write(*,'(a16,a20,f9.2)') '  (file,time) : ',trim(filename),tload
 
    call input_open(fid,filename)

    varname = 'U'                       ! U
    print*, varname
    call input_wind(fid,varname,uut1,tload,stagz,mdv,xmin,xmax,ymin,ymax, &
  &                 dx,dy,nx,ny,nz,timecheck)

    varname = 'V'                       ! V
    print*, varname
    call input_wind(fid,varname,vvt1,tload,stagz,mdv,xmin,xmax,ymin,ymax, &
  &                 dx,dy,nx,ny,nz,timecheck)

    if ( modlev .eqv. .false. ) then

       varname = 'OMEGA'                ! OMEGA
       print*, varname
       call input_wind(fid,varname,wwt1,tload,stagz,mdv,xmin,xmax,ymin,ymax, &
  &                    dx,dy,nx,ny,nz,timecheck)
        
    end if

    if ( modlev .eqv. .false. ) then
                                        ! GRID - AK,BK -> P
       call input_grid(fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tload, &
  &                    pollon,pollat,p3t1,spt1,nz,ak,bk,stagz,timecheck)

    else
                                        ! GRID - P, PS    
       varname = 'P.ML'
       call input_grid(fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tload, &
  &                    pollon,pollat,p3t1,spt1,nz,ak,bk,stagz,timecheck)

    end if

    call input_close(fid)


    ! Loop over all input files (with timestep is {timeinc} )

    do itm = 1, numdat - 1

        ! Calculate actual and next time
        time0 = tstart + real(itm-1)*timeinc*fbflag
        time1 = time0  + timeinc*fbflag

        ! Copy old velocities and pressure fields to new ones
        do i = 1, nx*ny*nz
           uut0 (i) = uut1 (i) 
           vvt0 (i) = vvt1 (i)
           wwt0 (i) = wwt1 (i)
           p3t0 (i) = p3t1 (i)
        end do 

        do i = 1, nx*ny
           spt0 (i) = spt1 (i)
        end do

        ! Read wind fields and surface pressure at next time
        filename = dat(itm + 1)

        call frac2hhmm(time1,tload)
        write (*, '(a16,a20,f9.2)') '    (file, time) : ',trim(filename),tload

        call input_open (fid,filename)

        varname = 'U'
        print*, varname
        call input_wind (fid,varname,uut1,tload,stagz,mdv,xmin,xmax,ymin,ymax, &
                     &   dx,dy,nx,ny,nz,timecheck)

        varname = 'V'
        print*, varname
        call input_wind (fid,varname,vvt1,tload,stagz,mdv,xmin,xmax,ymin,ymax, &
                     &   dx,dy,nx,ny,nz,timecheck)

        if ( modlev .eqv. .false. ) then
           varname = 'OMEGA'
           print*, varname
           call input_wind (fid,varname,wwt1,tload,stagz,mdv,xmin,xmax,ymin,ymax, &
                       &    dx,dy,nx,ny,nz,timecheck)
        
                                                        ! GRID - AK,BK -> P
           call input_grid (fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tload, &
                       &    pollon,pollat,p3t1,spt1,nz,ak,bk,stagz,timecheck) 
        
        else

                                                        ! GRID - P,PS
           varname = 'P.ML'
           print*, varname
           call input_grid (fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tload, &
                            pollon,pollat,p3t1,spt1,nz,ak,bk,stagz,timecheck)

        end if

        call input_close(fid)

        ! Determine the first and last loop indices
        if (numdat .eq. 2 ) then
           filo = nint(tst/ts) + 1
           lalo = nint((timeinc - ten)/ts )
        else if (itm .eq. 1) then
           filo = nint(tst/ts) + 1
           lalo = nint(timeinc/ts)
        else if (itm.eq. (numdat - 1)) then
           filo = 1
           lalo = nint((timeinc - ten)/ts)
        else
           filo = 1
           lalo = nint(timeinc/ts)
        end if

        ! Split the interval <timeinc> into computational time steps <ts>
        do iloop = filo, lalo

           ! Calculate relative time position (0=beginning, 1=end)
           reltpos0 = (( real(iloop) - 1.)*ts)/timeinc
           reltpos1 = real(iloop)*ts/timeinc

           ! Timestep for all trajectories
           do i = 1, ntra
              
              ! Check if trajectory has already left the data domain
              if ( leftflag(i) .ne. 1 ) then

                 ! 3D: Iterative Euler timestep (x0,y0,p0 -> x1,y1,p1)
                 if ( (imethod .eq.     1    ) .and.        &
        &             (modlev  .eqv.  .false.) ) then
                    call euler_3d( xx1,yy1,pp1,leftflag(i),xx0(i),yy0(i),pp0(i), &
        &                          reltpos0,reltpos1,ts*3600,numit,jflag,mdv,    &
        &                          wfactor,fbflag,spt0,spt1,p3t0,p3t1,uut0,uut1, &
        &                          vvt0,vvt1,wwt0,wwt1,xmin,ymin,dx,dy,per,hem,  &
        &                          nx,ny,nz)

                ! 3D: Runge-Kutta timestep (x0,y0,p0 -> x1,y1,p1)
                else if ( (imethod .eq.      3    ) .and.    &
        &                 (modlev .eqv. .false.   ) ) then
                    print*, 'i, xx0, yy0, pp0',i,xx0(i),yy0(i),pp0(i)
                    call runge( xx1,yy1,pp1,leftflag(i),xx0(i),yy0(i),pp0(i),  &
        &                       reltpos0,reltpos1,ts*3600,numit,jflag,mdv,     &
        &                       wfactor,fbflag,spt0,spt1,p3t0,p3t1,uut0,uut1,  &
        &                       vvt0,vvt1,wwt0,wwt1,xmin,ymin,dx,dy,per,hem,   &
        &                       nx,ny,nz)
                    print *,  'i, xx1, yy1, pp1',i, xx1, yy1, pp1    ! check
   

                ! 2D: MODEL-LEVEL Iterative Euler timestep (x0,y0,p0 ->
                ! x1,y1,p1)
                else if ( (imethod .eq. 1       ) .and. &
        &                 (modlev .eqv. .true.  ) ) then
                    call euler_2d(xx1,yy1,pp1,leftflag(i),xx0(i),yy0(i),pp0(i),&
        &                         zindex(i),reltpos0,reltpos1,ts*3600,numit,   &
        &                         jflag,mdv,wfactor,fbflag,spt0,spt1,p3t0,p3t1,&
        &                         uut0,uut1,vvt0,vvt1,xmin,ymin,dx,dy,per,hem, &
        &                         nx,ny,nz)

                end if

                ! Update trajectory position, or increase number of trajectories
                ! leaving domain
                if ( leftflag(i) .eq. 1 ) then
                   leftcount = leftcount + 1
                   print*, '           -> Trajectory ',i,' leaves domain'
                else
                   xx0(i) = xx1
                   yy0(i) = yy1
                   pp0(i) = pp1
                end if
                
              ! Trajectories has already left data domain ( mark as mdv)
              else
                xx0(i) = mdv
                yy0(i) = mdv
                pp0(i) = mdv

              end if

        end do

        ! Save positions only every deltout minutes
        delta = aint(iloop*60*ts/deltout) - iloop*60*ts/deltout

        if ( abs(delta) .lt. eps) then
        
           time = time0 + reltpos1*timeinc*fbflag
           itim = itim + 1
           if ( itim .le. ntim ) then
              do i = 1, ntra
                 call frac2hhmm (time, tload )
                 traout(i,itim,1) = tload
                 traout(i,itim,2) = xx0(i)
                 traout(i,itim,3) = yy0(i)
                 traout(i,itim,4) = pp0(i)
              end do
           end if
        end if

        end do
    
    end do

    !  Write trajectory file
      vars(1)  ='time'
      vars(2)  ='lon'
      vars(3)  ='lat'
      vars(4)  ='p'
      call wopen_tra(cdfid,cdfname,ntra,ntim,4,reftime,vars,outmode)
      call write_tra(cdfid,traout,ntra,ntim,4,outmode)
      call close_tra(cdfid,outmode)

    ! Write some status information, and end of program message
    print*
    print*,"-------------------STATUS INFORMATION---------------------"
    print*
    print*,"   # leaving domain         ", leftcount
    print*,"   # staying in domain      ", ntra - leftcount
    print*
    print*,"              ***  END OF PROGRAM CALTRA ***              "
    print*,"=========================================================="

        

1000 continue
end program caltra
