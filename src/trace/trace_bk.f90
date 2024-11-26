program trace

! ***********************************************************************
! * Trace fields along trajectories
! ***********************************************************************

    use mpi
    use interp
    use iotra
    use caltime
    use calvar
    use ncinput
    use common_mpi

    implicit none

    ! -------------------------------------------------------------------
    ! Declaratione of parameters
    ! -------------------------------------------------------------------

    ! Maximum number of levels for input files
    integer,parameter:: nlevmax = 100

    ! Maximum number of input files (dates, length of trajectories)
    integer,parameter:: ndatmax = 500

    ! Numerical epsilon (for float comparison)
    real,parameter:: eps = 1E-4

    ! Conversion factors
    real,parameter:: pi180 = 4*atan(1.)/180.    ! deg -> rad
    real,parameter:: deg2km = 1.112E5           ! deg -> km
    real,parameter:: pir = 255032235.95489      ! 2*Pi*R^2

    ! -------------------------------------------------------------------
    ! Declaratione of variables
    ! -------------------------------------------------------------------
    
    ! Input and output format for trajectories
    integer     :: inpmode, outmode

    ! Input parameters
    character*80:: inpfile, outfile     ! Input and output trajectories files
    integer     :: ntra                 ! Number of trajectories
    integer     :: ntim                 ! Number of times per trajectory
    integer     :: ncol                 ! Number of columns (including time,lon,lat,p)
    integer     :: ntrace0              ! Number of trace variables
    character*80:: tvar0(200)           ! Tracing variable (with mode specification)
    character*80:: tvar(200)            ! Tracing variable name
    character*80:: tfile                ! Tracing file name prefix
    real        :: fac(200)             ! Scaling factor
    real        :: shift_val(200)       ! Shift in space and time relative to trajectory position
    character*80:: shift_dir(200)       ! Direction of shift
    character*80:: shift_rel(200)       ! Operator/relator for variable
    integer     :: compfl(200)          ! Computation flag (1=compute)
    integer     :: numdat               ! Number of input files
    character*20:: dat(ndatmax)         ! Dates of input files
    real        :: timeinc              ! Time increment between input files
    real        :: tst                  ! Time shift of start relative to first data file
    real        :: ten                  ! Time shift of end relatiev to first data file
    character*20:: startdate            ! First time/date on trajectory
    character*20:: enddate              ! Last time/date on trajectory
    integer     :: ntrace1              ! Count trace and additional variables
    logical     :: timecheck            ! Either 'yes' or 'no'
    character(len=80) :: intmode        ! Interpolation mode ('normal','nearest','clustering','circle_avg','circle_max','circle_min')
    integer     :: year,month,day,hour,minute,time_range

    ! Trajectories
    real,allocatable, dimension (:,:,:) :: trainp          ! Input trajectories (ntra,ntim,ncol)
    real,allocatable, dimension (:,:,:) :: traint          ! Internal trajectories (ntra,ntim,ncol+ntrace1)
    real,allocatable, dimension (:,:,:) :: traout          ! Output trajectories (ntra,ntim,ncol+ntrace0)
    integer     :: reftime(6)           ! Reference date
    character*80:: varsinp(200)         ! Field names for input trajectory
    character*80:: varsint(200)         ! Field names for internal trajectory
    character*80:: varsout(200)         ! Field names for output trajectory
    integer :: fid,fod                  ! File identifier for inp and out trajectories
    real :: x0,y0,p0                    ! Position of air parcel (physical space)
    real :: reltpos0                    ! Relative time of air parcel
    real :: xind,yind,pind              ! Position of air parcel (grid space)
    integer :: fbflag                   ! Flag for forward (1)or backward (-1) trajectories
    integer :: fok(200)                 ! Flag whether field is ready
    
    ! Eigenvalues 
    double precision, dimension (2)     :: eig_re, eig_im   ! Eigenvalues of complex

    ! Meteorogogical fields
    real,allocatable,dimension (:)      :: spt0,spt1    ! Surface pressure(nx*ny)
    real,allocatable,dimension (:)      :: p3t0,p3t1    ! 3d-pressure(nx*ny*nz)
    real,allocatable,dimension (:)      :: f3t0,f3t1    ! 3d-field(nx*ny*nz)

    ! Grid description
    real        :: pollon,pollat        ! Longitude/Latitude of pole
    real        :: ak(100)              ! Vertical layers and levels
    real        :: bk(100)
    real        :: xmin,xmax            ! Zonal grid extension
    real        :: ymin,ymax            ! Meridional grid extension
    integer     :: nx,ny,nz             ! Grid dimensions
    real        :: dx,dy                ! Horizontal grid resolution
    integer     :: hem                  ! Flag for hemispheric domain
    integer     :: per                  ! Flag for periodic domain
    real        :: stagz                ! Vertical staggering
    real        :: mdv                  ! Missing data value

    ! Auxiliary variables
    integer     :: i,itm,j,k,l,m,n
    real        :: rd
    real,allocatable, dimension (:) :: r1d
    character*80:: filename,varname
    real        :: time0,time1,reltpos
    integer     :: itime0,itime1
    integer     :: stat
    real        :: tstart
    integer     :: iloaded0,iloaded1
    real        :: f0
    real        :: frac
    real        :: tload,tfrac
    integer     :: ind
    integer     :: ind1,ind2,ind3,ind4,ind5
    integer     :: ind6,ind7,ind8,ind9,ind0
    real        :: tmp1,tmp2,tmp3,tmp4
    integer     :: noutside
    real        :: delta
    integer     :: itrace0
    character*80:: string
    character*80:: tmpstring
    integer     :: err_c1,err_c2,err_c3

    ! Special interpolation variables
    real        :: dist,circlesum,circlemax,circlemin,circleavg,radius  ! distance (great circle), sum/max/min/avg in circle, radius of circle
    integer     :: ist,jst,kst,sp,ml,mr,nd,nu   ! ijk in stack, sp=stack counter, ml (left), mr (right), nd (down), nu (up)
    integer,allocatable,dimension(:,:):: connect
    integer,allocatable,dimension(:) :: stackx,stacky   ! lon/lat of stack
    real,allocatable,dimension(:) :: circlelon,circlelat     ! value of f, lon and lat in circle
    real,allocatable,dimension(:) :: circlef,circlearea      ! value of f, area in circle
    real,allocatable,dimension(:) :: longrid,latgrid         ! arrays of longitude and latitude of grid points
!tmp    logical     :: file_exists

    ! MPI
    integer :: ierr,nprocs,myrank,irank
    integer :: jsta,jend,ista,iend,iprev,inext,num_rows
    integer :: istatus(MPI_STATUS_SIZE)
    integer :: debug,irec ! record and debug
    integer :: ureq1
    integer,allocatable :: itype(:),ureq(:)

    namelist /params/ fbflag,timecheck,tst,ten
    namelist /input/  startdate,enddate,inpfile,numdat,timeinc,per,ntra,ntim,ncol
    namelist /tracevars/ ntrace0,tvar,tfile
    namelist /interparams/ intmode,radius
    namelist /output/ outfile

    !print *, "myrank =",myrank
    !!!!!!!! MPI IS COMING !!!!!!!!!!
    !print*, 'ntra',ntra
    ! indexing the MPI jobs

    ! ---------------------------------------------------------------------------
    ! initialize MPI communications
    ! ---------------------------------------------------------------------------
     call MPI_INIT(ierr)
     call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
     call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

    ! ---------------------------------------------------------------------------
    ! Start of program, Read parameters, get grid parameters
    ! ---------------------------------------------------------------------------

    ! Let 0 be the root process 
    ! Write start message
    if (myrank.eq.0) then
    print*, '================================================================'
    print*, '               *** START OF PROGRAM TRACE ***                   '
    print*
    end if
    ! Read parameters
    open (100, file='namelist.trace')
        read (100,params) 
        read (100,input)
        read (100,tracevars)
        read (100,interparams)
        read (100,output)
    close (100)


!tmp   INQUIRE(FILE="input.lst", EXIST=file_exists)
!tmp   print*, file_exists
   
!    print*, fbflag, startdate, numdat
!    open (200, file='input.lst',status='old')
!        if ( fbflag .eq. 1 ) then
!             do i = 1,numdat
!                read (200,*) dat(i)
!             end do
!        else
!             do i = 1,numdat,-1
!                read (200,*) dat(i)
!                write(*,*) dat(i)," is available"
!             end do
!        end if 
!    close (200)
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
           if (myrank.eq.0) then
           print*, "ERROR: fbflag is not sufficient, -1/1 only", fbflag
           end if
           go to 1005
        end if
    close(101)


    i = 0
    open (300, file=tfile, status='old')
        read (300,*)
        read (300,*)
        read (300,*)
        read (300,*)

10      continue
        read (300,'(all)',end=11) tmpstring

        if (myrank.eq.0) then
        print*, trim(adjustl(tmpstring))
        end if

        do i = 1,ntrace0
            if ( tvar(i) .eq. trim(tmpstring(1:10)) ) then
               read(tmpstring,*) tvar(i),fac(i),compfl(i) 
            end if
        end do
        go to 10
11      continue
    close (300)

    ! error if radius < 0
    if (((intmode .eq. "circle_avg") .or. (intmode .eq. "circle_min") .or. &
     &   (intmode .eq. "circle_max")) .and. (radius .lt. 0)) then
         if (myrank.eq.0) then
         print*,'ERROR (circle): radius < 0!'
         end if
         
         stop
    endif

    ! Remove commented tracing fields
    itrace0 = 1
    do while ( itrace0.le.ntrace0)
        string = tvar(itrace0)
        if ( string(1:1).eq.'#' ) then
            do i=itrace0,ntrace0-1
                tvar(i)   = tvar(i+1)
                fac(i)    = fac(i+1)
                compfl(i) = compfl(i+1)
            enddo
            ntrace0 = ntrace0 - 1
        else
            itrace0 = itrace0 + 1
        endif
    enddo

    ! Save the tracing variable (including all mode specifications)
    do i=1,ntrace0
        tvar0(i) = tvar(i)
    enddo

    ! Set the formats of the input and output files
    call mode_tra(inpmode,inpfile)
    if (inpmode.eq.-1) inpmode=1
    call mode_tra(outmode,outfile)
    if (outmode.eq.-1) outmode=1

    ! Convert time shifts <tst,ten> from <hh.mm> into fractional time
    call hhmm2frac(tst,frac)
    tst = frac
    call hhmm2frac(ten,frac)
    ten = frac


    ! Read the constant grid parameters
    ! (nx,ny,nz,xmin,xmax,ymin,ymax,pollon,pollat)
    ! The negative <-fid> of the file identifier is used as a flag for parameter
    ! retrieval
    filename = dat(1)
    varname  = 'U'
    call input_open (fid,filename)
    call input_grid (-fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tstart,pollon,pollat,r1d,r1d,nz,r1d,r1d,rd,timecheck)
    call input_close(fid)

    ! Allocate memory for some meteorological arrays
    allocate(spt0(nx*ny),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array spt0 ***'   ! Surface pressure
    allocate(spt1(nx*ny),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array spt1 ***'
    allocate(p3t0(nx*ny*nz),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array p3t0 ***'   ! Pressure
    allocate(p3t1(nx*ny*nz),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array p3t1 ***'
    allocate(f3t0(nx*ny*nz),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array p3t0 ***'   ! Tracing field
    allocate(f3t1(nx*ny*nz),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array p3t1 ***'

    ! Get memory for trajectory arrays
    allocate(trainp(ntra,ntim,ncol),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array tra      ***'

    ! allocate memory for circle mode
    if ( (intmode.eq.'circle_avg') .or. (intmode.eq.'circle_min') .or. (intmode.eq.'circle_max') ) then

        allocate(connect(nx,ny),stat=stat)
        if (stat.ne.0) print*,'*** error allocating connect ***'
        allocate(stackx(nx*ny),stat=stat)
        if (stat.ne.0) print*,'*** error allocating stackx ***'
        allocate(stacky(nx*ny),stat=stat)
        if (stat.ne.0) print*,'*** error allocating stacky ***'
        allocate(circlelon(nx*ny),stat=stat)
        if (stat.ne.0) print*,'*** error allocating circlelon ***'
        allocate(circlelat(nx*ny),stat=stat)
        if (stat.ne.0) print*,'*** error allocating circlelat ***'
        allocate(circlef(nx*ny),stat=stat)
        if (stat.ne.0) print*,'*** error allocating circlef ***'
        allocate(circlearea(nx*ny),stat=stat)
        if (stat.ne.0) print*,'*** error allocating circlearea ***'
        allocate(longrid(nx),stat=stat)
        if (stat.ne.0) print*,'*** error allocating longrid ***'
        allocate(latgrid(ny),stat=stat)
        if (stat.ne.0) print*,'*** error allocating latgrid ***'
        !print*, 'dx, dy, xmin, ymin',dx, dy, xmin, ymin ! check
        do m = 1, nx
            longrid(m) = xmin + dx * (m-1)
        end do
        do n = 1, ny
            latgrid(n) = ymin + dy * (n-1)
        end do

    end if

    ! Set the flag for periodic domains
    if ( abs(xmax - xmin - 360.) .lt. eps ) then
        per = 1
    else if ( abs(xmax - xmin - 360. + dx ) .lt. eps ) then
        per = 2
    else 
        per = 0
    end if

    ! Set logical flag for periodic dataset (hemispheric or not)
    hem = 0
    if ( per .eq. 0 ) then
        delta = xmax - xmin - 360.
        if ( abs(delta+dx) .lt. eps ) then              ! Program abours: arrays must be closed
           if (myrank.eq.0) then
             print*,' ERROR: arrays must be closed... Stop'
           end if
        else if (abs(delta) .lt. eps ) then             ! Periodic and hemispheric
             hem = 1
             per = 360.
        end if
    else
        hem = 1
    end if

    ! Write some status information
    if (myrank.eq.0) then
    print*,'-------------------- INPUT PARAMETERS ----------------------'
    print*
    print*,'   Input trajectory file    : ',trim(inpfile)
    print*,'   Output trajectory file   : ',trim(outfile)
    print*,'   Format of input file     : ',inpmode
    print*,'   Format of output file    : ',outmode
    print*,'   Forward/backward         : ',fbflag
    print*,'   # tra                    : ',ntra
    print*,'   # col                    : ',ncol
    print*,'   # tim                    : ',ntim
    print*,'   No time check            : ',timecheck
    print*,'   Interpolation mode       : ',trim(intmode)

    do i = 1, ntrace0
        if ( compfl(i) .eq. 0 ) then
             print*,'   Tracing field           : ', trim(tvar(i)), fac(i),  ' 0'
        else
             print*,'   Tracing field           : ', trim(tvar(i)), fac(i),  ' 1'
        end if
    end do

    print*
    print*,'------------------- INPUT DATA FILES ----------------------'
    print*
    call frac2hhmm(tstart,tload)
    print*,'  Time of 1st data file     : ',tload
    print*,'  # input file              : ',numdat
    print*,'  time increment            : ',timeinc
    call frac2hhmm(tst,tload)
    print*,'  Shift of start            : ',tload
    call frac2hhmm(ten,tload)
    print*,'  Shift of end              : ',tload
    print*,'  First/Last input file     : ',trim(dat(1)),'  ...  ',trim(dat(2))
    print*,'  Primary variables         : ',(trim(tvar(i))//'  ',i=1,ntrace0)

    print*
    print*,'------------------- CONSTANT GRID PARAMETERS ----------------------'
    print*
    print*,'   xmin,xmax                : ',xmin,xmax
    print*,'   ymin,ymax                : ',ymin,ymax
    print*,'   dx,dy                    : ',dx,dy
    print*,'   pollon,pollat            : ',pollon,pollat
    print*,'   nx,ny,nz                 : ',nx,ny,nz
    print*,'   per,hem                  : ',per,hem
    end if

    ! -------------------------------------------------------------------------
    ! Load the input trajectories
    ! -------------------------------------------------------------------------

    ! Read the input trajectory file
    call ropen_tra(fid,inpfile,ntra,ntim,ncol,reftime,varsinp,inpmode)
    call read_tra (fid,trainp,ntra,ntim,ncol,inpmode)
    call close_tra(fid,inpmode)


    ! Check that first 4 columns correspond to time,lon,lat,p
    if ( (varsinp(1).ne.'time') .or.                            &
       & (varsinp(2).ne.'xpos').and.(varsinp(2).ne.'lon') .or.  &
       & (varsinp(3).ne.'ypos').and.(varsinp(3).ne.'lat') .or.  &
       & (varsinp(4).ne.'ppos').and.(varsinp(4).ne.'p'  ) ) then

        if (myrank.eq.0) then
           print*,' ERROR: problem with input trajectories ...'
        end if
        stop

    end if

    varsinp(1) = 'time'
    varsinp(2) = 'lon'
    varsinp(3) = 'lat'
    varsinp(4) = 'p'

    if (myrank.eq.0) then
    ! Write some status information of the input trajectories
    print*
    print*,'------------------ INPUT TRAJECTORIES -------------------'
    print*
    print*,'  Start date                : ',trim(startdate)
    print*,'  End date                  : ',trim(enddate)
    print*,'  Reference time (year)     : ',reftime(1)
    print*,'                 (month)    : ',reftime(2)
    print*,'                 (date)     : ',reftime(3)
    print*,'                 (hour)     : ',reftime(4)
    print*,'                 (minute)   : ',reftime(5)
    print*,'  Time range (in min)       : ',reftime(6)
    do i = 1, ncol
        print*,'  Var                           : ',i,trim(varsinp(i))
    end do
    print*

    ! Check that first time is 0 - otherwise the tracing will produce wrong
    ! results because later in the code only absolute times are considered:
    ! [itime] = int(abs(tfrac - tstart)/timeinc) + 1.
    if ( abs(trainp(1,1,1)) .gt. eps ) then
        print*,' ERROR: First time of trajectory must be 0, i.e. correspond to '
        print*,' the reference date. Otherwise the tracing will give wrong     '
        print*,' results... STOP'
    end if

    end if
    ! ------------------------------------------------------------------------
    ! CHeck dependencies for trace fields which must be calculated 
    ! ------------------------------------------------------------------------

    ! Set the counter for extra fields
    ntrace1 = ntrace0
    ! Loop over all tracing variables
    i = 1

    do while ( i .le. ntrace1 ) 

        ! Skip fields which must be available on the input files
        if ( i.le.ntrace0 ) then
             if (compfl(i).eq.0) go to 1000
        end if


        ! Get the dependencies for potential temperature (TH)
        if ( tvar(i) .eq. 'TH' ) then
             varname = 'P'
             call add2list(varname,tvar,ntrace1)
             varname = 'T'
             call add2list(varname,tvar,ntrace1)
        
        ! Get the dependencies for density (RHO)
        else if ( tvar(i) .eq. 'RHO' ) then
             varname = 'P'
             call add2list(varname,tvar,ntrace1)
             varname = 'T'
             call add2list(varname,tvar,ntrace1)

        ! Get the dependecies for relative humidity (RH)
        else if ( tvar(i) .eq. 'RH' ) then
             varname = 'P'
             call add2list(varname,tvar,ntrace1)
             varname = 'T'
             call add2list(varname,tvar,ntrace1)
             varname = 'Q'
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for equivalent potential temperature (THE)
        else if ( tvar(i) .eq. 'THE' ) then
             varname = 'P'
             call add2list(varname,tvar,ntrace1)
             varname = 'T'
             call add2list(varname,tvar,ntrace1)
             varname = 'Q'      
             call add2list(varname,tvar,ntrace1)
        
        ! Get the dependencies for latent heating rate (LHR)
        else if ( tvar(i) .eq. 'LHR' ) then
             varname = 'P'
             call add2list(varname,tvar,ntrace1)
             varname = 'T'
             call add2list(varname,tvar,ntrace1)
             varname = 'Q'
             call add2list(varname,tvar,ntrace1)
             varname = 'OMEGA'
             call add2list(varname,tvar,ntrace1)
             varname = 'RH'
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for wind speed (VEL)
        else if ( tvar(i) .eq. 'VEL' ) then
             varname = 'U'
             call add2list(varname,tvar,ntrace1)
             varname = 'V'
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for wind direction (DIR)
        else if ( tvar(i) .eq. 'DIR' ) then
             varname = 'U'
             call add2list(varname,tvar,ntrace1)
             varname = 'V'
             call add2list(varname,tvar,ntrace1)
        
        ! Get the dependenceis for du/dx (DUDX)
        else if ( tvar(i) .eq. 'DUDX' ) then
             varname='U'                              ! U
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLON'
             call add2list(varname,tvar,ntrace1)

        ! Get the dependenceis for du/dy (DUDY)
        else if ( tvar(i) .eq. 'DUDY' ) then   
             varname='U' ! U
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLAT'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLAT'
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for dv/dx
        else if ( tvar(i) .eq. 'DVDX' ) then
             varname='V'                              ! V
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLON'
             call add2list(varname,tvar,ntrace1)
        
        ! Get the dependencies for dv/dy (DVDY)
        else if ( tvar(i) .eq. 'DVDY' ) then
             varname='V'                              ! V
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLAT'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLAT'
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for eigenvalues
        else if ( (tvar(i) .eq. 'R_EIGVAL1') .or. (tvar(i) .eq. 'R_EIGVAL2') .or.    &
                  (tvar(i) .eq. 'I_EIGVAL1') .or. (tvar(i) .eq. 'I_EIGVAL2') ) then
             varname='U'                              ! U
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLAT'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLAT'
             call add2list(varname,tvar,ntrace1)

             varname='V'                              ! V
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLAT'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLAT'
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for du/dp (DUDP)
        elseif ( tvar(i).eq.'DUDP' ) then
             varname='U'                              ! U:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='P'                              ! U:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='U:+1DP'                              ! U:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='U:-1DP'                              ! U:-1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:+1DP'                              ! P:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:-1DP'                              ! P:-1DP
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for dv/dp (DVDP)
        elseif ( tvar(i).eq.'DVDP' ) then
             varname='V'                              ! V
             call add2list(varname,tvar,ntrace1)
             varname='P'                              ! P
             call add2list(varname,tvar,ntrace1)
             varname='V:+1DP'                              ! V:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='V:-1DP'                              ! V:-1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:+1DP'                              ! P:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:-1DP'                              ! P:-1DP
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for dt/dx (DTDX)
        elseif ( tvar(i).eq.'DTDX' ) then
             varname='T'                              ! T
             call add2list(varname,tvar,ntrace1)
             varname='T:+1DLON'                            ! T:+1DLON
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DLON'                            ! T:-1DLON
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for dth/dy (DTHDY)
        elseif ( tvar(i).eq.'DTHDY' ) then
             varname='T'                              ! T
             call add2list(varname,tvar,ntrace1)
             varname='T:+1DLAT'                            ! T:+1DLON
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DLAT'                            ! T:-1DLON
             call add2list(varname,tvar,ntrace1)
             varname='P'                                   ! P
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for dth/dx (DTHDX)
        elseif ( tvar(i).eq.'DTHDX' ) then
             varname='T'                              ! T
             call add2list(varname,tvar,ntrace1)
             varname='T:+1DLON'                            ! T:+1DLON
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DLON'                            ! T:-1DLON
             call add2list(varname,tvar,ntrace1)
             varname='P'                                   ! P
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for dt/dy (DTDY)
        elseif ( tvar(i).eq.'DTDY' ) then
             varname='T'                              ! T
             call add2list(varname,tvar,ntrace1)
             varname='T:+1DLAT'                            ! T:+1DLON
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DLAT'                            ! T:-1DLON
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for dt/dp (DTDP)
        elseif ( tvar(i).eq.'DTDP' ) then
             varname='T'                              ! T
             call add2list(varname,tvar,ntrace1)
             varname='P'                                   ! P
             call add2list(varname,tvar,ntrace1)
             varname='T:+1DP'                              ! T:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DP'                              ! T:-1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:+1DP'                              ! P:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:-1DP'                              ! P:-1DP
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for dth/dp (DTHDP)
        elseif ( tvar(i).eq.'DTHDP' ) then
             varname='T'                                   ! T
             call add2list(varname,tvar,ntrace1)
             varname='P'                                   ! P
              call add2list(varname,tvar,ntrace1)
             varname='T:+1DP'                              ! T:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DP'                              ! T:-1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:+1DP'                              ! P:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:-1DP'                              ! P:-1DP
             call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for squared Brunt Vaiäläa frequency (NSQ)
        elseif ( tvar(i).eq.'NSQ' ) then
             varname='T'                                   ! T
             call add2list(varname,tvar,ntrace1)
             varname='P'                                   ! P
              call add2list(varname,tvar,ntrace1)
             varname='T:+1DP'                              ! T:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DP'                              ! T:-1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:+1DP'                              ! P:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:-1DP'                              ! P:-1DP                                ! DTHDP
             call add2list(varname,tvar,ntrace1)
             !varname='TH'                                   ! TH
             !call add2list(varname,tvar,ntrace1)
             !varname='RHO'                                  ! RHO
             !call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for relative vorticity (RELVORT)
        elseif ( tvar(i).eq.'RELVORT' ) then
             varname='U'                              ! U
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLAT'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLAT'
             call add2list(varname,tvar,ntrace1)

             varname='V'                              ! V
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLON'
             call add2list(varname,tvar,ntrace1)

             !varname='U'                                    ! U
             !call add2list(varname,tvar,ntrace1)
             !varname='DUDY'                                 ! DUDY
             !call add2list(varname,tvar,ntrace1)
             !varname='DVDX'                                 ! DVDX
             !call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for relative vorticity (ABSVORT)
        elseif ( tvar(i).eq.'ABSVORT' ) then
             varname='U'                              ! U
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLAT'             
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLAT'
             call add2list(varname,tvar,ntrace1)

             varname='V'                              ! V
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLON'
             call add2list(varname,tvar,ntrace1)

             !varname='U'                                    ! U
             !call add2list(varname,tvar,ntrace1)
             !varname='DUDY'                                 ! DUDY
             !call add2list(varname,tvar,ntrace1)
             !varname='DVDX'                                 ! DVDX
             !call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for divergence (DIV)
        elseif ( tvar(i).eq.'DIV' ) then

             varname='U'                              ! U
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLON'             
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLON'
             call add2list(varname,tvar,ntrace1)

             varname='V'                              ! V
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLAT'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLAT'
             call add2list(varname,tvar,ntrace1)

             !varname='V'                                    ! U
             !call add2list(varname,tvar,ntrace1)
             !varname='DUDX'                                 ! DUDX
             !call add2list(varname,tvar,ntrace1)
             !varname='DVDY'                                 ! DVDY
             !call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for deformation (DEF)
        elseif ( tvar(i).eq.'DEF' ) then
             varname='U'                              ! U
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLAT'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLAT'
             call add2list(varname,tvar,ntrace1)

             varname='V'                              ! V
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLAT'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLAT'
             call add2list(varname,tvar,ntrace1)


             !call add2list(varname,tvar,ntrace1)
             !varname='DUDX'                                 ! DUDX
             !call add2list(varname,tvar,ntrace1)
             !varname='DVDY'                                 ! DVDY
             !call add2list(varname,tvar,ntrace1)
             !varname='DUDY'                                 ! DUDY
             !call add2list(varname,tvar,ntrace1)
             !varname='DVDX'                                 ! DVDX
             !call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for potential vorticity (PV)
        elseif ( tvar(i).eq.'PV' ) then
             ! ABSVORT
             varname='U'                              ! U
             call add2list(varname,tvar,ntrace1)
             varname = 'U:+1DLAT'
             call add2list(varname,tvar,ntrace1)
             varname = 'U:-1DLAT'
             call add2list(varname,tvar,ntrace1)

             varname='V'                              ! V
             call add2list(varname,tvar,ntrace1)
             varname = 'V:+1DLON'
             call add2list(varname,tvar,ntrace1)
             varname = 'V:-1DLON'
             call add2list(varname,tvar,ntrace1)
 
             ! DTHDP
             varname='T'                                   ! T
             call add2list(varname,tvar,ntrace1)
             varname='P'                                   ! P
              call add2list(varname,tvar,ntrace1)
             varname='T:+1DP'                              ! T:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DP'                              ! T:-1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:+1DP'                              ! P:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='P:-1DP'                              ! P:-1DP
             call add2list(varname,tvar,ntrace1)

             ! DUDP
             varname='U:+1DP'                              ! U:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='U:-1DP'                              ! U:-1DP
             call add2list(varname,tvar,ntrace1)

             ! DVDP
             varname='V:+1DP'                              ! V:+1DP
             call add2list(varname,tvar,ntrace1)
             varname='V:-1DP'                              ! V:-1DP
             call add2list(varname,tvar,ntrace1)

             ! DTHDY
             varname='T:+1DLAT'                            ! T:+1DLON
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DLAT'                            ! T:-1DLON
             call add2list(varname,tvar,ntrace1)

             ! DTHDX
             varname='T:+1DLON'                            ! T:+1DLON
             call add2list(varname,tvar,ntrace1)
             varname='T:-1DLON'                            ! T:-1DLON
             call add2list(varname,tvar,ntrace1)



             !varname='ABSVORT'                              ! ABSVORT
             !call add2list(varname,tvar,ntrace1)
             !varname='DTHDP'                                ! DTHDP
             !call add2list(varname,tvar,ntrace1)
             !varname='DUDP'                                 ! DUDP
             !call add2list(varname,tvar,ntrace1)
             !varname='DVDP'                                 ! DVDP
             !call add2list(varname,tvar,ntrace1)
             !varname='DTHDX'                                ! DTHDX
             !call add2list(varname,tvar,ntrace1)
             !varname='DTHDY'                                ! DTHDY
             !call add2list(varname,tvar,ntrace1)

        ! Get the dependencies for Richardson number (RI)
        elseif ( tvar(i).eq.'RI' ) then
             varname='DUDP'                                 ! DUDP
             call add2list(varname,tvar,ntrace1)
             varname='DVDP'                                 ! DVDP
             call add2list(varname,tvar,ntrace1)
             varname='NSQ'                                  ! NSQ
             call add2list(varname,tvar,ntrace1)
             varname='RHO'                                  ! RHO
             call add2list(varname,tvar,ntrace1)

        end if
        
        ! exit points for handling additional field
1000    continue
        i = i + 1

    end do

    ! Save the full variable name
    do i = 1, ncol
        varsint(i)      = varsinp(i)
    end do

    do i = 1,ntrace1
        varsint(i+ncol) = tvar(i)
    end do

    ! Split the tracing variables
    do i = 1, ntrace0
        call splitvar(tvar(i),shift_val(i),shift_dir(i),shift_rel(i) )
    end do  

    ! Split the variable name and set flags
    do i = ntrace0 + 1, ntrace1

        ! Set the scaling factor
        fac(i) = 1.

        ! Set the base variable name, the shift and the direction
        call splitvar(tvar(i),shift_val(i),shift_dir(i),shift_rel(i) )

        ! Check if tvar used to appeared before and set the computational flag
        do j = 1,i-1
             if ( tvar(i) .eq. tvar(j) ) then 
                compfl(i) = 0
                go to 1001
             else if ( (tvar(i).eq.'P'   ) .or. &
                       (tvar(i).eq.'PLAY') .or. &
                       (tvar(i).eq.'PLEV') ) then
                compfl(i) = 0
                go to 1001
             else
                compfl(i) = 1
             end if
        end do
 
    open (300, file=tfile, status='old')
        read (300,*)
        read (300,*)
        read (300,*)
        read (300,*)

12      continue
        read (300,'(all)',end=13) tmpstring
            if ( tvar(i) .eq. trim(tmpstring(1:10)) ) then
               read(tmpstring,*) tvar(i),fac(i),compfl(i)
            end if
        go to 12
13      continue
    close (300)

1001    continue
         
    end do

    ! Check whether the shift modes are supported
    do i = 1, ntrace1
        if ( ( shift_dir(i) .ne. 'nil'          ) .and. &
             ( shift_dir(i) .ne. 'DLON'         ) .and. &
             ( shift_dir(i) .ne. 'DLAT'         ) .and. &
             ( shift_dir(i) .ne. 'DP'           ) .and. &
             ( shift_dir(i) .ne. 'HPA'          ) .and. &
             ( shift_dir(i) .ne. 'HPA(ABS)'     ) .and. &
             ( shift_dir(i) .ne. 'KM(LON)'      ) .and. &
             ( shift_dir(i) .ne. 'KM(LAT)'      ) .and. &
             ( shift_dir(i) .ne. 'H'            ) .and. &
             ( shift_dir(i) .ne. 'MIN'          ) .and. &
             ( shift_dir(i) .ne. 'INDP'         ) .and. &
             ( shift_dir(i) .ne. 'PMIN'         ) .and. &
             ( shift_dir(i) .ne. 'PMAX'         ) ) then

             print*, ' ERROR: shift mode ',trim(shift_dir(i)), ' not supported'
             stop
        end if
    end do

    ! Write status information 
    if (myrank.eq.0) then
    print*
    print*,' --------- COMPLETE TABLE FOR TRACING ------------- '
    print*
    do i = 1, ntrace1
       ! if ( ( shift_dir(i) .eq. 'nil' ) ) then
             write(*,'(i4,a4,a15,f10.2,a8,a8,3x,i5)') i,' : ',trim(tvar(i)),&
                shift_val(i),trim(shift_dir(i)),trim(shift_rel(i)),compfl(i)
       ! else
       !      write(*,'(i4,a4,a15,10x,8x,3x,i5)') i,' : ',compfl(i)
       ! end if
    end do
    end if

    ! -----------------------------------------------------------------------
    ! Prepare the internal and output trajectories
    ! -----------------------------------------------------------------------

    ! Allocate memory for internal trajectories
    allocate(traint(ntra,ntim,ncol+ntrace1),stat = stat)
    if ( stat.ne.0 ) print*, '*** error allocating array traint ***'

    ! Copy input to output trajectory
    do i = 1, ntra
        do j = 1, ntim
             do k = 1, ncol
                traint(i,j,k) = trainp(i,j,k)
             end do
        end do
    end do


    ! Set the flags for ready fields/columns - at begin only read-in fields are
    ! ready
    do i = 1, ncol
        fok(i) = 1
    end do
    do i = ncol + 1,ntrace1
        fok(i) = 0
    end do

    ! ----------------------------------------------------------------------
    ! Trace the fields (fields available on input files)
    ! ----------------------------------------------------------------------

    if (myrank.eq.0) then
    print*
    print*,'----- TRACING FROM FILES ---------'
    end if
  
    ! Loop over all tracing fields
    do i = 1, ntrace1
        
        ! Skip fields which must be computed (compfl=1), will be handle later
        if ( compfl(i) .eq. 1 ) go to 1100

        ! Write some status information
        if (myrank.eq.0) then
        print*
        print*,' Now tracing                  : ',trim(tvar(i)),shift_val(i),trim(shift_dir(i)),compfl(i),' '
        end if        

        ! Set the flag for ready field/column
        fok (ncol + i) = 1

        ! Reset flags for load manager
        iloaded0 = -1
        iloaded1 = -1

        ! Reset the counter for fields outside domain
        noutside = 0
        err_c1   = 0
        err_c2   = 0
        err_c3   = 0

        ! Loop over all times
        do j = 1, ntim

             ! Convert trajectory time from hh.mm to fractional time
             call hhmm2frac(trainp(1,j,1),tfrac)

             ! Shift time if requested
             if ( shift_dir(i) .eq. 'H' ) then
                tfrac = tfrac + shift_val(i)
             else if ( shift_dir(i) .eq. 'MIN' ) then
                tfrac = tfrac + shift_val(i)/60.
             end if

             ! Get the times which are needed
             itime0     = int(abs(tfrac-tstart)/timeinc) + 1
             time0      = tstart + fbflag * real(itime0-1) * timeinc
    !        print*,"tfrac:", tfrac,"tstart:", tstart,"itime0", itime0,"time0", time0 ! check check
             itime1     = itime0 + 1
             time1      = time0 + fbflag * timeinc
             if ( itime1 .gt. numdat ) then
                itime1  = itime0
                time1   = time0
             end if

             ! Load manager: Check whether itime0 can be copied from itime1
             if ( itime0 .eq. iloaded1 ) then
                f3t0    = f3t1
                p3t0    = p3t1
                spt0    = spt1
                iloaded0= itime0
             end if

             ! Load manager: check whether itime1 can be copied from itime0
             if ( itime1 .eq. iloaded0 ) then
                f3t1    = f3t0
                p3t1    = p3t0
                spt1    = spt0
                iloaded1= itime1
             end if

             ! Load manager: load first time (tracing variable and grid)
             if ( itime0 .ne. iloaded0 ) then
                
                filename = trim(dat(itime0))
                call frac2hhmm (time0,tload)
                varname  = tvar(i)
                if (myrank.eq.0) then
                   write(*,'(a23,a20,a3,a5,f7.2)') '        -> loading     :',trim(filename),    &
                         '   ', trim(varname),tload
                end if
                call input_open (fid,filename)
                mdv = -999.
                call input_wind (fid,varname,f3t0,tload,stagz,mdv,xmin,xmax,ymin,ymax,   &
                                 dx,dy,nx,ny,nz,timecheck)
                call input_grid (fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tload,     &
                                 pollon,pollat,p3t0,spt0,nz,ak,bk,stagz,timecheck)
                call input_close(fid)

                iloaded0 = itime0
             end if

             ! Load manager: load second time (tracing variable and grid)
             if ( itime1 .ne. iloaded1 ) then
                
                filename = trim(dat(itime1))
                call frac2hhmm (time1,tload)
                varname = tvar(i)
                if (myrank.eq.0) then
                   write(*,'(a23,a20,a3,a5,f7.2)') '        -> loading     :',trim(filename),    & 
                         '   ', trim(varname),tload
                end if
                call input_open (fid,filename)
                call input_wind (fid,varname,f3t1,tload,stagz,mdv,xmin,xmax,ymin,ymax,   &
                                 dx,dy,nx,ny,nz,timecheck)
                call input_grid (fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tload,      &
                                 pollon,pollat,p3t1,spt1,nz,ak,bk,stagz,timecheck)
                call input_close(fid)

                iloaded1 = itime1
             end if

             ! Loop over all trajectories

             call para_range(1,ntra,nprocs,myrank,ista,iend)
             iprev     = myrank-1
             inext     = myrank+1
             if (myrank.eq.0)        iprev = MPI_PROC_NULL
             if (myrank.eq.nprocs-1) inext = MPI_PROC_NULL
             print*, "my rank = ",myrank,ista,iend


             trajectory_loop: do k = ista,iend

                ! Set the horizontal position where to interpolate to
                x0      = traint(k,j,2)                 ! Longitude
                y0      = traint(k,j,3)                 ! Latitude

                ! Set the vertical position where to interpolate to
                if ( nz.gt.1 ) then
                     p0 = traint(k,j,4)                 ! Pressure (3D tracing)
                else 
                     p0 = 1050.                         ! Lowest level
                end if

                ! Set negative pressures to mdv
                if ( p0 .lt. 0. ) then
                     f0 = mdv
                     go to 1009
                end if

                !print*, "myrank, traj,x0, y0, p0 :", myrank,k, x0, y0, p0 ! check mpi

                ! Set the relative time
                call hhmm2frac (traint(k,j,1),tfrac)
                reltpos0 = fbflag * (tfrac-time0)/timeinc
                !print*, "reltpos check", k, j, traint(k,j,2), traint(k,j,3), traint(k,j,4), reltpos0, traint(k,j,1), tfrac, time0
             !/   print*, "reltpos check",j,  reltpos0,tfrac, time0

                ! Make adjustments depending on the shift flag
                if ( shift_dir(i) .eq. 'DLON' ) then            ! DLON
                     x0 = x0 + shift_val(i)
        
                else if ( shift_dir(i) .eq. 'DLAT' ) then       ! DLAT
                     y0 = y0 + shift_val(i)

                else if ( shift_dir(i) .eq. 'KM(LON)' ) then    ! KM(LON)
                     x0 = x0 + shift_val(i)/deg2km * 1./cos(y0*pi180)

                else if ( shift_dir(i) .eq. 'KM(LAT)' ) then    ! KM(LAT)
                     y0 = y0 + shift_val(i)/deg2km
        
                else if ( shift_dir(i) .eq. 'HPA' ) then        ! HPA
                     p0 = p0 + shift_val(i)
        
                else if ( shift_dir(i) .eq. 'HPA(ABS)' ) then   ! HPA(ABS)
                     p0 = shift_val(i)

                else if ( shift_dir(i) .eq. 'DP' ) then         ! DP
                     call get_index4 (xind,yind,pind,x0,y0,p0,reltpos0,   &
                        p3t0,p3t1,spt0,spt1,3,nx,ny,nz,xmin,ymin,dx,dy,mdv)
                     pind = pind - shift_val(i)
                     p0   = int_index4 (p3t0,p3t1,nx,ny,nz,xind,yind,pind, &
                        reltpos0,mdv)
                     
                else if ( shift_dir(i) .eq. 'INDP' ) then
                     p0   = int_index4 (p3t0,p3t1,nx,ny,nz,xind,yind,shift_val(i), &
                        reltpos0,mdv)
                end if

                ! Handle periodic boundaries in zonal direction
                if ( (x0.gt.xmax).and.(per.ne.0) ) x0 = x0 - 360.
                if ( (x0.lt.xmin).and.(per.ne.0) ) x0 = x0 + 360.

                ! Handle pole problems for hemispheric data (taken from
                ! caltra.f)
                if ( (hem.eq.1) .and. (y0.gt.90.) ) then

                   if (myrank.eq.0) then
                     print*,' WARNING: y0>90 ',y0,' => setting to 180 - y0',180.-y0
                   end if  
                     y0   = 180. - y0
                     x0   = x0 + per/2.
                end if

                if ( (hem.eq.1) .and. (y0.lt.-90.) ) then
                     print*,' WARNING: y0<-90 ',y0,' => setting to -180 - y0',-180.-y0
                     y0   = -180. -y0
                     x0   = x0 + per/2.
                end if

                ! Get the index where to interpolate (x0,y0,p0)
                if ( (abs(x0-mdv).gt.eps) .and. (abs(y0-mdv).gt.eps) ) then
                     call get_index4 (xind,yind,pind,x0,y0,p0,reltpos0, &
                            p3t0,p3t1,spt0,spt1,3,nx,ny,nz,xmin,ymin,dx,dy,mdv)

                     !print*, 'myrank,xind,yind,x0,y0,p0',myrank,xind,yind,x0,y0,p0! check check

                else
                     xind = mdv
                     yind = mdv
                     pind = mdv
                end if


                ! Check if point is within grid (keep indices if ok)
                if ( (xind.ge.1.).and.(xind.le.real(nx)).and.   &
                     (yind.ge.1.).and.(yind.le.real(ny)).and.   &
                     (pind.ge.1.).and.(pind.le.real(nz)) ) then
                        xind = xind
                        yind = yind
                        pind = pind

                ! Check if pressure is outside, but rest okay --> adjust to
                ! lowest or highest level
                else if ( (xind.ge.1.).and.(xind.le.real(nx)).and.   &
                          (yind.ge.1.).and.(yind.le.real(ny)) ) then      ! only vertical problem
 
                     if ( pind.gt.nz ) then     ! pressure too low, index too high
                        err_c1 = err_c1 + 1

                        if ( err_c1 .lt. 10 ) then

                           if (myrank.eq.0) then
                           write (*,'(a,f7.3,a)')' WARNING: pressure too low (pind = ',pind,    &
                                ') => adjusted to highest level (pind = nz)'
                           print*, '(x0,y0,p0) =',x0,y0,p0
                           end if
                           pind = real(nz)
                        else if (err_c1.eq.10) then

                           if (myrank.eq.0) then
                           print*,' WARNING: more pressures too low -> adjusted to highest level'
                           end if
                           pind = real(nz)

                        else
                           pind = real(nz)
                        end if

                     else if ( pind.lt.1. ) then   ! pressure too high, index too low
                        err_c2 = err_c2 + 1
        
                        if ( err_c2.lt.10 ) then
                           if (myrank.eq.0) then
                                write(*,'(a,f7.3,a)') ' WARNING: pressure too high (pind = ',pind,   &
                                          ') => adjusted to lowest level (pind = 1.)'
                                 print*, '(x0,y0,p0) =',x0,y0,p0
                           end if
                           pind = 1.
                        else if (err_c2.eq.10) then
                           if (myrank.eq.0) then
                                print*,' WARNING: more pressures too high -> adjusted to lowest level'
                           end if
                           pind = 1.

                        else
                           pind = 1.
                        end if

                     end if 

                ! Grid point is outside!
                else

                     err_c3 = err_c3 + 1
                
                     if ( err_c3 .lt. 10 ) then

                        if (myrank.eq.0) then
                           print*,'ERROR: point is outside grid (horizontally)'
                           print*,'    Trajectory #  ',k
                           print*,'    Position      ',x0,y0,p0
                           print*,'  (xind,yind):    ',xind,yind
                        endif
                        xind            = mdv
                        yind            = mdv
                        pind            = mdv 
                        traint(k,j,2)   = mdv
                        traint(k,j,3)   = mdv
                        traint(k,j,4)   = mdv
                     else
                        xind            = mdv
                        yind            = mdv
                        pind            = mdv
                        traint(k,j,2)   = mdv
                        traint(k,j,3)   = mdv
                        traint(k,j,4)   = mdv
                     end if

                end if                                       

                ! --------------------- NEAREST mode --------------------------
                ! Interpolate to nearest grid point
                if ( intmode .eq. 'nearest' ) then

                     xind = real( nint(xind) )
                     yind = real( nint(yind) )
                     pind = real( nint(pind) )

                     if ( xind.lt.1. ) xind = 1.
                     if ( xind.gt.nx ) xind = real(nx)
                     if ( yind.lt.1. ) yind = 1.
                     if ( yind.gt.ny ) yind = real(ny)
                     if ( pind.lt.1. ) pind = 1.
                     if ( pind.gt.nz ) pind = real(nz)

                     if ( abs(reltpos0) .ge. eps ) then
                        if (myrank.eq.0) then
                        print*,' ERROR (nearest): reltpos != 0',reltpos0
                        end if
                        stop

                     end if

                     ! interpolate
                     f0 = int_index4(f3t0,f3t1,nz,ny,nz,xind,yind,pind,reltpos0,mdv)
                     

                ! ----------------- end NEAREST mode --------------------------

                ! -------------------- CIRCLE modes ---------------------------
                else if ( (intmode .eq. 'circle_avg') .or. (intmode.eq.'circle_min') .or. (intmode.eq.'circle_max') ) then
                
                     ! reset arrays for this point
                     connect = 0
                     stackx  = 0
                     stacky  = 0
                     circlelon = 0
                     circlelat = 0
                     circlef   = 0
                     circlearea= 0

                     ! Get indices of one coarse grid point within search radius
                     ! (nint = round to next integer)
                     if ( sdis(x0,y0,longrid(nint(xind)),latgrid(nint(yind))) .gt. radius) then
   !                     print*,'x0,y0,longrid(nint(xind)),latgrid(nint(yind))',x0,y0,&
   !  &                          xind,yind,longrid(nint(xind)),latgrid(nint(yind))
                        if (myrank.eq.0) then
                        print*,'ERROR (circle): Search radius is too small... (1). r =',radius
                        print*,'Distance to nint grid point (minimum search radius) =', &
                                sdis(x0,y0,longrid(nint(xind)),latgrid(nint(yind)))
                        end if
                        stop

                     end if

                     ! Initialize stack with nint(xind),nint(yind)
                     kst = 0            ! counts the number of points in circle
                     stackx(1) = nint(xind)
                     stacky(1) = nint(yind)
                     sp  = 1            ! stack counter
        
                     do while (sp .ne. 0)

                        ! Get an element from stack
                        ist = stackx(sp)
                        jst = stacky(sp)
                        sp  = sp - 1

                        ! Get distance from reference point
                        dist = sdis(x0,y0,longrid(ist),latgrid(jst))

                        ! Check whether distance is smaller than search radius :
                        ! connected
                        if ( dist .le. radius ) then

                           ! Increase the total stack index
                           kst = kst + 1
                           circlelon(kst) = longrid(ist)
                           circlelat(kst) = latgrid(jst)
                        
                           ! Interpolate field to position of point
                           ! (interpolation in time!)
                           circlef(kst) = int_index4(f3t0,f3t1,nx,ny,nz,real(ist),real(jst),pind,reltpos0,mdv)
                           
                           ! Calculate area of point (for circle_avg mode only)
                           if ( intmode .eq. 'circle_avg' ) then
                                circlearea(kst) = pir/(nx-1) * ( sin(pi180 * abs(circlelat(kst)))       &
                                                  - sin(pi180 * (abs(circlelat(kst)) - dy)) )
                           end if
                        
                           ! Mark this point as visited
                           connect(ist,jst) = 1

                           ! Get coordinates of neighbouring points and
                           ! implement periodicity 
                           mr = ist + 1
                           if ( mr .gt. nx ) mr = 1
                           ml = ist - 1
                           if ( mr .lt.  1 ) ml = nx
                           nu = jst + 1
                           if ( nu .gt. ny ) nu = ny
                           nd = jst - 1
                           if ( nd .lt.  1 ) nd = 1
                           ! Update stack with neighbouring points
                           if ( connect(mr,jst) .ne. 1 ) then
                                connect(mr,jst) = 1
                                sp = sp + 1
                                stackx(sp) = mr
                                stacky(sp) = jst
                           end if

                           if ( connect(ml,jst) .ne. 1 ) then
                                connect(ml,jst) = 1
                                sp = sp + 1
                                stackx(sp) = ml
                                stacky(sp) = jst
                           end if

                           if ( connect(ist,nd) .ne. 1 ) then
                                connect(ist,nd) = 1
                                sp = sp + 1
                                stackx(sp) = ist
                                stacky(sp) = nd
                           end if

                           if ( connect(ist,nu) .ne. 1 ) then
                                connect(ist,nu) = 1
                                sp = sp + 1
                                stackx(sp) = ist
                                stacky(sp) = nu
                           end if
                       
                        end if  ! end if radius is smaller -> end of updating stack

                     end do     ! end working on stack

                     if ( kst.ge.1) then
                        ! Choose output depending on intmode
                        if ( intmode .eq. 'circle_avg' ) then
                           
                           ! Calculate area-weighted average of fi in circle
                           circlesum = 0.
                           do l = 1, kst
                                circlesum = circlesum + circlef(l) * circlearea(l)
                           end do

                           circleavg = circlesum/sum(circlearea(1:kst))
                           f0        = circleavg

                        else if ( intmode .eq. 'circle_min' ) then
                           ! calculate minimum in circle
                           circlemin = circlef(1)
                           do l = 1, kst
                              if ( circlef(l) .lt. circlemin ) then
                                circlemin = circlef(l)
                              end if
                           end do

                           f0       = circlemin

                        else if ( intmode .eq. 'circle_max' ) then
                           ! calculate maximum in circle
                           circlemax = circlef(1)
                           do l = 1, kst
                              if ( circlef(l) .gt. circlemax ) then
                                circlemax = circlef(l)
                              end if
                           end do
                        
                        else
                           if (myrank.eq.0) then
                           print*,' ERROR (circle): intmode not valid!'
                           end if
                           stop
                        end if
                
                   else
                        if (myrank.eq.0) then
                        print*,' ERROR (circle): Search radius is too small... (2). r =',radius
                        end if
                        stop 

                   end if

                ! ---------------------- end CIRCLE modes ----------------------

                ! ---------------------- NORMAL mode ----------------------
                else ! not clustering nor circle: NORMAL mode
                    
                   if ( (shift_dir(i).ne.'PMIN') .and. (shift_dir(i).ne.'PMAX') ) then
                        f0 = int_index4(f3t0,f3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                
                   else if ( shift_dir(i).eq.'PMIN' ) then
                        f0 = int_index4(f3t0,f3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                        
                        ! Handle > and < operators
                        if ( (shift_rel(i).eq.'>').and.(f0.gt.shift_val(i)) ) then
                            do while ( (f0.gt.shift_val(i))  .and. (pind.lt.nz))
                                pind = pind + 0.1    
                                f0 = int_index4(f3t0,f3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                            end do
                            f0 = int_index4(p3t0,p3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)

                        else if ( (shift_rel(i).eq.'<').and.(f0.lt.shift_val(i)) ) then
                            do while ( (f0.lt.shift_val(i)) .and. (pind.lt.nz) )
                                pind = pind + 0.1
                                f0 = int_index4(f3t0,f3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                            end do
                            f0 = int_index4(p3t0,p3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                        
                        else
                            f0 = mdv
                        end if

                   else if ( shift_dir(i).eq.'PMAX' ) then
                        f0 = int_index4(f3t0,f3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                        
                        ! Handle > and < operators
                        if ( (shift_rel(i).eq.'>') .and. (f0.gt.shift_val(i)) ) then
                            do while ( (f0.gt.shift_val(i)).and.(pind.gt.1) )
                                pind = pind - 0.1
                                f0 = int_index4(f3t0,f3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                            end do
                            f0 = int_index4(f3t0,f3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                        
                        else if ( (shift_rel(i).eq.'<').and.(f0.lt.shift_val(i)) ) then
                            do while ( (f0.lt.shift_val(i)).and.(pind.gt.1) )
                                pind = pind - 0.1
                                f0 = int_index4(f3t0,f3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                            end do
                            f0 = int_index4(p3t0,p3t1,nx,ny,nz,xind,yind,pind,reltpos0,mdv)
                        
                        else
                            f0 = mdv
                        end if

                   end if

            ! ---------------------- end of NORMAL mode -----------------------                        
            end if ! end of choosing mode

            ! exit for loop over all times - save interpolated value
1009        continue
            ! save the new field
            if ( abs(f0 - mdv).gt.eps ) then
                traint(k,j,ncol+i) = f0
            else     
                traint(k,j,ncol+i) = mdv
            end if

        end do trajectory_loop ! end loop over all trajectories

        ! initiate rank communication for MPI
        allocate(itype(0:nprocs-1))
        allocate(ureq(0:nprocs-1))

        ! get itype(irank)
        do irank       = 0,nprocs - 1
            call para_range(1,ntra,nprocs,irank,ista,iend)
            call para_type_block2(1,ntra,1,ista,iend,1,1,MPI_REAL,itype(irank))
        enddo

        if (myrank .eq. 0) then
             ! gather the updates from all cores (non-contiguous memory)
             do irank = 1, nprocs - 1
                 call para_range(1,ntra,nprocs,irank,jsta,jend)
                 num_rows = jend-jsta+1
                 call MPI_IRECV(traint(:,j,ncol+i),1,itype(irank),irank,1,MPI_COMM_WORLD,ureq(irank),ierr)

                 !call MPI_IRECV(traint(jsta:jend,j,ncol+i),num_rows,itype(irank),irank,1,MPI_COMM_WORLD,ureq(irank),ierr)
             end do

             do irank = 1, nprocs - 1
                 call MPI_WAIT(ureq(irank),istatus,ierr)
             end do
        else
             num_rows = iend - ista + 1
             call MPI_ISEND(traint(:,j,ncol+i),1,itype(myrank),0,1,MPI_COMM_WORLD,ureq1,ierr)

             !call MPI_ISEND(traint(ista:iend,j,ncol+i),num_rows,itype(myrank),0,1,MPI_COMM_WORLD,ureq1,ierr)
             call MPI_WAIT(ureq1,istatus,ierr)
        end if

       
        deallocate(itype)
        deallocate(ureq) 

      end do ! end loop over all times

      ! Exit point for loop over all tracing variables
1100  continue
         
    end do ! end loop over all variables

    ! ----------------------------------------------------------------------
    ! Calculate additional fields along the trajectories
    ! ----------------------------------------------------------------------

    if (myrank .eq. 0) then
    print*
    print*,'----- CALCULATE ADDITIONAL FIELDS FROM TRAJECTORY TABLE ------'
    end if

    ! Loop over all tracing fields

    do i = ntrace1, 1, -1

        ! Skip fields which must not be computed (compfl = 0)
        if ( compfl(i) .eq. 0 ) go to 1200
       
        if (myrank .eq. 0) then 
        ! Write some status information
        print*
        write(*,'(a10,f10.2,a5,i3)') trim(tvar(i)),shift_val(i),trim(shift_dir(i)),compfl(i)
        end if        

        ! Loop over trajectories and times
        do k = 1, ntim

        call para_range(1,ntra,nprocs,myrank,ista,iend)
        iprev     = myrank-1
        inext     = myrank+1
        if (myrank.eq.0)        iprev = MPI_PROC_NULL
        if (myrank.eq.nprocs-1) inext = MPI_PROC_NULL
        print*, "my rank = ",myrank,ista,iend


        trajectory_loop2: do j = ista,iend
        
             ! Potenttial tempreature (TH)
             if ( varsint(i+ncol) .eq. 'TH' ) then

                varname = 'T'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname = 'P'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                
                call calc_TH ( traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2) )

             ! Density (RHO)
             elseif  ( varsint(i+ncol).eq.'RHO' ) then

                varname='T'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='P'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)

                call calc_RHO (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2) )

            ! Relative humidity (RH)
            elseif  ( varsint(i+ncol).eq.'RH' ) then

                varname='T'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='p'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='Q'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)

                call calc_RH (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3) )

            ! Equivalent potential temperature (THE)
            elseif  ( varsint(i+ncol).eq.'THE' ) then

                varname='T'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='p'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='Q'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)

                call calc_THE (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3) )

            ! Latent heating rate (LHR)
            elseif  ( varsint(i+ncol).eq.'LHR' ) then

                varname='T'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='p'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='Q'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='OMEGA'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)


                call calc_RH (tmp1, traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3) )

                !varname='RH'
                !call list2ind (ind5,varname,varsint,fok,ncol+ntrace1)

                call calc_LHR (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3)  &
                                ,traint(j,k,ind4),tmp1)

            ! Wind speed (VEL)
            elseif  ( varsint(i+ncol).eq.'VEL' ) then

                varname='U'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='V'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)

                call calc_VEL (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2) )

            ! Wind direction (DIR)
            elseif  ( varsint(i+ncol).eq.'DIR' ) then

                varname='U'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='V'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)

                call calc_DIR (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2) )

            ! Zonal gradient of U (DUDX)
            elseif  ( varsint(i+ncol).eq.'DUDX' ) then
                varname='U:+1DLON'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                print*,traint(j,k,ind1)
                varname='U:-1DLON'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)

                call calc_DFDX (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3) )

            ! Zonal gradient of V (DVDX)
            elseif  ( varsint(i+ncol).eq.'DVDX' ) then

                varname='V:+1DLON'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='V:-1DLON'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)

                call calc_DFDX (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3) )

            ! Zonal gradient of T (DTDX)
            elseif  ( varsint(i+ncol).eq.'DVDX' ) then

                varname='T:+1DLON'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='T:-1DLON'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)

                call calc_DFDX (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3) )

            ! Zonal gradient of TH (DTHDX)
            elseif  ( varsint(i+ncol).eq.'DTHDX' ) then

                varname='T:+1DLON'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='T:-1DLON'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='P'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)

                call calc_DTHDX (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),traint(j,k,ind4) )

            ! Meridional gradient of U (DUDY)
            elseif  ( varsint(i+ncol).eq.'DUDY' ) then

                varname='U:+1DLAT'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='U:-1DLAT'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)

                call calc_DFDY (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2) )

            ! Meridional gradient of V (DVDY)
            elseif  ( varsint(i+ncol).eq.'DVDY' ) then

                varname='V:+1DLAT'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='V:-1DLAT'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)

                call calc_DFDY (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2) )

            ! Eigenvalues 
            elseif ( (varsint(i+ncol).eq.'R_EIGVAL1') .or. (varsint(i+ncol).eq.'R_EIGVAL2') .or.  &
                     (varsint(i+ncol).eq.'I_EIGVAL1') .or. (varsint(i+ncol).eq.'I_EIGVAL2') ) then
        
                varname='U:+1DLON'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='U:-1DLON'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                call calc_DFDY (tmp1, traint(j,k,ind1), traint(j,k,ind2) )

                varname='V:+1DLON'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='V:-1DLON'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                call calc_DFDY (tmp2, traint(j,k,ind1), traint(j,k,ind2) )

                varname='U:+1DLAT'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='U:-1DLAT'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                call calc_DFDY (tmp3, traint(j,k,ind1), traint(j,k,ind2) )

                varname='V:+1DLAT'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='V:-1DLAT'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                call calc_DFDY (tmp4, traint(j,k,ind1), traint(j,k,ind2) )

                call calc_EIG (eig_re,eig_im, tmp1,tmp2,tmp3,tmp4)
                
                if ( varsint(i+ncol).eq.'R_EIGVAL1' ) then
                   traint(j,k,ncol+i) = eig_re(1)
                elseif ( varsint(i+ncol).eq.'R_EIGVAL2' ) then
                   traint(j,k,ncol+i) = eig_re(2)
                elseif ( varsint(i+ncol).eq.'I_EIGVAL1' ) then
                   traint(j,k,ncol+i) = eig_im(1)
                elseif ( varsint(i+ncol).eq.'I_EIGVAL2' ) then
                   traint(j,k,ncol+i) = eig_im(2)
                endif
    

            ! Meridional gradient of T (DTDY)
            elseif  ( varsint(i+ncol).eq.'DTDY' ) then

                varname='T:+1DLAT'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='T:-1DLAT'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)

                call calc_DFDY (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2) )

            ! Meridional gradient of TH (DTHDY)
            elseif  ( varsint(i+ncol).eq.'DTHDY' ) then

                varname='T:+1DLAT'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='T:-1DLAT'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='P'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)

                call calc_DTHDY (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3) )


            ! Vertical wind shear DU/DP (DUDP)
            elseif  ( varsint(i+ncol).eq.'DUDP' ) then

                varname='U:+1DP'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='U:-1DP'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='P:+1DP'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='P:-1DP'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)

                call calc_DFDP (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),traint(j,k,ind4) )

            ! Vertical wind shear DV/DP (DVDP)
            elseif  ( varsint(i+ncol).eq.'DVDP' ) then

                varname='V:+1DP'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='V:-1DP'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)

                call calc_DFDP (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),traint(j,k,ind4) )

            ! Vertical derivative of T (DTDP)
            elseif  ( varsint(i+ncol).eq.'DTDP' ) then

                varname='T:+1DP'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='T:-1DP'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='P:+1DP'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='P:-1DP'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)

                call calc_DFDP (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),traint(j,k,ind4) )

            ! Vertical derivative of TH (DTHDP)
            elseif  ( varsint(i+ncol).eq.'DTHDP' ) then

                varname='T:+1DP'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='T:-1DP'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='P:+1DP'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='P:-1DP'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)
                varname='P'
                call list2ind (ind5,varname,varsint,fok,ncol+ntrace1)
                varname='T'
                call list2ind (ind6,varname,varsint,fok,ncol+ntrace1)

                call calc_DTHDP (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),       &
                                traint(j,k,ind4),traint(j,k,ind5),traint(j,k,ind6))

            ! Squared Brunt-Vaisäla frequency (NSQ)
            elseif  ( varsint(i+ncol).eq.'NSQ' ) then

                varname='T:+1DP'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='T:-1DP'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='P:+1DP'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='P:-1DP'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)
                varname='P'
                call list2ind (ind5,varname,varsint,fok,ncol+ntrace1)
                varname='T'
                call list2ind (ind6,varname,varsint,fok,ncol+ntrace1)

                call calc_DTHDP (tmp1, traint(j,k,ind1),traint(j,k,ind2),traint(j,k,ind3),       &
                                traint(j,k,ind4),traint(j,k,ind5),traint(j,k,ind6))

                call calc_TH (tmp2, traint(j,k,ind6), traint(j,k,ind5) )

                call calc_RHO (tmp3, traint(j,k,ind6),traint(j,k,ind5) )

                !varname='DTHDP'
                !call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                !varname='TH'
                !call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                !varname='RHO'
                !call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)

                call calc_NSQ (traint(j,k,ncol+i), tmp1, tmp2, tmp3)

            ! Relative vorticity (RELVORT)
            elseif  ( varsint(i+ncol).eq.'RELVORT' ) then

                varname='DUDY'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='DVDX'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='U'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)

                call calc_RELVORT (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),traint(j,k,ind4))

            ! Absolute vorticity (ABSVORT)
            elseif  ( varsint(i+ncol).eq.'ABSVORT' ) then

                varname='DUDY'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='DVDX'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='U'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)

                call calc_ABSVORT (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),traint(j,k,ind4))

            ! Divergence (DIV)
            elseif  ( varsint(i+ncol).eq.'DIV' ) then

                varname='DUDX'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='DVDY'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='V'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)

                call calc_DIV (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),traint(j,k,ind4))

            ! Deformation (DEF)
            elseif  ( varsint(i+ncol).eq.'DEF' ) then

                varname='DUDX'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='DVDX'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='DUDY'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='DVDY'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)

                call calc_DEF (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),traint(j,k,ind4))

            ! Potential Vorticity (PV)
            elseif  ( varsint(i+ncol).eq.'PV' ) then

                varname='ABSVORT'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='DTHDP'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='DUDP'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='DVDP'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)
                varname='DTHDX'
                call list2ind (ind5,varname,varsint,fok,ncol+ntrace1)
                varname='DTHDY'
                call list2ind (ind6,varname,varsint,fok,ncol+ntrace1)

                call calc_PV (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),          &
                              traint(j,k,ind4),traint(j,k,ind5),traint(j,k,ind6))

            ! Richardson number (RI)
            elseif  ( varsint(i+ncol).eq.'RI' ) then

                varname='DUDP'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='DVDP'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)
                varname='NSQ'
                call list2ind (ind3,varname,varsint,fok,ncol+ntrace1)
                varname='RHO'
                call list2ind (ind4,varname,varsint,fok,ncol+ntrace1)

                call calc_RI (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,k,ind3),traint(j,k,ind4) )

            ! Spherical distance from starting position (DIST0)
            elseif  ( varsint(i+ncol).eq.'DIST0' ) then

                varname='lon'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)

                call calc_DIST0 (traint(j,k,ncol+i), traint(j,k,ind1), traint(j,k,ind2),traint(j,1,ind1),traint(j,1,ind2) )

            ! Spherical distance length of trajectory (DIST)
            elseif  ( varsint(i+ncol).eq.'DIST' ) then

                varname='lon'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)

                if ( k.eq.1 ) then
                   traint(j,k,ncol+i) = 0.
                else
                call calc_DIST0 (delta, traint(j,k  ,ind1), traint(j,k,ind2),traint(j,k-1,ind1),traint(j,k-1,ind2) )
                                traint(j,k,ncol+i) = traint(j,k-1,ncol+i) + delta
                endif

            ! Heading of the trajectory (HEAD)
            elseif  ( varsint(i+ncol).eq.'HEAD' ) then

                varname='lon'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='lat'

                if (k.eq.ntim) then
                    traint(j,k,ncol+i) = mdv
                else
                    call calc_HEAD (traint(j,k,ncol+i),traint(j,k,ind1),traint(j,k,ind2),traint(j,k+1,ind1),traint(j,k+1,ind2) )
                endif

            ! Directional change (DANGLE)
            elseif  ( varsint(i+ncol).eq.'DANGLE' ) then

                varname='lon'
                call list2ind (ind1,varname,varsint,fok,ncol+ntrace1)
                varname='lat'
                call list2ind (ind2,varname,varsint,fok,ncol+ntrace1)

                if (k.eq.ntim) then
                    traint(j,k,ncol+i) = mdv
                else
                   if ( k.eq.1 ) then
                        traint(j,k,ncol+i) = mdv
                   elseif ( k.eq.ntim ) then
                        traint(j,k,ncol+i) = mdv
                   else
                        call calc_DANGLE (traint(j,k,ncol+i),      &
                             traint(j,k-1,ind1),traint(j,k-1,ind2), &
                             traint(j,k  ,ind1),traint(j,k  ,ind2), &
                             traint(j,k+1,ind1),traint(j,k+1,ind2) )
                   endif
                endif

            !elseif ( varsint(i+ncol).eq.'

            ! Invalid tracing variable
            else

                if (myrank .eq. 0) then
                print*,' ERROR: invalid tracing variable ', trim(varsint(i+ncol))
                end if
                stop


            end if

        ! End loop over all trajectories and times
        end do trajectory_loop2
        
                ! initiate rank communication for MPI
        allocate(itype(0:nprocs-1))
        allocate(ureq(0:nprocs-1))

        ! get itype(irank)
        do irank       = 0,nprocs - 1
            call para_range(1,ntra,nprocs,irank,ista,iend)
            call para_type_block2(1,ntra,1,ista,iend,1,1,MPI_REAL,itype(irank))
        enddo

        if (myrank .eq. 0) then
             ! gather the updates from all cores (non-contiguous memory)
             do irank = 1, nprocs - 1
                 !call para_range(1,ntra,nprocs,irank,jsta,jend)
                 !num_rows = jend-jsta + 1
                 call MPI_IRECV(traint(:,k,ncol+i),1,itype(irank),irank,1,MPI_COMM_WORLD,ureq(irank),ierr)

                 !call MPI_IRECV(traint(jsta:jend,k,ncol+i),num_rows,itype(irank),irank,1,MPI_COMM_WORLD,ureq(irank),ierr)
             end do

             do irank = 1, nprocs - 1
                 call MPI_WAIT(ureq(irank),istatus,ierr)
             end do
        else
             !num_rows = iend-ista + 1
             !call MPI_ISEND(traint(ista:iend,k,ncol+i),num_rows,itype(myrank),0,1,MPI_COMM_WORLD,ureq1,ierr)
             call MPI_ISEND(traint(:,k,ncol+i),1,itype(myrank),0,1,MPI_COMM_WORLD,ureq1,ierr)

             call MPI_WAIT(ureq1,istatus,ierr)
        end if


        deallocate(itype)
        deallocate(ureq)

        end do

        ! Set the flag for a ready field/column
        fok (ncol + i) = 1

        ! Exit point for loop over all tracing fields
1200    continue

   end do

   ! --------------------------------------------------------------------------
   ! Write output to output trajectory file
   ! --------------------------------------------------------------------------

   ! only for RANK 0
   if (myrank.eq.0) then
   ! Write status information
   print*
   print*,'-------------- WRITE OUTPUT TRAJECTORIES -------------------'
   print*

   ! Allocate memory for internal trajectories
   allocate(traout(ntra,ntim,ncol+ntrace0),stat = stat)
   if ( stat.ne.0 ) print*,'*** error allocating array traout ***'
   
   ! Copy input to output trajectory (apply scaling of output)
   do i = 1, ntra
        do j = 1, ntim
             do k = 1, ncol + ntrace0
                if ( k .le. ncol ) then
                   traout(i,j,k) = traint(i,j,k)
                else if ( abs(traint(i,j,k) - mdv).gt.eps ) then   
                   traout(i,j,k) = fac(k-ncol) * traint(i,j,k)
                else
                   traout(i,j,k) = mdv
                end if
             end do
        end do
   end do
 
   ! Set the variable names for output trajectory
   do i = 1, ncol + ntrace0
        varsout(i)      = varsint(i)
   end do
   do i = 1, ntrace0
        if ( (shift_dir(i).eq.'PMIN').or.(shift_dir(i).eq.'PMAX') ) then
             varsout(ncol+i) = trim(tvar(i))//':'//trim(shift_dir(i))
        end if
   end do

   ! Write trajectories
   call wopen_tra(fod,outfile,ntra,ntim,ncol+ntrace0,reftime,varsout,outmode)
   call write_tra(fod,traout ,ntra,ntim,ncol+ntrace0,outmode)
   call close_tra(fod,outmode)

   ! Write some status information, and end of program message

   print*
   print*,' ---------- STATUS INFORMATION ------------- '
   print*
   print*,' Everything is okay! '
   print*
   print*,'       *** END OF PROGRAM TRACE ***      '
   print*,'============================================ '
   end if

1005 continue
CALL MPI_FINALIZE(ierr)

stop
end program trace

