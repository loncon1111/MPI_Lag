program create_startf

!    *****************************************************************
!    * Create a startfile. The starting points are equidistantly     *
!    * distributed                                                   *
!    *****************************************************************

     use netcdf
     use ncinput
     use interp
     use sub_startf
     use caltime
     use iotra

     implicit none

     ! ---------------------------------------------------------------
     ! Set parameters
     ! ---------------------------------------------------------------

     ! Maximum number of starting positions
     integer,parameter :: nmax=100000000

     ! Maximum number of model levels
     integer,parameter :: nlevmax=200

     ! Grid constant (distance in km corresponding to 1 deg at the equator)
     real,parameter    :: deltat=111.

     ! Mathematical constant
     real,parameter    :: pi180=4*atan(1.0)/180.

     ! Numerical epsilon
     real,parameter    :: eps=1.e-05

     ! ---------------------------------------------------------------
     ! Set variables
     ! ---------------------------------------------------------------

     ! Filenames and output format
     character*80      :: file0,file1   ! filenames
     character*80      :: ofile         ! output filename
     integer           :: oformat       ! output format
     real              :: timeshift     ! timeshift relative to datafiles
     real              :: timeinc       ! time increment between input files

     ! Horizontal grid
     real              :: lat1,lat2,lon1,lon2 ! Lat/lon boundaries
     real              :: ds,dlat,dlon        ! distance and lat/lon shifts
     character*80      :: hfile               ! Filename
     character*80      :: hmode               ! Horizontal mode
     integer           :: hn                  ! Number of entries in lat/lon list
     real              :: latlist(nmax)       ! List of latitudes
     real              :: lonlist(nmax)       ! List of longitudes
     integer           :: pn                  ! Number of entries in lat/lon poly
     real              :: latpoly(500)        ! List of polygon latitudes
     real              :: lonpoly(500)        ! List of polygon longitudes
     real              :: loninpoly,latinpoly   ! Lon/lat inside polygon
     character*80      :: regionf               ! region file
     integer           :: iregion               ! region number
     real              :: xcorner(4),ycorner(4) ! Vertices of region

     ! Vertical grid
     character*80      :: vmode               ! Vertical mode
     real              :: lev1,lev2,levlist(nmax)! Single leves, and list of levels
     character*80      :: vfile               ! Filename
     integer           :: vn                  ! Number of entries

     ! Unit of vertical axis
     character*80      :: umode               ! Unit of vertical axis

     ! Flag for 'no time check'
     logical           :: timecheck           ! Either 'true' or 'false'

     ! List of all starting poins
     integer           :: start_n             ! Number of coordinates
     real              :: start_lat(nmax)     ! Latitudes
     real              :: start_lon(nmax)     ! Longitudes
     real              :: start_lev(nmax)     ! Levels (depending on vertical units)
     real              :: start_pre(nmax)     ! Level in hPa
     integer           :: start_valid(nmax)   ! Flag whether starting height is valid
     integer           :: reftime(6)          ! Reference time
     integer           :: year,month,day,hour,minute,time_range
     character*80      :: vars(10)            ! Name of output fields (time,lon,lat,p)
     real,dimension(:,:,:),allocatable:: tra  ! Trajectories (ntra,ntim,ncol)
     real              :: latmin,latmax      
     real              :: lonmin,lonmax
     real              :: premin,premax

     ! Grid description
     real              :: pollon,pollat       ! Longitude/Latitude of pole
     real              :: ak(nlevmax)         ! Vertical layers and levels
     real              :: bk(nlevmax)         ! 
     real              :: xmin,xmax           ! Zonal grid extension
     real              :: ymin,ymax           ! Meridional grid extension
     integer           :: nx,ny,nz            ! Grid dimensions
     real              :: dx,dy               ! Horizontal grid resolution
     real,allocatable,dimension(:,:,:) :: pr  ! 3D pressure
     real,allocatable,dimension(:,:)   :: prs ! surface pressure
     real,allocatable,dimension(:,:,:) :: in  ! 3D 'dummy) array with vertical indices
     character*80      :: varname             ! Name of input variables
     integer           :: fid                 ! File identifier
     real              :: stagz               ! Vertical staggering
     real              :: mdv                 ! Missing data values
     real              :: tstart,tend         ! Time on file
     real              :: rid,rjd,rkd         ! Real grid position

     ! Auxiliary variables
     integer           :: i,j,k
     real              :: lon,lat
     real              :: rd
     real,allocatable,dimension(:) :: r1d
     integer           :: stat, flag
     real              :: tmp1,tmp2
     real              :: tfrac,frac
     real              :: radius,dist
     character*80      :: string
     character*80      :: selectstr
     character*80      :: umode_save
     character*80      :: maskname
     character*80      :: maskfile
     real              :: maskvalue
     character*2       :: maskoper
     integer           :: cdfid,varid,ierr
     integer           :: nmask
     integer           :: indx,indy
     character         :: ch1,ch2
     integer           :: len
     real,allocatable,dimension(:,:,:) :: fld0
     real,allocatable,dimension(:,:,:) :: fld1
     real,allocatable,dimension(:,:)   :: sfc0
     real,allocatable,dimension(:,:)   :: sfc1
     real,allocatable,dimension(:,:)   :: mask
     integer           :: n_outside

     ! namelist.startf
     namelist /share/ year,month,day,hour,minute,time_range
     namelist /input/ file0,file1,regionf,timeshift,timeinc
     namelist /output/ ofile
     namelist /params/ hmode,hfile,lon1,lon2,lat1,lat2,ds,dlon, &
        &              dlat,radius,iregion,maskname,vmode,vfile,&
        &              lev1,lev2,vn,levlist,umode,selectstr,timecheck

     ! Externals
     

!    -------------------------------------------------------------------------
!    Start of program. Read parameters
!    -------------------------------------------------------------------------

     ! Write start message

     print*, "============================================================"
     print*, "       *** START OF PROGRAM CREATE_STARTF ***               "
     print*

     ! Read namelist.startf 
     open (100,file="namelist.startf",status='old')
        read(100,share)
        print*,'check 1'
        read(100,input)
        print*, 'check 2'
        read(100,output)
        print*, 'check 3'
        read(100,params)
        print*, ' check 4'
     close (100)

     reftime(1) = year
     reftime(2) = month
     reftime(3) = day
     reftime(4) = hour
     reftime(5) = minute
     reftime(6) = time_range

     ! Check the availability of horizontal mode
     select case (trim(hmode))
     case ("file","line","box.eqd","box.grid","point","shift","polygon.eqd", &
        &  "polygon.grid","circle.eqd","circle.grid","region.eqd","region.grid",&
        &  "mask.grid","mask.eqd")
        print*
     case default
        print*," ERROR: hozizontal mode not supported ",trim(hmode)
        stop
     end select

     ! Check the availability of vertical mode
     select case (trim(vmode))
     case ("file","level","list","profile","grid")
        print*
     case default
        print*," ERROR: vertical mode not supported ",trim(vmode)
        stop
     end select

     ! Check units of vertical axis
     select case (trim(umode))
     case ("hPa","hPa.agl","INDEX")
        print*
     case default
        print*," ERROR: unit not supported ",trim(umode)
     end select

     ! Decide which output format is used (1..4: trajectory format, -1: triple
     ! list)
     call mode_tra(oformat,ofile)

     ! Decide whether all lat/lon/lev coordinates are read from one file
     if ( (hmode.eq.'file').and.(vmode.eq.'nil') ) then
        hmode = 'file3'
     else if ( (hmode.eq.'file').and.(vmode.ne.'nil') ) then
        hmode = 'file2'
     end if

     ! Convert timeshift from (hh.mm) into a fractional time shift
     call hhmm2frac(timeshift,tfrac)
     if (tfrac.gt.0) then
        tfrac = tfrac/timeinc
     else
        tfrac = 0.
     end if

     ! If a mask file is provided, no time interpolation is allowed
     if ( (hmode.eq.'mask.grid').or.(hmode.eq.'mask.eqd') ) then
        if ( abs(tfrac).gt.eps ) then
           print*, " ERROR: no intermediate times allowed for ", trim(hmode)
           stop
        end if
     end if

     ! Read the region coordinates if needed
     if ( (hmode.eq.'region.eqd').or.(hmode.eq.'region.grid') ) then
        open (200, file=regionf, status='old')

50        read (200,*,end=51) string
          
          if ( string(1:1).ne.'#' ) then
             call regionsplit(string,i,xcorner,ycorner)
             if (i.eq.iregion) goto 52
          end if

          go to 50

51      close (200)
        print*,' ERROR: region ',iregion,' not found on ',trim(regionf)
        stop

52      continue

     end if

     ! Write some status information
     print*,'---------------- INPUT PARAMETERS -------------------'
     print*

     if ( timeshift.gt.0. ) then
        print*, ' - Input file                  :',trim(file0),'   ',trim(file1)
     else
        print*, ' - Input file                  :',trim(file0)
     end if

     print*, ' - Output file                  :',trim(ofile)
     print*

     if ( oformat.eq.-1 ) then
        print*,' - Output format                  : (lon,lat,lev)-list'
     else
        print*,' - Output format                  :',oformat
     end if

     print*
     print*,' - Reference time (year)           : ',reftime(1)
     print*,'                  (month)          : ',reftime(2)
     print*,'                  (day)            : ',reftime(3)
     print*,'                  (hour)           : ',reftime(4)
     print*,'                  (minute)         : ',reftime(5)
     print*,' - Time range                      : ',reftime(6)
     print*
     print*,' - Time shift                      : ',timeshift,' + ',trim(file0)
     print*,' - Region file                     : ',regionf
     print*
     print*,' - Horizontal mode                 : ',trim(hmode)
     if ( hmode .eq.'file2' ) then
        print*,'        file name [lat/lon]       : ',trim(hfile)
     else if ( hmode .eq. 'file3' ) then
        print*,'        file name [lat/lon/lev]   : ',trim(hfile)
     else if ( hmode .eq. 'line'  ) then
        write(*,'(a30,4f10.2,i4)') ' lon1,lon2,lat1,lat2,n      : ', &
        &               lon1,lon2,lat1,lat2,hn
     else if ( hmode .eq. 'box.eqd' ) then
        write(*,'(a30,5f10.2)') ' lon1,lon2,lat1,lat2,ds        : ', &
        &               lon1,lon2,lat1,lat2,ds
     else if ( hmode .eq. 'box.grid') then
        write(*,'(a30,4f10.2)') ' lon1,lon2,lat1,lat2           : ', &
        &               lon1,lon2,lat1,lat2
     else if ( hmode .eq. 'point' ) then
        write(*,'(a30,2f10.2)') ' lon,lat                       : ', &
        &               lon1,lat1
     else if ( hmode .eq. 'shift' ) then
        write(*,'(a30,4f10.2)') ' lon1,lat1,dlon,dlat           : ', &
        &               lon1,lat1,dlon,dlat
     else if ( hmode .eq. 'polygon.eqd' ) then
        write(*,'(a30,a10,f10.2)') ' hfile, ds                  : ', &
        &               hfile, ds
     else if ( hmode .eq. 'polygon,grid' ) then
        write(*,'(a30,a10)')    ' hfile                         : ', &
        &               hfile
     else if ( hmode .eq. 'circle.eqd' ) then
        write(*,'(a30,4f10.2)') ' lonc,latc,radius,ds           : ', &
        &               lon1,lat1,radius,ds
     else if ( hmode .eq. 'circle.grid' ) then
        write(*,'(a30,3f10.2)') ' lonc,latc,radius              : ', &
        &               lon1,lat1,radius
     else if ( hmode .eq. 'region.eqd' ) then
        write(*,'(a30,i4,f10.2)')' iregion,ds                   : ', &
        &               iregion,ds
        write(*,'(a30,4f10.2)') ' xregion                       : ', &
        &               (xcorner(i),i=1,4)
        write(*,'(a30,4f10.2)') ' yregion                       : ', &
        &               (ycorner(i),i=1,4)
     else if ( hmode .eq. 'region,grid' ) then
        write(*,'(a30,i4)')     ' iregion                       : ',iregion
        write(*,'(a30,4f10.2)') ' xregion                       : ', &
        &               (xcorner(i),i=1,4)
        write(*,'(a30,4f10.2)') ' yregion                       : ', &
        &               (ycorner(i),i=1,4)
     else if ( hmode .eq. 'mask.eqd' ) then
        write(*,'(a30,a15,a15,f10.2)') ' hfile,variable,ds              : ',    &
        &               trim(hfile),trim(maskname),ds
     else if ( hmode .eq. 'mask.grid' ) then
        write(*,'(a30,2a15)') ' hfile, variable                 : ', &
        &               trim(hfile),trim(maskname)
     end if

     print*
     print*, ' Vertical mode (vmode)            : ', trim(vmode)
     if ( vmode .eq. 'file' ) then
        print*, ' filename                      : ', trim(vfile)
     else if ( vmode .eq. 'level' ) then
        print*, ' level                         : ', lev1
     else if ( vmode .eq. 'list' ) then
        print*, ' number of levels              : ', vn
        print*, ' list of levels                : ', (levlist(i),i=1,vn)
     else if ( vmode .eq. 'profile' ) then
        print*, ' lev1/lev2/n                   : ', lev1, lev2, vn
     else if ( vmode .eq. 'grid' ) then
        print*, ' lev1/lev2                     : ', lev1, lev2
     end if

     print*
     print*, ' umode                            : ', trim(umode)
     print*
     print*, ' time check                       : ', timecheck
     print*

! <mask.grid> and <mask.eqd>: split variable into name, operator, value
     if ( (hmode.eq.'mask.grid') .or. (hmode.eq.'mask.eqd') ) then
        len = len_trim(maskname)
        do i = 1, len_trim(maskname)-1

           ch1 = maskname(i:i)
           ch2 = maskname(i+1:i+1)

           if ( (ch1.eq.'<') .and. (ch2.eq.'>') ) then
              read(maskname(i+2:len),*) maskvalue
              maskname = maskname(1:i-1)
              maskoper = 'ne'
              go to 90
           else if ( (ch1.eq.'<') .and. (ch2.eq.'=') ) then
              read(maskname(i+2:len),*) maskvalue
              maskname = maskname(1:i-1)
              maskoper = 'le'
              go to 90
           else if ( (ch1.eq.'>') .and. (ch2.eq.'=') ) then
              read(maskname(i+2:len),*) maskvalue
              maskname = maskname(1:i-1)
              maskoper = 'ge'
              go to 90
           else if ( ch1.eq.'>' ) then
              read(maskname(i+1:len),*) maskvalue
              maskname = maskname(1:i-1)
              maskoper = 'gt'
              go to 90
           else if ( ch1.eq.'<' ) then
              read(maskname(i+1:len),*) maskvalue
              maskname = maskname(1:i-1)
              maskoper = 'lt'
              go to 90
           else if ( (ch1.eq.'=').and.(ch2.eq.'=') ) then
              read(maskname(i+2:len),*) maskvalue
              maskname = maskname(1:i-1)
              maskoper = 'eq'
              go to 90
           else if ( ch1.eq.'=' ) then
              read(maskname(i+1:len),*) maskvalue
              maskname = maskname(1:i-1)
              maskoper = 'eq'
              go to 90
           end if
                               
        end do

     end if
90   continue

!   ------------------------------------------------------------------------
!   Read grid parameters from intial files
!   ------------------------------------------------------------------------

!   Get the time of the first and second data file
    tstart = -timeshift                                 ! Format hh.mm
    call hhmm2frac(tstart,frac)
    frac = frac + timeinc                               ! Format frac
    call frac2hhmm(frac,tend)

!   Convert timeshift (hh.mm) into a fractional time shift
    tfrac = real(int(timeshift)) + 100.*(timeshift-real(int(timeshift)))/60.
    if ( tfrac .gt. 0. ) then
       tfrac = tfrac/timeinc
    else
       tfrac = 0.
    end if

!   Read the constant grid parameters
!   (nx,ny,nz,xmin,xmax,ymin,ymax,pollon,pollat). The negative <-fid> of the
!   file identifier is used as a flag for parameter retrieval.
    varname = 'U'
    nx      = 1
    ny      = 1
    nz      = 1
    call input_open (fid,file0)
    call input_grid (-fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,     &
        &            tstart,pollon,pollat,r1d,r1d,nz,r1d,r1d,rd,timecheck)
    call input_close(fid)

!   Check whether region coordinates are within the domain
    if ( (hmode.eq.'region.eqd').or.(hmode.eq.'region.grid') ) then
       do i = 1,4
          if ( (xcorner(i).lt.xmin).or.(xcorner(i).gt.xmax).or. &
        &      (ycorner(i).lt.ymin).or.(xcorner(i).gt.ymax) ) then
               print*,' ERROR: region not included in data domain... '
               print*,'         ', trim(string)
               print*,'         ',(xcorner(j),j=1,4)
               print*,'         ',(ycorner(j),j=1,4)
               print*
               stop
          end if
      end do
    end if

!   Check if the number of levels is too large
    if ( nz.gt.nlevmax ) go to 993

!   Allocate memory for 3d arrays: pressure
    allocate(pr(nx,ny,nz),stat=stat)
    if (stat.ne.0) print*, '*** error allocating array pr ***'
    allocate(prs(nx,ny),stat=stat)
    if (stat.ne.0) print*, '*** error allocating array prs ***'
    allocate(in(nx,ny,nz))
    if (stat.ne.0) print*, '*** error allocating array in ***'
    allocate(mask(nx,ny),stat=stat)
    if (stat.ne.0) print*, '*** error allocating array mask ***'

!   Allocate memory for temporary arrays for time interpolation
    allocate(fld0(nx,ny,nz),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array tmp0 ***'
    allocate(fld1(nx,ny,nz),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array tmp1 ***'
    allocate(sfc0(nx,ny),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array sfc0 ***'
    allocate(sfc1(nx,ny),stat=stat)
    if (stat.ne.0) print*,'*** error allocating array sfc1 ***'

!   ---------------- Index ---------------------------------

!   Init the dummy array with vertical index
    do i = 1,nx
       do j = 1,ny
          do k = 1,nz
             in(i,j,k) = real(k)
          end do
       end do
    end do

!   ---------------- Pressure ------------------------------
    
!   Read pressure from first data file (file0) on grid; we have to set mdv
!   explicitly since it's not read from netCDF file 
    call input_open(fid,file0)
    varname = 'U'
    stagz   = -0.5
    call input_grid(fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tstart, &
        &           pollon,pollat,fld0,sfc0,nz,ak,bk,stagz,timecheck)
    mdv = -999.99
    call input_close(fid)

!   Read or set pressure for second data file (file1)
    if ( timeshift.ne.0 ) then
       call input_open(fid,file1)
       varname = 'U'
       call input_grid(fid,varname,xmin,xmax,ymin,ymax,dx,dy,nx,ny,tend,&
        &              pollon,pollat,fld1,sfc1,nz,ak,bk,stagz,timecheck)
       call input_close(fid)
    else
       do i = 1,nx
          do j = 1,ny
             do k = 1,nz
                fld1(i,j,k) = fld0(i,j,k)
             end do  
             sfc1(i,j) = sfc0(i,j)
          end do
       end do
    end if

!   Time interpolation to get the final pressure field
    do i = 1,nx
       do j = 1,ny
          do k = 1,nz
             pr(i,j,k) = (1.-tfrac)*fld0(i,j,k) + tfrac*fld1(i,j,k)
          end do
             prs(i,j) = (1.-tfrac)*sfc0(i,j) + tfrac*sfc1(i,j)
       end do
    end do

!   ------------- Load 0/1 label (mask) ------------------------------
    if ( (hmode.eq.'mask.eqd').or.(hmode.eq.'mask.grid') ) then

       ! Explicitly read netCDF maskfile
       ierr = nf90_open(maskfile,nf90_noWrite,cdfid)
       if (ierr .ne. nf90_NoErr) print*, NF90_STRERROR(ierr)
       ierr = nf90_inq_varid(cdfid,maskname,varid)
       if (ierr .ne. nf90_NoErr) print*, NF90_STRERROR(ierr)
       ierr = nf90_get_var(cdfid,varid,mask)
       if (ierr .ne. nf90_NoErr) print*, NF90_STRERROR(ierr)
       ierr = nf90_close(cdfid)
       if (ierr .ne. nf90_NoErr) print*, NF90_STRERROR(ierr)

       nmask = 0
       do i = 1,nx
          do j = 1,ny

             if ( maskoper.eq.'eq' ) then
                if ( abs(mask(i,j) - maskvalue).lt.eps ) then
                   nmask = nmask + 1
                   mask(i,j) = 1.
                else
                   mask(i,j) = 0.
                end if

             else if ( maskoper.eq.'ne' ) then
                if ( abs(mask(i,j) - maskvalue).gt.eps ) then
                   nmask = nmask + 1
                   mask(i,j) = 1.
                else
                   mask(i,j) = 0.
                end if

             else if ( maskoper.eq.'le' ) then
                if ( mask(i,j).le.maskvalue ) then
                   nmask = nmask + 1
                   mask(i,j) = 1.
                else
                   mask(i,j) = 0.
                end if
        
             else if ( maskoper.eq.'lt' ) then
                if ( mask(i,j).lt.maskvalue ) then
                   nmask = nmask + 1
                   mask(i,j) = 1.
                else
                   mask(i,j) = 0.
                end if

             else if ( maskoper.eq.'ge' ) then
                if ( mask(i,j).ge.maskvalue ) then
                   nmask = nmask + 1
                   mask(i,j) = 1.
                else
                   mask(i,j) = 0.
                end if

             else if ( maskoper.eq.'gt' ) then
                if ( mask(i,j).ge.maskvalue ) then
                   nmask = nmask + 1
                   mask(i,j) = 1.
                else
                   mask(i,j) = 0.
                end if

             end if
        
          end do
       end do

       print*
       print*, ' Mask read from file ---> ',nmask,' 1-points'
       print*
    end if

!   ----------- Write some status information -----------------------
    print*,' ------- CONSTANT GRID PARAMETERS ---------- '
    print*
    print*,' + nx/ny/nz                ',nx,ny,nz
    print*,' + dx/dy                   ',dx,dy
    print*,' + xmin/xmax               ',xmin,xmax
    print*,' + ymin/ymax               ',ymin,ymax
    print*,' + pollon/pollat           ',pollon,pollat
    print*
    print*,' + Pressure loaded         ',trim(file0),'     ',trim(file1)

!   ---------- Determine the expanded list of starting coordinates ---
    
!   Write some status information
    print*
    print*,' ------ EXPAND LIST OF STARTING POSITIONS ---------- '
    print*

!   Read lon/lat/lev from hfile 'file3'
    if ( hmode .eq. 'file3' ) then
        start_n = 0
        open (110, file = hfile, status='old')

100          continue
             start_n = start_n + 1
             read (110,*,end=101) start_lon(start_n),start_lat(start_n), &
        &                       start_lev(start_n)
             go to 100

101          continue
             start_n = start_n - 1
        close (110)
        go to 400
    end if

!   ------------ Read lat/lon (horizontal coordinates) -----------------

!   Read lat/lon from hfile
    if ( hmode .eq. 'file2' ) then
        hn = 0
        open (111, file = hfile, status='old')

102          continue
             hn = hn + 1
             read (111,*,end=103) lonlist(hn),latlist(hn)
             go to 102

103          continue
             hn = hn - 1
        close (111)
    end if

!   Read lat/lon along a line (linear in lat/lon space with hn points)
    if ( hmode .eq. 'line' ) then
        do i = 1,hn
           lonlist(i) = lon1 + real(i-1)/real(hn-1)*(lon2-lon1)
           latlist(i) = lat1 + real(i-1)/real(hn-1)*(lat2-lat1)
        end do
    end if
       
!   Lon/lat box: equidistant
    if ( hmode .eq. 'box.eqd' ) then
        hn = 0
        lat = lat1

        do while (lat.le.lat2)
           lon = lon1
           do while (lon.le.lon2)
              hn          = hn + 1
              lonlist(hn) = lon
              latlist(hn) = lat
              lon         = lon + ds/(deltat*cos(pi180*lat))
           end do
           lat = lat + ds/deltat
        end do
    end if
    
!   Lon/lat box: grid
    if ( hmode .eq. 'box.grid' ) then
       hn = 0
       do i = 1,nx
          do j = 1,ny
             lon = xmin + real(i-1) * dx
             lat = ymin + real(j-1) * dy
             
             if ( (lon.ge.lon1).and.(lon.le.lon2).and. &
        &         (lat.ge.lat1).and.(lat.le.lat2) ) then
                hn = hn + 1
                lonlist(hn) = lon
                latlist(hn) = lat
             end if
          end do
       end do
    end if

!   point
    if ( hmode .eq. 'point' ) then
       hn = 1
       lonlist(hn) = lon1
       latlist(hn) = lat1
    end if

!   Get shifted and central starting point
    if ( hmode .eq. 'shift' ) then
        hn         = 5

        lonlist(1) = lon1
        latlist(1) = lat1

        lonlist(2) = lon1+dlon
        latlist(2) = lat

        lonlist(3) = lon1-dlon
        latlist(3) = lat1

        lonlist(4) = lon1
        latlist(4) = lat1+dlat

        lonlist(5) = lon1
        latlist(5) = lat1-dlat
    end if

!   Lat/lon polygon: grid
    if ( hmode .eq. 'poly.grid' ) then

!       Read list of polygon coordinates
        pn = 0
        open(112, file=hfile, status='old')
            read (112,*) loninpoly,latinpoly

104         continue
            pn = pn + 1
            read(112,*,end=104) lonpoly(pn),latpoly(pn)
            print*, pn,lonpoly(pn),latpoly(pn)
            go to 105
105         continue
            pn = pn - 1

        close (112)

!       Define polygon boundaries
        call DefSPolyBndry(latpoly,lonpoly,pn,latinpoly,loninpoly)

!       Get the grid points inside the polygon
        hn = 0
        do i = 1, nx
           do j = 1, ny

              lon = xmin + real(i-1) * dx
              lat = ymin + real(j-1) * dy

              call LctPtRelBndry(lat,lon,flag)

              if ( (flag.eq.1).or.(flag.eq.2) ) then
                 hn          = hn + 1
                 lonlist(hn) = lon
                 latlist(hn) = lat
              end if

           end do
        end do
        
    end if

!   Lat/lon polygon: equidistant
    if ( hmode.eq.'polygon.eqd' ) then

!       Read list of polygon coordinates
        pn = 0

        open (113,file=hfile,status='old')
            read (113,*) loninpoly, latinpoly

106         continue
            pn = pn + 1
            read (113,*,end=107) lonpoly(pn),latpoly(pn)
            go to 106
107         continue
            pn = pn - 1
        close (113)

!       Define the polygon boundaries
        call DefSPolyBndry(latpoly,lonpoly,pn,latinpoly,loninpoly)

!       Get the grid points inside the polygon
        hn      = 0
        lat     = -90.
        do while ( lat.le.90. )
           lon = -180.
           do while ( lon.lt.180. )

              call LctPtRelBndry(lat,lon,flag)

              if ( (flag.eq.1).or.(flag.eq.2) ) then
                 hn          = hn + 1
                 lonlist(hn) = lon
                 latlist(hn) = lat
              end if

              lon = lon +ds/(deltat*cos(pi180*lat))

          end do

          lat = lat + ds/deltat

        end do
    end if

!   Circle: equidistant
    if ( hmode.eq.'circle.eqd' ) then
        hn  = 0
        lat = ymin
        do while ( lat.le.ymax )
           lon = xmin
           do while ( lon.le.xmax )

              dist = sdis(lon1,lat1,lon,lat)
              if ( dist.le.radius ) then
                 hn          = hn + 1
                 lonlist(hn) = lon
                 latlist(hn) = lat
              end if
              lon = lon + ds/(deltat*cos(pi180 * lat))

           end do
           lat = lat + ds/deltat

        end do
    end if

!   Circle: grid
    if ( hmode.eq.'circle.grid' ) then
       hn = 0
       do i = 1, nx
          do j = 1, ny
             lon = xmin + real(i - 1) * dx
             lat = ymin + real(j - 1) * dy
             dist = sdis(lon1,lat1,lon,lat)
             if ( dist.le.radius ) then
                hn          = hn + 1
                lonlist(hn) = lon
                latlist(hn) = lat
             end if
          end do
       end do
    end if

!   Region: equidistant
    if ( hmode.eq.'region.eqd' ) then
       hn      = 0
       lat     = ymin
       do while ( lat.le.ymax )
          lon = xmin
          do while ( lon.le.xmax )
              flag = inregion(lon,lat,xcorner,ycorner)
              if ( flag.eq.1) then
                 hn          = hn + 1
                 lonlist(hn) = lon
                 latlist(hn) = lat
              end if
              lon = lon + ds/(deltat*cos(pi180*lat))
          end do
          lat = lat + ds/deltat
       end do
    end if

!   Region: grid
    if ( hmode.eq.'region.grid' ) then
        hn = 0
        do i = 1, nx
        do j = 1, ny
           lon  = xmin + real(i-1) * dx
           lat  = ymin + real(j-1) * dy
           flag = inregion(lon,lat,xcorner,ycorner)
           if ( flag.eq.1 ) then
              hn = hn  + 1
              lonlist(hn) = lon
              latlist(hn) = lat
           end if
        end do
        end do
    end if

!   Mask: grid
    if ( hmode .eq. 'mask.grid' ) then
       hn = 0
       do i = 1, nx
       do j = 1, ny
          if ( mask(i,j) .gt. 0.5 ) then
             hn = hn + 1
             lonlist(hn) = xmin + real(i - 1) * dx
             latlist(hn) = xmax + real(j - 1) * dy
          end if
       end do
       end do
    end if

!   Mask: equidistant
    if ( hmode .eq. 'mask.eqd' ) then
       hn = 0
       lat = -90. * dy
       do while (lat .le. ymax )
          lon = -180.
          do while ( lon .le. xmax )
               indx = nint( (lon - xmin)/dx + 1. )
               indy = nint( (lat - ymin)/dy + 1. )
               if ( indx .lt. 1 ) indx  = 1
               if ( indy .lt. 1 ) indy  = 1
               if ( indx .gt. nx ) indx = nx
               if ( indy .gt. ny ) indy = ny

               if ( mask(indx,indy) .gt. 0.5 ) then
                  hn          = hn + 1
                  lonlist(hn) = lon
                  latlist(hn) = lat
               end if
               lon = lon + ds/( deltat*cos(pi180*lat) )
          end do
          lat = lat + ds/deltat
       end do
    end if

!   ------------ Get lev (vertical) coordinates ----------------------

!   Read level list from file
    if ( vmode .eq. 'file' ) then
        vn = 0
        open (114, file = vfile, status='old' )

108          continue
                vn = vn + 1
                read(114,*,end=109) levlist(vn)
                go to 108
109          continue
             vn = vn - 1
        close (114)
    end if

!   Get single start level
    if ( vmode .eq. 'level' ) then
        vn = 1
        levlist(vn) = lev1
    end if

!   GEt level profiles
    if ( vmode .eq. 'profile' ) then
        do i = 1, vn
           levlist(i) = lev1 + real(i-1)/real(vn-1)*(lev2 - lev1)
        end do
    end if

!   Get all grid points in a layer: at the moment set the list of levels to all
!   indices from 1 to nz; later the corrected subset of indices will be chosen

    if ( vmode .eq. 'grid' ) then
        vn = nz
        do i = 1, vn
           levlist(i) = real(i)
        end do
        umode_save = umode
        umode      = 'INDEX'
    end if

!   ------- Compile the complete list of starting positions -----------
!   Get all starting points in specified vertical coordinate system
    start_n = 0
    do i = 1, vn
    do j = 1, hn
        
        start_n = start_n + 1
        if ( start_n .ge. nmax ) then
           print*, ' ERROR: maximum number of start points exceeded '
           stop
        end if
        start_lon(start_n) = lonlist(j)
        start_lat(start_n) = latlist(j)
        start_lev(start_n) = levlist(i)
    end do    
    end do 

!   ---------- Exit point of this section
400 continue

!   Write status information
    print*,' ----------- STARTING POSITIONS ------------------ '
    print*

!   Vertical mode [hPa.asl] or simply [hPa]
    if ( (umode.eq.'hPa.asl').or.(umode.eq.'hPa') ) then
        
        do i = 1, start_n
             start_pre(i) = start_lev(i)
        end do

!   Vertical mode [hPa.agl]
    else if ( umode.eq.'hPa.agl' ) then
        
        do i = 1, start_n
             call get_index3(rid,rjd,rkd,start_lon(i),start_lat(i),1050., &
        &                    3,pr,prs,nx,ny,nz,xmin,ymin,dx,dy,mdv)
             tmp1 = int_index3 (prs,nx,ny,1,rid,rjd,1.,mdv)
             start_pre(i) = tmp1 - start_lev(i)
        end do
           
!   Vertical mode [INDEX]
    else if ( umode .eq. 'INDEX' ) then
        
        do i = 1, start_n
             call get_index3(rid,rjd,rkd,start_lon(i),start_lat(i),1050., &
        &                    2,pr,prs,nx,ny,nz,xmin,ymin,dx,dy,mdv)
             rkd = start_lev(i)
             tmp1 = int_index3(pr,nx,ny,nz,rid,rjd,rkd,mdv)
             start_pre(i) = tmp1
        end do

    end if

!   ------------------------------------------------------------
!   Remove invalid points from the list
!   ------------------------------------------------------------

!   Select the correct subset if [vmode=grid]: starting points outside the layer
!   will receive a mdv vertical pressure and will be removed 

    if ( vmode .eq. 'grid' ) then
       
       do i = 1, start_n
       !   Get the pressure at the grid point 
           if ( ( umode_save .eq. 'hPa'     ) .or. &
        &     ( umode_save .eq. 'hPa.asl' ) ) then
                
              call get_index3(rid,rjd,rkd,start_lon(i),start_lat(i), &
        &                     start_pre(i),3,pr,prs,nx,ny,nz,xmin,   &
        &                     ymin,dx,dy,mdv)  
              tmp1 = int_index3(pr,nx,ny,nz,rid,rjd,rkd,mdv)
              call get_index3(rid,rjd,rkd,start_lon(i),start_lat(i), &
                              1050.,3,pr,prs,nx,ny,nz,xmin,ymin,dx,  &
                              dy,mdv)
              tmp2 = int_index3(prs,nx,ny,1,rid,rjd,1.,mdv)
              tmp1 = tmp2 - tmp1

       !   Get vertical index at the grid point
           else if ( umode_save .eq. 'INDEX' ) then
              
              call get_index3(rid,rjd,rkd,start_lon(i),start_lat(i), &
        &                     start_pre(i),3,pr,prs,nx,ny,nz,xmin,   &
        &                     ymin,dx,dy,mdv)
              tmp1 = int_index3(in,nx,ny,nz,rid,rjd,rkd,mdv)

           end if

       !  Remove points outside layer
          if ( ( tmp1.lt.lev1).or.(tmp1.gt.lev2) ) then
              start_pre(i) = mdv
          end if

       end do

    end if

!   Check whether the starting levels are valid (in data domain)
    do i = 1, start_n

        call get_index3(rid,rjd,rkd,start_lon(i),start_lat(i),1050., &
        &               3,pr,prs,nx,ny,nz,xmin,ymin,dx,dy,mdv)
        tmp1 = int_index3 (prs,nx,ny, 1,rid,rjd,      1.,mdv)   ! Surface
        print*, tmp1
        tmp2 = int_index3 (pr ,nx,ny,nz,rid,rjd,real(nz),mdv)   ! Top of domain
        print*, tmp2
        if ( (start_pre(i).gt.tmp1) .or.        &
        &    (start_pre(i).lt.tmp2) .or.        &
        &    (start_lon(i).lt.xmin) .or.        &
        &    (start_lon(i).gt.xmax) .or.        &
        &    (start_lat(i).lt.ymin) .or.        &
        &    (start_lat(i).gt.ymax) ) then
             start_pre(i) = mdv
        end if
    end do

!   Mark all starting points outside the domain
    n_outside = 0
    do i = 1, start_n

        start_valid(i) = 1
        if ( abs(start_pre(i) - mdv) .lt. eps ) then

           start_valid(i) = 0
           if ( vmode .ne. 'grid' ) then
              if ( n_outside .lt. 0 ) then
                 print*, ' Outside: ',start_lon(i),start_lat(i),start_lev(i)
                 n_outside = n_outside + 1

              else if ( n_outside .eq. 10 ) then
                print*,' Outside: more than 10 starting points'
                n_outside = n_outside + 1    

              else
                n_outside = n_outside + 1
              end if
          end if

        end if

    end do

!   Only keep valid starting points
    j = 0
    do i = 1, start_n
        if ( start_valid(i) .eq. 1 ) then
           j = j + 1
           start_lon(j) = start_lon(i)
           start_lat(j) = start_lat(i)
           start_lev(j) = start_lev(i)
           start_pre(j) = start_pre(i)
        end if
    end do
    start_n = j

!   Write some status information
    latmin = start_lat(1)
    latmax = start_lat(1)
    lonmin = start_lon(1)
    lonmax = start_lon(1)
    premin = start_pre(1)
    premax = start_pre(1)
    do i = 1, start_n
        if (start_lat(i) .lt. latmin) latmin = start_lat(i)
        if (start_lat(i) .gt. latmax) latmax = start_lat(i)
        if (start_lon(i) .lt. lonmin) lonmin = start_lon(i)
        if (start_lon(i) .gt. lonmax) lonmax = start_lon(i)
        if (start_pre(i) .lt. premin) premin = start_pre(i)
        if (start_pre(i) .gt. premax) premax = start_pre(i)
    end do

    print*,' min(lat),max(lat) : ',latmin,latmax
    print*,' min(lon),max(lon) : ',lonmin,lonmax
    print*,' min(pre),max(pre) : ',premin,premax
    print*
    print*,' No. of starting points : ',start_n
    print*,' No. of outside points  : ',n_outside
    print*

!   --------------------------------------------------------------------
!   Write starting positions to output file
!   --------------------------------------------------------------------

!   Output as a trajectory file (with only one time == 0)
    if ( oformat.ne.-1 ) then
        
       allocate(tra(start_n,1,5),stat=stat)

       vars(1) = 'time'
       vars(2) = 'lon'
       vars(3) = 'lat'
       vars(4) = 'p'
       vars(5) = 'level'

       call wopen_tra(fid,ofile,start_n,1,5,reftime,vars,oformat)

       do i = 1,start_n
          tra(i,1,1) = 0.
          tra(i,1,2) = start_lon(i)
          tra(i,1,3) = start_lat(i)
          tra(i,1,4) = start_pre(i)
          tra(i,1,5) = start_lev(i)
       end do

       call write_tra(fid,tra,start_n,1,5,oformat)

       call close_tra(fid,oformat)

!   Output as a triple list (corresponding to [startf] file)
    else

       fid = 10
       open (fid,file=ofile,status='new')
       do i = 1,start_n
            write(fid,'(3f10.3)') start_lon(i),start_lat(i),start_pre(i)
       end do
       close(fid)
  
    end if

!   Write some status information, and end of program message
    print*
    print*,' ---------- STATUS INFORMATION ------------- '
    print*,
    print*
    print*,'    *** END OF PROGRAM CREATE_STARTF ***     '
    print*,' =========================================== '

!   ------------------------------------------------------------------
!   Exceptional handling
!   ------------------------------------------------------------------
    stop

993 write(*,*) '*** ERROR: problems with array size ***'
    call exit(1)

end
