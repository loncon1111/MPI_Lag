! ############################################################################################
! 1. input_open: to open a datafile
! 2. input_dim : to read dimensions
! 3. input_3ddata: to read data information
! 4. input_close: to close a datafile
! Re-created from Michael Spenger, 2004
! By HoaDao, 2019
! ############################################################################################

module module_ncinput
        contains

        subroutine input_open(fid,filename)
           use netcdf
           implicit none
           
           ! declaration of subroutine variables
           integer       :: fid      ! file identifer
           character*100 :: filename ! filename

           integer       :: istat    ! status number

           ! open netcdf file
           istat = nf90_open(trim(filename), nf90_NoWrite, fid)
           if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
        end subroutine input_open

        subroutine input_dim(fid,fieldname,rx,ry,rz,nx,ny,nz,
       >                     dx,dy,time,timecheck,xmin,xmax,ymin,ymax)

           ! read grid information at <time> (each file contains one frame time only) 
           ! with identifier <fid>. 
           ! The horizontal grid is characterized by <rx,ry,dx,dy>
           ! and grid dimension <dx,dy>. The 3d arrays give the vertical
           ! coordinates, either on the staggered or unstaggered grid.
           ! the surface pressure <ps(nx,ny)>. 

           use netcdf
           implicit none

           ! declaration of subroutine variables
           integer      :: fid                   ! file identifier
           character*100:: fieldname             ! variable in which to take grid info
           integer,intent(out):: nx, ny, nz      ! grid dimension
           real,dimension(:),allocatable,intent(out)::rx, ry, rz ! grid lon,lat,lev
           real,dimension(:),allocatable:: rtime
           real,intent(in) :: time
           integer,intent(out):: timecheck       ! timecheck is to get the right index of time in case file has many timesteps
           real, intent(out):: xmin, xmax, ymin, ymax ! minimum,maximum lat/lon location
           real,dimension(:,:) intent(out),allocatable:: ps ! surface pressure

           ! auxiliary variables
           integer      :: i,j,k                ! temporary variables
           integer      :: dimids( nf90_max_vars_dim),dimid     ! dimension identifiers
           character*80 :: dimname(nf90_max_vars_dim)           ! dimension name
           integer      :: istat                ! status number
           integer      :: varid                ! temporary variable identifier
           character*100:: leveltype            ! type of vertical dimension
           integer,dimension(:),allocatable:: vardim               !
           character*80 :: units                ! unit of vertical dim  

           ! get an example var to read dimensions, get number of dims of var ->
           ! ndim
           istat = nf90_inq_varid(fid, fieldname, varid)
           if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
           istat = nf90_inquire_variable(fid,varid,ndims = ndim)
           if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)

           ! get dimension names and information following number ndim
           allocate(vardim(ndim))
           istat = nf90_inquire_variable(fid, varid, dimids = dimids(1:ndim))
           if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)

           do i = 1, ndim
              !istat = nf90_inq_dimid(fid, varid, dimid = dimids(i) )
              istat = nf90_inquire_dimension(fid, dimids(i), name = dimname(i))
              if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
              istat = nf90_inquire_dimension(fid, dimids(i), len = vardim(i))
              if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
           enddo

           ! get horizontal and vertical dimension, but first to make sure this file has
           ! ndims >= 3
           ! also to check the type of vertical dimension
           if ( ndim .lt. 3 ) then
              print*, "Not enough dimensions..."
              return
           endif

           do i = 1, ndim

              if (( (trim(dimname(i)).eq.'longitude' ).or.((trim(dimname(i)).eq.'lon' )) then
                allocate(rx(vardim(i)),stat=stat)
                if ( stat .ne. 0 ) print*, 'Unable to create longitude variable')
                ! get longitude variable as rx(:)
                istat = nf90_inq_varid( fid, dimname(i), varid)
                if ( istat .ne. nf90_NoErr ) print*,NF90_STERROR(istat)
                istat = nf90_get_var( fid, varid, rx)
                if ( istat .ne. nf90_NoErr ) print*,NF90_STERROR(istat)
                nx = vardim(i)
              end if

              if (( (trim(dimname(i)).eq.'latitude').or.((trim(dimname(i)).eq.'lat' )) then
                allocate(ry(vardim(i)),stat=stat)
                if ( stat .ne. 0 ) print*, 'Unable to create latitude variable')
                ! get latitude variable as ry(:)
                istat = nf90_inq_varid( fid, dimname(i), varid)
                if ( istat .ne. nf90_NoErr ) print*,NF90_STERROR(istat)
                istat = nf90_get_var( fid, varid, ry)
                if ( istat .ne. nf90_NoErr ) print*,NF90_STERROR(istat)
                ny = vardim(i)
              end if

              if (((trim(dimname(i)).eq.'level').or.((trim(dimname(i)).eq.'lev' )) then
                allocate(rz(vardim(i)),stat=stat)
                if ( stat .ne. 0 ) print*, 'Unable to create level variable')
                ! get level variable as rz(:)
                istat = nf90_inq_varid( fid, dimname(i), varid)
                if ( istat .ne. nf90_NoErr ) print*,NF90_STERROR(istat)
                istat = nf90_get_var( fid, varid, ry)
                if ( istat .ne. nf90_NoErr ) print*,NF90_STERROR(istat)
                ! get type of vertical dimension
                istat = nf90_get_att( fid, varid, 'standard_name', leveltype)
                if ( istat .ne. nf90_NoErr ) print*,NF90_STERROR(istat)
                if (levtype.ne.'air_pressure') then
                   print*, "Unsupport vertical type,
        >           please to make sure its standard_name level type is air_pressure" 
                   return
                end if
                ! check whether lev type is in hPa or Pa - correct if necessary
                istat = nf90_get_att( fid, varid, 'units', units)
                if ( istat .ne. nf90_NoErr ) print*,NF90_STERROR(istat)
                if ( trim(units) .eq. 'Pa' ) then
                   do j = 1, vardim(i)
                      rz(j) = rz(j)/100.
                   end do
                end if 
                nz = vardim(i)
              end if

              ! get time information
              if (trim(dimname(i)).eq.'time') then
                allocate(rtime(vardim(i)))
                istat = nf90_inq_varid( fid, dimname(i), varid)
                if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
                istat = nf90_get_var( fid, varid, rtime )
                if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
                ! check whether time is correct
                do j = 1, vardim(i)
                   if ( abs(time - rtime(j)) .eq. 0. ) then
                      timecheck = j
                   end if
                end do
                if ( timecheck .eq. 0 ) then
                   print*, "ERROR: Unavailability of time at ",time," not found
        >                   on NetCDF file"
                   return
                endif
              endif

           end do

           ! get surface pressure
           allocate(ps(nx,ny))
           istat = nf90_inq_varid( fid, 'PS', varid)
           if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
           istat = nf90_get_var( fid, ps, varid)
           ! check whether ps's units are 'hPa' or not
           

           ! Printout dx, dy
           xmin = rx(1)
           xmax = rx(nx)
           ymin = ry(1)
           ymax = ry(ny)
           dx = (xmax - xmin)/real(nx - 1) ! dx in degree
           dy = (xmax - xmin)/real(ny - 1) ! dy in degree
           
        return
        end subroutine input_dim

        subroutine input_3ddata(fid,fieldname,field,dx,dy,nx,ny,nz,timecheck,
        >                       xmin,xmax,ymin,ymax,ps)
        
        ! Read the data <fieldname> from the file with identifier <fid> and save
        ! it in the 3d array <field> (which time step is <timecheck> of the time
        ! dims). A consistency check is performed to have an agreement with the
        ! grid specified by <xmax,xmin,ymax,ymin,dx,dy,nx,ny,nz>
           use netcdf
           implicit none

           ! Declaration of variables
           integer,intent(in)   :: fid          ! file identifier
           character*100,intent(in):: fieldname ! name of input variable
           real, intent(out)    :: field(nx,ny,nz)    ! data of input variable
           real, intent(in)     :: dx,dy        ! grid spacing
           integer,intent(in)   :: nx,ny,nz     ! length of dimensions
           integer,intent(in)   :: timecheck
           real, intent(in)     :: xmin,xmax,ymin,ymax  ! horizontal first/last points
           
           ! Auxiliary variables
           integer              :: i,j,k        ! temporary variables
           integer      :: dimids( nf90_max_vars_dim),dimid     ! dimensionidentifiers
           character*80 :: dimname(nf90_max_vars_dim)           ! dimension name
           integer              :: ndim 
           integer              :: istat        ! status number
           integer              :: varid        ! variable identifier
           real                 :: add, factor  ! add_offset and scale_factor of variable
           integer,dimension(:),allocatable:: vardim

           istat = nf90_inq_varid( fid, fieldname, varid )
           if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)

           ! check if variable is truly 3D (not include time)
           istat = nf90_inquire_variable( fid, varid, ndims = ndim)

           ! get dimension names and information following number ndim
           allocate(vardim(ndim))
           istat = nf90_inquire_variable(fid, varid, dimids = dimids(1:ndim))
           if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)

           do i = 1, ndim
              !istat = nf90_inq_dimid(fid, varid, dimid = dimids(i) )
              istat = nf90_inquire_dimension(fid, dimids(i), name = dimname(i))
              if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
              istat = nf90_inquire_dimension(fid, dimids(i), len = vardim(i))
              if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
           enddo

           ! make sure its dimensions are lon/lat/lev/time respectively
           if ( ndim.ne.4 ) then
              print*, "ERROR: Inconsistence in number of dims from variable",fieldname
           end if

           icheck = 0
           if ((vardim(1).eq.nx).and.((dimname(1).eq.'longitude').or.(dimname(1).eq.'lon'))) icheck = icheck+1
           if ((vardim(2).eq.ny).and.((dimname(2).eq.'latitude').or.(dimname(2).eq.'lat')))  icheck = icheck+1
           if ((vardim(3).eq.nz).and.((dimname(3).eq.'level').or.(dimname(3).eq.'lev')))     icheck = icheck+1
           if (dimname(3).eq.'time') icheck = icheck+1
           if ( icheck .ne. 4 ) then
              Print*, 'ERROR: Inconsistence in lon(longitude)/lat(latitude)/lev(level)/time respectively' 
              return
           end if

           ! get variable
           istat = nf90_get_var( fid, varid, field(:,:,:,timecheck))
           if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
           ! if this variable is 'short', read add_offset & scale_factor
           istat = nf90_get_att( fid, varid, 'add_offset', add )
           istat = nf90_get_att( fid, varid, 'scale_factor', factor)
           do i = 1, nx
           do j = 1, ny
           do k = 1, nz
              field(i,j,k) = field(i,j,k) * factor + add
           end do 
           end do

        end subroutine input_3ddata

        subroutine input_close(fid,istat)
        ! Close file NetCDF
           
           ! Declaration of variables
           integer,intent(in) :: fid
           integer            :: istat
           
           istat = nf90_close(fid)
           if ( istat .ne. nf90_NoErr ) print*, NF90_STERROR(istat)
        end subroutine input_close
end module module_ncinput
