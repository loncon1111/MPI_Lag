program density

    use netcdf
    use interp
    use iotra
    use ncinput
    

    implicit none

!   ---------------------------------------------------------------------
!   Declaration of variables
!   ---------------------------------------------------------------------

    ! Parameter and working arrays
    real                    :: radius
    character*80            :: runit
    integer                 :: nx,ny
    integer                 :: nlonlat
    real                    :: dlonlat
    real                    :: xmin,ymin,dx,dy
    real                    :: clon,clat
    integer                 :: ntime,nfield,ntra
    character*80            :: inpfile
    character*80            :: outfile
    character*80            :: mode
    real                    :: param
    integer                 :: opts,npts
    integer                 :: step
    character*80            :: gridtype
    character*80            :: field
    integer                 :: crefile,crevar
    real,allocatable,dimension(:,:) :: cnt,res,fld,area
    real,allocatable,dimension(:)   :: traj
    real,allocatable,dimension(:)   :: olon,olat,otim,ofld
    real,allocatable,dimension(:)   :: nlon,nlat,ntim,nfld

    ! Output format
    character*80           :: outformat

    ! Physical and mathematical constants
    real,parameter         :: pi180 = 4*atan(1.)/180.    ! deg -> rad
    real,parameter         :: deltay = 111.2             ! km
    real,parameter         :: eps = 0.001

    ! Input trajectories
    integer                :: inpmode                   
    real,allocatable,dimension(:,:,:) :: trainp         ! Input trajectories(ntra,ntim,ncol)
    integer                :: reftime(6)                ! Reference date 
    character*80           :: varsinp(100)              ! Field names for input trajectory
    integer,allocatable,dimension(:)  :: sel_flag       
    character*80           :: sel_file
    character*80           :: sel_format

    ! Auxiliary variables
    character*80           :: cdfname,varname
    integer                :: i,j,k
    integer                :: stat
    integer,allocatable,dimension(:,:) :: connect0, connect1, connect2
    integer                :: connectval0,connectval1,connectval2
    real                   :: slat
    integer                :: ipre
    real                   :: addvalue
    real                   :: xmax,ymax
    real,allocatable,dimension(:) :: odist,ndist
    real                   :: dt
    integer                :: fid
    integer                :: dynamic_grid
    real                   :: ycen,xcen
    integer                :: indx,indy
    character*80           :: unit
    real                   :: pollon,pollat
    real                   :: rlon0,rlat0,rlon,rlat
    real                   :: lon,lat
    real                   :: crot
    integer                :: count
    character*80           :: longname,varunit
    real                   :: time
    integer                :: ind
    integer                :: ifield
    real                   :: hhmm,frac
    integer                :: ierr,ncID

    ! External functions

    namelist /params/ 
    namelist /input/  inpfile, ntime, nfield, ntra, field
    namelist /output/ outfile
