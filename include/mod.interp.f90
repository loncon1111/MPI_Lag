module interp

    ! =================================================================
    !     This module provides a general interpolation routine
    ! =================================================================

    !     The main interface routines are:
    !      get_index3,4 : get the grid indices for interpolation
    !      int_index3,4 : interpolate to the grid position

      contains

      subroutine get_index3(rid,rjd,rkd,xpo,ypo,ppo,mode,vert,surf,&
                           &nx,ny,nz,lonw,lats,dlon,dlat,misdat)

      !     Purpose:
      !        This subroutine determines the indices (rid,rjd,rkd) in grid space
      !        for a point in physical space (xpo,ypo,ppo). The horizontal grid 
      !        is specified by the south-west point (lonw,lats) and the grid
      !        spacing (dlat,dlon). The vertical grid is given by
      !        <vert(n1,n2,n3)>. The lower boundary (typically surface pressure)
      !        is given by <surf(n1,n2)>

      !     Arguments:
      !        rid,rjd,rkd  real  output   grid location to be interpolated to
      !        xpo,ypo,ppo  real  input    physical coordinates
      !        rtp          real  input    relative time position (0=beginning,
      !        1=end)
      !        nx,ny,nz     int   input    grid dimensions in x-, y- and
      !        p-direction
      !        lats,lonw    real  input    south and west boundary of grid space
      !        vert         real  input    vertical coordinate grid
      !        surf         real  input    lower boundary (surface pressure)
      !        mode         int   input    direction of vertical axis
      !        (p=1,th=-1)
      !                                        1: linear, 1 -> nz (th)
      !                                        2: linear, nz -> 1 (pv)
      !                                        3: binary (p)

          implicit none
      ! declaration of subroutine variables
          real,intent(out)  :: rid,rjd,rkd      ! grid location to be interpolated to
          real,intent(in)   :: xpo,ypo,ppo      ! physical coordinates
          integer,intent(in):: ny,nx,nz         ! grid dimensions in x-, y- and p- direction
          real,intent(in)   :: lats,lonw        ! south and west boundary of grid space
          real,intent(in)   :: vert(nx*ny*nz)             ! vertical coordinate grid 
          real,intent(in)   :: surf(nx*ny)             ! surface pressure
          integer,intent(in)   :: mode
          real,intent(in)   :: dlat,dlon        ! grid spacing in x and y         
          real              :: misdat

          real,parameter :: eps=1.2e-8
      ! local variables
          integer           :: i,j,k
          real              :: ppo0,ppo1,ppom,psur
          integer           :: i0,im,i1

      ! Externals
          real, external    :: int_index3

      ! get the horizontal indices refer to south-west point
          rid = (xpo - lonw)/dlon+1. ! spacing plus 1 as number of points
          rjd = (ypo - lats)/dlat+1.

      ! if 2D horizontal interpolation is enough, there's only 1 vertical point
          if ( nz .eq. 1) then
             rkd = 1
             return
          end if

      ! retrieve pressure at surface and near bottom of the domain
          ppo0 = int_index3(vert,nx,ny,nz,rid,rjd,real(1),0.)       ! pressure at near bottom
          psur = int_index3(surf,nx,ny, 1,rid,rjd,real(1),0.)       ! pressure at surface
          
      ! see if the pressure is descending or ascending
          ppo1 = int_index3(vert,nx,ny,nz,rid,rjd,real(nz),0.)      ! pressure on top
          if ( ppo0 .lt. ppo1 ) ppo0 = ppo1

      ! if the point is between the bottom and surface, return
          if ( mode .eq. 3 ) then
          if ((ppo .le. ppo0) .and. (ppo .ge. psur) .or.&
        &     (ppo .ge. ppo0) .and. (ppo .le. psur)) then
              rkd = (psur - ppo)/(psur - ppo0)
              return
          end if  
          end if        

      ! full-level search (TH): linear ascending scanning through all levels
          if ( mode .eq. 1 ) then
              ppo0 = int_index3(vert,nx,ny,nz,rid,rjd,real(1),0.)
              rkd  = 0.

              do k = 1, nz-1
                 ppo1 = int_index3(vert,nx,ny,nz,rid,rjd,real(k+1),0.)
                 if ( ppo .le. ppo1 .and. ppo .gt. ppo0 ) then 
                    rkd = real(k) + (ppo - ppo0)/(ppo1 - ppo0)
                    return
                 end if
                 ppo0 = ppo1
              end do

      ! full-level search (PV): linear descending scanning through all levels
          else if ( mode .eq. 2 ) then
              ppo0 = int_index3(vert,nx,ny,nz,rid,rjd,real(nz),0.)
              rkd = 0
                
              do k = nz-1, 1, -1
                 ppo1 = int_index3(vert,nx,ny,nz,rid,rjd,real(k),0.)
                 if ( ppo .ge. ppo1 .and. ppo .lt. ppo0) then 
                    rkd = real(k) + (ppo0 - ppo)/(ppo1 - ppo0)
                    return
                 end if
                 ppo0 = ppo1
              end do

      ! full-level search (P): binary search
          else if ( mode .eq. 3 ) then
              rkd  = 0
              i0   = 1
              i1   = nz  
              ppo0 = int_index3(vert,nx,ny,nz,rid,rjd,real(i0), 0.)
              ppo1 = int_index3(vert,nx,ny,nz,rid,rjd,real(i1), 0.)

              do while ( (i0+1) .lt. i1)
                 im  = int((i0 + i1)/2)
                 ppom= int_index3(vert,nx,ny,nz,rid,rjd,real(im),0.)

                 if (ppom .lt. ppo) then   ! because it is in descending order
                    i1 = im
                    ppo1 = ppom
                 else
                    i0 = im
                    ppo0 = ppom
                 end if
              end do

              rkd = real(i0) + (ppo0 - ppo)/(ppo0 - ppo1)
          end if

 
          return       
      end subroutine get_index3

      ! ---------------------------------------------------
      ! Interpolate to an arbitrary position in grid space
      ! ---------------------------------------------------

      real function int_index3(arr,n1,n2,n3,rid,rjd,rkd,misdat)
      !     Purpose:
      !        This function interpolates the 3D-array into an arbitrary
      !        location within the grid. The interpolation include the testing
      !        of existing miss data flag 'misdat'. If one dimension is one, 2D
      !        interpolation is performed. If 2 dimensions are one, 1D
      !        interpolation is performed. If all 3 dimensions are one, no
      !        interpolation is performed, return value is the input value.


      ! three dimensional interpolation grid
      !            100.___.__________.110
      !               |\   \         |\
      !               | \   \        | \
      !               |  \   *d1     |  \
      !               |101.__|\______|___.111
      !               |   |  |       |   |
      !               |   |  *P      |   |
      !               |   |  |       |   |
      !            000.___|__|_______.010|
      !                \  |\ |        \  |
      !                 \ | \|         \ |
      !                  \|  *d2        \|
      !                001.___\__________.010
      ! ar(P) = frac001 * val001 + ... (8-point stencils)

        implicit none
        ! declaration of function variables
        real   :: rid,rjd,rkd        ! grid location to be interpolated to
        integer:: n1, n2, n3         ! grid dimensions in x-, y- and p- direction
        real   :: arr(n1*n2*n3)      ! input data array
        real   :: misdat             ! missing value flag

        ! epsilon
        real,parameter ::eps = 1.e-08

        ! local variables
        integer:: i,j,k,ip1,jp1,kp1
        real   :: frac0i,frac0j,frac0k,frac1i,frac1j,frac1k     
        real   :: ri,rj,rk
        real   :: val000,val001,val010,val100,val110,val111,val011,val101  ! value of nearby points
        real   :: frc000,frc001,frc010,frc100,frc110,frc111,frc011,frc101  ! fraction of nearby points
        real   :: mdv                ! missing data value
        real   :: val
        real   :: frc

        ! elementary tests for dimension
        if ( n1.lt.1 .or. n2.lt.1 .or. n3.lt.1 ) then
           print*, "Invalid dimensions' size :", n1, n2, n3
           return
        end if
        
        ! activate or inactivate the missing data 
        if ( misdat .ne. 0 ) then
           mdv = misdat
        else
           mdv = 257.22725394015
        end if

        ! bring the indices into the grid space
        ri = amax1(1., amin1(float(n1), rid))  ! find real max value in case outside dom 
        rj = amax1(1., amin1(float(n2), rjd))  ! find max, in case rid,... is negative
        rk = amax1(1., amin1(float(n3), rkd))

        ! get the index of the west-south-bottom corner of the box
        i  = min0(int(ri),n1 - 1)
        ip1= i + 1
        j  = min0(int(rj),n2 - 1)
        jp1= j + 1
        k  = min0(int(rk), n3 - 1)
        kp1= k + 1

        ! special handlings for 2D horizontal dimensions
        if ( n3 .eq. 1 ) then
           k = 1
           kp1 = 1
        end if

        ! get location relative to grid box
        frac0i = ri - float(i)
        frac1i = 1. - frac0i

        frac0j = rj - float(j)
        frac1j = 1. - frac1j

        if ( k .ne. kp1) then
           frac0k = rk - float(k)
           frac1k = 1. - frac0k
        else
           frac0k = 0.
           frac1k = 1.
        end if

        ! on a grid point, take the grid point value
        if ((abs(frac0i).lt.eps).and.(abs(frac0j).lt.eps).and.(abs(frac0k.lt.eps))) then
           val = arr( i + n1*(j-1) + n1*n2*(k-1) )
           return
        end if

        ! init the fractions
        frc000 = frac0i * frac0j * frac0k
        frc001 = frac0i * frac1j * frac0k
        frc010 = frac1i * frac0j * frac0k
        frc100 = frac0i * frac0j * frac1k
        frc110 = frac1i * frac0j * frac1k
        frc101 = frac0i * frac1j * frac1k
        frc011 = frac1i * frac1j * frac0k
        frc111 = frac1i * frac1j * frac1k

        ! init the values
        val000 = arr(i   + n1 * (j  -1) + n1 * n2 * (k  -1))
        val001 = arr(i   + n1 * (jp1-1) + n1 * n2 * (k  -1))
        val010 = arr(ip1 + n1 * (j  -1) + n1 * n2 * (k  -1))
        val100 = arr(i   + n1 * (j  -1) + n1 * n2 * (kp1-1))
        val110 = arr(ip1 + n1 * (j  -1) + n1 * n2 * (kp1-1))
        val101 = arr(i   + n1 * (jp1-1) + n1 * n2 * (kp1-1))
        val011 = arr(ip1 + n1 * (jp1-1) + n1 * n2 * (k  -1))
        val111 = arr(ip1 + n1 * (jp1-1) + n1 * n2 * (kp1-1))

        ! check with missing values
        if ( abs(val000 - mdv) .lt. eps ) frc000 = 0.
        if ( abs(val001 - mdv) .lt. eps ) frc001 = 0.
        if ( abs(val010 - mdv) .lt. eps ) frc010 = 0.
        if ( abs(val100 - mdv) .lt. eps ) frc100 = 0.
        if ( abs(val110 - mdv) .lt. eps ) frc110 = 0.
        if ( abs(val101 - mdv) .lt. eps ) frc101 = 0.
        if ( abs(val011 - mdv) .lt. eps ) frc011 = 0.
        if ( abs(val111 - mdv) .lt. eps ) frc111 = 0.

        ! build the final values
        frc = frc000 + frc001 + frc010 + frc011 +&
             &frc100 + frc101 + frc110 + frc111
        if ( frc.gt.0. ) then
           val = 1./frc * ( frc000 * val000 + frc001 * val001 +&
                           &frc010 * val010 + frc011 * val011 +&
                           &frc100 * val100 + frc101 * val101 +&
                           &frc110 * val110 + frc111 * val111 )
        else
           val = misdat
        endif

        ! return the final value
        int_index3 = val

        return
      end function int_index3


      ! ---------------------------------------------------
      ! Interpolate to time
      ! ---------------------------------------------------

      real function int_time(val0,val1,reltpos,misdat)

      !     Purpose:
      !        This function interpolates linearly in time between 2 values  

        implicit none 

        ! declaration of function variables
        real    :: val0         ! value at time 0
        real    :: val1         ! value at time 1
        real    :: reltpos      ! relative time to value at time 1 (between 0 and 1)
        real    :: misdat       ! missing value flag ( on if misdat.ne.0)
        ! epsilon
        real,parameter ::eps = 1.e-08
        ! local variables
        real    :: val
        real    :: mdv

        ! activate or inactivate missing data 
        if ( misdat .ne. 0. ) then
           mdv = misdat
        else
           mdv = 257.22725394015
        end if

        ! do the interpolation
        if ( abs(reltpos).lt.eps ) then
           val = val0
        elseif ( abs(1-reltpos).lt.eps) then
           val = val1
        elseif ( (abs(val0-mdv).gt.eps) .and.&
                &(abs(val1-mdv).gt.eps) ) then
           
           val = (1.-reltpos)*val0 + reltpos*val1
        else
           val = mdv
        end if

        ! return value
           int_time = val
        
      end function int_time

      ! ---------------------------------------------------
      ! Interpolation to an arbitrary grid space and time
      ! ---------------------------------------------------
      
      real function int_index4(arr0,arr1,nx,ny,nz,rid,rjd,rkd,rtp,misdat)

      !     Purpose:
      !        This function interpolates the 3D-array into an arbitrary
      !        location within the grid and in a time relative to rpt.
      !        using linear time-interpolation.

        implicit none

        ! declaration of function variables
        real    :: arr0(nx*ny*nz)       ! input array at pre-time
        real    :: arr1(nx*ny*nz)       ! input array at later
        integer :: nx,ny,nz             ! grid dimensions in x-, y- and p- direction
        real    :: rid,rjd,rkd          ! grid location indices
        real    :: misdat               ! missing value flag (<>0 if enable)
        real    :: rtp                  ! relative time position as compared to later time

        ! numerical parameters
        real,parameter :: eps = 1.e-08

        ! local variables
        real    :: val,val0,val1

        ! externals
        real, external :: int_index3    ! 3d interpolation
        real, external :: int_time      ! time interpolation

        if ( abs(rtp) .lt. eps ) then
           val = int_index3(arr0,nx,ny,nz,rid,rjd,rkd,misdat)
        elseif ( abs(1.-rtp) .lt. eps ) then
           val = int_index3(arr1,nx,ny,nz,rid,rjd,rkd,misdat)
        else
           val0 = int_index3(arr0,nx,ny,nz,rid,rjd,rkd,misdat)
           val1 = int_index3(arr1,nx,ny,nz,rid,rjd,rkd,misdat)
           val  = int_time(val0,val1,rtp,misdat)
        end if

        ! return value
        int_index4 = val
        
        return
      end function int_index4


      subroutine get_index4 (rid,rjd,rkd,xpo,ypo,ppo,rtp,&
                            &vert0,vert1,surf0,surf1,mode,&
                            &nx,ny,nz,lonw,lats,dlon,dlat,misdat)

      !     Purpose:
      !        This subroutine determines the indices (rid,rjd,rkd) in grid 
      !        space for a point in physical space (xpo,ypo,ppo). The 
      !        horizontal grid is specified by the south-west point (lats,lonw)
      !        and the grid spacing (dlat,dlon). The vertical grid is given
      !        by <vert(n1,n2,n3)>. The lower boundary (typically surface 
      !        pressure) is given by <surf(n1,n2)>.

      !     Arguments:
      !        rid,rjd,rkd  real  output   grid location to be interpolated to
      !        xpo,ypo,ppo  real  input    physical coordinates
      !        rtp          real  input    relative time position (0=beginning, 1=end)
      !        n1,n2,n3     int   input    grid dimensions in x-, y- and p-direction
      !        lats,lonw    real  input    south and west boundary of grid space
      !        vert         real  input    vertical coordinate grid
      !        surf         real  input    lower boundary (surface pressure)
      !        mode         int   input    direction of vertical axis (p=1,th=-1)
      !                                        1: linear, 1 -> nz (th)
      !                                        2: linear, nz -> 1 (pv)
      !                                        3: binary (p)

        implicit none
        ! declaration of subroutine variables
        real, intent(out) :: rid, rjd, rkd     ! grid location to be interpolated to
        real, intent(in)  :: xpo, ypo, ppo     ! physical coordinates
        real, intent(in)  :: rtp               ! relative time position (0=beginning, 1=end)
        integer,intent(in):: nx,ny,nz          ! grid dimensions in x-, y- and p- direction
        real, intent(in)  :: lats, lonw        ! south and west boundary of grid space
        real  :: vert0(nx*ny*nz),vert1(nx*ny*nz)       ! vertical coordinate grid at pre and later time
        real  :: surf0(nx*ny),surf1(nx*ny)       ! lower boundary surface pressure
        integer,intent(in):: mode              ! direction of vertical axis
        real, intent(in)  :: dlon,dlat         ! grid spacing
        real              :: misdat            ! missing data value
        ! numerical parameters:
        real, parameter :: eps = 1.e-08
        ! auxiliary variables
        real :: rid0,rjd0,rkd0,rid1,rjd1,rkd1
        ! externals
        real,external :: int_time
        
        if ( abs(rtp) .lt. eps ) then

           call get_index3(rid0,rjd0,rkd0,xpo,ypo,ppo,mode,vert0,surf0,&
                          &nx,ny,nz,lonw,lats,dlon,dlat,misdat)
           rid = rid0
           rjd = rjd0
           rkd = rkd0

        else if ( abs(1.-rtp) .lt. eps ) then

           call get_index3(rid1,rjd1,rkd1,xpo,ypo,ppo,mode,vert1,surf1,&
                           &nx,ny,nz,lonw,lats,dlon,dlat,misdat)
           rid = rid1
           rjd = rjd1
           rkd = rkd1

        else
        
           call get_index3(rid0,rjd0,rkd0,xpo,ypo,ppo,mode,vert0,surf0,&
                           &nx,ny,nz,lonw,lats,dlon,dlat,misdat)

           call get_index3(rid1,rjd1,rkd1,xpo,ypo,ppo,mode,vert1,surf1,&
                           &nx,ny,nz,lonw,lats,dlon,dlat,misdat)

           rid = int_time(rid0,rid1,rtp,misdat)
           rjd = int_time(rjd0,rjd1,rtp,misdat)
           rkd = int_time(rkd0,rkd1,rtp,misdat)

        end if
        return           
      end subroutine get_index4



end module interp
