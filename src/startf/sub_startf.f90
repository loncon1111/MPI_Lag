module sub_startf

contains

subroutine regionsplit(string,iregion,xcorner,ycorner)

! The region string comes either as <lonw,lone,lats,latn> or as
! <lon1,lat1,lon2,lat2,lon3,lat3,lon4,lat4>: split it into ints components and
! get the four coordinates for the region

    implicit none

! Declaration of subroutine parameters
    character*80       :: string
    real,dimension(4)  :: xcorner, ycorner
    integer            :: iregion

! Local variables
    integer            :: i,n
    integer            :: il,ir
    real,dimension(80) :: subfloat
    integer            :: stat
    integer            :: len

! Split the string
    i   = 1
    n   = 0
    stat= 0
    il  = 1
    len = len_trim(string)

100 continue

! Find start of a substring 

    do while ( stat.eq.0 )
        if ( (string(i:i).eq.' ') .or. (i.eq.len) ) then
           stat = 1
           il   = i
        else
           i    = i + 1
        end if
    end do

! Find end of a substring
    do while ( stat.eq.1 )
        if ( string(i:i).eq.' ') then
           stat = 2
           ir   = i
        else
           i = i + 1
        end if
    end do

! Convert the substring into a number
    if ( stat.eq.2 ) then
        n = n + 1
        read(string(il:ir),*) subfloat(n)
        stat = 0
    end if

    if ( i.lt.len ) go to 100

! Get the region number 
    iregion = nint(subfloat(1))
! Get the corners of the region
    if ( n.eq.5 ) then   ! lonw(2),lone(3),lats(4),latn(5)
        xcorner(1) = subfloat(2)
        ycorner(1) = subfloat(4)

        xcorner(2) = subfloat(3)
        ycorner(2) = subfloat(4)

        xcorner(3) = subfloat(3)
        ycorner(3) = subfloat(5)

        xcorner(4) = subfloat(2)
        ycorner(4) = subfloat(5)
    else if ( n.eq.9) then   !lon1(2),lat1(3),lon2(4),lat2(5),lon3(6),lat3(7),lon4(8),lat4(9)
        xcorner(1) = subfloat(2)
        ycorner(1) = subfloat(3)

        xcorner(2) = subfloat(4)
        ycorner(2) = subfloat(5)

        xcorner(3) = subfloat(6)
        ycorner(3) = subfloat(7)

        xcorner(4) = subfloat(8)
        ycorner(4) = subfloat(9)
    else
        print*, " ERROR: Invalid region specification "
        print*, "       ",trim(string)
        stop
    end if

end subroutine regionsplit

real function sdis(xp,yp,xq,yq)

!   calculate spherical distance (in km) between 2 points given by their
!   spherical coordinates (xp,yp) and (xq,yq), respectively.

    real,parameter :: re = 6370.
    real,parameter :: pi180 = 4*atan(1.0)/180.
    real           :: xp,yp,xq,yq,arg

    arg = sin(pi180*yp)*sin(pi180*yq) + cos(pi180*yp)*cos(pi180*yq)*cos(pi180*(xp-xq))
    if (arg.lt.-1.) arg = -1.
    if (arg.gt. 1.) arg =  1.

    sdis = re * acos(arg)

end function sdis

!   ---------------------------------------------------------------------------
!   Decide whether lat/lon point is in or out of region
!   ---------------------------------------------------------------------------

integer function inregion(lon,lat,xcorner,ycorner)
    
!   Decide whether point (lon/lat) is in the region specified by <xcorner(1..4),
!   ycorner(1..4)

    implicit none

!   Declaration of subroutine parameters
    real        :: lon,lat
    real        :: xcorner(4),ycorner(4)

!   Local variables
    integer     :: flag
    real        :: xmin,xmax,ymin,ymax
    integer     :: i

!   Reset the flag
    flag = 0

!   Set some boundaries
    xmax = xcorner(1)
    xmin = xcorner(1)
    ymax = ycorner(1)
    ymin = ycorner(1)

    do i = 2, 4
        if (xcorner(i).lt.xmin) xmin = xcorner(i)
        if (xcorner(i).gt.xmax) xmax = xcorner(i)
        if (ycorner(i).lt.ymin) ymin = ycorner(i)
        if (ycorner(i).gt.ymax) ymax = ycorner(i)
    end do

!   Do the test. If all tests passed then flag = 1
    flag = 0
    if ( lon.lt.xmin ) go to 970
    if ( lon.gt.xmax ) go to 970
    if ( lat.lt.ymin ) go to 970
    if ( lat.gt.ymax ) go to 970

    if ( (lon - xcorner(1)) * (ycorner(2) - ycorner(1)) - &
        &(lat - ycorner(1)) * (xcorner(2) - xcorner(1)) .gt. 0. ) go to 970

    if ( (lon - xcorner(2)) * (ycorner(3) - ycorner(2)) - &
        &(lat - ycorner(2)) * (xcorner(3) - xcorner(2)) .gt. 0. ) go to 970

    if ( (lon - xcorner(3)) * (ycorner(4) - ycorner(3)) - &
        &(lat - ycorner(3)) * (xcorner(4) - xcorner(3)) .gt. 0. ) go to 970
 
    if ( (lon - xcorner(4)) * (ycorner(1) - ycorner(4)) - &
        &(lat - ycorner(4)) * (xcorner(1) - ycorner(4)) .gt. 0. ) go to 970

    flag = 1

!   Return the value 
970 continue

    inregion = flag

    return

end function inregion

!   ****************************************************************************
!   *     Given some spherical polygon S and some point X known to be located  *
!   * inside S, these routines will determine if an arbitrary point P lies     *
!   * inside S, outside S, or on its boundary.                                 *
!   *  - The calling program must first call DefSpolyBndry to define the       *
!   *  boundary of S and the point X.                                          *
!   *  - Any subsequent call to subroutine LctPtRelBndry will determine if some*
!   *   point P lies inside or outside S, or on its boundary.                  *
!   *  - (Usually DefSPolyBndry is called once,                                *
!   *         the LctPtRelBndry is called many times.                          *
!   ****************************************************************************

subroutine DefSPolyBndry(vlat,vlon,nv,xlat,xlon)
    
!   ****************************************************************************
!   * This mmn entry point is used m define ~ e sheric~ polygon S and the point*
!   * X.                                                                       *
!   * ARGUMENTS:                                                               *
!   * vlat, vlon (sent) ... vectors containing the latitude and longitude of   *
!   *                       each vertex of the spherical polygon S. The ith    *
!   *                       vertex is located at [vlat(i),vlon(i)].            *
!   * nv         (sent) ... the number of vertices and sides in the spherical  *
!   *                       polygon S.                                         *
!   * xlat,xlon  (sent) ... latitude and longitude of some point X located     *
!   *                       inside S. X must not be located on any great circle*
!   *                       that includes 2 vertices of S.                     *
!   *                                                                          *
!   * UNITS AND SIGN CONVENTION:                                               *
!   *  Latitudes and longitudes are specified in degrees.                      *
!   *  Latitudes are positive to the north and negative to the south.          *
!   *  Longitudes are positive to the east and negative to the west.           *
!   *                                                                          *
!   * VERTEX ENUMERATION:                                                      *
!   *  The vertices of S should be numbered sequentially around the border of  *
!   *  the spherical polygon. Vertex 1 lies between vertex n and vertex 2.     *
!   *  Neighbouring vertices must be separated by less than 180 degrees. (In   *
!   *  order to generate a polygon side whose arc length equals to exceeds 180 *
!   *  degrees simply introduce an additional (pseudo)vertex). Having chosen vertex
!   *  1, the user may number the remaining vertices in either direction.      *
!   *  However if the user wishes to use the subroutine SPA to determine the area of the
!   *  polygon S (Bevis & Cambareri, 1987, Math. Geol., v.19, p. 335-346) then *
!   *  one must follow the convention whereby in moving around the polygon border
!   *  in the direction of increasing vertex number clockwise bends occur at   *
!   *  salient vertices. A vertex is salient if the interior angle is less than 180
!   *  degrees. (In the case of a convex polygon thes convention implies that  *
!   *  vertices are numbered in clockwise sequence).                           *
!   ****************************************************************************

    implicit none

    integer :: mxnv, nv

!   ----------------------------------------------------------------------------
!   Edit next statement to increase maximum number of vertices that may be used
!   to define the spherical polygon S.
!   The value of parameter mxnv in subroutine LctPtRelBndry must match that of
!   parameter mxnv in this subroutine, as assigned above.       
!   ----------------------------------------------------------------------------
    parameter (mxnv=500)

    real :: vlat(nv),vlon(nv),xlat,xlon,dellon
    real :: tlonv(mxnv),vlat_c(mxnv),vlon_c(mxnv),xlon_c,xlat_c
    integer :: i,ibndry,nv_c,ip

    data ibndry/0/

    common /spolybndry/ vlat_c,vlon_c,nv_c,xlat_c,xlon_c,tlonv,ibndry

    if (nv.gt.mxnv) then
        print*,'nv exceeds maximum allowed value'
        print*,'adjust parameter mxnv in subroutine DefSPolyBndry'
        stop
    end if

    ibndry = 1                  ! boundary defined at leas once (flag)
    nv_c   = nv                 ! copy for named common
    xlat_c = xlat
    xlon_c = xlon

    do i = 1, nv
        vlat_c(i) = vlat(i)
        vlon_c(i) = vlon(i)

        call TrnsfmLon(xlat,xlon,vlat(i),vlon(i),tlonv(i))

        if (i.gt.1) then
           ip = i - 1
        else
           ip = nv
        end if

        if ( (vlat(i).eq.vlat(ip)).and.(vlon(i).eq.vlon(ip)) ) then
           print*,' DefSPolyBndry detects user error:'
           print*,' Vertices ',i,' and ',ip,' are not distinct'
           print*,' lat ',i,ip,vlat(i),vlat(ip)
           print*,' lon ',i,ip,vlon(i),vlon(ip)
           stop
        end if
       
        if ( tlonv(i).eq.tlonv(ip) ) then
           print*,' DefSPolyBndry detects user error:'
           print*,' Vertices ',i,' and ',ip,' are on the same gt. circle as X'
           stop
        end if

        if ( vlat(i) .eq. (-vlat(ip)) ) then
           dellon = vlon(i) - vlon(ip)
           if ( dellon.gt.+180. ) dellon = dellon - 360.
           if ( dellon.lt.-180. ) dellon = dellon + 360.
           if ( (dellon.eq.+180.).or.(dellon.eq.-180.) ) then
              print*,' DefSPolyBndry detects user error:'
              print*,' Vertices ',i,' and ',ip,' are antipodal.'
              stop
           end if
        end if
    end do 

    return
end subroutine DefSPolyBndry

subroutine LctPtRelBndry(plat,plon,location)

!   ****************************************************************************
!   * This routine is used to see if some point P is located inside,outside or
!   * on the boundary of the spherical polygon S previously defined by a call to
!   * subroutine DefSPolyBndry. There is a single restriction to point P: it must
!   * not be antipodal to the point X defined in the call to DefSPolyBndry (i.e. P
!   * and X cannot be separated by exactly 180 degrees.
!   * ARGUMENTS:
!   * plat,plon (sent) ... the latitude and longitude of point P
!   * location  (returned) specifies the location of P:
!   *                      location=0 implies P is outside of S
!   *                      location=1 implies P is inside of S
!   *                      location=2 implies P is on boundary of S
!   *                      location=3 implies usererror (P is antipodal to X
!   *
!   * UNIT AND SIGN CONVENTION:
!   *  Latitude and longitudes are specified in degrees.
!   *  Latitudes are positive to the north and negative to the south
!   *  Longitudes are positive to the east and negative to the west
!   ****************************************************************************

    implicit none

!   -----------------------------------------------------------------------------
!   Thd statement below must match that in subroutine DefSPolyBndry
!   -----------------------------------------------------------------------------

    integer, parameter :: mxnv = 500
    real        :: tlonv(mxnv),vlat_c(mxnv),vlon_c(mxnv),xlat_c,xlon_c
    real        :: plat,plon,vAlat,vAlon,vBlat,vBlon,tlonA,tlonB,tonP
    real        :: tlon_X,tlon_P,tlon_B,dellon
    integer     :: i,ibndry,nv_c,location,icross,ibrngAB,ibrngAP,ibrngPB
    integer     :: ibrng_BX,ibrng_BP,istrike

    common /spolybndry/ vlat_c,vlon_c,nv_c,xlat_c,xlon_c,tlonv,ibndry

    if (ibndry.eq.0) then       ! user has never defined the bndry
        print*,' Subroutine LctPtRelBndry detects user error:'
        print*,' Subroutine DefSPolyBndry must be called before'
        print*,' Subroutine LctPtRelBndry can be called'
        stop
    end if

    if ( plat.eq.(-xlat_c) ) then
        dellon = plon - xlon_c
        if ( dellon.lt.(-180.) ) dellon = dellon + 360.
        if ( dellon.gt.(+180.) ) dellon = dellon - 360.
        if ( (dellon.eq.+180.).or.(dellon.eq.-180.) ) then
           print*,' Warning: LctPtRelBndry detects case P antipodal to X'
           print*,' location of P relative to S is undetermined'
           location = 3
           return
        end if
    end if

    location = 0                ! default ( P is outside S )
    icross   = 0                ! initialize counter

    if ((plat.eq.xlat_c).and.(plon.eq.xlon_c)) then
        location = 1
        return
    end if

    call TrnsfmLon (xlat_c,xlon_c,plat,plon,tlon_P)

    do i = 1,nv_c               ! start of loop over sides of S
        
        vAlat = vlat_c(i)
        vAlon = vlon_c(i)
        tlonA = tlonv(i)

        if (i.lt.nv_c) then
           vBlat = vlat_c(i+1)
           vBlon = vlon_c(i+1)
           tlonB = tlonv(i+1)
        else
           vBlat = vlat_c(1)
           vBlon = vlon_c(1)
           tlonB = tlonv(1)
        end if

        istrike = 0

        if (tlon_P.eq.tlonA) then
           istrike = 1
        else
           call EastorWest(tlonA,tlon_B,ibrngAB)
           call EastorWest(tlonA,tlon_P,ibrngAP)
           call EastorWest(tlon_P,tlon_B,ibrngPB)

           if ( (ibrngAP.eq.ibrngAB).and.(ibrngPB.eq.ibrngAB) ) istrike = 1
        end if

        if (istrike.eq.1) then
           
           if ( (plat.eq.vAlat).and.(plon.eq.vAlon) ) then
              location = 2      ! P lies on a vertex of S
              return
           end if
           call TrnsfmLon(vAlat,vAlon,xlat_c,xlon_c,tlon_X)
           call TrnsfmLon(vAlat,vAlon,vBlat,vBlon,tlon_B)
           call TrnsfmLon(vAlat,vAlon,plat,plon,tlon_P)

           if (tlon_P.eq.tlon_B) then
              location = 2     ! P lies on side of S
              return
           else
              call EastorWest(tlon_B,tlon_X,ibrng_BX)
              call EastorWest(tlon_B,tlon_P,ibrng_BP)
              if (ibrng_BX.eq.(-ibrng_BP)) icross = icross+1
           end if
        end if
    end do                     ! end of loop over the sides of S

!   If the arc XP crosses the boundary S an even number of times when P is in S

    if ( mod(icross,2).eq.0) location = 1

    return
     
endsubroutine LctPtRelBndry

subroutine TrnsfmLon(plat,plon,qlat,qlon,tranlon)

!   ****************************************************************************
!   * This subroutine is required by subroutine DefSPolyBndry & LctPtRelBndry. *
!   * It finds the 'longitude' of point Q in a geographic coordinate system for*
!   * which point P acts as a 'north pole'. SENT: plat,plot,qlat,qlon, in degs.*
!   * RETURNED: tranlon, in degs.                                              *
!   ****************************************************************************

    implicit none

    real :: pi,dtr,plat,plon,qlat,qlon,tranlon,t,b
    parameter (pi=4*atan(1.0),dtr=pi/180.)

    if (plat.eq.90) then
        tranlon = qlon
    else
        t = sin((qlon-plon)*dtr) * cos(qlat*dtr)
        b = sin(qlat*dtr)*cos(plat*dtr) - cos(qlat*dtr)*sin(plat*dtr)*cos((qlon-plon)*dtr)
        tranlon = atan2(t,b)/dtr
    end if

    return
end subroutine TrnsfmLon

subroutine EastOrWest(clon,dlon,ibrng)

!  ****************************************************************************
!  * This subroutine is required by subroutine LctPtRelBndry.
!  * This subroutine determenes if in travelling the shortest path from point C
!  * (at longitude clon) to point D (at longitude dlon) one is heading east,
!  * west or neither.
!  * SENT: clon,dlon in degrees. RETURNED: ibrng (1=east,-1=west,0=neither)
!  ****************************************************************************

   implicit none
   real         :: clon,dlon,del
   integer      :: ibrng
   
   del = dlon - clon
   if (del.gt.180.) del = del - 360.
   if (del.lt.-180.) del = del + 360.
   if ( (del.gt.0.).and.(del.ne.180.) ) then
        ibrng = -1                      ! D is west of C
   else if ( (del.lt.0.).and.(del.ne.-180.) ) then
        ibrng = +1                      ! D is east of C
   else
        ibrng = 0                       ! D north or south of C
   end if
   return

end subroutine EastOrWest

end module sub_startf
