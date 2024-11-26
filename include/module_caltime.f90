module caltime
!   ***************************************************************
!   *  This library provides subroutines related to time and file *
!   *  name handling                                              *
!   *                                                             *
!   ***************************************************************
    contains

!   ---------------------------------------------------------------
!   Days of month calculation
!   ---------------------------------------------------------------

    integer function daysofmonth(yyyy,mm)
        implicit none
        integer :: yyyy, mm

        select case (mm)

        case (2)
          if ( (mod(yyyy,4) .eq. 0) .and. (mod(yyyy,100).ne.0) .or. &
  &            (mod(yyyy,400).eq.0) ) then
             daysofmonth = 29
          else
             daysofmonth = 28
          end if

        case (1,3,5,7,8,10,12)
          daysofmonth = 31

        case default 
          daysofmonth = 30

        end select

        return
    end function daysofmonth

!   ---------------------------------------------------------------
!   Concatenate a datestring
!   ---------------------------------------------------------------
 
    subroutine datestring(datestr,yyyy,mm,dd,hh)
        implicit none
        ! declaration of subroutine variables
        integer     :: yyyy,mm,dd,hh
        character*30:: datestr

        ! auxiliary variables
        character*10:: y1,m1,d1,h1

        datestr = ''

        write (y1,'(i4.4)') yyyy
        if ( len(trim(y1)) .eq. 4 ) then
           datestr = trim(datestr)//trim(y1)
        else 
           print*, "Invalid year... Stop"
           stop
        end if

        if ( (mm .ge. 1).and.(mm.le.12) ) then
           write (m1,'(i2.2)') mm
           datestr = trim(datestr)//trim(m1)
        else
          print*, "Invalid month... Stop"
          stop
        end if

        if ( (dd .ge. 1).and.(dd .le. daysofmonth(yyyy,mm) ) ) then
           write (d1,'(i2.2)') dd
           datestr = trim(datestr)//trim(d1)
        else
           print*, "Invalid day... Stop"
           stop
        end if

        if ( hh.ge.0 ) then
           write (h1,'(i2.2)') hh
           datestr = trim(datestr)//"_"//trim(h1)
        end if
    end subroutine datestring

!   --------------------------------------------------------------------
!   Calculate the new date when diff (in hours) is added to date1
!   --------------------------------------------------------------------

    subroutine newdate(date1,diff,date2)

!   date1       int     input   array contains a date in the form
!                               year,month,day,hour,step
!   date1       int     output  array contains new date in the same form
!   diff        real    intput  timestep in hours to go from date1

    implicit none
    integer     :: date1(5),date2(5)
    real        :: diff
    logical     :: yearchange
    integer     :: i        

    yearchange = .false.

    do i = 1, 4
        date2(i) = date1(i)
    end do
    date2(5) = 0

    date2(4) = date1(4) + int(diff) + date1(5)

    if ( date2(4) .ge. 24 ) then
       date2(3) = date2(3) + int(date2(4)/24)
       date2(4) = date2(4) - 24*int(date2(4)/24)
    end if

    if ( date2(4) .lt. 0 ) then
       if (mod(date2(4),24) .eq. 0 ) then
          date2(3) = date2(3) - int(abs(date2(4))/24)
          date2(4) = date2(4) + 24*int(abs(date2(4))/24)
       else
          date2(3) = date2(3) - (1+int(abs(date2(4))/24))
          date2(4) = date2(4) + 24*(1+int(abs(date2(4))/24))
       end if
    end if

    if ( date2(3) .ge. 1 ) then

       do while ( date2(3) .gt. daysofmonth(date2(1),date2(2)) )
          date2(3) = date2(3) - daysofmonth(date2(1),date2(2))
          date2(2) = date2(2) + 1
       
          if ( date2(2) .gt. 12 ) then
             date2(2) = date2(2) - 12
             date2(1) = date2(1) + 1
          end if

          if ( date2(2) .lt. 1  ) then
             date2(1) = date2(1) - (1+int(abs(date2(2)/12)))
             date2(2) = date2(2) + (1+int(abs(date2(2)/12)))*12
          end if
       end do

    else
       
       do while ( date2(3) .lt. 1 )
          date2(2) = date2(2) - 1

          if ( date2(2) .gt. 12 ) then
             date2(2) = date2(2) + (1+int(abs(date2(2)/12)))*12
             date1(1) = date2(1) - (1+int(abs(date2(2)/12)))
          end if

          if ( date2(2) .lt. 1 ) then
             date2(2) = date2(2) + (1+int(abs(date2(2)/12)))*12
             date2(1) = date2(1) - (1+int(abs(date2(2)/12)))
          end if

          date2(3) = date2(3) + daysofmonth(date2(1),date2(2)) 
       end do
    end if

       if ( date2(2) .gt. 12) then
          date2(2) = date2(2) - int(date2(2)/12)*12
          date2(1) = date2(1) + int(date2(2)/12)
       end if

       if ( date2(2) .lt. 1 ) then
          date2(2) = date2(2) + (1+int(abs(date2(2)/12)))*12
          date2(1) = date2(1) - (1+int(abs(date2(2)/12)))
       end if

       return    
    end subroutine newdate

!   -----------------------------------------------------------------
!   Convert [hh.mm] to [frac] and [frac] to [hh.mm]
!   -----------------------------------------------------------------

    subroutine hhmm2frac (hhmm,frac)
       implicit none

       real :: hhmm
       real :: frac

       frac = real(int(hhmm)) + 100.*(hhmm-real(int(hhmm)))/60.

    end subroutine hhmm2frac

    subroutine frac2hhmm(frac,hhmm)
       implicit none
        
       real :: hhmm
       real :: frac

       real :: hh, mm
       integer :: changesign

       real,parameter :: eps = 1.e-5

       changesign = 0.
       if ( frac.lt.0. ) changesign = 1

       if ( changesign .eq. 1 ) frac = -frac

       hh = real( int(frac+eps) )
       mm = real( nint( 60. * (frac-hh) ) )

       hhmm = hh + 0.01*mm

       if ( changesign.eq.1 ) then
          hhmm = - hhmm
          frac = - frac
       end if

    end subroutine frac2hhmm

!   ----------------------------------------------------------------
!   Time difference between 2 dates
!   ----------------------------------------------------------------

    subroutine timediff(date1,date2,diff)

!   new version with hour and minutes ( for hour and step (in hours)
!   use the routine oldtimediff)
!
!   calculate the time difference in hours (and minutes) for the 2 dates
!   specified by the 2 arrays date1 and date2.
!   they are expected to contain the following date information:
!       year    month   day     hour    minute.

      integer   date1(5),date2(5)
      integer   idays(12)       ! array containing the days of the monthes
      real      diff
      integer   ixday,imdiff,ihdiff,iddiff,j
      integer   yy,yy1,yy2

      idays(1)=31
      idays(2)=28
      idays(3)=31
      idays(4)=30
      idays(5)=31
      idays(6)=30
      idays(7)=31
      idays(8)=31
      idays(9)=30
      idays(10)=31
      idays(11)=30
      idays(12)=31

!     Check format of year (YYYY or YY - in case of YY assume 19YY)

      if (date1(1).lt.100) date1(1)=1900+date1(1)
      if (date2(1).lt.100) date2(1)=1900+date2(1)

!     Determine if the period between date1 and date2 contains a Feb.29

      ixday=0   ! extra day flag

      yy1=min(date1(1),date2(1))
      yy2=max(date1(1),date2(1))
      if (yy1.eq.yy2) then
        if (mod(yy1,4).eq.0) then
          idays(2)=29
        endif
      else
        if (mod(yy1,4).eq.0) then
          if (((yy1.eq.date1(1)).and.(date1(2).le.2)).or.       &
     &        ((yy1.eq.date2(1)).and.(date2(2).le.2))) then
            ixday=ixday+1
          endif
        endif
        if (mod(yy2,4).eq.0) then
          if (((yy2.eq.date1(1)).and.(date1(2).gt.2)).or.       &
     &        ((yy2.eq.date2(1)).and.(date2(2).gt.2))) then
            ixday=ixday+1
          endif
        endif
        if (yy2-yy1.gt.1) then
          do yy=yy1+1,yy2-1
            if (mod(yy,4).eq.0) then
              ixday=ixday+1
            endif
          enddo
        endif
      endif

      ihdiff=0  ! diff. in hours between date1/date2
      iddiff=0  ! diff. in days  between date1/date2

      if (date1(1).gt.date2(1)) then            ! compare years
        do j=date2(1),date1(1)-1
          iddiff=iddiff+365
        enddo
        iddiff=iddiff+ixday
      else if (date1(1).lt.date2(1)) then
        do j=date1(1),date2(1)-1
          iddiff=iddiff-365
        enddo
        iddiff=iddiff-ixday
      endif

      if (date1(2).gt.date2(2)) then            ! compare monthes
        do j=date2(2),date1(2)-1
          iddiff=iddiff+idays(j)
        enddo
      else if (date1(2).lt.date2(2)) then
        do j=date1(2),date2(2)-1
          iddiff=iddiff-idays(j)
        enddo
      endif

      iddiff=iddiff+date1(3)-date2(3)
      ihdiff=iddiff*24+date1(4)-date2(4)
      imdiff=ihdiff*60+date1(5)-date2(5)

      ihdiff=imdiff/60
      imdiff=mod(imdiff,60)

      diff=real(ihdiff)+real(imdiff)/100.

      return

    end subroutine timediff
end module caltime
