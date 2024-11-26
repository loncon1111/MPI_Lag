   program gettidiff

        implicit none

        integer :: idate(5), irefdate(5)
        real    :: ihdiff

        integer :: iargc
        character*80:: arg
        integer :: nc1,nc2,flag1,flag2

        ! check for sufficient requested arguments
        if (iargc() .ne. 2) then
           go to 101
        end if

        ! read and transform input
        call getarg(1,arg)   ! default in fortran (at least in GNU)
        call lenchar(arg,nc1)
        call checkchar(arg,'_',flag1)
        
        idate(5)    = 0
        irefdate(5) = 0

        if (flag1 .eq. 7) then

           read(arg(1:2),'(i2)',err=120) idate(1)
           read(arg(3:4),'(i2)',err=120) idate(2)
           read(arg(5:6),'(i2)',err=120) idate(3)
           read(arg(8:9),'(i2)',err=120) idate(4)

           if (nc1.eq.11) then 
              read(arg(10:11),'(i2)',err=120) idate(5)
           else if (nc1.ne.9) then
              go to 101
           end if

        else if (flag1 .eq. 9) then

           read(arg(1:4),'(i4)',err=120) idate(1)
           read(arg(5:6),'(i2)',err=120) idate(2)
           read(arg(7:8),'(i2)',err=120) idate(3)
           read(arg(10:11),'(i2)',err=120) idate(4)

           if (nc1.eq.13) then
              read(arg(12:13),'(i2)',err=120) idate(5)
           else if (nc1.ne.11) then
              go to 101
           end if

        else
          
           go to 101

        end if

        ! reference date
        call getarg(2,arg)
        call lenchar(arg,nc2)
        call checkchar(arg,'_',flag2)

        if (flag1 .ne. flag2) then
           print*, "error: both dates must be in the same format"
           go to 101
        end if

        if (flag2 .eq. 7) then

           read(arg(1:2),'(i2)',err=120) irefdate(1)
           read(arg(3:4),'(i2)',err=120) irefdate(2)
           read(arg(5:6),'(i2)',err=120) irefdate(3)
           read(arg(8:9),'(i2)',err=120) irefdate(4)

           if (nc2.eq.11) then
              read(arg(10:11),'(i2)',err=120) irefdate(5)
           else if (nc2.ne.9) then
              go to 101
           end if

        else if (flag2 .eq. 9) then

           read(arg(1:4),'(i4)',err=120) irefdate(1)
           read(arg(5:6),'(i2)',err=120) ierfdate(2)
           read(arg(7:8),'(i2)',err=120) irefdate(3)
           read(arg(10:11),'(i2)',err=120) irefdate(4)
        
           if (nc2.eq.13) then
              read(arg(12:13),'(i2)',err=120) irefdate(5)
           else if (nc2.ne.11) then
              go to 101
           end if

        else

           go to 101

        end if
        
        call timediff(idate,irefdate,ihdiff)

        if (int(100.*ihdiff) .eq. 100*int(ihdiff)) then
           write(*,*) int(ihdiff)
        else
           write(*,'(f7.2)') int(ihdiff)
        end if
        go to 102
        

   101  print*, 'USAGE: gettidiff date1 date2 (format (YY)YYMMDD_HH(MM))'
        call exit(1)

   102  continue
   end

   subroutine lenchar(string,lstr)

        character*(*) ::string
        integer       :: n,lstr

        do n = 1, len(string)
           if ( string(n:n) .eq. "" ) then
              lstr = n - 1
              go to 100
           end if
        end do

   100  continue

   end subroutine lenchar

   subroutine checkchar(string,char,flag)
        
        character*(*) :: string
        character*1   :: char
        integer       :: n, flag

        flag = 0
        do n = 1, len(string)
           if ( string(n:n) .eq. char ) then
              flag = n
              return
           endif
        end do

   end subroutine checkchar
