module iotra

!  *****************************************************************************
!  * This package provides IO routines for trajectories. A file                *
!  * is characterized by the filename [filename] and the file identifier       *
!  * [fid]. Different modes [mode] are supported:                              *
!  *    mode = 1: ascii, sorted by trajectory                                  *
!  *    mode = 2: ascii, sorted by time                                        *
!  *    mode = 3: fortran (unformatted)                                        *
!  *    mode = 4: IVE netcdf (for compatibility reasons)                       *
!  *    mode = 5: KML                                                          *
!  *    mode = 6: as mode 1, but with pressure as real k number                *
!  * A trajectory set is given as 3d array [tra(ntra,ntim,ncol)] where ntra is *
!  * the number of trajectories, [ntim] the number of times of each trajectory *
!  * and [ncol] the number of columns of the trajectory. The first 4 columns   *
!  * are: time,longitude,latitude,pressure. The other columns are trace fields.*
!  * The complete list of all columns is given in the array [vars(ncol)].      *
!  * Finally, the reference date is given in the array                         *
!  * [time(6)=year/month/day/hour/time length of the trajectory {hour,min}     *
!  *****************************************************************************

     implicit none
     contains

! ---------------------------------------------------------
! Open a trajectory file for reading
! ---------------------------------------------------------
     subroutine ropen_tra(fid,filename,ntra,ntim,ncol,time,vars,mode)

        implicit none

        ! declaration of subroutine parameters
        integer       :: fid
        character*80  :: filename
        integer       :: mode
        integer       :: ntra,ntim,ncol
        integer       :: time(6)
        character*80  :: vars(ncol)

        ! auxiliary variables
        integer       :: vardim(4)
        real          :: varmin(4),varmax(4),stag(4)
        real          :: mdv
        character*80  :: cfn
        integer       :: ierr
        integer       :: i
        integer       :: nvars

        ! openf file
        if ( (mode.eq.1).or.(mode.eq.2) ) then
           fid = 10
           open(fid,file=filename,status='old')
        else if (mode.eq.3) then
           open(fid,file = filename,form = 'unformatted')
        else if (mode.eq.4) then
           call cdfopn(filename,fid,ierr)
        else if (mode.eq.5) then
           print*,' ERROR: Reading KML not supported'
           stop
        else if (mode.eq.6) then
           fid = 10
           open (fid,file=filename,status='old')
        end if

        !print*, 'check consistence',mode
        ! read header information
        call read_hea(fid,time,vars,ntra,ntim,ncol,mode)

     end subroutine ropen_tra

! ---------------------------------------------------------
! Open a trajectory file for writing
! ---------------------------------------------------------
     subroutine wopen_tra(fid,filename,ntra,ntim,ncol,time,vars,mode)
        
        implicit none

        ! declaration of subroutine parameter
        integer      :: fid
        character*80 :: filename
        integer      :: mode
        integer      :: ntra,ntim,ncol
        integer      :: time(6)
        character*80 :: vars(ncol)

        ! auxiliary variables
        integer      :: vardim(4)
        real         :: varmin(4),varmax(4),stag(4)
        real         :: mdv
        character*80 :: cfn
        integer      :: ierr
        integer      :: i
        character*80 :: varname
        real         :: rtime(6)

        ! Open file
        if ( (mode.eq.1).or.(mode.eq.2).or.(mode.eq.5).or.(mode.eq.6) ) then
           fid = 1
           open(fid,file=filename)
        else if (mode.eq.3) then
           open(fid,file=filename,form='unformatted')
        else if (mode.eq.4) then
           vardim(1) = ntra
           vardim(2) = 1
           vardim(3) = 1
           vardim(4) = 1
           cfn       = trim(filename)//'_cst'
           mdv       =-999.99999
           call crecdf(filename,fid,varmin,varmax,3,cfn,ierr)
        end if

        ! Write header information
        call write_hea(fid,time,vars,ntra,ntim,ncol,mode)
        
     end subroutine wopen_tra

! ---------------------------------------------------------
! Read a trajectory
! ---------------------------------------------------------
     subroutine read_tra(fid,tra,ntra,ntim,ncol,mode)
       

        implicit none
        
        ! declaration of subroutine parameters
        integer     :: fid
        integer     :: ntim
        integer     :: ncol,ntra
        real        :: tra(ntra,ntim,ncol)
        integer     :: mode

        ! auxiliary variables
        integer     :: i,j,n
        real        :: arr(ntra)
        integer     :: ntimes
        real        :: times(100000)
        integer     :: ierr
        character*80:: vars(ncol+2)
        integer     :: nvars

        ! read ascii mode, sorted by trajectory (mode = 1)
        if (mode.eq.1) then
           read(fid,*,end=100)
           do n = 1, ntra
              do i = 1, ntim
                read(fid,*,end=100) (tra(n,i,j),j=1,ncol)
              end do
           end do

        ! read ascii mode, sorted by time (mode = 2)
        else if (mode.eq.2) then
           read(fid,*,end=100)
           do i = 1,ntim
              do n = 1, ntra
                read(fid,*,end=100) (tra(n,i,j),j=1,ncol)
              end do
           end do

        ! read fortran mode (mode=3)
        else if (mode.eq.3) then
           read(fid) tra

        ! read IVE netcdf mode (mode=4)
        else if (mode.eq.4) then
           call gettimes(fid,times,ntimes,ierr)
           call getvars(fid,nvars,vars,ierr)
           do i = 1, ntim
           do j = 1, ncol
              if (j.eq.1) then
                 do n = 1,ntra
                      tra(n,i,1) = times(i)
                 enddo
              else
                 call getdat(fid,vars(j),times(i),0,arr,ierr)
                   do n=1,ntra
                      tra(n,i,j)=arr(n)
                   enddo
              end if
           end do
           end do

        ! read ascii trajectory with pressure as real
        else if (mode.eq.6) then
           read(fid,*,end=100)
           do n = 1,ntra
           do i = 1,ntim
                read(fid,*,end=110) (tra(n,i,j),j=1,ncol)
           end do
           end do

        end if

        return

        ! end of file has been reached: set negative [fid]
100     fid = -fid
        return

        ! error: incomplete trajectory
110     print*,'[read_tra]: Incomplete trajectory... Stop'
        stop

     end subroutine read_tra

! ---------------------------------------------------------
! Write a trajectory
! ---------------------------------------------------------

     subroutine write_tra(fid,tra,ntra,ntim,ncol,mode)

        implicit none
        
        ! declaration of subroutine parameters
        integer      :: fid
        integer      :: ntim
        integer      :: ntra,ncol
        real         :: tra(ntra,ntim,ncol)
        integer      :: mode

        ! auxiliary variables
        integer      :: i,j,n
        real         :: arr(ntra)
        integer      :: ierr
        real         :: time
        character*80 :: vars(ncol+2)
        integer      :: nvars
        character*20 :: lonstr,latstr,levstr
        character*80 :: outstr
        real         :: ref_z(3000),ref_p(3000),ref_t(3000)
        real         :: lev
        character*80 :: path

        ! write ascii mode, sorted by trajectory (mode=1)
        if (mode.eq.1) then
           do n = 1, ntra
              write(fid,*)
              do i = 1, ntim
                 
                ! avoid ugly *s or missing space in output
                do j = 1, ncol
                   if (abs(tra(n,i,j)).gt.9999.) then
                      print*,'Format problem : ',tra(n,i,j),' -> -999.99'
                      tra(n,i,j) = -999.99
                   end if
                end do

                write(fid,'(f7.2,f9.2,f8.2,f9.1,100(2X,e10.3))')       &
        &       (tra(n,i,j),j=1,3),      & ! time, lon, lat
        &       tra(n,i,4),               & ! pressure
        &       (tra(n,i,j),j=5,ncol)      
              end do
           end do

        ! write ascii mode, sorted by time
        else if (mode.eq.2) then
          do i = 1, ntim
             write(fid,*)
             do n = 1, ntra

                ! avoid ugly *s or missing space in output
                do j = 1, ncol
                   if (abs(tra(n,i,j)).gt.9999.) then
                      print*,'Format problem : ',tra(n,i,j),' -> -999.99'
                      tra(n,i,j) = -999.99
                   end if
                end do

                write(fid,'(f7.2,f9.2,f8.2,i6,100f10.3)')       &
        &       (tra(n,i,j),j=1,3),      & ! time,lon,lat
        &       tra(n,i,4),              & ! pressure
        &       (tra(n,i,j),j=5,ncol)     ! other fields
             end do
          end do

        ! write unformatted fortran mode (mode=3)
        else if (mode.eq.3) then
          write(fid) tra
        
        ! write netcdf mode
        else if (mode.eq.4) then
          
          call getvars(fid,nvars,vars,ierr)

          do i = 1,ntim
             time = tra(1,i,1)
             print*, time
             do j = 1,ncol
                do n = 1, ntra
                  arr(n) = tra(n,i,j)  
                end do
                call putdat(fid,vars(j),time,0,arr,ierr)
             end do
          end do

        ! write KML mode (mode=5)
        else if (mode.eq.5) then

          path = '../lib/'
          open(fid+1, file=trim(path)//'reformat.refprof',status='old')
              
              do n = 1,6
                 read(fid+1,*)
              end do

              do n = 1,3000
                 read(fid+1,*) ref_z(n), ref_t(n), ref_p(n)
                 ref_p(n) = ref_p(n) * 0.01
              end do

          close(fid+1)

          do n = 1, ntra
              write(fid,"(A)") '<Placemark>'
              write(fid,"(A)") '<name>Absolute Extruded</name>'
              write(fid,"(A)") '<styleUrl>#yellowkLineGreenPoly</styleUrl>'
              write(fid,"(A)") '<LineString>'
              write(fid,"(A)") '<extrude>1</extrude>'
              write(fid,"(A)") '<tessellate>1</tessellate>'
              write(fid,"(A)") '<altitudeMode>absolute</altitudeMode>'
              write(fid,"(A)") '<coordinates>'

              do i = 1,ntim
                 write(lonstr,*) tra(n,i,2)
                 write(latstr,*) tra(n,i,3)

                 call binary(lev,tra(n,i,4),ref_z,ref_p)
                 write(levstr,*) lev

                 outstr = trim(adjustl(lonstr))//','//trim(adjustl(latstr))//','//&
        &                 trim(adjustl(levstr))

                 write(fid,"(A)") outstr
              end do

              write(fid,"(A)") '</coordinates>'
              write(fid,"(A)") '</LineString>'
              write(fid,"(A)") '</Placemark>'
          end do

        ! write ASCII trajectory with pressure as real
        else if (mode .eq. 6) then
           
           do n = 1, ntra
                write (fid,*) 
                do i = 1, ntim

                   ! Avoid ugly *s or missing space in output
                   do j = 5, ncol
                        if ( abs(tra(n,i,j)).gt.9999. ) then
                           print*,' Format problem : ', tra(n,i,j),' -> -999.99'
                           tra(n,i,j) = -999.99
                        end if 
                   end do

                   write(fid,'(f7.2,f9.2,f8.2,f8.2,100f10.3)')          &
        &                (tra(n,i,j),j=1,3),                            & ! time,lon,lat      
        &                tra(n,i,4),                                    & ! P
        &                (tra(n,i,j),j=5,ncol)                            ! fields
                end do    
           end do
        end if
          
     end subroutine write_tra

! ---------------------------------------------------------
! Read header from trajectory file
! ---------------------------------------------------------
     subroutine read_hea(fid,time,vars,ntra,ntim,ncol,mode)

        implicit none   
        
        ! declaration of subroutine variables
        integer         :: fid
        integer         :: time(6)
        integer         :: ntra,ntim,ncol
        character*80    :: vars(ncol)
        integer         :: mode

        ! auxiliary variables 
        integer         :: i    
        character       :: ch(500)
        character*500   :: str
        integer         :: ich(500)
        integer         :: isstr,ileft,iright
        character*80    :: varname
        real            :: rtime(6)
        integer         :: ierr
        integer         :: nvars
        character*15    :: str1
        character       :: str2
        character*13    :: str3
        character*4     :: str4
        character*80    :: linestr
        integer         :: itmp1,itmp2  

        ! Read ASCII format (mode=1,2,6)
        if ((mode.eq.1).or.(mode.eq.2).or.(mode.eq.6)) then

           ! Read the time specification (old and new format)
           read (fid,'(a80)') linestr
           if ( trim(linestr(1:15)).eq.'Reference date' ) then
                !print*,'check heading'
                read(linestr,'(a15,i4,i2,i2,a1,i2,i2,a13,i8,a4)')       &
        &            str1,time(1),time(2),time(3),str2,time(4),time(5),      &
        &            str3,time(6),str4
           else if ( linestr(1:11).eq.'time period' ) then
                read(linestr,'(a12,i4,i2,i2,a1,i2,a4,i6,a1,i2,a2)')     &
        &            str1,time(1),time(2),time(3),str2,time(4),str3,    &
        &            itmp1,str3,itmp2,str4
                time(5) = 0
                time(6) = itmp1 * 60 + itmp2

           end if

           ! Skip the empty line and read field names
           read(fid,*)
           read(fid,'(a500)',end=100) str
           do i = 1,500
                ch(i) = str(i:i)
           end do

           ! Split the input string
           isstr = 0
           nvars = 0

           do i = 1, 500
                if ( (isstr.eq.0) .and. (ch(i).ne.' ') ) then
                     isstr = 1
                     ileft = i
                else if ( (isstr.eq.1) .and. (ch(i).eq.' ') ) then
                     iright = i - 1
                     isstr = 0
                     nvars = nvars + 1
                     vars(nvars) = str(ileft:iright)
                end if
           end do

           ! Skip the emply line
           read(fid,*,end=100)

        ! Read fortran unformatted mode
        else if (mode.eq.3) then
            
           read (fid) ntra,ntim,ncol
           read (fid) time
           read (fid) vars

        ! Read IVE NetCDF mode
        else if (mode.eq.4) then

           call getvars(fid,nvars,vars,ierr)
           varname = 'BASEDATE'
           call getdat (fid,varname,0.,0,rtime,ierr)
           do i = 1, 6
                time(i) = nint(rtime(i))
           end do

        end if

        return
 
        ! ----------------- End of file -----------------
100     fid = -fid
        return

        ! ---------------- Exceptional handling ---------
110     print*,'<read hea>: unexpected time format... Exitting...'
        stop

     end subroutine read_hea

     subroutine write_hea(fid,time,vars,ntra,ntim,ncol,mode)

        implicit none

        ! declaration of subroutine variables
        integer         :: fid
        integer         :: time(6)
        integer         :: ntra,ntim,ncol
        character*80    :: vars(ncol)
        integer         :: mode

        ! auxiliary variables
        integer         :: i
        character*500   :: str
        character*4     :: str1
        character*2     :: str2,str3,str4,str5,str6
        integer         :: vardim(4)
        real            :: varmin(4),varmax(4),stag(4)
        real            :: mdv
        integer         :: ierr
        character*80    :: varname
        real            :: rtime(6)
        integer         :: nvars
        
        ! Write ASCII Format (mode = 1, 2)
        if ( (mode.eq.1) .or. (mode.eq.2) ) then
             
             ! get the string for output
             write(str1,'(i4)') time(1)
             write(str2,'(i2)') time(2)
             write(str3,'(i2)') time(3)
             write(str4,'(i2)') time(4)
             write(str5,'(i2)') time(5)
             if ( time(2).eq.0 ) str2(1:1) = '0'
             if ( time(3).eq.0 ) str3(1:1) = '0'
             if ( time(4).eq.0 ) str4(1:1) = '0'
             if ( time(5).eq.0 ) str5(1:1) = '0'
             if ( time(2).lt.10 ) str2(1:1) = '0'
             if ( time(3).lt.10 ) str3(1:1) = '0'
             if ( time(4).lt.10 ) str4(1:1) = '0'
             if ( time(5).lt.10 ) str5(1:1) = '0'

             ! write the time specification
             write(fid,'(a15,a4,a2,a2,a1,a2,a2,a13,i8,a4)') 'Reference date ',&
        &         str1,str2,str3,'_',str4,str5,' / Time range',time(6),' min'
             write(fid,*)

             ! write variable names
             str = ''
             do i = 1, ncol
                varname = vars(i)
                vars(i) = varname(1:9)
                str     = trim(str)//vars(i)
             end do
             write(fid,'(a6,a9,a8,a6,100a10)') (trim(vars(i)),i=1,ncol) 
             write(fid,'(a6,a9,a8,a6,100a10)')                          &
        &              '------','---------','--------','------',        &
        &              ('----------',i=5,ncol)

        !     Write fortran mode (mode=3)
        else if (mode.eq.3) then
         
             write(fid) ntra,ntim,ncol
             write(fid) time
             write(fid) vars

        !     Write IVE netcdf format (mode=4)
        else if (mode.eq.4) then
        
              vardim(1) = ntra
              vardim(2) = 1
              vardim(3) = 1
              vardim(4) = 1
              mdv       = -999.98999

              do i=2,ncol
                 call putdef(fid,vars(i),4,mdv,vardim,varmin,varmax,stag,ierr)
              enddo

              varname='BASEDATE'
              vardim(1)=6
              call putdef(fid,varname,4,mdv,vardim,varmin,varmax,stag,ierr)
              do i=1,6
                !print*,time(i)
                rtime(i)=real(time(i))
              enddo
              call putdat(fid,varname,0.,0,rtime,ierr)

        !     Write KML format (mode=5)
        else if (mode.eq.5) then

              write(fid,"(A)") '<?xml version="1.0" encoding="UTF-8"?>'
              write(fid,"(A)") '<kml xmlns="http://www.opengis.net/kml/2.2">'
              write(fid,"(A)") '<Document>'
              write(fid,"(A)") '<name>Paths</name>'
              write(fid,"(A)") '<Style id="yellowLineGreenPoly">'
              write(fid,"(A)") '<LineStyle>'
!      write(fid,*) '<color>7f00ffff</color>'    ! Yellow
              write(fid,"(A)") '<color>500A0A0A</color>'     ! Black
              write(fid,"(A)") '<width>4</width>'
              write(fid,"(A)") '</LineStyle>'
              write(fid,"(A)") '<PolyStyle>'
              write(fid,"(A)") '<color>7f00ff00</color>'
              write(fid,"(A)") '</PolyStyle>'
              write(fid,"(A)") '</Style>'

        !Write header for ASCII mode iwth real pressure      
        else if ( mode.eq.6 ) then

              ! Get the strings for output
              write(str1,'(i4)') time(1)
              write(str2,'(i2)') time(2)
              write(str3,'(i2)') time(3)
              write(str4,'(i2)') time(4)
              write(str5,'(i2)') time(5)
              if (time(2).eq. 0) str2(1:1)='0'
              if (time(3).eq. 0) str3(1:1)='0'
              if (time(4).eq. 0) str4(1:1)='0'
              if (time(5).eq. 0) str5(1:1)='0'
              if (time(2).lt.10) str2(1:1)='0'
              if (time(3).lt.10) str3(1:1)='0'
              if (time(4).lt.10) str4(1:1)='0'
              if (time(5).lt.10) str5(1:1)='0'

              ! Write the time specification
              write(fid,'(a15,a4,a2,a2,a1,a2,a2,a13,i8,a4)') 'Reference date ',&
        &           str1,str2,str3,'_',str4,str5,' / Time range',time(6), ' min'
              write(fid,*)

              ! Write variable names
              str=''
              do i=1,ncol
                varname = vars(i)
                vars(i) = varname(1:9)
                str     = trim(str)//trim(vars(i))
              enddo
              write(fid,'(a6,a9,a8,a8,100a10)') (trim(vars(i)),i=1,ncol)
              write(fid,'(a6,a9,a8,a8,100a10)')                         &
        &              '------','---------','--------','--------',      &
        &           ('----------',i=5,ncol)

        endif

     end subroutine write_hea

! ---------------------------------------------------------
! Determine the mode of a trajectory file
! ---------------------------------------------------------
     subroutine mode_tra(mode,filename)
        
        implicit none
        ! declaration of subroutine parameters
        integer      :: mode
        character*80 :: filename

        ! auxiliary variables
        integer      :: len
        character    :: char0,char1,char2,char3,char4

        ! set mode
        mode = -1

        len = len_trim(filename)
        
        ! mode specified by number
        char0 = filename((len-1):(len-1))
        char1 = filename(len:len)

        if ( (char0.eq.'.').and.(char1.eq.'1') ) mode = 1
        if ( (char0.eq.'.').and.(char1.eq.'2') ) mode = 2
        if ( (char0.eq.'.').and.(char1.eq.'3') ) mode = 3
        if ( (char0.eq.'.').and.(char1.eq.'4') ) mode = 4
        if ( (char0.eq.'.').and.(char1.eq.'5') ) mode = 5
        if ( (char0.eq.'.').and.(char1.eq.'6') ) mode = 6

        if ( mode.gt.0 ) return

        ! mode specified by appendix
        char0 = filename((len-3):(len-3))
        char1 = filename((len-2):(len-2))
        char2 = filename((len-1):(len-1))
        char3 = filename(len:len)

        if ( (char1.eq.'.').and.(char2.eq.'l').and.(char3.eq.'s') ) mode = 1
        if ( (char1.eq.'.').and.(char2.eq.'t').and.(char3.eq.'i') ) mode = 2
        if ( (char1.eq.'.').and.(char2.eq.'d').and.(char3.eq.'u') ) mode = 3
        if ( (char1.eq.'.').and.(char2.eq.'n').and.(char3.eq.'c') ) mode = 4

        if ( (char0.eq.'.').and.(char1.eq.'k').and.(char2.eq.'l').and.&
        &    (char3.eq.'m') ) mode = 5

        if ( (char1.eq.'.').and.(char2.eq.'r').and.(char3.eq.'p') ) mode = 6

        return
     end subroutine mode_tra

!     ----------------------------------------------------------------
!     Close a trajectory file
!     ----------------------------------------------------------------

     subroutine close_tra(fid,mode)

        implicit none

        ! Declaration of subroutine parameters
        integer      fid
        integer      mode

        ! Auxiliary variables
        integer      ierr

        !  Close file
        if (mode.eq.1) then
           close(abs(fid))

        else if (mode.eq.2) then
           close(abs(fid))

        else if (mode.eq.3) then
           close(fid)

        else if (mode.eq.4) then
           call clscdf(fid,ierr)

        else if (mode.eq.5) then
           write(fid,"(A)") '</Document>'
           write(fid,"(A)") '</kml>'
           close(abs(fid))

        else if (mode.eq.6) then
           close(abs(fid))

        endif

     end subroutine close_tra

!     ----------------------------------------------------------------
!     Get dimension of a trajectory file
!     ----------------------------------------------------------------

     subroutine info_tra(filename,ntra,ntim,ncol,mode)

        implicit none

        !  Declaration of subroutine parameters
        integer      fid
        character*80 filename
        integer      mode
        integer      ntra,ntim,ncol

        ! Auxiliary variables
        integer       vardim(4)
        real          varmin(4),varmax(4),stag(4)
        real          mdv
        character*80  cfn
        integer       ierr
        integer       i,ndim
        integer       nvars
        integer       ntimes
        real          times(100)
        character*500 str
        integer       nline0,nline1,nline2
        integer       isstr,isok
        character     ch
        character*80  vars(200)
        integer       ios

        !  Open file
        if (mode.eq.1) then

            fid=10
            open(fid,file=filename)

        elseif (mode.eq.2) then
         
            fid=10
            open(fid,file=filename)

        elseif (mode.eq.3) then
            
            fid=10
            open(fid,file=filename,form='unformatted')

        elseif (mode.eq.4) then

            call cdfopn(filename,fid,ierr)

        elseif (mode.eq.6) then
         
            fid=10
         open(fid,file=filename)
      endif

      !  Get dimension information

      if ( (mode.eq.1).or.(mode.eq.2).or.(mode.eq.6) ) then
         read(fid,*)
         read(fid,*)
         read(fid,'(a500)') str
         read(fid,*)

         ! Get the number of columns
         isstr=0
         ncol =0
         do i=1,500
            ch = str(i:i)
            if ( (isstr.eq.0).and.(ch.ne.' ') ) then
               isstr=1
            elseif ( (isstr.eq.1).and.(ch.eq.' ') ) then
               isstr=0
               ncol=ncol+1
            endif
         enddo


         ! Init the dimensions for empty trajectory files
         ntra = 0
         ntim = 0

         ! Get the first data block
         nline0  = 5
         nline1  = 5
         read(fid,*,end=140,iostat=ios)
 100     read(fid,'(a500)',end=110,iostat=ios) str
         if (str.ne.'') then
            nline1 = nline1 + 1
            goto 100
         endif
 110     continue

         ! Get the total numbers of lines in the data block
         nline2 = nline1
         if ( ios.eq.0 ) then
 120        read(fid,*,end=130)
            nline2 = nline2 + 1
            goto 120
 130        nline2 = nline2 + 1
         endif

         ! Set the dimensions
         if (mode.eq.1) then

            ntim = nline1 - nline0
            ntra = (nline2-nline0+1)/(ntim+1)

         else

            ntra = nline1 - nline0
            ntim = (nline2-nline0+1)/(ntra+1)

         endif

      elseif (mode.eq.3) then

         read(fid) ntra,ntim,ncol

      elseif (mode.eq.4) then

         call gettimes(fid,times,ntimes,ierr)
         call getvars(fid,nvars,vars,ierr)
         call getdef(fid,trim(vars(2)),ndim,mdv,vardim,varmin,varmax,stag,ierr)
         ntra = vardim(1)
         ntim = ntimes
         ncol = nvars-1

      endif

      ! Close file
 140  continue
      if (mode.eq.1) then
         close(fid)
      elseif (mode.eq.2) then
         close(fid)
      elseif (mode.eq.3) then
         close(fid)
      elseif (mode.eq.4) then
         call clscdf(fid,ierr)
      end if

    end subroutine info_tra

!     ----------------------------------------------------------------
!     Binary search algorithm
!     ----------------------------------------------------------------

      subroutine binary (z,p,ref_z,ref_p)

      implicit none

!     Declaration of subroutine parameters
      real   z
      real   p
      real   ref_z(3000)
      real   ref_p(3000)

!     Auxiliary variables
      integer i0,i1,im

!     Binary search
      i0 = 1
      i1 = 3000
 100  continue
        im = (i0 + i1) / 2
        if ( p.lt.ref_p(im) ) then
           i0 = im
        else
           i1 = im
        endif
      if ( (i1-i0).gt.1 ) goto 100

!     Linear interpolation in between
      z = ref_z(i0) + ( p - ref_p(i0) ) / ( ref_p(i1) - ref_p(i0) ) * ( ref_z(i1) - ref_z(i0) )

      end
end module iotra
