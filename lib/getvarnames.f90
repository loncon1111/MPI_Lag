program getvarnames

!  ***************************************************************
!  * Get list of variables on netCDF file                        *
!  ***************************************************************

        use netcdf

        implicit none

        integer :: nvars,ierr
        integer :: cdfid, i
        character*80:: cdfname
        character,dimension(200):: vnam

        integer :: iargc
        character*80:: arg

        ! check for sufficient requested arguments
        if (iargc() .ne. 1) then
           print*, 'USAGE: getvars NetCDF-filename'
           call exit(1)
        endif

        ! read and transform input

   
end
