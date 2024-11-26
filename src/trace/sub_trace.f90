! ------------------------------------------------------------------
! Add a variable to the list if not yet included in this list
! ------------------------------------------------------------------

subroutine add2list(varname,list,nlist)

    implicit none

    ! Arguments declaration
    character*80:: varname  
    character*80:: list(200)
    integer     :: nlist

    ! Auxiliary variables
    integer     :: i,j
    integer     :: flag

    ! Expand the list, if necessary

    flag = 0
    do i = 1, nlist
        if ( list(i) .eq. varname ) then
             flag = 1
        end if
    end do

    if ( flag .eq. 0 ) then
        nlist       = nlist + 1
        list(nlist) = varname
    end if

    ! Check for too large number of fields
    if ( nlist .gt. 200 ) then
        print*,' ERROR: too many additional fields for tracing'
        stop
    end if


end subroutine add2list

! ------------------------------------------------------------------
! Get the index of a variable in the list
! ------------------------------------------------------------------

subroutine list2ind(ind,varname,list,fok,nlist)

    implicit none

    ! Argument declaration
    character*80:: varname
    integer     :: ind
    character*80:: list(200)
    integer     :: fok(200)
    integer     :: nlist

    ! Auxiliary variable
    integer     :: i,j
    integer     :: flag

    ! Get the index - error message if not found
    do i = 1, nlist
        if ( list(i) .eq. varname ) then
             ind   = i
             go to 100
        end if
    end do

    if ( ind .eq. 0 ) then
        print*
        print*,' ERROR: cannot find ',trim(varname),' in list ...'
        do i = 1, nlist
            print*, i, trim(list(i))
        end do
        print*
        stop
    end if

    ! Exit point
100 continue

    !print*, list(ind), varname, ind
    ! Check whether the field/column is ready
    if ( fok(ind).eq.0 ) then
        print*
        print*,' ERROR: unresolved dependence : ',trim(list(ind))
        print*
        stop
    endif

end subroutine list2ind

subroutine splitvar(tvar,shift_val,shift_dir,shift_rel)

    implicit none

    ! Arguments declaration
    character*80:: tvar
    character*80:: shift_rel
    character*80:: shift_dir
    real        :: shift_val

    ! Auxiliary variable
    integer     :: i,j
    integer     :: icolon,irelator,inumber
    character*80:: name
    character   :: ch
    integer     :: isabsval

    ! Save variable name
    name        = tvar
    shift_rel   = 'nil'
    shift_dir   = 'nil'

    ! Search for colon
    icolon      = 0
    do i = 1, 80        ! This is fuckin' stupid,just use icolon=index(name,':')
        if ( (name(i:i).eq.':').and.(icolon.ne.0) ) go to 100
        if ( (name(i:i).eq.':').and.(icolon.eq.0) ) icolon = i
    end do

    ! if there is a icolon, split the variable name
    if ( icolon .ne. 0 ) then
        
        tvar = name(1:(icolon-1))
        
        ! Get the index for number
        do i = icolon+1,80
           ch = name(i:i)
           if ( ( ch.ne.'0' ).and. ( ch.ne.'1' ).and.( ch.ne.'2' ).and. &
                ( ch.ne.'3' ).and. ( ch.ne.'4' ).and.( ch.ne.'5' ).and. &
                ( ch.ne.'6' ).and. ( ch.ne.'7' ).and.( ch.ne.'8' ).and. &
                ( ch.ne.'9' ).and. ( ch.ne.'+' ).and.( ch.ne.'-' ).and. &
                ( ch.ne.'.' ).and. ( ch.ne.' ' )  ) then
                inumber = i
                exit
           endif
        enddo

        ! If the variable name is e.g. PMIN:UMF>0, the variable to be read
        ! is UMF, the value is 0, and the direction is 'PMIN'
        if ( (tvar.eq.'PMIN') .or. (tvar.eq.'PMAX') ) then
           shift_dir = tvar
           irelator  = 0
           do i = icolon + 1,80
                ch = name(i:i)
                if ( (ch.eq.'>') .or. (ch.eq.'<') ) then
                   irelator = i
                end if
           end do
           if ( irelator .eq. 0 ) then
                print*,' ERROR: donot know how to interpret ...',trim(name)
                stop
           end if
           tvar = name(icolon+1:irelator-1)
           read (name(irelator+1:80),*) shift_val
           shift_rel = name(irelator:irelator)

           go to 90        
        end if


        ! Get the number
        read (name(icolon+1:inumber-1),*) shift_val

        ! Decide whether it is a shift relatieve to trajectory or absolute value
        ! If the number starts with + or -, it is relative to the trajectory
        isabsval = 1
        do i = icolon+1, inumber-1
             ch = name(i:i)
             if ( (ch.eq.'+') .or. (ch.eq.'-') ) isabsval = 0
        end do

        ! Get the unit/shift axis
        shift_dir = name(inumber:80)
        if ( isabsval .eq. 1 ) shift_dir = trim(shift_dir)//'(ABS)'

    else

        shift_dir = 'nil'
        shift_val = 0.

    end if
90  return
    ! Error handlint
100 continue
    print*,' ERROR: cannot split variable name ',trim(tvar)
    stop
    
end subroutine splitvar
