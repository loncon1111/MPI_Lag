program test_ichar
        use iotra

        integer i
        character*80 name
        integer,parameter:: ntra=10,ntim=2,ncol=2
        real :: tra
        integer::mode

        allocate(tra(ntra,ntim,ncol))
        mode = 1

        i =16
        write(name,'(i4.4)') i
        i = 1
        do while ( i .lt. 5 ) 
           print*, i
           i = i+1
        end do
        print*, name

        
        call read_tra(10,tra,ntra,ntim,ncol,mode)
        print*,'do something'
end
