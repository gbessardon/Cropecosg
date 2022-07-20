 PROGRAM MAIN
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    IMPLICIT NONE
    CHARACTER(LEN=512):: COVERFILE !ECOSG cover file
    CHARACTER(LEN=512):: INFILE  ! file to open
    CHARACTER(LEN=512) :: OUTFILE ! file where the 0 values are applied
    CHARACTER(LEN=512) :: COMPRESSED_FILE ! file to write
    INTEGER :: NCOL, NLIN
    
    call read_some_parameters('Namelist_compress.nml', COVERFILE, INFILE,&
                              OUTFILE, COMPRESSED_FILE, NCOL, NLIN)
                              

    print *, NCOL
    print *, NLIN
    
    print *, 'Compress'
    call MAKE_COMPRESS_FILE(OUTFILE,COMPRESSED_FILE,NCOL,NLIN)
CONTAINS
    subroutine read_some_parameters(file_path, COVERFILE, INFILE,&
                              OUTFILE, COMPRESSED_FILE, NCOL, NLIN)
        !! Read some parmeters,  Here we use a namelist 
        !! but if you were to change the storage format (TOML,or home-made), 
        !! this signature would not change

        character(len=*),  intent(in)  :: file_path
        character(len=512), intent(out) :: COVERFILE
        character(len=512), intent(out) :: INFILE
        character(len=512), intent(out) :: OUTFILE
        character(len=512), intent(out) :: COMPRESSED_FILE
        integer, intent(out)           :: NCOL, NLIN
        
        integer                        :: file_unit, iostat

        ! Namelist definition===============================
        namelist /INPUTS/ &
            COVERFILE, &
            INFILE, &
            OUTFILE, &
            COMPRESSED_FILE, &
            NCOL, &
            NLIN
        COVERFILE = "ecosg_final.dir"
        INFILE = "LAI_uncompressed.dir"
        OUTFILE = "LAI_withnewvalues.dir"
        COMPRESSED_FILE = "LAI_withnewvaluescompressed.dir"
        NCOL = 43200
        NLIN = 16800
        ! Namelist definition===============================

        call open_inputfile(file_path, file_unit, iostat)
        if (iostat /= 0) then
            !! write here what to do if opening failed"
            return
        end if

        read (nml=INPUTS, iostat=iostat, unit=file_unit)
        call close_inputfile(file_path, file_unit, iostat)
        if (iostat /= 0) then
            !! write here what to do if reading failed"
            return
        end if
    end subroutine read_some_parameters
    
        !! Namelist helpers

    subroutine open_inputfile(file_path, file_unit, iostat)
        !! Check whether file exists, with consitent error message
        !! return the file unit
        character(len=*),  intent(in)  :: file_path
        integer,  intent(out) :: file_unit, iostat

        inquire (file=file_path, iostat=iostat)
        if (iostat /= 0) then
            write (stderr, '(3a)') 'Error: file "', trim(file_path), '" not found!'
        end if
        open (action='read', file=file_path, iostat=iostat, newunit=file_unit)
    end subroutine open_inputfile

    subroutine close_inputfile(file_path, file_unit, iostat)
        !! Check the reading was OK
        !! return error line IF not
        !! close the unit
        character(len=*),  intent(in)  :: file_path
        character(len=1000) :: line
        integer,  intent(in) :: file_unit, iostat

        if (iostat /= 0) then
            write (stderr, '(2a)') 'Error reading file :"', trim(file_path)
            write (stderr, '(a, i0)') 'iostat was:"', iostat
            backspace(file_unit)
            read(file_unit,fmt='(A)') line
            write(stderr,'(A)') &
                'Invalid line : '//trim(line)
        end if
        close (file_unit)   
    end subroutine close_inputfile

        
    SUBROUTINE MAKE_COMPRESS_FILE(INFILE,OUTFILE,NCOL,NLIN)
        CHARACTER(LEN=512), INTENT(IN) :: INFILE    ! file to open
        CHARACTER(LEN=512), INTENT(IN) :: OUTFILE   ! file to write
        INTEGER, INTENT(IN)        :: NCOL, NLIN
        integer*2,dimension(NCOL*3) :: lread
        integer*2,dimension(NCOL*3) :: lwrite
        integer*4 :: j, i, k, icnt0
        integer*2 :: icpt
        integer*4 :: irec
        integer*4,dimension(NLIN*3) :: icpt2
        integer*2 :: inb, ire
        integer*4 :: ilong
        integer*8 :: ipos

        OPEN(11,file=INFILE,form='unformatted',access='direct',recl=NCOL*2*3)
        OPEN(12,file=OUTFILE,form='unformatted',access='stream')
        
        irec = 0
        ipos = NLIN*3*4+1
        ! boucle sur les lignes
        do j=1,NLIN*3
          !print*,j
          ! lecture du lai
          
          read(11,rec=j) lread
          ! tableau qui contient la ligne de lai à écrire
          lwrite(:) = 0

          icpt2(j) = 0

          i = 1

          ! boucle sur les colonnes
          do 
            ! si on a dépassé la dernière colonne, on sort de la boucle
            if (i>NCOL*3) exit
            
            ! si la valeur est valide
            if (lread(i)/=1) then
            
              !print*,'cas1'
              ! on la met dans lwrite à l'indice icpt
              icpt2(j) = icpt2(j) + 1
             lwrite(icpt2(j)) = lread(i)
              ! on incrémente i
              i = i+1
            else
                              !  print*,'cas2'
              ! nombre de 0 dans la ligne à partir de l'indice i
              do k = i,size(lread)
                if (lread(k)/=1) exit
              end do
              !print*,'ki ',k,i
              icnt0 = k-i
              if (icnt0==-1) icnt0 = NCOL*3-i+1
              if (icnt0>NCOL) then

                ilong = NCOL
                inb = icnt0/NCOL
                ire = mod(icnt0,NCOL)
                do k=1,inb
                  icpt2(j) = icpt2(j) + 1
                  lwrite(icpt2(j)) = 4000+ilong
                end do
                if (ire/=0) then
                  icpt2(j) = icpt2(j) + 1
                  lwrite(icpt2(j)) = 4000+ire
                endif

              else

                ! on met le nombre de zér0s à la suite dans lwrite indice icpt
                icpt2(j) = icpt2(j) + 1
                lwrite(icpt2(j)) = 4000+icnt0

              endif
              
              ! on passe à la colonne après la suite de z0
              i = i+icnt0

            endif

          end do
          
          write(12,pos=ipos) lwrite(1:icpt2(j))
          ipos = ipos + icpt2(j)*2

        end do
        
        write(12,pos=1) icpt2
        close(11)
        close(12)
        print*,j  
        print*,i
      END SUBROUTINE MAKE_COMPRESS_FILE
        
        
END PROGRAM MAIN