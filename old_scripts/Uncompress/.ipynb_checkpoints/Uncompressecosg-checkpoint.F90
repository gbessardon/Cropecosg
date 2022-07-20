
PROGRAM MAIN
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    IMPLICIT NONE
    CHARACTER(LEN=512):: INFILE   ! file to open
    CHARACTER(LEN=512) :: OUTFILE ! file to write
    INTEGER :: NCOL, NLIN
    
        ! Read from file.
    call read_some_parameters('namelist.nml', INFILE, OUTFILE, NCOL, NLIN)
    
    CALL UNCOMPRESS(INFILE,OUTFILE,NCOL,NLIN)

CONTAINS
    subroutine read_some_parameters(file_path, INFILE, OUTFILE, NCOL, NLIN)
        !! Read some parmeters,  Here we use a namelist 
        !! but if you were to change the storage format (TOML,or home-made), 
        !! this signature would not change

        character(len=*),  intent(in)  :: file_path
        character(len=512), intent(out) :: INFILE
        character(len=512), intent(out) :: OUTFILE
        integer, intent(out)           :: NCOL, NLIN
        
        integer                        :: file_unit, iostat

        ! Namelist definition===============================
        namelist /INPUTS/ &
            INFILE, &
            OUTFILE, &
            NCOL, &
            NLIN
        INFILE = "undefined"
        OUTFILE = "undefined"
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



    SUBROUTINE UNCOMPRESS(INFILE,OUTFILE,ncol,nlin)
        CHARACTER(LEN=512), INTENT(IN) :: INFILE    ! file to open
        CHARACTER(LEN=512), INTENT(IN) :: OUTFILE   ! file to write
        INTEGER, INTENT(IN)        :: ncol, nlin
        INTEGER*2,DIMENSION(ncol*3) :: lread
        INTEGER*4,DIMENSION(ncol*3) :: lread2
        INTEGER*1,DIMENSION(ncol*3) :: lwrite
        INTEGER*4 :: j, j2, i, k
        INTEGER*4 :: icpt
        INTEGER*4,DIMENSION(nlin*3) :: icpt2

        OPEN(11,file=INFILE,form='unformatted',access='stream',status='old') 
        OPEN(13,file=OUTFILE,form='unformatted',access='direct',recl=ncol*3) 

        READ(11) icpt2
        ! boucle sur les lignes
        DO j=1,nlin

          DO j2 = 1,3

        ! lecture du lai
            lread(:) = 0
            READ(11) lread(1:icpt2((j-1)*3+j2))
            lread2(:) = lread(:)
            DO k = 1,icpt2((j-1)*3+j2)
              IF (lread2(k)<0) lread2(k) = 32768*2 + lread2(k)
            END DO

            icpt = 0

            i = 1

        ! boucle sur les colonnes
            DO 

          ! si on a dépassé la dernière colonne, on sort de la boucle
              IF (icpt>=ncol*3) EXIT

          ! si la valeur est valide
              IF (lread2(i)<4000) THEN

            ! on la met dans lwrite à l'indice icpt
                icpt = icpt + 1
                lwrite(icpt) = lread2(i)-floor(lread2(i)/100.)*100.
                ! on incrémente i
                i = i+1

              ELSE

                DO k = 1,lread2(i)-4000
                  icpt = icpt + 1
                  IF (icpt>ncol*3) EXIT
                  lwrite(icpt) = 0
                END DO

                i = i+1

              ENDIF

            END DO

            WRITE(13,rec=(j-1)*3+j2) lwrite(:)

          END DO

        END DO

        CLOSE(11)
        CLOSE(13)

    END SUBROUTINE UNCOMPRESS

END PROGRAM MAIN