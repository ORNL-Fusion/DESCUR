      SUBROUTINE read_indata(in_file, iunit, ier_flag,
     1                       nphi20, rbdy_3d, zbdy_3d)

      USE safe_open_mod
      USE Vname0
      USE Vname1, ONLY : mpol_in => mpol, nfp_in => nfp,
     1    nphi2_in => nphi2
      USE vmec_input
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER :: ier_flag, iunit, nphi20
      CHARACTER(LEN=*), INTENT(IN) :: in_file
      INTEGER :: m, n
      REAL(rprec) :: rbdy_3d(0:mu-1,-nphi20:nphi20,2),
     1               zbdy_3d(0:mu-1,-nphi20:nphi20,2)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: ireadseq, iosnml
!-----------------------------------------------
      ier_flag=0
      rbdy_3d = 0
      zbdy_3d = 0

      CALL safe_open (iunit, ireadseq, in_file, 'old', 'formatted')
      IF (ireadseq .ne. 0) THEN
         WRITE (6, '(3a,i4)') ' In DESCUR, error opening input file: ',
     1   TRIM(in_file), '. Iostat = ', ireadseq
         ier_flag = 1
         RETURN
      ENDIF

      CALL read_namelist (iunit, iosnml, 'indata')
      IF (iosnml .ne. 0) THEN
         WRITE (6, '(a,i4)') 
     1   ' In DESCUR, indata NAMELIST error: iostat = ', iosnml
         ier_flag = 1
         RETURN
      ENDIF

!
!     SAME AS READ FROM WOUT FILE
!
      mpol_in  = mpol
      nfp_in   = nfp
      nphi2_in = ntor

      IF (mpol .gt. mu) STOP 'mpol-input > mu'
      IF (nfp .le. 0) nfp = 1

!
!     GENERALIZE TO ASYMMETRY
!
      DO m = 0, mpol
         DO n = -ntor, ntor
            rbdy_3d(m,n,1) = rbc(n,m)
            zbdy_3d(m,n,1) = zbs(n,m)
            rbdy_3d(m,n,2) = rbs(n,m)
            zbdy_3d(m,n,2) = zbc(n,m)
         END DO
      END DO

      END SUBROUTINE read_indata
