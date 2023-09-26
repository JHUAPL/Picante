      PROGRAM TESTFROMCARTTOCOORD
      IMPLICIT NONE

      CHARACTER          TYPE

      CHARACTER*(256)    CMDARG

      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      VX
      DOUBLE PRECISION      VY
      DOUBLE PRECISION      VZ

      CALL GETARG ( 1, CMDARG )
      READ (CMDARG, *) TYPE 
      CALL GETARG ( 2, CMDARG )
      READ (CMDARG, *) X
      CALL GETARG ( 3, CMDARG )
      READ (CMDARG, *) Y
      CALL GETARG ( 4, CMDARG )
      READ (CMDARG, *) Z
      CALL GETARG ( 5, CMDARG )
      READ (CMDARG, *) VX
      CALL GETARG ( 6, CMDARG )
      READ (CMDARG, *) VY
      CALL GETARG ( 7, CMDARG )
      READ (CMDARG, *) VZ

      CALL ERRPRT ( 'SET' , 'NONE' )
      CALL ERRACT ( 'SET', 'RETURN' )
      
      IF (TYPE .EQ. 'L') THEN
         CALL RECTOLAT(X,Y,Z, VX,VY,VZ)
      ENDIF
      IF (TYPE .EQ. 'S') THEN
         CALL RECTOSPH(X,Y,Z, VX,VY,VZ)
      ENDIF
      IF (TYPE .EQ. 'C') THEN
         CALL RECTOCYL(X,Y,Z, VX,VY,VZ)
      ENDIF

      END


C     COMMENTS
      SUBROUTINE RECTOLAT (X, Y, Z, VX, VY, VZ)

      IMPLICIT NONE

      LOGICAL            FAILED

      DOUBLE PRECISION      POS(3)
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      VEL(3)
      DOUBLE PRECISION      VX
      DOUBLE PRECISION      VY
      DOUBLE PRECISION      VZ

      DOUBLE PRECISION      R
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LATV(3)

      DOUBLE PRECISION      JACOBI ( 3, 3 )
      
      POS(1) = X
      POS(2) = Y
      POS(3) = Z

      VEL(1) = VX
      VEL(2) = VY
      VEL(3) = VZ

      CALL RECLAT (POS, R, LONG, LAT )
      CALL DLATDR (X, Y, Z, JACOBI )

      CALL MXV ( JACOBI, VEL, LATV )

      IF( FAILED() ) THEN
         WRITE(*,*) 'FAILED'
      ELSE 
         WRITE(*,*)  R, LONG, LAT, 
     .        LATV(1),LATV(2),LATV(3)
      ENDIF


      END


C     COMMENTS
      SUBROUTINE RECTOSPH (X, Y, Z, VX, VY, VZ)

      IMPLICIT NONE

      LOGICAL            FAILED

      DOUBLE PRECISION      POS(3)
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      VEL(3)
      DOUBLE PRECISION      VX
      DOUBLE PRECISION      VY
      DOUBLE PRECISION      VZ

      DOUBLE PRECISION      R
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LATV(3)

      DOUBLE PRECISION      JACOBI ( 3, 3 )
      
      POS(1) = X
      POS(2) = Y
      POS(3) = Z

      VEL(1) = VX
      VEL(2) = VY
      VEL(3) = VZ

      CALL RECSPH (POS, R, LONG, LAT )
      CALL DSPHDR (X, Y, Z, JACOBI )

      CALL MXV ( JACOBI, VEL, LATV )


      IF( FAILED() ) THEN
         WRITE(*,*) 'FAILED'
      ELSE 
         WRITE(*,*)  R, LONG, LAT, 
     .        LATV(1),LATV(2),LATV(3)
      ENDIF


      END


C     COMMENTS
      SUBROUTINE RECTOCYL (X, Y, Z, VX, VY, VZ)

      IMPLICIT NONE

      LOGICAL            FAILED

      DOUBLE PRECISION      POS(3)
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      VEL(3)
      DOUBLE PRECISION      VX
      DOUBLE PRECISION      VY
      DOUBLE PRECISION      VZ

      DOUBLE PRECISION      R
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LATV(3)

      DOUBLE PRECISION      JACOBI ( 3, 3 )
      
      POS(1) = X
      POS(2) = Y
      POS(3) = Z

      VEL(1) = VX
      VEL(2) = VY
      VEL(3) = VZ

      CALL RECCYL (POS, R, LONG, LAT )
      CALL DCYLDR (X, Y, Z, JACOBI )

      CALL MXV ( JACOBI, VEL, LATV )


      IF( FAILED() ) THEN
         WRITE(*,*) 'FAILED'
      ELSE 
         WRITE(*,*)  R, LONG, LAT, 
     .        LATV(1),LATV(2),LATV(3)
      ENDIF


      END
