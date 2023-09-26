C
C     This program generates a set of binary test files for
C     use in validating the Leapseconds class.  It simply
C     executes, loads the test leapseconds kernel, and
C     produces four binary .dat files that contain the
C     outputs of the SPICE conversions.
C
C     Note: the test code in the Java package assumes that
C     this is run on a little endian, IEEE compliant system.
C
      PROGRAM LSK
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      UNITIM
 
C
C     Parameters
C
      INTEGER               ARYLEN
      PARAMETER           ( ARYLEN = 21 )
 
C
C     Variables
C
      DOUBLE PRECISION      ET ( ARYLEN )
      DOUBLE PRECISION      TS ( ARYLEN )
 
      INTEGER               I
      INTEGER               LUN
 
C
C     Load the test leapseconds kernel into the application.
C     The data files this program produces will need to be
C     regenerated if this kernel's contents are altered.
C
      CALL FURNSH ( 'test.tls' )
 
C
C     We have four separate tests to set up.  Populate the ET
C     array and convert to the two alternate time systems.
C
      ET(1) = -2398376758.876600000D0
      ET(2) = -1325419200.000000000D0
      ET(3) = -883655957.816080000D0
      ET(4) = -883525447.692940000D0
      ET(19) = 220886059.747320000D0
      ET(20) = 3155716865.183900000D0
      ET(21) = 0.0D0
 
      DO I = 5, 18
         ET(I) = ET(4) + (ET(19)-ET(4))/(20-5)*(I-4)
      END DO
 
C
C     Start with the ET to TDT conversion.
C
      CALL OPENFL ( 'tdbtotdt.dat', LUN )
 
      DO I = 1, ARYLEN
         TS(I) = UNITIM ( ET(I), 'TDB', 'TDT' )
      END DO
 
      CALL WRITFL ( LUN, ARYLEN, ET, TS )
 
      CLOSE ( UNIT = LUN )
 
C
C     And now the ET to TAI conversion.
C
      CALL OPENFL ( 'tdbtotai.dat', LUN )
 
      DO I = 1, ARYLEN
         TS(I) = UNITIM ( ET(I), 'TDB', 'TAI' )
      END DO
 
      CALL WRITFL ( LUN, ARYLEN, ET, TS )
 
      CLOSE ( UNIT = LUN )
 
C
C     At this point, it seems apparent we can just
C     invert the sense of the conversion and still
C     start with the same "ET" array.  It makes
C     little difference at this point.
C
      CALL OPENFL ( 'taitotdb.dat', LUN )
 
      DO I = 1, ARYLEN
         TS(I) = UNITIM ( ET(I), 'TAI', 'TDB' )
      END DO
 
      CALL WRITFL ( LUN, ARYLEN, ET, TS )
 
      CLOSE ( UNIT = LUN )
 
C
C     And the TDT to TDB conversion.
C
      CALL OPENFL ( 'tdttotdb.dat', LUN )
 
      DO I = 1, ARYLEN
         TS(I) = UNITIM ( ET(I), 'TDT', 'TDB' )
      END DO
 
      CALL WRITFL ( LUN, ARYLEN, ET, TS )
 
      CLOSE ( UNIT = LUN )
 
      END
 
 
 
      SUBROUTINE WRITFL ( LUN, ARYLEN, FROM, TO )
 
C
C     Argument Declarations
C
      INTEGER               LUN
      INTEGER               ARYLEN
      DOUBLE PRECISION      FROM   ( * )
      DOUBLE PRECISION      TO     ( * )
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
 
C
C     Local Variables
C
      INTEGER               I
      INTEGER               IOSTAT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'WRITFL' )
 
      DO I = 1, ARYLEN
         WRITE ( LUN, REC = I, IOSTAT = IOSTAT ) FROM(I), TO(I)
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Failure to write record #. IOSTAT was #.' )
            CALL ERRINT ( '#', I                                     )
            CALL ERRINT ( '#', IOSTAT                                )
            CALL SIGERR ( 'SPICE(WRITEFAILED)'                       )
            CALL CHKOUT ( 'WRITFL'                                   )
            RETURN
         END IF
      END DO
 
      CALL CHKOUT ( 'WRITFL' )
      RETURN
 
      END
 
 
 
 
      SUBROUTINE OPENFL ( FILE, LUN )
 
C
C     Argument Declarations
C
      CHARACTER*(*)         FILE
      INTEGER               LUN
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C     Record length parameter; note this value is known only
C     to work with g77 on a LINUX system.  The FORTRAN standard
C     does not address whether this is in words or bytes, etc.
C     which results in compiler specific definitions.
C
      INTEGER               RECL
      PARAMETER           ( RECL = 16 )
 
C
C     Local Variables
C
      INTEGER               IOSTAT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'OPENFL' )
 
C
C     Fetch a logical unit to use.
C
      CALL GETLUN ( LUN )
 
      OPEN ( UNIT = LUN,
     .       FILE = FILE,
     .       ACCESS = 'DIRECT',
     .       RECL = RECL,
     .       STATUS = 'NEW',
     .       IOSTAT = IOSTAT )
 
C
C     Check to see if the file open failed.
C
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Unable to open file.  IOSTAT was #.' )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'               )
         CALL CHKOUT ( 'OPENFL'                              )
         RETURN
      END IF
 
      CALL CHKOUT ( 'OPENFL' )
      RETURN
 
      END
