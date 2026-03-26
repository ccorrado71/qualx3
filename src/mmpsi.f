      DOUBLE PRECISION FUNCTION MMPSI (ARG,IER)
C
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER            IER
      DOUBLE PRECISION   ARG
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER            I,M,N,NQ,IEND,IPEND,IQEND,JEND,JENDP1
      DOUBLE PRECISION   P1(9),P2(5),Q1(8),Q2(5)
      DOUBLE PRECISION   AUG,DEN,FUDGE,PIOV4,SCALE,SGN,XSMALL,UPPER
      DOUBLE PRECISION   W,X01,X02,XINF,XMINS,Z,XLARGE,XMAX1,X,XMX
C                                  MACHINE DEPENDENT CONSTANTS
C                                    PIOV4 = PI / 4
C                                    SCALE = SCALING FACTOR
C                                    XINF = LARGEST POSITIVE MACHINE
C                                    NUMBER
C                                    XMAX1 = SMALLEST POSITIVE
C                                    FLOATING-POINT CONSTANT WITH
C                                    ENTIRELY INTEGER REPRESENTATION.
C                                    ALSO USED AS NEGATIVE OF LOWER
C                                    BOUND ON ACCEPTABLE NEGATIVE
C                                    ARGUMENTS
C                                    XLARGE = THE POSITIVE
C                                    ARGUMENT BEYOND WHICH PSI MAY BE
C                                    REPRESENTED AS DLOG(ARG)
C                                    XMINS= SMALLEST ACCEPTABLE ABSOLUTE
C                                    ARGUMENT SCALED BY SCALE
C                                    XSMALL = ABSOLUTE ARGUMENT BELOW
C                                    WHICH PI*COTAN(PI*ARG) MAY BE
C                                    REPRESENTED BY 1/ARG
C                                    X01 + X02 = ZERO OF PSI TO
C                                    EXTENDED PRECISION
      DATA               PIOV4/.7853981633974483D0/
      DATA               SCALE/2251799813685248D0/
      DATA               XINF/.1797693D+308/
      DATA               XMAX1/4503599627370496D0/
      DATA               XMINS/7.65D-283/
      DATA               XSMALL/.2328306436538696D-09/
      DATA               X01/1.461632132530212D0/
      DATA               X02/.1243814993891891D-07/
      DATA               FUDGE/.0000000000000000D0/
      DATA               XLARGE/5764607523034235D0/
C                                  COEFFICIENTS FOR RATIONAL
C                                  APPROXIMATION OF PSI(ARG) / (ARG -
C                                    X0), 0.5 .LE. ARG .LE. 3.0
      DATA               P1/.451046812457629341D-02,
     1                   .549328558330003853D01,.376466931759292768D03,
     2                   .795254908491519980D04,.714515958189519331D05,
     3                   .306559763019873657D06,.636069977889644587D06,
     4                   .580413127835375699D06,.165856950297610223D06/
      DATA               Q1/.961416547742223584D02,
     1                   .262877157905811933D04,.298624970222502779D05,
     2                   .162065660915336716D06,.434878807127683290D06,
     3                   .542563845372699936D06,.242421850020179852D06,
     4                   .641552237835762260D-07/
C                                  COEFFICIENTS FOR RATIONAL
C                                    APPROXIMATION OF PSI(ARG) - LN(ARG)
C                                    + 1 / (2*ARG), ARG .GT. 3.0
      DATA               P2/-.243139315843465550D01,
     1                   -.107724056346479299D02,
     2                   -.104226833638835286D02,
     3                   -.305024768080386749D01,
     4                   -.246151396734562890D00/
      DATA               Q2/.386804660835486703D02,
     1                   .140521631326370313D03,.128621377815264254D03,
     2                   .368983538456960431D02,.295381676081483886D01/
C                                  FIRST EXECUTABLE STATEMENT
      X = ARG
      IEND = 7
      IPEND = 9
      IQEND = 8
      JEND = 4
      JENDP1 = 5
      IER = 0
      AUG = 0.0D0
      IF (X.GE.0.5D0) GO TO 25
C                                  ARG .LT. 0.5, USE REFLECTION
C                                    FORMULA PSI(1-ARG) = PSI(ARG) + PI
C                                    * COTAN(PI*ARG)
      IF (DABS(X).GT.XSMALL) GO TO 5
      IF(DABS(X)*SCALE*1024.D0.LT.XMINS) GO TO 55
C                                  XMIN .LT. DABS(ARG) .LE. XSMALL. USE
C                                    1/ARG AS A SUBSTITUTE FOR
C                                    PI*COTAN(PI*ARG)
      AUG = -1.0D0/X
      GO TO 20
C                                  REDUCTION OF ARGUMENT FOR COTAN
    5 W = -X
      SGN = PIOV4
      IF (W.GT.0.0D0) GO TO 10
      W = -W
      SGN = -SGN
C                                  MAKE AN ERROR EXIT IF ARG .LE.
C                                    -XMAX1
   10 XMX = XMAX1
      IF (W.GE.XMX) GO TO 50
      Z = DINT(W)
      W = W - Z
      NQ = IDINT(W*4.0D0)
      W = 4.0D0 * (W - (DBLE(FLOAT(NQ))*.25D0))
C                                  W IS NOW RELATED TO THE FRACTIONAL
C                                    PART OF 4.0 * ARG. ADJUST ARGUMENT
C                                    TO CORRESPOND TO VALUES IN FIRST
C                                    QUADRANT AND DETERMINE SIGN
      N = NQ/2
      IF ((N+N).NE.NQ) W = 1.0D0-W
      Z = PIOV4*W
      M = N/2
      IF ((M+M).NE.N) SGN = -SGN
C                                  DETERMINE FINAL VALUE FOR
C                                    -PI*COTAN(PI*ARG)
      N = (NQ+1)/2
      M = N/2
      M = M+M
      IF (M.NE.N) GO TO 15
C                                  CHECK FOR SINGULARITY
      IF (Z.EQ.0.0D0) GO TO 55
C                                  USE COS/SIN AS A SUBSTITUTE FOR
C                                    COTAN, AND SIN/COS AS A SUBSTITUTE
C                                    FOR TAN
      AUG = SGN*((DCOS(Z)/DSIN(Z))*4.0D0)
      GO TO 20
   15 AUG = SGN*((DSIN(Z)/DCOS(Z))*4.0D0)
   20 X = 1.0D0-X
   25 IF (X.GT.3.0D0) GO TO 35
C                                  0.5 .LE. ARG .LE. 3.0
      DEN = X
      UPPER = P1(1)*X
      DO 30 I=1,IEND
         DEN = (DEN+Q1(I))*X
         UPPER = (UPPER+P1(I+1))*X
   30 CONTINUE
      DEN = (UPPER+P1(IPEND))/(DEN+Q1(IQEND))
      MMPSI = DEN*((X-X01)-X02) + AUG
      GO TO 9005
C                                  IF ARG .GE. XMAX1, PSI = LN(ARG)
   35 IF (AUG.EQ.0.0D0) AUG = FUDGE
      IF (X.GE.XLARGE) GO TO 45
C                                  3.0 .LT. ARG .LT. XMAX1
      W = 1.0D0/(X*X)
      DEN = W
      UPPER = P2(1)*W
      DO 40 I=1,JEND
         DEN = (DEN+Q2(I))*W
         UPPER = (UPPER+P2(I+1))*W
   40 CONTINUE
      AUG = UPPER/(DEN+Q2(JENDP1))-0.5D0/X+AUG
   45 MMPSI = AUG+DLOG(X)
      GO TO 9005
C                                  ERROR RETURN FOR ARG .LE. -XMAX1
   50 MMPSI = 0.0D0
      IER = 129
      GO TO 9000
C                                  ERROR RETURN FOR -ARG AN INTEGER OR
C                                    DABS(ARG) .LT. XMIN
   55 MMPSI = XINF
      IER = 130
      IF (X .LE. 0.0D0) GO TO 9000
      MMPSI = -XINF
      IER = 131
C                                  UPDATE ERROR COUNTS, ETC.
 9000 CONTINUE
      CALL UERTST (IER,'MMPSI ')
 9005 RETURN
      END
C
      SUBROUTINE UERTST (IER,NAME)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER            IER
      CHARACTER          NAME*(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER            I,IEQDF,IOUNIT,LEVEL,LEVOLD,NIN,NMTB
      CHARACTER          IEQ,NAMEQ(6),NAMSET(6),NAMUPK(6)
      DATA               NAMSET/'U','E','R','S','E','T'/
      DATA               NAMEQ/6*' '/
      DATA               LEVEL/4/,IEQDF/0/,IEQ/'='/
C                                  UNPACK NAME INTO NAMUPK
C                                  FIRST EXECUTABLE STATEMENT
      CALL USPKD (NAME,6,NAMUPK,NMTB)
C                                  GET OUTPUT UNIT NUMBER
      CALL UGETIO(1,NIN,IOUNIT)
C                                  CHECK IER
      IF (IER.GT.999) GO TO 25
      IF (IER.LT.-32) GO TO 55
      IF (IER.LE.128) GO TO 5
      IF (LEVEL.LT.1) GO TO 30
C                                  PRINT TERMINAL MESSAGE
      IF (IEQDF.EQ.1) WRITE(IOUNIT,35) IER,NAMEQ,IEQ,NAMUPK
      IF (IEQDF.EQ.0) WRITE(IOUNIT,35) IER,NAMUPK
      GO TO 30
    5 IF (IER.LE.64) GO TO 10
      IF (LEVEL.LT.2) GO TO 30
C                                  PRINT WARNING WITH FIX MESSAGE
      IF (IEQDF.EQ.1) WRITE(IOUNIT,40) IER,NAMEQ,IEQ,NAMUPK
      IF (IEQDF.EQ.0) WRITE(IOUNIT,40) IER,NAMUPK
      GO TO 30
   10 IF (IER.LE.32) GO TO 15
C                                  PRINT WARNING MESSAGE
      IF (LEVEL.LT.3) GO TO 30
      IF (IEQDF.EQ.1) WRITE(IOUNIT,45) IER,NAMEQ,IEQ,NAMUPK
      IF (IEQDF.EQ.0) WRITE(IOUNIT,45) IER,NAMUPK
      GO TO 30
   15 CONTINUE
C                                  CHECK FOR UERSET CALL
      DO 20 I=1,6
         IF (NAMUPK(I).NE.NAMSET(I)) GO TO 25
   20 CONTINUE
      LEVOLD = LEVEL
      LEVEL = IER
      IER = LEVOLD
      IF (LEVEL.LT.0) LEVEL = 4
      IF (LEVEL.GT.4) LEVEL = 4
      GO TO 30
   25 CONTINUE
      IF (LEVEL.LT.4) GO TO 30
C                                  PRINT NON-DEFINED MESSAGE
      IF (IEQDF.EQ.1) WRITE(IOUNIT,50) IER,NAMEQ,IEQ,NAMUPK
      IF (IEQDF.EQ.0) WRITE(IOUNIT,50) IER,NAMUPK
   30 IEQDF = 0
      RETURN
   35 FORMAT(19H *** TERMINAL ERROR,10X,7H(IER = ,I3,
     1       20H) FROM IMSL ROUTINE ,6A1,A1,6A1)
   40 FORMAT(27H *** WARNING WITH FIX ERROR,2X,7H(IER = ,I3,
     1       20H) FROM IMSL ROUTINE ,6A1,A1,6A1)
   45 FORMAT(18H *** WARNING ERROR,11X,7H(IER = ,I3,
     1       20H) FROM IMSL ROUTINE ,6A1,A1,6A1)
   50 FORMAT(20H *** UNDEFINED ERROR,9X,7H(IER = ,I5,
     1       20H) FROM IMSL ROUTINE ,6A1,A1,6A1)
C
C                                  SAVE P FOR P = R CASE
C                                    P IS THE PAGE NAMUPK
C                                    R IS THE ROUTINE NAMUPK
   55 IEQDF = 1
      DO I=1,6
      NAMEQ(I) = NAMUPK(I)
      ENDDO
      RETURN
      END
C
      SUBROUTINE UGETIO(IOPT,NIN,NOUT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER            IOPT,NIN,NOUT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER            NIND,NOUTD
      DATA               NIND/5/,NOUTD/6/
C                                  FIRST EXECUTABLE STATEMENT
      IF (IOPT.EQ.3) GO TO 10
      IF (IOPT.EQ.2) GO TO 5
      IF (IOPT.NE.1) GO TO 9005
      NIN = NIND
      NOUT = NOUTD
      GO TO 9005
    5 NIND = NIN
      GO TO 9005
   10 NOUTD = NOUT
 9005 RETURN
      END
C
      SUBROUTINE USPKD  (PACKED,NCHARS,UNPAKD,NCHMTB)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER            NC,NCHARS,NCHMTB
C
      CHARACTER          UNPAKD(1),IBLANK
CORO  CHARACTER*(1)      PACKED(1)
      CHARACTER          PACKED*(*)
      DATA               IBLANK /' '/
C                                  INITIALIZE NCHMTB
      NCHMTB = 0
C                                  RETURN IF NCHARS IS LE ZERO
      IF(NCHARS.LE.0) RETURN
C                                  SET NC=NUMBER OF CHARS TO BE DECODED
      NC = MIN0 (129,NCHARS)
      DO 5 I=1,NC
CORO     UNPAKD(I) = PACKED(I)
         UNPAKD(I) = PACKED(I:I)
    5 CONTINUE
C                                  CHECK UNPAKD ARRAY AND SET NCHMTB
C                                  BASED ON TRAILING BLANKS FOUND
      DO 200 N = 1,NC
         NN = NC - N + 1
         IF(UNPAKD(NN) .NE. IBLANK) GO TO 210
  200 CONTINUE
      NN = 0
  210 NCHMTB = NN
      RETURN
      END
