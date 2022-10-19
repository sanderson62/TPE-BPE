       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ECS056.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.

      *REMARKS.

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 062304    2004061600004  PEMA  ADD PROCESSING FOR CANCELED
                                       GIANT CERTS
110304* 110304    2004110200007  PEMA  ADD NI AND NU TO LIST OF AH
110304*                                GIANT BEN CODES
      *                                ADD MS TO LIFE
072005* 072005    2005071100001  PEMA  CREATE CREDIT ON REFUNDS
080305* 080305    2005080300010  PEMA  ADD SUM REM PMTS FORMULA
071906* 071906    2006012700002  PEMA  ADD MI AND MU TO CASH CERT
061112* 061112    2012060600001  AJRA  ADD P TYPE
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS                ASSIGN TO SYS010.
           SELECT EXTRACT              ASSIGN TO SYS011
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DEBUG-EXTRACT        ASSIGN TO SYS012
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DISK-DATE            ASSIGN TO SYS019.
           SELECT PRINTX               ASSIGN TO SYS008.
           SELECT FICH                 ASSIGN TO SYS020.

           SELECT ERPYAJ               ASSIGN TO ERPYAJ
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS PY-CONTROL-PRIMARY
                  FILE STATUS IS ERPYAJ-FILE-STATUS.

           SELECT ERDUEP               ASSIGN TO ERDUEP
                  ACCESS IS DYNAMIC
                  ORGANIZATION IS INDEXED
                  FILE STATUS IS ERDUEP-FILE-STATUS
                  RECORD KEY IS DP-CONTROL-PRIMARY.
                                                                        
       DATA DIVISION.
       FILE SECTION.


       EJECT
       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.

           EJECT
       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

      *         236 BYTES
       01  EXTRACT-RECORD.
           05  EXT-CAR                 PIC X.   
           05  WS-TAB1                 PIC X.
           05  EXT-GROUP               PIC X(6).
           05  WS-TAB2                 PIC X.
           05  EXT-STATE               PIC XX.
           05  WS-TAB3                 PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  WS-TAB4                 PIC X.
           05  EXT-EFF-DATE            PIC X(10).
           05  WS-TAB5                 PIC X.
           05  EXT-ENT-DATE            PIC X(10).
           05  WS-TAB6                 PIC X.
           05  EXT-VAL-DATE            PIC X(10).
           05  WS-TAB7                 PIC X.
           05  EXT-EARN-TERM           PIC 999.
           05  WS-TAB8                 PIC X.
           05  EXT-CERT-NO             PIC X(11).
           05  WS-TAB9                 PIC X.
           05  EXT-LF-BEN-CODE         PIC XX.
           05  WS-TAB10                PIC X.
           05  EXT-AH-BEN-CODE         PIC XX.
           05  WS-TAB11                PIC X.
           05  EXT-LF-TERM             PIC 999.
           05  WS-TAB12                PIC X.
           05  EXT-AH-TERM             PIC 999.
           05  WS-TAB13                PIC X.
           05  EXT-LF-EXP-DATE         PIC X(10).
           05  WS-TAB14                PIC X.
           05  EXT-AH-EXP-DATE         PIC X(10).
           05  WS-TAB15                PIC X.
           05  EXT-EXP-PREM            PIC -ZZ,ZZZ,ZZ9.99.
           05  WS-TAB16                PIC X.
           05  EXT-REC-PREM            PIC -ZZ,ZZZ,ZZ9.99.
           05  WS-TAB17                PIC X.
           05  EXT-BASE-COMM           PIC -ZZZ,ZZ9.99.
           05  WS-TAB18                PIC X.
           05  EXT-GA-COMM             PIC -ZZZ,ZZ9.99.
           05  WS-TAB19                PIC X.
           05  EXT-MISC1               PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB20                PIC X.
           05  EXT-MISC2               PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB21                PIC X.
           05  EXT-MISC3               PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB22                PIC X.
           05  EXT-MISC4               PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-TAB23                PIC X.
           05  EXT-MISC5               PIC ZZZ,ZZZ,ZZ9.99.


       FD  DEBUG-EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

      *         236 BYTES
       01  DEBUG-EXTRACT-RECORD.
           05  DB-ACCOUNT              PIC X(10).
           05  DB-TAB1                 PIC X.
           05  DB-CERT-NO              PIC X(11).
           05  DB-TAB2                 PIC X.
           05  DB-EFF-DATE             PIC X(10).
           05  DB-TAB3                 PIC X.
           05  DB-ENT-DATE             PIC X(10).
           05  DB-TAB4                 PIC X.
           05  DB-VAL-DATE             PIC X(10).
           05  DB-TAB5                 PIC X.
           05  DB-PERIOD               PIC 999.
           05  DB-TAB6                 PIC X.
           05  DB-PAYMENT              PIC -ZZ,ZZZ,ZZ9.99.
           05  DB-TAB7                 PIC X.
           05  DB-PRINC                PIC -ZZZ,ZZ9.99.
           05  DB-TAB8                 PIC X.
           05  DB-INTEREST             PIC -Z,ZZ9.99.
           05  DB-TAB9                 PIC X.
           05  DB-LINS                 PIC -Z,ZZ9.99.
           05  DB-TAB10                PIC X.
           05  DB-DINS                 PIC -Z,ZZ9.99.
           05  DB-TAB11                PIC X.
           05  DB-BALANCE              PIC -ZZ,ZZZ,ZZ9.99.
           05  DB-TAB12                PIC X.
           05  DB-LF-BEN-CODE          PIC XX.
           05  DB-TAB13                PIC X.
           05  DB-AH-BEN-CODE          PIC XX.
           05  DB-TAB14                PIC X.
           05  DB-LF-TERM              PIC 999.
           05  DB-TAB15                PIC X.
           05  DB-AH-TERM              PIC 999.
           05  DB-TAB16                PIC X.
           05  DB-LF-EXP-DATE          PIC X(10).
           05  DB-TAB17                PIC X.
           05  DB-AH-EXP-DATE          PIC X(10).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       FD  FICH
                                       COPY ELCFCHFD.
       EJECT
       FD  ERDUEP.
                                       COPY ERCDUEP.
       EJECT
       FD  ERPYAJ.

                                       COPY ERCPYAJ.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '      ECS056  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-FILE                VALUE 'Y'.
           88  MORE-RECORDS               VALUE 'N'.
       77  IYR                         PIC S9(03) COMP-3 VALUE +0.
       77  VYR                         PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  SPACE-NP                    PIC X      VALUE '1'.
       77  SPACE-1                     PIC X      VALUE ' '.
       77  SPACE-2                     PIC X      VALUE '0'.
       77  SPACE-3                     PIC X      VALUE '-'.
       77  X                           PIC X      VALUE ' '.
       77  LNCTR                       PIC S999   VALUE +60 COMP-3.
       77  PGCTR                       PIC S9(5)  VALUE +0 COMP-3.

       01  HD1.
           12  FILLER              PIC  X(44)      VALUE SPACES.
           12  FILLER              PIC  X(44)      VALUE
                   '        DUE PREMIUM AND COMMISSION         '.
           12  FILLER              PIC  X(31)      VALUE SPACES.
           12  FILLER              PIC  X(08)      VALUE 'ECS056'.

       01  HD2.
           12  FILLER              PIC  X(51)      VALUE SPACES.
           12  HD-CO               PIC  X(30).
           12  FILLER              PIC  X(38)      VALUE SPACES.
           12  HD-RUN-DT           PIC  X(08)      VALUE SPACES.

       01  HD3.
           12  FILLER              PIC  X(57)      VALUE SPACES.
           12  HD-DT               PIC  X(18).
           12  FILLER              PIC  X(44)      VALUE SPACES.
           12  FILLER              PIC  X(05)      VALUE 'PAGE '.
           12  HD-PG               PIC ZZ,ZZ9.

       01  HD4.
           12  FILLER              PIC  X(44)      VALUE
                   '-- ACCOUNT TOTALS  --      BEGINNING   EXPEC'.
           12  FILLER              PIC  X(44)      VALUE
                   'TED   RECEIVED              ENDING          '.
           12  FILLER              PIC  X(44)      VALUE
                   '                    DUE      NO OF          '.

       01  HD5.
           12  FILLER              PIC  X(44)      VALUE
                   '--CAR GROUP  ST  ACCOUNT--- BALANCE    PREMI'.
           12  FILLER              PIC  X(44)      VALUE
                   'UMS   PREMIUMS  ADJUSTMENTS BALANCE      < 9'.
           12  FILLER              PIC  X(44)      VALUE
                   '0        > 90   COMMISSION   MONTHS         '.
       01  DETAIL-LINE.
           05  FILLER                  PIC X(4).
           05  DTL1-CAR                PIC X.
           05  FILLER                  PIC X.
           05  DTL1-GRP                PIC X(6).
           05  FILLER                  PIC X.
           05  DTL1-ST                 PIC XX.
           05  FILLER                  PIC X.
           05  DTL1-ACCT               PIC X(10).
           05  FILLER                  PIC X.
           05  DTL1-BEG-BAL            PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-EXP-PREM           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-REC-PREM           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-ADJS               PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-END-BAL            PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-DUE-LT90           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-DUE-GT90           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-COMM               PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X(4).
           05  DTL1-NO-MOS             PIC ZZ9.
       EJECT
       01  WS-DUE-PREM-ADJ-TABLE.
           05  WS-DUEP-ADJ OCCURS 500.
               10  WS-DA-KEY           PIC X(33).
               10  WS-DA-AMOUNT        PIC S9(7)V99 COMP-3.

       01  WS-DUE-PREM-TABLE.
           05  WS-RECORD.
               10  WS-CAR              PIC X.   
               10  WS-GROUP            PIC X(6).
               10  WS-STATE            PIC XX.
               10  WS-ACCOUNT          PIC X(10).
               10  WS-EFF-DATE         PIC X(10).
               10  WS-CERT-NO          PIC X(11).
               10  WS-LF-BEN-CODE      PIC XX.
               10  WS-AH-BEN-CODE      PIC XX.
               10  WS-LF-TERM          PIC ZZ9.
               10  WS-AH-TERM          PIC ZZ9.
               10  WS-LF-EXP-DATE      PIC X(10).
               10  WS-AH-EXP-DATE      PIC X(10).
               10  WS-LF-EXP-PREM      PIC S9(9)V99 COMP-3 VALUE +0.
               10  WS-AH-EXP-PREM      PIC S9(9)V99 COMP-3 VALUE +0.
               10  WS-EXP-PREM         PIC S9(9)V99 COMP-3 VALUE +0.
               10  WS-REC-PREM         PIC S9(9)V99 COMP-3 VALUE +0.
               10  WS-LF-BASE-COMM     PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-AH-BASE-COMM     PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-BASE-COMM        PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-REC-BASE-COMM    PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-LF-GA-COMM       PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-AH-GA-COMM       PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-GA-COMM          PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC1            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC2            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC3            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC4            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC5            PIC S9(7)V99 COMP-3 VALUE +0.

       01  WS-MISC.
           05  WS-INIT-EXTRACT         PIC X(236) VALUE LOW-VALUES.
           05  WS-INIT-DB-EXTRACT      PIC X(236) VALUE LOW-VALUES.
           05  WS-PYAJ-MATCH-KEY       PIC X(28) VALUE LOW-VALUES.
           05  WS-ERPYAJ-KEY.
               10  WS-PY-COMPANY-CD    PIC X.
               10  WS-PY-CARRIER       PIC X.
               10  WS-PY-GROUPING      PIC X(6).
               10  WS-PY-FIN-RESP      PIC X(10).
               10  WS-PY-ACCOUNT       PIC X(10).
           05  ERDUEP-FILE-STATUS      PIC XX.
           05  ERPYAJ-FILE-STATUS      PIC XX.
           05  WS-HOLD-PRT             PIC X(133).
           05  WS-DUEP-CONTROL         PIC X(19)    VALUE LOW-VALUES.
           05  WS-HOLD-CONTROL.
               10  WS-HOLD-CAR         PIC X.
               10  WS-HOLD-GRP         PIC X(6).
               10  WS-HOLD-ST          PIC XX.
               10  WS-HOLD-ACCT        PIC X(10).
           05  WS-WORK-TERM            PIC S999   COMP-3 VALUE +0.
           05  INTERMED                PIC S9(9)V9(6)  COMP-3.
           05  WS-REM-AMT              PIC S9(11)V99 COMP-3 VALUE +0.
           05  WS-DISPLAY-DATE         PIC ZZZ9(8).
           05  WS-DISPLAY-TERM         PIC Z99.
           05  WS-DISPLAY-RTERM        PIC Z99.99.
           05  WS-DISPLAY-AMT          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-RAMT         PIC ZZZZZZZ.99.
           05  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-UEP          PIC ZZZZZZZ.99.
           05  WS-BIN-LAST-MONTH-DT    PIC XX  VALUE LOW-VALUES.
           05  WS-BIN-CR-DT            PIC XX  VALUE LOW-VALUES.
           05  WS-BIN-1ST-DT           PIC XX  VALUE LOW-VALUES.
           05  WS-DAYS-TO-1ST          PIC S9(3) COMP-3 VALUE +0.
           05  WS-BIN-LF-END-DATE      PIC XX  VALUE LOW-VALUES.
           05  WS-BIN-AH-END-DATE      PIC XX  VALUE LOW-VALUES.
072005     05  WS-BIN-LF-CAN-DATE      PIC XX  VALUE LOW-VALUES.
072005     05  WS-BIN-AH-CAN-DATE      PIC XX  VALUE LOW-VALUES.
072005     05  WS-LF-CAN-TERM          PIC S9(3) COMP-3 VALUE +0.
072005     05  WS-AH-CAN-TERM          PIC S9(3) COMP-3 VALUE +0.
           05  WS-LF-MIN-TERM          PIC S9(3) COMP-3 VALUE +0.
           05  WS-LF-MAX-TERM          PIC S9(3) COMP-3 VALUE +0.
           05  WS-AH-MIN-TERM          PIC S9(3) COMP-3 VALUE +0.
           05  WS-AH-MAX-TERM          PIC S9(3) COMP-3 VALUE +0.
           05  WS-LF-REF               PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-AH-REF               PIC S9(7)V99 COMP-3 VALUE +0.
072005     05  WS-LF-CAN-QUAL-SW       PIC X     VALUE 'N'.
072005         88  CREDIT-LF-CANCEL              VALUE 'Y'.
072005     05  WS-AH-CAN-QUAL-SW       PIC X     VALUE 'N'.
072005         88  CREDIT-AH-CANCEL              VALUE 'Y'.
           05  WS-END-YEAR             PIC 9(11)  VALUE ZEROS.
           05  FILLER REDEFINES WS-END-YEAR.
               10  WS-CCYY             PIC 9(7).
               10  WS-MMDD             PIC 9(4).
       01  WS-DEBUG-AREA.
           12  WS-DEBUG-SW             PIC X(01) VALUE ' '.
               88  DEBUG-IS-ON                VALUES '1' '2' '3' '4'.
               88  DEBUG-LF-INFORCE-CNT       VALUE '1'.
               88  DEBUG-AH-INFORCE-CNT       VALUE '2'.
               88  DEBUG-LF-STATUTORY         VALUE '3'.
               88  DEBUG-AH-STATUTORY         VALUE '4'.

           EJECT

       01  TEXAS-REGS-WORK-AREAS.
           12  TEX-FACT-1              PIC S9(7)V9(2)    COMP-3.
           12  TEX-FACT-2              PIC S9(3)   COMP-3.
           12  TEX-FACT-3              PIC S9(3)   COMP-3.
           12  TEX-FACT-4              PIC S9(7)   COMP-3.
           12  TEX-FACT-5              PIC S9(3)   COMP-3.
           12  TEX-FACT-6              PIC S9(3)   COMP-3.
           12  TEX-FACT-7              PIC S9(7)   COMP-3.
           12  TEX-FACT-8              PIC S9V9(6) COMP-3.
           12  TEX-FACT-9              PIC S9(4)V9(11)   COMP-3.

       01  NET-PAY-INTERFACE.
           05  NP-APR                  PIC S9(3)V9(4)    COMP-3.
           05  NP-ORIG                 PIC S9(3)         COMP-3.
           05  NP-REM                  PIC S9(3)         COMP-3.
           05  NP-OPT                  PIC X(01).
           05  NP-CAP                  PIC S9(3)         COMP-3.
           05  NP-FACTOR               PIC S9(4)V9(9)    COMP-3.
           05  NP-WORK1                PIC S9(6)V9(9)    COMP-3.
           05  NP-WORK2                PIC S9(6)V9(9)    COMP-3.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  DATE-AREAS.
           05  WS-THIS-MONTH-DATE      PIC 9(11).
           05  WS-LAST-MONTH-DATE      PIC 9(11).
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC 9(4).
               10  WS-WORK-MMDD        PIC 9(4).
           05  WS-ISSUE-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-ISSUE-DATE.
               10  FILLER              PIC XXX.
               10  WS-ISSUE-CCYY       PIC 9(4).
               10  WS-ISSUE-MMDD       PIC 9(4).
           05  WS-EXPIRE-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-EXPIRE-DATE.
               10  FILLER              PIC XXX.
               10  WS-EXPIRE-CCYY      PIC 9(4).
               10  WS-EXPIRE-MMDD      PIC 9(4).
           05  WS-ENTRY-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-ENTRY-DATE.
               10  FILLER              PIC XXX.
               10  WS-ENTRY-CCYY       PIC 9(4).
               10  WS-ENTRY-MMDD       PIC 9(4).
           05  WS-CANCEL-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-CANCEL-DATE.
               10  FILLER              PIC XXX.
               10  WS-CANCEL-CCYY      PIC 9(4).
               10  WS-CANCEL-MMDD      PIC 9(4).
           05  WS-CLAIM-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-CLAIM-DATE.
               10  FILLER              PIC XXX.
               10  WS-CLAIM-CCYY       PIC 9(4).
               10  WS-CLAIM-MMDD       PIC 9(4).
           05  WS-POSTING-DATE         PIC X(8).
           05  WS-HI-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-LO-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-HI-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-LO-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-BIN-VAL-DATES OCCURS 50
                                       PIC XX.

       01  WS-MISC-AREA.
           12  WS-DP-EXP-PREM          PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DP-REC-PREM          PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DP-GA-COMM           PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DP-BASE-COMM         PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DP-ADJ               PIC S9(9)V99 COMP-3 VALUE +0.
      *   1ST OCCURANCE           ACCOUNT TOTALS
      *   2ND                     STATE
      *   3RD                     GROUP
      *   4TH                     CARRIER
      *   5TH                     FINAL                
           12  WS-TOTAL-TABLE OCCURS 5.
               16  WS-TOT-EXP-PREM     PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-REC-PREM     PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-GA-COMM      PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-BASE-COMM    PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DISPLAY-FLD          PIC 9(9).9(5).
           12  WS-LF-PRM               PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-AH-PRM               PIC S9(7)V99 COMP-3  VALUE +0.
           12  CERT-IN-CNT             PIC 9(9)     VALUE ZEROS.
           12  WS-INT                  PIC S9(5)V999 COMP-3 VALUE +0.
           12  WS-FINT                 PIC S9(5)V999 COMP-3 VALUE +0.
           12  WS-LINS                 PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-DINS                 PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-FLINS                PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-FDINS                PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-I                    PIC SV9(9)   COMP-3  VALUE +0.
           12  WS-J                    PIC SV9(9)   COMP-3  VALUE +0.
           12  WS-Y                    PIC S99V9(9)   COMP-3  VALUE +0.
           12  WS-K                    PIC SV9(9)   COMP-3  VALUE +0.
           12  WS-VM                   PIC S9V9(9)  COMP-3  VALUE +0.
           12  WS-P                    PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-B                    PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-R                    PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-I1                   PIC S9(7)V99 COMP-3  VALUE +0.
           12  X1                      PIC S999     COMP-3  VALUE +0.
           12  WS-A                    PIC S999     COMP-3  VALUE +0.
           12  WS-PYAJ-INDEX           PIC S999     COMP-3  VALUE +0.
           12  WS-PYAJ-MAX             PIC S999     COMP-3  VALUE +0.
           12  WS-ANGLEN               PIC S9(5)V9999 COMP-3  VALUE +0.
           12  WS-OD                   PIC S9(5)V9999 COMP-3  VALUE +0.
           12  WS-AMT                  PIC S9(9)V99 COMP-3  VALUE +0.
           12  WS-WORK-AMT             PIC S9(9)V99 COMP-3  VALUE +0.
           12  WS-ANGLEM               PIC S9(5)V9999 COMP-3  VALUE +0.
           12  WS-ANGLEM-1             PIC S9(5)V9999 COMP-3  VALUE +0.
           12  WS-ANGLEMJ              PIC S9(5)V9999 COMP-3  VALUE +0.
           12  WS-ANGLEMK              PIC S9(5)V9999 COMP-3  VALUE +0.
           12  WS-ANGLEN-M             PIC S9(5)V9999 COMP-3  VALUE +0.
           12  LF-REM-TERM             PIC S999     COMP-3  VALUE +0.
           12  AH-REM-TERM             PIC S999     COMP-3  VALUE +0.
           12  LF-REM-TRM1             PIC S999V99  COMP-3  VALUE +0.
           12  AH-REM-TRM1             PIC S999V99  COMP-3  VALUE +0.
           12  LF-REM-TRM2             PIC S999V99  COMP-3  VALUE +0.
           12  AH-REM-TRM2             PIC S999V99  COMP-3  VALUE +0.
           12  REM-TERM                PIC S9(3)V99    COMP-3.
           12  REM-TRM2                PIC S9(3)V99    COMP-3.
           12  WS-LF-EARN-TERM         PIC S999            VALUE ZEROS.
           12  WS-AH-EARN-TERM         PIC S999            VALUE ZEROS.
           12  WS-EARN-TERM            PIC S999            VALUE ZEROS.
           12  WS-GIANT-SW             PIC X           VALUE SPACES.
               88  THIS-IS-A-GIANT-CERT                VALUE 'X'.


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +064.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCCALC.
      /
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           MOVE COMPANY-NAME           TO  HD-CO
           MOVE ALPH-DATE              TO  HD-DT
           MOVE WS-CURRENT-DATE        TO  HD-RUN-DT
           MOVE RUN-DATE               TO WS-THIS-MONTH-DATE

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK
           .

       0010-INITIALIZE.

           MOVE +0                     TO WS-EXP-PREM  
                                          WS-REC-PREM  
                                          WS-GA-COMM
                                          WS-BASE-COMM
                                          WS-REC-BASE-COMM

           PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
              (WS-A > +5)
              MOVE +0                  TO WS-TOT-EXP-PREM (WS-A)
                                          WS-TOT-REC-PREM (WS-A)
                                          WS-TOT-GA-COMM (WS-A)
           END-PERFORM
           MOVE LOW-VALUES             TO WS-ERPYAJ-KEY

           PERFORM VARYING WS-PYAJ-INDEX FROM +1 BY +1 UNTIL
              (WS-PYAJ-INDEX > +500)
              MOVE LOW-VALUES          TO WS-DA-KEY (WS-PYAJ-INDEX)
              MOVE +0                  TO WS-DA-AMOUNT (WS-PYAJ-INDEX)
           END-PERFORM

           MOVE +0                     TO WS-PYAJ-INDEX
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE '1'                    TO DC-END-OF-MONTH
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '6'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-LAST-MONTH-DATE
              MOVE DC-BIN-DATE-2       TO WS-BIN-LAST-MONTH-DT
              DISPLAY ' LAST MONTH IS ' WS-LAST-MONTH-DATE
           ELSE
              DISPLAY ' ERROR IN LAST MONTH DATE CONVERSION '
           END-IF

           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE WS-WORK-MMDD           TO WS-POSTING-DATE (1:4)
           MOVE WS-WORK-CCYY           TO WS-POSTING-DATE (5:4)

           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT CERTS
                      ERPYAJ
               OUTPUT EXTRACT PRINTX DEBUG-EXTRACT
               I-O ERDUEP

           IF ERDUEP-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERDUEP OPEN  '
                 ERDUEP-FILE-STATUS
              MOVE ' ERROR ON ERDUEP OPEN  '
                                       TO WS-ABEND-MESSAGE
              MOVE ERDUEP-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
              PERFORM ABEND-PGM
           END-IF

           IF ERPYAJ-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERPYAJ OPEN  '
                 ERPYAJ-FILE-STATUS
              MOVE ' ERROR ON ERPYAJ OPEN  '
                                       TO WS-ABEND-MESSAGE
              MOVE ERPYAJ-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
              PERFORM ABEND-PGM
           END-IF

           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE ';'                    TO WS-TAB1  WS-TAB2  WS-TAB3
                                          WS-TAB4  WS-TAB5  WS-TAB6
                                          WS-TAB7  WS-TAB8  WS-TAB9
                                          WS-TAB10 WS-TAB11 WS-TAB12
                                          WS-TAB13 WS-TAB14 WS-TAB15
                                          WS-TAB16 WS-TAB17 WS-TAB18
                                          WS-TAB19 WS-TAB20 WS-TAB21
                                          WS-TAB22 WS-TAB23

           MOVE ZEROS                  TO EXT-MISC1
                                          EXT-MISC2
                                          EXT-MISC3
                                          EXT-MISC4
                                          EXT-MISC5

           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT

           MOVE SPACES                 TO DEBUG-EXTRACT-RECORD
           MOVE ';'                    TO DB-TAB1  DB-TAB2  DB-TAB3
                                          DB-TAB4  DB-TAB5  DB-TAB6
                                          DB-TAB7  DB-TAB8  DB-TAB9
                                          DB-TAB10 DB-TAB11 DB-TAB12
                                          DB-TAB13 DB-TAB14 DB-TAB15
                                          DB-TAB16 DB-TAB17

           MOVE DEBUG-EXTRACT-RECORD   TO WS-INIT-DB-EXTRACT

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' MADE IT TO CLOSE FILES '
           CLOSE CERTS PRINTX
               EXTRACT ERDUEP DEBUG-EXTRACT

           IF ERDUEP-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERDUEP CLOSE '
                 ERDUEP-FILE-STATUS
              MOVE ' ERROR ON ERDUEP OPEN  '
                                       TO WS-ABEND-MESSAGE
              MOVE ERDUEP-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
              PERFORM ABEND-PGM
           END-IF
              

           .

       0030-EXIT.
           EXIT.

       0042-START-ERPYAJ.

           MOVE LOW-VALUES             TO PY-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO PY-COMPANY-CD

           START ERPYAJ KEY IS NOT < PY-CONTROL-PRIMARY

           IF ERPYAJ-FILE-STATUS = '10' OR '23'
              SET END-OF-FILE          TO TRUE
           ELSE
              IF ERPYAJ-FILE-STATUS NOT = '00'
                 MOVE ' ERROR ON ERPYAJ START '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERPYAJ-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
                 DISPLAY WS-ABEND-MESSAGE '  ' WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 DISPLAY ' GOOD START ON ERPYAJ '
              END-IF
           END-IF
           
           .
       0042-EXIT.
           EXIT.
                 
       0043-READ-ERPYAJ.

           READ ERPYAJ NEXT RECORD

           IF (ERPYAJ-FILE-STATUS = '10' OR '23')
              OR (PY-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-FILE          TO TRUE
           ELSE
              IF ERPYAJ-FILE-STATUS NOT = '00'
                 MOVE ' ERROR ON ERPYAJ READ  '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERPYAJ-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
                 DISPLAY WS-ABEND-MESSAGE '  ' WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 DISPLAY ' GOOD READ ON ERPYAJ ' PY-CONTROL-PRIMARY
              END-IF
           END-IF
           
           .
       0043-EXIT.
           EXIT.
                 
       0045-PROCESS-ERPYAJ.

           IF PY-RECORD-TYPE = 'P'
              ADD +1                   TO WS-PYAJ-INDEX
              MOVE PY-CONTROL-PRIMARY  TO WS-DA-KEY (WS-PYAJ-INDEX)
              MOVE PY-ENTRY-AMT        TO WS-DA-AMOUNT (WS-PYAJ-INDEX)
      *       DISPLAY ' FOUND A HIT ' 
           END-IF
           
           PERFORM 0043-READ-ERPYAJ    THRU 0043-EXIT

           .
       0045-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           PERFORM 0042-START-ERPYAJ   THRU 0042-EXIT
           IF NOT END-OF-FILE
              PERFORM 0043-READ-ERPYAJ THRU 0043-EXIT
           END-IF

           PERFORM 0045-PROCESS-ERPYAJ THRU 0045-EXIT UNTIL
              END-OF-FILE
           MOVE WS-PYAJ-INDEX          TO WS-PYAJ-MAX

           CLOSE ERPYAJ

           IF ERPYAJ-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE ' ERROR ON ERPYAJ CLOSE '
                                       TO WS-ABEND-MESSAGE
              MOVE ERPYAJ-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
              DISPLAY WS-ABEND-MESSAGE '  ' WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           SET MORE-RECORDS            TO TRUE
           PERFORM 0060-READ-CERT      THRU 0060-EXIT
           MOVE CR-ACCT-CONTROL        TO WS-HOLD-CONTROL
           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
                 END-OF-FILE

           PERFORM 0150-UPDATE-DUEP    THRU 0150-EXIT
           PERFORM 0300-ACCT-BREAK     THRU 0300-EXIT
           PERFORM 0310-ST-BREAK       THRU 0310-EXIT
           PERFORM 0320-GRP-BREAK      THRU 0320-EXIT
           PERFORM 0330-CAR-BREAK      THRU 0330-EXIT
           PERFORM 0340-FIN-BREAK      THRU 0340-EXIT

           .

       0050-EXIT.
           EXIT.

       0060-READ-CERT.

           READ CERTS AT END
               SET END-OF-FILE         TO TRUE
           END-READ

           IF NOT END-OF-FILE
              ADD +1                   TO CERT-IN-CNT
              MOVE ' '                 TO WS-GIANT-SW
           END-IF
           .

       0060-EXIT.
           EXIT.

       0080-PROCESS-CERT.

           IF CR-LFTYP NOT = '  ' AND ZEROS
              PERFORM 0207-FIND-LIFE-TYPE
                                       THRU 0207-EXIT
           END-IF
                                       
           IF CR-AHTYP NOT = '  ' AND ZEROS
              PERFORM 0208-FIND-AH-TYPE
                                       THRU 0208-EXIT
           END-IF
                                       
110304     IF (CR-LFTYP = 'MJ' OR 'MS'
               OR 'NJ' OR 'NL'
061112         OR 'PL' OR 'PJ'
               OR 'SJ' OR 'SL')
110304        OR (CR-AHTYP =
071906               'MJ' OR 'MS' OR 'MI' OR 'MU'
                  OR 'NI' OR 'NJ' OR 'NL' OR 'NU'
061112            OR 'PI' OR 'PJ' OR 'PL' OR 'PU'
                  OR 'SI' OR 'SJ' OR 'SL' OR 'SU'
                  OR 'MX' OR 'MY' OR 'MZ')
              IF (CR-ENTRY-DATE = WS-THIS-MONTH-DATE)
062304           OR (CR-LF-CANCEL-EXIT-DATE = WS-THIS-MONTH-DATE)
062304           OR (CR-AH-CANCEL-EXIT-DATE = WS-THIS-MONTH-DATE)              
                 SET THIS-IS-A-GIANT-CERT TO TRUE
              ELSE
                 MOVE 'V'              TO CR-ENTRY-STATUS
              END-IF
           END-IF
           
           IF CR-ACCT-CONTROL NOT = WS-HOLD-CONTROL
              PERFORM 0250-BREAK-RTN   THRU 0250-EXIT
           END-IF

           MOVE CR-ACCT-CONTROL        TO WS-HOLD-CONTROL
           MOVE CR-DT                  TO WS-ISSUE-DATE

           IF (CR-ENTRY-STATUS = '9' OR 'D' OR 'V')
                       OR
061112        ((CR-LFTYP (1:1) NOT = 'M' AND 'N' AND 'P' AND 'S')
061112        AND (CR-AHTYP (1:1) NOT = 'M' AND 'N' AND 'P' AND 'S'))
              CONTINUE
           ELSE
              IF CR-ENTRY-STATUS NOT = '5'
                 PERFORM 0200-CALC-UEP THRU 0200-EXIT
                 PERFORM 0090-FIND-ADJ THRU 0090-EXIT
              END-IF
           END-IF

           IF (WS-EXP-PREM NOT = +0)
              OR (WS-REC-PREM NOT = +0)
              OR (WS-GA-COMM NOT = +0)
              PERFORM 0100-WRITE-EXTRACT
                                       THRU 0100-EXIT
           END-IF

           MOVE +0                     TO WS-EXP-PREM
                                          WS-REC-PREM
                                          WS-GA-COMM
                                          WS-BASE-COMM
                                          WS-REC-BASE-COMM
                                          
           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .

       0080-EXIT.
           EXIT.
           
       0090-FIND-ADJ.
       
           MOVE CR-COMPANY-CD          TO WS-PYAJ-MATCH-KEY (1:1)
           MOVE CR-CARRIER             TO WS-PYAJ-MATCH-KEY (2:1)
           MOVE CR-GROUPING            TO WS-PYAJ-MATCH-KEY (3:6)
           IF CR-REMIT-TO NOT NUMERIC
              MOVE ZEROS               TO CR-REMIT-TO
           END-IF
           IF CR-REMIT-TO = ZEROS
              MOVE 01                  TO CR-REMIT-TO
           END-IF
           MOVE CR-COM-AGT (CR-REMIT-TO)
                                       TO WS-PYAJ-MATCH-KEY (9:10)
           PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
              (WS-A > +10)
              OR (CR-AGT-TYPE (WS-A) = 'D' OR 'C')
           END-PERFORM

           IF WS-A < +11
              MOVE CR-COM-AGT (WS-A)   TO WS-PYAJ-MATCH-KEY (19:10)
           ELSE
              MOVE CR-ACCOUNT          TO WS-PYAJ-MATCH-KEY (19:10)
           END-IF
      *    MOVE +0                     TO WS-DP-ADJ
      *    DISPLAY ' CERT KEY TO MATCH ' WS-PYAJ-MATCH-KEY
           PERFORM VARYING WS-PYAJ-INDEX FROM +1 BY +1 UNTIL
              (WS-PYAJ-INDEX > WS-PYAJ-MAX)
              IF WS-DA-KEY (WS-PYAJ-INDEX) (1:28) = WS-PYAJ-MATCH-KEY
                 COMPUTE WS-DP-ADJ
                    = WS-DP-ADJ + WS-DA-AMOUNT (WS-PYAJ-INDEX)
                 MOVE LOW-VALUES       TO WS-DA-KEY (WS-PYAJ-INDEX)
              END-IF
           END-PERFORM
           
           . 
       0090-EXIT.
           EXIT.


       0100-WRITE-EXTRACT.

           MOVE WS-INIT-EXTRACT        TO EXTRACT-RECORD

           COMPUTE WS-TOT-EXP-PREM (1) = WS-TOT-EXP-PREM (1)
              + WS-EXP-PREM

           COMPUTE WS-TOT-REC-PREM (1) = WS-TOT-REC-PREM (1)
              + WS-REC-PREM

           COMPUTE WS-TOT-BASE-COMM (1) = WS-TOT-BASE-COMM (1)
              + WS-BASE-COMM 

           COMPUTE WS-TOT-GA-COMM (1) = WS-TOT-GA-COMM (1)
              + WS-GA-COMM 

           MOVE CR-CARRIER             TO EXT-CAR
           MOVE CR-GROUPING            TO EXT-GROUP
           MOVE CR-STATE               TO EXT-STATE
           MOVE CR-ACCOUNT             TO EXT-ACCOUNT
           MOVE CR-DT                  TO WS-ISSUE-DATE
           MOVE WS-ISSUE-MMDD (1:2)    TO EXT-EFF-DATE (1:2)
           MOVE '-'                    TO EXT-EFF-DATE (3:1)
                                          EXT-EFF-DATE (6:1)
           MOVE WS-ISSUE-MMDD (3:2)    TO EXT-EFF-DATE (4:2)
           MOVE WS-ISSUE-CCYY          TO EXT-EFF-DATE (7:4)
           MOVE CR-ENTRY-DATE          TO WS-ENTRY-DATE
           MOVE WS-ENTRY-MMDD (1:2)    TO EXT-ENT-DATE (1:2)
           MOVE '-'                    TO EXT-ENT-DATE (3:1)
                                          EXT-ENT-DATE (6:1)
           MOVE WS-ENTRY-MMDD (3:2)    TO EXT-ENT-DATE (4:2)
           MOVE WS-ENTRY-CCYY          TO EXT-ENT-DATE (7:4)
           MOVE RUN-DATE               TO WS-ENTRY-DATE
           MOVE WS-ENTRY-MMDD (1:2)    TO EXT-VAL-DATE (1:2)
           MOVE '-'                    TO EXT-VAL-DATE (3:1)
                                          EXT-VAL-DATE (6:1)
           MOVE WS-ENTRY-MMDD (3:2)    TO EXT-VAL-DATE (4:2)
           MOVE WS-ENTRY-CCYY          TO EXT-VAL-DATE (7:4)
           
           MOVE WS-EARN-TERM           TO EXT-EARN-TERM
           MOVE CR-CERT-NO             TO EXT-CERT-NO
           MOVE CR-LFTYP               TO EXT-LF-BEN-CODE
           MOVE CR-AHTYP               TO EXT-AH-BEN-CODE
           MOVE CR-LF-TERM             TO EXT-LF-TERM
           MOVE CR-AH-TERM             TO EXT-AH-TERM
           MOVE CR-LF-EXPIRE-DATE      TO WS-EXPIRE-DATE
           MOVE WS-EXPIRE-MMDD (1:2)   TO EXT-LF-EXP-DATE (1:2)
           MOVE '-'                    TO EXT-LF-EXP-DATE (3:1)
                                          EXT-LF-EXP-DATE (6:1)
           MOVE WS-EXPIRE-MMDD (3:2)   TO EXT-LF-EXP-DATE (4:2)
           MOVE WS-EXPIRE-CCYY         TO EXT-LF-EXP-DATE (7:4)

           MOVE CR-AH-EXPIRE-DATE      TO WS-EXPIRE-DATE
           MOVE WS-EXPIRE-MMDD (1:2)   TO EXT-AH-EXP-DATE (1:2)
           MOVE '-'                    TO EXT-AH-EXP-DATE (3:1)
                                          EXT-AH-EXP-DATE (6:1)
           MOVE WS-EXPIRE-MMDD (3:2)   TO EXT-AH-EXP-DATE (4:2)
           MOVE WS-EXPIRE-CCYY         TO EXT-AH-EXP-DATE (7:4)
           MOVE WS-EXP-PREM            TO EXT-EXP-PREM
           MOVE WS-REC-PREM            TO EXT-REC-PREM
           MOVE WS-BASE-COMM           TO EXT-BASE-COMM

      *    IF WS-BASE-COMM > ZEROS
      *       MOVE WS-BASE-COMM        TO EXT-BASE-COMM
      *    ELSE
      *       MOVE WS-REC-BASE-COMM    TO EXT-BASE-COMM
      *    END-IF

           MOVE WS-GA-COMM             TO EXT-GA-COMM
           
           WRITE EXTRACT-RECORD
           PERFORM 0125-ACCUM-DUEP     THRU 0125-EXIT
           
           .

       0100-EXIT.
           EXIT.
       0125-ACCUM-DUEP.

           IF (WS-DUEP-CONTROL NOT = LOW-VALUES)
              AND (WS-DUEP-CONTROL NOT = CR-ACCT-CONTROL)
              PERFORM 0150-UPDATE-DUEP  THRU 0150-EXIT
              MOVE +0                  TO WS-DP-REC-PREM
                                          WS-DP-EXP-PREM
                                          WS-DP-GA-COMM
                                          WS-DP-BASE-COMM
                                          WS-DP-ADJ
           END-IF
           
           MOVE CR-ACCT-CONTROL        TO WS-DUEP-CONTROL

           COMPUTE WS-DP-REC-PREM = WS-DP-REC-PREM + WS-REC-PREM
           COMPUTE WS-DP-EXP-PREM = WS-DP-EXP-PREM + WS-EXP-PREM
           COMPUTE WS-DP-GA-COMM = WS-DP-GA-COMM + WS-GA-COMM
           COMPUTE WS-DP-BASE-COMM = WS-DP-BASE-COMM + WS-BASE-COMM
           
           
       
           .
       0125-EXIT.
           EXIT.           

       0150-UPDATE-DUEP.

           MOVE DTE-CLASIC-COMPANY-CD  TO DP-COMPANY-CD
           MOVE WS-DUEP-CONTROL        TO DP-ACCOUNT-CONTROL
           MOVE LOW-VALUES             TO DP-KEY-FILLER
           READ ERDUEP
           IF ERDUEP-FILE-STATUS = '00'
      *       COMPUTE WS-DP-REC-PREM = WS-DP-REC-PREM
              MOVE WS-DP-REC-PREM      TO DP-REC-PREM
              MOVE WS-DP-EXP-PREM      TO DP-EXP-PREM
              MOVE WS-DP-GA-COMM       TO DP-GA-COMM
              MOVE WS-DP-BASE-COMM     TO DP-BASE-COMM
              MOVE WS-DP-ADJ           TO DP-ADJUSTMENTS
              IF DP-LAST-MONTH-END-DT NOT = BIN-RUN-DATE
      *  NEED TO THINK ABOUT THE AGEING STUFF RIGHT HERE        
                 MOVE DP-END-BAL       TO DP-BAL-FWD
                 MOVE BIN-RUN-DATE     TO DP-LAST-MONTH-END-DT
              END-IF
              COMPUTE DP-END-BAL = DP-BAL-FWD + DP-EXP-PREM
                 - DP-REC-PREM + DP-ADJUSTMENTS
              REWRITE DUE-PREMIUM-RECORD
           ELSE
              IF ERDUEP-FILE-STATUS = '23'
                 MOVE DTE-CLASIC-COMPANY-CD
                                       TO DP-COMPANY-CD
                 MOVE WS-DUEP-CONTROL  TO DP-ACCOUNT-CONTROL
                 MOVE +0               TO DP-BAL-FWD
                                          DP-ADJUSTMENTS
                                          DP-END-BAL
                 MOVE WS-DP-REC-PREM   TO DP-REC-PREM
                 MOVE WS-DP-EXP-PREM   TO DP-EXP-PREM
                 MOVE WS-DP-GA-COMM    TO DP-GA-COMM
                 MOVE WS-DP-BASE-COMM  TO DP-BASE-COMM
                 MOVE WS-DP-ADJ        TO DP-ADJUSTMENTS
                 COMPUTE DP-END-BAL = DP-BAL-FWD + DP-EXP-PREM
                    - DP-REC-PREM + DP-ADJUSTMENTS
                 MOVE BIN-RUN-DATE     TO DP-LAST-MONTH-END-DT
                 WRITE DUE-PREMIUM-RECORD
              END-IF
           END-IF
                 
        
           .
       0150-EXIT.
           EXIT.
           

       0200-CALC-UEP.

           MOVE LOW-VALUES             TO WS-BIN-AH-END-DATE
                                          WS-BIN-LF-END-DATE
                                          
           IF CR-STATE = STATE-SUB (CLAS-INDEXS)
              CONTINUE
           ELSE
              PERFORM VARYING CLAS-INDEXS FROM +1 BY +1 UNTIL
                  (CLAS-INDEXS = CLAS-MAXS) OR
                  CR-STATE = STATE-SUB (CLAS-INDEXS)
              END-PERFORM
              IF CR-STATE = STATE-SUB (CLAS-INDEXS)
                 CONTINUE
              ELSE
                 DISPLAY 'STATE ' CR-STATE ' NOT IN TABLE'
                 MOVE 0302 TO WS-RETURN-CODE
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-BIN-DATE-1          TO CP-CERT-EFF-DT
                                          WS-BIN-CR-DT

           IF CR-LOAN-1ST-PMT-DT NOT NUMERIC
              MOVE ZEROS               TO CR-LOAN-1ST-PMT-DT
           END-IF

           MOVE LOW-VALUES             TO CP-FIRST-PAY-DATE

           IF CR-LOAN-1ST-PMT-DT NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO DC-GREG-DATE-1-YMD
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              MOVE '3'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1   TO CP-FIRST-PAY-DATE
                                         WS-BIN-1ST-DT
              ELSE
                 MOVE ZEROS           TO CR-LOAN-1ST-PMT-DT
              END-IF
            END-IF

           IF CP-FIRST-PAY-DATE  LESS THAN  CP-CERT-EFF-DT
              MOVE ZEROS  TO  CR-LOAN-1ST-PMT-DT
           END-IF

           IF CR-LOAN-1ST-PMT-DT = ZEROS
              MOVE CP-CERT-EFF-DT      TO DC-BIN-DATE-1
              MOVE +1                  TO DC-ELAPSED-MONTHS
              MOVE ZEROS               TO DC-ELAPSED-DAYS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              MOVE DC-BIN-DATE-2       TO CP-FIRST-PAY-DATE
                                          WS-BIN-1ST-DT
              MOVE DC-GREG-DATE-1-YMD  TO CR-LOAN-1ST-PMT-DT
           END-IF

           MOVE WS-BIN-CR-DT           TO DC-BIN-DATE-1
           MOVE WS-BIN-1ST-DT          TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           MOVE ZEROS                  TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-ELAPSED-DAYS     TO WS-DAYS-TO-1ST
           ELSE
              MOVE +30                 TO WS-DAYS-TO-1ST
           END-IF

           MOVE CR-LF-EXPIRE-DATE      TO WS-WORK-DATE

072005     MOVE LOW-VALUES             TO WS-BIN-LF-CAN-DATE
072005     MOVE +0                     TO WS-LF-CAN-TERM
072005     MOVE 'N'                    TO WS-LF-CAN-QUAL-SW
072005     
072005     IF CR-LF-CANC-DT NOT = ZEROS
072005        MOVE CR-LF-CANC-DT       TO DC-GREG-DATE-CYMD
072005        MOVE 'L'                 TO DC-OPTION-CODE
072005        PERFORM 8510-DATE-CONVERSION
072005                                 THRU 8590-EXIT
072005        IF NO-CONVERSION-ERROR
072005           MOVE DC-BIN-DATE-1    TO WS-BIN-LF-CAN-DATE
072005        ELSE
072005           MOVE LOW-VALUES       TO WS-BIN-LF-CAN-DATE
072005        END-IF
072005        MOVE BIN-RUN-DATE        TO DC-BIN-DATE-2
072005        MOVE '1'                 TO DC-OPTION-CODE
072005        MOVE +0                  TO DC-ELAPSED-MONTHS
072005                                    DC-ELAPSED-DAYS
072005        PERFORM 8510-DATE-CONVERSION
072005                                 THRU 8590-EXIT
072005        IF NO-CONVERSION-ERROR
072005           MOVE DC-ELAPSED-MONTHS
072005                                 TO WS-LF-CAN-TERM
                 IF (WS-LF-CAN-TERM > +0)
                    AND (WS-BIN-LF-CAN-DATE > WS-BIN-1ST-DT)
                    SUBTRACT +1        FROM WS-LF-CAN-TERM
                 END-IF
072005           IF (WS-LF-CAN-TERM > +0)
072005              AND (WS-THIS-MONTH-DATE = CR-LF-CANCEL-EXIT-DATE)
072005              SET CREDIT-LF-CANCEL TO TRUE
072005           END-IF
072005        END-IF
072005     END-IF

      *    IF (CR-LF-CANC-DT NOT = ZEROS) AND
      *       (CR-LF-CANC-DT < WS-WORK-DATE)
      *       MOVE CR-LF-CANC-DT           TO WS-WORK-DATE
      *    END-IF

           IF (CR-LF-CANCEL-EXIT-DATE NOT = ZEROS)
              AND (CR-LF-CANCEL-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-LF-CANCEL-EXIT-DATE
                                       TO WS-WORK-DATE
           END-IF

           IF (CR-LF-CLAIM-EXIT-DATE NOT = ZEROS) AND
              (CR-LF-CLAIM-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-LF-CLAIM-EXIT-DATE   TO WS-WORK-DATE
           END-IF

           MOVE WS-WORK-DATE           TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-LF-END-DATE
           ELSE
              DISPLAY ' NO LIFE COVERAGE ?? ' CR-ACCT-CONTROL
                 '   ' CR-CERT-NO
           END-IF

           MOVE CR-AH-EXPIRE-DATE          TO WS-WORK-DATE

072005     MOVE LOW-VALUES             TO WS-BIN-AH-CAN-DATE
072005     MOVE +0                     TO WS-AH-CAN-TERM
072005     MOVE 'N'                    TO WS-AH-CAN-QUAL-SW

072005     IF CR-AH-CANC-DT NOT = ZEROS
072005        MOVE CR-AH-CANC-DT       TO DC-GREG-DATE-CYMD
072005        MOVE 'L'                 TO DC-OPTION-CODE
072005        PERFORM 8510-DATE-CONVERSION
072005                                 THRU 8590-EXIT
072005        IF NO-CONVERSION-ERROR
072005           MOVE DC-BIN-DATE-1    TO WS-BIN-AH-CAN-DATE
072005        ELSE
072005           MOVE LOW-VALUES       TO WS-BIN-AH-CAN-DATE
072005        END-IF
072005        MOVE BIN-RUN-DATE        TO DC-BIN-DATE-2
072005        MOVE '1'                 TO DC-OPTION-CODE
072005        MOVE +0                  TO DC-ELAPSED-MONTHS
072005                                    DC-ELAPSED-DAYS
072005        PERFORM 8510-DATE-CONVERSION
072005                                 THRU 8590-EXIT
072005        IF NO-CONVERSION-ERROR
072005           MOVE DC-ELAPSED-MONTHS
072005                                 TO WS-AH-CAN-TERM
                 IF (WS-AH-CAN-TERM > +0)
                    AND (WS-BIN-AH-CAN-DATE > WS-BIN-1ST-DT)
                    SUBTRACT +1        FROM WS-AH-CAN-TERM
                 END-IF
072005           IF (WS-AH-CAN-TERM > +0)
072005              AND (WS-THIS-MONTH-DATE = CR-AH-CANCEL-EXIT-DATE)
072005              SET CREDIT-AH-CANCEL TO TRUE
072005           END-IF
072005        END-IF
072005     END-IF

      *    IF (CR-AH-CANC-DT NOT = ZEROS) AND
      *       (CR-AH-CANC-DT < WS-WORK-DATE)
      *       MOVE CR-AH-CANC-DT           TO WS-WORK-DATE
      *    END-IF

           IF (CR-AH-CANCEL-EXIT-DATE NOT = ZEROS)
              AND (CR-AH-CANCEL-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-AH-CANCEL-EXIT-DATE
                                       TO WS-WORK-DATE
           END-IF

           IF (CR-AH-SETTLEMENT-EXIT-DATE NOT = ZEROS) AND
              (CR-AH-SETTLEMENT-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-AH-SETTLEMENT-EXIT-DATE
                                           TO WS-WORK-DATE
           END-IF

           MOVE WS-WORK-DATE           TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-AH-END-DATE
           ELSE
              DISPLAY ' NO AH COVERAGE ' CR-ACCT-CONTROL
                 '   ' CR-CERT-NO
           END-IF

           MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE
           MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV
           MOVE '3'                        TO CP-PROCESS-TYPE
           MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD
           MOVE DTE-CLIENT                 TO CP-COMPANY-ID
           MOVE SPACES                     TO CP-ACCT-FLD-5
           MOVE DTE-REM-TRM                TO CP-REM-TERM-METHOD

      *    IF (WS-BIN-LF-END-DATE NOT > BIN-RUN-DATE)
      *       AND (NOT THIS-IS-A-GIANT-CERT)
      *       AND (NOT CREDIT-LF-CANCEL)
      *       MOVE ZEROS               TO CR-LFPRM
      *                                   CR-LFTYP
      *                                   CR-LF-TERM
      *                                   CR-LFPRM-RATE
      *    END-IF

           IF (WS-BIN-LF-END-DATE NOT > WS-BIN-LAST-MONTH-DT)
              AND (NOT THIS-IS-A-GIANT-CERT)
              MOVE ZEROS               TO CR-LFPRM
                                          CR-LFTYP
                                          CR-LF-TERM
                                          CR-LFPRM-RATE
           END-IF

      *    IF (WS-BIN-AH-END-DATE NOT > BIN-RUN-DATE)
      *       AND (NOT THIS-IS-A-GIANT-CERT)
      *       AND (NOT CREDIT-AH-CANCEL)
      *       MOVE ZEROS               TO CR-AHPRM
      *                                   CR-AHTYP
      *                                   CR-AH-TERM
      *                                   CR-AHPRM-RATE
      *    END-IF

           IF (WS-BIN-AH-END-DATE NOT > WS-BIN-LAST-MONTH-DT)
              AND (NOT THIS-IS-A-GIANT-CERT)
              MOVE ZEROS               TO CR-AHPRM
                                          CR-AHTYP
                                          CR-AH-TERM
                                          CR-AHPRM-RATE
           END-IF

           IF CR-LF-TERM > CR-AH-TERM
              MOVE CR-LF-TERM          TO WS-WORK-TERM
           ELSE
              MOVE CR-AH-TERM          TO WS-WORK-TERM
           END-IF
           
           IF (CR-LFTYP = '00' OR '  ')
              AND (CR-AHTYP = '00' OR '  ')
              CONTINUE
           ELSE
              PERFORM 0205-CALC-REM-TERMS
                                       THRU 0205-EXIT
              IF LF-REM-TERM > AH-REM-TERM
                 MOVE LF-REM-TERM      TO REM-TERM
              ELSE
                 MOVE AH-REM-TERM      TO REM-TERM
              END-IF

              PERFORM 0210-CALC-UEP    THRU 0210-EXIT
           END-IF
              
           
           .

       0200-EXIT.
           EXIT.

       0205-CALC-REM-TERMS.
       
           MOVE +0                     TO LF-REM-TERM
                                          AH-REM-TERM
                                          REM-TERM

           IF CR-LFTYP NOT = '  ' AND ZEROS
              PERFORM 0207-FIND-LIFE-TYPE
                                       THRU 0207-EXIT
              IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'
                 CONTINUE
              ELSE
                 PERFORM 0220-CALC-LF-REM-TERM
                                       THRU 0220-EXIT
              END-IF
           END-IF
                                       
           IF CR-AHTYP NOT = '  ' AND ZEROS
              PERFORM 0208-FIND-AH-TYPE
                                       THRU 0208-EXIT
              IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'
                 CONTINUE
              ELSE
                 PERFORM 0225-CALC-AH-REM-TERM
                                       THRU 0225-EXIT
              END-IF
           END-IF
                                       
                     
           .
       0205-EXIT.
          EXIT.
          
       0207-FIND-LIFE-TYPE.
       
           IF CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL)
              CONTINUE
           ELSE
              PERFORM VARYING CLAS-INDEXL FROM +1 BY +1 UNTIL
               (CLAS-INDEXL = CLAS-MAXL) OR
               (CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL))
              END-PERFORM
              IF CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL)
                 CONTINUE
              ELSE
                 DISPLAY 'LIFE BENEFIT ' CR-LFTYP ' NOT IN TABLE'
                 DISPLAY 'RETURN CODE - 0401'
                 MOVE 0401 TO WS-RETURN-CODE
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0207-EXIT.
           EXIT.

       0208-FIND-AH-TYPE.
       
           IF CR-AHTYP = CLAS-I-BEN (CLAS-INDEXA)
              CONTINUE
           ELSE
              PERFORM VARYING CLAS-INDEXA FROM +1 BY +1 UNTIL
                  (CLAS-INDEXA = CLAS-MAXA) OR
                  (CR-AHTYP = CLAS-I-BEN (CLAS-INDEXA))
              END-PERFORM
              IF CR-AHTYP = CLAS-I-BEN (CLAS-INDEXA)
                 CONTINUE
              ELSE
                 DISPLAY ' AH  BENEFIT ' CR-AHTYP ' NOT IN TABLE'
                 DISPLAY 'RETURN CODE - 0401'
                 MOVE 0401 TO WS-RETURN-CODE
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0208-EXIT.
           EXIT.

       0210-CALC-UEP.

           MOVE ZEROS                  TO WS-REM-AMT
           IF THIS-IS-A-GIANT-CERT
              PERFORM 0215-CALC-RECEIVE 
                                       THRU 0215-EXIT
           ELSE
              MOVE +0            TO WS-BASE-COMM
                                    WS-GA-COMM
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 (X1 > +10)
                 IF CR-AGT-TYPE (X1) = 'C' OR 'D'
                    COMPUTE WS-BASE-COMM = WS-BASE-COMM
                    + (CR-LFPRM * CR-LCOM-L (X1))
                    + (CR-LFPRM-ALT * CR-LCOM-L (X1))
                    + (CR-AHPRM * CR-LCOM-AH (X1))
                 END-IF
052814           IF CR-AGT-TYPE (X1) = 'O' OR 'P' or 'S'
                    COMPUTE WS-GA-COMM = WS-GA-COMM +
                       (CR-LFPRM * CR-LCOM-L (X1)) +
                       (CR-LFPRM-ALT * CR-LCOM-L (X1)) +
                       (CR-AHPRM * CR-LCOM-AH (X1))
                 END-IF
              END-PERFORM
              IF (CR-LFTYP (1:1) = 'M')
                 OR (CR-AHTYP (1:1) = 'M')
      *          COMPUTE WS-EARN-TERM = CR-LF-TERM - LF-REM-TRM2
                 COMPUTE WS-EARN-TERM = WS-WORK-TERM - REM-TERM
                 COMPUTE WS-EXP-PREM = (CR-LFPRM + CR-AHPRM)
                 IF (CREDIT-LF-CANCEL)
                    OR (CREDIT-AH-CANCEL)
                    PERFORM 0212-CALC-CANCEL-CREDIT
                                       THRU 0212-EXIT
                 ELSE
                   IF RUN-DATE = CR-ENTRY-DATE
                    COMPUTE WS-EXP-PREM = WS-EXP-PREM
                       * WS-EARN-TERM
                    COMPUTE WS-BASE-COMM = WS-BASE-COMM
                       * WS-EARN-TERM
                    COMPUTE WS-GA-COMM   = WS-GA-COMM  
                       * WS-EARN-TERM
                   END-IF
                   COMPUTE WS-EXP-PREM = 
                    (WS-EXP-PREM - WS-BASE-COMM)
                 END-IF
              ELSE
                 IF (CR-LFTYP (1:1) = 'S')
                    OR (CR-AHTYP (1:1) = 'S')
                    PERFORM 0240-CALC-SRP
                                       THRU 0240-EXIT
                 ELSE
                    PERFORM 0230-CALC-UEP
                                       THRU 0230-EXIT
                 END-IF
              END-IF
           END-IF

           .

       0210-EXIT.
           EXIT.

       0212-CALC-CANCEL-CREDIT.
       
           MOVE ZEROS                  TO WS-EXP-PREM
                                          WS-LF-EXP-PREM
                                          WS-AH-EXP-PREM
                                          WS-LF-BASE-COMM
                                          WS-AH-BASE-COMM
                                          WS-LF-GA-COMM
                                          WS-AH-GA-COMM
                                          

           IF CREDIT-LF-CANCEL
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 (X1 > +10)
                 IF CR-AGT-TYPE (X1) = 'C' OR 'D'
                    COMPUTE WS-LF-BASE-COMM = WS-LF-BASE-COMM
                    + (CR-LFPRM * CR-LCOM-L (X1))
                    + (CR-LFPRM-ALT * CR-LCOM-L (X1))
                 END-IF
052814           IF CR-AGT-TYPE (X1) = 'O' OR 'P' or 'S'
                    COMPUTE WS-LF-GA-COMM = WS-LF-GA-COMM +
                       (CR-LFPRM * CR-LCOM-L (X1)) +
                       (CR-LFPRM-ALT * CR-LCOM-L (X1))
                 END-IF
              END-PERFORM
              COMPUTE WS-LF-EXP-PREM = CR-LFPRM * WS-LF-CAN-TERM
              COMPUTE WS-LF-BASE-COMM = WS-LF-BASE-COMM * WS-LF-CAN-TERM
              COMPUTE WS-LF-GA-COMM = WS-LF-GA-COMM * WS-LF-CAN-TERM
           END-IF

           IF CREDIT-AH-CANCEL
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 (X1 > +10)
                 IF CR-AGT-TYPE (X1) = 'C' OR 'D'
                    COMPUTE WS-AH-BASE-COMM = WS-AH-BASE-COMM
                    + (CR-AHPRM * CR-LCOM-AH (X1))
                 END-IF
052814           IF CR-AGT-TYPE (X1) = 'O' OR 'P' or 'S'
                    COMPUTE WS-AH-GA-COMM = WS-AH-GA-COMM +
                       (CR-AHPRM * CR-LCOM-AH (X1))
                 END-IF
              END-PERFORM
              COMPUTE WS-AH-EXP-PREM = CR-AHPRM * WS-AH-CAN-TERM
              COMPUTE WS-AH-BASE-COMM = WS-AH-BASE-COMM * WS-AH-CAN-TERM
              COMPUTE WS-AH-GA-COMM = WS-AH-GA-COMM * WS-AH-CAN-TERM
           END-IF

           COMPUTE WS-EXP-PREM = WS-LF-EXP-PREM + WS-AH-EXP-PREM
           COMPUTE WS-BASE-COMM = WS-LF-BASE-COMM + WS-AH-BASE-COMM
           COMPUTE WS-GA-COMM = WS-LF-GA-COMM + WS-AH-GA-COMM
           COMPUTE WS-EXP-PREM = (WS-EXP-PREM - WS-BASE-COMM) * -1
           
           .
       0212-EXIT.
           EXIT.
           

       0215-CALC-RECEIVE.    

           MOVE +0                     TO WS-GA-COMM
                                          WS-REC-BASE-COMM

           IF WS-THIS-MONTH-DATE = CR-ENTRY-DATE
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 (X1 > +10)
                 IF CR-AGT-TYPE (X1) = 'C' OR 'D'
                    COMPUTE WS-REC-BASE-COMM = WS-REC-BASE-COMM +
                       (CR-LFPRM * CR-LCOM-L (X1)) +
                       (CR-LFPRM-ALT * CR-LCOM-L (X1)) +
                       (CR-AHPRM * CR-LCOM-AH (X1))
                 END-IF
              END-PERFORM
              COMPUTE WS-REC-PREM = (CR-LFPRM + CR-LFPRM-ALT + CR-AHPRM)
           END-IF

           IF WS-THIS-MONTH-DATE = CR-LF-CANCEL-EXIT-DATE
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 (X1 > +10)
                 IF CR-AGT-TYPE (X1) = 'C' OR 'D'
                    COMPUTE WS-REC-BASE-COMM = WS-REC-BASE-COMM -
                       (CR-LFRFND * CR-LCOM-L (X1))
                 END-IF
              END-PERFORM
              COMPUTE WS-REC-PREM = WS-REC-PREM - CR-LFRFND
           END-IF

           IF WS-THIS-MONTH-DATE = CR-AH-CANCEL-EXIT-DATE
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 (X1 > +10)
                 IF CR-AGT-TYPE (X1) = 'C' OR 'D'
                    COMPUTE WS-REC-BASE-COMM = WS-REC-BASE-COMM -
                       (CR-AHRFND * CR-LCOM-AH (X1))
                 END-IF
              END-PERFORM
              COMPUTE WS-REC-PREM = WS-REC-PREM - CR-AHRFND
           END-IF

           COMPUTE WS-REC-PREM = WS-REC-PREM - WS-REC-BASE-COMM
      *    COMPUTE WS-REC-PREM = (CR-LFPRM + CR-LFPRM-ALT + CR-AHPRM)
      *       - WS-REC-BASE-COMM
              
           .

       0215-EXIT.
           EXIT.

       0220-CALC-LF-REM-TERM.

           MOVE ZEROS                  TO LF-REM-TERM
           MOVE WS-BIN-1ST-DT          TO DC-BIN-DATE-1
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-2

           IF DC-BIN-DATE-2 NOT > DC-BIN-DATE-1
              MOVE ZEROS               TO WS-LF-EARN-TERM
           ELSE
              MOVE '1'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-ELAPSED-MONTHS
                                       TO WS-LF-EARN-TERM
                 IF DC-ODD-DAYS-OVER > +0
                    ADD +1             TO WS-LF-EARN-TERM
                 END-IF
              ELSE
                 DISPLAY ' PROBLEM WITH LIFE EARN TERM '
                    CR-ACCT-CONTROL '   ' CR-CERT-NO
              END-IF
           END-IF

           IF CR-LF-TERM NOT = ZEROS
              COMPUTE LF-REM-TERM = CR-LF-TERM - WS-LF-EARN-TERM
           END-IF

      *    IF WS-BIN-LF-END-DATE = LOW-VALUES
      *       MOVE ZEROS               TO LF-REM-TERM
      *    ELSE
      *       MOVE BIN-RUN-DATE        TO DC-BIN-DATE-1
      *       MOVE WS-BIN-LF-END-DATE  TO DC-BIN-DATE-2
      *       MOVE '1'                 TO DC-OPTION-CODE
      *       IF DC-BIN-DATE-2 NOT > DC-BIN-DATE-1
      *          MOVE +1               TO LF-REM-TERM
      *       ELSE
      *          PERFORM 8510-DATE-CONVERSION
      *                                THRU 8590-EXIT
      *          IF NO-CONVERSION-ERROR
      *             MOVE DC-ELAPSED-MONTHS
      *                                TO LF-REM-TERM
      *             IF DC-ODD-DAYS-OVER > +0
      *                ADD +1          TO LF-REM-TERM
      *             END-IF
      *          ELSE
      *             DISPLAY ' ERROR WITH LIFE REM TERM '
      *               CR-ACCT-CONTROL '  ' CR-CERT-NO
      *             MOVE ZEROS         TO LF-REM-TERM
      *          END-IF
      *       END-IF
      *    END-IF

      *    IF LF-REM-TERM > CR-LF-TERM
      *       MOVE CR-LF-TERM          TO LF-REM-TERM
      *       DISPLAY ' LF REM TERM > LF TERM ' CR-ACCT-CONTROL
      *          '   ' CR-CERT-NO
      *    END-IF

      *    MOVE BIN-RUN-DATE           TO CP-VALUATION-DT

      *    MOVE CLAS-I-RL-AH (CLAS-INDEXL)
      *                                TO CP-BENEFIT-TYPE
      *    MOVE CLAS-I-BAL (CLAS-INDEXL)
      *                                TO CP-SPECIAL-CALC-CD
      *    MOVE CR-LF-TERM             TO CP-ORIGINAL-TERM
      *                                   CP-LOAN-TERM

      *    IF ((CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L') AND
      *       CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT EQUAL 'L')
      *       ADD +1                   TO CP-ORIGINAL-TERM
      *                                   CP-LOAN-TERM
      *    END-IF

      *    IF CP-TERM-IS-DAYS
      *       MOVE CR-LF-TERM-IN-DAYS  TO CP-TERM-OR-EXT-DAYS
      *    ELSE
      *       MOVE ZEROS               TO CP-TERM-OR-EXT-DAYS
      *    END-IF

      *    MOVE DTE-REM-TRM-CALC-OPTION
      *    MOVE '3'                    TO CP-REM-TRM-CALC-OPTION

      *    PERFORM 0510-GET-REMAINING-TERM
      *                                THRU 0510-EXIT

      *    IF ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND
      *         CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L')
      *       MOVE CP-REMAINING-TERM-2 TO LF-BAL-REMTERM
      *       COMPUTE CP-REMAINING-TERM-1 =
      *                      CP-REMAINING-TERM-1 - 1
      *       COMPUTE CP-REMAINING-TERM-2 =
      *                      CP-REMAINING-TERM-2 - 1
      *    END-IF

      *    IF CP-REMAINING-TERM-1 NEGATIVE
      *        MOVE ZEROS              TO CP-REMAINING-TERM-1
      *    END-IF

      *    IF CP-REMAINING-TERM-2 NEGATIVE
      *        MOVE ZEROS              TO CP-REMAINING-TERM-2
      *    END-IF

      *    MOVE CP-REMAINING-TERM-1    TO LF-REM-TRM1
      *    MOVE CP-REMAINING-TERM-2    TO LF-REM-TRM2
      *    DISPLAY ' LF CERT ' CR-CERT-NO ' TERM ' CR-LF-TERM
      *       ' REM2 ' LF-REM-TRM2
              
           .

       0220-EXIT.
           EXIT.

       0225-CALC-AH-REM-TERM.

           MOVE +0                     TO AH-REM-TERM
           MOVE WS-BIN-1ST-DT          TO DC-BIN-DATE-1
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-2

           IF DC-BIN-DATE-2 NOT > DC-BIN-DATE-1
              MOVE ZEROS               TO WS-AH-EARN-TERM
           ELSE
              MOVE '1'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-ELAPSED-MONTHS
                                       TO WS-AH-EARN-TERM
                 IF DC-ODD-DAYS-OVER > +0
                    ADD +1             TO WS-AH-EARN-TERM
                 END-IF
              ELSE
                 DISPLAY ' PROBLEM WITH  AH  EARN TERM '
                    CR-ACCT-CONTROL '   ' CR-CERT-NO
              END-IF
           END-IF

           IF CR-AH-TERM NOT = ZEROS
              COMPUTE AH-REM-TERM = CR-AH-TERM - WS-AH-EARN-TERM
           END-IF

      *    IF WS-BIN-AH-END-DATE = LOW-VALUES
      *       MOVE ZEROS               TO AH-REM-TERM
      *    ELSE
      *       MOVE BIN-RUN-DATE        TO DC-BIN-DATE-1
      *       MOVE WS-BIN-AH-END-DATE  TO DC-BIN-DATE-2
      *       IF DC-BIN-DATE-2 NOT > DC-BIN-DATE-1
      *          MOVE +1               TO AH-REM-TERM
      *       ELSE
      *          MOVE '1'              TO DC-OPTION-CODE
      *          PERFORM 8510-DATE-CONVERSION
      *                                THRU 8590-EXIT
      *          IF NO-CONVERSION-ERROR
      *             MOVE DC-ELAPSED-MONTHS
      *                                TO AH-REM-TERM
      *             IF DC-ODD-DAYS-OVER > +0
      *                ADD +1          TO AH-REM-TERM
      *             END-IF
      *          ELSE
      *             DISPLAY ' ERROR WITH  AH  REM TERM '
      *               CR-ACCT-CONTROL '  ' CR-CERT-NO
      *             MOVE ZEROS         TO AH-REM-TERM
      *          END-IF
      *       END-IF
      *    END-IF

      *    IF AH-REM-TERM > CR-AH-TERM
      *       MOVE CR-AH-TERM          TO AH-REM-TERM
      *       DISPLAY ' AH REM TERM > AH TERM ' CR-ACCT-CONTROL
      *          '   ' CR-CERT-NO
      *    END-IF

      *    MOVE BIN-RUN-DATE           TO CP-VALUATION-DT

      *    MOVE CLAS-I-RL-AH (CLAS-INDEXA)
      *                                TO CP-BENEFIT-TYPE
      *    MOVE CLAS-I-BAL (CLAS-INDEXA)
      *                                TO CP-SPECIAL-CALC-CD
      *    MOVE CR-AH-TERM             TO CP-ORIGINAL-TERM
      *                                   CP-LOAN-TERM

      *    MOVE DTE-REM-TRM-CALC-OPTION
      *    MOVE '3'                    TO CP-REM-TRM-CALC-OPTION

      *    PERFORM 0510-GET-REMAINING-TERM
      *                                THRU 0510-EXIT

      *    IF CP-REMAINING-TERM-1 NEGATIVE
      *        MOVE ZEROS              TO CP-REMAINING-TERM-1
      *    END-IF

      *    IF CP-REMAINING-TERM-2 NEGATIVE
      *        MOVE ZEROS              TO CP-REMAINING-TERM-2
      *    END-IF

      *    MOVE CP-REMAINING-TERM-1    TO AH-REM-TRM1
      *    MOVE CP-REMAINING-TERM-2    TO AH-REM-TRM2
      *    DISPLAY ' AH CERT ' CR-CERT-NO ' TERM ' CR-AH-TERM
      *       ' REM2 ' AH-REM-TRM2
      *       
           .

       0225-EXIT.
           EXIT.

       0230-CALC-UEP.
           
      *     DISPLAY ' MADE IT TO 0230 ' CR-CERT-NO
           
           COMPUTE WS-EARN-TERM = WS-WORK-TERM - REM-TERM

           MOVE WS-EARN-TERM           TO WS-DISPLAY-FLD
           IF CR-CERT-NO = '0004052380 '
              DISPLAY ' EARN TRM ' WS-DISPLAY-FLD
           END-IF
           IF CR-LOAN-TERM NOT NUMERIC
              MOVE CR-LF-TERM          TO CR-LOAN-TERM
           END-IF
           IF CR-LOAN-TERM = ZEROS
              MOVE CR-LF-TERM          TO CR-LOAN-TERM
           END-IF
           IF CR-LOAN-TERM = ZEROS
              MOVE CR-AH-TERM          TO CR-LOAN-TERM
           END-IF
      *    COMPUTE WS-I = CR-APR / +1200

      *    MOVE WS-I                   TO WS-DISPLAY-FLD
      *    DISPLAY ' WS I ' WS-DISPLAY-FLD
      *    
      *    COMPUTE WS-J = WS-I + ((CR-LFPRM-RATE / +1000)
      *       + (CR-AHPRM-RATE / +1000))

      *    MOVE WS-J                   TO WS-DISPLAY-FLD
      *    DISPLAY ' WS J ' WS-DISPLAY-FLD

      *    COMPUTE WS-VM = (1 + WS-J) ** (WS-WORK-TERM * -1)

      *    MOVE WS-VM                  TO WS-DISPLAY-FLD
      *    DISPLAY ' WS VM ' WS-DISPLAY-FLD
      *    
      *    COMPUTE WS-ANGLEMJ = (1 - ((1 + WS-J)
      *       ** (WS-WORK-TERM * -1))) / WS-J

      *    MOVE WS-ANGLEMJ             TO WS-DISPLAY-FLD
      *    DISPLAY ' WS ANGLEMJ ' WS-DISPLAY-FLD
      *    COMPUTE WS-ANGLEN = (1 - ((1 + WS-I)
      *       ** (CR-LOAN-TERM * -1))) / WS-I

      *    MOVE WS-ANGLEN              TO WS-DISPLAY-FLD
      *    DISPLAY ' WS ANGLEN  ' WS-DISPLAY-FLD
      *    COMPUTE WS-ANGLEM = (1 - ((1 + WS-I)
      *       ** (WS-WORK-TERM * -1))) / WS-I

      *    MOVE WS-ANGLEM              TO WS-DISPLAY-FLD
      *    DISPLAY ' WS ANGLEM  ' WS-DISPLAY-FLD
      *    COMPUTE WS-ANGLEN-M = (1 - ((1 + WS-I)
      *       ** ((CR-LOAN-TERM - WS-WORK-TERM) * -1))) / WS-I

      *    MOVE WS-ANGLEN-M            TO WS-DISPLAY-FLD
      *    DISPLAY ' WS ANGLEN - M ' WS-DISPLAY-FLD

      *    IF CR-LFAMT = ZEROS
      *       COMPUTE CR-LFAMT = CR-AHAMT * WS-WORK-TERM
      *    END-IF

      *    COMPUTE WS-P = CR-LFAMT / WS-ANGLEN
      *    MOVE WS-P                   TO WS-DISPLAY-FLD
      *    DISPLAY ' WS P       ' WS-DISPLAY-FLD
      *    COMPUTE WS-B = WS-P * WS-ANGLEN-M
      *    MOVE WS-B                   TO WS-DISPLAY-FLD
      *    DISPLAY ' WS B       ' WS-DISPLAY-FLD
      *    COMPUTE WS-R = (CR-LFAMT - (WS-B * WS-VM))
      *       / WS-ANGLEMJ           
      *    MOVE WS-R                   TO WS-DISPLAY-FLD
      *    DISPLAY ' WS R       ' WS-DISPLAY-FLD

      *    MOVE CR-LFAMT               TO WS-AMT

      *    MOVE CR-LFPRM-RATE          TO WS-DISPLAY-FLD
      *    DISPLAY ' LF RATE ' WS-DISPLAY-FLD
      *    MOVE CR-AHPRM-RATE          TO WS-DISPLAY-FLD
      *    DISPLAY ' AH RATE ' WS-DISPLAY-FLD

      *    MOVE +0                     TO WS-LF-PRM
      *                                   WS-AH-PRM
      *    PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
      *       (WS-A > WS-EARN-TERM)
      *       IF WS-A = +1
      *          COMPUTE WS-I1 ROUNDED = (WS-AMT * (WS-I / +30
      *             * (CR-PMT-EXTENSION-DAYS + +30)) * +100) / +100
      *       ELSE
      *          COMPUTE WS-I1 ROUNDED = (WS-AMT * WS-I * +100)
      *             / +100
      *       END-IF
      *       MOVE WS-I1               TO WS-DISPLAY-FLD
      *       DISPLAY ' ' WS-A '  WS-I1  ' WS-DISPLAY-FLD
      *       DISPLAY ' RUN DATE ' RUN-DATE
      *       DISPLAY ' ENTRY DATE ' CR-ENTRY-DATE
      *       IF RUN-DATE = CR-ENTRY-DATE
      *          COMPUTE WS-LF-PRM = WS-LF-PRM
      *             + (WS-AMT / +1000 * CR-LFPRM-RATE)
      *          COMPUTE WS-AH-PRM = WS-AH-PRM
      *             + (WS-AMT / +1000 * CR-AHPRM-RATE)
      *          COMPUTE WS-BASE-COMM = WS-BASE-COMM * WS-A
      *          COMPUTE WS-GA-COMM   = WS-GA-COMM   * WS-A
      *       ELSE
      *          COMPUTE WS-LF-PRM = WS-AMT / +1000 * CR-LFPRM-RATE
      *          COMPUTE WS-AH-PRM = WS-AMT / +1000 * CR-AHPRM-RATE
      *       END-IF
      *       MOVE WS-AMT              TO WS-DISPLAY-FLD
      *       DISPLAY ' WS-AMT B4    ' WS-AMT
      *       COMPUTE WS-AMT ROUNDED = (.05 + (WS-AMT - (WS-R
      *          - WS-I1 - (WS-LF-PRM + WS-AH-PRM))) * +100)
      *          / +100
      *       MOVE WS-AMT              TO WS-DISPLAY-FLD
      *       DISPLAY ' WS-AMT AFTER ' WS-AMT
      *       IF WS-AMT < +0
      *          COMPUTE WS-P = WS-P + WS-AMT
      *          MOVE ZEROS            TO WS-AMT
      *       END-IF
      *       IF WS-A = CR-LOAN-TERM
      *          IF WS-AMT > ZEROS
      *             IF WS-R > WS-AMT
      *                COMPUTE WS-R = WS-R - WS-AMT
      *                MOVE ZEROS      TO WS-AMT
      *             ELSE
      *                MOVE CR-LFAMT-ALT TO WS-R
      *                MOVE ZEROS      TO WS-AMT
      *             END-IF
      *          END-IF
      *       END-IF
      *    END-PERFORM

      *    COMPUTE WS-EXP-PREM = (WS-LF-PRM + WS-AH-PRM)

      *    DISPLAY ' WS-LF-PRM        = ' WS-LF-PRM
      *    DISPLAY ' WS-AH-PRM        = ' WS-AH-PRM
      *    DISPLAY ' WS-BASE-COMM     = ' WS-BASE-COMM
      *    DISPLAY ' WS-REC-BASE-COMM = ' WS-REC-BASE-COMM
      *       - WS-BASE-COMM


      *    MOVE CR-LFPRM-RATE          TO WS-DISPLAY-FLD
      *    DISPLAY ' LFRATE   ' WS-DISPLAY-FLD
      *    MOVE CR-AHPRM-RATE          TO WS-DISPLAY-FLD
      *    DISPLAY ' AHRATE   ' WS-DISPLAY-FLD
      *    MOVE CR-APR                 TO WS-DISPLAY-FLD
      *    DISPLAY ' APR      ' WS-DISPLAY-FLD
           COMPUTE WS-I = CR-APR / +1200
      *    MOVE WS-I                   TO WS-DISPLAY-FLD
      *    DISPLAY ' WS I     ' WS-DISPLAY-FLD

           COMPUTE WS-J = WS-I + ((CR-LFPRM-RATE / +1000)
              + (CR-AHPRM-RATE / +1000))
      *    MOVE WS-J                   TO WS-DISPLAY-FLD
      *    DISPLAY ' WS J     ' WS-DISPLAY-FLD
           COMPUTE WS-K = WS-I + (CR-LFPRM-RATE / +1000)

      *    I = APR/1200
      *    J = APR/1200 THEN ADJUSTED FOR THE LIFE AND DIS RATE
      *    RA = PAYMENT AMOUNT
      *    WK5-CSL = THE BALANCE (PAY OFF )
      *    WK3 = THE ACCUMULATED PREMIUM
      *    WK2 = (1 + I) ^ (m * -1)
      *
      *
           COMPUTE WS-OD = (1 + (WS-DAYS-TO-1ST * WS-I / 30))
              / (1 + WS-I)

      *    MOVE WS-DAYS-TO-1ST         TO WS-DISPLAY-FLD
      *    DISPLAY ' DYS 2 1ST' WS-DISPLAY-FLD
      *    MOVE WS-OD                  TO WS-DISPLAY-FLD
      *    DISPLAY ' WS OD    ' WS-DISPLAY-FLD
           COMPUTE WS-ANGLEN-M = ((1 - ((1 + WS-I)
              ** ((CR-LOAN-TERM - WS-WORK-TERM) * -1))) / WS-I)
              / WS-OD
           COMPUTE WS-VM = (1 + WS-J) ** (WS-WORK-TERM * -1)
      *    MOVE WS-VM                  TO WS-DISPLAY-FLD
      *    DISPLAY ' WS VM    ' WS-DISPLAY-FLD
           COMPUTE WS-ANGLEN-M = WS-ANGLEN-M * WS-VM
      *    MOVE WS-ANGLEN-M            TO WS-DISPLAY-FLD
      *    DISPLAY ' WS AN-M  ' WS-DISPLAY-FLD
           COMPUTE WS-ANGLEMJ = ((1 - ((1 + WS-J)
              ** (WS-WORK-TERM * -1))) / WS-J) / WS-OD
      *    MOVE WS-ANGLEMJ             TO WS-DISPLAY-FLD
      *    DISPLAY ' WS AMJ   ' WS-DISPLAY-FLD
           COMPUTE WS-ANGLEM = ((1 - ((1 + WS-I)
              ** (WS-WORK-TERM * -1))) / WS-I) / WS-OD
061112     MOVE WS-ANGLEM              TO WS-DISPLAY-FLD
061112*     DISPLAY ' WS Anglem 230 ' WS-DISPLAY-FLD
           COMPUTE WS-ANGLEM-1 = (1 - ((1 + WS-J)
              ** ((WS-WORK-TERM - 1) * -1))) / WS-J


           IF CR-LFAMT = ZEROS
              COMPUTE CR-LFAMT = CR-AHAMT * WS-ANGLEM
           END-IF

           IF CR-LFAMT-ALT > +0
              COMPUTE WS-P = (CR-LFAMT * WS-OD - (CR-LFAMT-ALT *
                 (1 + WS-K) ** (WS-WORK-TERM * -1))) / WS-ANGLEM-1
           ELSE
              COMPUTE WS-P = CR-LFAMT / (WS-ANGLEMJ + WS-ANGLEN-M)
           END-IF
      *    MOVE WS-P                   TO WS-DISPLAY-FLD
      *    DISPLAY ' WS P     ' WS-DISPLAY-FLD
           COMPUTE WS-ANGLEN-M = (1 - ((1 + WS-I)
              ** ((CR-LOAN-TERM - WS-WORK-TERM) * -1))) / WS-I
      *    MOVE WS-ANGLEN-M            TO WS-DISPLAY-FLD
      *    DISPLAY ' WS AN-M  ' WS-DISPLAY-FLD
           MOVE +0                     TO WS-LF-PRM
                                          WS-AH-PRM
                                          WS-LF-REF
                                          WS-AH-REF
                                          WS-LF-MIN-TERM
                                          WS-LF-MAX-TERM
                                          WS-AH-MIN-TERM
                                          WS-AH-MAX-TERM
           IF WS-LF-CAN-TERM > +0
              COMPUTE WS-LF-MIN-TERM = WS-EARN-TERM - WS-LF-CAN-TERM
              COMPUTE WS-LF-MAX-TERM = WS-LF-MIN-TERM + WS-LF-CAN-TERM
                 - +1
           END-IF

           IF WS-AH-CAN-TERM > +0
              COMPUTE WS-AH-MIN-TERM = WS-EARN-TERM - WS-AH-CAN-TERM
              COMPUTE WS-AH-MAX-TERM = WS-AH-MIN-TERM + WS-AH-CAN-TERM
                 - +1
           END-IF

           PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
              (WS-A > WS-EARN-TERM)
              IF CR-LFAMT-ALT > +0
                 COMPUTE WS-ANGLEM = (1 - ((1 + WS-J)
                   ** ((WS-WORK-TERM - WS-A) * -1))) / WS-J
                 COMPUTE WS-AMT = (WS-P * WS-ANGLEM) +
                   (CR-LFAMT-ALT * (1 / (1 + WS-K)
                   ** (WS-WORK-TERM - WS-A + 1)))
              ELSE

                 COMPUTE WS-ANGLEM = (1 - ((1 + WS-J)
                   ** ((WS-WORK-TERM - WS-A + 1) * -1))) / WS-J
                 COMPUTE WS-VM = (1 / (1 + WS-J))
                   ** (WS-WORK-TERM - WS-A + 1)
                 COMPUTE WS-AMT = WS-P *
                   (WS-ANGLEM + (WS-ANGLEN-M * WS-VM))
              END-IF
061112        MOVE WS-ANGLEM              TO WS-DISPLAY-FLD
061112*        DISPLAY ' WS Anglem 230b ' WS-DISPLAY-FLD

      *       IF WS-A = +1
      *          MOVE WS-VM               TO WS-DISPLAY-FLD
      *          DISPLAY ' WS VM    ' WS-DISPLAY-FLD
      *          MOVE WS-ANGLEM           TO WS-DISPLAY-FLD
      *          DISPLAY ' WS AM    ' WS-DISPLAY-FLD
      *          MOVE WS-AMT TO WS-DISPLAY-FLD
      *          DISPLAY ' AMT FIRST ' WS-DISPLAY-FLD
      *       END-IF
              IF RUN-DATE = CR-ENTRY-DATE
                 COMPUTE WS-LF-PRM = WS-LF-PRM
                    + (WS-AMT / +1000 * CR-LFPRM-RATE)
                 IF CR-LFAMT-ALT > +0
                    COMPUTE WS-AH-PRM = WS-AH-PRM +
                      (WS-AMT - (CR-LFAMT-ALT * (1 / (1 + WS-K)
                      ** (WS-WORK-TERM - WS-A + 1)))) * 
                      (CR-AHPRM-RATE / 1000)
                 ELSE
                    COMPUTE WS-AH-PRM = WS-AH-PRM
                      + (WS-AMT / +1000 * CR-AHPRM-RATE)
                 END-IF
      *          MOVE WS-LF-PRM           TO WS-DISPLAY-FLD
      *          DISPLAY ' WS LFPRM ' WS-DISPLAY-FLD
                 COMPUTE WS-BASE-COMM = WS-BASE-COMM * WS-A
                 COMPUTE WS-GA-COMM   = WS-GA-COMM   * WS-A
              ELSE
                 COMPUTE WS-LF-PRM = WS-AMT * CR-LFPRM-RATE / +1000
                 IF CR-LFAMT-ALT > +0
                    COMPUTE WS-AH-PRM =
                      (WS-AMT - (CR-LFAMT-ALT * (1 / (1 + WS-K)
                      ** (WS-WORK-TERM - WS-A + 1)))) * 
                      (CR-AHPRM-RATE / 1000)
                 ELSE
                    COMPUTE WS-AH-PRM = WS-AMT * CR-AHPRM-RATE / +1000
                 END-IF
              END-IF
              IF (CREDIT-LF-CANCEL)
                 AND (WS-LF-CAN-TERM > +0)
                 AND (WS-A NOT < WS-LF-MIN-TERM)
                 AND (WS-A NOT > WS-LF-MAX-TERM)
                 MOVE WS-A             TO WS-DISPLAY-FLD
                 DISPLAY ' FOUND LF ADJUST ' CR-ACCT-CONTROL
                    '   ' CR-CERT-NO '  ' WS-DISPLAY-FLD
                 MOVE WS-LF-PRM        TO WS-DISPLAY-FLD
                 DISPLAY '  LF REFUND ' WS-DISPLAY-FLD
                 COMPUTE WS-LF-REF = WS-LF-REF + WS-LF-PRM
              END-IF
              IF (CREDIT-AH-CANCEL)
                 AND (WS-AH-CAN-TERM > +0)
                 AND (WS-A NOT < WS-AH-MIN-TERM)
                 AND (WS-A NOT > WS-AH-MAX-TERM)
                 MOVE WS-A             TO WS-DISPLAY-FLD
                 DISPLAY ' FOUND AH ADJUST ' CR-ACCT-CONTROL
                    '   ' CR-CERT-NO '  ' WS-DISPLAY-FLD
                 MOVE WS-AH-PRM        TO WS-DISPLAY-FLD
                 DISPLAY '  AH REFUND ' WS-DISPLAY-FLD
                 COMPUTE WS-AH-REF = WS-AH-REF + WS-AH-PRM
              END-IF
           END-PERFORM

           IF (WS-LF-REF > +0)
              OR (WS-AH-REF > +0)
              COMPUTE WS-EXP-PREM = (WS-LF-REF + WS-AH-REF) * -1
           ELSE
              COMPUTE WS-EXP-PREM = (WS-LF-PRM + WS-AH-PRM)
           END-IF

           MOVE +0                     TO WS-BASE-COMM
                                          WS-GA-COMM
           PERFORM VARYING X1 FROM +1 BY +1 UNTIL
              (X1 > +10)
              IF CR-AGT-TYPE (X1) = 'C' OR 'D'
                 COMPUTE WS-BASE-COMM = WS-BASE-COMM
      *          + (WS-LF-PRM * CR-LCOM-L (X1))
      *          + (WS-AH-PRM * CR-LCOM-AH (X1))
                 + (WS-LF-PRM / (1 - CR-LCOM-L (X1))
                    * CR-LCOM-L (X1))
                 + (WS-AH-PRM / (1 - CR-LCOM-AH (X1))
                    * CR-LCOM-AH (X1))




              END-IF
052814        IF CR-AGT-TYPE (X1) = 'O' OR 'P' or 'S'
                 COMPUTE WS-GA-COMM = WS-GA-COMM
      *          + (WS-LF-PRM * CR-LCOM-L (X1))
      *          + (WS-AH-PRM * CR-LCOM-AH (X1))
                 + (WS-LF-PRM / (1 - CR-LCOM-L (X1))
                    * CR-LCOM-L (X1))
                 + (WS-AH-PRM / (1 - CR-LCOM-AH (X1))
                    * CR-LCOM-AH (X1))
              END-IF
           END-PERFORM
           .

       0230-EXIT.
           EXIT.

       0240-CALC-SRP.
           
           DISPLAY ' MADE IT TO 0240 ' CR-CERT-NO
           
           COMPUTE WS-EARN-TERM = WS-WORK-TERM - REM-TERM

           IF CR-LOAN-TERM NOT NUMERIC
              MOVE CR-LF-TERM          TO CR-LOAN-TERM
           END-IF
           IF CR-LOAN-TERM = ZEROS
              MOVE CR-LF-TERM          TO CR-LOAN-TERM
           END-IF
           IF CR-LOAN-TERM = ZEROS
              MOVE CR-AH-TERM          TO CR-LOAN-TERM
           END-IF

           COMPUTE WS-I = CR-APR / +1200

           COMPUTE WS-J = WS-I + ((CR-LFPRM-RATE / +1000)
              + (CR-AHPRM-RATE / +1000))

           COMPUTE WS-K = WS-I + (CR-LFPRM-RATE / +1000)

           COMPUTE WS-Y = (1 + (WS-DAYS-TO-1ST * WS-K / 30))
              / (1 + WS-K)

           COMPUTE WS-ANGLEMK = ((1 - ((1 + WS-K)
              ** (WS-WORK-TERM * -1))) / WS-K)

           IF CR-LFAMT = ZEROS
061112*        COMPUTE CR-LFAMT = CR-AHAMT * WS-ANGLEM
061112         COMPUTE CR-LFAMT = CR-AHAMT * WS-ANGLEMK
           END-IF
           MOVE CR-LFAMT               TO WS-AMT

061112     MOVE WS-ANGLEM              TO WS-DISPLAY-FLD
061112*     DISPLAY ' WS Anglem 240 ' WS-DISPLAY-FLD
061112     MOVE WS-ANGLEMk             TO WS-DISPLAY-FLD
061112*     DISPLAY ' WS Anglemk 240 ' WS-DISPLAY-FLD

           COMPUTE WS-P = WS-AMT / ((WS-ANGLEMK / WS-Y)
              - ((((WS-WORK-TERM - WS-ANGLEMK) / WS-K)
              * CR-AHPRM-RATE / 1000)
              + WS-ANGLEMK / WS-Y * (WS-DAYS-TO-1ST - 30) / 30
              * CR-AHPRM-RATE / 1000))

           MOVE +0                     TO WS-LF-PRM
                                          WS-AH-PRM
                                          WS-LF-REF
                                          WS-AH-REF
                                          WS-LF-MIN-TERM
                                          WS-LF-MAX-TERM
                                          WS-AH-MIN-TERM
                                          WS-AH-MAX-TERM
                                          WS-LINS WS-DINS
                                          WS-FLINS WS-FDINS
                                          WS-INT   WS-FINT
           IF WS-LF-CAN-TERM > +0
              COMPUTE WS-LF-MIN-TERM = WS-EARN-TERM - WS-LF-CAN-TERM
              COMPUTE WS-LF-MAX-TERM = WS-LF-MIN-TERM + WS-LF-CAN-TERM
                 - +1
           END-IF

           IF WS-AH-CAN-TERM > +0
              COMPUTE WS-AH-MIN-TERM = WS-EARN-TERM - WS-AH-CAN-TERM
              COMPUTE WS-AH-MAX-TERM = WS-AH-MIN-TERM + WS-AH-CAN-TERM
                 - +1
           END-IF

      ***  THE BELOW 3 FIELDS ARE FOR CALCULATING THE FEES AND INT
      ***  FOR THE TIME BETWEEN THE EFF DATE AND 1ST PMT DATE ONLY
      
           COMPUTE WS-FINT = WS-AMT * (1 + WS-I *
              (WS-DAYS-TO-1ST - 30) / 30) - WS-AMT

           COMPUTE WS-FLINS = WS-AMT * CR-LFPRM-RATE / 1000
              * (WS-DAYS-TO-1ST - 30) / 30

           COMPUTE WS-FDINS = WS-P * WS-WORK-TERM * CR-AHPRM-RATE
              / 1000 * (WS-DAYS-TO-1ST - 30) / 30

           IF CR-ACCOUNT = '0005500363'
      *    IF CR-CERT-NO = '0126880300 ' OR '0018664366 '
           MOVE WS-INIT-DB-EXTRACT     TO DEBUG-EXTRACT-RECORD
           MOVE CR-ACCOUNT             TO DB-ACCOUNT
           MOVE CR-CERT-NO             TO DB-CERT-NO

           MOVE CR-DT                  TO WS-ISSUE-DATE
           MOVE WS-ISSUE-MMDD (1:2)    TO DB-EFF-DATE (1:2)
           MOVE '-'                    TO DB-EFF-DATE (3:1)
                                          DB-EFF-DATE (6:1)
           MOVE WS-ISSUE-MMDD (3:2)    TO DB-EFF-DATE (4:2)
           MOVE WS-ISSUE-CCYY          TO DB-EFF-DATE (7:4)
           MOVE CR-ENTRY-DATE          TO WS-ENTRY-DATE
           MOVE WS-ENTRY-MMDD (1:2)    TO DB-ENT-DATE (1:2)
           MOVE '-'                    TO DB-ENT-DATE (3:1)
                                          DB-ENT-DATE (6:1)
           MOVE WS-ENTRY-MMDD (3:2)    TO DB-ENT-DATE (4:2)
           MOVE WS-ENTRY-CCYY          TO DB-ENT-DATE (7:4)
           MOVE RUN-DATE               TO WS-ENTRY-DATE
           MOVE WS-ENTRY-MMDD (1:2)    TO DB-VAL-DATE (1:2)
           MOVE '-'                    TO DB-VAL-DATE (3:1)
                                          DB-VAL-DATE (6:1)
           MOVE WS-ENTRY-MMDD (3:2)    TO DB-VAL-DATE (4:2)
           MOVE WS-ENTRY-CCYY          TO DB-VAL-DATE (7:4)
           MOVE ZEROS                  TO DB-PERIOD
           MOVE WS-P                   TO DB-PAYMENT
           MOVE ZEROS                  TO DB-PRINC
           MOVE WS-FINT                TO DB-INTEREST
           MOVE WS-FLINS               TO DB-LINS
           MOVE WS-FDINS               TO DB-DINS
           MOVE WS-AMT                 TO DB-BALANCE
           MOVE CR-LFTYP               TO DB-LF-BEN-CODE
           MOVE CR-AHTYP               TO DB-AH-BEN-CODE
           MOVE CR-LF-TERM             TO DB-LF-TERM
           MOVE CR-AH-TERM             TO DB-AH-TERM
           MOVE CR-LF-EXPIRE-DATE      TO WS-EXPIRE-DATE
           MOVE WS-EXPIRE-MMDD (1:2)   TO DB-LF-EXP-DATE (1:2)
           MOVE '-'                    TO DB-LF-EXP-DATE (3:1)
                                          DB-LF-EXP-DATE (6:1)
           MOVE WS-EXPIRE-MMDD (3:2)   TO DB-LF-EXP-DATE (4:2)
           MOVE WS-EXPIRE-CCYY         TO DB-LF-EXP-DATE (7:4)

           MOVE CR-AH-EXPIRE-DATE      TO WS-EXPIRE-DATE
           MOVE WS-EXPIRE-MMDD (1:2)   TO DB-AH-EXP-DATE (1:2)
           MOVE '-'                    TO DB-AH-EXP-DATE (3:1)
                                          DB-AH-EXP-DATE (6:1)
           MOVE WS-EXPIRE-MMDD (3:2)   TO DB-AH-EXP-DATE (4:2)
           MOVE WS-EXPIRE-CCYY         TO DB-AH-EXP-DATE (7:4)
           WRITE DEBUG-EXTRACT-RECORD
           END-IF

           PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
              (WS-A > WS-EARN-TERM)

              IF RUN-DATE = CR-ENTRY-DATE
                 COMPUTE WS-LINS = WS-LINS + WS-FLINS
                    + (WS-AMT / 1000 * CR-LFPRM-RATE)
                 COMPUTE WS-DINS = WS-DINS + WS-FDINS
                    + (WS-WORK-TERM - WS-A + 1) * WS-P
                    * CR-AHPRM-RATE / 1000   
                 COMPUTE WS-BASE-COMM = WS-BASE-COMM * WS-A
                 COMPUTE WS-GA-COMM   = WS-GA-COMM   * WS-A
              ELSE
                 COMPUTE WS-LINS = WS-FLINS
                    + (WS-AMT / 1000 * CR-LFPRM-RATE)
                 COMPUTE WS-DINS = WS-FDINS
                    + (WS-WORK-TERM - WS-A + 1) * WS-P
                    * CR-AHPRM-RATE / 1000   
              END-IF 

              COMPUTE WS-INT = WS-AMT * WS-I
              COMPUTE WS-INT = WS-INT + WS-FINT

              IF WS-A = +1
                 COMPUTE WS-AMT = WS-AMT - WS-P + WS-LINS
                    + WS-DINS + WS-INT
              ELSE
                 COMPUTE WS-AMT = WS-AMT -
                   (WS-P - WS-INT - WS-LINS - WS-DINS)
              END-IF


              IF (CREDIT-LF-CANCEL)
                 AND (WS-LF-CAN-TERM > +0)
                 AND (WS-A NOT < WS-LF-MIN-TERM)
                 AND (WS-A NOT > WS-LF-MAX-TERM)
                 MOVE WS-A             TO WS-DISPLAY-FLD
                 DISPLAY ' FOUND LF ADJUST ' CR-ACCT-CONTROL
                    '   ' CR-CERT-NO '  ' WS-DISPLAY-FLD
                 MOVE WS-LINS          TO WS-DISPLAY-FLD
                 DISPLAY '  LF REFUND ' WS-DISPLAY-FLD
                 COMPUTE WS-LF-REF = WS-LF-REF + WS-LINS
              END-IF
              IF (CREDIT-AH-CANCEL)
                 AND (WS-AH-CAN-TERM > +0)
                 AND (WS-A NOT < WS-AH-MIN-TERM)
                 AND (WS-A NOT > WS-AH-MAX-TERM)
                 MOVE WS-A             TO WS-DISPLAY-FLD
                 DISPLAY ' FOUND AH ADJUST ' CR-ACCT-CONTROL
                    '   ' CR-CERT-NO '  ' WS-DISPLAY-FLD
                 MOVE WS-DINS          TO WS-DISPLAY-FLD
                 DISPLAY '  AH REFUND ' WS-DISPLAY-FLD
                 COMPUTE WS-AH-REF = WS-AH-REF + WS-DINS
              END-IF



              MOVE +0                  TO WS-FLINS
                                          WS-FDINS
                                          WS-FINT
              

           IF CR-ACCOUNT = '0005500363'
      *    IF CR-CERT-NO = '0126880300 ' OR '0018664366 '
           MOVE WS-INIT-DB-EXTRACT     TO DEBUG-EXTRACT-RECORD
           MOVE CR-ACCOUNT             TO DB-ACCOUNT
           MOVE CR-CERT-NO             TO DB-CERT-NO

           MOVE CR-DT                  TO WS-ISSUE-DATE
           MOVE WS-ISSUE-MMDD (1:2)    TO DB-EFF-DATE (1:2)
           MOVE '-'                    TO DB-EFF-DATE (3:1)
                                          DB-EFF-DATE (6:1)
           MOVE WS-ISSUE-MMDD (3:2)    TO DB-EFF-DATE (4:2)
           MOVE WS-ISSUE-CCYY          TO DB-EFF-DATE (7:4)
           MOVE CR-ENTRY-DATE          TO WS-ENTRY-DATE
           MOVE WS-ENTRY-MMDD (1:2)    TO DB-ENT-DATE (1:2)
           MOVE '-'                    TO DB-ENT-DATE (3:1)
                                          DB-ENT-DATE (6:1)
           MOVE WS-ENTRY-MMDD (3:2)    TO DB-ENT-DATE (4:2)
           MOVE WS-ENTRY-CCYY          TO DB-ENT-DATE (7:4)
           MOVE RUN-DATE               TO WS-ENTRY-DATE
           MOVE WS-ENTRY-MMDD (1:2)    TO DB-VAL-DATE (1:2)
           MOVE '-'                    TO DB-VAL-DATE (3:1)
                                          DB-VAL-DATE (6:1)
           MOVE WS-ENTRY-MMDD (3:2)    TO DB-VAL-DATE (4:2)
           MOVE WS-ENTRY-CCYY          TO DB-VAL-DATE (7:4)
           MOVE WS-A                   TO DB-PERIOD
           MOVE WS-P                   TO DB-PAYMENT
           COMPUTE DB-PRINC = WS-P - WS-INT - WS-LINS - WS-DINS
           MOVE WS-INT                 TO DB-INTEREST
           MOVE WS-LINS                TO DB-LINS
           MOVE WS-DINS                TO DB-DINS
           MOVE WS-AMT                 TO DB-BALANCE
           MOVE CR-LFTYP               TO DB-LF-BEN-CODE
           MOVE CR-AHTYP               TO DB-AH-BEN-CODE
           MOVE CR-LF-TERM             TO DB-LF-TERM
           MOVE CR-AH-TERM             TO DB-AH-TERM
           MOVE CR-LF-EXPIRE-DATE      TO WS-EXPIRE-DATE
           MOVE WS-EXPIRE-MMDD (1:2)   TO DB-LF-EXP-DATE (1:2)
           MOVE '-'                    TO DB-LF-EXP-DATE (3:1)
                                          DB-LF-EXP-DATE (6:1)
           MOVE WS-EXPIRE-MMDD (3:2)   TO DB-LF-EXP-DATE (4:2)
           MOVE WS-EXPIRE-CCYY         TO DB-LF-EXP-DATE (7:4)

           MOVE CR-AH-EXPIRE-DATE      TO WS-EXPIRE-DATE
           MOVE WS-EXPIRE-MMDD (1:2)   TO DB-AH-EXP-DATE (1:2)
           MOVE '-'                    TO DB-AH-EXP-DATE (3:1)
                                          DB-AH-EXP-DATE (6:1)
           MOVE WS-EXPIRE-MMDD (3:2)   TO DB-AH-EXP-DATE (4:2)
           MOVE WS-EXPIRE-CCYY         TO DB-AH-EXP-DATE (7:4)
           
           WRITE DEBUG-EXTRACT-RECORD
           END-IF
           END-PERFORM

           IF (WS-LF-REF > +0)
              OR (WS-AH-REF > +0)
              COMPUTE WS-EXP-PREM = (WS-LF-REF + WS-AH-REF) * -1
           ELSE
              COMPUTE WS-EXP-PREM = (WS-LINS + WS-DINS)
           END-IF

           MOVE +0                     TO WS-BASE-COMM
                                          WS-GA-COMM
           PERFORM VARYING X1 FROM +1 BY +1 UNTIL
              (X1 > +10)
              IF CR-AGT-TYPE (X1) = 'C' OR 'D'
                 COMPUTE WS-BASE-COMM = WS-BASE-COMM
      *          + (WS-LF-PRM * CR-LCOM-L (X1))
      *          + (WS-AH-PRM * CR-LCOM-AH (X1))
                 + (WS-LINS / (1 - CR-LCOM-L (X1))
                    * CR-LCOM-L (X1))
                 + (WS-DINS / (1 - CR-LCOM-AH (X1))
                    * CR-LCOM-AH (X1))




              END-IF
052814        IF CR-AGT-TYPE (X1) = 'O' OR 'P' or 'S'
                 COMPUTE WS-GA-COMM = WS-GA-COMM
      *          + (WS-LF-PRM * CR-LCOM-L (X1))
      *          + (WS-AH-PRM * CR-LCOM-AH (X1))
                 + (WS-LINS / (1 - CR-LCOM-L (X1))
                    * CR-LCOM-L (X1))
                 + (WS-DINS / (1 - CR-LCOM-AH (X1))
                    * CR-LCOM-AH (X1))
              END-IF
           END-PERFORM
           .

       0240-EXIT.
           EXIT.

       0250-BREAK-RTN.
       
      *    DISPLAY ' MADE IT TO 0250      BREAK '
           IF WS-HOLD-CAR NOT = CR-CARRIER
              PERFORM 0300-ACCT-BREAK  THRU 0300-EXIT
              PERFORM 0310-ST-BREAK    THRU 0310-EXIT
              PERFORM 0320-GRP-BREAK   THRU 0320-EXIT
              PERFORM 0330-CAR-BREAK   THRU 0330-EXIT
           ELSE
              IF WS-HOLD-GRP NOT = CR-GROUPING
                 PERFORM 0300-ACCT-BREAK
                                       THRU 0300-EXIT
                 PERFORM 0310-ST-BREAK THRU 0310-EXIT
                 PERFORM 0320-GRP-BREAK
                                       THRU 0320-EXIT
              ELSE
                 IF WS-HOLD-ST NOT = CR-STATE
                    PERFORM 0300-ACCT-BREAK
                                       THRU 0300-EXIT
                    PERFORM 0310-ST-BREAK
                                       THRU 0310-EXIT
                 ELSE
                    IF WS-HOLD-ACCT NOT = CR-ACCOUNT
                       PERFORM 0300-ACCT-BREAK
                                       THRU 0300-EXIT
                    END-IF
                 END-IF
              END-IF
           END-IF
           .        
       0250-EXIT.
           EXIT.

       0300-ACCT-BREAK.
       
      *    DISPLAY ' MADE IT TO 0300 ACCT BREAK '
           IF (WS-TOT-EXP-PREM (1) = ZEROS)
              AND (WS-TOT-REC-PREM (1) = ZEROS)
              AND (WS-TOT-GA-COMM (1) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE WS-HOLD-GRP         TO DTL1-GRP
              MOVE WS-HOLD-ST          TO DTL1-ST
              MOVE WS-HOLD-ACCT        TO DTL1-ACCT
              MOVE WS-TOT-EXP-PREM (1) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (1) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (1)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-1             TO P-CTL
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-EXP-PREM (2) = WS-TOT-EXP-PREM (2)
                 + WS-TOT-EXP-PREM (1)
              COMPUTE WS-TOT-REC-PREM (2) = WS-TOT-REC-PREM (2)
                 + WS-TOT-REC-PREM (1)
              COMPUTE WS-TOT-GA-COMM (2) = WS-TOT-GA-COMM (2)
                 + WS-TOT-GA-COMM (1)
           END-IF
           MOVE +0                     TO WS-TOT-EXP-PREM (1)
                                          WS-TOT-REC-PREM (1)
                                          WS-TOT-GA-COMM  (1)
              
           .
       0300-EXIT.
           EXIT.
           
       0310-ST-BREAK.
       
      *    DISPLAY ' MADE IT TO 0310  ST  BREAK '
           IF (WS-TOT-EXP-PREM (2) = ZEROS)
              AND (WS-TOT-REC-PREM (2) = ZEROS)
              AND (WS-TOT-GA-COMM (2) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE WS-HOLD-GRP         TO DTL1-GRP
              MOVE WS-HOLD-ST          TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-EXP-PREM (2) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (2) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (2)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-2             TO P-CTL
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-EXP-PREM (3) = WS-TOT-EXP-PREM (3)
                 + WS-TOT-EXP-PREM (2)
              COMPUTE WS-TOT-REC-PREM (3) = WS-TOT-REC-PREM (3)
                 + WS-TOT-REC-PREM (2)
              COMPUTE WS-TOT-GA-COMM (3) = WS-TOT-GA-COMM (3)
                 + WS-TOT-GA-COMM (2)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (2)
                                          WS-TOT-REC-PREM (2)
                                          WS-TOT-GA-COMM  (2)
              
           .
       0310-EXIT.
           EXIT.
           
       0320-GRP-BREAK.
       
      *    DISPLAY ' MADE IT TO 0320 GRP  BREAK '
           IF (WS-TOT-EXP-PREM (3) = ZEROS)
              AND (WS-TOT-REC-PREM (3) = ZEROS)
              AND (WS-TOT-GA-COMM (3) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE WS-HOLD-GRP         TO DTL1-GRP
              MOVE SPACES              TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-EXP-PREM (3) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (3) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (3)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-2             TO P-CTL
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-EXP-PREM (4) = WS-TOT-EXP-PREM (4)
                 + WS-TOT-EXP-PREM (3)
              COMPUTE WS-TOT-REC-PREM (4) = WS-TOT-REC-PREM (4)
                 + WS-TOT-REC-PREM (3)
              COMPUTE WS-TOT-GA-COMM (4) = WS-TOT-GA-COMM (4)
                 + WS-TOT-GA-COMM (3)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (3)
                                          WS-TOT-REC-PREM (3)
                                          WS-TOT-GA-COMM  (3)
              
           .
       0320-EXIT.
           EXIT.
           
       0330-CAR-BREAK.
       
           DISPLAY ' MADE IT TO 0330 CAR  BREAK '
           IF (WS-TOT-EXP-PREM (4) = ZEROS)
              AND (WS-TOT-REC-PREM (4) = ZEROS)
              AND (WS-TOT-GA-COMM (4) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE SPACES              TO DTL1-GRP
              MOVE SPACES              TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-EXP-PREM (4) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (4) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (4)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-2             TO P-CTL
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-EXP-PREM (5) = WS-TOT-EXP-PREM (5)
                 + WS-TOT-EXP-PREM (4)
              COMPUTE WS-TOT-REC-PREM (5) = WS-TOT-REC-PREM (5)
                 + WS-TOT-REC-PREM (4)
              COMPUTE WS-TOT-GA-COMM (5) = WS-TOT-GA-COMM (5)
                 + WS-TOT-GA-COMM (4)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (4)
                                          WS-TOT-REC-PREM (4)
                                          WS-TOT-GA-COMM  (4)
              
           .
       0330-EXIT.
           EXIT.
           
       0340-FIN-BREAK.
       
           DISPLAY ' MADE IT TO 0340 FIN  BREAK '
           IF (WS-TOT-EXP-PREM (5) = ZEROS)
              AND (WS-TOT-REC-PREM (5) = ZEROS)
              AND (WS-TOT-GA-COMM (5) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE SPACES              TO DTL1-CAR
              MOVE SPACES              TO DTL1-GRP
              MOVE SPACES              TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE ' GRAND TOTALS '    TO DETAIL-LINE (1:14)
              MOVE WS-TOT-EXP-PREM (5) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (5) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (5)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-2             TO P-CTL
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

      *       COMPUTE WS-TOT-EXP-PREM (5) = WS-TOT-EXP-PREM (5)
      *          + WS-TOT-EXP-PREM (4)
      *       COMPUTE WS-TOT-REC-PREM (5) = WS-TOT-REC-PREM (5)
      *          + WS-TOT-REC-PREM (4)
      *       COMPUTE WS-TOT-GA-COMM (5) = WS-TOT-GA-COMM (5)
      *          + WS-TOT-GA-COMM (4)
           END-IF
              
           .
       0340-EXIT.
           EXIT.



       0510-GET-REMAINING-TERM.

           CALL 'ELRTRMX' USING CALCULATION-PASS-AREA

           .

       0510-EXIT.
           EXIT.

       8600-HD-RTN.

           MOVE +0                     TO LNCTR
           MOVE HD1                    TO  P-DATA
           MOVE SPACE-NP               TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           ADD +1                      TO  PGCTR
                                                
           MOVE PGCTR                  TO  HD-PG
           MOVE HD2                    TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE HD3                    TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE SPACE-2                TO  P-CTL
           MOVE HD4                    TO  P-DATA
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE HD5                    TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE SPACES                 TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE +10                    TO  LNCTR
      *    MOVE SPACE-2                TO  P-CTL

           .
       8699-EXIT.
           EXIT.

       8800-PRT-RTN.

           IF LNCTR > +56
              MOVE PRT                 TO WS-HOLD-PRT
              PERFORM 8600-HD-RTN      THRU 8699-EXIT
              MOVE WS-HOLD-PRT         TO PRT
           END-IF

           MOVE P-CTL                  TO X
           IF P-CTL = SPACE-1
              ADD +1                   TO LNCTR
           ELSE
              IF P-CTL = SPACE-2
                 ADD +2                TO LNCTR
              ELSE
                 IF P-CTL = SPACE-3
                    ADD +3             TO LNCTR
                 END-IF
              END-IF
           END-IF
            
           .
       8850-COPY-PRT-RTN.
                                       COPY ELCPRT2.
                         
       8800-EXIT.        
           EXIT.         
       EJECT             
       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
