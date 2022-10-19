       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL355.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *REMARKS.

      *             TTTTTTT     BBBBBBB     DDDDDD
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBBB    D     D
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBB     DDDDDD

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 030909 CR2008100900001   PEMA  NEW PROGRAM
052614* 052614   2014022100001   AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518 CR2017061500001   TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELMSTR           ASSIGN TO ELMSTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-PRIMARY
                                   FILE STATUS IS ELMSTR-FILE-STATUS.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

           SELECT ELDENY           ASSIGN TO ELDENY
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS DN-CONTROL-PRIMARY
                                   FILE STATUS IS ELDENY-FILE-STATUS.

           SELECT SORT-FILE        ASSIGN TO SORTWK1.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT PRNTR            ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  ELMSTR.

                                       COPY ELCMSTR.

       FD  ELTRLR.

                                       COPY ELCTRLR.

       FD  ELDENY.

                                       COPY ELCDENY.

       SD  SORT-FILE.

       01  SORT-RECORD.
           05  SORT-DENIAL-KEY.
               10  SORT-STATE          PIC XX.
               10  SORT-CODE           PIC XXXX.
               10  SORT-TYPE           PIC X.
           05  SORT-COUNT              PIC 9.

       FD  PRNTR                       COPY ELCPRTFD.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   EL355    WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  CLM-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  TRL-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CLM-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  T1                      PIC S9(3) VALUE +0 COMP-3.
       77  T2                      PIC S9(3) VALUE +0 COMP-3.
       77  T3                      PIC S9(3) VALUE +0 COMP-3.
       77  H1                      PIC S9(3) VALUE +0 COMP-3.
       77  H2                      PIC S9(3) VALUE +0 COMP-3.
       77  WS-GOT-IT-SW            PIC X     VALUE ' '.
           88  WE-GOT-IT-ALL                 VALUE 'Y'.
       77  WS-BIN-LAST-EOM-DT      PIC XX    VALUE LOW-VALUES.

       01  WS-HEADING1.                                                 
           05  FILLER                      PIC X(44)       VALUE '1'.   
           05  WS-H1-TITLE                 PIC X(76)       VALUE        
               'MTD CLAIMS NOT APPROVED - DENIALS '.
           05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL355'.
                                                                        
       01  WS-HEADING2.                                                 
           05  FILLER                      PIC X(45)       VALUE SPACES.
           05  WS-H2-CLIENT-NAME           PIC X(75)       VALUE SPACES.
           05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.
           05  FILLER                      PIC X           VALUE SPACES.
                                                                        
       01  WS-HEADING3.                                                 
           05  FILLER                      PIC X(51)       VALUE SPACES.
           05  WS-H3-DATE                  PIC X(69)       VALUE SPACES.
           05  FILLER                      PIC X(5)        VALUE 'PAGE'.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  
           05  FILLER                      PIC X(11)       VALUE SPACES.
                                                                        
       01  WS-HEADING4.
           05  FILLER                      PIC X(15)       VALUE '-'.
           05  WS-HEAD4-DETAIL OCCURS 16   PIC X(7).

       01  WS-HEADING5.
           05  FILLER                      PIC X(15)       VALUE ' '.
           05  WS-HEAD5-DETAIL OCCURS 16  PIC X(7).

       01  WS-DETAIL1.                                                  
           05  FILLER                      PIC XX.
           05  WS-D1-STATE                 PIC XX.
           05  FILLER                      PIC XXXX.
           05  WS-D1-TYPE                  PIC X(5).
           05  FILLER OCCURS 16.
               10  FILLER                  PIC XX.
               10  WS-D1-COUNT             PIC ZZZZ9.

       01  WS-CODE-TABLE.
           05  WS-DENIAL-CODES OCCURS 30.
               10  WS-DENIAL-CODE       PIC XXXX.
               10  WS-DENIAL-DESC       PIC X(50).

       01  WS-DENIAL-TABLE.
           05  WS-DENIAL-STATES OCCURS 60.
               10  WS-STATE-CODE            PIC XX.
               10  FILLER OCCURS 30.
022122             15  WS-DENIAL-TYPES OCCURS 8.
                       20  WS-DENIAL-TYPE   PIC X.
                       20  WS-DENIAL-COUNT  PIC 9(7).

      ******************************************************************
       01  WS-MISC.
           05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.
           05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  
           05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   
           05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +59.   
           05  ELMSTR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELTRLR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELDENY-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.
           05  PGM-SUB          COMP-3 PIC S9(04) VALUE +585.
           05  WS-RETURN-CODE   COMP   PIC S9(03) VALUE +0.
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
           05  WS-ZERO          COMP-3 PIC S9(01) VALUE +0.
           05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0022-BUILD-ELDENY   THRU 0022-EXIT

           MOVE ' '                    TO WS-EOF-SW
           MOVE +1                     TO T2

           SORT SORT-FILE ASCENDING KEY SORT-DENIAL-KEY
              INPUT PROCEDURE 0030-INPUT-PROCEDURE
                                       THRU 0030-EXIT
              OUTPUT PROCEDURE 0400-OUTPUT-PROCEDURE
                                       THRU 0400-EXIT.

           PERFORM 0500-PRINT-REPORT   THRU 0500-EXIT
           PERFORM 1000-CLOSE-FILES    THRU 1000-EXIT

           DISPLAY ' CLAIM RECORDS READ    '  CLM-RECS-IN
           DISPLAY ' TRLR  RECORDS READ    '  TRL-RECS-IN
           DISPLAY ' CLAIM RECORDS WRITTEN '  CLM-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT ELMSTR ELTRLR ELDENY
              OUTPUT PRNTR

           IF ELMSTR-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ELMSTR - OPEN ' ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELTRLR-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ELTRLR - OPEN ' ELTRLR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELDENY-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ELDENY - OPEN ' ELDENY-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           INITIALIZE WS-DENIAL-TABLE

           MOVE SPACES                 TO WS-CODE-TABLE

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '1'                    TO DC-END-OF-MONTH
           MOVE '6'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO WS-BIN-LAST-EOM-DT
              DISPLAY ' LAST EOM DATE ' DC-GREG-DATE-2-EDIT
              ' A ' DC-GREG-DATE-A-EDIT
              ' B ' DC-GREG-DATE-B-EDIT
           ELSE
              DISPLAY ' ERROR COVERTING LAST EOM DATE '
              PERFORM ABEND-PGM
           END-IF
           PERFORM 0250-START-ELMSTR   THRU 0250-EXIT
           PERFORM 0200-READ-ELMSTR    THRU 0200-EXIT

           .
       0020-EXIT.
           EXIT.

       0022-BUILD-ELDENY.

           PERFORM 0024-START-ELDENY   THRU 0024-EXIT

           PERFORM 0025-READ-ELDENY    THRU 0025-EXIT

           MOVE +1                     TO T2

           PERFORM 0023-PROCESS-ELDENY THRU 0023-EXIT UNTIL
              END-OF-INPUT

           .
       0022-EXIT.
           EXIT.

       0023-PROCESS-ELDENY.

           IF DN-DENIAL-CODE (1:1) = 'D'
              MOVE DN-DENIAL-CODE      TO WS-DENIAL-CODE (T2)
              MOVE DN-DESCRIPTION      TO WS-DENIAL-DESC (T2)
              ADD +1 TO T2
           END-IF

           PERFORM 0025-READ-ELDENY    THRU 0025-EXIT

           .
       0023-EXIT.
           EXIT.

       0024-START-ELDENY.

           MOVE LOW-VALUES             TO DN-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO DN-COMPANY-CD

           START ELDENY KEY >= DN-CONTROL-PRIMARY

           IF ELDENY-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ELDENY-FILE-STATUS NOT = '00'
                 DISPLAY 'ELDENY - ERROR - START ' ELDENY-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0024-EXIT.
           EXIT.

       0025-READ-ELDENY.

           READ ELDENY NEXT RECORD

           IF (ELDENY-FILE-STATUS = '10' OR '23')
              OR (DN-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ELDENY-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ELDENY - READ ' ELDENY-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           .
       0025-EXIT.
           EXIT.

       0030-INPUT-PROCEDURE.

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
                 (END-OF-INPUT)
PEMTST*          OR (CLM-RECS-IN > 1000)

           .
       0030-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           IF CLAIM-DENIED
              PERFORM 0100-PROCESS-ELMSTR
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-ELMSTR    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.
       0100-PROCESS-ELMSTR.

           MOVE SPACES                 TO SORT-RECORD
           MOVE CL-CERT-STATE          TO SORT-STATE
           MOVE CL-CLAIM-TYPE          TO SORT-TYPE
           MOVE 1                      TO SORT-COUNT
           PERFORM 0210-GET-ELTRLR     THRU 0210-EXIT
           IF SORT-CODE (1:1) = 'D'
              DISPLAY ' RELEASING RECORD ' CL-CERT-STATE ' '
                 CL-CLAIM-TYPE ' ' CL-CLAIM-NO ' ' CL-CERT-NO ' '
                 SORT-CODE
              RELEASE SORT-RECORD
              ADD 1                    TO CLM-RECS-OUT
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-READ-ELMSTR.

           READ ELMSTR NEXT RECORD

           IF (ELMSTR-FILE-STATUS = '10' OR '23')
              OR (CL-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ELMSTR - READ ' ELMSTR-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-INPUT 
              ADD 1                    TO CLM-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0210-GET-ELTRLR.

           MOVE SPACES                 TO WS-GOT-IT-SW

           PERFORM 0220-START-ELTRLR   THRU 0220-EXIT
           IF (ELTRLR-FILE-STATUS = '00')
      *       AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              PERFORM 0230-READ-ELTRLR THRU 0230-EXIT UNTIL
                 WE-GOT-IT-ALL
           END-IF

           .
       0210-EXIT.
           EXIT.

       0220-START-ELTRLR.

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +93                    TO AT-SEQUENCE-NO
           START ELTRLR KEY >= AT-CONTROL-PRIMARY
           IF ELTRLR-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ELTRLR - START ' ELTRLR-FILE-STATUS
                 ' ' CL-CONTROL-PRIMARY (2:19)
           END-IF

           .
       0220-EXIT.
           EXIT.

       0230-READ-ELTRLR.

           READ ELTRLR NEXT RECORD

           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              IF (DENIAL-TR)
                 AND (AT-DENIAL-DT NOT = SPACES AND LOW-VALUES)
                 AND (AT-RETRACTION-DT = SPACES OR LOW-VALUES)
                 AND (AT-DENIAL-DT > WS-BIN-LAST-EOM-DT)
                 AND (AT-DENIAL-DT <= BIN-RUN-DATE)
                 IF AT-DENIAL-REASON-CODE = LOW-VALUES
                    MOVE SPACES        TO AT-DENIAL-REASON-CODE
                 END-IF
                 MOVE AT-DENIAL-REASON-CODE
                                       TO SORT-CODE
                 SET WE-GOT-IT-ALL     TO TRUE
              END-IF
           ELSE
              SET WE-GOT-IT-ALL        TO TRUE
           END-IF

           .
       0230-EXIT.
           EXIT.

       0250-START-ELMSTR.

           MOVE LOW-VALUES             TO CL-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD

           START ELMSTR KEY >= CL-CONTROL-PRIMARY

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR START     ' ELMSTR-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           .
       0250-EXIT.
           EXIT.

       0400-OUTPUT-PROCEDURE.

           MOVE ' '                    TO WS-EOF-SW

           PERFORM 0410-RETURN-REC     THRU 0410-EXIT
           PERFORM 0420-PROCESS-RECS   THRU 0420-EXIT UNTIL
              END-OF-INPUT


           .
       0400-EXIT.
           EXIT.

       0410-RETURN-REC.

           RETURN SORT-FILE AT END
              SET END-OF-INPUT         TO TRUE
           END-RETURN

           .
       0410-EXIT.
           EXIT.

       0420-PROCESS-RECS.

           PERFORM 0430-BUILD-TABLE    THRU 0430-EXIT

           PERFORM 0410-RETURN-REC     THRU 0410-EXIT

           .
       0420-EXIT.
           EXIT.

       0430-BUILD-TABLE.

           IF SORT-STATE NOT = WS-STATE-CODE (T1)
              PERFORM VARYING T1 FROM +1 BY +1 UNTIL
                 (SORT-STATE = WS-STATE-CODE (T1))
                 OR (WS-STATE-CODE (T1) = SPACES)
                 OR (T1 > +59)
              END-PERFORM
              IF T1 > +59
                 DISPLAY ' EXCEEDED NO OF STATES ' T1 ' ' SORT-STATE
                 PERFORM ABEND-PGM
              ELSE
                 IF WS-STATE-CODE (T1) = SPACES
                    MOVE SORT-STATE    TO WS-STATE-CODE (T1)
                 END-IF
              END-IF
           END-IF
           IF SORT-CODE NOT = WS-DENIAL-CODE (T2)
              PERFORM VARYING T2 FROM +1 BY +1 UNTIL
                 (SORT-CODE = WS-DENIAL-CODE (T2))
                 OR (WS-DENIAL-CODE (T2) = SPACES)
                 OR (T2 > +29)
              END-PERFORM
              IF T2 > +29
                 DISPLAY ' EXCEEDED NO OF DEN CODES ' T2 ' ' SORT-CODE
              ELSE
                 IF WS-DENIAL-CODE (T2) = SPACES
                    DISPLAY ' FOUND ROUGE DENIAL CODE ' SORT-CODE
                    MOVE SORT-CODE     TO WS-DENIAL-CODE (T2)
                 END-IF
              END-IF
           END-IF
052614     EVALUATE TRUE           
052614        WHEN SORT-TYPE = 'L'
052614           MOVE +2                  TO T3
052614        WHEN SORT-TYPE = 'I'
052614           MOVE +3                  TO T3
052614        WHEN SORT-TYPE = 'G'
052614           MOVE +4                  TO T3
052614        WHEN SORT-TYPE = 'F'
052614           MOVE +5                  TO T3
100518        WHEN SORT-TYPE = 'O'
100518           MOVE +6                  TO T3
022122        WHEN SORT-TYPE = 'B'
022122           MOVE +7                  TO T3
022122        WHEN SORT-TYPE = 'H'
022122           MOVE +8                  TO T3
052614        WHEN OTHER
052614           MOVE +1                  TO T3
052614     END-EVALUATE

           IF SORT-STATE = 'FL'
              DISPLAY ' DEN CODE ' SORT-CODE ' '
                 WS-DENIAL-CODE (T2) ' DEN TYPE ' SORT-TYPE ' '
                 WS-DENIAL-TYPE (T1 T2 T3) ' COUNT ' SORT-COUNT
              DISPLAY ' SUBS ' T1 ' ' T2 ' ' T3
           END-IF

           ADD SORT-COUNT              TO WS-DENIAL-COUNT (T1 T2 T3)
           ADD SORT-COUNT              TO WS-DENIAL-COUNT (T1 30 T3)
           ADD SORT-COUNT              TO WS-DENIAL-COUNT (60 T2 T3)
           ADD SORT-COUNT              TO WS-DENIAL-COUNT (60 30 T3)

           .
       0430-EXIT.
           EXIT.

       0500-PRINT-REPORT.

           PERFORM 0510-PRINT1 THRU 0510-EXIT VARYING
              T1 FROM +1 BY +1 UNTIL 
              (T1 > +59)
              OR (WS-STATE-CODE (T1) = SPACES)

           IF WS-LINE-COUNT > 58
              MOVE 99                  TO WS-LINE-COUNT
           END-IF

           MOVE '0GRAND'               TO WS-DETAIL1
           MOVE WS-DETAIL1             TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           MOVE ' TOTALS'              TO WS-DETAIL1
           MOVE 'A&H'                  TO WS-D1-TYPE
           PERFORM VARYING T2 FROM +1 BY +1 UNTIL
              (T2 > +15)
              OR (WS-DENIAL-CODE (T2) = SPACES)
              MOVE WS-DENIAL-COUNT (60 T2 1)
                                       TO WS-D1-COUNT (T2)
           END-PERFORM
           MOVE WS-DENIAL-COUNT (60 30 1)
                                       TO WS-D1-COUNT (T2)

           MOVE WS-DETAIL1             TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           MOVE SPACES                 TO WS-DETAIL1
           MOVE 'LIFE'                 TO WS-D1-TYPE
           PERFORM VARYING T2 FROM +1 BY +1 UNTIL
              (T2 > +15)
              OR (WS-DENIAL-CODE (T2) = SPACES)
              MOVE WS-DENIAL-COUNT (60 T2 2)
                                       TO WS-D1-COUNT (T2)
           END-PERFORM

           MOVE WS-DENIAL-COUNT (60 30 2)
                                       TO WS-D1-COUNT (T2)

           MOVE WS-DETAIL1             TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT
052614
052614     MOVE SPACES                 TO WS-DETAIL1
052614     MOVE 'IU  '                 TO WS-D1-TYPE
052614     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
052614        (T2 > +15)
052614        OR (WS-DENIAL-CODE (T2) = SPACES)
052614        MOVE WS-DENIAL-COUNT (60 T2 3)
052614                                 TO WS-D1-COUNT (T2)
052614     END-PERFORM
052614
052614     MOVE WS-DENIAL-COUNT (60 30 3)
052614                                 TO WS-D1-COUNT (T2)
052614
052614     MOVE WS-DETAIL1             TO PRT
052614     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT
052614
052614     MOVE SPACES                 TO WS-DETAIL1
052614     MOVE 'GAP '                 TO WS-D1-TYPE
052614     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
052614        (T2 > +15)
052614        OR (WS-DENIAL-CODE (T2) = SPACES)
052614        MOVE WS-DENIAL-COUNT (60 T2 4)
052614                                 TO WS-D1-COUNT (T2)
052614     END-PERFORM
052614
052614     MOVE WS-DENIAL-COUNT (60 30 4)
052614                                 TO WS-D1-COUNT (T2)
052614
052614     MOVE WS-DETAIL1             TO PRT
052614     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT
052614
052614     MOVE SPACES                 TO WS-DETAIL1
052614     MOVE 'FAM '                 TO WS-D1-TYPE
052614     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
052614        (T2 > +15)
052614        OR (WS-DENIAL-CODE (T2) = SPACES)
052614        MOVE WS-DENIAL-COUNT (60 T2 5)
052614                                 TO WS-D1-COUNT (T2)
052614     END-PERFORM
052614
052614     MOVE WS-DENIAL-COUNT (60 30 5)
052614                                 TO WS-D1-COUNT (T2)
052614
052614     MOVE WS-DETAIL1             TO PRT
052614     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT
100518
100518     MOVE SPACES                 TO WS-DETAIL1
100518     MOVE 'OTH '                 TO WS-D1-TYPE
100518     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
100518        (T2 > +15)
100518        OR (WS-DENIAL-CODE (T2) = SPACES)
100518        MOVE WS-DENIAL-COUNT (60 T2 6)
100518                                 TO WS-D1-COUNT (T2)
100518     END-PERFORM
100518
100518     MOVE WS-DENIAL-COUNT (60 30 6)
100518                                 TO WS-D1-COUNT (T2)
100518
100518     MOVE WS-DETAIL1             TO PRT
100518     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

022122     MOVE SPACES                 TO WS-DETAIL1
022122     MOVE 'BRV '                 TO WS-D1-TYPE
022122     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
022122        (T2 > +15)
022122        OR (WS-DENIAL-CODE (T2) = SPACES)
022122        MOVE WS-DENIAL-COUNT (60 T2 7)
022122                                 TO WS-D1-COUNT (T2)
022122     END-PERFORM
022122
022122     MOVE WS-DENIAL-COUNT (60 30 7)
022122                                 TO WS-D1-COUNT (T2)
022122
022122     MOVE WS-DETAIL1             TO PRT
022122     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

022122     MOVE SPACES                 TO WS-DETAIL1
022122     MOVE 'HOS '                 TO WS-D1-TYPE
022122     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
022122        (T2 > +15)
022122        OR (WS-DENIAL-CODE (T2) = SPACES)
022122        MOVE WS-DENIAL-COUNT (60 T2 8)
022122                                 TO WS-D1-COUNT (T2)
022122     END-PERFORM
022122
022122     MOVE WS-DENIAL-COUNT (60 30 8)
022122                                 TO WS-D1-COUNT (T2)
022122
022122     MOVE WS-DETAIL1             TO PRT
022122     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           IF WS-LINE-COUNT > 56
              MOVE 99                  TO WS-LINE-COUNT
           END-IF

           DISPLAY ' LINE COUNT ' WS-LINE-COUNT
           MOVE '-REASON CODES '       TO WS-DETAIL1
           MOVE WS-DETAIL1             TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           PERFORM VARYING T2 FROM +1 BY +1 UNTIL
              T2 > +30
              DISPLAY ' ' T2 ' ' WS-DENIAL-CODE (T2)
                 ' ' WS-DENIAL-DESC (T2)
           END-PERFORM

           PERFORM VARYING T2 FROM +1 BY +2 UNTIL
              (T2 > +15)
              OR (WS-DENIAL-CODE (T2) = SPACES)
              MOVE SPACES              TO WS-DETAIL1
              STRING '  ' WS-DENIAL-CODE (T2) '   ' WS-DENIAL-DESC (T2)
                '   ' WS-DENIAL-CODE (T2 + 1) '   '
                WS-DENIAL-DESC (T2 + 1) DELIMITED BY SIZE
                INTO WS-DETAIL1
              END-STRING

              MOVE WS-DETAIL1             TO PRT
              PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT
           END-PERFORM

           .
       0500-EXIT.
           EXIT.

       0510-PRINT1.

           IF WS-LINE-COUNT > 58
              MOVE 99                  TO WS-LINE-COUNT
           END-IF

           MOVE '0'                    TO WS-DETAIL1
           MOVE WS-STATE-CODE (T1)     TO WS-D1-STATE

           MOVE 'A&H'                  TO WS-D1-TYPE
           PERFORM VARYING T2 FROM +1 BY +1 UNTIL
              (T2 > +15)
              OR (WS-DENIAL-CODE (T2) = SPACES)
              MOVE WS-DENIAL-COUNT (T1 T2 1)
                                       TO WS-D1-COUNT (T2)
           END-PERFORM
           MOVE WS-DENIAL-COUNT (T1 30 1)
                                       TO WS-D1-COUNT (T2)

           MOVE WS-DETAIL1             TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           MOVE SPACES                 TO WS-DETAIL1
           MOVE 'LIFE'                 TO WS-D1-TYPE
           MOVE +1                     TO T2
           PERFORM VARYING T2 FROM +1 BY +1 UNTIL
              (T2 > +15)
              OR (WS-DENIAL-CODE (T2) = SPACES)
              MOVE WS-DENIAL-COUNT (T1 T2 2)
                                       TO WS-D1-COUNT (T2)
           END-PERFORM

           MOVE WS-DENIAL-COUNT (T1 30 2)
                                       TO WS-D1-COUNT (T2)

           MOVE WS-DETAIL1             TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT
052614
052614     MOVE SPACES                 TO WS-DETAIL1
052614     MOVE 'IU  '                 TO WS-D1-TYPE
052614     MOVE +1                     TO T2
052614     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
052614        (T2 > +15)
052614        OR (WS-DENIAL-CODE (T2) = SPACES)
052614        MOVE WS-DENIAL-COUNT (T1 T2 3)
052614                                 TO WS-D1-COUNT (T2)
052614     END-PERFORM
052614
052614     MOVE WS-DENIAL-COUNT (T1 30 3)
052614                                 TO WS-D1-COUNT (T2)
052614
052614     MOVE WS-DETAIL1             TO PRT
052614     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT
052614
052614     MOVE SPACES                 TO WS-DETAIL1
052614     MOVE 'GAP  '                 TO WS-D1-TYPE
052614     MOVE +1                     TO T2
052614     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
052614        (T2 > +15)
052614        OR (WS-DENIAL-CODE (T2) = SPACES)
052614        MOVE WS-DENIAL-COUNT (T1 T2 4)
052614                                 TO WS-D1-COUNT (T2)
052614     END-PERFORM
052614
052614     MOVE WS-DENIAL-COUNT (T1 30 4)
052614                                 TO WS-D1-COUNT (T2)
052614
052614     MOVE WS-DETAIL1             TO PRT
052614     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT
052614
052614     MOVE SPACES                 TO WS-DETAIL1
052614     MOVE 'FAM '                 TO WS-D1-TYPE
052614     MOVE +1                     TO T2
052614     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
052614        (T2 > +15)
052614        OR (WS-DENIAL-CODE (T2) = SPACES)
052614        MOVE WS-DENIAL-COUNT (T1 T2 5)
052614                                 TO WS-D1-COUNT (T2)
052614     END-PERFORM
052614
052614     MOVE WS-DENIAL-COUNT (T1 30 5)
052614                                 TO WS-D1-COUNT (T2)
052614
052614     MOVE WS-DETAIL1             TO PRT
052614     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT
100518
100518     MOVE SPACES                 TO WS-DETAIL1
100518     MOVE 'OTH '                 TO WS-D1-TYPE
100518     MOVE +1                     TO T2
100518     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
100518        (T2 > +15)
100518        OR (WS-DENIAL-CODE (T2) = SPACES)
100518        MOVE WS-DENIAL-COUNT (T1 T2 6)
100518                                 TO WS-D1-COUNT (T2)
100518     END-PERFORM
100518
100518     MOVE WS-DENIAL-COUNT (T1 30 6)
100518                                 TO WS-D1-COUNT (T2)
100518
100518     MOVE WS-DETAIL1             TO PRT
100518     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

022122     MOVE SPACES                 TO WS-DETAIL1
022122     MOVE 'BRV '                 TO WS-D1-TYPE
022122     MOVE +1                     TO T2
022122     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
022122        (T2 > +15)
022122        OR (WS-DENIAL-CODE (T2) = SPACES)
022122        MOVE WS-DENIAL-COUNT (T1 T2 7)
022122                                 TO WS-D1-COUNT (T2)
022122     END-PERFORM
022122
022122     MOVE WS-DENIAL-COUNT (T1 30 7)
022122                                 TO WS-D1-COUNT (T2)
022122
022122     MOVE WS-DETAIL1             TO PRT
022122     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

022122     MOVE SPACES                 TO WS-DETAIL1
022122     MOVE 'HOS '                 TO WS-D1-TYPE
022122     MOVE +1                     TO T2
022122     PERFORM VARYING T2 FROM +1 BY +1 UNTIL
022122        (T2 > +15)
022122        OR (WS-DENIAL-CODE (T2) = SPACES)
022122        MOVE WS-DENIAL-COUNT (T1 T2 8)
022122                                 TO WS-D1-COUNT (T2)
022122     END-PERFORM
022122
022122     MOVE WS-DENIAL-COUNT (T1 30 8)
022122                                 TO WS-D1-COUNT (T2)
022122
022122     MOVE WS-DETAIL1             TO PRT
022122     PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           .
       0510-EXIT.
           EXIT.

       0700-WRITE-REPORT.

           IF WS-LINE-COUNT > WS-LINE-COUNT-MAX
              PERFORM 0750-WRITE-HEADINGS
                                       THRU 0750-EXIT
           END-IF

           EVALUATE P-CTL
              WHEN '1'
                 MOVE +1               TO WS-LINE-COUNT            
              WHEN ' '
                 ADD +1                TO WS-LINE-COUNT         
              WHEN '0'
                 ADD +2                TO WS-LINE-COUNT     
              WHEN OTHER
                 ADD +3                TO WS-LINE-COUNT
           END-EVALUATE

           PERFORM 0710-WRITE-A-LINE   THRU 0710-EXIT

           .
       0700-EXIT.
           EXIT.

       0710-WRITE-A-LINE.

           WRITE PRT

           .
       0710-EXIT.
           EXIT.

       0750-WRITE-HEADINGS.

           IF WS-H2-DATE = SPACES                      
              MOVE WS-CURRENT-DATE     TO WS-H2-DATE
              MOVE COMPANY-NAME        TO WS-H2-CLIENT-NAME
              MOVE ALPH-DATE           TO WS-H3-DATE
           END-IF

           ADD +1                      TO WS-PAGE
           MOVE WS-PAGE                TO WS-H3-PAGE
           MOVE PRT                    TO WS-SAVE-PRINT-RECORD
           MOVE ZERO                   TO WS-LINE-COUNT

           MOVE WS-HEADING1            TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           MOVE WS-HEADING2            TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           MOVE WS-HEADING3            TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           MOVE '-'                    TO WS-HEADING4
           MOVE ' STATE  TYPE   '      TO WS-HEADING5
         
           PERFORM VARYING H1 FROM +1 BY +1 UNTIL
              (H1 > +15)
              OR (WS-DENIAL-CODE (H1) = SPACES)
              MOVE WS-DENIAL-CODE (H1)
                                       TO WS-HEAD5-DETAIL (H1)
           END-PERFORM

           MOVE 'STATE'                TO WS-HEAD4-DETAIL (H1)
           MOVE 'TOTALS'               TO WS-HEAD5-DETAIL (H1)

           MOVE WS-HEADING4            TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           MOVE WS-HEADING5            TO PRT
           PERFORM 0700-WRITE-REPORT   THRU 0700-EXIT

           MOVE +8                     TO WS-LINE-COUNT

           MOVE WS-SAVE-PRINT-RECORD   TO PRT
           MOVE '0'                    TO P-CTL

           .
       0750-EXIT.
           EXIT.

       1000-CLOSE-FILES.

           CLOSE ELMSTR PRNTR ELTRLR ELDENY

           .
       1000-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
