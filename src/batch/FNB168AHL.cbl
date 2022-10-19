       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB168AHL.
       AUTHOR.        PABLO.
       DATE-COMPILED.

070904*  This program currently only runs for AHL
070904******************************************************************
070904*                   C H A N G E   L O G
070904*
070904* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070904*-----------------------------------------------------------------
070904*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070904* EFFECTIVE    NUMBER
070904*-----------------------------------------------------------------
070904* 070904                   PEMA  PROCESS REIN RECS ONLY TO SYNC
070904*                                CLMRSRV INTERFACE W/ 45A & 20R
032905* 032905    2005030300002  PEMA  LIMIT FEED TO REIN CO 300 AND 500
122205* 122205    2005033100001  PEMA  ADD PROCESSING FOR CSI
110807* 110807    2007110500002  PEMA  ADD REIN CO 700 FOR CSI
110907* 110907  IR2007110500002  PEMA  SEPERATE CSI FROM DCC
101708* 101708    2008050500001  AJRA  ADD PROCESSING FOR CCC
021312* 021312    2011110200001  AJRA  AHL PROCESSING
042512* 042512  IR2012042500001  AJRA  FORCE CYCLE DATE INTO FX DATES
070904******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EPEC-IN              ASSIGN TO SYS010.
           SELECT SORT-EPECS           ASSIGN TO SORTWK1.
           SELECT EXTRACT              ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DISK-DATE            ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  EPEC-IN
                                       COPY ECSEPCFD.
                                       COPY ECSEPC01.

           EJECT
       SD  SORT-EPECS.

       01  SORT-EPEC-REC.
           05  SORT-KEY.
122205         10  SORT-CARRIER        PIC X.
               10  SORT-STATE          PIC XX.
           05  SORT-CLM-DU             PIC S9(9)V99 COMP-3.
           05  SORT-CLM-PV             PIC S9(9)V99 COMP-3.
           05  SORT-CLM-IBNR           PIC S9(11)V99 COMP-3.
           05  SORT-CLM-LOSS-RESV      PIC S9(11)V99 COMP-3.
           05  FILLER                  PIC X(6).

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD.                                                 
           COPY FNC022.                                                 

       FD  DISK-DATE
                                       COPY ELCDTEFD.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     FNB168   WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-EPEC                    VALUE 'Y'.
           88  MORE-EPECS                     VALUE 'N'.
       77  WS-EPEC-IN                  PIC 9(9) VALUE ZEROS.
       77  WS-EXT-OUT                  PIC 9(9) VALUE ZEROS.

       01  WS-BIG-TABLE.
           05  WS-KEY.
122205         10  WS-CARRIER          PIC X.
               10  WS-STATE            PIC XX.
           05  WS-CLM-DU               PIC S9(9)V99 COMP-3.
           05  WS-CLM-PV               PIC S9(9)V99 COMP-3.
           05  WS-CLM-IBNR             PIC S9(11)V99 COMP-3.
           05  WS-CLM-LOSS-RESV        PIC S9(11)V99 COMP-3.
           05  FILLER                  PIC X(6) VALUE SPACES.

       01  WS-MISC.
           05  SYSTEM-DATE.
               10  SYS-MO              PIC 99  VALUE 00.
               10  SYS-DA              PIC 99  VALUE 00.
               10  SYS-CCYY            PIC 9(4) VALUE 0000.
           05  WS-BIN-AH-END-DATE      PIC XX  VALUE LOW-VALUES.
           05  WS-END-YEAR             PIC 9(11)  VALUE ZEROS.
           05  FILLER REDEFINES WS-END-YEAR.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9(4).
               10  WS-MMDD             PIC 9(4).

042512     05  DATE-SW             PIC X     VALUE ' '.                 
042512         88  VALID-DATE                VALUE 'V'.                 
042512     05  DUMP                PIC X     VALUE ' '.                 
042512     05  FORCE-DUMP REDEFINES DUMP PIC S9 COMP-3.                 
          EJECT

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
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC 9(4).
               10  WS-WORK-MMDD        PIC 9(4).

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.
                                       COPY ELCFUNDT.

042512 LINKAGE SECTION.                                                 
042512                                                                        
042512 01  PARM.                                                        
042512     05  PARM-LENGTH      PIC S9(4)   COMP.                       
042512     05  CYCLE-DATE       PIC X(8).                               
                                                                        
042512 PROCEDURE DIVISION USING PARM.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           SORT SORT-EPECS ON ASCENDING KEY SORT-KEY
                INPUT PROCEDURE 0002-INPUT THRU 0002-EXIT
                OUTPUT PROCEDURE 0003-OUTPUT THRU 0003-EXIT

           DISPLAY ' EPEC IN           ' WS-EPEC-IN
           DISPLAY ' EXT OUT           ' WS-EXT-OUT

           GOBACK

             .
       0002-INPUT SECTION.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT

           .
       0002-EXIT.
           EXIT.

       0003-OUTPUT SECTION.

           SET MORE-EPECS              TO TRUE

           PERFORM 0070-RETURN-REC     THRU 0070-EXIT

           MOVE SORT-KEY               TO WS-KEY

           PERFORM 0075-PROCESS-RECS THRU 0075-EXIT UNTIL
                END-OF-EPEC

           PERFORM 0076-BREAK THRU 0076-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           .
       0003-EXIT.
           EXIT.


       0010-INITIALIZE.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           MOVE WS-FN-MO               TO SYS-MO
           MOVE WS-FN-DA               TO SYS-DA
           MOVE WS-FN-CCYY             TO SYS-CCYY
122205     MOVE SPACE                  TO WS-KEY

           MOVE +0                     TO WS-CLM-DU
                                          WS-CLM-PV
                                          WS-CLM-IBNR
                                          WS-CLM-LOSS-RESV
                                          
042512     CALL 'DATEEDIT' USING CYCLE-DATE,  DATE-SW                   
042512     IF NOT VALID-DATE                                            
042512         DISPLAY 'INVALID PARAMETER DATE: ' CYCLE-DATE            
042512         ADD +1 TO FORCE-DUMP
042512     END-IF
                                          

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EPEC-IN      
               OUTPUT EXTRACT

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE EPEC-IN
                 EXTRACT

           .
       0030-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           PERFORM 0080-PROCESS-EPEC   THRU 0080-EXIT UNTIL
                 END-OF-EPEC

           .
       0050-EXIT.
           EXIT.

       0060-READ-EPEC.

           READ EPEC-IN AT END
               SET END-OF-EPEC         TO TRUE
           END-READ

           IF NOT END-OF-EPEC
              ADD 1                    TO WS-EPEC-IN
           END-IF
           
           .
       0060-EXIT.
           EXIT.

       0070-RETURN-REC.

           RETURN SORT-EPECS    AT END
               SET END-OF-EPEC         TO TRUE
           END-RETURN

           .
       0070-EXIT.
           EXIT.

       0075-PROCESS-RECS.

           IF SORT-KEY NOT  = WS-KEY
              PERFORM 0076-BREAK THRU 0076-EXIT
           ELSE
              ADD SORT-CLM-DU        TO WS-CLM-DU  
              ADD SORT-CLM-PV        TO WS-CLM-PV 
              ADD SORT-CLM-IBNR      TO WS-CLM-IBNR
              ADD SORT-CLM-LOSS-RESV TO WS-CLM-LOSS-RESV
           END-IF

           PERFORM 0070-RETURN-REC THRU 0070-EXIT

           .
       0075-EXIT.
           EXIT.

       0076-BREAK.

           MOVE SPACES                 TO EXTRACT-RECORD
                                                                        
021312     MOVE 'CRLOGC'               TO FX-SOURCE-CODE                          
103002     MOVE '30'                   TO FX-TRAN-TYPE  
021312     IF WS-CARRIER = '8'
021312        MOVE '34'                TO FX-TRAN-TYPE
021212     END-IF                          
021312     MOVE '18'                   TO FX-SUB-TYPE
021312*     MOVE 'AHLCLMRSV '           TO FX-SYSTEM
021312     MOVE '02'                   TO FX-DIVISION                             
021312     MOVE 'S'                    TO FX-FY-REN                        
042512     IF DTE-CLIENT = 'AHL'
042512        MOVE CYCLE-DATE          TO FX-JOURNAL-DATE
042412                                    FX-POSTING-DATE
042512     ELSE
           MOVE SYSTEM-DATE            TO FX-JOURNAL-DATE
           STRING RUN-MO RUN-DA RUN-CCYR DELIMITED BY SIZE
              INTO FX-POSTING-DATE
           END-STRING
042512     END-IF
           MOVE WS-STATE               TO FX-STATE

021312**** THE '*' IN POSITION 250 ENSURES A 250 BYTE RECORD IS PASSED
021312**** TO FREEDOM - PREVENTS TRUNCATION OF BLANK FIELDS
021312     MOVE '*'                      TO EXTRACT-RECORD(250:1)

021312*     COMPUTE FX-AMOUNT = WS-CLM-DU + WS-CLM-PV
021312*                       + WS-CLM-IBNR + WS-CLM-LOSS-RESV

021312*        IF WS-CLM-DU <> 0 
021312*           MOVE 'AHL DU RSV'     TO FX-SYSTEM
021312*           MOVE WS-CLM-DU        TO FX-AMOUNT
021412*           MOVE 'LF DU RESERVE  ' TO FX-DESCRIPTION
021312*           WRITE EXTRACT-RECORD
021312*        END-IF
021312*        IF WS-CLM-PV <> 0 
021312*           MOVE 'AHL PV RSV'     TO FX-SYSTEM
021312*           MOVE WS-CLM-PV        TO FX-AMOUNT
021412*           MOVE 'LF PV RESERVE  ' TO FX-DESCRIPTION
021312*           WRITE EXTRACT-RECORD
021312*        END-IF
021312        IF WS-CLM-IBNR <> 0 
021312           MOVE 'AHL LFIBNR'     TO FX-SYSTEM
021312           MOVE WS-CLM-IBNR      TO FX-AMOUNT
021412           MOVE 'LF IBNR RESERVE' TO FX-DESCRIPTION
021312           WRITE EXTRACT-RECORD
021312        END-IF
021312*        IF WS-CLM-LOSS-RESV <> 0 
021312*           MOVE 'AHLLOSSRSV'     TO FX-SYSTEM
021312*           MOVE WS-CLM-LOSS-RESV TO FX-AMOUNT
021412*           MOVE 'LF LOSS RESERVE' TO FX-DESCRIPTION
021312*           WRITE EXTRACT-RECORD
021312*        END-IF

      *     WRITE EXTRACT-RECORD

           MOVE SORT-EPEC-REC          TO WS-BIG-TABLE

           .
       0076-EXIT.
           EXIT.

       0080-PROCESS-EPEC.

           IF EP-RECORD-ID = 'EP'
021312        IF (EP-REIN <> 'R')
                 AND (EP-PURGE = ' ')
                 AND (EP-RUN-DTE = RUN-DATE)
021312           AND (EP-RCD-TYPE = 'L')
                 MOVE EP-STATE         TO WS-STATE
                 MOVE EP-CLM-DU        TO WS-CLM-DU
                 MOVE EP-CLM-PV        TO WS-CLM-PV
                 MOVE EP-CLM-IBNR      TO WS-CLM-IBNR
                 MOVE EP-LOSS-RESV     TO WS-CLM-LOSS-RESV
                 MOVE EP-CARRIER       TO WS-CARRIER
                 PERFORM 0090-RELEASE  THRU 0090-EXIT
              END-IF
           END-IF
           
           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-RELEASE.

           RELEASE SORT-EPEC-REC       FROM WS-BIG-TABLE
           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           .
       0090-EXIT.
           EXIT.


       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
