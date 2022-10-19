       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCZX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
062922******************************************************************
062922*                   C H A N G E   L O G
062922*
062922* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062922*-----------------------------------------------------------------
062922*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062922* EFFECTIVE    NUMBER
062922*-----------------------------------------------------------------
062922* 062922  IR2022062000001  PEMA  FIX INVALID DATA
081222* 081222  IR2022081100003  PEMA  FIX INVALID DATA
062922******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCNOT           ASSIGN TO ERCNOT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CZ-CONTROL-PRIMARY
                                   FILE STATUS IS ERCNOT-FILE-STATUS.

           SELECT ERCNOT-OUT       ASSIGN TO ERCNOTOT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE     ASSIGN TO SYS019-FBA1-S-SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERCNOT.

           COPY ERCCNOT.

       FD  ERCNOT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  ERCNOT-OUT-REC              PIC X(228).

082603 FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCZX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  WS-RECS-IN                  PIC 9(9) VALUE ZEROS.
       77  WS-RECS-OUT                 PIC 9(9) VALUE ZEROS.
       77  WS-BAD-LINES                PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S9(5) VALUE +0 COMP-3.

       01  WS-MISC.
           05  PGM-SUB                 PIC S9(4)   VALUE +548.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.

       01  WS-SAVE-ERCNOT              PIC X(228) VALUE SPACES.

       01  ERCNOT-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-CERT-EFF-DT          PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-NOTE-TYPE            PIC X.
           12  EX-TAB7                 PIC X.
           12  EX-NOTE-SEQ             PIC ZZ99.
           12  EX-TAB8                 PIC X.
           12  EX-NOTE                 PIC X(65).
           12  EX-TAB9                 PIC X.
           12  EX-LAST-MAINT-USER      PIC X(4).
           12  EX-TAB10                PIC X.
           12  EX-LAST-MAINT-DT        PIC X(10).
           12  EX-TAB11                PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  WS-WORK-SEQ             PIC X.
           05  WS-NUM-SEQ REDEFINES WS-WORK-SEQ PIC 9.
           05  ERCNOT-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-INPUT  THRU 0100-EXIT UNTIL
                 (END-OF-INPUT)
PEMTST*          OR (WS-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' RECORDS READ    '  WS-RECS-IN
           DISPLAY ' RECORDS WRITTEN '  WS-RECS-OUT
           DISPLAY ' BAD LINES       '  WS-BAD-LINES
           GOBACK

           .
       0100-PROCESS-INPUT.

           PERFORM 0105-BUILD-EXTRACT  THRU 0105-EXIT

           PERFORM 0200-READ-ERCNOT    THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0105-BUILD-EXTRACT.

           MOVE WS-SAVE-ERCNOT         TO ERCNOT-DETAIL-RECORD
           MOVE CZ-CARRIER             TO EX-CARRIER
           MOVE CZ-GROUPING            TO EX-GROUPING
           MOVE CZ-STATE               TO EX-STATE
           MOVE CZ-ACCOUNT             TO EX-ACCOUNT

           MOVE CZ-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CERT-EFF-DT
           ELSE
              MOVE '01/01/1965'        TO EX-CERT-EFF-DT
              DISPLAY ' ERROR - EFF DT CONVERT ' CZ-CARRIER ' '
                 CZ-GROUPING ' ' CZ-STATE ' ' CZ-ACCOUNT ' '
                 CZ-CERT-NO
           END-IF

           IF CZ-CERT-SFX = X'00'
              MOVE 'U'                 TO CZ-CERT-SFX
              DISPLAY ' FIXING CERT SUFFIX ' CZ-CARRIER ' '
                 CZ-GROUPING ' ' CZ-STATE ' ' CZ-ACCOUNT ' '
                 EX-CERT-EFF-DT ' ' CZ-CERT-NO
           END-IF
           MOVE CZ-CERT-NO             TO EX-CERT-NO
           MOVE CZ-RECORD-TYPE         TO EX-NOTE-TYPE
           MOVE CZ-NOTE-SEQUENCE       TO EX-NOTE-SEQ


           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +60
              IF CZ-NOTE (S1:3) = X'434E04' OR X'434E9C'
                 DISPLAY ' REMOVING CONTENTS IN LINE ' CZ-CARRIER ' '
                    CZ-GROUPING ' ' CZ-STATE ' ' CZ-ACCOUNT ' '
                    EX-CERT-EFF-DT ' ' CZ-CERT-NO ' ' CZ-NOTE
                 MOVE SPACES        TO CZ-NOTE
                 MOVE +61           TO S1
                 ADD 1              TO WS-BAD-LINES
              END-IF
           END-PERFORM


081222     INSPECT cz-note REPLACING
081222        ALL X'3B' BY ' '  *>  ;
081222        ALL X'00' BY ' '
081222        ALL X'0A' BY ' '
081222        ALL X'0D' BY ' '
081222        ALL X'1E' BY ' '  *>  RS

           MOVE CZ-NOTE                TO EX-NOTE

           MOVE CZ-LAST-MAINT-USER     TO EX-LAST-MAINT-USER
           MOVE CZ-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
           ELSE
              MOVE '01/01/1753'        TO EX-LAST-MAINT-DT
              DISPLAY ' ERROR - MAINT DT CONVERT ' CZ-CARRIER ' '
                 CZ-GROUPING ' ' CZ-STATE ' ' CZ-ACCOUNT ' '
                 CZ-CERT-NO
           END-IF

           PERFORM 0300-WRITE-OUTPUT   THRU 0300-EXIT

           .
       0105-EXIT.
           EXIT.

       0200-READ-ERCNOT.

           READ ERCNOT NEXT RECORD

           IF ERCNOT-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCNOT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCNOT READ ' ERCNOT-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              ELSE
                 IF CZ-COMPANY-CD > DTE-CLASIC-COMPANY-CD
                    SET END-OF-INPUT  TO TRUE
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-INPUT
              ADD 1 TO WS-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-OUTPUT.

           INSPECT ERCNOT-DETAIL-RECORD REPLACING ALL ';' BY ' '
              ALL X'00' BY ' '
062922        ALL X'1E' BY ' '
              ALL X'09' BY ';'
           WRITE ERCNOT-OUT-REC        FROM ERCNOT-DETAIL-RECORD
           ADD 1 TO WS-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERCNOT
               OUTPUT ERCNOT-OUT

           IF ERCNOT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERCNOT - OPEN  ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERCNOT ERCNOT-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-ERCNOT.

           MOVE DTE-CLASIC-COMPANY-CD  TO CZ-CONTROL-PRIMARY

           START ERCNOT KEY > CZ-CONTROL-PRIMARY

           IF ERCNOT-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCNOT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCNOT - START  ' ERCNOT-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO ERCNOT-DETAIL-RECORD

           MOVE X'09'                  TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
                                          EX-TAB10
                                          EX-TAB11

           MOVE 'E'                    TO EX-EOR

           MOVE ERCNOT-DETAIL-RECORD   TO WS-SAVE-ERCNOT
           PERFORM 0550-START-ERCNOT   THRU 0550-EXIT
           PERFORM 0200-READ-ERCNOT    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.              COPY ELCABEND.
