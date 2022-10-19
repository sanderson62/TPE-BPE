       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL3241.
      *AUTHOR.     SUZAN.
      *DATE-COMPILED.
      *REMARKS.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
122602* 122602    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
092503* 092503                   PEMA  CHANGE LEN OF COMPANY NAME TO 30
121203* 121203    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
010704* 010704                   SMVA  DO NOT ABEND PGM ON ELTRLR STATUS 23
030904* 030904    2004102900003  SMVA  ADD TOTAL LINE FOR AMOUNT PAID
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
      ******************************************************************
           EJECT
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORT-FILE        ASSIGN TO SYS001-UT-3380-S-SORTWK1.
           SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.
           SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.
           SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS RF-CONTROL-PRIMARY
                                   FILE STATUS IS DTE-VSAM-FLAGS.

           SELECT ELMSTR-INFLE     ASSIGN TO ELMSTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-PRIMARY
                                   FILE STATUS IS ELMSTR-FILE-STATUS.

           SELECT ELTRLR-INFLE     ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

           EJECT
       DATA DIVISION.

       FILE SECTION.

       SD  SORT-FILE.
           01  SORT-REC.
               05  SORT-NONREPORT-FLDS.
                   12  SORT-CARRIER-ID     PIC X(01).
               05  SORT-DETAIL-LINE1.
                   12  FILLER              PIC X(63).
                   12  SORT-CHECK-NO       PIC X(07).
                   12  FILLER              PIC X(59).
               05  SORT-DETAIL-LINE2.
                   12  FILLER              PIC X(129).


       FD  DISK-DATE               COPY ELCDTEFD.

       FD  PRNTR                   COPY ELCPRTFD.

       FD  ELREPT.
                                   COPY ELCREPT.
           EJECT
       FD  ELMSTR-INFLE.
                                   COPY ELCMSTR.

           EJECT
       FD  ELTRLR-INFLE.
                                   COPY ELCTRLR.

           EJECT
       WORKING-STORAGE SECTION.
       01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.

       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*   EL3241 WORKING STORAGE     *'.
       77  FILLER  PIC X(32)   VALUE '********* VMOD=2.001 ***********'.

       01  FILLER                          COMP-3.
           05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.
           05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +59.
           05  WS-PAGE                     PIC S9(5)       VALUE ZERO.
           05  WS-REPORT-SW                PIC S9          VALUE +1.
           05  WS-PRINT-SW                 PIC S9          VALUE ZERO.
           05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.
           05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.
           05  WS-ZERO                     PIC S9          VALUE ZERO.
030904     05  WS-TOTAL-AMOUNT-PAID        PIC S9(07)V99   VALUE ZERO.

           EJECT
       01  FILLER                          COMP SYNC.
           05  PGM-SUB                     PIC S9(4)       VALUE +324.
           05  WS-INDEX                    PIC S9(4)       VALUE ZERO.

       01  FILLER.
           05  WS-PREV-CYCLE-DT            PIC XX     VALUE LOW-VALUES.
           05  WS-CURR-CYCLE-DT            PIC XX     VALUE LOW-VALUES.
           05  WS-PROC-DT                  PIC XX     VALUE LOW-VALUES.

           05  WS-ALPHA-DATE.
               10  WS-ALPHA-MONTH          PIC X(10)  VALUE SPACES.
               10  WS-ALPHA-DAY            PIC 9(02).
               10  FILLER                  PIC X(02)  VALUE ', '.
               10  WS-ALPHA-CC             PIC 9(02).
               10  WS-ALPHA-YY             PIC 9(02).

           05  WS-EOF-SW                   PIC X      VALUE SPACES.
               88  END-OF-ELTRLR                      VALUE 'Y'.
               88  END-OF-SORTFILE                    VALUE '1'.

           05  ABEND-CODE                  PIC X(4).
           05  ABEND-OPTION                PIC X.
           05  OLC-REPORT-NAME             PIC X(5)   VALUE '3241'.
           05  X                           PIC X      VALUE SPACE.

           05  WS-SAVE-PRINT-RECORD        PIC X(133) VALUE SPACES.

           05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.

           05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.
           05  ELTRLR-FILE-STATUS          PIC XX          VALUE ZERO.
           05  ELMSTR-FILE-STATUS          PIC XX          VALUE ZERO.

           05  WS-FILE-ERROR-MESSAGE.
               10  FILLER                  PIC X(24)       VALUE
                   'ERROR OCCURED OPENING - '.
               10  WS-FEM-FILE-NAME        PIC X(8).

           EJECT
       01  WS-HEADING1.
           05  FILLER                      PIC X(01)       VALUE '1'.
           05  FILLER                      PIC X(44)       VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(39)       VALUE
               'CLAIM PAYMENTS HELD FOR FUTURE PRINTING'.
           05  FILLER                      PIC X(28)       VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(06)       VALUE
               'EL3241'.
           05  FILLER                      PIC X(11)       VALUE SPACES.

       01  WS-HEADING2.
           05  FILLER                      PIC X(01)       VALUE SPACE.
           05  FILLER                      PIC X(52)       VALUE SPACES.
122602*    05  WS-H2-COMPANY-NAME          PIC X(23)       VALUE SPACES.
092503     05  WS-H2-COMPANY-NAME          PIC X(30)       VALUE SPACES.
           05  FILLER                      PIC X(29)       VALUE SPACES.
           05  WS-H2-DATE                  PIC X(08)       VALUE SPACES.
           05  FILLER                      PIC X(09)       VALUE SPACES.

       01  WS-HEADING3.
           05  FILLER                      PIC X(01)       VALUE SPACE.
           05  FILLER                      PIC X(46)       VALUE SPACES.
           05  FILLER                      PIC X(18)       VALUE
               'PAYMENTS RECORDED '.
           05  WS-H3-RECORDED-DATE         PIC X(18)       VALUE SPACES.
           05  FILLER                      PIC X(29)       VALUE SPACES.
           05  FILLER                      PIC X(05)       VALUE 'PAGE'.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.
           05  FILLER                      PIC X(06)       VALUE SPACES.

       01  WS-HEADING4.
           05  FILLER                      PIC X(01)       VALUE SPACE.
           05  FILLER                      PIC X(60)       VALUE SPACES.
           05  FILLER                      PIC X(09)       VALUE
               'CARRIER: '.
           05  WS-H4-CARRIER-ID            PIC X(01)       VALUE SPACE.
           05  FILLER                      PIC X(58)       VALUE SPACES.

       01  WS-HEADING5.
           05  FILLER                      PIC X(01)       VALUE '0'.
           05  FILLER                      PIC X(25)       VALUE SPACES.
           05  FILLER                      PIC X(13)       VALUE
               'CLAIM'.
           05  FILLER                      PIC X(07)       VALUE
               'CERT'.
           05  FILLER                      PIC X(09)       VALUE
               'CLAIM'.
           05  FILLER                      PIC X(09)       VALUE
               'CLAIM'.
           05  FILLER                      PIC X(25)       VALUE
               'CHECK'.
           05  FILLER                      PIC X(11)       VALUE
               'PMT'.
           05  FILLER                      PIC X(04)       VALUE
               'DATE'.
           05  FILLER                      PIC X(25)       VALUE SPACES.

       01  WS-HEADING6.
           05  FILLER                      PIC X(01)       VALUE SPACE.
           05  FILLER                      PIC X(25)       VALUE SPACES.
           05  FILLER                      PIC X(12)       VALUE
               'NUMBER'.
           05  FILLER                      PIC X(08)       VALUE
               'NUMBER'.
           05  FILLER                      PIC X(09)       VALUE
               'STATUS'.
           05  FILLER                      PIC X(09)       VALUE
               'TYPE'.
           05  FILLER                      PIC X(13)       VALUE
               'NUMBER'.
           05  FILLER                      PIC X(12)       VALUE
               'AMOUNT'.
           05  FILLER                      PIC X(08)       VALUE
               'TYPE'.
           05  FILLER                      PIC X(10)       VALUE
               'TO-BE-PAID'.
           05  FILLER                      PIC X(22)       VALUE SPACES.

           EJECT
       01  WS-DETAIL.
           03  WS-DETAIL-KEY.
               05  WS-DK-CARRIER-ID            PIC X(01).
           03  WS-DETAIL-LINE1.
               05  FILLER                      PIC X(01)   VALUE '0'.
               05  FILLER                      PIC X(24)   VALUE SPACES.
               05  WS-D1-CLAIM-NO              PIC X(07).
               05  FILLER                      PIC X(03)   VALUE SPACES.
               05  WS-D1-CERT-NO               PIC X(11).
               05  FILLER                      PIC X(01)   VALUE SPACE.
               05  WS-D1-CLAIM-STATUS          PIC X(06).
               05  FILLER                      PIC X(04)   VALUE SPACES.
               05  WS-D1-CLAIM-TYPE            PIC X(02).
               05  FILLER                      PIC X(04)   VALUE SPACES.
               05  WS-D1-CHECK-NO              PIC X(07).
               05  FILLER                      PIC X(03)   VALUE SPACES.
               05  WS-D1-AMOUNT-PAID           PIC $$ZZZZZZ.99.
               05  FILLER                      PIC X(03)   VALUE SPACES.
               05  WS-D1-PAYMENT-TYPE          PIC X(07).
               05  FILLER                      PIC X(03)   VALUE SPACES.
               05  WS-D1-DATE-TBPD             PIC X(10).
               05  FILLER                      PIC X(22)   VALUE SPACES.

           03  WS-DETAIL-LINE2.
               05  FILLER                      PIC X(01)   VALUE SPACE.
               05  FILLER                      PIC X(28)   VALUE SPACES.
               05  FILLER                      PIC X(10)   VALUE
                   'CLAIMANT: '.
               05  WS-D2-CLAIMANT-NAME         PIC X(30).
               05  FILLER                      PIC X(60)   VALUE SPACE.

030904 01  WS-TOTAL-LINE.
030904         05  FILLER                      PIC X(01)   VALUE SPACE.
030904         05  FILLER                      PIC X(47)   VALUE SPACES.
030904         05  FILLER                      PIC X(25)   VALUE 
030904             'TOTAL CLAIM PAYMENTS HELD'.
030904         05  WS-TL-AMOUNT-PAID           PIC $$,ZZZ,ZZZ.99.
030904         05  FILLER                      PIC X(49)   VALUE SPACES.
           EJECT
                                       COPY ELCDTECX.
           EJECT
                                       COPY ELCDTEVR.

      ************************************
      * COPYBOOK FOR DATE CONVERSION DATA

                                       COPY ELCDATE.

           EJECT
       LINKAGE SECTION.
       01  PARM.
           05  PARM-LEN      PIC S9(04) COMP.
           05  PARM-VALUE    PIC X(100).

       PROCEDURE DIVISION USING PARM.

       0000-DATE-CARD-READ. COPY ELCDTERX.

           PERFORM OPEN-FILES

122602     MOVE COMPANY-NAME             TO WS-H2-COMPANY-NAME
           SORT SORT-FILE  ASCENDING KEY SORT-CARRIER-ID SORT-CHECK-NO
              INPUT  PROCEDURE 1000-INPUT-PROCEDURE  THRU 1000-EXIT
              OUTPUT PROCEDURE 3000-OUTPUT-PROCEDURE THRU 3000-EXIT

           IF SORT-RETURN  NOT = ZEROS
              MOVE 'INTERNAL SORT ABORTED'
                                         TO  WS-ABEND-MESSAGE
              MOVE '0101'                TO  WS-RETURN-CODE
              PERFORM ABEND-PGM
           END-IF

           IF WS-RECORD-COUNT  EQUAL  ZERO
              PERFORM WRITE-HEADINGS
              MOVE '**** NO TRANSACTION FOR THIS REPORT ****'
                                         TO  P-DATA
              MOVE '-'                   TO  X
              PERFORM WRITE-A-LINE
030904     ELSE
030904        MOVE SPACES                TO PRT
030904        PERFORM WRITE-A-LINE
030904        MOVE WS-TOTAL-AMOUNT-PAID  TO WS-TL-AMOUNT-PAID
030904        MOVE WS-TOTAL-LINE         TO PRT
030904        PERFORM WRITE-A-LINE
           END-IF

           PERFORM CLOSE-FILES
           GOBACK
           .
       1000-INPUT-PROCEDURE     SECTION.

           PERFORM 1010-INIT    THRU 1010-EXIT
           PERFORM 1200-PROCESS THRU 1200-EXIT UNTIL
              END-OF-ELTRLR

           GO TO 1000-EXIT
           .
       1010-INIT.

           IF PARM-LEN = 0
              DISPLAY ' INVALID PARM '
              PERFORM ABEND-PGM
           END-IF

           MOVE PARM-VALUE (1:6)       TO DC-GREG-DATE-1-MDY
           MOVE '4'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-PREV-CYCLE-DT
              ELSE
              DISPLAY ' INVALID PREV DATE ' PARM-VALUE (1:6)
              PERFORM ABEND-PGM
           END-IF

           MOVE PARM-VALUE (7:6)       TO DC-GREG-DATE-1-MDY
           MOVE '4'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURR-CYCLE-DT
              MOVE SPACES              TO WS-ALPHA-MONTH
              MOVE DC-ALPHA-MONTH      TO WS-ALPHA-MONTH
              MOVE DC-ALPHA-DAY        TO WS-ALPHA-DAY
              MOVE DC-ALPHA-CENTURY    TO WS-ALPHA-CC
              MOVE DC-ALPHA-YEAR       TO WS-ALPHA-YY
           ELSE
              DISPLAY ' INVALID CURR DATE ' PARM-VALUE (7:6)
              PERFORM ABEND-PGM
           END-IF

      *    IF DC-DAY-OF-WEEK GREATER THAN +6
      *       MOVE +2                 TO  DC-ELAPSED-DAYS
      *       MOVE ZERO               TO  DC-ELAPSED-MONTHS
      *       MOVE '6'                TO  DC-OPTION-CODE
      *       PERFORM 8500-DATE-CONVERSION
      *       MOVE DC-BIN-DATE-2      TO  WS-PROC-DT
      *    ELSE
           IF DC-DAY-OF-WEEK GREATER THAN +5
              MOVE +3             TO  DC-ELAPSED-DAYS
              MOVE ZERO           TO  DC-ELAPSED-MONTHS
              MOVE '6'            TO  DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-BIN-DATE-2      TO  WS-PROC-DT
           ELSE
              MOVE +1                 TO  DC-ELAPSED-DAYS
              MOVE ZERO               TO  DC-ELAPSED-MONTHS
              MOVE '6'                TO  DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-BIN-DATE-2      TO  WS-PROC-DT
           END-IF
      *    END-IF

           MOVE LOW-VALUES TO AT-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD TO AT-COMPANY-CD
           START ELTRLR-INFLE KEY IS NOT < AT-CONTROL-PRIMARY

010704     EVALUATE TRUE
010704     WHEN ELTRLR-FILE-STATUS = '00'
               CONTINUE

010704     WHEN ELTRLR-FILE-STATUS = '23'
010704         SET END-OF-ELTRLR TO TRUE
010704         GO TO 1010-EXIT
 
010704     WHEN OTHER
              DISPLAY ' ELTRLR START ' ELTRLR-FILE-STATUS
              PERFORM ABEND-PGM
010704     END-EVALUATE

           PERFORM 1250-READ-ELTRLR THRU 1250-EXIT
           .
       1010-EXIT.
           EXIT.


       1200-PROCESS.

           IF  (AT-TRAILER-TYPE = '2'   AND
                AT-PAYMENT-ORIGIN = '1' AND
                AT-RECORDED-DT = WS-CURR-CYCLE-DT)

               IF  (AT-CHECK-WRITTEN-DT = LOW-VALUES AND
                    AT-VOID-DT = LOW-VALUES          AND
                    AT-TO-BE-WRITTEN-DT NOT = SPACES     AND
                    AT-TO-BE-WRITTEN-DT NOT = LOW-VALUES)

                   PERFORM 1270-READ-ELMSTR      THRU 1270-EXIT
                   PERFORM 1220-GET-REPORT-DATA  THRU 1220-EXIT
                   PERFORM 1900-RELEASE-SORT     THRU 1900-EXIT
               END-IF
           END-IF

           PERFORM 1250-READ-ELTRLR THRU 1250-EXIT
           .
       1200-EXIT.
           EXIT.

       1220-GET-REPORT-DATA.

           MOVE AT-CARRIER           TO WS-DK-CARRIER-ID

           INITIALIZE WS-DETAIL-LINE1

           MOVE AT-CLAIM-NO          TO  WS-D1-CLAIM-NO
           MOVE AT-CERT-NO           TO  WS-D1-CERT-NO


           IF CLAIM-IS-OPEN
               MOVE 'OPEN'           TO WS-D1-CLAIM-STATUS
           ELSE
               MOVE 'CLOSED'         TO WS-D1-CLAIM-STATUS
           END-IF


122602     EVALUATE TRUE
122602     WHEN AT-CLAIM-TYPE = 'A'
               MOVE 'AH'             TO WS-D1-CLAIM-TYPE

122602     WHEN AT-CLAIM-TYPE = 'L'
               MOVE 'LF'             TO WS-D1-CLAIM-TYPE

122602     WHEN AT-CLAIM-TYPE = 'I'
122602         MOVE 'IU'             TO WS-D1-CLAIM-TYPE

121203     WHEN AT-CLAIM-TYPE = 'G'
121203         MOVE 'GP'             TO WS-D1-CLAIM-TYPE
052614
052614     WHEN AT-CLAIM-TYPE = 'F'
052614         MOVE 'FL'             TO WS-D1-CLAIM-TYPE
100518
022122     WHEN AT-CLAIM-TYPE = 'B'
022122         MOVE 'BR'             TO WS-D1-CLAIM-TYPE
022122
022122     WHEN AT-CLAIM-TYPE = 'H'
022122         MOVE 'HS'             TO WS-D1-CLAIM-TYPE
100518
100518     WHEN AT-CLAIM-TYPE = 'O'
100518         MOVE 'OT'             TO WS-D1-CLAIM-TYPE

122602     END-EVALUATE


           MOVE AT-CHECK-NO          TO  WS-D1-CHECK-NO
           MOVE AT-AMOUNT-PAID       TO  WS-D1-AMOUNT-PAID
030904     ADD AT-AMOUNT-PAID        TO  WS-TOTAL-AMOUNT-PAID


           EVALUATE TRUE
           WHEN  PARTIAL-PAYMENT
                 MOVE 'PARTIAL'      TO  WS-D1-PAYMENT-TYPE

           WHEN  FINAL-PAYMENT  OR
                 LUMP-SUM-PAYMENT
                 MOVE 'FINAL'        TO  WS-D1-PAYMENT-TYPE

           WHEN  ADDITIONAL-PAYMENT
                 MOVE 'ADDL'         TO  WS-D1-PAYMENT-TYPE

           WHEN  OTHER
                 MOVE 'PARTIAL'      TO  WS-D1-PAYMENT-TYPE
           END-EVALUATE


           MOVE AT-TO-BE-WRITTEN-DT  TO  DC-BIN-DATE-1
           MOVE SPACES               TO  DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION

           IF  NO-CONVERSION-ERROR  OR
               DATE-IS-ZERO
               MOVE DC-GREG-DATE-A-EDIT  TO  WS-D1-DATE-TBPD
           ELSE
               MOVE ZERO                 TO  WS-D1-DATE-TBPD
           END-IF


           MOVE SPACES               TO WS-D2-CLAIMANT-NAME
           STRING CL-INSURED-LAST-NAME  DELIMITED BY SPACE
                  ' '                   DELIMITED BY SIZE
                  CL-INSURED-1ST-NAME   DELIMITED BY SPACE
                  ' '                   DELIMITED BY SIZE
                  CL-INSURED-MID-INIT   DELIMITED BY SPACE

                  INTO WS-D2-CLAIMANT-NAME
           .

       1220-EXIT.
           EXIT.


       1250-READ-ELTRLR.

           READ ELTRLR-INFLE NEXT RECORD

           IF ELTRLR-FILE-STATUS = '00'
              CONTINUE
           ELSE
              IF ELTRLR-FILE-STATUS = '10' OR '23'
                 SET END-OF-ELTRLR TO TRUE
              ELSE
                 DISPLAY ' ELTRLR READ  ' ELTRLR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           IF AT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD
              SET END-OF-ELTRLR    TO TRUE
           END-IF

           .
       1250-EXIT.
           EXIT.


       1270-READ-ELMSTR.

           IF AT-CONTROL-PRIMARY (1:20) = CL-CONTROL-PRIMARY
              CONTINUE
           ELSE
              MOVE AT-CONTROL-PRIMARY TO CL-CONTROL-PRIMARY
              READ ELMSTR-INFLE
              IF ELMSTR-FILE-STATUS = '00'
                 CONTINUE
              ELSE
                 DISPLAY ' ELMSTR READ  ' ELMSTR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           .

       1270-EXIT.
           EXIT.
           EJECT


       1900-RELEASE-SORT.

           RELEASE SORT-REC  FROM  WS-DETAIL
           .
       1900-EXIT.
           EXIT.

       1000-EXIT.
           EXIT.

       3000-OUTPUT-PROCEDURE     SECTION.

       3010-RETURN-LOOP.

           PERFORM 3020-RETURN-SORT THRU 3020-EXIT
               UNTIL END-OF-SORTFILE

           GO TO 3000-EXIT
           .
       3010-EXIT.
           EXIT.


       3020-RETURN-SORT.

           RETURN SORT-FILE  AT END
               SET END-OF-SORTFILE TO TRUE
               GO TO 3020-EXIT
           end-return
           PERFORM 3200-PRINT-REPORT THRU 3200-EXIT
           .
       3020-EXIT.
           EXIT.

       3200-PRINT-REPORT.

           MOVE SORT-DETAIL-LINE1      TO  PRT
           PERFORM WRITE-A-LINE

           MOVE SORT-DETAIL-LINE2      TO  PRT
           PERFORM WRITE-A-LINE
           ADD +1                      TO  WS-RECORD-COUNT
           .
       3200-EXIT.
           EXIT.

       3000-EXIT.
           EXIT.


           EJECT
       8500-DATE-CONVERSION SECTION. COPY ELCDCS.

           EJECT
       WRITE-A-LINE SECTION. COPY ELCWAL.

       WRITE-HEADINGS SECTION.

       WHS-010.
           IF  WS-H2-DATE EQUAL SPACES
               MOVE WS-CURRENT-DATE    TO  WS-H2-DATE
               MOVE WS-ALPHA-DATE      TO  WS-H3-RECORDED-DATE
           END-IF

           ADD +1  TO  WS-PAGE.
           MOVE WS-PAGE                TO  WS-H3-PAGE
           MOVE SORT-CARRIER-ID        TO  WS-H4-CARRIER-ID
           MOVE PRT                    TO  WS-SAVE-PRINT-RECORD
           MOVE ZERO                   TO  WS-LINE-COUNT

           MOVE WS-HEADING1            TO  PRT
           MOVE '1'                    TO  X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING2            TO  PRT
           MOVE ' '                    TO  X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING3            TO  PRT
           MOVE ' '                    TO  X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING4            TO  PRT
           MOVE ' '                    TO  X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING5            TO  PRT
           MOVE ' '                    TO  X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING6            TO  PRT
           PERFORM WRITE-PRINTER

           MOVE +8                     TO  WS-LINE-COUNT
           .
       WHS-020. COPY ELCWHS2.

       WRITE-PRINTER SECTION. COPY ELCWPS.

           IF DTE-PRT-OPT = 'S' OR 'T'
               IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)
                   OPEN I-O ELREPT
                   IF DTE-F-1 NOT = ZERO AND
                      DTE-VSAM-FLAGS NOT = '97'
                       MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS
                       MOVE 'ERROR OCCURED OPEN - ELREPT'
                                       TO  WS-ABEND-MESSAGE
                       PERFORM ABEND-PGM
                   ELSE
                       MOVE '1'                   TO REPT-OPEN
                       MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD
                       MOVE '1'                   TO RF-RECORD-TYPE
                       MOVE OLC-REPORT-NAME       TO RF-REPORT-ID
                       MOVE ZERO                  TO RF-LINE-NUMBER
                       START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY
                       PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT
                       MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD
                       MOVE '2'                   TO RF-RECORD-TYPE
                       MOVE OLC-REPORT-NAME       TO RF-REPORT-ID
                       MOVE ZERO                  TO RF-LINE-NUMBER
                       START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY
                       PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT
                       MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD
                       MOVE '1'                   TO RF-RECORD-TYPE
                       MOVE OLC-REPORT-NAME       TO RF-REPORT-ID
                       MOVE SPACES                TO RF-REPORT-LINE-133
                   END-IF
               END-IF
           END-IF

           IF DTE-ABEND-CD-1 = '81' AND
              DTE-PRT-OPT    = 'S'
               MOVE +0302  TO WS-RETURN-CODE
               PERFORM ABEND-PGM
           END-IF

           IF DTE-PRT-OPT = 'S' OR 'T'
               MOVE X      TO RF-CTL-CHAR-133
               MOVE P-DATA TO RF-DATA-133
                   IF DTE-ABEND-CD-1 = SPACES
                       ADD +1 TO DTE-TOT-LINES
                       MOVE DTE-TOT-LINES TO RF-LINE-NUMBER
                       WRITE REPORT-SAVE-FILE
                           INVALID KEY
                               MOVE '88' TO DTE-ABEND-CD-1
                               CLOSE ELREPT
                               MOVE SPACE TO REPT-OPEN
                   END-IF
           END-IF
           IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'
               MOVE X TO P-CTL
               WRITE PRT
           END-IF

           GO TO DTE-PRINT-EXIT
           .
       DTE-REPORT-DELETE.
           IF DTE-F-1 NOT = ZERO
               MOVE ZERO TO DTE-VSAM-FLAGS
               GO TO DTE-DELETE-EXIT
           END-IF

           READ ELREPT   NEXT RECORD
                 AT END   GO TO DTE-DELETE-EXIT

           IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND
              OLC-REPORT-NAME       = RF-REPORT-ID
               DELETE ELREPT RECORD
               GO TO DTE-REPORT-DELETE
           END-IF
           .
       DTE-DELETE-EXIT.
           EXIT.

       DTE-PRINT-EXIT.
           EXIT.
      ******************************************************************

           EJECT
       OPEN-FILES SECTION.

       OFS-010.
           OPEN INPUT ELMSTR-INFLE ELTRLR-INFLE
                OUTPUT PRNTR
           .
       OFS-EXIT.
           EXIT.

           EJECT
       CLOSE-FILES SECTION.

       CFS-010.

            IF REPT-OPEN NOT = SPACE
                MOVE '2'              TO RF-RECORD-TYPE
                ADD +1 TO DTE-TOT-LINES
                MOVE DTE-TOT-LINES    TO RF-LINE-NUMBER
                MOVE SPACES           TO RF-TRAILER-RECORD
                MOVE WS-TIME          TO RF-PRINT-HH-MM-SS
                MOVE WS-CURRENT-DATE  TO RF-CURRENT-DATE
                WRITE REPORT-SAVE-FILE
                IF DTE-VSAM-FLAGS NOT = ZEROS
                    DISPLAY 'ERROR DURING WRITE OF TYPE 2 REPORT REC'
                    DISPLAY 'VSAM RETURN CODE = ' DTE-VSAM-FLAGS
                    DISPLAY 'ERROR DURING WRITE OF TYPE 2 REPORT REC'
                        UPON CONSOLE
                    DISPLAY 'VSAM RETURN CODE = ' DTE-VSAM-FLAGS
                        UPON CONSOLE
                ELSE
                    DISPLAY '***************************************'
                    DISPLAY '***************************************'
                    DISPLAY '**               ' OLC-REPORT-NAME
                    DISPLAY '**  REPORT HAS BEEN SAVED IN THE     **'
                    DISPLAY '**  CLAS-IC ONLINE REPORT FILE       **'
                    DISPLAY '**                                   **'
                    DISPLAY '***************************************'
                    DISPLAY '***************************************'
                    CLOSE ELREPT
                END-IF
            END-IF

            IF DTE-ABEND-CD-1 = '81'
                DISPLAY '***************************************'
                DISPLAY '***************************************'
                DISPLAY '**               ' OLC-REPORT-NAME
                DISPLAY '**  UNABLE TO OPEN ONLINE FILE -     **'
                DISPLAY '**  REPORT PRINTED BUT NOT SAVED     **'
                DISPLAY '**                                   **'
                DISPLAY '***************************************'
                DISPLAY '***************************************'
                DISPLAY 'VSAM RETURN CODE WAS - ', DTE-VSAM-FLAGS
            END-IF

            IF DTE-ABEND-CD-1 = '88'
                DISPLAY '***************************************'
                DISPLAY '***************************************'
                DISPLAY '**               ' OLC-REPORT-NAME
                DISPLAY '**  REPORT FILE IS FULL-  REPORT WAS **'
                DISPLAY '**  PRINTED AND PARTIALLY SAVED      **'
                DISPLAY '**                                   **'
                DISPLAY '***************************************'
                DISPLAY '***************************************'
                DISPLAY 'VSAM RETURN CODE WAS - ', DTE-VSAM-FLAGS
            END-IF


            CLOSE ELMSTR-INFLE ELTRLR-INFLE
                 PRNTR
            .
       CFS-EXIT.
           EXIT.

       ABEND-PGM SECTION. COPY ELCABEND.

