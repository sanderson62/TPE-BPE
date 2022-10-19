       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCRX3.
       AUTHOR.     PABLO.
       DATE-COMPILED.
031811******************************************************************
031811*                   C H A N G E   L O G
031811*
031811* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031811*-----------------------------------------------------------------
031811*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031811* EFFECTIVE    NUMBER
031811*-----------------------------------------------------------------
031811* 031811 CR2011012700001   PEMA  ADD ACCT STATUS S - SUSPENDED
021916* 021916 CR2014010900001   TANA  ADD ACCT STATUS D,L,R,P
031811******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERT-FILE-IN     ASSIGN TO CERTIN.

           SELECT ERACCT           ASSIGN TO ERACCTT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS ERACCT-KEY
                                   FILE STATUS IS ERACCT-FILE-STATUS.


           SELECT EXTR-FILE-OUT    ASSIGN TO EXTROT.
      *       ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  CERT-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSCRT01.

       FD  ERACCT.
       
       01  ERACCT-IN-REC.
           05  FILLER                  PIC XX.
           05  ERACCT-KEY.
               10  ERACCT-COMP-CD      PIC X.
               10  ERACCT-ACCT-KEY     PIC X(19).
               10  ERACCT-EXP-DATE     PIC 9(11)  COMP-3.
           05  ERACCT-VG-KEY           PIC X(26).
           05  ERACCT-LAST-MAINT       PIC X(12).
           05  ERACCT-BIN-EFF-DT       PIC XX.
           05  FILLER                  PIC X(1932).


       FD  EXTR-FILE-OUT 
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-FILE-OUT-REC           PIC X(404).

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCRX3  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  ERACCT-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-CERT               VALUE 'Y'.
       77  WS-CERT-ACCT-KEY        PIC X(19) VALUE LOW-VALUES.
       77  CRT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                 PIC S999    COMP    VALUE +020.
       77  WS-EXPIRE-DT            PIC 9(11)   VALUE ZEROS.
       77  WS-CANCEL-DT            PIC 9(11)   VALUE ZEROS.
       77  WS-LF-CANCEL-DT         PIC 9(11)   VALUE ZEROS.
       77  WS-AH-CANCEL-DT         PIC 9(11)   VALUE ZEROS.
       77  WS-EFF-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-CAN-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EXP-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-BIN-12-MONTHS-AGO    PIC XX    VALUE LOW-VALUES.
       77  WS-BIN-37-MONTHS-AGO    PIC XX    VALUE LOW-VALUES.
       77  WS-BIN-36-MONTHS-AGO    PIC XX    VALUE LOW-VALUES.
       77  WS-BIN-LOW-EFF-DT       PIC XX    VALUE LOW-VALUES.
       77  WS-LAST-MONTH-DT        PIC 9(8)  VALUE ZEROS.
       77  WS-12-MONTHS-AGO        PIC 9(8)  VALUE ZEROS.
       77  WS-37-MONTHS-AGO        PIC 9(8)  VALUE ZEROS.
       77  WS-36-MONTHS-AGO        PIC 9(8)  VALUE ZEROS.
       77  WS-PREV-ACCT-KEY        PIC X(19) VALUE LOW-VALUES.
       77  WS-END-OF-CERTS-SW      PIC X     VALUE SPACES.
           88  END-OF-CERTS-FOR-THIS-ACCT    VALUE 'Y'.
       77  WS-PROC-DT                  PIC X(10).
       01  WS-BUS-TYPE                 PIC 99.
           88  WS-AUTO                     VALUE 01.
           88  WS-MARINE                   VALUE 06.
           88  WS-TV                       VALUE 07.
           88  WS-MUSIC                    VALUE 08.
           88  WS-MOBILE-HOME              VALUE 09.
           88  WS-HOME-IMPR                VALUE 10.
           88  WS-SWIMMING-POOL            VALUE 11.
           88  WS-USED-CARS                VALUE 14.
           88  WS-MOTORCYCLE               VALUE 15.
           88  WS-RECREATION-VEH           VALUE 18.
           88  WS-FURNITURE                VALUE 19.
           88  WS-DESIRED-GROUP            VALUE 01 06 07 08 09
                                              10 11 14 15 18 19.
       01  EXTR-DETAIL-RECORD.
           12  EX-ACCT-FIRST-TIME      PIC X.
           12  EX-DEL-1                PIC X.
           12  EX-STATE                PIC XX.
           12  EX-DEL-2                PIC X.
           12  EX-ACCOUNT-NO           PIC X(10).
           12  EX-DEL-3                PIC X.
           12  EX-ACCOUNT-NAME         PIC X(35).
           12  EX-DEL-4                PIC X.
           12  EX-ADDR1                PIC X(35).
           12  EX-DEL-5                PIC X.
           12  EX-CITY                 PIC X(35).
           12  EX-DEL-6                PIC X.
           12  EX-ZIP                  PIC X(9).
           12  EX-DEL-7                PIC X.
           12  EX-RPT-CDE1             PIC X(10).
           12  EX-DEL-8                PIC X.
           12  EX-RPT-CDE2             PIC X(10).
           12  EX-DEL-9                PIC X.
           12  EX-ACCT-ORIG-EFF-DT     PIC X(10).
           12  EX-DEL-10               PIC X.
           12  EX-ACCT-EXP-DT          PIC X(10).
           12  EX-DEL-11               PIC X.
           12  EX-ACCT-STATUS          PIC X(11).
           12  EX-DEL-12               PIC X.
           12  EX-INFORCE-CNT          PIC 9(9).
           12  EX-DEL-13               PIC X.
           12  EX-L12-ISS-CNT          PIC 9(9).
           12  EX-DEL-14               PIC X.
           12  EX-L12-EXPIRED-CNT      PIC 9(9).
           12  EX-DEL-15               PIC X.
           12  EX-L12-DTH-CLAIM-CNT    PIC 9(9).
           12  EX-DEL-16               PIC X.
           12  EX-L12-CANCEL-CNT       PIC 9(9).
           12  EX-DEL-17               PIC X.
           12  EX-L12-CANC-RATIO       PIC 999.
           12  EX-DEL-18               PIC X.
           12  EX-L12-ISS-LF-PREM      PIC ZZZZZZZZ9.99.
           12  EX-DEL-19               PIC X.
           12  EX-L12-REF-LF-PREM      PIC ZZZZZZZZ9.99.
           12  EX-DEL-20               PIC X.
           12  EX-L12-ISS-AH-PREM      PIC ZZZZZZZZ9.99.
           12  EX-DEL-21               PIC X.
           12  EX-L12-REF-AH-PREM      PIC ZZZZZZZZ9.99.
           12  EX-DEL-22               PIC X.
           12  EX-ITD-ISS-CNT          PIC 9(9).
           12  EX-DEL-23               PIC X.
           12  EX-ITD-EXPIRED-CNT      PIC 9(9).
           12  EX-DEL-24               PIC X.
           12  EX-ITD-DTH-CLAIM-CNT    PIC 9(9).
           12  EX-DEL-25               PIC X.
           12  EX-ITD-CANCEL-CNT       PIC 9(9).
           12  EX-DEL-26               PIC X.
           12  EX-ITD-CANC-RATIO       PIC 999.
           12  EX-DEL-27               PIC X.
           12  EX-ITD-ISS-LF-PREM      PIC ZZZZZZZZ9.99.
           12  EX-DEL-28               PIC X.
           12  EX-ITD-REF-LF-PREM      PIC ZZZZZZZZ9.99.
           12  EX-DEL-29               PIC X.
           12  EX-ITD-ISS-AH-PREM      PIC ZZZZZZZZ9.99.
           12  EX-DEL-30               PIC X.
           12  EX-ITD-REF-AH-PREM      PIC ZZZZZZZZ9.99.
           12  EX-DEL-31               PIC X.
           12  EX-PROC-DT              PIC X(10).
           12  EX-DEL-32               PIC X.
           12  EX-EOR                  PIC X.

       01  WS-ITD-EXTR-WORK.
           05  WS-ITD-CANC-RATIO       PIC 9(5)V9(5)       VALUE ZEROS.
           05  WS-ITD-ISS-CNT          PIC 9(9)            VALUE ZEROS.
           05  WS-ITD-GROSS-LF-PREM    PIC S9(9)V99 COMP-3 VALUE +0.
           05  WS-ITD-REF-LF-PREM      PIC S9(9)V99 COMP-3 VALUE +0.
           05  WS-ITD-GROSS-AH-PREM    PIC S9(9)V99 COMP-3 VALUE +0.
           05  WS-ITD-REF-AH-PREM      PIC S9(9)V99 COMP-3 VALUE +0.
           05  WS-INFORCE-CNT          PIC 9(9)            VALUE ZEROS.
           05  WS-ITD-EXPIRED-CNT      PIC 9(9)            VALUE ZEROS.
           05  WS-ITD-DTH-CLAIM-CNT    PIC 9(9)            VALUE ZEROS.
           05  WS-ITD-CANCEL-CNT       PIC 9(9)            VALUE ZEROS.
       01  WS-L12-EXTR-WORK.
           05  WS-L12-CANC-RATIO       PIC 9(5)V9(5)       VALUE ZEROS.
           05  WS-L12-ISS-CNT          PIC 9(9)            VALUE ZEROS.
           05  WS-L12-GROSS-LF-PREM    PIC S9(9)V99 COMP-3 VALUE +0.
           05  WS-L12-REF-LF-PREM      PIC S9(9)V99 COMP-3 VALUE +0.
           05  WS-L12-GROSS-AH-PREM    PIC S9(9)V99 COMP-3 VALUE +0.
           05  WS-L12-REF-AH-PREM      PIC S9(9)V99 COMP-3 VALUE +0.
           05  WS-L12-EXPIRED-CNT      PIC 9(9)            VALUE ZEROS.
           05  WS-L12-DTH-CLAIM-CNT    PIC 9(9)            VALUE ZEROS.
           05  WS-L12-CANCEL-CNT       PIC 9(9)            VALUE ZEROS.
      ******************************************************************
      ******************************************************************
       01  WS-MISC.
           05  WS-SAVE-EXTR            PIC X(404) VALUE LOW-VALUES.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.


                                       COPY ERCACCT.
                                       COPY ELCCALC.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.                       

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS        THRU 0050-EXIT UNTIL
              ERACCT-KEY = HIGH-VALUES
      *       (END-OF-CERT)
PEMTST*       OR (CRT-RECS-IN > 50000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ     '  CRT-RECS-IN
           DISPLAY ' EXTRACTS WRITTEN      '  EXT-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT CERT-FILE-IN ERACCT
               OUTPUT EXTR-FILE-OUT

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              DISPLAY ' PROCESS DATE    ' DC-GREG-DATE-CYMD
                 '  ' DC-GREG-DATE-A-EDIT
              MOVE DC-GREG-DATE-A-EDIT TO WS-PROC-DT
           END-IF

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
           MOVE ZEROS                  TO EX-ITD-CANC-RATIO
                                          EX-ITD-ISS-CNT
                                          EX-INFORCE-CNT
                                          EX-ITD-EXPIRED-CNT
                                          EX-ITD-CANCEL-CNT
                                          EX-ITD-DTH-CLAIM-CNT
                                          EX-ITD-ISS-LF-PREM
                                          EX-ITD-REF-LF-PREM
                                          EX-ITD-ISS-AH-PREM
                                          EX-ITD-REF-AH-PREM

           MOVE ZEROS                  TO EX-L12-CANC-RATIO
                                          EX-L12-ISS-CNT
                                          EX-L12-EXPIRED-CNT
                                          EX-L12-CANCEL-CNT
                                          EX-L12-DTH-CLAIM-CNT
                                          EX-L12-ISS-LF-PREM
                                          EX-L12-REF-LF-PREM
                                          EX-L12-ISS-AH-PREM
                                          EX-L12-REF-AH-PREM

           MOVE WS-PROC-DT             TO EX-PROC-DT
           MOVE 'E'                    TO EX-EOR

           MOVE ';'                    TO EX-DEL-1
                                          EX-DEL-2
                                          EX-DEL-3
                                          EX-DEL-4
                                          EX-DEL-5
                                          EX-DEL-6
                                          EX-DEL-7
                                          EX-DEL-8
                                          EX-DEL-9
                                          EX-DEL-10
                                          EX-DEL-11
                                          EX-DEL-12
                                          EX-DEL-13
                                          EX-DEL-14
                                          EX-DEL-15
                                          EX-DEL-16
                                          EX-DEL-17
                                          EX-DEL-18
                                          EX-DEL-19
                                          EX-DEL-20
                                          EX-DEL-21
                                          EX-DEL-22
                                          EX-DEL-23
                                          EX-DEL-24
                                          EX-DEL-25
                                          EX-DEL-26
                                          EX-DEL-27
                                          EX-DEL-28
                                          EX-DEL-29
                                          EX-DEL-30
                                          EX-DEL-31
                                          EX-DEL-32
         
           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR

           PERFORM 0280-READ-CERT      THRU 0280-EXIT
           PERFORM 0125-START-ERACCT   THRU 0125-EXIT
           PERFORM 0150-READ-ERACCT    THRU 0150-EXIT
           MOVE ERACCT-ACCT-KEY        TO WS-PREV-ACCT-KEY
           MOVE ERACCT-BIN-EFF-DT      TO WS-BIN-LOW-EFF-DT

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              DISPLAY ' BINARY RUN DATE ' DC-GREG-DATE-CYMD
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-LAST-MONTH-DT
                 DISPLAY ' THIS MONTH END DATE ' RUN-DATE
                 DISPLAY ' LAST MONTH END DATE ' WS-LAST-MONTH-DT
              ELSE
                 DISPLAY ' ERROR CREATING LAST MONTH END DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -36                    TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
                                          WS-BIN-36-MONTHS-AGO
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-36-MONTHS-AGO
                 DISPLAY ' 36 MONTHS AGO       ' WS-36-MONTHS-AGO
              ELSE
                 DISPLAY ' ERROR CREATING 36 MONTHS AGO DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -37                    TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
                                          WS-BIN-37-MONTHS-AGO
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-37-MONTHS-AGO
                 DISPLAY ' 37 MONTHS AGO       ' WS-37-MONTHS-AGO
              ELSE
                 DISPLAY ' ERROR CREATING 37 MONTHS AGO DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -12                    TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
                                          WS-BIN-12-MONTHS-AGO
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-12-MONTHS-AGO
                 DISPLAY ' 12 MONTHS AGO       ' WS-12-MONTHS-AGO
              ELSE
                 DISPLAY ' ERROR CREATING 12 MONTHS AGO DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0020-EXIT.
           EXIT.

       0050-PROCESS.
       
           IF ERACCT-ACCT-KEY NOT = WS-PREV-ACCT-KEY
              PERFORM 0060-NEW-ACCT    THRU 0060-EXIT
              MOVE ERACCT-ACCT-KEY     TO WS-PREV-ACCT-KEY
              MOVE ERACCT-BIN-EFF-DT   TO WS-BIN-LOW-EFF-DT
           END-IF
           
           MOVE ERACCT-IN-REC          TO ACCOUNT-MASTER

           PERFORM 0150-READ-ERACCT    THRU 0150-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-NEW-ACCT.

           IF (WS-BIN-LOW-EFF-DT <= WS-BIN-36-MONTHS-AGO)
              AND (AM-EXPIRE-DT = 99999999999)
              MOVE AM-GPCD             TO WS-BUS-TYPE
              IF WS-DESIRED-GROUP
                 PERFORM 0070-INIT-EXTR
                                       THRU 0070-EXIT
                 MOVE ' '              TO WS-END-OF-CERTS-SW
                 PERFORM 0100-PROCESS-CERT 
                                       THRU 0100-EXIT UNTIL
                    END-OF-CERTS-FOR-THIS-ACCT
                 PERFORM 0300-WRITE-EXTR
                                       THRU 0300-EXIT
              END-IF
           END-IF

           .
       0060-EXIT.
           EXIT.              
              
       0070-INIT-EXTR.

           MOVE ZEROS                  TO WS-ITD-CANC-RATIO
                                          WS-ITD-ISS-CNT
                                          WS-INFORCE-CNT
                                          WS-ITD-EXPIRED-CNT
                                          WS-ITD-DTH-CLAIM-CNT
                                          WS-ITD-CANCEL-CNT
                                          WS-ITD-GROSS-LF-PREM
                                          WS-ITD-REF-LF-PREM
                                          WS-ITD-GROSS-AH-PREM
                                          WS-ITD-REF-AH-PREM
                                          WS-L12-CANC-RATIO
                                          WS-L12-ISS-CNT
                                          WS-L12-GROSS-LF-PREM
                                          WS-L12-REF-AH-PREM
                                          WS-L12-GROSS-AH-PREM
                                          WS-L12-REF-LF-PREM
                                          WS-L12-EXPIRED-CNT
                                          WS-L12-DTH-CLAIM-CNT
                                          WS-L12-CANCEL-CNT

           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD

           MOVE AM-ACCOUNT             TO EX-ACCOUNT-NO
           MOVE AM-STATE               TO EX-STATE
           MOVE AM-NAME                TO EX-ACCOUNT-NAME
           MOVE AM-ADDRS               TO EX-ADDR1
           MOVE AM-CITY                TO EX-CITY
           MOVE AM-ZIP                 TO EX-ZIP
           MOVE AM-REPORT-CODE-1       TO EX-RPT-CDE1
           MOVE AM-REPORT-CODE-2       TO EX-RPT-CDE2

           IF (WS-BIN-LOW-EFF-DT > WS-BIN-37-MONTHS-AGO)
              AND (WS-BIN-LOW-EFF-DT <= WS-BIN-36-MONTHS-AGO)
              MOVE '*'                 TO EX-ACCT-FIRST-TIME
           END-IF

           MOVE WS-BIN-LOW-EFF-DT      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-ACCT-ORIG-EFF-DT
           ELSE
              MOVE 'DATE ERROR'        TO EX-ACCT-ORIG-EFF-DT
           END-IF

           MOVE '12/31/9999'           TO EX-ACCT-EXP-DT
           EVALUATE AM-STATUS
              WHEN '0'
                 MOVE 'ACTIVE'         TO EX-ACCT-STATUS
              WHEN '1'
                 MOVE 'INACTIVE'       TO EX-ACCT-STATUS
              WHEN '2'
                 MOVE 'TRANSFERRED'    TO EX-ACCT-STATUS
              WHEN '3'
                 MOVE 'CANCELLED'      TO EX-ACCT-STATUS
              WHEN '4'
                 MOVE 'FROZEN  '       TO EX-ACCT-STATUS
              WHEN '5'
                 MOVE 'SUSPENDED'      TO EX-ACCT-STATUS
021916        WHEN '6'
021916           MOVE 'DROPPED'        TO EX-ACCT-STATUS
021916        WHEN '7'
021916           MOVE 'LAPSED'         TO EX-ACCT-STATUS
021916        WHEN '8'
021916           MOVE 'RUN-OFF'        TO EX-ACCT-STATUS
021916        WHEN '9'
021916           MOVE 'PENDING'        TO EX-ACCT-STATUS
              WHEN OTHER
                 MOVE 'UNKNOWN'        TO EX-ACCT-STATUS
           END-EVALUATE

           .
       0070-EXIT.
           EXIT.

       0100-PROCESS-CERT.

           IF CR-ACCT-CONTROL < AM-CONTROL-A
              PERFORM 0280-READ-CERT   THRU 0280-EXIT
              GO TO 0100-EXIT
           ELSE
              IF CR-ACCT-CONTROL > AM-CONTROL-A
                 SET END-OF-CERTS-FOR-THIS-ACCT
                                       TO TRUE
                 GO TO 0100-EXIT
              ELSE
                 IF CR-ENTRY-STATUS = 'D' OR 'V' OR '9'
                    PERFORM 0280-READ-CERT
                                       THRU 0280-EXIT
                    GO TO 0100-EXIT
                 END-IF
              END-IF
           END-IF

      *  HANDLE RE ISSUES

           IF CR-ENTRY-STATUS = '5'
              MOVE ZEROS               TO CR-LFPRM
                                          CR-LFPRM-ALT
                                          CR-AHPRM
           END-IF

           IF CR-LF-CANCEL-EXIT-DATE NOT NUMERIC
              MOVE ZEROS               TO CR-LF-CANCEL-EXIT-DATE
           END-IF
           IF CR-AH-CANCEL-EXIT-DATE NOT NUMERIC
              MOVE ZEROS               TO CR-AH-CANCEL-EXIT-DATE
           END-IF
      *  CALCULATE EXPIRATION DATE

           MOVE CR-LF-EXPIRE-DATE      TO WS-EXPIRE-DT
           IF CR-AH-EXPIRE-DATE > WS-EXPIRE-DT
              MOVE CR-AH-EXPIRE-DATE   TO WS-EXPIRE-DT
           END-IF

      *  CALCULATE CANCEL DATE

           MOVE CR-LF-CANCEL-EXIT-DATE TO WS-CANCEL-DT
           IF CR-AH-CANCEL-EXIT-DATE > WS-CANCEL-DT
              MOVE CR-AH-CANCEL-EXIT-DATE
                                       TO WS-CANCEL-DT
           END-IF
           
      *  PROCESS INCEPTION TO DATE INFORMATION

           IF CR-ENTRY-STATUS NOT = '5'
              ADD 1                    TO WS-ITD-ISS-CNT
              IF CR-LFTYP NOT = ZEROS AND SPACES
                 COMPUTE WS-ITD-GROSS-LF-PREM = WS-ITD-GROSS-LF-PREM +
                 CR-LFPRM + CR-LFPRM-ALT
              END-IF
              IF CR-AHTYP NOT = ZEROS AND SPACES
                 COMPUTE WS-ITD-GROSS-AH-PREM = WS-ITD-GROSS-AH-PREM +
                 CR-AHPRM
              END-IF
           END-IF

           IF (WS-CANCEL-DT NOT = ZEROS)
              AND (WS-CANCEL-DT <= RUN-DATE)
              ADD 1                    TO WS-ITD-CANCEL-CNT
              IF CR-LFTYP NOT = ZEROS AND SPACES
                 COMPUTE WS-ITD-REF-LF-PREM = WS-ITD-REF-LF-PREM +
                 CR-LFRFND
              END-IF
              IF CR-AHTYP NOT = ZEROS AND SPACES
                 COMPUTE WS-ITD-REF-AH-PREM = WS-ITD-REF-AH-PREM +
                 CR-AHRFND
              END-IF
           END-IF

           IF (CR-LF-CLAIM-EXIT-DATE NOT = ZEROS)
              AND (CR-LF-CLAIM-EXIT-DATE <= RUN-DATE)
              ADD 1                    TO WS-ITD-DTH-CLAIM-CNT
           ELSE
              IF (WS-CANCEL-DT NOT = ZEROS)
                 AND (WS-CANCEL-DT <= RUN-DATE)
                 CONTINUE
              ELSE
                 IF CR-ENTRY-STATUS NOT = '5'
                    IF WS-EXPIRE-DT <= RUN-DATE
                       ADD 1           TO WS-ITD-EXPIRED-CNT
                    ELSE
                       ADD 1           TO WS-INFORCE-CNT
                    END-IF
                 END-IF
              END-IF
           END-IF


      *  PROCESS LAST 12 MONTHS INFORMATION

      *  CALCULATE CANCEL DATES I NEED TO CALC ONE FOR LIFE 
      *  AND ONE FOR AH BECAUSE OF THE L12 MONTHS THING

           MOVE CR-LF-CANCEL-EXIT-DATE TO WS-LF-CANCEL-DT
           MOVE CR-AH-CANCEL-EXIT-DATE TO WS-AH-CANCEL-DT

           IF (CR-ENTRY-STATUS NOT = '5')
              AND (CR-ENTRY-DATE > WS-12-MONTHS-AGO)
              ADD 1                    TO WS-L12-ISS-CNT
              IF CR-LFTYP NOT = ZEROS AND SPACES
                 COMPUTE WS-L12-GROSS-LF-PREM = WS-L12-GROSS-LF-PREM +
                 CR-LFPRM + CR-LFPRM-ALT
              END-IF
              IF CR-AHTYP NOT = ZEROS AND SPACES
                 COMPUTE WS-L12-GROSS-AH-PREM = WS-L12-GROSS-AH-PREM +
                 CR-AHPRM
              END-IF
           END-IF

           IF ((WS-LF-CANCEL-DT NOT = ZEROS)
              AND (WS-LF-CANCEL-DT <= RUN-DATE)
              AND (WS-LF-CANCEL-DT > WS-12-MONTHS-AGO))
                            OR
              ((WS-AH-CANCEL-DT NOT = ZEROS)
              AND (WS-AH-CANCEL-DT <= RUN-DATE)
              AND (WS-AH-CANCEL-DT > WS-12-MONTHS-AGO))
              ADD 1                 TO WS-L12-CANCEL-CNT
           END-IF

           IF (WS-LF-CANCEL-DT NOT = ZEROS)
              AND (WS-LF-CANCEL-DT <= RUN-DATE)
              AND (WS-LF-CANCEL-DT > WS-12-MONTHS-AGO)
              IF CR-LFTYP NOT = ZEROS AND SPACES
                 COMPUTE WS-L12-REF-LF-PREM = WS-L12-REF-LF-PREM +
                 CR-LFRFND
              END-IF
           END-IF

           IF (WS-AH-CANCEL-DT NOT = ZEROS)
              AND (WS-AH-CANCEL-DT <= RUN-DATE)
              AND (WS-AH-CANCEL-DT > WS-12-MONTHS-AGO)
              IF CR-AHTYP NOT = ZEROS AND SPACES
                 COMPUTE WS-L12-REF-AH-PREM = WS-L12-REF-AH-PREM +
                 CR-AHRFND
              END-IF
           END-IF

           IF (CR-LF-CLAIM-EXIT-DATE NOT = ZEROS)
              AND (CR-LF-CLAIM-EXIT-DATE <= RUN-DATE)
              AND (CR-LF-CLAIM-EXIT-DATE > WS-12-MONTHS-AGO)
              ADD 1                    TO WS-L12-DTH-CLAIM-CNT
           ELSE
              IF (WS-CANCEL-DT NOT = ZEROS)
                 AND (WS-CANCEL-DT <= RUN-DATE)
                 AND (WS-CANCEL-DT > WS-12-MONTHS-AGO)
                 CONTINUE
              ELSE
                 IF CR-ENTRY-STATUS NOT = '5'
                    IF (WS-EXPIRE-DT <= RUN-DATE)
                       AND (WS-EXPIRE-DT > WS-12-MONTHS-AGO)
                       ADD 1           TO WS-L12-EXPIRED-CNT
                    END-IF
                 END-IF
              END-IF
           END-IF

      *  this is for info only right now
           IF (WS-CANCEL-DT NOT = ZEROS)
              AND (CR-LF-CLAIM-EXIT-DATE NOT = ZEROS)
              IF WS-CANCEL-DT < CR-LF-CLAIM-EXIT-DATE
                 DISPLAY ' CANCELLED BEFORE DEATH ' CR-FULL-CONTROL
              END-IF
           END-IF

           PERFORM 0280-READ-CERT   THRU 0280-EXIT

           .
       0100-EXIT.
           EXIT.
           
       0125-START-ERACCT.

           MOVE LOW-VALUES             TO ERACCT-KEY
           MOVE DTE-CLASIC-COMPANY-CD  TO ERACCT-COMP-CD

           START ERACCT KEY >= ERACCT-KEY

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - START ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0125-EXIT.
           EXIT.

       0150-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (ERACCT-COMP-CD > DTE-CLASIC-COMPANY-CD)
              MOVE HIGH-VALUES         TO ERACCT-KEY
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACCT ERROR - READ ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0150-EXIT.
           EXIT.

       0280-READ-CERT.

           READ CERT-FILE-IN AT END
              SET END-OF-CERT          TO TRUE
           END-READ


           IF NOT END-OF-CERT
              ADD 1                    TO CRT-RECS-IN
           END-IF

           .
       0280-EXIT.
           EXIT.

       0300-WRITE-EXTR.

      *  WRITE ITD RECORD

           IF WS-ITD-ISS-CNT > ZEROS
              COMPUTE WS-ITD-CANC-RATIO ROUNDED = (WS-ITD-CANCEL-CNT
                 / WS-ITD-ISS-CNT) * 100
           ELSE
              MOVE ZEROS               TO WS-ITD-CANC-RATIO
           END-IF

           MOVE WS-ITD-CANC-RATIO      TO EX-ITD-CANC-RATIO
           MOVE WS-ITD-ISS-CNT         TO EX-ITD-ISS-CNT
           MOVE WS-ITD-CANCEL-CNT      TO EX-ITD-CANCEL-CNT
           MOVE WS-ITD-DTH-CLAIM-CNT   TO EX-ITD-DTH-CLAIM-CNT
           MOVE WS-ITD-EXPIRED-CNT     TO EX-ITD-EXPIRED-CNT
           MOVE WS-INFORCE-CNT         TO EX-INFORCE-CNT
           MOVE WS-ITD-GROSS-LF-PREM   TO EX-ITD-ISS-LF-PREM
           MOVE WS-ITD-REF-LF-PREM     TO EX-ITD-REF-LF-PREM
           MOVE WS-ITD-GROSS-AH-PREM   TO EX-ITD-ISS-AH-PREM
           MOVE WS-ITD-REF-AH-PREM     TO EX-ITD-REF-AH-PREM

      *    WRITE EXTR-FILE-OUT-REC     FROM EXTR-DETAIL-RECORD
      *    ADD 1                       TO EXT-RECS-OUT

      *  WRITE L12 RECORD

           IF WS-L12-ISS-CNT > ZEROS
              COMPUTE WS-L12-CANC-RATIO ROUNDED = (WS-L12-CANCEL-CNT
                 / WS-L12-ISS-CNT) * 100
           ELSE
              MOVE ZEROS               TO WS-L12-CANC-RATIO
           END-IF

           MOVE WS-L12-CANC-RATIO      TO EX-L12-CANC-RATIO
           MOVE WS-L12-ISS-CNT         TO EX-L12-ISS-CNT
           MOVE WS-L12-CANCEL-CNT      TO EX-L12-CANCEL-CNT
           MOVE WS-L12-DTH-CLAIM-CNT   TO EX-L12-DTH-CLAIM-CNT
           MOVE WS-L12-EXPIRED-CNT     TO EX-L12-EXPIRED-CNT
           MOVE WS-L12-GROSS-LF-PREM   TO EX-L12-ISS-LF-PREM
           MOVE WS-L12-REF-LF-PREM     TO EX-L12-REF-LF-PREM
           MOVE WS-L12-GROSS-AH-PREM   TO EX-L12-ISS-AH-PREM
           MOVE WS-L12-REF-AH-PREM     TO EX-L12-REF-AH-PREM

           WRITE EXTR-FILE-OUT-REC     FROM EXTR-DETAIL-RECORD
           ADD 1                       TO EXT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE CERT-FILE-IN EXTR-FILE-OUT
                 ERACCT

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - CLOSE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0500-EXIT.
           EXIT.


       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.

