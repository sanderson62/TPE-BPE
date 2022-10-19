00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                ELCANC.                               
00008 *                                                                 
00009 *AUTHOR.     PABLO.
00010 *            OMAHA, NE.
00011                                                                   
00012 *DATE-COMPILED.                                                   
00013                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.    *****************************************************
00025 *            *                                                   *
00026 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00027 *            *    OPTION SPECIFIED, CREATE A PENDING BATCH       *
00028 *            *    AND CANCEL RECORD.                             *
00029 *            *                                                   *
00030 *            *    ORIGINAL BENEFIT = CP-ORIGINAL-BENEFIT         *
00031 *            *    ORIGINAL TERM = CP-ORIGINAL-TERM               *
00032 *            *    REMAINING-TERM  = CP-REMAINING-TERM            *
00033 *            *    A.P.R (FOR NET PAY) = CP-LOAN-APR              *
00034 *            *    BENEFIT TYPE = CP-BENEFIT-TYPE                 *
00035 *            *    METHOD = CP-EARNING-METHOD                     *
00036 *            *    IF COMPANY SPECIAL METHOD  - PUT COMPANY       *
00037 *            *     I.D. IN CP-COMPANY-ID.                        *
00038 *            *                                                   *
00039 *            *****************************************************
122711******************************************************************
122711*                   C H A N G E   L O G
122711*
122711* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122711*-----------------------------------------------------------------
122711*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122711* EFFECTIVE    NUMBER
122711*-----------------------------------------------------------------
122711* 122711    2011022800001  AJRA  INCREMENT CERT SUFFIX
010412* 010412    2011022800001  AJRA  NAPERSOFT
091312* 091312    2012091100004  AJRA  FIX 1ST PMT DT PROCESS
091812* 091812  IR2012091400003  AJRA  POPULATE CARR,GRP,ST ON CANCEL
092512* 092512  IR2012092500004  AJRA  HANDLE DUPREC ON ADD
101512* 101512    2011022800001  AJRA  INCREASE DELAY ON BACKGROUND EDIT
120512* 120512    2012101700001  AJRA  FIX NON NUMERIC BATCH NO
121012* 121012    2012121000001  AJRA  FIX NEW BATCH NO
122712* 122712  CR2012101700002  AJRA  PUT P IN EC FIELD IF NO COMM PCTS
122712*                                ON CERT BUT COMM PCTS ON ACCT
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
100917* 100917  CR2017092000002  PEMA  Copy Elcrtt to new cert issue.
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTEREST
101918* 101918  CR2018050200001  PEMA  Add check for cancel rec
070622* 070622  CR2020061200002  TANA  Add cancel reason logic
071822* 071822  IR2022071400001  TANA  Change cancel reas to alpha
122711******************************************************************
00040  ENVIRONMENT DIVISION.                                            
00041                                                                   
00042  DATA DIVISION.                                                   
00043      EJECT                                                        
00044  WORKING-STORAGE SECTION.                                         
00045  77  FILLER   PIC X(32) VALUE '********************************'. 
00046  77  FILLER   PIC X(32) VALUE '**  ELCANC  WORKING STORAGE   **'. 
00047  77  FILLER   PIC X(32) VALUE '********* VMOD 2.001 ***********'. 
       77  WS-ELCERT-SW                PIC X  VALUE ' '.
           88  CERT-FOUND                  VALUE 'Y'.
       77  WS-ELCNTL-SW                PIC X  VALUE ' '.
           88  CNTL-FOUND                  VALUE 'Y'.
       77  WS-ERACCT-SW                PIC X  VALUE ' '.
           88  ACCT-FOUND                  VALUE 'Y'.
       77  WS-ERPNDB-SW                PIC X  VALUE ' '.
           88  PNDB-FOUND                  VALUE 'Y'.
122712 77  SUB1                        PIC S999 COMP-3 VALUE +0.
121012 77  WS-BATCH-NO-N               PIC 9(6).           
120512 77  WS-BATCH-NO                 PIC X(6).
       77  WS-NEXT-SEQ-NO              PIC 9(4) BINARY VALUE ZEROS.
       77  WS-RESPONSE         PIC S9(8)   COMP.                    
           88  RESP-NORMAL              VALUE +00.
           88  RESP-ERROR               VALUE +01.
           88  RESP-NOTFND              VALUE +13.
           88  RESP-DUPREC              VALUE +14.
           88  RESP-DUPKEY              VALUE +15.


00048                                                                   
       01  ws-hold-erpndb              pic x(585).
       01  WS-ELCERT-KEY               PIC X(33).
100917 01  ws-elcrtt-key               pic x(34).
122711 01  WS-WORK.
122711     05  WS-WRK-SFX-COMP         PIC S9(4) COMP.
122711     05  WS-WRK-SFX-R REDEFINES WS-WRK-SFX-COMP.
122711         10  FILLER              PIC X.
122711         10  WS-WRK-SFX          PIC X.
122711
       01  WS-ERPNDB-KEY.
           05  PNDB-COMPANY-CD         PIC X.
           05  PNDB-BATCH-NO           PIC X(6).
           05  PNDB-BATCH-SEQ          PIC 9(4) BINARY.
           05  PNDB-BATCH-CHG-SEQ      PIC 9(4) BINARY.

       01  WS-ERPNDB2-KEY.
           05  F                       PIC X(32).
           05  PNDB2-CERT-SUFFIX       PIC X.
           05  PNDB2-SEQ-NO            PIC 9(4) BINARY.
           05  PNDB2-REC-TYPE          PIC X.

       01  WS-ERACCT-KEY.
           05  ACCT-COMPANY-CD         PIC X.
           05  ACCT-CARRIER            PIC X.
           05  ACCT-GROUP              PIC X(6).
           05  ACCT-STATE              PIC XX.
           05  ACCT-ACCOUNT            PIC X(10).
           05  ACCT-EXP-DT             PIC XX.
           05  F                       PIC X(4).

       01  WS-ELCNTL-KEY.
           05  CNTL-COMP-ID            PIC XXX.
           05  CNTL-REC-TYPE           PIC X.
           05  CNTL-ACCESS             PIC XXXX.
           05  CNTL-SEQ-NO             PIC 9(4) BINARY.

       01  BATCH-TO-PROCESS.                                            
           05  EDIT-COMPANY-CD         PIC X       VALUE LOW-VALUES.    
           05  EDIT-BATCH              PIC X(6)    VALUE SPACES.        
           05  EDIT-COMPANY-ID         PIC XXX     VALUE SPACES.        
           05  EDIT-RESTART-BATCH      PIC X(6)    VALUE SPACES.        

       01  CANCEL-GEN-PASS-AREA.
           05  CG-OPTION-CODE          PIC X.
               88  CG-VALID-OPTION       VALUE '1' '2' '3' '4'.
               88  CG-FLAT-CANCEL        VALUE '1'.
               88  CG-CANCEL             VALUE '2'.
               88  CG-CANCEL-REISSUE     VALUE '3'.
               88  CG-FLAT-CANCEL-SAME-BATCH VALUE '4'.
           05  CG-ERROR-CODE           PIC 99.
               88  CG-SUCCESS            VALUE 00.
               88  CG-DATE-ERROR         VALUE 01.
               88  CG-CERT-NOT-FOUND     VALUE 02.
               88  CG-AMOUNT-ERROR       VALUE 04.
               88  CG-OPTION-ERROR       VALUE 05.
               88  CG-PREV-CAN           VALUE 06.
               88  CG-INVALID-DATA       VALUE 07.
               88  CG-NO-ACCT-MSTR       VALUE 08.
               88  CG-SFX-A-EXIST        VALUE 09.
               88  CG-MISC-ERROR         VALUE 99.
           05  CG-COMPANY-ID           PIC XXX.
           05  CG-PROC-ID              PIC XXXX.
           05  CG-CURRENT-DT           PIC XX.
           05  CG-MONTH-END-DT         PIC XX.
           05  CG-CERT-KEY.
               10  CG-CERT-COMPANY-CD  PIC X.
               10  CG-CERT-CARRIER     PIC X.
               10  CG-CERT-GROUP       PIC X(6).
               10  CG-CERT-STATE       PIC XX.
               10  CG-CERT-ACCOUNT     PIC X(10).
               10  CG-CERT-EFF-DT      PIC XX.
               10  CG-CERT-CERT-NO     PIC X(11).
           05  CG-LF-CAN-DATA.
               10  CG-LF-CAN-DT        PIC XX.
               10  CG-LF-CAN-AMT       PIC S9(7)V99 COMP-3.
           05  CG-AH-CAN-DATA.
               10  CG-AH-CAN-DT        PIC XX.
               10  CG-AH-CAN-AMT       PIC S9(7)V99 COMP-3.
           05  CG-CERT-PROFILE-DATA.
               10  CG-INS-LNAME        PIC X(15).
               10  CG-INS-FNAME        PIC X(10).
               10  CG-INS-MID-INIT     PIC X.
               10  CG-INS-AGE          PIC 99.
               10  CG-JNT-LNAME        PIC X(15).
               10  CG-JNT-FNAME        PIC X(10).
               10  CG-JNT-MID-INIT     PIC X.
               10  CG-JNT-AGE          PIC 99.
           05  CG-LF-ISS-DATA.
               10  CG-LF-BENCD         PIC XX.
               10  CG-LF-PREM-AMT      PIC S9(7)V99 COMP-3.
072312         10  CG-LF-ALT-PREM-AMT  PIC S9(7)V99 COMP-3.
           05  CG-AH-ISS-DATA.
               10  CG-AH-BENCD         PIC XX.
               10  CG-AH-PREM-AMT      PIC S9(7)V99 COMP-3.
           05  CG-BATCH-NO             PIC X(6).
010412     05  CG-BATCH-SEQ-NO         PIC 9(4) COMP.
072312     05  CG-DCC-REASON-CD        PIC X.
122712     05  CG-COMM-PCT-ZERO        PIC X.
100917     05  cg-vin                  pic x(17).
101918     05  cg-from-where           pic x(6).
100917     05  FILLER                  PIC X(281).


                                 COPY ELCCNTL.
                                 COPY ELCCERT.
100917                           COPY ELCCRTT.
                                 COPY ERCACCT.
                                 COPY ERCPNDB.
                                 COPY ERCPNDM.
                                 COPY ERCMAIL.
00053                            COPY ELCDATE.                          
00054                                                                   
00055  LINKAGE SECTION.                                                 
00056  01  DFHCOMMAREA                 PIC X(450).                      
00057                                                                   
00059  PROCEDURE DIVISION.                                              

           MOVE DFHCOMMAREA            TO CANCEL-GEN-PASS-AREA

           MOVE ZEROS                  TO CG-ERROR-CODE
           MOVE ' '                    TO WS-ELCERT-SW
                                          WS-ERACCT-SW
                                          WS-ERPNDB-SW
                                          WS-ELCNTL-SW

           PERFORM 0010-CHECK-FOR-ERRORS THRU 0010-EXIT
           IF NOT CG-SUCCESS
              GO TO 5000-RETURN
           END-IF

           PERFORM 0050-BUILD-CANCEL   THRU 0050-EXIT

           MOVE CM-COMPANY-CD          TO EDIT-COMPANY-CD
           MOVE WS-BATCH-NO            TO EDIT-BATCH
           MOVE CG-COMPANY-ID          TO EDIT-COMPANY-ID
           MOVE SPACES                 TO EDIT-RESTART-BATCH

           IF CG-CANCEL-REISSUE
              PERFORM 0100-BUILD-ISSUE THRU 0100-EXIT
           END-IF

           MOVE WS-BATCH-NO            TO CG-BATCH-NO

           IF CG-FLAT-CANCEL-SAME-BATCH
              PERFORM 0120-UPDATE-BATCH-HDR
                                       THRU 0120-EXIT
           ELSE

              PERFORM 0055-BUILD-BATCH-HDR
                                       THRU 0055-EXIT
           END-IF
010412
010412     MOVE PB-B-HIGHEST-SEQ-NO  TO CG-BATCH-SEQ-NO
           
           EXEC CICS START                                        
                TRANSID       ('EXEB')
                FROM          (BATCH-TO-PROCESS)
           END-EXEC

101918     if cg-from-where = 'EL1273'
101918        EXEC CICS DELAY
101918           INTERVAL    (01)
101918        END-EXEC
101918     else
101918        EXEC CICS DELAY
101918           INTERVAL    (03)
101918        END-EXEC
101918     end-if

           GO TO 5000-RETURN

           .
       0010-CHECK-FOR-ERRORS.

           IF NOT CG-VALID-OPTION
              SET CG-OPTION-ERROR      TO TRUE
              GO TO 0010-EXIT
           END-IF

           MOVE CG-CERT-KEY            TO WS-ELCERT-KEY
           PERFORM 0020-READ-ELCERT    THRU 0020-EXIT
           IF NOT CERT-FOUND
              SET CG-CERT-NOT-FOUND    TO TRUE
              GO TO 0010-EXIT
           END-IF

           PERFORM 0040-READ-ERACCT    THRU 0040-EXIT
           IF NOT ACCT-FOUND
              SET CG-NO-ACCT-MSTR      TO TRUE
              GO TO 0010-EXIT
           END-IF

           MOVE CM-CONTROL-PRIMARY     TO WS-ERPNDB2-KEY
           MOVE 0                      TO PNDB2-SEQ-NO
           MOVE '2'                    TO PNDB2-REC-TYPE

           PERFORM 0030-READ-ERPNDB2   THRU 0030-EXIT
           IF PNDB-FOUND
              SET CG-PREV-CAN          TO TRUE
              GO TO 0010-EXIT
           END-IF

           IF ((CG-LF-CAN-DT NOT = LOW-VALUES)
              AND (CM-LF-CANCEL-DT NOT = LOW-VALUES))
                       OR
              ((CG-AH-CAN-DT NOT = LOW-VALUES)
              AND (CM-AH-CANCEL-DT NOT = LOW-VALUES))
              SET CG-PREV-CAN          TO TRUE
              GO TO 0010-EXIT
           END-IF
                       
010412     IF (CM-LF-CANCEL-DT NOT = LOW-VALUES)
010412       AND (CM-AH-CANCEL-DT NOT = LOW-VALUES)
010412        SET CG-PREV-CAN          TO TRUE
010412        GO TO 0010-EXIT
010412     END-IF
010412
           IF ((CG-LF-CAN-DT NOT = LOW-VALUES)
              AND (CG-LF-CAN-AMT = ZEROS))
                       OR
              ((CG-AH-CAN-DT NOT = LOW-VALUES)
              AND (CG-AH-CAN-AMT = ZEROS))
              SET CG-INVALID-DATA      TO TRUE
              GO TO 0010-EXIT
           END-IF

           IF CG-CANCEL-REISSUE
              MOVE ' '                 TO WS-ERPNDB-SW
              MOVE CM-CONTROL-PRIMARY  TO WS-ERPNDB2-KEY
122711        IF CM-CERT-SFX GREATER THAN SPACES
122711           MOVE CM-CERT-SFX      TO WS-WRK-SFX
122711           ADD +1                TO WS-WRK-SFX-COMP
122711           MOVE WS-WRK-SFX       TO PNDB2-CERT-SUFFIX
122711        ELSE
122711           MOVE 'A'              TO PNDB2-CERT-SUFFIX
122711        END-IF
              MOVE 0                   TO PNDB2-SEQ-NO
              MOVE '1'                 TO PNDB2-REC-TYPE
              PERFORM 0030-READ-ERPNDB2
                                       THRU 0030-EXIT
              IF PNDB-FOUND
                 SET CG-SFX-A-EXIST    TO TRUE
                 GO TO 0010-EXIT
              END-IF
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-READ-ELCERT.

           EXEC CICS READ
              DATASET ('ELCERT')
              RIDFLD  (WS-ELCERT-KEY)
              INTO    (CERTIFICATE-MASTER)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              SET CERT-FOUND TO TRUE
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-READ-ERPNDB2.

           EXEC CICS READ
              DATASET ('ERPNDB2')
              RIDFLD  (WS-ERPNDB2-KEY)
              INTO    (PENDING-BUSINESS)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              SET PNDB-FOUND TO TRUE
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-READ-ERACCT.

           MOVE LOW-VALUES             TO WS-ERACCT-KEY
           MOVE CM-CONTROL-PRIMARY (1:22)
                                       TO WS-ERACCT-KEY (1:22)

           EXEC CICS STARTBR
              DATASET     ('ERACCT')
              RIDFLD      (WS-ERACCT-KEY)
              GTEQ
              RESP        (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              SET CG-NO-ACCT-MSTR      TO TRUE
              DISPLAY ' NO ACCT MSTR START ' WS-RESPONSE
              GO TO 0040-EXIT
           END-IF

           .
       0040-CONTINUE.

           EXEC CICS READNEXT
              DATASET ('ERACCT')
              RIDFLD  (WS-ERACCT-KEY)
              INTO    (ACCOUNT-MASTER)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF (RESP-NORMAL)
              AND (CM-CONTROL-PRIMARY (1:20) =
                 AM-CONTROL-PRIMARY (1:20))
              IF CM-CERT-EFF-DT = AM-EXPIRATION-DT
                 DISPLAY ' READ ERACCT AGAIN ' CM-CERT-NO
                 GO TO 0040-CONTINUE
              END-IF
              IF (CM-CERT-EFF-DT < AM-EXPIRATION-DT)
                 AND (CM-CERT-EFF-DT >= AM-EFFECTIVE-DT)
                 SET ACCT-FOUND TO TRUE
              END-IF
           END-IF

           EXEC CICS ENDBR
              DATASET ('ERACCT')
              RESP    (WS-RESPONSE)
           END-EXEC

           .
       0040-EXIT.
           EXIT.

       0050-BUILD-CANCEL.

           if cg-batch-no not = spaces
              move cg-batch-no         to ws-batch-no
           else
              PERFORM 0060-READ-ELCNTL THRU 0060-EXIT

              IF NOT CNTL-FOUND
                 SET CG-MISC-ERROR TO TRUE
                 GO TO 0050-EXIT
              END-IF
           end-if

           PERFORM 0070-INIT-PNDB-CAN  THRU 0070-EXIT

           MOVE CM-INSURED-LAST-NAME   TO PB-C-LAST-NAME

           IF CG-FLAT-CANCEL OR CG-FLAT-CANCEL-SAME-BATCH
              IF CM-LF-BENEFIT-CD NOT = ZEROS AND SPACES
010412          AND (CM-LF-CANCEL-DT = LOW-VALUES OR SPACES)
                 MOVE CM-CERT-EFF-DT   TO CG-LF-CAN-DT
                 COMPUTE CG-LF-CAN-AMT = CM-LF-PREMIUM-AMT +
                    CM-LF-ALT-PREMIUM-AMT
              END-IF
              IF CM-AH-BENEFIT-CD NOT = ZEROS AND SPACES
010412          AND (CM-AH-CANCEL-DT = LOW-VALUES OR SPACES)
                 MOVE CM-CERT-EFF-DT   TO CG-AH-CAN-DT
                 MOVE CM-AH-PREMIUM-AMT TO CG-AH-CAN-AMT
              END-IF
           END-IF

           MOVE CG-LF-CAN-DT           TO PB-C-LF-CANCEL-DT
           MOVE CG-AH-CAN-DT           TO PB-C-AH-CANCEL-DT
           MOVE CG-LF-CAN-AMT          TO PB-C-LF-CANCEL-AMT
           MOVE CG-AH-CAN-AMT          TO PB-C-AH-CANCEL-AMT

071822*    evaluate cg-option-code
071822*       when '3'
071822*          move '3'              to pb-c-cancel-reason
071822*       when '1'
071822*          move '2'              to pb-c-cancel-reason
071822*       when '2'
071822*          move '5'              to pb-c-cancel-reason
071822*       when '4'
071822*          move '4'              to pb-c-cancel-reason
071822*    end-evaluate
071822     IF CG-OPTION-CODE = '4'
071822        MOVE 'F'              TO PB-C-CANCEL-REASON
071822     END-IF.

           
020816*    IF CG-COMPANY-ID = 'DCC' or 'VPP'
070622     IF CG-COMPANY-ID = 'DCC' or 'VPP' OR 'CID'
070622       AND CG-DCC-REASON-CD  > SPACES
               MOVE CG-DCC-REASON-CD   TO PB-C-CANCEL-REASON
070622     ELSE
070622     IF AM-GPCD > 1
070622       AND AM-GPCD < 6
070622        MOVE 'Y'              TO PB-C-CANCEL-REASON
070622     END-IF.

070622     IF CG-FROM-WHERE = 'EL1273'
070622        EVALUATE CG-OPTION-CODE
070622           WHEN '3'
070622              MOVE 'F'              TO PB-C-CANCEL-REASON
070622           WHEN '1'
070622              MOVE 'X'              TO PB-C-CANCEL-REASON
070622        END-EVALUATE
070622     END-IF

           MOVE CF-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1
           MOVE CF-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1

      *    MOVE 'A'                    TO JP-RECORD-TYPE.               
      *    MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
      *    MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
      *    MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   

           IF CG-FLAT-CANCEL-SAME-BATCH
              MOVE PENDING-BUSINESS    TO WS-HOLD-ERPNDB
              PERFORM 0110-GET-NEXT-SEQ THRU 0110-EXIT
              IF RESP-NORMAL
                 MOVE WS-HOLD-ERPNDB   TO PENDING-BUSINESS
                 MOVE WS-NEXT-SEQ-NO   TO PB-BATCH-SEQ-NO
                                          PB-ORIGINAL-SEQ-NO
                                          PB-CSR-BATCH-SEQ-NO
              ELSE
                 SET CG-MISC-ERROR TO TRUE
                 GO TO 0050-EXIT
              END-IF
           END-IF

           SET RESP-DUPKEY             TO TRUE

           PERFORM UNTIL NOT RESP-DUPKEY
              EXEC CICS WRITE
                  DATASET ('ERPNDB')
                  FROM    (PENDING-BUSINESS)
                  RIDFLD  (PB-CONTROL-PRIMARY)
                  RESP    (WS-RESPONSE)
              END-EXEC
092512        IF RESP-DUPREC
092512            SET RESP-DUPKEY TO TRUE
092512        END-IF
              IF RESP-DUPKEY
                 ADD +1                TO PB-BATCH-SEQ-NO
                 MOVE PB-BATCH-SEQ-NO  TO WS-NEXT-SEQ-NO
                                          PB-ORIGINAL-SEQ-NO
                                          PB-CSR-BATCH-SEQ-NO
              END-IF
           END-PERFORM

           IF NOT RESP-NORMAL
              SET CG-MISC-ERROR TO TRUE
              DISPLAY ' ERPNDB CAN WRITE ERROR ' WS-RESPONSE
              GO TO 0050-EXIT
           END-IF

           .
       0050-EXIT.
           EXIT.

       0055-BUILD-BATCH-HDR.

           PERFORM 0090-INIT-PNDB-BATCH THRU 0090-EXIT

           MOVE CG-CERT-CARRIER        TO PB-CARRIER
                                          PB-SV-CARRIER
           MOVE CG-CERT-GROUP          TO PB-GROUPING
                                          PB-SV-GROUPING
           MOVE CG-CERT-STATE          TO PB-STATE
                                          PB-SV-STATE
           MOVE CG-CERT-ACCOUNT        TO PB-ACCOUNT
           MOVE '9'                    TO PB-RECORD-TYPE
           MOVE +1                     TO PB-B-CANCEL-CNT-REMITTED
           MOVE CG-LF-CAN-AMT          TO PB-B-LF-CAN-PRM-REMITTED
           MOVE CG-AH-CAN-AMT          TO PB-B-AH-CAN-PRM-REMITTED
           MOVE CG-MONTH-END-DT        TO PB-CREDIT-SELECT-DT
                                                                        
           MOVE +1                     TO PB-B-HIGHEST-SEQ-NO
                                                                        
           MOVE CG-PROC-ID             TO PB-LAST-MAINT-BY              
                                          PB-INPUT-BY
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS
           MOVE CG-CURRENT-DT          TO PB-LAST-MAINT-DT
                                          PB-INPUT-DT
                                          PB-B-RECEIVED-DT
                                                                        
           MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH

           MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD
           MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.           
           MOVE AM-CSR-CODE            TO PB-CSR-ID.                    
           MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.          
           MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO

           MOVE AM-NAME                TO  PB-ACCOUNT-NAME

           IF CG-CANCEL-REISSUE
              MOVE +1                  TO PB-B-ISSUE-CNT-REMITTED
              MOVE +2                  TO PB-B-HIGHEST-SEQ-NO
              if cg-lf-bencd not = '  ' and '00'
                 if cg-lf-prem-amt not = 9999999.99
                    move cg-lf-prem-amt to pb-b-lf-iss-prm-remitted
072312              ADD CG-LF-ALT-PREM-AMT TO PB-B-LF-ISS-PRM-REMITTED
                 end-if
              end-if
              if cg-ah-bencd not = '  ' and '00'
                 if cg-ah-prem-amt not = 9999999.99
                    move cg-ah-prem-amt to pb-b-ah-iss-prm-remitted
                 end-if
              end-if
           END-IF

           EXEC CICS WRITE
               DATASET('ERPNDB')
               FROM   (PENDING-BUSINESS)
               RIDFLD (PB-CONTROL-PRIMARY)
010412         RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              SET CG-MISC-ERROR TO TRUE
              DISPLAY ' ERPNDB BTCH WRITE ERROR ' WS-RESPONSE
           END-IF

           .
       0055-EXIT.
           EXIT.

       0060-READ-ELCNTL.

           MOVE CG-COMPANY-ID          TO WS-ELCNTL-KEY
           MOVE '1'                    TO CNTL-REC-TYPE
           MOVE ZEROS                  TO CNTL-SEQ-NO

           EXEC CICS READ
              UPDATE
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              INTO    (CONTROL-FILE)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 0060-EXIT
           END-IF

           IF CF-LAST-BATCH-NO NOT NUMERIC
              MOVE +0                  TO CF-LAST-BATCH-NO
           END-IF
           ADD +1                      TO CF-LAST-BATCH-NO
121012     MOVE CF-LAST-BATCH-NO       TO WS-BATCH-NO-N
121012     MOVE WS-BATCH-NO-N          TO WS-BATCH-NO
           EXEC CICS REWRITE
              DATASET   ('ELCNTL')
              FROM      (CONTROL-FILE)
              RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 0060-EXIT
           END-IF
           SET CNTL-FOUND              TO TRUE

           .
       0060-EXIT.
           EXIT.

       0070-INIT-PNDB-CAN.

           MOVE 'PB'                   TO PENDING-BUSINESS
           INITIALIZE PB-CANCEL-RECORD
           
           MOVE CM-CONTROL-PRIMARY     TO PB-CONTROL-BY-ACCOUNT
091812     MOVE PB-CARRIER             TO PB-SV-CARRIER
091812     MOVE PB-GROUPING            TO PB-SV-GROUPING
091812     MOVE PB-STATE               TO PB-SV-STATE
           MOVE +0                     TO PB-ALT-CHG-SEQ-NO
                                          PB-BATCH-CHG-SEQ-NO
                                          PB-CSR-BATCH-CHG-SEQ-NO
           MOVE '2'                    TO PB-RECORD-TYPE
           MOVE CM-COMPANY-CD          TO PB-COMPANY-CD
                                          PB-CSR-COMPANY-CD
           MOVE WS-BATCH-NO            TO PB-ENTRY-BATCH
                                          PB-CSR-ENTRY-BATCH
           MOVE +1                     TO PB-BATCH-SEQ-NO
                                          PB-CSR-BATCH-SEQ-NO
           MOVE AM-CSR-CODE            TO PB-CSR-ID

           MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS
           MOVE CG-PROC-ID             TO PB-LAST-MAINT-BY
                                          PB-INPUT-BY
           MOVE CG-CURRENT-DT          TO PB-LAST-MAINT-DT
                                          PB-INPUT-DT
           MOVE CG-COMPANY-ID          TO PB-COMPANY-ID
           MOVE +0                     TO PB-NO-OF-ERRORS
           MOVE LOW-VALUES             TO PB-COMMON-ERRORS

           MOVE ZEROS                  TO PB-C-LF-REF-CALC              
                                          PB-C-AH-REF-CALC              
                                          PB-C-LF-RFND-CLP
                                          PB-C-AH-RFND-CLP
                                          PB-CI-INSURED-AGE             
                                          PB-CI-LF-TERM                 
                                          PB-CI-AH-TERM                 
                                          PB-CI-LF-BENEFIT-CD           
                                          PB-CI-LF-BENEFIT-AMT          
                                          PB-CI-LF-ALT-BENEFIT-AMT      
                                          PB-CI-LF-PREMIUM-AMT          
                                          PB-CI-LF-ALT-PREMIUM-AMT      
                                          PB-CI-AH-BENEFIT-CD           
                                          PB-CI-AH-BENEFIT-AMT          
                                          PB-CI-AH-PREMIUM-AMT          
                                          PB-CI-PAY-FREQUENCY           
                                          PB-CI-LOAN-APR                
                                          PB-CI-LOAN-TERM               
                                          PB-CI-LIFE-COMMISSION         
                                          PB-CI-AH-COMMISSION           
                                          PB-CI-CURR-SEQ                
                                          PB-CI-AH-CANCEL-AMT           
                                          PB-CI-LF-CANCEL-AMT           
                                          PB-CI-RATE-DEV-PCT-LF         
                                          PB-CI-RATE-DEV-PCT-AH         
                                          PB-CI-EXTENTION-DAYS          
                                          PB-CI-TERM-IN-DAYS            
                                          PB-CI-LIVES                   
                                          PB-CI-LF-CRIT-PER             
                                          PB-CI-AH-CRIT-PER             
                                          PB-C-LF-REM-TERM              
                                          PB-C-AH-REM-TERM              
                                          PB-CHG-COUNT                  
                                          PB-LF-BILLED-AMTS             
                                          PB-AH-BILLED-AMTS             
062017                                    PB-C-INT-ON-REFS
                                          PB-CALC-TOLERANCE
                                                                        
           MOVE LOW-VALUES             TO PB-CI-AH-PAID-THRU-DT         
                                          PB-CI-AH-SETTLEMENT-DT        
                                          PB-CI-DEATH-DT                
                                          PB-CI-LF-PRIOR-CANCEL-DT      
                                          PB-CI-AH-PRIOR-CANCEL-DT      
                                          PB-CI-ENTRY-DT                
                                          PB-CI-LF-EXPIRE-DT            
                                          PB-CI-AH-EXPIRE-DT            
                                          PB-CI-LOAN-1ST-PMT-DT         
                                          PB-C-LF-CANCEL-DT             
                                          PB-C-AH-CANCEL-DT             
                                          PB-CREDIT-ACCEPT-DT           
                                          PB-BILLED-DT                  
                                          PB-ACCT-EFF-DT                
                                          PB-ACCT-EXP-DT
                                                                        
           MOVE CG-MONTH-END-DT        TO PB-CREDIT-SELECT-DT
                                                                        
062712*     MOVE 'X'                    TO PB-FATAL-FLAG
072312     IF CG-CANCEL-REISSUE
072312        MOVE '8'                 TO PB-FORCE-CODE
072312     END-IF


           .
       0070-EXIT.
           EXIT.
       0080-INIT-PNDB-ISS.

           MOVE 'PB'                   TO PENDING-BUSINESS
           INITIALIZE PB-ISSUE-RECORD
           
           MOVE CM-CONTROL-PRIMARY     TO PB-CONTROL-BY-ACCOUNT
122711     IF CM-CERT-SFX GREATER THAN SPACES
122711        MOVE CM-CERT-SFX         TO WS-WRK-SFX
122711        ADD +1                   TO WS-WRK-SFX-COMP
122711        MOVE WS-WRK-SFX          TO PB-CERT-SFX
122711     ELSE
122711        MOVE 'A'                 TO PB-CERT-SFX
122711     END-IF
           MOVE +0                     TO PB-ALT-CHG-SEQ-NO
                                          PB-BATCH-CHG-SEQ-NO
                                          PB-CSR-BATCH-CHG-SEQ-NO
           MOVE '1'                    TO PB-RECORD-TYPE
           MOVE CM-COMPANY-CD          TO PB-COMPANY-CD
                                          PB-CSR-COMPANY-CD
           MOVE WS-BATCH-NO            TO PB-ENTRY-BATCH
                                          PB-CSR-ENTRY-BATCH
           MOVE +2                     TO PB-BATCH-SEQ-NO
                                          PB-CSR-BATCH-SEQ-NO
           MOVE AM-CSR-CODE            TO PB-CSR-ID

           MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS
           MOVE CG-PROC-ID             TO PB-LAST-MAINT-BY
                                          PB-INPUT-BY
           MOVE CG-CURRENT-DT          TO PB-LAST-MAINT-DT
                                          PB-INPUT-DT
           MOVE CG-COMPANY-ID          TO PB-COMPANY-ID
           MOVE +0                     TO PB-NO-OF-ERRORS
           MOVE LOW-VALUES             TO PB-COMMON-ERRORS

           MOVE CG-MONTH-END-DT        TO PB-CREDIT-SELECT-DT
                                                                        
           MOVE ZEROS                  TO PB-I-LF-BENEFIT-CD
                                          PB-I-AH-BENEFIT-CD
062712*     MOVE 'X'                    TO PB-FATAL-FLAG
062712*     MOVE 'A'                    TO PB-FORCE-CODE

           MOVE ZEROS                  TO PB-CHG-COUNT                  
                                          PB-LF-BILLED-AMTS             
                                          PB-AH-BILLED-AMTS             
                                          PB-CALC-TOLERANCE
                                                                        
           MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           
                                          PB-BILLED-DT                  
                                          PB-ACCT-EFF-DT                
                                          PB-ACCT-EXP-DT

           .
       0080-EXIT.
           EXIT.
       0090-INIT-PNDB-BATCH.

           MOVE 'PB'                   TO PENDING-BUSINESS
           MOVE CM-COMPANY-CD          TO PB-COMPANY-CD                 
                                          PB-COMPANY-CD-A1
           MOVE CG-COMPANY-ID          TO PB-COMPANY-ID
           MOVE WS-BATCH-NO            TO PB-ENTRY-BATCH                
                                          PB-CERT-NO
           MOVE 9999                   TO PB-BATCH-SEQ-NO.              
           MOVE HIGH-VALUES            TO PB-CERT-EFF-DT.               
           MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
                                          PB-ALT-CHG-SEQ-NO

           MOVE ZEROS                  TO PB-B-LF-ISS-PRM-REMITTED
                                          PB-B-LF-ISS-PRM-ENTERED
                                          PB-B-AH-ISS-PRM-REMITTED
                                          PB-B-AH-ISS-PRM-ENTERED
                                          PB-B-ISSUE-CNT-REMITTED
                                          PB-B-ISSUE-CNT-ENTERED
                                          PB-B-CANCEL-CNT-ENTERED
                                          PB-B-LF-CAN-PRM-ENTERED
                                          PB-B-AH-CAN-PRM-ENTERED
                                          PB-B-LF-ISS-PRM-COMPUTED      
                                          PB-B-LF-CAN-PRM-COMPUTED      
                                          PB-B-AH-ISS-PRM-COMPUTED      
                                          PB-B-AH-CAN-PRM-COMPUTED      
                                          PB-LF-BILLED-AMTS             
                                          PB-AH-BILLED-AMTS             
                                          PB-CHG-COUNT                  
                                          PB-CALC-TOLERANCE
           MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           
                                          PB-BILLED-DT                  
                                          PB-ACCT-EFF-DT                
                                          PB-ACCT-EXP-DT


           .
       0090-EXIT.
           EXIT.

       0100-BUILD-ISSUE.

           PERFORM 0080-INIT-PNDB-ISS  THRU 0080-EXIT

           MOVE CG-INS-FNAME           TO PB-I-INSURED-FIRST-NAME
           MOVE CG-INS-LNAME           TO PB-I-INSURED-LAST-NAME
           MOVE CG-INS-MID-INIT        TO PB-I-INSURED-MIDDLE-INIT
           MOVE CG-INS-AGE             TO PB-i-age
           MOVE CG-JNT-FNAME           TO PB-I-JOINT-FIRST-NAME
           MOVE CG-JNT-LNAME           TO PB-I-JOINT-LAST-NAME
           MOVE CG-JNT-MID-INIT        TO PB-I-JOINT-MIDDLE-INIT
           MOVE CG-JNT-AGE             TO PB-i-joint-age
100917     move cg-vin                 to pb-i-vin

           MOVE CM-LOAN-1ST-PMT-DT     TO PB-I-1ST-PMT-DT
091312     MOVE '4'                    TO PB-I-DATA-ENTRY-SW
           MOVE CM-LOAN-APR            TO PB-I-LOAN-APR
           MOVE CM-SOC-SEC-NO          TO PB-I-SOC-SEC-NO
072312     MOVE CM-LOAN-TERM           TO PB-I-LOAN-TERM
091312     MOVE CM-LOAN-OFFICER        TO PB-I-LOAN-OFFICER


           IF CG-LF-BENCD NOT = '00' AND '  '
              MOVE CF-LIFE-OVERRIDE-L1 TO PB-LIFE-OVERRIDE-L1
              MOVE CG-LF-BENCD         TO PB-I-LIFE-BENEFIT-CD
                                          PB-I-LF-INPUT-CD
              MOVE CM-LF-ORIG-TERM     TO PB-I-LF-TERM
              MOVE CM-LF-BENEFIT-AMT   TO PB-I-LF-BENEFIT-AMT
              IF CG-LF-PREM-AMT = +9999999.99
                 MOVE '?'              TO PB-I-LF-CALC-FLAG
              ELSE
                 MOVE CG-LF-PREM-AMT   TO PB-I-LF-PREMIUM-AMT
              END-IF
072312        MOVE CG-LF-ALT-PREM-AMT  TO PB-I-LF-ALT-PREMIUM-AMT
              MOVE CM-LF-ALT-BENEFIT-AMT TO PB-I-LF-ALT-BENEFIT-AMT
              MOVE CM-LF-LOAN-EXPIRE-DT TO PB-I-LF-EXPIRE-DT
              MOVE CM-LF-CRITICAL-PERIOD TO PB-I-LF-CRIT-PER
           END-IF
           IF CG-AH-BENCD NOT = '00' AND '  '
              MOVE CF-AH-OVERRIDE-L1   TO PB-AH-OVERRIDE-L1
              MOVE CG-AH-BENCD         TO PB-I-AH-BENEFIT-CD
                                          PB-I-AH-INPUT-CD
              MOVE CM-AH-ORIG-TERM     TO PB-I-AH-TERM
              MOVE CM-AH-BENEFIT-AMT   TO PB-I-AH-BENEFIT-AMT
              IF CG-AH-PREM-AMT = +9999999.99
                 MOVE '?'              TO PB-I-AH-CALC-FLAG
              ELSE
                 MOVE CG-AH-PREM-AMT   TO PB-I-AH-PREMIUM-AMT
              END-IF
              MOVE CM-AH-LOAN-EXPIRE-DT TO PB-I-AH-EXPIRE-DT
              MOVE CM-AH-CRITICAL-PERIOD TO PB-I-AH-CRIT-PER
           END-IF
122712
122712     IF CG-COMM-PCT-ZERO = 'Y'
122712        IF ACCT-FOUND
122712            PERFORM VARYING SUB1 FROM +1 BY +1 
122712                    UNTIL SUB1 > +10
052814              IF AM-COM-TYP (SUB1) = 'C' OR 'D' OR 'F'
122712                 IF AM-A-COMA (SUB1) > SPACES OR
122712                    AM-J-COMA (SUB1) > SPACES OR
122712                    AM-L-COMA (SUB1) > SPACES
122712                       MOVE 'P' TO CG-COMM-PCT-ZERO
122712                 END-IF
122712              END-IF
122712            END-PERFORM
122712        END-IF
122712     END-IF
122712     IF CG-COMM-PCT-ZERO = 'P'
122712         MOVE 'P' TO PB-BATCH-ENTRY
122712     END-IF

           PERFORM 0200-CHECK-MAIL  THRU 0200-EXIT
100917     perform 0300-check-cert-trailer
100917                              thru 0300-exit

           EXEC CICS WRITE
               DATASET ('ERPNDB')
               FROM    (PENDING-BUSINESS)
               RIDFLD  (PB-CONTROL-PRIMARY)
               RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              SET CG-MISC-ERROR TO TRUE
              DISPLAY ' ERPNDB ISS WRITE ERROR ' WS-RESPONSE
           END-IF

           .
       0100-EXIT.
           EXIT.

       0110-GET-NEXT-SEQ.

           MOVE PB-CONTROL-PRIMARY     TO WS-ERPNDB-KEY
           MOVE 9999                   TO PNDB-BATCH-SEQ

           EXEC CICS READ
              DATASET ('ERPNDB')
              RIDFLD  (WS-ERPNDB-KEY)
              INTO    (PENDING-BUSINESS)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' BAD READ ON BATCH HEADER '
              GO TO 0110-EXIT
           END-IF

           COMPUTE WS-NEXT-SEQ-NO = PB-B-HIGHEST-SEQ-NO + +1

           .
       0110-EXIT.
           EXIT.

       0120-UPDATE-BATCH-HDR.

           MOVE PB-CONTROL-PRIMARY     TO WS-ERPNDB-KEY
           MOVE 9999                   TO PNDB-BATCH-SEQ

           EXEC CICS READ
              DATASET ('ERPNDB')
              RIDFLD  (WS-ERPNDB-KEY)
              INTO    (PENDING-BUSINESS)
              UPDATE
              RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' BAD READ UPDATE ON BATCH HEADER '
              GO TO 0120-EXIT
           END-IF

           MOVE WS-NEXT-SEQ-NO         TO PB-B-HIGHEST-SEQ-NO
           ADD +1                      TO PB-B-CANCEL-CNT-ENTERED
                                          PB-B-CANCEL-CNT-REMITTED

           COMPUTE PB-B-LF-CAN-PRM-REMITTED = PB-B-LF-CAN-PRM-REMITTED
              + CG-LF-CAN-AMT
           COMPUTE PB-B-LF-CAN-PRM-ENTERED  = PB-B-LF-CAN-PRM-ENTERED
              + CG-LF-CAN-AMT
           COMPUTE PB-B-LF-CAN-PRM-COMPUTED = PB-B-LF-CAN-PRM-COMPUTED
              + CG-LF-CAN-AMT

           COMPUTE PB-B-AH-CAN-PRM-REMITTED = PB-B-AH-CAN-PRM-REMITTED
              + CG-AH-CAN-AMT
           COMPUTE PB-B-AH-CAN-PRM-ENTERED  = PB-B-AH-CAN-PRM-ENTERED
              + CG-AH-CAN-AMT
           COMPUTE PB-B-AH-CAN-PRM-COMPUTED = PB-B-AH-CAN-PRM-COMPUTED
              + CG-AH-CAN-AMT


           EXEC CICS REWRITE
              DATASET   ('ERPNDB')
              FROM      (PENDING-BUSINESS)
              RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              DISPLAY ' BAD REWRITE ON BATCH HEADER '
              GO TO 0120-EXIT
           END-IF

           .
       0120-EXIT.
           EXIT.

       0200-CHECK-MAIL.

           EXEC CICS READ
              DATASET ('ERMAIL')
              RIDFLD  (WS-ELCERT-KEY)
              INTO    (MAILING-DATA)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 0200-EXIT
           END-IF

           MOVE 'PM'                   TO PENDING-MAILING-DATA
           MOVE PB-CONTROL-PRIMARY     TO PM-CONTROL-PRIMARY
           MOVE 'CR'                   TO PM-SOURCE-SYSTEM
           MOVE CG-CURRENT-DT          TO PM-RECORD-ADD-DT
           MOVE CG-PROC-ID             TO PM-RECORD-ADDED-BY
           move ma-profile-info        to pm-profile-info
           move ma-cred-bene-info      to pm-cred-bene-info
           move ma-post-card-mail-data to pm-post-card-mail-data

           EXEC CICS WRITE
               DATASET ('ERPNDM')
               FROM    (PENDING-MAILING-DATA)
               RIDFLD  (PM-CONTROL-PRIMARY)
               RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              SET CG-MISC-ERROR TO TRUE
              DISPLAY ' ERPNDM ISS WRITE ERROR ' WS-RESPONSE
              GO TO 0200-EXIT
           END-IF

           SET PB-I-MAIL-ADDRS-PRESENT TO TRUE
072312     MOVE MA-INSURED-BIRTH-DT    TO PB-I-BIRTHDAY
072312     MOVE MA-CRED-BENE-NAME      TO PB-I-BENEFICIARY-NAME

           .
       0200-EXIT.
           EXIT.

100917 0300-CHECK-CERT-TRAILER.
100917
100917     move ws-elcert-key          to ws-elcrtt-key
100917     move 'C'                    to ws-elcrtt-key (34:1)
100917
100917     EXEC CICS READ
100917        DATASET ('ELCRTT')
100917        RIDFLD  (WS-ELCrtt-KEY)
100917        INTO    (CERTIFICATE-TRAILERS)
100917        RESP    (WS-RESPONSE)
100917     END-EXEC
100917
100917     IF NOT RESP-NORMAL  *>  No worries,
100917        GO TO 0300-EXIT  *>  not necessary to have one
100917     END-IF
100917
100917     MOVE PB-CONTROL-by-account  TO cs-CONTROL-PRIMARY
100917     move 'C'                    to cs-trailer-type
100917     MOVE 'CS'                   TO CS-RECORD-ID
100917
100917     EXEC CICS WRITE
100917         DATASET ('ELCRTT')
100917         FROM    (CERTIFICATE-TRAILERS)
100917         RIDFLD  (CS-CONTROL-PRIMARY)
100917         RESP    (WS-RESPONSE)
100917     END-EXEC
100917
101918     IF NOT RESP-NORMAL and not RESP-DUPREC
100917        SET CG-MISC-ERROR TO TRUE
100917        DISPLAY ' ELCRTT ISS WRITE ERROR ' WS-RESPONSE
100917        GO TO 0300-EXIT
100917     END-IF
100917
100917     .
100917 0300-EXIT.
100917     EXIT.

       5000-RETURN.

           MOVE CANCEL-GEN-PASS-AREA   TO DFHCOMMAREA

00066      EXEC CICS RETURN                                             
00067          END-EXEC.                                                
00068                                                                   
00069       GOBACK.                                                     
00070                                                                   
00071  0030-CNVT-DT.                                                    
00072                                                                   
00073      EXEC CICS LINK                                               
00074          PROGRAM    ('ELDATCV')                                   
00075          COMMAREA   (DATE-CONVERSION-DATA)                        
00076          LENGTH     (DC-COMM-LENGTH)                              
00077      END-EXEC.                                                    
00078                                                                   
00079  0030-CNVT-EXIT.                                                  
00080      EXIT.                                                        
00081                                                                   
