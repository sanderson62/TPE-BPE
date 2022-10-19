       IDENTIFICATION DIVISION.
102003 PROGRAM-ID. CIDDEX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
100902******************************************************************
100902*                   C H A N G E   L O G
100902*
100902* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100902*-----------------------------------------------------------------
100902*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100902* EFFECTIVE    NUMBER
100902*-----------------------------------------------------------------
100902* 100902                   PEMA  DIVIDE OUTPUT FILE BY  DIRECT
100902*                                AND REINSURANCE
082603* 082603    2002092500008  SMVA  ADD PAYEE TYPE CD TO EXTRACT
102003* 102003    2002092500008  SMVA  ADD PAYEE INFO TO EXTRACT, ADD
102003*                                ELBENE FILE & ELMSTR, & REMOVE
102003*                                HARDCODED CO CD FOR DCC PROCESSING
011504* 011504                   SMVA  ADD INSURED DOB TO BOTH EXTR FILES
021704* 021704                   SMVA  HANDLE FILE STATUS 10,23, & 46 ERACCT
022404* 022404    2004021600003  SMVA  ADD LIIN FORM SENT DT TO EXTR FILES
030904* 030904    2003102900003  SMVA  ADD PARM-CLIENT TO PROCESSING
041404* 041404                   SMVA  ADD PAY CODE VALUE 'V' FOR ALL VOIDS
041404*                                AND LIFE VOID RECORDS TO EXTRACT
052704* 052704  IR2004052400001  SMVA  CHG DELIMITER FROM TAB TO ;   
020205* 020205  IR2005020100002  PEMA  REMOVE PLAN CODE PROCESS FOR DCC AND
020205* 020205                           FIX LOOP IN 1165-
061405* 061405                   PEMA  ADD CLP STATE PROCESS FOR DCC
031407* 031407                   PEMA  ADD DATE FILE PROCESSING, REMOVE PARM
031407* 031407                          AND CLEAN UP X'00' AND X'0C'
021011* 021011  IR2011020900001  AJRA  USE PAYEE NAME FROM PMT TRLR AND
021011* 021011                          GET ADDR FROM CORRECT SEQUENCE NO
050820* 050820  CR2020050700001  PEMA  Add pmt trlr seq no to extract
100902******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-FILE-IN     ASSIGN TO EXTRIN.

100902     SELECT EXTRD-FILE-OUT    ASSIGN TO EXTROTD
100902         ORGANIZATION IS LINE SEQUENTIAL.
100902
100902     SELECT EXTRR-FILE-OUT    ASSIGN TO EXTROTR
100902         ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERACCT          ASSIGN TO ERACCTT
                   ORGANIZATION INDEXED
                   ACCESS DYNAMIC
                   RECORD KEY AM-CONTROL-PRIMARY
                   FILE STATUS ERACCT-FILE-STATUS.

           SELECT ELTRLR          ASSIGN TO ELTRLR
                   ORGANIZATION INDEXED
                   ACCESS DYNAMIC
                   RECORD KEY AT-CONTROL-PRIMARY
                   FILE STATUS ELTRLR-FILE-STATUS.

102003     SELECT ELBENE          ASSIGN TO ELBENE
102003             ORGANIZATION INDEXED
102003             ACCESS DYNAMIC
102003             RECORD KEY BE-CONTROL-PRIMARY
102003             FILE STATUS ELBENE-FILE-STATUS.

102003     SELECT ELMSTR          ASSIGN TO ELMSTR
102003             ORGANIZATION INDEXED
102003             ACCESS DYNAMIC
102003             RECORD KEY CL-CONTROL-PRIMARY
102003             FILE STATUS ELMSTR-FILE-STATUS.

           SELECT PLAN-CODE-FILE                                        00160000
               ASSIGN TO PLNCODE                                        00170000
               ORGANIZATION IS INDEXED                                  00180000
               ACCESS IS RANDOM                                         00190000
               RECORD KEY IS VCL-PLAN                                   00200000
               STATUS IS PLAN-STATUS.                                   00210000
                                                                        00220000
           SELECT LOGIC-CONTROL-FILE
               ASSIGN TO ELCNTL
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS CF-CONTROL-PRIMARY
               FILE STATUS IS ELCNTL-STATUS.

031407     SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY ECSEXT01.
      /

100902 FD  EXTRD-FILE-OUT
100902     RECORDING MODE F
100902     LABEL RECORDS STANDARD
100902     BLOCK CONTAINS 0 RECORDS.
100902
050820 01  EXTRD-FILE-OUT-REC           PIC X(489).

100902 FD  EXTRR-FILE-OUT
100902     RECORDING MODE F
100902     LABEL RECORDS STANDARD
100902     BLOCK CONTAINS 0 RECORDS.
100902
050820 01  EXTRR-FILE-OUT-REC           PIC X(489).

       FD  ERACCT.

                                       COPY ERCACCT.

       FD  ELTRLR.

                                       COPY ELCTRLR.

102003 FD  ELBENE.
102003
102003                                 COPY ELCBENE.

102003 FD  ELMSTR.
102003
102003                                 COPY ELCMSTR.

       FD  PLAN-CODE-FILE.                                              00450000
           COPY AIRL0007.                                               00460000
                                                                        00470000
       FD  LOGIC-CONTROL-FILE.
           COPY ELCCNTL.

031407 FD  DISK-DATE
031407     COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
102003 77  FILLER  PIC X(32) VALUE '   CIDDEX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

022404 01  WS-MISC.            
031407     05  PGM-SUB          COMP-3 PIC S9(04) VALUE +585.
           05  WS-EOF-SW               PIC X     VALUE SPACES.
               88  END-OF-EXTR                   VALUE 'Y'.
102003     05  WS-STOP-SW              PIC X(01) VALUE SPACE.
102003         88  I-SAY-DONE                    VALUE 'Y'.
102003         88  NOT-DONE                      VALUE 'N'.
022404     05  WS-STOP-SW-1165         PIC X(01) VALUE SPACE.
022404         88  I-SAY-DONE-1165               VALUE 'Y'.
022404         88  NOT-DONE-1165                 VALUE 'N'.
102003     05  WS-PAYEE-TYPE-FND-SW    PIC X(01) VALUE SPACE.
102003         88  GOT-PAYEE-TYPE                VALUE 'Y'. 
102003         88  COULDNT-GET-PAYEE-TYPE        VALUE 'N'. 
           05  EXT-RECS-IN             PIC 9(9)  VALUE ZEROS.
           05  EXT-RECS-OUT            PIC 9(9)  VALUE ZEROS.
082603     05  WS-GOOD-READS-ELTRLR    PIC 9(9)  VALUE ZEROS.
082603     05  WS-BAD-READS-ELTRLR     PIC 9(9)  VALUE ZEROS.
102003     05  WS-PAYADDR-ONBENE       PIC 9(9)  VALUE ZEROS.
102003     05  WS-PAYADDR-ONACCT       PIC 9(9)  VALUE ZEROS.
102003     05  WS-PAYADDR-ONTRLR       PIC 9(9)  VALUE ZEROS.
           05  SUB1                    PIC S9(5) VALUE +0 COMP-3.
           05  ERACCT-FILE-STATUS      PIC XX    VALUE '00'.
           05  ELTRLR-FILE-STATUS      PIC XX    VALUE '00'.
102003     05  ELBENE-FILE-STATUS      PIC XX    VALUE '00'.
102003     05  ELMSTR-FILE-STATUS      PIC XX    VALUE '00'.
           05  PLAN-STATUS             PIC XX    VALUE '00'.
           05  ELCNTL-STATUS           PIC XX    VALUE '00'.
022404     05  WS-HOLD-AT-CONTROL      PIC X(20) VALUE SPACES.
021011     05  WS-HOLD-SEQ-NO          PIC 9(4)  VALUE ZEROS.
021011     05  FILLER REDEFINES WS-HOLD-SEQ-NO.
021011         10  WS-HOLD-SEQ-FIRST3  PIC 9(3).
021011         10  WS-HOLD-SEQ-LAST1   PIC 9(1).


       01  EXTR-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-EFF                  PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-CERT                 PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-REPORT-CODE-1        PIC X(10).
           12  EX-TAB7                 PIC X.
           12  EX-LNAME                PIC X(15).
           12  EX-TAB8                 PIC X.
           12  EX-FNAME                PIC X(10).
           12  EX-TAB9                 PIC X.
           12  EX-INIT                 PIC X.
           12  EX-TAB10                PIC X.
           12  EX-AGE                  PIC 99.
           12  EX-TAB11                PIC X.
           12  EX-LF-TYPE              PIC XX.
           12  EX-TAB12                PIC X.
           12  EX-LF-TERM              PIC 999.
           12  EX-TAB13                PIC X.
           12  EX-LF-BEN               PIC -9(9).99.
           12  EX-TAB14                PIC X.
           12  EX-AH-TYPE              PIC XX.
           12  EX-TAB15                PIC X.
           12  EX-AH-TERM              PIC 999.
           12  EX-TAB16                PIC X.
           12  EX-AH-BEN               PIC -9(7).99.
           12  EX-TAB17                PIC X.
           12  EX-LN-OFFICER           PIC XXX.
           12  EX-TAB18                PIC X.
           12  EX-TYPE                 PIC X.
           12  EX-TAB19                PIC X.
           12  EX-CLAIM-AMT            PIC -9(9).99.
           12  EX-TAB20                PIC X.
           12  EX-REI-CLAIM-AMT        PIC -9(9).99.
           12  EX-TAB21                PIC X.
           12  EX-INCUR                PIC X(10).
           12  EX-TAB22                PIC X.
           12  EX-REPORTED             PIC X(8).
           12  EX-TAB23                PIC X.
           12  EX-PAY                  PIC X(10).
           12  EX-TAB24                PIC X.
           12  EX-PAID-TO              PIC X(10).
           12  EX-TAB25                PIC X.
           12  EX-CNUM                 PIC X(7).
           12  EX-TAB26                PIC X.
           12  EX-CHECK                PIC X(7).
           12  EX-TAB27                PIC X.
           12  EX-CLM-PROC-DT          PIC X(10).
           12  EX-TAB28                PIC X.
           12  EX-REIN                 PIC X.
           12  EX-TAB29                PIC X.
           12  EX-REPORT-CODE-2        PIC X(10).
           12  EX-TAB30                PIC X.
           12  EX-REINCO               PIC XXX.
           12  EX-TAB31                PIC X.
           12  EX-REINCO-SUB           PIC XXX.
           12  EX-TAB32                PIC X.
           12  EX-PAY-CODE             PIC X.
           12  EX-TAB33                PIC X.
           12  EX-ACC-NAME             PIC X(30).
           12  EX-TAB34                PIC X.
           12  EX-PAID-FROM-DT         PIC X(10).
           12  EX-TAB35                PIC X.
           12  EX-VOID-DT              PIC X(10).
           12  EX-TAB36                PIC X.
           12  EX-RES-CAT              PIC XX.
082603     12  EX-TAB37                PIC X.
082603     12  EX-PAYEE-TYPE-CD        PIC X(02).
102003     12  EX-TAB38                PIC X.
102003     12  EX-PAYEE-MAIL-TO-NAME   PIC X(30).
102003     12  EX-TAB39                PIC X.
102003     12  EX-PAYEE-ADDR1          PIC X(30).
102003     12  EX-TAB40                PIC X.
102003     12  EX-PAYEE-ADDR2          PIC X(30).
102003     12  EX-TAB41                PIC X.
102003     12  EX-PAYEE-CITY-STATE     PIC X(30).
102003     12  EX-TAB42                PIC X.
102003     12  EX-PAYEE-ZIP            PIC X(09).
102003     12  EX-TAB43                PIC X.
102003     12  EX-PAYEE-INSURED-SSN    PIC X(11).
011504     12  EX-TAB44                PIC X.
011504     12  EX-INSURED-DOB          PIC X(10).
022404     12  EX-TAB45                PIC X.
022404     12  EX-LIIN-SENT-DT         PIC X(10).
061405     12  EX-TAB46                PIC X.
061405     12  EX-DCC-CLP-STATE        PIC XX.
050820     12  EX-TAB47                PIC X.
050820     12  EX-TRLR-PMT-SEQ-NO      PIC 9(4).
      ******************************************************************
       01  WS-MISC.
           05  SUB             PIC S9(3)  COMP-3 VALUE +0.
           05  DUMP                PIC X     VALUE ' '.
           05  FORCE-DUMP REDEFINES DUMP PIC S9 COMP-3.
           05  WS-ERACCT-SW            PIC X VALUE ' '.
               88  ERACCT-FINISHED           VALUE 'Y'.
050820     05  WS-SAVE-EXTR            PIC X(489) VALUE LOW-VALUES.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.
           05  WS-BEN-TYPE         PIC XX.
           05  WS-LF-PLAN.
               10  WS-LF-PLAN1     PIC X.
               10  WS-LF-PLAN2     PIC XXX.
           05  WS-AH-PLAN.
               10  WS-AH-PLAN1     PIC X.
               10  WS-AH-PLAN2     PIC XXX.

       01  FILLER.
100902     05  BENEFIT-CODE-TABLE OCCURS 450 TIMES INDEXED BY BEN-INDEX.
               10  BEN-NUM       PIC XX    VALUE HIGH-VALUE.
               10  BEN-ALPHA     PIC XXX   VALUE HIGH-VALUE.

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.


                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

030904 PROCEDURE DIVISION.

031407 0000-DATE-CARD-READ.            COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-EXTR   THRU 0100-EXIT UNTIL
              (END-OF-EXTR)
PEMTST*       OR (EXT-RECS-IN > +50000) 

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' EXTR RECORDS READ    '  EXT-RECS-IN
           DISPLAY ' EXTR RECORDS WRITTEN '  EXT-RECS-OUT
           DISPLAY ' GOOD ELTRLR READS    '  WS-GOOD-READS-ELTRLR
           DISPLAY ' BAD ELTRLR READS     '  WS-BAD-READS-ELTRLR
102003     DISPLAY ' WS-PAYADDR-ONBENE    '  WS-PAYADDR-ONBENE
102003     DISPLAY ' WS-PAYADDR-ONACCT    '  WS-PAYADDR-ONACCT
102003     DISPLAY ' WS-PAYADDR-ONTRLR    '  WS-PAYADDR-ONTRLR 
         
           GOBACK
           .

       0100-PROCESS-EXTR.

           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD

           IF (DE-CNTRL1 = AM-CONTROL-A) AND
              ((DE-EFF < AM-EXPIRE-DT) AND
              (DE-EFF NOT < AM-EFFECT-DT))
              CONTINUE
           ELSE
102003        MOVE DE-COMPANY-CD       TO AM-COMPANY-CD
              MOVE DE-CONTROL (1:25)   TO AM-CONTROL-PRIMARY (2:25)
              PERFORM 0360-STARTBR-ERACCT
                                       THRU 0360-EXIT
              PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
              PERFORM 0350-MATCH-TO-ERACCT
                                       THRU 0350-EXIT
                  UNTIL ERACCT-FINISHED
           END-IF

           MOVE AM-REPORT-CODE-1       TO EX-REPORT-CODE-1
           MOVE AM-REPORT-CODE-2       TO EX-REPORT-CODE-2

           MOVE DE-REIN                TO EX-REIN
           MOVE DE-CARRIER             TO EX-CARRIER
           MOVE DE-GROUPING            TO EX-GROUPING
           MOVE DE-STATE               TO EX-STATE
           MOVE DE-ACCOUNT             TO EX-ACCOUNT
           MOVE DE-REINCO              TO EX-REINCO
           MOVE DE-REINCO-SUB          TO EX-REINCO-SUB
           MOVE DE-EFF                 TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-EFF
           END-STRING

           MOVE DE-CERT                TO EX-CERT
      *    MOVE DE-REPORT-CODE-1       TO EX-REPORT-CODE-1
           MOVE DE-LNAME               TO EX-LNAME
           MOVE DE-FNAME               TO EX-FNAME
           MOVE DE-INIT                TO EX-INIT
           MOVE DE-AGE                 TO EX-AGE
           MOVE DE-LF-TYPE             TO EX-LF-TYPE
           MOVE DE-LF-TERM             TO EX-LF-TERM
           MOVE DE-LF-BEN              TO EX-LF-BEN
           MOVE DE-AH-TYPE             TO EX-AH-TYPE
           MOVE DE-AH-TERM             TO EX-AH-TERM
           MOVE DE-AH-BEN              TO EX-AH-BEN
      *    INSPECT DE-LN-OFFICER
      *       REPLACING ALL LOW-VALUES BY SPACES
      *    INSPECT DE-LN-OFFICER
      *       REPLACING ALL X'0C' BY SPACES
           MOVE DE-LN-OFFICER          TO EX-LN-OFFICER
           MOVE DE-TYPE                TO EX-TYPE
           MOVE DE-CLAIM-AMT           TO EX-CLAIM-AMT
           MOVE DE-REI-CLAIM-AMT       TO EX-REI-CLAIM-AMT
041404     IF DE-CLAIM-AMT > +0
041404         MOVE DE-PAY-CODE        TO EX-PAY-CODE
041404     ELSE
041404         MOVE 'V'                TO EX-PAY-CODE
041404     END-IF

           MOVE DE-INCUR               TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-INCUR
           END-STRING

           STRING DE-RPT-MO '/' DE-RPT-DA '/' DE-RPT-YR
                      DELIMITED BY SIZE
               INTO EX-REPORTED
           END-STRING

           MOVE DE-PAY                 TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-PAY
           END-STRING

           MOVE DE-PAID-TO             TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-PAID-TO
           END-STRING

           MOVE DE-CNUM                TO EX-CNUM
           MOVE DE-CHECK               TO EX-CHECK

061405     IF DTE-CLIENT = 'DCC'
061405        IF DE-CLP-STATE = SPACES OR ZEROS OR LOW-VALUES
061405           MOVE DE-STATE         TO DE-CLP-STATE
061405        END-IF
061405        MOVE DE-CLP-STATE        TO EX-DCC-CLP-STATE
061405     ELSE
061405        MOVE SPACES              TO EX-DCC-CLP-STATE
061405     END-IF

050820     if de-pmt-trailer-seq not numeric
050820        move zeros               to de-pmt-trailer-seq
050820     end-if
050820     move de-pmt-trailer-seq     to ex-trlr-pmt-seq-no

           MOVE DE-CLM-PROC-DT         TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-CLM-PROC-DT
           END-STRING
           IF DE-DAYS-DISAB NOT NUMERIC
              MOVE ZEROS               TO DE-DAYS-DISAB
           END-IF
           MOVE DE-COMPANY-CD          TO AT-COMPANY-CD
           MOVE DE-CARRIER             TO AT-CARRIER
           MOVE DE-CNUM                TO AT-CLAIM-NO
           MOVE DE-CERT                TO AT-CERT-NO
           MOVE DE-PMT-TRAILER-SEQ     TO AT-SEQUENCE-NO

021011     IF DE-PMT-TRAILER-SEQ = ZERO
021011         ADD +1 TO WS-BAD-READS-ELTRLR
021011         SET COULDNT-GET-PAYEE-TYPE   TO TRUE
021011     ELSE
082603       READ ELTRLR
082603       IF ELTRLR-FILE-STATUS = '00'
082603           ADD +1 TO WS-GOOD-READS-ELTRLR
102003           SET GOT-PAYEE-TYPE         TO TRUE
102003           MOVE AT-PAYEE-TYPE-CD      TO EX-PAYEE-TYPE-CD
021011           MOVE AT-PAYEES-NAME        TO EX-PAYEE-MAIL-TO-NAME
082603       ELSE
082603           ADD +1 TO WS-BAD-READS-ELTRLR
102003           SET COULDNT-GET-PAYEE-TYPE TO TRUE         
082603       END-IF
021011     END-IF
082603

           IF DE-DEATH
               MOVE EX-INCUR                TO EX-PAID-FROM-DT
041404     END-IF

102003     IF GOT-PAYEE-TYPE
102003*********** GOOD READ ON ELTRLR
082603             IF AT-TRAILER-TYPE = '2'
                       MOVE AT-PAID-FROM-DT TO DC-BIN-DATE-1
                       MOVE ' '             TO DC-OPTION-CODE
                       PERFORM 8510-DATE-CONVERSION
                                            THRU 8590-EXIT
                       IF NO-CONVERSION-ERROR
                           MOVE DC-GREG-DATE-A-EDIT TO EX-PAID-FROM-DT
                       END-IF
                       IF AT-VOID-DT NOT = LOW-VALUES
                           MOVE AT-VOID-DT  TO DC-BIN-DATE-1
                           MOVE ' '         TO DC-OPTION-CODE
                           PERFORM 8510-DATE-CONVERSION
                                            THRU 8590-EXIT
                           IF NO-CONVERSION-ERROR
                               MOVE DC-GREG-DATE-A-EDIT TO EX-VOID-DT
                           END-IF
                       END-IF
102003             END-IF
           ELSE
102003************** DIDN'T GET ELTRLR RECORD
                   MOVE DE-PAID-TO          TO DC-GREG-DATE-CYMD
                   MOVE 'l'                 TO DC-OPTION-CODE
                   PERFORM 8510-DATE-CONVERSION
                                            THRU 8590-EXIT
                   IF NO-CONVERSION-ERROR
                       MOVE +0              TO DC-ELAPSED-MONTHS
                       COMPUTE DC-ELAPSED-DAYS = DE-DAYS-DISAB * -1
                       MOVE '6'              TO DC-OPTION-CODE
                       PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
                       IF NO-CONVERSION-ERROR
                           MOVE DC-GREG-DATE-B-EDIT TO EX-PAID-FROM-DT
                       END-IF
                   END-IF
           END-IF

102003     IF GOT-PAYEE-TYPE
102003         EVALUATE TRUE
102003         WHEN AT-PAYEE-TYPE-CD = 'B0'
102003             PERFORM 1000-READ-ELMSTR         THRU 1000-EXIT
102003             IF ELMSTR-FILE-STATUS = '00'
011504                 IF CL-INSURED-BIRTH-DT NOT = SPACES OR LOW-VALUES
011504                     MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
011504                     MOVE ' '                 TO DC-OPTION-CODE
011504                     PERFORM 8510-DATE-CONVERSION
011504                                              THRU 8590-EXIT
011504                     IF NO-CONVERSION-ERROR
011504                         MOVE DC-GREG-DATE-A-EDIT
011504                                              TO EX-INSURED-DOB
011504                     END-IF
011504                 ELSE
011504                     MOVE SPACES              TO EX-INSURED-DOB
011504                 END-IF
011504                 
102003                 PERFORM 1050-GET-PAYADDR-ONBENE THRU 1050-EXIT
102003             END-IF
102003
102003         WHEN AT-PAYEE-TYPE-CD = 'A0'
021011*             MOVE AM-NAME             TO EX-PAYEE-MAIL-TO-NAME  
102003             MOVE AM-PERSON           TO EX-PAYEE-ADDR1  
102003             MOVE AM-ADDRS            TO EX-PAYEE-ADDR2 
102003             MOVE AM-CITY             TO EX-PAYEE-CITY-STATE
102003             MOVE AM-ZIP              TO EX-PAYEE-ZIP
102003             ADD +1 TO WS-PAYADDR-ONACCT
011504             PERFORM 1000-READ-ELMSTR         THRU 1000-EXIT
011504             IF ELMSTR-FILE-STATUS = '00'
011504                 IF CL-INSURED-BIRTH-DT NOT = SPACES OR LOW-VALUES
011504                     MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
011504                     MOVE ' '                 TO DC-OPTION-CODE
011504                     PERFORM 8510-DATE-CONVERSION
011504                                              THRU 8590-EXIT
011504                     IF NO-CONVERSION-ERROR
011504                         MOVE DC-GREG-DATE-A-EDIT
011504                                              TO EX-INSURED-DOB
011504                     END-IF
011504                 ELSE             
011504                     MOVE SPACES              TO EX-INSURED-DOB
011504                 END-IF
011504             END-IF
102003
102003         WHEN AT-PAYEE-TYPE-CD (1:1) = 'I'
102003             PERFORM 1100-GET-PAYADDR-ONTRLR  THRU 1100-EXIT
102003             PERFORM 1000-READ-ELMSTR         THRU 1000-EXIT
102003             IF ELMSTR-FILE-STATUS = '00'
102003                 MOVE CL-SOC-SEC-NO   TO EX-PAYEE-INSURED-SSN 
011504                 IF CL-INSURED-BIRTH-DT NOT = SPACES OR LOW-VALUES
011504                     MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
011504                     MOVE ' '                 TO DC-OPTION-CODE
011504                     PERFORM 8510-DATE-CONVERSION
011504                                              THRU 8590-EXIT
011504                     IF NO-CONVERSION-ERROR
011504                         MOVE DC-GREG-DATE-A-EDIT
011504                                              TO EX-INSURED-DOB
011504                     END-IF
011504                 ELSE
011504                     MOVE SPACES              TO EX-INSURED-DOB
011504                 END-IF
102003             END-IF
102003
102003         WHEN OTHER
021011*             DISPLAY '*** OTHER PAYEE TYPE ' AT-PAYEE-TYPE-CD
102003             PERFORM 1100-GET-PAYADDR-ONTRLR  THRU 1100-EXIT
011504             PERFORM 1000-READ-ELMSTR         THRU 1000-EXIT
011504             IF ELMSTR-FILE-STATUS = '00'
011504                 IF CL-INSURED-BIRTH-DT NOT = SPACES OR LOW-VALUES
011504                     MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
011504                     MOVE ' '                 TO DC-OPTION-CODE
011504                     PERFORM 8510-DATE-CONVERSION
011504                                              THRU 8590-EXIT
011504                     IF NO-CONVERSION-ERROR
011504                         MOVE DC-GREG-DATE-A-EDIT
011504                                              TO EX-INSURED-DOB
011504                     END-IF
011504                 ELSE
011504                     MOVE SPACES              TO EX-INSURED-DOB
011504                 END-IF
011504             END-IF
102003         END-EVALUATE 
102003     END-IF 

           MOVE DE-ACC-NAME            TO EX-ACC-NAME

           MOVE DE-LF-TYPE             TO WS-LF-PLAN
           MOVE DE-AH-TYPE             TO WS-AH-PLAN

           IF DTE-CLIENT NOT = 'DCC'
              PERFORM 1200-GET-PLAN    THRU 1200-EXIT

              IF (DE-TYPE = '1' OR '3')
                 AND (DE-LF-TYPE NOT = ZERO)
                 MOVE WS-LF-PLAN       TO VCL-PLAN
                 PERFORM 0150-GET-PLAN-DATA
                                       THRU 0150-EXIT
                 IF PLAN-STATUS = '00'
                   MOVE VCL-CLM-RESERVE-CATEGORY-CODE
                                       TO EX-RES-CAT
                 END-IF
              END-IF

              IF (DE-TYPE = '2' OR '4')
                 AND (DE-AH-TYPE NOT = ZERO)
                 MOVE WS-AH-PLAN       TO VCL-PLAN
                 PERFORM 0150-GET-PLAN-DATA
                                       THRU 0150-EXIT
                 IF PLAN-STATUS = '00'
                   MOVE VCL-CLM-RESERVE-CATEGORY-CODE
                                       TO EX-RES-CAT
                 END-IF
              END-IF
           END-IF

022404     IF DE-DEATH
022404         PERFORM 1160-GET-LIIN-SENT-DT THRU 1160-EXIT
022404     END-IF

           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0150-GET-PLAN-DATA.                                              03360000
      *                                                                 03370000
           READ PLAN-CODE-FILE.                                         03390000
                                                                        03400000
           IF PLAN-STATUS = '00'                                        03410000
              CONTINUE                                                  03420000
           ELSE                                                         03460000
           IF PLAN-STATUS = '23'                                        03470000
              DISPLAY 'PLAN CODE NOT FOUND: ' VCL-PLAN                  03480000
           ELSE                                                         03490000
              DISPLAY 'READ ERROR ' PLAN-STATUS ' ON PLNCODE'           03500000
              ADD +1 TO FORCE-DUMP.                                     03510000
                                                                        03520000
       0150-EXIT.                                                       03530000
           EXIT.                                                        03540000
                                                                        03550000
       0200-READ-EXTR.

           READ EXTR-FILE-IN AT END
              SET END-OF-EXTR          TO TRUE
           END-READ


           IF NOT END-OF-EXTR
              ADD 1 TO EXT-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           INSPECT EXTR-DETAIL-RECORD
              REPLACING ALL X'00'      BY SPACES
                        ALL X'0C'      BY SPACES
                        ALL ';'        BY SPACES
                        ALL X'09'      BY ';'

100902     IF EX-REIN = 'R'
100902        WRITE EXTRR-FILE-OUT-REC     FROM EXTR-DETAIL-RECORD
100902     ELSE
100902        WRITE EXTRD-FILE-OUT-REC     FROM EXTR-DETAIL-RECORD
100902     END-IF
           
           ADD 1 TO EXT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0350-MATCH-TO-ERACCT.

           IF (DE-CNTRL1 > AM-CONTROL-A)
              PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
           ELSE
              IF (DE-CNTRL1 = AM-CONTROL-A) AND
                 ((DE-EFF < AM-EXPIRE-DT)   AND
                 (DE-EFF NOT < AM-EFFECT-DT))
                 SET ERACCT-FINISHED TO TRUE
              ELSE
                 IF DE-CNTRL1 < AM-CONTROL-A
                    MOVE 'NOT FOUND'   TO AM-REPORT-CODE-1
                                          AM-REPORT-CODE-2
                    SET ERACCT-FINISHED TO TRUE
                 ELSE
                    PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
                 END-IF
              END-IF
           END-IF

           .
       0350-EXIT.
           EXIT.

       0360-STARTBR-ERACCT.

           MOVE ' '                    TO WS-ERACCT-SW
           START ERACCT KEY IS NOT < AM-CONTROL-PRIMARY

021704     EVALUATE TRUE
021704     WHEN ERACCT-FILE-STATUS = '00'
               CONTINUE
021704     WHEN ERACCT-FILE-STATUS = '10' OR '23'
021704         MOVE HIGH-VALUES      TO AM-CONTROL-PRIMARY
021704     WHEN OTHER
               DISPLAY ' ERROR ON STARTBR ERACCT '
                            ERACCT-FILE-STATUS
               PERFORM ABEND-PGM
021704     END-EVALUATE

           .
       0360-EXIT.
           EXIT.

       0370-READNEXT-ERACCT.

021704     IF AM-CONTROL-PRIMARY = HIGH-VALUES
021704         GO TO 0370-EXIT
021704     END-IF

           READ ERACCT NEXT RECORD

           IF ERACCT-FILE-STATUS = '00'
102003        IF AM-COMPANY-CD > DE-COMPANY-CD
                 MOVE HIGH-VALUES      TO AM-CONTROL-PRIMARY
              END-IF
           ELSE
              IF ERACCT-FILE-STATUS = '10' OR '23'
                 MOVE HIGH-VALUES      TO AM-CONTROL-PRIMARY
              ELSE
                 DISPLAY ' ERROR ON READNEXT ERACCT '
                            ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0370-EXIT.
           EXIT.


       0400-OPEN-FILES.

           OPEN INPUT EXTR-FILE-IN
                      ERACCT
                      PLAN-CODE-FILE
                      ELTRLR
102003                ELBENE
102003                ELMSTR
               OUTPUT EXTRD-FILE-OUT 
                      EXTRR-FILE-OUT

           IF PLAN-STATUS = '00' OR '97'                                01390000
               CONTINUE                                                 01400000
           ELSE                                                         01410000
               DISPLAY 'OPEN ERROR ' PLAN-STATUS ' ON PLNCODE'          01420000
               DISPLAY '*** JOB CANCELLED ***'                          01430000
               ADD +1 TO FORCE-DUMP                                     01440000
           END-IF                                                       01450000
           IF ERACCT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON OPEN  - ERACCT ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF eltrlr-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON OPEN  - eltrlr ' eltrlr-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-FILE-IN
                 EXTRD-FILE-OUT 
                 ERACCT 
                 ELTRLR
                 PLAN-CODE-FILE
                 EXTRR-FILE-OUT
102003           ELBENE 
102003           ELMSTR 
           IF ERACCT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON CLOSE - ERACCT ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELTRLR-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON CLOSE - eltrlr ' eltrlr-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0500-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
052704     MOVE X'09'                  TO EX-TAB1
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
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TAB15
                                          EX-TAB16
                                          EX-TAB17
                                          EX-TAB18
                                          EX-TAB19
                                          EX-TAB20
                                          EX-TAB21
                                          EX-TAB22
                                          EX-TAB23
                                          EX-TAB24
                                          EX-TAB25
                                          EX-TAB26
                                          EX-TAB27
                                          EX-TAB28
                                          EX-TAB29
                                          EX-TAB30
                                          EX-TAB31
                                          EX-TAB32
                                          EX-TAB33
                                          EX-TAB34
                                          EX-TAB35
                                          EX-TAB36
102003                                    EX-TAB37
102003                                    EX-TAB38
102003                                    EX-TAB39
102003                                    EX-TAB40
102003                                    EX-TAB41
102003                                    EX-TAB42
102003                                    EX-TAB43
011504                                    EX-TAB44
022404                                    EX-TAB45
061405                                    EX-TAB46
050820                                    EX-TAB47

           MOVE EXTR-DETAIL-RECORD         TO WS-SAVE-EXTR
102003     PERFORM 0200-READ-EXTR          THRU 0200-EXIT
           PERFORM 9000-LOAD-BENEFIT-TABLE THRU 9000-EXIT
           MOVE LOW-VALUES                 TO AM-CONTROL-PRIMARY
102003     MOVE DE-COMPANY-CD              TO AM-COMPANY-CD
           PERFORM 0360-STARTBR-ERACCT     THRU 0360-EXIT
           PERFORM 0370-READNEXT-ERACCT    THRU 0370-EXIT
           .

       0600-EXIT.
           EXIT.

102003 1000-READ-ELMSTR.
102003
102003     MOVE DE-COMPANY-CD         TO CL-COMPANY-CD
102003     MOVE DE-CARRIER            TO CL-CARRIER
102003     MOVE DE-CNUM               TO CL-CLAIM-NO
102003     MOVE DE-CERT               TO CL-CERT-NO
102003
102003     READ ELMSTR
102003
102003     .
102003 1000-EXIT.
102003     EXIT.

102003 1050-GET-PAYADDR-ONBENE.
102003
102003     MOVE DE-COMPANY-CD         TO BE-COMPANY-CD
102003     MOVE 'B'                   TO BE-RECORD-TYPE
102003     MOVE CL-BENEFICIARY        TO BE-BENEFICIARY
102003
102003     READ ELBENE
102003
102003     EVALUATE TRUE
102003     WHEN ELBENE-FILE-STATUS = '00'
021011*         MOVE BE-MAIL-TO-NAME   TO EX-PAYEE-MAIL-TO-NAME
102003         MOVE BE-ADDRESS-LINE-1 TO EX-PAYEE-ADDR1
102003         MOVE BE-ADDRESS-LINE-2 TO EX-PAYEE-ADDR2
102003         MOVE BE-CITY-STATE     TO EX-PAYEE-CITY-STATE
102003         MOVE BE-ZIP-PRIME      TO EX-PAYEE-ZIP
102003         ADD +1 TO WS-PAYADDR-ONBENE
102003
102003     WHEN ELBENE-FILE-STATUS = '23'
102003         CONTINUE
102003
102003     WHEN OTHER
102003         DISPLAY 'ERROR READING ELBENE STATUS= '
102003                  ELBENE-FILE-STATUS
102003     END-EVALUATE
102003
102003     .
102003 1050-EXIT.
102003     EXIT.
102003
102003
102003 1100-GET-PAYADDR-ONTRLR.
102003  
102003     MOVE +1                      TO AT-SEQUENCE-NO
102003     START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY
102003     IF ELTRLR-FILE-STATUS NOT = '00'
102003         DISPLAY ' ERROR ON STARTBR ELTRLR '
102003                 ELTRLR-FILE-STATUS
102003         GO TO 1100-EXIT
102003     END-IF
102003     SET NOT-DONE                 TO TRUE
102003     PERFORM 1155-READNEXT-ELTRLR THRU 1155-EXIT
102003         UNTIL I-SAY-DONE
102003 
102003     .
102003 1100-EXIT.
102003     EXIT.


102003 1155-READNEXT-ELTRLR.
102003
102003     READ ELTRLR NEXT RECORD
102003
           IF ELTRLR-FILE-STATUS NOT = '00'
              SET I-SAY-DONE           TO TRUE
              GO TO 1155-EXIT
           END-IF

102003     IF AT-SEQUENCE-NO >= +1
102003        AND  <= +69
102003         CONTINUE
102003     ELSE
102003        SET I-SAY-DONE          TO TRUE
102003        GO TO 1155-EXIT
102003     END-IF
102003
102003
102003     IF AT-TRAILER-TYPE = '5'
102003         CONTINUE
102003     ELSE
102003         GO TO 1155-EXIT
102003     END-IF
102003
021011     MOVE AT-SEQUENCE-NO        TO WS-HOLD-SEQ-NO
021011
102003     IF AT-ADDRESS-TYPE = EX-PAYEE-TYPE-CD (1:1)
021011       AND WS-HOLD-SEQ-LAST1 = EX-PAYEE-TYPE-CD (2:1)
021011*         MOVE AT-MAIL-TO-NAME   TO EX-PAYEE-MAIL-TO-NAME
102003         MOVE AT-ADDRESS-LINE-1 TO EX-PAYEE-ADDR1  
102003         MOVE AT-ADDRESS-LINE-2 TO EX-PAYEE-ADDR2 
102003         MOVE AT-CITY-STATE     TO EX-PAYEE-CITY-STATE
102003         MOVE AT-ZIP            TO EX-PAYEE-ZIP
102003         SET I-SAY-DONE         TO TRUE
102003         ADD +1 TO WS-PAYADDR-ONTRLR
102003     END-IF
102003
102003     .
102003 1155-EXIT.
102003     EXIT.

022404 1160-GET-LIIN-SENT-DT.
022404  
022404     MOVE AT-CONTROL-PRIMARY (1:20)       TO WS-HOLD-AT-CONTROL
022404     MOVE +70                             TO AT-SEQUENCE-NO
022404     START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY
022404     IF ELTRLR-FILE-STATUS NOT = '00'
022404         DISPLAY ' ERROR ON STARTBR ELTRLR para 1165 '
022404                 ELTRLR-FILE-STATUS
022404         MOVE 'ARCHIVED  '                TO EX-LIIN-SENT-DT
022404         GO TO 1160-EXIT
022404     END-IF
022404     SET NOT-DONE-1165                    TO TRUE
022404     PERFORM 1165-READNEXT-ELTRLR         THRU 1165-EXIT
022404         UNTIL I-SAY-DONE-1165
022404 
022404     .
022404 1160-EXIT.
022404     EXIT.


022404 1165-READNEXT-ELTRLR.
022404

022404     READ ELTRLR NEXT RECORD
022404
           IF ELTRLR-FILE-STATUS NOT = '00'
              SET I-SAY-DONE-1165      TO TRUE
              GO TO 1165-EXIT
           END-IF

022404     IF AT-CONTROL-PRIMARY (1:20) = WS-HOLD-AT-CONTROL
022404         CONTINUE
022404     ELSE
022404         MOVE 'NO LIIN   '                TO EX-LIIN-SENT-DT
022404         SET I-SAY-DONE-1165              TO TRUE
022404         GO TO 1165-EXIT
022404     END-IF

022404     IF AT-TRAILER-TYPE = '4'
022404         CONTINUE
022404     ELSE
022404         GO TO 1165-EXIT
022404     END-IF

022404     IF AT-STD-LETTER-FORM = 'LIIN'
022404         IF AT-LETTER-SENT-DT NOT = SPACES OR LOW-VALUES
022404             MOVE AT-LETTER-SENT-DT       TO DC-BIN-DATE-1
022404             MOVE ' '                     TO DC-OPTION-CODE
022404             PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
022404             IF NO-CONVERSION-ERROR
022404                 MOVE DC-GREG-DATE-A-EDIT TO EX-LIIN-SENT-DT
022404             END-IF
022404         ELSE
022404             MOVE 'NO DATE   '            TO EX-LIIN-SENT-DT
022404         END-IF
022404                 
022404         SET I-SAY-DONE-1165              TO TRUE
022404     END-IF
022404
022404     .
022404 1165-EXIT.
022404     EXIT.


       1200-GET-PLAN.
      *
           MOVE SPACES TO WS-LF-PLAN
           MOVE SPACES TO WS-AH-PLAN

           IF DE-LF-TYPE NOT = ZERO
              IF DE-IG = '1'
                 MOVE 'I' TO WS-LF-PLAN1
              ELSE
                 MOVE 'G' TO WS-LF-PLAN1
              END-IF
              SET BEN-INDEX TO +1
              SEARCH BENEFIT-CODE-TABLE
                 WHEN BEN-NUM (BEN-INDEX) = DE-LF-TYPE
                   MOVE BEN-ALPHA (BEN-INDEX) TO WS-LF-PLAN2
                 WHEN BEN-NUM (BEN-INDEX) = HIGH-VALUE
                   MOVE ZEROS TO WS-LF-PLAN2
                   DISPLAY 'INVALID LIFE BEN TYPE: '
                            DE-LF-TYPE '  ' DE-CONTROL
               END-SEARCH
           END-IF

           IF DE-AH-TYPE NOT = ZERO
              IF DE-IG = '1'
                 MOVE 'I' TO WS-AH-PLAN1
              ELSE
                 MOVE 'G' TO WS-AH-PLAN1
              END-IF
              MOVE DE-AH-TYPE TO WS-AH-PLAN2
           END-IF
           .
       1200-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.

       9000-LOAD-BENEFIT-TABLE.
      *
           OPEN INPUT LOGIC-CONTROL-FILE
           IF ELCNTL-STATUS = '00' OR '97'
               MOVE '00' TO ELCNTL-STATUS
           ELSE
               DISPLAY 'OPEN ERROR ' ELCNTL-STATUS ' ON ELCNTL'
               ADD +1 TO FORCE-DUMP.

           SET BEN-INDEX TO +1.
030904     MOVE SPACES                 TO CF-CONTROL-PRIMARY.
030904     MOVE DTE-CLIENT             TO CF-COMPANY-ID.
030904     MOVE '4'                    TO CF-RECORD-TYPE. 
030904     START LOGIC-CONTROL-FILE KEY NOT < CF-CONTROL-PRIMARY.

030904     IF ELCNTL-STATUS = '00'
030904         CONTINUE
030904     ELSE
030904         DISPLAY ' ERROR ON START ELCNTL '
030904                      ELCNTL-STATUS
030904         PERFORM ABEND-PGM       THRU APS-EXIT 
030904     END-IF.

           PERFORM UNTIL ELCNTL-STATUS NOT = '00'
030904                   OR CF-COMPANY-ID NOT = DTE-CLIENT
030904         READ LOGIC-CONTROL-FILE NEXT RECORD
030904         IF CF-COMPANY-ID = DTE-CLIENT AND CF-RECORD-TYPE = '4'
                   PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > +8
                       MOVE CF-BENEFIT-NUMERIC (SUB) TO 
                                                   BEN-NUM (BEN-INDEX)
                       MOVE CF-BENEFIT-ALPHA (SUB)   TO 
                                                   BEN-ALPHA (BEN-INDEX)
                       SET BEN-INDEX UP BY +1
100902                 IF BEN-INDEX > 450 
                           DISPLAY
                           'TABLE OVERFLOW IN 0100-LOAD-BENEFIT-TABLE'
                           ADD +1 TO FORCE-DUMP
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

           CLOSE LOGIC-CONTROL-FILE
           .
       9000-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.

