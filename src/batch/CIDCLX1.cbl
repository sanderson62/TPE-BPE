       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCLX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
021704******************************************************************
021704*                   C H A N G E   L O G
021704*
021704* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
021704*-----------------------------------------------------------------
021704*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
021704* EFFECTIVE    NUMBER
021704*-----------------------------------------------------------------
021704* 021704                   SMVA  ADD DCC TO PROCESSING  
052704* 052704 IR2004052400001   SMVA  CHG DELIMITER FROM TAB TO ;
092204* 092204 CR2004090800004   PEMA  ADD BIRTH DATE TO EXTRACT
091207* 091207 CR2007010300001   PEMA  ADD NO OF PMTS THRU LST PD AMT
021009* 021009 CR2008021500002   PEMA  ADD TERM
042309* 042309  CR2008052100001  PEMA  ADD FIELDS TO EXTRACT
082710* 082710 CR2010042900001   PEMA  REFORMAT CITY STAT FIELD
043012* 043012 CR2012042700004   PEMA  ADD TOT INTEREST PAID
062812* 062812 CR2012052100001   PEMA  ADD NEXT AUTO PAY DT
040814* 040814 CR2014030500002   AJRA  ADD ICD CODES
071014* 071014 CR2014022100001   AJRA  ADD FIELDS FOR FAM LEAVE
092215* 092215 CR2015060300001   TANA  ADD ELTRLR FILE OUTPUT
013017* 013017 CR2016053100001   PEMA  ADD LOAN NUMBER TO EXTRACT
052217* 052217 CR2016053100001   TANA  ADD ACH PAYMENT TO EXTRACT
062217* 062217 CR2017050300002   TANA  ADD AUTH RCVD TO EXTRACT
081817* 081817 IR2016100700001   TANA  ADD # EXTENSIONS , CRIT PERIOD
100917* 100917 CR2017092200001   PEMA  ADD CLAIMANT TYPE TO EXTRACT
020218* 020218 CR2017062000002   PEMA  add  age fields to extract
121018* 121018 CR2018120700001   PEMA  add claim type O
042320* 042320 CR2020042200002   PEMA  Include payment notes(eob codes)
050420* 050420 IR2020042900001   PEMA  Correct garbage in extract rec
021704******************************************************************
       ENVIRONMENT DIVISION.
050420 CONFIGURATION SECTION.
050420 SPECIAL-NAMES.
050420 CLASS VALID-FIELD 'A' THRU 'I'
050420 'J' THRU 'R'
050420 'S' THRU 'Z'
050420 '0' THRU '9'
050420 ' '.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACCT           ASSIGN TO ERACCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

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

042320     SELECT ELTRLRN          ASSIGN TO ELTRLRN
042320                             ORGANIZATION IS INDEXED
042320                             ACCESS IS RANDOM
042320                             RECORD KEY IS PN-CONTROL-PRIMARY
042320                             FILE STATUS IS ELTRLRN-FILE-STATUS.

           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.

           SELECT ELMSTR-OUT       ASSIGN TO ELMSTROT
               ORGANIZATION IS LINE SEQUENTIAL.

092215     SELECT ELTRLR-OUT       ASSIGN TO ELTRLROT
092215         ORGANIZATION IS LINE SEQUENTIAL.

021704     SELECT DISK-DATE        ASSIGN TO SYS019.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

           COPY ERCACCT.

       FD  ELMSTR.

           COPY ELCMSTR.

       FD  ELTRLR.

           COPY ELCTRLR.

042320 FD  ELTRLRN.
042320
042320 01  PN-ACTIVITY-TRAILERS.                                           
042320     12  FILLER                  PIC XX.
042320     12  PN-CONTROL-PRIMARY.                                      
042320         16  PN-COMPANY-CD               PIC X.                   
042320         16  PN-CARRIER                  PIC X.                   
042320         16  PN-CLAIM-NO                 PIC X(7).                
042320         16  PN-CERT-NO                  PIC X(11).
042320         16  PN-SEQUENCE-NO              PIC S9(4)     COMP.
042320                                                                  
042320     12  PN-TRAILER-TYPE                 PIC X.                   
042320                                                                  
042320     12  PN-RECORDED-DT                  PIC XX.                  
042320     12  PN-RECORDED-BY                  PIC X(4).                
042320     12  PN-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.    
042320                                                                  
042320     12  PN-TRAILER-BODY                 PIC X(165).              
042320     12  PN-GENERAL-INFO-TR  REDEFINES  PN-TRAILER-BODY.      
042320         16  PN-INFO-LINE-1              PIC X(60).           
042320         16  FILLER REDEFINES PN-INFO-LINE-1.
042320             20  PN-NOTE-ERROR-NO OCCURS 15
042320                                         PIC X(4).
042320         16  PN-INFO-LINE-2              PIC X(60).           
042320         16  FILLER REDEFINES PN-INFO-LINE-2.
042320             20  PN-ICD-CODE-1           PIC X(8).
042320             20  PN-ICD-CODE-2           PIC X(8).
042320             20  FILLER                  PIC X(44).
042320         16  PN-INFO-TRAILER-TYPE        PIC X.               
042320             88  PN-ERRORS-NOTE          VALUE 'E'.
042320             88  PN-PAYMENT-NOTE         VALUE 'P'.           
042320             88  PN-CALL-NOTE            VALUE 'C'.           
042320             88  PN-MAINT-NOTE           VALUE 'M'.           
042320             88  PN-CERT-CHANGE          VALUE 'X'.           
042320             88  PN-APPROVAL-NOTE        VALUE 'R'.
042320             88  PN-NOTE-FILE-NOTE       VALUE 'N'.
042320             88  PN-CERT-CANCELLED       VALUE 'T'.
042320         16  PN-CALL-TYPE                PIC X.               
042320             88  PN-PHONE-CALL-IN        VALUE 'I'.           
042320             88  PN-PHONE-CALL-NEW       VALUE 'N'.           
042320             88  PN-PHONE-CALL-OUT       VALUE 'O'.           
042320         16  PN-NOTE-CONTINUATION        PIC X.               
042320             88  PN-CONTINUED-NOTE       VALUE 'X'.
042320         16  PN-EOB-CODES-EXIST          PIC X.
042320             88  PN-EOB-CODES-PRESENT    VALUE 'Y'.
042320         16  FILLER                      PIC X(35).           
042320         16  PN-GEN-INFO-LAST-MAINT-DT   PIC XX.              
042320         16  PN-GEN-INFO-LAST-UPDATED-BY PIC X(4).            

       FD  ELCERT.

           COPY ELCCERT.

       FD  ELMSTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

020218 01  ELMSTR-OUT-REC              PIC X(632).

092215 FD  ELTRLR-OUT
092215     RECORDING MODE F
092215     LABEL RECORDS STANDARD
092215     BLOCK CONTAINS 0 RECORDS.
092215
062217 01  ELTRL-OUT-REC               PIC X(1575).

021704 FD  DISK-DATE
021704     COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCLX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELMSTR             VALUE 'Y'.
       77  CLM-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CRT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  TRL-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CLM-RECS-OUT            PIC 9(9) VALUE ZEROS.
092215 77  TRL-RECS-OUT            PIC 9(9) VALUE ZEROS.
092215 77  X                       PIC S9(5) VALUE +0 COMP-3.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  WS-DIAGNOSIS            PIC X(60) VALUE SPACES.
013017 77  ws-loan-no              pic x(25) value spaces.
040814 77  WS-ICD-CODE-1           PIC X(8)  VALUE SPACES.
040814 77  WS-ICD-CODE-2           PIC X(8)  VALUE SPACES.
       77  WS-LAST-ACT-DT          PIC XX    VALUE LOW-VALUES.
       77  WS-ACTIVITY-DT          PIC XX    VALUE LOW-VALUES.
       77  WS-LAST-ACT-TYPE        PIC X(15) VALUE SPACES.
       77  WS-ACTIVITY-TYPE        PIC X(15) VALUE SPACES.
       77  WS-FORM                 PIC X(4)  VALUE SPACES.
       77  WS-GOT-IT-SW            PIC X     VALUE ' '.
           88  WE-GOT-IT-ALL                 VALUE 'Y'.
       77  WS-AM-NAME                  PIC X(30)  VALUE SPACES.
020218 77  ws-current-bin-dt           pic xx    value low-values.
020218 77  ws-work-age                 pic s999 comp-3 value zeros.
020218 77  ws-clmnt-age                pic s999 comp-3 value zeros.

       01  WS-INS-NAME-AND-ADDRESS.
           05  WS-INS-NAME             PIC X(30).
           05  WS-INS-ADDR1            PIC X(30).
           05  WS-INS-ADDR2            PIC X(30).
           05  WS-INS-CITY-ST          PIC X(30).
           05  WS-INS-ZIP              PIC X(9).
           05  WS-INS-PHONE            PIC X(13).
           05  WS-WORK-PHONE           PIC X(11).

100917*****LENGTH OF ELMSTR-DETAIL-RECORD IS CURRENTLY 632.
020218 01  WS-SAVE-ELMSTR              PIC X(632) VALUE LOW-VALUES.
       01  ELMSTR-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-CLAIM-NO             PIC X(7).
           12  EX-TAB2                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-INSURED-LAST-NAME    PIC X(15).
           12  EX-TAB5                 PIC X.
           12  EX-INSURED-1ST-NAME     PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-CLAIM-STATUS         PIC X.
           12  EX-TAB7                 PIC X.
           12  EX-CLAIM-TYPE           PIC X.
           12  EX-TAB8                 PIC X.
           12  EX-FILE-ESTABLISH-DT    PIC X(10).
           12  EX-TAB9                 PIC X.
           12  EX-TOTAL-PAID-AMT       PIC -9(7).99.
           12  EX-TAB10                PIC X.
           12  EX-INSURED-SEX-CD       PIC X.
           12  EX-TAB11                PIC X.
           12  EX-ACCOUNT-ADDR-CNT     PIC 9.
           12  EX-TAB12                PIC X.
           12  EX-CERT-STATE           PIC XX.
           12  EX-TAB13                PIC X.
           12  EX-CERT-ACCOUNT         PIC X(10).
           12  EX-TAB14                PIC X.
           12  EX-CERT-EFF-DT          PIC X(10).
           12  EX-TAB15                PIC X.
           12  EX-INCURRED-DT          PIC X(10).
           12  EX-TAB16                PIC X.
           12  EX-REPORTED-DT          PIC X(10).
           12  EX-TAB17                PIC X.
           12  EX-PAID-THRU-DT         PIC X(10).
           12  EX-TAB18                PIC X.
           12  EX-LAST-PMT-DT          PIC X(10).
           12  EX-TAB19                PIC X.
           12  EX-LAST-MAINT-DT        PIC X(10).
           12  EX-TAB20                PIC X.
           12  EX-LAST-MAINT-USER      PIC X(4).
           12  EX-TAB21                PIC X.
           12  EX-LAST-MAINT-TYPE      PIC X.
           12  EX-TAB22                PIC X.
           12  EX-DIAG                 PIC X(60).
           12  EX-TAB23                PIC X.
           12  EX-EXP-DT               PIC X(10).
           12  EX-TAB24                PIC X.
           12  EX-LAST-CLOSE-REASON    PIC X.
           12  EX-TAB25                PIC X.
092204     12  EX-BIRTH-DT             PIC X(10).
092204     12  EX-TAB26                PIC X.
091207     12  EX-NO-OF-PMTS           PIC ZZ9.
091207     12  EX-TAB27                PIC X.
           12  EX-LAST-ACT-DT          PIC X(10).
           12  EX-TAB28                PIC X.
           12  EX-ACTIVITY-DT          PIC X(10).
           12  EX-TAB29                PIC X.
           12  EX-LAST-ACT-TYPE        PIC X(15).
           12  EX-TAB30                PIC X.
           12  EX-ACTIVITY-TYPE        PIC X(15).
           12  EX-TAB31                PIC X.
           12  EX-FORM                 PIC X(4).
           12  EX-TAB32                PIC X.
           12  EX-ACCT-NAME            PIC X(30).
           12  EX-TAB33                PIC X.
           12  EX-LAST-PAID-AMT        PIC -9(7).99.
           12  EX-TAB34                PIC X.
           12  EX-TERM                 PIC 999.
           12  EX-TAB35                PIC X.
           12  EX-BEN-CODE             PIC XX.
           12  EX-TAB36                PIC X.
           12  EX-INS-NAME             PIC X(30).
           12  EX-TAB37                PIC X.
           12  EX-INS-ADDR1            PIC X(30).
           12  EX-TAB38                PIC X.
           12  EX-INS-ADDR2            PIC X(30).
           12  EX-TAB39                PIC X.
           12  EX-INS-CITY-ST          PIC X(30).
           12  EX-TAB40                PIC X.
           12  EX-INS-ZIP              PIC X(9).
           12  EX-TAB41                PIC X.
           12  EX-INS-PHONE            PIC X(13).
           12  EX-TAB42                PIC X.
043012     12  EX-TOTAL-INTEREST-PD    PIC -9(5).99.
           12  EX-TAB43                PIC X.
062812     12  EX-NEXT-AUTO-PAY-DT     PIC X(10).
062812     12  EX-TAB44                PIC X.
040814     12  EX-ICD-CODE-1           PIC X(8).
040814     12  EX-TAB45                PIC X.
040814     12  EX-ICD-CODE-2           PIC X(8).
040814     12  EX-TAB46                PIC X.
071014     12  EX-LAST-CLOSE-DT        PIC X(10).
071014     12  EX-TAB47                PIC X.
071014     12  EX-LAST-REOPEN-DT       PIC X(10).
071014     12  EX-TAB48                PIC X.
071014     12  EX-NO-OF-DAYS-PAID      PIC ZZZ9.
071014     12  EX-TAB49                PIC X.
071014     12  EX-DENIAL-TYPE          PIC X(1).
071014     12  EX-TAB50                PIC X.
013017     12  EX-LOAN-NO              PIC X(25).
013017     12  EX-TAB51                PIC X.
081817     12  EX-CRITICAL-PERIOD      PIC 99.
081817     12  EX-TAB52                PIC X.
081817     12  EX-NO-OF-EXTENSIONS     PIC 99.
081817     12  EX-TAB53                PIC X.
100917     12  ex-claimant-code        pic x.
100917     12  ex-tab54                pic x.
020218     12  ex-clmnt-curr-age       pic 999.
020218     12  ex-tab55                pic x.
           12  EX-LAST-BYTE            PIC X.
092215******************************************************************
062217 01  SAVE-ELTRL-OUT-REC               PIC X(1575).
092215
092215 01  ELTRLR-OUT-REC.
092215     12  TR-CONTROL-PRIMARY.
092215         16  TR-COMPANY-CD               PIC X.
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CARRIER                  PIC X.                   1
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CLAIM-NO                 PIC X(7).                2
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CERT-NO.                                          3
092215             20  TR-CERT-PRIME           PIC X(10).
092215             20  TR-CERT-SFX             PIC X.
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-SEQUENCE-NO              PIC 9(4).                4
092215         16  FILLER                      PIC X VALUE X'A2'.
092215
092215     12  TR-TRAILER-TYPE                 PIC X.                   5
092215     12  FILLER                          PIC X VALUE X'A2'.
092215
092215     12  TR-RECORDED-DT                  PIC X(10).               6
092215     12  FILLER                          PIC X VALUE X'A2'.
092215     12  TR-RECORDED-BY                  PIC X(4).                7
092215     12  FILLER                          PIC X VALUE X'A2'.
092215     12  TR-LAST-MAINT-HHMMSS            PIC 9(6).                8
092215     12  FILLER                          PIC X VALUE X'A2'.
092215
092215     12  TR-RESERVE-EXPENSE-TR.
092215         16  TR-MANUAL-SW                PIC X.                   9
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-FUTURE-SW                PIC X.                   10
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PTC-SW                   PIC X.                   11
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-IBNR-SW                  PIC X.                   12
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PTC-LF-SW                PIC X.                   13
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CDT-ACCESS-METHOD        PIC X.                   14
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PERCENT-OF-CDT           PIC -9(3).99.            15
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-LAST-COMPUTED-DT         PIC X(10).               16
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-FUTURE-RESERVE           PIC -9(7).99.            17
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAY-CURRENT-RESERVE      PIC -9(7).99.            18
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-IBNR-RESERVE             PIC -9(7).99.            19
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-INITIAL-MANUAL-RESERVE   PIC -9(7).99.            20
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CURRENT-MANUAL-RESERVE   PIC -9(7).99.            21
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ITD-ADDITIONAL-RESERVE   PIC -9(7).99.            22
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-EXPENSE-METHOD           PIC X.                   23
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-EXPENSE-PERCENT          PIC -9(3).99.            24
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-EXPENSE-DOLLAR           PIC -9(3).99.            25
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ITD-PAID-EXPENSES        PIC -9(5).99.            26
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ITD-CHARGEABLE-EXPENSE   PIC -9(5).99.            27
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ITD-LIFE-REFUNDS         PIC -9(5).99.            28
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ITD-AH-REFUNDS           PIC -9(5).99.            29
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-RESERVES-LAST-MAINT-DT   PIC X(10).               30
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-RESERVES-LAST-UPDATED-BY PIC X(4).                31
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
092215             20  TR-OPEN-CLOSE-DATE      PIC X(10).               32
092215             20  FILLER                      PIC X VALUE X'A2'.
092215             20  TR-OPEN-CLOSE-TYPE      PIC X.                   33
092215             20  FILLER                      PIC X VALUE X'A2'.
092215             20  TR-OPEN-CLOSE-REASON    PIC X(5).                34
092215             20  FILLER                      PIC X VALUE X'A2'.
092215
092215     12  TR-PAYMENT-TR.
092215         16  TR-PAYMENT-TYPE             PIC X.                   50
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CLAIM-TYPE               PIC X.                   51
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CLAIM-PREM-TYPE          PIC X.                   52
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AMOUNT-PAID              PIC -9(7).99.            53
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CHECK-NO                 PIC X(7).                54
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAID-FROM-DT             PIC X(10).               55
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAID-THRU-DT             PIC X(10).               56
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DAYS-IN-PERIOD           PIC 9(4).                57
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAYEES-NAME              PIC X(30).               58
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAYMENT-ORIGIN           PIC X.                   59
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CHECK-WRITTEN-DT         PIC X(10).               60
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-TO-BE-WRITTEN-DT         PIC X(10).               61
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-VOID-DATA.
092215             20  TR-VOID-DT              PIC X(10).               62
092215             20  FILLER                  PIC X VALUE X'A2'.
092215             20  TR-VOID-REASON          PIC X(26).               63
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PMT-APPROVED-BY          PIC X(04).               64
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ADDL-RESERVE             PIC -9(5).99.            65
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-EXPENSE-PER-PMT          PIC -9(5).99.            66
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PMT-SELECT-DT            PIC X(10).               67
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PMT-ACCEPT-DT            PIC X(10).               68
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-VOID-SELECT-DT           PIC X(10).               69
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-VOID-ACCEPT-DT           PIC X(10).               70
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CHECK-QUE-CONTROL        PIC 9(8).                71
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CHECK-QUE-SEQUENCE       PIC 9(4).                72
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-FORCE-CONTROL            PIC X.                   73
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PREV-LAST-PMT-DT         PIC X(10).               74
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PREV-PAID-THRU-DT        PIC X(10).               75
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PREV-LAST-PMT-AMT        PIC -9(7).99.            76
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ELIMINATION-DAYS         PIC 999.                 77
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DAILY-RATE               PIC -9(3).99.            78
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-BENEFIT-TYPE             PIC X.                   79
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-EXPENSE-TYPE             PIC X.                   80
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAYMENT-APPROVAL-SW      PIC X.                   81
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAYEE-TYPE               PIC X.                   82
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAYEE-SEQ                PIC X.                   83
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CASH-PAYMENT             PIC X.                   84
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-GROUPED-PAYMENT          PIC X.                   85
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAYMENT-NOTE-SEQ-NO      PIC 9(4).                86
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-APPROVAL-LEVEL-REQD      PIC X.                   87
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-APPROVED-LEVEL           PIC X.                   88
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-VOID-TYPE                PIC X.                   89
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AIG-UNEMP-IND            PIC X.                   90
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ASSOCIATES               PIC X.                   91
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-FORM-CTL-SEQ-NO          PIC 9(4).                92
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CV-PMT-CODE              PIC X.                   93
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-EOB-CODE1                PIC XXX.                 94
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-EOB-CODE2                PIC XXX.                 95
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-EOB-CODE3                PIC XXX.                 96
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-INT-PMT-SELECT-DT        PIC X(10).               97
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PMT-PROOF-DT             PIC X(10).               98
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PRINT-EOB-WITH-CHECK     PIC X.                   99
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAYMENT-LAST-MAINT-DT    PIC X(10).               100
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PAYMENT-LAST-UPDATED-BY  PIC X(4).                101
092215         16  FILLER                      PIC X VALUE X'A2'.
092215
092215     12  TR-AUTO-PAY-TR.
092215         16  TR-SCHEDULE-START-DT        PIC X(10).               102
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-SCHEDULE-END-DT          PIC X(10).               103
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-TERMINATED-DT            PIC X(10).               104
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-LAST-PMT-TYPE            PIC X.                   105
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-FIRST-PMT-AMT            PIC -9(7).99.            106
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DAYS-IN-1ST-PMT          PIC 9(4).                107
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-1ST-PAY-THRU-DT          PIC X(10).               108
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-REGULAR-PMT-AMT          PIC -9(7).99.            109
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DAYS-IN-REG-PMT          PIC 9(4).                110
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-INTERVAL-MONTHS          PIC 9(4).                111
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AUTO-PAYEE-TYPE          PIC X.                   112
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AUTO-PAYEE-SEQ           PIC X.                   113
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AUTO-PAY-DAY             PIC 99.                  114
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AUTO-CASH                PIC X.                   115
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AUTO-END-LETTER          PIC X(4).                116
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AUTO-PAY-LAST-MAINT-DT   PIC X(10).               117
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AUTO-PAY-LAST-UPDATED-BY PIC X(4).                118
092215         16  FILLER                      PIC X VALUE X'A2'.
092215
092215     12  TR-CORRESPONDENCE-TR.
092215         16  TR-LETTER-SENT-DT           PIC X(10).               119
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-RECEIPT-FOLLOW-UP        PIC X(10).               120
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AUTO-RE-SEND-DT          PIC X(10).               121
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-LETTER-ANSWERED-DT       PIC X(10).               122
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-LETTER-ARCHIVE-NO        PIC 9(8).                123
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-LETTER-ORIGIN            PIC X.                   124
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-STD-LETTER-FORM          PIC X(4).                125
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-REASON-TEXT              PIC X(70).               126
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ADDRESS-REC-SEQ-NO       PIC 9(4).                127
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ADDRESEE-TYPE            PIC X.                   128
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-ADDRESSEE-NAME           PIC X(30).               129
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-INITIAL-PRINT-DATE       PIC X(10).               130
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-RESEND-PRINT-DATE        PIC X(10).               131
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CORR-SOL-UNSOL           PIC X.                   132
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-LETTER-PURGED-DT         PIC X(10).               133
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-RESEND-LETTER-FORM       PIC X(4).                134
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-AUTO-CLOSE-IND           PIC X(1).                135
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-LETTER-TO-BENE           PIC X(1).                136
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-STOP-LETTER-DT           PIC X(10).               137
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CSO-LETTER-STATUS        PIC X.                   138
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CSO-LETTER-PURGE-DATE    PIC X(10).               139
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CSO-LETTER-RELOAD-DATE   PIC X(10).               140
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CORR-LAST-MAINT-DT       PIC X(10).               141
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CORR-LAST-UPDATED-BY     PIC X(4).                142
092215         16  FILLER                      PIC X VALUE X'A2'.
092215
092215*    12  TR-ADDRESS-TR.
092215*        16  TR-ADDRESS-TYPE             PIC X.
092215*        16  TR-MAIL-TO-NAME             PIC X(30).
092215*        16  TR-ADDRESS-LINE-1           PIC X(30).
092215*        16  TR-ADDRESS-LINE-2           PIC X(30).
092215*        16  TR-CITY-STATE.
092215*            20  TR-CITY                 PIC X(28).
092215*            20  TR-STATE                PIC XX.
092215*        16  TR-ZIP.                     PIC X(9).
092215*        16  TR-PHONE-NO                 PIC 9(11).
092215*        16  TR-VFY-2ND-BENE-SSN         PIC X(9).
092215*        16  TR-VFY-2ND-BENE-VERIFIED    PIC X.
092215*        16  TR-ADDRESS-LAST-MAINT-DT    PIC X(10).
092215*        16  TR-ADDRESS-LAST-UPDATED-BY  PIC X(4).
092215
092215     12  TR-GENERAL-INFO-TR.
092215         16  TR-INFO-LINE-1              PIC X(60).               143
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-INFO-LINE-2              PIC X(60).               144
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-INFO-TRAILER-TYPE        PIC X.                   145
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-CALL-TYPE                PIC X.                   146
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-NOTE-CONTINUATION        PIC X.                   147
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-EOB-CODES-EXIST          PIC X.                   148
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-GEN-INFO-LAST-MAINT-DT   PIC X(10).               149
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-GEN-INFO-LAST-UPDATED-BY PIC X(4).                150
092215         16  FILLER                      PIC X VALUE X'A2'.
092215
092215     12  TR-AUTO-PROMPT-TR.
092215         16  TR-PROMPT-LINE-1            PIC X(60).               151
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PROMPT-LINE-2            PIC X(60).               152
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PROMPT-START-DT          PIC X(10).               153
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PROMPT-END-DT            PIC X(10).               154
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PROMPT-LAST-MAINT-DT     PIC X(10).               155
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-PROMPT-LAST-UPDATED-BY   PIC X(4).                156
092215         16  FILLER                      PIC X VALUE X'A2'.
092215
092215     12  TR-DENIAL-INFO-TR.
092215         16  TR-DENIAL-INFO-1            PIC X(60).               157
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DENIAL-INFO-2            PIC X(60).               158
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DENIAL-DT                PIC X(10).               159
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-RETRACTION-DT            PIC X(10).               160
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DENIAL-REASON-CODE       PIC X(4).                161
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DENIAL-PROOF-DT          PIC X(10).               162
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DENIAL-LAST-MAINT-DT     PIC X(10).               163
092215         16  FILLER                      PIC X VALUE X'A2'.
092215         16  TR-DENIAL-LAST-UPDATED-BY   PIC X(4).                164
052217         16  FILLER                      PIC X VALUE X'A2'.
052217     12  TR-PAYMENT-TR2.
052217         16  TR-ACH-PAYMENT              PIC X.                   165
062217         16  FILLER                      PIC X VALUE X'A2'.
062217     12  TR-CORRESPONDENCE-TR2.
062217         16  TR-AUTH-RCVD                PIC X(1).                166
092215         16  FILLER                      PIC X VALUE  '*'.
092215
092215*    12  TR-INCURRED-CHG-TR.
092215*        16  TR-OLD-INCURRED-DT          PIC X(10).
092215*        16  TR-OLD-REPORTED-DT          PIC X(10).
092215*        16  TR-OLD-ESTABLISHED-DT       PIC X(10).
092215*        16  TR-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
092215*        16  TR-OLD-DAYS-PAID            PIC S9(4)        COMP.
092215*        16  TR-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
092215*        16  TR-OLD-PAID-THRU-DT         PIC X(10).
092215*        16  TR-LAST-PMT-MADE-DT         PIC X(10).
092215*        16  TR-OLD-DIAG-CODE            PIC X(6).
092215*        16  TR-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
092215*        16  TR-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
092215*        16  TR-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
092215*        16  TR-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
092215*        16  TR-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
092215*        16  TR-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
092215*        16  TR-OLD-DIAG-DESCRIP         PIC X(60).
092215*        16  TR-OLD-ICD-CODE-1           PIC X(8).
092215*        16  TR-OLD-ICD-CODE-2           PIC X(8).
092215*        16  TR-INCURRED-LAST-UPDATED-BY PIC X(4).
092215
092215*    12  TR-FORM-CONTROL-TR  REDEFINES  TR-TRAILER-BODY.
092215*        16  TR-FORM-SEND-ON-DT          PIC X(10).
092215*        16  TR-FORM-FOLLOW-UP-DT        PIC X(10).
092215*        16  TR-FORM-RE-SEND-DT          PIC X(10).
092215*        16  TR-FORM-ANSWERED-DT         PIC X(10).
092215*        16  TR-FORM-PRINTED-DT          PIC X(10).
092215*        16  TR-FORM-REPRINT-DT          PIC X(10).
092215*        16  TR-FORM-TYPE                PIC X.
092215*            88  INITIAL-FORM                  VALUE '1'.
092215*            88  PROGRESS-FORM                 VALUE '2'.
092215*        16  TR-INSTRUCT-LN-1            PIC X(28).
092215*        16  TR-INSTRUCT-LN-2            PIC X(28).
092215*        16  TR-INSTRUCT-LN-3            PIC X(28).
092215*        16  TR-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
092215*        16  TR-FORM-ADDRESS             PIC X.
092215*            88  FORM-TO-INSURED              VALUE 'I'.
092215*            88  FORM-TO-ACCOUNT              VALUE 'A'.
092215*            88  FORM-TO-OTHER-1              VALUE 'O'.
092215*            88  FORM-TO-OTHER-2              VALUE 'Q'.
092215*        16  TR-RELATED-1.
092215*            20 TR-REL-CARR-1            PIC X.
092215*            20 TR-REL-CLAIM-1           PIC X(7).
092215*            20 TR-REL-CERT-1            PIC X(11).
092215*        16  TR-RELATED-2.
092215*            20 TR-REL-CARR-2            PIC X.
092215*            20 TR-REL-CLAIM-2           PIC X(7).
092215*            20 TR-REL-CERT-2            PIC X(11).
092215*        16  TR-EMP-FORM-SEND-ON-DT      PIC X(10).
092215*        16  TR-PHY-FORM-SEND-ON-DT      PIC X(10).
092215*        16  TR-EMP-FORM-ANSWERED-DT     PIC X(10).
092215*        16  TR-PHY-FORM-ANSWERED-DT     PIC X(10).
092215*        16  TR-FORM-REM-PRINT-DT        PIC X(10).
092215*        16  TR-STOP-FORM-DT             PIC X(2).
092215*        16  TR-FORM-LAST-MAINT-DT       PIC X(10).
092215*        16  TR-FORM-LAST-UPDATED-BY     PIC X(4).
092215
092215 01  ELTRLR-HEADER-REC.
092215     12  FILLER    PIC X(13) VALUE  'TR_COMPANY_CD'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_CARRIER'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_CLAIM_NO'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_CERT_NO'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(14) VALUE  'TR_SEQUENCE_NO'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_TRAILER_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(14) VALUE  'TR_RECORDED_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(14) VALUE  'TR_RECORDED_BY'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_LAST_MAINT_HHMMSS'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_MANUAL_SW'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_FUTURE_SW'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_PTC_SW'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(10) VALUE  'TR_IBNR_SW'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(13) VALUE  'TR_PTC_LF_SW'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_CDT_ACCESS_METHOD'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_PERCENT_OF_CDT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(19) VALUE  'TR_LAST_COMPUTED_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_FUTURE_RESERVE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(24) VALUE  'TR_PAY_CURRENT_RESERVE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_IBNR_RESERVE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_INITIAL_MANUAL_RESERVE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_CURRENT_MANUAL_RESERVE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_ITD_ADDITIONAL_RESERVE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_EXPENSE_METHOD'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_EXPENSE_PERCENT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_EXPENSE_DOLLAR'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_ITD_PAID_EXPENSES'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_ITD_CHARGEABLE_EXPENSE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(19) VALUE  'TR_ITD_LIFE_REFUNDS'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_ITD_AH_REFUNDS'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_RESERVES_LAST_MAINT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_RESERVES_LAST_UPDATED_BY'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_DATE_1'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_TYPE_1'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(22) VALUE  'TR_OPEN_CLOSE_REASON_1'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_DATE_2'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_TYPE_2'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(22) VALUE  'TR_OPEN_CLOSE_REASON_2'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_DATE_3'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_TYPE_3'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_OPEN_CLOSE_REASON_3'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_DATE_4'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_TYPE_4'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(22) VALUE  'TR_OPEN_CLOSE_REASON_4'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_DATE_5'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_TYPE_5'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(22) VALUE  'TR_OPEN_CLOSE_REASON_5'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_DATE_6'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_OPEN_CLOSE_TYPE_6'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(22) VALUE  'TR_OPEN_CLOSE_REASON_6'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_PAYMENT_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(13) VALUE  'TR_CLAIM_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_CLAIM_PREM_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(14) VALUE  'TR_AMOUNT_PAID'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_CHECK_NO'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_PAID_FROM_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_PAID_THRU_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_DAYS_IN_PERIOD'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_PAYEES_NAME'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_PAYMENT_ORIGIN'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(19) VALUE  'TR_CHECK_WRITTEN_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(19) VALUE  'TR_TO_BE_WRITTEN_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(10) VALUE  'TR_VOID_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(14) VALUE  'TR_VOID_REASON'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_PMT_APPROVED_BY'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_ADDL_RESERVE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_EXPENSE_PER_PMT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_PMT_SELECT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_PMT_ACCEPT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_VOID_SELECT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_VOID_ACCEPT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_CHECK_QUE_CONTROL'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(21) VALUE  'TR_CHECK_QUE_SEQUENCE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_FORCE_CONTROL'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(19) VALUE  'TR_PREV_LAST_PMT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_PREV_PAID_THRU_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_PREV_LAST_PMT_AMT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(19) VALUE  'TR_ELIMINATION_DAYS'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(13) VALUE  'TR_DAILY_RATE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_BENEFIT_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_EXPENSE_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(23) VALUE  'TR_PAYMENT_APPROVAL_SW'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(13) VALUE  'TR_PAYEE_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_PAYEE_SEQ'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_CASH_PAYMENT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_GROUPED_PAYMENT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(22) VALUE  'TR_PAYMENT_NOTE_SEQ_NO'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(22) VALUE  'TR_APPROVAL_LEVEL_REQD'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_APPROVED_LEVEL'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_VOID_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_AIG_UNEMP_IND'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(13) VALUE  'TR_ASSOCIATES'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_FORM_CTL_SEQ_NO'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(14) VALUE  'TR_CV_PMT_CODE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_EOB_CODE1'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_EOB_CODE2'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_EOB_CODE3'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_EOB_CODE4'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_PMT_PROOF_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(25) VALUE  'TR_PRINT_EOB_WITH_CHECK'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(26) VALUE  'TR_PAYMENT_LAST_MAINT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_PAYMENT_LAST_UPDATED_BY'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_SCHEDULE_START_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_SCHEDULE_END_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_TERMINATED_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_LAST_PMT_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_FIRST_PMT_AMT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_DAYS_IN_1ST_PMT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_1ST_PAY_THRU_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_REGULAR_PMT_AMT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_DAYS_IN_REG_PMT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_INTERVAL_MONTHS'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_AUTO_PAYEE_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_AUTO_PAYEE_SEQ'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(15) VALUE  'TR_AUTO_PAY_DAY'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(13) VALUE  'TR_AUTO_CASH'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_AUTO_END_LETTER'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_AUTO_PAY_LAST_MAINT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_AUTO_PAY_LAST_UPDATED_BY'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(17) VALUE  'TR_LETTER_SENT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_RECEIPT_FOLLOW_UP'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(18) VALUE  'TR_AUTO_RE_SEND_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(21) VALUE  'TR_LETTER_ANSWERED_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(20) VALUE  'TR_LETTER_ARCHIVE_NO'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_LETTER_ORIGIN'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_STD_LETTER_FORM'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_REASON_TEXT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_ADDRESS_REC_SEQ_NO'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_ADDRESEE_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_ADDRESSEE_NAME'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_INITIAL_PRINT_DATE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_RESEND_PRINT_DATE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_CORR_SOL_UNSOL'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_LETTER_PURGED_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_RESEND_LETTER_FORM'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_AUTO_CLOSE_IND'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_LETTER_TO_BENE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_STOP_LETTER_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_CSO_LETTER_STATUS'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_CSO_LETTER_PURGE_DATE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_CSO_LETTER_RELOAD_DATE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_CORR_LAST_MAINT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_CORR_LAST_UPDATED_BY'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(14) VALUE  'TR_INFO_LINE_1'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(14) VALUE  'TR_INFO_LINE_2'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_INFO_TRAILER_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(12) VALUE  'TR_CALL_TYPE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_NOTE_CONTINUATION'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_EOB_CODES_EXIST'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_GEN_INFO_LAST_MAINT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_GEN_INFO_LAST_UPDATED_BY'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_PROMPT_LINE_1'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_PROMPT_LINE_2'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_PROMPT_START_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_PROMPT_END_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_PROMPT_LAST_MAINT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_PROMPT_LAST_UPDATED_BY'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_DENIAL_INFO_1'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_DENIAL_INFO_2'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(14) VALUE  'TR_DENIAL_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(16) VALUE  'TR_RETRACTION_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_DENIAL_REASON_CODE'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_DENIAL_PROOF_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_DENIAL_LAST_MAINT_DT'.
092215     12  FILLER                      PIC X VALUE ';'.
092215     12  FILLER    PIC X(27) VALUE  'TR_DENIAL_LAST_UPDATED_BY'.
092215     12  FILLER                      PIC X VALUE '*'.

      ******************************************************************
       01  WS-MISC.
           05  ERACCT-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELMSTR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELTRLR-FILE-STATUS      PIC XX     VALUE ZEROS.
042320     05  ELTRLRN-FILE-STATUS     PIC XX     VALUE ZEROS.
           05  ELCERT-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.
021704     05  PGM-SUB          COMP-3 PIC S9(04) VALUE +585.
021704     05  WS-RETURN-CODE   COMP   PIC S9(03) VALUE +0.
021704     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
021704     05  WS-ZERO          COMP-3 PIC S9(01) VALUE +0.
021704     05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

021704 0000-DATE-CARD-READ. COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-ELMSTR THRU 0100-EXIT UNTIL
                 (END-OF-ELMSTR)
PEMTST*          OR (CLM-RECS-IN > 1)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CLAIM RECORDS READ    '  CLM-RECS-IN
           DISPLAY ' TRLR  RECORDS READ    '  TRL-RECS-IN
           DISPLAY ' CLAIM RECORDS WRITTEN '  CLM-RECS-OUT
092215     DISPLAY ' TRAILER RECORDS WRITTEN '  TRL-RECS-OUT
           GOBACK

           .
       0100-PROCESS-ELMSTR.

           MOVE WS-SAVE-ELMSTR         TO ELMSTR-DETAIL-RECORD

           MOVE CL-CARRIER             TO EX-CARRIER
           MOVE CL-CLAIM-NO            TO EX-CLAIM-NO
           MOVE CL-CERT-NO             TO EX-CERT-NO
           MOVE CL-CERT-ACCOUNT        TO EX-ACCOUNT
           MOVE CL-INSURED-LAST-NAME   TO EX-INSURED-LAST-NAME
           MOVE CL-INSURED-1ST-NAME    TO EX-INSURED-1ST-NAME
           MOVE CL-CLAIM-STATUS        TO EX-CLAIM-STATUS
           MOVE CL-CLAIM-TYPE          TO EX-CLAIM-TYPE
      *    MOVE CL-FILE-ESTABLISH-DT   TO EX-FILE-ESTABLISH-DT
           MOVE CL-TOTAL-PAID-AMT      TO EX-TOTAL-PAID-AMT
           MOVE CL-NO-OF-PMTS-MADE     TO EX-NO-OF-PMTS
071014     MOVE CL-NO-OF-DAYS-PAID     TO EX-NO-OF-DAYS-PAID
           MOVE CL-LAST-PMT-AMT        TO EX-LAST-PAID-AMT
           MOVE CL-INSURED-SEX-CD      TO EX-INSURED-SEX-CD
           MOVE CL-ACCOUNT-ADDR-CNT    TO EX-ACCOUNT-ADDR-CNT
           MOVE CL-CERT-STATE          TO EX-CERT-STATE
           MOVE CL-CERT-ACCOUNT        TO EX-CERT-ACCOUNT
           MOVE CL-LAST-MAINT-USER     TO EX-LAST-MAINT-USER
           MOVE CL-LAST-MAINT-TYPE     TO EX-LAST-MAINT-TYPE
           MOVE CL-LAST-CLOSE-REASON   TO EX-LAST-CLOSE-REASON
071014     MOVE CL-LAST-CLOSE-DT       TO DC-BIN-DATE-1
071014     MOVE ' '                    TO DC-OPTION-CODE
071014     PERFORM 8510-DATE-CONVERSION
071014                                 THRU 8590-EXIT
071014     IF NO-CONVERSION-ERROR
071014        MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-CLOSE-DT
071014     END-IF
071014     MOVE CL-LAST-REOPEN-DT      TO DC-BIN-DATE-1
071014     MOVE ' '                    TO DC-OPTION-CODE
071014     PERFORM 8510-DATE-CONVERSION
071014                                 THRU 8590-EXIT
071014     IF NO-CONVERSION-ERROR
071014        MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-REOPEN-DT
071014     END-IF
071014
071014     INSPECT CL-DENIAL-TYPE
071014        REPLACING ALL X'00' BY SPACES
071014
071014     MOVE CL-DENIAL-TYPE         TO EX-DENIAL-TYPE

           MOVE CL-FILE-ESTABLISH-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-FILE-ESTABLISH-DT
           END-IF

           MOVE CL-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CERT-EFF-DT
           END-IF

           MOVE CL-PAID-THRU-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-PAID-THRU-DT
           END-IF

           MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-PMT-DT
           END-IF

           MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-INCURRED-DT
           END-IF

           MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-REPORTED-DT
           END-IF

           MOVE CL-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
           END-IF

092204     MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1
092204     MOVE ' '                    TO DC-OPTION-CODE
092204     PERFORM 8510-DATE-CONVERSION
092204                                 THRU 8590-EXIT
092204     IF NO-CONVERSION-ERROR
092204        MOVE DC-GREG-DATE-A-EDIT TO EX-BIRTH-DT
092204     END-IF

020218     if cl-insured-birth-dt <> low-values and spaces
020218        move cl-insured-birth-dt to dc-bin-date-1
020218        move ws-current-bin-dt   to dc-bin-date-2
020218        move '1'                 to dc-option-code
020218        perform 8510-date-conversion
020218                                 thru 8590-exit
020218        if no-conversion-error
020218*          display ' did dob ' cl-claim-no ' ' cl-cert-no ' '
020218*             dc-elapsed-months
020218           compute ws-work-age = dc-elapsed-months / 12
020218           move ws-work-age      to ex-clmnt-curr-age
020218           go to 0100-continue
020218        else
020218           display ' error birth dt ' cl-claim-no ' ' cl-cert-no
020218        end-if
020218     end-if
020218
020218     if elcert-file-status <> '00'
020218        move 42                  to ex-clmnt-curr-age
020218        go to 0100-continue
020218     end-if
020218
020218     if cl-insured-type = 'C'
020218        if (cm-insured-joint-age not numeric)
020218           or (cm-insured-joint-age = zeros)
020218           move 42               to cm-insured-joint-age
020218        end-if
020218        move cm-insured-joint-age to ws-clmnt-age
020218     else
020218        if (cm-insured-issue-age not numeric)
020218           or (cm-insured-issue-age = zeros)
020218           move 42               to cm-insured-issue-age
020218        end-if
020218        move cm-insured-issue-age to ws-clmnt-age
020218     end-if
020218
020218     move cm-cert-eff-dt         to dc-bin-date-1
020218     move ws-current-bin-dt      to dc-bin-date-2
020218     move '1'                    to dc-option-code
020218     perform 8510-date-conversion
020218                                 thru 8590-exit
020218     if no-conversion-error
020218*       display ' did age ' ws-clmnt-age ' ' dc-elapsed-months
020218        compute ws-work-age = ws-clmnt-age +
020218           (dc-elapsed-months / 12)
020218        move ws-work-age         to ex-clmnt-curr-age
020218     end-if
020218
020218     .
020218 0100-continue.

043012     IF CL-TOTAL-INT-PAID NOT NUMERIC
043012        MOVE ZEROS               TO CL-TOTAL-INT-PAID
043012     END-IF
043012     MOVE CL-TOTAL-INT-PAID      TO EX-TOTAL-INTEREST-PD

062812     if cl-next-auto-pay-dt = spaces or low-values
062812        continue
062812     else
062812        move cl-next-auto-pay-dt to dc-bin-date-1
062812        move ' '                 to dc-option-code
062812        perform 8510-date-conversion
062812                                 thru 8590-exit
062812        if no-conversion-error
062812           move dc-greg-date-a-edit
062812                                 to ex-next-auto-pay-dt
062812        end-if
062812     end-if

           INSPECT WS-DIAGNOSIS
              REPLACING ALL X'00' BY SPACES

           MOVE WS-DIAGNOSIS           TO EX-DIAG

013017     INSPECT WS-loan-no
013017        REPLACING ALL X'00' BY SPACES
013017
013017     MOVE WS-loan-no             TO EX-loan-no
040814
040814     INSPECT WS-ICD-CODE-1
040814        REPLACING ALL X'00' BY SPACES
040814
040814     MOVE WS-ICD-CODE-1          TO EX-ICD-CODE-1
040814
040814     INSPECT WS-ICD-CODE-2
040814        REPLACING ALL X'00' BY SPACES
040814
040814     MOVE WS-ICD-CODE-2          TO EX-ICD-CODE-2

           MOVE WS-LAST-ACT-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-ACT-DT
           END-IF

           MOVE WS-ACTIVITY-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-ACTIVITY-DT
           END-IF

           MOVE WS-LAST-ACT-TYPE       TO EX-LAST-ACT-TYPE
           MOVE WS-ACTIVITY-TYPE       TO EX-ACTIVITY-TYPE
           IF WS-FORM = LOW-VALUES
              MOVE SPACES              TO WS-FORM
           END-IF
           MOVE WS-FORM                TO EX-FORM
           MOVE WS-AM-NAME             TO EX-ACCT-NAME

           IF WS-INS-NAME-AND-ADDRESS NOT = SPACES
              MOVE WS-INS-NAME         TO EX-INS-NAME
              MOVE WS-INS-ADDR1        TO EX-INS-ADDR1
              MOVE WS-INS-ADDR2        TO EX-INS-ADDR2
              MOVE WS-INS-CITY-ST      TO EX-INS-CITY-ST
              MOVE WS-INS-ZIP          TO EX-INS-ZIP
              MOVE WS-INS-PHONE        TO EX-INS-PHONE
           END-IF

      *    IF ELTRLR-FILE-STATUS = '00'
      *       INSPECT AT-INFO-LINE-1
      *          REPLACING ALL X'00' BY SPACES
      *       MOVE AT-INFO-LINE-1      TO EX-DIAG
      *    ELSE
      *       MOVE SPACES              TO EX-DIAG
      *    END-IF

           IF ELCERT-FILE-STATUS = '00'
121018        IF CL-CLAIM-TYPE = 'L' or 'O'
                 MOVE CM-LF-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
                 MOVE CM-LF-ORIG-TERM  TO EX-TERM
                 MOVE CM-LF-BENEFIT-CD TO EX-BEN-CODE
              ELSE
                 MOVE CM-AH-LOAN-EXPIRE-DT 
                                       TO DC-BIN-DATE-1
                 MOVE CM-AH-ORIG-TERM  TO EX-TERM
                 MOVE CM-AH-BENEFIT-CD TO EX-BEN-CODE
              END-IF
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-EXP-DT
              END-IF
           END-IF

081817     IF CL-CRITICAL-PERIOD IS NUMERIC
081817        MOVE CL-CRITICAL-PERIOD  TO EX-CRITICAL-PERIOD
081817     ELSE
081817        MOVE ZERO  TO EX-CRITICAL-PERIOD
081817     END-IF
081817     IF CL-NO-OF-EXTENSIONS IS NUMERIC
081817        MOVE CL-NO-OF-EXTENSIONS TO EX-NO-OF-EXTENSIONS
081817     ELSE
081817        MOVE ZERO TO EX-NO-OF-EXTENSIONS
081817     END-IF

100917     if cl-insured-type = 'P' or 'C'
100917        move cl-insured-type     to ex-claimant-code
100917     else
100917        move spaces              to ex-claimant-code
100917     end-if

           PERFORM 0300-WRITE-MSTR     THRU 0300-EXIT
           PERFORM 0200-READ-ELMSTR    THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ELMSTR.

           READ ELMSTR NEXT RECORD

           IF (ELMSTR-FILE-STATUS = '10' OR '23')
              OR (CL-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ELMSTR        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR READ NEXT ' ELMSTR-FILE-STATUS
                 SET END-OF-ELMSTR     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELMSTR
              ADD 1 TO CLM-RECS-IN
              PERFORM 0210-GET-ELTRLR  THRU 0210-EXIT
      *       PERFORM 0250-READ-ELTRLR THRU 0250-EXIT
              PERFORM 0275-READ-ELCERT THRU 0275-EXIT
           END-IF

           .
       0200-EXIT.
           EXIT.

       0210-GET-ELTRLR.

           MOVE SPACES                 TO WS-DIAGNOSIS
040814                                    WS-ICD-CODE-1
040814                                    WS-ICD-CODE-2
                                          WS-LAST-ACT-TYPE
                                          WS-ACTIVITY-TYPE
                                          WS-FORM
                                          WS-GOT-IT-SW
013017                                    ws-loan-no
           MOVE LOW-VALUES             TO WS-LAST-ACT-DT
                                          WS-ACTIVITY-DT

           PERFORM 0215-GET-INS-ADDR-TRLR
                                       THRU 0215-EXIT

           PERFORM 0220-START-ELTRLR   THRU 0220-EXIT
           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              PERFORM 0230-READ-ELTRLR THRU 0230-EXIT UNTIL
092215          (CL-CONTROL-PRIMARY NOT = AT-CONTROL-PRIMARY (1:20))
092215           OR (ELTRLR-FILE-STATUS  NOT =  '00')
092215*          WE-GOT-IT-ALL
           END-IF

           .
       0210-EXIT.
           EXIT.

       0215-GET-INS-ADDR-TRLR.

           MOVE SPACES                 TO WS-INS-NAME-AND-ADDRESS
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +1                     TO AT-SEQUENCE-NO

           READ ELTRLR

           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              IF AT-SEQUENCE-NO = +1
                 MOVE AT-MAIL-TO-NAME  TO WS-INS-NAME
                 MOVE AT-ADDRESS-LINE-1
                                       TO WS-INS-ADDR1
                 MOVE AT-ADDRESS-LINE-2
                                       TO WS-INS-ADDR2
082710           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
082710              INTO WS-INS-CITY-ST
082710           END-STRING
082710*          MOVE AT-CITY-STATE    TO WS-INS-CITY-ST
                 MOVE AT-ZIP           TO WS-INS-ZIP
                 IF AT-PHONE-NO NOT = ZEROS
                    MOVE AT-PHONE-NO   TO WS-WORK-PHONE
                    STRING '(' WS-WORK-PHONE (2:3) ')'
                       WS-WORK-PHONE (5:3) '-' WS-WORK-PHONE (8:4)
                       DELIMITED BY SIZE INTO WS-INS-PHONE
                    END-STRING
                 END-IF
              END-IF
           END-IF

           .
       0215-EXIT.
           EXIT.

       0220-START-ELTRLR.

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +90                    TO AT-SEQUENCE-NO
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
092215     IF ELTRLR-FILE-STATUS = '10' OR '23'
092215        CONTINUE
092215     ELSE
092215        IF ELTRLR-FILE-STATUS NOT = '00'
092215           DISPLAY 'ELTRLR READ      ' ELTRLR-FILE-STATUS
092215        ELSE
092215           ADD 1 TO TRL-RECS-IN
092215        END-IF
092215     END-IF



           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
092215        PERFORM 0240-WRITE-ELTRLR THRU 0240-EXIT
092215        IF WE-GOT-IT-ALL
092215           GO TO 0230-EXIT
092215        END-IF
              EVALUATE TRUE
                 when at-sequence-no = +90
                    MOVE AT-INFO-LINE-1   TO WS-DIAGNOSIS
040814              MOVE AT-ICD-CODE-1    TO WS-ICD-CODE-1
040814              MOVE AT-ICD-CODE-2    TO WS-ICD-CODE-2
013017           when at-sequence-no = +91
013017              MOVE AT-INFO-LINE-1   TO WS-loan-no
                 WHEN AT-TRAILER-TYPE = '2'
                    IF AT-CHECK-WRITTEN-DT > WS-LAST-ACT-DT
                       MOVE 'PAYMT' TO WS-LAST-ACT-TYPE
                       MOVE AT-CHECK-WRITTEN-DT
                                    TO WS-LAST-ACT-DT
                    END-IF
                    IF AT-PAID-THRU-DT > WS-ACTIVITY-DT
                       MOVE AT-PAID-THRU-DT
                                    TO WS-ACTIVITY-DT
                       IF FINAL-PAYMENT
                          MOVE '*FINAL*'
                                    TO WS-ACTIVITY-TYPE
                       END-IF
                       IF PARTIAL-PAYMENT
                          MOVE 'PARTIAL'
                                    TO WS-ACTIVITY-TYPE
                       END-IF
                       IF LUMP-SUM-PAYMENT
                          MOVE 'LUMP-S*'
                                    TO WS-ACTIVITY-TYPE
                       END-IF
                       IF (ONLINE-AUTO-PMT)
                          AND (PARTIAL-PAYMENT)
                          MOVE 'AUTO-P'
                                    TO WS-ACTIVITY-TYPE
                       END-IF
                       IF (ONLINE-AUTO-PMT)
                          AND (FINAL-PAYMENT)
                          MOVE 'AUTO-F'
                                    TO WS-ACTIVITY-TYPE
                       END-IF
                       IF ADDITIONAL-PAYMENT
                          MOVE 'ADDITIONAL'
                                    TO WS-ACTIVITY-TYPE
                       END-IF
                       IF CHARGEABLE-EXPENSE
                          MOVE 'EXPENSE'
                                    TO WS-ACTIVITY-TYPE
                       END-IF
                    END-IF
                 WHEN AT-TRAILER-TYPE = '4'
                    IF AT-RECORDED-DT > WS-LAST-ACT-DT
                       MOVE AT-RECORDED-DT
                                    TO WS-LAST-ACT-DT
                       MOVE 'LETTER'
                                    TO WS-LAST-ACT-TYPE
                       MOVE AT-STD-LETTER-FORM
                                    TO WS-FORM
                    END-IF
                    IF AT-LETTER-SENT-DT > WS-ACTIVITY-DT
                       MOVE AT-LETTER-SENT-DT
                                    TO WS-ACTIVITY-DT
                       MOVE 'SENT'  TO WS-ACTIVITY-TYPE
                    END-IF
                    IF AT-RESEND-PRINT-DATE > WS-ACTIVITY-DT
                       MOVE AT-RESEND-PRINT-DATE
                                    TO WS-ACTIVITY-DT
                       MOVE 'RESENT'
                                    TO WS-ACTIVITY-TYPE
                    END-IF
                    IF AT-LETTER-ANSWERED-DT > WS-ACTIVITY-DT
                       MOVE AT-LETTER-ANSWERED-DT
                                    TO WS-ACTIVITY-DT
                       MOVE 'RECVD' TO WS-ACTIVITY-TYPE
                    END-IF
                 WHEN AT-TRAILER-TYPE = '8'
                    IF AT-RECORDED-DT > WS-LAST-ACT-DT
                       MOVE AT-RECORDED-DT
                                    TO WS-LAST-ACT-DT
                       MOVE 'DENIAL'
                                    TO WS-LAST-ACT-TYPE
                    END-IF
                 WHEN AT-TRAILER-TYPE = 'A'
                    IF AT-RECORDED-DT > WS-LAST-ACT-DT
                       MOVE AT-RECORDED-DT
                                    TO WS-LAST-ACT-DT
                       MOVE 'FORMS' TO WS-LAST-ACT-TYPE
                       IF INITIAL-FORM
                          MOVE 'INIT'
                                    TO WS-FORM
                       END-IF
                       IF PROGRESS-FORM
                          MOVE 'PROG'
                                    TO WS-FORM
                       END-IF
                    END-IF
                    IF AT-FORM-SEND-ON-DT > WS-ACTIVITY-DT
                       MOVE AT-FORM-SEND-ON-DT
                                    TO WS-ACTIVITY-DT
                    END-IF
                    IF AT-FORM-REPRINT-DT > WS-ACTIVITY-DT
                       MOVE AT-FORM-REPRINT-DT
                                    TO WS-ACTIVITY-DT
                    END-IF
                    IF AT-FORM-ANSWERED-DT > WS-ACTIVITY-DT
                       MOVE AT-FORM-ANSWERED-DT
                                    TO WS-ACTIVITY-DT
                    END-IF
              END-EVALUATE
           ELSE
              SET WE-GOT-IT-ALL        TO TRUE
           END-IF

           IF WS-LAST-ACT-DT NOT = LOW-VALUES
              SET WE-GOT-IT-ALL        TO TRUE
           END-IF

           .
       0230-EXIT.
           EXIT.

092215 0240-WRITE-ELTRLR.
092215     INITIALIZE ELTRLR-OUT-REC
092215
092215     MOVE AT-COMPANY-CD    TO   TR-COMPANY-CD
092215     MOVE AT-CARRIER       TO   TR-CARRIER
092215     MOVE AT-CLAIM-NO      TO   TR-CLAIM-NO
092215     MOVE AT-CERT-NO       TO   TR-CERT-NO
092215     MOVE AT-SEQUENCE-NO        TO  TR-SEQUENCE-NO
092215     MOVE AT-TRAILER-TYPE       TO  TR-TRAILER-TYPE
092215

050420     if at-recorded-dt = low-values
050420        move cl-file-establish-dt
050420                                TO AT-RECORDED-DT
050420        display ' setting rec dt ' at-claim-no ' '
050420           at-cert-no
050420     end-if
092215     MOVE AT-RECORDED-DT        TO DC-BIN-DATE-1
020218     move ' ' to dc-option-code
092215     PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215     IF NO-CONVERSION-ERROR
092215        MOVE DC-GREG-DATE-A-EDIT TO TR-RECORDED-DT
092215     END-IF
092215
092215     MOVE AT-RECORDED-BY        TO  TR-RECORDED-BY
092215     MOVE AT-LAST-MAINT-HHMMSS  TO  TR-LAST-MAINT-HHMMSS.
092215
092215     EVALUATE TRUE
092215       WHEN RESERVE-EXPENSE-TR
092215         MOVE AT-MANUAL-SW           TO TR-MANUAL-SW
092215         MOVE AT-FUTURE-SW           TO TR-FUTURE-SW
092215         MOVE AT-PTC-SW              TO TR-PTC-SW
092215         MOVE AT-IBNR-SW             TO TR-IBNR-SW
092215         MOVE AT-PTC-LF-SW           TO TR-PTC-LF-SW
092215         MOVE AT-CDT-ACCESS-METHOD   TO TR-CDT-ACCESS-METHOD
092215         MOVE AT-PERCENT-OF-CDT      TO TR-PERCENT-OF-CDT
092215         MOVE AT-LAST-COMPUTED-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-LAST-COMPUTED-DT
092215         END-IF
092215
092215         MOVE AT-FUTURE-RESERVE      TO TR-FUTURE-RESERVE
092215         MOVE AT-PAY-CURRENT-RESERVE TO TR-PAY-CURRENT-RESERVE
092215         MOVE AT-IBNR-RESERVE        TO TR-IBNR-RESERVE
092215      MOVE AT-INITIAL-MANUAL-RESERVE TO TR-INITIAL-MANUAL-RESERVE
092215      MOVE AT-CURRENT-MANUAL-RESERVE TO TR-CURRENT-MANUAL-RESERVE
092215      MOVE AT-ITD-ADDITIONAL-RESERVE TO TR-ITD-ADDITIONAL-RESERVE
092215         MOVE AT-EXPENSE-METHOD         TO TR-EXPENSE-METHOD
092215         MOVE AT-EXPENSE-PERCENT        TO TR-EXPENSE-PERCENT
092215         MOVE AT-EXPENSE-DOLLAR         TO TR-EXPENSE-DOLLAR
092215         MOVE AT-ITD-PAID-EXPENSES      TO TR-ITD-PAID-EXPENSES
092215      MOVE AT-ITD-CHARGEABLE-EXPENSE TO TR-ITD-CHARGEABLE-EXPENSE
092215         MOVE AT-ITD-LIFE-REFUNDS       TO TR-ITD-LIFE-REFUNDS
092215         MOVE AT-ITD-AH-REFUNDS         TO TR-ITD-AH-REFUNDS
092215         MOVE AT-RESERVES-LAST-MAINT-DT TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-RESERVES-LAST-MAINT-DT
092215         END-IF
092215
092215         MOVE AT-RESERVES-LAST-UPDATED-BY
092215                                   TO TR-RESERVES-LAST-UPDATED-BY
092215         PERFORM VARYING X FROM 1 BY 1
092215              UNTIL X > 6
092215           MOVE AT-OPEN-CLOSE-DATE(X)    TO DC-BIN-DATE-1
020218           move ' ' to dc-option-code
092215           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215           IF NO-CONVERSION-ERROR
092215               MOVE DC-GREG-DATE-A-EDIT TO TR-OPEN-CLOSE-DATE(X)
092215           END-IF
092215           MOVE AT-OPEN-CLOSE-TYPE(X) TO TR-OPEN-CLOSE-TYPE(X)
092215           MOVE AT-OPEN-CLOSE-REASON(X) TO TR-OPEN-CLOSE-REASON(X)
092215         END-PERFORM
092215
092215       WHEN PAYMENT-TR
092215         MOVE AT-PAYMENT-TYPE         TO TR-PAYMENT-TYPE
092215         MOVE AT-CLAIM-TYPE           TO TR-CLAIM-TYPE
092215         MOVE AT-CLAIM-PREM-TYPE      TO TR-CLAIM-PREM-TYPE
092215         MOVE AT-AMOUNT-PAID          TO TR-AMOUNT-PAID
092215         MOVE AT-CHECK-NO             TO TR-CHECK-NO
092215         MOVE AT-PAID-FROM-DT         TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PAID-FROM-DT
092215         END-IF
092215
092215         MOVE AT-PAID-THRU-DT         TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PAID-THRU-DT
092215         END-IF
092215
092215         MOVE AT-DAYS-IN-PERIOD       TO TR-DAYS-IN-PERIOD
052217         IF AT-ACH-PAYMENT = 'Y'
052217            MOVE AT-ACH-PAYMENT       TO TR-ACH-PAYMENT
052217         ELSE
052217            MOVE 'N'                  TO TR-ACH-PAYMENT
052217         END-IF
050420         if at-payees-name = low-values
050420            move spaces to at-payees-name
050420         end-if
092215         MOVE AT-PAYEES-NAME          TO TR-PAYEES-NAME
092215         MOVE AT-PAYMENT-ORIGIN       TO TR-PAYMENT-ORIGIN
092215         MOVE AT-CHECK-WRITTEN-DT     TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-CHECK-WRITTEN-DT
092215         END-IF
092215         MOVE AT-TO-BE-WRITTEN-DT     TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-TO-BE-WRITTEN-DT
092215         END-IF
092215         MOVE AT-VOID-DT     TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-VOID-DT
092215         END-IF
092215         MOVE AT-VOID-REASON TO TR-VOID-REASON
092215         MOVE AT-PMT-APPROVED-BY      TO TR-PMT-APPROVED-BY
092215         MOVE AT-ADDL-RESERVE         TO TR-ADDL-RESERVE
092215         MOVE AT-EXPENSE-PER-PMT      TO TR-EXPENSE-PER-PMT
092215         MOVE AT-PMT-SELECT-DT      TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PMT-SELECT-DT
092215         END-IF
092215         MOVE AT-PMT-ACCEPT-DT    TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PMT-ACCEPT-DT
092215         END-IF
092215         MOVE AT-VOID-SELECT-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-VOID-SELECT-DT
092215         END-IF
092215         MOVE AT-VOID-ACCEPT-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-VOID-ACCEPT-DT
092215         END-IF
092215
092215         MOVE AT-CHECK-QUE-CONTROL    TO TR-CHECK-QUE-CONTROL
092215         MOVE AT-CHECK-QUE-SEQUENCE   TO TR-CHECK-QUE-SEQUENCE
092215         MOVE AT-FORCE-CONTROL        TO TR-FORCE-CONTROL
092215         MOVE AT-PREV-LAST-PMT-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PREV-LAST-PMT-DT
092215         END-IF
092215         MOVE AT-PREV-PAID-THRU-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PREV-PAID-THRU-DT
092215         END-IF
092215         MOVE AT-PREV-LAST-PMT-AMT   TO TR-PREV-LAST-PMT-AMT
092215
092215         MOVE AT-ELIMINATION-DAYS   TO TR-ELIMINATION-DAYS
092215         MOVE AT-DAILY-RATE         TO TR-DAILY-RATE
092215         MOVE AT-BENEFIT-TYPE       TO TR-BENEFIT-TYPE
092215         MOVE AT-EXPENSE-TYPE       TO TR-EXPENSE-TYPE
092215         MOVE AT-PAYMENT-APPROVAL-SW TO TR-PAYMENT-APPROVAL-SW
092215         MOVE AT-PAYEE-TYPE         TO TR-PAYEE-TYPE
092215         MOVE AT-PAYEE-SEQ          TO TR-PAYEE-SEQ
092215         MOVE AT-CASH-PAYMENT       TO TR-CASH-PAYMENT
092215         MOVE AT-GROUPED-PAYMENT    TO TR-GROUPED-PAYMENT
092215         MOVE AT-PAYMENT-NOTE-SEQ-NO TO TR-PAYMENT-NOTE-SEQ-NO
092215         MOVE AT-APPROVAL-LEVEL-REQD TO TR-APPROVAL-LEVEL-REQD
092215         MOVE AT-APPROVED-LEVEL     TO TR-APPROVED-LEVEL
092215         MOVE AT-VOID-TYPE          TO TR-VOID-TYPE
092215         MOVE AT-AIG-UNEMP-IND      TO TR-AIG-UNEMP-IND
092215         MOVE AT-ASSOCIATES         TO TR-ASSOCIATES
092215         MOVE AT-FORM-CTL-SEQ-NO    TO TR-FORM-CTL-SEQ-NO
092215         MOVE AT-CV-PMT-CODE        TO TR-CV-PMT-CODE
092215         MOVE AT-EOB-CODE1          TO TR-EOB-CODE1
092215         MOVE AT-EOB-CODE2          TO TR-EOB-CODE2
092215         MOVE AT-EOB-CODE3          TO TR-EOB-CODE3
092215*        MOVE AT-EOB-CODE4          TO TR-EOB-CODE4
092215
092215         MOVE AT-INT-PMT-SELECT-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-INT-PMT-SELECT-DT
092215         END-IF
092215
092215*        MOVE AT-EOB-CODE5          TO TR-EOB-CODE5
092215         MOVE AT-PMT-PROOF-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PMT-PROOF-DT
092215         END-IF
092215
092215         MOVE AT-PRINT-EOB-WITH-CHECK TO TR-PRINT-EOB-WITH-CHECK
092215         MOVE AT-PAYMENT-LAST-MAINT-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PAYMENT-LAST-MAINT-DT
092215         END-IF
092215         MOVE AT-PAYMENT-LAST-UPDATED-BY
092215                                    TO TR-PAYMENT-LAST-UPDATED-BY
042320         if AT-PAYMENT-NOTE-SEQ-NO > +0
042320            perform 0260-get-pmt-note
042320                                 thru 0260-exit
042320         end-if
092215
092215       WHEN AUTO-PAY-TR
092215         GO TO 0240-EXIT
092215       WHEN CORRESPONDENCE-TR
092215         MOVE AT-LETTER-SENT-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-LETTER-SENT-DT
092215         END-IF
092215
092215         MOVE AT-RECEIPT-FOLLOW-UP   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-RECEIPT-FOLLOW-UP
092215         END-IF
092215
092215         MOVE AT-AUTO-RE-SEND-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-AUTO-RE-SEND-DT
092215         END-IF
092215         MOVE AT-LETTER-ANSWERED-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-LETTER-ANSWERED-DT
050420         else
050420            move spaces to tr-letter-answered-dt
092215         END-IF
092215
050420         if at-letter-archive-no not numeric
050420            move zeros to at-letter-archive-no
050420         end-if

092215         MOVE AT-LETTER-ARCHIVE-NO   TO TR-LETTER-ARCHIVE-NO
092215         MOVE AT-LETTER-ORIGIN       TO TR-LETTER-ORIGIN
050420         if at-std-letter-form = low-values
050420            move spaces to at-std-letter-form
050420         end-if
092215         MOVE AT-STD-LETTER-FORM     TO TR-STD-LETTER-FORM
092215         MOVE AT-REASON-TEXT         TO TR-REASON-TEXT
092215         MOVE AT-ADDRESS-REC-SEQ-NO  TO TR-ADDRESS-REC-SEQ-NO
092215
092215         MOVE AT-ADDRESEE-TYPE       TO TR-ADDRESEE-TYPE
092215         MOVE AT-ADDRESSEE-NAME      TO TR-ADDRESSEE-NAME
092215         MOVE AT-INITIAL-PRINT-DATE   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-INITIAL-PRINT-DATE
092215         END-IF
092215         MOVE AT-RESEND-PRINT-DATE   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-RESEND-PRINT-DATE
092215         END-IF
092215
050420         if at-corr-sol-unsol = low-values
050420            move spaces to at-corr-sol-unsol
050420         end-if

092215         MOVE AT-CORR-SOL-UNSOL     TO TR-CORR-SOL-UNSOL
092215         MOVE AT-LETTER-PURGED-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-LETTER-PURGED-DT
092215         END-IF
092215
050420         if at-resend-letter-form = low-values
050420            move spaces to at-resend-letter-form
050420         end-if
050420
050420         if at-resend-letter-form valid-field
050420            continue
050420         else
050420            move spaces to at-resend-letter-form
050420         end-if

092215         MOVE AT-RESEND-LETTER-FORM TO TR-RESEND-LETTER-FORM
092215         MOVE AT-AUTO-CLOSE-IND     TO TR-AUTO-CLOSE-IND
092215         MOVE AT-LETTER-TO-BENE     TO TR-LETTER-TO-BENE
092215         MOVE AT-STOP-LETTER-DT   TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-STOP-LETTER-DT
092215         END-IF
092215
092215         MOVE AT-CSO-LETTER-STATUS  TO TR-CSO-LETTER-STATUS
092215         MOVE AT-CSO-LETTER-PURGE-DATE  TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-CSO-LETTER-PURGE-DATE
092215         END-IF
092215         MOVE AT-CSO-LETTER-RELOAD-DATE  TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-CSO-LETTER-RELOAD-DATE
092215         END-IF
092215         MOVE AT-CORR-LAST-MAINT-DT  TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-CORR-LAST-MAINT-DT
092215         END-IF
092215         MOVE AT-CORR-LAST-UPDATED-BY TO TR-CORR-LAST-UPDATED-BY
062217         IF AT-AUTH-RCVD = 'Y' OR 'N'
062217            MOVE AT-AUTH-RCVD       TO TR-AUTH-RCVD
062217         END-IF

092215       WHEN GENERAL-INFO-TR
092215         GO TO 0240-EXIT
092215         MOVE AT-INFO-LINE-1       TO TR-INFO-LINE-1
092215         MOVE AT-INFO-LINE-2       TO TR-INFO-LINE-2
092215         MOVE AT-INFO-TRAILER-TYPE TO TR-INFO-TRAILER-TYPE
092215         MOVE AT-CALL-TYPE         TO TR-CALL-TYPE
092215         MOVE AT-NOTE-CONTINUATION TO TR-NOTE-CONTINUATION
092215         MOVE AT-EOB-CODES-EXIST   TO TR-EOB-CODES-EXIST
092215         MOVE AT-GEN-INFO-LAST-MAINT-DT  TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-GEN-INFO-LAST-MAINT-DT
092215         END-IF
092215         MOVE AT-GEN-INFO-LAST-UPDATED-BY
092215                                  TO TR-GEN-INFO-LAST-UPDATED-BY
092215       WHEN AUTO-PROMPT-TR
092215*        GO TO 0240-EXIT
092215         MOVE AT-PROMPT-LINE-1        TO TR-PROMPT-LINE-1
092215         MOVE AT-PROMPT-LINE-2        TO TR-PROMPT-LINE-2
092215         MOVE AT-PROMPT-START-DT  TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PROMPT-START-DT
092215         END-IF
092215         MOVE AT-PROMPT-END-DT  TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PROMPT-END-DT
092215         END-IF
092215         MOVE AT-PROMPT-LAST-MAINT-DT TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-PROMPT-LAST-MAINT-DT
092215         END-IF
092215         MOVE AT-PROMPT-LAST-UPDATED-BY
092215                                     TO TR-PROMPT-LAST-UPDATED-BY
092215       WHEN DENIAL-TR
092215*        GO TO 0240-EXIT
092215         MOVE AT-DENIAL-INFO-1  TO TR-DENIAL-INFO-1
092215         MOVE AT-DENIAL-INFO-2  TO TR-DENIAL-INFO-2
092215         MOVE AT-DENIAL-DT TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-DENIAL-DT
092215         END-IF
092215         MOVE AT-RETRACTION-DT TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-RETRACTION-DT
092215         END-IF
050420         if at-denial-reason-code = low-values
050420            move spaces to at-denial-reason-code
050420         end-if
092215         MOVE AT-DENIAL-REASON-CODE  TO TR-DENIAL-REASON-CODE
092215         MOVE AT-DENIAL-PROOF-DT TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-DENIAL-PROOF-DT
092215         END-IF
092215
092215         MOVE AT-DENIAL-LAST-MAINT-DT TO DC-BIN-DATE-1
020218         move ' ' to dc-option-code
092215         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
092215         IF NO-CONVERSION-ERROR
092215            MOVE DC-GREG-DATE-A-EDIT TO TR-DENIAL-LAST-MAINT-DT
092215         END-IF
092215         MOVE AT-DENIAL-LAST-UPDATED-BY
092215                                   TO TR-DENIAL-LAST-UPDATED-BY
092215     END-EVALUATE.
092215
092215     INSPECT ELTRLR-OUT-REC REPLACING ALL ';' BY ' '
092215     INSPECT ELTRLR-OUT-REC REPLACING ALL X'A2' BY ';'
092215
092215*        IF TRL-RECS-OUT > 64600
092215*          AND  TRL-RECS-OUT < 64640
092215*           DISPLAY ' ADDRESS-REC-SEQ-NO '   TRL-RECS-OUT   ' '
092215*            TR-ADDRESS-REC-SEQ-NO
092215*          WRITE ELTRL-OUT-REC        FROM ELTRLR-OUT-REC
092215*        END-IF
092215
092215     WRITE ELTRL-OUT-REC        FROM ELTRLR-OUT-REC
092215     ADD 1 TO TRL-RECS-OUT.
092215     MOVE SAVE-ELTRL-OUT-REC TO ELTRLR-OUT-REC.
092215
092215 0240-EXIT.
092215     EXIT.
       0250-READ-ELTRLR.

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +90                    TO AT-SEQUENCE-NO

           READ ELTRLR

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              CONTINUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR READ      ' ELTRLR-FILE-STATUS
              ELSE
                 ADD 1 TO TRL-RECS-IN   
              END-IF
           END-IF

           .
       0250-EXIT.
           EXIT.
042320 0260-get-pmt-note.
042320
042320     move at-control-primary     to pn-control-primary
042320     move AT-PAYMENT-NOTE-SEQ-NO to pn-sequence-no
042320     read eltrlrn
042320     if eltrlrn-file-status = '00'
042320        if (pn-trailer-type = '6')
042320           and (pn-info-trailer-type = 'P')
042320           move pn-info-line-1   to tr-info-line-1
042320           move pn-info-line-2   to tr-info-line-2
042320        end-if
042320     end-if
042320
042320     .
042320 0260-exit.
042320     exit.
042320
       0275-READ-ELCERT.

           MOVE CL-CERT-KEY-DATA       TO CM-CONTROL-PRIMARY (2:21)
021704     MOVE DTE-CLASIC-COMPANY-CD  TO CM-COMPANY-CD
           MOVE CL-CERT-NO             TO CM-CERT-NO
           
           READ ELCERT

           IF ELCERT-FILE-STATUS = '10' OR '23'
              CONTINUE
           ELSE
              IF ELCERT-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCERT READ      ' ELCERT-FILE-STATUS
              ELSE
                 PERFORM 0280-GET-ERACCT
                                       THRU 0280-EXIT
                 ADD 1 TO CRT-RECS-IN   
              END-IF
           END-IF

           .
       0275-EXIT.
           EXIT.

       0280-GET-ERACCT.

           MOVE SPACES                 TO WS-AM-NAME
           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE CM-CONTROL-PRIMARY (1:20)
                                       TO AM-CONTROL-PRIMARY (1:20)
           MOVE CM-CERT-EFF-DT         TO AM-EXPIRATION-DT
           START ERACCT KEY >= AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS = '00'
              READ ERACCT NEXT RECORD
              IF ERACCT-FILE-STATUS = '00'
                 IF CM-CONTROL-PRIMARY (1:20) =
                    AM-CONTROL-PRIMARY (1:20)
                    MOVE AM-NAME       TO WS-AM-NAME
                 END-IF
              END-IF
           END-IF

           .
       0280-EXIT.
           EXIT.

       0300-WRITE-MSTR.

           INSPECT ELMSTR-DETAIL-RECORD REPLACING ALL ';' BY ' '
           INSPECT ELMSTR-DETAIL-RECORD REPLACING ALL X'A2' BY ';'
           WRITE ELMSTR-OUT-REC        FROM ELMSTR-DETAIL-RECORD
           ADD 1 TO CLM-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

042320     OPEN INPUT ELMSTR ELTRLR ELCERT ERACCT ELTRLRN
               OUTPUT ELMSTR-OUT
092215                ELTRLR-OUT
092215     IF ELMSTR-FILE-STATUS NOT = '00'
092215        DISPLAY 'ELMSTR OPEN     ' ELMSTR-FILE-STATUS
092215     END-IF
092215     IF ELTRLR-FILE-STATUS NOT = '00'
092215        DISPLAY 'ELTRLR OPEN     ' ELTRLR-FILE-STATUS
092215     END-IF

042320     IF ELTRLRN-FILE-STATUS NOT = '00'
042320        DISPLAY 'ELTRLRN OPEN     ' ELTRLRN-FILE-STATUS
042320        PERFORM ABEND-PGM
042320     END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELMSTR ELMSTR-OUT ELTRLR ELCERT ERACCT
042320           ELTRLR-OUT ELTRLRN
           .
       0500-EXIT.
           EXIT.

       0550-START-ELMSTR.

           MOVE LOW-VALUES             TO CL-CONTROL-PRIMARY
021704     MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD

           START ELMSTR KEY IS NOT < CL-CONTROL-PRIMARY

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELMSTR        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR START     ' ELMSTR-FILE-STATUS
                 SET END-OF-ELMSTR     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO ELMSTR-DETAIL-RECORD
052704     MOVE X'A2'                  TO EX-TAB1
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
                                          EX-TAB37
                                          EX-TAB38
                                          EX-TAB39
                                          EX-TAB40
                                          EX-TAB41
                                          EX-TAB42
043012                                    EX-TAB43
062812                                    ex-tab44
040814                                    EX-TAB45
040814                                    EX-TAB46
071014                                    EX-TAB47
071014                                    EX-TAB48
071014                                    EX-TAB49
071014                                    EX-TAB50
013017                                    ex-tab51
081817                                    EX-TAB52
081817                                    EX-TAB53
100917                                    ex-tab54
020218                                    ex-tab55

           MOVE '*'                    TO EX-LAST-BYTE

020218     display ' current date ' ws-current-date
020218
020218     MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT
020218     move +0 to dc-elapsed-days
020218                dc-elapsed-months
020218     MOVE '2'                    TO DC-OPTION-CODE
020218     PERFORM 8510-date-conversion thru 8590-exit
020218     if not no-conversion-error
020218        display ' current date error ' dc-error-code
020218     end-if
020218     MOVE DC-BIN-DATE-1          TO ws-current-bin-dt

           MOVE ELMSTR-DETAIL-RECORD   TO WS-SAVE-ELMSTR
092215     MOVE ELTRLR-OUT-REC TO SAVE-ELTRL-OUT-REC

           PERFORM 0550-START-ELMSTR   THRU 0550-EXIT
092215     PERFORM 0200-READ-ELMSTR    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
