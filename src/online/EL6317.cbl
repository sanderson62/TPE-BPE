       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL6317.
      *
      *AUTHOR.        PABLO
      *               OMAHA, NEBRASKA
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CNETRAL STATES  *
      *            *   HEALTH AND LIFE                                 *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO.        IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES. *
      *            *                                                   *
      *            *****************************************************
052307*-----------------------------------------------------------------
052307*                   C H A N G E   L O G
052307*
052307* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052307*-----------------------------------------------------------------
052307*  CHANGE  CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052307* EFFECTIVE    NUMBER
052307*-----------------------------------------------------------------
      *          2011022800001  PEMA  NEW PROGRAM
012412* 012412   2011022800001  AJRA  NAPERSOFT
062712* 062712   2011022800001  AJRA  REDEFINE ORIG DATA
071712* 071712 CR2011022800001  AJRA  NAPERSOFT CANCELS
072312* 072312   2011022800001  AJRA  NAPERSOFT MISC
100412* 100412   2011022800001  AJRA  POST IMPLEMENTATION ITEMS
102212* 102212   2012101700002  AJRA  SCREEN ID
103012* 103012   2012101700002  AJRA  ADD CERT NOTES TO TOP OF LIST
110612* 110612   2012101700002  AJRA  ADD NEW FIELDS
112612* 112612   2012101700002  AJRA  CHECK FOR PROMPT LETTER
121112* 121112   2012101700002  AJRA  CHANGE PF KEYS
121212* 121212   2012101700002  AJRA  ADD BILLING NOTE ENTRY
121712* 121712   2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413 IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
120313* 120313   2013090300001  AJRA  NAPERSOFT PHASE 2
121713* 121713   2013090300001  AJRA  ADD ENCLOSURE CODE TO SCREEN
041320* 041320 CR2020030500002  PEMA  Issue, cancel billing notes
052307*-----------------------------------------------------------------
      *REMARKS.
      *        TRANSACTION - EXBD - CERTIFICATE VERIFICATION
       EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL6317 WORKING STORAGE    *'.
       77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'.
       77  NOTE-SUB PIC S9(5) COMP-3 VALUE +0.
       77  WS-SEQ-NO                   PIC 9(4) BINARY.
       77  WS-SET-CODES-MDT            PIC X  VALUE SPACES.
       77  s1                          pic s999 comp-3 value +0.
       77  s2                          pic s999 comp-3 value +0.
       77  W-ARCH-NUMBER               PIC S9(08)      COMP value +0.
062712 77  WS-REASON-SW                PIC X  VALUE ' '.
062712     88  REASONS-FOUND                  VALUE 'Y'.
062712     88  REASONS-NOT-FOUND              VALUE 'N'.
072312 77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
072312     88  NO-CERT-RW                 VALUE 'N'.
072312     88  CERT-RW                    VALUE 'Y'.
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  KIXHOST             pic x(9) value Z"HOSTNAME".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
       01  WS-KIXHOST                  PIC X(10).
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).
      *                                COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
      *                                COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
       01 srch-commarea.
      *                                copy ELCADLTRSPI.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 060611    2011022800001  PEMA  NEW COPYBOOK
101812* 101812    2012101700002  AJRA  ADD ENDT ARCHIVE NO, SCREENID
110612* 110612    2012101700002  AJRA  EXPAND PASSED DATA
      ******************************************************************
      ****************************************
      *  commarea for NaperSoft On Demand Admin services letters
      *  (business logic input & output)
      ****************************************
           03  BL-INPUT.
               05  BL-DATA-SRCE        PIC X.
               05  BL-LETTER-ID        PIC XXXX.
               05  BL-CARRIER          PIC X.
               05  BL-GROUP            PIC X(6).
               05  BL-STATE            PIC XX.
               05  BL-ACCOUNT          PIC X(10).
               05  BL-EFF-DT           PIC X(10).
               05  BL-CERT-NO          PIC X(11).
               05  BL-BATCH-NO         PIC X(6).
               05  BL-BATCH-SEQ        PIC 9(8).
               05  BL-RESP-NO          PIC X(10).
               05  BL-NO-OF-COPIES     PIC 99.
               05  BL-PROC-ID          PIC XXXX.
               05  BL-COMP-ID          PIC XXX.
               05  BL-PRINT-NOW-SW     PIC X.
               05  BL-ENC-CD           PIC XXX.
               05  BL-RESEND-DT        PIC X(10).
               05  BL-FOLLOW-UP-DT     PIC X(10).
               05  BL-ARCHIVE-NO       PIC 9(8).
               05  BL-FUNC             PIC X(8).
110612         05  BL-COMMENTS         PIC X(100).
               05  FILLER REDEFINES BL-COMMENTS.
                   10  BL-REASON-CODE OCCURS 12 PIC X(4).
                   10  BL-LETTER-TO-ACCT PIC X.
                   10  BL-LETTER-TO-BENE PIC X.
                   10  BL-WRITE-ERARCH   PIC X.
                       88  ERARCH-QWS      VALUE 'Q'.
                       88  ERARCH-BATCH    VALUE 'B'.
                       88  ERARCH-TEMP     VALUE 'T'.
                   10  BL-PROCESS-TYPE PIC X(07).
                   10  BL-CERT-FORM-ID PIC X(05).
101812             10  BL-ENDT-ARCH-NO PIC 9(08) BINARY.
101812             10  BL-SOURCE-SCREEN PIC X(8).
110612             10  FILLER          PIC X(25).
           03  BL-OUTPUT.
               05  BL-STATUS                   PIC X.
                   88  BL-OK                      VALUE "P".
                   88  BL-FAIL                  VALUE "F".
               05  BL-MESSAGE          PIC X(50).
110612     03  BL-RECORD-PASSED-DATA   PIC X(6200).
110612     03  FILLER                  PIC X(31).
012412
012412*** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
012412*                                COPY ELCZREC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCZREC.                            *
      *                                                                *
      *   FILE DESCRIPTION = Z CONTROL RECORD LAYOUT                   *
      *                                                                *
      ******************************************************************
      *-----------------------------------------------------------------
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 122011    2011022800001  AJRA  NEW FILE
073112* 073112    2011022800001  AJRA  ADD ACCT SUMM, CSO SUMM
122712* 122712    2012101700002  AJRA  ADD REASON CODE REQUIRED FLAG
072313* 072313    2013062000003  AJRA  ADD IND FOR INSERTION BAR CODE
091913* 091913    2013090300001  AJRA  ADD SIGNATURE FLAG DEFAULT
      *-----------------------------------------------------------------
       01  W-Z-CONTROL-DATA.
           05  W-NUMBER-OF-COPIES      PIC  9.
           05  FILLER                  PIC  X.
           05  W-DAYS-TO-FOLLOW-UP     PIC  999.
           05  FILLER                  PIC  X.
           05  W-DAYS-TO-RESEND        PIC  999.
           05  FILLER                  PIC  X.
           05  W-FORM-TO-RESEND        PIC  X(4).
072313     05  W-ADD-BAR-CODE          PIC  X.
           05  W-PROMPT-LETTER         PIC  X.
072313     05  W-HAS-RETURN-ENV        PIC  X.
           05  W-ENCLOSURE-CD          PIC  XXX.
091913     05  W-SIG-FLAG-DEFAULT      PIC  X.
           05  W-AUTO-CLOSE-IND        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TO-BENE        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TO-ACCT        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TYPE           PIC  X.
           05  FILLER                  PIC  X.
           05  W-PRINT-CERTIFICATE     PIC  X.
           05  FILLER                  PIC  X.
           05  W-REFUND-REQUIRED       PIC  X.
           05  FILLER                  PIC  X.
           05  W-ONBASE-CODE           PIC  XX.
073112     05  FILLER                  PIC  X.
073112     05  W-ACCT-SUMM             PIC  X.
073112     05  FILLER                  PIC  X.
073112     05  W-CSO-SUMM              PIC  X.
122712     05  FILLER                  PIC  X.
122712     05  W-REASONS-REQUIRED      PIC  X.
122712     05  FILLER                  PIC  X(29).
       01  WS-PASSED-REASON-CODES      PIC X(60)  VALUE SPACES.
       01  STANDARD-AREAS.
00102      12  W-ARCH-SUPPRESS         PIC ZZZZZZZ9.
00103      12  W-ARCH-EDIT REDEFINES W-ARCH-SUPPRESS
00104                                  PIC  X(08).
00552      12  W-ARCH-LENGTH           PIC S9(04)  COMP  VALUE +250.
00142      12  QID.
00143          16  QID-TERM        PIC X(4)    VALUE SPACES.
00144          16  FILLER          PIC X(4)    VALUE '631J'.
           12  MAP-LENGTH          PIC 9(4)   VALUE 359 BINARY.
           12  TIME-MT                 PIC S9(7).
           12  TIME-MT-R  REDEFINES TIME-MT.
               16  FILLER              PIC X.
               16  TIME-LMT            PIC 99V99.
               16  FILLER              PIC X(2).
           12  GETMAIN-SPACE           PIC X VALUE SPACES.
PEMMOD     12  ERENDT-LENGTH           PIC S9(04) VALUE +579 COMP.
           12  ERPNDB-LENGTH           PIC 9(04)  VALUE 585 COMP.
072312     12  ERCNOT-LENGTH           PIC S9(04) VALUE +150 COMP.
072312     12  ERNOTE-LENGTH           PIC S9(04) VALUE +825 COMP.
072312     12  ELEOBC-LENGTH           PIC S9(04) VALUE +350 COMP.
121713     12  ELENCC-LENGTH           PIC S9(04) VALUE +400 COMP.
00434      12  W-CNTL-KEY.
00435          16  W-CNTL-COMPANY-ID   PIC  X(03).
00436          16  W-CNTL-RECORD-TYPE  PIC  X(01)   VALUE '1'.
00437          16  W-CNTL-GENL.
00438              20  W-CNTL-GEN1     PIC  X(02)   VALUE SPACES.
00439              20  W-CNTL-GEN2.
00440                  24 W-CNTL-GEN3  PIC  X(01)   VALUE SPACES.
00441                  24 W-CNTL-GEN4  PIC  X(01)   VALUE SPACES.
00442          16  W-CNTL-SEQ-NO       PIC S9(04)   VALUE +0    COMP.
072312     12  WS-ELCERT-KEY.
072312         16  W-CERT-COMPANY-CD   PIC X.
072312         16  W-CERT-CARRIER      PIC X.
072312         16  W-CERT-GROUPING     PIC X(6).
072312         16  W-CERT-STATE        PIC XX.
072312         16  W-CERT-ACCOUNT      PIC X(10).
072312         16  W-CERT-CERT-EFF-DT  PIC XX.
072312         16  W-CERT-CERT-PRIME   PIC X(10).
072312         16  W-CERT-CERT-SFX     PIC X.
121712
121712     12  WS-ELCRTT-KEY.
121712         16  WS-ELCRTT-PRIMARY   PIC X(33).
121712         16  WS-ELCRTT-REC-TYPE  PIC X(1).
121712     12  ELCRTT-RECORD-LENGTH    PIC S9(4) COMP VALUE +552.
121712     12  ELCRTT-JOURNAL-LENGTH   PIC S9(4) COMP VALUE +575.
121712
072312     12  WS-ELEOBC-KEY.
072312         16  WS-EOBC-COMPANY-CD  PIC X.
072312         16  WS-EOBC-REC-TYPE    PIC X.
072312         16  WS-EOBC-CODE        PIC X(4).
072312         16  FILLER              PIC X(9).
           12  WS-ERNOTE-KEY.
               16  W-NOTE-COMPANY-CD   PIC X.
               16  W-NOTE-CARRIER      PIC X.
               16  W-NOTE-GROUPING     PIC X(6).
               16  W-NOTE-STATE        PIC XX.
               16  W-NOTE-ACCOUNT      PIC X(10).
               16  W-NOTE-CERT-EFF-DT  PIC XX.
               16  W-NOTE-CERT-PRIME   PIC X(10).
               16  W-NOTE-CERT-SFX     PIC X.
041320         16  w-note-record-type  pic x.
           12  ELLETR-KEY.
112612         16  LETR-PART-KEY.
112612             20  LETR-COMPANY-CD PIC X.
112612             20  LETR-LETTER-ID  PIC X(4).
               16  LETR-FILLER         PIC X(8).
               16  LETR-SEQ-NO         PIC 9(4) BINARY.
112612     12  ELLETR-SAVE-PART-KEY    PIC X(5).
           12  ERPNDB2-KEY.
               16  PNDB2-COMPANY-CD    PIC X.
               16  PNDB2-CARRIER       PIC X.
               16  PNDB2-GROUPING      PIC X(6).
               16  PNDB2-STATE         PIC XX.
               16  PNDB2-ACCOUNT       PIC X(10).
               16  PNDB2-CERT-EFF-DT   PIC XX.
               16  PNDB2-CERT-PRIME    PIC X(10).
               16  PNDB2-CERT-SFX      PIC X.
               16  PNDB2-CHG-SEQ-NO    PIC 9(4) BINARY.
               16  PNDB2-RECORD-TYPE   PIC X.
           12  ERPNDB-KEY.
               16  PNDB-COMPANY-CD     PIC X.
               16  PNDB-BATCH-NO       PIC X(6).
               16  PNDB-BATCH-SEQ-NO   PIC 9(4) BINARY.
               16  PNDB-CHG-SEQ-NO     PIC 9(4) BINARY.
           12  ERENDT-KEY.
               16  ENDT-COMPANY-CD       PIC X.
               16  ENDT-CARRIER          PIC X.
               16  ENDT-GROUPING         PIC X(6).
               16  ENDT-STATE            PIC XX.
               16  ENDT-ACCOUNT          PIC X(10).
               16  ENDT-CERT-EFF-DT      PIC XX.
               16  ENDT-CERT-PRIME       PIC X(10).
               16  ENDT-CERT-SFX         PIC X.
               16  ENDT-RECORD-TYPE      PIC X.
               16  ENDT-SEQ-NO           PIC 9(4) BINARY.
           12  ELCRTO-KEY.
               16  CRTO-COMPANY-CD     PIC X.
               16  CRTO-CARRIER        PIC X.
               16  CRTO-GROUPING       PIC X(6).
               16  CRTO-STATE          PIC XX.
               16  CRTO-ACCOUNT        PIC X(10).
               16  CRTO-CERT-EFF-DT    PIC XX.
               16  CRTO-CERT-PRIME     PIC X(10).
               16  CRTO-CERT-SFX       PIC X.
               16  CRTO-RECORD-TYPE    PIC X.
               16  CRTO-SEQ-NO         PIC 9(4) BINARY.
           12  WS-ERMAIL-KEY.
               16  MAIL-COMPANY-CD     PIC X.
               16  MAIL-CARRIER        PIC X.
               16  MAIL-GROUPING       PIC X(6).
               16  MAIL-STATE          PIC XX.
               16  MAIL-ACCOUNT        PIC X(10).
               16  MAIL-CERT-EFF-DT    PIC XX.
               16  MAIL-CERT-PRIME     PIC X(10).
               16  MAIL-CERT-SFX       PIC X.
121713
121713     12  WS-ELENCC-KEY.
121713         16  WS-ENCC-COMPANY-CD  PIC X.
121713         16  WS-ENCC-REC-TYPE    PIC X.
121713         16  WS-ENCC-ENC-CODE    PIC X(5).
121713         16  FILLER              PIC X(09).
121713
           12  WS-LF-CANC-DT           PIC XX   VALUE LOW-VALUES.
           12  WS-AH-CANC-DT           PIC XX   VALUE LOW-VALUES.
           12  WS-CURRENT-DT.
               16  FILLER              PIC X(6)    VALUE SPACES.
               16  WS-CURRENT-YR       PIC XX      VALUE SPACES.
           12  WS-CURRENT-BIN-DT       PIC XX    VALUE LOW-VALUES.
           12  WS-ISS-CAN-SW           PIC X     VALUE LOW-VALUES.
               88  CANCEL-REC            VALUE 'C'.
               88  ISSUE-REC             VALUE 'I'.
           12  WS-WORK-AMT             PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-HOLD-ACCT-KEY        PIC X(26) VALUE LOW-VALUES.
           12  ELCNTL-KEY.
               16  CNTL-COMP-ID        PIC X(3)  VALUE SPACES.
               16  CNTL-REC-TYPE       PIC X     VALUE SPACES.
               16  CNTL-ACCESS.
                   20  CNTL-STATE      PIC XX    VALUE SPACES.
                   20  FILLER          PIC X     VALUE SPACES.
                   20  CNTL-CARRIER    PIC X     VALUE SPACES.
               16  CNTL-SEQ            PIC S9(4) VALUE +0 COMP.
           12  ERACCT-KEY.
               16  ERACCT-COMP-KEY.
                   20  ACCT-CO         PIC X     VALUE SPACES.
                   20  ACCT-CARRIER    PIC X     VALUE SPACES.
                   20  ACCT-GROUPING   PIC X(6)  VALUE SPACES.
                   20  ACCT-STATE      PIC XX    VALUE SPACES.
                   20  ACCT-ACCOUNT    PIC X(10) VALUE SPACES.
               16  ACCT-EXP-DATE       PIC XX    VALUE SPACES.
               16  FILLER              PIC X(4)  VALUE LOW-VALUES.
           12  ERACCT-SAVE-KEY         PIC X(20) VALUE SPACES.
           12  WS-ELCERT-FILE-ID       PIC X(8) VALUE 'ELCERT'.
           12  WS-ELCNTL-FILE-ID       PIC X(8) VALUE 'ELCNTL'.
           12  WS-ERNOTE-FILE-ID       PIC X(8) VALUE 'ERNOTE'.
           12  WS-ELCRTO-FILE-ID       PIC X(8) VALUE 'ELCRTO'.
           12  WS-ERENDT-FILE-ID       PIC X(8) VALUE 'ERENDT'.
           12  WS-ERPNDB2-FILE-ID      PIC X(8) VALUE 'ERPNDB2'.
           12  WS-ERPNDB-FILE-ID       PIC X(8) VALUE 'ERPNDB'.
           12  WS-ERACCT2-FILE-ID      PIC X(8) VALUE 'ERACCT2'.
072312     12  WS-ERCNOT-FILE-ID       PIC X(8) VALUE 'ERCNOT'.
072312     12  WS-ELEOBC-FILE-ID       PIC X(8) VALUE 'ELEOBC'.
121712     12  WS-ELCRTT-FILE-ID       PIC X(8) VALUE 'ELCRTT'.
121713     12  WS-ELENCC-FILE-ID       PIC X(8) VALUE 'ELENCC'.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
062712         88  RESP-DUPREC                  VALUE +14.
062712         88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  MAP-NAME            PIC X(8)    VALUE 'EL631J'.
           12  MAPSET-NAME         PIC X(8)    VALUE 'EL6317S'.
           12  SCREEN-NUMBER       PIC X(4)    VALUE '631J'.
           12  TRANS-ID            PIC X(4)    VALUE 'EXBD'.
           12  THIS-PGM            PIC X(8)    VALUE 'EL6317'.
           12  PGM-NAME            PIC X(8).
           12  TIME-IN             PIC S9(7).
           12  TIME-OUT-R  REDEFINES TIME-IN.
               16  FILLER          PIC X.
               16  TIME-OUT        PIC 99V99.
               16  FILLER          PIC X(2).
           12  XCTL-005            PIC X(8)    VALUE 'EL005'.
           12  XCTL-010            PIC X(8)    VALUE 'EL010'.
           12  XCTL-626            PIC X(8)    VALUE 'EL626'.
           12  XCTL-6311           PIC X(8)    VALUE 'EL6311'.
           12  LINK-001            PIC X(8)    VALUE 'EL001'.
           12  LINK-004            PIC X(8)    VALUE 'EL004'.
           12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
           12  WS-PHONE.
               16  WS-PH1              PIC XXX.
               16  WS-PH2              PIC XXX.
               16  WS-PH3              PIC XXXX.
           12  WS-PHONE-NUM REDEFINES WS-PHONE PIC 9(10).
           12  WS-DEV-RT               PIC S9V9(6) COMP-3.
072312
072312     12  WS-BILLING-NOTE.
072312         16  WS-BN-NOTE          PIC X(25).
072312         16  WS-BN-LTRID         PIC X(4).
072312         16  FILLER              PIC X(3).
072312         16  WS-BN-DATE          PIC X(8).
072312         16  FILLER              PIC X(3).
072312         16  WS-BN-USERID        PIC X(4).
072312         16  FILLER              PIC X(30).
121212     12  WS-MANUAL-BILL-NOTE REDEFINES WS-BILLING-NOTE.
121212         16  WS-MAN-BN-NOTE      PIC X(63).
121212         16  FILLER              PIC X(14).
121212     12  WS-LEN                  PIC S9(5) COMP-3 VALUE +0.
062712
062712 01  NEW-ELCRTO-REC.
062712     12  NEW-ELCRTO-KEY.
062712         16  NCRTO-COMPANY-CD      PIC X.
062712         16  NCRTO-CARRIER         PIC X.
062712         16  NCRTO-GROUPING        PIC X(6).
062712         16  NCRTO-STATE           PIC XX.
062712         16  NCRTO-ACCOUNT         PIC X(10).
062712         16  NCRTO-CERT-EFF-DT     PIC XX.
062712         16  NCRTO-CERT-PRIME      PIC X(10).
062712         16  NCRTO-CERT-SFX        PIC X.
062712         16  NCRTO-RECORD-TYPE     PIC X.
062712         16  NCRTO-SEQ-NO          PIC 9(4) BINARY.
062712     12  NEW-ORIG-REC.
062712         16  NCRTO-INS-LAST-NAME   PIC X(15).
062712         16  NCRTO-INS-FIRST-NAME  PIC X(10).
062712         16  NCRTO-INS-MIDDLE-INIT PIC X.
062712         16  NCRTO-INS-AGE         PIC S999     COMP-3.
062712         16  NCRTO-JNT-LAST-NAME   PIC X(15).
062712         16  NCRTO-JNT-FIRST-NAME  PIC X(10).
062712         16  NCRTO-JNT-MIDDLE-INIT PIC X.
062712         16  NCRTO-JNT-AGE         PIC S999     COMP-3.
062712         16  NCRTO-LF-BENCD        PIC XX.
062712         16  NCRTO-LF-TERM         PIC S999      COMP-3.
062712         16  NCRTO-LF-BEN-AMT      PIC S9(9)V99  COMP-3.
062712         16  NCRTO-LF-PRM-AMT      PIC S9(7)V99  COMP-3.
062712         16  NCRTO-LF-ALT-BEN-AMT  PIC S9(9)V99  COMP-3.
062712         16  NCRTO-LF-ALT-PRM-AMT  PIC S9(7)V99  COMP-3.
062712         16  NCRTO-LF-EXP-DT       PIC XX.
062712         16  NCRTO-LF-COMM-PCT     PIC SV9(5)    COMP-3.
062712         16  NCRTO-LF-CANCEL-DT    PIC XX.
062712         16  NCRTO-LF-CANCEL-AMT   PIC S9(7)V99  COMP-3.
071712         16  NCRTO-LF-ITD-CANCEL-AMT PIC S9(7)V99 COMP-3.
062712         16  NCRTO-AH-BENCD        PIC XX.
062712         16  NCRTO-AH-TERM         PIC S999      COMP-3.
062712         16  NCRTO-AH-BEN-AMT      PIC S9(9)V99  COMP-3.
062712         16  NCRTO-AH-PRM-AMT      PIC S9(7)V99  COMP-3.
062712         16  NCRTO-AH-EXP-DT       PIC XX.
062712         16  NCRTO-AH-COMM-PCT     PIC SV9(5)    COMP-3.
062712         16  NCRTO-AH-CP           PIC 99.
062712         16  NCRTO-AH-CANCEL-DT    PIC XX.
062712         16  NCRTO-AH-CANCEL-AMT   PIC S9(7)V99  COMP-3.
071712         16  NCRTO-AH-ITD-CANCEL-AMT PIC S9(7)V99 COMP-3.
062712         16  NCRTO-CRED-BENE-NAME  PIC X(25).
062712         16  NCRTO-1ST-PMT-DT      PIC XX.
121712         16  NCRTO-INS-AGE-DEFAULT-FLAG PIC X.
121712         16  NCRTO-JNT-AGE-DEFAULT-FLAG PIC X.
011413         16  NCRTO-ISSUE-TRAN-IND  PIC X.
011413         16  NCRTO-CANCEL-TRAN-IND PIC X.
011413         16  FILLER                PIC X(235).
103012
103012 01  CERT-NOTE-ENTRIES.
103012     12  CERT-NT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.
103012         16  CERT-NT-TEXT                PIC X(63).
103012         16  CERT-NT-LAST-MAINT-BY       PIC XXXX.
103012         16  CERT-NT-LAST-MAINT-DT       PIC XX.
103012         16  CERT-NT-LAST-MAINT-HHMMSS   PIC S9(7) COMP-3.
103012
103012 01  ERCNOT-AREA.
103012     12  ERCNOT-KEY-LENGTH       PIC S9(4)   COMP VALUE +36.
103012     12  ERCNOT-START-LENGTH     PIC S9(4)   COMP VALUE +34.
103012     12  ERCNOT-KEY.
103012         16  ERCNOT-PARTIAL-KEY.
103012             20 ERCNOT-COMPANY-CD    PIC X.
103012             20 ERCNOT-CARRIER       PIC X.
103012             20 ERCNOT-GROUPING      PIC X(06).
103012             20 ERCNOT-STATE         PIC XX.
103012             20 ERCNOT-ACCOUNT       PIC X(10).
103012             20 ERCNOT-EFF-DT        PIC XX.
103012             20 ERCNOT-CERT-NO.
103012                25 ERCNOT-CERT-PRIME PIC X(10).
103012                25 ERCNOT-CERT-SFX   PIC X.
103012             20 ERCNOT-REC-TYP       PIC X.
103012         16 ERCNOT-SEQ           PIC S9(4) COMP.
103012     12  SV-PARTIAL-KEY.
103012         20 SV-COMPANY-CD            PIC X.
103012         20 SV-CARRIER               PIC X.
103012         20 SV-GROUPING              PIC X(06).
103012         20 SV-STATE                 PIC XX.
103012         20 SV-ACCOUNT               PIC X(10).
103012         20 SV-EFF-DT                PIC XX.
103012         20 SV-CERT-NO.
103012            25 SV-CERT-PRIME         PIC X(10).
103012            25 SV-CERT-SFX           PIC X(1).
103012         20 SV-REC-TYP               PIC X.
       01  ERROR-MESSAGES.
           12  ER-0000                 PIC X(4)  VALUE '0000'.
           12  ER-0004                 PIC X(4)  VALUE '0004'.
           12  ER-0008                 PIC X(4)  VALUE '0008'.
           12  ER-0023                 PIC X(4)  VALUE '0023'.
           12  ER-0029                 PIC X(4)  VALUE '0029'.
           12  ER-0068                 PIC X(4)  VALUE '0068'.
           12  ER-0132                 PIC X(4)  VALUE '0132'.
           12  ER-0142                 PIC X(4)  VALUE '0142'.
           12  ER-0148                 PIC X(4)  VALUE '0148'.
           12  ER-0215                 PIC X(4)  VALUE '0215'.
           12  ER-0280                 PIC X(4)  VALUE '0280'.
           12  ER-0348                 PIC X(4)  VALUE '0348'.
           12  ER-0419                 PIC X(4)  VALUE '0419'.
112612     12  ER-0894                 PIC X(4)  VALUE '0894'.
           12  ER-1101                 PIC X(4)  VALUE '1101'.
           12  ER-1236                 PIC X(4)  VALUE '1236'.
121713     12  ER-1560                 PIC X(4)  VALUE '1560'.
           12  ER-1818                 PIC X(4)  VALUE '1818'.
           12  ER-1820                 PIC X(4)  VALUE '1820'.
           12  ER-2056                 PIC X(4)  VALUE '2056'.
           12  ER-2079                 PIC X(4)  VALUE '2079'.
           12  ER-2208                 PIC X(4)  VALUE '2208'.
           12  ER-2209                 PIC X(4)  VALUE '2209'.
           12  ER-2237                 PIC X(4)  VALUE '2237'.
           12  ER-2238                 PIC X(4)  VALUE '2238'.
           12  ER-2253                 PIC X(4)  VALUE '2253'.
           12  ER-2619                 PIC X(4)  VALUE '2619'.
           12  ER-2651                 PIC X(4)  VALUE '2651'.
           12  ER-2784                 PIC X(4)  VALUE '2784'.
           12  ER-2785                 PIC X(4)  VALUE '2785'.
           12  ER-2786                 PIC X(4)  VALUE '2786'.
052307     12  ER-2791                 PIC X(4)  VALUE '2791'.
           12  ER-2845                 PIC X(4)  VALUE '2845'.
           12  ER-2848                 PIC X(4)  VALUE '2848'.
           12  ER-2851                 PIC X(4)  VALUE '2851'.
062412     12  ER-3830                 PIC X(4)  VALUE '3830'.
121112     12  ER-3833                 PIC X(4)  VALUE '3833'.
           12  ER-7345                 PIC X(4)  VALUE '7345'.
           12  ER-7638                 PIC X(4)  VALUE '7638'.
           12  ER-8888                 PIC X(4)  VALUE '8888'.
           12  ER-9999                 PIC X(4)  VALUE '9999'.
      *    COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
      *    COPY ELCTEXT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCTEXT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
00008 *                                     FORM LETTERS,              *
00009 *                                     CERT FORM DISPLAY.
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 100   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
00015 *       ALTERNATE INDEX = NONE                                   *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
00018 *       ALTERNATE INDEX = NONE                                   *
00019 *                                                                *
00020 *   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
00021 *       ALTERNATE INDEX = NONE                                   *
00022 *                                                                *
00023 *   LOG = NO                                                     *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  TEXT-FILES.
00027      12  TEXT-FILE-ID                PIC XX.
00028          88  FORMS-FILE-TEXT            VALUE 'TF'.
00029          88  LETTER-FILE-TEXT           VALUE 'TL'.
00030          88  HELP-FILE-TEXT             VALUE 'TH'.
00031
00032      12  TX-CONTROL-PRIMARY.
00033          16  TX-COMPANY-CD           PIC X.
00034              88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
00035          16  TX-ACCESS-CD-GENL       PIC X(12).
00036
00037          16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00038              20  TX-LETTER-NO        PIC X(4).
00039              20  FILLER              PIC X(8).
00040
00041          16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00042              20  TX-FORM-NO          PIC X(12).
00043
00044          16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00045              20  TX-HELP-TYPE        PIC X.
00046                  88  HELP-FOR-GENERAL   VALUE ' '.
00047                  88  HELP-BY-SCREEN     VALUE 'S'.
00048                  88  HELP-BY-ERROR      VALUE 'E'.
00049              20  TX-SCREEN-OR-ERROR  PIC X(4).
00050                  88  GENERAL-INFO-HELP  VALUE '0000'.
00051              20  TX-HELP-FOR-COMPANY  PIC XXX.
00052                  88  NOT-COMPANY-SPECIFIC VALUE '   '.
00053              20  FILLER              PIC X(4).
00054
00055          16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
00056
00057      12  TX-PROCESS-CONTROL          PIC XX.
00058          88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
00059
00060      12  TX-TEXT-LINE                PIC X(70).
00061
00062      12  TX-FORM-SQUEEZE-CONTROL     PIC X.
00063          88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
00064          88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
00065          88  TX-VALID-FORM-SQUEEZE-VALUE
00066                                         VALUE 'Y' ' '.
00067
00068      12  TX-LINE-SQUEEZE-CONTROL     PIC X.
00069          88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
00070          88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
00071          88  TX-DO-NOT-ADJUST           VALUE 'N'.
00072          88  TX-FORM-CONTROL-LINE       VALUE 'K'.
00073          88  TX-NEW-PARAGRAPH           VALUE 'P'.
00074          88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
00075          88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
00076                                               'K' 'N' ' '
00077                                               'Z'.
00078
00079      12  TX-ARCHIVE-SW               PIC X.
00080          88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
00081          88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
00082          88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
00083
00084      12  TX-LAST-MAINTENANCED-BY     PIC X(4).
00085      12  TX-LAST-MAINTENANCED-DT     PIC X(2).
00086
00087      12  TX-BSR-CODE                 PIC X.
00088          88  TX-BSR-LETTER              VALUE 'B'.
00089          88  TX-NON-BSR-LETTER          VALUE ' '.
00090
00091      12  FILLER                      PIC X.
00092 *****************************************************************
      *    COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
      *    COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
      *    COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
      *    COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
              16  PI-ISS-CAN-SW           PIC X.
              16  PI-ERENDT-KEY.
                  20  PI-ENDT-COMPANY-CD  PIC X.
                  20  PI-ENDT-CARRIER     PIC X.
                  20  PI-ENDT-GROUPING    PIC X(6).
                  20  PI-ENDT-STATE       PIC XX.
                  20  PI-ENDT-ACCOUNT     PIC X(10).
                  20  PI-ENDT-CERT-EFF-DT PIC XX.
                  20  PI-ENDT-CERT-PRIME  PIC X(10).
                  20  PI-ENDT-CERT-SFX    PIC X.
                  20  PI-ENDT-RECORD-TYPE PIC X.
                  20  PI-ENDT-SEQ-NO      PIC 9(4) BINARY.
              16  PI-ELCRTO-KEY.
                  20  PI-CRTO-COMPANY-CD  PIC X.
                  20  PI-CRTO-CARRIER     PIC X.
                  20  PI-CRTO-GROUPING    PIC X(6).
                  20  PI-CRTO-STATE       PIC XX.
                  20  PI-CRTO-ACCOUNT     PIC X(10).
                  20  PI-CRTO-CERT-EFF-DT PIC XX.
                  20  PI-CRTO-CERT-PRIME  PIC X(10).
                  20  PI-CRTO-CERT-SFX    PIC X.
                  20  PI-CRTO-RECORD-TYPE PIC X.
                  20  PI-CRTO-SEQ-NO      PIC 9(4) BINARY.
              16  PI-ERPNDB-KEY           PIC X(11).
              16  PI-DOCUMENT-PROCESSED   PIC X.
                  88  PI-VERI-PROCESSED      VALUE 'V'.
                  88  PI-ENDO-PROCESSED      VALUE 'G'.
062712        16  PI-PREV-ERENDT-KEY      PIC X(36).
062712        16  PI-RES-REF-CLM-TYPE     PIC X.
011712        16  PI-CLM-REFORM-IND       PIC X.
062712        16  PI-CLEAR-ERROR-SW       PIC X.
062712            88  PI-CLEAR-ERROR         VALUE 'Y'.
062712        16  PI-PRINT-CERTIFICATE    PIC X.
062712        16  PI-ENTERED.
062712            20  PI-PRTSW-ENTERED    PIC X.
062712            20  PI-SIGREQ-ENTERED   PIC X.
062712            20  PI-MONEY-ENTERED    PIC X.
062712            20  PI-LTRID-ENTERED    PIC X(4).
062712            20  PI-REASONS-ENTERED  PIC X(84).
072312            20  PI-CERTNT1-ENTERED  PIC X(63).
072312            20  PI-CERTNT2-ENTERED  PIC X(63).
072312        16  FILLER                  PIC X(17).
072312        16  PI-FINALIZED-IND        PIC X.
121212        16  PI-BILLNT1-ENTERED      PIC X(63).
121212        16  PI-BILLNT2-ENTERED      PIC X(63).
121713        16  PI-ENCCODE              PIC X(3).
121713        16  FILLER                  PIC X(151).
          12  FILLER                      PIC X(276).
      *    COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
       01  FILLER    REDEFINES DFHAID.
           12  FILLER              PIC X(8).
           12  PF-VALUES           PIC X       OCCURS 24 TIMES.
      *   COPY EL6317S.
       01  EL631JI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  PROCIDL PIC S9(0004) COMP.
           05  PROCIDF PIC  X(0001).
           05  FILLER REDEFINES PROCIDF.
               10  PROCIDA PIC  X(0001).
           05  PROCIDI PIC  X(0004).
      *    -------------------------------
           05  HOSTL PIC S9(0004) COMP.
           05  HOSTF PIC  X(0001).
           05  FILLER REDEFINES HOSTF.
               10  HOSTA PIC  X(0001).
           05  HOSTI PIC  X(0009).
      *    -------------------------------
           05  SYSL PIC S9(0004) COMP.
           05  SYSF PIC  X(0001).
           05  FILLER REDEFINES SYSF.
               10  SYSA PIC  X(0001).
           05  SYSI PIC  X(0005).
      *    -------------------------------
           05  COMPANYL PIC S9(0004) COMP.
           05  COMPANYF PIC  X(0001).
           05  FILLER REDEFINES COMPANYF.
               10  COMPANYA PIC  X(0001).
           05  COMPANYI PIC  X(0003).
      *    -------------------------------
           05  DATASORL PIC S9(0004) COMP.
           05  DATASORF PIC  X(0001).
           05  FILLER REDEFINES DATASORF.
               10  DATASORA PIC  X(0001).
           05  DATASORI PIC  X(0001).
      *    -------------------------------
           05  BATCHNOL PIC S9(0004) COMP.
           05  BATCHNOF PIC  X(0001).
           05  FILLER REDEFINES BATCHNOF.
               10  BATCHNOA PIC  X(0001).
           05  BATCHNOI PIC  X(0006).
      *    -------------------------------
           05  BSEQNOL PIC S9(0004) COMP.
           05  BSEQNOF PIC  X(0001).
           05  FILLER REDEFINES BSEQNOF.
               10  BSEQNOA PIC  X(0001).
           05  BSEQNOI PIC  X(0008).
      *    -------------------------------
           05  ARCHNOL PIC S9(0004) COMP.
           05  ARCHNOF PIC  X(0001).
           05  FILLER REDEFINES ARCHNOF.
               10  ARCHNOA PIC  X(0001).
           05  ARCHNOI PIC  X(0008).
      *    -------------------------------
           05  PRTSWL PIC S9(0004) COMP.
           05  PRTSWF PIC  X(0001).
           05  FILLER REDEFINES PRTSWF.
               10  PRTSWA PIC  X(0001).
           05  PRTSWI PIC  X(0001).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0008).
      *    -------------------------------
           05  CERTNOL PIC S9(0004) COMP.
           05  CERTNOF PIC  X(0001).
           05  FILLER REDEFINES CERTNOF.
               10  CERTNOA PIC  X(0001).
           05  CERTNOI PIC  X(0010).
      *    -------------------------------
           05  CRTSFXL PIC S9(0004) COMP.
           05  CRTSFXF PIC  X(0001).
           05  FILLER REDEFINES CRTSFXF.
               10  CRTSFXA PIC  X(0001).
           05  CRTSFXI PIC  X(0001).
      *    -------------------------------
           05  SEQNOL PIC S9(0004) COMP.
           05  SEQNOF PIC  X(0001).
           05  FILLER REDEFINES SEQNOF.
               10  SEQNOA PIC  X(0001).
           05  SEQNOI PIC  X(0006).
      *    -------------------------------
           05  SIGREQL PIC S9(0004) COMP.
           05  SIGREQF PIC  X(0001).
           05  FILLER REDEFINES SIGREQF.
               10  SIGREQA PIC  X(0001).
           05  SIGREQI PIC  X(0001).
      *    -------------------------------
           05  LTRIDL PIC S9(0004) COMP.
           05  LTRIDF PIC  X(0001).
           05  FILLER REDEFINES LTRIDF.
               10  LTRIDA PIC  X(0001).
           05  LTRIDI PIC  X(0004).
      *    -------------------------------
           05  ENCCODEL PIC S9(0004) COMP.
           05  ENCCODEF PIC  X(0001).
           05  FILLER REDEFINES ENCCODEF.
               10  ENCCODEA PIC  X(0001).
           05  ENCCODEI PIC  X(0003).
      *    -------------------------------
           05  RCD01L PIC S9(0004) COMP.
           05  RCD01F PIC  X(0001).
           05  FILLER REDEFINES RCD01F.
               10  RCD01A PIC  X(0001).
           05  RCD01I PIC  X(0004).
      *    -------------------------------
           05  RCD02L PIC S9(0004) COMP.
           05  RCD02F PIC  X(0001).
           05  FILLER REDEFINES RCD02F.
               10  RCD02A PIC  X(0001).
           05  RCD02I PIC  X(0004).
      *    -------------------------------
           05  RCD03L PIC S9(0004) COMP.
           05  RCD03F PIC  X(0001).
           05  FILLER REDEFINES RCD03F.
               10  RCD03A PIC  X(0001).
           05  RCD03I PIC  X(0004).
      *    -------------------------------
           05  RCD04L PIC S9(0004) COMP.
           05  RCD04F PIC  X(0001).
           05  FILLER REDEFINES RCD04F.
               10  RCD04A PIC  X(0001).
           05  RCD04I PIC  X(0004).
      *    -------------------------------
           05  RCD05L PIC S9(0004) COMP.
           05  RCD05F PIC  X(0001).
           05  FILLER REDEFINES RCD05F.
               10  RCD05A PIC  X(0001).
           05  RCD05I PIC  X(0004).
      *    -------------------------------
           05  RCD06L PIC S9(0004) COMP.
           05  RCD06F PIC  X(0001).
           05  FILLER REDEFINES RCD06F.
               10  RCD06A PIC  X(0001).
           05  RCD06I PIC  X(0004).
      *    -------------------------------
           05  RCD07L PIC S9(0004) COMP.
           05  RCD07F PIC  X(0001).
           05  FILLER REDEFINES RCD07F.
               10  RCD07A PIC  X(0001).
           05  RCD07I PIC  X(0004).
      *    -------------------------------
           05  RCD08L PIC S9(0004) COMP.
           05  RCD08F PIC  X(0001).
           05  FILLER REDEFINES RCD08F.
               10  RCD08A PIC  X(0001).
           05  RCD08I PIC  X(0004).
      *    -------------------------------
           05  RCD09L PIC S9(0004) COMP.
           05  RCD09F PIC  X(0001).
           05  FILLER REDEFINES RCD09F.
               10  RCD09A PIC  X(0001).
           05  RCD09I PIC  X(0004).
      *    -------------------------------
           05  RCD10L PIC S9(0004) COMP.
           05  RCD10F PIC  X(0001).
           05  FILLER REDEFINES RCD10F.
               10  RCD10A PIC  X(0001).
           05  RCD10I PIC  X(0004).
      *    -------------------------------
           05  RCD11L PIC S9(0004) COMP.
           05  RCD11F PIC  X(0001).
           05  FILLER REDEFINES RCD11F.
               10  RCD11A PIC  X(0001).
           05  RCD11I PIC  X(0004).
      *    -------------------------------
           05  RCD12L PIC S9(0004) COMP.
           05  RCD12F PIC  X(0001).
           05  FILLER REDEFINES RCD12F.
               10  RCD12A PIC  X(0001).
           05  RCD12I PIC  X(0004).
      *    -------------------------------
           05  CRTNT1L PIC S9(0004) COMP.
           05  CRTNT1F PIC  X(0001).
           05  FILLER REDEFINES CRTNT1F.
               10  CRTNT1A PIC  X(0001).
           05  CRTNT1I PIC  X(0063).
      *    -------------------------------
           05  CRTNT2L PIC S9(0004) COMP.
           05  CRTNT2F PIC  X(0001).
           05  FILLER REDEFINES CRTNT2F.
               10  CRTNT2A PIC  X(0001).
           05  CRTNT2I PIC  X(0063).
      *    -------------------------------
           05  BILNT1L PIC S9(0004) COMP.
           05  BILNT1F PIC  X(0001).
           05  FILLER REDEFINES BILNT1F.
               10  BILNT1A PIC  X(0001).
           05  BILNT1I PIC  X(0063).
      *    -------------------------------
           05  BILNT2L PIC S9(0004) COMP.
           05  BILNT2F PIC  X(0001).
           05  FILLER REDEFINES BILNT2F.
               10  BILNT2A PIC  X(0001).
           05  BILNT2I PIC  X(0063).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0075).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0075).
      *    -------------------------------
           05  PFENTRL PIC S9(0004) COMP.
           05  PFENTRF PIC  X(0001).
           05  FILLER REDEFINES PFENTRF.
               10  PFENTRA PIC  X(0001).
           05  PFENTRI PIC  9(2).
       01  EL631JO REDEFINES EL631JI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOSTO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SYSO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPANYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATASORO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BATCHNOO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQNOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHNOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQNOO PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SIGREQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LTRIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENCCODEO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD01O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD02O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD03O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD04O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD05O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD06O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD07O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD08O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD09O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCD12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTNT1O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTNT2O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILNT1O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILNT2O PIC  X(0063).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTRO PIC  X(0002).
      *    -------------------------------
       01  filler redefines el631ji.
121713     12  filler                   pic x(188).
062712     12  REASONCDS.
062712         16  filler occurs 12.
062712             20  reacdl       pic s9(4) comp.
062712             20  reacda       pic x.
062712             20  reacd-in     pic x(4).
102212    12  FILLER                   PIC X(293).
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
       01  DFHCOMMAREA                 PIC X(1300).
       01  var  pic x(30).
      *                                COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
      *                                COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
      *                                COPY ERCPNDB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
071211* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
073114* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
010517* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
012220* 012220  CR2018092700002  TANA ADD LETTER REQUIRED FIELD
122002******************************************************************
00032
00033  01  PENDING-BUSINESS.
00034      12  PB-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  PB-CONTROL-PRIMARY.
00038          16  PB-COMPANY-CD                PIC X.
00039          16  PB-ENTRY-BATCH               PIC X(6).
00040          16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  PB-CONTROL-BY-ACCOUNT.
00044          16  PB-COMPANY-CD-A1             PIC X.
00045          16  PB-CARRIER                   PIC X.
00046          16  PB-GROUPING.
00047              20  PB-GROUPING-PREFIX       PIC XXX.
00048              20  PB-GROUPING-PRIME        PIC XXX.
00049          16  PB-STATE                     PIC XX.
00050          16  PB-ACCOUNT.
00051              20  PB-ACCOUNT-PREFIX        PIC X(4).
00052              20  PB-ACCOUNT-PRIME         PIC X(6).
00053          16  PB-CERT-EFF-DT               PIC XX.
00054          16  PB-CERT-NO.
00055              20  PB-CERT-PRIME            PIC X(10).
00056              20  PB-CERT-SFX              PIC X.
00057          16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  PB-RECORD-TYPE               PIC X.
00060              88  PB-MAILING-DATA                VALUE '0'.
00061              88  PB-ISSUE                       VALUE '1'.
00062              88  PB-CANCELLATION                VALUE '2'.
00063              88  PB-BATCH-TRAILER               VALUE '9'.
00064
00065      12  PB-CONTROL-BY-ORIG-BATCH.
00066          16  PB-ORIGINAL-COMPANY-CD       PIC X.
00067          16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  PB-CONTROL-BY-CSR.
00072          16  PB-CSR-COMPANY-CD            PIC X.
00073          16  PB-CSR-ID                    PIC X(4).
00074          16  PB-CSR-ENTRY-BATCH           PIC X(6).
00075          16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  PB-LAST-MAINT-DT                 PIC XX.
00082      12  PB-LAST-MAINT-BY                 PIC X(4).
00083      12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  PB-RECORD-BODY                   PIC X(375).
00086
00087      12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
00088          16  PB-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  PB-I-NAME.
00091              20  PB-I-INSURED-LAST-NAME   PIC X(15).
00092              20  PB-I-INSURED-FIRST-NAME.
00093                  24  PB-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  PB-I-INSURED-MIDDLE-INIT PIC X.
00096          16  PB-I-AGE                     PIC S99   COMP-3.
00097          16  PB-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  PB-I-BIRTHDAY                PIC XX.
00099          16  PB-I-INSURED-SEX             PIC X.
00100              88  PB-SEX-MALE     VALUE 'M'.
00101              88  PB-SEX-FEMALE   VALUE 'F'.
00102
00103          16  PB-I-LF-TERM                 PIC S999   COMP-3.
00104          16  PB-I-AH-TERM                 PIC S999   COMP-3.
00105          16  PB-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  PB-I-SKIP-CODE               PIC X.
00108              88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  PB-SKIP-JULY              VALUE '1'.
00110              88  PB-SKIP-AUGUST            VALUE '2'.
00111              88  PB-SKIP-SEPTEMBER         VALUE '3'.
00112              88  PB-SKIP-JULY-AUG          VALUE '4'.
00113              88  PB-SKIP-AUG-SEPT          VALUE '5'.
00114              88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  PB-SKIP-JUNE              VALUE '8'.
00117              88  PB-SKIP-JUNE-JULY         VALUE '9'.
00118              88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  PB-I-TERM-TYPE               PIC X.
00121              88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  PB-PAID-WEEKLY            VALUE 'W'.
00123              88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  PB-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  PB-PAID-13-YEARLY         VALUE 'T'.
00126          16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  PB-I-POLICY-FORM-NO          PIC X(12).
00128          16  PB-I-DATA-ENTRY-SW           PIC X.
00129              88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
012220         16  PB-I-LETTER-REQD             PIC X.
00136
00137          16  PB-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  PB-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  PB-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  PB-I-AMOUNT-FINANCED REDEFINES
100703                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  PB-I-UNPAID-CASH-PRICE REDEFINES
100703                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  PB-I-CLP-AMOUNT REDEFINES
100703                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  PB-I-LF-CALC-FLAG            PIC X.
00148              88 PB-COMP-LF-PREM               VALUE '?'.
00149          16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  PB-I-LF-ABBR                 PIC XXX.
00157          16  PB-I-LF-INPUT-CD             PIC XX.
00158
00159          16  PB-I-AH-BENEFIT-CD           PIC XX.
00160              88  PB-VALID-AH                 VALUE '01' THRU '89'.
00161              88  PB-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  PB-I-AH-CALC-FLAG            PIC X.
00166              88 PB-COMP-AH-PREM                  VALUE '?'.
00167          16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
010716         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
00170          16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  PB-I-AH-ABBR                 PIC XXX.
00173          16  PB-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  PB-I-SPECIAL-REIN-CODE       PIC X.
00176          16  PB-I-REIN-TABLE              PIC XXX.
00177          16  PB-I-BUSINESS-TYPE           PIC 99.
00178          16  PB-I-INDV-GRP-CD             PIC X.
00179          16  PB-I-MORT-CODE.
00180              20  PB-I-TABLE               PIC X.
00181              20  PB-I-INTEREST            PIC XX.
00182              20  PB-I-MORT-TYP            PIC X.
00183          16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  PB-I-INDV-GRP-OVRD           PIC X.
00187          16  PB-I-RATE-CLASS-OVRD         PIC XX.
00188          16  PB-I-SIG-SW                  PIC X.
00189              88  PB-POLICY-SIGNED             VALUE 'Y'.
00190          16  PB-I-RATE-CLASS              PIC XX.
00191          16  PB-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  PB-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  PB-I-BENEFIT-TYPE            PIC XXX.
00199          16  PB-I-OB-FLAG                 PIC X.
00200              88  PB-I-OB                      VALUE 'B'.
00201              88  PB-I-SUMMARY                 VALUE 'Z'.
00202          16  PB-I-ENTRY-STATUS            PIC X.
00203              88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  PB-I-NORMAL-ENTRY            VALUE '1'.
00206              88  PB-I-POLICY-PENDING          VALUE '2'.
00207              88  PB-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
                   88  PB-I-POLICY-IS-CASH          VALUE 'C'.
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  PB-I-REIN-ONLY               VALUE '9'.
00210              88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  PB-I-INT-CODE                PIC X.
00215              88  PB-ADD-ON-INTEREST           VALUE 'A'.
00216              88  PB-SIMPLE-INTEREST           VALUE 'S'.
00217          16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  PB-I-SOC-SEC-NO              PIC X(11).
00219          16  PB-I-MEMBER-NO               PIC X(12).
00220          16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  PB-I-OLD-LOF                 PIC XXX.
00222          16  PB-I-LF-EXPIRE-DT            PIC XX.
00223          16  PB-I-AH-EXPIRE-DT            PIC XX.
00224          16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  PB-I-LIFE-INDICATOR          PIC X.
00227              88  PB-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  PB-I-LIVES                   PIC S9(7)       COMP-3.
071211         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
071211                                          PIC S9(5)V99    COMP-3.
00229          16  PB-I-MAIL-ADDRS-SW           PIC X.
00230              88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  PB-I-1ST-PMT-DT              PIC XX.
00233          16  PB-I-JOINT-INSURED.
00234              20 PB-I-JOINT-LAST-NAME      PIC X(15).
00235              20 PB-I-JOINT-FIRST-NAME.
00236                 24  PB-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 PB-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  PB-I-BENEFICIARY-NAME.
100703             20  PB-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  PB-I-LAST-ADD-ON-DT          PIC XX.
011904         16  PB-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES PB-I-REFERENCE.
011904             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  PB-I-CLP-STATE           PIC XX.
00242          16  PB-I-UNDERWRITING-STATUS     PIC X.
00243              88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  PB-I-POLICY-DECLINED         VALUE 'D'.
00245              88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  PB-I-RESIDENT-STATE          PIC XX.
00249          16  PB-I-RATE-CODE               PIC X(4).
00250          16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  PB-I-BANK-NOCHRGB            PIC 99.
040504         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  PB-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
00254          16  PB-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  PB-C-CANCEL-ORIGIN           PIC X.
00257              88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  PB-C-LF-CANCEL-DT            PIC XX.
00259          16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  PB-C-LF-CALC-REQ             PIC X.
00261              88 PB-COMP-LF-CANCEL            VALUE '?'.
00262          16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  PB-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  PB-C-AH-CANCEL-DT            PIC XX.
00267          16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  PB-C-AH-CALC-REQ             PIC X.
00269              88 PB-COMP-AH-CANCEL            VALUE '?'.
00270          16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  PB-C-LAST-NAME               PIC X(15).
00273          16  PB-C-REFUND-SW               PIC X.
00274              88  PB-C-REFUND-CREATED          VALUE 'Y'.
00275              88  PB-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  PB-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  PB-C-PAYEE-CODE              PIC X(6).
00278          16  PB-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  PB-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  PB-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  PB-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  PB-C-REFERENCE               PIC X(12).
PEMMOD         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  PB-C-POST-CARD-IND           PIC X.
081606         16  PB-C-CANCEL-REASON           PIC X.
072308         16  PB-C-REF-INTERFACE-SW        PIC X.
071211         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
071211         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
00283          16  FILLER                       PIC X(01).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  PB-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
062017         16  PB-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
00286          16  PB-CANCELED-CERT-DATA.
00287              20  PB-CI-INSURED-NAME.
00288                  24  PB-CI-LAST-NAME      PIC X(15).
00289                  24  PB-CI-INITIALS       PIC XX.
00290              20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  PB-CI-INSURED-SEX        PIC X.
00292              20  PB-CI-LF-TERM            PIC S999        COMP-3.
00293              20  PB-CI-LF-BENEFIT-CD      PIC XX.
00294              20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  PB-CI-AH-TERM            PIC S999        COMP-3.
00299              20  PB-CI-AH-BENEFIT-CD      PIC XX.
00300              20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  PB-CI-RATE-CLASS         PIC XX.
00303              20  PB-CI-RATE-DEV-LF        PIC XXX.
00304              20  PB-CI-RATE-DEV-AH        PIC XXX.
00305              20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  PB-CI-LF-ABBR            PIC X(3).
00310              20  PB-CI-AH-ABBR            PIC X(3).
00311              20  PB-CI-OB-FLAG            PIC X.
00312                  88  PB-CI-OB                VALUE 'B'.
00313              20  PB-CI-LF-POLICY-STATUS   PIC X.
00314                  88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  PB-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  PB-CI-AH-POLICY-STATUS   PIC X.
00328                  88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  PB-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  PB-CI-PAY-FREQUENCY      PIC 99.
00342              20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  PB-CI-SOC-SEC-NO         PIC X(11).
00344              20  PB-CI-MEMBER-NO          PIC X(12).
00345              20  PB-CI-INT-CODE           PIC X.
00346                  88  PB-CI-ADD-ON                  VALUE 'A'.
00347                  88  PB-CI-SIMPLE                  VALUE 'S'.
00348              20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  PB-CI-COMP-EXCP-SW       PIC X.
00351                  88  PB-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  PB-CI-ENTRY-STATUS       PIC X.
00354              20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  PB-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  PB-CI-DEATH-DT           PIC XX.
00358              20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  PB-CI-ENTRY-DT              PIC XX.
00365              20  PB-CI-ENTRY-BATCH           PIC X(6).
00366              20  PB-CI-LF-EXPIRE-DT          PIC XX.
00367              20  PB-CI-AH-EXPIRE-DT          PIC XX.
00368              20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  PB-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  PB-CI-INDV-GRP-CD           PIC X.
100703             20  PB-CI-BENEFICIARY-NAME.
100703                 24  PB-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  PB-CI-NOTE-SW               PIC X.
00377              20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  PB-CI-LOAN-OFFICER          PIC X(5).
032306             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  PB-CI-FIRST-NAME            PIC X(10).
071211             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
00380
072209         16  FILLER                       PIC X(13).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  PB-M-INSURED-LAST-NAME       PIC X(15).
00386          16  PB-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  PB-M-INSURED-MID-INIT        PIC X.
00388          16  PB-M-INSURED-AGE             PIC 99.
00389          16  PB-M-INSURED-BIRTHDAY        PIC XX.
00390          16  PB-M-INSURED-SEX             PIC X.
00391          16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  PB-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  PB-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  PB-M-INSURED-CITY-STATE.
051810             20  PB-M-INSURED-CITY        PIC X(28).
051810             20  PB-M-INSURED-STATE       PIC XX.
00395          16  PB-M-INSURED-ZIP-CODE.
00396              20  PB-M-INSURED-ZIP-PRIME.
00397                  24  PB-M-INSURED-ZIP-1   PIC X.
00398                      88  PB-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         PB-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  PB-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  PB-M-JOINT-BIRTHDAY          PIC XX.
               16  PB-M-CRED-BENE-NAME          PIC X(30).
               16  PB-M-CRED-BENE-ADDR1         PIC X(30).
               16  PB-M-CRED-BENE-ADDR2         PIC X(30).
               16  PB-M-CRED-BENE-CITYST.
                   20  PB-M-CRED-BENE-CITY      PIC X(28).
                   20  PB-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  PB-ACCOUNT-NAME              PIC X(30).
00430          16  PB-PREM-REF-RPT-FLAG         PIC X.
00431          16  PB-REFERENCE                 PIC X(12).
00432          16  PB-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  PB-RECORD-STATUS.
00436          16  PB-CREDIT-SELECT-DT          PIC XX.
00437          16  PB-CREDIT-ACCEPT-DT          PIC XX.
00438          16  PB-BILLED-DT                 PIC XX.
00439          16  PB-BILLING-STATUS            PIC X.
00440              88  PB-ENTRY-REVERSED            VALUE 'R'.
00441              88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  PB-RECORD-BILL               PIC X.
00444              88  PB-RECORD-ON-HOLD            VALUE 'H'.
00445              88  PB-RECORD-RETURNED           VALUE 'R'.
00446              88  PB-RECORD-ENDORSED           VALUE 'E'.
00447              88  PB-OVERRIDE-LIFE             VALUE 'L'.
00448              88  PB-OVERRIDE-AH               VALUE 'A'.
00449              88  PB-OVERRIDE-BOTH             VALUE 'B'.
00450          16  PB-BATCH-ENTRY               PIC X.
00451              88  PB-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  PB-REIN-ONLY-CERT            VALUE 'R'.
00453              88  PB-REISSUED-CERT             VALUE 'E'.
                   88  PB-CASH-CERT                 VALUE 'C'.
122002             88  PB-MONTHLY-CERT              VALUE 'M'.
00454              88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  PB-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  PB-FORCE-CODE                PIC X.
00458              88  PB-FORCE-OFF                 VALUE ' ' '0'.
00459              88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  PB-CANCEL-FORCE              VALUE '8'.
00461              88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  PB-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
010517             88  PB-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
073107             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  PB-FATAL-FLAG                PIC X.
00468              88  PB-FATAL-ERRORS              VALUE 'X'.
00469          16  PB-FORCE-ER-CD               PIC X.
00470              88  PB-FORCE-ERRORS              VALUE 'F'.
00471              88  PB-UNFORCED-ERRORS           VALUE 'X'.
00472          16  PB-WARN-ER-CD                PIC X.
00473              88  PB-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  PB-OUT-BAL-CD                PIC X.
00476              88  PB-OUT-OF-BAL                VALUE 'O'.
00477          16  PB-LIFE-OVERRIDE-L1          PIC X.
00478          16  PB-AH-OVERRIDE-L1            PIC X.
00479          16  PB-INPUT-DT                  PIC XX.
00480          16  PB-INPUT-BY                  PIC X(4).
00481          16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  PB-TOLERANCE-REJECT-SW       PIC X.
00484          16  PB-LF-EARNING-METHOD         PIC X.
00485          16  PB-AH-EARNING-METHOD         PIC X.
00486          16  PB-LF-TERM-CALC-METHOD       PIC X.
00487          16  PB-AH-TERM-CALC-METHOD       PIC X.
00488          16  PB-REIN-CD                   PIC XXX.
00489          16  PB-LF-REFUND-TYPE            PIC X.
00490          16  PB-AH-REFUND-TYPE            PIC X.
00491          16  PB-ACCT-EFF-DT               PIC XX.
00492          16  PB-ACCT-EXP-DT               PIC XX.
00493          16  PB-COMPANY-ID                PIC X(3).
00494          16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  PB-SV-CARRIER                PIC X.
00497          16  PB-SV-GROUPING               PIC X(6).
00498          16  PB-SV-STATE                  PIC XX.
00499          16  PB-CONFIRMATION-REPT-DT      PIC XX.
00500          16  PB-GA-BILLING-INFO.
00501              20  PB-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  PB-SV-REMIT-TO  REDEFINES
00504              PB-GA-BILLING-INFO           PIC X(10).
00505          16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  PB-I-LOAN-OFFICER            PIC X(5).
081606         16  PB-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  PB-COMMON-ERRORS.
00514          16  PB-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
      *                                COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
      *                                COPY ELCCRTO.
      ******************************************************************
      *                                                                *
      *                            ELCCRTO.                            *
      *                                                                *
      *   FILE DESCRIPTION = ORIGINAL CERTIFICATE INFORMATION          *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 524  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ELCRTO                         RKP=2,LEN=36   *
      *                                                                *
      *   LOG = YES                                                    *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
061011* 061011  2011022800001    PEMA  NEW FILE TO SAVE ORIG CERT INFO
062712* 062712  2011022800001    AJRA  REDEFINE ORIG DATA
071712* 071712  CR2011022800001  AJRA  NAPERSOFT CANCELS
121812* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
      ******************************************************************
       01  ORIGINAL-CERTIFICATE.
           12  OC-RECORD-ID                      PIC XX.
               88  VALID-OC-ID                      VALUE 'OC'.
           12  OC-CONTROL-PRIMARY.
               16  OC-COMPANY-CD                 PIC X.
               16  OC-CARRIER                    PIC X.
               16  OC-GROUPING                   PIC X(6).
               16  OC-STATE                      PIC XX.
               16  OC-ACCOUNT                    PIC X(10).
               16  OC-CERT-EFF-DT                PIC XX.
               16  OC-CERT-NO.
                   20  OC-CERT-PRIME             PIC X(10).
                   20  OC-CERT-SFX               PIC X.
               16  OC-RECORD-TYPE                PIC X.
               16  OC-KEY-SEQ-NO                 PIC 9(4) BINARY.
           12  OC-LAST-MAINT-DT                  PIC XX.
           12  OC-LAST-MAINT-BY                  PIC X(4).
           12  OC-LAST-MAINT-HHMMSS              PIC S9(6)   COMP-3.
           12  OC-ENDORSEMENT-PROCESSED-DT       PIC XX.
           12  FILLER                            PIC X(49).
062712     12  OC-ORIG-REC.
062712         16  OC-INS-LAST-NAME              PIC X(15).
062712         16  OC-INS-FIRST-NAME             PIC X(10).
062712         16  OC-INS-MIDDLE-INIT            PIC X.
062712         16  OC-INS-AGE                    PIC S999     COMP-3.
062712         16  OC-JNT-LAST-NAME              PIC X(15).
062712         16  OC-JNT-FIRST-NAME             PIC X(10).
062712         16  OC-JNT-MIDDLE-INIT            PIC X.
062712         16  OC-JNT-AGE                    PIC S999     COMP-3.
062712         16  OC-LF-BENCD                   PIC XX.
062712         16  OC-LF-TERM                    PIC S999      COMP-3.
062712         16  OC-LF-BEN-AMT                 PIC S9(9)V99  COMP-3.
062712         16  OC-LF-PRM-AMT                 PIC S9(7)V99  COMP-3.
062712         16  OC-LF-ALT-BEN-AMT             PIC S9(9)V99  COMP-3.
062712         16  OC-LF-ALT-PRM-AMT             PIC S9(7)V99  COMP-3.
062712         16  OC-LF-EXP-DT                  PIC XX.
062712         16  OC-LF-COMM-PCT                PIC SV9(5)    COMP-3.
062712         16  OC-LF-CANCEL-DT               PIC XX.
062712         16  OC-LF-CANCEL-AMT              PIC S9(7)V99  COMP-3.
071712         16  OC-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
062712         16  OC-AH-BENCD                   PIC XX.
062712         16  OC-AH-TERM                    PIC S999      COMP-3.
062712         16  OC-AH-BEN-AMT                 PIC S9(9)V99  COMP-3.
062712         16  OC-AH-PRM-AMT                 PIC S9(7)V99  COMP-3.
062712         16  OC-AH-EXP-DT                  PIC XX.
062712         16  OC-AH-COMM-PCT                PIC SV9(5)    COMP-3.
062712         16  OC-AH-CP                      PIC 99.
062712         16  OC-AH-CANCEL-DT               PIC XX.
062712         16  OC-AH-CANCEL-AMT              PIC S9(7)V99  COMP-3.
071712         16  OC-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
062712         16  OC-CRED-BENE-NAME             PIC X(25).
062712         16  OC-1ST-PMT-DT                 PIC XX.
121812         16  OC-INS-AGE-DEFAULT-FLAG       PIC X.
121812         16  OC-JNT-AGE-DEFAULT-FLAG       PIC X.
011413         16  OC-ISSUE-TRAN-IND             PIC X.
011413         16  OC-CANCEL-TRAN-IND            PIC X.
011413         16  FILLER                        PIC X(211).
062712
062712     12  FILLER                            PIC X(50).
      ******************************************************************
      *                                COPY ERCENDT.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCENDT.                            *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = ENDORSEMENT FILE                          *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
PEMMOD*   RECORD SIZE = 579  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ERENDT                         RKP=02,LEN=36  *
      *       ALTERNATE PATH1 = ERENDT2 (BY ARCH NO)    RKP=38,LEN=05  *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
052307*-----------------------------------------------------------------
052307*                   C H A N G E   L O G
052307*
052307* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052307*-----------------------------------------------------------------
052307*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052307* EFFECTIVE    NUMBER
052307*-----------------------------------------------------------------
052307* 052307    2006052600001  AJRA  ADDED FLAG FOR CANCELS ON CERTS
052307*                                WITH OPEN CLAIMS
072312* 072312    2011022800001  AJRA  ADDED BATCH NUMBER
110612* 110612    2012101700002  AJRA  ADD NEW FIELDS
121812* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
010616* 010616  CR2015082000001  PEMA  ADD ENDORSEMENT CHECK PROCESSING
052307*-----------------------------------------------------------------
       01  ENDORSEMENT-RECORD.
           12  EN-RECORD-ID                PIC XX.
               88  VALID-EN-ID                VALUE 'EN'.
           12  EN-CONTROL-PRIMARY.
               16  EN-COMPANY-CD           PIC X.
               16  EN-CARRIER              PIC X.
               16  EN-GROUPING             PIC X(6).
               16  EN-STATE                PIC XX.
               16  EN-ACCOUNT              PIC X(10).
               16  EN-CERT-EFF-DT          PIC XX.
               16  EN-CERT-NO.
                   20  EN-CERT-PRIME       PIC X(10).
                   20  EN-CERT-SFX         PIC X.
               16  EN-REC-TYPE             PIC X.
                   88  EN-ISSUE               VALUE 'I'.
                   88  EN-CANCELLATION        VALUE 'C'.
               16  EN-SEQ-NO               PIC 9(04) BINARY.
           12  EN-CONTROL-BY-ARCH-NO.
               16  EN-COMPANY-CD-A1              PIC X.
               16  EN-ARCHIVE-NO                 PIC 9(8) BINARY.
           12  EN-LAST-MAINT-DT            PIC XX.
           12  EN-LAST-MAINT-BY            PIC X(4).
           12  EN-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
           12  EN-ENDORSEMENT-RECORD       PIC X(329).
           12  EN-ISSUE-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
               16  EN-INS-ORIG-LAST-NAME   PIC X(15).
               16  EN-INS-ORIG-FIRST-NAME  PIC X(10).
               16  EN-INS-ORIG-MIDDLE-INIT PIC X.
               16  EN-INS-ORIG-AGE         PIC S999     COMP-3.
               16  EN-JNT-ORIG-LAST-NAME   PIC X(15).
               16  EN-JNT-ORIG-FIRST-NAME  PIC X(10).
               16  EN-JNT-ORIG-MIDDLE-INIT PIC X.
               16  EN-JNT-ORIG-AGE         PIC S999     COMP-3.
               16  EN-LF-ORIG-BENCD        PIC XX.
               16  EN-LF-ORIG-TERM         PIC S999      COMP-3.
               16  EN-LF-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
               16  EN-LF-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
               16  EN-LF-ORIG-ALT-BEN-AMT  PIC S9(9)V99  COMP-3.
               16  EN-LF-ORIG-ALT-PRM-AMT  PIC S9(7)V99  COMP-3.
               16  EN-LF-ORIG-EXP-DT       PIC XX.
      *        16  EN-LF-ORIG-COV-TYPE     PIC X(10).
               16  EN-ORIG-CRED-BENE       PIC X(25).
               16  EN-LF-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
               16  FILLER                  PIC X.
               16  EN-AH-ORIG-BENCD        PIC XX.
               16  EN-AH-ORIG-TERM         PIC S999      COMP-3.
               16  EN-AH-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
               16  EN-AH-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
               16  EN-AH-ORIG-EXP-DT       PIC XX.
      *        16  EN-AH-ORIG-COV-TYPE     PIC X(10).
      *        16  EN-AH-ORIG-WAIT-PER     PIC 99.
               16  EN-AH-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
               16  F                       PIC X(09).
               16  EN-AH-ORIG-CP           PIC 99.
               16  EN-INS-NEW-LAST-NAME    PIC X(15).
               16  EN-INS-NEW-FIRST-NAME   PIC X(10).
               16  EN-INS-NEW-MIDDLE-INIT  PIC X.
               16  EN-INS-NEW-AGE          PIC S999     COMP-3.
               16  EN-JNT-NEW-LAST-NAME    PIC X(15).
               16  EN-JNT-NEW-FIRST-NAME   PIC X(10).
               16  EN-JNT-NEW-MIDDLE-INIT  PIC X.
               16  EN-JNT-NEW-AGE          PIC S999     COMP-3.
               16  EN-LF-NEW-BENCD         PIC XX.
               16  EN-LF-NEW-TERM          PIC S999      COMP-3.
               16  EN-LF-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
               16  EN-LF-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
               16  EN-LF-NEW-ALT-BEN-AMT   PIC S9(9)V99  COMP-3.
               16  EN-LF-NEW-ALT-PRM-AMT   PIC S9(7)V99  COMP-3.
               16  EN-LF-NEW-EXP-DT        PIC XX.
               16  EN-NEW-CRED-BENE        PIC X(25).
               16  EN-LF-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
               16  FILLER                  PIC X.
               16  EN-AH-NEW-BENCD         PIC XX.
               16  EN-AH-NEW-TERM          PIC S999      COMP-3.
               16  EN-AH-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
               16  EN-AH-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
               16  EN-AH-NEW-EXP-DT        PIC XX.
      *        16  EN-AH-NEW-COV-TYPE      PIC X(10).
      *        16  EN-AH-NEW-WAIT-PER      PIC 99.
      *        16  F                       PIC X(12).
               16  EN-AH-NEW-CP            PIC 99.
               16  EN-SIG-SW               PIC X.
               16  EN-AH-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
121812         16  EN-INS-ORIG-AGE-DEF-FLAG PIC X.
121812         16  EN-JNT-ORIG-AGE-DEF-FLAG PIC X.
121812         16  EN-INS-NEW-AGE-DEF-FLAG PIC X.
121812         16  EN-JNT-NEW-AGE-DEF-FLAG PIC X.
121812         16  FILLER                  PIC X(33).
           12  EN-CANCEL-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
               16  EN-LF-ORIG-REF-DT       PIC XX.
               16  EN-LF-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
               16  EN-AH-ORIG-REF-DT       PIC XX.
               16  EN-AH-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
               16  EN-LF-NEW-REF-DT        PIC XX.
               16  EN-LF-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
               16  EN-AH-NEW-REF-DT        PIC XX.
               16  EN-AH-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
               16  EN-FLAG-CERT            PIC X.
               16  EN-INS-LAST-NAME        PIC X(15).
               16  EN-INS-FIRST-NAME       PIC X(10).
               16  EN-INS-MIDDLE-INIT      PIC X.
110612         16  EN-LF-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
110612         16  EN-AH-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
110612         16  EN-LF-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
110612         16  EN-AH-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
110612         16  FILLER                  PIC X(262).
           12  EN-MONEY-SW             PIC X.
           12  EN-HEALTH-APP           PIC X.
           12  EN-VOUCHER-SW           PIC X.
           12  EN-PAYEE                PIC X(14).
           12  EN-INPUT-DT             PIC XX.
           12  EN-PROCESS-DT           PIC XX.
           12  EN-LF-COMMISSION        PIC SV9(5)    COMP-3.
           12  EN-AH-COMMISSION        PIC SV9(5)    COMP-3.
           12  EN-REASON-CODES.
               16  F OCCURS 12.
                   20  EN-REASON-CODE  PIC X(4).
           12  EN-TEMPLATE-USED        PIC X(8).
           12  EN-DOCU-TYPE            PIC X.
               88  EN-VERI-DOCU          VALUE 'V'.
               88  EN-GCE-DOCU           VALUE 'G'.
               88  EN-CANC-DOCU          VALUE 'C'.
           12  EN-COMMENTS1            PIC X(13).
           12  EN-COMMENTS2            PIC X(70).
           12  EN-COMM-CHGBK           PIC X.
               88  EN-DO-NOT-CHG-ACCT    VALUE 'N'.
               88  EN-CHG-ACCT           VALUE 'Y'.
           12  EN-CSO-PORTION          PIC S9(5)V99  COMP-3.
           12  EN-ACCT-PORTION         PIC S9(5)V99  COMP-3.
072312     12  EN-BATCH-NUMBER         PIC X(6).
072312     12  EN-ACCT-SUMM            PIC X.
072312     12  EN-CSO-SUMM             PIC X.
010616     12  en-check-type           pic x.
072312     12  FILLER                  PIC X(12).
      ******************************************************************
      *                                COPY ERCNOTE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCNOTE                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE AND BILLING NOTES        *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 825    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERNOTE        RKP=2,LEN=34               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  CERT NOTES MOVED TO NEW FILE. THI
091509*                                FILE WILL CONTAIN BILLING NOTES O
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
00017 ******************************************************************
00018
00019  01  CERTIFICATE-NOTE.
00020      12  CN-RECORD-ID                PIC  XX.
00021          88  VALID-CN-ID                  VALUE 'CN'.
00022
00023      12  CN-CONTROL-PRIMARY.
00024          16  CN-COMPANY-CD           PIC X.
00025          16  CN-CARRIER              PIC X.
00026          16  CN-GROUPING.
00027              20 CN-GROUPING-PREFIX   PIC XXX.
00028              20 CN-GROUPING-PRIME    PIC XXX.
00029          16  CN-STATE                PIC XX.
00030          16  CN-ACCOUNT.
00031              20 CN-ACCOUNT-PREFIX    PIC X(4).
00032              20 CN-ACCOUNT-PRIME     PIC X(6).
00033          16  CN-CERT-EFF-DT          PIC XX.
00034          16  CN-CERT-NO.
00035              20  CN-CERT-PRIME       PIC X(10).
00036              20  CN-CERT-SFX         PIC X.
041320         16  CN-RECORD-TYPE          PIC X.
041320             88  CN-ISSUE-BILLING-NOTE    VALUE '1'.
041320             88  CN-CANCEL-BILLING-NOTE   VALUE '2'.
00038      12  CN-BILLING-START-LINE-NO    PIC 99.
00039      12  CN-BILLING-END-LINE-NO      PIC 99.
00040
00041      12  CN-LINES.
00042          16  CN-LINE OCCURS 10       PIC X(77).
00043
00044      12  CN-LAST-MAINT-DT            PIC XX.
00045      12  CN-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00046      12  CN-LAST-MAINT-USER          PIC X(4).
041320     12  FILLER                      PIC X(5).
00048 ******************************************************************
      *                                COPY ERCMAIL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCMAIL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
111108* 111108                   PEMA  ADD CRED BENE ADDR2
00017 ******************************************************************
00018
00019  01  MAILING-DATA.
00020      12  MA-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'MA'.
00022
00023      12  MA-CONTROL-PRIMARY.
00024          16  MA-COMPANY-CD                 PIC X.
00025          16  MA-CARRIER                    PIC X.
00026          16  MA-GROUPING.
00027              20  MA-GROUPING-PREFIX        PIC XXX.
00028              20  MA-GROUPING-PRIME         PIC XXX.
00029          16  MA-STATE                      PIC XX.
00030          16  MA-ACCOUNT.
00031              20  MA-ACCOUNT-PREFIX         PIC X(4).
00032              20  MA-ACCOUNT-PRIME          PIC X(6).
00033          16  MA-CERT-EFF-DT                PIC XX.
00034          16  MA-CERT-NO.
00035              20  MA-CERT-PRIME             PIC X(10).
00036              20  MA-CERT-SFX               PIC X.
00037
00038      12  FILLER                            PIC XX.
00039
00040      12  MA-ACCESS-CONTROL.
00041          16  MA-SOURCE-SYSTEM              PIC XX.
00042              88  MA-FROM-CREDIT                VALUE 'CR'.
00043              88  MA-FROM-VSI                   VALUE 'VS'.
00044              88  MA-FROM-WARRANTY              VALUE 'WA'.
00045              88  MA-FROM-OTHER                 VALUE 'OT'.
00046          16  MA-RECORD-ADD-DT              PIC XX.
00047          16  MA-RECORD-ADDED-BY            PIC XXXX.
00048          16  MA-LAST-MAINT-DT              PIC XX.
00049          16  MA-LAST-MAINT-BY              PIC XXXX.
00050          16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00051
00052      12  MA-PROFILE-INFO.
00053          16  MA-QUALIFY-CODE-1             PIC XX.
00054          16  MA-QUALIFY-CODE-2             PIC XX.
00055          16  MA-QUALIFY-CODE-3             PIC XX.
00056          16  MA-QUALIFY-CODE-4             PIC XX.
00057          16  MA-QUALIFY-CODE-5             PIC XX.
00058
00059          16  MA-INSURED-LAST-NAME          PIC X(15).
00060          16  MA-INSURED-FIRST-NAME         PIC X(10).
00061          16  MA-INSURED-MIDDLE-INIT        PIC X.
00062          16  MA-INSURED-ISSUE-AGE          PIC 99.
00063          16  MA-INSURED-BIRTH-DT           PIC XX.
00064          16  MA-INSURED-SEX                PIC X.
00065              88  MA-SEX-MALE                   VALUE 'M'.
00066              88  MA-SEX-FEMALE                 VALUE 'F'.
00067          16  MA-INSURED-SOC-SEC-NO         PIC X(11).
00068
080406         16  MA-ADDRESS-CORRECTED          PIC X.
081108         16  MA-JOINT-BIRTH-DT             PIC XX.
00069 *        16  FILLER                        PIC X(12).
00070
00071          16  MA-ADDRESS-LINE-1             PIC X(30).
00072          16  MA-ADDRESS-LINE-2             PIC X(30).
00073          16  MA-CITY-STATE.
                   20  MA-CITY                   PIC X(28).
                   20  MA-ADDR-STATE             PIC XX.
00074          16  MA-ZIP.
00075              20  MA-ZIP-CODE.
00076                  24  MA-ZIP-CODE-1ST       PIC X(1).
00077                      88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00078                  24  FILLER                PIC X(4).
00079              20  MA-ZIP-PLUS4              PIC X(4).
00080          16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
00081              20  MA-CAN-POSTAL-CODE-1      PIC X(3).
00082              20  MA-CAN-POSTAL-CODE-2      PIC X(3).
00083              20  FILLER                    PIC X(3).
00084
00085          16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
00086
               16  FILLER                        PIC XXX.
00087 *        16  FILLER                        PIC X(10).
00088
           12  MA-CRED-BENE-INFO.
CIDMOD         16  MA-CRED-BENE-NAME                 PIC X(25).
CIDMOD         16  MA-CRED-BENE-ADDR                 PIC X(30).
               16  MA-CRED-BENE-ADDR2                PIC X(30).
CIDMOD         16  MA-CRED-BENE-CTYST.
                   20  MA-CRED-BENE-CITY             PIC X(28).
                   20  MA-CRED-BENE-STATE            PIC XX.
CIDMOD         16  MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-ZIP-CODE.
CIDMOD                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
CIDMOD                     88  MA-CB-CANADIAN-POST-CODE
                                                 VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                    PIC X(4).
CIDMOD             20  MA-CB-ZIP-PLUS4               PIC X(4).
CIDMOD         16  MA-CB-CANADIAN-POSTAL-CODE
                                  REDEFINES MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
CIDMOD             20  FILLER                        PIC X(3).
080406     12  MA-POST-CARD-MAIL-DATA.
080406         16  MA-MAIL-DATA OCCURS 7.
080406             20  MA-MAIL-TYPE              PIC X.
080406                 88  MA-12MO-MAILING           VALUE '1'.
080406                 88  MA-EXP-MAILING            VALUE '2'.
080406             20  MA-MAIL-STATUS            PIC X.
080406                 88  MA-MAIL-ST-MAILED         VALUE '1'.
080406                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  MA-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC XX.
080406*    12  FILLER                            PIC X(30).
00090 ******************************************************************
      *                                COPY ERCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 250  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCH                        RKP=2,LEN=5     *
00013 *     ALTERNATE PATH1 = ERARCH2 (CERT/RESP)      RKP=07,LEN=35   *
00014 *     ALTERNATE PATH2 = ERARCH3 (FORM NUMBER)    RKP=44,LEN=28   *
00015 *     ALTERNATE PATH3 = ERARCH4 (PROCCESSOR ID)  RKP=73,LEN=28   *
00016 *     ALTERNATE PATH4 = ERARCH5 (ACCOUNT KEY)    RKP=100,LEN=24  *
00017 *     ALTERNATE PATH5 = ERARCH6 (BTCH/CHK KEY)   RKP=124,LEN=11  *
00018 *                                                                *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
031011*                   C H A N G E   L O G
031011*
031011* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031011*-----------------------------------------------------------------
031011*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031011* EFFECTIVE    NUMBER
031011*-----------------------------------------------------------------
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
070711* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
110612* 110612    2012101700002  AJRA  ADD NEW FIELDS
102918* 102918  CR2018080300002  PEMA  ADD ONBASE STUFF
031011******************************************************************
00021  01  LETTER-ARCHIVE.
00022      12  LA-RECORD-ID                PIC  X(02).
00023          88  LA-VALID-ID                VALUE 'LA'.
00024
00025      12  LA-CONTROL-PRIMARY.
00026          16  LA-COMPANY-CD           PIC  X(01).
00027          16  LA-ARCHIVE-NO           PIC S9(08)    COMP.
00028
00029      12  LA-CONTROL-BY-CERT-RESP.
00030          16  LA-COMPANY-CD-A2        PIC  X(01).
00031          16  LA-CERT-NO-A2.
00032              20  LA-CERT-PRIME-A2    PIC  X(10).
00033              20  LA-CERT-SUFFIX-A2   PIC  X(01).
00034          16  LA-RSP-PERSON-A2 REDEFINES LA-CERT-NO-A2.
00035              20  LA-RESP-PERSON-A2   PIC  X(10).
00036              20  LA-TYPE-A2          PIC  X(01).
00037          16  LA-CARRIER-A2           PIC  X(01).
00038          16  LA-GROUPING-A2          PIC  X(06).
00039          16  LA-STATE-A2             PIC  X(02).
00040          16  LA-ACCOUNT-A2           PIC  X(10).
00041          16  LA-EFFECT-DATE-A2       PIC  X(02).
00042          16  LA-ARCHIVE-NO-A2        PIC S9(08)    COMP.
00043
00044      12  LA-CONTROL-BY-FORM.
00045          16  LA-COMPANY-CD-A3        PIC  X(01).
00046          16  LA-FORM-A3              PIC  X(04).
00047          16  LA-CARRIER-A3           PIC  X(01).
00048          16  LA-GROUPING-A3          PIC  X(06).
00049          16  LA-STATE-A3             PIC  X(02).
00050          16  LA-ACCOUNT-A3           PIC  X(10).
00051          16  LA-ARCHIVE-NO-A3        PIC S9(08)    COMP.
00052
00053      12  LA-CONTROL-BY-PROCESSOR.
00054          16  LA-COMPANY-CD-A4        PIC  X(01).
00055          16  LA-PROCESSOR-CD         PIC  X(04).
00056          16  LA-CARRIER-A4           PIC  X(01).
00057          16  LA-GROUPING-A4          PIC  X(06).
00058          16  LA-STATE-A4             PIC  X(02).
00059          16  LA-ACCOUNT-A4           PIC  X(10).
00060          16  LA-ARCHIVE-NO-A4        PIC S9(08)    COMP.
00061
00062      12  LA-CONTROL-BY-KEY-FIELDS.
00063          16  LA-COMPANY-CD-A5        PIC  X(01).
00064          16  LA-CARRIER-A5           PIC  X(01).
00065          16  LA-GROUPING-A5          PIC  X(06).
00066          16  LA-STATE-A5             PIC  X(02).
00067          16  LA-ACCOUNT-A5           PIC  X(10).
00068          16  LA-ARCHIVE-NO-A5        PIC S9(08)    COMP.
00069
00070      12  LA-CONTROL-BY-GROUP-CODE.
00071          16  LA-COMPANY-CD-A6        PIC  X(01).
00072          16  LA-ENTRY-A6.
00073              20  LA-FILLER           PIC  X(02).
00074              20  LA-QUE-CONTROL-A6   PIC S9(08)    COMP.
00075          16  LA-ARCHIVE-NO-A6        PIC S9(08)    COMP.
00076
00077      12  FILLER                      PIC  X(09).
00078
00079      12  LA-HEADER-RECORD.
00080          16  LA-NUMBER-LABEL-LINES   PIC S9(04)    COMP.
00081          16  LA-CREATION-DATE        PIC  X(02).
00082          16  LA-FOLLOW-UP-DATE       PIC  X(02).
070711         16  LA-FINAL-ACT-DATE       REDEFINES
070711               LA-FOLLOW-UP-DATE     PIC  X(02).
00083          16  LA-INITIAL-PRINT-DATE   PIC  X(02).
00084          16  LA-NO-OF-COPIES         PIC S9(01).
00085          16  LA-NO-OF-TEXT-RECORDS   PIC S9(04)    COMP.
00086          16  LA-REPLY-DATE           PIC  X(02).
00087          16  LA-RESEND-DATES.
00090              20  LA-RESEND-DATE      PIC  X(02).
00091              20  LA-SENT-DATE        PIC  X(02).
                   20  FILLER              PIC  X(08).
00099          16  LA-SOURCE-INFORMATION.
00100              20  LA-DATA-SOURCE      PIC  X(01).
00101              20  LA-ADDR-SOURCE      PIC  X(01).
00102          16  LA-STATUS               PIC  X(01).
00103              88  LA-STATUS-ACTIVE         VALUE 'A'.
00104              88  LA-STATUS-COMPLETED      VALUE 'C'.
00105              88  LA-STATUS-ON-HOLD        VALUE 'H'.
00106              88  LA-STATUS-TO-BE-PURGED   VALUE 'X'.
00107              88  LA-STATUS-PURGED         VALUE 'P'.
00108              88  LA-STATUS-VOIDED         VALUE 'V'.
00109          16  LA-LAST-RESENT-PRINT-DATE
00110                                      PIC  X(02).
00111          16  LA-PRINT-RESTRICTION    PIC  X(01).
00112              88  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
00113                                           VALUE 'C'.
00114              88  LA-PRINT-ONLY-WHEN-FORM-GIVEN
00115                                           VALUE 'F'.
00116              88  LA-PRINT-ONLY-WHEN-PROC-GIVEN
00117                                           VALUE 'P'.
00118          16  LA-PURGED-DATE          PIC  X(02).
00119          16  LA-VOIDED-DATE          PIC  X(02).
101705         16  LA-RESEND-LETR          PIC  X(4).
102918         16  LA-VOID-ONBASE-YN       PIC  X.
102918         16  LA-ONBASE-UNIQUE-ID     PIC S9(5) COMP-3.
102918         16  FILLER                  PIC  X(04).
101705*        16  LA-RESEND-LETR-2        PIC  X(4).
101705*        16  LA-RESEND-LETR-3        PIC  X(4).
070711*        16  FILLER                  PIC  X(59).
               16  LA-ARCHIVE-STATUS       PIC  X.
                   88  LA-TEMP                VALUE 'T'.
                   88  LA-QWS                 VALUE 'Q'.
                   88  LA-BATCH               VALUE 'B'.
070711         16  LA-FINAL-ACT-IND        PIC  X(1).
070711         16  LA-VA-DISCLOSURE-IND    PIC  X(1).
110612         16  LA-ENDT-ARCH-NO         PIC S9(8) COMP.
110612         16  LA-ENDT-ARCH-NO-X REDEFINES LA-ENDT-ARCH-NO
110612                                     PIC X(4).
110612         16  FILLER                  PIC  X(42).
00120 *        16  FILLER                  PIC  X(71).
070711         16  LA-LAST-MAINT-DATE      PIC  X(2).
070711         16  LA-LAST-MAINT-TIME      PIC S9(6) COMP-3.
070711         16  LA-LAST-MAINT-TIMEX  REDEFINES LA-LAST-MAINT-TIME
070711                                     PIC  X(4).
070711         16  LA-LAST-UPDATED-BY      PIC  X(4).
00121
00122 ******************************************************************
072312*                                COPY ERCCNOT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCCNOT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE NOTES                    *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 150    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERCNOT        RKP=2,LEN=36               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  NEW FILE FOR CERT NOTES.
00017 ******************************************************************
00018
00019  01  CERT-NOTE-FILE.
00020      12  CZ-RECORD-ID                PIC  XX.
00021          88  VALID-CZ-ID                  VALUE 'CZ'.
00022
00023      12  CZ-CONTROL-PRIMARY.
00024          16  CZ-COMPANY-CD           PIC X.
00025          16  CZ-CARRIER              PIC X.
00026          16  CZ-GROUPING.
00027              20 CZ-GROUPING-PREFIX   PIC XXX.
00028              20 CZ-GROUPING-PRIME    PIC XXX.
00029          16  CZ-STATE                PIC XX.
00030          16  CZ-ACCOUNT.
00031              20 CZ-ACCOUNT-PREFIX    PIC X(4).
00032              20 CZ-ACCOUNT-PRIME     PIC X(6).
00033          16  CZ-CERT-EFF-DT          PIC XX.
00034          16  CZ-CERT-NO.
00035              20  CZ-CERT-PRIME       PIC X(10).
00036              20  CZ-CERT-SFX         PIC X.
00037          16  CZ-RECORD-TYPE          PIC X.
00038              88  CERT-NOTE           VALUE '1'.
                   88  CLAIM-CERT-NOTE     VALUE '2'.
00039          16  CZ-NOTE-SEQUENCE        PIC S9(4)     COMP.
00040
00041      12  CZ-LAST-MAINT-DT            PIC XX.
00042      12  CZ-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00043      12  CZ-LAST-MAINT-USER          PIC X(4).
00044
00045      12  CZ-NOTE-INFORMATION.
00046          16  CZ-NOTE                 PIC X(63).
00047          16  FILLER                  PIC X(39).
00048 ******************************************************************
072312*                                COPY ELCEOBC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCEOBC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   CLAIM SYSTEM EOB CODE TABLE                                  *
      *                                                                *
      *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
      *   VSAM EOB CODE TABLE                                          *
      *                                                                *
      *   FILE DESCRIPTION = EOB CODE TABLE                            *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 350   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = ELEOBC                    RKP=2,LEN=15   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 120808    2008100900001  PEMA  NEW COPYBOOK/FILE
      * 081511    2011022800001  PEMA  CHG FOR ADMIN SERV NAPERSOFT
091913* 091913    2013090300001  AJRA  ADDITIONAL RECORD TYPES
      ******************************************************************
       01  EOB-CODES.
           12  EO-RECORD-ID                      PIC XX.
               88  VALID-DN-ID                      VALUE 'EO'.
           12  EO-CONTROL-PRIMARY.
               16  EO-COMPANY-CD                 PIC X.
               16  EO-RECORD-TYPE                PIC X.
                   88  EO-EOB-RECS                  VALUE '1'.
                   88  EO-VERIF-RECS                VALUE '2'.
                   88  EO-GCE-RECS                  VALUE '3'.
091913             88  EO-CANC-RECS                 VALUE '4'.
091913             88  EO-BILL-NOTE-RECS            VALUE '5'.
               16  EO-EOB-CODE                   PIC X(4).
               16  FILLER                        PIC X(9).
           12  EO-MAINT-INFORMATION.
               16  EO-LAST-MAINT-DT              PIC XX.
               16  EO-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
               16  EO-LAST-MAINT-USER            PIC X(4).
               16  FILLER                        PIC XX.
           12  EO-DESCRIPTION                    PIC X(275).
           12  FILLER                            PIC X(46).
      ******************************************************************
121712*                                COPY ELCCRTT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCRTT.                            *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 552  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
111204******************************************************************
111204*                   C H A N G E   L O G
111204*
111204* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111204*-----------------------------------------------------------------
111204*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111204* EFFECTIVE    NUMBER
111204*-----------------------------------------------------------------
111204* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
040109* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
012010* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
022715* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
012918* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
091318* 091318  CR2018073000001  PEMA  ADD Refund methods
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
111204******************************************************************
00021
00022  01  CERTIFICATE-TRAILERS.
00023      12  CS-RECORD-ID                      PIC XX.
00024          88  VALID-CS-ID                      VALUE 'CS'.
00025
00026      12  CS-CONTROL-PRIMARY.
00027          16  CS-COMPANY-CD                 PIC X.
00028          16  CS-CARRIER                    PIC X.
00029          16  CS-GROUPING                   PIC X(6).
00032          16  CS-STATE                      PIC XX.
00033          16  CS-ACCOUNT                    PIC X(10).
00036          16  CS-CERT-EFF-DT                PIC XX.
00037          16  CS-CERT-NO.
00038              20  CS-CERT-PRIME             PIC X(10).
00039              20  CS-CERT-SFX               PIC X.
               16  CS-TRAILER-TYPE               PIC X.
                   88  COMM-TRLR           VALUE 'A'.
061013             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
040109             88  CERT-DATA-TRLR      VALUE 'C'.
00040
040109     12  CS-DATA-AREA                      PIC X(516).
040109
040109     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
040109         16  CS-BANK-COMMISSION-AREA.
040109             20  CS-BANK-COMMS       OCCURS 10.
040109                 24  CS-AGT                PIC X(10).
040109                 24  CS-COM-TYP            PIC X.
040109                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
040109                 24  CS-RECALC-LV-INDIC    PIC X.
040109                 24  FILLER                PIC X(10).
040109         16  FILLER                        PIC X(256).
040109
061013     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
061013****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
               16  CS-MB-CLAIM-DATA OCCURS 24.
                   20  CS-CLAIM-NO               PIC X(7).
                   20  CS-CLAIM-TYPE             PIC X.
                       88  CS-AH-CLM               VALUE 'A'.
                       88  CS-IU-CLM               VALUE 'I'.
                       88  CS-GP-CLM               VALUE 'G'.
                       88  CS-LF-CLM               VALUE 'L'.
                       88  CS-PR-CLM               VALUE 'P'.
052614                 88  CS-FL-CLM               VALUE 'F'.
100518                 88  CS-OT-CLM               VALUE 'O'.
                   20  CS-INSURED-TYPE           PIC X.
                       88  CS-PRIM-INSURED          VALUE 'P'.
                       88  CS-CO-BORROWER           VALUE 'C'.
                   20  CS-BENEFIT-PERIOD         PIC 99.
                   20  CS-DAYS-PAID              PIC S9(5) COMP-3.
                   20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
                   20  CS-REMAINING-BENS         PIC S999 COMP-3.
               16  FILLER                        PIC X(12).
040109     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
040109         16  CS-VIN-NUMBER                 PIC X(17).
012010         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
121712         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
121712         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
022715         16  cs-agent-name.
022715             20  cs-agent-fname            pic x(20).
022715             20  cs-agent-mi               pic x.
022715             20  cs-agent-lname            pic x(25).
022715         16  cs-license-no                 pic x(15).
022715         16  cs-npn-number                 pic x(10).
022715         16  cs-agent-edit-status          pic x.
022715             88  cs-ae-refer-to-manager      value 'M'.
022715             88  cs-ae-cover-sheet           value 'C'.
022715             88  cs-ae-sig-form              value 'S'.
022715             88  cs-ae-verified              value 'V'.
022715             88  cs-unidentified-signature   value 'U'.
022715             88  cs-cert-returned            value 'R'.
022715             88  cs-accept-no-commission     value 'N'.
020816         16  cs-year                       pic 9999.
020816         16  cs-make                       pic x(20).
020816         16  cs-model                      pic x(20).
020816         16  cs-future                     pic x(20).
020816         16  cs-vehicle-odometer           pic s9(7) comp-3.
012918         16  cs-claim-verification-status  pic x.
012918             88  cs-clm-ver-eligible         value 'A'.
012918             88  cs-clm-ver-partial-elig     value 'B'.
012918             88  cs-clm-ver-not-eligible     value 'C'.
012918             88  cs-clm-ver-not-elig-opn-clm value 'D'.
012918             88  cs-clm-ver-not-part-elig-rw value 'E'.
012918             88  cs-clm-ver-ND-CERT          value 'F'.
012918             88  cs-clm-ver-spec-other       value 'G'.
012918             88  cs-clam-ver-pratial-corrected
012918                                             value 'H'.
012918             88  cs-clm-ver-no-matches       value 'I'.
012918             88  cs-clm-ver-not-elig-corrected
012918                                             value 'J'.
012918             88  cs-clm-ver-needs-review     value 'R'.
012918             88  cs-clm-ver-sent-to-claims   value 'W'.
091318         16  CS-LF-REFUND-METHOD           PIC X.
091318         16  CS-AH-REFUND-METHOD           PIC X.
020816         16  FILLER                        PIC X(353). *> was 420
121712*        16  FILLER                        PIC X(496).
121713*                                COPY ELCENCC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCENCC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   CLAIM SYSTEM ENCLOSURE CODE TABLE                            *
      *                                                                *
      *   THIS COPYBOOK IS USED FOR THE ONLINE PROCESS OF CREATING     *
      *   A NAPERSOFT DOCUMENT                                         *
      *                                                                *
      *   FILE DESCRIPTION = ENCLOSURE CODE TABLE                      *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 400   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = ELENCC                    RKP=2,LEN=16   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 082010    2008100900001  PEMA  NEW COPYBOOK/FILE
      * 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
      ******************************************************************
       01  ENCLOSURE-CODES.
           12  NC-RECORD-ID                      PIC XX.
               88  VALID-NC-ID                      VALUE 'NC'.
           12  NC-CONTROL-PRIMARY.
               16  NC-COMPANY-CD                 PIC X.
               16  NC-REC-TYPE                   PIC X.
                   88  NC-CLAIMS                   VALUE '1'.
                   88  NC-ADMIN                    VALUE '2'.
               16  NC-ENC-CODE                   PIC X(5).
               16  FILLER                        PIC X(09).
           12  NC-MAINT-INFORMATION.
               16  NC-LAST-MAINT-DT              PIC XX.
               16  NC-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
               16  NC-LAST-MAINT-USER            PIC X(4).
               16  FILLER                        PIC XX.
           12  NC-OUTPUT-STACK                   PIC XXX.
           12  NC-ENCLOSURE-LINE                 PIC X(100).
           12  NC-ATTACHMENTS                    PIC X(255).
           12  NC-FUTURE                         PIC X(12).
      ******************************************************************
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR
                                ACCOUNT-MASTER CONTROL-FILE
                                PENDING-BUSINESS CERTIFICATE-MASTER
                                ORIGINAL-CERTIFICATE
                                ENDORSEMENT-RECORD CERTIFICATE-NOTE
                                MAILING-DATA LETTER-ARCHIVE
                                CERT-NOTE-FILE EOB-CODES
                                CERTIFICATE-TRAILERS ENCLOSURE-CODES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6317' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT
           MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT
           MOVE EIBTRMID               TO QID-TERM
           MOVE 2                      TO EMI-NUMBER-OF-LINES
           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF
           MOVE FUNCTION LENGTH(EL631JI) TO MAP-LENGTH
           IF PI-CALLING-PROGRAM NOT = THIS-PGM
              IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
                 MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
                 MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
                 MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
                 MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
                 MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
                 MOVE THIS-PGM             TO PI-CALLING-PROGRAM
                 MOVE LOW-VALUES           TO EL631JI
062712           MOVE 'Y'                  TO PI-CLEAR-ERROR-SW
                 GO TO 0350-BUILD-INIT-MAP
              END-IF
           END-IF
           IF PI-CALLING-PROGRAM = 'EL614'
              MOVE PI-PROGRAM-WORK-AREA (1:60)
                                       TO WS-PASSED-REASON-CODES
              DISPLAY ' COMING BACK FROM 614 ' WS-PASSED-REASON-CODES
              IF WS-PASSED-REASON-CODES NOT = SPACES
                 MOVE 'Y'              TO WS-SET-CODES-MDT
              ELSE
                 MOVE 'N'              TO WS-SET-CODES-MDT
              END-IF
              PERFORM 0600-RECOVER-TEMP-STORAGE
                                       THRU 0600-EXIT
121713        PERFORM 0325-EDIT-SCREEN THRU 0325-EXIT
062712        GO TO 0350-BUILD-INIT-MAP
           END-IF
           IF EIBAID = DFHCLEAR
062712        IF PI-CLEAR-ERROR
062712           MOVE SPACES            TO PI-CLEAR-ERROR-SW
121112           MOVE ER-3833           TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 0350-BUILD-INIT-MAP
062712        ELSE
062712           GO TO 9400-CLEAR
062712        END-IF
           END-IF
           .
       0200-RECEIVE.
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE ER-0008             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO PFENTRL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF
           
      * EXEC CICS RECEIVE
      *        MAP      (MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        INTO     (EL631JI)
      *    END-EXEC
           MOVE LENGTH OF
            EL631JI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005827' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL631JI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF PFENTRL = 0
               GO TO 0300-CHECK-PFKEYS
           END-IF
           IF EIBAID NOT = DFHENTER
               MOVE ER-0004            TO EMI-ERROR
               GO TO 0320-INPUT-ERROR
           END-IF
           IF (PFENTRI NUMERIC) AND (PFENTRI > 0 AND < 25)
              MOVE PF-VALUES (PFENTRI) TO EIBAID
           ELSE
              MOVE ER-0029            TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF
           .
       0300-CHECK-PFKEYS.
121112     IF EIBAID = DFHPF3
062712        MOVE 'Y'                 TO PI-CLEAR-ERROR-SW
              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0500-EXIT
              MOVE 'EL614'             TO PGM-NAME
              MOVE SPACES              TO PI-PROGRAM-WORK-AREA
              move +1                  to s2
              perform varying s1 from +1 by +1 until s1 > +12
                 if reacd-in (s1) not = spaces and low-values
                    move reacd-in (s1)    to pi-program-work-area (s2:4)
                    add +5 to s2
                 end-if
              END-perform
              DISPLAY ' GOING TO 614 ' PI-PROGRAM-WORK-AREA (1:60)
              GO TO 9300-XCTL
           END-IF
121112     IF EIBAID = DFHPF4 OR DFHPF8
              GO TO 0360-FINALIZE
           END-IF
           IF EIBAID = DFHPF23
062712        IF PI-CLEAR-ERROR
062712           MOVE SPACES            TO PI-CLEAR-ERROR-SW
121112           MOVE ER-3833           TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 0350-BUILD-INIT-MAP
062712        ELSE
062712           GO TO 8810-PF23
062712        END-IF
           END-IF
           IF EIBAID = DFHPF24
062712        IF PI-CLEAR-ERROR
062712           MOVE SPACES            TO PI-CLEAR-ERROR-SW
121112           MOVE ER-3833           TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 0350-BUILD-INIT-MAP
062712        ELSE
062712           GO TO 9200-RETURN-MAIN-MENU
062712        END-IF
           END-IF
           IF EIBAID = DFHPF12
              GO TO 9500-PF12
           END-IF
           IF EIBAID = DFHENTER
062712        MOVE 'Y'                 TO PI-CLEAR-ERROR-SW
121713        PERFORM 0325-EDIT-SCREEN THRU 0325-EXIT
121713        GO TO 0330-CHECK-ERRORS
           END-IF
           MOVE ER-0029                TO EMI-ERROR
           .
       0320-INPUT-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           MOVE AL-UNBON               TO PFENTRA
           MOVE -1                     TO PFENTRL
           GO TO 8200-SEND-DATAONLY
           .
       0325-EDIT-SCREEN.
           if prtswl > zeros
              if prtswi = 'Y' OR 'N' OR 'P' OR ' '
072312*          MOVE AL-UANON       TO PRTSWA
072312*          IF PRTSWI = ' '
072312              MOVE 'P'         TO PRTSWI
072312*          END-IF
072312*          MOVE PRTSWI         TO PI-PRTSW-ENTERED
              END-IF
           END-IF
           IF (SIGREQL > ZEROS)
              AND (SIGREQI = 'Y' OR 'N' OR ' ')
              MOVE AL-UANON         TO SIGREQA
              IF SIGREQI = ' '
                 MOVE 'N'           TO SIGREQI
              END-IF
062712        MOVE SIGREQI          TO PI-SIGREQ-ENTERED
           ELSE
              MOVE ER-2651          TO EMI-ERROR
              MOVE -1               TO SIGREQL
              MOVE AL-UABON         TO SIGREQA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           IF LTRIDL > ZEROS
              AND LTRIDI > SPACES
              MOVE PI-COMPANY-CD       TO LETR-COMPANY-CD
              MOVE LTRIDI              TO LETR-LETTER-ID
062712                                    PI-LTRID-ENTERED
112612        MOVE LETR-PART-KEY       TO ELLETR-SAVE-PART-KEY
              MOVE SPACES              TO LETR-FILLER
112612        MOVE 0                   TO LETR-SEQ-NO
112612        PERFORM 0326-GET-Z-RECORD THRU 0326-EXIT
           ELSE
              MOVE ER-1236          TO EMI-ERROR
              MOVE -1               TO LTRIDL
              MOVE AL-UABON         TO LTRIDA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
112612
112612     IF W-PROMPT-LETTER EQUAL 'Y'
112612         MOVE ER-0894            TO EMI-ERROR
112612         MOVE -1                 TO LTRIDL
112612         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112612     END-IF
121713
121713     IF ENCCODEL > ZEROS
121713        MOVE FUNCTION UPPER-CASE(ENCCODEI) TO ENCCODEI
121713                                              PI-ENCCODE
121713        MOVE SPACES             TO WS-ELENCC-KEY
121713        MOVE PI-COMPANY-CD      TO WS-ENCC-COMPANY-CD
121713        MOVE '2'                TO WS-ENCC-REC-TYPE
121713        MOVE ENCCODEI           TO WS-ENCC-ENC-CODE
121713
121713        
      * EXEC CICS READ
121713*           DATASET    (WS-ELENCC-FILE-ID)
121713*           SET        (ADDRESS OF ENCLOSURE-CODES)
121713*           RIDFLD     (WS-ELENCC-KEY)
121713*           RESP       (WS-RESPONSE)
121713*       END-EXEC
      *    MOVE '&"S        E          (  N#00005956' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035393536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELENCC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENCLOSURE-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
121713
121713        IF RESP-NORMAL
121713           MOVE ENCCODEI         TO PI-ENCCODE
121713                                    W-ENCLOSURE-CD
121713           MOVE AL-UANON         TO ENCCODEA
121713        ELSE
121713           MOVE ER-1560          TO EMI-ERROR
121713           MOVE -1               TO ENCCODEL
121713           MOVE AL-UABON         TO ENCCODEA
121713           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121713        END-IF
121713     END-IF
121713
062712     SET REASONS-NOT-FOUND TO TRUE
062712     PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
062712        IF REACDL (S1) = +4 OR
062712          (REACD-IN (S1) NOT = SPACES AND LOW-VALUES)
062712              MOVE AL-SANON     TO REACDA (S1)
062712              SET REASONS-FOUND TO TRUE
062712        END-IF
062712     END-PERFORM
062712
062712     IF REASONS-NOT-FOUND
062712        MOVE ER-1820             TO EMI-ERROR
062712        MOVE -1                  TO PFENTRL
062712        PERFORM 9900-ERROR-FORMAT
062712                                 THRU 9900-EXIT
062712     ELSE
062712        MOVE REASONCDS           TO PI-REASONS-ENTERED
062712     END-IF
062712
072312     IF CRTNT1L > ZEROS
072312        IF CRTNT1I > SPACES
072312           MOVE CRTNT1I          TO PI-CERTNT1-ENTERED
072312        END-IF
072312     END-IF
072312
072312     IF CRTNT2L > ZEROS
072312        IF CRTNT2I > SPACES
072312           MOVE CRTNT2I          TO PI-CERTNT2-ENTERED
072312        END-IF
072312     END-IF
072312
121212     IF BILNT1L > ZEROS
121212        IF BILNT1I > SPACES
121212           MOVE BILNT1I          TO PI-BILLNT1-ENTERED
121212        END-IF
121212     END-IF
121212
121212     IF BILNT2L > ZEROS
121212        IF BILNT2I > SPACES
121212           MOVE BILNT2I          TO PI-BILLNT2-ENTERED
121212        END-IF
121212     END-IF
121212
           .
       0325-EXIT.
           EXIT.
112612
112612 0326-GET-Z-RECORD.
112612
112612     MOVE SPACES  TO W-Z-CONTROL-DATA
112612
112612     
      * EXEC CICS STARTBR
112612*         DATASET    ('ELLETR')
112612*         RIDFLD     (ELLETR-KEY)
112612*         GTEQ
112612*         RESP      (WS-RESPONSE)
112612*    END-EXEC.
           MOVE 'ELLETR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006025' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036303235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
112612     IF NOT RESP-NORMAL
112612         MOVE ER-1236          TO EMI-ERROR
112612         MOVE -1               TO LTRIDL
112612         MOVE AL-UABON         TO LTRIDA
112612         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112612         GO TO 0326-ENDBR
112612     END-IF.
112612
112612 0326-READNEXT.
112612
112612     
      * EXEC CICS READNEXT
112612*        DATASET   ('ELLETR')
112612*        INTO      (TEXT-FILES)
112612*        RIDFLD    (ELLETR-KEY)
112612*        RESP      (WS-RESPONSE)
112612*    END-EXEC
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV12
           MOVE 'ELLETR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006041' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036303431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 TEXT-FILES, 
                 DFHEIV12, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
112612     IF RESP-NORMAL
112612        IF TX-CONTROL-PRIMARY(1:5) NOT = ELLETR-SAVE-PART-KEY
112612           MOVE ER-1236          TO EMI-ERROR
112612           MOVE -1               TO LTRIDL
112612           MOVE AL-UABON         TO LTRIDA
112612           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112612           GO TO 0326-ENDBR
112612        END-IF
112612
112612        MOVE AL-UANON         TO LTRIDA
112612        IF TX-LINE-SQUEEZE-CONTROL = 'Z'
112612           MOVE TX-TEXT-LINE  TO W-Z-CONTROL-DATA
112612        ELSE
112612           GO TO 0326-READNEXT
112612        END-IF
112612     ELSE
112612        MOVE ER-1236          TO EMI-ERROR
112612        MOVE -1               TO LTRIDL
112612        MOVE AL-UABON         TO LTRIDA
112612        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112612        GO TO 0326-ENDBR
112612     END-IF.
112612
121713     IF PI-ENCCODE NOT GREATER THAN SPACES
121713         MOVE W-ENCLOSURE-CD            TO PI-ENCCODE
121713                                           ENCCODEO
121713         MOVE AL-UANON                  TO ENCCODEA
121713         MOVE +3                        TO ENCCODEL
121713     END-IF.
121713
112612 0326-ENDBR.
112612
112612     
      * EXEC CICS ENDBR
112612*        DATASET     ('ELLETR')
112612*    END-EXEC.
           MOVE 'ELLETR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006079' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
112612
112612 0326-EXIT.
112612      EXIT.
       0330-CHECK-ERRORS.
           IF EMI-NO-ERRORS
062712        GO TO 0350-BUILD-INIT-MAP
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
       0330-EXIT.
           EXIT.
       0350-BUILD-INIT-MAP.
062712
062712     IF PI-RETURN-TO-PROGRAM = 'EL1273'
120313      OR 'EL6314'
              display 'coming from ' PI-RETURN-TO-PROGRAM
062712        MOVE PI-COMPANY-CD          TO CRTO-COMPANY-CD
062712        MOVE PI-CARRIER             TO CRTO-CARRIER
062712        MOVE PI-GROUPING            TO CRTO-GROUPING
062712        MOVE PI-STATE               TO CRTO-STATE
062712        MOVE PI-ACCOUNT             TO CRTO-ACCOUNT
062712        MOVE PI-CERT-PRIME          TO CRTO-CERT-PRIME
062712        MOVE PI-CERT-SFX            TO CRTO-CERT-SFX
062712        MOVE PI-CERT-EFF-DT         TO CRTO-CERT-EFF-DT
062712        MOVE 'I'                    TO CRTO-RECORD-TYPE
062712        MOVE ZEROS                  TO CRTO-SEQ-NO
062712        MOVE ELCRTO-KEY             TO PI-ELCRTO-KEY
062712                                       PI-ERENDT-KEY
062712     END-IF
           MOVE PI-CRTO-CARRIER        TO CARRO
           MOVE PI-CRTO-GROUPING       TO GROUPO
           MOVE PI-CRTO-STATE          TO STATEO
           MOVE PI-CRTO-ACCOUNT        TO ACCTO
           MOVE PI-CRTO-CERT-PRIME     TO CERTNOO
           MOVE PI-CRTO-CERT-SFX       TO CRTSFXO
           MOVE PI-CRTO-SEQ-NO         TO SEQNOO
           MOVE PI-CRTO-CERT-EFF-DT    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           END-IF
062712
072312*    IF PI-PRTSW-ENTERED > SPACES
072312*       MOVE PI-PRTSW-ENTERED    TO PRTSWI
072312*       MOVE AL-UANON            TO PRTSWA
072312*       MOVE +1                  TO PRTSWL
072312*    END-IF
062712
062712     IF PI-SIGREQ-ENTERED > SPACES
062712        MOVE PI-SIGREQ-ENTERED   TO SIGREQI
062712        MOVE AL-UANON            TO SIGREQA
062712        MOVE +1                  TO SIGREQL
062712     END-IF
062712
062712     IF PI-LTRID-ENTERED > SPACES
062712        MOVE PI-LTRID-ENTERED    TO LTRIDI
062712        MOVE AL-UANON            TO LTRIDA
062712        MOVE +4                  TO LTRIDL
062712     END-IF
121713
121713     IF PI-ENCCODE > SPACES
121713        MOVE PI-ENCCODE          TO ENCCODEI
121713        MOVE AL-UANON            TO ENCCODEA
121713        MOVE +3                  TO ENCCODEL
121713     END-IF
062712
062712     MOVE PI-REASONS-ENTERED     TO REASONCDS
062712     PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
062712        IF REACDL (S1) = +4 OR
062712          (REACD-IN (S1) NOT = SPACES AND LOW-VALUES)
062712              MOVE AL-SANON      TO REACDA (S1)
062712              MOVE +4            TO REACDL (S1)
062712              SET REASONS-FOUND  TO TRUE
062712        END-IF
062712     END-PERFORM
062712
121212     IF PI-CERTNT1-ENTERED > SPACES
121212        MOVE PI-CERTNT1-ENTERED  TO CRTNT1I
121212        MOVE AL-UANON            TO CRTNT1A
121212        MOVE +63                 TO CRTNT1L
121212     END-IF
121212
121212     IF PI-CERTNT2-ENTERED > SPACES
121212        MOVE PI-CERTNT2-ENTERED  TO CRTNT2I
121212        MOVE AL-UANON            TO CRTNT2A
121212        MOVE +63                 TO CRTNT2L
121212     END-IF
121212
121212     IF PI-BILLNT1-ENTERED > SPACES
121212        MOVE PI-BILLNT1-ENTERED  TO BILNT1I
121212        MOVE AL-UANON            TO BILNT1A
121212        MOVE +63                 TO BILNT1L
121212     END-IF
121212
121212     IF PI-BILLNT2-ENTERED > SPACES
121212        MOVE PI-BILLNT2-ENTERED  TO BILNT2I
121212        MOVE AL-UANON            TO BILNT2A
121212        MOVE +63                 TO BILNT2L
121212     END-IF
121212
           GO TO 8100-SEND-INITIAL-MAP
           .
       0360-FINALIZE.
           display ' made it to 0360 '
           PERFORM 0325-EDIT-SCREEN    THRU 0325-EXIT
           IF NOT EMI-NO-ERRORS
121112       AND EIBAID = DFHPF4
              GO TO 8200-SEND-DATAONLY
           END-IF
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00'
      *       DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
      *       DISPLAY ' WS KIX SYS ' WS-KIXSYS
      *       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if
           set P to address of KIXHOST
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixhost not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00'
      *       DISPLAY '  KIXHOST = ' var (1:env-var-len)
              MOVE var(1:env-var-len)  to ws-kixhost
              DISPLAY ' WS KIX HOST ' WS-KIXhost
           end-if
           MOVE PI-ELCRTO-KEY          TO ELCRTO-KEY
           PERFORM 0362-READ-ELCRTO    THRU 0362-EXIT
           IF RESP-NORMAL
062712        AND (PI-ELCRTO-KEY (1:33) = OC-CONTROL-PRIMARY (1:33))
              DISPLAY ' RESP NORMAL AND = '
062712        MOVE OC-KEY-SEQ-NO  TO CRTO-SEQ-NO
062712                               PI-CRTO-SEQ-NO
062712        MOVE LOW-VALUES     TO NEW-ELCRTO-REC
062712        MOVE PI-ELCRTO-KEY  TO NEW-ELCRTO-KEY
062712        SUBTRACT +1         FROM NCRTO-SEQ-NO
              PERFORM 0400-ADD-ERENDT  THRU 0400-EXIT
      *       PERFORM 1000-BUILD-ERARCH THRU 1000-EXIT
              PERFORM 1100-CALL-NS-BUS-LOGIC
                                       THRU 1100-EXIT
              IF BL-OK
                 CONTINUE
              ELSE
                 MOVE ER-8888          TO EMI-ERROR-NUMBER (1)
                 MOVE BL-MESSAGE       TO EMI-ERROR-TEXT (1)
                 MOVE -1               TO CARRL
              END-IF
           ELSE
              MOVE ER-9999             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF
           MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
                                       TO SYSO
           MOVE WS-KIXHOST             TO HOSTO
           MOVE PI-COMPANY-ID          TO COMPANYO
062712     IF PI-RETURN-TO-PROGRAM = 'EL1273'
120313       OR 'EL6314'
062712         MOVE '2'                TO DATASORO
062712     ELSE
062712         MOVE '4'                TO DATASORO
062712     END-IF
           MOVE BL-BATCH-NO            TO BATCHNOO
           MOVE BL-BATCH-SEQ           TO BSEQNOO
           MOVE BL-ARCHIVE-NO          TO ARCHNOO
           MOVE PI-PROCESSOR-ID        TO PROCIDO
072312*    MOVE -1                     TO PRTSWL
072312*    MOVE AL-UANON               TO PRTSWA
072312     MOVE -1                     TO SIGREQL
072312     MOVE AL-UANON               TO SIGREQA
121112     IF EIBAID = DFHPF4
062712        MOVE SPACES              TO PI-CLEAR-ERROR-SW
              MOVE ER-0280             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE BL-ARCHIVE-NO       TO W-ARCH-SUPPRESS
              MOVE W-ARCH-EDIT         TO EMI-TEXT-VARIABLE (1)
072312        MOVE 'Y'                 TO PI-FINALIZED-IND
072312        GO TO 9400-CLEAR
           ELSE
              MOVE ER-1818             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           GO TO 0350-BUILD-INIT-MAP
           .
       0360-EXIT.
           EXIT.
       0362-READ-ELCRTO.
           display ' made it to 0362 '
           
      * EXEC CICS READ
      *        DATASET   (WS-ELCRTO-FILE-ID)
      *        SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
      *        RIDFLD    (ELCRTO-KEY)
062712*        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        G          (  N#00006289' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCRTO-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ORIGINAL-CERTIFICATE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0362-EXIT.
           EXIT.
       0363-READ-ELCRTO-UPD.
           display ' made it to 0363 '
           
      * EXEC CICS READ
      *        DATASET   (WS-ELCRTO-FILE-ID)
      *        SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
      *        RIDFLD    (ELCRTO-KEY)
      *        UPDATE
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        EU         (  N#00006301' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036333031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCRTO-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ORIGINAL-CERTIFICATE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0363-EXIT.
           EXIT.
       0365-READ-ERENDT.
           
      * EXEC CICS READ
      *        DATASET   (WS-ERENDT-FILE-ID)
      *        SET       (ADDRESS OF ENDORSEMENT-RECORD)
      *        RIDFLD    (ERENDT-KEY)
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        E          (  N#00006312' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036333132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERENDT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERENDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENDORSEMENT-RECORD TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0365-EXIT.
           EXIT.
       0400-ADD-ERENDT.
           PERFORM 1500-GET-ARCH-NO    THRU 1500-EXIT
           display ' made it to 0400 '
           
      * EXEC CICS GETMAIN
      *         SET      (ADDRESS OF ENDORSEMENT-RECORD)
      *         LENGTH   (ERENDT-LENGTH)
      *         INITIMG  (GETMAIN-SPACE)
      *    END-EXEC
      *    MOVE ',"IL                  $   #00006324' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERENDT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ENDORSEMENT-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           PERFORM 0410-INIT-ERENDT    THRU 0410-EXIT
           MOVE PI-PROCESSOR-ID        TO EN-LAST-MAINT-BY
           MOVE EIBTIME                TO EN-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DT      TO EN-LAST-MAINT-DT
                                          EN-INPUT-DT
                                          EN-PROCESS-DT
062712
062712     MOVE OC-INS-FIRST-NAME    TO EN-INS-ORIG-FIRST-NAME
062712     MOVE OC-INS-MIDDLE-INIT   TO EN-INS-ORIG-MIDDLE-INIT
062712     MOVE OC-INS-LAST-NAME     TO EN-INS-ORIG-LAST-NAME
062712     MOVE OC-INS-AGE           TO EN-INS-ORIG-AGE
121712     MOVE OC-INS-AGE-DEFAULT-FLAG TO EN-INS-ORIG-AGE-DEF-FLAG
062712     MOVE OC-JNT-FIRST-NAME    TO EN-JNT-ORIG-FIRST-NAME
062712     MOVE OC-JNT-MIDDLE-INIT   TO EN-JNT-ORIG-MIDDLE-INIT
062712     MOVE OC-JNT-LAST-NAME     TO EN-JNT-ORIG-LAST-NAME
062712     MOVE OC-JNT-AGE           TO EN-JNT-ORIG-AGE
121712     MOVE OC-JNT-AGE-DEFAULT-FLAG TO EN-JNT-ORIG-AGE-DEF-FLAG
062712     MOVE OC-LF-BENCD          TO EN-LF-ORIG-BENCD
062712     IF OC-LF-BENCD NOT = '00' AND '  '
062712        MOVE OC-LF-BEN-AMT       TO EN-LF-ORIG-BEN-AMT
062712        MOVE OC-LF-PRM-AMT       TO EN-LF-ORIG-PRM-AMT
062712        MOVE OC-LF-ALT-BEN-AMT   TO EN-LF-ORIG-ALT-BEN-AMT
062712        MOVE OC-LF-ALT-PRM-AMT   TO EN-LF-ORIG-ALT-PRM-AMT
062712        MOVE OC-LF-TERM          TO EN-LF-ORIG-TERM
062712        MOVE OC-LF-EXP-DT        TO EN-LF-ORIG-EXP-DT
062712        MOVE OC-LF-COMM-PCT      TO EN-LF-ORIG-COMM-PCT
062712     END-IF
062712     MOVE OC-AH-BENCD          TO EN-AH-ORIG-BENCD
062712     IF OC-AH-BENCD NOT = '00' AND '  '
062712        MOVE OC-AH-BEN-AMT       TO EN-AH-ORIG-BEN-AMT
062712        MOVE OC-AH-PRM-AMT       TO EN-AH-ORIG-PRM-AMT
062712        MOVE OC-AH-TERM          TO EN-AH-ORIG-TERM
062712        MOVE OC-AH-EXP-DT        TO EN-AH-ORIG-EXP-DT
062712        MOVE OC-AH-CP            TO EN-AH-ORIG-CP
062712        MOVE OC-AH-COMM-PCT      TO EN-AH-ORIG-COMM-PCT
062712     END-IF
062712     MOVE OC-CRED-BENE-NAME      TO EN-ORIG-CRED-BENE
062712
062712     IF PI-RETURN-TO-PROGRAM = 'EL1273'
120313       OR 'EL6314'
               display 'from ' PI-RETURN-TO-PROGRAM
                       ', using crto for new crto'
062712
062712        MOVE OC-INS-FIRST-NAME    TO EN-INS-NEW-FIRST-NAME
062712                                     NCRTO-INS-FIRST-NAME
062712        MOVE OC-INS-MIDDLE-INIT   TO EN-INS-NEW-MIDDLE-INIT
062712                                     NCRTO-INS-MIDDLE-INIT
062712        MOVE OC-INS-LAST-NAME     TO EN-INS-NEW-LAST-NAME
062712                                     NCRTO-INS-LAST-NAME
062712        MOVE OC-INS-AGE           TO EN-INS-NEW-AGE
062712                                     NCRTO-INS-AGE
121712        MOVE OC-INS-AGE-DEFAULT-FLAG TO EN-INS-NEW-AGE-DEF-FLAG
121712                                     NCRTO-INS-AGE-DEFAULT-FLAG
062712        MOVE OC-JNT-FIRST-NAME    TO EN-JNT-NEW-FIRST-NAME
062712                                     NCRTO-JNT-FIRST-NAME
062712        MOVE OC-JNT-MIDDLE-INIT   TO EN-JNT-NEW-MIDDLE-INIT
062712                                     NCRTO-JNT-MIDDLE-INIT
062712        MOVE OC-JNT-LAST-NAME     TO EN-JNT-NEW-LAST-NAME
062712                                     NCRTO-JNT-LAST-NAME
062712        MOVE OC-JNT-AGE           TO EN-JNT-NEW-AGE
062712                                     NCRTO-JNT-AGE
121712        MOVE OC-JNT-AGE-DEFAULT-FLAG TO EN-JNT-NEW-AGE-DEF-FLAG
121712                                     NCRTO-JNT-AGE-DEFAULT-FLAG
062712        MOVE OC-LF-BENCD          TO EN-LF-NEW-BENCD
062712                                     NCRTO-LF-BENCD
062712        IF OC-LF-BENCD NOT = '00' AND '  '
062712           MOVE OC-LF-BEN-AMT       TO EN-LF-NEW-BEN-AMT
062712                                       NCRTO-LF-BEN-AMT
062712           MOVE OC-LF-PRM-AMT       TO EN-LF-NEW-PRM-AMT
062712                                       NCRTO-LF-PRM-AMT
062712           MOVE OC-LF-ALT-BEN-AMT   TO EN-LF-NEW-ALT-BEN-AMT
062712                                       NCRTO-LF-ALT-BEN-AMT
062712           MOVE OC-LF-ALT-PRM-AMT   TO EN-LF-NEW-ALT-PRM-AMT
062712                                       NCRTO-LF-ALT-PRM-AMT
062712           MOVE OC-LF-TERM          TO EN-LF-NEW-TERM
062712                                       NCRTO-LF-TERM
062712           MOVE OC-LF-EXP-DT        TO EN-LF-NEW-EXP-DT
062712                                       NCRTO-LF-EXP-DT
062712           MOVE OC-LF-COMM-PCT      TO EN-LF-NEW-COMM-PCT
062712                                       NCRTO-LF-COMM-PCT
062712           MOVE OC-LF-CANCEL-DT     TO NCRTO-LF-CANCEL-DT
062712           MOVE OC-LF-CANCEL-AMT    TO NCRTO-LF-CANCEL-AMT
071712           MOVE OC-LF-ITD-CANCEL-AMT TO NCRTO-LF-ITD-CANCEL-AMT
062712        END-IF
062712        MOVE OC-AH-BENCD          TO EN-AH-NEW-BENCD
062712                                     NCRTO-AH-BENCD
062712        IF OC-AH-BENCD NOT = '00' AND '  '
062712           MOVE OC-AH-BEN-AMT       TO EN-AH-NEW-BEN-AMT
062712                                       NCRTO-AH-BEN-AMT
062712           MOVE OC-AH-PRM-AMT       TO EN-AH-NEW-PRM-AMT
062712                                       NCRTO-AH-PRM-AMT
062712           MOVE OC-AH-TERM          TO EN-AH-NEW-TERM
062712                                       NCRTO-AH-TERM
062712           MOVE OC-AH-EXP-DT        TO EN-AH-NEW-EXP-DT
062712                                       NCRTO-AH-EXP-DT
062712           MOVE OC-AH-CP            TO EN-AH-NEW-CP
062712                                       NCRTO-AH-CP
062712           MOVE OC-AH-COMM-PCT      TO EN-AH-NEW-COMM-PCT
062712                                       NCRTO-AH-COMM-PCT
062712           MOVE OC-AH-CANCEL-DT     TO NCRTO-AH-CANCEL-DT
062712           MOVE OC-AH-CANCEL-AMT    TO NCRTO-AH-CANCEL-AMT
071712           MOVE OC-AH-ITD-CANCEL-AMT TO NCRTO-AH-ITD-CANCEL-AMT
062712        END-IF
062712        MOVE OC-CRED-BENE-NAME      TO EN-ORIG-CRED-BENE
062712                                       NCRTO-CRED-BENE-NAME
062712
062712        GO TO 0400-WRITE-ENDT
062712     END-IF
           
      * EXEC CICS GETMAIN
      *         SET      (ADDRESS OF PENDING-BUSINESS)
      *         LENGTH   (ERPNDB-LENGTH)
      *         INITIMG  (GETMAIN-SPACE)
      *    END-EXEC
      *    MOVE ',"IL                  $   #00006437' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPNDB-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           DISPLAY ' JUST DID GETMAIN ON ERPNDB '
           MOVE PI-ERPNDB-KEY          TO ERPNDB-KEY
           
      * EXEC CICS READ
      *        DATASET   (WS-ERPNDB-FILE-ID)
      *        INTO      (PENDING-BUSINESS)
      *        RIDFLD    (ERPNDB-KEY)
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&"IL       E          (  N#00006444' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 0400-EXIT
           END-IF
      *    set found-pndb to true
           display ' good read on erpndb '
           MOVE PB-I-INSURED-FIRST-NAME  TO EN-INS-NEW-FIRST-NAME
062712                                      NCRTO-INS-FIRST-NAME
           MOVE PB-I-INSURED-MIDDLE-INIT TO EN-INS-NEW-MIDDLE-INIT
062712                                      NCRTO-INS-MIDDLE-INIT
           MOVE PB-I-INSURED-LAST-NAME   TO EN-INS-NEW-LAST-NAME
062712                                      NCRTO-INS-LAST-NAME
           MOVE PB-I-AGE                 TO EN-INS-NEW-AGE
062712                                      NCRTO-INS-AGE
           MOVE PB-I-JOINT-FIRST-NAME    TO EN-JNT-NEW-FIRST-NAME
062712                                      NCRTO-JNT-FIRST-NAME
           MOVE PB-I-JOINT-MIDDLE-INIT   TO EN-JNT-NEW-MIDDLE-INIT
062712                                      NCRTO-JNT-MIDDLE-INIT
           MOVE PB-I-JOINT-LAST-NAME     TO EN-JNT-NEW-LAST-NAME
062712                                      NCRTO-JNT-LAST-NAME
           MOVE PB-I-JOINT-AGE           TO EN-JNT-NEW-AGE
062712                                      NCRTO-JNT-AGE
062712     MOVE PB-I-LF-BENEFIT-CD       TO EN-LF-NEW-BENCD
062712                                      NCRTO-LF-BENCD
           IF PB-I-LF-BENEFIT-CD NOT = '00' AND '  '
              MOVE PB-I-LF-BENEFIT-AMT   TO EN-LF-NEW-BEN-AMT
062712                                      NCRTO-LF-BEN-AMT
              MOVE PB-I-LF-PREMIUM-AMT   TO EN-LF-NEW-PRM-AMT
062712                                      NCRTO-LF-PRM-AMT
              MOVE PB-I-LF-ALT-BENEFIT-AMT TO EN-LF-NEW-ALT-BEN-AMT
062712                                      NCRTO-LF-ALT-BEN-AMT
              MOVE PB-I-LF-ALT-PREMIUM-AMT TO EN-LF-NEW-ALT-PRM-AMT
062712                                      NCRTO-LF-ALT-PRM-AMT
              MOVE PB-I-LF-TERM          TO EN-LF-NEW-TERM
062712                                      NCRTO-LF-TERM
              MOVE PB-I-LF-EXPIRE-DT     TO EN-LF-NEW-EXP-DT
062712                                      NCRTO-LF-EXP-DT
              IF PB-I-JOINT-COMMISSION > +0
                 MOVE PB-I-JOINT-COMMISSION
                                         TO EN-LF-NEW-COMM-PCT
062712                                      NCRTO-LF-COMM-PCT
              ELSE
                 MOVE PB-I-LIFE-COMMISSION
                                         TO EN-LF-NEW-COMM-PCT
062712                                      NCRTO-LF-COMM-PCT
062712        MOVE LOW-VALUES            TO NCRTO-LF-CANCEL-DT
062712        MOVE +0                    TO NCRTO-LF-CANCEL-AMT
071712                                      NCRTO-LF-ITD-CANCEL-AMT
              END-IF
           END-IF
062712     MOVE PB-I-AH-BENEFIT-CD       TO EN-AH-NEW-BENCD
062712                                      NCRTO-AH-BENCD
           IF PB-I-AH-BENEFIT-CD NOT = '00' AND '  '
              MOVE PB-I-AH-BENEFIT-AMT   TO EN-AH-NEW-BEN-AMT
062712                                      NCRTO-AH-BEN-AMT
              MOVE PB-I-AH-PREMIUM-AMT   TO EN-AH-NEW-PRM-AMT
062712                                      NCRTO-AH-PRM-AMT
              MOVE PB-I-AH-TERM          TO EN-AH-NEW-TERM
062712                                      NCRTO-AH-TERM
              MOVE PB-I-AH-EXPIRE-DT     TO EN-AH-NEW-EXP-DT
062712                                      NCRTO-AH-EXP-DT
              MOVE PB-I-AH-CRIT-PER      TO EN-AH-NEW-CP
062712                                      NCRTO-AH-CP
              MOVE PB-I-AH-COMMISSION    TO EN-AH-NEW-COMM-PCT
062712                                      NCRTO-AH-COMM-PCT
062712        MOVE LOW-VALUES            TO NCRTO-AH-CANCEL-DT
062712        MOVE +0                    TO NCRTO-AH-CANCEL-AMT
071712                                      NCRTO-AH-ITD-CANCEL-AMT
           END-IF
062712     MOVE PB-I-1ST-PMT-DT          TO NCRTO-1ST-PMT-DT
072312     MOVE PB-ENTRY-BATCH           TO EN-BATCH-NUMBER
           move pb-control-by-account (1:33)
                                       to ws-ermail-key
           
      * EXEC CICS READ
      *        DATASET   ('ERMAIL')
      *        SET       (ADDRESS OF MAILING-DATA)
      *        RIDFLD    (WS-ERMAIL-KEY)
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00006522' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERMAIL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              DISPLAY ' FOUND ERMAIL '
              MOVE MA-CRED-BENE-NAME   TO EN-NEW-CRED-BENE
062712                                    NCRTO-CRED-BENE-NAME
           ELSE
              DISPLAY ' ERMAIL READ ' WS-RESPONSE
           END-IF
121712
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO WS-ELCRTT-PRIMARY
121712     MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
121712
121712     
      * EXEC CICS READ
121712*         DATASET  (WS-ELCRTT-FILE-ID)
121712*         RIDFLD   (WS-ELCRTT-KEY)
121712*         SET      (ADDRESS OF CERTIFICATE-TRAILERS)
121712*         RESP     (WS-RESPONSE)
121712*    END-EXEC
      *    MOVE '&"S        E          (  N#00006539' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCRTT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
121712
121712     IF RESP-NORMAL
121712        MOVE CS-INS-AGE-DEFAULT-FLAG TO EN-INS-NEW-AGE-DEF-FLAG
121712                                     NCRTO-INS-AGE-DEFAULT-FLAG
121712        MOVE CS-JNT-AGE-DEFAULT-FLAG TO EN-JNT-NEW-AGE-DEF-FLAG
121712                                     NCRTO-JNT-AGE-DEFAULT-FLAG
121712     END-IF.
062712      .
062712 0400-WRITE-ENDT.
062712
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
              IF REACD-IN (S1) NOT = LOW-VALUES
                 MOVE REACD-IN (S1)    TO EN-REASON-CODE (S1)
              END-IF
           END-PERFORM
           MOVE LTRIDI                 TO EN-TEMPLATE-USED
           MOVE 'V'                    TO EN-DOCU-TYPE
           MOVE ' '                    TO EN-MONEY-SW
           MOVE SIGREQI                TO EN-SIG-SW
072312     MOVE W-ACCT-SUMM            TO EN-ACCT-SUMM
072312     MOVE W-CSO-SUMM             TO EN-CSO-SUMM
           PERFORM 0430-WRITE-ERENDT   THRU 0430-EXIT
112612*    IF NOT EMI-NO-ERRORS
112612*       DISPLAY ' GOING TO 8200 FROM 0400 '
112612*       GO TO 8200-SEND-DATAONLY
112612*    END-IF
           DISPLAY ' 0400 EXIT '
           .
       0400-EXIT.
           EXIT.
       0410-INIT-ERENDT.
           display ' made it to 0410 '
           MOVE OC-CONTROL-PRIMARY     TO ERENDT-KEY
           MOVE ZEROS                  TO ENDT-SEQ-NO
           
      * EXEC CICS READ
      *        DATASET    (WS-ERENDT-FILE-ID)
      *        RIDFLD     (ERENDT-KEY)
      *        INTO       (ENDORSEMENT-RECORD)
      *        RESP       (WS-RESPONSE)
      *        GTEQ
      *    END-EXEC
           MOVE LENGTH OF
            ENDORSEMENT-RECORD
             TO DFHEIV11
      *    MOVE '&"IL       G          (  N#00006579' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERENDT-FILE-ID, 
                 ENDORSEMENT-RECORD, 
                 DFHEIV11, 
                 ERENDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              AND (EN-CONTROL-PRIMARY (1:34) =
                 OC-CONTROL-PRIMARY (1:34))
              COMPUTE WS-SEQ-NO = EN-SEQ-NO - +1
           ELSE
              MOVE +4096               TO WS-SEQ-NO
           END-IF
           MOVE 'EN'                   TO ENDORSEMENT-RECORD
           MOVE OC-CONTROL-PRIMARY     TO EN-CONTROL-PRIMARY
           MOVE EN-COMPANY-CD          TO EN-COMPANY-CD-A1
           MOVE WS-SEQ-NO              TO EN-SEQ-NO
           INITIALIZE EN-ISSUE-RECORD
           MOVE LOW-VALUES             TO EN-LF-ORIG-EXP-DT
                                          EN-AH-ORIG-EXP-DT
                                          EN-LF-NEW-EXP-DT
                                          EN-AH-NEW-EXP-DT
                                          EN-PROCESS-DT
           .
       0410-EXIT.
           EXIT.
       0420-UPDATE-ELCRTO.
           PERFORM 0363-READ-ELCRTO-UPD THRU 0363-EXIT
           IF RESP-NORMAL
              MOVE WS-CURRENT-BIN-DT   TO OC-ENDORSEMENT-PROCESSED-DT
062712                                    OC-LAST-MAINT-DT
062712        MOVE EIBTIME             TO OC-LAST-MAINT-HHMMSS
062712        MOVE PI-PROCESSOR-ID     TO OC-LAST-MAINT-BY
              
      * EXEC CICS REWRITE
      *          FROM     (ORIGINAL-CERTIFICATE)
      *          DATASET  ('ELCRTO')
      *       END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&& L                  %   #00006613' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           .
       0420-EXIT.
           EXIT.
062712
062712 0425-ADD-NEW-ELCRTO.
062712     MOVE SPACES                 TO ORIGINAL-CERTIFICATE
062712     MOVE 'OC'                   TO OC-RECORD-ID
062712     MOVE NEW-ELCRTO-KEY         TO OC-CONTROL-PRIMARY
062712     MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
062712     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
062712     MOVE WS-CURRENT-BIN-DT      TO OC-LAST-MAINT-DT
062712     MOVE NCRTO-INS-FIRST-NAME   TO OC-INS-FIRST-NAME
062712     MOVE NCRTO-INS-MIDDLE-INIT  TO OC-INS-MIDDLE-INIT
062712     MOVE NCRTO-INS-LAST-NAME    TO OC-INS-LAST-NAME
062712     MOVE NCRTO-INS-AGE          TO OC-INS-AGE
121712     MOVE NCRTO-INS-AGE-DEFAULT-FLAG TO OC-INS-AGE-DEFAULT-FLAG
062712     MOVE NCRTO-JNT-FIRST-NAME   TO OC-JNT-FIRST-NAME
062712     MOVE NCRTO-JNT-MIDDLE-INIT  TO OC-JNT-MIDDLE-INIT
062712     MOVE NCRTO-JNT-LAST-NAME    TO OC-JNT-LAST-NAME
062712     MOVE NCRTO-JNT-AGE          TO OC-JNT-AGE
121712     MOVE NCRTO-JNT-AGE-DEFAULT-FLAG TO OC-JNT-AGE-DEFAULT-FLAG
062712     MOVE NCRTO-LF-BENCD         TO OC-LF-BENCD
062712     MOVE NCRTO-LF-BEN-AMT       TO OC-LF-BEN-AMT
062712     MOVE NCRTO-LF-PRM-AMT       TO OC-LF-PRM-AMT
062712     MOVE NCRTO-LF-ALT-BEN-AMT   TO OC-LF-ALT-BEN-AMT
062712     MOVE NCRTO-LF-ALT-PRM-AMT   TO OC-LF-ALT-PRM-AMT
062712     MOVE NCRTO-LF-TERM          TO OC-LF-TERM
062712     MOVE NCRTO-LF-EXP-DT        TO OC-LF-EXP-DT
062712     MOVE NCRTO-LF-COMM-PCT      TO OC-LF-COMM-PCT
062712     MOVE NCRTO-LF-CANCEL-DT     TO OC-LF-CANCEL-DT
062712     MOVE NCRTO-LF-CANCEL-AMT    TO OC-LF-CANCEL-AMT
071712     MOVE NCRTO-LF-ITD-CANCEL-AMT TO OC-LF-ITD-CANCEL-AMT
062712     MOVE NCRTO-AH-BENCD         TO OC-AH-BENCD
062712     MOVE NCRTO-AH-BEN-AMT       TO OC-AH-BEN-AMT
062712     MOVE NCRTO-AH-PRM-AMT       TO OC-AH-PRM-AMT
062712     MOVE NCRTO-AH-TERM          TO OC-AH-TERM
062712     MOVE NCRTO-AH-EXP-DT        TO OC-AH-EXP-DT
062712     MOVE NCRTO-AH-CP            TO OC-AH-CP
062712     MOVE NCRTO-AH-COMM-PCT      TO OC-AH-COMM-PCT
062712     MOVE NCRTO-AH-CANCEL-DT     TO OC-AH-CANCEL-DT
062712     MOVE NCRTO-AH-CANCEL-AMT    TO OC-AH-CANCEL-AMT
071712     MOVE NCRTO-AH-ITD-CANCEL-AMT TO OC-AH-ITD-CANCEL-AMT
062712     MOVE NCRTO-CRED-BENE-NAME   TO OC-CRED-BENE-NAME
062712     MOVE NCRTO-1ST-PMT-DT       TO OC-1ST-PMT-DT
011413     MOVE 'Y'                    TO OC-ISSUE-TRAN-IND
011413     MOVE 'N'                    TO OC-CANCEL-TRAN-IND
062712     MOVE LOW-VALUES             TO OC-ENDORSEMENT-PROCESSED-DT
062712     .
062712 0425-WRITE-ELCRTO.
062712
062712     
      * EXEC CICS WRITE
062712*       DATASET   ('ELCRTO')
062712*       FROM      (ORIGINAL-CERTIFICATE)
062712*       RIDFLD    (OC-CONTROL-PRIMARY)
062712*       RESP      (WS-RESPONSE)
062712*    END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00006668' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303036363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062712
062712     display ' just wrote new crto ' ws-response
                                 ' ' oc-key-seq-no
062712     IF RESP-DUPKEY
062712        SUBTRACT +1              FROM OC-KEY-SEQ-NO
062712        GO TO 0425-WRITE-ELCRTO
062712     ELSE
062712        IF NOT RESP-NORMAL
062712           MOVE -1               TO SIGREQL
062712           MOVE ER-3830          TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT
062712                                 THRU 9900-EXIT
062712        ELSE
062712           MOVE OC-CONTROL-PRIMARY TO PI-ELCRTO-KEY
062712        END-IF
062712     END-IF
062712
062712     .
062712 0425-EXIT.
062712     EXIT.
062712
072312 0426-ADD-CERT-NOTES.
072312
103012     MOVE LOW-VALUES TO CERT-NOTE-ENTRIES
103012     SET TB-INDX TO 1
103012
072312     IF PI-CERTNT1-ENTERED > SPACES
103012         MOVE PI-CERTNT1-ENTERED TO CERT-NT-TEXT (TB-INDX)
103012         MOVE PI-PROCESSOR-ID    TO
103012                      CERT-NT-LAST-MAINT-BY (TB-INDX)
103012         MOVE EIBTIME            TO
103012                      CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012         MOVE WS-CURRENT-BIN-DT  TO
103012                      CERT-NT-LAST-MAINT-DT (TB-INDX)
103012         SET TB-INDX UP BY +1
072312     END-IF
072312
072312     IF PI-CERTNT2-ENTERED > SPACES
103012         MOVE PI-CERTNT2-ENTERED TO CERT-NT-TEXT (TB-INDX)
103012         MOVE PI-PROCESSOR-ID    TO
103012                      CERT-NT-LAST-MAINT-BY (TB-INDX)
103012         MOVE EIBTIME            TO
103012                      CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012         MOVE WS-CURRENT-BIN-DT  TO
103012                      CERT-NT-LAST-MAINT-DT (TB-INDX)
103012         SET TB-INDX UP BY +1
072312     END-IF
103012
103012     IF CERT-NOTE-ENTRIES > SPACES
103012         PERFORM 0427-LOAD-CURRENT-CERT-NOTES THRU 0427-EXIT
103012         PERFORM 1700-WRITE-CERT-NOTE THRU 1799-EXIT
103012     END-IF
121212
121212     IF PI-BILLNT1-ENTERED > SPACES
121212         MOVE SPACES             TO WS-MANUAL-BILL-NOTE
121212         MOVE PI-BILLNT1-ENTERED TO WS-MAN-BN-NOTE
121212         MOVE +63 TO WS-LEN
121212         PERFORM 0441-UPDATE-BILLING-NOTE THRU 0441-EXIT
121212     END-IF
121212
121212     IF PI-BILLNT2-ENTERED > SPACES
121212         MOVE SPACES             TO WS-MANUAL-BILL-NOTE
121212         MOVE PI-BILLNT2-ENTERED TO WS-MAN-BN-NOTE
121212         MOVE +63 TO WS-LEN
121212         PERFORM 0441-UPDATE-BILLING-NOTE THRU 0441-EXIT
121212     END-IF
072312
072312     .
072312 0426-EXIT.
072312     EXIT.
072312
103012 0427-LOAD-CURRENT-CERT-NOTES.
103012
103012     MOVE PI-CRTO-COMPANY-CD  TO ERCNOT-COMPANY-CD
103012     MOVE PI-CRTO-CARRIER     TO ERCNOT-CARRIER
103012     MOVE PI-CRTO-GROUPING    TO ERCNOT-GROUPING
103012     MOVE PI-CRTO-STATE       TO ERCNOT-STATE
103012     MOVE PI-CRTO-ACCOUNT     TO ERCNOT-ACCOUNT
103012     MOVE PI-CRTO-CERT-EFF-DT TO ERCNOT-EFF-DT
103012     MOVE PI-CRTO-CERT-PRIME  TO ERCNOT-CERT-PRIME
103012     MOVE PI-CRTO-CERT-SFX    TO ERCNOT-CERT-SFX
103012     MOVE '1'                 TO ERCNOT-REC-TYP
103012     MOVE +0                  TO ERCNOT-SEQ
103012     MOVE ERCNOT-PARTIAL-KEY  TO SV-PARTIAL-KEY
103012
103012     
      * EXEC CICS STARTBR
103012*         DATASET(WS-ERCNOT-FILE-ID)
103012*         RIDFLD(ERCNOT-KEY)
103012*         KEYLENGTH(ERCNOT-START-LENGTH)
103012*         RESP      (WS-RESPONSE)
103012*         GENERIC
103012*         GTEQ
103012*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    G          &  N#00006759' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036373539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCNOT-FILE-ID, 
                 ERCNOT-KEY, 
                 ERCNOT-START-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
103012
103012     IF NOT RESP-NORMAL
103012        GO TO 0427-ENDBR
103012     END-IF.
103012
103012 0427-LOOP.
103012     
      * EXEC CICS READNEXT
103012*         SET(ADDRESS OF CERT-NOTE-FILE)
103012*         DATASET(WS-ERCNOT-FILE-ID)
103012*         RIDFLD(ERCNOT-KEY)
103012*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006773' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCNOT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCNOT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERT-NOTE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
103012
103012     IF CZ-COMPANY-CD NOT = PI-CRTO-COMPANY-CD
103012         GO TO 0427-ENDBR
103012     END-IF.
103012
103012     IF (CZ-CARRIER = SV-CARRIER)
103012        AND (CZ-GROUPING = SV-GROUPING)
103012        AND (CZ-STATE = SV-STATE)
103012        AND (CZ-ACCOUNT = SV-ACCOUNT)
103012        AND (CZ-CERT-EFF-DT = SV-EFF-DT)
103012        AND (CZ-CERT-NO = SV-CERT-NO)
103012        AND (CZ-RECORD-TYPE = '1')
103012          MOVE CZ-NOTE TO CERT-NT-TEXT (TB-INDX)
103012          MOVE CZ-LAST-MAINT-USER TO
103012                      CERT-NT-LAST-MAINT-BY (TB-INDX)
103012          MOVE CZ-LAST-MAINT-DT TO
103012                      CERT-NT-LAST-MAINT-DT (TB-INDX)
103012          MOVE CZ-LAST-MAINT-HHMMSS TO
103012                      CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012          SET TB-INDX UP BY 1
103012          GO TO 0427-LOOP
103012     END-IF.
103012
103012 0427-ENDBR.
103012
103012     
      * EXEC CICS ENDBR
103012*         DATASET(WS-ERCNOT-FILE-ID)
103012*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006803' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCNOT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
103012
103012     .
103012 0427-EXIT.
103012     EXIT.
103012
       0430-WRITE-ERENDT.
           DISPLAY ' NO ERRORS, ABOUT TO WRITE ERENDT '
           MOVE EN-CONTROL-PRIMARY     TO PI-ERENDT-KEY
           MOVE W-ARCH-NUMBER          TO EN-ARCHIVE-NO
           
      * EXEC CICS WRITE
      *         DATASET   (WS-ERENDT-FILE-ID)
      *         FROM      (ENDORSEMENT-RECORD)
      *         RIDFLD    (EN-CONTROL-PRIMARY)
      *         RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            ENDORSEMENT-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00006815' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303036383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERENDT-FILE-ID, 
                 ENDORSEMENT-RECORD, 
                 DFHEIV11, 
                 EN-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              MOVE 'V'                 TO PI-DOCUMENT-PROCESSED
              display ' good write erendt '
121112        IF EIBAID = DFHPF4
062712           PERFORM 0420-UPDATE-ELCRTO THRU 0420-EXIT
062712           PERFORM 0425-ADD-NEW-ELCRTO THRU 0425-EXIT
072312           PERFORM 0426-ADD-CERT-NOTES THRU 0426-EXIT
072312           PERFORM 0440-ADD-BILLING-NOTE THRU 0440-EXIT
062712        END-IF
           ELSE
              DISPLAY ' NOT A GOOD WRITE ERENDT ' WS-RESPONSE
              MOVE -1                  TO CARRL
              MOVE ER-0132             TO EMI-ERROR
              MOVE AL-UABON            TO CARRA
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           END-IF
           .
       0430-EXIT.
           EXIT.
072312 0440-ADD-BILLING-NOTE.
072312     
      * EXEC CICS GETMAIN
072312*         SET      (ADDRESS OF EOB-CODES)
072312*         LENGTH   (ELEOBC-LENGTH)
072312*         INITIMG  (GETMAIN-SPACE)
072312*    END-EXEC
      *    MOVE ',"IL                  $   #00006841' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELEOBC-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF EOB-CODES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312
072312     MOVE LOW-VALUES             TO WS-ELEOBC-KEY
072312     MOVE PI-COMPANY-CD          TO WS-EOBC-COMPANY-CD
072312     MOVE '5'                    TO WS-EOBC-REC-TYPE
072312
072312     
      * EXEC CICS STARTBR
072312*        DATASET   ('ELEOBC')
072312*        RIDFLD    (WS-ELEOBC-KEY)
072312*        GTEQ
072312*        RESP      (WS-RESPONSE)
072312*    END-EXEC
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006851' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036383531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312
072312     IF NOT RESP-NORMAL
072312        GO TO 0440-EXIT
072312     END-IF
072312      .
072312 0440-READNEXT-ELEOBC.
072312
072312     
      * EXEC CICS READNEXT
072312*       INTO    (EOB-CODES)
072312*       DATASET ('ELEOBC')
072312*       RIDFLD  (WS-ELEOBC-KEY)
072312*       RESP    (WS-RESPONSE)
072312*    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006864' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036383634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EOB-CODES, 
                 DFHEIV12, 
                 WS-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312
072312     IF RESP-NORMAL
072312         IF EO-RECORD-TYPE NOT = '5'
072312             GO TO 0440-EXIT
072312         END-IF
072312     ELSE
072312         GO TO 0440-EXIT
072312     END-IF
072312
072312     IF EO-RECORD-TYPE = '5' AND
072312        EO-EOB-CODE = PI-LTRID-ENTERED
072312           CONTINUE
072312     ELSE
072312         GO TO 0440-READNEXT-ELEOBC
072312     END-IF
072312
072312     MOVE SPACES TO WS-BILLING-NOTE
072312     MOVE EO-DESCRIPTION TO WS-BN-NOTE
072312     MOVE PI-LTRID-ENTERED TO WS-BN-LTRID
072312     MOVE WS-CURRENT-DT TO WS-BN-DATE
072312     MOVE PI-PROCESSOR-ID TO WS-BN-USERID
121212     MOVE +25 TO WS-LEN
072312
121212     PERFORM 0441-UPDATE-BILLING-NOTE THRU 0441-EXIT
121212     .
121212 0440-EXIT.
121212     EXIT.
121212
121212 0441-UPDATE-BILLING-NOTE.
072312
072312     
      * EXEC CICS GETMAIN
072312*         SET      (ADDRESS OF CERTIFICATE-NOTE)
072312*         LENGTH   (ERNOTE-LENGTH)
072312*         INITIMG  (GETMAIN-SPACE)
072312*    END-EXEC
      *    MOVE ',"IL                  $   #00006900' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERNOTE-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312
072312     MOVE PI-ERENDT-KEY (1:33) TO WS-ERNOTE-KEY
041320     move '1'                    to w-note-record-type
072312
072312     
      * EXEC CICS READ
072312*       DATASET    (WS-ERNOTE-FILE-ID)
072312*       RIDFLD     (WS-ERNOTE-KEY)
072312*       INTO       (CERTIFICATE-NOTE)
072312*       RESP       (WS-RESPONSE)
072312*       UPDATE
072312*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00006909' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERNOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 WS-ERNOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312
072312     IF RESP-NORMAL
072312       IF CN-BILLING-START-LINE-NO NOT NUMERIC
072312          MOVE ZEROS            TO CN-BILLING-START-LINE-NO
072312       END-IF
072312       IF CN-BILLING-END-LINE-NO NOT NUMERIC
072312          MOVE ZEROS            TO CN-BILLING-END-LINE-NO
072312       END-IF
072312       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
072312           (NOTE-SUB > +10) OR
121212           (CN-LINE (NOTE-SUB) (1:WS-LEN) =
121212                             WS-BILLING-NOTE (1:WS-LEN))
072312       END-PERFORM
121212       IF CN-LINE (NOTE-SUB) (1:WS-LEN) =
121212                              WS-BILLING-NOTE (1:WS-LEN)
072312         
      * EXEC CICS UNLOCK
072312*           DATASET    (WS-ERNOTE-FILE-ID)
072312*        END-EXEC
      *    MOVE '&*                    #   #00006931' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERNOTE-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312       ELSE
072312         PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
072312           (NOTE-SUB > +10) OR
072312           (CN-LINE (NOTE-SUB) = SPACES OR LOW-VALUES)
072312         END-PERFORM
072312         IF (NOTE-SUB < +11)
072312           IF NOTE-SUB >= CN-BILLING-START-LINE-NO AND
072312              NOTE-SUB <= CN-BILLING-END-LINE-NO
072312                MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
072312           ELSE
072312             IF (CN-BILLING-END-LINE-NO NOT = ZEROS) AND
072312              (NOTE-SUB = (CN-BILLING-END-LINE-NO + +1))
072312                MOVE WS-BILLING-NOTE   TO CN-LINE (NOTE-SUB)
072312                MOVE NOTE-SUB     TO CN-BILLING-END-LINE-NO
072312             ELSE
072312               IF (CN-BILLING-START-LINE-NO NOT = ZEROS) AND
072312                  (NOTE-SUB = (CN-BILLING-START-LINE-NO - +1))
072312                     MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
072312                     MOVE NOTE-SUB  TO CN-BILLING-START-LINE-NO
072312               ELSE
072312                 IF (CN-BILLING-END-LINE-NO = ZEROS)
072312                   MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB)
072312                   MOVE NOTE-SUB    TO CN-BILLING-END-LINE-NO
072312                                       CN-BILLING-START-LINE-NO
072312                 ELSE
072312                    PERFORM 0442-SQUEEZE-IT-IN THRU 0442-EXIT
072312                 END-IF
072312               END-IF
072312             END-IF
072312           END-IF
072312           MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
072312           MOVE WS-CURRENT-BIN-DT   TO CN-LAST-MAINT-DT
072312           MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
072312           
      * EXEC CICS REWRITE
072312*             DATASET    (WS-ERNOTE-FILE-ID)
072312*             FROM       (CERTIFICATE-NOTE)
072312*             RESP       (WS-RESPONSE)
072312*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00006967' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303036393637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERNOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312           PERFORM 0445-CERTIFICATE-UPDATE THRU 0445-EXIT
072312         END-IF
072312       END-IF
072312     ELSE
072312        MOVE SPACES              TO CERTIFICATE-NOTE
072312        MOVE 'CN'                TO CN-RECORD-ID
072312        MOVE PI-ERENDT-KEY (1:33) TO CN-CONTROL-PRIMARY
072312                                     WS-ERNOTE-KEY
041320        move '1'                 to cn-record-type
041320                                    w-note-record-type
072312        MOVE 01                  TO CN-BILLING-START-LINE-NO
072312                                    CN-BILLING-END-LINE-NO
072312        MOVE WS-BILLING-NOTE     TO CN-LINE (01)
072312        MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
072312        MOVE WS-CURRENT-BIN-DT   TO CN-LAST-MAINT-DT
072312        MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
072312        
      * EXEC CICS WRITE
072312*          DATASET    (WS-ERNOTE-FILE-ID)
072312*          FROM       (CERTIFICATE-NOTE)
072312*          RIDFLD     (WS-ERNOTE-KEY)
072312*          RESP       (WS-RESPONSE)
072312*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00006988' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303036393838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERNOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 WS-ERNOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312
072312        PERFORM 0445-CERTIFICATE-UPDATE THRU 0445-EXIT
072312     END-IF
072312
072312     .
121212 0441-EXIT.
072312     EXIT.
072312
072312
072312 0442-SQUEEZE-IT-IN.
072312
072312     IF NOTE-SUB < CN-BILLING-START-LINE-NO
072312        PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
072312           NOTE-SUB = +10
072312           MOVE CN-LINE (NOTE-SUB + 1) TO CN-LINE (NOTE-SUB)
072312           IF (NOTE-SUB + 1) = (CN-BILLING-START-LINE-NO - 1)
072312             MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB + 1)
072312             COMPUTE CN-BILLING-START-LINE-NO = NOTE-SUB + 1
072312             MOVE +9 TO NOTE-SUB
072312           END-IF
072312        END-PERFORM
072312     ELSE
072312        IF NOTE-SUB > CN-BILLING-END-LINE-NO
072312           PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY -1
072312             UNTIL NOTE-SUB = +1
072312             MOVE CN-LINE (NOTE-SUB - 1) TO CN-LINE (NOTE-SUB)
072312             IF (NOTE-SUB - 1) = (CN-BILLING-END-LINE-NO + 1)
072312                MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB - 1)
072312                COMPUTE CN-BILLING-END-LINE-NO = NOTE-SUB - 1
072312                MOVE +2          TO NOTE-SUB
072312             END-IF
072312           END-PERFORM
072312        END-IF
072312     END-IF
072312
072312     .
072312 0442-EXIT.
072312     EXIT.
072312
072312 0445-CERTIFICATE-UPDATE.
          display 'billing note written, updating cert'
072312     PERFORM 1800-READ-ELCERT-UPDATE THRU 1800-EXIT
072312     IF RESP-NORMAL
072312        EVALUATE CM-NOTE-SW
072312           WHEN '2'
072312           WHEN '3'
072312           WHEN '6'
072312           WHEN '7'
072312              SET NO-CERT-RW     TO TRUE
072312           WHEN ' '
072312              MOVE '2'           TO CM-NOTE-SW
072312           WHEN '1'
072312              MOVE '3'           TO CM-NOTE-SW
072312           WHEN '4'
072312              MOVE '6'           TO CM-NOTE-SW
072312           WHEN '5'
072312              MOVE '7'           TO CM-NOTE-SW
072312        END-EVALUATE
072312     END-IF
072312     IF NOT NO-CERT-RW
072312        PERFORM 1810-REWRITE-ELCERT
072312                                 THRU 1810-EXIT
072312     ELSE
072312        
      * EXEC CICS UNLOCK
072312*          DATASET    (WS-ELCERT-FILE-ID)
072312*       END-EXEC
      *    MOVE '&*                    #   #00007057' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCERT-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312     END-IF
072312
072312     .
072312 0445-EXIT.
072312     EXIT.
072312
       0500-CREATE-TEMP-STORAGE.
           display ' about to write pi ' program-interface-block
           
      * EXEC CICS WRITEQ TS
      *        QUEUE (QID)
      *        FROM  (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH(PI-COMM-LENGTH)
      *    END-EXEC
      *    MOVE '*"     L              ''   #00007068' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           display ' about to write map ' el631ji
           
      * EXEC CICS WRITEQ TS
      *        QUEUE (QID)
      *        FROM  (EL631JI)
      *        LENGTH(MAP-LENGTH)
      *    END-EXEC.
      *    MOVE '*"     L              ''   #00007074' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL631JI, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       0500-EXIT.
            EXIT.
       0600-RECOVER-TEMP-STORAGE.
           display ' about to recover pi '
           
      * EXEC CICS READQ TS
      *        QUEUE (QID)
      *        INTO  (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH(PI-COMM-LENGTH)
      *    END-EXEC
      *    MOVE '*$I    L              ''   #00007083' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           display ' pi ' pi-program-work-area
           display ' about to recover map '
           
      * EXEC CICS READQ TS
      *        QUEUE (QID)
      *        INTO  (EL631JI)
      *        LENGTH(MAP-LENGTH)
      *    END-EXEC
      *    MOVE '*$I    L              ''   #00007090' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL631JI, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           display ' map ' el631ji
           PERFORM 0800-DELETE-TS THRU 0800-EXIT.
           MOVE AL-UANON            TO SIGREQA
           MOVE AL-UANON            TO LTRIDA
           IF WS-SET-CODES-MDT = 'Y'
              move +1                  to s2
              perform varying s1 from +1 by +5 until s1 > +60
                 MOVE WS-PASSED-REASON-CODES (s1:4)
                                       TO reacd-in (s2)
012412           IF REACD-IN (S2) NOT EQUAL SPACES AND LOW-VALUES
012412               MOVE AL-SANON     TO REACDA (S2)
062712               MOVE +4           TO REACDL (S2)
012412           END-IF
                 add +1                to s2
              end-perform
062712        MOVE REASONCDS       TO PI-REASONS-ENTERED
           END-IF
121713
121713     IF ENCCODEI > SPACES
121713         MOVE AL-UANON       TO ENCCODEA
121713     END-IF
072312
072312     IF CRTNT1I > SPACES
072312         MOVE AL-UANON       TO CRTNT1A
072312     END-IF
072312
072312     IF CRTNT2I > SPACES
072312         MOVE AL-UANON       TO CRTNT2A
072312     END-IF
121212
121212     IF BILNT1I > SPACES
121212         MOVE AL-UANON       TO BILNT1A
121212     END-IF
           .
       0600-EXIT.
            EXIT.
       0800-DELETE-TS.
           
      * EXEC CICS DELETEQ TS
      *        QUEUE(QID)
      *    END-EXEC
      *    MOVE '*&                    #   #00007132' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0800-EXIT.
            EXIT.
       1000-BUILD-ERARCH.
           display ' made it to 1000 '
           MOVE SPACES                 TO W-CNTL-KEY
           MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID
           MOVE '1'                    TO W-CNTL-RECORD-TYPE
           MOVE ZEROS                  TO W-CNTL-SEQ-NO
           
      * EXEC CICS READ
      *         DATASET    (WS-ELCNTL-FILE-ID)
      *         SET        (ADDRESS OF CONTROL-FILE)
      *         RIDFLD     (W-CNTL-KEY)
      *         UPDATE
      *         RESP       (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        EU         (  N#00007144' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              display ' good read elcntl '
              IF CF-CREDIT-LAST-ARCH-NUM NOT NUMERIC
                 MOVE ZEROS            TO CF-CREDIT-LAST-ARCH-NUM
              END-IF
              ADD 1                    TO CF-CREDIT-LAST-ARCH-NUM
              MOVE CF-CREDIT-LAST-ARCH-NUM
                                       TO W-ARCH-NUMBER
              
      * EXEC CICS REWRITE
      *          FROM      (CONTROL-FILE)
      *          DATASET   (WS-ELCNTL-FILE-ID)
      *       END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007159' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           
      * EXEC CICS GETMAIN
      *       SET      (ADDRESS OF LETTER-ARCHIVE)
      *       LENGTH   (W-ARCH-LENGTH)
      *    END-EXEC
      *    MOVE '," L                  $   #00007164' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-ARCH-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE 'LA'                   TO LETTER-ARCHIVE
           MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
                                          LA-ARCHIVE-NO-A2
                                          LA-ARCHIVE-NO-A3
                                          LA-ARCHIVE-NO-A4
                                          LA-ARCHIVE-NO-A5
                                          LA-ARCHIVE-NO-A6
           MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
                                          LA-COMPANY-CD-A2
                                          LA-COMPANY-CD-A3
                                          LA-COMPANY-CD-A4
                                          LA-COMPANY-CD-A5
                                          LA-COMPANY-CD-A6
           MOVE PB-CARRIER             TO LA-CARRIER-A2
                                          LA-CARRIER-A3
                                          LA-CARRIER-A4
                                          LA-CARRIER-A5
           MOVE Pb-cert-eff-dt         TO LA-EFFECT-DATE-A2
           MOVE PB-GROUPING            TO LA-GROUPING-A2
                                          LA-GROUPING-A3
                                          LA-GROUPING-A4
                                          LA-GROUPING-A5
           MOVE PB-ACCOUNT             TO LA-ACCOUNT-A2
                                          LA-ACCOUNT-A3
                                          LA-ACCOUNT-A4
                                          LA-ACCOUNT-A5
           MOVE PB-STATE               TO LA-STATE-A2
                                          LA-STATE-A3
                                          LA-STATE-A4
                                          LA-STATE-A5
           MOVE PB-CERT-PRIME          TO LA-CERT-PRIME-A2
           MOVE PB-CERT-SFX            TO LA-CERT-SUFFIX-A2
           MOVE PB-ENTRY-BATCH         TO LA-ENTRY-A6
           MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD
           MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
                                          LA-INITIAL-PRINT-DATE
                                          LA-SENT-DATE
                                          LA-REPLY-DATE
                                          LA-RESEND-DATE
                                          LA-FOLLOW-UP-DATE
           MOVE 'A'                    TO LA-STATUS
           MOVE W-NUMBER-OF-COPIES     TO LA-NO-OF-COPIES
           MOVE W-AUTO-CLOSE-IND       TO LA-FINAL-ACT-IND
           MOVE LTRIDI                 TO LA-FORM-A3
           MOVE '4'                    TO LA-DATA-SOURCE
           MOVE WS-CURRENT-BIN-DT      TO LA-CREATION-DATE
           MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES
                                          LA-NO-OF-TEXT-RECORDS
           
      * EXEC CICS WRITE
      *         DATASET   ('ERARCH')
      *         FROM      (LETTER-ARCHIVE)
      *         RIDFLD    (LA-CONTROL-PRIMARY)
      *         RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
           MOVE 'ERARCH' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00007216' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303037323136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              display ' good write erarch '
              MOVE ER-0280             TO EMI-ERROR
032712        MOVE -1                  TO SIGREQL
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE W-ARCH-NUMBER       TO W-ARCH-SUPPRESS
              MOVE W-ARCH-EDIT         TO EMI-TEXT-VARIABLE (1)
           ELSE
              MOVE ER-7345             TO EMI-ERROR
062712        MOVE -1                  TO SIGREQL
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           .
       1000-EXIT.
           EXIT.
       1100-CALL-NS-BUS-LOGIC.
           DISPLAY ' MADE IT TO 1100 '
           MOVE SPACES                 TO BL-INPUT
           MOVE W-ARCH-NUMBER          TO BL-ARCHIVE-NO
062712     IF PI-RETURN-TO-PROGRAM = 'EL1273'
120313       OR 'EL6314'
062712         MOVE '2'                TO BL-DATA-SRCE
062712         MOVE SPACES             TO BL-BATCH-NO
062712         MOVE ZEROS              TO BL-BATCH-SEQ
062712     ELSE
062712         MOVE '4'                TO BL-DATA-SRCE
062712         MOVE PB-ENTRY-BATCH     TO BL-BATCH-NO
062712         MOVE PB-BATCH-SEQ-NO    TO BL-BATCH-SEQ
062712     END-IF
062712     MOVE PI-CARRIER             TO BL-CARRIER
062712     MOVE PI-GROUPING            TO BL-GROUP
062712     MOVE PI-STATE               TO BL-STATE
062712     MOVE PI-ACCOUNT             TO BL-ACCOUNT
062712     IF PI-CERT-EFF-DT NOT = LOW-VALUES
062712        MOVE PI-CERT-EFF-DT      TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-date-link
              IF NO-CONVERSION-ERROR
062712           MOVE DC-GREG-DATE-1-EDIT
                                       TO BL-EFF-DT
              END-IF
           END-IF
062712     MOVE PI-CERT-NO             TO BL-CERT-NO
           MOVE SPACES                 TO BL-RESP-NO
           MOVE LTRIDI                 TO BL-LETTER-ID
           MOVE W-NUMBER-OF-COPIES     TO BL-NO-OF-COPIES
           MOVE PI-PROCESSOR-ID        TO BL-PROC-ID
100412     MOVE 'CrtVerif'               TO BL-FUNC
      *    MOVE 'ALWA'                 TO BL-PROC-ID
           MOVE PI-COMPANY-ID          TO BL-COMP-ID
      *    MOVE IFF-PRINT-NOW-SW       TO BL-PRINT-NOW-SW
      *    MOVE IFF-ENC-CD             TO BL-ENC-CD
121713     MOVE PI-ENCCODE             TO BL-ENC-CD
110612     MOVE W-ARCH-NUMBER          TO BL-ENDT-ARCH-NO
102212     MOVE 'VERIFY  '             TO BL-SOURCE-SCREEN
121112     IF EIBAID = DFHPF4
              MOVE 'B'                 TO BL-WRITE-ERARCH
           ELSE
              MOVE 'T'                 TO BL-WRITE-ERARCH
           END-IF
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
              MOVE REACD-IN (S1)       TO BL-REASON-CODE (S1)
           END-PERFORM
PEMTST    DISPLAY ' ABOUT TO LINK TO NSRASBL '
      *****************************************
      * Invoke the LETTER business logic
      *****************************************
           
      * exec cics link
      *       program('NSRASBL')
      *       commarea(srch-commarea)
      *    end-exec.
           MOVE LENGTH OF
            srch-commarea
             TO DFHEIV11
           MOVE 'NSRASBL' TO DFHEIV1
      *    MOVE '."C                   (   #00007291' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           DISPLAY ' MADE IT BACK FROM NSRASBL ' BL-STATUS ' '
              BL-MESSAGE
           .
       1100-EXIT.
           EXIT.
       1500-GET-ARCH-NO.
           display ' made it to 1500 '
           MOVE SPACES                 TO ELCNTL-KEY
           MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
           MOVE '1'                    TO CNTL-REC-TYPE
           MOVE ZEROS                  TO CNTL-SEQ
           
      * EXEC CICS READ
      *         DATASET    (WS-ELCNTL-FILE-ID)
      *         SET        (ADDRESS OF CONTROL-FILE)
      *         RIDFLD     (ELCNTL-KEY)
      *         UPDATE
      *         RESP       (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        EU         (  N#00007306' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037333036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              display ' good read elcntl '
              IF CF-CREDIT-LAST-ARCH-NUM NOT NUMERIC
                 MOVE ZEROS            TO CF-CREDIT-LAST-ARCH-NUM
              END-IF
              ADD 1                    TO CF-CREDIT-LAST-ARCH-NUM
              MOVE CF-CREDIT-LAST-ARCH-NUM
                                       TO W-ARCH-NUMBER
              display ' new arch number ' w-arch-number
              
      * EXEC CICS REWRITE
      *          FROM      (CONTROL-FILE)
      *          DATASET   (WS-ELCNTL-FILE-ID)
      *       END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007322' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           .
       1500-EXIT.
           EXIT.
072312
072312
072312 1700-WRITE-CERT-NOTE      SECTION.
072312
103012
103012     MOVE SV-PARTIAL-KEY TO ERCNOT-PARTIAL-KEY.
103012     MOVE ZERO           TO ERCNOT-SEQ.
103012     SET TB-INDX         DOWN BY 1.
103012     SET TB-INDX1        TO TB-INDX.
103012
103012****DELETE CURRENT CERT NOTES
103012 1700-LOOP.
103012     
      * EXEC CICS READ
103012*        DATASET (WS-ERCNOT-FILE-ID)
103012*        RIDFLD  (ERCNOT-KEY)
103012*        SET     (ADDRESS OF CERT-NOTE-FILE)
103012*        RESP    (WS-RESPONSE)
103012*        GTEQ
103012*    END-EXEC.
      *    MOVE '&"S        G          (  N#00007342' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCNOT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCNOT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERT-NOTE-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
103012
103012     IF NOT RESP-NORMAL
103012        GO TO 1700-ENDDEL
103012     END-IF
103012
103012     MOVE CZ-CONTROL-PRIMARY     TO ERCNOT-KEY.
103012
103012     IF ERCNOT-PARTIAL-KEY NOT = SV-PARTIAL-KEY
103012         GO TO 1700-ENDDEL
103012     END-IF.
103012
103012     
      * EXEC CICS DELETE
103012*        DATASET (WS-ERCNOT-FILE-ID)
103012*        RIDFLD  (ERCNOT-KEY)
103012*    END-EXEC.
      *    MOVE '&(  R                 &   #00007360' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCNOT-FILE-ID, 
                 ERCNOT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
103012
103012     GO TO 1700-LOOP.
103012
103012 1700-ENDDEL.
103012
103012     
      * EXEC CICS GETMAIN
103012*         LENGTH(ERCNOT-LENGTH)
103012*         SET(ADDRESS OF CERT-NOTE-FILE)
103012*         INITIMG(GETMAIN-SPACE)
103012*    END-EXEC.
      *    MOVE ',"IL                  $   #00007369' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCNOT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERT-NOTE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
103012
103012     SET TB-INDX TO 1.
103012     MOVE SV-PARTIAL-KEY TO ERCNOT-PARTIAL-KEY
103012     MOVE +0             TO ERCNOT-SEQ
103012
103012     PERFORM VARYING TB-INDX FROM 1 BY 1
103012             UNTIL TB-INDX > TB-INDX1
103012        MOVE SPACES                 TO  CERT-NOTE-FILE
103012        ADD 1                       TO  ERCNOT-SEQ
103012        MOVE ERCNOT-KEY             TO  CZ-CONTROL-PRIMARY
103012        MOVE  'CZ'                  TO  CZ-RECORD-ID
103012        MOVE CERT-NT-TEXT (TB-INDX) TO  CZ-NOTE
103012        MOVE CERT-NT-LAST-MAINT-BY (TB-INDX)
103012                                    TO  CZ-LAST-MAINT-USER
103012        MOVE CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012                                    TO  CZ-LAST-MAINT-HHMMSS
103012        MOVE CERT-NT-LAST-MAINT-DT (TB-INDX)
103012                                    TO  CZ-LAST-MAINT-DT
103012
103012        
      * EXEC CICS WRITE
103012*            DATASET(WS-ERCNOT-FILE-ID)
103012*            FROM(CERT-NOTE-FILE)
103012*            RIDFLD(ERCNOT-KEY)
103012*       END-EXEC
           MOVE LENGTH OF
            CERT-NOTE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007393' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ERCNOT-FILE-ID, 
                 CERT-NOTE-FILE, 
                 DFHEIV11, 
                 ERCNOT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
103012     END-PERFORM
072312
072312     PERFORM 1800-READ-ELCERT-UPDATE THRU 1800-EXIT
072312     IF RESP-NORMAL
072312        EVALUATE CM-NOTE-SW
072312           WHEN '1'
072312           WHEN '3'
072312           WHEN '5'
072312           WHEN '7'
072312              SET NO-CERT-RW     TO TRUE
072312           WHEN ' '
072312              MOVE '1'           TO CM-NOTE-SW
072312           WHEN '2'
072312              MOVE '3'           TO CM-NOTE-SW
072312           WHEN '4'
072312              MOVE '5'           TO CM-NOTE-SW
072312           WHEN '6'
072312              MOVE '7'           TO CM-NOTE-SW
072312        END-EVALUATE
072312     END-IF
072312     IF NOT NO-CERT-RW
072312        PERFORM 1810-REWRITE-ELCERT
072312                                 THRU 1810-EXIT
072312     ELSE
072312        
      * EXEC CICS UNLOCK
072312*          DATASET    (WS-ELCERT-FILE-ID)
072312*       END-EXEC
      *    MOVE '&*                    #   #00007422' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCERT-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312     END-IF
072312
072312     .
072312 1799-EXIT.
072312     EXIT.
072312
072312 1800-READ-ELCERT-UPDATE.
072312
072312     MOVE PI-COMPANY-CD          TO  W-CERT-COMPANY-CD.
072312     MOVE PI-CRTO-CARRIER        TO  W-CERT-CARRIER.
072312     MOVE PI-CRTO-GROUPING       TO  W-CERT-GROUPING.
072312     MOVE PI-CRTO-STATE          TO  W-CERT-STATE.
072312     MOVE PI-CRTO-ACCOUNT        TO  W-CERT-ACCOUNT.
072312     MOVE PI-CRTO-CERT-EFF-DT    TO  W-CERT-CERT-EFF-DT.
072312     MOVE PI-CRTO-CERT-PRIME     TO  W-CERT-CERT-PRIME.
072312     MOVE PI-CRTO-CERT-SFX       TO  W-CERT-CERT-SFX.
072312
072312     
      * EXEC CICS READ
072312*        UPDATE
072312*        DATASET  (WS-ELCERT-FILE-ID)
072312*        RIDFLD   (WS-ELCERT-KEY)
072312*        SET      (ADDRESS OF CERTIFICATE-MASTER)
072312*        RESP     (WS-RESPONSE)
072312*    END-EXEC
      *    MOVE '&"S        EU         (  N#00007442' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072312
072312     .
072312 1800-EXIT.
072312     EXIT.
072312
072312 1810-REWRITE-ELCERT.
072312
072312     
      * EXEC CICS REWRITE
072312*        FROM     (CERTIFICATE-MASTER)
072312*        DATASET  (WS-ELCERT-FILE-ID)
072312*        RESP     (WS-RESPONSE)
072312*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00007456' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
072312
072312     .
072312 1810-EXIT.
072312     EXIT.
072312
       8100-SEND-INITIAL-MAP.
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK.
           MOVE DC-GREG-DATE-1-EDIT    TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
062712     MOVE -1                     TO SIGREQL.
012412     IF PI-PROCESSOR-IS-CSR-SUPER
              perform varying s1 from +1 by +1 until
                 s1 > +12
                 MOVE AL-UANON            TO reacda (s1)
              end-perform
012412     END-IF
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
           MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
           
      * EXEC CICS SEND
      *        MAP      (MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL631JO)
      *        ERASE
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL631JO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007482' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL631JO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 9100-RETURN-TRAN.
       8200-SEND-DATAONLY.
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK.
           MOVE DC-GREG-DATE-1-EDIT    TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE -1                     TO PFENTRL
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
           MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O
           
      * EXEC CICS SEND
      *        MAP      (MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL631JO)
      *        DATAONLY
      *        ERASEAUP
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL631JO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00007500' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL631JO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 9100-RETURN-TRAN.
       8300-SEND-TEXT.
           
      * EXEC CICS SEND TEXT
      *        FROM     (LOGOFF-TEXT)
      *        LENGTH   (LOGOFF-LENGTH)
      *        ERASE
      *        FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007510' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS RETURN
      *    END-EXEC.
      *    MOVE '.(                    ''   #00007516' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8800-UNAUTHORIZED-ACCESS.
           MOVE UNACCESS-MSG           TO LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.
       8810-PF23.
           MOVE EIBAID                 TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           GO TO 9300-XCTL.
       9000-RETURN-CICS.
           
      * EXEC CICS RETURN
      *    END-EXEC.
      *    MOVE '.(                    ''   #00007526' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
           MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
           
      * EXEC CICS RETURN
      *        TRANSID    (TRANS-ID)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.(CT                  ''   #00007531' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9200-RETURN-MAIN-MENU.
           MOVE XCTL-626               TO PGM-NAME.
           GO TO 9300-XCTL.
       9300-XCTL.
           
      * EXEC CICS XCTL
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.$C                   %   #00007540' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9400-CLEAR.
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
           
      * EXEC CICS XCTL
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.$C                   %   #00007547' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9500-PF12.
           MOVE XCTL-010               TO PGM-NAME.
           GO TO 9300-XCTL.
       9600-PGMID-ERROR.
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR    (8300-SEND-TEXT)
      *    END-EXEC.
      *    MOVE '"$L                   ! " #00007556' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303037353536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
           MOVE ' '                    TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           MOVE PGM-NAME               TO LOGOFF-PGM.
           MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
           GO TO 9300-XCTL.
       9700-DATE-LINK.
           MOVE LINK-ELDATCV           TO PGM-NAME.
           
      * EXEC CICS LINK
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (DATE-CONVERSION-DATA)
      *        LENGTH     (DC-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '."C                   (   #00007567' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9900-ERROR-FORMAT.
           IF NOT EMI-ERRORS-COMPLETE
              MOVE LINK-001            TO PGM-NAME
              
      * EXEC CICS LINK
      *           PROGRAM    (PGM-NAME)
      *           COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
      *           LENGTH     (EMI-COMM-LENGTH)
      *       END-EXEC
      *    MOVE '."C                   (   #00007575' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           .
       9900-EXIT.
           EXIT.
       9990-ABEND.
           MOVE LINK-004               TO PGM-NAME.
           MOVE DFHEIBLK               TO EMI-LINE1.
           
      * EXEC CICS LINK
      *        PROGRAM   (PGM-NAME)
      *        COMMAREA  (EMI-LINE1)
      *        LENGTH    (72)
      *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007587' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 8200-SEND-DATAONLY.
           
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6317' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9995-SECURITY-VIOLATION.
      *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00007612' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
       9995-EXIT.
           EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6317' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6317' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
