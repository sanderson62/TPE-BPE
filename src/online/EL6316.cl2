       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL6316.
      *
      *AUTHOR.        PABLO
      *               OMAHA, NEBRASKA

      *DATE-COMPILED.

      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
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
110410* 110410   2007070900001  PEMA  ADD REFORMATIONS AND RESCISSIONS
012412* 012412   2011022800001  AJRA  NAPERSOFT
062712* 062712   2011022800001  AJRA  REDEFINE ORIG DATA
072312* 072312   2011022800001  AJRA  NAPERSOFT MISC
091812* 091812 IR2012091400003  AJRA  CHANGE MSG 9999
101512* 101512   2011022800001  AJRA  FIX CLM RESC DEFAULT
102212* 102212   2012101700002  AJRA  SCREEN ID
103012* 103012   2012101700002  AJRA  ADD CERT NOTES TO TOP OF LIST
110612* 110612   2012101700002  AJRA  ADD NEW FIELDS
112612* 112612   2012101700002  AJRA  CHECK FOR PROMPT LETTER
121212* 121212   2012101700002  AJRA  ADD BILLING NOTE ENTRY
121712* 121712   2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413 IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
121713* 121713   2013090300001  AJRA  ADD ENCLOSURE CODE TO SCREEN
122313* 122313   2013090300001  AJRA  DO ENDORSEMENT FROM CERT SCREEN
041320* 041320 CR2020030500002  PEMA  Issue, cancel billing notes
052307*-----------------------------------------------------------------

      *REMARKS.
      *        TRANSACTION - EXBC - CANCEL CHANGES
       EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL6316 WORKING STORAGE    *'.
       77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'.
       77  NOTE-SUB PIC S9(5) COMP-3 VALUE +0.
       77  WS-SEQ-NO                   PIC 9(4) BINARY.
       77  W-ARCH-NUMBER               PIC 9(8) BINARY  VALUE ZEROS.
       77  w-prtsw                     pic x  value spaces.
       77  WS-SET-CODES-MDT            PIC X  VALUE SPACES.

       77  WS-ERACCT-SW                PIC X  VALUE ' '.
           88  ACCT-FOUND                 VALUE 'Y'.
072312 77  WS-CHGBACK                  PIC 99  VALUE ZEROS.
       77  WS-CSO-PORTION              PIC S9(5)V99 COMP-3 VALUE +0.
       77  WS-ACCT-PORTION             PIC S9(5)V99 COMP-3 VALUE +0.
       77  WS-DIFF                     PIC 99 VALUE ZEROS.
       77  WS-SAVE-ERACCT-KEY          PIC X(20)  VALUE SPACES.
       77  WS-DUE-BORROWER             PIC S9(7)V99 COMP-3 VALUE +0.

       77  s1                          pic s999 comp-3 value +0.
       77  s2                          pic s999 comp-3 value +0.
062712 77  WS-REASON-SW                PIC X  VALUE ' '.
062712     88  REASONS-FOUND                  VALUE 'Y'.
062712     88  REASONS-NOT-FOUND              VALUE 'N'.
072312 77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
072312     88  NO-CERT-RW                 VALUE 'N'.
072312     88  CERT-RW                    VALUE 'Y'.
072312 77  WS-COMM-LENGTH              PIC 9(4) BINARY.
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

           COPY ELCSCTM.

           COPY ELCSCRTY.

       01 srch-commarea.
                                       copy ELCADLTRSPI.

012412
012412*** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
012412                                 COPY ELCZREC.

       01  WS-PASSED-REASON-CODES      PIC X(60)  VALUE SPACES.

       01  STANDARD-AREAS.
00102      12  W-ARCH-SUPPRESS         PIC ZZZZZZZ9.
00103      12  W-ARCH-EDIT REDEFINES W-ARCH-SUPPRESS
00104                                  PIC  X(08).
00142      12  QID.
00143          16  QID-TERM        PIC X(4)    VALUE SPACES.
00144          16  FILLER          PIC X(4)    VALUE '631I'.
           12  TIME-MT                 PIC S9(7).
           12  TIME-MT-R  REDEFINES TIME-MT.
               16  FILLER              PIC X.
               16  TIME-LMT            PIC 99V99.
               16  FILLER              PIC X(2).
           12  GETMAIN-SPACE           PIC X VALUE SPACES.
           12  MAP-LENGTH              PIC 9(4)   VALUE 359 BINARY.
PEMMOD     12  ERENDT-LENGTH           PIC S9(04) VALUE +579 COMP.
           12  ERPNDB-LENGTH           PIC 9(04)  VALUE 585 COMP.
072312     12  ERCNOT-LENGTH           PIC S9(04) VALUE +150 COMP.
072312     12  ERNOTE-LENGTH           PIC S9(04) VALUE +825 COMP.
072312     12  ELEOBC-LENGTH           PIC S9(04) VALUE +350 COMP.
121713     12  ELENCC-LENGTH           PIC S9(04) VALUE +400 COMP.
072312     12  WS-ELCERT-KEY.
072312         16  W-CERT-COMPANY-CD   PIC X.
072312         16  W-CERT-CARRIER      PIC X.
072312         16  W-CERT-GROUPING     PIC X(6).
072312         16  W-CERT-STATE        PIC XX.
072312         16  W-CERT-ACCOUNT      PIC X(10).
072312         16  W-CERT-CERT-EFF-DT  PIC XX.
072312         16  W-CERT-CERT-PRIME   PIC X(10).
072312         16  W-CERT-CERT-SFX     PIC X.
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
041320         16  W-NOTE-RECORD-TYPE  PIC X.               
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
122313     12  WS-ELCRTT-KEY.
122313         16  WS-ELCRTT-PRIMARY       PIC X(33).
122313         16  WS-ELCRTT-REC-TYPE      PIC X(1).
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
           12  WS-ERACCT-KEY.
               16  ACCT-COMPANY-CD     PIC X.
               16  ACCT-CARRIER        PIC X.
               16  ACCT-GROUPING       PIC X(6).
               16  ACCT-STATE          PIC XX.
               16  ACCT-ACCOUNT        PIC X(10).
               16  ACCT-EXP-DATE       PIC XX    VALUE SPACES.
               16  FILLER              PIC X(4)  VALUE LOW-VALUES.
           12  WS-ELCNTL-FILE-ID       PIC X(8) VALUE 'ELCNTL'.
           12  WS-ERNOTE-FILE-ID       PIC X(8) VALUE 'ERNOTE'.
           12  WS-ELCRTO-FILE-ID       PIC X(8) VALUE 'ELCRTO'.
           12  WS-ERENDT-FILE-ID       PIC X(8) VALUE 'ERENDT'.
           12  WS-ERPNDB2-FILE-ID      PIC X(8) VALUE 'ERPNDB2'.
           12  WS-ERPNDB-FILE-ID       PIC X(8) VALUE 'ERPNDB'.
           12  WS-ERACCT2-FILE-ID      PIC X(8) VALUE 'ERACCT2'.
072312     12  WS-ELCERT-FILE-ID       PIC X(8) VALUE 'ELCERT'.
072312     12  WS-ERCNOT-FILE-ID       PIC X(8) VALUE 'ERCNOT'.
072312     12  WS-ELEOBC-FILE-ID       PIC X(8) VALUE 'ELEOBC'.
121713     12  WS-ELENCC-FILE-ID       PIC X(8) VALUE 'ELENCC'.
122313     12  WS-ELCRTT-FILE-ID       PIC X(8) VALUE 'ELCRTT'.
122313     12  WS-ERMAIL-FILE-ID       PIC X(8) VALUE 'ERMAIL'.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
062712         88  RESP-DUPREC                  VALUE +14.
062712         88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  MAP-NAME            PIC X(8)    VALUE 'EL631I'.
           12  MAPSET-NAME         PIC X(8)    VALUE 'EL6316S'.
           12  SCREEN-NUMBER       PIC X(4)    VALUE '631I'.
           12  TRANS-ID            PIC X(4)    VALUE 'EXBC'.
           12  THIS-PGM            PIC X(8)    VALUE 'EL6316'.
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
           12  XCTL-6317           PIC X(8)    VALUE 'EL6317'.
           12  XCTL-6318           PIC X(8)    VALUE 'EL6318'.
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
122313     12  WS-TMPDATE.
122313         16  FILLER              PIC X(2).
122313         16  WS-TMPDT            PIC X(6).
122313     12  WS-AMTX                 PIC X(10).
122313     12  WS-AMTN  REDEFINES WS-AMTX PIC 9(10).
122313     12  WS-AMTV2 REDEFINES WS-AMTN PIC 9(8)V99.
           
062712     12  NEW-ORIG-REC.
062712         16  NEW-INS-LAST-NAME             PIC X(15).
062712         16  NEW-INS-FIRST-NAME            PIC X(10).
062712         16  NEW-INS-MIDDLE-INIT           PIC X.
062712         16  NEW-INS-AGE                   PIC S999     COMP-3.
062712         16  NEW-JNT-LAST-NAME             PIC X(15).
062712         16  NEW-JNT-FIRST-NAME            PIC X(10).
062712         16  NEW-JNT-MIDDLE-INIT           PIC X.
062712         16  NEW-JNT-AGE                   PIC S999     COMP-3.
062712         16  NEW-LF-BENCD                  PIC XX.
062712         16  NEW-LF-TERM                   PIC S999      COMP-3.
062712         16  NEW-LF-BEN-AMT                PIC S9(9)V99  COMP-3.
062712         16  NEW-LF-PRM-AMT                PIC S9(7)V99  COMP-3.
062712         16  NEW-LF-ALT-BEN-AMT            PIC S9(9)V99  COMP-3.
062712         16  NEW-LF-ALT-PRM-AMT            PIC S9(7)V99  COMP-3.
062712         16  NEW-LF-EXP-DT                 PIC XX.
062712         16  NEW-LF-COMM-PCT               PIC SV9(5)    COMP-3.
062712         16  NEW-LF-CANCEL-DT              PIC XX.
062712         16  NEW-LF-CANCEL-AMT             PIC S9(7)V99  COMP-3.
071712         16  NEW-LF-ITD-CANCEL-AMT         PIC S9(7)V99  COMP-3.
062712         16  NEW-AH-BENCD                  PIC XX.
062712         16  NEW-AH-TERM                   PIC S999      COMP-3.
062712         16  NEW-AH-BEN-AMT                PIC S9(9)V99  COMP-3.
062712         16  NEW-AH-PRM-AMT                PIC S9(7)V99  COMP-3.
062712         16  NEW-AH-EXP-DT                 PIC XX.
062712         16  NEW-AH-COMM-PCT               PIC SV9(5)    COMP-3.
062712         16  NEW-AH-CP                     PIC 99.
062712         16  NEW-AH-CANCEL-DT              PIC XX.
062712         16  NEW-AH-CANCEL-AMT             PIC S9(7)V99  COMP-3.
071712         16  NEW-AH-ITD-CANCEL-AMT         PIC S9(7)V99  COMP-3.
062712         16  NEW-CRED-BENE-NAME            PIC X(25).
062712         16  NEW-1ST-PMT-DT                PIC XX.
121712         16  NEW-INS-AGE-DEFAULT-FLAG      PIC X.
121712         16  NEW-JNT-AGE-DEFAULT-FLAG      PIC X.
011413         16  NEW-ISSUE-TRAN-IND            PIC X.
011413         16  NEW-CANCEL-TRAN-IND           PIC X.
011413         16  FILLER                        PIC X(235).
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
091812     12  ER-3829                 PIC X(4)  VALUE '3829'.
062712     12  ER-3830                 PIC X(4)  VALUE '3830'.
062712     12  ER-3833                 PIC X(4)  VALUE '3833'.
           12  ER-7638                 PIC X(4)  VALUE '7638'.
           12  ER-8888                 PIC X(4)  VALUE '8888'.
           12  ER-9999                 PIC X(4)  VALUE '9999'.

           COPY ELCDATE.

           COPY ELCLOGOF.

           COPY ELCATTR.

           COPY ELCEMIB.

           COPY ELCTEXT.

           COPY ELCINTF.
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
072312        16  PI-PREV-ERENDT-KEY      PIC X(36).
062712        16  PI-RES-REF-CLM-TYPE     PIC X.
062712        16  PI-CLM-RESC-IND         PIC X.
062712            88  PI-CLM-RESCISSION      VALUE 'Y'.
062712        16  PI-CLEAR-ERROR-SW       PIC X.
062712            88  PI-CLEAR-ERROR         VALUE 'Y'.
062712        16  PI-PRTSW-ENTERED        PIC X.
062712        16  PI-LTRID-ENTERED        PIC X(4).
062712        16  PI-REASONS-ENTERED      PIC X(84).
072312        16  PI-CERTNT1-ENTERED      PIC X(63).
072312        16  PI-CERTNT2-ENTERED      PIC X(63).
072312        16  FILLER                  PIC X(20).
072312        16  PI-FINALIZED-IND        PIC X.
121212        16  PI-BILLNT1-ENTERED      PIC X(63).
121713        16  PI-ENCCODE              PIC X(3).
122313        16  PI-FROM-CERT            PIC X.
122313        16  PI-ENDORSE-RECORD       PIC X.
122313        16  FILLER                  PIC X(212).
          12  FILLER                      PIC X(276).

           COPY ELCAID.

       01  FILLER    REDEFINES DFHAID.
           12  FILLER              PIC X(8).
           12  PF-VALUES           PIC X       OCCURS 24 TIMES.

          COPY EL6316S.

       01  FILLER REDEFINES EL631II.
122313     12  FILLER                   PIC X(355).
062712     12  REASONCDS.
062712         16  FILLER OCCURS 12.
062712             20  REACDL       PIC S9(4) COMP.
062712             20  REACDA       PIC X.
062712             20  REACD-IN     PIC X(4).
072312     12  FILLER               PIC X(215).


       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(1300).

       01  var  pic x(30).
                                       COPY ERCACCT.
                                       COPY ELCCNTL.
                                       COPY ERCPNDB.
                                       COPY ELCCERT.
                                       COPY ELCCRTO.
                                       COPY ERCENDT.
                                       COPY ERCNOTE.
                                       COPY ERCMAIL.
072312                                 COPY ERCCNOT.
072312                                 COPY ELCEOBC.
121713                                 COPY ELCENCC.
122313                                 COPY ELCCRTT.

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT
           MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT
072312     MOVE EIBTRMID               TO QID-TERM

           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF

           DISPLAY ' ENDORSE LENGTH  '
                      FUNCTION LENGTH(ENDORSEMENT-RECORD)
           move function length(EL631II) TO MAP-LENGTH
           DISPLAY ' MAP LENGTH      ' MAP-LENGTH

122313     IF PI-CALLING-PROGRAM = 'EL6314'
122313         MOVE 'Y'                    TO PI-FROM-CERT
122313         MOVE THIS-PGM               TO PI-CALLING-PROGRAM
122313         MOVE LOW-VALUES             TO EL631II
122313         MOVE 'Y'                    TO PI-CLEAR-ERROR-SW
122313         GO TO 0350-DISPLAY-COMMON-DATA
122313     END-IF
122313
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
                 MOVE LOW-VALUES             TO EL631II
062712           MOVE 'Y'                  TO PI-CLEAR-ERROR-SW
                 GO TO 0350-DISPLAY-COMMON-DATA
              END-IF
           END-IF

           IF PI-CALLING-PROGRAM = 'EL614'
              MOVE PI-PROGRAM-WORK-AREA (1:60)
                                       TO WS-PASSED-REASON-CODES
              DISPLAY ' COMING BACK FROM 614 ' WS-PASSED-REASON-CODES
              IF WS-PASSED-REASON-CODES NOT = SPACES
                 MOVE 'Y'                TO WS-SET-CODES-MDT
              ELSE
                 MOVE 'N'                TO WS-SET-CODES-MDT
              END-IF
              PERFORM 0610-RECOVER-TEMP-STORAGE
                                       THRU 0610-EXIT 
122313        PERFORM 0325-EDIT-SCREEN THRU 0325-EXIT
              GO TO 0350-DISPLAY-COMMON-DATA
           END-IF

           IF EIBAID = DFHCLEAR
062712        IF PI-CLEAR-ERROR
062712           MOVE SPACES            TO PI-CLEAR-ERROR-SW
062712           MOVE ER-3833           TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 0350-DISPLAY-COMMON-DATA
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

           EXEC CICS RECEIVE
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               INTO     (EL631II)
           END-EXEC

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

           IF EIBAID = DFHPF1
              MOVE PI-ERENDT-KEY       TO ERENDT-KEY
              PERFORM 0520-ERENDT-STARTBR
                                       THRU 0520-EXIT
              PERFORM 0510-ERENDT-READNEXT
                                       THRU 0510-EXIT
072312        IF ERENDT-KEY = PI-PREV-ERENDT-KEY
                  PERFORM 0510-ERENDT-READNEXT
                                       THRU 0510-EXIT
072312        END-IF
              IF (RESP-NORMAL) AND
072312         (PI-ERENDT-KEY (1:34) = EN-CONTROL-PRIMARY (1:34))
                 PERFORM 0390-BUILD-FROM-ERENDT
                                       THRU 0390-EXIT
122313           MOVE 'Y'              TO PI-ENDORSE-RECORD
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8100-SEND-INITIAL-MAP
              ELSE
                 MOVE ER-2237          TO EMI-ERROR
                 MOVE -1               TO CARRL
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF

           IF EIBAID = DFHPF2
              MOVE PI-ERENDT-KEY       TO ERENDT-KEY
              PERFORM 0520-ERENDT-STARTBR
                                       THRU 0520-EXIT
              PERFORM 0510-ERENDT-READNEXT
                                       THRU 0510-EXIT
              PERFORM 0530-ERENDT-READPREV
                                       THRU 0530-EXIT
              PERFORM 0530-ERENDT-READPREV
                                       THRU 0530-EXIT
              IF (RESP-NORMAL) AND
072312         (PI-ERENDT-KEY (1:34) = EN-CONTROL-PRIMARY (1:34))
                 PERFORM 0390-BUILD-FROM-ERENDT
                                       THRU 0390-EXIT
122313           MOVE 'Y'              TO PI-ENDORSE-RECORD
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8100-SEND-INITIAL-MAP
              ELSE
122313           MOVE 'N'              TO PI-ENDORSE-RECORD
122313           MOVE LOW-VALUES       TO  EL631II
072312           GO TO 0350-DISPLAY-COMMON-DATA
                 MOVE ER-2238          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO CARRL
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF
122313
122313     IF (EIBAID = DFHPF3 OR DFHPF4 OR DFHPF8) AND
122313        PI-FROM-CERT = 'Y' AND
122313        NOT PI-PROCESSOR-IS-CSR-SUPER
122313           GO TO 0300-CONTINUE
122313     END-IF
122313
122313     IF (EIBAID = DFHPF3 OR DFHPF4 OR DFHPF8) AND
122313        PI-ENDORSE-RECORD = 'Y'
122313           GO TO 0300-CONTINUE
122313     END-IF

           IF EIBAID = DFHPF3
122313        IF PI-FROM-CERT = 'Y'
122313         AND PI-PROCESSOR-IS-CSR-SUPER
122313            MOVE PI-ELCRTO-KEY      TO ELCRTO-KEY
122313            PERFORM 0363-READ-ELCRTO-UPD THRU 0363-EXIT
122313            PERFORM 0785-CHG-CRTO-REC THRU 0785-EXIT
122313        END-IF
122313
062712        MOVE 'Y'                 TO PI-CLEAR-ERROR-SW
              PERFORM 0600-CREATE-TEMP-STORAGE
                                       THRU 0600-EXIT
              MOVE 'EL614'             TO PGM-NAME
              MOVE SPACES              TO PI-PROGRAM-WORK-AREA
              MOVE +1                  TO S2
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
                 IF REACD-IN (S1) NOT = SPACES AND LOW-VALUES
                    MOVE REACD-IN (S1) TO PI-PROGRAM-WORK-AREA (S2:4)
                    ADD +5 TO S2
                 END-IF
              END-PERFORM
              DISPLAY ' GOING TO 614 ' PI-PROGRAM-WORK-AREA (1:60)
              GO TO 9300-XCTL
           END-IF


           IF EIBAID = DFHPF4 OR DFHPF8
              GO TO 1000-FINALIZE
           END-IF
122313
122313      .
122313 0300-CONTINUE.

           IF EIBAID = DFHPF23
062712        IF PI-CLEAR-ERROR
062712           MOVE SPACES            TO PI-CLEAR-ERROR-SW
062712           MOVE ER-3833           TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 0350-DISPLAY-COMMON-DATA
062712        ELSE
062712           GO TO 8810-PF23
062712        END-IF
           END-IF

           IF EIBAID = DFHPF24
062712        IF PI-CLEAR-ERROR
062712           MOVE SPACES            TO PI-CLEAR-ERROR-SW
062712           MOVE ER-3833           TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 0350-DISPLAY-COMMON-DATA
062712        ELSE
062712           GO TO 9200-RETURN-MAIN-MENU
062712        END-IF
           END-IF

           IF EIBAID = DFHPF12
              GO TO 9500-PF12
           END-IF
122313
122313     IF EIBAID = DFHENTER
122313      AND PI-FROM-CERT = 'Y'
122313      AND PI-PROCESSOR-IS-CSR-SUPER
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
122313         MOVE PI-ELCRTO-KEY          TO ELCRTO-KEY
122313         PERFORM 0363-READ-ELCRTO-UPD THRU 0363-EXIT
122313         PERFORM 0785-CHG-CRTO-REC THRU 0785-EXIT
122313         PERFORM 0755-READ-CERT THRU 0755-EXIT
122313         PERFORM 0770-MOVE-CERT-TO-SCREEN THRU 0770-EXIT
122313*         MOVE ER-0000          TO EMI-ERROR
122313*         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122313*         GO TO 8100-SEND-INITIAL-MAP
122313     END-IF

           IF EIBAID = DFHENTER
062712        MOVE 'Y'                 TO PI-CLEAR-ERROR-SW
              PERFORM 0325-EDIT-SCREEN THRU 0325-EXIT
062712        IF EMI-NO-ERRORS
062712           GO TO 0350-DISPLAY-COMMON-DATA
062712        ELSE
062712           GO TO 8200-SEND-DATAONLY
062712        END-IF
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

           IF PRTSWL > ZEROS
              IF PRTSWI = 'Y' OR 'N' OR 'P' OR ' '
072312*          move prtswi to w-prtsw
072312*          MOVE AL-UANON         TO PRTSWA
072312*          IF PRTSWI = ' '
062712              MOVE 'P'         TO PRTSWI
072312*          END-IF
072312*          MOVE PRTSWI         TO PI-PRTSW-ENTERED
              END-IF
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
121713        EXEC CICS READ
121713            DATASET    (WS-ELENCC-FILE-ID)
121713            SET        (ADDRESS OF ENCLOSURE-CODES)
121713            RIDFLD     (WS-ELENCC-KEY)
121713            RESP       (WS-RESPONSE)
121713        END-EXEC
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
           .
       0325-EXIT.
           EXIT.
112612
112612 0326-GET-Z-RECORD.
112612
112612     MOVE SPACES  TO W-Z-CONTROL-DATA
112612     
112612     EXEC CICS STARTBR
112612          DATASET    ('ELLETR')
112612          RIDFLD     (ELLETR-KEY)
112612          GTEQ
112612          RESP      (WS-RESPONSE)
112612     END-EXEC.
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
112612     EXEC CICS READNEXT
112612         DATASET   ('ELLETR')
112612         INTO      (TEXT-FILES)
112612         RIDFLD    (ELLETR-KEY)
112612         RESP      (WS-RESPONSE)
112612     END-EXEC
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
112612     EXEC CICS ENDBR
112612         DATASET     ('ELLETR')
112612     END-EXEC.
112612
112612 0326-EXIT.
112612      EXIT.

       0350-DISPLAY-COMMON-DATA.

           display ' made it to 0350 '
122313
122313     IF PI-FROM-CERT = 'Y'
122313         PERFORM 0700-LOAD-FROM-CERT THRU 0700-EXIT
122313     ELSE
122313         PERFORM 0360-CHECK-PENDING THRU 0360-EXIT
122313     END-IF

062712     IF PI-LTRID-ENTERED > SPACES
062712        MOVE PI-LTRID-ENTERED    TO LTRIDI
062712        MOVE AL-UANON            TO LTRIDA
062712        MOVE +4                  TO LTRIDL
062712     ELSE
062712        IF PI-CLM-RESCISSION
062712            MOVE 'CLRC'          TO LTRIDI
062712                                    PI-LTRID-ENTERED
062712            MOVE +4              TO LTRIDL
062712            MOVE AL-UANON        TO LTRIDA
062712        END-IF
062712     END-IF
121713
121713     IF PI-ENCCODE > SPACES
121713        MOVE PI-ENCCODE          TO ENCCODEI
121713        MOVE AL-UANON            TO ENCCODEA
121713        MOVE +3                  TO ENCCODEL
121713     END-IF
062712
101512     SET REASONS-NOT-FOUND TO TRUE
062712     MOVE PI-REASONS-ENTERED     TO REASONCDS
062712     PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
062712        IF REACDL (S1) = +4 OR
062712          (REACD-IN (S1) NOT = SPACES AND LOW-VALUES)
062712              MOVE AL-SANON      TO REACDA (S1)
062712              MOVE +4            TO REACDL (S1)
062712              SET REASONS-FOUND  TO TRUE
062712        END-IF
062712     END-PERFORM
072312
072312     IF REASONS-NOT-FOUND
072312         IF PI-CLM-RESCISSION
072312            MOVE 'CRNA'          TO REACD-IN (1)
072312            MOVE AL-SANON        TO REACDA (1)
072312            MOVE +4              TO REACDL (1)
072312            SET REASONS-FOUND    TO TRUE
072312        END-IF
072312     END-IF
072312
072312     IF PI-CERTNT1-ENTERED > SPACES
072312        MOVE PI-CERTNT1-ENTERED  TO CRTNT1I
072312        MOVE AL-UANON            TO CRTNT1A
072312        MOVE +63                 TO CRTNT1L
072312     END-IF
072312
072312     IF PI-CERTNT2-ENTERED > SPACES
072312        MOVE PI-CERTNT2-ENTERED  TO CRTNT2I
072312        MOVE AL-UANON            TO CRTNT2A
072312        MOVE +63                 TO CRTNT2L
072312     END-IF
072312
121212     IF PI-BILLNT1-ENTERED > SPACES
121212        MOVE PI-BILLNT1-ENTERED  TO BILNT1I
121212        MOVE AL-UANON            TO BILNT1A
121212        MOVE +63                 TO BILNT1L
121212     END-IF

           GO TO 8100-SEND-INITIAL-MAP

           .
       0350-EXIT.
           EXIT.

       0360-CHECK-PENDING.

           display ' made it to 0360 '

           MOVE PI-COMPANY-CD          TO CRTO-COMPANY-CD
           MOVE PI-CARRIER             TO CRTO-CARRIER
           MOVE PI-GROUPING            TO CRTO-GROUPING
           MOVE PI-STATE               TO CRTO-STATE
           MOVE PI-ACCOUNT             TO CRTO-ACCOUNT
           MOVE PI-CERT-PRIME          TO CRTO-CERT-PRIME
           MOVE PI-CERT-SFX            TO CRTO-CERT-SFX
           MOVE PI-CERT-EFF-DT         TO CRTO-CERT-EFF-DT
062712     MOVE 'I'                    TO CRTO-RECORD-TYPE
           MOVE ZEROS                  TO CRTO-SEQ-NO
           MOVE ELCRTO-KEY             TO PI-ELCRTO-KEY
072312     MOVE ELCRTO-KEY (1:33)      TO PI-ERENDT-KEY (1:33)
072312     MOVE 'C'                    TO PI-ENDT-RECORD-TYPE
072312     MOVE ZEROS                  TO PI-ENDT-SEQ-NO
072312     MOVE PI-ERENDT-KEY          TO PI-PREV-ERENDT-KEY  

           PERFORM 0362-READ-ELCRTO    THRU 0362-EXIT
            display 'response ' ws-response
            display 'pi-elcrto-key ' pi-elcrto-key
            display 'oc key ' oc-control-primary

           IF RESP-NORMAL
012412       IF (PI-ELCRTO-KEY (1:33) =
012412              OC-CONTROL-PRIMARY (1:33))
062712          DISPLAY 'CRTO RESP NORMAL '
012412          MOVE OC-CONTROL-PRIMARY  TO PI-ELCRTO-KEY
012412          PERFORM 0361-MOVE-CRTO-TO-SCREEN THRU 0361-EXIT
012412          PERFORM 0370-READ-ERPNDB THRU 0370-EXIT
012412          IF RESP-NORMAL
012412             MOVE PB-CONTROL-PRIMARY TO PI-ERPNDB-KEY
012412             PERFORM 0380-BUILD-FROM-ERPNDB THRU 0380-EXIT
012412          END-IF
012412       ELSE
091812          MOVE ER-3829           TO EMI-ERROR
012412          PERFORM 9900-ERROR-FORMAT
012412                                 THRU 9900-EXIT
012412          MOVE -1                TO CARRL
012412          GO TO 8100-SEND-INITIAL-MAP
012412       END-IF
           ELSE
091812        MOVE ER-3829             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           .
       0360-EXIT.
           EXIT.

       0361-MOVE-CRTO-TO-SCREEN.

           display ' made it to 0361 '

062712     MOVE OC-LF-CANCEL-DT          TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO OLFDTO
           END-IF
062712     MOVE OC-AH-CANCEL-DT          TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO OAHDTO
           END-IF

122313     IF OC-LF-CANCEL-AMT NUMERIC
122313        MOVE OC-LF-CANCEL-AMT      TO OLFAMTO
122313     ELSE
122313        MOVE ZEROS                 TO OLFAMTO
122313     END-IF
122313     IF OC-AH-CANCEL-AMT NUMERIC
122313        MOVE OC-AH-CANCEL-AMT      TO OAHAMTO
122313     ELSE
122313        MOVE ZEROS                 TO OAHAMTO
122313     END-IF

           .
       0361-EXIT.
           EXIT.

       0362-READ-ELCRTO.

           display ' made it to 0362 '
           EXEC CICS READ
               DATASET   (WS-ELCRTO-FILE-ID)
               SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
               RIDFLD    (ELCRTO-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           .
       0362-EXIT.
           EXIT.

       0363-READ-ELCRTO-UPD.

           display ' made it to 0363 '
           EXEC CICS READ
               DATASET   (WS-ELCRTO-FILE-ID)
               SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
               RIDFLD    (ELCRTO-KEY)
               UPDATE
               RESP      (WS-RESPONSE)
           END-EXEC

           .
       0363-EXIT.
           EXIT.
012412
012412 0364-MOVE-ZERO-TO-SCREEN.
012412
012412     MOVE ZEROS    TO OLFDTO
012412                      OAHDTO
012412                      OLFAMTO
012412                      OAHAMTO.
012412
012412 0364-EXIT.
012412     EXIT.
012412

       0365-READ-ERENDT.

           EXEC CICS READ
               DATASET   (WS-ERENDT-FILE-ID)
               SET       (ADDRESS OF ENDORSEMENT-RECORD)
               RIDFLD    (ERENDT-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC

           .
       0365-EXIT.
           EXIT.

       0370-READ-ERPNDB.

           display ' made it to 0370 '
           MOVE PI-COMPANY-CD          TO PNDB2-COMPANY-CD
           MOVE PI-CRTO-CARRIER        TO PNDB2-CARRIER
           MOVE PI-CRTO-GROUPING       TO PNDB2-GROUPING
           MOVE PI-CRTO-STATE          TO PNDB2-STATE
           MOVE PI-CRTO-ACCOUNT        TO PNDB2-ACCOUNT
           MOVE PI-CRTO-CERT-PRIME     TO PNDB2-CERT-PRIME
           MOVE PI-CRTO-CERT-SFX       TO PNDB2-CERT-SFX
           MOVE PI-CRTO-CERT-EFF-DT    TO PNDB2-CERT-EFF-DT
           MOVE +0                     TO PNDB2-CHG-SEQ-NO
           MOVE '2'                    TO PNDB2-RECORD-TYPE


           EXEC CICS READ
               DATASET   (WS-ERPNDB2-FILE-ID)
               SET       (ADDRESS OF PENDING-BUSINESS)
               RIDFLD    (ERPNDB2-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC


           .
       0370-EXIT.
           EXIT.

       0380-BUILD-FROM-ERPNDB.

           display ' made it to 0380 '

           MOVE PB-CARRIER             TO CARRO
           MOVE PB-GROUPING            TO GROUPO
           MOVE PB-STATE               TO STATEO
           MOVE PB-ACCOUNT             TO ACCTO
           MOVE PB-CERT-PRIME          TO CERTNOO
           MOVE PB-CERT-SFX            TO CRTSFXO

           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           END-IF

           MOVE PB-CI-LAST-NAME        TO LNAMEO
           MOVE PB-CI-FIRST-NAME       TO FNAMEO
           MOVE PB-CI-INITIALS (2:1)   TO MINITO

           MOVE PB-C-LF-CANCEL-DT        TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO NLFDTO
           END-IF
           MOVE PB-C-AH-CANCEL-DT        TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO NAHDTO
           END-IF

           MOVE PB-C-LF-CANCEL-AMT       TO NLFAMTO
           MOVE PB-C-AH-CANCEL-AMT       TO NAHAMTO

           IF PB-CI-LIFE-COMMISSION NOT = ZEROS
              MOVE PB-CI-LIFE-COMMISSION
                                    TO LFCOMMO
      *       MOVE AL-UNNON         TO LFCOMMA
           END-IF
           IF PB-CI-AH-COMMISSION NOT = ZEROS
              MOVE PB-CI-AH-COMMISSION
                                    TO AHCOMMO
      *       MOVE AL-UNNON         TO AHCOMMA
           END-IF

           MOVE ER-2785                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT

           .
       0380-EXIT.
           EXIT.

       0390-BUILD-FROM-ERENDT.

           MOVE LOW-VALUES             TO EL631II
072312*    move PI-PRTSW-ENTERED       to prtswo
072312     IF PI-CERTNT1-ENTERED > SPACES
072312        MOVE PI-CERTNT1-ENTERED  TO CRTNT1I
072312        MOVE AL-UANON            TO CRTNT1A
072312        MOVE +63                 TO CRTNT1L
072312     END-IF
072312
072312     IF PI-CERTNT2-ENTERED > SPACES
072312        MOVE PI-CERTNT2-ENTERED  TO CRTNT2I
072312        MOVE AL-UANON            TO CRTNT2A
072312        MOVE +63                 TO CRTNT2L
072312     END-IF
121212
121212     IF PI-BILLNT1-ENTERED > SPACES
121212        MOVE PI-BILLNT1-ENTERED  TO BILNT1I
121212        MOVE AL-UANON            TO BILNT1A
121212        MOVE +63                 TO BILNT1L
121212     END-IF
121212
121713     IF PI-ENCCODE > SPACES
121713         MOVE PI-ENCCODE         TO ENCCODEO
121713         MOVE AL-UANON           TO ENCCODEA
121713         MOVE +3                 TO ENCCODEL
121713     END-IF.
121713
           MOVE PI-COMPANY-CD          TO PI-ENDT-COMPANY-CD
           MOVE EN-CARRIER             TO PI-ENDT-CARRIER
           MOVE EN-GROUPING            TO PI-ENDT-GROUPING
           MOVE EN-STATE               TO PI-ENDT-STATE
           MOVE EN-ACCOUNT             TO PI-ENDT-ACCOUNT
           MOVE EN-CERT-PRIME          TO PI-ENDT-CERT-PRIME
           MOVE EN-CERT-SFX            TO PI-ENDT-CERT-SFX
           MOVE EN-CERT-EFF-DT         TO PI-ENDT-CERT-EFF-DT
           MOVE EN-REC-TYPE            TO PI-ENDT-RECORD-TYPE
           MOVE EN-SEQ-NO              TO PI-ENDT-SEQ-NO
072312     MOVE PI-ERENDT-KEY          TO PI-PREV-ERENDT-KEY

           MOVE EN-CARRIER             TO CARRO
           MOVE EN-GROUPING            TO GROUPO
           MOVE EN-STATE               TO STATEO
           MOVE EN-ACCOUNT             TO ACCTO
           MOVE EN-CERT-PRIME          TO CERTNOO
           MOVE EN-CERT-SFX            TO CRTSFXO
           MOVE EN-SEQ-NO              TO SEQNOO
           MOVE EN-TEMPLATE-USED       TO LTRIDO
122313
122313     IF PI-FROM-CERT = 'Y'
122313         PERFORM 0755-READ-CERT THRU 0755-EXIT
122313         MOVE CM-INSURED-LAST-NAME TO LNAMEO
122313         MOVE CM-INSURED-FIRST-NAME TO FNAMEO
122313         MOVE CM-INSURED-INITIAL2  TO MINITO
122313         IF CM-LIFE-COMM-PCT NOT = ZEROS
122313             MOVE CM-LIFE-COMM-PCT TO LFCOMMO
122313         END-IF
122313         IF CM-AH-COMM-PCT NOT = ZEROS
122313             MOVE CM-AH-COMM-PCT   TO AHCOMMO
122313         END-IF
122313     ELSE
122313        PERFORM 0370-READ-ERPNDB THRU 0370-EXIT
122313        MOVE PB-CI-LAST-NAME        TO LNAMEO
122313        MOVE PB-CI-FIRST-NAME       TO FNAMEO
122313        MOVE PB-CI-INITIALS (2:1)   TO MINITO
122313        IF PB-CI-LIFE-COMMISSION NOT = ZEROS
122313           MOVE PB-CI-LIFE-COMMISSION TO LFCOMMO
122313        END-IF
122313        IF PB-CI-AH-COMMISSION NOT = ZEROS
122313           MOVE PB-CI-AH-COMMISSION TO AHCOMMO
122313        END-IF
122313     END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
              IF EN-REASON-CODE (S1) NOT = SPACES
                 MOVE EN-REASON-CODE (S1) TO REACD-IN (S1)
062712           MOVE AL-SANON         TO REACDA (S1)
062712           MOVE +4               TO REACDL (S1)
              END-IF
           END-PERFORM

           MOVE EN-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           END-IF


           IF EN-PROCESS-DT NOT = SPACES AND LOW-VALUES
              MOVE EN-PROCESS-DT       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO PROCDTO
              END-IF
           END-IF

           IF EN-INPUT-DT NOT = SPACES AND LOW-VALUES
              MOVE EN-INPUT-DT         TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO INPDTO
              END-IF
           END-IF

           MOVE EN-LF-ORIG-REF-DT        TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO OLFDTO
           END-IF
           MOVE EN-LF-ORIG-REF-AMT       TO OLFAMTO

           MOVE EN-AH-ORIG-REF-DT        TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO OAHDTO
           END-IF
           MOVE EN-AH-ORIG-REF-AMT       TO OAHAMTO

           MOVE EN-LF-NEW-REF-DT         TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO NLFDTO
           END-IF
           MOVE EN-LF-NEW-REF-AMT        TO NLFAMTO

           MOVE EN-AH-NEW-REF-DT         TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO NAHDTO
           END-IF
           MOVE EN-AH-NEW-REF-AMT        TO NAHAMTO

           MOVE EN-ARCHIVE-NO            TO ARCHNOO
           .
       0390-EXIT.
           EXIT.

       0400-ADD-ERENDT.

           PERFORM 1500-GET-ARCH-NO    THRU 1500-EXIT

           EXEC CICS GETMAIN
                SET      (ADDRESS OF ENDORSEMENT-RECORD)
                LENGTH   (ERENDT-LENGTH)
                INITIMG  (GETMAIN-SPACE)
           END-EXEC
       
           PERFORM 0410-INIT-ERENDT    THRU 0410-EXIT
           MOVE PI-PROCESSOR-ID        TO EN-LAST-MAINT-BY
           MOVE EIBTIME                TO EN-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DT      TO EN-LAST-MAINT-DT
                                          EN-INPUT-DT

           MOVE OC-LF-CANCEL-DT        TO EN-LF-ORIG-REF-DT
           MOVE OC-AH-CANCEL-DT        TO EN-AH-ORIG-REF-DT

           MOVE OC-LF-CANCEL-AMT       TO EN-LF-ORIG-REF-AMT
           MOVE OC-AH-CANCEL-AMT       TO EN-AH-ORIG-REF-AMT
071712
071712     MOVE OC-LF-ITD-CANCEL-AMT   TO NEW-LF-ITD-CANCEL-AMT
071712     MOVE OC-AH-ITD-CANCEL-AMT   TO NEW-AH-ITD-CANCEL-AMT
072312
072312     MOVE OC-INS-LAST-NAME       TO EN-INS-LAST-NAME
072312     MOVE OC-INS-FIRST-NAME      TO EN-INS-FIRST-NAME
072312     MOVE OC-INS-MIDDLE-INIT     TO EN-INS-MIDDLE-INIT
110612
110612     MOVE OC-LF-COMM-PCT         TO EN-LF-ORIG-REF-COMM-PCT
110612     MOVE OC-AH-COMM-PCT         TO EN-AH-ORIG-REF-COMM-PCT
122313
122313     IF PI-FROM-CERT = 'Y'
122313         DISPLAY 'USING CERT FOR NEW'
122313
122313         MOVE PI-COMPANY-CD       TO  W-CERT-COMPANY-CD
122313         MOVE PI-CRTO-CARRIER     TO  W-CERT-CARRIER
122313         MOVE PI-CRTO-GROUPING    TO  W-CERT-GROUPING
122313         MOVE PI-CRTO-STATE       TO  W-CERT-STATE
122313         MOVE PI-CRTO-ACCOUNT     TO  W-CERT-ACCOUNT
122313         MOVE PI-CRTO-CERT-EFF-DT TO  W-CERT-CERT-EFF-DT
122313         MOVE PI-CRTO-CERT-PRIME  TO  W-CERT-CERT-PRIME
122313         MOVE PI-CRTO-CERT-SFX    TO  W-CERT-CERT-SFX
122313
122313         EXEC CICS READ
122313             DATASET  (WS-ELCERT-FILE-ID)
122313             RIDFLD   (WS-ELCERT-KEY)
122313             SET      (ADDRESS OF CERTIFICATE-MASTER)
122313             RESP     (WS-RESPONSE)
122313         END-EXEC
122313         IF NOT RESP-NORMAL
122313            GO TO 0400-EXIT
122313         END-IF
122313
122313         MOVE CM-LF-CANCEL-DT    TO EN-LF-NEW-REF-DT
122313                                    NEW-LF-CANCEL-DT
122313         MOVE CM-AH-CANCEL-DT    TO EN-AH-NEW-REF-DT
122313                                    NEW-AH-CANCEL-DT
122313
122313         MOVE CM-LF-ITD-CANCEL-AMT TO EN-LF-NEW-REF-AMT
122313                                      NEW-LF-CANCEL-AMT
122313         MOVE CM-AH-ITD-CANCEL-AMT TO EN-AH-NEW-REF-AMT
122313                                      NEW-AH-CANCEL-AMT
122313
122313         MOVE CM-LIFE-COMM-PCT   TO EN-LF-NEW-COMM-PCT
122313                                    EN-LF-NEW-REF-COMM-PCT
122313                                    NEW-LF-COMM-PCT
122313
122313         MOVE CM-AH-COMM-PCT     TO EN-AH-NEW-COMM-PCT
122313                                    EN-AH-NEW-REF-COMM-PCT
122313                                    NEW-AH-COMM-PCT
122313
122313
122313         GO TO 0400-CONTINUE
122313     END-IF.
122313
           EXEC CICS READ
               DATASET   (WS-ERPNDB-FILE-ID)
               SET       (ADDRESS OF PENDING-BUSINESS)
               RIDFLD    (PI-ERPNDB-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 0400-EXIT
           END-IF

           MOVE PB-C-LF-CANCEL-DT      TO EN-LF-NEW-REF-DT
062712                                    NEW-LF-CANCEL-DT
           MOVE PB-C-AH-CANCEL-DT      TO EN-AH-NEW-REF-DT
062712                                    NEW-AH-CANCEL-DT

           MOVE PB-C-LF-CANCEL-AMT     TO EN-LF-NEW-REF-AMT
062712                                    NEW-LF-CANCEL-AMT
           MOVE PB-C-AH-CANCEL-AMT     TO EN-AH-NEW-REF-AMT
062712                                    NEW-AH-CANCEL-AMT
           MOVE PB-CI-LIFE-COMMISSION  TO EN-LF-COMMISSION
110612                                    EN-LF-NEW-REF-COMM-PCT
110612                                    NEW-LF-COMM-PCT
           MOVE PB-CI-AH-COMMISSION    TO EN-AH-COMMISSION
110612                                    EN-AH-NEW-REF-COMM-PCT
110612                                    NEW-AH-COMM-PCT.
122313     MOVE PB-ENTRY-BATCH         TO EN-BATCH-NUMBER.
122313
122313 0400-CONTINUE.
122313
           MOVE LTRIDI                 TO EN-TEMPLATE-USED
072312     MOVE 'C'                    TO EN-DOCU-TYPE
072312     MOVE 'Y'                    TO EN-MONEY-SW
072312     MOVE W-ACCT-SUMM            TO EN-ACCT-SUMM
072312     MOVE W-CSO-SUMM             TO EN-CSO-SUMM
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
              IF REACD-IN (S1) NOT = LOW-VALUES
                 MOVE REACD-IN (S1)    TO EN-REASON-CODE (S1)
              END-IF
           END-PERFORM
           
072312     PERFORM 0450-GET-ERACCT     THRU 0450-EXIT
072312
072312     IF ACCT-FOUND
122313        IF PI-FROM-CERT = 'Y'
122313            MOVE CM-CERT-EFF-DT  TO DC-BIN-DATE-1
122313            MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-2
122313            IF CM-LF-CANCEL-DT > DC-BIN-DATE-2
122313                MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-2
122313            END-IF
122313        ELSE
122313            MOVE PB-CERT-EFF-DT      TO DC-BIN-DATE-1
122313            MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-2
122313            IF PB-C-LF-CANCEL-DT > DC-BIN-DATE-2
122313               MOVE PB-C-LF-CANCEL-DT TO DC-BIN-DATE-2
122313            END-IF
122313        END-IF
072312        MOVE '1'                 TO DC-OPTION-CODE
072312        PERFORM 9700-DATE-LINK
072312        IF NO-CONVERSION-ERROR
072312           MOVE DC-ELAPSED-MONTHS TO WS-DIFF
072312           IF DC-ODD-DAYS-OVER > +1
072312              ADD +1          TO WS-DIFF
072312           END-IF
072312        ELSE
072312            MOVE +0           TO WS-DIFF
072312        END-IF
072312     ELSE
072312        MOVE +0           TO WS-DIFF
072312     END-IF
072312
072312     COMPUTE WS-DUE-BORROWER = (EN-LF-NEW-REF-AMT
072312         + EN-AH-NEW-REF-AMT) 
072312
072312     IF (WS-CHGBACK = 99)
072312         OR (WS-DIFF > WS-CHGBACK AND WS-CHGBACK > 0)
072312          MOVE WS-DUE-BORROWER TO EN-CSO-PORTION
072312          MOVE ZEROS           TO EN-ACCT-PORTION
072312          MOVE 'N'             TO EN-COMM-CHGBK
072312     ELSE
072312         COMPUTE EN-ACCT-PORTION = (EN-LF-NEW-REF-AMT
072312                 * EN-LF-COMMISSION) +
072312                (EN-AH-NEW-REF-AMT * EN-AH-COMMISSION)
072312         COMPUTE EN-CSO-PORTION = WS-DUE-BORROWER
072312                 - EN-ACCT-PORTION
072312         MOVE 'Y'                TO EN-COMM-CHGBK
072312     END-IF

           PERFORM 0430-WRITE-ERENDT   THRU 0430-EXIT

           .
       0400-EXIT.
           EXIT.

       0410-INIT-ERENDT.

062712     MOVE OC-CONTROL-PRIMARY (1:33) TO ERENDT-KEY (1:33)
062712     MOVE 'C'                    TO ENDT-RECORD-TYPE
           MOVE ZEROS                  TO ENDT-SEQ-NO

           EXEC CICS READ
               DATASET    (WS-ERENDT-FILE-ID)
               RIDFLD     (ERENDT-KEY)
               SET        (ADDRESS OF ENDORSEMENT-RECORD)
               RESP       (WS-RESPONSE)
               GTEQ
           END-EXEC

           IF RESP-NORMAL
              AND (EN-CONTROL-PRIMARY (1:34) = ERENDT-KEY (1:34))
              COMPUTE WS-SEQ-NO = EN-SEQ-NO - +1
           ELSE
              MOVE +4096               TO WS-SEQ-NO
           END-IF

           MOVE 'EN'                   TO ENDORSEMENT-RECORD
           MOVE OC-CONTROL-PRIMARY     TO EN-CONTROL-PRIMARY
           MOVE EN-COMPANY-CD          TO EN-COMPANY-CD-A1
           MOVE WS-SEQ-NO              TO EN-SEQ-NO
           MOVE WS-CURRENT-BIN-DT      TO EN-PROCESS-DT
012412     MOVE 'C'                    TO EN-REC-TYPE

           INITIALIZE EN-CANCEL-RECORD

           MOVE ZEROS TO EN-CSO-PORTION EN-ACCT-PORTION
           MOVE LOW-VALUES             TO EN-LF-ORIG-REF-DT
                                          EN-AH-ORIG-REF-DT
                                          EN-LF-NEW-REF-DT
                                          EN-AH-NEW-REF-DT
       
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
              EXEC CICS REWRITE
                 FROM     (ORIGINAL-CERTIFICATE)
                 DATASET  ('ELCRTO')
              END-EXEC
           END-IF

           .
       0420-EXIT.
           EXIT.
062712
062712 0425-ADD-NEW-ELCRTO.
062712     MOVE SPACES                 TO ORIGINAL-CERTIFICATE
062712     MOVE 'OC'                   TO OC-RECORD-ID
062712     MOVE PI-ELCRTO-KEY          TO OC-CONTROL-PRIMARY
062712     SUBTRACT +1                 FROM OC-KEY-SEQ-NO
062712     MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
062712     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
062712     MOVE WS-CURRENT-BIN-DT      TO OC-LAST-MAINT-DT
062712     MOVE NEW-INS-FIRST-NAME     TO OC-INS-FIRST-NAME   
062712     MOVE NEW-INS-MIDDLE-INIT    TO OC-INS-MIDDLE-INIT  
062712     MOVE NEW-INS-LAST-NAME      TO OC-INS-LAST-NAME    
062712     MOVE NEW-INS-AGE            TO OC-INS-AGE          
121712     MOVE NEW-INS-AGE-DEFAULT-FLAG TO OC-INS-AGE-DEFAULT-FLAG
062712     MOVE NEW-JNT-FIRST-NAME     TO OC-JNT-FIRST-NAME   
062712     MOVE NEW-JNT-MIDDLE-INIT    TO OC-JNT-MIDDLE-INIT  
062712     MOVE NEW-JNT-LAST-NAME      TO OC-JNT-LAST-NAME    
062712     MOVE NEW-JNT-AGE            TO OC-JNT-AGE          
121712     MOVE NEW-JNT-AGE-DEFAULT-FLAG TO OC-JNT-AGE-DEFAULT-FLAG
062712     MOVE NEW-LF-BENCD           TO OC-LF-BENCD    
062712     MOVE NEW-LF-BEN-AMT         TO OC-LF-BEN-AMT   
062712     MOVE NEW-LF-PRM-AMT         TO OC-LF-PRM-AMT   
062712     MOVE NEW-LF-ALT-BEN-AMT     TO OC-LF-ALT-BEN-AMT
062712     MOVE NEW-LF-ALT-PRM-AMT     TO OC-LF-ALT-PRM-AMT
062712     MOVE NEW-LF-TERM            TO OC-LF-TERM          
062712     MOVE NEW-LF-EXP-DT          TO OC-LF-EXP-DT     
062712     MOVE NEW-LF-COMM-PCT        TO OC-LF-COMM-PCT
062712     MOVE NEW-LF-CANCEL-DT       TO OC-LF-CANCEL-DT    
062712     MOVE NEW-LF-CANCEL-AMT      TO OC-LF-CANCEL-AMT
062712     MOVE NEW-AH-BENCD           TO OC-AH-BENCD 
062712     MOVE NEW-AH-BEN-AMT         TO OC-AH-BEN-AMT
062712     MOVE NEW-AH-PRM-AMT         TO OC-AH-PRM-AMT
062712     MOVE NEW-AH-TERM            TO OC-AH-TERM       
062712     MOVE NEW-AH-EXP-DT          TO OC-AH-EXP-DT  
062712     MOVE NEW-AH-CP              TO OC-AH-CP   
062712     MOVE NEW-AH-COMM-PCT        TO OC-AH-COMM-PCT   
062712     MOVE NEW-AH-CANCEL-DT       TO OC-AH-CANCEL-DT    
062712     MOVE NEW-AH-CANCEL-AMT      TO OC-AH-CANCEL-AMT
062712     MOVE NEW-CRED-BENE-NAME     TO OC-CRED-BENE-NAME
062712     MOVE NEW-1ST-PMT-DT         TO OC-1ST-PMT-DT
011413     MOVE 'N'                    TO OC-ISSUE-TRAN-IND
011413     MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
062712     MOVE LOW-VALUES             TO OC-ENDORSEMENT-PROCESSED-DT
062712     .
062712 0425-WRITE-ELCRTO.
062712
062712     EXEC CICS WRITE
062712        DATASET   ('ELCRTO')
062712        FROM      (ORIGINAL-CERTIFICATE)
062712        RIDFLD    (OC-CONTROL-PRIMARY)
062712        RESP      (WS-RESPONSE)
062712     END-EXEC
062712
062712     display ' just wrote new crto ' ws-response
062712     IF RESP-DUPKEY
062712        SUBTRACT +1              FROM OC-KEY-SEQ-NO
062712        GO TO 0425-WRITE-ELCRTO
062712     ELSE
062712        IF NOT RESP-NORMAL
062712           MOVE -1               TO LTRIDL
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
103012     EXEC CICS STARTBR
103012          DATASET(WS-ERCNOT-FILE-ID)
103012          RIDFLD(ERCNOT-KEY)
103012          KEYLENGTH(ERCNOT-START-LENGTH)
103012          RESP      (WS-RESPONSE)
103012          GENERIC
103012          GTEQ
103012     END-EXEC.
103012
103012     IF NOT RESP-NORMAL
103012        GO TO 0427-ENDBR
103012     END-IF.
103012
103012 0427-LOOP.
103012     EXEC CICS READNEXT
103012          SET(ADDRESS OF CERT-NOTE-FILE)
103012          DATASET(WS-ERCNOT-FILE-ID)
103012          RIDFLD(ERCNOT-KEY)
103012     END-EXEC.
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
103012     EXEC CICS ENDBR
103012          DATASET(WS-ERCNOT-FILE-ID)
103012     END-EXEC.
103012
103012     .
103012 0427-EXIT.
103012     EXIT.
103012

       0430-WRITE-ERENDT.

           DISPLAY ' NO ERRORS, ABOUT TO WRITE ERENDT '
           MOVE EN-CONTROL-PRIMARY     TO PI-ERENDT-KEY
           MOVE W-ARCH-NUMBER          TO EN-ARCHIVE-NO
           EXEC CICS WRITE
                DATASET   (WS-ERENDT-FILE-ID)
                FROM      (ENDORSEMENT-RECORD)
                RIDFLD    (EN-CONTROL-PRIMARY)
                RESP      (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL
              MOVE 'C'                 TO PI-DOCUMENT-PROCESSED
              display ' good write erendt '
062712        IF EIBAID = DFHPF4
062712           PERFORM 0420-UPDATE-ELCRTO THRU 0420-EXIT
062712           PERFORM 0425-ADD-NEW-ELCRTO THRU 0425-EXIT
072312           PERFORM 0426-ADD-CERT-NOTES THRU 0426-EXIT
072312           PERFORM 0440-ADD-BILLING-NOTE THRU 0440-EXIT
062712        END-IF
           ELSE
              DISPLAY ' NOT A GOOD WRITE ERENDT ' WS-RESPONSE
062712        MOVE -1                  TO LTRIDL
              MOVE ER-0132             TO EMI-ERROR
062712        MOVE AL-UABON            TO LTRIDA
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           END-IF

           .
       0430-EXIT.
           EXIT.

072312 0440-ADD-BILLING-NOTE.
072312     EXEC CICS GETMAIN
072312          SET      (ADDRESS OF EOB-CODES)
072312          LENGTH   (ELEOBC-LENGTH)
072312          INITIMG  (GETMAIN-SPACE)
072312     END-EXEC           
072312
072312     MOVE LOW-VALUES             TO WS-ELEOBC-KEY
072312     MOVE PI-COMPANY-CD          TO WS-EOBC-COMPANY-CD
072312     MOVE '5'                    TO WS-EOBC-REC-TYPE
072312
072312     EXEC CICS STARTBR                                            
072312         DATASET   ('ELEOBC')
072312         RIDFLD    (WS-ELEOBC-KEY)
072312         GTEQ
072312         RESP      (WS-RESPONSE)
072312     END-EXEC
072312
072312     IF NOT RESP-NORMAL
072312        GO TO 0440-EXIT
072312     END-IF
072312      .
072312 0440-READNEXT-ELEOBC.
072312
072312     EXEC CICS READNEXT
072312        INTO    (EOB-CODES)
072312        DATASET ('ELEOBC')
072312        RIDFLD  (WS-ELEOBC-KEY)
072312        RESP    (WS-RESPONSE)
072312     END-EXEC
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
072312     EXEC CICS GETMAIN
072312          SET      (ADDRESS OF CERTIFICATE-NOTE)
072312          LENGTH   (ERNOTE-LENGTH)
072312          INITIMG  (GETMAIN-SPACE)
072312     END-EXEC           
072312     
072312     MOVE PI-ERENDT-KEY (1:33) TO WS-ERNOTE-KEY
041320     move '2'                    to w-note-record-type
072312
072312     EXEC CICS READ
072312        DATASET    (WS-ERNOTE-FILE-ID)
072312        RIDFLD     (WS-ERNOTE-KEY)
072312        INTO       (CERTIFICATE-NOTE)
072312        RESP       (WS-RESPONSE)
072312        UPDATE
072312     END-EXEC
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
072312         EXEC CICS UNLOCK
072312            DATASET    (WS-ERNOTE-FILE-ID)
072312         END-EXEC
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
072312           EXEC CICS REWRITE
072312              DATASET    (WS-ERNOTE-FILE-ID)
072312              FROM       (CERTIFICATE-NOTE)
072312              RESP       (WS-RESPONSE)
072312           END-EXEC
072312           PERFORM 0445-CERTIFICATE-UPDATE THRU 0445-EXIT
072312         END-IF
072312       END-IF
072312     ELSE
072312        MOVE SPACES              TO CERTIFICATE-NOTE
072312        MOVE 'CN'                TO CN-RECORD-ID
072312        MOVE PI-ERENDT-KEY (1:33) TO CN-CONTROL-PRIMARY
072312                                     WS-ERNOTE-KEY
041320        move '2'                 to cn-record-type
041320                                    w-note-record-type
072312        MOVE 01                  TO CN-BILLING-START-LINE-NO
072312                                    CN-BILLING-END-LINE-NO
072312        MOVE WS-BILLING-NOTE     TO CN-LINE (01)
072312        MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
072312        MOVE WS-CURRENT-BIN-DT   TO CN-LAST-MAINT-DT
072312        MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
072312        EXEC CICS WRITE
072312           DATASET    (WS-ERNOTE-FILE-ID)
072312           FROM       (CERTIFICATE-NOTE)
072312           RIDFLD     (WS-ERNOTE-KEY)
072312           RESP       (WS-RESPONSE)
072312        END-EXEC
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
072312        EXEC CICS UNLOCK
072312           DATASET    (WS-ELCERT-FILE-ID)
072312        END-EXEC
072312     END-IF
072312
072312     .
072312 0445-EXIT.
072312     EXIT.
072312

       0450-GET-ERACCT.

           DISPLAY ' MADE IT TO 0450 '
           MOVE ' '                    TO WS-ERACCT-SW
072312     MOVE ZEROS                  TO WS-CHGBACK
                                          WS-CSO-PORTION
                                          WS-ACCT-PORTION
                                          WS-DIFF

           PERFORM 0460-STARTBR-ERACCT THRU 0460-EXIT

           IF RESP-NORMAL
              DISPLAY ' GOOD STARTBR '
              PERFORM 0470-READNEXT-ERACCT
                                       THRU 0470-EXIT
           END-IF

           IF RESP-NORMAL
              DISPLAY ' GOOD READNEXT '
              IF AM-CONTROL-PRIMARY (1:20) = EN-CONTROL-PRIMARY (1:20)
                 IF (EN-CERT-EFF-DT < AM-EXPIRATION-DT)
                    AND (EN-CERT-EFF-DT >= AM-EFFECTIVE-DT)
                    DISPLAY ' SETTING ACCT FOUND TO TRUE '
                    SET ACCT-FOUND TO TRUE
                 ELSE
                    PERFORM 0470-READNEXT-ERACCT
                                       THRU 0470-EXIT
                    IF RESP-NORMAL
                       IF AM-CONTROL-PRIMARY (1:20)
                          = EN-CONTROL-PRIMARY (1:20)
                          IF (EN-CERT-EFF-DT < AM-EXPIRATION-DT)
                             AND (EN-CERT-EFF-DT >= AM-EFFECTIVE-DT)
                             DISPLAY ' SETTING ACCT FOUND TO TRUE '
                             SET ACCT-FOUND TO TRUE
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
                    
           IF ACCT-FOUND
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (AM-COM-TYP (S1) = 'C' OR 'D')
                 OR (S1 > +10)
              END-PERFORM
              IF S1 < +11
                 MOVE AM-COMM-CHARGEBACK (S1)
072312                                 TO WS-CHGBACK
              END-IF
           END-IF

072312     EXEC CICS ENDBR
072312         DATASET     ('ERACCT')
072312     END-EXEC.

          .
       0450-EXIT.
           EXIT.

       0460-STARTBR-ERACCT.

           MOVE LOW-VALUES             TO WS-ERACCT-KEY
           MOVE EN-CONTROL-PRIMARY     TO WS-ERACCT-KEY (1:22)
           MOVE WS-ERACCT-KEY          TO WS-SAVE-ERACCT-KEY

           EXEC CICS STARTBR                                            
               DATASET   ('ERACCT')
               RIDFLD    (WS-ERACCT-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           .
       0460-EXIT.
           EXIT.

       0470-READNEXT-ERACCT.

           EXEC CICS READNEXT
              SET      (ADDRESS OF ACCOUNT-MASTER)
              DATASET ('ERACCT')
              RIDFLD  (WS-ERACCT-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

           .
       0470-EXIT.
           EXIT.

       0493-SQUEEZE-ERNOTE.

111109     IF CN-BILLING-START-LINE-NO > 0 
111109         MOVE CN-BILLING-START-LINE-NO TO NOTE-SUB
111109     ELSE
111109         MOVE +1                 TO NOTE-SUB
111109     END-IF.
111109
111109     PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
              (NOTE-SUB > +9)
              IF CN-LINE (NOTE-SUB) = SPACES
                 MOVE CN-LINE (NOTE-SUB + +1)
                                       TO CN-LINE (NOTE-SUB)
                 MOVE SPACES           TO CN-LINE (NOTE-SUB + +1)
              END-IF
           END-PERFORM


           .
       0493-EXIT.
           EXIT.

       0500-ERENDT-READ-GTEQ.

           EXEC CICS READ
               DATASET    (WS-ERENDT-FILE-ID)
               RIDFLD     (ERENDT-KEY)
               SET        (ADDRESS OF ENDORSEMENT-RECORD)
               RESP       (WS-RESPONSE)
               GTEQ
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-2237             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       0500-EXIT.
           EXIT.

       0510-ERENDT-READNEXT.

           EXEC CICS READNEXT
              DATASET    (WS-ERENDT-FILE-ID)
              RIDFLD     (ERENDT-KEY)
              SET        (ADDRESS OF ENDORSEMENT-RECORD)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-2237             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       0510-EXIT.
           EXIT.

       0520-ERENDT-STARTBR.

           EXEC CICS STARTBR
              DATASET    (WS-ERENDT-FILE-ID)
              RIDFLD     (ERENDT-KEY)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-2237             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       0520-EXIT.
           EXIT.

       0530-ERENDT-READPREV.

           EXEC CICS READPREV
              DATASET    (WS-ERENDT-FILE-ID)
              RIDFLD     (ERENDT-KEY)
              SET        (ADDRESS OF ENDORSEMENT-RECORD)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-2238             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
122313        MOVE 'N'                 TO PI-ENDORSE-RECORD
122313        MOVE LOW-VALUES          TO  EL631II
122313        GO TO 0350-DISPLAY-COMMON-DATA
           END-IF

           .
       0530-EXIT.
           EXIT.

       0600-CREATE-TEMP-STORAGE.                          
                                                          
           EXEC CICS WRITEQ TS                            
               QUEUE (QID)                                
               FROM  (PROGRAM-INTERFACE-BLOCK)            
               LENGTH(PI-COMM-LENGTH)                     
           END-EXEC
                                                          
           display ' about to write map ' el631ii
           EXEC CICS WRITEQ TS                            
               QUEUE (QID)                                
               FROM  (EL631II)                            
               LENGTH(MAP-LENGTH)                         
           END-EXEC.                                      
                                                          
       0600-EXIT.                                         
            EXIT.                                         

       0610-RECOVER-TEMP-STORAGE.


           EXEC CICS READQ TS
               QUEUE (QID)
               INTO  (PROGRAM-INTERFACE-BLOCK)
               LENGTH(PI-COMM-LENGTH)
           END-EXEC
                                                             
           EXEC CICS READQ TS                                
               QUEUE (QID)                                   
               INTO  (EL631II)
               LENGTH(MAP-LENGTH)                            
           END-EXEC
                                                             
           display ' map ' EL631II
           PERFORM 0620-DELETE-TS THRU 0620-EXIT
                                                             
           MOVE AL-UANON            TO LTRIDA
122313     IF PI-FROM-CERT = 'Y' AND
122313        PI-PROCESSOR-IS-CSR-SUPER
122313            MOVE AL-UNNON        TO OLFDTA
122313                                    OAHDTA
122313                                    OLFAMTA
122313                                    OAHAMTA
122313     END-IF

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
       0610-EXIT.                                              
            EXIT.                                              

       0620-DELETE-TS.                    
                                          
           EXEC CICS DELETEQ TS           
               QUEUE(QID)                 
           END-EXEC

           .                                          
       0620-EXIT.                         
            EXIT.                         

122313
122313 0700-LOAD-FROM-CERT.
122313     PERFORM 0755-READ-CERT THRU 0755-EXIT
122313     PERFORM 0760-CHECK-OLD-ORIG THRU 0760-EXIT
122313     PERFORM 0770-MOVE-CERT-TO-SCREEN THRU 0770-EXIT
122313     MOVE ER-2784                TO EMI-ERROR
122313     PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
122313     GO TO 8100-SEND-INITIAL-MAP
122313     .                                          
122313 0700-EXIT.                         
122313      EXIT.                         
122313
122313
122313 0755-READ-CERT.
122313*     display 'made it to 0755'
122313     MOVE PI-COMPANY-CD          TO W-CERT-COMPANY-CD
122313     MOVE PI-CARRIER             TO W-CERT-CARRIER
122313     MOVE PI-GROUPING            TO W-CERT-GROUPING
122313     MOVE PI-STATE               TO W-CERT-STATE
122313     MOVE PI-ACCOUNT             TO W-CERT-ACCOUNT
122313     MOVE PI-CERT-PRIME          TO W-CERT-CERT-PRIME
122313     MOVE PI-CERT-SFX            TO W-CERT-CERT-SFX
122313     MOVE PI-CERT-EFF-DT         TO W-CERT-CERT-EFF-DT
122313
122313     EXEC CICS READ
122313         DATASET   (WS-ELCERT-FILE-ID)
122313         SET       (ADDRESS OF CERTIFICATE-MASTER)
122313         RIDFLD    (WS-ELCERT-KEY)
122313         RESP      (WS-RESPONSE)
122313     END-EXEC
122313
122313     IF RESP-NORMAL
122313        CONTINUE
122313     ELSE
122313        IF RESP-NOTFND
122313           MOVE LOW-VALUES       TO EL631II
122313           MOVE ER-2253          TO EMI-ERROR
122313           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122313           MOVE -1               TO CARRL
122313           GO TO 8100-SEND-INITIAL-MAP
122313        END-IF
122313     END-IF
122313
122313     .
122313 0755-EXIT.
122313     EXIT.
122313
122313
122313 0760-CHECK-OLD-ORIG.
122313
           display ' made it to 0760 '
122313
122313     MOVE PI-COMPANY-CD          TO CRTO-COMPANY-CD
122313     MOVE PI-CARRIER             TO CRTO-CARRIER
122313     MOVE PI-GROUPING            TO CRTO-GROUPING
122313     MOVE PI-STATE               TO CRTO-STATE
122313     MOVE PI-ACCOUNT             TO CRTO-ACCOUNT
122313     MOVE PI-CERT-PRIME          TO CRTO-CERT-PRIME
122313     MOVE PI-CERT-SFX            TO CRTO-CERT-SFX
122313     MOVE PI-CERT-EFF-DT         TO CRTO-CERT-EFF-DT
122313     MOVE 'I'                    TO CRTO-RECORD-TYPE
122313     MOVE ZEROS                  TO CRTO-SEQ-NO
122313     MOVE ELCRTO-KEY             TO PI-ELCRTO-KEY
122313     MOVE ELCRTO-KEY (1:33)      TO PI-ERENDT-KEY (1:33)
122313     MOVE 'C'                    TO PI-ENDT-RECORD-TYPE
122313     MOVE ZEROS                  TO PI-ENDT-SEQ-NO
122313     MOVE PI-ERENDT-KEY          TO PI-PREV-ERENDT-KEY  
122313
122313     PERFORM 0362-READ-ELCRTO    THRU 0362-EXIT
            display 'response ' ws-response
            display 'pi-elcrto-key ' pi-elcrto-key
            display 'oc key ' oc-control-primary

122313     IF RESP-NORMAL
122313       AND (PI-ELCRTO-KEY (1:33) =
122313              OC-CONTROL-PRIMARY (1:33))
122313          DISPLAY 'CRTO RESP NORMAL AND = '
122313          IF OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES
122313             MOVE OC-CONTROL-PRIMARY  TO PI-ELCRTO-KEY
122313          ELSE
122313             SUBTRACT +1 FROM OC-KEY-SEQ-NO
122313             PERFORM 0780-ADD-CRTO-REC THRU 0780-EXIT
122313          END-IF
122313     ELSE
122313          MOVE SPACES              TO ORIGINAL-CERTIFICATE
122313          MOVE 'OC'                TO OC-RECORD-ID
122313          MOVE ELCRTO-KEY (1:33)   TO OC-CONTROL-PRIMARY (1:33)
122313          MOVE +4096               TO OC-KEY-SEQ-NO
122313          PERFORM 0780-ADD-CRTO-REC THRU 0780-EXIT
122313     END-IF
122313
122313     PERFORM 0361-MOVE-CRTO-TO-SCREEN THRU 0361-EXIT
122313
122313     .
122313 0760-EXIT.
122313     EXIT.
122313
122313 0770-MOVE-CERT-TO-SCREEN.
122313
122313     display ' made it to 0770 '
122313
122313     MOVE CM-CARRIER             TO CARRO
122313     MOVE CM-GROUPING            TO GROUPO
122313     MOVE CM-STATE               TO STATEO
122313     MOVE CM-ACCOUNT             TO ACCTO
122313     MOVE CM-CERT-PRIME          TO CERTNOO
122313     MOVE CM-CERT-SFX            TO CRTSFXO
122313
122313     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
122313     MOVE ' '                    TO DC-OPTION-CODE
122313     PERFORM 9700-DATE-LINK
122313     IF NO-CONVERSION-ERROR
122313        MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
122313     END-IF
122313
122313     MOVE CM-INSURED-LAST-NAME   TO LNAMEO
122313     MOVE CM-INSURED-FIRST-NAME  TO FNAMEO
122313     MOVE CM-INSURED-INITIAL2    TO MINITO
122313
122313     MOVE CM-LF-CANCEL-DT        TO DC-BIN-DATE-1
122313     MOVE ' '                    TO DC-OPTION-CODE
122313     PERFORM 9700-DATE-LINK
122313     IF NO-CONVERSION-ERROR
122313        MOVE DC-GREG-DATE-1-EDIT TO NLFDTO
122313     END-IF
122313     MOVE CM-AH-CANCEL-DT        TO DC-BIN-DATE-1
122313     MOVE ' '                    TO DC-OPTION-CODE
122313     PERFORM 9700-DATE-LINK
122313     IF NO-CONVERSION-ERROR
122313        MOVE DC-GREG-DATE-1-EDIT TO NAHDTO
122313     END-IF
122313
122313     MOVE CM-LF-ITD-CANCEL-AMT   TO NLFAMTO
122313     MOVE CM-AH-ITD-CANCEL-AMT   TO NAHAMTO
122313
122313     IF CM-LIFE-COMM-PCT NOT = ZEROS
122313        MOVE CM-LIFE-COMM-PCT TO LFCOMMO
122313     END-IF
122313     IF CM-AH-COMM-PCT NOT = ZEROS
122313        MOVE CM-AH-COMM-PCT   TO AHCOMMO
122313     END-IF
122313
122313
122313     .
122313 0770-EXIT.
122313     EXIT.
122313
122313
122313 0780-ADD-CRTO-REC.
            display 'made it to 0780'
122313     PERFORM 0783-LOAD-CERT-TO-CRTO-REC THRU 0783-EXIT
122313
122313     EXEC CICS WRITE
122313        DATASET   ('ELCRTO')
122313        FROM      (ORIGINAL-CERTIFICATE)
122313        RIDFLD    (OC-CONTROL-PRIMARY)
122313        RESP      (WS-RESPONSE)
122313     END-EXEC.
122313
122313     IF NOT RESP-NORMAL
122313         DISPLAY 'ERROR IN 0780 WRITING ELCRTO ' WS-RESPONSE
122313         MOVE ER-9999      TO EMI-ERROR
122313         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122313         GO TO 8200-SEND-DATAONLY
122313     END-IF
122313
122313     .
122313 0780-EXIT.
122313     EXIT.
122313
122313
122313 0783-LOAD-CERT-TO-CRTO-REC.
122313
            display 'made it to 0783'
122313
122313     MOVE '6316'                 TO OC-LAST-MAINT-BY
122313     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
122313     MOVE WS-CURRENT-BIN-DT      TO OC-LAST-MAINT-DT
122313     MOVE 'I'                    TO OC-RECORD-TYPE
122313     MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
122313     MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
122313     MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
122313     MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
122313     MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
122313     MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
122313     MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
122313     MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
122313     MOVE CM-LF-BENEFIT-CD       TO OC-LF-BENCD        
122313     IF CM-LF-BENEFIT-CD NOT = '00' AND '  '
122313         MOVE CM-LF-ORIG-TERM    TO OC-LF-TERM         
122313         MOVE CM-LF-BENEFIT-AMT  TO OC-LF-BEN-AMT      
122313         MOVE CM-LF-PREMIUM-AMT  TO OC-LF-PRM-AMT      
122313         MOVE CM-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT  
122313         MOVE CM-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT  
122313         MOVE CM-LF-LOAN-EXPIRE-DT TO OC-LF-EXP-DT       
122313         MOVE CM-LIFE-COMM-PCT   TO OC-LF-COMM-PCT     
122313         MOVE CM-LF-CANCEL-DT    TO OC-LF-CANCEL-DT
122313         MOVE CM-LF-ITD-CANCEL-AMT TO OC-LF-CANCEL-AMT
122313                                      OC-LF-ITD-CANCEL-AMT
122313     END-IF
122313     MOVE CM-AH-BENEFIT-CD       TO OC-AH-BENCD        
122313     IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
122313         MOVE CM-AH-ORIG-TERM    TO OC-AH-TERM         
122313         MOVE CM-AH-BENEFIT-AMT  TO OC-AH-BEN-AMT      
122313         MOVE CM-AH-PREMIUM-AMT  TO OC-AH-PRM-AMT      
122313         MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT       
122313         MOVE CM-AH-COMM-PCT     TO OC-AH-COMM-PCT     
122313         MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP
122313         MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
122313         MOVE CM-AH-ITD-CANCEL-AMT TO OC-AH-CANCEL-AMT
122313                                      OC-AH-ITD-CANCEL-AMT
122313     END-IF
122313     MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
122313     MOVE 'N'                    TO OC-ISSUE-TRAN-IND
122313     MOVE 'N'                    TO OC-CANCEL-TRAN-IND
122313
122313     MOVE LOW-VALUES             TO OC-ENDORSEMENT-PROCESSED-DT
122313
122313     MOVE ELCRTO-KEY (1:33) TO WS-ERMAIL-KEY
122313
122313     EXEC CICS READ
122313          DATASET   (WS-ERMAIL-FILE-ID)
122313          RIDFLD    (WS-ERMAIL-KEY)
122313          SET       (ADDRESS OF MAILING-DATA)
122313          RESP      (WS-RESPONSE)
122313     END-EXEC
122313
122313     IF RESP-NORMAL
122313         MOVE MA-CRED-BENE-NAME TO OC-CRED-BENE-NAME
122313     END-IF.
122313
122313     MOVE ELCRTO-KEY (1:33)     TO WS-ELCRTT-PRIMARY
122313     MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
122313
122313     EXEC CICS READ
122313          DATASET  (WS-ELCRTT-FILE-ID)
122313          RIDFLD   (WS-ELCRTT-KEY)
122313          SET      (ADDRESS OF CERTIFICATE-TRAILERS)
122313          RESP     (WS-RESPONSE)
122313     END-EXEC
122313
122313     IF RESP-NORMAL
122313         MOVE CS-INS-AGE-DEFAULT-FLAG TO OC-INS-AGE-DEFAULT-FLAG
122313         MOVE CS-JNT-AGE-DEFAULT-FLAG TO OC-JNT-AGE-DEFAULT-FLAG
122313     END-IF.
122313
122313
122313     .
122313 0783-EXIT.
122313     EXIT.
122313
122313 0785-CHG-CRTO-REC.
122313 
           display 'made it to 0785'
122313
122313     MOVE '6316'                 TO OC-LAST-MAINT-BY
122313     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
122313     MOVE WS-CURRENT-BIN-DT      TO OC-LAST-MAINT-DT
122313
122313     MOVE OLFDTO                 TO WS-TMPDATE
122313     EXEC CICS BIF DEEDIT
122313          FIELD   (WS-TMPDATE)
122313          LENGTH  (8)
122313     END-EXEC
122313     MOVE WS-TMPDT               TO DC-GREG-DATE-1-MDY
122313     MOVE '4'                    TO DC-OPTION-CODE
122313     PERFORM 9700-DATE-LINK
122313     IF NOT DATE-CONVERSION-ERROR
122313        MOVE DC-BIN-DATE-1       TO OC-LF-CANCEL-DT
122313     END-IF
122313
           display 'lf amt ' olfamto
122313     MOVE OLFAMTO                TO WS-AMTX
122313     EXEC CICS BIF DEEDIT
122313          FIELD   (WS-AMTX)
122313          LENGTH  (10)
122313     END-EXEC  
122313     MOVE WS-AMTV2               TO OC-LF-CANCEL-AMT
122313
122313     MOVE OAHDTO                 TO WS-TMPDATE
122313     EXEC CICS BIF DEEDIT
122313          FIELD   (WS-TMPDATE)
122313          LENGTH  (8)
122313     END-EXEC
122313     MOVE WS-TMPDT               TO DC-GREG-DATE-1-MDY
122313     MOVE '4'                    TO DC-OPTION-CODE
122313     PERFORM 9700-DATE-LINK
122313     IF NOT DATE-CONVERSION-ERROR
122313        MOVE DC-BIN-DATE-1       TO OC-AH-CANCEL-DT
122313     END-IF
122313
           display 'ah amt ' oahamto
122313     MOVE OAHAMTO                TO WS-AMTX
122313     EXEC CICS BIF DEEDIT
122313          FIELD   (WS-AMTX)
122313          LENGTH  (10)
122313     END-EXEC  
122313     MOVE WS-AMTV2               TO OC-AH-CANCEL-AMT
122313
122313     EXEC CICS REWRITE
122313        DATASET   ('ELCRTO')
122313        FROM      (ORIGINAL-CERTIFICATE)
122313        RESP      (WS-RESPONSE)
122313     END-EXEC
122313
122313     IF NOT RESP-NORMAL
122313         DISPLAY 'ERROR IN 0385 REWRITING ELCRTO ' WS-RESPONSE
122313         MOVE ER-9999      TO EMI-ERROR
122313         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122313         GO TO 8200-SEND-DATAONLY
122313     END-IF
122313
122313     PERFORM 0361-MOVE-CRTO-TO-SCREEN THRU 0361-EXIT
122313
122313     .
122313 0785-EXIT.
122313     EXIT.
122313

       1000-FINALIZE.

           PERFORM 0325-EDIT-SCREEN THRU 0325-EXIT

           IF NOT EMI-NO-ERRORS
112612      AND EIBAID = DFHPF4           
              GO TO 8200-SEND-DATAONLY
           END-IF

           display ' made it to 1000 '

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
012412        AND (PI-ELCRTO-KEY (1:33) =
012412              OC-CONTROL-PRIMARY (1:33))
                 DISPLAY ' RESP NORMAL AND = '
062712           MOVE OC-ORIG-REC         TO NEW-ORIG-REC
                 PERFORM 0400-ADD-ERENDT  THRU 0400-EXIT
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
091812        MOVE ER-3829             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           PERFORM 0390-BUILD-FROM-ERENDT THRU 0390-EXIT

           MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
                                       TO SYSO
           MOVE WS-KIXHOST             TO HOSTO
           MOVE PI-COMPANY-ID          TO COMPANYO
           MOVE PI-PROCESSOR-ID        TO PROCIDO
122313     IF PI-FROM-CERT = 'Y'
122313         MOVE '2'                TO DATASORO
122313     ELSE
122313         MOVE '4'                TO DATASORO
122313     END-IF
           MOVE BL-BATCH-NO            TO BATCHNOO
           MOVE BL-BATCH-SEQ           TO BSEQNOO
           MOVE BL-ARCHIVE-NO          TO ARCHNOO
      *    MOVE -1                     TO PRTSWL
      *    MOVE AL-UANON               TO PRTSWA


           IF EIBAID = DFHPF4
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

           GO TO 8100-SEND-INITIAL-MAP

           .
       1000-EXIT.
           EXIT.

       1100-CALL-NS-BUS-LOGIC.

           DISPLAY ' MADE IT TO 1100 ' w-arch-number

           MOVE SPACES                 TO BL-INPUT
           MOVE W-ARCH-NUMBER          TO BL-ARCHIVE-NO
122313     IF PI-FROM-CERT = 'Y'
122313         MOVE '2'                TO BL-DATA-SRCE
122313         MOVE SPACES             TO BL-BATCH-NO
122313         MOVE ZEROS              TO BL-BATCH-SEQ
122313     ELSE
122313         MOVE '4'                TO BL-DATA-SRCE
122313         MOVE PB-ENTRY-BATCH     TO BL-BATCH-NO
122313         MOVE PB-BATCH-SEQ-NO    TO BL-BATCH-SEQ
122313     END-IF
122313     MOVE PI-CARRIER             TO BL-CARRIER
122313     MOVE PI-GROUPING            TO BL-GROUP
122313     MOVE PI-STATE               TO BL-STATE
122313     MOVE PI-ACCOUNT             TO BL-ACCOUNT
122313     IF PI-CERT-EFF-DT NOT = LOW-VALUES
122313        MOVE PI-CERT-EFF-DT      TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-date-link
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-B-EDIT
                                       TO BL-EFF-DT
              END-IF
           END-IF
122313     MOVE PI-CERT-NO             TO BL-CERT-NO
           MOVE SPACES                 TO BL-RESP-NO
           MOVE LTRIDI                 TO BL-LETTER-ID
           MOVE W-NUMBER-OF-COPIES     TO BL-NO-OF-COPIES
           MOVE PI-PROCESSOR-ID        TO BL-PROC-ID
      *    MOVE 'ALWA'                 TO BL-PROC-ID
           MOVE PI-COMPANY-ID          TO BL-COMP-ID
121713     MOVE PI-ENCCODE             TO BL-ENC-CD
           IF EIBAID = DFHPF4
              MOVE 'B'                 TO BL-WRITE-ERARCH
           ELSE
              MOVE 'T'                 TO BL-WRITE-ERARCH
           END-IF
072312     MOVE SPACES                 TO BL-CERT-FORM-ID
110612     MOVE W-ARCH-NUMBER          TO BL-ENDT-ARCH-NO
102212     MOVE 'CAN ENDT'             TO BL-SOURCE-SCREEN

072312     PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
072312        IF REACD-IN (S1) NOT = LOW-VALUES
072312           MOVE REACD-IN (S1)    TO BL-REASON-CODE (S1)
072312        END-IF
072312     END-PERFORM

PEMTST    DISPLAY ' ABOUT TO LINK TO NSRASBL '
           DISPLAY ' BL INPUT ' BL-INPUT
      *****************************************
      * Invoke the LETTER business logic
      *****************************************
072312     MOVE FUNCTION LENGTH(SRCH-COMMAREA) TO WS-COMM-LENGTH
           display ' srch commarea ' srch-commarea
           display ' length ' ws-comm-length

           exec cics link
              program('NSRASBL')
              commarea(srch-commarea)
072312        LENGTH  (WS-COMM-LENGTH)
           end-exec.

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
      
           EXEC CICS READ
                DATASET    (WS-ELCNTL-FILE-ID)
                SET        (ADDRESS OF CONTROL-FILE)
                RIDFLD     (ELCNTL-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              display ' good read elcntl '
              IF CF-CREDIT-LAST-ARCH-NUM NOT NUMERIC
                 MOVE ZEROS            TO CF-CREDIT-LAST-ARCH-NUM
              END-IF
              ADD 1                    TO CF-CREDIT-LAST-ARCH-NUM
              MOVE CF-CREDIT-LAST-ARCH-NUM
                                       TO W-ARCH-NUMBER
              display ' new arch number ' w-arch-number
              EXEC CICS REWRITE
                 FROM      (CONTROL-FILE)
                 DATASET   (WS-ELCNTL-FILE-ID)
              END-EXEC
           END-IF

           .
       1500-EXIT.
           EXIT.

072312           
072312     
072312 1700-WRITE-CERT-NOTE      SECTION.
103012
103012     MOVE SV-PARTIAL-KEY TO ERCNOT-PARTIAL-KEY.
103012     MOVE ZERO           TO ERCNOT-SEQ.
103012     SET TB-INDX         DOWN BY 1.
103012     SET TB-INDX1        TO TB-INDX.
103012
103012****DELETE CURRENT CERT NOTES
103012 1700-LOOP.
103012     EXEC CICS READ
103012         DATASET (WS-ERCNOT-FILE-ID)
103012         RIDFLD  (ERCNOT-KEY)
103012         SET     (ADDRESS OF CERT-NOTE-FILE)
103012         RESP    (WS-RESPONSE)
103012         GTEQ
103012     END-EXEC.
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
103012     EXEC CICS DELETE
103012         DATASET (WS-ERCNOT-FILE-ID)
103012         RIDFLD  (ERCNOT-KEY)
103012     END-EXEC.
103012
103012     GO TO 1700-LOOP.
103012
103012 1700-ENDDEL.
103012
103012     EXEC CICS GETMAIN
103012          LENGTH(ERCNOT-LENGTH)
103012          SET(ADDRESS OF CERT-NOTE-FILE)
103012          INITIMG(GETMAIN-SPACE)
103012     END-EXEC.
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
103012        EXEC CICS WRITE 
103012             DATASET(WS-ERCNOT-FILE-ID)
103012             FROM(CERT-NOTE-FILE)
103012             RIDFLD(ERCNOT-KEY)
103012        END-EXEC
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
072312        EXEC CICS UNLOCK
072312           DATASET    (WS-ELCERT-FILE-ID)
072312        END-EXEC
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
072312     EXEC CICS READ
072312         UPDATE
072312         DATASET  (WS-ELCERT-FILE-ID)
072312         RIDFLD   (WS-ELCERT-KEY)
072312         SET      (ADDRESS OF CERTIFICATE-MASTER)
072312         RESP     (WS-RESPONSE)
072312     END-EXEC
072312
072312     .
072312 1800-EXIT.
072312     EXIT.
072312
072312 1810-REWRITE-ELCERT.
072312
072312     EXEC CICS REWRITE
072312         FROM     (CERTIFICATE-MASTER)
072312         DATASET  (WS-ELCERT-FILE-ID)
072312         RESP     (WS-RESPONSE)
072312     END-EXEC.
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
           MOVE -1                     TO LTRIDL
072312*    MOVE AL-UANON               TO PRTSWA

012412     IF PI-PROCESSOR-IS-CSR-SUPER
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
              perform varying s1 from +1 by +1 until
                 s1 > +12
                 MOVE AL-UANON            TO reacda (s1)
              end-perform
012412     END-IF              
122313
122313     IF PI-FROM-CERT = 'Y' AND
122313        PI-PROCESSOR-IS-CSR-SUPER AND
122313        PI-ENDORSE-RECORD NOT EQUAL 'Y'
122313            MOVE AL-UNNON        TO OLFDTA
122313                                    OAHDTA
122313                                    OLFAMTA
122313                                    OAHAMTA
122313     END-IF
122313
122313     IF PI-FROM-CERT = 'Y' AND
122313        NOT PI-PROCESSOR-IS-CSR-SUPER
122313            MOVE AL-SANOF        TO LTRIDA
122313                                    ENCCODEA
122313                                    CRTNT1A
122313                                    CRTNT2A
122313                                    BILNT1A
122313            MOVE AL-SADOF        TO PF3A
122313                                    PF4A
122313     END-IF
122313
122313     IF PI-ENDORSE-RECORD = 'Y'
122313            MOVE AL-SANOF        TO LTRIDA
122313                                    ENCCODEA
122313                                    CRTNT1A
122313                                    CRTNT2A
122313                                    BILNT1A
122313            MOVE AL-SADOF        TO PF3A
122313                                    PF4A
122313     END-IF

           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
           EXEC CICS SEND
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               FROM     (EL631IO)
               ERASE
               CURSOR
           END-EXEC.

           GO TO 9100-RETURN-TRAN.

       8200-SEND-DATAONLY.

           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK.
           MOVE DC-GREG-DATE-1-EDIT    TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE -1                     TO PFENTRL
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO
           EXEC CICS SEND
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               FROM     (EL631IO)
               DATAONLY
               ERASEAUP
               CURSOR
           END-EXEC.

           GO TO 9100-RETURN-TRAN.

       8300-SEND-TEXT.
           EXEC CICS SEND TEXT
               FROM     (LOGOFF-TEXT)
               LENGTH   (LOGOFF-LENGTH)
               ERASE
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

       8800-UNAUTHORIZED-ACCESS.
           MOVE UNACCESS-MSG           TO LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.

       8810-PF23.
           MOVE EIBAID                 TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           GO TO 9300-XCTL.
       9000-RETURN-CICS.
           EXEC CICS RETURN
           END-EXEC.

       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
           MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.

           EXEC CICS RETURN
               TRANSID    (TRANS-ID)
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)
               LENGTH     (1300)
           END-EXEC.

       9200-RETURN-MAIN-MENU.
           MOVE XCTL-626               TO PGM-NAME.
           GO TO 9300-XCTL.

       9300-XCTL.
           EXEC CICS XCTL
               PROGRAM    (PGM-NAME)
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)
               LENGTH     (1300)
           END-EXEC.

       9400-CLEAR.
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.

           EXEC CICS XCTL
               PROGRAM    (PGM-NAME)
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)
               LENGTH     (1300)
           END-EXEC.

       9500-PF12.
           MOVE XCTL-010               TO PGM-NAME.
           GO TO 9300-XCTL.

       9600-PGMID-ERROR.
           EXEC CICS HANDLE CONDITION
               PGMIDERR    (8300-SEND-TEXT)
           END-EXEC.

           MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
           MOVE ' '                    TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           MOVE PGM-NAME               TO LOGOFF-PGM.
           MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
           GO TO 9300-XCTL.

       9700-DATE-LINK.
           MOVE LINK-ELDATCV           TO PGM-NAME.

           EXEC CICS LINK
               PROGRAM    (PGM-NAME)
               COMMAREA   (DATE-CONVERSION-DATA)
               LENGTH     (DC-COMM-LENGTH)
           END-EXEC.

       9900-ERROR-FORMAT.
           IF NOT EMI-ERRORS-COMPLETE
              MOVE LINK-001            TO PGM-NAME
              EXEC CICS LINK
                  PROGRAM    (PGM-NAME)
                  COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
                  LENGTH     (EMI-COMM-LENGTH)
              END-EXEC
           END-IF

           .
       9900-EXIT.
           EXIT.

       9990-ABEND.
           MOVE LINK-004               TO PGM-NAME.
           MOVE DFHEIBLK               TO EMI-LINE1.
           EXEC CICS LINK
               PROGRAM   (PGM-NAME)
               COMMAREA  (EMI-LINE1)
               LENGTH    (72)
           END-EXEC.

           GO TO 8200-SEND-DATAONLY.

           GOBACK.

       9995-SECURITY-VIOLATION.
                                   COPY ELCSCTP.

       9995-EXIT.
           EXIT.

