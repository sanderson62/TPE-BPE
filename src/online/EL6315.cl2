       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL6315.
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
052307* 052307   2006052600001  AJRA  ADDED FLAG FOR CANCELS ON CERTS  
052307*                               WITH OPEN CLAIMS
072209* 072209   2008101500003  AJRA  PRINT BORROWER FIRST NAME
111109* 111109   2008100900003  AJRA  UPDATE CERT WHEN NOTE DELETED
061410* 061410   2009061500002  AJRA  ADD 3 FOR OPEN CLAIM FLAG
110410* 110410   2007070900001  PEMA  ADD REFORMATIONS AND RESCISSIONS
011712* 011712   2011022800002  AJRA  NAPERSOFT
062712* 062712   2011022800001  AJRA  REDEFINE ORIG DATA
072312* 072312   2011022800001  AJRA  NAPERSOFT MISC
101812* 101812   2012101700002  AJRA  READ ENDT BY ARCHIVE NO
121012* 121012   2012101700002  AJRA  HIGHLIGHT REASON CODES
121712* 121712   2012101700002  AJRA  ADD DEFAULT AGE FLAG
020613* 020613   2013013100003  AJRA  HIGHLIGHT AGE CHANGE REASON
122313* 122313   2013090300001  AJRA  NO PF KEYS FROM ENDOREMENT REVIEW
021214* 021214   2014021200001  AJRA  FIX FINALIZE FLAG
091615* 091615  2015082000001   PEMA  Endorsement check processing
092618* 092618 CR2018050200001  PEMA  Add check for cancel rec
041320* 041320 CR2020030500002  PEMA  Issue, cancel billing notes
052307*-----------------------------------------------------------------

      *REMARKS.
      *        TRANSACTION - EXBB - ENDORSEMENT MAINTENANCE
       EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL6315 WORKING STORAGE    *'.
       77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'.
       77  NOTE-SUB PIC S9(5) COMP-3 VALUE +0.
       77  WS-SEQ-NO                   PIC 9(4) BINARY.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  WS-LF-ABBRV                 PIC X(10) VALUE SPACES.
       77  WS-LF-DESC                  PIC X(02) VALUE SPACES.
       77  ws-lf-joint-ind             pic x value ' '.
       77  WS-WAIT-PER                 PIC X(03) VALUE ZEROS.
       77  WS-RET-ELIM                 PIC X(01) VALUE SPACES.
       77  WS-AH-DESC                  PIC X(35) VALUE SPACES.
       77  WS-BEN-DAYS                 PIC X(02) VALUE ZEROS.
       77  WS-LF-BENCD                 PIC XX    VALUE '00'.
       77  WS-AH-BENCD                 PIC XX    VALUE '00'.
091615 77  ws-tot-new-prem             pic s9(7)v99 comp-3 value +0.
091615 77  ws-tot-new-comm             pic s9(5)v99 comp-3 value +0.
091615 77  ws-tot-old-prem             pic s9(7)v99 comp-3 value +0.
091615 77  ws-tot-old-comm             pic s9(7)v99 comp-3 value +0.
091615 77  ws-cert-found               pic x value ' '.
091615     88  cert-found                 value 'Y'.
091615 77  ws-stop-sw                  pic x value ' '.
091615     88  i-say-stop                 value 'Y'.
091615 77  ws-check-sw                 pic x value ' '.
091615     88  i-have-checks              value 'Y'.
111513 77  ws-chek-browse-sw           pic x value ' '.
092618 77  e1                          pic s999 comp-3 value +0.

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

       01  ELCNTL-KEY.
           05  CNTL-COMP-ID            PIC X(3)  VALUE SPACES.
           05  CNTL-REC-TYPE           PIC X     VALUE SPACES.
           05  CNTL-ACCESS.
               10  CNTL-STATE          PIC XX    VALUE SPACES.
               10  CNTL-BEN-CD.
                   15  F               PIC X.
                   15  CNTL-CARR       PIC X.
           05  CNTL-SEQ                PIC S9(4) VALUE +0 COMP.

       01  STANDARD-AREAS.
           12  TIME-MT                 PIC S9(7).
           12  TIME-MT-R  REDEFINES TIME-MT.
               16  FILLER              PIC X.
               16  TIME-LMT            PIC 99V99.
               16  FILLER              PIC X(2).
           12  GETMAIN-SPACE           PIC X VALUE SPACES.
PEMMOD*    12  ERENDT-LENGTH           PIC S9(04) VALUE +373 COMP.
PEMMOD     12  ERENDT-LENGTH           PIC S9(04) VALUE +579 COMP.
           12  ERPNDB-LENGTH           PIC 9(04)  VALUE 585 COMP.
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
           12  WS-ELCERT-KEY.
               16  CERT-COMPANY-CD     PIC X.
               16  CERT-CARRIER        PIC X.
               16  CERT-GROUPING       PIC X(6).
               16  CERT-STATE          PIC XX.
               16  CERT-ACCOUNT        PIC X(10).
               16  CERT-CERT-EFF-DT    PIC XX.
               16  CERT-CERT-PRIME     PIC X(10).
               16  CERT-CERT-SFX       PIC X.
121712     12  WS-ELCRTT-KEY.
121712         16  WS-ELCRTT-PRIMARY       PIC X(33).
121712         16  WS-ELCRTT-REC-TYPE      PIC X(1).
           12  WS-ERMAIL-KEY.
               16  MAIL-COMPANY-CD     PIC X.
               16  MAIL-CARRIER        PIC X.
               16  MAIL-GROUPING       PIC X(6).
               16  MAIL-STATE          PIC XX.
               16  MAIL-ACCOUNT        PIC X(10).
               16  MAIL-CERT-EFF-DT    PIC XX.
               16  MAIL-CERT-PRIME     PIC X(10).
               16  MAIL-CERT-SFX       PIC X.
091615     12  ERCHEK-KEY.
091615         16  CHEK-COMPANY-CD     PIC X.
091615         16  CHEK-CARRIER        PIC X.
091615         16  CHEK-GROUPING       PIC X(6).
091615         16  CHEK-STATE          PIC XX.
091615         16  CHEK-ACCOUNT        PIC X(10).
091615         16  CHEK-CERT-EFF-DT    PIC XX.
091615         16  CHEK-CERT-PRIME     PIC X(10).
091615         16  CHEK-CERT-SFX       PIC X.
091615         16  CHEK-SEQ-NO         PIC 9(4) BINARY.
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
           12  WS-ELCRTO-FILE-ID       PIC X(8) VALUE 'ELCRTO'.
           12  WS-ERENDT-FILE-ID       PIC X(8) VALUE 'ERENDT'.
           12  WS-ERPNDB2-FILE-ID      PIC X(8) VALUE 'ERPNDB2'.
           12  WS-ERPNDB-FILE-ID       PIC X(8) VALUE 'ERPNDB'.
           12  WS-ERACCT2-FILE-ID      PIC X(8) VALUE 'ERACCT2'.
121712     12  WS-ELCRTT-FILE-ID       PIC X(8) VALUE 'ELCRTT'.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  MAP-NAME            PIC X(8)    VALUE 'EL631H'.
           12  MAPSET-NAME         PIC X(8)    VALUE 'EL6315S'.
           12  SCREEN-NUMBER       PIC X(4)    VALUE '631H'.
           12  TRANS-ID            PIC X(4)    VALUE 'EXBB'.
           12  THIS-PGM            PIC X(8)    VALUE 'EL6315'.
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
091615     12  XCTL-677            PIC X(8)    VALUE 'EL677'.
           12  LINK-001            PIC X(8)    VALUE 'EL001'.
           12  LINK-004            PIC X(8)    VALUE 'EL004'.
           12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
           12  WS-PHONE.
               16  WS-PH1              PIC XXX.
               16  WS-PH2              PIC XXX.
               16  WS-PH3              PIC XXXX.
           12  WS-PHONE-NUM REDEFINES WS-PHONE PIC 9(10).
           12  WS-DEV-RT               PIC S9V9(6) COMP-3.

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
           12  ER-0348                 PIC X(4)  VALUE '0348'.
           12  ER-0419                 PIC X(4)  VALUE '0419'.
           12  ER-1101                 PIC X(4)  VALUE '1101'.
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
011712     12  ER-2883                 PIC X(4)  VALUE '2883'.
092618     12  ER-3273                 PIC X(4)  VALUE '3273'.
062712     12  ER-3831                 PIC X(4)  VALUE '3831'.
072312     12  ER-3836                 PIC X(4)  VALUE '3836'.
072312     12  ER-3837                 PIC X(4)  VALUE '3837'.
           12  ER-7638                 PIC X(4)  VALUE '7638'.
           12  ER-9999                 PIC X(4)  VALUE '9999'.

           COPY ELCDATE.
           COPY ELCLOGOF.
           COPY ELCATTR.
           COPY ELCEMIB.

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
              16  PI-PREV-ERENDT-KEY      PIC X(36).
062712        16  PI-RES-REF-CLM-TYPE     PIC X.
011712        16  PI-CLM-REFORM-IND       PIC X.
011712            88  PI-CLAIM-REFORMATION   VALUE 'Y'.
072312        16  PI-PASS-AREA.
072312            20  FILLER              PIC X(237).
072312            20  PI-CHANGES-MADE     PIC X.
072312            20  PI-AMT-CHANGES-MADE PIC X.
122313            20  PI-ENDORSE-RECORD   PIC X.
122313            20  FILLER              PIC X(277).
062712        16  PI-6317-PASS-AREA REDEFINES PI-PASS-AREA.
072312            20  PI-6317-DATA        PIC X(93).
072312            20  FILLER              PIC X(143).
072312            20  PI-6317-FINALIZED   PIC X.
072312            20  FILLER              PIC X(280).
062712        16  PI-6318-PASS-AREA REDEFINES PI-PASS-AREA.
021214            20  PI-6318-DATA        PIC X(235).
072312            20  PI-6318-FINALIZED   PIC X.
021214            20  FILLER              PIC X(253).
121012            20  PI-6318-REASON-IND  PIC X(26).
121012            20  FILLER              PIC X(2).
091615    12  pi-endt-prem-diff            pic s9(7)v99 comp-3.
091615    12  pi-endt-comm-diff           pic s9(5)v99 comp-3.
091615    12  FILLER                      PIC X(267).  *> was 276

121012 01  WS-REASON-CODE-IND.
121012     12  WS-REASON-A                PIC X  VALUE SPACES.
121012     12  WS-REASON-B                PIC X  VALUE SPACES.
121012     12  WS-REASON-C                PIC X  VALUE SPACES.
121012     12  WS-REASON-D                PIC X  VALUE SPACES.
121012     12  WS-REASON-E                PIC X  VALUE SPACES.
121012     12  WS-REASON-F                PIC X  VALUE SPACES.
121012     12  WS-REASON-G                PIC X  VALUE SPACES.
121012     12  WS-REASON-H                PIC X  VALUE SPACES.
121012     12  WS-REASON-I                PIC X  VALUE SPACES.
121012     12  WS-REASON-J                PIC X  VALUE SPACES.
121012     12  WS-REASON-K                PIC X  VALUE SPACES.
121012     12  WS-REASON-L                PIC X  VALUE SPACES.
121012     12  WS-REASON-M                PIC X  VALUE SPACES.
121012     12  WS-REASON-N                PIC X  VALUE SPACES.
121012     12  WS-REASON-O                PIC X  VALUE SPACES.
121012     12  WS-REASON-P                PIC X  VALUE SPACES.
121012     12  WS-REASON-Q                PIC X  VALUE SPACES.
121012     12  WS-REASON-R                PIC X  VALUE SPACES.
121012     12  WS-REASON-S                PIC X  VALUE SPACES.
121012     12  WS-REASON-T                PIC X  VALUE SPACES.
121012     12  WS-REASON-U                PIC X  VALUE SPACES.
121012     12  WS-REASON-V                PIC X  VALUE SPACES.
121012     12  WS-REASON-W                PIC X  VALUE SPACES.
121012     12  WS-REASON-X                PIC X  VALUE SPACES.
121012     12  WS-REASON-Y                PIC X  VALUE SPACES.
121012     12  WS-REASON-Z                PIC X  VALUE SPACES.
121012
           COPY ELCAID.

       01  FILLER    REDEFINES DFHAID.
           12  FILLER              PIC X(8).
           12  PF-VALUES           PIC X       OCCURS 24 TIMES.

          COPY EL6315S.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(1300).
       01  var  pic x(30).
                                       COPY ERCACCT.
                                       COPY ELCCNTL.
                                       COPY ERCPNDB.
                                       COPY ELCCERT.
                                       COPY ELCCRTO.
                                       COPY ERCENDT.
                                       COPY ERCMAIL.
121712                                 COPY ELCCRTT.
091615                                 copy ERCCHEK.

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT
           MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT

           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF
           display ' entering el6315 '
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
              DISPLAY ' WS KIX HOST ' WS-KIXSYS
      *       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if

           MOVE LOW-VALUES             TO EL631HI

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
                 GO TO 0350-DISPLAY-COMMON-DATA
              ELSE
072312         IF (PI-CALLING-PROGRAM = 'EL6318' AND 
072312             PI-6318-FINALIZED = 'Y')   OR
072312            (PI-CALLING-PROGRAM = 'EL6317' AND 
072312             PI-6317-FINALIZED = 'Y')
072312           MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM  
072312           MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM
072312           MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1  
072312           MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2  
072312           MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3  
072312           MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4  
072312           MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5  
072312           MOVE SPACES                 TO PI-SAVED-PROGRAM-6
072312           GO TO 9400-CLEAR
072312         ELSE
                 MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM  
                 MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM
                 MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1  
                 MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2  
                 MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3  
                 MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4  
                 MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5  
                 MOVE SPACES                 TO PI-SAVED-PROGRAM-6
                 if pi-veri-processed or pi-endo-processed
                    go to 0330-SHOW-ERENDT
                 end-if
                 GO TO 0350-DISPLAY-COMMON-DATA
072312         END-IF
              END-IF
           END-IF

           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR
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
               INTO     (EL631HI)
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
              IF ERENDT-KEY = PI-PREV-ERENDT-KEY
                 PERFORM 0510-ERENDT-READNEXT
                                       THRU 0510-EXIT
              END-IF
              IF (RESP-NORMAL) AND
                 (PI-ERENDT-KEY (1:34) = EN-CONTROL-PRIMARY (1:34))
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
                 (PI-ERENDT-KEY (1:34) = EN-CONTROL-PRIMARY (1:34))
                 PERFORM 0390-BUILD-FROM-ERENDT
                                       THRU 0390-EXIT
122313           MOVE 'Y'              TO PI-ENDORSE-RECORD
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8100-SEND-INITIAL-MAP
              ELSE
122313           MOVE 'N'              TO PI-ENDORSE-RECORD
122313           MOVE LOW-VALUES       TO EL631HI
                 GO TO 0350-DISPLAY-COMMON-DATA
                 MOVE ER-2238          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO CARRL
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF
           
072312     IF (EIBAID = DFHPF4 OR DFHPF5)
122313       AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
072312         IF PI-CHANGES-MADE = 'N'
072312           MOVE ER-3837          TO EMI-ERROR
072312           PERFORM 9900-ERROR-FORMAT
072312                                 THRU 9900-EXIT
072312           MOVE -1               TO CARRL
072312           GO TO 8200-SEND-DATAONLY
072312        END-IF
072312     END-IF
072312     IF EIBAID = DFHPF3
122313       AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
072312         IF PI-AMT-CHANGES-MADE = 'Y'
072312           MOVE ER-3836          TO EMI-ERROR
072312           PERFORM 9900-ERROR-FORMAT
072312                                 THRU 9900-EXIT
072312           MOVE -1               TO CARRL
072312           GO TO 8200-SEND-DATAONLY
072312        END-IF
072312     END-IF

           IF EIBAID = DFHPF3
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
062712        MOVE LOW-VALUES             TO PI-6317-PASS-AREA
              MOVE XCTL-6317              TO PGM-NAME
              GO TO 9300-XCTL
           END-IF

           IF EIBAID = DFHPF4
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
121012        MOVE PI-6318-REASON-IND     TO WS-REASON-CODE-IND
062712        MOVE LOW-VALUES             TO PI-6318-PASS-AREA
121012        MOVE WS-REASON-CODE-IND     TO PI-6318-REASON-IND
              MOVE XCTL-6318              TO PGM-NAME
              GO TO 9300-XCTL
           END-IF
062712
062712     IF EIBAID = DFHPF5
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
062712        IF PI-RES-REF-CLM-TYPE NOT EQUAL '2' AND '3' AND '4'
062712           MOVE ER-3831     TO  EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 8200-SEND-DATAONLY
010412        END-IF
062712     END-IF
011712
011712     IF EIBAID = DFHPF5
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
121012        MOVE PI-6318-REASON-IND     TO WS-REASON-CODE-IND
062712        MOVE LOW-VALUES             TO PI-6318-PASS-AREA
011712        MOVE 'Y'                    TO PI-CLM-REFORM-IND
121012        MOVE WS-REASON-CODE-IND     TO PI-6318-REASON-IND
011712        MOVE XCTL-6318              TO PGM-NAME
011712        GO TO 9300-XCTL
011712     END-IF

091615     IF EIBAID = DFHPF6
091615        MOVE XCTL-677            TO PGM-NAME
091615        GO TO 9300-XCTL
091615     END-IF

           IF EIBAID = DFHPF23
              GO TO 8810-PF23
           END-IF

           IF EIBAID = DFHPF24
              GO TO 9200-RETURN-MAIN-MENU
           END-IF

           IF EIBAID = DFHPF12
              GO TO 9500-PF12
           END-IF

           IF EIBAID = DFHENTER
122313       AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
                 GO TO 0350-DISPLAY-COMMON-DATA
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

           MOVE PI-COMPANY-CD          TO ENDT-COMPANY-CD


           IF CRTSFXL > +0
              MOVE CRTSFXI             TO ENDT-CERT-SFX
              MOVE AL-UANON            TO CRTSFXA
           END-IF

           MOVE +0                     TO ENDT-SEQ-NO
           MOVE 'I'                    TO ENDT-RECORD-TYPE

           IF EFFDTL > +0
              EXEC CICS BIF DEEDIT
                   FIELD   (EFFDTI)
                   LENGTH  (8)
              END-EXEC
              MOVE EFFDTI              TO DC-GREG-DATE-1-MDY
              MOVE '4'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF DATE-CONVERSION-ERROR
                 MOVE ER-0215          TO EMI-ERROR
                 MOVE -1               TO EFFDTL
                 MOVE AL-UABON         TO EFFDTA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 MOVE DC-BIN-DATE-1    TO ENDT-CERT-EFF-DT
                 MOVE AL-UANON         TO EFFDTA
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO EFFDTO
              END-IF
           END-IF

           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       0325-EXIT.
           EXIT.

       0330-SHOW-ERENDT.



           MOVE PI-ERENDT-KEY          TO ERENDT-KEY

           PERFORM 0365-READ-ERENDT    THRU 0365-EXIT

           IF RESP-NORMAL
              PERFORM 0390-BUILD-FROM-ERENDT
                                       THRU 0390-EXIT
           ELSE
              IF RESP-NOTFND OR RESP-ENDFILE
                 go to 0350-display-common-data
                 MOVE ER-2253          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO CARRL
                 GO TO 8200-SEND-DATAONLY
              ELSE
                 MOVE ER-9999          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO CARRL
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF

           GO TO 8100-SEND-INITIAL-MAP

           .
       0330-EXIT.
           EXIT.

       0350-DISPLAY-COMMON-DATA.

           IF PI-RETURN-TO-PROGRAM (1:5) = 'EL631'
092618        perform 0357-check-for-cancel
092618                                  thru 0357-exit
              PERFORM 0360-CHECK-PENDING THRU 0360-EXIT
           ELSE
              PERFORM 0355-CHECK-ELCERT THRU 0355-EXIT
           END-IF
072312
072312     PERFORM 0400-COMPARE-FROM-TO THRU 0400-EXIT
091615     PERFORM 0410-LOOK-FOR-CHECKS THRU 0410-EXIT

           GO TO 8100-SEND-INITIAL-MAP

           .
       0350-EXIT.
           EXIT.

       0355-CHECK-ELCERT.

           MOVE PI-COMPANY-CD          TO CRTO-COMPANY-CD
           MOVE PI-CARRIER             TO CRTO-CARRIER
           MOVE PI-GROUPING            TO CRTO-GROUPING
           MOVE PI-STATE               TO CRTO-STATE
           MOVE PI-ACCOUNT             TO CRTO-ACCOUNT
           MOVE PI-CERT-PRIME          TO CRTO-CERT-PRIME
           MOVE PI-CERT-SFX            TO CRTO-CERT-SFX
           MOVE PI-CERT-EFF-DT         TO CRTO-CERT-EFF-DT
           MOVE 'I'                    TO CRTO-RECORD-TYPE
           MOVE ZEROS                  TO CRTO-SEQ-NO
           MOVE ELCRTO-KEY             TO PI-ELCRTO-KEY
                                          PI-ERENDT-KEY

           PERFORM 0362-READ-ELCRTO    THRU 0362-EXIT

           IF RESP-NORMAL
              AND (PI-ELCRTO-KEY (1:33) =
                    OC-CONTROL-PRIMARY (1:33))
              AND (OC-RECORD-TYPE = 'I')
              DISPLAY ' RESP NORMAL AND = '
              MOVE OC-CONTROL-PRIMARY  TO PI-ELCRTO-KEY
              PERFORM 0361-MOVE-CRTO-TO-SCREEN THRU 0361-EXIT
           ELSE
              MOVE ER-9999             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           MOVE PI-COMPANY-CD          TO CERT-COMPANY-CD
           MOVE PI-CARRIER             TO CERT-CARRIER
           MOVE PI-GROUPING            TO CERT-GROUPING
           MOVE PI-STATE               TO CERT-STATE
           MOVE PI-ACCOUNT             TO CERT-ACCOUNT
           MOVE PI-CERT-PRIME          TO CERT-CERT-PRIME
           MOVE PI-CERT-SFX            TO CERT-CERT-SFX
           MOVE PI-CERT-EFF-DT         TO CERT-CERT-EFF-DT

           PERFORM 0375-READ-ELCERT    THRU 0375-EXIT

           .
       0355-EXIT.
           EXIT.

092618 0357-CHECK-for-cancel.
092618
092618     if pi-cert-sfx <> 'A'
092618        go to 0357-exit
092618     end-if
092618
092618     MOVE PI-COMPANY-CD          TO PNDB2-COMPANY-CD
092618     MOVE PI-carrier             TO PNDB2-CARRIER
092618     MOVE PI-grouping            TO PNDB2-GROUPING
092618     MOVE PI-state               TO PNDB2-STATE
092618     MOVE PI-account             TO PNDB2-ACCOUNT
092618     MOVE PI-cert-no             TO PNDB2-CERT-PRIME
092618     MOVE pi-cert-sfx            TO PNDB2-CERT-SFX
092618     MOVE PI-cert-eff-dt         TO PNDB2-CERT-EFF-DT
092618     MOVE +0                     TO PNDB2-CHG-SEQ-NO
092618     MOVE '1'                    TO PNDB2-RECORD-TYPE
092618
092618
092618     EXEC CICS READ
092618         DATASET   (WS-ERPNDB2-FILE-ID)
092618         SET       (ADDRESS OF PENDING-BUSINESS)
092618         RIDFLD    (ERPNDB2-KEY)
092618         RESP      (WS-RESPONSE)
092618     END-EXEC
092618
092618     if (not resp-normal)
092618        or (pb-no-of-errors = zeros)
092618        go to 0357-exit
092618     end-if
092618        
092618     perform varying e1 from +1 by +1 until
092618        (e1 > +10)
092618        or (pb-common-error(e1) = 3273)
092618     end-perform
092618     if e1 < +11
092618        MOVE ER-3273             TO EMI-ERROR
092618        PERFORM 9900-ERROR-FORMAT
092618                                 THRU 9900-EXIT
092618        MOVE -1                  TO PFENTRL
092618     end-if
092618
092618     .
092618 0357-EXIT.
           EXIT.

       0360-CHECK-PENDING.

           MOVE PI-COMPANY-CD          TO CRTO-COMPANY-CD
           MOVE PI-CARRIER             TO CRTO-CARRIER
           MOVE PI-GROUPING            TO CRTO-GROUPING
           MOVE PI-STATE               TO CRTO-STATE
           MOVE PI-ACCOUNT             TO CRTO-ACCOUNT
           MOVE PI-CERT-PRIME          TO CRTO-CERT-PRIME
           MOVE PI-CERT-SFX            TO CRTO-CERT-SFX
           MOVE PI-CERT-EFF-DT         TO CRTO-CERT-EFF-DT
           MOVE 'I'                    TO CRTO-RECORD-TYPE
           MOVE ZEROS                  TO CRTO-SEQ-NO
           MOVE ELCRTO-KEY             TO PI-ELCRTO-KEY
                                          PI-ERENDT-KEY
                                          PI-PREV-ERENDT-KEY

           PERFORM 0362-READ-ELCRTO    THRU 0362-EXIT

           IF RESP-NORMAL
              AND (PI-ELCRTO-KEY (1:33) =
                    OC-CONTROL-PRIMARY (1:33))
              AND (OC-RECORD-TYPE = 'I')
              DISPLAY ' RESP NORMAL AND = '
              MOVE OC-CONTROL-PRIMARY  TO PI-ELCRTO-KEY
              PERFORM 0361-MOVE-CRTO-TO-SCREEN THRU 0361-EXIT
              PERFORM 0370-READ-ERPNDB THRU 0370-EXIT
              IF RESP-NORMAL
                 MOVE PB-CONTROL-PRIMARY TO PI-ERPNDB-KEY
                 PERFORM 0380-BUILD-FROM-ERPNDB THRU 0380-EXIT
              END-IF
           ELSE
              MOVE ER-9999             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           .
       0360-EXIT.
           EXIT.

       0361-MOVE-CRTO-TO-SCREEN.

062712     MOVE OC-LF-BENCD              TO WS-LF-BENCD
062712     MOVE OC-AH-BENCD              TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT

062712     MOVE OC-INS-FIRST-NAME        TO OIFNAMEO
062712     MOVE OC-INS-MIDDLE-INIT       TO OIMIDO
062712     MOVE OC-INS-LAST-NAME         TO OILNAMEO
062712     MOVE OC-INS-AGE               TO OIAGEO
121712     IF OC-INS-AGE-DEFAULT-FLAG = 'Y'
121712         MOVE '*'                  TO OIAGEDFO
121712     ELSE
121712         MOVE ' '                  TO OIAGEDFO
121712     END-IF
062712     MOVE OC-JNT-FIRST-NAME        TO OJFNAMEO
062712     MOVE OC-JNT-MIDDLE-INIT       TO OJMIDO
062712     MOVE OC-JNT-LAST-NAME         TO OJLNAMEO
062712     MOVE OC-JNT-AGE               TO OJAGEO
121712     IF OC-JNT-AGE-DEFAULT-FLAG = 'Y'
121712         MOVE '*'                  TO OJAGEDFO
121712     ELSE
121712         MOVE ' '                  TO OJAGEDFO
121712     END-IF
062712     MOVE OC-1ST-PMT-DT            TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO O1PMTDTO
           END-IF
062712     MOVE OC-CRED-BENE-NAME        TO OBENEO
062712     IF OC-LF-BENCD NOT = '00' AND '  '
062712        MOVE OC-LF-BENCD           TO OLBENCDO
062712        MOVE OC-LF-BEN-AMT         TO OLBENAO
062712        MOVE OC-LF-PRM-AMT         TO OLPREMO
072312        MOVE OC-LF-ALT-BEN-AMT     TO OLALTBNO
072312        MOVE OC-LF-ALT-PRM-AMT     TO OLALTPRO
091615        compute ws-tot-old-prem =
091615           oc-lf-prm-amt + oc-lf-alt-prm-amt
091615*       compute ws-tot-old-comm rounded =
091615        compute ws-tot-old-comm =
091615           ws-tot-old-prem * oc-lf-comm-pct

062712        MOVE OC-LF-TERM            TO OLTERMO
              MOVE WS-LF-ABBRV           TO OLCOVTO
062712        MOVE OC-LF-EXP-DT          TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO OLEXPDTO
              END-IF
           END-IF

062712     IF OC-AH-BENCD NOT = '00' AND '  '
062712        MOVE OC-AH-BENCD           TO OABENCDO
062712        MOVE OC-AH-BEN-AMT         TO OABENAO
062712        MOVE OC-AH-PRM-AMT         TO OAPREMO
091615        compute ws-tot-old-prem =
091615           ws-tot-old-prem + oc-ah-prm-amt
091615*       compute ws-tot-old-comm rounded =
091615        compute ws-tot-old-comm =
091615           ws-tot-old-comm + (oc-ah-prm-amt * oc-ah-comm-pct)
062712        MOVE OC-AH-CP              TO OCPO
062712        MOVE OC-AH-TERM            TO OATERMO
              MOVE WS-WAIT-PER           TO OWAITPO
              MOVE WS-AH-DESC            TO OACOVTO
062712        MOVE OC-AH-EXP-DT          TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO OAEXPDTO
              END-IF
           END-IF

           .
       0361-EXIT.
           EXIT.

       0362-READ-ELCRTO.

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

           MOVE PI-COMPANY-CD          TO PNDB2-COMPANY-CD
           MOVE PI-CRTO-CARRIER        TO PNDB2-CARRIER
           MOVE PI-CRTO-GROUPING       TO PNDB2-GROUPING
           MOVE PI-CRTO-STATE          TO PNDB2-STATE
           MOVE PI-CRTO-ACCOUNT        TO PNDB2-ACCOUNT
           MOVE PI-CRTO-CERT-PRIME     TO PNDB2-CERT-PRIME
           MOVE PI-CRTO-CERT-SFX       TO PNDB2-CERT-SFX
           MOVE PI-CRTO-CERT-EFF-DT    TO PNDB2-CERT-EFF-DT
           MOVE +0                     TO PNDB2-CHG-SEQ-NO
           MOVE '1'                    TO PNDB2-RECORD-TYPE


           EXEC CICS READ
               DATASET   (WS-ERPNDB2-FILE-ID)
               SET       (ADDRESS OF PENDING-BUSINESS)
               RIDFLD    (ERPNDB2-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC


           .
       0370-EXIT.
           EXIT.

       0375-READ-ELCERT.

           EXEC CICS READ
               DATASET   (WS-ELCERT-FILE-ID)
               SET       (ADDRESS OF CERTIFICATE-MASTER)
               RIDFLD    (WS-ELCERT-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              set cert-found           to true
              PERFORM 0395-BUILD-FROM-ELCERT THRU 0395-EXIT
           ELSE
              IF RESP-NOTFND
                 MOVE LOW-VALUES       TO EL631HI
                 MOVE ER-2253          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO CARRL
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF

           .
       0375-EXIT.
           EXIT.

       0380-BUILD-FROM-ERPNDB.

           move pb-control-by-account (1:33)
                                       to ws-ermail-key
           EXEC CICS READ
               DATASET   ('ERMAIL')
               SET       (ADDRESS OF MAILING-DATA)
               RIDFLD    (WS-ERMAIL-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              MOVE MA-CRED-BENE-NAME   TO NBENEO
           END-IF
011712     IF NBENEO EQUAL LOW-VALUES OR SPACES
011712         MOVE ER-2883            TO EMI-ERROR
011712         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
011712     END-IF
121712
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO WS-ELCRTT-PRIMARY
121712     MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
121712
121712     EXEC CICS READ
121712          DATASET  (WS-ELCRTT-FILE-ID)
121712          RIDFLD   (WS-ELCRTT-KEY)
121712          SET      (ADDRESS OF CERTIFICATE-TRAILERS)
121712          RESP     (WS-RESPONSE)
121712     END-EXEC
121712
121712     IF NOT RESP-NORMAL
121712        MOVE SPACES TO CS-INS-AGE-DEFAULT-FLAG
121712                       CS-JNT-AGE-DEFAULT-FLAG 
121712     END-IF
121712
           MOVE PB-ENTRY-BATCH         to batchnoo
           MOVE PB-BATCH-SEQ-NO        TO BSEQNOO
101812     MOVE ZEROS                  TO ARCHNOO
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

           MOVE PB-I-LF-BENEFIT-CD     TO WS-LF-BENCD
           MOVE PB-I-AH-BENEFIT-CD     TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT

           MOVE PB-I-1ST-PMT-DT          TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO N1PMTDTO
           END-IF

           MOVE PB-I-INSURED-FIRST-NAME  TO NIFNAMEO
           MOVE PB-I-INSURED-MIDDLE-INIT TO NIMIDO
           MOVE PB-I-INSURED-LAST-NAME   TO NILNAMEO
           MOVE PB-I-AGE                 TO NIAGEO
121712     IF CS-INS-AGE-DEFAULT-FLAG = 'Y'
121712         MOVE '*'                  TO NIAGEDFO
121712     ELSE
121712         MOVE ' '                  TO NIAGEDFO
121712     END-IF
           MOVE PB-I-JOINT-FIRST-NAME    TO NJFNAMEO
           MOVE PB-I-JOINT-MIDDLE-INIT   TO NJMIDO
           MOVE PB-I-JOINT-LAST-NAME     TO NJLNAMEO
           MOVE PB-I-JOINT-AGE           TO NJAGEO
121712     IF CS-JNT-AGE-DEFAULT-FLAG = 'Y'
121712         MOVE '*'                  TO NJAGEDFO
121712     ELSE
121712         MOVE ' '                  TO NJAGEDFO
121712     END-IF

           IF PB-I-LF-BENEFIT-CD NOT = '00' AND '  '
              MOVE PB-I-LF-BENEFIT-CD    TO NLBENCDO
              MOVE PB-I-LF-BENEFIT-AMT   TO NLBENAO
              MOVE PB-I-LF-PREMIUM-AMT   TO NLPREMO
072312        MOVE PB-I-LF-ALT-BENEFIT-AMT TO NLALTBNO
072312        MOVE PB-I-LF-ALT-PREMIUM-AMT TO NLALTPRO
091615        compute ws-tot-new-prem =
091615           pb-i-lf-premium-amt + pb-i-lf-alt-premium-amt
091615*       compute ws-tot-new-comm rounded =
091615        compute ws-tot-new-comm =
091615           ws-tot-new-prem * pb-i-life-commission
              MOVE PB-I-LF-TERM          TO NLTERMO
              MOVE WS-LF-ABBRV           TO NLCOVTO
              MOVE PB-I-LF-EXPIRE-DT     TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO NLEXPDTO
              END-IF
           END-IF

           IF PB-I-AH-BENEFIT-CD NOT = '00' AND '  '
              MOVE PB-I-AH-BENEFIT-CD    TO NABENCDO
              MOVE PB-I-AH-BENEFIT-AMT   TO NABENAO
              MOVE PB-I-AH-PREMIUM-AMT   TO NAPREMO
091615        compute ws-tot-new-prem =
091615           ws-tot-new-prem + pb-i-ah-premium-amt
091615*       compute ws-tot-new-comm rounded = ws-tot-new-comm +
091615        compute ws-tot-new-comm = ws-tot-new-comm +
091615           (pb-i-ah-premium-amt * pb-i-ah-commission)
              MOVE PB-I-AH-TERM          TO NATERMO
              MOVE PB-I-AH-CRIT-PER      TO NCPO
              MOVE WS-WAIT-PER           TO NWAITPO
              MOVE WS-AH-DESC            TO NACOVTO
              MOVE PB-I-AH-EXPIRE-DT     TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO NAEXPDTO
              END-IF
           END-IF
091615     compute pi-endt-prem-diff =
091615        ws-tot-old-prem - ws-tot-new-prem
091615     move pi-endt-prem-diff       to prmdiffo
091615     compute pi-endt-comm-diff = 
091615        ws-tot-old-comm - ws-tot-new-comm

091615     move pi-endt-comm-diff      to comdiffo

           IF PB-I-LIFE-COMMISSION NOT = ZEROS
              MOVE PB-I-LIFE-COMMISSION
                                    TO LFCOMMO
      *       MOVE AL-UNNON         TO LFCOMMA
           END-IF
           IF PB-I-AH-COMMISSION NOT = ZEROS
              MOVE PB-I-AH-COMMISSION
                                    TO AHCOMMO
      *       MOVE AL-UNNON         TO AHCOMMA
           END-IF


           MOVE ER-2785                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT

           .
       0380-EXIT.
           EXIT.

       0390-BUILD-FROM-ERENDT.

           MOVE LOW-VALUES             TO EL631HI
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
           MOVE PI-ERENDT-KEY          TO PI-PREV-ERENDT-KEY

           MOVE EN-CARRIER             TO CARRO
           MOVE EN-GROUPING            TO GROUPO
           MOVE EN-STATE               TO STATEO
           MOVE EN-ACCOUNT             TO ACCTO
           MOVE EN-CERT-PRIME          TO CERTNOO
           MOVE EN-CERT-SFX            TO CRTSFXO
           MOVE EN-SEQ-NO              TO SEQNOO
101812     MOVE EN-ARCHIVE-NO          TO ARCHNOO

           MOVE EN-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           END-IF

           IF EN-LAST-MAINT-HHMMSS NOT NUMERIC
              MOVE +0                  TO EN-LAST-MAINT-HHMMSS
           END-IF

           MOVE EN-LAST-MAINT-HHMMSS   TO TIME-MT
                                          PI-UPDATE-HHMMSS

           IF EN-PROCESS-DT NOT = SPACES AND LOW-VALUES
              MOVE EN-PROCESS-DT       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO PROCDTO
              END-IF
           END-IF
           IF EN-TEMPLATE-USED NOT = SPACES
              MOVE EN-TEMPLATE-USED    TO TEMPLO
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

           MOVE EN-LF-ORIG-BENCD       TO WS-LF-BENCD
           MOVE EN-AH-ORIG-BENCD       TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT

           MOVE EN-INS-ORIG-LAST-NAME    TO OILNAMEO          
           MOVE EN-INS-ORIG-FIRST-NAME   TO OIFNAMEO          
           MOVE EN-INS-ORIG-MIDDLE-INIT  TO OIMIDO
           MOVE EN-INS-ORIG-AGE          TO OIAGEO
121712     IF EN-INS-ORIG-AGE-DEF-FLAG = 'Y'
121712         MOVE '*'                  TO OIAGEDFO
121712     ELSE
121712         MOVE ' '                  TO OIAGEDFO
121712     END-IF
           MOVE EN-JNT-ORIG-LAST-NAME    TO OJLNAMEO          
           MOVE EN-JNT-ORIG-FIRST-NAME   TO OJFNAMEO          
           MOVE EN-JNT-ORIG-MIDDLE-INIT  TO OJMIDO
           MOVE EN-JNT-ORIG-AGE          TO OJAGEO
121712     IF EN-JNT-ORIG-AGE-DEF-FLAG = 'Y'
121712         MOVE '*'                  TO OJAGEDFO
121712     ELSE
121712         MOVE ' '                  TO OJAGEDFO
121712     END-IF

091615     move +0                     to ws-tot-old-prem
091615                                    ws-tot-new-prem

           IF EN-LF-ORIG-BENCD NOT = '00' AND '  '
              MOVE EN-LF-ORIG-BENCD    TO OLBENCDO
              MOVE EN-LF-ORIG-TERM     TO OLTERMO
              MOVE EN-LF-ORIG-BEN-AMT  TO OLBENAO
              MOVE EN-LF-ORIG-PRM-AMT  TO OLPREMO
072312        MOVE EN-LF-ORIG-ALT-BEN-AMT TO OLALTBNO
072312        MOVE EN-LF-ORIG-ALT-PRM-AMT TO OLALTPRO
091615        compute ws-tot-old-prem =
091615           en-lf-orig-prm-amt + en-lf-orig-alt-prm-amt
091615        compute ws-tot-old-comm =
091615           ws-tot-old-prem * en-lf-orig-comm-pct
              MOVE WS-LF-ABBRV         TO OLCOVTO
              MOVE EN-LF-ORIG-EXP-DT   TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO OLEXPDTO
              END-IF
           END-IF

           IF EN-AH-ORIG-BENCD NOT = '00' AND '  '
              MOVE EN-AH-ORIG-BENCD    TO OABENCDO
              MOVE EN-AH-ORIG-TERM     TO OATERMO
              MOVE EN-AH-ORIG-BEN-AMT  TO OABENAO
              MOVE EN-AH-ORIG-PRM-AMT  TO OAPREMO
091615        compute ws-tot-old-prem =
091615           ws-tot-old-prem + en-ah-orig-prm-amt
091615        compute ws-tot-old-comm = ws-tot-old-comm +
091615           (en-ah-orig-prm-amt * en-ah-orig-comm-pct)
              MOVE EN-AH-ORIG-CP       TO OCPO
              MOVE WS-WAIT-PER         TO OWAITPO
              MOVE WS-AH-DESC          TO OACOVTO
              MOVE EN-AH-ORIG-EXP-DT   TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO OAEXPDTO
              END-IF
           END-IF

           MOVE EN-INS-NEW-LAST-NAME     TO NILNAMEO          
           MOVE EN-INS-NEW-FIRST-NAME    TO NIFNAMEO          
           MOVE EN-INS-NEW-MIDDLE-INIT   TO NIMIDO
           MOVE EN-INS-NEW-AGE       TO NIAGEO
121712     IF EN-INS-NEW-AGE-DEF-FLAG = 'Y'
121712         MOVE '*'                  TO NIAGEDFO
121712     ELSE
121712         MOVE ' '                  TO NIAGEDFO
121712     END-IF
           MOVE EN-JNT-NEW-LAST-NAME     TO NJLNAMEO          
           MOVE EN-JNT-NEW-FIRST-NAME    TO NJFNAMEO          
           MOVE EN-JNT-NEW-MIDDLE-INIT   TO NJMIDO
           MOVE EN-JNT-NEW-AGE           TO NJAGEO
121712     IF EN-JNT-NEW-AGE-DEF-FLAG = 'Y'
121712         MOVE '*'                  TO NJAGEDFO
121712     ELSE
121712         MOVE ' '                  TO NJAGEDFO
121712     END-IF

           MOVE EN-LF-NEW-BENCD        TO WS-LF-BENCD
           MOVE EN-AH-NEW-BENCD        TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT

           IF EN-LF-NEW-BENCD NOT = '00' AND '  '
              MOVE EN-LF-NEW-BENCD     TO NLBENCDO
              MOVE EN-LF-NEW-TERM      TO NLTERMO
              MOVE EN-LF-NEW-BEN-AMT   TO NLBENAO
              MOVE EN-LF-NEW-PRM-AMT   TO NLPREMO
072312        MOVE EN-LF-NEW-ALT-BEN-AMT TO NLALTBNO
072312        MOVE EN-LF-NEW-ALT-PRM-AMT TO NLALTPRO
091615        compute ws-tot-new-prem =
091615           en-lf-new-prm-amt + en-lf-new-alt-prm-amt
091615        compute ws-tot-new-comm =
091615           ws-tot-new-prem * en-lf-new-comm-pct
              MOVE WS-LF-ABBRV         TO NLCOVTO
              MOVE EN-LF-NEW-EXP-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO NLEXPDTO
              END-IF
           END-IF

           IF EN-AH-NEW-BENCD NOT = '00' AND '  '
              MOVE EN-AH-NEW-BENCD     TO NABENCDO
              MOVE EN-AH-NEW-TERM      TO NATERMO
              MOVE EN-AH-NEW-BEN-AMT   TO NABENAO
              MOVE EN-AH-NEW-PRM-AMT   TO NAPREMO
091615        compute ws-tot-new-prem =
091615           ws-tot-new-prem + en-ah-new-prm-amt
091615        compute ws-tot-new-comm = ws-tot-new-comm +
091615           en-ah-new-prm-amt * en-ah-new-comm-pct
              MOVE EN-AH-NEW-CP        TO NCPO
              MOVE WS-WAIT-PER         TO NWAITPO
              MOVE WS-AH-DESC          TO NACOVTO
              MOVE EN-AH-NEW-EXP-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO NAEXPDTO
              END-IF
           END-IF
091615     compute pi-endt-prem-diff =
091615        ws-tot-old-prem - ws-tot-new-prem
091615     move pi-endt-prem-diff       to prmdiffo
091615     if en-check-type = 'C'
091615        move 'YES' to corchkso
091615     end-if

091615     compute pi-endt-comm-diff = 
091615        ws-tot-old-comm - ws-tot-new-comm
091615     move pi-endt-comm-diff      to comdiffo

           STRING EN-REASON-CODE (1)    ' '
                  EN-REASON-CODE (2)    ' '
                  EN-REASON-CODE (3)    ' '
                  EN-REASON-CODE (4)    ' '
                  EN-REASON-CODE (5)    ' '
                  EN-REASON-CODE (6)    ' '
                  EN-REASON-CODE (7)    ' '
                  EN-REASON-CODE (8)    ' '
                  EN-REASON-CODE (9)    ' '
                  EN-REASON-CODE (10)   ' '
                  EN-REASON-CODE (11)   ' '
                  EN-REASON-CODE (12)   ' '
              DELIMITED BY '  ' INTO REACDSO
           END-STRING
           MOVE EN-SIG-SW              TO SIGO
           MOVE EN-MONEY-SW            TO MONEYO
           MOVE EN-HEALTH-APP          TO HLTHAPPO
           MOVE EN-VOUCHER-SW          TO VCHERO
           MOVE EN-PAYEE               TO PAYEEO

           .
       0390-EXIT.
           EXIT.

       0395-BUILD-FROM-ELCERT.
121712
121712     MOVE WS-ELCERT-KEY         TO WS-ELCRTT-PRIMARY
121712     MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
121712
121712     EXEC CICS READ
121712          DATASET  (WS-ELCRTT-FILE-ID)
121712          RIDFLD   (WS-ELCRTT-KEY)
121712          SET      (ADDRESS OF CERTIFICATE-TRAILERS)
121712          RESP     (WS-RESPONSE)
121712     END-EXEC
121712
121712     IF NOT RESP-NORMAL
121712        MOVE SPACES TO CS-INS-AGE-DEFAULT-FLAG
121712                       CS-JNT-AGE-DEFAULT-FLAG 
121712     END-IF
121712

           MOVE CM-CARRIER             TO CARRO
           MOVE CM-GROUPING            TO GROUPO
           MOVE CM-STATE               TO STATEO
           MOVE CM-ACCOUNT             TO ACCTO
           MOVE CM-CERT-PRIME          TO CERTNOO
           MOVE CM-CERT-SFX            TO CRTSFXO
       
           MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           END-IF

           MOVE CM-LF-BENEFIT-CD       TO WS-LF-BENCD
           MOVE CM-AH-BENEFIT-CD       TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT

           MOVE CM-LOAN-1ST-PMT-DT     TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO N1PMTDTO
           END-IF

           MOVE CM-INSURED-FIRST-NAME  TO NIFNAMEO
           MOVE CM-INSURED-INITIAL2    TO NIMIDO
           MOVE CM-INSURED-LAST-NAME   TO NILNAMEO
           MOVE CM-INSURED-ISSUE-AGE   TO NIAGEO
121712     IF CS-INS-AGE-DEFAULT-FLAG = 'Y'
121712         MOVE '*'                TO NIAGEDFO
121712     ELSE
121712         MOVE ' '                TO NIAGEDFO
121712     END-IF
           MOVE CM-JT-FIRST-NAME       TO NJFNAMEO
           MOVE CM-JT-INITIAL          TO NJMIDO
           MOVE CM-JT-LAST-NAME        TO NJLNAMEO
           MOVE CM-INSURED-JOINT-AGE   TO NJAGEO
121712     IF CS-JNT-AGE-DEFAULT-FLAG = 'Y'
121712         MOVE '*'                TO NJAGEDFO
121712     ELSE
121712         MOVE ' '                TO NJAGEDFO
121712     END-IF

           IF CM-LF-BENEFIT-CD NOT = '00' AND '  '
              MOVE CM-LF-BENEFIT-CD    TO NLBENCDO
              MOVE CM-LF-BENEFIT-AMT   TO NLBENAO
              MOVE CM-LF-PREMIUM-AMT   TO NLPREMO
072312        MOVE CM-LF-ALT-BENEFIT-AMT TO NLALTBNO
072312        MOVE CM-LF-ALT-PREMIUM-AMT TO NLALTPRO
091615        compute ws-tot-new-prem =
091615           cm-lf-premium-amt + cm-lf-alt-premium-amt
091615*       compute ws-tot-new-comm rounded =
091615        compute ws-tot-new-comm =
091615           ws-tot-new-prem * cm-life-comm-pct
              MOVE CM-LF-ORIG-TERM     TO NLTERMO
              MOVE WS-LF-ABBRV         TO NLCOVTO
              MOVE CM-LF-LOAN-EXPIRE-DT TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO NLEXPDTO
              END-IF
           END-IF

           IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
              MOVE CM-AH-BENEFIT-CD      TO NABENCDO
              MOVE CM-AH-BENEFIT-AMT     TO NABENAO
              MOVE CM-AH-PREMIUM-AMT     TO NAPREMO
091615        compute ws-tot-new-prem =
091615           ws-tot-new-prem + cm-ah-premium-amt
091615*       compute ws-tot-new-comm rounded = ws-tot-new-comm +
091615        compute ws-tot-new-comm = ws-tot-new-comm +
091615           (cm-ah-premium-amt * cm-ah-comm-pct)
              MOVE CM-AH-ORIG-TERM       TO NATERMO
              MOVE CM-AH-CRITICAL-PERIOD TO NCPO
              MOVE WS-WAIT-PER           TO NWAITPO
              MOVE WS-AH-DESC            TO NACOVTO
              MOVE CM-AH-LOAN-EXPIRE-DT  TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO NAEXPDTO
              END-IF
           END-IF
091615     compute pi-endt-prem-diff =
091615        ws-tot-old-prem - ws-tot-new-prem
091615     move pi-endt-prem-diff       to prmdiffo

091615     compute pi-endt-comm-diff = 
091615        ws-tot-old-comm - ws-tot-new-comm
091615     move pi-endt-comm-diff      to comdiffo

           IF CM-LIFE-COMM-PCT NOT = ZEROS
              MOVE CM-LIFE-COMM-PCT    TO LFCOMMO
           END-IF
           IF CM-AH-COMM-PCT NOT = ZEROS
              MOVE CM-AH-COMM-PCT      TO AHCOMMO
           END-IF

           MOVE ER-2784                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT

           .
       0395-EXIT.
           EXIT.

072312 0400-COMPARE-FROM-TO.
072312
072312      IF OILNAMEO = NILNAMEO AND
072312         OIFNAMEO = NIFNAMEO AND
072312         OIMIDO   = NIMIDO   AND
072312         OIAGEO   = NIAGEO   AND
072312         OJLNAMEO = NJLNAMEO AND
072312         OJFNAMEO = NJFNAMEO AND
072312         OJMIDO   = NJMIDO   AND
072312         OJAGEO   = NJAGEO   AND
072312         OLBENCDO = NLBENCDO AND   
072312         OLTERMO  = NLTERMO  AND   
072312         OLBENAO  = NLBENAO  AND   
072312         OLPREMO  = NLPREMO  AND 
072312         OLALTBNO = NLALTBNO AND 
072312         OLALTPRO = NLALTPRO AND   
072312         OLCOVTO  = NLCOVTO  AND
072312         OLEXPDTO = NLEXPDTO AND
072312         OABENCDO = NABENCDO AND                     
072312         OATERMO  = NATERMO  AND                     
072312         OABENAO  = NABENAO  AND                     
072312         OAPREMO  = NAPREMO  AND                     
072312         OCPO     = NCPO     AND                     
072312         OWAITPO  = NWAITPO  AND                     
072312         OACOVTO  = NACOVTO  AND                     
072312         OAEXPDTO = NAEXPDTO AND
072312         O1PMTDTO = N1PMTDTO
072312            MOVE 'N'   TO PI-CHANGES-MADE
072312      ELSE
072312            MOVE 'Y'   TO PI-CHANGES-MADE
072312      END-IF
072312
072312      IF OLBENCDO = NLBENCDO AND   
072312         OLTERMO  = NLTERMO  AND   
072312         OLBENAO  = NLBENAO  AND   
072312         OLPREMO  = NLPREMO  AND 
072312         OLALTBNO = NLALTBNO AND 
072312         OLALTPRO = NLALTPRO AND   
072312         OLCOVTO  = NLCOVTO  AND
072312         OLEXPDTO = NLEXPDTO AND
072312         OABENCDO = NABENCDO AND                     
072312         OATERMO  = NATERMO  AND                     
072312         OABENAO  = NABENAO  AND                     
072312         OAPREMO  = NAPREMO  AND                     
072312         OCPO     = NCPO     AND                     
072312         OWAITPO  = NWAITPO  AND                     
072312         OACOVTO  = NACOVTO  AND                     
072312         OAEXPDTO = NAEXPDTO AND
072312         O1PMTDTO = N1PMTDTO
072312            MOVE 'N'   TO PI-AMT-CHANGES-MADE
072312      ELSE
072312            MOVE 'Y'   TO PI-AMT-CHANGES-MADE
072312      END-IF
121012
121012      MOVE SPACES TO WS-REASON-CODE-IND
121012
121012      IF OILNAMEO NOT = NILNAMEO OR
121012         OIFNAMEO NOT = NIFNAMEO OR
121012         OIMIDO   NOT = NIMIDO  
121012          MOVE 'A' TO WS-REASON-A
121012      END-IF
121012
121012      IF OJLNAMEO NOT = NJLNAMEO OR
121012         OJFNAMEO NOT = NJFNAMEO OR
121012         OJMIDO   NOT = NJMIDO  
121012          MOVE 'B' TO WS-REASON-B
121012      END-IF
020613
020613      IF OIAGEO NOT = NIAGEO     
020613          MOVE 'C' TO WS-REASON-C
020613      END-IF
020613
020613      IF OJAGEO NOT = NJAGEO     
020613          MOVE 'D' TO WS-REASON-D
020613      END-IF
121012
121012      IF OLBENCDO NOT = NLBENCDO
121012          MOVE 'E' TO WS-REASON-E
121012      END-IF
121012
121012      IF OLBENAO NOT = NLBENAO  OR   
121012         OLALTBNO NOT = NLALTBNO
121012          MOVE 'F' TO WS-REASON-F
121012      END-IF
121012
121012      IF OLPREMO NOT = NLPREMO  OR 
121012         OLALTPRO NOT = NLALTPRO 
121012          MOVE 'G' TO WS-REASON-G
121012      END-IF
121012
121012      IF OLTERMO NOT = NLTERMO     
121012          MOVE 'H' TO WS-REASON-H
121012      END-IF
121012
121012      IF OLEXPDTO NOT = NLEXPDTO 
121012          MOVE 'I' TO WS-REASON-I
121012      END-IF
121012
121012      IF OABENCDO NOT = NABENCDO
121012          MOVE 'J' TO WS-REASON-J
121012      END-IF
121012
121012      IF OABENAO NOT = NABENAO
121012          MOVE 'K' TO WS-REASON-K
121012      END-IF
121012
121012      IF OAPREMO NOT = NAPREMO
121012          MOVE 'L' TO WS-REASON-L
121012      END-IF
121012
121012      IF OATERMO NOT = NATERMO
121012          MOVE 'M' TO WS-REASON-M
121012      END-IF
121012
121012      IF OAEXPDTO NOT = NAEXPDTO
121012          MOVE 'N' TO WS-REASON-N
121012      END-IF
072312
121012      MOVE WS-REASON-CODE-IND TO PI-6318-REASON-IND
121012
072312     .
072312 0400-EXIT.
072312     EXIT.
072312

091615 0410-LOOK-FOR-CHECKS.
091615
091615     move pi-company-cd          to CHEK-COMPANY-CD 
091615     move pi-carrier             to CHEK-CARRIER    
091615     move pi-grouping            to CHEK-GROUPING   
091615     move pi-state               to CHEK-STATE      
091615     move pi-account             to CHEK-ACCOUNT    
091615     move pi-cert-eff-dt         to CHEK-CERT-EFF-DT
091615     move pi-cert-prime          to CHEK-CERT-PRIME 
091615     move pi-cert-sfx            to CHEK-CERT-SFX   
091615     move +0                     to CHEK-SEQ-NO     
091615
091615     perform 0420-startbr-erchek thru 0420-exit
091615
091615     if not resp-normal
091615        go to 0410-exit
091615     end-if
091615
091615     perform until i-say-stop or i-have-checks
091615        perform 0430-readnext-erchek
091615                                 thru 0430-exit
091615        if (not resp-normal)
091615           or (pb-control-by-account (1:33) not =
091615              ch-control-primary (1:33))
091615              set i-say-stop to true
091615        else
091615           if (ch-void-dt = low-values)
091615              set i-have-checks to true
091615           end-if
091615        end-if
091615     end-perform
091615
091615*    if i-have-checks
091615*       move 'YES'               to corchkso
091615*    else
091615*       move 'NO'                to corchkso
091615*    end-if
091615
091615     if ws-chek-browse-sw = 'Y'
091615        perform 0440-endbr-erchek thru 0440-exit
091615     end-if
091615
091615     .
091615 0410-EXIT.
091615     EXIT.
091615
091615 0420-startbr-erchek.
091615
091615     move pb-control-by-account  to erchek-key
091615     move +0                     to chek-seq-NO
091615
091615     exec cics startbr
091615        dataset   ('ERCHEK')
091615        ridfld    (erchek-key)
091615        gteq
091615        resp      (ws-response)
091615     end-exec
091615
091615     IF RESP-NORMAL
091615        move 'Y'                 to ws-chek-browse-sw
091615     end-if
091615
091615     .
091615 0420-exit.
091615     exit.
091615
091615 0430-readnext-erchek.
091615
091615     EXEC CICS READNEXT
091615        SET     (ADDRESS OF CHECK-RECORDS)
091615        DATASET ('ERCHEK')
091615        RIDFLD  (ERCHEK-KEY)
091615        RESP    (WS-RESPONSE)
091615     END-EXEC
091615
091615     .
091615 0430-exit.
091615     exit.
091615
091615 0440-endbr-erchek.
091615
091615     exec cics endbr
091615        dataset  ('ERCHEK')
091615     end-exec
091615
091615     .
091615 0440-exit.
091615     exit.

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
122313        MOVE 'N'              TO PI-ENDORSE-RECORD
122313        MOVE LOW-VALUES       TO EL631HI
122313        GO TO 0350-DISPLAY-COMMON-DATA
           END-IF

           .
       0530-EXIT.
           EXIT.

       1130-MATCH-BENCDS.

           MOVE SPACES                 TO WS-LF-ABBRV
                                          WS-LF-DESC
                                          WS-WAIT-PER
                                          WS-RET-ELIM
                                          WS-AH-DESC
                                          WS-BEN-DAYS
           IF WS-LF-BENCD NOT = ZEROS AND SPACES
              PERFORM 1140-MATCH-LFBEN THRU 1140-EXIT
           END-IF

           IF WS-AH-BENCD NOT = ZEROS AND SPACES
              PERFORM 1150-MATCH-AHBEN THRU 1150-EXIT
           END-IF

           .
       1130-EXIT.
           EXIT.

       1140-MATCH-LFBEN.

           MOVE PI-COMPANY-ID          TO ELCNTL-KEY
           MOVE '4'                    TO CNTL-REC-TYPE
           MOVE WS-LF-BENCD            TO CNTL-BEN-CD
           MOVE +0                     TO CNTL-SEQ
           EXEC CICS READ
              SET     (ADDRESS OF CONTROL-FILE)
              DATASET (WS-ELCNTL-FILE-ID)
              RIDFLD  (ELCNTL-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (WS-LF-BENCD = CF-BENEFIT-CODE (S1))
                 OR (S1 > +8)
              END-PERFORM
              IF S1 < + 9
                 MOVE CF-BENEFIT-DESCRIP (S1)
                                       TO WS-LF-ABBRV
091615           move cf-joint-indicator (s1)
091615                                 to ws-lf-joint-ind
                 MOVE WS-LF-BENCD      TO WS-LF-DESC
              ELSE
                 MOVE 'XX'             TO WS-LF-DESC
                 MOVE 'XXX'            TO WS-LF-ABBRV
              END-IF
           ELSE
              DISPLAY ' NO LF BEN CD ' WS-LF-BENCD
           END-IF

           .
       1140-EXIT.
           EXIT.

       1150-MATCH-AHBEN.

           MOVE PI-COMPANY-ID          TO ELCNTL-KEY
           MOVE '5'                    TO CNTL-REC-TYPE
           MOVE WS-AH-BENCD            TO CNTL-BEN-CD
           MOVE +0                     TO CNTL-SEQ
           EXEC CICS READ
              SET     (ADDRESS OF CONTROL-FILE)
              DATASET (WS-ELCNTL-FILE-ID)
              RIDFLD  (ELCNTL-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (WS-AH-BENCD = CF-BENEFIT-CODE (S1))
                 OR (S1 > +8)
              END-PERFORM
              IF S1 < + 9
                 MOVE CF-BENEFIT-ALPHA (S1)
                                       TO WS-WAIT-PER
                 MOVE CF-BENEFIT-ALPHA (S1) (3:1)
                                       TO WS-RET-ELIM
                 MOVE CF-BENEFIT-ALPHA (S1) (1:2)
                                       TO WS-BEN-DAYS
                 IF CF-SPECIAL-CALC-CD (S1) = 'C'
                    IF CF-JOINT-INDICATOR (S1) = 'J'
                       MOVE 'JOINT CRITICAL PERIOD DISABILITY'
                                       TO WS-AH-DESC
                    ELSE
                       MOVE 'SINGLE CRITICAL PERIOD DISABILITY'
                                       TO WS-AH-DESC
                    END-IF
                 ELSE
                    IF CF-JOINT-INDICATOR (S1) = 'J'
                       MOVE 'JOINT DISABILITY'
                                       TO WS-AH-DESC
                    ELSE
                       MOVE 'SINGLE DISABILITY'
                                       TO WS-AH-DESC
                    END-IF
                 END-IF
              ELSE
                 MOVE 'NOT FOUND'      TO WS-AH-DESC
              END-IF
           ELSE
              DISPLAY ' NO AH BEN CD ' WS-AH-BENCD
           END-IF

           .
       1150-EXIT.
           EXIT.

       8100-SEND-INITIAL-MAP.

           MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
                                       TO SYSO
           MOVE WS-KIXHOST             TO HOSTO
           MOVE PI-COMPANY-ID          TO COMPANYO
           MOVE '4'                    TO DATASORO
           MOVE PI-PROCESSOR-ID        TO PROCIDO

101812*    MOVE ZEROS                  TO ARCHNOO
122313     IF PI-ENDORSE-RECORD = 'Y'
122313            MOVE AL-SADOF        TO PF3N5A
122313                                    PF4A
122313     END-IF

           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK.
           MOVE DC-GREG-DATE-1-EDIT    TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE -1                     TO CARRL
011712     IF EMI-ERROR-NUMBER (1) = ER-2883
011712         MOVE 'W'                TO EMI-SEVERITY (1)
011712     END-IF.
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.

           EXEC CICS SEND
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               FROM     (EL631HO)
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
               FROM     (EL631HO)
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

