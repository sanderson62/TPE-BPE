      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL691.
00004 *
00005 *AUTHOR.    LOGIC, INC.
00006 *           DALLAS, TEXAS.
00007
00008 *DATE-COMPILED.
00009
00010 *REMARKS.
00011 *        THIS PROGRAM PROVIDES THE FUNCTIONS TO BROWSE AND EDIT
00012 *    THE DETAIL OF AN ARCHIVED LETTER.
00013
00014 *    TRANS ID = EXN1
00015
071111******************************************************************
071111*                   C H A N G E   L O G
071111*
071111* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
071111*-----------------------------------------------------------------
071111*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
071111* EFFECTIVE    NUMBER
071111*-----------------------------------------------------------------
071111* 071111    2011022800001  AJRA  NAPERSOFT - NEW SCREEN
100312* 100312    2011022800001  AJRA  CHANGE RECEIVED MSG
121112* 121112    2012101700002  AJRA  ADD PF6 TO CERT NOTE, ENDT ARCH N
122612* 122612    2012101700002  AJRA  UPDATE BILLING NOTE ON RECEIVED
100813* 100813    2013100700002  AJRA  VERITY RESEND LETTER ID ON CHANGE
102918* 102918  CR2018080300002  PEMA  ADD VOID OB LETTER OPTION
041320* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
102020* 102020 IR2020101300001   PEMA  Correct billing notes
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
071111******************************************************************
00016
00017  DATA DIVISION.
00018  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00019  77  FILLER  PIC X(32) VALUE '********************************'.
00020  77  FILLER  PIC X(32) VALUE '*    EL691 WORKING STORAGE     *'.
00021  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.037 *********'.
102918 77  c1                          pic s9(5) comp-3 value +0.
102918 77  n1                          pic s999  comp-3 value +0.
102918 77  w-comment-line-cnt          pic s999 comp-3 value +0.
102918 77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
102918 77  note-count                  pic s999 comp-3 value +0.
102918 77  ws-build-note-sw            pic x value ' '.
102918     88  finished-with-notes      value 'Y'.
102918 77  ws-ercnot-sw                pic x  value spaces.
102918     88  ercnot-startbr            value 'Y'.
102918
102918 01  P pointer.
102918 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
102918 01  var-ptr pointer.
102918 01  env-var-len                 pic 9(4)  binary.
102918 01  rc                          pic 9(9)  binary.
102918
102918 01  WS-KIXSYS.
102918     05  WS-KIX-FIL1             PIC X(10).
102918     05  WS-KIX-APPS             PIC X(10).
102918     05  WS-KIX-ENV              PIC X(10).
102918     05  WS-KIX-MYENV            PIC X(10).
102918     05  WS-KIX-SYS              PIC X(10).
       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC
       01  sqlcmd                      pic x(1024).
       01  WS-MOE-DATE                 pic x(10).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-sql-code                 pic s9(7) value zeros.
       01  ws-dis-sql-code             pic -9999999 value zeros.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is passed nulls from sql. The indicator will be -1        ***
      ***  if the value on sql is nulls and +0 if the value is       ***
      ***  something other than nulls. Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch keyword into                                  ***
      ***           :db-Complete-dt :nu-Complete-date,               ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
       01  indicator-vaiables-for-nulls.
           05  nu-Complete-date        pic s9(4) comp value +0.
       01  key-word-key-data.
           05  kd-unique-id            pic 9(7).
           05  kd-cert-no              pic x(11).
       01  key-word-table-data.
           05  db-req-date             pic x(10).
           05  db-doc-type             pic x(30).
           05  db-Cert-no              pic x(11).
           05  db-Print-dt             pic x(10).
           05  db-Cert-exp-dt          pic x(10).
           05  db-cert-state           pic xx.
           05  db-key-word-type        pic x(30).
           05  db-key-word-value       pic x(30).
           05  db-complete-dt          pic x(10).
       EXEC SQL
          END DECLARE SECTION
       END-EXEC
00023
00024  01  W-WORK-AREAS.
102918     12  ws-sql-update-sw        pic x value ' '.
102918         88  sql-update-succeeded    value 'Y'.
102918         88  sql-update-failed       value 'N'.
102918     12  ws-connect-sw           pic x  value ' '.
102918         88  connected-to-db        value 'Y'.
102918     12  ws-cert-sw              pic x value ' '.
102918         88  cert-found             value 'Y'.
00025      12  FILLER                  PIC  X(18)
00026                                       VALUE 'PROGRAM WORK AREA:'.
00027      12  W-LAST-ERROR            PIC  9(04) VALUE 9999.
00028      12  W-CALL-PGM              PIC  X(08).
00029      12  W-CURRENT-SAVE          PIC  X(02) VALUE SPACES.
00030      12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
00031      12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.
00032
00033      12  W-TIME-IN               PIC S9(07).
00034      12  FILLER REDEFINES W-TIME-IN.
00035          16  FILLER              PIC  X(01).
00036          16  W-TIME-OUT          PIC  9(02)V9(02).
00037          16  FILLER              PIC  X(02).
00038
00039      12  W-DEEDIT-FIELD          PIC  X(15).
00040      12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD PIC S9(15).
00041
00042      12  W-ERROR-COUNT           PIC S9(3)       VALUE ZERO.
00043      12  W-UPDATE-SW             PIC S9          VALUE ZERO.
00044      12  W-RESEND-DATE           PIC XX    VALUE LOW-VALUES.
00045      12  W-SENT-DATE             PIC XX    VALUE LOW-VALUES.
00046      12  W-FINAL-ACT-DATE        PIC XX    VALUE LOW-VALUES.
00047      12  W-RECEIVED-DATE         PIC XX    VALUE LOW-VALUES.
00048      12  W-STOP-LETTER-DATE      PIC XX    VALUE LOW-VALUES.
00049      12  W-RESPONSE              PIC S9(8)   COMP.
00050          88  RESP-NORMAL                  VALUE +00.
00051          88  RESP-NOTFND                  VALUE +13.
00052          88  RESP-DUPREC                  VALUE +14.
00053          88  RESP-DUPKEY                  VALUE +15.
00054          88  RESP-NOTOPEN                 VALUE +19.
00055          88  RESP-ENDFILE                 VALUE +20.
102918         88  resp-lengtherr               value +22.
00056
00057      12  W-ARCH-SAVE-KEY         PIC  X(03).
00058      12  W-ARCH-KEY.
00059          16  W-ARCH-COMPANY-CD   PIC  X(01).
00060          16  W-ARCH-NUMBER       PIC S9(08)      COMP.
00061
00062      12  W-ARCT-KEY.
00063          16  W-ARCT-COMPANY-CD   PIC  X(01).
00064          16  W-ARCT-ARCHIVE-NO   PIC S9(08) COMP.
00065          16  W-ARCT-RECORD-TYPE  PIC  X(01).
00066              88  W-ARCT-COMMENT-DATA   VALUE '3'.
00067          16  W-ARCT-LINE-SEQ-NO  PIC S9(04) COMP.
00068
102918     12  w-stop-letter-comment   pic x(126) value spaces.
102918     12  w-cert-note-comment     pic x(70) value spaces.
00069      12  W-CERT-NOTE-MSG.
00070          16  FILLER              PIC X(29)
100312             VALUE 'REQUESTED DOCUMENT RECEIVED  '.
00072          16  W-CERT-NOTE-RECV-DT PIC X(8).
122612
122612     12  WS-FIND-BILLING-NOTE.
122612         16  WS-FBN-NOTE         PIC X(25).
122612         16  WS-FBN-LTRID        PIC X(4).
122612
122612     12  WS-RECEIVED-NOTE.
122612         16  FILLER              PIC X(12)
122612             VALUE ' - RECEIVED '.
122612         16  WS-BN-RECV-DATE     PIC X(8).
100813
100813     12  WS-Z-RECORD-IND         PIC X(1) VALUE 'N'.
100813         88 Z-RECORD-FOUND                VALUE 'Y'.
100813         88 Z-RECORD-NOT-FOUND            VALUE 'N'.
00073
00074      12  ELCERT-FILE-ID          PIC  X(08)  VALUE 'ELCERT'.
00075      12  ERCNOT-FILE-ID          PIC  X(08)  VALUE 'ERCNOT'.
00076      12  ERCNOT-LENGTH           PIC S9(4)   COMP VALUE +150.
00077      12  ERCNOT-KEY-LENGTH       PIC S9(4)   COMP VALUE +36.
00078      12  ERCNOT-START-LENGTH     PIC S9(4)   COMP VALUE +34.
00079      12  ERCNOT-KEY.
00080          16  ERCNOT-PARTIAL-KEY.
00081              20 ERCNOT-COMPANY-CD    PIC X.
00082              20 ERCNOT-CARRIER       PIC X.
00083              20 ERCNOT-GROUPING      PIC X(06).
00084              20 ERCNOT-STATE         PIC XX.
00085              20 ERCNOT-ACCOUNT       PIC X(10).
00086              20 ERCNOT-EFF-DT        PIC XX.
00087              20 ERCNOT-CERT-NO.
00088                 25 ERCNOT-CERT-PRIME PIC X(10).
00089                 25 ERCNOT-CERT-SFX   PIC X.
00090              20 ERCNOT-REC-TYP       PIC X.
00091          16 ERCNOT-SEQ           PIC S9(4) COMP.
00092      12  SV-PRIOR-KEY.
00093          16 SV-COMPANY-CD            PIC X.
00094          16 SV-CARRIER               PIC X.
00095          16 SV-GROUPING              PIC X(06).
00096          16 SV-STATE                 PIC XX.
00097          16 SV-ACCOUNT               PIC X(10).
00098          16 SV-EFF-DT                PIC XX.
00099          16 SV-CERT-NO.
00100             20 SV-CERT-PRIME         PIC X(10).
00101             20 SV-CERT-SFX           PIC X(1).
00102          16 SV-REC-TYP               PIC X.
00103          16  SV-NOTE-SEQUENCE        PIC S9(4) COMP.
00104      12  ELCERT-KEY.
00105          16  ELCERT-COMPANY-CD        PIC X.
00106          16  ELCERT-CARRIER           PIC X.
00107          16  ELCERT-GROUPING          PIC X(6).
00108          16  ELCERT-STATE             PIC XX.
00109          16  ELCERT-ACCOUNT           PIC X(10).
00110          16  ELCERT-EFF-DT            PIC XX.
00111          16  ELCERT-CERT-NO.
00112              20  ELCERT-CERT-PRIME    PIC X(10).
00113              20  ELCERT-CERT-SFX      PIC X.
122612     12  ELEOBC-FILE-ID          PIC X(08)  VALUE 'ELEOBC'.
122612     12  ELEOBC-LENGTH           PIC S9(04) VALUE +350 COMP.
122612     12  ELEOBC-KEY.
122612         16  EOBC-COMPANY-CD     PIC X.
122612         16  EOBC-REC-TYPE       PIC X.
122612         16  EOBC-CODE           PIC X(4).
122612         16  FILLER              PIC X(9).
122612     12  ERNOTE-FILE-ID          PIC X(08)  VALUE 'ERNOTE'.
122612     12  ERNOTE-LENGTH           PIC S9(04) VALUE +825 COMP.
122612     12  ERNOTE-KEY.
122612         16  ERNOTE-COMPANY-CD   PIC X.
122612         16  ERNOTE-CARRIER      PIC X.
122612         16  ERNOTE-GROUPING     PIC X(6).
122612         16  ERNOTE-STATE        PIC XX.
122612         16  ERNOTE-ACCOUNT      PIC X(10).
122612         16  ERNOTE-CERT-EFF-DT  PIC XX.
122612         16  ERNOTE-CERT-PRIME   PIC X(10).
122612         16  ERNOTE-CERT-SFX     PIC X.
041320         16  ernote-record-type  pic x.
122612     12  NOTE-SUB PIC S9(5) COMP-3 VALUE +0.
100813     12  ELLETR-KEY.
100813         16  LETR-PART-KEY.
100813             20  LETR-COMPANY-CD PIC X.
100813             20  LETR-LETTER-ID  PIC X(4).
100813         16  LETR-FILLER         PIC X(8).
100813         16  LETR-SEQ-NO         PIC 9(4) BINARY.
100813     12  ELLETR-SAVE-PART-KEY    PIC X(5).
102918 01  w-comment-line-1            pic x(63) value spaces.
102918 01  w-comment-line-2            pic x(63) value spaces.
102918 01  cert-note-records-holder.
102918     05  cert-note-record occurs 500.
102918         10  filler              pic x(48).
102918         10  cnr-rest            pic x(102).
00114
00115
00116  01  FILLER                      PIC  X(22)
00117                                  VALUE 'INTERFACE AREA STARTS:'.
00118 *    COPY ELCINTF.
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
00119      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00120 *    COPY ELC1042.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                           ELC1042                              *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *    NOTE                                                        *
00007 *        THE WORK AREA IS USED BY EL152, EL1522, EL1042, EL153,  *
00008 *        EM152, EM1522, EL689, EL6892, EL6311, AND EL690.        *
00009 *        THIS COPYBOOK SHOULD NOT BE CHANGED WITHOUT REFERENCE   *
00010 *        TO THESE PROGRAMS.                                      *
00011 *                                                                *
00012 *    NOTE                                                        *
00013 *        THE FILLER AREA AT THE BOTTOM ARE FOR FUTURE EL1042     *
00014 *        USE ONLY!                                               *
00015 *                                                                *
00016 ******************************************************************
00017
00018          16  PI-1042-WA.
00019              20  PI-ACTION       PIC  X(01).
00020                  88 PI-SHOW-MODE           VALUE '1'.
00021                  88 PI-CLEAR-MODE          VALUE '2'.
00022                  88 PI-CREATE-MODE         VALUE '3'.
00023              20  PI-COMM-CONTROL PIC  X(12).
00024              20  PI-CURRENT-LINE PIC S9(03) COMP-3.
00025              20  PI-EOF-SW       PIC  X(01).
00026                  88  PI-FILE-EOF           VALUE 'Y'.
00027              20  PI-FILETYP      PIC  X(01).
00028              20  PI-FORM-SQUEEZE-CONTROL
00029                                  PIC  X(01).
00030                  88  PI-FORM-SQUEEZE-ON     VALUE 'Y'.
00031                  88  PI-FORM-SQUEEZE-OFF    VALUE ' '.
00032              20  PI-LAST-CONTROL PIC  X(12).
00033              20  PI-TEMP-STOR-ITEMS
00034                                  PIC S9(04) COMP.
00035              20  PI-TOTAL-LINES  PIC S9(03) COMP-3.
00036              20  PI-UPDATE-SW    PIC  9(01).
00037                  88 ANY-UPDATES            VALUE 1.
00038              20  PI-104-SCREEN-SENT-IND
00039                                  PIC  X(01).
00040                  88  PI-104-SCREEN-SENT    VALUE 'Y'.
00041                  88  PI-104-SCREEN-NOT-SENT VALUE 'N'.
00042              20  PI-1042-SCREEN-SENT-IND
00043                                  PIC  X(01).
00044                  88  PI-1042-SCREEN-SENT    VALUE 'Y'.
00045                  88  PI-1042-SCREEN-NOT-SENT VALUE 'N'.
00046              20  PI-1042-ARCHIVE-IND
00047                                  PIC  X(01).
00048                  88  PI-1042-ARCHIVE-LETTER VALUE 'Y'.
00049              20  FILLER          PIC  X(29).
00121 *    COPY ELC689PI.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELC689PI                            *
00004 *                            VMOD=2.003                          *
00005 *                                                                *
00006 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
00007 *    CREDIT CORRESPONDENCE SUB-SYSTEM.  ANY CHANGES WILL         *
00008 *    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
00009 *                                                                *
00010 *    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
00011 *    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
00012 *    BETWEEN PROGRAMS.                                           *
00013 *                                                                *
00014 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
00015 *                                                                *
00016 *               EL631 - EL689  - EL6891 - EL6892                 *
00017 *                                                                *
00018 ******************************************************************
081004*                   C H A N G E   L O G
081004*
081004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081004*-----------------------------------------------------------------
081004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081004* EFFECTIVE    NUMBER
081004*-----------------------------------------------------------------
081004* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
100705* 100705  CR2004072800004  PEMA  ADD LETTERS TO BE RESENT
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
081004******************************************************************
00019
00020
00021          16  PI-689-WORK-AREA.
00022              20  PI-689-ALT-PRINTER-ID
00023                                  PIC  X(04).
00024              20  PI-689-ARCHIVE-NUMBER
00025                                  PIC  9(08).
00026              20  PI-689-ARCHIVE-SW
00027                                  PIC  X(01).
00028                  88  PI-689-ARCHIVE-LETTER VALUE 'Y'.
00029              20  PI-689-DATA-SOURCE
00030                                  PIC  X(01).
00031                  88  PI-689-SRC-ACCOUNT        VALUE '1'.
00032                  88  PI-689-SRC-CERTIFICATE    VALUE '2'.
00033                  88  PI-689-SRC-COMPENSATION   VALUE '3'.
00034                  88  PI-689-SRC-PEND-BUSINESS  VALUE '4'.
00035                  88  PI-689-SRC-CHECKS         VALUE '5'.
00036              20  PI-689-ERROR-IND
00037                                  PIC  X(01).
00038                  88  PI-689-ERR-DETECTED-PREV  VALUE 'Y'.
00039              20  PI-689-ERROR    PIC  9(04).
00040                  88  PI-689-NO-ERRORS-DETECTED VALUE 0000.
00041                  88  PI-689-FATAL-ERROR
00042                      VALUES 0004 0006 0008 0013 0023 0029 0033
00043                             0042 0047 0051 0066 0067 0070
00044                             0168 0169 0174 0175 0176 0177 0179
00045                             0180 0181 0182 0184 0185 0189 0190
00046                             0191
00047                             0215 0279 0280
00048                             0412 0413 0454
00049                             0533 0537
00050                             2055 2114 2208 2209 2216 2232 2369
00051                             2398 2433 2908 2999
00052                             3000 3770 3771 3775
00053                             7250 7365 7367 7368 7369 7370 7371
00054                             7272 7373 7374 7376 7377 7378 7379
00055                             7381 7388 7390 7393 7395 7396 7398
00056                             9095 9096 9281 9298 9299 9320 9327
00057                             9426 9427.
00058                  88  PI-689-STOP-ERROR
00059                      VALUES 0004 0008 0013 0023 0029 0033
00060                             0042 0047 0066 0067 0070
00061                             0168 0169 0174 0175 0176 0177
00062                             0181 0182 0184 0185 0189 0190
00063                             0279 0280
00064                             0412 0413 0454
00065                             2055 2208 2209 2216 2232
00066                             2398 2999
00067                             3000 3770 3771 3775
00068                             7250 7365 7369 7370 7371
00069                             7272 7373 7374 7376 7377 7378 7379
00070                             7381 7388 7390 7393 7396 7398
00071                             9095 9096 9299 9320 9426.
00072              20  PI-689-FOLLOW-UP-DATE
00073                                  PIC  X(02).
00074              20  PI-689-FORM-NUMBER
00075                                  PIC  X(04).
00076              20  PI-689-LABEL-SOURCE
00077                                  PIC X(01).
00078                  88  PI-689-SOURCE-ACCOUNT  VALUE '1'.
00079                  88  PI-689-SOURCE-CARRIER  VALUE '2'.
00080                  88  PI-689-SOURCE-COMPANY  VALUE '3'.
00081                  88  PI-689-SOURCE-COMP     VALUE '4'.
00082                  88  PI-689-SOURCE-MAIL     VALUE '5'.
00083                  88  PI-689-SOURCE-CHECK    VALUE '6'.
00084                  88  PI-689-SOURCE-VARIABLE VALUE '7'.
00085              20  PI-689-NUMBER-COPIES
00086                                  PIC  9(01).
00087              20  PI-689-NUMBER-LABEL-LINES
00088                                  PIC  9(01).
00089              20  PI-689-NUMBER-TEXT-RECORDS
00090                                  PIC  9(03).
00091              20  PI-689-PRINT-ORDER-SW
00092                                  PIC  X(01).
00093                  88  PI-689-PRINT-FIRST     VALUE '1'.
00094                  88  PI-689-PRINT-SECOND    VALUE '2'.
00095                  88  PI-689-PRINT-LATER     VALUE '3'.
00096                  88  PI-689-PRINT-ONLY      VALUE '4'.
00097              20  PI-689-PRINT-RESTRICTION
00098                                  PIC  X(01).
00099                  88  PI-689-VALID-RESTRICT     VALUE 'C' 'F'.
00100                  88  PI-689-PRT-ONLY-WITH-CNTL VALUE 'C'.
00101                  88  PI-689-PRT-ONLY-WITH-FORM VALUE 'F'.
00102              20  PI-689-PRINT-SW PIC  X(01).
00103                  88  PI-689-PRINT-PERFORMED VALUE '1'.
00104              20  PI-689-RESEND-DATE-1
00105                                  PIC  X(02).
100705             20  PI-689-RESEND-LETR-1
                                       PIC X(4).
00110              20  PI-689-TEMP-STOR-ID
00111                                  PIC  X(08).
00112              20  PI-689-USE-SCREEN-IND
00113                                  PIC  X(01).
00114                  88  PI-689-CREATE-NO-SCREENS VALUE '1'.
00115              20  PI-689-ARCH-POINTER
00116                                  PIC S9(08) COMP.
00117                  88  PI-689-GET-ARCH-MAIN     VALUE +0.
00118              20  PI-689-ARCT-POINTER
00119                                  PIC S9(08) COMP.
00120                  88  PI-689-GET-ARCT-MAIN     VALUE +0.
00121              20  PI-689-VARIABLE-DATA-GRP.
00122                  24  PI-689-VARIABLE-DATA-1
00123                                  PIC  X(30).
00124                  24  PI-689-VARIABLE-DATA-2
00125                                  PIC  X(30).
00126                  24  PI-689-VARIABLE-DATA-3
00127                                  PIC  X(30).
00128                  24  PI-689-VARIABLE-DATA-4
00129                                  PIC  X(30).
00130
00131          16  PI-689-KEY-DATA-FIELDS.
00132              20  PI-689-ACCOUNT  PIC  X(10).
00133              20  PI-689-CARRIER  PIC  X(01).
00134              20  PI-689-CERT-NO.
00135                  24  PI-689-CERT-PRIME
00136                                  PIC  X(10).
00137                  24  PI-689-CERT-SFX
00138                                  PIC  X(01).
00139              20  PI-689-CHG-SEQ-NO
00140                                  PIC S9(04)    COMP.
00141              20  PI-689-CHG-SEQ-NOX REDEFINES PI-689-CHG-SEQ-NO
00142                                  PIC  X(02).
00143              20  PI-689-ENTRY-BATCH
00144                                  PIC  X(06).
00145              20  PI-689-EFF-DATE PIC  X(02).
00146              20  PI-689-EXP-DATE PIC  X(02).
00147              20  PI-689-GROUPING PIC  X(06).
00148              20  PI-689-RESP-PERSON
00149                                  PIC  X(10).
00150              20  PI-689-SEQ-NO   PIC S9(08)    COMP.
00151              20  PI-689-SEQ-NOX REDEFINES PI-689-SEQ-NO
00152                                  PIC  X(04).
00153              20  PI-689-STATE    PIC  X(02).
00154              20  PI-689-TYPE     PIC  X(01).
00155              20  PI-689-CONTROL  PIC S9(08)    COMP.
00156              20  PI-689-ALT-SEQ-NO
00157                                  PIC S9(04)    COMP.
00158          16  PI-689-DATE-EDIT    PIC  X(08).
00159          16  PI-689-FOLLOW-UP-EDIT
00160                                  PIC  X(08).
00161          16  PI-689-RESEND1-EDIT PIC  X(08).
00164          16  PI-689-SEQ-EDIT     PIC  X(08).
00165          16  PI-689-BCSEQ-EDIT   PIC  X(04).
00166          16  PI-689-LBL-OVERRIDE PIC  X(01).
00167              88  PI-689-LABELS-OVERRIDEN  VALUES 'N'.
081004         16  PI-689-FATAL-CTR    PIC 999     COMP-3.
081004         16  PI-689-FORCABLE-CTR PIC 999     COMP-3.
00122          16  PI-690-WORK-AREA.
00123              20  PI-690-ARCHIVE-TABLE.
00124                  24  PI-690-ARCHIVE-NUM OCCURS 12 TIMES
00125                                  PIC S9(08) COMP.
00126              20  PI-690-CURSOR   PIC S9(04) COMP.
00127              20  PI-690-FIRST-DATA.
00128                  24  PI-690-FIRST-CERT-NO.
00129                      28  PI-690-FIRST-CERT-PRIME
00130                                  PIC  X(10).
00131                      28  PI-690-FIRST-SUFFIX
00132                                  PIC  X(01).
00133                  24  PI-690-FIRST-CARRIER
00134                                  PIC  X(01).
00135                  24  PI-690-FIRST-GROUPING
00136                                  PIC  X(06).
00137                  24  PI-690-FIRST-STATE
00138                                  PIC  X(02).
00139                  24  PI-690-FIRST-ACCOUNT
00140                                  PIC  X(10).
00141                  24  PI-690-FIRST-EFFECT-DATE
00142                                  PIC  X(02).
00143                  24  PI-690-FIRST-ENTRY.
00144                      28  PI-690-FIRST-CONTROL-PREFIX
00145                                  PIC  X(02).
00146                      28  PI-690-FIRST-CONTROL
00147                                  PIC S9(08) COMP.
00148                  24  PI-690-FIRST-FORM
00149                                  PIC  X(04).
00150                  24  PI-690-FIRST-PROCESSOR
00151                                  PIC  X(04).
00152                  24  PI-690-FIRST-ARCHIVE-NO
00153                                  PIC S9(08) COMP.
00154              20  PI-690-INIT-DATA.
00155                  24  PI-690-INIT-CERT-NO.
00156                      28  PI-690-INIT-CERT-PRIME
00157                                  PIC  X(10).
00158                      28  PI-690-INIT-SUFFIX
00159                                  PIC  X(01).
00160                  24  PI-690-INIT-CARRIER
00161                                  PIC  X(01).
00162                  24  PI-690-INIT-GROUPING
00163                                  PIC  X(06).
00164                  24  PI-690-INIT-STATE
00165                                  PIC  X(02).
00166                  24  PI-690-INIT-ACCOUNT
00167                                  PIC  X(10).
00168                  24  PI-690-INIT-EFFECT-DATE
00169                                  PIC  X(02).
00170                  24  PI-690-INIT-EFF-DTE
00171                                  PIC  X(08).
00172                  24  PI-690-INIT-ENTRY.
00173                      28  PI-690-INIT-CONTROL-PREFIX
00174                                  PIC  X(02).
00175                      28  PI-690-INIT-CONTROL
00176                                  PIC S9(08) COMP.
00177                  24  PI-690-INIT-FORM
00178                                  PIC  X(04).
00179                  24  PI-690-INIT-PROCESSOR
00180                                  PIC  X(04).
00181                  24  PI-690-INIT-ARCHIVE-NO
00182                                  PIC S9(08) COMP.
00183              20  PI-690-LAST-DATA.
00184                  24  PI-690-LAST-CERT-NO.
00185                      28  PI-690-LAST-CERT-PRIME
00186                                  PIC  X(10).
00187                      28  PI-690-LAST-SUFFIX
00188                                  PIC  X(01).
00189                  24  PI-690-LAST-CARRIER
00190                                  PIC  X(01).
00191                  24  PI-690-LAST-GROUPING
00192                                  PIC  X(06).
00193                  24  PI-690-LAST-STATE
00194                                  PIC  X(02).
00195                  24  PI-690-LAST-ACCOUNT
00196                                  PIC  X(10).
00197                  24  PI-690-LAST-EFFECT-DATE
00198                                  PIC  X(02).
00199                  24  PI-690-LAST-ENTRY.
00200                      28  PI-690-LAST-CONTROL-PREFIX
00201                                  PIC  X(02).
00202                      28  PI-690-LAST-CONTROL
00203                                  PIC S9(08) COMP.
00204                  24  PI-690-LAST-FORM
00205                                  PIC  X(04).
00206                  24  PI-690-LAST-PROCESSOR
00207                                  PIC  X(04).
00208                  24  PI-690-LAST-ARCHIVE-NO
00209                                  PIC S9(08) COMP.
00210              20  PI-690-LAST-ARCH-NDX
00211                                  PIC S9(04) COMP.
00212              20  PI-690-BRWS-TYPE-IND
00213                                  PIC  9(01).
00214                  88  PI-690-BRWS-CERTRP               VALUE 1.
00215                  88  PI-690-BRWS-FORM                 VALUE 2.
00216                  88  PI-690-BRWS-PROCESSOR            VALUE 3.
00217                  88  PI-690-BRWS-ACCOUNT              VALUE 4.
00218                  88  PI-690-BRWS-ENTRY-CNTL           VALUE 5.
00219                  88  PI-690-BRWS-ARCHIVE              VALUE 6.
00220              20  PI-690-LAST-BROWSE-IND
00221                                  PIC  X(01).
00222                  88  PI-690-LAST-BRWS-FWRD            VALUE '1'.
00223                  88  PI-690-LAST-BRWS-BWRD            VALUE '2'.
00224              20  PI-690-STATUS-SELECTION-IND
00225                                  PIC  X(01).
00226                  88  PI-690-SELECT-ALL       VALUE 'N'.
00227                  88  PI-690-VALID-SELECTION  VALUE 'A' 'C' 'H'
00228                                                'X' 'P' 'V' 'N'.
00229          16  PI-ARCHIVE-COMPLETE    PIC X(01).
00230          16  PI-ARCHIVE-RECEIVED    PIC X(01).
00231          16  PI-ARCHIVE-STOPPED     PIC X(01).
00232          16  PI-ARCHIVE-FINAL       PIC X(01).
00233          16  PI-COMMENT-INDEX       PIC S9(4) COMP.
122612         16  PI-ARCHIVE-LTRID       PIC X(4).
102918         16  pi-create-date         pic x(08).
102918         16  pi-initial-print-date  pic xx.
102918         16  FILLER              PIC X(49).
00236
00237  01  W-CONSTANT-AREA.
00238      12  FILLER                  PIC  X(18)
00239                                  VALUE 'PROGRAM CONSTANTS:'.
00240      12  W-APPL-SCRTY-NDX        PIC S9(04)  COMP  VALUE +03.
00241
00242      12  W-ARCH-FILE-ID          PIC  X(08)  VALUE 'ERARCH'.
00243      12  W-ARCT-FILE-ID          PIC  X(08)  VALUE 'ERARCT'.
00244      12  W-ARCT-LENGTH           PIC S9(04)  COMP  VALUE +1640.
00245      12  W-LINK-001              PIC  X(08)  VALUE 'EL001'.
00246      12  W-LINK-004              PIC  X(08)  VALUE 'EL004'.
00247      12  W-MAP                   PIC  X(08)  VALUE 'EL691A'.
00248      12  W-MAP-REDEFINE  REDEFINES   W-MAP.
00249          16  FILLER              PIC  X(02).
00250          16  W-MAP-NUM           PIC  X(06).
00251      12  W-MAPSET                PIC  X(08)  VALUE 'EL691S'.
00252      12  W-THIS-PGM              PIC  X(08)  VALUE 'EL691'.
00253      12  W-TRANSACTION           PIC  X(04)  VALUE 'EXN1'.
00254      12  W-XCTL-005              PIC  X(08)  VALUE 'EL005'.
00255      12  W-XCTL-626              PIC  X(08)  VALUE 'EL626'.
121112     12  W-XCTL-1279             PIC  X(08)  VALUE 'EL1279'.
00256      12  SLASH                   PIC X       VALUE '/'.
00257      12  W-ZEROS                 PIC  S9(03) VALUE +0 COMP-3.
00258      12  W-ADD-ARCT              PIC  X      VALUE 'N'.
00259      12  W-DONE-ADDING           PIC  X      VALUE 'N'.
00260      12  W-NEED-COMMENT          PIC  X      VALUE 'N'.
00261
00262  01  ERROR-MESSAGES.
00263      12  ER-0000                 PIC  X(04) VALUE '0000'.
00264      12  ER-0004                 PIC  X(04) VALUE '0004'.
00265      12  ER-0029                 PIC  X(04) VALUE '0029'.
00266      12  ER-0051                 PIC  X(04) VALUE '0051'.
00267      12  ER-0070                 PIC  X(04) VALUE '0070'.
102918     12  ER-0249                 PIC  X(04) VALUE '0249'.
00268      12  ER-0295                 PIC  X(04) VALUE '0295'.
00269      12  ER-0296                 PIC  X(04) VALUE '0296'.
00270      12  ER-0539                 PIC  X(04) VALUE '0539'.
00271      12  ER-0895                 PIC  X(04) VALUE '0895'.
00272      12  ER-0897                 PIC  X(04) VALUE '0897'.
100813     12  ER-1236                 PIC  X(04) VALUE '1236'.
00273      12  ER-3112                 PIC  X(04) VALUE '3112'.
00274      12  ER-7008                 PIC  X(04) VALUE '7008'.
102918     12  ER-7330                 PIC  X(04) VALUE '7330'.
102918     12  ER-7331                 PIC  X(04) VALUE '7331'.
102918     12  ER-7332                 PIC  X(04) VALUE '7332'.
102918     12  ER-7333                 PIC  X(04) VALUE '7333'.
102918     12  ER-7334                 PIC  X(04) VALUE '7334'.
00275      12  ER-7363                 PIC  X(04) VALUE '7363'.
102918     12  ER-7364                 PIC  X(04) VALUE '7364'.
00276      12  ER-7371                 PIC  X(04) VALUE '7371'.
00277      12  ER-7373                 PIC  X(04) VALUE '7373'.
00278      12  ER-7388                 PIC  X(04) VALUE '7388'.
00279      12  ER-9097                 PIC  X(04) VALUE '9097'.
00280      12  ER-9245                 PIC  X(04) VALUE '9245'.
00282 *                                COPY ELCAID.
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
00283  01  FILLER    REDEFINES DFHAID.
00284      12  FILLER                  PIC  X(08).
00285      12  PF-VALUES               PIC  X(01) OCCURS 2.
00287 *                                COPY ELCATTR.
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
00289 *                                COPY ELCDATE.
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
100813*                                COPY ELCTEXT.
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
00291 *                                COPY ELCNWA.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNWA.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 *            M O V E   N A M E   W O R K   A R E A.             *
00008 *                                                               *
00009 *****************************************************************.
00010
00011  01  WS-NAME-WORK-AREA.
00012      05  WS-INSURED-LAST-NAME        PIC X(15).
00013      05  WS-INSURED-1ST-NAME         PIC X(12).
00014      05  WS-INSURED-MID-INIT         PIC X.
00015
00016      05  WS-NAME-WORK.
00017          10  WS-NW                   PIC X
00018              OCCURS 30 TIMES INDEXED BY NWA-INDEX.
00019
00020      05  WS-NAME-WORK2.
00021          10  WS-NW2                  PIC X
00022              OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
00023                                         NWA-INDEX0.
00024
00025      05  WS-NAME-SW                  PIC S9          VALUE ZERO
00026                                      COMP-3.
00027
00293 *                                COPY ELCEMIB.
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
      *                                COPY ERCCNOT.
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
00294  01  EMI-SAVE-AREA               PIC X(400).
00296 *                                COPY ELCLOGOF.
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
00298 *                                COPY ELCSCTM.
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
00300 *                                COPY ELCSCRTY.
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
00303 *                                COPY EL691S.
       01  EL691AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  COMPANYL PIC S9(0004) COMP.
           05  COMPANYF PIC  X(0001).
           05  FILLER REDEFINES COMPANYF.
               10  COMPANYA PIC  X(0001).
           05  COMPANYI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
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
           05  CERTL PIC S9(0004) COMP.
           05  CERTF PIC  X(0001).
           05  FILLER REDEFINES CERTF.
               10  CERTA PIC  X(0001).
           05  CERTI PIC  X(0010).
      *    -------------------------------
           05  SFXL PIC S9(0004) COMP.
           05  SFXF PIC  X(0001).
           05  FILLER REDEFINES SFXF.
               10  SFXA PIC  X(0001).
           05  SFXI PIC  X(0001).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
      *    -------------------------------
           05  MAINTDTL PIC S9(0004) COMP.
           05  MAINTDTF PIC  X(0001).
           05  FILLER REDEFINES MAINTDTF.
               10  MAINTDTA PIC  X(0001).
           05  MAINTDTI PIC  X(0008).
      *    -------------------------------
           05  MAINTTML PIC S9(0004) COMP.
           05  MAINTTMF PIC  X(0001).
           05  FILLER REDEFINES MAINTTMF.
               10  MAINTTMA PIC  X(0001).
           05  MAINTTMI PIC  X(0005).
      *    -------------------------------
           05  ARCHNOL PIC S9(0004) COMP.
           05  ARCHNOF PIC  X(0001).
           05  FILLER REDEFINES ARCHNOF.
               10  ARCHNOA PIC  X(0001).
           05  ARCHNOI PIC  X(0008).
      *    -------------------------------
           05  ENDARCHL PIC S9(0004) COMP.
           05  ENDARCHF PIC  X(0001).
           05  FILLER REDEFINES ENDARCHF.
               10  ENDARCHA PIC  X(0001).
           05  ENDARCHI PIC  X(0008).
      *    -------------------------------
           05  FORMNOL PIC S9(0004) COMP.
           05  FORMNOF PIC  X(0001).
           05  FILLER REDEFINES FORMNOF.
               10  FORMNOA PIC  X(0001).
           05  FORMNOI PIC  X(0004).
      *    -------------------------------
           05  RESFORML PIC S9(0004) COMP.
           05  RESFORMF PIC  X(0001).
           05  FILLER REDEFINES RESFORMF.
               10  RESFORMA PIC  X(0001).
           05  RESFORMI PIC  X(0004).
      *    -------------------------------
           05  CREATDTL PIC S9(0004) COMP.
           05  CREATDTF PIC  X(0001).
           05  FILLER REDEFINES CREATDTF.
               10  CREATDTA PIC  X(0001).
           05  CREATDTI PIC  X(0008).
      *    -------------------------------
           05  RESENDL PIC S9(0004) COMP.
           05  RESENDF PIC  X(0001).
           05  FILLER REDEFINES RESENDF.
               10  RESENDA PIC  X(0001).
           05  RESENDI PIC  X(0008).
      *    -------------------------------
           05  CREATBYL PIC S9(0004) COMP.
           05  CREATBYF PIC  X(0001).
           05  FILLER REDEFINES CREATBYF.
               10  CREATBYA PIC  X(0001).
           05  CREATBYI PIC  X(0004).
      *    -------------------------------
           05  PRINTDTL PIC S9(0004) COMP.
           05  PRINTDTF PIC  X(0001).
           05  FILLER REDEFINES PRINTDTF.
               10  PRINTDTA PIC  X(0001).
           05  PRINTDTI PIC  X(0008).
      *    -------------------------------
           05  RESPRNTL PIC S9(0004) COMP.
           05  RESPRNTF PIC  X(0001).
           05  FILLER REDEFINES RESPRNTF.
               10  RESPRNTA PIC  X(0001).
           05  RESPRNTI PIC  X(0008).
      *    -------------------------------
           05  STCOMPL PIC S9(0004) COMP.
           05  STCOMPF PIC  X(0001).
           05  FILLER REDEFINES STCOMPF.
               10  STCOMPA PIC  X(0001).
           05  STCOMPI PIC  X(0019).
      *    -------------------------------
           05  REPLYL PIC S9(0004) COMP.
           05  REPLYF PIC  X(0001).
           05  FILLER REDEFINES REPLYF.
               10  REPLYA PIC  X(0001).
           05  REPLYI PIC  X(0008).
      *    -------------------------------
           05  STRECVL PIC S9(0004) COMP.
           05  STRECVF PIC  X(0001).
           05  FILLER REDEFINES STRECVF.
               10  STRECVA PIC  X(0001).
           05  STRECVI PIC  X(0019).
      *    -------------------------------
           05  STOPDTEL PIC S9(0004) COMP.
           05  STOPDTEF PIC  X(0001).
           05  FILLER REDEFINES STOPDTEF.
               10  STOPDTEA PIC  X(0001).
           05  STOPDTEI PIC  X(0008).
      *    -------------------------------
           05  STSTOPL PIC S9(0004) COMP.
           05  STSTOPF PIC  X(0001).
           05  FILLER REDEFINES STSTOPF.
               10  STSTOPA PIC  X(0001).
           05  STSTOPI PIC  X(0019).
      *    -------------------------------
           05  OBVYNL PIC S9(0004) COMP.
           05  OBVYNF PIC  X(0001).
           05  FILLER REDEFINES OBVYNF.
               10  OBVYNA PIC  X(0001).
           05  OBVYNI PIC  X(0001).
      *    -------------------------------
           05  FINDATEL PIC S9(0004) COMP.
           05  FINDATEF PIC  X(0001).
           05  FILLER REDEFINES FINDATEF.
               10  FINDATEA PIC  X(0001).
           05  FINDATEI PIC  X(0008).
      *    -------------------------------
           05  STFINLL PIC S9(0004) COMP.
           05  STFINLF PIC  X(0001).
           05  FILLER REDEFINES STFINLF.
               10  STFINLA PIC  X(0001).
           05  STFINLI PIC  X(0019).
      *    -------------------------------
           05  OBUIDL PIC S9(0004) COMP.
           05  OBUIDF PIC  X(0001).
           05  FILLER REDEFINES OBUIDF.
               10  OBUIDA PIC  X(0001).
           05  OBUIDI PIC  X(0005).
      *    -------------------------------
           05  FINLACTL PIC S9(0004) COMP.
           05  FINLACTF PIC  X(0001).
           05  FILLER REDEFINES FINLACTF.
               10  FINLACTA PIC  X(0001).
           05  FINLACTI PIC  X(0001).
      *    -------------------------------
           05  COMMENTL PIC S9(0004) COMP.
           05  COMMENTF PIC  X(0001).
           05  FILLER REDEFINES COMMENTF.
               10  COMMENTA PIC  X(0001).
           05  COMMENTI PIC  X(0069).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0079).
       01  EL691AO REDEFINES EL691AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPANYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
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
           05  CERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTTMO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHNOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENDARCHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMNOO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESFORMO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREATDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESENDO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREATBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINTDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESPRNTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STCOMPO PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPLYO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STRECVO PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STOPDTEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STSTOPO PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OBVYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINDATEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STFINLO PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OBUIDO PIC  ZZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINLACTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMMENTO PIC  X(0069).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0079).
      *    -------------------------------
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
00306  01  DFHCOMMAREA                 PIC X(1024).
00308 *                                COPY ERCARCH.
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
00310 *                                COPY ERCARCT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT OF ARCHIVED LETTERDS                 *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 1640  RECFORM = FIXED                          *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCT                        RKP=2,LEN=8     *
00013 *                                                                *
00014 *   LOG = NO                                                     *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
070711*                   C H A N G E   L O G
070711*
070711* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070711*-----------------------------------------------------------------
070711*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070711* EFFECTIVE    NUMBER
070711*-----------------------------------------------------------------
070711* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
070711******************************************************************
00017  01  LETTER-ARCHIVE-TEXT.
00018      12  LT-RECORD-ID                PIC  X(02).
070711         88  LT-VALID-ID                VALUE 'LT'.
00020
00021      12  LT-CONTROL-PRIMARY.
00022          16  LT-COMPANY-CD           PIC  X(01).
00023          16  LT-ARCHIVE-NO           PIC S9(08)    COMP.
00024          16  LT-RECORD-TYPE          PIC  X(01).
00025              88  LT-ADDRESS-DATA        VALUE '1'.
00026              88  LT-TEXT-DATA           VALUE '2'.
070711             88  LT-COMMENT-DATA        VALUE '3'.
00027          16  LT-LINE-SEQ-NO          PIC S9(04)    COMP.
00028
00029      12  FILLER                      PIC  X(28).
00030      12  LT-NUM-LINES-ON-RECORD      PIC S9(04)    COMP.
00031
00032      12  LT-TEXT-RECORD.
00033          16  LT-LETTER-TEXT OCCURS 20 TIMES
00034                             INDEXED BY LT-NDX.
00035              20  LT-TEXT-LINE        PIC  X(70).
00036              20  LT-SKIP-CONTROL     PIC  X(02).
00037                  88  LT-NO-LINES-SKIPPED             VALUE SPACES.
00038                  88  LT-SKIP-TO-NEXT-PAGE            VALUE '99'.
00039              20  FILLER              PIC  X(08).
070711
070711     12  LT-COMMENT-RECORD  REDEFINES LT-TEXT-RECORD.
070711         16  LT-LETTER-COMMENT OCCURS 20 TIMES INDEXED BY LC-NDX.
070711             20  LT-COMMENT-LINE     PIC X(69).
070711             20  LT-COMMENT-CHG-DT   PIC X(02).
070711             20  LT-COMMENT-CHG-BY   PIC X(04).
070711             20  FILLER              PIC X(05).
00040
00314 *                                COPY ELCCERT.
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
122612*                                COPY ELCEOBC.
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
122612*                                COPY ERCNOTE.
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
102918 01  var                         pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA LETTER-ARCHIVE
                                LETTER-ARCHIVE-TEXT
                                CERTIFICATE-MASTER EOB-CODES
                                CERTIFICATE-NOTE VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL691' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00318      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00319
00320      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00321      MOVE '5'                    TO DC-OPTION-CODE.
00322      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
00323      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
00324      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE
00325                                     W-CURRENT-SAVE.
00326
00327      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00328      MOVE ERROR-MESSAGE-INTERFACE-BLOCK
00329                                  TO EMI-SAVE-AREA.
00330
00331      IF  EIBCALEN EQUAL 0
00332          MOVE UNACCESS-MSG       TO LOGOFF-MSG
00333          GO TO 8300-SEND-TEXT
00334      END-IF.
102918     set P to address of KIXSYS
102918     CALL "getenv" using by value P returning var-ptr
102918     if var-ptr = null then
102918        display ' kixsys not set '
102918     else
102918        set address of var to var-ptr
102918        move 0 to env-var-len
102918        inspect var tallying env-var-len
102918          for characters before X'00'
102918        unstring var (1:env-var-len) delimited by '/'
102918           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
102918              WS-KIX-SYS
102918        end-unstring
102918     end-if
00336      IF  PI-CALLING-PROGRAM NOT EQUAL W-THIS-PGM
00337          IF  PI-RETURN-TO-PROGRAM NOT EQUAL W-THIS-PGM
00338              MOVE PI-SAVED-PROGRAM-5
00339                                  TO PI-SAVED-PROGRAM-6
00340              MOVE PI-SAVED-PROGRAM-4
00341                                  TO PI-SAVED-PROGRAM-5
00342              MOVE PI-SAVED-PROGRAM-3
00343                                  TO PI-SAVED-PROGRAM-4
00344              MOVE PI-SAVED-PROGRAM-2
00345                                  TO PI-SAVED-PROGRAM-3
00346              MOVE PI-SAVED-PROGRAM-1
00347                                  TO PI-SAVED-PROGRAM-2
00348              MOVE PI-RETURN-TO-PROGRAM
00349                                  TO PI-SAVED-PROGRAM-1
00350              MOVE PI-CALLING-PROGRAM
00351                                  TO PI-RETURN-TO-PROGRAM
00352              MOVE W-THIS-PGM     TO PI-CALLING-PROGRAM
00353          ELSE
00354              MOVE PI-CALLING-PROGRAM TO W-CALL-PGM
00355              MOVE PI-RETURN-TO-PROGRAM
00356                                      TO PI-CALLING-PROGRAM
00357              MOVE PI-SAVED-PROGRAM-1 TO PI-RETURN-TO-PROGRAM
00358              MOVE PI-SAVED-PROGRAM-2 TO PI-SAVED-PROGRAM-1
00359              MOVE PI-SAVED-PROGRAM-3 TO PI-SAVED-PROGRAM-2
00360              MOVE PI-SAVED-PROGRAM-4 TO PI-SAVED-PROGRAM-3
00361              MOVE PI-SAVED-PROGRAM-5 TO PI-SAVED-PROGRAM-4
00362              MOVE PI-SAVED-PROGRAM-6 TO PI-SAVED-PROGRAM-5
00363              MOVE SPACES             TO PI-SAVED-PROGRAM-6
00364          END-IF
00365      ELSE
00366          GO TO 0200-RECEIVE
00367      END-IF.
00368
00369      GO TO 1000-SHOW.
00370
00371                                  EJECT
00372
00373  0200-RECEIVE.
00374
00375      
      * EXEC CICS HANDLE AID
00376 *        CLEAR    (9300-DFHCLEAR)
00377 *        PA1      (9200-PA)
00378 *        PA2      (9200-PA)
00379 *        PA3      (9200-PA)
00380 *    END-EXEC.
      *    MOVE '"&=!"#               V! " #00002756' TO DFHEIV0
           MOVE X'22263D212223202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032373536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00381
00382      
      * EXEC CICS HANDLE CONDITION
00383 *        PGMIDERR (9700-PGMID-ERROR)
00384 *        ERROR    (9800-ABEND)
00385 *    END-EXEC.
      *    MOVE '"$L.                  ! # #00002763' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032373633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00386
00387      
      * EXEC CICS RECEIVE
00388 *        MAP      (W-MAP)
00389 *        MAPSET   (W-MAPSET)
00390 *        INTO     (EL691AI)
00391 *    END-EXEC.
           MOVE LENGTH OF
            EL691AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002768' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL691AI, 
                 DFHEIV11, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00392
00393      IF  NOT DISPLAY-CAP
00394          MOVE 'READ'             TO SM-READ
00395          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00396          MOVE ER-9097            TO EMI-ERROR
00397          MOVE -1                 TO MAINTL
00398          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00399          GO TO 8100-SEND-INITIAL-MAP
00400      END-IF.
00401
00402
00403  0300-CHECK-PFKEYS.
00404
00405      IF  EIBAID EQUAL DFHPF23
00406          MOVE EIBAID             TO PI-ENTRY-CD-1
00407          MOVE W-XCTL-005         TO W-CALL-PGM
00408          GO TO 9400-XCTL
00409      END-IF.
00410
00411      IF  EIBAID EQUAL DFHPF24
00412          MOVE W-XCTL-626         TO W-CALL-PGM
00413          GO TO 9400-XCTL
00414      END-IF.
121112
121112     IF  EIBAID EQUAL DFHPF6
102020         move pi-689-chg-seq-nox(1:1)
102020                                 to PI-PROGRAM-WORK-AREA(1:1)
121112         MOVE W-XCTL-1279        TO W-CALL-PGM
121112         GO TO 9400-XCTL
121112     END-IF
00415
00416      IF  MAINTI EQUAL 'C'  AND
00417          EIBAID EQUAL DFHENTER
00418           GO TO 0700-PROCESS-CHANGES
00419      END-IF.
00420
00421      MOVE ER-0029                TO EMI-ERROR.
00422
00423
00424  0320-INPUT-ERROR.
00425
00426      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00427      MOVE -1                     TO  MAINTL.
00428      GO TO 8200-SEND-DATAONLY.
00429
00430
00431  0700-PROCESS-CHANGES.
00432
00433      IF  NOT MODIFY-CAP
00434          MOVE 'UPDATE'           TO SM-READ
00435          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00436          MOVE ER-0070            TO EMI-ERROR
00437          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00438          MOVE -1                 TO MAINTL
00439          GO TO 8100-SEND-INITIAL-MAP
00440      END-IF.
00441
100813     IF RESFORML > ZEROS
100813        AND RESFORMI > SPACES
100813        MOVE PI-COMPANY-CD       TO LETR-COMPANY-CD
100813        MOVE RESFORMI            TO LETR-LETTER-ID
100813        MOVE LETR-PART-KEY       TO ELLETR-SAVE-PART-KEY
100813        MOVE SPACES              TO LETR-FILLER
100813        MOVE 0                   TO LETR-SEQ-NO
100813        PERFORM 1500-CHECK-Z-RECORD THRU 1500-EXIT
100813        IF Z-RECORD-NOT-FOUND
100813           MOVE ER-1236          TO EMI-ERROR
100813           MOVE -1               TO RESFORML
100813           MOVE AL-UABON         TO RESFORMA
100813           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100813        END-IF
100813     END-IF
100813
00442      MOVE 'N'                    TO W-NEED-COMMENT.
00443
00444      IF RESENDL GREATER ZERO
00445          MOVE 'Y'                TO W-NEED-COMMENT
00446          IF RESENDI = SPACES
00447              MOVE AL-UANON       TO  RESENDA
00448              MOVE +1             TO  W-UPDATE-SW
00449              MOVE LOW-VALUES     TO  W-RESEND-DATE
00450          ELSE
00451              MOVE RESENDI       TO  W-DEEDIT-FIELD
00452              PERFORM 8600-DEEDIT THRU 8600-EXIT
00453              IF W-DEEDIT-FIELD-V0 IS NUMERIC
00454                  MOVE W-DEEDIT-FIELD-V0  TO  RESENDO
00455                  INSPECT RESENDI CONVERTING SPACES TO SLASH
00456                  MOVE '4'        TO  DC-OPTION-CODE
00457                  MOVE W-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
00458                  PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00459                  IF DC-ERROR-CODE NOT = SPACES
00460                      MOVE ER-0295         TO  EMI-ERROR
00461                      MOVE -1              TO  RESENDL
00462                      MOVE AL-UABON        TO  RESENDA
00463                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00464                  ELSE
00465                      MOVE AL-UANON       TO  RESENDA
00466                      MOVE +1             TO  W-UPDATE-SW
00467                      MOVE DC-BIN-DATE-1  TO  W-RESEND-DATE
00468                  END-IF
00469              ELSE
00470                  MOVE ER-0295    TO  EMI-ERROR
00471                  MOVE -1         TO  RESENDL
00472                  MOVE AL-UABON   TO  RESENDA
00473                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00474              END-IF
00475          END-IF
00476      END-IF.
00477
00478      IF FINDATEL GREATER ZERO
00479          IF FINDATEI = SPACES
00480              MOVE AL-UANON       TO  FINDATEA
00481              MOVE +1             TO  W-UPDATE-SW
00482              MOVE LOW-VALUES     TO  W-FINAL-ACT-DATE
00483              MOVE SPACES         TO  PI-ARCHIVE-FINAL
00484          ELSE
00485              MOVE 'Y'             TO  PI-ARCHIVE-FINAL
00486              MOVE FINDATEI        TO  W-DEEDIT-FIELD
00487              PERFORM 8600-DEEDIT THRU 8600-EXIT
00488              IF W-DEEDIT-FIELD-V0 IS NUMERIC
00489                  MOVE W-DEEDIT-FIELD-V0  TO  FINDATEO
00490                  INSPECT FINDATEI CONVERTING SPACES TO SLASH
00491                  MOVE '4'                 TO  DC-OPTION-CODE
00492                  MOVE W-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
00493                  PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00494                  IF DC-ERROR-CODE NOT = SPACES
00495                      MOVE ER-0296        TO  EMI-ERROR
00496                      MOVE -1             TO  FINDATEL
00497                      MOVE AL-UABON       TO  FINDATEA
00498                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00499                  ELSE
00500                      MOVE AL-UANON       TO  FINDATEA
00501                      MOVE +1             TO  W-UPDATE-SW
00502                      MOVE DC-BIN-DATE-1  TO  W-FINAL-ACT-DATE
00503                  END-IF
00504              ELSE
00505                  MOVE ER-0296    TO  EMI-ERROR
00506                  MOVE -1         TO  FINDATEL
00507                  MOVE AL-UABON   TO  FINDATEA
00508                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00509              END-IF
00510          END-IF
00511      END-IF.
00512
00513      IF REPLYL GREATER ZERO
00514          IF REPLYI = SPACES
00515              MOVE AL-UANON       TO  REPLYA
00516              MOVE +1             TO  W-UPDATE-SW
00517              MOVE LOW-VALUES     TO  W-RECEIVED-DATE
00518              MOVE SPACES         TO  PI-ARCHIVE-RECEIVED
122612                                     WS-RECEIVED-NOTE
122612             PERFORM 4500-UPDATE-BILL-NOTE THRU 4500-EXIT
00519          ELSE
00520              MOVE 'Y'            TO  PI-ARCHIVE-RECEIVED
00521              MOVE REPLYI         TO  W-DEEDIT-FIELD
00522              PERFORM 8600-DEEDIT THRU 8600-EXIT
00523              IF W-DEEDIT-FIELD-V0 IS NUMERIC
00524                  MOVE W-DEEDIT-FIELD-V0  TO  REPLYO
00525                  INSPECT REPLYI CONVERTING SPACES TO SLASH
00526                  MOVE '4'        TO  DC-OPTION-CODE
00527                  MOVE W-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
00528                  PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00529                  IF DC-ERROR-CODE NOT = SPACES
00530                      MOVE ER-9245        TO  EMI-ERROR
00531                      MOVE -1             TO  REPLYL
00532                      MOVE AL-UABON       TO  REPLYA
00533                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00534                  ELSE
00535                    IF DC-BIN-DATE-1 GREATER THAN W-SAVE-BIN-DATE
00536                        MOVE ER-0539      TO  EMI-ERROR
00537                        MOVE -1           TO  REPLYL
00538                        MOVE AL-UABON     TO  REPLYA
00539                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00540                    ELSE
00541                        MOVE AL-UANON     TO  REPLYA
00542                        MOVE +1           TO  W-UPDATE-SW
00543                        MOVE DC-BIN-DATE-1 TO W-RECEIVED-DATE
00544                        MOVE DC-GREG-DATE-1-EDIT TO
00545                                         W-CERT-NOTE-RECV-DT
122612                                        WS-BN-RECV-DATE
00546                    END-IF
00547                  END-IF
00548              ELSE
00549                  MOVE ER-9245    TO  EMI-ERROR
00550                  MOVE -1         TO  REPLYL
00551                  MOVE AL-UABON   TO  REPLYA
00552                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00553              END-IF
00554          END-IF
00555      END-IF.
00556
00557      IF STOPDTEL GREATER ZERO
00558          IF STOPDTEI = SPACES
00559              MOVE AL-UANON       TO  STOPDTEA
00560              MOVE +1             TO  W-UPDATE-SW
00561              MOVE LOW-VALUES     TO  W-STOP-LETTER-DATE
00562              MOVE SPACES         TO  PI-ARCHIVE-STOPPED
00563              MOVE AL-UANOF       TO  RESENDA
00564                                      RESFORMA
00565                                      FINDATEA
00566                                      FINLACTA
00567                                      REPLYA
00568                                      COMMENTA
00569          ELSE
00570              MOVE 'Y'            TO  PI-ARCHIVE-STOPPED
00571              MOVE 'Y'            TO  W-NEED-COMMENT
00572              MOVE STOPDTEI       TO  W-DEEDIT-FIELD
00573              PERFORM 8600-DEEDIT THRU 8600-EXIT
00574              IF W-DEEDIT-FIELD-V0 IS NUMERIC
00575                  MOVE W-DEEDIT-FIELD-V0  TO  STOPDTEO
00576                  INSPECT STOPDTEI CONVERTING SPACES TO SLASH
00577                  MOVE '4'        TO  DC-OPTION-CODE
00578                  MOVE W-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
00579                  PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00580                  IF DC-ERROR-CODE NOT = SPACES
00581                      MOVE ER-0897         TO  EMI-ERROR
00582                      MOVE -1              TO  STOPDTEL
00583                      MOVE AL-UABON        TO  STOPDTEA
00584                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00585                  ELSE
00586                    IF DC-BIN-DATE-1 GREATER THAN W-SAVE-BIN-DATE
00587                        MOVE ER-0895      TO  EMI-ERROR
00588                        MOVE -1           TO  STOPDTEL
00589                        MOVE AL-UABON     TO  STOPDTEA
00590                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00591                    ELSE
00592                      MOVE AL-UANON       TO  STOPDTEA
00593                      MOVE +1             TO  W-UPDATE-SW
00594                      MOVE DC-BIN-DATE-1  TO  W-STOP-LETTER-DATE
00595                    END-IF
00596                  END-IF
00597              ELSE
00598                  MOVE ER-0897    TO  EMI-ERROR
00599                  MOVE -1         TO  STOPDTEL
00600                  MOVE AL-UABON   TO  STOPDTEA
00601                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00602              END-IF
00603          END-IF
00604      END-IF.
00605
00606      IF W-NEED-COMMENT = 'Y'  AND
00607         COMMENTL NOT > ZERO
00608            MOVE -1         TO  COMMENTL
00609            MOVE ER-7363    TO  EMI-ERROR
00610            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00611      END-IF.
102918     if (stopdtel > zeros)
102918        and (PI-ARCHIVE-STOPPED = 'Y')
102918        and (pi-initial-print-date <> low-values)
102918        if obvyni not = 'Y' AND 'N'
102918           move -1               to obvynl
102918           move al-uabon         to obvyna
102918           move er-7364          to emi-error
102918           perform 9900-error-format
102918                                 thru 9900-exit
102918        end-if
102918     end-if
102918     if obvynl > zeros
102918        and pi-archive-stopped <> 'Y'
102918        move -1                  to stopdtel
102918        move al-uabon            to stopdtea
102918        move er-7333             to emi-error
102918        perform 9900-error-format
102918                                 thru 9900-exit
102918     end-if
00613      IF COMMENTL GREATER ZERO
00614       OR  RESFORML GREATER ZERO
00615       OR  FINLACTL GREATER ZERO
102918      or obvynl > 0
00616          MOVE +1          TO  W-UPDATE-SW
00617      END-IF.
00618
00619      IF W-ERROR-COUNT GREATER ZERO
00620          GO TO 8200-SEND-DATAONLY
00621      END-IF.
00622
00623      IF W-UPDATE-SW NOT GREATER ZERO
00624          MOVE ER-3112    TO  EMI-ERROR
00625          MOVE -1         TO  MAINTL
00626          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00627          GO TO 8200-SEND-DATAONLY
00628      END-IF.
00629
00630      IF PI-ARCHIVE-STOPPED = 'Y'
00631         MOVE AL-SANOF       TO RESENDA
00632                                RESFORMA
00633                                FINDATEA
00634                                FINLACTA
00635                                REPLYA
102918*                              COMMENTA
00637      END-IF.
00638
00639      PERFORM 3000-READ-FOR-UPDATE THRU 3000-EXIT.
00640
00641      IF RESENDL GREATER ZERO
00642          MOVE W-RESEND-DATE      TO  LA-RESEND-DATE
00643      END-IF.
00644
00645      IF FINDATEL GREATER ZERO
00646          MOVE W-FINAL-ACT-DATE   TO  LA-FINAL-ACT-DATE
00647      END-IF.
00649      IF REPLYL GREATER ZERO  *> its really received date
00650         MOVE W-RECEIVED-DATE     TO LA-REPLY-DATE
00651         IF W-RECEIVED-DATE <> LOW-VALUES
102918           move w-cert-note-msg  to w-comment-line-1
102918           move +1               to w-comment-line-cnt
00652            PERFORM 4000-ADD-CERT-NOTE
                                       THRU 4099-EXIT
122612           PERFORM 4500-UPDATE-BILL-NOTE
                                       THRU 4500-EXIT
00653         END-IF
00654      END-IF
00656      IF RESFORML GREATER ZERO
00657         MOVE RESFORMI            TO  LA-RESEND-LETR
00658      END-IF.
00659
00660      IF FINLACTL GREATER ZERO
00661         MOVE FINLACTI            TO  LA-FINAL-ACT-IND
00662      END-IF.
00663
00664      IF STOPDTEL GREATER ZERO
00665         MOVE W-STOP-LETTER-DATE  TO  LA-VOIDED-DATE
00666      END-IF.
102918     perform 1620-read-elcert    thru 1620-exit
102918
102918     IF (OBVYNL > ZERO)
102918        and (obvyni <> la-void-onbase-yn)
102918        if (obvyni = 'N')
102918           and (la-void-onbase-yn = spaces)
102918           continue
102918        else
102918           if la-initial-print-date <> low-values
102918              perform 1600-update-obkeywords
102918                                 thru 1600-exit
102918           end-if
102918        end-if
102918        MOVE OBVYNI              TO LA-VOID-ONBASE-YN
102918     end-if
102918
102918     if (not cert-found)
102918        or (sql-update-failed)
102918        or ((la-initial-print-date = low-values)
102918              and
102918            (la-void-onbase-yn = 'Y'))
102918        
      * exec cics unlock
102918*          dataset (W-ARCH-FILE-ID)
102918*       end-exec
      *    MOVE '&*                    #   #00003117' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102918        if connected-to-db
102918           move ' '              to ws-connect-sw
102918           display ' about to disconnect '
102918           exec sql
102918              disconnect
102918           end-exec
102918        end-if
102918     end-if
102918
102918     if not cert-found
102918        move er-0249             to emi-error
102918        move -1                  to maintl
102918        perform 9900-error-format thru 9900-exit
102918        go to 8200-send-dataonly
102918     end-if
102918
102918     if sql-update-failed
102918        move er-7331             to emi-error
102918        move -1                  to maintl
102918        perform 9900-error-format thru 9900-exit
102918        go to 8200-send-dataonly
102918     end-if
102918
102918     if (la-void-onbase-yn = 'Y')
102918        and (la-initial-print-date = low-values)
102918        move er-7334             to emi-error
102918        move -1                  to maintl
102918        perform 9900-error-format thru 9900-exit
102918        go to 8200-send-dataonly
102918     end-if
102918
102918     if sql-update-succeeded
102918        display ' about to commit work '
102918        exec sql
102918           commit work
102918        end-exec
102918     end-if
102918     if connected-to-db
102918        display ' about to disconnect all '
102918        exec sql
102918           disconnect
102918        end-exec
102918        move ' ' to ws-connect-sw
102918     end-if
00668      PERFORM 3100-REWRITE THRU 3100-EXIT.
00669
00670      IF COMMENTL NOT GREATER ZERO
00671
00672          MOVE ER-0000            TO  EMI-ERROR
00673          MOVE ' '                TO  MAINTO
00674          MOVE -1                 TO  MAINTL
00675          MOVE AL-UANOF           TO  MAINTA
00676          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00677          GO TO 1000-SHOW
00678      END-IF.
102918     if (commentl > zeros)
102918        and (commenti not = spaces)
102918        move commenti            to w-cert-note-comment
102918        perform 4160-check-comment thru 4160-exit
102918        PERFORM 4000-ADD-CERT-NOTE
102918                                 THRU 4099-EXIT
102918     end-if
00680      PERFORM 3200-READ-ARCT-FOR-UPDATE THRU 3200-EXIT.
00681
00682      IF PI-COMMENT-INDEX EQUAL +20
00683          MOVE ER-0051    TO  EMI-ERROR
00684          MOVE -1         TO  MAINTL
00685          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00686          GO TO 8200-SEND-DATAONLY
00687      END-IF
00688
00689      SET PI-COMMENT-INDEX UP BY +1.
00690      SET LC-NDX TO PI-COMMENT-INDEX.
00691      MOVE COMMENTI        TO  LT-COMMENT-LINE (LC-NDX)
00692      MOVE W-SAVE-BIN-DATE TO  LT-COMMENT-CHG-DT (LC-NDX).
00693      MOVE PI-PROCESSOR-ID TO  LT-COMMENT-CHG-BY (LC-NDX).
00694      ADD +1               TO  LT-NUM-LINES-ON-RECORD.
00695
00696
00697      IF W-ADD-ARCT = 'N'
00698          PERFORM 3300-REWRITE-ARCT THRU 3300-EXIT
00699      ELSE
00700          PERFORM 3350-INSERT-ARCT THRU 3350-EXIT
00701      END-IF.
00702
00703
00704      MOVE ER-0000                TO  EMI-ERROR.
00705      MOVE ' '                    TO  MAINTO.
00706      MOVE -1                     TO  MAINTL.
00707      MOVE AL-UANOF               TO  MAINTA.
00708      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00709      GO TO 1000-SHOW.
00710
00711                                  EJECT
00712
00713  1000-SHOW.
00714 ***************************************************************
00715 *     THIS ROUTINE WILL READ THE ARCHIVE FILE WITH THE        *
00716 *     ARCHIVE NUMBER SPECIFIED FROM THE PRIOR SCREEN.         *
00717 ***************************************************************
00718      MOVE LOW-VALUES              TO W-ARCT-KEY.
00719      MOVE PI-COMPANY-CD           TO W-ARCH-COMPANY-CD
00720                                      W-ARCT-COMPANY-CD.
00721      MOVE PI-689-ARCHIVE-NUMBER   TO W-ARCH-NUMBER
00722                                      W-ARCT-ARCHIVE-NO.
00723      MOVE '3'                     TO W-ARCT-RECORD-TYPE.
00724      MOVE +0                      TO W-ARCT-LINE-SEQ-NO.
00725
00726      
      * EXEC CICS HANDLE CONDITION
00727 *         NOTOPEN    (8010-ARCH-NOT-OPEN)
00728 *         NOTFND     (1070-ARCH-NOT-FOUND)
00729 *         ENDFILE    (1070-ARCH-NOT-FOUND)
00730 *    END-EXEC.
      *    MOVE '"$JI''                 ! $ #00003228' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033323238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00731
00732      
      * EXEC CICS READ
00733 *        DATASET (W-ARCH-FILE-ID)
00734 *        SET     (ADDRESS OF LETTER-ARCHIVE)
00735 *        RIDFLD  (W-ARCH-KEY)
00736 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003234' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00737
00738      MOVE LA-FORM-A3             TO FORMNOO.
122612     MOVE LA-FORM-A3             TO PI-ARCHIVE-LTRID
00739      MOVE LA-ARCHIVE-NO          TO ARCHNOO.
00740      MOVE LA-CARRIER-A2          TO CARRIERO.
00741      MOVE LA-GROUPING-A2         TO GROUPO.
00742      MOVE LA-STATE-A2            TO STATEO.
00743      MOVE LA-ACCOUNT-A2          TO ACCTO.
00744      MOVE LA-CERT-PRIME-A2       TO CERTO.
00745      MOVE LA-CERT-SUFFIX-A2      TO SFXO.
121112
121112     IF LA-ENDT-ARCH-NO-X EQUAL LOW-VALUES OR SPACES
121112          MOVE SPACES            TO ENDARCHO
121112     ELSE
121112          MOVE LA-ENDT-ARCH-NO   TO ENDARCHO
121112     END-IF
00746
00747      IF  LA-CREATION-DATE EQUAL LOW-VALUES OR SPACES
00748          MOVE SPACES             TO CREATDTI
102918                                    pi-create-date
00749      ELSE
00750          MOVE LA-CREATION-DATE   TO DC-BIN-DATE-1
00751          MOVE ' '                TO DC-OPTION-CODE
00752          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00753          MOVE DC-GREG-DATE-1-EDIT TO CREATDTI
102918                                     pi-create-date
00754      END-IF.
00755
00756      MOVE LA-PROCESSOR-CD        TO CREATBYI.
00757
00758      IF  LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES OR SPACES
00759          MOVE SPACES             TO PRINTDTI
102918         move low-values         to pi-initial-print-date
00760      ELSE
102918         move la-initial-print-date
102918                                 to pi-initial-print-date
00761          MOVE LA-INITIAL-PRINT-DATE TO DC-BIN-DATE-1
00762          MOVE ' '                TO DC-OPTION-CODE
00763          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00764          MOVE DC-GREG-DATE-1-EDIT TO PRINTDTI
00765      END-IF.
00766
00767      IF  LA-RESEND-DATE = LOW-VALUES OR SPACES
00768          MOVE SPACES             TO RESENDI
00769      ELSE
00770          MOVE LA-RESEND-DATE     TO DC-BIN-DATE-1
00771          MOVE ' '                TO DC-OPTION-CODE
00772          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00773          MOVE DC-GREG-DATE-1-EDIT TO RESENDI
00774      END-IF.
00775
00776      MOVE LA-RESEND-LETR         TO RESFORMI.
00777
00778      IF  LA-SENT-DATE = LOW-VALUES OR SPACES
00779          MOVE SPACES             TO RESPRNTI
00780          MOVE SPACES             TO PI-ARCHIVE-COMPLETE
00781      ELSE
00782          MOVE 'Y'                TO PI-ARCHIVE-COMPLETE
00783          MOVE LA-SENT-DATE     TO DC-BIN-DATE-1
00784          MOVE ' '                TO DC-OPTION-CODE
00785          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00786          MOVE DC-GREG-DATE-1-EDIT TO RESPRNTI
00787      END-IF.
00788
00789      IF  LA-FINAL-ACT-DATE EQUAL LOW-VALUES OR SPACES
00790          MOVE SPACES             TO FINDATEI
00791          MOVE SPACES             TO PI-ARCHIVE-FINAL
00792      ELSE
00793          MOVE 'Y'                TO PI-ARCHIVE-FINAL
00794          MOVE LA-FINAL-ACT-DATE  TO DC-BIN-DATE-1
00795          MOVE ' '                TO DC-OPTION-CODE
00796          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00797          MOVE DC-GREG-DATE-1-EDIT TO FINDATEI
00798      END-IF.
00799
00800      MOVE LA-FINAL-ACT-IND       TO FINLACTI.
00801
00802      IF  LA-REPLY-DATE = LOW-VALUES OR SPACES
00803          MOVE SPACES             TO REPLYI
00804          MOVE SPACES             TO PI-ARCHIVE-RECEIVED
00805      ELSE
00806          MOVE 'Y'                TO PI-ARCHIVE-RECEIVED
00807          MOVE LA-REPLY-DATE      TO DC-BIN-DATE-1
00808          MOVE ' '                TO DC-OPTION-CODE
00809          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00810          MOVE DC-GREG-DATE-1-EDIT TO REPLYI
00811      END-IF.
00812
00813      IF  LA-VOIDED-DATE = LOW-VALUES OR SPACES
00814          MOVE SPACES             TO STOPDTEI
00815          MOVE SPACES             TO PI-ARCHIVE-STOPPED
00816      ELSE
00817          MOVE 'Y'                TO PI-ARCHIVE-STOPPED
00818          MOVE LA-VOIDED-DATE     TO DC-BIN-DATE-1
00819          MOVE ' '                TO DC-OPTION-CODE
00820          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00821          MOVE DC-GREG-DATE-1-EDIT TO STOPDTEI
00822      END-IF.
102918     if LA-VOID-ONBASE-YN not = spaces
102918        move LA-VOID-ONBASE-YN   to obvyno
102918     end-if
102918     if la-onbase-unique-id numeric
102918        move la-onbase-unique-id to obuido
102918     end-if
00824      IF LA-LAST-MAINT-DATE = LOW-VALUES OR SPACES
00825          MOVE SPACES             TO MAINTDTI
00826      ELSE
00827          MOVE LA-LAST-MAINT-DATE TO DC-BIN-DATE-1
00828          MOVE ' '                TO DC-OPTION-CODE
00829          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00830          MOVE DC-GREG-DATE-1-EDIT TO MAINTDTI
00831      END-IF.
00832
00833      IF LA-LAST-MAINT-TIMEX = LOW-VALUES OR SPACES
00834          MOVE ZEROES             TO MAINTTMO
00835      ELSE
00836          MOVE LA-LAST-MAINT-TIME TO W-TIME-IN
00837          MOVE W-TIME-OUT         TO MAINTTMO
00838      END-IF.
00839
00840      MOVE LA-LAST-UPDATED-BY     TO MAINTBYI.
00841
00842      IF (LA-SENT-DATE NOT EQUAL LOW-VALUES AND SPACES)
00843        OR (LA-VOIDED-DATE  NOT EQUAL LOW-VALUES AND SPACES)
00844           MOVE AL-SANOF          TO RESENDA
00845                                     RESFORMA
00846                                     FINDATEA
00847                                     FINLACTA
00848                                     REPLYA
00849                                     COMMENTA
00850      END-IF.
00851
00852      IF LA-SENT-DATE NOT EQUAL LOW-VALUES AND SPACES
00853          MOVE AL-SANOF           TO STOPDTEA
00854      END-IF.
00855
00856      
      * EXEC CICS HANDLE CONDITION
00857 *         NOTOPEN    (8015-ARCT-NOT-OPEN)
00858 *         NOTFND     (1000-ARCT-NOT-FOUND)
00859 *         ENDFILE    (1000-ARCT-NOT-FOUND)
00860 *    END-EXEC.
      *    MOVE '"$JI''                 ! % #00003375' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033333735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00861
00862      
      * EXEC CICS READ
00863 *        DATASET  (W-ARCT-FILE-ID)
00864 *        SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
00865 *        RIDFLD   (W-ARCT-KEY)
00866 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003381' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE-TEXT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00867
00868      SET LC-NDX TO W-ZEROS.
00869      PERFORM 20 TIMES
00870          SET LC-NDX UP BY +1
00871          IF LT-COMMENT-LINE (LC-NDX) > SPACES
00872              MOVE LT-COMMENT-LINE (LC-NDX) TO COMMENTI
00873              SET PI-COMMENT-INDEX TO LC-NDX
00874          END-IF
00875      END-PERFORM.
00876
00877      MOVE -1                     TO MAINTL.
00878      GO TO 8100-SEND-INITIAL-MAP.
00879
00880  1000-ARCT-NOT-FOUND.
00881
00882      SET PI-COMMENT-INDEX        TO W-ZEROS.
00883      MOVE -1                     TO MAINTL.
00884      GO TO 8100-SEND-INITIAL-MAP.
00885
00886  1000-EXIT.
00887      EXIT.
00888
00889
00890  1070-ARCH-NOT-FOUND.
00891
00892      MOVE ER-7371                TO EMI-ERROR.
00893      MOVE -1                     TO ARCHNOL.
00894      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00895      GO TO 8100-SEND-INITIAL-MAP.
00896
100813
100813 1500-CHECK-Z-RECORD.
100813
100813     MOVE 'N' TO WS-Z-RECORD-IND
100813
100813     
      * EXEC CICS STARTBR
100813*         DATASET    ('ELLETR')
100813*         RIDFLD     (ELLETR-KEY)
100813*         GTEQ
100813*         RESP      (W-RESPONSE)
100813*    END-EXEC.
           MOVE 'ELLETR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00003421' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033343231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
100813     IF NOT RESP-NORMAL
100813         GO TO 1500-ENDBR
100813     END-IF.
100813
100813 1500-READNEXT.
100813
100813     
      * EXEC CICS READNEXT
100813*        DATASET   ('ELLETR')
100813*        INTO      (TEXT-FILES)
100813*        RIDFLD    (ELLETR-KEY)
100813*        RESP      (W-RESPONSE)
100813*    END-EXEC
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV12
           MOVE 'ELLETR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00003433' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303033343333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 TEXT-FILES, 
                 DFHEIV12, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100813     IF RESP-NORMAL
100813        IF TX-CONTROL-PRIMARY(1:5) NOT = ELLETR-SAVE-PART-KEY
100813           GO TO 1500-ENDBR
100813        END-IF
100813
100813        IF TX-LINE-SQUEEZE-CONTROL = 'Z'
100813           MOVE 'Y' TO WS-Z-RECORD-IND
100813           GO TO 1500-ENDBR
100813        ELSE
100813           GO TO 1500-READNEXT
100813        END-IF
100813     ELSE
100813        GO TO 1500-ENDBR
100813     END-IF.
100813
100813 1500-ENDBR.
100813
100813     
      * EXEC CICS ENDBR
100813*        DATASET     ('ELLETR')
100813*    END-EXEC.
           MOVE 'ELLETR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003456' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
100813
100813 1500-EXIT.
100813      EXIT.
102918 1600-update-obkeywords.
102918
102918     if not connected-to-db
102918        perform 1700-CONNECT-TO-DB
102918                                 thru 1700-exit
102918     end-if
102918
102918*    perform 1620-read-elcert    thru 1620-exit
102918*    if not cert-found
102918*       go to 1600-exit
102918*    end-if
102918
102918     if obvyni = 'N'
102918        if la-onbase-unique-id numeric
102918           and la-onbase-unique-id > 0
102918           perform 1610-void-void thru 1610-exit
102918           go to 1600-exit
102918        end-if
102918     end-if
102918
102918***  If I get here I assume they are voiding the letter
102918***  I am using a stored procedure so I can get back the
102918***  unique id from the insert and update the erarch
102918***  record with it.  May come in handy down the road
102918
102918     if obvyni = 'Y'
102918        and (la-onbase-unique-id not numeric
102918           or la-onbase-unique-id = zeros)
102918        continue
102918     else
102918        go to 1600-exit
102918     end-if
102918
102918     move 'CID Certs'            to db-doc-type
102918     move 'Status'               to db-key-word-type
102918     move 'V'                    to db-key-word-value
102918     move cm-state               to db-cert-state
102918     move cm-cert-prime          to db-cert-no
102918     move cm-lf-loan-expire-dt   to dc-bin-date-1
102918     if cm-ah-loan-expire-dt > cm-lf-loan-expire-dt
102918        move cm-ah-loan-expire-dt
102918                                 to dc-bin-date-1
102918     end-if
102918     move ' '                    to dc-option-code
102918     perform 9500-LINK-DATE-CONVERT thru 9500-exit
102918     if no-conversion-error
102918        move dc-greg-date-a-edit to db-cert-exp-dt
102918        inspect db-cert-exp-dt
102918           converting '/' to '-'
102918     end-if
102918     move la-initial-print-date  to dc-bin-date-1
102918     move ' '                    to dc-option-code
102918     perform 9500-LINK-DATE-CONVERT thru 9500-exit
102918     if no-conversion-error
102918        move dc-greg-date-a-edit to db-print-dt
102918        inspect db-print-dt
102918           converting '/' to '-'
102918     end-if
102918
102918     EXEC SQL
102918        CALL sp_OBKeyWordChgs_ADD
102918           @DocType       =  :db-doc-type,
102918           @CertNo        =  :db-cert-no,
102918           @PrintDt       =  :db-print-dt,
102918           @CertExpDt     =  :db-cert-exp-dt,
102918           @CertSt        =  :db-cert-state,
102918           @KeyWordType   =  :db-key-word-type,
102918           @KeyWordValue  =  :db-key-word-value,
102918           @RetValue      = :kd-unique-id OUT
102918     END-EXEC
102918
102918     if sqlcode not = 0
102918        move sqlcode             to ws-sql-code
102918        move ws-sql-code         to ws-dis-sql-code
102918        display ' Error: Cannot INSERT row ' kd-unique-id
102918        display ' sql return code        ' ws-dis-sql-code
102918        display ' sql err mess           ' sqlerrmc
102918        set sql-update-failed to true
102918     else
102918        move kd-unique-id        to la-onbase-unique-id
102918                                    obuido
102918        set sql-update-succeeded to true
102918     end-if
102918
102918     .
102918 1600-exit.
102918     exit.
102918
102918 1610-void-void.
102918
102918     move la-onbase-unique-id    to kd-unique-id
102918     EXEC SQL
102918        SELECT
102918           RequestDate,
102918           KeyWordType,
102918           KeyWordValue,
102918           CompletionDate
102918        INTO
102918           :db-req-date,
102918           :db-key-word-type,
102918           :db-key-word-value,
102918           :db-complete-dt  :nu-Complete-date
102918        FROM
102918           OBKeyWordChgs
102918        where
102918           :kd-unique-id = UniqueID
102918     END-EXEC
102918
102918     if sqlcode not = 0
102918        move sqlcode             to ws-sql-code
102918        move ws-sql-code         to ws-dis-sql-code
102918        display ' Error: Cannot read row ' kd-unique-id
102918        display ' sql return code        ' ws-dis-sql-code
102918        display ' sql err mess           ' sqlerrmc
102918        go to 1610-exit
102918     end-if
102918     if nu-complete-date = -1
102918        display ' complete date is null '
102918     end-if
102918     if nu-complete-date = -1  *> isnull
102918        and db-key-word-type = 'Status'
102918        and obvyni <> 'Y'
102918        and db-key-word-value = 'V'
102918
102918        EXEC SQL
102918           DELETE
102918              OBKeyWordChgs
102918           WHERE
102918              UniqueID = :kd-unique-id
102918        END-EXEC
102918
102918        if sqlcode = 0
102918           display ' delete succeeded '
102918           set sql-update-succeeded to true
102918           move zeros            to la-onbase-unique-id
102918        else
102918           display ' delete not successful ' sqlcode
102918           set sql-update-failed to true
102918        end-if
102918     end-if
102918
102918     .
102918 1610-exit.
102918     exit.
102918
102918 1620-read-elcert.
102918
102918     move la-company-cd          to elcert-key
102918     move la-carrier-a2          to elcert-carrier
102918     move la-grouping-a2         to elcert-grouping
102918     move la-state-a2            to elcert-state
102918     move la-account-a2          to elcert-account
102918     move la-effect-date-a2      to elcert-eff-dt
102918     move la-cert-no-a2          to elcert-cert-no
102918
102918     
      * exec cics read
102918*       dataset    ('ELCERT')
102918*       ridfld     (elcert-key)
102918*       SET        (ADDRESS OF CERTIFICATE-MASTER)
102918*       resp       (w-response)
102918*    end-exec
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00003617' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033363137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 elcert-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102918
102918     if resp-normal
102918        set cert-found to true
102918     end-if
102918
102918     .
102918 1620-exit.
102918     exit.
102918
102918 1700-CONNECT-TO-DB.
102918
063022     move 'TEST_Logic'           to svr
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     if ws-kix-myenv = 'cid1p'
063022        move 'PROD_Logic'        to svr
063022     end-if
102918
102918     string
102918         usr delimited space
102918         "." delimited size
102918         pass delimited space into usr-pass
102918     end-string
102918
102918     EXEC SQL
102918        CONNECT TO :svr USER :usr-pass
102918     END-EXEC
102918
102918     if sqlcode not = 0
102918        move sqlcode             to ws-sql-code
102918        move ws-sql-code         to ws-dis-sql-code
102918        display ' Error: Cannot Connect  ' kd-unique-id
102918        display ' sql return code        ' ws-dis-sql-code
102918        display ' sql err mess           ' sqlerrmc
102918        move er-7332             to emi-error
102918        perform 9900-error-format thru 9900-exit
102918        go to 8200-send-dataonly
102918     end-if
102918
102918     set connected-to-db to true
102918     display ' good connection '
102918
102918     .
102918 1700-EXIT.
102918     EXIT.
00898  3000-READ-FOR-UPDATE.
00899
00900      
      * EXEC CICS HANDLE CONDITION
00901 *         NOTOPEN    (8010-ARCH-NOT-OPEN)
00902 *         NOTFND     (1070-ARCH-NOT-FOUND)
00903 *         ENDFILE    (1070-ARCH-NOT-FOUND)
00904 *    END-EXEC.
      *    MOVE '"$JI''                 ! & #00003671' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033363731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00905
00906      MOVE PI-COMPANY-CD TO W-ARCH-COMPANY-CD
00907      MOVE PI-689-ARCHIVE-NUMBER TO W-ARCH-NUMBER
00908
00909      
      * EXEC CICS READ
00910 *        DATASET (W-ARCH-FILE-ID)
00911 *        SET     (ADDRESS OF LETTER-ARCHIVE)
00912 *        RIDFLD  (W-ARCH-KEY)
00913 *        UPDATE
00914 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003680' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00915
00916  3000-EXIT.
00917      EXIT.
00918
00919                                  EJECT
00920
00921  3100-REWRITE.
00922
00923      MOVE PI-PROCESSOR-ID        TO  LA-LAST-UPDATED-BY
00924                                      MAINTBYI.
00925      MOVE W-SAVE-BIN-DATE        TO  LA-LAST-MAINT-DATE.
00926      MOVE W-SAVE-DATE            TO  MAINTDTI.
00927
00928      MOVE EIBTIME                TO  LA-LAST-MAINT-TIME
00929                                      W-TIME-IN.
00930      MOVE W-TIME-OUT             TO  MAINTTMO.
00931
00932      
      * EXEC CICS REWRITE
00933 *        DATASET (W-ARCH-FILE-ID)
00934 *        FROM    (LETTER-ARCHIVE)
00935 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003703' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00936
00937  3100-EXIT.
00938      EXIT.
00939                                  EJECT
00940  3200-READ-ARCT-FOR-UPDATE.
00941
00942      
      * EXEC CICS HANDLE CONDITION
00943 *         NOTFND     (3200-NOT-FOUND)
00944 *         ENDFILE    (3200-NOT-FOUND)
00945 *    END-EXEC.
      *    MOVE '"$I''                  ! '' #00003713' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033373133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00946
00947      MOVE PI-COMPANY-CD TO W-ARCT-COMPANY-CD.
00948      MOVE PI-689-ARCHIVE-NUMBER TO W-ARCT-ARCHIVE-NO.
00949      MOVE '3'                   TO W-ARCT-RECORD-TYPE.
00950      MOVE +0                    TO W-ARCT-LINE-SEQ-NO.
00951
00952      
      * EXEC CICS READ
00953 *        DATASET  (W-ARCT-FILE-ID)
00954 *        SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
00955 *        RIDFLD   (W-ARCT-KEY)
00956 *        UPDATE
00957 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003723' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE-TEXT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00958
00959      MOVE 'N' TO W-ADD-ARCT.
00960      GO TO 3200-EXIT.
00961
00962  3200-NOT-FOUND.
00963
00964      
      * EXEC CICS GETMAIN
00965 *         SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
00966 *         LENGTH   (W-ARCT-LENGTH)
00967 *    END-EXEC.
      *    MOVE '," L                  $   #00003735' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-ARCT-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE-TEXT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00968
00969      MOVE 'Y'                    TO W-ADD-ARCT.
00970      MOVE LOW-VALUES             TO LETTER-ARCHIVE-TEXT.
00971      MOVE 'LT'                   TO LT-RECORD-ID.
00972      MOVE PI-COMPANY-CD          TO LT-COMPANY-CD.
00973      MOVE PI-689-ARCHIVE-NUMBER  TO LT-ARCHIVE-NO.
00974      MOVE '3'                    TO LT-RECORD-TYPE.
00975      MOVE +0                     TO LT-LINE-SEQ-NO.
00976      MOVE +0                     TO LT-NUM-LINES-ON-RECORD.
00977      SET PI-COMMENT-INDEX        TO W-ZEROS.
00978
00979  3200-EXIT.
00980      EXIT.
00981
00982  3300-REWRITE-ARCT.
00983
00984      
      * EXEC CICS REWRITE
00985 *        DATASET (W-ARCT-FILE-ID)
00986 *        FROM    (LETTER-ARCHIVE-TEXT)
00987 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE-TEXT
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003755' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 LETTER-ARCHIVE-TEXT, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00988
00989  3300-EXIT.
00990      EXIT.
00991                                  EJECT
00992
00993  3350-INSERT-ARCT.
00994      
      * EXEC CICS WRITE
00995 *         DATASET   (W-ARCT-FILE-ID)
00996 *         FROM      (LETTER-ARCHIVE-TEXT)
00997 *         RIDFLD    (LT-CONTROL-PRIMARY)
00998 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE-TEXT
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003765' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 LETTER-ARCHIVE-TEXT, 
                 DFHEIV11, 
                 LT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00999
01000  3350-EXIT.
01001      EXIT.
01002                                  EJECT
01003
01004
01005  4000-ADD-CERT-NOTE.
01006
01007      MOVE SPACES                 TO  ERCNOT-KEY
01008                                      ELCERT-KEY.
102918     move spaces                 to cert-note-records-holder
102918                                    ws-build-note-sw
102918                                    ws-ercnot-sw
102918     move +0                     to c1
01010      MOVE PI-COMPANY-CD          TO  ERCNOT-COMPANY-CD
01011                                      ELCERT-COMPANY-CD.
01012      MOVE PI-CARRIER             TO  ERCNOT-CARRIER
01013                                      ELCERT-CARRIER.
01014      MOVE PI-GROUPING            TO  ERCNOT-GROUPING
01015                                      ELCERT-GROUPING.
01016      MOVE PI-STATE               TO  ERCNOT-STATE
01017                                      ELCERT-STATE.
01018      MOVE PI-ACCOUNT             TO  ERCNOT-ACCOUNT
01019                                      ELCERT-ACCOUNT.
01020      MOVE PI-CERT-EFF-DT         TO  ERCNOT-EFF-DT
01021                                      ELCERT-EFF-DT.
01022      MOVE PI-CERT-PRIME          TO  ERCNOT-CERT-PRIME
01023                                      ELCERT-CERT-PRIME.
01024      MOVE PI-CERT-SFX            TO  ERCNOT-CERT-SFX
01025                                      ELCERT-CERT-SFX.
01026      MOVE '1'                    TO  ERCNOT-REC-TYP
01027      MOVE ZEROS                  TO  ERCNOT-SEQ.
01028      MOVE ERCNOT-KEY             TO  SV-PRIOR-KEY.
102918     
      * EXEC CICS STARTBR
102918*       DATASET    ('ERCNOT')
102918*       RIDFLD     (ercnot-key)
102918*       GTEQ
102918*       RESP       (W-RESPONSE)
102918*    END-EXEC
           MOVE 'ERCNOT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00003803' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ercnot-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102918
102918     IF RESP-NORMAL
102918        set ercnot-startbr to true
102918*       display ' resp normal startbr '
102918        perform until finished-with-notes
102918           
      * EXEC CICS READNEXT
102918*             DATASET    ('ERCNOT')
102918*             RIDFLD     (ercnot-key)
102918*             INTO       (cert-note-file)
102918*             resp       (w-response)
102918*          end-exec
           MOVE LENGTH OF
            cert-note-file
             TO DFHEIV12
           MOVE 'ERCNOT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00003814' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303033383134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 cert-note-file, 
                 DFHEIV12, 
                 ercnot-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102918           if (resp-normal)
102918              and (cz-control-primary (1:33) =
102918                 sv-prior-key (1:33))
102918              if cz-record-type = '1'
102918                 add +1 to c1
102918                 move cert-note-file
102918                                 to cert-note-record (c1)
102918              end-if
102918           else
102918              set finished-with-notes to true
102918           end-if
102918        end-perform
102918     end-if
102918
102918     if ercnot-startbr
102918*       display ' about to endbr ercnot '
102918        
      * exec cics endbr
102918*          dataset    ('ERCNOT')
102918*       end-exec
           MOVE 'ERCNOT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003836' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102918     end-if
102918     move c1                     to note-count
102918
102918     if c1 = +0
102918        perform 4100-add-note    thru 4100-exit
102918     else
102918        perform 4120-delete-cert-notes
102918                                 thru 4120-exit
102918        if resp-normal
102918           perform 4100-add-note thru 4100-exit
102918           if resp-normal
102918              perform 4140-put-back-cert-notes
102918                                 thru 4140-exit
102918              if resp-normal
102918                 display 'NOTE SUCCESSFULLY ADDED'
102918              else
102918                 display ' something wrong with put back '
102918              end-if
102918           else
102918              display ' something went wrong with adding note '
102918           end-if
102918        else
102918           display ' something went wrong with generic delete '
102918        end-if
102918     end-if
102918
102918     .
102918 4099-EXIT.
102918     EXIT.
102918
102918
102918 4100-add-note.
102918
102918***  Need to check how long the note is
102918
102918     if w-comment-line-cnt = +0
102918        go to 4100-exit
102918     end-if
102918
102918     move 'CZ'                to cert-note-file
102918     move sv-prior-key        to cz-control-primary
102918     move '1'                 to cz-record-type
102918     move +1                  to cz-note-sequence
102918     move w-save-bin-date     to cz-last-maint-dt
102918     move eibtime             to cz-last-maint-hhmmss
102918     move pi-processor-id     to cz-last-maint-user
102918     move w-comment-line-1    to cz-note
102918
102918     .
102918 4100-write.
102918
102918     
      * exec cics write
102918*       dataset   ('ERCNOT')
102918*       from      (cert-note-file)
102918*       ridfld    (cz-control-primary)
102918*       resp      (w-response)
102918*    end-exec
           MOVE LENGTH OF
            cert-note-file
             TO DFHEIV11
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003890' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033383930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 cert-note-file, 
                 DFHEIV11, 
                 cz-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102918     if not resp-normal
102918        display ' error-ercnot-write ' w-response ' '
102918           cz-control-primary (2:33)
102918        go to 4100-exit
102918     else
102918        display 'NOTE SUCCESSFULLY ADDED'
102918     end-if
102918
102918     if w-comment-line-cnt > +1
102918        move w-comment-line-2    to cz-note
102918        add +1 to cz-note-sequence
102918        move +0                  to w-comment-line-cnt
102918        go to 4100-write
102918     end-if
102918
102918     .
102918 4100-exit.
102918     exit.
102918
102918
102918
102918 4120-delete-cert-notes.
102918
102918     move sv-prior-key (1:33)    to ercnot-key
102918     move '1'                    to ercnot-rec-typ
102918     move +0                     to ercnot-seq
102918     
      * exec cics delete
102918*       dataset    ('ERCNOT')
102918*       keylength  (ws-cert-note-generic-key-len)
102918*       ridfld     (ercnot-key (1:34))
102918*       generic
102918*       resp       (w-response)
102918*    end-exec
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&(  RKG               &  N#00003922' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033393232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ercnot-key(1 : 34), 
                 ws-cert-note-generic-key-len, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102918
102918     .
102918 4120-exit.
102918     exit.
102918
102918 4140-put-back-cert-notes.
102918
102918     perform varying c1 from +1 by +1 until
102918        c1 > note-count
102918        move cert-note-record (c1)
102918                                 to cert-note-file
102918        add +2                   to cz-note-sequence
102918*       display ' about to write ' cz-control-primary (2:19) ' '
102918*          cz-control-primary (23:11) ' ' cz-note-sequence ' '
102918*           cz-record-type ' ' cz-note-information
102918        
      * exec cics write
102918*          dataset ('ERCNOT')
102918*          FROM    (cert-note-file)
102918*          ridfld  (cz-control-primary)
102918*          resp    (w-response)
102918*       end-exec
           MOVE LENGTH OF
            cert-note-file
             TO DFHEIV11
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003944' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033393434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 cert-note-file, 
                 DFHEIV11, 
                 cz-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102918        if not resp-normal
102918           display ' error-ercnot-write subsequ ' w-response ' '
102918              cz-control-primary (2:33)
102918           move +999             to c1
102918        end-if
102918     end-perform
102918
102918    .
102918 4140-exit.
102918     exit.
102918
102918 4160-check-comment.
102918
102918     string
102918        w-cert-note-comment ' ' delimited by '  '
102918        la-form-a3 ' ' delimited by size
102918        la-processor-cd ' ' delimited by size
102918        pi-create-date delimited by size
102918           into w-stop-letter-comment
102918     end-string
102918
102918     perform varying n1 from +126 by -1 until
102918        w-stop-letter-comment(n1:1) <> space
102918     end-perform
102918     if n1 < +64
102918        move +1                  to w-comment-line-cnt
102918        move w-stop-letter-comment
102918                                 to w-comment-line-1
102918        go to 4160-exit
102918     end-if
102918
102918     move +2                     to w-comment-line-cnt
102918
102918     perform varying n1 from n1 by -1 until
102918        ((w-stop-letter-comment(n1:1) = ' ')
102918            and
102918         (n1 < +64))
102918        or (n1 < +1)
102918     end-perform
102918
102918     if n1 < +1
102918        move w-stop-letter-comment(1:63)
102918                                 to w-comment-line-1
102918        move w-stop-letter-comment(64:63)
102918                                 to w-comment-line-2
102918     else
102918        move w-stop-letter-comment(1:n1)
102918                                 to w-comment-line-1
102918        move w-stop-letter-comment(n1 + 1:126 - n1)
102918                                 to w-comment-line-2
102918     end-if
102918
102918     .
102918 4160-exit.
102918     exit.
122612 4500-UPDATE-BILL-NOTE SECTION.
122612
122612     
      * EXEC CICS GETMAIN
122612*         SET      (ADDRESS OF EOB-CODES)
122612*         LENGTH   (ELEOBC-LENGTH)
122612*    END-EXEC
      *    MOVE '," L                  $   #00004007' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELEOBC-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF EOB-CODES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
122612
122612     MOVE LOW-VALUES             TO ELEOBC-KEY
122612     MOVE PI-COMPANY-CD          TO EOBC-COMPANY-CD
122612     MOVE '5'                    TO EOBC-REC-TYPE
122612
122612     
      * EXEC CICS STARTBR
122612*        DATASET   (ELEOBC-FILE-ID)
122612*        RIDFLD    (ELEOBC-KEY)
122612*        GTEQ
122612*        RESP      (W-RESPONSE)
122612*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004016' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303034303136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELEOBC-FILE-ID, 
                 ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
122612
122612     IF NOT RESP-NORMAL
122612        GO TO 4500-EXIT
122612     END-IF
122612      .
122612 4500-READNEXT-ELEOBC.
122612
122612     
      * EXEC CICS READNEXT
122612*       INTO    (EOB-CODES)
122612*       DATASET (ELEOBC-FILE-ID)
122612*       RIDFLD  (ELEOBC-KEY)
122612*       RESP    (W-RESPONSE)
122612*    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00004029' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303034303239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELEOBC-FILE-ID, 
                 EOB-CODES, 
                 DFHEIV12, 
                 ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
122612
122612     IF RESP-NORMAL
122612         IF EO-RECORD-TYPE NOT = '5'
122612             GO TO 4500-EXIT
122612         END-IF
122612     ELSE
122612         GO TO 4500-EXIT
122612     END-IF
122612
122612     IF EO-RECORD-TYPE = '5' AND
122612        EO-EOB-CODE = PI-ARCHIVE-LTRID
122612           CONTINUE
122612     ELSE
122612         GO TO 4500-READNEXT-ELEOBC
122612     END-IF
122612
122612     MOVE SPACES TO WS-FIND-BILLING-NOTE
122612     MOVE EO-DESCRIPTION TO WS-FBN-NOTE
122612     MOVE PI-ARCHIVE-LTRID TO WS-FBN-LTRID
122612
122612     
      * EXEC CICS GETMAIN
122612*         SET      (ADDRESS OF CERTIFICATE-NOTE)
122612*         LENGTH   (ERNOTE-LENGTH)
122612*    END-EXEC
      *    MOVE '," L                  $   #00004055' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERNOTE-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
122612
122612     MOVE PI-COMPANY-CD          TO  ERNOTE-COMPANY-CD
122612     MOVE PI-CARRIER             TO  ERNOTE-CARRIER
122612     MOVE PI-GROUPING            TO  ERNOTE-GROUPING
122612     MOVE PI-STATE               TO  ERNOTE-STATE
122612     MOVE PI-ACCOUNT             TO  ERNOTE-ACCOUNT
122612     MOVE PI-CERT-EFF-DT         TO  ERNOTE-CERT-EFF-DT
122612     MOVE PI-CERT-PRIME          TO  ERNOTE-CERT-PRIME
122612     MOVE PI-CERT-SFX            TO  ERNOTE-CERT-SFX
041320     move '1'                    to  ernote-record-type
122612
122612     
      * EXEC CICS READ
122612*       DATASET    (ERNOTE-FILE-ID)
122612*       RIDFLD     (ERNOTE-KEY)
122612*       INTO       (CERTIFICATE-NOTE)
122612*       RESP       (W-RESPONSE)
122612*       UPDATE
122612*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00004070' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034303730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 ERNOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
122612
122612     IF RESP-NORMAL
122612       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
122612           (NOTE-SUB > +10) OR
122612           (CN-LINE (NOTE-SUB) (1:29) =
122612                             WS-FIND-BILLING-NOTE (1:29))
122612       END-PERFORM
122612       IF CN-LINE (NOTE-SUB) (1:29) NOT =
122612                              WS-FIND-BILLING-NOTE (1:29)
122612         
      * EXEC CICS UNLOCK
122612*           DATASET    (ERNOTE-FILE-ID)
122612*        END-EXEC
      *    MOVE '&*                    #   #00004086' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
122612         GO TO 4500-EXIT
122612       END-IF
122612
122612       MOVE WS-RECEIVED-NOTE TO CN-LINE (NOTE-SUB) (50:20)
122612       MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
122612       MOVE W-SAVE-BIN-DATE     TO CN-LAST-MAINT-DT
122612       MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
122612       
      * EXEC CICS REWRITE
122612*         DATASET    (ERNOTE-FILE-ID)
122612*         FROM       (CERTIFICATE-NOTE)
122612*         RESP       (W-RESPONSE)
122612*      END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00004096' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303034303936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
122612
122612       .
122612
122612 4500-EXIT.
122612     EXIT.
122612
01096
01097  4800-CERTIFICATE-UPDATE SECTION.
01098
01099      
      * EXEC CICS HANDLE CONDITION
01100 *        NOTFND   (4899-EXIT)
01101 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00004110' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034313130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01102
01103      
      * EXEC CICS READ
01104 *    EQUAL
01105 *    DATASET   (ELCERT-FILE-ID)
01106 *    SET       (ADDRESS OF CERTIFICATE-MASTER)
01107 *    RIDFLD    (ELCERT-KEY)
01108 *    UPDATE
01109 *    RESP      (W-RESPONSE)
01110 *    END-EXEC.
      *    MOVE '&"S        EU         (  N#00004114' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034313134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01111
01112      IF RESP-NORMAL
01113         IF CM-NOTE-SW = ' '
01114            MOVE '1'        TO CM-NOTE-SW
01115         ELSE
01116            IF CM-NOTE-SW = '2'
01117               MOVE '3'      TO CM-NOTE-SW
01118            ELSE
01119               IF CM-NOTE-SW = '4'
01120                   MOVE '5'    TO CM-NOTE-SW
01121               ELSE
01122                  IF CM-NOTE-SW = '6'
01123                     MOVE '7'  TO CM-NOTE-SW
01124                  END-IF
01125               END-IF
01126            END-IF
01127         END-IF
01128         
      * EXEC CICS REWRITE
01129 *          FROM      (CERTIFICATE-MASTER)
01130 *          DATASET   (ELCERT-FILE-ID)
01131 *       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004139' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01132      END-IF.
01133
01134  4899-EXIT.
01135       EXIT.
01136
01137
01138
01139  8010-ARCH-NOT-OPEN.
01140
01141      MOVE ER-7388                TO EMI-ERROR.
01142      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01143      MOVE -1                     TO MAINTL.
01144      GO TO 8200-SEND-DATAONLY.
01145
01146  8015-ARCT-NOT-OPEN.
01147
01148      MOVE ER-7373                TO EMI-ERROR.
01149      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01150      MOVE -1                     TO MAINTL.
01151      GO TO 8200-SEND-DATAONLY.
01152                                  EJECT
01153
01154  8100-SEND-INITIAL-MAP.
01155
01156      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
01157
01158      IF  NOT EMI-NO-ERRORS
01159          MOVE EMI-MESSAGE-AREA (1)
01160                                  TO ERRMSG1O
01161      ELSE
01162          MOVE SPACES             TO ERRMSG1O
01163      END-IF.
01164
01165      
      * EXEC CICS SEND
01166 *        MAP    (W-MAP)
01167 *        MAPSET (W-MAPSET)
01168 *        FROM   (EL691AO)
01169 *        ERASE
01170 *        CURSOR
01171 *    END-EXEC.
           MOVE LENGTH OF
            EL691AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004176' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL691AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01172
01173      GO TO 9000-RETURN-TRANS.
01174
01175  8100-EXIT.
01176      EXIT.
01177                                  EJECT
01178
01179  8200-SEND-DATAONLY.
01180
01181      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
01182
01183      IF  NOT EMI-NO-ERRORS
01184          MOVE EMI-MESSAGE-AREA (1)
01185                                  TO ERRMSG1O
01186      ELSE
01187          MOVE SPACES             TO ERRMSG1O
01188      END-IF.
01189
01190      
      * EXEC CICS SEND
01191 *        MAP    (W-MAP)
01192 *        MAPSET (W-MAPSET)
01193 *        FROM   (EL691AO)
01194 *        DATAONLY
01195 *        CURSOR
01196 *    END-EXEC.
           MOVE LENGTH OF
            EL691AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004201' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL691AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01197
01198      GO TO 9000-RETURN-TRANS.
01199
01200  8200-EXIT.
01201      EXIT.
01202                                  EJECT
01203
01204  8300-SEND-TEXT.
01205
01206      
      * EXEC CICS SEND TEXT
01207 *        FROM    (LOGOFF-TEXT)
01208 *        LENGTH  (LOGOFF-LENGTH)
01209 *        ERASE
01210 *        FREEKB
01211 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004217' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323137' TO DFHEIV0(25:11)
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
           
01212
01213      GO TO 9000-RETURN-TRANS.
01214
01215  8300-EXIT.
01216      EXIT.
01217                                  EJECT
01218
01219  8600-DEEDIT.
01220
01221      
      * EXEC CICS BIF DEEDIT
01222 *         FIELD    (W-DEEDIT-FIELD)
01223 *         LENGTH   (15)
01224 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004232' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01225
01226  8600-EXIT.
01227      EXIT.
01228                                  EJECT
01229
01230  9000-RETURN-TRANS.
01231
01232      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO
01233      MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO
01234      
      * EXEC CICS RETURN
01235 *        TRANSID    (W-TRANSACTION)
01236 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01237 *        LENGTH     (PI-COMM-LENGTH)
01238 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00004245' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TRANSACTION, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01239
01240  9000-EXIT.
01241      EXIT.
01242                                  EJECT
01243
01244  9200-PA.
01245
01246      MOVE ER-7008                TO EMI-ERROR.
01247      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01248      MOVE -1                     TO MAINTL.
01249      GO TO 8200-SEND-DATAONLY.
01250
01251  9200-EXIT.
01252      EXIT.
01253                                  EJECT
01254
01255  9300-DFHCLEAR.
01256
01257      MOVE PI-RETURN-TO-PROGRAM TO W-CALL-PGM.
01258      GO TO 9400-XCTL.
01259
01260  9300-EXIT.
01261      EXIT.
01262
01263  9400-XCTL.
01264
01265      
      * EXEC CICS XCTL
01266 *        PROGRAM  (W-CALL-PGM)
01267 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01268 *        LENGTH   (PI-COMM-LENGTH)
01269 *    END-EXEC.
      *    MOVE '.$C                   %   #00004276' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01270
01271  9400-EXIT.
01272      EXIT.
01273                                  EJECT
01274
01275  9500-LINK-DATE-CONVERT.
01276
01277      
      * EXEC CICS LINK
01278 *        PROGRAM    ('ELDATCV')
01279 *        COMMAREA   (DATE-CONVERSION-DATA)
01280 *        LENGTH     (DC-COMM-LENGTH)
01281 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00004288' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01282
01283  9500-EXIT.
01284      EXIT.
01285
01286  9600-FORMAT-DATE-TIME.
01287
01288      MOVE W-SAVE-DATE            TO RUNDTEO.
01289      MOVE EIBTIME                TO W-TIME-IN.
01290      MOVE W-TIME-OUT             TO RUNTIMEO.
01291      MOVE PI-COMPANY-ID          TO COMPANYO.
01292      MOVE PI-PROCESSOR-ID        TO USERIDO.
01293      MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.
01294
01295      IF PI-ARCHIVE-COMPLETE = 'Y'
01296         MOVE AL-SANOF           TO STCOMPA
01297         MOVE AL-SADOF           TO STRECVA
01298                                    STSTOPA
01299                                    STFINLA
01300      ELSE
01301        IF PI-ARCHIVE-RECEIVED = 'Y'
01302            MOVE AL-SANOF        TO STRECVA
01303            MOVE AL-SADOF        TO STCOMPA
01304                                    STSTOPA
01305                                    STFINLA
01306        ELSE
01307          IF PI-ARCHIVE-STOPPED = 'Y'
01308              MOVE AL-SANOF      TO STSTOPA
01309              MOVE AL-SADOF      TO STCOMPA
01310                                    STRECVA
01311                                    STFINLA
01312          ELSE
01313            IF PI-ARCHIVE-FINAL = 'Y'
01314                MOVE AL-SANOF    TO STFINLA
01315                MOVE AL-SADOF    TO STCOMPA
01316                                    STRECVA
01317                                    STSTOPA
01318            ELSE
01319                MOVE AL-SADOF    TO STCOMPA
01320                                    STRECVA
01321                                    STSTOPA
01322                                    STFINLA
01323            END-IF
01324          END-IF
01325        END-IF
01326      END-IF.
01327
01328
01329  9600-EXIT.
01330      EXIT.
01331                                  EJECT
01332
01333  9700-PGMID-ERROR.
01334
01335      
      * EXEC CICS  HANDLE CONDITION
01336 *        PGMIDERR  (8300-SEND-TEXT)
01337 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00004346' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01338
01339      MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.
01340      MOVE ' '                    TO PI-ENTRY-CD-1.
01341      MOVE W-XCTL-005             TO W-CALL-PGM
01342                                     LOGOFF-PGM.
01343      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01344
01345      PERFORM 9400-XCTL THRU 9400-EXIT.
01346
01347  9700-EXIT.
01348      EXIT.
01349
01350  9800-ABEND.
01351
01352      MOVE W-LINK-004             TO W-CALL-PGM.
01353      MOVE DFHEIBLK               TO EMI-LINE1
01354
01355      
      * EXEC CICS  LINK
01356 *        PROGRAM   (W-CALL-PGM)
01357 *        COMMAREA  (EMI-LINE1)
01358 *        LENGTH    (72)
01359 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004366' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01360
01361      GO TO 8200-SEND-DATAONLY.
01362
01363  9800-EXIT.
01364      EXIT.
01365                                  EJECT
01366
01367  9900-ERROR-FORMAT.
01368
01369      IF  EMI-ERROR EQUAL ER-9097
01370          NEXT SENTENCE
01371      ELSE
01372          IF  EMI-ERRORS-COMPLETE
01373                  OR
01374              EMI-ERROR EQUAL W-LAST-ERROR
01375              GO TO 9900-EXIT
01376      END-IF.
01377
01378      ADD +1  TO  W-ERROR-COUNT.
01379      MOVE W-LINK-001             TO W-CALL-PGM.
01380
01381      
      * EXEC CICS LINK
01382 *        PROGRAM    (W-CALL-PGM)
01383 *        COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01384 *        LENGTH     (EMI-COMM-LENGTH)
01385 *    END-EXEC.
      *    MOVE '."C                   (   #00004392' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01386
01387      MOVE EMI-ERROR              TO W-LAST-ERROR.
01388
01389  9900-EXIT.
01390      EXIT.
01391
01392  9905-INITIALIZE-SECURITY.
01393 ******************************************************************
01394 *                                                                *
01395 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
01396 *       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *
01397 *       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *
01398 *       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *
01399 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
01400 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
01401 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
01402 *       ERROR CONDITION AND EXIT THE PROGRAM.                    *
01403 *                                                                *
01404 *       NOTE:  THE CARRIER/GRP/STATE/ACCOUNT SECURITY DATA       *
01405 *       IS ALSO PROVIDED BY THIS LOGIC.                          *
01406 *                                                                *
01407 ******************************************************************
01408
01409      IF  PI-PROCESSOR-ID EQUAL 'LGXX'
01410          MOVE 'Y'                TO PI-DISPLAY-CAP
01411                                     PI-MODIFY-CAP
01412      ELSE
01413          
      * EXEC CICS READQ TS
01414 *            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
01415 *            INTO   (SECURITY-CONTROL)
01416 *            LENGTH (SC-COMM-LENGTH)
01417 *            ITEM   (1)
01418 *        END-EXEC
           MOVE 1
             TO DFHEIV11
      *    MOVE '*$II   L              ''   #00004424' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01419          MOVE SC-CREDIT-DISPLAY (W-APPL-SCRTY-NDX)
01420                                  TO PI-DISPLAY-CAP
01421          MOVE SC-CREDIT-UPDATE (W-APPL-SCRTY-NDX)
01422                                  TO PI-MODIFY-CAP
01423      END-IF.
01424
01425  9905-EXIT.
01426                                  EJECT
01427
01428  9995-SECURITY-VIOLATION.
01429
01430      MOVE EIBDATE                TO SM-JUL-DATE.
01431      MOVE EIBTRMID               TO SM-TERMID.
01432      MOVE W-THIS-PGM             TO SM-PGM.
01433      MOVE EIBTIME                TO W-TIME-IN.
01434      MOVE W-TIME-OUT             TO SM-TIME.
01435      MOVE PI-PROCESSOR-ID        TO SM-PROCESSOR-ID.
01436
01437      
      * EXEC CICS LINK
01438 *         PROGRAM  ('EL003')
01439 *         COMMAREA (SECURITY-MESSAGE)
01440 *         LENGTH   (80)
01441 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00004448' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01442
01443  9995-EXIT.
01444      EXIT.
01445
01446  9999-GOBACK.
01447
01448      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL691' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01449
01450  9999-EXIT.
01451      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL691' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9300-DFHCLEAR,
                     9200-PA,
                     9200-PA,
                     9200-PA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9700-PGMID-ERROR,
                     9800-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8010-ARCH-NOT-OPEN,
                     1070-ARCH-NOT-FOUND,
                     1070-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8015-ARCT-NOT-OPEN,
                     1000-ARCT-NOT-FOUND,
                     1000-ARCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8010-ARCH-NOT-OPEN,
                     1070-ARCH-NOT-FOUND,
                     1070-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3200-NOT-FOUND,
                     3200-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4899-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL691' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
