00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL685 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 07/20/95 07:57:46.
00007 *                            VMOD=2.018
00008 *
00009 *AUTHOR.    LOGIC, INC.
00010 *           DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *    SKIP3
00025 *REMARKS.    TRANSACTION -  EXG1
00026
00027 *        THIS PROGRAM PRODUCES A REPORT SHOWING ALL CHECKS THAT
00028 *    ARE WAITING TO BE PRINTED.
00029
00030 *        THIS PROGRAM MAY BE ENTERED INTO THRU THE CREDIT OR
00031 *                   ACCOUNTS RECEIVABLE SYSTEMS.
00032
00033 *    SCREENS     - EL685A - CHECKS TO BE PRINTED
00034 *                  EL685B - A/R CHECKS TO BE PRINTED
00035 *                  EL850C - CHECKS PRINTED
00036 *                  EL685D - A/R CHECKS PRINTED
00037
00038 *    ENTERED BY  - EL671  - REPORT MENU
00039 *                    OR
00040 *                  EL850  - ACCOUNTS RECEIVABLE MENU
00041
00042 *    EXIT TO     - EL671  - RESULT OF CLEAR OR END OF JOB
00043 *                    OR
00044 *                  EL850  - RESULT OF CLEAR OR END OF JOB
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
030612******************************************************************
00045
00046
00047      EJECT
00048  ENVIRONMENT DIVISION.
00049
00050  DATA DIVISION.
00051
00052  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00053
00054  77  FILLER  PIC X(32)  VALUE '********************************'.
00055  77  FILLER  PIC X(32)  VALUE '*   EL685  WORKING STORAGE     *'.
00056  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.018 *********'.
00057
00058  01  FILLER                          COMP-3.
00059      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.
00060
00061      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.
00062      05  WS-TIME                     REDEFINES
00063          WS-TIME-WORK                PIC S9(3)V9(4).
00064
00065  01  FILLER                          COMP SYNC.
00066      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1920.
00067
00068  01  FILLER.
00069      05  CNTL-KEY.
00070          10  CNTL-CO                 PIC X(3).
00071          10  CNTL-RECORD-TYPE        PIC X           VALUE '1'.
00072          10  CNTL-GENL               PIC X(4).
00073          10  CNTL-SEQ                PIC S9(4) VALUE +0      COMP.
00074
00075      05  WS-START-CNTLNO             PIC S9(8)   VALUE +0    COMP.
00076
00077      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL685S'.
00078      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL685A'.
00079
00080      05  FILLER                      REDEFINES
00081          WS-MAP-NAME.
00082          20  FILLER                  PIC XX.
00083          20  WS-MAP-NUMBER           PIC X(6).
00084
00085      05  WS-PROGRAM-ID               PIC X(8)      VALUE 'EL685'.
00086
00087      05  WS-CHECK-QUEUE-DSID         PIC X(8) VALUE 'ERCHKQ'.
00088      05  WS-COMCK-QUEUE-DSID         PIC X(8) VALUE 'ERCMKQ'.
00089      05  WS-CONTROL-DSID             PIC X(8) VALUE 'ELCNTL'.
00090
00091      05  WS-TRANS-ID                 PIC X(4) VALUE 'EXG1'.
00092      05  WS-PRINT-TRAN-ID            PIC X(4) VALUE 'EXG9'.
00093      05  WS-PRINTER-ID               PIC X(4).
00094
00095      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70
00096                                      COMP
00097                                      SYNCHRONIZED.
00098
00099      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.
00100
00101      05  WS-SUCCESSFUL-MESSAGE       PIC X(79)       VALUE
00102          '0000 TRANSACTION SUCCESSFUL'.
00103
00104      05  WS-PAYEE-DESC               PIC X(08)       VALUE
00105          'PAYEE   '.
00106
00107      05  WS-PAYEE-SEQ-DESC           PIC X(07)       VALUE
00108          'SEQ    '.
00109
00110      05  WS-FINRESP-DESC             PIC X(08)       VALUE
00111          'FIN.RESP'.
00112
00113      05  WS-ACCOUNT-DESC             PIC X(07)       VALUE
00114          'ACCOUNT'.
00115
00116      05  WS-TO-BE-PRINTED-DESC       PIC X(28)       VALUE
00117          '-   CHECKS TO BE PRINTED   -'.
00118
00119      05  WS-CHECKS-PRINTED-DESC      PIC X(28)       VALUE
00120          '-      PRINTED CHECKS      -'.
00121
00122      05  WS-AR-TO-BE-PRINTED-DESC    PIC X(28)       VALUE
00123          '- A/R CHECKS TO BE PRINTED -'.
00124
00125      05  WS-AR-CHECKS-PRINTED-DESC   PIC X(28)       VALUE
00126          '-    A/R PRINTED CHECKS    -'.
00127
00128      05  WS-TO-BE-PRINTED-PFDESC     PIC X(29)       VALUE
00129          'PF4=LIST PRINTED CHECKS      '.
00130
00131      05  WS-CHECKS-PRINTED-PFDESC    PIC X(29)       VALUE
00132          'PF4=LIST TO BE PRINTED CHECKS'.
00133
00134      05  WS-SAVE-CHECK-MODE          PIC X           VALUE SPACE.
00135
00136      05  WS-SAVE-AR-MODE             PIC X           VALUE SPACE.
00137
00138      05  WS-TEMP-STORAGE-KEY.
00139          10  WS-TS-TERM-ID           PIC X(4).
00140          10  FILLER                  PIC X(4)        VALUE '685'.
00141
00142      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO
00143                                      COMP
00144                                      SYNCHRONIZED.
00145
00146  01  HOLD-CHECK-RECORD.
00147      12  HOLD-RECORD-ID              PIC XX.
00148          88  VALID-HOLD-ID                   VALUE 'MQ'.
00149
00150      12  HOLD-CONTROL-PRIMARY.
00151          16  HOLD-COMPANY-CD         PIC X.
00152          16  HOLD-CONTROL-NUMBER     PIC S9(8)   COMP VALUE ZEROS.
00153          16  HOLD-SEQUENCE-NUMBER    PIC S9(4)   COMP VALUE ZEROS.
00154
00155      12  HOLD-CONTROL-BY-PAYEE.
00156          16  HOLD-COMPANY-CD-A1      PIC X.
00157          16  HOLD-CSR-A1             PIC X(4).
00158          16  HOLD-CARRIER-A1         PIC X.
00159          16  HOLD-GROUPING-A1        PIC X(6).
00160          16  HOLD-PAYEE-A1           PIC X(10).
00161          16  HOLD-PAYEE-SEQ-A1       PIC S9(4)   COMP VALUE ZEROS.
00162          16  HOLD-CONTROL-NUMBER-A1  PIC S9(8)   COMP VALUE ZEROS.
00163          16  HOLD-SEQUENCE-NUMBER-A1 PIC S9(4)   COMP VALUE ZEROS.
00164
00165      12  HOLD-ENTRY-TYPE             PIC X.
00166
00167      12  FILLER                      PIC X(10).
00168
00169      12  HOLD-CREDIT-CHEK-CNTL.
00170          16  HOLD-CHEK-CSR           PIC X(4).
00171          16  HOLD-CHEK-CARRIER       PIC X.
00172          16  HOLD-CHEK-GROUPING      PIC X(6).
00173          16  HOLD-CHEK-PAYEE         PIC X(10).
00174          16  HOLD-CHEK-PAYEE-SEQ     PIC S9(4)   COMP VALUE ZEROS.
00175          16  HOLD-CHEK-SEQ-NO        PIC S9(4)   COMP VALUE ZEROS.
00176
00177      12  FILLER                      PIC X(10).
00178
00179      12  HOLD-PAYEE-INFO.
00180          16  HOLD-PAYEE-NAME         PIC X(30).
00181          16  HOLD-PAYEE-ADDRESS-1    PIC X(30).
00182          16  HOLD-PAYEE-ADDRESS-2    PIC X(30).
00183          16  HOLD-PAYEE-CITY-ST      PIC X(30).
00184          16  HOLD-PAYEE-ZIP-CODE.
00185              20  HOLD-PAYEE-ZIP.
00186                  24  FILLER          PIC X(1).
00187                  24  FILLER          PIC X(4).
00188              20  HOLD-PAYEE-ZIP-EXT  PIC X(4).
00189
00190      12  HOLD-CREDIT-PYAJ-CNTL.
00191          16  HOLD-PYAJ-CARRIER       PIC X.
00192          16  HOLD-PYAJ-GROUPING      PIC X(6).
00193          16  HOLD-PYAJ-FIN-RESP      PIC X(10).
00194          16  FILLER                  PIC X(6).
00195
00196      12  HOLD-CHECK-NUMBER           PIC X(6).
00197      12  HOLD-CHECK-AMOUNT           PIC S9(7)V99
00198                                                COMP-3 VALUE ZEROS.
00199      12  HOLD-NUMBER-OF-CK-STUBS     PIC S9(3)
00200                                                COMP-3 VALUE ZEROS.
00201      12  HOLD-VOID-DT                PIC XX.
00202      12  HOLD-TIMES-PRINTED          PIC S9(4)   COMP VALUE ZEROS.
00203      12  HOLD-PRINT-AT-HHMM          PIC S9(4)   COMP VALUE ZEROS.
00204      12  HOLD-CHECK-BY-USER          PIC X(4).
00205      12  HOLD-PRE-NUMBERING-SW       PIC X.
00206
00207      12  HOLD-CHECK-WRITTEN-DT       PIC XX.
00208      12  HOLD-LAST-MAINT-BY          PIC X(4).
00209      12  HOLD-LAST-MAINT-HHMMSS      PIC S9(6) COMP-3 VALUE ZEROS.
00210      12  HOLD-LAST-MAINT-DT          PIC XX.
00211      12  HOLD-CHECK-RELEASE-DT       PIC XX.
00212      12  HOLD-RECORD-TYPE            PIC X.
00213
00214      12  HOLD-DETAIL-INFORMATION.
00215          16  HOLD-DETAIL-INFO      OCCURS 15 TIMES.
00216              20  HOLD-CHECK-STUB-LINE.
00217                  24  HOLD-STUB-COMMENT      PIC X(23).
00218                  24  HOLD-ACCT-AGENT        PIC X(10).
00219                  24  HOLD-INVOICE           PIC X(6).
00220                  24  HOLD-REFERENCE         PIC X(12).
00221                  24  HOLD-LEDGER-NO         PIC X(14).
00222                  24  HOLD-PYAJ-AMT          PIC S9(7)V99
00223                                                COMP-3 VALUE ZEROS.
00224                  24  HOLD-PYAJ-REC-TYPE     PIC X.
00225                  24  HOLD-PYAJ-SEQ          PIC S9(8)
00226                                                COMP VALUE ZEROS.
00227                  24  HOLD-PAYMENT-TYPE      PIC X.
00228                  24  HOLD-PYAJ-PMT-APPLIED  PIC X.
00229                  24  HOLD-LAST-MAINT-APPLIED PIC X.
00230                  24  HOLD-NON-AR-ITEM       PIC X.
00231                  24  FILLER                 PIC X(19).
00232
00233      12  HOLD-CHECK-STUB-TEXT REDEFINES HOLD-DETAIL-INFORMATION.
00234          16  HOLD-CHECK-TEXT-ITEMS OCCURS 3 TIMES.
00235              20  HOLD-STUB-TEXT      PIC X(70).
00236          16  HOLD-STUB-FILLER        PIC X(1260).
00237
00238      12  HOLD-CREDIT-SELECT-DATE     PIC XX.
00239      12  HOLD-CREDIT-ACCEPT-DATE     PIC XX.
00240
00241      12  HOLD-AR-STATEMENT-DT        PIC XX.
00242      12  HOLD-CO-TYPE                PIC X.
00243
00244      12  HOLD-STARTING-CHECK-NUMBER  PIC X(06).
00245      12  FILLER                      PIC X(41).
00246 ******************************************************************
00247
00248      EJECT
00249 *                            COPY ELCINTF.
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
00250
00251      12  FILLER                      REDEFINES
00252          PI-PROGRAM-WORK-AREA.
00253          16  PI-CHECK-QUE-KEY.
00254              20  PI-CK-COMPANY-CODE  PIC X.
00255              20  PI-CK-CONTROL-NO    PIC S9(8)          COMP.
00256              20  PI-CK-SEQUENCE-NO   PIC S9(4)          COMP.
00257
00258          16  PI-PREV-CHECK-QUE-KEY.
00259              20  PI-PREV-CK-COMPANY-CODE     PIC X.
00260              20  PI-PREV-CK-CONTROL-NO       PIC S9(8) COMP.
00261              20  PI-PREV-CK-SEQUENCE-NO      PIC S9(4) COMP.
00262
00263
00264          16  PI-TEMP-STORAGE-ITEM    PIC S9(4)
00265                                      COMP
00266                                      SYNCHRONIZED.
00267
00268          16  PI-END-OF-FILE          PIC S9
00269                                      COMP-3.
00270
00271          16 PI-CONTROL-TOTALS.
00272             20 PI-CONTROL-TOT        PIC S9(7)V99 COMP-3.
00273             20 PI-CONTROL-GRAND-TOT  PIC S9(7)V99 COMP-3.
00274             20 PI-CONTROL-SAVE-CONTROL
00275                                      PIC S9(8) COMP.
00276          16 PI-FIRST-TIME-SW         PIC X.
00277             88 PI-FIRST-TIME         VALUE 'Y'.
00278             88 PI-NOT-FIRST-TIME     VALUE 'N'.
00279          16 PI-EOF-SWT               PIC X.
00280             88 PI-EOF                VALUE 'Y'.
00281             88 PI-NOT-EOF            VALUE 'N'.
00282          16 PI-SEND-TOT-SWT          PIC X.
00283             88 PI-SEND-TOT           VALUE 'Y'.
00284             88 PI-NOT-SEND-TOT       VALUE 'N'.
00285          16 PI-CHECK-MODE            PIC X.
00286             88  CHECKS-PRINTED       VALUE 'Y'.
00287             88  CHECKS-TO-BE-PRINTED VALUE SPACE.
00288          16 PI-CURRENT-DATE-BIN      PIC XX.
00289          16 PI-CURRENT-DATE          PIC X(8).
00290          16 PI-AR-MODE               PIC X.
00291             88 PI-CALLED-FROM-AR-MENU VALUE 'A'.
00292          16 PI-PREV-DISPLAY-SWT      PIC X.
00293             88 PI-TOTAL-SCREEN       VALUE 'T'.
00294          16 PI-START-CONTROL-NO      PIC S9(08)  COMP.
00295          16 PI-FIRST-TOTAL-SW        PIC X.
00296             88 PI-FIRST-TOTAL        VALUE 'Y'.
00297          16 PI-PAGING-SW             PIC X.
00298             88 PI-PAGE-FORWARD       VALUE 'Y'.
00299          16 FILLER                   PIC X(587).
00300      EJECT
00301 *            COPY EL685S.
       01  EL685BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  BTITLEL PIC S9(0004) COMP.
           05  BTITLEF PIC  X(0001).
           05  FILLER REDEFINES BTITLEF.
               10  BTITLEA PIC  X(0001).
           05  BTITLEI PIC  X(0028).
      *    -------------------------------
           05  BSCREENL PIC S9(0004) COMP.
           05  BSCREENF PIC  X(0001).
           05  FILLER REDEFINES BSCREENF.
               10  BSCREENA PIC  X(0001).
           05  BSCREENI PIC  X(0006).
      *    -------------------------------
           05  BPAGEL PIC S9(0004) COMP.
           05  BPAGEF PIC  X(0001).
           05  FILLER REDEFINES BPAGEF.
               10  BPAGEA PIC  X(0001).
           05  BPAGEI PIC  S9(4).
      *    -------------------------------
           05  BCTRL01L PIC S9(0004) COMP.
           05  BCTRL01F PIC  X(0001).
           05  FILLER REDEFINES BCTRL01F.
               10  BCTRL01A PIC  X(0001).
           05  BCTRL01I PIC  X(0007).
      *    -------------------------------
           05  BCKNO01L PIC S9(0004) COMP.
           05  BCKNO01F PIC  X(0001).
           05  FILLER REDEFINES BCKNO01F.
               10  BCKNO01A PIC  X(0001).
           05  BCKNO01I PIC  X(0006).
      *    -------------------------------
           05  BCKDT01L PIC S9(0004) COMP.
           05  BCKDT01F PIC  X(0001).
           05  FILLER REDEFINES BCKDT01F.
               10  BCKDT01A PIC  X(0001).
           05  BCKDT01I PIC  X(0008).
      *    -------------------------------
           05  BCARR01L PIC S9(0004) COMP.
           05  BCARR01F PIC  X(0001).
           05  FILLER REDEFINES BCARR01F.
               10  BCARR01A PIC  X(0001).
           05  BCARR01I PIC  X(0001).
      *    -------------------------------
           05  BGRUP01L PIC S9(0004) COMP.
           05  BGRUP01F PIC  X(0001).
           05  FILLER REDEFINES BGRUP01F.
               10  BGRUP01A PIC  X(0001).
           05  BGRUP01I PIC  X(0006).
      *    -------------------------------
           05  BPYEE01L PIC S9(0004) COMP.
           05  BPYEE01F PIC  X(0001).
           05  FILLER REDEFINES BPYEE01F.
               10  BPYEE01A PIC  X(0001).
           05  BPYEE01I PIC  X(0010).
      *    -------------------------------
           05  BSEQN01L PIC S9(0004) COMP.
           05  BSEQN01F PIC  X(0001).
           05  FILLER REDEFINES BSEQN01F.
               10  BSEQN01A PIC  X(0001).
           05  BSEQN01I PIC  X(0004).
      *    -------------------------------
           05  BPYNM01L PIC S9(0004) COMP.
           05  BPYNM01F PIC  X(0001).
           05  FILLER REDEFINES BPYNM01F.
               10  BPYNM01A PIC  X(0001).
           05  BPYNM01I PIC  X(0012).
      *    -------------------------------
           05  BPAMT01L PIC S9(0004) COMP.
           05  BPAMT01F PIC  X(0001).
           05  FILLER REDEFINES BPAMT01F.
               10  BPAMT01A PIC  X(0001).
           05  BPAMT01I PIC  X(0013).
      *    -------------------------------
           05  BCTRL02L PIC S9(0004) COMP.
           05  BCTRL02F PIC  X(0001).
           05  FILLER REDEFINES BCTRL02F.
               10  BCTRL02A PIC  X(0001).
           05  BCTRL02I PIC  X(0007).
      *    -------------------------------
           05  BCKNO02L PIC S9(0004) COMP.
           05  BCKNO02F PIC  X(0001).
           05  FILLER REDEFINES BCKNO02F.
               10  BCKNO02A PIC  X(0001).
           05  BCKNO02I PIC  X(0006).
      *    -------------------------------
           05  BCKDT02L PIC S9(0004) COMP.
           05  BCKDT02F PIC  X(0001).
           05  FILLER REDEFINES BCKDT02F.
               10  BCKDT02A PIC  X(0001).
           05  BCKDT02I PIC  X(0008).
      *    -------------------------------
           05  BCARR02L PIC S9(0004) COMP.
           05  BCARR02F PIC  X(0001).
           05  FILLER REDEFINES BCARR02F.
               10  BCARR02A PIC  X(0001).
           05  BCARR02I PIC  X(0001).
      *    -------------------------------
           05  BGRUP02L PIC S9(0004) COMP.
           05  BGRUP02F PIC  X(0001).
           05  FILLER REDEFINES BGRUP02F.
               10  BGRUP02A PIC  X(0001).
           05  BGRUP02I PIC  X(0006).
      *    -------------------------------
           05  BPYEE02L PIC S9(0004) COMP.
           05  BPYEE02F PIC  X(0001).
           05  FILLER REDEFINES BPYEE02F.
               10  BPYEE02A PIC  X(0001).
           05  BPYEE02I PIC  X(0010).
      *    -------------------------------
           05  BSEQN02L PIC S9(0004) COMP.
           05  BSEQN02F PIC  X(0001).
           05  FILLER REDEFINES BSEQN02F.
               10  BSEQN02A PIC  X(0001).
           05  BSEQN02I PIC  X(0004).
      *    -------------------------------
           05  BPYNM02L PIC S9(0004) COMP.
           05  BPYNM02F PIC  X(0001).
           05  FILLER REDEFINES BPYNM02F.
               10  BPYNM02A PIC  X(0001).
           05  BPYNM02I PIC  X(0012).
      *    -------------------------------
           05  BPAMT02L PIC S9(0004) COMP.
           05  BPAMT02F PIC  X(0001).
           05  FILLER REDEFINES BPAMT02F.
               10  BPAMT02A PIC  X(0001).
           05  BPAMT02I PIC  X(0013).
      *    -------------------------------
           05  BCTRL03L PIC S9(0004) COMP.
           05  BCTRL03F PIC  X(0001).
           05  FILLER REDEFINES BCTRL03F.
               10  BCTRL03A PIC  X(0001).
           05  BCTRL03I PIC  X(0007).
      *    -------------------------------
           05  BCKNO03L PIC S9(0004) COMP.
           05  BCKNO03F PIC  X(0001).
           05  FILLER REDEFINES BCKNO03F.
               10  BCKNO03A PIC  X(0001).
           05  BCKNO03I PIC  X(0006).
      *    -------------------------------
           05  BCKDT03L PIC S9(0004) COMP.
           05  BCKDT03F PIC  X(0001).
           05  FILLER REDEFINES BCKDT03F.
               10  BCKDT03A PIC  X(0001).
           05  BCKDT03I PIC  X(0008).
      *    -------------------------------
           05  BCARR03L PIC S9(0004) COMP.
           05  BCARR03F PIC  X(0001).
           05  FILLER REDEFINES BCARR03F.
               10  BCARR03A PIC  X(0001).
           05  BCARR03I PIC  X(0001).
      *    -------------------------------
           05  BGRUP03L PIC S9(0004) COMP.
           05  BGRUP03F PIC  X(0001).
           05  FILLER REDEFINES BGRUP03F.
               10  BGRUP03A PIC  X(0001).
           05  BGRUP03I PIC  X(0006).
      *    -------------------------------
           05  BPYEE03L PIC S9(0004) COMP.
           05  BPYEE03F PIC  X(0001).
           05  FILLER REDEFINES BPYEE03F.
               10  BPYEE03A PIC  X(0001).
           05  BPYEE03I PIC  X(0010).
      *    -------------------------------
           05  BSEQN03L PIC S9(0004) COMP.
           05  BSEQN03F PIC  X(0001).
           05  FILLER REDEFINES BSEQN03F.
               10  BSEQN03A PIC  X(0001).
           05  BSEQN03I PIC  X(0004).
      *    -------------------------------
           05  BPYNM03L PIC S9(0004) COMP.
           05  BPYNM03F PIC  X(0001).
           05  FILLER REDEFINES BPYNM03F.
               10  BPYNM03A PIC  X(0001).
           05  BPYNM03I PIC  X(0012).
      *    -------------------------------
           05  BPAMT03L PIC S9(0004) COMP.
           05  BPAMT03F PIC  X(0001).
           05  FILLER REDEFINES BPAMT03F.
               10  BPAMT03A PIC  X(0001).
           05  BPAMT03I PIC  X(0013).
      *    -------------------------------
           05  BCTRL04L PIC S9(0004) COMP.
           05  BCTRL04F PIC  X(0001).
           05  FILLER REDEFINES BCTRL04F.
               10  BCTRL04A PIC  X(0001).
           05  BCTRL04I PIC  X(0007).
      *    -------------------------------
           05  BCKNO04L PIC S9(0004) COMP.
           05  BCKNO04F PIC  X(0001).
           05  FILLER REDEFINES BCKNO04F.
               10  BCKNO04A PIC  X(0001).
           05  BCKNO04I PIC  X(0006).
      *    -------------------------------
           05  BCKDT04L PIC S9(0004) COMP.
           05  BCKDT04F PIC  X(0001).
           05  FILLER REDEFINES BCKDT04F.
               10  BCKDT04A PIC  X(0001).
           05  BCKDT04I PIC  X(0008).
      *    -------------------------------
           05  BCARR04L PIC S9(0004) COMP.
           05  BCARR04F PIC  X(0001).
           05  FILLER REDEFINES BCARR04F.
               10  BCARR04A PIC  X(0001).
           05  BCARR04I PIC  X(0001).
      *    -------------------------------
           05  BGRUP04L PIC S9(0004) COMP.
           05  BGRUP04F PIC  X(0001).
           05  FILLER REDEFINES BGRUP04F.
               10  BGRUP04A PIC  X(0001).
           05  BGRUP04I PIC  X(0006).
      *    -------------------------------
           05  BPYEE04L PIC S9(0004) COMP.
           05  BPYEE04F PIC  X(0001).
           05  FILLER REDEFINES BPYEE04F.
               10  BPYEE04A PIC  X(0001).
           05  BPYEE04I PIC  X(0010).
      *    -------------------------------
           05  BSEQN04L PIC S9(0004) COMP.
           05  BSEQN04F PIC  X(0001).
           05  FILLER REDEFINES BSEQN04F.
               10  BSEQN04A PIC  X(0001).
           05  BSEQN04I PIC  X(0004).
      *    -------------------------------
           05  BPYNM04L PIC S9(0004) COMP.
           05  BPYNM04F PIC  X(0001).
           05  FILLER REDEFINES BPYNM04F.
               10  BPYNM04A PIC  X(0001).
           05  BPYNM04I PIC  X(0012).
      *    -------------------------------
           05  BPAMT04L PIC S9(0004) COMP.
           05  BPAMT04F PIC  X(0001).
           05  FILLER REDEFINES BPAMT04F.
               10  BPAMT04A PIC  X(0001).
           05  BPAMT04I PIC  X(0013).
      *    -------------------------------
           05  BCTRL05L PIC S9(0004) COMP.
           05  BCTRL05F PIC  X(0001).
           05  FILLER REDEFINES BCTRL05F.
               10  BCTRL05A PIC  X(0001).
           05  BCTRL05I PIC  X(0007).
      *    -------------------------------
           05  BCKNO05L PIC S9(0004) COMP.
           05  BCKNO05F PIC  X(0001).
           05  FILLER REDEFINES BCKNO05F.
               10  BCKNO05A PIC  X(0001).
           05  BCKNO05I PIC  X(0006).
      *    -------------------------------
           05  BCKDT05L PIC S9(0004) COMP.
           05  BCKDT05F PIC  X(0001).
           05  FILLER REDEFINES BCKDT05F.
               10  BCKDT05A PIC  X(0001).
           05  BCKDT05I PIC  X(0008).
      *    -------------------------------
           05  BCARR05L PIC S9(0004) COMP.
           05  BCARR05F PIC  X(0001).
           05  FILLER REDEFINES BCARR05F.
               10  BCARR05A PIC  X(0001).
           05  BCARR05I PIC  X(0001).
      *    -------------------------------
           05  BGRUP05L PIC S9(0004) COMP.
           05  BGRUP05F PIC  X(0001).
           05  FILLER REDEFINES BGRUP05F.
               10  BGRUP05A PIC  X(0001).
           05  BGRUP05I PIC  X(0006).
      *    -------------------------------
           05  BPYEE05L PIC S9(0004) COMP.
           05  BPYEE05F PIC  X(0001).
           05  FILLER REDEFINES BPYEE05F.
               10  BPYEE05A PIC  X(0001).
           05  BPYEE05I PIC  X(0010).
      *    -------------------------------
           05  BSEQN05L PIC S9(0004) COMP.
           05  BSEQN05F PIC  X(0001).
           05  FILLER REDEFINES BSEQN05F.
               10  BSEQN05A PIC  X(0001).
           05  BSEQN05I PIC  X(0004).
      *    -------------------------------
           05  BPYNM05L PIC S9(0004) COMP.
           05  BPYNM05F PIC  X(0001).
           05  FILLER REDEFINES BPYNM05F.
               10  BPYNM05A PIC  X(0001).
           05  BPYNM05I PIC  X(0012).
      *    -------------------------------
           05  BPAMT05L PIC S9(0004) COMP.
           05  BPAMT05F PIC  X(0001).
           05  FILLER REDEFINES BPAMT05F.
               10  BPAMT05A PIC  X(0001).
           05  BPAMT05I PIC  X(0013).
      *    -------------------------------
           05  BCTRL06L PIC S9(0004) COMP.
           05  BCTRL06F PIC  X(0001).
           05  FILLER REDEFINES BCTRL06F.
               10  BCTRL06A PIC  X(0001).
           05  BCTRL06I PIC  X(0007).
      *    -------------------------------
           05  BCKNO06L PIC S9(0004) COMP.
           05  BCKNO06F PIC  X(0001).
           05  FILLER REDEFINES BCKNO06F.
               10  BCKNO06A PIC  X(0001).
           05  BCKNO06I PIC  X(0006).
      *    -------------------------------
           05  BCKDT06L PIC S9(0004) COMP.
           05  BCKDT06F PIC  X(0001).
           05  FILLER REDEFINES BCKDT06F.
               10  BCKDT06A PIC  X(0001).
           05  BCKDT06I PIC  X(0008).
      *    -------------------------------
           05  BCARR06L PIC S9(0004) COMP.
           05  BCARR06F PIC  X(0001).
           05  FILLER REDEFINES BCARR06F.
               10  BCARR06A PIC  X(0001).
           05  BCARR06I PIC  X(0001).
      *    -------------------------------
           05  BGRUP06L PIC S9(0004) COMP.
           05  BGRUP06F PIC  X(0001).
           05  FILLER REDEFINES BGRUP06F.
               10  BGRUP06A PIC  X(0001).
           05  BGRUP06I PIC  X(0006).
      *    -------------------------------
           05  BPYEE06L PIC S9(0004) COMP.
           05  BPYEE06F PIC  X(0001).
           05  FILLER REDEFINES BPYEE06F.
               10  BPYEE06A PIC  X(0001).
           05  BPYEE06I PIC  X(0010).
      *    -------------------------------
           05  BSEQN06L PIC S9(0004) COMP.
           05  BSEQN06F PIC  X(0001).
           05  FILLER REDEFINES BSEQN06F.
               10  BSEQN06A PIC  X(0001).
           05  BSEQN06I PIC  X(0004).
      *    -------------------------------
           05  BPYNM06L PIC S9(0004) COMP.
           05  BPYNM06F PIC  X(0001).
           05  FILLER REDEFINES BPYNM06F.
               10  BPYNM06A PIC  X(0001).
           05  BPYNM06I PIC  X(0012).
      *    -------------------------------
           05  BPAMT06L PIC S9(0004) COMP.
           05  BPAMT06F PIC  X(0001).
           05  FILLER REDEFINES BPAMT06F.
               10  BPAMT06A PIC  X(0001).
           05  BPAMT06I PIC  X(0013).
      *    -------------------------------
           05  BCTRL07L PIC S9(0004) COMP.
           05  BCTRL07F PIC  X(0001).
           05  FILLER REDEFINES BCTRL07F.
               10  BCTRL07A PIC  X(0001).
           05  BCTRL07I PIC  X(0007).
      *    -------------------------------
           05  BCKNO07L PIC S9(0004) COMP.
           05  BCKNO07F PIC  X(0001).
           05  FILLER REDEFINES BCKNO07F.
               10  BCKNO07A PIC  X(0001).
           05  BCKNO07I PIC  X(0006).
      *    -------------------------------
           05  BCKDT07L PIC S9(0004) COMP.
           05  BCKDT07F PIC  X(0001).
           05  FILLER REDEFINES BCKDT07F.
               10  BCKDT07A PIC  X(0001).
           05  BCKDT07I PIC  X(0008).
      *    -------------------------------
           05  BCARR07L PIC S9(0004) COMP.
           05  BCARR07F PIC  X(0001).
           05  FILLER REDEFINES BCARR07F.
               10  BCARR07A PIC  X(0001).
           05  BCARR07I PIC  X(0001).
      *    -------------------------------
           05  BGRUP07L PIC S9(0004) COMP.
           05  BGRUP07F PIC  X(0001).
           05  FILLER REDEFINES BGRUP07F.
               10  BGRUP07A PIC  X(0001).
           05  BGRUP07I PIC  X(0006).
      *    -------------------------------
           05  BPYEE07L PIC S9(0004) COMP.
           05  BPYEE07F PIC  X(0001).
           05  FILLER REDEFINES BPYEE07F.
               10  BPYEE07A PIC  X(0001).
           05  BPYEE07I PIC  X(0010).
      *    -------------------------------
           05  BSEQN07L PIC S9(0004) COMP.
           05  BSEQN07F PIC  X(0001).
           05  FILLER REDEFINES BSEQN07F.
               10  BSEQN07A PIC  X(0001).
           05  BSEQN07I PIC  X(0004).
      *    -------------------------------
           05  BPYNM07L PIC S9(0004) COMP.
           05  BPYNM07F PIC  X(0001).
           05  FILLER REDEFINES BPYNM07F.
               10  BPYNM07A PIC  X(0001).
           05  BPYNM07I PIC  X(0012).
      *    -------------------------------
           05  BPAMT07L PIC S9(0004) COMP.
           05  BPAMT07F PIC  X(0001).
           05  FILLER REDEFINES BPAMT07F.
               10  BPAMT07A PIC  X(0001).
           05  BPAMT07I PIC  X(0013).
      *    -------------------------------
           05  BCTRL08L PIC S9(0004) COMP.
           05  BCTRL08F PIC  X(0001).
           05  FILLER REDEFINES BCTRL08F.
               10  BCTRL08A PIC  X(0001).
           05  BCTRL08I PIC  X(0007).
      *    -------------------------------
           05  BCKNO08L PIC S9(0004) COMP.
           05  BCKNO08F PIC  X(0001).
           05  FILLER REDEFINES BCKNO08F.
               10  BCKNO08A PIC  X(0001).
           05  BCKNO08I PIC  X(0006).
      *    -------------------------------
           05  BCKDT08L PIC S9(0004) COMP.
           05  BCKDT08F PIC  X(0001).
           05  FILLER REDEFINES BCKDT08F.
               10  BCKDT08A PIC  X(0001).
           05  BCKDT08I PIC  X(0008).
      *    -------------------------------
           05  BCARR08L PIC S9(0004) COMP.
           05  BCARR08F PIC  X(0001).
           05  FILLER REDEFINES BCARR08F.
               10  BCARR08A PIC  X(0001).
           05  BCARR08I PIC  X(0001).
      *    -------------------------------
           05  BGRUP08L PIC S9(0004) COMP.
           05  BGRUP08F PIC  X(0001).
           05  FILLER REDEFINES BGRUP08F.
               10  BGRUP08A PIC  X(0001).
           05  BGRUP08I PIC  X(0006).
      *    -------------------------------
           05  BPYEE08L PIC S9(0004) COMP.
           05  BPYEE08F PIC  X(0001).
           05  FILLER REDEFINES BPYEE08F.
               10  BPYEE08A PIC  X(0001).
           05  BPYEE08I PIC  X(0010).
      *    -------------------------------
           05  BSEQN08L PIC S9(0004) COMP.
           05  BSEQN08F PIC  X(0001).
           05  FILLER REDEFINES BSEQN08F.
               10  BSEQN08A PIC  X(0001).
           05  BSEQN08I PIC  X(0004).
      *    -------------------------------
           05  BPYNM08L PIC S9(0004) COMP.
           05  BPYNM08F PIC  X(0001).
           05  FILLER REDEFINES BPYNM08F.
               10  BPYNM08A PIC  X(0001).
           05  BPYNM08I PIC  X(0012).
      *    -------------------------------
           05  BPAMT08L PIC S9(0004) COMP.
           05  BPAMT08F PIC  X(0001).
           05  FILLER REDEFINES BPAMT08F.
               10  BPAMT08A PIC  X(0001).
           05  BPAMT08I PIC  X(0013).
      *    -------------------------------
           05  BCTRL09L PIC S9(0004) COMP.
           05  BCTRL09F PIC  X(0001).
           05  FILLER REDEFINES BCTRL09F.
               10  BCTRL09A PIC  X(0001).
           05  BCTRL09I PIC  X(0007).
      *    -------------------------------
           05  BCKNO09L PIC S9(0004) COMP.
           05  BCKNO09F PIC  X(0001).
           05  FILLER REDEFINES BCKNO09F.
               10  BCKNO09A PIC  X(0001).
           05  BCKNO09I PIC  X(0006).
      *    -------------------------------
           05  BCKDT09L PIC S9(0004) COMP.
           05  BCKDT09F PIC  X(0001).
           05  FILLER REDEFINES BCKDT09F.
               10  BCKDT09A PIC  X(0001).
           05  BCKDT09I PIC  X(0008).
      *    -------------------------------
           05  BCARR09L PIC S9(0004) COMP.
           05  BCARR09F PIC  X(0001).
           05  FILLER REDEFINES BCARR09F.
               10  BCARR09A PIC  X(0001).
           05  BCARR09I PIC  X(0001).
      *    -------------------------------
           05  BGRUP09L PIC S9(0004) COMP.
           05  BGRUP09F PIC  X(0001).
           05  FILLER REDEFINES BGRUP09F.
               10  BGRUP09A PIC  X(0001).
           05  BGRUP09I PIC  X(0006).
      *    -------------------------------
           05  BPYEE09L PIC S9(0004) COMP.
           05  BPYEE09F PIC  X(0001).
           05  FILLER REDEFINES BPYEE09F.
               10  BPYEE09A PIC  X(0001).
           05  BPYEE09I PIC  X(0010).
      *    -------------------------------
           05  BSEQN09L PIC S9(0004) COMP.
           05  BSEQN09F PIC  X(0001).
           05  FILLER REDEFINES BSEQN09F.
               10  BSEQN09A PIC  X(0001).
           05  BSEQN09I PIC  X(0004).
      *    -------------------------------
           05  BPYNM09L PIC S9(0004) COMP.
           05  BPYNM09F PIC  X(0001).
           05  FILLER REDEFINES BPYNM09F.
               10  BPYNM09A PIC  X(0001).
           05  BPYNM09I PIC  X(0012).
      *    -------------------------------
           05  BPAMT09L PIC S9(0004) COMP.
           05  BPAMT09F PIC  X(0001).
           05  FILLER REDEFINES BPAMT09F.
               10  BPAMT09A PIC  X(0001).
           05  BPAMT09I PIC  X(0013).
      *    -------------------------------
           05  BCTRL10L PIC S9(0004) COMP.
           05  BCTRL10F PIC  X(0001).
           05  FILLER REDEFINES BCTRL10F.
               10  BCTRL10A PIC  X(0001).
           05  BCTRL10I PIC  X(0007).
      *    -------------------------------
           05  BCKNO10L PIC S9(0004) COMP.
           05  BCKNO10F PIC  X(0001).
           05  FILLER REDEFINES BCKNO10F.
               10  BCKNO10A PIC  X(0001).
           05  BCKNO10I PIC  X(0006).
      *    -------------------------------
           05  BCKDT10L PIC S9(0004) COMP.
           05  BCKDT10F PIC  X(0001).
           05  FILLER REDEFINES BCKDT10F.
               10  BCKDT10A PIC  X(0001).
           05  BCKDT10I PIC  X(0008).
      *    -------------------------------
           05  BCARR10L PIC S9(0004) COMP.
           05  BCARR10F PIC  X(0001).
           05  FILLER REDEFINES BCARR10F.
               10  BCARR10A PIC  X(0001).
           05  BCARR10I PIC  X(0001).
      *    -------------------------------
           05  BGRUP10L PIC S9(0004) COMP.
           05  BGRUP10F PIC  X(0001).
           05  FILLER REDEFINES BGRUP10F.
               10  BGRUP10A PIC  X(0001).
           05  BGRUP10I PIC  X(0006).
      *    -------------------------------
           05  BPYEE10L PIC S9(0004) COMP.
           05  BPYEE10F PIC  X(0001).
           05  FILLER REDEFINES BPYEE10F.
               10  BPYEE10A PIC  X(0001).
           05  BPYEE10I PIC  X(0010).
      *    -------------------------------
           05  BSEQN10L PIC S9(0004) COMP.
           05  BSEQN10F PIC  X(0001).
           05  FILLER REDEFINES BSEQN10F.
               10  BSEQN10A PIC  X(0001).
           05  BSEQN10I PIC  X(0004).
      *    -------------------------------
           05  BPYNM10L PIC S9(0004) COMP.
           05  BPYNM10F PIC  X(0001).
           05  FILLER REDEFINES BPYNM10F.
               10  BPYNM10A PIC  X(0001).
           05  BPYNM10I PIC  X(0012).
      *    -------------------------------
           05  BPAMT10L PIC S9(0004) COMP.
           05  BPAMT10F PIC  X(0001).
           05  FILLER REDEFINES BPAMT10F.
               10  BPAMT10A PIC  X(0001).
           05  BPAMT10I PIC  X(0013).
      *    -------------------------------
           05  BCTRL11L PIC S9(0004) COMP.
           05  BCTRL11F PIC  X(0001).
           05  FILLER REDEFINES BCTRL11F.
               10  BCTRL11A PIC  X(0001).
           05  BCTRL11I PIC  X(0007).
      *    -------------------------------
           05  BCKNO11L PIC S9(0004) COMP.
           05  BCKNO11F PIC  X(0001).
           05  FILLER REDEFINES BCKNO11F.
               10  BCKNO11A PIC  X(0001).
           05  BCKNO11I PIC  X(0006).
      *    -------------------------------
           05  BCKDT11L PIC S9(0004) COMP.
           05  BCKDT11F PIC  X(0001).
           05  FILLER REDEFINES BCKDT11F.
               10  BCKDT11A PIC  X(0001).
           05  BCKDT11I PIC  X(0008).
      *    -------------------------------
           05  BCARR11L PIC S9(0004) COMP.
           05  BCARR11F PIC  X(0001).
           05  FILLER REDEFINES BCARR11F.
               10  BCARR11A PIC  X(0001).
           05  BCARR11I PIC  X(0001).
      *    -------------------------------
           05  BGRUP11L PIC S9(0004) COMP.
           05  BGRUP11F PIC  X(0001).
           05  FILLER REDEFINES BGRUP11F.
               10  BGRUP11A PIC  X(0001).
           05  BGRUP11I PIC  X(0006).
      *    -------------------------------
           05  BPYEE11L PIC S9(0004) COMP.
           05  BPYEE11F PIC  X(0001).
           05  FILLER REDEFINES BPYEE11F.
               10  BPYEE11A PIC  X(0001).
           05  BPYEE11I PIC  X(0010).
      *    -------------------------------
           05  BSEQN11L PIC S9(0004) COMP.
           05  BSEQN11F PIC  X(0001).
           05  FILLER REDEFINES BSEQN11F.
               10  BSEQN11A PIC  X(0001).
           05  BSEQN11I PIC  X(0004).
      *    -------------------------------
           05  BPYNM11L PIC S9(0004) COMP.
           05  BPYNM11F PIC  X(0001).
           05  FILLER REDEFINES BPYNM11F.
               10  BPYNM11A PIC  X(0001).
           05  BPYNM11I PIC  X(0012).
      *    -------------------------------
           05  BPAMT11L PIC S9(0004) COMP.
           05  BPAMT11F PIC  X(0001).
           05  FILLER REDEFINES BPAMT11F.
               10  BPAMT11A PIC  X(0001).
           05  BPAMT11I PIC  X(0013).
      *    -------------------------------
           05  BCTRL12L PIC S9(0004) COMP.
           05  BCTRL12F PIC  X(0001).
           05  FILLER REDEFINES BCTRL12F.
               10  BCTRL12A PIC  X(0001).
           05  BCTRL12I PIC  X(0007).
      *    -------------------------------
           05  BCKNO12L PIC S9(0004) COMP.
           05  BCKNO12F PIC  X(0001).
           05  FILLER REDEFINES BCKNO12F.
               10  BCKNO12A PIC  X(0001).
           05  BCKNO12I PIC  X(0006).
      *    -------------------------------
           05  BCKDT12L PIC S9(0004) COMP.
           05  BCKDT12F PIC  X(0001).
           05  FILLER REDEFINES BCKDT12F.
               10  BCKDT12A PIC  X(0001).
           05  BCKDT12I PIC  X(0008).
      *    -------------------------------
           05  BCARR12L PIC S9(0004) COMP.
           05  BCARR12F PIC  X(0001).
           05  FILLER REDEFINES BCARR12F.
               10  BCARR12A PIC  X(0001).
           05  BCARR12I PIC  X(0001).
      *    -------------------------------
           05  BGRUP12L PIC S9(0004) COMP.
           05  BGRUP12F PIC  X(0001).
           05  FILLER REDEFINES BGRUP12F.
               10  BGRUP12A PIC  X(0001).
           05  BGRUP12I PIC  X(0006).
      *    -------------------------------
           05  BPYEE12L PIC S9(0004) COMP.
           05  BPYEE12F PIC  X(0001).
           05  FILLER REDEFINES BPYEE12F.
               10  BPYEE12A PIC  X(0001).
           05  BPYEE12I PIC  X(0010).
      *    -------------------------------
           05  BSEQN12L PIC S9(0004) COMP.
           05  BSEQN12F PIC  X(0001).
           05  FILLER REDEFINES BSEQN12F.
               10  BSEQN12A PIC  X(0001).
           05  BSEQN12I PIC  X(0004).
      *    -------------------------------
           05  BPYNM12L PIC S9(0004) COMP.
           05  BPYNM12F PIC  X(0001).
           05  FILLER REDEFINES BPYNM12F.
               10  BPYNM12A PIC  X(0001).
           05  BPYNM12I PIC  X(0012).
      *    -------------------------------
           05  BPAMT12L PIC S9(0004) COMP.
           05  BPAMT12F PIC  X(0001).
           05  FILLER REDEFINES BPAMT12F.
               10  BPAMT12A PIC  X(0001).
           05  BPAMT12I PIC  X(0013).
      *    -------------------------------
           05  BCTRL13L PIC S9(0004) COMP.
           05  BCTRL13F PIC  X(0001).
           05  FILLER REDEFINES BCTRL13F.
               10  BCTRL13A PIC  X(0001).
           05  BCTRL13I PIC  X(0007).
      *    -------------------------------
           05  BCKNO13L PIC S9(0004) COMP.
           05  BCKNO13F PIC  X(0001).
           05  FILLER REDEFINES BCKNO13F.
               10  BCKNO13A PIC  X(0001).
           05  BCKNO13I PIC  X(0006).
      *    -------------------------------
           05  BCKDT13L PIC S9(0004) COMP.
           05  BCKDT13F PIC  X(0001).
           05  FILLER REDEFINES BCKDT13F.
               10  BCKDT13A PIC  X(0001).
           05  BCKDT13I PIC  X(0008).
      *    -------------------------------
           05  BCARR13L PIC S9(0004) COMP.
           05  BCARR13F PIC  X(0001).
           05  FILLER REDEFINES BCARR13F.
               10  BCARR13A PIC  X(0001).
           05  BCARR13I PIC  X(0001).
      *    -------------------------------
           05  BGRUP13L PIC S9(0004) COMP.
           05  BGRUP13F PIC  X(0001).
           05  FILLER REDEFINES BGRUP13F.
               10  BGRUP13A PIC  X(0001).
           05  BGRUP13I PIC  X(0006).
      *    -------------------------------
           05  BPYEE13L PIC S9(0004) COMP.
           05  BPYEE13F PIC  X(0001).
           05  FILLER REDEFINES BPYEE13F.
               10  BPYEE13A PIC  X(0001).
           05  BPYEE13I PIC  X(0010).
      *    -------------------------------
           05  BSEQN13L PIC S9(0004) COMP.
           05  BSEQN13F PIC  X(0001).
           05  FILLER REDEFINES BSEQN13F.
               10  BSEQN13A PIC  X(0001).
           05  BSEQN13I PIC  X(0004).
      *    -------------------------------
           05  BPYNM13L PIC S9(0004) COMP.
           05  BPYNM13F PIC  X(0001).
           05  FILLER REDEFINES BPYNM13F.
               10  BPYNM13A PIC  X(0001).
           05  BPYNM13I PIC  X(0012).
      *    -------------------------------
           05  BPAMT13L PIC S9(0004) COMP.
           05  BPAMT13F PIC  X(0001).
           05  FILLER REDEFINES BPAMT13F.
               10  BPAMT13A PIC  X(0001).
           05  BPAMT13I PIC  X(0013).
      *    -------------------------------
           05  BCTRL14L PIC S9(0004) COMP.
           05  BCTRL14F PIC  X(0001).
           05  FILLER REDEFINES BCTRL14F.
               10  BCTRL14A PIC  X(0001).
           05  BCTRL14I PIC  X(0007).
      *    -------------------------------
           05  BCKNO14L PIC S9(0004) COMP.
           05  BCKNO14F PIC  X(0001).
           05  FILLER REDEFINES BCKNO14F.
               10  BCKNO14A PIC  X(0001).
           05  BCKNO14I PIC  X(0006).
      *    -------------------------------
           05  BCKDT14L PIC S9(0004) COMP.
           05  BCKDT14F PIC  X(0001).
           05  FILLER REDEFINES BCKDT14F.
               10  BCKDT14A PIC  X(0001).
           05  BCKDT14I PIC  X(0008).
      *    -------------------------------
           05  BCARR14L PIC S9(0004) COMP.
           05  BCARR14F PIC  X(0001).
           05  FILLER REDEFINES BCARR14F.
               10  BCARR14A PIC  X(0001).
           05  BCARR14I PIC  X(0001).
      *    -------------------------------
           05  BGRUP14L PIC S9(0004) COMP.
           05  BGRUP14F PIC  X(0001).
           05  FILLER REDEFINES BGRUP14F.
               10  BGRUP14A PIC  X(0001).
           05  BGRUP14I PIC  X(0006).
      *    -------------------------------
           05  BPYEE14L PIC S9(0004) COMP.
           05  BPYEE14F PIC  X(0001).
           05  FILLER REDEFINES BPYEE14F.
               10  BPYEE14A PIC  X(0001).
           05  BPYEE14I PIC  X(0010).
      *    -------------------------------
           05  BSEQN14L PIC S9(0004) COMP.
           05  BSEQN14F PIC  X(0001).
           05  FILLER REDEFINES BSEQN14F.
               10  BSEQN14A PIC  X(0001).
           05  BSEQN14I PIC  X(0004).
      *    -------------------------------
           05  BPYNM14L PIC S9(0004) COMP.
           05  BPYNM14F PIC  X(0001).
           05  FILLER REDEFINES BPYNM14F.
               10  BPYNM14A PIC  X(0001).
           05  BPYNM14I PIC  X(0012).
      *    -------------------------------
           05  BPAMT14L PIC S9(0004) COMP.
           05  BPAMT14F PIC  X(0001).
           05  FILLER REDEFINES BPAMT14F.
               10  BPAMT14A PIC  X(0001).
           05  BPAMT14I PIC  X(0013).
      *    -------------------------------
           05  BCTRL15L PIC S9(0004) COMP.
           05  BCTRL15F PIC  X(0001).
           05  FILLER REDEFINES BCTRL15F.
               10  BCTRL15A PIC  X(0001).
           05  BCTRL15I PIC  X(0007).
      *    -------------------------------
           05  BCKNO15L PIC S9(0004) COMP.
           05  BCKNO15F PIC  X(0001).
           05  FILLER REDEFINES BCKNO15F.
               10  BCKNO15A PIC  X(0001).
           05  BCKNO15I PIC  X(0006).
      *    -------------------------------
           05  BCKDT15L PIC S9(0004) COMP.
           05  BCKDT15F PIC  X(0001).
           05  FILLER REDEFINES BCKDT15F.
               10  BCKDT15A PIC  X(0001).
           05  BCKDT15I PIC  X(0008).
      *    -------------------------------
           05  BCARR15L PIC S9(0004) COMP.
           05  BCARR15F PIC  X(0001).
           05  FILLER REDEFINES BCARR15F.
               10  BCARR15A PIC  X(0001).
           05  BCARR15I PIC  X(0001).
      *    -------------------------------
           05  BGRUP15L PIC S9(0004) COMP.
           05  BGRUP15F PIC  X(0001).
           05  FILLER REDEFINES BGRUP15F.
               10  BGRUP15A PIC  X(0001).
           05  BGRUP15I PIC  X(0006).
      *    -------------------------------
           05  BPYEE15L PIC S9(0004) COMP.
           05  BPYEE15F PIC  X(0001).
           05  FILLER REDEFINES BPYEE15F.
               10  BPYEE15A PIC  X(0001).
           05  BPYEE15I PIC  X(0010).
      *    -------------------------------
           05  BSEQN15L PIC S9(0004) COMP.
           05  BSEQN15F PIC  X(0001).
           05  FILLER REDEFINES BSEQN15F.
               10  BSEQN15A PIC  X(0001).
           05  BSEQN15I PIC  X(0004).
      *    -------------------------------
           05  BPYNM15L PIC S9(0004) COMP.
           05  BPYNM15F PIC  X(0001).
           05  FILLER REDEFINES BPYNM15F.
               10  BPYNM15A PIC  X(0001).
           05  BPYNM15I PIC  X(0012).
      *    -------------------------------
           05  BPAMT15L PIC S9(0004) COMP.
           05  BPAMT15F PIC  X(0001).
           05  FILLER REDEFINES BPAMT15F.
               10  BPAMT15A PIC  X(0001).
           05  BPAMT15I PIC  X(0013).
      *    -------------------------------
           05  BCTRL16L PIC S9(0004) COMP.
           05  BCTRL16F PIC  X(0001).
           05  FILLER REDEFINES BCTRL16F.
               10  BCTRL16A PIC  X(0001).
           05  BCTRL16I PIC  X(0007).
      *    -------------------------------
           05  BCKNO16L PIC S9(0004) COMP.
           05  BCKNO16F PIC  X(0001).
           05  FILLER REDEFINES BCKNO16F.
               10  BCKNO16A PIC  X(0001).
           05  BCKNO16I PIC  X(0006).
      *    -------------------------------
           05  BCKDT16L PIC S9(0004) COMP.
           05  BCKDT16F PIC  X(0001).
           05  FILLER REDEFINES BCKDT16F.
               10  BCKDT16A PIC  X(0001).
           05  BCKDT16I PIC  X(0008).
      *    -------------------------------
           05  BCARR16L PIC S9(0004) COMP.
           05  BCARR16F PIC  X(0001).
           05  FILLER REDEFINES BCARR16F.
               10  BCARR16A PIC  X(0001).
           05  BCARR16I PIC  X(0001).
      *    -------------------------------
           05  BGRUP16L PIC S9(0004) COMP.
           05  BGRUP16F PIC  X(0001).
           05  FILLER REDEFINES BGRUP16F.
               10  BGRUP16A PIC  X(0001).
           05  BGRUP16I PIC  X(0006).
      *    -------------------------------
           05  BPYEE16L PIC S9(0004) COMP.
           05  BPYEE16F PIC  X(0001).
           05  FILLER REDEFINES BPYEE16F.
               10  BPYEE16A PIC  X(0001).
           05  BPYEE16I PIC  X(0010).
      *    -------------------------------
           05  BSEQN16L PIC S9(0004) COMP.
           05  BSEQN16F PIC  X(0001).
           05  FILLER REDEFINES BSEQN16F.
               10  BSEQN16A PIC  X(0001).
           05  BSEQN16I PIC  X(0004).
      *    -------------------------------
           05  BPYNM16L PIC S9(0004) COMP.
           05  BPYNM16F PIC  X(0001).
           05  FILLER REDEFINES BPYNM16F.
               10  BPYNM16A PIC  X(0001).
           05  BPYNM16I PIC  X(0012).
      *    -------------------------------
           05  BPAMT16L PIC S9(0004) COMP.
           05  BPAMT16F PIC  X(0001).
           05  FILLER REDEFINES BPAMT16F.
               10  BPAMT16A PIC  X(0001).
           05  BPAMT16I PIC  X(0013).
      *    -------------------------------
           05  BCTRL17L PIC S9(0004) COMP.
           05  BCTRL17F PIC  X(0001).
           05  FILLER REDEFINES BCTRL17F.
               10  BCTRL17A PIC  X(0001).
           05  BCTRL17I PIC  X(0007).
      *    -------------------------------
           05  BCKNO17L PIC S9(0004) COMP.
           05  BCKNO17F PIC  X(0001).
           05  FILLER REDEFINES BCKNO17F.
               10  BCKNO17A PIC  X(0001).
           05  BCKNO17I PIC  X(0006).
      *    -------------------------------
           05  BCKDT17L PIC S9(0004) COMP.
           05  BCKDT17F PIC  X(0001).
           05  FILLER REDEFINES BCKDT17F.
               10  BCKDT17A PIC  X(0001).
           05  BCKDT17I PIC  X(0008).
      *    -------------------------------
           05  BCARR17L PIC S9(0004) COMP.
           05  BCARR17F PIC  X(0001).
           05  FILLER REDEFINES BCARR17F.
               10  BCARR17A PIC  X(0001).
           05  BCARR17I PIC  X(0001).
      *    -------------------------------
           05  BGRUP17L PIC S9(0004) COMP.
           05  BGRUP17F PIC  X(0001).
           05  FILLER REDEFINES BGRUP17F.
               10  BGRUP17A PIC  X(0001).
           05  BGRUP17I PIC  X(0006).
      *    -------------------------------
           05  BPYEE17L PIC S9(0004) COMP.
           05  BPYEE17F PIC  X(0001).
           05  FILLER REDEFINES BPYEE17F.
               10  BPYEE17A PIC  X(0001).
           05  BPYEE17I PIC  X(0010).
      *    -------------------------------
           05  BSEQN17L PIC S9(0004) COMP.
           05  BSEQN17F PIC  X(0001).
           05  FILLER REDEFINES BSEQN17F.
               10  BSEQN17A PIC  X(0001).
           05  BSEQN17I PIC  X(0004).
      *    -------------------------------
           05  BPYNM17L PIC S9(0004) COMP.
           05  BPYNM17F PIC  X(0001).
           05  FILLER REDEFINES BPYNM17F.
               10  BPYNM17A PIC  X(0001).
           05  BPYNM17I PIC  X(0012).
      *    -------------------------------
           05  BPAMT17L PIC S9(0004) COMP.
           05  BPAMT17F PIC  X(0001).
           05  FILLER REDEFINES BPAMT17F.
               10  BPAMT17A PIC  X(0001).
           05  BPAMT17I PIC  X(0013).
      *    -------------------------------
           05  BCTRL18L PIC S9(0004) COMP.
           05  BCTRL18F PIC  X(0001).
           05  FILLER REDEFINES BCTRL18F.
               10  BCTRL18A PIC  X(0001).
           05  BCTRL18I PIC  X(0007).
      *    -------------------------------
           05  BCKNO18L PIC S9(0004) COMP.
           05  BCKNO18F PIC  X(0001).
           05  FILLER REDEFINES BCKNO18F.
               10  BCKNO18A PIC  X(0001).
           05  BCKNO18I PIC  X(0006).
      *    -------------------------------
           05  BCKDT18L PIC S9(0004) COMP.
           05  BCKDT18F PIC  X(0001).
           05  FILLER REDEFINES BCKDT18F.
               10  BCKDT18A PIC  X(0001).
           05  BCKDT18I PIC  X(0008).
      *    -------------------------------
           05  BCARR18L PIC S9(0004) COMP.
           05  BCARR18F PIC  X(0001).
           05  FILLER REDEFINES BCARR18F.
               10  BCARR18A PIC  X(0001).
           05  BCARR18I PIC  X(0001).
      *    -------------------------------
           05  BGRUP18L PIC S9(0004) COMP.
           05  BGRUP18F PIC  X(0001).
           05  FILLER REDEFINES BGRUP18F.
               10  BGRUP18A PIC  X(0001).
           05  BGRUP18I PIC  X(0006).
      *    -------------------------------
           05  BPYEE18L PIC S9(0004) COMP.
           05  BPYEE18F PIC  X(0001).
           05  FILLER REDEFINES BPYEE18F.
               10  BPYEE18A PIC  X(0001).
           05  BPYEE18I PIC  X(0010).
      *    -------------------------------
           05  BSEQN18L PIC S9(0004) COMP.
           05  BSEQN18F PIC  X(0001).
           05  FILLER REDEFINES BSEQN18F.
               10  BSEQN18A PIC  X(0001).
           05  BSEQN18I PIC  X(0004).
      *    -------------------------------
           05  BPYNM18L PIC S9(0004) COMP.
           05  BPYNM18F PIC  X(0001).
           05  FILLER REDEFINES BPYNM18F.
               10  BPYNM18A PIC  X(0001).
           05  BPYNM18I PIC  X(0012).
      *    -------------------------------
           05  BPAMT18L PIC S9(0004) COMP.
           05  BPAMT18F PIC  X(0001).
           05  FILLER REDEFINES BPAMT18F.
               10  BPAMT18A PIC  X(0001).
           05  BPAMT18I PIC  X(0013).
      *    -------------------------------
           05  BEMSG1L PIC S9(0004) COMP.
           05  BEMSG1F PIC  X(0001).
           05  FILLER REDEFINES BEMSG1F.
               10  BEMSG1A PIC  X(0001).
           05  BEMSG1I PIC  X(0079).
      *    -------------------------------
           05  BPFKL PIC S9(0004) COMP.
           05  BPFKF PIC  X(0001).
           05  FILLER REDEFINES BPFKF.
               10  BPFKA PIC  X(0001).
           05  BPFKI PIC  9(2).
      *    -------------------------------
           05  BPRINTRL PIC S9(0004) COMP.
           05  BPRINTRF PIC  X(0001).
           05  FILLER REDEFINES BPRINTRF.
               10  BPRINTRA PIC  X(0001).
           05  BPRINTRI PIC  X(0004).
      *    -------------------------------
           05  BPFDESCL PIC S9(0004) COMP.
           05  BPFDESCF PIC  X(0001).
           05  FILLER REDEFINES BPFDESCF.
               10  BPFDESCA PIC  X(0001).
           05  BPFDESCI PIC  X(0029).
      *    -------------------------------
           05  BCNTLNDL PIC S9(0004) COMP.
           05  BCNTLNDF PIC  X(0001).
           05  FILLER REDEFINES BCNTLNDF.
               10  BCNTLNDA PIC  X(0001).
           05  BCNTLNDI PIC  X(0011).
      *    -------------------------------
           05  BCNTLNOL PIC S9(0004) COMP.
           05  BCNTLNOF PIC  X(0001).
           05  FILLER REDEFINES BCNTLNOF.
               10  BCNTLNOA PIC  X(0001).
           05  BCNTLNOI PIC  X(0007).
       01  EL685BO REDEFINES EL685BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTITLEO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSCREENO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAGEO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO01O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP01O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN01O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM01O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT01O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO02O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP02O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN02O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM02O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT02O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO03O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP03O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN03O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM03O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT03O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO04O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP04O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN04O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM04O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT04O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO05O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP05O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN05O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM05O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT05O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO06O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP06O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN06O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM06O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT06O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO07O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP07O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN07O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM07O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT07O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO08O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP08O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN08O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM08O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT08O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO09O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP09O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE09O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN09O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM09O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT09O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM10O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT10O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO11O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP11O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM11O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT11O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO12O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP12O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM12O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT12O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO13O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP13O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN13O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM13O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT13O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO14O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT14O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP14O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN14O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM14O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT14O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO15O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT15O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP15O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN15O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM15O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT15O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO16O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT16O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP16O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE16O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN16O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM16O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT16O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL17O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO17O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT17O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP17O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE17O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN17O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM17O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT17O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTRL18O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKNO18O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCKDT18O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRUP18O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYEE18O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQN18O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPYNM18O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAMT18O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFKO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPRINTRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFDESCO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTLNDO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTLNOO PIC  X(0007).
      *    -------------------------------
       01  EL685AI REDEFINES EL685BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  ATITLEL PIC S9(0004) COMP.
           05  ATITLEF PIC  X(0001).
           05  FILLER REDEFINES ATITLEF.
               10  ATITLEA PIC  X(0001).
           05  ATITLEI PIC  X(0028).
      *    -------------------------------
           05  ASCREENL PIC S9(0004) COMP.
           05  ASCREENF PIC  X(0001).
           05  FILLER REDEFINES ASCREENF.
               10  ASCREENA PIC  X(0001).
           05  ASCREENI PIC  X(0006).
      *    -------------------------------
           05  APAGEL PIC S9(0004) COMP.
           05  APAGEF PIC  X(0001).
           05  FILLER REDEFINES APAGEF.
               10  APAGEA PIC  X(0001).
           05  APAGEI PIC  S9(4).
      *    -------------------------------
           05  ADESC1L PIC S9(0004) COMP.
           05  ADESC1F PIC  X(0001).
           05  FILLER REDEFINES ADESC1F.
               10  ADESC1A PIC  X(0001).
           05  ADESC1I PIC  X(0008).
      *    -------------------------------
           05  ADESC2L PIC S9(0004) COMP.
           05  ADESC2F PIC  X(0001).
           05  FILLER REDEFINES ADESC2F.
               10  ADESC2A PIC  X(0001).
           05  ADESC2I PIC  X(0008).
      *    -------------------------------
           05  ACTRL01L PIC S9(0004) COMP.
           05  ACTRL01F PIC  X(0001).
           05  FILLER REDEFINES ACTRL01F.
               10  ACTRL01A PIC  X(0001).
           05  ACTRL01I PIC  X(0007).
      *    -------------------------------
           05  ACKNO01L PIC S9(0004) COMP.
           05  ACKNO01F PIC  X(0001).
           05  FILLER REDEFINES ACKNO01F.
               10  ACKNO01A PIC  X(0001).
           05  ACKNO01I PIC  X(0007).
      *    -------------------------------
           05  ATYPE01L PIC S9(0004) COMP.
           05  ATYPE01F PIC  X(0001).
           05  FILLER REDEFINES ATYPE01F.
               10  ATYPE01A PIC  X(0001).
           05  ATYPE01I PIC  X(0011).
      *    -------------------------------
           05  ACARR01L PIC S9(0004) COMP.
           05  ACARR01F PIC  X(0001).
           05  FILLER REDEFINES ACARR01F.
               10  ACARR01A PIC  X(0001).
           05  ACARR01I PIC  X(0001).
      *    -------------------------------
           05  AGRUP01L PIC S9(0004) COMP.
           05  AGRUP01F PIC  X(0001).
           05  FILLER REDEFINES AGRUP01F.
               10  AGRUP01A PIC  X(0001).
           05  AGRUP01I PIC  X(0006).
      *    -------------------------------
           05  AFINR01L PIC S9(0004) COMP.
           05  AFINR01F PIC  X(0001).
           05  FILLER REDEFINES AFINR01F.
               10  AFINR01A PIC  X(0001).
           05  AFINR01I PIC  X(0010).
      *    -------------------------------
           05  AACCT01L PIC S9(0004) COMP.
           05  AACCT01F PIC  X(0001).
           05  FILLER REDEFINES AACCT01F.
               10  AACCT01A PIC  X(0001).
           05  AACCT01I PIC  X(0010).
      *    -------------------------------
           05  APAMT01L PIC S9(0004) COMP.
           05  APAMT01F PIC  X(0001).
           05  FILLER REDEFINES APAMT01F.
               10  APAMT01A PIC  X(0001).
           05  APAMT01I PIC  X(0013).
      *    -------------------------------
           05  ACTRL02L PIC S9(0004) COMP.
           05  ACTRL02F PIC  X(0001).
           05  FILLER REDEFINES ACTRL02F.
               10  ACTRL02A PIC  X(0001).
           05  ACTRL02I PIC  X(0007).
      *    -------------------------------
           05  ACKNO02L PIC S9(0004) COMP.
           05  ACKNO02F PIC  X(0001).
           05  FILLER REDEFINES ACKNO02F.
               10  ACKNO02A PIC  X(0001).
           05  ACKNO02I PIC  X(0007).
      *    -------------------------------
           05  ATYPE02L PIC S9(0004) COMP.
           05  ATYPE02F PIC  X(0001).
           05  FILLER REDEFINES ATYPE02F.
               10  ATYPE02A PIC  X(0001).
           05  ATYPE02I PIC  X(0011).
      *    -------------------------------
           05  ACARR02L PIC S9(0004) COMP.
           05  ACARR02F PIC  X(0001).
           05  FILLER REDEFINES ACARR02F.
               10  ACARR02A PIC  X(0001).
           05  ACARR02I PIC  X(0001).
      *    -------------------------------
           05  AGRUP02L PIC S9(0004) COMP.
           05  AGRUP02F PIC  X(0001).
           05  FILLER REDEFINES AGRUP02F.
               10  AGRUP02A PIC  X(0001).
           05  AGRUP02I PIC  X(0006).
      *    -------------------------------
           05  AFINR02L PIC S9(0004) COMP.
           05  AFINR02F PIC  X(0001).
           05  FILLER REDEFINES AFINR02F.
               10  AFINR02A PIC  X(0001).
           05  AFINR02I PIC  X(0010).
      *    -------------------------------
           05  AACCT02L PIC S9(0004) COMP.
           05  AACCT02F PIC  X(0001).
           05  FILLER REDEFINES AACCT02F.
               10  AACCT02A PIC  X(0001).
           05  AACCT02I PIC  X(0010).
      *    -------------------------------
           05  APAMT02L PIC S9(0004) COMP.
           05  APAMT02F PIC  X(0001).
           05  FILLER REDEFINES APAMT02F.
               10  APAMT02A PIC  X(0001).
           05  APAMT02I PIC  X(0013).
      *    -------------------------------
           05  ACTRL03L PIC S9(0004) COMP.
           05  ACTRL03F PIC  X(0001).
           05  FILLER REDEFINES ACTRL03F.
               10  ACTRL03A PIC  X(0001).
           05  ACTRL03I PIC  X(0007).
      *    -------------------------------
           05  ACKNO03L PIC S9(0004) COMP.
           05  ACKNO03F PIC  X(0001).
           05  FILLER REDEFINES ACKNO03F.
               10  ACKNO03A PIC  X(0001).
           05  ACKNO03I PIC  X(0007).
      *    -------------------------------
           05  ATYPE03L PIC S9(0004) COMP.
           05  ATYPE03F PIC  X(0001).
           05  FILLER REDEFINES ATYPE03F.
               10  ATYPE03A PIC  X(0001).
           05  ATYPE03I PIC  X(0011).
      *    -------------------------------
           05  ACARR03L PIC S9(0004) COMP.
           05  ACARR03F PIC  X(0001).
           05  FILLER REDEFINES ACARR03F.
               10  ACARR03A PIC  X(0001).
           05  ACARR03I PIC  X(0001).
      *    -------------------------------
           05  AGRUP03L PIC S9(0004) COMP.
           05  AGRUP03F PIC  X(0001).
           05  FILLER REDEFINES AGRUP03F.
               10  AGRUP03A PIC  X(0001).
           05  AGRUP03I PIC  X(0006).
      *    -------------------------------
           05  AFINR03L PIC S9(0004) COMP.
           05  AFINR03F PIC  X(0001).
           05  FILLER REDEFINES AFINR03F.
               10  AFINR03A PIC  X(0001).
           05  AFINR03I PIC  X(0010).
      *    -------------------------------
           05  AACCT03L PIC S9(0004) COMP.
           05  AACCT03F PIC  X(0001).
           05  FILLER REDEFINES AACCT03F.
               10  AACCT03A PIC  X(0001).
           05  AACCT03I PIC  X(0010).
      *    -------------------------------
           05  APAMT03L PIC S9(0004) COMP.
           05  APAMT03F PIC  X(0001).
           05  FILLER REDEFINES APAMT03F.
               10  APAMT03A PIC  X(0001).
           05  APAMT03I PIC  X(0013).
      *    -------------------------------
           05  ACTRL04L PIC S9(0004) COMP.
           05  ACTRL04F PIC  X(0001).
           05  FILLER REDEFINES ACTRL04F.
               10  ACTRL04A PIC  X(0001).
           05  ACTRL04I PIC  X(0007).
      *    -------------------------------
           05  ACKNO04L PIC S9(0004) COMP.
           05  ACKNO04F PIC  X(0001).
           05  FILLER REDEFINES ACKNO04F.
               10  ACKNO04A PIC  X(0001).
           05  ACKNO04I PIC  X(0007).
      *    -------------------------------
           05  ATYPE04L PIC S9(0004) COMP.
           05  ATYPE04F PIC  X(0001).
           05  FILLER REDEFINES ATYPE04F.
               10  ATYPE04A PIC  X(0001).
           05  ATYPE04I PIC  X(0011).
      *    -------------------------------
           05  ACARR04L PIC S9(0004) COMP.
           05  ACARR04F PIC  X(0001).
           05  FILLER REDEFINES ACARR04F.
               10  ACARR04A PIC  X(0001).
           05  ACARR04I PIC  X(0001).
      *    -------------------------------
           05  AGRUP04L PIC S9(0004) COMP.
           05  AGRUP04F PIC  X(0001).
           05  FILLER REDEFINES AGRUP04F.
               10  AGRUP04A PIC  X(0001).
           05  AGRUP04I PIC  X(0006).
      *    -------------------------------
           05  AFINR04L PIC S9(0004) COMP.
           05  AFINR04F PIC  X(0001).
           05  FILLER REDEFINES AFINR04F.
               10  AFINR04A PIC  X(0001).
           05  AFINR04I PIC  X(0010).
      *    -------------------------------
           05  AACCT04L PIC S9(0004) COMP.
           05  AACCT04F PIC  X(0001).
           05  FILLER REDEFINES AACCT04F.
               10  AACCT04A PIC  X(0001).
           05  AACCT04I PIC  X(0010).
      *    -------------------------------
           05  APAMT04L PIC S9(0004) COMP.
           05  APAMT04F PIC  X(0001).
           05  FILLER REDEFINES APAMT04F.
               10  APAMT04A PIC  X(0001).
           05  APAMT04I PIC  X(0013).
      *    -------------------------------
           05  ACTRL05L PIC S9(0004) COMP.
           05  ACTRL05F PIC  X(0001).
           05  FILLER REDEFINES ACTRL05F.
               10  ACTRL05A PIC  X(0001).
           05  ACTRL05I PIC  X(0007).
      *    -------------------------------
           05  ACKNO05L PIC S9(0004) COMP.
           05  ACKNO05F PIC  X(0001).
           05  FILLER REDEFINES ACKNO05F.
               10  ACKNO05A PIC  X(0001).
           05  ACKNO05I PIC  X(0007).
      *    -------------------------------
           05  ATYPE05L PIC S9(0004) COMP.
           05  ATYPE05F PIC  X(0001).
           05  FILLER REDEFINES ATYPE05F.
               10  ATYPE05A PIC  X(0001).
           05  ATYPE05I PIC  X(0011).
      *    -------------------------------
           05  ACARR05L PIC S9(0004) COMP.
           05  ACARR05F PIC  X(0001).
           05  FILLER REDEFINES ACARR05F.
               10  ACARR05A PIC  X(0001).
           05  ACARR05I PIC  X(0001).
      *    -------------------------------
           05  AGRUP05L PIC S9(0004) COMP.
           05  AGRUP05F PIC  X(0001).
           05  FILLER REDEFINES AGRUP05F.
               10  AGRUP05A PIC  X(0001).
           05  AGRUP05I PIC  X(0006).
      *    -------------------------------
           05  AFINR05L PIC S9(0004) COMP.
           05  AFINR05F PIC  X(0001).
           05  FILLER REDEFINES AFINR05F.
               10  AFINR05A PIC  X(0001).
           05  AFINR05I PIC  X(0010).
      *    -------------------------------
           05  AACCT05L PIC S9(0004) COMP.
           05  AACCT05F PIC  X(0001).
           05  FILLER REDEFINES AACCT05F.
               10  AACCT05A PIC  X(0001).
           05  AACCT05I PIC  X(0010).
      *    -------------------------------
           05  APAMT05L PIC S9(0004) COMP.
           05  APAMT05F PIC  X(0001).
           05  FILLER REDEFINES APAMT05F.
               10  APAMT05A PIC  X(0001).
           05  APAMT05I PIC  X(0013).
      *    -------------------------------
           05  ACTRL06L PIC S9(0004) COMP.
           05  ACTRL06F PIC  X(0001).
           05  FILLER REDEFINES ACTRL06F.
               10  ACTRL06A PIC  X(0001).
           05  ACTRL06I PIC  X(0007).
      *    -------------------------------
           05  ACKNO06L PIC S9(0004) COMP.
           05  ACKNO06F PIC  X(0001).
           05  FILLER REDEFINES ACKNO06F.
               10  ACKNO06A PIC  X(0001).
           05  ACKNO06I PIC  X(0007).
      *    -------------------------------
           05  ATYPE06L PIC S9(0004) COMP.
           05  ATYPE06F PIC  X(0001).
           05  FILLER REDEFINES ATYPE06F.
               10  ATYPE06A PIC  X(0001).
           05  ATYPE06I PIC  X(0011).
      *    -------------------------------
           05  ACARR06L PIC S9(0004) COMP.
           05  ACARR06F PIC  X(0001).
           05  FILLER REDEFINES ACARR06F.
               10  ACARR06A PIC  X(0001).
           05  ACARR06I PIC  X(0001).
      *    -------------------------------
           05  AGRUP06L PIC S9(0004) COMP.
           05  AGRUP06F PIC  X(0001).
           05  FILLER REDEFINES AGRUP06F.
               10  AGRUP06A PIC  X(0001).
           05  AGRUP06I PIC  X(0006).
      *    -------------------------------
           05  AFINR06L PIC S9(0004) COMP.
           05  AFINR06F PIC  X(0001).
           05  FILLER REDEFINES AFINR06F.
               10  AFINR06A PIC  X(0001).
           05  AFINR06I PIC  X(0010).
      *    -------------------------------
           05  AACCT06L PIC S9(0004) COMP.
           05  AACCT06F PIC  X(0001).
           05  FILLER REDEFINES AACCT06F.
               10  AACCT06A PIC  X(0001).
           05  AACCT06I PIC  X(0010).
      *    -------------------------------
           05  APAMT06L PIC S9(0004) COMP.
           05  APAMT06F PIC  X(0001).
           05  FILLER REDEFINES APAMT06F.
               10  APAMT06A PIC  X(0001).
           05  APAMT06I PIC  X(0013).
      *    -------------------------------
           05  ACTRL07L PIC S9(0004) COMP.
           05  ACTRL07F PIC  X(0001).
           05  FILLER REDEFINES ACTRL07F.
               10  ACTRL07A PIC  X(0001).
           05  ACTRL07I PIC  X(0007).
      *    -------------------------------
           05  ACKNO07L PIC S9(0004) COMP.
           05  ACKNO07F PIC  X(0001).
           05  FILLER REDEFINES ACKNO07F.
               10  ACKNO07A PIC  X(0001).
           05  ACKNO07I PIC  X(0007).
      *    -------------------------------
           05  ATYPE07L PIC S9(0004) COMP.
           05  ATYPE07F PIC  X(0001).
           05  FILLER REDEFINES ATYPE07F.
               10  ATYPE07A PIC  X(0001).
           05  ATYPE07I PIC  X(0011).
      *    -------------------------------
           05  ACARR07L PIC S9(0004) COMP.
           05  ACARR07F PIC  X(0001).
           05  FILLER REDEFINES ACARR07F.
               10  ACARR07A PIC  X(0001).
           05  ACARR07I PIC  X(0001).
      *    -------------------------------
           05  AGRUP07L PIC S9(0004) COMP.
           05  AGRUP07F PIC  X(0001).
           05  FILLER REDEFINES AGRUP07F.
               10  AGRUP07A PIC  X(0001).
           05  AGRUP07I PIC  X(0006).
      *    -------------------------------
           05  AFINR07L PIC S9(0004) COMP.
           05  AFINR07F PIC  X(0001).
           05  FILLER REDEFINES AFINR07F.
               10  AFINR07A PIC  X(0001).
           05  AFINR07I PIC  X(0010).
      *    -------------------------------
           05  AACCT07L PIC S9(0004) COMP.
           05  AACCT07F PIC  X(0001).
           05  FILLER REDEFINES AACCT07F.
               10  AACCT07A PIC  X(0001).
           05  AACCT07I PIC  X(0010).
      *    -------------------------------
           05  APAMT07L PIC S9(0004) COMP.
           05  APAMT07F PIC  X(0001).
           05  FILLER REDEFINES APAMT07F.
               10  APAMT07A PIC  X(0001).
           05  APAMT07I PIC  X(0013).
      *    -------------------------------
           05  ACTRL08L PIC S9(0004) COMP.
           05  ACTRL08F PIC  X(0001).
           05  FILLER REDEFINES ACTRL08F.
               10  ACTRL08A PIC  X(0001).
           05  ACTRL08I PIC  X(0007).
      *    -------------------------------
           05  ACKNO08L PIC S9(0004) COMP.
           05  ACKNO08F PIC  X(0001).
           05  FILLER REDEFINES ACKNO08F.
               10  ACKNO08A PIC  X(0001).
           05  ACKNO08I PIC  X(0007).
      *    -------------------------------
           05  ATYPE08L PIC S9(0004) COMP.
           05  ATYPE08F PIC  X(0001).
           05  FILLER REDEFINES ATYPE08F.
               10  ATYPE08A PIC  X(0001).
           05  ATYPE08I PIC  X(0011).
      *    -------------------------------
           05  ACARR08L PIC S9(0004) COMP.
           05  ACARR08F PIC  X(0001).
           05  FILLER REDEFINES ACARR08F.
               10  ACARR08A PIC  X(0001).
           05  ACARR08I PIC  X(0001).
      *    -------------------------------
           05  AGRUP08L PIC S9(0004) COMP.
           05  AGRUP08F PIC  X(0001).
           05  FILLER REDEFINES AGRUP08F.
               10  AGRUP08A PIC  X(0001).
           05  AGRUP08I PIC  X(0006).
      *    -------------------------------
           05  AFINR08L PIC S9(0004) COMP.
           05  AFINR08F PIC  X(0001).
           05  FILLER REDEFINES AFINR08F.
               10  AFINR08A PIC  X(0001).
           05  AFINR08I PIC  X(0010).
      *    -------------------------------
           05  AACCT08L PIC S9(0004) COMP.
           05  AACCT08F PIC  X(0001).
           05  FILLER REDEFINES AACCT08F.
               10  AACCT08A PIC  X(0001).
           05  AACCT08I PIC  X(0010).
      *    -------------------------------
           05  APAMT08L PIC S9(0004) COMP.
           05  APAMT08F PIC  X(0001).
           05  FILLER REDEFINES APAMT08F.
               10  APAMT08A PIC  X(0001).
           05  APAMT08I PIC  X(0013).
      *    -------------------------------
           05  ACTRL09L PIC S9(0004) COMP.
           05  ACTRL09F PIC  X(0001).
           05  FILLER REDEFINES ACTRL09F.
               10  ACTRL09A PIC  X(0001).
           05  ACTRL09I PIC  X(0007).
      *    -------------------------------
           05  ACKNO09L PIC S9(0004) COMP.
           05  ACKNO09F PIC  X(0001).
           05  FILLER REDEFINES ACKNO09F.
               10  ACKNO09A PIC  X(0001).
           05  ACKNO09I PIC  X(0007).
      *    -------------------------------
           05  ATYPE09L PIC S9(0004) COMP.
           05  ATYPE09F PIC  X(0001).
           05  FILLER REDEFINES ATYPE09F.
               10  ATYPE09A PIC  X(0001).
           05  ATYPE09I PIC  X(0011).
      *    -------------------------------
           05  ACARR09L PIC S9(0004) COMP.
           05  ACARR09F PIC  X(0001).
           05  FILLER REDEFINES ACARR09F.
               10  ACARR09A PIC  X(0001).
           05  ACARR09I PIC  X(0001).
      *    -------------------------------
           05  AGRUP09L PIC S9(0004) COMP.
           05  AGRUP09F PIC  X(0001).
           05  FILLER REDEFINES AGRUP09F.
               10  AGRUP09A PIC  X(0001).
           05  AGRUP09I PIC  X(0006).
      *    -------------------------------
           05  AFINR09L PIC S9(0004) COMP.
           05  AFINR09F PIC  X(0001).
           05  FILLER REDEFINES AFINR09F.
               10  AFINR09A PIC  X(0001).
           05  AFINR09I PIC  X(0010).
      *    -------------------------------
           05  AACCT09L PIC S9(0004) COMP.
           05  AACCT09F PIC  X(0001).
           05  FILLER REDEFINES AACCT09F.
               10  AACCT09A PIC  X(0001).
           05  AACCT09I PIC  X(0010).
      *    -------------------------------
           05  APAMT09L PIC S9(0004) COMP.
           05  APAMT09F PIC  X(0001).
           05  FILLER REDEFINES APAMT09F.
               10  APAMT09A PIC  X(0001).
           05  APAMT09I PIC  X(0013).
      *    -------------------------------
           05  ACTRL10L PIC S9(0004) COMP.
           05  ACTRL10F PIC  X(0001).
           05  FILLER REDEFINES ACTRL10F.
               10  ACTRL10A PIC  X(0001).
           05  ACTRL10I PIC  X(0007).
      *    -------------------------------
           05  ACKNO10L PIC S9(0004) COMP.
           05  ACKNO10F PIC  X(0001).
           05  FILLER REDEFINES ACKNO10F.
               10  ACKNO10A PIC  X(0001).
           05  ACKNO10I PIC  X(0007).
      *    -------------------------------
           05  ATYPE10L PIC S9(0004) COMP.
           05  ATYPE10F PIC  X(0001).
           05  FILLER REDEFINES ATYPE10F.
               10  ATYPE10A PIC  X(0001).
           05  ATYPE10I PIC  X(0011).
      *    -------------------------------
           05  ACARR10L PIC S9(0004) COMP.
           05  ACARR10F PIC  X(0001).
           05  FILLER REDEFINES ACARR10F.
               10  ACARR10A PIC  X(0001).
           05  ACARR10I PIC  X(0001).
      *    -------------------------------
           05  AGRUP10L PIC S9(0004) COMP.
           05  AGRUP10F PIC  X(0001).
           05  FILLER REDEFINES AGRUP10F.
               10  AGRUP10A PIC  X(0001).
           05  AGRUP10I PIC  X(0006).
      *    -------------------------------
           05  AFINR10L PIC S9(0004) COMP.
           05  AFINR10F PIC  X(0001).
           05  FILLER REDEFINES AFINR10F.
               10  AFINR10A PIC  X(0001).
           05  AFINR10I PIC  X(0010).
      *    -------------------------------
           05  AACCT10L PIC S9(0004) COMP.
           05  AACCT10F PIC  X(0001).
           05  FILLER REDEFINES AACCT10F.
               10  AACCT10A PIC  X(0001).
           05  AACCT10I PIC  X(0010).
      *    -------------------------------
           05  APAMT10L PIC S9(0004) COMP.
           05  APAMT10F PIC  X(0001).
           05  FILLER REDEFINES APAMT10F.
               10  APAMT10A PIC  X(0001).
           05  APAMT10I PIC  X(0013).
      *    -------------------------------
           05  ACTRL11L PIC S9(0004) COMP.
           05  ACTRL11F PIC  X(0001).
           05  FILLER REDEFINES ACTRL11F.
               10  ACTRL11A PIC  X(0001).
           05  ACTRL11I PIC  X(0007).
      *    -------------------------------
           05  ACKNO11L PIC S9(0004) COMP.
           05  ACKNO11F PIC  X(0001).
           05  FILLER REDEFINES ACKNO11F.
               10  ACKNO11A PIC  X(0001).
           05  ACKNO11I PIC  X(0007).
      *    -------------------------------
           05  ATYPE11L PIC S9(0004) COMP.
           05  ATYPE11F PIC  X(0001).
           05  FILLER REDEFINES ATYPE11F.
               10  ATYPE11A PIC  X(0001).
           05  ATYPE11I PIC  X(0011).
      *    -------------------------------
           05  ACARR11L PIC S9(0004) COMP.
           05  ACARR11F PIC  X(0001).
           05  FILLER REDEFINES ACARR11F.
               10  ACARR11A PIC  X(0001).
           05  ACARR11I PIC  X(0001).
      *    -------------------------------
           05  AGRUP11L PIC S9(0004) COMP.
           05  AGRUP11F PIC  X(0001).
           05  FILLER REDEFINES AGRUP11F.
               10  AGRUP11A PIC  X(0001).
           05  AGRUP11I PIC  X(0006).
      *    -------------------------------
           05  AFINR11L PIC S9(0004) COMP.
           05  AFINR11F PIC  X(0001).
           05  FILLER REDEFINES AFINR11F.
               10  AFINR11A PIC  X(0001).
           05  AFINR11I PIC  X(0010).
      *    -------------------------------
           05  AACCT11L PIC S9(0004) COMP.
           05  AACCT11F PIC  X(0001).
           05  FILLER REDEFINES AACCT11F.
               10  AACCT11A PIC  X(0001).
           05  AACCT11I PIC  X(0010).
      *    -------------------------------
           05  APAMT11L PIC S9(0004) COMP.
           05  APAMT11F PIC  X(0001).
           05  FILLER REDEFINES APAMT11F.
               10  APAMT11A PIC  X(0001).
           05  APAMT11I PIC  X(0013).
      *    -------------------------------
           05  ACTRL12L PIC S9(0004) COMP.
           05  ACTRL12F PIC  X(0001).
           05  FILLER REDEFINES ACTRL12F.
               10  ACTRL12A PIC  X(0001).
           05  ACTRL12I PIC  X(0007).
      *    -------------------------------
           05  ACKNO12L PIC S9(0004) COMP.
           05  ACKNO12F PIC  X(0001).
           05  FILLER REDEFINES ACKNO12F.
               10  ACKNO12A PIC  X(0001).
           05  ACKNO12I PIC  X(0007).
      *    -------------------------------
           05  ATYPE12L PIC S9(0004) COMP.
           05  ATYPE12F PIC  X(0001).
           05  FILLER REDEFINES ATYPE12F.
               10  ATYPE12A PIC  X(0001).
           05  ATYPE12I PIC  X(0011).
      *    -------------------------------
           05  ACARR12L PIC S9(0004) COMP.
           05  ACARR12F PIC  X(0001).
           05  FILLER REDEFINES ACARR12F.
               10  ACARR12A PIC  X(0001).
           05  ACARR12I PIC  X(0001).
      *    -------------------------------
           05  AGRUP12L PIC S9(0004) COMP.
           05  AGRUP12F PIC  X(0001).
           05  FILLER REDEFINES AGRUP12F.
               10  AGRUP12A PIC  X(0001).
           05  AGRUP12I PIC  X(0006).
      *    -------------------------------
           05  AFINR12L PIC S9(0004) COMP.
           05  AFINR12F PIC  X(0001).
           05  FILLER REDEFINES AFINR12F.
               10  AFINR12A PIC  X(0001).
           05  AFINR12I PIC  X(0010).
      *    -------------------------------
           05  AACCT12L PIC S9(0004) COMP.
           05  AACCT12F PIC  X(0001).
           05  FILLER REDEFINES AACCT12F.
               10  AACCT12A PIC  X(0001).
           05  AACCT12I PIC  X(0010).
      *    -------------------------------
           05  APAMT12L PIC S9(0004) COMP.
           05  APAMT12F PIC  X(0001).
           05  FILLER REDEFINES APAMT12F.
               10  APAMT12A PIC  X(0001).
           05  APAMT12I PIC  X(0013).
      *    -------------------------------
           05  ACTRL13L PIC S9(0004) COMP.
           05  ACTRL13F PIC  X(0001).
           05  FILLER REDEFINES ACTRL13F.
               10  ACTRL13A PIC  X(0001).
           05  ACTRL13I PIC  X(0007).
      *    -------------------------------
           05  ACKNO13L PIC S9(0004) COMP.
           05  ACKNO13F PIC  X(0001).
           05  FILLER REDEFINES ACKNO13F.
               10  ACKNO13A PIC  X(0001).
           05  ACKNO13I PIC  X(0007).
      *    -------------------------------
           05  ATYPE13L PIC S9(0004) COMP.
           05  ATYPE13F PIC  X(0001).
           05  FILLER REDEFINES ATYPE13F.
               10  ATYPE13A PIC  X(0001).
           05  ATYPE13I PIC  X(0011).
      *    -------------------------------
           05  ACARR13L PIC S9(0004) COMP.
           05  ACARR13F PIC  X(0001).
           05  FILLER REDEFINES ACARR13F.
               10  ACARR13A PIC  X(0001).
           05  ACARR13I PIC  X(0001).
      *    -------------------------------
           05  AGRUP13L PIC S9(0004) COMP.
           05  AGRUP13F PIC  X(0001).
           05  FILLER REDEFINES AGRUP13F.
               10  AGRUP13A PIC  X(0001).
           05  AGRUP13I PIC  X(0006).
      *    -------------------------------
           05  AFINR13L PIC S9(0004) COMP.
           05  AFINR13F PIC  X(0001).
           05  FILLER REDEFINES AFINR13F.
               10  AFINR13A PIC  X(0001).
           05  AFINR13I PIC  X(0010).
      *    -------------------------------
           05  AACCT13L PIC S9(0004) COMP.
           05  AACCT13F PIC  X(0001).
           05  FILLER REDEFINES AACCT13F.
               10  AACCT13A PIC  X(0001).
           05  AACCT13I PIC  X(0010).
      *    -------------------------------
           05  APAMT13L PIC S9(0004) COMP.
           05  APAMT13F PIC  X(0001).
           05  FILLER REDEFINES APAMT13F.
               10  APAMT13A PIC  X(0001).
           05  APAMT13I PIC  X(0013).
      *    -------------------------------
           05  ACTRL14L PIC S9(0004) COMP.
           05  ACTRL14F PIC  X(0001).
           05  FILLER REDEFINES ACTRL14F.
               10  ACTRL14A PIC  X(0001).
           05  ACTRL14I PIC  X(0007).
      *    -------------------------------
           05  ACKNO14L PIC S9(0004) COMP.
           05  ACKNO14F PIC  X(0001).
           05  FILLER REDEFINES ACKNO14F.
               10  ACKNO14A PIC  X(0001).
           05  ACKNO14I PIC  X(0007).
      *    -------------------------------
           05  ATYPE14L PIC S9(0004) COMP.
           05  ATYPE14F PIC  X(0001).
           05  FILLER REDEFINES ATYPE14F.
               10  ATYPE14A PIC  X(0001).
           05  ATYPE14I PIC  X(0011).
      *    -------------------------------
           05  ACARR14L PIC S9(0004) COMP.
           05  ACARR14F PIC  X(0001).
           05  FILLER REDEFINES ACARR14F.
               10  ACARR14A PIC  X(0001).
           05  ACARR14I PIC  X(0001).
      *    -------------------------------
           05  AGRUP14L PIC S9(0004) COMP.
           05  AGRUP14F PIC  X(0001).
           05  FILLER REDEFINES AGRUP14F.
               10  AGRUP14A PIC  X(0001).
           05  AGRUP14I PIC  X(0006).
      *    -------------------------------
           05  AFINR14L PIC S9(0004) COMP.
           05  AFINR14F PIC  X(0001).
           05  FILLER REDEFINES AFINR14F.
               10  AFINR14A PIC  X(0001).
           05  AFINR14I PIC  X(0010).
      *    -------------------------------
           05  AACCT14L PIC S9(0004) COMP.
           05  AACCT14F PIC  X(0001).
           05  FILLER REDEFINES AACCT14F.
               10  AACCT14A PIC  X(0001).
           05  AACCT14I PIC  X(0010).
      *    -------------------------------
           05  APAMT14L PIC S9(0004) COMP.
           05  APAMT14F PIC  X(0001).
           05  FILLER REDEFINES APAMT14F.
               10  APAMT14A PIC  X(0001).
           05  APAMT14I PIC  X(0013).
      *    -------------------------------
           05  ACTRL15L PIC S9(0004) COMP.
           05  ACTRL15F PIC  X(0001).
           05  FILLER REDEFINES ACTRL15F.
               10  ACTRL15A PIC  X(0001).
           05  ACTRL15I PIC  X(0007).
      *    -------------------------------
           05  ACKNO15L PIC S9(0004) COMP.
           05  ACKNO15F PIC  X(0001).
           05  FILLER REDEFINES ACKNO15F.
               10  ACKNO15A PIC  X(0001).
           05  ACKNO15I PIC  X(0007).
      *    -------------------------------
           05  ATYPE15L PIC S9(0004) COMP.
           05  ATYPE15F PIC  X(0001).
           05  FILLER REDEFINES ATYPE15F.
               10  ATYPE15A PIC  X(0001).
           05  ATYPE15I PIC  X(0011).
      *    -------------------------------
           05  ACARR15L PIC S9(0004) COMP.
           05  ACARR15F PIC  X(0001).
           05  FILLER REDEFINES ACARR15F.
               10  ACARR15A PIC  X(0001).
           05  ACARR15I PIC  X(0001).
      *    -------------------------------
           05  AGRUP15L PIC S9(0004) COMP.
           05  AGRUP15F PIC  X(0001).
           05  FILLER REDEFINES AGRUP15F.
               10  AGRUP15A PIC  X(0001).
           05  AGRUP15I PIC  X(0006).
      *    -------------------------------
           05  AFINR15L PIC S9(0004) COMP.
           05  AFINR15F PIC  X(0001).
           05  FILLER REDEFINES AFINR15F.
               10  AFINR15A PIC  X(0001).
           05  AFINR15I PIC  X(0010).
      *    -------------------------------
           05  AACCT15L PIC S9(0004) COMP.
           05  AACCT15F PIC  X(0001).
           05  FILLER REDEFINES AACCT15F.
               10  AACCT15A PIC  X(0001).
           05  AACCT15I PIC  X(0010).
      *    -------------------------------
           05  APAMT15L PIC S9(0004) COMP.
           05  APAMT15F PIC  X(0001).
           05  FILLER REDEFINES APAMT15F.
               10  APAMT15A PIC  X(0001).
           05  APAMT15I PIC  X(0013).
      *    -------------------------------
           05  ACTRL16L PIC S9(0004) COMP.
           05  ACTRL16F PIC  X(0001).
           05  FILLER REDEFINES ACTRL16F.
               10  ACTRL16A PIC  X(0001).
           05  ACTRL16I PIC  X(0007).
      *    -------------------------------
           05  ACKNO16L PIC S9(0004) COMP.
           05  ACKNO16F PIC  X(0001).
           05  FILLER REDEFINES ACKNO16F.
               10  ACKNO16A PIC  X(0001).
           05  ACKNO16I PIC  X(0007).
      *    -------------------------------
           05  ATYPE16L PIC S9(0004) COMP.
           05  ATYPE16F PIC  X(0001).
           05  FILLER REDEFINES ATYPE16F.
               10  ATYPE16A PIC  X(0001).
           05  ATYPE16I PIC  X(0011).
      *    -------------------------------
           05  ACARR16L PIC S9(0004) COMP.
           05  ACARR16F PIC  X(0001).
           05  FILLER REDEFINES ACARR16F.
               10  ACARR16A PIC  X(0001).
           05  ACARR16I PIC  X(0001).
      *    -------------------------------
           05  AGRUP16L PIC S9(0004) COMP.
           05  AGRUP16F PIC  X(0001).
           05  FILLER REDEFINES AGRUP16F.
               10  AGRUP16A PIC  X(0001).
           05  AGRUP16I PIC  X(0006).
      *    -------------------------------
           05  AFINR16L PIC S9(0004) COMP.
           05  AFINR16F PIC  X(0001).
           05  FILLER REDEFINES AFINR16F.
               10  AFINR16A PIC  X(0001).
           05  AFINR16I PIC  X(0010).
      *    -------------------------------
           05  AACCT16L PIC S9(0004) COMP.
           05  AACCT16F PIC  X(0001).
           05  FILLER REDEFINES AACCT16F.
               10  AACCT16A PIC  X(0001).
           05  AACCT16I PIC  X(0010).
      *    -------------------------------
           05  APAMT16L PIC S9(0004) COMP.
           05  APAMT16F PIC  X(0001).
           05  FILLER REDEFINES APAMT16F.
               10  APAMT16A PIC  X(0001).
           05  APAMT16I PIC  X(0013).
      *    -------------------------------
           05  AVYTL17L PIC S9(0004) COMP.
           05  AVYTL17F PIC  X(0001).
           05  FILLER REDEFINES AVYTL17F.
               10  AVYTL17A PIC  X(0001).
           05  AVYTL17I PIC  X(0007).
      *    -------------------------------
           05  ACKNO17L PIC S9(0004) COMP.
           05  ACKNO17F PIC  X(0001).
           05  FILLER REDEFINES ACKNO17F.
               10  ACKNO17A PIC  X(0001).
           05  ACKNO17I PIC  X(0007).
      *    -------------------------------
           05  ATYPE17L PIC S9(0004) COMP.
           05  ATYPE17F PIC  X(0001).
           05  FILLER REDEFINES ATYPE17F.
               10  ATYPE17A PIC  X(0001).
           05  ATYPE17I PIC  X(0011).
      *    -------------------------------
           05  ACARR17L PIC S9(0004) COMP.
           05  ACARR17F PIC  X(0001).
           05  FILLER REDEFINES ACARR17F.
               10  ACARR17A PIC  X(0001).
           05  ACARR17I PIC  X(0001).
      *    -------------------------------
           05  AGRUP17L PIC S9(0004) COMP.
           05  AGRUP17F PIC  X(0001).
           05  FILLER REDEFINES AGRUP17F.
               10  AGRUP17A PIC  X(0001).
           05  AGRUP17I PIC  X(0006).
      *    -------------------------------
           05  AFINR17L PIC S9(0004) COMP.
           05  AFINR17F PIC  X(0001).
           05  FILLER REDEFINES AFINR17F.
               10  AFINR17A PIC  X(0001).
           05  AFINR17I PIC  X(0010).
      *    -------------------------------
           05  AACCT17L PIC S9(0004) COMP.
           05  AACCT17F PIC  X(0001).
           05  FILLER REDEFINES AACCT17F.
               10  AACCT17A PIC  X(0001).
           05  AACCT17I PIC  X(0010).
      *    -------------------------------
           05  APAMT17L PIC S9(0004) COMP.
           05  APAMT17F PIC  X(0001).
           05  FILLER REDEFINES APAMT17F.
               10  APAMT17A PIC  X(0001).
           05  APAMT17I PIC  X(0013).
      *    -------------------------------
           05  ACTRL18L PIC S9(0004) COMP.
           05  ACTRL18F PIC  X(0001).
           05  FILLER REDEFINES ACTRL18F.
               10  ACTRL18A PIC  X(0001).
           05  ACTRL18I PIC  X(0007).
      *    -------------------------------
           05  ACKNO18L PIC S9(0004) COMP.
           05  ACKNO18F PIC  X(0001).
           05  FILLER REDEFINES ACKNO18F.
               10  ACKNO18A PIC  X(0001).
           05  ACKNO18I PIC  X(0007).
      *    -------------------------------
           05  ATYPE18L PIC S9(0004) COMP.
           05  ATYPE18F PIC  X(0001).
           05  FILLER REDEFINES ATYPE18F.
               10  ATYPE18A PIC  X(0001).
           05  ATYPE18I PIC  X(0011).
      *    -------------------------------
           05  ACARR18L PIC S9(0004) COMP.
           05  ACARR18F PIC  X(0001).
           05  FILLER REDEFINES ACARR18F.
               10  ACARR18A PIC  X(0001).
           05  ACARR18I PIC  X(0001).
      *    -------------------------------
           05  AGRUP18L PIC S9(0004) COMP.
           05  AGRUP18F PIC  X(0001).
           05  FILLER REDEFINES AGRUP18F.
               10  AGRUP18A PIC  X(0001).
           05  AGRUP18I PIC  X(0006).
      *    -------------------------------
           05  AFINR18L PIC S9(0004) COMP.
           05  AFINR18F PIC  X(0001).
           05  FILLER REDEFINES AFINR18F.
               10  AFINR18A PIC  X(0001).
           05  AFINR18I PIC  X(0010).
      *    -------------------------------
           05  AACCT18L PIC S9(0004) COMP.
           05  AACCT18F PIC  X(0001).
           05  FILLER REDEFINES AACCT18F.
               10  AACCT18A PIC  X(0001).
           05  AACCT18I PIC  X(0010).
      *    -------------------------------
           05  APAMT18L PIC S9(0004) COMP.
           05  APAMT18F PIC  X(0001).
           05  FILLER REDEFINES APAMT18F.
               10  APAMT18A PIC  X(0001).
           05  APAMT18I PIC  X(0013).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  9(2).
      *    -------------------------------
           05  PRINTERL PIC S9(0004) COMP.
           05  PRINTERF PIC  X(0001).
           05  FILLER REDEFINES PRINTERF.
               10  PRINTERA PIC  X(0001).
           05  PRINTERI PIC  X(0004).
      *    -------------------------------
           05  APFDESCL PIC S9(0004) COMP.
           05  APFDESCF PIC  X(0001).
           05  FILLER REDEFINES APFDESCF.
               10  APFDESCA PIC  X(0001).
           05  APFDESCI PIC  X(0029).
      *    -------------------------------
           05  CNTLNODL PIC S9(0004) COMP.
           05  CNTLNODF PIC  X(0001).
           05  FILLER REDEFINES CNTLNODF.
               10  CNTLNODA PIC  X(0001).
           05  CNTLNODI PIC  X(0011).
      *    -------------------------------
           05  CNTLNOL PIC S9(0004) COMP.
           05  CNTLNOF PIC  X(0001).
           05  FILLER REDEFINES CNTLNOF.
               10  CNTLNOA PIC  X(0001).
           05  CNTLNOI PIC  X(0007).
       01  EL685AO REDEFINES EL685BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATITLEO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASCREENO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAGEO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADESC2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE01O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP01O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT01O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE02O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP02O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT02O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE03O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP03O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT03O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE04O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP04O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT04O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE05O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP05O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT05O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE06O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP06O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT06O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE07O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP07O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT07O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE08O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP08O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT08O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE09O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP09O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR09O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT09O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT09O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE10O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT10O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE11O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP11O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT11O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE12O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP12O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT12O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP13O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT13O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE14O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP14O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT14O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE15O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP15O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT15O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE16O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP16O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR16O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT16O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT16O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AVYTL17O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO17O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE17O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP17O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR17O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT17O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT17O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTRL18O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNO18O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATYPE18O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARR18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRUP18O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFINR18O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT18O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAMT18O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINTERO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFDESCO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNTLNODO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNTLNOO PIC  X(0007).
      *    -------------------------------
00302
00303  01  FILLER                          REDEFINES
00304      EL685BI.
00305
00306      05  FILLER                      PIC X(78).
00307
00308      05  FILLER                      OCCURS 18 TIMES
00309                                      INDEXED BY EL685B-INDEX.
00310
00311          15  EL685B-CONTROL-LENGTH   PIC S9(4)
00312                                      COMP.
00313          15  EL685B-CONTROL-ATTRB    PIC X.
00314          15  EL685B-CONTROL          PIC 9(7).
00315
00316          15  EL685B-CHECK-NO-LENGTH  PIC S9(4)
00317                                      COMP.
00318          15  EL685B-CHECK-NO-ATTRB   PIC X.
00319          15  EL685B-CHECK-NO         PIC X(6).
00320
00321          15  EL685B-CHK-DT-LENGTH    PIC S9(4)
00322                                      COMP.
00323          15  EL685B-CHK-DT-ATTRB     PIC X.
00324          15  EL685B-CHK-DT           PIC X(08).
00325
00326          15  EL685B-CARRIER-LENGTH   PIC S9(4)
00327                                      COMP.
00328          15  EL685B-CARRIER-ATTRB    PIC X.
00329          15  EL685B-CARRIER          PIC X.
00330
00331          15  EL685B-GROUPING-LENGTH  PIC S9(4)
00332                                      COMP.
00333          15  EL685B-GROUPING-ATTRB   PIC X.
00334          15  EL685B-GROUPING         PIC X(6).
00335          15  EL685B-GROUPING-RDF
00336              REDEFINES EL685B-GROUPING.
00337              20  EL685B-DESC-ONE     PIC X(6).
00338
00339          15  EL685B-PAYEE-LENGTH     PIC S9(4)
00340                                      COMP.
00341          15  EL685B-PAYEE-ATTRB      PIC X.
00342          15  EL685B-PAYEE            PIC X(10).
00343          15  EL685B-PAYEE-RDF
00344              REDEFINES EL685B-PAYEE.
00345              20  EL685B-RDF-AREA.
00346                  25  EL685B-DESC-TWO PIC X(5).
00347                  25  FILLER          PIC X(5).
00348
00349          15  EL685B-PAYEE-SEQ-LENGTH PIC S9(4)
00350                                      COMP.
00351          15  EL685B-PAYEE-SEQ-ATTRB  PIC X.
00352          15  EL685B-PAYEE-SEQ        PIC ZZZ9.
00353
00354          15  EL685B-PAYEE-NA-LENGTH  PIC S9(4)
00355                                      COMP.
00356          15  EL685B-PAYEE-NA-ATTRB   PIC X.
00357          15  EL685B-PAYEE-NA         PIC X(12).
00358
00359          15  EL685B-AMT-LENGTH       PIC S9(4)
00360                                      COMP.
00361          15  EL685B-AMT-ATTRB        PIC X.
00362          15  EL685B-AMT              PIC Z,ZZZ,ZZ9.99-.
00363
00364  01  FILLER                          REDEFINES
00365      EL685AI.
00366
00367      05  FILLER                      PIC X(100).
00368
00369      05  FILLER                      OCCURS 18 TIMES
00370                                      INDEXED BY EL685A-INDEX.
00371
00372          15  EL685A-CONTROL-LENGTH   PIC S9(4)
00373                                      COMP.
00374          15  EL685A-CONTROL-ATTRB    PIC X.
00375          15  EL685A-CONTROL          PIC 9(7).
00376
00377          15  EL685A-CHECK-NO-LENGTH  PIC S9(4)
00378                                      COMP.
00379          15  EL685A-CHECK-NO-ATTRB   PIC X.
00380          15  EL685A-CHECK-NO         PIC X(7).
00381
00382          15  EL685A-PMT-TYPE-LENGTH  PIC S9(4)
00383                                      COMP.
00384          15  EL685A-PMT-TYPE-ATTRB   PIC X.
00385          15  EL685A-PMT-TYPE         PIC X(11).
00386
00387          15  EL685A-CARRIER-LENGTH   PIC S9(4)
00388                                      COMP.
00389          15  EL685A-CARRIER-ATTRB    PIC X.
00390          15  EL685A-CARRIER          PIC X.
00391
00392          15  EL685A-GROUPING-LENGTH  PIC S9(4)
00393                                      COMP.
00394          15  EL685A-GROUPING-ATTRB   PIC X.
00395          15  EL685A-GROUPING         PIC X(6).
00396
00397          15  EL685A-FIN-RESP-LENGTH  PIC S9(4)
00398                                      COMP.
00399          15  EL685A-FIN-RESP-ATTRB   PIC X.
00400          15  EL685A-FIN-RESP         PIC X(10).
00401          15  EL685A-FIN-RESP-RDF
00402              REDEFINES EL685A-FIN-RESP.
00403              20  EL685A-PAYEE        PIC X(10).
00404
00405          15  EL685A-ACCOUNT-LENGTH   PIC S9(4)
00406                                      COMP.
00407          15  EL685A-ACCOUNT-ATTRB    PIC X.
00408          15  EL685A-ACCOUNT          PIC X(10).
00409          15  EL685A-ACCOUNT-RDF
00410              REDEFINES EL685A-ACCOUNT.
00411              20  EL685A-PAYEE-SEQ    PIC ZZZ9-.
00412              20  FILLER              PIC X(05).
00413          15  EL685A-AMT-LENGTH       PIC S9(4)
00414                                      COMP.
00415          15  EL685A-AMT-ATTRB        PIC X.
00416          15  EL685A-AMT              PIC Z,ZZZ,ZZ9.99-.
00417
00418      EJECT
00419 *                                COPY ELCEMIB.
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
00420      EJECT
00421 *                                COPY ELCDATE.
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
00422      EJECT
00423 *                                COPY ELCLOGOF.
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
00424      EJECT
00425 *                                COPY ELCATTR.
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
00426      EJECT
00427 *                                COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '?'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00428
00429  01  FILLER                      REDEFINES
00430      DFHAID.
00431
00432      05  FILLER                      PIC X(8).
00433
00434      05  PF-VALUES                   PIC X
00435          OCCURS 24 TIMES.
00436      EJECT
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
00438
00439  01  DFHCOMMAREA                     PIC X(1024).
00440
S0441      EJECT
00442 *                                    COPY ERCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE CREDIT SYSTEM      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH  = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID                     VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-ENTRY-TYPE               PIC X.
00028              88  CHECK-ON-QUE           VALUE 'Q'.
00029              88  ALIGNMENT-CHECK        VALUE 'A'.
00030              88  MANUAL-CHECK           VALUE 'M'.
00031              88  SPOILED-CHECK          VALUE 'S'.
00032              88  VOIDED-CHECK           VALUE 'V'.
00033              88  PAYMENT-ABORTED        VALUE 'X'.
00034
00035      12  CQ-CREDIT-MASTER-CNTL       PIC X(50).
00036
00037      12  CQ-CREDIT-PYAJ-CNTL         REDEFINES
00038          CQ-CREDIT-MASTER-CNTL.
00039          16  CQ-PYAJ-CARRIER         PIC X.
00040          16  CQ-PYAJ-GROUPING        PIC X(6).
00041          16  CQ-PYAJ-FIN-RESP        PIC X(10).
00042          16  CQ-PYAJ-ACCOUNT         PIC X(10).
00043          16  CQ-PYAJ-SEQ             PIC S9(8)  COMP.
00044          16  CQ-PYAJ-REC-TYPE        PIC X.
00045          16  FILLER                  PIC X(18).
00046
00047      12  CQ-CREDIT-CHEK-CNTL         REDEFINES
00048          CQ-CREDIT-MASTER-CNTL.
00049          16  CQ-CHEK-CARRIER         PIC X.
00050          16  CQ-CHEK-GROUPING        PIC X(6).
00051          16  CQ-CHEK-STATE           PIC XX.
00052          16  CQ-CHEK-ACCOUNT         PIC X(10).
00053          16  CQ-CHEK-CERT-EFF-DT     PIC XX.
00054          16  CQ-CHEK-CERT-NO.
00055              20  CQ-CHEK-CERT-PRIME  PIC X(10).
00056              20  CQ-CHEK-CERT-SFX    PIC X.
00057          16  CQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00058          16  CQ-CHEK-FIN-RESP        PIC X(10).
00059          16  FILLER                  PIC X(06).
00060
00061      12  CQ-CHECK-NUMBER             PIC X(7).
00062      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00063      12  CQ-PAYMENT-TYPE             PIC X.
00064              88  CQ-BILLING-CREDIT         VALUE '1'.
00065              88  CQ-REFUND-PMT             VALUE '2'.
00066              88  CQ-CHECK-MAINT-PMT        VALUE '3'.
00067      12  CQ-VOID-INDICATOR           PIC X.
00068              88  CHECK-IS-VOID             VALUE 'V'.
00069      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00070      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00071      12  CQ-CHECK-BY-USER            PIC X(4).
00072      12  CQ-PRE-NUMBERING-SW         PIC X.
00073        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00074        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00075
00076      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00077      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00078      12  CQ-ACCOUNT-AGENT            PIC X(10).
00079      12  CQ-CHECK-VOIDED-DT          PIC XX.
00080
00081      12  CQ-LETTERS-IND              PIC X.
00082          88  CQ-LETTERS-REQUIRED           VALUE 'Y'.
00083
00084 ******************************************************************
00443      EJECT
00444 *                                    COPY ELCCNTL.
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
00445      EJECT
00446 *                                    COPY ERCCMKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCMKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE COMMISSION         *
00008 *                      CHECK OF THE CREDIT SYSTEM                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 1800 RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERCMKQ                         RKP=2,LEN=7    *
00014 *       ALTERNATE PATH  = ERCMKQ2  (BY PAYEE CONTRO AND          *
00015 *                                      CONTROL NUMBER)           *
00016 *                                                 RKP=9,LEN=30   *
00017 *                                                                *
00018 *   LOG = NO                                                     *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
00021  01  COMMISSION-CHECK-QUE.
00022      12  MQ-RECORD-ID                PIC XX.
00023          88  VALID-MQ-ID                     VALUE 'MQ'.
00024
00025      12  MQ-CONTROL-PRIMARY.
00026          16  MQ-COMPANY-CD           PIC X.
00027          16  MQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00028          16  MQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00029
00030      12  MQ-CONTROL-BY-PAYEE.
00031          16  MQ-COMPANY-CD-A1        PIC X.
00032          16  MQ-CSR-A1               PIC X(4).
00033          16  MQ-CARRIER-A1           PIC X.
00034          16  MQ-GROUPING-A1          PIC X(6).
00035          16  MQ-PAYEE-A1             PIC X(10).
00036          16  MQ-PAYEE-SEQ-A1         PIC S9(4)       COMP.
00037          16  MQ-CONTROL-NUMBER-A1    PIC S9(8)       COMP.
00038          16  MQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00039
00040      12  MQ-ENTRY-TYPE               PIC X.
00041              88  CHECK-ON-QUE           VALUE 'Q'.
00042              88  ALIGNMENT-CHECK        VALUE 'A'.
00043              88  SPOILED-CHECK          VALUE 'S'.
00044              88  PAYMENT-ABORTED        VALUE 'X'.
00045              88  ACH-PAYMENT            VALUE 'P'.
00046
00047      12  FILLER                      PIC X(10).
00048
00049      12  MQ-CREDIT-CHEK-CNTL.
00050          16  MQ-CHEK-CSR             PIC X(4).
00051          16  MQ-CHEK-CARRIER         PIC X.
00052          16  MQ-CHEK-GROUPING        PIC X(6).
00053          16  MQ-CHEK-PAYEE           PIC X(10).
00054          16  MQ-CHEK-PAYEE-SEQ       PIC S9(4)       COMP.
00055          16  MQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00056
00057      12  FILLER                      PIC X(10).
00058
00059      12  MQ-PAYEE-INFO.
00060          16  MQ-PAYEE-NAME           PIC X(30).
00061          16  MQ-PAYEE-ADDRESS-1      PIC X(30).
00062          16  MQ-PAYEE-ADDRESS-2      PIC X(30).
00063          16  MQ-PAYEE-CITY-ST        PIC X(30).
00064          16  MQ-PAYEE-ZIP-CODE.
00065              20  MQ-PAYEE-ZIP.
00066                  24  FILLER          PIC X(1).
00067                      88 MQ-PAYEE-CANADIAN-POST-CODE
00068                                      VALUE 'A' THRU 'Z'.
00069                  24  FILLER          PIC X(4).
00070              20  MQ-PAYEE-ZIP-EXT    PIC X(4).
00071          16  MQ-PAYEE-CANADIAN-POSTAL-CODES
00072                  REDEFINES MQ-PAYEE-ZIP-CODE.
00073              20  MQ-PAY-CAN-POSTAL-CD-1
00074                                      PIC X(3).
00075              20  MQ-PAY-CAN-POSTAL-CD-2
00076                                      PIC X(3).
00077              20  FILLER              PIC X(3).
00078
00079      12  MQ-CREDIT-PYAJ-CNTL.
00080          16  MQ-PYAJ-CARRIER         PIC X.
00081          16  MQ-PYAJ-GROUPING        PIC X(6).
00082          16  MQ-PYAJ-FIN-RESP        PIC X(10).
00083          16  FILLER                  PIC X(6).
00084
00085      12  MQ-CHECK-NUMBER             PIC X(6).
00086      12  MQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00087      12  MQ-NUMBER-OF-CK-STUBS       PIC S9(3)       COMP-3.
00088      12  MQ-VOID-DT                  PIC XX.
00089      12  MQ-TIMES-PRINTED            PIC S9(4)       COMP.
00090      12  MQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00091      12  MQ-CHECK-BY-USER            PIC X(4).
00092      12  MQ-PRE-NUMBERING-SW         PIC X.
00093        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00094        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00095
00096      12  MQ-CHECK-WRITTEN-DT         PIC XX.
00097      12  MQ-ACH-WRITTEN-DT REDEFINES  MQ-CHECK-WRITTEN-DT
00098                                      PIC XX.
00099      12  MQ-LAST-MAINT-BY            PIC X(4).
00100      12  MQ-LAST-MAINT-HHMMSS        PIC S9(6)       COMP-3.
00101      12  MQ-LAST-MAINT-DT            PIC XX.
00102      12  MQ-CHECK-RELEASE-DT         PIC XX.
00103      12  MQ-RECORD-TYPE              PIC X.
00104          88  MQ-DETAIL                     VALUE 'D'.
00105          88  MQ-TEXT                       VALUE 'T'.
00106
00107      12  MQ-DETAIL-INFORMATION.
00108          16  MQ-DETAIL-INFO        OCCURS 15 TIMES.
00109              20  MQ-CHECK-STUB-LINE.
00110                  24  MQ-STUB-COMMENT        PIC X(23).
00111                  24  MQ-ACCT-AGENT          PIC X(10).
00112                  24  MQ-INVOICE             PIC X(6).
00113                  24  MQ-REFERENCE           PIC X(12).
00114                  24  MQ-LEDGER-NO           PIC X(14).
00115                  24  MQ-PYAJ-AMT            PIC S9(7)V99 COMP-3.
00116                  24  MQ-PYAJ-REC-TYPE       PIC X.
00117                  24  MQ-PYAJ-SEQ            PIC S9(8)    COMP.
00118                  24  MQ-PAYMENT-TYPE        PIC X.
00119                  24  MQ-PYAJ-PMT-APPLIED    PIC X.
00120                  24  MQ-LAST-MAINT-APPLIED  PIC X.
00121                  24  MQ-NON-AR-ITEM         PIC X.
00122                  24  FILLER                 PIC X(19).
00123
00124      12  MQ-CHECK-STUB-TEXT REDEFINES MQ-DETAIL-INFORMATION.
00125          16  MQ-CHECK-TEXT-ITEMS   OCCURS 3 TIMES.
00126              20  MQ-STUB-TEXT        PIC X(70).
00127          16  MQ-STUB-FILLER          PIC X(1260).
00128
00129      12  MQ-CREDIT-SELECT-DATE       PIC XX.
00130      12  MQ-CREDIT-ACCEPT-DATE       PIC XX.
00131
00132      12  MQ-AR-STATEMENT-DT          PIC XX.
00133      12  MQ-CO-TYPE                  PIC X.
00134
00135      12  MQ-STARTING-CHECK-NUMBER    PIC X(06).
00136      12  FILLER                      PIC X(41).
00137 ******************************************************************
00447      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-QUE
                                CONTROL-FILE COMMISSION-CHECK-QUE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL685' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00449
00450      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00451
00452 *    NOTE *******************************************************
00453 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00454 *         *  FROM ANOTHER MODULE.                               *
00455 *         *******************************************************.
00456
00457      IF EIBCALEN NOT GREATER THAN ZERO
00458          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00459          GO TO 8300-SEND-TEXT.
00460
00461      
      * EXEC CICS HANDLE CONDITION
00462 *        PGMIDERR (9600-PGMIDERR)
00463 *        ERROR    (9990-ERROR)
00464 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005967' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035393637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00465
00466      EJECT
00467  0010-MAIN-LOGIC.
00468      IF PI-CALLED-FROM-AR-MENU
00469          MOVE 'EL685B' TO WS-MAP-NAME.
00470
00471      IF PI-CALLING-PROGRAM NOT = WS-PROGRAM-ID
00472          IF PI-RETURN-TO-PROGRAM NOT = WS-PROGRAM-ID
00473              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00474              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00475              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00476              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00477              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00478              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00479              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00480              MOVE WS-PROGRAM-ID        TO  PI-CALLING-PROGRAM
00481            ELSE
00482              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00483              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00484              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00485              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00486              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00487              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00488              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00489              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00490      ELSE
00491          GO TO 0020-MAIN-LOGIC.
00492
00493
00494  0015-MAIN-LOGIC.
00495
00496
00497 *    NOTE *******************************************************
00498 *         *                                                     *
00499 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00500 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00501 *         *                                                     *
00502 *         *******************************************************.
00503
00504      
      * EXEC CICS HANDLE CONDITION
00505 *        QIDERR (0015-NEXT-SENTENCE)
00506 *    END-EXEC.
      *    MOVE '"$N                   ! # #00006010' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036303130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00507
00508      MOVE EIBTRMID               TO  WS-TS-TERM-ID.
00509
00510      
      * EXEC CICS DELETEQ TS
00511 *        QUEUE (WS-TEMP-STORAGE-KEY)
00512 *    END-EXEC.
      *    MOVE '*&                    #   #00006016' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00513
00514  0015-NEXT-SENTENCE.
00515
00516      MOVE PI-AR-MODE             TO  WS-SAVE-AR-MODE.
00517      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
00518
00519      MOVE LOW-VALUES             TO  PI-CHECK-QUE-KEY
00520                                      PI-PREV-CHECK-QUE-KEY
00521
00522      MOVE ZERO                   TO  PI-END-OF-FILE
00523                                      PI-TEMP-STORAGE-ITEM
00524                                      PI-CONTROL-TOT
00525                                      PI-CONTROL-SAVE-CONTROL
00526                                      PI-START-CONTROL-NO
00527                                      PI-CONTROL-GRAND-TOT.
00528      MOVE WS-SAVE-CHECK-MODE     TO  PI-CHECK-MODE.
00529      MOVE WS-SAVE-AR-MODE        TO  PI-AR-MODE.
00530      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CODE
00531      MOVE 'Y'                    TO  PI-FIRST-TIME-SW
00532      MOVE 'Y'                    TO  PI-FIRST-TOTAL-SW
00533      MOVE 'N'                    TO  PI-SEND-TOT-SWT
00534      MOVE 'N'                    TO  PI-EOF-SWT
00535      INITIALIZE HOLD-CHECK-RECORD.
00536
00537      IF WS-START-CNTLNO GREATER ZERO
00538          MOVE WS-START-CNTLNO    TO  PI-CK-CONTROL-NO.
00539
00540      MOVE EIBDATE                TO  DC-JULIAN-YYDDD
00541      MOVE '5'                    TO  DC-OPTION-CODE
00542      PERFORM 8500-DATE-CONVERSION
00543      MOVE DC-BIN-DATE-1          TO  PI-CURRENT-DATE-BIN
00544      MOVE DC-GREG-DATE-1-EDIT    TO  PI-CURRENT-DATE.
00545
00546      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.
00547
00548      EJECT
00549  0020-MAIN-LOGIC.
00550
00551
00552 *    NOTE *******************************************************
00553 *         *                                                     *
00554 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00555 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00556 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00557 *         *                                                     *
00558 *         *******************************************************.
00559
00560      IF EIBAID = DFHCLEAR
00561          GO TO 9400-CLEAR.
00562
00563      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00564          MOVE +8                 TO  EMI-ERROR
00565          MOVE -1                 TO  APFKL
00566          PERFORM 8200-SEND-DATAONLY.
00567
00568      IF PI-CALLED-FROM-AR-MENU
00569          
      * EXEC CICS RECEIVE
00570 *            INTO   (EL685BI)
00571 *            MAPSET (WS-MAPSET-NAME)
00572 *            MAP    (WS-MAP-NAME)
00573 *        END-EXEC
           MOVE LENGTH OF
            EL685BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00006075' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL685BI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00574       ELSE
00575          
      * EXEC CICS RECEIVE
00576 *            INTO   (EL685AI)
00577 *            MAPSET (WS-MAPSET-NAME)
00578 *            MAP    (WS-MAP-NAME)
00579 *        END-EXEC.
           MOVE LENGTH OF
            EL685AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00006081' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL685AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00580
00581      IF PI-CALLED-FROM-AR-MENU       AND
00582            BPFKL IS GREATER THAN ZERO
00583            IF EIBAID NOT = DFHENTER
00584                MOVE +4             TO  EMI-ERROR
00585                MOVE AL-UNBOF       TO  BPFKA
00586                MOVE -1             TO  BPFKL
00587                PERFORM 8200-SEND-DATAONLY
00588            ELSE
00589              IF BPFKO IS NUMERIC
00590                AND BPFKO IS GREATER THAN ZERO
00591                AND BPFKO IS LESS THAN '25'
00592                  MOVE PF-VALUES (BPFKI)  TO  EIBAID
00593                ELSE
00594                  MOVE +29            TO  EMI-ERROR
00595                  MOVE AL-UNBOF       TO  BPFKA
00596                  MOVE -1             TO  BPFKL
00597                  PERFORM 8200-SEND-DATAONLY.
00598
00599      IF NOT PI-CALLED-FROM-AR-MENU   AND
00600            APFKL IS GREATER THAN ZERO
00601            IF EIBAID NOT = DFHENTER
00602                MOVE +4             TO  EMI-ERROR
00603                MOVE AL-UNBOF       TO  APFKA
00604                MOVE -1             TO  APFKL
00605                PERFORM 8200-SEND-DATAONLY
00606            ELSE
00607              IF APFKO IS NUMERIC
00608                AND APFKO IS GREATER THAN ZERO
00609                AND APFKO IS LESS THAN '25'
00610                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00611                ELSE
00612                  MOVE +29            TO  EMI-ERROR
00613                  MOVE AL-UNBOF       TO  APFKA
00614                  MOVE -1             TO  APFKL
00615                  PERFORM 8200-SEND-DATAONLY.
00616
00617      IF EIBAID IS = DFHPF12
00618          MOVE 'EL010   '         TO  WS-PROGRAM-ID
00619          GO TO 9300-XCTL.
00620
00621      IF EIBAID IS = DFHPF23
00622          GO TO 9000-RETURN-CICS.
00623
00624      IF EIBAID IS = DFHPF24
00625          MOVE 'EL126   '         TO  WS-PROGRAM-ID
00626          GO TO 9300-XCTL.
00627
00628      IF EIBAID = (DFHENTER OR DFHPF1 OR DFHPF2
00629                            OR DFHPF3 OR DFHPF4)
00630          NEXT SENTENCE
00631        ELSE
00632          MOVE +8                 TO  EMI-ERROR
00633          MOVE -1                 TO  APFKL
00634          PERFORM 8200-SEND-DATAONLY.
00635
00636      EJECT
00637  0100-MAIN-LOGIC.
00638
00639      IF PI-CALLED-FROM-AR-MENU         AND
00640         EIBAID = DFHPF3
00641          IF BCNTLNOL IS GREATER THAN +0
00642              MOVE BCNTLNOI           TO  PI-START-CONTROL-NO
00643              GO TO 0130-MAIN-LOGIC
00644          ELSE
00645              GO TO 0130-MAIN-LOGIC.
00646
00647      IF NOT PI-CALLED-FROM-AR-MENU         AND
00648         EIBAID = DFHPF3
00649          IF CNTLNOL IS GREATER THAN +0
00650              MOVE CNTLNOI            TO  PI-START-CONTROL-NO
00651              GO TO 0130-MAIN-LOGIC
00652          ELSE
00653              GO TO 0130-MAIN-LOGIC.
00654
00655      IF NOT PI-CALLED-FROM-AR-MENU     AND
00656         EIBAID = DFHPF4
00657          IF CHECKS-TO-BE-PRINTED
00658              MOVE 'Y' TO WS-SAVE-CHECK-MODE
00659              IF CNTLNOL GREATER ZERO
00660                  IF CNTLNOI NUMERIC
00661                      MOVE CNTLNOI TO WS-START-CNTLNO
00662                      GO TO 0015-MAIN-LOGIC
00663                  ELSE
00664                      GO TO 0015-MAIN-LOGIC
00665              ELSE
00666                  GO TO 0015-MAIN-LOGIC
00667          ELSE
00668              IF CHECKS-PRINTED
00669                  MOVE SPACE TO WS-SAVE-CHECK-MODE
00670                  IF CNTLNOL IS GREATER THAN +0
00671                      IF CNTLNOI IS NUMERIC
00672                          MOVE CNTLNOI  TO  WS-START-CNTLNO
00673                          GO TO 0015-MAIN-LOGIC
00674                      ELSE
00675                          GO TO 0015-MAIN-LOGIC
00676                  ELSE
00677                      GO TO 0015-MAIN-LOGIC.
00678
00679      IF PI-CALLED-FROM-AR-MENU         AND
00680         EIBAID = DFHPF4
00681          IF CHECKS-TO-BE-PRINTED
00682              MOVE 'Y' TO WS-SAVE-CHECK-MODE
00683              IF BCNTLNOL GREATER ZERO
00684                  IF BCNTLNOI NUMERIC
00685                      MOVE BCNTLNOI TO WS-START-CNTLNO
00686                      GO TO 0015-MAIN-LOGIC
00687                  ELSE
00688                      GO TO 0015-MAIN-LOGIC
00689              ELSE
00690                  GO TO 0015-MAIN-LOGIC
00691          ELSE
00692              IF CHECKS-PRINTED
00693                  MOVE SPACE TO WS-SAVE-CHECK-MODE
00694                  IF BCNTLNOL IS GREATER THAN +0
00695                      IF BCNTLNOI IS NUMERIC
00696                          MOVE BCNTLNOI   TO  WS-START-CNTLNO
00697                          GO TO 0015-MAIN-LOGIC
00698                      ELSE
00699                          GO TO 0015-MAIN-LOGIC
00700                  ELSE
00701                      GO TO 0015-MAIN-LOGIC.
00702
00703      IF EIBAID = DFHPF1 OR DFHPF2
00704          GO TO 0110-MAIN-LOGIC.
00705
00706      IF NOT PI-CALLED-FROM-AR-MENU    AND
00707         CNTLNOL GREATER ZERO
00708          IF CNTLNOI NUMERIC
00709              MOVE CNTLNOI TO WS-START-CNTLNO
00710              MOVE PI-CHECK-MODE TO WS-SAVE-CHECK-MODE
00711              GO TO 0015-MAIN-LOGIC.
00712
00713      IF PI-CALLED-FROM-AR-MENU    AND
00714         BCNTLNOL GREATER ZERO
00715          IF BCNTLNOI NUMERIC
00716              MOVE BCNTLNOI TO WS-START-CNTLNO
00717              MOVE PI-CHECK-MODE TO WS-SAVE-CHECK-MODE
00718              GO TO 0015-MAIN-LOGIC.
00719
00720      IF PI-END-OF-FILE NOT = ZERO
00721          PERFORM 9400-CLEAR.
00722
00723      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.
00724
00725  0110-MAIN-LOGIC.
00726      IF PI-CALLED-FROM-AR-MENU
00727          MOVE BPAGEI                 TO  WS-TEMP-STORAGE-ITEM
00728      ELSE
00729          MOVE APAGEI                 TO  WS-TEMP-STORAGE-ITEM.
00730
00731      IF EIBAID = DFHPF1 AND
00732         PI-SEND-TOT     AND
00733         PI-EOF          AND
00734         WS-TEMP-STORAGE-ITEM = PI-TEMP-STORAGE-ITEM
00735         IF NOT PI-CALLED-FROM-AR-MENU
00736             MOVE LOW-VALUE TO EL685AI
00737             MOVE WS-TEMP-STORAGE-ITEM TO APAGEO
00738             MOVE +375                   TO  EMI-ERROR
00739             SET EL685A-INDEX TO 1
00740             MOVE 'CONTL TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)
00741             MOVE PI-CONTROL-TOT TO EL685A-AMT (EL685A-INDEX)
00742             SET EL685A-INDEX UP BY +1
00743             MOVE PI-CONTROL-GRAND-TOT TO EL685A-AMT (EL685A-INDEX)
00744             MOVE 'GRAND TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)
00745             MOVE -1 TO APFKL
00746             MOVE 'T' TO PI-PREV-DISPLAY-SWT
00747             GO TO 8100-SEND-INITIAL-MAP
00748         ELSE
00749             MOVE LOW-VALUE TO EL685BI
00750             MOVE WS-TEMP-STORAGE-ITEM TO BPAGEO
00751             MOVE +375                   TO  EMI-ERROR
00752             SET EL685B-INDEX TO 1
00753             MOVE ' CONTL'      TO EL685B-DESC-ONE   (EL685B-INDEX)
00754             MOVE 'TOTAL'       TO EL685B-DESC-TWO   (EL685B-INDEX)
00755             MOVE PI-CONTROL-TOT TO EL685B-AMT (EL685B-INDEX)
00756             SET EL685B-INDEX UP BY +1
00757             MOVE PI-CONTROL-GRAND-TOT TO EL685B-AMT (EL685B-INDEX)
00758             MOVE ' GRAND'      TO EL685B-DESC-ONE   (EL685B-INDEX)
00759             MOVE 'TOTAL'       TO EL685B-DESC-TWO   (EL685B-INDEX)
00760             MOVE -1 TO BPFKL
00761             MOVE 'T' TO PI-PREV-DISPLAY-SWT
00762             GO TO 8100-SEND-INITIAL-MAP.
00763
00764      IF EIBAID = DFHPF1
00765         IF WS-TEMP-STORAGE-ITEM LESS THAN PI-TEMP-STORAGE-ITEM
00766          ADD +1  TO  WS-TEMP-STORAGE-ITEM
00767          GO TO 0120-MAIN-LOGIC
00768         ELSE
00769          IF PI-NOT-EOF
00770             GO TO 4000-BROWSE-CHECK-QUEUE-FILE
00771          ELSE
00772             NEXT SENTENCE
00773      ELSE
00774         NEXT SENTENCE.
00775
00776      IF EIBAID = DFHPF2
00777        IF WS-TEMP-STORAGE-ITEM GREATER THAN +1
00778            IF PI-TOTAL-SCREEN
00779                MOVE SPACES TO PI-PREV-DISPLAY-SWT
00780                GO TO 0120-MAIN-LOGIC
00781            ELSE
00782                SUBTRACT +1 FROM WS-TEMP-STORAGE-ITEM
00783                GO TO 0120-MAIN-LOGIC
00784        ELSE
00785            IF PI-TOTAL-SCREEN
00786                MOVE SPACES TO PI-PREV-DISPLAY-SWT
00787                GO TO 0120-MAIN-LOGIC.
00788
00789      MOVE +312                   TO  EMI-ERROR
00790      IF PI-CALLED-FROM-AR-MENU
00791          MOVE -1                     TO  BPFKL
00792      ELSE
00793          MOVE -1                     TO  APFKL.
00794      PERFORM 8200-SEND-DATAONLY.
00795      EJECT
00796  0120-MAIN-LOGIC.
00797      MOVE EIBTRMID               TO  WS-TS-TERM-ID.
00798
00799      IF PI-CALLED-FROM-AR-MENU
00800          
      * EXEC CICS READQ TS
00801 *            QUEUE  (WS-TEMP-STORAGE-KEY)
00802 *            ITEM   (WS-TEMP-STORAGE-ITEM)
00803 *            INTO   (EL685BI)
00804 *            LENGTH (WS-TS-LENGTH)
00805 *        END-EXEC
      *    MOVE '*$II   L              ''   #00006306' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL685BI, 
                 WS-TS-LENGTH, 
                 WS-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00806      ELSE
00807          
      * EXEC CICS READQ TS
00808 *            QUEUE  (WS-TEMP-STORAGE-KEY)
00809 *            ITEM   (WS-TEMP-STORAGE-ITEM)
00810 *            INTO   (EL685AI)
00811 *            LENGTH (WS-TS-LENGTH)
00812 *        END-EXEC.
      *    MOVE '*$II   L              ''   #00006313' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL685AI, 
                 WS-TS-LENGTH, 
                 WS-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00813
00814      IF PI-CALLED-FROM-AR-MENU
00815          MOVE WS-TEMP-STORAGE-ITEM  TO  BPAGEO
00816      ELSE
00817          MOVE WS-TEMP-STORAGE-ITEM  TO  APAGEO.
00818
00819      PERFORM 8100-SEND-INITIAL-MAP.
00820      EJECT
00821  0130-MAIN-LOGIC.
00822      
      * EXEC CICS HANDLE CONDITION
00823 *         TERMIDERR    (0130-TERMID-ERROR)
00824 *         TRANSIDERR   (0130-TRANS-ERROR)
00825 *         END-EXEC.
      *    MOVE '"$[\                  ! $ #00006328' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303036333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00826
00827      MOVE SPACES                     TO PI-ALT-DMD-PRT-ID.
00828      IF PI-CALLED-FROM-AR-MENU
00829          IF BPRINTRL NOT = ZEROS
00830             MOVE BPRINTRI            TO WS-PRINTER-ID
00831                                         PI-ALT-DMD-PRT-ID
00832             GO TO 0130-START
00833          ELSE
00834             NEXT SENTENCE
00835      ELSE
00836          IF PRINTERL NOT = ZEROS
00837             MOVE PRINTERI            TO WS-PRINTER-ID
00838                                         PI-ALT-DMD-PRT-ID
00839             GO TO 0130-START.
00840
00841      IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES
00842          MOVE PI-PROCESSOR-PRINTER   TO  WS-PRINTER-ID
00843          GO TO 0130-START.
00844
00845      MOVE PI-COMPANY-ID          TO CNTL-CO
00846      MOVE '1'                    TO CNTL-RECORD-TYPE
00847      MOVE SPACES                 TO CNTL-GENL
00848      MOVE ZEROS                  TO CNTL-SEQ
00849      
      * EXEC CICS READ
00850 *         DATASET   (WS-CONTROL-DSID)
00851 *         SET       (ADDRESS OF CONTROL-FILE)
00852 *         RIDFLD    (CNTL-KEY)
00853 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006355' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00854
00855      MOVE CF-FORMS-PRINTER-ID    TO WS-PRINTER-ID.
00856
00857  0130-START.
00858
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00860 *        MOVE EIBTRMID       TO WS-PRINTER-ID
00861          
      * EXEC CICS START
00862 *             INTERVAL(0)
00863 *             TRANSID    (WS-PRINT-TRAN-ID)
00864 *             FROM       (PROGRAM-INTERFACE-BLOCK)
00865 *             LENGTH     (PI-COMM-LENGTH)
00866 *             TERMID     (WS-PRINTER-ID)
00867 *        END-EXEC
           MOVE 0 TO DFHEIV10
      *    MOVE '0(ILF                 1   #00006367' TO DFHEIV0
           MOVE X'3028494C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 WS-PRINT-TRAN-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00868      ELSE
00869          
      * EXEC CICS START
00870 *             INTERVAL(0)
00871 *             TRANSID    (WS-PRINT-TRAN-ID)
00872 *             FROM       (PROGRAM-INTERFACE-BLOCK)
00873 *             LENGTH     (PI-COMM-LENGTH)
00874 *             TERMID     (WS-PRINTER-ID)
00875 *        END-EXEC.
           MOVE 0 TO DFHEIV10
      *    MOVE '0(ILFT                1   #00006375' TO DFHEIV0
           MOVE X'3028494C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 WS-PRINT-TRAN-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 WS-PRINTER-ID, 
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
           
00876
00877      MOVE 0567                   TO EMI-ERROR
00878      MOVE -1                     TO APFKL
00879      GO TO 8200-SEND-DATAONLY.
00880
00881
00882  0130-TERMID-ERROR.
00883      MOVE 0412                   TO EMI-ERROR
00884      MOVE -1                     TO APFKL
00885      GO TO 8200-SEND-DATAONLY.
00886  0130-TRANS-ERROR.
00887      MOVE 0413                   TO EMI-ERROR
00888      MOVE -1                     TO APFKL
00889      GO TO 8200-SEND-DATAONLY.
00890
00891      EJECT
00892  4000-BROWSE-CHECK-QUEUE-FILE SECTION.
00893
00894      IF PI-CALLED-FROM-AR-MENU
00895          PERFORM 5000-BROWSE-COMM-CHECK-QUEUE
00896          GO TO 4990-EXIT.
00897
00898      
      * EXEC CICS HANDLE CONDITION
00899 *        NOTFND   (8400-NOTFND)
00900 *    END-EXEC.
      *    MOVE '"$I                   ! % #00006404' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036343034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00901
00902      MOVE LOW-VALUES             TO  EL685AI
00903
00904      
      * EXEC CICS STARTBR
00905 *        DATASET (WS-CHECK-QUEUE-DSID)
00906 *        RIDFLD  (PI-CHECK-QUE-KEY)
00907 *        GTEQ
00908 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006410' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 PI-CHECK-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00909
00910      SET EL685A-INDEX TO +1.
00911
00912  4100-READNEXT.
00913      MOVE PI-CHECK-QUE-KEY           TO  PI-PREV-CHECK-QUE-KEY
00914
00915      
      * EXEC CICS HANDLE CONDITION
00916 *        ENDFILE  (4800-END-OF-FILE)
00917 *    END-EXEC.
      *    MOVE '"$''                   ! & #00006421' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036343231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00918
00919      
      * EXEC CICS READNEXT
00920 *        DATASET (WS-CHECK-QUEUE-DSID)
00921 *        RIDFLD  (PI-CHECK-QUE-KEY)
00922 *        SET     (ADDRESS OF CHECK-QUE) END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006425' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CHECK-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00923
00924      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
00925          GO TO 4800-END-OF-FILE.
00926
00927      IF CQ-ENTRY-TYPE NOT = 'Q'
00928          GO TO 4100-READNEXT.
00929
00930      IF CQ-VOID-INDICATOR = 'V'
00931          GO TO 4100-READNEXT.
00932
00933      IF CQ-CHECK-AMOUNT = ZEROS
00934          GO TO 4100-READNEXT.
00935
00936      IF CQ-TIMES-PRINTED NOT = ZERO
00937          IF CHECKS-PRINTED
00938              NEXT SENTENCE
00939          ELSE
00940              GO TO 4100-READNEXT
00941      ELSE
00942          IF CHECKS-TO-BE-PRINTED
00943              NEXT SENTENCE
00944          ELSE
00945              GO TO 4100-READNEXT.
00946
00947      IF PI-FIRST-TIME
00948          IF CNTLNOL IS EQUAL TO +0
00949              MOVE PI-CK-CONTROL-NO   TO  PI-START-CONTROL-NO.
CIDMOD
CIDMOD     
      * EXEC CICS
CIDMOD*         ASKTIME
CIDMOD*    END-EXEC.
      *    MOVE '0"                    "   #00006457' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00950
00951      IF EL685A-INDEX LESS THAN +18
00952         IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER
00953            IF PI-FIRST-TIME
00954               MOVE CQ-CONTROL-NUMBER TO PI-CONTROL-SAVE-CONTROL
00955            ELSE
00956               MOVE 'CONTL TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)
00957               MOVE PI-CONTROL-TOT TO EL685A-AMT (EL685A-INDEX)
00958               SET EL685A-INDEX UP BY +1
00959               MOVE ZEROS TO PI-CONTROL-TOT
00960               MOVE CQ-CONTROL-NUMBER TO PI-CONTROL-SAVE-CONTROL.
00961
00962      MOVE 'N'                    TO PI-FIRST-TIME-SW.
00963
00964      IF EL685A-INDEX GREATER THAN +17
00965         MOVE +1 TO WS-READNEXT-SW.
00966
00967      IF WS-READNEXT-SW GREATER THAN ZERO
00968          GO TO 4900-ENDBROWSE.
00969
00970      ADD CQ-CHECK-AMOUNT        TO  PI-CONTROL-TOT
00971                                     PI-CONTROL-GRAND-TOT.
00972      MOVE CQ-CONTROL-NUMBER     TO  EL685A-CONTROL  (EL685A-INDEX)
00973      MOVE CQ-CHECK-NUMBER       TO  EL685A-CHECK-NO (EL685A-INDEX)
00974      MOVE CQ-CHECK-AMOUNT       TO  EL685A-AMT      (EL685A-INDEX)
00975
00976      IF CQ-BILLING-CREDIT
00977         MOVE 'BILL CREDIT'     TO  EL685A-PMT-TYPE (EL685A-INDEX)
00978      ELSE
00979          IF CQ-REFUND-PMT
00980             MOVE 'REFUND PMT'  TO  EL685A-PMT-TYPE (EL685A-INDEX)
00981          ELSE
00982             MOVE 'CHECK MAINT' TO  EL685A-PMT-TYPE (EL685A-INDEX).
00983
00984      IF CQ-CHECK-MAINT-PMT OR CQ-REFUND-PMT
00985        IF PI-COMPANY-ID = 'LAP'  OR  'RMC'
00986          MOVE CQ-CHEK-GROUPING  TO  EL685A-GROUPING (EL685A-INDEX)
00987          MOVE CQ-CHEK-CARRIER   TO  EL685A-CARRIER  (EL685A-INDEX)
00988          MOVE CQ-CHEK-ACCOUNT   TO  EL685A-FIN-RESP (EL685A-INDEX)
00989          MOVE CQ-CHEK-CERT-NO   TO  EL685A-ACCOUNT  (EL685A-INDEX)
00990        ELSE
00991          MOVE CQ-CHEK-GROUPING  TO  EL685A-GROUPING (EL685A-INDEX)
00992          MOVE CQ-CHEK-CARRIER   TO  EL685A-CARRIER  (EL685A-INDEX)
00993          MOVE CQ-CHEK-FIN-RESP  TO  EL685A-FIN-RESP (EL685A-INDEX)
00994          MOVE CQ-CHEK-ACCOUNT   TO  EL685A-ACCOUNT  (EL685A-INDEX)
00995      ELSE
00996          MOVE CQ-PYAJ-GROUPING  TO  EL685A-GROUPING (EL685A-INDEX)
00997          MOVE CQ-PYAJ-CARRIER   TO  EL685A-CARRIER  (EL685A-INDEX)
00998          MOVE CQ-PYAJ-FIN-RESP  TO  EL685A-FIN-RESP (EL685A-INDEX)
00999          MOVE CQ-PYAJ-ACCOUNT   TO  EL685A-ACCOUNT (EL685A-INDEX).
01000
01001      IF EL685A-INDEX LESS THAN +18
01002          SET EL685A-INDEX UP BY +1
01003          GO TO 4100-READNEXT.
01004
01005      MOVE +1                     TO  WS-READNEXT-SW.
01006      GO TO 4100-READNEXT.
01007
01008  4800-END-OF-FILE.
CIDMOD     
      * EXEC CICS
CIDMOD*         ASKTIME
CIDMOD*    END-EXEC.
      *    MOVE '0"                    "   #00006519' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01009
01010      MOVE +1                     TO  PI-END-OF-FILE.
01011
01012      MOVE 'Y'                    TO PI-EOF-SWT.
01013      IF EL685A-INDEX GREATER +16
01014         MOVE 'Y' TO PI-SEND-TOT-SWT
01015         GO TO 4900-ENDBROWSE.
01016
01017      MOVE +375                   TO  EMI-ERROR.
01018      IF EL685A-INDEX LESS THAN +18
01019               MOVE 'CONTL TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)
01020               MOVE PI-CONTROL-TOT TO EL685A-AMT (EL685A-INDEX)
01021               SET EL685A-INDEX UP BY +1
01022      ELSE
01023         NEXT SENTENCE.
01024      IF EL685A-INDEX LESS THAN +18
01025         SET EL685A-INDEX UP BY +1
01026         MOVE PI-CONTROL-GRAND-TOT TO EL685A-AMT (EL685A-INDEX)
01027         MOVE 'GRAND TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)
01028      ELSE
01029         NEXT SENTENCE.
01030
01031  4900-ENDBROWSE.
01032      MOVE -1                     TO  APFKL
01033
01034      MOVE EIBTRMID               TO  WS-TS-TERM-ID
01035
01036      
      * EXEC CICS WRITEQ TS
01037 *        QUEUE  (WS-TEMP-STORAGE-KEY)
01038 *        ITEM   (PI-TEMP-STORAGE-ITEM)
01039 *        FROM   (EL685AI)
01040 *        LENGTH (WS-TS-LENGTH)
01041 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00006549' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL685AI, 
                 WS-TS-LENGTH, 
                 PI-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01042
01043      MOVE PI-TEMP-STORAGE-ITEM  TO  APAGEO
01044
01045      PERFORM 8100-SEND-INITIAL-MAP.
01046
01047  4990-EXIT.
01048      EXIT.
01049
01050      EJECT
01051  5000-BROWSE-COMM-CHECK-QUEUE SECTION.
01052
01053      
      * EXEC CICS HANDLE CONDITION
01054 *        NOTFND   (8400-NOTFND)
01055 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00006566' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036353636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01056
01057      MOVE LOW-VALUES             TO  EL685BI
01058
01059      
      * EXEC CICS STARTBR
01060 *        DATASET (WS-COMCK-QUEUE-DSID)
01061 *        RIDFLD  (PI-CHECK-QUE-KEY)
01062 *        GTEQ
01063 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006572' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-COMCK-QUEUE-DSID, 
                 PI-CHECK-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01064
01065      SET EL685B-INDEX TO +1.
01066
01067  5100-READNEXT.
01068
01069      
      * EXEC CICS HANDLE CONDITION
01070 *        ENDFILE  (5800-END-OF-FILE)
01071 *    END-EXEC.
      *    MOVE '"$''                   ! ( #00006582' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036353832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01072
01073      
      * EXEC CICS READNEXT
01074 *        DATASET (WS-COMCK-QUEUE-DSID)
01075 *        RIDFLD  (PI-CHECK-QUE-KEY)
01076 *        SET     (ADDRESS OF COMMISSION-CHECK-QUE)
01077 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006586' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-COMCK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CHECK-QUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMMISSION-CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01078
01079      IF MQ-COMPANY-CD NOT = PI-COMPANY-CD
01080          GO TO 5800-END-OF-FILE.
01081
01082      IF MQ-VOID-DT NOT = LOW-VALUES
01083          GO TO 5100-READNEXT.
01084
01085      IF MQ-ENTRY-TYPE = 'Q' OR 'P'
01086          NEXT SENTENCE
01087      ELSE
01088          GO TO 5100-READNEXT.
01089
01090      IF CHECKS-TO-BE-PRINTED
01091          IF MQ-CHECK-AMOUNT = ZEROS
01092              GO TO 5100-READNEXT.
01093
01094      IF MQ-TIMES-PRINTED NOT = ZERO
01095          IF CHECKS-PRINTED
01096              NEXT SENTENCE
01097          ELSE
01098              GO TO 5100-READNEXT
01099      ELSE
01100          IF CHECKS-TO-BE-PRINTED
01101              NEXT SENTENCE
01102          ELSE
01103              GO TO 5100-READNEXT.
01104
01105      IF PI-PAGE-FORWARD
01106          MOVE COMMISSION-CHECK-QUE
01107                                  TO  HOLD-CHECK-RECORD
01108          MOVE 'N'                TO  PI-PAGING-SW
01109          IF CHECKS-PRINTED
01110              GO TO 5100-READNEXT.
01111
CIDMOD     IF PI-FIRST-TIME
CIDMOD         IF BCNTLNOL IS EQUAL TO +0
CIDMOD             MOVE PI-CK-CONTROL-NO   TO  PI-START-CONTROL-NO
CIDMOD         END-IF
CIDMOD     END-IF.
CIDMOD
01112      IF MQ-COMPANY-CD-A1  = HOLD-COMPANY-CD-A1  AND
01113         MQ-CONTROL-NUMBER = HOLD-CONTROL-NUMBER AND
01114         MQ-CARRIER-A1     = HOLD-CARRIER-A1     AND
01115         MQ-GROUPING-A1    = HOLD-GROUPING-A1    AND
01116         MQ-PAYEE-A1       = HOLD-PAYEE-A1       AND
01117         MQ-PAYEE-SEQ-A1   = HOLD-PAYEE-SEQ-A1
01118          NEXT SENTENCE
01119      ELSE
01120          MOVE PI-CHECK-QUE-KEY   TO  PI-PREV-CHECK-QUE-KEY.
01121
01122      IF CHECKS-PRINTED
01123          GO TO 5200-PRINTED.
01124
01125  5100-TO-BE-PRINTED.
01126
01127      IF PI-FIRST-TOTAL
01128          IF BCNTLNOL IS EQUAL TO +0
01129              MOVE PI-CK-CONTROL-NO
01130                                  TO  PI-START-CONTROL-NO.
01131
CIDMOD     
      * EXEC CICS
CIDMOD*         ASKTIME
CIDMOD*    END-EXEC.
      *    MOVE '0"                    "   #00006651' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
01132      IF EL685B-INDEX LESS THAN +18
01133         IF PI-CONTROL-SAVE-CONTROL NOT = MQ-CONTROL-NUMBER
01134            IF PI-FIRST-TOTAL
01135                MOVE MQ-CONTROL-NUMBER
01136                                  TO PI-CONTROL-SAVE-CONTROL
01137                MOVE PI-CK-CONTROL-NO
01138                                  TO  PI-START-CONTROL-NO
01139            ELSE
01140               MOVE ' CONTL'      TO EL685B-DESC-ONE (EL685B-INDEX)
01141               MOVE 'TOTAL'       TO EL685B-DESC-TWO (EL685B-INDEX)
01142               MOVE PI-CONTROL-TOT
01143                                  TO EL685B-AMT (EL685B-INDEX)
01144               SET EL685B-INDEX UP BY +1
01145               MOVE ZEROS         TO PI-CONTROL-TOT
01146               MOVE MQ-CONTROL-NUMBER
01147                                  TO PI-CONTROL-SAVE-CONTROL.
01148
01149      MOVE 'N'                    TO PI-FIRST-TOTAL-SW.
01150
01151      IF EL685B-INDEX GREATER THAN +17
01152         MOVE +1 TO WS-READNEXT-SW.
01153
01154      IF WS-READNEXT-SW GREATER THAN ZERO
01155          MOVE PI-PREV-CHECK-QUE-KEY
01156                                  TO  PI-CHECK-QUE-KEY
01157          MOVE 'Y'                TO  PI-PAGING-SW
01158          GO TO 5900-WRITE-REPORT.
01159
01160      ADD MQ-CHECK-AMOUNT      TO  PI-CONTROL-TOT
01161                                   PI-CONTROL-GRAND-TOT.
01162      MOVE MQ-CONTROL-NUMBER   TO  EL685B-CONTROL   (EL685B-INDEX)
01163
01164      IF ACH-PAYMENT
01165          MOVE ' ACH  '
01166                               TO  EL685B-CHECK-NO  (EL685B-INDEX)
01167      ELSE
01168          MOVE MQ-CHECK-NUMBER
01169                               TO  EL685B-CHECK-NO  (EL685B-INDEX).
01170
01171      MOVE MQ-CHECK-AMOUNT     TO  EL685B-AMT       (EL685B-INDEX)
01172      MOVE MQ-CHEK-CARRIER     TO  EL685B-CARRIER   (EL685B-INDEX)
01173      MOVE MQ-CHEK-GROUPING    TO  EL685B-GROUPING  (EL685B-INDEX)
01174      MOVE MQ-CHEK-PAYEE       TO  EL685B-PAYEE     (EL685B-INDEX)
01175      MOVE MQ-PAYEE-SEQ-A1     TO  EL685B-PAYEE-SEQ (EL685B-INDEX)
01176      MOVE MQ-PAYEE-NAME       TO  EL685B-PAYEE-NA  (EL685B-INDEX)
01177
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
CIDMOD         MOVE MQ-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1
CIDMOD         MOVE ' '                    TO  DC-OPTION-CODE
CIDMOD         PERFORM 8500-DATE-CONVERSION
CIDMOD         MOVE DC-GREG-DATE-1-EDIT TO
CIDMOD                                  EL685B-CHK-DT (EL685B-INDEX)
CIDMOD     ELSE
CIDMOD         MOVE SPACES              TO  EL685B-CHK-DT(EL685B-INDEX)
CIDMOD     END-IF.
01179
01180      IF EL685B-INDEX LESS THAN +18
01181          SET EL685B-INDEX UP BY +1
01182          GO TO 5100-READNEXT.
01183
01184      MOVE +1                     TO  WS-READNEXT-SW.
01185      GO TO 5100-READNEXT.
01186
01187  5200-PRINTED.
01188
01189      IF PI-FIRST-TIME
01190          MOVE COMMISSION-CHECK-QUE
01191                                  TO  HOLD-CHECK-RECORD
01192          IF BCNTLNOL IS EQUAL TO +0
01193              MOVE PI-CK-CONTROL-NO
01194                                  TO  PI-START-CONTROL-NO
01195              MOVE 'N'            TO  PI-FIRST-TIME-SW
01196              GO TO 5100-READNEXT.
01197
01198      IF MQ-COMPANY-CD-A1  = HOLD-COMPANY-CD-A1  AND
01199         MQ-CONTROL-NUMBER = HOLD-CONTROL-NUMBER AND
01200         MQ-CARRIER-A1     = HOLD-CARRIER-A1     AND
01201         MQ-GROUPING-A1    = HOLD-GROUPING-A1    AND
01202         MQ-PAYEE-A1       = HOLD-PAYEE-A1       AND
01203         MQ-PAYEE-SEQ-A1   = HOLD-PAYEE-SEQ-A1
01204         IF MQ-TEXT
01205             IF MQ-CHECK-NUMBER = HOLD-CHECK-NUMBER
01206                 GO TO 5100-READNEXT
01207             ELSE
01208                 MOVE HOLD-CHECK-AMOUNT
01209                                  TO  MQ-CHECK-AMOUNT
01210                 MOVE ZEROS       TO  HOLD-CHECK-AMOUNT
01211         ELSE
01212             IF MQ-CHECK-NUMBER = HOLD-CHECK-NUMBER
01213                 MOVE COMMISSION-CHECK-QUE
01214                                  TO  HOLD-CHECK-RECORD
01215                 GO TO 5100-READNEXT
01216             ELSE
01217                 GO TO 5210-CONTINUE.
01218
01219      IF MQ-CHECK-NUMBER NOT = HOLD-CHECK-NUMBER
01220          NEXT SENTENCE
01221      ELSE
01222          MOVE COMMISSION-CHECK-QUE
01223                                  TO  HOLD-CHECK-RECORD
01224          GO TO 5100-READNEXT.
01225
01226  5210-CONTINUE.
01227
01228 *    IF EL685B-INDEX LESS THAN +18
01229         IF PI-CONTROL-SAVE-CONTROL NOT = HOLD-CONTROL-NUMBER
01230            IF PI-FIRST-TOTAL
01231                MOVE HOLD-CONTROL-NUMBER
01232                                  TO PI-CONTROL-SAVE-CONTROL
01233                MOVE PI-CK-CONTROL-NO
01234                                  TO  PI-START-CONTROL-NO
01235                MOVE 'N'          TO  PI-FIRST-TOTAL-SW
01236            ELSE
01237               MOVE ' CONTL'      TO EL685B-DESC-ONE (EL685B-INDEX)
01238               MOVE 'TOTAL'       TO EL685B-DESC-TWO (EL685B-INDEX)
01239               MOVE PI-CONTROL-TOT
01240                                  TO EL685B-AMT (EL685B-INDEX)
01241               SET EL685B-INDEX UP BY +1
01242               MOVE ZEROS         TO PI-CONTROL-TOT
01243               MOVE HOLD-CONTROL-NUMBER
01244                                  TO PI-CONTROL-SAVE-CONTROL.
01245
01246      ADD HOLD-CHECK-AMOUNT       TO  PI-CONTROL-TOT
01247                                      PI-CONTROL-GRAND-TOT.
01248      MOVE HOLD-CONTROL-NUMBER TO  EL685B-CONTROL   (EL685B-INDEX).
01249
01250      IF HOLD-ENTRY-TYPE = 'P'
01251          MOVE ' ACH  '
01252                               TO  EL685B-CHECK-NO  (EL685B-INDEX)
01253      ELSE
01254          MOVE HOLD-CHECK-NUMBER
01255                               TO  EL685B-CHECK-NO  (EL685B-INDEX).
01256
01257      MOVE HOLD-CHECK-AMOUNT   TO  EL685B-AMT       (EL685B-INDEX).
01258      MOVE HOLD-CHEK-CARRIER   TO  EL685B-CARRIER   (EL685B-INDEX).
01259      MOVE HOLD-CHEK-GROUPING  TO  EL685B-GROUPING  (EL685B-INDEX).
01260      MOVE HOLD-CHEK-PAYEE     TO  EL685B-PAYEE     (EL685B-INDEX).
01261      MOVE HOLD-PAYEE-SEQ-A1   TO  EL685B-PAYEE-SEQ (EL685B-INDEX).
01262      MOVE HOLD-PAYEE-NAME     TO  EL685B-PAYEE-NA  (EL685B-INDEX).
01263
01264      IF HOLD-CHECK-WRITTEN-DT = ZEROS OR LOW-VALUES OR SPACES
01265          MOVE SPACES
01266                               TO  EL685B-CHK-DT (EL685B-INDEX)
01267      ELSE
01268          MOVE HOLD-CHECK-WRITTEN-DT
01269                               TO  DC-BIN-DATE-1
01270          MOVE ' '             TO  DC-OPTION-CODE
01271          PERFORM 8500-DATE-CONVERSION
01272          MOVE DC-GREG-DATE-1-EDIT
01273                               TO  EL685B-CHK-DT (EL685B-INDEX).
01274
01275      MOVE COMMISSION-CHECK-QUE   TO  HOLD-CHECK-RECORD.
01276
01277      IF EL685B-INDEX LESS THAN +18
01278          SET EL685B-INDEX UP BY +1
01279          GO TO 5100-READNEXT.
01280
01281      IF EL685B-INDEX GREATER THAN +17
01282          MOVE 'Y'                TO  PI-PAGING-SW
01283          GO TO 5900-WRITE-REPORT.
01284
01285      GO TO 5100-READNEXT.
01286
01287  5800-END-OF-FILE.
01288
01289      IF CHECKS-PRINTED
01290          GO TO 5800-END-PRINTED.
01291
01292      IF EL685B-INDEX LESS THAN +18
01293          SET EL685B-INDEX UP BY +1.
CIDMOD
CIDMOD     
      * EXEC CICS
CIDMOD*         ASKTIME
CIDMOD*    END-EXEC.
      *    MOVE '0"                    "   #00006826' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01294
01295      MOVE +1                     TO  PI-END-OF-FILE.
01296      MOVE 'Y'                    TO  PI-EOF-SWT.
01297
01298      IF EL685B-INDEX GREATER +16
01299         MOVE 'Y'                 TO PI-SEND-TOT-SWT
01300         GO TO 5900-WRITE-REPORT.
01301
01302      MOVE +375                   TO  EMI-ERROR.
01303
01304      IF EL685B-INDEX LESS THAN +18
01305               MOVE ' CONTL'     TO EL685B-DESC-ONE (EL685B-INDEX)
01306               MOVE 'TOTAL'      TO  EL685B-DESC-TWO (EL685B-INDEX)
01307               MOVE PI-CONTROL-TOT
01308                                  TO EL685B-AMT (EL685B-INDEX)
01309               SET EL685B-INDEX UP BY +1
01310      ELSE
01311         NEXT SENTENCE.
01312
01313      IF EL685B-INDEX LESS THAN +18
01314         SET EL685B-INDEX UP BY +1
01315         MOVE PI-CONTROL-GRAND-TOT
01316                                  TO EL685B-AMT (EL685B-INDEX)
01317         MOVE ' GRAND'            TO EL685B-DESC-ONE (EL685B-INDEX)
01318         MOVE 'TOTAL'             TO EL685B-DESC-TWO (EL685B-INDEX)
01319      ELSE
01320         NEXT SENTENCE.
01321
01322      GO TO 5900-WRITE-REPORT.
01323
01324  5800-END-PRINTED.
01325
01326      IF EL685B-INDEX LESS THAN +18
01327         IF PI-CONTROL-SAVE-CONTROL NOT = HOLD-CONTROL-NUMBER
01328            IF PI-FIRST-TOTAL
01329                MOVE HOLD-CONTROL-NUMBER
01330                                  TO PI-CONTROL-SAVE-CONTROL
01331                MOVE PI-CK-CONTROL-NO
01332                                  TO  PI-START-CONTROL-NO
01333                MOVE 'N'          TO  PI-FIRST-TOTAL-SW
01334            ELSE
01335               MOVE ' CONTL'      TO EL685B-DESC-ONE (EL685B-INDEX)
01336               MOVE 'TOTAL'       TO EL685B-DESC-TWO (EL685B-INDEX)
01337               MOVE PI-CONTROL-TOT
01338                                  TO EL685B-AMT (EL685B-INDEX)
01339               SET EL685B-INDEX UP BY +1
01340               MOVE ZEROS         TO PI-CONTROL-TOT
01341               MOVE HOLD-CONTROL-NUMBER
01342                                  TO PI-CONTROL-SAVE-CONTROL.
01343
01344      ADD HOLD-CHECK-AMOUNT       TO  PI-CONTROL-TOT
01345                                      PI-CONTROL-GRAND-TOT
01346      MOVE HOLD-CONTROL-NUMBER TO  EL685B-CONTROL   (EL685B-INDEX)
01347
01348      IF HOLD-ENTRY-TYPE = 'P'
01349          MOVE ' ACH  '
01350                               TO  EL685B-CHECK-NO  (EL685B-INDEX)
01351      ELSE
01352          MOVE HOLD-CHECK-NUMBER
01353                               TO  EL685B-CHECK-NO  (EL685B-INDEX).
01354
01355      MOVE HOLD-CHECK-AMOUNT   TO  EL685B-AMT       (EL685B-INDEX)
01356      MOVE HOLD-CHEK-CARRIER   TO  EL685B-CARRIER   (EL685B-INDEX)
01357      MOVE HOLD-CHEK-GROUPING  TO  EL685B-GROUPING  (EL685B-INDEX)
01358      MOVE HOLD-CHEK-PAYEE     TO  EL685B-PAYEE     (EL685B-INDEX)
01359      MOVE HOLD-PAYEE-SEQ-A1   TO  EL685B-PAYEE-SEQ (EL685B-INDEX)
01360      MOVE HOLD-PAYEE-NAME     TO  EL685B-PAYEE-NA  (EL685B-INDEX)
01361      MOVE HOLD-CHECK-WRITTEN-DT
01362                               TO  DC-BIN-DATE-1
01363      MOVE ' '                 TO  DC-OPTION-CODE
01364      PERFORM 8500-DATE-CONVERSION
01365      MOVE DC-GREG-DATE-1-EDIT
01366                               TO  EL685B-CHK-DT (EL685B-INDEX).
01367
01368      IF EL685B-INDEX LESS THAN +18
01369          SET EL685B-INDEX UP BY +1.
01370
01371      MOVE +1                     TO  PI-END-OF-FILE.
01372      MOVE 'Y'                    TO  PI-EOF-SWT.
01373
01374      IF EL685B-INDEX GREATER +16
01375         MOVE 'Y'                 TO PI-SEND-TOT-SWT
01376         GO TO 5900-WRITE-REPORT.
01377
01378      MOVE +375                   TO  EMI-ERROR.
01379
01380      IF EL685B-INDEX LESS THAN +18
01381               MOVE ' CONTL'     TO EL685B-DESC-ONE (EL685B-INDEX)
01382               MOVE 'TOTAL'      TO  EL685B-DESC-TWO (EL685B-INDEX)
01383               MOVE PI-CONTROL-TOT
01384                                  TO EL685B-AMT (EL685B-INDEX)
01385               SET EL685B-INDEX UP BY +1
01386      ELSE
01387         NEXT SENTENCE.
01388
01389      IF EL685B-INDEX LESS THAN +18
01390         SET EL685B-INDEX UP BY +1
01391         MOVE PI-CONTROL-GRAND-TOT
01392                                  TO EL685B-AMT (EL685B-INDEX)
01393         MOVE ' GRAND'            TO EL685B-DESC-ONE (EL685B-INDEX)
01394         MOVE 'TOTAL'             TO EL685B-DESC-TWO (EL685B-INDEX)
01395      ELSE
01396         NEXT SENTENCE.
01397
01398  5900-WRITE-REPORT.
01399      MOVE -1                     TO  BPFKL
01400
01401      MOVE EIBTRMID               TO  WS-TS-TERM-ID
01402
01403      
      * EXEC CICS WRITEQ TS
01404 *        QUEUE  (WS-TEMP-STORAGE-KEY)
01405 *        ITEM   (PI-TEMP-STORAGE-ITEM)
01406 *        FROM   (EL685BI)
01407 *        LENGTH (WS-TS-LENGTH)
01408 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00006938' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL685BI, 
                 WS-TS-LENGTH, 
                 PI-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01409
01410      MOVE PI-TEMP-STORAGE-ITEM   TO  BPAGEO.
01411
01412      PERFORM 8100-SEND-INITIAL-MAP.
01413
01414  5990-EXIT.
01415      EXIT.
01416
01417      EJECT
01418  8100-SEND-INITIAL-MAP SECTION.
01419
01420      IF PI-COMPANY-ID = 'LAP'  OR  'RMC'
01421          IF NOT PI-CALLED-FROM-AR-MENU
01422              MOVE ' ACCOUNT'     TO ADESC1O
01423              MOVE 'CERT NO.'     TO ADESC2O.
01424
01425      IF CHECKS-TO-BE-PRINTED
01426          IF PI-CALLED-FROM-AR-MENU
01427              MOVE WS-TO-BE-PRINTED-PFDESC     TO BPFDESCO
01428              MOVE WS-AR-TO-BE-PRINTED-DESC    TO BTITLEO
01429              MOVE 'EL685B'                    TO ASCREENO
01430          ELSE
01431              MOVE WS-TO-BE-PRINTED-PFDESC     TO APFDESCO
01432              MOVE WS-TO-BE-PRINTED-DESC       TO ATITLEO
01433              MOVE 'EL685A'                    TO ASCREENO
01434      ELSE
01435          IF PI-CALLED-FROM-AR-MENU
01436              MOVE WS-CHECKS-PRINTED-PFDESC    TO BPFDESCO
01437              MOVE WS-AR-CHECKS-PRINTED-DESC   TO BTITLEO
01438              MOVE 'EL685D'                    TO BSCREENO
01439          ELSE
01440              MOVE WS-CHECKS-PRINTED-PFDESC    TO APFDESCO
01441              MOVE WS-CHECKS-PRINTED-DESC      TO ATITLEO
01442              MOVE 'EL685C'                    TO ASCREENO.
01443
01444      IF EMI-ERROR NOT = ZERO
01445          PERFORM 9900-ERROR-FORMAT.
01446
01447      MOVE EIBTIME              TO  WS-TIME-WORK.
01448      IF PI-CALLED-FROM-AR-MENU
01449          MOVE PI-CURRENT-DATE      TO  BDATEO
01450          MOVE WS-TIME              TO  BTIMEO
01451          MOVE EMI-MESSAGE-AREA (1) TO  BEMSG1O
01452      ELSE
01453          MOVE PI-CURRENT-DATE      TO  ADATEO
01454          MOVE WS-TIME              TO  ATIMEO
01455          MOVE EMI-MESSAGE-AREA (1) TO  AEMSG1O.
01456
01457      IF PI-CALLED-FROM-AR-MENU
01458          
      * EXEC CICS SEND
01459 *            FROM   (EL685BI)
01460 *            MAPSET (WS-MAPSET-NAME)
01461 *            MAP    (WS-MAP-NAME)
01462 *            CURSOR ERASE
01463 *        END-EXEC
           MOVE LENGTH OF
            EL685BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006993' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL685BI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01464      ELSE
01465          
      * EXEC CICS SEND
01466 *            FROM   (EL685AI)
01467 *            MAPSET (WS-MAPSET-NAME)
01468 *            MAP    (WS-MAP-NAME)
01469 *            CURSOR ERASE
01470 *        END-EXEC.
           MOVE LENGTH OF
            EL685AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007000' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL685AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01471
01472      PERFORM 9100-RETURN-TRAN.
01473
01474  8100-EXIT.
01475      EXIT.
01476
01477      EJECT
01478  8200-SEND-DATAONLY SECTION.
01479
01480      IF PI-COMPANY-ID = 'LAP'  OR  'RMC'
01481          IF NOT PI-CALLED-FROM-AR-MENU
01482              MOVE ' ACCOUNT'                  TO ADESC1O
01483              MOVE 'CERT NO.'                  TO ADESC2O.
01484
01485      IF CHECKS-TO-BE-PRINTED
01486          IF PI-CALLED-FROM-AR-MENU
01487              MOVE WS-TO-BE-PRINTED-PFDESC     TO BPFDESCO
01488              MOVE WS-AR-TO-BE-PRINTED-DESC    TO BTITLEO
01489              MOVE 'EL685B'                    TO BSCREENO
01490          ELSE
01491              MOVE WS-TO-BE-PRINTED-PFDESC     TO APFDESCO
01492              MOVE WS-TO-BE-PRINTED-DESC       TO ATITLEO
01493              MOVE 'EL685A'                    TO ASCREENO
01494      ELSE
01495          IF PI-CALLED-FROM-AR-MENU
01496              MOVE WS-CHECKS-PRINTED-PFDESC    TO BPFDESCO
01497              MOVE WS-AR-CHECKS-PRINTED-DESC   TO BTITLEO
01498              MOVE 'EL685D'                    TO BSCREENO
01499          ELSE
01500              MOVE WS-CHECKS-PRINTED-PFDESC    TO APFDESCO
01501              MOVE WS-CHECKS-PRINTED-DESC      TO ATITLEO
01502              MOVE 'EL685C'                    TO ASCREENO.
01503
01504      IF EMI-ERROR NOT = ZERO
01505          PERFORM 9900-ERROR-FORMAT.
01506
01507      MOVE EIBTIME                TO  WS-TIME-WORK.
01508      IF PI-CALLED-FROM-AR-MENU
01509          MOVE PI-CURRENT-DATE        TO  BDATEO
01510          MOVE WS-TIME                TO  BTIMEO
01511          MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O
01512      ELSE
01513          MOVE PI-CURRENT-DATE        TO  ADATEO
01514          MOVE WS-TIME                TO  ATIMEO
01515          MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
01516
01517      IF PI-CALLED-FROM-AR-MENU
01518          
      * EXEC CICS SEND DATAONLY
01519 *            FROM   (EL685AI)
01520 *            MAPSET (WS-MAPSET-NAME)
01521 *            MAP    (WS-MAP-NAME)
01522 *            CURSOR
01523 *        END-EXEC
           MOVE LENGTH OF
            EL685AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00007053' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL685AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01524      ELSE
01525          
      * EXEC CICS SEND DATAONLY
01526 *            FROM   (EL685BI)
01527 *            MAPSET (WS-MAPSET-NAME)
01528 *            MAP    (WS-MAP-NAME)
01529 *            CURSOR
01530 *        END-EXEC.
           MOVE LENGTH OF
            EL685BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00007060' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL685BI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01531
01532      PERFORM 9100-RETURN-TRAN.
01533
01534  8100-EXIT.
01535      EXIT.
01536
01537      EJECT
01538  8300-SEND-TEXT SECTION.
01539
01540      
      * EXEC CICS SEND TEXT
01541 *        FROM   (LOGOFF-TEXT)
01542 *        LENGTH (LOGOFF-LENGTH)
01543 *        ERASE  FREEKB
01544 *    END-EXEC,
      *    MOVE '8&      T  E F  H   F -   #00007075' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303735' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
01545
01546      
      * EXEC CICS RETURN
01547 *        END-EXEC.
      *    MOVE '.(                    ''   #00007081' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01548
01549
01550  8300-EXIT.
01551      EXIT.
01552
01553  8400-NOTFND.
01554      MOVE 0586 TO EMI-ERROR.
01555      PERFORM 9900-ERROR-FORMAT
01556         THRU 9900-EXIT.
01557
01558      IF  PI-CALLED-FROM-AR-MENU
01559          MOVE -1 TO BPFKL
01560      ELSE
01561          MOVE -1 TO APFKL.
01562
01563      GO TO 8100-SEND-INITIAL-MAP.
01564
01565  8400-EXIT.
01566      EXIT.
01567
01568      EJECT
01569  8500-DATE-CONVERSION SECTION.
01570
01571      
      * EXEC CICS LINK
01572 *        PROGRAM  ('ELDATCV')
01573 *        COMMAREA (DATE-CONVERSION-DATA)
01574 *        LENGTH   (DC-COMM-LENGTH)
01575 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00007106' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01576
01577  8500-EXIT.
01578      EXIT.
01579
01580      EJECT
01581  9000-RETURN-CICS SECTION.
01582
01583      MOVE 'EL005   '             TO  WS-PROGRAM-ID
01584      MOVE EIBAID                 TO  PI-ENTRY-CD-1
01585      PERFORM 9300-XCTL.
01586
01587  9000-EXIT.
01588      EXIT.
01589
01590  9100-RETURN-TRAN SECTION.
01591
01592      MOVE EMI-ERROR-NUMBER (1)  TO  PI-LAST-ERROR-NO
01593      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO
01594
01595      
      * EXEC CICS RETURN
01596 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01597 *        LENGTH   (PI-COMM-LENGTH)
01598 *        TRANSID  (WS-TRANS-ID)
01599 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00007130' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01600
01601  9100-EXIT.
01602      EXIT.
01603
01604  9300-XCTL SECTION.
01605      
      * EXEC CICS HANDLE CONDITION
01606 *        QIDERR (9300-NEXT-SENTENCE)
01607 *    END-EXEC.
      *    MOVE '"$N                   ! ) #00007140' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303037313430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01608
01609      MOVE EIBTRMID               TO  WS-TS-TERM-ID
01610
01611      
      * EXEC CICS DELETEQ TS
01612 *        QUEUE (WS-TEMP-STORAGE-KEY)
01613 *    END-EXEC.
      *    MOVE '*&                    #   #00007146' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01614
01615  9300-NEXT-SENTENCE.
01616
01617      MOVE DFHENTER               TO  EIBAID
01618
01619      
      * EXEC CICS XCTL
01620 *        PROGRAM  (WS-PROGRAM-ID)
01621 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01622 *        LENGTH   (PI-COMM-LENGTH)
01623 *    END-EXEC.
      *    MOVE '.$C                   %   #00007154' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PROGRAM-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01624
01625  9300-EXIT.
01626      EXIT.
01627
01628  9400-CLEAR SECTION.
01629
01630      MOVE PI-RETURN-TO-PROGRAM  TO  WS-PROGRAM-ID
01631      PERFORM 9300-XCTL.
01632
01633  9400-EXIT.
01634      EXIT.
01635
01636  9600-PGMIDERR SECTION.
01637
01638      
      * EXEC CICS HANDLE CONDITION
01639 *        PGMIDERR (8300-SEND-TEXT)
01640 *    END-EXEC.
      *    MOVE '"$L                   ! * #00007173' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303037313733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01641
01642      MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM
01643                                      LOGOFF-PGM
01644
01645      MOVE 'EL005   '             TO  WS-PROGRAM-ID
01646      MOVE SPACES                 TO  PI-ENTRY-CD-1
01647
01648      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL
01649
01650      PERFORM 9300-XCTL.
01651
01652  9600-EXIT.
01653      EXIT.
01654
01655      EJECT
01656  9900-ERROR-FORMAT SECTION.
01657
01658      
      * EXEC CICS LINK
01659 *        PROGRAM  ('EL001')
01660 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01661 *        LENGTH   (EMI-COMM-LENGTH)
01662 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00007193' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01663
01664  9900-EXIT.
01665      EXIT.
01666
01667  9990-ERROR SECTION.
01668
01669      MOVE DFHEIBLK TO EMI-LINE1.
01670      
      * EXEC CICS LINK
01671 *        PROGRAM  ('EL004')
01672 *        COMMAREA (EMI-LINE1)
01673 *        LENGTH   (72)
01674 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007205' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01675
01676      IF PI-CALLED-FROM-AR-MENU
01677          MOVE -1 TO BPFKL
01678      ELSE
01679          MOVE -1 TO APFKL.
01680      PERFORM 8100-SEND-INITIAL-MAP.
01681      GO TO 9100-RETURN-TRAN.
01682
01683  9990-EXIT.
01684      EXIT.
01685
01686  9999-LAST-PARAGRAPH SECTION.
01687
01688      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL685' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01689

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL685' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0015-NEXT-SENTENCE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0130-TERMID-ERROR,
                     0130-TRANS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8400-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 4800-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8400-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5800-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 9300-NEXT-SENTENCE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL685' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
