00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL630.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 04/19/94 08:59:00.
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.046.
00009
00010 *AUTHOR.     LOGIC,INC.
00011 *            DALLAS, TEXAS.
00012
00013 *DATE-COMPILED.
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.    TRANSACTION - EXA5 - NEW BUSINESS - DATA ENTRY.
103001*
103001******************************************************************
103001*                   C H A N G E   L O G
103001*
103001* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103001*-----------------------------------------------------------------
103001*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103001* EFFECTIVE    NUMBER
103001*-----------------------------------------------------------------
103001* 103001    2001100100006  SMVA  CHECK FOR ? MARKS ANYWHERE IN
103001*                              BATCH NUMBER
072308* 072308  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
111109* 111109  CR2008100900003  AJRA  ADD NEW CERT NOTE TABLE
030310* 030310  CR2009031200002  PEMA  OPEN LOAN OFFICER ON 6301
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
103001******************************************************************
00025
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL630 WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.046 *********'.
00035
00036 *    COPY ELCSCTM.
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
00037 *    COPY ELCSCRTY.
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
00038
00039      EJECT
00040
00041  01  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1900.
00042  01  STANDARD-AREAS.
00043      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.
00044      12  GETMAIN-SPACE           PIC X       VALUE SPACE.
00045      12  MAP-NAME                PIC X(8)    VALUE 'EL630A'.
00046      12  MAPSET-NAME             PIC X(8)    VALUE 'EL630S '.
00047      12  SCREEN-NUMBER           PIC X(4)    VALUE '630A'.
00048      12  TRANS-ID                PIC X(4)    VALUE 'EXA5'.
00049      12  EDIT-TRANS              PIC X(4)    VALUE 'EXEB'.
00050      12  PASS-AREA-LEN           PIC S9(4)   COMP VALUE +16.
00051      12  THIS-PGM                PIC X(8)    VALUE 'EL630'.
00052      12  PGM-NAME                PIC X(8)    VALUE SPACES.
00053      12  TIME-IN                 PIC S9(7)   VALUE ZEROS.
00054      12  TIME-OUT-R  REDEFINES TIME-IN.
00055          16  FILLER              PIC X.
00056          16  TIME-OUT            PIC 99V99.
00057          16  FILLER              PIC X(2).
00058      12  XCTL-005                PIC X(8)    VALUE 'EL005'.
00059      12  XCTL-010                PIC X(8)    VALUE 'EL010'.
00060      12  XCTL-626                PIC X(8)    VALUE 'EL626'.
00061      12  XCTL-6301               PIC X(8)    VALUE 'EL6301'.
00062      12  XCTL-6302               PIC X(8)    VALUE 'EL6302'.
00063      12  XCTL-633                PIC X(8)    VALUE 'EL633'.
00064      12  XCTL-633DMD             PIC X(8)    VALUE 'EL633DMD'.
00065      12  XCTL-635                PIC X(8)    VALUE 'EL635'.
00066      12  LINK-001                PIC X(8)    VALUE 'EL001'.
00067      12  LINK-004                PIC X(8)    VALUE 'EL004'.
00068      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00069      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.
00070      12  ERPNDB-FILE-ID          PIC X(8)    VALUE 'ERPNDB'.
00071      12  ERACCT-FILE-ID          PIC X(8)    VALUE 'ERACCT'.
00072      12  ERACCT2-FILE-ID         PIC X(8)    VALUE 'ERACCT2'.
00073      12  ELCERT-FILE-ID          PIC X(8)    VALUE 'ELCERT'.
00074      12  ERNOTE-FILE-ID          PIC X(8)    VALUE 'ERNOTE'.
111109     12  ERCNOT-FILE-ID          PIC X(8)    VALUE 'ERCNOT'.
00075      12  ERMAIL-FILE-ID          PIC X(8)    VALUE 'ERMAIL'.
00076      12  ERCOMP-FILE-ID          PIC X(8)    VALUE 'ERCOMP'.
00077      12  ERPNDM-FILE-ID          PIC X(8)    VALUE 'ERPNDM'.
00078      12  ERRQST-FILE-ID          PIC X(8)    VALUE 'ERRQST'.
00079      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.
00080      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.
00081      12  WS-CURRENT-YMD          PIC 9(6)    VALUE ZEROS.
00082      12  WS-SYNC-CNTR            PIC S9(3)   VALUE +0 COMP-3.
00083      12  WS-SUB1                 PIC S9(4)   VALUE +0 COMP.
00084      12  WS-SUB2                 PIC S9(4)   VALUE +0 COMP.
103001     12  PIC6-SUB                PIC S9(04)  VALUE +0 COMP.
00085      12  WS-AR-REPORTING         PIC X       VALUE SPACE.
00086          88  AR-NET-REPORT               VALUE 'N'.
00087          88  AR-GROSS-REPORT             VALUE 'G'.
00088      12  WS-COMP-MASTER-ERROR-SW PIC X       VALUE SPACE.
00089          88  WS-COMP-MASTER-ERROR        VALUE 'Y'.
00090
00091      12  WS-UPDATE-REFERENCE-SW  PIC X       VALUE SPACE.
00092          88  WS-UPDATE-REFERENCE         VALUE 'Y'.
00093
103001     12  WS-SWITCH               PIC X(01)   VALUE SPACE.
103001         88  QUESTION-MARK-FOUND             VALUE 'Y'.
103001
00094      12  WS-RECORD-FOUND-SW      PIC X       VALUE SPACE.
00095          88  RECORD-FOUND                VALUE 'Y'.
00096          88  RECORD-NOT-FOUND            VALUE 'N'.
00097
00098      12  WS-ON1-SW1              PIC X(01)   VALUE 'Y'.
00099      12  WS-ON1-SW2              PIC X(01)   VALUE 'Y'.
00100      12  WS-ON1-SW3              PIC X(01)   VALUE 'Y'.
00101      12  WS-ON1-SW4              PIC X(01)   VALUE 'Y'.
00102      12  WS-ON1-SW5              PIC X(01)   VALUE 'Y'.
00103      12  WS-ON1-SW6              PIC X(01)   VALUE 'Y'.
00104      12  WS-ON1-SW7              PIC X(01)   VALUE 'Y'.
00105
00106      12  WS-ERPNDB-RECORD        PIC X(585)  VALUE SPACES.
00107
00108  01  BATCH-TO-PROCESS.
00109      05  EDIT-COMPANY-CD         PIC X       VALUE LOW-VALUES.
00110      05  EDIT-BATCH              PIC X(6)    VALUE SPACES.
00111      05  EDIT-COMPANY-ID         PIC XXX     VALUE SPACES.
00112      05  EDIT-RESTART-BATCH      PIC X(6)    VALUE SPACES.
00113
00114      EJECT
00115  01  ERROR-MESSAGES.
00116      12  ER-0008                 PIC X(4)    VALUE '0008'.
00117      12  ER-0023                 PIC X(4)    VALUE '0023'.
00118      12  ER-0029                 PIC X(4)    VALUE '0029'.
00119      12  ER-0070                 PIC X(4)    VALUE '0070'.
00120      12  ER-0194                 PIC X(4)    VALUE '0194'.
00121      12  ER-0195                 PIC X(4)    VALUE '0195'.
00122      12  ER-0196                 PIC X(4)    VALUE '0196'.
00123      12  ER-0197                 PIC X(4)    VALUE '0197'.
00124      12  ER-0340                 PIC X(4)    VALUE '0340'.
00125      12  ER-0587                 PIC X(4)    VALUE '0587'.
00126      12  ER-0905                 PIC X(4)    VALUE '0905'.
00127      12  ER-2119                 PIC X(4)    VALUE '2119'.
00128      12  ER-2126                 PIC X(4)    VALUE '2126'.
00129      12  ER-2132                 PIC X(4)    VALUE '2132'.
00130      12  ER-2201                 PIC X(4)    VALUE '2201'.
00131      12  ER-2208                 PIC X(4)    VALUE '2208'.
00132      12  ER-2209                 PIC X(4)    VALUE '2209'.
00133      12  ER-2210                 PIC X(4)    VALUE '2210'.
00134      12  ER-2211                 PIC X(4)    VALUE '2211'.
00135      12  ER-2212                 PIC X(4)    VALUE '2212'.
00136      12  ER-2213                 PIC X(4)    VALUE '2213'.
00137      12  ER-2214                 PIC X(4)    VALUE '2214'.
00138      12  ER-2215                 PIC X(4)    VALUE '2215'.
00139      12  ER-2216                 PIC X(4)    VALUE '2216'.
00140      12  ER-2229                 PIC X(4)    VALUE '2229'.
00141      12  ER-2242                 PIC X(4)    VALUE '2242'.
00142      12  ER-2248                 PIC X(4)    VALUE '2248'.
00143      12  ER-2370                 PIC X(4)    VALUE '2370'.
00144      12  ER-2371                 PIC X(4)    VALUE '2371'.
00145      12  ER-2402                 PIC X(4)    VALUE '2402'.
00146      12  ER-2422                 PIC X(4)    VALUE '2422'.
00147      12  ER-2800                 PIC X(4)    VALUE '2800'.
00148      12  ER-2880                 PIC X(4)    VALUE '2880'.
00149      12  ER-2990                 PIC X(4)    VALUE '2990'.
00150
00151      EJECT
00152  01  ACCESS-KEYS.
00153      12  ELCNTL-KEY.
00154          16  CNTL-COMP-ID        PIC X(3)  VALUE SPACES.
00155          16  CNTL-REC-TYPE       PIC X     VALUE SPACES.
00156          16  CNTL-ACCESS.
00157              20  CNTL-STATE      PIC XX    VALUE SPACES.
00158              20  FILLER          PIC X     VALUE SPACES.
00159              20  CNTL-CARRIER    PIC X     VALUE SPACES.
00160          16  CNTL-SEQ            PIC S9(4) VALUE +0 COMP.
00161
00162      12  ERPNDB-KEY.
00163          16  PNDB-COMP-CD        PIC X     VALUE SPACE.
00164          16  PNDB-ENTRY-BATCH    PIC X(6)  VALUE SPACES.
00165          16  PNDB-BATCH-SEQ      PIC S9(4) VALUE +0 COMP.
00166          16  PNDB-BATCH-CHG-SEQ  PIC S9(4) VALUE +0 COMP.
00167
00168      12  ERACCT-KEY.
00169          16  ERACCT-COMP-KEY.
00170              20  ACCT-CO         PIC X     VALUE SPACES.
00171              20  ACCT-CARRIER    PIC X     VALUE SPACES.
00172              20  ACCT-GROUPING   PIC X(6)  VALUE SPACES.
00173              20  ACCT-STATE      PIC XX    VALUE SPACES.
00174              20  ACCT-ACCOUNT    PIC X(10) VALUE SPACES.
00175          16  ACCT-EXP-DATE       PIC XX    VALUE SPACES.
00176          16  FILLER              PIC X(4)  VALUE LOW-VALUES.
00177      12  ERACCT-SAVE-KEY         PIC X(20) VALUE SPACES.
00178
00179      12  ELCERT-KEY.
00180          16  CERT-COMPANY-CD     PIC X     VALUE SPACES.
00181          16  CERT-CARRIER        PIC X     VALUE SPACES.
00182          16  CERT-GROUPING       PIC X(6)  VALUE SPACES.
00183          16  CERT-STATE          PIC XX    VALUE SPACES.
00184          16  FILLER              PIC X(23) VALUE SPACES.
00185
041320     12  ernote-generic-key-len  pic s9(4) comp value +33.
041320     12  ernote-key.
041320         16  ernote-company-cd   pic x     value spaces.
041320         16  ernote-carrier      pic x     value spaces.
041320         16  ernote-grouping     pic x(6)  value spaces.
041320         16  ernote-state        pic xx    value spaces.
041320         16  ernote-account      pic x(10) value spaces.
041320         16  ernote-cert-eff-dt  pic xx    value spaces.
041320         16  ernote-cert-no      pic x(11) value spaces.
041320         16  ernote-rec-type     pic x     value spaces.
00186      12  ERCOMP-KEY.
00187          16  ERCOMP-COMP-CD      PIC X     VALUE SPACE.
00188          16  ERCOMP-CARRIER      PIC X     VALUE SPACES.
00189          16  ERCOMP-GROUPING     PIC X(6)  VALUE SPACES.
00190          16  ERCOMP-FIN-RESP     PIC X(10) VALUE SPACES.
00191          16  ERCOMP-ACCT         PIC X(10) VALUE SPACES.
00192          16  ERCOMP-RECORD-TYPE  PIC X     VALUE SPACES.
00193
00194      12  ERRQST-KEY.
00195          16  ERRQST-COMP-CD      PIC X     VALUE SPACE.
00196          16  ERRQST-ENTRY-BATCH  PIC X(6)  VALUE SPACES.
111109
111109     12  ERCNOT-KEY.
111109         16  ERCNOT-PART-KEY.
111109             20  ERCNOT-COMPANY-CD   PIC X        VALUE SPACE.
111109             20  ERCNOT-CARRIER      PIC X        VALUE SPACES.
111109             20  ERCNOT-GROUPING     PIC X(6)     VALUE SPACES.
111109             20  ERCNOT-STATE        PIC XX       VALUE SPACES.
111109             20  ERCNOT-ACCOUNT      PIC X(10)    VALUE SPACES.
111109             20  ERCNOT-CERT-EFF-DT  PIC XX       VALUE SPACES.
111109             20  ERCNOT-CERT-NO.
111109                 25  ERCNOT-CERT-PRIME PIC X(10)  VALUE SPACES.
111109                 25  ERCNOT-CERT-SFX PIC X        VALUE SPACES.
111109         16  ERCNOT-REC-TYPE         PIC X        VALUE SPACES.
111109         16  ERCNOT-SEQUENCE         PIC S9(4) COMP VALUE +0.
111109
00197
00198  01  FILLER.
041320     12  W-RESPONSE              PIC S9(8)   COMP.
041320         88  RESP-NORMAL               VALUE +00.
041320         88  RESP-NOTFND               VALUE +13.
041320         88  RESP-DUPREC               VALUE +14.
041320         88  RESP-DUPKEY               VALUE +15.
041320         88  RESP-NOTOPEN              VALUE +19.
041320         88  RESP-ENDFILE              VALUE +20.
041320         88  resp-lengtherr            value +22.
00199      12  WS-DEEDIT-FIELD         PIC S9(8)V99 VALUE ZEROS.
00200      12  WS-DT-DEEDIT-FIELD REDEFINES
00201          WS-DEEDIT-FIELD         PIC X(10).
00202      12  WS-DEEDIT-FIELD-DATE  REDEFINES  WS-DEEDIT-FIELD.
00203          16   FILLER                           PIC X(04).
00204          16   WS-DEEDIT-FIELD-DATE-OUT         PIC X(06).
00205      12  QUESTION-MARKS          PIC X(6)      VALUE '??????'.
103001     12  QUESTION-MARK           PIC X(01)     VALUE '?'.
00206      12  WS-BATCH-NO             PIC 9(6)      VALUE ZEROS.
00207      12  WS-OBAL                 PIC S9(8)V99  VALUE ZEROS.
00208      12  WS-OCNT                 PIC S9(5)     VALUE ZEROS.
00209      12  WS-DELETE-CNT           PIC S9(5)     VALUE ZEROS.
00210      12  WS-PF1-SW               PIC X         VALUE SPACES.
00211          88  WS-PF1                            VALUE 'Y'.
00212
00213      12  WS-SAV-PNDB-KEY.
00214          16  WS-SAV-COMP-CD        PIC X       VALUE LOW-VALUES.
00215          16  WS-SAV-ENTRY-BATCH    PIC X(6)       VALUE SPACES.
00216          16  WS-SAV-BATCH-SEQ      PIC S9(4) COMP VALUE ZEROS.
00217          16  WS-SAV-BATCH-CHG-SEQ  PIC S9(4) COMP VALUE ZEROS.
00218
00219      12  DATE-TEST-AREA          PIC 9(6)         VALUE ZEROS.
00220      12  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.
00221          16  DATE-TEST-MM        PIC 99.
00222          16  DATE-TEST-DD        PIC 99.
00223          16  DATE-TEST-YY        PIC 99.
00224      12  DIVIDE-RESULT           PIC 99           VALUE ZEROS.
00225      12  DIVIDE-REMAINDER        PIC 9            VALUE 0.
00226      12  WS-CERT-NOTE-SW         PIC X            VALUE ' '.
00227          88 CERT-NOTES-ARE-PRESENT   VALUE 'Y'.
00228      12  WS-CERT-ADDRESS-SW      PIC X            VALUE ' '.
00229          88 CERT-ADDRESS-PRESENT     VALUE 'Y'.
00230      12  WS-PRM-HEADER.
00231          16  WS-PRM-OVERRIDE     PIC XX           VALUE SPACES.
00232          16  FILLER              PIC X(8)  VALUE '-PREMIUM'.
00233      12  WS-REFUND-HEADER.
00234          16  WS-REFUND-OVERRIDE  PIC XX    VALUE SPACES.
00235          16  FILLER              PIC X(7)  VALUE '-REFUND'.
00236
00237      EJECT
00238
00239 *    COPY ELCDATE.
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
00240
00241      EJECT
00242 *    COPY ELCLOGOF.
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
00243
00244      EJECT
00245 *    COPY ELCATTR.
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
00246
00247      EJECT
00248 *    COPY ELCEMIB.
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
00249
00250      EJECT
00251 *    COPY ELCINTF.
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
00252 *    COPY ELC630PI.
00001 ******************************************************************
00004 *                                                                *
00002 *                            ELC630PI                            *
00003 *                            VMOD=2.014                          *
00004 *                                                                *
00005 * - PI-PROGRAM-WORK-AREA FOR THE DATA-ENTRY SUB-SYSTEM -         *
00006 *                                                                *
00007 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK.                   *
00008 *                                                                *
00009 *               EL630 - EL6301 - EL6302                          *
00010 ******************************************************************
072308*                   C H A N G E   L O G
072308*
072308* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
072308*-----------------------------------------------------------------
072308*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
072308* EFFECTIVE    NUMBER
072308*-----------------------------------------------------------------
072308* 072308  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
030310* 030310  CR2009031200002  PEMA  OPEN LOAN OFFICER FIELD
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
00017 ******************************************************************
00011
00012      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00013          16  PI-AM-NAME                  PIC X(30).
00014          16  PI-MAP-NAME                 PIC X(8).
00015          16  PI-BATCH-AMOUNTS    COMP-3.
00016              20  PI-LF-ISS-REMITTED      PIC S9(8)V99.
00017              20  PI-LF-ISS-ENTERED       PIC S9(8)V99.
00018              20  PI-LF-CAN-REMITTED      PIC S9(8)V99.
00019              20  PI-LF-CAN-ENTERED       PIC S9(8)V99.
00020              20  PI-AH-ISS-REMITTED      PIC S9(8)V99.
00021              20  PI-AH-ISS-ENTERED       PIC S9(8)V99.
00022              20  PI-AH-CAN-REMITTED      PIC S9(8)V99.
00023              20  PI-AH-CAN-ENTERED       PIC S9(8)V99.
00024              20  PI-ISS-CNT-REMITTED     PIC S9(5).
00025              20  PI-ISS-CNT-ENTERED      PIC S9(5).
00026              20  PI-CAN-CNT-REMITTED     PIC S9(5).
00027              20  PI-CAN-CNT-ENTERED      PIC S9(5).
00028          16  PI-MAINT-FUNC               PIC X.
00029          16  PI-ERROR-SW                 PIC X.
00030              88  PI-DATA-ERRORS              VALUE 'Y'.
00031          16  PI-UPDATE-SW                PIC X.
00032              88  PI-DATA-UPDATED             VALUE 'Y'.
00033          16  PI-DISPLAY-SW               PIC X.
00034              88  PI-LAST-FUNC-DISPLAY        VALUE 'Y'.
00035          16  PI-SAVE-CALLING-PGM         PIC X(8).
00036          16  PI-LAST-SEQ-NO-ADDED        PIC S9(4) COMP.
00037          16  PI-NEXT-DISPLAY-SEQ-NO      PIC S9(4) COMP.
00038          16  PI-SAV-CARRIER              PIC X.
00039          16  PI-SAV-GROUPING             PIC X(6).
00040          16  PI-SAV-STATE                PIC XX.
00041          16  PI-SAV-ACCOUNT              PIC X(10).
00042          16  PI-SAV-CERT-EFF-DT          PIC XX.
00043          16  PI-SAV-CERT-NO.
00044              20  PI-SAV-CERT-PRIME       PIC X(14).
00045              20  PI-SAV-CERT-SFX         PIC X.
00046          16  PI-PYAJ-REFERENCE REDEFINES PI-SAV-CERT-NO.
00047              20  PI-SAV-PYAJ-REFERENCE   PIC X(12).
00048              20  FILLER                  PIC X(3).
00049          16  PI-SAV-ENDING-ERPNDB-KEY.
00050              20  PI-SAV-COMP-CD          PIC X.
00051              20  PI-SAV-ENTRY-BATCH      PIC X(6).
00052              20  PI-SAV-BATCH-SEQ        PIC S9(4) COMP.
00053              20  PI-SAV-BATCH-CHG-SEQ    PIC S9(4) COMP.
00054          16  PI-SAV-REFERENCE            PIC X(12).
00055          16  PI-SAV-FULL-CONTROL.
00056              20  PI-SAV-FC-CARRIER       PIC X.
00057              20  PI-SAV-FC-GROUPING      PIC X(6).
00058              20  PI-SAV-FC-STATE         PIC XX.
00059          16  PI-VERIFY-DELETE-SW         PIC X.
00060              88  PI-DELETE-IS-OK             VALUE 'Y'.
00061          16  PI-EL630-FIRST-TIME-SW      PIC X.
00062              88  PI-EL630-FIRST-TIME         VALUE SPACE.
00063          16  PI-CREDIT-EDIT-CONTROLS.
00064              20  PI-MIN-PREMIUM          PIC S9(3)V99  COMP-3.
00065              20  PI-MIN-AGE              PIC 99.
00066              20  PI-DEFAULT-AGE          PIC 99.
00067              20  PI-MIN-TERM             PIC S9(3)     COMP-3.
00068              20  PI-MAX-TERM             PIC S9(3)     COMP-3.
00069              20  PI-DEFAULT-SEX          PIC X.
00070              20  PI-JOINT-AGE-INPUT      PIC X.
00071                  88 PI-JOINT-AGE-IS-INPUT       VALUE '1'.
00072              20  PI-BIRTH-DATE-INPUT     PIC X.
00073                  88 PI-BIRTH-DATE-IS-INPUT      VALUE '1'.
00074          16  PI-KEYED-SWITCHES.
00075              20  PI-ISS-SUFFIX-KEYED-SW  PIC X.
00076                  88  PI-ISS-SUFFIX-KEYED     VALUE 'Y'.
00077              20  PI-CAN-SUFFIX-KEYED-SW  PIC X.
00078                  88  PI-CAN-SUFFIX-KEYED     VALUE 'Y'.
00079              20  PI-IG-KEYED-SW          PIC X.
00080                  88  PI-IG-KEYED             VALUE 'Y'.
00081              20  PI-APR-KEYED-SW         PIC X.
00082                  88  PI-APR-KEYED            VALUE 'Y'.
00083 *            20  PI-FREQ-KEYED-SW        PIC X.
00084 *                88  PI-FREQ-KEYED           VALUE 'Y'.
00083              20  PI-VIN-KEYED-SW         PIC X.
00084                  88  PI-VIN-KEYED            VALUE 'Y'.
00085              20  PI-SIG-KEYED-SW         PIC X.
00086                  88  PI-SIG-KEYED            VALUE 'Y'.
00087              20  PI-LFRT-KEYED-SW        PIC X.
00088                  88  PI-LFRT-KEYED           VALUE 'Y'.
00089              20  PI-AHRT-KEYED-SW        PIC X.
00090                  88  PI-AHRT-KEYED           VALUE 'Y'.
00091              20  PI-SSNUM-KEYED-SW       PIC X.
00092                  88  PI-SSNUM-KEYED          VALUE 'Y'.
00093              20  PI-JNT-SSNUM-KEYED-SW   PIC X.
00094                  88  PI-JNT-SSNUM-KEYED      VALUE 'Y'.
00095              20  PI-MEMBER-KEYED-SW      PIC X.
00096                  88  PI-MEMBER-KEYED         VALUE 'Y'.
00097              20  PI-MODE-KEYED-SW        PIC X.
00098                  88  PI-MODE-KEYED           VALUE 'Y'.
00099              20  PI-PMTS-KEYED-SW        PIC X.
00100                  88  PI-PMTS-KEYED           VALUE 'Y'.
00101              20  PI-LN-OFFICER-KEYED-SW  PIC X.
00102                  88  PI-LN-OFFICER-KEYED     VALUE 'Y'.
00103              20  PI-ENTRY-KEYED-SW       PIC X.
00104                  88  PI-ENTRY-KEYED          VALUE 'Y'.
00105              20  PI-FORCE-KEYED-SW       PIC X.
00106                  88  PI-FORCE-KEYED          VALUE 'Y'.
00107              20  PI-RINCD-KEYED-SW       PIC X.
00108                  88  PI-RINCD-KEYED          VALUE 'Y'.
00109              20  PI-BILLCD-KEYED-SW      PIC X.
00110                  88  PI-BILLCD-KEYED         VALUE 'Y'.
00111              20  PI-RTCLS-KEYED-SW       PIC X.
00112                  88  PI-RTCLS-KEYED          VALUE 'Y'.
00113              20  PI-LNTRM-KEYED-SW       PIC X.
00114                  88  PI-LNTRM-KEYED          VALUE 'Y'.
00115              20  PI-EXPIR-KEYED-SW       PIC X.
00116                  88  PI-EXPIR-KEYED          VALUE 'Y'.
00117              20  PI-PMT-KEYED-SW         PIC X.
00118                  88  PI-PMT-KEYED            VALUE 'Y'.
00119              20  PI-1ST-PMT-KEYED-SW     PIC X.
00120                  88  PI-1ST-PMT-KEYED        VALUE 'Y'.
00121              20  PI-DAYS-KEYED-SW        PIC X.
00122                  88  PI-DAYS-KEYED           VALUE 'Y'.
00123              20  PI-SKPCD-KEYED-SW       PIC X.
00124                  88  PI-SKPCD-KEYED          VALUE 'Y'.
00125              20  PI-JNT-AGE-KEYED-SW     PIC X.
00126                  88  PI-JNT-AGE-KEYED        VALUE 'Y'.
00127              20  PI-JNT-NAME-KEYED-SW    PIC X.
00128                  88  PI-JNT-NAME-KEYED       VALUE 'Y'.
00129              20  PI-ISS-LIVES-KEYED-SW   PIC X.
00130                  88  PI-ISS-LIVES-KEYED      VALUE 'Y'.
00131              20  PI-CAN-LIVES-KEYED-SW   PIC X.
00132                  88  PI-CAN-LIVES-KEYED      VALUE 'Y'.
00133              20  PI-PAYEE-KEYED-SW       PIC X.
00134                  88  PI-PAYEE-KEYED          VALUE 'Y'.
00135              20  PI-CHK-REQ-KEYED-SW     PIC X.
00136                  88  PI-CHK-REQ-KEYED        VALUE 'Y'.
00137              20  PI-ZIP4-KEYED-SW        PIC X.
00138                  88  PI-ZIP4-KEYED           VALUE 'Y'.
00139              20  PI-POLICY-KEYED-SW      PIC X.
00140                  88  PI-POLICY-KEYED         VALUE 'Y'.
00141              20  PI-EXPIRE-KEYED-SW      PIC X.
00142                  88  PI-EXPIRE-KEYED         VALUE 'Y'.
00143              20  PI-CRIT-PERD-KEYED-SW    PIC X.
00144                  88  PI-CRIT-PERD-KEYED      VALUE 'Y'.
00145              20  PI-BENEFICIARY-KEYED-SW PIC X.
00146                  88  PI-BENEFICIARY-KEYED    VALUE 'Y'.
00147              20  PI-PHONE-KEYED-SW       PIC X.
00148                  88  PI-PHONE-KEYED          VALUE 'Y'.
00149              20  PI-ALT-BEN-KEYED-SW     PIC X.
00150                  88  PI-ALT-BEN-KEYED        VALUE 'Y'.
00151              20  PI-ALT-PREM-KEYED-SW    PIC X.
00152                  88  PI-ALT-PREM-KEYED       VALUE 'Y'.
00153              20  PI-REFUND-MTHD-KEYED-SW PIC X.
00154                  88  PI-REFUND-MTHD-KEYED    VALUE 'Y'.
00155          16  PI-ACCT-LOW-EFF-DT          PIC XX.
00156          16  PI-ACCT-HIGH-EXP-DT         PIC XX.
00157          16  PI-BATCH-EOF-SW             PIC X.
00158              88  PI-BATCH-EOF                VALUE 'Y'.
00159          16  PI-NB-MONTH-END-DT          PIC XX.
00160          16  PI-ISSUE-ADDED-SW           PIC X.
00161              88  PI-ISSUE-ADDED              VALUE 'Y'.
00162          16  PI-BROWSE-SW                PIC X.
00163              88  PI-BROWSE                   VALUE 'Y'.
00164          16  PI-ACCT-AGENT-ERROR-SW      PIC X.
00165              88  PI-ACCT-AGENT-ERROR         VALUE 'Y'.
00166          16  PI-FIN-RESP-ERROR-SW        PIC X.
00167              88  PI-FIN-RESP-ERROR           VALUE 'Y'.
071211         16  PI-CAN-REA-KEYED-SW         PIC X.
071211             88  PI-CAN-REA-KEYED            VALUE 'Y'.
030310         16  PI-AM-EDIT-LOAN-OFC         PIC X.
072308         16  PI-AM-ADDR1                 PIC X(30).
072308         16  PI-AM-ADDR2                 PIC X(30).
               16  PI-AM-CITYST.
072308             20  PI-AM-CITY              PIC X(28).
                   20  PI-AM-STATE             PIC XX.
072308         16  PI-AM-ZIP                   PIC X(9).
072308         16  FILLER                      PIC X(290).
072308*        16  FILLER                      PIC X(390).
00171      12  PI-MISC.
00172          16  PI-ACCT-DATE-RANGES OCCURS 32 TIMES.
00173              20  PI-ACCT-EFF-DT          PIC XX.
00174              20  PI-ACCT-EXP-DT          PIC XX.
00175              20  PI-REMIT-AGENT          PIC X(10).
00176              20  PI-ACCT-AGENT           PIC X(10).
00177          16  PI-ACCOUNT-AGENT            PIC X(10).
00178          16  PI-FIN-RESP                 PIC X(10).
00179          16  PI-SUMMARY-CODE             PIC X(6).
00180          16  PI-SUB                      PIC S9(4) COMP.
00181          16  PI-COMP-CARRIER             PIC X.
00182          16  PI-COMP-GROUPING            PIC X(6).
00183          16  PI-ACCT-AGENT-PROCESSED-SW  PIC X.
00184              88  PI-ACCT-AGENT-PROCESSED     VALUE 'Y'.
00185          16  PI-CLEAR-ERROR-SW           PIC X.
00186              88  PI-CLEAR-ERROR              VALUE 'Y'.
00187          16  PI-AGE-KEYED-SW             PIC X.
00188              88  PI-AGE-KEYED                VALUE 'Y'.
00189          16  PI-BIRTHDT-KEYED-SW         PIC X.
00190              88  PI-BIRTHDT-KEYED            VALUE 'Y'.
00191          16  PI-RECEIVED-DT              PIC XX.
00192          16  PI-CSR-ID                   PIC X(4).
00193
00194      EJECT
00253           16  FILLER              PIC X(390).
00254
00255      EJECT
00256 *    COPY ELCAID.
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
00257  01  FILLER    REDEFINES DFHAID.
00258      12  FILLER              PIC X(8).
00259      12  PF-VALUES           PIC X       OCCURS 2.
00260      12  FILLER              PIC X(25).
00261
00262      EJECT
00263 *    COPY EL630S.
       01  EL630AI.
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
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  REFHDGL PIC S9(0004) COMP.
           05  REFHDGF PIC  X(0001).
           05  FILLER REDEFINES REFHDGF.
               10  REFHDGA PIC  X(0001).
           05  REFHDGI PIC  X(0015).
      *    -------------------------------
           05  CARRHDGL PIC S9(0004) COMP.
           05  CARRHDGF PIC  X(0001).
           05  FILLER REDEFINES CARRHDGF.
               10  CARRHDGA PIC  X(0001).
           05  CARRHDGI PIC  X(0007).
      *    -------------------------------
           05  GRPHDGL PIC S9(0004) COMP.
           05  GRPHDGF PIC  X(0001).
           05  FILLER REDEFINES GRPHDGF.
               10  GRPHDGA PIC  X(0001).
           05  GRPHDGI PIC  X(0008).
      *    -------------------------------
           05  STHDGL PIC S9(0004) COMP.
           05  STHDGF PIC  X(0001).
           05  FILLER REDEFINES STHDGF.
               10  STHDGA PIC  X(0001).
           05  STHDGI PIC  X(0005).
      *    -------------------------------
           05  BATCHL PIC S9(0004) COMP.
           05  BATCHF PIC  X(0001).
           05  FILLER REDEFINES BATCHF.
               10  BATCHA PIC  X(0001).
           05  BATCHI PIC  X(0006).
      *    -------------------------------
           05  REFL PIC S9(0004) COMP.
           05  REFF PIC  X(0001).
           05  FILLER REDEFINES REFF.
               10  REFA PIC  X(0001).
           05  REFI PIC  X(0012).
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
           05  ACCOUNTL PIC S9(0004) COMP.
           05  ACCOUNTF PIC  X(0001).
           05  FILLER REDEFINES ACCOUNTF.
               10  ACCOUNTA PIC  X(0001).
           05  ACCOUNTI PIC  X(0010).
      *    -------------------------------
           05  LFPHDGL PIC S9(0004) COMP.
           05  LFPHDGF PIC  X(0001).
           05  FILLER REDEFINES LFPHDGF.
               10  LFPHDGA PIC  X(0001).
           05  LFPHDGI PIC  X(0010).
      *    -------------------------------
           05  AHPHDGL PIC S9(0004) COMP.
           05  AHPHDGF PIC  X(0001).
           05  FILLER REDEFINES AHPHDGF.
               10  AHPHDGA PIC  X(0001).
           05  AHPHDGI PIC  X(0010).
      *    -------------------------------
           05  LFRHDGL PIC S9(0004) COMP.
           05  LFRHDGF PIC  X(0001).
           05  FILLER REDEFINES LFRHDGF.
               10  LFRHDGA PIC  X(0001).
           05  LFRHDGI PIC  X(0009).
      *    -------------------------------
           05  AHRHDGL PIC S9(0004) COMP.
           05  AHRHDGF PIC  X(0001).
           05  FILLER REDEFINES AHRHDGF.
               10  AHRHDGA PIC  X(0001).
           05  AHRHDGI PIC  X(0009).
      *    -------------------------------
           05  EISSCNTL PIC S9(0004) COMP.
           05  EISSCNTF PIC  X(0001).
           05  FILLER REDEFINES EISSCNTF.
               10  EISSCNTA PIC  X(0001).
           05  EISSCNTI PIC  S9(6).
      *    -------------------------------
           05  ELFISSL PIC S9(0004) COMP.
           05  ELFISSF PIC  X(0001).
           05  FILLER REDEFINES ELFISSF.
               10  ELFISSA PIC  X(0001).
           05  ELFISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  EAHISSL PIC S9(0004) COMP.
           05  EAHISSF PIC  X(0001).
           05  FILLER REDEFINES EAHISSF.
               10  EAHISSA PIC  X(0001).
           05  EAHISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  ECANCNTL PIC S9(0004) COMP.
           05  ECANCNTF PIC  X(0001).
           05  FILLER REDEFINES ECANCNTF.
               10  ECANCNTA PIC  X(0001).
           05  ECANCNTI PIC  S9(6).
      *    -------------------------------
           05  ELFCANL PIC S9(0004) COMP.
           05  ELFCANF PIC  X(0001).
           05  FILLER REDEFINES ELFCANF.
               10  ELFCANA PIC  X(0001).
           05  ELFCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  EAHCANL PIC S9(0004) COMP.
           05  EAHCANF PIC  X(0001).
           05  FILLER REDEFINES EAHCANF.
               10  EAHCANA PIC  X(0001).
           05  EAHCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  AISSCNTL PIC S9(0004) COMP.
           05  AISSCNTF PIC  X(0001).
           05  FILLER REDEFINES AISSCNTF.
               10  AISSCNTA PIC  X(0001).
           05  AISSCNTI PIC  S9(6).
      *    -------------------------------
           05  ALFISSL PIC S9(0004) COMP.
           05  ALFISSF PIC  X(0001).
           05  FILLER REDEFINES ALFISSF.
               10  ALFISSA PIC  X(0001).
           05  ALFISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  AAHISSL PIC S9(0004) COMP.
           05  AAHISSF PIC  X(0001).
           05  FILLER REDEFINES AAHISSF.
               10  AAHISSA PIC  X(0001).
           05  AAHISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  ACANCNTL PIC S9(0004) COMP.
           05  ACANCNTF PIC  X(0001).
           05  FILLER REDEFINES ACANCNTF.
               10  ACANCNTA PIC  X(0001).
           05  ACANCNTI PIC  S9(6).
      *    -------------------------------
           05  ALFCANL PIC S9(0004) COMP.
           05  ALFCANF PIC  X(0001).
           05  FILLER REDEFINES ALFCANF.
               10  ALFCANA PIC  X(0001).
           05  ALFCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  AAHCANL PIC S9(0004) COMP.
           05  AAHCANF PIC  X(0001).
           05  FILLER REDEFINES AAHCANF.
               10  AAHCANA PIC  X(0001).
           05  AAHCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  OISSCNTL PIC S9(0004) COMP.
           05  OISSCNTF PIC  X(0001).
           05  FILLER REDEFINES OISSCNTF.
               10  OISSCNTA PIC  X(0001).
           05  OISSCNTI PIC  S9(6).
      *    -------------------------------
           05  OLFISSL PIC S9(0004) COMP.
           05  OLFISSF PIC  X(0001).
           05  FILLER REDEFINES OLFISSF.
               10  OLFISSA PIC  X(0001).
           05  OLFISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  OAHISSL PIC S9(0004) COMP.
           05  OAHISSF PIC  X(0001).
           05  FILLER REDEFINES OAHISSF.
               10  OAHISSA PIC  X(0001).
           05  OAHISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  OCANCNTL PIC S9(0004) COMP.
           05  OCANCNTF PIC  X(0001).
           05  FILLER REDEFINES OCANCNTF.
               10  OCANCNTA PIC  X(0001).
           05  OCANCNTI PIC  S9(6).
      *    -------------------------------
           05  OLFCANL PIC S9(0004) COMP.
           05  OLFCANF PIC  X(0001).
           05  FILLER REDEFINES OLFCANF.
               10  OLFCANA PIC  X(0001).
           05  OLFCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  OAHCANL PIC S9(0004) COMP.
           05  OAHCANF PIC  X(0001).
           05  FILLER REDEFINES OAHCANF.
               10  OAHCANA PIC  X(0001).
           05  OAHCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  RECEVDTL PIC S9(0004) COMP.
           05  RECEVDTF PIC  X(0001).
           05  FILLER REDEFINES RECEVDTF.
               10  RECEVDTA PIC  X(0001).
           05  RECEVDTI PIC  X(0008).
      *    -------------------------------
           05  MNTHNDTL PIC S9(0004) COMP.
           05  MNTHNDTF PIC  X(0001).
           05  FILLER REDEFINES MNTHNDTF.
               10  MNTHNDTA PIC  X(0001).
           05  MNTHNDTI PIC  X(0008).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0076).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0076).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
      *    -------------------------------
           05  PF5L PIC S9(0004) COMP.
           05  PF5F PIC  X(0001).
           05  FILLER REDEFINES PF5F.
               10  PF5A PIC  X(0001).
           05  PF5I PIC  X(0015).
       01  EL630AO REDEFINES EL630AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFHDGO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRHDGO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPHDGO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BATCHO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFO PIC  X(0012).
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
           05  ACCOUNTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPHDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPHDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFRHDGO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRHDGO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EISSCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ELFISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EAHISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECANCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ELFCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EAHCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AISSCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACANCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OISSCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OLFISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OAHISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OCANCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OLFCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OAHCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECEVDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTHNDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF5O PIC  X(0015).
      *    -------------------------------
00264
00265      EJECT
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
00267  01  DFHCOMMAREA             PIC X(1900).
00268
00269      EJECT
00270
00271 *    COPY ELCCNTL.
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
00272      EJECT
00273 *    COPY ERCPNDB.
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
00274      EJECT
00275 *    COPY ERCACCT.
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
00276      EJECT
00277 *    COPY ELCCERT.
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
00278      EJECT
00279 *    COPY ERCNOTE.
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
00280      EJECT
00281 *    COPY ERCMAIL.
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
00282      EJECT
00283 *    COPY ERCPNDM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING MAILING DATA                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERPNDM                 RKP=2,LEN=11      *
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
071108* 071108  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
100217* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
00017 ******************************************************************
00018
00019  01  PENDING-MAILING-DATA.
00020      12  PM-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'PM'.
00022
00023      12  PM-CONTROL-PRIMARY.
00024          16  PM-COMPANY-CD                 PIC X.
00025          16  PM-ENTRY-BATCH                PIC X(6).
00026          16  PM-BATCH-SEQ-NO               PIC S9(4)     COMP.
00027          16  PM-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.
00028
00029      12  FILLER                            PIC X(14).
00030
00031      12  PM-ACCESS-CONTROL.
00032          16  PM-SOURCE-SYSTEM              PIC XX.
00033              88  PM-FROM-CREDIT                VALUE 'CR'.
00034              88  PM-FROM-VSI                   VALUE 'VS'.
00035              88  PM-FROM-WARRANTY              VALUE 'WA'.
00036              88  PM-FROM-OTHER                 VALUE 'OT'.
00037          16  PM-RECORD-ADD-DT              PIC XX.
00038          16  PM-RECORD-ADDED-BY            PIC XXXX.
00039          16  PM-LAST-MAINT-DT              PIC XX.
00040          16  PM-LAST-MAINT-BY              PIC XXXX.
00041          16  PM-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00042
00043      12  PM-PROFILE-INFO.
00044          16  PM-QUALIFY-CODE-1             PIC XX.
00045          16  PM-QUALIFY-CODE-2             PIC XX.
00046          16  PM-QUALIFY-CODE-3             PIC XX.
00047          16  PM-QUALIFY-CODE-4             PIC XX.
00048          16  PM-QUALIFY-CODE-5             PIC XX.
00049
00050          16  PM-INSURED-LAST-NAME          PIC X(15).
00051          16  PM-INSURED-FIRST-NAME         PIC X(10).
00052          16  PM-INSURED-MIDDLE-INIT        PIC X.
00053          16  PM-INSURED-ISSUE-AGE          PIC 99.
00054          16  PM-INSURED-BIRTH-DT           PIC XX.
00055          16  PM-INSURED-SEX                PIC X.
00056              88  PM-SEX-MALE                   VALUE 'M'.
00057              88  PM-SEX-FEMALE                 VALUE 'F'.
00058          16  PM-INSURED-SOC-SEC-NO         PIC X(11).
00059
080406         16  PM-ADDRESS-CORRECTED          PIC X.
081108         16  PM-JOINT-BIRTH-DT             PIC XX.
00060 *        16  FILLER                        PIC X(12).
00061
00062          16  PM-ADDRESS-LINE-1             PIC X(30).
00063          16  PM-ADDRESS-LINE-2             PIC X(30).
00064          16  PM-CITY-STATE.
                   20  PM-CITY                   PIC X(28).
                   20  PM-STATE                  PIC XX.
00065          16  PM-ZIP.
00066              20  PM-ZIP-CODE.
00067                  24  PM-ZIP-1              PIC X.
00068                      88  PM-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00069                  24  FILLER                PIC X(4).
00070              20  PM-ZIP-PLUS4              PIC X(4).
00071          16  PM-CANADIAN-ZIP  REDEFINES  PM-ZIP.
00072              20  PM-CAN-POST1              PIC XXX.
00073              20  PM-CAN-POST2              PIC XXX.
00074              20  FILLER                    PIC XXX.
00075
00076          16  PM-PHONE-NO                   PIC 9(11)       COMP-3.
100217         16  pm-city-st-zip-verified       pic x.
100217         16  FILLER                        PIC XX.
00079
           12  PM-CRED-BENE-INFO.
CIDMOD         16  PM-CRED-BENE-NAME             PIC X(25).
CIDMOD         16  PM-CRED-BENE-ADDR             PIC X(30).
071108         16  PM-CRED-BENE-ADDR2            PIC X(30).
CIDMOD         16  PM-CRED-BENE-CTYST.
                   20  PM-CRED-BENE-CITY         PIC X(28).
                   20  PM-CRED-BENE-STATE        PIC XX.
CIDMOD         16  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-ZIP-CODE.
CIDMOD                 24  PM-CB-ZIP-1           PIC X.
CIDMOD                     88  PM-CB-CANADIAN-POST-CODE
                                        VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                PIC X(4).
CIDMOD             20  PM-CB-ZIP-PLUS4           PIC X(4).
CIDMOD         16  PM-CB-CANADIAN-ZIP  REDEFINES  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-CAN-POST1           PIC XXX.
CIDMOD             20  PM-CB-CAN-POST2           PIC XXX.
CIDMOD             20  FILLER                    PIC XXX.
080406     12  PM-POST-CARD-MAIL-DATA.
080406         16  PM-MAIL-DATA OCCURS 7.
080406             20  PM-MAIL-TYPE              PIC X.
080406                 88  PM-12MO-MAILING           VALUE '1'.
080406                 88  PM-EXP-MAILING            VALUE '2'.
080406             20  PM-MAIL-STATUS            PIC X.
080406                 88  PM-MAIL-ST-MAILED         VALUE '1'.
080406                 88  PM-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  PM-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  PM-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC X(12).
080406*    12  FILLER                            PIC X(30).
00075
00081 ******************************************************************
00284      EJECT
00285 *    COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
071712* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
071712     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
071712         16  CO-OV120                      PIC S9(7)V99   COMP-3.
071712         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
00286      EJECT
00287 *    COPY ERCRQST.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCRQST.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACCOUNTS RECEIVABLE REQUEST RECORD        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERRQST                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH1 = ERRQST2  (BY CO, CAR, GROUP, ST,       *
00014 *                                   ACCOUNT, REF, BATCH)         *
00015 *                                                RKP=9, LEN=38   *
00016 *       ALTERNATE PATH2 = ERRQST3  (BY CO, CAR, GROUP, FIN  RESP *
00017 *                                   ACCOUNT, REF, BATCH)         *
00018 *                                                RKP=47, LEN=46  *
00019 *       ALTERNATE PATH3 = ERRQST4  (BY CO, CAR, GROUP, AGENT,    *
00020 *                                   BATCH)                       *
00021 *                                                RKP=93, LEN=24  *
00022 *       ALTERNATE PATH4 = ERRQST5  (BY CO, SUMMARY CODE, ACCT,   *
00023 *                                   REF, BATCH)                  *
00024 *                                                RKP=117, LEN=35 *
00025 *   LOG = NO                                                     *
00026 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00027 ******************************************************************
00028
00029  01  AR-REQUEST-RECORD.
00030      12  RQ-RECORD-ID                     PIC XX.
00031          88  VALID-RQ-ID                        VALUE 'RQ'.
00032
00033      12  RQ-CONTROL-PRIMARY.
00034          16  RQ-COMPANY-CD                PIC X.
00035          16  RQ-ENTRY-BATCH               PIC X(6).
00036
00037      12  RQ-CONTROL-BY-ACCT-REF.
00038          16  RQ-COMPANY-CD-A1             PIC X.
00039          16  RQ-CARRIER-A1                PIC X.
00040          16  RQ-GROUPING-A1               PIC X(6).
00041          16  RQ-STATE-A1                  PIC XX.
00042          16  RQ-ACCOUNT-A1                PIC X(10).
00043          16  RQ-REFERENCE-A1              PIC X(12).
00044          16  RQ-BATCH-A1                  PIC X(6).
00045
00046      12  RQ-CONTROL-BY-FIN-RESP.
00047          16  RQ-COMPANY-CD-A2             PIC X.
00048          16  RQ-CARRIER-A2                PIC X.
00049          16  RQ-GROUPING-A2               PIC X(6).
00050          16  RQ-FIN-RESP-A2               PIC X(10).
00051          16  RQ-ACCT-AGENT-A2             PIC X(10).
00052          16  RQ-REFERENCE-A2              PIC X(12).
00053          16  RQ-BATCH-A2                  PIC X(6).
00054
00055      12  RQ-CONTROL-BY-ACCT-AGENT.
00056          16  RQ-COMPANY-CD-A3             PIC X.
00057          16  RQ-CARRIER-A3                PIC X.
00058          16  RQ-GROUPING-A3               PIC X(6).
00059          16  RQ-ACCT-AGENT-A3             PIC X(10).
00060          16  RQ-BATCH-A3                  PIC X(6).
00061
00062      12  RQ-CONTROL-BY-SUMMARY.
00063          16  RQ-COMPANY-CD-A4             PIC X.
00064          16  RQ-SUMMARY-CODE              PIC X(6).
00065          16  RQ-ACCOUNT-A4                PIC X(10).
00066          16  RQ-REFERENCE-A4              PIC X(12).
00067          16  RQ-BATCH-A4                  PIC X(6).
00068
00069      12  RQ-REQUEST-METHOD                PIC X.
00070          88 RQ-FIN-RESP-REQUEST               VALUE 'F'.
00071          88 RQ-ACCT-AGENT-REQUEST             VALUE 'A'.
00072          88 RQ-SUMMARY-REQUEST                VALUE 'S'.
00073          88 RQ-BATCH-REQUEST                  VALUE 'B'.
00074      12  FILLER                           PIC X.
00075      12  RQ-STATUS                        PIC X.
00076          88  RQ-REQUEST-ERROR                 VALUE 'E'.
00077          88  RQ-RESUBMIT                      VALUE 'R'.
00078      12  RQ-PROCESSOR-ID                  PIC X(4).
00079      12  RQ-ENTRY-DT                      PIC XX.
00080      12  RQ-MO-END-DT                     PIC XX.
00081      12  RQ-REQUEST-DT                    PIC XX.
00082      12  RQ-STMT-DT                       PIC XX.
00083      12  RQ-REVERSAL-DT                   PIC XX.
00084      12  RQ-CREDIT-SELECT-DT              PIC XX.
00085      12  RQ-CREDIT-ACCEPT-DT              PIC XX.
00086
00087      12  FILLER                           PIC X(27).
00088
00089 ******************************************************************
00090
00288      EJECT
111109*    COPY ERCCNOT.
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
00289
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                PENDING-BUSINESS ACCOUNT-MASTER
                                CERTIFICATE-MASTER CERTIFICATE-NOTE
                                MAILING-DATA PENDING-MAILING-DATA
                                COMPENSATION-MASTER AR-REQUEST-RECORD
                                CERT-NOTE-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL630' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00291
00292      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00293      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00294      IF EIBCALEN = 0
00295          GO TO 8800-UNAUTHORIZED-ACCESS.
00296
00297      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00298      MOVE '5'                    TO DC-OPTION-CODE.
00299      PERFORM 9700-DATE-LINK.
00300
00301      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
00302      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
00303      MOVE DC-GREG-DATE-1-YMD     TO WS-CURRENT-YMD.
00304
00305      MOVE PI-CALLING-PROGRAM TO PI-SAVE-CALLING-PGM.
00306      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00307          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00308              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00309              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00310              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00311              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00312              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00313              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00314              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00315              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00316              MOVE SPACES               TO PI-PROGRAM-WORK-AREA
00317                                           PI-MISC
00318          ELSE
00319              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00320              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00321              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00322              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00323              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00324              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00325              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00326              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00327
00328      MOVE LOW-VALUES             TO EL630AI.
00329
00330      IF PI-SAVE-CALLING-PGM = XCTL-6302
00331          GO TO 4000-SHOW-TOTALS.
00332
020816     IF PI-SAVE-CALLING-PGM = XCTL-6301 or 'VP6301'
00334          MOVE 'N'                TO PI-EL630-FIRST-TIME-SW
00335          IF (PI-CAN-CNT-REMITTED GREATER ZERO OR
00336              PI-ISS-CNT-REMITTED GREATER ZERO)
00337                  GO TO 4000-SHOW-TOTALS
00338          ELSE
00339          IF PI-ISS-CNT-ENTERED = ZEROS  AND
00340             PI-CAN-CNT-ENTERED = ZEROS
00341              MOVE 'Y'            TO PI-VERIFY-DELETE-SW
00342              GO TO 3000-DELETE-ENTERED-BATCH
00343          ELSE
00344              GO TO 4000-SHOW-TOTALS.
00345
00346      IF EIBTRNID NOT = TRANS-ID
00347          MOVE SPACE              TO PI-BROWSE-SW
00348          MOVE ZEROS              TO PI-LAST-SEQ-NO-ADDED
00349          MOVE SPACES             TO PI-CR-CONTROL-IN-PROGRESS
00350          MOVE QUESTION-MARKS     TO BATCHI
00351          MOVE AL-UANON           TO BATCHA
00352          GO TO 8100-SEND-INITIAL-MAP.
00353
00354      
      * EXEC CICS HANDLE CONDITION
00355 *        PGMIDERR  (9600-PGMID-ERROR)
00356 *        ERROR     (9990-ABEND)
00357 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005572' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035353732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00358
00359      IF EIBAID = DFHCLEAR
00360         MOVE SPACES              TO PI-ACCT-AGENT-PROCESSED-SW
00361         GO TO 9400-CLEAR.
00362
00363      IF PI-PROCESSOR-ID = 'LGXX'
00364          GO TO 0200-RECEIVE.
00365
00366      
      * EXEC CICS READQ TS
00367 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00368 *        INTO   (SECURITY-CONTROL)
00369 *        LENGTH (SC-COMM-LENGTH)
00370 *        ITEM   (SC-ITEM)
00371 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00005584' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00372
00373      MOVE SC-CREDIT-DISPLAY (11)  TO PI-DISPLAY-CAP.
00374      MOVE SC-CREDIT-UPDATE  (11)  TO PI-MODIFY-CAP.
00375
00376      IF NOT DISPLAY-CAP
00377          MOVE 'READ'          TO SM-READ
00378          PERFORM 9995-SECURITY-VIOLATION
00379                               THRU 9995-EXIT
00380          MOVE ER-0070         TO  EMI-ERROR
00381          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00382          GO TO 8100-SEND-INITIAL-MAP.
00383
00384      EJECT
00385  0200-RECEIVE.
00386      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00387          MOVE ER-0008            TO EMI-ERROR
00388          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00389          MOVE -1                 TO MAINTL
00390          GO TO 8200-SEND-DATAONLY.
00391
00392      
      * EXEC CICS RECEIVE
00393 *        MAP      (MAP-NAME)
00394 *        MAPSET   (MAPSET-NAME)
00395 *        INTO     (EL630AI)
00396 *    END-EXEC.
           MOVE LENGTH OF
            EL630AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005610' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL630AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00397
00398      IF PFENTERL = 0
00399          GO TO 0300-CHECK-PFKEYS.
00400
00401      IF PFENTERI NUMERIC
00402         IF PFENTERI GREATER 0 AND LESS 25
00403            MOVE PF-VALUES (PFENTERI) TO EIBAID
00404         ELSE
00405            MOVE ER-0029            TO EMI-ERROR
00406            GO TO 0320-INPUT-ERROR.
00407
00408  0300-CHECK-PFKEYS.
00409      IF EIBAID = DFHENTER
00410         IF PI-CLEAR-ERROR
00411            MOVE -1                 TO PFENTERL
00412            MOVE ER-2213            TO EMI-ERROR
00413            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00414            GO TO 4000-SHOW-TOTALS.
00415
00416      IF EIBAID = DFHPF23
00417          GO TO 8810-PF23.
00418      IF EIBAID = DFHPF24
00419          GO TO 9200-RETURN-MAIN-MENU.
00420      IF EIBAID = DFHPF12
00421          GO TO 9500-PF12.
00422
00423      IF EIBAID = DFHPF1
00424         IF PI-DATA-UPDATED
00425            MOVE 'Y'            TO WS-PF1-SW
00426            MOVE SPACE          TO PI-NB-MONTH-END-DT
00427                                   PI-CLEAR-ERROR-SW
00428            PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT
00429            MOVE SPACES TO BATCH-TO-PROCESS
00430            MOVE PI-COMPANY-ID  TO EDIT-COMPANY-ID
00431            MOVE PI-COMPANY-CD  TO EDIT-COMPANY-CD
00432            MOVE PI-SAV-ENTRY-BATCH TO EDIT-BATCH
00433            
      * EXEC CICS START
00434 *               TRANSID       (EDIT-TRANS)
00435 *               FROM          (BATCH-TO-PROCESS)
00436 *               LENGTH        (PASS-AREA-LEN)
00437 *          END-EXEC
      *    MOVE '0( LF                 1   #00005651' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 EDIT-TRANS, 
                 DFHEIV99, 
                 BATCH-TO-PROCESS, 
                 PASS-AREA-LEN, 
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
00438            MOVE PI-SAV-REFERENCE TO PI-SAV-PYAJ-REFERENCE
00439            MOVE SPACES         TO PI-UPDATE-SW
00440                                   PI-KEYED-SWITCHES
00441                                   PI-AGE-KEYED-SW
00442                                   PI-BIRTHDT-KEYED-SW
00443                                   PI-ACCT-AGENT-PROCESSED-SW
00444                                   PI-SAV-REFERENCE
00445            MOVE ZEROS          TO PI-ISS-CNT-ENTERED
00446                                   PI-CAN-CNT-ENTERED
00447            MOVE LOW-VALUES     TO EL630AO
00448            MOVE SPACE          TO PI-BROWSE-SW
00449            MOVE QUESTION-MARKS TO BATCHI
00450            MOVE AL-UANON       TO BATCHA
00451            MOVE -1             TO MAINTL
00452            IF EMI-NO-ERRORS
00453                MOVE ZEROS          TO EMI-ERROR
00454                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00455                GO TO 8100-SEND-INITIAL-MAP
00456            ELSE
00457                GO TO 8100-SEND-INITIAL-MAP
00458         ELSE
00459            IF  MAINTI = 'N'
00460                MOVE -1             TO MAINTL
00461                MOVE ER-2211        TO EMI-ERROR
00462                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00463                GO TO 8200-SEND-DATAONLY.
00464
00465      IF EIBAID = DFHPF1
00466         MOVE SPACE          TO PI-NB-MONTH-END-DT
00467                                PI-CLEAR-ERROR-SW
00468         MOVE SPACES TO BATCH-TO-PROCESS
00469         MOVE PI-COMPANY-ID  TO EDIT-COMPANY-ID
00470         MOVE PI-COMPANY-CD  TO EDIT-COMPANY-CD
00471         MOVE PI-SAV-ENTRY-BATCH TO EDIT-BATCH
00472         IF EDIT-BATCH GREATER SPACES
00473              
      * EXEC CICS START
00474 *               TRANSID       (EDIT-TRANS)
00475 *               FROM          (BATCH-TO-PROCESS)
00476 *               LENGTH        (PASS-AREA-LEN)
00477 *            END-EXEC
      *    MOVE '0( LF                 1   #00005691' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 EDIT-TRANS, 
                 DFHEIV99, 
                 BATCH-TO-PROCESS, 
                 PASS-AREA-LEN, 
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
00478              MOVE SPACES         TO PI-UPDATE-SW
00479                                     PI-KEYED-SWITCHES
00480                                     PI-AGE-KEYED-SW
00481                                     PI-BIRTHDT-KEYED-SW
00482                                     PI-ACCT-AGENT-PROCESSED-SW
00483                                     PI-SAV-REFERENCE
00484              MOVE ZEROS          TO PI-ISS-CNT-ENTERED
00485                                     PI-CAN-CNT-ENTERED
00486                                     EMI-ERROR
00487              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00488              MOVE LOW-VALUES     TO EL630AO
00489              MOVE SPACE          TO PI-BROWSE-SW
00490              MOVE QUESTION-MARKS TO BATCHI
00491              MOVE AL-UANON       TO BATCHA
00492              MOVE -1             TO MAINTL
00493              GO TO 8100-SEND-INITIAL-MAP
00494         ELSE
00495              MOVE -1             TO MAINTL
00496              MOVE ER-2211        TO EMI-ERROR
00497              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00498              GO TO 8200-SEND-DATAONLY.
00499
00500      IF EIBAID = DFHPF3
00501          IF NOT PI-DATA-UPDATED
00502              MOVE SPACE          TO PI-EL630-FIRST-TIME-SW
00503              IF PI-AR-PROCESSING
00504                 MOVE XCTL-635       TO PGM-NAME
00505                 GO TO 9300-XCTL
00506              ELSE
00507                 IF PI-COMPANY-ID = 'DMD'
00508                    MOVE XCTL-633DMD TO PGM-NAME
00509                    GO TO 9300-XCTL
00510                 ELSE
00511                    MOVE XCTL-633    TO PGM-NAME
00512                    GO TO 9300-XCTL
00513          ELSE
00514              MOVE -1             TO BATCHL
00515              MOVE AL-UABON       TO BATCHA
00516              MOVE ER-2213        TO EMI-ERROR
00517              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00518              GO TO 8200-SEND-DATAONLY.
00519
00520      IF EIBAID = DFHPF4
00521          IF NOT PI-EL630-FIRST-TIME
00522              MOVE XCTL-6302      TO PGM-NAME
00523              GO TO 9300-XCTL
00524          ELSE
00525              PERFORM 0400-PRIME-BATCH-TOTALS THRU 0490-EXIT
00526              IF EMI-ERROR = ZEROS
00527                  MOVE 'N'        TO PI-EL630-FIRST-TIME-SW
00528                  MOVE XCTL-6302  TO PGM-NAME
00529                  GO TO 9300-XCTL
00530              ELSE
00531                  GO TO 8200-SEND-DATAONLY.
00532
00533      IF EIBAID = DFHPF5
00534              IF PI-EL630-FIRST-TIME
00535                  MOVE -1         TO MAINTL
00536                  GO TO 8200-SEND-DATAONLY
00537              ELSE
00538              IF PI-ISS-CNT-ENTERED = ZEROS  AND
00539                 PI-CAN-CNT-ENTERED = ZEROS
00540                  MOVE LOW-VALUES TO     EL630AO
00541                  MOVE QUESTION-MARKS TO BATCHI
00542                  MOVE AL-UANON   TO     BATCHA
00543                  MOVE -1         TO     MAINTL
00544                  GO TO 8100-SEND-INITIAL-MAP
00545              ELSE
00546                  MOVE -1         TO BATCHL
00547                  MOVE AL-UABON   TO BATCHA
00548                  MOVE ER-2213    TO EMI-ERROR
00549                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00550                  GO TO 4000-SHOW-TOTALS.
00551
00552      IF EIBAID = DFHPF2
00553         MOVE  AL-UANON           TO  BATCHA
00554         MOVE  BATCHI             TO PI-SAV-ENTRY-BATCH
00555         GO TO 3000-DELETE-ENTERED-BATCH.
00556
00557      IF  MAINTI = 'S'
00558          MOVE  MAINTI            TO  PI-MAINT-FUNC
00559          PERFORM 0400-PRIME-BATCH-TOTALS  THRU  0490-EXIT
00560              IF EMI-ERROR = ZEROS
00561                  MOVE 'N'        TO PI-EL630-FIRST-TIME-SW
00562                  GO TO 4000-SHOW-TOTALS
00563              ELSE
00564                  GO TO 8200-SEND-DATAONLY.
00565
00566      IF EIBAID = DFHENTER
00567          GO TO 0330-EDIT-DATA.
00568
00569  0320-INPUT-ERROR.
00570      MOVE ER-0029                TO EMI-ERROR.
00571      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00572      MOVE AL-UNBON               TO PFENTERA.
00573      IF PFENTERL = 0
00574          MOVE -1                 TO MAINTL
00575      ELSE
00576          MOVE -1                 TO PFENTERL.
00577
00578      GO TO 8200-SEND-DATAONLY.
00579
00580      EJECT
00581  0330-EDIT-DATA.
00582      IF MODIFY-CAP
00583          NEXT SENTENCE
00584        ELSE
00585          IF MAINTI NOT = 'S'
00586            MOVE 'UPDATE'       TO SM-READ
00587            PERFORM 9995-SECURITY-VIOLATION
00588                               THRU 9995-EXIT
00589            MOVE ER-0070        TO EMI-ERROR
00590            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00591            GO TO 8100-SEND-INITIAL-MAP.
00592
00593      MOVE SPACE                  TO  PI-NB-MONTH-END-DT.
00594
00595      PERFORM 1250-READ-COMPANY-REC THRU 1250-EXIT.
00596
00597      IF MAINTI = 'N' OR 'C' OR 'B' OR 'K'
00598          NEXT SENTENCE
00599      ELSE
00600          MOVE ER-0023            TO EMI-ERROR
00601          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00602          MOVE -1                 TO MAINTL
00603          MOVE AL-UABON           TO MAINTA
00604          GO TO 8200-SEND-DATAONLY.
00605
00606      IF PI-CARRIER-SECURITY GREATER SPACES
00607          IF CARRIERL GREATER ZEROS
00608            IF PI-CARRIER-SECURITY = CARRIERI
00609               MOVE AL-UANON       TO CARRIERA
00610             ELSE
00611               MOVE -1             TO CARRIERL
00612               MOVE AL-UABON       TO CARRIERA
00613               MOVE ER-2370       TO EMI-ERROR
00614               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00615
00616      IF PI-ACCOUNT-SECURITY GREATER SPACES
00617          IF ACCOUNTL GREATER ZEROS
00618              IF PI-ACCOUNT-SECURITY = ACCOUNTI
00619                  MOVE AL-UANON   TO ACCOUNTA
00620                ELSE
00621                  MOVE -1         TO ACCOUNTL
00622                  MOVE AL-UABON   TO ACCOUNTA
00623                  MOVE ER-2371   TO EMI-ERROR
00624                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00625
00626      IF  NOT EMI-NO-ERRORS
00627          GO TO 8200-SEND-DATAONLY.
00628
00629      MOVE MAINTI                 TO PI-MAINT-FUNC.
00630      MOVE AL-UANON               TO MAINTA.
00631
00632      MOVE PI-COMPANY-CD          TO PI-SAV-COMP-CD.
00633      MOVE ZEROS                  TO PI-SAV-BATCH-SEQ
00634                                     PI-SAV-BATCH-CHG-SEQ.
00635      MOVE SPACES                 TO PI-SAV-CERT-EFF-DT
00636                                     PI-SAV-CERT-NO.
00637
00638      IF BATCHL = ZEROS
00639          MOVE -1                 TO BATCHL
00640          MOVE AL-UNBON           TO BATCHA
00641          MOVE ER-2201            TO EMI-ERROR
00642          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00643      ELSE
00644          IF BATCHI = QUESTION-MARKS OR SPACES
00645              IF MAINTI = 'N'
00646                  PERFORM 1300-GET-BATCH-NO THRU 1300-EXIT
00647              ELSE
00648                  MOVE -1         TO BATCHL
00649                  MOVE AL-UNBON   TO BATCHA
00650                  MOVE ER-2201    TO EMI-ERROR
00651                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00652          ELSE
103001             MOVE SPACE TO WS-SWITCH
103001             PERFORM 0335-FAT-FINGER-EDIT VARYING PIC6-SUB
103001                 FROM +1 BY +1
103001                 UNTIL PIC6-SUB > +6 OR QUESTION-MARK-FOUND.
103001
103001     IF QUESTION-MARK-FOUND
103001         IF MAINTI = 'N'
103001             PERFORM 1300-GET-BATCH-NO THRU 1300-EXIT
103001         ELSE
103001             MOVE -1         TO BATCHL
103001             MOVE AL-UNBON   TO BATCHA
103001             MOVE ER-2201    TO EMI-ERROR
103001             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
103001     ELSE
00653          IF BATCHI = PI-SAV-ENTRY-BATCH
00654              MOVE AL-UANON   TO BATCHA
00655          ELSE
00656              MOVE BATCHI TO PI-SAV-ENTRY-BATCH
00657              IF MAINTI = 'C' OR 'B'
00658                  MOVE AL-UANON TO BATCHA
00659                  MOVE ZEROS      TO ELFISSL  ELFCANL
00660                                     EAHISSL  EAHCANL
00661                                     EISSCNTL ECANCNTL
00662                                     PI-LF-ISS-REMITTED
00663                                     PI-LF-CAN-REMITTED
00664                                     PI-AH-ISS-REMITTED
00665                                     PI-AH-CAN-REMITTED
00666                                     PI-ISS-CNT-REMITTED
00667                                     PI-CAN-CNT-REMITTED.
00668
00669      IF REFL GREATER ZEROS
00670         MOVE REFI                TO PI-SAV-REFERENCE.
00671
00672      IF MAINTI = 'C' OR 'B'
00673          GO TO 0340-CHECK-TOTALS.
00674
00675      IF CARRIERL GREATER ZEROS
00676          MOVE AL-UANON           TO CARRIERA
00677          PERFORM 1000-VERIFY-CARRIER-ID THRU 1000-EXIT
00678      ELSE
00679          IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL
00680              MOVE -1             TO CARRIERL
00681              MOVE AL-UABON       TO CARRIERA
00682              MOVE ER-0194        TO EMI-ERROR
00683              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00684
00685      IF GROUPL GREATER ZEROS
00686          MOVE AL-UANON           TO GROUPA
00687      ELSE
00688          IF CARR-GROUP-ST-ACCNT-CNTL
00689              MOVE -1 TO          GROUPL
00690              MOVE AL-UABON       TO GROUPA
00691              MOVE ER-0195        TO EMI-ERROR
00692              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00693
00694      IF STATEL GREATER ZEROS
00695          MOVE AL-UANON           TO STATEA
00696          PERFORM 1100-VERIFY-STATE-ID THRU 1100-EXIT
00697      ELSE
00698          IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL
00699              MOVE -1             TO STATEL
00700              MOVE AL-UABON       TO STATEA
00701              MOVE ER-0196        TO EMI-ERROR
00702              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00703
00704      IF ACCOUNTL GREATER ZEROS
00705          MOVE AL-UANON           TO ACCOUNTA
00706          PERFORM 1200-VERIFY-ACCOUNT THRU 1200-EXIT
00707      ELSE
00708          MOVE -1 TO ACCOUNTL
00709          MOVE AL-UABON           TO ACCOUNTA
00710          MOVE ER-0197            TO EMI-ERROR
00711          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
103001
103001 0335-FAT-FINGER-EDIT.
103001
103001     IF BATCHI (PIC6-SUB:1) = QUESTION-MARK
103001         SET QUESTION-MARK-FOUND TO TRUE
103001     END-IF.
00712
00713  0340-CHECK-TOTALS.
00714 ******************************************************************
00715 *        IF MAINTENANCE FUNCTION = 'K' (COPY BATCH)              *
00716 *           GO TO CHECK ERRORS.                                  *
00717 ******************************************************************
00718
00719      IF  MAINTI = 'K'
00720          GO TO 0340-ERROR-CHECK.
00721
00722 *    ********************************************************
00723 *    *         THE FOLLOWING FIELDS ARE NOT REQUIRED        *
00724 *    *         IF ENTERED THE ONLY REQUIREMENT IS NUMERIC   *
00725 *    ********************************************************
00726
00727      IF ELFISSL GREATER ZEROS
00728          MOVE AL-UNNON           TO ELFISSA
00729          
      * EXEC CICS BIF DEEDIT
00730 *            FIELD   (ELFISSI)
00731 *            LENGTH  (12)
00732 *        END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005967' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELFISSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00733          MOVE ELFISSI            TO PI-LF-ISS-REMITTED
00734      ELSE
00735          MOVE ZEROS              TO PI-LF-ISS-REMITTED.
00736
00737      IF ELFCANL GREATER ZEROS
00738          MOVE AL-UNNON           TO ELFCANA
00739          
      * EXEC CICS BIF DEEDIT
00740 *            FIELD   (ELFCANI)
00741 *            LENGTH  (10)
00742 *        END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005977' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELFCANI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00743          MOVE ELFCANI            TO PI-LF-CAN-REMITTED
00744      ELSE
00745          MOVE ZEROS              TO PI-LF-CAN-REMITTED.
00746
00747      IF EAHISSL GREATER ZEROS
00748          MOVE AL-UNNON           TO EAHISSA
00749          
      * EXEC CICS BIF DEEDIT
00750 *            FIELD   (EAHISSI)
00751 *            LENGTH  (12)
00752 *        END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005987' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EAHISSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00753          MOVE EAHISSI            TO PI-AH-ISS-REMITTED
00754      ELSE
00755          MOVE ZEROS              TO PI-AH-ISS-REMITTED.
00756
00757      IF EAHCANL GREATER ZEROS
00758          MOVE AL-UNNON           TO EAHCANA
00759          
      * EXEC CICS BIF DEEDIT
00760 *            FIELD   (EAHCANI)
00761 *            LENGTH  (10)
00762 *        END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005997' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EAHCANI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00763          MOVE EAHCANI            TO PI-AH-CAN-REMITTED
00764      ELSE
00765          MOVE ZEROS              TO PI-AH-CAN-REMITTED.
00766
00767      IF EISSCNTL GREATER ZEROS
00768          MOVE AL-UNNON           TO EISSCNTA
00769          
      * EXEC CICS BIF DEEDIT
00770 *            FIELD (EISSCNTI)
00771 *            LENGTH(6)
00772 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006007' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EISSCNTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00773          MOVE EISSCNTI           TO PI-ISS-CNT-REMITTED
00774      ELSE
00775          MOVE ZEROS              TO PI-ISS-CNT-REMITTED.
00776
00777      IF ECANCNTL GREATER ZEROS
00778          MOVE AL-UNNON           TO ECANCNTA
00779          
      * EXEC CICS BIF DEEDIT
00780 *            FIELD  (ECANCNTI)
00781 *            LENGTH (6)
00782 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006017' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ECANCNTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00783          MOVE ECANCNTI           TO PI-CAN-CNT-REMITTED
00784      ELSE
00785          MOVE ZEROS              TO PI-CAN-CNT-REMITTED.
00786
00787      IF MNTHNDTL GREATER ZEROS
00788         NEXT SENTENCE
00789        ELSE
00790         GO TO 0340-RECEIVED-DATE.
00791
00792      MOVE MNTHNDTI               TO WS-DT-DEEDIT-FIELD.
00793      PERFORM 8600-DEEDIT THRU 8600-EXIT.
00794
00795      MOVE WS-DEEDIT-FIELD-DATE-OUT
00796                                  TO DC-GREG-DATE-1-MDY.
00797      MOVE '4'          TO DC-OPTION-CODE.
00798      PERFORM 9700-DATE-LINK.
00799
00800      IF DATE-CONVERSION-ERROR
00801         GO TO 0340-MNTH-DAY-ERROR.
00802
00803      MOVE DC-GREG-DATE-1-EDIT    TO MNTHNDTO.
00804      MOVE DC-BIN-DATE-1          TO PI-NB-MONTH-END-DT.
00805      MOVE DC-GREG-DATE-1-MDY     TO DATE-TEST-AREA.
00806
00807      IF DATE-TEST-DD = DC-DAYS-IN-MONTH OR 01
00808         CONTINUE
00809      ELSE
00810         GO TO 0340-MNTH-DAY-ERROR.
00811
00812  0340-RECEIVED-DATE.
00813
00814      IF RECEVDTL GREATER ZEROS
00815         NEXT SENTENCE
00816        ELSE
00817         MOVE WS-CURRENT-DT       TO RECEVDTI.
00818
00819      MOVE RECEVDTI               TO WS-DT-DEEDIT-FIELD.
00820      PERFORM 8600-DEEDIT THRU 8600-EXIT.
00821
00822      MOVE WS-DEEDIT-FIELD-DATE-OUT
00823                                  TO DC-GREG-DATE-1-MDY.
00824      MOVE '4'          TO DC-OPTION-CODE.
00825      PERFORM 9700-DATE-LINK.
00826
00827      IF DATE-CONVERSION-ERROR
00828         GO TO 0340-RECEV-DAY-ERROR.
00829
00830      MOVE DC-GREG-DATE-1-EDIT    TO RECEVDTO.
00831      MOVE DC-BIN-DATE-1          TO PI-RECEIVED-DT.
00832
00833      GO TO 0340-ERROR-CHECK.
00834
00835  0340-MNTH-DAY-ERROR.
00836
00837      MOVE -1       TO MNTHNDTL.
00838      MOVE AL-UABON TO MNTHNDTA.
00839      MOVE ER-0587  TO EMI-ERROR.
00840      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00841      GO TO 0340-ERROR-CHECK.
00842
00843  0340-RECEV-DAY-ERROR.
00844
00845      MOVE -1       TO RECEVDTL.
00846      MOVE AL-UABON TO RECEVDTA.
00847      MOVE ER-0905  TO EMI-ERROR.
00848      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00849
00850  0340-ERROR-CHECK.
00851
00852      IF NOT EMI-NO-ERRORS
00853          GO TO 8200-SEND-DATAONLY.
00854
00855      IF PI-NB-MONTH-END-DT EQUAL LOW-VALUES OR SPACES
00856         GO TO 0345-END-DATE-CHECK.
00857
00858      IF PI-NB-MONTH-END-DT LESS THAN PI-CR-MONTH-END-DT
00859         MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1
00860         MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-2
00861      ELSE
00862      IF PI-CR-MONTH-END-DT LESS THAN PI-NB-MONTH-END-DT
00863         MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-2
00864         MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1
00865      ELSE
00866         GO TO 0345-END-DATE-CHECK.
00867
00868      MOVE  '1'                   TO  DC-OPTION-CODE.
00869      PERFORM  9700-DATE-LINK.
00870
00871      IF (DATE-CONVERSION-ERROR)
00872                    OR
00873         ((DC-ELAPSED-MONTHS GREATER THAN +1) AND
00874          (PI-COMPANY-ID NOT EQUAL 'DMD'))
00875         MOVE -1       TO MNTHNDTL
00876         MOVE AL-UABON TO MNTHNDTA
00877         MOVE ER-0587  TO EMI-ERROR
00878         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00879
00880  0345-END-DATE-CHECK.
00881
00882      IF NOT EMI-NO-ERRORS
00883          GO TO 8200-SEND-DATAONLY.
00884
00885      IF PI-DELETE-IS-OK
00886          MOVE SPACE              TO PI-VERIFY-DELETE-SW.
00887
00888      IF MAINTI = 'N'
00889        AND PI-DATA-UPDATED
00890          MOVE 'C'                TO PI-MAINT-FUNC
00891          GO TO 0360-XCTL-EL6301.
00892
00893      
      * EXEC CICS HANDLE CONDITION
00894 *        NOTFND  (0350-NO-RECORDS)
00895 *        ENDFILE (0350-NO-RECORDS)
00896 *    END-EXEC.
      *    MOVE '"$I''                  ! # #00006131' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036313331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00897
00898      
      * EXEC CICS READ
00899 *        SET     (ADDRESS OF PENDING-BUSINESS)
00900 *        DATASET (ERPNDB-FILE-ID)
00901 *        RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)
00902 *        GTEQ
00903 *    END-EXEC.
      *    MOVE '&"S        G          (   #00006136' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00904
00905      IF PI-SAV-COMP-CD     = PB-COMPANY-CD  AND
00906         PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH
00907          IF MAINTI = 'N'
00908              MOVE ER-2229        TO EMI-ERROR
00909              MOVE -1             TO MAINTL
00910              MOVE AL-UABON       TO MAINTA
00911              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00912              GO TO 8200-SEND-DATAONLY
00913          ELSE
00914              NEXT SENTENCE
00915      ELSE
00916          GO TO 0350-NO-RECORDS.
00917
00918      IF PI-CARRIER-SECURITY GREATER SPACES
00919         IF PB-CARRIER = PI-CARRIER-SECURITY
00920            NEXT SENTENCE
00921          ELSE
00922            GO TO 0350-NO-RECORDS.
00923
00924      IF PI-ACCOUNT-SECURITY GREATER SPACES
00925         IF PB-ACCOUNT = PI-ACCOUNT-SECURITY
00926            NEXT SENTENCE
00927          ELSE
00928            GO TO 0350-NO-RECORDS.
00929
00930      IF PB-BILLED-DT NOT = LOW-VALUES
00931          MOVE ER-2402            TO EMI-ERROR
00932          MOVE -1                 TO BATCHL
00933          MOVE AL-UABON           TO BATCHA
00934          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00935          GO TO 8200-SEND-DATAONLY.
00936
00937 ******************************************************************
00938 *        IF MAINTENANCE FUNCTION = 'K'                           *
00939 *           REWRITE ENTIRE BATCH WITH NEW ACCOUNT INFORMATION.   *
00940 ******************************************************************
00941
00942      IF  PI-SAV-COMP-CD = PB-COMPANY-CD
00943          IF  PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH
00944              IF  MAINTI = 'K'
00945                  GO TO 6000-REWRITE-ENTIRE-BATCH.
00946
00947      IF PI-SAV-COMP-CD     = PB-COMPANY-CD   AND
00948         PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH  AND
00949         MAINTI NOT = 'N'
00950          MOVE PB-CARRIER         TO PI-SAV-CARRIER
00951                                     ACCT-CARRIER
00952          MOVE PB-GROUPING        TO PI-SAV-GROUPING
00953                                     ACCT-GROUPING
00954          MOVE PB-STATE           TO PI-SAV-STATE
00955                                     ACCT-STATE
00956          MOVE PB-ACCOUNT         TO PI-SAV-ACCOUNT
00957                                     ACCT-ACCOUNT
00958          PERFORM 1200-READ-ACCOUNT THRU 1200-EXIT
00959          PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT
00960          MOVE 'EL630B'           TO PI-MAP-NAME.
00961          GO TO 0360-XCTL-EL6301.
00962
00963  0350-NO-RECORDS.
00964      IF MAINTI = 'N'
00965          MOVE SPACE              TO PI-ISSUE-ADDED-SW
00966          MOVE ZEROS              TO PI-LF-ISS-ENTERED
00967                                     PI-LF-CAN-ENTERED
00968                                     PI-AH-ISS-ENTERED
00969                                     PI-AH-CAN-ENTERED
00970                                     PI-ISS-CNT-ENTERED
00971                                     PI-CAN-CNT-ENTERED
00972          PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT
00973          IF PI-LF-ISS-REMITTED  = ZEROS AND
00974             PI-AH-ISS-REMITTED  = ZEROS AND
00975             PI-LF-CAN-REMITTED  = ZEROS AND
00976             PI-AH-CAN-REMITTED  = ZEROS AND
00977             PI-ISS-CNT-REMITTED = ZEROS AND
00978             PI-CAN-CNT-REMITTED = ZEROS
00979              MOVE 'EL630B'     TO PI-MAP-NAME
00980              GO TO 0360-XCTL-EL6301
00981          ELSE
00982              IF PI-LF-ISS-REMITTED  = ZEROS AND
00983                 PI-AH-ISS-REMITTED  = ZEROS AND
00984                 PI-ISS-CNT-REMITTED = ZEROS
00985                  MOVE 'EL630C'     TO PI-MAP-NAME
00986                  GO TO 0360-XCTL-EL6301
00987              ELSE
00988                  MOVE 'EL630B'     TO PI-MAP-NAME
00989                  GO TO 0360-XCTL-EL6301
00990      ELSE
00991          MOVE ER-2212            TO EMI-ERROR
00992          MOVE -1                 TO BATCHL
00993          MOVE AL-UABON           TO BATCHA
00994          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00995          GO TO 8200-SEND-DATAONLY.
00996
00997  0360-XCTL-EL6301.
00998      IF PI-MAINT-FUNC = 'N'
00999          MOVE SPACES             TO PI-KEYED-SWITCHES
01000                                     PI-AGE-KEYED-SW
01001                                     PI-BIRTHDT-KEYED-SW.
01002
020816     if pi-company-id = 'VPP'
020816        move 'VP6301'            to pgm-name
020816        move 'VP630B'            to PI-MAP-NAME
020816     else
              MOVE XCTL-6301           TO PGM-NAME
020816     end-if
01004
01005      GO TO 9300-XCTL.
01006      EJECT
01007  0400-PRIME-BATCH-TOTALS.
01008      
      * EXEC CICS HANDLE CONDITION
01009 *        NOTFND (0480-NO-BATCH-TRAILER)
01010 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00006251' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303036323531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01011
01012      MOVE PI-COMPANY-CD          TO PNDB-COMP-CD.
01013      MOVE 9999                   TO PNDB-BATCH-SEQ.
01014      MOVE BATCHI                 TO PNDB-ENTRY-BATCH.
01015
01016      
      * EXEC CICS READ
01017 *        DATASET (ERPNDB-FILE-ID)
01018 *        SET     (ADDRESS OF PENDING-BUSINESS)
01019 *        RIDFLD  (ERPNDB-KEY)
01020 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006259' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01021
01022      IF PB-BILLED-DT NOT = LOW-VALUES
01023          MOVE ER-2402            TO EMI-ERROR
01024          MOVE -1                 TO BATCHL
01025          MOVE AL-UABON           TO BATCHA
01026          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01027          GO TO 0490-EXIT.
01028
01029      MOVE SPACES                   TO PI-ACCT-AGENT-PROCESSED-SW.
01030      MOVE PI-COMPANY-CD            TO PI-SAV-COMP-CD.
01031      MOVE PB-ENTRY-BATCH           TO PI-SAV-ENTRY-BATCH.
01032
01033      MOVE PB-REFERENCE             TO PI-SAV-REFERENCE.
01034      MOVE PB-CSR-ID                TO PI-CSR-ID.
01035
01036      MOVE PB-CARRIER               TO PI-SAV-CARRIER.
01037      MOVE PB-GROUPING              TO PI-SAV-GROUPING.
01038      MOVE PB-STATE                 TO PI-SAV-STATE.
01039      MOVE PB-ACCOUNT               TO PI-SAV-ACCOUNT.
01040      MOVE PB-B-LF-ISS-PRM-REMITTED TO PI-LF-ISS-REMITTED.
01041      MOVE PB-B-LF-ISS-PRM-ENTERED  TO PI-LF-ISS-ENTERED.
01042      MOVE PB-B-AH-ISS-PRM-REMITTED TO PI-AH-ISS-REMITTED.
01043      MOVE PB-B-AH-ISS-PRM-ENTERED  TO PI-AH-ISS-ENTERED.
01044      MOVE PB-B-LF-CAN-PRM-REMITTED TO PI-LF-CAN-REMITTED.
01045      MOVE PB-B-LF-CAN-PRM-ENTERED  TO PI-LF-CAN-ENTERED.
01046      MOVE PB-B-AH-CAN-PRM-REMITTED TO PI-AH-CAN-REMITTED.
01047      MOVE PB-B-AH-CAN-PRM-ENTERED  TO PI-AH-CAN-ENTERED.
01048      MOVE PB-B-ISSUE-CNT-REMITTED  TO PI-ISS-CNT-REMITTED.
01049      MOVE PB-B-ISSUE-CNT-ENTERED   TO PI-ISS-CNT-ENTERED.
01050      MOVE PB-B-CANCEL-CNT-REMITTED TO PI-CAN-CNT-REMITTED.
01051      MOVE PB-B-CANCEL-CNT-ENTERED  TO PI-CAN-CNT-ENTERED.
01052
01053 ******************************************************************
01054 *    IF THE MONTH-END-DATE IN THE TRAILER RECORD (BATCH HEADER)  *
01055 *       IS NOT EQUAL TO THE ACCOUNTS MONTH-END-DATE PRIME THE    *
01056 *       TRAILER'S MONTH-END-DATE.                                *
01057 ******************************************************************
01058
01059      MOVE SPACE                    TO PI-NB-MONTH-END-DT.
01060      IF  PB-CREDIT-SELECT-DT = PI-CR-MONTH-END-DT
01061          NEXT SENTENCE
01062        ELSE
01063          MOVE PB-CREDIT-SELECT-DT  TO  PI-NB-MONTH-END-DT.
01064
01065      IF  PB-B-RECEIVED-DT = SPACES OR LOW-VALUES
01066          MOVE PB-INPUT-DT          TO PI-RECEIVED-DT
01067      ELSE
01068          MOVE PB-B-RECEIVED-DT     TO PI-RECEIVED-DT.
01069
01070      GO TO 0490-EXIT.
01071
01072  0480-NO-BATCH-TRAILER.
01073      MOVE ER-2212                TO EMI-ERROR.
01074      MOVE -1                     TO BATCHL.
01075      MOVE AL-UABON               TO BATCHA.
01076      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01077
01078  0490-EXIT.
01079      EXIT.
01080      EJECT
01081  1000-VERIFY-CARRIER-ID.
01082      MOVE SPACES                 TO ELCNTL-KEY.
01083      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
01084      MOVE '6'                    TO CNTL-REC-TYPE.
01085      MOVE CARRIERI               TO CNTL-CARRIER
01086      MOVE +0                     TO CNTL-SEQ.
01087
01088      
      * EXEC CICS HANDLE CONDITION
01089 *        NOTFND   (1000-NO-CARRIER)
01090 *    END-EXEC.
      *    MOVE '"$I                   ! % #00006331' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036333331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01091
01092      
      * EXEC CICS READ
01093 *        DATASET   (ELCNTL-FILE-ID)
01094 *        SET       (ADDRESS OF CONTROL-FILE)
01095 *        RIDFLD    (ELCNTL-KEY)
01096 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006335' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01097
01098      GO TO 1000-EXIT.
01099
01100  1000-NO-CARRIER.
01101      MOVE ER-2208                TO EMI-ERROR
01102      MOVE -1                     TO CARRIERL
01103      MOVE AL-UABON               TO CARRIERA
01104      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01105
01106  1000-EXIT.
01107      EXIT.
01108      EJECT
01109  1100-VERIFY-STATE-ID.
01110      MOVE SPACES                 TO ELCNTL-KEY.
01111      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
01112      MOVE '3'                    TO CNTL-REC-TYPE.
01113      MOVE STATEI                 TO CNTL-STATE.
01114      MOVE +0                     TO CNTL-SEQ.
01115
01116      
      * EXEC CICS HANDLE CONDITION
01117 *        NOTFND   (1100-NO-STATE)
01118 *    END-EXEC.
      *    MOVE '"$I                   ! & #00006359' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036333539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01119
01120      
      * EXEC CICS READ
01121 *        DATASET   (ELCNTL-FILE-ID)
01122 *        SET       (ADDRESS OF CONTROL-FILE)
01123 *        RIDFLD    (ELCNTL-KEY)
01124 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006363' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01125
01126      GO TO 1100-EXIT.
01127
01128  1100-NO-STATE.
01129      MOVE ER-2209                TO EMI-ERROR.
01130      MOVE -1                     TO STATEL.
01131      MOVE AL-UABON               TO STATEA.
01132      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01133      GO TO 8200-SEND-DATAONLY.
01134
01135  1100-EXIT.
01136      EXIT.
01137      EJECT
01138
01139  1200-VERIFY-ACCOUNT.
01140      IF CARRIERL GREATER ZEROS
01141          MOVE CARRIERI           TO ACCT-CARRIER
01142      ELSE
01143          MOVE SPACES             TO ACCT-CARRIER.
01144
01145      IF GROUPL GREATER ZEROS
01146          MOVE GROUPI             TO ACCT-GROUPING
01147      ELSE
01148          MOVE SPACES             TO ACCT-GROUPING.
01149
01150      IF STATEL GREATER ZEROS
01151          MOVE STATEI             TO ACCT-STATE
01152      ELSE
01153          MOVE SPACES             TO ACCT-STATE.
01154
01155      MOVE ACCOUNTI               TO ACCT-ACCOUNT.
01156
01157  1200-READ-ACCOUNT.
01158      MOVE PI-COMPANY-CD          TO ACCT-CO.
01159      MOVE LOW-VALUES             TO ACCT-EXP-DATE
01160                                     PI-ACCT-LOW-EFF-DT
01161                                     PI-ACCT-HIGH-EXP-DT.
01162
01163      
      * EXEC CICS HANDLE CONDITION
01164 *        NOTFND   (1200-ACCOUNT-INVALID)
01165 *        ENDFILE  (1200-ACCOUNT-INVALID)
01166 *    END-EXEC.
      *    MOVE '"$I''                  ! '' #00006406' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036343036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01167
01168      
      * EXEC CICS STARTBR
01169 *        DATASET (ERACCT2-FILE-ID)
01170 *        RIDFLD  (ERACCT-KEY)
01171 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006411' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-FILE-ID, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01172
01173      MOVE ERACCT-KEY             TO ERACCT-SAVE-KEY.
01174
01175  1200-READ-LOOP.
01176      
      * EXEC CICS READNEXT
01177 *        DATASET   (ERACCT2-FILE-ID)
01178 *        SET       (ADDRESS OF ACCOUNT-MASTER)
01179 *        RIDFLD    (ERACCT-KEY)
01180 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006419' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01181
01182      IF ERACCT-COMP-KEY NOT = ERACCT-SAVE-KEY
01183          GO TO 1200-ACCOUNT-INVALID.
01184
01185      IF WS-ON1-SW1 IS EQUAL TO 'Y'
01186          MOVE 'N'                TO WS-ON1-SW1
01187          MOVE +0                 TO WS-SUB1
01188          MOVE AM-CARRIER         TO PI-SAV-FC-CARRIER
01189          MOVE AM-GROUPING        TO PI-SAV-FC-GROUPING
01190          MOVE AM-STATE           TO PI-SAV-FC-STATE
01191          MOVE AM-VG-CARRIER      TO PI-SAV-CARRIER
01192          MOVE AM-VG-GROUPING     TO PI-SAV-GROUPING
01193          MOVE AM-VG-STATE        TO PI-SAV-STATE
01194          MOVE AM-VG-ACCOUNT      TO PI-SAV-ACCOUNT
01195          MOVE AM-EFFECTIVE-DT    TO PI-ACCT-LOW-EFF-DT.
01196
01197      MOVE AM-EXPIRATION-DT       TO PI-ACCT-HIGH-EXP-DT.
01198      MOVE AM-NAME                TO PI-AM-NAME.
030310     IF AM-EDIT-LOAN-OFC = 'Y'
030310        MOVE 'Y'                 TO PI-AM-EDIT-LOAN-OFC
030310     ELSE
030310        MOVE 'N'                 TO PI-AM-EDIT-LOAN-OFC
030310     END-IF
072308     IF AM-GPCD = 02 OR 03 OR 04 OR 05
072308        MOVE AM-PERSON           TO PI-AM-ADDR1
072308        MOVE AM-ADDRS            TO PI-AM-ADDR2
072308*       MOVE AM-CITY             TO PI-AM-CITYST
              MOVE AM-ADDR-CITY        TO PI-AM-CITY
              MOVE AM-ADDR-STATE       TO PI-AM-STATE
072308        MOVE AM-ZIP              TO PI-AM-ZIP
           ELSE
              MOVE SPACES TO PI-AM-ADDR1 PI-AM-ADDR2
                            PI-AM-CITYST PI-AM-ZIP
072308     END-IF
01199
01200      MOVE AM-CARRIER             TO PI-COMP-CARRIER.
01201      MOVE AM-GROUPING            TO PI-COMP-GROUPING.
01202      MOVE AM-CSR-CODE            TO PI-CSR-ID.
01203
01204      IF PI-AR-PROCESSING
01205         MOVE 'Y'                 TO WS-RECORD-FOUND-SW
01206         PERFORM 1260-FIND-ACCT-AGENT THRU 1260-EXIT
01207         IF RECORD-NOT-FOUND
01208            GO TO 8200-SEND-DATAONLY.
01209
01210      GO TO 1200-READ-LOOP.
01211
01212  1200-ACCOUNT-INVALID.
01213      IF PI-ACCT-LOW-EFF-DT NOT = LOW-VALUES
01214         MOVE 'Y'                 TO PI-ACCT-AGENT-PROCESSED-SW
01215         GO TO 1200-EXIT.
01216
01217      IF CARR-GROUP-ST-ACCNT-CNTL
01218          MOVE -1                     TO CARRIERL
01219          MOVE AL-UABON               TO CARRIERA
01220                                         GROUPA
01221                                         STATEA
01222                                         ACCOUNTA
01223      ELSE
01224          IF ST-ACCNT-CNTL
01225              MOVE -1                 TO STATEL
01226              MOVE AL-UABON           TO STATEA
01227                                         ACCOUNTA
01228          ELSE
01229              IF CARR-ST-ACCNT-CNTL
01230                  MOVE -1             TO CARRIERL
01231                  MOVE AL-UABON       TO CARRIERA
01232                                         STATEA
01233                                         ACCOUNTA
01234              ELSE
01235                  IF ACCNT-CNTL
01236                      MOVE -1         TO ACCOUNTL
01237                      MOVE AL-UABON   TO ACCOUNTA
01238                  ELSE
01239                      MOVE -1         TO CARRIERL
01240                      MOVE AL-UABON   TO CARRIERA
01241                                         ACCOUNTA.
01242
01243      MOVE ER-2210                TO EMI-ERROR.
01244      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01245
01246  1200-EXIT.
01247      EXIT.
01248      EJECT
01249  1250-READ-COMPANY-REC.
01250      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
01251      MOVE '1'                    TO CNTL-REC-TYPE.
01252      MOVE SPACES                 TO CNTL-ACCESS.
01253      MOVE +0                     TO CNTL-SEQ.
01254
01255      
      * EXEC CICS HANDLE CONDITION
01256 *        NOTFND   (1250-NO-COMPANY-REC)
01257 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00006514' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036353134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01258
01259      
      * EXEC CICS READ
01260 *        DATASET   (ELCNTL-FILE-ID)
01261 *        SET       (ADDRESS OF CONTROL-FILE)
01262 *        RIDFLD    (ELCNTL-KEY)
01263 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006518' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01264
01265      MOVE CF-CREDIT-EDIT-CONTROLS TO PI-CREDIT-EDIT-CONTROLS.
01266      GO TO 1250-EXIT.
01267
01268  1250-NO-COMPANY-REC.
01269      MOVE ER-2248                TO EMI-ERROR.
01270      MOVE -1                     TO BATCHL.
01271      MOVE AL-UABON               TO CARRIERA
01272      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01273      GO TO 8200-SEND-DATAONLY.
01274
01275  1250-EXIT.
01276      EXIT.
01277      EJECT
01278
01279  1260-FIND-ACCT-AGENT.
01280
01281 ******************************************************************
01282 *                                                                *
01283 *            F I N D   A C C O U N T   A G E N T                 *
01284 *                                                                *
01285 *    NOTE:  IT IS NECESSARY TO LOAD EVERY DATE RANGE FOR         *
01286 *           EACH ACCOUNT AGENT / FINANCIAL RESPONSIBILITY        *
01287 *           INTO THE PROGRAM INTERFACE AREA.  THE ACCOUNT        *
01288 *           DATE RANGES ARE CHECKED DURING DATA ENTRY IN         *
01289 *           EL6301.                                              *
01290 *                                                                *
01291 ******************************************************************
01292
01293      IF PI-ACCT-AGENT-PROCESSED
01294         GO TO 1260-EXIT.
01295
01296      IF WS-ON1-SW2 IS EQUAL TO 'Y'
01297         MOVE 'N'                 TO WS-ON1-SW2
01298      ELSE
01299         MOVE +0                  TO WS-SUB2
01300         GO TO 1260-CONT-FIND-ACCT-AGENT.
01301
01302      MOVE +0                     TO WS-SUB2.
01303
01304  1260-INITIALIZE-DATE-RANGES.
01305      ADD +1                      TO WS-SUB2.
01306
01307      IF WS-SUB2 GREATER +32
01308         GO TO 1260-INITIALIZE-FINISHED.
01309
01310      MOVE SPACES                 TO PI-ACCT-EFF-DT (WS-SUB2)
01311                                     PI-ACCT-EXP-DT (WS-SUB2)
01312                                     PI-REMIT-AGENT (WS-SUB2)
01313                                     PI-ACCT-AGENT  (WS-SUB2).
01314
01315      GO TO 1260-INITIALIZE-DATE-RANGES.
01316
01317  1260-INITIALIZE-FINISHED.
01318      MOVE SPACES                 TO PI-ACCOUNT-AGENT
01319                                     PI-FIN-RESP.
01320
01321      MOVE 'N'                    TO PI-ACCT-AGENT-ERROR-SW
01322                                     PI-FIN-RESP-ERROR-SW.
01323
01324      MOVE +0                     TO WS-SUB2.
01325
01326  1260-CONT-FIND-ACCT-AGENT.
01327      ADD  +1                     TO WS-SUB2.
01328
01329      IF WS-SUB2 GREATER +10
01330         MOVE  SPACES             TO PI-ACCT-AGENT (WS-SUB1)
01331         GO TO 1260-EXIT.
01332
052814     IF AM-COM-TYP (WS-SUB2) = 'C' OR 'D' OR 'F'
01334         NEXT SENTENCE
01335      ELSE
01336         GO TO 1260-CONT-FIND-ACCT-AGENT.
01337
01338      IF WS-SUB1 = +0
01339         MOVE +1                   TO WS-SUB1
01340                                      PI-SUB
01341         MOVE AM-EFFECTIVE-DT      TO PI-ACCT-EFF-DT (WS-SUB1)
01342         MOVE AM-EXPIRATION-DT     TO PI-ACCT-EXP-DT (WS-SUB1)
01343         MOVE AM-AGT (WS-SUB2)     TO PI-ACCT-AGENT  (WS-SUB1)
01344         MOVE AM-AGT (AM-REMIT-TO) TO PI-REMIT-AGENT (WS-SUB1)
01345         GO TO 1260-EXIT.
01346
01347      IF AM-AGT (WS-SUB2)     = PI-ACCT-AGENT  (WS-SUB1) AND
01348         AM-AGT (AM-REMIT-TO) = PI-REMIT-AGENT (WS-SUB1)
01349           MOVE AM-EXPIRATION-DT  TO PI-ACCT-EXP-DT (WS-SUB1)
01350         ELSE
01351           ADD +1                      TO WS-SUB1
01352           IF WS-SUB1 GREATER +32
01353               MOVE  ER-2119             TO EMI-ERROR
01354               MOVE -1                   TO MAINTL
01355               PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01356               MOVE 'N'                  TO WS-RECORD-FOUND-SW
01357               GO TO 1260-EXIT
01358           ELSE
01359            MOVE WS-SUB1              TO PI-SUB
01360            MOVE AM-EFFECTIVE-DT      TO PI-ACCT-EFF-DT (WS-SUB1)
01361            MOVE AM-EXPIRATION-DT     TO PI-ACCT-EXP-DT (WS-SUB1)
01362            MOVE AM-AGT (WS-SUB2)     TO PI-ACCT-AGENT  (WS-SUB1)
01363            MOVE AM-AGT (AM-REMIT-TO) TO PI-REMIT-AGENT (WS-SUB1).
01364
01365  1260-EXIT.
01366       EXIT.
01367
01368      EJECT
01369
01370  1300-GET-BATCH-NO.
01371      
      * EXEC CICS HANDLE CONDITION
01372 *        NOTFND   (1300-NO-COMPANY-REC)
01373 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00006630' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303036363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01374
01375      
      * EXEC CICS READ
01376 *        DATASET   (ELCNTL-FILE-ID)
01377 *        SET       (ADDRESS OF CONTROL-FILE)
01378 *        RIDFLD    (ELCNTL-KEY)
01379 *        UPDATE
01380 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006634' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01381
01382  1300-ASSIGN-NUMBER-LOOP.
01383      IF CF-LAST-BATCH-RESET
01384          MOVE ZEROS              TO CF-LAST-BATCH-NO
01385          ADD +1                  TO CF-LAST-BATCH-NO
01386      ELSE
01387          ADD +1                  TO CF-LAST-BATCH-NO.
01388
01389      MOVE CF-LAST-BATCH-NO       TO WS-BATCH-NO.
01390      MOVE WS-BATCH-NO            TO PI-SAV-ENTRY-BATCH.
01391
01392  1300-REWRITE-COMPANY-REC.
01393      MOVE WS-BATCH-NO            TO BATCHI.
01394      MOVE AL-UANON               TO BATCHA.
01395
01396      
      * EXEC CICS REWRITE
01397 *        DATASET (ELCNTL-FILE-ID)
01398 *        FROM    (CONTROL-FILE)
01399 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006655' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01400
01401      
      * EXEC CICS SYNCPOINT
01402 *    END-EXEC.
      *    MOVE '6"                    !   #00006660' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01403
01404  1300-CHECK-FOR-DUP-BATCH.
01405      
      * EXEC CICS HANDLE CONDITION
01406 *        NOTFND  (1300-EXIT)
01407 *    END-EXEC.
      *    MOVE '"$I                   ! * #00006664' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303036363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01408
01409      
      * EXEC CICS READ
01410 *        SET     (ADDRESS OF PENDING-BUSINESS)
01411 *        DATASET (ERPNDB-FILE-ID)
01412 *        RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)
01413 *        GTEQ
01414 *    END-EXEC.
      *    MOVE '&"S        G          (   #00006668' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01415
01416      IF PI-SAV-COMP-CD NOT = PB-COMPANY-CD
01417          GO TO 1300-EXIT.
01418
01419      IF PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH
01420          MOVE LOW-VALUES         TO EL630AO
01421          MOVE QUESTION-MARKS     TO BATCHO
01422          MOVE AL-UABON           TO BATCHA
01423          MOVE -1                 TO BATCHL
01424          MOVE ER-2229            TO EMI-ERROR
01425          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01426          GO TO 8100-SEND-INITIAL-MAP.
01427
01428      GO TO 1300-EXIT.
01429
01430  1300-NO-COMPANY-REC.
01431      MOVE ER-2248                TO EMI-ERROR.
01432      MOVE -1                     TO BATCHL.
01433      MOVE AL-UABON               TO CARRIERA
01434      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01435      GO TO 8200-SEND-DATAONLY.
01436
01437  1300-EXIT.
01438      EXIT.
01439      EJECT
01440
01441  1400-VERIFY-COMP-MASTER.
01442
01443 ******************************************************************
01444 *
01445 *      V E R I F Y    C O M P E N S A T I O N    M A S T E R
01446 *
01447 *       READ COMPENSATION MASTER AND SAVE SUMMARY CODE FOR
01448 *       A.R. PROCESSING.
01449 *
01450 ******************************************************************
01451
01452      
      * EXEC CICS HANDLE CONDITION
01453 *        NOTFND   (1470-NO-COMP-MSTR)
01454 *    END-EXEC.
      *    MOVE '"$I                   ! + #00006711' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303036373131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01455
01456      MOVE SPACE                  TO WS-COMP-MASTER-ERROR-SW.
01457      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.
01458
01459      IF PI-ZERO-CARRIER OR
01460         PI-ZERO-CAR-GROUP
01461         MOVE ZEROS               TO ERCOMP-CARRIER
01462      ELSE
01463         MOVE PI-COMP-CARRIER     TO ERCOMP-CARRIER.
01464
01465      IF PI-ZERO-GROUPING OR
01466         PI-ZERO-CAR-GROUP
01467         MOVE ZEROS               TO ERCOMP-GROUPING
01468      ELSE
01469         MOVE PI-COMP-GROUPING    TO ERCOMP-GROUPING.
01470
01471      MOVE PI-FIN-RESP            TO ERCOMP-FIN-RESP.
01472      MOVE PI-ACCOUNT-AGENT       TO ERCOMP-ACCT.
01473      MOVE 'A'                    TO ERCOMP-RECORD-TYPE.
01474
01475      
      * EXEC CICS READ
01476 *        DATASET   (ERCOMP-FILE-ID)
01477 *        SET       (ADDRESS OF COMPENSATION-MASTER)
01478 *        RIDFLD    (ERCOMP-KEY)
01479 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006734' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01480
01481      MOVE CO-AR-SUMMARY-CODE     TO PI-SUMMARY-CODE.
01482
01483      IF REFL GREATER ZEROS
01484          GO TO 1490-EXIT.
01485
01486      IF PI-SAV-REFERENCE GREATER SPACES
01487          GO TO 1490-EXIT.
01488
01489      IF CO-AR-NET-REPORT
01490         MOVE 'Y'                 TO WS-UPDATE-REFERENCE-SW
01491         MOVE WS-CURRENT-YMD      TO PI-SAV-REFERENCE
01492      ELSE
01493         MOVE SPACES              TO PI-SAV-REFERENCE.
01494
01495      GO TO 1490-EXIT.
01496
01497  1470-NO-COMP-MSTR.
01498      MOVE ER-2800                TO EMI-ERROR.
01499      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01500      MOVE 'Y'                    TO WS-COMP-MASTER-ERROR-SW.
01501
01502  1490-EXIT.
01503      EXIT.
01504
01505      EJECT
01506
01507 ******************************************************************
01508 *                                                                *
01509 *           W R I T E   R E Q U E S T   R E C O R D              *
01510 *                                                                *
01511 ******************************************************************
01512
01513  1900-WRITE-REQUEST-RECORD.
01514      
      * EXEC CICS GETMAIN
01515 *         SET       (ADDRESS OF AR-REQUEST-RECORD)
01516 *         LENGTH    (200)
01517 *         INITIMG   (GETMAIN-SPACE)
01518 *    END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00006773' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01519
01520      MOVE 'RQ'                   TO RQ-RECORD-ID.
01521
01522      MOVE PI-SAV-ENTRY-BATCH     TO RQ-ENTRY-BATCH
01523                                     RQ-BATCH-A1
01524                                     RQ-BATCH-A2
01525                                     RQ-BATCH-A3
01526                                     RQ-BATCH-A4.
01527
01528      MOVE PI-SAV-REFERENCE       TO RQ-REFERENCE-A1
01529                                     RQ-REFERENCE-A2
01530                                     RQ-REFERENCE-A4.
01531
01532      MOVE PI-COMPANY-CD          TO RQ-COMPANY-CD
01533                                     RQ-COMPANY-CD-A1
01534                                     RQ-COMPANY-CD-A2
01535                                     RQ-COMPANY-CD-A3
01536                                     RQ-COMPANY-CD-A4.
01537
01538      MOVE PI-SAV-FC-CARRIER      TO RQ-CARRIER-A1
01539                                     RQ-CARRIER-A2
01540                                     RQ-CARRIER-A3
01541                                     PI-CR-CARRIER.
01542
01543      MOVE PI-SAV-FC-GROUPING     TO RQ-GROUPING-A1
01544                                     RQ-GROUPING-A2
01545                                     RQ-GROUPING-A3
01546                                     PI-CR-GROUPING.
01547
01548      IF PI-ZERO-CARRIER
01549         MOVE ZERO                TO RQ-CARRIER-A2
01550                                     PI-CR-CARRIER.
01551
01552      IF PI-ZERO-GROUPING
01553         MOVE ZEROS               TO RQ-GROUPING-A2
01554                                     PI-CR-GROUPING.
01555
01556      IF PI-ZERO-CAR-GROUP
01557         MOVE ZEROS               TO RQ-CARRIER-A2
01558                                     RQ-GROUPING-A2
01559                                     PI-CR-CARRIER
01560                                     PI-CR-GROUPING.
01561
01562      MOVE PI-SAV-FC-STATE        TO RQ-STATE-A1
01563                                     PI-CR-STATE.
01564
01565      MOVE PI-SAV-ACCOUNT         TO RQ-ACCOUNT-A1
01566                                     RQ-ACCOUNT-A4
01567                                     PI-CR-ACCOUNT.
01568
01569
01570      MOVE PI-ACCOUNT-AGENT       TO RQ-ACCT-AGENT-A2
01571                                     RQ-ACCT-AGENT-A3.
01572
01573      MOVE WS-CURRENT-BIN-DT      TO RQ-ENTRY-DT.
01574
01575      IF  PI-NB-MONTH-END-DT GREATER SPACES
01576          MOVE PI-NB-MONTH-END-DT TO RQ-MO-END-DT
01577      ELSE
01578          MOVE PI-CR-MONTH-END-DT TO RQ-MO-END-DT.
01579
01580      MOVE LOW-VALUES             TO RQ-REQUEST-DT
01581                                     RQ-STMT-DT
01582                                     RQ-REVERSAL-DT
01583                                     RQ-CREDIT-SELECT-DT
01584                                     RQ-CREDIT-ACCEPT-DT.
01585
01586      IF PI-FIN-RESP-ERROR
01587         MOVE 'E'                 TO RQ-STATUS
01588         MOVE 'UNKNOWN'           TO RQ-FIN-RESP-A2
01589                                     PI-CR-FIN-RESP
01590      ELSE
01591         MOVE PI-FIN-RESP         TO RQ-FIN-RESP-A2
01592                                     PI-CR-FIN-RESP.
01593
01594      IF PI-ACCT-AGENT-ERROR
01595         MOVE 'E'                 TO RQ-STATUS
01596         MOVE 'UNKNOWN'           TO RQ-ACCT-AGENT-A2
01597                                     RQ-ACCT-AGENT-A3.
01598
01599      IF WS-COMP-MASTER-ERROR
01600         MOVE 'E'                 TO RQ-STATUS
01601         MOVE 'UNKNOWN'           TO PI-CR-FIN-RESP.
01602
01603      MOVE RQ-CONTROL-PRIMARY     TO ERRQST-KEY.
01604      MOVE LOW-VALUES             TO RQ-STMT-DT
01605                                     RQ-REQUEST-DT
01606                                     RQ-REVERSAL-DT.
01607
01608      MOVE CO-AR-SUMMARY-CODE     TO RQ-SUMMARY-CODE.
01609
01610      IF  PI-NB-MONTH-END-DT GREATER SPACES
01611          MOVE PI-NB-MONTH-END-DT TO PB-CREDIT-SELECT-DT
01612      ELSE
01613          MOVE PI-CR-MONTH-END-DT TO PB-CREDIT-SELECT-DT.
01614
01615      
      * EXEC CICS HANDLE CONDITION
01616 *        DUPREC (1990-EXIT)
01617 *    END-EXEC.
      *    MOVE '"$%                   ! , #00006874' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303036383734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01618
01619      
      * EXEC CICS WRITE
01620 *         DATASET    (ERRQST-FILE-ID)
01621 *         FROM       (AR-REQUEST-RECORD)
01622 *         RIDFLD     (ERRQST-KEY)
01623 *    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006878' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 ERRQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01624
01625  1990-EXIT.
01626      EXIT.
01627
01628      EJECT
01629  2000-WRITE-BATCH-TOTAL-REC.
01630
01631      MOVE PI-COMPANY-CD          TO PNDB-COMP-CD.
01632      MOVE PI-SAV-ENTRY-BATCH     TO PNDB-ENTRY-BATCH.
01633      MOVE 9999                   TO PNDB-BATCH-SEQ.
01634
01635      
      * EXEC CICS HANDLE CONDITION
01636 *        NOTFND (2100-ADD-BATCH-TOTAL-REC)
01637 *    END-EXEC.
      *    MOVE '"$I                   ! - #00006894' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303036383934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01638
01639      
      * EXEC CICS READ
01640 *        SET    (ADDRESS OF PENDING-BUSINESS)
01641 *        DATASET(ERPNDB-FILE-ID)
01642 *        RIDFLD (ERPNDB-KEY)
01643 *        UPDATE
01644 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006898' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01645
01646 ******************************************************************
01647 *    IF THE MONTH-END-DATE IN THE TRAILER RECORD (BATCH HEADER)  *
01648 *       IS NOT EQUAL TO THE ACCOUNTS MONTH-END-DATE AND THE      *
01649 *       OPERATOR HAS NOT ENTERED A NEW MONTH-END-DATE USE THE    *
01650 *       MONTH-END-DATE IN TRAILER RECORD FOR ALL DETAIL RECORDS. *
01651 ******************************************************************
01652
01653      IF  PI-NB-MONTH-END-DT = SPACES
01654          IF  PB-CREDIT-SELECT-DT = PI-CR-MONTH-END-DT
01655              NEXT SENTENCE
01656          ELSE
01657              MOVE PB-CREDIT-SELECT-DT  TO  PI-NB-MONTH-END-DT.
01658
01659      IF PI-AR-PROCESSING
01660         IF REFL = ZEROS
01661            IF PB-REFERENCE GREATER SPACES
01662               MOVE PB-REFERENCE  TO PI-SAV-REFERENCE
01663            ELSE
01664               NEXT SENTENCE
01665         ELSE
01666            IF PI-SAV-REFERENCE NOT = PB-REFERENCE
01667               MOVE 'Y'              TO WS-UPDATE-REFERENCE-SW
01668                                        WS-RECORD-FOUND-SW
01669               PERFORM 7100-REWRITE-REQUEST-REC THRU 7190-EXIT
01670               IF RECORD-NOT-FOUND
01671                   GO TO 8200-SEND-DATAONLY
01672               ELSE
01673                   MOVE PI-SAV-REFERENCE TO PB-REFERENCE.
01674
01675      IF ELFISSL NOT = ZEROS
01676          MOVE PI-LF-ISS-REMITTED  TO PB-B-LF-ISS-PRM-REMITTED.
01677      IF EAHISSL NOT = ZEROS
01678          MOVE PI-AH-ISS-REMITTED  TO PB-B-AH-ISS-PRM-REMITTED.
01679      IF EISSCNTL NOT = ZEROS
01680          MOVE PI-ISS-CNT-REMITTED TO PB-B-ISSUE-CNT-REMITTED.
01681      IF ECANCNTL NOT = ZEROS
01682          MOVE PI-CAN-CNT-REMITTED TO PB-B-CANCEL-CNT-REMITTED.
01683      IF ELFCANL NOT = ZEROS
01684          MOVE PI-LF-CAN-REMITTED  TO PB-B-LF-CAN-PRM-REMITTED.
01685      IF EAHCANL NOT = ZEROS
01686          MOVE PI-AH-CAN-REMITTED  TO PB-B-AH-CAN-PRM-REMITTED.
01687
01688      IF WS-PF1
01689          MOVE PI-LF-ISS-ENTERED   TO PB-B-LF-ISS-PRM-ENTERED
01690          MOVE PI-AH-ISS-ENTERED   TO PB-B-AH-ISS-PRM-ENTERED
01691          MOVE PI-ISS-CNT-ENTERED  TO PB-B-ISSUE-CNT-ENTERED
01692          MOVE PI-CAN-CNT-ENTERED  TO PB-B-CANCEL-CNT-ENTERED
01693          MOVE PI-LF-CAN-ENTERED   TO PB-B-LF-CAN-PRM-ENTERED
01694          MOVE PI-AH-CAN-ENTERED   TO PB-B-AH-CAN-PRM-ENTERED
01695          MOVE PI-LAST-SEQ-NO-ADDED TO PB-B-HIGHEST-SEQ-NO.
01696
01697      MOVE PI-RECEIVED-DT         TO PB-B-RECEIVED-DT.
01698
01699      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
01700      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
01701      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.
01702
01703      IF PI-AR-PROCESSING
01704         IF PI-DATA-UPDATED
01705            PERFORM 1400-VERIFY-COMP-MASTER   THRU 1490-EXIT
01706            PERFORM 1900-WRITE-REQUEST-RECORD THRU 1990-EXIT
01707            IF PI-MAINT-FUNC NOT = 'K'
01708               MOVE PI-SAV-REFERENCE TO PB-REFERENCE.
01709
01710      
      * EXEC CICS REWRITE
01711 *        DATASET(ERPNDB-FILE-ID)
01712 *        FROM   (PENDING-BUSINESS)
01713 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006969' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01714
01715      IF  PI-AR-PROCESSING
01716          IF WS-UPDATE-REFERENCE
01717             PERFORM 5000-UPDATE-ENTIRE-BATCH THRU 5900-EXIT
01718             GO TO 2990-EXIT.
01719
01720      IF PI-NB-MONTH-END-DT = PB-CREDIT-SELECT-DT
01721         GO TO 2990-EXIT.
01722
01723      IF PI-NB-MONTH-END-DT GREATER SPACES
01724          PERFORM 5000-UPDATE-ENTIRE-BATCH THRU 5900-EXIT
01725              IF PI-AR-PROCESSING
01726                  MOVE 'Y'          TO WS-RECORD-FOUND-SW
01727                  PERFORM 7200-REWRITE-REQUEST THRU 7299-EXIT
01728                  IF RECORD-NOT-FOUND
01729                      GO TO 8200-SEND-DATAONLY.
01730
01731      GO TO 2990-EXIT.
01732
01733      EJECT
01734
01735  2100-ADD-BATCH-TOTAL-REC.
01736      
      * EXEC CICS GETMAIN
01737 *        SET    (ADDRESS OF PENDING-BUSINESS)
01738 *        LENGTH (585)
01739 *        INITIMG(GETMAIN-SPACE)
01740 *    END-EXEC.
           MOVE 585
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00006995' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01741
01742      MOVE 'PB'                   TO PB-RECORD-ID.
01743      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD
01744                                     PB-COMPANY-CD-A1.
01745      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.
01746      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH
01747                                     PB-CERT-NO.
01748      MOVE 9999                   TO PB-BATCH-SEQ-NO.
01749      MOVE HIGH-VALUES            TO PB-CERT-EFF-DT.
01750      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO
01751                                     PB-ALT-CHG-SEQ-NO.
01752
01753      IF REFL GREATER THAN +0
01754         MOVE REFI                TO PB-REFERENCE
01755                                     PI-SAV-REFERENCE.
01756
01757      IF PI-LF-ISS-REMITTED     NOT NUMERIC
01758         MOVE +0                  TO PI-LF-ISS-REMITTED.
01759
01760      IF PI-LF-ISS-ENTERED      NOT NUMERIC
01761         MOVE +0                  TO PI-LF-ISS-ENTERED.
01762
01763      IF PI-AH-ISS-REMITTED     NOT NUMERIC
01764         MOVE +0                  TO PI-AH-ISS-REMITTED.
01765
01766      IF PI-AH-ISS-ENTERED      NOT NUMERIC
01767         MOVE +0                  TO PI-AH-ISS-ENTERED.
01768
01769      IF PI-ISS-CNT-REMITTED    NOT NUMERIC
01770         MOVE +0                  TO PI-ISS-CNT-REMITTED.
01771
01772      IF PI-ISS-CNT-ENTERED     NOT NUMERIC
01773         MOVE +0                  TO PI-ISS-CNT-ENTERED.
01774
01775      IF PI-CAN-CNT-REMITTED    NOT NUMERIC
01776         MOVE +0                  TO PI-CAN-CNT-REMITTED.
01777
01778      IF PI-CAN-CNT-ENTERED     NOT NUMERIC
01779         MOVE +0                  TO PI-CAN-CNT-ENTERED.
01780
01781      IF PI-LF-CAN-REMITTED     NOT NUMERIC
01782         MOVE +0                  TO PI-LF-CAN-REMITTED.
01783
01784      IF PI-LF-CAN-ENTERED      NOT NUMERIC
01785         MOVE +0                  TO PI-LF-CAN-ENTERED.
01786
01787      IF PI-LF-CAN-REMITTED     NOT NUMERIC
01788         MOVE +0                  TO PI-LF-CAN-REMITTED.
01789
01790      IF PI-AH-CAN-REMITTED     NOT NUMERIC
01791         MOVE +0                  TO PI-AH-CAN-REMITTED.
01792
01793      IF PI-AH-CAN-ENTERED      NOT NUMERIC
01794         MOVE +0                  TO PI-AH-CAN-ENTERED.
01795
01796
01797      MOVE PI-SAV-CARRIER         TO PB-CARRIER.
01798      MOVE PI-SAV-GROUPING        TO PB-GROUPING.
01799      MOVE PI-SAV-STATE           TO PB-STATE.
01800      MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.
01801      MOVE '9'                    TO PB-RECORD-TYPE.
01802      MOVE PI-LF-ISS-REMITTED     TO PB-B-LF-ISS-PRM-REMITTED.
01803      MOVE PI-LF-ISS-ENTERED      TO PB-B-LF-ISS-PRM-ENTERED.
01804      MOVE PI-AH-ISS-REMITTED     TO PB-B-AH-ISS-PRM-REMITTED.
01805      MOVE PI-AH-ISS-ENTERED      TO PB-B-AH-ISS-PRM-ENTERED.
01806      MOVE PI-ISS-CNT-REMITTED    TO PB-B-ISSUE-CNT-REMITTED.
01807      MOVE PI-ISS-CNT-ENTERED     TO PB-B-ISSUE-CNT-ENTERED.
01808      MOVE PI-CAN-CNT-REMITTED    TO PB-B-CANCEL-CNT-REMITTED.
01809      MOVE PI-CAN-CNT-ENTERED     TO PB-B-CANCEL-CNT-ENTERED.
01810      MOVE PI-LF-CAN-REMITTED     TO PB-B-LF-CAN-PRM-REMITTED.
01811      MOVE PI-LF-CAN-ENTERED      TO PB-B-LF-CAN-PRM-ENTERED.
01812      MOVE PI-AH-CAN-REMITTED     TO PB-B-AH-CAN-PRM-REMITTED.
01813      MOVE PI-AH-CAN-ENTERED      TO PB-B-AH-CAN-PRM-ENTERED.
01814      MOVE ZEROS                  TO PB-B-LF-ISS-PRM-COMPUTED
01815                                     PB-B-LF-CAN-PRM-COMPUTED
01816                                     PB-B-AH-ISS-PRM-COMPUTED
01817                                     PB-B-AH-CAN-PRM-COMPUTED
01818                                     PB-LF-BILLED-AMTS
01819                                     PB-AH-BILLED-AMTS
01820                                     PB-CHG-COUNT
01821                                     PB-CALC-TOLERANCE.
01822      MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT
01823                                     PB-BILLED-DT
01824                                     PB-ACCT-EFF-DT
01825                                     PB-ACCT-EXP-DT.
01826
01827      IF  PI-NB-MONTH-END-DT GREATER SPACES
01828          MOVE PI-NB-MONTH-END-DT TO PB-CREDIT-SELECT-DT
01829         ELSE
01830          MOVE PI-CR-MONTH-END-DT TO PB-CREDIT-SELECT-DT.
01831
01832      MOVE PI-LAST-SEQ-NO-ADDED   TO PB-B-HIGHEST-SEQ-NO.
01833
01834      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY
01835                                     PB-INPUT-BY.
01836      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
01837      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT
01838                                     PB-INPUT-DT.
01839
01840      MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.
01841
01842      MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.
01843      MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.
01844      MOVE PI-CSR-ID              TO PB-CSR-ID.
01845      MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.
01846      MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.
01847
01848      MOVE PI-RECEIVED-DT         TO PB-B-RECEIVED-DT.
01849
01850      MOVE PI-AM-NAME             TO  PB-ACCOUNT-NAME.
01851      MOVE PI-SAV-FC-CARRIER      TO  PB-SV-CARRIER.
01852      MOVE PI-SAV-FC-GROUPING     TO  PB-SV-GROUPING.
01853      MOVE PI-SAV-FC-STATE        TO  PB-SV-STATE.
01854
01855      MOVE PB-CONTROL-PRIMARY     TO  ERPNDB-KEY.
01856
01857      
      * EXEC CICS WRITE
01858 *        DATASET(ERPNDB-FILE-ID)
01859 *        FROM   (PENDING-BUSINESS)
01860 *        RIDFLD (PB-CONTROL-PRIMARY)
01861 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007116' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01862
01863  2990-EXIT.
01864      EXIT.
01865
01866      EJECT
01867  3000-DELETE-ENTERED-BATCH.
01868      MOVE SPACE          TO PI-NB-MONTH-END-DT
01869                             PI-SAV-REFERENCE.
01870
01871      IF NOT PI-DELETE-IS-OK
01872          MOVE 'Y'                TO PI-VERIFY-DELETE-SW
01873          GO TO 3300-FIRST-PF2.
01874
01875      MOVE SPACE                  TO PI-ACCT-AGENT-PROCESSED-SW
01876                                     PI-VERIFY-DELETE-SW.
01877      MOVE PI-COMPANY-CD          TO PNDB-COMP-CD.
01878      MOVE PI-SAV-ENTRY-BATCH     TO PNDB-ENTRY-BATCH.
01879      MOVE 1                      TO PNDB-BATCH-SEQ.
01880
01881      
      * EXEC CICS SYNCPOINT
01882 *    END-EXEC.
      *    MOVE '6"                    !   #00007140' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01883
01884      
      * EXEC CICS HANDLE CONDITION
01885 *        NOTFND(3200-NO-RECORDS)
01886 *    END-EXEC.
      *    MOVE '"$I                   ! . #00007143' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303037313433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01887
01888      
      * EXEC CICS STARTBR
01889 *        DATASET(ERPNDB-FILE-ID)
01890 *        RIDFLD(ERPNDB-KEY)
01891 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007147' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01892
01893  3000-GET-NEXT-RECORD.
01894      
      * EXEC CICS HANDLE CONDITION
01895 *        NOTFND(3120-END-ROUTINE)
01896 *    END-EXEC.
      *    MOVE '"$I                   ! / #00007153' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303037313533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01897
01898      ADD +1                      TO  WS-SYNC-CNTR.
01899
01900      IF WS-SYNC-CNTR GREATER +100
01901          MOVE +0                 TO  WS-SYNC-CNTR
01902          
      * EXEC  CICS SYNCPOINT
01903 *        END-EXEC.
      *    MOVE '6"                    !   #00007161' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01904
01905      IF WS-ON1-SW3 IS EQUAL TO 'Y'
01906          MOVE 'N'                TO  WS-ON1-SW3
01907          GO TO 3005-READ-NEXT-RECORD.
01908
01909      
      * EXEC CICS STARTBR
01910 *        DATASET(ERPNDB-FILE-ID)
01911 *        RIDFLD (ERPNDB-KEY)
01912 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007168' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01913
01914      
      * EXEC CICS HANDLE CONDITION
01915 *        NOTFND (3100-STOP-BROWSE)
01916 *        ENDFILE(3100-STOP-BROWSE)
01917 *    END-EXEC.
      *    MOVE '"$I''                  ! 0 #00007173' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303037313733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01918
01919  3005-READ-NEXT-RECORD.
01920      
      * EXEC CICS READNEXT
01921 *        SET    (ADDRESS OF PENDING-BUSINESS)
01922 *        DATASET(ERPNDB-FILE-ID)
01923 *        RIDFLD (ERPNDB-KEY)
01924 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007179' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01925
01926      IF PB-COMPANY-CD  = PI-SAV-COMP-CD AND
01927         PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
01928          NEXT SENTENCE
01929      ELSE
01930          GO TO 3100-STOP-BROWSE.
01931
01932      IF PB-BILLED-DT NOT = LOW-VALUES
01933          
      * EXEC CICS SYNCPOINT ROLLBACK
01934 *        END-EXEC
      *    MOVE '6"R                   !   #00007192' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01935          MOVE ER-2402            TO EMI-ERROR
01936          MOVE -1                 TO BATCHL
01937          MOVE AL-UABON           TO BATCHA
01938          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01939          GO TO 8200-SEND-DATAONLY.
01940
01941 ******************************************************************
01942 *                DELETE PENDING ADDRESS RECORD                   *
01943 ******************************************************************
01944
01945      IF PB-I-MAIL-ADDRS-PRESENT
01946         NEXT SENTENCE
01947        ELSE
01948         GO TO 3006-DELETE-CERTIFICATE.
01949
01950      
      * EXEC CICS HANDLE CONDITION
01951 *         NOTOPEN  (3006-DELETE-CERTIFICATE)
01952 *         NOTFND   (3006-DELETE-CERTIFICATE)
01953 *    END-EXEC.
      *    MOVE '"$JI                  ! 1 #00007209' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303037323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01954
01955      
      * EXEC CICS READ
01956 *        EQUAL
01957 *        DATASET   (ERPNDM-FILE-ID)
01958 *        SET       (ADDRESS OF PENDING-MAILING-DATA)
01959 *        RIDFLD    (ERPNDB-KEY)
01960 *        UPDATE
01961 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007214' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01962
01963      
      * EXEC CICS DELETE
01964 *        DATASET   (ERPNDM-FILE-ID)
01965 *    END-EXEC.
      *    MOVE '&(                    &   #00007222' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDM-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01966
01967  3006-DELETE-CERTIFICATE.
01968      IF PB-BATCH-TRAILER
01969         GO TO 3020-CONTINUE-DELETE.
01970
01971      
      * EXEC CICS HANDLE CONDITION
01972 *         NOTFND  (3020-CONTINUE-DELETE)
01973 *    END-EXEC.
      *    MOVE '"$I                   ! 2 #00007230' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303037323330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01974
01975      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.
01976      MOVE PB-SV-CARRIER          TO CERT-CARRIER.
01977      MOVE PB-SV-GROUPING         TO CERT-GROUPING.
01978      MOVE PB-SV-STATE            TO CERT-STATE.
01979      
      * EXEC CICS READ
01980 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
01981 *        DATASET (ELCERT-FILE-ID)
01982 *        RIDFLD  (ELCERT-KEY)
01983 *        UPDATE
01984 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007238' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323338' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01985
01986      IF INSURED-ADDR-PRESENT
01987         MOVE 'Y'                  TO WS-CERT-ADDRESS-SW
01988      ELSE
01989         MOVE ' '                  TO WS-CERT-ADDRESS-SW.
01990
01991      IF  CERT-NOTES-ARE-NOT-PRESENT
01992          MOVE ' '                TO WS-CERT-NOTE-SW
01993      ELSE
01994          MOVE 'Y'                TO WS-CERT-NOTE-SW.
01995
01996      IF PB-CANCELLATION
01997         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES
01998            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2
01999            MOVE ZERO                  TO CM-LF-ITD-CANCEL-AMT
02000            MOVE LOW-VALUES            TO CM-LF-CANCEL-EXIT-DT
02001                                          CM-LF-CANCEL-DT
02002            MOVE SPACE                 TO CM-LF-EXIT-BATCH
02003         ELSE
02004            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2
02005            MOVE PB-CI-LF-CANCEL-AMT   TO CM-LF-ITD-CANCEL-AMT
02006            MOVE PB-CI-LF-PRIOR-CANCEL-DT TO CM-LF-CANCEL-DT
02007            MOVE PB-CI-LIVES              TO CM-LIVES.
02008
02009      IF PB-CANCELLATION
02010         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES
02011            IF CM-LF-STATUS-AT-CANCEL NOT EQUAL SPACE
02012                MOVE CM-LF-STATUS-AT-CANCEL
02013                                         TO CM-LF-CURRENT-STATUS
02014                MOVE SPACE
02015                                         TO CM-LF-STATUS-AT-CANCEL
02016             ELSE
02017                MOVE PB-CI-LF-POLICY-STATUS
02018                                         TO CM-LF-CURRENT-STATUS
02019         ELSE
02020             MOVE PB-CI-LF-POLICY-STATUS TO CM-LF-CURRENT-STATUS.
02021
02022      IF PB-CANCELLATION
02023         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES
02024            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2
02025            MOVE ZERO                  TO CM-AH-ITD-CANCEL-AMT
02026            MOVE LOW-VALUES            TO CM-AH-CANCEL-EXIT-DT
02027                                          CM-AH-CANCEL-DT
02028            MOVE SPACE                 TO CM-AH-EXIT-BATCH
02029         ELSE
02030            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2
02031            MOVE PB-CI-AH-CANCEL-AMT   TO CM-AH-ITD-CANCEL-AMT
02032            MOVE PB-CI-AH-PRIOR-CANCEL-DT TO CM-AH-CANCEL-DT
02033            MOVE PB-CI-LIVES           TO CM-LIVES.
02034
02035      IF PB-CANCELLATION
02036         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES
02037            IF CM-AH-STATUS-AT-CANCEL NOT EQUAL SPACE
02038                MOVE CM-AH-STATUS-AT-CANCEL
02039                                         TO CM-AH-CURRENT-STATUS
02040                MOVE SPACE
02041                                         TO CM-AH-STATUS-AT-CANCEL
02042                GO TO 3010-REWRITE-CERT
02043             ELSE
02044                MOVE PB-CI-AH-POLICY-STATUS
02045                                         TO CM-AH-CURRENT-STATUS
02046                GO TO 3010-REWRITE-CERT
02047         ELSE
02048             MOVE PB-CI-AH-POLICY-STATUS TO CM-AH-CURRENT-STATUS
02049             GO TO 3010-REWRITE-CERT.
02050
02051 ******************************************************************
02052 ******************************************************************
02053 ******* THIS CODE SHOULD BYPASS SETTING THE CLAIM-INTERFACE-SW  **
02054 ******* IF THE CERTIFICATE HAS ALREADY GONE THROUGH A MONTH END **
02055 ******************************************************************
02056 ******************************************************************
02057
02058      IF CERT-ADDED-BATCH AND
02059         (NOT NO-CLAIM-ATTACHED
02060          OR
02061          CM-CLAIM-ATTACHED-COUNT GREATER THAN +0)
02062         GO TO 3010-REWRITE-CERT.
02063
02064 ******************************************************************
02065 ******************************************************************
02066
02067      IF CERT-ADDED-BATCH  AND
02068         (NO-CLAIM-ATTACHED OR CM-CLAIM-ATTACHED-COUNT = ZEROS)
02069         GO TO 3010-REWRITE-CERT.
02070
02071      IF NO-CLAIM-ATTACHED
02072          NEXT SENTENCE
02073        ELSE
02074          MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-1
02075          MOVE '2'                    TO CM-CLAIM-INTERFACE-SW
02076          GO TO 3010-REWRITE-CERT.
02077
02078         
      * EXEC CICS DELETE
02079 *            DATASET    (ELCERT-FILE-ID)
02080 *       END-EXEC.
      *    MOVE '&(                    &   #00007337' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02081
02082 ******************************************************************
02083 *       DELETE CERTIFICATE NOTES FOR ALL DELETED CERTIFICATES    *
02084 ******************************************************************
02085
02086      IF CERT-NOTES-ARE-PRESENT
02087         NEXT SENTENCE
02088      ELSE
02089         GO TO 3007-DELETE-MAILING-DATA.
02090
111109     
      * EXEC CICS HANDLE CONDITION
111109*         NOTFND  (3006-DELETE-ERNOTE)
111109*    END-EXEC.
      *    MOVE '"$I                   ! 3 #00007350' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303037333530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
111109
111109     MOVE ELCERT-KEY            TO ERCNOT-KEY.
111109     MOVE '0'                   TO ERCNOT-REC-TYPE.
111109     MOVE 0                     TO ERCNOT-SEQUENCE.
111109
111109 3006-DELETE-ERCNOT-LOOP.
111109
111109     
      * EXEC CICS READ
111109*        GTEQ
111109*        DATASET   (ERCNOT-FILE-ID)
111109*        SET       (ADDRESS OF CERT-NOTE-FILE)
111109*        RIDFLD    (ERCNOT-KEY)
111109*    END-EXEC.
      *    MOVE '&"S        G          (   #00007360' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCNOT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCNOT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERT-NOTE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
111109
111109     MOVE CZ-CONTROL-PRIMARY      TO ERCNOT-KEY
111109     IF ERCNOT-PART-KEY NOT EQUAL ELCERT-KEY
111109         GO TO 3006-DELETE-ERNOTE
111109     END-IF.
111109
111109     
      * EXEC CICS DELETE
111109*        DATASET   (ERCNOT-FILE-ID)
111109*        RIDFLD    (ERCNOT-KEY)
111109*        END-EXEC.
      *    MOVE '&(  R                 &   #00007372' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCNOT-FILE-ID, 
                 ERCNOT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
111109
111109     GO TO 3006-DELETE-ERCNOT-LOOP.
111109
111109 3006-DELETE-ERNOTE.
111109
041320     move elcert-key             to ernote-key
041320     move '1'                    to ernote-rec-type
041320
041320     
      * exec cics delete
041320*       dataset    (ernote-file-id)
041320*       keylength  (ernote-generic-key-len)
041320*       ridfld     (ernote-key(1:33))
041320*       generic
041320*       resp       (w-response)
041320*    end-exec
      *    MOVE '&(  RKG               &  N#00007384' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303037333834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ernote-file-id, 
                 ernote-key(1 : 33), 
                 ernote-generic-key-len, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
02107 ******************************************************************
02108 *     DELETE MAILING ADDRESS RECORDS FOR DELETED CERTIFICATES    *
02109 ******************************************************************
02110
02111  3007-DELETE-MAILING-DATA.
02112      IF CERT-ADDRESS-PRESENT
02113         NEXT SENTENCE
02114      ELSE
02115         GO TO 3020-CONTINUE-DELETE.
02116
02117      
      * EXEC CICS HANDLE CONDITION
02118 *         NOTOPEN  (3020-CONTINUE-DELETE)
02119 *         NOTFND   (3020-CONTINUE-DELETE)
02120 *    END-EXEC.
      *    MOVE '"$JI                  ! 4 #00007402' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303037343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02121
02122      
      * EXEC CICS READ
02123 *        EQUAL
02124 *        DATASET   (ERMAIL-FILE-ID)
02125 *        SET       (ADDRESS OF MAILING-DATA)
02126 *        RIDFLD    (ELCERT-KEY)
02127 *        UPDATE
02128 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007407' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02129
02130      
      * EXEC CICS DELETE
02131 *        DATASET   (ERMAIL-FILE-ID)
02132 *    END-EXEC.
      *    MOVE '&(                    &   #00007415' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02133
02134      GO TO 3020-CONTINUE-DELETE.
02135
02136  3010-REWRITE-CERT.
02137
02138      
      * EXEC CICS REWRITE
02139 *        DATASET    (ELCERT-FILE-ID)
02140 *        FROM       (CERTIFICATE-MASTER)
02141 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007423' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02142
02143  3020-CONTINUE-DELETE.
02144
02145      
      * EXEC CICS ENDBR
02146 *        DATASET(ERPNDB-FILE-ID)
02147 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007430' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02148
02149      
      * EXEC CICS DELETE
02150 *        DATASET(ERPNDB-FILE-ID)
02151 *        RIDFLD (ERPNDB-KEY)
02152 *    END-EXEC.
      *    MOVE '&(  R                 &   #00007434' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02153
02154      ADD 1   TO WS-DELETE-CNT.
02155      GO TO 3000-GET-NEXT-RECORD.
02156
02157  3100-STOP-BROWSE.
02158      
      * EXEC CICS ENDBR
02159 *        DATASET(ERPNDB-FILE-ID)
02160 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007443' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02161
02162  3120-END-ROUTINE.
02163      MOVE SPACE                  TO PI-UPDATE-SW.
02164
02165      IF WS-DELETE-CNT NOT GREATER ZERO
02166          GO TO 3200-NO-RECORDS.
02167
02168      IF PI-AR-PROCESSING
02169         MOVE 'Y'                 TO WS-RECORD-FOUND-SW
02170         PERFORM 7000-VERIFY-REQUEST-REC THRU 7090-EXIT
02171         IF RECORD-NOT-FOUND
02172            GO TO 8200-SEND-DATAONLY.
02173
020816     IF PI-SAVE-CALLING-PGM NOT = XCTL-6301 and 'VP6301'
02175          MOVE ZEROS              TO EMI-ERROR
02176          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02177
02178      MOVE SPACE                  TO PI-BROWSE-SW.
02179      MOVE LOW-VALUES             TO EL630AO.
02180      MOVE QUESTION-MARKS         TO BATCHI.
02181      MOVE AL-UANON               TO BATCHA.
02182      MOVE -1                     TO MAINTL.
02183      GO TO 8100-SEND-INITIAL-MAP.
02184
02185  3200-NO-RECORDS.
02186      MOVE ER-2242                TO EMI-ERROR.
02187      MOVE -1 TO BATCHL
02188      MOVE AL-UABON               TO BATCHA.
02189      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02190      GO TO 8200-SEND-DATAONLY.
02191
02192  3300-FIRST-PF2.
02193      MOVE SPACE                  TO PI-CLEAR-ERROR-SW.
02194      MOVE ER-2422                TO EMI-ERROR.
02195      MOVE -1                     TO PFENTERL.
02196      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02197      GO TO 8100-SEND-INITIAL-MAP.
02198      EJECT
02199
02200  4000-SHOW-TOTALS.
02201
02202      MOVE LOW-VALUES             TO EL630AI.
02203      MOVE PI-SAV-ENTRY-BATCH     TO BATCHO.
02204      MOVE PI-SAV-REFERENCE       TO REFO.
02205
02206      IF PI-DATA-UPDATED
02207         MOVE 'C'                TO PI-MAINT-FUNC.
02208
02209      MOVE PI-MAINT-FUNC          TO MAINTO.
02210
02211      IF PI-SAV-CARRIER NOT = SPACES
02212          MOVE PI-SAV-CARRIER     TO CARRIERO.
02213
02214      IF PI-SAV-GROUPING NOT = SPACES
02215          MOVE PI-SAV-GROUPING    TO GROUPO.
02216
02217      IF PI-SAV-STATE NOT = SPACES
02218          MOVE PI-SAV-STATE       TO STATEO.
02219
02220      MOVE PI-SAV-ACCOUNT         TO ACCOUNTO.
02221
02222      IF PI-DATA-UPDATED
02223          MOVE AL-SANON           TO BATCHA
02224                                     MAINTA
02225                                     CARRIERA
02226                                     GROUPA
02227                                     STATEA
02228                                     ACCOUNTA
02229      ELSE
02230          MOVE AL-UANON           TO BATCHA
02231                                     MAINTA
02232                                     CARRIERA
02233                                     GROUPA
02234                                     STATEA
02235                                     ACCOUNTA.
02236
02237      MOVE PI-LF-ISS-ENTERED      TO ALFISSO.
02238      IF PI-LF-ISS-REMITTED NOT = ZEROS
02239          MOVE PI-LF-ISS-REMITTED TO ELFISSO
02240          MOVE AL-UNNON           TO ELFISSA
02241          COMPUTE WS-OBAL = PI-LF-ISS-REMITTED - PI-LF-ISS-ENTERED
02242          IF WS-OBAL NOT = ZEROS
02243              MOVE WS-OBAL        TO OLFISSO.
02244
02245      MOVE PI-LF-CAN-ENTERED      TO ALFCANO.
02246      IF PI-LF-CAN-REMITTED NOT = ZEROS
02247          MOVE PI-LF-CAN-REMITTED TO ELFCANO
02248          MOVE AL-UNNON           TO ELFCANA
02249          COMPUTE WS-OBAL = PI-LF-CAN-REMITTED - PI-LF-CAN-ENTERED
02250          IF WS-OBAL NOT = ZEROS
02251              MOVE WS-OBAL        TO OLFCANO.
02252
02253      MOVE PI-AH-ISS-ENTERED      TO AAHISSO.
02254      IF PI-AH-ISS-REMITTED NOT = ZEROS
02255          MOVE PI-AH-ISS-REMITTED TO EAHISSO
02256          MOVE AL-UNNON           TO EAHISSA
02257          COMPUTE WS-OBAL = PI-AH-ISS-REMITTED - PI-AH-ISS-ENTERED
02258          IF WS-OBAL NOT = ZEROS
02259              MOVE WS-OBAL        TO OAHISSO.
02260
02261      MOVE PI-AH-CAN-ENTERED      TO AAHCANO.
02262      IF PI-AH-CAN-REMITTED NOT = ZEROS
02263          MOVE PI-AH-CAN-REMITTED TO EAHCANO
02264          MOVE AL-UNNON           TO EAHCANA
02265          COMPUTE WS-OBAL = PI-AH-CAN-REMITTED - PI-AH-CAN-ENTERED
02266          IF WS-OBAL NOT = ZEROS
02267              MOVE WS-OBAL        TO OAHCANO.
02268
02269      MOVE PI-ISS-CNT-ENTERED     TO AISSCNTO.
02270      IF PI-ISS-CNT-REMITTED NOT = ZEROS
02271          MOVE PI-ISS-CNT-REMITTED TO EISSCNTO
02272          MOVE AL-UNNON            TO EISSCNTA
02273          COMPUTE WS-OCNT = PI-ISS-CNT-REMITTED -
02274                                 PI-ISS-CNT-ENTERED
02275          IF WS-OCNT NOT = ZEROS
02276              MOVE WS-OCNT        TO OISSCNTO.
02277
02278      MOVE PI-CAN-CNT-ENTERED     TO ACANCNTO.
02279      IF PI-CAN-CNT-REMITTED NOT = ZEROS
02280          MOVE PI-CAN-CNT-REMITTED TO ECANCNTO
02281          MOVE AL-UNNON            TO ECANCNTA
02282          COMPUTE WS-OCNT = PI-CAN-CNT-REMITTED -
02283                                 PI-CAN-CNT-ENTERED
02284          IF WS-OCNT NOT = ZEROS
02285              MOVE WS-OCNT        TO OCANCNTO.
02286
02287      IF  PI-NB-MONTH-END-DT GREATER SPACES
02288          MOVE PI-NB-MONTH-END-DT TO DC-BIN-DATE-1
02289        ELSE
02290          MOVE PI-CR-MONTH-END-DT TO DC-BIN-DATE-1.
02291
02292      MOVE  SPACE                 TO  DC-OPTION-CODE.
02293      PERFORM  9700-DATE-LINK.
02294
02295      IF  DATE-CONVERSION-ERROR
02296          MOVE LOW-VALUES TO MNTHNDTO
02297          GO TO 0490-EXIT.
02298
02299      MOVE DC-GREG-DATE-1-EDIT    TO  MNTHNDTO.
02300      MOVE AL-UANON               TO  MNTHNDTA.
02301
02302      IF  PI-NB-MONTH-END-DT GREATER SPACES
02303          MOVE PI-NB-MONTH-END-DT TO DC-BIN-DATE-1
02304        ELSE
02305          MOVE PI-CR-MONTH-END-DT TO DC-BIN-DATE-1.
02306
02307      MOVE  SPACE                 TO  DC-OPTION-CODE.
02308      PERFORM  9700-DATE-LINK.
02309
02310      IF  DATE-CONVERSION-ERROR
02311          MOVE LOW-VALUES TO MNTHNDTO
02312      ELSE
02313          MOVE DC-GREG-DATE-1-EDIT    TO  MNTHNDTO
02314          MOVE AL-UANON               TO  MNTHNDTA.
02315
02316      MOVE PI-RECEIVED-DT         TO DC-BIN-DATE-1.
02317
02318      MOVE  SPACE                 TO  DC-OPTION-CODE.
02319      PERFORM  9700-DATE-LINK.
02320
02321      IF  DATE-CONVERSION-ERROR
02322          MOVE LOW-VALUES TO RECEVDTO
02323      ELSE
02324          MOVE DC-GREG-DATE-1-EDIT    TO  RECEVDTO
02325          MOVE AL-UANON               TO  RECEVDTA.
02326
02327      GO TO 8100-SEND-INITIAL-MAP.
02328      EJECT
02329
02330 *****************************************************************
02331 *            U P D A T E   E N T I R E   B A T C H              *
02332 *  THIS SECTION UPDATES AN ENTIRE BATCH WITH A NEW MONTH END    *
02333 *  DATE.                                                        *
02334 *****************************************************************
02335
02336  5000-UPDATE-ENTIRE-BATCH.
02337
02338      MOVE ZEROS                  TO  PNDB-BATCH-SEQ
02339                                      PNDB-BATCH-CHG-SEQ.
02340
02341      
      * EXEC CICS HANDLE CONDITION
02342 *        NOTFND(5900-EXIT)
02343 *    END-EXEC.
      *    MOVE '"$I                   ! 5 #00007626' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303037363236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02344
02345      
      * EXEC CICS STARTBR
02346 *        DATASET(ERPNDB-FILE-ID)
02347 *        RIDFLD (ERPNDB-KEY)
02348 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007630' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02349
02350  5010-GET-NEXT-RECORD.
02351      
      * EXEC CICS HANDLE CONDITION
02352 *        NOTFND (5100-STOP-BROWSE)
02353 *        ENDFILE(5100-STOP-BROWSE)
02354 *    END-EXEC.
      *    MOVE '"$I''                  ! 6 #00007636' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303037363336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02355
02356  5020-READ-NEXT-RECORD.
02357      
      * EXEC CICS READNEXT
02358 *        SET    (ADDRESS OF PENDING-BUSINESS)
02359 *        DATASET(ERPNDB-FILE-ID)
02360 *        RIDFLD (ERPNDB-KEY)
02361 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007642' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02362
02363      IF WS-ON1-SW4 IS EQUAL TO 'Y'
02364          MOVE 'N'                    TO  WS-ON1-SW4
02365          MOVE PB-CONTROL-PRIMARY     TO  WS-SAV-PNDB-KEY
02366          GO TO 5030-READ-FOR-UPDATE.
02367
02368      IF ERPNDB-KEY = WS-SAV-PNDB-KEY
02369         GO TO 5020-READ-NEXT-RECORD.
02370
02371      IF PB-COMPANY-CD  = WS-SAV-COMP-CD  AND
02372         PB-ENTRY-BATCH = WS-SAV-ENTRY-BATCH
02373          NEXT SENTENCE
02374      ELSE
02375          GO TO 5100-STOP-BROWSE.
02376
02377      IF PB-ISSUE
02378         IF PB-I-REFERENCE NOT = PI-SAV-REFERENCE
02379            GO TO 5030-READ-FOR-UPDATE.
02380
02381      IF PB-CANCELLATION
02382         IF PB-C-REFERENCE NOT = PI-SAV-REFERENCE
02383            GO TO 5030-READ-FOR-UPDATE.
02384
02385      IF PB-CREDIT-SELECT-DT = PI-NB-MONTH-END-DT
02386          GO TO 5010-GET-NEXT-RECORD.
02387
02388  5030-READ-FOR-UPDATE.
02389      
      * EXEC CICS ENDBR
02390 *        DATASET(ERPNDB-FILE-ID)
02391 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007674' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02392
02393      
      * EXEC CICS READ
02394 *        SET     (ADDRESS OF PENDING-BUSINESS)
02395 *        DATASET (ERPNDB-FILE-ID)
02396 *        RIDFLD  (ERPNDB-KEY)
02397 *        UPDATE
02398 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007678' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02399
02400      MOVE ERPNDB-KEY                  TO WS-SAV-PNDB-KEY.
02401
02402      IF PI-NB-MONTH-END-DT GREATER THAN SPACES
02403         MOVE  PI-NB-MONTH-END-DT TO PB-CREDIT-SELECT-DT.
02404
02405      IF PB-ISSUE
02406         IF PB-I-REFERENCE NOT = PI-SAV-REFERENCE
02407            MOVE PI-SAV-REFERENCE TO PB-I-REFERENCE.
02408
02409      IF PB-CANCELLATION
02410         IF PB-C-REFERENCE NOT = PI-SAV-REFERENCE
02411            MOVE PI-SAV-REFERENCE TO PB-C-REFERENCE.
02412
02413  5050-REWRITE-PENDING-BUS.
02414       
      * EXEC CICS REWRITE
02415 *         DATASET    (ERPNDB-FILE-ID)
02416 *         FROM       (PENDING-BUSINESS)
02417 *     END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007699' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02418
02419      
      * EXEC CICS STARTBR
02420 *        DATASET(ERPNDB-FILE-ID)
02421 *        RIDFLD (ERPNDB-KEY)
02422 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007704' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02423
02424      GO TO 5010-GET-NEXT-RECORD.
02425
02426  5100-STOP-BROWSE.
02427      
      * EXEC CICS ENDBR
02428 *        DATASET(ERPNDB-FILE-ID)
02429 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007712' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02430
02431  5900-EXIT.
02432       EXIT.
02433
02434      EJECT
02435
02436 ******************************************************************
02437 *          R E W R I T E   E N T I R E   B A T C H              *
02438 *  THIS SECTION REWRITES AN ENTIRE BATCH WITH NEW ACCOUNT INFO. *
02439 *  1.  DELETE THE PENDING BUSINESS RECORD                       *
02440 *  2.  IF THE PENDING RECORD IS AN ISSUE, AND THE CERTIFICATE   *
02441 *      HAS NOT BEEN ADDED TO BATCH OR A CLAIM IS NOT ATTACHED,  *
02442 *      DELETE THE CERTIFICATE,CERTIFICATE NOTES, AND MAILING    *
02443 *      RECORDS                                                  *
02444 *  3.  IF THE PENDING RECORD HAS BEEN ADDED TO BATCH OR A CLAIM *
02445 *      IS ATTACHED MOVE SPACE TO CM-CREDIT-INTERFACE-SW-1 AND   *
02446 *      REWRITE THE CERTIFICATE.                                 *
02447 *  4.  IF THE PENDING RECORD IS CANCELLATION, BACKOUT THE       *
02448 *      CANCELLATION INFORMATION IN THE CERTIFICATE RECORD       *
02449 *      AND REWRITE THE CERTIFICATE.                             *
02450 *  5.  WRITE A NEW PENDING BUSINESS RECORD WITH THE NEW ACCOUNT *
02451 *      INFORMATION.                                             *
02452 *  6.  AFTER EACH DETAIL RECORD IS PROCESSED REWRITE THE BATCH  *
02453 *      RECORD WITH THE NEW ACCOUNT INFORMATION.                 *
02454 *  7.  ONCE THE BATCH IS REWRITTEN FORCE THE OPERATOR TO        *
02455 *      RE-EDITED THE BATCH.                                     *
02456 *  NOTE:  THE PENDING BUSINESS RECORDS ARE DELETED AND ADDED    *
02457 *         TO RESET THE ALTERNATE INDEX.                         *
02458 ******************************************************************
02459  6000-REWRITE-ENTIRE-BATCH.
02460      IF  NOT EMI-NO-ERRORS
02461          GO TO 8200-SEND-DATAONLY.
02462
02463      PERFORM 0400-PRIME-BATCH-TOTALS  THRU  0490-EXIT.
02464
02465      IF  EMI-ERROR = ZEROS
02466          MOVE 'N'                TO  PI-EL630-FIRST-TIME-SW
02467        ELSE
02468          GO TO 8200-SEND-DATAONLY.
02469
02470      MOVE PI-SAV-COMP-CD         TO  PNDB-COMP-CD.
02471      MOVE PI-SAV-ENTRY-BATCH     TO  PNDB-ENTRY-BATCH.
02472
02473      MOVE ZEROS                  TO  PNDB-BATCH-SEQ
02474                                      PNDB-BATCH-CHG-SEQ.
02475
02476      
      * EXEC CICS GETMAIN
02477 *        SET    (ADDRESS OF PENDING-BUSINESS)
02478 *        LENGTH (585)
02479 *        INITIMG(GETMAIN-SPACE)
02480 *    END-EXEC.
           MOVE 585
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007761' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02481
02482      
      * EXEC CICS HANDLE CONDITION
02483 *        NOTFND(6500-NO-RECORDS)
02484 *    END-EXEC.
      *    MOVE '"$I                   ! 7 #00007767' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303037373637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02485
02486  6010-GET-NEXT-RECORD.
02487      IF WS-ON1-SW5 IS EQUAL TO 'Y'
02488          MOVE 'N'                TO  WS-ON1-SW5
02489          GO TO 6015-START-BROWSE.
02490
02491      
      * EXEC CICS HANDLE CONDITION
02492 *        NOTFND (6400-STOP-BROWSE)
02493 *        ENDFILE(6400-STOP-BROWSE)
02494 *    END-EXEC.
      *    MOVE '"$I''                  ! 8 #00007776' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303037373736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02495
02496  6015-START-BROWSE.
02497      
      * EXEC CICS STARTBR
02498 *        DATASET(ERPNDB-FILE-ID)
02499 *        RIDFLD (ERPNDB-KEY)
02500 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007782' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02501
02502  6020-READ-NEXT-RECORD.
02503      
      * EXEC CICS READNEXT
02504 *        INTO   (PENDING-BUSINESS)
02505 *        DATASET(ERPNDB-FILE-ID)
02506 *        RIDFLD (ERPNDB-KEY)
02507 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00007788' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV12, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02508
02509      IF WS-ON1-SW6 IS EQUAL TO 'Y'
02510          MOVE 'N'                    TO WS-ON1-SW6
02511          MOVE PB-CONTROL-PRIMARY     TO WS-SAV-PNDB-KEY
02512          GO TO 6030-READ-FOR-UPDATE.
02513
02514      IF ERPNDB-KEY = WS-SAV-PNDB-KEY
02515          GO TO 6020-READ-NEXT-RECORD.
02516
02517      IF PB-COMPANY-CD  = WS-SAV-COMP-CD  AND
02518         PB-ENTRY-BATCH = WS-SAV-ENTRY-BATCH
02519          NEXT SENTENCE
02520      ELSE
02521          GO TO 6400-STOP-BROWSE.
02522
02523  6030-READ-FOR-UPDATE.
02524      
      * EXEC CICS ENDBR
02525 *        DATASET(ERPNDB-FILE-ID)
02526 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007809' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02527
02528      
      * EXEC CICS READ
02529 *        INTO (PENDING-BUSINESS)
02530 *        DATASET (ERPNDB-FILE-ID)
02531 *        RIDFLD  (ERPNDB-KEY)
02532 *        UPDATE
02533 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00007813' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02534
02535      MOVE ERPNDB-KEY                  TO WS-SAV-PNDB-KEY.
02536
02537      MOVE PENDING-BUSINESS       TO WS-ERPNDB-RECORD.
02538
02539  6050-DELETE-PENDING-BUS.
02540      
      * EXEC CICS DELETE
02541 *        DATASET    (ERPNDB-FILE-ID)
02542 *    END-EXEC.
      *    MOVE '&(                    &   #00007825' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02543
02544      MOVE WS-ERPNDB-RECORD       TO PENDING-BUSINESS.
02545
02546      IF  PB-BATCH-TRAILER
02547          GO TO 6300-WRITE-PNDB-RECORD.
02548
02549      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.
02550      IF  PB-SV-CARRIER GREATER SPACES
02551          MOVE PB-SV-CARRIER          TO CERT-CARRIER.
02552      IF  PB-SV-GROUPING GREATER SPACES
02553          MOVE PB-SV-GROUPING         TO CERT-GROUPING.
02554      IF  PB-SV-STATE GREATER SPACES
02555          MOVE PB-SV-STATE            TO CERT-STATE.
02556
02557      IF PB-ISSUE
02558          NEXT SENTENCE
02559        ELSE
02560          GO TO 6200-PROCESS-CANCELS.
02561
02562      
      * EXEC CICS HANDLE CONDITION
02563 *         NOTFND  (6300-WRITE-PNDB-RECORD)
02564 *    END-EXEC.
      *    MOVE '"$I                   ! 9 #00007847' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303037383437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02565
02566      
      * EXEC CICS READ
02567 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
02568 *        DATASET (ELCERT-FILE-ID)
02569 *        RIDFLD  (ELCERT-KEY)
02570 *        UPDATE
02571 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007851' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383531' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02572
02573      IF INSURED-ADDR-PRESENT
02574         MOVE 'Y'                  TO WS-CERT-ADDRESS-SW
02575      ELSE
02576         MOVE ' '                  TO WS-CERT-ADDRESS-SW.
02577
02578      IF  CERT-NOTES-ARE-NOT-PRESENT
02579          MOVE ' '                TO WS-CERT-NOTE-SW
02580      ELSE
02581          MOVE 'Y'                TO WS-CERT-NOTE-SW.
02582
02583      IF  CERT-ADDED-BATCH
02584          
      * EXEC CICS UNLOCK
02585 *             DATASET (ELCERT-FILE-ID)
02586 *        END-EXEC
      *    MOVE '&*                    #   #00007869' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02587          GO TO 6300-WRITE-PNDB-RECORD.
02588
02589      IF  CM-CLAIM-ATTACHED-COUNT GREATER +0
02590          NEXT SENTENCE
02591         ELSE
02592          GO TO 6060-DELETE-CERTIFICATE.
02593
02594      MOVE SPACE                 TO  CM-CREDIT-INTERFACE-SW-1.
02595      MOVE '2'                   TO  CM-CLAIM-INTERFACE-SW.
02596
02597      
      * EXEC CICS REWRITE
02598 *         DATASET    (ELCERT-FILE-ID)
02599 *         FROM       (CERTIFICATE-MASTER)
02600 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007882' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02601
02602      GO TO 6300-WRITE-PNDB-RECORD.
02603
02604  6060-DELETE-CERTIFICATE.
02605
02606      
      * EXEC CICS DELETE
02607 *         DATASET    (ELCERT-FILE-ID)
02608 *    END-EXEC.
      *    MOVE '&(                    &   #00007891' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02609
02610 ******************************************************************
02611 *       DELETE CERTIFICATE NOTES FOR ALL DELETED CERTIFICATES    *
02612 ******************************************************************
02613
02614      IF CERT-NOTES-ARE-PRESENT
02615         NEXT SENTENCE
02616      ELSE
02617         GO TO 6070-DELETE-MAILING.
02618
02619      
      * EXEC CICS HANDLE CONDITION
02620 *         NOTFND  (6070-DELETE-MAILING)
02621 *    END-EXEC.
      *    MOVE '"$I                   ! : #00007904' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303037393034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02622
02623      
      * EXEC CICS READ
02624 *        EQUAL
02625 *        DATASET   (ERNOTE-FILE-ID)
02626 *        SET       (ADDRESS OF CERTIFICATE-NOTE)
02627 *        RIDFLD    (ELCERT-KEY)
02628 *        UPDATE
02629 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007908' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02630
02631      
      * EXEC CICS DELETE
02632 *        DATASET   (ERNOTE-FILE-ID)
02633 *    END-EXEC.
      *    MOVE '&(                    &   #00007916' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02634
02635 ******************************************************************
02636 *     DELETE MAILING ADDRESS RECORDS FOR DELETED CERTIFICATES    *
02637 ******************************************************************
02638
02639  6070-DELETE-MAILING.
02640      IF CERT-ADDRESS-PRESENT
02641         NEXT SENTENCE
02642      ELSE
02643         GO TO 6300-WRITE-PNDB-RECORD.
02644
02645      
      * EXEC CICS HANDLE CONDITION
02646 *         NOTOPEN (6300-WRITE-PNDB-RECORD)
02647 *         NOTFND  (6300-WRITE-PNDB-RECORD)
02648 *    END-EXEC.
      *    MOVE '"$JI                  ! ; #00007930' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303037393330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02649
02650      
      * EXEC CICS READ
02651 *        EQUAL
02652 *        DATASET   (ERMAIL-FILE-ID)
02653 *        SET       (ADDRESS OF MAILING-DATA)
02654 *        RIDFLD    (ELCERT-KEY)
02655 *        UPDATE
02656 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007935' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02657
02658      
      * EXEC CICS DELETE
02659 *        DATASET   (ERMAIL-FILE-ID)
02660 *    END-EXEC.
      *    MOVE '&(                    &   #00007943' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02661
02662      GO TO 6300-WRITE-PNDB-RECORD.
02663
02664  6200-PROCESS-CANCELS.
02665      
      * EXEC CICS HANDLE CONDITION
02666 *         NOTFND  (6300-WRITE-PNDB-RECORD)
02667 *    END-EXEC.
      *    MOVE '"$I                   ! < #00007950' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303037393530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02668
02669      
      * EXEC CICS READ
02670 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
02671 *        DATASET (ELCERT-FILE-ID)
02672 *        RIDFLD  (ELCERT-KEY)
02673 *        UPDATE
02674 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007954' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393534' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02675
02676      IF PB-CANCELLATION
02677         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES
02678            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
02679            MOVE ZERO                   TO CM-LF-ITD-CANCEL-AMT
02680            MOVE LOW-VALUES             TO CM-LF-CANCEL-EXIT-DT
02681                                           CM-LF-CANCEL-DT
02682         ELSE
02683            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
02684            MOVE PB-CI-LF-CANCEL-AMT    TO CM-LF-ITD-CANCEL-AMT
02685            MOVE PB-CI-LF-PRIOR-CANCEL-DT TO CM-LF-CANCEL-DT
02686            MOVE PB-CI-LIVES            TO CM-LIVES.
02687
02688      IF PB-CANCELLATION
02689         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES
02690            IF CM-LF-STATUS-AT-CANCEL NOT EQUAL SPACE
02691                MOVE CM-LF-STATUS-AT-CANCEL
02692                                         TO CM-LF-CURRENT-STATUS
02693                MOVE SPACE
02694                                         TO CM-LF-STATUS-AT-CANCEL
02695             ELSE
02696                MOVE PB-CI-LF-POLICY-STATUS
02697                                         TO CM-LF-CURRENT-STATUS
02698         ELSE
02699             MOVE PB-CI-LF-POLICY-STATUS TO CM-LF-CURRENT-STATUS.
02700
02701      IF PB-CANCELLATION
02702         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES
02703            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
02704            MOVE ZERO                   TO CM-AH-ITD-CANCEL-AMT
02705            MOVE LOW-VALUES             TO CM-AH-CANCEL-EXIT-DT
02706                                           CM-AH-CANCEL-DT
02707         ELSE
02708            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
02709            MOVE PB-CI-AH-CANCEL-AMT    TO CM-AH-ITD-CANCEL-AMT
02710            MOVE PB-CI-AH-PRIOR-CANCEL-DT TO CM-AH-CANCEL-DT
02711            MOVE PB-CI-LIVES            TO CM-LIVES.
02712
02713      IF PB-CANCELLATION
02714         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES
02715            IF CM-AH-STATUS-AT-CANCEL NOT EQUAL SPACE
02716                MOVE CM-AH-STATUS-AT-CANCEL
02717                                         TO CM-AH-CURRENT-STATUS
02718                MOVE SPACE
02719                                         TO CM-AH-STATUS-AT-CANCEL
02720             ELSE
02721                MOVE PB-CI-AH-POLICY-STATUS
02722                                         TO CM-AH-CURRENT-STATUS
02723         ELSE
02724             MOVE PB-CI-AH-POLICY-STATUS TO CM-AH-CURRENT-STATUS.
02725
02726  6255-REWRITE-CERT.
02727
02728      
      * EXEC CICS REWRITE
02729 *        DATASET    (ELCERT-FILE-ID)
02730 *        FROM       (CERTIFICATE-MASTER)
02731 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008013' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02732
02733  6300-WRITE-PNDB-RECORD.
02734
02735      MOVE 'X'                    TO  PB-FATAL-FLAG.
02736
02737      MOVE SPACES                 TO  PB-I-RATE-DEVIATION-LF
02738                                      PB-I-RATE-DEVIATION-AH
02739                                      PB-REIN-CD
02740                                      PB-SV-CARRIER
02741                                      PB-SV-GROUPING
02742                                      PB-SV-STATE.
02743
02744      IF PB-BATCH-TRAILER
02745          MOVE PI-AM-NAME         TO  PB-ACCOUNT-NAME
02746          MOVE PI-SAV-FC-CARRIER  TO  PB-SV-CARRIER
02747          MOVE PI-SAV-FC-GROUPING TO  PB-SV-GROUPING
02748          MOVE PI-SAV-FC-STATE    TO  PB-SV-STATE.
02749
02750      MOVE LOW-VALUES             TO  PB-ACCT-EFF-DT
02751                                      PB-ACCT-EXP-DT.
02752
02753      MOVE ZEROS                  TO  PB-CALC-TOLERANCE.
02754
02755      IF  CARRIERL GREATER ZEROS
02756          MOVE CARRIERI           TO PB-CARRIER
02757         ELSE
02758          MOVE SPACES             TO PB-CARRIER.
02759
02760      IF  GROUPL   GREATER ZEROS
02761          MOVE GROUPI             TO PB-GROUPING
02762         ELSE
02763          MOVE SPACES             TO PB-GROUPING.
02764
02765      IF  STATEL   GREATER ZEROS
02766          MOVE STATEI             TO PB-STATE
02767         ELSE
02768          MOVE SPACES             TO PB-STATE.
02769
02770      IF  ACCOUNTL GREATER ZEROS
02771          MOVE ACCOUNTI           TO PB-ACCOUNT
02772         ELSE
02773          MOVE SPACES             TO PB-ACCOUNT.
02774
02775      IF WS-ON1-SW7 IS EQUAL TO 'Y'
02776          MOVE 'N'                TO WS-ON1-SW7
02777          MOVE PB-GROUPING        TO PI-SAV-GROUPING
02778          MOVE PB-STATE           TO PI-SAV-STATE
02779          MOVE PB-ACCOUNT         TO PI-SAV-ACCOUNT.
02780
02781      
      * EXEC CICS WRITE
02782 *        DATASET(ERPNDB-FILE-ID)
02783 *        FROM   (PENDING-BUSINESS)
02784 *        RIDFLD (PB-CONTROL-PRIMARY)
02785 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008066' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02786
02787      GO TO 6010-GET-NEXT-RECORD.
02788
02789  6400-STOP-BROWSE.
02790      
      * EXEC CICS ENDBR
02791 *        DATASET(ERPNDB-FILE-ID)
02792 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008075' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02793
02794      MOVE 'Y'                    TO  PI-UPDATE-SW.
02795
02796      IF PI-AR-PROCESSING
02797         PERFORM 6600-REWRITE-REQUEST THRU 6690-EXIT.
02798
02799      MOVE ER-2990                TO EMI-ERROR.
02800      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02801      MOVE -1                     TO MAINTL.
02802      MOVE AL-SANON               TO MAINTA.
02803      MOVE PI-SAV-REFERENCE       TO REFO.
02804      GO TO 8200-SEND-DATAONLY.
02805
02806  6500-NO-RECORDS.
02807      MOVE ER-2242                TO EMI-ERROR.
02808      MOVE -1                     TO BATCHL.
02809      MOVE AL-UABON               TO BATCHA.
02810      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02811      GO TO 8200-SEND-DATAONLY.
02812
02813      EJECT
02814
02815 ******************************************************************
02816 *                                                                *
02817 *          R E W R I T E   R E Q U E S T   R E C O R D           *
02818 *                                                                *
02819 ******************************************************************
02820
02821  6600-REWRITE-REQUEST.
02822
02823      
      * EXEC CICS HANDLE CONDITION
02824 *        NOTFND (6690-EXIT)
02825 *    END-EXEC.
      *    MOVE '"$I                   ! = #00008108' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303038313038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02826
02827      MOVE PI-COMPANY-CD          TO  ERRQST-COMP-CD.
02828      MOVE PI-SAV-ENTRY-BATCH     TO  ERRQST-ENTRY-BATCH.
02829
02830      
      * EXEC CICS READ
02831 *         DATASET    (ERRQST-FILE-ID)
02832 *         SET        (ADDRESS OF AR-REQUEST-RECORD)
02833 *         RIDFLD     (ERRQST-KEY)
02834 *         UPDATE
02835 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008115' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02836
02837      IF  CARRIERL GREATER ZEROS
02838          MOVE CARRIERI           TO RQ-CARRIER-A1
02839                                     RQ-CARRIER-A2
02840                                     RQ-CARRIER-A2.
02841
02842      IF  GROUPL   GREATER ZEROS
02843          MOVE GROUPI             TO RQ-GROUPING-A1
02844                                     RQ-GROUPING-A2
02845                                     RQ-GROUPING-A3.
02846
02847      IF  STATEL   GREATER ZEROS
02848          MOVE STATEI             TO RQ-STATE-A1.
02849
02850      IF  ACCOUNTL GREATER ZEROS
02851          MOVE ACCOUNTI           TO RQ-ACCOUNT-A1
02852                                     RQ-ACCOUNT-A4.
02853
02854      IF PI-ZERO-CARRIER
02855         MOVE ZERO                TO RQ-CARRIER-A2.
02856
02857      IF PI-ZERO-GROUPING
02858         MOVE ZEROS               TO RQ-GROUPING-A2.
02859
02860      IF PI-ZERO-CAR-GROUP
02861         MOVE ZEROS               TO RQ-CARRIER-A2
02862                                     RQ-GROUPING-A2.
02863
02864      MOVE PI-REMIT-AGENT (PI-SUB) TO RQ-FIN-RESP-A2
02865                                      PI-FIN-RESP.
02866
02867      MOVE PI-ACCT-AGENT  (PI-SUB) TO RQ-ACCT-AGENT-A2
02868                                      PI-ACCOUNT-AGENT.
02869
02870      
      * EXEC CICS REWRITE
02871 *         DATASET    (ERRQST-FILE-ID)
02872 *         FROM       (AR-REQUEST-RECORD)
02873 *    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008155' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02874
02875  6690-EXIT.
02876      EXIT.
02877      EJECT
02878
02879 ******************************************************************
02880 *                                                                *
02881 *           V E R I F Y   R E Q U E S T   R E C O R D            *
02882 *                                                                *
02883 ******************************************************************
02884
02885  7000-VERIFY-REQUEST-REC.
02886      
      * EXEC CICS HANDLE CONDITION
02887 *        NOTFND(7090-EXIT)
02888 *    END-EXEC.
      *    MOVE '"$I                   ! > #00008171' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303038313731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02889
02890      MOVE PI-COMPANY-CD          TO ERRQST-COMP-CD.
02891      MOVE PI-SAV-ENTRY-BATCH     TO ERRQST-ENTRY-BATCH.
02892
02893      
      * EXEC CICS READ
02894 *        SET    (ADDRESS OF AR-REQUEST-RECORD)
02895 *        DATASET(ERRQST-FILE-ID)
02896 *        RIDFLD (ERRQST-KEY)
02897 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008178' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02898
02899      IF RQ-STMT-DT NOT = LOW-VALUES
02900         MOVE ER-2126        TO EMI-ERROR
02901         MOVE -1             TO BATCHL
02902         MOVE AL-UABON       TO BATCHA
02903         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
02904         MOVE 'N'            TO WS-RECORD-FOUND-SW
02905         GO TO 7090-EXIT.
02906
02907 ******************************************************************
02908 *                                                                *
02909 *   IF THE BATCH IS BEING DELETED (PF2), DELETE REQUEST RECORD.  *
02910 *                                                                *
02911 ******************************************************************
02912
02913      IF EIBAID = DFHPF2
02914         NEXT SENTENCE
02915      ELSE
02916         GO TO 7090-EXIT.
02917
02918      
      * EXEC CICS READ
02919 *        SET    (ADDRESS OF AR-REQUEST-RECORD)
02920 *        DATASET(ERRQST-FILE-ID)
02921 *        RIDFLD (ERRQST-KEY)
02922 *        UPDATE
02923 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008203' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02924
02925      
      * EXEC CICS DELETE
02926 *        DATASET(ERRQST-FILE-ID)
02927 *    END-EXEC.
      *    MOVE '&(                    &   #00008210' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02928
02929  7090-EXIT.
02930       EXIT.
02931
02932       EJECT
02933
02934  7100-REWRITE-REQUEST-REC.
02935
02936      
      * EXEC CICS HANDLE CONDITION
02937 *        NOTFND(7180-NO-REQUEST-REC)
02938 *    END-EXEC.
      *    MOVE '"$I                   ! ? #00008221' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3F20233030303038323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02939
02940
02941      MOVE PI-COMPANY-CD          TO ERRQST-COMP-CD.
02942      MOVE PI-SAV-ENTRY-BATCH     TO ERRQST-ENTRY-BATCH.
02943
02944      
      * EXEC CICS READ
02945 *        SET    (ADDRESS OF AR-REQUEST-RECORD)
02946 *        DATASET(ERRQST-FILE-ID)
02947 *        RIDFLD (ERRQST-KEY)
02948 *        UPDATE
02949 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008229' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02950
02951      IF RQ-STMT-DT NOT = LOW-VALUES
02952         MOVE ER-2126        TO EMI-ERROR
02953         MOVE -1             TO BATCHL
02954         MOVE AL-UABON       TO BATCHA
02955         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
02956         MOVE 'N'            TO WS-RECORD-FOUND-SW
02957         GO TO 7190-EXIT.
02958
02959      MOVE PI-SAV-REFERENCE  TO RQ-REFERENCE-A1
02960                                RQ-REFERENCE-A2
02961                                RQ-REFERENCE-A4.
02962
02963      
      * EXEC CICS REWRITE
02964 *        DATASET(ERRQST-FILE-ID)
02965 *        FROM   (AR-REQUEST-RECORD)
02966 *    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008248' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02967
02968      GO TO 7190-EXIT.
02969
02970  7180-NO-REQUEST-REC.
02971      MOVE ER-2132        TO EMI-ERROR.
02972      MOVE -1             TO BATCHL.
02973      MOVE AL-UABON       TO BATCHA.
02974      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
02975      MOVE 'N'            TO WS-RECORD-FOUND-SW.
02976      GO TO 7190-EXIT.
02977
02978  7190-EXIT.
02979      EXIT.
02980
02981      EJECT
02982
02983 ******************************************************************
02984 *
02985 *     7200-REWRITE IS USED TO REWRITE THE REQUEST FILE WHEN
02986 *     THE EOM DATE IS CHANGED.
02987 *
02988 ******************************************************************
02989
02990  7200-REWRITE-REQUEST.
02991      
      * EXEC CICS HANDLE CONDITION
02992 *        NOTFND(7280-NO-REQUEST-REC)
02993 *    END-EXEC.
      *    MOVE '"$I                   ! @ #00008276' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4020233030303038323736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02994
02995      MOVE PI-COMPANY-CD          TO ERRQST-COMP-CD.
02996      MOVE PI-SAV-ENTRY-BATCH     TO ERRQST-ENTRY-BATCH.
02997
02998      
      * EXEC CICS READ
02999 *        SET    (ADDRESS OF AR-REQUEST-RECORD)
03000 *        DATASET(ERRQST-FILE-ID)
03001 *        RIDFLD (ERRQST-KEY)
03002 *        UPDATE
03003 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008283' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03004
03005      IF RQ-STMT-DT NOT = LOW-VALUES
03006         MOVE ER-2126        TO EMI-ERROR
03007         MOVE -1             TO BATCHL
03008         MOVE AL-UABON       TO BATCHA
03009         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03010         MOVE 'N'            TO WS-RECORD-FOUND-SW
03011         GO TO 7299-EXIT.
03012
03013
03014      IF PI-NB-MONTH-END-DT NOT = RQ-MO-END-DT
03015         MOVE PI-NB-MONTH-END-DT  TO  RQ-MO-END-DT.
03016
03017      IF PI-SAV-REFERENCE NOT = RQ-REFERENCE-A1
03018         MOVE PI-SAV-REFERENCE    TO RQ-REFERENCE-A1
03019                                     RQ-REFERENCE-A2
03020                                     RQ-REFERENCE-A4.
03021
03022      
      * EXEC CICS REWRITE
03023 *        DATASET(ERRQST-FILE-ID)
03024 *        FROM   (AR-REQUEST-RECORD)
03025 *    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008307' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRQST-FILE-ID, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03026
03027      GO TO 7299-EXIT.
03028
03029  7280-NO-REQUEST-REC.
03030      MOVE ER-2132        TO EMI-ERROR.
03031      MOVE -1             TO BATCHL.
03032      MOVE AL-UABON       TO BATCHA.
03033      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03034      MOVE 'N'            TO WS-RECORD-FOUND-SW.
03035
03036  7299-EXIT.
03037      EXIT.
03038
03039      EJECT
03040
03041  8100-SEND-INITIAL-MAP.
03042      MOVE WS-CURRENT-DT          TO DATEO.
03043      MOVE EIBTIME                TO TIME-IN.
03044      MOVE TIME-OUT               TO TIMEO.
03045      MOVE -1                     TO MAINTL.
03046      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
03047      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
03048
03049      MOVE PI-LIFE-OVERRIDE-L2    TO WS-PRM-OVERRIDE
03050                                     WS-REFUND-OVERRIDE.
03051      MOVE WS-PRM-HEADER          TO LFPHDGO.
03052      MOVE WS-REFUND-HEADER       TO LFRHDGO.
03053
03054      MOVE PI-AH-OVERRIDE-L2      TO WS-PRM-OVERRIDE
03055                                     WS-REFUND-OVERRIDE.
03056      MOVE WS-PRM-HEADER          TO AHPHDGO.
03057      MOVE WS-REFUND-HEADER       TO AHRHDGO.
03058
03059      IF NOT PI-AR-PROCESSING
03060         MOVE AL-SADOF            TO REFHDGA
03061                                     REFA.
03062
03063      MOVE AL-SANOF               TO PF5A.
03064
03065      IF CARR-GROUP-ST-ACCNT-CNTL
03066          NEXT SENTENCE
03067      ELSE
03068          IF ST-ACCNT-CNTL
03069              MOVE AL-SADOF       TO CARRHDGA
03070                                     GRPHDGA
03071              MOVE AL-SANOF       TO CARRIERA
03072                                     GROUPA
03073          ELSE
03074              IF CARR-ST-ACCNT-CNTL
03075                  MOVE AL-SADOF   TO GRPHDGA
03076                  MOVE AL-SANOF   TO GROUPA
03077              ELSE
03078                  IF ACCNT-CNTL
03079                      MOVE AL-SADOF TO CARRHDGA
03080                                       GRPHDGA
03081                                       STHDGA
03082                      MOVE AL-SANOF TO CARRIERA
03083                                       GROUPA
03084                                       STATEA
03085                  ELSE
03086                      IF CARR-ACCNT-CNTL
03087                          MOVE AL-SADOF TO GRPHDGA
03088                                           STHDGA
03089                          MOVE AL-SANOF TO GROUPA
03090                                           STATEA.
03091      
      * EXEC CICS SEND
03092 *        MAP      (MAP-NAME)
03093 *        MAPSET   (MAPSET-NAME)
03094 *        FROM     (EL630AO)
03095 *        ERASE
03096 *        CURSOR
03097 *    END-EXEC.
           MOVE LENGTH OF
            EL630AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00008376' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL630AO, 
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
           
03098
03099      GO TO 9100-RETURN-TRAN.
03100      EJECT
03101
03102  8200-SEND-DATAONLY.
03103      MOVE WS-CURRENT-DT          TO DATEO.
03104      MOVE EIBTIME                TO TIME-IN.
03105      MOVE TIME-OUT               TO TIMEO.
03106      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
03107      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
03108      
      * EXEC CICS SEND
03109 *        MAP      (MAP-NAME)
03110 *        MAPSET   (MAPSET-NAME)
03111 *        FROM     (EL630AO)
03112 *        DATAONLY
03113 *        ERASEAUP
03114 *        CURSOR
03115 *    END-EXEC.
           MOVE LENGTH OF
            EL630AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00008393' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL630AO, 
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
           
03116
03117      GO TO 9100-RETURN-TRAN.
03118
03119  8300-SEND-TEXT.
03120      
      * EXEC CICS SEND TEXT
03121 *        FROM     (LOGOFF-TEXT)
03122 *        LENGTH   (LOGOFF-LENGTH)
03123 *        ERASE
03124 *        FREEKB
03125 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00008405' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343035' TO DFHEIV0(25:11)
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
           
03126
03127      
      * EXEC CICS RETURN
03128 *    END-EXEC.
      *    MOVE '.(                    ''   #00008412' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03129      EJECT
03130  8600-DEEDIT.
03131      
      * EXEC CICS BIF DEEDIT
03132 *        FIELD (WS-DEEDIT-FIELD)
03133 *        LENGTH(10)
03134 *     END-EXEC.
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008416' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03135
03136  8600-EXIT.
03137      EXIT.
03138      EJECT
03139  8800-UNAUTHORIZED-ACCESS.
03140      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
03141      GO TO 8300-SEND-TEXT.
03142
03143  8810-PF23.
03144      IF NOT PI-DATA-UPDATED
03145          NEXT SENTENCE
03146      ELSE
03147          MOVE -1                 TO PFENTERL
03148          MOVE ER-2213            TO EMI-ERROR
03149          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03150          GO TO 4000-SHOW-TOTALS.
03151
03152      MOVE EIBAID                 TO PI-ENTRY-CD-1.
03153      MOVE XCTL-005               TO PGM-NAME.
03154      GO TO 9300-XCTL.
03155
03156  9100-RETURN-TRAN.
03157      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
03158      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
03159      
      * EXEC CICS RETURN
03160 *        TRANSID    (TRANS-ID)
03161 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
03162 *        LENGTH     (WS-COMM-LENGTH)
03163 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00008444' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03164
03165      MOVE ZEROS  TO RETURN-CODE.
03165      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL630' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
03166
03167  9200-RETURN-MAIN-MENU.
03168      IF NOT PI-DATA-UPDATED
03169          NEXT SENTENCE
03170      ELSE
03171          MOVE -1                 TO PFENTERL
03172          MOVE ER-2213            TO EMI-ERROR
03173          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03174          GO TO 4000-SHOW-TOTALS.
03175
03176      MOVE XCTL-626               TO PGM-NAME.
03177      GO TO 9300-XCTL.
03178
03179  9300-XCTL.
03180      
      * EXEC CICS XCTL
03181 *        PROGRAM    (PGM-NAME)
03182 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
03183 *        LENGTH     (WS-COMM-LENGTH)
03184 *    END-EXEC.
      *    MOVE '.$C                   %   #00008466' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03185      MOVE ZEROS  TO RETURN-CODE.
03185      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL630' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
03186
03187  9400-CLEAR.
03188      IF NOT PI-DATA-UPDATED
03189          NEXT SENTENCE
03190      ELSE
03191          MOVE 'Y'                TO PI-CLEAR-ERROR-SW
03192          MOVE -1                 TO PFENTERL
03193          MOVE ER-2213            TO EMI-ERROR
03194          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03195          GO TO 4000-SHOW-TOTALS.
03196
03197      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
03198      GO TO 9300-XCTL.
03199
03200  9500-PF12.
03201      MOVE XCTL-010               TO PGM-NAME.
03202      GO TO 9300-XCTL.
03203
03204  9600-PGMID-ERROR.
03205      
      * EXEC CICS HANDLE CONDITION
03206 *        PGMIDERR    (8300-SEND-TEXT)
03207 *    END-EXEC.
      *    MOVE '"$L                   ! A #00008492' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4120233030303038343932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03208
03209      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
03210      MOVE ' '                    TO PI-ENTRY-CD-1.
03211      MOVE XCTL-005               TO PGM-NAME.
03212      MOVE PGM-NAME               TO LOGOFF-PGM.
03213      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
03214      GO TO 9300-XCTL.
03215
03216  9700-DATE-LINK.
03217      MOVE LINK-ELDATCV           TO PGM-NAME.
03218      
      * EXEC CICS LINK
03219 *        PROGRAM    (PGM-NAME)
03220 *        COMMAREA   (DATE-CONVERSION-DATA)
03221 *        LENGTH     (DC-COMM-LENGTH)
03222 *    END-EXEC.
      *    MOVE '."C                   (   #00008505' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03223
03224  9900-ERROR-FORMAT.
03225      IF NOT EMI-ERRORS-COMPLETE
03226          MOVE LINK-001           TO PGM-NAME
03227          
      * EXEC CICS LINK
03228 *            PROGRAM    (PGM-NAME)
03229 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
03230 *            LENGTH     (EMI-COMM-LENGTH)
03231 *        END-EXEC.
      *    MOVE '."C                   (   #00008514' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03232
03233  9900-EXIT.
03234      EXIT.
03235
03236  9990-ABEND.
03237      MOVE LINK-004               TO PGM-NAME.
03238      MOVE DFHEIBLK               TO EMI-LINE1.
03239      
      * EXEC CICS LINK
03240 *        PROGRAM   (PGM-NAME)
03241 *        COMMAREA  (EMI-LINE1)
03242 *        LENGTH    (72)
03243 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00008526' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03244
03245      MOVE -1                     TO PFENTERL.
03246      GO TO 8200-SEND-DATAONLY.
03247
03248  9995-SECURITY-VIOLATION.
03249 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00008553' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353533' TO DFHEIV0(25:11)
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
03250
03251  9995-EXIT.
03252      EXIT.
03253

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL630' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0350-NO-RECORDS,
                     0350-NO-RECORDS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0480-NO-BATCH-TRAILER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1000-NO-CARRIER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1100-NO-STATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1200-ACCOUNT-INVALID,
                     1200-ACCOUNT-INVALID
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1250-NO-COMPANY-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1300-NO-COMPANY-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 1300-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 1470-NO-COMP-MSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 1990-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 2100-ADD-BATCH-TOTAL-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 3200-NO-RECORDS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 3120-END-ROUTINE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 3100-STOP-BROWSE,
                     3100-STOP-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 3006-DELETE-CERTIFICATE,
                     3006-DELETE-CERTIFICATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 3020-CONTINUE-DELETE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 3006-DELETE-ERNOTE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 3020-CONTINUE-DELETE,
                     3020-CONTINUE-DELETE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 5900-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 5100-STOP-BROWSE,
                     5100-STOP-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 6500-NO-RECORDS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 6400-STOP-BROWSE,
                     6400-STOP-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 6300-WRITE-PNDB-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 6070-DELETE-MAILING
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 6300-WRITE-PNDB-RECORD,
                     6300-WRITE-PNDB-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 6300-WRITE-PNDB-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 6690-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 7090-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 7180-NO-REQUEST-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 7280-NO-REQUEST-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL630' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
