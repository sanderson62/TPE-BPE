00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL640.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 04/27/94 15:38:12.
00007 *                            VMOD=2.040
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022 *
00023 *REMARKS.    TRANSACTION - EXC1 - ACCOUNT BILLING.
00024 *
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL640AO FILLER
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
083104* 083104    2004083000002  PEMA PREVENT ONLINE BILLING
072105* 072105    2004120100002  PEMA FIX ONLINE BILLING FOR DCC SPP
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
080612* 080612  CR2012042700005  PEMA  ADD OVER 120 DAYS FOR AHL
032113* 032113  CR2012110800003  PEMA  CORRECT SPP AH REF COMMISSION
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
032219* 032219  CR2019020800001  PEMA  Increase width of bi-type column
101101******************************************************************
00025  ENVIRONMENT DIVISION.
00026
00027      EJECT
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030  77  FILLER  PIC X(32)  VALUE '********************************'.
00031  77  FILLER  PIC X(32)  VALUE '*    EL640 WORKING STORAGE     *'.
00032  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.040 ************'.
070805 77  CNC-FACT                    PIC S999V9(7) COMP-3 VALUE +0.
070805 77  S1                          PIC S999 COMP-3 VALUE +0.
00034 *    COPY ELCSCTM.
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
00035 *    COPY ELCSCRTY.
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
00200      12  WS-RESPONSE             PIC S9(8)       COMP.
00201          88  WS-RESP-NORMAL                      VALUE +00.
00202          88  WS-RESP-NOTFND                      VALUE +13.
               88  WS-RESP-DUPREC                      VALUE +14.
               88  WS-RESP-DUPKEY                      VALUE +15.
00203          88  WS-RESP-NOTOPEN                     VALUE +19.
               88  WS-RESP-ENDFILE                     VALUE +20.
032219 01  benefit-code-stuff.
032219     05  ws-lf-benefit-cd-stuff.
032219         10  ws-lf-alpha         pic xxx   value spaces.
032219         10  ws-lf-descrip       pic x(10) value spaces.
032219         10  ws-lf-comment       pic x(10) value spaces.
032219         10  ws-lf-cov-type      pic x     value spaces.
032219         10  ws-lf-spec-calc-cd  pic x     value spaces.
032219         10  ws-lf-joint-ind     pic x     value spaces.
032219     05  ws-ah-benefit-cd-stuff.
032219         10  ws-ah-alpha         pic xxx   value spaces.
032219         10  ws-ah-descrip       pic x(10) value spaces.
032219         10  ws-ah-comment       pic x(10) value spaces.
032219         10  ws-ah-spec-calc-cd  pic x     value spaces.
032219         10  ws-ah-joint-ind     pic x     value spaces.
032219         10  WS-AH-CATEGORY      PIC X     VALUE ' '.
00039  01  STANDARD-AREAS.
070805     12  CLAS-LOOK                   PIC XX      VALUE '  '.
00040      12  SC-ITEM             PIC S9(4) COMP VALUE +1.
00041
00042      12  W-TRANSFER-CONTROL.
00043          16  WT-CR-CARRIER           PIC X     VALUE SPACES.
00044          16  WT-CR-GROUPING          PIC X(6)  VALUE SPACES.
00045          16  WT-CR-STATE             PIC XX    VALUE SPACES.
00046          16  WT-CR-ACCOUNT           PIC X(10) VALUE SPACES.
00047          16  WT-CR-FIN-RESP          PIC X(10) VALUE SPACES.
00048          16  WT-CR-TYPE              PIC X     VALUE SPACES.
00049
00050      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
00051      12  MAP-NAME            PIC X(8)    VALUE 'EL640A'.
00052      12  MAPSET-NAME         PIC X(8)    VALUE 'EL640S'.
00053      12  SCREEN-NUMBER       PIC X(4)    VALUE '640A'.
00054      12  TRANS-ID            PIC X(4)    VALUE 'EXC1'.
00055      12  EL6311-TRANS-ID     PIC X(4)    VALUE 'EXB1'.
00056      12  EL633-TRANS-ID      PIC X(4)    VALUE 'EXB7'.
00057      12  EL633DMD-TRANS-ID   PIC X(4)    VALUE 'EX1F'.
00058      12  EL635-TRANS-ID      PIC X(4)    VALUE 'EXJ4'.
00059      12  EL650-TRANS-ID      PIC X(4)    VALUE 'EXC4'.
00060      12  EL652-TRANS-ID      PIC X(4)    VALUE 'EXD4'.
00061      12  EL658-TRANS-ID      PIC X(4)    VALUE 'EXJ3'.
00062      12  THIS-PGM            PIC X(8)    VALUE 'EL640'.
00063      12  EL640A              PIC X(8)    VALUE 'EL640A'.
00064      12  EL640B              PIC X(8)    VALUE 'EL640B'.
00065      12  PGM-NAME            PIC X(8).
00066      12  TIME-IN             PIC S9(7).
00067      12  TIME-OUT-R  REDEFINES TIME-IN.
00068          16  FILLER          PIC X.
00069          16  TIME-OUT        PIC 99V99.
00070          16  FILLER          PIC XX.
00071
00072      12  XCTL-005            PIC X(8)    VALUE 'EL005'.
00073      12  XCTL-010            PIC X(8)    VALUE 'EL010'.
00074      12  XCTL-626            PIC X(8)    VALUE 'EL626'.
00075      12  XCTL-633            PIC X(8)    VALUE 'EL633'.
00076      12  XCTL-633DMD         PIC X(8)    VALUE 'EL633DMD'.
00077      12  XCTL-635            PIC X(8)    VALUE 'EL635'.
00078      12  XCTL-PYAJ           PIC X(8)    VALUE 'EL633'.
00079      12  XCTL-650            PIC X(8)    VALUE 'EL650'.
00080      12  XCTL-652            PIC X(8)    VALUE 'EL652'.
00081      12  XCTL-658            PIC X(8)    VALUE 'EL658'.
00082      12  XCTL-6401           PIC X(8)    VALUE 'EL6401'.
00083      12  LINK-001            PIC X(8)    VALUE 'EL001'.
00084      12  LINK-004            PIC X(8)    VALUE 'EL004'.
00085      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00086      12  ERPNDB-ALT-FILE-ID  PIC X(8)    VALUE 'ERPNDB2'.
00087      12  ERPNDB-FILE-ID      PIC X(8)    VALUE 'ERPNDB'.
00088      12  ELCNTL-FILE-ID      PIC X(8)    VALUE 'ELCNTL'.
00089      12  ELLETR-FILE-ID      PIC X(8)    VALUE 'ELLETR'.
00090      12  ERCOMP-FILE-ID      PIC X(8)    VALUE 'ERCOMP'.
00091      12  ERPYAJ-FILE-ID      PIC X(8)    VALUE 'ERPYAJ'.
00092      12  ERACCT-ALT-FILE-ID  PIC X(8)    VALUE 'ERACCT2'.
00093      12  ERACCT-FILE-ID      PIC X(8)    VALUE 'ERACCT'.
00094      12  ERBILL-FILE-ID      PIC X(8)    VALUE 'ERBILL'.
00095      12  RETURNED-FROM       PIC X(8)    VALUE SPACES.
00096      12  QID.
00097          16  QID-TERM        PIC X(4).
00098          16  FILLER          PIC X(4)    VALUE '640A'.
CIDMOD     12  CLIENT-ITY          PIC XXX     VALUE 'ITY'.
00099
00100  01  WORK-AREA.
00101      12  W-LETTER-IND            PIC X     VALUE SPACE.
00102          88  W-LETTER-NOT-FOUND    VALUE 'Y'.
00103      12  FIRST-TIME-SW           PIC X     VALUE 'Y'.
00104          88  FIRST-TIME            VALUE 'Y'.
00105      12  C-RECORD-FOUND-SW       PIC X     VALUE 'Y'.
00106          88  C-RECORD-FOUND        VALUE 'Y'.
00107      12  ACCOUNT-FIN-RESP-SW     PIC X     VALUE ' '.
00108          88  ACCOUNT-FIN-RESP      VALUE 'Y'.
00109          88  ACCOUNT-NOT-FIN-RESP  VALUE 'N'.
00110      12  VALID-BILL-TYPE-VALUES  PIC X     VALUE SPACE.
083104         88  VALID-BILL-TYPE       VALUE  '1' '2'.
083104*        88  VALID-BILL-TYPE       VALUE  '1' '2' '3' '4' '5'.
00112      12  BILL-BATCH-TRAILER-SW   PIC X     VALUE SPACE.
00113          88  BILL-BATCH-TRAILER    VALUE 'Y'.
00114      12  PNDB-EOF-SW             PIC X     VALUE SPACE.
00115          88  PNDB-EOF              VALUE 'Y'.
00116      12  PYAJ-EOF-SW             PIC X     VALUE SPACE.
00117          88  PYAJ-EOF              VALUE 'Y'.
00118      12  COMP-UPDATE-SW          PIC X     VALUE SPACE.
00119          88  UPDATE-COMP-TOTALS    VALUE 'Y'.
00120      12  LIMIT-BILLING-SW        PIC X     VALUE SPACE.
00121          88  LIMIT-BILLING         VALUE 'Y'.
00122      12  WS-PROCESS-SW           PIC X     VALUE SPACE.
00123          88  DO-NOT-BILL-THIS-ACCT VALUE 'Y'.
00124      12  WS-CHARGEBACK-LF-SW     PIC X     VALUE SPACE.
00125          88  CHARGEBACK-CAN-LF     VALUE 'Y'.
00126          88  NO-CHARGEBACK-LF      VALUE 'N'.
00127      12  WS-CHARGEBACK-AH-SW     PIC X     VALUE SPACE.
00128          88  CHARGEBACK-CAN-AH     VALUE 'Y'.
00129          88  NO-CHARGEBACK-AH      VALUE 'N'.
00130      12  NO-BILL-REC-SW          PIC X     VALUE SPACE.
00131          88  NO-BILL-RECS          VALUE 'Y'.
00132      12  DATA-VOIDED-SW          PIC X     VALUE SPACE.
00133          88  DATA-VOIDED           VALUE 'Y'.
00134      12  BATCHES-PROCESSED-SW    PIC X     VALUE SPACE.
00135          88  MORE-THAN-6-BATCHES   VALUE 'Y'.
00136      12  BILLING-DETAIL-TYPE     PIC XX    VALUE SPACES.
00137          88  TOTAL-STATEMENT       VALUE 'TS'.
00138          88  BILLING-DETAIL        VALUE 'BD'.
00139          88  CARRIER-ADDRESS       VALUE 'CA'.
00140          88  GEN-AGT-ADDRESS       VALUE 'GA'.
00141      12  ELCNTL-UPDATE-SW        PIC X     VALUE SPACE.
00142          88  ELCNTL-UPDATE         VALUE 'Y'.
00143      12  WS-ENTERED-FROM-MAP     PIC X     VALUE 'A'.
00144          88  WS-ENTERED-FROM-A     VALUE 'A'.
00145
00146      12  ACCOM-SUB               PIC 99    VALUE ZEROS.
00147      12  SUB                     PIC S999  COMP-3  VALUE ZEROS.
00148
00149      12  WORK-ZIP                PIC 9(9)  VALUE ZEROS.
00150      12  WORK-ZIPR1 REDEFINES WORK-ZIP.
00151          16  WORK-ZIPR1-4        PIC 9(4).
00152          16  WORK-ZIPR1-5        PIC 9(5).
00153      12  WORK-ZIPR2 REDEFINES WORK-ZIP.
00154          16  WORK-ZIPR2-5        PIC 9(5).
00155          16  WORK-ZIPR2-4        PIC 9(4).
00156      12  WS-AM-ZIP.
00157          16  WS-AM-ZIP-PRIME     PIC X(5).
00158          16  WS-AM-ZIP-DASH      PIC X.
00159          16  WS-AM-ZIP-PLUS4     PIC X(4).
00160      12  WS-AM-CANADIAN-ZIP  REDEFINES  WS-AM-ZIP.
00161          16  WS-AM-CAN-POST-1    PIC XXX.
00162          16  FILLER              PIC X.
00163          16  WS-AM-CAN-POST-2    PIC XXX.
00164          16  FILLER              PIC XXX.
00165      12  WS-CF-ZIP.
00166          16  WS-CF-ZIP-PRIME     PIC X(5) VALUE SPACES.
00167          16  WS-CF-ZIP-DASH      PIC X    VALUE SPACES.
00168          16  WS-CF-ZIP-PLUS4     PIC X(4) VALUE SPACES.
00169      12  WS-CF-CANADIAN-ZIP  REDEFINES  WS-CF-ZIP.
00170          16  WS-CF-CAN-POST-1    PIC XXX.
00171          16  FILLER              PIC X.
00172          16  WS-CF-CAN-POST-2    PIC XXX.
00173          16  FILLER              PIC XXX.
00174      12  WS-CO-ZIP.
00175          16  WS-CO-ZIP-PRIME     PIC X(5) VALUE SPACES.
00176          16  WS-CO-ZIP-DASH      PIC X    VALUE SPACES.
00177          16  WS-CO-ZIP-PLUS4     PIC X(4) VALUE SPACES.
00178      12  WS-CO-CANADIAN-ZIP  REDEFINES  WS-CO-ZIP.
00179          16  WS-CO-CAN-POST-1    PIC XXX.
00180          16  FILLER              PIC X.
00181          16  WS-CO-CAN-POST-2    PIC XXX.
00182          16  FILLER              PIC XXX.
00183
00184      12  WORK-SEQ-NO             PIC S9(9)   COMP-3 VALUE +0.
00185      12  WS-ERROR-MSG.
00186          16  WS-ERROR-TEXT       PIC X(35) VALUE SPACES.
00187          16  WS-ERROR-BATCH      PIC X(6)  VALUE SPACES.
00188          16  FILLER              PIC X(24) VALUE SPACES.
00189      12  WS-WORK-DATE.
00190          16  WS-MONTH            PIC 99    VALUE ZEROS.
00191          16  WS-DAY              PIC 99    VALUE ZEROS.
00192          16  WS-YEAR             PIC 99    VALUE ZEROS.
00193      12  WS-MONTH-END-DATE.
00194          16  WS-ME-YEAR          PIC 99    VALUE ZEROS.
00195          16  WS-ME-MONTH         PIC 99    VALUE ZEROS.
00196          16  WS-ME-DAY           PIC 99    VALUE ZEROS.
00197      12  WS-CURRENT-DATE         PIC XX    VALUE SPACES.
00198      12  WS-CURRENT-DATE-MDY     PIC X(6)  VALUE SPACES.
00199      12  WS-CURRENT-DATE-EDIT    PIC X(8)  VALUE SPACES.
00200      12  WS-PREV-BILL-DATE       PIC XX    VALUE LOW-VALUES.
00201      12  WS-BEGIN-DATE.
00202          16  FILLER              PIC X(3)  VALUE SPACES.
00203          16  WS-BEGIN-DAY        PIC XX    VALUE SPACES.
00204          16  FILLER              PIC X(3)  VALUE SPACES.
CIDMOD     12  PRINT-CONTROL.
CIDMOD         16  SINGLE-SPACE        PIC X     VALUE SPACE.
CIDMOD         16  DOUBLE-SPACE        PIC X     VALUE ZERO.
CIDMOD         16  TRIPLE-SPACE        PIC X     VALUE '-'.
CIDMOD         16  SUPPRESS-SPACE      PIC X     VALUE '+'.
CIDMOD         16  TOP-OF-PAGE         PIC X     VALUE '1'.
00205      12  WS-EXP-DATE.
00206          16  WS-SAV-EXP-DT     OCCURS 10 TIMES
00207                                INDEXED BY DTNDX  PIC XX.
00208      12  WS-REMIT-TO.
00209          16  WS-SAV-REMIT-TO   OCCURS 10 TIMES
00210                                INDEXED BY RTNDX  PIC X(10).
00211      12  WORK-REMIT-TO           PIC X(10) VALUE SPACES.
CIDMOD     12  WS-REMITTED             PIC S9(6)V99 VALUE ZEROS.
00212      12  WS-LINECTR              PIC 99    VALUE ZEROS.
00213      12  WS-PGECTR               PIC 999   VALUE 1.
00214      12  WS-LINE-SEQ-NO          PIC S9(4) COMP VALUE +1.
00215      12  WS-CARR-COMP.
00216          16  WS-CARRIER          PIC X     VALUE SPACE.
00217          16  WS-COMP             PIC X(6)  VALUE SPACES.
00218
00219      12  REPORT-TOTALS.
00220          16  TOT-REPT-LF-PREM    PIC S9(7)V99   VALUE ZEROS.
CIDMOD         16  TOT-REPT-LF-REF     PIC S9(7)V99   VALUE ZEROS.
00221          16  TOT-REPT-LF-COMP    PIC S9(7)V99   VALUE ZEROS.
CIDMOD         16  TOT-REPT-LF-COMP-R  PIC S9(7)V99   VALUE ZEROS.
00222          16  TOT-REPT-AH-PREM    PIC S9(7)V99   VALUE ZEROS.
CIDMOD         16  TOT-REPT-AH-REF     PIC S9(7)V99   VALUE ZEROS.
00223          16  TOT-REPT-AH-COMP    PIC S9(7)V99   VALUE ZEROS.
CIDMOD         16  TOT-REPT-AH-COMP-R  PIC S9(7)V99   VALUE ZEROS.
00224          16  TOT-LF-REPT-FACE-AMT   PIC S9(7)V99   VALUE ZEROS.
00225          16  TOT-AH-REPT-FACE-AMT   PIC S9(7)V99   VALUE ZEROS.
00226          16  TOT-REPT-PREM       PIC S9(7)V99   VALUE ZEROS.
00227          16  TOT-REPT-CANCEL-FEE PIC S9(7)V99   VALUE ZEROS.
00228      12  WORK-AMT                PIC S9(7)V99   VALUE ZEROS.
00229      12  WORK-CHECK-AMT          PIC S9(7)V99   VALUE ZEROS.
00230      12  WORK-CANCEL-FEE         PIC S9(7)V99   VALUE ZEROS.
00231      12  WS-I-LF-PREMIUM-AMT     PIC S9(7)V99   COMP-3 VALUE +0.
00232      12  WS-I-LF-BENEFIT-AMT     PIC S9(7)V99   COMP-3 VALUE +0.
00233      12  WS-I-AH-PREMIUM-AMT     PIC S9(7)V99   COMP-3 VALUE +0.
00234      12  WS-I-AH-BENEFIT-AMT     PIC S9(7)V99   COMP-3 VALUE +0.
00235      12  WS-C-LF-CANCEL-AMT      PIC S9(5)V99   COMP-3 VALUE +0.
00236      12  WS-C-AH-CANCEL-AMT      PIC S9(5)V99   COMP-3 VALUE +0.
00237      12  WS-C-CANCEL-FEE         PIC S9(5)V99   COMP-3 VALUE +0.
00238      12  WS-LF-CAN-COMP          PIC S9(6)V99   COMP-3 VALUE +0.
00239      12  WS-AH-CAN-COMP          PIC S9(6)V99   COMP-3 VALUE +0.
00240      12  WS-PAY-ADJ              PIC S9(6)V99   COMP-3 VALUE +0.
00241      12  WS-CI-LIFE-BENEFIT      PIC S9(7)V99   COMP-3 VALUE +0.
00242      12  LF-COMM-TBL-NO          PIC X(3)       VALUE SPACES.
00243      12  JT-COMM-TBL-NO          PIC X(3)       VALUE SPACES.
00244      12  AH-COMM-TBL-NO          PIC X(3)       VALUE SPACES.
00245      12  AGENT-OWES              PIC X(18)
00246                          VALUE 'AGENT OWES       ='.
00247      12  OWED-TO-AGENT           PIC X(18)
00248                          VALUE 'OWED TO AGENT    ='.
00249      12  WS-PY-ENTRY-COMMENT.
00250          16  WS-PY-CURRENT-DATE      PIC X(6) VALUE SPACES.
00251          16  FILLER                  PIC X(14) VALUE
00252              ' CHK TO AGENT'.
00253
00254      12  WS-ITY-COMMENT.
00255          16  WS-PY-BEGIN-DT          PIC X(8) VALUE SPACES.
00256          16  FILLER                  PIC X(06) VALUE
00257              ' THRU '.
00258          16  WS-PY-END-DT            PIC X(8) VALUE SPACES.
00259
00260      EJECT
00261      12  WS-ACCT-ADDR-AREA.
00262          16  WS-ACCT-LINES OCCURS 6 TIMES INDEXED BY A-INDX.
00263            18  WS-ACCT-ZIP           PIC X(10) VALUE SPACES.
00264            18  FILLER                PIC X(10) VALUE SPACES.
00265            18  WS-A-LAST-ZIP         PIC X(10) VALUE SPACES.
00266
00267      12  WS-REMIT-ADDR-AREA.
00268          16  WS-REMIT-ACCT           PIC X(10) VALUE SPACES.
00269          16  WS-REMIT-LINES OCCURS 6 TIMES INDEXED BY R-INDX.
00270            18  WS-REMIT-ZIP          PIC X(10) VALUE SPACES.
00271            18  FILLER                PIC X(10) VALUE SPACES.
00272            18  WS-R-LAST-ZIP         PIC X(10) VALUE SPACES.
00273
00274      12  WS-EFFECT-DATE.
00275          16  WS-EFFECT-YR            PIC 99 VALUE ZEROS.
00276          16  WS-EFFECT-MO            PIC 99 VALUE ZEROS.
00277          16  WS-EFFECT-DA            PIC 99 VALUE ZEROS.
00278
00279      12  WS-LIFE-CANCEL-DT.
00280          16  WS-LF-CANCEL-YR         PIC 99 VALUE ZEROS.
00281          16  WS-LF-CANCEL-MO         PIC 99 VALUE ZEROS.
00282          16  WS-LF-CANCEL-DA         PIC 99 VALUE ZEROS.
00283
00284      12  WS-AH-CANCEL-DT.
00285          16  WS-AH-CANCEL-YR         PIC 99 VALUE ZEROS.
00286          16  WS-AH-CANCEL-MO         PIC 99 VALUE ZEROS.
00287          16  WS-AH-CANCEL-DA         PIC 99 VALUE ZEROS.
00288
00289      12  MONTHS-DIFF-LF            PIC S9(05)  VALUE ZEROS.
00290      12  MONTHS-DIFF-AH            PIC S9(05)  VALUE ZEROS.
00291
00292      12  WS-BENEFIT.
00293          16  FILLER                PIC X   VALUE SPACES.
00294          16  WS-BENE-CD            PIC XX  VALUE SPACES.
00295
00296      12  WS-DESC.
00297          16 WS-BATCH-DESC        PIC X(10) VALUE SPACES.
00298          16 WS-BATCH-NBR         PIC X(6)  VALUE SPACES.
00299          16 FILLER               PIC X(4)  VALUE SPACES.
00300      12  WS-NON-PREM             PIC S9(7)V99   COMP-3.
00301      12  WS-NON-COMM             PIC S9(7)V99   COMP-3.
00302      EJECT
00303  01  ACCESS-KEYS.
00304      12  ERPNDB-PRIME-KEY.
00305          16  ERPNDB-CO-CD        PIC X     VALUE SPACES.
00306          16  ERPNDB-BATCH        PIC X(6)  VALUE SPACES.
00307          16  ERPNDB-SEQ-NO       PIC S9(4) COMP VALUE +0.
00308          16  ERPNDB-CHG-SEQ-NO   PIC S9(4) COMP VALUE +0.
00309
00310      12  ERPNDB-LENGTH           PIC S9(4) COMP VALUE +585.
00311
00312      12  ERPNDB-ALT-KEY.
00313          16  ERPNDB-CO-CD-A1     PIC X     VALUE SPACES.
00314          16  ERPNDB-CARR         PIC X     VALUE SPACES.
00315          16  ERPNDB-GROUP        PIC X(6)  VALUE SPACES.
00316          16  ERPNDB-STATE        PIC XX    VALUE SPACES.
00317          16  ERPNDB-ACCT         PIC X(10) VALUE SPACES.
00318          16  ERPNDB-EFF-DT       PIC XX    VALUE SPACES.
00319          16  ERPNDB-CERT.
00320              20  ERPNDB-CERT-PRM PIC X(10) VALUE SPACES.
00321              20  ERPNDB-CERTSFX  PIC X     VALUE SPACES.
00322          16  ERPNDB-ACHG-SEQ-NO  PIC S9(4) COMP.
00323          16  ERPNDB-REC-TYPE     PIC X     VALUE SPACES.
00324
00325      12  ELCNTL-KEY.
00326          16  ELCNTL-COMPANY-ID   PIC XXX   VALUE SPACES.
00327          16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
070805         16  ELCNTL-ACCESS.
00328              20  ELCNTL-FILLER   PIC X(3)  VALUE SPACES.
00329              20  ELCNTL-CARRIER  PIC X     VALUE SPACES.
00330          16  ELCNTL-SEQ-NO       PIC S9(4) COMP VALUE ZEROS.
00331
00332      12  ELCNTL-LENGTH           PIC S9(4) COMP VALUE +504.
00333
00334      12  ERCOMP-KEY.
00335          16  ERCOMP-COMP-CD      PIC X     VALUE SPACE.
00336          16  ERCOMP-CARRIER      PIC X     VALUE SPACES.
00337          16  ERCOMP-GROUPING     PIC X(6)  VALUE SPACES.
00338          16  ERCOMP-FIN-RESP     PIC X(10) VALUE SPACES.
00339          16  ERCOMP-ACCT         PIC X(10) VALUE SPACES.
00340          16  ERCOMP-RECORD-TYPE  PIC X     VALUE SPACES.
00341
00342      12  ERPYAJ-BROWSE-COMP-KEY.
00343          16  ERPYAJ-BR-COMP-CD   PIC X     VALUE SPACE.
00344          16  ERPYAJ-BR-CARRIER   PIC X     VALUE SPACES.
00345          16  ERPYAJ-BR-GROUPING  PIC X(6)  VALUE SPACES.
00346          16  ERPYAJ-BR-FIN-RESP  PIC X(10) VALUE SPACES.
00347          16  ERPYAJ-BR-ACCOUNT   PIC X(10) VALUE SPACES.
00348
00349      12  ERCOMP-LENGTH           PIC S9(4) COMP VALUE +700.
00350
00351      12  ERPYAJ-KEY.
00352          16  ERPYAJ-COMP-CD      PIC X     VALUE SPACE.
00353          16  ERPYAJ-CARRIER      PIC X     VALUE SPACES.
00354          16  ERPYAJ-GROUPING     PIC X(6)  VALUE SPACES.
00355          16  ERPYAJ-FIN-RESP     PIC X(10) VALUE SPACES.
00356          16  ERPYAJ-ACCOUNT      PIC X(10) VALUE SPACES.
00357          16  ERPYAJ-FILE-SEQ-NO  PIC S9(8) VALUE +0 COMP.
00358          16  ERPYAJ-RECORD-TYPE  PIC X     VALUE SPACES.
00359
00360      12  ERPYAJ-LENGTH           PIC S9(4) COMP VALUE +200.
00361
00362      12  ERACCT-PRIME-KEY.
00363          16  ERACCT-P-CO-CD      PIC X     VALUE SPACES.
00364          16  ERACCT-P-CARRIER    PIC X     VALUE SPACES.
00365          16  ERACCT-P-GROUPING   PIC X(6)  VALUE SPACES.
00366          16  ERACCT-P-STATE      PIC XX    VALUE SPACES.
00367          16  ERACCT-P-ACCOUNT    PIC X(10) VALUE SPACES.
00368          16  ERACCT-P-EXP-DATE   PIC XX    VALUE SPACES.
00369          16  FILLER              PIC X(4)  VALUE SPACES.
00370
00371      12  ERACCT-LENGTH           PIC S9(4) COMP VALUE +2000.
00372
00373      12  ERACCT-ALT-KEY.
00374          16  ERACCT-A-CO-CD      PIC X     VALUE SPACES.
00375          16  ERACCT-A-CARRIER    PIC X     VALUE SPACES.
00376          16  ERACCT-A-GROUPING   PIC X(6)  VALUE SPACES.
00377          16  ERACCT-A-STATE      PIC XX    VALUE SPACES.
00378          16  ERACCT-A-ACCOUNT    PIC X(10) VALUE SPACES.
00379          16  ERACCT-A-EXP-DATE   PIC XX    VALUE SPACES.
00380          16  FILLER              PIC X(4)  VALUE SPACES.
00381
00382      12  ERBILL-KEY.
00383          16  ERBILL-CO-CD        PIC X.
00384          16  ERBILL-CARRIER      PIC X.
00385          16  ERBILL-GROUP        PIC X(6).
00386          16  ERBILL-ACCT         PIC X(10).
00387          16  ERBILL-FIN-RESP     PIC X(10).
00388          16  ERBILL-REC-TYPE     PIC X.
00389          16  ERBILL-LINE-SEQ-NO  PIC S9(4) COMP.
00390
00391      12  ERBILL-LENGTH           PIC S9(4) COMP VALUE +210.
00392
00393      12  ELLETR-KEY.
00394          16  ELLETR-COMPANY-CD   PIC  X      VALUE SPACE.
00395          16  ELLETR-FORM-NO      PIC  X(12)  VALUE SPACES.
00396          16  ELLETR-LINE-SEQ     PIC S9(04)  VALUE +0 COMP.
00397
00398      EJECT
00399
00400  01  ERROR-NUMBERS.
00401      12  ER-0004                 PIC X(4)  VALUE '0004'.
00402      12  ER-0008                 PIC X(4)  VALUE '0008'.
00403      12  ER-0013                 PIC X(4)  VALUE '0013'.
00404      12  ER-0022                 PIC X(4)  VALUE '0022'.
00405      12  ER-0029                 PIC X(4)  VALUE '0029'.
00406      12  ER-0070                 PIC X(4)  VALUE '0070'.
00407      12  ER-0194                 PIC X(4)  VALUE '0194'.
00408      12  ER-0195                 PIC X(4)  VALUE '0195'.
00409      12  ER-0196                 PIC X(4)  VALUE '0196'.
00410      12  ER-0197                 PIC X(4)  VALUE '0197'.
00411      12  ER-2208                 PIC X(4)  VALUE '2208'.
00412      12  ER-2210                 PIC X(4)  VALUE '2210'.
00413      12  ER-2212                 PIC X(4)  VALUE '2212'.
00414      12  ER-2215                 PIC X(4)  VALUE '2215'.
00415      12  ER-2230                 PIC X(4)  VALUE '2230'.
00416      12  ER-2233                 PIC X(4)  VALUE '2233'.
00417      12  ER-2249                 PIC X(4)  VALUE '2249'.
00418      12  ER-2250                 PIC X(4)  VALUE '2250'.
00419      12  ER-2370                 PIC X(4)  VALUE '2370'.
00420      12  ER-2371                 PIC X(4)  VALUE '2371'.
00421      12  ER-2383                 PIC X(4)  VALUE '2383'.
00422      12  ER-2385                 PIC X(4)  VALUE '2385'.
00423      12  ER-2401                 PIC X(4)  VALUE '2401'.
00424      12  ER-2403                 PIC X(4)  VALUE '2403'.
00425      12  ER-2404                 PIC X(4)  VALUE '2404'.
00426      12  ER-2405                 PIC X(4)  VALUE '2405'.
00427      12  ER-2406                 PIC X(4)  VALUE '2406'.
00428      12  ER-2407                 PIC X(4)  VALUE '2407'.
00429      12  ER-2408                 PIC X(4)  VALUE '2408'.
00430      12  ER-2409                 PIC X(4)  VALUE '2409'.
00431      12  ER-2411                 PIC X(4)  VALUE '2411'.
00432      12  ER-2421                 PIC X(4)  VALUE '2421'.
00433      12  ER-2434                 PIC X(4)  VALUE '2434'.
00434      12  ER-2435                 PIC X(4)  VALUE '2435'.
00435      12  ER-2436                 PIC X(4)  VALUE '2436'.
00436      12  ER-2438                 PIC X(4)  VALUE '2438'.
00437      12  ER-2439                 PIC X(4)  VALUE '2439'.
00438      12  ER-2443                 PIC X(4)  VALUE '2443'.
00439      12  ER-2472                 PIC X(4)  VALUE '2472'.
00440      12  ER-2539                 PIC X(4)  VALUE '2539'.
00441      12  ER-2564                 PIC X(4)  VALUE '2564'.
00442      12  ER-2570                 PIC X(4)  VALUE '2570'.
00443      12  ER-2571                 PIC X(4)  VALUE '2571'.
00444      12  ER-2597                 PIC X(4)  VALUE '2597'.
00445      12  ER-3145                 PIC X(4)  VALUE '3145'.
00446      12  ER-3165                 PIC X(4)  VALUE '3165'.
00447      12  ER-3188                 PIC X(4)  VALUE '3188'.
032219     12  ER-7285                 PIC X(4)  VALUE '7285'.
00448      12  ER-7389                 PIC X(4)  VALUE '7389'.
00449      EJECT
00450  01  BI-REPORT-HEADINGS.
00451      12  BI-PREVIEW-HD.
00452          16  FILLER          PIC X(42)           VALUE SPACES.
00453          16  FILLER          PIC X(47)           VALUE
00454                 '**** STATEMENT IS FOR REVIEW PURPOSES ONLY ****'.
00455          16  FILLER          PIC X(43)           VALUE SPACES.
00456
00457      12  BI-HD1.
00458          16  FILLER          PIC X(44)           VALUE SPACES.
00459          16  FILLER          PIC X(44)           VALUE
00460                  '  CREDIT LIFE, ACCIDENT & HEALTH STATEMENT  '.
00461          16  FILLER          PIC X(36)           VALUE SPACES.
00462          16  FILLER          PIC X(8)            VALUE ' EL640 '.
00463
00464      12  BI-HD2.
00465          16  FILLER          PIC X(51)           VALUE SPACES.
00466          16  BI-HD-CO        PIC X(30).
00467          16  FILLER          PIC X(43)           VALUE SPACES.
00468          16  BI-HD-RUN-DT    PIC X(8)            VALUE SPACES.
00469
00470      12  BI-HD3.
00471          16  FILLER          PIC X(57)           VALUE SPACES.
00472          16  BI-HD-BILL-DT   PIC X(18).
00473          16  FILLER          PIC X(37)           VALUE SPACES.
00474          16  FILLER          PIC X(5)            VALUE 'PAGE'.
00475          16  BI-HD-PG        PIC ZZ,ZZ9.
00476
032219     12  BI-HD4.
032219         16  filler          pic  x(92)      value
032219         '                             CERTIFICATE  EFFECTIVE CANC
032219-        'EL          BEN.            WRITTEN '.
032219
032219*        16  FILLER          PIC  X(44)      VALUE
032219*            '                             CERTIFICATE  EF'.
032219*        16  FILLER          PIC  X(44)      VALUE
032219*            'EFFECTIVE   CANCEL       BEN.      WRITTEN  '.
032219*  ===>  12  FILLER          PIC  X(44)      VALUE
032219*            '    COMP.                    FACE AMOUNT /  '.
00484
032219     12  BI-HD5.
032219         16  filler          pic x(131)   value
032219             '       NAME OF INSURED         NUMBER       DATE
032219-            ' DATE   TERM    TYPE            PREMIUM       PCT.
032219-            '   COMPENSATION    BENEFIT '.
032219
032219*        16  FILLER          PIC  X(44)      VALUE
032219*            '       NAME OF INSURED            NUMBER    '.
032219*        16  FILLER          PIC  X(44)      VALUE
032219*            '  DATE       DATE   TERM TYPE      PREMIUM  '.
032219*        16  FILLER          PIC  X(44)      VALUE
032219*            '     PCT.    COMPENSATION        BENEFIT    '.
00492
00493      12  BI-HD6.
00494          16  FILLER          PIC  X(44)      VALUE SPACES.
00495          16  FILLER          PIC  X(37)      VALUE SPACES.
00496          16  FILLER          PIC  X(45)      VALUE
00497              '     PROCESSED                  NON-PROCESSED'.
00498          16  FILLER          PIC  X(07)      VALUE SPACES.
00499      12  BI-HD7.
00500          16  FILLER          PIC  X(44)      VALUE SPACES.
00501          16  FILLER          PIC  X(35)      VALUE SPACES.
00502          16  FILLER          PIC  X(45)      VALUE
00503              ' PREMIUM     COMMISSION        PREMIUM     CO'.
00504          16  FILLER          PIC  X(09)      VALUE
00505              'MMISSION'.
00506
00509 *                                COPY ELCDATE.
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
00511 *                                COPY ELCLOGOF.
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
00513 *                                COPY ELCATTR.
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
00515 *                                COPY ELCEMIB.
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
00517 *                                COPY ELCINTF.
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
00518      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00519          16  PI-MAP-NAME         PIC X(8).
00520          16  PI-ACCT-NAME        PIC X(30).
00521          16  PI-SAV-REMIT-TO     PIC X(10).
00522          16  PI-SAV-ACCT         PIC X(10).
00523          16  PI-SAV-CARR         PIC X.
00524          16  PI-SAV-GROUP        PIC X(6).
00525          16  PI-SAV-STATE        PIC XX.
00526          16  PI-SAV-EXP-DT       PIC XX.
00527          16  PI-BAL-FRWD-GA      PIC S9(7)V99   COMP-3.
00528          16  PI-BAL-FRWD         PIC S9(7)V99   COMP-3.
00529          16  PI-PREMIUM          PIC S9(7)V99   COMP-3.
00530          16  PI-REMITTED         PIC S9(7)V99   COMP-3.
00531          16  PI-TOT-ISS-COMP     PIC S9(7)V99   COMP-3.
00532          16  PI-TOT-CAN-COMP     PIC S9(7)V99   COMP-3.
00533          16  PI-ADJUSTMENTS      PIC S9(7)V99   COMP-3.
00534          16  PI-DISBURSED        PIC S9(7)V99   COMP-3.
00535          16  PI-END-BAL          PIC S9(7)V99   COMP-3.
00536          16  PI-LF-ISS-COMP      PIC S9(7)V99   COMP-3.
00537          16  PI-AH-ISS-COMP      PIC S9(7)V99   COMP-3.
00538          16  PI-LF-CAN-COMP      PIC S9(7)V99   COMP-3.
00539          16  PI-AH-CAN-COMP      PIC S9(7)V99   COMP-3.
00540          16  PI-LIMIT-BILLING-BTCHS.
00541              20  PI-BILLING-BATCHES   OCCURS 3 TIMES
00542                                  PIC X(6).
00543          16  PI-BILL-TYPE        PIC X.
00544              88  PI-PREV-BILL        VALUE '1'.
00545              88  PI-PREV-REBILL      VALUE '2'.
00546              88  PI-PREVIEW          VALUE '1' '2'.
00547              88  PI-BILL             VALUE '3'.
00548              88  PI-REBILLING        VALUE '4'.
00549              88  PI-VOID-BILL        VALUE '5'.
00550              88  PI-TOT-REBILL       VALUE '2' '4'.
00551              88  PI-UPDATE-FILES     VALUE '3' '4' '5'.
00552              88  PI-BILLING-FUNCTION VALUE '1' '2' '3' '4'.
00553          16  PI-BILL-ERRS        PIC X.
00554          16  PI-CHECK-SW         PIC X.
00555              88  PI-CHECK-PRODUCED   VALUE 'Y'.
00556          16  PI-DATA-BILLED-SW   PIC X.
00557              88  PI-DATA-BILLED      VALUE 'Y'.
00558          16  PI-MONTH-END-DATE.
00559              20  PI-ME-MONTH     PIC 99.
00560              20  FILLER          PIC X.
00561              20  PI-ME-DAY       PIC 99.
00562              20  FILLER          PIC X.
00563              20  PI-ME-YEAR      PIC 99.
00564          16  PI-BILLING-COUNTS.
00565              20  FILLER OCCURS 6 TIMES INDEXED BY PINDX.
00566                  24  PI-BATCH    PIC X(6).
00567                  24  PI-BILLED   PIC 9(6)  COMP-3.
00568                  24  PI-PREV     PIC 9(6)  COMP-3.
00569                  24  PI-NOBILL   PIC 9(6)  COMP-3.
00570                  24  PI-PREM     PIC S9(7)V99   COMP-3.
00571                  24  PI-COMM     PIC S9(7)V99   COMP-3.
00572                  24  PI-NON-PREM PIC S9(7)V99   COMP-3.
00573                  24  PI-NON-COMM PIC S9(7)V99   COMP-3.
00574          16  PI-COMP-CONTROL.
00575              20  PI-COMP-CARRIER     PIC X.
00576              20  PI-COMP-GROUPING    PIC X(6).
00577              20  PI-COMP-FIN-RESP    PIC X(10).
00578          16  PI-SCRN-CONTROL.
00579              20  PI-SCR-CARRIER      PIC X.
00580              20  PI-SCR-GROUPING     PIC X(6).
00581              20  PI-SCR-STATE        PIC X(02).
00582              20  PI-SCR-ACCOUNT      PIC X(10).
00583              20  PI-SCR-FIN-RESP     PIC X(10).
00584              20  PI-SCR-TYPE         PIC X.
00585          16  PI-TRANSFER-SW          PIC X.
00586              88  PI-TRANSFER-BEFORE-ACT   VALUE 'Y'.
00587          16  FILLER                  PIC X(200).
00588
00589      EJECT
00590 *                            COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00591                              PIC X(1464).
00592      EJECT
00593 *                            COPY ELCAID.
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
00594  01  FILLER    REDEFINES DFHAID.
00595      12  FILLER              PIC X(8).
00596      12  PF-VALUES           PIC X       OCCURS 2.
00597      EJECT
00598 *                                         COPY EL640S.
       01  EL640AI.
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
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  AACTNAML PIC S9(0004) COMP.
           05  AACTNAMF PIC  X(0001).
           05  FILLER REDEFINES AACTNAMF.
               10  AACTNAMA PIC  X(0001).
           05  AACTNAMI PIC  X(0030).
      *    -------------------------------
           05  ACARHDGL PIC S9(0004) COMP.
           05  ACARHDGF PIC  X(0001).
           05  FILLER REDEFINES ACARHDGF.
               10  ACARHDGA PIC  X(0001).
           05  ACARHDGI PIC  X(0007).
      *    -------------------------------
           05  AGRPHDGL PIC S9(0004) COMP.
           05  AGRPHDGF PIC  X(0001).
           05  FILLER REDEFINES AGRPHDGF.
               10  AGRPHDGA PIC  X(0001).
           05  AGRPHDGI PIC  X(0005).
      *    -------------------------------
           05  ASTHDGL PIC S9(0004) COMP.
           05  ASTHDGF PIC  X(0001).
           05  FILLER REDEFINES ASTHDGF.
               10  ASTHDGA PIC  X(0001).
           05  ASTHDGI PIC  X(0005).
      *    -------------------------------
           05  AACCTL PIC S9(0004) COMP.
           05  AACCTF PIC  X(0001).
           05  FILLER REDEFINES AACCTF.
               10  AACCTA PIC  X(0001).
           05  AACCTI PIC  X(0010).
      *    -------------------------------
           05  ACARIERL PIC S9(0004) COMP.
           05  ACARIERF PIC  X(0001).
           05  FILLER REDEFINES ACARIERF.
               10  ACARIERA PIC  X(0001).
           05  ACARIERI PIC  X(0001).
      *    -------------------------------
           05  AGROUPL PIC S9(0004) COMP.
           05  AGROUPF PIC  X(0001).
           05  FILLER REDEFINES AGROUPF.
               10  AGROUPA PIC  X(0001).
           05  AGROUPI PIC  X(0006).
      *    -------------------------------
           05  ASTATEL PIC S9(0004) COMP.
           05  ASTATEF PIC  X(0001).
           05  FILLER REDEFINES ASTATEF.
               10  ASTATEA PIC  X(0001).
           05  ASTATEI PIC  X(0002).
      *    -------------------------------
           05  ABALFWDL PIC S9(0004) COMP.
           05  ABALFWDF PIC  X(0001).
           05  FILLER REDEFINES ABALFWDF.
               10  ABALFWDA PIC  X(0001).
           05  ABALFWDI PIC  X(0011).
      *    -------------------------------
           05  APREMUML PIC S9(0004) COMP.
           05  APREMUMF PIC  X(0001).
           05  FILLER REDEFINES APREMUMF.
               10  APREMUMA PIC  X(0001).
           05  APREMUMI PIC  X(0011).
      *    -------------------------------
           05  AHD1L PIC S9(0004) COMP.
           05  AHD1F PIC  X(0001).
           05  FILLER REDEFINES AHD1F.
               10  AHD1A PIC  X(0001).
           05  AHD1I PIC  X(0018).
      *    -------------------------------
           05  AREMITL PIC S9(0004) COMP.
           05  AREMITF PIC  X(0001).
           05  FILLER REDEFINES AREMITF.
               10  AREMITA PIC  X(0001).
           05  AREMITI PIC  X(0011).
      *    -------------------------------
           05  ABATCH1L PIC S9(0004) COMP.
           05  ABATCH1F PIC  X(0001).
           05  FILLER REDEFINES ABATCH1F.
               10  ABATCH1A PIC  X(0001).
           05  ABATCH1I PIC  X(0006).
      *    -------------------------------
           05  ABILL1L PIC S9(0004) COMP.
           05  ABILL1F PIC  X(0001).
           05  FILLER REDEFINES ABILL1F.
               10  ABILL1A PIC  X(0001).
           05  ABILL1I PIC  X(0006).
      *    -------------------------------
           05  APREV1L PIC S9(0004) COMP.
           05  APREV1F PIC  X(0001).
           05  FILLER REDEFINES APREV1F.
               10  APREV1A PIC  X(0001).
           05  APREV1I PIC  X(0006).
      *    -------------------------------
           05  ANOBIL1L PIC S9(0004) COMP.
           05  ANOBIL1F PIC  X(0001).
           05  FILLER REDEFINES ANOBIL1F.
               10  ANOBIL1A PIC  X(0001).
           05  ANOBIL1I PIC  X(0006).
      *    -------------------------------
           05  AHD2L PIC S9(0004) COMP.
           05  AHD2F PIC  X(0001).
           05  FILLER REDEFINES AHD2F.
               10  AHD2A PIC  X(0001).
           05  AHD2I PIC  X(0018).
      *    -------------------------------
           05  ACOMPISL PIC S9(0004) COMP.
           05  ACOMPISF PIC  X(0001).
           05  FILLER REDEFINES ACOMPISF.
               10  ACOMPISA PIC  X(0001).
           05  ACOMPISI PIC  X(0011).
      *    -------------------------------
           05  ABATCH2L PIC S9(0004) COMP.
           05  ABATCH2F PIC  X(0001).
           05  FILLER REDEFINES ABATCH2F.
               10  ABATCH2A PIC  X(0001).
           05  ABATCH2I PIC  X(0006).
      *    -------------------------------
           05  ABILL2L PIC S9(0004) COMP.
           05  ABILL2F PIC  X(0001).
           05  FILLER REDEFINES ABILL2F.
               10  ABILL2A PIC  X(0001).
           05  ABILL2I PIC  X(0006).
      *    -------------------------------
           05  APREV2L PIC S9(0004) COMP.
           05  APREV2F PIC  X(0001).
           05  FILLER REDEFINES APREV2F.
               10  APREV2A PIC  X(0001).
           05  APREV2I PIC  X(0006).
      *    -------------------------------
           05  ANOBIL2L PIC S9(0004) COMP.
           05  ANOBIL2F PIC  X(0001).
           05  FILLER REDEFINES ANOBIL2F.
               10  ANOBIL2A PIC  X(0001).
           05  ANOBIL2I PIC  X(0006).
      *    -------------------------------
           05  AHD3L PIC S9(0004) COMP.
           05  AHD3F PIC  X(0001).
           05  FILLER REDEFINES AHD3F.
               10  AHD3A PIC  X(0001).
           05  AHD3I PIC  X(0018).
      *    -------------------------------
           05  ACOMCANL PIC S9(0004) COMP.
           05  ACOMCANF PIC  X(0001).
           05  FILLER REDEFINES ACOMCANF.
               10  ACOMCANA PIC  X(0001).
           05  ACOMCANI PIC  X(0011).
      *    -------------------------------
           05  ABATCH3L PIC S9(0004) COMP.
           05  ABATCH3F PIC  X(0001).
           05  FILLER REDEFINES ABATCH3F.
               10  ABATCH3A PIC  X(0001).
           05  ABATCH3I PIC  X(0006).
      *    -------------------------------
           05  ABILL3L PIC S9(0004) COMP.
           05  ABILL3F PIC  X(0001).
           05  FILLER REDEFINES ABILL3F.
               10  ABILL3A PIC  X(0001).
           05  ABILL3I PIC  X(0006).
      *    -------------------------------
           05  APREV3L PIC S9(0004) COMP.
           05  APREV3F PIC  X(0001).
           05  FILLER REDEFINES APREV3F.
               10  APREV3A PIC  X(0001).
           05  APREV3I PIC  X(0006).
      *    -------------------------------
           05  ANOBIL3L PIC S9(0004) COMP.
           05  ANOBIL3F PIC  X(0001).
           05  FILLER REDEFINES ANOBIL3F.
               10  ANOBIL3A PIC  X(0001).
           05  ANOBIL3I PIC  X(0006).
      *    -------------------------------
           05  AHD4L PIC S9(0004) COMP.
           05  AHD4F PIC  X(0001).
           05  FILLER REDEFINES AHD4F.
               10  AHD4A PIC  X(0001).
           05  AHD4I PIC  X(0018).
      *    -------------------------------
           05  AADJUSTL PIC S9(0004) COMP.
           05  AADJUSTF PIC  X(0001).
           05  FILLER REDEFINES AADJUSTF.
               10  AADJUSTA PIC  X(0001).
           05  AADJUSTI PIC  X(0011).
      *    -------------------------------
           05  ABATCH4L PIC S9(0004) COMP.
           05  ABATCH4F PIC  X(0001).
           05  FILLER REDEFINES ABATCH4F.
               10  ABATCH4A PIC  X(0001).
           05  ABATCH4I PIC  X(0006).
      *    -------------------------------
           05  ABILL4L PIC S9(0004) COMP.
           05  ABILL4F PIC  X(0001).
           05  FILLER REDEFINES ABILL4F.
               10  ABILL4A PIC  X(0001).
           05  ABILL4I PIC  X(0006).
      *    -------------------------------
           05  APREV4L PIC S9(0004) COMP.
           05  APREV4F PIC  X(0001).
           05  FILLER REDEFINES APREV4F.
               10  APREV4A PIC  X(0001).
           05  APREV4I PIC  X(0006).
      *    -------------------------------
           05  ANOBIL4L PIC S9(0004) COMP.
           05  ANOBIL4F PIC  X(0001).
           05  FILLER REDEFINES ANOBIL4F.
               10  ANOBIL4A PIC  X(0001).
           05  ANOBIL4I PIC  X(0006).
      *    -------------------------------
           05  AHD5L PIC S9(0004) COMP.
           05  AHD5F PIC  X(0001).
           05  FILLER REDEFINES AHD5F.
               10  AHD5A PIC  X(0001).
           05  AHD5I PIC  X(0018).
      *    -------------------------------
           05  ADISBURL PIC S9(0004) COMP.
           05  ADISBURF PIC  X(0001).
           05  FILLER REDEFINES ADISBURF.
               10  ADISBURA PIC  X(0001).
           05  ADISBURI PIC  X(0011).
      *    -------------------------------
           05  ABATCH5L PIC S9(0004) COMP.
           05  ABATCH5F PIC  X(0001).
           05  FILLER REDEFINES ABATCH5F.
               10  ABATCH5A PIC  X(0001).
           05  ABATCH5I PIC  X(0006).
      *    -------------------------------
           05  ABILL5L PIC S9(0004) COMP.
           05  ABILL5F PIC  X(0001).
           05  FILLER REDEFINES ABILL5F.
               10  ABILL5A PIC  X(0001).
           05  ABILL5I PIC  X(0006).
      *    -------------------------------
           05  APREV5L PIC S9(0004) COMP.
           05  APREV5F PIC  X(0001).
           05  FILLER REDEFINES APREV5F.
               10  APREV5A PIC  X(0001).
           05  APREV5I PIC  X(0006).
      *    -------------------------------
           05  ANOBIL5L PIC S9(0004) COMP.
           05  ANOBIL5F PIC  X(0001).
           05  FILLER REDEFINES ANOBIL5F.
               10  ANOBIL5A PIC  X(0001).
           05  ANOBIL5I PIC  X(0006).
      *    -------------------------------
           05  AENDHDGL PIC S9(0004) COMP.
           05  AENDHDGF PIC  X(0001).
           05  FILLER REDEFINES AENDHDGF.
               10  AENDHDGA PIC  X(0001).
           05  AENDHDGI PIC  X(0018).
      *    -------------------------------
           05  ANETDUEL PIC S9(0004) COMP.
           05  ANETDUEF PIC  X(0001).
           05  FILLER REDEFINES ANETDUEF.
               10  ANETDUEA PIC  X(0001).
           05  ANETDUEI PIC  X(0011).
      *    -------------------------------
           05  ABATCH6L PIC S9(0004) COMP.
           05  ABATCH6F PIC  X(0001).
           05  FILLER REDEFINES ABATCH6F.
               10  ABATCH6A PIC  X(0001).
           05  ABATCH6I PIC  X(0006).
      *    -------------------------------
           05  ABILL6L PIC S9(0004) COMP.
           05  ABILL6F PIC  X(0001).
           05  FILLER REDEFINES ABILL6F.
               10  ABILL6A PIC  X(0001).
           05  ABILL6I PIC  X(0006).
      *    -------------------------------
           05  APREV6L PIC S9(0004) COMP.
           05  APREV6F PIC  X(0001).
           05  FILLER REDEFINES APREV6F.
               10  APREV6A PIC  X(0001).
           05  APREV6I PIC  X(0006).
      *    -------------------------------
           05  ANOBIL6L PIC S9(0004) COMP.
           05  ANOBIL6F PIC  X(0001).
           05  FILLER REDEFINES ANOBIL6F.
               10  ANOBIL6A PIC  X(0001).
           05  ANOBIL6I PIC  X(0006).
      *    -------------------------------
           05  ABILTYPL PIC S9(0004) COMP.
           05  ABILTYPF PIC  X(0001).
           05  FILLER REDEFINES ABILTYPF.
               10  ABILTYPA PIC  X(0001).
           05  ABILTYPI PIC  X(0001).
      *    -------------------------------
           05  APRODSWL PIC S9(0004) COMP.
           05  APRODSWF PIC  X(0001).
           05  FILLER REDEFINES APRODSWF.
               10  APRODSWA PIC  X(0001).
           05  APRODSWI PIC  X(0001).
      *    -------------------------------
           05  ABILERRL PIC S9(0004) COMP.
           05  ABILERRF PIC  X(0001).
           05  FILLER REDEFINES ABILERRF.
               10  ABILERRA PIC  X(0001).
           05  ABILERRI PIC  X(0001).
      *    -------------------------------
           05  ABTCH1L PIC S9(0004) COMP.
           05  ABTCH1F PIC  X(0001).
           05  FILLER REDEFINES ABTCH1F.
               10  ABTCH1A PIC  X(0001).
           05  ABTCH1I PIC  X(0006).
      *    -------------------------------
           05  ABTCH2L PIC S9(0004) COMP.
           05  ABTCH2F PIC  X(0001).
           05  FILLER REDEFINES ABTCH2F.
               10  ABTCH2A PIC  X(0001).
           05  ABTCH2I PIC  X(0006).
      *    -------------------------------
           05  ABTCH3L PIC S9(0004) COMP.
           05  ABTCH3F PIC  X(0001).
           05  FILLER REDEFINES ABTCH3F.
               10  ABTCH3A PIC  X(0001).
           05  ABTCH3I PIC  X(0006).
      *    -------------------------------
           05  AERMSG1L PIC S9(0004) COMP.
           05  AERMSG1F PIC  X(0001).
           05  FILLER REDEFINES AERMSG1F.
               10  AERMSG1A PIC  X(0001).
           05  AERMSG1I PIC  X(0076).
      *    -------------------------------
           05  AERMSG2L PIC S9(0004) COMP.
           05  AERMSG2F PIC  X(0001).
           05  FILLER REDEFINES AERMSG2F.
               10  AERMSG2A PIC  X(0001).
           05  AERMSG2I PIC  X(0076).
      *    -------------------------------
           05  APFNTERL PIC S9(0004) COMP.
           05  APFNTERF PIC  X(0001).
           05  FILLER REDEFINES APFNTERF.
               10  APFNTERA PIC  X(0001).
           05  APFNTERI PIC  9(2).
      *    -------------------------------
           05  APF5HDGL PIC S9(0004) COMP.
           05  APF5HDGF PIC  X(0001).
           05  FILLER REDEFINES APF5HDGF.
               10  APF5HDGA PIC  X(0001).
           05  APF5HDGI PIC  X(0020).
      *    -------------------------------
           05  AGAHDGL PIC S9(0004) COMP.
           05  AGAHDGF PIC  X(0001).
           05  FILLER REDEFINES AGAHDGF.
               10  AGAHDGA PIC  X(0001).
           05  AGAHDGI PIC  X(0025).
       01  EL640AO REDEFINES EL640AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACTNAMO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARHDGO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRPHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABALFWDO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APREMUMO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHD1O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREMITO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABATCH1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILL1O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APREV1O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANOBIL1O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHD2O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMPISO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABATCH2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILL2O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APREV2O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANOBIL2O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHD3O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMCANO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABATCH3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILL3O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APREV3O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANOBIL3O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHD4O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AADJUSTO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABATCH4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILL4O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APREV4O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANOBIL4O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHD5O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADISBURO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABATCH5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILL5O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APREV5O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANOBIL5O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AENDHDGO PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANETDUEO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABATCH6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILL6O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APREV6O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANOBIL6O PIC  Z(5)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APRODSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILERRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABTCH1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABTCH2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABTCH3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AERMSG1O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AERMSG2O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFNTERO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APF5HDGO PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGAHDGO PIC  X(0025).
      *    -------------------------------
       01  EL640BI REDEFINES EL640AI.
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
           05  BCMPNYL PIC S9(0004) COMP.
           05  BCMPNYF PIC  X(0001).
           05  FILLER REDEFINES BCMPNYF.
               10  BCMPNYA PIC  X(0001).
           05  BCMPNYI PIC  X(0003).
      *    -------------------------------
           05  BUSERIDL PIC S9(0004) COMP.
           05  BUSERIDF PIC  X(0001).
           05  FILLER REDEFINES BUSERIDF.
               10  BUSERIDA PIC  X(0001).
           05  BUSERIDI PIC  X(0004).
      *    -------------------------------
           05  BCHKNOL PIC S9(0004) COMP.
           05  BCHKNOF PIC  X(0001).
           05  FILLER REDEFINES BCHKNOF.
               10  BCHKNOA PIC  X(0001).
           05  BCHKNOI PIC  9(6).
      *    -------------------------------
           05  BNAMEL PIC S9(0004) COMP.
           05  BNAMEF PIC  X(0001).
           05  FILLER REDEFINES BNAMEF.
               10  BNAMEA PIC  X(0001).
           05  BNAMEI PIC  X(0029).
      *    -------------------------------
           05  BCHKAMTL PIC S9(0004) COMP.
           05  BCHKAMTF PIC  X(0001).
           05  FILLER REDEFINES BCHKAMTF.
               10  BCHKAMTA PIC  X(0001).
           05  BCHKAMTI PIC  S9(9)V99.
      *    -------------------------------
           05  BADDR1L PIC S9(0004) COMP.
           05  BADDR1F PIC  X(0001).
           05  FILLER REDEFINES BADDR1F.
               10  BADDR1A PIC  X(0001).
           05  BADDR1I PIC  X(0029).
      *    -------------------------------
           05  BADDR2L PIC S9(0004) COMP.
           05  BADDR2F PIC  X(0001).
           05  FILLER REDEFINES BADDR2F.
               10  BADDR2A PIC  X(0001).
           05  BADDR2I PIC  X(0029).
      *    -------------------------------
           05  BPAYDT1L PIC S9(0004) COMP.
           05  BPAYDT1F PIC  X(0001).
           05  FILLER REDEFINES BPAYDT1F.
               10  BPAYDT1A PIC  X(0001).
           05  BPAYDT1I PIC  X(0008).
      *    -------------------------------
           05  BCITYSTL PIC S9(0004) COMP.
           05  BCITYSTF PIC  X(0001).
           05  FILLER REDEFINES BCITYSTF.
               10  BCITYSTA PIC  X(0001).
           05  BCITYSTI PIC  X(0029).
      *    -------------------------------
           05  BPAYDT2L PIC S9(0004) COMP.
           05  BPAYDT2F PIC  X(0001).
           05  FILLER REDEFINES BPAYDT2F.
               10  BPAYDT2A PIC  X(0001).
           05  BPAYDT2I PIC  X(0008).
      *    -------------------------------
           05  BZIPL PIC S9(0004) COMP.
           05  BZIPF PIC  X(0001).
           05  FILLER REDEFINES BZIPF.
               10  BZIPA PIC  X(0001).
           05  BZIPI PIC  X(0010).
      *    -------------------------------
           05  BACCNAML PIC S9(0004) COMP.
           05  BACCNAMF PIC  X(0001).
           05  FILLER REDEFINES BACCNAMF.
               10  BACCNAMA PIC  X(0001).
           05  BACCNAMI PIC  X(0030).
      *    -------------------------------
           05  BACCTL PIC S9(0004) COMP.
           05  BACCTF PIC  X(0001).
           05  FILLER REDEFINES BACCTF.
               10  BACCTA PIC  X(0001).
           05  BACCTI PIC  X(0010).
      *    -------------------------------
           05  BCARIERL PIC S9(0004) COMP.
           05  BCARIERF PIC  X(0001).
           05  FILLER REDEFINES BCARIERF.
               10  BCARIERA PIC  X(0001).
           05  BCARIERI PIC  X(0001).
      *    -------------------------------
           05  BGROUPL PIC S9(0004) COMP.
           05  BGROUPF PIC  X(0001).
           05  FILLER REDEFINES BGROUPF.
               10  BGROUPA PIC  X(0001).
           05  BGROUPI PIC  X(0006).
      *    -------------------------------
           05  BSTATEL PIC S9(0004) COMP.
           05  BSTATEF PIC  X(0001).
           05  FILLER REDEFINES BSTATEF.
               10  BSTATEA PIC  X(0001).
           05  BSTATEI PIC  X(0002).
      *    -------------------------------
           05  BFORM1L PIC S9(0004) COMP.
           05  BFORM1F PIC  X(0001).
           05  FILLER REDEFINES BFORM1F.
               10  BFORM1A PIC  X(0001).
           05  BFORM1I PIC  X(0004).
      *    -------------------------------
           05  BFORM2L PIC S9(0004) COMP.
           05  BFORM2F PIC  X(0001).
           05  FILLER REDEFINES BFORM2F.
               10  BFORM2A PIC  X(0001).
           05  BFORM2I PIC  X(0004).
      *    -------------------------------
           05  BFORM3L PIC S9(0004) COMP.
           05  BFORM3F PIC  X(0001).
           05  FILLER REDEFINES BFORM3F.
               10  BFORM3A PIC  X(0001).
           05  BFORM3I PIC  X(0004).
      *    -------------------------------
           05  BALAMTL PIC S9(0004) COMP.
           05  BALAMTF PIC  X(0001).
           05  FILLER REDEFINES BALAMTF.
               10  BALAMTA PIC  X(0001).
           05  BALAMTI PIC  X(0011).
      *    -------------------------------
           05  BERMSGL PIC S9(0004) COMP.
           05  BERMSGF PIC  X(0001).
           05  FILLER REDEFINES BERMSGF.
               10  BERMSGA PIC  X(0001).
           05  BERMSGI PIC  X(0078).
      *    -------------------------------
           05  BPFNTERL PIC S9(0004) COMP.
           05  BPFNTERF PIC  X(0001).
           05  FILLER REDEFINES BPFNTERF.
               10  BPFNTERA PIC  X(0001).
           05  BPFNTERI PIC  9(2).
       01  EL640BO REDEFINES EL640AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCMPNYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BUSERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCHKNOO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAMEO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCHKAMTO PIC  Z(7).ZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDR1O PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDR2O PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAYDT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITYSTO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAYDT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCNAMO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFORM1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFORM2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFORM3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALAMTO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSGO PIC  X(0078).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFNTERO PIC  9(2).
      *    -------------------------------
00599  01  MAP-A REDEFINES EL640AO.
101101     12  FILLER                  PIC X(162).
00601      12  FILLER OCCURS 6 TIMES INDEXED BY ANDX.
00602          16  FILLER              PIC X(35).
00603          16  BATCH-LEN           PIC S9(4)   COMP.
00604          16  BATCH-ATTRB         PIC X.
00605          16  BATCH               PIC X(6).
00606          16  BILL-LEN            PIC S9(4)   COMP.
00607          16  BILL-ATTRB          PIC X.
00608          16  BILL                PIC Z(6).
00609          16  PREV-LEN            PIC S9(4)   COMP.
00610          16  PREV-ATTRB          PIC X.
00611          16  PREV                PIC Z(6).
00612          16  NOBILL-LEN          PIC S9(4)   COMP.
00613          16  NOBILL-ATTRB        PIC X.
00614          16  NOBILL              PIC Z(6).
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
00617  01  DFHCOMMAREA             PIC X(1024).
00618
00619      EJECT
00620      EJECT
00621 *                            COPY ERCPNDB.
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
011410         16  FILLER                       PIC X.
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
00622      EJECT
00623 *                            COPY ELCCNTL.
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
00624      EJECT
00625 *                            COPY ERCCOMP.
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
00626      EJECT
00627 *                            COPY ERCPYAJ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCPYAJ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
00008 *                                                                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
042303******************************************************************
042303*                   C H A N G E   L O G
042303*
042303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042303*-----------------------------------------------------------------
042303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042303* EFFECTIVE    NUMBER
042303*-----------------------------------------------------------------
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
042303******************************************************************
00019
00020  01  PENDING-PAY-ADJ.
00021      12  PY-RECORD-ID                     PIC XX.
00022          88  VALID-PY-ID                        VALUE 'PY'.
00023
00024      12  PY-CONTROL-PRIMARY.
00025          16  PY-COMPANY-CD                PIC X.
00026          16  PY-CARRIER                   PIC X.
00027          16  PY-GROUPING                  PIC X(6).
00028          16  PY-FIN-RESP                  PIC X(10).
00029          16  PY-ACCOUNT                   PIC X(10).
00030          16  PY-PRODUCER REDEFINES PY-ACCOUNT
00031                                           PIC X(10).
00032          16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
00033          16  PY-RECORD-TYPE               PIC X.
00034              88  PY-REMIT-RECEIVED            VALUE 'R'.
00035              88  PY-DEPOSIT                   VALUE 'D'.
00036              88  PY-CHARGE-TO-AGENT           VALUE 'C'.
00037              88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
00038              88  PY-ADJ-DEPOSIT               VALUE 'T'.
00039              88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
00040              88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
00041              88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
00042              88  PY-ADD-TO-BALANCE            VALUE 'Z'.
00043              88  PY-FICA-ENTRY                VALUE 'F'.
00044              88  PY-REMIT-IND-GROUPING        VALUE 'G'.
00045              88  PY-POLICY-FEE                VALUE 'W'.
042303             88  PY-DUE-PREM-ADJ              VALUE 'P'.
00046
00047      12  PY-PYMT-TYPE                     PIC X.
00048              88  PY-NEW-BUS-PYMT              VALUE 'B'.
00049              88  PY-REINS-PYMT                VALUE 'R'.
00050              88  PY-EXP-PYMT                  VALUE 'E'.
00051
00052      12  PY-BIL-INV                       PIC X(6).
00053      12  PY-REF-NO                        PIC X(12).
00054
00055      12  PY-LAST-MAINT-DT                 PIC XX.
00056      12  PY-LAST-MAINT-BY                 PIC X(4).
00057      12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00058
00059      12  PY-PYADJ-RECORD.
00060          16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
00061          16  PY-ENTRY-COMMENT             PIC X(30).
CIDMOD         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
CIDMOD             20  PY-GL-ACCOUNT            PIC X(10).
CIDMOD             20  PY-GL-STATE              PIC X(02).
CIDMOD             20  PY-GL-CANC-SW            PIC X(01).
CIDMOD                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
CIDMOD                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
CIDMOD             20  PY-GL-COMMENT            PIC X(10).
CIDMOD             20  FILLER      REDEFINES PY-GL-COMMENT.
CIDMOD                 24  PY-GL-CHECK-NO       PIC 9(06).
CIDMOD                 24  FILLER               PIC X(04).
CIDMOD             20  FILLER                   PIC X(07).
00074          16  PY-SAVE-ACCOUNT              PIC X(10).
00075          16  PY-SAVE-TYPE                 PIC X(01).
00076
00077          16  PY-LETTERS.
00078              20  PY-LETTER OCCURS 3 TIMES
00079                            INDEXED BY PY-LET-NDX
00080                                           PIC X(04).
00081
060205         16  PY-ERCOMP-TYPE               PIC X.
060205             88  PY-ACCOUNT-TYPE              VALUE 'A'.
060205             88  PY-GA-TYPE                   VALUE 'G'.
060205             88  PY-BANK-TYPE                 VALUE 'B'.
060205         16  FILLER                       PIC X(05).
00083
00084      12  PY-RECORD-STATUS.
00085          16  PY-CREDIT-SELECT-DT          PIC XX.
00086          16  PY-CREDIT-ACCEPT-DT          PIC XX.
00087          16  PY-BILLED-DATE               PIC XX.
00088          16  PY-REPORTED-DT               PIC XX.
00089          16  PY-PMT-APPLIED               PIC X.
00090              88  PY-ACCOUNT-PMT               VALUE 'A'.
00091              88  PY-GA-PMT                    VALUE 'G'.
00092              88  PY-OVWRITE-PMT               VALUE 'O'.
00093              88  PY-NON-AR-PMT                VALUE 'N'.
00094          16  FILLER                       PIC X(5).
00095          16  PY-INPUT-DT                  PIC XX.
00096          16  PY-CHECK-NUMBER              PIC X(6).
00097          16  PY-VOID-SW                   PIC X.
00098              88  PY-CHECK-VOIDED              VALUE 'V'.
00099          16  PY-CHECK-ORIGIN-SW           PIC X.
00100              88  PY-BILLING-CHECK             VALUE 'B'.
00101              88  PY-REFUND-CHECK              VALUE 'R'.
00102              88  PY-GA-CHECK                  VALUE 'G'.
00103              88  PY-CHECK-WRITTEN             VALUE 'W'.
00104              88  PY-CHECK-REVERSAL            VALUE 'V'.
00105          16  PY-CHECK-WRITTEN-DT          PIC XX.
00106          16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
00107          16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
00108          16  PY-BILL-FLAG                 PIC X.
00109              88  PY-BILLED                    VALUE 'B'.
00110          16  PY-AR-FLAG                   PIC X.
00111              88  PY-AR-CYCLE                  VALUE 'C'.
00112              88  PY-AR-MONTH-END              VALUE 'M'.
00113          16  PY-AR-DATE                   PIC XX.
00114
00115      12  PY-GL-CODES.
00116          16  PY-GL-DB                     PIC X(14).
00117          16  PY-GL-CR                     PIC X(14).
00118          16  PY-GL-FLAG                   PIC X.
00119          16  PY-GL-DATE                   PIC XX.
00120
00121      12  PY-CANCEL-FEE-FLAG               PIC X(2).
00122      12  FILLER                           PIC X(3).
00123 ******************************************************************
00628      EJECT
00629 *                            COPY ERCACCT.
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
00630      EJECT
00631 *                            COPY ERCBILL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCBILL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BILLING STATEMENTS FOR PRINTING           *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 210  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERBILL                        RKP=2,LEN=31    *
00013 *                                                                *
00014 *   LOG = NO                                                     *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00024 ******************************************************************
032219*                   C H A N G E   L O G
032219*
032219* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
032219*-----------------------------------------------------------------
032219*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
032219* EFFECTIVE    NUMBER
032219*-----------------------------------------------------------------
032219* 032219  CR2019020800001  PEMA  Increase width of bi-type column
032219******************************************************************
00017  01  BILLING-STATEMENT.
00018      02 BILLING-STATEMENT-FILE.
00019      12  BI-RECORD-ID                PIC XX.
00020          88  VALID-BI-ID                VALUE 'BI'.
00021
00022      12  BI-CONTROL-PRIMARY.
00023          16  BI-COMPANY-CD           PIC X.
00024          16  BI-CARRIER              PIC X.
00025          16  BI-GROUPING             PIC X(6).
00026          16  BI-ACCOUNT              PIC X(10).
00027          16  BI-FIN-RESP             PIC X(10).
00028          16  BI-RECORD-TYPE          PIC X.
00029              88  BI-HEADER-DATA         VALUE '1'.
00030              88  BI-ADDRESS-DATA        VALUE '2'.
00031              88  BI-TEXT-DATA           VALUE '3'.
00032          16  BI-LINE-SEQ-NO          PIC S9(4)     COMP.
00033
00034      12  BI-TEXT-RECORD.
00035          16  BI-SKIP-CONTROL         PIC X.
00036          16  BI-TEXT-LINE            PIC X(157).
00037          16  BI-TEXT-LINE-1 REDEFINES BI-TEXT-LINE.
00038              20  BI-ADDR-LIT         PIC X(14).
00039              20  BI-CO               PIC X(7).
00040              20  BI-DASH             PIC X.
00041              20  BI-ACCT             PIC X(10).
00042              20  FILLER              PIC XX.
00043              20  BI-ACCT-ADDR        PIC X(30).
00044              20  FILLER              PIC X.
00045              20  BI-REMIT-LIT        PIC X(11).
00046              20  FILLER              PIC XX.
00047              20  BI-REMIT-ADDR       PIC X(30).
00048              20  FILLER              PIC X(49).
00049          16  BI-TEXT-LINE-2 REDEFINES BI-TEXT-LINE.
00050              20  BI-INS-LAST-NAME    PIC X(15).
00051              20  FILLER              PIC X.
00052              20  BI-INS-1ST-NAME     PIC X(10).
00053              20  FILLER REDEFINES BI-INS-1ST-NAME.
00054                  24  BI-INS-INITS    PIC XX.
00055                  24  FILLER          PIC X(8).
00056              20  FILLER              PIC X.
00057              20  BI-UNDRWRTR.
00058                  24  BI-INS-INIT     PIC X.
032219                 24  FILLER          PIC XX.
00060              20  BI-CERT             PIC X(11).
032219             20  FILLER              PIC X.
00062              20  BI-EFF-DT           PIC X(8).
032219             20  FILLER              PIC X.
00064              20  BI-CAN-DT           PIC X(8).
032219             20  FILLER              PIC X.
00066              20  BI-ED-TERM          PIC ZZZ.
032219             20  FILLER              PIC X.
032219             20  BI-TYPE             PIC X(17).
032219*            20  FILLER              PIC X.
032219             20  BI-PREM             PIC Z,ZZZ,ZZZ.ZZ-.
032219             20  FILLER              PIC XX.
00072              20  BI-ED-RATE          PIC ZZZ.ZZZ.
00073              20  FILLER              PIC XX.
032219             20  BI-COMM             PIC ZZZ,ZZZ.ZZ-.
032219             20  FILLER              PIC X.
032219             20  BI-FACE-AMT         PIC ZZ,ZZZ,ZZZ.ZZ-.
00077              20  FILLER              PIC X(26).
00078          16  BI-TEXT-LINE-3 REDEFINES BI-TEXT-LINE.
00079              20  FILLER              PIC X(42).
00080              20  BI-TOT-DESC         PIC X(20).
00081              20  FILLER REDEFINES BI-TOT-DESC.
00082                  24  BI-TOT-LIT      PIC X(6).
00083                  24  BI-OVERRIDE-L6  PIC X(14).
032219             20  FILLER              PIC X(17).
00085              20  BI-TOT-PREM         PIC ZZZ,ZZZ,ZZZ.99-.
00086              20  BI-TOT-DASH REDEFINES
00087                  BI-TOT-PREM         PIC X(15).
032219             20  FILLER              PIC X(07).
00089              20  BI-COM-TOT          PIC ZZZ,ZZZ,ZZZ.99-.
00090              20  FILLER              PIC XX.
00091              20  BI-FACE-TOT         PIC ZZZ,ZZZ,ZZZ.99-.
032219             20  FILLER              PIC X(24).
00093          16  BI-TEXT-LINE-4 REDEFINES BI-TEXT-LINE.
00094              20  BI-ENTRY-DESC       PIC X(30).
00095              20  FILLER              PIC X(11).
00096              20  BI-ENTRY-AMT        PIC ZZZZ,ZZZ,ZZZ.99-.
00097              20  FILLER              PIC X(31).
00098              20  BI-ACCTG-COMMENT    PIC X(30).
00099              20  FILLER              PIC X(39).
00100          16  BI-TEXT-LINE-5 REDEFINES BI-TEXT-LINE.
00101              20  FILLER              PIC X(42).
00102              20  BI-TOT-DESC5        PIC X(20).
00103              20  FILLER              PIC X(11).
00104              20  BI-TOT-PREM5        PIC ZZZ,ZZZ,ZZZ.99-.
00105              20  BI-COM-TOT5         PIC ZZZ,ZZZ,ZZZ.99-.
00106              20  BI-NON-PREM5        PIC ZZZ,ZZZ,ZZZ.99-.
00107              20  BI-NON-COMM5        PIC ZZZ,ZZZ,ZZZ.99-.
00108              20  FILLER              PIC X(24).
00109          16  BI-TEXT-FIRST REDEFINES BI-TEXT-LINE.
00110              20  BI-TEXT-2-81        PIC X(80).
00111              20  FILLER              PIC X(77).
00112          16  BI-TEXT-LAST REDEFINES BI-TEXT-LINE.
00113              20  FILLER              PIC X(53).
00114              20  BI-TEXT-55-133      PIC X(79).
00115              20  FILLER              PIC X(25).
00116          16  BI-TEXT-TYPE            PIC X.
00117              88 DETAIL-LINE              VALUE 'D'.
00118          16  BI-TERM                 PIC S999.
00119          16  BI-BENEFIT-AMT          PIC S9(9)V99 COMP-3.
00120          16  BI-PREMIUM-AMT          PIC S9(7)V99 COMP-3.
00121          16  BI-RATE                 PIC S99V9(5) COMP-3.
00122
00123      12  BI-ADDRESS-RECORD  REDEFINES  BI-TEXT-RECORD.
00124          16  FILLER                  PIC X.
00125          16  BI-ACCT-ADDRESS-LINE    PIC X(30).
00126          16  FILLER                  PIC X(10).
00127          16  BI-REMIT-ADDRESS-LINE   PIC X(30).
00128          16  FILLER                  PIC X(106).
00129
00130      12  BI-HEADER-RECORD  REDEFINES  BI-TEXT-RECORD.
00131          16  BI-PROCESSOR-CD         PIC X(4).
00132          16  BI-STATEMENT-TYPE       PIC X.
00133              88  BI-PREVIEW-ONLY         VALUE 'P'.
00134          16  BI-NO-OF-COPIES         PIC S9.
00135          16  BI-CREATION-DT          PIC XX.
00136          16  BI-INITIAL-PRINT-DATE   PIC XX.
00137          16  BI-ACCOUNT-TOTALS.
00138              20  BI-BAL-FRWD         PIC S9(9)V99     COMP-3.
00139              20  BI-PREMIUM          PIC S9(9)V99     COMP-3.
00140              20  BI-REMITTED         PIC S9(9)V99     COMP-3.
00141              20  BI-TOT-ISS-COMP     PIC S9(9)V99     COMP-3.
00142              20  BI-TOT-CAN-COMP     PIC S9(9)V99     COMP-3.
00143              20  BI-ADJUSTMNTS       PIC S9(9)V99     COMP-3.
00144              20  BI-DISBURSED        PIC S9(9)V99     COMP-3.
00145              20  BI-END-BAL          PIC S9(9)V99     COMP-3.
00146          16  BI-FIN-RESP-ACCT        PIC X(10).
00147          16  BI-FIN-RESP-NAME        PIC X(30).
00148          16  FILLER                  PIC X(79).
00149
00150
00151      02 GA-BILLING-STATEMENT REDEFINES BILLING-STATEMENT-FILE.
00152      12  FILLER                      PIC XX.
00153
00154      12  GA-CONTROL-PRIMARY.
00155          16  FILLER                  PIC X(31).
00156
00157      12  GA-TEXT-RECORD.
00158          16  GA-SKIP-CONTROL         PIC X.
00159          16  GA-TEXT-LINE            PIC X(132).
00160          16  GA-TEXT-LINE-1 REDEFINES GA-TEXT-LINE.
00161              20  FILLER              PIC X.
00162              20  GA-CARRIER          PIC X.
00163              20  GA-GROUPING         PIC X(6).
00164              20  GA-DASH             PIC X.
00165              20  GA-AGENT            PIC X(10).
00166              20  FILLER              PIC X.
00167              20  GA-AGENT-ADDR       PIC X(30).
00168              20  FILLER              PIC X(82).
00169          16  GA-TEXT-LINE-2 REDEFINES GA-TEXT-LINE.
00170              20  GA-ACCT             PIC X(10).
00171              20  FILLER              PIC X.
00172              20  GA-ACCT-NAME        PIC X(30).
00173              20  GA-BEG-BAL          PIC ZZZZ,ZZZ.99-.
00174              20  GA-NET-PREM         PIC ZZZZ,ZZZ.ZZ-.
00175              20  GA-ACCT-COMP        PIC ZZZZ,ZZZ.ZZ-.
00176              20  GA-PMTS-ADJS        PIC ZZZZ,ZZZ.ZZ-.
00177              20  GA-UNPAID-NET-PREM  PIC ZZZZ,ZZZ.ZZ-.
00178              20  GA-BEN-OVERRIDE-L6  PIC X(6).
00179              20  FILLER              PIC X.
00180              20  GA-OVERWRITE        PIC ZZZZ,ZZZ.ZZ-.
00181              20  GA-AMT-DUE          PIC ZZZ,ZZZ.99-.
00182              20  FILLER              PIC X.
00183          16  GA-TEXT-LINE-3 REDEFINES GA-TEXT-LINE.
00184              20  FILLER              PIC X(11).
00185              20  GA-ENTRY-DESC       PIC X(30).
00186              20  FILLER              PIC X(60).
00187              20  GA-ENTRY-COMMENT    PIC X(30).
00188              20  FILLER              PIC X.
00189          16  FILLER.
00190              20  GA-BENEFIT-CD       PIC XX.
00191              20  GA-BEG-BAL-AMT      PIC S9(7)V99 COMP-3.
00192              20  GA-END-BAL-AMT      PIC S9(7)V99 COMP-3.
00193              20  GA-NET-PREM-AMT     PIC S9(7)V99 COMP-3.
00194              20  GA-ACCT-COMP-AMT    PIC S9(7)V99 COMP-3.
00195              20  GA-PMTS-ADJS-AMT    PIC S9(7)V99 COMP-3.
00196              20  GA-UNPAID-NET-AMT   PIC S9(7)V99 COMP-3.
00197              20  GA-OVERWRITE-AMT    PIC S9(7)V99 COMP-3.
00198              20  GA-AMT-DUE-AMT      PIC S9(7)V99 COMP-3.
00199
00200      12  GA-ADDRESS-RECORD  REDEFINES  GA-TEXT-RECORD.
00201          16  FILLER                  PIC X.
00202          16  GA-ACCT-ADDRESS-LINE    PIC X(30).
00203          16  FILLER                  PIC X(144).
00204
00205      12  GA-HEADER-RECORD  REDEFINES  GA-TEXT-RECORD.
00206          16  GA-PROCESSOR-CD         PIC X(4).
00207          16  GA-STATEMENT-TYPE       PIC X.
00208              88  GA-PREVIEW-ONLY         VALUE 'P'.
00209          16  GA-NO-OF-COPIES         PIC S9.
00210          16  GA-CREATION-DT          PIC XX.
00211          16  GA-INITIAL-PRINT-DATE   PIC XX.
00212          16  GA-AGENT-TOTALS.
00213              20  GA-NET-UNPD         PIC S9(9)V99     COMP-3.
00214              20  GA-COMP-UNPD-PREM   PIC S9(9)V99     COMP-3.
00215              20  GA-PREMIUM          PIC S9(9)V99     COMP-3.
00216              20  GA-REMITTED         PIC S9(9)V99     COMP-3.
00217              20  GA-TOT-ISS-COMP     PIC S9(9)V99     COMP-3.
00218              20  GA-TOT-CAN-COMP     PIC S9(9)V99     COMP-3.
00219              20  GA-ADJUSTMNTS       PIC S9(9)V99     COMP-3.
00220              20  GA-DISBURSED        PIC S9(9)V99     COMP-3.
00221              20  GA-END-BALANCE      PIC S9(9)V99     COMP-3.
00222          16  GA-AGENTS-NAME          PIC X(30).
00223          16  FILLER                  PIC X(81).
00224      12  FILLER                      PIC XX.
00225
00226
00632      EJECT
00633 *                            COPY ELCTEXT.
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
00634      EJECT
00635
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-BUSINESS
                                CONTROL-FILE COMPENSATION-MASTER
                                PENDING-PAY-ADJ ACCOUNT-MASTER
                                BILLING-STATEMENT TEXT-FILES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL640' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00637
00638      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00639      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00640      MOVE EIBTRMID               TO QID-TERM.
00641      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00642      MOVE '5'                    TO DC-OPTION-CODE.
00643      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00644      MOVE DC-BIN-DATE-1          TO WS-CURRENT-DATE.
00645      MOVE DC-GREG-DATE-1-MDY     TO WS-CURRENT-DATE-MDY.
00646      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE-EDIT.
00647
00648      IF EIBCALEN = 0
00649          GO TO 8800-UNAUTHORIZED-ACCESS.
00650
00651      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00652          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.
00653
00654      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00655          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00656              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00657              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00658              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00659              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00660              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00661              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00662              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00663              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00664              MOVE PI-CR-CONTROL-IN-PROGRESS TO W-TRANSFER-CONTROL
00665              MOVE SPACES          TO PI-CR-CONTROL-IN-PROGRESS
00666          ELSE
00667              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00668              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00669              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00670              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00671              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00672              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00673              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00674              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00675
00676      MOVE LOW-VALUES             TO EL640AI.
00677
00678      IF PI-AR-PROCESSING
00679         MOVE XCTL-635            TO XCTL-PYAJ.
00680
00681      IF PI-COMPANY-ID = 'DMD'
00682         MOVE XCTL-633DMD         TO XCTL-PYAJ.
00683
00684      IF RETURNED-FROM NOT = SPACES
00685          PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0690-EXIT
00686          IF RETURNED-FROM = XCTL-PYAJ AND NOT PI-VOID-BILL
00687              MOVE ZEROS          TO PI-ADJUSTMENTS
00688                                     PI-DISBURSED
00689                                     PI-REMITTED
00690              GO TO 1100-PYAJ-BILLING-PROCESS
00691          ELSE
00692              MOVE -1             TO APFNTERL
00693              PERFORM 5000-FORMAT-SCREEN THRU 5090-EXIT
00694              GO TO 8100-SEND-INITIAL-MAP.
00695
00696      IF EIBTRNID NOT = TRANS-ID
00697          PERFORM 0400-CLEAR-PI  THRU  0499-EXIT
00698          MOVE EL640A             TO PI-MAP-NAME
00699          MOVE -1                 TO APFNTERL
00700          GO TO 8100-SEND-INITIAL-MAP.
00701
00702      
      * EXEC CICS HANDLE CONDITION
00703 *        PGMIDERR  (9600-PGMID-ERROR)
00704 *        ERROR     (9990-ABEND)
00705 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005793' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035373933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00706
00707      IF EIBAID = DFHCLEAR
00708          IF PI-MAP-NAME = EL640B
00709              PERFORM 5000-FORMAT-SCREEN THRU 5090-EXIT
00710              MOVE EL640A         TO PI-MAP-NAME
00711              MOVE -1             TO APFNTERL
00712              GO TO 8100-SEND-INITIAL-MAP
00713          ELSE
00714              GO TO 9400-CLEAR.
00715
00716      IF PI-PROCESSOR-ID = 'LGXX'
00717          GO TO 0200-RECEIVE.
00718
00719      
      * EXEC CICS READQ TS
00720 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00721 *        INTO   (SECURITY-CONTROL)
00722 *        LENGTH (SC-COMM-LENGTH)
00723 *        ITEM   (SC-ITEM)
00724 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00005810' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00725
00726      MOVE SC-CREDIT-DISPLAY (18)  TO PI-DISPLAY-CAP.
00727      MOVE SC-CREDIT-UPDATE  (18)  TO PI-MODIFY-CAP.
00728
00729      IF NOT DISPLAY-CAP
00730          MOVE 'READ'          TO SM-READ
00731          PERFORM 9995-SECURITY-VIOLATION
00732          MOVE ER-0070         TO  EMI-ERROR
00733          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00734          GO TO 8100-SEND-INITIAL-MAP.
00735
00736  0200-RECEIVE.
00737      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00738          MOVE ER-0008            TO EMI-ERROR
00739          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00740          IF PI-MAP-NAME = EL640A
00741              MOVE -1             TO APFNTERL
00742              GO TO 8200-SEND-DATAONLY
00743          ELSE
00744              MOVE -1             TO BPFNTERL
00745              GO TO 8200-SEND-DATAONLY.
00746
00747      
      * EXEC CICS RECEIVE
00748 *        MAP      (PI-MAP-NAME)
00749 *        MAPSET   (MAPSET-NAME)
00750 *        INTO     (EL640AI)
00751 *    END-EXEC.
           MOVE LENGTH OF
            EL640AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005838' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL640AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00752
CIDMOD     IF PI-END-BAL NOT NUMERIC
CIDMOD         MOVE ZEROS              TO PI-END-BAL.
CIDMOD
00753      IF PI-MAP-NAME = EL640A
00754          IF APFNTERL > ZERO
00755              IF EIBAID NOT = DFHENTER
00756                  MOVE ER-0004    TO EMI-ERROR
00757                  MOVE AL-UNBOF   TO APFNTERA
00758                  MOVE -1         TO APFNTERL
00759                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00760                  GO TO 8200-SEND-DATAONLY
00761              ELSE
00762                  IF APFNTERI NUMERIC AND
00763                    (APFNTERI > 0 AND < 25)
00764                      MOVE PF-VALUES (APFNTERI) TO EIBAID
00765                  ELSE
00766                      MOVE ER-0029  TO EMI-ERROR
00767                      MOVE AL-UNBOF TO APFNTERA
00768                      MOVE -1       TO APFNTERL
00769                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00770                      GO TO 8200-SEND-DATAONLY
00771          ELSE
00772              NEXT SENTENCE
00773      ELSE
00774      IF PI-MAP-NAME = EL640B
00775          IF BPFNTERL > ZERO
00776              IF EIBAID NOT = DFHENTER
00777                  MOVE ER-0004    TO EMI-ERROR
00778                  MOVE AL-UNBOF   TO BPFNTERA
00779                  MOVE -1         TO BPFNTERL
00780                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00781                  GO TO 8200-SEND-DATAONLY
00782              ELSE
00783                  IF BPFNTERI NUMERIC AND
00784                    (BPFNTERI > 0 AND < 25)
00785                      MOVE PF-VALUES (BPFNTERI) TO EIBAID
00786                  ELSE
00787                      MOVE ER-0029  TO EMI-ERROR
00788                      MOVE AL-UNBOF TO BPFNTERA
00789                      MOVE -1       TO BPFNTERL
00790                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00791                      GO TO 8200-SEND-DATAONLY.
00792      EJECT
00793  0300-CHECK-PFKEYS.
00794      IF EIBAID = DFHPF23
00795          GO TO 8810-PF23.
00796
00797      IF EIBAID = DFHPF24
00798          GO TO 9200-RETURN-MAIN-MENU.
00799
00800      IF EIBAID = DFHPF12
00801          GO TO 9500-PF12.
00802
00803 ******************************************************************
00804 *       IF CARRIER OR ACCOUNT SECURITY ALLOW ONLY                *
00805 *            BILLING TYPE OPTION ONE OR TWO.                     *
00806 *                        04/27/84                                *
00807 ******************************************************************
00808
00809      IF PI-NO-CARRIER-SECURITY OR
00810         PI-NO-ACCOUNT-SECURITY
00811           GO TO 0305-CHECK-PFKEYS.
00812
00813      IF EIBAID = DFHENTER OR DFHPF6
00814         NEXT SENTENCE
00815        ELSE
00816         GO TO 0320-INPUT-ERROR.
00817
00818  0305-CHECK-PFKEYS.
00819      IF EIBAID = DFHPF3
00820        AND PI-MAP-NAME = EL640A
00821          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00822          MOVE PI-CARRIER         TO PI-CR-CARRIER
00823          MOVE PI-GROUPING        TO PI-CR-GROUPING
00824          MOVE PI-STATE           TO PI-CR-STATE
00825          MOVE PI-ACCOUNT         TO PI-CR-ACCOUNT
00826          MOVE XCTL-650           TO PGM-NAME
00827          GO TO 9300-XCTL.
00828
00829      IF EIBAID = DFHPF4
00830        AND PI-MAP-NAME = EL640A
00831         NEXT SENTENCE
00832      ELSE
00833         GO TO 0320-CONT-PFKEYS.
00834
00835      IF PI-CR-FIN-RESP NOT = SPACES
00836          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT
00837          MOVE PI-COMP-CARRIER    TO PI-CR-CARRIER
00838          MOVE PI-COMP-GROUPING   TO PI-CR-GROUPING
00839          MOVE PI-COMP-FIN-RESP   TO PI-CR-FIN-RESP
00840          MOVE XCTL-652           TO PGM-NAME
00841          GO TO 9300-XCTL.
00842
00843      IF PI-SCR-FIN-RESP = SPACES OR LOW-VALUES
00844          GO TO 0315-PF4-ERROR.
00845
00846      PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT.
00847
00848      MOVE PI-SCR-CARRIER    TO PI-CR-CARRIER.
00849      MOVE PI-SCR-GROUPING   TO PI-CR-GROUPING.
00850      MOVE PI-SCR-FIN-RESP   TO PI-CR-FIN-RESP.
00851      MOVE PI-SCR-ACCOUNT    TO PI-CR-ACCOUNT.
00852      MOVE PI-SCR-TYPE       TO PI-CR-TYPE.
00853      MOVE 'Y'               TO PI-TRANSFER-SW.
00854
00855      IF PI-ZERO-CARRIER  OR
00856         PI-ZERO-CAR-GROUP
00857          MOVE ZEROS              TO PI-CR-CARRIER.
00858
00859      IF PI-ZERO-GROUPING  OR
00860         PI-ZERO-CAR-GROUP
00861          MOVE ZEROS              TO PI-CR-GROUPING.
00862
00863      MOVE XCTL-652               TO PGM-NAME.
00864
00865      GO TO 9300-XCTL.
00866
00867  0315-PF4-ERROR.
00868
00869      MOVE ER-2407                TO EMI-ERROR.
00870      MOVE -1                     TO APFNTERL.
00871      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00872      GO TO 8200-SEND-DATAONLY.
00873
00874  0320-CONT-PFKEYS.
00875      IF EIBAID = DFHPF6
00876        AND PI-MAP-NAME = EL640A
00877          MOVE LOW-VALUES         TO EL640AO
00878          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00879          MOVE XCTL-6401          TO PGM-NAME
00880          GO TO 9300-XCTL.
00881
00882      IF EIBAID = DFHPF7
00883        AND PI-MAP-NAME = EL640A
00884          IF PI-CR-FIN-RESP NOT = SPACES
00885              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00886              MOVE PI-COMP-CARRIER    TO PI-CR-CARRIER
00887              MOVE PI-COMP-GROUPING   TO PI-CR-GROUPING
00888              MOVE PI-COMP-FIN-RESP   TO PI-CR-FIN-RESP
00889              MOVE XCTL-PYAJ          TO PGM-NAME
00890              GO TO 9300-XCTL
00891          ELSE
00892              IF PI-RETURN-TO-PROGRAM = XCTL-633 OR XCTL-635 OR
00893                                        XCTL-633DMD
00894                  GO TO 9400-CLEAR
00895              ELSE
00896                  MOVE ER-2411        TO EMI-ERROR
00897                  MOVE -1             TO APFNTERL
00898                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00899                  GO TO 8200-SEND-DATAONLY.
00900
00901      IF EIBAID = DFHPF8
00902        AND PI-MAP-NAME = EL640A
00903            PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00904            MOVE SPACES         TO PI-CR-CONTROL-IN-PROGRESS
00905            MOVE XCTL-658       TO PGM-NAME
00906            GO TO 9300-XCTL.
00907
00908      IF EIBAID = DFHPF1
00909        AND PI-MAP-NAME = EL640B
00910          GO TO 7500-PRODUCE-CHECK.
00911
00912      IF EIBAID = DFHPF5
00913        AND PI-AR-PROCESSING
00914          MOVE ER-3188            TO EMI-ERROR
00915          MOVE -1                 TO APFNTERL
00916          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00917          GO TO 8200-SEND-DATAONLY.
00918
00919      IF PI-COMPANY-ID = 'DMD'
00920        IF EIBAID = DFHPF5
00921          GO TO 0320-INPUT-ERROR.
00922
00923      IF EIBAID = DFHPF5
00924        AND PI-MAP-NAME = EL640A
00925             MOVE EL640B     TO PI-MAP-NAME
00926             GO TO 7000-PROCESS-CHECK.
00927
00928      IF PI-MAP-NAME = EL640A
00929          IF EIBAID = DFHENTER OR DFHPF3
00930              GO TO 0330-EDIT-DATA
00931          ELSE
00932              GO TO 0320-INPUT-ERROR
00933      ELSE
00934          IF PI-MAP-NAME = EL640B
00935              IF EIBAID = DFHENTER
00936                  
      * EXEC CICS BIF DEEDIT
00937 *                    FIELD  (BCHKAMTI)
00938 *                    LENGTH (11)
00939 *                END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006030' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BCHKAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00940                  MOVE BCHKAMTI   TO  WORK-CHECK-AMT
00941                  MOVE 'B'        TO  WS-ENTERED-FROM-MAP
00942                  GO TO 7000-PROCESS-CHECK.
00943
00944  0320-INPUT-ERROR.
00945      MOVE ER-0029                TO EMI-ERROR.
00946      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00947
00948      IF PI-MAP-NAME = EL640A
00949          MOVE AL-UNBON           TO APFNTERA
00950          MOVE -1                 TO APFNTERL
00951      ELSE
00952          MOVE AL-UNBON           TO BPFNTERA
00953          MOVE -1                 TO BPFNTERL.
00954
00955      GO TO 8200-SEND-DATAONLY.
00956
00957      EJECT
00958  0330-EDIT-DATA.
00959      MOVE ABILTYPI               TO VALID-BILL-TYPE-VALUES.
00960
00961      IF VALID-BILL-TYPE
00962         IF NOT MODIFY-CAP
00963            IF ABILTYPI = '1' OR '2'
00964               NEXT SENTENCE
00965            ELSE
00966               MOVE 'UPDATE'         TO SM-READ
00967               PERFORM 9995-SECURITY-VIOLATION
00968               MOVE ER-0070          TO EMI-ERROR
00969               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00970               GO TO 8100-SEND-INITIAL-MAP.
00971
00972      MOVE SPACES                 TO PI-BILLING-COUNTS
00973                                     PI-LIMIT-BILLING-BTCHS.
00974
00975 ******************************************************************
00976 *         SECURITY CHECK FOR CARRIER AND ACCOUNT.                *
00977 *                     04/12/84                                   *
00978 ******************************************************************
00979
00980      IF PI-CARRIER-SECURITY > SPACES
00981         IF ACARIERL > ZEROS
00982             IF PI-CARRIER-SECURITY = ACARIERI
00983                 MOVE AL-UANON    TO ACARIERA
00984             ELSE
00985                 MOVE -1          TO ACARIERL
00986                 MOVE AL-UABON    TO ACARIERA
00987                 MOVE ER-2370     TO EMI-ERROR
00988                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00989
00990  350-ERROR-CHECK.
00991      IF EMI-ERROR = ZEROS
00992          NEXT SENTENCE
00993      ELSE
00994          GO TO 8200-SEND-DATAONLY.
00995
00996      IF AACCTL > ZEROS
00997          MOVE AL-UANON           TO AACCTA
00998          MOVE AACCTI             TO PI-SAV-ACCT
00999                                     PI-CR-ACCOUNT
01000      ELSE
01001          MOVE -1                 TO AACCTL
01002          MOVE AL-UABON           TO AACCTA
01003          MOVE ER-0197            TO EMI-ERROR
01004          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01005
01006      IF ACARIERL > ZEROS
01007          MOVE AL-UANON           TO ACARIERA
01008          MOVE ACARIERI           TO PI-SAV-CARR
01009                                     WS-CARRIER
01010      ELSE
01011          IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL
01012              MOVE -1             TO ACARIERL
01013              MOVE AL-UABON       TO ACARIERA
01014              MOVE ER-0194        TO EMI-ERROR
01015              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01016
01017      IF AGROUPL > ZEROS
01018          MOVE AL-UANON           TO AGROUPA
01019          MOVE AGROUPI            TO PI-SAV-GROUP
01020                                     WS-COMP
01021      ELSE
01022          IF CARR-GROUP-ST-ACCNT-CNTL
01023              MOVE -1             TO AGROUPL
01024              MOVE AL-UABON       TO AGROUPA
01025              MOVE ER-0195        TO EMI-ERROR
01026              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01027
01028      IF ASTATEL > ZEROS
01029          MOVE AL-UANON           TO ASTATEA
01030          MOVE ASTATEI            TO PI-SAV-STATE
01031      ELSE
01032          IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL
01033              MOVE -1             TO ASTATEL
01034              MOVE AL-UABON       TO ASTATEA
01035              MOVE ER-0196        TO EMI-ERROR
01036              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01037
01038      MOVE ABILTYPI               TO VALID-BILL-TYPE-VALUES.
01039
01040      IF VALID-BILL-TYPE
01041          MOVE AL-UANON           TO ABILTYPA
01042          MOVE ABILTYPI           TO PI-BILL-TYPE
01043      ELSE
01044          MOVE -1                 TO ABILTYPL
01045          MOVE ER-2249            TO EMI-ERROR
01046          MOVE AL-UABON           TO ABILTYPA
01047          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01048
01049      IF PI-AR-PROCESSING
01050         IF PI-PREVIEW
01051            NEXT SENTENCE
01052         ELSE
01053            MOVE -1               TO ABILTYPL
01054            MOVE ER-3145          TO EMI-ERROR
01055            MOVE AL-UABON         TO ABILTYPA
01056            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01057
01058      IF APRODSWL NOT = ZEROS
01059          IF PI-PREVIEW
01060              IF APRODSWI = 'Y' OR 'N'
01061                  MOVE AL-UANON   TO APRODSWA
01062              ELSE
01063                  MOVE -1         TO APRODSWL
01064                  MOVE ER-2436    TO EMI-ERROR
01065                  MOVE AL-UABON   TO APRODSWA
01066                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01067          ELSE
01068              MOVE -1             TO APRODSWL
01069              MOVE ER-2434        TO EMI-ERROR
01070              MOVE AL-UABON       TO APRODSWA
01071              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01072      ELSE
01073          IF PI-PREVIEW
01074              MOVE -1             TO APRODSWL
01075              MOVE ER-2435        TO EMI-ERROR
01076              MOVE AL-UABON       TO APRODSWA
01077              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01078
01079      IF ABILERRI = 'Y' OR 'N'
01080          MOVE ABILERRI           TO PI-BILL-ERRS
01081          MOVE AL-UANON           TO ABILERRA
01082      ELSE
01083          IF NOT PI-VOID-BILL
01084              MOVE -1             TO ABILERRL
01085              MOVE ER-2250        TO EMI-ERROR
01086              MOVE AL-UABON       TO ABILERRA
01087              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01088
01089      IF ABTCH1L NOT = ZEROS
01090          MOVE AL-UANON           TO ABTCH1A
01091          MOVE 'Y'                TO LIMIT-BILLING-SW
01092          MOVE ABTCH1I            TO PI-BILLING-BATCHES (1).
01093
01094      IF ABTCH2L NOT = ZEROS
01095          MOVE AL-UANON           TO ABTCH2A
01096          MOVE 'Y'                TO LIMIT-BILLING-SW
01097          MOVE ABTCH2I            TO PI-BILLING-BATCHES (2).
01098
01099      IF ABTCH3L NOT = ZEROS
01100          MOVE AL-UANON           TO ABTCH3A
01101          MOVE 'Y'                TO LIMIT-BILLING-SW
01102          MOVE ABTCH3I            TO PI-BILLING-BATCHES (3).
01103
01104      IF EMI-ERROR = ZEROS
01105          GO TO 1000-BILLING-PROCESS.
01106
01107      GO TO 8200-SEND-DATAONLY.
01108      EJECT
01109  0400-CLEAR-PI.
01110      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.
01111
01112      MOVE ZEROS                  TO PI-BAL-FRWD-GA  PI-BAL-FRWD
01113                                     PI-PREMIUM      PI-REMITTED
01114                                     PI-TOT-ISS-COMP
01115                                     PI-TOT-CAN-COMP
01116                                     PI-ADJUSTMENTS  PI-DISBURSED
01117                                     PI-END-BAL      PI-LF-ISS-COMP
01118                                     PI-AH-ISS-COMP  PI-LF-CAN-COMP
01119                                     PI-AH-CAN-COMP.
01120
01121      MOVE SPACES                 TO PI-BATCH (1)
01122                                     PI-BATCH (2)
01123                                     PI-BATCH (3)
01124                                     PI-BATCH (4)
01125                                     PI-BATCH (5)
01126                                     PI-BATCH (6).
01127
01128      MOVE SPACES                 TO PI-BILLING-BATCHES (1)
01129                                     PI-BILLING-BATCHES (2)
01130                                     PI-BILLING-BATCHES (3).
01131
01132      MOVE ZEROS                  TO PI-BILLED (1) PI-PREV (1)
01133                                     PI-NOBILL (1)
01134                                     PI-PREM (1)   PI-COMM (1)
01135                                     PI-NON-PREM (1)
01136                                     PI-NON-COMM (1)
01137                                     PI-BILLED (2) PI-PREV (2)
01138                                     PI-NOBILL (2)
01139                                     PI-PREM (2)   PI-COMM (2)
01140                                     PI-NON-PREM (2)
01141                                     PI-NON-COMM (2)
01142                                     PI-BILLED (3) PI-PREV (3)
01143                                     PI-NOBILL (3)
01144                                     PI-PREM (3)   PI-COMM (3)
01145                                     PI-NON-PREM (3)
01146                                     PI-NON-COMM (3)
01147                                     PI-BILLED (4) PI-PREV (4)
01148                                     PI-NOBILL (4)
01149                                     PI-PREM (4)   PI-COMM (4)
01150                                     PI-NON-PREM (4)
01151                                     PI-NON-COMM (4)
01152                                     PI-BILLED (5) PI-PREV (5)
01153                                     PI-NOBILL (5)
01154                                     PI-PREM (5)   PI-COMM (5)
01155                                     PI-NON-PREM (5)
01156                                     PI-NON-COMM (5)
01157                                     PI-BILLED (6) PI-PREV (6)
01158                                     PI-NOBILL (6)
01159                                     PI-PREM (6)   PI-COMM (6)
01160                                     PI-NON-PREM (6)
01161                                     PI-NON-COMM (6).
01162
01163  0499-EXIT.
01164       EXIT.
01165
01166  0500-CREATE-TEMP-STORAGE.
01167
01168      IF PI-BAL-FRWD NOT NUMERIC
01169          MOVE ZEROS              TO PI-BAL-FRWD
01170                                     PI-PREMIUM
01171                                     PI-REMITTED
01172                                     PI-TOT-ISS-COMP
01173                                     PI-TOT-CAN-COMP
01174                                     PI-ADJUSTMENTS
01175                                     PI-DISBURSED
01176                                     PI-END-BAL.
01177
01178      
      * EXEC CICS WRITEQ TS
01179 *        QUEUE  (QID)
01180 *        FROM   (PROGRAM-INTERFACE-BLOCK)
01181 *        LENGTH (PI-COMM-LENGTH)
01182 *    END-EXEC.
      *    MOVE '*"     L              ''   #00006272' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01183
01184  0590-EXIT.
01185       EXIT.
01186
01187  0600-RECOVER-TEMP-STORAGE.
01188      
      * EXEC CICS READQ TS
01189 *        QUEUE  (QID)
01190 *        INTO   (PROGRAM-INTERFACE-BLOCK)
01191 *        LENGTH (PI-COMM-LENGTH)
01192 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00006282' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01193
01194      PERFORM 0800-DELETE-TS THRU 0890-EXIT.
01195
01196      MOVE PI-CR-CARRIER    TO PI-SCR-CARRIER.
01197      MOVE PI-CR-GROUPING   TO PI-SCR-GROUPING.
01198      MOVE PI-CR-FIN-RESP   TO PI-SCR-FIN-RESP.
01199      MOVE PI-CR-ACCOUNT    TO PI-SCR-ACCOUNT.
01200      MOVE PI-CR-TYPE       TO PI-SCR-TYPE.
01201
01202      MOVE PI-SCRN-CONTROL TO W-TRANSFER-CONTROL.
01203
01204  0690-EXIT.
01205       EXIT.
01206
01207  0800-DELETE-TS.
01208      
      * EXEC CICS HANDLE CONDITION
01209 *        QIDERR (0890-EXIT)
01210 *    END-EXEC.
      *    MOVE '"$N                   ! # #00006302' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036333032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01211
01212      
      * EXEC CICS DELETEQ TS
01213 *        QUEUE  (QID)
01214 *    END-EXEC.
      *    MOVE '*&                    #   #00006306' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01215
01216  0890-EXIT.
01217       EXIT.
01218      EJECT
01219  1000-BILLING-PROCESS.
01220 *                *******************************************
01221 *                *  THIS SECTION PROCESSES THE PENDING     *
01222 *                *  BUSINESS AND PAYMNTS/ADJMNTS FILES.    *
01223 *                *  THE PREMIUM AMT WRITTEN IS EXTRACTED   *
01224 *                *  FROM THE PENDING BUSINESS FILE.        *
01225 *                *******************************************
01226
01227      MOVE PI-COMPANY-CD          TO ERPNDB-CO-CD
01228                                     ERPNDB-CO-CD-A1
01229                                     ERCOMP-COMP-CD
01230                                     ERPYAJ-COMP-CD
01231                                     ERACCT-P-CO-CD
01232                                     ERACCT-A-CO-CD.
01233
01234      MOVE WS-CURRENT-DATE-EDIT   TO BI-HD-RUN-DT.
01235
01236      MOVE ZEROS                  TO PI-BAL-FRWD
01237                                     PI-PREMIUM
01238                                     PI-REMITTED
01239                                     PI-TOT-ISS-COMP
01240                                     PI-TOT-CAN-COMP
01241                                     PI-ADJUSTMENTS
01242                                     PI-DISBURSED
01243                                     PI-END-BAL.
01244
01245      IF PI-VOID-BILL
01246          GO TO 2000-VOID-BILLING.
01247
01248      MOVE SPACES                 TO ELCNTL-KEY.
01249      MOVE '1'                    TO ELCNTL-REC-TYPE.
01250      PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT.
01251
01252      IF EMI-ERROR NOT = ZEROS
01253          GO TO 8200-SEND-DATAONLY.
01254
01255      IF CF-ACCOUNT-MSTR-MAINT-DT      = LOW-VALUES  OR
01256         CF-COMPENSATION-MSTR-MAINT-DT = LOW-VALUES
01257          MOVE ER-2571            TO EMI-ERROR
01258          MOVE -1                 TO APFNTERL
01259          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01260          GO TO 8200-SEND-DATAONLY.
01261
01262      IF CF-FORMS-PRINTER-ID = SPACES
01263         IF ABILTYPI = '3' OR '4'
01264           MOVE 2590               TO EMI-ERROR
01265           MOVE -1                 TO ABILTYPL
01266           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01267           GO TO 8200-SEND-DATAONLY.
01268
01269      MOVE CF-CL-MAIL-TO-NAME     TO BI-HD-CO.
01270      MOVE CF-CR-MONTH-END-DT     TO DC-BIN-DATE-1.
01271      MOVE SPACE                  TO DC-OPTION-CODE.
01272      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01273      MOVE DC-GREG-DATE-1-EDIT    TO PI-MONTH-END-DATE.
01274      MOVE DC-GREG-DATE-1-ALPHA   TO BI-HD-BILL-DT.
01275      MOVE SPACE                  TO PI-DATA-BILLED-SW.
01276
01277      PERFORM 4000-PNDB-START-BROWSE THRU 4090-EXIT.
01278
01279      IF PNDB-EOF
01280          GO TO 1100-PYAJ-BILLING-PROCESS.
01281
01282  1010-PNDB-BILLING-PROCESS-LOOP.
01283      PERFORM 4100-PNDB-READ-NEXT THRU 4190-EXIT.
01284
01285      IF PNDB-EOF
01286          PERFORM 3500-TOTAL-OUT-DETAIL THRU 3599-EXIT
01287          GO TO 1100-PYAJ-BILLING-PROCESS.
01288
01289      IF PB-COMPANY-CD-A1 = PI-COMPANY-CD  AND
01290         PB-CARRIER       = PI-SAV-CARR    AND
01291         PB-GROUPING      = PI-SAV-GROUP   AND
01292         PB-STATE         = PI-SAV-STATE   AND
01293         PB-ACCOUNT       = PI-SAV-ACCT
01294          NEXT SENTENCE
01295      ELSE
01296          PERFORM 3500-TOTAL-OUT-DETAIL THRU 3599-EXIT
01297          GO TO 1100-PYAJ-BILLING-PROCESS.
01298
01299      IF (PB-ISSUE) AND
01300         (PB-I-POLICY-IS-REISSUE OR PB-I-REIN-ONLY OR
122002         PB-I-POLICY-IS-MONTHLY OR
01301          PB-I-POLICY-IS-DECLINED OR PB-I-POLICY-IS-VOIDED OR
01302          PB-I-UNDERWRITE-POLICY)
01303           GO TO 1010-PNDB-BILLING-PROCESS-LOOP
01304      ELSE
01305         IF (PB-CANCELLATION)
01306            AND  (PB-CI-LF-POLICY-IS-REISSUE
01307              OR  PB-CI-AH-POLICY-IS-REISSUE
122002             OR  PB-CI-LF-POLICY-IS-MONTHLY
122002             OR  PB-CI-AH-POLICY-IS-MONTHLY
01308              OR  PB-CI-LF-POLICY-IS-DECLINED
01309              OR  PB-CI-AH-POLICY-IS-VOID
01310              OR  PB-CI-LF-REIN-ONLY
01311              OR  PB-CI-AH-REIN-ONLY)
01312              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01313
01314      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES
01315          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01316
01317      IF PB-RECORD-ON-HOLD OR
01318         PB-RECORD-RETURNED
01319          PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT
01320          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01321
01322      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS
01323          IF PB-ENTRY-REVERSED
01324              GO TO 1010-PNDB-BILLING-PROCESS-LOOP
01325          ELSE
01326              IF PI-REBILLING
01327                  PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT
01328                  GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01329
01330      IF PB-BILLED-DT = LOW-VALUES
01331        OR PI-TOT-REBILL
01332        OR PB-ALT-CHG-SEQ-NO NOT = ZEROS
01333          NEXT SENTENCE
01334      ELSE
01335          IF NOT PB-BATCH-TRAILER
01336              PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT
01337              GO TO 1010-PNDB-BILLING-PROCESS-LOOP
01338          ELSE
01339              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01340
01341      IF LIMIT-BILLING
01342          IF PB-ENTRY-BATCH = ABTCH1I OR
01343                              ABTCH2I OR
01344                              ABTCH3I
01345              NEXT SENTENCE
01346          ELSE
01347              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01348
01349      IF PB-FATAL-ERRORS  OR
01350         PB-UNFORCED-ERRORS
01351          IF ABILERRI = 'N'
01352              PERFORM 8900-SYNCPOINT-ROLLBACK THRU 8900-EXIT
01353              PERFORM 3940-BILL-GENERIC-DELETE THRU 3940-EXIT
01354              MOVE ER-2570        TO EMI-ERROR
01355              MOVE -1             TO ABILERRL
01356              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01357              SET EMI-INDX TO 1
01358              IF EMI-ERROR-NUMBER (EMI-INDX) = ER-2570
01359                  MOVE EMI-ERROR-TEXT (EMI-INDX) TO WS-ERROR-TEXT
01360                  MOVE PB-ENTRY-BATCH            TO WS-ERROR-BATCH
01361                  MOVE WS-ERROR-MSG TO EMI-ERROR-TEXT (EMI-INDX)
01362                  GO TO 8200-SEND-DATAONLY
01363              ELSE
01364                  SET EMI-INDX    TO 2
01365                  MOVE EMI-ERROR-TEXT (EMI-INDX) TO WS-ERROR-TEXT
01366                  MOVE PB-ENTRY-BATCH            TO WS-ERROR-BATCH
01367                  MOVE WS-ERROR-MSG TO EMI-ERROR-TEXT (EMI-INDX)
01368                  GO TO 8200-SEND-DATAONLY
01369          ELSE
01370              PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT
01371              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01372
01373      IF PB-BATCH-TRAILER
01374          IF PI-UPDATE-FILES
01375              PERFORM 1050-BILL-BATCH-TRAILER-CHK THRU 1059-EXIT
01376              IF BILL-BATCH-TRAILER
01377                  MOVE SPACE           TO BILL-BATCH-TRAILER-SW
01378                  MOVE WS-CURRENT-DATE TO PB-BILLED-DT
01379                  PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT
01380                  GO TO 1010-PNDB-BILLING-PROCESS-LOOP
01381              ELSE
01382                  GO TO 1010-PNDB-BILLING-PROCESS-LOOP
01383          ELSE
01384              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01385
01386      MOVE 'Y'                    TO PI-DATA-BILLED-SW.
01387
01388      PERFORM 1300-CHECK-OVERRIDE-PREM THRU 1399-EXIT.
01389
01390      IF PB-CERT-EFF-DT > ERACCT-A-EXP-DATE
01391          MOVE SPACE              TO WS-PROCESS-SW
01392          MOVE PB-CERT-EFF-DT     TO PI-SAV-EXP-DT
01393          PERFORM 4300-READ-ACCOUNT-MASTER  THRU 4390-EXIT
01394          PERFORM 1020-CHK-IF-AGENT-IS-SAME THRU 1029-EXIT
01395          IF EMI-ERROR NOT = ZEROS
01396              IF PI-UPDATE-FILES
01397                OR (PI-PREVIEW AND APRODSWI = 'Y')
01398                  PERFORM 8900-SYNCPOINT-ROLLBACK THRU 8900-EXIT
01399                  IF EMI-ERROR NOT = '2403'
01400                      PERFORM 3940-BILL-GENERIC-DELETE
01401                              THRU 3940-EXIT
01402                      GO TO 8200-SEND-DATAONLY
01403                  ELSE
01404                      GO TO 8200-SEND-DATAONLY
01405              ELSE
01406                  GO TO 8200-SEND-DATAONLY.
01407
01408      PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT.
01409
01410      IF DO-NOT-BILL-THIS-ACCT
01411          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01412
01413      PERFORM 1030-COMPUTE-COMMISSION-TOTALS THRU 1039-EXIT.
01414
01415      IF EMI-ERROR NOT = ZEROS
01416          IF PI-UPDATE-FILES
01417            OR (PI-PREVIEW AND APRODSWI = 'Y')
01418              PERFORM 8900-SYNCPOINT-ROLLBACK THRU 8900-EXIT
01419              PERFORM 3940-BILL-GENERIC-DELETE THRU 3940-EXIT
01420              GO TO 8200-SEND-DATAONLY
01421          ELSE
01422              GO TO 8200-SEND-DATAONLY.
01423
01424      IF PI-UPDATE-FILES
01425        OR (PI-PREVIEW AND APRODSWI = 'Y')
01426          MOVE 'BD'               TO BILLING-DETAIL-TYPE
01427          PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT.
01428
01429      IF PI-BILL OR PI-REBILLING
01430          PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT.
01431
01432      GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01433      EJECT
01434  1020-CHK-IF-AGENT-IS-SAME.
01435      MOVE +1 TO ACCOM-SUB.
01436
01437  1025-FIND-AGENT-LOOP.
052814     IF AM-COM-TYP (ACCOM-SUB) = 'C' OR 'D' OR 'F'
01439          NEXT SENTENCE
01440      ELSE
01441          ADD +1 TO ACCOM-SUB
01442          IF ACCOM-SUB > 10
01443              MOVE -1             TO AACCTL
01444              MOVE ER-2404        TO EMI-ERROR
01445              MOVE AL-UABON       TO AACCTA
01446              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01447          ELSE
01448              GO TO 1025-FIND-AGENT-LOOP.
01449
01450      IF AM-AGT (ACCOM-SUB) NOT = PI-SAV-ACCT
01451          IF FIRST-TIME
01452              MOVE -1             TO AACCTL
01453              MOVE ER-2405        TO EMI-ERROR
01454              MOVE AL-UABON       TO AACCTA
01455              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01456              GO TO 1029-EXIT
01457          ELSE
01458              MOVE 'Y'            TO WS-PROCESS-SW
01459              GO TO 1029-EXIT.
01460
01461      IF FIRST-TIME
01462          MOVE AM-AGT (AM-REMIT-TO) TO PI-SAV-REMIT-TO
01463                                       PI-CR-FIN-RESP
01464          MOVE 'A'                  TO PI-CR-TYPE
01465          PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT
01466          MOVE SPACE                TO FIRST-TIME-SW
01467          IF EMI-ERROR NOT = ZEROS
01468              GO TO 1029-EXIT.
01469
01470      IF AM-AGT (AM-REMIT-TO) = PI-SAV-REMIT-TO
01471          NEXT SENTENCE
01472      ELSE
01473          MOVE 'Y'                TO WS-PROCESS-SW
01474          MOVE ER-2408            TO EMI-ERROR
01475          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01476          PERFORM 8900-SYNCPOINT-ROLLBACK THRU 8900-EXIT
01477          PERFORM 3940-BILL-GENERIC-DELETE  THRU  3940-EXIT
01478          MOVE -1                 TO AACCTL
01479          GO TO 8200-SEND-DATAONLY.
01480
01481      MOVE AM-NAME                TO PI-ACCT-NAME.
01482
01483      IF PI-BILL OR PI-REBILLING
01484          PERFORM 4400-ERACCT-REWRITE THRU 4490-EXIT.
01485
01486  1029-EXIT.
01487      EXIT.
01488      EJECT
01489  1030-COMPUTE-COMMISSION-TOTALS.
01490      IF PB-CANCELLATION
01491          GO TO 1035-COMPUTE-CANCEL-COMM.
01492
032219     MOVE SPACES                 TO WS-AH-benefit-cd-stuff
032219     IF NOT PB-INVALID-AH
032219        MOVE SPACES              TO ELCNTL-KEY
032219        MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
032219        MOVE '5'                 TO ELCNTL-REC-TYPE
032219        MOVE PB-I-AH-BENEFIT-CD  TO ELCNTL-ACCESS (3:2)
032219                                    CLAS-LOOK
032219        MOVE +0                  TO ELCNTL-SEQ-NO
032219        PERFORM 6130-FIND-AH-BENEFIT-CD
032219                                 THRU 6130-EXIT
032219     END-IF
020816     if (pi-company-id = 'DCC' or 'VPP')
032113        and (ws-ah-category = 'G' or 'L')
032113        compute pi-premium = ws-i-ah-premium-amt + pi-premium
032113        move zeros               to ws-i-lf-premium-amt
032113     else
032113        COMPUTE PI-PREMIUM = (WS-I-LF-PREMIUM-AMT +
032113           WS-I-AH-PREMIUM-AMT) + PI-PREMIUM
032113     end-if
01495      COMPUTE PI-LF-ISS-COMP ROUNDED =
01496          (WS-I-LF-PREMIUM-AMT * PB-I-LIFE-COMMISSION).
092705     IF WS-AH-CATEGORY = 'G' OR 'L'
070805        COMPUTE PI-AH-ISS-COMP ROUNDED =
070805           WS-I-AH-PREMIUM-AMT - PB-I-LF-ALT-PREMIUM-AMT
070805              - PB-I-ADDL-CLP
070805     ELSE
01497         COMPUTE PI-AH-ISS-COMP ROUNDED =
01498             (WS-I-AH-PREMIUM-AMT * PB-I-AH-COMMISSION)
070805     END-IF
01499      COMPUTE PI-TOT-ISS-COMP = PI-TOT-ISS-COMP +
01500                                PI-LF-ISS-COMP +
01501                                PI-AH-ISS-COMP.
01502      GO TO 1039-EXIT.
01503
01504  1035-COMPUTE-CANCEL-COMM.
01505
032219     MOVE SPACES                 TO WS-AH-benefit-cd-stuff
032219     if (PB-CI-AH-BENEFIT-CD NOT = SPACES AND ZEROS)
032219        MOVE SPACES              TO ELCNTL-KEY
032219        MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
032219        MOVE '5'                 TO ELCNTL-REC-TYPE
032219        MOVE PB-CI-AH-BENEFIT-CD TO ELCNTL-ACCESS (3:2)
032219                                    CLAS-LOOK
032219        MOVE +0                  TO ELCNTL-SEQ-NO
032219        PERFORM 6130-FIND-AH-BENEFIT-CD
032219                                 THRU 6130-EXIT
032219     END-IF
092705     IF WS-AH-CATEGORY = 'G' OR 'L'
              COMPUTE PI-PREMIUM = PI-PREMIUM -
032113          WS-C-AH-CANCEL-AMT
           ELSE
01506         COMPUTE PI-PREMIUM = PI-PREMIUM -
01507          (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT)
           END-IF
01508
01509      COMPUTE PI-LF-CAN-COMP   ROUNDED =
01510          (WS-C-LF-CANCEL-AMT * PB-CI-LIFE-COMMISSION) * -1.
092705     IF WS-AH-CATEGORY = 'G' OR 'L'
032113        IF AM-DCC-PRODUCT-CODE = 'DDF'
032113           COMPUTE CNC-FACT = PB-C-AH-RFND-CLP /
032113              pb-ci-lf-alt-premium-amt
032113        else
070805           COMPUTE CNC-FACT = PB-C-AH-CANCEL-AMT /
070805              PB-CI-AH-PREMIUM-AMT
032113        end-if
070805        COMPUTE PI-AH-CAN-COMP ROUNDED =
070805           (CNC-FACT * (PB-CI-AH-PREMIUM-AMT -
070805           PB-CI-LF-ALT-PREMIUM-AMT - PB-CI-ADDL-CLP))
070805     ELSE
01512         COMPUTE PI-AH-CAN-COMP   ROUNDED =
01513          (WS-C-AH-CANCEL-AMT * PB-CI-AH-COMMISSION) * -1
01514      END-IF
01515      MOVE PB-CERT-EFF-DT           TO  DC-BIN-DATE-1.
01516      MOVE PB-C-LF-CANCEL-DT        TO  DC-BIN-DATE-2.
01517      MOVE '1'                      TO  DC-OPTION-CODE.
01518      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01519      MOVE DC-ELAPSED-MONTHS        TO  MONTHS-DIFF-LF.
01520
01521      IF DC-ODD-DAYS-OVER > ZEROS
01522          ADD +1 TO MONTHS-DIFF-LF.
01523
01524      MOVE PB-CERT-EFF-DT           TO  DC-BIN-DATE-1.
01525      MOVE PB-C-AH-CANCEL-DT        TO  DC-BIN-DATE-2.
01526      MOVE '1'                      TO  DC-OPTION-CODE.
01527      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01528      MOVE DC-ELAPSED-MONTHS        TO  MONTHS-DIFF-AH.
01529
01530      IF DC-ODD-DAYS-OVER > ZEROS
01531          ADD +1 TO MONTHS-DIFF-AH.
01532
01533      PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT.
01534
01535      MOVE  +0                       TO  SUB.
01536      MOVE  'Y'                      TO  WS-CHARGEBACK-LF-SW
01537                                         WS-CHARGEBACK-AH-SW.
01538
01539      MOVE ZEROS                     TO  WS-LF-CAN-COMP
01540                                         WS-AH-CAN-COMP.
01541  1035-ACCT-COMM-LOOP.
01542
01543      ADD +1 TO SUB.
01544
01545      IF SUB > +10
01546          GO TO 1035-COMPUTE-TOT-CAN.
01547
052814     IF AM-COM-TYP (SUB) NOT = 'C' AND 'D' AND 'F'
01549          GO TO 1035-ACCT-COMM-LOOP.
01550
01551      IF AM-COMM-CHARGEBACK (SUB) NOT NUMERIC
01552          MOVE  ZEROS            TO  AM-COMM-CHARGEBACK (SUB).
01553
01554      IF AM-COMM-CHARGEBACK (SUB) = '99'
01555          MOVE  'N'               TO  WS-CHARGEBACK-LF-SW
01556          GO TO 1035-CHECK-AH-CHARGEBACK.
01557
01558      IF (MONTHS-DIFF-LF > AM-COMM-CHARGEBACK (SUB))
01559                             AND
01560         (AM-COMM-CHARGEBACK (SUB) NOT = ZEROS)
01561          MOVE  'N'               TO  WS-CHARGEBACK-LF-SW
01562      ELSE
01563          COMPUTE WS-LF-CAN-COMP   ROUNDED =  WS-LF-CAN-COMP +
01564             (WS-C-LF-CANCEL-AMT * PB-CI-LIFE-COMMISSION).
01565
01566  1035-CHECK-AH-CHARGEBACK.
01567
01568      IF AM-COMM-CHARGEBACK (SUB) = '99'
01569          MOVE  'N'                TO  WS-CHARGEBACK-AH-SW
01570          GO TO  1035-ACCT-COMM-LOOP.
01571
01572      IF (MONTHS-DIFF-AH > AM-COMM-CHARGEBACK (SUB))
01573                             AND
01574         (AM-COMM-CHARGEBACK (SUB) NOT = ZEROS)
01575          MOVE  'N'                TO  WS-CHARGEBACK-AH-SW
01576      ELSE
092705        IF WS-AH-CATEGORY = 'G' OR 'L'
                 MOVE PI-AH-CAN-COMP   TO WS-AH-CAN-COMP
              ELSE
01577          COMPUTE WS-AH-CAN-COMP ROUNDED = WS-AH-CAN-COMP +
01578             (WS-C-AH-CANCEL-AMT * PB-CI-AH-COMMISSION)
              END-IF
           END-IF
01579
01580      GO TO 1035-ACCT-COMM-LOOP.
01581
01582  1035-COMPUTE-TOT-CAN.
01583
01584      COMPUTE PI-TOT-CAN-COMP = PI-TOT-CAN-COMP +
01585                                WS-LF-CAN-COMP +
01586                                WS-AH-CAN-COMP.
01587
01588  1039-EXIT.
01589      EXIT.
01590
01591      EJECT
01592  1040-UPDATE-BILLING-STATISTICS.
01593      SET PINDX                   TO 1.
01594
01595  1041-SEARCH-LOOP.
01596      IF PI-NON-PREM (PINDX) NOT NUMERIC
01597         MOVE ZEROS               TO PI-NON-PREM (PINDX).
01598      IF WS-NON-PREM  NOT NUMERIC
01599         MOVE ZEROS               TO WS-NON-PREM.
01600      IF PI-NON-COMM (PINDX) NOT NUMERIC
01601         MOVE ZEROS               TO PI-NON-COMM (PINDX).
01602      IF WS-NON-COMM  NOT NUMERIC
01603         MOVE ZEROS               TO WS-NON-COMM.
01604
01605      IF PI-BATCH (PINDX) NOT = SPACES
01606          GO TO 1042-CHECK-FOR-MATCH.
01607
01608      MOVE PB-ENTRY-BATCH         TO PI-BATCH (PINDX).
01609
01610      IF PI-COMPANY-ID = 'DMD'
01611          PERFORM 1060-DMD-ERROR-ROUTINE THRU 1060-EXIT.
01612
01613      IF DO-NOT-BILL-THIS-ACCT OR
01614         PB-FATAL-ERRORS       OR
01615         PB-UNFORCED-ERRORS    OR
01616         PB-RECORD-ON-HOLD     OR
01617         PB-RECORD-RETURNED    OR
01618         PI-VOID-BILL
01619          MOVE +1                 TO PI-NOBILL (PINDX)
01620          MOVE ZEROS              TO PI-BILLED (PINDX)
01621                                     PI-PREV   (PINDX)
01622      ELSE
01623          IF PB-BILLED-DT = LOW-VALUES
01624            OR PI-TOT-REBILL
01625              MOVE +1             TO PI-BILLED (PINDX)
01626              MOVE ZEROS          TO PI-NOBILL (PINDX)
01627                                     PI-PREV   (PINDX)
01628          ELSE
01629              MOVE +1             TO PI-PREV   (PINDX)
01630              MOVE ZEROS          TO PI-BILLED (PINDX)
01631                                     PI-NOBILL (PINDX).
01632
01633      GO TO 1049-EXIT.
01634
01635  1042-CHECK-FOR-MATCH.
01636      IF PI-BATCH (PINDX) = PB-ENTRY-BATCH
01637          NEXT SENTENCE
01638      ELSE
01639          GO TO 1043-SET-INDEX-UP.
01640
01641      IF PI-COMPANY-ID = 'DMD'
01642          PERFORM 1060-DMD-ERROR-ROUTINE THRU 1060-EXIT.
01643
01644      IF DO-NOT-BILL-THIS-ACCT OR
01645         PB-FATAL-ERRORS       OR
01646         PB-UNFORCED-ERRORS    OR
01647         PB-RECORD-ON-HOLD     OR
01648         PB-RECORD-RETURNED    OR
01649         PI-VOID-BILL
01650          ADD +1 TO PI-NOBILL (PINDX)
01651      ELSE
01652          IF PB-BILLED-DT = LOW-VALUES
01653            OR PI-TOT-REBILL
01654              ADD +1          TO PI-BILLED (PINDX)
01655          ELSE
01656              ADD +1          TO PI-PREV   (PINDX).
01657
01658      GO TO 1049-EXIT.
01659
01660  1043-SET-INDEX-UP.
01661      SET PINDX UP BY 1.
01662
01663      IF PINDX > +6
01664          MOVE 'Y'                TO BATCHES-PROCESSED-SW
01665          GO TO 1049-EXIT
01666      ELSE
01667          GO TO 1041-SEARCH-LOOP.
01668
01669  1049-EXIT.
01670      EXIT.
01671
01672      EJECT
01673  1050-BILL-BATCH-TRAILER-CHK.
01674      SET PINDX                   TO 1.
01675
01676  1052-LOOP.
01677      IF PI-BATCH (PINDX) = SPACES
01678          GO TO 1059-EXIT.
01679
01680      IF PB-ENTRY-BATCH = PI-BATCH (PINDX)
01681        AND PI-BILLED (PINDX) NOT = ZEROS
01682          MOVE 'Y'                TO BILL-BATCH-TRAILER-SW
01683          GO TO 1059-EXIT.
01684
01685      SET PINDX UP BY 1.
01686      IF PINDX NOT > 6
01687          GO TO 1052-LOOP.
01688
01689  1059-EXIT.
01690      EXIT.
01691
01692      EJECT
01693  1060-DMD-ERROR-ROUTINE.
01694      IF PB-FATAL-ERRORS OR PB-UNFORCED-ERRORS
01695         IF ABILERRI = 'Y'
01696            IF PB-ISSUE
01697               COMPUTE WS-NON-PREM = PB-I-LF-PREMIUM-AMT
01698                  + PB-I-LF-ALT-PREMIUM-AMT
01699                  + PB-I-AH-PREMIUM-AMT
01700               ADD WS-NON-PREM TO PI-NON-PREM (PINDX)
01701               COMPUTE WS-NON-COMM ROUNDED =
01702               ((PB-I-LF-PREMIUM-AMT + PB-I-LF-ALT-PREMIUM-AMT)
01703                                     * PB-I-LIFE-COMMISSION)
01704                                     +
01705                (PB-I-AH-PREMIUM-AMT * PB-I-AH-COMMISSION)
01706               ADD WS-NON-COMM TO PI-NON-COMM (PINDX)
01707            ELSE
01708            IF PB-CANCELLATION
01709               COMPUTE WS-NON-PREM =
01710               (PB-C-LF-CANCEL-AMT +  PB-C-AH-CANCEL-AMT) * -1
01711               ADD WS-NON-PREM TO PI-NON-PREM (PINDX)
01712               COMPUTE WS-NON-COMM ROUNDED =
01713               ((PB-C-LF-CANCEL-AMT * PB-CI-LIFE-COMMISSION)
01714                                    +
01715                (PB-C-AH-CANCEL-AMT * PB-CI-AH-COMMISSION))
01716                                    * -1
01717               ADD WS-NON-COMM TO PI-NON-COMM (PINDX).
01718
01719  1060-EXIT.
01720      EXIT.
01721
01722      EJECT
01723  1100-PYAJ-BILLING-PROCESS.
01724 *                *******************************************
01725 *                *  PROCESS THE PAYMENTS/ADJUSTMENTS FILE    *
01726 *                *******************************************
01727
01728      IF LIMIT-BILLING AND NOT PI-DATA-BILLED
01729          MOVE ER-2443            TO EMI-ERROR
01730          MOVE -1                 TO ABTCH1L
01731          MOVE AL-UABON           TO ABTCH1A
01732          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01733          GO TO 8200-SEND-DATAONLY.
01734
01735      IF NOT PI-DATA-BILLED
01736          PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT
01737          IF EMI-ERROR = ZEROS
01738              MOVE AM-AGT (AM-REMIT-TO) TO PI-SAV-REMIT-TO
01739                                           PI-CR-FIN-RESP
01740              MOVE AM-REMIT-TO            TO ACCOM-SUB
01741              MOVE 'A'                    TO PI-CR-TYPE
01742              PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT
01743              IF EMI-ERROR NOT = ZEROS
01744                  GO TO 8200-SEND-DATAONLY
01745              ELSE
01746                  NEXT SENTENCE
01747          ELSE
01748              GO TO 8200-SEND-DATAONLY.
01749
01750  1105-PYAJ-BILLING-STARTBR-LOOP.
01751      PERFORM 4500-PYAJ-START-BROWSE THRU 4590-EXIT.
01752
01753      IF PYAJ-EOF
01754          GO TO 1200-PYAJ-BILLING-COMPLETE.
01755
01756  1110-PYAJ-BILLING-READ-LOOP.
01757      PERFORM 4600-PYAJ-READ-NEXT THRU 4690-EXIT.
01758
01759      IF PYAJ-EOF
01760          GO TO 1200-PYAJ-BILLING-COMPLETE.
01761
01762      IF PY-COMPANY-CD = PI-COMPANY-CD      AND
01763         PY-CARRIER    = ERPYAJ-BR-CARRIER  AND
01764         PY-GROUPING   = ERPYAJ-BR-GROUPING AND
01765         PY-FIN-RESP   = PI-SAV-REMIT-TO    AND
01766         PY-ACCOUNT    = PI-SAV-ACCT
01767          NEXT SENTENCE
01768      ELSE
01769          GO TO 1200-PYAJ-BILLING-COMPLETE.
01770
01771      IF PY-CREDIT-ACCEPT-DT NOT = LOW-VALUES
01772          GO TO 1110-PYAJ-BILLING-READ-LOOP.
01773
01774      IF PY-RECORD-TYPE = 'R' OR 'D' OR 'C' OR 'S' OR 'T' OR 'U'
01775                          OR 'Z'
01776          NEXT SENTENCE
01777      ELSE
01778          GO TO 1110-PYAJ-BILLING-READ-LOOP.
01779
CIDMOD     IF PY-VOID-SW NOT = SPACES
CIDMOD         GO TO 1110-PYAJ-BILLING-READ-LOOP.
CIDMOD
01780      IF PI-AR-PROCESSING
01781         IF PY-AR-DATE = LOW-VALUES
01782            OR PI-TOT-REBILL
01783            OR RETURNED-FROM = XCTL-PYAJ
01784            NEXT SENTENCE
01785         ELSE
01786            GO TO 1110-PYAJ-BILLING-READ-LOOP
01787      ELSE
01788         IF (PY-BILLED-DATE = LOW-VALUES AND
01789             PY-AR-DATE = LOW-VALUES)
01790            OR PI-TOT-REBILL
01791            OR RETURNED-FROM = XCTL-PYAJ
01792            NEXT SENTENCE
01793         ELSE
01794            GO TO 1110-PYAJ-BILLING-READ-LOOP.
01795
01796      MOVE 'Y'                    TO PI-DATA-BILLED-SW.
01797
01798      IF PY-REMIT-RECEIVED OR PY-ADJ-REM-RECEIVED OR
01799          PY-DEPOSIT OR PY-ADJ-DEPOSIT OR
01800          PY-ADD-TO-BALANCE
01801          ADD PY-ENTRY-AMT  TO  PI-REMITTED
01802      ELSE
01803          IF (PY-CHARGE-TO-AGENT OR PY-ADJ-CHG-TO-AGT)
01804             AND PY-BILLING-CHECK
01805              ADD PY-ENTRY-AMT TO PI-DISBURSED
01806          ELSE
01807              ADD PY-ENTRY-AMT TO PI-ADJUSTMENTS.
01808
01809      IF RETURNED-FROM = SPACES
01810          IF PI-UPDATE-FILES
01811            OR (PI-PREVIEW AND APRODSWI = 'Y')
01812              MOVE 'BP'           TO BILLING-DETAIL-TYPE
01813              PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT.
01814
01815      IF RETURNED-FROM = SPACES
01816        AND (PI-BILL OR PI-REBILLING)
01817          PERFORM 4800-PYAJ-END-BROWSE
01818          PERFORM 4700-PYAJ-REWRITE THRU 4790-EXIT
01819          ADD +1 TO ERPYAJ-FILE-SEQ-NO
01820          GO TO 1105-PYAJ-BILLING-STARTBR-LOOP.
01821
01822      GO TO 1110-PYAJ-BILLING-READ-LOOP.
01823
01824      EJECT
01825  1200-PYAJ-BILLING-COMPLETE.
01826      IF PI-DATA-BILLED
01827        IF RETURNED-FROM = SPACES
01828          IF PI-UPDATE-FILES
01829             OR (PI-PREVIEW AND APRODSWI = 'Y')
01830                MOVE 'TS'           TO BILLING-DETAIL-TYPE
01831                PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT.
01832
01833      PERFORM 5000-FORMAT-SCREEN THRU 5090-EXIT.
01834
01835      GO TO 8100-SEND-INITIAL-MAP.
01836
01837      EJECT
01838  1300-CHECK-OVERRIDE-PREM.
070805     IF PB-CANCELLATION
01839         IF PB-CI-CANCEL-FEE NOT NUMERIC
01840            MOVE ZEROS  TO  PB-CI-CANCEL-FEE
              END-IF
           END-IF
01841
070805     MOVE ZEROS                  TO WS-I-LF-PREMIUM-AMT
070805                                    WS-I-LF-BENEFIT-AMT
01842      IF PB-OVERRIDE-LIFE  OR
01843         PB-OVERRIDE-BOTH
01844          GO TO 1310-LIFE-OVERRIDE.
01845
01846      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS
01847          IF PB-ISSUE
070805           IF NOT PB-INVALID-LIFE
01848             COMPUTE  WS-I-LF-PREMIUM-AMT =
01849                     (PB-I-LF-PREMIUM-AMT +
01850                      PB-I-LF-ALT-PREMIUM-AMT) * -1
01851             COMPUTE  WS-I-LF-BENEFIT-AMT =
01852                     (PB-I-LF-BENEFIT-AMT +
01853                      PB-I-LF-ALT-BENEFIT-AMT) * -1
070805           END-IF
01854          ELSE
01855              MULTIPLY PB-C-LF-CANCEL-AMT BY -1
01856                             GIVING WS-C-LF-CANCEL-AMT
01857              MULTIPLY PB-CI-LF-BENEFIT-AMT BY -1
01858                             GIVING WS-CI-LIFE-BENEFIT
070805         END-IF
01859      ELSE
01860          IF PB-ISSUE
070805           IF NOT PB-INVALID-LIFE
01861             ADD PB-I-LF-PREMIUM-AMT   PB-I-LF-ALT-PREMIUM-AMT
01862                 GIVING WS-I-LF-PREMIUM-AMT
01863             COMPUTE  WS-I-LF-BENEFIT-AMT =
01864                      PB-I-LF-BENEFIT-AMT +
01865                      PB-I-LF-ALT-BENEFIT-AMT
070805           END-IF
01866          ELSE
01867             MOVE PB-C-LF-CANCEL-AMT   TO WS-C-LF-CANCEL-AMT
01868             MOVE PB-CI-LF-BENEFIT-AMT TO WS-CI-LIFE-BENEFIT
070805         END-IF
070805     END-IF
01869
01870      GO TO 1320-CHECK-AH
01871
           .
01872  1310-LIFE-OVERRIDE.
01873      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS
01874          IF PB-ISSUE
070805           IF NOT PB-INVALID-LIFE
01875             COMPUTE WS-I-LF-PREMIUM-AMT =
01876                    (PB-I-LF-PREM-CALC +
01877                       PB-I-LF-ALT-PREM-CALC) * -1
01878             COMPUTE  WS-I-LF-BENEFIT-AMT =
01879                      (PB-I-LF-BENEFIT-AMT +
01880                       PB-I-LF-ALT-BENEFIT-AMT) * -1
070805           END-IF
01881          ELSE
01882              MULTIPLY PB-C-LF-REF-CALC BY -1
01883                             GIVING WS-C-LF-CANCEL-AMT
01884              MULTIPLY PB-CI-LF-BENEFIT-AMT BY -1
01885                             GIVING WS-CI-LIFE-BENEFIT
070805         END-IF
01886      ELSE
01887          IF PB-ISSUE
070805           IF NOT PB-INVALID-LIFE
01888             ADD PB-I-LF-PREM-CALC   PB-I-LF-ALT-PREM-CALC
01889                 GIVING WS-I-LF-PREMIUM-AMT
01890             COMPUTE  WS-I-LF-BENEFIT-AMT =
01891                      PB-I-LF-BENEFIT-AMT +
01892                      PB-I-LF-ALT-BENEFIT-AMT
070805           END-IF
01893          ELSE
01894             MOVE PB-C-LF-REF-CALC     TO WS-C-LF-CANCEL-AMT
01895             MOVE PB-CI-LF-BENEFIT-AMT TO WS-CI-LIFE-BENEFIT
               END-IF
           END-IF
           .
01897  1320-CHECK-AH.
01898      IF PB-OVERRIDE-AH  OR
01899         PB-OVERRIDE-BOTH
01900          GO TO 1330-AH-OVERRIDE.
01901
01902      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS
01903          IF PB-ISSUE
01904             MULTIPLY PB-I-AH-BENEFIT-AMT BY -1
01905                             GIVING WS-I-AH-BENEFIT-AMT
01906             MULTIPLY PB-I-AH-PREMIUM-AMT BY -1
01907                             GIVING WS-I-AH-PREMIUM-AMT
01908          ELSE
01909              MULTIPLY PB-C-AH-CANCEL-AMT BY -1
01910                             GIVING WS-C-AH-CANCEL-AMT
01911      ELSE
01912          IF PB-ISSUE
01913             MOVE PB-I-AH-BENEFIT-AMT TO WS-I-AH-BENEFIT-AMT
01914             MOVE PB-I-AH-PREMIUM-AMT TO WS-I-AH-PREMIUM-AMT
01915          ELSE
01916             MOVE PB-C-AH-CANCEL-AMT TO WS-C-AH-CANCEL-AMT
01917             IF  PB-CI-CANCEL-FEE > ZEROS
01918                 MOVE PB-CI-CANCEL-FEE     TO WS-C-CANCEL-FEE
01919                 MULTIPLY WS-C-CANCEL-FEE BY -1
01920                                           GIVING WORK-CANCEL-FEE
01921                 ADD WORK-CANCEL-FEE       TO TOT-REPT-CANCEL-FEE.
01922
01923      GO TO 1399-EXIT.
01924
01925  1330-AH-OVERRIDE.
01926      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS
01927          IF PB-ISSUE
01928             MULTIPLY PB-I-AH-BENEFIT-AMT BY -1
01929                             GIVING WS-I-AH-BENEFIT-AMT
01930             MULTIPLY PB-I-AH-PREM-CALC BY -1
01931                             GIVING WS-I-AH-PREMIUM-AMT
01932          ELSE
01933              MULTIPLY PB-C-AH-REF-CALC BY -1
01934                             GIVING WS-C-AH-CANCEL-AMT
01935      ELSE
01936          IF PB-ISSUE
01937             MOVE PB-I-AH-BENEFIT-AMT TO WS-I-AH-BENEFIT-AMT
01938             MOVE PB-I-AH-PREM-CALC   TO WS-I-AH-PREMIUM-AMT
01939          ELSE
01940             MOVE PB-C-AH-REF-CALC    TO WS-C-AH-CANCEL-AMT
01941             IF PB-CI-CANCEL-FEE > ZEROS
01942                 MOVE PB-CI-CANCEL-FEE     TO WS-C-CANCEL-FEE
01943                 MULTIPLY WS-C-CANCEL-FEE BY -1
01944                                           GIVING WORK-CANCEL-FEE
01945                 ADD WORK-CANCEL-FEE       TO TOT-REPT-CANCEL-FEE.
01946
01947  1399-EXIT.
01948      EXIT.
01949
01950      EJECT
01951  2000-VOID-BILLING.
01952
01953 *                *******************************************
01954 *                *  PROCESS VOID BILLING.  THIS SECTION    *
01955 *                *  WILL RESET THE FILES TO THE STATUS     *
01956 *                *  THEY WERE IN PRIOR TO THE LAST BILLING *
01957 *                *******************************************
01958
01959      MOVE SPACES                 TO WS-EXP-DATE
01960                                     WS-REMIT-TO.
01961      SET DTNDX  RTNDX            TO 1.
01962      MOVE SPACES                 TO ELCNTL-KEY.
01963      MOVE '1'                    TO ELCNTL-REC-TYPE.
01964
01965      PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT.
01966
01967      IF EMI-ERROR NOT = ZEROS
01968          GO TO 8200-SEND-DATAONLY.
01969
01970      MOVE CF-CURRENT-MONTH-END   TO DC-BIN-DATE-1.
01971      MOVE SPACE                  TO DC-OPTION-CODE.
01972      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01973      MOVE DC-GREG-DATE-1-EDIT    TO PI-MONTH-END-DATE.
01974
01975      PERFORM 4000-PNDB-START-BROWSE THRU 4090-EXIT.
01976
01977      IF PNDB-EOF
01978          GO TO 2100-VOID-PNDB-COMPLETE.
01979
01980  2010-VOID-PNDB-LOOP.
01981      PERFORM 4100-PNDB-READ-NEXT THRU 4190-EXIT.
01982
01983      IF PNDB-EOF
01984          GO TO 2100-VOID-PNDB-COMPLETE.
01985
01986      IF PB-COMPANY-CD-A1 = PI-COMPANY-CD  AND
01987         PB-CARRIER       = PI-SAV-CARR    AND
01988         PB-GROUPING      = PI-SAV-GROUP   AND
01989         PB-STATE         = PI-SAV-STATE   AND
01990         PB-ACCOUNT       = PI-SAV-ACCT
01991          NEXT SENTENCE
01992      ELSE
01993          GO TO 2100-VOID-PNDB-COMPLETE.
01994
01995      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES
01996          GO TO 2010-VOID-PNDB-LOOP.
01997
01998      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS
01999          MOVE PB-CONTROL-PRIMARY TO ERPNDB-PRIME-KEY
02000          
      * EXEC CICS ENDBR
02001 *             DATASET (ERPNDB-ALT-FILE-ID)
02002 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007175' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02003          
      * EXEC CICS DELETE
02004 *            DATASET (ERPNDB-FILE-ID)
02005 *            RIDFLD  (ERPNDB-PRIME-KEY)
02006 *        END-EXEC
      *    MOVE '&(  R                 &   #00007178' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 ERPNDB-PRIME-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02007          PERFORM 4000-PNDB-START-BROWSE THRU 4090-EXIT
02008          IF PNDB-EOF
02009             GO TO 2100-VOID-PNDB-COMPLETE
02010          ELSE
02011             GO TO 2010-VOID-PNDB-LOOP.
02012
02013      IF PB-BILLED-DT = LOW-VALUES  AND
02014         PB-CHG-COUNT = ZEROS
02015          GO TO 2010-VOID-PNDB-LOOP.
02016
02017      IF PB-BATCH-TRAILER
02018          GO TO 2115-CONT.
02019
02020      IF PB-CERT-EFF-DT > ERACCT-A-EXP-DATE
02021          MOVE PB-CERT-EFF-DT     TO PI-SAV-EXP-DT
02022          PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT
02023          IF AM-AGT (AM-REMIT-TO) NOT = WORK-REMIT-TO
02024              MOVE AM-AGT (AM-REMIT-TO) TO WORK-REMIT-TO
02025                                           WS-SAV-REMIT-TO (RTNDX)
02026              IF RTNDX < 10
02027                  SET RTNDX UP BY 1.
02028
02029  2115-CONT.
02030      IF NOT PB-BATCH-TRAILER
02031          PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT
02032          PERFORM 1300-CHECK-OVERRIDE-PREM       THRU 1399-EXIT
02033          PERFORM 1030-COMPUTE-COMMISSION-TOTALS THRU 1039-EXIT.
02034
02035      PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT.
02036
02037      MOVE 'Y'                    TO DATA-VOIDED-SW.
02038      GO TO 2010-VOID-PNDB-LOOP.
02039
02040  2100-VOID-PNDB-COMPLETE.
02041      SET RTNDX                   TO 1.
02042
02043      IF NOT DATA-VOIDED
02044          SET DTNDX TO RTNDX
02045          PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT
02046          MOVE AM-AGT (AM-REMIT-TO) TO WS-SAV-REMIT-TO (RTNDX).
02047
02048  2105-PYAJ-LOOP.
02049      IF RTNDX > 10
02050        OR WS-SAV-REMIT-TO (RTNDX) = SPACES
02051          GO TO 2105-VOID-CONTINUE.
02052
02053      MOVE WS-SAV-REMIT-TO (RTNDX) TO PI-SAV-REMIT-TO.
02054      PERFORM 2200-VOID-PYAJ THRU 2290-EXIT.
02055
02056      SET RTNDX UP BY 1.
02057      GO TO 2105-PYAJ-LOOP.
02058
02059  2105-VOID-CONTINUE.
02060      IF DATA-VOIDED
02061          NEXT SENTENCE
02062      ELSE
02063          MOVE -1                 TO APFNTERL
02064          MOVE ER-2401            TO EMI-ERROR
02065          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02066          GO TO 8100-SEND-INITIAL-MAP.
02067
02068      SET DTNDX                   TO 1.
02069
02070  2105-VOID-MASTERS-LOOP.
02071      IF WS-SAV-EXP-DT (DTNDX) = SPACES
02072        OR DTNDX > 10
02073          GO TO 2105-VOID-COMPLETE.
02074
02075      MOVE WS-SAV-EXP-DT (DTNDX) TO PI-SAV-EXP-DT.
02076
02077      PERFORM 4400-ERACCT-REWRITE THRU 4490-EXIT.
02078
02079      IF PI-SAV-REMIT-TO NOT = ERCOMP-FIN-RESP
02080          PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT
02081          PERFORM 3930-BILL-READ        THRU 3930-EXIT
02082          IF NOT NO-BILL-RECS
02083              PERFORM 3940-BILL-GENERIC-DELETE THRU 3940-EXIT.
02084
02085      SET DTNDX UP BY 1.
02086
02087      GO TO 2105-VOID-MASTERS-LOOP.
02088
02089  2105-VOID-COMPLETE.
02090      MOVE 0000                   TO EMI-ERROR.
02091      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02092      SET PINDX ANDX TO 1.
02093
02094  2110-FORMAT-LOOP.
02095      IF PI-BATCH (PINDX) NOT = SPACES
02096          MOVE PI-BATCH  (PINDX)  TO BATCH  (ANDX)
02097          MOVE PI-NOBILL (PINDX)  TO NOBILL (ANDX)
02098          MOVE PI-BILLED (PINDX)  TO BILL   (ANDX)
02099          SET ANDX PINDX UP BY 1
02100          IF PINDX > 6
02101              NEXT SENTENCE
02102          ELSE
02103              GO TO 2110-FORMAT-LOOP.
02104
02105      MOVE -1                     TO AACCTL.
02106
02107      GO TO 8100-SEND-INITIAL-MAP.
02108      EJECT
02109  2200-VOID-PYAJ.
02110      PERFORM 4500-PYAJ-START-BROWSE THRU 4590-EXIT.
02111
02112      IF PYAJ-EOF
02113          GO TO 2290-EXIT.
02114
02115  2210-VOID-PYAJ-LOOP.
02116      PERFORM 4600-PYAJ-READ-NEXT THRU 4690-EXIT.
02117
02118      IF PYAJ-EOF
02119          PERFORM 4800-PYAJ-END-BROWSE
02120          GO TO 2290-EXIT.
02121
02122      IF PY-COMPANY-CD = PI-COMPANY-CD      AND
02123         PY-CARRIER    = ERPYAJ-BR-CARRIER  AND
02124         PY-GROUPING   = ERPYAJ-BR-GROUPING AND
02125         PY-FIN-RESP   = PI-SAV-REMIT-TO    AND
02126         PY-ACCOUNT    = PI-SAV-ACCT
02127          NEXT SENTENCE
02128      ELSE
02129          PERFORM 4800-PYAJ-END-BROWSE
02130          GO TO 2290-EXIT.
02131
02132      IF (PY-BILLED-DATE = LOW-VALUES AND
02133             PY-AR-DATE = LOW-VALUES)
02134        OR PY-CREDIT-ACCEPT-DT NOT = LOW-VALUES
02135          GO TO 2210-VOID-PYAJ-LOOP.
02136
02137      PERFORM 4800-PYAJ-END-BROWSE.
02138
02139      PERFORM 4700-PYAJ-REWRITE THRU 4790-EXIT.
02140
02141      MOVE 'Y'                    TO DATA-VOIDED-SW.
02142      ADD +1 TO ERPYAJ-FILE-SEQ-NO.
02143
02144      GO TO 2200-VOID-PYAJ.
02145
02146  2290-EXIT.
02147      EXIT.
02148
02149      EJECT
02150
02151  3000-WRITE-BILLING-DETAIL.
02152
02153 *                *******************************************
02154 *                *  FORMATS THE BILLING DETAIL RECORDS.    *
02155 *                *  THESE RECORDS ARE CREATED IN THE       *
02156 *                *  SAME FORMAT THAT THEY WILL APPEAR      *
02157 *                *  WHEN PRINTED.                          *
02158 *                *******************************************
02159
02160      IF CARRIER-ADDRESS  OR
02161         GEN-AGT-ADDRESS
02162           PERFORM 3930-BILL-READ THRU 3930-EXIT
02163          IF NO-BILL-RECS
02164              GO TO 3200-FORMAT-HDR-ADDR-RECS
02165          ELSE
02166              IF BI-PREVIEW-ONLY
02167                OR BI-INITIAL-PRINT-DATE NOT = LOW-VALUES
02168                OR PI-TOT-REBILL
02169                  PERFORM 3940-BILL-GENERIC-DELETE THRU 3940-EXIT
02170                  GO TO 3200-FORMAT-HDR-ADDR-RECS
02171              ELSE
02172                  MOVE ER-2403    TO EMI-ERROR
02173                  MOVE -1         TO ABILTYPL
02174                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02175                  GO TO 3990-EXIT.
02176
02177      IF TOTAL-STATEMENT
02178          GO TO 3400-FORMAT-TOTAL-LINES.
02179
02180  3100-FORMAT-DETAIL-LINES.
02181      IF PI-PREM (PINDX) NOT NUMERIC
02182         MOVE ZEROS               TO PI-PREM (PINDX).
02183      IF PI-COMM (PINDX) NOT NUMERIC
02184         MOVE ZEROS               TO PI-COMM (PINDX).
02185
02186      IF WS-LINECTR > 23
02187          MOVE ZEROS              TO WS-LINECTR
02188          MOVE '3'                TO BI-RECORD-TYPE
CIDMOD         MOVE TRIPLE-SPACE       TO BI-SKIP-CONTROL
02190          MOVE '          CONTINUED ON NEXT PAGE'
02191                                  TO BI-TEXT-LINE
02192          PERFORM 3920-BILL-WRITE
02193          PERFORM 3300-FORMAT-HEADINGS THRU 3380-EXIT.
02194
02195      IF WS-LINECTR = ZEROS
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL
02197      ELSE
CIDMOD         MOVE SINGLE-SPACE       TO BI-SKIP-CONTROL
CIDMOD     END-IF.
02199
02200      MOVE '3'                    TO BI-RECORD-TYPE.
02201
02202      IF BILLING-DETAIL-TYPE = 'BP'
02203         IF PY-CHARGE-TO-AGENT OR PY-ADJ-CHG-TO-AGT
02204              MOVE PY-ENTRY-COMMENT   TO BI-TEXT-LINE
02205              MULTIPLY PY-ENTRY-AMT   BY -1
02206                               GIVING BI-TOT-PREM
02207              MOVE '* ACCOUNTING ENTRY *'  TO BI-TOT-DESC
02208              ADD +1 TO WS-LINECTR
02209              PERFORM 3920-BILL-WRITE
02210              GO TO 3990-EXIT.
02211
02212      IF BILLING-DETAIL-TYPE = 'BP'
02213          MOVE PY-ENTRY-COMMENT   TO BI-TEXT-LINE
02214          MOVE PY-ENTRY-AMT       TO BI-TOT-PREM
02215          MOVE '* ACCOUNTING ENTRY *'  TO BI-TOT-DESC
02216          ADD +1 TO WS-LINECTR
02217          PERFORM 3920-BILL-WRITE
02218          GO TO 3990-EXIT.
02219
02220      MOVE SPACES                 TO BI-TEXT-LINE.
02221
02222      IF PB-ISSUE
02223         NEXT SENTENCE
02224      ELSE
02225         GO TO 3110-FORMAT-DETAIL-CANCEL.
02226
02227      MOVE PB-I-INSURED-FIRST-NAME    TO BI-INS-1ST-NAME.
02228      MOVE PB-I-INSURED-MIDDLE-INIT   TO BI-INS-INIT.
02233
02234      MOVE PB-I-INSURED-LAST-NAME     TO BI-INS-LAST-NAME.
02235      MOVE PB-CERT-NO                 TO BI-CERT.
02236      MOVE PB-I-LF-TERM               TO BI-ED-TERM  BI-TERM.
02237      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.
02238      MOVE SPACE                      TO DC-OPTION-CODE.
02239      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
02240      MOVE DC-GREG-DATE-1-EDIT        TO BI-EFF-DT.
02241
02242      IF PI-COMPANY-ID = 'DMD' AND
02243         PB-ISSUE  AND
02244         NOT PB-INVALID-LIFE
02245           MOVE PB-I-LF-BENEFIT-CD    TO WS-BENE-CD
02246           MOVE WS-BENEFIT            TO PB-I-LF-ABBR.
032219     move spaces                 to ws-lf-benefit-cd-stuff
032219                                    bi-type
032219     if not pb-invalid-life
032219        move spaces              to bi-type
032219        MOVE SPACES              TO ELCNTL-KEY
032219        MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
032219        MOVE '4'                 TO ELCNTL-REC-TYPE
032219        MOVE PB-I-LF-BENEFIT-CD  TO ELCNTL-ACCESS (3:2)
032219                                    CLAS-LOOK
032219        MOVE +0                  TO ELCNTL-SEQ-NO
032219        perform 6135-find-lf-benefit-cd
032219                                 thru 6135-exit
032219        if ws-lf-alpha <> spaces
032219           if ws-lf-joint-ind = 'J'
032219              move 'JOINT LIFE'  to bi-type
032219           else
032219              move 'SINGLE LIFE' to bi-type
032219           end-if
032219        end-if
032219     end-if
02248      IF NOT PB-INVALID-LIFE
              if bi-type = spaces
                 move pb-i-lf-abbr         to bi-type
              end-if
02250         MOVE WS-I-LF-PREMIUM-AMT     TO BI-PREM
02251                                         BI-PREMIUM-AMT
02252         ADD WS-I-LF-PREMIUM-AMT      TO TOT-REPT-LF-PREM
02253         MULTIPLY PB-I-LIFE-COMMISSION BY 100 GIVING BI-ED-RATE
02254         MOVE PB-I-LIFE-COMMISSION    TO BI-RATE
02255         MOVE PI-LF-ISS-COMP          TO BI-COMM
02256         ADD  PI-LF-ISS-COMP          TO TOT-REPT-LF-COMP
02257         MOVE WS-I-LF-BENEFIT-AMT     TO BI-FACE-AMT
02258                                         BI-BENEFIT-AMT
02259         ADD WS-I-LF-BENEFIT-AMT      TO TOT-LF-REPT-FACE-AMT
02260         ADD WS-I-LF-PREMIUM-AMT      TO  PI-PREM (PINDX)
02261         ADD PI-LF-ISS-COMP           TO  PI-COMM (PINDX)
02262         PERFORM 3920-BILL-WRITE
02263         ADD +1 TO WS-LINECTR
CIDMOD        MOVE SINGLE-SPACE            TO BI-SKIP-CONTROL
02265         MOVE SPACES                  TO BI-TEXT-LINE.
02266
02267      IF PI-COMPANY-ID = 'DMD' AND
02268         PB-ISSUE  AND
02269         NOT PB-INVALID-AH
02270           MOVE PB-I-AH-BENEFIT-CD    TO WS-BENE-CD
02271           MOVE WS-BENEFIT            TO PB-I-AH-ABBR.
02272
02273      IF NOT PB-INVALID-AH
032219        if ws-ah-alpha <> spaces
032219           if ws-ah-joint-ind = 'J'
032219              move 'JOINT DISABILITY'
032219                                 to bi-type
032219           else
032219              move 'SINGLE DISABILITY'
032219                                 to bi-type
032219           end-if
032219        else
032219           MOVE PB-I-AH-ABBR         TO BI-TYPE
032219        end-if
02275         MOVE WS-I-AH-PREMIUM-AMT     TO BI-PREM
02276                                         BI-PREMIUM-AMT
02277         ADD WS-I-AH-PREMIUM-AMT      TO TOT-REPT-AH-PREM
02278         MULTIPLY PB-I-AH-COMMISSION BY 100 GIVING BI-ED-RATE
02279         MOVE PB-I-AH-COMMISSION      TO BI-RATE
02280         MOVE PI-AH-ISS-COMP          TO BI-COMM
02281         ADD PI-AH-ISS-COMP           TO TOT-REPT-AH-COMP
02282         MOVE WS-I-AH-BENEFIT-AMT     TO BI-FACE-AMT
02283         ADD WS-I-AH-BENEFIT-AMT      TO TOT-AH-REPT-FACE-AMT
02284         ADD WS-I-AH-PREMIUM-AMT      TO  PI-PREM (PINDX)
02285         ADD PI-AH-ISS-COMP           TO  PI-COMM (PINDX)
02286         PERFORM 3920-BILL-WRITE
CIDMOD        MOVE SINGLE-SPACE            TO BI-SKIP-CONTROL
02288         ADD +1                       TO WS-LINECTR.
02289
02290      GO TO 3990-EXIT.
02291
02292  3110-FORMAT-DETAIL-CANCEL.
02293      MOVE PB-CI-LAST-NAME            TO BI-INS-LAST-NAME.
02294      MOVE PB-CI-INITIALS             TO BI-INS-INITS.
02295
02300      MOVE PB-CERT-NO                 TO BI-CERT.
02301      MOVE PB-CI-LF-TERM              TO BI-ED-TERM
02302                                         BI-TERM.
02303      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.
02304      MOVE SPACE                      TO DC-OPTION-CODE.
02305      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
02306      MOVE DC-GREG-DATE-1-EDIT        TO BI-EFF-DT.
02307
02308      IF PI-COMPANY-ID = 'DMD' AND
02309         PB-CANCELLATION AND
02310         PB-CI-LF-ABBR > SPACES
02311           MOVE PB-CI-LF-BENEFIT-CD   TO WS-BENE-CD
02312           MOVE WS-BENEFIT            TO PB-CI-LF-ABBR.
032219     move spaces                 to bi-type
032219     if pb-ci-lf-benefit-cd <> '  ' and '00'
032219        move spaces              to bi-type
032219        MOVE SPACES              TO ELCNTL-KEY
032219        MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
032219        MOVE '4'                 TO ELCNTL-REC-TYPE
032219        MOVE PB-ci-lf-benefit-cd TO ELCNTL-ACCESS (3:2)
032219                                    CLAS-LOOK
032219        MOVE +0                  TO ELCNTL-SEQ-NO
032219        perform 6135-find-lf-benefit-cd
032219                                 thru 6135-exit
032219        if ws-lf-alpha not = spaces
032219           if ws-lf-joint-ind = 'J'
032219              move 'JOINT LIFE'  to bi-type
032219           else
032219              move 'SINGLE LIFE' to bi-type
032219           end-if
032219        end-if
032219     end-if
02314      IF PB-CI-LF-ABBR > SPACES
02315          MOVE PB-C-LF-CANCEL-DT      TO DC-BIN-DATE-1
02316          MOVE SPACE                  TO DC-OPTION-CODE
02317          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
02318          MOVE DC-GREG-DATE-1-EDIT    TO BI-CAN-DT
032219         if bi-type = spaces
02319             MOVE PB-CI-LF-ABBR       TO BI-TYPE
032219         end-if
02320          MOVE WS-C-LF-CANCEL-AMT     TO BI-PREMIUM-AMT
02321          MULTIPLY WS-C-LF-CANCEL-AMT BY -1 GIVING WORK-AMT
02322          MOVE WORK-AMT               TO BI-PREM
CIDMOD         ADD WS-C-LF-CANCEL-AMT      TO TOT-REPT-LF-REF
02323          ADD WORK-AMT                TO TOT-REPT-LF-PREM
02324                                         PI-PREM (PINDX)
02325          MULTIPLY PB-CI-LIFE-COMMISSION
02326                                      BY 100 GIVING BI-ED-RATE
02327          MOVE PB-CI-LIFE-COMMISSION  TO BI-RATE
02328          IF WS-CHARGEBACK-LF-SW = 'N'
02329              MOVE  ZEROS                 TO PI-LF-CAN-COMP
02330              MOVE PI-LF-CAN-COMP         TO BI-COMM
02331              ADD PI-LF-CAN-COMP          TO TOT-REPT-LF-COMP
02332                                             PI-COMM (PINDX)
CIDMOD             COMPUTE TOT-REPT-LF-COMP-R =
CIDMOD               TOT-REPT-LF-COMP-R + (PI-LF-CAN-COMP * -1)
02333              MOVE WS-CI-LIFE-BENEFIT     TO BI-BENEFIT-AMT
02334              ADD +1                      TO WS-LINECTR
02335              PERFORM 3920-BILL-WRITE
02336              MOVE SPACES                 TO BI-TEXT-LINE
02337          ELSE
02338              MOVE PI-LF-CAN-COMP         TO BI-COMM
02339              ADD PI-LF-CAN-COMP          TO TOT-REPT-LF-COMP
02340                                             PI-COMM (PINDX)
CIDMOD             COMPUTE TOT-REPT-LF-COMP-R =
CIDMOD               TOT-REPT-LF-COMP-R + (PI-LF-CAN-COMP * -1)
02341              MOVE  ZEROS                 TO PI-LF-CAN-COMP
02342              MOVE WS-CI-LIFE-BENEFIT     TO BI-BENEFIT-AMT
02343              ADD +1                      TO WS-LINECTR
02344              PERFORM 3920-BILL-WRITE
02345              MOVE SPACES                 TO BI-TEXT-LINE.
02346
02347      IF PI-COMPANY-ID = 'DMD' AND
02348         PB-CANCELLATION AND
02349         PB-CI-AH-ABBR > SPACES
02350           MOVE PB-CI-AH-BENEFIT-CD   TO WS-BENE-CD
02351           MOVE WS-BENEFIT            TO PB-CI-AH-ABBR.
02352
02353      IF PB-CI-AH-ABBR > SPACES
032219        if ws-ah-alpha not = spaces
032219           if ws-ah-joint-ind = 'J'
032219              move 'JOINT DISABILITY'
032219                                 to bi-type
032219           else
032219              move 'SINGLE DISABILITY'
032219                                 to bi-type
032219           end-if
032219        else
032219           move pb-ci-ah-abbr    to bi-type
032219        end-if
02354          MOVE PB-C-AH-CANCEL-DT      TO DC-BIN-DATE-1
02355          MOVE SPACE                  TO DC-OPTION-CODE
02356          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
02357          MOVE DC-GREG-DATE-1-EDIT    TO BI-CAN-DT
02359          MOVE WS-C-AH-CANCEL-AMT     TO BI-PREMIUM-AMT
02360          MULTIPLY WS-C-AH-CANCEL-AMT BY -1 GIVING WORK-AMT
02361          MOVE WORK-AMT               TO BI-PREM
CIDMOD         ADD WS-C-AH-CANCEL-AMT      TO TOT-REPT-AH-REF
02362          ADD WORK-AMT                TO TOT-REPT-AH-PREM
02363                                         PI-PREM (PINDX)
02364          MULTIPLY PB-CI-AH-COMMISSION BY 100 GIVING BI-ED-RATE
02365          MOVE PB-CI-AH-COMMISSION    TO BI-RATE
02366          IF WS-CHARGEBACK-AH-SW = 'N'
02367              MOVE  ZEROS                 TO PI-AH-CAN-COMP
02368              MOVE PI-AH-CAN-COMP         TO BI-COMM
02369              ADD PI-AH-CAN-COMP          TO TOT-REPT-AH-COMP
02370                                             PI-COMM (PINDX)
CIDMOD             COMPUTE TOT-REPT-AH-COMP-R =
CIDMOD               TOT-REPT-AH-COMP-R + (PI-AH-CAN-COMP * -1)
02371              ADD +1                      TO WS-LINECTR
02372              PERFORM 3920-BILL-WRITE
02373          ELSE
02374              MOVE PI-AH-CAN-COMP         TO BI-COMM
02375              ADD PI-AH-CAN-COMP          TO TOT-REPT-AH-COMP
02376                                             PI-COMM (PINDX)
CIDMOD             COMPUTE TOT-REPT-AH-COMP-R =
CIDMOD               TOT-REPT-AH-COMP-R + (PI-AH-CAN-COMP * -1)
02377              MOVE  ZEROS                 TO PI-AH-CAN-COMP
02378              ADD +1                      TO WS-LINECTR
02379              PERFORM 3920-BILL-WRITE.
02380
02381      IF PB-CI-CANCEL-FEE NOT NUMERIC
02382          MOVE ZEROS                      TO PB-CI-CANCEL-FEE.
02383
02384      IF PB-CI-CANCEL-FEE > ZEROS
02385          MOVE SPACES                 TO BI-INS-LAST-NAME
02386                                         BI-INS-INITS
02387                                         BI-EFF-DT
02388                                         BI-CAN-DT
02389                                         BI-TYPE
02390          MOVE 'CANCEL FEE'           TO BI-CERT
02391          MOVE WORK-CANCEL-FEE        TO BI-PREM
02392                                         BI-COMM
02393          MOVE ZEROS                  TO BI-PREMIUM-AMT
02394                                         BI-RATE
02395                                         BI-ED-RATE
02396                                         BI-ED-TERM
02397                                         BI-TERM
02398          ADD +1                      TO WS-LINECTR
02399          PERFORM 3920-BILL-WRITE.
02400
02401      GO TO 3990-EXIT.
02402
02403      EJECT
02404
02405  3200-FORMAT-HDR-ADDR-RECS.
02406      PERFORM 3910-BILL-GETMAIN.
02407
02408      MOVE ZEROS                  TO WS-LINE-SEQ-NO.
02409      SET A-INDX  R-INDX          TO 1.
02410      PERFORM 3210-FORMAT-ADDR-RECS  THRU 3210-EXIT 6 TIMES.
02411      MOVE ZEROS                  TO WS-LINE-SEQ-NO.
02412      PERFORM 3300-FORMAT-HEADINGS THRU 3380-EXIT.
02413      GO TO 3990-EXIT.
02414
02415  3210-FORMAT-ADDR-RECS.
02416      MOVE '2'                     TO BI-RECORD-TYPE.
02417      MOVE WS-ACCT-LINES (A-INDX)  TO BI-ACCT-ADDRESS-LINE.
02418      MOVE WS-REMIT-LINES (R-INDX) TO BI-REMIT-ADDRESS-LINE.
02419      PERFORM 3920-BILL-WRITE.
02420      SET A-INDX  R-INDX         UP BY 1.
02421
02422  3210-EXIT.
02423      EXIT.
02424
02425      EJECT
02426  3300-FORMAT-HEADINGS.
02427      MOVE WS-PGECTR              TO BI-HD-PG.
02428      ADD +1                      TO WS-PGECTR.
02429      MOVE '3'                    TO BI-RECORD-TYPE.
CIDMOD     MOVE TOP-OF-PAGE            TO BI-SKIP-CONTROL.
02431
02432      IF NOT PI-PREVIEW
02433          MOVE BI-HD1             TO BI-TEXT-LINE
02434      ELSE
02435          MOVE BI-PREVIEW-HD      TO BI-TEXT-LINE
02436          PERFORM 3920-BILL-WRITE
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL
02438          MOVE BI-HD1             TO BI-TEXT-LINE.
02439
02440      PERFORM 3920-BILL-WRITE.
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
02442      MOVE BI-HD2                 TO BI-TEXT-LINE.
02443      PERFORM 3920-BILL-WRITE.
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
02445      MOVE BI-HD3                 TO BI-TEXT-LINE.
02446      PERFORM 3920-BILL-WRITE.
02447      SET A-INDX  R-INDX          TO 1.
02448      PERFORM 3390-FORMAT-ADDR-TEXT-LINES THRU 3390-EXIT 5 TIMES.
02449      MOVE '3'                    TO BI-RECORD-TYPE.
CIDMOD     MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.
02451      MOVE BI-HD4                 TO BI-TEXT-LINE.
02452      PERFORM 3920-BILL-WRITE.
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
02454      MOVE BI-HD5                 TO BI-TEXT-LINE.
02455      PERFORM 3920-BILL-WRITE.
02456
02457  3380-EXIT.
02458      EXIT.
02459
02460  3390-FORMAT-ADDR-TEXT-LINES.
02461      MOVE '3'                    TO BI-RECORD-TYPE.
02462      MOVE SPACES                 TO BI-TEXT-LINE.
02463
02464      IF A-INDX = 1
02465          MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL
02466          MOVE 'ACCOUNT NO. - '   TO BI-ADDR-LIT
02467          MOVE WS-CARR-COMP       TO BI-CO
02468          MOVE '-'                TO BI-DASH
02469          MOVE PI-SAV-ACCT        TO BI-ACCT
02470          MOVE 'REMIT TO - '      TO BI-REMIT-LIT
02471      ELSE
02472          MOVE SINGLE-SPACE       TO BI-SKIP-CONTROL.
02473
02474      MOVE WS-ACCT-LINES (A-INDX)  TO BI-ACCT-ADDR.
02475      MOVE WS-REMIT-LINES (R-INDX) TO BI-REMIT-ADDR.
02476      PERFORM 3920-BILL-WRITE.
02477      SET A-INDX  R-INDX  UP BY 1.
02478
02479  3390-EXIT.
02480      EXIT.
02481
02482      EJECT
02483  3400-FORMAT-TOTAL-LINES.
02484      COMPUTE PI-END-BAL = PI-BAL-FRWD +
02485                           PI-PREMIUM +
02486                           TOT-REPT-CANCEL-FEE -
02487                           PI-REMITTED -
02488                           PI-TOT-ISS-COMP +
02489                           PI-TOT-CAN-COMP +
02490                           PI-ADJUSTMENTS +
02491                           PI-DISBURSED.
02492
02493      MOVE SPACES                 TO BI-TEXT-LINE.
02494
02495      MOVE '1'                    TO BI-RECORD-TYPE.
02496      MOVE PI-PROCESSOR-ID        TO BI-PROCESSOR-CD.
02497
02498      IF PI-PREV-BILL OR PI-PREV-REBILL
02499          MOVE 'P'                TO BI-STATEMENT-TYPE.
02500
02501      MOVE +1                     TO BI-NO-OF-COPIES.
02502      MOVE WS-CURRENT-DATE        TO BI-CREATION-DT.
02503      MOVE LOW-VALUES             TO BI-INITIAL-PRINT-DATE.
02504      MOVE PI-BAL-FRWD            TO BI-BAL-FRWD.
02505      MOVE PI-PREMIUM             TO BI-PREMIUM.
02506      MOVE PI-REMITTED            TO BI-REMITTED.
02507      MOVE PI-TOT-ISS-COMP        TO BI-TOT-ISS-COMP.
02508      MOVE PI-TOT-CAN-COMP        TO BI-TOT-CAN-COMP.
02509      MOVE PI-ADJUSTMENTS         TO BI-ADJUSTMNTS.
02510      MOVE PI-DISBURSED           TO BI-DISBURSED.
02511      MOVE PI-END-BAL             TO BI-END-BAL.
02512
02513      IF PI-SAV-ACCT = PI-SAV-REMIT-TO
02514          MOVE WS-ACCT-LINES (1)  TO BI-FIN-RESP-NAME
02515      ELSE
02516          MOVE WS-REMIT-LINES (1) TO BI-FIN-RESP-NAME.
02517
02518      PERFORM 3920-BILL-WRITE.
02519
02520      IF WS-LINECTR > 16
02521          MOVE ZEROS              TO WS-LINECTR
02522          MOVE '3'                TO BI-RECORD-TYPE
CIDMOD         MOVE TRIPLE-SPACE       TO BI-SKIP-CONTROL
02524          MOVE '          CONTINUED ON NEXT PAGE'
02525                                  TO BI-TEXT-LINE
02526          PERFORM 3920-BILL-WRITE
02527          PERFORM 3300-FORMAT-HEADINGS THRU 3380-EXIT.
02528
02529      IF PI-BAL-FRWD NOT = ZERO
02530          MOVE SPACES             TO BI-TEXT-LINE
02531          MOVE '3'                TO BI-RECORD-TYPE
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL
02533          MOVE 'BALANCE FORWARD'  TO BI-TOT-DESC
02534          MOVE PI-BAL-FRWD        TO BI-TOT-PREM
02535          PERFORM 3920-BILL-WRITE.
02536
02537      IF PI-PREMIUM NOT = ZERO
02538          MOVE SPACES             TO BI-TEXT-LINE
02539          MOVE '3'                TO BI-RECORD-TYPE
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL
02541          MOVE 'THIS MONTHS PREMIUM'  TO BI-TOT-DESC
02542          COMPUTE WORK-AMT  =  TOT-REPT-CANCEL-FEE + PI-PREMIUM
02543          MOVE WORK-AMT           TO BI-TOT-PREM
02544          PERFORM 3920-BILL-WRITE.
02545
02546      IF PI-TOT-ISS-COMP NOT = ZEROS  OR
02547         PI-TOT-CAN-COMP NOT = ZEROS
02548          MOVE SPACES             TO BI-TEXT-LINE
02549          MOVE '3'                TO BI-RECORD-TYPE
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL
02551          MOVE 'LESS COMPENSATION' TO BI-TOT-DESC
02552          SUBTRACT PI-TOT-CAN-COMP FROM PI-TOT-ISS-COMP
02553                           GIVING BI-TOT-PREM
02554          PERFORM 3920-BILL-WRITE.
02555
02556      IF PI-REMITTED  NOT = ZEROS  OR
02557         PI-DISBURSED NOT = ZEROS
02558          MOVE SPACES             TO BI-TEXT-LINE
02559          MOVE '3'                TO BI-RECORD-TYPE
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL
02561          MOVE 'LESS PAYMENTS'    TO BI-TOT-DESC
02562          IF PI-COMPANY-ID = 'UCL'
02563              SUBTRACT PI-DISBURSED FROM PI-REMITTED
02564                       GIVING BI-TOT-PREM
02565              PERFORM 3920-BILL-WRITE
02566            ELSE
02567              MOVE PI-REMITTED        TO BI-TOT-PREM
02568              PERFORM 3920-BILL-WRITE.
02569
02570      IF PI-ADJUSTMENTS NOT = ZERO
02571          MOVE SPACES             TO BI-TEXT-LINE
02572          MOVE '3'                TO BI-RECORD-TYPE
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL
02574          MOVE 'ADJUSTMENT'       TO BI-TOT-DESC
02575          ADD PI-ADJUSTMENTS, PI-DISBURSED GIVING BI-TOT-PREM
02576          PERFORM 3920-BILL-WRITE.
02577
02578      MOVE SPACES                 TO BI-TEXT-LINE.
02579      MOVE '3'                    TO BI-RECORD-TYPE.
CIDMOD     MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.
02581      MOVE ALL '-'                TO BI-TOT-DASH.
02582      PERFORM 3920-BILL-WRITE.
02583
02584      MOVE SPACES                 TO BI-TEXT-LINE.
02585      MOVE '3'                    TO BI-RECORD-TYPE.
CIDMOD     MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.
02587
02588      IF PI-END-BAL < +1.00 AND > -1.00
02589          NEXT SENTENCE
02590      ELSE
02591          IF PI-END-BAL < +0
02592              MOVE 'WE WILL REFUND' TO BI-TOT-DESC
02593          ELSE
02594              MOVE 'PLEASE REMIT' TO BI-TOT-DESC.
02595
02596      MOVE PI-END-BAL             TO BI-TOT-PREM.
02597
02598      PERFORM 3920-BILL-WRITE.
02599
02600      IF CO-CARRY-BALANCE
02601          IF PI-END-BAL > +1.00
02602              MOVE SPACES         TO BI-TEXT-LINE
02603              MOVE '3'            TO BI-RECORD-TYPE
CIDMOD             MOVE DOUBLE-SPACE   TO BI-SKIP-CONTROL
02605              MOVE 'PLEASE RETURN ONE COPY WITH REMITTANCE'
02606                                  TO BI-TEXT-LINE
02607              PERFORM 3920-BILL-WRITE.
02608
02609      IF PI-COMPANY-ID = 'DMD'
02610         MOVE SPACES                 TO BI-TEXT-LINE
02611         MOVE '3'                    TO BI-RECORD-TYPE
02612         MOVE '0'                    TO BI-SKIP-CONTROL
02613         MOVE BI-HD6                 TO BI-TEXT-LINE
02614         PERFORM 3920-BILL-WRITE
02615         MOVE SPACES                 TO BI-TEXT-LINE
02616         MOVE ' '                    TO BI-SKIP-CONTROL
02617         MOVE BI-HD7                 TO BI-TEXT-LINE
02618         PERFORM 3920-BILL-WRITE
02619
02620         PERFORM 3600-FIND-BATCH-NUMBER THRU 3699-EXIT
02621           VARYING PINDX FROM 1 BY 1
02622             UNTIL PINDX > +6.
02623
02624      MOVE ZEROS                  TO PI-PREM     (PINDX)
02625                                     PI-COMM     (PINDX)
02626                                     PI-NON-PREM (PINDX)
02627                                     PI-NON-COMM (PINDX).
02628
02629      GO TO 3990-EXIT.
02630      EJECT
02631
02632  3500-TOTAL-OUT-DETAIL.
02633      IF PI-DATA-BILLED AND
02634         RETURNED-FROM = SPACES
02635            NEXT SENTENCE
02636        ELSE
02637            GO TO 3599-EXIT.
02638
02639      IF PI-UPDATE-FILES  OR
02640         (PI-PREVIEW AND APRODSWI = 'Y')
02641            NEXT SENTENCE
02642         ELSE
02643            GO TO 3599-EXIT.
02644
02645      MOVE SPACES                 TO BI-TEXT-LINE.
02646 *    MOVE '3'                    TO BI-RECORD-TYPE.
CIDMOD*    MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.
02648 *    MOVE 'TOTAL'                TO BI-TOT-LIT.
02649 *    MOVE PI-LIFE-OVERRIDE-L6    TO BI-OVERRIDE-L6.
02650 *    MOVE TOT-REPT-LF-PREM       TO BI-TOT-PREM.
02651 *    MOVE TOT-REPT-LF-COMP       TO BI-COM-TOT.
02652 *    MOVE TOT-LF-REPT-FACE-AMT   TO BI-FACE-TOT.
02653 *    PERFORM 3920-BILL-WRITE.
02654 *
02655 *    MOVE SPACES                 TO BI-TEXT-LINE.
CIDMOD*    MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
02657 *    MOVE 'TOTAL'                TO BI-TOT-LIT.
02658 *    MOVE PI-AH-OVERRIDE-L6      TO BI-OVERRIDE-L6.
02659 *    MOVE TOT-REPT-AH-PREM       TO BI-TOT-PREM
02660 *    MOVE TOT-REPT-AH-COMP       TO BI-COM-TOT.
02661 *    MOVE TOT-AH-REPT-FACE-AMT   TO BI-FACE-TOT.
02662 *    PERFORM 3920-BILL-WRITE.
02663
02645      MOVE SPACES                 TO BI-TEXT-LINE.
02646      MOVE '3'                    TO BI-RECORD-TYPE.
CIDMOD     MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.
CIDMOD     MOVE 'TOTAL PREM '          TO BI-TOT-DESC
CIDMOD     MOVE PI-LIFE-OVERRIDE-L6    TO BI-TOT-DESC (12:6)
CIDMOD     ADD TOT-REPT-LF-REF TOT-REPT-LF-PREM
CIDMOD          GIVING BI-TOT-PREM
CIDMOD     ADD TOT-REPT-LF-COMP TOT-REPT-LF-COMP-R
CIDMOD          GIVING BI-COM-TOT
CIDMOD*    MOVE TOT-REPT-LF-COMP       TO BI-COM-TOT.
CIDMOD*    MOVE TOT-LF-REPT-FACE-AMT   TO BI-FACE-TOT.
02653      PERFORM 3920-BILL-WRITE.
02654
02645      MOVE SPACES                 TO BI-TEXT-LINE.
02646      MOVE '3'                    TO BI-RECORD-TYPE.
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
CIDMOD     MOVE 'TOTAL REF  '          TO BI-TOT-DESC
CIDMOD     MOVE PI-LIFE-OVERRIDE-L6    TO BI-TOT-DESC (12:6)
CIDMOD     MOVE TOT-REPT-LF-REF        TO BI-TOT-PREM.
CIDMOD     MOVE TOT-REPT-LF-COMP-R     TO BI-COM-TOT
02653      PERFORM 3920-BILL-WRITE.
02654
02645      MOVE SPACES                 TO BI-TEXT-LINE.
02646      MOVE '3'                    TO BI-RECORD-TYPE.
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
CIDMOD     MOVE 'TOTAL NET  '          TO BI-TOT-DESC
CIDMOD     MOVE PI-LIFE-OVERRIDE-L6    TO BI-TOT-DESC (12:6)
CIDMOD     MOVE TOT-REPT-LF-PREM       TO BI-TOT-PREM
CIDMOD     MOVE TOT-REPT-LF-COMP       TO BI-COM-TOT
02652      MOVE TOT-LF-REPT-FACE-AMT   TO BI-FACE-TOT.
02653      PERFORM 3920-BILL-WRITE.
02654
02655      MOVE SPACES                 TO BI-TEXT-LINE.
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
CIDMOD     MOVE 'TOTAL PREM '          TO BI-TOT-DESC
CIDMOD     MOVE PI-AH-OVERRIDE-L6      TO BI-TOT-DESC (12:6)
CIDMOD     ADD TOT-REPT-AH-REF TOT-REPT-AH-PREM
CIDMOD          GIVING BI-TOT-PREM
CIDMOD     ADD TOT-REPT-AH-COMP TOT-REPT-AH-COMP-R
CIDMOD          GIVING BI-COM-TOT
CIDMOD*    MOVE TOT-AH-REPT-FACE-AMT   TO BI-FACE-TOT.
02662      PERFORM 3920-BILL-WRITE.
02663
02655      MOVE SPACES                 TO BI-TEXT-LINE.
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
CIDMOD     MOVE 'TOTAL REF  '          TO BI-TOT-DESC
CIDMOD     MOVE PI-AH-OVERRIDE-L6      TO BI-TOT-DESC (12:6)
CIDMOD     MOVE TOT-REPT-AH-REF        TO BI-TOT-PREM
CIDMOD     MOVE TOT-REPT-AH-COMP-R     TO BI-COM-TOT.
02662      PERFORM 3920-BILL-WRITE.
02663
02655      MOVE SPACES                 TO BI-TEXT-LINE.
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
CIDMOD     MOVE 'TOTAL NET  '          TO BI-TOT-DESC
CIDMOD     MOVE PI-AH-OVERRIDE-L6      TO BI-TOT-DESC (12:6)
CIDMOD     MOVE TOT-REPT-AH-PREM       TO BI-TOT-PREM
CIDMOD     MOVE TOT-REPT-AH-COMP       TO BI-COM-TOT.
02661      MOVE TOT-AH-REPT-FACE-AMT   TO BI-FACE-TOT.
02662      PERFORM 3920-BILL-WRITE.
02663
02664      IF PB-CI-CANCEL-FEE NOT NUMERIC
02665          MOVE ZEROS  TO  PB-CI-CANCEL-FEE.
02666
02667      IF PB-CI-CANCEL-FEE > ZEROS
02668          MOVE SPACES                 TO BI-TEXT-LINE
CIDMOD         MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL
02670          MOVE 'TOTAL'                TO BI-TOT-LIT
02671          MOVE 'CAN-FE'               TO BI-OVERRIDE-L6
02672          MOVE TOT-REPT-CANCEL-FEE    TO BI-TOT-PREM
02673                                         BI-COM-TOT
02674          MOVE ZEROS                  TO BI-FACE-TOT
02675          PERFORM 3920-BILL-WRITE.
02676
02677      MOVE SPACES                 TO BI-TEXT-LINE.
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.
CIDMOD     PERFORM 3920-BILL-WRITE.
02680
02681  3599-EXIT.
02682      EXIT.
02683      EJECT
02684
02685  3600-FIND-BATCH-NUMBER.
02686      IF PI-PREM (PINDX) NOT NUMERIC
02687         MOVE ZEROS               TO PI-PREM (PINDX).
02688      IF PI-COMM (PINDX) NOT NUMERIC
02689         MOVE ZEROS               TO PI-COMM (PINDX).
02690      IF PI-NON-PREM (PINDX) NOT NUMERIC
02691         MOVE ZEROS               TO PI-NON-PREM (PINDX).
02692      IF PI-NON-COMM (PINDX) NOT NUMERIC
02693         MOVE ZEROS               TO PI-NON-COMM (PINDX).
02694
02695      IF PI-PREM (PINDX)     = ZEROS   AND
02696         PI-COMM (PINDX)     = ZEROS   AND
02697         PI-NON-PREM (PINDX) = ZEROS   AND
02698         PI-NON-COMM (PINDX) = ZEROS
02699           GO TO 3699-EXIT.
02700
02701      MOVE SPACES                 TO BI-TEXT-LINE.
02702      MOVE '3'                    TO BI-RECORD-TYPE.
02703      MOVE ' '                    TO BI-SKIP-CONTROL.
02704      MOVE 'BATCH NO. '           TO WS-BATCH-DESC.
02705      MOVE PI-BATCH (PINDX)       TO WS-BATCH-NBR.
02706      MOVE WS-DESC                TO BI-TOT-DESC5.
02707      MOVE PI-PREM (PINDX)        TO BI-TOT-PREM5.
02708      MOVE PI-COMM (PINDX)        TO BI-COM-TOT5.
02709      MOVE PI-NON-PREM (PINDX)    TO BI-NON-PREM5.
02710      MOVE PI-NON-COMM (PINDX)    TO BI-NON-COMM5.
02711      PERFORM 3920-BILL-WRITE.
02712
02713  3699-EXIT.
02714      EXIT.
02715      EJECT
02716
02717  3910-BILL-GETMAIN.
02718      
      * EXEC CICS GETMAIN
02719 *        SET     (ADDRESS OF BILLING-STATEMENT)
02720 *        LENGTH  (210)
02721 *        INITIMG (GETMAIN-SPACE)
02722 *    END-EXEC.
           MOVE 210
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008017' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02723
02724  3920-BILL-WRITE.
02725      MOVE PI-COMPANY-CD          TO BI-COMPANY-CD.
02726      MOVE PI-CR-CARRIER          TO BI-CARRIER.
02727      MOVE PI-CR-GROUPING         TO BI-GROUPING
02728      MOVE PI-SAV-ACCT            TO BI-ACCOUNT.
02729      MOVE PI-SAV-REMIT-TO        TO BI-FIN-RESP.
02730
02731      MOVE 'BI'                   TO BI-RECORD-ID.
02732      ADD +1                      TO WS-LINE-SEQ-NO.
02733
02734      IF BI-RECORD-TYPE = '1'
02735          MOVE ZEROS              TO BI-LINE-SEQ-NO
02736      ELSE
02737          MOVE WS-LINE-SEQ-NO     TO BI-LINE-SEQ-NO.
02738
02739      
      * EXEC CICS WRITE
02740 *        DATASET (ERBILL-FILE-ID)
02741 *        FROM    (BILLING-STATEMENT)
02742 *        RIDFLD  (BI-CONTROL-PRIMARY)
02743 *    END-EXEC.
           MOVE LENGTH OF
            BILLING-STATEMENT
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008038' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 BILLING-STATEMENT, 
                 DFHEIV11, 
                 BI-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02744
02745  3930-BILL-READ.
02746      MOVE PI-COMPANY-CD          TO ERBILL-CO-CD.
02747      MOVE PI-CR-CARRIER          TO ERBILL-CARRIER.
02748      MOVE PI-CR-GROUPING         TO ERBILL-GROUP.
02749      MOVE PI-SAV-ACCT            TO ERBILL-ACCT.
02750      MOVE PI-SAV-REMIT-TO        TO ERBILL-FIN-RESP.
02751      MOVE '1'                    TO ERBILL-REC-TYPE.
02752      MOVE ZEROS                  TO ERBILL-LINE-SEQ-NO.
02753
02754      
      * EXEC CICS HANDLE CONDITION
02755 *        NOTFND  (3930-BILL-NOTFND)
02756 *        NOTOPEN (3930-BILL-NOTOPEN)
02757 *    END-EXEC.
      *    MOVE '"$IJ                  ! $ #00008053' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303038303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02758
02759      
      * EXEC CICS READ
02760 *        SET     (ADDRESS OF BILLING-STATEMENT)
02761 *        DATASET (ERBILL-FILE-ID)
02762 *        RIDFLD  (ERBILL-KEY)
02763 *        GTEQ
02764 *    END-EXEC.
      *    MOVE '&"S        G          (   #00008058' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERBILL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02765
02766      IF BI-COMPANY-CD = PI-COMPANY-CD  AND
02767         BI-CARRIER    = ERBILL-CARRIER AND
02768         BI-GROUPING   = ERBILL-GROUP   AND
02769         BI-ACCOUNT    = ERBILL-ACCT    AND
02770         BI-FIN-RESP   = ERBILL-FIN-RESP
02771           MOVE SPACE              TO NO-BILL-REC-SW
02772           GO TO 3930-EXIT
02773       ELSE
02774           GO TO 3930-BILL-NOTFND.
02775
02776  3930-BILL-NOTOPEN.
02777      MOVE ER-2564                TO EMI-ERROR.
02778      MOVE -1 TO APFNTERL.
02779      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02780      GO TO 8200-SEND-DATAONLY.
02781
02782  3930-BILL-NOTFND.
02783      MOVE 'Y'                    TO NO-BILL-REC-SW.
02784
02785  3930-EXIT.
02786      EXIT.
02787
02788  3940-BILL-GENERIC-DELETE.
02789      
      * EXEC CICS HANDLE CONDITION
02790 *        NOTFND (3940-EXIT)
02791 *    END-EXEC.
      *    MOVE '"$I                   ! % #00008088' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303038303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02792
02793      MOVE PI-COMPANY-CD          TO ERBILL-CO-CD.
02794      MOVE PI-CR-CARRIER          TO ERBILL-CARRIER.
02795      MOVE PI-CR-GROUPING         TO ERBILL-GROUP.
02796      MOVE PI-SAV-ACCT            TO ERBILL-ACCT.
02797
02798      IF ST-ACCNT-CNTL OR ACCNT-CNTL
02799         MOVE ZEROS               TO ERBILL-CARRIER
02800                                     ERBILL-GROUP.
02801
02802      IF CARR-ST-ACCNT-CNTL OR CARR-ACCNT-CNTL
02803         MOVE ZEROS               TO ERBILL-GROUP.
02804
02805      
      * EXEC CICS DELETE
02806 *        DATASET   (ERBILL-FILE-ID)
02807 *        RIDFLD    (ERBILL-KEY)
02808 *        KEYLENGTH (18)
02809 *        GENERIC
02810 *    END-EXEC.
           MOVE 18
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00008104' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 ERBILL-KEY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02811
02812  3940-EXIT.
02813      EXIT.
02814
02815  3990-EXIT.
02816      EXIT.
02817      EJECT
02818 ******************************************************************
02819 *                                                                *
02820 *   ALL I/O FOR THE VARIOUS FILES OTHER THAN THE COMPENSATION    *
02821 *   MASTER IS DONE IN THE FOLLOWING SECTIONS BEGINNING WITH 4XXX *
02822 *   WERE THE XXX INDICATES NUMERIC VALUES.                       *
02823 *                                                                *
02824 ******************************************************************
02825  4000-PNDB-START-BROWSE.
02826      MOVE PI-COMPANY-CD          TO ERPNDB-CO-CD-A1.
02827      MOVE PI-SAV-CARR            TO ERPNDB-CARR
02828      MOVE PI-SAV-GROUP           TO ERPNDB-GROUP.
02829      MOVE PI-SAV-STATE           TO ERPNDB-STATE.
02830      MOVE PI-SAV-ACCT            TO ERPNDB-ACCT.
02831      MOVE LOW-VALUES             TO ERPNDB-CERT.
02832
02833      
      * EXEC CICS HANDLE CONDITION
02834 *        NOTFND (4010-REC-NOT-FND)
02835 *    END-EXEC.
      *    MOVE '"$I                   ! & #00008132' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303038313332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02836
02837      
      * EXEC CICS STARTBR
02838 *        DATASET(ERPNDB-ALT-FILE-ID)
02839 *        RIDFLD (ERPNDB-ALT-KEY)
02840 *        GTEQ
02841 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008136' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 ERPNDB-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02842
02843      GO TO 4090-EXIT.
02844
02845  4010-REC-NOT-FND.
02846      MOVE 'Y'                    TO PNDB-EOF-SW.
02847
02848  4090-EXIT.
02849      EXIT.
02850      EJECT
02851  4100-PNDB-READ-NEXT.
02852      
      * EXEC CICS HANDLE CONDITION
02853 *        ENDFILE (4110-END-OF-FILE)
02854 *    END-EXEC.
      *    MOVE '"$''                   ! '' #00008151' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303038313531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02855
02856      
      * EXEC CICS READNEXT
02857 *        SET     (ADDRESS OF PENDING-BUSINESS)
02858 *        DATASET (ERPNDB-ALT-FILE-ID)
02859 *        RIDFLD  (ERPNDB-ALT-KEY)
02860 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008155' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02861
02862      GO TO 4190-EXIT.
02863
02864  4110-END-OF-FILE.
02865      MOVE 'Y'                    TO PNDB-EOF-SW.
02866
02867  4190-EXIT.
02868      EXIT.
02869
02870      EJECT
02871  4200-PNDB-REWRITE.
02872      MOVE PB-COMPANY-CD          TO ERPNDB-CO-CD.
02873      MOVE PB-ENTRY-BATCH         TO ERPNDB-BATCH.
02874      MOVE PB-BATCH-SEQ-NO        TO ERPNDB-SEQ-NO.
02875      MOVE PB-BATCH-CHG-SEQ-NO    TO ERPNDB-CHG-SEQ-NO.
02876
02877      
      * EXEC CICS ENDBR
02878 *        DATASET (ERPNDB-ALT-FILE-ID)
02879 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008176' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02880
02881      
      * EXEC CICS HANDLE CONDITION
02882 *        NOTFND   (4270-PNDB-NOTFIND)
02883 *        NOTOPEN  (4280-PNDB-FILE-NOTOPEN)
02884 *    END-EXEC.
      *    MOVE '"$IJ                  ! ( #00008180' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303038313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02885
02886      
      * EXEC CICS READ
02887 *        SET     (ADDRESS OF PENDING-BUSINESS)
02888 *        DATASET (ERPNDB-FILE-ID)
02889 *        RIDFLD  (ERPNDB-PRIME-KEY)
02890 *        UPDATE
02891 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008185' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-PRIME-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02892
02893      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS
02894          MOVE 'R'                TO PB-BILLING-STATUS.
02895
02896      IF PI-VOID-BILL
02897          MOVE LOW-VALUES         TO PB-BILLED-DT
02898          MOVE ZEROS              TO PB-CHG-COUNT
02899      ELSE
02900          MOVE WS-CURRENT-DATE    TO PB-BILLED-DT.
02901
02902      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
02903      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
02904      MOVE WS-CURRENT-DATE        TO PB-LAST-MAINT-DT.
02905
02906      
      * EXEC CICS REWRITE
02907 *        DATASET (ERPNDB-FILE-ID)
02908 *        FROM    (PENDING-BUSINESS)
02909 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008205' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02910
02911      
      * EXEC CICS STARTBR
02912 *        DATASET (ERPNDB-ALT-FILE-ID)
02913 *        RIDFLD  (ERPNDB-ALT-KEY)
02914 *        GTEQ
02915 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008210' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 ERPNDB-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02916
02917      PERFORM 4100-PNDB-READ-NEXT THRU 4190-EXIT.
02918
02919      GO TO 4290-EXIT.
02920
02921  4270-PNDB-NOTFIND.
02922      MOVE ER-2212                TO EMI-ERROR.
02923      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02924      GO TO 4290-EXIT.
02925
02926  4280-PNDB-FILE-NOTOPEN.
02927      MOVE -1                     TO ACARIERL.
02928      MOVE ER-2210                TO EMI-ERROR.
02929      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02930
02931  4290-EXIT.
02932      EXIT.
02933      EJECT
02934  4300-READ-ACCOUNT-MASTER.
02935      MOVE PI-COMPANY-CD          TO ERACCT-A-CO-CD.
02936      MOVE PI-SAV-CARR            TO ERACCT-A-CARRIER.
02937      MOVE PI-SAV-GROUP           TO ERACCT-A-GROUPING.
02938      MOVE PI-SAV-STATE           TO ERACCT-A-STATE.
02939      MOVE PI-SAV-ACCT            TO ERACCT-A-ACCOUNT.
02940      MOVE PI-SAV-EXP-DT          TO ERACCT-A-EXP-DATE.
02941
02942      MOVE SPACES                 TO WS-AM-ZIP.
02943
02944      
      * EXEC CICS HANDLE CONDITION
02945 *        NOTFND   (4370-ACCOUNT-INVALID)
02946 *        NOTOPEN  (4380-ACCT-FILE-NOTOPEN)
02947 *    END-EXEC.
      *    MOVE '"$IJ                  ! ) #00008243' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303038323433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02948
02949      
      * EXEC CICS READ
02950 *        DATASET   (ERACCT-ALT-FILE-ID)
02951 *        SET       (ADDRESS OF ACCOUNT-MASTER)
02952 *        RIDFLD    (ERACCT-ALT-KEY)
02953 *        GTEQ
02954 *    END-EXEC.
      *    MOVE '&"S        G          (   #00008248' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ALT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02955
02956      IF AM-CANADIAN-POST-CODE
02957          MOVE AM-CAN-POSTAL-1    TO WS-AM-CAN-POST-1
02958          MOVE AM-CAN-POSTAL-2    TO WS-AM-CAN-POST-2
02959      ELSE
02960          MOVE AM-ZIP-PRIME       TO WS-AM-ZIP-PRIME
02961          IF AM-ZIP-PLUS4 NOT = ZEROS  AND  SPACES
02962              MOVE '-'            TO WS-AM-ZIP-DASH
02963              MOVE AM-ZIP-PLUS4   TO WS-AM-ZIP-PLUS4.
02964
02965      IF AM-REMIT-TO NOT NUMERIC
02966          MOVE +00 TO AM-REMIT-TO.
02967
02968      IF AM-REMIT-TO = ZEROS
02969          MOVE ER-2385            TO EMI-ERROR
02970          MOVE -1                 TO AACCTL
02971          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02972
02973      MOVE AM-CARRIER             TO PI-CARRIER.
02974      MOVE AM-GROUPING            TO PI-GROUPING.
02975      MOVE AM-STATE               TO PI-STATE.
02976      MOVE AM-ACCOUNT             TO PI-ACCOUNT.
02977
02978      MOVE AM-AGT (AM-REMIT-TO)   TO PI-COMP-FIN-RESP.
02979
02980      IF PI-ZERO-CARRIER  OR
02981         PI-ZERO-CAR-GROUP
02982          MOVE ZEROS              TO PI-COMP-CARRIER
02983      ELSE
02984          MOVE AM-CARRIER         TO PI-COMP-CARRIER.
02985
02986      IF PI-ZERO-GROUPING  OR
02987         PI-ZERO-CAR-GROUP
02988          MOVE ZEROS              TO PI-COMP-GROUPING
02989      ELSE
02990          MOVE AM-GROUPING        TO PI-COMP-GROUPING.
02991
02992      IF CARR-GROUP-ST-ACCNT-CNTL
02993          MOVE AM-CARRIER         TO PI-CR-CARRIER
02994          MOVE AM-GROUPING        TO PI-CR-GROUPING
02995          MOVE AM-STATE           TO PI-CR-STATE.
02996
02997      IF ST-ACCNT-CNTL
02998          MOVE ZEROS              TO PI-CR-CARRIER
02999                                     PI-CR-GROUPING
03000          MOVE AM-STATE           TO PI-CR-STATE.
03001
03002      IF CARR-ST-ACCNT-CNTL
03003          MOVE AM-CARRIER         TO PI-CR-CARRIER
03004          MOVE ZEROS              TO PI-CR-GROUPING
03005          MOVE AM-STATE           TO PI-CR-STATE.
03006
03007      IF ACCNT-CNTL
03008          MOVE ZEROS              TO PI-CR-CARRIER
03009                                     PI-CR-GROUPING
03010                                     PI-CR-STATE.
03011
03012      IF CARR-ACCNT-CNTL
03013          MOVE AM-CARRIER         TO PI-CR-CARRIER
03014          MOVE ZEROS              TO PI-CR-GROUPING
03015                                     PI-CR-STATE.
03016
03017      MOVE AM-EXPIRATION-DT       TO ERACCT-A-EXP-DATE.
03018
03019      IF AM-AGT (AM-REMIT-TO) = PI-SAV-ACCT
03020          MOVE 'Y'                TO ACCOUNT-FIN-RESP-SW
03021      ELSE
03022          MOVE 'N'                TO ACCOUNT-FIN-RESP-SW.
03023
03024      IF PI-VOID-BILL
03025          MOVE AM-EXPIRATION-DT   TO WS-SAV-EXP-DT (DTNDX)
03026          IF DTNDX < 10
03027              SET DTNDX UP BY 1
03028              GO TO 4390-EXIT
03029          ELSE
03030              GO TO 4390-EXIT.
03031
03032      IF PI-COMPANY-CD = AM-COMPANY-CD-A1 AND
03033         PI-SAV-CARR   = AM-VG-CARRIER    AND
03034         PI-SAV-GROUP  = AM-VG-GROUPING   AND
03035         PI-SAV-STATE  = AM-VG-STATE      AND
03036         PI-SAV-ACCT   = AM-VG-ACCOUNT
03037           NEXT SENTENCE
03038      ELSE
03039           GO TO 4370-ACCOUNT-INVALID.
03040
03041      IF PI-GA-BILLING
03042          NEXT SENTENCE
03043      ELSE
03044          GO TO 4390-EXIT.
03045
03046      IF AM-AGT (AM-REMIT-TO) = PI-SAV-ACCT
03047          MOVE 'Y'                TO ACCOUNT-FIN-RESP-SW
03048      ELSE
03049          MOVE 'N'                TO ACCOUNT-FIN-RESP-SW.
03050
03051      GO TO 4390-EXIT.
03052
03053  4370-ACCOUNT-INVALID.
03054      MOVE -1                     TO AACCTL.
03055
03056      MOVE SPACES                 TO PI-CARRIER
03057                                     PI-GROUPING
03058                                     PI-STATE
03059                                     PI-ACCOUNT.
03060
03061      IF CARR-GROUP-ST-ACCNT-CNTL
03062          MOVE AL-UABON           TO ACARIERA
03063                                     AGROUPA
03064                                     ASTATEA
03065                                     AACCTA
03066      ELSE
03067          IF ST-ACCNT-CNTL
03068              MOVE AL-UABON       TO ASTATEA
03069                                     AACCTA
03070          ELSE
03071              IF CARR-ST-ACCNT-CNTL
03072                  MOVE AL-UABON   TO ACARIERA
03073                                     ASTATEA
03074                                     AACCTA
03075              ELSE
03076                  IF ACCNT-CNTL
03077                      MOVE AL-UABON
03078                                  TO AACCTA
03079                  ELSE
03080                      MOVE AL-UABON
03081                                  TO ACARIERA
03082                                     AACCTA.
03083
03084      MOVE ER-2210                TO EMI-ERROR.
03085      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03086      GO TO 4390-EXIT.
03087
03088  4380-ACCT-FILE-NOTOPEN.
03089      MOVE -1                     TO ACARIERL.
03090      MOVE ER-2215                TO EMI-ERROR.
03091      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03092
03093  4390-EXIT.
03094      EXIT.
03095
03096      EJECT
03097  4400-ERACCT-REWRITE.
03098      IF NOT PI-VOID-BILL
03099          MOVE LOW-VALUES         TO ERACCT-PRIME-KEY
03100          MOVE AM-COMPANY-CD      TO ERACCT-P-CO-CD
03101          MOVE AM-CARRIER         TO ERACCT-P-CARRIER
03102          MOVE AM-GROUPING        TO ERACCT-P-GROUPING
03103          MOVE AM-STATE           TO ERACCT-P-STATE
03104          MOVE AM-ACCOUNT         TO ERACCT-P-ACCOUNT
03105          MOVE AM-EXPIRATION-DT   TO ERACCT-P-EXP-DATE
03106      ELSE
03107          MOVE LOW-VALUES         TO ERACCT-PRIME-KEY
03108          MOVE PI-COMPANY-CD      TO ERACCT-P-CO-CD
03109          MOVE PI-CARRIER         TO ERACCT-P-CARRIER
03110          MOVE PI-GROUPING        TO ERACCT-P-GROUPING
03111          MOVE PI-STATE           TO ERACCT-P-STATE
03112          MOVE PI-SAV-ACCT        TO ERACCT-P-ACCOUNT
03113          MOVE PI-SAV-EXP-DT      TO ERACCT-P-EXP-DATE.
03114
03115      
      * EXEC CICS HANDLE CONDITION
03116 *        NOTFND   (4470-ACCOUNT-INVALID)
03117 *        NOTOPEN  (4480-ACCT-FILE-NOTOPEN)
03118 *    END-EXEC.
      *    MOVE '"$IJ                  ! * #00008414' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303038343134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03119
03120      
      * EXEC CICS READ
03121 *        DATASET   (ERACCT-FILE-ID)
03122 *        SET       (ADDRESS OF ACCOUNT-MASTER)
03123 *        RIDFLD    (ERACCT-PRIME-KEY)
03124 *        UPDATE
03125 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008419' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-PRIME-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03126
03127      IF PI-VOID-BILL
03128          MOVE AM-AGT (AM-REMIT-TO) TO PI-SAV-REMIT-TO.
03129
03130      IF PI-VOID-BILL
03131          MOVE SPACE              TO AM-BILLING-STATUS
03132      ELSE
03133          MOVE 'B'                TO AM-BILLING-STATUS.
03134
03135      MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.
03136      MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.
03137      MOVE WS-CURRENT-DATE        TO AM-LAST-MAINT-DT.
03138
03139      
      * EXEC CICS REWRITE
03140 *        DATASET (ERACCT-FILE-ID)
03141 *        FROM    (ACCOUNT-MASTER)
03142 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008438' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03143
03144      GO TO 4490-EXIT.
03145
03146  4470-ACCOUNT-INVALID.
03147      MOVE -1                     TO AACCTL.
03148
03149      MOVE SPACES                 TO PI-CARRIER
03150                                     PI-GROUPING
03151                                     PI-STATE
03152                                     PI-ACCOUNT.
03153
03154      IF CARR-GROUP-ST-ACCNT-CNTL
03155          MOVE AL-UABON           TO ACARIERA
03156                                     AGROUPA
03157                                     ASTATEA
03158                                     AACCTA
03159      ELSE
03160          IF ST-ACCNT-CNTL
03161              MOVE AL-UABON       TO ASTATEA
03162                                     AACCTA
03163          ELSE
03164              IF CARR-ST-ACCNT-CNTL
03165                  MOVE AL-UABON   TO ACARIERA
03166                                     ASTATEA
03167                                     AACCTA
03168              ELSE
03169                  IF ACCNT-CNTL
03170                      MOVE AL-UABON
03171                                  TO AACCTA
03172                  ELSE
03173                      MOVE AL-UABON
03174                                  TO ACARIERA
03175                                     AACCTA.
03176
03177      MOVE ER-2210                TO EMI-ERROR.
03178      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03179      GO TO 4490-EXIT.
03180
03181  4480-ACCT-FILE-NOTOPEN.
03182      MOVE -1                     TO ACARIERL.
03183      MOVE ER-2215                TO EMI-ERROR.
03184      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03185
03186  4490-EXIT.
03187      EXIT.
03188      EJECT
03189
03190  4500-PYAJ-START-BROWSE.
03191      MOVE PI-COMPANY-CD          TO ERPYAJ-COMP-CD.
03192
03193      IF NOT PI-ZERO-CARRIER  AND
03194         NOT PI-ZERO-CAR-GROUP
03195          MOVE PI-COMP-CARRIER    TO ERPYAJ-CARRIER
03196      ELSE
03197          MOVE ZEROS              TO ERPYAJ-CARRIER.
03198
03199      IF NOT PI-ZERO-GROUPING  AND
03200         NOT PI-ZERO-CAR-GROUP
03201          MOVE PI-COMP-GROUPING   TO ERPYAJ-GROUPING
03202      ELSE
03203          MOVE ZEROS              TO ERPYAJ-GROUPING.
03204
03205      MOVE PI-SAV-REMIT-TO        TO ERPYAJ-FIN-RESP.
03206      MOVE PI-SAV-ACCT            TO ERPYAJ-ACCOUNT.
03207
03208      IF PI-SAV-REMIT-TO = SPACES
03209          MOVE PI-SAV-ACCT        TO PI-SAV-REMIT-TO
03210                                     ERPYAJ-FIN-RESP.
03211
03212      MOVE LOW-VALUES             TO ERPYAJ-RECORD-TYPE.
03213      MOVE ERPYAJ-KEY             TO ERPYAJ-BROWSE-COMP-KEY.
03214
03215      
      * EXEC CICS HANDLE CONDITION
03216 *        NOTFND (4510-REC-NOT-FND)
03217 *    END-EXEC.
      *    MOVE '"$I                   ! + #00008514' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303038353134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03218
03219      
      * EXEC CICS STARTBR
03220 *        DATASET(ERPYAJ-FILE-ID)
03221 *        RIDFLD (ERPYAJ-KEY)
03222 *        GTEQ
03223 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008518' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03224
03225      GO TO 4590-EXIT.
03226
03227  4510-REC-NOT-FND.
03228      MOVE 'Y'                    TO PYAJ-EOF-SW.
03229
03230  4590-EXIT.
03231      EXIT.
03232
03233      EJECT
03234  4600-PYAJ-READ-NEXT.
03235      
      * EXEC CICS HANDLE CONDITION
03236 *        ENDFILE (4610-END-OF-FILE)
03237 *    END-EXEC.
      *    MOVE '"$''                   ! , #00008534' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303038353334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03238
03239      
      * EXEC CICS READNEXT
03240 *        SET     (ADDRESS OF PENDING-PAY-ADJ)
03241 *        DATASET (ERPYAJ-FILE-ID)
03242 *        RIDFLD  (ERPYAJ-KEY)
03243 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008538' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03244
03245      GO TO 4690-EXIT.
03246
03247  4610-END-OF-FILE.
03248      MOVE 'Y'                    TO PYAJ-EOF-SW.
03249
03250  4690-EXIT.
03251      EXIT.
03252      EJECT
03253  4700-PYAJ-REWRITE.
03254      
      * EXEC CICS READ
03255 *        SET     (ADDRESS OF PENDING-PAY-ADJ)
03256 *        DATASET (ERPYAJ-FILE-ID)
03257 *        RIDFLD  (ERPYAJ-KEY)
03258 *        UPDATE
03259 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008553' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03260
03261      IF PI-VOID-BILL
03262          MOVE LOW-VALUES         TO PY-BILLED-DATE
03263                                     PY-AR-DATE
03264          IF ACCOUNT-NOT-FIN-RESP
03265              IF PY-REMIT-RECEIVED OR PY-ADJ-REM-RECEIVED OR
03266                  PY-DEPOSIT OR PY-ADJ-DEPOSIT OR
03267                  PY-ADD-TO-BALANCE
03268                  ADD PY-ENTRY-AMT  TO  PI-REMITTED
03269              ELSE
03270                  IF (PY-CHARGE-TO-AGENT OR PY-ADJ-CHG-TO-AGT)
03271                     AND PY-BILLING-CHECK
03272                     ADD PY-ENTRY-AMT TO PI-DISBURSED
03273                  ELSE
03274                      ADD PY-ENTRY-AMT TO PI-ADJUSTMENTS
03275          ELSE
03276              NEXT SENTENCE
03277      ELSE
03278          MOVE WS-CURRENT-DATE    TO PY-BILLED-DATE.
03279
03280      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.
03281      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.
03282      MOVE WS-CURRENT-DATE        TO PY-LAST-MAINT-DT.
03283
03284      
      * EXEC CICS REWRITE
03285 *        DATASET (ERPYAJ-FILE-ID)
03286 *        FROM    (PENDING-PAY-ADJ)
03287 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008583' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03288
03289  4790-EXIT.
03290      EXIT.
03291
03292  4800-PYAJ-END-BROWSE.
03293      
      * EXEC CICS ENDBR
03294 *        DATASET (ERPYAJ-FILE-ID)
03295 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008592' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03296      EJECT
03297  5000-FORMAT-SCREEN.
03298 *                **********************************************
03299 *                *  FORMAT THE BILLING TOTALS SCREEN (EL640A) *
03300 *                **********************************************
03301      MOVE LOW-VALUES             TO EL640AO.
03302
03303      IF PI-SAV-ACCT > SPACES
03304         MOVE PI-SAV-ACCT         TO AACCTO
03305         MOVE PI-SAV-CARR         TO ACARIERO
03306         MOVE PI-SAV-GROUP        TO AGROUPO
03307         MOVE PI-SAV-STATE        TO ASTATEO.
03308
03309      MOVE PI-BILL-TYPE           TO ABILTYPI.
03310      MOVE PI-BILL-ERRS           TO ABILERRI.
03311      MOVE AL-UANON               TO AACCTA
03312                                     ACARIERA
03313                                     AGROUPA
03314                                     ASTATEA
03315                                     ABILTYPA
03316                                     ABILERRA.
03317
03318      IF ACCOUNT-NOT-FIN-RESP
03319          MOVE ER-2539            TO EMI-ERROR
03320          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03321
03322      IF PI-BILLING-BATCHES (1) NOT = SPACES
03323          MOVE PI-BILLING-BATCHES (1) TO ABTCH1I
03324          MOVE AL-UANON               TO ABTCH1A.
03325
03326      IF PI-BILLING-BATCHES (2) NOT = SPACES
03327          MOVE PI-BILLING-BATCHES (2) TO ABTCH2I
03328          MOVE AL-UANON               TO ABTCH2A.
03329
03330      IF PI-BILLING-BATCHES (3) NOT = SPACES
03331          MOVE PI-BILLING-BATCHES (3) TO ABTCH3I
03332          MOVE AL-UANON               TO ABTCH3A.
03333
03334      SET ANDX  PINDX            TO 1.
03335
03336  5005-FORMAT-LOOP.
03337      IF PI-BATCH (PINDX) NOT = SPACES
03338          MOVE PI-BATCH  (PINDX)  TO BATCH  (ANDX)
03339          MOVE PI-NOBILL (PINDX)  TO NOBILL (ANDX)
03340          MOVE PI-BILLED (PINDX)  TO BILL   (ANDX)
03341          MOVE PI-PREV   (PINDX)  TO PREV   (ANDX)
03342          SET ANDX PINDX UP BY 1
03343          IF ANDX > 6
03344              NEXT SENTENCE
03345          ELSE
03346              GO TO 5005-FORMAT-LOOP.
03347
03348      IF MORE-THAN-6-BATCHES
03349          MOVE ER-2472            TO EMI-ERROR
03350          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03351          MOVE ZEROS              TO EMI-ERROR.
03352
03353      IF NOT PI-DATA-BILLED
03354          IF RETURNED-FROM NOT = SPACES
03355              GO TO 5080-CONT
03356          ELSE
03357              MOVE ER-2409        TO EMI-ERROR
03358              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03359              GO TO 5090-EXIT.
03360
03361      MOVE PI-BAL-FRWD            TO ABALFWDO.
03362      MOVE PI-PREMIUM             TO APREMUMO.
03363      MOVE PI-REMITTED            TO AREMITO.
03364      MOVE PI-TOT-ISS-COMP        TO ACOMPISO.
03365      MOVE PI-TOT-CAN-COMP        TO ACOMCANO.
03366      COMPUTE PI-ADJUSTMENTS  =   PI-ADJUSTMENTS  +
03367                                  TOT-REPT-CANCEL-FEE.
03368      MOVE PI-ADJUSTMENTS         TO AADJUSTO.
03369      MOVE PI-DISBURSED           TO ADISBURO.
03370
03371      COMPUTE PI-END-BAL = PI-BAL-FRWD +
03372                           PI-PREMIUM -
03373                           PI-REMITTED -
03374                           PI-TOT-ISS-COMP +
03375                           PI-TOT-CAN-COMP +
03376                           PI-ADJUSTMENTS +
03377                           PI-DISBURSED.
03378
03379      MOVE PI-END-BAL             TO ANETDUEO.
03380
03381      IF PI-END-BAL NEGATIVE
03382          MOVE OWED-TO-AGENT      TO AENDHDGO
03383      ELSE
03384          IF PI-END-BAL > ZERO
03385              MOVE AGENT-OWES     TO AENDHDGO
03386          ELSE
03387              MOVE 'BALANCE          =' TO AENDHDGO.
03388
03389  5080-CONT.
03390      IF RETURNED-FROM NOT = SPACES
03391          IF (RETURNED-FROM NOT = XCTL-6401)  AND
03392              (PI-CR-FIN-RESP NOT = SPACES)
03393                MOVE ER-2421        TO EMI-ERROR
03394                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03395                MOVE ZEROS          TO EMI-ERROR
03396          ELSE
03397              NEXT SENTENCE
03398      ELSE
03399          IF PI-BILL OR PI-REBILLING
03400              MOVE 'Y'            TO COMP-UPDATE-SW
03401              PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT
03402              MOVE 'Y'            TO ELCNTL-UPDATE-SW
03403              MOVE SPACES         TO ELCNTL-KEY
03404              MOVE '1'            TO ELCNTL-REC-TYPE
03405              PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT
03406              MOVE WS-CURRENT-DATE TO CF-ACCOUNT-MSTR-MAINT-DT
03407                                      CF-COMPENSATION-MSTR-MAINT-DT
03408              PERFORM 6200-REWRITE-CONTROL-FILE THRU 6290-EXIT.
03409
03410  5090-EXIT.
03411      EXIT.
03412
03413      EJECT
03414  6000-READ-COMP-MASTER.
03415      
      * EXEC CICS HANDLE CONDITION
03416 *        NOTFND   (6070-NO-COMP-MSTR)
03417 *        NOTOPEN  (6080-COMP-FILE-NOTOPEN)
03418 *    END-EXEC.
      *    MOVE '"$IJ                  ! - #00008714' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303038373134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03419
03420      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.
03421      MOVE PI-COMP-CARRIER        TO ERCOMP-CARRIER
03422      MOVE PI-COMP-GROUPING       TO ERCOMP-GROUPING
03423      MOVE PI-COMP-FIN-RESP       TO ERCOMP-FIN-RESP.
03424
03425      MOVE SPACES                 TO WS-CO-ZIP.
03426
03427      IF EIBAID = DFHPF5
03428          MOVE LOW-VALUES         TO ERCOMP-ACCT
03429          MOVE 'G'                TO ERCOMP-RECORD-TYPE
03430      ELSE
03431          MOVE PI-SAV-ACCT        TO ERCOMP-ACCT
03432          MOVE 'A'                TO ERCOMP-RECORD-TYPE.
03433
03434  6001-READ.
03435      IF UPDATE-COMP-TOTALS OR PI-VOID-BILL
03436          
      * EXEC CICS READ
03437 *            DATASET   (ERCOMP-FILE-ID)
03438 *            SET       (ADDRESS OF COMPENSATION-MASTER)
03439 *            RIDFLD    (ERCOMP-KEY)
03440 *            UPDATE
03441 *        END-EXEC
      *    MOVE '&"S        EU         (   #00008735' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373335' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03442      ELSE
03443          
      * EXEC CICS READ
03444 *            DATASET   (ERCOMP-FILE-ID)
03445 *            SET       (ADDRESS OF COMPENSATION-MASTER)
03446 *            RIDFLD    (ERCOMP-KEY)
03447 *        END-EXEC.
      *    MOVE '&"S        E          (   #00008742' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373432' TO DFHEIV0(25:11)
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
           
03448
03449      IF CO-CANADIAN-POST-CODE
03450          MOVE CO-CAN-POSTAL-1    TO WS-CO-CAN-POST-1
03451          MOVE CO-CAN-POSTAL-2    TO WS-CO-CAN-POST-2
03452      ELSE
03453          MOVE CO-ZIP-PRIME       TO WS-CO-ZIP-PRIME
03454          IF CO-ZIP-PLUS4 NOT = ZEROS  AND  SPACES
03455              MOVE '-'            TO WS-CO-ZIP-DASH
03456              MOVE CO-ZIP-PLUS4   TO WS-CO-ZIP-PLUS4.
03457
03458  6002-CONTINUE.
03459      IF EIBAID = DFHPF5
03460          GO TO 6090-EXIT.
03461
03462      IF FIRST-TIME
03463         MOVE CO-MAIL-NAME        TO WS-ACCT-LINES (1)
03464         MOVE CO-ACCT-NAME        TO WS-ACCT-LINES (2)
03465         MOVE CO-ADDR-1           TO WS-ACCT-LINES (3)
03466         MOVE CO-ADDR-2           TO WS-ACCT-LINES (4)
03467         MOVE CO-ADDR-3           TO WS-ACCT-LINES (5)
03468         MOVE WS-CO-ZIP           TO WS-ACCT-LINES (6)
03469         PERFORM 6400-FORMAT-ACCOUNT-ADDRS THRU 6490-EXIT.
03470
03471      IF PI-VOID-BILL
03472          MOVE  PI-ME-MONTH       TO WS-ME-MONTH
03473          MOVE  PI-ME-DAY         TO WS-ME-DAY
03474          MOVE  PI-ME-YEAR        TO WS-ME-YEAR
03475          IF ACCOUNT-FIN-RESP
03476              MOVE CO-MONTHLY-TOTALS TO CO-CURRENT-MONTHLY-TOTALS
03477              MOVE CO-AGING-TOTALS   TO CO-CURRENT-AGING-TOTALS
03478              MOVE CO-YTD-TOTALS     TO CO-CURRENT-YTD-TOTALS
03479              MOVE CO-LAST-STMT-DT   TO CO-CURRENT-LAST-STMT-DT
03480              PERFORM 6060-ERCOMP-REWRITE
03481              GO TO 6090-EXIT
03482          ELSE
03483              MOVE CO-YTD-TOTALS     TO CO-CURRENT-YTD-TOTALS
03484              MOVE CO-LAST-STMT-DT   TO CO-CURRENT-LAST-STMT-DT
03485              PERFORM 6060-ERCOMP-REWRITE
03486              MOVE LOW-VALUES        TO ERCOMP-ACCT
03487              MOVE 'G'               TO ERCOMP-RECORD-TYPE
03488              PERFORM 6001-READ
03489              COMPUTE CO-CURRENT-CUR-CHG =
03490                      CO-CURRENT-CUR-CHG - PI-PREMIUM
03491              COMPUTE CO-CURRENT-CUR-COM =
03492                      CO-CURRENT-CUR-COM - PI-TOT-ISS-COMP -
03493                      PI-TOT-CAN-COMP
03494              COMPUTE CO-CURRENT-CUR-PMT =
03495                      CO-CURRENT-CUR-PMT - PI-REMITTED +
03496                      PI-DISBURSED + PI-ADJUSTMENTS
03497              COMPUTE CO-CURRENT-END-BAL =
03498                      CO-CURRENT-BAL-FWD + CO-CURRENT-CUR-CHG -
03499                      CO-CURRENT-CUR-COM - CO-CURRENT-CUR-PMT
03500              MOVE CO-AGING-TOTALS   TO  CO-CURRENT-AGING-TOTALS
03501              PERFORM 6050-AGING  THRU  6059-AGING-EXIT
03502              IF CO-CURRENT-CUR-CHG = ZEROS AND
03503                 CO-CURRENT-CUR-COM = ZEROS AND
03504                 CO-CURRENT-CUR-PMT = ZEROS
03505                  MOVE CO-LAST-STMT-DT TO CO-CURRENT-LAST-STMT-DT
03506                  PERFORM 6060-ERCOMP-REWRITE
03507                  GO TO 6090-EXIT
03508              ELSE
03509                  PERFORM 6060-ERCOMP-REWRITE
03510                  GO TO 6090-EXIT.
03511
03512      IF UPDATE-COMP-TOTALS
03513          MOVE  PI-ME-MONTH       TO WS-ME-MONTH
03514          MOVE  PI-ME-DAY         TO WS-ME-DAY
03515          MOVE  PI-ME-YEAR        TO WS-ME-YEAR
03516          IF ACCOUNT-FIN-RESP
03517              IF NOT PI-CHECK-PRODUCED
03518                  IF CO-CURRENT-LAST-STMT-DT = WS-MONTH-END-DATE
03519                      MOVE CO-CURRENT-END-BAL
03520                                  TO CO-CURRENT-BAL-FWD
03521                      COMPUTE CO-CURRENT-CUR-COM =
03522                              CO-CURRENT-CUR-COM +
03523                              PI-TOT-ISS-COMP - PI-TOT-CAN-COMP
03524                      COMPUTE CO-CURRENT-YTD-COM =
03525                              CO-CURRENT-YTD-COM +
03526                              PI-TOT-ISS-COMP - PI-TOT-CAN-COMP
03527                      COMPUTE CO-CURRENT-CUR-CHG =
03528                              CO-CURRENT-CUR-CHG + PI-PREMIUM
03529                      COMPUTE CO-CURRENT-CUR-PMT =
03530                              CO-CURRENT-CUR-PMT +
03531                              PI-REMITTED - PI-DISBURSED -
03532                              PI-ADJUSTMENTS
03533                      COMPUTE CO-CURRENT-END-BAL =
03534                              CO-CURRENT-BAL-FWD + PI-PREMIUM -
03535                              PI-TOT-ISS-COMP + PI-TOT-CAN-COMP -
03536                              PI-REMITTED + PI-DISBURSED +
03537                              PI-ADJUSTMENTS
03538                      PERFORM 6050-AGING THRU 6059-AGING-EXIT
03539                      PERFORM 6060-ERCOMP-REWRITE
03540                      GO TO 6090-EXIT
03541                 ELSE
03542                     MOVE CO-CURRENT-END-BAL TO CO-CURRENT-BAL-FWD
03543                     MOVE PI-END-BAL         TO CO-CURRENT-END-BAL
03544                     COMPUTE CO-CURRENT-CUR-COM =
03545                             PI-TOT-ISS-COMP - PI-TOT-CAN-COMP
03546                     ADD CO-CURRENT-CUR-COM TO CO-CURRENT-YTD-COM
03547                     MOVE PI-PREMIUM        TO CO-CURRENT-CUR-CHG
03548                     COMPUTE CO-CURRENT-CUR-PMT =
03549                              PI-REMITTED - PI-DISBURSED -
03550                              PI-ADJUSTMENTS
03551                     PERFORM 6050-AGING THRU 6059-AGING-EXIT
03552                     MOVE WS-MONTH-END-DATE
03553                                  TO CO-CURRENT-LAST-STMT-DT
03554                     PERFORM 6060-ERCOMP-REWRITE
03555                     GO TO 6090-EXIT
03556              ELSE
03557                  MOVE SPACE      TO PI-CHECK-SW
03558                  MOVE PI-END-BAL TO CO-CURRENT-END-BAL
03559                  COMPUTE CO-CURRENT-CUR-CHG = PI-PREMIUM +
03560                          (PI-ADJUSTMENTS - PI-DISBURSED)
03561                  PERFORM 6060-ERCOMP-REWRITE
03562                  GO TO 6090-EXIT
03563          ELSE
03564              COMPUTE CO-CURRENT-YTD-COM =
03565                      CO-CURRENT-YTD-COM + PI-TOT-ISS-COMP
03566                                         - PI-TOT-CAN-COMP
03567              PERFORM 6060-ERCOMP-REWRITE
03568              MOVE LOW-VALUES    TO ERCOMP-ACCT
03569              MOVE 'G'           TO ERCOMP-RECORD-TYPE
03570              PERFORM 6001-READ
03571              IF CO-CURRENT-LAST-STMT-DT = WS-MONTH-END-DATE
03572                  MOVE CO-CURRENT-END-BAL TO CO-CURRENT-BAL-FWD
03573                  COMPUTE CO-CURRENT-CUR-COM =
03574                          CO-CURRENT-CUR-COM +
03575                          PI-TOT-ISS-COMP - PI-TOT-CAN-COMP
03576                  COMPUTE CO-CURRENT-CUR-CHG =
03577                          CO-CURRENT-CUR-CHG + PI-PREMIUM
03578                  COMPUTE CO-CURRENT-CUR-PMT =
03579                          CO-CURRENT-CUR-PMT +
03580                          PI-REMITTED - PI-DISBURSED -
03581                          PI-ADJUSTMENTS
03582                  COMPUTE CO-CURRENT-END-BAL =
03583                          CO-CURRENT-BAL-FWD + PI-PREMIUM -
03584                          PI-TOT-ISS-COMP + PI-TOT-CAN-COMP -
03585                          PI-REMITTED + PI-DISBURSED +
03586                          PI-ADJUSTMENTS
03587                  PERFORM 6050-AGING THRU 6059-AGING-EXIT
03588                  PERFORM 6060-ERCOMP-REWRITE
03589                  GO TO 6090-EXIT
03590              ELSE
03591                  MOVE CO-CURRENT-END-BAL TO CO-CURRENT-BAL-FWD
03592                  COMPUTE CO-CURRENT-CUR-COM =
03593                          PI-TOT-ISS-COMP - PI-TOT-CAN-COMP
03594                  MOVE PI-PREMIUM         TO CO-CURRENT-CUR-CHG
03595                  COMPUTE CO-CURRENT-CUR-PMT =
03596                          PI-REMITTED - PI-DISBURSED -
03597                          PI-ADJUSTMENTS
03598                  COMPUTE CO-CURRENT-END-BAL =
03599                          CO-CURRENT-BAL-FWD + PI-PREMIUM -
03600                          PI-TOT-ISS-COMP + PI-TOT-CAN-COMP -
03601                          PI-REMITTED + PI-DISBURSED +
03602                          PI-ADJUSTMENTS
03603                  PERFORM 6050-AGING THRU 6059-AGING-EXIT
03604                  MOVE WS-MONTH-END-DATE
03605                                       TO CO-CURRENT-LAST-STMT-DT
03606                  PERFORM 6060-ERCOMP-REWRITE
03607                  GO TO 6090-EXIT.
03608
03609      IF PI-TOT-REBILL
03610          MOVE CO-END-BAL           TO PI-BAL-FRWD
03611      ELSE
03612          MOVE CO-CURRENT-END-BAL   TO PI-BAL-FRWD.
03613
03614      IF PI-UPDATE-FILES
03615        OR (PI-PREVIEW AND APRODSWI = 'Y')
03616          NEXT SENTENCE
03617      ELSE
03618          GO TO 6090-EXIT.
03619
03620      IF AM-AGT (ACCOM-SUB) = PI-SAV-REMIT-TO
03621          PERFORM 6040-READ-COMP-MASTER THRU 6049-EXIT
03622          IF C-RECORD-FOUND
03623              MOVE CO-ACCT-NAME   TO WS-REMIT-LINES (1)
03624              MOVE CO-MAIL-NAME   TO WS-REMIT-LINES (2)
03625              MOVE CO-ADDR-1      TO WS-REMIT-LINES (3)
03626              MOVE CO-ADDR-2      TO WS-REMIT-LINES (4)
03627              MOVE CO-ADDR-3      TO WS-REMIT-LINES (5)
03628              MOVE WS-CO-ZIP      TO WS-REMIT-LINES (6)
03629              PERFORM 6300-ELIMINATE-LINE-SPACES THRU 6390-EXIT
03630              MOVE 'CA'           TO BILLING-DETAIL-TYPE
03631              PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT
03632          ELSE
03633              MOVE SPACES         TO ELCNTL-KEY
03634              MOVE '6'            TO ELCNTL-REC-TYPE
03635              MOVE PI-CARRIER     TO ELCNTL-CARRIER
03636              PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT
03637              MOVE CF-MAIL-TO-NAME   TO WS-REMIT-LINES (1)
03638              MOVE CF-IN-CARE-OF     TO WS-REMIT-LINES (2)
03639              MOVE CF-ADDRESS-LINE-1 TO WS-REMIT-LINES (3)
03640              MOVE CF-ADDRESS-LINE-2 TO WS-REMIT-LINES (4)
03641              MOVE CF-CITY-STATE     TO WS-REMIT-LINES (5)
03642              MOVE WS-CF-ZIP         TO WS-REMIT-LINES (6)
03643              PERFORM 6300-ELIMINATE-LINE-SPACES THRU 6390-EXIT
03644              MOVE 'CA'              TO BILLING-DETAIL-TYPE
03645              PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT
03646      ELSE
03647          MOVE LOW-VALUES         TO ERCOMP-ACCT
03648          MOVE 'G'                TO ERCOMP-RECORD-TYPE
03649          PERFORM 6001-READ
03650          MOVE CO-ACCT-NAME       TO WS-REMIT-LINES (1)
03651          MOVE CO-MAIL-NAME       TO WS-REMIT-LINES (2)
03652          MOVE CO-ADDR-1          TO WS-REMIT-LINES (3)
03653          MOVE CO-ADDR-2          TO WS-REMIT-LINES (4)
03654          MOVE CO-ADDR-3          TO WS-REMIT-LINES (5)
03655          MOVE WS-CO-ZIP          TO WS-REMIT-LINES (6)
03656          MOVE CO-CURRENT-END-BAL TO PI-BAL-FRWD-GA
03657          PERFORM 6300-ELIMINATE-LINE-SPACES THRU 6390-EXIT
03658          MOVE 'GA'               TO BILLING-DETAIL-TYPE
03659          PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT.
03660
03661      GO TO 6090-EXIT.
03662
03663  6070-NO-COMP-MSTR.
03664      MOVE ER-2230                TO EMI-ERROR.
03665      MOVE -1                     TO AACCTL.
03666      MOVE AL-UABON               TO ACARIERA
03667                                     AGROUPA
03668                                     AACCTA.
03669      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03670      GO TO 6090-EXIT.
03671
03672  6080-COMP-FILE-NOTOPEN.
03673      MOVE -1                     TO APFNTERL
03674      MOVE ER-2233                TO EMI-ERROR.
03675      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03676
03677  6090-EXIT.
03678      EXIT.
03679      EJECT
03680
03681  6040-READ-COMP-MASTER.
03682      
      * EXEC CICS HANDLE CONDITION
03683 *        NOTFND   (6045-NO-COMP-MSTR)
03684 *        NOTOPEN  (6045-COMP-FILE-NOTOPEN)
03685 *    END-EXEC.
      *    MOVE '"$IJ                  ! . #00008981' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303038393831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03686
03687      MOVE 'Y'                    TO C-RECORD-FOUND-SW.
03688      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.
03689      MOVE PI-COMP-CARRIER        TO ERCOMP-CARRIER
03690      MOVE PI-COMP-GROUPING       TO ERCOMP-GROUPING
03691      MOVE LOW-VALUES             TO ERCOMP-FIN-RESP
03692                                     ERCOMP-ACCT.
03693      MOVE 'C'                    TO ERCOMP-RECORD-TYPE.
03694
03695      MOVE SPACES                 TO WS-CO-ZIP.
03696
03697      
      * EXEC CICS READ
03698 *        DATASET   (ERCOMP-FILE-ID)
03699 *        SET       (ADDRESS OF COMPENSATION-MASTER)
03700 *        RIDFLD    (ERCOMP-KEY)
03701 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008996' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393936' TO DFHEIV0(25:11)
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
           
03702
03703      IF CO-CANADIAN-POST-CODE
03704          MOVE CO-CAN-POSTAL-1    TO WS-CO-CAN-POST-1
03705          MOVE CO-CAN-POSTAL-2    TO WS-CO-CAN-POST-2
03706      ELSE
03707          MOVE CO-ZIP-PRIME       TO WS-CO-ZIP-PRIME
03708          IF CO-ZIP-PLUS4 NOT = ZEROS  AND  SPACES
03709              MOVE '-'            TO WS-CO-ZIP-DASH
03710              MOVE CO-ZIP-PLUS4   TO WS-CO-ZIP-PLUS4.
03711
03712      GO TO 6049-EXIT.
03713
03714  6045-COMP-FILE-NOTOPEN.
03715      MOVE -1                     TO APFNTERL.
03716      MOVE ER-2233                TO EMI-ERROR.
03717      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03718
03719      GO TO 6049-EXIT.
03720
03721  6045-NO-COMP-MSTR.
03722      MOVE 'N'                    TO C-RECORD-FOUND-SW.
03723
03724  6049-EXIT.
03725      EXIT.
03726      EJECT
03727
03728  6050-AGING.
03729
03730      IF NOT PI-VOID-BILL
03731         IF CO-CURRENT-LAST-STMT-DT NOT = WS-MONTH-END-DATE
080612           if pi-company-id = 'AHL'
080612              add co-current-ov90
080612                                 to co-current-ov120
080612              move co-current-ov60
080612                                 to co-current-ov90
080612              move co-current-ov30
080612                                 to co-current-ov60
080612              move co-current-cur
080612                                 to co-current-ov30
080612           else
03732               ADD CO-CURRENT-OV60
                                       TO CO-CURRENT-OV90
03733               MOVE CO-CURRENT-OV30
03734                                  TO CO-CURRENT-OV60
03735               MOVE CO-CURRENT-CUR
                                       TO CO-CURRENT-OV30
080612           end-if
080612        end-if
080612     end-if
03736
03737      COMPUTE CO-CURRENT-CUR =
03738              CO-CURRENT-CUR-CHG - CO-CURRENT-CUR-COM.
03739
03740      COMPUTE WS-PAY-ADJ =
03741              PI-REMITTED - PI-DISBURSED - PI-ADJUSTMENTS.
03742
03743      IF WS-PAY-ADJ NEGATIVE
03744          SUBTRACT WS-PAY-ADJ  FROM  CO-CURRENT-CUR
03745      ELSE
080612        if pi-company-id = 'AHL'
080612           subtract ws-pay-adj from co-current-ov120
080612        else
03746            SUBTRACT WS-PAY-ADJ  FROM  CO-CURRENT-OV90
              end-if
           end-if
03747
03748      IF CO-CURRENT-CUR NEGATIVE
080612        if pi-company-id = 'AHL'
080612           add co-current-cur to co-current-ov120
080612        else
03749            ADD CO-CURRENT-CUR  TO  CO-CURRENT-OV90
080612        end-if
03750         MOVE ZEROS              TO  CO-CURRENT-CUR
080612     end-if
03751
080612     if pi-company-id = 'AHL'
080612        if co-current-ov120 negative
080612           add co-current-ov120 to co-current-ov90
080612           move zeros           to co-current-ov120
080612        end-if
080612     end-if
03752      IF CO-CURRENT-OV90 NEGATIVE
03753          ADD CO-CURRENT-OV90  TO  CO-CURRENT-OV60
03754          MOVE ZEROS              TO  CO-CURRENT-OV90.
03755
03756      IF CO-CURRENT-OV60 NEGATIVE
03757          ADD CO-CURRENT-OV60  TO  CO-CURRENT-OV30
03758          MOVE ZEROS              TO  CO-CURRENT-OV60.
03759
03760      IF CO-CURRENT-OV30 NEGATIVE
03761          ADD CO-CURRENT-OV30  TO  CO-CURRENT-CUR
03762          MOVE ZEROS              TO  CO-CURRENT-OV30.
03763
03764  6059-AGING-EXIT.
03765      EXIT.
03766      EJECT
03767
03768  6060-ERCOMP-REWRITE.
03769
03770      
      * EXEC CICS REWRITE
03771 *        DATASET (ERCOMP-FILE-ID)
03772 *        FROM    (COMPENSATION-MASTER)
03773 *    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009100' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03774
03775  6100-READ-CONTROL-FILE.
03776      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
03777      MOVE +0                     TO ELCNTL-SEQ-NO.
03778
03779      MOVE SPACES                 TO WS-CF-ZIP.
03780
03781      
      * EXEC CICS HANDLE CONDITION
03782 *        NOTFND   (6110-NO-RECORD)
03783 *    END-EXEC.
      *    MOVE '"$I                   ! / #00009111' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303039313131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03784
03785      IF NOT ELCNTL-UPDATE
03786          
      * EXEC CICS READ
03787 *            DATASET (ELCNTL-FILE-ID)
03788 *            SET     (ADDRESS OF CONTROL-FILE)
03789 *            RIDFLD  (ELCNTL-KEY)
03790 *        END-EXEC
      *    MOVE '&"S        E          (   #00009116' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313136' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03791      ELSE
03792          MOVE SPACE              TO ELCNTL-UPDATE-SW
03793          
      * EXEC CICS READ
03794 *            DATASET (ELCNTL-FILE-ID)
03795 *            SET     (ADDRESS OF CONTROL-FILE)
03796 *            RIDFLD  (ELCNTL-KEY)
03797 *            UPDATE
03798 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00009123' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313233' TO DFHEIV0(25:11)
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
           
03799
03800      IF CF-ZIP-CODE-NUM NOT NUMERIC
03801          MOVE ZEROS              TO CF-ZIP-CODE-NUM.
03802
03803      IF CF-ZIP-CODE-NUM NOT = ZEROS
03804          MOVE CF-ZIP-CODE-NUM    TO WORK-ZIP
03805          IF WORK-ZIPR1-4 = ZEROS
03806              MOVE WORK-ZIPR1-5   TO WORK-ZIPR2-5
03807              MOVE ZEROS          TO WORK-ZIPR2-4
03808              MOVE WORK-ZIP       TO CF-ZIP-CODE
03809          ELSE
03810              MOVE WORK-ZIP       TO CF-ZIP-CODE.
03811
03812      IF CF-CANADIAN-POST-CODE
03813          MOVE CF-CAN-POSTAL-1    TO WS-CF-CAN-POST-1
03814          MOVE CF-CAN-POSTAL-2    TO WS-CF-CAN-POST-2
03815      ELSE
03816          MOVE CF-ZIP-PRIME       TO WS-CF-ZIP-PRIME
03817          IF CF-ZIP-PLUS4 NOT = ZEROS  AND  SPACES
03818              MOVE '-'            TO WS-CF-ZIP-DASH
03819              MOVE CF-ZIP-PLUS4   TO WS-CF-ZIP-PLUS4.
03820
03821      GO TO 6120-EXIT.
03822
03823  6110-NO-RECORD.
03824      IF ELCNTL-REC-TYPE = 1
03825          MOVE ER-0022            TO EMI-ERROR
03826      ELSE
03827          MOVE ER-2208            TO EMI-ERROR
03828          MOVE -1                 TO ACARIERL
03829          MOVE AL-UABON           TO ACARIERA
03830          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03831
03832  6120-EXIT.
03833      EXIT.
032219 6130-FIND-AH-BENEFIT-CD.
           
      * EXEC CICS READ
      *       DATASET (ELCNTL-FILE-ID)
      *       SET     (ADDRESS OF CONTROL-FILE)
      *       RIDFLD  (ELCNTL-KEY)
      *       GTEQ
      *       RESP    (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        G          (  N#00009165' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313635' TO DFHEIV0(25:11)
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF ws-RESP-NORMAL
              IF (CF-COMPANY-ID = PI-COMPANY-ID)
                 AND (CF-RECORD-TYPE = '5')
                 PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                    (S1 > +8)
                    OR (CF-BENEFIT-CODE (S1) = CLAS-LOOK)
                 END-PERFORM
                 IF S1 NOT > +8
                    MOVE CF-BENEFIT-CATEGORY (S1)
                                       TO WS-AH-CATEGORY
032219              move cf-benefit-alpha(s1)
032219                                 to ws-ah-alpha
032219              move cf-benefit-descrip(s1)
032219                                 to ws-ah-descrip
032219              move cf-benefit-comment(s1)
032219                                 to ws-ah-comment
032219              move cf-special-calc-cd(s1)
032219                                 to ws-ah-spec-calc-cd
032219              move cf-joint-indicator(s1)
032219                                 to ws-ah-joint-ind
032219              GO TO 6130-EXIT
                 END-IF
              END-IF
           END-IF
032219     MOVE ER-7285                TO EMI-ERROR
032219     MOVE -1                     TO ACARIERL
032219     MOVE AL-UABON               TO ACARIERA
032219     PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
032219
032219     .
032219 6130-EXIT.
032219     EXIT.
032219 6135-FIND-LF-BENEFIT-CD.
032219
032219     move spaces                 to ws-lf-benefit-cd-stuff
032219
032219     
      * EXEC CICS READ
032219*       DATASET (ELCNTL-FILE-ID)
032219*       SET     (ADDRESS OF CONTROL-FILE)
032219*       RIDFLD  (ELCNTL-KEY)
032219*       GTEQ
032219*       RESP    (WS-RESPONSE)
032219*    END-EXEC
      *    MOVE '&"S        G          (  N#00009208' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039323038' TO DFHEIV0(25:11)
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
032219
032219     IF ws-RESP-NORMAL
032219        IF (CF-COMPANY-ID = PI-COMPANY-ID)
032219           AND (CF-RECORD-TYPE = '4')
032219           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
032219              (S1 > +8)
032219              OR (CF-BENEFIT-CODE (S1) = CLAS-LOOK)
032219           END-PERFORM
032219           IF S1 NOT > +8
032219              move cf-benefit-alpha(s1)
032219                                 to ws-lf-alpha
032219              move cf-benefit-descrip(s1)
032219                                 to ws-lf-descrip
032219              move cf-benefit-comment(s1)
032219                                 to ws-lf-comment
032219              move cf-lf-coverage-type(s1)
032219                                 to ws-lf-cov-type
032219              move cf-special-calc-cd(s1)
032219                                 to ws-lf-spec-calc-cd
032219              move cf-joint-indicator(s1)
032219                                 to ws-lf-joint-ind
032219              GO TO 6135-EXIT
032219           END-IF
032219        END-IF
032219     END-IF
032219
032219     MOVE ER-7285                TO EMI-ERROR
032219     MOVE -1                     TO ACARIERL
032219     MOVE AL-UABON               TO ACARIERA
032219     PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
032219
032219     .
032219 6135-EXIT.
032219     EXIT.
03835  6200-REWRITE-CONTROL-FILE.
03836
03837      
      * EXEC CICS REWRITE
03838 *        DATASET (ELCNTL-FILE-ID)
03839 *        FROM    (CONTROL-FILE)
03840 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009251' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03841
03842  6290-EXIT.
03843      EXIT.
03844
03845      EJECT
03846  6300-ELIMINATE-LINE-SPACES.
03847      IF WS-REMIT-LINES (1) = SPACES
03848          MOVE WS-REMIT-LINES (2)  TO WS-REMIT-LINES (1)
03849          MOVE WS-REMIT-LINES (3)  TO WS-REMIT-LINES (2)
03850          MOVE WS-REMIT-LINES (4)  TO WS-REMIT-LINES (3)
03851          MOVE WS-REMIT-LINES (5)  TO WS-REMIT-LINES (4)
03852          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)
03853          MOVE SPACES              TO WS-REMIT-LINES (6)
03854          GO TO 6300-ELIMINATE-LINE-SPACES.
03855
03856      IF WS-REMIT-LINES (2) = SPACES AND
03857         WS-REMIT-LINES (3) = SPACES AND
03858         WS-REMIT-LINES (4) = SPACES AND
03859         WS-REMIT-LINES (5) = SPACES AND
03860         WS-REMIT-LINES (6) = SPACES
03861           GO TO 6350-MOVE-ZIP.
03862
03863      IF WS-REMIT-LINES (2) = SPACES
03864          MOVE WS-REMIT-LINES (3)  TO WS-REMIT-LINES (2)
03865          MOVE WS-REMIT-LINES (4)  TO WS-REMIT-LINES (3)
03866          MOVE WS-REMIT-LINES (5)  TO WS-REMIT-LINES (4)
03867          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)
03868          MOVE SPACES              TO WS-REMIT-LINES (6)
03869          GO TO 6300-ELIMINATE-LINE-SPACES.
03870
03871      IF WS-REMIT-LINES (3) = SPACES AND
03872         WS-REMIT-LINES (4) = SPACES AND
03873         WS-REMIT-LINES (5) = SPACES AND
03874         WS-REMIT-LINES (6) = SPACES
03875           GO TO 6350-MOVE-ZIP.
03876
03877      IF WS-REMIT-LINES (3) = SPACES
03878          MOVE WS-REMIT-LINES (4)  TO WS-REMIT-LINES (3)
03879          MOVE WS-REMIT-LINES (5)  TO WS-REMIT-LINES (4)
03880          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)
03881          MOVE SPACES              TO WS-REMIT-LINES (6)
03882          GO TO 6300-ELIMINATE-LINE-SPACES.
03883
03884      IF WS-REMIT-LINES (4) = SPACES AND
03885         WS-REMIT-LINES (5) = SPACES AND
03886         WS-REMIT-LINES (6) = SPACES
03887           GO TO 6350-MOVE-ZIP.
03888
03889      IF WS-REMIT-LINES (4) = SPACES
03890          MOVE WS-REMIT-LINES (5)  TO WS-REMIT-LINES (4)
03891          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)
03892          MOVE SPACES              TO WS-REMIT-LINES (6)
03893          GO TO 6300-ELIMINATE-LINE-SPACES.
03894
03895      IF WS-REMIT-LINES (5) = SPACES
03896          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)
03897          MOVE SPACES              TO WS-REMIT-LINES (6).
03898
03899  6350-MOVE-ZIP.
03900      IF WS-REMIT-LINES (6) NOT = SPACES
03901          MOVE WS-REMIT-ZIP (6)        TO WS-R-LAST-ZIP (5)
03902          MOVE SPACES                  TO WS-REMIT-LINES (6)
03903      ELSE
03904          IF WS-REMIT-LINES (5) NOT = SPACES
03905              MOVE WS-REMIT-ZIP (5)     TO WS-R-LAST-ZIP (5)
03906              MOVE SPACES               TO WS-REMIT-ZIP (5)
03907          ELSE
03908              IF WS-REMIT-LINES (4) NOT = SPACES
03909                  MOVE WS-REMIT-ZIP (4) TO WS-R-LAST-ZIP (4)
03910                  MOVE SPACES           TO WS-REMIT-ZIP (4).
03911
03912  6390-EXIT.
03913      EXIT.
03914
03915      EJECT
03916  6400-FORMAT-ACCOUNT-ADDRS.
03917      IF WS-ACCT-ADDR-AREA = SPACES
03918          GO TO 6490-EXIT.
03919
03920      IF WS-ACCT-LINES (1) = SPACES
03921          MOVE WS-ACCT-LINES (2)  TO WS-ACCT-LINES (1)
03922          MOVE WS-ACCT-LINES (3)  TO WS-ACCT-LINES (2)
03923          MOVE WS-ACCT-LINES (4)  TO WS-ACCT-LINES (3)
03924          MOVE WS-ACCT-LINES (5)  TO WS-ACCT-LINES (4)
03925          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)
03926          MOVE SPACES              TO WS-ACCT-LINES (6)
03927          GO TO 6400-FORMAT-ACCOUNT-ADDRS.
03928
03929      IF WS-ACCT-LINES (2) = SPACES AND
03930         WS-ACCT-LINES (3) = SPACES AND
03931         WS-ACCT-LINES (4) = SPACES AND
03932         WS-ACCT-LINES (5) = SPACES AND
03933         WS-ACCT-LINES (6) = SPACES
03934           GO TO 6450-MOVE-ZIP.
03935
03936      IF WS-ACCT-LINES (2) = SPACES
03937          MOVE WS-ACCT-LINES (3)  TO WS-ACCT-LINES (2)
03938          MOVE WS-ACCT-LINES (4)  TO WS-ACCT-LINES (3)
03939          MOVE WS-ACCT-LINES (5)  TO WS-ACCT-LINES (4)
03940          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)
03941          MOVE SPACES             TO WS-ACCT-LINES (6)
03942          GO TO 6400-FORMAT-ACCOUNT-ADDRS.
03943
03944      IF WS-ACCT-LINES (3) = SPACES AND
03945         WS-ACCT-LINES (4) = SPACES AND
03946         WS-ACCT-LINES (5) = SPACES AND
03947         WS-ACCT-LINES (6) = SPACES
03948           GO TO 6450-MOVE-ZIP.
03949
03950      IF WS-ACCT-LINES (3) = SPACES
03951          MOVE WS-ACCT-LINES (4)  TO WS-ACCT-LINES (3)
03952          MOVE WS-ACCT-LINES (5)  TO WS-ACCT-LINES (4)
03953          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)
03954          MOVE SPACES             TO WS-ACCT-LINES (6)
03955          GO TO 6400-FORMAT-ACCOUNT-ADDRS.
03956
03957      IF WS-ACCT-LINES (4) = SPACES AND
03958         WS-ACCT-LINES (5) = SPACES AND
03959         WS-ACCT-LINES (6) = SPACES
03960           GO TO 6450-MOVE-ZIP.
03961
03962      IF WS-ACCT-LINES (4) = SPACES
03963          MOVE WS-ACCT-LINES (5)  TO WS-ACCT-LINES (4)
03964          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)
03965          MOVE SPACES             TO WS-ACCT-LINES (6)
03966          GO TO 6400-FORMAT-ACCOUNT-ADDRS.
03967
03968      IF WS-ACCT-LINES (5) = SPACES
03969          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)
03970          MOVE SPACES             TO WS-ACCT-LINES (6).
03971
03972  6450-MOVE-ZIP.
03973      IF WS-ACCT-LINES (6) NOT = SPACES
03974          MOVE WS-ACCT-ZIP (6)         TO WS-A-LAST-ZIP (5)
03975          MOVE SPACES                  TO WS-ACCT-LINES (6)
03976      ELSE
03977          IF WS-ACCT-LINES (5) NOT = SPACES
03978              MOVE WS-ACCT-ZIP (5)      TO WS-A-LAST-ZIP (5)
03979              MOVE SPACES               TO WS-ACCT-ZIP (5)
03980          ELSE
03981              IF WS-ACCT-LINES (4) NOT = SPACES
03982                  MOVE WS-ACCT-ZIP (4)  TO WS-A-LAST-ZIP (4)
03983                  MOVE SPACES           TO WS-ACCT-ZIP (4).
03984
03985  6490-EXIT.
03986      EXIT.
03987      EJECT
03988  7000-PROCESS-CHECK.
03989 *                *******************************************
03990 *                *    FORMAT THE CHECK SCREEN (EL640D)     *
03991 *                *******************************************
03992      MOVE LOW-VALUES             TO EL640BO.
03993
03994      IF PI-SAV-ACCT = PI-SAV-REMIT-TO
03995          PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT
03996          IF EMI-ERROR = ZEROS
03997              MOVE AM-NAME        TO BNAMEO
03998              MOVE AM-PERSON      TO BADDR1O
03999              MOVE AM-ADDRS       TO BADDR2O
04000 *            MOVE AM-CITY        TO BCITYSTO
                   STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                      DELIMITED BY '  ' INTO BCITYSTO
                   END-STRING
04001              MOVE WS-AM-ZIP      TO BZIPO
04002          ELSE
04003              MOVE EL640A         TO PI-MAP-NAME
04004              MOVE -1             TO BPFNTERL
04005              GO TO 8200-SEND-DATAONLY
04006      ELSE
04007          PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT
04008          IF EMI-ERROR = ZEROS
04009              MOVE CO-ACCT-NAME   TO BNAMEO
04010              MOVE CO-ADDR-1      TO BADDR1O
04011              MOVE CO-ADDR-2      TO BADDR2O
04012 *            MOVE CO-ADDR-3      TO BCITYSTO
                   STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
                      DELIMITED BY '  ' INTO BCITYSTO
                   END-STRING
04013              MOVE WS-CO-ZIP      TO BZIPO
04014          ELSE
04015              MOVE EL640A         TO PI-MAP-NAME
04016              MOVE -1             TO BPFNTERL
04017              GO TO 8200-SEND-DATAONLY.
04018
04019      MOVE AL-UNNON               TO BCHKAMTA.
04020      MOVE PI-ACCT-NAME           TO BACCNAMO.
04021      MOVE PI-SAV-ACCT            TO BACCTO.
04022      MOVE PI-COMP-CARRIER        TO BCARIERO.
04023      MOVE PI-COMP-GROUPING       TO BGROUPO.
04024      MOVE PI-SAV-STATE           TO BSTATEO.
04025      MOVE PI-MONTH-END-DATE      TO BPAYDT2O
04026                                     WS-BEGIN-DATE.
04027      MOVE '01'                   TO WS-BEGIN-DAY.
04028      MOVE WS-BEGIN-DATE          TO BPAYDT1O.
04029      MOVE -1                     TO BCHKNOL
04030
04031      IF WS-ENTERED-FROM-A
04032          COMPUTE WORK-CHECK-AMT = PI-END-BAL * -1
04033          MOVE WORK-CHECK-AMT     TO BCHKAMTO
04034      ELSE
04035          MOVE WORK-CHECK-AMT     TO BCHKAMTO.
04036
04037      IF WORK-CHECK-AMT NOT > ZEROS
04038          MOVE ER-3165    TO  EMI-ERROR
04039          MOVE -1         TO  BCHKAMTL
04040          MOVE AL-UNBON   TO  BCHKAMTA
04041          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04042
04043      GO TO 8100-SEND-INITIAL-MAP.
04044      EJECT
04045
04046  7500-PRODUCE-CHECK.
04047 *                *******************************************
04048 *                *   PRODUCES A CHECK FOR LATER PRINTING   *
04049 *                *******************************************
04050      MOVE SPACES                 TO ELCNTL-KEY.
04051      MOVE '1'                    TO ELCNTL-REC-TYPE.
04052      MOVE 'Y'                    TO ELCNTL-UPDATE-SW.
04053      PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT.
04054
04055      IF EMI-ERROR NOT = ZEROS
04056          GO TO 8200-SEND-DATAONLY.
04057
04058      IF CR-CHECK-NO-AUTO-SEQ
04059          IF CR-CHECK-CNT-RESET-VALUE
04060              MOVE CF-CR-CHECK-COUNTER TO BCHKNOI
04061              MOVE +1             TO CF-CR-CHECK-COUNTER
04062              MOVE 6              TO BCHKNOL
04063          ELSE
04064              MOVE CF-CR-CHECK-COUNTER TO BCHKNOI
04065              MOVE 6              TO BCHKNOL
04066              ADD +1              TO CF-CR-CHECK-COUNTER.
04067
04068      IF CR-CHECK-NO-MANUAL
04069          IF BCHKNOL = ZEROS
04070              MOVE ER-2438        TO EMI-ERROR
04071              MOVE -1             TO BCHKNOL
04072              MOVE AL-UNBON       TO BCHKNOA
04073              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04074              GO TO 8200-SEND-DATAONLY
04075          ELSE
04076              IF BCHKNOI NOT NUMERIC
04077                  MOVE ER-2439    TO EMI-ERROR
04078                  MOVE -1         TO BCHKNOL
04079                  MOVE AL-UNBON   TO BCHKNOA
04080                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04081                  GO TO 8200-SEND-DATAONLY.
04082
04083      PERFORM 7600-CHECK-FORMS THRU 7600-EXIT.
04084
04085      
      * EXEC CICS GETMAIN
04086 *        SET     (ADDRESS OF PENDING-PAY-ADJ)
04087 *        LENGTH  (ERPYAJ-LENGTH)
04088 *        INITIMG (GETMAIN-SPACE)
04089 *    END-EXEC.
      *    MOVE ',"IL                  $   #00009505' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04090
04091      
      * EXEC CICS BIF DEEDIT
04092 *        FIELD  (BCHKAMTI)
04093 *        LENGTH (11)
04094 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009511' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BCHKAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04095
04096      IF BCHKAMTI NOT > ZEROS
04097          MOVE ER-3165    TO  EMI-ERROR
04098          MOVE -1         TO  BCHKAMTL
04099          MOVE AL-UNBON   TO  BCHKAMTA
04100          MOVE BCHKAMTI   TO  BCHKAMTO
04101          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04102          GO TO 8200-SEND-DATAONLY.
04103
04104      MOVE 'Y'                    TO PI-CHECK-SW.
04105      MOVE 'PY'                   TO PY-RECORD-ID.
04106      MOVE PI-COMPANY-CD          TO PY-COMPANY-CD.
04107      MOVE PI-COMP-CARRIER        TO PY-CARRIER.
04108      MOVE PI-COMP-GROUPING       TO PY-GROUPING.
04109      MOVE PI-SAV-REMIT-TO        TO PY-FIN-RESP.
04110      MOVE PI-SAV-ACCT            TO PY-ACCOUNT.
04111      MOVE 'C'                    TO PY-RECORD-TYPE.
04112      MOVE EIBTIME                TO PY-FILE-SEQ-NO.
04113
CIDMOD     IF PI-COMPANY-ID = CLIENT-ITY
04115         MOVE PI-MONTH-END-DATE   TO WS-PY-END-DT
04116                                     WS-BEGIN-DATE
04117         MOVE '01'                TO WS-BEGIN-DAY
04118         MOVE WS-BEGIN-DATE       TO WS-PY-BEGIN-DT
04119         MOVE WS-ITY-COMMENT      TO PY-ENTRY-COMMENT
04120      ELSE
04121         MOVE WS-CURRENT-DATE-MDY TO WS-PY-CURRENT-DATE
04122         MOVE WS-PY-ENTRY-COMMENT TO PY-ENTRY-COMMENT.
04123
04124      MOVE BCHKAMTI               TO PY-ENTRY-AMT.
04125      ADD BCHKAMTI TO PI-DISBURSED.
04126      MOVE BFORM1I                TO PY-LETTER (1).
04127      MOVE BFORM2I                TO PY-LETTER (2).
04128      MOVE BFORM3I                TO PY-LETTER (3).
04129
04130      IF NOT CR-CHECK-NO-AT-PRINT
04131          MOVE BCHKNOI            TO PY-CHECK-NUMBER.
04132
04133      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.
04134      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.
04135      MOVE WS-CURRENT-DATE        TO PY-LAST-MAINT-DT
04136                                     PY-INPUT-DT.
04137      IF PI-BILL OR PI-REBILLING
04138          MOVE DC-BIN-DATE-1      TO PY-BILLED-DATE
04139      ELSE
04140          MOVE LOW-VALUES         TO PY-BILLED-DATE
04141                                     PY-AR-DATE.
04142
04143      MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL
04144                                     PY-CHECK-QUE-SEQUENCE.
04145      MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT
04146                                     PY-REPORTED-DT
04147                                     PY-CHECK-WRITTEN-DT.
04148      MOVE PI-CR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT.
04149      MOVE 'B'                    TO PY-CHECK-ORIGIN-SW.
04150
04151      
      * EXEC CICS WRITE
04152 *        DATASET (ERPYAJ-FILE-ID)
04153 *        FROM    (PENDING-PAY-ADJ)
04154 *        RIDFLD  (PY-CONTROL-PRIMARY)
04155 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009571' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04156
04157      IF CR-CHECK-NO-AUTO-SEQ
04158          PERFORM 6200-REWRITE-CONTROL-FILE THRU 6290-EXIT
04159      ELSE
04160          
      * EXEC CICS UNLOCK
04161 *            DATASET (ELCNTL-FILE-ID)
04162 *        END-EXEC.
      *    MOVE '&*                    #   #00009580' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04163
04164      MOVE EL640A                 TO PI-MAP-NAME.
04165      PERFORM 5000-FORMAT-SCREEN THRU 5090-EXIT.
04166      GO TO 8100-SEND-INITIAL-MAP.
04167
04168  7600-CHECK-FORMS.
04169
04170      IF BFORM1L NOT = ZEROS
04171              AND
04172         BFORM1I NOT = SPACES
04173          MOVE BFORM1I            TO ELLETR-FORM-NO
04174          PERFORM 7620-CHECK-ON-LETTER THRU 7620-EXIT
04175          IF W-LETTER-NOT-FOUND
04176              MOVE ER-7389        TO EMI-ERROR
04177              MOVE -1             TO BFORM1L
04178              MOVE AL-UABON       TO BFORM1A
04179              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04180              GO TO 8200-SEND-DATAONLY.
04181
04182      IF BFORM2L NOT = ZEROS
04183              AND
04184         BFORM2I NOT = SPACES
04185          MOVE BFORM2I            TO ELLETR-FORM-NO
04186          PERFORM 7620-CHECK-ON-LETTER THRU 7620-EXIT
04187
04188          IF W-LETTER-NOT-FOUND
04189              MOVE ER-7389        TO EMI-ERROR
04190              MOVE -1             TO BFORM2L
04191              MOVE AL-UABON       TO BFORM2A
04192              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04193              GO TO 8200-SEND-DATAONLY.
04194
04195      IF BFORM3L NOT = ZEROS
04196              AND
04197         BFORM3I NOT = SPACES
04198          MOVE BFORM3I            TO ELLETR-FORM-NO
04199          IF  W-LETTER-NOT-FOUND
04200              MOVE ER-7389        TO EMI-ERROR
04201              MOVE -1             TO BFORM3L
04202              MOVE AL-UABON       TO BFORM3A
04203              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04204              GO TO 8200-SEND-DATAONLY.
04205
04206      IF BFORM1L = ZEROS
04207          IF BFORM2L NOT = ZEROS
04208              MOVE BFORM2L        TO BFORM1L
04209              MOVE BFORM3L        TO BFORM2L
04210              MOVE BFORM2A        TO BFORM1A
04211              MOVE BFORM3A        TO BFORM2A
04212              MOVE BFORM2I        TO BFORM1I
04213              MOVE BFORM3I        TO BFORM2I
04214              MOVE LOW-VALUES     TO BFORM3I
04215                                     BFORM3A
04216              MOVE ZEROS          TO BFORM3L
04217          ELSE
04218              IF BFORM3L NOT = ZEROS
04219                  MOVE BFORM3L    TO BFORM1L
04220                  MOVE BFORM3I    TO BFORM1I
04221                  MOVE BFORM3A    TO BFORM1A
04222                  MOVE LOW-VALUES TO BFORM3I
04223                                     BFORM3A
04224                  MOVE ZEROS      TO BFORM3L
04225              ELSE
04226                  NEXT SENTENCE
04227      ELSE
04228          IF BFORM2L = ZEROS
04229              IF BFORM3L NOT = ZEROS
04230                  MOVE BFORM3L    TO BFORM2L
04231                  MOVE BFORM3I    TO BFORM2I
04232                  MOVE BFORM3A    TO BFORM2A
04233                  MOVE LOW-VALUES TO BFORM3I
04234                                     BFORM3A
04235                  MOVE ZEROS      TO BFORM3L.
04236
04237  7600-EXIT.
04238       EXIT.
04239  EJECT
04240  7620-CHECK-ON-LETTER.
04241
04242      MOVE SPACES                 TO W-LETTER-IND.
04243
04244      MOVE PI-COMPANY-CD          TO ELLETR-COMPANY-CD.
04245      MOVE +1                     TO ELLETR-LINE-SEQ.
04246
04247      
      * EXEC CICS HANDLE CONDITION
04248 *        NOTFND   (7620-LETR-NOT-FOUND)
04249 *        ENDFILE  (7620-LETR-NOT-FOUND)
04250 *        NOTOPEN  (7620-LETR-NOT-OPEN)
04251 *    END-EXEC.
      *    MOVE '"$I''J                 ! 0 #00009667' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303039363637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04252
04253      
      * EXEC CICS READ
04254 *        SET     (ADDRESS OF TEXT-FILES)
04255 *        DATASET (ELLETR-FILE-ID)
04256 *        RIDFLD  (ELLETR-KEY)
04257 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009673' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELLETR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04258
04259      GO TO 7620-EXIT.
04260
04261  7620-LETR-NOT-FOUND.
04262
04263      MOVE 'Y'                    TO W-LETTER-IND.
04264      GO TO 7620-EXIT.
04265
04266  7620-LETR-NOT-OPEN.
04267
04268      MOVE ER-0013                TO EMI-ERROR.
04269      MOVE -1                     TO BFORM1L.
04270      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04271      GO TO 8200-SEND-DATAONLY.
04272
04273  7620-EXIT.
04274       EXIT.
04275      EJECT
04276  8100-SEND-INITIAL-MAP.
04277
04278      MOVE SPACES TO PI-TRANSFER-SW.
04279
04280      IF PI-MAP-NAME = EL640A
04281          NEXT SENTENCE
04282      ELSE
04283          GO TO 8110-SEND-INITIAL-CHECK-MAP.
04284
04285      IF PI-COMPANY-ID = 'DMD'
04286         MOVE AL-SADOF            TO APF5HDGA.
04287
04288      IF NOT PI-GA-BILLING
04289         MOVE AL-SADOF            TO AGAHDGA.
04290
04291      MOVE WS-CURRENT-DATE-EDIT   TO ADATEO.
04292      MOVE EIBTIME                TO TIME-IN.
04293      MOVE TIME-OUT               TO ATIMEO.
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
04294
04295      MOVE -1                     TO AACCTL.
04296      MOVE EMI-MESSAGE-AREA (1)   TO AERMSG1O.
04297      MOVE EMI-MESSAGE-AREA (2)   TO AERMSG2O.
04298
04299      IF CARR-GROUP-ST-ACCNT-CNTL
04300          NEXT SENTENCE
04301      ELSE
04302          IF ST-ACCNT-CNTL
04303              MOVE AL-SADOF       TO ACARHDGA
04304                                     AGRPHDGA
04305              MOVE AL-SANOF       TO ACARIERA
04306                                     AGROUPA
04307          ELSE
04308              IF CARR-ST-ACCNT-CNTL
04309                  MOVE AL-SADOF   TO AGRPHDGA
04310                  MOVE AL-SANOF   TO AGROUPA
04311              ELSE
04312                  IF ACCNT-CNTL
04313                      MOVE AL-SADOF TO ACARHDGA
04314                                       AGRPHDGA
04315                                       ASTHDGA
04316                      MOVE AL-SANOF TO ACARIERA
04317                                       AGROUPA
04318                                       ASTATEA
04319                  ELSE
04320                      IF CARR-ACCNT-CNTL
04321                          MOVE AL-SADOF TO AGRPHDGA
04322                                           ASTHDGA
04323                          MOVE AL-SANOF TO AGROUPA
04324                                           ASTATEA.
04325
04326      IF (EIBTRNID = EL633-TRANS-ID  OR  EL635-TRANS-ID  OR
04327                     EL633DMD-TRANS-ID  OR
04328                     EL650-TRANS-ID  OR  EL652-TRANS-ID  OR
04329                     EL658-TRANS-ID)  AND
04330           (WT-CR-ACCOUNT NOT = SPACES AND LOW-VALUES)
04331         MOVE WT-CR-CARRIER       TO PI-SCR-CARRIER
04332         MOVE WT-CR-GROUPING      TO PI-SCR-GROUPING
04333         MOVE WT-CR-STATE         TO PI-SCR-STATE
04334         MOVE WT-CR-FIN-RESP      TO PI-SCR-FIN-RESP
04335         MOVE WT-CR-TYPE          TO PI-SCR-TYPE
04336         IF CARR-GROUP-ST-ACCNT-CNTL
04337             MOVE AL-UANON        TO ACARIERA
04338                                     AGROUPA
04339                                     ASTATEA
04340                                     AACCTA
04341             MOVE WT-CR-CARRIER   TO ACARIERI
04342                                     PI-SCR-CARRIER
04343             MOVE WT-CR-GROUPING  TO AGROUPI
04344                                     PI-SCR-GROUPING
04345             MOVE WT-CR-ACCOUNT   TO AACCTI
04346                                     PI-SCR-ACCOUNT
04347             MOVE 1               TO ACARIERL
04348             MOVE 6               TO AGROUPL
04349             MOVE 10              TO AACCTL
04350             MOVE -1              TO ASTATEL
04351         ELSE
04352          IF ST-ACCNT-CNTL
04353             MOVE AL-UANON        TO ASTATEA
04354                                     AACCTA
04355             MOVE WT-CR-ACCOUNT   TO AACCTI
04356                                     PI-SCR-ACCOUNT
04357             MOVE 10              TO AACCTL
04358             MOVE -1              TO ASTATEL
04359          ELSE
04360             IF CARR-ST-ACCNT-CNTL
04361                 MOVE AL-UANON       TO ACARIERA
04362                                        ASTATEA
04363                                        AACCTA
04364                 MOVE WT-CR-CARRIER TO ACARIERI
04365                                       PI-SCR-CARRIER
04366                 MOVE WT-CR-ACCOUNT TO AACCTI
04367                                       PI-SCR-ACCOUNT
04368                 MOVE 1              TO ACARIERL
04369                 MOVE 10             TO AACCTL
04370                 MOVE -1             TO ASTATEL
04371             ELSE
04372                 IF ACCNT-CNTL
04373                     MOVE AL-UANON       TO AACCTA
04374                     MOVE WT-CR-ACCOUNT  TO AACCTI
04375                                            PI-SCR-ACCOUNT
04376                     MOVE 10             TO AACCTL
04377                     MOVE -1             TO ABILTYPL
04378                 ELSE
04379                     IF CARR-ACCNT-CNTL
04380                         MOVE AL-UANON        TO ACARIERA
04381                                                 AACCTA
04382                         MOVE WT-CR-CARRIER   TO ACARIERI
04383                                                 PI-SCR-CARRIER
04384                         MOVE WT-CR-ACCOUNT   TO AACCTI
04385                                                 PI-SCR-ACCOUNT
04386                         MOVE 1               TO ACARIERL
04387                         MOVE 10              TO AACCTL
04388                         MOVE -1              TO ABILTYPL.
04389
04390      IF (EIBTRNID = EL6311-TRANS-ID) AND
04391           (WT-CR-ACCOUNT NOT = SPACES AND LOW-VALUES)
04392         MOVE WT-CR-CARRIER       TO PI-SCR-CARRIER
04393         MOVE WT-CR-GROUPING      TO PI-SCR-GROUPING
04394         MOVE WT-CR-STATE         TO PI-SCR-STATE
04395         MOVE WT-CR-FIN-RESP      TO PI-SCR-FIN-RESP
04396         MOVE WT-CR-TYPE          TO PI-SCR-TYPE
04397         MOVE SPACES              TO PI-SAV-CARR
04398                                     PI-SAV-GROUP
04399                                     PI-SAV-STATE
04400                                     PI-SAV-ACCT
04401         MOVE LOW-VALUES          TO PI-SAV-EXP-DT
04402         IF CARR-GROUP-ST-ACCNT-CNTL
04403             MOVE AL-UANON        TO ACARIERA
04404                                     AGROUPA
04405                                     ASTATEA
04406                                     AACCTA
04407             MOVE WT-CR-CARRIER   TO ACARIERI
04408                                     PI-SCR-CARRIER
04409                                     PI-SAV-CARR
04410             MOVE WT-CR-GROUPING  TO AGROUPI
04411                                     PI-SCR-GROUPING
04412                                     PI-SAV-GROUP
04413             MOVE WT-CR-STATE     TO ASTATEI
04414                                     PI-SCR-STATE
04415             MOVE WT-CR-ACCOUNT   TO AACCTI
04416                                     PI-SCR-ACCOUNT
04417             MOVE 1               TO ACARIERL
04418             MOVE 6               TO AGROUPL
04419             MOVE 10              TO AACCTL
04420             MOVE 2               TO ASTATEL
04421             MOVE -1              TO ABILTYPL
04422         ELSE
04423          IF ST-ACCNT-CNTL
04424             MOVE AL-UANON        TO ASTATEA
04425                                     AACCTA
04426             MOVE WT-CR-STATE     TO ASTATEI
04427                                     PI-SCR-STATE
04428                                     PI-SAV-STATE
04429             MOVE WT-CR-ACCOUNT   TO AACCTI
04430                                     PI-SCR-ACCOUNT
04431                                     PI-SAV-ACCT
04432             MOVE 10              TO AACCTL
04433             MOVE -1              TO ASTATEL
04434          ELSE
04435             IF CARR-ST-ACCNT-CNTL
04436                 MOVE AL-UANON      TO ACARIERA
04437                                       ASTATEA
04438                                       AACCTA
04439                 MOVE WT-CR-CARRIER TO ACARIERI
04440                                       PI-SCR-CARRIER
04441                                       PI-SAV-CARR
04442                 MOVE WT-CR-STATE   TO ASTATEI
04443                                       PI-SCR-STATE
04444                                       PI-SAV-STATE
04445                 MOVE WT-CR-ACCOUNT TO AACCTI
04446                                       PI-SCR-ACCOUNT
04447                                       PI-SAV-ACCT
04448                 MOVE 1             TO ACARIERL
04449                 MOVE 10            TO AACCTL
04450                 MOVE 2             TO ASTATEL
04451                 MOVE -1            TO ABILTYPL
04452             ELSE
04453                 IF ACCNT-CNTL
04454                     MOVE AL-UANON       TO AACCTA
04455                     MOVE WT-CR-ACCOUNT  TO AACCTI
04456                                            PI-SCR-ACCOUNT
04457                                            PI-SAV-ACCT
04458                     MOVE 10             TO AACCTL
04459                     MOVE -1             TO ABILTYPL
04460                 ELSE
04461                     IF CARR-ACCNT-CNTL
04462                         MOVE AL-UANON        TO ACARIERA
04463                                                 AACCTA
04464                         MOVE WT-CR-CARRIER   TO ACARIERI
04465                                                 PI-SCR-CARRIER
04466                                                 PI-SAV-CARR
04467                         MOVE WT-CR-ACCOUNT   TO AACCTI
04468                                                 PI-SCR-ACCOUNT
04469                                                 PI-SAV-ACCT
04470                         MOVE 1               TO ACARIERL
04471                         MOVE 10              TO AACCTL
04472                         MOVE -1              TO ABILTYPL.
04473
04474      
      * EXEC CICS SEND
04475 *        MAP      (PI-MAP-NAME)
04476 *        MAPSET   (MAPSET-NAME)
04477 *        FROM     (EL640AO)
04478 *        ERASE
04479 *        CURSOR
04480 *    END-EXEC.
           MOVE LENGTH OF
            EL640AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009896' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL640AO, 
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
           
04481
04482      GO TO 9100-RETURN-TRAN.
04483      EJECT
04484  8110-SEND-INITIAL-CHECK-MAP.
04485      MOVE WS-CURRENT-DATE-EDIT   TO BDATEO.
04486      MOVE EIBTIME                TO TIME-IN.
04487      MOVE TIME-OUT               TO BTIMEO.
101101     MOVE PI-COMPANY-ID          TO BCMPNYO.
101101     MOVE PI-PROCESSOR-ID        TO BUSERIDO.
04488      MOVE -1                     TO BPFNTERL.
04489      MOVE EMI-MESSAGE-AREA (1)   TO BERMSGO.
04490
04491      
      * EXEC CICS SEND
04492 *        MAP      (PI-MAP-NAME)
04493 *        MAPSET   (MAPSET-NAME)
04494 *        FROM     (EL640BO)
04495 *        ERASE
04496 *        CURSOR
04497 *    END-EXEC.
           MOVE LENGTH OF
            EL640BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009915' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL640BO, 
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
           
04498
04499      GO TO 9100-RETURN-TRAN.
04500      EJECT
04501  8200-SEND-DATAONLY.
04502      IF PI-MAP-NAME = EL640A
04503          MOVE WS-CURRENT-DATE-EDIT   TO ADATEO
04504          MOVE EIBTIME                TO TIME-IN
04505          MOVE TIME-OUT               TO ATIMEO
101101         MOVE PI-COMPANY-ID          TO CMPNYIDO
101101         MOVE PI-PROCESSOR-ID        TO USERIDO
04506          MOVE EMI-MESSAGE-AREA (1)   TO AERMSG1O
04507          MOVE EMI-MESSAGE-AREA (2)   TO AERMSG2O
04508          
      * EXEC CICS SEND
04509 *            MAP      (PI-MAP-NAME)
04510 *            MAPSET   (MAPSET-NAME)
04511 *            FROM     (EL640AO)
04512 *            DATAONLY
04513 *            ERASEAUP
04514 *            CURSOR
04515 *        END-EXEC
           MOVE LENGTH OF
            EL640AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00009934' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL640AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04516      ELSE
04517          MOVE WS-CURRENT-DATE-EDIT   TO BDATEO
04518          MOVE EIBTIME                TO TIME-IN
04519          MOVE TIME-OUT               TO BTIMEO
101101         MOVE PI-COMPANY-ID          TO BCMPNYO
101101         MOVE PI-PROCESSOR-ID        TO BUSERIDO
04520          MOVE EMI-MESSAGE-AREA (1)   TO BERMSGO
04521          
      * EXEC CICS SEND
04522 *            MAP      (PI-MAP-NAME)
04523 *            MAPSET   (MAPSET-NAME)
04524 *            FROM     (EL640BO)
04525 *            DATAONLY
04526 *            ERASEAUP
04527 *            CURSOR
04528 *        END-EXEC.
           MOVE LENGTH OF
            EL640BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00009949' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL640BO, 
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
           
04529
04530      GO TO 9100-RETURN-TRAN.
04531      EJECT
04532  8300-SEND-TEXT.
04533      
      * EXEC CICS SEND TEXT
04534 *        FROM     (LOGOFF-TEXT)
04535 *        LENGTH   (LOGOFF-LENGTH)
04536 *        ERASE
04537 *        FREEKB
04538 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009961' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393631' TO DFHEIV0(25:11)
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
           
04539
04540      
      * EXEC CICS RETURN
04541 *    END-EXEC.
      *    MOVE '.(                    ''   #00009968' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04542
04543  8500-DATE-CONVERT.
04544      
      * EXEC CICS LINK
04545 *        PROGRAM  (LINK-ELDATCV)
04546 *        COMMAREA (DATE-CONVERSION-DATA)
04547 *        LENGTH   (DC-COMM-LENGTH)
04548 *    END-EXEC.
      *    MOVE '."C                   (   #00009972' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04549
04550  8500-EXIT.
04551      EXIT.
04552      EJECT
04553  8800-UNAUTHORIZED-ACCESS.
04554      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
04555      GO TO 8300-SEND-TEXT.
04556
04557  8810-PF23.
04558      MOVE EIBAID                 TO PI-ENTRY-CD-1.
04559      MOVE XCTL-005               TO PGM-NAME.
04560      GO TO 9300-XCTL.
04561
04562  8900-SYNCPOINT-ROLLBACK.
04563      
      * EXEC CICS SYNCPOINT
04564 *         ROLLBACK
04565 *    END-EXEC.
      *    MOVE '6"R                   !   #00009991' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04566
04567  8900-EXIT.
04568      EXIT.
04569
04570  9100-RETURN-TRAN.
04571      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
04572      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
04573
04574      
      * EXEC CICS RETURN
04575 *        TRANSID    (TRANS-ID)
04576 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
04577 *        LENGTH     (PI-COMM-LENGTH)
04578 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00010002' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04579
04580  9200-RETURN-MAIN-MENU.
04581      MOVE XCTL-626               TO PGM-NAME.
04582      GO TO 9300-XCTL.
04583
04584  9300-XCTL.
04585      
      * EXEC CICS XCTL
04586 *        PROGRAM    (PGM-NAME)
04587 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
04588 *        LENGTH     (PI-COMM-LENGTH)
04589 *    END-EXEC.
      *    MOVE '.$C                   %   #00010013' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04590
04591  9400-CLEAR.
04592      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
04593      GO TO 9300-XCTL.
04594
04595  9500-PF12.
04596      MOVE XCTL-010               TO PGM-NAME.
04597      GO TO 9300-XCTL.
04598      EJECT
04599  9600-PGMID-ERROR.
04600      
      * EXEC CICS HANDLE CONDITION
04601 *        PGMIDERR    (8300-SEND-TEXT)
04602 *    END-EXEC.
      *    MOVE '"$L                   ! 1 #00010028' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303130303238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04603
04604      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
04605      MOVE ' '                    TO PI-ENTRY-CD-1.
04606      MOVE XCTL-005               TO PGM-NAME.
04607      MOVE PGM-NAME               TO LOGOFF-PGM.
04608      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
04609      GO TO 9300-XCTL.
04610
04611  9900-ERROR-FORMAT.
04612      IF NOT EMI-ERRORS-COMPLETE
04613          MOVE LINK-001           TO PGM-NAME
04614          
      * EXEC CICS LINK
04615 *            PROGRAM    (PGM-NAME)
04616 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
04617 *            LENGTH     (EMI-COMM-LENGTH)
04618 *        END-EXEC.
      *    MOVE '."C                   (   #00010042' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04619
04620  9900-EXIT.
04621      EXIT.
04622
04623  9990-ABEND.
04624      MOVE LINK-004               TO PGM-NAME.
04625      MOVE DFHEIBLK               TO EMI-LINE1.
04626
04627      
      * EXEC CICS LINK
04628 *        PROGRAM   (PGM-NAME)
04629 *        COMMAREA  (EMI-LINE1)
04630 *        LENGTH    (72)
04631 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00010055' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04632
04633      IF PI-MAP-NAME = EL640A
04634          MOVE -1 TO APFNTERL
04635      ELSE
04636          MOVE -1 TO BPFNTERL.
04637
04638      GO TO 8200-SEND-DATAONLY.
04639
04640  9995-SECURITY-VIOLATION.
04641 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00010086' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303836' TO DFHEIV0(25:11)
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
04642

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL640' TO DFHEIV1
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
               GO TO 0890-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3930-BILL-NOTFND,
                     3930-BILL-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3940-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 4010-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 4110-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4270-PNDB-NOTFIND,
                     4280-PNDB-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4370-ACCOUNT-INVALID,
                     4380-ACCT-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4470-ACCOUNT-INVALID,
                     4480-ACCT-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4510-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 4610-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 6070-NO-COMP-MSTR,
                     6080-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6045-NO-COMP-MSTR,
                     6045-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 6110-NO-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 7620-LETR-NOT-FOUND,
                     7620-LETR-NOT-FOUND,
                     7620-LETR-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL640' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
