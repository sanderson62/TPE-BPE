00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6501.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 05/11/94 14:25:07.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.060.
00009
00010 *AUTHOR.        LOGIC,INC.
00011 *               DALLAS, TEXAS.
00012
00013 *DATE-COMPILED.
00014
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024 *
00025 *REMARKS.
00026 *        TRANSACTION - EXC5 - ACCOUNT MAINT (GENERAL)
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                                ADJUSTED MAP-R REDEFINES FILLER
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
100703*                          SMVA  ADD CLP TOL PCT CODE
102004* 102004                   PEMA  ADD STATUS OF CANCEL
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
021506* 021506  CR2006021400002  PEMA  DARKEN DCC FIELDS FOR CID
052306* 052306  CR2006050800002  PEMA  ADD COMM TYPE J
111606* 111606  CR2002061800017  PEMA  FIX ERCOMP UPDATE ROUTINE
012407* 110706  CR2006071700002  PEMA  FIX NAME LOOK UP
031607* 031607                   PEMA  PER TRWA AND KLSC DEFAULT TO P
062707* 062707  CR2007010300002  PEMA  REMOVE LEV 5 RESTRICTION FOR DCC
071207* 071207                   PEMA  ADD NEW TYPE 'M'
080807* 080807  CR2006062900001  PEMA  AUTOMATE STEPS TO CANCEL ACCT
022808* 022808  CR2007083100002  PEMA  ADD 'F' ACCT STATUS
042809* 042809    2007070200004  PEMA  ADD EDIT FOR KY ACCOUNTS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020210* 020210  CR2020010400006  PEMA  REMOVE CHANGES MADE ON 09/09
031811* 031811  CR2011012700001  PEMA  ADD 'S' ACCT STATUS
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
080612* 080612  CR2012042700005  PEMA  ADD OVER 120 DAYS FOR AHL
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
070714* 070714    2014052800001  PEMA  correct read on erpdef for DCC
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
021916* 021916  CR2014010900001  TANA  ADD ACCT STATUS D,L,R,P
102717* 102717  CR2017062000003  PEMA  Add comm cap edits
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
101101******************************************************************
00027
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  EJECT
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
00033  77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
00034                                    USAGE POINTER.
00035  77  FILLER  PIC X(32)  VALUE '********************************'.
00036  77  FILLER  PIC X(32)  VALUE '*    EL6501 WORKING STORAGE    *'.
00037  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.060 ***********'.
111606 77  WS-LAST-DATE-RANGE-SW       PIC X    VALUE SPACES.
111606     88  ON-LAST-DATE-RANGE             VALUE 'Y'.
080807 77  WS-AUTO-CANCEL-SW           PIC X  VALUE ' '.
080807     88  AUTO-CANCEL-ACCOUNT            VALUE 'Y'.
       77  WS-DELAY-INT                PIC S9(7) VALUE +2.
       77  WS-CHANGE-ALL-CNT           PIC S999  VALUE +0.
DEBUG  01  WS-DEGUG-AREA.
           05  FILLER PIC X(20) VALUE '&&&&&& DEBUG &&&&&&&'.
           05  FILLER PIC X(20) VALUE '  PAGE NUMBER HERE  '.
           05  WS-PAGE-NUMBER PIC 99 VALUE ZEROS.
           05  FILLER PIC X(20) VALUE '  TOTAL LINES HERE  '.
           05  WS-TOTAL-LINES PIC 99 VALUE ZEROS.
           05  FILLER PIC X(20) VALUE ' LINE SELECTED HERE '.
           05  WS-LINE-SELECTED PIC 99 VALUE ZEROS.
           05  FILLER PIC X(20) VALUE '&&&&&& DEBUG &&&&&&&'.
       01  S1                          PIC S999   VALUE +0    COMP.
       01  WS-WORK-FIELD.
           12  WS-WORK-NUM                 PIC 9(5)    VALUE ZEROS.
00039  01  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.
00040  01  WS-DATE-AREA.
00041      12  SAVE-DATE               PIC X(8)    VALUE SPACES.
00042      12  SAVE-BIN-DATE           PIC XX      VALUE SPACES.
00043
00044 ***DMD CUSTOM CODE *****START
00045      12  WS-DMD-EXPR-DATE        PIC 9(8) COMP-3.
00046      12  WS-DMD-AGENT            PIC X(10).
00047      12  WS-YYYYMMDD             PIC X(8).
00048      12  WS-COMP-DATE REDEFINES WS-YYYYMMDD
00049                                  PIC 9(8).
00050 ***DMD CUSTOM CODE *****END
00051
00052  01  WS-ACCOUNT-NUM-AREA.
00053      12  WS-ACCOUNT-FILLER       PIC X(06).
00054      12  WS-ACCT-BRANCH-CD       PIC X(04).
00055
00056  01  WS-NAME-AREA.
00057      12  WS-NAME-1-4             PIC X(04).
00058      12  WS-NAME-FILLER          PIC X(26).
00059
00060  01  STANDARD-AREAS.
CIDMOD     12  WS-STATE-MAX-SW         PIC X       VALUE ' '.
CIDMOD         88  EXCEEDS-STATE-MAX      VALUE 'Y'.
CIDMOD     12  WS-RESPONSE             PIC S9(8)   COMP.
CIDMOD         88  RESP-NORMAL                  VALUE +00.
CIDMOD         88  RESP-NOTFND                  VALUE +13.
080807         88  RESP-DUPREC                  VALUE +14.
080807         88  RESP-DUPKEY                  VALUE +15.
CIDMOD         88  RESP-NOTOPEN                 VALUE +19.
080807         88  RESP-ENDFILE                 VALUE +20.
CIDMOD
00061      12  GETMAIN-SPACE           PIC X       VALUE SPACE.
00062      12  MAP-NAME                PIC X(8)    VALUE 'EL6501A'.
00063      12  MAPSET-NAME             PIC X(8)    VALUE 'EL6501S'.
00064      12  SCREEN-NUMBER           PIC X(4)    VALUE '650B'.
00065      12  TRANS-ID                PIC X(4)    VALUE 'EXC5'.
00066      12  THIS-PGM                PIC X(8)    VALUE 'EL6501'.
00067      12  PGM-NAME                PIC X(8).
00068      12  TIME-IN                 PIC S9(7).
00069      12  TIME-OUT-R REDEFINES TIME-IN.
00070          16  FILLER              PIC X.
00071          16  TIME-OUT            PIC 99V99.
00072          16  FILLER              PIC XX.
00073      12  XCTL-005                PIC X(8)    VALUE 'EL005'.
00074      12  XCTL-010                PIC X(8)    VALUE 'EL010'.
00075      12  XCTL-126                PIC X(8)    VALUE 'EL126'.
00076      12  XCTL-601                PIC X(8)    VALUE 'EL601'.
00077      12  XCTL-608                PIC X(8)    VALUE 'EL608'.
00078      12  XCTL-626                PIC X(8)    VALUE 'EL626'.
00079      12  XCTL-650                PIC X(8)    VALUE 'EL650'.
00080      12  XCTL-6502               PIC X(8)    VALUE 'EL6502'.
00081      12  XCTL-6503               PIC X(8)    VALUE 'EL6503'.
00082      12  XCTL-6504               PIC X(8)    VALUE 'EL6504'.
00083      12  XCTL-6505               PIC X(8)    VALUE 'EL6505'.
00084      12  XCTL-6506               PIC X(8)    VALUE 'EL6506'.
00085      12  XCTL-652                PIC X(8)    VALUE 'EL652'.
00086      12  XCTL-689                PIC X(8)    VALUE 'EL689'.
00087      12  XCTL-690                PIC X(8)    VALUE 'EL690'.
00088      12  XCTL-653                PIC X(8)    VALUE 'EL653'.
00089      12  LINK-001                PIC X(8)    VALUE 'EL001'.
00090      12  LINK-004                PIC X(8)    VALUE 'EL004'.
00091      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00092      12  FILE-ID                 PIC X(8).
00093      12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.
00094      12  ACCT-FILE-ID            PIC X(8)    VALUE 'ERACCT'.
080807     12  ERACNT-FILE-ID          PIC X(8)    VALUE 'ERACNT'.
00095      12  REIN-FILE-ID            PIC X(8)    VALUE 'ERREIN'.
00096      12  COMM-FILE-ID            PIC X(8)    VALUE 'ERCTBL'.
00097      12  COMP-FILE-ID            PIC X(8)    VALUE 'ERCOMP'.
00098      12  NAME-FILE-ID            PIC X(8)    VALUE 'ERNAME'.
00099      12  RQST-FILE-ID            PIC X(8)    VALUE 'ERRQST'.
00100      12  RQST3-FILE-ID           PIC X(8)    VALUE 'ERRQST3'.
00101      12  ERGXRF-FILE-ID          PIC X(8)    VALUE 'ERGXRF'.
00102      12  ACCT-REC-LEN            PIC S9(4)   VALUE +2000  COMP.
080807     12  ERACNT-REC-LEN          PIC S9(4)   VALUE +120   COMP.
00103      12  SC-ITEM                 PIC S9(4)   VALUE +1     COMP.
00104      12  COMP-REC-LEN            PIC S9(4)   VALUE +700   COMP.
00105      12  NAME-REC-LEN            PIC S9(4)   VALUE +160   COMP.
00106      12  ERGXRF-REC-LEN          PIC S9(4)   VALUE +0     COMP.
00107      12  ERGXRF-INC-LEN          PIC S9(4)   VALUE +31    COMP.
00108      12  ERGXRF-MIN-LEN          PIC S9(4)   VALUE +109   COMP.
00109      12  ERGXRF-MAX-LEN          PIC S9(8)   VALUE +31264 COMP.
CIDMOD     12  CNTL-REC-LEN            PIC S9(4)   VALUE +750   COMP.
CIDMOD     12  JRNL-REC-LEN            PIC S9(4)   VALUE ZERO   COMP.
00110 *                                copy ERCGXRF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCGXRF                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = GENERAL AGENT CROSS REFERENCE             *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 62 - 32,062   RECFORM = VARIABLE
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERGXRF                   RKP=2,LEN=18    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
00021
00022  01  AGENT-CROSS-REFERENCE.
00023      12  GX-RECORD-ID                PIC XX.
00024          88  VALID-GX-ID             VALUE 'GX'.
00025
00026      12  GX-CONTROL-PRIMARY.
00027          16  GX-COMPANY-CD           PIC X.
00028          16  GX-CARRIER              PIC X.
00029          16  GX-GROUPING             PIC X(6).
00030          16  GX-AGENT-NO             PIC X(10).
00031
00032      12  GX-MAINT-INFORMATION.
00033          16  GX-LAST-MAINT-DT        PIC XX.
00034          16  GX-LAST-MAINT-HHMMSS    PIC S9(7)  COMP-3.
00035          16  GX-LAST-MAINT-USER      PIC X(4).
00036          16  FILLER                  PIC X(9).
00037
00038      12  FILLER                      PIC X(37).
00039
00040      12  GX-AGENT-POINTER-CNT        PIC S9(4)  COMP.
00041
00042      12  GX-AGENT-POINTER   OCCURS 1 TO 1006 TIMES
00043                             DEPENDING ON GX-AGENT-POINTER-CNT.
00044          16  GX-AM-CARRIER           PIC X.
00045          16  GX-AM-GROUPING          PIC X(6).
00046          16  GX-AM-STATE             PIC XX.
00047          16  GX-AM-ACCOUNT           PIC X(10).
00048          16  GX-AM-EXPIRATION-DT     PIC XX.
00049          16  GX-AM-LEVEL-NO          PIC S9(4)     COMP.
00050          16  GX-LAST-BILL-DT         PIC XX.
00051          16  GX-AM-EFF-DT            PIC XX.
00052          16  FILLER                  PIC X(4).
00053
00054 ******************************************************************
052113 01  ERPDEF-KEY-SAVE             PIC X(18).
052113 01  ERPDEF-KEY.
052113     12  ERPDEF-COMPANY-CD       PIC X.
052113     12  ERPDEF-STATE            PIC XX.
052113     12  ERPDEF-PROD-CD          PIC XXX.
052113     12  F                       PIC X(7).
052113     12  ERPDEF-BEN-TYPE         PIC X.
052113     12  ERPDEF-BEN-CODE         PIC XX.
052113     12  ERPDEF-EXP-DT           PIC XX.
00111  01  WORK-AREAS-AND-SWITCHES.
           12  ws-comp-cd-dis          pic s9(4) comp value +0.
           12  filler redefines ws-comp-cd-dis.
               16  filler              pic x.
               16  ws-comp-cd          pic x.
00112      12  W-ONE                   PIC S9(04)  COMP   VALUE +1.
CIDMOD     12  WS-TABLE-MAX            PIC S9V9(4) COMP-3 VALUE +0.
00113      12  WS-ST-COMMISSION-CAPS.
00114          16  WS-ST-COMM-CAP-SL     PIC S9V9(4) COMP-3 VALUE +0.
00115          16  WS-ST-COMM-CAP-JL     PIC S9V9(4) COMP-3 VALUE +0.
00116          16  WS-ST-COMM-CAP-SA     PIC S9V9(4) COMP-3 VALUE +0.
00117          16  WS-ST-COMM-CAP-JA     PIC S9V9(4) COMP-3 VALUE +0.
102717     12  WS-ST-GA-COMMISSION-CAPS.
102717         16  WS-ST-GA-COMM-CAP-SL  PIC S9V9(4) COMP-3 VALUE +0.
102717         16  WS-ST-GA-COMM-CAP-JL  PIC S9V9(4) COMP-3 VALUE +0.
102717         16  WS-ST-GA-COMM-CAP-SA  PIC S9V9(4) COMP-3 VALUE +0.
102717         16  WS-ST-GA-COMM-CAP-JA  PIC S9V9(4) COMP-3 VALUE +0.
102717     12  WS-ST-TOT-COMMISSION-CAPS.
102717         16  WS-ST-TOT-COMM-CAP-SL PIC S9V9(4) COMP-3 VALUE +0.
102717         16  WS-ST-TOT-COMM-CAP-JL PIC S9V9(4) COMP-3 VALUE +0.
102717         16  WS-ST-TOT-COMM-CAP-SA PIC S9V9(4) COMP-3 VALUE +0.
102717         16  WS-ST-TOT-COMM-CAP-JA PIC S9V9(4) COMP-3 VALUE +0.
           12  WS-DCC-MAX-MARKET-FEE   PIC S9(5)   COMP-3 VALUE +100.
00118      12  WS-COMM-CAP-LIMIT-TO    PIC X.
102717         88  WS-EXCLUDE-ACCOUNT              VALUE 'A'.
102717         88  WS-EXCLUDE-GA                   VALUE 'G'.
102717         88  WS-EXCLUDE-BOTH                 VALUE 'B'.
00120
00121      12  COMMISSION-ACCUMS.
102717         16  COMM-SL-ACCUM-AL    PIC S9V9(4) COMP-3 VALUE ZERO.
102717         16  COMM-JL-ACCUM-AL    PIC S9V9(4) COMP-3 VALUE ZERO.
102717         16  COMM-AH-ACCUM-AL    PIC S9V9(4) COMP-3 VALUE ZERO.
102717         16  COMM-SL-ACCUM-GL    PIC S9V9(4) COMP-3 VALUE ZERO.
102717         16  COMM-JL-ACCUM-GL    PIC S9V9(4) COMP-3 VALUE ZERO.
102717         16  COMM-AH-ACCUM-GL    PIC S9V9(4) COMP-3 VALUE ZERO.
00122          16  COMM-SL-ACCUM       PIC S9V9(4) COMP-3 VALUE ZERO.
00123          16  COMM-JL-ACCUM       PIC S9V9(4) COMP-3 VALUE ZERO.
00124          16  COMM-AH-ACCUM       PIC S9V9(4) COMP-3 VALUE ZERO.
042310         16  COMM-MFEE-ACCUM     PIC S9(5)   COMP-3 VALUE ZERO.
00125
00126      12  SAVE-FIN-RESP           PIC X(10)   VALUE SPACES.
00127      12  SAVE-ACCT-AGENT         PIC X(10)   VALUE SPACES.
00128      12  SAVE-NEW-ACCT-AGENT     PIC X(10)   VALUE SPACES.
00129      12  SUB1                    PIC S9(4)   VALUE +0 COMP.
00129      12  WS-NDX                  PIC S9(5)   VALUE +0 COMP-3.
00130      12  CURRENT-SAVE            PIC 9(8).
00131      12  BIN-CURRENT-SAVE        PIC XX.
00132      12  CYMD-CURRENT-SAVE       PIC 9(8).
00133      12  SUB                     PIC 99.
00134      12  AGT-SUB                 PIC 99      VALUE ZEROS.
00135      12  VALID-AGT-SW            PIC X       VALUE 'N'.
00136      12  CHANGE-WAS-MADE-SW      PIC X       VALUE 'N'.
00137          88  CHANGE-WAS-MADE                 VALUE 'Y'.
00138
00139      12  WS-EDIT-BEN-CODE        PIC XX.
00140          88  INVALID-BENEFIT-CODE   VALUE '  ' '00'
00141                                           '90' THRU '99'.
00142
00143      12  WS-KEY-SAVE.
00144            18  WS-ACCT-CCGSA-KEY.
00145              20  WS-ACCT-CO             PIC X.
00146              20  WS-ACCT-CARRIER        PIC X.
00147              20  WS-ACCT-GROUPING       PIC X(6).
00148              20  WS-ACCT-STATE          PIC XX.
00149              20  WS-ACCT-ACCOUNT        PIC X(10).
00150            18  WS-ACCT-EXP-DT           PIC XX.
00151            18  WS-ACCT-REST-OF-EXP      PIC X(4).
00152
00153      12  DEEDIT-FIELD                   PIC X(15).
00154      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
CIDMOD     12  DEEDIT-FIELD-V1 REDEFINES DEEDIT-FIELD   PIC S9(14)V9.
CIDMOD     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.
CIDMOD     12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD   PIC S9(10)V9(5).
CIDMOD     12  DEEDIT-FIELD-V6 REDEFINES DEEDIT-FIELD   PIC S9(9)V9(6).
CIDMOD
00156      12  ELCNTL-KEY.
00157          16  CNTL-COMP-ID        PIC XXX.
00158          16  CNTL-REC-TYPE       PIC X.
00159          16  CNTL-ACCESS.
00160              20  FILLER          PIC X(4).
00161          16  CNTL-BUS REDEFINES CNTL-ACCESS.
00162              20  FILLER          PIC XX.
00163              20  CNTL-BUS-TYPE   PIC XX.
00164          16  CNTL-BEN REDEFINES CNTL-ACCESS.
00165              20  FILLER          PIC XX.
00166              20  CNTL-BEN-TYPE   PIC XX.
00167          16  CNTL-SEQ-NO     PIC S9(4)    COMP.
00168
00169      12  REIN-KEY.
00170          16  REIN-COMP-CD        PIC X.
00171          16  REIN-CODE           PIC X.
00172          16  REIN-TABLE          PIC X(3).
00173          16  FILLER              PIC X(3).
00174
CIDMOD     12  WS-HOLD-COMM-KEY        PIC X(7) VALUE LOW-VALUES.
00175      12  COMM-KEY.
00176          16  COMM-COMP-CD        PIC X.
00177          16  COMM-TABLE          PIC XXX.
00178          16  COMM-LF-AH          PIC X.
00179          16  COMM-FILLER         PIC XX    VALUE LOW-VALUES.
00180
00181      12  RQST-KEY.
00182          16  RQST-COMPANY-CD     PIC X     VALUE SPACES.
00183          16  RQST-ENTRY-BATCH    PIC X(6)  VALUE SPACES.
00184
00185      12  RQST3-KEY.
00186          16  RQST3-COMPANY-CD    PIC X     VALUE SPACES.
00187          16  RQST3-CARRIER       PIC X     VALUE SPACES.
00188          16  RQST3-GROUPING      PIC X(6)  VALUE SPACES.
00189          16  RQST3-FIN-RESP      PIC X(10) VALUE SPACES.
00190          16  RQST3-ACCT-AGENT    PIC X(10) VALUE SPACES.
00191          16  RQST3-REFERENCE     PIC X(12) VALUE SPACES.
00192          16  RQST3-BATCH         PIC X(6)  VALUE SPACES.
00193
00194      12  SAVE-RQST3-KEY.
00195          16  FILLER              PIC X     VALUE SPACES.
00196          16  FILLER              PIC X     VALUE SPACES.
00197          16  FILLER              PIC X(6)  VALUE SPACES.
00198          16  FILLER              PIC X(10) VALUE SPACES.
00199          16  FILLER              PIC X(10) VALUE SPACES.
00200          16  FILLER              PIC X(12) VALUE SPACES.
00201          16  FILLER              PIC X(6)  VALUE SPACES.
00202
00203      12  COMP-KEY.
00204          16  COMP-COMPANY-CD     PIC X.
00205          16  COMP-CARRIER        PIC X.
00206          16  COMP-GROUPING       PIC X(6).
00207          16  COMP-RESP-NO        PIC X(10).
00208          16  COMP-ACCOUNT        PIC X(10).
00209          16  COMP-TYPE           PIC X.
00210
00211      12  WS-AXRF-KEY.
00212          16  WS-AXRF-COMPANY-CD  PIC X.
00213          16  WS-AXRF-CARRIER     PIC X.
00214          16  WS-AXRF-GROUPING    PIC X(6).
00215          16  WS-AXRF-AGENT-NO    PIC X(10).
00216
00217      12  GX-AM-CONTROL-PRIMARY.
00218          16  GX-AM-COMPANY-CD    PIC X.
00219          16  GX-REST-OF-KEY      PIC X(20).
00220
00221      12  QID.
00222          16  QID-TERM            PIC X(4).
00223          16  FILLER              PIC X(4)    VALUE '6501'.
00224
00225      12  BROWSE-STARTED-SW       PIC X       VALUE ' '.
00226          88  BROWSE-STARTED         VALUE 'Y'.
00227      12  FIRST-TIME-SW           PIC X       VALUE ' '.
00228          88  FIRST-TIME             VALUE 'Y'.
00229
00230      12  AXRF-SUB                PIC S9(4)  COMP VALUE ZEROS.
00231      12  GX-AGENT-SUB            PIC S9(4)  COMP VALUE ZEROS.
00232      12  COMM-WORK-SUB           PIC S9(4)  COMP VALUE ZEROS.
00233      12  FIND-AGENT-SUB1         PIC S9(4)  COMP VALUE ZEROS.
00234      12  FIND-AGENT-SUB2         PIC S9(4)  COMP VALUE ZEROS.
00235      12  FIND-AGENT-SUB3         PIC S9(4)  COMP VALUE ZEROS.
00236      12  GETMAIN-SW              PIC X       VALUE SPACES.
00237      12  COMMISSION-GETMAIN-SW   PIC X       VALUE SPACES.
00238          88  GETMAIN-ACQUIRED                VALUE 'X'.
00239
00240      12  ER-0000                 PIC X(4)    VALUE '0000'.
00241      12  ER-0002                 PIC X(4)    VALUE '0002'.
00242      12  ER-0004                 PIC X(4)    VALUE '0004'.
00243      12  ER-0008                 PIC X(4)    VALUE '0008'.
00244      12  ER-0023                 PIC X(4)    VALUE '0023'.
00245      12  ER-0029                 PIC X(4)    VALUE '0029'.
00246      12  ER-0033                 PIC X(4)    VALUE '0033'.
           12  ER-0144                 PIC X(4)    VALUE '0144'.
00247      12  ER-0070                 PIC X(4)    VALUE '0070'.
00248      12  ER-0250                 PIC X(4)    VALUE '0250'.
00249      12  ER-0348                 PIC X(4)    VALUE '0348'.
00250      12  ER-0454                 PIC X(4)    VALUE '0454'.
00251      12  ER-0625                 PIC X(4)    VALUE '0625'.
00252      12  ER-0759                 PIC X(4)    VALUE '0759'.
00253      12  ER-0788                 PIC X(4)    VALUE '0788'.
00254      12  ER-0852                 PIC X(4)    VALUE '0852'.
00255      12  ER-0899                 PIC X(4)    VALUE '0899'.
100703     12  ER-1778                 PIC X(4)    VALUE '1778'.
042310     12  ER-1817                 PIC X(4)    VALUE '1817'.
00256      12  ER-1883                 PIC X(4)    VALUE '1883'.
00257      12  ER-1884                 PIC X(4)    VALUE '1884'.
00258      12  ER-1885                 PIC X(4)    VALUE '1885'.
00259      12  ER-1886                 PIC X(4)    VALUE '1886'.
00260      12  ER-1887                 PIC X(4)    VALUE '1887'.
102717     12  ER-1955                 PIC X(4)    VALUE '1955'.
102717     12  ER-1956                 PIC X(4)    VALUE '1956'.
102717     12  ER-1957                 PIC X(4)    VALUE '1957'.
102717     12  ER-1958                 PIC X(4)    VALUE '1958'.
102717     12  ER-1959                 PIC X(4)    VALUE '1959'.
102717     12  ER-1960                 PIC X(4)    VALUE '1960'.
102717     12  ER-1961                 PIC X(4)    VALUE '1961'.
102717     12  ER-1962                 PIC X(4)    VALUE '1962'.
102717     12  ER-1963                 PIC X(4)    VALUE '1963'.
00261      12  ER-2045                 PIC X(4)    VALUE '2045'.
00262      12  ER-2071                 PIC X(4)    VALUE '2071'.
00263      12  ER-2131                 PIC X(4)    VALUE '2131'.
062707*    12  ER-3057                 PIC X(4)    VALUE '3057'.
00264      12  ER-2151                 PIC X(4)    VALUE '2151'.
00265      12  ER-2152                 PIC X(4)    VALUE '2152'.
00266      12  ER-2153                 PIC X(4)    VALUE '2153'.
00267      12  ER-2154                 PIC X(4)    VALUE '2154'.
00268      12  ER-2155                 PIC X(4)    VALUE '2155'.
00269      12  ER-2156                 PIC X(4)    VALUE '2156'.
00270      12  ER-2157                 PIC X(4)    VALUE '2157'.
00271      12  ER-2158                 PIC X(4)    VALUE '2158'.
00272      12  ER-2159                 PIC X(4)    VALUE '2159'.
00273      12  ER-2160                 PIC X(4)    VALUE '2160'.
00274      12  ER-2165                 PIC X(4)    VALUE '2165'.
00275      12  ER-2168                 PIC X(4)    VALUE '2168'.
00276      12  ER-2169                 PIC X(4)    VALUE '2169'.
00277      12  ER-2170                 PIC X(4)    VALUE '2170'.
00278      12  ER-2172                 PIC X(4)    VALUE '2172'.
00279      12  ER-2173                 PIC X(4)    VALUE '2173'.
00280      12  ER-2174                 PIC X(4)    VALUE '2174'.
00281      12  ER-2175                 PIC X(4)    VALUE '2175'.
00282      12  ER-2176                 PIC X(4)    VALUE '2176'.
00283      12  ER-2177                 PIC X(4)    VALUE '2177'.
00284      12  ER-2178                 PIC X(4)    VALUE '2178'.
00285      12  ER-2179                 PIC X(4)    VALUE '2179'.
00286      12  ER-2180                 PIC X(4)    VALUE '2180'.
00287      12  ER-2181                 PIC X(4)    VALUE '2181'.
00288      12  ER-2182                 PIC X(4)    VALUE '2182'.
00289      12  ER-2183                 PIC X(4)    VALUE '2183'.
00290      12  ER-2189                 PIC X(4)    VALUE '2189'.
00291      12  ER-2572                 PIC X(4)    VALUE '2572'.
042809     12  ER-2615                 PIC X(4)    VALUE '2615'.
00292      12  ER-2949                 PIC X(4)    VALUE '2949'.
00293      12  ER-2970                 PIC X(4)    VALUE '2970'.
102717     12  ER-3065                 PIC X(4)    VALUE '3065'.
102717     12  ER-3066                 PIC X(4)    VALUE '3066'.
00294      12  ER-4009                 PIC X(4)    VALUE '4009'.
00295      12  ER-4010                 PIC X(4)    VALUE '4010'.
00296      12  ER-4011                 PIC X(4)    VALUE '4011'.
00297      12  ER-4012                 PIC X(4)    VALUE '4012'.
           12  ER-7717                 PIC X(4)    VALUE '7717'.
           12  er-8156                 pic x(4)    value '8156'.
00298      12  ER-9999                 PIC X(4)    VALUE '9999'.
00299      12  ER-XXXX                 PIC X(4)    VALUE 'XXXX'.
00300
00301      12  WSS-LINE-LIAB-LIMITS.
00302          16  WSS-LF-LIMITS.
00303              20  WSS-L-TYPE          PIC  XX    VALUE SPACES.
00304              20  WSS-LF-ATT-AGE      PIC S99    VALUE +0 COMP-3.
00305              20  WSS-L-LIMITS        OCCURS 4 TIMES.
00306                  24  WSS-LF-LM-AGE   PIC S99     COMP-3.
00307                  24  WSS-LF-LM-DUR   PIC S999    COMP-3.
00308                  24  WSS-LF-LM-AMT   PIC S9(6)   COMP-3.
00309          16  WSS-AH-LIMITS.
00310              20  WSS-A-TYPE          PIC  XX    VALUE SPACES.
00311              20  WSS-AH-ATT-AGE      PIC S99    VALUE +0 COMP-3.
00312              20  WSS-A-LIMITS        OCCURS 4 TIMES.
00313                  24  WSS-AH-LM-AGE   PIC S99     COMP-3.
00314                  24  WSS-AH-LM-DUR   PIC S999    COMP-3.
00315                  24  WSS-AH-LM-MOA   PIC S9(4)   COMP-3.
00316                  24  WSS-AH-LM-AMT   PIC S9(6)   COMP-3.
00317
00318      12  WSS-COMM-STRUCTURE.
00319          16  WSS-DEFN-1.
00320              20  WSS-AGT-COMMS.
00321                  24  WSS-AGT     PIC  X(10)   VALUE SPACES.
00322                  24  WSS-COM-TYP PIC  X       VALUE SPACES.
00323                  24  WSS-L-COM   PIC SV9(5)   VALUE ZEROS COMP-3.
00324                  24  WSS-J-COM   PIC SV9(5)   VALUE ZEROS COMP-3.
00325                  24  WSS-A-COM   PIC SV9(5)   VALUE ZEROS COMP-3.
00326                  24  FILLER      PIC X(6)     VALUE SPACES.
00327
00328      12  WS-PHONE.
00329          16  WS-PH1              PIC XXX.
00330          16  WS-PH2              PIC XXX.
00331          16  WS-PH3              PIC XXXX.
00332      12  WS-PHONE-NUM REDEFINES WS-PHONE PIC 9(10).
00333
00334      12  WS-LIFE-PSI             PIC S9V9(6)  VALUE +0    COMP-3.
00335      12  WS-AH-PSI               PIC S9V9(6)  VALUE +0    COMP-3.
00336      12  WS-ACCOUNT-CTR-C        PIC 99       VALUE ZEROS COMP-3.
00337      12  WS-ACCOUNT-CTR-R        PIC 99       VALUE ZEROS COMP-3.
00338      12  WS-ACCOUNT-CTR-D        PIC 99       VALUE ZEROS COMP-3.
00339      12  WS-BUS-TYPE             PIC 99.
00340      12  WS-BUS-ENTRY            PIC 99.
00341      12  WS-ANVR-DT              PIC 9(11)    VALUE ZEROS.
00342      12  WS-PEFF-DT              PIC 9(11)    VALUE ZEROS.
00343      12  WS-PEXP-DT              PIC 9(11)    VALUE ZEROS.
00344      12  WS-TOL-PREM             PIC S999V99  VALUE ZEROS.
00345      12  WS-TOL-REF              PIC S999V99  VALUE ZEROS.
00346      12  WS-TOL-CLM              PIC S999V99  VALUE ZEROS.
00347      12  WS-COMM-ERROR-SW        PIC 9   COMP-3.
00348          88  COMMISSION-ERROR    VALUE 1.
00349      12  WS-JTYPEID              PIC XX       VALUE SPACES.
00350      12  AGENT-FIND              PIC X        VALUE SPACES.
00351          88  AGENT-RECORD-NOT-FOUND           VALUE 'X'.
00352          88  ACCT-RECORD-NOT-FOUND            VALUE 'Y'.
00353          88  ACCT-LEVEL-DIFFERENT             VALUE 'Z'.
00354
00355      12  WS-ZIP-CODE.
00356          16  WS-ZIP-1            PIC X.
00357              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
00358          16  WS-ZIP-2-3          PIC XX.
00359          16  WS-ZIP-4            PIC X.
00360          16  WS-ZIP-5            PIC X.
00361          16  WS-ZIP-6            PIC X.
00362          16  FILLER              PIC X(4).
00363      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
00364          16  WS-ZIP-AM-1-CODE    PIC X(5).
00365          16  WS-ZIP-AM-1-PLUS4   PIC X(4).
00366          16  FILLER              PIC X.
00367      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
00368          16  WS-ZIP-AM-2-CODE    PIC X(5).
00369          16  WS-ZIP-AM-2-DASH    PIC X.
00370          16  WS-ZIP-AM-2-PLUS4   PIC X(4).
00371      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
00372          16  WS-ZIP-CAN-1-POST1  PIC XXX.
00373          16  WS-ZIP-CAN-1-POST2  PIC XXX.
00374          16  FILLER              PIC X(4).
00375      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
00376          16  WS-ZIP-CAN-2-POST1  PIC XXX.
00377          16  FILLER              PIC X.
00378          16  WS-ZIP-CAN-2-POST2  PIC XXX.
00379          16  FILLER              PIC XXX.
00380      12  WS-SOC-SEC-WORK.
00381          16  WS-SS-TYPE          PIC X.
00382          16  WS-SOC-SEC          PIC X(12).
00383  EJECT
00384 *    COPY ELCSCTM.
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
00385  EJECT
00386 *    COPY ELCSCRTY.
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
00387  EJECT
00388 *    COPY ELCLOGOF.
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
00389  EJECT
00390 *    COPY ELCDATE.
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
00391  EJECT
00392 *    COPY ELCATTR.
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
00393  EJECT
00394 *    COPY ELCEMIB.
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
00395  EJECT
00396 *    COPY ELCINTF.
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
00397 *    COPY ELC650PI.
00001 ******************************************************************
00002 *
00003 *                            ELC650PI.
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007
00006 *  ***  NOTE  ***   IF ANY CHANGES ARE MADE TO THIS COPYBOOK
00007 *   YOU MUST CONSIDER ALL PROGRAMS THAT USE THIS COPYBOOK AND
00008 * PROGRAM EL6565.  ALSO, CONSIDER EL106 AND EL1061
00009 *
00010 ******************************************************************
101916******************************************************************
101916*                   C H A N G E   L O G
101916*
101916* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101916*-----------------------------------------------------------------
101916*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101916* EFFECTIVE    NUMBER
101916*-----------------------------------------------------------------
101916* 101916  IR2016101900001  PEMA  Inc tot line to 3 bytes
00011
00012      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00013          16  PI-MAINT                   PIC X.
00014          16  PI-PREV-ACCOUNT            PIC X(20).
00015          16  PI-PREV-VG-ACCOUNT         PIC X(20).
00016          16  PI-ACCT-KEY.
00017              20  PI-ACCT-CCGSA-KEY.
00018                  24  PI-ACCT-CO             PIC X.
00019                  24  PI-ACCT-CARRIER        PIC X.
00020                  24  PI-ACCT-GROUPING       PIC X(6).
00021                  24  PI-ACCT-STATE          PIC XX.
00022                  24  PI-ACCT-ACCOUNT        PIC X(10).
00023              20  PI-ACCT-EXP-DT           PIC XX.
00024              20  PI-ACCT-REST-OF-EXP      PIC X(4).
00025          16  PI-ACCT-ID                 PIC X(8).
00026          16  PI-PLAN-KEY.
00027              20  PI-PLAN-ACCT-KEY.
00028                  24  PI-PLAN-COMPANY-CD PIC X.
00029                  24  PI-PLAN-CARRIER    PIC X.
00030                  24  PI-PLAN-GROUP      PIC X(6).
00031                  24  PI-PLAN-STATE      PIC X(2).
00032                  24  PI-PLAN-ACCOUNT    PIC X(10).
00033              20  PI-PLAN-BEN-TYPE       PIC X.
00034              20  PI-PLAN-BEN            PIC XX.
00035              20  PI-PLAN-REVISION       PIC X(3).
00036          16  PI-WS-STATE                PIC XX.
00037          16  PI-WS-CLASS                PIC XX.
00038          16  PI-WS-DEV                  PIC X(3).
00039          16  PI-WS-TYPE                 PIC X.
00040          16  PI-WS-PLAN                 PIC XX.
00041
00042          16  PI-ERPNDB-ALT-KEY.
00043              20  PI-PB-COMPANY-CD-A1    PIC X.
00044              20  PI-PB-CARRIER          PIC X.
00045              20  PI-PB-GROUPING         PIC X(6).
00046              20  PI-PB-STATE            PIC XX.
00047              20  PI-PB-ACCOUNT          PIC X(10).
00048              20  PI-PB-CERT-EFF-DT      PIC XX.
00049              20  PI-PB-CERT-NO          PIC X(10).
00050              20  PI-PB-ALT-CHG-SEQ-NO   PIC S9(4)      COMP.
00051              20  PI-PB-RECORD-TYPE      PIC X.
00052
00053          16  PI-DATE-RANGE-TABLE.
00054              20  PI-TABLE-ENT OCCURS 32 TIMES
00055                             INDEXED BY T-INDEX.
00056                  24  PI-BIN-EFF-DT          PIC XX.
00057                  24  PI-BIN-EXP-DT          PIC XX.
00058                  24  PI-BIN-MAINT-DT        PIC XX.
00059                  24  PI-BIN-LO-CERT         PIC XX.
00060                  24  PI-BIN-AR-HI-CERT      PIC XX.
00061                  24  PI-BIN-HI-CERT         PIC XX.
00062          16  PI-PAGE-NUMBER             PIC S9.
00063              88  PI-FST-PAGE               VALUE +1.
00064              88  PI-2ND-PAGE               VALUE +2.
00065              88  PI-3RD-PAGE               VALUE +3.
00066              88  PI-LST-PAGE               VALUE +4.
101916         16  PI-TOTAL-LINES             PIC S999.
00068          16  PI-LINE-SELECTED    PIC S9.
00069 ***  Y2K PROJ 7744
00070          16  EFFCHG-SAVE         PIC 9(11)   COMP-3.
00071          16  BIN-EFFCHG-SAVE     PIC XX.
00072          16  EXPCHG-SAVE         PIC 9(11)   COMP-3.
00073 ***  Y2K PROJ 7744
00074          16  BIN-EXPCHG-SAVE     PIC XX.
00075          16  PI-RECORD-ADDED-SW  PIC X.
00076              88  PI-RECORD-ADDED            VALUE '1'.
00077              88  PI-RECORD-NOT-CREATED      VALUE SPACE.
00078          16  PI-ACCNAME          PIC X(30).
00079          16  PI-COMM-POINTER     PIC S9(8)   COMP.
00080          16  PI-SV-MAINT         PIC X.
00081          16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.
00082          16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.
00083          16  PI-UPDATE-SW        PIC X.
00084              88  PI-CHANGES-MADE             VALUE '1'.
00085          16  PI-NOTE-TYPE        PIC X.
00086              88  PI-ACCT-NOTE                VALUE '1'.
00087          16  PI-DMD-FILE-SW      PIC X.
00088              88  END-OF-FILE                 VALUE 'E'.
00089              88  INTO-NEXT-BENEFITS          VALUE 'I'.
00090              88  FIRST-OCCURS                VALUE 'F'.
00091          16  PI-DMD-OCCURS       PIC S999.
00092          16  PI-DMD-SCREEN       PIC X.
00093              88  SCREEN-1-DISPLAYED  VALUE '1'.
00094              88  SCREEN-2-DISPLAYED  VALUE '2'.
00095              88  SCREEN-3-DISPLAYED  VALUE '3'.
00096          16  PI-NAMEFLG          PIC X.
PEMTST         16  PI-EL650-DEL-SW     PIC X.
               16  PI-MAX-MFEE         PIC S9(5) COMP-3.
               16  PI-DCC-PROD-CODE    PIC XXX.
101916         16  FILLER              PIC X(34).
00098      EJECT
CIDMOD
CIDMOD*    COPY ELCJPFX.
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
CIDMOD         PIC X(2000).
00398  EJECT
00399 *    COPY ELCAID.
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
00400  01  FILLER    REDEFINES DFHAID.
00401      12  FILLER                  PIC X(8).
00402      12  PF-VALUES               PIC X       OCCURS 24 TIMES.
00403  EJECT
00404 *    COPY EL6501S.
       01  EL6501AI.
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
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  GROUPINL PIC S9(0004) COMP.
           05  GROUPINF PIC  X(0001).
           05  FILLER REDEFINES GROUPINF.
               10  GROUPINA PIC  X(0001).
           05  GROUPINI PIC  X(0006).
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
           05  EFFDTEL PIC S9(0004) COMP.
           05  EFFDTEF PIC  X(0001).
           05  FILLER REDEFINES EFFDTEF.
               10  EFFDTEA PIC  X(0001).
           05  EFFDTEI PIC  X(0008).
      *    -------------------------------
           05  EXPDTEL PIC S9(0004) COMP.
           05  EXPDTEF PIC  X(0001).
           05  FILLER REDEFINES EXPDTEF.
               10  EXPDTEA PIC  X(0001).
           05  EXPDTEI PIC  X(0008).
      *    -------------------------------
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
      *    -------------------------------
           05  PCONTL PIC S9(0004) COMP.
           05  PCONTF PIC  X(0001).
           05  FILLER REDEFINES PCONTF.
               10  PCONTA PIC  X(0001).
           05  PCONTI PIC  X(0030).
      *    -------------------------------
           05  CSRL PIC S9(0004) COMP.
           05  CSRF PIC  X(0001).
           05  FILLER REDEFINES CSRF.
               10  CSRA PIC  X(0001).
           05  CSRI PIC  X(0004).
      *    -------------------------------
           05  PDSPLYL PIC S9(0004) COMP.
           05  PDSPLYF PIC  X(0001).
           05  FILLER REDEFINES PDSPLYF.
               10  PDSPLYA PIC  X(0001).
           05  PDSPLYI PIC  X(0006).
      *    -------------------------------
           05  PEFFDTEL PIC S9(0004) COMP.
           05  PEFFDTEF PIC  X(0001).
           05  FILLER REDEFINES PEFFDTEF.
               10  PEFFDTEA PIC  X(0001).
           05  PEFFDTEI PIC  X(0008).
      *    -------------------------------
           05  PEXPDTEL PIC S9(0004) COMP.
           05  PEXPDTEF PIC  X(0001).
           05  FILLER REDEFINES PEXPDTEF.
               10  PEXPDTEA PIC  X(0001).
           05  PEXPDTEI PIC  X(0008).
      *    -------------------------------
           05  NAMEL PIC S9(0004) COMP.
           05  NAMEF PIC  X(0001).
           05  FILLER REDEFINES NAMEF.
               10  NAMEA PIC  X(0001).
           05  NAMEI PIC  X(0030).
      *    -------------------------------
           05  TAXNOL PIC S9(0004) COMP.
           05  TAXNOF PIC  X(0001).
           05  FILLER REDEFINES TAXNOF.
               10  TAXNOA PIC  X(0001).
           05  TAXNOI PIC  X(0011).
      *    -------------------------------
           05  STATUSL PIC S9(0004) COMP.
           05  STATUSF PIC  X(0001).
           05  FILLER REDEFINES STATUSF.
               10  STATUSA PIC  X(0001).
           05  STATUSI PIC  X(0001).
      *    -------------------------------
           05  INCAREL PIC S9(0004) COMP.
           05  INCAREF PIC  X(0001).
           05  FILLER REDEFINES INCAREF.
               10  INCAREA PIC  X(0001).
           05  INCAREI PIC  X(0030).
      *    -------------------------------
           05  PHONEL PIC S9(0004) COMP.
           05  PHONEF PIC  X(0001).
           05  FILLER REDEFINES PHONEF.
               10  PHONEA PIC  X(0001).
           05  PHONEI PIC  X(0012).
      *    -------------------------------
           05  INDGRPL PIC S9(0004) COMP.
           05  INDGRPF PIC  X(0001).
           05  FILLER REDEFINES INDGRPF.
               10  INDGRPA PIC  X(0001).
           05  INDGRPI PIC  X(0001).
      *    -------------------------------
           05  ADDR1L PIC S9(0004) COMP.
           05  ADDR1F PIC  X(0001).
           05  FILLER REDEFINES ADDR1F.
               10  ADDR1A PIC  X(0001).
           05  ADDR1I PIC  X(0030).
      *    -------------------------------
           05  CONTRL PIC S9(0004) COMP.
           05  CONTRF PIC  X(0001).
           05  FILLER REDEFINES CONTRF.
               10  CONTRA PIC  X(0001).
           05  CONTRI PIC  X(0008).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  99.
      *    -------------------------------
           05  ACITYL PIC S9(0004) COMP.
           05  ACITYF PIC  X(0001).
           05  FILLER REDEFINES ACITYF.
               10  ACITYA PIC  X(0001).
           05  ACITYI PIC  X(0028).
      *    -------------------------------
           05  ASTATEL PIC S9(0004) COMP.
           05  ASTATEF PIC  X(0001).
           05  FILLER REDEFINES ASTATEF.
               10  ASTATEA PIC  X(0001).
           05  ASTATEI PIC  X(0002).
      *    -------------------------------
           05  PRODATEL PIC S9(0004) COMP.
           05  PRODATEF PIC  X(0001).
           05  FILLER REDEFINES PRODATEF.
               10  PRODATEA PIC  X(0001).
           05  PRODATEI PIC  X(0008).
      *    -------------------------------
           05  STDBENL PIC S9(0004) COMP.
           05  STDBENF PIC  X(0001).
           05  FILLER REDEFINES STDBENF.
               10  STDBENA PIC  X(0001).
           05  STDBENI PIC  X(0002).
      *    -------------------------------
           05  ZIPL PIC S9(0004) COMP.
           05  ZIPF PIC  X(0001).
           05  FILLER REDEFINES ZIPF.
               10  ZIPA PIC  X(0001).
           05  ZIPI PIC  X(0010).
      *    -------------------------------
           05  CTPLABLL PIC S9(0004) COMP.
           05  CTPLABLF PIC  X(0001).
           05  FILLER REDEFINES CTPLABLF.
               10  CTPLABLA PIC  X(0001).
           05  CTPLABLI PIC  X(0010).
      *    -------------------------------
           05  CLPTOLPL PIC S9(0004) COMP.
           05  CLPTOLPF PIC  X(0001).
           05  FILLER REDEFINES CLPTOLPF.
               10  CLPTOLPA PIC  X(0001).
           05  CLPTOLPI PIC  S9(02)V9999.
      *    -------------------------------
           05  RETROCDL PIC S9(0004) COMP.
           05  RETROCDF PIC  X(0001).
           05  FILLER REDEFINES RETROCDF.
               10  RETROCDA PIC  X(0001).
           05  RETROCDI PIC  X(0003).
      *    -------------------------------
           05  BILLCDL PIC S9(0004) COMP.
           05  BILLCDF PIC  X(0001).
           05  FILLER REDEFINES BILLCDF.
               10  BILLCDA PIC  X(0001).
           05  BILLCDI PIC  X(0003).
      *    -------------------------------
           05  LCLABLL PIC S9(0004) COMP.
           05  LCLABLF PIC  X(0001).
           05  FILLER REDEFINES LCLABLF.
               10  LCLABLA PIC  X(0001).
           05  LCLABLI PIC  X(0011).
      *    -------------------------------
           05  LCOMML PIC S9(0004) COMP.
           05  LCOMMF PIC  X(0001).
           05  FILLER REDEFINES LCOMMF.
               10  LCOMMA PIC  X(0001).
           05  LCOMMI PIC  S9(6)V9(2).
      *    -------------------------------
           05  MAXFEEHL PIC S9(0004) COMP.
           05  MAXFEEHF PIC  X(0001).
           05  FILLER REDEFINES MAXFEEHF.
               10  MAXFEEHA PIC  X(0001).
           05  MAXFEEHI PIC  X(0013).
      *    -------------------------------
           05  MAXMFEEL PIC S9(0004) COMP.
           05  MAXMFEEF PIC  X(0001).
           05  FILLER REDEFINES MAXMFEEF.
               10  MAXMFEEA PIC  X(0001).
           05  MAXMFEEI PIC  X(0005).
      *    -------------------------------
           05  CLPSTHL PIC S9(0004) COMP.
           05  CLPSTHF PIC  X(0001).
           05  FILLER REDEFINES CLPSTHF.
               10  CLPSTHA PIC  X(0001).
           05  CLPSTHI PIC  X(0010).
      *    -------------------------------
           05  CLPSTL PIC S9(0004) COMP.
           05  CLPSTF PIC  X(0001).
           05  FILLER REDEFINES CLPSTF.
               10  CLPSTA PIC  X(0001).
           05  CLPSTI PIC  X(0002).
      *    -------------------------------
           05  PRODCDHL PIC S9(0004) COMP.
           05  PRODCDHF PIC  X(0001).
           05  FILLER REDEFINES PRODCDHF.
               10  PRODCDHA PIC  X(0001).
           05  PRODCDHI PIC  X(0012).
      *    -------------------------------
           05  PRODCDL PIC S9(0004) COMP.
           05  PRODCDF PIC  X(0001).
           05  FILLER REDEFINES PRODCDF.
               10  PRODCDA PIC  X(0001).
           05  PRODCDI PIC  X(0003).
      *    -------------------------------
           05  REMITL PIC S9(0004) COMP.
           05  REMITF PIC  X(0001).
           05  FILLER REDEFINES REMITF.
               10  REMITA PIC  X(0001).
           05  REMITI PIC  99.
      *    -------------------------------
           05  COMMCALL PIC S9(0004) COMP.
           05  COMMCALF PIC  X(0001).
           05  FILLER REDEFINES COMMCALF.
               10  COMMCALA PIC  X(0001).
           05  COMMCALI PIC  X(0001).
      *    -------------------------------
           05  REINTABL PIC S9(0004) COMP.
           05  REINTABF PIC  X(0001).
           05  FILLER REDEFINES REINTABF.
               10  REINTABA PIC  X(0001).
           05  REINTABI PIC  X(0003).
      *    -------------------------------
           05  REINCALL PIC S9(0004) COMP.
           05  REINCALF PIC  X(0001).
           05  FILLER REDEFINES REINCALF.
               10  REINCALA PIC  X(0001).
           05  REINCALI PIC  X(0001).
      *    -------------------------------
           05  AHHEADL PIC S9(0004) COMP.
           05  AHHEADF PIC  X(0001).
           05  FILLER REDEFINES AHHEADF.
               10  AHHEADA PIC  X(0001).
           05  AHHEADI PIC  X(0006).
      *    -------------------------------
           05  AGNT1L PIC S9(0004) COMP.
           05  AGNT1F PIC  X(0001).
           05  FILLER REDEFINES AGNT1F.
               10  AGNT1A PIC  X(0001).
           05  AGNT1I PIC  X(0010).
      *    -------------------------------
           05  TYPE1L PIC S9(0004) COMP.
           05  TYPE1F PIC  X(0001).
           05  FILLER REDEFINES TYPE1F.
               10  TYPE1A PIC  X(0001).
           05  TYPE1I PIC  X(0001).
      *    -------------------------------
           05  SINRT1L PIC S9(0004) COMP.
           05  SINRT1F PIC  X(0001).
           05  FILLER REDEFINES SINRT1F.
               10  SINRT1A PIC  X(0001).
           05  SINRT1I PIC  X(0006).
      *    -------------------------------
           05  JNTRT1L PIC S9(0004) COMP.
           05  JNTRT1F PIC  X(0001).
           05  FILLER REDEFINES JNTRT1F.
               10  JNTRT1A PIC  X(0001).
           05  JNTRT1I PIC  X(0006).
      *    -------------------------------
           05  AHRT1L PIC S9(0004) COMP.
           05  AHRT1F PIC  X(0001).
           05  FILLER REDEFINES AHRT1F.
               10  AHRT1A PIC  X(0001).
           05  AHRT1I PIC  X(0006).
      *    -------------------------------
           05  RECAL1L PIC S9(0004) COMP.
           05  RECAL1F PIC  X(0001).
           05  FILLER REDEFINES RECAL1F.
               10  RECAL1A PIC  X(0001).
           05  RECAL1I PIC  X(0001).
      *    -------------------------------
           05  COMM1L PIC S9(0004) COMP.
           05  COMM1F PIC  X(0001).
           05  FILLER REDEFINES COMM1F.
               10  COMM1A PIC  X(0001).
           05  COMM1I PIC  X(0001).
      *    -------------------------------
           05  CHGBK1L PIC S9(0004) COMP.
           05  CHGBK1F PIC  X(0001).
           05  FILLER REDEFINES CHGBK1F.
               10  CHGBK1A PIC  X(0001).
           05  CHGBK1I PIC  X(0002).
      *    -------------------------------
           05  CCE01L PIC S9(0004) COMP.
           05  CCE01F PIC  X(0001).
           05  FILLER REDEFINES CCE01F.
               10  CCE01A PIC  X(0001).
           05  CCE01I PIC  X(0001).
      *    -------------------------------
           05  AGNT2L PIC S9(0004) COMP.
           05  AGNT2F PIC  X(0001).
           05  FILLER REDEFINES AGNT2F.
               10  AGNT2A PIC  X(0001).
           05  AGNT2I PIC  X(0010).
      *    -------------------------------
           05  TYPE2L PIC S9(0004) COMP.
           05  TYPE2F PIC  X(0001).
           05  FILLER REDEFINES TYPE2F.
               10  TYPE2A PIC  X(0001).
           05  TYPE2I PIC  X(0001).
      *    -------------------------------
           05  SINRT2L PIC S9(0004) COMP.
           05  SINRT2F PIC  X(0001).
           05  FILLER REDEFINES SINRT2F.
               10  SINRT2A PIC  X(0001).
           05  SINRT2I PIC  X(0006).
      *    -------------------------------
           05  JNTRT2L PIC S9(0004) COMP.
           05  JNTRT2F PIC  X(0001).
           05  FILLER REDEFINES JNTRT2F.
               10  JNTRT2A PIC  X(0001).
           05  JNTRT2I PIC  X(0006).
      *    -------------------------------
           05  AHRT2L PIC S9(0004) COMP.
           05  AHRT2F PIC  X(0001).
           05  FILLER REDEFINES AHRT2F.
               10  AHRT2A PIC  X(0001).
           05  AHRT2I PIC  X(0006).
      *    -------------------------------
           05  RECAL2L PIC S9(0004) COMP.
           05  RECAL2F PIC  X(0001).
           05  FILLER REDEFINES RECAL2F.
               10  RECAL2A PIC  X(0001).
           05  RECAL2I PIC  X(0001).
      *    -------------------------------
           05  COMM2L PIC S9(0004) COMP.
           05  COMM2F PIC  X(0001).
           05  FILLER REDEFINES COMM2F.
               10  COMM2A PIC  X(0001).
           05  COMM2I PIC  X(0001).
      *    -------------------------------
           05  CHGBK2L PIC S9(0004) COMP.
           05  CHGBK2F PIC  X(0001).
           05  FILLER REDEFINES CHGBK2F.
               10  CHGBK2A PIC  X(0001).
           05  CHGBK2I PIC  X(0002).
      *    -------------------------------
           05  CCE02L PIC S9(0004) COMP.
           05  CCE02F PIC  X(0001).
           05  FILLER REDEFINES CCE02F.
               10  CCE02A PIC  X(0001).
           05  CCE02I PIC  X(0001).
      *    -------------------------------
           05  AGNT3L PIC S9(0004) COMP.
           05  AGNT3F PIC  X(0001).
           05  FILLER REDEFINES AGNT3F.
               10  AGNT3A PIC  X(0001).
           05  AGNT3I PIC  X(0010).
      *    -------------------------------
           05  TYPE3L PIC S9(0004) COMP.
           05  TYPE3F PIC  X(0001).
           05  FILLER REDEFINES TYPE3F.
               10  TYPE3A PIC  X(0001).
           05  TYPE3I PIC  X(0001).
      *    -------------------------------
           05  SINRT3L PIC S9(0004) COMP.
           05  SINRT3F PIC  X(0001).
           05  FILLER REDEFINES SINRT3F.
               10  SINRT3A PIC  X(0001).
           05  SINRT3I PIC  X(0006).
      *    -------------------------------
           05  JNTRT3L PIC S9(0004) COMP.
           05  JNTRT3F PIC  X(0001).
           05  FILLER REDEFINES JNTRT3F.
               10  JNTRT3A PIC  X(0001).
           05  JNTRT3I PIC  X(0006).
      *    -------------------------------
           05  AHRT3L PIC S9(0004) COMP.
           05  AHRT3F PIC  X(0001).
           05  FILLER REDEFINES AHRT3F.
               10  AHRT3A PIC  X(0001).
           05  AHRT3I PIC  X(0006).
      *    -------------------------------
           05  RECAL3L PIC S9(0004) COMP.
           05  RECAL3F PIC  X(0001).
           05  FILLER REDEFINES RECAL3F.
               10  RECAL3A PIC  X(0001).
           05  RECAL3I PIC  X(0001).
      *    -------------------------------
           05  COMM3L PIC S9(0004) COMP.
           05  COMM3F PIC  X(0001).
           05  FILLER REDEFINES COMM3F.
               10  COMM3A PIC  X(0001).
           05  COMM3I PIC  X(0001).
      *    -------------------------------
           05  CHGBK3L PIC S9(0004) COMP.
           05  CHGBK3F PIC  X(0001).
           05  FILLER REDEFINES CHGBK3F.
               10  CHGBK3A PIC  X(0001).
           05  CHGBK3I PIC  X(0002).
      *    -------------------------------
           05  CCE03L PIC S9(0004) COMP.
           05  CCE03F PIC  X(0001).
           05  FILLER REDEFINES CCE03F.
               10  CCE03A PIC  X(0001).
           05  CCE03I PIC  X(0001).
      *    -------------------------------
           05  AGNT4L PIC S9(0004) COMP.
           05  AGNT4F PIC  X(0001).
           05  FILLER REDEFINES AGNT4F.
               10  AGNT4A PIC  X(0001).
           05  AGNT4I PIC  X(0010).
      *    -------------------------------
           05  TYPE4L PIC S9(0004) COMP.
           05  TYPE4F PIC  X(0001).
           05  FILLER REDEFINES TYPE4F.
               10  TYPE4A PIC  X(0001).
           05  TYPE4I PIC  X(0001).
      *    -------------------------------
           05  SINRT4L PIC S9(0004) COMP.
           05  SINRT4F PIC  X(0001).
           05  FILLER REDEFINES SINRT4F.
               10  SINRT4A PIC  X(0001).
           05  SINRT4I PIC  X(0006).
      *    -------------------------------
           05  JNTRT4L PIC S9(0004) COMP.
           05  JNTRT4F PIC  X(0001).
           05  FILLER REDEFINES JNTRT4F.
               10  JNTRT4A PIC  X(0001).
           05  JNTRT4I PIC  X(0006).
      *    -------------------------------
           05  AHRT4L PIC S9(0004) COMP.
           05  AHRT4F PIC  X(0001).
           05  FILLER REDEFINES AHRT4F.
               10  AHRT4A PIC  X(0001).
           05  AHRT4I PIC  X(0006).
      *    -------------------------------
           05  RECAL4L PIC S9(0004) COMP.
           05  RECAL4F PIC  X(0001).
           05  FILLER REDEFINES RECAL4F.
               10  RECAL4A PIC  X(0001).
           05  RECAL4I PIC  X(0001).
      *    -------------------------------
           05  COMM4L PIC S9(0004) COMP.
           05  COMM4F PIC  X(0001).
           05  FILLER REDEFINES COMM4F.
               10  COMM4A PIC  X(0001).
           05  COMM4I PIC  X(0001).
      *    -------------------------------
           05  CHGBK4L PIC S9(0004) COMP.
           05  CHGBK4F PIC  X(0001).
           05  FILLER REDEFINES CHGBK4F.
               10  CHGBK4A PIC  X(0001).
           05  CHGBK4I PIC  X(0002).
      *    -------------------------------
           05  CCE04L PIC S9(0004) COMP.
           05  CCE04F PIC  X(0001).
           05  FILLER REDEFINES CCE04F.
               10  CCE04A PIC  X(0001).
           05  CCE04I PIC  X(0001).
      *    -------------------------------
           05  AGNT5L PIC S9(0004) COMP.
           05  AGNT5F PIC  X(0001).
           05  FILLER REDEFINES AGNT5F.
               10  AGNT5A PIC  X(0001).
           05  AGNT5I PIC  X(0010).
      *    -------------------------------
           05  TYPE5L PIC S9(0004) COMP.
           05  TYPE5F PIC  X(0001).
           05  FILLER REDEFINES TYPE5F.
               10  TYPE5A PIC  X(0001).
           05  TYPE5I PIC  X(0001).
      *    -------------------------------
           05  SINRT5L PIC S9(0004) COMP.
           05  SINRT5F PIC  X(0001).
           05  FILLER REDEFINES SINRT5F.
               10  SINRT5A PIC  X(0001).
           05  SINRT5I PIC  X(0006).
      *    -------------------------------
           05  JNTRT5L PIC S9(0004) COMP.
           05  JNTRT5F PIC  X(0001).
           05  FILLER REDEFINES JNTRT5F.
               10  JNTRT5A PIC  X(0001).
           05  JNTRT5I PIC  X(0006).
      *    -------------------------------
           05  AHRT5L PIC S9(0004) COMP.
           05  AHRT5F PIC  X(0001).
           05  FILLER REDEFINES AHRT5F.
               10  AHRT5A PIC  X(0001).
           05  AHRT5I PIC  X(0006).
      *    -------------------------------
           05  RECAL5L PIC S9(0004) COMP.
           05  RECAL5F PIC  X(0001).
           05  FILLER REDEFINES RECAL5F.
               10  RECAL5A PIC  X(0001).
           05  RECAL5I PIC  X(0001).
      *    -------------------------------
           05  COMM5L PIC S9(0004) COMP.
           05  COMM5F PIC  X(0001).
           05  FILLER REDEFINES COMM5F.
               10  COMM5A PIC  X(0001).
           05  COMM5I PIC  X(0001).
      *    -------------------------------
           05  CHGBK5L PIC S9(0004) COMP.
           05  CHGBK5F PIC  X(0001).
           05  FILLER REDEFINES CHGBK5F.
               10  CHGBK5A PIC  X(0001).
           05  CHGBK5I PIC  X(0002).
      *    -------------------------------
           05  CCE05L PIC S9(0004) COMP.
           05  CCE05F PIC  X(0001).
           05  FILLER REDEFINES CCE05F.
               10  CCE05A PIC  X(0001).
           05  CCE05I PIC  X(0001).
      *    -------------------------------
           05  AGNT6L PIC S9(0004) COMP.
           05  AGNT6F PIC  X(0001).
           05  FILLER REDEFINES AGNT6F.
               10  AGNT6A PIC  X(0001).
           05  AGNT6I PIC  X(0010).
      *    -------------------------------
           05  TYPE6L PIC S9(0004) COMP.
           05  TYPE6F PIC  X(0001).
           05  FILLER REDEFINES TYPE6F.
               10  TYPE6A PIC  X(0001).
           05  TYPE6I PIC  X(0001).
      *    -------------------------------
           05  SINRT6L PIC S9(0004) COMP.
           05  SINRT6F PIC  X(0001).
           05  FILLER REDEFINES SINRT6F.
               10  SINRT6A PIC  X(0001).
           05  SINRT6I PIC  X(0006).
      *    -------------------------------
           05  JNTRT6L PIC S9(0004) COMP.
           05  JNTRT6F PIC  X(0001).
           05  FILLER REDEFINES JNTRT6F.
               10  JNTRT6A PIC  X(0001).
           05  JNTRT6I PIC  X(0006).
      *    -------------------------------
           05  AHRT6L PIC S9(0004) COMP.
           05  AHRT6F PIC  X(0001).
           05  FILLER REDEFINES AHRT6F.
               10  AHRT6A PIC  X(0001).
           05  AHRT6I PIC  X(0006).
      *    -------------------------------
           05  RECAL6L PIC S9(0004) COMP.
           05  RECAL6F PIC  X(0001).
           05  FILLER REDEFINES RECAL6F.
               10  RECAL6A PIC  X(0001).
           05  RECAL6I PIC  X(0001).
      *    -------------------------------
           05  COMM6L PIC S9(0004) COMP.
           05  COMM6F PIC  X(0001).
           05  FILLER REDEFINES COMM6F.
               10  COMM6A PIC  X(0001).
           05  COMM6I PIC  X(0001).
      *    -------------------------------
           05  CHGBK6L PIC S9(0004) COMP.
           05  CHGBK6F PIC  X(0001).
           05  FILLER REDEFINES CHGBK6F.
               10  CHGBK6A PIC  X(0001).
           05  CHGBK6I PIC  X(0002).
      *    -------------------------------
           05  CCE06L PIC S9(0004) COMP.
           05  CCE06F PIC  X(0001).
           05  FILLER REDEFINES CCE06F.
               10  CCE06A PIC  X(0001).
           05  CCE06I PIC  X(0001).
      *    -------------------------------
           05  AGNT7L PIC S9(0004) COMP.
           05  AGNT7F PIC  X(0001).
           05  FILLER REDEFINES AGNT7F.
               10  AGNT7A PIC  X(0001).
           05  AGNT7I PIC  X(0010).
      *    -------------------------------
           05  TYPE7L PIC S9(0004) COMP.
           05  TYPE7F PIC  X(0001).
           05  FILLER REDEFINES TYPE7F.
               10  TYPE7A PIC  X(0001).
           05  TYPE7I PIC  X(0001).
      *    -------------------------------
           05  SINRT7L PIC S9(0004) COMP.
           05  SINRT7F PIC  X(0001).
           05  FILLER REDEFINES SINRT7F.
               10  SINRT7A PIC  X(0001).
           05  SINRT7I PIC  X(0006).
      *    -------------------------------
           05  JNTRT7L PIC S9(0004) COMP.
           05  JNTRT7F PIC  X(0001).
           05  FILLER REDEFINES JNTRT7F.
               10  JNTRT7A PIC  X(0001).
           05  JNTRT7I PIC  X(0006).
      *    -------------------------------
           05  AHRT7L PIC S9(0004) COMP.
           05  AHRT7F PIC  X(0001).
           05  FILLER REDEFINES AHRT7F.
               10  AHRT7A PIC  X(0001).
           05  AHRT7I PIC  X(0006).
      *    -------------------------------
           05  RECAL7L PIC S9(0004) COMP.
           05  RECAL7F PIC  X(0001).
           05  FILLER REDEFINES RECAL7F.
               10  RECAL7A PIC  X(0001).
           05  RECAL7I PIC  X(0001).
      *    -------------------------------
           05  COMM7L PIC S9(0004) COMP.
           05  COMM7F PIC  X(0001).
           05  FILLER REDEFINES COMM7F.
               10  COMM7A PIC  X(0001).
           05  COMM7I PIC  X(0001).
      *    -------------------------------
           05  CHGBK7L PIC S9(0004) COMP.
           05  CHGBK7F PIC  X(0001).
           05  FILLER REDEFINES CHGBK7F.
               10  CHGBK7A PIC  X(0001).
           05  CHGBK7I PIC  X(0002).
      *    -------------------------------
           05  CCE07L PIC S9(0004) COMP.
           05  CCE07F PIC  X(0001).
           05  FILLER REDEFINES CCE07F.
               10  CCE07A PIC  X(0001).
           05  CCE07I PIC  X(0001).
      *    -------------------------------
           05  AGNT8L PIC S9(0004) COMP.
           05  AGNT8F PIC  X(0001).
           05  FILLER REDEFINES AGNT8F.
               10  AGNT8A PIC  X(0001).
           05  AGNT8I PIC  X(0010).
      *    -------------------------------
           05  TYPE8L PIC S9(0004) COMP.
           05  TYPE8F PIC  X(0001).
           05  FILLER REDEFINES TYPE8F.
               10  TYPE8A PIC  X(0001).
           05  TYPE8I PIC  X(0001).
      *    -------------------------------
           05  SINRT8L PIC S9(0004) COMP.
           05  SINRT8F PIC  X(0001).
           05  FILLER REDEFINES SINRT8F.
               10  SINRT8A PIC  X(0001).
           05  SINRT8I PIC  X(0006).
      *    -------------------------------
           05  JNTRT8L PIC S9(0004) COMP.
           05  JNTRT8F PIC  X(0001).
           05  FILLER REDEFINES JNTRT8F.
               10  JNTRT8A PIC  X(0001).
           05  JNTRT8I PIC  X(0006).
      *    -------------------------------
           05  AHRT8L PIC S9(0004) COMP.
           05  AHRT8F PIC  X(0001).
           05  FILLER REDEFINES AHRT8F.
               10  AHRT8A PIC  X(0001).
           05  AHRT8I PIC  X(0006).
      *    -------------------------------
           05  RECAL8L PIC S9(0004) COMP.
           05  RECAL8F PIC  X(0001).
           05  FILLER REDEFINES RECAL8F.
               10  RECAL8A PIC  X(0001).
           05  RECAL8I PIC  X(0001).
      *    -------------------------------
           05  COMM8L PIC S9(0004) COMP.
           05  COMM8F PIC  X(0001).
           05  FILLER REDEFINES COMM8F.
               10  COMM8A PIC  X(0001).
           05  COMM8I PIC  X(0001).
      *    -------------------------------
           05  CHGBK8L PIC S9(0004) COMP.
           05  CHGBK8F PIC  X(0001).
           05  FILLER REDEFINES CHGBK8F.
               10  CHGBK8A PIC  X(0001).
           05  CHGBK8I PIC  X(0002).
      *    -------------------------------
           05  CCE08L PIC S9(0004) COMP.
           05  CCE08F PIC  X(0001).
           05  FILLER REDEFINES CCE08F.
               10  CCE08A PIC  X(0001).
           05  CCE08I PIC  X(0001).
      *    -------------------------------
           05  AGNT9L PIC S9(0004) COMP.
           05  AGNT9F PIC  X(0001).
           05  FILLER REDEFINES AGNT9F.
               10  AGNT9A PIC  X(0001).
           05  AGNT9I PIC  X(0010).
      *    -------------------------------
           05  TYPE9L PIC S9(0004) COMP.
           05  TYPE9F PIC  X(0001).
           05  FILLER REDEFINES TYPE9F.
               10  TYPE9A PIC  X(0001).
           05  TYPE9I PIC  X(0001).
      *    -------------------------------
           05  SINRT9L PIC S9(0004) COMP.
           05  SINRT9F PIC  X(0001).
           05  FILLER REDEFINES SINRT9F.
               10  SINRT9A PIC  X(0001).
           05  SINRT9I PIC  X(0006).
      *    -------------------------------
           05  JNTRT9L PIC S9(0004) COMP.
           05  JNTRT9F PIC  X(0001).
           05  FILLER REDEFINES JNTRT9F.
               10  JNTRT9A PIC  X(0001).
           05  JNTRT9I PIC  X(0006).
      *    -------------------------------
           05  AHRT9L PIC S9(0004) COMP.
           05  AHRT9F PIC  X(0001).
           05  FILLER REDEFINES AHRT9F.
               10  AHRT9A PIC  X(0001).
           05  AHRT9I PIC  X(0006).
      *    -------------------------------
           05  RECAL9L PIC S9(0004) COMP.
           05  RECAL9F PIC  X(0001).
           05  FILLER REDEFINES RECAL9F.
               10  RECAL9A PIC  X(0001).
           05  RECAL9I PIC  X(0001).
      *    -------------------------------
           05  COMM9L PIC S9(0004) COMP.
           05  COMM9F PIC  X(0001).
           05  FILLER REDEFINES COMM9F.
               10  COMM9A PIC  X(0001).
           05  COMM9I PIC  X(0001).
      *    -------------------------------
           05  CHGBK9L PIC S9(0004) COMP.
           05  CHGBK9F PIC  X(0001).
           05  FILLER REDEFINES CHGBK9F.
               10  CHGBK9A PIC  X(0001).
           05  CHGBK9I PIC  X(0002).
      *    -------------------------------
           05  CCE09L PIC S9(0004) COMP.
           05  CCE09F PIC  X(0001).
           05  FILLER REDEFINES CCE09F.
               10  CCE09A PIC  X(0001).
           05  CCE09I PIC  X(0001).
      *    -------------------------------
           05  AGNT10L PIC S9(0004) COMP.
           05  AGNT10F PIC  X(0001).
           05  FILLER REDEFINES AGNT10F.
               10  AGNT10A PIC  X(0001).
           05  AGNT10I PIC  X(0010).
      *    -------------------------------
           05  TYPE10L PIC S9(0004) COMP.
           05  TYPE10F PIC  X(0001).
           05  FILLER REDEFINES TYPE10F.
               10  TYPE10A PIC  X(0001).
           05  TYPE10I PIC  X(0001).
      *    -------------------------------
           05  SINRT10L PIC S9(0004) COMP.
           05  SINRT10F PIC  X(0001).
           05  FILLER REDEFINES SINRT10F.
               10  SINRT10A PIC  X(0001).
           05  SINRT10I PIC  X(0006).
      *    -------------------------------
           05  JNTRT10L PIC S9(0004) COMP.
           05  JNTRT10F PIC  X(0001).
           05  FILLER REDEFINES JNTRT10F.
               10  JNTRT10A PIC  X(0001).
           05  JNTRT10I PIC  X(0006).
      *    -------------------------------
           05  AHRT10L PIC S9(0004) COMP.
           05  AHRT10F PIC  X(0001).
           05  FILLER REDEFINES AHRT10F.
               10  AHRT10A PIC  X(0001).
           05  AHRT10I PIC  X(0006).
      *    -------------------------------
           05  RECAL10L PIC S9(0004) COMP.
           05  RECAL10F PIC  X(0001).
           05  FILLER REDEFINES RECAL10F.
               10  RECAL10A PIC  X(0001).
           05  RECAL10I PIC  X(0001).
      *    -------------------------------
           05  COMM10L PIC S9(0004) COMP.
           05  COMM10F PIC  X(0001).
           05  FILLER REDEFINES COMM10F.
               10  COMM10A PIC  X(0001).
           05  COMM10I PIC  X(0001).
      *    -------------------------------
           05  CHGBK10L PIC S9(0004) COMP.
           05  CHGBK10F PIC  X(0001).
           05  FILLER REDEFINES CHGBK10F.
               10  CHGBK10A PIC  X(0001).
           05  CHGBK10I PIC  X(0002).
      *    -------------------------------
           05  CCE10L PIC S9(0004) COMP.
           05  CCE10F PIC  X(0001).
           05  FILLER REDEFINES CCE10F.
               10  CCE10A PIC  X(0001).
           05  CCE10I PIC  X(0001).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0075).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
      *    -------------------------------
           05  PF1PF2L PIC S9(0004) COMP.
           05  PF1PF2F PIC  X(0001).
           05  FILLER REDEFINES PF1PF2F.
               10  PF1PF2A PIC  X(0001).
           05  PF1PF2I PIC  X(0035).
       01  EL6501AO REDEFINES EL6501AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPINO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCOUNTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCONTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDSPLYO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PEFFDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PEXPDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAXNOO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCAREO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CONTRO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRODATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STDBENO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTPLABLO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLPTOLPO PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETROCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILLCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCLABLO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCOMMO PIC  ZZ99.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXFEEHO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXMFEEO PIC  Z,Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLPSTHO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLPSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRODCDHO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRODCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REMITO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMMCALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REINTABO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REINCALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHEADO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SINRT10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTRT10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRT10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGBK10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCE10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF1PF2O PIC  X(0035).
      *    -------------------------------
00405  01  MAP-R REDEFINES EL6501AI.
100703*    12  FILLER                  PIC X(518).
092705*    12  FILLER                  PIC X(543).
092705     12  FILLER                  PIC X(546).
00407      12  MAP-AGENT-AREA.
00408          16  AGENT-AREA OCCURS 10 TIMES INDEXED BY M-INDEX.
00409              20  AGENTL          PIC S9(4) COMP.
00410              20  AGENTA          PIC X.
00411              20  AGENT           PIC X(10).
00412              20  ATYPEL          PIC S9(4) COMP.
00413              20  ATYPEA          PIC X.
00414              20  ATYPE           PIC X.
00415              20  SINGLEL         PIC S9(4) COMP.
00416              20  SINGLEA         PIC X.
00417              20  SINGLE-COMM-T   PIC X(6).
00418              20  SINGLE-COMM-N REDEFINES SINGLE-COMM-T.
00419                  24  SINGLE-COMM-R PIC V9(5).
00420                  24  SINGLE-DASH   PIC X.
00421              20  SINGLE-COMM-O REDEFINES SINGLE-COMM-T
00422                                  PIC V9(5)-.
00423              20  JOINTL          PIC S9(4) COMP.
00424              20  JOINTA          PIC X.
00425              20  JOINT-COMM-T    PIC X(6).
00426              20  JOINT-COMM-N REDEFINES JOINT-COMM-T.
00427                  24  JOINT-COMM-R PIC V9(5).
00428                  24  JOINT-DASH   PIC X.
00429              20  JOINT-COMM-O REDEFINES JOINT-COMM-T
00430                                  PIC V9(5)-.
00431              20  A-HL            PIC S9(4) COMP.
00432              20  A-HA            PIC X.
00433              20  A-H-COMM-T      PIC X(6).
00434              20  A-H-COMM-N  REDEFINES A-H-COMM-T.
00435                  24  A-H-COMM-R  PIC V9(5).
00436                  24  A-H-DASH    PIC X.
00437              20  A-H-COMM-O  REDEFINES A-H-COMM-T
00438                                  PIC V9(5)-.
00439              20  RECALL          PIC S9(4) COMP.
00440              20  RECALA          PIC X.
00441              20  RECAL           PIC X.
00442              20  RCOMML          PIC S9(4) COMP.
00443              20  RCOMMA          PIC X.
00444              20  RCOMM           PIC X.
00445              20  CHGBCKL         PIC S9(4) COMP.
00446              20  CHGBCKA         PIC X.
00447              20  CHGBCK          PIC X(02).
102717             20  CCEL            PIC S9(4) COMP.
102717             20  CCEA            PIC X.
102717             20  CCEIND          PIC X.
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
00453  01  DFHCOMMAREA                 PIC X(1500).
00454  EJECT
00455 *01 PARMLIST .
00456 *    02  FILLER                  PIC S9(8)   COMP.
00457 *    02  ERACCT-POINTER          PIC S9(8)   COMP.
00458 *    02  ELCNTL-POINTER          PIC S9(8)   COMP.
00459 *    02  REIN-POINTER            PIC S9(8)   COMP.
00460 *    02  CTBL-POINTER            PIC S9(8)   COMP.
00461 *    02  COMM-POINTER            PIC S9(8)   COMP.
00462 *    02  COMP-POINTER            PIC S9(8)   COMP.
00463 *    02  NAME-POINTER            PIC S9(8)   COMP.
00464 *    02  RQST-POINTER            PIC S9(8)   COMP.
00465 *    02  AXRF-POINTER            PIC S9(8)   COMP.
00466 *    02  AXRF2-POINTER           PIC S9(8)   COMP.
00467 *    02  AXRF3-POINTER           PIC S9(8)   COMP.
00468 *    02  AXRF4-POINTER           PIC S9(8)   COMP.
00469 *    02  AXRF5-POINTER           PIC S9(8)   COMP.
00470 *    02  AXRF6-POINTER           PIC S9(8)   COMP.
00471 *    02  AXRF7-POINTER           PIC S9(8)   COMP.
00472 *    02  AXRF8-POINTER           PIC S9(8)   COMP.
00474 *    COPY ERCACCT.
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
      *    copy ERCPDEF.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCPDEF.                            *
      *                                                                *
      *    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
      *                                                                *
      *    FILE TYPE = VSAM,KSDS                                       *
      *    RECORD SIZE = 1319 RECFORM = FIXED                          *
      *                                                                *
      *    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
      *                                                                *
      *    LOG = YES                                                   *
      *    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
051414*                   C H A N G E   L O G
051414*
051414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051414*-----------------------------------------------------------------
051414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051414* EFFECTIVE    NUMBER
051414*-----------------------------------------------------------------
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
      ******************************************************************
       01  PRODUCT-MASTER.
          12  PD-RECORD-ID                 PIC X(02).
              88  VALID-PD-ID                  VALUE 'PD'.
          12  PD-CONTROL-PRIMARY.
              16  PD-COMPANY-CD            PIC X.
              16  PD-STATE                 PIC XX.
              16  PD-PRODUCT-CD            PIC XXX.
              16  PD-FILLER                PIC X(7).
              16  PD-BEN-TYPE              PIC X.
              16  PD-BEN-CODE              PIC XX.
              16  PD-PROD-EXP-DT           PIC XX.
          12  FILLER                       PIC X(50).
          12  PD-PRODUCT-DATA OCCURS 8.
              16  PD-PROD-CODE             PIC X.
                  88  PD-PROD-LIFE           VALUE 'L'.
                  88  PD-PROD-PROP           VALUE 'P'.
                  88  PD-PROD-AH             VALUE 'A'.
                  88  PD-PROD-IU             VALUE 'I'.
                  88  PD-PROD-GAP            VALUE 'G'.
052614            88  PD-PROD-FAML           VALUE 'F'.
100518            88  PD-PROD-OTH            VALUE 'O'.
              16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
              16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
              16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
              16  PD-MAX-TERM              PIC S999        COMP-3.
              16  PD-MAX-AMT               PIC S9(07)      COMP-3.
              16  FILLER                   PIC X.
              16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
              16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
              16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
              16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
              16  PD-CRIT-PERIOD           PIC S999        COMP-3.
              16  PD-REC-CRIT-PERIOD       PIC 99.
              16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
                  20  PD-RECURRING-YN      PIC X.
                  20  FILLER               PIC X.
              16  PD-RTW-MOS               PIC 99.
051414        16  PD-MAX-EXTENSION         PIC 99.
100314        16  pd-ben-pct               pic sv999 comp-3.
100314*       16  FILLER                   PIC XX.
          12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
          12  PD-TERM-LIMITS OCCURS 15.
              16  PD-LOW-TERM              PIC S999        COMP-3.
              16  PD-HI-TERM               PIC S999        COMP-3.
      *  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
          12  PD-LOAN-AMT-LIMITS OCCURS 15.
              16  PD-LOW-AMT               PIC S9(5)       COMP-3.
              16  PD-HI-AMT                PIC S9(7)       COMP-3.
          12  PD-EARN-FACTORS.
              16  FILLER OCCURS 15.
                  20  FILLER OCCURS 15.
                      24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
          12  PD-PRODUCT-DESC              PIC X(80).
          12  PD-TRUNCATED                 PIC X.
          12  FILLER                       PIC X(59).
          12  PD-MAINT-INFORMATION.
              16  PD-LAST-MAINT-DT         PIC X(02).
              16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
              16  PD-LAST-MAINT-BY         PIC X(04).
00476 *    COPY ELCCNTL.
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
00478 *    COPY ERCREIN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCREIN                             *
00003 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00004 *                            VMOD=2.010                          *
00005 *                                                                *
00006 *   ONLINE CREDIT SYSTEM                                         *
00007 *                                                                *
00008 *   FILE DESCRIPTION = REINSURANCE MASTER FILE                   *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 4000  RECFORM = FIXED                          *
00012 *                                                                *
00013 *   BASE CLUSTER NAME = ERREIN                   RKP=2,LEN=8     *
00014 *       ALTERNATE PATH = NONE                                    *
00015 *                                                                *
00016 *   LOG = NO                                                     *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 *                                                                *
00019 ******************************************************************
103101*                   C H A N G E   L O G
103101*
103101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103101*-----------------------------------------------------------------
103101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103101* EFFECTIVE    NUMBER
103101*-----------------------------------------------------------------
103101* 103101    2001100100006  SMVA  ADD STATE EXHIBIT REPORT OPTION F
032707* 032707    2007032100006  PEMA  ADD EXCISE TAX CAPABILITY
103101******************************************************************
00021  01  REINSURANCE-RECORD.
00022      12  RE-RECORD-ID                      PIC XX.
00023          88  VALID-RE-ID                      VALUE 'RE'.
00024
00025      12  RE-CONTROL-PRIMARY.
00026          16  RE-COMPANY-CD                 PIC X.
00027          16  RE-KEY.
00028              20  RE-CODE                   PIC X.
00029                  88  RE-TABLE-RECORD          VALUE 'A'.
00030                  88  RE-COMPANY-RECORD        VALUE 'B'.
00031              20  RE-TABLE                  PIC XXX.
00032              20  FILLER                    PIC XXX.
00033          16  RE-COMPANY-KEY REDEFINES RE-KEY.
00034              20  FILLER                    PIC X.
00035              20  RE-COMPANY.
00036                  24  RE-COMP-PRIME         PIC XXX.
00037                  24  RE-COMP-SUB           PIC XXX.
00038
00039      12  RE-MAINT-INFORMATION.
00040          16  RE-LAST-MAINT-DT              PIC XX.
00041          16  RE-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00042          16  RE-LAST-MAINT-USER            PIC X(4).
00043          16  FILLER                        PIC X(10).
00044
00045      12  RE-TABLE-DATA.
00046          16  RE-100-COMP                   PIC 99.
00047
00048          16  RE-COMP-INFO    OCCURS 30 TIMES.
00049              20  RE-REI-COMP-NO.
00050                  24  RE-REI-COMP           PIC XXX.
00051                  24  RE-REI-COMP-SUB       PIC XXX.
00052              20  RE-LF-QC                  PIC X.
00053              20  RE-AH-QC                  PIC X.
00054              20  RE-LO-DATE                PIC 9(11)     COMP-3.
00055              20  RE-HI-DATE                PIC 9(11)     COMP-3.
00056              20  RE-LFAGE-LO               PIC 99.
00057              20  RE-LFAGE-HI               PIC 99.
00058              20  RE-AHAGE-LO               PIC 99.
00059              20  RE-AHAGE-HI               PIC 99.
00060              20  RE-LFTRM-LO               PIC S999       COMP-3.
00061              20  RE-LFTRM-HI               PIC S999       COMP-3.
00062              20  RE-AHTRM-LO               PIC S999       COMP-3.
00063              20  RE-AHTRM-HI               PIC S999       COMP-3.
00064              20  RE-LF-PCT                 PIC S9V9999    COMP-3.
00065              20  RE-AH-PCT                 PIC S9V9999    COMP-3.
00066              20  RE-LF-LIM-LO              PIC S9(9)V99   COMP-3.
00067              20  RE-LF-LIM-HI              PIC S9(9)V99   COMP-3.
00068              20  RE-LF-LO                  PIC S9(9)V99   COMP-3.
00069              20  RE-LF-HI                  PIC S9(9)V99   COMP-3.
00070              20  RE-AHBEN-LIM-LO           PIC S9(7)V99   COMP-3.
00071              20  RE-AHBEN-LIM-HI           PIC S9(7)V99   COMP-3.
00072              20  RE-AHBEN-LO               PIC S9(7)V99   COMP-3.
00073              20  RE-AHBEN-HI               PIC S9(7)V99   COMP-3.
00074              20  RE-AHMOA-LIM-LO           PIC S9(7)V99   COMP-3.
00075              20  RE-AHMOA-LIM-HI           PIC S9(7)V99   COMP-3.
00076              20  RE-AHMOA-LO               PIC S9(7)V99   COMP-3.
00077              20  RE-AHMOA-HI               PIC S9(7)V99   COMP-3.
00078              20  RE-LF-BEN-CODE            PIC X.
00079              20  RE-AH-BEN-CODE            PIC X.
00080              20  RE-INTERACTIVE            PIC X.
00081              20  RE-REMAINING              PIC X.
CIDMOD             20  RE-LF-RUNOFF-SW           PIC X.
CIDMOD             20  RE-AH-RUNOFF-SW           PIC X.
CIDMOD             20  FILLER                    PIC X(19).
00083
00084          16  RE-COMP-INFO-END              PIC X(6).
00085          16  RE-NSP-ST-CD-LF               PIC XX.
00086          16  RE-NSP-ST-CD-AH               PIC XX.
00087          16  RE-TABLE-CARRIER-SECURITY     PIC X.
00088              88  NO-TABLE-CARRIER-SECURITY    VALUE SPACE.
00089
00090          16  FILLER                        PIC X(27).
00091
00092      12  RE-COMPANY-DATA   REDEFINES   RE-TABLE-DATA.
00093          16  RE-NAME                       PIC X(30).
00094          16  RE-LF-PE                      PIC X.
00095          16  RE-AH-PE                      PIC X.
00096          16  RE-LF-FEE                     PIC S9V9999    COMP-3.
00097          16  RE-AH-FEE                     PIC S9V9999    COMP-3.
00098          16  RE-AH-PR-PCT                  PIC S9V9999    COMP-3.
00099          16  RE-AH-78-PCT                  PIC S9V9999    COMP-3.
00100          16  RE-PRT-ST                     PIC X.
00101          16  RE-PRT-OW                     PIC X.
00102          16  RE-MORT-CODE                  PIC X(4).
00103          16  RE-CLAIM-CODE                 PIC X.
00104          16  RE-ZERO-LF-FEE                PIC X.
00105          16  RE-ZERO-AH-FEE                PIC X.
00106          16  RE-CEDE-NAME                  PIC X(30).
00107          16  RE-LF-COMM                    PIC X.
00108          16  RE-AH-COMM                    PIC X.
00109          16  RE-LF-TAX                     PIC X.
00110          16  RE-AH-TAX                     PIC X.
00111          16  RE-CLM-INCURRED-LIM           PIC 9(11)  COMP-3.
00116          16  RE-LF-IBNR-PCT                PIC SV999      COMP-3.
00117          16  RE-AH-IBNR-PCT                PIC SV999      COMP-3.
00118
00119          16  RE-COMP-CARRIER-SECURITY      PIC X.
00120              88  NO-COMP-CARRIER-SECURITY     VALUE SPACE.
00121
00122          16  RE-LF-CEDING-FEE-BRACKETS.
00123              20  RE-LF-FEE-METHOD          PIC X.
00124                  88  RE-LF-FEE-BRACKETED         VALUE '1' '2'.
00125                  88  RE-LF-FEE-METHOD-1          VALUE '1'.
00126                  88  RE-LF-FEE-METHOD-2          VALUE '2'.
00127                  88  RE-LF-FEE-PERCENT           VALUE ' ' 'P'.
00128              20  RE-LF-FEE-BASIS           PIC X.
00129                  88  RE-LF-GROSS-CEDED             VALUE '1'.
00130                  88  RE-LF-NET-CEDED               VALUE '2'.
00131                  88  RE-LF-GROSS-WRITTEN           VALUE '3'.
00132                  88  RE-LF-NET-WRITTEN             VALUE '4'.
00133                  88  RE-LF-COMBINE-GROSS-CEDED     VALUE '5'.
00134                  88  RE-LF-COMBINE-NET-CEDED       VALUE '6'.
00135                  88  RE-LF-COMBINE-GROSS-WRITTEN   VALUE '7'.
00136                  88  RE-LF-COMBINE-NET-WRITTEN     VALUE '8'.
00137              20  FILLER                    PIC XXX.
00138              20  RE-LF-FEE-RANGES  OCCURS 6 TIMES.
00139                  24  RE-LF-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00140                  24  RE-LF-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00141
00142          16  RE-AH-CEDING-FEE-BRACKETS.
00143              20  RE-AH-FEE-METHOD          PIC X.
00144                  88  RE-AH-FEE-BRACKETED         VALUE '1' '2'.
00145                  88  RE-AH-FEE-METHOD-1          VALUE '1'.
00146                  88  RE-AH-FEE-METHOD-2          VALUE '2'.
00147                  88  RE-AH-FEE-PERCENT           VALUE ' ' 'P'.
00148              20  RE-AH-FEE-BASIS           PIC X.
00149                  88  RE-AH-GROSS-CEDED             VALUE '1'.
00150                  88  RE-AH-NET-CEDED               VALUE '2'.
00151                  88  RE-AH-GROSS-WRITTEN           VALUE '3'.
00152                  88  RE-AH-NET-WRITTEN             VALUE '4'.
00153                  88  RE-AH-COMBINE-GROSS-CEDED     VALUE '5'.
00154                  88  RE-AH-COMBINE-NET-CEDED       VALUE '6'.
00155                  88  RE-AH-COMBINE-GROSS-WRITTEN   VALUE '7'.
00156                  88  RE-AH-COMBINE-NET-WRITTEN     VALUE '8'.
00157              20  FILLER                    PIC XXX.
00158              20  RE-AH-FEE-RANGES  OCCURS 6 TIMES.
00159                  24  RE-AH-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00160                  24  RE-AH-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00161
00162          16  RE-EARNING-START-DT           PIC 9(11)  COMP-3.
00166
00167          16  RE-OLD-CEDING-STMT            PIC X.
00168
00169          16  RE-LF-CLM-PCT                 PIC S9V9999    COMP-3.
00170          16  RE-AH-CLM-PCT                 PIC S9V9999    COMP-3.
00171          16  RE-LF-CLM-MAX                 PIC S9(7)V99   COMP-3.
00172          16  RE-AH-CLM-MAX                 PIC S9(7)V99   COMP-3.
00173          16  RE-LF-PR-PCT                  PIC S9V9999    COMP-3.
00174          16  RE-LF-78-PCT                  PIC S9V9999    COMP-3.
00175          16  RE-REINS-GROUPING-CODE        PIC X(6).
00176          16  RE-MORT-SW                    PIC X.
00177          16  RE-CEDING-TYPE-FLAG           PIC X.
00178              88  RE-NO-CESSION-TYPE                VALUE ' '.
00179              88  RE-CEDED                          VALUE 'C'.
00180              88  RE-ASSUMED                        VALUE 'A'.
00181              88  RE-PHANTOM                        VALUE 'P'.
00182
00183          16  RE-CEDING-STMT-OPT-A          PIC X.
00184              88  REPORT-A-WANTED    VALUE ' ' 'Y'.
00185          16  RE-CEDING-STMT-OPT-B          PIC X.
00186              88  REPORT-B-WANTED    VALUE ' ' 'Y'.
00187          16  RE-CEDING-STMT-OPT-C          PIC X.
00188              88  REPORT-C-WANTED    VALUE ' ' 'Y'.
00189          16  RE-CEDING-STMT-OPT-D          PIC X.
00190              88  REPORT-D-WANTED    VALUE ' ' 'Y'.
00191          16  RE-CEDING-STMT-OPT-E          PIC X.
00192              88  REPORT-E-WANTED    VALUE ' ' 'Y'.
00193
00194          16  RE-PRT-CRSV                   PIC X.
00195
00196          16  RE-GL-CENTER                  PIC X(4).
00197
00198          16  RE-CUSTODIAL-BAL              PIC S9(7)V99   COMP-3.
00199
00200          16  RE-EARNING-STOP-DT            PIC 9(11)  COMP-3.
00204
00205          16  RE-EARN-STOP-CODE             PIC X.
00206              88  STOP-LIFE-EARNING  VALUE 'L' 'B'.
00207              88  STOP-AH-EARNING    VALUE 'A' 'B'.
00208
103101         16  RE-STATE-EXHIBIT-OPT-F        PIC X.
103101             88  RPTF-ECS152-WANTED VALUE ' ' 'Y'.
103101
032707         16  RE-EXCISE-TAX                 PIC S9V9999 COMP-3.
032707         16  FILLER                        PIC X(2281).
00210
00211          16  RE-DESC OCCURS 18 TIMES       PIC X(79).
00212
00213 ******************************************************************
00480 *    COPY ERCCTBL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCTBL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION TABLE                        *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 200   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCTBL                   RKP=2,LEN=7     *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  COMM-TABLE-RECORD.
00024      12  CT-RECORD-ID                      PIC XX.
00025          88  VALID-CT-ID                      VALUE 'CT'.
00026
00027      12  CT-CONTROL-PRIMARY.
00028          16  CT-COMPANY-CD                 PIC X.
00029          16  CT-TABLE                      PIC XXX.
00030          16  CT-CNTRL-2.
00031              20  CT-BEN-TYPE               PIC X.
00032              20  CT-BEN-CODE               PIC XX.
00033
00034      12  CT-MAINT-INFORMATION.
00035          16  CT-LAST-MAINT-DT              PIC XX.
00036          16  CT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00037          16  CT-LAST-MAINT-USER            PIC X(4).
00038          16  FILLER                        PIC X(31).
00039
00040      12  CT-LIMITS.
00041          16  CT-TBF OCCURS 3 TIMES         PIC S9(7)V99   COMP-3.
00042
00043          16  CT-AGE OCCURS 3 TIMES         PIC S99        COMP-3.
00044
00045          16  CT-TRM OCCURS 3 TIMES         PIC S999       COMP-3.
00046
00047      12  CT-RATES.
00048          16  CT-RTX          OCCURS 27 TIMES.
00049              20  CT-RT                     PIC SV9(5)     COMP-3.
00050              20  CT-RT-R   REDEFINES
00051                  CT-RT                     PIC XXX.
00052
00053      12  FILLER                            PIC  X(42).
00054
00055 ******************************************************************
080807*                                COPY ERCACNT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACNT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = NOTE FILE FOR RECORDING OF ACCOUNT NOTES  *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 120   RECFORM = FIXED                          *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERACNT             RKP=2,LEN=23          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
110706*                   C H A N G E   L O G
110706*
110706* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110706*-----------------------------------------------------------------
110706*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110706* EFFECTIVE    NUMBER
110706*-----------------------------------------------------------------
110706* 110706  CR2006071700004  PEMA  ADD BRANCH LOCATIONS
110706*           AND SHIPPING ADDRESS TO ACCOUNT NOTES FILE
110706******************************************************************
00019  01  NOTE-FILE.
00020      12  NT-FILE-ID                  PIC XX.
00021          88  VALID-NOTE-ID              VALUE 'NT'.
00022
00023      12  NT-CONTROL-PRIMARY.
00024          16  NT-COMPANY-CD           PIC X.
00027          16  NT-ACCT-NOTE-KEY.
00028              18  NT-CARRIER              PIC X.
00029              18  NT-GROUPING             PIC X(06).
00030              18  NT-STATE                PIC XX.
00031              18  NT-ACCOUNT              PIC X(10).
00025          16  NT-RECORD-TYPE          PIC X.
00026               88  ACCT-NOTE          VALUE '1'.
110706              88  ACCT-BRANCH-LOC    VALUE '2'.
110706              88  ACCT-SHIPPING-ADDR VALUE '3'.
00032          16  NT-LINE-SEQUENCE        PIC S9(4)     COMP.
00033
00034      12  NT-LAST-MAINT-DT            PIC XX.
00035      12  NT-LAST-MAINT-BY            PIC X(4).
00036      12  NT-LAST-MAINT-HHMMSS        PIC S9(7) COMP-3.
00037
110706*  ALL NOTE LINES ARE RECORD TYPE '1' WITH ALMOST UNLIMITED
110706*     SEQUENCE NUMBERS
110706     12  NT-NOTE-INFORMATION.
110706         16  NT-NOTE-LINE            PIC X(60).
00040          16  FILLER                  PIC X(25).
110706*  BOTH BRANCH LOCATION LINES ARE RECORD TYPE '2' SEQ 1 AND 2
110706     12  NT-LOCATION-INFORMATION REDEFINES
110706                         NT-NOTE-INFORMATION.
110706         16  NT-BRANCH-LOC-LINE      PIC X(60).
110706         16  FILLER                  PIC X(25).
052918* Account special indicator is record type '2', sequence 3
052918     12  filler REDEFINES NT-NOTE-INFORMATION.
052918         16  nt-account-special      PIC X.
052918         16  FILLER                  PIC X(84).
110706*  ALL SHIPPING ADDRESS LINES ARE RECORD TYPE '3'AND
      *     SEQUENCE NUMBER 1 IS NAME LINE 1
      *     SEQUENCE NUMBER 2 IS NAME LINE 2
      *     SEQUENCE NUMBER 3 IS ADDR LINE 1
      *     SEQUENCE NUMBER 4 IS ADDR LINE 2
      *     SEQUENCE NUMBER 5 IS ADDR LINE 3
      *     SEQUENCE NUMBER 6 IS CITY, ST AND ZIP
110706     12  NT-SHIPPING-INFORMATION REDEFINES
110706                         NT-NOTE-INFORMATION.
               16  NT-SHIPPING-LINE        PIC X(60).
110706         16  NT-SHIP-STATE           PIC XX.
110706         16  NT-SHIP-ZIP             PIC X(10).
110706         16  FILLER                  PIC X(13).
00041 *****************************************************************
CIDMOD
CIDMOD 01  COMMISSION-WORK-AREA.
00482      05  WK-AGT-COMMS   OCCURS 10 TIMES.
00483          10  WK-AM-AGT           PIC X(10).
00484          10  WK-AM-TYPE          PIC X.
00485          10  WK-FILLER           PIC X(13).
00486          10  WK-GA-BILL-DT       PIC XX.
00487  EJECT
00489 *    COPY ERCCOMP.
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
00490  EJECT
00491 *    COPY ERCNAME.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCNAME                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER NAME, COMPENSATION MASTER       *
00008 *       NAME, REINSURANCE COMPANY NAME LOOKUP FILE.              *
00009 *                                                                *
00010 *   FILE DESCRIPTION = NAME LOOKUP FILE                          *
00011 *                                                                *
00012 *   FILE TYPE = VSAM,KSDS                                        *
00013 *   RECORD SIZE = 160   RECFORM = FIX                            *
00014 *                                                                *
00015 *   BASE CLUSTER NAME = ERNAME                    RKP=2,LEN=61   *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  NAME-LOOKUP-MASTER.
00024      12  NL-RECORD-ID                PIC  X(02).
00025          88  VALID-NL-ID                         VALUE 'NL'.
00026
00027      12  NL-RECORD-KEY.
00028          16  NL-CONTROL-PRIMARY.
00029              20  NL-COMPANY-CD       PIC  X(01).
00030              20  NL-NAME             PIC  X(30).
00031              20  NL-RECORD-TYPE      PIC  X(01).
00032                  88  NL-ACCOUNT-TYPE             VALUE 'A'.
00033                  88  NL-COMPENSATION-TYPE        VALUE 'C'.
00034                  88  NL-REINSURANCE-TYPE         VALUE 'R'.
00035
00036          16  NL-ACCOUNT-MASTER.
00037              20  NL-AM-COMPANY-CD    PIC  X(01).
00038              20  NL-AM-CARRIER       PIC  X(01).
00039              20  NL-AM-GROUPING      PIC  X(06).
00040              20  NL-AM-STATE         PIC  X(02).
00041              20  NL-AM-ACCOUNT       PIC  X(10).
00042              20  FILLER              PIC  X(09).
00043
00044          16  NL-COMPENSATION-MASTER
00045                                  REDEFINES  NL-ACCOUNT-MASTER.
00046              20  NL-CO-COMPANY-CD    PIC  X(01).
00047              20  NL-CO-CARRIER       PIC  X(01).
00048              20  NL-CO-GROUPING      PIC  X(06).
00049              20  NL-CO-RESP-NO       PIC  X(10).
00050              20  NL-CO-ACCOUNT       PIC  X(10).
00051              20  NL-CO-TYPE          PIC  X(01).
00052
00053          16  NL-REINSURANCE-RECORD
00054                                  REDEFINES  NL-ACCOUNT-MASTER.
00055              20  NL-RE-COMPANY-CD    PIC  X(01).
00056              20  NL-RE-CODE          PIC  X(01).
00057              20  NL-RE-COMPANY.
00058                  24  NL-RE-COMP      PIC  X(03).
00059                  24  NL-RE-CO-SUB    PIC  X(03).
00060              20  NL-RE-TABLE         PIC  X(03).
00061              20  FILLER              PIC  X(18).
00062
00063      12  NL-MAINT-INFORMATION.
00064          16  NL-LAST-MAINT-DT        PIC  X(02).
00065          16  NL-LAST-MAINT-HHMMSS    PIC S9(07)  COMP-3.
00066          16  NL-LAST-MAINT-USER      PIC  X(04).
00067          16  FILLER                  PIC  X(10).
00068
00069      12  NL-RE-LEVELS  OCCURS  30  TIMES.
00070          16  NL-RE-LEVEL             PIC  9(02).
00071
00072      12  NL-CITY                     PIC  X(15).
00073      12  NL-ST                       PIC  XX.
00074
00075 ******************************************************************
00492  EJECT
00493 *    COPY ERCRQST.
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
00494 *EJECT
00495 *    COPY ERCGXRF.
00496  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ACCOUNT-MASTER
                                PRODUCT-MASTER CONTROL-FILE
                                REINSURANCE-RECORD COMM-TABLE-RECORD
                                NOTE-FILE COMMISSION-WORK-AREA
                                COMPENSATION-MASTER
                                NAME-LOOKUP-MASTER AR-REQUEST-RECORD.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6501' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00498      MOVE EIBTRMID              TO QID-TERM.
00499
00500      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00501      MOVE '5'                   TO DC-OPTION-CODE.
00502      PERFORM 9700-DATE-LINK.
00503      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00504      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00505
00506 ***  Y2K PROJ 7744
00507      MOVE DC-GREG-DATE-1-YMD    TO  CYMD-CURRENT-SAVE
00508                                     CURRENT-SAVE
00509      MOVE DC-ALPHA-CENTURY      TO  CYMD-CURRENT-SAVE(1:2)
00510                                     CURRENT-SAVE(1:2)
00511 ***  Y2K PROJ 7744
00512
00513      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00514      IF EIBCALEN = 0
00515          GO TO 8800-UNAUTHORIZED-ACCESS.
00516
00517      MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN.
00518      MOVE LOW-VALUES             TO EL6501AI.
00519
00520      
      * EXEC CICS HANDLE CONDITION
00521 *        PGMIDERR  (9600-PGMID-ERROR)
00522 *        ERROR     (9990-ABEND)
00523 *        QIDERR    (0500-CHECK-MAINT-TYPE)
00524 *    END-EXEC.
      *    MOVE '"$L.N                 ! " #00005875' TO DFHEIV0
           MOVE X'22244C2E4E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035383735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00525
00526      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00527          IF PI-CALLING-PROGRAM = XCTL-650
00528              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00529              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00530              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00531              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00532              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00533              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00534              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00535              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00536              PERFORM 5200-DELETE-TS THRU 5299-EXIT
00537              GO TO 0500-CHECK-MAINT-TYPE
00538          ELSE
00539              PERFORM 5200-RECOVER-TS  THRU  5299-EXIT
00540              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00541              GO TO 0600-DISPLAY-RECORD.
00542
00543      IF EIBAID = DFHCLEAR
00544          GO TO 9400-CLEAR.
00545
00546  EJECT
00547  0200-RECEIVE.
00548      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00549          MOVE ER-0008            TO EMI-ERROR
00550          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00551          MOVE -1                 TO PFENTERL
00552          GO TO 8200-SEND-DATAONLY.
00553
00554      
      * EXEC CICS RECEIVE
00555 *        MAP      (MAP-NAME)
00556 *        MAPSET   (MAPSET-NAME)
00557 *        INTO     (EL6501AI)
00558 *    END-EXEC.
           MOVE LENGTH OF
            EL6501AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005909' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6501AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00559
00560      IF PFENTERL = 0
00561          GO TO 0300-CHECK-PFKEYS.
00562      IF EIBAID NOT = DFHENTER
00563          MOVE ER-0004            TO EMI-ERROR
00564          GO TO 0320-INPUT-ERROR.
00565      IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)
00566          MOVE PF-VALUES (PFENTERI) TO EIBAID
00567      ELSE
00568          MOVE ER-0029            TO EMI-ERROR
00569          GO TO 0320-INPUT-ERROR.
00570
00571  0300-CHECK-PFKEYS.
00572      IF EIBAID = DFHPF23
00573          GO TO 8810-PF23.
00574      IF EIBAID = DFHPF24
00575          GO TO 9200-RETURN-MAIN-MENU.
00576      IF EIBAID = DFHPF12
00577          GO TO 9500-PF12.
00578
00579 * CHECK PF10 KEY FOR A CALL TO EL608 TO BROWSE RESIDENT STATE TAX
00580 *             MASTER(ERRESC)
00581 *
00582 *    IF  EIBAID = DFHPF10
00583 *        IF PI-COMPANY-ID = 'DMD'
00584 *           PERFORM 0700-FORMAT-KEYDATA  THRU 0700-EXIT
00585 *           MOVE LOW-VALUES TO WS-DMD-AGENT
00586 *           PERFORM 6500-READ-ACCT       THRU 6599-EXIT
00587 *           PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT
00588 *           PERFORM 6000-MOVE-TO-SCREEN  THRU 6099-EXIT
00589 *           MOVE EIBAID       TO PI-ENTRY-CD-1
00590 *           MOVE WS-COMP-DATE TO PI-PREV-VG-ACCOUNT (11:5)
00591 *           MOVE WS-DMD-AGENT TO PI-PREV-VG-ACCOUNT (1:10)
00592 *           MOVE XCTL-608           TO PGM-NAME
00593 *                                      PI-RETURN-TO-PROGRAM
00594 *                                      PI-CALLING-PROGRAM
00595 *           MOVE '106E'             TO PI-CURRENT-SCREEN-NO
00596 *           GO TO 9300-XCTL.
00597
00598      IF (EIBAID = DFHPF1  OR  DFHPF2  OR DFHPF3 OR
00599                   DFHPF4  OR  DFHPF5  OR DFHPF7 OR
00600                   DFHPF8  OR  DFHPF9 OR DFHPF10)
00601          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00602              MOVE ER-0029        TO  EMI-ERROR
00603              GO TO 0320-INPUT-ERROR
00604          ELSE
00605              GO TO 0800-CHECK-MAINT.
00606
00607      IF EIBAID = DFHENTER
00608          GO TO 0330-EDIT-DATA.
00609
00610      MOVE ER-0029                TO EMI-ERROR.
00611
00612  0320-INPUT-ERROR.
00613      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00614      MOVE AL-UNBON               TO PFENTERA.
00615      MOVE -1                     TO PFENTERL.
00616      GO TO 8200-SEND-DATAONLY.
00617  EJECT
00618  0330-EDIT-DATA.
00619      IF MAINTI  = 'S'
00620         PERFORM 0700-FORMAT-KEYDATA  THRU 0700-EXIT
00621         PERFORM 6500-READ-ACCT       THRU 6599-EXIT
00622         PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT
00623         PERFORM 6000-MOVE-TO-SCREEN  THRU 6099-EXIT
00624         MOVE -1                  TO MAINTL
00625         GO TO 8100-SEND-INITIAL-MAP.
00626
CIDMOD     PERFORM 8000-STATE-REC-READ   THRU 8010-EXIT.
00627      PERFORM 6800-COMPANY-REC-READ THRU 6899-EXIT.
00628
00629      IF EMI-ERROR NOT = ZEROS
00630          MOVE -1                 TO MAINTL
00631          GO TO 8200-SEND-DATAONLY.
00632
00633      IF NOT MODIFY-CAP
00634          MOVE 'UPDATE'           TO SM-READ
00635          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00636          MOVE ER-0070            TO EMI-ERROR
00637          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00638          GO TO 8100-SEND-INITIAL-MAP.
00639
00640      MOVE SAVE-BIN-DATE          TO BIN-CURRENT-SAVE.
00641
00642      IF MAINTI = 'A'
00643         GO TO 1000-ADD-A-RECORD.
00644
00645      IF MAINTI = 'C'
00646         IF PI-SV-MAINT  =  'L'
00647             MOVE -1             TO MAINTL
00648             MOVE AL-UABON       TO MAINTA
00649             MOVE ER-0899        TO EMI-ERROR
00650             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00651             GO TO 8200-SEND-DATAONLY
00652         ELSE
00653             GO TO 2000-CHANGE-A-RECORD.
00654
00655      IF MAINTI = 'L'
00656         MOVE 'L'                TO PI-SV-MAINT
00657         GO TO 3000-CHANGE-ALL-RECORDS.
00658
00659      MOVE ER-0023                TO EMI-ERROR.
00660      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00661      MOVE -1                     TO MAINTL.
00662      MOVE AL-UABON               TO MAINTA.
00663      GO TO 8200-SEND-DATAONLY.
00664  EJECT
00665  0500-CHECK-MAINT-TYPE.
00666      IF PI-MAINT = 'A'
00667         PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT
00668         PERFORM 0700-FORMAT-KEYDATA THRU 0700-EXIT
00669         MOVE -1                  TO NAMEL
00670         GO TO 8100-SEND-INITIAL-MAP.
00671
00672  0600-DISPLAY-RECORD.
00673      PERFORM 0700-FORMAT-KEYDATA  THRU 0700-EXIT.
00674      PERFORM 6500-READ-ACCT       THRU 6599-EXIT.
00675      PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT.
00676      PERFORM 6000-MOVE-TO-SCREEN  THRU 6099-EXIT.
00677
00678      IF PI-MAINT = 'A'
00679         MOVE -1                  TO NAMEL
00680      ELSE
00681         MOVE -1                  TO MAINTL.
00682
00683      GO TO 8100-SEND-INITIAL-MAP.
00684
00685  0700-FORMAT-KEYDATA.
00686      MOVE PI-MAINT               TO MAINTI.
00687
00688      IF PI-MAINT = 'A'
00689         MOVE AL-PANON            TO MAINTA
00690         SET T-INDEX TO 1
00691      ELSE
00692         SET T-INDEX              TO PI-LINE-SELECTED
00693         MOVE AL-UANON            TO MAINTA.
00694
00695      IF PI-2ND-PAGE
00696          SET T-INDEX UP BY 8
00697      ELSE
00698      IF PI-3RD-PAGE
00699          SET T-INDEX UP BY 16
00700      ELSE
00701      IF PI-LST-PAGE
00702          SET T-INDEX UP BY 24.
00703
00704      MOVE PI-ACCT-CARRIER        TO CARRI.
00705      MOVE PI-ACCT-GROUPING       TO GROUPINI.
00706      MOVE PI-ACCT-STATE          TO STATEI.
00707      MOVE PI-ACCT-ACCOUNT        TO ACCOUNTI.
00708
00709      IF PI-ACCT-EXP-DT = LOW-VALUES
00710         MOVE SPACES              TO EXPDTEI
00711      ELSE
00712         IF PI-ACCT-EXP-DT NOT = HIGH-VALUES
00713            MOVE PI-ACCT-EXP-DT      TO DC-BIN-DATE-1
00714            MOVE SPACE               TO DC-OPTION-CODE
00715            PERFORM 9700-DATE-LINK
00716            MOVE DC-GREG-DATE-1-EDIT TO EXPDTEI
00717         ELSE
00718            MOVE 999999              TO EXPDTEO
00719            INSPECT EXPDTEI CONVERTING SPACES TO '/'.
00720
00721 ***DMD CUSTOM CODE *****START
00722      MOVE EXPDTEI (1:2)             TO WS-YYYYMMDD (5:2).
00723      MOVE EXPDTEI (4:2)             TO WS-YYYYMMDD (7:2).
00724      MOVE EXPDTEI (6:2)             TO WS-YYYYMMDD (3:2).
00725      IF EXPDTEI (7:1) IS NOT EQUAL TO '9'
00726         MOVE '20'                 TO WS-YYYYMMDD (1:2)
00727      ELSE
00728         MOVE '19'                 TO WS-YYYYMMDD (1:2).
00729 ***DMD CUSTOM CODE *****END
00730
00731      MOVE PI-BIN-EFF-DT (T-INDEX) TO DC-BIN-DATE-1.
00732      MOVE SPACE                   TO DC-OPTION-CODE.
00733      PERFORM 9700-DATE-LINK.
00734      MOVE DC-GREG-DATE-1-EDIT     TO EFFDTEI.
00735
00736  0700-EXIT.
00737       EXIT.
00738
00739  0800-CHECK-MAINT.
00740      IF PI-MAINT = 'A'
00741         IF PI-RECORD-NOT-CREATED
00742            MOVE ER-2071          TO EMI-ERROR
00743            MOVE  -1              TO NAMEL
00744            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00745            GO TO 8200-SEND-DATAONLY.
00746
00747      IF  EIBAID = DFHPF1
00748              AND
00749          CREDIT-SESSION
00750          PERFORM 5100-WRITE-TS THRU 5199-EXIT
00751          MOVE PI-ACCT-CARRIER    TO PI-CARRIER
00752          MOVE PI-ACCT-GROUPING   TO PI-GROUPING
00753          MOVE PI-ACCT-STATE      TO PI-STATE
00754          MOVE PI-ACCT-ACCOUNT    TO PI-ACCOUNT
00755          MOVE PI-ACCT-EXP-DT     TO PI-CERT-EFF-DT
00756          MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA
00757          MOVE XCTL-689           TO PGM-NAME
00758          GO TO 9300-XCTL.
00759
CIDMOD     IF  EIBAID = DFHPF2 AND CREDIT-SESSION
CIDMOD         PERFORM 5100-WRITE-TS THRU 5199-EXIT
CIDMOD         MOVE PI-ACCT-CARRIER    TO PI-CARRIER
CIDMOD         MOVE PI-ACCT-GROUPING   TO PI-GROUPING
CIDMOD         MOVE PI-ACCT-STATE      TO PI-STATE
CIDMOD         MOVE PI-ACCT-ACCOUNT    TO PI-ACCOUNT
CIDMOD         MOVE LOW-VALUES         TO PI-CERT-EFF-DT
CIDMOD         MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA
CIDMOD         MOVE XCTL-690           TO PGM-NAME
CIDMOD         GO TO 9300-XCTL
CIDMOD     END-IF.
00772
00773      IF EIBAID = DFHPF3
00774              AND
00775          CREDIT-SESSION
00776          PERFORM 5100-WRITE-TS THRU 5199-EXIT
00777          MOVE PI-ACCT-CARRIER    TO PI-CARRIER
00778          MOVE PI-ACCT-GROUPING   TO PI-GROUPING
00779          MOVE PI-ACCT-STATE      TO PI-STATE
00780          MOVE PI-ACCT-ACCOUNT    TO PI-ACCOUNT
00781          MOVE LOW-VALUES         TO PI-CERT-EFF-DT
00782          MOVE XCTL-653           TO PGM-NAME
00783          GO TO 9300-XCTL.
00784
00785      IF EIBAID = DFHPF4
00786          PERFORM 5100-WRITE-TS  THRU  5199-EXIT
00787          MOVE XCTL-652           TO PGM-NAME
00788          GO TO 9300-XCTL.
00789
00790      IF EIBAID = DFHPF5
00791          MOVE XCTL-6502          TO PGM-NAME
00792          GO TO 9300-XCTL.
00793
00794      IF EIBAID = DFHPF7
00795          MOVE XCTL-6504          TO PGM-NAME
00796          GO TO 9300-XCTL.
00797
00798      IF EIBAID = DFHPF8
00799          MOVE XCTL-6506          TO PGM-NAME
00800          GO TO 9300-XCTL.
00801
00802      IF EIBAID = DFHPF9
00803         MOVE XCTL-6505           TO PGM-NAME
00804         GO TO 9300-XCTL.
00805
00806      IF EIBAID = DFHPF10
00807         IF PI-COMPANY-ID = 'DMD'
00808            PERFORM 5100-WRITE-TS THRU 5199-EXIT
00809            MOVE XCTL-608         TO PGM-NAME
00810            GO TO 9300-XCTL.
00811
00812      MOVE ER-0029                TO EMI-ERROR.
00813      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00814      MOVE AL-UNBON               TO PFENTERA.
00815      MOVE -1                     TO PFENTERL.
00816      GO TO 8200-SEND-DATAONLY.
00817  EJECT
00818  1000-ADD-A-RECORD.
00819      IF PI-MAINT = 'A'
00820         PERFORM 4000-EDITS THRU 4900-EXIT
00821      ELSE
00822         MOVE ER-2180             TO EMI-ERROR
00823         MOVE -1                  TO MAINTL
00824         MOVE AL-UABON            TO MAINTA
00825         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00826
00827      IF EMI-FORCABLE-CTR GREATER ZERO  OR
00828         EMI-FATAL-CTR    GREATER ZERO
00829          GO TO 8200-SEND-DATAONLY.
00830
00831      
      * EXEC CICS GETMAIN
00832 *         LENGTH   (ACCT-REC-LEN)
00833 *         SET      (ADDRESS OF ACCOUNT-MASTER)
00834 *         INITIMG  (GETMAIN-SPACE)
00835 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006186' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACCT-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00836
00837      MOVE ZEROS          TO WSS-LF-LM-AGE (1) WSS-LF-LM-AGE (2)
00838                             WSS-LF-LM-AGE (3) WSS-LF-LM-AGE (4)
00839                             WSS-LF-LM-DUR (1) WSS-LF-LM-DUR (2)
00840                             WSS-LF-LM-DUR (3) WSS-LF-LM-DUR (4)
00841                             WSS-LF-LM-AMT (1) WSS-LF-LM-AMT (2)
00842                             WSS-LF-LM-AMT (3) WSS-LF-LM-AMT (4).
00843
00844      MOVE ZEROS          TO WSS-AH-LM-AGE (1) WSS-LF-LM-AGE (2)
00845                             WSS-AH-LM-AGE (3) WSS-LF-LM-AGE (4)
00846                             WSS-AH-LM-DUR (1) WSS-LF-LM-DUR (2)
00847                             WSS-AH-LM-DUR (3) WSS-LF-LM-DUR (4)
00848                             WSS-AH-LM-AMT (1) WSS-LF-LM-AMT (2)
00849                             WSS-AH-LM-AMT (3) WSS-LF-LM-AMT (4)
00850                             WSS-AH-LM-MOA (1) WSS-AH-LM-MOA (2)
00851                             WSS-AH-LM-MOA (3) WSS-AH-LM-MOA (4).
00852
00853      MOVE WSS-AGT-COMMS          TO AM-AGT-COMMS (1)
00854                                     AM-AGT-COMMS (2)
00855                                     AM-AGT-COMMS (3)
00856                                     AM-AGT-COMMS (4)
00857                                     AM-AGT-COMMS (5)
00858                                     AM-AGT-COMMS (6)
00859                                     AM-AGT-COMMS (7)
00860                                     AM-AGT-COMMS (8)
00861                                     AM-AGT-COMMS (9)
00862                                     AM-AGT-COMMS (10).
00863
00864      IF PI-COMPANY-ID = 'FLI'
00865          MOVE ZEROS              TO AM-FLI-BANK-BALANCE
00866                                     AM-FLI-BANK-1ST-6-PREM
00867                                     AM-FLI-BANK-CAP-AMT
00868                                     AM-FLI-AGT-SHARE-PCT (1)
00869                                     AM-FLI-AGT-SHARE-PCT (2)
00870                                     AM-FLI-AGT-SHARE-PCT (3)
00871                                     AM-FLI-AGT-SHARE-PCT (4)
00872                                     AM-FLI-AGT-SHARE-PCT (5)
00873                                     AM-FLI-AGT-SHARE-PCT (6)
00874                                     AM-FLI-AGT-SHARE-PCT (7)
00875                                     AM-FLI-AGT-SHARE-PCT (8)
00876                                     AM-FLI-AGT-SHARE-PCT (9)
00877                                     AM-FLI-AGT-SHARE-PCT (10).
00878
00879      MOVE ZEROS                  TO AM-GPCD
00880                                     AM-LF-RET
00881                                     AM-AH-RET
00882                                     AM-LF-OB-RATE
00883                                     AM-LF-OB-RATE-JNT
00884                                     AM-AH-OB-RATE
00885                                     AM-AH-OB-RATE-JNT
00886                                     AM-REI-FEE-LF
00887                                     AM-REI-FEE-AH
00888                                     AM-REI-LF-TAX
00889                                     AM-REI-PR-PCT
00890                                     AM-REI-78-PCT
00891                                     AM-REI-AH-TAX
00892                                     AM-TOL-PREM
00893                                     AM-TOL-REF
100703                                    AM-CLP-TOL-PCT
00894                                     AM-TOL-CLM
00895                                     AM-CERTS-PURGED-DATE
00896                                     AM-HI-CERT-DATE
00897                                     AM-LO-CERT-DATE
00898                                     AM-1ST-PROD-DATE
00899                                     AM-GPCD
00900                                     AM-CAL-TABLE
00901                                     AM-LF-DEVIATION
00902                                     AM-LF-DEVIATION-PCT
00903                                     AM-AH-DEVIATION
00904                                     AM-AH-DEVIATION-PCT
00905                                     AM-ANNIVERSARY-DATE
00906                                     AM-TEL-NO
00907                                     AM-ENTRY-DATE
00908                                     AM-INACTIVE-DATE
00909                                     AM-RET-MIN-LOSS-L
00910                                     AM-RET-MIN-LOSS-A
00911                                     AM-LF-RPT021-EXP-PCT
00912                                     AM-AH-RPT021-EXP-PCT
00913                                     AM-MAX-MON-BEN
00914                                     AM-MAX-TOT-BEN
                                          AM-DCC-MAX-MARKETING-FEE
                                          AM-SPP-LEASE-COMM
00915                                     AM-RETRO-QUALIFY-LIMIT
00916                                     AM-RETRO-RET-PCT-LF (1)
00917                                     AM-RETRO-RET-PCT-LF (2)
00918                                     AM-RETRO-RET-PCT-LF (3)
00919                                     AM-RETRO-RET-THRU-LF (1)
00920                                     AM-RETRO-RET-THRU-LF (2)
00921                                     AM-RETRO-RET-THRU-LF (3)
00922                                     AM-RETRO-RET-PCT-AH (1)
00923                                     AM-RETRO-RET-PCT-AH (2)
00924                                     AM-RETRO-RET-PCT-AH (3)
00925                                     AM-RETRO-RET-THRU-AH (1)
00926                                     AM-RETRO-RET-THRU-AH (2)
00927                                     AM-RETRO-RET-THRU-AH (3).
00928
020816     IF PI-COMPANY-ID = 'DCC' or 'VPP'
              MOVE +100                TO AM-DCC-MAX-MARKETING-FEE
           END-IF
00929      MOVE LOW-VALUES             TO PI-ACCT-REST-OF-EXP
00930                                     AM-AR-HI-CERT-DATE.
00931
00932      MOVE 'AM'                   TO AM-RECORD-ID.
00933      MOVE PI-ACCT-KEY            TO AM-CONTROL-PRIMARY
00934                                     AM-CONTROL-BY-VAR-GRP.
00935
00936      IF ST-ACCNT-CNTL
00937         MOVE SPACES              TO AM-VG-CARRIER
00938                                     AM-VG-GROUPING.
00939      IF CARR-ST-ACCNT-CNTL
00940         MOVE SPACES              TO AM-VG-GROUPING.
00941
00942      IF ACCNT-CNTL
00943         MOVE SPACES              TO AM-VG-CARRIER
00944                                     AM-VG-STATE
00945                                     AM-VG-GROUPING.
00946
00947      IF CARR-ACCNT-CNTL
00948         MOVE SPACES              TO AM-VG-STATE
00949                                     AM-VG-GROUPING.
00950
00951      MOVE CURRENT-SAVE           TO AM-ENTRY-DATE.
00952      MOVE BIN-EXPCHG-SAVE        TO AM-VG-EXPIRATION-DT.
00953      MOVE PI-BIN-EFF-DT (1)      TO AM-EFFECTIVE-DT.
00954
00955      MOVE BIN-CURRENT-SAVE       TO AM-LAST-MAINT-DT.
00956      MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.
00957      MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.
00958
00959      PERFORM 5000-MOVE-TO-RECORD THRU 5000-EXIT.
00960
00961      IF PI-COMPANY-ID = 'NCL'
00962          MOVE 'N'                TO AM-AH-ONLY-INDICATOR.
00963
01290      IF EMI-FORCABLE-CTR GREATER ZERO  OR
01291         EMI-FATAL-CTR    GREATER ZERO
01292          GO TO 8200-SEND-DATAONLY.
01293
020816     IF PI-COMPANY-ID = 'DCC' or 'VPP'
              IF CLPSTL <= ZEROS
                 MOVE ER-0144          TO  EMI-ERROR
                 MOVE -1               TO  CLPSTL
                 MOVE AL-UABON         TO  CLPSTA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF
00964      MOVE AM-COMM-STRUCTURE      TO AM-COMM-STRUCTURE-SAVED.
00965      MOVE SPACE                  TO AM-COMM-CHANGE-STATUS.
00966
00967      MOVE +1 TO AXRF-SUB
00968                 COMM-WORK-SUB.
00969
00970      PERFORM 6700-BUILD-COMM-WORK     THRU 6799-EXIT.
00971      PERFORM 6900-READ-AND-CHECK-AXRF THRU 6999-EXIT.
00972
00973      PERFORM 2100-ADD-ACCOUNT-NAME THRU 2100-EXIT.
00974
00975      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00976          NEXT SENTENCE
00977      ELSE
00978          PERFORM 1500-ADD-COMP     THRU 1500-EXIT.
00979
00980      
      * EXEC CICS WRITE
00981 *        FROM      (ACCOUNT-MASTER)
00982 *        RIDFLD    (AM-CONTROL-PRIMARY)
00983 *        DATASET   (ACCT-FILE-ID)
00984 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006355' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00985
00986      PERFORM 6600-UPDATE-MAINT-DT THRU 6699-EXIT.
00987
00988      MOVE PI-MAINT               TO PI-SV-MAINT.
00989      MOVE 'C'                    TO PI-MAINT.
00990      MOVE LOW-VALUES             TO EL6501AI.
00991
00992      PERFORM 0700-FORMAT-KEYDATA THRU 0700-EXIT.
00993      PERFORM 6500-READ-ACCT      THRU 6599-EXIT.
00994      PERFORM 6000-MOVE-TO-SCREEN THRU 6099-EXIT.
00995
00996      IF PI-COMPANY-ID = 'NCL'
00997          MOVE ER-4011            TO EMI-ERROR
00998       ELSE
00999          MOVE ER-0000            TO EMI-ERROR.
01000
01001      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01002
01003      MOVE -1                     TO MAINTL.
01004      MOVE AL-UANON               TO MAINTA.
01005      MOVE '1'                    TO PI-RECORD-ADDED-SW.
01006      GO TO 8100-SEND-INITIAL-MAP.
01007
01008      EJECT
01009  1500-ADD-COMP.
01010      
      * EXEC CICS GETMAIN
01011 *         LENGTH   (COMP-REC-LEN)
01012 *         SET      (ADDRESS OF COMPENSATION-MASTER)
01013 *         INITIMG  (GETMAIN-SPACE)
01014 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006385' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 COMP-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01015
01016      MOVE SPACES                 TO COMPENSATION-MASTER.
01017      MOVE ZEROS TO CO-BAL-FWD      CO-CUR-COM   CO-CUR-CHG
01018                    CO-CUR-PMT      CO-END-BAL   CO-CUR
01019                    CO-OV30         CO-OV60      CO-OV90
01020                    CO-YTD-COM      CO-YTD-OV    CO-CUR-OVR-UNDR
01021                    CO-YTD-OVR-UNDR CO-CUR-FICA CO-YTD-FICA
01022                    CO-LF-CLM-AMT   CO-AH-CLM-AMT
01023                    CO-CURRENT-BAL-FWD CO-CURRENT-CUR-COM
01024                    CO-CURRENT-CUR-CHG CO-CURRENT-END-BAL
01025                    CO-CURRENT-CUR     CO-CURRENT-OV30
01026                    CO-CURRENT-OV60    CO-CURRENT-OV90
01027                    CO-CURRENT-CUR-PMT
01028                    CO-CURRENT-YTD-COM CO-CURRENT-YTD-OV
01029                    CO-ACT-YEAR        CO-ACT-MONTH
01030                    CO-ACT-DAY         CO-LAST-STMT-YEAR
01031                    CO-LAST-STMT-MONTH CO-LAST-STMT-DAY
01032                    CO-CURRENT-LAST-STMT-YEAR
01033                    CO-CURRENT-LAST-STMT-MONTH
01034                    CO-CURRENT-LAST-STMT-DAY
01035                    CO-YTD-PAID-COM
01036                    CO-YTD-PAID-OV
080612                   co-ov120 co-current-ov120
01037
01038      MOVE BIN-CURRENT-SAVE       TO CO-LAST-MAINT-DT.
01039      MOVE PI-PROCESSOR-ID        TO CO-LAST-MAINT-USER.
01040      MOVE EIBTIME                TO CO-LAST-MAINT-HHMMSS.
01041      MOVE PI-CR-MONTH-END-DT     TO CO-ROLADEX-PRINT-DT.
01042      MOVE PI-COMPANY-CD          TO CO-COMPANY-CD.
01043
01044      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
01045          MOVE ZEROS              TO CO-CARRIER
01046      ELSE
01047          MOVE AM-CARRIER         TO CO-CARRIER.
01048
01049      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
01050          MOVE ZEROS              TO CO-GROUPING
01051      ELSE
01052          MOVE AM-GROUPING        TO CO-GROUPING.
01053
090507     MOVE 'CO'                   TO CO-RECORD-ID
01054      MOVE 'A'                    TO CO-TYPE.
01055      MOVE AM-NAME                TO CO-ACCT-NAME.
01056      MOVE AM-PERSON              TO CO-MAIL-NAME.
01057      MOVE AM-ADDRS               TO CO-ADDR-1.
01058 *    MOVE AM-CITY                TO CO-ADDR-3.
           MOVE AM-ADDR-CITY           TO CO-ADDR-CITY
           MOVE AM-ADDR-STATE          TO CO-ADDR-STATE
01059      MOVE AM-ZIP                 TO CO-ZIP.
01060      MOVE AM-CSR-CODE            TO CO-CSR-CODE.
01061      MOVE AM-TEL-NO              TO CO-TELEPHONE.
01062
01063      IF PI-COMPANY-ID = 'NCL'
01064          MOVE AM-ID-NO           TO  WS-SOC-SEC-WORK
01065          IF WS-SS-TYPE = 'C' OR 'P' OR 'S' OR 'T' OR
01066                          'X' OR 'I'
01067              MOVE WS-SS-TYPE     TO  CO-TYPE-AGENT
01068              MOVE WS-SOC-SEC     TO  CO-SOC-SEC
01069          ELSE
01070              MOVE SPACE          TO  CO-TYPE-AGENT
01071              MOVE AM-ID-NO       TO  CO-SOC-SEC
01072      ELSE
01073          MOVE AM-ID-NO           TO CO-SOC-SEC.
01074
01075      IF AM-REMIT-TO NOT = ZERO
01076          MOVE AM-AGT (AM-REMIT-TO) TO CO-RESP-NO
01077        ELSE
01078          MOVE AM-ACCOUNT           TO CO-RESP-NO.
01079
01080      MOVE +0                      TO AGT-SUB.
01081
01082      PERFORM 1600-FIND-C-AGT THRU 1600-EXIT.
01083
01084      IF VALID-AGT-SW = 'Y'
01085          NEXT SENTENCE
01086      ELSE
01087          GO TO 1500-EXIT.
01088
01089      IF CO-RESP-NO = CO-ACCOUNT
01090          MOVE 'Y'                  TO CO-BALANCE-CONTROL
01091      ELSE
01092          MOVE 'N'                  TO CO-BALANCE-CONTROL.
01093
01094      IF PI-AR-PROCESSING
01095         MOVE 'G'                   TO CO-AR-REPORTING
01096         MOVE '1'                   TO CO-AR-BAL-LEVEL
01097         MOVE 'Y'                   TO CO-AR-PULL-CHECK
01098         MOVE 'Y'                   TO CO-AR-NORMAL-PRINT.
01099
01100      IF PI-COMPANY-ID = 'CRI' OR 'HER' OR 'HSL'
01101         MOVE 'N'                   TO CO-AR-REPORTING.
01102
01103      IF PI-COMPANY-ID = 'HER' OR 'HSL'
01104         MOVE '999999'              TO CO-AR-SUMMARY-CODE.
01105
01106      PERFORM 2200-ADD-COMPENSATION-NAME THRU 2200-EXIT.
01107
01108      
      * EXEC CICS HANDLE CONDITION
01109 *         DUPREC   (1500-EXIT)
01110 *    END-EXEC.
      *    MOVE '"$%                   ! # #00006487' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036343837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01111
01112      
      * EXEC CICS WRITE
01113 *        FROM      (COMPENSATION-MASTER)
01114 *        RIDFLD    (CO-CONTROL-PRIMARY)
01115 *        DATASET   (COMP-FILE-ID)
01116 *    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006491' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01117
01118  1500-EXIT.
01119       EXIT.
01120
01121  1600-FIND-C-AGT.
01122      ADD 1 TO  AGT-SUB.
01123      IF AGT-SUB GREATER 10
01124          MOVE 'N'                TO  VALID-AGT-SW
01125          GO TO 1600-EXIT.
01126
01127 ************************************************************
01128 **  MODIFICATION MADE TO ADD SERVICE FEES                 **
01129 ************************************************************
01130
052814     IF AM-COM-TYP (AGT-SUB) = 'C' OR 'D' OR 'F'
01132          MOVE AM-AGT (AGT-SUB)   TO  CO-ACCOUNT
01133          MOVE 'Y'                TO  VALID-AGT-SW
01134          GO TO 1600-EXIT
01135      ELSE
01136          GO TO 1600-FIND-C-AGT.
01137
01138  1600-EXIT.
01139      EXIT.
01140  EJECT
01141  1700-CHANGE-COMP.
01142      MOVE 'N'                    TO CHANGE-WAS-MADE-SW.
01143      MOVE PI-COMPANY-CD          TO COMP-COMPANY-CD.
01144
01145      IF PI-ZERO-CARRIER
01146        OR PI-ZERO-CAR-GROUP
01147          MOVE ZEROS              TO COMP-CARRIER
01148      ELSE
01149          MOVE AM-CARRIER         TO COMP-CARRIER.
01150
01151      IF PI-ZERO-GROUPING
01152        OR PI-ZERO-CAR-GROUP
01153          MOVE ZEROS              TO COMP-GROUPING
01154      ELSE
01155          MOVE AM-GROUPING        TO COMP-GROUPING.
01156
01157      IF AM-REMIT-TO NOT = ZERO
01158          MOVE AM-AGT (AM-REMIT-TO)
01159                                  TO COMP-RESP-NO
01160      ELSE
01161          MOVE AM-ACCOUNT         TO COMP-RESP-NO.
01162
01163      MOVE ZERO                   TO AGT-SUB.
01164
01165      PERFORM 1800-FIND-C-AGT THRU 1899-EXIT.
01166
01167      IF VALID-AGT-SW = 'Y'
01168          NEXT SENTENCE
01169      ELSE
01170          GO TO 1799-EXIT.
01171
01172      MOVE 'A'                    TO COMP-TYPE.
01173
01174      
      * EXEC CICS HANDLE CONDITION
01175 *         ENDFILE     (1799-EXIT)
01176 *         NOTFND      (1799-EXIT)
01177 *    END-EXEC.
      *    MOVE '"$''I                  ! $ #00006553' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303036353533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01178
01179      
      * EXEC CICS READ
01180 *        UPDATE
01181 *        DATASET   (COMP-FILE-ID)
01182 *        SET       (ADDRESS OF COMPENSATION-MASTER)
01183 *        RIDFLD    (COMP-KEY)
01184 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006558' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01185
111606     IF (ON-LAST-DATE-RANGE)
              IF NAMEL GREATER ZEROS
                 MOVE 'Y'              TO CHANGE-WAS-MADE-SW
                 MOVE AM-NAME          TO CO-ACCT-NAME
              END-IF
              IF INCAREL GREATER ZEROS
                 MOVE 'Y'              TO CHANGE-WAS-MADE-SW
                 MOVE AM-PERSON        TO CO-MAIL-NAME
              END-IF
              IF ADDR1L GREATER ZEROS
                 MOVE 'Y'              TO CHANGE-WAS-MADE-SW
                 MOVE AM-ADDRS         TO CO-ADDR-1
              END-IF
              IF ACITYL  GREATER ZEROS
                 MOVE 'Y'              TO CHANGE-WAS-MADE-SW
                 MOVE AM-ADDR-CITY     TO CO-ADDR-CITY
              END-IF
              IF ASTATEL  GREATER ZEROS
                 MOVE 'Y'              TO CHANGE-WAS-MADE-SW
                 MOVE AM-ADDR-STATE    TO CO-ADDR-STATE
              END-IF
              IF ZIPL GREATER ZEROS
                 MOVE 'Y'              TO CHANGE-WAS-MADE-SW
                 MOVE AM-ZIP           TO CO-ZIP
              END-IF
              IF CSRL GREATER ZEROS
                 MOVE 'Y'              TO CHANGE-WAS-MADE-SW
                 MOVE AM-CSR-CODE      TO CO-CSR-CODE
              END-IF
              IF PHONEL GREATER ZEROS
                 MOVE 'Y'              TO CHANGE-WAS-MADE-SW
                 MOVE AM-TEL-NO        TO CO-TELEPHONE
              END-IF
              IF TAXNOL GREATER ZEROS
                 MOVE 'Y'              TO CHANGE-WAS-MADE-SW
                 MOVE AM-ID-NO         TO CO-SOC-SEC
              END-IF
              IF AUTO-CANCEL-ACCOUNT
                 IF CO-BILL-SW = ' '
                    MOVE 'B'           TO CO-BILL-SW
                    MOVE 'Y'           TO CHANGE-WAS-MADE-SW
                 END-IF
              END-IF
CIDMOD     END-IF
01217
CIDMOD     IF PCONTL > 0
CIDMOD         MOVE 'Y'                TO CHANGE-WAS-MADE-SW
PMETST         MOVE AM-CONTROL-NAME    TO CO-CONTROL-NAME
CIDMOD     END-IF
01217
01218      IF CHANGE-WAS-MADE
01219          MOVE BIN-CURRENT-SAVE   TO CO-LAST-MAINT-DT
01220          MOVE PI-PROCESSOR-ID    TO CO-LAST-MAINT-USER
01221          MOVE EIBTIME            TO CO-LAST-MAINT-HHMMSS.
01222
01223      IF NAMEL GREATER THAN ZERO
01224          PERFORM 2200-ADD-COMPENSATION-NAME THRU 2200-EXIT.
01225
01226      
      * EXEC CICS REWRITE
01227 *        FROM      (COMPENSATION-MASTER)
01228 *        DATASET   (COMP-FILE-ID)
01229 *    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006623' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01230
01231  1799-EXIT.
01232      EXIT.
01233  EJECT
01234  1800-FIND-C-AGT.
01235      ADD 1                       TO  AGT-SUB.
01236
01237      IF AGT-SUB GREATER 10
01238          MOVE 'N'                TO  VALID-AGT-SW
01239          GO TO 1899-EXIT.
01240
01241 ************************************************************
01242 **  MODIFICATION MADE TO ADD SERVICE FEES                 **
01243 ************************************************************
01244
052814     IF AM-COM-TYP (AGT-SUB) = 'C' OR 'D' OR 'F'
01246          MOVE AM-AGT (AGT-SUB)   TO  COMP-ACCOUNT
01247          MOVE 'Y'                TO  VALID-AGT-SW
01248          GO TO 1899-EXIT
01249      ELSE
01250          GO TO 1800-FIND-C-AGT.
01251
01252  1899-EXIT.
01253      EXIT.
01254  EJECT
01255  2000-CHANGE-A-RECORD.
01256      PERFORM 4000-EDITS THRU 4900-EXIT.
01257
01258      IF EMI-FORCABLE-CTR GREATER ZERO  OR
01259         EMI-FATAL-CTR    GREATER ZERO
01260          GO TO 8200-SEND-DATAONLY.
01261
01262      
      * EXEC CICS READ
01263 *        UPDATE
01264 *        DATASET   (ACCT-FILE-ID)
01265 *        SET       (ADDRESS OF ACCOUNT-MASTER)
01266 *        RIDFLD    (PI-ACCT-KEY)
01267 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006659' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01268
01269      PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT.
01270
01271      IF NOT PI-AR-PROCESSING
01272         GO TO 2075-PROCESS-CHANGE.
01273
01274      MOVE AM-AGT (AM-REMIT-TO)   TO SAVE-FIN-RESP.
01275      MOVE +0                     TO SUB1.
01276
01277  2075-PROCESS-CHANGE.
01278      PERFORM 5000-MOVE-TO-RECORD THRU 5000-EXIT.
01279
01280      IF PI-AR-PROCESSING
01281         PERFORM 5300-UPDATE-REQUEST-FILE THRU 5399-EXIT.
01282
01283      IF PI-COMPANY-ID = 'DMD'
01284      IF AM-ANNIVERSARY-DATE = ZEROS
01285            MOVE ER-0852             TO EMI-ERROR
01286            MOVE -1                  TO CONTRL
01287            MOVE AL-UABON            TO CONTRA
01288            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01289
01290      IF EMI-FORCABLE-CTR GREATER ZERO  OR
01291         EMI-FATAL-CTR    GREATER ZERO
01292          GO TO 8200-SEND-DATAONLY.
01293
01294      MOVE +1                     TO AXRF-SUB
01295                                     COMM-WORK-SUB.
01296      PERFORM 6900-READ-AND-CHECK-AXRF THRU 6999-EXIT.
01297
01298      MOVE BIN-CURRENT-SAVE       TO AM-LAST-MAINT-DT.
01299      MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.
01300      MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.
01301
111606     IF ((PI-FST-PAGE)
111606        AND (PI-TOTAL-LINES = PI-LINE-SELECTED))
111606                      OR
111606        ((PI-2ND-PAGE)
111606        AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 8))
111606                      OR
111606        ((PI-3RD-PAGE)
111606        AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 16))
111606                      OR
111606        ((PI-LST-PAGE)
111606        AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 24))
111606        SET ON-LAST-DATE-RANGE   TO TRUE
111606     END-IF
01302      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01303          NEXT SENTENCE
01304      ELSE
111606         IF (ON-LAST-DATE-RANGE)
01305                           OR
062121            ((PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL') AND
CIDMOD             (PCONTL > ZERO))
CIDMOD             PERFORM 1700-CHANGE-COMP THRU 1799-EXIT.
01307
01308      IF NAMEL GREATER THAN ZERO
01309          PERFORM 2100-ADD-ACCOUNT-NAME THRU 2100-EXIT.
01310
01311      
      * EXEC CICS REWRITE
01312 *        FROM      (ACCOUNT-MASTER)
01313 *        DATASET   (ACCT-FILE-ID)
01314 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006724' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01315
01316      PERFORM 6600-UPDATE-MAINT-DT THRU 6699-EXIT.
01317
01318      IF CSRL GREATER ZEROS
01319          GO TO 3500-CHANGE-ALL-CSR.
01320
CIDMOD     MOVE ER-0000                TO EMI-ERROR.
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01321      MOVE LOW-VALUES             TO EL6501AI.
01322
01323      PERFORM 0700-FORMAT-KEYDATA THRU 0700-EXIT.
01324      PERFORM 6500-READ-ACCT      THRU 6599-EXIT.
01325      PERFORM 6000-MOVE-TO-SCREEN THRU 6099-EXIT.
01326
01327      MOVE ER-0000                TO EMI-ERROR.
01328      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01329
01330      MOVE -1                     TO MAINTL.
01331      MOVE AL-UANON               TO MAINTA.
01332      GO TO 8100-SEND-INITIAL-MAP.
01333
01334  EJECT
01335  2100-ADD-ACCOUNT-NAME.
01336      
      * EXEC CICS HANDLE CONDITION
01337 *         DUPREC   (2100-EXIT)
01338 *    END-EXEC.
      *    MOVE '"$%                   ! % #00006751' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036373531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01339
01340      
      * EXEC CICS GETMAIN
01341 *         LENGTH   (NAME-REC-LEN)
01342 *         SET      (ADDRESS OF NAME-LOOKUP-MASTER)
01343 *         INITIMG  (GETMAIN-SPACE)
01344 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006755' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 NAME-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01345
01346      MOVE SPACES                 TO  NAME-LOOKUP-MASTER.
01347      MOVE BIN-CURRENT-SAVE       TO  NL-LAST-MAINT-DT.
01348      MOVE PI-PROCESSOR-ID        TO  NL-LAST-MAINT-USER.
01349      MOVE EIBTIME                TO  NL-LAST-MAINT-HHMMSS.
01350      MOVE PI-COMPANY-CD          TO  NL-COMPANY-CD.
01351      MOVE AM-NAME                TO  NL-NAME.
012407     IF NL-NAME (1:4) = 'THE ' OR 'The ' OR 'the '
012407        MOVE NL-NAME (5:26)      TO NL-NAME
012407     END-IF
01352      MOVE AM-ADDR-CITY           TO  NL-CITY.
01353      MOVE AM-ADDR-STATE          TO  NL-ST.
01354      MOVE 'A'                    TO  NL-RECORD-TYPE.
01355      MOVE PI-COMPANY-CD          TO  NL-AM-COMPANY-CD.
01356      MOVE AM-CARRIER             TO  NL-AM-CARRIER.
01357      MOVE AM-GROUPING            TO  NL-AM-GROUPING.
01358      MOVE AM-STATE               TO  NL-AM-STATE.
01359      MOVE AM-ACCOUNT             TO  NL-AM-ACCOUNT.
01360
01361      
      * EXEC CICS WRITE
01362 *        FROM      (NAME-LOOKUP-MASTER)
01363 *        RIDFLD    (NL-CONTROL-PRIMARY)
01364 *        DATASET   (NAME-FILE-ID)
01365 *    END-EXEC.
           MOVE LENGTH OF
            NAME-LOOKUP-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006779' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NAME-FILE-ID, 
                 NAME-LOOKUP-MASTER, 
                 DFHEIV11, 
                 NL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01366
01367  2100-EXIT.
01368      EXIT.
01369  EJECT
01370  2200-ADD-COMPENSATION-NAME.
01371      
      * EXEC CICS HANDLE CONDITION
01372 *         DUPREC   (2200-EXIT)
01373 *    END-EXEC.
      *    MOVE '"$%                   ! & #00006789' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036373839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01374
01375      
      * EXEC CICS GETMAIN
01376 *         LENGTH   (NAME-REC-LEN)
01377 *         SET      (ADDRESS OF NAME-LOOKUP-MASTER)
01378 *         INITIMG  (GETMAIN-SPACE)
01379 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006793' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 NAME-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01380
01381      MOVE SPACES                 TO  NAME-LOOKUP-MASTER.
01382      MOVE BIN-CURRENT-SAVE       TO  NL-LAST-MAINT-DT.
01383      MOVE PI-PROCESSOR-ID        TO  NL-LAST-MAINT-USER.
01384      MOVE EIBTIME                TO  NL-LAST-MAINT-HHMMSS.
01385      MOVE PI-COMPANY-CD          TO  NL-COMPANY-CD.
01386      MOVE CO-ACCT-NAME           TO  NL-NAME.
012407     IF NL-NAME (1:4) = 'THE ' OR 'The ' OR 'the '
012407        MOVE NL-NAME (5:26)      TO NL-NAME
012407     END-IF
01387      MOVE CO-ADDR-CITY           TO  NL-CITY.
           MOVE CO-ADDR-STATE          TO  NL-ST
01388      MOVE 'C'                    TO  NL-RECORD-TYPE.
01389      MOVE PI-COMPANY-CD          TO  NL-CO-COMPANY-CD.
01390      MOVE CO-CARRIER             TO  NL-CO-CARRIER.
01391      MOVE CO-GROUPING            TO  NL-CO-GROUPING.
01392      MOVE CO-RESP-NO             TO  NL-CO-RESP-NO.
01393      MOVE CO-ACCOUNT             TO  NL-CO-ACCOUNT.
01394      MOVE CO-TYPE                TO  NL-CO-TYPE.
01395
01396      
      * EXEC CICS WRITE
01397 *        FROM      (NAME-LOOKUP-MASTER)
01398 *        RIDFLD    (NL-CONTROL-PRIMARY)
01399 *        DATASET   (NAME-FILE-ID)
01400 *    END-EXEC.
           MOVE LENGTH OF
            NAME-LOOKUP-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006818' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NAME-FILE-ID, 
                 NAME-LOOKUP-MASTER, 
                 DFHEIV11, 
                 NL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01401
01402  2200-EXIT.
01403      EXIT.
01404  EJECT
01405  3000-CHANGE-ALL-RECORDS.
01406      MOVE LOW-VALUES             TO PI-ACCT-REST-OF-EXP.
01407      MOVE PI-ACCT-KEY            TO WS-KEY-SAVE.
01408
01409      
      * EXEC CICS HANDLE CONDITION
01410 *         ENDFILE     (3030-END-FILE)
01411 *    END-EXEC.
      *    MOVE '"$''                   ! '' #00006831' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036383331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01412
01413      PERFORM 6300-STARTBR THRU 6300-EXIT.
01414
01415  3010-READ-NEXT.
01416      PERFORM 6310-READNEXT THRU 6310-EXIT.
01417
01418      IF PI-ACCT-CCGSA-KEY NOT = WS-ACCT-CCGSA-KEY
01419         GO TO 3030-END-FILE.
01420
01421      
      * EXEC CICS ENDBR
01422 *        DATASET   (ACCT-FILE-ID)
01423 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006843' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
080807     ADD +1                      TO WS-CHANGE-ALL-CNT
01425      
      * EXEC CICS READ
01426 *        UPDATE
01427 *        DATASET   (ACCT-FILE-ID)
01428 *        SET       (ADDRESS OF ACCOUNT-MASTER)
01429 *        RIDFLD    (WS-KEY-SAVE)
01430 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006847' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-KEY-SAVE, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01431
01432      IF NAMEL    GREATER THAN ZERO  OR
01433         INCAREL  GREATER THAN ZERO  OR
01434         ADDR1L   GREATER THAN ZERO  OR
01435         ACITYL   GREATER THAN ZERO  OR
01435         ASTATEL  GREATER THAN ZERO  OR
01436         ZIPL     GREATER THAN ZERO  OR
100703        CLPTOLPL GREATER THAN ZERO  OR
01437         PHONEL   GREATER THAN ZERO
01438          MOVE '*'                TO AM-PROFILE-CHANGE-SWITCH.
01439
022808     IF STATUSL > ZERO
              EVALUATE TRUE
                 WHEN STATUSI = 'A'
                    MOVE '0'           TO AM-STATUS
                 WHEN STATUSI = 'I'
                    MOVE '1'           TO AM-STATUS
                 WHEN (STATUSI = 'C')
                    AND (AM-STATUS NOT = '3' AND 'C')
                    MOVE '3'           TO AM-STATUS
                    SET AUTO-CANCEL-ACCOUNT
                                       TO TRUE
                 WHEN (STATUSI = 'C')
                    MOVE '3'           TO AM-STATUS
                 WHEN STATUSI = 'F'
                    MOVE '4'           TO AM-STATUS
                 WHEN STATUSI = 'S'
                    MOVE '5'           TO AM-STATUS
021916           WHEN STATUSI = 'D'
021916              MOVE '6'           TO AM-STATUS
021916           WHEN STATUSI = 'L'
021916              MOVE '7'           TO AM-STATUS
021916           WHEN STATUSI = 'R'
021916              MOVE '8'           TO AM-STATUS
021916           WHEN STATUSI = 'P'
021916              MOVE '9'           TO AM-STATUS
                 WHEN OTHER
                    MOVE -1            TO STATUSL
                    MOVE AL-UABON      TO STATUSA
                    MOVE ER-2153       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-EVALUATE
           END-IF
01440 *    IF STATUSL > ZERO
01441 *       IF STATUSI = 'A'
01442 *          MOVE '0'              TO AM-STATUS
01443 *       ELSE
01444 *          IF STATUSI = 'I'
01445 *             MOVE '1'           TO AM-STATUS
01446 *          ELSE
102004*             IF STATUSI = 'C'
080807*                IF AM-STATUS NOT = '3' AND 'C'
080807*                   SET AUTO-CANCEL-ACCOUNT
080807*                                TO TRUE
080807*                END-IF
102004*                MOVE '3'        TO AM-STATUS
102004*             ELSE
      *                IF STATUSI = 'F'
      *                   MOVE '4'     TO AM-STATUS
      *                ELSE
01447 *                   MOVE -1         TO STATUSL
01448 *                   MOVE AL-UABON   TO STATUSA
01449 *                   MOVE ER-2153    TO EMI-ERROR
01450 *                   PERFORM 9900-ERROR-FORMAT
      *                                THRU 9900-EXIT
      *                END-IF
      *             END-IF
      *          END-IF
      *       END-IF
102004*    END-IF
01451
01452      IF CSRL GREATER ZERO
01453         MOVE CSRI                TO AM-CSR-CODE.
01454      IF NAMEL GREATER THAN ZERO
01455         MOVE NAMEI               TO AM-NAME.
CIDMOD     IF PCONTL GREATER THAN ZERO
CIDMOD        MOVE PCONTI              TO AM-CONTROL-NAME
CIDMOD     END-IF
01456      IF INCAREL GREATER ZEROS
01457         MOVE INCAREI             TO AM-PERSON.
01458      IF ADDR1L GREATER ZEROS
01459         MOVE ADDR1I              TO AM-ADDRS.
01460      IF ACITYL GREATER ZEROS
01461         MOVE ACITYI             TO AM-ADDR-CITY.
01460      IF ASTATEL GREATER ZEROS
01461         MOVE ASTATEI            TO AM-ADDR-STATE.
01462      IF ZIPL NOT GREATER ZEROS
01463          GO TO 3015-CONT-CHANGING.
01464
01465      MOVE ZIPI                   TO WS-ZIP-CODE.
01466
01467      IF WS-CANADIAN-ZIP
01468          IF WS-ZIP-4 = SPACE  OR  '-'
01469              MOVE WS-ZIP-CAN-2-POST1  TO AM-CAN-POSTAL-1
01470              MOVE WS-ZIP-CAN-2-POST2  TO AM-CAN-POSTAL-2
01471          ELSE
01472              MOVE WS-ZIP-CAN-1-POST1  TO AM-CAN-POSTAL-1
01473              MOVE WS-ZIP-CAN-1-POST2  TO AM-CAN-POSTAL-2
01474      ELSE
01475          IF WS-ZIP-6 = SPACE  OR  '-'
01476              MOVE WS-ZIP-AM-2-CODE    TO AM-ZIP-PRIME
01477              MOVE WS-ZIP-AM-2-PLUS4   TO AM-ZIP-PLUS4
01478          ELSE
01479              MOVE WS-ZIP-AM-1-CODE    TO AM-ZIP-PRIME
01480              MOVE WS-ZIP-AM-1-PLUS4   TO AM-ZIP-PLUS4.
01481
01482  3015-CONT-CHANGING.
100703
100703     IF CLPTOLPL GREATER ZEROS
100703         
      * EXEC CICS BIF
100703*            DEEDIT
100703*            FIELD    (CLPTOLPI)
100703*            LENGTH   (6)
100703*        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006963' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLPTOLPI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100703         IF CLPTOLPI NUMERIC
100703             MOVE CLPTOLPI            TO AM-CLP-TOL-PCT
100703         ELSE
100703             MOVE -1                  TO CLPTOLPL
100703             MOVE AL-UNBON            TO CLPTOLPA
100703             MOVE ER-1778             TO EMI-ERROR
100703             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100703         END-IF
100703     END-IF
100703
092705     IF LCOMML > ZEROS
092705        
      * EXEC CICS BIF
092705*            DEEDIT
092705*            FIELD    (LCOMMI)
092705*            LENGTH   (8)
092705*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006979' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCOMMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
092705         IF LCOMMI NUMERIC
092705            MOVE LCOMMI          TO AM-SPP-LEASE-COMM
092705         ELSE
092705            MOVE -1              TO LCOMML
092705            MOVE AL-UNBON        TO LCOMMA
092705            MOVE ER-1778         TO EMI-ERROR
092705            PERFORM 9900-ERROR-FORMAT
092705                                 THRU 9900-EXIT
092705         END-IF
092705     END-IF
092705
01483      IF PHONEL GREATER ZEROS
01484         MOVE PHONEI              TO DEEDIT-FIELD
01485         PERFORM 8600-DEEDIT
01486         MOVE DEEDIT-FIELD-V0     TO WS-PHONE-NUM
01487         MOVE WS-PH1              TO AM-AREA-CODE
01488         MOVE WS-PH2              TO AM-TEL-PRE
01489         MOVE WS-PH3              TO AM-TEL-NBR.
01490
01491 *    IF PSILFFL GREATER +0
CIDMOD*       MOVE PSILFFI             TO DEEDIT-FIELD
CIDMOD*       PERFORM 8600-DEEDIT
CIDMOD*       IF DEEDIT-FIELD NUMERIC
CIDMOD*          MOVE DEEDIT-FIELD-V5  TO AM-LF-PSI-FACTOR.
01498 *
01499 *    IF PSIAHFL GREATER +0
CIDMOD*       MOVE PSIAHFI             TO DEEDIT-FIELD
CIDMOD*       PERFORM 8600-DEEDIT
CIDMOD*       IF DEEDIT-FIELD NUMERIC
CIDMOD*          MOVE DEEDIT-FIELD-V5  TO AM-AH-PSI-FACTOR.
CIDMOD
01507      MOVE BIN-CURRENT-SAVE       TO AM-LAST-MAINT-DT.
01508      MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.
01509      MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.
01510
111606     IF ((PI-FST-PAGE)
111606        AND (PI-TOTAL-LINES = WS-CHANGE-ALL-CNT))
111606                      OR
111606        ((PI-2ND-PAGE)
111606        AND (PI-TOTAL-LINES = WS-CHANGE-ALL-CNT + 8))
111606                      OR
111606        ((PI-3RD-PAGE)
111606        AND (PI-TOTAL-LINES = WS-CHANGE-ALL-CNT + 16))
111606                      OR
111606        ((PI-LST-PAGE)
111606        AND (PI-TOTAL-LINES = WS-CHANGE-ALL-CNT + 24))
111606        SET ON-LAST-DATE-RANGE   TO TRUE
111606     END-IF
111606*    IF ((PI-FST-PAGE)
111606*       AND (PI-TOTAL-LINES = PI-LINE-SELECTED))
111606*                     OR
111606*       ((PI-2ND-PAGE)
111606*       AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 8))
111606*                     OR
111606*       ((PI-3RD-PAGE)
111606*       AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 16))
111606*                     OR
111606*       ((PI-LST-PAGE)
111606*       AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 24))
111606*       SET ON-LAST-DATE-RANGE   TO TRUE
111606*    END-IF
DEBUG      MOVE PI-PAGE-NUMBER TO WS-PAGE-NUMBER
           MOVE PI-TOTAL-LINES TO WS-TOTAL-LINES
           MOVE PI-LINE-SELECTED TO WS-LINE-SELECTED
      *    EXEC CICS DELAY
      *       INTERVAL (WS-DELAY-INT)
      *    END-EXEC
01511      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01512         CONTINUE
01513      ELSE
111606        IF (ON-LAST-DATE-RANGE)
01514                    OR
062121           ((PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL') AND
CIDMOD           (PCONTL > ZERO))
CIDMOD           PERFORM 1700-CHANGE-COMP
                                       THRU 1799-EXIT
              END-IF
           END-IF
01516
01517      IF NAMEL GREATER THAN ZERO
01518          PERFORM 2100-ADD-ACCOUNT-NAME THRU 2100-EXIT.
01519
01520      
      * EXEC CICS REWRITE
01521 *        FROM      (ACCOUNT-MASTER)
01522 *        DATASET   (ACCT-FILE-ID)
01523 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007066' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01524
01525      PERFORM 6300-STARTBR  THRU 6300-EXIT.
01526      PERFORM 6310-READNEXT THRU 6310-EXIT.
01527
01528      GO TO 3010-READ-NEXT.
01529
01530  3030-END-FILE.
01531      IF BROWSE-STARTED
01532         PERFORM 6320-ENDBR THRU 6320-EXIT.
01533
01534      PERFORM 6600-UPDATE-MAINT-DT THRU 6699-EXIT.
01535
080807     IF AUTO-CANCEL-ACCOUNT
              
      * EXEC CICS GETMAIN
      *          LENGTH   (ERACNT-REC-LEN)
      *          SET      (ADDRESS OF NOTE-FILE)
      *          INITIMG  (GETMAIN-SPACE)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
      *    MOVE ',"IL                  $  N#00007083' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'204E233030303037303833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERACNT-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF NOTE-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE PI-ACCT-CCGSA-KEY   TO NT-CONTROL-PRIMARY
              MOVE '1'                 TO NT-RECORD-TYPE
              MOVE +1                  TO NT-LINE-SEQUENCE
              MOVE SAVE-BIN-DATE       TO NT-LAST-MAINT-DT
              MOVE PI-PROCESSOR-ID     TO NT-LAST-MAINT-BY
              MOVE EIBTIME             TO NT-LAST-MAINT-HHMMSS
              MOVE 'CANCELLED DUE TO LOW PRODUCTION'
                                       TO NT-NOTE-LINE
              PERFORM WITH TEST AFTER UNTIL
                 (NOT RESP-DUPKEY)
                 AND (NOT RESP-DUPREC)
                 
      * EXEC CICS WRITE
      *             FROM      (NOTE-FILE)
      *             RIDFLD    (NT-CONTROL-PRIMARY)
      *             DATASET   (ERACNT-FILE-ID)
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00007100' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303037313030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACNT-FILE-ID, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 NT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF RESP-DUPKEY OR RESP-DUPREC
                    ADD +1             TO NT-LINE-SEQUENCE
                 END-IF
              END-PERFORM
           END-IF
01536      IF NOT EMI-NO-ERRORS
01537          GO TO 8200-SEND-DATAONLY.
01538
01539      MOVE -1                     TO NAMEL.
01540      MOVE ER-0000                TO EMI-ERROR.
01541      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01542      GO TO 8200-SEND-DATAONLY.
01543  EJECT
01544  3500-CHANGE-ALL-CSR.
01545      MOVE PI-ACCT-KEY            TO WS-KEY-SAVE.
01546      MOVE LOW-VALUES             TO WS-ACCT-EXP-DT
01547                                     WS-ACCT-REST-OF-EXP.
01548
01549      
      * EXEC CICS HANDLE CONDITION
01550 *         ENDFILE     (3530-END-FILE)
01551 *    END-EXEC.
      *    MOVE '"$''                   ! ( #00007124' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303037313234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01552
01553      PERFORM 6300-STARTBR THRU 6300-EXIT.
01554
01555  3510-READ-NEXT.
01556      PERFORM 6310-READNEXT THRU 6310-EXIT.
01557
01558      IF PI-ACCT-CCGSA-KEY NOT = WS-ACCT-CCGSA-KEY
01559         GO TO 3530-END-FILE.
01560
01561      
      * EXEC CICS ENDBR
01562 *        DATASET   (ACCT-FILE-ID)
01563 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007136' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01564
01565      
      * EXEC CICS READ
01566 *        UPDATE
01567 *        DATASET   (ACCT-FILE-ID)
01568 *        SET       (ADDRESS OF ACCOUNT-MASTER)
01569 *        RIDFLD    (WS-KEY-SAVE)
01570 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007140' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-KEY-SAVE, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01571
01572      MOVE BIN-CURRENT-SAVE       TO AM-LAST-MAINT-DT.
01573      MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.
01574      MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.
01575
01576      MOVE CSRI                   TO AM-CSR-CODE.
01577
01578      
      * EXEC CICS REWRITE
01579 *        FROM      (ACCOUNT-MASTER)
01580 *        DATASET   (ACCT-FILE-ID)
01581 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007153' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01582
01583      PERFORM 6300-STARTBR  THRU 6300-EXIT.
01584      PERFORM 6310-READNEXT THRU 6310-EXIT.
01585
01586      GO TO 3510-READ-NEXT.
01587
01588  3530-END-FILE.
01589      IF BROWSE-STARTED
01590         PERFORM 6320-ENDBR THRU 6320-EXIT.
01591
01592      MOVE ER-4009                TO EMI-ERROR.
01593      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01594      MOVE LOW-VALUES             TO EL6501AI.
01595
01596      PERFORM 0700-FORMAT-KEYDATA THRU 0700-EXIT.
01597      PERFORM 6500-READ-ACCT      THRU 6599-EXIT.
01598      PERFORM 6000-MOVE-TO-SCREEN THRU 6099-EXIT.
01599
01600      PERFORM 6600-UPDATE-MAINT-DT THRU 6699-EXIT.
01601
01602      MOVE -1                     TO MAINTL.
01603      MOVE AL-UANON               TO MAINTA.
01604      GO TO 8100-SEND-INITIAL-MAP.
01605
01606  EJECT
01607  4000-EDITS.
01608      IF PI-COMPANY-ID = 'NCL'
01609       IF MAINTI = 'A' OR 'C'
01610         IF CSRL GREATER ZEROS
01611             PERFORM 8050-USER-REC-READ THRU 8060-EXIT
01612           ELSE
01613             IF MAINTI = 'A'
01614                 MOVE ER-1883         TO EMI-ERROR
01615                 MOVE -1              TO CSRL
01616                 MOVE AL-UABON        TO CSRA
01617                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01618
01619      IF PI-COMPANY-ID  =  'AIG' OR 'AUK'
01620         IF NAMEL NOT GREATER ZEROS
01621            IF MAINTI = 'C'
01622               GO TO 4070-CHECK-STATUS
01623            ELSE
01624               MOVE -1               TO  NAMEL
01625               MOVE AL-UABON         TO  NAMEA
01626               MOVE ER-2045          TO  EMI-ERROR
01627               PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01628         ELSE
01629             GO TO 4065-CHECK-ASSOCIATES.
01630
01631      IF TYPEL NOT GREATER ZEROS
01632         IF MAINTI = 'C'
01633            GO TO 4050-CONTINUE
01634         ELSE
01635            GO TO 4020-BUS-TYPE-ERROR.
01636
01637      IF TYPEI NOT NUMERIC
01638         GO TO 4020-BUS-TYPE-ERROR
01639      ELSE
01640         IF TYPEI LESS 1 OR GREATER 99
01641            GO TO 4020-BUS-TYPE-ERROR.
01642
01643      MOVE SPACES                 TO ELCNTL-KEY.
01644
01645      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
01646      MOVE TYPEI                  TO CNTL-BUS-TYPE.
01647      MOVE '8'                    TO CNTL-REC-TYPE.
01648      MOVE +0                     TO CNTL-SEQ-NO.
01649
01650      
      * EXEC CICS HANDLE CONDITION
01651 *        NOTFND   (4020-BUS-TYPE-ERROR)
01652 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00007225' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303037323235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01653
01654      
      * EXEC CICS READ
01655 *        GTEQ
01656 *        DATASET   (CNTL-FILE-ID)
01657 *        SET       (ADDRESS OF CONTROL-FILE)
01658 *        RIDFLD    (ELCNTL-KEY)
01659 *    END-EXEC.
      *    MOVE '&"S        G          (   #00007229' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01660
01661      IF TYPEI LESS 21
01662         MOVE TYPEI               TO WS-BUS-ENTRY
01663      ELSE
01664         DIVIDE TYPEI BY 20
01665             GIVING WS-BUS-TYPE REMAINDER WS-BUS-ENTRY
01666         IF WS-BUS-ENTRY = ZEROS
01667            MOVE 20               TO WS-BUS-ENTRY.
01668
01669      IF CF-BUSINESS-TITLE (WS-BUS-ENTRY) NOT = SPACES
01670         GO TO 4050-CONTINUE.
01671
01672  4020-BUS-TYPE-ERROR.
01673      MOVE -1                     TO TYPEL.
01674      MOVE AL-UNBON               TO TYPEA.
01675      MOVE ER-2178                TO EMI-ERROR.
01676      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01677
01678  4050-CONTINUE.
           IF ASTATEL > 0
              MOVE SPACES                 TO  ELCNTL-KEY
              MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID
              MOVE '3'                    TO  CNTL-REC-TYPE
              MOVE ASTATEI                TO  CNTL-ACCESS (1:2)
              MOVE +0                     TO  CNTL-SEQ-NO
              
      * EXEC CICS  READ
      *           DATASET  ('ELCNTL')
      *           SET      (ADDRESS OF CONTROL-FILE)
      *           RIDFLD   (ELCNTL-KEY)
      *           RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00007260' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
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
              IF NOT RESP-NORMAL
                 MOVE ER-0144                TO  EMI-ERROR
                 MOVE -1                     TO  ASTATEL
                 MOVE AL-UABON               TO  ASTATEA
                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
              END-IF
           ELSE
              IF MAINTI = 'A'
                 MOVE ER-0144                TO  EMI-ERROR
                 MOVE -1                     TO  ASTATEL
                 MOVE AL-UABON               TO  ASTATEA
                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
              END-IF
           END-IF
01679      IF INDGRPL NOT GREATER ZEROS
01680         IF MAINTI = 'C'
01681            GO TO 4070-CHECK-STATUS
01682         ELSE
01683            GO TO 4060-IG-ERROR.
01684
01685      IF INDGRPI = 'I' OR 'G'
01686         GO TO 4070-CHECK-STATUS.
01687
01688  4060-IG-ERROR.
01689      MOVE -1                     TO INDGRPL.
01690      MOVE AL-UABON               TO INDGRPA.
01691      MOVE ER-2152                TO EMI-ERROR.
01692      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01693
01694  4065-CHECK-ASSOCIATES.
01695      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01696         MOVE  PI-ACCT-ACCOUNT    TO  WS-ACCOUNT-NUM-AREA
01697         MOVE  NAMEI              TO  WS-NAME-AREA
01698      ELSE
01699         GO TO 4070-CHECK-STATUS.
01700
01701      IF WS-NAME-1-4  =  WS-ACCT-BRANCH-CD
01702         NEXT SENTENCE
01703      ELSE
01704         MOVE -1                  TO  NAMEL
01705         MOVE AL-UABON            TO  NAMEA
01706         MOVE ER-0788             TO  EMI-ERROR
01707         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01708
01709  4070-CHECK-STATUS.
01710      IF STATUSL NOT GREATER ZEROS
01711         IF MAINTI = 'C'
01712            GO TO 4090-CHECK-STD-AH-TYPE
01713         ELSE
01714            MOVE 'A'              TO STATUSI
01715            MOVE AL-UANON         TO STATUSA
01716            GO TO 4090-CHECK-STD-AH-TYPE.
01717
022808*    IF STATUSI = 'A' OR 'I' OR 'C' OR 'F' OR 'S'
021916     IF STATUSI = 'A' OR 'I' OR 'C' OR 'F' OR 'S' OR 'D' OR 'L'
021916                      OR 'R' OR 'P'
01719         GO TO 4090-CHECK-STD-AH-TYPE.
01720
01721  4080-STATUS-ERROR.
01722      MOVE -1                     TO STATUSL.
01723      MOVE AL-UABON               TO STATUSA.
01724      MOVE ER-2153                TO EMI-ERROR.
01725      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01726
01727  4090-CHECK-STD-AH-TYPE.
01728
01729      IF PI-COMPANY-ID  = 'AIG' OR 'AUK'
01730          GO TO 4900-EXIT.
01731
01732      IF STDBENL NOT GREATER ZEROS
01733         IF MAINTI = 'C'
100703*          GO TO 4110-CHECK-REMIT
100703           GO TO 4100-CHECK-CLPTOLP
01735         ELSE
01736            MOVE ZEROS            TO STDBENI
01737            MOVE AL-UANON         TO STDBENA
100703*          GO TO 4110-CHECK-REMIT.
100703           GO TO 4100-CHECK-CLPTOLP.
01739
01740      IF STDBENI = SPACES
01741          MOVE ZEROS               TO STDBENI.
01742      IF STDBENI = ZEROS
100703*        GO TO 4110-CHECK-REMIT.
100703         GO TO 4100-CHECK-CLPTOLP.
01744
01745      MOVE STDBENI                TO WS-EDIT-BEN-CODE.
01746      IF INVALID-BENEFIT-CODE
01747         GO TO 4099-STD-BEN-ERROR.
01748
01749      MOVE SPACES                 TO ELCNTL-KEY.
01750
01751      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
01752      MOVE STDBENI                TO CNTL-BEN-TYPE.
01753      MOVE '5'                    TO CNTL-REC-TYPE.
01754      MOVE +0                     TO CNTL-SEQ-NO.
01755
01756      
      * EXEC CICS HANDLE CONDITION
01757 *        NOTFND   (4099-STD-BEN-ERROR)
01758 *    END-EXEC.
      *    MOVE '"$I                   ! * #00007362' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303037333632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01759
01760      
      * EXEC CICS READ
01761 *        GTEQ
01762 *        DATASET   (CNTL-FILE-ID)
01763 *        SET       (ADDRESS OF CONTROL-FILE)
01764 *        RIDFLD    (ELCNTL-KEY)
01765 *    END-EXEC.
      *    MOVE '&"S        G          (   #00007366' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01766
01767      IF CNTL-COMP-ID  = CF-COMPANY-ID  AND
01768         CNTL-REC-TYPE = CF-RECORD-TYPE
01769          MOVE 1                  TO SUB
01770      ELSE
01771          GO TO 4099-STD-BEN-ERROR.
01772
01773  4095-CHECK-BEN-TYPE.
01774      IF STDBENI = CF-BENEFIT-CODE (SUB)
100703*       GO TO 4110-CHECK-REMIT.
100703        GO TO 4100-CHECK-CLPTOLP.
01776
01777      IF SUB LESS 8
01778          ADD 1 TO SUB
01779          GO TO 4095-CHECK-BEN-TYPE.
01780
01781  4099-STD-BEN-ERROR.
01782      MOVE -1                     TO STDBENL.
01783      MOVE AL-UABON               TO STDBENA.
01784      MOVE ER-0250                TO EMI-ERROR.
01785      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01786
100703 4100-CHECK-CLPTOLP.
100703
100703     IF CLPTOLPL > ZERO
100703         
      * EXEC CICS BIF
100703*            DEEDIT
100703*            FIELD    (CLPTOLPI)
100703*            LENGTH   (6)
100703*        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007397' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLPTOLPI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100703         IF CLPTOLPI NOT NUMERIC
100703             MOVE ER-1778        TO EMI-ERROR
100703             MOVE -1             TO CLPTOLPL
100703             MOVE AL-UNBON       TO CLPTOLPA
100703             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100703         END-IF
100703     END-IF.
092705     IF LCOMML > ZERO
092705         
      * EXEC CICS BIF
092705*            DEEDIT
092705*            FIELD    (LCOMMI)
092705*            LENGTH   (8)
092705*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007410' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCOMMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
092705         IF LCOMMI NOT NUMERIC
092705             MOVE ER-1778        TO EMI-ERROR
092705             MOVE -1             TO LCOMML
092705             MOVE AL-UNBON       TO LCOMMA
092705             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
092705         END-IF
092705     END-IF
           IF MAXMFEEL > ZEROS
              
      * EXEC CICS BIF DEEDIT
      *          FIELD   (MAXMFEEI)
      *          LENGTH  (5)
      *       END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007423' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAXMFEEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF MAXMFEEI NUMERIC
                 MOVE MAXMFEEI         TO WS-WORK-FIELD
                 MOVE WS-WORK-NUM      TO MAXMFEEO
              ELSE
                 MOVE -1               TO MAXMFEEL
                 MOVE AL-UABON         TO MAXMFEEA
                 MOVE ER-7717          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF (CLPSTL > 0)
               MOVE AL-UANON           TO CLPSTA
              IF (CLPSTI NOT = SPACES)
                 MOVE SPACES           TO ELCNTL-KEY
                 MOVE PI-COMPANY-ID    TO CNTL-COMP-ID
                 MOVE '3'              TO CNTL-REC-TYPE
                 MOVE CLPSTI           TO CNTL-ACCESS (1:2)
                 MOVE +0               TO CNTL-SEQ-NO
                 
      * EXEC CICS  READ
      *              DATASET  ('ELCNTL')
      *              SET      (ADDRESS OF CONTROL-FILE)
      *              RIDFLD   (ELCNTL-KEY)
      *              RESP     (WS-RESPONSE)
      *          END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00007445' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
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
                 IF NOT RESP-NORMAL
                    MOVE ER-0144       TO EMI-ERROR
                    MOVE -1            TO CLPSTL
                    MOVE AL-UABON      TO CLPSTA
                    PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
                 END-IF
              ELSE
                 MOVE ER-0144          TO EMI-ERROR
                 MOVE -1               TO CLPSTL
                 MOVE AL-UABON         TO CLPSTA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              END-IF
           END-IF
           IF (PRODCDL > ZEROS)
              and (emi-error not = er-0144)
070714        if prodcdi = spaces
070714           continue
070714        else
070714           move low-values       to erpdef-key
070714           move pi-company-cd    to erpdef-company-cd
070714           move clpsti           to erpdef-state
070714           move prodcdi          to erpdef-prod-cd
070714           move erpdef-key       to erpdef-key-save
070714           
      * exec cics read
070714*             dataset ('ERPDEF')
070714*             set     (address of product-master)
070714*             ridfld  (erpdef-key)
070714*             length  (6)
070714*             gteq
070714*             resp    (ws-response)
070714*          end-exec
           MOVE 'ERPDEF' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00007476' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 erpdef-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF product-master TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
070714           if (resp-normal)
070714              and (pd-control-primary (1:6) =
070714                           erpdef-key-save (1:6))
070714              continue
070714           else
070714              move er-8156       to emi-error
070714              MOVE -1            TO PRODCDL
070714              MOVE AL-UABON      TO PRODCDA
070714              PERFORM 9900-ERROR-FORMAT
070714                                 THRU  9900-EXIT
070714           end-if
070714        end-if
070714     end-if
           .
01787  4110-CHECK-REMIT.
01788      IF REMITL NOT GREATER ZEROS
01789         MOVE 1                   TO REMITI
01790         MOVE AL-UNNON            TO REMITA
01791         GO TO 4120-REMIT-ERROR.
01792
01793      IF (REMITI NOT NUMERIC)  OR
01794         (REMITI LESS 1 OR GREATER 10)
01795            GO TO 4120-REMIT-ERROR.
01796
01797  4115-CHECK-AGENT.
01798      SET M-INDEX                 TO REMITI.
01799
01800      IF AGENTL (M-INDEX)  NOT GREATER ZEROS
01801         MOVE ER-2156             TO EMI-ERROR
01802         GO TO 4120-R-ERROR.
01803
01804      GO TO 4130-CHECK-CONTRACT.
01805
01806  4120-REMIT-ERROR.
01807      MOVE ER-2155                TO EMI-ERROR.
01808
01809  4120-R-ERROR.
01810      MOVE -1                     TO REMITL.
01811      MOVE AL-UABON               TO REMITA.
01812      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01813
01814  4130-CHECK-CONTRACT.
01815      IF CONTRL NOT = ZEROS AND CONTRI NOT = SPACES
01816         MOVE CONTRI                TO DEEDIT-FIELD
01817         PERFORM 8600-DEEDIT
01818         MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY
01819         MOVE '4'                    TO DC-OPTION-CODE
01820         PERFORM 9700-DATE-LINK
01821         IF DATE-CONVERSION-ERROR
01822            MOVE ER-2157             TO EMI-ERROR
01823            MOVE -1                  TO CONTRL
01824            MOVE AL-UABON            TO CONTRA
01825            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01826         ELSE
01827 ***  Y2K PROJ 7744
01828            MOVE DC-GREG-DATE-1-EDIT TO CONTRI
01829            MOVE AL-UANON            TO CONTRA
01830            MOVE DC-GREG-DATE-1-YMD  TO WS-ANVR-DT
01831            MOVE DC-ALPHA-CENTURY    TO WS-ANVR-DT(4:2)
01832         END-IF
01833      END-IF.
01834 ***  Y2K PROJ 7744
01835
01836      IF PI-COMPANY-ID = 'DMD'
01837        IF PI-MAINT = 'A'
01838         IF CONTRL = ZERO
01839            MOVE ER-0852             TO EMI-ERROR
01840            MOVE -1                  TO CONTRL
01841            MOVE AL-UABON            TO CONTRA
01842            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01843
01844 *4131-CHECK-PSI-FACTOR.
01845 *    IF PSILFFL GREATER +0
CIDMOD*       MOVE PSILFFI             TO DEEDIT-FIELD
CIDMOD*       PERFORM 8600-DEEDIT
CIDMOD*       IF DEEDIT-FIELD NUMERIC
CIDMOD*          MOVE DEEDIT-FIELD-V5  TO WS-LIFE-PSI
CIDMOD*                                   PSILFFO
01853 *       ELSE
01854 *          MOVE ER-9999          TO EMI-ERROR
01855 *          MOVE -1               TO PSILFFL
01856 *          MOVE AL-UABON         TO PSILFFA
01857 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01858 *
01859 *    IF PSIAHFL GREATER +0
CIDMOD*       MOVE PSIAHFI             TO DEEDIT-FIELD
CIDMOD*       PERFORM 8600-DEEDIT
CIDMOD*       IF DEEDIT-FIELD NUMERIC
CIDMOD*          MOVE DEEDIT-FIELD-V5  TO WS-AH-PSI
CIDMOD*                                   PSIAHFO
01867 *       ELSE
01868 *          MOVE ER-9999          TO EMI-ERROR
01869 *          MOVE -1               TO PSIAHFL
01870 *          MOVE AL-UABON         TO PSIAHFA
01871 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01872
01873  4135-CHECK-PREVIOUS-DATES.
01874      IF PEFFDTEL NOT = ZEROS AND PEFFDTEI NOT = SPACES
01875         MOVE PEFFDTEI              TO DEEDIT-FIELD
01876         PERFORM 8600-DEEDIT
01877         MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY
01878         MOVE '4'                    TO DC-OPTION-CODE
01879         PERFORM 9700-DATE-LINK
01880         IF DATE-CONVERSION-ERROR
01881            MOVE ER-0348             TO EMI-ERROR
01882            MOVE -1                  TO PEFFDTEL
01883            MOVE AL-UABON            TO PEFFDTEA
01884            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01885         ELSE
01886 ***  Y2K PROJ 7744
01887            MOVE DC-GREG-DATE-1-EDIT TO PEFFDTEI
01888            MOVE AL-UANON            TO PEFFDTEA
01889            MOVE DC-GREG-DATE-1-YMD  TO WS-PEFF-DT
01890            MOVE DC-ALPHA-CENTURY    TO WS-PEFF-DT(4:2)
01891         END-IF
01892      END-IF.
01893
01894      IF PEXPDTEL NOT = ZEROS AND PEXPDTEI NOT = SPACES
01895         MOVE PEXPDTEI              TO DEEDIT-FIELD
01896         PERFORM 8600-DEEDIT
01897         IF DEEDIT-FIELD-V0 NOT LESS 999999
01898            MOVE '99/99/99'             TO PEXPDTEO
01899            MOVE 99999999999            TO WS-PEXP-DT
01900            MOVE AL-UANON               TO PEXPDTEA
01901         ELSE
01902            MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY
01903            MOVE '4'                    TO DC-OPTION-CODE
01904            PERFORM 9700-DATE-LINK
01905            IF DATE-CONVERSION-ERROR
01906               MOVE ER-0454             TO EMI-ERROR
01907               MOVE -1                  TO PEXPDTEL
01908               MOVE AL-UABON            TO PEXPDTEA
01909               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01910            ELSE
01911               MOVE DC-GREG-DATE-1-EDIT TO PEXPDTEI
01912               MOVE AL-UANON            TO PEXPDTEA
01913               MOVE DC-GREG-DATE-1-YMD  TO WS-PEXP-DT
01914               MOVE DC-ALPHA-CENTURY    TO WS-PEXP-DT(4:2)
01915            END-IF
01916         END-IF
01917      END-IF.
01918 ***  Y2K PROJ 7744
01919
01920      IF COMMCALL NOT GREATER ZEROS
01921          GO TO 4150-CHECK-REINCAL.
01922
01923      IF (PI-COMPANY-ID = 'ADL' OR 'DEF') AND
01924          (EFFCHG-SAVE LESS 841101)       AND
01925          (COMMCALI = 'Y' OR '1')         AND
01926          (NOT MODIFY-CAP)
01927            MOVE -1               TO COMMCALL
01928            MOVE AL-UABON         TO COMMCALA
01929            MOVE ER-2189          TO EMI-ERROR
01930            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01931
01932      IF COMMCALI = 'Y' OR 'N' OR '1' OR ' '
01933          GO TO 4150-CHECK-REINCAL.
01934
01935  4140-COMMCAL-ERROR.
01936      MOVE -1                     TO COMMCALL.
01937      MOVE AL-UABON               TO COMMCALA.
01938      MOVE ER-2158                TO EMI-ERROR.
01939      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01940
01941  4150-CHECK-REINCAL.
01942      IF REINCALL NOT GREATER ZEROS
01943          GO TO 4170-CHECK-REINTAB.
01944
01945      IF REINCALI = 'Y' OR 'N'
01946         GO TO 4170-CHECK-REINTAB.
01947
01948  4160-REINCAL-ERROR.
01949      MOVE -1                     TO REINCALL.
01950      MOVE AL-UABON               TO REINCALA.
01951      MOVE ER-2179                TO EMI-ERROR.
01952      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01953
01954  4170-CHECK-REINTAB.
01955      IF REINTABL NOT GREATER ZEROS
01956         IF PI-COMPANY-ID = 'NCL' AND
01957            MAINTI = 'A'
01958                 MOVE ER-4010         TO EMI-ERROR
01959                 MOVE -1              TO REINTABL
01960                 MOVE AL-UABON        TO REINTABA
01961                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01962
01963      IF REINTABL NOT GREATER ZEROS OR REINTABI = SPACES
01964          GO TO 4410-CHECK-AGENTS.
042809     IF PI-COMPANY-ID = 'CID'
042809        IF PI-ACCT-STATE = 'KY'
042809           IF REINTABI NOT = 'K98' AND 'K99' AND 'KYA'
042809              MOVE -1            TO REINTABL
042809              MOVE AL-UABON      TO REINTABA
042809              MOVE ER-2159       TO EMI-ERROR
042809              PERFORM 9900-ERROR-FORMAT
042809                                 THRU 9900-EXIT
042809           END-IF
042809        END-IF
042809     END-IF
01966      MOVE LOW-VALUES             TO REIN-KEY.
01967
01968      MOVE PI-COMPANY-CD          TO REIN-COMP-CD.
01969      MOVE 'A'                    TO REIN-CODE.
01970      MOVE REINTABI               TO REIN-TABLE.
01971
01972      
      * EXEC CICS HANDLE CONDITION
01973 *        NOTFND   (4180-REIN-ERROR)
01974 *    END-EXEC.
      *    MOVE '"$I                   ! + #00007689' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303037363839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01975
01976      
      * EXEC CICS READ
01977 *        DATASET   (REIN-FILE-ID)
01978 *        SET       (ADDRESS OF REINSURANCE-RECORD)
01979 *        RIDFLD    (REIN-KEY)
01980 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007693' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 REIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01981
01982      GO TO 4410-CHECK-AGENTS.
01983
01984  4180-REIN-ERROR.
01985      MOVE -1                     TO REINTABL.
01986      MOVE AL-UABON               TO REINTABA.
042809     MOVE ER-2615                TO EMI-ERROR.
01988      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01989
01990  EJECT
01991  4410-CHECK-AGENTS.
01992      MOVE PI-COMPANY-CD          TO COMM-COMP-CD.
01993      SET M-INDEX TO 1.
01994
01995      IF PI-COMPANY-ID = 'NCL'
01996        IF AGENTL (M-INDEX) GREATER ZEROS
01997          MOVE PI-ACCT-ACCOUNT    TO AGENT  (M-INDEX).
01998
01999  4420-LOOP.
02000      IF AGENTL (M-INDEX) NOT GREATER ZEROS
02001         IF (ATYPEL (M-INDEX)  GREATER ZEROS AND
02002             ATYPE  (M-INDEX) NOT = SPACES)  OR
02003            SINGLEL (M-INDEX) GREATER ZEROS  OR
02004            JOINTL  (M-INDEX) GREATER ZEROS  OR
02005            A-HL    (M-INDEX) GREATER ZEROS  OR
02006            RECALL  (M-INDEX) GREATER ZEROS  OR
02007            RCOMML  (M-INDEX) GREATER ZEROS  OR
02008            CHGBCKL (M-INDEX) GREATER ZEROS  OR
102717           CCEL    (M-INDEX) GREATER ZEROS
02010            MOVE ER-2172          TO EMI-ERROR
02011            MOVE -1               TO AGENTL (M-INDEX)
02012            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02013            GO TO 4450-BUMP-INDEX
102717        else
102717           go to 4430-check-single
02014 *       ELSE
02015 *          GO TO 4450-BUMP-INDEX.
102717        end-if
102717     end-if
02017      IF AGENT (M-INDEX) = ZEROS
02018          MOVE ER-2970            TO  EMI-ERROR
02019          MOVE -1                 TO  AGENTL (M-INDEX)
02020          MOVE AL-UABON           TO  AGENTA (M-INDEX)
02021          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02022          GO TO 4450-BUMP-INDEX.
02023
02024      IF ATYPEL (M-INDEX) = ZEROS
02025         MOVE -1                  TO ATYPEL (M-INDEX)
02026         MOVE ER-2173             TO EMI-ERROR
02027         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02028         GO TO 4430-CHECK-SINGLE.
02029
02030      IF ATYPE (M-INDEX) = 'C'
02031         ADD 1                    TO WS-ACCOUNT-CTR-C
02032        ELSE
02033         IF ATYPE (M-INDEX) = 'R'
02034            ADD 1                 TO WS-ACCOUNT-CTR-R
02035           ELSE
02036            IF ATYPE (M-INDEX) = 'D'
02037               ADD 1                 TO WS-ACCOUNT-CTR-D
02038              ELSE
02039               IF ATYPE (M-INDEX) = 'O' OR 'P' OR 'T' OR 'W'
100703                                OR 'B' OR 'I' OR 'K' OR 'L'
011410                                OR 'J' OR 'M' OR 'A' OR 'N'
052814                                OR 'S'
02040                  NEXT SENTENCE
02041                 ELSE
02042                  IF ATYPE (M-INDEX) = SPACES
02043                     NEXT SENTENCE
02044                    ELSE
02045                     MOVE ER-2175       TO EMI-ERROR
02046                     MOVE -1            TO ATYPEL (M-INDEX)
02047                     MOVE AL-UABON      TO ATYPEA (M-INDEX)
02048                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02049
02050  4430-CHECK-SINGLE.
           IF SINGLEL (M-INDEX) GREATER ZEROS
              IF SINGLE-COMM-R (M-INDEX) NOT NUMERIC
                 MOVE SINGLE-COMM-T (M-INDEX)    TO COMM-TABLE
                 MOVE PI-LIFE-OVERRIDE-L1        TO COMM-LF-AH
                 MOVE LOW-VALUES                 TO COMM-FILLER
                 PERFORM 6400-READ-COMMISSION THRU 6459-EXIT
                 IF COMMISSION-ERROR
                    MOVE AL-UABON      TO SINGLEA (M-INDEX)
                    MOVE -1            TO SINGLEL (M-INDEX)
                 ELSE
102717              if cceind(m-index) <> 'Y'
102717                 add ws-table-max to COMM-SL-ACCUM
102717                 if atype (m-index) = 'C' or 'D'
102717                    add ws-table-max to comm-sl-accum-al
102717                 else
102717                    if atype (m-index) = 'O' OR 'P'
102717                       add ws-table-max to comm-sl-accum-gl
102717                    end-if
102717                 end-if
102717              end-if
                 END-IF
              END-IF
           END-IF
           IF JOINTL (M-INDEX) GREATER ZEROS
              IF JOINT-COMM-R (M-INDEX) NOT NUMERIC
                 MOVE JOINT-COMM-T (M-INDEX) TO COMM-TABLE
                 MOVE PI-LIFE-OVERRIDE-L1    TO COMM-LF-AH
                 MOVE LOW-VALUES             TO COMM-FILLER
                 PERFORM 6400-READ-COMMISSION THRU 6459-EXIT
                 IF COMMISSION-ERROR
                    MOVE AL-UABON      TO JOINTA (M-INDEX)
                    MOVE -1            TO JOINTL (M-INDEX)
                 ELSE
102717              if cceind(m-index) <> 'Y'
102717                 add ws-table-max to COMM-JL-ACCUM
102717                 if atype (m-index) = 'C' or 'D'
102717                    add ws-table-max to comm-jl-accum-al
102717                 else
102717                    if atype (m-index) = 'O' OR 'P'
102717                       add ws-table-max to comm-jl-accum-gl
102717                 end-if
102717              end-if
                 END-IF
              END-IF
           END-IF
           IF A-HL (M-INDEX) GREATER ZEROS
              IF A-H-COMM-R (M-INDEX)  NOT NUMERIC
                 MOVE A-H-COMM-T (M-INDEX)      TO COMM-TABLE
                 MOVE PI-AH-OVERRIDE-L1         TO COMM-LF-AH
                 MOVE LOW-VALUES                 TO COMM-FILLER
                 PERFORM 6400-READ-COMMISSION THRU 6459-EXIT
                 IF COMMISSION-ERROR
                    MOVE AL-UABON      TO A-HA (M-INDEX)
                    MOVE -1            TO A-HL (M-INDEX)
                 ELSE
102717              if cceind(m-index) <> 'Y'
102717                 add ws-table-max to COMM-ah-ACCUM
102717                 if atype (m-index) = 'C' or 'D'
102717                    add ws-table-max to comm-ah-accum-al
102717                 else
102717                    if atype (m-index) = 'O' OR 'P'
102717                       add ws-table-max to comm-ah-accum-gl
102717                 end-if
102717              end-if
                 END-IF
              END-IF
           END-IF
02078      IF RECALL (M-INDEX) GREATER ZEROS
02079          IF RECAL (M-INDEX) = 'Y' OR 'N' OR '1' OR SPACE
02080              MOVE AL-UANON       TO RECALA (M-INDEX)
02081          ELSE
02082              MOVE -1             TO RECALL (M-INDEX)
02083              MOVE AL-UABON       TO RECALA (M-INDEX)
02084              MOVE ER-2158        TO EMI-ERROR
02085              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02086
02087      IF RCOMML (M-INDEX) GREATER ZEROS
02088          IF RCOMM (M-INDEX) = 'Y' OR 'N' OR 'A' OR SPACE
02089              MOVE AL-UANON       TO RCOMMA (M-INDEX)
02090          ELSE
02091              MOVE -1             TO RCOMML (M-INDEX)
02092              MOVE AL-UABON       TO RCOMMA (M-INDEX)
02093              MOVE ER-0625        TO EMI-ERROR
02094              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02095
02096      IF  CHGBCKL (M-INDEX)  GREATER THAN  ZEROS
02097          IF  CHGBCK (M-INDEX)    NUMERIC
02098              MOVE  AL-UNNON        TO  CHGBCKA (M-INDEX)
02099          ELSE
02100              MOVE  -1              TO  CHGBCKL (M-INDEX)
02101              MOVE  AL-UNBON        TO  CHGBCKA (M-INDEX)
02102              MOVE  ER-0759         TO  EMI-ERROR
02103              PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT.
102717     IF CCEL (M-INDEX) <> ZEROS
              IF CCEIND(M-INDEX) = 'Y'
                 MOVE AL-UANON         TO CCEA(M-INDEX)
                 IF ATYPE(M-INDEX) = 'C' OR 'D'
                    IF WS-COMM-CAP-LIMIT-TO <> 'A' AND 'B'
102717                 MOVE  -1        TO CCEL (M-INDEX)
102717                 MOVE  AL-UABON  TO CCEA (M-INDEX)
102717                 MOVE  ER-3065   TO EMI-ERROR
102717                 PERFORM  9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
                    END-IF
                 ELSE
                    IF ATYPE(M-INDEX) = 'O' OR 'P'
                       IF WS-COMM-CAP-LIMIT-TO <> 'G' AND 'B'
102717                    MOVE  -1        TO CCEL (M-INDEX)
102717                    MOVE  AL-UABON  TO CCEA (M-INDEX)
102717                    MOVE  ER-3065   TO EMI-ERROR
102717                    PERFORM  9900-ERROR-FORMAT
102717                                    THRU 9900-EXIT
                       END-IF
                    END-IF
                 END-IF
              ELSE
102717           IF CCEIND (M-INDEX) = 'N' OR ' '
102717              MOVE  AL-UANON        TO CCEA (M-INDEX)
102717           ELSE
102717              MOVE  -1              TO CCEL (M-INDEX)
102717              MOVE  AL-UABON        TO CCEA (M-INDEX)
102717              MOVE  ER-3066         TO EMI-ERROR
102717              PERFORM  9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717           END-IF
102717        END-IF
102717     END-IF
CIDMOD     IF ATYPE (M-INDEX) NOT = 'C' AND 'D' AND 'O' AND 'P'
              AND 'J'
CIDMOD         GO TO 4450-BUMP-INDEX.
102717     IF SINGLE-COMM-R (M-INDEX) NUMERIC
102717        if cceind(m-index) <> 'Y'
102717           ADD SINGLE-COMM-R (M-INDEX) TO COMM-SL-ACCUM
102717           if atype (m-index) = 'C' or 'D'
102717              add SINGLE-COMM-R (M-INDEX) to comm-sl-accum-al
102717           else
102717              if atype (m-index) = 'O' or 'P'
102717                 add single-comm-r (m-index) to comm-sl-accum-gl
102717              end-if
102717           end-if
102717        end-if
102717     else
102717        display ' single comm r not numeric '
102717     end-if
102717
102717     IF JOINT-COMM-R (M-INDEX) NUMERIC
102717        if cceind(m-index) <> 'Y'
102717           ADD JOINT-COMM-R (M-INDEX)  TO COMM-JL-ACCUM
102717           if atype (m-index) = 'C' or 'D'
102717              add JOINT-COMM-R (M-INDEX) to comm-jl-accum-al
102717           else
102717              if atype (m-index) = 'O' or 'P'
102717                 add joint-comm-r (m-index) to comm-jl-accum-gl
102717              end-if
102717           end-if
102717        end-if
102717     end-if
102717
102717     IF A-H-COMM-R (M-INDEX) NUMERIC
102717        if cceind(m-index) <> 'Y'
102717           ADD A-H-COMM-R (M-INDEX)    TO COMM-AH-ACCUM
102717           if atype (m-index) = 'C' or 'D'
102717              add A-H-COMM-R (M-INDEX) to comm-ah-accum-al
102717           else
102717              if atype (m-index) = 'O' or 'P'
102717                 add a-h-comm-r (m-index) to comm-ah-accum-gl
102717              end-if
102717           end-if
102717        end-if
102717     END-IF
           .
02114  4450-BUMP-INDEX.
02115      SET M-INDEX UP BY 1.
02116
02117      IF M-INDEX NOT = 11
02118         GO TO 4420-LOOP.
02119
02120      IF WS-ACCOUNT-CTR-C = ZEROS AND
02121         WS-ACCOUNT-CTR-D = ZEROS AND
02122         WS-ACCOUNT-CTR-R = ZEROS
02123           MOVE ER-2177             TO EMI-ERROR
02124           MOVE -1                  TO AGENTL (1)
02125           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02126           GO TO 4900-EXIT.
02127
02128      IF (WS-ACCOUNT-CTR-C  GREATER 0 AND
02129          WS-ACCOUNT-CTR-D  GREATER 0) OR
02130         (WS-ACCOUNT-CTR-R  GREATER 0 AND
02131          WS-ACCOUNT-CTR-D  GREATER 0)
02132            MOVE ER-2174             TO EMI-ERROR
02133            MOVE -1                  TO AGENTL (1)
02134            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02135            GO TO 4900-EXIT.
02136
02137      IF WS-ACCOUNT-CTR-C GREATER 1 OR
02138         WS-ACCOUNT-CTR-R GREATER 1 OR
02139         WS-ACCOUNT-CTR-D GREATER 1
02140           MOVE ER-2182             TO EMI-ERROR
02141           MOVE -1                  TO AGENTL (1)
02142           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02143           GO TO 4900-EXIT.
02144
CIDMOD
102717     IF WS-ST-COMM-CAP-SL NOT NUMERIC
102717        MOVE ZEROS               TO WS-ST-COMM-CAP-SL
102717     end-if
102717     IF WS-ST-COMM-CAP-JL NOT NUMERIC
102717        MOVE ZEROS               TO WS-ST-COMM-CAP-JL
102717     END-IF
102717     IF WS-ST-COMM-CAP-SA NOT NUMERIC
102717        MOVE ZEROS               TO WS-ST-COMM-CAP-SA
102717     END-IF
102717
102717     if ws-st-comm-cap-sl = zeros
102717        continue
102717     else
102717        if comm-sl-accum-al numeric
102717           if (comm-sl-accum-al > ws-st-comm-cap-sl)
102717              MOVE -1         TO MAINTL
102717              MOVE ER-1955    TO EMI-ERROR
102717              PERFORM 9900-ERROR-FORMAT
102717                              THRU 9900-EXIT
102717           end-if
102717        end-if
102717     end-if
102717
102717     if ws-st-comm-cap-jl = zeros
102717        continue
102717     else
102717        if comm-jl-accum-al numeric
102717           if (comm-jl-accum-al > ws-st-comm-cap-jl)
102717              MOVE -1         TO MAINTL
102717              MOVE ER-1956    TO EMI-ERROR
102717              PERFORM 9900-ERROR-FORMAT
102717                              THRU 9900-EXIT
102717           end-if
102717        end-if
102717     end-if
102717
102717     if ws-st-comm-cap-sa = zeros
102717        continue
102717     else
102717        if comm-ah-accum-al numeric
102717           if (comm-ah-accum-al > ws-st-comm-cap-sa)
102717              MOVE -1         TO MAINTL
102717              MOVE ER-1957    TO EMI-ERROR
102717              PERFORM 9900-ERROR-FORMAT
102717                              THRU 9900-EXIT
102717           end-if
102717        end-if
102717     end-if
102717     if ws-st-ga-comm-cap-sl = zeros
102717        continue
102717     else
102717        if comm-sl-accum-gl numeric
102717           if (comm-sl-accum-gl > ws-st-ga-comm-cap-sl)
102717              MOVE -1         TO MAINTL
102717              MOVE ER-1958    TO EMI-ERROR
102717              PERFORM 9900-ERROR-FORMAT
102717                              THRU 9900-EXIT
102717           end-if
102717        end-if
102717     end-if
102717
102717     if ws-st-ga-comm-cap-jl = zeros
102717        continue
102717     else
102717        if comm-jl-accum-gl numeric
102717           if (comm-jl-accum-gl > ws-st-ga-comm-cap-jl)
102717              MOVE -1         TO MAINTL
102717              MOVE ER-1959    TO EMI-ERROR
102717              PERFORM 9900-ERROR-FORMAT
102717                              THRU 9900-EXIT
102717           end-if
102717        end-if
102717     end-if
102717
102717     if ws-st-ga-comm-cap-sa = zeros
102717        continue
102717     else
102717        if comm-ah-accum-gl numeric
102717           if (comm-ah-accum-gl > ws-st-ga-comm-cap-sa)
102717              MOVE -1         TO MAINTL
102717              MOVE ER-1960    TO EMI-ERROR
102717              PERFORM 9900-ERROR-FORMAT
102717                              THRU 9900-EXIT
102717           end-if
102717        end-if
102717     end-if
102717     if ws-st-tot-comm-cap-sl = zeros
102717        continue
102717     else
102717        if comm-sl-accum numeric
102717           if (comm-sl-accum > ws-st-tot-comm-cap-sl)
102717              MOVE -1         TO MAINTL
102717              MOVE ER-1961    TO EMI-ERROR
102717              PERFORM 9900-ERROR-FORMAT
102717                              THRU 9900-EXIT
102717           end-if
102717        end-if
102717     end-if
102717
102717     if ws-st-tot-comm-cap-jl = zeros
102717        continue
102717     else
102717        if comm-jl-accum numeric
102717           if (comm-jl-accum > ws-st-tot-comm-cap-jl)
102717              MOVE -1         TO MAINTL
102717              MOVE ER-1962    TO EMI-ERROR
102717              PERFORM 9900-ERROR-FORMAT
102717                              THRU 9900-EXIT
102717           end-if
102717        end-if
102717     end-if
102717
102717     if ws-st-tot-comm-cap-sa = zeros
102717        continue
102717     else
102717        if comm-ah-accum numeric
102717           if (comm-ah-accum > ws-st-tot-comm-cap-sa)
102717              MOVE -1         TO MAINTL
102717              MOVE ER-1963    TO EMI-ERROR
102717              PERFORM 9900-ERROR-FORMAT
102717                              THRU 9900-EXIT
102717           end-if
102717        end-if
102717     end-if
02145      IF MAINTI NOT = 'C'
02146         GO TO 4900-EXIT.
02147
02148      IF NOT PI-GA-BILLING
02149         GO TO 4900-EXIT.
02150
02151  4460-CHECK-IF-GA-BILLED.
02152      PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT.
02153
02154      MOVE PI-COMM-POINTER        TO LCP-WS-ADDR-COMP
02155      SET ADDRESS OF COMMISSION-WORK-AREA TO LCP-WS-ADDR-PNTR.
02156      SET M-INDEX TO +1.
02157      MOVE +1 TO AXRF-SUB.
02158
02159  4470-GA-LOOP.
02160      IF ((AGENT (M-INDEX) NOT = WK-AM-AGT (AXRF-SUB)  OR
02161         ATYPE  (M-INDEX) NOT = WK-AM-TYPE (AXRF-SUB)) OR
02162         SINGLEL (M-INDEX) GREATER ZERO                OR
02163         JOINTL  (M-INDEX) GREATER ZERO                OR
02164         A-HL    (M-INDEX) GREATER ZERO                OR
02165         RCOMML  (M-INDEX) GREATER ZERO                OR
02166         CHGBCKL (M-INDEX) GREATER ZERO                OR
102717        CCEL    (M-INDEX) GREATER ZERO)               AND
02168         (WK-GA-BILL-DT (AXRF-SUB) NOT = LOW-VALUES AND SPACES)
02169            MOVE ER-2131   TO EMI-ERROR
02170            MOVE -1        TO AGENTL (M-INDEX)
02171            MOVE AL-UABON  TO AGENTA (M-INDEX)
02172            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02173
02174      ADD +1 TO AXRF-SUB.
02175      SET M-INDEX UP BY +1.
02176
02177      IF AXRF-SUB NOT = +11
02178         GO TO 4470-GA-LOOP.
02179
02180  4900-EXIT.
02181       EXIT.
02182  EJECT
02183  5000-MOVE-TO-RECORD.
02184
02185      IF NAMEL   GREATER THAN ZERO  OR
02186         INCAREL GREATER THAN ZERO  OR
02187         ADDR1L  GREATER THAN ZERO  OR
02188         ACITYL  GREATER THAN ZERO  OR
02188         ASTATEL GREATER THAN ZERO  OR
02189         ZIPL    GREATER THAN ZERO  OR
02190         PHONEL  GREATER THAN ZERO  OR
02191         TAXNOL  GREATER THAN ZERO
02192          MOVE '*'                TO AM-PROFILE-CHANGE-SWITCH.
02193
02194      IF CSRL GREATER ZEROS
02195         MOVE CSRI                TO AM-CSR-CODE.
02196      IF NAMEL GREATER ZEROS
02197         MOVE NAMEI               TO AM-NAME.
CIDMOD     IF PCONTL GREATER ZEROS
CIDMOD        MOVE PCONTI              TO AM-CONTROL-NAME
CIDMOD     END-IF
02198      IF INCAREL GREATER ZEROS
02199         MOVE INCAREI             TO AM-PERSON.
02200      IF ADDR1L GREATER ZEROS
02201         MOVE ADDR1I              TO AM-ADDRS.
02202      IF TAXNOL GREATER ZEROS
02203         MOVE TAXNOI              TO AM-ID-NO.
02204      IF PHONEL GREATER ZEROS
02205         MOVE PHONEI              TO DEEDIT-FIELD
02206         PERFORM 8600-DEEDIT
02207         MOVE DEEDIT-FIELD-V0     TO WS-PHONE-NUM
02208         MOVE WS-PH1              TO AM-AREA-CODE
02209         MOVE WS-PH2              TO AM-TEL-PRE
02210         MOVE WS-PH3              TO AM-TEL-NBR.
02211
02212      IF ACITYL GREATER ZEROS
02213         MOVE ACITYI             TO AM-ADDR-CITY.
02212      IF ASTATEL GREATER ZEROS
02213         MOVE ASTATEI            TO AM-ADDR-STATE.
02214
02215      IF ZIPL NOT GREATER ZEROS
02216          GO TO 5010-CONT-MOVING.
02217
02218      MOVE ZIPI                   TO WS-ZIP-CODE.
02219
02220      IF WS-CANADIAN-ZIP
02221          IF WS-ZIP-4 = SPACE  OR  '-'
02222              MOVE WS-ZIP-CAN-2-POST1  TO AM-CAN-POSTAL-1
02223              MOVE WS-ZIP-CAN-2-POST2  TO AM-CAN-POSTAL-2
02224          ELSE
02225              MOVE WS-ZIP-CAN-1-POST1  TO AM-CAN-POSTAL-1
02226              MOVE WS-ZIP-CAN-1-POST2  TO AM-CAN-POSTAL-2
02227      ELSE
02228          IF WS-ZIP-6 = SPACE  OR  '-'
02229              MOVE WS-ZIP-AM-2-CODE    TO AM-ZIP-PRIME
02230              MOVE WS-ZIP-AM-2-PLUS4   TO AM-ZIP-PLUS4
02231          ELSE
02232              MOVE WS-ZIP-AM-1-CODE    TO AM-ZIP-PRIME
02233              MOVE WS-ZIP-AM-1-PLUS4   TO AM-ZIP-PLUS4.
02234
02235  5010-CONT-MOVING.
02236      IF TYPEL GREATER ZEROS
02237         MOVE TYPEI               TO AM-GPCD.
02238
02239      IF STDBENL GREATER ZEROS
02240         MOVE STDBENI             TO AM-STD-AH-TYPE.
02241
02242      IF INDGRPL GREATER ZEROS
02243         MOVE INDGRPI             TO AM-IG
02244         INSPECT AM-IG CONVERTING 'IG' TO '12'.
02245
02246      IF STATUSL GREATER ZEROS
02247         MOVE STATUSI             TO AM-STATUS
022808*       INSPECT AM-STATUS CONVERTING 'SFCTIA' TO '543210'
021916        INSPECT AM-STATUS CONVERTING 'PRLDSFCTIA' TO '9876543210'
102004     END-IF
02249
100703     IF CLPTOLPL GREATER ZEROS
100703        MOVE CLPTOLPI            TO AM-CLP-TOL-PCT
100703     END-IF.
100703
070109     IF PRODCDL > 0
070109        MOVE PRODCDI             TO AM-DCC-PRODUCT-CODE
070109     END-IF
041910     IF CLPSTL > 0
041910        MOVE CLPSTI              TO AM-DCC-CLP-STATE
041910     END-IF
           IF MAXMFEEL > ZEROS
              MOVE WS-WORK-NUM         TO AM-DCC-MAX-MARKETING-FEE
           END-IF
092705     IF LCOMML > ZEROS
092705        MOVE LCOMMI              TO AM-SPP-LEASE-COMM
092705     END-IF
092705
02250      IF REMITL GREATER ZEROS
02251         MOVE REMITI              TO AM-REMIT-TO.
02252      IF CONTRL GREATER ZEROS
02253         MOVE WS-ANVR-DT          TO AM-ANNIVERSARY-DATE.
02254 *    IF PSILFFL GREATER +0
02255 *       MOVE WS-LIFE-PSI         TO AM-LF-PSI-FACTOR.
02256 *    IF PSIAHFL GREATER +0
02257 *       MOVE WS-AH-PSI           TO AM-AH-PSI-FACTOR.
02258      IF PEFFDTEL GREATER ZEROS
02259         MOVE WS-PEFF-DT          TO AM-PREV-EFF-DT.
02260      IF PEXPDTEL GREATER ZEROS
02261         MOVE WS-PEXP-DT          TO AM-PREV-EXP-DT.
02262      IF COMMCALL GREATER ZEROS
02263         MOVE COMMCALI            TO AM-RECALC-COMM
02264         MOVE '*'                 TO AM-COMM-CHANGE-STATUS
02265      ELSE
02266          IF MAINTI = 'A'
02267              MOVE 'N'            TO AM-RECALC-COMM.
02268
02269      IF REINCALL GREATER ZEROS
02270         MOVE REINCALI            TO AM-RECALC-REIN
02271      ELSE
02272          IF MAINTI = 'A'
02273              MOVE 'N'            TO AM-RECALC-REIN.
02274
02275      IF REINTABL GREATER ZEROS
02276         MOVE REINTABI            TO AM-REI-TABLE.
02277
031607     IF PI-MAINT = 'A'
031607        MOVE 'P'                 TO AM-RET-ST-TAX-USE
031607     END-IF
031607
02278      MOVE 1                      TO SUB.
02279      SET M-INDEX                 TO 1.
CIDMOD
CIDMOD     
      * EXEC CICS
CIDMOD*         ASKTIME
CIDMOD*    END-EXEC.
      *    MOVE '0"                    "   #00008266' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02280
02281  5000-LOOP.
062707*    IF PI-COMPANY-ID = 'DCC'
100703*       IF (M-INDEX = +5)
100703*          AND ((AGENTL (5) > ZEROS)
100703*              OR (ATYPEL (5) > ZEROS))
100703*             MOVE ER-3057         TO EMI-ERROR
100703*             MOVE -1              TO AGENTL (5)
100703*             MOVE AL-UABON        TO AGENTA (5)
100703*             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100703*             GO TO 5000-EXIT
      *       END-IF
062707*    END-IF
02282      IF MAINTI = 'C'
02283         IF ((AM-AGT (SUB) NOT = ZEROS AND AGENT (M-INDEX)) AND
02284            (COMMCALI NOT = 'Y' AND '1')                    AND
02285             AM-HI-CERT-DATE NOT = ZEROS)                    OR
02286            ((AM-AGT (SUB) = ZEROS AND
02287              (AGENT (M-INDEX)) NOT = LOW-VALUES AND SPACES AND
02288                                      ZEROS) AND
02289            (COMMCALI NOT = 'Y' AND '1')  AND
02290             AM-HI-CERT-DATE NOT = ZEROS)
02291               MOVE ER-2181         TO EMI-ERROR
02292               MOVE -1              TO COMMCALL
02293               MOVE AL-UABON        TO COMMCALA
02294               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02295               GO TO 5000-EXIT.
02296
02297      IF MAINTI = 'C'
02298         IF ((AM-COM-TYP (SUB) NOT = SPACES AND
02299              ATYPE (M-INDEX)) AND
02300            (COMMCALI NOT = 'Y' AND '1') AND
02301             AM-HI-CERT-DATE NOT = ZEROS)
02302             OR
02303            ((AM-COM-TYP (SUB) = SPACES AND
02304              (ATYPE (M-INDEX)) NOT = LOW-VALUES AND SPACES) AND
02305            (COMMCALI NOT = 'Y' AND '1')  AND
02306             AM-HI-CERT-DATE NOT = ZEROS)
02307               MOVE ER-2181         TO EMI-ERROR
02308               MOVE -1              TO COMMCALL
02309               MOVE AL-UABON        TO COMMCALA
02310               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02311               GO TO 5000-EXIT.
02312
02313      IF AM-ACCOUNT-BILLED
02314         IF COMMCALI = 'Y' OR '1'
02315             MOVE ER-2183         TO EMI-ERROR
02316             MOVE -1              TO COMMCALL
02317             MOVE AL-UABON        TO COMMCALA
02318             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02319             GO TO 5000-EXIT.
02320
02321      IF (AGENTL (M-INDEX) NOT GREATER ZEROS AND
02322         ATYPEL  (M-INDEX) NOT GREATER ZEROS)
02323         OR
02324         ((AGENT (M-INDEX) = ZEROS OR SPACES) AND
02325          ATYPE  (M-INDEX) = SPACES)
02326            MOVE ZEROS              TO AM-AGT   (SUB)
02327                                       AM-L-COM (SUB)
02328                                       AM-J-COM (SUB)
02329                                       AM-A-COM (SUB)
02330            MOVE SPACE TO AM-COM-TYP (SUB)
02331            SET M-INDEX UP BY 1
02332            ADD 1 TO SUB
02333            IF SUB = 11
02334               GO TO 5000-CHECK-MAX-FEES
02335             ELSE
02336               GO TO 5000-LOOP.
02337
02338      MOVE AGENT (M-INDEX)        TO AM-AGT (SUB).
02339      MOVE ATYPE (M-INDEX)        TO AM-COM-TYP (SUB).
02340
02341      IF SINGLEL (M-INDEX) GREATER ZEROS
02342         MOVE '*'                 TO AM-COMM-CHANGE-STATUS
02343         IF SINGLE-COMM-R (M-INDEX) NOT NUMERIC
02344            MOVE SINGLE-COMM-T (M-INDEX) TO AM-L-COMA (SUB)
02345           ELSE
02346            MOVE SINGLE-COMM-R (M-INDEX) TO AM-L-COM  (SUB)
02347            IF SINGLE-DASH (M-INDEX) = '-'
02348                MULTIPLY AM-L-COM (SUB) BY -1
02349                      GIVING AM-L-COM (SUB).
02350
02351      IF JOINTL (M-INDEX) GREATER ZEROS
02352         MOVE '*'                 TO AM-COMM-CHANGE-STATUS
02353         IF JOINT-COMM-R (M-INDEX) NOT NUMERIC
02354            MOVE JOINT-COMM-T (M-INDEX)  TO AM-J-COMA (SUB)
02355           ELSE
02356            MOVE JOINT-COMM-R (M-INDEX)  TO AM-J-COM (SUB)
02357            IF JOINT-DASH (M-INDEX) = '-'
02358                MULTIPLY AM-J-COM (SUB) BY -1
02359                      GIVING AM-J-COM (SUB).
02360
02361      IF A-HL (M-INDEX) GREATER ZEROS
02362         MOVE '*'                 TO AM-COMM-CHANGE-STATUS
02363         IF A-H-COMM-R (M-INDEX) NOT NUMERIC
02364            MOVE A-H-COMM-T (M-INDEX)  TO AM-A-COMA (SUB)
02365           ELSE
02366            MOVE A-H-COMM-R (M-INDEX)  TO AM-A-COM (SUB)
02367            IF A-H-DASH (M-INDEX) = '-'
02368                MULTIPLY AM-A-COM (SUB) BY -1
02369                      GIVING AM-A-COM (SUB).
02370
02371      IF RECALL (M-INDEX) GREATER ZEROS
02372          MOVE RECAL  (M-INDEX)   TO AM-RECALC-LV-INDIC (SUB).
02373
02374      IF RCOMML (M-INDEX) GREATER ZEROS
02375          MOVE RCOMM  (M-INDEX)   TO AM-RETRO-LV-INDIC (SUB).
02376
02377      IF CHGBCKL (M-INDEX) GREATER ZEROS
02378          MOVE CHGBCK (M-INDEX)   TO AM-COMM-CHARGEBACK (SUB).
02379
102717     IF CCEL (M-INDEX) GREATER ZEROS
102717         MOVE CCEIND (M-INDEX)   TO AM-GL-CODES (SUB).
02382
02383      SET M-INDEX UP BY 1.
02384      ADD 1 TO SUB.
02385      IF M-INDEX NOT = 11
02386         GO TO 5000-LOOP.
02387
       5000-CHECK-MAX-FEES.
           IF (AM-DCC-PRODUCT-CODE = 'DDF')
              MOVE +1                     TO S1
              PERFORM UNTIL S1 > +10
                 IF (AM-A-COM (S1) NUMERIC)
                    AND (AM-A-COMA (S1) (3:1) NOT = 'L' AND 'M'
                           AND 'O')
                    AND (AM-COM-TYP (S1) = 'J')
                       COMPUTE COMM-MFEE-ACCUM = COMM-MFEE-ACCUM
                          + (AM-A-COM (S1) * +1000)
                 END-IF
                 ADD +1             TO S1
              END-PERFORM
              IF COMM-MFEE-ACCUM > AM-DCC-MAX-MARKETING-FEE
                 MOVE -1               TO MAXMFEEL
                 MOVE AL-UABON         TO MAXMFEEA
                 MOVE ER-1817          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           .
02388  5000-EXIT.
02389       EXIT.
02390  EJECT
02391
02392  5100-WRITE-TS.
02393      PERFORM 6500-READ-ACCT THRU 6599-EXIT.
02394
02395      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
02396          MOVE ZEROS              TO PI-CR-CARRIER
02397      ELSE
02398          MOVE AM-CARRIER         TO PI-CR-CARRIER.
02399
02400      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
02401          MOVE ZEROS              TO PI-CR-GROUPING
02402      ELSE
02403          MOVE AM-GROUPING        TO PI-CR-GROUPING.
02404
02405      IF AM-REMIT-TO NOT = ZERO
02406          MOVE AM-AGT (AM-REMIT-TO)
02407                                  TO PI-CR-FIN-RESP
02408        ELSE
02409          MOVE AM-ACCOUNT         TO PI-CR-FIN-RESP.
02410
02411      MOVE 'A'                    TO PI-CR-TYPE.
02412  5110-AGT-LOOP.
02413      ADD 1                       TO  AGT-SUB.
02414      IF AGT-SUB GREATER 10
02415          MOVE LOW-VALUES         TO PI-CR-CONTROL-IN-PROGRESS
02416          GO TO 5120-WRITE.
02417
02418 ************************************************************
02419 **  MODIFICATION MADE TO ADD SERVICE FEES                 **
02420 ************************************************************
02421
02422      IF AM-COM-TYP (AGT-SUB) = 'C' OR 'D' OR 'R'
052814                  OR 'F' OR 'U'
02424          MOVE AM-AGT (AGT-SUB)   TO  PI-CR-ACCOUNT
02425          GO TO 5120-WRITE
02426      ELSE
02427          GO TO 5110-AGT-LOOP.
02428
02429  5120-WRITE.
02430      
      * EXEC CICS WRITEQ TS
02431 *         QUEUE    (QID)
02432 *         FROM     (PROGRAM-INTERFACE-BLOCK)
02433 *         LENGTH   (WS-COMM-LENGTH)
02434 *         ITEM     (W-ONE)
02435 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00008451' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 W-ONE, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02436
02437  5199-EXIT.
02438      EXIT.
02439
02440  EJECT
02441
02442  5200-RECOVER-TS.
02443
02444      IF PI-CALLING-PROGRAM NOT = XCTL-652 AND
02445                                  XCTL-653 AND
02446                                  XCTL-689 AND
02447                                  XCTL-608 AND
02448                                  XCTL-690
02449          GO TO 5299-EXIT.
02450
02451      
      * EXEC CICS HANDLE CONDITION
02452 *        QIDERR  (5250-QID-ERROR)
02453 *    END-EXEC.
      *    MOVE '"$N                   ! , #00008472' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303038343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02454
02455      
      * EXEC CICS READQ TS
02456 *        QUEUE    (QID)
02457 *        INTO     (PROGRAM-INTERFACE-BLOCK)
02458 *        LENGTH   (WS-COMM-LENGTH)
02459 *        ITEM     (W-ONE)
02460 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00008476' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 W-ONE, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02461
02462  5200-DELETE-TS.
02463
02464      
      * EXEC CICS DELETEQ TS
02465 *        QUEUE    (QID)
02466 *    END-EXEC.
      *    MOVE '*&                    #   #00008485' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02467
02468      GO TO 5299-EXIT.
02469
02470  5250-QID-ERROR.
02471      MOVE ER-0033                TO  EMI-ERROR.
02472      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02473
02474  5299-EXIT.
02475      EXIT.
02476
02477      EJECT
02478
02479  5300-UPDATE-REQUEST-FILE.
02480      MOVE +0                     TO SUB1.
02481
02482  5300-FIND-ACCOUNT-AGENT.
02483      ADD +1                      TO SUB1.
02484
02485      IF SUB1 GREATER THAN +10
02486         MOVE ER-2949             TO EMI-ERROR
02487         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02488         MOVE -1                  TO AGENTL (1)
02489         
      * EXEC CICS SYNCPOINT ROLLBACK
02490 *       END-EXEC
      *    MOVE '6"R                   !   #00008510' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02491         GO TO 8200-SEND-DATAONLY.
02492
02493      MOVE SAVE-ACCT-AGENT      TO SAVE-NEW-ACCT-AGENT.
02494
02495 ************************************************************
02496 **  MODIFICATION MADE TO ADD SERVICE FEES                 **
02497 ************************************************************
02498
052814     IF AM-COM-TYP  (SUB1) = 'C' OR 'D' OR 'F'
02500         IF AM-AGT   (SUB1) NOT = SAVE-ACCT-AGENT
02501            MOVE AM-AGT (SUB1)  TO SAVE-NEW-ACCT-AGENT.
02502
02503      IF AM-AGT (AM-REMIT-TO) NOT = SAVE-FIN-RESP
02504         GO TO 5325-PROCESS-RQST-FILE.
02505
02506      IF SAVE-ACCT-AGENT NOT = SAVE-NEW-ACCT-AGENT
02507         GO TO 5325-PROCESS-RQST-FILE.
02508
02509      GO TO 5399-EXIT.
02510
02511  5325-PROCESS-RQST-FILE.
02512      MOVE LOW-VALUES             TO RQST3-KEY.
02513      MOVE AM-COMPANY-CD          TO RQST3-COMPANY-CD.
02514      MOVE AM-CARRIER             TO RQST3-CARRIER.
02515      MOVE AM-GROUPING            TO RQST3-GROUPING.
02516      MOVE SAVE-FIN-RESP          TO RQST3-FIN-RESP.
02517      MOVE SAVE-ACCT-AGENT        TO RQST3-ACCT-AGENT.
02518
02519      
      * EXEC CICS HANDLE CONDITION
02520 *        NOTFND(5399-EXIT)
02521 *        ENDFILE(5399-EXIT)
02522 *    END-EXEC.
      *    MOVE '"$I''                  ! - #00008540' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303038353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02523
02524  5325-PROCESS-REQUEST-FILE.
02525      
      * EXEC CICS STARTBR
02526 *         DATASET   (RQST3-FILE-ID)
02527 *         RIDFLD    (RQST3-KEY)
02528 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008546' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST3-FILE-ID, 
                 RQST3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02529
02530  5330-READNEXT-REQUEST.
02531      
      * EXEC CICS READNEXT
02532 *        SET    (ADDRESS OF AR-REQUEST-RECORD)
02533 *        DATASET(RQST3-FILE-ID)
02534 *        RIDFLD (RQST3-KEY)
02535 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008552' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST3-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 RQST3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02536
02537      IF RQST3-KEY = SAVE-RQST3-KEY
02538         GO TO 5330-READNEXT-REQUEST.
02539
02540      MOVE RQST3-KEY              TO SAVE-RQST3-KEY.
02541
02542      IF RQ-COMPANY-CD-A2 NOT = PI-COMPANY-CD
02543         GO TO 5350-REQUESTS-FINISHED.
02544
02545      IF RQ-FIN-RESP-A2 NOT = SAVE-FIN-RESP
02546         GO TO 5350-REQUESTS-FINISHED.
02547
02548      IF RQ-ACCT-AGENT-A2 NOT = SAVE-ACCT-AGENT
02549         GO TO 5350-REQUESTS-FINISHED.
02550
02551      MOVE RQ-CONTROL-PRIMARY     TO RQST-KEY.
02552
02553      
      * EXEC CICS ENDBR
02554 *        DATASET(RQST3-FILE-ID)
02555 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008574' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST3-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02556
02557      
      * EXEC CICS READ
02558 *        SET    (ADDRESS OF AR-REQUEST-RECORD)
02559 *        DATASET(RQST-FILE-ID)
02560 *        RIDFLD (RQST-KEY)
02561 *        UPDATE
02562 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008578' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 RQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02563
02564      MOVE AM-AGT (AM-REMIT-TO)   TO RQ-FIN-RESP-A2.
02565      MOVE SAVE-NEW-ACCT-AGENT    TO RQ-ACCT-AGENT-A2.
02566
02567      
      * EXEC CICS REWRITE
02568 *        DATASET   (RQST-FILE-ID)
02569 *        FROM      (AR-REQUEST-RECORD)
02570 *    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008588' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02571
02572      GO TO  5325-PROCESS-REQUEST-FILE.
02573
02574  5350-REQUESTS-FINISHED.
02575      
      * EXEC CICS ENDBR
02576 *        DATASET(RQST3-FILE-ID)
02577 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008596' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST3-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02578
02579  5399-EXIT.
02580      EXIT.
02581
02582
02583  EJECT
02584
02585  6000-MOVE-TO-SCREEN.
02586      PERFORM 8000-STATE-REC-READ   THRU 8010-EXIT.
02587
02588      MOVE AM-LAST-MAINT-USER     TO MAINTBYO.
02589      MOVE AM-NAME                TO NAMEO.
CIDMOD     MOVE AM-CONTROL-NAME        TO PCONTO.
02590      MOVE AM-CSR-CODE            TO CSRO.
02591      MOVE AM-PERSON              TO INCAREO.
02592      MOVE AM-ADDRS               TO ADDR1O.
02593      MOVE AM-ID-NO               TO TAXNOO.
02594      MOVE AM-TEL-NO              TO WS-PHONE.
02595      MOVE WS-PHONE-NUM           TO PHONEO.
02596      INSPECT PHONEI CONVERTING SPACES TO '-'.
02597      MOVE AM-ADDR-CITY           TO ACITYO
           MOVE AM-ADDR-STATE          TO ASTATEO
02598
02599      MOVE SPACES                 TO WS-ZIP-CODE.
02600      IF AM-CANADIAN-POST-CODE
02601          MOVE AM-CAN-POSTAL-1    TO WS-ZIP-CAN-2-POST1
02602          MOVE AM-CAN-POSTAL-2    TO WS-ZIP-CAN-2-POST2
02603      ELSE
02604          MOVE AM-ZIP-PRIME       TO WS-ZIP-AM-2-CODE
02605          IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
02606              MOVE '-'            TO WS-ZIP-AM-2-DASH
02607              MOVE AM-ZIP-PLUS4   TO WS-ZIP-AM-2-PLUS4.
02608
02609      MOVE WS-ZIP-CODE            TO ZIPO.
02610
02611      MOVE AM-GPCD                TO TYPEO.
02612      MOVE AM-STD-AH-TYPE         TO STDBENO.
02613
02614      MOVE AM-IG                  TO INDGRPO.
02615      INSPECT INDGRPO CONVERTING '12' TO 'IG'.
02616
02617      MOVE AM-STATUS              TO STATUSO.
022808*    INSPECT STATUSO CONVERTING '543210' TO 'SFCTIA'
021916     INSPECT STATUSO CONVERTING '9876543210' TO 'PRLDSFCTIA'
02619
02620      MOVE AM-REMIT-TO            TO REMITO.
02621
02622      IF PI-AR-PROCESSING
02623         IF AM-AR-HI-CERT-DATE = LOW-VALUES
02624            MOVE AL-UNNON         TO REMITA
02625         ELSE
02626            MOVE AL-SANON         TO REMITA
02627      ELSE
02628         MOVE AL-UNNON            TO REMITA.
02629
02630 ***  Y2K PROJ 7744
02631      MOVE AM-ANNIVERSARY-DATE    TO DC-GREG-DATE-1-YMD.
02632      MOVE '3'                    TO DC-OPTION-CODE.
02633      PERFORM 9700-DATE-LINK.
02634      IF DATE-CONVERSION-ERROR
02635         MOVE SPACES              TO CONTRO
02636      ELSE
02637         MOVE DC-GREG-DATE-1-EDIT TO CONTRO
02638      END-IF.
02639 ***  Y2K PROJ 7744
02640
02641 *    MOVE PI-LIFE-OVERRIDE-L6    TO PSILFTO
02642 *    MOVE PI-AH-OVERRIDE-L6      TO PSIAHTO.
02643
02644 *    IF AM-LF-PSI-FACTOR NUMERIC
02645 *       MOVE AM-LF-PSI-FACTOR    TO PSILFFO.
02646 *
02647 *    IF AM-AH-PSI-FACTOR NUMERIC
02648 *       MOVE AM-AH-PSI-FACTOR    TO PSIAHFO.
02649
02650      MOVE AM-RECALC-COMM         TO COMMCALO.
02651      MOVE AL-UANON               TO COMMCALA.
02652      MOVE AM-RECALC-REIN         TO REINCALO.
02653      MOVE AM-REI-TABLE           TO REINTABO.
02654      MOVE AM-1ST-PROD-DATE       TO DC-GREG-DATE-1-YMD.
02655      MOVE '3'                    TO DC-OPTION-CODE.
02656      PERFORM 9700-DATE-LINK.
02657      IF DATE-CONVERSION-ERROR
02658         MOVE SPACES              TO PRODATEO
02659      ELSE
02660         MOVE DC-GREG-DATE-1-EDIT TO PRODATEO.
02661
020816     IF PI-COMPANY-ID = 'DCC' or 'VPP'
              IF AM-CLP-TOL-PCT NOT NUMERIC
                 MOVE ZEROS            TO AM-CLP-TOL-PCT
              END-IF
              IF AM-SPP-LEASE-COMM NOT NUMERIC
                 MOVE ZEROS            TO AM-SPP-LEASE-COMM
              END-IF
100703        MOVE AM-CLP-TOL-PCT      TO CLPTOLPO
092705        MOVE AM-SPP-LEASE-COMM   TO LCOMMO
              MOVE AM-DCC-PRODUCT-CODE TO PRODCDO
041910        MOVE AM-DCC-CLP-STATE    TO CLPSTO
              MOVE AL-UANON            TO CLPSTA
              IF AM-DCC-MAX-MARKETING-FEE NOT NUMERIC
                 MOVE ZEROS            TO AM-DCC-MAX-MARKETING-FEE
              END-IF
              MOVE AM-DCC-MAX-MARKETING-FEE
                                       TO MAXMFEEO
100703     ELSE
100703        MOVE AL-SADOF            TO CTPLABLA
100703                                    CLPTOLPA
092705                                    LCLABLA
092705                                    LCOMMA
100703     END-IF
100703
02662      IF AM-RET-Y-N = ' '  OR  'N'
02663          MOVE 'NO '              TO RETROCDO
02664      ELSE
02665          MOVE 'YES'              TO RETROCDO.
02666
02667      IF AM-ACCOUNT-BILLED
02668          MOVE 'YES'              TO BILLCDO
02669      ELSE
02670          MOVE 'NO '              TO BILLCDO.
02671
02672      MOVE AL-UANON               TO RETROCDA.
02673      MOVE AL-SANON               TO PRODATEA
02674                                     BILLCDA.
02675      MOVE AM-PREV-EFF-DT TO DC-GREG-DATE-1-YMD.
02676      MOVE '3' TO DC-OPTION-CODE.
02677      PERFORM 9700-DATE-LINK.
02678      IF DATE-CONVERSION-ERROR
02679         MOVE SPACES TO PEFFDTEO
02680      ELSE
02681         MOVE DC-GREG-DATE-1-EDIT TO PEFFDTEO.
02682
02683      IF AM-PREV-EXP-DT NOT = 99999999999
02684         MOVE AM-PREV-EXP-DT TO DC-GREG-DATE-1-YMD
02685         MOVE '3' TO DC-OPTION-CODE
02686         PERFORM 9700-DATE-LINK
02687         IF DATE-CONVERSION-ERROR
02688            MOVE SPACES TO PEXPDTEO
02689         ELSE
02690            MOVE DC-GREG-DATE-1-EDIT TO PEXPDTEO
02691      ELSE
02692         MOVE '99/99/99' TO PEXPDTEO.
02693
02694      IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'
02695         MOVE AL-UANON            TO PEXPDTEA
02696                                     PEFFDTEA
02697         MOVE AL-SANOF TO            PDSPLYA.
02698
02699      MOVE LOW-VALUES             TO MAP-AGENT-AREA.
02700      MOVE PI-COMM-POINTER        TO LCP-WS-ADDR-COMP
02701      SET ADDRESS OF COMMISSION-WORK-AREA TO LCP-WS-ADDR-PNTR.
02702
02703      MOVE 1  TO SUB.
02704      SET M-INDEX TO 1.
02705
02706  6005-LOOP.
02707      IF AM-AGT (SUB) = SPACES  OR ZEROS
02708         IF PI-AR-PROCESSING
02709            IF AM-AR-HI-CERT-DATE GREATER THAN LOW-VALUES
02710               MOVE AL-SANOF     TO AGENTA  (M-INDEX)
02711                                    ATYPEA (M-INDEX)
02712                                    SINGLEA (M-INDEX)
02713                                    JOINTA (M-INDEX)
02714                                    A-HA    (M-INDEX)
02715               GO TO 6010-BUMP-SUB
02716            ELSE
02717               GO TO 6010-BUMP-SUB
02718         ELSE
02719            GO TO 6010-BUMP-SUB.
02720
02721      MOVE AM-AGT (SUB)           TO AGENT (M-INDEX).
02722
02723 ***DMD CUSTOM CODE - CAPTURE THE FIRST AGENT TO BE DISPLAYED
02724      IF WS-DMD-AGENT = LOW-VALUES
02725         MOVE AM-AGT (SUB)        TO WS-DMD-AGENT.
02726 ***DMD ******************************************************
02727
02728      IF PI-GA-BILLING
02729         MOVE AM-AGT (SUB)        TO WK-AM-AGT (SUB).
02730      MOVE AL-UANON               TO AGENTA (M-INDEX).
02731
02732      MOVE AM-COM-TYP (SUB)       TO ATYPE (M-INDEX).
02733      IF PI-GA-BILLING
02734         MOVE AM-COM-TYP (SUB)    TO WK-AM-TYPE (SUB).
02735      MOVE AL-UANON               TO ATYPEA (M-INDEX).
02736
pemuni     IF (AM-L-COM (SUB) NOT NUMERIC)
PEMUNI        OR (AM-L-COMA (SUB) (3:1) = 'L' OR 'M' OR 'O')
02738         MOVE AM-L-COMA (SUB)     TO SINGLE-COMM-T (M-INDEX)
02739      ELSE
02740         IF AM-L-COM (SUB) NOT = ZEROS
02741            MOVE AM-L-COM  (SUB)  TO SINGLE-COMM-O (M-INDEX).
102717     move al-uanon to singlea (m-index)
02742
02743      IF (AM-J-COM (SUB) NOT NUMERIC)
PEMUNI        OR (AM-J-COMA (SUB) (3:1) = 'L' OR 'M' OR 'O')
02744         MOVE AM-J-COMA (SUB)     TO JOINT-COMM-T (M-INDEX)
02745      ELSE
02746         IF AM-J-COM (SUB) NOT = ZEROS
02747            MOVE AM-J-COM  (SUB)  TO JOINT-COMM-O (M-INDEX).
102717     move al-uanon to jointa (m-index)
02748
02749      IF (AM-A-COM (SUB) NOT NUMERIC)
PEMUNI        OR (AM-A-COMA (SUB) (3:1) = 'L' OR 'M' OR 'O')
02750         MOVE AM-A-COMA (SUB)     TO A-H-COMM-T (M-INDEX)
02751      ELSE
02752         IF AM-A-COM (SUB) NOT = ZEROS
02753            MOVE AM-A-COM  (SUB)  TO A-H-COMM-O (M-INDEX).
102717     move al-uanon to a-ha (m-index)
02754
02755 ************************************************************
02756 **  MODIFICATION MADE TO ADD SERVICE FEES                 **
02757 ************************************************************
02758
02759      IF (AM-COM-TYP (SUB) NOT = 'C' AND 'D' AND 'O' AND 'P'
100703            AND 'F' AND 'S' AND 'G' AND 'B' AND 'I'
011410            AND 'K' AND 'L' AND 'J' AND 'M' AND 'A' AND 'N')
02761          GO TO 6006-CONTINUE.
           .
02772  6006-CONTINUE.
           IF WS-ST-COMM-CAP-SL NOT NUMERIC
              MOVE ZEROS               TO WS-ST-COMM-CAP-SL
           end-if
           IF WS-ST-COMM-CAP-JL NOT NUMERIC
              MOVE ZEROS               TO WS-ST-COMM-CAP-JL
           end-if
           IF WS-ST-COMM-CAP-SA NOT NUMERIC
              MOVE ZEROS               TO WS-ST-COMM-CAP-SA
           end-if
           IF (AM-L-COM (SUB) NUMERIC)
              AND (AM-L-COMA (SUB) (3:1) NOT = 'L' AND 'M' AND 'O')
              if am-gl-codes(sub) <> 'Y'
                 ADD AM-L-COM (SUB)    TO COMM-SL-ACCUM
                 if am-com-typ (sub) = 'C' or 'D'
                    ADD AM-L-COM (SUB) TO COMM-SL-ACCUM-al
                 else
                    if am-com-typ (sub) = 'O' or 'P'
                       add am-l-com (sub) to comm-sl-accum-gl
                    end-if
                 end-if
              end-if
           ELSE
              MOVE PI-COMPANY-CD    TO COMM-COMP-CD
              MOVE AM-L-COMA (SUB)  TO COMM-TABLE
              MOVE PI-LIFE-OVERRIDE-L1
                                    TO COMM-LF-AH
              MOVE LOW-VALUES       TO COMM-FILLER
              PERFORM 6400-READ-COMMISSION
                                    THRU 6459-EXIT
              if am-gl-codes(sub) <> 'Y'
                 add ws-table-max         to COMM-SL-ACCUM
                 if atype (m-index) = 'C' or 'D'
                    add ws-table-max to comm-sl-accum-al
                 else
                    if atype (m-index) = 'O' or 'P'
                       add ws-table-max to comm-sl-accum-gl
                    end-if
                 end-if
              end-if
           END-IF
           IF (AM-J-COM (SUB) NUMERIC)
              AND (AM-J-COMA (SUB) (3:1) NOT = 'L' AND 'M' AND 'O')
              if am-gl-codes(sub) <> 'Y'
                 ADD AM-J-COM (SUB)    TO COMM-JL-ACCUM
                 if am-com-typ (sub) = 'C' or 'D'
                    ADD AM-J-COM (SUB) TO COMM-JL-ACCUM-al
                 else
                    if am-com-typ (sub) = 'O' or 'P'
                       add am-J-com (sub) to comm-Jl-accum-gl
                    end-if
                 end-if
              end-if
           ELSE
              MOVE PI-COMPANY-CD    TO COMM-COMP-CD
              MOVE AM-J-COMA (SUB)  TO COMM-TABLE
              MOVE PI-LIFE-OVERRIDE-L1
                                    TO COMM-LF-AH
              MOVE LOW-VALUES       TO COMM-FILLER
              PERFORM 6400-READ-COMMISSION
                                    THRU 6459-EXIT
              if am-gl-codes(sub) <> 'Y'
                 add ws-table-max to COMM-JL-ACCUM
                 if atype (m-index) = 'C' or 'D'
                    add ws-table-max to comm-jl-accum-al
                 else
                    if atype (m-index) = 'O' or 'P'
                       add ws-table-max to comm-jl-accum-gl
                    end-if
                 end-if
              end-if
           END-IF
           IF (AM-A-COM (SUB) NUMERIC)
              AND (AM-A-COMA (SUB) (3:1) NOT = 'L' AND 'M' AND 'O')
              if am-gl-codes(sub) <> 'Y'
                 ADD AM-A-COM (SUB)    TO COMM-AH-ACCUM
                 if am-com-typ (sub) = 'C' or 'D'
                    ADD AM-A-COM (SUB) TO COMM-AH-ACCUM-al
                 else
                    if am-com-typ (sub) = 'O' or 'P'
                       add am-a-com (sub) to comm-AH-accum-gl
                    end-if
                 end-if
              end-if
           ELSE
              MOVE PI-COMPANY-CD    TO COMM-COMP-CD
              MOVE AM-A-COMA (SUB)  TO COMM-TABLE
              MOVE PI-AH-OVERRIDE-L1
                                    TO COMM-LF-AH
              MOVE LOW-VALUES       TO COMM-FILLER
              PERFORM 6400-READ-COMMISSION
                                    THRU 6459-EXIT
              if am-gl-codes(sub) <> 'Y'
                 add ws-table-max to COMM-AH-ACCUM
                 if atype (m-index) = 'C' or 'D'
                    add ws-table-max to comm-AH-accum-al
                 else
                    if atype (m-index) = 'O' or 'P'
                       add ws-table-max to comm-ah-accum-gl
                    end-if
                 end-if
              end-if
           END-IF
02804
02807
02821      IF AM-RECALC-LV-INDIC (SUB) NOT = SPACE
02822          MOVE AM-RECALC-LV-INDIC (SUB) TO RECAL (M-INDEX).
02823
02824      IF AM-RETRO-LV-INDIC (SUB) NOT = SPACE
02825          MOVE AM-RETRO-LV-INDIC (SUB) TO RCOMM (M-INDEX).
02826
02827      IF AM-COMM-CHARGEBACK (SUB)  NUMERIC
02828         NEXT SENTENCE
02829      ELSE
02830         MOVE ZEROS              TO  AM-COMM-CHARGEBACK (SUB).
02831
02832      IF AM-COMM-CHARGEBACK (SUB)   NOT  =    ZEROS
02833          MOVE AM-COMM-CHARGEBACK (SUB) TO CHGBCK (M-INDEX).
102717     IF AM-GL-CODES (SUB) = 'Y'
102717        MOVE AM-GL-CODES (SUB)   TO CCEIND (M-INDEX)
              MOVE AL-UANON            TO CCEA   (M-INDEX)
102717     ELSE
102717        MOVE 'N'                 TO CCEIND (M-INDEX)
102717     END-IF
           if am-gl-codes(sub) = 'Y'
              if ((am-com-typ(sub) = 'C' OR 'D')
                 and (WS-COMM-CAP-LIMIT-TO not = 'A' AND 'B'))
                                  or
                 ((am-com-typ(sub) = 'O' or 'P')
                 and (ws-comm-cap-limit-to not = 'G' AND 'B'))
                 move er-3065       to emi-error
                 move -1            to maintl
                 perform 9900-error-format
                                       thru 9900-exit
              end-if
           end-if
02835      IF AM-GL-CODES (SUB)   NOT  =    ZEROS
102717         MOVE AM-GL-CODES (SUB)  TO  CCEIND (M-INDEX).
02837
02838      IF NOT PI-AR-PROCESSING
02839         GO TO 6010-BUMP-SUB.
02840
02841      IF AM-AR-HI-CERT-DATE = LOW-VALUES
02842         GO TO 6010-BUMP-SUB.
02843
02844      MOVE AL-SANON              TO AGENTA  (M-INDEX)
02845                                    ATYPEA  (M-INDEX)
02846                                    SINGLEA (M-INDEX)
02847                                    JOINTA  (M-INDEX)
02848                                    A-HA    (M-INDEX).
02849
02850  6010-BUMP-SUB.
02851      ADD 1  TO SUB.
02852      SET M-INDEX UP BY 1.
02853      IF SUB LESS 11
02854         GO TO 6005-LOOP.
102717     if ws-st-comm-cap-sl = zeros
102717        continue
102717     else
102717        if comm-sl-accum-al > ws-st-comm-cap-sl
102717           MOVE -1               TO MAINTL
102717           MOVE ER-1955          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717        end-if
102717     end-if
102717
102717     if ws-st-comm-cap-jl = zeros
102717        continue
102717     else
102717        if comm-jl-accum-al > ws-st-comm-cap-jl
102717           MOVE -1               TO MAINTL
102717           MOVE ER-1956          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717        end-if
102717     end-if
102717
102717     if ws-st-comm-cap-sa = zeros
102717        continue
102717     else
102717        if comm-ah-accum-al > ws-st-comm-cap-sa
102717           MOVE -1               TO MAINTL
102717           MOVE ER-1957          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717        end-if
102717     end-if
102717
102717
102717     if ws-st-ga-comm-cap-sl = zeros
102717        continue
102717     else
102717        if comm-sl-accum-gl > ws-st-ga-comm-cap-sl
102717           MOVE -1               TO MAINTL
102717           MOVE ER-1958          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717        end-if
102717     end-if
102717
102717     if ws-st-ga-comm-cap-jl = zeros
102717        continue
102717     else
102717        if comm-jl-accum-gl > ws-st-ga-comm-cap-jl
102717           MOVE -1               TO MAINTL
102717           MOVE ER-1959          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717        end-if
102717     end-if
102717
102717     if ws-st-ga-comm-cap-sa = zeros
102717        continue
102717     else
102717        if comm-ah-accum-gl > ws-st-ga-comm-cap-sa
102717           MOVE -1               TO MAINTL
102717           MOVE ER-1960          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717        end-if
102717     end-if
102717
102717     if ws-st-tot-comm-cap-sl = zeros
102717        continue
102717     else
102717        if comm-sl-accum > ws-st-tot-comm-cap-sl
102717           MOVE -1                  TO MAINTL
102717           MOVE ER-1961             TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                    THRU 9900-EXIT
102717        end-if
102717     end-if
102717
102717     if ws-st-tot-comm-cap-jl = zeros
102717        continue
102717     else
102717        if comm-jl-accum > ws-st-tot-comm-cap-jl
102717           MOVE -1                  TO MAINTL
102717           MOVE ER-1962             TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                    THRU 9900-EXIT
102717        end-if
102717     end-if
102717
102717     if ws-st-tot-comm-cap-sa = zeros
102717        continue
102717     else
102717        if comm-ah-accum > ws-st-tot-comm-cap-sa
102717           MOVE -1                  TO MAINTL
102717           MOVE ER-1963             TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                    THRU 9900-EXIT
102717        end-if
102717     end-if
           .
02856  6099-EXIT.
02857       EXIT.
02858  EJECT
02859  6300-STARTBR.
02860      
      * EXEC CICS STARTBR
02861 *         DATASET   (ACCT-FILE-ID)
02862 *         RIDFLD    (WS-KEY-SAVE)
02863 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009078' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 WS-KEY-SAVE, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02864
02865      MOVE 'Y'                    TO BROWSE-STARTED-SW.
02866
02867  6300-EXIT.
02868       EXIT.
02869
02870  6310-READNEXT.
02871      
      * EXEC CICS READNEXT
02872 *         DATASET    (ACCT-FILE-ID)
02873 *         SET        (ADDRESS OF ACCOUNT-MASTER)
02874 *         RIDFLD     (WS-KEY-SAVE)
02875 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009089' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-KEY-SAVE, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02876
02877  6310-EXIT.
02878       EXIT.
02879
02880  6320-ENDBR.
02881      MOVE SPACE               TO BROWSE-STARTED-SW.
02882      
      * EXEC CICS ENDBR
02883 *        DATASET(ACCT-FILE-ID)
02884 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009100' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02885
02886  6320-EXIT.
02887       EXIT.
02888  EJECT
02889  6400-READ-COMMISSION.
02890      MOVE ZEROS                  TO WS-COMM-ERROR-SW
02891      
      * EXEC CICS HANDLE CONDITION
02892 *         NOTFND   (6450-NOT-FOUND)
02893 *    END-EXEC
      *    MOVE '"$I                   ! . #00009109' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303039313039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02894
02895      
      * EXEC CICS READ
02896 *        GTEQ
02897 *        DATASET   (COMM-FILE-ID)
02898 *        SET       (ADDRESS OF COMM-TABLE-RECORD)
02899 *        RIDFLD    (COMM-KEY)
02900 *    END-EXEC
      *    MOVE '&"S        G          (   #00009113' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 COMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02901
02902      IF CT-COMPANY-CD = PI-COMPANY-CD AND
02903         CT-TABLE      = COMM-TABLE    AND
02904         CT-BEN-TYPE   = COMM-LF-AH
CIDMOD        PERFORM 6460-CHECK-MAX THRU 6460-EXIT
02905         GO TO 6459-EXIT
CIDMOD     END-IF
02906      .
02907  6450-NOT-FOUND.
02908      MOVE ER-2176                TO EMI-ERROR.
02909      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02910      MOVE 1  TO WS-COMM-ERROR-SW.
02911
02912  6459-EXIT.
02913       EXIT.
02914
CIDMOD 6460-CHECK-MAX.
CIDMOD
CIDMOD     
      * EXEC CICS STARTBR
CIDMOD*         DATASET   (COMM-FILE-ID)
CIDMOD*         RIDFLD    (COMM-KEY)
CIDMOD*         RESP      (WS-RESPONSE)
CIDMOD*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00009137' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-FILE-ID, 
                 COMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD
CIDMOD     MOVE SPACES          TO WS-STATE-MAX-SW
CIDMOD
CIDMOD     IF RESP-NORMAL
CIDMOD        MOVE LOW-VALUES          TO COMM-FILLER
CIDMOD        MOVE COMM-KEY            TO WS-HOLD-COMM-KEY
CIDMOD        MOVE ZEROS               TO WS-TABLE-MAX
CIDMOD        PERFORM UNTIL
CIDMOD              (NOT RESP-NORMAL) OR
CIDMOD              (COMM-KEY (1:5) NOT = WS-HOLD-COMM-KEY (1:5))
CIDMOD            
      * EXEC CICS READNEXT
CIDMOD*             DATASET   (COMM-FILE-ID)
CIDMOD*             SET       (ADDRESS OF COMM-TABLE-RECORD)
CIDMOD*             RIDFLD    (COMM-KEY)
CIDMOD*             RESP      (WS-RESPONSE)
CIDMOD*           END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00009152' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 COMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD            IF (RESP-NORMAL) AND
CIDMOD               (COMM-KEY (1:5) = WS-HOLD-COMM-KEY (1:5))
CIDMOD               PERFORM VARYING WS-NDX FROM +1 BY +1 UNTIL
CIDMOD                   (WS-NDX > +27)
CIDMOD                   IF CT-RT (WS-NDX) > WS-TABLE-MAX
CIDMOD                      MOVE CT-RT (WS-NDX)
CIDMOD                                 TO WS-TABLE-MAX
CIDMOD                   END-IF
CIDMOD               END-PERFORM
CIDMOD            END-IF
CIDMOD        END-PERFORM
CIDMOD        
      * EXEC CICS ENDBR
CIDMOD*            DATASET   (COMM-FILE-ID)
CIDMOD*       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009169' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD     END-IF
CIDMOD
CIDMOD     .
CIDMOD 6460-EXIT.
CIDMOD      EXIT.
CIDMOD
CIDMOD
02915  6500-READ-ACCT.
02916      
      * EXEC CICS READ
02917 *        GTEQ
02918 *        DATASET   (ACCT-FILE-ID)
02919 *        SET       (ADDRESS OF ACCOUNT-MASTER)
02920 *        RIDFLD    (PI-ACCT-KEY)
02921 *    END-EXEC.
      *    MOVE '&"S        G          (   #00009180' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02922
02923  6599-EXIT.
02924       EXIT.
02925  EJECT
02926  6600-UPDATE-MAINT-DT.
02927      MOVE SPACES                 TO ELCNTL-KEY.
02928
02929      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02930      MOVE '1'                    TO CNTL-REC-TYPE.
02931      MOVE +0                     TO CNTL-SEQ-NO.
02932
02933      
      * EXEC CICS HANDLE CONDITION
02934 *        NOTFND   (6699-EXIT)
02935 *    END-EXEC.
      *    MOVE '"$I                   ! / #00009197' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303039313937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02936
02937      
      * EXEC CICS READ
02938 *        UPDATE
02939 *        DATASET   (CNTL-FILE-ID)
02940 *        SET       (ADDRESS OF CONTROL-FILE)
02941 *        RIDFLD    (ELCNTL-KEY)
02942 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00009201' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02943
02944      MOVE BIN-CURRENT-SAVE       TO CF-ACCOUNT-MSTR-MAINT-DT.
02945
02946      
      * EXEC CICS REWRITE
02947 *        DATASET   (CNTL-FILE-ID)
02948 *        FROM      (CONTROL-FILE)
02949 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009210' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02950
02951  6699-EXIT.
02952       EXIT.
02953  EJECT
02954  6700-BUILD-COMM-WORK.
02955      IF NOT PI-GA-BILLING
02956          GO TO 6799-EXIT.
02957
02958      
      * EXEC CICS GETMAIN
02959 *         LENGTH   (260)
02960 *         SET      (ADDRESS OF COMMISSION-WORK-AREA)
02961 *         INITIMG  (GETMAIN-SPACE)
02962 *     END-EXEC.
           MOVE 260
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00009222' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMMISSION-WORK-AREA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02963      SET LCP-WS-ADDR-PNTR TO ADDRESS OF COMMISSION-WORK-AREA
02964
02965      MOVE LCP-WS-ADDR-COMP TO PI-COMM-POINTER.
02966      MOVE SPACES             TO COMMISSION-WORK-AREA.
02967      MOVE 'X'                TO COMMISSION-GETMAIN-SW.
02968
02969      IF PI-MAINT = 'A'
02970          GO TO 6799-EXIT.
02971
02972      MOVE AM-COMM-STRUCTURE TO COMMISSION-WORK-AREA.
02973      MOVE +1                TO AXRF-SUB.
02974
02975  6710-AXRF-LOOP.
02976      MOVE SPACES         TO AGENT-FIND.
02977      MOVE ERGXRF-MAX-LEN TO ERGXRF-REC-LEN.
02978
02979      PERFORM 6920-READ-AXRF-FILE THRU 6929-EXIT.
02980
02981      IF AGENT-FIND = SPACES
02982         MOVE GX-LAST-BILL-DT (GX-AGENT-SUB) TO
02983              WK-GA-BILL-DT (AXRF-SUB)
02984      ELSE
02985         MOVE LOW-VALUES TO WK-GA-BILL-DT (AXRF-SUB).
02986
02987      ADD +1 TO AXRF-SUB.
02988
02989      IF AXRF-SUB NOT = +11
02990         GO TO 6710-AXRF-LOOP.
02991
02992  6799-EXIT.
02993      EXIT.
02994  EJECT
02995  6800-COMPANY-REC-READ.
02996      MOVE SPACES                 TO ELCNTL-KEY.
02997      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02998      MOVE '1'                    TO CNTL-REC-TYPE.
02999      MOVE +0                     TO CNTL-SEQ-NO.
03000      
      * EXEC CICS HANDLE CONDITION
03001 *        NOTFND   (6880-NO-COMP)
03002 *    END-EXEC.
      *    MOVE '"$I                   ! 0 #00009264' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303039323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03003
03004      
      * EXEC CICS READ
03005 *        DATASET   (CNTL-FILE-ID)
03006 *        SET       (ADDRESS OF CONTROL-FILE)
03007 *        RIDFLD    (ELCNTL-KEY)
03008 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009268' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03009
03010      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES
03011          MOVE ER-2572               TO EMI-ERROR
03012          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03013
03014      GO TO 6899-EXIT.
03015
03016  6880-NO-COMP.
03017      MOVE ER-0002                TO EMI-ERROR.
03018      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03019
03020  6899-EXIT.
03021      EXIT.
03022  EJECT
03023  6900-READ-AND-CHECK-AXRF.
03024      IF NOT PI-GA-BILLING
03025         GO TO 6999-EXIT.
03026
03027      MOVE PI-COMM-POINTER TO LCP-WS-ADDR-COMP
03028      SET ADDRESS OF COMMISSION-WORK-AREA TO LCP-WS-ADDR-PNTR.
03029
03030  6905-AGENT-LEVEL-LOOP.
03031      MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN.
03032      MOVE SPACES TO AGENT-FIND.
03033      IF ((AM-AGT (AXRF-SUB) NOT = WK-AM-AGT (AXRF-SUB))
03034       OR
03035         (AM-COM-TYP (AXRF-SUB) NOT = WK-AM-TYPE (AXRF-SUB)))
03036       AND
03037         ((AM-COM-TYP (AXRF-SUB) = 'O' OR 'P' OR 'G'
100703                             OR 'B' OR 'I'
011410                             OR 'K' OR 'L' OR 'J' OR 'M'
052814                             OR 'S' OR 'A' OR 'N')
011410                            OR
03038         (WK-AM-TYPE (AXRF-SUB) = 'O' OR 'P' OR 'G' OR
011410                'B' OR 'I' OR 'K' OR 'L' OR 'J' OR 'M'
052814                                  OR 'S' OR 'A' OR 'N'))
03039             PERFORM 6920-READ-AXRF-FILE THRU 6929-EXIT
03040             IF AGENT-RECORD-NOT-FOUND
03041                PERFORM 6950-ADD-AGENT-RECORD THRU 6959-EXIT
03042            ELSE
03043             IF ACCT-RECORD-NOT-FOUND OR ACCT-LEVEL-DIFFERENT
03044                PERFORM 6970-REWRITE-AGENT-RECORD THRU 6979-EXIT
03045            ELSE
03046         IF (AM-COM-TYP (AXRF-SUB) NOT = 'O' AND 'P' AND 'G'
052814              AND 'B' AND 'I' AND 'S'
011410              AND 'K' AND 'L' AND 'J' AND 'M' AND 'A' AND 'N') OR
03048            (AM-AGT (AXRF-SUB) NOT =
03049                    WK-AM-AGT (AXRF-SUB))
03050            PERFORM 7000-DELETE-AXRF-REC THRU 7999-EXIT
03051            GO TO 6910-INC-SUB.
03052
03053      MOVE SPACES TO AGENT-FIND.
03054      IF (AM-AGT (AXRF-SUB) NOT = WK-AM-AGT (AXRF-SUB)) AND
03055         (WK-AM-AGT (AXRF-SUB) NOT = LOW-VALUES AND
03056                                SPACES AND ZEROS)
03057           MOVE WK-AM-AGT (AXRF-SUB)   TO WS-AXRF-AGENT-NO
03058           MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN
03059           PERFORM 6921-READ-AXRF THRU 6929-EXIT
03060           IF AGENT-FIND = SPACES
03061               PERFORM 7000-DELETE-AXRF-REC THRU 7999-EXIT.
03062
03063  6910-INC-SUB.
03064      ADD +1                      TO AXRF-SUB.
03065      IF AXRF-SUB NOT = +11
03066         GO TO 6905-AGENT-LEVEL-LOOP.
03067
03068      GO TO 6999-EXIT.
03069
03070  6920-READ-AXRF-FILE.
03071 *    IF GETMAIN-SW = 'X'
03072 *       GO TO 6920-BUILD-KEY.
03073 *
03074 *    EXEC CICS GETMAIN
03075 *         LENGTH   (ERGXRF-REC-LEN)
03076 *         SET      (ADDRESS OF AGENT-CROSS-REFERENCE)
03077 *         INITIMG  (GETMAIN-SPACE)
03078 *    END-EXEC.
03079 *
03080 *    MOVE 'X' TO GETMAIN-SW.
03081
03082  6920-BUILD-KEY.
03083      MOVE PI-COMPANY-CD          TO WS-AXRF-COMPANY-CD.
03084
03085      IF PI-ZERO-CARRIER
03086        OR PI-ZERO-CAR-GROUP
03087          MOVE ZERO               TO WS-AXRF-CARRIER
03088      ELSE
03089          MOVE AM-CARRIER         TO WS-AXRF-CARRIER.
03090
03091      IF PI-ZERO-GROUPING
03092        OR PI-ZERO-CAR-GROUP
03093          MOVE ZEROS              TO WS-AXRF-GROUPING
03094      ELSE
03095          MOVE AM-GROUPING        TO WS-AXRF-GROUPING.
03096
03097      IF AM-AGT (AXRF-SUB) NOT = SPACES AND ZEROS AND
03098                                               LOW-VALUES
03099         MOVE AM-AGT (AXRF-SUB) TO WS-AXRF-AGENT-NO
03100      ELSE
03101      IF WK-AM-AGT (AXRF-SUB) NOT = SPACES AND ZEROS AND
03102                                               LOW-VALUES
03103         MOVE WK-AM-AGT (AXRF-SUB) TO WS-AXRF-AGENT-NO
03104      ELSE
03105         MOVE 'X' TO AGENT-FIND
03106         GO TO 6929-EXIT.
03107
03108      MOVE +1 TO GX-AGENT-SUB.
03109
03110  6921-READ-AXRF.
03111      
      * EXEC CICS HANDLE CONDITION
03112 *         ENDFILE     (6925-NOT-FIND)
03113 *         NOTFND      (6925-NOT-FIND)
03114 *    END-EXEC.
      *    MOVE '"$''I                  ! 1 #00009382' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303039333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03115
03116      
      * EXEC CICS READ
03117 *        DATASET   (ERGXRF-FILE-ID)
03118 *        LENGTH    (ERGXRF-REC-LEN)
03119 *        INTO      (AGENT-CROSS-REFERENCE)
03120 *        RIDFLD    (WS-AXRF-KEY)
03121 *        EQUAL
03122 *    END-EXEC.
      *    MOVE '&"IL       E          (   #00009387' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 WS-AXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03123
03124      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
03125
03126      PERFORM 6921-FIND-ACCT-LOOP VARYING GX-AGENT-SUB
03127              FROM +1 BY +1 UNTIL
03128          (GX-AGENT-SUB GREATER THAN GX-AGENT-POINTER-CNT) OR
03129          (GX-AM-ACCOUNT (GX-AGENT-SUB) = AM-ACCOUNT      AND
03130          GX-AM-EXPIRATION-DT (GX-AGENT-SUB) = AM-EXPIRATION-DT
03131          AND
03132          GX-AM-EFF-DT (GX-AGENT-SUB)   = AM-EFFECTIVE-DT AND
03133          GX-AM-CARRIER (GX-AGENT-SUB)  = AM-CARRIER      AND
03134          GX-AM-GROUPING (GX-AGENT-SUB) = AM-GROUPING     AND
03135          GX-AM-STATE (GX-AGENT-SUB)    = AM-STATE).
03136
03137      IF GX-AGENT-SUB GREATER GX-AGENT-POINTER-CNT
03138         MOVE 'Y' TO AGENT-FIND
03139         GO TO 6929-EXIT.
03140
03141      IF GX-AM-LEVEL-NO (GX-AGENT-SUB) NOT = AXRF-SUB
03142         MOVE 'Z' TO AGENT-FIND.
03143
03144      GO TO 6929-EXIT.
03145
03146  6921-FIND-ACCT-LOOP.
03147 ***  DUMMY PARAGRAPH  ***
03148
03149  6925-NOT-FIND.
03150      MOVE 'X' TO AGENT-FIND.
03151
03152  6929-EXIT.
03153      EXIT.
03154  EJECT
03155  6950-ADD-AGENT-RECORD.
03156      IF AM-AGT (AXRF-SUB) = SPACES OR ZEROS
03157         GO TO 6959-EXIT.
03158
03159      MOVE +109                   TO ERGXRF-REC-LEN.
03160
03161      MOVE +1                     TO GX-AGENT-POINTER-CNT.
03162      MOVE 'GX'                   TO GX-RECORD-ID.
03163      MOVE AM-COMPANY-CD          TO GX-COMPANY-CD.
03164
03165      IF PI-ZERO-CARRIER
03166        OR PI-ZERO-CAR-GROUP
03167          MOVE ZERO               TO GX-CARRIER
03168      ELSE
03169          MOVE AM-CARRIER         TO GX-CARRIER.
03170
03171      IF PI-ZERO-GROUPING
03172        OR PI-ZERO-CAR-GROUP
03173          MOVE ZERO               TO GX-GROUPING
03174      ELSE
03175          MOVE AM-GROUPING        TO GX-GROUPING.
03176
03177      MOVE AM-AGT (AXRF-SUB)      TO GX-AGENT-NO.
03178
03179      MOVE AM-CARRIER             TO GX-AM-CARRIER (1).
03180      MOVE AM-GROUPING            TO GX-AM-GROUPING (1).
03181      MOVE AM-STATE               TO GX-AM-STATE (1).
03182      MOVE AM-ACCOUNT             TO GX-AM-ACCOUNT (1).
03183      MOVE AM-EXPIRATION-DT       TO GX-AM-EXPIRATION-DT (1).
03184      MOVE AM-EFFECTIVE-DT        TO GX-AM-EFF-DT (1).
03185      MOVE AXRF-SUB               TO GX-AM-LEVEL-NO (1).
03186      MOVE LOW-VALUES             TO GX-LAST-BILL-DT (1).
03187
03188      MOVE BIN-CURRENT-SAVE       TO GX-LAST-MAINT-DT.
03189      MOVE EIBTIME                TO GX-LAST-MAINT-HHMMSS.
03190      MOVE PI-PROCESSOR-ID        TO GX-LAST-MAINT-USER.
03191
03192      
      * EXEC CICS WRITE
03193 *        FROM      (AGENT-CROSS-REFERENCE)
03194 *        LENGTH    (ERGXRF-REC-LEN)
03195 *        RIDFLD    (GX-CONTROL-PRIMARY)
03196 *        DATASET   (ERGXRF-FILE-ID)
03197 *    END-EXEC.
      *    MOVE '&$ L                  ''   #00009463' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 GX-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03198
03199  6959-EXIT.
03200       EXIT.
03201  EJECT
03202  6970-REWRITE-AGENT-RECORD.
03203      MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN.
03204
03205      
      * EXEC CICS READ
03206 *        DATASET   (ERGXRF-FILE-ID)
03207 *        INTO      (AGENT-CROSS-REFERENCE)
03208 *        LENGTH    (ERGXRF-REC-LEN)
03209 *        RIDFLD    (WS-AXRF-KEY)
03210 *        EQUAL
03211 *        UPDATE
03212 *    END-EXEC.
      *    MOVE '&"IL       EU         (   #00009476' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 WS-AXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03213
03214      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
03215
03216      IF ACCT-LEVEL-DIFFERENT
03217         GO TO 6975-UPDATE-LEVEL-NO.
03218
03219      IF GX-AGENT-POINTER-CNT = +1006
03220          GO TO 6979-EXIT.
03221
03222      MOVE +1 TO FIND-AGENT-SUB1.
03223
03224  6972-FIND-REC-LOCATION.
03225      MOVE GX-AGENT-POINTER (FIND-AGENT-SUB1) TO GX-REST-OF-KEY.
03226      MOVE AM-COMPANY-CD TO GX-AM-COMPANY-CD.
03227
03228      IF (GX-AM-CONTROL-PRIMARY GREATER AM-CONTROL-PRIMARY)  OR
03229         (FIND-AGENT-SUB1 GREATER GX-AGENT-POINTER-CNT)
03230         NEXT SENTENCE
03231      ELSE
03232         ADD +1 TO FIND-AGENT-SUB1
03233         GO TO 6972-FIND-REC-LOCATION.
03234
03235      MOVE GX-AGENT-POINTER-CNT TO FIND-AGENT-SUB2
03236                                   FIND-AGENT-SUB3.
03237      ADD +1 TO FIND-AGENT-SUB3.
03238
03239      IF FIND-AGENT-SUB1 GREATER GX-AGENT-POINTER-CNT
03240         NEXT SENTENCE
03241      ELSE
03242         PERFORM 6977-BUMP-AXRF-REC VARYING FIND-AGENT-SUB2
03243         FROM FIND-AGENT-SUB2 BY -1 UNTIL
03244         FIND-AGENT-SUB3 = FIND-AGENT-SUB1.
03245
03246      ADD ERGXRF-INC-LEN          TO ERGXRF-REC-LEN.
03247      ADD  +1                     TO GX-AGENT-POINTER-CNT.
03248
03249      MOVE AM-CARRIER  TO GX-AM-CARRIER (FIND-AGENT-SUB3).
03250      MOVE AM-GROUPING TO GX-AM-GROUPING (FIND-AGENT-SUB3).
03251
03252      MOVE AM-STATE    TO GX-AM-STATE (FIND-AGENT-SUB3).
03253      MOVE AM-ACCOUNT  TO GX-AM-ACCOUNT (FIND-AGENT-SUB3).
03254      MOVE AM-EXPIRATION-DT TO
03255                   GX-AM-EXPIRATION-DT (FIND-AGENT-SUB3).
03256      MOVE AM-EFFECTIVE-DT  TO
03257                   GX-AM-EFF-DT (FIND-AGENT-SUB3).
03258      MOVE AXRF-SUB    TO GX-AM-LEVEL-NO (FIND-AGENT-SUB3).
03259      MOVE LOW-VALUES  TO GX-LAST-BILL-DT (FIND-AGENT-SUB3).
03260
03261  6975-UPDATE-LEVEL-NO.
03262      MOVE AXRF-SUB TO GX-AM-LEVEL-NO (GX-AGENT-SUB).
03263      MOVE BIN-CURRENT-SAVE       TO GX-LAST-MAINT-DT.
03264      MOVE EIBTIME                TO GX-LAST-MAINT-HHMMSS.
03265      MOVE PI-PROCESSOR-ID        TO GX-LAST-MAINT-USER.
pemgm      compute ergxrf-rec-len = (gx-agent-pointer-cnt * +31) + +78
pemgm      if ergxrf-rec-len > +31264
pemgm         move +31264               to ergxrf-rec-len
pemgm         move +1006                to gx-agent-pointer-cnt
pemgm      end-if
03266
03267      
      * EXEC CICS REWRITE
03268 *        FROM      (AGENT-CROSS-REFERENCE)
03269 *        LENGTH    (ERGXRF-REC-LEN)
03270 *        DATASET   (ERGXRF-FILE-ID)
03271 *    END-EXEC.
      *    MOVE '&& L                  %   #00009543' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03272
03273      GO TO 6979-EXIT.
03274
03275  6977-BUMP-AXRF-REC.
03276      MOVE GX-AGENT-POINTER (FIND-AGENT-SUB2) TO
03277           GX-AGENT-POINTER (FIND-AGENT-SUB3).
03278
03279      SUBTRACT +1 FROM FIND-AGENT-SUB3.
03280
03281  6979-EXIT.
03282      EXIT.
03283
03284  6999-EXIT.
03285      EXIT.
03286  EJECT
03287  7000-DELETE-AXRF-REC.
03288      MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN.
03289
03290      
      * EXEC CICS HANDLE CONDITION
03291 *         ENDFILE     (7999-EXIT)
03292 *         NOTFND      (7999-EXIT)
03293 *    END-EXEC.
      *    MOVE '"$''I                  ! 2 #00009566' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303039353636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03294
03295      
      * EXEC CICS READ
03296 *        DATASET   (ERGXRF-FILE-ID)
03297 *        INTO      (AGENT-CROSS-REFERENCE)
03298 *        LENGTH    (ERGXRF-REC-LEN)
03299 *        RIDFLD    (WS-AXRF-KEY)
03300 *        EQUAL
03301 *        UPDATE
03302 *    END-EXEC.
      *    MOVE '&"IL       EU         (   #00009571' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 WS-AXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03303
03304      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
03305
03306      IF GX-AGENT-SUB = +0
03307         PERFORM 6921-FIND-ACCT-LOOP VARYING GX-AGENT-SUB
03308         FROM +1 BY +1 UNTIL
03309          (GX-AGENT-SUB GREATER GX-AGENT-POINTER-CNT) OR
03310          (GX-AM-ACCOUNT (GX-AGENT-SUB) = AM-ACCOUNT  AND
03311          GX-AM-EXPIRATION-DT (GX-AGENT-SUB) = AM-EXPIRATION-DT
03312          AND
03313          GX-AM-EFF-DT (GX-AGENT-SUB)   = AM-EFFECTIVE-DT AND
03314          GX-AM-CARRIER (GX-AGENT-SUB)  = AM-CARRIER      AND
03315          GX-AM-GROUPING (GX-AGENT-SUB) = AM-GROUPING     AND
03316          GX-AM-STATE (GX-AGENT-SUB)    = AM-STATE).
03317
03318      IF GX-AGENT-SUB GREATER GX-AGENT-POINTER-CNT
03319         GO TO 7999-EXIT.
03320
03321      MOVE GX-AGENT-SUB TO FIND-AGENT-SUB1.
03322      PERFORM 7010-BUMP-RECORDS VARYING GX-AGENT-SUB
03323                                   FROM GX-AGENT-SUB BY +1 UNTIL
03324                             GX-AGENT-SUB = GX-AGENT-POINTER-CNT.
03325      SUBTRACT ERGXRF-INC-LEN FROM ERGXRF-REC-LEN.
03326      SUBTRACT +1             FROM GX-AGENT-POINTER-CNT.
03327      GO TO 7020-REWRITE-AXRF-RECORD.
03328
03329  7010-BUMP-RECORDS.
03330      ADD +1 TO FIND-AGENT-SUB1.
03331      MOVE GX-AGENT-POINTER (FIND-AGENT-SUB1) TO
03332           GX-AGENT-POINTER (GX-AGENT-SUB).
03333
03334  7020-REWRITE-AXRF-RECORD.
03335      IF GX-AGENT-POINTER-CNT = +0
03336         
      * EXEC CICS DELETE
03337 *            DATASET   (ERGXRF-FILE-ID)
03338 *       END-EXEC
      *    MOVE '&(                    &   #00009612' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03339      ELSE
03340         MOVE BIN-CURRENT-SAVE       TO GX-LAST-MAINT-DT
03341         MOVE EIBTIME                TO GX-LAST-MAINT-HHMMSS
03342         MOVE PI-PROCESSOR-ID        TO GX-LAST-MAINT-USER
pemgm         compute ergxrf-rec-len =
pemgm             (gx-agent-pointer-cnt * +31) + +78
pemgm         if ergxrf-rec-len > +31264
pemgm            move +31264               to ergxrf-rec-len
pemgm            move +1006                to gx-agent-pointer-cnt
pemgm         end-if
03343         
      * EXEC CICS REWRITE
03344 *            FROM      (AGENT-CROSS-REFERENCE)
03345 *            LENGTH    (ERGXRF-REC-LEN)
03346 *            DATASET   (ERGXRF-FILE-ID)
03347 *       END-EXEC.
      *    MOVE '&& L                  %   #00009625' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03348
03349  7999-EXIT.
03350      EXIT.
03351
03352 *************************************
03353  8000-STATE-REC-READ.
03354      MOVE SPACES                 TO ELCNTL-KEY.
03355      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
03356      MOVE '3'                    TO CNTL-REC-TYPE.
03357      MOVE PI-ACCT-STATE          TO CNTL-ACCESS.
03358      MOVE +0                     TO CNTL-SEQ-NO.
03359      
      * EXEC CICS HANDLE CONDITION
03360 *        NOTFND  (8010-EXIT)
03361 *    END-EXEC.
      *    MOVE '"$I                   ! 3 #00009641' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303039363431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03362
03363      
      * EXEC CICS READ
03364 *        DATASET   (CNTL-FILE-ID)
03365 *        SET       (ADDRESS OF CONTROL-FILE)
03366 *        RIDFLD    (ELCNTL-KEY)
03367 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009645' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03368
03369      MOVE CF-ST-COMMISSION-CAPS   TO WS-ST-COMMISSION-CAPS
102717     MOVE CF-ST-GA-COMMISSION-CAPS
102717                                  TO WS-ST-GA-COMMISSION-CAPS
102717     MOVE CF-ST-TOT-COMMISSION-CAPS
102717                                  TO WS-ST-TOT-COMMISSION-CAPS
03370      MOVE CF-COMM-CAP-LIMIT-TO    TO WS-COMM-CAP-LIMIT-TO.
03371
03372  8010-EXIT.
03373       EXIT.
03374
03375  8050-USER-REC-READ.
03376      MOVE SPACES                 TO ELCNTL-KEY.
03377      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
03378      MOVE '2'                    TO CNTL-REC-TYPE.
03379      MOVE CSRI                   TO CNTL-ACCESS.
03380      MOVE +0                     TO CNTL-SEQ-NO.
03381      
      * EXEC CICS HANDLE CONDITION
03382 *        NOTFND  (8055-NOTFND)
03383 *    END-EXEC.
      *    MOVE '"$I                   ! 4 #00009667' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303039363637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03384
03385      
      * EXEC CICS READ
03386 *        DATASET   (CNTL-FILE-ID)
03387 *        SET       (ADDRESS OF CONTROL-FILE)
03388 *        RIDFLD    (ELCNTL-KEY)
03389 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009671' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03390
03391      GO TO 8060-EXIT.
03392
03393  8055-NOTFND.
03394      MOVE ER-1883             TO EMI-ERROR.
03395      MOVE -1                  TO CSRL.
03396      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03397
03398  8060-EXIT.
03399       EXIT.
03400
03401 *************************************
03402
03403  8100-SEND-INITIAL-MAP.
03404      MOVE SAVE-DATE              TO DATEO.
03405      MOVE EIBTIME                TO TIME-IN.
03406      MOVE TIME-OUT               TO TIMEO.
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
03407      MOVE -1                     TO PFENTERL.
03408      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
03409
03410      MOVE PI-AH-OVERRIDE-L6      TO AHHEADO.
03411
03412 *    IF PI-COMPANY-ID = 'HAN' OR 'LGX' OR 'BNE'
03413 *       NEXT SENTENCE
03414 *    ELSE
03415 *       MOVE AL-SADOF            TO PSIHEADA
03416 *                                   PSILFTA
03417 *                                   PSILFCA
03418 *                                   PSIAHTA
03419 *                                   PSIAHCA
03420 *                                   PSILFFA
03421 *                                   PSIAHFA.
03422
03423 *    IF PI-COMPANY-ID = 'DMD'
03424 *         MOVE AL-SANOF          TO KEY10A.
03425
020816     IF PI-COMPANY-ID = 'DCC' or 'VPP'
021506        CONTINUE
021506     ELSE
021506        MOVE AL-SADOF            TO CTPLABLA
021506                                    CLPTOLPA
021506                                    LCLABLA
021506                                    LCOMMA
                                          MAXFEEHA
                                          CLPSTHA
                                          PRODCDHA
021506     END-IF
03426      IF PI-COMPANY-ID = 'NCL' OR 'LGX'
03427         MOVE -1                  TO CSRL.
03428
03429      IF  CLAIM-SESSION
03430          MOVE SPACES             TO PF1PF2O
03431          MOVE AL-SANOF           TO PF1PF2A.
03432
03433      
      * EXEC CICS SEND
03434 *        MAP      (MAP-NAME)
03435 *        MAPSET   (MAPSET-NAME)
03436 *        FROM     (EL6501AO)
03437 *        ERASE
03438 *        CURSOR
03439 *    END-EXEC.
           MOVE LENGTH OF
            EL6501AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009732' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6501AO, 
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
           
03440
03441      GO TO 9100-RETURN-TRAN.
03442
03443  8200-SEND-DATAONLY.
03444      MOVE SAVE-DATE              TO DATEO.
03445      MOVE EIBTIME                TO TIME-IN.
03446      MOVE TIME-OUT               TO TIMEO.
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
03447      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
03448
03449      MOVE PI-AH-OVERRIDE-L6      TO AHHEADO.
03450
020816     IF PI-COMPANY-ID = 'DCC' or 'VPP'
021506        CONTINUE
021506     ELSE
021506        MOVE AL-SADOF            TO CTPLABLA
021506                                    CLPTOLPA
021506                                    LCLABLA
021506                                    LCOMMA
                                          MAXFEEHA
                                          CLPSTHA
                                          PRODCDHA
021506     END-IF
03451      IF  CLAIM-SESSION
03452          MOVE SPACES             TO PF1PF2O
03453          MOVE AL-SANOF           TO PF1PF2A.
03454
03455      
      * EXEC CICS SEND
03456 *        MAP      (MAP-NAME)
03457 *        MAPSET   (MAPSET-NAME)
03458 *        FROM     (EL6501AO)
03459 *        DATAONLY
03460 *        CURSOR
03461 *    END-EXEC.
           MOVE LENGTH OF
            EL6501AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009767' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6501AO, 
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
           
03462
03463      GO TO 9100-RETURN-TRAN.
03464
03465  8300-SEND-TEXT.
03466      
      * EXEC CICS SEND TEXT
03467 *        FROM     (LOGOFF-TEXT)
03468 *        LENGTH   (LOGOFF-LENGTH)
03469 *        ERASE
03470 *        FREEKB
03471 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009778' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373738' TO DFHEIV0(25:11)
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
           
03472
03473      
      * EXEC CICS RETURN
03474 *    END-EXEC.
      *    MOVE '.(                    ''   #00009785' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03475
03476  8600-DEEDIT.
03477      
      * EXEC CICS BIF DEEDIT
03478 *         FIELD(DEEDIT-FIELD)
03479 *         LENGTH(15)
03480 *     END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009789' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03481
03482  8800-UNAUTHORIZED-ACCESS.
03483      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
03484      GO TO 8300-SEND-TEXT.
03485
03486  8810-PF23.
03487      MOVE EIBAID                 TO PI-ENTRY-CD-1.
03488      MOVE XCTL-005               TO PGM-NAME.
03489      GO TO 9300-XCTL.
03490
03491  9100-RETURN-TRAN.
03492      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
03493      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
03494      
      * EXEC CICS RETURN
03495 *        TRANSID    (TRANS-ID)
03496 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
03497 *        LENGTH     (WS-COMM-LENGTH)
03498 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00009806' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03499
03500  9200-RETURN-MAIN-MENU.
03501      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
03502          MOVE XCTL-126           TO  PGM-NAME
03503      ELSE
03504          MOVE XCTL-626           TO  PGM-NAME.
03505
03506      GO TO 9300-XCTL.
03507
03508  9300-XCTL.
03509      
      * EXEC CICS XCTL
03510 *        PROGRAM    (PGM-NAME)
03511 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
03512 *        LENGTH     (WS-COMM-LENGTH)
03513 *    END-EXEC.
      *    MOVE '.$C                   %   #00009821' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03514
03515  9400-CLEAR.
03516      IF PI-GA-BILLING
03517         IF GETMAIN-ACQUIRED
03518            MOVE PI-COMM-POINTER TO LCP-WS-ADDR-COMP
03519            SET ADDRESS OF COMMISSION-WORK-AREA TO LCP-WS-ADDR-PNTR
03520            
      * EXEC CICS FREEMAIN
03521 *               DATA     (COMMISSION-WORK-AREA)
03522 *          END-EXEC.
      *    MOVE ',$D                   "   #00009832' TO DFHEIV0
           MOVE X'2C2444202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMMISSION-WORK-AREA
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03523
03524      MOVE SPACES                 TO PI-SV-MAINT.
03525      MOVE XCTL-650               TO PGM-NAME.
03526      MOVE XCTL-650               TO PI-RETURN-TO-PROGRAM.
03527      GO TO 9300-XCTL.
03528
03529  9500-PF12.
03530      MOVE XCTL-010               TO PGM-NAME.
03531      GO TO 9300-XCTL.
03532
03533  9600-PGMID-ERROR.
03534      
      * EXEC CICS HANDLE CONDITION
03535 *        PGMIDERR    (8300-SEND-TEXT)
03536 *    END-EXEC.
      *    MOVE '"$L                   ! 5 #00009846' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303039383436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03537
03538      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
03539      MOVE ' '                    TO PI-ENTRY-CD-1.
03540      MOVE XCTL-005               TO PGM-NAME.
03541      MOVE PGM-NAME               TO LOGOFF-PGM.
03542      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
03543      GO TO 9300-XCTL.
03544
03545  9700-DATE-LINK.
03546      MOVE LINK-ELDATCV           TO PGM-NAME.
03547      
      * EXEC CICS LINK
03548 *        PROGRAM    (PGM-NAME)
03549 *        COMMAREA   (DATE-CONVERSION-DATA)
03550 *        LENGTH     (DC-COMM-LENGTH)
03551 *    END-EXEC.
      *    MOVE '."C                   (   #00009859' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03552
03553  9900-ERROR-FORMAT.
03554      IF NOT EMI-ERRORS-COMPLETE
03555          MOVE LINK-001           TO PGM-NAME
03556          
      * EXEC CICS LINK
03557 *            PROGRAM    (PGM-NAME)
03558 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
03559 *            LENGTH     (EMI-COMM-LENGTH)
03560 *        END-EXEC.
      *    MOVE '."C                   (   #00009868' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03561
03562  9900-EXIT.
03563      EXIT.
03564
03565  9990-ABEND.
03566      MOVE LINK-004               TO PGM-NAME.
03567      MOVE DFHEIBLK               TO EMI-LINE1.
03568      
      * EXEC CICS LINK
03569 *        PROGRAM   (PGM-NAME)
03570 *        COMMAREA  (EMI-LINE1)
03571 *        LENGTH    (72)
03572 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00009880' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03573
03574      MOVE -1                     TO PFENTERL.
03575      GO TO 8200-SEND-DATAONLY.
03576
03577      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6501' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
03578
03579  9995-SECURITY-VIOLATION.
03580 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00009909' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393039' TO DFHEIV0(25:11)
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
03581
03582  9995-EXIT.
03583      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6501' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND,
                     0500-CHECK-MAINT-TYPE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1500-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1799-EXIT,
                     1799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2100-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 2200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3030-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 3530-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4020-BUS-TYPE-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4099-STD-BEN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4180-REIN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5250-QID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 5399-EXIT,
                     5399-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6450-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 6699-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 6880-NO-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 6925-NOT-FIND,
                     6925-NOT-FIND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 7999-EXIT,
                     7999-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8010-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 8055-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6501' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
