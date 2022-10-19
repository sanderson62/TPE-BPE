00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL650 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 07/14/94 14:53:46.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.048.
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
00026 *        TRANSACTION - EXC4 - ACCOUNT MASTER MAINTENANCE.
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
101101*                              ADJUSTED REDEFINES EL650AI FILLER
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
110706* 110706  CR2006071700004  PEMA  ADD BRANCH LOCATIONS
110706*           AND SHIPPING ADDRESS TO ACCOUNT NOTES FILE
121207* 121207  CR2007121100001  PEMA  SPACE OUT ORIG DLR NO ON COPY
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
080612* 080612  CR2012042700005  PEMA  ADD OVER 120 DAYS FOR AHL
111513* 111513  IR2013110500003  PEMA  CORRECT MAINT FUNC OF "N"
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
052716* 052716  CR2016052600003  PEMA  Correct "D" delete processing
101101******************************************************************
00027
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  EJECT
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC  X(32) VALUE '********************************'.
00033  77  FILLER  PIC  X(32) VALUE '*    EL650 WORKING STORAGE     *'.
00034  77  FILLER  PIC  X(32) VALUE '********* VMOD=2.048 ***********'.
111513 77  d1                      pic s999 value +0 comp-3.
00035
00036  01  WS-COMM-LENGTH          PIC S9(4) COMP VALUE +1500.
00037
111513 01  ws-display-eff-date     pic x(10) value spaces.
111513 01  ws-display-exp-date     pic x(10) value spaces.
00038  01  WS-DATE-AREA.
00039      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.
00040      12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.
00041
00042  01  STANDARD-AREAS.
00043      12  GETMAIN-SPACE           PIC  X      VALUE SPACE.
00044      12  MAP-NAME                PIC  X(8)   VALUE 'EL650A'.
00045      12  MAPSET-NAME             PIC  X(8)   VALUE 'EL650S'.
00046      12  SCREEN-NUMBER           PIC  X(4)   VALUE '650A'.
00047      12  TRANS-ID                PIC  X(4)   VALUE 'EXC4'.
00048      12  EL6311-TRANS-ID         PIC  X(4)   VALUE 'EXB1'.
020816     12  vp6311-trans-id         pic  x(4)   value 'VPB1'.
CIDMOD     12  EL1273-TRANS-ID         PIC  X(4)   VALUE 'EXX3'.
CIDMOD     12  EL652-TRANS-ID          PIC  X(4)   VALUE 'EXD4'.
100703     12  EL6523-TRANS-ID         PIC  X(4)   VALUE 'EXDD'.
00049      12  EL6311NC-TRANS-ID       PIC  X(4)   VALUE 'NXB1'.
00050      12  EL640-TRANS-ID          PIC  X(4)   VALUE 'EXC1'.
00051      12  EL658-TRANS-ID          PIC  X(4)   VALUE 'EXJ3'.
00052      12  EL130-TRANS-ID          PIC  X(4)   VALUE 'EX19'.
110706     12  EL6503-TRANS-ID         PIC  X(4)   VALUE 'EXC7'.
00053      12  EL6509-TRANS-ID         PIC  X(4)   VALUE 'EXDA'.
00054      12  EL6592-TRANS-ID         PIC  X(4)   VALUE 'EX66'.
00055      12  EL608-TRANS-ID          PIC  X(4)   VALUE 'EX1E'.
00056      12  THIS-PGM                PIC  X(8)   VALUE 'EL650'.
00057      12  PGM-NAME                PIC  X(8).
00058      12  RETURNED-FROM           PIC  X(8)   VALUE SPACES.
00059      12  TIME-IN                 PIC S9(7)   VALUE ZEROS.
00060      12  TIME-OUT-R  REDEFINES  TIME-IN.
00061          16  FILLER              PIC  X.
00062          16  TIME-OUT            PIC  99V99.
00063          16  FILLER              PIC  XX.
00064      12  XCTL-005                PIC  X(8)   VALUE 'EL005'.
00065      12  XCTL-010                PIC  X(8)   VALUE 'EL010'.
00066      12  XCTL-126                PIC  X(8)   VALUE 'EL126'.
00067      12  XCTL-EL601              PIC  X(8)   VALUE 'EL601 '.
00068      12  XCTL-6501               PIC  X(8)   VALUE 'EL6501'.
00069      12  XCTL-6502               PIC  X(8)   VALUE 'EL6502'.
110706     12  XCTL-6503               PIC  X(8)   VALUE 'EL6503'.
00071      12  XCTL-6504               PIC  X(8)   VALUE 'EL6504'.
00072      12  XCTL-6505               PIC  X(8)   VALUE 'EL6505'.
00073      12  XCTL-6506               PIC  X(8)   VALUE 'EL6506'.
00074      12  XCTL-6508               PIC  X(8)   VALUE 'EL6508'.
00075      12  XCTL-6509               PIC  X(8)   VALUE 'EL6509'.
00076      12  LINK-001                PIC  X(8)   VALUE 'EL001'.
00077      12  LINK-004                PIC  X(8)   VALUE 'EL004'.
00078      12  LINK-ELDATCV            PIC  X(8)   VALUE 'ELDATCV'.
00079      12  FILE-ID                 PIC  X(8)   VALUE SPACES.
00080      12  CNTL-FILE-ID            PIC  X(8)   VALUE 'ELCNTL'.
00081      12  ACCT-FILE-ID            PIC  X(8)   VALUE 'ERACCT'.
00082      12  COMP-FILE-ID            PIC  X(8)   VALUE 'ERCOMP'.
00083      12  NAME-FILE-ID            PIC  X(8)   VALUE 'ERNAME'.
00084      12  PEND2-FILE-ID           PIC  X(8)   VALUE 'ERPNDB2'.
00085      12  PLAN-FILE-ID            PIC  X(8)   VALUE 'ERPLAN'.
00086      12  ACCT2-FILE-ID           PIC  X(8)   VALUE 'ERACCT2'.
00087      12  ACCT-REC-LEN            PIC S9(4)   VALUE +2000  COMP.
00088      12  PLAN-REC-LEN            PIC S9(4)   VALUE +420   COMP.
00089      12  COMP-REC-LEN            PIC S9(4)   VALUE +700   COMP.
00090      12  NAME-REC-LEN            PIC S9(4)   VALUE +160   COMP.
00091      12  SC-ITEM                 PIC S9(4)   VALUE +1     COMP.
00092      12  ERGXRF-FILE-ID          PIC  X(8)   VALUE 'ERGXRF'.
00093      12  ERGXRF-REC-LEN          PIC S9(4)   VALUE +0     COMP.
00094      12  ERGXRF-INC-LEN          PIC S9(4)   VALUE +31    COMP.
00095      12  ERGXRF-MIN-LEN          PIC S9(4)   VALUE +109   COMP.
00096      12  ERGXRF-MAX-LEN          PIC S9(8)   VALUE +31264 COMP.
00097      12  BIN-MIN-DATE            PIC  XX     VALUE LOW-VALUES.
00098      12  BIN-MAX-DATE            PIC  XX     VALUE LOW-VALUES.
00099      12  B4-ACCT-KEY.
00100          16  B4-ACCT-CO          PIC  X      VALUE LOW-VALUES.
00101          16  B4-ACCT-CARRIER     PIC  X      VALUE SPACES.
00102          16  B4-ACCT-GROUPING    PIC  X(6)   VALUE SPACES.
00103          16  B4-ACCT-STATE       PIC  XX     VALUE SPACES.
00104          16  B4-ACCT-ACCOUNT     PIC  X(10)  VALUE SPACES.
00105          16  B4-ACCT-EXP-DT      PIC  XX     VALUE LOW-VALUES.
00106          16  B4-ACCT-EFF-DT      PIC  XX     VALUE LOW-VALUES.
00107      12  AF-ACCT-KEY.
00108          16  AF-ACCT-CO          PIC  X      VALUE LOW-VALUES.
00109          16  AF-ACCT-CARRIER     PIC  X      VALUE SPACES.
00110          16  AF-ACCT-GROUPING    PIC  X(6)   VALUE SPACES.
00111          16  AF-ACCT-STATE       PIC  XX     VALUE SPACES.
00112          16  AF-ACCT-ACCOUNT     PIC  X(10)  VALUE SPACES.
00113          16  AF-ACCT-EXP-DT      PIC  XX     VALUE LOW-VALUES.
00114          16  AF-ACCT-EFF-DT      PIC  XX     VALUE LOW-VALUES.
00115      12  GX-AM-CONTROL-PRIMARY.
00116          16  GX-AM-COMPANY-CD    PIC  X      VALUE LOW-VALUES.
00117          16  GX-REST-OF-KEY      PIC  X(21)  VALUE SPACES.
00118          16  GX-AM-CNTL-FILLER   PIC  X(4)   VALUE SPACES.
00119  EJECT
      *                                copy ERCGXRF.
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
00120  01  WORK-AREAS-AND-SWITCHES.
00121      12  WS-PAGE-LINES               PIC  S99    VALUE ZEROS.
00122      12  WS-ACCT2-FOUND              PIC  X      VALUE 'N'.
00123      12  GETMAIN-SW                  PIC  X      VALUE SPACES.
00124      12  NEW-KEY-SAVE.
00125          16  NEW-KEY-COMPANY-CD      PIC  X      VALUE LOW-VALUES.
00126          16  NEW-KEY-CNTRL           PIC  X(19)  VALUE SPACES.
00127      12  WS-PLAN-KEY.
00128          16  EK-CCGSA-KEY.
00129            18  EK-COMPANY-CD         PIC  X     VALUE LOW-VALUES.
00130            18  EK-CARRIER            PIC  X     VALUE SPACES.
00131            18  EK-GROUPING           PIC  X(6)  VALUE SPACES.
00132            18  EK-STATE              PIC  XX    VALUE SPACES.
00133            18  EK-ACCOUNT            PIC  X(10) VALUE SPACES.
00134          16  EK-BTYPE                PIC  X     VALUE SPACES.
00135          16  EK-BCODE                PIC  XX    VALUE SPACES.
00136          16  EK-REVISION             PIC  XXX   VALUE SPACES.
00137      12  WS-EXP-SAVE                 PIC  XX    VALUE LOW-VALUES.
00138      12  CURRENT-SAVE                PIC  X(6)  VALUE SPACES.
00139      12  SAVE-ACCT-KEY               PIC  X(26).
00140      12  SAVE-CURR-EXP-DT            PIC  XX    VALUE LOW-VALUES.
00141      12  SAVE-CURR-EFF-DT            PIC  XX    VALUE LOW-VALUES.
00142      12  WS-EFFCHG-SAVE              PIC  9(11) VALUE ZEROS.
00143      12  WS-EXPCHG-SAVE              PIC  9(11) VALUE ZEROS.
00144      12  BIN-CURRENT-SAVE            PIC  XX    VALUE LOW-VALUES.
00145      12  WS-VERIFY-AREA.
00146          16  FILLER      OCCURS  33  TIMES
00147                              INDEXED  BY  WS-INDEX.
00148              20  WS-ACCT-KEY         PIC  X(20).
00149              20  WS-ACCT-EXP-DT      PIC  XX.
00150              20  WS-ACCT-EFF-DT      PIC  XX.
00151              20  WS-ACCT-HI-CERT     PIC  XX.
00152              20  WS-ACCT-LO-CERT     PIC  XX.
00153      12  WS-TEST-KEY                 PIC  X(20)  VALUE SPACES.
00154      12  WS-SAVE-VERIFY-KEY          PIC  X(20)  VALUE SPACES.
00155      12  WS-ACCT-VERIFY-KEY.
00156          16  WS-ACCT-VER-COMP-CD     PIC  X      VALUE LOW-VALUES.
00157          16  WS-ACCT-VER-CARR        PIC  X      VALUE SPACES.
00158          16  WS-ACCT-VER-GROUP       PIC  X(6)   VALUE SPACES.
00159          16  WS-ACCT-VER-STATE       PIC  XX     VALUE SPACES.
00160          16  WS-ACCT-VER-ACCOUNT     PIC  X(10)  VALUE SPACES.
00161          16  WS-ACCT-VER-EXP-DT      PIC  XX     VALUE LOW-VALUES.
pemtst     12  filler                      pic x(10)   value spaces.
00162      12  KEY-SAVE.
00163          16  SV-CO                   PIC  X      VALUE LOW-VALUES.
00164          16  SV-CARRIER              PIC  X      VALUE SPACES.
00165          16  SV-GROUPING             PIC  X(6)   VALUE SPACES.
00166          16  SV-STATE                PIC  XX     VALUE SPACES.
00167          16  SV-ACCOUNT              PIC  X(10)  VALUE SPACES.
00168
00169      12  WS-DEEDIT-FIELD              PIC X(10)  VALUE SPACES.
00170      12  WS-DT-DEEDIT-FIELD REDEFINES WS-DEEDIT-FIELD
00171                                       PIC X(10).
00172      12  WS-DEEDIT-FIELD-DATE REDEFINES WS-DT-DEEDIT-FIELD.
00173          16  FILLER                   PIC X(4).
00174          16  WS-DEEDIT-FIELD-DATE-OUT PIC X(6).
00175
00176      12  WS-AXRF-KEY.
00177          16  WS-AXRF-COMPANY-CD      PIC  X     VALUE LOW-VALUES.
00178          16  WS-AXRF-CARRIER         PIC  X     VALUE SPACES.
00179          16  WS-AXRF-GROUPING        PIC  X(6)  VALUE SPACES.
00180          16  WS-AXRF-AGENT-NO        PIC  X(10) VALUE SPACES.
00181      12  AXRF-SUB                    PIC S9(4) COMP VALUE +0.
00182      12  GX-AGENT-SUB                PIC S9(4) COMP VALUE +0.
00183      12  COMM-WORK-SUB               PIC S9(4) COMP VALUE +0.
00184      12  COMM-WORK-SUB2              PIC S9(4) COMP VALUE +0.
00185      12  ELCNTL-KEY.
00186          16  CNTL-COMP-ID            PIC  X(3) VALUE SPACES.
00187          16  CNTL-REC-TYPE           PIC  X    VALUE SPACES.
00188          16  CNTL-ACCESS.
00189              20  CNTL-STATE          PIC  XX   VALUE SPACES.
00190              20  FILLER              PIC  X    VALUE SPACES.
00191              20  CNTL-CARRIER        PIC  X    VALUE SPACES.
00192          16  CNTL-SEQ-NO             PIC S9(4) COMP VALUE +0.
00193      12  ERPNDB-KEY.
00194          16  PNDB-COMPANY-CD-A1      PIC  X  VALUE LOW-VALUES.
00195          16  PNDB-CARRIER            PIC  X    VALUE SPACES.
00196          16  PNDB-GROUPING           PIC  X(6) VALUE SPACES.
00197          16  PNDB-STATE              PIC  XX   VALUE SPACES.
00198          16  PNDB-ACCOUNT            PIC  X(10) VALUE SPACES.
00199          16  PNDB-CERT-EFF-DT        PIC  XX    VALUE LOW-VALUES.
00200          16  PNDB-CERT-NO            PIC  X(11) VALUE SPACES.
00201          16  PNDB-ALT-CHG-SEQ-NO     PIC S9(4)   COMP VALUE +0.
00202          16  PNDB-RECORD-TYPE        PIC  X   VALUE SPACES.
00203      12  REC-SW                      PIC  9          VALUE 0.
00204          88  REC-FOUND                               VALUE 1.
00205      12  K-WITH-LINE-USED            PIC  9          VALUE 0.
00206          88  K-WITH-LINE-NO                          VALUE 1.
00207      12  RETURN-FROM-SW              PIC  9          VALUE 0.
00208          88  RETURNING-FROM-6501                     VALUE 1.
00209      12  BROWSE-STARTED-SW           PIC  X          VALUE ' '.
00210          88  BROWSE-STARTED                          VALUE 'Y'.
00211      12  FIRST-TIME-SW               PIC  X          VALUE ' '.
00212          88  FIRST-TIME                              VALUE 'Y'.
00213      12  FROM-WHERE-SW               PIC  X          VALUE ' '.
00214          88  FROM-DELETE                             VALUE '1'.
00215          88  FROM-CHANGE                             VALUE '2'.
00216      12  FROM-EL608-SW               PIC  X          VALUE 'N'.
00217      12  PENDING-BUSINESS-SW         PIC  X          VALUE 'N'.
00218          88  THERE-IS-PENDING-BUSINESS               VALUE 'Y'.
00219      12  PENDING-BROWSE-SW           PIC  X          VALUE 'N'.
00220          88  PENDING-BROWSE-STARTED                  VALUE 'Y'.
00221      12  TS-QUEUE.
00222          16  TS-SCREEN               PIC  X(4)   VALUE SPACES.
00223          16  TS-TERM                 PIC  X(4)   VALUE SPACES.
00224      12  TS-LENGTH                   PIC S9(4) COMP VALUE +640.
00225      12  TS-ITEM                     PIC S9(4) COMP VALUE +0.
00226      12  AGT-SUB                     PIC 99      VALUE ZEROS.
00227      12  VALID-AGT-SW                PIC X       VALUE 'N'.
00228
00229  01  ERROR-MESSAGES.
00230      12  ER-0000             PIC  X(4)       VALUE '0000'.
00231      12  ER-0004             PIC  X(4)       VALUE '0004'.
00232      12  ER-0008             PIC  X(4)       VALUE '0008'.
00233      12  ER-0022             PIC  X(4)       VALUE '0022'.
00234      12  ER-0023             PIC  X(4)       VALUE '0023'.
00235      12  ER-0029             PIC  X(4)       VALUE '0029'.
00236      12  ER-0042             PIC  X(4)       VALUE '0042'.
00237      12  ER-0050             PIC  X(4)       VALUE '0050'.
00238      12  ER-0052             PIC  X(4)       VALUE '0052'.
00239      12  ER-0066             PIC  X(4)       VALUE '0066'.
00240      12  ER-0067             PIC  X(4)       VALUE '0067'.
00241      12  ER-0070             PIC  X(4)       VALUE '0070'.
00242      12  ER-0144             PIC  X(4)       VALUE '0144'.
00243      12  ER-0168             PIC  X(4)       VALUE '0168'.
00244      12  ER-0193             PIC  X(4)       VALUE '0193'.
00245      12  ER-0195             PIC  X(4)       VALUE '0195'.
00246      12  ER-0216             PIC  X(4)       VALUE '0216'.
00247      12  ER-0226             PIC  X(4)       VALUE '0226'.
00248      12  ER-0348             PIC  X(4)       VALUE '0348'.
00249      12  ER-0453             PIC  X(4)       VALUE '0453'.
00250      12  ER-0454             PIC  X(4)       VALUE '0454'.
00251      12  ER-0455             PIC  X(4)       VALUE '0455'.
00252      12  ER-0456             PIC  X(4)       VALUE '0456'.
00253      12  ER-0457             PIC  X(4)       VALUE '0457'.
00254      12  ER-0470             PIC  X(4)       VALUE '0470'.
00255      12  ER-0471             PIC  X(4)       VALUE '0471'.
00256      12  ER-0472             PIC  X(4)       VALUE '0472'.
00257      12  ER-0482             PIC  X(4)       VALUE '0482'.
00258      12  ER-0561             PIC  X(4)       VALUE '0561'.
00259      12  ER-0619             PIC  X(4)       VALUE '0619'.
00260      12  ER-0630             PIC  X(4)       VALUE '0630'.
00261      12  ER-0668             PIC  X(4)       VALUE '0668'.
00262      12  ER-0899             PIC  X(4)       VALUE '0899'.
00263      12  ER-1220             PIC  X(4)       VALUE '1220'.
00264      12  ER-1228             PIC  X(4)       VALUE '1228'.
00265      12  ER-1238             PIC  X(4)       VALUE '1238'.
00265      12  ER-1299             PIC  X(4)       VALUE '1299'.
00266      12  ER-1612             PIC  X(4)       VALUE '1612'.
00267      12  ER-2051             PIC  X(4)       VALUE '2051'.
00268      12  ER-2052             PIC  X(4)       VALUE '2052'.
00269      12  ER-2053             PIC  X(4)       VALUE '2053'.
00270      12  ER-2054             PIC  X(4)       VALUE '2054'.
00271      12  ER-2058             PIC  X(4)       VALUE '2058'.
00272      12  ER-2059             PIC  X(4)       VALUE '2059'.
00273      12  ER-2060             PIC  X(4)       VALUE '2060'.
00274      12  ER-2061             PIC  X(4)       VALUE '2061'.
00275      12  ER-2062             PIC  X(4)       VALUE '2062'.
00276      12  ER-2063             PIC  X(4)       VALUE '2063'.
00277      12  ER-2064             PIC  X(4)       VALUE '2064'.
00278      12  ER-2065             PIC  X(4)       VALUE '2065'.
00279      12  ER-2066             PIC  X(4)       VALUE '2066'.
00280      12  ER-2068             PIC  X(4)       VALUE '2068'.
00281      12  ER-2069             PIC  X(4)       VALUE '2069'.
00282      12  ER-2070             PIC  X(4)       VALUE '2070'.
00283      12  ER-2072             PIC  X(4)       VALUE '2072'.
00284      12  ER-2073             PIC  X(4)       VALUE '2073'.
00285      12  ER-2074             PIC  X(4)       VALUE '2074'.
00286      12  ER-2075             PIC  X(4)       VALUE '2075'.
00287      12  ER-2087             PIC  X(4)       VALUE '2087'.
00288      12  ER-2113             PIC  X(4)       VALUE '2113'.
00289      12  ER-2151             PIC  X(4)       VALUE '2151'.
00290      12  ER-2370             PIC  X(4)       VALUE '2370'.
00291      12  ER-2572             PIC  X(4)       VALUE '2572'.
00292      12  ER-3155             PIC  X(4)       VALUE '3155'.
00293      12  ER-3785             PIC  X(4)       VALUE '3785'.
00294      12  ER-9638             PIC  X(4)       VALUE '9638'.
00294      12  ER-9999             PIC  X(4)       VALUE '9999'.
00295
00296  01  TEMP-STORAGE-RECORD.
00297      12  TS-ACCT-KEY     OCCURS  32  TIMES
00298                              INDEXED  BY  TS-INDEX
00299                                  PIC  X(20).
00300      EJECT
00301 *    COPY ELCSCTM.
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
00302      EJECT
00303 *    COPY ELCSCRTY.
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
00304      EJECT
00305 *    COPY ELCLOGOF.
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
00306      EJECT
00307 *    COPY ELCDATE.
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
00308      EJECT
00309 *    COPY ELCATTR.
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
00310      EJECT
00311 *    COPY ELCEMIB.
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
00312      EJECT
00313 *    COPY ELCINTF.
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
00314 *    COPY ELC650PI.
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
00315 *          16  PI-NAMEFLG       PIC X.
00316
00317 *    COPY ELCJPFX.
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
00318                                  PIC  X(2000).
00319  EJECT
00320 *    COPY ELCAID.
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
00321
00322  01  FILLER  REDEFINES  DFHAID.
00323      12  FILLER              PIC  X(8).
00324      12  PF-VALUES           PIC  X      OCCURS  24  TIMES.
00325      12  FILLER              PIC  X(3).
00326  EJECT
00327 *    COPY EL650S.
       01  EL650AI.
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
           05  TRNMSG1L PIC S9(0004) COMP.
           05  TRNMSG1F PIC  X(0001).
           05  FILLER REDEFINES TRNMSG1F.
               10  TRNMSG1A PIC  X(0001).
           05  TRNMSG1I PIC  X(0017).
      *    -------------------------------
           05  LINEL PIC S9(0004) COMP.
           05  LINEF PIC  X(0001).
           05  FILLER REDEFINES LINEF.
               10  LINEA PIC  X(0001).
           05  LINEI PIC  9.
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
           05  AACCTL PIC S9(0004) COMP.
           05  AACCTF PIC  X(0001).
           05  FILLER REDEFINES AACCTF.
               10  AACCTA PIC  X(0001).
           05  AACCTI PIC  X(0010).
      *    -------------------------------
           05  NAMEFLGL PIC S9(0004) COMP.
           05  NAMEFLGF PIC  X(0001).
           05  FILLER REDEFINES NAMEFLGF.
               10  NAMEFLGA PIC  X(0001).
           05  NAMEFLGI PIC  X(0001).
      *    -------------------------------
           05  NAMEL PIC S9(0004) COMP.
           05  NAMEF PIC  X(0001).
           05  FILLER REDEFINES NAMEF.
               10  NAMEA PIC  X(0001).
           05  NAMEI PIC  X(0030).
      *    -------------------------------
           05  ARHDGL PIC S9(0004) COMP.
           05  ARHDGF PIC  X(0001).
           05  FILLER REDEFINES ARHDGF.
               10  ARHDGA PIC  X(0001).
           05  ARHDGI PIC  X(0010).
      *    -------------------------------
           05  DET1L PIC S9(0004) COMP.
           05  DET1F PIC  X(0001).
           05  FILLER REDEFINES DET1F.
               10  DET1A PIC  X(0001).
           05  DET1I PIC  X(0070).
      *    -------------------------------
           05  DET2L PIC S9(0004) COMP.
           05  DET2F PIC  X(0001).
           05  FILLER REDEFINES DET2F.
               10  DET2A PIC  X(0001).
           05  DET2I PIC  X(0070).
      *    -------------------------------
           05  DET3L PIC S9(0004) COMP.
           05  DET3F PIC  X(0001).
           05  FILLER REDEFINES DET3F.
               10  DET3A PIC  X(0001).
           05  DET3I PIC  X(0070).
      *    -------------------------------
           05  DET4L PIC S9(0004) COMP.
           05  DET4F PIC  X(0001).
           05  FILLER REDEFINES DET4F.
               10  DET4A PIC  X(0001).
           05  DET4I PIC  X(0070).
      *    -------------------------------
           05  DET5L PIC S9(0004) COMP.
           05  DET5F PIC  X(0001).
           05  FILLER REDEFINES DET5F.
               10  DET5A PIC  X(0001).
           05  DET5I PIC  X(0070).
      *    -------------------------------
           05  DET6L PIC S9(0004) COMP.
           05  DET6F PIC  X(0001).
           05  FILLER REDEFINES DET6F.
               10  DET6A PIC  X(0001).
           05  DET6I PIC  X(0070).
      *    -------------------------------
           05  DET7L PIC S9(0004) COMP.
           05  DET7F PIC  X(0001).
           05  FILLER REDEFINES DET7F.
               10  DET7A PIC  X(0001).
           05  DET7I PIC  X(0070).
      *    -------------------------------
           05  DET8L PIC S9(0004) COMP.
           05  DET8F PIC  X(0001).
           05  FILLER REDEFINES DET8F.
               10  DET8A PIC  X(0001).
           05  DET8I PIC  X(0070).
      *    -------------------------------
           05  TRNMSG2L PIC S9(0004) COMP.
           05  TRNMSG2F PIC  X(0001).
           05  FILLER REDEFINES TRNMSG2F.
               10  TRNMSG2A PIC  X(0001).
           05  TRNMSG2I PIC  X(0020).
      *    -------------------------------
           05  EFFCHGL PIC S9(0004) COMP.
           05  EFFCHGF PIC  X(0001).
           05  FILLER REDEFINES EFFCHGF.
               10  EFFCHGA PIC  X(0001).
           05  EFFCHGI PIC  X(0008).
      *    -------------------------------
           05  EXPCHGL PIC S9(0004) COMP.
           05  EXPCHGF PIC  X(0001).
           05  FILLER REDEFINES EXPCHGF.
               10  EXPCHGA PIC  X(0001).
           05  EXPCHGI PIC  X(0008).
      *    -------------------------------
           05  TRNMSG3L PIC S9(0004) COMP.
           05  TRNMSG3F PIC  X(0001).
           05  FILLER REDEFINES TRNMSG3F.
               10  TRNMSG3A PIC  X(0001).
           05  TRNMSG3I PIC  X(0003).
      *    -------------------------------
           05  TRNTOL PIC S9(0004) COMP.
           05  TRNTOF PIC  X(0001).
           05  FILLER REDEFINES TRNTOF.
               10  TRNTOA PIC  X(0001).
           05  TRNTOI PIC  X(0019).
      *    -------------------------------
           05  TRNTODTL PIC S9(0004) COMP.
           05  TRNTODTF PIC  X(0001).
           05  FILLER REDEFINES TRNTODTF.
               10  TRNTODTA PIC  X(0001).
           05  TRNTODTI PIC  X(0008).
      *    -------------------------------
           05  TRNMSG4L PIC S9(0004) COMP.
           05  TRNMSG4F PIC  X(0001).
           05  FILLER REDEFINES TRNMSG4F.
               10  TRNMSG4A PIC  X(0001).
           05  TRNMSG4I PIC  X(0005).
      *    -------------------------------
           05  TRNFROML PIC S9(0004) COMP.
           05  TRNFROMF PIC  X(0001).
           05  FILLER REDEFINES TRNFROMF.
               10  TRNFROMA PIC  X(0001).
           05  TRNFROMI PIC  X(0019).
      *    -------------------------------
           05  TRNFRDTL PIC S9(0004) COMP.
           05  TRNFRDTF PIC  X(0001).
           05  FILLER REDEFINES TRNFRDTF.
               10  TRNFRDTA PIC  X(0001).
           05  TRNFRDTI PIC  X(0008).
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
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL650AO REDEFINES EL650AI.
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
           05  TRNMSG1O PIC  X(0017).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINEO PIC  X(0001).
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
           05  AACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAMEFLGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARHDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DET1O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DET2O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DET3O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DET4O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DET5O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DET6O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DET7O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DET8O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNMSG2O PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFCHGO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPCHGO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNMSG3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNTOO PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNTODTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNMSG4O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNFROMO PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNFRDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00328  EJECT
00329  01  EL650R  REDEFINES  EL650AI.
101101     12  FILLER                      PIC  X(153).
00331      12  MAP-RECORD-AREA.
00332          16  DATE-RANGES     OCCURS  8  TIMES
00333                                  INDEXED  BY  M-INDEX.
00334              20  FILLER              PIC  X(3).
00335              20  MAP-EFF-DT          PIC  X(8).
00336              20  FILLER              PIC  X(6).
00337              20  MAP-EXP-DT          PIC  X(8).
00338              20  MAP-EXP-DT-ED  REDEFINES
00339                  MAP-EXP-DT          PIC 99B99B99.
00340              20  FILLER              PIC  X(4).
00341              20  MAP-LST-MAINT-DT    PIC  X(8).
00342              20  FILLER              PIC  X(3).
00343              20  MAP-LOW-CERT        PIC  X(8).
00344              20  FILLER              PIC  X(3).
00345              20  MAP-HIGH-CERT       PIC  X(8).
00346              20  FILLER              PIC  X(4).
00347              20  MAP-AR-DATE         PIC  X(8).
00348              20  FILLER              PIC  XX.
00349      12  FILLER                      PIC  X(286).
00350  EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
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
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
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
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00352  01  DFHCOMMAREA             PIC  X(1500).
00353  EJECT
00354 *01 PARMLIST .
00355 *    12  FILLER              PIC S9(8)   COMP.
00356 *    12  ERACCT-POINTER      PIC S9(8)   COMP.
00357 *    12  ERCOMP-POINTER      PIC S9(8)   COMP.
00358 *    12  ERNAME-POINTER      PIC S9(8)   COMP.
00359 *    12  ERPLAN-POINTER      PIC S9(8)   COMP.
00360 *    12  ELCNTL-POINTER      PIC S9(8)   COMP.
00361 *    12  ERPNDB-POINTER      PIC S9(8)   COMP.
00362 *    12  AXRF-POINTER        PIC S9(8)   COMP.
00363 *    12  AXRF2-POINTER       PIC S9(8)   COMP.
00364 *    12  AXRF3-POINTER       PIC S9(8)   COMP.
00365 *    12  AXRF4-POINTER       PIC S9(8)   COMP.
00366 *    12  AXRF5-POINTER       PIC S9(8)   COMP.
00367 *    12  AXRF6-POINTER       PIC S9(8)   COMP.
00368 *    12  AXRF7-POINTER       PIC S9(8)   COMP.
00369 *    12  AXRF8-POINTER       PIC S9(8)   COMP.
00370  EJECT
00371 *    COPY ERCACCT.
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
00372  EJECT
00373 *    COPY ERCCOMP.
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
00374  EJECT
00375 *    COPY ERCNAME.
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
00376  EJECT
00377 *    COPY ERCPLAN.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPLAN                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT PLAN MASTER FILE                       *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR THE ONLINE VSAM ACCOUNT            *
00010 *   PLAN MASTER                                                  *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER PLAN MASTER           *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 420   RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERPLAN                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = N/A                                    *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
00025
00026  01  PLAN-MASTER.
00027      12  PL-RECORD-ID                      PIC XX.
00028          88  VALID-PL-ID                      VALUE 'PL'.
00029
00030      12  PL-CONTROL-PRIMARY.
00031          16  PL-COMPANY-CD                 PIC X.
00032          16  PL-MSTR-CNTRL.
00033              20  PL-CONTROL-A.
00034                  24  PL-CARRIER            PIC X.
00035                  24  PL-GROUPING           PIC X(6).
00036                  24  PL-STATE              PIC XX.
00037                  24  PL-ACCOUNT            PIC X(10).
00038              20  PL-BENEFIT-TYPE           PIC X.
00039              20  PL-BENEFIT-CODE           PIC XX.
00040              20  PL-REVISION-NO            PIC X(3).
00041
00042      12  PL-CONTROL-BY-VAR-GRP.
00043          16  PL-COMPANY-CD-A1              PIC X.
00044          16  PL-VG-CARRIER                 PIC X.
00045          16  PL-VG-GROUPING                PIC X(6).
00046          16  PL-VG-STATE                   PIC XX.
00047          16  PL-VG-ACCOUNT                 PIC X(10).
00048          16  PL-BENEFIT-TYPE-A1            PIC X.
00049          16  PL-BENEFIT-CODE-A1            PIC XX.
00050          16  PL-REVISION-NO-A1             PIC 999.
00051
00052      12  PL-MAINT-INFORMATION.
00053          16  PL-LAST-MAINT-DT              PIC XX.
00054          16  PL-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00055          16  PL-LAST-MAINT-USER            PIC X(4).
00056          16  FILLER                        PIC XX.
00057
00058      12  PL-LIABILITY-LIMITS.
00059          16  PL-ATT-AGE                    PIC S99        COMP-3.
00060          16  PL-L-LIMITS        OCCURS 8 TIMES.
00061              20  PL-LM-AGE                 PIC S99        COMP-3.
00062              20  PL-LM-DUR                 PIC S999       COMP-3.
00063              20  PL-LM-MOA                 PIC S9(4)      COMP-3.
00064              20  PL-LM-AMT                 PIC S9(6)      COMP-3.
00065
00066      12  FILLER                            PIC X(30).
00067
00068      12  PL-GL-ACCOUNT-NOS.
00069          16  PL-PREMIUM-GL                 PIC X(8).
00070          16  PL-COMM-GL                    PIC X(8).
00071          16  PL-CLAIM-GL                   PIC X(8).
00072      12  FILLER                            PIC X(24).
00073      12  PL-TOLERANCES.
00074          16  PL-TOL-PREM-AMT               PIC S999V99    COMP-3.
00075          16  PL-TOL-REF-AMT                PIC S999V99    COMP-3.
00076          16  PL-TOL-CLM-AMT                PIC S999V99    COMP-3.
00077          16  PL-TOL-PREM-PCT               PIC S9V9999    COMP-3.
00078          16  PL-TOL-REF-PCT                PIC S9V9999    COMP-3.
00079          16  PL-TOL-CLM-PCT                PIC S9V9999    COMP-3.
00080      12  PL-OVER-SHORT.
00081          16  PL-OVER-SHORT-AMT             PIC S999V99    COMP-3.
00082          16  PL-OVER-SHORT-PCT             PIC S9V9(4)    COMP-3.
00083      12  FILLER                            PIC X(24).
00084      12  PLAN-MISC.
00085          16  PL-POLICY-FEE                 PIC S9(3)V99   COMP-3.
00086          16  PL-STATE-TAX                  PIC S9V9999    COMP-3.
00087          16  PL-IG                         PIC X.
00088          16  PL-RETRO-RET                  PIC S9V9999    COMP-3.
00089          16  PL-POLICY-FORM                PIC X(12).
00090          16  PL-EDIT-FOR-FORM              PIC X(01).
00091          16  PL-DEV-CODE                   PIC XXX.
00092          16  PL-DEV-PCT                    PIC S9V9(6)    COMP-3.
00093          16  PL-CALC-METHOD                PIC X.
00094          16  PL-BENEFIT-GROUP              PIC X(05).
00095          16  PL-SALES-TAX                  PIC S9V9999    COMP-3.
00096
00097          16  FILLER                        PIC X(99).
00098 ******************************************************************
00378  EJECT
00379 *    COPY ELCCNTL.
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
012913         16  FILLER                         PIC X(181).
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
00380  EJECT
00381 *    COPY ERCPNDB.
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
072308         16  PB-C-NH-INT-ON-REFS          PIC S9(7)V99   COMP-3.
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
00382 *EJECT
00383 *    COPY ERCGXRF.
00384  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ACCOUNT-MASTER
                                COMPENSATION-MASTER
                                NAME-LOOKUP-MASTER PLAN-MASTER
                                CONTROL-FILE PENDING-BUSINESS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL650' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00386
00387      MOVE LOW-VALUES             TO  WS-VERIFY-AREA.
00388      INITIALIZE  TEMP-STORAGE-RECORD.
00389
00390      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00391      MOVE '5'                    TO  DC-OPTION-CODE.
00392
00393      PERFORM 9700-DATE-CONVERSION.
00394
00395      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00396      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00397      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00398      MOVE 2                      TO  EMI-NUMBER-OF-LINES.
pemcp *    display ' beginning of program el650   '
00399
00400      IF EIBCALEN  =  0
00401          GO TO 8800-UNAUTHORIZED-ACCESS.
00402
00403      MOVE SCREEN-NUMBER          TO  TS-SCREEN.
00404      MOVE EIBTRMID               TO  TS-TERM.
00405      MOVE LOW-VALUES             TO  EL650AI.
00406
00407      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00408          MOVE PI-CALLING-PROGRAM        TO  RETURNED-FROM.
00409
00410      IF PI-CALLING-PROGRAM  NOT = THIS-PGM
00411          IF PI-RETURN-TO-PROGRAM  NOT = THIS-PGM
00412              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00413              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00414              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00415              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00416              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00417              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00418              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00419              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00420              MOVE LOW-VALUES            TO  PI-PROGRAM-WORK-AREA
00421              MOVE ZEROS                 TO  PI-TOTAL-LINES
00422                                             PI-LINE-SELECTED
00423              MOVE SPACE                 TO  PI-RECORD-ADDED-SW
00424              MOVE 1                     TO  PI-PAGE-NUMBER
00425          ELSE
00426              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00427              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00428              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00429              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00430              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00431              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00432              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00433              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00434
00435 **************************************************************
00436 *               CLIENT "DMD" CUSTOM CODE
00437 * CHECK IF CONTROL PASSED FROM EL608 PROGRAM
00438 * START BROWSE BEGINNING WITH KEY PASSED BY EL608
00439 *              CSO CR# 6475
00440      IF EIBTRNID  =  EL608-TRANS-ID
00441          MOVE 1                     TO RETURN-FROM-SW
00442          MOVE 'Y'                   TO FROM-EL608-SW
00443          MOVE TRANS-ID              TO EIBTRNID
00444          MOVE XCTL-EL601            TO PI-RETURN-TO-PROGRAM
00445          MOVE PI-PREV-VG-ACCOUNT    TO PI-ACCT-CCGSA-KEY
00446                                        KEY-SAVE
00447          MOVE LOW-VALUES            TO PI-ACCT-EXP-DT
00448                                        PI-ACCT-REST-OF-EXP
00449                                        PI-DATE-RANGE-TABLE
00450          
      * EXEC CICS  HANDLE CONDITION
00451 *            NOTFND   (5340-END)
00452 *            ENDFILE  (5330-NOT-FOUND)
00453 *        END-EXEC
      *    MOVE '"$I''                  ! " #00004900' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034393030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00454          MOVE 'Y'                   TO FIRST-TIME-SW
00455          MOVE ZEROS                 TO PI-TOTAL-LINES
00456          SET T-INDEX
00457              TS-INDEX  TO  +1
00458          MOVE ACCT-FILE-ID          TO PI-ACCT-ID
00459          MOVE 1                     TO PI-PAGE-NUMBER
00460          MOVE 'S'                   TO PI-MAINT
00461          MOVE DFHPF1                TO EIBAID
00462          GO TO 1002-SHOW-NEW-RECORDS.
00463 *    END OF CLIENT "DMD" CUSTOM CODE
00464 **************************************************************
00465
00466      IF RETURNED-FROM = XCTL-6501 OR XCTL-6502 OR XCTL-6503 OR
00467                         XCTL-6504 OR XCTL-6505 OR XCTL-6506 OR
00468                         XCTL-6508
00469          MOVE 1                     TO  RETURN-FROM-SW
00470          MOVE PI-PREV-VG-ACCOUNT    TO  PI-ACCT-CCGSA-KEY
00471          MOVE PI-ACCT-CCGSA-KEY     TO  KEY-SAVE
00472          MOVE LOW-VALUES            TO  PI-ACCT-EXP-DT
00473          GO TO 1002-SHOW-NEW-RECORDS.
00474
00475  EJECT
00476      IF EIBTRNID  NOT =  TRANS-ID
00477          IF EIBTRNID  NOT = EL640-TRANS-ID  AND
00478             EIBTRNID  NOT = EL658-TRANS-ID  AND
00479             EIBTRNID  NOT = EL6311-TRANS-ID AND
020816            EIBTRNID  NOT = VP6311-TRANS-ID AND
CIDMOD            EIBTRNID  NOT = EL1273-TRANS-ID AND
CIDMOD            EIBTRNID  NOT = EL652-TRANS-ID  AND
100703            EIBTRNID  NOT = EL6523-TRANS-ID AND
00480             EIBTRNID  NOT = EL6311NC-TRANS-ID AND
00481             EIBTRNID  NOT = EL130-TRANS-ID  AND
00482             EIBTRNID  NOT = EL6592-TRANS-ID AND
00483             EIBTRNID  NOT = EL6509-TRANS-ID
110706            AND EIBTRNID NOT = EL6503-TRANS-ID
00484              GO TO 8100-SEND-INITIAL-MAP
00485          ELSE
00486              IF PI-CR-CONTROL-IN-PROGRESS  = SPACES
00487                  GO TO 8100-SEND-INITIAL-MAP
00488              ELSE
00489                  MOVE 'S'             TO  MAINTI
00490                  MOVE PI-CR-CARRIER   TO  ACARIERI
00491                  MOVE PI-CR-GROUPING  TO  AGROUPI
00492                  MOVE PI-CR-STATE     TO  ASTATEI
00493                  MOVE PI-CR-ACCOUNT   TO  AACCTI
00494                  MOVE +1              TO  ACARIERL
00495                                           MAINTL
00496                  MOVE +6              TO  AGROUPL
00497                  MOVE +2              TO  ASTATEL
00498                  MOVE +10             TO  AACCTL
00499                  GO TO 0330-EDIT-DATA.
00500
00501      
      * EXEC CICS  HANDLE CONDITION
00502 *        PGMIDERR  (9600-PGMID-ERROR)
00503 *        ERROR     (9990-ABEND)
00504 *    END-EXEC.
      *    MOVE '"$L.                  ! # #00004956' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034393536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00505
00506      IF EIBAID  = DFHCLEAR
00507          GO TO 9400-CLEAR.
00508
00509      IF PI-PROCESSOR-ID  =  'LGXX'
00510          GO TO 0200-RECEIVE.
00511
00512      
      * EXEC CICS  READQ TS
00513 *        QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00514 *        INTO    (SECURITY-CONTROL)
00515 *        LENGTH  (SC-COMM-LENGTH)
00516 *        ITEM    (SC-ITEM)
00517 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00004967' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00518
00519      MOVE SC-CREDIT-DISPLAY (04)  TO  PI-DISPLAY-CAP.
00520      MOVE SC-CREDIT-UPDATE  (04)  TO  PI-MODIFY-CAP.
00521
00522      IF NOT DISPLAY-CAP
00523          MOVE 'READ'             TO  SM-READ
00524          PERFORM 9995-SECURITY-VIOLATION  THRU  9995-EXIT
00525          MOVE ER-0070            TO  EMI-ERROR
00526          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00527          GO TO 8100-SEND-INITIAL-MAP.
00528  EJECT
00529  0200-RECEIVE.
00530      IF EIBAID  = DFHPA1  OR  DFHPA2  OR  DFHPA3
00531          MOVE ER-0008            TO  EMI-ERROR
00532          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00533          MOVE -1                 TO  MAINTL
00534          GO TO 8200-SEND-DATAONLY.
00535
00536      
      * EXEC CICS  RECEIVE
00537 *        MAP     (MAP-NAME)
00538 *        MAPSET  (MAPSET-NAME)
00539 *        INTO    (EL650AI)
00540 *    END-EXEC.
           MOVE LENGTH OF
            EL650AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004991' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL650AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00541
00542      IF PFENTERL  = ZERO
00543          GO TO 0300-CHECK-PFKEYS.
00544
00545      IF EIBAID  NOT = DFHENTER
00546          MOVE ER-0004            TO  EMI-ERROR
00547          GO TO 0320-INPUT-ERROR.
00548
00549      IF (PFENTERI NUMERIC)
00550        AND  (PFENTERI GREATER ZERO AND LESS 25)
00551          MOVE PF-VALUES (PFENTERI)  TO  EIBAID
00552      ELSE
00553          MOVE ER-0029               TO  EMI-ERROR
00554          GO TO 0320-INPUT-ERROR.
00555
00556  0300-CHECK-PFKEYS.
00557      IF EIBAID  = DFHPF23
00558          GO TO 8810-PF23.
00559
00560      IF EIBAID  = DFHPF24
00561          GO TO 9200-RETURN-MAIN-MENU.
00562
00563      IF EIBAID  = DFHPF12
00564          GO TO 9500-PF12.
00565
00566      IF MAINTL  NOT = ZERO   AND
00567         MAINTI  NOT = SPACE  AND
00568         EIBAID  NOT = DFHENTER
00569           MOVE ER-0050            TO  EMI-ERROR
00570           GO TO 0320-INPUT-ERROR.
00571
pemtst     if (maintl not = zeros)
pemtst        and (mainti not = 'D')
pemtst        move spaces              to pi-el650-del-sw
pemtst     end-if
00572      IF EIBAID  = DFHPF1
00573          GO TO 5300-FIND-NEXT-ACCOUNT.
00574
00575      IF EIBAID  = DFHPF2
00576          GO TO 5400-FIND-PREV-ACCOUNT.
00577
00578      IF EIBAID  = DFHPF3
00579          GO TO 5100-FIND-NEXT-RANGE.
00580
00581      IF EIBAID  = DFHPF4
00582          GO TO 5200-FIND-PREV-RANGE.
00583
00584      IF EIBAID  = DFHPF5
00585          GO TO 8900-PF5.
00586
110706     IF EIBAID  = DFHPF6
110706        GO TO 8910-PF6
110706     END-IF
00587      IF EIBAID  = DFHENTER
00588          GO TO 0330-EDIT-DATA.
00589
00590      MOVE ER-0029                TO  EMI-ERROR.
00591
00592  0320-INPUT-ERROR.
00593      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00594
00595      MOVE AL-UNBON               TO  PFENTERA.
00596
00597      IF PFENTERL  = ZERO
00598          MOVE -1                 TO  MAINTL
00599      ELSE
00600          MOVE -1                 TO  PFENTERL.
00601
00602      GO TO 8200-SEND-DATAONLY.
00603  EJECT
00604  0330-EDIT-DATA.
00605      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00606      MOVE '5'                    TO  DC-OPTION-CODE.
00607
00608      PERFORM 9700-DATE-CONVERSION.
00609
00610      MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE.
00611      MOVE DC-GREG-DATE-1-YMD     TO  CURRENT-SAVE.
00612
00613      IF ACARIERL GREATER ZEROS
00614          IF PI-CARRIER-SECURITY GREATER SPACES
00615              IF ACARIERI = PI-CARRIER-SECURITY
00616                  NEXT SENTENCE
00617              ELSE
00618                  MOVE -1         TO  ACARIERL
00619                  MOVE ER-2370    TO  EMI-ERROR
00620                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00621                  MOVE AL-UABON   TO  ACARIERA
00622                  GO TO 8200-SEND-DATAONLY.
00623
00624      IF MAINTI  = 'S'
00625        IF LINEL = ZEROS
00626            MOVE 1 TO PI-PAGE-NUMBER
00627            GO TO 1000-SHOW-ACCOUNT
00628        ELSE
00629            GO TO 1000-SHOW-ACCOUNT.
00630
00631      PERFORM 6800-COMPANY-REC-READ  THRU  6899-EXIT.
00632
00633      IF EMI-ERROR  NOT = ZEROS
00634          MOVE -1                 TO  MAINTL
00635          GO TO 8200-SEND-DATAONLY.
00636
00637      IF NOT  MODIFY-CAP
00638          MOVE 'UPDATE'           TO  SM-READ
00639          PERFORM 9995-SECURITY-VIOLATION  THRU  9995-EXIT
00640          MOVE ER-0070            TO  EMI-ERROR
00641          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00642          MOVE AL-UANON           TO  ACARIERA  AGROUPA
00643                                      ASTATEA   AACCTA
00644          GO TO 8100-SEND-INITIAL-MAP.
00645
00646      IF MAINTI  = 'A'
00647          GO TO 1500-ADD-ACCOUNT.
00648
00649      IF MAINTI  = 'C'
00650          IF PI-SV-MAINT IS EQUAL TO 'L'
00651             MOVE -1              TO  MAINTL
00652             MOVE AL-UABON        TO  MAINTA
00653             MOVE ER-0899         TO  EMI-ERROR
00654             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00655             GO TO 8200-SEND-DATAONLY
00656          ELSE
00657             GO TO 2000-CHANGE-ACCOUNT.
00658
00659      IF MAINTI  = 'D'
00660          GO TO 2500-DELETE-ACCOUNT.
00661
00662      IF MAINTI  = 'X'
00663          GO TO 3000-CHANGE-EXPIRATION.
00664
00665      IF MAINTI  = 'E'
00666          GO TO 3500-CHANGE-EFFECTIVE.
00667
00668      IF MAINTI  = 'L'
00669          MOVE 'L'             TO  PI-SV-MAINT
00670          GO TO 4000-CHANGE-ALL.
00671
00672      IF MAINTI  = 'K'
00673        IF LINEL GREATER ZERO
00674            GO TO 4900-CREATE-NEW-ACCOUNT
00675         ELSE
00676            GO TO 4500-CREATE-NEW-ACCOUNT.
00677
00678      IF PI-COMPANY-ID  =  'LGX' OR 'MIC' OR 'MCC'
00679          IF MAINTI  = 'T'
00680              GO TO 4500-CREATE-NEW-ACCOUNT.
00681
00682      IF MAINTI  = 'N'
00683          GO TO 5000-CREATE-NEW-RANGE.
00684
00685      MOVE ER-0023                TO  EMI-ERROR.
00686
00687      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00688
00689      MOVE -1                     TO  MAINTL.
00690      MOVE AL-UABON               TO  MAINTA.
00691
00692      GO TO 8200-SEND-DATAONLY.
00693  EJECT
00694  1000-SHOW-ACCOUNT.
00695      PERFORM 6100-BUILD-KEY  THRU  6199-EXIT.
00696
00697      IF LINEL GREATER ZEROS   AND
00698         PI-TOTAL-LINES  NOT =  ZEROS
00699          NEXT SENTENCE
00700      ELSE
00701          GO TO 1002-SHOW-NEW-RECORDS.
00702
00703      PERFORM 6000-EDIT-LINE  THRU  6000-LIN-EXIT.
00704
00705      IF EMI-ERROR  NOT = ZEROS
00706          GO TO 8200-SEND-DATAONLY.
00707
00708      SET T-INDEX                 TO  LINEI.
00709
00710      IF PI-2ND-PAGE
00711          SET T-INDEX  UP  BY  8
00712      ELSE
00713      IF PI-3RD-PAGE
00714          SET T-INDEX  UP  BY  16
00715      ELSE
00716      IF PI-LST-PAGE
00717          SET T-INDEX  UP  BY  24.
00718
00719      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT.
00720      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
00721
00722      IF PI-PREV-ACCOUNT  =  PI-ACCT-CCGSA-KEY
00723          MOVE MAINTI             TO  PI-MAINT
00724          MOVE XCTL-6501          TO  PGM-NAME
00725          GO TO 9300-XCTL
00726      ELSE
00727          IF PI-PREV-VG-ACCOUNT = PI-ACCT-CCGSA-KEY
00728              MOVE MAINTI         TO  PI-MAINT
00729              MOVE XCTL-6501      TO  PGM-NAME
00730              PERFORM 7400-READ-TEMP-STORAGE  THRU  7400-EXIT
00731              SET TS-INDEX        TO  T-INDEX
00732              MOVE TS-ACCT-KEY (TS-INDEX)
00733                                  TO  PI-ACCT-CCGSA-KEY
00734              GO TO 9300-XCTL.
00735
00736  1002-SHOW-NEW-RECORDS.
00737      
      * EXEC CICS  HANDLE CONDITION
00738 *         NOTFND   (1010-NOT-FOUND)
00739 *         ENDFILE  (1010-NOT-FOUND)
00740 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00005199' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035313939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00741
00742      SET T-INDEX
00743         TS-INDEX  TO  +1.
00744
00745      MOVE PI-ACCT-CCGSA-KEY      TO  KEY-SAVE.
00746
00747      MOVE LOW-VALUES             TO  TEMP-STORAGE-RECORD.
00748
00749      MOVE ZEROS                  TO  PI-TOTAL-LINES.
00750
00751      PERFORM 7100-START-BROWSE  THRU  7100-EXIT.
00752
00753      MOVE 'Y'                    TO  BROWSE-STARTED-SW.
00754
00755  1005-READ-ACCT.
00756      PERFORM 6310-READNEXT  THRU  6310-EXIT.
00757
00758      IF K-WITH-LINE-NO
00759        IF SV-CARRIER   NOT = AM-CARRIER  OR
00760           SV-GROUPING  NOT = AM-GROUPING OR
00761           SV-STATE     NOT = AM-STATE    OR
00762           SV-ACCOUNT   NOT = AM-ACCOUNT
00763             GO TO 1010-NOT-FOUND.
00764
00765      IF SV-CO  NOT = AM-COMPANY-CD
00766          GO TO 1010-NOT-FOUND.
00767
00768      IF SV-ACCOUNT  NOT =  AM-ACCOUNT
00769          GO TO 1010-NOT-FOUND.
00770
00771      IF SV-STATE  NOT = AM-STATE
00772        AND  (ST-ACCNT-CNTL
00773        OR  CARR-ST-ACCNT-CNTL
00774        OR  CARR-GROUP-ST-ACCNT-CNTL)
00775          GO TO 1010-NOT-FOUND.
00776
00777      IF SV-CARRIER  NOT =  AM-CARRIER
00778        AND  (CARR-ACCNT-CNTL
00779        OR  CARR-ST-ACCNT-CNTL
00780        OR  CARR-GROUP-ST-ACCNT-CNTL)
00781          GO TO 1010-NOT-FOUND.
00782
00783      IF SV-GROUPING  NOT = AM-GROUPING
00784        AND  CARR-GROUP-ST-ACCNT-CNTL
00785          GO TO 1010-NOT-FOUND.
00786
00787      IF AM-TRNTO-DTE  =  SPACES
00788          MOVE LOW-VALUES         TO  AM-TRNTO-DTE.
00789
00790      IF AM-TRNFROM-DTE  = SPACES
00791          MOVE LOW-VALUES         TO  AM-TRNFROM-DTE.
00792
00793      IF T-INDEX  = 1
00794          IF PI-COMPANY-ID  =  'LGX' OR 'MIC' OR 'MCC'
00795              MOVE AM-TRANSFERRED-TO  TO  TRNTOO
00796              MOVE AM-TRNTO-DTE       TO  DC-BIN-DATE-1
00797              MOVE SPACE              TO  DC-OPTION-CODE
00798              PERFORM 9700-DATE-CONVERSION
00799              IF DATE-CONVERSION-ERROR
00800                  MOVE SPACES         TO  TRNTODTO
00801              ELSE
00802                  MOVE DC-GREG-DATE-1-EDIT
00803                                      TO  TRNTODTO.
00804
00805      IF T-INDEX  =  1
00806          IF PI-COMPANY-ID  =  'LGX' OR 'MIC' OR 'MCC'
00807              MOVE AM-TRANSFERRED-FROM  TO  TRNFROMO
00808              MOVE AM-TRNFROM-DTE       TO  DC-BIN-DATE-1
00809              MOVE SPACE                TO  DC-OPTION-CODE
00810              PERFORM 9700-DATE-CONVERSION
00811              IF DATE-CONVERSION-ERROR
00812                  MOVE SPACES           TO  TRNFRDTO
00813              ELSE
00814                  MOVE DC-GREG-DATE-1-EDIT
00815                                        TO  TRNFRDTO.
00816
00817      IF T-INDEX  = 1
00818          MOVE AM-CONTROL-PRIMARY  TO  PI-PREV-ACCOUNT
00819          MOVE PI-ACCT-CCGSA-KEY   TO  PI-PREV-VG-ACCOUNT
00820          MOVE AM-NAME             TO  NAMEO
00821                                       PI-ACCNAME
00822          MOVE SPACE               TO  NAMEFLGO
00823                                       PI-NAMEFLG.
00824
00825      IF AM-NAME  NOT = NAMEO
00826          MOVE '*'                TO  NAMEFLGO
00827                                      PI-NAMEFLG.
00828
00829      PERFORM 1020-MOVE-TO-TABLE  THRU  1020-EXIT.
00830
00831      GO TO 1005-READ-ACCT.
00832
00833  1010-NOT-FOUND.
00834      IF BROWSE-STARTED
00835          
      * EXEC CICS  ENDBR
00836 *            DATASET  (PI-ACCT-ID)
00837 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005297' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00838
00839      IF T-INDEX  NOT = 1
00840          PERFORM 7200-WRITE-TEMP-STORAGE  THRU  7200-EXIT
00841          GO TO 7000-BUILD-OUTPUT-MAP.
00842
00843      MOVE ER-0226                TO  EMI-ERROR.
00844
00845      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00846
00847      MOVE -1                     TO  ACARIERL.
00848      MOVE AL-UABON               TO  ACARIERA
00849                                      AGROUPA
00850                                      ASTATEA
00851                                      AACCTA.
00852
00853      IF FROM-EL608-SW = 'Y'
00854          MOVE SV-CARRIER     TO  ACARIERO
00855          MOVE SV-GROUPING    TO  AGROUPO
00856          MOVE SV-STATE       TO  ASTATEO
00857          MOVE SV-ACCOUNT     TO  AACCTO
00858          MOVE 'N'            TO  FROM-EL608-SW.
00859
00860      IF PI-MAINT = 'A'
00861          IF PI-RECORD-NOT-CREATED
00862              MOVE SV-CARRIER     TO  ACARIERO
00863              MOVE SV-GROUPING    TO  AGROUPO
00864              MOVE SV-STATE       TO  ASTATEO
00865              MOVE SV-ACCOUNT     TO  AACCTO
00866              MOVE ER-2151        TO  EMI-ERROR
00867              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00868              MOVE 'S'            TO  PI-MAINT.
00869
00870      GO TO 8100-SEND-INITIAL-MAP.
00871  EJECT
00872  1020-MOVE-TO-TABLE.
00873      MOVE AM-EXPIRATION-DT       TO  PI-BIN-EXP-DT   (T-INDEX).
00874      MOVE AM-EFFECTIVE-DT        TO  PI-BIN-EFF-DT   (T-INDEX).
00875      MOVE AM-LAST-MAINT-DT       TO  PI-BIN-MAINT-DT (T-INDEX).
00876      MOVE AM-HI-CERT-DATE        TO  DC-GREG-DATE-1-YMD.
00877      MOVE '3'                    TO  DC-OPTION-CODE.
00878
00879      PERFORM 9700-DATE-CONVERSION.
00880
00881      IF DATE-CONVERSION-ERROR
00882          MOVE LOW-VALUES         TO  PI-BIN-HI-CERT (T-INDEX)
00883      ELSE
00884          MOVE DC-BIN-DATE-1      TO  PI-BIN-HI-CERT (T-INDEX).
00885
00886      MOVE AM-LO-CERT-DATE        TO  DC-GREG-DATE-1-YMD.
00887      MOVE '3'                    TO  DC-OPTION-CODE.
00888
00889      PERFORM 9700-DATE-CONVERSION.
00890
00891      IF DATE-CONVERSION-ERROR
00892          MOVE LOW-VALUES         TO  PI-BIN-LO-CERT (T-INDEX)
00893      ELSE
00894          MOVE DC-BIN-DATE-1      TO  PI-BIN-LO-CERT (T-INDEX).
00895
00896      MOVE AM-AR-HI-CERT-DATE     TO  PI-BIN-AR-HI-CERT (T-INDEX).
00897      MOVE AM-CONTROL-PRIMARY     TO  TS-ACCT-KEY (TS-INDEX).
00898
00899      ADD 1                       TO  PI-TOTAL-LINES.
00900
00901      SET T-INDEX
00902         TS-INDEX  UP  BY  +1.
00903
00904      IF T-INDEX  = +33
00905          GO TO 7000-BUILD-OUTPUT-MAP.
00906
00907
00908  1020-EXIT.
00909      EXIT.
00910  EJECT
00911  1500-ADD-ACCOUNT.
00912      PERFORM 6100-BUILD-KEY             THRU  6199-EXIT.
00913
00914      MOVE PI-ACCT-CCGSA-KEY      TO  NEW-KEY-SAVE.
00915
00916      MOVE LOW-VALUES             TO  PI-ACCT-EXP-DT
00917                                      PI-ACCT-REST-OF-EXP.
00918
00919      PERFORM 6500-READ-ACCT  THRU  6599-EXIT.
00920
00921      IF REC-FOUND
00922          MOVE -1                 TO  ACARIERL
00923          MOVE ER-0561            TO  EMI-ERROR
00924          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00925          GO TO 8200-SEND-DATAONLY.
00926
00927      MOVE NEW-KEY-SAVE           TO PI-ACCT-CCGSA-KEY.
00928
00929      PERFORM 6000-EDIT-EFFECTIVE-DATE   THRU  6000-EFF-EXIT.
00930
00931      PERFORM 6000-EDIT-EXPIRATION-DATE  THRU  6000-EXP-EXIT.
00932
00933      PERFORM 6700-VERIFY-STATE-CARRIER  THRU  6799-EXIT.
00934
00935      IF BIN-EXPCHG-SAVE NOT GREATER BIN-EFFCHG-SAVE
00936          MOVE -1                 TO  EXPCHGL
00937          MOVE ER-1228            TO  EMI-ERROR
00938          MOVE AL-UABON           TO  EXPCHGA
00939          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00940
00941      IF NOT EMI-NO-ERRORS
00942          GO TO 8200-SEND-DATAONLY.
00943
00944      PERFORM 4100-BUILD-VERIFICATION  THRU  4199-EXIT.
00945
00946      IF WS-ACCT2-FOUND  = 'Y'
111513         PERFORM 4200-CHECK-VALIDITY  THRU  4200-EXIT
00948              VARYING  WS-INDEX  FROM  +1  BY  +1
00949                 UNTIL WS-INDEX GREATER +32.
00950
00951      IF NOT EMI-NO-ERRORS
00952          GO TO 8200-SEND-DATAONLY.
00953
00954      MOVE PI-ACCT-CCGSA-KEY      TO  NEW-KEY-SAVE.
00955
00956      MOVE NEW-KEY-SAVE           TO  PI-ACCT-CCGSA-KEY
00957                                      PI-PREV-ACCOUNT
00958                                      PI-PREV-VG-ACCOUNT.
00959      MOVE LOW-VALUES             TO  PI-DATE-RANGE-TABLE
00960                                      PI-ACCT-REST-OF-EXP.
00961      MOVE BIN-EXPCHG-SAVE        TO  PI-ACCT-EXP-DT
00962                                      PI-BIN-EXP-DT (1)
00963      MOVE BIN-EFFCHG-SAVE        TO  PI-BIN-EFF-DT (1).
00964      MOVE 1                      TO  PI-TOTAL-LINES
00965                                      PI-PAGE-NUMBER
00966                                      PI-LINE-SELECTED.
00967      MOVE SPACE                  TO  PI-RECORD-ADDED-SW.
00968      MOVE MAINTI                 TO  PI-MAINT.
00969      MOVE XCTL-6501              TO  PGM-NAME.
00970
00971      GO TO 9300-XCTL.
00972  EJECT
00973  2000-CHANGE-ACCOUNT.
00974      PERFORM 6100-BUILD-KEY  THRU  6199-EXIT.
00975
00976      PERFORM 6200-CHECK-KEY  THRU  6200-EXIT.
00977
00978      PERFORM 6000-EDIT-LINE  THRU  6000-LIN-EXIT.
00979
00980      IF NOT EMI-NO-ERRORS
00981          GO TO 8200-SEND-DATAONLY.
00982
00983      SET T-INDEX TO LINEI.
00984
00985      IF PI-2ND-PAGE
00986          SET T-INDEX  UP  BY  8
00987      ELSE
00988      IF PI-3RD-PAGE
00989          SET T-INDEX  UP  BY  16
00990      ELSE
00991      IF PI-LST-PAGE
00992          SET T-INDEX  UP  BY  24.
00993
00994      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT.
00995      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
00996      MOVE MAINTI                   TO  PI-MAINT.
00997      MOVE XCTL-6501                TO  PGM-NAME.
00998
00999      GO TO 9300-XCTL.
01000  EJECT
01001  2500-DELETE-ACCOUNT.
01002      PERFORM 6100-BUILD-KEY  THRU  6199-EXIT.
01003
01004      PERFORM 6200-CHECK-KEY  THRU  6200-EXIT.
01005
01006      PERFORM 6000-EDIT-LINE  THRU  6000-LIN-EXIT.
01007
01008      IF NOT EMI-NO-ERRORS
01009          GO TO 8200-SEND-DATAONLY.
01010
01011      SET T-INDEX                 TO  LINEI.
01012
01013      IF PI-2ND-PAGE
01014          SET T-INDEX  UP  BY  8
01015      ELSE
01016      IF PI-3RD-PAGE
01017          SET T-INDEX  UP  BY  16
01018      ELSE
01019      IF PI-LST-PAGE
01020          SET T-INDEX  UP  BY  24.
01021
01022      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT
01023                                        BIN-MAX-DATE.
01024      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
01025      MOVE PI-BIN-EFF-DT (T-INDEX)  TO  BIN-MIN-DATE.
01026      MOVE '1'                      TO  FROM-WHERE-SW.
01027
01028      PERFORM 2550-CHECK-HI-LO-DATES  THRU  2550-EXIT
01029        VARYING  T-INDEX  FROM  +1  BY  +1
01030          UNTIL  T-INDEX GREATER PI-TOTAL-LINES.
01031
01032      MOVE SPACE                  TO  FROM-WHERE-SW.
01033
01034      IF NOT EMI-NO-ERRORS
01035          GO TO 8200-SEND-DATAONLY.
01036
01037      SET T-INDEX TO LINEI.
01038
01039      IF PI-2ND-PAGE
01040          SET T-INDEX  UP  BY  8
01041      ELSE
01042      IF PI-3RD-PAGE
01043          SET T-INDEX  UP  BY  16
01044      ELSE
01045      IF PI-LST-PAGE
01046          SET T-INDEX  UP  BY  24.
01047
01048      
      * EXEC CICS  READ
01049 *        UPDATE
01050 *        DATASET  (PI-ACCT-ID)
01051 *        SET      (ADDRESS OF ACCOUNT-MASTER)
01052 *        RIDFLD   (PI-ACCT-KEY)
01053 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005510' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01054
01055      IF PI-BIN-LO-CERT (T-INDEX)  NOT =  LOW-VALUES  OR
01056         PI-BIN-HI-CERT (T-INDEX)  NOT =  LOW-VALUES
01057          MOVE ER-2070            TO  EMI-ERROR
01058          MOVE -1                 TO  MAINTL
01059          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01060          GO TO 8200-SEND-DATAONLY.
01061
01062      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
01063          MOVE 'N'                TO  PENDING-BUSINESS-SW
01064      ELSE
01065          PERFORM 2560-CHECK-PENDING-BUSINESS  THRU  2569-EXIT.
01066
01067      IF THERE-IS-PENDING-BUSINESS
01068          MOVE ER-1612            TO  EMI-ERROR
01069          MOVE -1                 TO  MAINTL
01070          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01071          GO TO 8200-SEND-DATAONLY.
01072
01073      MOVE AM-CONTROL-PRIMARY     TO  B4-ACCT-KEY.
01074      MOVE AM-EFFECTIVE-DT        TO  B4-ACCT-EFF-DT.
01075
052716     if pi-el650-del-sw = 'Y'
052716        move ' '                 to pi-el650-del-sw
052716     else
052716        move 'Y'                 to pi-el650-del-sw
052716        move er-1299             to emi-error
052716        perform 9900-error-format thru 9900-exit
052716*       MOVE 1 TO PI-PAGE-NUMBER
052716        move spaces              to mainto
052716        MOVE -1                  TO MAINTL
052716        GO TO 1000-SHOW-ACCOUNT
052716     end-if
01076      PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT
01077
01078 ****** DO NOT DELETE PLAN MASTER RECORDS IF MORE ACCT DATE RANGES
01079      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
01080          NEXT SENTENCE
01081      ELSE
01082          IF PI-TOTAL-LINES  = 1
01083              PERFORM 4800-DELETE-PLAN-REC   THRU  4899-EXIT
               end-if
           end-if
01084
01085      MOVE 'D'                    TO  JP-RECORD-TYPE
01086      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA
01087
01088      
      * EXEC CICS  DELETE
01089 *        DATASET  (PI-ACCT-ID)
01090 *    END-EXEC
      *    MOVE '&(                    &   #00005563' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01091
01092      PERFORM 8400-LOG-JOURNAL-RECORD
01093
01094      PERFORM 6600-UPDATE-MAINT-DT  THRU  6699-EXIT
01095
01096      MOVE LOW-VALUES             TO  PI-ACCT-EXP-DT
01097                                      PI-ACCT-REST-OF-EXP
01098                                      LINEO
01099      MOVE ER-0000                TO  EMI-ERROR
01100
01101      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01102
01103      IF PI-TOTAL-LINES  = 1
01104          MOVE LOW-VALUES         TO  PI-DATE-RANGE-TABLE
01105          MOVE ZEROS              TO  PI-TOTAL-LINES
01106          MOVE 1                  TO  PI-PAGE-NUMBER
01107          GO TO 7000-BUILD-OUTPUT-MAP
pemtst     end-if
01109      GO TO 1002-SHOW-NEW-RECORDS.
01110  EJECT
01111  2550-CHECK-HI-LO-DATES.
01112      IF FROM-CHANGE
01113          GO TO 2550-CHECK-CHANGE.
01114
01115      IF PI-BIN-LO-CERT (T-INDEX)  =  LOW-VALUES  AND
01116         PI-BIN-HI-CERT (T-INDEX)  =  LOW-VALUES
01117          GO TO 2550-CHECK-AR-DELETE.
01118
01119      IF ((PI-BIN-HI-CERT (T-INDEX) NOT LESS BIN-MIN-DATE AND
01120           PI-BIN-HI-CERT (T-INDEX) LESS BIN-MAX-DATE)
01121        OR
01122          (PI-BIN-LO-CERT (T-INDEX) NOT LESS BIN-MIN-DATE AND
01123           PI-BIN-LO-CERT (T-INDEX) LESS BIN-MAX-DATE))
01124        AND
01125             (PI-PROCESSOR-ID  NOT =  'LGXX')
01126          MOVE ER-2087            TO  EMI-ERROR
01127          MOVE -1                 TO  MAINTL
01128          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01129
01130  2550-CHECK-AR-DELETE.
01131      IF NOT PI-AR-PROCESSING
01132          GO TO 2550-EXIT.
01133
01134      IF T-INDEX NOT = LINEI
01135          GO TO 2550-EXIT.
01136
01137      IF PI-BIN-AR-HI-CERT (T-INDEX) = LOW-VALUES OR SPACES
01138         GO TO 2550-EXIT.
01139
01140      MOVE ER-3155                TO  EMI-ERROR
01141      MOVE -1                     TO  MAINTL
01142      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01143
01144  2550-CHECK-CHANGE.
01145      IF PI-BIN-HI-CERT (T-INDEX)  = LOW-VALUES OR SPACES
01146          GO TO 2550-CHECK-AR-CHANGE.
01147
01148      IF PI-BIN-HI-CERT (T-INDEX) NOT LESS BIN-MAX-DATE
01149          MOVE ER-2113            TO  EMI-ERROR
01150          MOVE -1                 TO  MAINTL
01151          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01152
01153  2550-CHECK-AR-CHANGE.
01154       IF NOT PI-AR-PROCESSING
01155           GO TO 2550-EXIT.
01156
01157       IF PI-BIN-AR-HI-CERT (T-INDEX) = LOW-VALUES
01158           GO TO 2550-EXIT.
01159
01160       IF PI-BIN-AR-HI-CERT (T-INDEX) NOT LESS BIN-MAX-DATE
01161          MOVE ER-3155            TO  EMI-ERROR
01162          MOVE -1                 TO  MAINTL
01163          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01164
01165  2550-EXIT.
01166      EXIT.
01167  EJECT
01168  2560-CHECK-PENDING-BUSINESS.
01169      MOVE 'N'                    TO  PENDING-BUSINESS-SW.
01170      MOVE LOW-VALUES             TO  ERPNDB-KEY
01171                                      PI-ERPNDB-ALT-KEY.
01172      MOVE AM-COMPANY-CD-A1       TO  PNDB-COMPANY-CD-A1
01173                                      PI-PB-COMPANY-CD-A1.
01174      MOVE AM-VG-CARRIER          TO  PNDB-CARRIER
01175                                      PI-PB-CARRIER.
01176      MOVE AM-VG-GROUPING         TO  PNDB-GROUPING
01177                                      PI-PB-GROUPING.
01178      MOVE AM-VG-STATE            TO  PNDB-STATE
01179                                      PI-PB-STATE.
01180      MOVE AM-VG-ACCOUNT          TO  PNDB-ACCOUNT
01181                                      PI-PB-ACCOUNT.
01182
01183      
      * EXEC CICS  HANDLE CONDITION
01184 *         NOTFND   (2568-NO-RECORDS)
01185 *         ENDFILE  (2568-NO-RECORDS)
01186 *    END-EXEC.
      *    MOVE '"$I''                  ! % #00005658' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035363538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01187
01188      
      * EXEC CICS  STARTBR
01189 *        DATASET  (PEND2-FILE-ID)
01190 *        RIDFLD   (ERPNDB-KEY)
01191 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005663' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PEND2-FILE-ID, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01192
01193      MOVE 'Y'                    TO  PENDING-BROWSE-SW.
01194
01195  2562-READ-NEXT-PENDING.
01196      
      * EXEC CICS  READNEXT
01197 *        DATASET  (PEND2-FILE-ID)
01198 *        RIDFLD   (ERPNDB-KEY)
01199 *        SET      (ADDRESS OF PENDING-BUSINESS)
01200 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005671' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PEND2-FILE-ID, 
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
           
01201
01202      IF PB-COMPANY-CD-A1 = PI-PB-COMPANY-CD-A1 AND
01203         PB-CARRIER       = PI-PB-CARRIER       AND
01204         PB-GROUPING      = PI-PB-GROUPING      AND
01205         PB-STATE         = PI-PB-STATE         AND
01206         PB-ACCOUNT       = PI-PB-ACCOUNT
01207             NEXT SENTENCE
01208           ELSE
01209             GO TO 2568-NO-RECORDS.
01210
01211      IF PB-ISSUE
01212        OR  PB-CANCELLATION
01213          NEXT SENTENCE
01214      ELSE
01215          GO TO 2562-READ-NEXT-PENDING.
01216
01217      IF PB-CERT-EFF-DT  IS EQUAL TO  AM-EFFECTIVE-DT
01218          MOVE 'Y'                TO  PENDING-BUSINESS-SW
01219          GO TO 2568-NO-RECORDS.
01220
01221      IF PB-CERT-EFF-DT  IS GREATER THAN  AM-EFFECTIVE-DT
01222        AND PB-CERT-EFF-DT  IS LESS THAN  AM-EXPIRATION-DT
01223          MOVE 'Y'                TO  PENDING-BUSINESS-SW
01224          GO TO 2568-NO-RECORDS.
01225
01226      GO TO 2562-READ-NEXT-PENDING.
01227
01228  2568-NO-RECORDS.
01229      IF PENDING-BROWSE-STARTED
01230          MOVE ' '                TO  PENDING-BROWSE-SW
01231         
      * EXEC CICS ENDBR
01232 *           DATASET  (PEND2-FILE-ID)
01233 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005706' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PEND2-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01234
01235  2569-EXIT.
01236      EXIT.
01237  EJECT
01238  3000-CHANGE-EXPIRATION.
01239      PERFORM 6100-BUILD-KEY             THRU  6199-EXIT.
01240      PERFORM 6200-CHECK-KEY             THRU  6200-EXIT.
01241      PERFORM 6000-EDIT-EXPIRATION-DATE  THRU  6000-EXP-EXIT.
01242      PERFORM 4100-BUILD-VERIFICATION    THRU  4199-EXIT.
01243
01244      SET T-INDEX  TO  PI-TOTAL-LINES.
01245
01246      PERFORM VARYING WS-INDEX FROM +1 BY +1
01247              UNTIL  PI-BIN-EXP-DT (T-INDEX)
01248                         =  WS-ACCT-EXP-DT (WS-INDEX)
01249                  OR  WS-INDEX GREATER +32
01250      END-PERFORM.
01251
01252      IF PI-BIN-EXP-DT (T-INDEX) = WS-ACCT-EXP-DT (WS-INDEX)
01253          MOVE BIN-EXPCHG-SAVE    TO  WS-ACCT-EXP-DT (WS-INDEX)
01254          PERFORM 4300-VERIFY-INTEGRITY THRU 4399-EXIT
01255              VARYING  WS-INDEX  FROM  +1  BY  +1
01256                UNTIL  WS-INDEX GREATER +32
01257      ELSE
01258          MOVE ER-0453            TO  EMI-ERROR
01259          MOVE -1                 TO  MAINTL
01260          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01261
01262      IF NOT EMI-NO-ERRORS
01263          GO TO 8200-SEND-DATAONLY.
01264
01265      MOVE BIN-EXPCHG-SAVE        TO  BIN-MAX-DATE.
01266      MOVE '2'                    TO  FROM-WHERE-SW.
01267
01268      PERFORM 2550-CHECK-HI-LO-DATES  THRU  2550-EXIT
01269        VARYING  T-INDEX  FROM  +1  BY  +1
01270          UNTIL  T-INDEX GREATER PI-TOTAL-LINES.
01271
01272      MOVE SPACE                  TO  FROM-WHERE-SW.
01273
01274      IF NOT EMI-NO-ERRORS
01275          GO TO 8200-SEND-DATAONLY.
01276
01277      SET T-INDEX  TO  PI-TOTAL-LINES.
01278      PERFORM 6010-DATE-CHECK  THRU  6010-EXIT.
01279
01280      IF NOT EMI-NO-ERRORS
01281          GO TO 8200-SEND-DATAONLY.
01282
01283      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT.
01284      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
01285      MOVE PI-ACCT-KEY              TO  B4-ACCT-KEY.
01286      MOVE PI-BIN-EFF-DT (T-INDEX)  TO  B4-ACCT-EFF-DT.
01287
01288      PERFORM 3100-DELETE-DATE-RANGE  THRU  3199-EXIT.
01289
01290      MOVE BIN-EXPCHG-SAVE        TO  AM-EXPIRATION-DT
01291                                      AM-VG-EXPIRATION-DT
01292                                      PI-BIN-EXP-DT (T-INDEX).
01293      MOVE BIN-CURRENT-SAVE       TO  AM-LAST-MAINT-DT
01294                                      PI-BIN-MAINT-DT (T-INDEX).
01295      MOVE AM-CONTROL-PRIMARY     TO  AF-ACCT-KEY.
01296      MOVE AM-EFFECTIVE-DT        TO  AF-ACCT-EFF-DT.
01297
01298      PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT.
01299
01300      PERFORM 3200-ADD-DATE-RANGE    THRU  3299-EXIT.
01301
01302      PERFORM 6600-UPDATE-MAINT-DT   THRU  6699-EXIT.
01303
CIDMOD     MOVE SPACES                 TO  EXPCHGI.
CIDMOD     MOVE AL-UANOF               TO  EXPCHGA.
CIDMOD     MOVE ER-0000                TO  EMI-ERROR.
01305
01306      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01307
01308      GO TO 7000-BUILD-OUTPUT-MAP.
01309
01310  EJECT
01311  3100-DELETE-DATE-RANGE.
01312      
      * EXEC CICS GETMAIN
01313 *        LENGTH   (ACCT-REC-LEN)
01314 *        SET      (ADDRESS OF ACCOUNT-MASTER)
01315 *        INITIMG  (GETMAIN-SPACE)
01316 *    END-EXEC.
      *    MOVE ',"IL                  $   #00005789' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACCT-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01317
01318      
      * EXEC CICS  HANDLE CONDITION
01319 *        NOTFND   (3175-CONTINUE)
01320 *        ENDFILE  (3175-CONTINUE)
01321 *    END-EXEC.
      *    MOVE '"$I''                  ! & #00005795' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035373935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01322
01323
01324      
      * EXEC CICS  READ
01325 *        UPDATE
01326 *        DATASET  (PI-ACCT-ID)
01327 *        INTO     (ACCOUNT-MASTER)
01328 *        RIDFLD   (PI-ACCT-KEY)
01329 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00005801' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01330
01331      MOVE 'D'                    TO  JP-RECORD-TYPE.
01332      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
01333      MOVE ACCT-FILE-ID           TO  FILE-ID.
01334
01335      
      * EXEC CICS  DELETE
01336 *        DATASET  (PI-ACCT-ID)
01337 *    END-EXEC.
      *    MOVE '&(                    &   #00005812' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01338
01339      PERFORM 8400-LOG-JOURNAL-RECORD.
01340
01341      GO TO 3199-EXIT.
01342
01343  3175-CONTINUE.
01344      IF BROWSE-STARTED
01345          PERFORM 6320-ENDBR THRU 6320-EXIT.
01346
01347      MOVE ER-0226                TO  EMI-ERROR.
01348
01349      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01350
01351      MOVE -1                     TO  MAINTL.
01352
01353      GO TO 8200-SEND-DATAONLY.
01354
01355  3199-EXIT.
01356      EXIT.
01357
01358  3200-ADD-DATE-RANGE.
01359      MOVE 'A'                    TO  JP-RECORD-TYPE.
01360      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
01361
01362      
      * EXEC CICS  WRITE
01363 *        FROM     (ACCOUNT-MASTER)
01364 *        RIDFLD   (AM-CONTROL-PRIMARY)
01365 *        DATASET  (PI-ACCT-ID)
01366 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005839' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01367
01368      PERFORM 8400-LOG-JOURNAL-RECORD.
01369
01370  3299-EXIT.
01371      EXIT.
01372  EJECT
01373  3500-CHANGE-EFFECTIVE.
01374      PERFORM 6100-BUILD-KEY  THRU  6199-EXIT.
01375
01376      PERFORM 6200-CHECK-KEY  THRU  6200-EXIT.
01377
01378      PERFORM 6000-EDIT-EFFECTIVE-DATE  THRU  6000-EFF-EXIT.
01379
01380      IF NOT EMI-NO-ERRORS
01381          GO TO 8200-SEND-DATAONLY.
01382
01383      PERFORM 4100-BUILD-VERIFICATION  THRU  4199-EXIT.
01384
01385      SET T-INDEX                 TO  +1.
01386
01387      PERFORM VARYING  WS-INDEX  FROM  +1  BY  +1
01388              UNTIL  PI-BIN-EFF-DT (T-INDEX)
01389                         =  WS-ACCT-EFF-DT (WS-INDEX)
01390                  OR  WS-INDEX GREATER +32
01391      END-PERFORM.
01392
01393      IF PI-BIN-EFF-DT (T-INDEX)
01394                  = WS-ACCT-EFF-DT (WS-INDEX)
01395          MOVE BIN-EFFCHG-SAVE    TO  WS-ACCT-EFF-DT (WS-INDEX)
01396          PERFORM 4300-VERIFY-INTEGRITY THRU 4399-EXIT
01397              VARYING  WS-INDEX  FROM  +1  BY  +1
01398                UNTIL  WS-INDEX GREATER +32
01399      ELSE
01400          MOVE ER-0453            TO  EMI-ERROR
01401          MOVE -1                 TO  MAINTL
01402          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01403
01404      IF NOT EMI-NO-ERRORS
01405          GO TO 8200-SEND-DATAONLY.
01406
01407      SET T-INDEX TO  1.
01408
01409      PERFORM 6010-DATE-CHECK  THRU  6010-EXIT.
01410
01411      IF NOT EMI-NO-ERRORS
01412          GO TO 8200-SEND-DATAONLY.
01413
01414      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT.
01415      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
01416      MOVE PI-ACCT-KEY              TO  B4-ACCT-KEY.
01417      MOVE PI-BIN-EFF-DT (T-INDEX)  TO  B4-ACCT-EFF-DT.
01418
01419      
      * EXEC CICS  READ
01420 *        UPDATE
01421 *        DATASET  (PI-ACCT-ID)
01422 *        SET      (ADDRESS OF ACCOUNT-MASTER)
01423 *        RIDFLD   (PI-ACCT-KEY)
01424 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005896' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01425
01426      MOVE 'B'                    TO  JP-RECORD-TYPE.
01427      MOVE ACCT-FILE-ID           TO  FILE-ID.
01428      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
01429
01430      PERFORM 8400-LOG-JOURNAL-RECORD.
01431
01432      MOVE BIN-EFFCHG-SAVE        TO  AM-EFFECTIVE-DT
01433                                      PI-BIN-EFF-DT (T-INDEX).
01434      MOVE EFFCHG-SAVE            TO  AM-EFFECT-DT.
01435      MOVE BIN-CURRENT-SAVE       TO  AM-LAST-MAINT-DT
01436                                      PI-BIN-MAINT-DT (T-INDEX).
01437      MOVE AM-CONTROL-PRIMARY     TO  AF-ACCT-KEY.
01438      MOVE AM-EFFECTIVE-DT        TO  AF-ACCT-EFF-DT.
01439      MOVE 'C'                    TO  JP-RECORD-TYPE.
01440      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
01441
01442      PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT.
01443
01444      
      * EXEC CICS  REWRITE
01445 *        FROM     (ACCOUNT-MASTER)
01446 *        DATASET  (PI-ACCT-ID)
01447 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005921' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01448
01449      PERFORM 8400-LOG-JOURNAL-RECORD.
01450
01451      PERFORM 6600-UPDATE-MAINT-DT  THRU  6699-EXIT.
01452
CIDMOD     MOVE SPACES                 TO  EFFCHGO.
CIDMOD
01453      MOVE ZERO                   TO  EFFCHGL
01454                                      EXPCHGL.
CIDMOD
CIDMOD     MOVE AL-UANOF               TO  EFFCHGA.
CIDMOD
01455      MOVE ER-0000                TO  EMI-ERROR.
01456
01457      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01458
01459      GO TO 7000-BUILD-OUTPUT-MAP.
01460  EJECT
01461  4000-CHANGE-ALL.
01462      PERFORM 6100-BUILD-KEY  THRU  6199-EXIT.
01463
01464      PERFORM 6200-CHECK-KEY  THRU  6200-EXIT.
01465
01466      IF NOT EMI-NO-ERRORS
01467          GO TO 8200-SEND-DATAONLY.
01468
01469      MOVE 1                      TO  PI-LINE-SELECTED.
01470      MOVE MAINTI                 TO  PI-MAINT.
01471      MOVE XCTL-6501              TO  PGM-NAME.
01472
01473      GO TO 9300-XCTL.
01474  EJECT
01475  4100-BUILD-VERIFICATION.
01476      MOVE SPACES                 TO  BROWSE-STARTED-SW.
01477
01478      SET WS-INDEX  TO  +1.
01479
01480      MOVE 'N'                    TO  WS-ACCT2-FOUND.
01481      MOVE PI-ACCT-KEY            TO  WS-ACCT-VERIFY-KEY.
01482      MOVE LOW-VALUES             TO  WS-ACCT-VER-EXP-DT
01483                                      WS-VERIFY-AREA.
01484
01485      IF ST-ACCNT-CNTL  OR
01486         ACCNT-CNTL
01487           MOVE SPACES             TO  WS-ACCT-VER-CARR.
01488
01489      IF ST-ACCNT-CNTL      OR
01490         CARR-ST-ACCNT-CNTL OR
01491         ACCNT-CNTL         OR
01492         CARR-ACCNT-CNTL
01493           MOVE SPACES             TO  WS-ACCT-VER-GROUP.
01494
01495      IF ACCNT-CNTL OR
01496         CARR-ACCNT-CNTL
01497           MOVE SPACES             TO  WS-ACCT-VER-STATE.
01498
01499      MOVE WS-ACCT-VERIFY-KEY     TO  WS-SAVE-VERIFY-KEY.
01500
01501      
      * EXEC CICS  HANDLE CONDITION
01502 *        NOTFND   (4175-CONTINUE)
01503 *        ENDFILE  (4175-CONTINUE)
01504 *    END-EXEC.
      *    MOVE '"$I''                  ! '' #00005983' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035393833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01505
01506      
      * EXEC CICS  STARTBR
01507 *        DATASET  (ACCT2-FILE-ID)
01508 *        RIDFLD   (WS-ACCT-VERIFY-KEY)
01509 *        GTEQ
01510 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005988' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT2-FILE-ID, 
                 WS-ACCT-VERIFY-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01511
01512      MOVE 'Y'                    TO  BROWSE-STARTED-SW.
01513
01514  4150-READ-NEXT.
01515      
      * EXEC CICS  READNEXT
01516 *        DATASET  (ACCT2-FILE-ID)
01517 *        RIDFLD   (WS-ACCT-VERIFY-KEY)
01518 *        SET      (ADDRESS OF ACCOUNT-MASTER)
01519 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005997' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT2-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACCT-VERIFY-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01520
01521      MOVE AM-CONTROL-BY-VAR-GRP  TO  WS-TEST-KEY.
01522
01523      IF WS-TEST-KEY  NOT = WS-SAVE-VERIFY-KEY
01524          GO TO 4175-CONTINUE.
01525
01526      MOVE 'Y'                    TO  WS-ACCT2-FOUND.
01527      MOVE AM-CONTROL-PRIMARY     TO  WS-ACCT-KEY (WS-INDEX).
01528      MOVE AM-VG-EXPIRATION-DT    TO  WS-ACCT-EXP-DT (WS-INDEX).
01529      MOVE AM-EFFECTIVE-DT        TO  WS-ACCT-EFF-DT (WS-INDEX).
01530      MOVE AM-HI-CERT-DATE        TO  DC-GREG-DATE-1-YMD.
01531      MOVE '3'                    TO  DC-OPTION-CODE.
01532
01533      PERFORM 9700-DATE-CONVERSION.
01534
01535      IF DATE-CONVERSION-ERROR
01536          MOVE LOW-VALUES         TO  WS-ACCT-HI-CERT (WS-INDEX)
01537      ELSE
01538          MOVE DC-BIN-DATE-1      TO  WS-ACCT-HI-CERT (WS-INDEX).
01539
01540      MOVE AM-LO-CERT-DATE        TO  DC-GREG-DATE-1-YMD.
01541      MOVE '3'                    TO  DC-OPTION-CODE.
01542
01543      PERFORM 9700-DATE-CONVERSION.
01544
01545      IF DATE-CONVERSION-ERROR
01546          MOVE LOW-VALUES         TO  WS-ACCT-LO-CERT (WS-INDEX)
01547      ELSE
01548          MOVE DC-BIN-DATE-1      TO  WS-ACCT-LO-CERT (WS-INDEX).
01549
01550      SET WS-INDEX  UP  BY  +1.
01551
01552      GO TO 4150-READ-NEXT.
01553
01554  4175-CONTINUE.
01555      IF BROWSE-STARTED
01556          
      * EXEC CICS  ENDBR
01557 *            DATASET  (ACCT2-FILE-ID)
01558 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006038' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT2-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01559          MOVE SPACES             TO  BROWSE-STARTED-SW.
01560
01561  4199-EXIT.
01562      EXIT.
01563  EJECT
01564  4200-CHECK-VALIDITY.
01565      IF (BIN-EXPCHG-SAVE GREATER
01566                     WS-ACCT-EFF-DT (WS-INDEX))
01567        AND  (BIN-EXPCHG-SAVE NOT GREATER
01568                     WS-ACCT-EXP-DT (WS-INDEX))
01569          MOVE ER-2051            TO  EMI-ERROR
01570          MOVE -1                 TO  EXPCHGL
01571          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01572
01573      IF BIN-EFFCHG-SAVE NOT LESS WS-ACCT-EFF-DT (WS-INDEX) AND
01574         BIN-EFFCHG-SAVE LESS     WS-ACCT-EXP-DT (WS-INDEX)
01575           MOVE ER-2051            TO  EMI-ERROR
01576           MOVE -1                 TO  EFFCHGL
01577           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01578
111513 4200-EXIT.
01580      EXIT.
111513 4220-DISPLAY-VERI-TABLE.
111513
111513     perform varying d1 from +1 by +1 until
111513        (d1 > +32)
111513        or (ws-acct-eff-dt (d1) = low-values)
111513        move ws-acct-eff-dt (d1) to dc-bin-date-1
111513        move ' '                 to dc-option-code
111513        perform 9700-date-conversion
111513        move dc-greg-date-1-edit to ws-display-eff-date
111513
111513        move ws-acct-exp-dt (d1) to dc-bin-date-1
111513        move ' '                 to dc-option-code
111513        perform 9700-date-conversion
111513        move dc-greg-date-1-edit to ws-display-exp-date
111513
111513        display ' date range ' d1 ' ' ws-display-eff-date
111513                                  ' ' ws-display-exp-date
111513     end-perform
111513
111513     .
111513 4220-EXIT.
           EXIT.
01581
01582  4300-VERIFY-INTEGRITY.
111513     set d1 to ws-index
01583      IF WS-ACCT-EFF-DT (WS-INDEX + 1)  = LOW-VALUES
01584          SET WS-INDEX  TO  +32
01585          GO TO 4399-EXIT.
01586
01587      IF ((WS-ACCT-EXP-DT (WS-INDEX)  =  BIN-EXPCHG-SAVE)  OR
01588         (WS-ACCT-EFF-DT (WS-INDEX + 1)  =  BIN-EFFCHG-SAVE))
111513                  AND
111513        (WS-ACCT-EXP-DT (WS-INDEX) > WS-ACCT-EFF-DT (WS-INDEX))
01589          SET WS-INDEX  TO  +33
01590          GO TO 4399-EXIT.
01591
01592      IF (WS-ACCT-EXP-DT (WS-INDEX) NOT GREATER
01593                     WS-ACCT-EFF-DT (WS-INDEX + 1))
01594        AND  (WS-ACCT-EXP-DT (WS-INDEX) GREATER
01595                     WS-ACCT-EFF-DT (WS-INDEX))
01596          GO TO 4399-EXIT.
01597
01598      MOVE ER-2051                TO  EMI-ERROR.
01599      MOVE -1                     TO  EXPCHGL.
01600
01601      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01602
01603  4399-EXIT.
01604      EXIT.
01605  EJECT
01606  4500-CREATE-NEW-ACCOUNT.
01607      PERFORM 6100-BUILD-KEY            THRU 6199-EXIT.
01608
01609      PERFORM 6700-VERIFY-STATE-CARRIER THRU 6799-EXIT.
01610
01611      IF MAINTI = 'T'
01612          PERFORM 6000-EDIT-EFFECTIVE-DATE THRU 6000-EFF-EXIT.
01613
01614      PERFORM 4100-BUILD-VERIFICATION THRU 4199-EXIT.
01615
01616      IF WS-ACCT2-FOUND = 'Y'
01617          MOVE ER-0561            TO  EMI-ERROR
01618          MOVE -1                 TO  MAINTL
01619          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01620
01621      MOVE SPACES                 TO  GETMAIN-SW.
01622
01623      IF NOT EMI-NO-ERRORS
01624          GO TO 8200-SEND-DATAONLY.
01625
01626      IF PI-PREV-ACCOUNT  =  LOW-VALUES
01627          MOVE ER-0453            TO  EMI-ERROR
01628          MOVE -1                 TO  MAINTL
01629          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01630          GO TO 8200-SEND-DATAONLY.
01631
01632      MOVE PI-ACCT-CCGSA-KEY      TO  NEW-KEY-SAVE.
01633
01634      MOVE LOW-VALUES             TO  PI-ACCT-EXP-DT
01635                                      PI-ACCT-REST-OF-EXP.
01636
01637      PERFORM 6500-READ-ACCT  THRU  6599-EXIT.
01638
01639      IF REC-FOUND
01640          MOVE -1                 TO  ACARIERL
01641          MOVE ER-0561            TO  EMI-ERROR
01642          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01643          GO TO 8200-SEND-DATAONLY.
01644
01645      MOVE PI-PREV-ACCOUNT        TO  PI-ACCT-CCGSA-KEY.
01646      MOVE LOW-VALUES             TO  PI-ACCT-EXP-DT
01647                                      PI-ACCT-REST-OF-EXP.
01648
01649      
      * EXEC CICS  HANDLE CONDITION
01650 *        NOTFND   (4530-NOT-FOUND)
01651 *        ENDFILE  (4530-NOT-FOUND)
01652 *    END-EXEC.
      *    MOVE '"$I''                  ! ( #00006156' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036313536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01653
01654      PERFORM 6300-STARTBR  THRU  6300-EXIT.
01655
01656  4510-READ-NEXT.
01657      PERFORM 6310-READNEXT  THRU  6310-EXIT.
01658
01659      IF PI-ACCT-CCGSA-KEY  NOT = PI-PREV-ACCOUNT
01660          GO TO 4540-END-COPY.
01661
01662      MOVE AM-EXPIRATION-DT       TO  WS-EXP-SAVE.
01663      MOVE NEW-KEY-SAVE           TO  AM-CONTROL-PRIMARY.
01664      MOVE LOW-VALUES             TO  AM-CNTRL-B.
01665      MOVE WS-EXP-SAVE            TO  AM-EXPIRATION-DT.
01666
01667      PERFORM 6400-BUILD-VG-KEY  THRU  6499-EXIT.
01668
01669      IF MAINTI  = 'T'
01670          MOVE '2'                TO  AM-STATUS
01671          MOVE AM-ACCOUNT         TO  AM-AGT (1)
01672          MOVE PI-ACCT-CARRIER    TO  AM-TRNFROM-CARRIER
01673          MOVE PI-ACCT-GROUPING   TO  AM-TRNFROM-GROUPING
01674          MOVE PI-ACCT-STATE      TO  AM-TRNFROM-STATE
01675          MOVE PI-ACCT-ACCOUNT    TO  AM-TRNFROM-ACCOUNT
01676          MOVE BIN-EFFCHG-SAVE    TO  AM-TRNFROM-DTE
01677      ELSE
01678          MOVE LOW-VALUES         TO  AM-AR-HI-CERT-DATE
01679          MOVE ZEROS              TO  AM-HI-CERT-DATE
01680                                      AM-LO-CERT-DATE
01681          MOVE SPACES             TO  AM-1ST-PROD-DATE
01682                                      AM-PREV-DATES
121207                                     AM-ORIG-DEALER-NO
121207     END-IF
01683
01684      MOVE SPACES                 TO  AM-CERTS-PURGED-DATE.
01685      MOVE BIN-CURRENT-SAVE       TO  AM-LAST-MAINT-DT.
01686      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.
01687      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.
01688
01689      PERFORM 5500-ADD-NEW-AXRF-RECORD  THRU  5599-EXIT.
01690
01691      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
01692      MOVE 'A'                    TO  JP-RECORD-TYPE.
01693
01694      PERFORM 6320-ENDBR THRU 6320-EXIT.
01695
01696      
      * EXEC CICS GETMAIN
01697 *        LENGTH   (ACCT-REC-LEN)
01698 *        SET      (ADDRESS OF ACCOUNT-MASTER)
01699 *        INITIMG  (GETMAIN-SPACE)
01700 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006205' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACCT-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01701
01702      MOVE JP-RECORD-AREA         TO  ACCOUNT-MASTER.
01703
01704      
      * EXEC CICS  WRITE
01705 *        DATASET  (PI-ACCT-ID)
01706 *        RIDFLD   (AM-CONTROL-PRIMARY)
01707 *        FROM     (ACCOUNT-MASTER)
01708 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006213' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01709
01710      MOVE PI-ACCT-ID             TO  FILE-ID.
01711
01712      PERFORM 8400-LOG-JOURNAL-RECORD.
01713
01714      PERFORM 6300-STARTBR  THRU 6300-EXIT.
01715      PERFORM 6310-READNEXT THRU 6310-EXIT.
01716
01717      GO TO 4510-READ-NEXT.
01718
01719  4530-NOT-FOUND.
01720      IF BROWSE-STARTED
01721          PERFORM 6320-ENDBR THRU 6320-EXIT.
01722
01723      MOVE ER-0226                TO  EMI-ERROR.
01724
01725      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01726
01727      MOVE -1                     TO  MAINTL.
01728
01729      GO TO 8200-SEND-DATAONLY.
01730
01731  4540-END-COPY.
01732      IF MAINTI = 'K'
01733          IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
01734              NEXT SENTENCE
01735          ELSE
01736              PERFORM 4600-ADD-ERPLAN-RECORD THRU 4699-EXIT.
01737
01738      IF BROWSE-STARTED
01739          PERFORM 6320-ENDBR THRU 6320-EXIT.
01740
01741      IF MAINTI  NOT = 'T'
01742          GO TO 4590-SHOW-NEW-ACCOUNT.
01743
01744      MOVE PI-PREV-ACCOUNT        TO  PI-ACCT-CCGSA-KEY.
01745      MOVE LOW-VALUES             TO  PI-ACCT-EXP-DT
01746                                      PI-ACCT-REST-OF-EXP.
01747
01748      PERFORM 6300-STARTBR  THRU  6300-EXIT.
01749
01750  4550-READ-NEXT.
01751      PERFORM 6310-READNEXT  THRU  6310-EXIT.
01752
01753      IF PI-ACCT-CCGSA-KEY  NOT =  PI-PREV-ACCOUNT
01754          GO TO 4570-END-REWRITE.
01755
01756      
      * EXEC CICS  READ
01757 *        UPDATE
01758 *        DATASET  (PI-ACCT-ID)
01759 *        SET      (ADDRESS OF ACCOUNT-MASTER)
01760 *        RIDFLD   (PI-ACCT-KEY)
01761 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006265' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01762
01763      MOVE 'B'                    TO  JP-RECORD-TYPE.
01764      MOVE ACCT-FILE-ID           TO  FILE-ID.
01765      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
01766
01767      PERFORM 8400-LOG-JOURNAL-RECORD.
01768
01769      MOVE NEW-KEY-CNTRL          TO  AM-TRANSFERRED-TO.
01770      MOVE BIN-EFFCHG-SAVE        TO  AM-TRNTO-DTE.
01771      MOVE BIN-CURRENT-SAVE       TO  AM-LAST-MAINT-DT.
01772      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.
01773      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.
01774      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
01775      MOVE 'C'                    TO  JP-RECORD-TYPE.
01776
01777      
      * EXEC CICS  REWRITE
01778 *        DATASET  (PI-ACCT-ID)
01779 *        FROM     (ACCOUNT-MASTER)
01780 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006286' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01781
01782      MOVE PI-ACCT-ID             TO  FILE-ID.
01783
01784      PERFORM 8400-LOG-JOURNAL-RECORD.
01785
01786      GO TO 4550-READ-NEXT.
01787
01788  4570-END-REWRITE.
01789      IF BROWSE-STARTED
01790          PERFORM 6320-ENDBR  THRU  6320-EXIT.
01791
01792      MOVE SPACES                 TO  MAINTI
01793                                      EFFCHGI.
01794      MOVE AL-UANOF               TO  EFFCHGA.
01795
01796  4590-SHOW-NEW-ACCOUNT.
01797      PERFORM 6600-UPDATE-MAINT-DT  THRU  6699-EXIT.
01798
01799      MOVE NEW-KEY-SAVE           TO  PI-PREV-ACCOUNT
01800                                      PI-ACCT-CCGSA-KEY.
01801
01802      MOVE -1                     TO  MAINTL.
01803      MOVE ER-0000                TO  EMI-ERROR.
01804
01805      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01806
01807      MOVE LOW-VALUES             TO  PI-ACCT-EXP-DT.
01808
01809      GO TO 1002-SHOW-NEW-RECORDS.
01810
01811  EJECT
01812  4600-ADD-ERPLAN-RECORD.
01813      
      * EXEC CICS  HANDLE CONDITION
01814 *        NOTFND   (4699-EXIT)
01815 *        ENDFILE  (4689-EOF)
01816 *    END-EXEC.
      *    MOVE '"$I''                  ! ) #00006322' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303036333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01817
01818      MOVE LOW-VALUES            TO WS-PLAN-KEY.
01819      MOVE PI-PREV-ACCOUNT       TO EK-CCGSA-KEY.
01820
01821      PERFORM 4700-STARTBR-PLAN THRU 4700-EXIT.
01822
01823  4610-READ-NEXT.
01824      PERFORM 4710-READNEXT-PLAN THRU 4710-EXIT.
01825
01826      IF EK-CCGSA-KEY NOT = PI-PREV-ACCOUNT
01827          GO TO 4689-EOF.
01828
01829      MOVE NEW-KEY-SAVE           TO  PL-CONTROL-PRIMARY.
01830
01831      MOVE EK-BTYPE               TO  PL-BENEFIT-TYPE.
01832      MOVE EK-BCODE               TO  PL-BENEFIT-CODE.
01833      MOVE EK-REVISION            TO  PL-REVISION-NO.
01834
01835      MOVE PL-CONTROL-PRIMARY     TO  PL-CONTROL-BY-VAR-GRP.
01836
01837      MOVE PLAN-MASTER            TO  JP-RECORD-AREA.
01838
01839      
      * EXEC CICS  ENDBR
01840 *        DATASET  (PLAN-FILE-ID)
01841 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006348' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01842
01843      
      * EXEC CICS GETMAIN
01844 *        LENGTH   (PLAN-REC-LEN)
01845 *        SET      (ADDRESS OF PLAN-MASTER)
01846 *        INITIMG  (GETMAIN-SPACE)
01847 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006352' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 PLAN-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01848
01849      MOVE JP-RECORD-AREA         TO PLAN-MASTER.
01850
01851      
      * EXEC CICS  WRITE
01852 *        DATASET  (PLAN-FILE-ID)
01853 *        RIDFLD   (PL-CONTROL-PRIMARY)
01854 *        FROM     (PLAN-MASTER)
01855 *    END-EXEC.
           MOVE LENGTH OF
            PLAN-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006360' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 PLAN-MASTER, 
                 DFHEIV11, 
                 PL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01856
01857      PERFORM 4700-STARTBR-PLAN  THRU 4700-EXIT.
01858      PERFORM 4710-READNEXT-PLAN THRU 4710-EXIT.
01859
01860      GO TO 4610-READ-NEXT.
01861
01862  4689-EOF.
01863      
      * EXEC CICS  ENDBR
01864 *        DATASET  (PLAN-FILE-ID)
01865 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006372' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01866
01867  4699-EXIT.
01868       EXIT.
01869
01870  4700-STARTBR-PLAN.
01871      
      * EXEC CICS  STARTBR
01872 *        DATASET  (PLAN-FILE-ID)
01873 *        RIDFLD   (WS-PLAN-KEY)
01874 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006380' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 WS-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01875
01876  4700-EXIT.
01877       EXIT.
01878
01879  4710-READNEXT-PLAN.
01880      
      * EXEC CICS  READNEXT
01881 *        DATASET  (PLAN-FILE-ID)
01882 *        SET      (ADDRESS OF PLAN-MASTER)
01883 *        RIDFLD   (WS-PLAN-KEY)
01884 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006389' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01885
01886  4710-EXIT.
01887       EXIT.
01888
01889  EJECT
01890  4800-DELETE-PLAN-REC.
01891      
      * EXEC CICS  HANDLE CONDITION
01892 *        NOTFND   (4899-EXIT)
01893 *        ENDFILE  (4889-EOF)
01894 *    END-EXEC.
      *    MOVE '"$I''                  ! * #00006400' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303036343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01895
01896      MOVE LOW-VALUES            TO WS-PLAN-KEY.
01897      MOVE AM-CONTROL-PRIMARY    TO EK-CCGSA-KEY
01898                                    PI-PREV-ACCOUNT.
01899
01900  4805-STARTBR.
01901      PERFORM 4700-STARTBR-PLAN THRU 4700-EXIT.
01902
01903  4810-READ-NEXT.
01904      PERFORM 4710-READNEXT-PLAN THRU 4710-EXIT.
01905
01906      IF EK-CCGSA-KEY NOT = PI-PREV-ACCOUNT
01907          GO TO 4889-EOF.
01908
01909      
      * EXEC CICS  ENDBR
01910 *        DATASET  (PLAN-FILE-ID)
01911 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006418' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01912
01913      
      * EXEC CICS  READ
01914 *        UPDATE
01915 *        DATASET  (PLAN-FILE-ID)
01916 *        SET      (ADDRESS OF PLAN-MASTER)
01917 *        RIDFLD   (WS-PLAN-KEY)
01918 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006422' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01919
01920      
      * EXEC CICS  DELETE
01921 *        DATASET  (PLAN-FILE-ID)
01922 *    END-EXEC.
      *    MOVE '&(                    &   #00006429' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01923
01924      GO TO 4805-STARTBR.
01925
01926  4889-EOF.
01927      
      * EXEC CICS  ENDBR
01928 *        DATASET  (PLAN-FILE-ID)
01929 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006436' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01930
01931  4899-EXIT.
01932       EXIT.
01933  EJECT
01934  4900-CREATE-NEW-ACCOUNT.
01935      PERFORM 6100-BUILD-KEY             THRU  6199-EXIT.
01936
01937      SET T-INDEX                 TO  LINEI.
01938
01939      IF PI-2ND-PAGE
01940          SET T-INDEX  UP  BY  8
01941      ELSE
01942      IF PI-3RD-PAGE
01943          SET T-INDEX  UP  BY  16
01944      ELSE
01945      IF PI-LST-PAGE
01946          SET T-INDEX  UP  BY  24.
01947
01948      IF EFFCHGL NOT =  ZERO
01949          PERFORM 6000-EDIT-EFFECTIVE-DATE   THRU  6000-EFF-EXIT
01950        ELSE
01951          MOVE PI-BIN-EFF-DT (T-INDEX)  TO  BIN-EFFCHG-SAVE.
01952
01953      IF EXPCHGL NOT =  ZERO
01954          PERFORM 6000-EDIT-EXPIRATION-DATE  THRU  6000-EXP-EXIT
01955        ELSE
01956          MOVE PI-BIN-EXP-DT (T-INDEX)  TO  BIN-EXPCHG-SAVE.
01957
01958      PERFORM 6700-VERIFY-STATE-CARRIER  THRU  6799-EXIT.
01959
01960      IF BIN-EXPCHG-SAVE NOT GREATER BIN-EFFCHG-SAVE
01961          MOVE -1                 TO  EXPCHGL
01962          MOVE ER-1228            TO  EMI-ERROR
01963          MOVE AL-UABON           TO  EXPCHGA
01964          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01965
01966      PERFORM 6000-EDIT-LINE  THRU  6000-LIN-EXIT.
01967
01968      IF EMI-ERROR  NOT = ZEROS
01969          GO TO 8200-SEND-DATAONLY.
01970
01971      MOVE SPACES                 TO  GETMAIN-SW.
01972
01973      IF NOT EMI-NO-ERRORS
01974          GO TO 8200-SEND-DATAONLY.
01975
01976      IF PI-PREV-ACCOUNT  =  LOW-VALUES
01977          MOVE ER-0453            TO  EMI-ERROR
01978          MOVE -1                 TO  MAINTL
01979          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01980          GO TO 8200-SEND-DATAONLY.
01981
01982      MOVE PI-ACCT-CCGSA-KEY      TO  NEW-KEY-SAVE.
01983
01984      PERFORM 4100-BUILD-VERIFICATION  THRU  4199-EXIT.
01985
111513     PERFORM 4200-CHECK-VALIDITY  THRU  4200-EXIT
01987          VARYING  WS-INDEX  FROM  +1  BY  +1
01988             UNTIL WS-INDEX GREATER +32.
01989
01990      IF NOT EMI-NO-ERRORS
01991          GO TO 8200-SEND-DATAONLY.
01992
01993      MOVE PI-PREV-ACCOUNT          TO  PI-ACCT-CCGSA-KEY.
01994      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT.
01995      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
01996
01997      PERFORM 6500-READ-ACCT     THRU  6599-EXIT.
01998
01999      MOVE LOW-VALUES             TO  AM-AR-HI-CERT-DATE
02000      MOVE ZEROS                  TO  AM-HI-CERT-DATE
02001                                      AM-LO-CERT-DATE
           MOVE ZEROS                  TO  AM-DCC-MAX-MARKETING-FEE
02002      MOVE SPACES                 TO  AM-1ST-PROD-DATE
02003                                      AM-PREV-DATES
121207                                     AM-ORIG-DEALER-NO
02004
02005      MOVE SPACES                 TO  AM-CERTS-PURGED-DATE.
02006      MOVE BIN-CURRENT-SAVE       TO  AM-LAST-MAINT-DT.
02007      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.
02008      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.
02009
02010      PERFORM 5500-ADD-NEW-AXRF-RECORD  THRU  5599-EXIT.
02011
02012      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
02013      MOVE 'A'                    TO  JP-RECORD-TYPE.
02014
02015      
      * EXEC CICS GETMAIN
02016 *        LENGTH   (ACCT-REC-LEN)
02017 *        SET      (ADDRESS OF ACCOUNT-MASTER)
02018 *        INITIMG  (GETMAIN-SPACE)
02019 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006526' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACCT-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02020
02021      MOVE JP-RECORD-AREA         TO  ACCOUNT-MASTER.
02022      MOVE NEW-KEY-SAVE           TO  AM-CONTROL-PRIMARY.
02023      MOVE LOW-VALUES             TO  AM-CNTRL-B.
02024      MOVE BIN-EFFCHG-SAVE        TO  AM-EFFECTIVE-DT.
02025      MOVE BIN-EXPCHG-SAVE        TO  AM-EXPIRATION-DT
02026                                      AM-VG-EXPIRATION-DT.
02027
02028      PERFORM 6400-BUILD-VG-KEY  THRU  6499-EXIT.
02029
02030      
      * EXEC CICS  HANDLE CONDITION
02031 *        DUPREC   (4580-DUPREC)
02032 *        DUPKEY   (4580-DUPREC)
02033 *    END-EXEC.
      *    MOVE '"$%$                  ! + #00006541' TO DFHEIV0
           MOVE X'222425242020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303036353431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02034
02035      
      * EXEC CICS  WRITE
02036 *        DATASET  (PI-ACCT-ID)
02037 *        RIDFLD   (AM-CONTROL-PRIMARY)
02038 *        FROM     (ACCOUNT-MASTER)
02039 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006546' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02040
02041      MOVE PI-ACCT-ID             TO  FILE-ID.
02042
02043      PERFORM 8400-LOG-JOURNAL-RECORD.
02044
02045      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
02046          NEXT SENTENCE
02047      ELSE
02048          PERFORM 4950-ADD-ERPLAN-RECORD THRU 4959-EXIT
02049          PERFORM 4960-ADD-ERCOMP-RECORD THRU 4969-EXIT.
02050
02051  4580-DUPREC.
02052      MOVE 1                  TO K-WITH-LINE-USED.
02053      MOVE LOW-VALUES         TO EL650AI.
02054
02055      GO TO 4590-SHOW-NEW-ACCOUNT.
02056
02057  EJECT
02058  4950-ADD-ERPLAN-RECORD.
02059      
      * EXEC CICS GETMAIN
02060 *        LENGTH   (PLAN-REC-LEN)
02061 *        SET      (ADDRESS OF PLAN-MASTER)
02062 *        INITIMG  (GETMAIN-SPACE)
02063 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006570' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 PLAN-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02064
02065      
      * EXEC CICS  HANDLE CONDITION
02066 *        DUPREC   (4959-EXIT)
02067 *        DUPKEY   (4959-EXIT)
02068 *    END-EXEC.
      *    MOVE '"$%$                  ! , #00006576' TO DFHEIV0
           MOVE X'222425242020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303036353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02069
02070      MOVE NEW-KEY-SAVE           TO  PL-CONTROL-PRIMARY.
02071
02072      MOVE EK-BTYPE               TO  PL-BENEFIT-TYPE.
02073      MOVE EK-BCODE               TO  PL-BENEFIT-CODE.
02074      MOVE EK-REVISION            TO  PL-REVISION-NO.
02075
02076      MOVE PL-CONTROL-PRIMARY     TO  PL-CONTROL-BY-VAR-GRP.
02077
02078      
      * EXEC CICS  WRITE
02079 *        DATASET  (PLAN-FILE-ID)
02080 *        RIDFLD   (PL-CONTROL-PRIMARY)
02081 *        FROM     (PLAN-MASTER)
02082 *    END-EXEC.
           MOVE LENGTH OF
            PLAN-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006589' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 PLAN-MASTER, 
                 DFHEIV11, 
                 PL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02083
02084  4959-EXIT.
02085      EXIT.
02086
02087  EJECT
02088  4960-ADD-ERCOMP-RECORD.
02089      
      * EXEC CICS GETMAIN
02090 *         LENGTH   (COMP-REC-LEN)
02091 *         SET      (ADDRESS OF COMPENSATION-MASTER)
02092 *         INITIMG  (GETMAIN-SPACE)
02093 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006600' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 COMP-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02094
02095      MOVE SPACES                 TO COMPENSATION-MASTER.
02096      MOVE ZEROS TO CO-BAL-FWD      CO-CUR-COM   CO-CUR-CHG
02097                    CO-CUR-PMT      CO-END-BAL   CO-CUR
02098                    CO-OV30         CO-OV60      CO-OV90
02099                    CO-YTD-COM      CO-YTD-OV    CO-CUR-OVR-UNDR
02100                    CO-YTD-OVR-UNDR CO-CUR-FICA CO-YTD-FICA
02101                    CO-LF-CLM-AMT   CO-AH-CLM-AMT
02102                    CO-CURRENT-BAL-FWD CO-CURRENT-CUR-COM
02103                    CO-CURRENT-CUR-CHG CO-CURRENT-END-BAL
02104                    CO-CURRENT-CUR     CO-CURRENT-OV30
02105                    CO-CURRENT-OV60    CO-CURRENT-OV90
02106                    CO-CURRENT-CUR-PMT
02107                    CO-CURRENT-YTD-COM CO-CURRENT-YTD-OV
02108                    CO-ACT-YEAR        CO-ACT-MONTH
02109                    CO-ACT-DAY         CO-LAST-STMT-YEAR
02110                    CO-LAST-STMT-MONTH CO-LAST-STMT-DAY
02111                    CO-CURRENT-LAST-STMT-YEAR
02112                    CO-CURRENT-LAST-STMT-MONTH
02113                    CO-CURRENT-LAST-STMT-DAY
02114                    CO-YTD-PAID-COM
080612                   CO-YTD-PAID-OV co-ov120 co-current-ov120
02117      MOVE BIN-CURRENT-SAVE       TO CO-LAST-MAINT-DT.
02118      MOVE PI-PROCESSOR-ID        TO CO-LAST-MAINT-USER.
02119      MOVE EIBTIME                TO CO-LAST-MAINT-HHMMSS.
02120      MOVE PI-CR-MONTH-END-DT     TO CO-ROLADEX-PRINT-DT.
02121      MOVE PI-COMPANY-CD          TO CO-COMPANY-CD.
02122
02123      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
02124          MOVE ZEROS              TO CO-CARRIER
02125      ELSE
02126          MOVE AM-CARRIER         TO CO-CARRIER.
02127
02128      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
02129          MOVE ZEROS              TO CO-GROUPING
02130      ELSE
02131          MOVE AM-GROUPING        TO CO-GROUPING.
02132
02133      MOVE 'A'                    TO CO-TYPE.
02134      MOVE AM-NAME                TO CO-ACCT-NAME.
02135      MOVE AM-PERSON              TO CO-MAIL-NAME.
02136      MOVE AM-ADDRS               TO CO-ADDR-1.
02137 *    MOVE AM-CITY                TO CO-ADDR-3.
           MOVE AM-ADDR-CITY           TO CO-ADDR-CITY
           MOVE AM-ADDR-STATE          TO CO-ADDR-STATE
02138      MOVE AM-ZIP                 TO CO-ZIP.
02139      MOVE AM-CSR-CODE            TO CO-CSR-CODE.
02140      MOVE AM-TEL-NO              TO CO-TELEPHONE.
02141      MOVE AM-ID-NO               TO CO-SOC-SEC.
02142
02143      IF AM-REMIT-TO NOT = ZERO
02144          MOVE AM-AGT (AM-REMIT-TO) TO CO-RESP-NO
02145        ELSE
02146          MOVE AM-ACCOUNT           TO CO-RESP-NO.
02147
02148      MOVE +0                      TO AGT-SUB.
02149
02150      PERFORM 4970-FIND-C-AGT THRU 4970-EXIT.
02151
02152      IF VALID-AGT-SW = 'Y'
02153          NEXT SENTENCE
02154      ELSE
02155          GO TO 4969-EXIT.
02156
02157      IF CO-RESP-NO = CO-ACCOUNT
02158          MOVE 'Y'                  TO CO-BALANCE-CONTROL
02159      ELSE
02160          MOVE 'N'                  TO CO-BALANCE-CONTROL.
02161
02162      IF PI-AR-PROCESSING
02163         MOVE 'G'                   TO CO-AR-REPORTING
02164         MOVE '1'                   TO CO-AR-BAL-LEVEL
02165         MOVE 'Y'                   TO CO-AR-PULL-CHECK
02166         MOVE 'Y'                   TO CO-AR-NORMAL-PRINT.
02167
02168      IF PI-COMPANY-ID = 'CRI' OR 'HER' OR 'HSL'
02169         MOVE 'N'                   TO CO-AR-REPORTING.
02170
02171      IF PI-COMPANY-ID = 'HER' OR 'HSL'
02172         MOVE '999999'              TO CO-AR-SUMMARY-CODE.
02173
02174      PERFORM 4980-ADD-COMPENSATION-NAME THRU 4980-EXIT.
02175
02176      
      * EXEC CICS HANDLE CONDITION
02177 *         DUPREC   (4969-EXIT)
02178 *         DUPKEY   (4969-EXIT)
02179 *    END-EXEC.
      *    MOVE '"$%$                  ! - #00006688' TO DFHEIV0
           MOVE X'222425242020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303036363838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02180
02181      
      * EXEC CICS WRITE
02182 *        FROM      (COMPENSATION-MASTER)
02183 *        RIDFLD    (CO-CONTROL-PRIMARY)
02184 *        DATASET   (COMP-FILE-ID)
02185 *    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006693' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02186
02187  4969-EXIT.
02188      EXIT.
02189
02190  4970-FIND-C-AGT.
02191      ADD 1 TO  AGT-SUB.
02192      IF AGT-SUB GREATER 10
02193          MOVE 'N'                TO  VALID-AGT-SW
02194          GO TO 4970-EXIT.
02195
052814     IF (AM-COM-TYP (AGT-SUB) = 'C' OR 'D' OR 'F')
02197          MOVE AM-AGT (AGT-SUB)   TO  CO-ACCOUNT
02198          MOVE 'Y'                TO  VALID-AGT-SW
02199          GO TO 4970-EXIT
02200      ELSE
02201          GO TO 4970-FIND-C-AGT.
02202
02203  4970-EXIT.
02204      EXIT.
02205
02206  4980-ADD-COMPENSATION-NAME.
02207      
      * EXEC CICS HANDLE CONDITION
02208 *         DUPREC   (4980-EXIT)
02209 *    END-EXEC.
      *    MOVE '"$%                   ! . #00006719' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303036373139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02210
02211      
      * EXEC CICS GETMAIN
02212 *         LENGTH   (NAME-REC-LEN)
02213 *         SET      (ADDRESS OF NAME-LOOKUP-MASTER)
02214 *         INITIMG  (GETMAIN-SPACE)
02215 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006723' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 NAME-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02216
02217      MOVE SPACES                 TO  NAME-LOOKUP-MASTER.
02218      MOVE BIN-CURRENT-SAVE       TO  NL-LAST-MAINT-DT.
02219      MOVE PI-PROCESSOR-ID        TO  NL-LAST-MAINT-USER.
02220      MOVE EIBTIME                TO  NL-LAST-MAINT-HHMMSS.
02221      MOVE PI-COMPANY-CD          TO  NL-COMPANY-CD.
02222      MOVE CO-ACCT-NAME           TO  NL-NAME.
02223      MOVE CO-ADDR-CITY           TO  NL-CITY
           MOVE CO-ADDR-STATE          TO  NL-ST
02224      MOVE 'C'                    TO  NL-RECORD-TYPE.
02225      MOVE PI-COMPANY-CD          TO  NL-CO-COMPANY-CD.
02226      MOVE CO-CARRIER             TO  NL-CO-CARRIER.
02227      MOVE CO-GROUPING            TO  NL-CO-GROUPING.
02228      MOVE CO-RESP-NO             TO  NL-CO-RESP-NO.
02229      MOVE CO-ACCOUNT             TO  NL-CO-ACCOUNT.
02230      MOVE CO-TYPE                TO  NL-CO-TYPE.
02231
02232      
      * EXEC CICS WRITE
02233 *        FROM      (NAME-LOOKUP-MASTER)
02234 *        RIDFLD    (NL-CONTROL-PRIMARY)
02235 *        DATASET   (NAME-FILE-ID)
02236 *    END-EXEC.
           MOVE LENGTH OF
            NAME-LOOKUP-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006745' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NAME-FILE-ID, 
                 NAME-LOOKUP-MASTER, 
                 DFHEIV11, 
                 NL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02237
02238  4980-EXIT.
02239      EXIT.
02240  EJECT
02241  5000-CREATE-NEW-RANGE.
02242
02243      PERFORM 6100-BUILD-KEY  THRU  6199-EXIT.
02244      PERFORM 6200-CHECK-KEY  THRU  6200-EXIT.
02245
02246      PERFORM 6000-EDIT-EFFECTIVE-DATE  THRU 6000-EFF-EXIT.
02247      PERFORM 6000-EDIT-EXPIRATION-DATE THRU 6000-EXP-EXIT.
02248
02249      SET T-INDEX TO PI-TOTAL-LINES.
02250
02251      PERFORM 600A-CHECK-HI-LO-DATES    THRU 600A-EXIT
02252         VARYING T-INDEX FROM T-INDEX BY +1
02253          UNTIL  T-INDEX GREATER PI-TOTAL-LINES.
02254
02255      IF NOT EMI-NO-ERRORS
02256          GO TO 8200-SEND-DATAONLY.
02257
02258      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT.
02259      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
02260
02261      IF BIN-EFFCHG-SAVE NOT LESS BIN-EXPCHG-SAVE
02262          MOVE -1                 TO  EFFCHGL EXPCHGL
02263          MOVE AL-UANON           TO  EFFCHGA EXPCHGA
02264          MOVE ER-2053            TO  EMI-ERROR
02265          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02266
02267      IF NOT EMI-NO-ERRORS
02268          GO TO 8200-SEND-DATAONLY.
02269
02270 ***************************************************************
02271 *    NO EFFECTIVE DATE ENTERED, WILL USE CURRENT EXPIRATION   *
02272 *    DATE IF WITHIN LIMITS                                    *
02273 ***************************************************************
02274
02275      IF EFFCHGL = ZERO
02276          SET T-INDEX             TO  PI-TOTAL-LINES
02277          IF PI-BIN-EXP-DT (T-INDEX)  =  HIGH-VALUES
02278              MOVE ER-2060        TO  EMI-ERROR
02279              MOVE -1             TO  EFFCHGL
02280              MOVE AL-UABON       TO  EFFCHGA
02281              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02282              GO TO 8200-SEND-DATAONLY
02283          ELSE
02284              IF PI-BIN-EXP-DT (T-INDEX) NOT LESS
02285                     BIN-EXPCHG-SAVE
02286                  MOVE ER-2061    TO  EMI-ERROR
02287                  MOVE -1         TO  EXPCHGL
02288                  MOVE AL-UABON   TO  EXPCHGA
02289                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02290                  GO TO 8200-SEND-DATAONLY.
02291
02292      PERFORM 4100-BUILD-VERIFICATION  THRU  4199-EXIT.
02293
111513*    display ' VERI TABLE BEFORE '
111513*    perform 4220-DISPLAY-VERI-TABLE
111513*                                thru 4220-exit
02294      IF EFFCHGL = ZERO
02295          MOVE PI-BIN-EXP-DT (T-INDEX)
02296                                  TO WS-ACCT-EFF-DT (WS-INDEX)
02297          MOVE BIN-EXPCHG-SAVE    TO WS-ACCT-EXP-DT (WS-INDEX)
02298      ELSE
02299          MOVE BIN-EFFCHG-SAVE    TO WS-ACCT-EFF-DT (WS-INDEX)
02300                                     WS-ACCT-EXP-DT (WS-INDEX - 1)
02301          MOVE BIN-EXPCHG-SAVE    TO WS-ACCT-EXP-DT (WS-INDEX).
02302
111513*    display ' VERI TABLE AFTER  '
111513*    perform 4220-DISPLAY-VERI-TABLE
111513*                                thru 4220-exit
02303      PERFORM 4300-VERIFY-INTEGRITY  THRU  4399-EXIT
02304          VARYING  WS-INDEX  FROM  +1  BY  +1
02305            UNTIL  WS-INDEX  GREATER +32.
02306
02307      IF NOT EMI-NO-ERRORS
02308          GO TO 8200-SEND-DATAONLY.
02309
02310      IF EFFCHGL  =  ZEROS
02311          MOVE PI-BIN-EXP-DT (T-INDEX)
02312                                   TO  PI-ACCT-EXP-DT
02313                                       BIN-EFFCHG-SAVE
02314                                       DC-BIN-DATE-1
02315          MOVE SPACE               TO  DC-OPTION-CODE
02316          PERFORM 9700-DATE-CONVERSION
02317 ***  Y2K PROJ 7744
02318          MOVE DC-GREG-DATE-1-YMD  TO  WS-EFFCHG-SAVE
02319          MOVE DC-ALPHA-CENTURY    TO  WS-EFFCHG-SAVE(4:2)
02320          MOVE WS-EFFCHG-SAVE      TO  EFFCHG-SAVE
02321
02322          PERFORM 5020-READ-PREV-REC
02323          PERFORM 5050-CREATE-NEW-REC  THRU  5050-EXIT
02324          GO TO 5005-EXIT
02325      END-IF.
02326 ***  Y2K PROJ 7744
02327
02328      IF NOT EMI-NO-ERRORS
02329          GO TO 8200-SEND-DATAONLY.
02330
02331      SET T-INDEX  TO  PI-TOTAL-LINES.
02332
CIDMOD*    IF (EFFCHGL GREATER THAN +0) AND
02334 *       (EXPCHGL GREATER THAN +0)
02335 *       IF (PI-BIN-EXP-DT (T-INDEX) EQUAL HIGH-VALUES) AND
02336 *          (BIN-EXPCHG-SAVE EQUAL HIGH-VALUES)
02337 *          NEXT SENTENCE
02338 *       ELSE
02339 *          GO TO 5006-FIND-EXP-DATE.
02340 *
02341
02342
02343 ***************************************************************
02344 *    EFFECTIVE DATE ENTERED, WILL REPLACE CURRENT EXPIRATION  *
02345 *    DATE IF ALL NINES AND CREATE NEW RANGE                   *
02346 ***************************************************************
02347      SET T-INDEX  TO  PI-TOTAL-LINES.
02348
02349      IF BROWSE-STARTED-SW EQUAL 'Y'
02350         PERFORM 6320-ENDBR THRU 6320-EXIT.
02351
02352      IF PI-BIN-EXP-DT (T-INDEX)  = HIGH-VALUES
02353          IF PI-BIN-EFF-DT (T-INDEX) NOT LESS BIN-EFFCHG-SAVE
02354              MOVE ER-2063             TO  EMI-ERROR
02355              MOVE -1                  TO  EFFCHGL
02356              MOVE AL-UABON            TO  EFFCHGA
02357              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02358              GO TO 8200-SEND-DATAONLY
02359          ELSE
02360              MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT
02361              MOVE LOW-VALUES              TO  PI-ACCT-REST-OF-EXP
02362              MOVE PI-ACCT-KEY         TO  B4-ACCT-KEY
02363              MOVE PI-BIN-EFF-DT (T-INDEX)
02364                                       TO  B4-ACCT-EFF-DT
02365              PERFORM 3100-DELETE-DATE-RANGE  THRU  3199-EXIT
02366              MOVE BIN-EFFCHG-SAVE     TO  AM-EXPIRATION-DT
02367                                           AM-VG-EXPIRATION-DT
02368                                           PI-BIN-EXP-DT
02369                                                     (T-INDEX)
02370              MOVE AM-CONTROL-PRIMARY  TO  AF-ACCT-KEY
02371              MOVE AM-EFFECTIVE-DT     TO  AF-ACCT-EFF-DT
02372              PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT
02373              PERFORM 3200-ADD-DATE-RANGE    THRU  3299-EXIT
02374              PERFORM 5050-CREATE-NEW-REC    THRU  5050-EXIT
02375              GO TO 5005-EXIT.
02376
02377 ***************************************************************
02378 *    EFFECTIVE DATE ENTERED, CHECK TO ENSURE EFFECTIVE        *
02379 *    DATE GREATER PREVIOUS EXPIRATION DATE                    *
02380 ***************************************************************
02381      SET T-INDEX  TO  PI-TOTAL-LINES.
02382
02383      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT.
02384      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
02385
02386      IF BIN-EFFCHG-SAVE LESS PI-BIN-EXP-DT (T-INDEX)
02387          PERFORM 5010-CHECK-VALID-NEW-RANGE  THRU  5010-EXIT
02388      ELSE
02389          PERFORM 5020-READ-PREV-REC
02390          PERFORM 5050-CREATE-NEW-REC  THRU  5050-EXIT.
02391
02392  5005-EXIT.
02393      MOVE SPACES                 TO  EFFCHGO
02394                                      EXPCHGI.
02395      MOVE AL-UANOF               TO  EFFCHGA
02396                                      EXPCHGA.
02397      MOVE ER-0000                TO  EMI-ERROR.
02398
02399      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02400
02401      GO TO 7000-BUILD-OUTPUT-MAP.
02402
02403      EJECT
02404  5006-FIND-EXP-DATE.
02405      MOVE LOW-VALUES             TO  PI-ACCT-EXP-DT
02406                                      PI-ACCT-REST-OF-EXP.
02407      PERFORM 6300-STARTBR  THRU  6300-EXIT.
02408
02409      PERFORM 6310-READNEXT  THRU  6310-EXIT
02410          VARYING  T-INDEX  FROM  +1  BY  +1
02411            UNTIL  PI-BIN-EXP-DT (T-INDEX)  > BIN-EFFCHG-SAVE.
02412
02413      IF BIN-EFFCHG-SAVE > PI-BIN-EXP-DT (T-INDEX)
02414         MOVE ER-1220             TO  EMI-ERROR
02415         MOVE -1                  TO  EFFCHGL
02416         MOVE AL-UABON            TO  EFFCHGA
02417         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02418         GO TO         8200-SEND-DATAONLY.
02419
02420      IF (BIN-EFFCHG-SAVE > PI-BIN-EFF-DT (T-INDEX)) AND
02421         (BIN-EXPCHG-SAVE < PI-BIN-EXP-DT (T-INDEX))
02422         PERFORM 5007-CREATE-SUB-DATE-RANGES THRU 5007-EXIT
02423         MOVE 1 TO PI-PAGE-NUMBER
02424         GO TO 1000-SHOW-ACCOUNT
02425      ELSE
02426      IF (BIN-EFFCHG-SAVE = PI-BIN-EFF-DT (T-INDEX)) AND
02427         (BIN-EXPCHG-SAVE < PI-BIN-EXP-DT (T-INDEX))
02428         PERFORM 5008-CREATE-SUB-DATE-RANGES THRU 5008-EXIT
02429         MOVE 1 TO PI-PAGE-NUMBER
02430         GO TO 1000-SHOW-ACCOUNT
02431      ELSE
02432         MOVE ER-2051             TO  EMI-ERROR
02433         MOVE -1                  TO  EFFCHGL
02434         MOVE AL-UABON            TO  EFFCHGA
02435         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02436         GO TO 8200-SEND-DATAONLY.
02437
02438  5006-EXIT. EXIT.
02439      EJECT
02440  5007-CREATE-SUB-DATE-RANGES.
02441
02442      IF BROWSE-STARTED-SW EQUAL 'Y'
02443         PERFORM 6320-ENDBR THRU 6320-EXIT.
02444
02445      IF NOT SYSTEM-MODIFY-CAP
02446          MOVE 'UPDATE'           TO  SM-READ
02447          PERFORM 9995-SECURITY-VIOLATION  THRU  9995-EXIT
02448          MOVE ER-0070            TO  EMI-ERROR
02449          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02450          GO TO 8100-SEND-INITIAL-MAP.
02451
02452      
      * EXEC CICS GETMAIN
02453 *        LENGTH   (ACCT-REC-LEN)
02454 *        SET      (ADDRESS OF ACCOUNT-MASTER)
02455 *        INITIMG  (GETMAIN-SPACE)
02456 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006971' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACCT-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02457
02458      
      * EXEC CICS  HANDLE CONDITION
02459 *        ENDFILE  (5007-EXIT)
02460 *        NOTFND   (5007-EXIT)
02461 *    END-EXEC.
      *    MOVE '"$''I                  ! / #00006977' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303036393737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02462
02463      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT.
02464      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
02465
02466      
      * EXEC CICS  READ
02467 *        UPDATE
02468 *        DATASET  (PI-ACCT-ID)
02469 *        SET      (ADDRESS OF ACCOUNT-MASTER)
02470 *        RIDFLD   (PI-ACCT-KEY)
02471 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006985' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02472
02473      MOVE AM-CONTROL-PRIMARY     TO  SAVE-ACCT-KEY.
02474      MOVE AM-EXPIRATION-DT       TO  SAVE-CURR-EXP-DT.
02475      MOVE AM-EFFECTIVE-DT        TO  SAVE-CURR-EFF-DT.
02476      MOVE AM-CONTROL-PRIMARY     TO  AF-ACCT-KEY.
02477      MOVE AM-EXPIRATION-DT       TO  AF-ACCT-EFF-DT.
02478      MOVE BIN-CURRENT-SAVE       TO  AM-LAST-MAINT-DT
02479      MOVE 'C'                    TO  JP-RECORD-TYPE.
02480      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
02481      PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT.
02482
02483      MOVE BIN-EXPCHG-SAVE     TO  AM-EFFECTIVE-DT
02484                                   PI-BIN-EFF-DT (T-INDEX).
02485      
      * EXEC CICS  REWRITE
02486 *        FROM     (ACCOUNT-MASTER)
02487 *        DATASET  (PI-ACCT-ID)
02488 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007004' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02489
02490      
      * EXEC CICS SYNCPOINT
02491 *    END-EXEC.
      *    MOVE '6"                    !   #00007009' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02492
02493      PERFORM 8400-LOG-JOURNAL-RECORD.
02494      PERFORM 6600-UPDATE-MAINT-DT  THRU  6699-EXIT.
02495      MOVE SAVE-ACCT-KEY       TO  AM-CONTROL-PRIMARY.
02496      MOVE LOW-VALUES          TO  AM-CNTRL-B.
02497      MOVE SAVE-CURR-EFF-DT    TO  PI-BIN-EFF-DT (T-INDEX)
02498                                   AM-EFFECTIVE-DT.
02499      MOVE BIN-EFFCHG-SAVE     TO  PI-BIN-EXP-DT (T-INDEX)
02500                                   AM-EXPIRATION-DT
02501                                   AM-VG-EXPIRATION-DT.
02502      MOVE BIN-CURRENT-SAVE    TO  AM-LAST-MAINT-DT
02503                                   PI-BIN-MAINT-DT (T-INDEX).
02504      MOVE AM-CONTROL-PRIMARY  TO  AF-ACCT-KEY.
02505      MOVE AM-EFFECTIVE-DT     TO  AF-ACCT-EFF-DT.
02506      MOVE LOW-VALUES          TO  AM-AR-HI-CERT-DATE
02507      MOVE SPACES              TO  AM-1ST-PROD-DATE
02508                                   AM-PREV-DATES.
02509      PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT.
02510
02511      
      * EXEC CICS  HANDLE CONDITION
02512 *        ENDFILE  (5007-EXIT)
02513 *        NOTFND   (5007-EXIT)
02514 *        DUPREC   (5007-EXIT)
02515 *        ILLOGIC  (5007-EXIT)
02516 *        INVREQ   (5007-EXIT)
02517 *    END-EXEC.
      *    MOVE '"$''I%38               ! 0 #00007030' TO DFHEIV0
           MOVE X'222427492533382020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303037303330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02518
02519      MOVE 'AM'                TO AM-RECORD-ID.
02520
02521      PERFORM 3200-ADD-DATE-RANGE    THRU  3299-EXIT.
02522      PERFORM 8400-LOG-JOURNAL-RECORD.
02523      PERFORM 6600-UPDATE-MAINT-DT   THRU  6699-EXIT.
02524
02525      MOVE SAVE-ACCT-KEY       TO  AM-CONTROL-PRIMARY.
02526      MOVE BIN-EFFCHG-SAVE     TO  AM-EFFECTIVE-DT
02527                                   PI-BIN-EFF-DT (T-INDEX).
02528      MOVE BIN-EXPCHG-SAVE     TO  PI-BIN-EXP-DT (T-INDEX)
02529                                   AM-EXPIRATION-DT
02530                                   AM-VG-EXPIRATION-DT.
02531      MOVE BIN-CURRENT-SAVE    TO  AM-LAST-MAINT-DT
02532                                   PI-BIN-MAINT-DT (T-INDEX).
02533      MOVE LOW-VALUES          TO  AM-AR-HI-CERT-DATE
02534      MOVE SPACES              TO  AM-1ST-PROD-DATE
02535                                   AM-PREV-DATES.
02536      MOVE AM-CONTROL-PRIMARY  TO  AF-ACCT-KEY.
02537      MOVE AM-EFFECTIVE-DT     TO  AF-ACCT-EFF-DT.
02538      PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT.
02539      PERFORM 3200-ADD-DATE-RANGE    THRU  3299-EXIT.
02540      PERFORM 8400-LOG-JOURNAL-RECORD.
02541      PERFORM 6600-UPDATE-MAINT-DT   THRU  6699-EXIT.
02542
02543      MOVE ZERO                TO EFFCHGL
02544                                  EXPCHGL.
02545
02546      
      * EXEC CICS  HANDLE CONDITION
02547 *         NOTFND   (5015-NOT-FOUND)
02548 *         ENDFILE  (5015-NOT-FOUND)
02549 *    END-EXEC.
      *    MOVE '"$I''                  ! 1 #00007065' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303037303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02550
02551  5007-EXIT. EXIT.
02552  EJECT
02553  5008-CREATE-SUB-DATE-RANGES.
02554
02555      IF BROWSE-STARTED-SW EQUAL 'Y'
02556         PERFORM 6320-ENDBR THRU 6320-EXIT.
02557
02558      IF NOT SYSTEM-MODIFY-CAP
02559          MOVE 'UPDATE'           TO  SM-READ
02560          PERFORM 9995-SECURITY-VIOLATION  THRU  9995-EXIT
02561          MOVE ER-0070            TO  EMI-ERROR
02562          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02563          GO TO 8100-SEND-INITIAL-MAP.
02564
02565      MOVE PI-BIN-EXP-DT (T-INDEX)  TO  PI-ACCT-EXP-DT.
02566      MOVE LOW-VALUES               TO  PI-ACCT-REST-OF-EXP.
02567      MOVE PI-ACCT-KEY              TO  B4-ACCT-KEY.
02568      MOVE PI-BIN-EFF-DT (T-INDEX)  TO  B4-ACCT-EFF-DT.
02569
02570      PERFORM 3100-DELETE-DATE-RANGE  THRU  3199-EXIT.
02571
02572      MOVE BIN-EXPCHG-SAVE        TO  AM-EXPIRATION-DT
02573                                      AM-VG-EXPIRATION-DT
02574                                      PI-BIN-EXP-DT (T-INDEX).
02575      MOVE BIN-CURRENT-SAVE       TO  AM-LAST-MAINT-DT
02576                                      PI-BIN-MAINT-DT (T-INDEX).
02577      MOVE AM-CONTROL-PRIMARY     TO  AF-ACCT-KEY.
02578      MOVE AM-EFFECTIVE-DT        TO  AF-ACCT-EFF-DT.
02579
02580      PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT.
02581
02582      PERFORM 3200-ADD-DATE-RANGE    THRU  3299-EXIT.
02583
02584      PERFORM 6600-UPDATE-MAINT-DT   THRU  6699-EXIT.
02585
02586      IF T-INDEX = PI-TOTAL-LINES
02587         MOVE ZERO             TO EFFCHGL
02588                                  EXPCHGL
02589         GO TO 5008-EXIT.
02590
02591      MOVE BIN-EXPCHG-SAVE        TO  PI-ACCT-EXP-DT
02592      MOVE LOW-VALUES             TO  PI-ACCT-REST-OF-EXP.
02593      PERFORM 6300-STARTBR  THRU  6300-EXIT.
02594
02595      PERFORM 6310-READNEXT  THRU  6310-EXIT.
02596
02597      PERFORM 6310-READNEXT  THRU  6310-EXIT
02598          VARYING  T-INDEX  FROM T-INDEX  BY  +1
02599            UNTIL  PI-BIN-EXP-DT (T-INDEX)  > BIN-EXPCHG-SAVE.
02600
02601      IF BROWSE-STARTED-SW EQUAL 'Y'
02602         PERFORM 6320-ENDBR THRU 6320-EXIT.
02603
02604      
      * EXEC CICS  READ
02605 *        UPDATE
02606 *        DATASET  (PI-ACCT-ID)
02607 *        SET      (ADDRESS OF ACCOUNT-MASTER)
02608 *        RIDFLD   (PI-ACCT-KEY)
02609 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007123' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02610
02611      MOVE 'B'                    TO  JP-RECORD-TYPE.
02612      MOVE ACCT-FILE-ID           TO  FILE-ID.
02613      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
02614
02615      PERFORM 8400-LOG-JOURNAL-RECORD.
02616
02617      MOVE BIN-EXPCHG-SAVE        TO  AM-EFFECTIVE-DT
02618                                      PI-BIN-EFF-DT (T-INDEX).
02619      MOVE EXPCHG-SAVE            TO  AM-EFFECT-DT.
02620      MOVE BIN-CURRENT-SAVE       TO  AM-LAST-MAINT-DT
02621                                      PI-BIN-MAINT-DT (T-INDEX).
02622      MOVE AM-CONTROL-PRIMARY     TO  AF-ACCT-KEY.
02623      MOVE AM-EFFECTIVE-DT        TO  AF-ACCT-EFF-DT.
02624      MOVE 'C'                    TO  JP-RECORD-TYPE.
02625      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
02626
02627      PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT.
02628
02629      
      * EXEC CICS  REWRITE
02630 *        FROM     (ACCOUNT-MASTER)
02631 *        DATASET  (PI-ACCT-ID)
02632 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007148' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02633
02634      PERFORM 8400-LOG-JOURNAL-RECORD.
02635
02636      PERFORM 6600-UPDATE-MAINT-DT  THRU  6699-EXIT.
02637
02638
02639
02640      MOVE ZERO                TO EFFCHGL
02641                                  EXPCHGL.
02642
02643      
      * EXEC CICS  HANDLE CONDITION
02644 *         NOTFND   (5015-NOT-FOUND)
02645 *         ENDFILE  (5015-NOT-FOUND)
02646 *    END-EXEC.
      *    MOVE '"$I''                  ! 2 #00007162' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303037313632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02647
02648  5008-EXIT. EXIT.
02649  EJECT
02650  5010-CHECK-VALID-NEW-RANGE.
02651
02652 ***************************************************************
02653 *    THIS PARAGRAPH ALLOWS YOU TO EXPIRE A RANGE PRIOR TO HI  *
02654 *    CERT DATE AS LONG AS A NEW RANGE IS BEING CREATED        *
02655 ***************************************************************
02656
02657      IF PI-AR-PROCESSING
02658         GO TO 5010-CHECK-AR-VALID-NEW-RANGE.
02659
02660      IF BIN-EXPCHG-SAVE GREATER PI-BIN-EXP-DT (T-INDEX)  AND
02661                         GREATER BIN-EFFCHG-SAVE          AND
02662                         GREATER PI-BIN-HI-CERT (T-INDEX)
02663          MOVE PI-BIN-EXP-DT (T-INDEX)
02664                                   TO  PI-ACCT-EXP-DT
02665          MOVE LOW-VALUES          TO  PI-ACCT-REST-OF-EXP
02666          MOVE PI-ACCT-KEY         TO  B4-ACCT-KEY
02667          MOVE PI-BIN-EFF-DT (T-INDEX)
02668                                   TO  B4-ACCT-EFF-DT
02669          PERFORM 3100-DELETE-DATE-RANGE  THRU  3199-EXIT
02670          MOVE BIN-EFFCHG-SAVE     TO  AM-EXPIRATION-DT
02671                                       AM-VG-EXPIRATION-DT
02672                                       PI-BIN-EXP-DT (T-INDEX)
02673          MOVE BIN-CURRENT-SAVE    TO  PI-BIN-MAINT-DT (T-INDEX)
02674          MOVE AM-CONTROL-PRIMARY  TO  AF-ACCT-KEY
02675          MOVE AM-EFFECTIVE-DT     TO  AF-ACCT-EFF-DT
02676          PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT
02677          PERFORM 3200-ADD-DATE-RANGE    THRU  3299-EXIT
02678          PERFORM 5050-CREATE-NEW-REC    THRU  5050-EXIT
02679      ELSE
02680          MOVE ER-2051             TO  EMI-ERROR
02681          MOVE -1                  TO  EFFCHGL
02682          MOVE AL-UABON            TO  EFFCHGA  EXPCHGA
02683          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02684          GO TO 8200-SEND-DATAONLY.
02685
02686      GO TO 5010-EXIT.
02687
02688  5010-CHECK-AR-VALID-NEW-RANGE.
02689
02690 ***************************************************************
02691 *    THIS PARAGRAPH ALLOWS YOU TO EXPIRE A RANGE PRIOR TO HI  *
02692 *    A/R CERT DATE AS LONG AS A NEW RANGE IS BEING CREATED    *
02693 ***************************************************************
02694
02695      IF BIN-EXPCHG-SAVE GREATER PI-BIN-EXP-DT (T-INDEX)  AND
02696                         GREATER BIN-EFFCHG-SAVE          AND
02697                         GREATER PI-BIN-AR-HI-CERT (T-INDEX)
02698          MOVE PI-BIN-EXP-DT (T-INDEX)
02699                                   TO  PI-ACCT-EXP-DT
02700          MOVE LOW-VALUES          TO  PI-ACCT-REST-OF-EXP
02701          MOVE PI-ACCT-KEY         TO  B4-ACCT-KEY
02702          MOVE PI-BIN-EFF-DT (T-INDEX)
02703                                   TO  B4-ACCT-EFF-DT
02704          PERFORM 3100-DELETE-DATE-RANGE  THRU  3199-EXIT
02705          MOVE BIN-EFFCHG-SAVE     TO  AM-EXPIRATION-DT
02706                                       AM-VG-EXPIRATION-DT
02707                                       PI-BIN-EXP-DT (T-INDEX)
02708          MOVE BIN-CURRENT-SAVE    TO  PI-BIN-MAINT-DT (T-INDEX)
02709          MOVE AM-CONTROL-PRIMARY  TO  AF-ACCT-KEY
02710          MOVE AM-EFFECTIVE-DT     TO  AF-ACCT-EFF-DT
02711          PERFORM 5600-CHG-AXRF-RECORDS  THRU  5699-EXIT
02712          PERFORM 3200-ADD-DATE-RANGE    THRU  3299-EXIT
02713          PERFORM 5050-CREATE-NEW-REC    THRU  5050-EXIT
02714      ELSE
02715          MOVE ER-2051             TO  EMI-ERROR
02716          MOVE -1                  TO  EFFCHGL
02717          MOVE AL-UABON            TO  EFFCHGA  EXPCHGA
02718          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02719          GO TO 8200-SEND-DATAONLY.
02720
02721  5010-EXIT.
02722      EXIT.
02723
02724  EJECT
02725  5015-NOT-FOUND.
02726      MOVE ER-0226                TO  EMI-ERROR.
02727      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02728      MOVE -1                 TO  MAINTL.
02729      GO TO 8200-SEND-DATAONLY.
02730
02731  5015-EXIT.
02732      EXIT.
02733  EJECT
02734  5020-READ-PREV-REC.
02735      
      * EXEC CICS  READ
02736 *        DATASET  (PI-ACCT-ID)
02737 *        SET      (ADDRESS OF ACCOUNT-MASTER)
02738 *        RIDFLD   (PI-ACCT-KEY)
02739 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007254' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02740
02741  5050-CREATE-NEW-REC.
02742      MOVE BIN-EFFCHG-SAVE        TO  AM-EFFECTIVE-DT.
02743      MOVE BIN-EXPCHG-SAVE        TO  AM-EXPIRATION-DT
02744                                      AM-VG-EXPIRATION-DT.
02745      MOVE EFFCHG-SAVE            TO  AM-EFFECT-DT.
02746      MOVE SPACES                 TO  AM-CERTS-PURGED-DATE
02747                                      AM-1ST-PROD-DATE
02748                                      AM-PREV-DATES
02749                                      AM-BILLING-STATUS.
02750      MOVE ZEROS                  TO  AM-HI-CERT-DATE
02751                                      AM-LO-CERT-DATE.
02752
02753      IF AM-AR-HI-CERT-DATE NOT LESS THAN AM-EFFECTIVE-DT
02754         NEXT SENTENCE
02755      ELSE
02756         MOVE LOW-VALUES          TO  AM-AR-HI-CERT-DATE.
02757
02758      MOVE BIN-CURRENT-SAVE       TO  AM-LAST-MAINT-DT.
02759      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.
02760      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.
02761
02762      SET T-INDEX  UP  BY  1.
02763      IF T-INDEX  GREATER 32
02764          MOVE ER-2062            TO  EMI-ERROR
02765          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02766          MOVE -1                 TO  MAINTL
02767          GO TO 8200-SEND-DATAONLY.
02768
02769      ADD 1                       TO  PI-TOTAL-LINES.
02770
02771      MOVE LOW-VALUES             TO  PI-TABLE-ENT    (T-INDEX).
02772      MOVE BIN-EFFCHG-SAVE        TO  PI-BIN-EFF-DT   (T-INDEX).
02773      MOVE BIN-EXPCHG-SAVE        TO  PI-BIN-EXP-DT   (T-INDEX).
02774      MOVE AM-AR-HI-CERT-DATE     TO  PI-BIN-AR-HI-CERT (T-INDEX).
02775      MOVE BIN-CURRENT-SAVE       TO  PI-BIN-MAINT-DT (T-INDEX).
02776
02777      PERFORM 5500-ADD-NEW-AXRF-RECORD  THRU  5599-EXIT.
02778
02779      MOVE 'A'                    TO  JP-RECORD-TYPE.
02780      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
02781      MOVE ACCT-FILE-ID           TO  FILE-ID.
02782
02783      
      * EXEC CICS  WRITE
02784 *        RIDFLD   (AM-CONTROL-PRIMARY)
02785 *        FROM     (ACCOUNT-MASTER)
02786 *        DATASET  (PI-ACCT-ID)
02787 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007302' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02788
02789      PERFORM 8400-LOG-JOURNAL-RECORD.
02790
02791      PERFORM 6600-UPDATE-MAINT-DT  THRU  6699-EXIT.
02792
02793  5050-EXIT.
02794      EXIT.
02795  EJECT
02796  5100-FIND-NEXT-RANGE.
02797      MOVE PI-PREV-VG-ACCOUNT     TO  KEY-SAVE.
02798
02799      IF PI-TOTAL-LINES  = ZERO
02800          GO TO 5210-NO-DATA.
02801
02802      IF PI-LST-PAGE
02803        OR (PI-FST-PAGE AND PI-TOTAL-LINES NOT GREATER 8)
02804        OR (PI-2ND-PAGE AND PI-TOTAL-LINES NOT GREATER 16)
02805        OR (PI-3RD-PAGE AND PI-TOTAL-LINES NOT GREATER 24)
02806          MOVE ER-2065            TO  EMI-ERROR
02807          MOVE -1                 TO  MAINTL
02808          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02809          GO TO 8200-SEND-DATAONLY.
02810
02811      IF PI-FST-PAGE
02812          MOVE 2                  TO  PI-PAGE-NUMBER
02813      ELSE
02814      IF PI-2ND-PAGE
02815          MOVE 3                  TO  PI-PAGE-NUMBER
02816      ELSE
02817      IF PI-3RD-PAGE
02818          MOVE 4                  TO  PI-PAGE-NUMBER.
02819
02820      GO TO 7000-BUILD-OUTPUT-MAP.
02821
02822  5200-FIND-PREV-RANGE.
02823      MOVE PI-PREV-VG-ACCOUNT     TO  KEY-SAVE.
02824
02825      IF PI-TOTAL-LINES  =  ZERO
02826          GO TO 5210-NO-DATA.
02827
02828      IF PI-FST-PAGE
02829          MOVE ER-2066            TO  EMI-ERROR
02830          MOVE -1                 TO  MAINTL
02831          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02832          GO TO 8200-SEND-DATAONLY.
02833
02834      IF PI-2ND-PAGE
02835          MOVE 1                  TO  PI-PAGE-NUMBER
02836      ELSE
02837      IF PI-3RD-PAGE
02838          MOVE 2                  TO  PI-PAGE-NUMBER
02839      ELSE
02840      IF PI-LST-PAGE
02841          MOVE 3                  TO  PI-PAGE-NUMBER.
02842
02843      GO TO 7000-BUILD-OUTPUT-MAP.
02844
02845  5210-NO-DATA.
02846      MOVE ER-2069                TO  EMI-ERROR.
02847      MOVE -1                     TO  MAINTL.
02848
02849      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02850
02851      GO TO 8200-SEND-DATAONLY.
02852  EJECT
02853  5300-FIND-NEXT-ACCOUNT.
02854      MOVE PI-PREV-ACCOUNT        TO  PI-ACCT-CCGSA-KEY.
02855      MOVE 'Y'                    TO  FIRST-TIME-SW.
02856
02857      IF PI-PREV-ACCOUNT  = LOW-VALUES
02858          MOVE PI-COMPANY-CD      TO  PI-ACCT-CO
02859      ELSE
02860          PERFORM 6100-BUILD-KEY  THRU  6199-EXIT
02861          MOVE HIGH-VALUES        TO  PI-ACCT-EXP-DT
02862          MOVE LOW-VALUES         TO  PI-ACCT-REST-OF-EXP.
02863
02864      IF NOT EMI-NO-ERRORS
02865          GO TO 8200-SEND-DATAONLY.
02866
02867      
      * EXEC CICS  HANDLE CONDITION
02868 *        NOTFND   (5340-END)
02869 *        ENDFILE  (5330-NOT-FOUND)
02870 *    END-EXEC.
      *    MOVE '"$I''                  ! 3 #00007386' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303037333836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02871
02872  5305-START-BR.
02873      MOVE ZEROS                  TO  PI-TOTAL-LINES.
02874
02875      SET T-INDEX
02876         TS-INDEX  TO  +1.
02877
02878      PERFORM 6300-STARTBR  THRU  6300-EXIT.
02879
02880  5310-READ-NEXT.
02881      PERFORM 6310-READNEXT  THRU  6310-EXIT.
02882
02883      IF PI-ACCT-CO  NOT = PI-COMPANY-CD
02884          GO TO 5330-NOT-FOUND.
02885
02886      IF PI-CARRIER-SECURITY GREATER SPACES
02887          IF PI-ACCT-CARRIER  =  PI-CARRIER-SECURITY
02888              NEXT SENTENCE
02889          ELSE
02890              GO TO 5310-READ-NEXT.
02891
02892      IF FIRST-TIME
02893          IF PI-ACCT-CCGSA-KEY  NOT = KEY-SAVE
02894              MOVE SPACES             TO  FIRST-TIME-SW
02895              MOVE PI-ACCT-CCGSA-KEY  TO  KEY-SAVE
02896                                          PI-PREV-VG-ACCOUNT
02897          ELSE
02898              GO TO 5310-READ-NEXT.
02899
02900      IF PI-PREV-ACCOUNT  =  LOW-VALUES
02901          IF PI-COMPANY-CD  = PI-ACCT-CO
02902              GO TO 5320-DISPLAY-RECORD
02903          ELSE
02904              MOVE ER-0066        TO  EMI-ERROR
02905              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02906              MOVE -1             TO  MAINTL
02907              GO TO 8200-SEND-DATAONLY.
02908
02909      IF PI-ACCT-CCGSA-KEY  = KEY-SAVE
02910          GO TO 5320-DISPLAY-RECORD
02911      ELSE
02912          MOVE 1                  TO  PI-PAGE-NUMBER
02913          GO TO 7000-BUILD-OUTPUT-MAP.
02914
02915  5320-DISPLAY-RECORD.
02916      IF T-INDEX  = 1
02917          MOVE LOW-VALUES         TO  PI-DATE-RANGE-TABLE
02918          MOVE SPACE              TO  NAMEFLGO
02919                                      PI-NAMEFLG.
02920
02921      IF AM-TRNTO-DTE  =  SPACES
02922          MOVE LOW-VALUES         TO  AM-TRNTO-DTE.
02923
02924      IF AM-TRNFROM-DTE  = SPACES
02925          MOVE LOW-VALUES         TO  AM-TRNFROM-DTE.
02926
02927      IF T-INDEX  = 1
02928          IF PI-COMPANY-ID  = 'LGX'  OR
02929                              'MIC'  OR  'MCC'
02930              MOVE AM-TRANSFERRED-TO  TO  TRNTOO
02931              MOVE AM-TRNTO-DTE       TO  DC-BIN-DATE-1
02932              MOVE SPACE              TO  DC-OPTION-CODE
02933              PERFORM 9700-DATE-CONVERSION
02934              IF DATE-CONVERSION-ERROR
02935                  MOVE SPACES         TO  TRNTODTO
02936              ELSE
02937                  MOVE DC-GREG-DATE-1-EDIT
02938                                      TO  TRNTODTO.
02939
02940      IF T-INDEX  = 1
02941          IF PI-COMPANY-ID  =  'LGX'  OR
02942                               'MIC'  OR  'MCC'
02943              MOVE AM-TRANSFERRED-FROM  TO  TRNFROMO
02944              MOVE AM-TRNFROM-DTE       TO  DC-BIN-DATE-1
02945              MOVE SPACE                TO  DC-OPTION-CODE
02946              PERFORM 9700-DATE-CONVERSION
02947              IF DATE-CONVERSION-ERROR
02948                  MOVE SPACES           TO  TRNFRDTO
02949              ELSE
02950                  MOVE DC-GREG-DATE-1-EDIT
02951                                        TO  TRNFRDTO.
02952
02953      IF NAMEO  NOT = LOW-VALUES
02954          IF AM-NAME  NOT = NAMEO
02955              MOVE '*'            TO  NAMEFLGO
02956                                      PI-NAMEFLG.
02957
02958      MOVE AM-CONTROL-PRIMARY     TO  PI-PREV-ACCOUNT.
02959
02960      IF T-INDEX  = 1
02961          MOVE AM-NAME                TO  NAMEO
02962                                          PI-ACCNAME
02963         ELSE
02964          MOVE PI-ACCNAME             TO  NAMEO.
02965
02966      PERFORM 1020-MOVE-TO-TABLE  THRU  1020-EXIT.
02967
02968      GO TO 5310-READ-NEXT.
02969
02970  5330-NOT-FOUND.
02971      IF BROWSE-STARTED
02972          PERFORM 6320-ENDBR  THRU  6320-EXIT
02973      ELSE
02974          GO TO 5340-END.
02975
02976      IF PI-ACCT-ACCOUNT  = LOW-VALUE
02977          MOVE ER-0472            TO  EMI-ERROR
02978          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02979          MOVE -1                 TO  MAINTL
02980          GO TO 8200-SEND-DATAONLY.
02981
02982      IF PI-ACCT-CCGSA-KEY  =  PI-PREV-ACCOUNT
02983        AND  T-INDEX  NOT = 1
02984          MOVE 1                  TO  PI-PAGE-NUMBER
02985          GO TO 7000-BUILD-OUTPUT-MAP
02986      ELSE
02987          IF T-INDEX  NOT = 1
02988              MOVE ER-0066        TO  EMI-ERROR
02989              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02990              MOVE 1              TO  PI-PAGE-NUMBER
02991              GO TO 7000-BUILD-OUTPUT-MAP.
02992
02993  5340-END.
02994      MOVE ER-0066                TO  EMI-ERROR.
02995
02996      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02997
02998      MOVE -1                     TO  MAINTL.
02999
03000      GO TO 8200-SEND-DATAONLY.
03001  EJECT
03002  5400-FIND-PREV-ACCOUNT.
03003      IF PI-PREV-ACCOUNT  = LOW-VALUES
03004          GO TO 5430-NOT-FOUND.
03005
03006      MOVE PI-PREV-ACCOUNT        TO  PI-ACCT-CCGSA-KEY.
03007      MOVE PI-BIN-EXP-DT (1)      TO  PI-ACCT-EXP-DT.
03008      MOVE LOW-VALUES             TO  PI-ACCT-REST-OF-EXP.
03009
03010      
      * EXEC CICS  HANDLE CONDITION
03011 *        NOTFND   (5440-NOT-FOUND)
03012 *        ENDFILE  (5430-NOT-FOUND)
03013 *    END-EXEC.
      *    MOVE '"$I''                  ! 4 #00007529' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303037353239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03014
03015      
      * EXEC CICS  STARTBR
03016 *        DATASET  (PI-ACCT-ID)
03017 *        RIDFLD   (PI-ACCT-KEY)
03018 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007534' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03019
03020      MOVE 'Y'                    TO  BROWSE-STARTED-SW
03021                                      FIRST-TIME-SW.
03022  5410-READ-PREV.
03023      
      * EXEC CICS  READPREV
03024 *        DATASET  (PI-ACCT-ID)
03025 *        SET      (ADDRESS OF ACCOUNT-MASTER)
03026 *        RIDFLD   (PI-ACCT-KEY)
03027 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007542' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03028
03029      IF FIRST-TIME
03030          MOVE SPACE              TO  FIRST-TIME-SW
03031          GO TO 5410-READ-PREV.
03032
03033      IF PI-ACCT-CO  NOT = PI-COMPANY-CD
03034          GO TO 5430-NOT-FOUND.
03035
03036      IF PI-CARRIER-SECURITY GREATER SPACES
03037          IF PI-ACCT-CARRIER  = PI-CARRIER-SECURITY
03038              NEXT SENTENCE
03039          ELSE
03040              GO TO 5410-READ-PREV.
03041
03042      MOVE AM-CONTROL-PRIMARY     TO  PI-PREV-ACCOUNT.
03043      MOVE PI-ACCT-CCGSA-KEY      TO  KEY-SAVE
03044                                      PI-PREV-VG-ACCOUNT.
03045      MOVE LOW-VALUES             TO  PI-ACCT-EXP-DT
03046                                      PI-ACCT-REST-OF-EXP
03047                                      PI-DATE-RANGE-TABLE.
03048
03049      MOVE SPACE                  TO  BROWSE-STARTED-SW.
03050
03051      
      * EXEC CICS  ENDBR
03052 *        DATASET  (PI-ACCT-ID)
03053 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007570' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03054
03055      GO TO 5305-START-BR.
03056
03057  5430-NOT-FOUND.
03058      IF BROWSE-STARTED
03059          MOVE SPACE              TO  BROWSE-STARTED-SW
03060          
      * EXEC CICS  ENDBR
03061 *            DATASET  (PI-ACCT-ID)
03062 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007579' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03063
03064      MOVE ER-0067                TO  EMI-ERROR.
03065
03066      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03067
03068      MOVE -1                     TO  MAINTL.
03069
03070      IF PI-PREV-ACCOUNT  = LOW-VALUES
03071          GO TO 5300-FIND-NEXT-ACCOUNT.
03072
03073      GO TO 8200-SEND-DATAONLY.
03074
03075  5440-NOT-FOUND.
03076      MOVE PI-PREV-ACCOUNT        TO  PI-ACCT-CCGSA-KEY.
03077
03078      PERFORM 6310-READNEXT THRU 6310-EXIT.
03079
03080      GO TO 5410-READ-PREV.
03081  EJECT
03082  5500-ADD-NEW-AXRF-RECORD.
03083      IF NOT PI-GA-BILLING
03084          GO TO 5599-EXIT.
03085
03086      MOVE +1                     TO  AXRF-SUB.
03087
03088 *    IF MAINTI  = 'K'
03089 *        IF GETMAIN-SW  = 'X'
03090 *            GO TO 5510-BUILD-AXRF-KEY.
03091
03092      MOVE ERGXRF-MAX-LEN         TO  ERGXRF-REC-LEN.
03093
03094 *    EXEC CICS  GETMAIN
03095 *        LENGTH   (ERGXRF-REC-LEN)
03096 *        SET      (ADDRESS OF AGENT-CROSS-REFERENCE)
03097 *        INITIMG  (GETMAIN-SPACE)
03098 *    END-EXEC.
pemcp *    display ' beginning of CP displays  '
pemcp *    display ' ergxrf-rec-len  ==   '  ergxrf-rec-len
pemcp *    display ' address         ==   '
pemcp *              address of agent-cross-reference
pemcp *    display ' contents        ==   '  agent-cross-reference (1:1)
pemcp *    display ' end       of CP displays  '
03099
03100 *    MOVE 'X'                    TO  GETMAIN-SW.
03101
03102  5510-BUILD-AXRF-KEY.
03103      MOVE ERGXRF-MAX-LEN         TO  ERGXRF-REC-LEN.
03104
03105      IF AXRF-SUB  =  +11
03106          GO TO 5599-EXIT.
03107
03108      IF AM-AGT (AXRF-SUB)  =  SPACES  OR ZEROS OR LOW-VALUES
03109          ADD +1 TO AXRF-SUB
03110          GO TO 5510-BUILD-AXRF-KEY.
03111
052814     IF (AM-COM-TYP (AXRF-SUB)  NOT = 'O' AND 'P' AND 'S'
011410         AND 'G' AND 'B' AND 'I' AND 'L' AND 'K' AND 'A')
03114          ADD +1 TO AXRF-SUB
03115          GO TO 5510-BUILD-AXRF-KEY.
03116
03117      MOVE PI-COMPANY-CD          TO  WS-AXRF-COMPANY-CD.
03118
03119      IF PI-ZERO-CARRIER  OR
03120         PI-ZERO-CAR-GROUP
03121          MOVE ZERO               TO  WS-AXRF-CARRIER
03122      ELSE
03123          MOVE AM-CARRIER         TO  WS-AXRF-CARRIER.
03124
03125      IF PI-ZERO-GROUPING  OR
03126         PI-ZERO-CAR-GROUP
03127          MOVE ZEROS              TO  WS-AXRF-GROUPING
03128      ELSE
03129          MOVE AM-GROUPING        TO  WS-AXRF-GROUPING.
03130
03131      MOVE AM-AGT (AXRF-SUB)      TO  WS-AXRF-AGENT-NO.
03132
03133      
      * EXEC CICS  HANDLE CONDITION
03134 *        ENDFILE  (5525-SKIP-REWRITE)
03135 *        NOTFND   (5525-SKIP-REWRITE)
03136 *    END-EXEC.
      *    MOVE '"$''I                  ! 5 #00007658' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303037363538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03137
03138      
      * EXEC CICS  READ
03139 *        DATASET  (ERGXRF-FILE-ID)
03140 *        LENGTH   (ERGXRF-REC-LEN)
03141 *        INTO     (AGENT-CROSS-REFERENCE)
03142 *        RIDFLD   (WS-AXRF-KEY)
03143 *        EQUAL
03144 *        UPDATE
03145 *    END-EXEC.
      *    MOVE '&"IL       EU         (   #00007663' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 WS-AXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03146
03147      MOVE GX-AGENT-POINTER-CNT   TO  GX-AGENT-POINTER-CNT.
03148      MOVE +1                     TO  GX-AGENT-SUB.
03149
03150      IF GX-AGENT-POINTER-CNT = +1006
03151          GO TO 5525-SKIP-REWRITE.
03152
03153
03154  5520-FIND-REC-LOCATION.
03155      MOVE AM-COMPANY-CD          TO  GX-AM-COMPANY-CD.
03156      MOVE GX-AGENT-POINTER (GX-AGENT-SUB)
03157                                  TO  GX-REST-OF-KEY.
03158      MOVE LOW-VALUES             TO  GX-AM-CNTL-FILLER.
03159
03160      IF GX-AM-CONTROL-PRIMARY GREATER AM-CONTROL-PRIMARY  OR
03161         GX-AGENT-SUB          GREATER GX-AGENT-POINTER-CNT
03162          NEXT SENTENCE
03163      ELSE
03164          ADD +1 TO GX-AGENT-SUB
03165          GO TO 5520-FIND-REC-LOCATION.
03166
03167      MOVE GX-AGENT-POINTER-CNT   TO  COMM-WORK-SUB
03168                                      COMM-WORK-SUB2.
03169
03170      ADD +1 TO COMM-WORK-SUB2.
03171
03172      IF GX-AGENT-SUB GREATER GX-AGENT-POINTER-CNT
03173          NEXT SENTENCE
03174       ELSE
03175          PERFORM 5530-BUMP-AXRF-REC
03176              VARYING  COMM-WORK-SUB  FROM  COMM-WORK-SUB  BY  -1
03177                UNTIL  COMM-WORK-SUB2 = GX-AGENT-SUB.
03178
03179      ADD ERGXRF-INC-LEN TO  ERGXRF-REC-LEN.
03180      ADD  +1 TO  GX-AGENT-POINTER-CNT.
03181
03182      MOVE AM-CARRIER        TO  GX-AM-CARRIER   (COMM-WORK-SUB2).
03183      MOVE AM-GROUPING       TO  GX-AM-GROUPING  (COMM-WORK-SUB2).
03184      MOVE AM-STATE          TO  GX-AM-STATE     (COMM-WORK-SUB2).
03185      MOVE AM-ACCOUNT        TO  GX-AM-ACCOUNT   (COMM-WORK-SUB2).
03186      MOVE AM-EXPIRATION-DT  TO  GX-AM-EXPIRATION-DT
03187                                                 (COMM-WORK-SUB2).
03188      MOVE AM-EFFECTIVE-DT   TO  GX-AM-EFF-DT    (COMM-WORK-SUB2).
03189      MOVE AXRF-SUB          TO  GX-AM-LEVEL-NO  (COMM-WORK-SUB2).
03190      MOVE LOW-VALUES        TO  GX-LAST-BILL-DT (COMM-WORK-SUB2).
03191      MOVE BIN-CURRENT-SAVE  TO  GX-LAST-MAINT-DT.
03192      MOVE EIBTIME           TO  GX-LAST-MAINT-HHMMSS.
03193      MOVE PI-PROCESSOR-ID   TO  GX-LAST-MAINT-USER.
pemgm      compute ergxrf-rec-len = (gx-agent-pointer-cnt * +31) + +78
pemgm      if ergxrf-rec-len > +31264
pemgm         move +31264          to ergxrf-rec-len
pemgm         move +1006           to gx-agent-pointer-cnt
pemgm      end-if
03194
03195      
      * EXEC CICS  REWRITE
03196 *        FROM     (AGENT-CROSS-REFERENCE)
03197 *        LENGTH   (ERGXRF-REC-LEN)
03198 *        DATASET  (ERGXRF-FILE-ID)
03199 *    END-EXEC.
      *    MOVE '&& L                  %   #00007725' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03200
03201  5525-SKIP-REWRITE.
03202
03203      ADD +1 TO AXRF-SUB.
03204
03205      GO TO 5510-BUILD-AXRF-KEY.
03206
03207  5530-BUMP-AXRF-REC.
03208      MOVE GX-AGENT-POINTER (COMM-WORK-SUB)
03209                           TO  GX-AGENT-POINTER (COMM-WORK-SUB2).
03210
03211      SUBTRACT +1  FROM  COMM-WORK-SUB2.
03212
03213  5599-EXIT.
03214      EXIT.
03215  EJECT
03216  5600-CHG-AXRF-RECORDS.
03217      IF NOT PI-GA-BILLING
03218          GO TO 5699-EXIT.
03219
03220      MOVE +1                     TO  AXRF-SUB.
03221      MOVE ERGXRF-MAX-LEN         TO  ERGXRF-REC-LEN.
03222
03223 *    IF GETMAIN-SW  = 'X'
03224 *        GO TO 5610-BUILD-AXRF-KEY.
03225
03226 *    EXEC CICS  GETMAIN
03227 *        LENGTH   (ERGXRF-REC-LEN)
03228 *        SET      (ADDRESS OF AGENT-CROSS-REFERENCE)
03229 *        INITIMG  (GETMAIN-SPACE)
03230 *    END-EXEC.
      *
pemcp *    display ' beginning of CP displays  '
pemcp *    display ' ergxrf-rec-len  ==   '  ergxrf-rec-len
pemcp *    display ' address         ==   '
pemcp *              address of agent-cross-reference
pemcp *    display ' contents        ==   '  agent-cross-reference (1:1)
pemcp *    display ' end       of CP displays  '
03231
03232 *    MOVE 'X'                    TO  GETMAIN-SW.
03233
03234  5610-BUILD-AXRF-KEY.
03235      IF AXRF-SUB  = +11
03236          GO TO 5699-EXIT.
03237
03238      IF AM-AGT (AXRF-SUB)  = SPACES OR ZEROS OR LOW-VALUES
03239          ADD +1 TO AXRF-SUB
03240          GO TO 5610-BUILD-AXRF-KEY.
03241
052814     IF (AM-COM-TYP (AXRF-SUB) NOT = 'O' AND 'P' AND 'S'
011410         AND 'G' AND 'B' AND 'I' AND 'L' AND 'K' AND 'A')
03244          ADD +1 TO AXRF-SUB
03245          GO TO 5610-BUILD-AXRF-KEY.
03246
03247      MOVE ERGXRF-MAX-LEN         TO  ERGXRF-REC-LEN.
03248      MOVE PI-COMPANY-CD          TO  WS-AXRF-COMPANY-CD.
03249
03250      IF PI-ZERO-CARRIER OR
03251         PI-ZERO-CAR-GROUP
03252          MOVE ZERO               TO  WS-AXRF-CARRIER
03253      ELSE
03254          MOVE AM-CARRIER         TO  WS-AXRF-CARRIER.
03255
03256      IF PI-ZERO-GROUPING  OR
03257         PI-ZERO-CAR-GROUP
03258          MOVE ZEROS              TO  WS-AXRF-GROUPING
03259      ELSE
03260          MOVE AM-GROUPING        TO  WS-AXRF-GROUPING.
03261
03262      MOVE AM-AGT (AXRF-SUB)      TO  WS-AXRF-AGENT-NO.
03263
03264      
      * EXEC CICS  HANDLE CONDITION
03265 *        ENDFILE  (5615-SKIP-REWRITE)
03266 *        NOTFND   (5615-SKIP-REWRITE)
03267 *    END-EXEC.
      *    MOVE '"$''I                  ! 6 #00007801' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303037383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03268
03269      
      * EXEC CICS  READ
03270 *        DATASET  (ERGXRF-FILE-ID)
03271 *        LENGTH   (ERGXRF-REC-LEN)
03272 *        INTO     (AGENT-CROSS-REFERENCE)
03273 *        RIDFLD   (WS-AXRF-KEY)
03274 *        EQUAL
03275 *        UPDATE
03276 *    END-EXEC.
      *    MOVE '&"IL       EU         (   #00007806' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 WS-AXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03277
03278      MOVE GX-AGENT-POINTER-CNT   TO  GX-AGENT-POINTER-CNT.
03279
03280      PERFORM VARYING GX-AGENT-SUB FROM +1 BY +1
03281              UNTIL
03282        (GX-AGENT-SUB GREATER GX-AGENT-POINTER-CNT)
03283             OR
03284        (GX-AM-ACCOUNT       (GX-AGENT-SUB) = B4-ACCT-ACCOUNT  AND
03285         GX-AM-EXPIRATION-DT (GX-AGENT-SUB) = B4-ACCT-EXP-DT   AND
03286         GX-AM-EFF-DT        (GX-AGENT-SUB) = B4-ACCT-EFF-DT   AND
03287         GX-AM-CARRIER       (GX-AGENT-SUB) = B4-ACCT-CARRIER  AND
03288         GX-AM-GROUPING      (GX-AGENT-SUB) = B4-ACCT-GROUPING AND
03289         GX-AM-STATE         (GX-AGENT-SUB) = B4-ACCT-STATE)
03290      END-PERFORM.
03291
03292      IF MAINTI  = 'D'
03293         IF GX-AGENT-SUB NOT GREATER GX-AGENT-POINTER-CNT
03294             MOVE GX-AGENT-SUB    TO  COMM-WORK-SUB
03295             PERFORM 5630-BUMP-RECORDS
03296                 VARYING  GX-AGENT-SUB  FROM  GX-AGENT-SUB  BY  +1
03297                   UNTIL  GX-AGENT-SUB  = GX-AGENT-POINTER-CNT
03298             SUBTRACT ERGXRF-INC-LEN  FROM  ERGXRF-REC-LEN
03299             SUBTRACT +1              FROM  GX-AGENT-POINTER-CNT
03300             GO TO 5615-REWRITE-AXRF-RECORD.
03301
03302      IF GX-AGENT-SUB NOT GREATER GX-AGENT-POINTER-CNT
03303         MOVE AF-ACCT-EFF-DT  TO GX-AM-EFF-DT        (GX-AGENT-SUB)
03304         MOVE AF-ACCT-EXP-DT  TO GX-AM-EXPIRATION-DT (GX-AGENT-SUB)
03305         MOVE AXRF-SUB        TO GX-AM-LEVEL-NO      (GX-AGENT-SUB)
03306         GO TO 5615-REWRITE-AXRF-RECORD.
03307
03308      
      * EXEC CICS  UNLOCK
03309 *        DATASET  (ERGXRF-FILE-ID)
03310 *    END-EXEC.
      *    MOVE '&*                    #   #00007845' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03311
03312      GO TO 5615-SKIP-REWRITE.
03313
03314  5615-REWRITE-AXRF-RECORD.
03315      IF MAINTI  = 'D'
03316        AND  GX-AGENT-POINTER-CNT  =  +0
03317          
      * EXEC CICS  DELETE
03318 *            DATASET  (ERGXRF-FILE-ID)
03319 *        END-EXEC
      *    MOVE '&(                    &   #00007854' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03320      ELSE
03321          MOVE BIN-CURRENT-SAVE   TO  GX-LAST-MAINT-DT
03322          MOVE EIBTIME            TO  GX-LAST-MAINT-HHMMSS
03323          MOVE PI-PROCESSOR-ID    TO  GX-LAST-MAINT-USER
pemgm          compute ergxrf-rec-len =
pemgm               (gx-agent-pointer-cnt * +31) + +78
pemgm          if ergxrf-rec-len > +31264
pemgm             move +31264          to ergxrf-rec-len
pemgm             move +1006           to gx-agent-pointer-cnt
pemgm          end-if
03324          
      * EXEC CICS  REWRITE
03325 *            FROM     (AGENT-CROSS-REFERENCE)
03326 *            LENGTH   (ERGXRF-REC-LEN)
03327 *            DATASET  (ERGXRF-FILE-ID)
03328 *        END-EXEC.
      *    MOVE '&& L                  %   #00007867' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03329
03330  5615-SKIP-REWRITE.
03331      ADD +1  TO  AXRF-SUB.
03332
03333      GO TO 5610-BUILD-AXRF-KEY.
03334
03335  5630-BUMP-RECORDS.
03336      ADD +1  TO  COMM-WORK-SUB.
03337
03338      MOVE GX-AGENT-POINTER (COMM-WORK-SUB)
03339                             TO  GX-AGENT-POINTER (GX-AGENT-SUB).
03340
03341  5699-EXIT.
03342      EXIT.
03343  EJECT
03344  6000-EDIT-EFFECTIVE-DATE.
03345      IF EFFCHGL  NOT =  ZEROS
03346          MOVE EFFCHGI                  TO  WS-DT-DEEDIT-FIELD
03347          PERFORM 8600-DEEDIT  THRU 8600-EXIT
03348          MOVE WS-DEEDIT-FIELD-DATE-OUT TO  DC-GREG-DATE-1-MDY
03349          MOVE '4'                      TO  DC-OPTION-CODE
03350          PERFORM 9700-DATE-CONVERSION
03351          IF DATE-CONVERSION-ERROR
03352              MOVE ER-0348              TO  EMI-ERROR
03353              MOVE -1                   TO  EFFCHGL
03354              MOVE AL-UABON             TO  EFFCHGA
03355              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03356          ELSE
03357              MOVE DC-GREG-DATE-1-EDIT  TO  EFFCHGI
03358              MOVE AL-UANON             TO  EFFCHGA
03359 ***  Y2K PROJ 7744
03360              MOVE DC-GREG-DATE-1-YMD   TO  WS-EFFCHG-SAVE
03361              MOVE DC-ALPHA-CENTURY     TO  WS-EFFCHG-SAVE(4:2)
03362              MOVE WS-EFFCHG-SAVE       TO  EFFCHG-SAVE
03363              MOVE DC-BIN-DATE-1        TO  BIN-EFFCHG-SAVE
03364          END-IF
03365 ***  Y2K PROJ 7744
03366      ELSE
03367          IF MAINTI  NOT = 'N'
03368              MOVE ER-0216              TO  EMI-ERROR
03369              MOVE -1                   TO  EFFCHGL
03370              MOVE AL-UABON             TO  EFFCHGA
03371              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03372
03373  6000-EFF-EXIT.
03374      EXIT.
03375  EJECT
03376  6000-EDIT-EXPIRATION-DATE.
03377 ***  Y2K PROJ 7744
03378      IF EXPCHGL  NOT = ZEROS
03379          MOVE EXPCHGI                  TO  WS-DT-DEEDIT-FIELD
03380          PERFORM 8600-DEEDIT  THRU 8600-EXIT
03381          IF WS-DEEDIT-FIELD-DATE-OUT NOT LESS 999999
03382              MOVE WS-DEEDIT-FIELD-DATE-OUT TO  EXPCHGO
03383              INSPECT EXPCHGI CONVERTING SPACES TO '/'
03384              MOVE HIGH-VALUES              TO  BIN-EXPCHG-SAVE
03385              MOVE 99999999999              TO  EXPCHG-SAVE
03386          ELSE
03387              MOVE WS-DEEDIT-FIELD-DATE-OUT TO  DC-GREG-DATE-1-MDY
03388              MOVE '4'                      TO  DC-OPTION-CODE
03389              PERFORM 9700-DATE-CONVERSION
03390              IF DATE-CONVERSION-ERROR
03391                  MOVE ER-0454              TO  EMI-ERROR
03392                  MOVE -1                   TO  EXPCHGL
03393                  MOVE AL-UABON             TO  EXPCHGA
03394                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03395              ELSE
03396                  MOVE DC-GREG-DATE-1-EDIT  TO  EXPCHGI
03397                  MOVE AL-UANON             TO  EXPCHGA
03398                  MOVE DC-GREG-DATE-1-YMD   TO  WS-EXPCHG-SAVE
03399                  MOVE DC-ALPHA-CENTURY     TO  WS-EXPCHG-SAVE(4:2)
03400                  MOVE WS-EXPCHG-SAVE       TO  EXPCHG-SAVE
03401                  MOVE DC-BIN-DATE-1        TO  BIN-EXPCHG-SAVE
03402              END-IF
03403          END-IF
03404      ELSE
03405          IF MAINTI  NOT =  'N'
03406              MOVE ER-2058                  TO  EMI-ERROR
03407              MOVE -1                       TO  EXPCHGL
03408              MOVE AL-UABON                 TO  EXPCHGA
03409              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03410          ELSE
03411              MOVE HIGH-VALUES              TO  BIN-EXPCHG-SAVE
03412              MOVE 99999999999              TO  EXPCHG-SAVE
03413          END-IF
03414      END-IF.
03415 ***  Y2K PROJ 7744
03416
03417  6000-EXP-EXIT.
03418      EXIT.
03419  EJECT
03420  6000-EDIT-LINE.
03421      IF LINEL  = ZEROS
03422          GO TO 6000-NO-LINE.
03423
03424      MOVE AL-UNNON               TO  LINEA.
03425
03426      IF LINEI NOT NUMERIC
03427          GO TO 6000-LINE-ERROR.
03428
03429      IF LINEI LESS 1 OR GREATER 8 OR GREATER PI-TOTAL-LINES
03430          GO TO 6000-LINE-ERROR.
03431
03432      MOVE PI-TOTAL-LINES         TO WS-PAGE-LINES.
03433
03434      IF PI-2ND-PAGE
03435          SUBTRACT 8 FROM WS-PAGE-LINES
03436      ELSE
03437      IF PI-3RD-PAGE
03438          SUBTRACT 16 FROM WS-PAGE-LINES
03439      ELSE
03440      IF PI-LST-PAGE
03441          SUBTRACT 24 FROM WS-PAGE-LINES.
03442
03443      IF LINEI GREATER WS-PAGE-LINES
03444          GO TO 6000-LINE-ERROR.
03445
03446      MOVE LINEI                  TO  PI-LINE-SELECTED.
03447
03448      GO TO 6000-LIN-EXIT.
03449
03450  6000-NO-LINE.
03451      IF PI-TOTAL-LINES  = 1
03452          MOVE 1                  TO  PI-LINE-SELECTED
03453                                      LINEI
03454          MOVE AL-UNNON           TO  LINEA
03455          GO TO 6000-LIN-EXIT.
03456
03457  6000-LINE-ERROR.
03458      MOVE ER-2064                TO  EMI-ERROR.
03459      MOVE -1                     TO  LINEL.
03460      MOVE AL-UABON               TO  LINEA.
03461
03462      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03463
03464  6000-LIN-EXIT.
03465      EXIT.
03466  EJECT
03467  600A-CHECK-HI-LO-DATES.
03468
03469      IF PI-BIN-HI-CERT (T-INDEX)  =  LOW-VALUES
03470          GO TO 600A-CHECK-AR-DATES.
03471
03472      IF BIN-EFFCHG-SAVE GREATER THAN PI-BIN-EFF-DT (T-INDEX)
03473      IF (PI-BIN-HI-CERT (T-INDEX) NOT LESS BIN-EXPCHG-SAVE)
03474          MOVE ER-2113            TO  EMI-ERROR
03475          MOVE -1                 TO  MAINTL
03476          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03477
03478 *    IF (PI-BIN-HI-CERT (T-INDEX) NOT LESS BIN-EXPCHG-SAVE)
03479 *        MOVE ER-2113            TO  EMI-ERROR
03480 *        MOVE -1                 TO  MAINTL
03481 *        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03482
03483  600A-CHECK-AR-DATES.
03484      IF NOT PI-AR-PROCESSING
03485          GO TO 600A-EXIT.
03486
03487      IF PI-BIN-AR-HI-CERT (T-INDEX) = LOW-VALUES OR SPACES
03488         GO TO 600A-EXIT.
03489
03490      IF (PI-BIN-AR-HI-CERT (T-INDEX) NOT LESS BIN-EXPCHG-SAVE)
03491          MOVE ER-2113            TO  EMI-ERROR
03492          MOVE -1                 TO  MAINTL
03493          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03494
03495  600A-EXIT.
03496      EXIT.
03497  EJECT
03498  6010-DATE-CHECK.
03499      IF MAINTI  = 'E'
03500          GO TO 6010-CHECK-EFF.
03501
03502      IF PI-BIN-HI-CERT (T-INDEX)  NOT = LOW-VALUES
03503          IF BIN-EXPCHG-SAVE NOT GREATER PI-BIN-HI-CERT (T-INDEX)
03504              MOVE ER-2072        TO  EMI-ERROR
03505              MOVE -1             TO  EXPCHGL
03506              MOVE AL-UABON       TO  EXPCHGA
03507              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03508
03509      IF BIN-EXPCHG-SAVE NOT GREATER PI-BIN-EFF-DT (T-INDEX)
03510          MOVE ER-2074            TO  EMI-ERROR
03511          MOVE -1                 TO  EXPCHGL
03512          MOVE AL-UABON           TO  EXPCHGA
03513          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03514
03515      GO TO 6010-EXIT.
03516
03517  6010-CHECK-EFF.
03518      IF PI-BIN-LO-CERT (T-INDEX)  NOT =  LOW-VALUES
03519         IF (BIN-EFFCHG-SAVE >= PI-BIN-LO-CERT (T-INDEX))
                 AND (PI-PROCESSOR-ID NOT = 'PEMA')
03520            MOVE ER-2073          TO EMI-ERROR
03521            MOVE -1               TO EFFCHGL
03522            MOVE AL-UABON         TO EFFCHGA
03523            PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
03524
03525      IF BIN-EFFCHG-SAVE NOT LESS PI-BIN-EXP-DT (T-INDEX)
03526          MOVE ER-2075            TO  EMI-ERROR
03527          MOVE -1                 TO  EFFCHGL
03528          MOVE AL-UABON           TO  EFFCHGA
03529          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03530
03531  6010-EXIT.
03532      EXIT.
03533  EJECT
03534  6100-BUILD-KEY.
03535      MOVE LOW-VALUES             TO  PI-ACCT-KEY.
03536      MOVE PI-COMPANY-CD          TO  PI-ACCT-CO.
03537
03538      IF ACARIERI  =  SPACES  OR  LOW-VALUES
03539          IF MAINTI  = 'S'
03540            OR  FIRST-TIME
03541              MOVE SPACES         TO  PI-ACCT-CARRIER
03542          ELSE
03543              MOVE -1             TO  ACARIERL
03544              MOVE ER-2059        TO  EMI-ERROR
03545              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03546      ELSE
03547          MOVE AL-UANON           TO  ACARIERA
03548          MOVE ACARIERI           TO  PI-ACCT-CARRIER.
03549
03550      IF AGROUPI  =  SPACES  OR  LOW-VALUES
03551          IF MAINTI  = 'S'
03552            OR  FIRST-TIME
03553              MOVE SPACES         TO  PI-ACCT-GROUPING
03554          ELSE
03555              MOVE -1             TO  AGROUPL
03556              MOVE ER-2059        TO  EMI-ERROR
03557              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03558      ELSE
03559          IF AGROUPI  =  '0     '
03560              MOVE -1             TO  AGROUPL
03561              MOVE ER-0195        TO  EMI-ERROR
03562              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03563          ELSE
03564              MOVE AL-UANON       TO  AGROUPA
03565              MOVE AGROUPI        TO  PI-ACCT-GROUPING.
03566
03567      IF ASTATEI  = SPACES  OR  LOW-VALUES
03568          IF MAINTI  =  'S'
03569            OR  FIRST-TIME
03570              MOVE SPACES         TO  PI-ACCT-STATE
03571          ELSE
03572              MOVE -1             TO  ASTATEL
03573              MOVE ER-2059        TO  EMI-ERROR
03574              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03575      ELSE
03576          MOVE AL-UANON           TO  ASTATEA
03577          MOVE ASTATEI            TO  PI-ACCT-STATE.
03578
03579      IF AACCTI  = SPACES  OR  LOW-VALUES
03580         IF MAINTI  = 'S'
03581           OR  FIRST-TIME
03582             MOVE SPACES          TO  PI-ACCT-ACCOUNT
03583         ELSE
03584             MOVE -1              TO  AACCTL
03585             MOVE ER-2059         TO  EMI-ERROR
03586             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03587      ELSE
03588          MOVE AL-UANON           TO  AACCTA
03589          MOVE AACCTI             TO  PI-ACCT-ACCOUNT.
03590
03591      IF PI-ACCT-CARRIER  = SPACES
03592        AND  (CARR-ACCNT-CNTL
03593        OR   CARR-ST-ACCNT-CNTL
03594        OR   CARR-GROUP-ST-ACCNT-CNTL)
03595          MOVE ER-0193            TO  EMI-ERROR
03596          MOVE -1                 TO  ACARIERL
03597          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03598
03599      IF PI-ACCT-GROUPING  =  SPACES
03600        AND  CARR-GROUP-ST-ACCNT-CNTL
03601          MOVE ER-0195            TO  EMI-ERROR
03602          MOVE -1                 TO  AGROUPL
03603          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03604
03605      IF PI-ACCT-STATE  = SPACES
03606        AND  (ST-ACCNT-CNTL
03607        OR   CARR-ST-ACCNT-CNTL
03608        OR   CARR-GROUP-ST-ACCNT-CNTL)
03609          MOVE ER-0144            TO  EMI-ERROR
03610          MOVE -1                 TO  ASTATEL
03611          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03612
03613      IF MAINTI  =  'S'
03614        OR  FIRST-TIME
03615          NEXT SENTENCE
03616      ELSE
03617          MOVE PI-ACCT-CCGSA-KEY  TO  KEY-SAVE.
03618
03619      IF NOT EMI-NO-ERRORS
020816         IF EIBTRNID = EL6311-TRANS-ID OR VP6311-TRANS-ID
03621              GO TO 8100-SEND-INITIAL-MAP
03622            ELSE
03623              GO TO 8200-SEND-DATAONLY.
03624
03625      IF PI-ACCT-CARRIER   = SPACES  OR
03626         PI-ACCT-STATE     = SPACES  OR
03627         PI-ACCT-GROUPING  = SPACES
03628          MOVE ACCT2-FILE-ID      TO  PI-ACCT-ID
03629      ELSE
03630          MOVE ACCT-FILE-ID       TO  PI-ACCT-ID.
03631
03632      MOVE LOW-VALUES             TO  PI-ACCT-EXP-DT
03633                                      PI-ACCT-REST-OF-EXP.
03634
03635  6199-EXIT.
03636      EXIT.
03637
03638  6200-CHECK-KEY.
03639      IF PI-PREV-ACCOUNT  NOT = PI-ACCT-CCGSA-KEY
03640          MOVE ER-0453            TO  EMI-ERROR
03641          MOVE -1                 TO  MAINTL
03642          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03643
03644  6200-EXIT.
03645      EXIT.
03646  EJECT
03647  6300-STARTBR.
03648      MOVE ACCT-FILE-ID           TO  PI-ACCT-ID.
03649
03650      
      * EXEC CICS  STARTBR
03651 *        DATASET  (PI-ACCT-ID)
03652 *        RIDFLD   (PI-ACCT-KEY)
03653 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008197' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03654
03655      MOVE 'Y'                    TO  BROWSE-STARTED-SW.
03656      MOVE PI-ACCT-CCGSA-KEY      TO  KEY-SAVE.
03657
03658  6300-EXIT.
03659      EXIT.
03660
03661  6310-READNEXT.
03662      
      * EXEC CICS  READNEXT
03663 *        DATASET  (PI-ACCT-ID)
03664 *        SET      (ADDRESS OF ACCOUNT-MASTER)
03665 *        RIDFLD   (PI-ACCT-KEY)
03666 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008209' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03667
03668  6310-EXIT.
03669      EXIT.
03670
03671  6320-ENDBR.
03672      MOVE SPACE                  TO  BROWSE-STARTED-SW.
03673
03674      
      * EXEC CICS  ENDBR
03675 *        DATASET  (PI-ACCT-ID)
03676 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008221' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03677
03678  6320-EXIT.
03679      EXIT.
03680  EJECT
03681  6400-BUILD-VG-KEY.
03682      MOVE AM-CONTROL-PRIMARY     TO  AM-CONTROL-BY-VAR-GRP.
03683
03684      IF ST-ACCNT-CNTL
03685          MOVE SPACES             TO  AM-VG-CARRIER
03686                                      AM-VG-GROUPING.
03687
03688      IF CARR-ST-ACCNT-CNTL
03689          MOVE SPACES             TO  AM-VG-GROUPING.
03690
03691      IF ACCNT-CNTL
03692          MOVE SPACES             TO  AM-VG-CARRIER
03693                                      AM-VG-STATE
03694                                      AM-VG-GROUPING.
03695
03696      IF CARR-ACCNT-CNTL
03697          MOVE SPACES             TO  AM-VG-STATE
03698                                      AM-VG-GROUPING.
03699
03700  6499-EXIT.
03701      EXIT.
03702  EJECT
03703  6500-READ-ACCT.
03704      
      * EXEC CICS  HANDLE CONDITION
03705 *        NOTFND  (6599-EXIT)
03706 *    END-EXEC.
      *    MOVE '"$I                   ! 7 #00008251' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303038323531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03707
03708      MOVE ZERO                   TO  REC-SW.
03709
03710      
      * EXEC CICS  READ
03711 *        GTEQ
03712 *        DATASET  (ACCT-FILE-ID)
03713 *        SET      (ADDRESS OF ACCOUNT-MASTER)
03714 *        RIDFLD   (PI-ACCT-KEY)
03715 *    END-EXEC.
      *    MOVE '&"S        G          (   #00008257' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323537' TO DFHEIV0(25:11)
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
           
03716
03717      MOVE AM-CONTROL-PRIMARY     TO  PI-ACCT-KEY.
03718      MOVE LOW-VALUES             TO  PI-ACCT-REST-OF-EXP.
03719
03720      IF PI-ACCT-CCGSA-KEY  = NEW-KEY-SAVE
03721          MOVE 1                  TO  REC-SW.
03722
03723  6599-EXIT.
03724      EXIT.
03725  EJECT
03726  6600-UPDATE-MAINT-DT.
03727      MOVE SPACES                 TO  ELCNTL-KEY.
03728      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
03729      MOVE '1'                    TO  CNTL-REC-TYPE.
03730      MOVE +0                     TO  CNTL-SEQ-NO.
03731
03732      
      * EXEC CICS  HANDLE CONDITION
03733 *        NOTFND  (6699-EXIT)
03734 *    END-EXEC.
      *    MOVE '"$I                   ! 8 #00008279' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303038323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03735
03736      
      * EXEC CICS  READ
03737 *        UPDATE
03738 *        DATASET  (CNTL-FILE-ID)
03739 *        SET      (ADDRESS OF CONTROL-FILE)
03740 *        RIDFLD   (ELCNTL-KEY)
03741 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008283' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323833' TO DFHEIV0(25:11)
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
           
03742
03743      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
03744      MOVE 'B'                    TO  JP-RECORD-TYPE.
03745      MOVE CNTL-FILE-ID           TO  FILE-ID.
03746
03747      PERFORM 8400-LOG-JOURNAL-RECORD.
03748
03749      MOVE BIN-CURRENT-SAVE       TO  CF-ACCOUNT-MSTR-MAINT-DT.
03750      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
03751      MOVE 'C'                    TO  JP-RECORD-TYPE.
03752      MOVE CNTL-FILE-ID           TO  FILE-ID.
03753
03754      
      * EXEC CICS  REWRITE
03755 *        DATASET  (CNTL-FILE-ID)
03756 *        FROM     (CONTROL-FILE)
03757 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008301' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03758
03759      PERFORM 8400-LOG-JOURNAL-RECORD.
03760
03761  6699-EXIT.
03762      EXIT.
03763  EJECT
03764  6700-VERIFY-STATE-CARRIER.
03765      IF PI-ACCT-CARRIER  = SPACES
03766          GO TO 6750-CHECK-STATE.
03767
03768      MOVE SPACES                 TO  ELCNTL-KEY.
03769      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
03770      MOVE '6'                    TO  CNTL-REC-TYPE.
03771      MOVE PI-ACCT-CARRIER        TO  CNTL-CARRIER
03772      MOVE +0                     TO  CNTL-SEQ-NO.
03773
03774      
      * EXEC CICS  HANDLE CONDITION
03775 *        NOTFND  (6720-NO-CARRIER)
03776 *    END-EXEC.
      *    MOVE '"$I                   ! 9 #00008321' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303038333231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03777
03778      
      * EXEC CICS  READ
03779 *        DATASET  (CNTL-FILE-ID)
03780 *        SET      (ADDRESS OF CONTROL-FILE)
03781 *        RIDFLD   (ELCNTL-KEY)
03782 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008325' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333235' TO DFHEIV0(25:11)
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
           
03783
03784      GO TO 6750-CHECK-STATE.
03785
03786  6720-NO-CARRIER.
03787      MOVE ER-0193                TO  EMI-ERROR.
03788      MOVE -1                     TO  ACARIERL.
03789      MOVE AL-UABON               TO  ACARIERA.
03790
03791      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03792
03793  6750-CHECK-STATE.
03794      IF PI-ACCT-STATE  =  SPACES
03795          GO TO 6799-EXIT.
03796
03797      MOVE SPACES                 TO  ELCNTL-KEY.
03798      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
03799      MOVE '3'                    TO  CNTL-REC-TYPE.
03800      MOVE PI-ACCT-STATE          TO  CNTL-STATE.
03801      MOVE +0                     TO  CNTL-SEQ-NO.
03802
03803      
      * EXEC CICS  HANDLE CONDITION
03804 *        NOTFND  (6780-NO-STATE)
03805 *    END-EXEC.
      *    MOVE '"$I                   ! : #00008350' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303038333530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03806
03807      
      * EXEC CICS  READ
03808 *        DATASET  (CNTL-FILE-ID)
03809 *        SET      (ADDRESS OF CONTROL-FILE)
03810 *        RIDFLD   (ELCNTL-KEY)
03811 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008354' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333534' TO DFHEIV0(25:11)
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
           
03812
03813      GO TO 6799-EXIT.
03814
03815  6780-NO-STATE.
03816      MOVE ER-0144                TO  EMI-ERROR.
03817      MOVE -1                     TO  ASTATEL.
03818      MOVE AL-UABON               TO  ASTATEA.
03819
03820      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03821
03822  6799-EXIT.
03823      EXIT.
03824  EJECT
03825  6800-COMPANY-REC-READ.
03826      MOVE SPACES                 TO  ELCNTL-KEY.
03827      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
03828      MOVE '1'                    TO  CNTL-REC-TYPE.
03829      MOVE +0                     TO  CNTL-SEQ-NO.
03830
03831      
      * EXEC CICS  HANDLE CONDITION
03832 *        NOTFND  (6880-NO-COMP)
03833 *    END-EXEC.
      *    MOVE '"$I                   ! ; #00008378' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303038333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03834
03835      
      * EXEC CICS  READ
03836 *        DATASET  (CNTL-FILE-ID)
03837 *        SET      (ADDRESS OF CONTROL-FILE)
03838 *        RIDFLD   (ELCNTL-KEY)
03839 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008382' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333832' TO DFHEIV0(25:11)
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
           
03840
03841      IF CF-ACCOUNT-MSTR-MAINT-DT  = LOW-VALUES
03842          MOVE ER-2572            TO  EMI-ERROR
03843          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03844
03845      GO TO 6899-EXIT.
03846
03847  6880-NO-COMP.
03848      MOVE ER-0022                TO  EMI-ERROR.
03849
03850      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03851
03852  6899-EXIT.
03853      EXIT.
03854  EJECT
03855  7000-BUILD-OUTPUT-MAP.
03856      IF RETURNING-FROM-6501
03857          MOVE PI-PREV-VG-ACCOUNT  TO  KEY-SAVE.
03858
03859      IF PI-COMPANY-ID  = 'LGX'  OR  'MIC'  OR  'MCC'
03860          MOVE AL-SANOF           TO  TRNMSG1A
03861                                      TRNMSG2A
03862                                      TRNMSG3A
03863                                      TRNMSG4A.
03864
03865      MOVE AL-UANON               TO  ACARIERA
03866                                      AGROUPA
03867                                      ASTATEA
03868                                      AACCTA.
03869      MOVE SV-CARRIER             TO  ACARIERO.
03870      MOVE SV-GROUPING            TO  AGROUPO.
03871      MOVE SV-STATE               TO  ASTATEO.
03872      MOVE SV-ACCOUNT             TO  AACCTO.
03873
03874      SET M-INDEX  TO  1.
03875
03876      MOVE LOW-VALUES             TO  MAP-RECORD-AREA.
03877
03878      IF PI-MAINT  = 'A'
03879        AND  PI-RECORD-NOT-CREATED
03880          MOVE ER-2151            TO  EMI-ERROR
03881          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03882          GO TO 7000-END.
03883
03884      IF PI-FST-PAGE
03885          SET T-INDEX   TO  1
03886      ELSE
03887      IF PI-2ND-PAGE
03888          SET T-INDEX   TO  9
03889      ELSE
03890      IF PI-3RD-PAGE
03891          SET T-INDEX   TO  17
03892      ELSE
03893          SET T-INDEX   TO  25.
03894
03895  7000-LOOP.
03896      IF PI-BIN-EXP-DT (T-INDEX)  NOT = HIGH-VALUES
03897          MOVE PI-BIN-EXP-DT (T-INDEX)
03898                                  TO  DC-BIN-DATE-1
03899          MOVE SPACE              TO  DC-OPTION-CODE
03900          PERFORM 9700-DATE-CONVERSION
03901          IF DATE-CONVERSION-ERROR
03902              MOVE SPACES         TO  MAP-EXP-DT (M-INDEX)
03903          ELSE
03904              MOVE DC-GREG-DATE-1-EDIT
03905                                  TO  MAP-EXP-DT (M-INDEX)
03906      ELSE
03907          MOVE 999999             TO  MAP-EXP-DT-ED (M-INDEX)
03908          INSPECT MAP-EXP-DT-ED (M-INDEX) CONVERTING SPACES TO '/'.
03909
03910      MOVE PI-BIN-EFF-DT (T-INDEX)  TO  DC-BIN-DATE-1.
03911      MOVE SPACE                    TO  DC-OPTION-CODE.
03912
03913      PERFORM 9700-DATE-CONVERSION.
03914
03915      IF DATE-CONVERSION-ERROR
03916          MOVE SPACES               TO  MAP-EFF-DT (M-INDEX)
03917      ELSE
03918          MOVE DC-GREG-DATE-1-EDIT  TO  MAP-EFF-DT (M-INDEX).
03919
03920      IF PI-BIN-MAINT-DT (T-INDEX)  = LOW-VALUES
03921          MOVE SPACES             TO  MAP-LST-MAINT-DT (M-INDEX)
03922      ELSE
03923          MOVE PI-BIN-MAINT-DT (T-INDEX)
03924                                  TO  DC-BIN-DATE-1
03925          MOVE SPACE              TO  DC-OPTION-CODE
03926          PERFORM 9700-DATE-CONVERSION
03927          IF DATE-CONVERSION-ERROR
03928              MOVE SPACES         TO  MAP-LST-MAINT-DT (M-INDEX)
03929          ELSE
03930              MOVE DC-GREG-DATE-1-EDIT
03931                                  TO  MAP-LST-MAINT-DT (M-INDEX).
03932
03933      IF PI-BIN-HI-CERT (T-INDEX)  =  LOW-VALUES
03934          MOVE SPACES             TO  MAP-HIGH-CERT (M-INDEX)
03935      ELSE
03936          MOVE PI-BIN-HI-CERT (T-INDEX)
03937                                  TO  DC-BIN-DATE-1
03938          MOVE SPACE              TO  DC-OPTION-CODE
03939          PERFORM 9700-DATE-CONVERSION
03940          IF DATE-CONVERSION-ERROR
03941              MOVE SPACES         TO  MAP-HIGH-CERT (M-INDEX)
03942          ELSE
03943              MOVE DC-GREG-DATE-1-EDIT
03944                                  TO  MAP-HIGH-CERT (M-INDEX).
03945
03946      IF PI-BIN-LO-CERT (T-INDEX)  = LOW-VALUES
03947          MOVE SPACES             TO  MAP-LOW-CERT (M-INDEX)
03948      ELSE
03949          MOVE PI-BIN-LO-CERT (T-INDEX)
03950                                  TO  DC-BIN-DATE-1
03951          MOVE SPACE              TO  DC-OPTION-CODE
03952          PERFORM 9700-DATE-CONVERSION
03953          IF DATE-CONVERSION-ERROR
03954              MOVE SPACES         TO  MAP-LOW-CERT (M-INDEX)
03955          ELSE
03956              MOVE DC-GREG-DATE-1-EDIT
03957                                  TO  MAP-LOW-CERT (M-INDEX).
03958
03959      IF PI-BIN-AR-HI-CERT (T-INDEX)  = LOW-VALUES OR SPACES
03960          MOVE SPACES             TO  MAP-AR-DATE (M-INDEX)
03961      ELSE
03962          MOVE PI-BIN-AR-HI-CERT (T-INDEX)
03963                                  TO  DC-BIN-DATE-1
03964          MOVE SPACE              TO  DC-OPTION-CODE
03965          PERFORM 9700-DATE-CONVERSION
03966          IF DATE-CONVERSION-ERROR
03967              MOVE SPACES         TO  MAP-AR-DATE (M-INDEX)
03968          ELSE
03969              MOVE DC-GREG-DATE-1-EDIT
03970                                  TO  MAP-AR-DATE (M-INDEX).
03971
03972      IF M-INDEX  =  8  OR
03973         T-INDEX  =  PI-TOTAL-LINES
03974          NEXT SENTENCE
03975      ELSE
03976          SET M-INDEX
03977              T-INDEX  UP  BY  1
03978          GO TO 7000-LOOP.
03979
03980      IF M-INDEX  =  8
03981        AND  T-INDEX  LESS THAN  PI-TOTAL-LINES
03982          MOVE ER-0668            TO  EMI-ERROR
03983          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03984
03985      IF T-INDEX  =  PI-TOTAL-LINES
03986          MOVE ER-0630            TO  EMI-ERROR
03987          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03988
03989  7000-END.
03990      MOVE -1                     TO  MAINTL.
03991      MOVE MAINTI                 TO  PI-MAINT.
03992
03993      IF MAINTI  = 'S'  OR  'D'
03994        OR  RETURNING-FROM-6501
03995          MOVE LOW-VALUES         TO  MAINTI
03996          MOVE PI-ACCNAME         TO  NAMEO
03997          MOVE PI-NAMEFLG         TO  NAMEFLGO
03998          GO TO 8100-SEND-INITIAL-MAP.
03999
04000      IF EIBAID  =  DFHPF1  OR  DFHPF2  OR
04001                    DFHPF3  OR  DFHPF4
04002          MOVE 'S'                TO  PI-MAINT
04003          MOVE PI-ACCNAME         TO  NAMEO
04004          MOVE PI-NAMEFLG         TO  NAMEFLGO
04005          GO TO 8100-SEND-INITIAL-MAP.
04006
04007      IF MAINTI  = 'N' OR 'E' OR 'X'
04008         MOVE SPACES                 TO  BIN-EXPCHG-SAVE
04009                                         BIN-EFFCHG-SAVE
04010         MOVE SPACES                 TO  EFFCHGI
04011                                         EXPCHGI
04012         MOVE AL-UANOF               TO  EFFCHGA
04013                                         EXPCHGA
04014         MOVE SPACES                 TO  PI-MAINT
04015         MOVE SPACES                 TO  MAINTI
04016                                         MAINTO
04017         MOVE AL-UANOF               TO  MAINTA.
04018
04019      IF K-WITH-LINE-NO
04020          MOVE ZERO         TO K-WITH-LINE-USED
04021          GO TO 8100-SEND-INITIAL-MAP.
04022
04023      GO TO 8200-SEND-DATAONLY.
04024  EJECT
04025  7100-START-BROWSE.
04026      
      * EXEC CICS  STARTBR
04027 *        DATASET  (PI-ACCT-ID)
04028 *        RIDFLD   (PI-ACCT-KEY)
04029 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008573' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-ACCT-ID, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04030
04031  7100-EXIT.
04032      EXIT.
04033  EJECT
04034  7200-WRITE-TEMP-STORAGE.
04035      PERFORM 7300-DELETE-TEMP-STORAGE  THRU  7300-EXIT.
04036
04037      
      * EXEC CICS  HANDLE CONDITION
04038 *        ITEMERR  (7200-EXIT)
04039 *        QIDERR   (7200-EXIT)
04040 *    END-EXEC.
      *    MOVE '"$<N                  ! < #00008584' TO DFHEIV0
           MOVE X'22243C4E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303038353834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04041
04042      MOVE +0                     TO  TS-ITEM.
04043
04044      
      * EXEC CICS  WRITEQ TS
04045 *        QUEUE   (TS-QUEUE)
04046 *        FROM    (TEMP-STORAGE-RECORD)
04047 *        LENGTH  (TS-LENGTH)
04048 *        ITEM    (TS-ITEM)
04049 *    END-EXEC.
      *    MOVE '*" I                  ''   #00008591' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE, 
                 TEMP-STORAGE-RECORD, 
                 TS-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04050
04051  7200-EXIT.
04052      EXIT.
04053
04054  7300-DELETE-TEMP-STORAGE.
04055      
      * EXEC CICS  HANDLE CONDITION
04056 *        ITEMERR  (7300-EXIT)
04057 *        QIDERR   (7300-EXIT)
04058 *    END-EXEC.
      *    MOVE '"$<N                  ! = #00008602' TO DFHEIV0
           MOVE X'22243C4E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303038363032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04059
04060      
      * EXEC CICS  DELETEQ TS
04061 *        QUEUE  (TS-QUEUE)
04062 *    END-EXEC.
      *    MOVE '*&                    #   #00008607' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04063
04064  7300-EXIT.
04065      EXIT.
04066
04067  7400-READ-TEMP-STORAGE.
04068      
      * EXEC CICS  HANDLE CONDITION
04069 *        ITEMERR  (7400-EXIT)
04070 *        QIDERR   (7400-EXIT)
04071 *    END-EXEC.
      *    MOVE '"$<N                  ! > #00008615' TO DFHEIV0
           MOVE X'22243C4E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303038363135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04072
04073      MOVE +1                     TO  TS-ITEM.
04074
04075      
      * EXEC CICS  READQ TS
04076 *        QUEUE   (TS-QUEUE)
04077 *        INTO    (TEMP-STORAGE-RECORD)
04078 *        LENGTH  (TS-LENGTH)
04079 *        ITEM    (TS-ITEM)
04080 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00008622' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE, 
                 TEMP-STORAGE-RECORD, 
                 TS-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04081
04082  7400-EXIT.
04083      EXIT.
04084
04085  8100-SEND-INITIAL-MAP.
04086      MOVE SAVE-DATE              TO  DATEO.
04087      MOVE EIBTIME                TO  TIME-IN.
04088      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
04089      MOVE -1                     TO  MAINTL.
04090      MOVE AL-UABON               TO  MAINTA.
04091      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
04092      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
04093
04094      IF PI-AR-PROCESSING
04095         MOVE AL-PANOF            TO  ARHDGA.
04096
04097      
      * EXEC CICS  SEND
04098 *        MAP     (MAP-NAME)
04099 *        MAPSET  (MAPSET-NAME)
04100 *        FROM    (EL650AO)
04101 *        ERASE
04102 *        CURSOR
04103 *    END-EXEC.
           MOVE LENGTH OF
            EL650AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00008646' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL650AO, 
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
           
04104
04105      GO TO 9100-RETURN-TRAN.
04106
04107  8200-SEND-DATAONLY.
04108      MOVE SAVE-DATE              TO  DATEO.
04109      MOVE EIBTIME                TO  TIME-IN.
04110      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
04111      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
04112      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
04113
04114      
      * EXEC CICS  SEND
04115 *        MAP     (MAP-NAME)
04116 *        MAPSET  (MAPSET-NAME)
04117 *        FROM    (EL650AO)
04118 *        DATAONLY
04119 *        CURSOR
04120 *    END-EXEC.
           MOVE LENGTH OF
            EL650AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00008665' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL650AO, 
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
           
04121
04122      GO TO 9100-RETURN-TRAN.
04123
04124  8300-SEND-TEXT.
04125      
      * EXEC CICS  SEND TEXT
04126 *        FROM    (LOGOFF-TEXT)
04127 *        LENGTH  (LOGOFF-LENGTH)
04128 *        ERASE
04129 *        FREEKB
04130 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00008676' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363736' TO DFHEIV0(25:11)
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
           
04131
04132      
      * EXEC CICS  RETURN
04133 *    END-EXEC.
      *    MOVE '.(                    ''   #00008683' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04134
04135  8400-LOG-JOURNAL-RECORD.
CIDMOD*    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
CIDMOD*    MOVE FILE-ID                TO  JP-FILE-ID.
CIDMOD*    MOVE THIS-PGM               TO  JP-PROGRAM-ID.
CIDMOD*
CIDMOD*    IF PI-JOURNAL-FILE-ID  NOT =  ZERO
CIDMOD*        EXEC CICS  JOURNAL
CIDMOD*            JFILEID  (PI-JOURNAL-FILE-ID)
CIDMOD*            JTYPEID  ('ER')
CIDMOD*            FROM     (JOURNAL-RECORD)
CIDMOD*            LENGTH   (2023)
CIDMOD*        END-EXEC.
04147
04148  8600-DEEDIT.
04149      
      * EXEC CICS  BIF DEEDIT
04150 *        FIELD   (WS-DEEDIT-FIELD)
04151 *        LENGTH  (10)
04152 *    END-EXEC.
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008700' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04153  8600-EXIT.
04154       EXIT.
04155  8800-UNAUTHORIZED-ACCESS.
04156      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
04157
04158      GO TO 8300-SEND-TEXT.
04159
04160  8810-PF23.
04161      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
04162      MOVE XCTL-005               TO  PGM-NAME.
04163
04164      GO TO 9300-XCTL.
04165
04166  8900-PF5.
04167      IF PI-PREV-VG-ACCOUNT EQUAL LOW-VALUES OR SPACES
04168          MOVE ER-3785            TO  EMI-ERROR
04169          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
04170          MOVE -1                 TO  MAINTL
04171          GO TO 8200-SEND-DATAONLY.
04172
04173      MOVE PI-PREV-VG-ACCOUNT     TO  PI-ACCT-CCGSA-KEY.
04174
04175      MOVE XCTL-6509              TO  PGM-NAME.
04176
04177      GO TO 9300-XCTL.
04178
110706 8910-PF6.
110706     IF PI-PREV-VG-ACCOUNT EQUAL LOW-VALUES OR SPACES
110706        MOVE ER-3785             TO EMI-ERROR
110706        PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
110706        MOVE -1                  TO MAINTL
110706        GO TO 8200-SEND-DATAONLY
110706     END-IF
110706     MOVE PI-PREV-VG-ACCOUNT     TO PI-ACCT-CCGSA-KEY
110706     MOVE XCTL-6503              TO PGM-NAME
110706     GO TO 9300-XCTL
           .
04179  9100-RETURN-TRAN.
04180      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
04181      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
04182      
      * EXEC CICS  RETURN
04183 *        TRANSID   (TRANS-ID)
04184 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
04185 *        LENGTH    (WS-COMM-LENGTH)
04186 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00008745' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04187
04188  9200-RETURN-MAIN-MENU.
04189      MOVE XCTL-126               TO  PGM-NAME.
04190
04191      GO TO 9300-XCTL.
04192
04193  9300-XCTL.
04194      
      * EXEC CICS  XCTL
04195 *        PROGRAM   (PGM-NAME)
04196 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
04197 *        LENGTH    (WS-COMM-LENGTH)
04198 *    END-EXEC.
      *    MOVE '.$C                   %   #00008757' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04199
04200  9400-CLEAR.
04201      MOVE SPACES                 TO  PI-SV-MAINT.
04202      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
04203
04204      GO TO 9300-XCTL.
04205
04206  9500-PF12.
04207      MOVE XCTL-010               TO  PGM-NAME.
04208
04209      GO TO 9300-XCTL.
04210
04211  9600-PGMID-ERROR.
04212      
      * EXEC CICS  HANDLE CONDITION
04213 *        PGMIDERR  (8300-SEND-TEXT)
04214 *    END-EXEC.
      *    MOVE '"$L                   ! ? #00008775' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3F20233030303038373735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04215
04216      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
04217      MOVE ' '                    TO  PI-ENTRY-CD-1.
04218      MOVE XCTL-005               TO  PGM-NAME.
04219      MOVE PGM-NAME               TO  LOGOFF-PGM.
04220      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
04221
04222      GO TO 9300-XCTL.
04223
04224  9700-DATE-CONVERSION.
04225      MOVE LINK-ELDATCV           TO  PGM-NAME.
04226
04227      
      * EXEC CICS  LINK
04228 *        PROGRAM   (PGM-NAME)
04229 *        COMMAREA  (DATE-CONVERSION-DATA)
04230 *        LENGTH    (DC-COMM-LENGTH)
04231 *    END-EXEC.
      *    MOVE '."C                   (   #00008790' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04232
04233  9900-ERROR-FORMAT.
04234      IF NOT EMI-ERRORS-COMPLETE
04235          MOVE LINK-001           TO  PGM-NAME
04236          
      * EXEC CICS  LINK
04237 *            PROGRAM   (PGM-NAME)
04238 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
04239 *            LENGTH    (EMI-COMM-LENGTH)
04240 *        END-EXEC.
      *    MOVE '."C                   (   #00008799' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04241
04242  9900-EXIT.
04243      EXIT.
04244
04245  9990-ABEND.
04246      MOVE LINK-004               TO  PGM-NAME.
04247      MOVE DFHEIBLK               TO  EMI-LINE1.
04248
04249      
      * EXEC CICS  LINK
04250 *        PROGRAM   (PGM-NAME)
04251 *        COMMAREA  (EMI-LINE1)
04252 *        LENGTH    (72)
04253 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00008812' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04254
04255      MOVE -1                     TO  MAINTL.
04256
04257      GO TO 8200-SEND-DATAONLY.
04258
04259  9995-SECURITY-VIOLATION.
04260 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00008840' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383430' TO DFHEIV0(25:11)
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
04261
04262  9995-EXIT.
04263      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL650' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 5340-END,
                     5330-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1010-NOT-FOUND,
                     1010-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2568-NO-RECORDS,
                     2568-NO-RECORDS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3175-CONTINUE,
                     3175-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 4175-CONTINUE,
                     4175-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4530-NOT-FOUND,
                     4530-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4699-EXIT,
                     4689-EOF
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4899-EXIT,
                     4889-EOF
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4580-DUPREC,
                     4580-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 4959-EXIT,
                     4959-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 4969-EXIT,
                     4969-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 4980-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 5007-EXIT,
                     5007-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 5007-EXIT,
                     5007-EXIT,
                     5007-EXIT,
                     5007-EXIT,
                     5007-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 5015-NOT-FOUND,
                     5015-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 5015-NOT-FOUND,
                     5015-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 5340-END,
                     5330-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 5440-NOT-FOUND,
                     5430-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 5525-SKIP-REWRITE,
                     5525-SKIP-REWRITE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 5615-SKIP-REWRITE,
                     5615-SKIP-REWRITE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 6599-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 6699-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 6720-NO-CARRIER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 6780-NO-STATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 6880-NO-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 7200-EXIT,
                     7200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 7300-EXIT,
                     7300-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 7400-EXIT,
                     7400-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL650' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
