00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL162 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 05/02/95 14:07:28.
00007 *                            VMOD=2.015
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00024 *REMARKS.    TRANSACTION - EX17 - RECORD MAIL RECEIVED
00025 *            USED TO REVIEW PENDING MAIL AND TO RECORD
00026 *            INCOMING MAIL.
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
121406* 121406    2006081100001  PEMA  ADD RECEIVE DATE EDIT
051107* 051107    2006052500001  AJRA  POPULATE UBY WITH USER ID
100610* 100610    2009122800001  AJRA  BYPASS LETTER W/ RESEND PRINTED D
102610* 102610    2009122800001  AJRA    REPLACE VERIFY W/ STOP DATE
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
010416* 010416    2015072900002  TANA  ADD PF7 TO CLAIM MEMO SCREEN
062217* 062217    2017050300002  TANA  ADD AUTH RCVD FIELD
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022619* 022619  CR2019021100002  PEMA  ADD EDIT TO RECEIVE DATE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00027
00028      EJECT
00029  ENVIRONMENT DIVISION.
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL162 WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.015 *********'.
00035
00036 *                            COPY ELCSCTM.
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
00037
00038 *                            COPY ELCSCRTY.
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
00039
00040  01  WS-DATE-AREA.
00041      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00042      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00043      05  CURRENT-MINUS1-SAVE PIC XX      VALUE SPACES.
022619 01  ws-response                 PIC S9(8)   COMP.
022619     88  resp-normal                  VALUE +00.
022619     88  resp-notfnd                  VALUE +13.
022619     88  resp-duprec                  value +14.
022619     88  resp-dupkey                  value +15.
022619     88  resp-notopen                 VALUE +19.
022619     88  resp-endfile                 VALUE +20.
00045  01  STANDARD-AREAS.
00046      12  SC-ITEM                 PIC S9(4)   VALUE +1  COMP.
00047      12  MAP-NAME.
00048          16  MAP-PRE             PIC XX      VALUE 'EL'.
00049          16  MAP-NUMBER          PIC X(4)    VALUE '162A'.
00050          16  FILLER              PIC XX      VALUE SPACES.
00051      12  MAPSET-NAME             PIC X(8)    VALUE 'EL162S'.
00052      12  GETMAIN-SPACE           PIC X       VALUE SPACE.
00053      12  TRANS-ID                PIC X(4)    VALUE 'EX17'.
00054      12  PGM-NAME                PIC X(8).
00055      12  TIME-IN                 PIC S9(7).
00056      12  TIME-OUT-R  REDEFINES TIME-IN.
00057          16  FILLER              PIC X.
00058          16  TIME-OUT            PIC 99V99.
00059          16  FILLER              PIC XX.
00060      12  XCTL-005                PIC X(5)    VALUE 'EL005'.
00061      12  XCTL-010                PIC X(5)    VALUE 'EL010'.
00062      12  XCTL-126                PIC X(5)    VALUE 'EL126'.
00063      12  LINK-001                PIC X(5)    VALUE 'EL001'.
00064      12  LINK-004                PIC X(5)    VALUE 'EL004'.
00065      12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.
00066      12  THIS-PGM                PIC X(8)    VALUE 'EL162'.
00067      12  PGM-EL141               PIC X(8)    VALUE 'EL141'.
00068      12  PGM-EL132               PIC X(8)    VALUE 'EL132'.
00069      12  PGM-EL150               PIC X(8)    VALUE 'EL150'.
010416     12  PGM-EL1284              PIC X(8)    VALUE 'EL1284'.
00070      12  ELMSTR-FILE-ID          PIC X(8)    VALUE 'ELMSTR'.
00071      12  ELTRLR-FILE-ID          PIC X(8)    VALUE 'ELTRLR'.
00072      12  ERACCT-FILE-ID          PIC X(8)    VALUE 'ERACCT'.
00073      12  SUB                     PIC S999    COMP-3.
00074      EJECT
00075  01  ERROR-MESSAGES.
00076      12  ER-ZEROS                PIC X(4)    VALUE '0000'.
00077      12  ER-0004                 PIC X(4)    VALUE '0004'.
00078      12  ER-0008                 PIC X(4)    VALUE '0008'.
00079      12  ER-0029                 PIC X(4)    VALUE '0029'.
00080      12  ER-0050                 PIC X(4)    VALUE '0050'.
00081      12  ER-0021                 PIC X(4)    VALUE '0021'.
00082      12  ER-0042                 PIC X(4)    VALUE '0042'.
00083      12  ER-0066                 PIC X(4)    VALUE '0066'.
00084      12  ER-0067                 PIC X(4)    VALUE '0067'.
00085      12  ER-0070                 PIC X(4)    VALUE '0070'.
00086      12  ER-0133                 PIC X(4)    VALUE '0133'.
00087      12  ER-0168                 PIC X(4)    VALUE '0168'.
00088      12  ER-0172                 PIC X(4)    VALUE '0172'.
00089      12  ER-0179                 PIC X(4)    VALUE '0179'.
062217     12  ER-0287                 PIC X(4)    VALUE '0287'.
00090      12  ER-0318                 PIC X(4)    VALUE '0318'.
00091      12  ER-0319                 PIC X(4)    VALUE '0319'.
00092      12  ER-0320                 PIC X(4)    VALUE '0320'.
00093      12  ER-0321                 PIC X(4)    VALUE '0321'.
00094      12  ER-0322                 PIC X(4)    VALUE '0322'.
00095      12  ER-0514                 PIC X(4)    VALUE '0514'.
00096      12  ER-0539                 PIC X(4)    VALUE '0539'.
102610     12  ER-0895                 PIC X(4)    VALUE '0895'.
102610     12  ER-0896                 PIC X(4)    VALUE '0896'.
022619     12  er-1825                 pic x(4)    value '1825'.
00097      12  ER-7839                 PIC X(4)    VALUE '7839'.
00098
00099      12  BIN-CURRENT-DATE        PIC XX.
00100      12  INDX-WORK               PIC 9(4).
00101      12  REC-DATA-SW             PIC X       VALUE ' '.
00102          88  REC-DATA                        VALUE '1'.
00103      12  VER-DATA-SW             PIC X       VALUE ' '.
00104          88  VER-DATA                        VALUE '1'.
00105      12  UNSOL-DATA-SW           PIC X       VALUE ' '.
00106          88  UNSOL-DATA                      VALUE '1'.
00107          88  FILE-DATA                       VALUE '2'.
00108      12  ADDR-TYPE               PIC X.
00109      12  ADDR-SEQ                PIC S9(4) COMP.
00110      12  ACTV-LENGTH             PIC S9(4) COMP VALUE +200.
00111      12  USENT-SAVE              PIC XX    VALUE LOW-VALUES.
00112      12  URECDTE-SAVE            PIC XX    VALUE LOW-VALUES.
00113      12  REC-DATE-SAVE OCCURS 4 TIMES INDEXED BY INDX     PIC XX.
102610     12  STOP-DATE-SAVE OCCURS 4 TIMES INDEXED BY INDX2   PIC XX.
00114      12  DEEDIT-FIELD            PIC X(15).
00115      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15).
00116
00117      12  ACCT-KEY.
00118          16  ACCT-PARTIAL-KEY.
00119              20  ACCT-CO         PIC X.
00120              20  ACCT-CARRIER    PIC X.
00121              20  ACCT-GROUPING   PIC X(6).
00122              20  ACCT-STATE      PIC XX.
00123              20  ACCT-ACCOUNT    PIC X(10).
00124          16  ACCT-EXP-DATE       PIC XX.
00125      12  FILLER                  PIC S9(9)  VALUE +0 COMP.
00126      12  ACCT-SAVE-KEY           PIC X(20).
00127      12  WS-ZIP.
00128          16  WS-ZIP-CODE         PIC X(5).
00129          16  WS-DASH             PIC X     VALUE '-'.
00130          16  WS-ZIP-PLUS4        PIC X(4).
00131      12  WS-CANADIAN-POSTAL-CODES REDEFINES WS-ZIP.
00132          16  WS-CAN-POSTAL-CD-1  PIC X(3).
00133          16  WS-DASH-CAN         PIC X.
00134          16  WS-CAN-POSTAL-CD-2  PIC X(3).
00135          16  WS-CAN-FILLER       PIC X(3).
00136      12  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.
00137      12  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE
00138                                  PIC 9(10).
00139      12  WS-REASON               PIC X(70).
00140
00141      EJECT
00142 *                            COPY ELCDATE.
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
00143
00144      EJECT
00145 *                            COPY ELCLOGOF.
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
00146
00147      EJECT
00148 *                            COPY ELCATTR.
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
00149
00150      EJECT
00151 *                            COPY ELCEMIB.
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
00152      EJECT
00153 *                            COPY ELCINTF.
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
00154      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00155          16  PI-NEXT-TRLR-SUB    PIC S999  COMP-3.
00156          16  PI-ACTV-SAVE        PIC X(20).
00157          16  PI-ACTV-SAVE-SEQ    PIC S9(04) COMP.
00158          16  PI-ACTV-KEY.
00159            18  PI-ACTV-PARTIAL.
00160              20  PI-ACTV-COMP-CD     PIC X.
00161              20  PI-ACTV-CARRIER     PIC X.
00162              20  PI-ACTV-CLAIM       PIC X(7).
00163              20  PI-ACTV-CERT-NO.
00164                  22  PI-ACTV-CERT-NO-PRIME PIC X(10).
00165                  22  PI-ACTV-CERT-NO-SUFX  PIC X.
00166            18  PI-ACTV-SEQ           PIC S9(4) COMP.
00167
00168          16  PI-DISPLAYED-TRAILER-CODES.
00169              18  PI-TRLRS OCCURS 50 TIMES    PIC S9(4) COMP.
00170          16  PI-CLAM-KEY.
00171              20  PI-CLAM-COMP-CD     PIC X.
00172              20  PI-CLAM-CARRIER     PIC X.
00173              20  PI-CLAM-CLAIM       PIC X(7).
00174              20  PI-CLAM-CERT-NO.
00175                  24  PI-CLAM-CERT-NO-PRIME  PIC X(10).
00176                  24  PI-CLAM-CERT-NO-SUFX   PIC X.
00177          16  PI-DMD-FORCE-ERROR      PIC X.
00178              88  PI-DMD-FORCED             VALUE 'X'.
022619         16  pi-receive-dt-cnt       pic 9.
022619         16  pi-prev-rec-dt          pic xx.
022619         16  FILLER                  PIC X(470).
00180
00181      EJECT
00182 *                            COPY ELCAID.
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
00183  01  FILLER    REDEFINES DFHAID.
00184      12  FILLER              PIC X(8).
00185      12  PF-VALUES           PIC X       OCCURS 2.
00186
00187      EJECT
00188 *                            COPY EL162S.
       01  EL162AI.
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
           05  CLAIMNOL PIC S9(0004) COMP.
           05  CLAIMNOF PIC  X(0001).
           05  FILLER REDEFINES CLAIMNOF.
               10  CLAIMNOA PIC  X(0001).
           05  CLAIMNOI PIC  X(0007).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0006).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  CERTNOL PIC S9(0004) COMP.
           05  CERTNOF PIC  X(0001).
           05  FILLER REDEFINES CERTNOF.
               10  CERTNOA PIC  X(0001).
           05  CERTNOI PIC  X(0011).
      *    -------------------------------
           05  LASTNMEL PIC S9(0004) COMP.
           05  LASTNMEF PIC  X(0001).
           05  FILLER REDEFINES LASTNMEF.
               10  LASTNMEA PIC  X(0001).
           05  LASTNMEI PIC  X(0015).
      *    -------------------------------
           05  FILEL PIC S9(0004) COMP.
           05  FILEF PIC  X(0001).
           05  FILLER REDEFINES FILEF.
               10  FILEA PIC  X(0001).
           05  FILEI PIC  X(0004).
      *    -------------------------------
           05  RECDTE1L PIC S9(0004) COMP.
           05  RECDTE1F PIC  X(0001).
           05  FILLER REDEFINES RECDTE1F.
               10  RECDTE1A PIC  X(0001).
           05  RECDTE1I PIC  X(0008).
      *    -------------------------------
           05  STOPDT1L PIC S9(0004) COMP.
           05  STOPDT1F PIC  X(0001).
           05  FILLER REDEFINES STOPDT1F.
               10  STOPDT1A PIC  X(0001).
           05  STOPDT1I PIC  X(0008).
      *    -------------------------------
           05  SENT1L PIC S9(0004) COMP.
           05  SENT1F PIC  X(0001).
           05  FILLER REDEFINES SENT1F.
               10  SENT1A PIC  X(0001).
           05  SENT1I PIC  X(0008).
      *    -------------------------------
           05  BY1L PIC S9(0004) COMP.
           05  BY1F PIC  X(0001).
           05  FILLER REDEFINES BY1F.
               10  BY1A PIC  X(0001).
           05  BY1I PIC  X(0004).
      *    -------------------------------
           05  RESEND1L PIC S9(0004) COMP.
           05  RESEND1F PIC  X(0001).
           05  FILLER REDEFINES RESEND1F.
               10  RESEND1A PIC  X(0001).
           05  RESEND1I PIC  X(0008).
      *    -------------------------------
           05  FOLLOW1L PIC S9(0004) COMP.
           05  FOLLOW1F PIC  X(0001).
           05  FILLER REDEFINES FOLLOW1F.
               10  FOLLOW1A PIC  X(0001).
           05  FOLLOW1I PIC  X(0008).
      *    -------------------------------
           05  FORM1L PIC S9(0004) COMP.
           05  FORM1F PIC  X(0001).
           05  FILLER REDEFINES FORM1F.
               10  FORM1A PIC  X(0001).
           05  FORM1I PIC  X(0004).
      *    -------------------------------
           05  TO1L PIC S9(0004) COMP.
           05  TO1F PIC  X(0001).
           05  FILLER REDEFINES TO1F.
               10  TO1A PIC  X(0001).
           05  TO1I PIC  X(0008).
      *    -------------------------------
           05  ARCH1L PIC S9(0004) COMP.
           05  ARCH1F PIC  X(0001).
           05  FILLER REDEFINES ARCH1F.
               10  ARCH1A PIC  X(0001).
           05  ARCH1I PIC  X(0008).
      *    -------------------------------
           05  TRLR1L PIC S9(0004) COMP.
           05  TRLR1F PIC  X(0001).
           05  FILLER REDEFINES TRLR1F.
               10  TRLR1A PIC  X(0001).
           05  TRLR1I PIC  X(0004).
      *    -------------------------------
           05  TYPE1L PIC S9(0004) COMP.
           05  TYPE1F PIC  X(0001).
           05  FILLER REDEFINES TYPE1F.
               10  TYPE1A PIC  X(0001).
           05  TYPE1I PIC  X(0001).
      *    -------------------------------
           05  RE1L PIC S9(0004) COMP.
           05  RE1F PIC  X(0001).
           05  FILLER REDEFINES RE1F.
               10  RE1A PIC  X(0001).
           05  RE1I PIC  X(0070).
      *    -------------------------------
           05  RECDTE2L PIC S9(0004) COMP.
           05  RECDTE2F PIC  X(0001).
           05  FILLER REDEFINES RECDTE2F.
               10  RECDTE2A PIC  X(0001).
           05  RECDTE2I PIC  X(0008).
      *    -------------------------------
           05  STOPDT2L PIC S9(0004) COMP.
           05  STOPDT2F PIC  X(0001).
           05  FILLER REDEFINES STOPDT2F.
               10  STOPDT2A PIC  X(0001).
           05  STOPDT2I PIC  X(0008).
      *    -------------------------------
           05  SENT2L PIC S9(0004) COMP.
           05  SENT2F PIC  X(0001).
           05  FILLER REDEFINES SENT2F.
               10  SENT2A PIC  X(0001).
           05  SENT2I PIC  X(0008).
      *    -------------------------------
           05  BY2L PIC S9(0004) COMP.
           05  BY2F PIC  X(0001).
           05  FILLER REDEFINES BY2F.
               10  BY2A PIC  X(0001).
           05  BY2I PIC  X(0004).
      *    -------------------------------
           05  RESEND2L PIC S9(0004) COMP.
           05  RESEND2F PIC  X(0001).
           05  FILLER REDEFINES RESEND2F.
               10  RESEND2A PIC  X(0001).
           05  RESEND2I PIC  X(0008).
      *    -------------------------------
           05  FOLLOW2L PIC S9(0004) COMP.
           05  FOLLOW2F PIC  X(0001).
           05  FILLER REDEFINES FOLLOW2F.
               10  FOLLOW2A PIC  X(0001).
           05  FOLLOW2I PIC  X(0008).
      *    -------------------------------
           05  FORM2L PIC S9(0004) COMP.
           05  FORM2F PIC  X(0001).
           05  FILLER REDEFINES FORM2F.
               10  FORM2A PIC  X(0001).
           05  FORM2I PIC  X(0004).
      *    -------------------------------
           05  TO2L PIC S9(0004) COMP.
           05  TO2F PIC  X(0001).
           05  FILLER REDEFINES TO2F.
               10  TO2A PIC  X(0001).
           05  TO2I PIC  X(0008).
      *    -------------------------------
           05  ARCH2L PIC S9(0004) COMP.
           05  ARCH2F PIC  X(0001).
           05  FILLER REDEFINES ARCH2F.
               10  ARCH2A PIC  X(0001).
           05  ARCH2I PIC  X(0008).
      *    -------------------------------
           05  TRLR2L PIC S9(0004) COMP.
           05  TRLR2F PIC  X(0001).
           05  FILLER REDEFINES TRLR2F.
               10  TRLR2A PIC  X(0001).
           05  TRLR2I PIC  X(0004).
      *    -------------------------------
           05  TYPE2L PIC S9(0004) COMP.
           05  TYPE2F PIC  X(0001).
           05  FILLER REDEFINES TYPE2F.
               10  TYPE2A PIC  X(0001).
           05  TYPE2I PIC  X(0001).
      *    -------------------------------
           05  RE2L PIC S9(0004) COMP.
           05  RE2F PIC  X(0001).
           05  FILLER REDEFINES RE2F.
               10  RE2A PIC  X(0001).
           05  RE2I PIC  X(0070).
      *    -------------------------------
           05  RECDTE3L PIC S9(0004) COMP.
           05  RECDTE3F PIC  X(0001).
           05  FILLER REDEFINES RECDTE3F.
               10  RECDTE3A PIC  X(0001).
           05  RECDTE3I PIC  X(0008).
      *    -------------------------------
           05  STOPDT3L PIC S9(0004) COMP.
           05  STOPDT3F PIC  X(0001).
           05  FILLER REDEFINES STOPDT3F.
               10  STOPDT3A PIC  X(0001).
           05  STOPDT3I PIC  X(0008).
      *    -------------------------------
           05  SENT3L PIC S9(0004) COMP.
           05  SENT3F PIC  X(0001).
           05  FILLER REDEFINES SENT3F.
               10  SENT3A PIC  X(0001).
           05  SENT3I PIC  X(0008).
      *    -------------------------------
           05  BY3L PIC S9(0004) COMP.
           05  BY3F PIC  X(0001).
           05  FILLER REDEFINES BY3F.
               10  BY3A PIC  X(0001).
           05  BY3I PIC  X(0004).
      *    -------------------------------
           05  RESEND3L PIC S9(0004) COMP.
           05  RESEND3F PIC  X(0001).
           05  FILLER REDEFINES RESEND3F.
               10  RESEND3A PIC  X(0001).
           05  RESEND3I PIC  X(0008).
      *    -------------------------------
           05  FOLLOW3L PIC S9(0004) COMP.
           05  FOLLOW3F PIC  X(0001).
           05  FILLER REDEFINES FOLLOW3F.
               10  FOLLOW3A PIC  X(0001).
           05  FOLLOW3I PIC  X(0008).
      *    -------------------------------
           05  FORM3L PIC S9(0004) COMP.
           05  FORM3F PIC  X(0001).
           05  FILLER REDEFINES FORM3F.
               10  FORM3A PIC  X(0001).
           05  FORM3I PIC  X(0004).
      *    -------------------------------
           05  TO3L PIC S9(0004) COMP.
           05  TO3F PIC  X(0001).
           05  FILLER REDEFINES TO3F.
               10  TO3A PIC  X(0001).
           05  TO3I PIC  X(0008).
      *    -------------------------------
           05  ARCH3L PIC S9(0004) COMP.
           05  ARCH3F PIC  X(0001).
           05  FILLER REDEFINES ARCH3F.
               10  ARCH3A PIC  X(0001).
           05  ARCH3I PIC  X(0008).
      *    -------------------------------
           05  TRLR3L PIC S9(0004) COMP.
           05  TRLR3F PIC  X(0001).
           05  FILLER REDEFINES TRLR3F.
               10  TRLR3A PIC  X(0001).
           05  TRLR3I PIC  X(0004).
      *    -------------------------------
           05  TYPE3L PIC S9(0004) COMP.
           05  TYPE3F PIC  X(0001).
           05  FILLER REDEFINES TYPE3F.
               10  TYPE3A PIC  X(0001).
           05  TYPE3I PIC  X(0001).
      *    -------------------------------
           05  RE3L PIC S9(0004) COMP.
           05  RE3F PIC  X(0001).
           05  FILLER REDEFINES RE3F.
               10  RE3A PIC  X(0001).
           05  RE3I PIC  X(0070).
      *    -------------------------------
           05  RECDTE4L PIC S9(0004) COMP.
           05  RECDTE4F PIC  X(0001).
           05  FILLER REDEFINES RECDTE4F.
               10  RECDTE4A PIC  X(0001).
           05  RECDTE4I PIC  X(0008).
      *    -------------------------------
           05  STOPDT4L PIC S9(0004) COMP.
           05  STOPDT4F PIC  X(0001).
           05  FILLER REDEFINES STOPDT4F.
               10  STOPDT4A PIC  X(0001).
           05  STOPDT4I PIC  X(0008).
      *    -------------------------------
           05  SENT4L PIC S9(0004) COMP.
           05  SENT4F PIC  X(0001).
           05  FILLER REDEFINES SENT4F.
               10  SENT4A PIC  X(0001).
           05  SENT4I PIC  X(0008).
      *    -------------------------------
           05  BY4L PIC S9(0004) COMP.
           05  BY4F PIC  X(0001).
           05  FILLER REDEFINES BY4F.
               10  BY4A PIC  X(0001).
           05  BY4I PIC  X(0004).
      *    -------------------------------
           05  RESEND4L PIC S9(0004) COMP.
           05  RESEND4F PIC  X(0001).
           05  FILLER REDEFINES RESEND4F.
               10  RESEND4A PIC  X(0001).
           05  RESEND4I PIC  X(0008).
      *    -------------------------------
           05  FOLLOW4L PIC S9(0004) COMP.
           05  FOLLOW4F PIC  X(0001).
           05  FILLER REDEFINES FOLLOW4F.
               10  FOLLOW4A PIC  X(0001).
           05  FOLLOW4I PIC  X(0008).
      *    -------------------------------
           05  FORM4L PIC S9(0004) COMP.
           05  FORM4F PIC  X(0001).
           05  FILLER REDEFINES FORM4F.
               10  FORM4A PIC  X(0001).
           05  FORM4I PIC  X(0004).
      *    -------------------------------
           05  TO4L PIC S9(0004) COMP.
           05  TO4F PIC  X(0001).
           05  FILLER REDEFINES TO4F.
               10  TO4A PIC  X(0001).
           05  TO4I PIC  X(0008).
      *    -------------------------------
           05  ARCH4L PIC S9(0004) COMP.
           05  ARCH4F PIC  X(0001).
           05  FILLER REDEFINES ARCH4F.
               10  ARCH4A PIC  X(0001).
           05  ARCH4I PIC  X(0008).
      *    -------------------------------
           05  TRLR4L PIC S9(0004) COMP.
           05  TRLR4F PIC  X(0001).
           05  FILLER REDEFINES TRLR4F.
               10  TRLR4A PIC  X(0001).
           05  TRLR4I PIC  X(0004).
      *    -------------------------------
           05  TYPE4L PIC S9(0004) COMP.
           05  TYPE4F PIC  X(0001).
           05  FILLER REDEFINES TYPE4F.
               10  TYPE4A PIC  X(0001).
           05  TYPE4I PIC  X(0001).
      *    -------------------------------
           05  RE4L PIC S9(0004) COMP.
           05  RE4F PIC  X(0001).
           05  FILLER REDEFINES RE4F.
               10  RE4A PIC  X(0001).
           05  RE4I PIC  X(0070).
      *    -------------------------------
           05  URECDTEL PIC S9(0004) COMP.
           05  URECDTEF PIC  X(0001).
           05  FILLER REDEFINES URECDTEF.
               10  URECDTEA PIC  X(0001).
           05  URECDTEI PIC  X(0008).
      *    -------------------------------
           05  USENTL PIC S9(0004) COMP.
           05  USENTF PIC  X(0001).
           05  FILLER REDEFINES USENTF.
               10  USENTA PIC  X(0001).
           05  USENTI PIC  X(0008).
      *    -------------------------------
           05  UBYL PIC S9(0004) COMP.
           05  UBYF PIC  X(0001).
           05  FILLER REDEFINES UBYF.
               10  UBYA PIC  X(0001).
           05  UBYI PIC  X(0004).
      *    -------------------------------
           05  UFORML PIC S9(0004) COMP.
           05  UFORMF PIC  X(0001).
           05  FILLER REDEFINES UFORMF.
               10  UFORMA PIC  X(0001).
           05  UFORMI PIC  X(0004).
      *    -------------------------------
           05  AUTHRCVL PIC S9(0004) COMP.
           05  AUTHRCVF PIC  X(0001).
           05  FILLER REDEFINES AUTHRCVF.
               10  AUTHRCVA PIC  X(0001).
           05  AUTHRCVI PIC  X(0001).
      *    -------------------------------
           05  UREL PIC S9(0004) COMP.
           05  UREF PIC  X(0001).
           05  FILLER REDEFINES UREF.
               10  UREA PIC  X(0001).
           05  UREI PIC  X(0070).
      *    -------------------------------
           05  ADDR1L PIC S9(0004) COMP.
           05  ADDR1F PIC  X(0001).
           05  FILLER REDEFINES ADDR1F.
               10  ADDR1A PIC  X(0001).
           05  ADDR1I PIC  X(0030).
      *    -------------------------------
           05  ADDR2L PIC S9(0004) COMP.
           05  ADDR2F PIC  X(0001).
           05  FILLER REDEFINES ADDR2F.
               10  ADDR2A PIC  X(0001).
           05  ADDR2I PIC  X(0030).
      *    -------------------------------
           05  ADDR3L PIC S9(0004) COMP.
           05  ADDR3F PIC  X(0001).
           05  FILLER REDEFINES ADDR3F.
               10  ADDR3A PIC  X(0001).
           05  ADDR3I PIC  X(0030).
      *    -------------------------------
           05  ADDR4L PIC S9(0004) COMP.
           05  ADDR4F PIC  X(0001).
           05  FILLER REDEFINES ADDR4F.
               10  ADDR4A PIC  X(0001).
           05  ADDR4I PIC  X(0030).
      *    -------------------------------
           05  ZIPL PIC S9(0004) COMP.
           05  ZIPF PIC  X(0001).
           05  FILLER REDEFINES ZIPF.
               10  ZIPA PIC  X(0001).
           05  ZIPI PIC  X(0010).
      *    -------------------------------
           05  PHONEL PIC S9(0004) COMP.
           05  PHONEF PIC  X(0001).
           05  FILLER REDEFINES PHONEF.
               10  PHONEA PIC  X(0001).
           05  PHONEI PIC  X(0012).
      *    -------------------------------
           05  TRLRL PIC S9(0004) COMP.
           05  TRLRF PIC  X(0001).
           05  FILLER REDEFINES TRLRF.
               10  TRLRA PIC  X(0001).
           05  TRLRI PIC  X(0004).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0072).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  99.
      *    -------------------------------
           05  PF4KEYL PIC S9(0004) COMP.
           05  PF4KEYF PIC  X(0001).
           05  FILLER REDEFINES PF4KEYF.
               10  PF4KEYA PIC  X(0001).
           05  PF4KEYI PIC  X(0015).
      *    -------------------------------
           05  PF5KEYL PIC S9(0004) COMP.
           05  PF5KEYF PIC  X(0001).
           05  FILLER REDEFINES PF5KEYF.
               10  PF5KEYA PIC  X(0001).
           05  PF5KEYI PIC  X(0014).
       01  EL162AO REDEFINES EL162AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTNOO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LASTNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECDTE1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STOPDT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SENT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BY1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESEND1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLLOW1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TO1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCH1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRLR1O PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RE1O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECDTE2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STOPDT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SENT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BY2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESEND2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLLOW2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TO2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCH2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRLR2O PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RE2O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECDTE3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STOPDT3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SENT3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BY3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESEND3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLLOW3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TO3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCH3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRLR3O PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RE3O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECDTE4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STOPDT4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SENT4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BY4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESEND4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLLOW4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORM4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TO4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCH4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRLR4O PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RE4O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  URECDTEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USENTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UFORMO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUTHRCVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UREO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR3O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR4O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRLRO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF4KEYO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF5KEYO PIC  X(0014).
      *    -------------------------------
00189
00190  01  MAP-REDEF REDEFINES EL162AI.
00191      12  FILLER              PIC X(93).
00192      12  TRL-LINES.
00193        14  TRAILER-LINES  OCCURS 4 TIMES INDEXED BY SC-INDX.
00194          16  SC-RECL         PIC S9(4)  COMP.
00195          16  SC-RECA         PIC X.
00196          16  SC-REC          PIC X(8).
00197          16  SC-REC-O REDEFINES SC-REC PIC 99B99B99.
102610         16  SC-STOPL        PIC S9(4)  COMP.
102610         16  SC-STOPA        PIC X.
102610         16  SC-STOP         PIC X(8).
102610         16  SC-STOP-O REDEFINES SC-STOP PIC 99B99B99.
00201          16  FILLER          PIC X(3).
00202          16  SC-SENT         PIC X(8).
00203          16  FILLER          PIC X(3).
00204          16  SC-BY           PIC X(4).
00205          16  FILLER          PIC X(3).
00206          16  SC-RESEND       PIC X(8).
00207          16  FILLER          PIC X(3).
00208          16  SC-FOLLOWUP     PIC X(8).
00209          16  FILLER          PIC X(3).
00210          16  SC-FORM         PIC X(4).
00211          16  FILLER          PIC X(3).
00212          16  SC-TO           PIC X(8).
00213          16  FILLER          PIC X(3).
00214          16  SC-ARCHIVE      PIC X(8).
00215          16  FILLER          PIC X(3).
00216          16  SC-TRLRNO       PIC 9(4).
00217          16  FILLER          PIC X(3).
00218          16  SC-TYPE         PIC X.
00219          16  FILLER          PIC X(3).
00220          16  SC-REASON       PIC X(70).
00221
00222      EJECT
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
00224
00225  01  DFHCOMMAREA             PIC X(1024).
00226
00227 *01 PARMLIST .
00228 *    02  FILLER              PIC S9(8)   COMP.
00229 *    02  CLAM-POINTER        PIC S9(8)   COMP.
00230 *    02  ACTV-POINTER        PIC S9(8)   COMP.
00231 *    02  ACCT-POINTER        PIC S9(8)   COMP.
00232
00233      EJECT
00234 *                            COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
022122*            88  hospital-claim         value 'H'.
022122*            88  bereavement-claim      value 'B'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
00235
00236      EJECT
00237 *                            COPY ELCTRLR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCTRLR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
050506* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
062806* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
080106* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
041807* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
101807* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
070909* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
071910* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
102610* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
061511* 061511    2011042000002  AJRA  ADD VFY 2ND BENE TO ADDRESS TRAIL
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
021213* 021213    2012092400007  AJRA  CAUSAL STATE SEQUENCE NO
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
102413* 102413  CR2013100800001  AJRA  ADD SPECIAL RELEASE IND
022614* 022614    2013050100003  AJRA  ADD CERT CANCELLED NOTE TYPE - T
040814* 040814    2014030500002  AJRA  ADD ICD CODES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
102418* 102418  CR2018083000001  TANA  ADD ADD NEW CALL TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
00017 ******************************************************************
00018  01  ACTIVITY-TRAILERS.
00019      12  AT-RECORD-ID                    PIC XX.
00020          88  VALID-AT-ID                       VALUE 'AT'.
00021
00022      12  AT-CONTROL-PRIMARY.
00023          16  AT-COMPANY-CD               PIC X.
00024          16  AT-CARRIER                  PIC X.
00025          16  AT-CLAIM-NO                 PIC X(7).
00026          16  AT-CERT-NO.
00027              20  AT-CERT-PRIME           PIC X(10).
00028              20  AT-CERT-SFX             PIC X.
00029          16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
00030              88  AT-1ST-TRL-AVAIL             VALUE +4095.
00031              88  AT-LAST-TRL-AVAIL            VALUE +100.
00032              88  AT-RESV-EXP-HIST-TRL         VALUE +0.
00033              88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
00034              88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
00035              88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
00036              88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
00037              88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
00038              88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
00039              88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
00040              88  AT-DIAGNOSIS-TRL             VALUE +90.
022106             88  AT-BENEFICIARY-TRL           VALUE +91.
022106             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
061511             88  AT-VFY-2ND-BENE-NOTE-TRL     VALUE +93.
021213             88  AT-VFY-CAUSAL-STATE          VALUE +94.
                   88  AT-ERROR-MSGS-TRL            VALUE +95.
00041
00042      12  AT-TRAILER-TYPE                 PIC X.
00043          88  RESERVE-EXPENSE-TR               VALUE '1'.
00044          88  PAYMENT-TR                       VALUE '2'.
00045          88  AUTO-PAY-TR                      VALUE '3'.
00046          88  CORRESPONDENCE-TR                VALUE '4'.
00047          88  ADDRESS-TR                       VALUE '5'.
00048          88  GENERAL-INFO-TR                  VALUE '6'.
00049          88  AUTO-PROMPT-TR                   VALUE '7'.
00050          88  DENIAL-TR                        VALUE '8'.
00051          88  INCURRED-CHG-TR                  VALUE '9'.
00052          88  FORM-CONTROL-TR                  VALUE 'A'.
00053
00054      12  AT-RECORDED-DT                  PIC XX.
00055      12  AT-RECORDED-BY                  PIC X(4).
00056      12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
00057
00058      12  AT-TRAILER-BODY                 PIC X(165).
00059
00060      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
00061          16  AT-RESERVE-CONTROLS.
00062              20  AT-MANUAL-SW            PIC X.
00063                  88  AT-MANUAL-RESERVES-USED VALUE '1'.
00064              20  AT-FUTURE-SW            PIC X.
00065                  88  AT-FUTURE-RESERVES-USED VALUE '1'.
00066              20  AT-PTC-SW               PIC X.
00067                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
00068              20  AT-IBNR-SW              PIC X.
00069                  88  AT-IBNR-RESERVES-USED   VALUE '1'.
00070              20  AT-PTC-LF-SW            PIC X.
00071                  88  AT-LF-PTC-USED          VALUE '1'.
00072              20  AT-CDT-ACCESS-METHOD    PIC X.
00073                  88  AT-CDT-ROUND-NEAR       VALUE '1'.
00074                  88  AT-CDT-ROUND-HIGH       VALUE '2'.
00075                  88  AT-CDT-INTERPOLATED     VALUE '3'.
00076              20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
00077          16  AT-LAST-COMPUTED-DT         PIC XX.
101807         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
101807         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
101807         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
101807         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
00084          16  AT-EXPENSE-CONTROLS.
00085              20  AT-EXPENSE-METHOD       PIC X.
00086                  88  NO-EXPENSE-CALCULATED    VALUE '1'.
00087                  88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
00088                  88  PERCENT-OF-PMT           VALUE '3'.
00089                  88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
00090              20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
00091              20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
00092          16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
00093          16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
00094
00095          16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
00096          16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
00097
101807*        16  FILLER                      PIC X(53).
101807         16  FILLER                      PIC X(47).
00099
00100          16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
00101          16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
00102
00103          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
00104              20  AT-OPEN-CLOSE-DATE      PIC XX.
00105              20  AT-OPEN-CLOSE-TYPE      PIC X.
00106 *                    C = CLOSED
00107 *                    O = OPEN
00108              20  AT-OPEN-CLOSE-REASON    PIC X(5).
00109 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
00110
00111      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
00112          16  AT-PAYMENT-TYPE             PIC X.
00113              88  PARTIAL-PAYMENT                VALUE '1'.
00114              88  FINAL-PAYMENT                  VALUE '2'.
00115              88  LUMP-SUM-PAYMENT               VALUE '3'.
00116              88  ADDITIONAL-PAYMENT             VALUE '4'.
00117              88  CHARGEABLE-EXPENSE             VALUE '5'.
00118              88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
00119              88  VOIDED-PAYMENT                 VALUE '9'.
00120              88  TRANSFER                       VALUE 'T'.
022106             88  LIFE-INTEREST                  VALUE 'I'.
00121
00122          16  AT-CLAIM-TYPE               PIC X.
00123              88  PAID-FOR-AH                    VALUE 'A'.
00124              88  PAID-FOR-LIFE                  VALUE 'L'.
00124              88  PAID-FOR-IUI                   VALUE 'I'.
120503             88  PAID-FOR-GAP                   VALUE 'G'.
052614             88  PAID-FOR-FAM                   VALUE 'F'.
022122             88  PAID-FOR-BRV                   VALUE 'B'.
022122             88  PAID-FOR-HOS                   VALUE 'H'.
100518             88  PAID-FOR-OTH                   VALUE 'O'.
00125          16  AT-CLAIM-PREM-TYPE          PIC X.
00126              88  AT-SINGLE-PREMIUM              VALUE '1'.
00127              88  AT-O-B-COVERAGE                VALUE '2'.
00128              88  AT-OPEN-END-COVERAGE           VALUE '3'.
00129          16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
00130          16  AT-CHECK-NO                 PIC X(7).
00131          16  AT-PAID-FROM-DT             PIC XX.
00132          16  AT-PAID-THRU-DT             PIC XX.
00133          16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
013017         16  AT-ACH-PAYMENT              PIC X.
013017*        16  FILLER                      PIC X.
00135          16  AT-PAYEES-NAME              PIC X(30).
00136          16  AT-PAYMENT-ORIGIN           PIC X.
00137              88  ONLINE-MANUAL-PMT              VALUE '1'.
00138              88  ONLINE-AUTO-PMT                VALUE '2'.
00139              88  OFFLINE-PMT                    VALUE '3'.
00140          16  AT-CHECK-WRITTEN-DT         PIC XX.
00141          16  AT-TO-BE-WRITTEN-DT         PIC XX.
00142          16  AT-VOID-DATA.
00143              20  AT-VOID-DT              PIC XX.
041807*00144       20  AT-VOID-REASON          PIC X(30).
041807             20  AT-VOID-REASON          PIC X(26).
041807         16  AT-PMT-APPROVED-BY          PIC X(04).
00145          16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
00146          16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
082807         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
082807                                         PIC S99V9(5)  COMP-3.
00147          16  AT-CREDIT-INTERFACE.
00148              20  AT-PMT-SELECT-DT        PIC XX.
00149                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
00150              20  AT-PMT-ACCEPT-DT        PIC XX.
00151                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
00152              20  AT-VOID-SELECT-DT       PIC XX.
00153                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
00154              20  AT-VOID-ACCEPT-DT       PIC XX.
00155                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
00156
00157          16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
00158                  88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00159                  88  CONVERSION-PAYMENT           VALUE +99999999.
00160          16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
00161
00162          16  AT-FORCE-CONTROL            PIC X.
00163              88  PAYMENT-WAS-FORCED           VALUE '1'.
00164          16  AT-PREV-LAST-PMT-DT         PIC XX.
00165          16  AT-PREV-PAID-THRU-DT        PIC XX.
00166          16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
00167          16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
00168          16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
00169          16  AT-BENEFIT-TYPE             PIC X.
00170
00171          16  AT-EXPENSE-TYPE             PIC X.
00172          16  AT-PAYMENT-APPROVAL-SW      PIC X.
00173
00174          16  AT-PAYEE-TYPE-CD.
00175              20  AT-PAYEE-TYPE           PIC X.
00176                  88  INSURED-PAID           VALUE 'I'.
00177                  88  BENEFICIARY-PAID       VALUE 'B'.
00178                  88  ACCOUNT-PAID           VALUE 'A'.
00179                  88  OTHER-1-PAID           VALUE 'O'.
00180                  88  OTHER-2-PAID           VALUE 'Q'.
00181                  88  DOCTOR-PAID            VALUE 'P'.
00182                  88  EMPLOYER-PAID          VALUE 'E'.
00183              20  AT-PAYEE-SEQ            PIC X.
00184
00185          16  AT-CASH-PAYMENT             PIC X.
00186          16  AT-GROUPED-PAYMENT          PIC X.
00187          16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
00188          16  AT-APPROVAL-LEVEL-REQD      PIC X.
00189          16  AT-APPROVED-LEVEL           PIC X.
00190          16  AT-VOID-TYPE                PIC X.
00191              88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
00192              88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
00193          16  AT-AIG-UNEMP-IND            PIC X.
00194              88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
00195          16  AT-ASSOCIATES               PIC X.
00196              88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
00197              88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
00198
00199          16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
00200          16  AT-CV-PMT-CODE              PIC X.
00201              88  FULL-DEATH-PAYMENT         VALUE '1'.
00202              88  HALF-DEATH-PAYMENT         VALUE '2'.
00203              88  FULL-ADD-PAYMENT           VALUE '3'.
00204              88  HALF-ADD-PAYMENT           VALUE '4'.
00205              88  FULL-RIDER-PAYMENT         VALUE '5'.
00206              88  HALF-RIDER-PAYMENT         VALUE '6'.
00207              88  NON-CHG-EXP-PAYMENT        VALUE '7'.
00208              88  ADDL-PAYMENT               VALUE '8'.
00209
00210          16  AT-EOB-CODE1                PIC XXX.
00211          16  AT-EOB-CODE2                PIC XXX.
00212          16  AT-EOB-CODE3                PIC XXX.
020413         16  FILLER REDEFINES AT-EOB-CODE3.
020413             20  AT-PRINT-CLM-FORM       PIC X.
020413             20  AT-PRINT-SURVEY         PIC X.
102413             20  AT-SPECIAL-RELEASE      PIC X.
00213          16  AT-EOB-CODE4                PIC XXX.
               16  FILLER REDEFINES AT-EOB-CODE4.
                   20  AT-INT-PMT-SELECT-DT    PIC XX.
                   20  FILLER                  PIC X.
00214          16  AT-EOB-CODE5                PIC XXX.
062806         16  FILLER REDEFINES AT-EOB-CODE5.
062806             20  AT-PMT-PROOF-DT         PIC XX.
062806             20  FILLER                  PIC X.
00215
071910         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
071910             88  AT-PRINT-EOB            VALUE 'Y'.
00217
00218          16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
00219          16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
00220
00221      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
00222          16  AT-SCHEDULE-START-DT        PIC XX.
00223          16  AT-SCHEDULE-END-DT          PIC XX.
00224          16  AT-TERMINATED-DT            PIC XX.
00225          16  AT-LAST-PMT-TYPE            PIC X.
00226              88  LAST-PMT-IS-FINAL              VALUE 'F'.
00227              88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
00228          16  AT-FIRST-PMT-DATA.
00229              20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
00230              20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
00231              20  AT-1ST-PAY-THRU-DT      PIC XX.
00232          16  AT-REGULAR-PMT-DATA.
00233              20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
00234              20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
00235              20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
00236          16  AT-AUTO-PAYEE-CD.
00237              20  AT-AUTO-PAYEE-TYPE      PIC X.
00238                  88  INSURED-PAID-AUTO      VALUE 'I'.
00239                  88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
00240                  88  ACCOUNT-PAID-AUTO      VALUE 'A'.
00241                  88  OTHER-1-PAID-AUTO      VALUE 'O'.
00242                  88  OTHER-2-PAID-AUTO      VALUE 'Q'.
00243              20  AT-AUTO-PAYEE-SEQ       PIC X.
00244          16  AT-AUTO-PAY-DAY             PIC 99.
00245          16  AT-AUTO-CASH                PIC X.
00246              88  AT-CASH                      VALUE 'Y'.
00247              88  AT-NON-CASH                  VALUE 'N'.
070909*        16  FILLER                      PIC X(129).
070909         16  AT-AUTO-END-LETTER          PIC X(4).
070909         16  FILLER                      PIC X(125).
00249
00250          16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
00251          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
00252
00253      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
00254          16  AT-LETTER-SENT-DT           PIC XX.
00255          16  AT-RECEIPT-FOLLOW-UP        PIC XX.
00256          16  AT-AUTO-RE-SEND-DT          PIC XX.
00257          16  AT-LETTER-ANSWERED-DT       PIC XX.
00258          16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
00259          16  AT-LETTER-ORIGIN            PIC X.
00260              88  ONLINE-CREATION              VALUE '1' '3'.
00261              88  OFFLINE-CREATION             VALUE '2' '4'.
                   88  NAPER-ONLINE-CREATION        VALUE '3'.
                   88  NAPER-OFFLINE-CREATION       VALUE '4'.
00262          16  AT-STD-LETTER-FORM          PIC X(4).
00263          16  AT-REASON-TEXT              PIC X(70).
00264          16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
00265          16  AT-ADDRESEE-TYPE            PIC X.
00266               88  INSURED-ADDRESEE            VALUE 'I'.
00267               88  BENEFICIARY-ADDRESEE        VALUE 'B'.
00268               88  ACCOUNT-ADDRESEE            VALUE 'A'.
00269               88  PHYSICIAN-ADDRESEE          VALUE 'P'.
00270               88  EMPLOYER-ADDRESEE           VALUE 'E'.
00271               88  OTHER-ADDRESEE-1            VALUE 'O'.
00272               88  OTHER-ADDRESEE-2            VALUE 'Q'.
00273          16  AT-ADDRESSEE-NAME           PIC X(30).
00274          16  AT-INITIAL-PRINT-DATE       PIC XX.
00275          16  AT-RESEND-PRINT-DATE        PIC XX.
00276          16  AT-CORR-SOL-UNSOL           PIC X.
00277          16  AT-LETTER-PURGED-DT         PIC XX.
CIDMOD*
CIDMOD*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
CIDMOD*
CIDMOD         16  AT-CSO-REDEFINITION.
040110             20  AT-RESEND-LETTER-FORM   PIC X(4).
040110             20  AT-AUTO-CLOSE-IND       PIC X(1).
040110             20  AT-LETTER-TO-BENE       PIC X(1).
102610             20  AT-STOP-LETTER-DT       PIC X(2).
062217             20  AT-AUTH-RCVD            PIC X(1).
062217             20  FILLER                  PIC X(18).
040110*             20  FILLER                  PIC X(27).
CIDMOD             20  AT-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
CIDMOD                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
CIDMOD                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
CIDMOD             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
CIDMOD             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
CIDMOD*
CIDMOD*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
CIDMOD*
CIDMOD*        16  FILLER                      PIC X(26).
CIDMOD*
CIDMOD*        16  AT-DMD-BSR-CODE             PIC X.
CIDMOD*            88  AT-AUTOMATED-BSR              VALUE 'A'.
CIDMOD*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
CIDMOD*
CIDMOD*        16  AT-DMD-LETTER-STATUS        PIC X.
CIDMOD*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
CIDMOD*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
CIDMOD*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
CIDMOD*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
CIDMOD*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
00290
00291          16  AT-CORR-LAST-MAINT-DT       PIC XX.
00292          16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
00293
00294      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
00295          16  AT-ADDRESS-TYPE             PIC X.
00296              88  INSURED-ADDRESS               VALUE 'I'.
00297              88  BENEFICIARY-ADDRESS           VALUE 'B'.
00298              88  ACCOUNT-ADDRESS               VALUE 'A'.
00299              88  PHYSICIAN-ADDRESS             VALUE 'P'.
00300              88  EMPLOYER-ADDRESS              VALUE 'E'.
00301              88  OTHER-ADDRESS-1               VALUE 'O'.
00302              88  OTHER-ADDRESS-2               VALUE 'Q'.
00303          16  AT-MAIL-TO-NAME             PIC X(30).
00304          16  AT-ADDRESS-LINE-1           PIC X(30).
00305          16  AT-ADDRESS-LINE-2           PIC X(30).
00306          16  AT-CITY-STATE.
                   20  AT-CITY                 PIC X(28).
                   20  AT-STATE                PIC XX.
00307          16  AT-ZIP.
00308              20  AT-ZIP-CODE.
00309                  24  AT-ZIP-1ST          PIC X.
00310                      88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00311                  24  FILLER              PIC X(4).
00312              20  AT-ZIP-PLUS4            PIC X(4).
00313          16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
00314              20  AT-CAN-POSTAL-1         PIC XXX.
00315              20  AT-CAN-POSTAL-2         PIC XXX.
00316              20  FILLER                  PIC XXX.
00317          16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
061511*         16  FILLER                      PIC X(23).
061511         16  AT-VFY-2ND-BENE-SSN         PIC X(9).
061511         16  AT-VFY-2ND-BENE-VERIFIED    PIC X.
061511         16  FILLER                      PIC X(13).
00319          16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
00320          16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
00321
00322      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00323          16  AT-INFO-LINE-1              PIC X(60).
061013         16  FILLER REDEFINES AT-INFO-LINE-1.
061013             20  AT-NOTE-ERROR-NO OCCURS 15
061013                                         PIC X(4).
00324          16  AT-INFO-LINE-2              PIC X(60).
040814         16  FILLER REDEFINES AT-INFO-LINE-2.
040814             20  AT-ICD-CODE-1           PIC X(8).
040814             20  AT-ICD-CODE-2           PIC X(8).
040814             20  FILLER                  PIC X(44).
00325          16  AT-INFO-TRAILER-TYPE        PIC X.
061013             88  AT-ERRORS-NOTE          VALUE 'E'.
00326              88  AT-PAYMENT-NOTE         VALUE 'P'.
00327              88  AT-CALL-NOTE            VALUE 'C'.
00328              88  AT-MAINT-NOTE           VALUE 'M'.
00329              88  AT-CERT-CHANGE          VALUE 'X'.
080106             88  AT-APPROVAL-NOTE        VALUE 'R'.
080106             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
022614             88  AT-CERT-CANCELLED       VALUE 'T'.
00330          16  AT-CALL-TYPE                PIC X.
00331              88  AT-PHONE-CALL-IN        VALUE 'I'.
102418             88  AT-PHONE-CALL-NEW       VALUE 'N'.
00332              88  AT-PHONE-CALL-OUT       VALUE 'O'.
00333          16  AT-NOTE-CONTINUATION        PIC X.
00334              88  AT-CONTINUED-NOTE       VALUE 'X'.
071910         16  AT-EOB-CODES-EXIST          PIC X.
071910             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
00335          16  FILLER                      PIC X(35).
00336          16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
00337          16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
00338
00339      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
00340          16  AT-PROMPT-LINE-1            PIC X(60).
00341          16  AT-PROMPT-LINE-2            PIC X(60).
00342          16  AT-PROMPT-START-DT          PIC XX.
00343          16  AT-PROMPT-END-DT            PIC XX.
00344          16  FILLER                      PIC X(35).
00345          16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
00346          16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
00347
00348      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00349          16  AT-DENIAL-INFO-1            PIC X(60).
00350          16  AT-DENIAL-INFO-2            PIC X(60).
00351          16  AT-DENIAL-DT                PIC XX.
00352          16  AT-RETRACTION-DT            PIC XX.
00353          16  AT-DENIAL-REASON-CODE       PIC X(4).
050506*         16  FILLER                      PIC X(31).
050506         16  AT-DENIAL-PROOF-DT          PIC XX.
050506         16  FILLER                      PIC X(29).
00355          16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
00356          16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
00357
00358      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
00359          16  AT-OLD-INCURRED-DT          PIC XX.
00360          16  AT-OLD-REPORTED-DT          PIC XX.
00361          16  AT-OLD-ESTABLISHED-DT       PIC XX.
00362          16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
00363          16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
00364          16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
00365          16  AT-OLD-PAID-THRU-DT         PIC XX.
00366          16  AT-LAST-PMT-MADE-DT         PIC XX.
00367          16  FILLER                      PIC X(26).
00368          16  AT-OLD-DIAG-CODE            PIC X(6).
00369          16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
00370          16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
00371          16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
00372          16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
00373          16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
00374          16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
00375          16  AT-OLD-DIAG-DESCRIP         PIC X(60).
040814         16  AT-OLD-ICD-CODE-1           PIC X(8).
040814         16  AT-OLD-ICD-CODE-2           PIC X(8).
040814         16  FILLER                      PIC X(9).
00377          16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
00378
00379      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
00380          16  AT-FORM-SEND-ON-DT          PIC XX.
00381          16  AT-FORM-FOLLOW-UP-DT        PIC XX.
00382          16  AT-FORM-RE-SEND-DT          PIC XX.
00383          16  AT-FORM-ANSWERED-DT         PIC XX.
00384          16  AT-FORM-PRINTED-DT          PIC XX.
00385          16  AT-FORM-REPRINT-DT          PIC XX.
00386          16  AT-FORM-TYPE                PIC X.
00387              88  INITIAL-FORM                  VALUE '1'.
00388              88  PROGRESS-FORM                 VALUE '2'.
00389          16  AT-INSTRUCT-LN-1            PIC X(28).
00390          16  AT-INSTRUCT-LN-2            PIC X(28).
00391          16  AT-INSTRUCT-LN-3            PIC X(28).
00392          16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
00393          16  AT-FORM-ADDRESS             PIC X.
00394              88  FORM-TO-INSURED              VALUE 'I'.
00395              88  FORM-TO-ACCOUNT              VALUE 'A'.
00396              88  FORM-TO-OTHER-1              VALUE 'O'.
00397              88  FORM-TO-OTHER-2              VALUE 'Q'.
00398          16  AT-RELATED-1.
00399              20 AT-REL-CARR-1            PIC X.
00400              20 AT-REL-CLAIM-1           PIC X(7).
00401              20 AT-REL-CERT-1            PIC X(11).
00402          16  AT-RELATED-2.
00403              20 AT-REL-CARR-2            PIC X.
00404              20 AT-REL-CLAIM-2           PIC X(7).
00405              20 AT-REL-CERT-2            PIC X(11).
00406          16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
00407          16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
00408          16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
00409          16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
00410          16  AT-FORM-REM-PRINT-DT        PIC XX.
102610         16  AT-STOP-FORM-DT             PIC X(2).
00411
102610         16  FILLER                      PIC X(09).
00413          16  AT-FORM-LAST-MAINT-DT       PIC XX.
00414          16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
00415 ******************************************************************
00238
00239      EJECT
00240 *                            COPY ERCACCT.
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
00241
00242      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                ACTIVITY-TRAILERS ACCOUNT-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL162' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00244
00245      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00246      MOVE '5'                   TO DC-OPTION-CODE.
00247      PERFORM 9700-DATE-LINK.
00248      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00249      MOVE DC-BIN-DATE-1         TO  BIN-CURRENT-DATE.
00250
00251      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00252
022619     MOVE -1                     TO DC-ELAPSED-MONTHS
022619     MOVE +0                     TO DC-ELAPSED-DAYS
022619     MOVE '6'                    TO DC-OPTION-CODE
022619     PERFORM 9700-DATE-LINK
022619     MOVE DC-BIN-DATE-2          TO CURRENT-MINUS1-SAVE
00259
00260      IF EIBCALEN NOT GREATER THAN ZERO
00261          GO TO 8800-UNAUTHORIZED-ACCESS.
00262
00263      IF PI-CALLING-PROGRAM = PGM-EL150
00264          MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00265          MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00266          MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00267          MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00268          MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00269          MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00270          MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00271          MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00272          MOVE +1                   TO SUB
00273          PERFORM  2000-ZERO-PI-TRLR-TABLE THRU 2010-EXIT
00274          MOVE LOW-VALUES           TO EL162AO
00275                                       PI-DISPLAYED-TRAILER-CODES
00276          MOVE 1                    TO PI-ACTV-SEQ
00277                                       PI-NEXT-TRLR-SUB
00278          GO TO 7000-BUILD-SCREEN.
00279
00280      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00281          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00282 ************ FIRST TIME IN FROM MENU, FORCE THE CLAIM
00283 ************ TO BE DESIGNATED THRU EL132.
00284              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00285              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00286              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00287              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00288              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00289              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00290              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00291              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00292              MOVE PGM-EL132            TO PGM-NAME
00293              GO TO 9300-XCTL
00294          ELSE
00295              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00296              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00297              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00298              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00299              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00300              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00301              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00302              MOVE SPACES               TO PI-SAVED-PROGRAM-6
00303              MOVE +1                   TO SUB
00304              PERFORM  2000-ZERO-PI-TRLR-TABLE THRU 2010-EXIT
00305              MOVE LOW-VALUES           TO EL162AO
00306                                        PI-DISPLAYED-TRAILER-CODES
00307              MOVE 1                    TO PI-ACTV-SEQ
00308                                           PI-NEXT-TRLR-SUB
00309              GO TO 7000-BUILD-SCREEN.
00310
00311      IF EIBAID = DFHCLEAR
00312          GO TO 9400-CLEAR.
00313
00314      
      * EXEC CICS HANDLE CONDITION
00315 *        PGMIDERR(9600-PGMID-ERROR)
00316 *        ERROR   (9990-ABEND)
00317 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00003127' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033313237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00318
00319      IF PI-PROCESSOR-ID = 'LGXX'
00320          GO TO 0200-RECEIVE.
00321
00322      
      * EXEC CICS READQ TS
00323 *        QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00324 *        INTO    (SECURITY-CONTROL)
00325 *        LENGTH  (SC-COMM-LENGTH)
00326 *        ITEM    (SC-ITEM)
00327 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003135' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00328
00329      MOVE SC-CLAIMS-DISPLAY (2)    TO  PI-DISPLAY-CAP.
00330      MOVE SC-CLAIMS-UPDATE  (2)    TO  PI-MODIFY-CAP.
00331
00332      IF NOT MODIFY-CAP
00333          MOVE 'UPDATE'             TO  SM-READ
00334          PERFORM 9995-SECURITY-VIOLATION
00335          MOVE ER-0070              TO  EMI-ERROR
00336          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00337          GO TO 8100-SEND-INITIAL-MAP.
00338
00339      EJECT
00340  0200-RECEIVE.
00341
00342      MOVE LOW-VALUES TO EL162AI.
00343
00344      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00345          MOVE ER-0008            TO EMI-ERROR
00346          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00347          GO TO 8200-SEND-DATAONLY.
00348
00349      
      * EXEC CICS RECEIVE
00350 *        MAP   (MAP-NAME)
00351 *        MAPSET(MAPSET-NAME)
00352 *        INTO  (EL162AI)
00353 *    END-EXEC.
           MOVE LENGTH OF
            EL162AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003162' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL162AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00354
00355      IF ENTERPFL = ZERO
00356          GO TO 0300-CHECK-PFKEYS.
00357
00358      IF EIBAID NOT = DFHENTER
00359          MOVE ER-0004            TO EMI-ERROR
00360          GO TO 0320-INPUT-ERROR.
00361
00362      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)
00363          MOVE PF-VALUES (ENTERPFI) TO EIBAID
00364      ELSE
00365          MOVE ER-0029            TO EMI-ERROR
00366          GO TO 0320-INPUT-ERROR.
00367
00368  0300-CHECK-PFKEYS.
00369      IF EIBAID = DFHPF23
00370          GO TO 8810-PF23.
00371
00372      IF EIBAID = DFHPF24
00373          GO TO 9200-RETURN-MAIN-MENU.
00374
00375      IF EIBAID = DFHPF12
00376          GO TO 9500-PF12.
00377
00378      IF PI-COMPANY-ID = 'DMD'
00379          MOVE SPACE              TO PI-DMD-FORCE-ERROR.
00380
00381      IF EIBAID = DFHPF4
00382          IF PI-COMPANY-ID NOT = 'DMD'
00383              MOVE ER-0029        TO EMI-ERROR
00384              GO TO 0320-INPUT-ERROR
00385          ELSE
00386              MOVE 'X'            TO PI-DMD-FORCE-ERROR
00387              MOVE DFHENTER       TO EIBAID
00388              GO TO 0330-EDIT-DATA.
00389
00390      IF EIBAID = DFHPF5
00391          IF PI-RETURN-TO-PROGRAM = 'EL150'
00392              MOVE ER-0029        TO EMI-ERROR
00393              GO TO 0320-INPUT-ERROR
00394          ELSE
00395              MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA
00396              MOVE PGM-EL132      TO PGM-NAME
00397              GO TO 9300-XCTL.
00398
00399      IF PI-CLAIM-NO = SPACES
00400         MOVE ER-0319             TO EMI-ERROR
00401         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00402         GO TO 8100-SEND-INITIAL-MAP.
00403
102610     IF SC-RECL (1) > 0 OR SC-STOPL (1) > 0 OR
102610        SC-RECL (2) > 0 OR SC-STOPL (2) > 0 OR
102610        SC-RECL (3) > 0 OR SC-STOPL (3) > 0 OR
102620        SC-RECL (4) > 0 OR SC-STOPL (4) > 0 OR
00408         UREL > 0  OR USENTL > 0  OR
051107        UFORML > 0  OR URECDTEL > 0
00410           IF EIBAID NOT = DFHENTER
00411              MOVE ER-0050            TO EMI-ERROR
00412              GO TO 0320-INPUT-ERROR.
00413
00414      IF EIBAID = DFHPF1
00415          GO TO 6000-BROWSE-FORWARD.
00416
00417      IF EIBAID = DFHPF2
00418          GO TO 6100-BROWSE-BACKWARD.
00419
00420      IF EIBAID = DFHPF6
00421          MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA
00422          MOVE PGM-EL141          TO PGM-NAME
00423          GO TO 9300-XCTL.
00419
010416     IF EIBAID = DFHPF7
010416         MOVE PGM-EL1284          TO PGM-NAME
010416         GO TO 9300-XCTL.
00424
00425      IF EIBAID = DFHENTER
00426          GO TO 0330-EDIT-DATA.
00427
00428      MOVE ER-0029                TO EMI-ERROR.
00429
00430  0320-INPUT-ERROR.
00431      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00432
00433      MOVE AL-UNBON        TO ENTERPFA.
00434
00435      IF ENTERPFL = 0
00436          MOVE -1          TO URECDTEL
00437      ELSE
00438          MOVE -1          TO ENTERPFL.
00439
00440      GO TO 8200-SEND-DATAONLY.
00441
00442      EJECT
00443  0330-EDIT-DATA.
00444      SET SC-INDX  INDX           TO 1.
102610     SET INDX2                   TO 1.
022619     perform 3000-BUILD-KEYS
022619     
      * EXEC CICS READ
022619*       DATASET (ELMSTR-FILE-ID)
022619*       RIDFLD  (PI-CLAM-KEY)
022619*       SET     (ADDRESS OF CLAIM-MASTER)
022619*       resp    (ws-response)
022619*    END-EXEC
      *    MOVE '&"S        E          (  N#00003264' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
022619
022619     if not resp-normal
022619        go to 8860-CLAIM-NOT-FOUND
022619     end-if
022619     .
00446  0331-LOOP.
00447      IF SC-RECL (SC-INDX) > ZERO
00448         MOVE SC-REC (SC-INDX)    TO DEEDIT-FIELD
00449         PERFORM 8600-DEEDIT
00450         MOVE '1'                 TO REC-DATA-SW
00451         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
00452         MOVE '4'                 TO DC-OPTION-CODE
00453         PERFORM 9700-DATE-LINK
00454         IF DATE-CONVERSION-ERROR
00455            MOVE ER-0021          TO EMI-ERROR
00456            MOVE -1               TO SC-RECL (SC-INDX)
00457            MOVE AL-UABON         TO SC-RECA (SC-INDX)
00458            PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
00459         ELSE
121406           IF DC-BIN-DATE-1 > BIN-CURRENT-DATE
121406              MOVE ER-0539       TO EMI-ERROR
121406              MOVE -1            TO SC-RECL (SC-INDX)
121406              MOVE AL-UABON      TO SC-RECA (SC-INDX)
121406              PERFORM 9900-ERROR-FORMAT
121406                                 THRU 9900-EXIT
                 ELSE
00460               MOVE DC-BIN-DATE-1 TO REC-DATE-SAVE (INDX)
00461               MOVE DEEDIT-FIELD-V0
                                       TO SC-REC-O (SC-INDX)
00462               INSPECT SC-REC (SC-INDX) CONVERTING ' ' TO '/'
121406           END-IF
121406        END-IF
00463      ELSE
00464         MOVE LOW-VALUES          TO REC-DATE-SAVE (INDX)
121406     END-IF
00466      IF PI-COMPANY-ID = 'DMD'
00467        IF SC-RECL (SC-INDX) > 0
00468          IF REC-DATE-SAVE (INDX) < CURRENT-MINUS1-SAVE
00469          IF NOT PI-DMD-FORCED
00470            MOVE ER-7839        TO EMI-ERROR
00471            MOVE -1             TO SC-RECL (SC-INDX)
00472            MOVE AL-UABON       TO SC-RECA (SC-INDX)
00473            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00474          END-IF
00475          ELSE
00476          IF REC-DATE-SAVE (INDX) > BIN-CURRENT-DATE
00477            MOVE ER-0539          TO EMI-ERROR
00478            MOVE -1               TO SC-RECL (SC-INDX)
00479            MOVE AL-UABON         TO SC-RECA (SC-INDX)
00480            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00481
102610     IF SC-STOPL (SC-INDX) > ZERO
102610        MOVE SC-STOP (SC-INDX)    TO DEEDIT-FIELD
102610        PERFORM 8600-DEEDIT
102610        MOVE '1'                 TO REC-DATA-SW
102610        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
102610        MOVE '4'                 TO DC-OPTION-CODE
102610        PERFORM 9700-DATE-LINK
102610        IF DATE-CONVERSION-ERROR
102610           MOVE ER-0021          TO EMI-ERROR
102610           MOVE -1               TO SC-STOPL (SC-INDX)
102610           MOVE AL-UABON         TO SC-STOPA (SC-INDX)
102610           PERFORM 9900-ERROR-FORMAT
102610                                 THRU 9900-EXIT
102610        ELSE
102610           IF DC-BIN-DATE-1 > BIN-CURRENT-DATE
102610              MOVE ER-0895       TO EMI-ERROR
102610              MOVE -1            TO SC-STOPL (SC-INDX)
102610              MOVE AL-UABON      TO SC-STOPA (SC-INDX)
102610              PERFORM 9900-ERROR-FORMAT
102610                                 THRU 9900-EXIT
102610           ELSE
102610              MOVE DC-BIN-DATE-1 TO STOP-DATE-SAVE (INDX2)
102610              MOVE DEEDIT-FIELD-V0 TO SC-STOP-O (SC-INDX)
102610              INSPECT SC-STOP (SC-INDX) CONVERTING ' ' TO '/'
102610           END-IF
102610        END-IF
102610     ELSE
102610        MOVE LOW-VALUES          TO STOP-DATE-SAVE (INDX2)
102610     END-IF.
00493
00494      SET SC-INDX
102610            INDX2
00495             INDX UP BY 1.
00496
00497      IF SC-INDX NOT = 5
00498         GO TO 0331-LOOP.
00502      IF URECDTEL > 0
00503         MOVE URECDTEI            TO DEEDIT-FIELD
00504         PERFORM 8600-DEEDIT
00505         MOVE '1'                 TO UNSOL-DATA-SW
00506         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
00507         MOVE '4'                 TO DC-OPTION-CODE
00508         PERFORM 9700-DATE-LINK
00509         IF DATE-CONVERSION-ERROR
00510            MOVE ER-0021          TO EMI-ERROR
00511            MOVE -1               TO URECDTEL
00512            MOVE AL-UABON         TO URECDTEA
00513            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00514         ELSE
00515            MOVE DC-BIN-DATE-1    TO URECDTE-SAVE
00516            MOVE DEEDIT-FIELD-V0  TO URECDTEO
00517            INSPECT URECDTEI CONVERTING ' ' TO '/'
022619           if (urecdte-save < cl-incurred-dt)
022619              or (urecdte-save < cl-reported-dt)
022619              or (urecdte-save < cl-file-establish-dt)
022619              or (urecdte-save < cl-cert-eff-dt)
022619              or (urecdte-save < CURRENT-MINUS1-SAVE)
022619              if (pi-receive-dt-cnt > 0)
022619                 and (urecdte-save = pi-prev-rec-dt)
022619                 move 0          to pi-receive-dt-cnt
022619                 move low-values to pi-prev-rec-dt
022619              else
022619                 move 1          to pi-receive-dt-cnt
022619                 move urecdte-save
022619                                 to pi-prev-rec-dt
022619                 move er-1825    to emi-error
022619                 move -1         to urecdtel
022619                 move al-uabon   to urecdtea
022619                 perform 9900-error-format
022619                                 thru 9900-exit
022619              end-if
022619           end-if
022619        end-if
022619     end-if
00519      IF PI-COMPANY-ID = 'DMD'
00520        IF URECDTEL > 0
00521          IF URECDTE-SAVE < CURRENT-MINUS1-SAVE
00522          IF NOT PI-DMD-FORCED
00523            MOVE ER-7839        TO EMI-ERROR
00524            MOVE -1             TO URECDTEL
00525            MOVE AL-UABON       TO URECDTEA
00526            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00527          END-IF
00528          ELSE
00529          IF URECDTE-SAVE > BIN-CURRENT-DATE
00530            MOVE ER-0539          TO EMI-ERROR
00531            MOVE -1               TO URECDTEL
00532            MOVE AL-UABON         TO URECDTEA
00533            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00534
00535      IF USENTL > 0
00536         MOVE USENTI              TO DEEDIT-FIELD
00537         PERFORM 8600-DEEDIT
00538         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
00539         MOVE '4'                 TO DC-OPTION-CODE
00540         PERFORM 9700-DATE-LINK
00541         IF DATE-CONVERSION-ERROR
00542            MOVE ER-0021          TO EMI-ERROR
00543            MOVE -1               TO USENTL
00544            MOVE AL-UABON         TO USENTA
00545            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00546           ELSE
00547            MOVE DC-BIN-DATE-1    TO USENT-SAVE
00548            MOVE DEEDIT-FIELD-V0  TO USENTO
00549            INSPECT USENTI CONVERTING ' ' TO '/'
00550         ELSE
00551            MOVE LOW-VALUES       TO USENT-SAVE.
00552
00553      IF USENT-SAVE   NOT = LOW-VALUES AND
00554         URECDTE-SAVE NOT = LOW-VALUES
00555           IF USENT-SAVE GREATER THAN URECDTE-SAVE
00556              MOVE ER-0514        TO EMI-ERROR
00557              MOVE -1             TO URECDTEL
00558              MOVE AL-UABON       TO URECDTEA
00559              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00560
00561      IF URECDTE-SAVE NOT = LOW-VALUES
00562           IF URECDTE-SAVE GREATER THAN BIN-CURRENT-DATE
00563              MOVE ER-0539        TO EMI-ERROR
00564              MOVE -1             TO URECDTEL
00565              MOVE AL-UABON       TO URECDTEA
00566              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00567
00568      IF URECDTEL = ZEROS
00569         IF USENTL > ZEROS  OR
051107*00570            UBYL   > ZEROS  OR
00571            UFORML > ZEROS  OR
00572            UREL   > ZEROS
00573               MOVE ER-0321          TO EMI-ERROR
00574               MOVE -1               TO URECDTEL
00575               PERFORM 9900-ERROR-FORMAT THRU  9900-EXIT.
062217     IF AUTHRCVL > 0
062217        IF AUTHRCVI = 'N' OR 'Y'
062217           CONTINUE
062217        ELSE
062217           MOVE ER-0287        TO EMI-ERROR
062217           MOVE -1             TO AUTHRCVL
062217           MOVE AL-UABON       TO AUTHRCVA
062217           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062217        END-IF
062217     END-IF.
00576
00577      IF NOT EMI-NO-ERRORS
00578         GO TO 8200-SEND-DATAONLY.
00579
00580      IF NOT MODIFY-CAP
00581          MOVE ER-0070            TO EMI-ERROR
00582          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00583          MOVE LOW-VALUES TO EL162AO
00584          GO TO 8100-SEND-INITIAL-MAP.
00585
00586      EJECT
00587      PERFORM 3000-BUILD-KEYS.
00588
00589      IF REC-DATA
00590         PERFORM 4000-UPDATE-RECEIVE-DATES THRU 4099-EXIT
00591                 VARYING SC-INDX FROM 1 BY 1 UNTIL
00592                 SC-INDX = 5.
00593
00594      PERFORM 4100-CREATE-NEW-TRLR THRU 4199-EXIT.
00600
00601      MOVE ER-ZEROS               TO EMI-ERROR.
00602      MOVE SPACES                 TO ADDR1I ADDR2I ADDR3I ADDR4I
00603                                     ZIPI PHONEI.
00604
00605      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00606
00607      MOVE -1                     TO SC-RECL (1).
00608      GO TO 8200-SEND-DATAONLY.
00609
00610      EJECT
00611  2000-ZERO-PI-TRLR-TABLE.
00612
00613      MOVE +0                     TO PI-TRLRS (SUB).
00614      ADD +1 TO SUB.
00615
00616      IF SUB > +50
00617         MOVE +0                  TO SUB
022619        move 0                   to pi-receive-dt-cnt
022619        move low-values          to pi-prev-rec-dt
00618         GO TO 2010-EXIT.
00619
00620      GO TO 2000-ZERO-PI-TRLR-TABLE.
00621
00622  2010-EXIT.
00623      EXIT.
022619 2200-edit-receive-date.
022619***  The ELMSTR has been read at this point and each of the
022619***  received dates will be edited here.
022619
022619     move sc-trlrno(sc-indx)     to pi-actv-seq
022619
022619     
      * EXEC CICS READ
022619*       DATASET (ELTRLR-FILE-ID)
022619*       RIDFLD  (PI-ACTV-KEY)
022619*       SET     (ADDRESS OF ACTIVITY-TRAILERS)
022619*       resp    (ws-response)
022619*    END-EXEC
      *    MOVE '&"S        E          (  N#00003514' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033353134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
022619
022619     if not resp-normal
022619        display 'error eltrlr read ' ws-response
022619        go to 2200-exit
022619     end-if
022619     if not correspondence-tr
022619        display ' is not corres trailer '
022619        go to 2200-exit
022619     end-if
022619
022619     .
022619 2200-exit.
022619     exit.
00625  3000-BUILD-KEYS.
00626      MOVE PI-CLAIM-NO            TO PI-CLAM-CLAIM
00627                                     PI-ACTV-CLAIM
00628                                     CLAIMNOI.
00629
00630      MOVE PI-CERT-NO             TO PI-CLAM-CERT-NO
00631                                     PI-ACTV-CERT-NO
00632                                     CERTNOI.
00633
00634      MOVE PI-COMPANY-CD          TO PI-CLAM-COMP-CD
00635                                     PI-ACTV-COMP-CD.
00636
00637      MOVE PI-CARRIER             TO PI-CLAM-CARRIER
00638                                     PI-ACTV-CARRIER
00639                                     CARRI.
00640      EJECT
00641  4000-UPDATE-RECEIVE-DATES.
00642
102610     IF SC-RECL (SC-INDX) = ZERO AND SC-STOPL (SC-INDX)
00644         GO TO 4099-EXIT.
00645
00646      MOVE SC-TRLRNO (SC-INDX)    TO PI-ACTV-SEQ.
00647
102610*00648      IF SC-RECL (SC-INDX) = ZERO
102610*00649          EXEC CICS READ
102610*00650               DATASET (ELTRLR-FILE-ID)
102610*00651               RIDFLD  (PI-ACTV-KEY)
102610*00652               SET     (ADDRESS OF ACTIVITY-TRAILERS)
102610*00653          END-EXEC
102610*00654         ELSE
00655          
      * EXEC CICS READ
00656 *             DATASET(ELTRLR-FILE-ID)
00657 *             RIDFLD (PI-ACTV-KEY)
00658 *             SET    (ADDRESS OF ACTIVITY-TRAILERS)
00659 *             UPDATE
00660 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00003563' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00661
102610     SET INDX2 TO SC-INDX.
102610
102610     IF SC-STOPL (SC-INDX) = ZEROS
102610         GO TO 4000-BYPASS-STOP-DATE
102610     END-IF.
102610
102610     IF (CORRESPONDENCE-TR AND
102610         STOP-DATE-SAVE (INDX2) LESS THAN AT-LETTER-SENT-DT)
102610              OR
102610        (FORM-CONTROL-TR AND
102610         STOP-DATE-SAVE (INDX2) LESS THAN AT-FORM-SEND-ON-DT)
102610           MOVE ER-0896          TO EMI-ERROR
102610           MOVE -1               TO SC-STOPL (SC-INDX)
102610           MOVE AL-UABON         TO SC-STOPA (SC-INDX)
102610           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
102610           
      * EXEC CICS SYNCPOINT
102610*               ROLLBACK
102610*          END-EXEC
      *    MOVE '6"R                   !   #00003585' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102610           GO TO 8200-SEND-DATAONLY
102610     END-IF.
102610
102610     IF CORRESPONDENCE-TR
102610        MOVE STOP-DATE-SAVE (INDX2) TO AT-STOP-LETTER-DT
102610        MOVE PI-PROCESSOR-ID     TO AT-CORR-LAST-UPDATED-BY
102610        MOVE BIN-CURRENT-DATE    TO AT-CORR-LAST-MAINT-DT
102610     END-IF.
102610
102610     IF FORM-CONTROL-TR
102610        MOVE STOP-DATE-SAVE (INDX2) TO AT-STOP-FORM-DT
102610        MOVE PI-PROCESSOR-ID     TO AT-FORM-LAST-UPDATED-BY
102610        MOVE BIN-CURRENT-DATE    TO AT-FORM-LAST-MAINT-DT
102610     END-IF.
102610     MOVE AL-UANOF               TO SC-STOPA (SC-INDX).
102610
102610 4000-BYPASS-STOP-DATE.
00671
00672      IF SC-RECL (SC-INDX) = ZEROS
102610*00673         GO TO 4099-EXIT.
102610        GO TO 4090-REWRITE
102610     END-IF.
00674
00675      SET INDX TO SC-INDX.
00676
00677      IF (CORRESPONDENCE-TR AND
00678          REC-DATE-SAVE (INDX)  LESS THAN AT-LETTER-SENT-DT)
00679               OR
00680         (FORM-CONTROL-TR AND
00681          REC-DATE-SAVE (INDX)  LESS THAN AT-FORM-SEND-ON-DT)
00682            MOVE ER-0514          TO EMI-ERROR
00683            MOVE -1               TO SC-RECL (SC-INDX)
00684            MOVE AL-UABON         TO SC-RECA (SC-INDX)
00685            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00686            
      * EXEC CICS SYNCPOINT
00687 *               ROLLBACK
00688 *          END-EXEC
      *    MOVE '6"R                   !   #00003622' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00689            GO TO 8200-SEND-DATAONLY.
00690
00691      IF CORRESPONDENCE-TR
00692         MOVE REC-DATE-SAVE (INDX)   TO AT-LETTER-ANSWERED-DT
00693         MOVE PI-PROCESSOR-ID        TO AT-CORR-LAST-UPDATED-BY
00694         MOVE BIN-CURRENT-DATE       TO AT-CORR-LAST-MAINT-DT.
00695
00696      IF FORM-CONTROL-TR
00697         MOVE PI-PROCESSOR-ID        TO AT-FORM-LAST-UPDATED-BY
00698         MOVE BIN-CURRENT-DATE       TO AT-FORM-LAST-MAINT-DT
00699         IF SC-TYPE (SC-INDX) = 'C'
00700            MOVE REC-DATE-SAVE (INDX)   TO AT-FORM-ANSWERED-DT
00701         ELSE
00702         IF SC-TYPE (SC-INDX) = 'E'
00703            MOVE REC-DATE-SAVE (INDX)   TO AT-EMP-FORM-ANSWERED-DT
00704         ELSE
00705         IF SC-TYPE (SC-INDX) = 'P'
00706            MOVE REC-DATE-SAVE (INDX)   TO AT-PHY-FORM-ANSWERED-DT.
00707
00708      MOVE AL-UANOF               TO SC-RECA (SC-INDX).
00709
102610 4090-REWRITE.
102610
00710      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
00711
00712      IF PI-COMPANY-ID = 'DMD'
00713          MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY.
00714
00715      
      * EXEC CICS REWRITE
00716 *         FROM   (ACTIVITY-TRAILERS)
00717 *         DATASET(ELTRLR-FILE-ID)
00718 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003653' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00719
00720  4099-EXIT.
00721       EXIT.
00722
00723      EJECT
00724  4100-CREATE-NEW-TRLR.
00725
00726      
      * EXEC CICS READ
00727 *         DATASET(ELMSTR-FILE-ID)
00728 *         SET    (ADDRESS OF CLAIM-MASTER)
00729 *         RIDFLD (PI-CLAM-KEY)
00730 *         UPDATE
00731 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003664' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00732
00733      IF FILEL > ZEROS
00734         MOVE FILEI               TO CL-FILE-LOCATION.
00735
00736      IF NOT UNSOL-DATA
00737         GO TO 4150-REWRITE-CLAIM.
00738
00739      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
00740
00741      
      * EXEC CICS GETMAIN
00742 *         SET    (ADDRESS OF ACTIVITY-TRAILERS)
00743 *         INITIMG(GETMAIN-SPACE)
00744 *         LENGTH (ACTV-LENGTH)
00745 *    END-EXEC.
      *    MOVE ',"IL                  $   #00003679' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACTV-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00746
00747      MOVE 'AT'                   TO AT-RECORD-ID.
00748      MOVE  4                     TO AT-TRAILER-TYPE.
00749      MOVE BIN-CURRENT-DATE       TO AT-RECORDED-DT
00750                                     AT-CORR-LAST-MAINT-DT
00751
00752      IF UBYL > +0
00753         MOVE UBYI                TO AT-RECORDED-BY
00754                                     AT-CORR-LAST-UPDATED-BY
00755      ELSE
00756         MOVE PI-PROCESSOR-ID     TO AT-RECORDED-BY
00757                                     AT-CORR-LAST-UPDATED-BY.
00758
00759      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
00760      MOVE PI-ACTV-KEY            TO AT-CONTROL-PRIMARY.
00761      MOVE PI-CLAIM-NO            TO AT-CLAIM-NO.
00762      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.
00763      MOVE USENT-SAVE             TO AT-LETTER-SENT-DT.
00764      MOVE LOW-VALUES             TO AT-RECEIPT-FOLLOW-UP
00765                                     AT-AUTO-RE-SEND-DT
00766                                     AT-INITIAL-PRINT-DATE
00767                                     AT-RESEND-PRINT-DATE.
00768      MOVE URECDTE-SAVE           TO AT-LETTER-ANSWERED-DT.
00769      MOVE ZEROS                  TO AT-LETTER-ARCHIVE-NO.
00770      MOVE '1'                    TO AT-LETTER-ORIGIN.
00771
00772      IF UFORML > 0
00773         MOVE UFORMI              TO AT-STD-LETTER-FORM
00774        ELSE
00775         MOVE SPACES              TO AT-STD-LETTER-FORM.
00776
00777      IF UREL > 0
00778         MOVE UREI                TO AT-REASON-TEXT
00779        ELSE
00780         MOVE SPACES              TO AT-REASON-TEXT.
062217     IF AUTHRCVL > 0
062217        MOVE AUTHRCVI            TO AT-AUTH-RCVD
062217     ELSE
062217        MOVE SPACES              TO AT-AUTH-RCVD
062217     END-IF.
00781
00782      MOVE SPACES                 TO AT-ADDRESSEE-NAME
00783                                     AT-ADDRESEE-TYPE.
00784      MOVE ZEROS                  TO AT-ADDRESS-REC-SEQ-NO.
00785      MOVE 'U'                    TO AT-CORR-SOL-UNSOL.
00786
00787      
      * EXEC CICS WRITE
00788 *         DATASET(ELTRLR-FILE-ID)
00789 *         FROM   (ACTIVITY-TRAILERS)
00790 *         RIDFLD (AT-CONTROL-PRIMARY)
00791 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003730' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00792
00793      MOVE SPACES                 TO URECDTEI
00794                                     USENTI
051107*00795                                     UBYI
00796                                     UFORMI
00797                                     UREI.
00798
00799      MOVE AL-UANOF               TO URECDTEA
00800                                     USENTA
051107*00801                                     UBYA
00802                                     UFORMA
00803                                     UREA.
051107
051107     MOVE PI-PROCESSOR-ID        TO UBYI.
00804
00805  4150-REWRITE-CLAIM.
00806      
      * EXEC CICS HANDLE CONDITION
00807 *        DUPKEY (4199-EXIT)
00808 *    END-EXEC.
      *    MOVE '"$$                   ! # #00003751' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033373531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00809
00810      MOVE PI-PROCESSOR-ID   TO  CL-LAST-MAINT-USER.
00811      MOVE EIBTIME           TO  CL-LAST-MAINT-HHMMSS.
00812      MOVE BIN-CURRENT-DATE  TO  CL-LAST-MAINT-DT.
00813      MOVE '2'               TO  CL-LAST-MAINT-TYPE.
00814
00815      IF PI-COMPANY-ID = 'DMD'
00816          MOVE 11                 TO CL-ACTIVITY-CODE
00817          MOVE BIN-CURRENT-DATE   TO CL-ACTIVITY-MAINT-DT
00818          MOVE 'CORR'             TO CL-ACTIVITY-MAINT-TYPE
00819          MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID.
00820
00821      
      * EXEC CICS REWRITE
00822 *         DATASET(ELMSTR-FILE-ID)
00823 *         FROM   (CLAIM-MASTER)
00824 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003766' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00825
00826  4199-EXIT.
00827       EXIT.
00828
00829      EJECT
00830  4200-LOCATE-ADDRESS.
00831      IF ADDR-TYPE = '3' AND ADDR-SEQ = ZEROS
00832         GO TO 4210-READ-ACCOUNT.
00833
00834      IF ADDR-TYPE = SPACES OR ADDR-SEQ = ZEROS
00835         GO TO 4290-ERROR.
00836
00837      MOVE ADDR-SEQ               TO PI-ACTV-SEQ.
00838
00839      
      * EXEC CICS HANDLE CONDITION
00840 *         NOTFND(4290-ERROR)
00841 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00003784' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033373834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00842
00843      
      * EXEC CICS READ
00844 *         DATASET(ELTRLR-FILE-ID)
00845 *         RIDFLD (PI-ACTV-KEY)
00846 *         SET    (ADDRESS OF ACTIVITY-TRAILERS)
00847 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003788' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00848
00849      MOVE AT-MAIL-TO-NAME        TO ADDR1O.
00850      MOVE AT-ADDRESS-LINE-1      TO ADDR2O.
00851      MOVE AT-ADDRESS-LINE-2      TO ADDR3O.
00852 *    MOVE AT-CITY-STATE          TO ADDR4O.
           STRING AT-CITY ' ' AT-STATE
              DELIMITED BY '  ' INTO ADDR4O
           END-STRING
00853      MOVE AT-PHONE-NO            TO PHONEO.
00854      INSPECT PHONEI CONVERTING ' ' TO '-'.
00855
00856      IF AT-CANADIAN-POST-CODE
00857          MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
00858          MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
00859          MOVE SPACES             TO WS-DASH-CAN
00860                                     WS-CAN-FILLER
00861          MOVE WS-CANADIAN-POSTAL-CODES
00862                                  TO ZIPO
00863      ELSE
00864          MOVE AT-ZIP-CODE        TO WS-ZIP-CODE
00865          IF AT-ZIP-PLUS4 = SPACES OR ZEROS
00866              MOVE SPACES         TO WS-ZIP-PLUS4
00867                                     WS-DASH
00868              MOVE WS-ZIP         TO ZIPO
00869          ELSE
00870              MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4
00871              MOVE '-'            TO WS-DASH
00872              MOVE WS-ZIP         TO ZIPO.
00873
00874      GO TO 4299-EXIT.
00875
00876  4210-READ-ACCOUNT.
00877      MOVE PI-COMPANY-CD          TO ACCT-CO.
00878      MOVE PI-CARRIER             TO ACCT-CARRIER.
00879      MOVE PI-GROUPING            TO ACCT-GROUPING.
00880      MOVE PI-STATE               TO ACCT-STATE.
00881      MOVE PI-ACCOUNT             TO ACCT-ACCOUNT.
00882      MOVE PI-CERT-EFF-DT         TO ACCT-EXP-DATE.
00883
00884      
      * EXEC CICS HANDLE CONDITION
00885 *         NOTOPEN(8880-ACCT-NOT-OPEN)
00886 *         NOTFND (4250-ACCT-NOT-FOUND)
00887 *    END-EXEC.
      *    MOVE '"$JI                  ! % #00003832' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033383332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00888
00889      
      * EXEC CICS STARTBR
00890 *         RIDFLD   (ACCT-KEY)
00891 *         DATASET  (ERACCT-FILE-ID)
00892 *         KEYLENGTH(13)
00893 *         GENERIC
00894 *    END-EXEC.
           MOVE 13
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00003837' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 ACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00895
00896      MOVE ACCT-PARTIAL-KEY       TO ACCT-SAVE-KEY.
00897
00898  4230-READNEXT.
00899      
      * EXEC CICS READNEXT
00900 *         DATASET(ERACCT-FILE-ID)
00901 *         SET    (ADDRESS OF ACCOUNT-MASTER)
00902 *         RIDFLD (ACCT-KEY)
00903 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003847' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00904
00905      IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY
00906         GO TO 4250-ACCT-NOT-FOUND.
00907
00908      IF PI-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT AND
00909         PI-CERT-EFF-DT     LESS THAN AM-EXPIRATION-DT
00910         NEXT SENTENCE
00911       ELSE
00912         GO TO 4230-READNEXT.
00913
00914      MOVE AM-NAME                TO ADDR1O.
00915      MOVE AM-PERSON              TO ADDR2O.
00916      MOVE AM-ADDRS               TO ADDR3O.
00917 *    MOVE AM-CITY                TO ADDR4O.
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
              DELIMITED BY '  ' INTO ADDR4O
           END-STRING
00918
00919      MOVE AM-TEL-NO              TO WS-WORK-PHONE.
00920      INSPECT WS-WORK-PHONE CONVERTING ' ' TO '0'.
00921      MOVE WS-NUMERIC-PHONE       TO PHONEO.
00922      INSPECT PHONEI CONVERTING ' ' TO '-'.
00923
00924      IF AM-CANADIAN-POST-CODE
00925          MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
00926          MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
00927          MOVE SPACES             TO WS-DASH-CAN
00928                                     WS-CAN-FILLER
00929          MOVE WS-CANADIAN-POSTAL-CODES
00930                                  TO ZIPO
00931      ELSE
00932          MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE
00933          IF AM-ZIP-PLUS4 = SPACES OR ZEROS
00934              MOVE SPACES         TO WS-ZIP-PLUS4
00935                                     WS-DASH
00936              MOVE WS-ZIP         TO ZIPO
00937          ELSE
00938              MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4
00939              MOVE '-'            TO WS-DASH
00940              MOVE WS-ZIP         TO ZIPO.
00941
00942      PERFORM 4240-ENDBR.
00943
00944      GO TO 4299-EXIT.
00945
00946  4240-ENDBR.
00947      
      * EXEC CICS ENDBR
00948 *         DATASET(ERACCT-FILE-ID)
00949 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003898' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00950
00951  4250-ACCT-NOT-FOUND.
00952      PERFORM 4240-ENDBR.
00953      MOVE ER-0179                TO EMI-ERROR.
00954      MOVE -1                     TO SC-RECL (1).
00955      MOVE SPACES                 TO ADDR1I ADDR2I ADDR3I ADDR4I
00956                                     ZIPI PHONEI.
00957
00958      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00959
00960  4290-ERROR.
00961      MOVE ER-0322                TO EMI-ERROR.
00962      MOVE -1                     TO SC-RECL (1).
00963      MOVE SPACES                 TO ADDR1I ADDR2I ADDR3I ADDR4I
00964                                     ZIPI PHONEI.
00965
00966      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00967
00968  4299-EXIT.
00969       EXIT.
00970
00971      EJECT
00972  6000-BROWSE-FORWARD.
00973      IF PI-ACTV-SAVE NOT = PI-ACTV-PARTIAL
00974         MOVE ER-0066             TO EMI-ERROR
00975         MOVE -1                  TO SC-RECL (1)
00976         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00977         GO TO 8200-SEND-DATAONLY.
00978
00979      MOVE LOW-VALUES             TO EL162AI.
00980      ADD 1 TO PI-ACTV-SEQ.
00981
00982      GO TO 7000-BUILD-SCREEN.
00983
00984  6100-BROWSE-BACKWARD.
00985
00986      IF PI-NEXT-TRLR-SUB > 8
00987         SUBTRACT 4 FROM PI-NEXT-TRLR-SUB.
00988
00989      MOVE PI-NEXT-TRLR-SUB       TO SUB.
00990
00991      IF SUB = 1 OR 5
00992         MOVE ER-0067             TO EMI-ERROR
00993         MOVE -1                  TO SC-RECL (1)
00994         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00995         MOVE  1                  TO SUB
00996         MOVE  +1                 TO PI-NEXT-TRLR-SUB
00997      ELSE
00998         SUBTRACT 4 FROM SUB
00999         MOVE SUB                 TO PI-NEXT-TRLR-SUB.
01000
01001      MOVE LOW-VALUES             TO EL162AI
01002                                     TRL-LINES.
01003      MOVE PI-ACTV-SAVE           TO PI-ACTV-PARTIAL.
01004
01005      MOVE PI-TRLRS (SUB)         TO PI-ACTV-SEQ.
01006
01007      GO TO 7000-BUILD-SCREEN.
01008
01009      EJECT
01010  6200-FORMAT-DATA.
01011
01012      IF FORM-CONTROL-TR
01013         GO TO 6250-FORMAT-FORM.
01014
01015      IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES
01016         MOVE AT-LETTER-ANSWERED-DT TO DC-BIN-DATE-1
01017         MOVE SPACES                TO DC-OPTION-CODE
01018         PERFORM 9700-DATE-LINK
01019         MOVE DC-GREG-DATE-1-EDIT   TO SC-REC (SC-INDX).
102610
102610     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
102610        MOVE AT-STOP-LETTER-DT     TO DC-BIN-DATE-1
102610        MOVE SPACES                TO DC-OPTION-CODE
102610        PERFORM 9700-DATE-LINK
102610        MOVE DC-GREG-DATE-1-EDIT   TO SC-STOP (SC-INDX)
102610     END-IF.
01020
01021      IF AT-LETTER-SENT-DT NOT = LOW-VALUES
01022         MOVE AT-LETTER-SENT-DT   TO DC-BIN-DATE-1
01023         MOVE SPACES              TO DC-OPTION-CODE
01024         PERFORM 9700-DATE-LINK
01025         MOVE DC-GREG-DATE-1-EDIT TO SC-SENT (SC-INDX).
01026
01027      IF AT-AUTO-RE-SEND-DT NOT = LOW-VALUES
01028         MOVE AT-AUTO-RE-SEND-DT  TO DC-BIN-DATE-1
01029         MOVE SPACES              TO DC-OPTION-CODE
01030         PERFORM 9700-DATE-LINK
01031         MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).
01032
01033      IF AT-RECEIPT-FOLLOW-UP NOT = LOW-VALUES
01034         MOVE AT-RECEIPT-FOLLOW-UP TO DC-BIN-DATE-1
01035         MOVE SPACES               TO DC-OPTION-CODE
01036         PERFORM 9700-DATE-LINK
01037         MOVE DC-GREG-DATE-1-EDIT  TO SC-FOLLOWUP (SC-INDX).
01038
01039      MOVE AT-STD-LETTER-FORM     TO SC-FORM (SC-INDX).
01040
01041      IF INSURED-ADDRESEE
01042         MOVE 'INSURED'           TO SC-TO (SC-INDX)
01043      ELSE
01044      IF BENEFICIARY-ADDRESEE
01045         MOVE 'BENEFI '           TO SC-TO (SC-INDX)
01046      ELSE
01047      IF ACCOUNT-ADDRESEE
01048         MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)
01049      ELSE
01050      IF PHYSICIAN-ADDRESEE
01051         MOVE 'DOCTOR'            TO SC-TO (SC-INDX)
01052      ELSE
01053      IF EMPLOYER-ADDRESEE
01054         MOVE 'EMPLOYER'          TO SC-TO (SC-INDX)
01055      ELSE
01056      IF OTHER-ADDRESEE-1
01057         MOVE 'OTHER-1'           TO SC-TO (SC-INDX)
01058      ELSE
01059         MOVE 'OTHER-2'           TO SC-TO (SC-INDX).
01060
01061      IF AT-LETTER-ARCHIVE-NO NOT = +0
01062         MOVE AT-LETTER-ARCHIVE-NO TO SC-ARCHIVE (SC-INDX)
01063      ELSE
01064         MOVE SPACES              TO SC-ARCHIVE (SC-INDX).
01065
01066      MOVE 'L'                    TO SC-TYPE   (SC-INDX).
01067
01068      MOVE AT-REASON-TEXT         TO SC-REASON (SC-INDX).
01069
CIDMOD*    IF PI-COMPANY-ID = 'DMD'
CIDMOD*       IF AT-DMD-LETTER-PURGED
CIDMOD*          MOVE AT-REASON-TEXT          TO WS-REASON
CIDMOD*          MOVE AT-DMD-LETTER-PURGE-DT  TO DC-BIN-DATE-1
CIDMOD*          MOVE SPACES                  TO DC-OPTION-CODE
CIDMOD*          PERFORM 9700-DATE-LINK
CIDMOD*          MOVE 'PURGED -'              TO WS-REASON (51:9)
CIDMOD*          MOVE DC-GREG-DATE-1-EDIT     TO WS-REASON (60:11)
CIDMOD*          MOVE WS-REASON               TO SC-REASON (SC-INDX).
CIDMOD*
01080      MOVE AT-RECORDED-BY         TO SC-BY     (SC-INDX).
01081      MOVE AT-SEQUENCE-NO         TO SC-TRLRNO (SC-INDX).
01082      MOVE AL-UANOF               TO SC-RECA   (SC-INDX)
102610                                    SC-STOPA  (SC-INDX).
01084
01085      GO TO 6299-EXIT.
01086
01087  6250-FORMAT-FORM.
102610
102610     IF AT-STOP-FORM-DT NOT = LOW-VALUES AND SPACES
102610        MOVE AT-STOP-FORM-DT       TO DC-BIN-DATE-1
102610        MOVE SPACES                TO DC-OPTION-CODE
102610        PERFORM 9700-DATE-LINK
102610        MOVE DC-GREG-DATE-1-EDIT   TO SC-STOP (SC-INDX)
102610     END-IF.
01088
01089      IF AT-FORM-SEND-ON-DT NOT = LOW-VALUES
01090         IF (AT-FORM-ANSWERED-DT = LOW-VALUES)
01091                          OR
01092            (AT-FORM-ANSWERED-DT NOT = LOW-VALUES  AND
01093             AT-FORM-ANSWERED-DT = BIN-CURRENT-DATE)
01094              NEXT SENTENCE
01095          ELSE
01096              GO TO 6260-CHECK-EMPLOYER-FORM
01097      ELSE
01098         GO TO 6260-CHECK-EMPLOYER-FORM.
01099
01100      IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES
01101         MOVE AT-FORM-ANSWERED-DT TO DC-BIN-DATE-1
01102         MOVE SPACES              TO DC-OPTION-CODE
01103         PERFORM 9700-DATE-LINK
01104         MOVE DC-GREG-DATE-1-EDIT TO SC-REC (SC-INDX).
01105
01106      IF AT-FORM-SEND-ON-DT NOT = LOW-VALUES
01107         MOVE AT-FORM-SEND-ON-DT  TO DC-BIN-DATE-1
01108         MOVE SPACES              TO DC-OPTION-CODE
01109         PERFORM 9700-DATE-LINK
01110         MOVE DC-GREG-DATE-1-EDIT TO SC-SENT (SC-INDX).
01111
01112      IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES
01113         MOVE AT-FORM-RE-SEND-DT  TO DC-BIN-DATE-1
01114         MOVE SPACES              TO DC-OPTION-CODE
01115         PERFORM 9700-DATE-LINK
01116         MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).
01117
01118      IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES
01119         MOVE AT-FORM-FOLLOW-UP-DT   TO DC-BIN-DATE-1
01120         MOVE SPACES                 TO DC-OPTION-CODE
01121         PERFORM 9700-DATE-LINK
01122         MOVE DC-GREG-DATE-1-EDIT TO SC-FOLLOWUP (SC-INDX).
01123
01124      IF AT-FORM-TYPE = '1'
01125         MOVE 'INIT'              TO SC-FORM (SC-INDX)
01126      ELSE
01127         MOVE 'PROG'              TO SC-FORM (SC-INDX).
01128
01129      IF FORM-TO-INSURED
01130         MOVE 'INSURED'           TO SC-TO (SC-INDX)
01131      ELSE
01132      IF FORM-TO-ACCOUNT
01133         MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)
01134      ELSE
01135      IF FORM-TO-OTHER-1
01136         MOVE 'OTHER-1'           TO SC-TO (SC-INDX)
01137      ELSE
01138      IF FORM-TO-OTHER-2
01139         MOVE 'OTHER-2'           TO SC-TO (SC-INDX).
01140
01141      MOVE 'CLAIMANT'             TO SC-ARCHIVE (SC-INDX).
01142      MOVE 'C'                    TO SC-TYPE    (SC-INDX).
01143      MOVE AT-SEQUENCE-NO         TO PI-TRLRS (PI-NEXT-TRLR-SUB)
01144      MOVE AT-INSTRUCT-LN-1       TO SC-REASON  (SC-INDX).
01145      MOVE AT-RECORDED-BY         TO SC-BY      (SC-INDX).
01146      MOVE AT-SEQUENCE-NO         TO SC-TRLRNO  (SC-INDX).
01147      MOVE AL-UANOF               TO SC-RECA    (SC-INDX).
102610     MOVE AL-UANOF               TO SC-STOPA   (SC-INDX).
01149
01150  6260-CHECK-EMPLOYER-FORM.
01151
01152      IF AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES
01153         IF (AT-EMP-FORM-ANSWERED-DT = LOW-VALUES)
01154                           OR
01155            (AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES AND
01156             AT-EMP-FORM-ANSWERED-DT = BIN-CURRENT-DATE)
01157               NEXT SENTENCE
01158           ELSE
01159               GO TO 6280-CHECK-PHYSICIAN-FORM
01160      ELSE
01161         GO TO 6280-CHECK-PHYSICIAN-FORM.
01162
01163      IF SC-TYPE (SC-INDX) = 'C'
01164         SET SC-INDX UP BY 1
01165         ADD +1 TO PI-NEXT-TRLR-SUB.
01166
01167      IF AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES
01168         MOVE AT-EMP-FORM-ANSWERED-DT TO DC-BIN-DATE-1
01169         MOVE SPACES                  TO DC-OPTION-CODE
01170         PERFORM 9700-DATE-LINK
01171         MOVE DC-GREG-DATE-1-EDIT     TO SC-REC (SC-INDX).
01172
01173      IF AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES
01174         MOVE AT-EMP-FORM-SEND-ON-DT TO DC-BIN-DATE-1
01175         MOVE SPACES                 TO DC-OPTION-CODE
01176         PERFORM 9700-DATE-LINK
01177         MOVE DC-GREG-DATE-1-EDIT    TO SC-SENT (SC-INDX).
01178
01179      IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES
01180         MOVE AT-FORM-RE-SEND-DT  TO DC-BIN-DATE-1
01181         MOVE SPACES              TO DC-OPTION-CODE
01182         PERFORM 9700-DATE-LINK
01183         MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).
01184
01185      IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES
01186         MOVE AT-FORM-FOLLOW-UP-DT   TO DC-BIN-DATE-1
01187         MOVE SPACES                 TO DC-OPTION-CODE
01188         PERFORM 9700-DATE-LINK
01189         MOVE DC-GREG-DATE-1-EDIT    TO SC-FOLLOWUP (SC-INDX).
01190
01191      IF AT-FORM-TYPE = '1'
01192         MOVE 'INIT'              TO SC-FORM (SC-INDX)
01193      ELSE
01194         MOVE 'PROG'              TO SC-FORM (SC-INDX).
01195
01196      IF FORM-TO-INSURED
01197         MOVE 'INSURED'           TO SC-TO (SC-INDX)
01198      ELSE
01199      IF FORM-TO-ACCOUNT
01200         MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)
01201      ELSE
01202      IF FORM-TO-OTHER-1
01203         MOVE 'OTHER-1'           TO SC-TO (SC-INDX)
01204      ELSE
01205      IF FORM-TO-OTHER-2
01206         MOVE 'OTHER-2'           TO SC-TO (SC-INDX).
01207
01208      MOVE 'EMPLOYER'             TO SC-ARCHIVE (SC-INDX).
01209      MOVE 'E'                    TO SC-TYPE    (SC-INDX).
01210      MOVE AT-SEQUENCE-NO         TO PI-TRLRS (PI-NEXT-TRLR-SUB)
01211      MOVE AT-INSTRUCT-LN-1       TO SC-REASON  (SC-INDX).
01212      MOVE AT-RECORDED-BY         TO SC-BY      (SC-INDX).
01213      MOVE AT-SEQUENCE-NO         TO SC-TRLRNO  (SC-INDX).
01214      MOVE AL-UANOF               TO SC-RECA    (SC-INDX)
102610                                    SC-STOPA   (SC-INDX).
01216
01217  6280-CHECK-PHYSICIAN-FORM.
01218
01219      IF AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES
01220         IF (AT-PHY-FORM-ANSWERED-DT = LOW-VALUES)
01221                          OR
01222            (AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES AND
01223             AT-PHY-FORM-ANSWERED-DT = BIN-CURRENT-DATE)
01224               NEXT SENTENCE
01225           ELSE
01226               GO TO 6299-EXIT
01227      ELSE
01228         GO TO 6299-EXIT.
01229
01230      IF SC-TYPE (SC-INDX) = 'C' OR 'E'
01231         SET SC-INDX UP BY 1
01232         ADD +1 TO PI-NEXT-TRLR-SUB.
01233
01234      IF AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES
01235         MOVE AT-PHY-FORM-ANSWERED-DT TO DC-BIN-DATE-1
01236         MOVE SPACES                  TO DC-OPTION-CODE
01237         PERFORM 9700-DATE-LINK
01238         MOVE DC-GREG-DATE-1-EDIT     TO SC-REC (SC-INDX).
01239
01240      IF AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES
01241         MOVE AT-PHY-FORM-SEND-ON-DT TO DC-BIN-DATE-1
01242         MOVE SPACES                 TO DC-OPTION-CODE
01243         PERFORM 9700-DATE-LINK
01244         MOVE DC-GREG-DATE-1-EDIT    TO SC-SENT (SC-INDX).
01245
01246      IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES
01247         MOVE AT-FORM-RE-SEND-DT  TO DC-BIN-DATE-1
01248         MOVE SPACES              TO DC-OPTION-CODE
01249         PERFORM 9700-DATE-LINK
01250         MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).
01251
01252      IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES
01253         MOVE AT-FORM-FOLLOW-UP-DT   TO DC-BIN-DATE-1
01254         MOVE SPACES                 TO DC-OPTION-CODE
01255         PERFORM 9700-DATE-LINK
01256         MOVE DC-GREG-DATE-1-EDIT    TO SC-FOLLOWUP (SC-INDX).
01257
01258      IF AT-FORM-TYPE = '1'
01259         MOVE 'INIT'              TO SC-FORM (SC-INDX)
01260      ELSE
01261         MOVE 'PROG'              TO SC-FORM (SC-INDX).
01262
01263      IF FORM-TO-INSURED
01264         MOVE 'INSURED'           TO SC-TO (SC-INDX)
01265      ELSE
01266      IF FORM-TO-ACCOUNT
01267         MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)
01268      ELSE
01269      IF FORM-TO-OTHER-1
01270         MOVE 'OTHER-1'           TO SC-TO (SC-INDX)
01271      ELSE
01272      IF FORM-TO-OTHER-2
01273         MOVE 'OTHER-2'           TO SC-TO (SC-INDX).
01274
01275      MOVE ' DOCTOR '             TO SC-ARCHIVE (SC-INDX).
01276      MOVE 'P'                    TO SC-TYPE    (SC-INDX).
01277      MOVE AT-SEQUENCE-NO         TO PI-TRLRS (PI-NEXT-TRLR-SUB)
01278      MOVE AT-INSTRUCT-LN-1       TO SC-REASON  (SC-INDX).
01279      MOVE AT-RECORDED-BY         TO SC-BY      (SC-INDX).
01280      MOVE AT-SEQUENCE-NO         TO SC-TRLRNO  (SC-INDX).
01281      MOVE AL-UANOF               TO SC-RECA    (SC-INDX)
102610                                    SC-STOPA   (SC-INDX).
01283
01284  6299-EXIT.
01285       EXIT.
01286
01287      EJECT
01288  7000-BUILD-SCREEN.
01289
01290      IF PI-CLAIM-NO = SPACES
01291         MOVE ER-0319             TO EMI-ERROR
01292         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01293         GO TO 8100-SEND-INITIAL-MAP.
01294
01295      PERFORM 3000-BUILD-KEYS.
051107
051107     MOVE PI-PROCESSOR-ID        TO UBYI.
01296
01297      
      * EXEC CICS HANDLE CONDITION
01298 *         NOTOPEN(8870-CLAIM-NOT-OPEN)
01299 *         NOTFND (8860-CLAIM-NOT-FOUND)
01300 *    END-EXEC.
      *    MOVE '"$JI                  ! & #00004264' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01301
01302      
      * EXEC CICS READ
01303 *         DATASET(ELMSTR-FILE-ID)
01304 *         RIDFLD (PI-CLAM-KEY)
01305 *         SET    (ADDRESS OF CLAIM-MASTER)
01306 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004269' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01307
121802     EVALUATE TRUE
121802     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
01309         MOVE PI-LIFE-OVERRIDE-L6 TO TYPEI
121802     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
01311         MOVE PI-AH-OVERRIDE-L6   TO TYPEI
121802     WHEN CL-CLAIM-TYPE = 'I'
121802        MOVE ' IU   '            TO TYPEI
121802     WHEN CL-CLAIM-TYPE = 'G'
121802        MOVE ' GAP  '            TO TYPEI
052614
052614     WHEN CL-CLAIM-TYPE = 'F'
052614        MOVE ' FAM  '            TO TYPEI
080322
080322     when cl-claim-type = 'B'
080322        move ' BRV  '            TO TYPEI
080322     when cl-claim-type = 'H'
080322        move ' HOSP '            TO TYPEI
100518
100518     WHEN CL-CLAIM-TYPE = 'O'
100518        MOVE ' OTH  '            TO TYPEI
121802     END-EVALUATE
01312
01313      MOVE CL-INSURED-LAST-NAME   TO LASTNMEI.
01314      MOVE CL-FILE-LOCATION       TO FILEI.
01315      MOVE PI-ACTV-PARTIAL        TO PI-ACTV-SAVE.
01316
01317      IF CL-1ST-TRL-AVAIL
01318         GO TO 7100-NOT-FOUND.
01319
01320  7010-START-BROWSE.
01321
01322      
      * EXEC CICS HANDLE CONDITION
01323 *         NOTOPEN(8820-ACTV-NOT-OPEN)
01324 *         NOTFND (7100-NOT-FOUND)
01325 *         ENDFILE(7100-NOT-FOUND)
01326 *    END-EXEC.
      *    MOVE '"$JI''                 ! '' #00004306' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034333036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01327
01328      
      * EXEC CICS STARTBR
01329 *         DATASET(ELTRLR-FILE-ID)
01330 *         RIDFLD (PI-ACTV-KEY)
01331 *         GTEQ
01332 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004312' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01333
01334      SET SC-INDX TO 1.
01335
01336  7020-READ-NEXT.
01337
01338      
      * EXEC CICS READNEXT
01339 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
01340 *         RIDFLD   (PI-ACTV-KEY)
01341 *         DATASET  (ELTRLR-FILE-ID)
01342 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004322' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01343
01344      IF PI-ACTV-PARTIAL NOT = PI-ACTV-SAVE
01345         GO TO 7100-NOT-FOUND.
01346
01347      IF AT-TRAILER-TYPE = '4' OR  'A'
01348         NEXT SENTENCE
01349      ELSE
01350         GO TO 7020-READ-NEXT.
01351
01352      IF CORRESPONDENCE-TR
01353         IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES AND
01354            AT-LETTER-ANSWERED-DT NOT = BIN-CURRENT-DATE
01355              GO TO 7020-READ-NEXT.
100610
100610     IF CORRESPONDENCE-TR AND
100610        AT-RESEND-PRINT-DATE NOT = LOW-VALUES AND
100610        AT-RESEND-LETTER-FORM NOT = LOW-VALUES
100610             GO TO 7020-READ-NEXT
100610     END-IF.
102610
102610     IF CORRESPONDENCE-TR AND
102610        AT-STOP-LETTER-DT NOT = LOW-VALUES AND
102610        AT-STOP-LETTER-DT NOT = SPACES AND
102610        AT-STOP-LETTER-DT NOT = BIN-CURRENT-DATE
102610*        AT-STOP-LETTER-DT = AT-LETTER-SENT-DT
102610             GO TO 7020-READ-NEXT
102610     END-IF.
01356
01357      IF NOT FORM-CONTROL-TR
01358         GO TO 7030-CONTINUE.
01359
01360      IF ((AT-FORM-ANSWERED-DT NOT = LOW-VALUES AND
01361           AT-FORM-ANSWERED-DT NOT = BIN-CURRENT-DATE) OR
01362           AT-FORM-SEND-ON-DT = LOW-VALUES)
01363        AND
01364         ((AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES AND
01365           AT-EMP-FORM-ANSWERED-DT NOT = BIN-CURRENT-DATE) OR
01366           AT-EMP-FORM-SEND-ON-DT = LOW-VALUES)
01367        AND
01368         ((AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES AND
01369           AT-PHY-FORM-ANSWERED-DT NOT = BIN-CURRENT-DATE) OR
01370           AT-PHY-FORM-SEND-ON-DT = LOW-VALUES)
01371              GO TO 7020-READ-NEXT.
01372
01373      IF (AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES) AND
01374         (AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES)
01375         IF SC-INDX > 2
01376            MOVE PI-ACTV-SAVE-SEQ TO PI-ACTV-SEQ
01377            GO TO 7040-END-BUILD-SCREEN.
01378
01379      IF (AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES) OR
01380         (AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES)
01381         IF SC-INDX > 3
01382            MOVE PI-ACTV-SAVE-SEQ TO PI-ACTV-SEQ
01383            GO TO 7040-END-BUILD-SCREEN.
01384
01385  7030-CONTINUE.
01386
01387      PERFORM 6200-FORMAT-DATA THRU 6299-EXIT.
01388
01389      MOVE PI-ACTV-SEQ             TO PI-ACTV-SAVE-SEQ.
01390
01391      MOVE AT-SEQUENCE-NO          TO PI-TRLRS (PI-NEXT-TRLR-SUB).
01392
01393      ADD 1   TO PI-NEXT-TRLR-SUB.
01394      SET SC-INDX UP BY 1.
01395
01396      IF SC-INDX NOT = 5
01397         GO TO 7020-READ-NEXT.
01398
01399  7040-END-BUILD-SCREEN.
01400
01401      IF SC-INDX = +2
01402         ADD +3 TO PI-NEXT-TRLR-SUB
01403      ELSE
01404      IF SC-INDX = +3
01405         ADD +2 TO PI-NEXT-TRLR-SUB
01406      ELSE
01407      IF SC-INDX = +4
01408         ADD +1 TO PI-NEXT-TRLR-SUB.
01409
01410      MOVE -1                     TO SC-RECL (1).
01411      GO TO 8100-SEND-INITIAL-MAP.
01412
01413  7100-NOT-FOUND.
01414      IF PI-NEXT-TRLR-SUB = 1
01415         MOVE ER-0320             TO EMI-ERROR
01416         MOVE -1                  TO URECDTEL
01417         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01418         GO TO 8100-SEND-INITIAL-MAP.
01419
01420      MOVE -1                     TO SC-RECL (1).
01421
01422 *    IF ANY LINES WERE PRINTED THEN
01423 *    SET THE SUB TO ALWAYS BE 1 GREATER THAN A MULTIPLE OF 4.
01424
01425      SET INDX-WORK TO SC-INDX.
01426
01427      IF SC-INDX NOT = 1
01428         COMPUTE PI-NEXT-TRLR-SUB = (4 - INDX-WORK) + 1 +
01429                                     PI-NEXT-TRLR-SUB.
01430
01431      IF EIBAID NOT = DFHPF1
01432         GO TO 8100-SEND-INITIAL-MAP.
01433
01434      MOVE ER-0066                TO EMI-ERROR.
01435      MOVE -1                     TO SC-RECL (1).
01436
01437      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01438
01439      GO TO 8100-SEND-INITIAL-MAP.
01440
01441      EJECT
01442  8100-SEND-INITIAL-MAP.
01443      MOVE SAVE-DATE    TO RUNDTEO.
01444      MOVE EIBTIME      TO TIME-IN.
01445      MOVE TIME-OUT     TO RUNTIMEO.
01446
01447      IF NOT EMI-NO-ERRORS
01448          SET EMI-INDX TO 1
01449          MOVE EMI-MESSAGE-AREA (EMI-INDX) TO ERRMSG1O.
01450
01451      IF PI-COMPANY-ID NOT = 'DMD'
01452          MOVE SPACES             TO PF4KEYO.
01453
01454      IF PI-RETURN-TO-PROGRAM = 'EL150'
01455          MOVE SPACES             TO PF5KEYO.
01456
01457      
      * EXEC CICS SEND
01458 *        MAP   (MAP-NAME)
01459 *        MAPSET(MAPSET-NAME)
01460 *        FROM  (EL162AO)
01461 *        ERASE
01462 *        CURSOR
01463 *    END-EXEC.
           MOVE LENGTH OF
            EL162AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004455' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL162AO, 
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
           
01464
01465      GO TO 9100-RETURN-TRAN.
01466
01467  8200-SEND-DATAONLY.
01468      MOVE SAVE-DATE            TO RUNDTEO.
01469      MOVE EIBTIME              TO TIME-IN.
01470      MOVE TIME-OUT             TO RUNTIMEO.
01471      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
01472
01473      IF PI-COMPANY-ID NOT = 'DMD'
01474          MOVE SPACES             TO PF4KEYO.
01475
01476      IF PI-RETURN-TO-PROGRAM = 'EL150'
01477          MOVE SPACES             TO PF5KEYO.
01478
01479      
      * EXEC CICS SEND
01480 *        MAP   (MAP-NAME)
01481 *        MAPSET(MAPSET-NAME)
01482 *        FROM  (EL162AO)
01483 *        DATAONLY
01484 *        CURSOR
01485 *    END-EXEC.
           MOVE LENGTH OF
            EL162AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004477' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL162AO, 
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
           
01486
01487      GO TO 9100-RETURN-TRAN.
01488
01489  8300-SEND-TEXT.
01490      
      * EXEC CICS SEND TEXT
01491 *        FROM  (LOGOFF-TEXT)
01492 *        LENGTH(LOGOFF-LENGTH)
01493 *        ERASE
01494 *        FREEKB
01495 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004488' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343838' TO DFHEIV0(25:11)
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
           
01496
01497      
      * EXEC CICS RETURN
01498 *    END-EXEC.
      *    MOVE '.(                    ''   #00004495' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01499
01500      EJECT
01501  8600-DEEDIT.
01502      
      * EXEC CICS BIF DEEDIT
01503 *        FIELD (DEEDIT-FIELD)
01504 *        LENGTH(15)
01505 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004500' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01506
01507  8800-UNAUTHORIZED-ACCESS.
01508      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01509      GO TO 8300-SEND-TEXT.
01510
01511  8810-PF23.
01512      MOVE EIBAID                   TO PI-ENTRY-CD-1.
01513      MOVE XCTL-005 TO PGM-NAME.
01514      GO TO 9300-XCTL.
01515
01516  8820-ACTV-NOT-OPEN.
01517      MOVE ER-0172                TO EMI-ERROR.
01518      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01519      GO TO 8200-SEND-DATAONLY.
01520
01521  8860-CLAIM-NOT-FOUND.
01522      MOVE ER-0319                TO EMI-ERROR.
01523      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01524      GO TO 8100-SEND-INITIAL-MAP.
01525
01526  8870-CLAIM-NOT-OPEN.
01527      MOVE ER-0042                TO EMI-ERROR.
01528      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01529      GO TO 8200-SEND-DATAONLY.
01530
01531  8880-ACCT-NOT-OPEN.
01532      MOVE ER-0168                TO EMI-ERROR
01533      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01534      GO TO 8200-SEND-DATAONLY.
01535
01536  9100-RETURN-TRAN.
01537      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
01538      MOVE MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.
01539      
      * EXEC CICS RETURN
01540 *        TRANSID(TRANS-ID)
01541 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
01542 *        LENGTH(PI-COMM-LENGTH)
01543 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00004537' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01544
01545  9200-RETURN-MAIN-MENU.
01546      MOVE XCTL-126 TO PGM-NAME.
01547      GO TO 9300-XCTL.
01548
01549  9300-XCTL.
01550      
      * EXEC CICS XCTL
01551 *        PROGRAM(PGM-NAME)
01552 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
01553 *        LENGTH(PI-COMM-LENGTH)
01554 *    END-EXEC.
      *    MOVE '.$C                   %   #00004548' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01555
01556  9400-CLEAR.
01557      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
01558      GO TO 9300-XCTL.
01559
01560  9500-PF12.
01561      MOVE XCTL-010 TO PGM-NAME.
01562      GO TO 9300-XCTL.
01563
01564  9600-PGMID-ERROR.
01565      
      * EXEC CICS HANDLE CONDITION
01566 *        PGMIDERR(8300-SEND-TEXT)
01567 *    END-EXEC.
      *    MOVE '"$L                   ! ( #00004563' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034353633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01568
01569      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.
01570      MOVE ' '          TO PI-ENTRY-CD-1.
01571      MOVE XCTL-005     TO PGM-NAME.
01572      MOVE PGM-NAME     TO LOGOFF-PGM.
01573      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
01574      GO TO 9300-XCTL.
01575
01576  9700-DATE-LINK.
01577      MOVE LINK-ELDATCV           TO PGM-NAME.
01578
01579      
      * EXEC CICS LINK
01580 *         PROGRAM (PGM-NAME)
01581 *         COMMAREA(DATE-CONVERSION-DATA)
01582 *         LENGTH  (DC-COMM-LENGTH)
01583 *    END-EXEC.
      *    MOVE '."C                   (   #00004577' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01584
01585      EJECT
01586  9900-ERROR-FORMAT.
01587      IF NOT EMI-ERRORS-COMPLETE
01588          MOVE LINK-001 TO PGM-NAME
01589          
      * EXEC CICS LINK
01590 *            PROGRAM (PGM-NAME)
01591 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
01592 *            LENGTH  (EMI-COMM-LENGTH)
01593 *        END-EXEC.
      *    MOVE '."C                   (   #00004587' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01594
01595  9900-EXIT.
01596      EXIT.
01597
01598  9990-ABEND.
01599      MOVE LINK-004 TO PGM-NAME.
01600      MOVE DFHEIBLK TO EMI-LINE1.
01601
01602      
      * EXEC CICS LINK
01603 *        PROGRAM (PGM-NAME)
01604 *        COMMAREA(EMI-LINE1)
01605 *        LENGTH  (72)
01606 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004600' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01607
01608      GO TO 8200-SEND-DATAONLY.
01609
01610  9995-SECURITY-VIOLATION.
01611 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00004626' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363236' TO DFHEIV0(25:11)
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
01612

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL162' TO DFHEIV1
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
               GO TO 4199-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 4290-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8880-ACCT-NOT-OPEN,
                     4250-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8870-CLAIM-NOT-OPEN,
                     8860-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8820-ACTV-NOT-OPEN,
                     7100-NOT-FOUND,
                     7100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL162' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
