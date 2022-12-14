00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL6041.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 17:00:55.
00007 *                            VMOD=2.004.
00008 *
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
00018 *                                                                *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.    TRANSACTION - EX67 - REPORT CUSTOMIZATION.
00025
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6041  WORKING STORAGE   *'.
00033  77  FILLER  PIC X(32)  VALUE '************VMOD=2.004 *********'.
00034
00035 *                                COPY ELCSCTM.
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
00036      EJECT
00037 *                                COPY ELCSCRTY.
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
00040  01  WS-DATE-AREA.
00041      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00042      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00043
00044  01  STANDARD-AREAS.
00045      12  SC-ITEM                     PIC S9(4) COMP VALUE +1.
00046      12  WS-TEST-CARRIER             PIC X       VALUE SPACES.
00047      12  WS-TEST-STATE               PIC XX      VALUE SPACES.
00048      12  WS-TEST-BUSTYP              PIC 99      VALUE ZEROS.
00049      12  WS-TEST-BEN                 PIC XX      VALUE SPACES.
00050          88  INVALID-BENEFIT-CODE       VALUE '00'
00051                                               '90' THRU '99'.
00052      12  WS-TEST-LEN                 PIC S9(4)   VALUE +0.
00053      12  WS-TEST-ATTRB               PIC X       VALUE SPACES.
00054      12  WS-SUB                      PIC S9(4)   VALUE +0 COMP.
00055      12  WS-BROWSE-SW                PIC X       VALUE SPACES.
00056      12  WS-SEQ-AREA.
00057          16  WS-SEQ-NO OCCURS 6 TIMES PIC X.
00058      12  WS-LO-EFF                   PIC XX      VALUE LOW-VALUES.
00059      12  WS-HI-EFF                   PIC XX      VALUE LOW-VALUES.
00060      12  WS-LO-ENT                   PIC XX      VALUE LOW-VALUES.
00061      12  WS-HI-ENT                   PIC XX      VALUE LOW-VALUES.
00062      12  WS-LO-LOSS                  PIC S9(3)V99 VALUE +0.
00063      12  WS-HI-LOSS                  PIC S9(3)V99 VALUE +0.
00064      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00065      12  MAP-NAME                    PIC X(8)    VALUE 'EL6041A'.
00066      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6041S'.
00067      12  SCREEN-NUMBER               PIC X(4)    VALUE '604B'.
00068      12  TRANS-ID                    PIC X(4)    VALUE 'EX67'.
00069      12  THIS-PGM                    PIC X(8)    VALUE 'EL6041'.
00070      12  PGM-NAME                    PIC X(8).
00071      12  TIME-IN                     PIC S9(7).
00072      12  TIME-OUT-R  REDEFINES TIME-IN.
00073          16  FILLER                  PIC X.
00074          16  TIME-OUT                PIC 99V99.
00075          16  FILLER                  PIC XX.
00076      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00077      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00078      12  XCTL-EL126                  PIC X(8)    VALUE 'EL126'.
00079      12  XCTL-EL626                  PIC X(8)    VALUE 'EL626'.
00080      12  XCTL-EM626                  PIC X(8)    VALUE 'EM626'.
00081      12  XCTL-GL800                  PIC X(8)    VALUE 'GL800'.
00082      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00083      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00084      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00085      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.
00086      12  WS-JOURNAL-RECORD-LENGTH PIC S9(4)   COMP VALUE +750.
00087
00088      12  DEEDIT-FIELD                PIC X(15).
00089      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00090      12  DEEDIT-FIELD-V1 REDEFINES DEEDIT-FIELD   PIC S9(14)V9.
00091      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.
00092
00093      12  RETURN-FROM                 PIC X(8).
00094      12  WS-REPORT-FOUND-SW          PIC X       VALUE 'N'.
00095          88  REPORT-WAS-FOUND                    VALUE 'Y'.
00096          88  REPORT-WAS-NOT-FOUND                VALUE 'N'.
00097
00098  01  WS-MISC-WORK.
00099      12  WS-COMBINED-LIFE-AH-OPT.
00100          16  WS-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
00101          16  WS-SINGLE-MO-PREM-PCT      PIC S9(02).
00102          16  WS-EARN-PREM-DECLINE       PIC S9(02).
00103          16  WS-CANCELLATION-RATIO      PIC S9(02).
00104          16  WS-RETENTION-LIMIT         PIC S9(07).
00105
00106      12  WS-LIFE-OPT.
00107          16  WS-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
00108          16  WS-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
00109          16  WS-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
00110          16  WS-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
00111          16  WS-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
00112          16  WS-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
00113          16  WS-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
00114          16  WS-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
00115          16  WS-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
00116          16  WS-LF-AVG-AGE-MAX          PIC S9(02).
00117
00118      12  WS-AH-OPT.
00119          16  WS-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
00120          16  WS-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
00121          16  WS-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
00122          16  WS-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
00123          16  WS-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
00124          16  WS-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
00125          16  WS-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
00126          16  WS-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
00127          16  WS-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
00128          16  WS-AH-AVG-AGE-MAX          PIC S9(02).
00129
00130      EJECT
00131  01   ERROR-MESSAGES.
00132      12  ER-0000                     PIC  X(4)   VALUE '0000'.
00133      12  ER-0004                     PIC  X(4)   VALUE '0004'.
00134      12  ER-0021                     PIC  X(4)   VALUE '0021'.
00135      12  ER-0023                     PIC  X(4)   VALUE '0023'.
00136      12  ER-0028                     PIC  X(4)   VALUE '0028'.
00137      12  ER-0029                     PIC  X(4)   VALUE '0029'.
00138      12  ER-0050                     PIC  X(4)   VALUE '0050'.
00139      12  ER-0068                     PIC  X(4)   VALUE '0068'.
00140      12  ER-0070                     PIC  X(4)   VALUE '0070'.
00141      12  ER-0132                     PIC  X(4)   VALUE '0132'.
00142      12  ER-0138                     PIC  X(4)   VALUE '0138'.
00143      12  ER-0139                     PIC  X(4)   VALUE '0139'.
00144      12  ER-0142                     PIC  X(4)   VALUE '0142'.
00145      12  ER-2001                     PIC  X(4)   VALUE '2001'.
00146      12  ER-2178                     PIC  X(4)   VALUE '2178'.
00147      12  ER-2223                     PIC  X(4)   VALUE '2223'.
00148      12  ER-2237                     PIC  X(4)   VALUE '2237'.
00149      12  ER-2238                     PIC  X(4)   VALUE '2238'.
00150      12  ER-2845                     PIC  X(4)   VALUE '2845'.
00151      12  ER-2848                     PIC  X(4)   VALUE '2848'.
00152      12  ER-7008                     PIC  X(4)   VALUE '7008'.
00153      12  ER-7123                     PIC  X(4)   VALUE '7123'.
00154      12  ER-7125                     PIC  X(4)   VALUE '7125'.
00155      12  ER-7354                     PIC  X(4)   VALUE '7354'.
00156      12  ER-7680                     PIC  X(4)   VALUE '7680'.
00157      12  ER-7681                     PIC  X(4)   VALUE '7681'.
00158      12  ER-7682                     PIC  X(4)   VALUE '7682'.
00159      12  ER-7683                     PIC  X(4)   VALUE '7683'.
00160      12  ER-7684                     PIC  X(4)   VALUE '7684'.
00161      12  ER-7685                     PIC  X(4)   VALUE '7685'.
00162      12  ER-9096                     PIC  X(4)   VALUE '9096'.
00163      12  ER-9097                     PIC  X(4)   VALUE '9097'.
00164
00165      EJECT
00166
00167  01  ACCESS-KEYS.
00168      12  ELCNTL-KEY.
00169          16  ELCNTL-COMPANY-ID          PIC  XXX.
00170          16  ELCNTL-RECORD-TYPE         PIC  X.
00171          16  FILLER                     PIC  X.
00172          16  ELCNTL-REPORT              PIC  999.
00173          16  ELCNTL-SEQ-NO              PIC  S9(4)   COMP.
00174
00175      12  ELCNTL-TEST-KEY.
00176          16  ELCNTL-TEST-COMP-ID        PIC  XXX.
00177          16  ELCNTL-TEST-REC-TYPE       PIC  X.
00178          16  ELCNTL-ACCESS-KEY.
00179              20  FILLER                 PIC  XXX.
00180              20  ELCNTL-TEST-CAR        PIC  X.
00181          16  ELCNTL-ACCESS-KEY1 REDEFINES ELCNTL-ACCESS-KEY.
00182              20  ELCNTL-TEST-STATE      PIC  XX.
00183              20  FILLER                 PIC  XX.
00184          16  ELCNTL-ACCESS-KEY2 REDEFINES ELCNTL-ACCESS-KEY.
00185              20  FILLER                 PIC  XX.
00186              20  ELCNTL-TEST-BEN        PIC  XX.
00187          16  ELCNTL-ACCESS-KEY3 REDEFINES ELCNTL-ACCESS-KEY.
00188              20  FILLER                 PIC  XX.
00189              20  ELCNTL-TEST-BUSTYP     PIC  99.
00190          16  ELCNTL-TEST-SEQ-NO         PIC  S9(4)   COMP.
00191
00192      12  ELCNTL-LENGTH           PIC S9(4) COMP VALUE +750.
00193
00194      EJECT
00195 *                                COPY ELCDATE.
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
00196      EJECT
00197 *                                COPY ELCLOGOF.
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
00198      EJECT
00199 *                                COPY ELCATTR.
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
00200      EJECT
00201 *                                COPY ELCEMIB.
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
00070      12  FILLER                      PIC X(137)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00202      EJECT
00203 *                                COPY ELCINTF.
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
00131      12  FILLER                          PIC X(4).
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
00204
00205      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00206          16  PI-LAST-REPORT        PIC 9(3).
00207          16  FILLER                PIC X(637).
00208      EJECT
00209 *                            COPY ELCJPFX.
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
00210                              PIC X(750).
00211      EJECT
00212 *                            COPY ELCAID.
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
00213
00214  01  FILLER    REDEFINES DFHAID.
00215      12  FILLER              PIC X(8).
00216      12  PF-VALUES           PIC X       OCCURS 24 TIMES.
00217
00218      EJECT
00219 *                                COPY EL6041S.
       01  EL6041AI.
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
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  REPORTL PIC S9(0004) COMP.
           05  REPORTF PIC  X(0001).
           05  FILLER REDEFINES REPORTF.
               10  REPORTA PIC  X(0001).
           05  REPORTI PIC  X(0003).
      *    -------------------------------
           05  CPYRPTL PIC S9(0004) COMP.
           05  CPYRPTF PIC  X(0001).
           05  FILLER REDEFINES CPYRPTF.
               10  CPYRPTA PIC  X(0001).
           05  CPYRPTI PIC  X(0003).
      *    -------------------------------
           05  LTONEYL PIC S9(0004) COMP.
           05  LTONEYF PIC  X(0001).
           05  FILLER REDEFINES LTONEYF.
               10  LTONEYA PIC  X(0001).
           05  LTONEYI PIC  X(0001).
      *    -------------------------------
           05  ZERMPDL PIC S9(0004) COMP.
           05  ZERMPDF PIC  X(0001).
           05  FILLER REDEFINES ZERMPDF.
               10  ZERMPDA PIC  X(0001).
           05  ZERMPDI PIC  X(0001).
      *    -------------------------------
           05  ISSCNTL PIC S9(0004) COMP.
           05  ISSCNTF PIC  X(0001).
           05  FILLER REDEFINES ISSCNTF.
               10  ISSCNTA PIC  X(0001).
           05  ISSCNTI PIC  99999.
      *    -------------------------------
           05  SMPRMPL PIC S9(0004) COMP.
           05  SMPRMPF PIC  X(0001).
           05  FILLER REDEFINES SMPRMPF.
               10  SMPRMPA PIC  X(0001).
           05  SMPRMPI PIC  99.
      *    -------------------------------
           05  EPRMDCL PIC S9(0004) COMP.
           05  EPRMDCF PIC  X(0001).
           05  FILLER REDEFINES EPRMDCF.
               10  EPRMDCA PIC  X(0001).
           05  EPRMDCI PIC  99.
      *    -------------------------------
           05  CNCRATL PIC S9(0004) COMP.
           05  CNCRATF PIC  X(0001).
           05  FILLER REDEFINES CNCRATF.
               10  CNCRATA PIC  X(0001).
           05  CNCRATI PIC  99.
      *    -------------------------------
           05  RETLIML PIC S9(0004) COMP.
           05  RETLIMF PIC  X(0001).
           05  FILLER REDEFINES RETLIMF.
               10  RETLIMA PIC  X(0001).
           05  RETLIMI PIC  9999999.
      *    -------------------------------
           05  LBENTPL PIC S9(0004) COMP.
           05  LBENTPF PIC  X(0001).
           05  FILLER REDEFINES LBENTPF.
               10  LBENTPA PIC  X(0001).
           05  LBENTPI PIC  X(0006).
      *    -------------------------------
           05  ABENTPL PIC S9(0004) COMP.
           05  ABENTPF PIC  X(0001).
           05  FILLER REDEFINES ABENTPF.
               10  ABENTPA PIC  X(0001).
           05  ABENTPI PIC  X(0006).
      *    -------------------------------
           05  LLSRATL PIC S9(0004) COMP.
           05  LLSRATF PIC  X(0001).
           05  FILLER REDEFINES LLSRATF.
               10  LLSRATA PIC  X(0001).
           05  LLSRATI PIC  999.
      *    -------------------------------
           05  LLTLRSL PIC S9(0004) COMP.
           05  LLTLRSF PIC  X(0001).
           05  FILLER REDEFINES LLTLRSF.
               10  LLTLRSA PIC  X(0001).
           05  LLTLRSI PIC  999.
      *    -------------------------------
           05  ALSRATL PIC S9(0004) COMP.
           05  ALSRATF PIC  X(0001).
           05  FILLER REDEFINES ALSRATF.
               10  ALSRATA PIC  X(0001).
           05  ALSRATI PIC  999.
      *    -------------------------------
           05  ALTLRSL PIC S9(0004) COMP.
           05  ALTLRSF PIC  X(0001).
           05  FILLER REDEFINES ALTLRSF.
               10  ALTLRSA PIC  X(0001).
           05  ALTLRSI PIC  999.
      *    -------------------------------
           05  LPDPROL PIC S9(0004) COMP.
           05  LPDPROF PIC  X(0001).
           05  FILLER REDEFINES LPDPROF.
               10  LPDPROA PIC  X(0001).
           05  LPDPROI PIC  999.
      *    -------------------------------
           05  LLTPRPL PIC S9(0004) COMP.
           05  LLTPRPF PIC  X(0001).
           05  FILLER REDEFINES LLTPRPF.
               10  LLTPRPA PIC  X(0001).
           05  LLTPRPI PIC  S9(3)V9(1).
      *    -------------------------------
           05  APDPROL PIC S9(0004) COMP.
           05  APDPROF PIC  X(0001).
           05  FILLER REDEFINES APDPROF.
               10  APDPROA PIC  X(0001).
           05  APDPROI PIC  999.
      *    -------------------------------
           05  ALTPRPL PIC S9(0004) COMP.
           05  ALTPRPF PIC  X(0001).
           05  FILLER REDEFINES ALTPRPF.
               10  ALTPRPA PIC  X(0001).
           05  ALTPRPI PIC  S9(3)V9(1).
      *    -------------------------------
           05  LLTIDCL PIC S9(0004) COMP.
           05  LLTIDCF PIC  X(0001).
           05  FILLER REDEFINES LLTIDCF.
               10  LLTIDCA PIC  X(0001).
           05  LLTIDCI PIC  S9(3)V9(1).
      *    -------------------------------
           05  ALTIDCL PIC S9(0004) COMP.
           05  ALTIDCF PIC  X(0001).
           05  FILLER REDEFINES ALTIDCF.
               10  ALTIDCA PIC  X(0001).
           05  ALTIDCI PIC  S9(3)V9(1).
      *    -------------------------------
           05  LLTTRML PIC S9(0004) COMP.
           05  LLTTRMF PIC  X(0001).
           05  FILLER REDEFINES LLTTRMF.
               10  LLTTRMA PIC  X(0001).
           05  LLTTRMI PIC  S9(3)V9(1).
      *    -------------------------------
           05  ALTTRML PIC S9(0004) COMP.
           05  ALTTRMF PIC  X(0001).
           05  FILLER REDEFINES ALTTRMF.
               10  ALTTRMA PIC  X(0001).
           05  ALTTRMI PIC  S9(3)V9(1).
      *    -------------------------------
           05  LTRMAVL PIC S9(0004) COMP.
           05  LTRMAVF PIC  X(0001).
           05  FILLER REDEFINES LTRMAVF.
               10  LTRMAVA PIC  X(0001).
           05  LTRMAVI PIC  S9(3)V9(1).
      *    -------------------------------
           05  ATRMAVL PIC S9(0004) COMP.
           05  ATRMAVF PIC  X(0001).
           05  FILLER REDEFINES ATRMAVF.
               10  ATRMAVA PIC  X(0001).
           05  ATRMAVI PIC  S9(3)V9(1).
      *    -------------------------------
           05  LLTAGPL PIC S9(0004) COMP.
           05  LLTAGPF PIC  X(0001).
           05  FILLER REDEFINES LLTAGPF.
               10  LLTAGPA PIC  X(0001).
           05  LLTAGPI PIC  S9(3)V9(1).
      *    -------------------------------
           05  ALTAGPL PIC S9(0004) COMP.
           05  ALTAGPF PIC  X(0001).
           05  FILLER REDEFINES ALTAGPF.
               10  ALTAGPA PIC  X(0001).
           05  ALTAGPI PIC  S9(3)V9(1).
      *    -------------------------------
           05  LAVAGWL PIC S9(0004) COMP.
           05  LAVAGWF PIC  X(0001).
           05  FILLER REDEFINES LAVAGWF.
               10  LAVAGWA PIC  X(0001).
           05  LAVAGWI PIC  S9(3)V9(1).
      *    -------------------------------
           05  AAVAGWL PIC S9(0004) COMP.
           05  AAVAGWF PIC  X(0001).
           05  FILLER REDEFINES AAVAGWF.
               10  AAVAGWA PIC  X(0001).
           05  AAVAGWI PIC  S9(3)V9(1).
      *    -------------------------------
           05  LAVAGML PIC S9(0004) COMP.
           05  LAVAGMF PIC  X(0001).
           05  FILLER REDEFINES LAVAGMF.
               10  LAVAGMA PIC  X(0001).
           05  LAVAGMI PIC  99.
      *    -------------------------------
           05  AAVAGML PIC S9(0004) COMP.
           05  AAVAGMF PIC  X(0001).
           05  FILLER REDEFINES AAVAGMF.
               10  AAVAGMA PIC  X(0001).
           05  AAVAGMI PIC  99.
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  99.
       01  EL6041AO REDEFINES EL6041AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPORTO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPYRPTO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LTONEYO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZERMPDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ISSCNTO PIC  ZZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SMPRMPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPRMDCO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNCRATO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETLIMO PIC  ZZZZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LBENTPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABENTPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLSRATO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLTLRSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALSRATO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTLRSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LPDPROO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLTPRPO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APDPROO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTPRPO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLTIDCO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTIDCO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLTTRMO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTTRMO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LTRMAVO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATRMAVO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLTAGPO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTAGPO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LAVAGWO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAVAGWO PIC  99.9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LAVAGMO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAVAGMO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  ZZ.
      *    -------------------------------
00220      EJECT
00221
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
00223  01  DFHCOMMAREA             PIC X(1024).
00224
00225      EJECT
00226
00227 *01 PARMLIST .
00228 *    12  FILLER                      PIC S9(8)   COMP.
00229 *    12  ELCNTL-POINTER              PIC S9(8)   COMP.
00230
00231      EJECT
00232
00233 *                            COPY ELCCNTL.
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
031808
031808         16  FILLER                         PIC X(82).
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
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
00498          16  FILLER                             PIC  X(240).
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
00607              20  FILLER                     PIC X.
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
011410         16  FILLER                         PIC X(187).
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
082603             20  FILLER                     PIC X(11).
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
092705         16  FILLER                         PIC X(448).
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
00234
00235      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6041' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00237
00238      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00239      MOVE '5'                    TO DC-OPTION-CODE.
00240      PERFORM 9700-DATE-LINK.
00241      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00242      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00243
00244      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00245      MOVE 1                      TO EMI-NUMBER-OF-LINES.
00246
00247      IF EIBCALEN = 0
00248          GO TO 8800-UNAUTHORIZED-ACCESS.
00249
00250      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00251          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00252              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00253              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00254              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00255              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00256              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00257              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00258              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00259              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00260              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
00261          ELSE
00262              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM
00263              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00264              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00265              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00266              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00267              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00268              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00269              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00270              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00271
00272      
      * EXEC CICS    HANDLE    CONDITION
00273 *         PGMIDERR          (9600-PGMID-ERROR)
00274 *         ERROR             (9990-ABEND)
00275 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00002908' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032393038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00276
00277      IF  EIBTRNID NOT = TRANS-ID
00278          MOVE LOW-VALUES         TO EL6041AI
00279          GO TO 8100-SEND-INITIAL-MAP.
00280
00281      IF  EIBAID = DFHCLEAR
00282              OR
00283          NOT SYSTEM-DISPLAY-CAP
00284          GO TO 9400-CLEAR.
00285
00286      EJECT
00287
00288  0200-RECEIVE.
00289      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00290         MOVE ER-7008            TO EMI-ERROR
00291         PERFORM 9900-ERROR-FORMAT
00292         MOVE -1                 TO MAINTL
00293         GO TO 8200-SEND-DATAONLY.
00294
00295      
      * EXEC CICS RECEIVE
00296 *        MAP      (MAP-NAME)
00297 *        MAPSET   (MAPSET-NAME)
00298 *        INTO     (EL6041AI)
00299 *    END-EXEC.
           MOVE LENGTH OF
            EL6041AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002931' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6041AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00300
00301      IF  PFKEYL = +0
00302          GO TO 0300-CHECK-PFKEYS.
00303
00304      IF  EIBAID NOT = DFHENTER
00305          MOVE ER-0004            TO EMI-ERROR
00306          GO TO 0320-INPUT-ERROR.
00307
00308      IF PFKEYI NOT NUMERIC
00309         MOVE ER-0029     TO EMI-ERROR
00310         GO TO 0320-INPUT-ERROR.
00311
00312      IF PFKEYI GREATER 01 AND LESS 25
00313         MOVE PF-VALUES (PFKEYI) TO EIBAID
00314      ELSE
00315         MOVE ER-0029        TO EMI-ERROR
00316         GO TO 0320-INPUT-ERROR.
00317
00318  0300-CHECK-PFKEYS.
00319      IF EIBAID = DFHPF23
00320          GO TO 8810-PF23.
00321
00322      IF EIBAID = DFHPF24
00323          GO TO 9200-RETURN-MAIN-MENU.
00324
00325      IF EIBAID = DFHPF12
00326          GO TO 9500-PF12.
00327
00328      IF MAINTL GREATER THAN +0 AND
00329               EIBAID NOT = DFHENTER
00330         MOVE -1             TO  MAINTL
00331         MOVE  ER-0050       TO  EMI-ERROR
00332         PERFORM 9900-ERROR-FORMAT
00333         GO TO 8200-SEND-DATAONLY.
00334
00335      IF  EIBAID = DFHPF1
00336          PERFORM 7000-BROWSE-FWRD-NEXT-ACCOUNT THRU 7090-EXIT
00337          GO TO 4000-SHOW-REPORT.
00338
00339      IF  EIBAID = DFHPF2
00340          PERFORM 7100-BROWSE-BWRD-NEXT-ACCOUNT THRU 7190-EXIT
00341          GO TO 4000-SHOW-REPORT.
00342
00343      IF EIBAID = DFHENTER
00344          GO TO 0400-EDIT-INPUT-DATA.
00345
00346      MOVE ER-0029                TO EMI-ERROR.
00347
00348  0320-INPUT-ERROR.
00349      PERFORM 9900-ERROR-FORMAT.
00350      MOVE AL-UNBON               TO PFKEYA.
00351      IF PFKEYL = 0
00352          MOVE -1                 TO MAINTL
00353      ELSE
00354          MOVE -1                 TO PFKEYL.
00355
00356      GO TO 8200-SEND-DATAONLY.
00357
00358      EJECT
00359
00360  0400-EDIT-INPUT-DATA.
00361      IF MAINTI = 'S'
00362         GO TO 4000-SHOW-REPORT.
00363
00364      IF   NOT SYSTEM-MODIFY-CAP
00365           MOVE 'UPDATE'       TO SM-READ
00366           PERFORM 9995-SECURITY-VIOLATION
00367           MOVE ER-0070        TO EMI-ERROR
00368           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00369           GO TO 8100-SEND-INITIAL-MAP.
00370
00371      IF MAINTI = 'A'
00372         GO TO 1000-ADD-REPORT.
00373
00374      IF MAINTI = 'C'
00375         GO TO 2000-CHANGE-REPORT.
00376
00377      IF MAINTI = 'D'
00378         GO TO 2500-DELETE-REPORT.
00379
00380      IF MAINTI = 'K'
00381         GO TO 3000-COPY-REPORT.
00382
00383      MOVE  ER-0023            TO EMI-ERROR
00384      MOVE -1                  TO MAINTL
00385      MOVE AL-UABON            TO MAINTA
00386      PERFORM 9900-ERROR-FORMAT
00387      GO TO 8200-SEND-DATAONLY.
00388
00389      EJECT
00390
00391  1000-ADD-REPORT.
00392      IF REPORTL GREATER +0 AND
00393         REPORTI NUMERIC
00394         NEXT SENTENCE
00395      ELSE
00396         MOVE ER-7680     TO EMI-ERROR
00397         MOVE -1          TO REPORTL
00398         MOVE AL-UNBON    TO REPORTA
00399         PERFORM 9900-ERROR-FORMAT
00400         GO TO 8200-SEND-DATAONLY.
00401
00402      MOVE SPACES            TO ELCNTL-KEY.
00403      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.
00404      MOVE REPORTI           TO ELCNTL-REPORT.
00405      MOVE 'C'               TO ELCNTL-RECORD-TYPE.
00406      MOVE +0002             TO ELCNTL-SEQ-NO.
00407
00408      
      * EXEC CICS HANDLE CONDITION
00409 *         NOTFND     (1100-EDIT-REPORT-SCREEN)
00410 *    END-EXEC.
      *    MOVE '"$I                   ! # #00003044' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033303434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00411
00412      
      * EXEC CICS READ
00413 *         DATASET    (ELCNTL-FILE-ID)
00414 *         RIDFLD     (ELCNTL-KEY)
00415 *         SET        (ADDRESS OF CONTROL-FILE)
00416 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003048' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303438' TO DFHEIV0(25:11)
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
           
00417
00418      MOVE -1                     TO REPORTL.
00419      MOVE  ER-0132               TO  EMI-ERROR.
00420      PERFORM 9900-ERROR-FORMAT.
00421      GO TO 8200-SEND-DATAONLY.
00422
00423  1100-EDIT-REPORT-SCREEN.
00424      IF LTONEYL GREATER +0
00425         IF LTONEYI = 'Y' OR 'N'
00426            NEXT SENTENCE
00427         ELSE
00428            MOVE ER-2001            TO EMI-ERROR
00429            MOVE -1                 TO LTONEYL
00430            MOVE AL-UABON           TO LTONEYA
00431            PERFORM 9900-ERROR-FORMAT.
00432
00433      IF ZERMPDL GREATER +0
00434         IF ZERMPDI = 'A' OR 'B' OR 'C'
00435            NEXT SENTENCE
00436         ELSE
00437            MOVE ER-7354            TO EMI-ERROR
00438            MOVE -1                 TO ZERMPDL
00439            MOVE AL-UABON           TO ZERMPDA
00440            PERFORM 9900-ERROR-FORMAT.
00441
00442      IF ISSCNTL GREATER THAN +0
00443          MOVE ISSCNTI TO DEEDIT-FIELD
00444          PERFORM 8600-DEEDIT
00445          IF DEEDIT-FIELD-V0  NOT NUMERIC
00446              MOVE -1 TO ISSCNTL
00447              MOVE AL-UNBON TO ISSCNTA
00448              MOVE ER-2223 TO EMI-ERROR
00449              PERFORM 9900-ERROR-FORMAT
00450          ELSE
00451              MOVE DEEDIT-FIELD-V0 TO WS-ISS-COUNT-DIFF.
00452
00453      IF SMPRMPL GREATER THAN +0
00454          MOVE SMPRMPI TO DEEDIT-FIELD
00455          PERFORM 8600-DEEDIT
00456          IF DEEDIT-FIELD-V0  NOT NUMERIC
00457              MOVE -1 TO SMPRMPL
00458              MOVE AL-UNBON TO SMPRMPA
00459              MOVE ER-2223 TO EMI-ERROR
00460              PERFORM 9900-ERROR-FORMAT
00461          ELSE
00462              MOVE DEEDIT-FIELD-V0 TO WS-SINGLE-MO-PREM-PCT.
00463
00464      IF EPRMDCL GREATER THAN +0
00465          MOVE EPRMDCI TO DEEDIT-FIELD
00466          PERFORM 8600-DEEDIT
00467          IF DEEDIT-FIELD-V0  NOT NUMERIC
00468              MOVE -1 TO EPRMDCL
00469              MOVE AL-UNBON TO EPRMDCA
00470              MOVE ER-2223 TO EMI-ERROR
00471              PERFORM 9900-ERROR-FORMAT
00472          ELSE
00473              MOVE DEEDIT-FIELD-V0 TO WS-EARN-PREM-DECLINE.
00474
00475      IF CNCRATL GREATER THAN +0
00476          MOVE CNCRATI TO DEEDIT-FIELD
00477          PERFORM 8600-DEEDIT
00478          IF DEEDIT-FIELD-V0  NOT NUMERIC
00479              MOVE -1 TO CNCRATL
00480              MOVE AL-UNBON TO CNCRATA
00481              MOVE ER-2223 TO EMI-ERROR
00482              PERFORM 9900-ERROR-FORMAT
00483          ELSE
00484              MOVE DEEDIT-FIELD-V0 TO WS-CANCELLATION-RATIO.
00485
00486      IF RETLIML GREATER THAN +0
00487          MOVE RETLIMI TO DEEDIT-FIELD
00488          PERFORM 8600-DEEDIT
00489          IF DEEDIT-FIELD-V0  NOT NUMERIC
00490              MOVE -1 TO RETLIML
00491              MOVE AL-UNBON TO RETLIMA
00492              MOVE ER-2223 TO EMI-ERROR
00493              PERFORM 9900-ERROR-FORMAT
00494          ELSE
00495              MOVE DEEDIT-FIELD-V0 TO WS-RETENTION-LIMIT.
00496
00497      IF LLSRATL GREATER THAN +0
00498          MOVE LLSRATI TO DEEDIT-FIELD
00499          PERFORM 8600-DEEDIT
00500          IF DEEDIT-FIELD-V0  NOT NUMERIC
00501              MOVE -1 TO LLSRATL
00502              MOVE AL-UNBON TO LLSRATA
00503              MOVE ER-2223 TO EMI-ERROR
00504              PERFORM 9900-ERROR-FORMAT
00505          ELSE
00506              MOVE DEEDIT-FIELD-V0 TO WS-LF-LOSS-RATIO-PCT.
00507
00508      IF LLTLRSL GREATER THAN +0
00509          MOVE LLTLRSI TO DEEDIT-FIELD
00510          PERFORM 8600-DEEDIT
00511          IF DEEDIT-FIELD-V0  NOT NUMERIC
00512              MOVE -1 TO LLTLRSL
00513              MOVE AL-UNBON TO LLTLRSA
00514              MOVE ER-2223 TO EMI-ERROR
00515              PERFORM 9900-ERROR-FORMAT
00516          ELSE
00517              MOVE DEEDIT-FIELD-V0 TO WS-LF-LTM-LOSS-RATIO.
00518
00519      IF LPDPROL GREATER THAN +0
00520          MOVE LPDPROI TO DEEDIT-FIELD
00521          PERFORM 8600-DEEDIT
00522          IF DEEDIT-FIELD-V0  NOT NUMERIC
00523              MOVE -1 TO LPDPROL
00524              MOVE AL-UNBON TO LPDPROA
00525              MOVE ER-2223 TO EMI-ERROR
00526              PERFORM 9900-ERROR-FORMAT
00527          ELSE
00528              MOVE DEEDIT-FIELD-V0 TO WS-LF-PERIOD-PROFIT.
00529
00530      IF LLTPRPL GREATER THAN +0
00531          MOVE LLTPRPI TO DEEDIT-FIELD
00532          PERFORM 8600-DEEDIT
00533          IF DEEDIT-FIELD-V0  NOT NUMERIC
00534              MOVE -1 TO LLTPRPL
00535              MOVE AL-UNBON TO LLTPRPA
00536              MOVE ER-2223 TO EMI-ERROR
00537              PERFORM 9900-ERROR-FORMAT
00538          ELSE
00539              MOVE DEEDIT-FIELD-V1 TO WS-LF-LTM-PROFIT-PCT.
00540
00541      IF LLTIDCL GREATER THAN +0
00542          MOVE LLTIDCI TO DEEDIT-FIELD
00543          PERFORM 8600-DEEDIT
00544          IF DEEDIT-FIELD-V0  NOT NUMERIC
00545              MOVE -1 TO LLTIDCL
00546              MOVE AL-UNBON TO LLTIDCA
00547              MOVE ER-2223 TO EMI-ERROR
00548              PERFORM 9900-ERROR-FORMAT
00549          ELSE
00550              MOVE DEEDIT-FIELD-V1 TO WS-LF-LTM-INFORCE-DECR.
00551
00552      IF LLTTRML GREATER THAN +0
00553          MOVE LLTTRMI TO DEEDIT-FIELD
00554          PERFORM 8600-DEEDIT
00555          IF DEEDIT-FIELD-V0  NOT NUMERIC
00556              MOVE -1 TO LLTTRML
00557              MOVE AL-UNBON TO LLTTRMA
00558              MOVE ER-2223 TO EMI-ERROR
00559              PERFORM 9900-ERROR-FORMAT
00560          ELSE
00561              MOVE DEEDIT-FIELD-V1 TO WS-LF-LTM-TERM-CHG.
00562
00563      IF LTRMAVL GREATER THAN +0
00564          MOVE LTRMAVI TO DEEDIT-FIELD
00565          PERFORM 8600-DEEDIT
00566          IF DEEDIT-FIELD-V0  NOT NUMERIC
00567              MOVE -1 TO LTRMAVL
00568              MOVE AL-UNBON TO LTRMAVA
00569              MOVE ER-2223 TO EMI-ERROR
00570              PERFORM 9900-ERROR-FORMAT
00571          ELSE
00572              MOVE DEEDIT-FIELD-V1 TO WS-LF-TERM-AVG-WEIGHTED.
00573
00574      IF LLTAGPL GREATER THAN +0
00575          MOVE LLTAGPI TO DEEDIT-FIELD
00576          PERFORM 8600-DEEDIT
00577          IF DEEDIT-FIELD-V0  NOT NUMERIC
00578              MOVE -1 TO LLTAGPL
00579              MOVE AL-UNBON TO LLTAGPA
00580              MOVE ER-2223 TO EMI-ERROR
00581              PERFORM 9900-ERROR-FORMAT
00582          ELSE
00583              MOVE DEEDIT-FIELD-V1 TO WS-LF-LTM-AGE-PCT.
00584
00585      IF LAVAGWL GREATER THAN +0
00586          MOVE LAVAGWI TO DEEDIT-FIELD
00587          PERFORM 8600-DEEDIT
00588          IF DEEDIT-FIELD-V0  NOT NUMERIC
00589              MOVE -1 TO LAVAGWL
00590              MOVE AL-UNBON TO LAVAGWA
00591              MOVE ER-2223 TO EMI-ERROR
00592              PERFORM 9900-ERROR-FORMAT
00593          ELSE
00594              MOVE DEEDIT-FIELD-V1 TO WS-LF-AGE-AVG-WEIGHTED.
00595
00596      IF LAVAGML GREATER THAN +0
00597          MOVE LAVAGMI TO DEEDIT-FIELD
00598          PERFORM 8600-DEEDIT
00599          IF DEEDIT-FIELD-V0  NOT NUMERIC
00600              MOVE -1 TO LAVAGML
00601              MOVE AL-UNBON TO LAVAGMA
00602              MOVE ER-2223 TO EMI-ERROR
00603              PERFORM 9900-ERROR-FORMAT
00604          ELSE
00605              MOVE DEEDIT-FIELD-V0 TO WS-LF-AVG-AGE-MAX.
00606
00607      IF ALSRATL GREATER THAN +0
00608          MOVE ALSRATI TO DEEDIT-FIELD
00609          PERFORM 8600-DEEDIT
00610          IF DEEDIT-FIELD-V0  NOT NUMERIC
00611              MOVE -1 TO ALSRATL
00612              MOVE AL-UNBON TO ALSRATA
00613              MOVE ER-2223 TO EMI-ERROR
00614              PERFORM 9900-ERROR-FORMAT
00615          ELSE
00616              MOVE DEEDIT-FIELD-V0 TO WS-AH-LOSS-RATIO-PCT.
00617
00618      IF ALTLRSL GREATER THAN +0
00619          MOVE ALTLRSI TO DEEDIT-FIELD
00620          PERFORM 8600-DEEDIT
00621          IF DEEDIT-FIELD-V0  NOT NUMERIC
00622              MOVE -1 TO ALTLRSL
00623              MOVE AL-UNBON TO ALTLRSA
00624              MOVE ER-2223 TO EMI-ERROR
00625              PERFORM 9900-ERROR-FORMAT
00626          ELSE
00627              MOVE DEEDIT-FIELD-V0 TO WS-AH-LTM-LOSS-RATIO.
00628
00629      IF APDPROL GREATER THAN +0
00630          MOVE APDPROI TO DEEDIT-FIELD
00631          PERFORM 8600-DEEDIT
00632          IF DEEDIT-FIELD-V0  NOT NUMERIC
00633              MOVE -1 TO APDPROL
00634              MOVE AL-UNBON TO APDPROA
00635              MOVE ER-2223 TO EMI-ERROR
00636              PERFORM 9900-ERROR-FORMAT
00637          ELSE
00638              MOVE DEEDIT-FIELD-V0 TO WS-AH-PERIOD-PROFIT.
00639
00640      IF ALTPRPL GREATER THAN +0
00641          MOVE ALTPRPI TO DEEDIT-FIELD
00642          PERFORM 8600-DEEDIT
00643          IF DEEDIT-FIELD-V0  NOT NUMERIC
00644              MOVE -1 TO ALTPRPL
00645              MOVE AL-UNBON TO ALTPRPA
00646              MOVE ER-2223 TO EMI-ERROR
00647              PERFORM 9900-ERROR-FORMAT
00648          ELSE
00649              MOVE DEEDIT-FIELD-V1 TO WS-AH-LTM-PROFIT-PCT.
00650
00651      IF ALTIDCL GREATER THAN +0
00652          MOVE ALTIDCI TO DEEDIT-FIELD
00653          PERFORM 8600-DEEDIT
00654          IF DEEDIT-FIELD-V0  NOT NUMERIC
00655              MOVE -1 TO ALTIDCL
00656              MOVE AL-UNBON TO ALTIDCA
00657              MOVE ER-2223 TO EMI-ERROR
00658              PERFORM 9900-ERROR-FORMAT
00659          ELSE
00660              MOVE DEEDIT-FIELD-V1 TO WS-AH-LTM-INFORCE-DECR.
00661
00662      IF ALTTRML GREATER THAN +0
00663          MOVE ALTTRMI TO DEEDIT-FIELD
00664          PERFORM 8600-DEEDIT
00665          IF DEEDIT-FIELD-V0  NOT NUMERIC
00666              MOVE -1 TO ALTTRML
00667              MOVE AL-UNBON TO ALTTRMA
00668              MOVE ER-2223 TO EMI-ERROR
00669              PERFORM 9900-ERROR-FORMAT
00670          ELSE
00671              MOVE DEEDIT-FIELD-V1 TO WS-AH-LTM-TERM-CHG.
00672
00673      IF ATRMAVL GREATER THAN +0
00674          MOVE ATRMAVI TO DEEDIT-FIELD
00675          PERFORM 8600-DEEDIT
00676          IF DEEDIT-FIELD-V0  NOT NUMERIC
00677              MOVE -1 TO ATRMAVL
00678              MOVE AL-UNBON TO ATRMAVA
00679              MOVE ER-2223 TO EMI-ERROR
00680              PERFORM 9900-ERROR-FORMAT
00681          ELSE
00682              MOVE DEEDIT-FIELD-V1 TO WS-AH-TERM-AVG-WEIGHTED.
00683
00684      IF ALTAGPL GREATER THAN +0
00685          MOVE ALTAGPI TO DEEDIT-FIELD
00686          PERFORM 8600-DEEDIT
00687          IF DEEDIT-FIELD-V0  NOT NUMERIC
00688              MOVE -1 TO ALTAGPL
00689              MOVE AL-UNBON TO ALTAGPA
00690              MOVE ER-2223 TO EMI-ERROR
00691              PERFORM 9900-ERROR-FORMAT
00692          ELSE
00693              MOVE DEEDIT-FIELD-V1 TO WS-AH-LTM-AGE-PCT.
00694
00695      IF AAVAGWL GREATER THAN +0
00696          MOVE AAVAGWI TO DEEDIT-FIELD
00697          PERFORM 8600-DEEDIT
00698          IF DEEDIT-FIELD-V0  NOT NUMERIC
00699              MOVE -1 TO AAVAGWL
00700              MOVE AL-UNBON TO AAVAGWA
00701              MOVE ER-2223 TO EMI-ERROR
00702              PERFORM 9900-ERROR-FORMAT
00703          ELSE
00704              MOVE DEEDIT-FIELD-V1 TO WS-AH-AGE-AVG-WEIGHTED.
00705
00706      IF AAVAGML GREATER THAN +0
00707          MOVE AAVAGMI TO DEEDIT-FIELD
00708          PERFORM 8600-DEEDIT
00709          IF DEEDIT-FIELD-V0  NOT NUMERIC
00710              MOVE -1 TO AAVAGML
00711              MOVE AL-UNBON TO AAVAGMA
00712              MOVE ER-2223 TO EMI-ERROR
00713              PERFORM 9900-ERROR-FORMAT
00714          ELSE
00715              MOVE DEEDIT-FIELD-V0 TO WS-AH-AVG-AGE-MAX.
00716
00717      IF NOT EMI-NO-ERRORS
00718         GO TO 8200-SEND-DATAONLY.
00719
00720  1200-ADD-REPORT-RECORD.
00721     
      * EXEC CICS GETMAIN
00722 *        SET       (ADDRESS OF CONTROL-FILE)
00723 *        LENGTH    (ELCNTL-LENGTH)
00724 *        INITIMG   (GETMAIN-SPACE)
00725 *    END-EXEC.
      *    MOVE ',"IL                  $   #00003357' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCNTL-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00726
00727      MOVE 'N'                   TO CF-ACCOUNTS-LT-ONE-YEAR.
00728
00729      MOVE ZEROS                 TO CF-ISS-COUNT-DIFF
00730                                    CF-SINGLE-MO-PREM-PCT
00731                                    CF-EARN-PREM-DECR-PCT
00732                                    CF-CANCELLATION-RATIO
00733                                    CF-RETENTION-LIMIT
00734                                    CF-LF-LOSS-RATIO-PCT
00735                                    CF-LF-LTM-LOSS-RATIO
00736                                    CF-LF-PERIOD-PROFIT
00737                                    CF-LF-LTM-PROFIT-PCT
00738                                    CF-LF-LTM-INFORCE-DECR
00739                                    CF-LF-LTM-TERM-CHG
00740                                    CF-LF-TERM-AVG-WEIGHTED
00741                                    CF-LF-LTM-AGE-PCT
00742                                    CF-LF-AGE-AVG-WEIGHTED
00743                                    CF-LF-AVG-AGE-MAX
00744                                    CF-AH-LOSS-RATIO-PCT
00745                                    CF-AH-LTM-LOSS-RATIO
00746                                    CF-AH-PERIOD-PROFIT
00747                                    CF-AH-LTM-PROFIT-PCT
00748                                    CF-AH-LTM-INFORCE-DECR
00749                                    CF-AH-LTM-TERM-CHG
00750                                    CF-AH-TERM-AVG-WEIGHTED
00751                                    CF-AH-LTM-AGE-PCT
00752                                    CF-AH-AGE-AVG-WEIGHTED
00753                                    CF-AH-AVG-AGE-MAX.
00754
00755  1500-BUILD-RECORD.
00756
00757      MOVE 'CF'                  TO CF-RECORD-ID
00758      MOVE PI-COMPANY-ID         TO CF-COMPANY-ID
00759      MOVE 'C'                   TO CF-RECORD-TYPE
00760      MOVE REPORTI               TO CF-CUSTOM-REPORT-NO
00761      MOVE +0002                 TO CF-SEQUENCE-NO.
00762
00763      MOVE SAVE-BIN-DATE         TO CF-LAST-MAINT-DT
00764      MOVE PI-PROCESSOR-ID       TO CF-LAST-MAINT-BY
00765      MOVE EIBTIME               TO CF-LAST-MAINT-HHMMSS.
00766
00767      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT
00768                                           REPORTO.
00769
00770      IF CF-ISS-COUNT-DIFF NOT NUMERIC
00771           MOVE ZEROS TO CF-ISS-COUNT-DIFF.
00772
00773      IF CF-SINGLE-MO-PREM-PCT NOT NUMERIC
00774          MOVE ZEROS TO CF-SINGLE-MO-PREM-PCT.
00775
00776      IF CF-EARN-PREM-DECR-PCT NOT NUMERIC
00777          MOVE ZEROS TO CF-SINGLE-MO-PREM-PCT.
00778
00779      IF CF-CANCELLATION-RATIO NOT NUMERIC
00780          MOVE ZEROS TO CF-CANCELLATION-RATIO.
00781
00782      IF CF-RETENTION-LIMIT NOT NUMERIC
00783           MOVE ZEROS TO CF-RETENTION-LIMIT.
00784
00785      IF CF-LF-LOSS-RATIO-PCT NOT NUMERIC
00786          MOVE ZEROS TO CF-LF-LOSS-RATIO-PCT.
00787
00788      IF CF-LF-LTM-LOSS-RATIO NOT NUMERIC
00789          MOVE ZEROS TO CF-LF-LTM-LOSS-RATIO.
00790
00791      IF CF-LF-PERIOD-PROFIT NOT NUMERIC
00792          MOVE ZEROS TO CF-LF-PERIOD-PROFIT.
00793
00794      IF CF-LF-LTM-PROFIT-PCT NOT NUMERIC
00795          MOVE ZEROS TO CF-LF-LTM-PROFIT-PCT.
00796
00797      IF CF-LF-LTM-INFORCE-DECR NOT NUMERIC
00798          MOVE ZEROS TO CF-LF-LTM-INFORCE-DECR.
00799
00800      IF CF-LF-LTM-TERM-CHG NOT NUMERIC
00801          MOVE ZEROS TO CF-LF-LTM-TERM-CHG.
00802
00803      IF CF-LF-TERM-AVG-WEIGHTED NOT NUMERIC
00804          MOVE ZEROS TO CF-LF-TERM-AVG-WEIGHTED.
00805
00806      IF CF-LF-LTM-AGE-PCT NOT NUMERIC
00807          MOVE ZEROS TO CF-LF-LTM-AGE-PCT.
00808
00809      IF CF-LF-AGE-AVG-WEIGHTED NOT NUMERIC
00810          MOVE ZEROS TO CF-LF-AGE-AVG-WEIGHTED.
00811
00812      IF CF-LF-AVG-AGE-MAX NOT NUMERIC
00813          MOVE ZEROS TO CF-LF-AVG-AGE-MAX.
00814
00815      IF CF-AH-LOSS-RATIO-PCT NOT NUMERIC
00816          MOVE ZEROS TO CF-AH-LOSS-RATIO-PCT.
00817
00818      IF CF-AH-LTM-LOSS-RATIO NOT NUMERIC
00819          MOVE ZEROS TO CF-AH-LTM-LOSS-RATIO.
00820
00821      IF CF-AH-PERIOD-PROFIT NOT NUMERIC
00822          MOVE ZEROS TO CF-AH-PERIOD-PROFIT.
00823
00824      IF CF-AH-LTM-PROFIT-PCT NOT NUMERIC
00825          MOVE ZEROS TO CF-AH-LTM-PROFIT-PCT.
00826
00827      IF CF-AH-LTM-INFORCE-DECR NOT NUMERIC
00828          MOVE ZEROS TO CF-AH-LTM-INFORCE-DECR.
00829
00830      IF CF-AH-LTM-TERM-CHG NOT NUMERIC
00831          MOVE ZEROS TO CF-AH-LTM-TERM-CHG.
00832
00833      IF CF-AH-TERM-AVG-WEIGHTED NOT NUMERIC
00834          MOVE ZEROS TO CF-AH-TERM-AVG-WEIGHTED.
00835
00836      IF CF-AH-LTM-AGE-PCT NOT NUMERIC
00837          MOVE ZEROS TO CF-AH-LTM-AGE-PCT.
00838
00839      IF CF-AH-AGE-AVG-WEIGHTED NOT NUMERIC
00840          MOVE ZEROS TO CF-AH-AGE-AVG-WEIGHTED.
00841
00842      IF CF-AH-AVG-AGE-MAX NOT NUMERIC
00843          MOVE ZEROS TO CF-AH-AVG-AGE-MAX.
00844
00845      IF LTONEYL GREATER THAN +0
00846          MOVE LTONEYI TO  CF-ACCOUNTS-LT-ONE-YEAR.
00847
00848      IF ZERMPDL GREATER THAN +0
00849          MOVE ZERMPDI TO  CF-ACCT-ZERO-MONTH-PRODUCTION.
00850
00851      IF ISSCNTL GREATER THAN +0
00852          MOVE WS-ISS-COUNT-DIFF TO  CF-ISS-COUNT-DIFF.
00853
00854      IF SMPRMPL GREATER THAN +0
00855          MOVE WS-SINGLE-MO-PREM-PCT TO  CF-SINGLE-MO-PREM-PCT.
00856
00857      IF EPRMDCL GREATER THAN +0
00858          MOVE WS-EARN-PREM-DECLINE TO  CF-EARN-PREM-DECR-PCT.
00859
00860      IF CNCRATL GREATER THAN +0
00861          MOVE WS-CANCELLATION-RATIO TO  CF-CANCELLATION-RATIO.
00862
00863      IF RETLIML GREATER THAN +0
00864          MOVE WS-RETENTION-LIMIT TO CF-RETENTION-LIMIT.
00865
00866      IF LLSRATL GREATER THAN +0
00867          MOVE WS-LF-LOSS-RATIO-PCT TO  CF-LF-LOSS-RATIO-PCT.
00868
00869      IF LLTLRSL GREATER THAN +0
00870          MOVE WS-LF-LTM-LOSS-RATIO TO  CF-LF-LTM-LOSS-RATIO.
00871
00872      IF LPDPROL GREATER THAN +0
00873          MOVE WS-LF-PERIOD-PROFIT TO  CF-LF-PERIOD-PROFIT.
00874
00875      IF LLTPRPL GREATER THAN +0
00876          MOVE WS-LF-LTM-PROFIT-PCT TO  CF-LF-LTM-PROFIT-PCT.
00877
00878      IF LLTIDCL GREATER THAN +0
00879          MOVE WS-LF-LTM-INFORCE-DECR TO CF-LF-LTM-INFORCE-DECR.
00880
00881      IF LLTTRML GREATER THAN +0
00882          MOVE WS-LF-LTM-TERM-CHG TO  CF-LF-LTM-TERM-CHG.
00883
00884      IF LTRMAVL GREATER THAN +0
00885          MOVE WS-LF-TERM-AVG-WEIGHTED
00886                                  TO  CF-LF-TERM-AVG-WEIGHTED.
00887
00888      IF LLTAGPL GREATER THAN +0
00889          MOVE WS-LF-LTM-AGE-PCT TO  CF-LF-LTM-AGE-PCT.
00890
00891      IF LAVAGWL GREATER THAN +0
00892          MOVE WS-LF-AGE-AVG-WEIGHTED TO  CF-LF-AGE-AVG-WEIGHTED.
00893
00894      IF LAVAGML GREATER THAN +0
00895          MOVE WS-LF-AVG-AGE-MAX TO  CF-LF-AVG-AGE-MAX.
00896
00897      IF ALSRATL GREATER THAN +0
00898          MOVE WS-AH-LOSS-RATIO-PCT TO  CF-AH-LOSS-RATIO-PCT.
00899
00900      IF ALTLRSL GREATER THAN +0
00901          MOVE WS-AH-LTM-LOSS-RATIO TO  CF-AH-LTM-LOSS-RATIO.
00902
00903      IF APDPROL GREATER THAN +0
00904          MOVE WS-AH-PERIOD-PROFIT TO  CF-AH-PERIOD-PROFIT.
00905
00906      IF ALTPRPL GREATER THAN +0
00907          MOVE WS-AH-LTM-PROFIT-PCT TO  CF-AH-LTM-PROFIT-PCT.
00908
00909      IF ALTIDCL GREATER THAN +0
00910          MOVE WS-AH-LTM-INFORCE-DECR TO  CF-AH-LTM-INFORCE-DECR.
00911
00912      IF ALTTRML GREATER THAN +0
00913          MOVE WS-AH-LTM-TERM-CHG TO  CF-AH-LTM-TERM-CHG.
00914
00915      IF ATRMAVL GREATER THAN +0
00916          MOVE WS-AH-TERM-AVG-WEIGHTED
00917                                  TO  CF-AH-TERM-AVG-WEIGHTED.
00918
00919      IF ALTAGPL GREATER THAN +0
00920          MOVE WS-AH-LTM-AGE-PCT TO  CF-AH-LTM-AGE-PCT.
00921
00922      IF AAVAGWL GREATER THAN +0
00923          MOVE WS-AH-AGE-AVG-WEIGHTED
00924                                  TO  CF-AH-AGE-AVG-WEIGHTED.
00925
00926      IF AAVAGML GREATER THAN +0
00927          MOVE WS-AH-AVG-AGE-MAX TO  CF-AH-AVG-AGE-MAX.
00928
00929      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.
00930      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.
00931
00932  1600-WRITE-REPORT-RECORD.
00933
00934      
      * EXEC CICS WRITE
00935 *         DATASET(ELCNTL-FILE-ID)
00936 *         FROM   (CONTROL-FILE)
00937 *         RIDFLD (ELCNTL-KEY)
00938 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003570' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00939
00940      MOVE ER-0000      TO EMI-ERROR.
00941      PERFORM 9900-ERROR-FORMAT.
00942      GO TO 4000-SHOW-REPORT.
00943
00944  2000-CHANGE-REPORT.
00945      IF REPORTI NOT NUMERIC
00946         MOVE ER-7680            TO EMI-ERROR
00947         MOVE -1                 TO REPORTL
00948         MOVE AL-UNBON           TO REPORTA
00949         PERFORM 9900-ERROR-FORMAT
00950         GO TO 8200-SEND-DATAONLY.
00951
00952      IF REPORTI NOT = PI-LAST-REPORT
00953         MOVE ER-0138            TO EMI-ERROR
00954         MOVE -1                 TO REPORTL
00955         MOVE AL-UNBON           TO REPORTA
00956         PERFORM 9900-ERROR-FORMAT
00957         GO TO 8200-SEND-DATAONLY.
00958
00959      MOVE SPACES                 TO ELCNTL-KEY
00960      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
00961      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.
00962      MOVE REPORTI                TO ELCNTL-REPORT.
00963      MOVE +0002                  TO ELCNTL-SEQ-NO.
00964
00965      
      * EXEC CICS HANDLE CONDITION
00966 *         NOTFND     (2100-NOT-FOUND)
00967 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00003601' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033363031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00968
00969      
      * EXEC CICS READ
00970 *         DATASET    (ELCNTL-FILE-ID)
00971 *         SET        (ADDRESS OF CONTROL-FILE)
00972 *         RIDFLD     (ELCNTL-KEY)
00973 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003605' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363035' TO DFHEIV0(25:11)
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
           
00974
00975      IF (CF-LAST-MAINT-BY NOT = PI-UPDATE-BY) OR
00976         (CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS)
00977         MOVE ER-0068          TO EMI-ERROR
00978         MOVE -1               TO MAINTL
00979         PERFORM 9900-ERROR-FORMAT
00980         GO TO 8200-SEND-DATAONLY.
00981
00982      PERFORM 1100-EDIT-REPORT-SCREEN.
00983
00984      IF NOT EMI-NO-ERRORS
00985         GO TO 8200-SEND-DATAONLY.
00986
00987      
      * EXEC CICS READ
00988 *         DATASET    (ELCNTL-FILE-ID)
00989 *         SET        (ADDRESS OF CONTROL-FILE)
00990 *         RIDFLD     (ELCNTL-KEY)
00991 *         UPDATE
00992 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003623' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363233' TO DFHEIV0(25:11)
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
           
00993
00994      PERFORM 1500-BUILD-RECORD.
00995
00996      IF NOT EMI-NO-ERRORS
00997         
      * EXEC CICS UNLOCK
00998 *            DATASET   (ELCNTL-FILE-ID)
00999 *       END-EXEC
      *    MOVE '&*                    #   #00003633' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01000         GO TO 8200-SEND-DATAONLY.
01001
01002      
      * EXEC CICS REWRITE
01003 *         DATASET   (ELCNTL-FILE-ID)
01004 *         FROM      (CONTROL-FILE)
01005 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003638' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01006
01007      MOVE ER-0000      TO EMI-ERROR.
01008      PERFORM 9900-ERROR-FORMAT.
01009      GO TO 4000-SHOW-REPORT.
01010
01011  2100-NOT-FOUND.
01012      MOVE ER-0139              TO EMI-ERROR.
01013      MOVE -1                   TO REPORTL.
01014      MOVE AL-UNBON             TO REPORTA.
01015      PERFORM 9900-ERROR-FORMAT.
01016      GO TO 8200-SEND-DATAONLY.
01017
01018      EJECT
01019
01020  2500-DELETE-REPORT.
01021      IF REPORTI NOT NUMERIC
01022         MOVE ER-7680            TO EMI-ERROR
01023         MOVE -1                 TO REPORTL
01024         MOVE AL-UNBON           TO REPORTA
01025         PERFORM 9900-ERROR-FORMAT
01026         GO TO 8200-SEND-DATAONLY.
01027
01028      IF REPORTI NOT = PI-LAST-REPORT
01029         MOVE ER-0138            TO EMI-ERROR
01030         MOVE -1                 TO REPORTL
01031         MOVE AL-UNBON           TO REPORTA
01032         PERFORM 9900-ERROR-FORMAT
01033         GO TO 8200-SEND-DATAONLY.
01034
01035      
      * EXEC CICS HANDLE CONDITION
01036 *        NOTFND   (2890-NOT-FOUND)
01037 *    END-EXEC.
      *    MOVE '"$I                   ! % #00003671' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033363731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01038
01039      MOVE SPACES                 TO ELCNTL-KEY.
01040
01041      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
01042      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.
01043      MOVE REPORTI                TO ELCNTL-REPORT.
01044      MOVE +0002                  TO ELCNTL-SEQ-NO.
01045
01046      
      * EXEC CICS DELETE
01047 *         DATASET   (ELCNTL-FILE-ID)
01048 *         RIDFLD    (ELCNTL-KEY)
01049 *    END-EXEC.
      *    MOVE '&(  R                 &   #00003682' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01050
01051      MOVE LOW-VALUES TO EL6041AI.
01052      MOVE ER-0000      TO EMI-ERROR.
01053      PERFORM 9900-ERROR-FORMAT.
01054      GO TO 8100-SEND-INITIAL-MAP.
01055
01056  2890-NOT-FOUND.
01057      MOVE  ER-0142               TO EMI-ERROR.
01058      MOVE -1                     TO REPORTL.
01059      MOVE AL-UNBON               TO REPORTA.
01060      PERFORM 9900-ERROR-FORMAT.
01061      GO TO 8200-SEND-DATAONLY.
01062
01063      EJECT
01064  3000-COPY-REPORT.
01065      IF REPORTL GREATER +0 AND
01066         REPORTI NUMERIC
01067         NEXT SENTENCE
01068      ELSE
01069         MOVE ER-7680     TO EMI-ERROR
01070         MOVE -1          TO REPORTL
01071         MOVE AL-UNBON    TO REPORTA
01072         PERFORM 9900-ERROR-FORMAT
01073         GO TO 8200-SEND-DATAONLY.
01074
01075      IF CPYRPTL GREATER +0 AND
01076         CPYRPTI NUMERIC
01077         NEXT SENTENCE
01078      ELSE
01079         MOVE ER-7680     TO EMI-ERROR
01080         MOVE -1          TO CPYRPTL
01081         MOVE AL-UNBON    TO CPYRPTA
01082         PERFORM 9900-ERROR-FORMAT
01083         GO TO 8200-SEND-DATAONLY.
01084
01085      MOVE SPACES            TO ELCNTL-KEY.
01086      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.
01087      MOVE REPORTI           TO ELCNTL-REPORT.
01088      MOVE 'C'               TO ELCNTL-RECORD-TYPE.
01089      MOVE +0002             TO ELCNTL-SEQ-NO.
01090
01091      
      * EXEC CICS HANDLE CONDITION
01092 *         NOTFND     (3100-CONTINUE)
01093 *    END-EXEC.
      *    MOVE '"$I                   ! & #00003727' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01094
01095      
      * EXEC CICS READ
01096 *         DATASET    (ELCNTL-FILE-ID)
01097 *         RIDFLD     (ELCNTL-KEY)
01098 *         SET        (ADDRESS OF CONTROL-FILE)
01099 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003731' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373331' TO DFHEIV0(25:11)
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
           
01100
01101      MOVE -1                     TO REPORTL.
01102      MOVE  ER-0132               TO  EMI-ERROR.
01103      PERFORM 9900-ERROR-FORMAT.
01104      GO TO 8200-SEND-DATAONLY.
01105
01106  3100-CONTINUE.
01107      MOVE SPACES            TO ELCNTL-KEY.
01108      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.
01109      MOVE CPYRPTI           TO ELCNTL-REPORT.
01110      MOVE 'C'               TO ELCNTL-RECORD-TYPE.
01111      MOVE +0002             TO ELCNTL-SEQ-NO.
01112
01113      
      * EXEC CICS HANDLE CONDITION
01114 *         NOTFND     (3900-NOT-FOUND)
01115 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00003749' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033373439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01116
01117      
      * EXEC CICS READ
01118 *         DATASET    (ELCNTL-FILE-ID)
01119 *         RIDFLD     (ELCNTL-KEY)
01120 *         SET        (ADDRESS OF CONTROL-FILE)
01121 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003753' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373533' TO DFHEIV0(25:11)
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
           
01122
01123      MOVE SPACES            TO ELCNTL-KEY.
01124      MOVE 'CF'                  TO CF-RECORD-ID
01125      MOVE PI-COMPANY-ID         TO CF-COMPANY-ID
01126                                    ELCNTL-COMPANY-ID.
01127      MOVE 'C'                   TO CF-RECORD-TYPE
01128                                    ELCNTL-RECORD-TYPE.
01129      MOVE REPORTI               TO CF-CUSTOM-REPORT-NO
01130                                    ELCNTL-REPORT.
01131      MOVE +0002                 TO CF-SEQUENCE-NO
01132                                    ELCNTL-SEQ-NO.
01133
01134      MOVE SAVE-BIN-DATE         TO CF-LAST-MAINT-DT
01135      MOVE PI-PROCESSOR-ID       TO CF-LAST-MAINT-BY
01136      MOVE EIBTIME               TO CF-LAST-MAINT-HHMMSS.
01137
01138      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT
01139                                           REPORTO.
01140
01141      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.
01142      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.
01143
01144      
      * EXEC CICS WRITE
01145 *         DATASET(ELCNTL-FILE-ID)
01146 *         FROM   (CONTROL-FILE)
01147 *         RIDFLD (ELCNTL-KEY)
01148 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003780' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01149
01150      MOVE ER-0000      TO EMI-ERROR.
01151      PERFORM 9900-ERROR-FORMAT.
01152      GO TO 4000-SHOW-REPORT.
01153
01154  3900-NOT-FOUND.
01155      MOVE  ER-0142               TO EMI-ERROR.
01156      MOVE -1                     TO CPYRPTL.
01157      MOVE AL-UNBON               TO CPYRPTA.
01158      PERFORM 9900-ERROR-FORMAT.
01159      GO TO 8200-SEND-DATAONLY.
01160
01161  4000-SHOW-REPORT.
01162      IF MAINTI = 'S'
01163         IF REPORTL GREATER THAN +0 AND
01164            REPORTI NUMERIC
01165            NEXT SENTENCE
01166         ELSE
01167            MOVE ER-7680     TO EMI-ERROR
01168            MOVE -1          TO REPORTL
01169            MOVE AL-UNBON    TO REPORTA
01170            PERFORM 9900-ERROR-FORMAT
01171            GO TO 8200-SEND-DATAONLY.
01172
01173      IF MAINTI = 'S'
01174         MOVE SPACES              TO ELCNTL-KEY
01175         MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
01176         MOVE 'C'                 TO ELCNTL-RECORD-TYPE
01177         MOVE REPORTI             TO ELCNTL-REPORT
01178         MOVE +0002               TO ELCNTL-SEQ-NO.
01179
01180      
      * EXEC CICS HANDLE CONDITION
01181 *         NOTFND     (4100-NOT-FOUND)
01182 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00003816' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033383136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01183
01184      
      * EXEC CICS READ
01185 *         DATASET     (ELCNTL-FILE-ID)
01186 *         RIDFLD      (ELCNTL-KEY)
01187 *         SET         (ADDRESS OF CONTROL-FILE)
01188 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003820' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383230' TO DFHEIV0(25:11)
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
           
01189
01190      MOVE LOW-VALUES                   TO EL6041AI.
01191
01192      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT
01193                                           REPORTO.
01194
01195      MOVE CF-ACCOUNTS-LT-ONE-YEAR       TO  LTONEYO.
01196      MOVE CF-ACCT-ZERO-MONTH-PRODUCTION TO  ZERMPDO.
01197      MOVE CF-ISS-COUNT-DIFF            TO  ISSCNTO.
01198      MOVE CF-SINGLE-MO-PREM-PCT        TO  SMPRMPO.
01199      MOVE CF-EARN-PREM-DECR-PCT        TO  EPRMDCO.
01200      MOVE CF-CANCELLATION-RATIO        TO  CNCRATO.
01201
01202      IF CF-RETENTION-LIMIT NUMERIC
01203          MOVE CF-RETENTION-LIMIT       TO  RETLIMO.
01204
01205      MOVE CF-LF-LOSS-RATIO-PCT         TO  LLSRATO.
01206      MOVE CF-LF-LTM-LOSS-RATIO         TO  LLTLRSO.
01207      MOVE CF-LF-PERIOD-PROFIT          TO  LPDPROO.
01208      MOVE CF-LF-LTM-PROFIT-PCT         TO  LLTPRPO.
01209      MOVE CF-LF-LTM-INFORCE-DECR       TO  LLTIDCO.
01210      MOVE CF-LF-LTM-TERM-CHG           TO  LLTTRMO.
01211      MOVE CF-LF-TERM-AVG-WEIGHTED      TO  LTRMAVO.
01212      MOVE CF-LF-LTM-AGE-PCT            TO  LLTAGPO.
01213      MOVE CF-LF-AGE-AVG-WEIGHTED       TO  LAVAGWO.
01214      MOVE CF-LF-AVG-AGE-MAX            TO  LAVAGMO.
01215
01216      MOVE CF-AH-LOSS-RATIO-PCT         TO  ALSRATO.
01217      MOVE CF-AH-LTM-LOSS-RATIO         TO  ALTLRSO.
01218      MOVE CF-AH-PERIOD-PROFIT          TO  APDPROO.
01219      MOVE CF-AH-LTM-PROFIT-PCT         TO  ALTPRPO.
01220      MOVE CF-AH-LTM-INFORCE-DECR       TO  ALTIDCO.
01221      MOVE CF-AH-LTM-TERM-CHG           TO  ALTTRMO.
01222      MOVE CF-AH-TERM-AVG-WEIGHTED      TO  ATRMAVO.
01223      MOVE CF-AH-LTM-AGE-PCT            TO  ALTAGPO.
01224      MOVE CF-AH-AGE-AVG-WEIGHTED       TO  AAVAGWO.
01225      MOVE CF-AH-AVG-AGE-MAX            TO  AAVAGMO.
01226
01227      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.
01228      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.
01229
01230      GO TO 8100-SEND-INITIAL-MAP.
01231
01232  4100-NOT-FOUND.
01233      MOVE  ER-0142               TO EMI-ERROR.
01234      MOVE -1                     TO REPORTL.
01235      MOVE AL-UNBON               TO REPORTA.
01236      PERFORM 9900-ERROR-FORMAT.
01237      GO TO 8200-SEND-DATAONLY.
01238
01239  5000-VERIFY-CARRIER.
01240      
      * EXEC CICS HANDLE CONDITION
01241 *         NOTFND    (5010-NOT-FOUND)
01242 *         ENDFILE   (5010-NOT-FOUND)
01243 *    END-EXEC.
      *    MOVE '"$I''                  ! ) #00003876' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303033383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01244
01245      MOVE SPACES TO ELCNTL-TEST-KEY.
01246      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.
01247      MOVE '6'                TO ELCNTL-TEST-REC-TYPE.
01248      MOVE WS-TEST-CARRIER    TO ELCNTL-TEST-CAR.
01249      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.
01250
01251      
      * EXEC CICS READ
01252 *         DATASET    (ELCNTL-FILE-ID)
01253 *         RIDFLD     (ELCNTL-TEST-KEY)
01254 *         SET        (ADDRESS OF CONTROL-FILE)
01255 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003887' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01256
01257      GO TO 5019-EXIT.
01258
01259  5010-NOT-FOUND.
01260      MOVE ER-2845           TO EMI-ERROR
01261      MOVE -1                TO WS-TEST-LEN.
01262      MOVE AL-UNBON          TO WS-TEST-ATTRB.
01263      PERFORM 9900-ERROR-FORMAT.
01264
01265  5019-EXIT.
01266      EXIT.
01267
01268  5020-VERIFY-STATE.
01269      
      * EXEC CICS HANDLE CONDITION
01270 *         NOTFND    (5030-NOT-FOUND)
01271 *         ENDFILE   (5030-NOT-FOUND)
01272 *    END-EXEC.
      *    MOVE '"$I''                  ! * #00003905' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303033393035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01273
01274      MOVE SPACES TO ELCNTL-TEST-KEY.
01275      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.
01276      MOVE '3'                TO ELCNTL-TEST-REC-TYPE.
01277      MOVE WS-TEST-STATE      TO ELCNTL-TEST-STATE.
01278      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.
01279
01280      
      * EXEC CICS READ
01281 *         DATASET    (ELCNTL-FILE-ID)
01282 *         RIDFLD     (ELCNTL-TEST-KEY)
01283 *         SET        (ADDRESS OF CONTROL-FILE)
01284 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003916' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01285
01286      GO TO 5039-EXIT.
01287
01288  5030-NOT-FOUND.
01289      MOVE ER-2848           TO EMI-ERROR
01290      MOVE -1                TO WS-TEST-LEN.
01291      MOVE AL-UNBON          TO WS-TEST-ATTRB.
01292      PERFORM 9900-ERROR-FORMAT.
01293
01294  5039-EXIT.
01295      EXIT.
01296
01297  5040-VERIFY-BEN.
01298      IF WS-TEST-BEN = SPACES
01299          GO TO 5059-EXIT.
01300
01301      MOVE SPACES TO WS-BROWSE-SW.
01302
01303      
      * EXEC CICS HANDLE CONDITION
01304 *         NOTFND    (5050-NOT-FOUND)
01305 *         ENDFILE   (5050-NOT-FOUND)
01306 *    END-EXEC.
      *    MOVE '"$I''                  ! + #00003939' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303033393339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01307
01308      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.
01309      MOVE WS-TEST-BEN        TO ELCNTL-TEST-BEN.
01310      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.
01311
01312      
      * EXEC CICS STARTBR
01313 *         DATASET    (ELCNTL-FILE-ID)
01314 *         RIDFLD     (ELCNTL-TEST-KEY)
01315 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003948' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01316
01317      MOVE 'Y'    TO WS-BROWSE-SW.
01318
01319      
      * EXEC CICS READNEXT
01320 *         DATASET    (ELCNTL-FILE-ID)
01321 *         RIDFLD     (ELCNTL-TEST-KEY)
01322 *         SET        (ADDRESS OF CONTROL-FILE)
01323 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003955' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01324
01325      MOVE +1  TO WS-SUB.
01326
01327  5041-SEARCH-BEN-TYPE.
01328      IF WS-SUB  GREATER THAN +8
01329         GO TO 5050-NOT-FOUND.
01330
01331      IF CF-BENEFIT-CODE (WS-SUB) = WS-TEST-BEN
01332         GO TO 5055-ENDBR
01333      ELSE
01334         ADD +1 TO WS-SUB
01335         GO TO 5041-SEARCH-BEN-TYPE.
01336
01337  5050-NOT-FOUND.
01338      MOVE ER-7685           TO EMI-ERROR
01339      MOVE -1                TO WS-TEST-LEN.
01340      MOVE AL-UNBON          TO WS-TEST-ATTRB.
01341      PERFORM 9900-ERROR-FORMAT.
01342
01343  5055-ENDBR.
01344      IF WS-BROWSE-SW = 'Y'
01345         
      * EXEC CICS ENDBR
01346 *            DATASET    (ELCNTL-FILE-ID)
01347 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003981' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01348
01349  5059-EXIT.
01350      EXIT.
01351
01352  5060-VERIFY-BUS-TYPE.
01353      MOVE SPACES TO WS-BROWSE-SW.
01354
01355      
      * EXEC CICS HANDLE CONDITION
01356 *         NOTFND    (5070-NOT-FOUND)
01357 *         ENDFILE   (5070-NOT-FOUND)
01358 *    END-EXEC.
      *    MOVE '"$I''                  ! , #00003991' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303033393931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01359
01360      MOVE SPACES             TO ELCNTL-TEST-KEY.
01361      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.
01362      MOVE '8'                TO ELCNTL-TEST-REC-TYPE.
01363      MOVE WS-TEST-BUSTYP     TO ELCNTL-TEST-BUSTYP.
01364      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.
01365
01366      
      * EXEC CICS STARTBR
01367 *         DATASET    (ELCNTL-FILE-ID)
01368 *         RIDFLD     (ELCNTL-TEST-KEY)
01369 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004002' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01370
01371      MOVE 'Y' TO WS-BROWSE-SW.
01372
01373      
      * EXEC CICS READNEXT
01374 *         DATASET    (ELCNTL-FILE-ID)
01375 *         RIDFLD     (ELCNTL-TEST-KEY)
01376 *         SET        (ADDRESS OF CONTROL-FILE)
01377 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004009' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01378
01379      COMPUTE WS-SUB = WS-TEST-BUSTYP -
01380                     (ELCNTL-TEST-BUSTYP - 20).
01381
01382      IF ELCNTL-TEST-BUSTYP = 99
01383         SUBTRACT +1 FROM WS-SUB.
01384
01385      IF CF-BUSINESS-TITLE (WS-SUB) = SPACES
01386         GO TO 5070-NOT-FOUND.
01387
01388      GO TO 5075-ENDBR.
01389
01390  5070-NOT-FOUND.
01391
01392      MOVE ER-2178           TO EMI-ERROR
01393      MOVE -1                TO WS-TEST-LEN.
01394      MOVE AL-UNBON          TO WS-TEST-ATTRB.
01395      PERFORM 9900-ERROR-FORMAT.
01396
01397  5075-ENDBR.
01398      IF WS-BROWSE-SW = 'Y'
01399         
      * EXEC CICS ENDBR
01400 *            DATASET    (ELCNTL-FILE-ID)
01401 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004035' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01402
01403  5079-EXIT.
01404      EXIT.
01405      EJECT
01406
01407  7000-BROWSE-FWRD-NEXT-ACCOUNT.
01408      MOVE SPACES            TO ELCNTL-KEY.
01409      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.
01410      MOVE 'C'               TO ELCNTL-RECORD-TYPE.
01411      MOVE +0003             TO ELCNTL-SEQ-NO.
01412
01413      IF REPORTL GREATER THAN +0
01414         MOVE REPORTI            TO ELCNTL-REPORT
01415      ELSE
01416         MOVE ZEROS              TO ELCNTL-REPORT.
01417
01418      MOVE 'N'                    TO WS-REPORT-FOUND-SW.
01419
01420      
      * EXEC CICS HANDLE CONDITION
01421 *        NOTFND   (7080-END-OF-SEARCH)
01422 *    END-EXEC.
      *    MOVE '"$I                   ! - #00004056' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303034303536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01423
01424      
      * EXEC CICS STARTBR
01425 *        DATASET  (ELCNTL-FILE-ID)
01426 *        RIDFLD   (ELCNTL-KEY)
01427 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004060' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01428
01429      
      * EXEC CICS HANDLE CONDITION
01430 *        NOTFND   (7070-END-OF-BROWSE)
01431 *        ENDFILE  (7070-END-OF-BROWSE)
01432 *    END-EXEC.
      *    MOVE '"$I''                  ! . #00004065' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303034303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01433
01434  7010-READ-FWRD-NEXT-RECORD.
01435      
      * EXEC CICS READNEXT
01436 *        DATASET  (ELCNTL-FILE-ID)
01437 *        RIDFLD   (ELCNTL-KEY)
01438 *        SET      (ADDRESS OF CONTROL-FILE)
01439 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004071' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01440
01441      IF ELCNTL-COMPANY-ID = PI-COMPANY-ID AND
01442         ELCNTL-RECORD-TYPE = 'C'
01443         NEXT SENTENCE
01444      ELSE
01445         GO TO 7070-END-OF-BROWSE.
01446
01447      IF ELCNTL-SEQ-NO = +0002
01448         NEXT SENTENCE
01449      ELSE
01450         GO TO 7010-READ-FWRD-NEXT-RECORD.
01451
01452      MOVE 'Y'                    TO WS-REPORT-FOUND-SW.
01453
01454  7070-END-OF-BROWSE.
01455      
      * EXEC CICS ENDBR
01456 *        DATASET  (ELCNTL-FILE-ID)
01457 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004091' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01458
01459       IF REPORT-WAS-NOT-FOUND
01460          NEXT SENTENCE
01461       ELSE
01462          GO TO 7090-EXIT.
01463
01464  7080-END-OF-SEARCH.
01465      MOVE -1                     TO MAINTL.
01466      MOVE  ER-2237               TO EMI-ERROR.
01467      PERFORM 9900-ERROR-FORMAT.
01468      GO TO 8200-SEND-DATAONLY.
01469
01470  7090-EXIT.
01471      EXIT.
01472
01473      EJECT
01474  7100-BROWSE-BWRD-NEXT-ACCOUNT.
01475      MOVE 'N'                    TO WS-REPORT-FOUND-SW.
01476
01477      MOVE SPACES                 TO ELCNTL-KEY.
01478      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
01479      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.
01480      MOVE +0                     TO ELCNTL-SEQ-NO.
01481
01482      IF REPORTL GREATER THAN +0
01483         MOVE REPORTI            TO ELCNTL-REPORT
01484      ELSE
01485         MOVE PI-LAST-REPORT     TO ELCNTL-REPORT.
01486
01487      
      * EXEC CICS HANDLE CONDITION
01488 *        NOTFND   (7180-END-OF-SEARCH)
01489 *    END-EXEC.
      *    MOVE '"$I                   ! / #00004123' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303034313233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01490
01491      
      * EXEC CICS STARTBR
01492 *        DATASET  (ELCNTL-FILE-ID)
01493 *        RIDFLD   (ELCNTL-KEY)
01494 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004127' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01495
01496      
      * EXEC CICS HANDLE CONDITION
01497 *        NOTFND   (7170-END-OF-BROWSE)
01498 *        ENDFILE  (7170-END-OF-BROWSE)
01499 *    END-EXEC.
      *    MOVE '"$I''                  ! 0 #00004132' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303034313332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01500
01501  7110-READ-FWRD-NEXT-RECORD.
01502      
      * EXEC CICS READNEXT
01503 *        DATASET  (ELCNTL-FILE-ID)
01504 *        RIDFLD   (ELCNTL-KEY)
01505 *        SET      (ADDRESS OF CONTROL-FILE)
01506 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004138' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01507
01508      
      * EXEC CICS READPREV
01509 *        DATASET  (ELCNTL-FILE-ID)
01510 *        RIDFLD   (ELCNTL-KEY)
01511 *        SET      (ADDRESS OF CONTROL-FILE)
01512 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004144' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01513
01514  7110-READ-BWRD-NEXT-RECORD.
01515      
      * EXEC CICS READPREV
01516 *        DATASET  (ELCNTL-FILE-ID)
01517 *        RIDFLD   (ELCNTL-KEY)
01518 *        SET      (ADDRESS OF CONTROL-FILE)
01519 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004151' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01520
01521      IF ELCNTL-COMPANY-ID = PI-COMPANY-ID AND
01522         ELCNTL-RECORD-TYPE = 'C'
01523         NEXT SENTENCE
01524      ELSE
01525         GO TO 7170-END-OF-BROWSE.
01526
01527      IF ELCNTL-SEQ-NO = +0002
01528         NEXT SENTENCE
01529      ELSE
01530         GO TO 7110-READ-BWRD-NEXT-RECORD.
01531
01532      MOVE 'Y'                    TO WS-REPORT-FOUND-SW.
01533
01534  7170-END-OF-BROWSE.
01535      
      * EXEC CICS ENDBR
01536 *        DATASET  (ELCNTL-FILE-ID)
01537 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004171' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01538
01539      IF REPORT-WAS-NOT-FOUND
01540         NEXT SENTENCE
01541      ELSE
01542         GO TO 7190-EXIT.
01543
01544  7180-END-OF-SEARCH.
01545      MOVE -1                     TO MAINTL.
01546      MOVE  ER-2238               TO EMI-ERROR.
01547      PERFORM 9900-ERROR-FORMAT.
01548      GO TO 8200-SEND-DATAONLY.
01549
01550   7190-EXIT.
01551      EXIT.
01552
01553      EJECT
01554  8100-SEND-INITIAL-MAP.
01555      MOVE SAVE-DATE              TO RUNDTEO.
01556      MOVE EIBTIME                TO TIME-IN.
01557      MOVE TIME-OUT               TO RUNTIMEO.
01558      MOVE SPACES                 TO MAINTO.
01559      MOVE -1                     TO MAINTL.
01560      MOVE PI-LIFE-OVERRIDE-L6    TO LBENTPO.
01561      MOVE PI-AH-OVERRIDE-L6      TO ABENTPO.
01562      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
01563 *****MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
01564
01565  8150-SEND-INITIAL-MAP.
01566      
      * EXEC CICS SEND
01567 *        MAP      (MAP-NAME)
01568 *        MAPSET   (MAPSET-NAME)
01569 *        FROM     (EL6041AO)
01570 *        ERASE
01571 *        CURSOR
01572 *    END-EXEC.
           MOVE LENGTH OF
            EL6041AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004202' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6041AO, 
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
           
01573
01574      GO TO 9100-RETURN-TRAN.
01575
01576  8200-SEND-DATAONLY.
01577      MOVE SAVE-DATE              TO RUNDTEO
01578      MOVE EIBTIME                TO TIME-IN.
01579      MOVE TIME-OUT               TO RUNTIMEO
01580      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
01581 *****MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
01582      
      * EXEC CICS SEND
01583 *        MAP      (MAP-NAME)
01584 *        MAPSET   (MAPSET-NAME)
01585 *        FROM     (EL6041AO)
01586 *        DATAONLY
01587 *        CURSOR
01588 *    END-EXEC.
           MOVE LENGTH OF
            EL6041AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004218' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6041AO, 
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
           
01589
01590      GO TO 9100-RETURN-TRAN.
01591
01592      EJECT
01593  8300-SEND-TEXT.
01594      
      * EXEC CICS SEND TEXT
01595 *        FROM     (LOGOFF-TEXT)
01596 *        LENGTH   (LOGOFF-LENGTH)
01597 *        ERASE
01598 *        FREEKB
01599 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004230' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323330' TO DFHEIV0(25:11)
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
           
01600
01601      
      * EXEC CICS RETURN
01602 *    END-EXEC.
      *    MOVE '.(                    &   #00004237' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01603
01604      EJECT
01605  8400-LOG-JOURNAL-RECORD.
01606      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
01607      MOVE ELCNTL-FILE-ID         TO JP-FILE-ID.
01608      MOVE THIS-PGM               TO JP-PROGRAM-ID.
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID     ('CR')
pemuni*        FROM        (JOURNAL-RECORD)
pemuni*        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
pemuni*    END-EXEC.
01615
01616  8400-EXIT.
01617      EXIT.
01618
01619  8600-DEEDIT.
01620      
      * EXEC CICS BIF DEEDIT
01621 *         FIELD   (DEEDIT-FIELD)
01622 *         LENGTH  (15)
01623 *     END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004256' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01624
01625  8800-UNAUTHORIZED-ACCESS.
01626      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01627      GO TO 8300-SEND-TEXT.
01628
01629  8810-PF23.
01630      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01631      MOVE XCTL-005               TO PGM-NAME.
01632      GO TO 9300-XCTL.
01633
01634  9000-RETURN-CICS.
01635      
      * EXEC CICS RETURN
01636 *    END-EXEC.
      *    MOVE '.(                    &   #00004271' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01637
01638  9100-RETURN-TRAN.
01639      MOVE    EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01640      MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.
01641      
      * EXEC CICS RETURN
01642 *        TRANSID    (TRANS-ID)
01643 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01644 *        LENGTH     (PI-COMM-LENGTH)
01645 *    END-EXEC.
      *    MOVE '.(CT                  &   #00004277' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01646
01647  9200-RETURN-MAIN-MENU.
01648
01649      IF  CREDIT-SESSION
01650          MOVE XCTL-EL626         TO PGM-NAME
01651
01652      ELSE
01653          IF  CLAIM-SESSION
01654              MOVE XCTL-EL126     TO PGM-NAME
01655
01656          ELSE
01657              IF  MORTGAGE-SESSION
01658                  MOVE XCTL-EM626 TO PGM-NAME
01659
01660              ELSE
01661                  IF  GENERAL-LEDGER-SESSION
01662                      MOVE XCTL-GL800
01663                                  TO PGM-NAME.
01664
01665      GO TO 9300-XCTL.
01666
01667  9300-XCTL.
01668      
      * EXEC CICS XCTL
01669 *        PROGRAM    (PGM-NAME)
01670 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01671 *        LENGTH     (PI-COMM-LENGTH)
01672 *    END-EXEC.
      *    MOVE '.$C                   $   #00004304' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01673
01674  9400-CLEAR.
01675      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
01676      GO TO 9300-XCTL.
01677
01678  9500-PF12.
01679      MOVE XCTL-010               TO PGM-NAME.
01680      GO TO 9300-XCTL.
01681
01682  9600-PGMID-ERROR.
01683      
      * EXEC CICS HANDLE CONDITION
01684 *        PGMIDERR    (8300-SEND-TEXT)
01685 *    END-EXEC.
      *    MOVE '"$L                   ! 1 #00004319' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303034333139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01686
01687      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01688      MOVE ' '                    TO PI-ENTRY-CD-1.
01689      MOVE XCTL-005               TO PGM-NAME.
01690      MOVE PGM-NAME               TO LOGOFF-PGM.
01691      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01692      GO TO 9300-XCTL.
01693
01694  9700-DATE-LINK.
01695      MOVE LINK-ELDATCV           TO PGM-NAME
01696      
      * EXEC CICS LINK
01697 *        PROGRAM    (PGM-NAME)
01698 *        COMMAREA   (DATE-CONVERSION-DATA)
01699 *        LENGTH     (DC-COMM-LENGTH)
01700 *    END-EXEC.
      *    MOVE '."C                   ''   #00004332' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01701
01702  9900-ERROR-FORMAT.
01703      IF NOT EMI-ERRORS-COMPLETE
01704          MOVE LINK-001           TO PGM-NAME
01705          
      * EXEC CICS LINK
01706 *            PROGRAM    (PGM-NAME)
01707 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01708 *            LENGTH     (EMI-COMM-LENGTH)
01709 *        END-EXEC.
      *    MOVE '."C                   ''   #00004341' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01710
01711  9900-EXIT.
01712      EXIT.
01713
01714  9990-ABEND.
01715      MOVE LINK-004               TO PGM-NAME.
01716      MOVE DFHEIBLK               TO EMI-LINE1
01717
01718      
      * EXEC CICS LINK
01719 *        PROGRAM   (PGM-NAME)
01720 *        COMMAREA  (EMI-LINE1)
01721 *        LENGTH    (72)
01722 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004354' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01723
01724      MOVE -1                     TO MAINTL.
01725      GO TO 8200-SEND-DATAONLY.
01726
01727      EJECT
01728  9910-INITIALIZE-SECURITY.
01729 ******************************************************************
01730 *                                                                *
01731 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
01732 *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *
01733 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
01734 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
01735 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
01736 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
01737 *                                                                *
01738 ******************************************************************
01739
01740      IF  PI-PROCESSOR-ID NOT = 'LGXX'
01741           MOVE 'Y'                     TO PI-DISPLAY-CAP
01742                                           PI-MODIFY-CAP
01743           IF  NOT SYSTEM-DISPLAY-CAP
01744                  MOVE 'READ'         TO SM-READ
01745                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
01746                  MOVE ER-9097        TO EMI-ERROR
01747                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01748                  GO TO 8100-SEND-INITIAL-MAP
01749
01750           ELSE
01751               GO TO 9910-EXIT.
01752
01753  9910-EXIT.
01754      EXIT.
01755
01756  9995-SECURITY-VIOLATION.
01757 *           COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00004410' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
01758
01759  9995-EXIT.
01760       EXIT.
01761

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6041' TO DFHEIV1
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
               GO TO 1100-EDIT-REPORT-SCREEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2890-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3100-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3900-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 5010-NOT-FOUND,
                     5010-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 5030-NOT-FOUND,
                     5030-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 5050-NOT-FOUND,
                     5050-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5070-NOT-FOUND,
                     5070-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 7080-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 7070-END-OF-BROWSE,
                     7070-END-OF-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7180-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 7170-END-OF-BROWSE,
                     7170-END-OF-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6041' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
