00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 ELRATE.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/05/96 16:20:51.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.012
00009
00010 *AUTHOR.     LOGIC, INC.
00011 *            DALLAS, TEXAS.
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
00024
00025 ******************************************************************
00026 *  REMARKS   *                                                   *
00027 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00028 *            *    OPTION SPECIFIED, COMPUTE PREMIUM AMOUNTS      *
00029 *            *    AND RATES.                                     *
00030 ******************************************************************
00031
00032
00033 ******************************************************************
00034 *                                                                *
00035 *              INPUT FIELDS USED                                 *
00036 *                                                                *
00037 ******************************************************************
00038 *  ORIGINAL TERM    - CP-ORIGINAL-TERM    (RATING TERM)          *
00039 *  CLASS CODE       - CP-CLASS-CODE                              *
00040 *  DEVIATION CODE   - CP-DEVIATION-CODE                          *
00041 *  ORIGINAL BENEFIT - CP-ORIGINAL-BENEFIT                        *
00042 *  RATING BENEFIT   - CP-RATING-BENEFIT-AMT (TOT BEN ON BALLOON) *
00043 *  BENEFIT TYPE     - CP-BENEFIT-TYPE                            *
00044 *  STATE CODE (NUM) - CP-STATE                                   *
00045 *  STATE CODE       - CP-STATE-STD-ABBV                          *
00046 *  COMPANY I.D.     - CP-COMPANY-ID                              *
00047 *  COMPANY CD (NUM) - CP-COMPANY-CD                              *
00048 *  INSURED AGE      - CP-ISSUE-AGE                               *
00049 *  RATING METHOD    - CP-EARNING-METHOD                          *
00050 *  SPECIAL METHOD   - CP-SPECIAL-CALC-CODE                       *
00051 *  A.P.R.           - CP-LOAN-APR                                *
00052 *  PAYMENT FREQUENCY- CP-PAY-FREQUENCY                           *
00053 *  CERT ISSUE DATE  - CP-CERT-EFF-DT                             *
00054 *  TERM OR EXT DAYS - CP-TERM-OR-EXT-DAYS                        *
00055 ******************************************************************
00056  ENVIRONMENT DIVISION.
00057
00058  DATA DIVISION.
00059      EJECT
00060  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00061  77  FILLER   PIC X(32) VALUE '********************************'.
00062  77  FILLER   PIC X(32) VALUE '**  ELRATE  WORKING STORAGE   **'.
00063  77  FILLER   PIC X(32) VALUE '***********VMOD 2.012 **********'.
00064
00065 ***  Y2K PROJ 7744
00066  01  WS-HOLD-RATE-EXP.
00067      05  WS-HOLD-RATE-EXP-AL PIC X(11).
00068      05  WS-HOLD-RATE-EXP-DT REDEFINES
00069          WS-HOLD-RATE-EXP-AL PIC 9(11).
00070 ***  Y2K PROJ 7744
00071
00072 *                            COPY ELCRATWS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCRATWS.                           *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.010                          *
00006 *                                                                *
00007 ******************************************************************
00008
00009  01  STANDARD-AREAS.
00010      12  ER-STATUS               PIC XX.
00011      12  WS-BROWSE-SW            PIC X       VALUE SPACE.
00012          88 WS-BROWSE-STARTED                VALUE 'Y'.
00013      12  WS-PREMIUM-RATE         PIC S99V9(5) COMP-3 VALUE ZERO.
00014      12  WS-FARM-TERM            PIC S9(3)    COMP-3 VALUE ZERO.
00015      12  OERATE-FILE-ID          PIC X(8)    VALUE 'OERATE'.
00016      12  ERRATE-FILE-ID          PIC X(8)    VALUE 'ERRATE'.
00017      12  ERRATE-KEY.
00018          16  RATE-COMPANY-CD     PIC X        VALUE SPACE.
00019          16  RATE-STATE-CODE.
00020              20  RATE-ST-CODE    PIC  XX      VALUE SPACES.
00021              20  RATE-ST-CLASS   PIC  XX      VALUE SPACES.
00022              20  RATE-ST-DEV     PIC  XXX     VALUE SPACES.
00023          16  RATE-L-AH-CODE.
00024              20  RATE-L-AH       PIC  X       VALUE SPACE.
00025              20  RATE-LAH-NUM    PIC  XX      VALUE ZEROS.
00026          16  RATE-LIMITS.
00027              20  RATE-HIGH-AGE   PIC  99      VALUE ZEROS.
00028              20  RATE-HIGH-AMT   PIC  9(6)    VALUE ZEROS.
00029              20  RATE-FUTURE     PIC  XX      VALUE SPACES.
00030              20  RATE-SEX        PIC  X       VALUE '9'.
00031          16  RATE-EXPIRY-DATE    PIC 9(11)    COMP-3.
00032
00033      12  SAVE-ERRATE-KEY.
00034          16  SVRT-COMPANY-CD     PIC X        VALUE SPACE.
00035          16  SVRT-STATE-CODE.
00036              20  SVRT-ST-CODE    PIC  XX      VALUE SPACES.
00037              20  SVRT-ST-CLASS   PIC  XX      VALUE SPACES.
00038              20  SVRT-ST-DEV     PIC  XXX     VALUE SPACES.
00039          16  SVRT-L-AH-CODE.
00040              20  SVRT-L-AH       PIC  X       VALUE SPACE.
00041              20  SVRT-LAH-NUM    PIC  XX      VALUE ZEROS.
00042          16  SVRT-LIMITS.
00043              20  SVRT-HIGH-AGE   PIC  99      VALUE ZEROS.
00044              20  SVRT-HIGH-AMT   PIC  9(6)    VALUE ZEROS.
00045              20  SVRT-FUTURE     PIC  XX      VALUE SPACES.
00046              20  SVRT-SEX        PIC  X       VALUE '9'.
00047          16  SVRT-EXPIRY-DATE    PIC  9(11)   COMP-3.
00048
00049      12  WS-SIN-PREM-RATE        PIC S99V9(07) VALUE +0.
00050      12  WS-TERM-MINUS-ONE       PIC S9(5)    COMP-3 VALUE +0.
00051
00052  01  CSL-WORK-AREAS          COMP-3.
00053      12  WK-C                PIC S9(3)V9(9)     VALUE ZEROS.
00054      12  WK-D                PIC S9(4)V9(9)     VALUE ZEROS.
00055      12  WK-J                PIC S999           VALUE ZEROS.
00056      12  WK-K                PIC S999           VALUE ZEROS.
00057      12  WK-L                PIC S99V9(9)       VALUE ZEROS.
00058      12  WK-S                PIC S999V9(4)      VALUE ZEROS.
00059      12  WK-T                PIC S999           VALUE ZEROS.
00060      12  WK-T1               PIC S999           VALUE ZEROS.
00061      12  WK-U                PIC S99V9(9)       VALUE ZEROS.
00062      12  WK-PREM             PIC S9(6)V999      VALUE ZEROS.
00063      12  WK-PAY-AMT          PIC S9(7)V99       VALUE ZEROS.
00064      12  WK-LEV-RATE         PIC S99V9(7)       VALUE ZEROS.
00065      12  WK-RED-RATE         PIC S99V9(7)       VALUE ZEROS.
00066      12  WK-DAILY-RATE       PIC S99V9(7)       VALUE ZEROS.
00067      12  WK-MONTHLY-RATE     PIC S99V9(7)       VALUE ZEROS.
00068      12  WK-MONTHLY-RATE-T   PIC S99V9(7)       VALUE ZEROS.
00069      12  WK-MONTHLY-RATE-J   PIC S99V9(7)       VALUE ZEROS.
00070      12  WS-MAX-TOT-BEN      PIC S9(7)V99       VALUE ZEROS.
00071      12  WS-MAX-MON-BEN      PIC S9(7)V99       VALUE ZEROS.
00072      12  WS-NUM-PAYMTS       PIC S9(5)          VALUE ZEROS.
00073      12  WS-PAY-SCH          PIC S99            VALUE ZEROS.
00074
00075  01  TEXAS-REG-WORK-AREAS.
00076      12  TEX-FACT-1          PIC S9(7)V99    COMP-3.
00077      12  TEX-FACT-2          PIC S9(3)       COMP-3.
00078      12  TEX-FACT-3          PIC S9(3)       COMP-3.
00079      12  TEX-FACT-4          PIC S9(7)       COMP-3.
00080      12  TEX-FACT-5          PIC S9(3)       COMP-3.
00081      12  TEX-FACT-6          PIC S9(3)       COMP-3.
00082      12  TEX-FACT-7          PIC S9(7)       COMP-3.
00083      12  TEX-FACT-8          PIC S9V9(6)     COMP-3.
00084      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.
00085
00086  01  NET-PAY-INTERFACE.
00087      12  N-P-APR             PIC S9(3)V9(4)  COMP-3.
00088      12  N-P-ORIG            PIC S9(3)       COMP-3.
00089      12  N-P-REM             PIC S9(3)       COMP-3.
00090      12  N-P-OPT             PIC X.
00091      12  N-P-LOAN            PIC S9(3)       COMP-3.
00092      12  N-P-FACTOR          PIC S9(4)V9(9)  COMP-3.
00093
00073
00074      EJECT
00075 *                            COPY ERCNETWS.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ERCNETWS                           *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                         *
00006 *                                                               *
00007 *    WORKING STORAGE TO USE WITH ERCNETP - NET PAY CALCULATIONS *
00008 *                                                               *
00009 *****************************************************************.
060909*                   C H A N G E   L O G
060909*
060909* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060909*-----------------------------------------------------------------
060909*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060909* EFFECTIVE    NUMBER
060909*-----------------------------------------------------------------
060909* 060909  CR2008042300002  PEMA  ADD NEW FARM PLAN CALC
101509* 101509  IR2009100700002  PEMA  INCREASE DECIMAL POSITIONS
071910* 071910  IR2010062900001  PEMA  ADD MN NET BALLOON REFUND CALC
071511* 071511  CR2011021500001  PEMA  ADD CODE FOR DCC 10C
050713* 050713  CR2008052200001  PEMA  ADD CODE FOR 0 PCT APR
060909******************************************************************
00010
00011  01  NET-PAY-WORK-AREA.
00012      12  OPTION-SW               PIC X   VALUE 'X'.
00013         88  NPO-STD         VALUE SPACE.
00014         88  NPO-ALT         VALUE 'A'.
00015         88  NPO-SIMPLE      VALUE 'S'.
00016         88  NPO-2MO         VALUE 'I'.
00017         88  NPO-3MO         VALUE 'J'.
00018         88  NPO-4MO         VALUE 'K'.
00019         88  NPO-TRUNC       VALUE 'T' 'U' 'V' 'W' 'X'.
00020         88  NPO-TRUNC-0     VALUE 'T'.
00021         88  NPO-TRUNC-1     VALUE 'U'.
00022         88  NPO-TRUNC-2     VALUE 'V'.
00023         88  NPO-TRUNC-3     VALUE 'W'.
00024         88  NPO-TRUNC-4     VALUE 'X'.
00025         88  NPO-REFUND      VALUE 'R'.
00026         88  NPO-PRUDENTIAL  VALUE 'P'.
00027
00028      12  TYPE-SW             PIC X   VALUE 'N'.
00029         88  NET-STD             VALUE 'N'.
00030         88  NET-SMP             VALUE 'S'.
00031
00032      12  NP-PROCESS-SW       PIC X   VALUE '1'.
00033         88  NP-RATING           VALUE '1'.
00034         88  NP-REFUND           VALUE '2'.
00035         88  NP-REMAIN-AMT       VALUE '3'.
00036
00037      12  COMP-3-WORK-AREA         COMP-3.
00038          16  WS-WORK-DIS-RATE PIC S99V9(06)    VALUE +0.
00039          16  V               PIC SV9(13)       VALUE +.0.
011904         16  J               PIC SV9(9)        VALUE +0.
011904         16  K               PIC SV9(9)        VALUE +0.
00040          16  I               PIC SV9(10)       VALUE +.0.
011907         16  D               PIC S9(5)         VALUE +.0.
00041          16  APR100          PIC SV9(9)        VALUE +.0.
00042          16  RA              PIC S9(6)V9(9)    VALUE +.0.
00043          16  WK1             PIC S9(4)V9(13)   VALUE +.0.
00044          16  WK2             PIC S9(4)V9(13)   VALUE +.0.
               16  pema            pic s9(2)v9(15)   value +0.
00045          16  WK3             PIC S9(7)V9(10)   VALUE +.0.
00046          16  WK4             PIC S9(7)V9(8)    VALUE +.0.
00047          16  WK5             PIC S9(5)V9(13)   VALUE +.0.
00048          16  WK5-CSL         PIC S9(9)V9(6)    VALUE +.0.
00049          16  WK6             PIC S9(7)V9(10)   VALUE +.0.
00050          16  WK7             PIC S9(7)V9(10)   VALUE +.0.
00051          16  WK8             PIC S9(3)V9(10)   VALUE +.0.
00052          16  WK9             PIC S9(3)V9(10)   VALUE +.0.
00053          16  GR              PIC S9(7)V9(10)   VALUE +.0.
00054          16  ONE-PLUS-I      PIC S9(3)V9(10)   VALUE +.0.
00055          16  ODF             PIC S9(3)V9(10)   VALUE +.0.
00056          16  OD              PIC S9(3)V9(10)   VALUE +.0.
00057          16  FM              PIC S9(3)V9(10)   VALUE +.0.
00058          16  FN              PIC S9(3)V9(10)   VALUE +.0.
00059          16  FNM             PIC S9(3)V9(10)   VALUE +.0.
00060          16  ANGLEM          PIC S9(5)V9(13)   VALUE +.0.
040615         16  ANGLEMY         PIC S9(5)V9(13)   VALUE +.0.
040615         16  ANGLEMP1        PIC S9(5)V9(13)   VALUE +.0.
00061          16  ANGLEN          PIC S9(5)V9(13)   VALUE +.0.
00061          16  ANGLENY         PIC S9(5)V9(13)   VALUE +.0.
00062          16  ANGLEN-M        PIC S9(5)V9(13)   VALUE +.0.
040615         16  ANGLEN-MP1      PIC S9(5)V9(13)   VALUE +.0.
040615         16  ANGLEN-MP1Y     PIC S9(5)V9(13)   VALUE +.0.
00063          16  ANGLEN-1        PIC S9(7)V9(10)   VALUE +.0.
00064          16  ANGLEM-1        PIC S9(7)V9(10)   VALUE +.0.
040615         16  ANGLEM-1Y       PIC S9(7)V9(10)   VALUE +.0.
071910         16  ANGLEM-T        PIC S9(5)V9(13)   VALUE +.0.
040615         16  PVBALLOON       PIC S9(7)V99      VALUE +.0.
00065          16  TRUNC-PMT       PIC S9(7)V99      VALUE +.0.
00066          16  K1              PIC S999          VALUE +1.
00067          16  K12             PIC S999          VALUE +12.
00068          16  K100            PIC S999          VALUE +100.
00069          16  K1000           PIC S9(7)         VALUE +1000.
00070          16  NC-LR           PIC S9(2)V9(5)    VALUE +.0.
               16  Y REDEFINES NC-LR
                                   PIC S99V9(5).
060909         16  TI              PIC S999V9(5)     VALUE +0.
00071          16  NC-R            PIC S9(7)V9(10)   VALUE +.0.
00072          16  NC-OB           PIC S9(7)V9(10)   VALUE +.0.
00073          16  NC-P            PIC S9(7)V9(10)   VALUE +.0.
00074          16  NC-D            PIC S9(7)V9(10)   VALUE +.0.
00075          16  NC-BIG-D        PIC S9(3)         VALUE +030.
00076          16  NC-LITTLE-D     PIC S9(2)V9(7)    VALUE +.0.
00077          16  NC-MP           PIC S9(5)V9(2)    VALUE +.0.
00078          16  NC-LP           PIC S9(5)V9(2)    VALUE +.0.
00079
00080          16  CA-MP           PIC S9(5)V9(2)    VALUE +.0.
00081          16  CA-API          PIC S9(2)V9(8)    VALUE +.0.
00082          16  CA-APJ          PIC S9(2)V9(8)    VALUE +.0.
00083          16  CA-VI           PIC S9(2)V9(8)    VALUE +.0.
00084          16  CA-J            PIC S9(2)V9(8)    VALUE +.0.
00085          16  CA-DISCOUNT     PIC S9(2)V9(8)    VALUE +.042000.
00086          16  GTL-IA-DIVIDEND PIC S9(5)V9(8)    VALUE +.0.
00087          16  GTL-IA-DIVISOR  PIC S9(5)V9(8)    VALUE +.0.
00088          16  GTL-IA-FACTOR   PIC S9(5)V9(8)    VALUE +.0.
00089          16  GTL-IA-ODF      PIC S9(5)V9(8)    VALUE +.0.
00090          16  GTL-IA-ODD-DAYS PIC S999          VALUE +.0.
00091          16  GTL-IA-SGN      PIC S9            VALUE +.0.
00092          16  GTL-IA-ABS      PIC 999           VALUE ZEROS.
00093
00094          16  ANNUAL-INT-RATE PIC S9(3)V9(4)    VALUE +.0.
00095
00096          16  ORIGINAL-TERM   PIC S999.
00097          16  M              REDEFINES ORIGINAL-TERM
00098                              PIC S999.
00099
00100          16  REMAINING-TERM  PIC S999.
00101          16  R              REDEFINES REMAINING-TERM
00102                              PIC S999.
00103
00104          16  LOAN-TERM       PIC S999.
00105          16  N              REDEFINES LOAN-TERM
00106                              PIC S999.
00107
00108          16  EXPIRED-TERM    PIC S999.
00109          16  E              REDEFINES EXPIRED-TERM
00110                              PIC S999.
00111
00112          16  PRUDENTIAL-WORK-AMT
00113                              PIC S9(7)V9(8).
00114          16  PWK            REDEFINES PRUDENTIAL-WORK-AMT
00115                              PIC S9(7)V9(8).
00116
00117          16  FACTOR          PIC S9(4)V9(9)    VALUE +0.0.
00118          16  VX              PIC S9V9(13)      VALUE +0.0.
00119          16  SV              PIC S9V9(13)      VALUE +0.0.
00120          16  SV-MINUS-ONE    PIC S999V9(09)    VALUE +0.0.
00121          16  SX              PIC S9V9(13)      VALUE +0.0.
00122          16  SE              PIC S9V9(10)      VALUE +0.0.
00123          16  N2              PIC S9(7)         VALUE +0.
00124          16  N3              PIC S9(7)         VALUE +0.
00125          16  K-I             PIC S9V9(8)       VALUE +0.0.
00126
00127      12  BINARY-WORK-AREA        COMP.
00128          16  X1              PIC S999        VALUE +0.
00129          16  X2              PIC S999        VALUE +0.
00130          16  MAX-X           PIC S9(5)       VALUE +0.
00131          16  B1              PIC S9(5)       VALUE +1.
00132          16  LINDX           PIC S999        VALUE ZEROS.
00133          16  AHNDX           PIC S999        VALUE ZEROS.
00134
           12  CSO-RATE-WORK-AREAS.
071511         16  C-PMTS-PER-YEAR     PIC S999V99 COMP-3 VALUE ZEROS.
071511         16  C-DAYS-IN-PMT-PER   PIC S999 COMP-3 VALUE ZEROS.
071511         16  C-TOT-PMTS          PIC S999 COMP-3 VALUE ZEROS.
00135      12  WS-WORK-MISC.
               16  ws-extra-pmts       pic s999         comp-3.
00136          16  NET-BEN             PIC S9(9)V9999   COMP-3.
00137          16  WS-RATE-TERM        PIC S9(3)        COMP-3.
00138 *        16  WS-AGE              PIC 99           VALUE ZEROS.
               16  WS-AGE              PIC S9(3)V99 COMP-3 VALUE +0.
00139          16  WS-AH-FACE-BENEFIT  PIC S9(7)        COMP-3.
00140          16  WS-COUNTER          PIC S9(3)    COMP-3  VALUE +0.
00141          16  WS-WORK-RATE        PIC S9(2)V9(8) COMP-3 VALUE +0.
00142          16  WS-DISCOUNT-OPTION  PIC X VALUE SPACES.
00143          16  WS-DUE-DATE         PIC S9(7) COMP-3 VALUE +0.
00144          16  WS-INT-BEGIN-DATE   PIC S9(7) COMP-3 VALUE +0.
00145          16  WS-X-Y-FACTOR       PIC S9(3)V9(15) COMP-3 VALUE +0.
00146          16  WS-Y-X-FACTOR       PIC S9(3)V9(15) COMP-3 VALUE +0.
00147          16  WS-ODD-DAY-UNITS    PIC S9(3)V9(15) COMP-3 VALUE +0.
00148          16  WS-CALC-FREQ-RATE   PIC S9(2)V9(15) COMP-3 VALUE +0.
00149          16  WS-NP-FACTOR        PIC S9(5)V9(10) COMP-3 VALUE +0.
00150 ******************************************************************
00076
00077      EJECT
00078 *                            COPY ELCDATE.
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
00079
00080      EJECT
00081 *                            COPY ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
040615* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
012820* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
012820                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
012820       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
092310           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
                 88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
               16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
                                         PIC S9(5)V99 COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
               16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
                                        PIC S9(7)V99 COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
               16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-SPEC-CALC      PIC X.
                   88  CP-CALC-GROSS-FEE        VALUE 'G'.
                   88  CP-CALC-CLP              VALUE 'C'.
               16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
               16  CP-CANCEL-REASON      PIC X.
               16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-PMT-MODE           PIC X.
               16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
071211         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
               16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
071211         16  FILLER                PIC X.
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
010716         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
092310           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
092310           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
                 88  CP-R-AS-REPOSSESSION                VALUE 'R'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
               16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
040615         16  cp-extra-periods      pic 9 value zeros.
070115         16  cp-net-only-state     pic x value spaces.
041710         16  FILLER                PIC X(13).
00514 ******************************************************************
00082
00083      EJECT
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
00085  01  DFHCOMMAREA             PIC X(450).
00086
00087      EJECT
00088 *01 PARMLIST              COMP.
00089 *    02  FILLER              PIC S9(8).
00090 *    02  ERRATE-POINTER      PIC S9(8).
00091
00092 *                                COPY ERCRATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCRATE                             *
00003 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00004 *                            VMOD=2.008                          *
00005 *                                                                *
00006 *   ONLINE CREDIT SYSTEM                                         *
00007 *                                                                *
00008 *   FILE DESCRIPTION = RATES MASTER FILE                         *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 1765  RECFORM = FIXED                          *
00012 *                                                                *
00013 *   BASE CLUSTER NAME = ERRATE                   RKP=2,LEN=28    *
00014 *       ALTERNATE PATH = NONE                                    *
00015 *                                                                *
00016 *   LOG = NO                                                     *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 *                                                                *
00019 ******************************************************************
010716******************************************************************
010716*                   C H A N G E   L O G
010716*
010716* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010716*-----------------------------------------------------------------
010716*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010716* EFFECTIVE    NUMBER
010716*-----------------------------------------------------------------
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
012820* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
010716******************************************************************
00020
00021  01  RATE-RECORD.
00022      12  RT-RECORD-ID                      PIC XX.
00023          88  VALID-RT-ID                      VALUE 'RT'.
00024
00025      12  RT-CONTROL-PRIMARY.
00026          16  RT-COMPANY-CD                 PIC X.
00027          16  RT-STATE-CODE.
00028              20  RT-ST-CODE                PIC XX.
00029              20  RT-ST-CLASS               PIC XX.
00030              20  RT-ST-DEV                 PIC XXX.
00031          16  RT-L-AH-CODE.
00032              20  RT-L-AH                   PIC X.
00033              20  RT-LAH-NUM                PIC XX.
00034          16  RT-LIMITS.
00035              20  RT-HIGH-AGE               PIC 99.
00036              20  RT-HIGH-AMT               PIC 9(6).
00037              20  RT-FUTURE                 PIC XX.
00038              20  RT-SEX                    PIC X.
00039          16  RT-EXPIRY-DATE                PIC 9(11)  COMP-3.
00043
00044      12  RT-MAINT-INFORMATION.
00045          16  RT-LAST-MAINT-DT              PIC XX.
00046          16  RT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00047          16  RT-LAST-MAINT-USER            PIC X(4).
00048          16  FILLER                        PIC X(10).
00049
00050      12  RT-STRUCTURE-COMMENT              PIC X(50).
00051      12  RT-RATE-COMMENT                   PIC X(50).
00052
00053      12  CSL-RESERVED                      PIC X(10).
00054      12  FILLER                            PIC X(12).
00055
00056      12  RT-MAX-AGE                        PIC 99.
00057
00058      12  RT-LIFE-LIMS-FLDS.
00059          16  RT-LIFE-MORT-CODE             PIC X(4).
00060          16  RT-LIFE-EXCEPTIONS   OCCURS 8 TIMES.
00061              20  RT-L-EX-AGE               PIC 99.
00062              20  RT-L-EX-TERM              PIC S999       COMP-3.
00063              20  RT-L-EX-FACE              PIC S9(7)      COMP-3.
012820         16  RT-LIFE-TERM-MINS OCCURS 8.
012820             20  RT-L-MIN-TERM             PIC S999       COMP-3.
012820         16  FILLER                        PIC X(4).
012820*        16  FILLER                        PIC X(20).
00065
00066      12  RT-AH-LIMS-FLDS   REDEFINES   RT-LIFE-LIMS-FLDS.
00067          16  RT-AH-EXCEPTIONS   OCCURS 8 TIMES.
00068              20  RT-AH-AGE                 PIC 99.
00069              20  RT-AH-TERM                PIC S999       COMP-3.
00070              20  RT-AH-BEN-M               PIC S9(5)      COMP-3.
00071              20  RT-AH-BEN-F               PIC S9(7)      COMP-3.
00072
00073      12  RT-LIFE-RATES.
00074          16  RT-L-RATE  OCCURS 360 TIMES   PIC S99V9(5)   COMP-3.
00075
00076      12  RT-AH-RATES   REDEFINES   RT-LIFE-RATES.
00077          16  RT-AH-RATE  OCCURS 360 TIMES  PIC S99V9(5)   COMP-3.
00078
00079      12  RT-DAILY-RATE                     PIC S99V9(5)   COMP-3.
00080
00081      12  RT-DISCOUNT-OPTION                PIC X.
00082          88  RT-DO-NOT-USE                     VALUE ' '.
00083          88  RT-USE-DISCOUNT-FACTOR            VALUE '1'.
00084          88  RT-USE-APR-AS-DISCOUNT            VALUE '2'.
00085
00086      12  RT-DISCOUNT-RATE                  PIC S99V9(5)   COMP-3.
00087      12  RT-DISCOUNT-OB-RATE               PIC S99V9(5)   COMP-3.
00088
00089      12  RT-COMPOSITE-OPTION               PIC X.
00090          88  RT-NO-COMPOSITE                   VALUE ' '.
00091          88  RT-USE-COMPOSITE-RATE             VALUE '1'.
00092
00093      12  RT-COMPOSITE-RATE                 PIC S99V9(5)   COMP-3.
00094
010716     12  RT-CANCEL-FEE                     PIC S9(3)V99   COMP-3.
00096      12  FILLER                            PIC X(13).
00097
00098      12  RT-TYPE-RATE                      PIC X.
00099          88  RT-IS-STND                        VALUE ' ' 'S'.
00100          88  RT-IS-OB                          VALUE 'O'.
00101
00102      12  RT-SRT-ALPHA                      PIC X.
00103
00104      12  RT-CONTROL-2.
00105          16  RTC-1                         PIC X(7).
00106          16  RTC-3                         PIC X(11).
00107          16  RTC-4                         PIC 9(11) COMP-3.
00108          16  RTC-2                         PIC X(3).
00109 ******************************************************************
00093      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA RATE-RECORD.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELRATE' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00095
00096      MOVE DFHCOMMAREA            TO  CALCULATION-PASS-AREA.
00097
00098  000-START-PREMIUM-CALC.
00099      MOVE ZERO                   TO CP-RETURN-CODE
00100                                     CP-CALC-PREMIUM
00101                                     CP-PREMIUM-RATE
020816                                    CP-cancel-FEE.
00103
00104      MOVE SPACES                 TO CP-MORTALITY-CODE.
00105      MOVE ALL '9'                TO ERRATE-KEY.
00106
00107 *    IF CP-RATE-FILE = 'O'
00108 *        MOVE OERATE-FILE-ID     TO ERRATE-FILE-ID.
00109
00110      MOVE CP-COMPANY-CD          TO RATE-COMPANY-CD.
00111      MOVE CP-STATE               TO RATE-ST-CODE.
00112      MOVE CP-CLASS-CODE          TO RATE-ST-CLASS.
00113      MOVE CP-DEVIATION-CODE      TO RATE-ST-DEV.
00114      MOVE CP-ISSUE-AGE           TO RATE-HIGH-AGE.
00115
00116      IF CP-RATING-BENEFIT-AMT NOT NUMERIC  OR
00117         CP-RATING-BENEFIT-AMT = ZEROS
00118          MOVE CP-ORIGINAL-BENEFIT    TO RATE-HIGH-AMT
00119      ELSE
00120          MOVE CP-RATING-BENEFIT-AMT  TO RATE-HIGH-AMT.
00121
00122      IF CP-AH
00123          MOVE CP-AH-OVERRIDE-CODE     TO RATE-L-AH
00124      ELSE
00125          MOVE CP-LIFE-OVERRIDE-CODE   TO RATE-L-AH.
00126
00127      MOVE CP-BENEFIT-CD          TO RATE-LAH-NUM.
00128      MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1.
00129      MOVE SPACE                  TO DC-OPTION-CODE.
00130
00131      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
00132
00133 *** Y2K, PROJ 7744
00134      IF NO-CONVERSION-ERROR
00135          MOVE ZEROS                 TO RATE-EXPIRY-DATE
00136                                        WS-HOLD-RATE-EXP-DT
00137          MOVE DC-GREG-DATE-CYMD     TO WS-HOLD-RATE-EXP-AL(4:8)
00138          MOVE WS-HOLD-RATE-EXP-DT   TO RATE-EXPIRY-DATE
00139          MOVE ERRATE-KEY            TO SAVE-ERRATE-KEY
00140      ELSE
00141          MOVE '2'                   TO CP-RETURN-CODE
00142          GO TO 9999-CALC-RATE-PREM-X
00143      END-IF.
00144 *** Y2K, PROJ 7744
00145
00146      
      * EXEC CICS HANDLE CONDITION
00147 *        NOTFND   (9999-RATE-NOTFND)
00148 *        NOTOPEN  (9999-RATE-NOTOPEN)
00149 *        ENDFILE  (9999-RATE-NOTFND)
00150 *    END-EXEC.
      *    MOVE '"$IJ''                 ! " #00001430' TO DFHEIV0
           MOVE X'2224494A2720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00151
00152  0050-RATE-START-BROWSE.
00153      
      * EXEC CICS STARTBR
00154 *        DATASET  (ERRATE-FILE-ID)
00155 *        RIDFLD   (ERRATE-KEY)
00156 *        GTEQ
00157 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001437' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRATE-FILE-ID, 
                 ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00158
00159      MOVE 'Y'                  TO WS-BROWSE-SW.
00160
00161  0100-READ-RATE-LOOP.
00162      
      * EXEC CICS READNEXT
00163 *        DATASET  (ERRATE-FILE-ID)
00164 *        SET      (ADDRESS OF RATE-RECORD)
00165 *        RIDFLD   (ERRATE-KEY)
00166 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001446' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRATE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00167
00168  0110-CHECK-KEYS.
120202*                            COPY ELCRATPD.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCRATPD                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.061                          *
00006 *                                                                *
00007 ******************************************************************
120202******************************************************************
120202*                   C H A N G E   L O G
120202*
120202* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120202*-----------------------------------------------------------------
120202*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120202* EFFECTIVE    NUMBER
120202*-----------------------------------------------------------------
120202* 120202    2001061800003  PEMA  ADD DCC PROCESSING
120502* 120502                   PEMA  ADD SPECIAL IN CALC FOR CID
032603* 032603                   PEMA  ADD SPECIAL AZ CALC
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
120403* 120403                   PEMA  ADD SPECIAL TX CALC
092205* 092205                   PEMA  FIX CRIT PER FOR DCC
011707* 011707                   PEMA  ADD SC NP PLUS 6 PMTS
021207* 021207                   PEMA  ADD PA NP
022107* 022107                   PEMA  ADD NH NP AND NP TRUNC
032807* 032807                   PEMA  ADD VT NP PLUS 2
052808* 052808    2008032000002  PEMA  ADD CODE FOR AK NET PAY
052808* 052808    2008050800003  PEMA  ADD CODE FOR NV NET +2
060909* 060909    2008042300002  PEMA  ADD NEW FARM PLAN CALC
010410* 010410    2008021200005  PEMA  ADD MN NET PAY BALLOON
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
040610* 040610  CR2010010400006  PEMA  ADD IN TO VT CODE
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
071211* 071211    2010012700001  PEMA  ADD SPPDD
071511* 071511    2011021500001  PEMA  ADD CODE FOR DCC 10C
092311* 092311  IR2011092100001  PEMA  ID 10C MONTHLY
112911* 112911  CR2011083000003  PEMA  ADD SPPDDF TRUNCATED
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
111212* 111212  CR2012013100001  PEMA  CHANGES FOR MN AND WA
122613* 122613  CR2011021500001  PEMA  ADD CODE 4 TRUNC BALLOON DCC 10C
050713* 050713  CR2008042200001  PEMA  ADD CODE FOR ZERO APR
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
021519* 021519  CR2019021100001  PEMA  non-monthly al calcs
072419* 072419  IR2019072400001  PEMA  Initialize pmt mode(AL)
012820* 012820  CR2020012800001  PEMA  ADD EDIT FOR MINIMUM TERMS
032020* 032020  CR2020032000001  PEMA  Use larger field for mon pmt
120202******************************************************************
00008
00009  0110-CHECK-KEY.
00010      IF SVRT-COMPANY-CD = RT-COMPANY-CD  AND
00011         SVRT-ST-CODE    = RT-ST-CODE     AND
00012         SVRT-ST-CLASS   = RT-ST-CLASS    AND
00013         SVRT-ST-DEV     = RT-ST-DEV      AND
00014         SVRT-L-AH       = RT-L-AH        AND
00015         SVRT-LAH-NUM    = RT-LAH-NUM
00016          NEXT SENTENCE
00017      ELSE
00018          GO TO 9999-RATE-NOTFND.
00019
00020      IF  SVRT-HIGH-AGE GREATER RT-HIGH-AGE
00021          GO TO 9999-RATE-NOTFND.
00022
00023      IF SVRT-HIGH-AMT NOT LESS THAN RT-HIGH-AMT
00024         GO TO 0100-READ-RATE-LOOP.
00025
00026      IF SVRT-EXPIRY-DATE GREATER RT-EXPIRY-DATE
00027          GO TO 0100-READ-RATE-LOOP.
00028
00029      IF SVRT-EXPIRY-DATE = RT-EXPIRY-DATE
00030          GO TO 0100-READ-RATE-LOOP.
00031
00032      IF RT-MAX-AGE = ZEROS
00033          NEXT SENTENCE
00034      ELSE
00035          COMPUTE WS-AGE =
00036              (CP-ISSUE-AGE + (CP-ORIGINAL-TERM / 12))
00037          IF WS-AGE >= RT-MAX-AGE
00038              MOVE 'B'               TO CP-RETURN-CODE.
00039
00040      IF CP-LOAN-TERM NOT NUMERIC
00041          MOVE +0                    TO CP-LOAN-TERM.
00042
00043      IF CP-LOAN-TERM = ZERO
00044          MOVE CP-ORIGINAL-TERM      TO CP-LOAN-TERM.
00045
00046      IF CP-TERM-OR-EXT-DAYS NOT NUMERIC
00047          MOVE +0                    TO CP-TERM-OR-EXT-DAYS.
00048
00049      MOVE CP-R-MAX-MON-BEN          TO WS-MAX-MON-BEN.
00050      MOVE CP-R-MAX-TOT-BEN          TO WS-MAX-TOT-BEN.
00051
00052      IF CP-AH
00053          GO TO 2000-TYPE-IS-AH.
00054
      *    display ' ben type           ' cp-benefit-type
      *    display ' earn meth          ' cp-earning-method
      *    display ' process type       ' cp-process-type
      *    display ' special            ' cp-special-calc-cd
      *    display ' ben code           ' cp-benefit-cd
      *    display ' ext days calc      ' cp-ext-days-calc
      *    display ' rating meth        ' cp-rating-method
      *    display ' orig ben           ' cp-original-benefit
022613     evaluate true
022613        when (rt-is-ob)
022613           and (cp-company-id not = 'CID')
022613              move cp-loan-term  to ws-rate-term
020816        when (cp-company-id = 'DCC' OR 'VPP')
022613           and (cp-benefit-cd (1:1) = 'P')
022613           move cp-loan-term     to ws-rate-term
022613        when other
022613           move cp-original-term to ws-rate-term
022613     end-evaluate
022613
022613*    IF ((RT-IS-OB)
022613*       AND (CP-COMPANY-ID NOT EQUAL 'CID'))
022613*                     OR
022613*       ((CP-COMPANY-ID = 'DCC')
022613*       AND (CP-BENEFIT-CD (1:1) = 'P'))
022613*       MOVE CP-LOAN-TERM        TO WS-RATE-TERM
022613*    ELSE
022613*       MOVE CP-ORIGINAL-TERM TO WS-RATE-TERM
022613*    END-IF
00059
00061      MOVE CP-CERT-EFF-DT            TO DC-BIN-DATE-1.
00062      MOVE SPACE                     TO DC-OPTION-CODE.
00063
00064      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
00065
00066  1000-TYPE-IS-LIFE.
00067      MOVE RT-MAX-AGE                TO CP-R-MAX-ATT-AGE.
00068
00069      MOVE ZERO                      TO CP-R-MAX-AGE
00070                                        CP-R-MAX-TERM
00071                                        CP-R-MAX-MON-BEN
00072                                        CP-R-MAX-TOT-BEN.
00073
00074      MOVE RT-LIFE-MORT-CODE         TO CP-MORTALITY-CODE.
00075
020816     IF RT-CANCEL-FEE NUMERIC
020816        MOVE RT-CANCEL-FEE          TO CP-CANCEL-FEE.
00078
00079      MOVE RT-DISCOUNT-OPTION     TO WS-DISCOUNT-OPTION.
00080
           if rt-discount-ob-rate not numeric
              move zeros               to rt-discount-ob-rate
           end-if
00081      IF (RT-DISCOUNT-OPTION EQUAL
00082                      '1' OR '2' OR '3' OR '4' OR '5') AND
00083         (RT-DISCOUNT-OB-RATE NUMERIC)         AND
00084         (RT-DISCOUNT-OB-RATE GREATER THAN +0)
00085          NEXT SENTENCE
00086      ELSE
00087          IF (RT-DISCOUNT-OPTION EQUAL '6') AND
00088             (CP-STATE-STD-ABBRV EQUAL 'CA')
00089              IF RT-DISCOUNT-RATE GREATER THAN ZERO
00090                  COMPUTE CA-DISCOUNT = RT-DISCOUNT-RATE / 100
00091                  GO TO 1050-REGULAR-RATES
00092              ELSE
00093                  GO TO 1050-REGULAR-RATES
00094          ELSE
00095              GO TO 1050-REGULAR-RATES.
00096
      *    display ' discount option ' rt-discount-option
00097      IF RT-DISCOUNT-RATE NOT NUMERIC
00098         MOVE +0                  TO RT-DISCOUNT-RATE.
00099
00100      IF CP-ORIGINAL-TERM GREATER ZERO AND LESS 361
00101         NEXT SENTENCE
00102      ELSE
00103         MOVE +0                  TO CP-PREMIUM-RATE
00104                                     CP-COMPOSITE-RATE
00105                                     CP-CALC-PREMIUM
00106          MOVE '4'                TO CP-RETURN-CODE
00107          GO TO 9999-CALC-RATE-PREM-X.
00108
00109      IF RT-DISCOUNT-OPTION = '1' OR '3' OR '5'
00110         MOVE RT-DISCOUNT-RATE TO WS-WORK-DIS-RATE
00111      ELSE
00112         MOVE CP-LOAN-APR TO WS-WORK-DIS-RATE.
00113
00114 *    IF RT-DISCOUNT-OPTION EQUAL '4'
00115 *       MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
00116 *       GO TO 1075-BYPASS-REGULAR-RATE.
00117
00118      IF RT-DISCOUNT-OPTION EQUAL '3' OR '4' OR '5'
00119         MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
00120         COMPUTE WS-WORK-DIS-RATE EQUAL
00121                 WS-WORK-DIS-RATE / +100
00122         GO TO 1075-BYPASS-REGULAR-RATE.
00123
00124      IF (CP-COMPANY-ID EQUAL 'WDS') AND
00125         (CP-STATE-STD-ABBRV EQUAL 'MN' OR 'NC') AND
00126         (CP-EARN-AS-NET-PAY OR CP-EARN-AS-REG-BALLOON)
00127         MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
00128         GO TO 1075-BYPASS-REGULAR-RATE.
00129
00130      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00131         (CP-STATE-STD-ABBRV EQUAL 'NJ') AND
00132         (CP-TRUNCATED-LIFE)
00133         MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
00134         GO TO 1075-BYPASS-REGULAR-RATE.
00135
00136      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00137         (CP-STATE-STD-ABBRV EQUAL 'GA') AND
00138         (CP-EARN-AS-NET-PAY)
00139 *       (CP-TRUNCATED-LIFE)
00140         MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
00141         GO TO 1075-BYPASS-REGULAR-RATE.
00142
00143      IF ((CP-COMPANY-ID EQUAL 'GTL') AND
00144         (CP-STATE-STD-ABBRV EQUAL 'RI'))
00145                    AND
00146         ((CP-EARN-AS-NET-PAY)
00147                OR
00148         (CP-SPECIAL-CALC-CD EQUAL 'H'))
00149         MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
00150         GO TO 1075-BYPASS-REGULAR-RATE.
00151
00152      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00153         (CP-STATE-STD-ABBRV EQUAL 'MA') AND
00154         (CP-EARN-AS-NET-PAY) AND
00155         (CP-SPECIAL-CALC-CD EQUAL 'H')
00156         MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
00157         GO TO 1075-BYPASS-REGULAR-RATE.
00158
030515     if (cp-company-id = 'CID')
021519        and (cp-state-std-abbrv = 'WA' or 'AL')
030515        and (rt-discount-option = '1')
030515        and (cp-earn-as-farm-plan)
030515        move rt-discount-ob-rate to cp-premium-rate
030515        go to 1075-bypass-regular-rate
030515     end-if
           IF (CP-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'MN')
              AND (RT-DISCOUNT-OPTION = '1')
              AND ((CP-EARN-AS-NET-PAY OR CP-EARN-AS-REG-BALLOON)
041310                     OR
111212            (CP-LEVEL-LIFE OR CP-EARN-AS-FARM-PLAN)
041310            AND (CP-CERT-EFF-DT > X'A4FF'))
              MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
              GO TO 1075-BYPASS-REGULAR-RATE
           END-IF
           if (cp-company-id = 'CID')
              and (cp-earn-as-reg-balloon)
              and (cp-ben-category = 'P' or 'I')
              and (rt-discount-ob-rate not = zeros)
              go to 1075-bypass-regular-rate
           end-if
011707     IF (CP-COMPANY-ID EQUAL 'CID')
022107        AND ((CP-STATE-STD-ABBRV = 'NH' OR 'NV' OR 'WA' OR 'AK')
                            OR
                  (CP-BENEFIT-CD = '49' OR '50' OR '53' OR '54'))
011707        AND (CP-EARN-AS-NET-PAY)
011707        MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
011707        GO TO 1075-BYPASS-REGULAR-RATE
011707     END-IF
00165      IF (CP-COMPANY-ID EQUAL 'NCB') AND
00166         (CP-STATE-STD-ABBRV EQUAL 'NC') AND
00167         (CP-EARN-AS-NET-PAY OR CP-TRUNCATED-LIFE)
00168          MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
00169          GO TO 1075-BYPASS-REGULAR-RATE.
00170
032612     IF CP-COMPANY-ID = 'CID' or 'AHL'
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        IF (CP-STATE-STD-ABBRV EQUAL 'NC') AND
CIDMOD           (CP-EARN-AS-NET-PAY ) AND
CIDMOD           (DC-GREG-DATE-CYMD GREATER THAN 19931231)
CIDMOD            GO TO 1075-BYPASS-REGULAR-RATE
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD     IF (CP-COMPANY-ID = 'CID') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-LEVEL-LIFE) AND
CIDMOD        (CP-STATE-STD-ABBRV = 'CA') AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE)
CIDMOD        COMPUTE WS-WORK-DIS-RATE =
CIDMOD             RT-DISCOUNT-RATE / +100
CIDMOD        MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
CIDMOD        GO TO 1075-BYPASS-REGULAR-RATE
CIDMOD     END-IF
CIDMOD
           IF (CP-COMPANY-ID = 'CID')
              AND (CP-EARN-AS-NET-PAY)
              AND (CP-STATE-STD-ABBRV = 'ME')
              COMPUTE WS-WORK-DIS-RATE =
                   RT-DISCOUNT-RATE / +100
              MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
              GO TO 1075-BYPASS-REGULAR-RATE
           END-IF
060909     IF (CP-COMPANY-ID = 'CID')
060909        AND (CP-STATE-STD-ABBRV = 'ND' OR 'MT' OR 'NJ'
111212           OR 'IN' OR 'AZ')
060909        AND (CP-FARM-PLAN)
060909        MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
060909        GO TO 1075-BYPASS-REGULAR-RATE
060909     END-IF
020816     IF (CP-COMPANY-ID = 'DCC' OR 'VPP')
061909        AND (CP-BEN-CATEGORY = 'D')
061909        AND (CP-EARN-AS-NET-PAY)
061909        MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
011410        COMPUTE CP-PREMIUM-RATE ROUNDED = CP-PREMIUM-RATE *
011410           (1 + CP-DCC-PMF-COMM) / (1 - CP-DCC-ACT-COMM)
      *       DISPLAY ' ACT COMM ' CP-DCC-ACT-COMM
      *       DISPLAY ' PMF COMM ' CP-DCC-PMF-COMM
      *       DISPLAY ' OB RATE  ' RT-DISCOUNT-OB-RATE
      *       DISPLAY ' NEW RATE ' CP-PREMIUM-RATE
061909        GO TO 1075-BYPASS-REGULAR-RATE
061909     END-IF
CIDMOD     IF (CP-COMPANY-ID = 'CID') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
021207        ((CP-STATE-STD-ABBRV = 'MT' OR 'NJ' OR 'PA'
                                  OR 'VT')
                         OR
              ((CP-STATE-STD-ABBRV = 'IN') AND
              (CP-CERT-EFF-DT > X'9A7F'))
032603                   OR
032603        ((CP-STATE-STD-ABBRV = 'AZ' OR 'ND') AND
032603        (CP-CERT-EFF-DT > X'9ADF'))) AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE)
CIDMOD        COMPUTE WS-WORK-DIS-RATE =
CIDMOD             RT-DISCOUNT-RATE / +100
CIDMOD        MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
CIDMOD        GO TO 1075-BYPASS-REGULAR-RATE
CIDMOD     END-IF
CIDMOD
120403     IF (CP-COMPANY-ID = 'CID') AND
120403        (CP-EARN-AS-NET-PAY) AND
120403        (CP-STATE-STD-ABBRV = 'TX') AND
120403        (CP-STATE-STD-ABBRV = CP-STATE) AND
120403        (WS-DISCOUNT-OPTION NOT = ' ')
120403        COMPUTE WS-WORK-DIS-RATE =
120403             RT-DISCOUNT-RATE / +100
120403        MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
120403        GO TO 1075-BYPASS-REGULAR-RATE
120403     END-IF
120403
CIDMOD     IF (CP-COMPANY-ID = 'CID') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-TRUNCATED-LIFE) AND
CIDMOD        (CP-STATE-STD-ABBRV = 'CA' OR 'VA') AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE) AND
CIDMOD        (WS-DISCOUNT-OPTION NOT = ' ')
CIDMOD        COMPUTE WS-WORK-DIS-RATE =
CIDMOD             RT-DISCOUNT-RATE / +100
CIDMOD        MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
CIDMOD        GO TO 1075-BYPASS-REGULAR-RATE
CIDMOD     END-IF
CIDMOD
00176      IF CP-SPECIAL-CALC-CD EQUAL 'H'
00177         IF WS-WORK-DIS-RATE NOT EQUAL +0
00178            COMPUTE WS-WORK-DIS-RATE EQUAL
00179                    WS-WORK-DIS-RATE / +100.
00180
00181      IF CP-SPECIAL-CALC-CD EQUAL 'H'
00182         IF CP-STATE-STD-ABBRV EQUAL 'NY'
00183            IF CP-MONTHLY-PAYMENT EQUAL +0
00184             IF CP-LOAN-APR GREATER THAN +2
00185               COMPUTE CP-MONTHLY-PAYMENT EQUAL
00186               CP-ORIGINAL-BENEFIT /
00187                ((1 - (1 + (CP-LOAN-APR / +1200)) **
00188                (WS-RATE-TERM * -1)) / (CP-LOAN-APR / +1200))
00189             ELSE
00190              COMPUTE CP-MONTHLY-PAYMENT EQUAL
00191                CP-ORIGINAL-BENEFIT / CP-ORIGINAL-TERM.
00192
00193      IF CP-SPECIAL-CALC-CD EQUAL 'H'
00194         IF CP-STATE-STD-ABBRV EQUAL 'NY'
00195           IF WS-WORK-DIS-RATE GREATER THAN +0
00196            COMPUTE WK1 ROUNDED EQUAL
00197            (1 - 1 / (1 + WS-WORK-DIS-RATE / +12) **
00198                               (WS-RATE-TERM - +1)) /
00199                         (WS-WORK-DIS-RATE / +12)
00200            COMPUTE WK2 ROUNDED EQUAL
00201            (1 - 1 / (1 + WS-WORK-DIS-RATE / +12) **
00202                               (WS-RATE-TERM)) /
00203                         (WS-WORK-DIS-RATE / +12)
00204            COMPUTE WK3 EQUAL
00205                 RT-DISCOUNT-OB-RATE *
00206                 (1 + WS-WORK-DIS-RATE / +12)
00207                 MOVE WK3 TO CP-PREMIUM-RATE
00208                 COMPUTE WK5 EQUAL WK3 / +1000
00209            GO TO 1075-BYPASS-REGULAR-RATE.
00210 *               (RT-DISCOUNT-OB-RATE / +1000) *
00211 *               (1 + WS-WORK-DIS-RATE / +12)
00212 *          GO TO 1075-BYPASS-REGULAR-RATE.
00213
00214      IF CP-SPECIAL-CALC-CD EQUAL 'H'
00215         IF RT-DISCOUNT-OB-RATE GREATER THAN +0
00216            COMPUTE WK3 EQUAL
00217            RT-DISCOUNT-OB-RATE *
00218            (24 / (24 + WS-WORK-DIS-RATE * WS-RATE-TERM))
00219            MOVE WK3 TO CP-PREMIUM-RATE
00220            COMPUTE WK5 EQUAL WK3 / +1000
00221            GO TO 1075-BYPASS-REGULAR-RATE.
00222
00223      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00224         (CP-STATE-STD-ABBRV EQUAL 'VT' OR 'MA')
00225         MOVE RT-DISCOUNT-OB-RATE TO CP-PREMIUM-RATE
00226         GO TO 1075-BYPASS-REGULAR-RATE.
00227
00228      COMPUTE WS-SIN-PREM-RATE =
00229              (RT-DISCOUNT-OB-RATE / +1000) *
00230              ((WS-RATE-TERM + +1) / +2).
00231
00232      COMPUTE CP-PREMIUM-RATE =
00233              WS-SIN-PREM-RATE /
00234              (+1 + (((WS-WORK-DIS-RATE / +100) *
00235              WS-RATE-TERM) / +24)).
00236
00237      IF CP-PREMIUM-RATE NOT = +0
00238         MULTIPLY +100 BY CP-PREMIUM-RATE
00239         MOVE CP-PREMIUM-RATE TO WS-PREMIUM-RATE
00240         GO TO 1075-BYPASS-REGULAR-RATE.
00241
00242  1050-REGULAR-RATES.
00243
00244      IF CP-SPECIAL-CALC-CD EQUAL 'H'
00245         COMPUTE WK5 EQUAL
00246         ((WS-RATE-TERM * RT-L-RATE (+12)) / +100) /
00247         (6 * (WS-RATE-TERM + 1))
00248         MOVE WK5 TO CP-PREMIUM-RATE
00249         GO TO 1075-BYPASS-REGULAR-RATE.
00250
00251      IF (CP-COMPANY-ID EQUAL 'CSL') AND
00252         (CP-CSL-VALID-NP-BENEFIT-CD)
00253         IF CP-JOINT-INDICATOR EQUAL 'J'
00254            MOVE RT-L-RATE (3) TO CP-PREMIUM-RATE
00255            GO TO 1075-BYPASS-REGULAR-RATE
00256         ELSE
00257            MOVE RT-L-RATE (1) TO CP-PREMIUM-RATE
00258            GO TO 1075-BYPASS-REGULAR-RATE.
00259
00260      IF (CP-COMPANY-ID EQUAL 'GTL')     AND
00261         (CP-STATE-STD-ABBRV EQUAL 'IA') AND
00262         (CP-EARN-AS-NET-PAY )
00263         MOVE RT-L-RATE (12)      TO CP-PREMIUM-RATE
00264         MOVE CP-CERT-EFF-DT      TO DC-BIN-DATE-1
00265         MOVE CP-FIRST-PAY-DATE   TO DC-BIN-DATE-2
00266         MOVE '1'                 TO DC-OPTION-CODE
00267         PERFORM 9100-CONVERT-DATE THRU 9100-EXIT
00268         IF DATE-CONVERSION-ERROR
00269             MOVE '2'                TO CP-RETURN-CODE
00270             GO TO 9999-CALC-RATE-PREM-X
00271         ELSE
00272             COMPUTE CP-ODD-DAYS-TO-PMT = DC-ELAPSED-MONTHS * 30 +
00273                                          DC-ODD-DAYS-OVER
00274             GO TO 1075-BYPASS-REGULAR-RATE.
00275
00276      IF RT-COMPOSITE-RATE NOT NUMERIC
00277          MOVE ZEROS                        TO RT-COMPOSITE-RATE.
00278
00279 *    IF CP-ORIGINAL-TERM GREATER ZERO AND LESS 361
00280      IF WS-RATE-TERM GREATER ZERO AND LESS 361
00281          MOVE RT-L-RATE (WS-RATE-TERM)     TO CP-PREMIUM-RATE
00282                                               WS-PREMIUM-RATE
00283          MOVE RT-COMPOSITE-RATE            TO CP-COMPOSITE-RATE
00284      ELSE
00285          MOVE +0                           TO CP-PREMIUM-RATE
00286                                               CP-COMPOSITE-RATE
00287                                               CP-CALC-PREMIUM
00288          GO TO 9999-CALC-RATE-PREM-X.
011707     IF (CP-COMPANY-ID EQUAL 'CID')
011707        AND (CP-STATE-STD-ABBRV EQUAL 'SC')
              AND (CP-BENEFIT-CD = '2I' OR '2J' OR '2K' OR '2L')
011707        AND (CP-EARN-AS-NET-PAY)
011707        GO TO 1075-BYPASS-REGULAR-RATE
011707     END-IF
00290 ******************************************************
CIDMOD*    IF CP-COMPANY-ID NOT = 'LBL'
CIDMOD     IF (CP-COMPANY-ID NOT = 'LBL' AND 'CID')
                         or
              ((cp-company-id = 'CID')
              and (cp-earn-as-reg-balloon)
              and (cp-ben-category = 'P' or 'I'))
00292          GO TO 1075-BYPASS-REGULAR-RATE.
00293
00294      MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1.
00295      MOVE CP-FIRST-PAY-DATE      TO DC-BIN-DATE-2.
00296      MOVE '1'                    TO DC-OPTION-CODE.
00297      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
00298      IF DATE-CONVERSION-ERROR
00299          MOVE '2'                TO CP-RETURN-CODE
00300          GO TO 9999-CALC-RATE-PREM-X
00301        ELSE
00302          MOVE DC-ELAPSED-MONTHS  TO CP-MNTHS-TO-FIRST-PMT
00303          MOVE DC-ODD-DAYS-OVER   TO CP-ODD-DAYS-TO-PMT.
00304
00305      IF CP-MNTHS-TO-FIRST-PMT GREATER THAN 1
00306         COMPUTE CP-ODD-DAYS-TO-PMT EQUAL CP-ODD-DAYS-TO-PMT +
00307         ((CP-MNTHS-TO-FIRST-PMT - 1) * +30).
00308
00309      IF (CP-EXT-CHG-LF OR CP-EXT-CHG-LF-AH)
00310         IF (CP-ODD-DAYS-TO-PMT NOT = ZEROS)
00311            NEXT SENTENCE
00312          ELSE
00313            GO TO 1075-BYPASS-REGULAR-RATE
00314        ELSE
00315         IF (CP-MNTHS-TO-FIRST-PMT = ZERO) AND
00316            (CP-ODD-DAYS-TO-PMT NOT = ZEROS)
00317            NEXT SENTENCE
00318          ELSE
00319            GO TO 1075-BYPASS-REGULAR-RATE.
00320
00321      IF CP-MNTHS-TO-FIRST-PMT GREATER ZERO
00322         COMPUTE CP-PREMIUM-RATE =
00323         ((CP-PREMIUM-RATE / CP-ORIGINAL-TERM
00324          * (CP-ORIGINAL-TERM + 1)) - CP-PREMIUM-RATE)
00325          * (CP-ODD-DAYS-TO-PMT * 12 / 365) + CP-PREMIUM-RATE.
00326
00327      IF CP-MNTHS-TO-FIRST-PMT = ZEROS
00328         COMPUTE CP-PREMIUM-RATE =
00329         (CP-PREMIUM-RATE - (CP-PREMIUM-RATE /
00330         CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM - 1))) *
00331         (CP-ODD-DAYS-TO-PMT * 12 / 365) +
00332         (CP-PREMIUM-RATE / CP-ORIGINAL-TERM
00333          * (CP-ORIGINAL-TERM - 1)).
00334 ******************************************************
00335
00336  1075-BYPASS-REGULAR-RATE.
00337
00338      MOVE CP-LOAN-TERM              TO WS-RATE-TERM.
00339
00340      MOVE +1 TO LINDX.
00341
00342      IF CP-COMPANY-ID EQUAL 'CRI'
00343         IF CP-SIG-SWITCH EQUAL 'S' OR 'J'
00344            MOVE +5               TO LINDX
00345            IF RT-L-EX-AGE (LINDX) EQUAL ZERO
00346               MOVE 'F'           TO CP-RETURN-CODE
00347               GO TO 1200-CALCULATE-PREMIUMS.
00348
00349      IF RT-L-EX-AGE (LINDX) = ZERO
00350           GO TO 1200-CALCULATE-PREMIUMS.
00351
00352  1100-SEARCH-LIFE-LIMITS.
00353
00354      IF CP-COMPANY-ID EQUAL 'CRI'
00355         IF CP-SIG-SWITCH NOT EQUAL 'S' AND 'J'
00356            IF LINDX GREATER THAN +4
00357               MOVE 'A'           TO CP-RETURN-CODE
00358               GO TO 1200-CALCULATE-PREMIUMS.
00359
00360      IF LINDX GREATER +8
00361          MOVE 'A'                TO CP-RETURN-CODE
00362          GO TO 1200-CALCULATE-PREMIUMS.
012820     if rt-l-min-term(lindx) not numeric
012820        move +0                  to rt-l-min-term(lindx)
012820     end-if
00364      IF CP-ISSUE-AGE GREATER RT-L-EX-AGE (LINDX)
00365          IF RT-L-EX-AGE (LINDX) = ZEROS
00366              ADD +1              TO LINDX
00367              GO TO 1100-SEARCH-LIFE-LIMITS
00368          ELSE
00369              MOVE RT-L-EX-AGE (LINDX)
00370                                  TO CP-R-MAX-AGE
00371              MOVE RT-L-EX-TERM (LINDX)
00372                                  TO CP-R-MAX-TERM
00373              MOVE RT-L-EX-FACE (LINDX)
00374                                  TO CP-R-MAX-TOT-BEN
00375              ADD +1              TO LINDX
00376              GO TO 1100-SEARCH-LIFE-LIMITS
00377      ELSE
00378          MOVE RT-L-EX-AGE (LINDX)
00379                                  TO CP-R-MAX-AGE
00380          MOVE RT-L-EX-TERM (LINDX)
00381                                  TO CP-R-MAX-TERM
00382          MOVE RT-L-EX-FACE (LINDX)
00383                                  TO CP-R-MAX-TOT-BEN.
00384
00385      IF CP-ORIGINAL-TERM GREATER RT-L-EX-TERM (LINDX)
00386          MOVE '9'                TO CP-RETURN-CODE.
00387
00388      IF CP-ORIGINAL-BENEFIT GREATER RT-L-EX-FACE (LINDX)
00389          MOVE '8'                TO CP-RETURN-CODE.
012820     if rt-l-min-term(lindx) <> zeros
012820        and CP-loan-term     < RT-L-min-TERM (LINDX)
012820        MOVE 'I'                 TO CP-RETURN-CODE
012820     end-if
012820
012820     .
00391  1200-CALCULATE-PREMIUMS.
00392
00393      IF CP-COMPANY-ID = 'CSL'
00394         IF CP-CSL-VALID-NP-BENEFIT-CD
00395            GO TO 1240-NET-PAY-CONT
00396         ELSE
00397            GO TO 1250-CSL-CALC.
00398
00399      IF CP-COMPANY-ID = 'WDS'
00400         IF CP-STATE-STD-ABBRV = 'MN' OR 'NC'
00401            IF CP-EARN-AS-REG-BALLOON
00402               GO TO 1240-NET-PAY-CONT.
00403
           IF (CP-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'MN')
111212        AND (CP-EARN-AS-REG-BALLOON OR CP-EARN-AS-FARM-PLAN)
              AND (CP-CERT-EFF-DT > X'A4FF')
              GO TO 1240-NET-PAY-CONT
           END-IF
111212     IF (CP-COMPANY-ID = 'CID')
021519        AND (CP-STATE-STD-ABBRV = 'WA' or 'AL')
111212        AND (CP-FARM-PLAN)
111212        GO TO 1240-NET-PAY-CONT
111212     END-IF
           if (cp-company-id = 'CID')
              and (cp-earn-as-reg-balloon)
              and (cp-ben-category = 'P' or 'I')
              display ' found balloon p or i ' rt-discount-ob-rate
              evaluate true
                 when (cp-net-only-state = 'Y')
                    and (rt-discount-ob-rate > zeros)
                    compute cp-premium-rate = rt-discount-ob-rate / 10
                 when (cp-net-only-state = 'Y')
                    and (rt-discount-ob-rate = zeros)
                    display ' not ob rate     ' cp-premium-rate
                    compute cp-premium-rate =
                       cp-premium-rate / cp-original-term * 12
                    display ' new rate ' cp-premium-rate
                    display ' net only state ' cp-premium-rate
                    compute cp-premium-rate rounded =
                      (cp-premium-rate *
                         (cp-original-term + 1) / 12) /
                      ((cp-original-term + 2) / 2)
                    display ' new OB rate ' cp-premium-rate
                 when (cp-net-only-state <> 'Y')
                    and (rt-discount-ob-rate = zeros)
                    display ' not net only st ' cp-premium-rate
                    compute cp-premium-rate =
                       cp-premium-rate / cp-original-term * 12
                    display ' new rate ' cp-premium-rate
              end-evaluate
              go to 1240-net-pay-cont
           end-if
060909     IF (CP-COMPANY-ID = 'CID')
060909        AND (CP-STATE-STD-ABBRV = 'ND' OR 'MT' OR 'NJ'
060909           OR 'IN' OR 'AZ')
060909        AND (CP-FARM-PLAN)
060909***  OK, I DID THIS SO ERCNETP WOULDN'T ERROR OUT
060909***  THE APR ISN'T EVEN USED IN THE ROUTINE, SO SUE ME :)
060909        MOVE +1.0                TO CP-LOAN-APR
060909        GO TO 1240-NET-PAY-CONT
060909     END-IF
00404      IF CP-RATE-AS-STANDARD
00405          GO TO 1210-REGULAR-RATE.
00406
00407      IF CP-CRITICAL-PERIOD
00408          GO TO 1220-CRITICAL-PERIOD-LF-CONT.
00409
00410      IF CP-EARN-AS-TEXAS
00411          GO TO 1230-TEXAS-CONT.
00412
00413      IF CP-EARN-AS-NET-PAY
00414         IF CP-STATE-STD-ABBRV = 'MD'
00415           IF CP-COMPANY-ID = 'MON'
00416             IF CP-BENEFIT-CD = '11' OR '12' OR '13' OR
00417                                '14' OR '26' OR '47'
00418               GO TO 1210-REGULAR-RATE.
00419
00420      IF CP-SPECIAL-CALC-CD EQUAL 'H'
00421         IF CP-MONTHLY-PAYMENT EQUAL +0
00422            COMPUTE CP-MONTHLY-PAYMENT EQUAL
00423                 CP-ORIGINAL-BENEFIT / CP-ORIGINAL-TERM.
00424
00425      IF CP-COMPANY-ID EQUAL 'GTL'
00426         NEXT SENTENCE
00427      ELSE
00428      IF CP-SPECIAL-CALC-CD EQUAL 'H'
00429         IF CP-STATE-STD-ABBRV NOT EQUAL 'NY'
00430            COMPUTE WK3 ROUNDED EQUAL
00431            CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM - +1) *
00432            CP-MONTHLY-PAYMENT
00433            COMPUTE WK3 ROUNDED EQUAL WK3 / 2
00434            COMPUTE WK3 ROUNDED EQUAL WK3 + (CP-ORIGINAL-TERM *
00435            CP-ALTERNATE-BENEFIT)
00436            COMPUTE CP-CALC-PREMIUM ROUNDED EQUAL
00437            WK3 * WK5
00438            GO TO 9999-CALC-RATE-PREM-X
00439         ELSE
00440            COMPUTE WK3 ROUNDED EQUAL (CP-MONTHLY-PAYMENT *
00441            (CP-ORIGINAL-TERM - 1 - WK1)) /
00442            (WS-WORK-DIS-RATE / +12)
00443            COMPUTE WK3 EQUAL WK3 + (CP-ALTERNATE-BENEFIT * WK2)
00444            COMPUTE CP-CALC-PREMIUM ROUNDED EQUAL WK3 *
00445                                WK5
00446            GO TO 9999-CALC-RATE-PREM-X.
00447
00448      IF CP-COMPANY-ID EQUAL 'GTL' AND
00449         CP-STATE-STD-ABBRV EQUAL 'RI' AND
00450         CP-SPECIAL-CALC-CD EQUAL 'H' AND
00451         CP-ORIGINAL-TERM LESS THAN +61
00452         COMPUTE CP-CALC-PREMIUM EQUAL
00453                         (CP-ORIGINAL-BENEFIT *
00454            (CP-ORIGINAL-TERM + 1) * CP-PREMIUM-RATE) /
00455            (20 * (1 + .0019 * CP-ORIGINAL-TERM) * 100)
00456         COMPUTE CP-CALC-PREMIUM EQUAL
00457                 CP-CALC-PREMIUM +
00458               (CP-ALTERNATE-BENEFIT * CP-PREMIUM-RATE *
00459         (CP-ORIGINAL-TERM + 1)) /
00460         (10 * (1 + .0027 * (CP-ORIGINAL-TERM + 1)) * 100)
00461         GO TO 9999-CALC-RATE-PREM-X.
00462
00463      IF CP-COMPANY-ID = 'GTL'     AND
00464         CP-STATE-STD-ABBRV = 'PA' AND
00465         CP-SPECIAL-CALC-CD = 'H'
00466          GO TO 1260-GTL-PA-LEASE.
00467
00468      IF (CP-EARN-AS-NET-PAY)
050713*       AND (CP-LOAN-APR > ZEROS)
00469         GO TO 1240-NET-PAY-CONT
           END-IF
00470
00471      IF CP-LIFE-OVERRIDE-CODE NOT = 'L'
00472          GO TO 1210-REGULAR-RATE.
00473
00474 **** CHECK TO SEE IF THE LEVEL PORTION OF A BALLOON IS BEING
00475 **** RATED AND BYPASS NET-PAY CHECK IF NO APR HAS BEEN ENTERED.
00476 ****
00477      IF CP-DEVIATION-CODE = 'LEV' AND
00478         CP-LOAN-APR = ZEROS
00479          GO TO 1210-REGULAR-RATE.
00480
00481      IF (CP-STATE-STD-ABBRV = 'OH')
CIDMOD        AND (CP-CLASS-CODE NOT = 'L ')
00482         AND (CP-ORIGINAL-TERM > 60)
00483         AND (DC-GREG-DATE-CYMD > 19831031)
102808        AND (CP-LOAN-APR > ZEROS)
00484         GO TO 1240-NET-PAY-CONT
           END-IF
00485
00486      IF (CP-STATE-STD-ABBRV = 'UT')
00487         AND (CP-ORIGINAL-TERM > 62)
00488         AND (DC-GREG-DATE-CYMD > 19810831)
00489         AND (DC-GREG-DATE-CYMD < 19830901)
102808        AND (CP-LOAN-APR > ZEROS)
00490         GO TO 1240-NET-PAY-CONT
           END-IF
00491
00492      IF (CP-STATE-STD-ABBRV = 'RI')
00493         AND (CP-ORIGINAL-TERM > 60)
00494         AND (DC-GREG-DATE-CYMD > 19831231)
102808        AND (CP-LOAN-APR > ZEROS)
00495         GO TO 1240-NET-PAY-CONT
           END-IF
           .
00497  1210-REGULAR-RATE.
00498
           IF (CP-COMPANY-ID = 'CID')
              AND (CP-EARN-AS-NET-PAY)
              AND (CP-LOAN-APR = 0)
              IF RT-L-RATE (WS-RATE-TERM) = 0
00228            COMPUTE WS-SIN-PREM-RATE =
00229               (RT-DISCOUNT-OB-RATE / +1000) *
00230               ((WS-RATE-TERM + +1) / +2)
00232            COMPUTE CP-PREMIUM-RATE =
00233               WS-SIN-PREM-RATE /
00234               (+1 + (((WS-WORK-DIS-RATE / +100) *
00235               WS-RATE-TERM) / +24))
              ELSE
                 MOVE RT-L-RATE (WS-RATE-TERM) TO CP-PREMIUM-RATE
              END-IF
           END-IF
00499 *    IF CP-SPECIAL-CALC-CD EQUAL 'H'
00500 *       IF CP-MONTHLY-PAYMENT EQUAL +0
00501 *          COMPUTE CP-MONTHLY-PAYMENT EQUAL
00502 *               CP-ORIGINAL-BENEFIT / CP-ORIGINAL-TERM.
00503 *
00504 *    IF CP-SPECIAL-CALC-CD EQUAL 'H'
00505 *       IF CP-STATE-STD-ABBRV NOT EQUAL 'NY'
00506 *          COMPUTE WK3 ROUNDED EQUAL
00507 *          CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM - +1) *
00508 *          CP-MONTHLY-PAYMENT
00509 *          COMPUTE WK3 ROUNDED EQUAL WK3 / 2
00510 *          COMPUTE WK3 ROUNDED EQUAL WK3 + (CP-ORIGINAL-TERM *
00511 *          CP-ALTERNATE-BENEFIT)
00512 *          COMPUTE CP-CALC-PREMIUM ROUNDED EQUAL
00513 *          WK3 * WK5
00514 *          GO TO 9999-CALC-RATE-PREM-X
00515 *       ELSE
00516 *          COMPUTE WK3 ROUNDED EQUAL (CP-MONTHLY-PAYMENT *
00517 *          (CP-ORIGINAL-TERM - 1 - WK1)) /
00518 *          (WS-WORK-DIS-RATE / +12)
00519 *          COMPUTE WK3 EQUAL WK3 + (CP-ALTERNATE-BENEFIT * WK2)
00520 *          COMPUTE CP-CALC-PREMIUM ROUNDED EQUAL WK3 *
00521 *                              WK5
00522 *          GO TO 9999-CALC-RATE-PREM-X.
00523
00524      IF CP-TERM-IS-DAYS
00525          COMPUTE CP-PREMIUM-RATE ROUNDED =
00526              ((CP-PREMIUM-RATE / CP-ORIGINAL-TERM) * 12) / +365
00527          COMPUTE CP-CALC-PREMIUM ROUNDED =
00528              ((CP-ORIGINAL-BENEFIT / 100) * CP-PREMIUM-RATE)
00529                           * CP-TERM-OR-EXT-DAYS
00530            GO TO 9999-CALC-RATE-PREM-X.
00531
032612     if (cp-company-id = 'AHL')
032612        and (cp-loan-term > cp-original-term)
032612        continue
032612     else
00532         IF NOT CP-TRUNCATED-LIFE
00533            GO TO 1212-GROSS-NOT-TRUNCATED
032612        end-if
032612     end-if
00534
00535 ****  GROSS TRUNCATED CALCULATION
00536
00537      IF CP-LOAN-TERM NOT GREATER THAN CP-ORIGINAL-TERM
00538          MOVE 'H'                TO  CP-RETURN-CODE
00539          GO TO 9999-CALC-RATE-PREM-X.
032612     if cp-company-id = 'AHL'
032612        if (cp-state = 'AZ' OR 'GA' OR 'IN' OR 'MT' OR 'NH'
032612           OR 'NJ' OR 'OH' OR 'PA' OR 'TX' OR 'VA')
032612           compute cp-calc-premium rounded = (1 - (cp-loan-term -
032612           cp-original-term) * (cp-loan-term - cp-original-term
032612            + 1) / cp-loan-term / (cp-loan-term + 1)) *
032612            cp-premium-rate / 100 * cp-original-term /
032612            cp-loan-term * cp-original-benefit
032612        else
032612           compute cp-calc-premium rounded = (1 - (cp-loan-term -
032612           cp-original-term) * (cp-loan-term - cp-original-term
032612            + 1) / cp-loan-term / (cp-loan-term + 1)) *
032612            cp-premium-rate * cp-original-benefit / 100
032612        end-if
032612        go to 9999-calc-rate-prem-x
032612     end-if
032612
00541      IF CP-COMPANY-ID = 'NCL'
00542          COMPUTE WK-MONTHLY-RATE =
00543                ((((CP-LOAN-TERM * (CP-LOAN-TERM + 1) -
00544                   (CP-LOAN-TERM - CP-ORIGINAL-TERM) *
00545                   (CP-LOAN-TERM - CP-ORIGINAL-TERM + 1))
00546                                  /
00547                   (CP-LOAN-TERM * (CP-LOAN-TERM + 1))))
00548                                  *
00549                   ((RT-L-RATE (12) * CP-LOAN-TERM / 12)))
00550          COMPUTE CP-CALC-PREMIUM =
00551                  WK-MONTHLY-RATE * CP-ORIGINAL-BENEFIT / 100
00553      ELSE
00554          COMPUTE CP-R-MAX-TERM = CP-LOAN-TERM -
00555                                  CP-ORIGINAL-TERM
00556          COMPUTE CP-CALC-PREMIUM = CP-ORIGINAL-BENEFIT *
00557                      ((RT-L-RATE (CP-LOAN-TERM)  -
00558                      (1 - (CP-ORIGINAL-TERM / CP-LOAN-TERM)) *
00559                      RT-L-RATE (CP-R-MAX-TERM)) / 100)
           END-IF
00573      IF CP-EXT-CHG-LF OR CP-EXT-CHG-LF-AH
00574         COMPUTE CP-CALC-PREMIUM ROUNDED = CP-CALC-PREMIUM +
00575            (((CP-ORIGINAL-BENEFIT / 100) * ((CP-PREMIUM-RATE /
00576            CP-ORIGINAL-TERM) / 30)) * CP-TERM-OR-EXT-DAYS)
           END-IF
           GO TO 9999-CALC-RATE-PREM-X
           .
00562  1212-GROSS-NOT-TRUNCATED.
      *    display ' premium rate ' cp-premium-rate
00563
042208     IF (CP-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'VT' OR 'MA'
040610           OR 'IN')
              AND (CP-BENEFIT-CD = '55' OR '56')
              COMPUTE CP-CALC-PREMIUM ROUNDED =
                (CP-ORIGINAL-BENEFIT / (CP-ORIGINAL-TERM)) *
                (CP-ORIGINAL-TERM) *
                ((CP-ORIGINAL-TERM) / 12) *
                ((6.5 * RT-DISCOUNT-OB-RATE) / 1000)
                GO TO 9999-CALC-RATE-PREM-X
           END-IF
           IF (CP-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'GA')
              AND (CP-BENEFIT-CD = '55' OR '56' OR '2M' OR '2N')
              COMPUTE WK3 ROUNDED =
                CP-ORIGINAL-BENEFIT *
                ((CP-ORIGINAL-TERM + 1) / 2) *
                (RT-DISCOUNT-OB-RATE / 1000)
                MOVE WK3 TO CP-CALC-PREMIUM
                GO TO 9999-CALC-RATE-PREM-X
           END-IF
041310     IF (CP-COMPANY-ID EQUAL 'CID')
041310        AND (CP-STATE-STD-ABBRV = 'MN')
041310        AND (CP-LEVEL-LIFE)
041310        AND (CP-CERT-EFF-DT > X'A4FF')
041310        MOVE CP-CERT-EFF-DT      TO DC-BIN-DATE-1
041310        MOVE CP-EXPIRE-DT        TO DC-BIN-DATE-2
041310        MOVE '1'                 TO DC-OPTION-CODE
041310        PERFORM 9100-CONVERT-DATE
041310                                 THRU 9100-EXIT
041310        IF DATE-CONVERSION-ERROR
041310           MOVE '2'              TO CP-RETURN-CODE
041310           MOVE ZEROS            TO FACTOR
041310                                 CP-CALC-PREMIUM
041310           GO TO 9999-CALC-RATE-PREM-X
041310        END-IF
112410        MOVE DC-ELAPSED-DAYS     TO D
032020        COMPUTE wk-pay-amt = CP-ORIGINAL-BENEFIT /
112410           (1 / (1 + (CP-LOAN-APR / 100) * D / 360) -
112410           (CP-PREMIUM-RATE * 12 / 365 * D / 1000))
032020        COMPUTE CP-CALC-PREMIUM = wk-pay-amt *
112410           CP-PREMIUM-RATE * 12 / 365 * D / 1000
041310        GO TO 9999-CALC-RATE-PREM-X
041310     END-IF
120202     IF (CP-COMPANY-ID = 'CID')
120202        AND (CP-BENEFIT-CD (1:1) = 'M' OR 'N' OR 'H')
120202        COMPUTE CP-CALC-PREMIUM ROUNDED =
120202           (CP-ORIGINAL-BENEFIT / 1000) * CP-PREMIUM-RATE
120202     ELSE
00564         COMPUTE CP-CALC-PREMIUM ROUNDED =
00565                  (CP-ORIGINAL-BENEFIT / 100) * CP-PREMIUM-RATE
120202     END-IF
      *    display ' calc premium ' cp-calc-premium
00566
CIDMOD*    IF CP-COMPANY-ID = 'LBL'
CIDMOD     IF (CP-COMPANY-ID = 'LBL' OR 'CID')
102808        AND (NOT CP-EARN-AS-NET-PAY)
00568         GO TO 9999-CALC-RATE-PREM-X
           END-IF
00569
00570      IF CP-TERM-OR-EXT-DAYS = ZEROS
00571          GO TO 9999-CALC-RATE-PREM-X.
00572
00573      IF CP-EXT-CHG-LF OR CP-EXT-CHG-LF-AH
00574          COMPUTE CP-CALC-PREMIUM ROUNDED = CP-CALC-PREMIUM +
00575             (((CP-ORIGINAL-BENEFIT / 100) * ((CP-PREMIUM-RATE /
00576                CP-ORIGINAL-TERM) / 30)) * CP-TERM-OR-EXT-DAYS).
00577
00578      GO TO 9999-CALC-RATE-PREM-X.
00579
00580  1220-CRITICAL-PERIOD-LF-CONT.
00581      COMPUTE CP-CALC-PREMIUM ROUNDED =
00582                (CP-ORIGINAL-BENEFIT / +100) * CP-PREMIUM-RATE.
00583
00584      GO TO 9999-CALC-RATE-PREM-X.
00585
00586  1230-TEXAS-CONT.
00587      IF CP-FARM-PLAN
00588          GO TO 1235-TEXAS-FARM-CONT.
00589
00590      COMPUTE CP-CALC-PREMIUM ROUNDED =
00591                (CP-ORIGINAL-BENEFIT / 100) * CP-PREMIUM-RATE.
00592
00593      COMPUTE TEX-FACT-9 =
00594                  (CP-ORIGINAL-TERM + CP-PAY-FREQUENCY) /
00595                  (CP-ORIGINAL-TERM + 1).
00596
00597      COMPUTE CP-CALC-PREMIUM ROUNDED =
00598                  CP-CALC-PREMIUM * TEX-FACT-9.
00599
00600      GO TO 9999-CALC-RATE-PREM-X.
00601
00602  1235-TEXAS-FARM-CONT.
00603      COMPUTE WS-FARM-TERM =
00604                (CP-ORIGINAL-TERM + CP-PAY-FREQUENCY).
00605
00606      COMPUTE WS-PREMIUM-RATE = RT-L-RATE (WS-FARM-TERM) / 2.
00607      MOVE WS-PREMIUM-RATE         TO CP-PREMIUM-RATE.
00608
00609
00610      COMPUTE CP-CALC-PREMIUM ROUNDED =
00611               (CP-ORIGINAL-BENEFIT / 100) * CP-PREMIUM-RATE.
00612
00613      GO TO 9999-CALC-RATE-PREM-X.
00614
00615  1240-NET-PAY-CONT.
00616
00617      MOVE CP-LOAN-APR             TO N-P-APR.
00618      MOVE CP-ORIGINAL-TERM        TO N-P-ORIG
00619                                      N-P-REM
00620                                      N-P-LOAN.
00621
00622      MOVE CP-SPECIAL-CALC-CD      TO N-P-OPT.
00623
00624      IF CP-TRUNCATED-LIFE
00625          MOVE CP-LOAN-TERM        TO N-P-LOAN.
00626
00627      IF CP-COMPANY-ID EQUAL 'NCL'
00628         NEXT SENTENCE
00629      ELSE
00630         GO TO 1241-NET-PAY-CONT.
00631
00632      MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1
00633      MOVE CP-FIRST-PAY-DATE      TO DC-BIN-DATE-2
00634      MOVE '1'                    TO DC-OPTION-CODE
00635      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
00636      IF DATE-CONVERSION-ERROR
00637          MOVE '2'                TO CP-RETURN-CODE
00638          GO TO 9999-CALC-RATE-PREM-X
00639      ELSE
00640         MOVE DC-ELAPSED-MONTHS   TO CP-MNTHS-TO-FIRST-PMT
00641         MOVE DC-ODD-DAYS-OVER    TO CP-ODD-DAYS-TO-PMT.
00642
00643      IF (CP-EXT-CHG-LF OR CP-EXT-CHG-LF-AH) AND
00644         (CP-ODD-DAYS-TO-PMT NOT EQUAL ZEROS)
00645         NEXT SENTENCE
00646      ELSE
00647         MOVE +1                  TO ODF
00648         GO TO 1241-NET-PAY-CONT.
00649
00650      IF CP-MNTHS-TO-FIRST-PMT GREATER THAN ZERO
00651         COMPUTE CP-PREMIUM-RATE EQUAL
00652         ((CP-PREMIUM-RATE / N-P-LOAN * (N-P-LOAN + 1)) -
00653         CP-PREMIUM-RATE) *
00654         (CP-ODD-DAYS-TO-PMT * 12 / 365) + CP-PREMIUM-RATE.
00655
00656      IF CP-MNTHS-TO-FIRST-PMT EQUAL ZEROS
00657         COMPUTE CP-PREMIUM-RATE EQUAL
00658         (CP-PREMIUM-RATE - (CP-PREMIUM-RATE /
00659         N-P-LOAN * (N-P-LOAN - 1))) *
00660         (CP-ODD-DAYS-TO-PMT * 12 / 365) +
00661         (CP-PREMIUM-RATE / N-P-LOAN * (N-P-LOAN - 1)).
00662
00663      COMPUTE ODF EQUAL
00664      ((1 + (N-P-APR / +1200)) ** (1 - CP-MNTHS-TO-FIRST-PMT)) /
00665      ((1 + (CP-ODD-DAYS-TO-PMT * 12 / 36500))).
00666
00667  1241-NET-PAY-CONT.
00668
00669      PERFORM 10000-NET-TERM THRU 99999-EXIT.
00670
CIDMOD*    IF CP-RETURN-CODE EQUAL '0' OR '8'
PEMMOD     IF CP-RETURN-CODE EQUAL '0' OR '8' OR '9' OR 'B'
00672         NEXT SENTENCE
00673      ELSE
00674         GO TO 9999-CALC-RATE-PREM-X.
00675
00676      IF RT-DISCOUNT-OPTION EQUAL '6'
00677          GO TO 9999-CALC-RATE-PREM-X.
00678
00679      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00680         (CP-STATE-STD-ABBRV EQUAL 'IA')
00681         GO TO 9999-CALC-RATE-PREM-X.
00682
00683      MOVE N-P-FACTOR              TO CP-CDT-FACTOR.
00684
00685      IF RT-DISCOUNT-OPTION EQUAL '3' OR '4' OR '5'
00686         GO TO 9999-CALC-RATE-PREM-X.
00687
00688      IF (CP-COMPANY-ID EQUAL 'MON') AND
00689         (CP-STATE-STD-ABBRV EQUAL 'MD') AND
00690         (CP-TRUNCATED-LIFE)
00691         GO TO 9999-CALC-RATE-PREM-X.
00692
00693      IF (CP-COMPANY-ID EQUAL 'CSL') AND
00694         (CP-CSL-VALID-NP-BENEFIT-CD)
00695         GO TO 9999-CALC-RATE-PREM-X.
00696
00697      IF (CP-COMPANY-ID EQUAL 'TMS') AND
00698         (CP-STATE-STD-ABBRV EQUAL 'MN' OR 'CA' OR 'WA') AND
00699         (CP-EARN-AS-NET-PAY)
00700         GO TO 9999-CALC-RATE-PREM-X.
00701
CIDMOD*    IF (CP-COMPANY-ID EQUAL 'CID') AND
CIDMOD*       (CP-STATE-STD-ABBRV EQUAL 'NV') AND
CIDMOD*       (CP-EARN-AS-NET-PAY OR CP-TRUNCATED-LIFE)
CIDMOD*       GO TO 9999-CALC-RATE-PREM-X.
00706
00707      IF (CP-COMPANY-ID EQUAL 'NCB') AND
00708         (CP-STATE-STD-ABBRV EQUAL 'NC') AND
00709         (CP-EARN-AS-NET-PAY)
00710         GO TO 9999-CALC-RATE-PREM-X.
00711
00712      IF (CP-COMPANY-ID EQUAL 'WDS') AND
00713         (CP-STATE-STD-ABBRV EQUAL 'MN' OR 'NC') AND
00714         (CP-EARN-AS-NET-PAY OR CP-EARN-AS-REG-BALLOON)
00715         GO TO 9999-CALC-RATE-PREM-X.
00716
00717      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00718         (CP-EARN-AS-NET-PAY)
00719         GO TO 9999-CALC-RATE-PREM-X.
00720
00721      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00722         (CP-STATE-STD-ABBRV EQUAL 'NJ') AND
00723         (CP-TRUNCATED-LIFE)
00724         GO TO 9999-CALC-RATE-PREM-X.
00725
00726      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00727         (CP-STATE-STD-ABBRV EQUAL 'GA') AND
00728         (CP-EARN-AS-NET-PAY)
00729 *       (CP-TRUNCATED-LIFE)
00730         GO TO 9999-CALC-RATE-PREM-X.
00731
00732      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00733         (CP-STATE-STD-ABBRV EQUAL 'RI')
00734                    AND
00735         ((CP-EARN-AS-NET-PAY)
00736                OR
00737         (CP-SPECIAL-CALC-CD EQUAL 'H'))
00738         GO TO 9999-CALC-RATE-PREM-X.
00739
00740      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00741         (CP-STATE-STD-ABBRV EQUAL 'MA') AND
00742         (CP-EARN-AS-NET-PAY) AND
00743         (CP-SPECIAL-CALC-CD EQUAL 'H')
00744         GO TO 9999-CALC-RATE-PREM-X.
00745
00746      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00747         (CP-STATE-STD-ABBRV EQUAL 'OR') AND
00748         (CP-EARN-AS-NET-PAY)
00749         GO TO 9999-CALC-RATE-PREM-X.
00750
032612     IF CP-COMPANY-ID = 'CID' or 'AHL'
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        IF (CP-STATE-STD-ABBRV EQUAL 'NC') AND
CIDMOD           (CP-EARN-AS-NET-PAY) AND
CIDMOD           (DC-GREG-DATE-CYMD GREATER THAN 19931231)
CIDMOD           GO TO 9999-CALC-RATE-PREM-X
CIDMOD        END-IF
CIDMOD     END-IF
00755
           IF (CP-COMPANY-ID = 'CID')
              AND (CP-EARN-AS-NET-PAY)
              AND (CP-STATE-STD-ABBRV = 'ME')
              GO TO 9999-CALC-RATE-PREM-X
           END-IF
020816     IF (CP-COMPANY-ID = 'DCC' OR 'VPP')
061909        AND (CP-BEN-CATEGORY = 'D')
061909        AND (CP-EARN-AS-NET-PAY)
060909        GO TO 9999-CALC-RATE-PREM-X
061909     END-IF
060909     IF (CP-COMPANY-ID = 'CID')
060909        AND (CP-STATE-STD-ABBRV = 'ND' OR 'MT' OR 'NJ'
111212           OR 'IN' OR 'AZ')
060909        AND (CP-FARM-PLAN)
060909        GO TO 9999-CALC-RATE-PREM-X
060909     END-IF
CIDMOD     IF (CP-COMPANY-ID = 'CID') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-LEVEL-LIFE) AND
CIDMOD        (CP-STATE-STD-ABBRV = 'CA' OR 'MN') AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE)
CIDMOD        GO TO 9999-CALC-RATE-PREM-X
CIDMOD     END-IF
CIDMOD
CIDMOD     IF (CP-COMPANY-ID = 'CID') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
021207        ((CP-STATE-STD-ABBRV = 'MT' OR 'NJ' OR 'PA'
                                  OR 'VT')
                         OR
              ((CP-STATE-STD-ABBRV = 'IN') AND
              (CP-CERT-EFF-DT > X'9A7F'))
032603                   OR
032603        ((CP-STATE-STD-ABBRV = 'AZ' OR 'ND') AND
032603        (CP-CERT-EFF-DT > X'9ADF'))) AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE)
CIDMOD        GO TO 9999-CALC-RATE-PREM-X
CIDMOD     END-IF
CIDMOD
120403     IF (CP-COMPANY-ID = 'CID') AND
120403        (CP-EARN-AS-NET-PAY) AND
120403        (CP-STATE-STD-ABBRV = 'TX') AND
120403        (CP-STATE-STD-ABBRV = CP-STATE) AND
120403        (WS-DISCOUNT-OPTION NOT = ' ')
120403        GO TO 9999-CALC-RATE-PREM-X
120403     END-IF
120403
030515     if (cp-company-id = 'CID')
021519        and (cp-state-std-abbrv = 'WA' or 'AL')
030515        and (rt-discount-option = '1')
030515        and (cp-earn-as-farm-plan)
030515        go to 9999-calc-rate-prem-x
030515     end-if
           if (cp-company-id = 'CID')
              and (cp-earn-as-reg-balloon)
              and (cp-ben-category = 'P' or 'I')
              GO TO 9999-CALC-RATE-PREM-X
           end-if
           IF (CP-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'MN')
              AND (RT-DISCOUNT-OPTION = '1')
              AND (CP-EARN-AS-NET-PAY OR CP-EARN-AS-REG-BALLOON OR
111212           CP-EARN-AS-FARM-PLAN)
              GO TO 9999-CALC-RATE-PREM-X
           END-IF
011707     IF (CP-COMPANY-ID = 'CID')
011707        AND (CP-EARN-AS-NET-PAY)
011707        AND ((CP-STATE-STD-ABBRV = 'NH' OR 'NV' OR 'WA' OR 'AK')
                            OR
                  (CP-BENEFIT-CD = '49' OR '50' OR '53' OR '54'))
011707        GO TO 9999-CALC-RATE-PREM-X
           END-IF
011707     IF (CP-COMPANY-ID = 'CID')
011707        AND (CP-EARN-AS-NET-PAY)
011707        AND (CP-STATE-STD-ABBRV = 'SC')
011707        AND (CP-BENEFIT-CD = '2I' OR '2J' OR '2K' OR '2L')
011707        GO TO 9999-CALC-RATE-PREM-X
           END-IF
CIDMOD     IF (CP-COMPANY-ID = 'CID') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-TRUNCATED-LIFE) AND
CIDMOD        (CP-STATE-STD-ABBRV = 'CA' OR 'VA') AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE) AND
CIDMOD        (WS-DISCOUNT-OPTION NOT = ' ')
CIDMOD        GO TO 9999-CALC-RATE-PREM-X
CIDMOD     END-IF
CIDMOD
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP') AND
120202        (CP-EARN-AS-NET-PAY)
120202        MOVE WS-PREMIUM-RATE  TO CP-PREMIUM-RATE
120202        GO TO 9999-CALC-RATE-PREM-X
120202     END-IF
120202
120202     IF (CP-COMPANY-ID EQUAL 'CID')
120202        AND (CP-EARN-AS-NET-PAY)
120202        AND (CP-BENEFIT-CD (1:1) = 'M' OR 'N' OR 'H')
120202        GO TO 9999-CALC-RATE-PREM-X
120202     END-IF
120202
00756      IF (RT-IS-OB) AND
00757         (CP-COMPANY-ID NOT EQUAL 'CID')
00758         COMPUTE CP-PREMIUM-RATE ROUNDED =
00759                (CP-PREMIUM-RATE * 12) / CP-LOAN-TERM
00760       ELSE
020816         IF (CP-COMPANY-ID = 'DCC' OR 'VPP')
120202            AND (CP-RATE-DEV-PCT NOT = ZEROS)
120202            COMPUTE CP-CALC-PREMIUM ROUNDED =
120202*           COMPUTE CP-PREMIUM-RATE ROUNDED =
120202             (CP-PREMIUM-RATE * CP-RATE-DEV-PCT * 12) /
120202               CP-ORIGINAL-TERM
120202             MOVE CP-CALC-PREMIUM TO CP-PREMIUM-RATE
120202             MOVE ZEROS          TO CP-RATE-DEV-PCT
120202        ELSE
00761            COMPUTE CP-PREMIUM-RATE ROUNDED =
00762                (CP-PREMIUM-RATE * 12) / CP-ORIGINAL-TERM
120202        END-IF
120202     END-IF
00763
00764      IF CP-ROA-PREM-AT-REFUND
00765          COMPUTE NET-BEN ROUNDED =
00766              (CP-REMAINING-BENEFIT - CP-ORIGINAL-PREMIUM) / 1000
00767      ELSE
00768          COMPUTE NET-BEN ROUNDED =
00769               (CP-ORIGINAL-BENEFIT - CP-ORIGINAL-PREMIUM) / 1000.
00770
00771      IF (CP-EXT-CHG-LF OR CP-EXT-CHG-LF-AH) AND
00772         (CP-TERM-OR-EXT-DAYS NOT = ZERO)
00773           COMPUTE NET-BEN ROUNDED =
00774               NET-BEN * (1 + (CP-LOAN-APR / 1200) *
00775                          (CP-TERM-OR-EXT-DAYS / 30)).
00776
CIDMOD     IF CP-COMPANY-ID = 'CDC' OR 'GTL' OR 'CID' OR 'MON' OR 'HAN'
120202*       OR 'DCC'
CIDMOD*    IF CP-COMPANY-ID = 'CDC' OR 'GTL' OR 'MON' OR 'HAN'
00778      COMPUTE CP-CALC-PREMIUM ROUNDED = NET-BEN * 1000 *
00779        (N-P-FACTOR * CP-PREMIUM-RATE * CP-ORIGINAL-TERM / 1200) /
00780        (1 - (N-P-FACTOR * CP-PREMIUM-RATE
00781                         * CP-ORIGINAL-TERM / 1200))
00782      ELSE
00783      COMPUTE CP-CALC-PREMIUM ROUNDED = NET-BEN * 1000 *
00784        (N-P-FACTOR * CP-PREMIUM-RATE * WS-RATE-TERM / 1200) /
00785        (1 - (N-P-FACTOR * CP-PREMIUM-RATE * WS-RATE-TERM / 1200)).
00786
00787 ****** THE BELOW VERSION OF THE NET PAY FORMULA IS USED WHEN NO
00788 ****** INTEREST IS CHARGED ON PREMIUM.
00789
00790 *    IF CP-STATE-STD-ABBRV = 'MI'
00791 *         COMPUTE CP-CALC-PREMIUM ROUNDED = NET-BEN * 1000 *
00792 *          (N-P-FACTOR * CP-PREMIUM-RATE * WS-RATE-TERM / 1200).
00793
00794      MOVE WS-PREMIUM-RATE  TO CP-PREMIUM-RATE.
00795
00796      GO TO 9999-CALC-RATE-PREM-X.
00797
00798 ******
00799 ****** BELOW ARE PREMIUM CALCULATIONS FOR CSL
00800 ******
00801
00802  1250-CSL-CALC.
00803      MOVE CP-CERT-EFF-DT TO DC-BIN-DATE-1.
00804      MOVE CP-FIRST-PAY-DATE TO DC-BIN-DATE-2.
00805      MOVE '1' TO DC-OPTION-CODE.
00806      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
00807
00808      IF DATE-CONVERSION-ERROR
00809          MOVE '2' TO CP-RETURN-CODE
00810          GO TO 9999-CALC-RATE-PREM-X.
00811
00812      IF CP-JOINT-INDICATOR = 'J'
00813          MOVE RT-L-RATE (3) TO WK-RED-RATE
00814      ELSE
00815          MOVE RT-L-RATE (1) TO WK-RED-RATE.
00816
00817      MOVE WK-RED-RATE TO CP-PREMIUM-RATE.
00818      MOVE CP-REMAINING-BENEFIT TO WK-PAY-AMT.
00819
00820      IF CP-PAY-FREQUENCY = 12
00821          MOVE 01               TO WS-PAY-SCH.
00822      IF CP-PAY-FREQUENCY = 04
00823          MOVE 03               TO WS-PAY-SCH.
00824      IF CP-PAY-FREQUENCY = 02
00825          MOVE 06               TO WS-PAY-SCH.
00826      IF CP-PAY-FREQUENCY = 01
00827          MOVE 12               TO WS-PAY-SCH.
00828
00829      COMPUTE WS-NUM-PAYMTS = CP-ORIGINAL-TERM / WS-PAY-SCH.
00830      IF CP-PAY-FREQUENCY NOT = 12
00831          COMPUTE WK-PAY-AMT = CP-REMAINING-BENEFIT /
00832                                 WS-NUM-PAYMTS.
00833
00834      IF CP-STATE = 'SC'  AND
00835        (CP-BENEFIT-CD = '07'  OR  '08')
00836          GO TO 1250-CSL-CALC-LIFE-6.
00837
00838      IF CP-CSL-METH-1
00839          GO TO 1250-CSL-CALC-LIFE-1.
00840
00841      IF CP-CSL-METH-2
00842          IF (WK-PAY-AMT * CP-LOAN-TERM) GREATER THAN
00843                           WS-MAX-TOT-BEN
00844              GO TO 1250-CSL-CALC-LIFE-2
00845          ELSE
00846              GO TO 1250-CSL-CALC-LIFE-1.
00847
00848      IF CP-CSL-METH-3
00849          IF WK-PAY-AMT GREATER THAN WS-MAX-MON-BEN
00850              GO TO 1250-CSL-CALC-LIFE-2
00851          ELSE
00852              GO TO 1250-CSL-CALC-LIFE-3.
00853
00854      IF CP-CSL-METH-4
00855          IF (WK-PAY-AMT * WS-NUM-PAYMTS) GREATER THAN
00856                           WS-MAX-TOT-BEN
00857              GO TO 1250-CSL-CALC-LIFE-5
00858          ELSE
00859              GO TO 1250-CSL-CALC-LIFE-4.
00860
00861  1250-CSL-CALC-LIFE-1.
00862      IF CP-JOINT-INDICATOR = 'J'
00863          MOVE RT-L-RATE (4) TO WK-LEV-RATE
00864      ELSE
00865          MOVE RT-L-RATE (2) TO WK-LEV-RATE.
00866
00867      COMPUTE WK-D = (((DC-ELAPSED-DAYS * CP-PAY-FREQUENCY) /
00868                     365) - 1) * 365 / 12.
00869      COMPUTE WK-S ROUNDED = ((144 * WK-D * WK-LEV-RATE) +
00870                     (365 * WS-NUM-PAYMTS * WK-RED-RATE)) /
00871                     4380.
00872      COMPUTE CP-CALC-PREMIUM ROUNDED = WK-PAY-AMT *
00873                     WS-NUM-PAYMTS * WK-S.
00874
00875      GO TO 9999-CALC-RATE-PREM-X.
00876
00877  1250-CSL-CALC-LIFE-2.
00878      IF CP-JOINT-INDICATOR = 'J'
00879          MOVE RT-L-RATE (4) TO WK-LEV-RATE
00880      ELSE
00881          MOVE RT-L-RATE (2) TO WK-LEV-RATE.
00882
00883      COMPUTE WK-K = WS-MAX-TOT-BEN / WK-PAY-AMT.
00884      COMPUTE WK-S ROUNDED = (WK-K * WK-RED-RATE) / 12.
00885      COMPUTE WK-D = (((DC-ELAPSED-DAYS * CP-PAY-FREQUENCY / 365)
00886                     - 1) + WS-NUM-PAYMTS - WK-K) *
00887                     (WK-LEV-RATE * WS-MAX-TOT-BEN).
00888      COMPUTE CP-CALC-PREMIUM ROUNDED = (WK-PAY-AMT * WK-K *
00889                     WK-S) + WK-D.
00890
00891      GO TO 9999-CALC-RATE-PREM-X.
00892
00893  1250-CSL-CALC-LIFE-3.
00894      IF CP-JOINT-INDICATOR = 'J'
00895          MOVE RT-L-RATE (4) TO WK-LEV-RATE
00896      ELSE
00897          MOVE RT-L-RATE (2) TO WK-LEV-RATE.
00898
00899      COMPUTE WK-K = WS-MAX-TOT-BEN / WK-PAY-AMT.
00900      COMPUTE WK-S ROUNDED = (WK-K * WK-RED-RATE) / 12.
00901      COMPUTE WK-D = (((DC-ELAPSED-DAYS * CP-PAY-FREQUENCY / 365)
00902                     - 1) + WS-NUM-PAYMTS - WK-K) *
00903                     (WK-LEV-RATE * WS-MAX-TOT-BEN).
00904      COMPUTE CP-CALC-PREMIUM ROUNDED = (WK-PAY-AMT * WK-K *
00905                     WK-S) + WK-D.
00906
00907      GO TO 9999-CALC-RATE-PREM-X.
00908
00909  1250-CSL-CALC-LIFE-4.
00910      IF CP-JOINT-INDICATOR = 'J'
00911          MOVE RT-L-RATE (4) TO WK-LEV-RATE
00912      ELSE
00913          MOVE RT-L-RATE (2) TO WK-LEV-RATE.
00914
00915      COMPUTE WK-D = DC-ELAPSED-DAYS * 12 / 365.
00916      COMPUTE WK-S ROUNDED =
00917                     (((6 * (WS-NUM-PAYMTS - 1)) + (WK-D *
00918                     CP-PAY-FREQUENCY)) * WK-LEV-RATE) /
00919                     CP-PAY-FREQUENCY.
00920      COMPUTE CP-CALC-PREMIUM ROUNDED =  WK-PAY-AMT *
00921                     WS-NUM-PAYMTS * WK-S.
00922
00923      GO TO 9999-CALC-RATE-PREM-X.
00924
00925  1250-CSL-CALC-LIFE-5.
00926      IF CP-JOINT-INDICATOR = 'J'
00927          MOVE RT-L-RATE (4) TO WK-LEV-RATE
00928      ELSE
00929          MOVE RT-L-RATE (2) TO WK-LEV-RATE.
00930
00931      COMPUTE WK-K = WS-MAX-TOT-BEN / WK-PAY-AMT.
00932      COMPUTE WK-S ROUNDED = (6 * (WK-K + 1) * WK-LEV-RATE) /
00933                     CP-PAY-FREQUENCY.
00934      COMPUTE WK-U = ((((DC-ELAPSED-DAYS * CP-PAY-FREQUENCY / 365)
00935                     - 1) + WS-NUM-PAYMTS) * 12) /
00936                     CP-PAY-FREQUENCY.
00937      COMPUTE WK-L = WK-U - (12 * WK-K / CP-PAY-FREQUENCY).
00938      COMPUTE CP-CALC-PREMIUM ROUNDED = (WK-L * WK-LEV-RATE *
00939                     WS-MAX-TOT-BEN) + (WK-K * WK-PAY-AMT * WK-S).
00940
00941      GO TO 9999-CALC-RATE-PREM-X.
00942
00943  1250-CSL-CALC-LIFE-6.
00944
00945      COMPUTE CP-PREMIUM-RATE ROUNDED =
00946                 (CP-PREMIUM-RATE * CP-ORIGINAL-TERM) / 12.
00947
00948      COMPUTE CP-CALC-PREMIUM ROUNDED =
00949                     CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE.
00950
00951      GO TO 9999-CALC-RATE-PREM-X.
00952
00953 ******
00954 ****** END OF PREMIUM CALCULATIONS FOR CSL
00955 ******
00956
00957  1260-GTL-PA-LEASE.
00958
00959      IF CP-REDUCING-LIFE
00960          MOVE RT-L-RATE (CP-ORIGINAL-TERM)
00961                                  TO  CP-PREMIUM-RATE
00962          COMPUTE CP-CALC-PREMIUM ROUNDED EQUAL
00963                  CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE / +100
00964      ELSE
00965          MOVE RT-L-RATE (CP-ORIGINAL-TERM)
00966                                  TO  CP-PREMIUM-RATE
00967          COMPUTE CP-CALC-PREMIUM ROUNDED EQUAL
00968                  CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE *
00969                  (+1 + CP-SALES-TAX) / +100.
00970
00971      GO TO 9999-CALC-RATE-PREM-X.
00972
00973      EJECT
00974 ******************************************************************
00975  2000-TYPE-IS-AH.
00976      MOVE RT-MAX-AGE                TO CP-R-MAX-ATT-AGE.
00977
00978      MOVE ZERO                      TO CP-R-MAX-AGE
00979                                        CP-R-MAX-TERM
00980                                        CP-R-MAX-MON-BEN
00981                                        CP-R-MAX-TOT-BEN.
00982
020816     IF RT-CANCEL-FEE NUMERIC
020816        MOVE RT-CANCEL-FEE          TO CP-CANCEL-FEE.
00985
00986      IF RT-COMPOSITE-RATE NOT NUMERIC
00987         MOVE ZEROS                          TO RT-COMPOSITE-RATE.
00988
00989      IF CP-ORIGINAL-TERM > ZERO AND < 361
00990         MOVE RT-AH-RATE (CP-ORIGINAL-TERM)
                                       TO CP-PREMIUM-RATE
00991                                     WS-PREMIUM-RATE
00992         MOVE RT-COMPOSITE-RATE   TO CP-COMPOSITE-RATE
00993      ELSE
00994          MOVE +0                 TO CP-PREMIUM-RATE
00995                                     CP-CALC-PREMIUM
00996          GO TO 9999-CALC-RATE-PREM-X
           END-IF
00997
020816     IF (CP-COMPANY-ID = 'DCC' OR 'VPP')
022613        AND (CP-DDF-SPEC-CALC = 'T')
022613        AND (CP-LOAN-TERM > ZERO AND < 361)
022613        MOVE RT-AH-RATE (CP-LOAN-TERM)
022613                                 TO CP-PREMIUM-RATE
022613                                    WS-PREMIUM-RATE
022613     END-IF
022613*    IF (CP-COMPANY-ID = 'DCC')
022613*       AND ((CP-BENEFIT-CD (1:1) = 'P')
022613*       OR (CP-DDF-SPEC-CALC = 'T'))
022613*       AND (CP-LOAN-TERM > ZERO AND < 361)
022613*       MOVE RT-AH-RATE (CP-LOAN-TERM)
022613*                                TO CP-PREMIUM-RATE
022613*                                   WS-PREMIUM-RATE
022613*    END-IF
00998      MOVE +1 TO AHNDX.
00999
01000      IF CP-COMPANY-ID EQUAL 'CRI'
01001         IF CP-SIG-SWITCH EQUAL 'S' OR 'J'
01002            MOVE +5               TO AHNDX
01003            IF RT-AH-AGE (AHNDX) EQUAL ZERO
01004               MOVE 'F'           TO CP-RETURN-CODE
01005               GO TO 2200-CALCULATE-PREMIUMS.
01006
01007      IF RT-AH-AGE (AHNDX) = ZERO
01008           GO TO 2200-CALCULATE-PREMIUMS.
01009
01010  2100-SEARCH-AH-LIMITS.
01011
01012      IF CP-COMPANY-ID EQUAL 'CRI'
01013         IF CP-SIG-SWITCH NOT EQUAL 'S' AND 'J'
01014            IF AHNDX GREATER THAN +4
01015               MOVE 'A'             TO CP-RETURN-CODE
01016               GO TO 2200-CALCULATE-PREMIUMS.
01017
01018      IF AHNDX GREATER +8
01019          MOVE 'A'                TO CP-RETURN-CODE
01020          GO TO 2200-CALCULATE-PREMIUMS.
01021
01022      IF CP-ISSUE-AGE GREATER RT-AH-AGE (AHNDX)
01023          IF RT-AH-AGE (AHNDX) = ZEROS
01024              ADD +1               TO AHNDX
01025              GO TO 2100-SEARCH-AH-LIMITS
01026          ELSE
01027              MOVE RT-AH-AGE (AHNDX)
01028                                  TO CP-R-MAX-AGE
01029              MOVE RT-AH-TERM (AHNDX)
01030                                  TO CP-R-MAX-TERM
01031              MOVE RT-AH-BEN-M (AHNDX)
01032                                  TO CP-R-MAX-MON-BEN
01033              ADD +1               TO AHNDX
01034              GO TO 2100-SEARCH-AH-LIMITS
01035      ELSE
01036          MOVE RT-AH-AGE  (AHNDX) TO CP-R-MAX-AGE
01037          MOVE RT-AH-TERM (AHNDX) TO CP-R-MAX-TERM
01038          MOVE RT-AH-BEN-M (AHNDX)
01039                                  TO CP-R-MAX-MON-BEN.
01040
01041      IF CP-ORIGINAL-TERM GREATER RT-AH-TERM (AHNDX)
01042          MOVE '9'                 TO CP-RETURN-CODE.
01043
01044      IF CP-ORIGINAL-BENEFIT GREATER RT-AH-BEN-M (AHNDX)
01045          MOVE '8'                 TO CP-RETURN-CODE.
01046
01047      MULTIPLY CP-ORIGINAL-BENEFIT BY CP-ORIGINAL-TERM
01048          GIVING WS-AH-FACE-BENEFIT.
01049
01050      IF WS-AH-FACE-BENEFIT GREATER RT-AH-BEN-F (AHNDX)
01051          MOVE 'C'                 TO CP-RETURN-CODE.
01052
01053  2200-CALCULATE-PREMIUMS.
01054
01055      IF CP-COMPANY-ID = 'CSL'
01056          GO TO 2250-CSL-CALC.
01057
120903*    IF CP-CRITICAL-PERIOD
120903*       GO TO 2200-CRITICAL-PERIOD-AH-CONT.
01060
120903     IF CP-CRITICAL-PERIOD
020816        IF (CP-COMPANY-ID = 'DCC' OR 'VPP')
120903           AND ((CP-BENEFIT-CD (1:1) = 'N' OR 'M' OR 'H'
                     OR 'S' OR 'P')
092205                          OR
092205                (CP-BEN-CATEGORY = 'G' OR 'L'))
120903           CONTINUE
120903        ELSE
120903           GO TO 2200-CRITICAL-PERIOD-AH-CONT
120903        END-IF
120903     END-IF
01060
020816     IF (CP-COMPANY-ID = 'DCC' OR 'VPP')
011410        AND (CP-BEN-CATEGORY = 'D')
011410        COMPUTE CP-PREMIUM-RATE ROUNDED = CP-PREMIUM-RATE *
011410           (1 + CP-DCC-PMF-COMM) / (1 - CP-DCC-ACT-COMM)
011410     END-IF
021519     IF (CP-COMPANY-ID = 'CID')
021519        AND (CP-STATE-STD-ABBRV = 'AL')
072419        and (cp-pmt-mode <> spaces and low-values)
021519        continue
021519     else
021519        go to 2200-continue
021519     end-if
021519
021519     evaluate true
021519        when cp-pmt-mode = 'B' *> Bi-Weekly
021519           move 26               to c-pmts-per-year
021519           move 14               to c-days-in-pmt-per
021519        when cp-pmt-mode = 'W' *> Weekly
021519           move 52               to c-pmts-per-year
021519           move 07               to c-days-in-pmt-per
021519        when cp-pmt-mode = 'S' *> Semi-Monthly
021519           move 24               to c-pmts-per-year
021519           move 15               to c-days-in-pmt-per
021519        when cp-pmt-mode = 'A' *> Annual
021519           move 1                to c-pmts-per-year
021519           move 360              to c-days-in-pmt-per
021519        when cp-pmt-mode = 'T' *> Semi-Annual
021519           move 2                to c-pmts-per-year
021519           move 180              to c-days-in-pmt-per
021519        when cp-pmt-mode = 'Q' *> Quarterly
021519           move 4                to c-pmts-per-year
021519           move 90               to c-days-in-pmt-per
021519        when cp-pmt-mode = 'N' *> Bi-Monthly ??
021519           move 6                to c-pmts-per-year
021519           move 60               to c-days-in-pmt-per
021519        when other             *> Assume Monthly
021519           move 12               to c-pmts-per-year
021519           move 30               to c-days-in-pmt-per
021519     end-evaluate
021519
021519     compute c-tot-pmts =
021519        cp-original-term / 12 * c-pmts-per-year
021519
021519     compute wk7 rounded =    *> tv3
021519        (cp-original-benefit * +12) / c-pmts-per-year
021519
021519     compute wk6 rounded =
021519        (wk7 * c-tot-pmts / 100) * cp-premium-rate
021519
021519     move wk6                    to cp-calc-premium
021519
021519     GO TO 9999-CALC-RATE-PREM-X
021519
021519     .
021519 2200-continue.
01061      IF CP-SPECIAL-CALC-CD EQUAL 'H'
01062         COMPUTE WS-TERM-MINUS-ONE EQUAL CP-ORIGINAL-TERM - +1
01063         MOVE RT-AH-RATE (WS-TERM-MINUS-ONE) TO
01064                                      CP-PREMIUM-RATE
01065         COMPUTE CP-CALC-PREMIUM ROUNDED EQUAL
01066         (CP-ORIGINAL-BENEFIT * (CP-LOAN-TERM - +1) *
01067         CP-PREMIUM-RATE) / +100
01068      ELSE
020816        IF (CP-COMPANY-ID = 'DCC' OR 'VPP')
100703           AND (CP-BEN-CATEGORY NOT = 'G' AND 'L')
011904           MOVE CP-PREMIUM-RATE  TO WK-S
011904*          MOVE CP-PREMIUM-RATE  TO CP-CALC-PREMIUM
011904           IF CP-RATE-DEV-PCT NOT = ZEROS
120202              COMPUTE WK-S ROUNDED =
120202                 (CP-PREMIUM-RATE * CP-RATE-DEV-PCT)
011904           END-IF
120202           IF CP-BENEFIT-CD (1:1) = 'M'
120202              COMPUTE CP-CALC-PREMIUM ROUNDED =
120202              CP-ORIGINAL-BENEFIT * WK-S / 100
120202           ELSE
120202              IF CP-BENEFIT-CD (1:1) = 'H'
120202                 COMPUTE CP-CALC-PREMIUM ROUNDED =
120202                    (CP-ORIGINAL-BENEFIT * CP-LOAN-TERM *
120202                    WK-S) / 1000
011904              ELSE
021005                 IF CP-BENEFIT-CD (1:1) = 'N' OR 'S'
                          GO TO 2300-CALC-DCC-MOB-NET-AH
120202                 ELSE
011410                    IF CP-BEN-CATEGORY = 'D'
011410                       COMPUTE CP-CALC-PREMIUM ROUNDED =
011410                        CP-ORIGINAL-BENEFIT *
011410                        CP-ORIGINAL-TERM * WK-S / 100
011410                    ELSE
                            IF CP-BENEFIT-CD (1:1) = 'P'
                               GO TO 2300-CALC-DCC-BEN-P-AH
                            ELSE
120202                       COMPUTE CP-CALC-PREMIUM ROUNDED =
120202                        (CP-ORIGINAL-BENEFIT * CP-LOAN-TERM *
120202                        WK-S) / 100
                            END-IF
011410                    END-IF
011904                 END-IF
120202              END-IF
120202           END-IF
120202           MOVE ZEROS            TO CP-RATE-DEV-PCT
120202        ELSE
120202           IF CP-BENEFIT-CD (1:1) = 'M'
120202              COMPUTE CP-CALC-PREMIUM ROUNDED =
120202                 CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE / 100
120202           ELSE
120202              IF CP-BENEFIT-CD (1:1) = 'N' OR 'H'
120202                 COMPUTE CP-CALC-PREMIUM ROUNDED =
120202                    (CP-NET-BENEFIT-AMT *
120202                    CP-PREMIUM-RATE) / 1000
120202              ELSE
                       IF CP-AH-BALLOON-SW = 'Y'
                          COMPUTE CP-CALC-PREMIUM ROUNDED =
                            (CP-ORIGINAL-BENEFIT * (CP-LOAN-TERM) *
                            CP-PREMIUM-RATE) / 100
                       ELSE
120202                   COMPUTE CP-CALC-PREMIUM ROUNDED =
020304                    (CP-ORIGINAL-BENEFIT * CP-ORIGINAL-TERM *
01071                     CP-PREMIUM-RATE) / 100
                       END-IF
120202              END-IF
120202           END-IF
120202        END-IF
120202     END-IF
01072
020816     IF (CP-COMPANY-ID = 'DCC' OR 'VPP')
100703        AND (CP-BEN-CATEGORY = 'G' OR 'L')
100703        COMPUTE CP-CALC-PREMIUM = CP-PREMIUM-RATE * +1000
100703     END-IF
01073      IF CP-TERM-OR-EXT-DAYS = ZEROS
01074          GO TO 9999-CALC-RATE-PREM-X.
01075
01076      IF CP-EXT-CHG-AH OR CP-EXT-CHG-LF-AH
01077          COMPUTE CP-CALC-PREMIUM ROUNDED =
01078              CP-CALC-PREMIUM +
01079                  (((CP-CALC-PREMIUM / CP-ORIGINAL-TERM) / 30)
01080                                        * CP-TERM-OR-EXT-DAYS).
01081
01082      GO TO 9999-CALC-RATE-PREM-X.
01083
01084  2200-CRITICAL-PERIOD-AH-CONT.
01085
01086      IF CP-COMPANY-ID = 'WDS'
01087          IF CP-ORIGINAL-TERM GREATER THAN 60
01088              MOVE 60              TO CP-LOAN-TERM.
01089
01090      IF CP-COMPANY-ID EQUAL 'CVL'
01091          COMPUTE CP-CALC-PREMIUM ROUNDED =
01092            ((CP-ORIGINAL-BENEFIT * CP-CRITICAL-MONTHS) / +100)
01093                                        * CP-PREMIUM-RATE
01094          GO TO 9999-CALC-RATE-PREM-X.
01095
01096      COMPUTE CP-CALC-PREMIUM ROUNDED =
020304           ((CP-ORIGINAL-BENEFIT * CP-ORIGINAL-TERM) / +100)
01098                                        * CP-PREMIUM-RATE.
01099
01100      GO TO 9999-CALC-RATE-PREM-X.
01101
01102  2250-CSL-CALC.
01103
01104      IF RT-DAILY-RATE NOT NUMERIC OR
01105         RT-DAILY-RATE = ZEROS
01106             MOVE ZEROS TO CP-CALC-PREMIUM
01107             MOVE '7' TO CP-RETURN-CODE
01108             GO TO 9999-CALC-RATE-PREM-X.
01109
01110      COMPUTE WK-DAILY-RATE = RT-DAILY-RATE / 100.
01111      COMPUTE WK-MONTHLY-RATE =
01112           RT-AH-RATE (CP-ORIGINAL-TERM) / 100.
01113
01114      MOVE CP-CERT-EFF-DT TO DC-BIN-DATE-1.
01115      MOVE CP-FIRST-PAY-DATE TO DC-BIN-DATE-2.
01116      MOVE '1' TO DC-OPTION-CODE.
01117      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
01118      IF DATE-CONVERSION-ERROR
01119          MOVE '2' TO CP-RETURN-CODE
01120          GO TO 9999-CALC-RATE-PREM-X.
01121
01122      MOVE CP-REMAINING-BENEFIT TO WK-PAY-AMT.
01123      IF CP-CSL-METH-1
01124          GO TO 2250-CSL-CALC-AH-1.
01125      IF CP-CSL-METH-2
01126          GO TO 2250-CSL-CALC-AH-2.
01127      IF CP-CSL-METH-3
01128          IF WK-PAY-AMT GREATER THAN WS-MAX-MON-BEN
01129              GO TO 2250-CSL-CALC-AH-4
01130          ELSE
01131              GO TO 2250-CSL-CALC-AH-3.
01132
01133  2250-CSL-CALC-AH-1.
01134
01135      COMPUTE WK-D = DC-ELAPSED-DAYS * CP-PAY-FREQUENCY / 365.
01136      COMPUTE WK-D = (((WK-D - 1) * (365 / 12)) * WK-DAILY-RATE)
01137                     + WK-MONTHLY-RATE.
01138      COMPUTE CP-CALC-PREMIUM ROUNDED = CP-MONTHLY-PAYMENT *
01139                     CP-ORIGINAL-TERM * WK-D.
01140
01141      GO TO 9999-CALC-RATE-PREM-X.
01142
01143
01144  2250-CSL-CALC-AH-2.
01145      COMPUTE WK-D = DC-ELAPSED-DAYS * CP-PAY-FREQUENCY / 365.
01146      COMPUTE WK-D = (((WK-D - 1) * (365 / 12)) * WK-DAILY-RATE)
01147                     + WK-MONTHLY-RATE.
01148      COMPUTE CP-CALC-PREMIUM ROUNDED = WS-MAX-MON-BEN *
01149                     CP-ORIGINAL-TERM * WK-D.
01150
01151      GO TO 9999-CALC-RATE-PREM-X.
01152
01153  2250-CSL-CALC-AH-3.
01154
01155      COMPUTE WK-K = WS-MAX-TOT-BEN / WK-PAY-AMT.
01156      COMPUTE WK-T = WK-K + 1.
01157      COMPUTE WK-MONTHLY-RATE-T =
01158           RT-AH-RATE (WK-T) / 100.
01159      COMPUTE WK-C = (CP-ORIGINAL-TERM - (WK-T * 2) + 1).
01160
01161      COMPUTE WK-T1 = WK-K * (CP-ORIGINAL-TERM - WK-T).
01162      IF WK-T1 EQUAL ZEROS
01163          MOVE ZEROS TO CP-CALC-PREMIUM
01164          MOVE 'G' TO CP-RETURN-CODE
01165          GO TO 9999-CALC-RATE-PREM-X.
01166
01167      COMPUTE WK-C = ((WK-C * WK-MONTHLY-RATE-T) + ((WK-T - 1) *
01168                     WK-MONTHLY-RATE)) / (WK-K * (CP-ORIGINAL-TERM
01169                     - WK-T)).
01170      COMPUTE WK-D = (DC-ELAPSED-DAYS * CP-PAY-FREQUENCY / 365)
01171      COMPUTE WK-PREM = ((CP-ORIGINAL-TERM -
01172                        (WK-K + 1) + (WK-D - 1))) * (WK-C *
01173                        WS-MAX-TOT-BEN).
01174      COMPUTE CP-CALC-PREMIUM ROUNDED = (WK-PAY-AMT * (WK-K + 1) *
01175                     WK-MONTHLY-RATE-T) + WK-PREM.
01176
01177      GO TO 9999-CALC-RATE-PREM-X.
01178
01179  2250-CSL-CALC-AH-4.
01180      COMPUTE WK-K = WS-MAX-TOT-BEN / WK-PAY-AMT.
01181      COMPUTE WK-J = WS-MAX-TOT-BEN / WS-MAX-MON-BEN.
01182      COMPUTE WK-MONTHLY-RATE-J =
01183           RT-AH-RATE (WK-J) / 100.
01184      COMPUTE WK-T = WK-J + 1.
01185      COMPUTE WK-MONTHLY-RATE-T =
01186           RT-AH-RATE (WK-T) / 100.
01187      COMPUTE WK-C = (CP-ORIGINAL-TERM - (WK-T * 2) + 1).
01188      COMPUTE WK-C = ((WK-C * WK-MONTHLY-RATE-T) + ((WK-T - 1) *
01189                     WK-MONTHLY-RATE)) / ((WK-T - 1) *
01190                     (CP-ORIGINAL-TERM - WK-T)).
01191      COMPUTE WK-D = (DC-ELAPSED-DAYS * CP-PAY-FREQUENCY / 365)
01192      COMPUTE WK-PREM = (CP-ORIGINAL-TERM -
01193                        (WK-J + 1) + (WK-D - 1)) * (WK-C *
01194                        WS-MAX-TOT-BEN).
01195      COMPUTE CP-CALC-PREMIUM ROUNDED = WK-PREM + ((WK-J + 1) *
01196                    (WS-MAX-MON-BEN * WK-MONTHLY-RATE-T)).
01197
01198      GO TO 9999-CALC-RATE-PREM-X.
01199
       2300-CALC-DCC-BEN-P-AH.
           IF CP-NO-OF-PMTS NOT NUMERIC
              MOVE +0                  TO CP-NO-OF-PMTS
           END-IF
           IF CP-PMT-MODE NOT = 'W' AND 'S' AND 'B' AND 'T'
              AND ' ' AND 'M'
              MOVE ' '                 TO CP-PMT-MODE
           END-IF
022613******************************************************************
022613***   Default is monthly, use loan term for rate and amort     ***
022613******************************************************************
022613     move cp-loan-term         to c-tot-pmts
022613     MOVE +30                  TO C-DAYS-IN-PMT-PER
022613     MOVE +12                  TO C-PMTS-PER-YEAR
022613******************************************************************
022613
022613     evaluate true
022613        when cp-pmt-mode = 'W'
022613           MOVE +52              TO C-PMTS-PER-YEAR
022613           compute c-tot-pmts = cp-loan-term /
022613              12 * c-pmts-per-year
022613           MOVE +7               TO C-DAYS-IN-PMT-PER
022613        when cp-pmt-mode = 'B'
022613           MOVE +26              TO C-PMTS-PER-YEAR
022613           compute c-tot-pmts = cp-loan-term /
022613              12 * c-pmts-per-year
022613           MOVE +14              TO C-DAYS-IN-PMT-PER
022613     end-evaluate
           COMPUTE I = CP-LOAN-APR / C-PMTS-PER-YEAR / 100
050713     if i = zeros
050713        move .0000000001         to i
050713     end-if
           COMPUTE Y = (1 + (CP-DAYS-TO-1ST-PMT * I /
              C-DAYS-IN-PMT-PER)) / (1 + I)
           COMPUTE ANGLEN = (1 - ((1 + I) ** (C-TOT-PMTS * -1))) / I
           COMPUTE ANGLEN-1 = ANGLEN / Y
           COMPUTE WK3 = (CP-NET-BENEFIT-AMT - CP-ORIGINAL-PREMIUM)
              / ANGLEN-1
           if cp-net-benefit-amt = zeros
              compute wk3 = cp-original-benefit - cp-original-premium
           end-if
           COMPUTE CP-CALC-PREMIUM = (WK3 / 100) * WK-S
           GO TO 9999-CALC-RATE-PREM-X
           .
       2300-CALC-DCC-MOB-NET-AH.
           IF CP-ALTERNATE-BENEFIT = ZEROS
              CONTINUE
           ELSE
              GO TO 2300-CALC-DCC-MOB-BAL-AH
           END-IF
           MOVE CP-ORIGINAL-TERM       TO M
           MOVE CP-LOAN-TERM           TO N
           IF N = ZEROS
              MOVE M                   TO N
           END-IF
           COMPUTE I = CP-LOAN-APR / +1200
050713     if i = zeros
050713        move .0000000001         to i
050713     end-if
           IF CP-BENEFIT-CD (1:1) = 'S'
              COMPUTE OD = CP-DAYS-TO-1ST-PMT - 30
              COMPUTE WK3 = CP-ORIGINAL-BENEFIT *
              ((M * (M + 1) / 2) + (M * OD / 30)) *
                (WK-S / 1000)
                * 1
              MOVE WK3                 TO CP-CALC-PREMIUM
              GO TO 9999-CALC-RATE-PREM-X
           END-IF
           COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
              / (1 + I)
011904     COMPUTE ANGLEN-M = ((1 - ((1 + I)
                ** ((N - M) * -1))) / I)
           COMPUTE ANGLEN-M = ANGLEN-M / OD
011904     COMPUTE J = I + (WK-S / +1000)
           COMPUTE WK2 = (1 + J) ** (M * -1)
           COMPUTE ANGLEN-M = ANGLEN-M * WK2
011904     COMPUTE ANGLEM = ((1 - ((1 + J)
                ** (M * -1))) / J) / OD
           IF CP-NET-BENEFIT-AMT = ZEROS
              COMPUTE CP-NET-BENEFIT-AMT = CP-ORIGINAL-BENEFIT
                 * ANGLEM
           END-IF
           COMPUTE RA = CP-NET-BENEFIT-AMT / (ANGLEM + ANGLEN-M)
011904     COMPUTE ANGLEN-M = ((1 - ((1 + I)
                ** ((N - M) * -1))) / I)
           MOVE +0                     TO WK3
011904     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
011904        (WS-COUNTER > M)
011904        COMPUTE ANGLEM = (1 - ((1 + J)
                ** ((M - WS-COUNTER + 1) * -1))) / J
              COMPUTE WK2 = (1 / (1 + j)) ** (M - WS-COUNTER + 1)
              COMPUTE WK5-CSL = RA * (ANGLEM + (ANGLEN-M * WK2))
              COMPUTE WK3 = WK3 + (WK5-CSL * WK-S / +1000)
           END-PERFORM
011904     MOVE WK3                    TO CP-CALC-PREMIUM
           GO TO 9999-CALC-RATE-PREM-X
           .
       2300-CALC-DCC-MOB-BAL-AH.
           IF CP-BENEFIT-CD (1:1) = 'S'
              GO TO 2300-CALC-DCC-MOB-BAL-AHSUM
           END-IF
           MOVE CP-ORIGINAL-TERM       TO M
           MOVE CP-LOAN-TERM           TO N
           IF N = ZEROS
              MOVE M                   TO N
           END-IF
           COMPUTE I = CP-LOAN-APR / +1200
050713     if i = zeros
050713        move .0000000001         to i
050713     end-if
           COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
              / (1 + I)
           COMPUTE ANGLEN-M = (WK5 / I) / OD
011904     COMPUTE J = I + (WK-S / +1000)
011904     COMPUTE K = I + (CP-DCC-LF-RATE / +1000)
           MOVE I TO K
           COMPUTE WK2 = (1 + J) ** (M * -1)
           COMPUTE ANGLEN-M = ANGLEN-M * WK2
011904     COMPUTE ANGLEM-1 = ((1 - ((1 + J)
                ** ((M - 1) * -1))) / J)
011904     COMPUTE ANGLEM = ((1 - ((1 + J)
                ** (M * -1))) / J) / OD
           IF CP-NET-BENEFIT-AMT = ZEROS
              COMPUTE CP-NET-BENEFIT-AMT = CP-ORIGINAL-BENEFIT
                 * ANGLEM
           END-IF
           COMPUTE RA = (CP-NET-BENEFIT-AMT * OD -
              (CP-ALTERNATE-BENEFIT * (1 + K) ** (M * -1)))
              / ANGLEM-1
011904     COMPUTE ANGLEN-M = ((1 - ((1 + I)
                ** ((N - M) * -1))) / I)
           MOVE +0                     TO WK3
011904     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
011904        (WS-COUNTER > M)
011904        COMPUTE ANGLEM = (1 - ((1 + J)
                ** ((M - WS-COUNTER) * -1))) / J
              COMPUTE WK2 = (1 / (1 + K)) ** (M - WS-COUNTER + 1)
              COMPUTE WK5-CSL = (RA * ANGLEM)
              + (CP-ALTERNATE-BENEFIT * WK2)
              COMPUTE WK3 = WK3 + (WK5-CSL - (CP-ALTERNATE-BENEFIT *
                (1 / (1 + K) ** (M - WS-COUNTER + 1)))) *
                (WK-S / 1000)
           END-PERFORM
011904     MOVE WK3                    TO CP-CALC-PREMIUM
           GO TO 9999-CALC-RATE-PREM-X
           .
       2300-CALC-DCC-MOB-BAL-AHSUM.
           MOVE CP-ORIGINAL-TERM       TO M
           COMPUTE WK3 = CP-ORIGINAL-BENEFIT * ((M - 1) * M / 2
              + (M - 1) * (CP-DAYS-TO-1ST-PMT - 30) / 30)
              * WK-S / 1000
      *    COMPUTE WK3 = CP-ORIGINAL-BENEFIT * ((M - 1) * M / 2
      *       + (M - 1) * ((CP-TERM-OR-EXT-DAYS + 30) - 30) / 30)
      *       * WK-S / 1000
011904     MOVE WK3                    TO CP-CALC-PREMIUM
           GO TO 9999-CALC-RATE-PREM-X
           .
01200  9999-RATE-NOTFND.
01201      MOVE '6'                    TO CP-RETURN-CODE.
01202      GO TO 9999-CALC-RATE-PREM-X.
01203
01204  9999-RATE-NOTOPEN.
01205      MOVE 'D'                    TO CP-RETURN-CODE.
01206      GO TO 9999-CALC-RATE-PREM-X.
01207
01208  9999-CALC-RATE-PREM-X.
020816     IF (CP-COMPANY-ID = 'DCC' OR 'VPP')
              AND (CP-BENEFIT-CD (1:1) = 'N' OR 'S' OR 'P')
              CONTINUE
           ELSE
01209         IF CP-RATE-DEV-PCT NOT = ZERO
01210            COMPUTE CP-CALC-PREMIUM =
01211               CP-CALC-PREMIUM * CP-RATE-DEV-PCT.
01212
00170
00171      MOVE CALCULATION-PASS-AREA  TO  DFHCOMMAREA.
00172
00173      IF WS-BROWSE-STARTED
00174         
      * EXEC CICS ENDBR
00175 *            DATASET  (ERRATE-FILE-ID)
00176 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003445' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRATE-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00177
00178      
      * EXEC CICS RETURN
00179 *    END-EXEC.
      *    MOVE '.(                    ''   #00003449' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00180
00181  9100-CONVERT-DATE.
00182      
      * EXEC CICS LINK
00183 *        PROGRAM  ('ELDATCV')
00184 *        COMMAREA (DATE-CONVERSION-DATA)
00185 *        LENGTH   (DC-COMM-LENGTH)
00186 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00003453' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00187
00188  9100-EXIT.
00189      EXIT.
00190
00191  EJECT
00192  10000-NET-TERM SECTION.
00193 *                         COPY ERCNETP.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ERCNETP                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.029                         *
00006 *                                                               *
00007 *****************************************************************.
00008 *
00009 *    FIVE PARAMETERS ARE PASSED TO THIS MODULE AND A FACTOR IS
00010 *    RETURNED. PARAMETERS PASSED  - A.P.R. (S999V9999)
00011 *                                   ORIGINAL TERM (S999)
00012 *                                   REMAINING TERM (S999)
00013 *                                   NET PAY OPTION (X)
00014 *                                   LOAN TERM (S999)
00015 *              FACTOR RETURNED IS - FACTOR (S9(4)V9(9))
00016 *
00017 *    FACTOR RETURNED IS MULTIPLIED BY ORIG. FACE TO GET REMAINING
00018 *    FACE. IF ORIGNAL TERM = REMAINING TERM, FACTOR WOULD BE 1,
00019 *    THEREFORE MODULE ASSUMES RATING IS DESIRED AND FACTOR THAT
00020 *    IS RETURNED MAY BE MULTIPLIED BY THOUSANDS OF ORIGINAL FACE
00021 *    AND REGULAR PREMIUM PER $100 PER MONTH TO GET PREMIUM TO BE
00022 *    CHARGED.
00023 *
00024 *    OPTIONS - S = NET SIMPLE
00025 *          SPACE = NET PAY  (1 MO. INTEREST)
00026 *              A = NET PAY  (0 MO. INTEREST)
00027 *              I = NET PAY  (2 MO. INTEREST)
00028 *              J = NET PAY  (3 MO. INTEREST)
00029 *              K = NET PAY  (4 MO. INTEREST)
00030 *              T = TRUNCATED  (0 MO. INTEREST)
00031 *              U = TRUNCATED  (1 MO. INTEREST)
00032 *              V = TRUNCATED  (2 MO. INTEREST)
00033 *              W = TRUNCATED  (3 MO. INTEREST)
00034 *              X = TRUNCATED  (4 MO. INTEREST)
00035 *              R = REFUNDS (REGULAR OR TRUNCATED)
120202******************************************************************
120202*                   C H A N G E   L O G
120202*
120202* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120202*-----------------------------------------------------------------
120202*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120202* EFFECTIVE    NUMBER
120202*-----------------------------------------------------------------
120202* 120202    2001061800003  PEMA  ADD DCC PROCESSING
120502* 120502                   PEMA  ADD SPECIAL IN CALC FOR CID
120403* 120403                   PEMA  ADD SPECIAL TX CALC
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
081205* 081205                   PEMA  ADD DAYS TO 1ST PMT ON 'S'
083105* 083105                   PEMA  ADD DAYS TO 1ST PMT ON 'N'
011707* 011707                   PEMA  ADD SC NP PLUS 6 PMTS
021207* 021207                   PEMA  ADD PA NP
022107* 022107                   PEMA  ADD NH NP AND NP TRUNC
032807* 032807                   PEMA  ADD VT NP PLUS 2
091307* 091307  IR2007090600001  PEMA  REMOVE LOW APR CHECK
110207* 110207    2007010300001  PEMA  FIX PROBLEM WHERE DIFF OF LOAN
110207* 110207      LOAN TERM AND INS TERM IS 1. ALSO ADD TRUNC
110207* 110207      TO 'AL' NET PAY CALC
052808* 052808    2008032000002  PEMA  ADD CODE FOR AK NET PAY
052808* 052808    2008050800003  PEMA  ADD CODE FOR NV NET +2
060308* 060308  IR2008052800002  PEMA  ADD TNP FOR ME
060308* 060308  CR2008032700002  PEMA  ADD EXT DAYS FOR ME
060909* 0600909 CR2008042300002  PEMA  ADD NEW FARM PLAN CALC
010410* 010410  CR2009092300002  PEMA  ADD MN NET CALC
010410* 010410  CR2008021200005  PEMA  ADD MN NET BALLOON
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
071910* 071910  IR2010062900001  PEMA ADD MN NET BALLOON REFUND CALC
071211* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
092311* 092311  IR2011092100001  PEMA  ID 10C MONTHLY
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
111212* 111212  CR2012013100001  PEMA  CHANGES FOR MN AND WA
022613* 022613  CR2011021500001  PEMA  ADD CODE 4 TRUNC BALLOON DCC 10C
050713* 050713  CR2008042200001  PEMA  ADD CODE FOR ZERO APR
030515* 030515  IR2015022300001  PEMA  ADD MN AND WA FARMPLAN CODING
040615* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS FOR NET BALLOON
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
021519* 021519  CR2019021100001  PEMA  non-monthly AL calcs
012820* 012820  CR2020010600001  PEMA  MOD TX NP FORMULA TO HANDLE TRUNC
012820* 012820    CONTINUED   AND TO ADJ AF FOR EXTRA DAYS.
120202******************************************************************
00036
00037      MOVE N-P-APR     TO ANNUAL-INT-RATE.
00038      MOVE N-P-ORIG    TO ORIGINAL-TERM.
00039      MOVE N-P-LOAN    TO LOAN-TERM.
00040      MOVE N-P-REM     TO REMAINING-TERM.
00041      MOVE N-P-OPT     TO OPTION-SW.
00042
00043      MOVE +0 TO FACTOR.
050713     if cp-company-id = 'CID'
050713        if annual-int-rate = +99.9999
050713           go to 99000-error
050713        end-if
050713     else
050713        if annual-int-rate = zero
050713           go to 99000-error
050713        end-if
050713     end-if
00047      IF ORIGINAL-TERM = ZERO
00048          GO TO 99999-RETURN.
00049      IF REMAINING-TERM = ZERO
00050          GO TO 99999-RETURN.
00051      IF REMAINING-TERM GREATER ORIGINAL-TERM
00052          GO TO 99000-ERROR.
00053      IF LOAN-TERM = ZERO
00054          GO TO 99999-RETURN.
00055      IF LOAN-TERM LESS ORIGINAL-TERM
00056          GO TO 99000-ERROR.
00057
091307*    IF ANNUAL-INT-RATE LESS +2
091307*        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.
091307*    IF ANNUAL-INT-RATE LESS +2
091307*        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.
091307*    IF ANNUAL-INT-RATE LESS +2
091307*        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.
00064
00065      IF NPO-REFUND
00066          MOVE '2' TO NP-PROCESS-SW
00067        ELSE
00068          IF ORIGINAL-TERM = REMAINING-TERM
00069             MOVE '1'   TO NP-PROCESS-SW
00070            ELSE
00071             MOVE '3'   TO NP-PROCESS-SW.
00072
00073      IF NPO-SIMPLE
00074          MOVE 'S' TO TYPE-SW
00075      ELSE
00076          MOVE 'N' TO TYPE-SW.
00077
00078      COMPUTE I ROUNDED = (ANNUAL-INT-RATE / K100) / K12.
050713     if i = zeros
050713        move .0000000001         to i
050713     end-if
00079
00080      COMPUTE V ROUNDED = K1 / (K1 + I).
00081
00082      MOVE V     TO VX
00083                    SV.
00084
00085      MOVE +1    TO X1
00086                    SX.
00087
00088      MOVE LOAN-TERM TO MAX-X.
00089
00090      COMPUTE X2 = MAX-X - ORIGINAL-TERM.
00091
00092      IF MAX-X = +1
00093         GO TO 10700-COMPUTE-REMAINING-FACTOR.
00094
00095      COMPUTE EXPIRED-TERM = ORIGINAL-TERM - REMAINING-TERM.
00096
00097      IF (CP-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'CID' OR 'MON'
020816         OR 'DCC' or 'VPP' OR 'NCL') AND
00099         (NOT NP-REFUND)
00100         NEXT SENTENCE
00101      ELSE
00102      IF LOAN-TERM NOT = ORIGINAL-TERM
00103         COMPUTE REMAINING-TERM = LOAN-TERM - EXPIRED-TERM.
00104
00105      COMPUTE SV-MINUS-ONE EQUAL ORIGINAL-TERM - +1.
00106
00107  10500-VX-LOOP.
00108
00109 ***  VX = AMORT. LOAN TERM.
00110 ***  SV = AMORT. INSURANCE TERM.
00111 ***  SV = AMORT. REMAINING TERM FOR REFUND CALCS.
00112 ***  SX = AMORT. LOAN TERM - INSURANCE TERM. (USED FOR TRUNCATED.
00113 ***  SV-MINUS-ONE = INSURANCE TERM - +1... (TMS ONLY)
00114
110207     IF X2 = 1
110207        MOVE V         TO SX
110207     END-IF
00115      COMPUTE VX ROUNDED = VX * V.
00116
00117      ADD B1 TO X1.
00118
00119      IF X1 = REMAINING-TERM
00120           MOVE VX    TO SV.
00121
00122      IF X1 EQUAL SV-MINUS-ONE
00123         MOVE VX      TO SV-MINUS-ONE.
00124
00125      IF X1 = X2
00126           MOVE VX    TO SX.
00127
00128      IF X1 NOT = MAX-X
00129           GO TO 10500-VX-LOOP.
00130
00131  10700-COMPUTE-REMAINING-FACTOR.
      *****  WK1 = LOAN TERM AMORT
00132      COMPUTE WK1 = K1 - VX.
      *****  WK2 = INS TERM AMORT
00133      COMPUTE WK2 = K1 - SV.
      *****  WK5 = LOAN TERM - INS TERM AMORT (TRUNCATED)
00134      COMPUTE WK5 = K1 - SX.
      *****  WK6 = INS TERM - 1
00135      COMPUTE WK6 = K1 - SV-MINUS-ONE.
00136
00137      IF NP-RATING
00138          GO TO 12000-PREMIUM-RATE.
00139
00140      IF NP-REFUND
00141          GO TO 11000-REFUND-CALC.
00142
00143      IF NET-STD
00144          COMPUTE WK3 ROUNDED = (WK2 * K1000) / WK1.
00145
00146      IF NET-SMP
00147          COMPUTE WK3 ROUNDED = ((R + 1) / (N + 1)) * (R / N)
00148          COMPUTE WK3 ROUNDED = (1 - WK3) * ((I * N / WK1) - 1)
00149          COMPUTE WK3 ROUNDED = WK3 + 1 - ((N - R) * I / WK1)
00150          COMPUTE WK3 ROUNDED = WK3 * 1000.
00151
00152      IF REMAINING-TERM LESS THAN X2
00153          MOVE +0 TO WK3.
00154
00155      MOVE WK3 TO FACTOR.
00156
00157      GO TO 99999-RETURN.
00158
00159  11000-REFUND-CALC.
00160      IF REMAINING-TERM NOT LESS MAX-X
00161         MOVE +1 TO FACTOR
00162         GO TO 99999-RETURN.
00163
00164      IF REMAINING-TERM LESS +1
00165         MOVE 0 TO FACTOR
00166         GO TO 99999-RETURN.
00167
00168      COMPUTE WK2 ROUNDED = WK2 / I.
00169      COMPUTE WK5 ROUNDED = WK5 / I.
00170      COMPUTE WK1 ROUNDED = WK1 / I.
00171
071910     IF (CP-COMPANY-ID EQUAL 'CID')
071910        AND (CP-STATE-STD-ABBRV = 'MN')
071910        AND (CP-CERT-EFF-DT > X'A4FF')
071910        AND (CP-RATE-AS-REG-BALLOON)
071910        AND (CP-EARN-AS-NET-PAY)
071910        AND (CP-DEVIATION-CODE NOT = 'LEV')
071910        CONTINUE
071910     ELSE
071910        GO TO 11000-CHECK-MN-LEV-BALLOON
071910     END-IF
071910
071910     COMPUTE R = M - E
071910     COMPUTE FACTOR = (R - WK2 + WK5) /
071910        (M - WK1 + WK5)
071910
071910     GO TO 99999-RETURN
071910
071910      .
071910 11000-CHECK-MN-LEV-BALLOON.
071910
071910     IF (CP-COMPANY-ID EQUAL 'CID')
071910        AND (CP-STATE-STD-ABBRV = 'MN')
071910        AND (CP-CERT-EFF-DT > X'A4FF')
071910        AND (CP-RATE-AS-REG-BALLOON)
071910        AND (CP-EARN-AS-NET-PAY)
071910        AND (CP-DEVIATION-CODE = 'LEV')
071910        CONTINUE
071910     ELSE
071910        GO TO 11000-CONTINUE-REFUND
071910     END-IF
071910
071910     COMPUTE ANGLEM = (1 - (1 / (1 + I)) ** M) / I
071910     COMPUTE ANGLEM-T = (1 - (1 / (1 + I))
071910        ** (M - E)) / I
071910
071910     COMPUTE FACTOR = ANGLEM-T / ANGLEM
071910
071910     GO TO 99999-RETURN
071910
071910     .
071910 11000-CONTINUE-REFUND.
033104*    IF CP-GAP-ACTUARIAL
033104*       COMPUTE ANGLEN = WK1 / ((1 + I) **
033104*          (CP-TERM-OR-EXT-DAYS * 12 / 365))
033104*       COMPUTE WK3 = WK2 / ANGLEN
      *       MOVE 'S'                 TO CP-REFUND-TYPE-USED
033104     IF CP-GAP-ACTUARIAL
033104        COMPUTE ANGLEN = WK1 / ((1 + I) **
033104           (CP-TERM-OR-EXT-DAYS * 12 / 365))
033104        COMPUTE WK3 = (R - WK2) / (M - ANGLEN)
033104     ELSE
00172         COMPUTE WK3 ROUNDED =
00173           (N-P-REM - WK2 + WK5) / (ORIGINAL-TERM - WK1 + WK5)
033104     END-IF
00174
00175      MOVE WK3  TO FACTOR.
00176
00177      GO TO 99999-RETURN.
00178
00179  12000-PREMIUM-RATE.
00180 *    K-I IS ADJUSTMENT FACTOR FOR NO. MONTHS ADD'L. INTEREST
00181 *      OPTION - N OR U OR SPACE  = 1 MO,  SO K-I = 1 + I
00182 *      OPTION - A OR T           = 0 MO,  SO K-I = 1
00183 *      OPTION - I OR V           = 2 MO,  SO K-I = 1 + 2I
00184 *      OPTION - J OR W           = 3 MO,  SO K-I = 1 + 3I
00185 *      OPTION - K OR X           = 4 MO,  SO K-I = 1 + 4I
00186
00187      COMPUTE K-I = K1 + I.
00188      MOVE K-I TO ONE-PLUS-I.
00189
00190      IF NPO-ALT OR NPO-TRUNC-0
00191          MOVE K1 TO K-I.
00192
00193      IF NPO-2MO OR NPO-TRUNC-2
00194          COMPUTE K-I = K1 + (2 * I).
00195
00196      IF NPO-3MO OR NPO-TRUNC-3
00197          COMPUTE K-I = K1 + (3 * I).
00198
00199      IF NPO-4MO OR NPO-TRUNC-4
00200          COMPUTE K-I = K1 + (4 * I).
00201
00202      COMPUTE RA ROUNDED = 1 -
00203              ((X2 * (X2 + 1)) /
00204               (N *  (N  + 1))).
00205
00206      IF (CP-COMPANY-ID EQUAL 'CSL') AND
00207         (CP-CSL-VALID-NP-BENEFIT-CD)
00208         NEXT SENTENCE
00209      ELSE
00210         GO TO XXXX-CHECK-DISCOUNT-5.
00211
00212      MOVE CP-CERT-EFF-DT TO DC-BIN-DATE-1.
00213      MOVE ' ' TO DC-OPTION-CODE.
00214      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
00215
00216      IF DATE-CONVERSION-ERROR
00217         MOVE '2'                 TO CP-RETURN-CODE
00218         MOVE ZEROS               TO FACTOR
00219                                     CP-CALC-PREMIUM
00220          GO TO 99999-RETURN.
00221
00222      MOVE DC-JULIAN-DATE-1 TO WS-INT-BEGIN-DATE
00223      MOVE CP-FIRST-PAY-DATE TO DC-BIN-DATE-1
00224      MOVE ' ' TO DC-OPTION-CODE
00225      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
00226
00227      IF DATE-CONVERSION-ERROR
00228         MOVE '2'                 TO CP-RETURN-CODE
00229         MOVE ZEROS               TO FACTOR
00230                                     CP-CALC-PREMIUM
00231          GO TO 99999-RETURN.
00232
00233      MOVE DC-JULIAN-DATE-1 TO WS-DUE-DATE.
00234
00235      COMPUTE ANGLEN-1 EQUAL 1 + (WK6 / I).
00236
00237      IF (WS-DUE-DATE - WS-INT-BEGIN-DATE) GREATER THAN 30
00238         COMPUTE WK3 EQUAL 1 + ((30 + CP-TERM-OR-EXT-DAYS) *
00239         CP-PAY-FREQUENCY / 365) * I
00240      ELSE
00241         COMPUTE WK3 EQUAL 1 + ((30 - CP-TERM-OR-EXT-DAYS) *
00242         CP-PAY-FREQUENCY / 365) * I.
00243
00244      COMPUTE WS-WORK-RATE EQUAL ((2 * CP-PREMIUM-RATE) /
00245      13).
00246
00247      COMPUTE WK5-CSL EQUAL WS-WORK-RATE * ((1 + I) / I) *
00248      (N - ( ANGLEN-1 / WK3)).
00249
00250      MOVE ZEROS TO WS-Y-X-FACTOR WS-X-Y-FACTOR.
00251
00252      COMPUTE WS-CALC-FREQ-RATE EQUAL
00253        (CP-LOAN-APR * .01) / CP-PAY-FREQUENCY.
00254
00255      COMPUTE WS-Y-X-FACTOR EQUAL
00256        1 + ((1 - (1 + WS-CALC-FREQ-RATE) ** -(N - 1)) /
00257        WS-CALC-FREQ-RATE).
00258
00259      COMPUTE WS-ODD-DAY-UNITS EQUAL
00260        (WS-DUE-DATE - WS-INT-BEGIN-DATE) * CP-PAY-FREQUENCY / 365.
00261
00262      COMPUTE WS-X-Y-FACTOR EQUAL
00263        1 + (WS-ODD-DAY-UNITS * WS-CALC-FREQ-RATE).
00264
00265      COMPUTE WS-NP-FACTOR EQUAL WS-WORK-RATE *
00266        ((1 + WS-CALC-FREQ-RATE) / WS-CALC-FREQ-RATE) *
00267        (N - (WS-Y-X-FACTOR / WS-X-Y-FACTOR)).
00268
00269      COMPUTE CP-CALC-PREMIUM EQUAL CP-MONTHLY-PAYMENT * WK5-CSL.
00270
00271      GO TO 99999-RETURN.
00272
00273  XXXX-CHECK-DISCOUNT-5.
00274
00275      IF WS-DISCOUNT-OPTION EQUAL '5'
00276         NEXT SENTENCE
00277      ELSE
00278         GO TO 12000-CHECK-CLIENT.
00279
00280       COMPUTE ANGLEN EQUAL WK1 / I.
00281
00282       COMPUTE APR100 EQUAL ANNUAL-INT-RATE / +100.
00283
00284       COMPUTE WK3 EQUAL
00285         (12 *
00286         (((CP-ORIGINAL-BENEFIT / ANGLEN) * N) -
00287         CP-ORIGINAL-BENEFIT)) /
00288         APR100.
00289
00290      COMPUTE WK3 EQUAL
00291         (WK3 * CP-PREMIUM-RATE) / +1000.
00292
00293      COMPUTE WK3 EQUAL
00294         (24 * WK3) / (24 + (WS-WORK-DIS-RATE * N)).
00295
00296       MOVE WK3 TO CP-CALC-PREMIUM.
00297
00298      GO TO 99999-RETURN.
00299
00300  12000-CHECK-CLIENT.
00301
00302      IF (CP-COMPANY-ID EQUAL 'MON') AND
00303         (CP-STATE-STD-ABBRV EQUAL 'MD') AND
00304         (CP-TRUNCATED-LIFE)
00305         NEXT SENTENCE
00306      ELSE
00307         GO TO 12000-CHECK-NEXT.
00308
00309      COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100.
00310
00311      COMPUTE ANGLEN EQUAL WK1 / I.
00312      COMPUTE ANGLEM EQUAL WK2 / I.
00313
00314      COMPUTE FM EQUAL
00315                     (((ONE-PLUS-I ** M) * I * M) /
00316                     ((ONE-PLUS-I ** M) - 1))
00317                     - 1.
00318
00319      COMPUTE FN EQUAL
00320                     (((ONE-PLUS-I ** N) * I * N) /
00321                     ((ONE-PLUS-I ** N) - 1))
00322                     - 1.
00323
00324      COMPUTE FNM EQUAL
00325                     (((ONE-PLUS-I ** (N - M)) * I * (N - M)) /
00326                     ((ONE-PLUS-I ** (N - M)) - 1))
00327                     - 1.
00328
00329      COMPUTE GR EQUAL
00330                     (WS-WORK-RATE * ANGLEM * I) /
00331                     (M - ANGLEM).
00332
00333      COMPUTE ODF EQUAL
00334                      (1 +
00335                      ((12 * I * (CP-TERM-OR-EXT-DAYS + 30)) /
00336                      360)) /
00337                      ONE-PLUS-I.
00338
00339      COMPUTE OD EQUAL
00340                     (1 + (ODF * (FN / I - 1))) +
00341                     ((12 * CP-TERM-OR-EXT-DAYS) / +360).
00342
00343      COMPUTE WK3 EQUAL
00344 *                   ((1 + FN) * ODF * CP-ORIGINAL-BENEFIT) /
00345                     ((1 + FN) * ODF *
00346                     (CP-ORIGINAL-BENEFIT - CP-ORIGINAL-PREMIUM)) /
00347                     (1 - (GR * (OD - ((ONE-PLUS-I ** M) / I) *
00348                     FNM * ODF * (1 - ANGLEM / ANGLEN)))).
00349
00350      COMPUTE TRUNC-PMT EQUAL WK3 / N.
00351      COMPUTE WK3 EQUAL TRUNC-PMT * N.
00352
00353      COMPUTE WK3 EQUAL
00354                     (WK3 / (ODF * (1 + FN))) -
00355 *                   CP-ORIGINAL-BENEFIT.
00356                     (CP-ORIGINAL-BENEFIT - CP-ORIGINAL-PREMIUM).
00357
00358       MOVE WK3 TO CP-CALC-PREMIUM.
00359
00360      GO TO 99999-RETURN.
00361
00362  12000-CHECK-NEXT.
00363
00364      IF WS-DISCOUNT-OPTION EQUAL '3' OR '4'
00365         NEXT SENTENCE
00366      ELSE
00367         GO TO 12000-CHECK-PA-TRUNC.
00368
00369       IF WS-WORK-DIS-RATE EQUAL +0
00370          MOVE '7'                TO CP-RETURN-CODE
00371          MOVE ZEROS              TO FACTOR
00372                                     CP-CALC-PREMIUM
00373          GO TO 99999-RETURN.
00374
00375       COMPUTE ANGLEN EQUAL WK1 / I.
00376
00377       COMPUTE APR100 EQUAL ANNUAL-INT-RATE / +100.
00378
00379       IF WS-WORK-DIS-RATE EQUAL APR100
00380          ADD .00001 TO APR100.
00381
00382       COMPUTE WK8 EQUAL 12 / (12 + WS-WORK-DIS-RATE).
00383       COMPUTE WK9 EQUAL 12 / (12 + APR100).
00384
021207     COMPUTE Y = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I) / +30)
              / (1 + I)
021207     COMPUTE ANGLEN-1 = ANGLEN / Y
00385       IF CP-COMPANY-ID EQUAL 'CDL'
00386          COMPUTE WK3 EQUAL
00387
00388           (144 * CP-ORIGINAL-BENEFIT / ANGLEN) *
00389           ((1 + WS-WORK-DIS-RATE / 12) ** .5) *
00390
00391           ((1 / (WS-WORK-DIS-RATE * APR100)) -
00392
00393           ((WK8 ** N) /
00394           (WS-WORK-DIS-RATE * (APR100 - WS-WORK-DIS-RATE))) +
00395
00396           ((WK9 ** N) /
00397           (APR100 * (APR100 - WS-WORK-DIS-RATE))))
00398
00399       ELSE
00400          COMPUTE WK3 EQUAL
00401
00402           ((CP-ORIGINAL-BENEFIT / ANGLEN-1) *
00403           (12 + WS-WORK-DIS-RATE) * 12) *
00404
00405           ((1 / (WS-WORK-DIS-RATE * APR100)) -
00406
00407           ((WK8 ** N) /
00408           (WS-WORK-DIS-RATE * (APR100 - WS-WORK-DIS-RATE))) +
00409
00410           ((WK9 ** N) /
00411           (APR100 * (APR100 - WS-WORK-DIS-RATE)))).
00412
00413       COMPUTE WK3 EQUAL
00414           (WK3 * CP-PREMIUM-RATE) / +1000.
00415
00416       MOVE WK3 TO CP-CALC-PREMIUM.
00417
00418      GO TO 99999-RETURN.
00419
00420  12000-CHECK-PA-TRUNC.
00421
00422      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00423         (CP-STATE-STD-ABBRV EQUAL 'PA') AND
00424         (CP-EARN-AS-NET-PAY) AND
00425         (CP-TRUNCATED-LIFE)
00426         NEXT SENTENCE
00427      ELSE
00428         GO TO 12000-CHECK-MD-TRUNC.
00429
00430      COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100.
00431
00432      COMPUTE ANGLEN-M EQUAL WK5 / I.
00433      COMPUTE ANGLEN EQUAL WK1 / I.
00434
00435      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00436
00437      COMPUTE WK3 EQUAL
00438         (CP-ORIGINAL-BENEFIT * 2 * (M - (ANGLEN-M - ANGLEN)) *
00439         WS-WORK-RATE) / (ANGLEN-1 * (N + 1) * I).
00440
00441      MOVE WK3 TO CP-CALC-PREMIUM.
00442
00443      GO TO 99999-RETURN.
00444
00445  12000-CHECK-MD-TRUNC.
00446
00447      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00448         (CP-STATE-STD-ABBRV EQUAL 'MD') AND
00449         (CP-EARN-AS-NET-PAY) AND
00450         (CP-TRUNCATED-LIFE)
00451         NEXT SENTENCE
00452      ELSE
00453         GO TO 12000-CHECK-MD.
00454
00455      COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100.
00456
00457      COMPUTE ANGLEN-M EQUAL WK5 / I.
00458      COMPUTE ANGLEM EQUAL WK2 / I.
00459      COMPUTE ANGLEN EQUAL WK1 / I.
00460
00461      COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00462
00463      COMPUTE WK3 EQUAL
00464         CP-ORIGINAL-BENEFIT * WS-WORK-RATE *
00465         ((ANGLEM * (M - (ANGLEN - ANGLEN-M))) /
00466         (ANGLEN-1 * (M - ANGLEM))).
00467
00468      MOVE WK3 TO CP-CALC-PREMIUM.
00469
00470      GO TO 99999-RETURN.
00471
00472  12000-CHECK-MD.
00473
00474      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00475         (CP-STATE-STD-ABBRV EQUAL 'MD') AND
00476         (CP-EARN-AS-NET-PAY)
00477         NEXT SENTENCE
00478      ELSE
00479         GO TO 12000-CHECK-ME-TRUNC.
00480
00481      COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100.
00482
00483      COMPUTE WK3 EQUAL
00484         CP-ORIGINAL-BENEFIT * WS-WORK-RATE.
00485
00486      MOVE WK3 TO CP-CALC-PREMIUM.
00487
00488      GO TO 99999-RETURN.
00489
00490  12000-CHECK-ME-TRUNC.
00491
060308     IF (CP-COMPANY-ID EQUAL 'NCL' OR 'CID')
00493         AND (CP-STATE-STD-ABBRV EQUAL 'ME')
00494         AND (CP-EARN-AS-NET-PAY)
00495         AND (CP-TRUNCATED-LIFE)
00496         CONTINUE
00497      ELSE
111212        GO TO 12000-CHECK-CID-WA-FARMPLAN
           END-IF
00499
060308     COMPUTE WS-WORK-RATE = CP-PREMIUM-RATE / +1000
060308     COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
060308     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
060308     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
060308     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
060308     COMPUTE ANGLEN-1 = ANGLEN / Y
060308     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
060308     COMPUTE WK5 = 1 / (1 + (WS-WORK-DIS-RATE * N / 24))
060308     COMPUTE WK9 = 1 / (1 + (WS-WORK-DIS-RATE * (N - M) / 24))
060308     COMPUTE WK3
060308        = (WK4 * ((N - ANGLEN) / I) * WS-WORK-RATE * WK5)
060308        - (WK4 * (((N - M) - ANGLEN-M) / I) * WS-WORK-RATE * WK9
060308            * (1 / (1.0635 ** (M / 12))))
NCLCOD*    COMPUTE WK8 EQUAL 1 / (1 + (.045 * N / 24)).
NCLCOD*    COMPUTE WK9 EQUAL
NCLCOD*       1 / (1 + (.045 * (N - M) / 24))
NCLCOD*    COMPUTE WK7 EQUAL
NCLCOD*       WS-WORK-RATE * 20 / (M + 1).
NCLCOD*    COMPUTE WK3 EQUAL
NCLCOD*       ((( N - ANGLEN) * WK7 * WK8) /
NCLCOD*       (10 * ANGLEN-1 * I)) -
NCLCOD*       (((N - M - ANGLEN-M) * WK7 * WK9 * 1) /
NCLCOD*       (10 * ANGLEN-1 * I * (1.0635 ** (M / 12)))).
NCLCOD*    COMPUTE WK3 EQUAL WK3 * CP-ORIGINAL-BENEFIT.
NCLCOD*
00519      MOVE WK3 TO CP-CALC-PREMIUM.
00520
00521      GO TO 99999-RETURN
            .
111212 12000-CHECK-CID-WA-FARMPLAN.
111212
111212     IF (CP-COMPANY-ID EQUAL 'CID')
111212        AND (CP-STATE-STD-ABBRV = 'WA')
111212        AND (CP-FARM-PLAN)
111212        CONTINUE
111212     ELSE
111212        GO TO 12000-CHECK-CID-FARMPLAN
111212     END-IF
030515**  This was removed due to IR 2015022300001
030515    GO TO 12000-CHECK-CID-FARMPLAN
111212
111212     move cp-original-term       to n
111212     move cp-no-of-pmts          TO C-TOT-PMTS
111212     compute c-pmts-per-year = 12 / cp-pay-frequency
111212     move cp-pay-frequency       to m
111212     compute c-days-in-pmt-per = 365 / c-pmts-per-year
111212     COMPUTE I = CP-LOAN-APR / C-PMTS-PER-YEAR / 100
050713     if i = zeros
050713        move .0000000001         to i
050713     end-if
111212     COMPUTE ANGLEN   = (1 - (1 / (1 + I)) ** c-tot-pmts) / I
111212
111212     COMPUTE Y = (1 + ((CP-DAYS-TO-1ST-PMT * I) /
111212        (m * 30))) / (1 + I)
111212     COMPUTE J = ((1 + WS-WORK-DIS-RATE)
111212        ** CP-PAY-FREQUENCY) - 1
111212     COMPUTE TI = (cp-days-to-1st-pmt - (M * 30)) / 30
111212     COMPUTE WK1 = (1 - (1 / (1 + WS-WORK-DIS-RATE))
111212        ** TI) / WS-WORK-DIS-RATE
111212     COMPUTE WK2 = (1 - (1 / (1 + J)) ** c-tot-pmts) / J
111212     COMPUTE WK3 = (1 - (1 / (1 + J)) ** (c-tot-pmts - 1)) / J
111212     COMPUTE WK4 = (1 - (1 / (1 + WS-WORK-DIS-RATE))
111212        ** m) / WS-WORK-DIS-RATE
111212     COMPUTE WK2 = WK2 * (1 + J)
111212     COMPUTE WK4 = WK4 * (1 + WS-WORK-DIS-RATE)
111212     COMPUTE WK1 = WK1 * (1 + WS-WORK-DIS-RATE)
111212     COMPUTE WK3 = WK3 * (1 + J)
111212     COMPUTE WK5 = (WK3 - (c-tot-pmts - 1) * (1 / (1 + J)
111212        ** (c-tot-pmts - 1))) / J
111212     COMPUTE GR = (((1 / (1 + WS-WORK-DIS-RATE) ** TI) * WK4 *
111212        ((c-tot-pmts * WK2) - WK5)) + (c-tot-pmts * WK1)) *
111212        (CP-PREMIUM-RATE / 1000)
111212     COMPUTE WK7 = CP-ORIGINAL-BENEFIT / (anglen / y)
111212     COMPUTE WK6 = (WK7 * GR)
111212
111212     MOVE WK6 TO CP-CALC-PREMIUM
111212
111212     GO TO 99999-RETURN
111212
111212     .
00490  12000-CHECK-CID-FARMPLAN.
           IF (CP-COMPANY-ID EQUAL 'CID')
              AND (CP-STATE-STD-ABBRV EQUAL 'ND' OR 'MT' OR 'NJ'
                 OR 'IN' OR 'AZ')
              AND (CP-FARM-PLAN)
              CONTINUE
           ELSE
021519        GO TO 12000-CHECK-CID-AL-FARMPLAN
           END-IF
           COMPUTE N = M / CP-PAY-FREQUENCY
           COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
           COMPUTE J = ((1 + WS-WORK-DIS-RATE)
              ** CP-PAY-FREQUENCY) - 1
           COMPUTE TI = (D - (CP-PAY-FREQUENCY * 30)) / 30
           COMPUTE WK1 = (1 - (1 / (1 + WS-WORK-DIS-RATE))
              ** TI) / WS-WORK-DIS-RATE
           COMPUTE WK2 = (1 - (1 / (1 + J)) ** N) / J
           COMPUTE WK3 = (1 - (1 / (1 + J)) ** (N - 1)) / J
           COMPUTE WK4 = (1 - (1 / (1 + WS-WORK-DIS-RATE))
              ** CP-PAY-FREQUENCY) / WS-WORK-DIS-RATE
           COMPUTE WK2 = WK2 * (1 + J)
           COMPUTE WK4 = WK4 * (1 + WS-WORK-DIS-RATE)
           COMPUTE WK1 = WK1 * (1 + WS-WORK-DIS-RATE)
           COMPUTE WK3 = WK3 * (1 + J)
           COMPUTE WK5 = (WK3 - (N - 1) * (1 / (1 + J)
              ** (N - 1))) / J
           COMPUTE GR = (((1 / (1 + WS-WORK-DIS-RATE) ** TI) * WK4 *
              ((N * WK2) - WK5)) + (N * WK1)) *
              (CP-PREMIUM-RATE / 1000)
           COMPUTE WK7 = CP-ORIGINAL-BENEFIT / N
           COMPUTE WK6 = (WK7 * GR)
           MOVE WK6 TO CP-CALC-PREMIUM
           GO TO 99999-RETURN
           .
021519 12000-CHECK-CID-AL-FARMPLAN.
021519
021519     IF (CP-COMPANY-ID EQUAL 'CID')
021519        AND (CP-STATE-STD-ABBRV = 'AL')
021519        AND (CP-FARM-PLAN)
021519        CONTINUE
021519     ELSE
021519        GO TO 12000-CHECK-SC-CID
021519     END-IF
021519
021519     display ' made AL Farm Plan calc '
021519
021519     evaluate true
021519        when cp-pmt-mode = 'B'
021519           move 26               to c-pmts-per-year
021519           move 14               to c-days-in-pmt-per
021519        when cp-pmt-mode = 'W'
021519           move 52               to c-pmts-per-year
021519           move 07               to c-days-in-pmt-per
021519        when cp-pmt-mode = 'S'
021519           move 24               to c-pmts-per-year
021519           move 15               to c-days-in-pmt-per
021519        when cp-pmt-mode = 'A'
021519           move 1                to c-pmts-per-year
021519           move 360              to c-days-in-pmt-per
021519        when cp-pmt-mode = 'T'
021519           move 2                to c-pmts-per-year
021519           move 180              to c-days-in-pmt-per
021519        when cp-pmt-mode = 'Q'
021519           move 4                to c-pmts-per-year
021519           move 90               to c-days-in-pmt-per
021519        when cp-pmt-mode = 'N' *> Bi-Monthly ??
021519           move 6                to c-pmts-per-year
021519           move 60               to c-days-in-pmt-per
021519        when other
021519           move 12               to c-pmts-per-year
021519           move 30               to c-days-in-pmt-per
021519     end-evaluate
021519
021519     compute c-tot-pmts = n / 12 * c-pmts-per-year
021519     compute i = cp-loan-apr / c-pmts-per-year / 100
021519     if i = zeros
021519        move .0000000001         to i
021519     end-if
021519     compute anglen  =
021519        (1 - (1 / (1 + I)) ** c-tot-pmts) / I
021519
021519     compute y = (1 + ((CP-DAYS-TO-1ST-PMT * I) /
021519        c-days-in-pmt-per)) / (1 + I)
021519
021519     compute angleny = anglen / y
021519
021519     compute wk3 rounded =   *> tv1
021519        ((c-tot-pmts - anglen + (i * c-tot-pmts))) / i
021519
021519     compute wk6 =           *> tv2
021519        cp-premium-rate / 10 * 12 / c-pmts-per-year / 100
021519
021519     compute wk7 =    *> tv3
021519        (cp-original-benefit / anglen)
021519
021519     compute wk5 rounded = wk7 *wk3 * wk6
021519
021519     move wk5                    to cp-calc-premium
021519     GO TO 99999-RETURN
           .
00490  12000-CHECK-SC-CID.
           IF (CP-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'SC')
              AND (CP-EARN-AS-NET-PAY)
              AND (CP-BENEFIT-CD = '2I' OR '2J' OR '2K' OR '2L')
              move 6                   to ws-extra-pmts
           ELSE
              GO TO 12000-CHECK-AL-CID
           END-IF
050713     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
050713     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
050713     COMPUTE WK2 = (CP-PREMIUM-RATE * ((M + ((D - 30) / 30))
050713        / 12)) / ((M + 1) / 2)
050713
050713     if i = .0000000001
050713        compute wk4 = cp-original-benefit / n
050713        compute wk3 = wk4 * (((n * (n + 1)) / 2 - ((n - m) *
050713            (n - m + 1)) / 2 + ws-extra-pmts * m) * wk2 / 100)
050713        display ' hit sc wo apr - wk2 ' wk2
050713     else
050713        COMPUTE ANGLEN rounded = (1 - (1 / (1 + I)) ** N) / I
050713        COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
050713        COMPUTE ANGLEN-1 = ANGLEN / Y
050713        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
050713        COMPUTE WK3 = WK4 * (((M - (ANGLEN - ANGLEN-M)) / I)
050713           + WS-EXTRA-PMTS * M) * (WK2 / 100) * (1 + I)
050713     end-if
050713*    COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
050713*    COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
050713*    COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
050713*    COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
050713*    COMPUTE ANGLEN-1 = ANGLEN / Y
050713*    COMPUTE WK2 = (CP-PREMIUM-RATE * ((M + ((D - 30) / 30))
050713*       / 12)) / ((M + 1) / 2)
050713*    COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
050713*    COMPUTE WK3 = WK4 * (((M - (ANGLEN - ANGLEN-M)) / I)
050713*       + WS-EXTRA-PMTS * M) * (WK2 / 100) * (1 + I)
           MOVE WK3 TO CP-CALC-PREMIUM
           GO TO 99999-RETURN
           .
       12000-CHECK-AL-CID.
           IF (CP-COMPANY-ID EQUAL 'CID')
              AND (CP-BENEFIT-CD = '49' OR '50' OR '53' OR '54')
              AND (CP-EARN-AS-NET-PAY)
              CONTINUE
           ELSE
              GO TO 12000-CHECK-NH-CID
           END-IF
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
110207     COMPUTE ANGLEM = (1 - (1 / (1 + I)) ** M) / I
110207     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
           COMPUTE WK8 = (1 + ((CP-TERM-OR-EXT-DAYS + +30)* I) / +30)
                         / (1 + I)
           COMPUTE ANGLEN-1 = ANGLEN / WK8
           COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
110207     COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M) + I * M) / I) *
              (CP-PREMIUM-RATE / 1000)
           MOVE WK3 TO CP-CALC-PREMIUM
           GO TO 99999-RETURN
           .
       12000-CHECK-NH-CID.
           IF (CP-COMPANY-ID EQUAL 'CID')
              AND (CP-STATE-STD-ABBRV = 'NH' OR 'AK')
              AND (CP-EARN-AS-NET-PAY)
              CONTINUE
           ELSE
              GO TO 12000-CHECK-PA-CID
           END-IF
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
           COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
           COMPUTE WK8 = (1 + ((CP-TERM-OR-EXT-DAYS + +30)* I) / +30)
                         / (1 + I)
           COMPUTE ANGLEN-1 = ANGLEN / WK8
           COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
           COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
              (1 + (2 * I)) * (CP-PREMIUM-RATE / 1000)
           MOVE WK3 TO CP-CALC-PREMIUM
           GO TO 99999-RETURN
           .
021207 12000-CHECK-PA-CID.
021207     IF (CP-COMPANY-ID EQUAL 'CID')
021207        AND (CP-STATE-STD-ABBRV EQUAL 'PA')
021207        AND (CP-EARN-AS-NET-PAY)
021207        CONTINUE
021207     ELSE
021207        GO TO 12000-CHECK-ME-CID
021207     END-IF
021207
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
           COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
021207     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
021207     COMPUTE ANGLEN-1 = ANGLEN / Y
021207     COMPUTE WK5 = 1 / (10 * (1 + (WS-WORK-DIS-RATE / 24) * N))
021207     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
021207     COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +100
021207     COMPUTE WK3 = WK4 * ((N - ANGLEN) / I) * WS-WORK-RATE
021207        * WK5
021207     MOVE WK3 TO CP-CALC-PREMIUM
021207     GO TO 99999-RETURN
021207     .
00490  12000-CHECK-ME-CID.
           IF (CP-COMPANY-ID EQUAL 'CID')
              AND (CP-STATE-STD-ABBRV EQUAL 'ME')
              AND (CP-EARN-AS-NET-PAY)
              CONTINUE
           ELSE
              GO TO 12000-CHECK-VT-TRUNC
           END-IF
           COMPUTE WS-WORK-RATE = CP-PREMIUM-RATE / +1000
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
060308     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
060308     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
060308     COMPUTE ANGLEN-1 = ANGLEN / Y
060308     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
           COMPUTE WK5 = 1 / (1 + (WS-WORK-DIS-RATE * N / 24))
           COMPUTE WK3 = WK4 * ((N - ANGLEN) / I) * WS-WORK-RATE
              * WK5
           MOVE WK3 TO CP-CALC-PREMIUM
           GO TO 99999-RETURN
           .
00523  12000-CHECK-VT-TRUNC.
00524
00525      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00526         (CP-STATE-STD-ABBRV EQUAL 'VT') AND
00527         (CP-EARN-AS-NET-PAY) AND
00528         (CP-TRUNCATED-LIFE)
00529         NEXT SENTENCE
00530      ELSE
00531         GO TO 12000-CHECK-VT.
00532
00533       COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +1000.
00534
00535       COMPUTE ANGLEN EQUAL WK1 / I.
00536
00537       COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00538
00539       COMPUTE WK8 EQUAL (1 / (1 + I)) ** N.
00540       COMPUTE WK9 EQUAL
00541         (1 - ((1 / (1 + ((.0054 - I) / (1 + I)))) ** (M - 1))) /
00542         ((.0054 - I) / (1 + I)).
00543       COMPUTE WK7 EQUAL
00544         (1 - (1 / 1.0054) ** (M - 1)) / .0054.
00545       COMPUTE WK3 EQUAL  CP-ORIGINAL-BENEFIT * WS-WORK-RATE *
00546         ((WK7 + 1) - (WK8 * (WK9 + 1))) / (ANGLEN-1 * I).
00547
00548       MOVE WK3 TO CP-CALC-PREMIUM.
00549
00550      GO TO 99999-RETURN.
00551
00552  12000-CHECK-VT.
00553
00554      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00555         (CP-STATE-STD-ABBRV EQUAL 'VT') AND
00556         (CP-EARN-AS-NET-PAY)
00557         NEXT SENTENCE
00558      ELSE
00559         GO TO 12000-CHECK-MINN-WDS.
00560
00561       COMPUTE WS-WORK-RATE EQUAL CP-PREMIUM-RATE / +1000.
00562
00563       COMPUTE ANGLEN EQUAL WK1 / I.
00564
00565       COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00566
00567       COMPUTE WK3 EQUAL ((CP-ORIGINAL-BENEFIT * WS-WORK-RATE) /
00568                         (ANGLEN-1 * I)) *
00569
00570                         (((1.0054 ** N - 1) /
00571                         (1.0054 ** (N - 1) * .0054)) -
00572
00573                         ((V * (1 - 1.0054 ** N * VX)) /
00574                         (1.0054 ** (N - 1) * (1 - 1.0054 * V)))).
00575
00576       MOVE WK3 TO CP-CALC-PREMIUM.
00577
00578      GO TO 99999-RETURN.
00579
00580  12000-CHECK-MINN-WDS.
00581
00582      IF (CP-COMPANY-ID EQUAL 'WDS') AND
00583         (CP-STATE-STD-ABBRV EQUAL 'MN') AND
00584         (CP-EARN-AS-NET-PAY)
00585         NEXT SENTENCE
00586      ELSE
00587         GO TO 12000-CHECK-MINN-BALL-LEV.
00588
00589      COMPUTE ANGLEN EQUAL WK1 / I.
00590      COMPUTE WK3 EQUAL (CP-PREMIUM-RATE / +100) *
00591               (N / +12) * (2 / (N + 1)).
00592      COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT / ANGLEN) *
00593          WK3 * ((N - ANGLEN) / I).
00594
00595       MOVE WK3 TO CP-CALC-PREMIUM.
00596
00597      GO TO 99999-RETURN.
00598
00599  12000-CHECK-MINN-BALL-LEV.
00600
00601      IF (CP-COMPANY-ID EQUAL 'WDS') AND
00602         (CP-STATE-STD-ABBRV EQUAL 'MN') AND
00603         (CP-DEVIATION-CODE EQUAL 'LEV') AND
00604         (CP-EARN-AS-REG-BALLOON)
00605         NEXT SENTENCE
00606      ELSE
00607         GO TO 12000-CHECK-MINN-BALL.
00608
00609      COMPUTE ANGLEN EQUAL WK1 / I.
00610
00611      COMPUTE WK3 EQUAL 1 / (1 + I) ** N.
00612
00613      COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE /
00614         +100 * N / 12 * 2 / (N + 1) * (ANGLEN / WK3).
00615
00616      MOVE WK3 TO CP-CALC-PREMIUM.
00617
00618      GO TO 99999-RETURN.
00619
00620  12000-CHECK-MINN-BALL.
00621
00622      IF (CP-COMPANY-ID EQUAL 'WDS') AND
00623         (CP-STATE-STD-ABBRV EQUAL 'MN') AND
00624         (CP-EARN-AS-REG-BALLOON)
00625         NEXT SENTENCE
00626      ELSE
00627         GO TO 12000-CHECK-CID-MN-BALL.
00628
00629      COMPUTE ANGLEN EQUAL WK1 / I.
00630      COMPUTE ANGLEN-1 EQUAL WK6 / I.
00631
00632      COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE
00633      / +100 * (N - 1) / 12 * 2 / N *
00634      (((N - 1) - ANGLEN-1) / (I * ANGLEN-1)).
00635
00636       MOVE WK3 TO CP-CALC-PREMIUM.
00637
00638      GO TO 99999-RETURN.
00639
       12000-CHECK-CID-MN-BALL.
           IF (CP-COMPANY-ID EQUAL 'CID')
              AND (CP-STATE-STD-ABBRV = 'MN')
              AND (CP-CERT-EFF-DT > X'A4FF')
              AND (CP-EARN-AS-REG-BALLOON)
              CONTINUE
           ELSE
111212        GO TO 12000-CHECK-CID-MN-FARM
           END-IF
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
           COMPUTE ANGLEM = (1 - (1 / (1 + I)) ** M) / I
           COMPUTE ANGLEMP1 = (1 - (1 / (1 + I)) ** (M + 1)) / I
           COMPUTE ANGLEM-1 = (1 - (1 / (1 + I)) ** (M - 1)) / I
           COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M + 1)) / I
           COMPUTE V = 1 / (1 + I)
           COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
           COMPUTE Y = (1 + (D * I) / +30)
              / (1 + I)
           COMPUTE CP-LF-BALLOON-PREM = CP-ALTERNATE-BENEFIT * (M + 1)
              * CP-PREMIUM-RATE / 1000
      *    IF CP-MONTHLY-PAYMENT NOT = ZEROS
      *       MOVE CP-MONTHLY-PAYMENT  TO GR
      *    ELSE
      *       COMPUTE WK6 = CP-ORIGINAL-BENEFIT + CP-ALTERNATE-BENEFIT
      *          * (ANGLEM / Y) * (CP-PREMIUM-RATE / 1000) -
      *          CP-ALTERNATE-BENEFIT * V ** (M + ((D - 30) / 30))
      *       DISPLAY ' BALLOON TOP ' WK6
      *       COMPUTE WK7 = (ANGLEM-1 / Y) - ((M - 1) - ANGLEM-1) / I
      *          * (CP-PREMIUM-RATE / 1000) * (1 + I) - ((M - 1) * 6.65)
      *          / 100
      *       DISPLAY ' BALLOON BOTTOM ' WK7
      *       COMPUTE GR = WK6 / WK7
      *       DISPLAY ' BALLOON PAYMENT ' GR
      *    END-IF
           COMPUTE WK8 = (1 + I) ** ((M + 1) * -1)
           COMPUTE CP-ORIGINAL-BENEFIT ROUNDED = (CP-ORIGINAL-BENEFIT
              + CP-ALTERNATE-BENEFIT) - (CP-ALTERNATE-BENEFIT * WK8)
           COMPUTE GR = CP-ORIGINAL-BENEFIT / ANGLEM
           COMPUTE WK3 = (GR * ((M - ANGLEM) / I) * (1 + I)
              + (CP-ALTERNATE-BENEFIT * ANGLEMP1) -
                 (CP-ALTERNATE-BENEFIT * (M + 1)))
                 * CP-PREMIUM-RATE / 1000
           MOVE WK3                    TO CP-CALC-PREMIUM
           GO TO 99999-RETURN.
           .
111212 12000-CHECK-CID-MN-FARM.
111212
111212     IF (CP-COMPANY-ID EQUAL 'CID')
030515        AND (CP-STATE-STD-ABBRV = 'MN' or 'WA')
111212        AND (CP-CERT-EFF-DT > X'A4FF')
111212        AND (CP-EARN-AS-FARM-PLAN)
111212        CONTINUE
111212     ELSE
111212        GO TO 12000-CHECK-CID-NET-BALLoons
111212     END-IF
111212
111212     EVALUATE CP-PMT-MODE
111212        WHEN 'B'
111212           MOVE +14              TO C-DAYS-IN-PMT-PER
111212           MOVE +26              TO C-PMTS-PER-YEAR
111212           MOVE CP-NO-OF-PMTS    TO C-TOT-PMTS
111212        WHEN 'S'
111212           MOVE +15              TO C-DAYS-IN-PMT-PER
111212           MOVE +24              TO C-PMTS-PER-YEAR
111212           MOVE CP-NO-OF-PMTS    TO C-TOT-PMTS
111212        WHEN 'T'
111212           MOVE +360             TO C-DAYS-IN-PMT-PER
111212           MOVE +1               TO C-PMTS-PER-YEAR
111212           MOVE CP-NO-OF-PMTS    TO C-TOT-PMTS
111212        WHEN 'W'
111212           MOVE +7               TO C-DAYS-IN-PMT-PER
111212           MOVE +52              TO C-PMTS-PER-YEAR
111212           MOVE CP-NO-OF-PMTS    TO C-TOT-PMTS
111212        WHEN OTHER
111212           move cp-no-of-pmts    to c-tot-pmts
111212           if cp-pay-frequency > zeros
111212              compute c-pmts-per-year =
111212                 12 / cp-pay-frequency
111212              compute c-days-in-pmt-per = 360 / c-pmts-per-year
111212           else
111212              MOVE +30           TO C-DAYS-IN-PMT-PER
111212              MOVE +12           TO C-PMTS-PER-YEAR
111212              MOVE CP-LOAN-TERM  TO C-TOT-PMTS
111212           end-if
111212     END-EVALUATE
111212
111212     COMPUTE I = CP-LOAN-APR / C-PMTS-PER-YEAR / 100
050713     if i = zeros
050713        move .0000000001         to i
050713     end-if
111212
111212     COMPUTE Y = (1 + (CP-DAYS-TO-1ST-PMT * I /
111212        C-DAYS-IN-PMT-PER)) / (1 + I)
111212
111212     compute cp-premium-rate =
030515        cp-premium-rate * 12 / c-pmts-per-year / 10
111212     move c-tot-pmts to n m
111212     COMPUTE ANGLEN   = (1 - (1 / (1 + I)) ** N) / I
111212     COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
111212     COMPUTE ANGLEN-1 = ANGLEN / Y
111212     COMPUTE WK3 = CP-ORIGINAL-BENEFIT / ANGLEN-1
111212
111212     compute wk3 = wk3 * ((m - (anglen - anglen-m)) / i) *
111212        (cp-premium-rate / 100)
111212
111212
111212     MOVE WK3                    TO CP-CALC-PREMIUM
111212
111212     GO TO 99999-RETURN.
111212
111212     .
040615 12000-check-cid-net-balloons.
           IF (CP-COMPANY-ID EQUAL 'CID')
              AND (CP-EARN-AS-REG-BALLOON)
              and (CP-NET-ONLY-STATE not = 'Y')
              AND (CP-BEN-CATEGORY = 'P' OR 'I')
              CONTINUE
           ELSE
              GO TO 12000-check-cid-net-only-balls
           END-IF
           display ' *** NOT NET ONLY STATE BALLOONS *** '
           COMPUTE Y =
              (1 + (CP-DAYS-TO-1ST-PMT * I / 30)) / (1 + I)
           COMPUTE ANGLEN   = (1 - (1 / (1 + I)) ** N) / I
           COMPUTE ANGLEM   = (1 - (1 / (1 + I)) ** M) / I
           COMPUTE ANGLEM-1   = (1 - (1 / (1 + I)) ** (M - 1)) / I
           COMPUTE ANGLEN-MP1 = (1 - (1 / (1 + I)) ** (N - M + 1)) / I
           COMPUTE ANGLEMY = ANGLEM / Y
           COMPUTE ANGLEM-1Y = ANGLEM-1 / Y
           COMPUTE ANGLENY = ANGLEN / Y
           COMPUTE ANGLEN-MP1Y = ANGLEN-MP1 / Y
           compute cp-original-benefit =
              cp-original-benefit + cp-alternate-benefit
           COMPUTE PVBALLOON rounded =
              CP-ALTERNATE-BENEFIT / ((1 + I) ** (N + 1))
           compute wk4 rounded =
              cp-original-benefit - pvballoon
           compute wk6 rounded =
              (cp-original-benefit - cp-alternate-benefit)
59*******  compute ra = wk4 / anglem-1y
           compute ra = wk4 / anglemy
           compute wk6 rounded = wk6 / anglemy
           display ' rate before ' cp-premium-rate
           compute cp-premium-rate =
              (cp-premium-rate * ((m + (cp-days-to-1st-pmt - 30)
                  / 30) / 12))     /
                 ((m + 1) / 2)
           display '************************************'
           display ' orign ben    ' cp-original-benefit
           display ' balloon      ' cp-alternate-benefit
           display ' ra           ' ra
           display ' wk6          ' wk6
           display ' m            ' m
           display ' anglemy      ' anglemy
           display ' anglem-1     ' anglem-1y
           display ' i            ' i
           display ' extra per    ' cp-extra-periods
           display ' prem rate    ' cp-premium-rate
59*******  compute wk3 = ra * ((m - 1) - anglem-1y) / i * (1 +
59*******     (cp-extra-periods * i)) * cp-premium-rate / 100
           compute wk3 = wk6 * (m - anglem) / i * (1 +
              (cp-extra-periods * i)) * cp-premium-rate / 100
           move wk3                    to cp-calc-premium
           display ' wk3          ' wk3
           display '************************************'
      *    compute wk3 rounded =
      *       cp-alternate-benefit * (m + 1) * cp-premium-rate / 100
      *    move wk3                    to cp-lf-balloon-prem
           GO TO 99999-RETURN.
           .
040615 12000-check-cid-net-only-balls.
           IF (CP-COMPANY-ID EQUAL 'CID')
              AND (CP-EARN-AS-REG-BALLOON)
              AND (CP-BEN-CATEGORY = 'P' OR 'I')
              CONTINUE
           ELSE
              GO TO 12000-CHECK-NC-WDS
           END-IF
           display ' *** NET ONLY STATE BALLOONS *** '
           COMPUTE Y =
              (1 + (CP-DAYS-TO-1ST-PMT * I / 30)) / (1 + I)
           COMPUTE ANGLEN   = (1 - (1 / (1 + I)) ** N) / I
           COMPUTE ANGLEM   = (1 - (1 / (1 + I)) ** M) / I
           COMPUTE ANGLEM-1   = (1 - (1 / (1 + I)) ** (M - 1)) / I
           COMPUTE ANGLEN-MP1 = (1 - (1 / (1 + I)) ** (N - M + 1)) / I
           COMPUTE ANGLEMY = ANGLEM / Y
           COMPUTE ANGLEM-1Y = ANGLEM-1 / Y
           COMPUTE ANGLENY = ANGLEN / Y
           COMPUTE ANGLEN-MP1Y = ANGLEN-MP1 / Y
           COMPUTE PVBALLOON rounded =
              CP-ALTERNATE-BENEFIT / ((1 + I) ** (N + 1))
           compute wk4 rounded =
              cp-original-benefit - pvballoon
      *    compute cp-premium-rate =
      *       (cp-premium-rate * (m + 1) / 12)  /
      *          ((m + 2) / 2)
59*******  compute ra = wk4 / anglem-1y
           compute ra = (cp-original-benefit + cp-alternate-benefit -
              pvballoon) / anglemy
           display '************************************'
           display ' ra           ' ra
           display ' m            ' m
           display ' anglemy      ' anglemy
           display ' anglem-1     ' anglem-1y
           display ' i            ' i
           display ' extra per    ' cp-extra-periods
           display ' prem rate    ' cp-premium-rate
           display '************************************'
59*******  compute wk3 = ra * ((m - 1) - anglem-1y) / i * (1 +
59*******     (cp-extra-periods * i)) * cp-premium-rate / 100
           compute wk3 = ra * (m - anglemy) / i * (1 +
              (cp-extra-periods * i)) * cp-premium-rate / 100
           move wk3                    to cp-calc-premium
           compute wk3 rounded =
              cp-alternate-benefit * (m + 1) * cp-premium-rate / 100
           move wk3                    to cp-lf-balloon-prem
           GO TO 99999-RETURN.
           .
111212 12000-CHECK-NC-WDS.
00641
00642      IF (CP-COMPANY-ID EQUAL 'WDS') AND
00643         (CP-STATE-STD-ABBRV EQUAL 'NC') AND
00644         (CP-EARN-AS-NET-PAY)
00645         NEXT SENTENCE
00646      ELSE
00647         GO TO 12000-CHECK-NC-BALL-LEV.
00648
00649      COMPUTE WK4 EQUAL N * (1 + I * 2).
00650      COMPUTE ANGLEN EQUAL WK1 / I.
00651      COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT *
00652               CP-PREMIUM-RATE / +100 *
00653               N / +12 * 2 / (N + 1) *
00654               (WK4 - ANGLEN) / (I * ANGLEN).
00655
00656       MOVE WK3 TO CP-CALC-PREMIUM.
00657
00658      GO TO 99999-RETURN.
00659
00660  12000-CHECK-NC-BALL-LEV.
00661
00662      IF (CP-COMPANY-ID EQUAL 'WDS') AND
00663         (CP-STATE-STD-ABBRV EQUAL 'NC') AND
00664         (CP-DEVIATION-CODE EQUAL 'LEV') AND
00665         (CP-EARN-AS-REG-BALLOON)
00666         NEXT SENTENCE
00667      ELSE
00668         GO TO 12000-CHECK-NC-BALL.
00669
00670      COMPUTE ANGLEN EQUAL WK1 / I.
00671
00672 **** VX = 1 / (1 + I)**N.
00673
00674      COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE /
00675         +100 * N / 12 * 2 / (N + 1) * (ANGLEN / VX).
00676
00677      MOVE WK3 TO CP-CALC-PREMIUM.
00678
00679      GO TO 99999-RETURN.
00680
00681  12000-CHECK-NC-BALL.
00682
00683      IF (CP-COMPANY-ID EQUAL 'WDS') AND
00684         (CP-STATE-STD-ABBRV EQUAL 'NC') AND
00685         (CP-EARN-AS-REG-BALLOON)
00686         NEXT SENTENCE
00687      ELSE
00688         GO TO 12000-CHECK-MINN-TRUNC.
00689
00690      COMPUTE WK4 EQUAL (N - 1) * (1 + I * 2).
00691      COMPUTE ANGLEN EQUAL WK1 / I.
00692      COMPUTE ANGLEN-1 EQUAL WK6 / I.
00693 **** VX = 1 / (1 + I)**N.
00694
00695      COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE
00696      / +100 * (N - 1) / 12 * 2 / N *
00697      (WK4 - ANGLEN-1) / (I * ANGLEN-1).
00698
00699       MOVE WK3 TO CP-CALC-PREMIUM.
00700
00701      GO TO 99999-RETURN.
00702
00703  12000-CHECK-MINN-TRUNC.
00704
00705      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00706         (CP-STATE-STD-ABBRV EQUAL 'MN') AND
00707         (M GREATER THAN +63) AND
00708         (CP-EARN-AS-NET-PAY) AND
00709         (CP-TRUNCATED-LIFE)
00710         NEXT SENTENCE
00711      ELSE
00712         GO TO 12000-CHECK-MINN.
00713
00714       COMPUTE ANGLEN EQUAL WK1 / I.
00715       COMPUTE ANGLEN-M EQUAL WK5 / I.
00716
00717       COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00718
00719       COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT *
00720        (CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
00721        (((M - ANGLEN + ANGLEN-M) / I) + (2 * M)).
00722
00723       MOVE WK3 TO CP-CALC-PREMIUM.
00724
00725      GO TO 99999-RETURN.
00726
00727  12000-CHECK-MINN.
00728
00729      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00730         (CP-STATE-STD-ABBRV EQUAL 'MN') AND
00731         (M GREATER THAN +63) AND
00732         (CP-EARN-AS-NET-PAY)
00733         NEXT SENTENCE
00734      ELSE
00735         GO TO 12000-CHECK-MONTANA-TRUNC.
00736
00737       COMPUTE ANGLEN EQUAL WK1 / I.
00738
00739       COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00740
00741       COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT *
00742        (CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
00743        (((N - ANGLEN) / I) + (2 * N)).
00744
00745       MOVE WK3 TO CP-CALC-PREMIUM.
00746
00747      GO TO 99999-RETURN.
00748
00749  12000-CHECK-MONTANA-TRUNC.
00750
00751      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00752         (CP-STATE-STD-ABBRV EQUAL 'MT') AND
00753         (M GREATER THAN +63) AND
00754         (CP-EARN-AS-NET-PAY) AND
00755         (CP-TRUNCATED-LIFE)
00756         NEXT SENTENCE
00757      ELSE
00758         GO TO 12000-CHECK-MONTANA.
00759
00760       COMPUTE ANGLEN EQUAL WK1 / I.
00761       COMPUTE ANGLEN-M EQUAL WK5 / I.
00762
00763       COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00764
00765       COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT *
00766       ((CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
00767        (((M - ANGLEN + ANGLEN-M) / I) + (4 * M)).
00768
00769       MOVE WK3 TO CP-CALC-PREMIUM.
00770
00771      GO TO 99999-RETURN.
00772
00773  12000-CHECK-MONTANA.
00774
00775      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00776         (CP-STATE-STD-ABBRV EQUAL 'MT') AND
00777         (M GREATER THAN +63) AND
00778         (CP-EARN-AS-NET-PAY)
00779         NEXT SENTENCE
00780      ELSE
00781         GO TO 12001-CONTINUE-NCL-TRUNC.
00782
00783       COMPUTE ANGLEN EQUAL WK1 / I.
00784
00785       COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00786
00787       COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT *
00788        (CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
00789        (((N - ANGLEN) / I) + (4 * N)).
00790
00791       MOVE WK3 TO CP-CALC-PREMIUM.
00792
00793      GO TO 99999-RETURN.
00794
00795  12001-CONTINUE-NCL-TRUNC.
00796
00797      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00798         (CP-EARN-AS-NET-PAY) AND
00799         (CP-TRUNCATED-LIFE)
00800         NEXT SENTENCE
00801      ELSE
00802         GO TO 12001-CONTINUE-NCL.
00803
00804       COMPUTE ANGLEN EQUAL WK1 / I.
00805       COMPUTE ANGLEN-M EQUAL WK5 / I.
00806
00807       COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00808
00809       COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT * 2 *
00810       (M - (ANGLEN - ANGLEN-M)) * (CP-PREMIUM-RATE / +100)) /
00811       (ANGLEN-1 * (N + 1) * I).
00812
00813       MOVE WK3 TO CP-CALC-PREMIUM.
00814
00815      GO TO 99999-RETURN.
00816
00817  12001-CONTINUE-NCL.
00818
00819      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00820         (CP-STATE-STD-ABBRV EQUAL 'NC') AND
00821         (CP-EARN-AS-NET-PAY)
00822         GO TO 12001-CHECK-NC-NET-PLUS3.
00823
00824      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00825         (CP-STATE-STD-ABBRV EQUAL 'CA') AND
00826         (CP-EARN-AS-NET-PAY)
00827         GO TO 12001-CHECK-CA.
00828
00829      IF (CP-COMPANY-ID EQUAL 'NCL') AND
00830         (CP-EARN-AS-NET-PAY)
00831         NEXT SENTENCE
00832      ELSE
00833         GO TO 12000-CONTINUE-NET.
00834
00835       COMPUTE ANGLEN EQUAL WK1 / I.
00836       MOVE ANGLEN                TO ANGLEN-1.
00837
00838       COMPUTE ANGLEN-1 EQUAL ANGLEN * ODF.
00839
00840       COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT *
00841        (CP-PREMIUM-RATE / +100) * 2) / (ANGLEN-1 * (N + 1)) *
00842        ((N - ANGLEN) / I).
00843
00844       MOVE WK3 TO CP-CALC-PREMIUM.
00845
00846      GO TO 99999-RETURN.
00847
00848  12000-CONTINUE-NET.
00849
00850      IF (CP-COMPANY-ID EQUAL 'TMS') AND
00851         (CP-STATE-STD-ABBRV EQUAL 'MN' OR 'CA' OR 'WA') AND
00852         (CP-EARN-AS-NET-PAY)
00853         NEXT SENTENCE
00854      ELSE
00855         GO TO 12001-CONTINUE-MINN.
00856
00857       COMPUTE ANGLEN EQUAL WK6 / I.
00858       COMPUTE ODF EQUAL 1 + (12 * CP-TERM-OR-EXT-DAYS / +365).
00859
00860       COMPUTE WK7 EQUAL (1 + ODF * I) / (1 + ANGLEN).
00861       COMPUTE WK3 EQUAL (M  * WK7) - 1.
00862       COMPUTE WK3 EQUAL (WK3 / I) *
00863        ((M * (CP-PREMIUM-RATE / +100)) / (+6 * (M + 1))).
00864
00865       IF CP-R-MAX-TOT-BEN NOT NUMERIC
00866          MOVE +0 TO CP-R-MAX-TOT-BEN.
00867
00868       COMPUTE WK3 EQUAL (WK3 * CP-ORIGINAL-BENEFIT)
00869
00870       IF (CP-ORIGINAL-BENEFIT GREATER THAN CP-R-MAX-TOT-BEN) AND
00871          (CP-R-MAX-TOT-BEN NOT EQUAL +0)
00872          COMPUTE WK3 EQUAL (WK3 * CP-R-MAX-TOT-BEN).
00873
00874       MOVE WK3 TO CP-CALC-PREMIUM.
00875
00876      GO TO 99999-RETURN.
00877
00878  12001-CONTINUE-MINN.
00879
00880      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00881         (CP-STATE-STD-ABBRV EQUAL 'NJ') AND
00882         (CP-TRUNCATED-LIFE)
00883         NEXT SENTENCE
00884      ELSE
00885         GO TO 12001-CHECK-RI-GTL.
00886
00887       COMPUTE ANGLEN EQUAL WK1 / I.
00888       COMPUTE ANGLEM EQUAL WK2 / I.
00889
00890       COMPUTE WK3 EQUAL ((M - ANGLEM * SX) /
00891        (I * ANGLEN) * CP-PREMIUM-RATE / (1 + WS-WORK-DIS-RATE
00892        * M / 2400) * 1000 + .5) / 1000000.
00893
00894       COMPUTE CP-CALC-PREMIUM EQUAL WK3 * (CP-ORIGINAL-BENEFIT
00895        - CP-ORIGINAL-PREMIUM) / (1 - WK3).
00896
00897      GO TO 99999-RETURN.
00898
00899  12001-CHECK-RI-GTL.
00900
00901      IF (CP-COMPANY-ID EQUAL 'GTL' ) AND
00902         (CP-STATE-STD-ABBRV EQUAL 'RI') AND
00903         (CP-EARN-AS-NET-PAY)
00904         NEXT SENTENCE
00905      ELSE
00906         GO TO 12001-CHECK-RI-LEASE-GTL.
00907
00908       COMPUTE ANGLEN EQUAL WK1 / I.
00909 *     COMPUTE ANGLEM EQUAL WK2 / I.
00910
00911       COMPUTE WK3 EQUAL (1000 * I * ANGLEN) *
00912                (1 + WS-WORK-DIS-RATE * N).
00913       COMPUTE CP-CALC-PREMIUM EQUAL CP-ORIGINAL-BENEFIT *
00914          ((N - ANGLEN) * (1 + 2 * I) * CP-PREMIUM-RATE) / WK3.
00915
00916
00917      GO TO 99999-RETURN.
00918
00919  12001-CHECK-RI-LEASE-GTL.
00920
00921      IF (CP-COMPANY-ID EQUAL 'GTL' ) AND
00922         (CP-STATE-STD-ABBRV EQUAL 'RI') AND
00923         (CP-SPECIAL-CALC-CD EQUAL 'H')
00924         NEXT SENTENCE
00925      ELSE
00926         GO TO 12001-CHECK-OR-GTL.
00927
00928      COMPUTE ANGLEN EQUAL WK1 / I.
00929
00930      COMPUTE WK3 EQUAL (CP-ORIGINAL-BENEFIT / ANGLEN *
00931         (N - ANGLEN) * CP-PREMIUM-RATE * (1 + 2 * I))
00932                         /
00933         (1000 * I * (1 + .0021 * N)).
00934      COMPUTE CP-CALC-PREMIUM EQUAL WK3 +
00935        (CP-ALTERNATE-BENEFIT * CP-PREMIUM-RATE * (N + 1))
00936                             /
00937        (1000 * (1 + .0027 * (N + 1))).
00938
00939      GO TO 99999-RETURN.
00940
00941  12001-CHECK-OR-GTL.
00942
00943      IF (CP-COMPANY-ID EQUAL 'GTL' ) AND
00944         (CP-STATE-STD-ABBRV EQUAL 'OR') AND
00945         (CP-EARN-AS-NET-PAY)
00946         NEXT SENTENCE
00947      ELSE
00948         GO TO 12001-CHECK-GA-GTL.
00949
00950      COMPUTE ANGLEN-M EQUAL WK5 / I.
00951      COMPUTE ANGLEN EQUAL WK1 / I.
00952
00953      IF M LESS THAN +64
00954         COMPUTE WK3 EQUAL (((N - (N - M)) - (ANGLEN - ANGLEN-M)) *
00955          (1 + I) * CP-PREMIUM-RATE * (N - (N - M)))
00956                    /
00957          (I * ANGLEN * (N - (N - M) + 1) * 600)
00958         COMPUTE CP-CALC-PREMIUM EQUAL WK3 * CP-ORIGINAL-BENEFIT
00959      ELSE
00960         COMPUTE WK3 EQUAL (((N - (N - M)) - (ANGLEN - ANGLEN-M)) *
00961         CP-PREMIUM-RATE * (1 + I))
00962                   /
00963         (+1000 * I * ANGLEN)
00964         COMPUTE CP-CALC-PREMIUM EQUAL WK3 * CP-ORIGINAL-BENEFIT.
00965 *    MOVE WK3 TO CP-CALC-PREMIUM.
00966      GO TO 99999-RETURN.
00967
00968  12001-CHECK-GA-GTL.
00969
00970      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00971         (CP-STATE-STD-ABBRV EQUAL 'GA') AND
00972         (CP-EARN-AS-NET-PAY)
00973 *       (CP-TRUNCATED-LIFE)
00974         NEXT SENTENCE
00975      ELSE
00976         GO TO 12001-CHECK-MA-GTL.
00977
00978      COMPUTE ANGLEN-M EQUAL WK5 / I.
00979      COMPUTE ANGLEN EQUAL WK1 / I.
00980
00981      COMPUTE WK3 EQUAL CP-ORIGINAL-BENEFIT / ANGLEN.
00982      COMPUTE WK3 EQUAL (CP-PREMIUM-RATE / +1000) * WK3 *
00983       ((N - (ANGLEN - ANGLEN-M)) / I) * (1 + 2 * I).
00984      MOVE WK3 TO CP-CALC-PREMIUM.
00985
00986      GO TO 99999-RETURN.
00987
00988  12001-CHECK-MA-GTL.
00989
00990      IF (CP-COMPANY-ID EQUAL 'GTL') AND
00991         (CP-STATE-STD-ABBRV EQUAL 'MA') AND
00992         (CP-EARN-AS-NET-PAY) AND
00993         (CP-SPECIAL-CALC-CD EQUAL 'H')
00994         NEXT SENTENCE
00995      ELSE
00996         GO TO 12001-CHECK-NV-NET.
00997
00998      COMPUTE ANGLEN EQUAL WK1 / I.
00999
01000      COMPUTE WK3 EQUAL (CP-PREMIUM-RATE / +1000) *
01001       CP-ALTERNATE-BENEFIT * N.
01002
01003      COMPUTE WK7 EQUAL (CP-ORIGINAL-BENEFIT + WK3) /
01004      (ANGLEN - (CP-PREMIUM-RATE / +1000) * (N - ANGLEN) / I).
01005
01006      COMPUTE WK3 EQUAL WK3 + (CP-PREMIUM-RATE / +1000) *
01007      WK7 * (N - ANGLEN) / I.
01008      MOVE WK3 TO CP-CALC-PREMIUM.
01009
01010      GO TO 99999-RETURN.
01011
01012  12001-CHECK-NV-NET.
CIDMOD     IF (CP-COMPANY-ID EQUAL 'CID')
CIDMOD        AND (CP-STATE-STD-ABBRV = 'NV' OR 'WA')
CIDMOD        AND (CP-EARN-AS-NET-PAY)
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        GO TO 12001-CHECK-MN-NET
           END-IF
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
           COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
           COMPUTE WK8 = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I) / +30)
                         / (1 + I)
           COMPUTE ANGLEN-1 = ANGLEN / WK8
           COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
           COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
              (CP-PREMIUM-RATE * K-I / 1000)
           MOVE WK3                    TO CP-CALC-PREMIUM
           GO TO 99999-RETURN
            .
       12001-CHECK-MN-NET.
           IF (CP-COMPANY-ID EQUAL 'CID')
              AND (CP-STATE-STD-ABBRV = 'MN')
              AND (CP-EARN-AS-NET-PAY)
              AND (NOT CP-LEVEL-LIFE)
              AND (WS-DISCOUNT-OPTION = '1')
              CONTINUE
           ELSE
              GO TO 12001-CHECK-NC-NET
           END-IF
050713     if i = .0000000001
050713        compute wk4 = cp-original-benefit / n
050713        compute wk3 = wk4 * ((n * (n + 1) / 2) - ((n - m) *
050713           (n - m + 1) / 2))*(cp-premium-rate / 1000)
050713        move wk3 to cp-calc-premium
050713        go to 99999-return
050713     end-if
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
           COMPUTE ANGLEN-M = (1 - (1 / (1 + I)) ** (N - M)) / I
           COMPUTE WK8 = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I) / +30)
                         / (1 + I)
           COMPUTE ANGLEN-1 = ANGLEN / WK8
           COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
           COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
              (CP-PREMIUM-RATE / 1000) * (1 + I)
           MOVE WK3                    TO CP-CALC-PREMIUM
           GO TO 99999-RETURN
            .
01052  12001-CHECK-NC-NET.
01053
01054      IF (CP-COMPANY-ID EQUAL 'NCB' ) AND
01055         (CP-STATE-STD-ABBRV EQUAL 'NC') AND
01056         (CP-EARN-AS-NET-PAY)
01057         NEXT SENTENCE
01058      ELSE
01059         GO TO 12001-CHECK-NC-NET-PLUS3.
01060
01061       COMPUTE ANGLEN EQUAL WK1 / I.
01062       COMPUTE ANGLEN-M EQUAL WK5 / I.
01063
01064       COMPUTE WK6 ROUNDED = CP-ORIGINAL-BENEFIT /
01065           (ANGLEN - (CP-PREMIUM-RATE / 1000) *
01066           ((N - (ANGLEN - ANGLEN-M)) / I + 3 * N) -
01067           N * (CP-PREMIUM-RATE / 1000)).
01068
01069       COMPUTE WK3 ROUNDED = (CP-PREMIUM-RATE / 1000) * WK6 *
01070           ((N - (ANGLEN - ANGLEN-M)) / I + 3 * N).
01071
01072       COMPUTE WK7 ROUNDED = (1 - (CP-PREMIUM-RATE / 1000) *
01073           ((N - ANGLEN) / I + 3) / ANGLEN) /
01074           (1 - (CP-PREMIUM-RATE / 1000) * N * (N + 1) /
01075           (ANGLEN * 2))
01076
01077       COMPUTE WK7 ROUNDED = WK7 * N - 3.
01078
01079       IF ANGLEN IS NOT LESS THAN WK7
01080           COMPUTE CP-PREMIUM-RATE ROUNDED =
01081               (CP-PREMIUM-RATE / 1000) * +6.5 * (N / 12)
01082           COMPUTE WK3 ROUNDED =
01083               (CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE).
01084
01085      MOVE WK3 TO CP-CALC-PREMIUM.
01086
01087      GO TO 99999-RETURN.
01088
01089  12001-CHECK-NC-NET-PLUS3.
01090
           IF (CP-STATE-STD-ABBRV EQUAL 'NC') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
020816        (CP-COMPANY-ID NOT = 'CID' AND 'DCC' AND 'AHL' AND 'VPP')
              NEXT SENTENCE
           ELSE
              GO TO 12001-CHECK-CA.
01096
01097      MOVE CP-CERT-EFF-DT TO DC-BIN-DATE-1.
01098      MOVE ' ' TO DC-OPTION-CODE.
01099      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
01100
01101      IF DATE-CONVERSION-ERROR
01102         MOVE '2'                 TO CP-RETURN-CODE
01103         MOVE ZEROS               TO FACTOR
01104                                     CP-CALC-PREMIUM
01105          GO TO 99999-RETURN.
01106
01107      IF DC-GREG-DATE-CYMD GREATER THAN 19931231
01108         NEXT SENTENCE
01109      ELSE
01110         GO TO 12001-CONTINUE-NET.
01111
01112      COMPUTE ANGLEN EQUAL WK1 / I.
01113      COMPUTE ANGLEN-M EQUAL WK5 / I.
01114
01115      COMPUTE NC-LR = CP-PREMIUM-RATE / ( ORIGINAL-TERM / 12).
01116
01117      COMPUTE NC-R = 20 *
01118                     ((NC-LR * ORIGINAL-TERM) / 12)
01119                           / (ORIGINAL-TERM + 1).
01120
01121      COMPUTE NC-OB = NC-R / 1000.
01122
01123      COMPUTE NC-P = (((ORIGINAL-TERM - ANGLEN) / I) * NC-OB) +
01124                     (NC-OB * ORIGINAL-TERM * 3).
01125
01126      IF CP-EXT-CHG-LF
01127          COMPUTE NC-BIG-D = CP-TERM-OR-EXT-DAYS + 30
01128      ELSE
01129          MOVE 30                 TO  NC-BIG-D.
01130
01131      COMPUTE NC-LITTLE-D = (1 + I) /
01132              ((( 1 + ((NC-BIG-D / 30.4166666)) * I))).
01133
01134      COMPUTE NC-MP ROUNDED = CP-ORIGINAL-BENEFIT  /
01135              (ANGLEN * NC-LITTLE-D ).
01136
01137      COMPUTE CP-CALC-PREMIUM ROUNDED = NC-MP * NC-P.
01138
01139      GO TO 99999-RETURN.
01140
01141  12001-CHECK-CA.
01142
01143      IF (CP-STATE-STD-ABBRV EQUAL 'CA') AND
01144         (WS-DISCOUNT-OPTION = '6')
01145         NEXT SENTENCE
01146      ELSE
01147         GO TO 12001-CHECK-GTL-IA.
01148
01149      COMPUTE CA-J = CA-DISCOUNT / 12.
01150      COMPUTE ANGLEN EQUAL WK1 / I.
01151      COMPUTE CA-MP = CP-ORIGINAL-BENEFIT  / ANGLEN.
01152      COMPUTE CA-API = (1 - (1 + I)** - ORIGINAL-TERM) / I.
01153      COMPUTE CA-APJ = (1 - (1 + CA-J)** - ORIGINAL-TERM) / CA-J.
01154      COMPUTE CA-VI = (1 + I)** -1.
01155      COMPUTE CP-CALC-PREMIUM =
01156          ((CP-PREMIUM-RATE * 2) / (100 * (ORIGINAL-TERM + 1)) *
01157          ((1 + CA-J) / I) *
01158          (CA-APJ + (CA-J * CA-APJ - I * CA-API) *
01159          ((CA-VI ** (LOAN-TERM - ORIGINAL-TERM)) /
01160          (I - CA-J))) * (1 + CP-ODD-DAYS-TO-PMT * I) * CA-MP).
01161
01162      GO TO 99999-RETURN.
01163
01164  12001-CHECK-GTL-IA.
01165
01166      IF (CP-COMPANY-ID EQUAL 'GTL')     AND
01167         (CP-STATE-STD-ABBRV EQUAL 'IA') AND
01168         (CP-EARN-AS-NET-PAY)
01169         NEXT SENTENCE
01170      ELSE
CIDMOD        GO TO 12001-CHECK-CID-TRN-VA.
01172
01173 ***************************************************************
01174
01175      COMPUTE GTL-IA-ODD-DAYS = CP-ODD-DAYS-TO-PMT - 30.
01176
01177      IF GTL-IA-ODD-DAYS = ZEROS
01178          MOVE ZERO               TO  GTL-IA-SGN
01179      ELSE
01180          IF GTL-IA-ODD-DAYS GREATER THAN ZERO
01181              MOVE +1             TO  GTL-IA-SGN
01182          ELSE
01183              MOVE -1             TO  GTL-IA-SGN.
01184
01185      MOVE GTL-IA-ODD-DAYS        TO  GTL-IA-ABS.
01186      COMPUTE GTL-IA-ABS = GTL-IA-ABS - 30.
01187
01188      COMPUTE GTL-IA-ODF = (1 + I * GTL-IA-ABS / 30) **
01189                           GTL-IA-SGN.
01190
01191      COMPUTE ANGLEN EQUAL WK1 / I.
01192      COMPUTE ANGLEN-M EQUAL WK5 / I.
01193
01194      IF GTL-IA-ODF NOT = ZERO
01195          COMPUTE ANGLEN EQUAL ANGLEN / GTL-IA-ODF
01196          COMPUTE ANGLEN-M EQUAL ANGLEN-M / GTL-IA-ODF.
01197
01198      COMPUTE GTL-IA-DIVIDEND =
01199            (((N-P-LOAN - (N-P-LOAN - N-P-ORIG)) -
01200                              (ANGLEN - ANGLEN-M))) *
01201            ((K1 + I) * CP-PREMIUM-RATE *
01202                      (N-P-LOAN - (N-P-LOAN - N-P-ORIG))).
01203
01204      COMPUTE GTL-IA-DIVISOR =
01205       I * ANGLEN * (N-P-LOAN - (N-P-LOAN - N-P-ORIG) + 1) * +600.
01206
01207      COMPUTE GTL-IA-FACTOR = GTL-IA-DIVIDEND / GTL-IA-DIVISOR.
01208
01209      COMPUTE CP-CALC-PREMIUM =
01210                            GTL-IA-FACTOR * CP-ORIGINAL-BENEFIT.
01211      GO TO 99999-RETURN.
01212
01212
CIDMOD 12001-CHECK-CID-TRN-VA.
CIDMOD
CIDMOD     IF (CP-COMPANY-ID EQUAL 'CID')     AND
CIDMOD        (CP-STATE-STD-ABBRV EQUAL 'VA') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-TRUNCATED-LIFE) AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE)
CIDMOD        CONTINUE
CIDMOD     ELSE
120403        GO TO 12001-CHECK-CID-TX
CIDMOD     END-IF
CIDMOD
CIDMOD     COMPUTE ANGLEN EQUAL WK1 / I
CIDMOD     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
CIDMOD     IF WS-WORK-DIS-RATE = ZEROS
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
CIDMOD        COMPUTE WK7 = (1 / I) *
CIDMOD              (((WK4 ** M - 1) / (WK4 - 1)) -
CIDMOD              ((WK4 ** M * SX - VX) /
CIDMOD              (WK4 * (1 + I) - 1))) *
CIDMOD              (CP-PREMIUM-RATE / 1000)
CIDMOD        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN
CIDMOD        COMPUTE WK8 = WK7 * 100 / ANGLEN
CIDMOD        COMPUTE CP-CALC-PREMIUM =
CIDMOD           WK8 *  ((WK4 * ANGLEN) / 100)
CIDMOD     END-IF
CIDMOD
CIDMOD     GO TO 99999-RETURN
CIDMOD
01163      .
01212
120403 12001-CHECK-CID-TX.
120403     IF (CP-COMPANY-ID EQUAL 'CID')     AND
120403        (CP-STATE-STD-ABBRV EQUAL 'TX') AND
120403        (CP-EARN-AS-NET-PAY) AND
120403        (WS-WORK-DIS-RATE > ZEROS) AND
120403        (CP-STATE-STD-ABBRV = CP-STATE)
120403        CONTINUE
120403     ELSE
120403        GO TO 12001-CHECK-CID-LEV-MN-NEW
120403     END-IF
      * THE FOLLOWING STMT FORCES ONE MONTH EXTRA INT PERIOD
           COMPUTE K-I = 1 + (1 * I)
012820     COMPUTE ANGLEm EQUAL WK1 / I   *>   Loan term
012820     COMPUTE ANGLEn EQUAL WK2 / I   *>   Ins  term
012820     COMPUTE D = CP-TERM-OR-EXT-DAYS + +30
012820
012820     COMPUTE Y = (1 + (D * I) / 30) / (1 + I)
012820     COMPUTE ANGLEmy = ANGLEm / Y
120403     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
120403     IF WS-WORK-DIS-RATE = ZEROS
120403        CONTINUE
120403     ELSE
120403        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
120403        COMPUTE WK7 = (K-I / I) *
120403              (((WK4 ** M - 1) / (WK4 - 1)) -
120403*             ((WK4 ** M * SX - VX) /
062007              ((WK4 ** M * SX) - VX) /
120403*             (WK4 * (1 + I) - 1))) *
062007              (WK4 * (1 + I) - 1)) *
120403              (CP-PREMIUM-RATE / 1000)
012820        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEmy
012820        COMPUTE WK8 = (WK7 * 100) / (ANGLEmy * K-I)
120403        COMPUTE CP-CALC-PREMIUM =
012820           WK8 *  ((WK4 * ANGLEmy * K-I) / 100)
120403     END-IF
120403
120403     GO TO 99999-RETURN
120403     .
       12001-CHECK-CID-LEV-MN-NEW.
      *    IF (CP-COMPANY-ID EQUAL 'CID')
      *       AND (CP-STATE-STD-ABBRV = 'MN')
      *       AND (CP-EARN-AS-NET-PAY)
      *       AND (CP-LEVEL-LIFE)
      *       AND (CP-CERT-EFF-DT > X'A4FF')
      *       CONTINUE
      *    ELSE
      *       GO TO 12001-CHECK-CID-LEV-MN
      *    END-IF
      *
      *    MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1
      *    MOVE CP-EXPIRE-DT           TO DC-BIN-DATE-2
      *    MOVE '1'                    TO DC-OPTION-CODE
      *    PERFORM 9100-CONVERT-DATE THRU 9100-EXIT
      *
      *    IF DATE-CONVERSION-ERROR
      *       MOVE '2'                 TO CP-RETURN-CODE
      *       MOVE ZEROS               TO FACTOR
      *                                   CP-CALC-PREMIUM
      *       GO TO 99999-RETURN
      *    END-IF
      *
      *    COMPUTE I = CP-LOAN-APR / 100
      *    COMPUTE D = DC-ELAPSED-DAYS + CP-TERM-OR-EXT-DAYS
      *
      *    COMPUTE WK3 = (CP-ORIGINAL-BENEFIT - CP-ORIGINAL-PREMIUM) /
      *      (1 / (1 + (I * D / 360)) - CP-PREMIUM-RATE / 1000
      *      * 12 / 360 * D)
      *
      *    COMPUTE CP-CALC-PREMIUM = WK3 * CP-PREMIUM-RATE / 1000
      *       * 12 / 360 * D
      *
      *    GO TO 99999-RETURN
            .
CIDMOD 12001-CHECK-CID-LEV-MN.
CIDMOD
CIDMOD     IF (CP-COMPANY-ID EQUAL 'CID')     AND
CIDMOD        (CP-STATE-STD-ABBRV EQUAL 'MN') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-LEVEL-LIFE) AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE)
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        GO TO 12001-CHECK-CID-LEV-CA
CIDMOD     END-IF
CIDMOD
CIDMOD     COMPUTE CP-CALC-PREMIUM = (CP-ORIGINAL-BENEFIT *
CIDMOD         (N / +12) * (CP-PREMIUM-RATE / +100)) /
CIDMOD         (1 - (N / +12) * (CP-PREMIUM-RATE / 100))
CIDMOD
CIDMOD     GO TO 99999-RETURN
CIDMOD
            .
CIDMOD 12001-CHECK-CID-LEV-CA.
CIDMOD
CIDMOD     IF (CP-COMPANY-ID EQUAL 'CID')     AND
CIDMOD        (CP-STATE-STD-ABBRV EQUAL 'CA') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-LEVEL-LIFE) AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE)
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        GO TO 12001-CHECK-CID-TRN-CA
CIDMOD     END-IF
CIDMOD
CIDMOD     COMPUTE WK4 = WS-WORK-DIS-RATE / 12
CIDMOD     COMPUTE WK7 = (1 / (1 + WK4)) ** N
CIDMOD     COMPUTE WK8 = (1 / (1 + WK4)) ** (N - 1)
CIDMOD     COMPUTE ANGLEN-1 = (1 - WK8) / WK4
CIDMOD     COMPUTE CP-CALC-PREMIUM =
CIDMOD          (CP-ORIGINAL-BENEFIT * (1 + ANGLEN-1) *
CIDMOD          (CP-PREMIUM-RATE / 1000)) /
CIDMOD          (1 - ((1 + ANGLEN-1) * (CP-PREMIUM-RATE / 1000)))
CIDMOD
CIDMOD     GO TO 99999-RETURN
CIDMOD
            .
CIDMOD 12001-CHECK-CID-TRN-CA.
CIDMOD
CIDMOD     IF (CP-COMPANY-ID EQUAL 'CID')     AND
CIDMOD        (CP-STATE-STD-ABBRV EQUAL 'CA') AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-TRUNCATED-LIFE) AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE) AND
CIDMOD        (WS-DISCOUNT-OPTION NOT = ' ')
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        GO TO 12001-CHECK-CID-TRN-MT
CIDMOD     END-IF
CIDMOD
CIDMOD     COMPUTE ANGLEN EQUAL WK1 / I
CIDMOD     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
CIDMOD     IF WS-WORK-DIS-RATE = ZEROS
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
CIDMOD        COMPUTE WK7 = (1 / I) *
CIDMOD              (((WK4 ** M - 1) / (WK4 - 1)) -
CIDMOD              ((WK4 ** M * SX - VX) /
CIDMOD              (WK4 * (1 + I) - 1))) *
CIDMOD              (CP-PREMIUM-RATE / 1000)
CIDMOD        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN
CIDMOD        COMPUTE CP-CALC-PREMIUM =
CIDMOD           WK4 *  WK7
CIDMOD     END-IF
CIDMOD
CIDMOD     GO TO 99999-RETURN
CIDMOD
CIDMOD      .
CIDMOD 12001-CHECK-CID-TRN-MT.
CIDMOD
CIDMOD     IF (CP-COMPANY-ID EQUAL 'CID') AND
120502        ((CP-STATE-STD-ABBRV EQUAL 'MT' OR 'VT')
                         OR
              ((CP-STATE-STD-ABBRV = 'IN') AND
              (CP-CERT-EFF-DT > X'9A7F'))
032603                   OR
032603        ((CP-STATE-STD-ABBRV = 'AZ' OR 'ND') AND
032603        (CP-CERT-EFF-DT > X'9ADF'))) AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-TRUNCATED-LIFE) AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE)
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        GO TO 12001-CHECK-CID-MT
CIDMOD     END-IF
CIDMOD
120502* K-I WILL BE USED FOR EXTRA INT. PERIODS
120502     IF CP-STATE-STD-ABBRV = 'IN' OR 'ND'
120502        MOVE 0                   TO K-I
120502     ELSE
120502        MOVE 2                   TO K-I
120502     END-IF
CIDMOD     COMPUTE CP-TERM-OR-EXT-DAYS =
CIDMOD           CP-TERM-OR-EXT-DAYS + 30
CIDMOD
CIDMOD     COMPUTE ANGLEN EQUAL WK1 / I
CIDMOD     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
CIDMOD     COMPUTE WK8 = (1 + (CP-TERM-OR-EXT-DAYS * I) / +30)
CIDMOD                   / (1 + I)
CIDMOD     COMPUTE ANGLEN-1 = ANGLEN / WK8
CIDMOD     IF WS-WORK-DIS-RATE = ZEROS
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
120502*       COMPUTE WK7 = ((1 + (2 * I)) / I) *
120502        COMPUTE WK7 = ((1 + (K-I * I)) / I) *
CIDMOD              (((WK4 ** M - 1) / (WK4 - 1)) -
CIDMOD              ((WK4 ** M * SX - VX) /
CIDMOD              (WK4 * (1 + I) - 1))) *
CIDMOD              (CP-PREMIUM-RATE / 1000)
CIDMOD        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
120502*       COMPUTE WK8 = (WK7 * 100) / (ANGLEN * (1 + 2 * I))
120502        COMPUTE WK8 = (WK7 * 100) / (ANGLEN * (1 + K-I * I))
CIDMOD        COMPUTE CP-CALC-PREMIUM =
120502           WK8 *  ((WK4 * ANGLEN * (1 + K-I * I)) / 100)
120502*          WK8 *  ((WK4 * ANGLEN * (1 + 2 * I)) / 100)
CIDMOD     END-IF
CIDMOD
CIDMOD     GO TO 99999-RETURN
CIDMOD
01163      .
CIDMOD 12001-CHECK-CID-MT.
CIDMOD
CIDMOD     IF (CP-COMPANY-ID EQUAL 'CID')     AND
CIDMOD        ((CP-STATE-STD-ABBRV EQUAL 'MT' OR 'NJ'
                                      OR 'VT')
                         OR
              ((CP-STATE-STD-ABBRV = 'IN') AND
              (CP-CERT-EFF-DT > X'9A7F'))
032603                   OR
032603        ((CP-STATE-STD-ABBRV = 'AZ' OR 'ND') AND
032603        (CP-CERT-EFF-DT > X'9ADF'))) AND
CIDMOD        (CP-EARN-AS-NET-PAY) AND
CIDMOD        (CP-STATE-STD-ABBRV = CP-STATE)
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        GO TO 12001-CHECK-DCC-DD
CIDMOD     END-IF
CIDMOD
CIDMOD     COMPUTE CP-TERM-OR-EXT-DAYS =
CIDMOD           CP-TERM-OR-EXT-DAYS + 30
CIDMOD
120502* K-I WILL BE USED FOR EXTRA INT. PERIODS
120502     IF CP-STATE-STD-ABBRV = 'IN' OR 'ND'
120502        MOVE 0                   TO K-I
120502     ELSE
              IF CP-STATE-STD-ABBRV = 'NJ'
                 MOVE 1                TO K-I
              ELSE
120502           MOVE 2                TO K-I
              END-IF
120502     END-IF
CIDMOD     COMPUTE ANGLEN EQUAL WK1 / I
CIDMOD     COMPUTE WS-WORK-DIS-RATE = WS-WORK-DIS-RATE / +12
CIDMOD     COMPUTE WK8 = (1 + (CP-TERM-OR-EXT-DAYS * I) / +30)
CIDMOD                   / (1 + I)
CIDMOD     COMPUTE ANGLEN-1 = ANGLEN / WK8
CIDMOD     IF WS-WORK-DIS-RATE = ZEROS
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        COMPUTE WK4 = 1 / (1 + WS-WORK-DIS-RATE)
120502*       COMPUTE WK7 = ((1 + (2 * I)) / I) *
120502        COMPUTE WK7 = ((1 + (K-I * I)) / I) *
CIDMOD              (((WK4 ** N - 1) / (WK4 - 1)) -
CIDMOD              ((WK4 ** N - SV) /
CIDMOD              (WK4 * (1 + I) - 1))) *
CIDMOD              (CP-PREMIUM-RATE / 1000)
CIDMOD        COMPUTE WK4 = CP-ORIGINAL-BENEFIT / ANGLEN-1
120502*       COMPUTE WK8 = (WK7 * 100) / (ANGLEN * (1 + 2 * I))
120502        COMPUTE WK8 = (WK7 * 100) / (ANGLEN * (1 + K-I * I))
CIDMOD        COMPUTE CP-CALC-PREMIUM =
120502           WK8 *  ((WK4 * ANGLEN * (1 + K-I * I)) / 100)
120502*          WK8 *  ((WK4 * ANGLEN * (1 + 2 * I)) / 100)
CIDMOD     END-IF
CIDMOD
CIDMOD     GO TO 99999-RETURN
CIDMOD
01163      .
120202 12001-CHECK-DCC-DD.
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
120202        AND (CP-EARN-AS-NET-PAY)
011904        AND (CP-BEN-CATEGORY = 'D')
120202        CONTINUE
120202     ELSE
120202        GO TO 12001-CHECK-DCC-NET
120202     END-IF
120202
120202     COMPUTE CP-TERM-OR-EXT-DAYS =
120202           CP-TERM-OR-EXT-DAYS + 30
120202
120202     COMPUTE ANGLEN EQUAL WK1 / I.
120202     COMPUTE ANGLEM EQUAL WK2 / I.
120202     COMPUTE ANGLEN-M EQUAL WK5 / I.
120202
120202     COMPUTE ODF = (1+ (CP-TERM-OR-EXT-DAYS * I / 30))
120202           / (1 + I)
120202     COMPUTE GR     = ANGLEN / ODF
120202*    COMPUTE CP-PREMIUM-RATE = CP-PREMIUM-RATE *
120202*      CP-RATE-DEV-PCT
120202     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / GR
120202     COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
120202       (CP-PREMIUM-RATE / 100) * (1 + (0 * (N-P-APR / 12)))
120202     MOVE ZEROS                  TO CP-RATE-DEV-PCT
120202     MOVE WK3                    TO CP-CALC-PREMIUM
120202
120202     GO TO 99999-RETURN
120202
120202     .
120202 12001-CHECK-DCC-NET.
120202
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
120202        AND (CP-EARN-AS-NET-PAY)
120202        AND (CP-BENEFIT-CD (1:1) NOT = 'M' AND 'N'
                   AND 'H' AND 'S' AND 'P')
120202        CONTINUE
120202     ELSE
120202        GO TO 12001-CHECK-DCC-PNET
120202     END-IF
120202
120202     COMPUTE CP-TERM-OR-EXT-DAYS =
120202           CP-TERM-OR-EXT-DAYS + 30
120202
120202     COMPUTE ANGLEN EQUAL WK1 / I.
120202     COMPUTE ANGLEM EQUAL WK2 / I.
120202     COMPUTE ANGLEN-M EQUAL WK5 / I.
120202
120202     COMPUTE ODF = (1+ (CP-TERM-OR-EXT-DAYS * I / 30))
120202           / (1 + I)
120202     COMPUTE GR     = ANGLEN / ODF
120202     COMPUTE CP-PREMIUM-RATE = CP-PREMIUM-RATE / M * 12
120202     COMPUTE CP-CALC-PREMIUM ROUNDED = CP-PREMIUM-RATE *
120202       CP-RATE-DEV-PCT
120202     MOVE CP-CALC-PREMIUM        TO CP-PREMIUM-RATE
120202*    COMPUTE WK8 = CP-PREMIUM-RATE * M / 12
120202
120202     COMPUTE WK8 = CP-PREMIUM-RATE *
120202        ((M + ((CP-TERM-OR-EXT-DAYS - 30) / 30)) / 12)
120202     COMPUTE WK8 = WK8 / ((M + (12 / 12)) / 2)
120202     COMPUTE WK4 = CP-ORIGINAL-BENEFIT / GR
120202     COMPUTE WK3 = WK4 * ((M - (ANGLEN - ANGLEN-M)) / I) *
120202       (WK8 / 100) * (1 + (0 * (N-P-APR / 12)))
120202     MOVE ZEROS                  TO CP-RATE-DEV-PCT
120202     MOVE WK3                    TO CP-CALC-PREMIUM
120202
120202     GO TO 99999-RETURN
120202
120202     .
       12001-CHECK-DCC-PNET.
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
              AND (CP-EARN-AS-NET-PAY)
              AND (CP-BENEFIT-CD (1:1) = 'P')
              CONTINUE
           ELSE
120202        GO TO 12001-CHECK-DCC-MNET
           END-IF
           IF CP-NO-OF-PMTS NOT NUMERIC
              MOVE +0                  TO CP-NO-OF-PMTS
           END-IF
           IF CP-PMT-MODE NOT = 'W' AND 'S' AND 'B' AND 'T'
              AND ' ' AND 'M'
              MOVE ' '                 TO CP-PMT-MODE
           END-IF
022613******************************************************************
022613***   Default is monthly, use loan term for rate and amort     ***
022613******************************************************************
022613     move cp-loan-term         to c-tot-pmts
022613     MOVE +30                  TO C-DAYS-IN-PMT-PER
022613     MOVE +12                  TO C-PMTS-PER-YEAR
022613******************************************************************
022613
022613     evaluate true
022613        when cp-pmt-mode = 'W'
022613           MOVE +52              TO C-PMTS-PER-YEAR
022613           compute c-tot-pmts = cp-loan-term /
022613              12 * c-pmts-per-year
022613           MOVE +7               TO C-DAYS-IN-PMT-PER
022613        when cp-pmt-mode = 'B'
022613           MOVE +26              TO C-PMTS-PER-YEAR
022613           compute c-tot-pmts = cp-loan-term /
022613              12 * c-pmts-per-year
022613           MOVE +14              TO C-DAYS-IN-PMT-PER
022613     end-evaluate
           COMPUTE I = CP-LOAN-APR / C-PMTS-PER-YEAR / 100
           COMPUTE Y = (1 + (CP-DAYS-TO-1ST-PMT * I /
              C-DAYS-IN-PMT-PER)) / (1 + I)
           compute anglen = (1 - (1 / (1 + i)) ** c-tot-pmts) / i
      *    COMPUTE ANGLEN = (1 - ((1 + I) ** (C-TOT-PMTS * -1))) / I
           COMPUTE ANGLEN-1 = ANGLEN / Y
           COMPUTE WK3 = CP-ORIGINAL-BENEFIT / ANGLEN-1
           IF CP-RATE-DEV-PCT NOT = ZEROS
              COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
                 CP-PREMIUM-RATE
           END-IF
           COMPUTE CP-CALC-PREMIUM = (WK3 / 100) * CP-PREMIUM-RATE
           GO TO 99999-RETURN
           .
120202 12001-CHECK-DCC-MNET.
120202
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
120202        AND (CP-EARN-AS-NET-PAY)
120202        AND (CP-BENEFIT-CD (1:1) = 'M' OR 'H')
120202        CONTINUE
120202     ELSE
120202        GO TO 12001-CHECK-DCC-MNET-LF-BAL
120202     END-IF
120202
120202*    COMPUTE ANGLEN EQUAL WK1 / I.
120202*    COMPUTE ANGLEM EQUAL WK2 / I.
120202*    COMPUTE ANGLEN-M EQUAL WK5 / I.
120202
120202     COMPUTE WK3 = CP-ORIGINAL-BENEFIT * CP-PREMIUM-RATE / 1000
           MOVE WK3                    TO CP-CALC-PREMIUM
120202
120202     GO TO 99999-RETURN
120202
120202     .
011904 12001-CHECK-DCC-MNET-LF-BAL.
011904
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
011904        AND (CP-EARN-AS-NET-PAY)
011904        AND (CP-BENEFIT-CD (1:1) = 'N')
011904        AND (CP-BEN-CATEGORY NOT = 'N')
              AND (CP-ALTERNATE-BENEFIT NOT = ZEROS)
011904        CONTINUE
011904     ELSE
011904        GO TO 12001-CHECK-DCC-MNET-LFSUM-BAL
011904     END-IF
011904
           IF CP-RATE-DEV-PCT NOT = ZEROS
              COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
                 CP-PREMIUM-RATE
           END-IF
      *    COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
083105     COMPUTE OD = (1 + (CP-DAYS-TO-1ST-PMT * I / 30))
              / (1 + I)
           COMPUTE ANGLEN-M = (WK5 / I) / OD
011904     COMPUTE J = I + (CP-PREMIUM-RATE / +1000)
           COMPUTE WK2 = (1 + J) ** (M * -1)
           COMPUTE ANGLEN-M = ANGLEN-M * WK2
011904     COMPUTE ANGLEM-1 = ((1 - ((1 + J)
                ** ((M - 1) * -1))) / J)
011904     COMPUTE ANGLEM = ((1 - ((1 + J)
                ** (M * -1))) / J) / OD
           COMPUTE RA = (CP-ORIGINAL-BENEFIT * OD -
              (CP-ALTERNATE-BENEFIT * (1 + J) ** (M * -1)))
              / ANGLEM-1
011904     COMPUTE ANGLEN-M = ((1 - ((1 + I)
                ** ((N - M) * -1))) / I)
           MOVE +0                     TO WK3
011904     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
011904        (WS-COUNTER > M)
011904        COMPUTE ANGLEM = (1 - ((1 + J)
                ** ((M - WS-COUNTER) * -1))) / J
              COMPUTE WK2 = (1 / (1 + j)) ** (M - WS-COUNTER + 1)
              COMPUTE WK5-CSL = (RA * ANGLEM)
              + (CP-ALTERNATE-BENEFIT * WK2)
              COMPUTE WK3 = WK3 + (WK5-CSL * CP-PREMIUM-RATE / +1000)
           END-PERFORM
011904     MOVE WK3                    TO CP-CALC-PREMIUM
011904
011904     GO TO 99999-RETURN
011904
011904     .
011904 12001-CHECK-DCC-MNET-LFSUM-BAL.
011904
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
011904        AND (CP-EARN-AS-NET-PAY)
011904        AND (CP-BENEFIT-CD (1:1) = 'S')
011904        AND (CP-BEN-CATEGORY NOT = 'N')
              AND (CP-ALTERNATE-BENEFIT NOT = ZEROS)
011904        CONTINUE
011904     ELSE
011904        GO TO 12001-CHECK-DCC-MNET-LF
011904     END-IF
011904
           IF CP-RATE-DEV-PCT NOT = ZEROS
              COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
                 CP-PREMIUM-RATE
           END-IF
011904     COMPUTE K = I + (CP-PREMIUM-RATE / +1000)
011904     COMPUTE ANGLEM-1 = ((1 - ((1 + K)
                ** ((M - 1) * -1))) / K)
011904     COMPUTE ANGLEM = ((1 - ((1 + K)
                ** (M * -1))) / K)
           MOVE CP-MONTHLY-PAYMENT     TO RA
           COMPUTE WK3 = (((((M - 1) - ANGLEM-1) / K) - ((((M - 1)
              * M / 2 - (((M - 1) - ANGLEM-1) / K)) / K) * 0 / 1000))
              * RA + (CP-ALTERNATE-BENEFIT * ANGLEM)) *
              CP-PREMIUM-RATE / 1000
011904     MOVE WK3                    TO CP-CALC-PREMIUM
011904
011904     GO TO 99999-RETURN
011904
011904     .
011904 12001-CHECK-DCC-MNET-LF.
011904
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
011904        AND (CP-EARN-AS-NET-PAY)
011904        AND (CP-BENEFIT-CD (1:1) = 'N')
011904        AND (CP-BEN-CATEGORY NOT = 'N')
011904        CONTINUE
011904     ELSE
011904        GO TO 12001-CHECK-DCC-MNET-SUMLF
011904     END-IF
011904
           IF CP-RATE-DEV-PCT NOT = ZEROS
              COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
                 CP-PREMIUM-RATE
           END-IF
      *    COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
083105     COMPUTE OD = (1 + (CP-DAYS-TO-1ST-PMT * I / 30))
              / (1 + I)
           COMPUTE ANGLEN-M = (WK5 / I) / OD
011904     COMPUTE J = I + (CP-PREMIUM-RATE / +1000)
           COMPUTE WK2 = (1 + J) ** (M * -1)
           COMPUTE ANGLEN-M = ANGLEN-M * WK2
011904     COMPUTE ANGLEM = ((1 - ((1 + J)
                ** (M * -1))) / J) / OD
           COMPUTE RA = CP-ORIGINAL-BENEFIT / (ANGLEM + ANGLEN-M)
011904     COMPUTE ANGLEN-M = ((1 - ((1 + I)
                ** ((N - M) * -1))) / I)
           MOVE +0                     TO WK3
011904     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
011904        (WS-COUNTER > M)
011904        COMPUTE ANGLEM = (1 - ((1 + J)
                ** ((M - WS-COUNTER + 1) * -1))) / J
              COMPUTE WK2 = (1 / (1 + j)) ** (M - WS-COUNTER + 1)
              COMPUTE WK5-CSL = RA * (ANGLEM + (ANGLEN-M * WK2))
              COMPUTE WK3 = WK3 + (WK5-CSL * CP-PREMIUM-RATE / +1000)
           END-PERFORM
011904     MOVE WK3                    TO CP-CALC-PREMIUM
011904
011904     GO TO 99999-RETURN
011904
011904     .
011904 12001-CHECK-DCC-MNET-SUMLF.
011904
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
011904        AND (CP-EARN-AS-NET-PAY)
011904        AND (CP-BENEFIT-CD (1:1) = 'S')
011904        AND (CP-BEN-CATEGORY NOT = 'N')
011904        CONTINUE
011904     ELSE
011904        GO TO 12001-CHECK-DCC-MNET-TOT-BAL-N
011904     END-IF
011904
           IF CP-RATE-DEV-PCT NOT = ZEROS
              COMPUTE CP-PREMIUM-RATE = CP-RATE-DEV-PCT *
                 CP-PREMIUM-RATE
           END-IF
           COMPUTE K = I + (CP-PREMIUM-RATE / +1000)
           COMPUTE Y = (1 + (CP-DAYS-TO-1ST-PMT * K / 30))
              / (1 + K)
           COMPUTE ANGLEN-M = (WK5 / I)
           COMPUTE WK2 = (1 + K) ** (M * -1)
           COMPUTE ANGLEN-M = ANGLEN-M * WK2
           COMPUTE ANGLEN-M = ANGLEN-M / Y
011904     COMPUTE ANGLEM = ((1 - ((1 + K)
                ** (M * -1))) / K) / Y
           COMPUTE RA = CP-ORIGINAL-BENEFIT / (ANGLEM + ANGLEN-M)
           COMPUTE WK5-CSL = (((M - ANGLEM) / K) + (ANGLEN-M *
              ((1 + K) ** (M + 1 / Y * (CP-DAYS-TO-1ST-PMT - +30) / 30)
              - 1)) / K) * RA
           COMPUTE WK7 = (((((M * (M + 1) / 2)
              + (CP-DAYS-TO-1ST-PMT - +30)
              / 30 * M - (M - ANGLEM) / K) / K)) * RA *
              +0 / 1000 * 12 / 12)
           COMPUTE WK3 = (WK5-CSL - WK7) * (CP-PREMIUM-RATE / 1000)
              * (12 / 12)
011904     MOVE WK3                    TO CP-CALC-PREMIUM
011904
011904     GO TO 99999-RETURN
011904
011904     .
011904 12001-CHECK-DCC-MNET-TOT-BAL-N.
011904
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
011904        AND (CP-EARN-AS-NET-PAY)
011904        AND (CP-BENEFIT-CD (1:1) = 'N')
011904        AND (CP-BEN-CATEGORY = 'N')
              AND (CP-ALTERNATE-BENEFIT NOT = ZEROS)
011904        CONTINUE
011904     ELSE
011904        GO TO 12001-CHECK-DCC-MNET-TOT-BAL-S
011904     END-IF
011904
      *    COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
083105     COMPUTE OD = (1 + (CP-DAYS-TO-1ST-PMT * I / 30))
              / (1 + I)
           COMPUTE ANGLEN-M = (WK5 / I) / OD
011904     COMPUTE J = I + ((CP-DCC-LF-RATE + CP-DCC-AH-RATE) / +1000)
011904     COMPUTE K = I + (CP-DCC-LF-RATE / +1000)
           COMPUTE WK2 = (1 + J) ** (M * -1)
           COMPUTE ANGLEN-M = ANGLEN-M * WK2
011904     COMPUTE ANGLEM-1 = ((1 - ((1 + J)
                ** ((M - 1) * -1))) / J)
011904     COMPUTE ANGLEM = ((1 - ((1 + J)
                ** (M * -1))) / J) / OD
           COMPUTE RA = (CP-ORIGINAL-BENEFIT * OD -
              (CP-ALTERNATE-BENEFIT * (1 + K) ** (M * -1)))
              / ANGLEM-1
011904     COMPUTE ANGLEN-M = ((1 - ((1 + I)
                ** ((N - M) * -1))) / I)
           MOVE +0                     TO WK3
011904     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
011904        (WS-COUNTER > M)
011904        COMPUTE ANGLEM = (1 - ((1 + J)
                ** ((M - WS-COUNTER) * -1))) / J
              COMPUTE WK2 = (1 / (1 + K)) ** (M - WS-COUNTER + 1)
              COMPUTE WK5-CSL = (RA * ANGLEM)
              + (CP-ALTERNATE-BENEFIT * WK2)
              COMPUTE WK3 = WK3 +
               (WK5-CSL * (CP-DCC-LF-RATE / +1000))
              COMPUTE WK3 = WK3 + (WK5-CSL - (CP-ALTERNATE-BENEFIT *
                (1 / (1 + K) ** (M - WS-COUNTER + 1)))) *
                (CP-DCC-AH-RATE / 1000)
           END-PERFORM
011904     MOVE WK3                    TO CP-CALC-PREMIUM
011904
011904     GO TO 99999-RETURN
011904
011904     .
011904 12001-CHECK-DCC-MNET-TOT-BAL-S.
011904
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
011904        AND (CP-EARN-AS-NET-PAY)
011904        AND (CP-BENEFIT-CD (1:1) = 'S')
011904        AND (CP-BEN-CATEGORY = 'N')
              AND (CP-ALTERNATE-BENEFIT NOT = ZEROS)
011904        CONTINUE
011904     ELSE
011904        GO TO 12001-CHECK-DCC-MNET-TOT-N
011904     END-IF
011904
011904     COMPUTE K = I + (CP-DCC-LF-RATE / +1000)
011904     COMPUTE ANGLEM-1 = ((1 - ((1 + K)
                ** ((M - 1) * -1))) / K)
011904     COMPUTE ANGLEM = ((1 - ((1 + K)
                ** (M * -1))) / K)
           MOVE CP-MONTHLY-PAYMENT     TO RA
           COMPUTE WK3 = (((((M - 1) - ANGLEM-1) / K) - ((((M - 1)
              * M / 2 - (((M - 1) - ANGLEM-1) / K)) / K)
              * CP-DCC-AH-RATE / 1000))
              * RA + (CP-ALTERNATE-BENEFIT * ANGLEM)) *
              CP-DCC-LF-RATE / 1000
011904     MOVE WK3                    TO CP-LF-PREM
           COMPUTE WK5-CSL = RA * ((M - 1) * M / 2
              + (M - 1) * (CP-DAYS-TO-1ST-PMT - 30) / 30)
              * CP-DCC-AH-RATE / 1000
           COMPUTE WK3 = WK3 + WK5-CSL
011904     MOVE WK3                    TO CP-CALC-PREMIUM
011904
011904     GO TO 99999-RETURN
            .
011904 12001-CHECK-DCC-MNET-TOT-N.
011904
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
011904        AND (CP-EARN-AS-NET-PAY)
011904        AND (CP-BENEFIT-CD (1:1) = 'N')
011904        AND (CP-BEN-CATEGORY = 'N')
011904        CONTINUE
011904     ELSE
011904        GO TO 12001-CHECK-DCC-MNET-TOT-S
011904     END-IF
011904
      *    I = APR/1200
      *    J = APR/1200 THEN ADJUSTED FOR THE LIFE AND DIS RATE
      *    RA = PAYMENT AMOUNT
      *    WK5-CSL = THE BALANCE (PAY OFF )
      *    WK3 = THE ACCUMULATED PREMIUM
      *    WK2 = (1 + I) ^ (m * -1)
      *    OD  = GAMMA
      *
      *    COMPUTE OD = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I / 30))
083105     COMPUTE OD = (1 + (CP-DAYS-TO-1ST-PMT * I / 30))
              / (1 + I)
           COMPUTE ANGLEN-M = (WK5 / I) / OD
011904     COMPUTE J = I + (CP-DCC-LF-RATE / +1000) +
011904        (CP-DCC-AH-RATE / +1000)
           COMPUTE WK2 = (1 + J) ** (M * -1)
           COMPUTE ANGLEN-M = ANGLEN-M * WK2
011904     COMPUTE ANGLEM = ((1 - ((1 + J)
                ** (M * -1))) / J) / OD
           COMPUTE RA = CP-ORIGINAL-BENEFIT / (ANGLEM + ANGLEN-M)
011904     COMPUTE ANGLEN-M = ((1 - ((1 + I)
                ** ((N - M) * -1))) / I)
           MOVE +0                     TO WK3
011904     PERFORM VARYING WS-COUNTER FROM +1 BY +1 UNTIL
011904        (WS-COUNTER > M)
011904        COMPUTE ANGLEM = (1 - ((1 + J)
                ** ((M - WS-COUNTER + 1) * -1))) / J
              COMPUTE WK2 = (1 / (1 + j)) ** (M - WS-COUNTER + 1)
              COMPUTE WK5-CSL = RA * (ANGLEM + (ANGLEN-M * WK2))
              COMPUTE WK3 = WK3 + (WK5-CSL * CP-DCC-LF-RATE / +1000)
                 + (WK5-CSL * CP-DCC-AH-RATE / +1000)
           END-PERFORM
011904     MOVE WK3                    TO CP-CALC-PREMIUM
011904
011904     GO TO 99999-RETURN
011904
011904     .
011904 12001-CHECK-DCC-MNET-TOT-S.
011904
020816     IF (CP-COMPANY-ID EQUAL 'DCC' OR 'VPP')
011904        AND (CP-EARN-AS-NET-PAY)
011904        AND (CP-BENEFIT-CD (1:1) = 'S')
011904        AND (CP-BEN-CATEGORY = 'N')
011904        CONTINUE
011904     ELSE
011904        GO TO 12001-CONTINUE-NET
011904     END-IF
011904
      *    I = APR/1200
      *    J = APR/1200 THEN ADJUSTED FOR THE LIFE RATE (K)
      *    RA = PAYMENT AMOUNT
      *    WK5-CSL = THE BALANCE (PAY OFF )
      *    WK3 = THE ACCUMULATED PREMIUM
      *    WK2 = (1 + I) ^ (m * -1)
      *    OD  = GAMMA
      *
011904     COMPUTE J = I + (CP-DCC-LF-RATE / +1000)
           COMPUTE OD = ((1 + (CP-DAYS-TO-1ST-PMT * J / 30)))
              / (1 + J)
011904     COMPUTE ANGLEM = ((1 - ((1 + J)
                ** (M * -1))) / J) / OD
011904     COMPUTE ANGLEN-M = ((1 - ((1 + I)
                ** ((N - M) * -1))) / I)
                * (1 + J) ** (-1 * M)
           COMPUTE ANGLEN-M = ANGLEN-M / OD
           MOVE CP-MONTHLY-PAYMENT     TO RA
           COMPUTE WK1 = (M + 1 / OD * (CP-DAYS-TO-1ST-PMT - +30)
              / +30)
           COMPUTE WK4 = (((M - ANGLEM) / J) + (ANGLEN-M * ((1 + J)
              ** (WK1) - 1)) / J) * RA
           COMPUTE WK5 = (((((M * (M + 1) / 2)
              + (CP-DAYS-TO-1ST-PMT - +30)
              / +30 * M - (M - ANGLEM) / J) / J)) * RA *
              CP-DCC-AH-RATE / +1000 * 12 / 12)
           COMPUTE WK3 = (WK4 - WK5) * (CP-DCC-LF-RATE / +1000) *
              (12 / 12)
           COMPUTE WK5-CSL = RA * (M * (M + 1) / 2 + M *
              (CP-DAYS-TO-1ST-PMT - +30) / +30) *
              (CP-DCC-AH-RATE / +1000) * 12 / 12
           COMPUTE WK3 = WK3 + WK5-CSL
011904     MOVE WK3                    TO CP-CALC-PREMIUM
           COMPUTE K = I + (CP-DCC-LF-RATE / +1000)
           COMPUTE Y = (1 + (CP-DAYS-TO-1ST-PMT * K / 30))
              / (1 + K)
           COMPUTE ANGLEN-M = ((1 - ((1 + I)
                ** ((N - M) * -1))) / I)
           COMPUTE WK2 = (1 + K) ** (M * -1)
           COMPUTE ANGLEN-M = ANGLEN-M * WK2
           COMPUTE ANGLEN-M = ANGLEN-M / Y
           COMPUTE ANGLEM = ((1 - ((1 + K)
                ** (M * -1))) / K) / Y
      *    COMPUTE RA = CP-ORIGINAL-BENEFIT / (ANGLEM + ANGLEN-M)
           COMPUTE WK6 = (((M - ANGLEM) / K) + (ANGLEN-M *
              ((1 + K) ** (M + 1 / Y
              * (CP-DAYS-TO-1ST-PMT - +30) / 30)
              - 1)) / K) * RA
           COMPUTE WK7 = (((((M * (M + 1) / 2)
              + (CP-DAYS-TO-1ST-PMT - +30)
              / 30 * M - (M - ANGLEM) / K) / K)) * RA *
              CP-DCC-AH-RATE / 1000 * 12 / 12)
           COMPUTE WK3 = (WK6 - WK7) * (CP-DCC-LF-RATE / 1000)
              * (12 / 12)
011904     MOVE WK3                    TO CP-LF-PREM
011904
011904     GO TO 99999-RETURN
011904
011904     .
01213  12001-CONTINUE-NET.
01214
01215      IF NET-STD
01216         IF CP-COMPANY-ID EQUAL 'CDC' OR 'GTL' OR 'CID' OR 'MON'
120202*          OR 'DCC'
01217          COMPUTE WK3 ROUNDED = ((I * M) + VX - SX) * 2 * N
01218          COMPUTE WK3 ROUNDED = WK3 / ((1 - VX) * M * I)
01219          COMPUTE WK3 ROUNDED = WK3 / ((2 * N) - M + 1)
01220          COMPUTE WK3 ROUNDED = (WK3 * RA * K-I)
01221                                * ((1 + N) / (1 + M))
050713         if (i = .0000000001)
050713            and (wk3 = zeros)
050713            move 1               to wk3
050713         end-if
01222         ELSE
01223          COMPUTE WK3 ROUNDED = ((I * M) + VX - SX) * 2 * N
01224          COMPUTE WK3 ROUNDED = WK3 / ((1 - VX) * M * I)
01225          COMPUTE WK3 ROUNDED = WK3 / ((2 * N) - M + 1)
01226          COMPUTE WK3 ROUNDED = WK3 * RA * K-I.
01227
01228      IF NET-SMP
01229          COMPUTE N2 = N * N
01230          COMPUTE N3 = N2 * N
01231          COMPUTE WK3 ROUNDED = 2 * N2 * WK1
01232          COMPUTE WK3 ROUNDED = WK3 + (N3 * I) - (N2 * I)
01233          COMPUTE WK3 ROUNDED = WK3 + (4 * N * WK1)
01234          COMPUTE WK3 ROUNDED = WK3 * (1 + I) * 10
01235          COMPUTE WK3 ROUNDED = WK3 / (36 * (N + 1) * WK1).
01236
01237      IF NPO-PRUDENTIAL
01238        PERFORM 12100-PRU-CALC THRU 12199-EXIT
01239            VARYING WS-COUNTER FROM 1 BY 1
01240                UNTIL WS-COUNTER = N
01241        COMPUTE WK3 ROUNDED = (N * (I * PWK / (PWK - 1))) - 1
01242        COMPUTE WK3 ROUNDED = WK3 / (I * 12).
01243
01244      MOVE WK3 TO FACTOR.
01245
01246      GO TO 99999-RETURN.
01247
01248  12100-PRU-CALC.
01249      IF WS-COUNTER = 1
01250          COMPUTE PWK = (I + 1)
01251      ELSE
01252          COMPUTE PWK = PWK * (I + 1).
01253
01254  12199-EXIT.
01255       EXIT.
01256
01257  99000-ERROR.
01258      MOVE +9999.999999999 TO N-P-FACTOR.
01259      MOVE '4'             TO CP-RETURN-CODE.
01260
01261      GO TO 99999-EXIT.
01262
01263  99999-RETURN.
01264      MOVE FACTOR          TO N-P-FACTOR.
01265
01266  99999-EXIT.
01267       EXIT.
00194  99999-DUMMY-STOP-RUN.
00195      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRATE' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00196

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRATE' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9999-RATE-NOTFND,
                     9999-RATE-NOTOPEN,
                     9999-RATE-NOTFND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRATE' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
