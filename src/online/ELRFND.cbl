00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 ELRFND.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/05/96 16:23:50.
00007 *                            VMOD=2.011
00008 *
00009 *AUTHOR.       LOGIC, INC.
00010 *              DALLAS, TEXAS.
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
00024 *REMARKS.    *****************************************************
00025 *            *                                                   *
00026 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00027 *            *    OPTION SPECIFIED, COMPUTE PREMIUM REFUNDS      *
00028 *            *****************************************************
00029
00030
00031 ******************************************************************
00032 *                                                                *
00033 *              INPUT FIELDS USED                                 *
00034 *                                                                *
00035 ******************************************************************
00036 *  CERT ISSUE DATE  - CP-CERT-EFF-DT                             *
00037 *  REFUND DATE      - CP-VALUATION-DT                            *
00038 *  ORIGINAL TERM    - CP-ORIGINAL-TERM                           *
00039 *  LOAN TERM        - CP-LOAN-TERM                               *
00040 *  REMAINING TERM   - CP-REMAINING-TERM                          *
00041 *  TERM OR EXT DAYS - CP-TERM-OR-EXT-DAYS                        *
00042 *  STATE CODE       - CP-STATE                                   *
00043 *  STATE CODE       - CP-STATE-STD-ABBV                          *
00044 *  CLASS CODE       - CP-CLASS-CODE                              *
00045 *  DEVIATION CODE   - CP-DEVIATION-CODE                          *
00046 *  ORIGINAL BENEFIT - CP-ORIGINAL-BENEFIT                        *
00047 *  PROCESS TYPE     - CP-PROCESS-TYPE                            *
00048 *  BENEFIT KIND     - CP-BENEFIT-TYPE                            *
00049 *  A.P.R.           - CP-LOAN-APR                                *
00050 *  METHOD           - CP-EARNING-METHOD                          *
00051 *  SPECIAL METHOD   - CP-SPECIAL-CALC-CODE                       *
00052 *  PAYMENT FREQUENCY- CP-PAY-FREQUENCY                           *
00053 *  COMPANY I.D.     - CP-COMPANY-ID                              *
00054 *  BENEFIT CODE     - CP-BENEFIT-CD                              *
00055 *  INSURED AGE      - CP-ISSUE-AGE                               *
00056 ******************************************************************
00057  ENVIRONMENT DIVISION.
00058
00059  DATA DIVISION.
00060      EJECT
00061  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00062  77  FILLER   PIC X(32) VALUE '********************************'.
00063  77  FILLER   PIC X(32) VALUE '**  ELRFND  WORKING STORAGE   **'.
00064  77  FILLER   PIC X(32) VALUE '********VMOD=2.011 *************'.
       77  WS-UE-CLP                   PIC S9(5)V99 COMP-3 VALUE +0.
       77  WS-GFR3                     PIC S9(5)V99 COMP-3 VALUE +0.
       77  WS-MONTH                    PIC S999     COMP-3 VALUE +0.
00065
00066  01  WS-WORK-MISC.
00067      12  WS-REMAINING-TERM   PIC S9(4)V9      COMP-3.
00068      12  WS-TEMP-RESULT      PIC S9(7)V9(6)   COMP-3.
00069      12  WS-REMAINING-AMT    PIC S9(7)V9(6)   COMP-3.
00070      12  WS-ORIG-PREM        PIC S9(7)V9(6)   COMP-3.
00071      12  WS-REMAIN-PREM      PIC S9(7)V9(6)   COMP-3.
00072      12  WS-NP-REF-FACTOR    PIC S9(4)V9(11)  COMP-3.
00073      12  WS-SAVE-TERM        PIC S9(3)        COMP-3.
00074      12  WS-SAVE-LOAN-TERM   PIC S9(3)        COMP-3.
00075      12  WS-SAVE-BENEFIT     PIC S9(9)V99     COMP-3.
00076      12  WS-SAVE-EARN-METHOD PIC X.
050713     12  ws-save-ext-days    pic s9(5)        comp-3.
00077      12  CAL-RFND            PIC S9(5)V99     COMP-3.
00078      12  FACTOR-1            PIC S9(4)V9(11)  COMP-3.
00079      12  FACTOR-2            PIC S9(4)V9(11)  COMP-3.
00080      12  FACTOR-3            PIC S9(4)V9(11)  COMP-3.
00081      12  FACTOR-4            PIC S9(4)V9(11)  COMP-3.
00082      12  FACTOR-5            PIC S9(7)        COMP-3.
00083      12  FACTOR-6            PIC S9(7)        COMP-3.
00084      12  RATE-FACTOR-1       PIC S9V99        COMP-3.
00085      12  RATE-FACTOR-2       PIC S999         COMP-3.
00086      12  R78-FACTOR          PIC S9(4)V9(11)  COMP-3.
00087      12  PR-FACTOR           PIC S9(4)V9(11)  COMP-3.
00088      12  RSUM-FACTOR         PIC S9(4)V9(11)  COMP-3.
00089      12  RSUM-REMAINING-TERM PIC S9(3)V99     COMP-3.
00090
00091  01  TEXAS-REG-WORK-AREAS.
00092      12  TEX-FACT-1          PIC S9(7)V9(2)  COMP-3.
00093      12  TEX-FACT-2          PIC S9(3)       COMP-3.
00094      12  TEX-FACT-3          PIC S9(3)       COMP-3.
00095      12  TEX-FACT-4          PIC S9(7)       COMP-3.
00096      12  TEX-FACT-5          PIC S9(3)       COMP-3.
00097      12  TEX-FACT-6          PIC S9(3)       COMP-3.
00098      12  TEX-FACT-7          PIC S9(7)       COMP-3.
00099      12  TEX-FACT-8          PIC S9V9(6)     COMP-3.
00100      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.
00101
00102  01  NET-PAY-INTERFACE.
00103      12  N-P-APR             PIC S9(3)V9(4)  COMP-3.
00104      12  N-P-ORIG            PIC S9(3)       COMP-3.
00105      12  N-P-REM             PIC S9(3)       COMP-3.
00106      12  N-P-OPT             PIC X.
00107      12  N-P-LOAN            PIC S9(3)       COMP-3.
00108      12  N-P-FACTOR          PIC S9(4)V9(9)  COMP-3.
00109
00110 *                        COPY ERCNETWS.
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
00111  EJECT
00112 *                        COPY ELCDATE.
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
00113  EJECT
00114 *                        COPY ELCCALC.
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
00115
00116
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
00118  01  DFHCOMMAREA                PIC X(450).
00119
00120      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELRFND' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00122      MOVE DFHCOMMAREA    TO  CALCULATION-PASS-AREA.
00123
00124  000-START-REFUND-CALC.
00125 *                      COPY ELCRFNDP.
00001 ****************************************************************
00002 *                                                              *
00003 *                            ELCRFNDP.                         *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.028                        *
00006 *                                                              *
00007 ****************************************************************.
042203*                   C H A N G E   L O G
042203*
042203* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042203*-----------------------------------------------------------------
042203*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042203* EFFECTIVE    NUMBER
042203*-----------------------------------------------------------------
042203* 042203                   PEMA ADD SPECIAL IN CALC FOR CID
033104* 033104    2003080800002  PEMA ADD GAP NON REFUNDABLE OPTION
042904* 042904    2003080800002  PEMA ADD ACTUARIAL EARNING METHOD
110609* 110609  CR2009092300002  PEMA CHANGE MN REFUNDS
000000* 000000  CR2008042200001  PEMA ADD 0 APR PROCESSING
071910* 071910  IR2010062900001  PEMA ADD MN NET BALLOON REFUND CALC
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
032612* 032612  CR2011110200001  PEMA AHL CHANGES
050713* 050713  CR2008042200001  PEMA  ADD CODE FOR ZERO APR NET PAY
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
011317* 011317  IR2017011000002  PEMA  REMOVE 'CT' HARDCODING
071222* 071222  IR2022071200001  PEMA  Ref Rea of "R" s/b DCC only
042203******************************************************************
00008
00009      MOVE ZERO                   TO  CP-RETURN-CODE
00010                                      CP-CALC-REFUND.
00011      MOVE SPACE                  TO  CP-REFUND-TYPE-USED.
00012
00013  0001-GET-CANCEL-TYPE.
00014      IF CP-CERT-EFF-DT = CP-VALUATION-DT
00015         MOVE CP-ORIGINAL-PREMIUM TO CP-CALC-REFUND
00016         GO TO 1999-CALC-REF-AMT-X.
00017
071222     IF (CP-CANCEL-REASON = 'R')
071222        and (cp-company-id = 'DCC')
              IF CP-CALC-CLP
                 MOVE CP-ORIGINAL-PREMIUM
                                       TO CP-CALC-REFUND
              ELSE
                 COMPUTE CP-CALC-REFUND = CP-ORIGINAL-PREMIUM -
                    CP-DDF-ADMIN-FEES
              END-IF
              MOVE 'R'                 TO CP-REFUND-TYPE-USED
              GO TO 1999-CALC-REF-AMT-X
           END-IF
00018      IF CP-REMAINING-TERM = ZERO   OR
00019         CP-REMAINING-TERM NEGATIVE
00020           MOVE ZERO TO CP-CALC-REFUND
00021           GO TO 1999-CALC-REF-AMT-X.
00022
00023      MOVE CP-CERT-EFF-DT     TO DC-BIN-DATE-1.
00024      MOVE SPACE              TO DC-OPTION-CODE.
00025      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
00026
00027      IF DATE-CONVERSION-ERROR
00028          MOVE '2' TO CP-RETURN-CODE
00029          GO TO 1999-CALC-REF-AMT-X.
00030
00031      IF CP-AH
00032         GO TO 1030-GET-AH-REFUND.
00033
00034 ********************    LIFE REFUNDS    **************************
00035
00036      IF CP-TERM-OR-EXT-DAYS NOT NUMERIC
00037          MOVE +0                TO CP-TERM-OR-EXT-DAYS.
00038
00039      IF CP-LOAN-TERM NOT NUMERIC
00040          MOVE +0                TO CP-LOAN-TERM.
00041
00042      COMPUTE PR-FACTOR = CP-REMAINING-TERM / CP-ORIGINAL-TERM.
00043
00044      COMPUTE R78-FACTOR =
00045             (CP-REMAINING-TERM * (CP-REMAINING-TERM + 1)) /
00046               (CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM + 1)).
CIDVAO************************************************************
CIDVAO***       ADDED IF CONDITION FOR TESTING PURPOSES.       ***
CIDVAO***---------------------------------------------------------
CIDVAO     IF CP-TRUNCATED-LIFE
CIDVAO        MOVE  CP-LOAN-TERM         TO  WS-RATE-TERM
CIDVAO     ELSE
CIDVAO        MOVE  CP-ORIGINAL-TERM     TO  WS-RATE-TERM.
CIDVAO***
CIDVAO************************************************************
00048 **** THE FOLLOWING IF STATEMENT FOR NCL ONLY SHOULD DO THE
00049 **** FOLLOWING.
00050 ****       FOR NC. LIFE..
00051 ****  LEVEL LIFE COVERAGES   --- REFUND AS PRO RATA.
00052 ****  TRUNCATED NET COVERAGE --- TERM > 60 USE RULE OF ANTIC.
00053 ****                             TERM <= 60 USE SUM OF DIGITS.
00054 ****  DECREASING COVERAGE    --- EFF DATE > 093081 AND
00055 ****                             TERM > 60 USE RULE OF ANTIC
00056 ****                             OTHERWISE USE RULE OF 78.
00057 ****
00058 ****
00059
00060      IF (CP-COMPANY-ID = 'NCL') AND
00061         (CP-STATE-STD-ABBRV = 'NC')
00062         IF CP-LEVEL-LIFE
00063            GO TO 1023-REF-PR
00064         ELSE
00065         IF CP-EARN-AS-NET-PAY AND
00066            CP-TRUNCATED-LIFE
00067            IF CP-ORIGINAL-TERM GREATER THAN 60
00068               GO TO 1021-ANTICIPATION-REF
00069            ELSE
00070               GO TO 1026-REF-R78
00071         ELSE
00072         IF CP-REDUCING-LIFE
00073            IF (DC-GREG-DATE-CYMD  GREATER 19810930 AND
00074               CP-ORIGINAL-TERM GREATER THAN 60)
00075               GO TO 1021-ANTICIPATION-REF
00076            ELSE
00077               GO TO 1075-SUM-OF-DIGITS.
00078
CIDMOD****
CIDMOD**** THE FOLLOWING IF STATEMENT FOR CID ONLY SHOULD DO THE
CIDMOD**** FOLLOWING.
CIDMOD****       FOR OH. LIFE..
CIDMOD****  LEVEL LIFE COVERAGES   --- REFUND AS PRO RATA.
CIDMOD****  NET/NET TRUNCATED      --- TERM > 60 USE RULE OF ANTIC.
CIDMOD****  GROSS DECREASING       --- RULE OF 78
CIDMOD****
CIDMOD****
CIDMOD
CIDMOD     IF (CP-COMPANY-ID = 'CID') AND
CIDMOD        (CP-STATE-STD-ABBRV = 'OH') AND
CIDMOD        (CP-CLASS-CODE NOT = 'L ')
CIDMOD        IF CP-LEVEL-LIFE
CIDMOD           GO TO 1023-REF-PR
CIDMOD        ELSE
CIDMOD           IF (CP-ORIGINAL-TERM > 60) OR
CIDMOD              (CP-EARN-AS-NET-PAY)
CIDMOD              GO TO 1028-NET-PAY-REFUND
CIDMOD           ELSE
CIDMOD              GO TO 1026-REF-R78
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
00078
00079 **** THE FOLLOWING IF STATEMENT FOR NCL ONLY SHOULD DO THE
00080 **** FOLLOWING.
00081 ****       FOR VA. LIFE..
00082 ****  LEVEL LIFE COVERAGES   --- REFUND AS PRO RATA.
00083 ****           NET COVERAGES --- USE RULE OF ANTIC.
00084 ****  DECREASING COVERAGE    --- EFF DATE < 010193 USE R78.
00085 ****                         --- EFF DATE > 123194 USE ANTIC.
00086 ****                         --- EFF DATE BETWEEN 010193 AND 12319
00087 ****                             WITH TERM > 61 USE RULE OF ANTIC
00088 ****                             OTHERWISE USE RULE OF 78.
00089 ****
00090 ****    NOTE... PER NCL WE SHOULD NOT INCLUDE THEM IN ANY
00091 ****            OF THE SYSTEM DEFAULT STATUTORY REFUND RULES.
00092 ****            NCL WILL SUPPLY US WITH THE STATUTORY RULES THEY
00093 ****            WILL FOLLOW.
00094 ****
00095 ****
00096 ****
00097
00098      IF (CP-COMPANY-ID = 'NCL') AND
00099         (CP-STATE-STD-ABBRV = 'VA')
00100         IF CP-LEVEL-LIFE
00101            GO TO 1023-REF-PR
00102         ELSE
00103         IF CP-EARN-AS-NET-PAY
00104            GO TO 1021-ANTICIPATION-REF
00105         ELSE
00106         IF CP-REDUCING-LIFE
00107            IF DC-GREG-DATE-CYMD LESS THAN 19930101
00108               GO TO 1026-REF-R78
00109            ELSE
00110            IF DC-GREG-DATE-CYMD GREATER THAN 19941231
00111               GO TO 1021-ANTICIPATION-REF
00112            ELSE
00113            IF CP-ORIGINAL-TERM GREATER THAN 61
00114               GO TO 1021-ANTICIPATION-REF
00115            ELSE
00116               GO TO 1026-REF-R78.
00117
042203     IF (CP-COMPANY-ID = 'CID')
042203        AND (CP-STATE-STD-ABBRV = 'IN')
042203        AND (DC-GREG-DATE-CYMD < 20030401)
042203        IF CP-REDUCING-LIFE
042203           GO TO 1026-REF-R78
042203        ELSE
042203           IF CP-LEVEL-LIFE
042203              GO TO 1023-REF-PR
042203           END-IF
042203        END-IF
042203     END-IF
042203
072403     IF (CP-COMPANY-ID = 'CID')
072403        AND (CP-STATE-STD-ABBRV = 'FL')
072403        AND (DC-GREG-DATE-CYMD > 20030630)
072403        IF CP-RATE-AS-NET-PAY
072403           GO TO 1028-NET-PAY-REFUND
072403        END-IF
072403     END-IF
072403
071910     IF (CP-COMPANY-ID = 'CID')
071910        AND (CP-STATE-STD-ABBRV = 'MN')
071910        AND (CP-EARN-AS-NET-PAY)
071910        AND (CP-RATE-AS-REG-BALLOON)
071910        AND (DC-GREG-DATE-CYMD > 20091231)
071910        GO TO 1028-NET-PAY-REFUND
071910     END-IF
110609     IF (CP-COMPANY-ID = 'CID')
110609        AND (CP-STATE-STD-ABBRV = 'MN')
110609        AND (DC-GREG-DATE-CYMD > 20091231)
110609        AND (CP-RATE-AS-NET-PAY)
110609        GO TO 1021-ANTICIPATION-REF
110609     END-IF
072403     IF (CP-COMPANY-ID = 'CID')
072403        AND (CP-STATE-STD-ABBRV = 'MO')
072403        AND (DC-GREG-DATE-CYMD > 20030630)
072403        AND (CP-LOAN-TERM < +121)
072403        IF CP-RATE-AS-NET-PAY
072403           GO TO 1028-NET-PAY-REFUND
072403        END-IF
072403     END-IF
072403
010304     IF (CP-COMPANY-ID = 'CID')
010304        AND (CP-STATE-STD-ABBRV = 'ND')
010304        AND (DC-GREG-DATE-CYMD > 20011231)
042203        AND (CP-REDUCING-LIFE)
010304        GO TO 1021-ANTICIPATION-REF
010304     END-IF
072403
00118      IF CP-CRITICAL-PERIOD
00119         IF CP-COMPANY-ID = 'NCL'
00120            NEXT SENTENCE
00121         ELSE
00122            GO TO 1022-REF-CRITICAL-PERIOD-LF.
00123
00124      IF CP-TERM-IS-DAYS  AND
00125         CP-TERM-OR-EXT-DAYS GREATER ZERO
00126          GO TO 1024-REF-PR-DAYS.
00127
CIDMOD     IF CP-STATE-STD-ABBRV = 'NM'
CIDMOD       IF CP-REDUCING-LIFE
CIDMOD*        IF DC-GREG-DATE-1-YMD LESS THAN '980901'
CIDMOD         IF DC-GREG-DATE-CYMD LESS THAN 19980901
CIDMOD             GO TO 1026-REF-R78.
CIDMOD
00128      IF CP-EARN-ANTICIPATION
00129          GO TO 1021-ANTICIPATION-REF.
00130
00131      IF CP-STATE-STD-ABBRV = 'ME'
00132          IF CP-COMPANY-ID = 'NCL' OR 'CVL'
00133             NEXT SENTENCE
00134          ELSE
00135             IF CP-REDUCING-LIFE
00136                GO TO 1021-ANTICIPATION-REF.
00137
CIDMOD     IF CP-COMPANY-ID EQUAL 'CID' OR 'CSO'
CIDMOD        IF CP-STATE-STD-ABBRV = 'TX'
CIDMOD            GO TO SKIP-CSO-TEXAS-LIFE-PROCESSING
CIDMOD        END-IF
CIDMOD     END-IF.
CIDMOD
00138      IF CP-STATE-STD-ABBRV = 'TX'
00139        IF CP-COMPANY-ID EQUAL 'HER' OR 'NCL'
00140           NEXT SENTENCE
00141        END-IF
00142         IF DC-GREG-DATE-CYMD GREATER 19920629  AND
00143            CP-ORIGINAL-TERM NOT GREATER THAN 120
00144            GO TO 1021-ANTICIPATION-REF.
00145
CIDMOD SKIP-CSO-TEXAS-LIFE-PROCESSING.
CIDMOD
00146      IF CP-EARN-BY-PRORATA
00147          GO TO 1023-REF-PR.
00148
00149      IF CP-EARN-AS-MEAN
00150          GO TO 1025-REF-MEAN.
00151
00152      IF CP-EARN-AS-TEXAS
00153          GO TO 1027-TEXAS-REFUND.
00154
00155      IF CP-EARN-AS-NET-PAY
00156          GO TO 1028-NET-PAY-REFUND.
00157
00158      IF CP-LIFE-OVERRIDE-CODE NOT = 'L'
00159          GO TO 1000-NO-NET-PAY.
00160
CIDMOD     IF  CP-COMPANY-ID IS EQUAL TO 'CID' OR 'CSO'
CIDMOD       IF CP-STATE-STD-ABBRV = 'NC'
CIDMOD         IF CP-REDUCING-LIFE
CIDMOD           IF CP-ORIGINAL-TERM GREATER 60  AND
CIDMOD              DC-GREG-DATE-CYMD GREATER 19810930
CIDMOD                GO TO 1028-NET-PAY-REFUND
CIDMOD           ELSE
CIDMOD              GO TO 1026-REF-R78.
CIDMOD
CIDMOD
00161      IF CP-STATE-STD-ABBRV = 'NC'
00162        IF CP-COMPANY-ID EQUAL 'NCL'
00163           NEXT SENTENCE
00164        END-IF
00165        IF CP-REDUCING-LIFE
00166          IF CP-ORIGINAL-TERM GREATER 60  AND
00167             DC-GREG-DATE-CYMD GREATER 19810930
00168              GO TO 1021-ANTICIPATION-REF
00169          ELSE
00170              GO TO 1026-REF-R78.
00171
00172      IF CP-STATE-STD-ABBRV = 'UT'
00173        IF CP-COMPANY-ID EQUAL 'NCL'
00174           NEXT SENTENCE
00175        END-IF
00176          IF CP-ORIGINAL-TERM GREATER 62         AND
00177             CP-LOAN-APR GREATER ZERO            AND
00178             DC-GREG-DATE-CYMD GREATER 19810831 AND
00179             DC-GREG-DATE-CYMD LESS    19830901
00180              GO TO 1028-NET-PAY-REFUND
00181          ELSE
00182              GO TO 1026-REF-R78.
00183
00184      IF CP-STATE-STD-ABBRV = 'OH'
00185        IF CP-COMPANY-ID EQUAL 'NCL' OR 'HER'
00186           NEXT SENTENCE
00187        END-IF
00188            IF CP-ORIGINAL-TERM GREATER 60         AND
CIDMOD              CP-CLASS-CODE NOT = 'L '            AND
00189               CP-LOAN-APR GREATER ZERO            AND
00190               DC-GREG-DATE-CYMD GREATER 19831031
00191               GO TO 1028-NET-PAY-REFUND
00192            ELSE
00193               GO TO 1026-REF-R78.
CIDMOD
CIDMOD     IF CP-COMPANY-ID EQUAL 'CID' OR 'CSO'
CIDMOD       IF CP-STATE-STD-ABBRV = 'MT'
CIDMOD         GO TO SKIP-CSO-MT-LF-PROCESSING.
CIDMOD
00194
00195      IF CP-STATE-STD-ABBRV = 'MT'
00196        IF CP-COMPANY-ID EQUAL 'NCL'
00197           NEXT SENTENCE
00198        END-IF
00199          IF CP-ORIGINAL-TERM GREATER 61         AND
00200             CP-LOAN-APR GREATER ZERO            AND
00201             DC-GREG-DATE-CYMD GREATER 19830318
00202              GO TO 1028-NET-PAY-REFUND
00203          ELSE
00204              GO TO 1026-REF-R78.
00205
CIDMOD SKIP-CSO-MT-LF-PROCESSING.
CIDMOD
00206      IF CP-STATE-STD-ABBRV = 'RI'
00207        IF CP-COMPANY-ID EQUAL 'NCL'
00208           NEXT SENTENCE
00209        END-IF
00210          IF CP-ORIGINAL-TERM GREATER 60         AND
00211             CP-LOAN-APR GREATER ZERO            AND
00212             DC-GREG-DATE-CYMD GREATER 19831231
00213              GO TO 1028-NET-PAY-REFUND
00214          ELSE
00215              GO TO 1026-REF-R78.
00216
00217
00218  1000-NO-NET-PAY.
00219
00220      IF CP-STATE-STD-ABBRV = 'WY'
00221        IF CP-COMPANY-ID EQUAL 'NCL'
00222           NEXT SENTENCE
00223        END-IF
00224         IF CP-COMPANY-ID = 'TMS'
00225            GO TO 1026-REF-R78
00226         ELSE
00227            GO TO 1023-REF-PR.
00228
00229      IF CP-EARN-BY-R78
00230          GO TO 1026-REF-R78.
00231
00232      IF CP-EARN-AS-SUM-OF-DIGITS
00233          GO TO 1075-SUM-OF-DIGITS.
00234
00235      IF CP-LEVEL-LIFE
00236          GO TO 1023-REF-PR
00237      ELSE
00238          GO TO 1026-REF-R78.
00239
00240 *-----------------------------------------------------------------
00241  1021-ANTICIPATION-REF.
00242      MOVE '6'                  TO CP-REFUND-TYPE-USED.
00243
00244      MOVE CP-LOAN-TERM         TO WS-SAVE-LOAN-TERM.
           IF CP-LOAN-TERM = ZEROS
              MOVE CP-ORIGINAL-TERM TO CP-LOAN-TERM
           END-IF
00245      COMPUTE CP-LOAN-TERM = CP-LOAN-TERM - CP-ORIGINAL-TERM +
00246                             CP-REMAINING-TERM.
00247
00248      MOVE CP-ORIGINAL-BENEFIT  TO WS-SAVE-BENEFIT.
00249      MOVE CP-ORIGINAL-TERM     TO WS-SAVE-TERM.
00250      MOVE CP-EARNING-METHOD    TO WS-SAVE-EARN-METHOD.
051713     move cp-term-or-ext-days  to ws-save-ext-days
00251
00252      MOVE CP-REMAINING-BENEFIT TO CP-ORIGINAL-BENEFIT.
00253      MOVE CP-REMAINING-TERM    TO CP-ORIGINAL-TERM.
00254      MOVE CP-RATING-METHOD     TO CP-EARNING-METHOD.
00255
00256      IF CP-REMAINING-TERM = WS-SAVE-TERM
00257          MOVE 'N'              TO CP-ROA-REFUND
00258      ELSE
00259          MOVE 'Y'              TO CP-ROA-REFUND.
00260
00261 ******    TO COMPUTE PREMIUM AT REFUND DATE
00262      PERFORM 2500-GET-RATE.
00263
00264      MOVE 'N'                  TO CP-ROA-REFUND.
00265      MOVE WS-SAVE-BENEFIT      TO CP-ORIGINAL-BENEFIT.
00266      MOVE WS-SAVE-TERM         TO CP-ORIGINAL-TERM.
00267      MOVE WS-SAVE-LOAN-TERM    TO CP-LOAN-TERM.
051713     move ws-save-ext-days     to cp-term-or-ext-days
00268
00269      IF CP-PREMIUM-RATE = ZEROS
00270         MOVE '7'                 TO CP-RETURN-CODE
00271         MOVE WS-SAVE-EARN-METHOD TO CP-EARNING-METHOD
00272         GO TO 1999-CALC-REF-AMT-X.
00273
00274      MOVE CP-CALC-PREMIUM         TO WS-REMAIN-PREM.
00275
00276 ******    TO COMPUTE PREMIUM AT ORIGINAL ISSUE DATE
00277      PERFORM 2500-GET-RATE.
00278
00279      MOVE WS-SAVE-EARN-METHOD     TO CP-EARNING-METHOD.
00280
00281      IF CP-PREMIUM-RATE = ZEROS
00282         MOVE '7'            TO CP-RETURN-CODE
00283         GO TO 1999-CALC-REF-AMT-X.
00284
00285      MOVE CP-CALC-PREMIUM         TO WS-ORIG-PREM.
00286
00287      IF  WS-ORIG-PREM = ZEROS
00288          MOVE ZEROS TO WS-NP-REF-FACTOR
00289         ELSE
00290          COMPUTE WS-NP-REF-FACTOR ROUNDED = WS-REMAIN-PREM /
00291                                             WS-ORIG-PREM.
00292
00293      COMPUTE CP-CALC-REFUND ROUNDED = WS-NP-REF-FACTOR *
00294                                         CP-ORIGINAL-PREMIUM.
00295
00296      GO TO 1999-CALC-REF-AMT-X.
00297
00298 *---------------------------------------------------------------
00299  1022-REF-CRITICAL-PERIOD-LF.
00300      GO TO 1023-REF-PR.
00301
00302 *---------------------------------------------------------------
00303  1023-REF-PR.
00304      MOVE '2'               TO CP-REFUND-TYPE-USED.
00305      COMPUTE CP-CALC-REFUND ROUNDED =
00306             CP-ORIGINAL-PREMIUM * PR-FACTOR.
00307
00308      GO TO 1999-CALC-REF-AMT-X.
00309
00310 *---------------------------------------------------------------
00311  1024-REF-PR-DAYS.
00312      MOVE CP-CERT-EFF-DT     TO DC-BIN-DATE-1.
00313      MOVE CP-VALUATION-DT    TO DC-BIN-DATE-2.
00314      MOVE '1'                TO DC-OPTION-CODE.
00315      PERFORM 9100-CONVERT-DATE THRU 9100-EXIT.
00316
00317      IF DATE-CONVERSION-ERROR
00318          MOVE '2' TO CP-RETURN-CODE
00319          GO TO 1999-CALC-REF-AMT-X.
00320
00321      COMPUTE CP-CALC-REFUND ROUNDED =
00322          (CP-ORIGINAL-PREMIUM / CP-TERM-OR-EXT-DAYS) *
00323                  (CP-TERM-OR-EXT-DAYS - DC-ELAPSED-DAYS).
00324
00325      GO TO 1999-CALC-REF-AMT-X.
00326
00327 *---------------------------------------------------------------
00328  1025-REF-MEAN.
00329      MOVE '8'               TO CP-REFUND-TYPE-USED.
00330      COMPUTE CP-CALC-REFUND ROUNDED =
00331           ((CP-ORIGINAL-PREMIUM * PR-FACTOR) +
00332            (CP-ORIGINAL-PREMIUM * R78-FACTOR)) * +.5.
00333
00334      GO TO 1999-CALC-REF-AMT-X.
00335
00336 *---------------------------------------------------------------
00337  1026-REF-R78.
00338      MOVE '1'               TO CP-REFUND-TYPE-USED.
00339 *    IF CP-TERM-TIMES-TERM
00340 *        COMPUTE R78-FACTOR =
00341 *            (CP-REMAINING-TERM * CP-REMAINING-TERM) /
00342 *               (CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM + 1)).
00343 *
00344      COMPUTE CP-CALC-REFUND ROUNDED =
00345         CP-ORIGINAL-PREMIUM * R78-FACTOR.
00346
00347      GO TO 1999-CALC-REF-AMT-X.
00348
00349 *---------------------------------------------------------------
00350  1027-TEXAS-REFUND.
00351      MOVE '4'               TO CP-REFUND-TYPE-USED.
00352      IF CP-PAY-FREQUENCY = ZERO
00353         MOVE ZERO TO CP-CALC-REFUND
00354         MOVE '5'  TO CP-RETURN-CODE
00355         GO TO 1999-CALC-REF-AMT-X.
00356
00357      COMPUTE TEX-FACT-4 = (CP-ORIGINAL-TERM * CP-ORIGINAL-TERM) +
00358          (CP-PAY-FREQUENCY * CP-ORIGINAL-TERM).
00359
00360      DIVIDE CP-REMAINING-TERM BY CP-PAY-FREQUENCY
00361          GIVING TEX-FACT-5
00362              REMAINDER TEX-FACT-6.
00363
00364      COMPUTE TEX-FACT-5 = TEX-FACT-5 * CP-PAY-FREQUENCY.
00365
00366      COMPUTE TEX-FACT-7 = (TEX-FACT-5 * TEX-FACT-5) +
00367          (TEX-FACT-5 * CP-PAY-FREQUENCY) +
00368          (2 * (TEX-FACT-6 * (TEX-FACT-5 + CP-PAY-FREQUENCY))).
00369
00370      COMPUTE TEX-FACT-8 ROUNDED = TEX-FACT-7 / TEX-FACT-4.
00371
00372      COMPUTE CP-CALC-REFUND ROUNDED = CP-ORIGINAL-PREMIUM
00373                                         * TEX-FACT-8.
00374
00375      GO TO 1999-CALC-REF-AMT-X.
00376
00377 *---------------------------------------------------------------
00378  1028-NET-PAY-REFUND.
00379      MOVE '5'               TO CP-REFUND-TYPE-USED.
102808     IF CP-LOAN-APR = ZERO
051713        move '1'                 to cp-refund-type-used
051713        compute cp-calc-refund rounded =
051713           cp-original-premium * r78-factor
051713        go to 1999-calc-ref-amt-x
051713     end-if
051713*       MOVE ZERO TO CP-CALC-REFUND
051713*       MOVE '1'  TO CP-RETURN-CODE
051713*       GO TO 1999-CALC-REF-AMT-X.
00384
00385      MOVE CP-LOAN-APR           TO N-P-APR.
00386      MOVE CP-ORIGINAL-TERM      TO N-P-ORIG  N-P-LOAN.
00387      MOVE CP-REMAINING-TERM     TO N-P-REM.
00388
00389      IF CP-TRUNCATED-LIFE
00390         MOVE CP-LOAN-TERM     TO N-P-LOAN.
00391
00392      MOVE 'R'                   TO N-P-OPT.
00393
00394      PERFORM 10000-NET-TERM THRU 99999-EXIT.
00395
00396      COMPUTE CP-CALC-REFUND ROUNDED = N-P-FACTOR *
00397                                         CP-ORIGINAL-PREMIUM.
00398
00400  1029-UTAH-REFUND.
CIDMOD*
CIDMOD*---------------------------------------------------------------
CIDMOD* FOLLOWING IS FOR TRISH - 10/26/98  (FOR UTAH ONLY).
CIDMOD*---------------------------------------------------------------
CIDMOD*
CIDMOD     GO TO 1999-CALC-REF-AMT-X.
CIDMOD*
CIDMOD*---------------------------------------------------------------
00401      IF (CP-STATE-STD-ABBRV NOT = 'UT')
00402                       OR
00403         (CP-COMPANY-ID EQUAL 'NCL')
00404          GO TO 1999-CALC-REF-AMT-X.
00405      MOVE '3'               TO CP-REFUND-TYPE-USED.
00406
00407      IF (CP-ORIGINAL-TERM - CP-REMAINING-TERM)
00408         GREATER ZERO AND LESS 13
00409          COMPUTE CP-CALC-REFUND = CP-CALC-REFUND -
00410                  (CP-CALC-REFUND * .05).
00411
00412      IF (CP-ORIGINAL-TERM - CP-REMAINING-TERM)
00413         GREATER 12 AND LESS 25
00414          COMPUTE CP-CALC-REFUND = CP-CALC-REFUND -
00415                  (CP-CALC-REFUND * .025).
00416
00417      GO TO 1999-CALC-REF-AMT-X.
00418 *---------------------------------------------------------------
00419      EJECT
00420 *---------------------------------------------------------------
00421  1030-GET-AH-REFUND.
00422      COMPUTE PR-FACTOR = CP-REMAINING-TERM / CP-ORIGINAL-TERM.
00423
00424      COMPUTE R78-FACTOR =
00425             ((CP-REMAINING-TERM * (CP-REMAINING-TERM + 1)) /
00426                 (CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM + 1))).
00427
00428 *    IF CP-TERM-TIMES-TERM
00429 *        COMPUTE R78-FACTOR =
00430 *            (CP-REMAINING-TERM * CP-REMAINING-TERM) /
00431 *            (CP-ORIGINAL-TERM * (CP-ORIGINAL-TERM + 1)).
00432 *
00433
00434 **** THE FOLLOWING IF STATEMENT FOR NCL ONLY SHOULD DO THE
00435 **** FOLLOWING.
00436 ****       FOR VA. DISABILITY
00437 ****                         --- EFF DATE > 123192 AND
00438 ****                             TERM > 61 USE RULE OF ANTIC
00439 ****                             OTHERWISE USE RULE OF 78.
00440 ****
00441 ****    NOTE... PER NCL WE SHOULD NOT INCLUDE THEM IN ANY
00442 ****            OF THE SYSTEM DEFAULT STATUTORY REFUND RULES.
00443 ****            NCL WILL SUPPLY US WITH THE STATUTORY RULES THEY
00444 ****            WILL FOLLOW.
00445 ****
00446 ****
00447 ****
032612     IF CP-COMPANY-ID = 'NCL' or 'AHL'
00449         IF CP-STATE-STD-ABBRV = 'VA'
00450            IF (DC-GREG-DATE-CYMD GREATER 19921231) AND
00451               (CP-ORIGINAL-TERM GREATER THAN 61)
00452                MOVE '6'        TO CP-EARNING-METHOD
00453                GO TO 1060-ANTICIPATION-REF
00454             ELSE
00455                MOVE '1'        TO CP-EARNING-METHOD
00456                GO TO 1050-R78-REF.
00457
00458      IF CP-STATE-STD-ABBRV = 'WY'
00459         IF CP-COMPANY-ID = 'TMS'
00460            GO TO 1050-R78-REF.
00461
032612     IF CP-COMPANY-ID = 'NCL' OR 'WDS' OR 'CID' OR 'DCC'
020816                     OR 'AHL' OR 'VPP'
00463          NEXT SENTENCE
00464      ELSE
00465          IF CP-CRITICAL-PERIOD
00466              GO TO 1035-REF-AH-CRITICAL-PERIOD.
00467
033104*      THE FOLLOWING HAPPENS WHEN THE REFUND METHOD FOR THE
033104*    BENEFIT CODE IS A NON REFUNDABLE METHOD
033104*    I AM CHECKING THE TERM AND REM TERM BECAUSE OF THE 30
033104*    DAY FREE LOOK PERIOD. ALSO, 55 AND 56 ARE THE BIU CODES
033104     IF (CP-GAP-NON-REFUNDABLE)
033104        IF (CP-ORIGINAL-TERM NOT = CP-REMAINING-TERM)
051909           OR (CP-BENEFIT-CD = '55' OR '56')
033014           MOVE ZEROS            TO CP-CALC-REFUND
033104           MOVE 'G'              TO CP-REFUND-TYPE-USED
033104           GO TO 1999-CALC-REF-AMT-X
033104        END-IF
033104     END-IF
042904     IF CP-GAP-ACTUARIAL
042904         GO TO 1070-NET-PAY-REFUND
042904     END-IF
110609     IF (CP-COMPANY-ID = 'CID')
110609        AND (CP-STATE-STD-ABBRV = 'MN')
110609        AND (DC-GREG-DATE-CYMD > 20091231)
110609        IF (CP-CRITICAL-PERIOD)
110609           GO TO 1040-PR-REF
110609        ELSE
110609           GO TO 1055-MEAN-REF
110609     END-IF
020816     IF CP-COMPANY-ID EQUAL 'CID' OR 'DCC' OR 'VPP'
CIDMOD        IF CP-STATE-STD-ABBRV = 'NC'
CIDMOD           GO TO SKIP-CSO-NC-AH-PROCESSING.
CIDMOD
042203     IF (CP-COMPANY-ID = 'CID')
042203        AND (CP-STATE-STD-ABBRV = 'IN')
042203        AND (DC-GREG-DATE-CYMD < 20030401)
042203        GO TO 1026-REF-R78
042203     END-IF
042203
072403     IF (CP-COMPANY-ID = 'CID')
072403        AND (CP-STATE-STD-ABBRV = 'FL')
072403        AND (DC-GREG-DATE-CYMD > 20030630)
072403        AND (CP-CRITICAL-PERIOD)
072403        GO TO 1040-PR-REF
072403     END-IF
072403
072403     IF (CP-COMPANY-ID = 'CID')
072403        AND (CP-STATE-STD-ABBRV = 'MO')
072403        AND (DC-GREG-DATE-CYMD > 20030630)
072403        AND (CP-CRITICAL-PERIOD)
072403        GO TO 1040-PR-REF
072403     END-IF
072403
010304     IF (CP-COMPANY-ID = 'CID')
010304        AND (CP-STATE-STD-ABBRV = 'ND')
010304        AND (CP-CRITICAL-PERIOD)
010304        AND (DC-GREG-DATE-CYMD > 20011231)
010304        GO TO 1040-PR-REF
010304     END-IF
010304
010304     IF (CP-COMPANY-ID = 'CID')
010304        AND (CP-STATE-STD-ABBRV = 'ND')
010304        AND (DC-GREG-DATE-CYMD > 20011231)
010304        GO TO 1060-ANTICIPATION-REF
010304     END-IF
010304
00468      IF CP-STATE-STD-ABBRV = 'NC'
00469        IF CP-COMPANY-ID = 'NCL' OR 'NCB'
00470           NEXT SENTENCE
00471        END-IF
00472          IF DC-GREG-DATE-CYMD GREATER 19810930
00473             IF CP-EARN-ANTICIPATION
00474                 GO TO 1060-ANTICIPATION-REF
00475             ELSE
00476                 GO TO 1055-MEAN-REF.
00477
CIDMOD SKIP-CSO-NC-AH-PROCESSING.
CIDMOD
00478      IF CP-STATE-STD-ABBRV = 'NM'
020816       IF CP-COMPANY-ID = 'NCL' OR 'DCC' OR 'AHL' OR 'VPP'
00480           NEXT SENTENCE
00481        END-IF
00482          IF DC-GREG-DATE-CYMD GREATER THAN 19871231
00483             IF CP-COMPANY-ID = 'MON'
00484               MOVE '6'             TO CP-EARNING-METHOD
00485               GO TO 1060-ANTICIPATION-REF
00486              ELSE
00487               GO TO 1055-MEAN-REF.
00488
011317*    IF CP-STATE-STD-ABBRV = 'CT'
011317*      IF CP-COMPANY-ID EQUAL 'NCL' OR 'DCC' OR 'VPP'
011317*         NEXT SENTENCE
011317*      END-IF
011317*         MOVE '6'              TO CP-EARNING-METHOD
011317*         GO TO 1060-ANTICIPATION-REF.
00495
00496      IF CP-STATE-STD-ABBRV = 'ME'
020816       IF CP-COMPANY-ID = 'NCL' OR 'CVL' OR 'DCC' OR 'VPP'
00498           NEXT SENTENCE
00499        END-IF
00500            IF CP-EARN-BY-PRORATA
00501               GO TO 1040-PR-REF
00502            ELSE
00503               MOVE '6'           TO CP-EARNING-METHOD
00504               GO TO 1060-ANTICIPATION-REF.
00505
CIDMOD
020816     IF CP-COMPANY-ID = 'CID' OR 'DCC' OR 'VPP'
CIDMOD        IF CP-STATE-STD-ABBRV = 'TX' OR 'VA'
CIDMOD            GO TO SKIP-CSO-TX-VA-AH-PROCESSING.
CIDMOD
00506      IF CP-STATE-STD-ABBRV = 'TX'
00507        IF CP-COMPANY-ID = 'NCL' OR 'HER'
00508           NEXT SENTENCE
00509        END-IF
00510         IF DC-GREG-DATE-CYMD GREATER 19920629  AND
00511            CP-ORIGINAL-TERM NOT GREATER THAN 120
00512            MOVE '6'              TO CP-EARNING-METHOD
00513            GO TO 1060-ANTICIPATION-REF.
00514
00515      IF CP-STATE-STD-ABBRV = 'VA'
00516        IF CP-COMPANY-ID EQUAL 'NCL'
00517           NEXT SENTENCE
00518        END-IF
00519        IF CP-COMPANY-ID = 'NCB'
00520            MOVE '6'                TO CP-EARNING-METHOD
00521            GO TO 1060-ANTICIPATION-REF
00522        ELSE
00523            IF DC-GREG-DATE-CYMD GREATER 19921231
00524                IF CP-ORIGINAL-TERM GREATER THAN 61
00525                    MOVE '6'        TO CP-EARNING-METHOD
00526                    GO TO 1060-ANTICIPATION-REF
00527                ELSE
00528                    MOVE '1'        TO CP-EARNING-METHOD
00529                    GO TO 1050-R78-REF.
00530
CIDMOD SKIP-CSO-TX-VA-AH-PROCESSING.
CIDMOD
00531      IF CP-EARN-BY-PRORATA
00532          GO TO 1040-PR-REF.
00533
00534      IF CP-EARN-BY-R78
00535          GO TO 1050-R78-REF.
00536
00537      IF CP-EARN-AS-MEAN
00538         GO TO 1055-MEAN-REF.
00539
00540      IF CP-EARN-AS-SUM-OF-DIGITS
00541          GO TO 1075-SUM-OF-DIGITS.
           IF CP-DCC-SPP-DDF
              GO TO 1080-DCC-DDF-SPECIAL
           END-IF
00543      IF CP-EARN-AS-CALIF
00544          GO TO 1060-ANTICIPATION-REF.
00545
00546      IF CP-EARN-ANTICIPATION
00547          GO TO 1060-ANTICIPATION-REF.
00548
00549      IF CP-EARN-AS-NET-PAY
00550          GO TO 1070-NET-PAY-REFUND.
00551
00552      GO TO 1050-R78-REF.
00553
00554 *---------------------------------------------------------------
00555  1035-REF-AH-CRITICAL-PERIOD.
00556      IF CP-REMAINING-TERM GREATER +6
00557         COMPUTE CP-CALC-REFUND ROUNDED =
00558           CP-ORIGINAL-PREMIUM * (((CP-REMAINING-TERM * +2) - +5) /
00559                        ((CP-ORIGINAL-TERM * +2) - + 5))
00560       ELSE
00561         COMPUTE CP-CALC-REFUND ROUNDED =
00562           CP-ORIGINAL-PREMIUM * ((CP-REMAINING-TERM *
00563                                            CP-REMAINING-TERM + 1)
00564                               /  (+12 * CP-ORIGINAL-TERM) - +30).
00565
00566      IF CP-CALC-REFUND LESS ZERO
00567          MOVE ZERO TO CP-CALC-REFUND.
00568
00569      GO TO 1999-CALC-REF-AMT-X.
00570
00571 *---------------------------------------------------------------
00572  1040-PR-REF.
00573      MOVE '2'               TO CP-REFUND-TYPE-USED.
00574      COMPUTE CP-CALC-REFUND = CP-ORIGINAL-PREMIUM * PR-FACTOR.
00575
00576      GO TO 1999-CALC-REF-AMT-X.
00577
00578 *---------------------------------------------------------------
00579  1050-R78-REF.
00580      MOVE '1'               TO CP-REFUND-TYPE-USED.
00581      COMPUTE CP-CALC-REFUND ROUNDED =
00582            CP-ORIGINAL-PREMIUM * R78-FACTOR.
00583
00584      GO TO 1999-CALC-REF-AMT-X.
00585
00586 *---------------------------------------------------------------
00587  1055-MEAN-REF.
00588      MOVE '8'               TO CP-REFUND-TYPE-USED.
00589      COMPUTE CP-CALC-REFUND ROUNDED =
00590          ((CP-ORIGINAL-PREMIUM * R78-FACTOR) +
00591           (CP-ORIGINAL-PREMIUM * PR-FACTOR)) * .5.
00592
00593      GO TO 1999-CALC-REF-AMT-X.
00594
00595 *---------------------------------------------------------------
00596  1060-ANTICIPATION-REF.
00597      MOVE '6'               TO CP-REFUND-TYPE-USED.
00598
00599      MOVE CP-ORIGINAL-TERM  TO WS-SAVE-TERM.
00600      MOVE CP-LOAN-TERM      TO WS-SAVE-LOAN-TERM.
00601      MOVE CP-EARNING-METHOD TO WS-SAVE-EARN-METHOD.
00602
00603      MOVE CP-REMAINING-TERM TO CP-ORIGINAL-TERM
00604                                CP-LOAN-TERM.
00605      MOVE CP-RATING-METHOD  TO CP-EARNING-METHOD.
00606
00607      PERFORM 2500-GET-RATE.
00608
00609      MOVE WS-SAVE-TERM      TO CP-ORIGINAL-TERM.
00610      MOVE WS-SAVE-LOAN-TERM TO CP-LOAN-TERM.
00611
00612      IF CP-PREMIUM-RATE = ZEROS
00613         MOVE '7'                 TO CP-RETURN-CODE
00614         MOVE WS-SAVE-EARN-METHOD TO CP-EARNING-METHOD
00615         GO TO 1999-CALC-REF-AMT-X.
00616
00617      MOVE CP-CALC-PREMIUM         TO WS-REMAIN-PREM.
00618
00619      PERFORM 2500-GET-RATE.
00620
00621      MOVE WS-SAVE-EARN-METHOD     TO CP-EARNING-METHOD.
00622
00623      IF CP-PREMIUM-RATE = ZEROS
00624         MOVE '7'            TO CP-RETURN-CODE
00625         GO TO 1999-CALC-REF-AMT-X.
00626
00627      MOVE CP-CALC-PREMIUM         TO WS-ORIG-PREM.
00628
00629      IF  WS-ORIG-PREM = ZERO
00630          MOVE ZERO                TO WS-NP-REF-FACTOR
00631         ELSE
00632          COMPUTE WS-NP-REF-FACTOR ROUNDED = WS-REMAIN-PREM /
00633                                             WS-ORIG-PREM.
00634
00635      COMPUTE CP-CALC-REFUND ROUNDED = WS-NP-REF-FACTOR *
00636                                         CP-ORIGINAL-PREMIUM.
00637
00638      IF NOT CP-EARN-AS-CALIF
00639          GO TO 1999-CALC-REF-AMT-X.
00640
00641 *---------------------------------------------------------------
00642  1065-CALIF-REGS.
00643      MOVE '3'               TO CP-REFUND-TYPE-USED.
00644      SUBTRACT CP-REMAINING-TERM FROM CP-ORIGINAL-TERM
00645          GIVING FACTOR-5.
00646
00647      IF FACTOR-5 NOT GREATER +6  AND
00648         CP-CLASS-CODE = 'A '
00649          GO TO 1050-R78-REF.
00650
00651      IF FACTOR-5 = ZERO
00652          MOVE CP-ORIGINAL-PREMIUM   TO CP-CALC-REFUND
00653          GO TO 1999-CALC-REF-AMT-X.
00654
00655      IF CP-ORIGINAL-TERM LESS +014
00656          GO TO 1999-CALC-REF-AMT-X.
00657
00658      MULTIPLY CP-ORIGINAL-TERM BY CP-ORIGINAL-TERM
00659                                    GIVING FACTOR-5 ROUNDED.
00660
00661      MULTIPLY FACTOR-5 BY +0.0000045573 GIVING FACTOR-1 ROUNDED.
00662
00663      MULTIPLY +0.001125 BY CP-ORIGINAL-TERM
00664                                    GIVING FACTOR-2 ROUNDED.
00665
00666      SUBTRACT FACTOR-2 FROM FACTOR-1.
00667      ADD +0.099375 TO FACTOR-1.
00668
00669      SUBTRACT CP-REMAINING-TERM FROM CP-ORIGINAL-TERM
00670                                    GIVING FACTOR-5 ROUNDED.
00671
00672      SUBTRACT +1.00 FROM FACTOR-5.
00673
00674      IF CP-ORIGINAL-TERM LESS +049
00675          SUBTRACT +013 FROM CP-ORIGINAL-TERM
00676                                    GIVING FACTOR-6 ROUNDED
00677      ELSE
00678          MOVE +035 TO FACTOR-6.
00679
00680      MOVE +001 TO FACTOR-2.
00681
00682      DIVIDE FACTOR-5 BY FACTOR-6 GIVING FACTOR-4 ROUNDED.
00683      SUBTRACT FACTOR-4 FROM FACTOR-2.
00684      MULTIPLY FACTOR-1 BY FACTOR-2 GIVING FACTOR-3 ROUNDED.
00685      MOVE +001 TO FACTOR-2.
00686      SUBTRACT FACTOR-3 FROM FACTOR-2 GIVING FACTOR-1 ROUNDED.
00687
00688      MULTIPLY FACTOR-1 BY CP-CALC-REFUND
00689          GIVING CAL-RFND ROUNDED.
00690
00691      IF CAL-RFND LESS +0.01
00692          MOVE ZEROS TO CAL-RFND.
00693
00694      IF CAL-RFND LESS CP-CALC-REFUND
00695          MOVE CAL-RFND TO CP-CALC-REFUND.
00696
00697      GO TO 1999-CALC-REF-AMT-X.
00698
00699 *-----------------------------------------------------------------
00700  1070-NET-PAY-REFUND.
00701      MOVE '5'                    TO  CP-REFUND-TYPE-USED.
102808     IF CP-LOAN-APR = ZERO
00704          MOVE ZEROS              TO  CP-CALC-REFUND
00705          MOVE '1'                TO  CP-RETURN-CODE
00706          GO TO 1999-CALC-REF-AMT-X.
00707
00708      MOVE CP-LOAN-APR            TO  N-P-APR.
00709      MOVE CP-ORIGINAL-TERM       TO  N-P-ORIG  N-P-LOAN.
00710      MOVE CP-REMAINING-TERM      TO  N-P-REM.
00711
00712      MOVE 'R'                    TO  N-P-OPT.
00713
00714      PERFORM 10000-NET-TERM THRU 99999-EXIT.
00715
00716      COMPUTE CP-CALC-REFUND ROUNDED = N-P-FACTOR *
00717                                         CP-ORIGINAL-PREMIUM.
00718
00719      GO TO 1999-CALC-REF-AMT-X.
00720 *---------------------------------------------------------------
00721
00722  1075-SUM-OF-DIGITS.
00723
00724      MOVE '9'               TO CP-REFUND-TYPE-USED.
00725
00726      COMPUTE RSUM-REMAINING-TERM =
00727              CP-LOAN-TERM -
00728                   (CP-ORIGINAL-TERM - (CP-REMAINING-TERM + 1))
00729
00730      COMPUTE RSUM-FACTOR =
00731         ((RSUM-REMAINING-TERM * (RSUM-REMAINING-TERM + 1)) -
00732                ((CP-LOAN-TERM - CP-ORIGINAL-TERM)          *
00733                  (CP-LOAN-TERM - CP-ORIGINAL-TERM + 1)))   /
00734             ((CP-LOAN-TERM * (CP-LOAN-TERM + 1))           -
00735                ((CP-LOAN-TERM - CP-ORIGINAL-TERM)          *
00736                  (CP-LOAN-TERM - CP-ORIGINAL-TERM  + 1))).
00737
00738      COMPUTE CP-CALC-REFUND ROUNDED =
00739            CP-ORIGINAL-PREMIUM * RSUM-FACTOR.
00740
00741      GO TO 1999-CALC-REF-AMT-X
           .
       1080-DCC-DDF-SPECIAL.
           COMPUTE CP-MONTH =
              FUNCTION REM(CP-ORIGINAL-TERM - CP-REMAINING-TERM, 12)
      *    DISPLAY ' SPEC       ' CP-DDF-SPEC-CALC
      *    DISPLAY ' MONTH      ' CP-MONTH
      *    DISPLAY ' ORIG TERM  ' CP-ORIGINAL-TERM
      *    DISPLAY ' REM TERM   ' CP-REMAINING-TERM
      *    DISPLAY ' ORIG PREM  ' CP-ORIGINAL-PREMIUM
      *    DISPLAY ' 1YR EXP AL ' CP-1ST-YR-ALLOW
      *    DISPLAY ' HI FACT    ' CP-DDF-HI-FACT
      *    DISPLAY ' LO FACT    ' CP-DDF-LO-FACT
      *    DISPLAY ' RATE UP    ' CP-IU-RATE-UP
      *    DISPLAY ' CLP RAT UP ' CP-CLP-RATE-UP
      *    DISPLAY ' EARM METH  ' CP-EARNING-METHOD
           IF CP-MONTH = 0
              MOVE 12                  TO CP-MONTH
           END-IF
           MOVE CP-MONTH               TO WS-MONTH
           MOVE 'D'                    TO CP-REFUND-TYPE-USED
           IF CP-DCC-SPP-DDF-IU
              GO TO 1090-DCC-DDF-IU
           END-IF
           IF CP-CALC-CLP
              GO TO 1085-DCC-DDF-SPECIAL-CLP
           END-IF
           EVALUATE TRUE
              WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 1)
                 OR (CP-ORIGINAL-PREMIUM = ZEROS)
                 MOVE CP-ORIGINAL-PREMIUM
                                       TO CP-CALC-REFUND
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 4
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1150-UE-GROSS-LT-4
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 13
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1160-UE-GROSS-LT-13
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
              WHEN OTHER
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1170-UE-GROSS-GT-12
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
           END-EVALUATE
           GO TO 1999-CALC-REF-AMT-X
           .
       1085-DCC-DDF-SPECIAL-CLP.
           EVALUATE TRUE
              WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 1)
                 OR (CP-ORIGINAL-PREMIUM = ZEROS)
                 MOVE CP-ORIGINAL-PREMIUM
                                       TO CP-CALC-REFUND
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 4
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1100-UE-CLP-LT-4
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 13
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1110-UE-CLP-LT-13
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
              WHEN OTHER
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1120-UE-CLP-GT-12
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
           END-EVALUATE
           GO TO 1999-CALC-REF-AMT-X
           .
       1090-DCC-DDF-IU.
           MOVE 'I'                    TO CP-REFUND-TYPE-USED
           IF CP-CALC-CLP
              GO TO 1095-DCC-DDF-IU-SPECIAL-CLP
           END-IF
           EVALUATE TRUE
              WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 1)
                 OR (CP-ORIGINAL-PREMIUM = ZEROS)
                 MOVE CP-ORIGINAL-PREMIUM
                                       TO CP-CALC-REFUND
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 4
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1250-IU-UE-GROSS-LT-4
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 13
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1260-IU-UE-GROSS-LT-13
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
              WHEN OTHER
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1270-IU-UE-GROSS-GT-12
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
           END-EVALUATE
           GO TO 1999-CALC-REF-AMT-X
           .
       1095-DCC-DDF-IU-SPECIAL-CLP.
           EVALUATE TRUE
              WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 1)
                 OR (CP-DDF-CLP = ZEROS)
                 MOVE CP-DDF-CLP       TO CP-CALC-REFUND
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 4
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1200-IU-UE-CLP-LT-4
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < 13
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1210-IU-UE-CLP-LT-13
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
              WHEN OTHER
                 MOVE CP-MONTH         TO WS-MONTH
                 PERFORM 1220-IU-UE-CLP-GT-12
                 COMPUTE CP-CALC-REFUND ROUNDED = WS-TEMP-RESULT * 1
           END-EVALUATE
           GO TO 1999-CALC-REF-AMT-X
           .
       1100-UE-CLP-LT-4.
      *****  (CLP-Yr1 Exp)-CLP*{((CLP-Yr1 Exp)/CLP - UEF1)*mo/12}
           COMPUTE WS-TEMP-RESULT ROUNDED = (CP-DDF-CLP - 0)
              - CP-DDF-CLP * (((CP-DDF-CLP - 0)
              / CP-DDF-CLP - CP-DDF-HI-FACT)
              * WS-MONTH / 12)
           .
       1110-UE-CLP-LT-13.
      *****  (CLP-Yr1 Exp)-CLP*{((CLP-Yr1 Exp)/CLP - UEF1)*3/12}
      *****  -[(CLP-Yr1 Exp)-CLP*{((CLP-Yr1 Exp)/CLP - UEF1)*3/12
      *****  -CLP*UEF1}*(mo-3)/9]
           COMPUTE WS-TEMP-RESULT ROUNDED = (CP-DDF-CLP - 0)
              - CP-DDF-CLP * (((CP-DDF-CLP - 0)
              / CP-DDF-CLP - CP-DDF-HI-FACT) * 3 / 12)
              -  ((CP-DDF-CLP - 0) - CP-DDF-CLP
              * (((CP-DDF-CLP - 0) / CP-DDF-CLP
              - CP-DDF-HI-FACT) * 3 / 12) - CP-DDF-CLP
              * CP-DDF-HI-FACT) * (WS-MONTH - 3) / 9
           .
       1120-UE-CLP-GT-12.
      *****  CLP * {UEF1 - (UEF1 -UEF2)*mo/12}
           COMPUTE WS-TEMP-RESULT ROUNDED = CP-DDF-CLP *
              (CP-DDF-HI-FACT - (CP-DDF-HI-FACT - CP-DDF-LO-FACT)
              * WS-MONTH / 12)
           .
       1150-UE-GROSS-LT-4.
           COMPUTE CP-1ST-YR-ALLOW = CP-1ST-YR-ALLOW - CP-DDF-YR1AF
           COMPUTE WS-TEMP-RESULT ROUNDED =
              (CP-ORIGINAL-PREMIUM - CP-1ST-YR-ALLOW - CP-DDF-YR1AF
              * WS-MONTH / 3) - (CP-ORIGINAL-PREMIUM -
              CP-1ST-YR-ALLOW - CP-DDF-YR1AF) * (1 - CP-DDF-HI-FACT)
              * WS-MONTH / 12
           .
       1160-UE-GROSS-LT-13.
           MOVE 3                      TO WS-MONTH
           PERFORM 1150-UE-GROSS-LT-4
           COMPUTE WS-GFR3 ROUNDED = WS-TEMP-RESULT * 1
           MOVE CP-MONTH               TO WS-MONTH
           COMPUTE WS-TEMP-RESULT ROUNDED = WS-GFR3 - (WS-GFR3
              - (CP-ORIGINAL-PREMIUM - CP-1ST-YR-ALLOW - CP-DDF-YR1AF)
              * (CP-DDF-HI-FACT)) * ((WS-MONTH - 3) / 9)
           .
       1170-UE-GROSS-GT-12.
      *****  (GF-Yr1 Texp) * {UEF1 - (UEF1 -UEF2)*mo/12}
           COMPUTE WS-TEMP-RESULT ROUNDED =
              (CP-ORIGINAL-PREMIUM - CP-1ST-YR-ALLOW)
              * (CP-DDF-HI-FACT - (CP-DDF-HI-FACT
              - CP-DDF-LO-FACT) * WS-MONTH / 12)
           .
       1200-IU-UE-CLP-LT-4.
      *****  no changes with < 4  pema 08/03/11
      *****  (CLPCounty-Yr1 Exp)-(CLP*(UEF1 - UEF2)*mo/12)
           COMPUTE WS-TEMP-RESULT ROUNDED =
              (CP-DDF-CLP - 0)
              - (CP-DDF-CLP - CP-CLP-RATE-UP)
              * (CP-DDF-HI-FACT - CP-DDF-LO-FACT)
              * WS-MONTH / 12
           .
       1210-IU-UE-CLP-LT-13.
      *****  (CLPCounty-Yr1 Exp)-(CLP*(UEF1 - UEF2)*3/12)
      *****  -[(CLPCounty-Yr1 Exp)-(CLP*(UEF1 - UEF2)*3/12)
      *****  - CLP*UEF1]*(mo-3)/9
           MOVE 3                      TO WS-MONTH
           PERFORM 1200-IU-UE-CLP-LT-4
           COMPUTE WS-UE-CLP ROUNDED = WS-TEMP-RESULT * 1
           MOVE CP-MONTH               TO WS-MONTH
           COMPUTE WS-TEMP-RESULT ROUNDED =
              WS-UE-CLP - (WS-UE-CLP - (CP-DDF-CLP - CP-CLP-RATE-UP)
                * CP-DDF-HI-FACT) * (WS-MONTH - 3) / 9
           .
       1220-IU-UE-CLP-GT-12.
      *****  CLP*{UEF1 - (UEF1 -UEF2)*mo/12}
           COMPUTE WS-TEMP-RESULT ROUNDED =
              (CP-DDF-CLP - CP-CLP-RATE-UP)
              * (CP-DDF-HI-FACT - (CP-DDF-HI-FACT
              - CP-DDF-LO-FACT) * WS-MONTH / 12)
           .
       1250-IU-UE-GROSS-LT-4.
      *****  (Comm+MngtFee)*UECLPmo/CLPCounty+CLPCounty+AdminFee-
      *****  Yr1 AF*(mo/3)-[CLP*(UEF1-UEF2)+(AdminFee-Yr1AF)*
      *****  (1-UEF1)]*mo/12
           PERFORM 1200-IU-UE-CLP-LT-4
           COMPUTE WS-UE-CLP ROUNDED = WS-TEMP-RESULT * 1
           COMPUTE WS-TEMP-RESULT = CP-DDF-COMM-AND-MFEE *
              WS-UE-CLP / CP-DDF-CLP + CP-DDF-CLP +
              CP-DDF-CSO-ADMIN-FEE - CP-DDF-YR1AF *
              WS-MONTH / 3 - ((CP-DDF-CLP - CP-CLP-RATE-UP)
              * (CP-DDF-HI-FACT - CP-DDF-LO-FACT)
              + (CP-DDF-CSO-ADMIN-FEE - CP-DDF-YR1AF)
              * (1 - CP-DDF-HI-FACT)) * WS-MONTH / 12
           .
       1260-IU-UE-GROSS-LT-13.
      ***** uegf3-(uegf3-(gf - 1st yr - clprateu)*hi))*(mo-3)/9
           MOVE 3                      TO WS-MONTH
           PERFORM 1250-IU-UE-GROSS-LT-4
           move cp-month               to ws-month
           COMPUTE WS-GFR3 ROUNDED = WS-TEMP-RESULT * 1
           COMPUTE WS-TEMP-RESULT ROUNDED =
              WS-GFR3 - (WS-GFR3 - ((CP-ORIGINAL-PREMIUM
              - CP-1ST-YR-ALLOW - cp-clp-rate-up))
              * CP-DDF-HI-FACT) * (WS-MONTH - 3) / 9
           .
       1270-IU-UE-GROSS-GT-12.
      ***** (gf - 1st yr - clprateu)*(hi-(hi-lo)*mo/12)
           COMPUTE WS-TEMP-RESULT ROUNDED =
              (CP-ORIGINAL-PREMIUM - CP-1ST-YR-ALLOW
              - CP-CLP-RATE-UP) *
              (CP-DDF-HI-FACT - (CP-DDF-HI-FACT - CP-DDF-LO-FACT)
              * WS-MONTH / 12)
           .
       1299-END-DDF-CALCS.
           .
00745  1999-CALC-REF-AMT-X.
00126
00127      MOVE CALCULATION-PASS-AREA TO DFHCOMMAREA.
00128
00129      
      * EXEC CICS RETURN
00130 *    END-EXEC.
      *    MOVE '.(                    ''   #00002403' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00131      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRFND' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00132
00133      EJECT
00134   2500-GET-RATE.
00135      
      * EXEC CICS LINK
00136 *        PROGRAM    ('ELRATE')
00137 *        COMMAREA   (CALCULATION-PASS-AREA)
00138 *        LENGTH     (CP-COMM-LENGTH)
00139 *        END-EXEC.
           MOVE 'ELRATE' TO DFHEIV1
      *    MOVE '."C                   (   #00002409' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00140
00141   2500-EXIT.
00142        EXIT.
00143
00144   9100-CONVERT-DATE.
00145      
      * EXEC CICS LINK
00146 *         PROGRAM   ('ELDATCV')
00147 *         COMMAREA  (DATE-CONVERSION-DATA)
00148 *         LENGTH    (DC-COMM-LENGTH)
00149 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002419' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00150
00151   9100-EXIT.
00152        EXIT.
00153
00154      EJECT
00155   10000-NET-TERM  SECTION.
00156 *                      COPY ERCNETP.
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

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRFND' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRFND' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
