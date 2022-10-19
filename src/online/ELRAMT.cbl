00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                ELRAMT.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/05/96 16:20:15.
00007 *                           VMOD=2.007
00008 *
00009 *AUTHOR.     LOGIC, INC.
00010 *            DALLAS, TEXAS.
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
00027 *            *    OPTION SPECIFIED, COMPUTE REMAINING AMOUNTS    *
00028 *            *    FOR A CERTIFICATE.                             *
00029 *            *                                                   *
00030 *            *    ORIGINAL BENEFIT = CP-ORIGINAL-BENEFIT         *
00031 *            *    ORIGINAL TERM = CP-ORIGINAL-TERM               *
00032 *            *    REMAINING-TERM  = CP-REMAINING-TERM            *
00033 *            *    A.P.R (FOR NET PAY) = CP-LOAN-APR              *
00034 *            *    BENEFIT TYPE = CP-BENEFIT-TYPE                 *
00035 *            *    METHOD = CP-EARNING-METHOD                     *
00036 *            *    IF COMPANY SPECIAL METHOD  - PUT COMPANY       *
00037 *            *     I.D. IN CP-COMPANY-ID.                        *
00038 *            *                                                   *
00039 *            *****************************************************
00040  ENVIRONMENT DIVISION.
00041
00042  DATA DIVISION.
00043      EJECT
00044  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00045  77  FILLER   PIC X(32) VALUE '********************************'.
00046  77  FILLER   PIC X(32) VALUE '**  ELRAMT  WORKING STORAGE   **'.
00047  77  FILLER   PIC X(32) VALUE '********* VMOD 2.007 ***********'.
00048
00049 *                          COPY ELCRAMTW.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCRAMTW.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                         *
00006 *                                                               *
00007 *****************************************************************.
041710******************************************************************
041710*                   C H A N G E   L O G
041710*
041710* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
041710*-----------------------------------------------------------------
041710*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
041710* EFFECTIVE    NUMBER
041710*-----------------------------------------------------------------
041710* 041710    2007111300001  AJRA  CALC SC NET PAY PLUS 6
071312* 071312  CR2012042600002  PEMA  AHL GROSS TRUNC RAMT
041710******************************************************************
00008
00009  01  FILLER.
00010      12  WS-PASS-SW          PIC 9                  VALUE ZERO.
00011        88 COMPUTING-CURRENT-MO                      VALUE ZERO.
00012        88 COMPUTING-PRIOR-MO                        VALUE 1.
00013
00014  01  WS-WORK-MISC.
071312     12  ws-extra-pmts       pic s999        comp-3.
00015      12  WS-REMAINING-TERM   PIC S9(4)V9     COMP-3.
00016      12  WS-TEMP-RESULT      PIC S9(9)V9(6)  COMP-3.
00017      12  WS-REMAINING-AMT    PIC S9(9)V99    COMP-3.
00018
00019  01  TEXAS-REG-WORK-AREAS.
00020      12  TEX-FACT-1          PIC S9(9)V9(2)  COMP-3.
00021      12  TEX-FACT-2          PIC S9(3)       COMP-3.
00022      12  TEX-FACT-3          PIC S9(3)       COMP-3.
00023      12  TEX-FACT-4          PIC S9(7)       COMP-3.
00024      12  TEX-FACT-5          PIC S9(3)       COMP-3.
00025      12  TEX-FACT-6          PIC S9(3)       COMP-3.
00026      12  TEX-FACT-7          PIC S9(7)       COMP-3.
00027      12  TEX-FACT-8          PIC S9V9(6)     COMP-3.
00028      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.
00029
00030  01  NET-PAY-INTERFACE.
00031      12  NP-APR              PIC S9(3)V9(4)  COMP-3.
00032      12  NP-ORIG             PIC S9(3)       COMP-3.
00033      12  NP-LOAN-TERM        PIC S9(3)       COMP-3.
00034      12  NP-REM              PIC S9(3)       COMP-3.
00035      12  NP-CAP              PIC S9(3)       COMP-3.
00036      12  NP-FACTOR           PIC S9(4)V9(9)  COMP-3.
00037      12  NP-WORK1            PIC S9(6)V9(6)  COMP-3.
00038      12  NP-WORK2            PIC S9(6)V9(6)  COMP-3.
00039      12  NP-OPT              PIC X(01).
00040          88  NPO-STD                         VALUE ' '.
00041          88  NPO-ALT                         VALUE 'A'.
00042          88  NPO-SIMPLE                      VALUE 'S'.
00043          88  NPO-2MO                         VALUE 'I'.
00044          88  NPO-TRUNC                       VALUE 'T' 'U' 'V'.
00045          88  NPO-TRUNC-0                     VALUE 'T'.
00046          88  NPO-TRUNC-1                     VALUE 'U'.
00047          88  NPO-TRUNC-2                     VALUE 'V'.
00048          88  NPO-REFUND                      VALUE 'R'.
00049      12  NP-TYPE             PIC X(01).
00050          88  NET-STD                         VALUE 'N'.
00051          88  NET-SMP                         VALUE 'S'.
00052
00053  01  COMP-3-WORK-AREA         COMP-3.
00054      12  V               PIC SV9(9)    VALUE +.0.
00055      12  I               PIC SV9(9)    VALUE +.0.
041710     12  Y               PIC S9(2)V9(5) VALUE +.0.
00056      12  WK1             PIC S9(4)V9(9).
00057      12  WK2             PIC S9(4)V9(9).
00058      12  WK3             PIC S9(4)V9(9).
00059      12  K1              PIC S999,     VALUE +1.
00060      12  K12             PIC S999,     VALUE +12.
00061      12  K100            PIC S999,     VALUE +100.
00062      12  K1000           PIC S9(7),    VALUE +1000.
00063      12  ANNUAL-INT-RATE PIC S9(3)V9(4).
00064      12  ORIGINAL-TERM   PIC S999.
00065      12  REMAINING-TERM  PIC S999.
00066      12  CAPPED-TERM     PIC S999.
00067      12  FACTOR          PIC S9(4)V9(9).
00068      12  VX              PIC S9V9(8)        VALUE +0.0.
00069      12  SV              PIC S9V9(8)        VALUE +0.0.
00070      12  SX              PIC S9V9(8)        VALUE +0.0.
00071      12  RA              PIC S9(6)V9(9)     VALUE +0.0.
00072      12  N2              PIC S9(7)          VALUE +0.
00073      12  N3              PIC S9(7)          VALUE +0.
00074      12  K-I             PIC S9V9(8)        VALUE +0.0.
00075      12  ODF             PIC S9(3)V9(10)    VALUE +0.0.
00076      12  OD              PIC S9(3)V9(10)    VALUE +0.0.
00077      12  ANGLEM          PIC S9(7)V9(10)    VALUE +0.0.
00078      12  ANGLEN          PIC S9(7)V9(10)    VALUE +0.0.
00079      12  ANGLEU          PIC S9(7)V9(10)    VALUE +0.0.
00080      12  ANGLEM1         PIC S9(7)V9(10)    VALUE +0.0.
00081      12  ANGLEN1         PIC S9(7)V9(10)    VALUE +0.0.
00082
00083  01  BINARY-WORK-AREA        COMP.
00084      12  X1      PIC S999        VALUE +0.
00085      12  X2      PIC S999        VALUE +0.
00086      12  MAX-X   PIC S9(5)       VALUE +0.
00087      12  B1      PIC S9(5)       VALUE +1.
00088      12  B2      PIC S9(5)       VALUE +2.
00089
00050      EJECT
00051 *                          COPY ELCCALC.
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
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
00178                                   '9' 'A' 'B' 'C' 'D' 'E' 'H'.
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
00408          16  CP-POLICY-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
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
041710         16  FILLER                PIC X(15).
00514 ******************************************************************
00052      EJECT
00053 *                          COPY ELCDATE.
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
00054
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
00056  01  DFHCOMMAREA                 PIC X(450).
00057
00058      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELRAMT' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00060      MOVE DFHCOMMAREA            TO  CALCULATION-PASS-AREA.
00061
00062  000-START-REM-CALC.        
      *                           COPY ELCRAMTP.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCRAMTP.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                         *
00006 *                                                               *
00007 *                CALCULATE THE REMAINING AMOUNT.                *
00008 *****************************************************************
041710*****************************************************************
041710*                   C H A N G E   L O G
041710*
041710* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
041710*-----------------------------------------------------------------
041710*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
041710* EFFECTIVE    NUMBER
041710*-----------------------------------------------------------------
041710* 041710    2007111300001  AJRA  CALC SC NET PAY PLUS 6
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
071312* 071312  CR2012042600002  PEMA  AHL GROSS TRUNC RAMT
060414* 060414  IR2014060200001  PEMA  DISAB REM AMT CHANGES
041710******************************************************************
00009
00010      MOVE ZERO                   TO  CP-RETURN-CODE
00011                                      CP-REMAINING-AMT
00012                                      CP-REMAINING-AMT-PRV
00013                                      CP-REMAMT-FACTOR
00014                                      NP-FACTOR
00015                                      WS-PASS-SW.
00016
00017  050-START.
00018      IF COMPUTING-PRIOR-MO
00019          COMPUTE WS-REMAINING-TERM = CP-REMAINING-TERM + +1
00020       ELSE
00021           MOVE CP-REMAINING-TERM   TO WS-REMAINING-TERM.
00022
00023      IF CP-BENEFIT-TYPE = 'A'
071211        IF (CP-R-MAX-MON-BEN NOT = ZEROS)
060414           and (cp-r-max-mon-ben < cp-original-benefit)
071211           COMPUTE CP-REMAINING-AMT = CP-REMAINING-TERM *
071211              CP-R-MAX-MON-BEN
071211        ELSE
00024            COMPUTE CP-REMAINING-AMT = CP-REMAINING-TERM  *
00025               CP-ORIGINAL-BENEFIT
071211        END-IF
00026         GO TO 999-CALC-REM-AMT-X
071211     END-IF
00027
00028      IF CP-BENEFIT-TYPE = 'L' OR 'P'
00029          IF CP-EARN-AS-NET-PAY
00030              NEXT SENTENCE
00031          ELSE
00032              MOVE CP-ORIGINAL-BENEFIT    TO  WS-REMAINING-AMT
00033              GO TO 800-CALC-REM-RESULT.
00034
00035      IF CP-ORIGINAL-TERM = ZERO
00036          MOVE '4'                TO  CP-RETURN-CODE
00037          GO TO 999-CALC-REM-AMT-X.
00038
00039 *    IF (CP-EARN-AS-REG-BALLOON)
00040 *      AND
00041 *       (NOT CP-BALLOON-LAST-PMT)
00042 *       COMPUTE WS-TEMP-RESULT ROUNDED EQUAL
00043 *         CP-ORIGINAL-BENEFIT / (CP-ORIGINAL-TERM - 1)
00044 *    ELSE
071312     if (cp-company-id = 'AHL')
071312        and (cp-lf-claim-calc-sw = 'Y')
071312        and (cp-loan-term > cp-original-term)
071312        compute ws-temp-result rounded =
071312           cp-original-benefit / cp-loan-term
071312     else
00045         COMPUTE WS-TEMP-RESULT ROUNDED EQUAL
00046                 CP-ORIGINAL-BENEFIT / CP-ORIGINAL-TERM
071312     end-if
00047
00048      IF CP-EARN-AS-REG-BALLOON
00049         GO TO 400-CALC-REM-BALLOON.
00050
00051      IF (CP-EARN-AS-NET-PAY)
pemtst*       AND (CP-LOAN-APR > ZEROS)
00052         GO TO 300-CALC-NET-PAY-REM
           END-IF
00053
00054      IF CP-SPECIAL-CALC-CD IS EQUAL TO 'N'
00055          GO TO 300-CALC-NET-PAY-REM.
00056
00057      MOVE CP-CERT-EFF-DT            TO  DC-BIN-DATE-1.
00058      MOVE ' '                       TO  DC-OPTION-CODE.
00059      PERFORM 0030-CNVT-DT THRU 0030-CNVT-EXIT.
00060
CIDMOD*    IF CP-STATE-STD-ABBRV IS EQUAL TO 'OH'   AND
CIDMOD     IF (CP-STATE-STD-ABBRV IS EQUAL TO 'OH')   AND
CIDMOD        (CP-CLASS-CODE NOT = 'L ') AND
00062         CP-ORIGINAL-TERM IS GREATER THAN +60  AND
00063         CP-LOAN-APR IS GREATER THAN +0
00064          IF DC-GREG-DATE-CYMD IS GREATER THAN 19831031
00065              GO TO 300-CALC-NET-PAY-REM.
00066
00067      IF CP-STATE-STD-ABBRV IS EQUAL TO 'MT'   AND
00068         CP-ORIGINAL-TERM IS GREATER THAN +61  AND
00069         CP-LOAN-APR IS GREATER THAN +0
00070          IF DC-GREG-DATE-CYMD IS GREATER THAN 19830318
00071              GO TO 300-CALC-NET-PAY-REM.
00072
00073      IF CP-STATE-STD-ABBRV IS EQUAL TO 'UT'   AND
00074         CP-ORIGINAL-TERM IS GREATER THAN +62  AND
00075         CP-LOAN-APR IS GREATER THAN +0
00076          IF DC-GREG-DATE-CYMD IS GREATER THAN 19810831  AND
00077             DC-GREG-DATE-CYMD IS LESS    THAN 19830901
00078              GO TO 300-CALC-NET-PAY-REM.
00079
00080      IF CP-STATE-STD-ABBRV IS EQUAL TO 'RI'   AND
00081         CP-ORIGINAL-TERM IS GREATER THAN +60  AND
00082         CP-LOAN-APR IS GREATER THAN +0
00083          IF DC-GREG-DATE-CYMD IS GREATER THAN 19831231
00084              GO TO 300-CALC-NET-PAY-REM.
00085
00086      IF WS-REMAINING-TERM = CP-ORIGINAL-TERM
00087          MOVE CP-ORIGINAL-BENEFIT TO WS-REMAINING-AMT
00088          GO TO 800-CALC-REM-RESULT.
00089
00090      IF CP-EARN-AS-TEXAS
00091          GO TO 200-CALC-TEXAS-REM.
00092
00093  100-ORDINARY-REM.
071312     if (cp-company-id = 'AHL')
071312        and (cp-loan-term > cp-original-term)
071312        compute ws-temp-result rounded =
071312           cp-original-benefit / cp-loan-term
071312        compute ws-remaining-amt rounded = ws-temp-result *
071312           (ws-remaining-term + cp-loan-term - cp-original-term)
071312     else
00094         COMPUTE WS-REMAINING-AMT ROUNDED = WS-TEMP-RESULT *
00095                                           WS-REMAINING-TERM
071312     end-if
00096      GO TO 800-CALC-REM-RESULT.
00097
00098  200-CALC-TEXAS-REM.
00099      DIVIDE CP-ORIGINAL-BENEFIT BY CP-ORIGINAL-TERM
00100          GIVING TEX-FACT-1.
00101
00102      IF CP-PAY-FREQUENCY = ZERO
00103          MOVE '5'                TO  CP-RETURN-CODE
00104          GO TO 999-CALC-REM-AMT-X.
00105
00106      DIVIDE WS-REMAINING-TERM BY CP-PAY-FREQUENCY
00107          GIVING TEX-FACT-2
00108          REMAINDER TEX-FACT-3.
00109
00110      IF TEX-FACT-3 NOT = ZERO
00111          ADD +1 TO TEX-FACT-2.
00112
00113      IF (TEX-FACT-2 * CP-PAY-FREQUENCY) = CP-ORIGINAL-TERM
00114          MOVE CP-ORIGINAL-BENEFIT TO WS-REMAINING-AMT
00115      ELSE
00116          COMPUTE WS-REMAINING-AMT ROUNDED =
00117              (TEX-FACT-1 * (TEX-FACT-2 * CP-PAY-FREQUENCY)).
00118
00119      GO TO 800-CALC-REM-RESULT.
00120
00121  300-CALC-NET-PAY-REM.
00122
00123      MOVE CP-LOAN-APR            TO  NP-APR.
00124      MOVE CP-ORIGINAL-TERM       TO  NP-ORIG.
00125      MOVE WS-REMAINING-TERM      TO  NP-REM.
00126      MOVE CP-LOAN-TERM           TO  NP-LOAN-TERM.
00127      MOVE CP-SPECIAL-CALC-CD     TO  NP-OPT.
00128
00129      IF CP-COMPANY-ID IS NOT EQUAL TO 'NCL'
00130          GO TO 300-CONT-CALC-NET-PAY-REM.
00131
00132      MOVE CP-CERT-EFF-DT         TO  DC-BIN-DATE-1.
00133      MOVE CP-FIRST-PAY-DATE      TO  DC-BIN-DATE-2.
00134      MOVE '1'                    TO  DC-OPTION-CODE.
00135      PERFORM 0030-CNVT-DT THRU 0030-CNVT-EXIT.
00136
00137      IF DATE-CONVERSION-ERROR
00138          MOVE '2'                TO  CP-RETURN-CODE
00139          GO TO 999-CALC-REM-AMT-X
00140      ELSE
00141          MOVE DC-ELAPSED-MONTHS  TO  CP-MNTHS-TO-FIRST-PMT
00142          MOVE DC-ODD-DAYS-OVER   TO  CP-ODD-DAYS-TO-PMT.
00143
00144      IF WS-REMAINING-TERM = CP-ORIGINAL-TERM
00145          MOVE +1                 TO ODF
00146      ELSE
00147          COMPUTE ODF =
00148          ((1 + (NP-APR / +1200)) ** (1 - CP-MNTHS-TO-FIRST-PMT)) /
00149          ((1 + (CP-ODD-DAYS-TO-PMT * NP-APR / 36500))).
00150
00151  300-CONT-CALC-NET-PAY-REM.
00152
00153      PERFORM 1000-NET-TERM.
00154
041710     IF ANNUAL-INT-RATE = ZEROS
041710         COMPUTE WS-REMAINING-AMT ROUNDED =
041710             (CP-ORIGINAL-BENEFIT / CP-ORIGINAL-TERM)
041710              * WS-REMAINING-TERM
041710     ELSE
00155      COMPUTE WS-REMAINING-AMT ROUNDED =
00156              NP-FACTOR * (CP-ORIGINAL-BENEFIT / +1000).
00157
032612     IF CP-COMPANY-ID NOT = 'CID' AND 'AHL'
TSTMOD        IF CP-STATE-STD-ABBRV IS EQUAL TO 'NC'
TSTMOD                  AND
TSTMOD           CP-EARN-AS-NET-PAY
TSTMOD            MOVE CP-CERT-EFF-DT         TO  DC-BIN-DATE-1
TSTMOD            MOVE ' '                    TO  DC-OPTION-CODE
TSTMOD            PERFORM 0030-CNVT-DT THRU 0030-CNVT-EXIT
TSTMOD            IF DC-GREG-DATE-CYMD IS GREATER THAN 19931231
TSTMOD                GO TO 500-CALC-REM-RESULT.
071312     evaluate true
071312        when (cp-company-id = 'AHL')
071312           and (cp-benefit-cd = '5M' OR '6M')
071312           and (cp-lf-claim-calc-sw = 'Y')
071312           move 1                to ws-extra-pmts
071312           go to 600-calc-rem-result
071312        when (cp-company-id = 'AHL')
071312           and (cp-benefit-cd = '5G' or '5J' or '5S'
071312                or '6D' or '6H' or '6R')
071312           and (cp-lf-claim-calc-sw = 'Y')
071312           move 2                to ws-extra-pmts
071312           go to 600-calc-rem-result
071312        when (cp-company-id = 'AHL')
071312           and (cp-benefit-cd = '5I' OR '6J')
071312           and (cp-lf-claim-calc-sw = 'Y')
071312           move 6                to ws-extra-pmts
071312           go to 600-calc-rem-result
071312        when(CP-COMPANY-ID = 'CID' AND
041710           CP-STATE-STD-ABBRV = 'SC' AND
041710           CP-EARN-AS-NET-PAY AND
041710           CP-LF-CLAIM-CALC-SW = 'Y' AND
041710           (CP-BENEFIT-CD = '2I' OR '2J' OR '2K' OR '2L'))
071312           move 6                to ws-extra-pmts
041710           GO TO 600-CALC-REM-RESULT
071312     end-evaluate
00167      GO TO 800-CALC-REM-RESULT.
00168
00169  400-CALC-REM-BALLOON.
00170      IF CP-ORIGINAL-TERM LESS THAN CP-REMAINING-TERM
00171         MOVE CP-ORIGINAL-BENEFIT TO CP-REMAINING-AMT
00172         ADD CP-ALTERNATE-BENEFIT TO CP-REMAINING-AMT
00173         GO TO 999-CALC-REM-AMT-X.
00174
00175      IF NOT CP-BALLOON-LAST-PMT
00176         COMPUTE WS-REMAINING-AMT ROUNDED =
00177           (WS-TEMP-RESULT * CP-REMAINING-TERM) +
00178 *         (WS-TEMP-RESULT * (CP-REMAINING-TERM - 1)) +
00179            CP-ALTERNATE-BENEFIT
00180      ELSE
00181         COMPUTE WS-REMAINING-AMT ROUNDED =
00182           (WS-TEMP-RESULT * CP-REMAINING-TERM ) +
00183            CP-ALTERNATE-BENEFIT.
00184
00185      GO TO 800-CALC-REM-RESULT.
00186
00187  500-CALC-REM-RESULT.
00188
00189      IF ANNUAL-INT-RATE = ZEROS
00190          MOVE CP-ORIGINAL-BENEFIT
00191                               TO  WS-REMAINING-AMT
00192          GO TO 999-CALC-REM-AMT-X.
00193
00194      IF REMAINING-TERM  = ZEROS
00195          GO TO 999-CALC-REM-AMT-X.
00196
00197      COMPUTE ANGLEN = WK1 / I.
00198      COMPUTE WS-TEMP-RESULT = CP-ORIGINAL-BENEFIT / ANGLEN.
00199      COMPUTE WS-REMAINING-AMT =
00200              WS-REMAINING-AMT + (WS-TEMP-RESULT * 3).
00201      MOVE WS-TEMP-RESULT      TO  CP-MONTHLY-PAYMENT.
00202
00203      GO TO 800-CALC-REM-RESULT.
00204
041710 600-CALC-REM-RESULT.
041710
041710     IF WS-REMAINING-TERM  = ZEROS
041710         GO TO 800-CALC-REM-RESULT
041710     END-IF.
041710
041710     IF ANNUAL-INT-RATE = ZEROS
041710         COMPUTE WS-TEMP-RESULT ROUNDED = CP-ORIGINAL-BENEFIT /
041710                                  CP-ORIGINAL-TERM
041710         COMPUTE WS-REMAINING-AMT ROUNDED =
071312             WS-TEMP-RESULT * (WS-REMAINING-TERM + WS-EXTRA-PMTS)
041710         IF WS-REMAINING-AMT > CP-ORIGINAL-BENEFIT
041710            MOVE CP-ORIGINAL-BENEFIT TO WS-REMAINING-AMT
041710         END-IF
041710         MOVE WS-TEMP-RESULT      TO  CP-MONTHLY-PAYMENT
041710         GO TO 800-CALC-REM-RESULT
041710     END-IF.
041710
041710     COMPUTE ANGLEN = WK1 / I.
041710     COMPUTE Y = (1 + ((CP-TERM-OR-EXT-DAYS + +30) * I) / +30)
041710                 / (1 + I)
041710     COMPUTE ANGLEN1 = ANGLEN / Y
041710     COMPUTE WS-TEMP-RESULT ROUNDED = CP-ORIGINAL-BENEFIT /
041710                                   ANGLEN1.
041710     COMPUTE CP-SCNP-6MO-AMT ROUNDED = WS-TEMP-RESULT
071312        * WS-EXTRA-PMTS.
041710     COMPUTE WS-REMAINING-AMT ROUNDED =
041710             WS-REMAINING-AMT + CP-SCNP-6MO-AMT.
041710     IF WS-REMAINING-AMT > CP-ORIGINAL-BENEFIT
041710         MOVE CP-ORIGINAL-BENEFIT TO WS-REMAINING-AMT
041710     END-IF
           .
00205  800-CALC-REM-RESULT.
00206      IF COMPUTING-CURRENT-MO
00207         MOVE WS-REMAINING-AMT TO CP-REMAINING-AMT
00208         MOVE NP-FACTOR        TO CP-REMAMT-FACTOR
00209         MOVE 1                TO WS-PASS-SW
071211        IF (CP-R-MAX-TOT-BEN NOT = ZEROS)
071211           AND (CP-R-MAX-TOT-BEN < CP-REMAINING-AMT)
071211           MOVE CP-R-MAX-TOT-BEN TO CP-REMAINING-AMT
071211        END-IF
00210         GO TO 050-START
00211       ELSE
00212         MOVE WS-REMAINING-AMT TO CP-REMAINING-AMT-PRV
071211        IF (CP-R-MAX-TOT-BEN NOT = ZEROS)
071211           AND (CP-R-MAX-TOT-BEN < CP-REMAINING-AMT-PRV)
071211           MOVE CP-R-MAX-TOT-BEN TO CP-REMAINING-AMT-PRV
071211        END-IF
071211     END-IF
071211     .
00214  999-CALC-REM-AMT-X.
00215
00063
00064      MOVE CALCULATION-PASS-AREA  TO  DFHCOMMAREA.
00065
00066      
      * EXEC CICS RETURN
00067 *        END-EXEC.
      *    MOVE '.(                    ''   #00001365' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00068
00069       
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRAMT' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00070
00071  0030-CNVT-DT.
00072
00073      
      * EXEC CICS LINK
00074 *        PROGRAM    ('ELDATCV')
00075 *        COMMAREA   (DATE-CONVERSION-DATA)
00076 *        LENGTH     (DC-COMM-LENGTH)
00077 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00001372' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00078
00079  0030-CNVT-EXIT.
00080      EXIT.
00081
00082  1000-NET-TERM SECTION.          
      *                                COPY ELCRAMTN.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCRAMTN.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                         *
00006 *                                                               *
00007 *             CALCULATE NET PAY REMAINING AMOUNT                *
00008 *****************************************************************.
00009
00010      MOVE NP-APR                 TO  ANNUAL-INT-RATE.
00011      MOVE NP-ORIG                TO  ORIGINAL-TERM.
00012      MOVE NP-REM                 TO  REMAINING-TERM.
00013
00014  1000-EDIT-DATA.
00015      MOVE +0 TO FACTOR.
00016
00017      IF ANNUAL-INT-RATE = ZERO
00018          GO TO 9000-ERROR.
00019
00020      IF ORIGINAL-TERM  = ZERO OR
00021         REMAINING-TERM = ZERO
00022          GO TO 9500-RETURN.
00023
00024      IF NP-LOAN-TERM NOT NUMERIC
00025         MOVE +0 TO NP-LOAN-TERM.
00026
00027      IF NP-LOAN-TERM EQUAL +0
00028         MOVE ORIGINAL-TERM TO NP-LOAN-TERM.
00029
00030      IF REMAINING-TERM GREATER ORIGINAL-TERM
00031          GO TO 9000-ERROR.
00032
00033 *    IF ANNUAL-INT-RATE LESS THAN +3
00034 *        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.
00035 *    IF ANNUAL-INT-RATE LESS THAN +3
00036 *        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.
00037 *    IF ANNUAL-INT-RATE LESS THAN +3
00038 *        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.
00039
00040      IF NPO-SIMPLE
00041          MOVE 'S'                TO  NP-TYPE
00042      ELSE
00043          MOVE 'N'                TO  NP-TYPE.
00044
00045      COMPUTE I ROUNDED = (ANNUAL-INT-RATE / K100) / K12.
00046
00047      COMPUTE V ROUNDED = K1 / (K1 + I).
00048
00049      IF (NP-LOAN-TERM GREATER THAN NP-ORIG)
00050                 AND
00051         (CP-TRUNCATED-LIFE OR CP-SPECIAL-CALC-CD = 'N')
00052          MOVE NP-LOAN-TERM TO MAX-X
00053          COMPUTE REMAINING-TERM EQUAL REMAINING-TERM +
00054              (NP-LOAN-TERM - NP-ORIG)
00055      ELSE
00056          MOVE ORIGINAL-TERM TO MAX-X.
00057
00058      COMPUTE X1 = MAX-X - ORIGINAL-TERM.
00059
00060      MOVE B1 TO X1 SX.
00061      MOVE V TO VX  SV.
00062
00063      IF MAX-X = B1
00064          GO TO 1000-COMPUTE-REMAINING-FACTOR.
00065
00066  1000-VX-LOOP.
00067      COMPUTE VX ROUNDED = VX * V.
00068
00069      ADD B1 TO X1.
00070
00071      IF X1 = REMAINING-TERM
00072          MOVE VX TO SV.
00073
00074      IF X1 = X2
00075          MOVE VX TO SX.
00076
00077      IF X1 NOT = MAX-X
00078          GO TO 1000-VX-LOOP.
00079
00080  1000-COMPUTE-REMAINING-FACTOR.
00081      COMPUTE WK1 = K1 - VX.
00082
00083      COMPUTE WK2 = K1 - SV.
00084
00085      IF CP-COMPANY-ID IS EQUAL TO 'NCL'       AND
00086         CP-EARN-AS-NET-PAY
00087          GO TO 2100-CHECK-NCL-NET-PAY.
00088
00089      IF CP-COMPANY-ID IS EQUAL TO 'NCL'
00090         IF CP-SPECIAL-CALC-CD IS EQUAL TO 'N'
00091             IF CP-LOAN-TERM IS GREATER THAN CP-ORIGINAL-TERM
00092                 GO TO 2100-NCL-NET-TRUNC
00093             ELSE
00094                 GO TO 2100-NCL-NET-PAY.
00095
00096 *    IF CP-TRUNCATED-LIFE
00097 *        IF CP-LOAN-TERM = REMAINING-TERM
00098 *            GO TO 2000-PREMIUM-RATE.
00099
00100 *    IF NOT CP-TRUNCATED-LIFE
00101 *        IF ORIGINAL-TERM = REMAINING-TERM
00102 *            GO TO 2000-PREMIUM-RATE.
00103
00104      COMPUTE WK3 ROUNDED = (WK2 * K1000) / WK1.
00105
00106      MOVE WK3 TO FACTOR.
00107
00108      GO TO 9500-RETURN.
00109
00110  2000-PREMIUM-RATE.
00111
00112      COMPUTE K-I = K1 + I
00113
00114      IF NPO-ALT OR NPO-TRUNC
00115          MOVE K1                 TO  K-I.
00116
00117      IF NPO-2MO OR NPO-TRUNC-2
00118          COMPUTE K-I = K1 + (2 * I).
00119
00120      COMPUTE RA ROUNDED = 1 -
00121          ((X2 * (X2 + 1)) /
00122           (MAX-X * (MAX-X + 1))).
00123
00124      IF NET-STD
00125          COMPUTE WK3 ROUNDED =
00126              ((I * ORIGINAL-TERM) + VX - SX) * 2 * MAX-X
00127          COMPUTE WK3 ROUNDED =
00128              WK3 / (( 1 - VX) * ORIGINAL-TERM * I)
00129          COMPUTE WK3 ROUNDED =
00130              WK3 / ((2 * MAX-X) - ORIGINAL-TERM + 1)
00131          COMPUTE WK3 ROUNDED =
00132              WK3 * RA * K-I.
00133
00134      IF NET-SMP
00135          COMPUTE N2 = MAX-X * MAX-X
00136          COMPUTE N3 = N2 * MAX-X
00137          COMPUTE WK3 ROUNDED = 2 * N2 * WK1
00138          COMPUTE WK3 ROUNDED = WK3 + (N3 * I) - (N2 * I)
00139          COMPUTE WK3 ROUNDED = WK3 + (4 * MAX-X * WK1)
00140          COMPUTE WK3 ROUNDED = WK3 * (1 + I) * 10
00141          COMPUTE WK3 ROUNDED = WK3 / (36 * (MAX-X + 1) * WK1).
00142
00143      MOVE WK3                    TO  FACTOR.
00144      GO TO 9500-RETURN.
00145
00146  2100-CHECK-NCL-NET-PAY.
00147
00148      IF CP-STATE-STD-ABBRV IS EQUAL TO 'MN'    AND
00149         CP-ORIGINAL-TERM IS GREATER THAN +63   AND
00150         CP-TRUNCATED-LIFE
00151          GO TO 2100-CHECK-MINN-TRUNC.
00152
00153      IF CP-STATE-STD-ABBRV IS EQUAL TO 'MN' AND
00154         CP-ORIGINAL-TERM IS GREATER THAN +63
00155          GO TO 2100-CHECK-MINN.
00156
00157      IF CP-STATE-STD-ABBRV IS EQUAL TO 'MT'    AND
00158         CP-ORIGINAL-TERM IS GREATER THAN +63   AND
00159         CP-TRUNCATED-LIFE
00160          GO TO 2100-CHECK-MONTANA-TRUNC.
00161
00162      IF CP-STATE-STD-ABBRV IS EQUAL TO 'MT' AND
00163         CP-ORIGINAL-TERM IS GREATER THAN +63
00164          GO TO 2100-CHECK-MONTANA.
00165
00166      IF CP-TRUNCATED-LIFE
00167          GO TO 2100-NCL-NET-TRUNC.
00168
00169  2100-NCL-NET-PAY.
00170
00171      COMPUTE ANGLEN = WK1 / I.
00172
00173      COMPUTE ANGLEU = WK2 / I.
00174
00175      COMPUTE ANGLEN1 = ANGLEN * ODF.
00176
00177      COMPUTE FACTOR = (ANGLEU / ANGLEN1) * 1000.
00178
00179      COMPUTE WS-REMAINING-AMT ROUNDED =
00180          (CP-ORIGINAL-BENEFIT * ANGLEU) / ANGLEN1.
00181
00182      GO TO 9500-RETURN.
00183
00184  2100-NCL-NET-TRUNC.
00185
00186      COMPUTE ANGLEM = WK1 / I.
00187
00188      COMPUTE ANGLEU = WK2 / I.
00189
00190      COMPUTE ANGLEM1 = ANGLEM * ODF.
00191
00192      COMPUTE FACTOR = (ANGLEU / ANGLEM1) * 1000.
00193
00194      COMPUTE WS-REMAINING-AMT ROUNDED =
00195          (CP-ORIGINAL-BENEFIT * ANGLEU) / ANGLEM1.
00196
00197      GO TO 9500-RETURN.
00198
00199  2100-CHECK-MINN-TRUNC.
00200
00201      COMPUTE ANGLEM = WK1 / I.
00202
00203      COMPUTE ANGLEU = WK2 / I.
00204
00205      COMPUTE ANGLEM1 = ANGLEM * ODF.
00206
00207      COMPUTE FACTOR = ((ANGLEU + 2) / ANGLEM1) * 1000.
00208
00209      COMPUTE WS-REMAINING-AMT ROUNDED =
00210          (CP-ORIGINAL-BENEFIT * (ANGLEU + 2)) / ANGLEM1.
00211
00212      GO TO 9500-RETURN.
00213
00214  2100-CHECK-MINN.
00215
00216      COMPUTE ANGLEN = WK1 / I.
00217
00218      COMPUTE ANGLEU = WK2 / I.
00219
00220      COMPUTE ANGLEN1 = ANGLEN * ODF.
00221
00222      COMPUTE FACTOR = ((ANGLEU + 2) / ANGLEN1) * 1000.
00223
00224      COMPUTE WS-REMAINING-AMT ROUNDED =
00225          (CP-ORIGINAL-BENEFIT * (ANGLEU + 2)) / ANGLEN1.
00226
00227      GO TO 9500-RETURN.
00228
00229  2100-CHECK-MONTANA-TRUNC.
00230
00231      COMPUTE ANGLEM = WK1 / I.
00232
00233      COMPUTE ANGLEU = WK2 / I.
00234
00235      COMPUTE ANGLEM1 = ANGLEM * ODF.
00236
00237      COMPUTE FACTOR = ((ANGLEU + 4) / ANGLEM1) * 1000.
00238
00239      COMPUTE WS-REMAINING-AMT ROUNDED =
00240          (CP-ORIGINAL-BENEFIT * (ANGLEU + 4)) / ANGLEM1.
00241
00242      GO TO 9500-RETURN.
00243
00244  2100-CHECK-MONTANA.
00245
00246      COMPUTE ANGLEN = WK1 / I.
00247
00248      COMPUTE ANGLEU = WK2 / I.
00249
00250      COMPUTE ANGLEN1 = ANGLEN * ODF.
00251
00252      COMPUTE FACTOR = ((ANGLEU + 4) / ANGLEN1) * 1000.
00253
00254      COMPUTE WS-REMAINING-AMT ROUNDED =
00255          (CP-ORIGINAL-BENEFIT * (ANGLEU + 4)) / ANGLEN1.
00256
00257      GO TO 9500-RETURN.
00258
00259  9000-ERROR.
00260      GO TO 9500-RETURN.
00261
00262  9500-RETURN.
00263      MOVE FACTOR                 TO  NP-FACTOR.
00264
00265  9999-EXIT.
00266      EXIT.
00267
00083

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRAMT' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRAMT' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
