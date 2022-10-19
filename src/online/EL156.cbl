00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL156 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 05/17/94 13:55:56.
00007 *                            VMOD=2.098
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00024 *REMARKS.    TRANSACTION - EX29 - PAYMENT PROCESSING
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST CALC
052506* 052506    2006030600001  AJRA  ADD PROOF DATE
071806* 071806    2004040700004  PEMA  ADD TONYS PROOF DATE TO INT CALC
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
110807* 110807  IR2007110700001  AJRA  DO NOT CALC INT ON MANUAL PMTS
022208* 022208    2008021500001  AJRA  DO NOT DEFAULT PROOF DATE,FORCE E
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
060608* 060608    2008040300002  PEMA  ADD EFF DT TO ELCLMI PASS AREA
091808* 091808    2008022800002  AJRA  GET CHECK NUMBER FROM STATE CNTL
012810* 012810    2007111300001  AJRA  EXPAND EDIT PATTERN
041710* 041710    2007111300001  AJRA  ADD MSG AND CALC PMT FOR SC NET P
092310* 092310  IR2010090900001  PEMA  CHANGE CRIT PERIOD EDIT
112210* 112210    2010091000003  AJRA  MICHIGAN FULL MONTH ON 1ST PAYMEN
042011* 042011  CR2011040600001  PEMA  ADD EOB PROCESSING FOR DCC
061511* 061511    2011042000002  AJRA  VERIFY 2ND BENEFICIARY SSN
030512* 030512    2011120900003  AJRA  ADD AHL COMPANY CODE
041812* 041812  IR2012041800001  AJRA  FIX TERM/REM ON SCREEN
013013* 013013    2012092400003  AJRA  SPECIAL EOB FOR ACCELERATED DEATH
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
020513* 020513    2012092400005  AJRA  LIFE PAYEE NAME SHOULD NOT = INS
032813* 032813    2011013100001  AJRA  SELECT CLAIM PAYMENTS FOR REAUDIT
061013* 061013    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
110513* 110513    2013102900002  AJRA  FIX AK PAYEE STATE CODE ROUTINE
102413* 102413    2013100800001  AJRA  ADD SPECIAL RELEASE IND
111113* 111113    2013110600002  AJRA  CHG LEVEL 5 RESTRICTIONS TO LEVEL
030514* 030514    2014021300001  AJRA  FORCE LF PAYMENT > BENEFIT AMT
032414* 032414    2013092400002  AJRA  LIFE ADDIT PMTS TO ESTATE SHOULD
032514* 032514    2013111100001  AJRA  VERIFY SSN FOR DISAB PMTS
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052814* 052814  CR2014041800001  AJRA  SET APPROVAL FLAG FOR 3550 MESSAG
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
070714* 070714    2014052800001  PEMA  correct read on erpdef for DCC
100314* 100314  CR2014061900001  PEMA  Add pct of benefit funcionality
111714* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
032015* 032015  CR2014121000003  TANA  Default prnt survey to 'N'for DCC
110515* 110515  IR2015103000001  TANA  Check app level for 3550 message
120115* 120115  CR2015082700002  PEMA  ADD JNT BORROWER PROCESSING
012016* 012016  IR2016011800003  PEMA  FIXED LENGTH OF PAYEE NAME
050616* 050616  CR2015030200004  TANA  Additional pmt Pd thru exp date
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
102617* 102617    2017101800003  TANA  Fix family leave auth level
022718* 022718    2017100200004  TANA  Set hold & pays > than 30 days
031218* 031218    2018030800002  TANA  Handle offline pmts w/ hold & pay
082218* 082218  CR2018051400001  TANA  Hold and Pay
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
112018* 112018  CR2018102900001  TANA  REQUIRE EOB NOTE STARTING WITH F
040819* 040819  IR2019030400004  TANA  ADD EDITS FOR HOLD AND PAY
043019* 043019  IR2019041900001  TANA  Correct total claim pymts msg
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
090821* 090821  CR2021081200003  PEMA  Add error if inc > act cnc dt
022122* 022122  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00025
00026      EJECT
00027  ENVIRONMENT DIVISION.
00028
00029  DATA DIVISION.
00030
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032
00033  77  FILLER  PIC X(32)  VALUE '********************************'.
00034  77  FILLER  PIC X(32)  VALUE '*    EL156 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.098 **********'.
       77  WS-SSN  PIC X(9) VALUE ZEROS.
       77  WS-SET-NOTE2-MDT            PIC X  VALUE SPACES.
       77  C0                          PIC S999 COMP-3 VALUE +0.
       77  C1                          PIC S999 COMP-3 VALUE +0.
       77  C2                          PIC S999 COMP-3 VALUE +0.
       77  C3                          PIC S999 COMP-3 VALUE +0.
       77  s1                          PIC S999 COMP-3 VALUE +0.
       77  s2                          pic s999 comp-3 value +0.
       77  P1                          PIC S999 COMP-3 VALUE +0.
       77  WS-DDF-COMM-AND-MFEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-ADMIN-FEES           PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-CSO-ADMIN-FEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-1ST-YR-TOT-EXP       PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-STATE-EXT-DAYS-CHG       PIC X  VALUE ' '.
       77  TEX-FACT-8                  PIC S9V9(6)     COMP-3.
       77  WS-COMM-PCT                 PIC S9(5)V9(5)  COMP-3 VALUE +0.
       77  WS-TERM                     PIC S999 COMP-3 VALUE +0.
       77  ws-monthly-benefit          pic s9(11)v99 comp-3 value +0.
       77  ws-max-bens                 pic s999 comp-3 value +0.
       77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
       77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.
100314 77  ws-work-ben-pct             pic s9v999 comp-3 value +0.
022122 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
022122 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.
022122 77  ws-MAX-TOT-BEN              pic s9(7)v99 comp-3 value +0.
00036
00037 *    COPY ELCSCTM.
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
00038
00039 *    COPY ELCSCRTY.
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
00040
012407 01  WS-BIN-PROOF-DT             PIC XX  VALUE LOW-VALUES.
00041  01  WS-DATE-AREA.
00042      12  SAVE-DATE           PIC X(8)    VALUE SPACES.
00043      12  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00044      12  SAVE-DATE-CCYYMMDD.
00045          16  SAVE-DATE-CC            PIC XX      VALUE SPACES.
00046          16  SAVE-DATE-YMD.
00047              20  SAVE-DATE-YY        PIC XX      VALUE SPACES.
00048              20  FILLER              PIC X(04)   VALUE SPACES.
00049
       01  WS-SAVED-EOB-CODES          PIC X(60) VALUE SPACES.
00050  01  STANDARD-AREAS.
CIDMOD     12  WS-RESPONSE         PIC S9(8)   COMP.
CIDMOD         88  WS-RESP-NORMAL              VALUE +00.
CIDMOD         88  WS-RESP-ERROR               VALUE +01.
CIDMOD         88  WS-RESP-NOTFND              VALUE +13.
022106         88  WS-RESP-DUPREC              VALUE +14.
               88  WS-RESP-ENDFILE             VALUE +20.
00051      12  PMT-HEAD-A.
00052          16  FILLER          PIC X(09)   VALUE 'PAY-FROM '.
00053          16  HDGA-VRBLE      PIC X(10)   VALUE 'PAY THRU  '.
00054          16  FILLER          PIC X(47)   VALUE 'DAYS   PAYMENT AMT
00055 -        '     RESERVES  TYPE    AMOUNT'.
00056      12  PMT-HEAD-B.
00057          16  FILLER          PIC X(12)   VALUE 'PAYS FROM   '.
00058          16  HDGB-VRBLE      PIC X(12)   VALUE 'PAYS THRU   '.
00059          16  FILLER          PIC X(10)   VALUE 'DAYS   BY '.
00060      12  SC-ITEM             PIC S9(4)   VALUE +0001  COMP.
00061      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
00062      12  MAP-NAME            PIC X(8)    VALUE 'EL156A'.
00063      12  MAP-NAMEA           PIC X(8)    VALUE 'EL156A'.
00064      12  MAP-NAMEB           PIC X(8)    VALUE 'EL156B'.
00065      12  MAPSET-NAME         PIC X(8)    VALUE 'EL156S'.
00066      12  TRANS-ID            PIC X(4)    VALUE 'EX29'.
00067      12  THIS-PGM            PIC X(8)    VALUE 'EL156'.
00068      12  PGM-NAME            PIC X(8).
00069      12  TIME-IN             PIC S9(7).
00070      12  TIME-OUT-R  REDEFINES TIME-IN.
00071          16  FILLER          PIC X.
00072          16  TIME-OUT        PIC 99V99.
00073          16  FILLER          PIC XX.
00074      12  XCTL-005            PIC X(8)    VALUE 'EL005'.
00075      12  XCTL-010            PIC X(8)    VALUE 'EL010'.
00076      12  XCTL-126            PIC X(8)    VALUE 'EL126'.
00077      12  XCTL-141            PIC X(8)    VALUE 'EL141'.
00078      12  XCTL-157            PIC X(8)    VALUE 'EL157'.
00079      12  XCTL-155            PIC X(8)    VALUE 'EL155'.
00080      12  XCTL-130            PIC X(8)    VALUE 'EL130'.
00081      12  XCTL-132            PIC X(8)    VALUE 'EL132'.
00082      12  LINK-001            PIC X(8)    VALUE 'EL001'.
00083      12  LINK-004            PIC X(8)    VALUE 'EL004'.
00084      12  LINK-1523           PIC X(8)    VALUE 'EL1523'.
00085      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00086      12  LINK-REMTERM        PIC X(8)    VALUE 'ELRTRM'.
00087      12  LINK-REMAMT         PIC X(8)    VALUE 'ELRAMT'.
00088      12  ELARCH-LENGTH       PIC S9(4)   VALUE +90     COMP.
CIDMOD     12  WS-BLANK            PIC X       VALUE ' '.
00089
00090  01  MISC-WORK-AREAS.
00091      12  W-INTEREST              PIC S9(7)V99  COMP-3 VALUE +0.
00092      12  W-PAYMENT-TOTAL         PIC S9(7)V99  COMP-3 VALUE +0.
00093      12  W-PERCENT               PIC S9V9(4)   COMP-3 VALUE +0.
00094      12  W-TOTAL-INTEREST        PIC S9(7)V99  COMP-3 VALUE +0.
00095      12  W-PAID-DAYS             PIC S999      COMP-3 VALUE +0.
00096      12  W-PAID-MONTHS           PIC S999      COMP-3 VALUE +0.
00097      12  W-REM                   PIC S999      COMP-3 VALUE +0.
00098      12  W-REM-DAYS              PIC S999      COMP-3 VALUE +0.
00099      12  W-TERM-IN-DAYS          PIC S9(4)     COMP-3.
00100      12  W-REM-TERM-IN-DAYS      PIC S9(4)     COMP-3.
00101      12  W-CALLED-NAME           PIC X(08).
00102      12  W-EOB-CODES.
00103          16  W-EOB-CODE-GRP OCCURS 5 TIMES.
00104              20  W-EOB-CODE      PIC XXX.
00105              20  W-EOB-FILLER    PIC X.
00106      12  W-EOB-NDX               PIC S9        VALUE ZERO.
00107      12  W-NAME-LAST             PIC X(15).
00108      12  W-NAME-FIRST            PIC X(15).
00109      12  W-NAME-MIDDLE.
00110          16  FILLER              PIC X.
00111          16  W-NAME-MIDDLE-2     PIC X.
00112          16  FILLER              PIC X(13).
00113      12  W-DMD-CHECK-NO.
00114          16  W-DMD-CHECK-NO-1    PIC X(4).
00115          16  W-DMD-CHECK-NO-2    PIC X.
00116          16  W-DMD-CHECK-NO-3    PIC XX.
00117
00118      12  WS-COMPUTE-FROM-DT  PIC XX.
00119      12  WS-PMT-MOS          PIC 999V99        VALUE ZEROS.
00120      12  WS-HER-PART-DAYS    PIC 999V99        VALUE ZEROS.
00121      12  WS-HER-MO-PMT-AMT   PIC 9(07)V99      VALUE ZEROS.
00122      12  WS-FORM-CTL-SEQ-NO  PIC S9(4)   COMP  VALUE +0.
00123      12  WS-HOLD-ELMSTR-KEY  PIC X(20) VALUE LOW-VALUES.
00124      12  WS-PRINTED-SW       PIC X VALUE SPACES.
00125          88  PAYMENT-HAS-BEEN-PRINTED  VALUE 'Y'.
00126          88  PAYMENT-NOT-PRINTED       VALUE 'N'.
00127      12  WS-RELEASED-SW      PIC X VALUE SPACES.
00128          88  PAYMENT-NOT-RELEASED      VALUE 'N'.
00129      12  WS-UPDATE-SW        PIC X VALUE ' '.
00130      12  WS-GROUPING.
00131          16  WS-GROUP-1      PIC X VALUE SPACES.
00132          16  FILLER          PIC X(05) VALUE SPACES.
00133      12  WS-BROWSE-TRLR-SW   PIC X VALUE ' '.
00134      12  WS-BROWSE-START-SW  PIC X VALUE ' '.
00135      12  WS-WORK-STATE-ACCT.
00136          16  WS-WORK-ACCT    PIC X(10)   VALUE SPACES.
00137          16  FILLER          PIC X       VALUE '/'.
00138          16  WS-WORK-STATE   PIC XX      VALUE SPACES.
00139      12  WS-CHECK-WRITTEN-DT PIC XX      VALUE LOW-VALUES.
00140      12  WS-PMT-APPROVAL-SW  PIC X       VALUE SPACES.
00141      12  WS-SPECIAL-CALC-CD  PIC X       VALUE SPACES.
00142      12  QID.
00143          16  QID-TERM        PIC X(4)    VALUE SPACES.
00144          16  FILLER          PIC X(4)    VALUE '156A'.
00145      12  MAP-LENGTH          PIC S9(4)   VALUE +1098 COMP.
00146      12  SUB-1               PIC S9(4)   VALUE +0     COMP.
00147      12  FIRST-ENTRY         PIC X       VALUE 'N'.
00148      12  FILE-SWITCH         PIC X(4)    VALUE SPACES.
00149      12  BENE-FOUND-SW       PIC X       VALUE SPACE.
00150      12  FOUND-TOL-SW        PIC X       VALUE 'N'.
00151      12  RETURNED-FROM-B     PIC X       VALUE 'N'.
00152      12  RETURNED-FROM       PIC X(8)    VALUE SPACES.
00153      12  WS-STATUS           PIC X       VALUE SPACES.
00154      12  WS-TODAY-DATE       PIC XX      VALUE LOW-VALUES.
00155      12  WS-RETRO-ELIM-DATE  PIC XX      VALUE LOW-VALUES.
00156      12  WS-EXP-DT           PIC XX      VALUE LOW-VALUES.
           12  ws-extended-exp-dt  pic xx      value low-values.
00157      12  WS-SV-EPYFROM       PIC XX      VALUE LOW-VALUES.
00158      12  WS-SV-AIGFROM       PIC XX      VALUE LOW-VALUES.
00159      12  WS-SV-EPYTHRU       PIC XX      VALUE LOW-VALUES.
00160      12  WS-SV-PREV-PD-THRU  PIC XX      VALUE LOW-VALUES.
00161      12  WS-STATE-ABBREV     PIC XX      VALUE SPACES.
00162      12  WS-BEN-DAYS         PIC 99      VALUE ZEROS.
00163      12  WS-PMT-UNAPPROVED-CNT PIC S9(3)   VALUE +0      COMP-3.
00164      12  WS-DAYS-TOL         PIC S9(3)     VALUE +0      COMP-3.
00165      12  WS-PMT-TOL          PIC S9(3)V99  VALUE +0      COMP-3.
00166      12  WS-MAX-DAYS-TOL     PIC S9(3)     VALUE +0      COMP-3.
00167      12  WS-MAX-PMT-TOL      PIC S9(7)V99  VALUE +0      COMP-3.
00168      12  WS-MAX-LF-PMT-TOL   PIC S9(7)V99  VALUE +0      COMP-3.
00169      12  WS-AH-BEN-AMT       PIC S9(7)V999 VALUE +0      COMP-3.
030514     12  WS-LF-BEN-AMT       PIC S9(9)V99  VALUE +0      COMP-3.
00170      12  WS-BEN-AMT-LEFT     PIC S9(7)V99  VALUE +0.
00171      12  WS-REMAINING-AMT    PIC S9(9)V99  VALUE +0.
00172      12  WS-TOL-SEVERITY     PIC X         VALUE ' '.
00173      12  WS-LF-COVERAGE-TYPE PIC X         VALUE ' '.
00174      12  WS-EARNING-METHOD   PIC X         VALUE ' '.
00175      12  WS-CALC-METHOD      PIC X         VALUE ' '.
00176      12  WS-PAY-TYPE         PIC X         VALUE ' '.
00177      12  EXPENSE-MONTH-SAVE      PIC S9(4)     COMP-3 VALUE +0.
00178      12  WS-RESERVE-WORK         PIC S9(6)V99  COMP-3 VALUE +0.
012810     12  WS-EDIT-PATTERN         PIC Z(7).99  VALUE ZEROS.
012810     12  WS-EDIT REDEFINES WS-EDIT-PATTERN PIC X(10).
00181      12  WORK-DATE-FROM.
00182          16  WDF-MONTH       PIC 99        VALUE ZERO.
00183          16  WDF-DAY         PIC 99        VALUE ZERO.
00184          16  WDF-YEAR        PIC 99        VALUE ZERO.
00185      12  WORK-DATE-THRU.
00186          16  WDT-MONTH       PIC 99        VALUE ZERO.
00187          16  WDT-DAY         PIC 99        VALUE ZERO.
00188          16  WDT-YEAR        PIC 99        VALUE ZERO.
00189      12  FROM-DAYS-IN-MONTH  PIC 99        VALUE ZERO.
00190      12  THRU-DAYS-IN-MONTH  PIC 99        VALUE ZERO.
00191      12  FROM-PMT-DAYS       PIC 99        VALUE ZERO.
00192      12  THRU-PMT-DAYS       PIC 99        VALUE ZERO.
00193      12  SAVE-ELAPSED-MONTHS PIC 999       VALUE ZERO.
00194      12  SAVE-ELAPSED-DAYS   PIC 9(04)     VALUE ZERO.
00195      12  WS-SAVE-ODD-DAYS    PIC S9(04)    VALUE ZERO.
00196      12  SAVE-ODD-DAYS-OVER  PIC 999       VALUE ZERO.
00197      12  WS-DAILY-RATE       PIC S999V99999  VALUE +0.
00198      12  WS-PY-WORK          PIC S9(8)V99 VALUE +0.
00199      12  WS-PY-WK REDEFINES WS-PY-WORK.
00200          16  WS-PY-NUM       PIC S9(7)V99.
00201          16  WS-PY-SIGN      PIC X.
00202      12  WS-DY-WORK          PIC S9(5).
00203      12  WS-DY-WK REDEFINES WS-DY-WORK.
00204          16  WS-DY-NUM       PIC S9(4).
00205          16  WS-DY-SIGN      PIC X.
00206      12  WS-COMP-MSTR-SW     PIC X       VALUE 'N'.
00207          88  COMP-REC-FOUND              VALUE 'Y'.
00208      12  WS-BEN-SEARCH-SW    PIC X       VALUE 'N'.
00209          88  BENEFIT-FOUND       VALUE 'Y'.
00210          88  NO-BENEFIT-FOUND    VALUE 'N'.
00211      12  WS-TERMS.
041812         16  WST-ORIG        PIC ZZ9     VALUE ZEROS.
00213          16  FILLER          PIC X       VALUE '/'.
041812         16  WST-REM         PIC ZZ9     VALUE ZEROS.
00215          16  WST-REM-DAYS-GRP.
00216              20  WST-SLASH   PIC X       VALUE '/'.
00217              20  WST-REM-DAYS
00218                              PIC --9.
00219      12  WS-ACCESS.
00220          16  FILLER          PIC XX      VALUE SPACES.
00221          16  WS-BEN-CD       PIC XX      VALUE SPACES.
00222      12  WS-STATE-ACCESS.
00223          16  WS-ST-ACCESS    PIC XX      VALUE SPACES.
00224          16  FILLER          PIC XX      VALUE SPACES.
00225      12  WS-CARR-ACCESS.
00226          16  FILLER          PIC X(3)    VALUE SPACES.
00227          16  WS-CARR         PIC X       VALUE SPACES.
00228      12  DATE-WORK.
00229          16  FILLER          PIC XX      VALUE SPACES.
00230          16  NUM-WORK        PIC X(6)    VALUE SPACES.
00231      12  WS-SAVE-INPUT.
00232          16  WS-PMTTYPE      PIC X       VALUE SPACE.
00233          16  WS-PAYEE        PIC XX      VALUE SPACE.
00234          16  WS-PMTNOTE1     PIC X(60)   VALUE SPACES.
00235          16  WS-PMTNOTE2     PIC X(60)   VALUE SPACES.
00236          16  WS-OFFLINE      PIC X       VALUE SPACE.
00237          16  WS-CHECKNO      PIC X(7)    VALUE SPACES.
00238          16  WS-HOLDTIL      PIC 9(6)    VALUE 0.
00239          16  WS-EPYFROM      PIC 9(6)    VALUE 0.
00240          16  WS-EPYTHRU      PIC 9(6)    VALUE 0.
00241          16  WS-EDAYS        PIC S9(5)    VALUE +0.
00242          16  WS-EPYAMT       PIC S9(7)V99 VALUE +0.
00243          16  WS-ERESV        PIC S9(7)V99 VALUE +0.
00244          16  WS-EEXPENS      PIC 9(6)V99  VALUE 0.
00245          16  WS-ETYPE        PIC X       VALUE SPACE.
00246          16  WS-CASH         PIC X       VALUE SPACE.
00247          16  WS-GROUPED      PIC X       VALUE SPACE.
00248          16  WS-INT-RATE     PIC S99V9(5) VALUE +0.
00249          16  WS-AIGFROM      PIC 9(6)    VALUE 0.
00250          16  WS-LOAN-NO      PIC X(20)   VALUE SPACES.
052506         16  WS-PROOF-DATE   PIC 9(6)    VALUE 0.
               16  WS-PRINT-EOB-YN PIC X       VALUE ' '.
020413         16  WS-PRINT-CLM-FRM-YN PIC X   VALUE SPACE.
020413         16  WS-PRINT-SURVEY-YN PIC X    VALUE SPACE.
102413         16  WS-SPECIAL-RELEASE-YN PIC X VALUE SPACE.
120115         16  ws-int-to-rem-borr pic x value spaces.
00251      12  LAST-ERROR-LINE.
00252          16  WS-FATAL-CTR        PIC ZZ9  VALUE ZEROS.
00253          16  FILLER              PIC X(30)
00254              VALUE ' FATAL ERRORS ENCOUNTERED'.
00255          16  WS-FORCE-CTR        PIC ZZ9   VALUE ZEROS.
00256          16  FILLER              PIC X(28)
00257              VALUE ' FORCIBLE ERRORS ENCOUNTERED'.
00258      12  WS-CHECK-AREA.
00259          16  WS-CHECK-CARR       PIC X   VALUE SPACES.
00260          16  WS-CHECK-NUM        PIC 9(6) VALUE ZEROS.
00261      12  WS-CHECK-NUMERIC REDEFINES WS-CHECK-AREA PIC 9(7).
00262      12  WS-CHECK-AREA-RDF REDEFINES WS-CHECK-AREA.
00263          16  FILLER              PIC XX.
00264          16  WS-A-CHECK-NUM-5    PIC 9(5).
00265      12  WS-CHECK-AREA-RDF1 REDEFINES WS-CHECK-AREA.
00266          16  FILLER              PIC X.
00267          16  WS-A-CHECK-NUM-6    PIC 9(6).
00268      12  OPTION-1-SAVE-AREA.
00269          16  SAVE1-FROM-DAYS-IN-MONTH    PIC 999  VALUE ZEROS.
00270          16  SAVE1-THRU-DAYS-IN-MONTH    PIC 999  VALUE ZEROS.
00271      12  WS-WORK-ZIP.
00272          16  WS-ZIP              PIC X(5)  VALUE ZEROS.
00273          16  WS-ZIP-EXT          PIC X(4)  VALUE ZEROS.
00274      12  WS-NUMERIC-ZIP REDEFINES WS-WORK-ZIP
00275                                  PIC 9(9).
00276      12  WS-FLI-PMTNOTE.
00277          16  WS-PMTOPTION        PIC XX.
00278          16  WS-DOCNAME          PIC X(28).
00279          16  WS-AMOUNT-LINE REDEFINES WS-DOCNAME.
00280              20  WS-AMOUNT       PIC X(8).
00281              20  FILLER          PIC X(20).
00282          16  WS-NUMERIC-LINE REDEFINES WS-DOCNAME.
00283              20  WS-NUM-AMT      PIC 9(6)V99.
00284              20  FILLER          PIC X(20).
00285
00286      12  WS-HAN-PMTNOTE.
00287          16  WS-HAN-PMT-CODE     PIC X.
00288          16  WS-HAN-PMT-NOTE     PIC X(39).
00289
00290      12  WS-NCL-PMT-OPTION       PIC XX.
00291          88  WS-VALID-NCL-OPTION           VALUE '00' THRU '19'.
00292
00293      12  WS-CLAIM-SEQUENCE.
00294          16  FILLER              PIC X VALUE '('.
00295          16  WS-CUR-SEQU         PIC Z9    VALUE ZEROS.
00296          16  FILLER              PIC X(04) VALUE ' OF '.
00297          16  WS-OF-SEQU          PIC Z9    VALUE ZEROS.
00298          16  FILLER              PIC X VALUE ')'.
00299
00300      12  WS-REFUND-AMT           PIC Z(7).99.
00301      12  WS-CALC-INT             PIC S9(7)V99.
00302      12  ws-benefits-paid     PIC S9(4)V999 VALUE +0  COMP-3.
00303
00304      12  SUB                     PIC 99 VALUE ZEROS.
00305
00306      12  WS-BOANOTE1.
00307          16  WS-BOANOTE1-DESC    PIC X(11).
00308          16  WS-BOANOTE1-EPYAMT  PIC Z,ZZZ,ZZZ.99.
00309          16  FILLER              PIC X(17).
00310      12  WS-BOANOTE2.
00311          16  WS-BOANOTE2-DESC    PIC X(11).
00312          16  WS-BOANOTE2-INT     PIC Z,ZZZ,ZZZ.99.
00313          16  FILLER              PIC X(17).
00314
00315      12  WS-NCLNOTE1.
00316          16  WS-NCLNOTE1-DESC    PIC X(11).
00317          16  WS-NCLNOTE1-CPYAMT  PIC Z,ZZZ,ZZZ.99.
00318          16  FILLER              PIC X(17).
00319      12  WS-NCLNOTE2.
00320          16  WS-NCLNOTE2-DESC    PIC X(11).
00321          16  WS-NCLNOTE2-INT     PIC Z,ZZZ,ZZZ.99.
00322          16  FILLER              PIC X(17).
00323
052506     12  WS-MAX-LETTER-ANSWER-DT PIC XX         VALUE LOW-VALUES.
031808     12  WS-SAVE-APPROVAL-LEVEL-REQD  PIC X.
031808     12  WS-SAVE-APPROVED-LEVEL  PIC X.
00324      12  WS-MAX-EXP-PMT          PIC S9(07)V99  VALUE +0.
00325      12  WS-ACTIVITY-CODE        PIC 99         VALUE ZEROS.
00326      12  WS-LETTER-SW            PIC X          VALUE 'N'.
00327      12  WS-ACT-REC-FOUND-SW     PIC X          VALUE 'N'.
00328      12  WS-BROWSE-SW            PIC X          VALUE 'N'.
00329      12  WS-APPROVAL-LEVEL       PIC X          VALUE ' '.
00330      12  WS-OPEN-CLOSE-SW        PIC X          VALUE ' '.
00331
00332      12  WS-AIG-WORK-AREAS.
00333          16  WS-RECALC-PAYFROM-SW  PIC X VALUE 'N'.
00334              88  WS-AIG-RECALC-PAYFROM   VALUE 'Y'.
00335          16  WS-AIG-PAYFROM-SW PIC X     VALUE 'N'.
00336              88  WS-USE-AIG-PAYFROM      VALUE 'Y'.
00337          16  WS-FROM-DATE-MDY.
00338              20  WS-FROM-MO      PIC 99.
00339              20  WS-FROM-DA      PIC 99.
00340              20  WS-FROM-YR      PIC 99.
00341          16  WS-TEMP-DATE-MDY.
00342              20  WS-TEMP-MO      PIC 99.
00343              20  WS-TEMP-DA      PIC 99.
00344              20  WS-TEMP-YR      PIC 99.
00345          16  WS-BIN-FROM-DT      PIC XX.
00346          16  WS-TEMP-INC-MDY.
00347              20  WS-TEMP-INC-MO  PIC 99.
00348              20  WS-TEMP-INC-DA  PIC 99.
00349              20  WS-TEMP-INC-YR  PIC 99.
00350          16  WS-PMT-DUE-MDY.
00351              20  WS-PMT-DUE-MO   PIC 99.
00352              20  WS-PMT-DUE-DA   PIC 99.
00353              20  WS-PMT-DUE-YR   PIC 99.
00354          16  WS-PMT-DUE-DT       PIC XX.
00355          16  WS-NEXT-PMT-DUE-DT  PIC XX.
00356          16  WS-RETRO-ELIM-DT    PIC XX.
00357          16  WS-DUE-DAY-SW               PIC X       VALUE 'N'.
00358              88 WS-RESTORE-DUE-DAY                   VALUE 'Y'.
00359          16  WS-SAVE-DUE-DA      PIC 99.
00360          16  WS-CDAYS            PIC S9(05).
00361          16  WS-DAYS-OFF         PIC S9(05).
00362          16  WS-ELIMINATION-SW   PIC X     VALUE 'N'.
00363              88  ELIM-SATISFIED            VALUE 'Y'.
00364          16  WS-AIG-CHECKNO.
00365              20  WS-A-CHECKNO-1-2        PIC XX      VALUE SPACES.
00366              20  WS-A-CHECKNO-3-7.
00367                  24  FILLER              PIC X(3)    VALUE SPACES.
00368                  24  WS-A-CHECKNO-6-7    PIC XX      VALUE SPACES.
00369                      88  WS-AIG-CREDIT-ENTERED       VALUE 'CR'.
00370          16  WS-AIG-GROUPING.
00371              20  WS-A-BENE               PIC XXX.
00372              20  FILLER                  PIC XXX.
00373          16  WS-PAYMENT-TRAILER-SW       PIC X       VALUE ' '.
00374              88  WS-1ST-SPECIAL-PMT                  VALUE '1'.
00375              88  WS-2ND-SPECIAL-PMT                  VALUE '2'.
00376          16  WS-AIG-UNEMPLY-DESC         PIC X(60)
00377                  VALUE 'UNEMPLOYMENT PAYMENT'.
00378          16  WS-CHK-THRU-DT              PIC XX.
00379          16  WS-HOLD-BIN-DATE-1          PIC XX.
00380
031808 01  MISC-CSO-AREA.
           12  ELCRTT-LENGTH       PIC S9(04)    COMP     VALUE +552.
041309     12  ELCRTT-DSID         PIC X(8)    VALUE 'ELCRTT'.
031808     12  WS-NEED-APPROVAL                PIC X(1)   VALUE 'N'.
031808         88  APPROVAL-NEEDED                        VALUE 'Y'.
031808     12  WS-APPROV-NOTE                  PIC X(43)
031808         VALUE 'REFERRED FOR CLAIM PAYMENT APPROVAL REVIEW.'.
052814     12  WS-3550-APPROV-NOTE             PIC X(43)
052814         VALUE 'APPROVL NEEDED-DOES CLAIMANT HAVE COVERAGE?'.
043019     12  WS-DUPE-APPROV-NOTE             PIC X(43)
043019         VALUE 'APPROVAL NEEDED - DUPLICATE PAYMENT EXISTS '.
031808     12  WS-REAUDIT-NOTE                 PIC X(43)
031808         VALUE 'REFERRED FOR RANDOM AUDIT APPROVAL REVIEW. '.
031808     12  WS-CALC-PMT-MONTHS      PIC S9(2)V9(8)  VALUE +0.
031808     12  WS-CALC-MO-AH-AMT       PIC S9(07)V99   VALUE +0.
091808     12  WS-SUB                  PIC S9(3) COMP-3 VALUE +0.
091808     12  WS-BEG-SUB              PIC S9(3) COMP-3 VALUE +0.
091808     12  WS-END-SUB              PIC S9(3) COMP-3 VALUE +0.
091808     12  WS-STATE-LENGTH         PIC S9(3) COMP-3 VALUE +0.
091808     12  WS-PAYEE-STATE-FOUND    PIC X(01)        VALUE 'N'.
091808         88 PAYEE-STATE-FOUND                     VALUE 'Y'.
091808     12  WS-PAYEE-CITY-STATE     PIC X(30).
091808     12  FILLER  REDEFINES WS-PAYEE-CITY-STATE.
091808         16  WS-PAYEE-CITY-ST OCCURS 30 TIMES PIC X(1).
112210     12  WS-PD-DAYS              PIC S9(5) VALUE +0.
020513     12  WS-SUB2                 PIC S9(3) COMP-3 VALUE +0.
020513     12  WS-WORK-NAME            PIC X(30).
020513     12  FILLER  REDEFINES WS-WORK-NAME.
020513         16  WS-WORK-NAME-X OCCURS 30 TIMES PIC X(1).
020513     12  WS-INSURED-NAME         PIC X(30).
020513     12  FILLER  REDEFINES WS-INSURED-NAME.
020513         16  WS-INSURED-NAME-X OCCURS 30 TIMES PIC X(1).
020513     12  WS-PAYEE-NAME           PIC X(30).
020513     12  FILLER  REDEFINES WS-PAYEE-NAME.
020513         16  WS-PAYEE-NAME-X OCCURS 30 TIMES PIC X(1).
022718     12  WS-HOLD-UNTIL-SW        PIC X VALUE SPACES.
022718         88  HOLD-UNTIL-CHK            VALUE 'Y'.
031218         88  HOLD-UNTIL-PMT            VALUE 'P'.
022718     12  WS-HOLD-UNTIL-DT        PIC X(02) VALUE ZERO.
040819     12  WS-UNPAID-HOLD-UNTIL-SW PIC X VALUE SPACES.
040819         88  UNPAID-HOLD-UNTIL-EXISTS  VALUE 'Y'.
040819     12  WS-DUPE-VOID-PMT-SW     PIC X VALUE SPACES.
040819         88  DUPE-VOID-PMT-EXISTS      VALUE 'Y'.
040819     12  WS-CHECK-ALL-PMTS-SW    PIC X VALUE SPACES.
040819         88  CHECK-ALL-PMTS            VALUE 'Y'.
040819     12  WS-TOT-AMOUNT-PAID      PIC S9(7)V99  COMP-3 VALUE ZERO.
040819     12  WS-UNPD-HOLD-UNTIL-DT   PIC X(02) VALUE ZERO.
031808
00381  01  ERROR-MESSAGES.
00382      12  ER-0000                 PIC X(4)  VALUE '0000'.
00383      12  ER-0004                 PIC X(4)  VALUE '0004'.
00384      12  ER-0008                 PIC X(4)  VALUE '0008'.
00385      12  ER-0019                 PIC X(4)  VALUE '0019'.
00386      12  ER-0029                 PIC X(4)  VALUE '0029'.
00387      12  ER-0042                 PIC X(4)  VALUE '0042'.
00388      12  ER-0070                 PIC X(4)  VALUE '0070'.
00389      12  ER-0130                 PIC X(4)  VALUE '0130'.
00390      12  ER-0149                 PIC X(4)  VALUE '0149'.
00391      12  ER-0154                 PIC X(4)  VALUE '0154'.
00392      12  ER-0168                 PIC X(4)  VALUE '0168'.
00393      12  ER-0169                 PIC X(4)  VALUE '0169'.
00394      12  ER-0172                 PIC X(4)  VALUE '0174'.
00395      12  ER-0198                 PIC X(4)  VALUE '0198'.
00396      12  ER-0204                 PIC X(4)  VALUE '0204'.
00397      12  ER-0206                 PIC X(4)  VALUE '0206'.
00398      12  ER-0252                 PIC X(4)  VALUE '0252'.
00399      12  ER-0254                 PIC X(4)  VALUE '0254'.
00400      12  ER-0282                 PIC X(4)  VALUE '0282'.
00401      12  ER-0283                 PIC X(4)  VALUE '0283'.
00402      12  ER-0294                 PIC X(4)  VALUE '0294'.
00403      12  ER-0338                 PIC X(4)  VALUE '0338'.
           12  ER-0419                 PIC X(4)  VALUE '0419'.
00404      12  ER-0420                 PIC X(4)  VALUE '0420'.
00405      12  ER-0421                 PIC X(4)  VALUE '0421'.
00406      12  ER-0422                 PIC X(4)  VALUE '0422'.
00407      12  ER-0433                 PIC X(4)  VALUE '0433'.
00408      12  ER-0436                 PIC X(4)  VALUE '0436'.
00409      12  ER-0437                 PIC X(4)  VALUE '0437'.
00410      12  ER-0438                 PIC X(4)  VALUE '0438'.
00411      12  ER-0439                 PIC X(4)  VALUE '0439'.
00412      12  ER-0440                 PIC X(4)  VALUE '0440'.
00413      12  ER-0441                 PIC X(4)  VALUE '0441'.
00414      12  ER-0442                 PIC X(4)  VALUE '0442'.
00415      12  ER-0443                 PIC X(4)  VALUE '0443'.
00416      12  ER-0444                 PIC X(4)  VALUE '0444'.
00417      12  ER-0445                 PIC X(4)  VALUE '0445'.
00418      12  ER-0446                 PIC X(4)  VALUE '0446'.
00419      12  ER-0447                 PIC X(4)  VALUE '0447'.
00420      12  ER-0448                 PIC X(4)  VALUE '0448'.
00421      12  ER-0449                 PIC X(4)  VALUE '0449'.
00422      12  ER-0452                 PIC X(4)  VALUE '0452'.
00423      12  ER-0458                 PIC X(4)  VALUE '0458'.
00424      12  ER-0459                 PIC X(4)  VALUE '0459'.
00425      12  ER-0460                 PIC X(4)  VALUE '0460'.
00426      12  ER-0461                 PIC X(4)  VALUE '0461'.
00427      12  ER-0462                 PIC X(4)  VALUE '0462'.
00428      12  ER-0463                 PIC X(4)  VALUE '0463'.
00429      12  ER-0464                 PIC X(4)  VALUE '0464'.
00430      12  ER-0465                 PIC X(4)  VALUE '0465'.
00431      12  ER-0466                 PIC X(4)  VALUE '0466'.
00432      12  ER-0467                 PIC X(4)  VALUE '0467'.
00433      12  ER-0468                 PIC X(4)  VALUE '0468'.
00434      12  ER-0469                 PIC X(4)  VALUE '0469'.
00435      12  ER-0473                 PIC X(4)  VALUE '0473'.
00436      12  ER-0474                 PIC X(4)  VALUE '0474'.
00437      12  ER-0475                 PIC X(4)  VALUE '0475'.
00438      12  ER-0476                 PIC X(4)  VALUE '0476'.
00439      12  ER-0477                 PIC X(4)  VALUE '0477'.
00440      12  ER-0478                 PIC X(4)  VALUE '0478'.
00441      12  ER-0479                 PIC X(4)  VALUE '0479'.
00442      12  ER-0481                 PIC X(4)  VALUE '0481'.
00443      12  ER-0491                 PIC X(4)  VALUE '0491'.
00444      12  ER-0492                 PIC X(4)  VALUE '0492'.
00445      12  ER-0493                 PIC X(4)  VALUE '0493'.
00446      12  ER-0494                 PIC X(4)  VALUE '0494'.
00447      12  ER-0495                 PIC X(4)  VALUE '0495'.
00448      12  ER-0499                 PIC X(4)  VALUE '0499'.
00449      12  ER-0500                 PIC X(4)  VALUE '0500'.
00450      12  ER-0501                 PIC X(4)  VALUE '0501'.
00451      12  ER-0502                 PIC X(4)  VALUE '0502'.
00452      12  ER-0508                 PIC X(4)  VALUE '0508'.
00453      12  ER-0513                 PIC X(4)  VALUE '0513'.
00454      12  ER-0518                 PIC X(4)  VALUE '0518'.
00455      12  ER-0524                 PIC X(4)  VALUE '0524'.
00456      12  ER-0525                 PIC X(4)  VALUE '0525'.
00457      12  ER-0526                 PIC X(4)  VALUE '0526'.
00458      12  ER-0530                 PIC X(4)  VALUE '0530'.
00459      12  ER-0540                 PIC X(4)  VALUE '0540'.
00460      12  ER-0541                 PIC X(4)  VALUE '0541'.
00461      12  ER-0542                 PIC X(4)  VALUE '0542'.
00462      12  ER-0543                 PIC X(4)  VALUE '0543'.
00463      12  ER-0545                 PIC X(4)  VALUE '0545'.
00464      12  ER-0552                 PIC X(4)  VALUE '0552'.
00465      12  ER-0555                 PIC X(4)  VALUE '0555'.
00466      12  ER-0556                 PIC X(4)  VALUE '0556'.
00467      12  ER-0557                 PIC X(4)  VALUE '0557'.
00468      12  ER-0572                 PIC X(4)  VALUE '0572'.
00469      12  ER-0573                 PIC X(4)  VALUE '0573'.
00470      12  ER-0594                 PIC X(4)  VALUE '0594'.
00471      12  ER-0616                 PIC X(4)  VALUE '0616'.
00472      12  ER-0617                 PIC X(4)  VALUE '0617'.
00473      12  ER-0618                 PIC X(4)  VALUE '0618'.
00474      12  ER-0657                 PIC X(4)  VALUE '0657'.
00475      12  ER-0658                 PIC X(4)  VALUE '0658'.
00476      12  ER-0699                 PIC X(4)  VALUE '0699'.
00477      12  ER-0730                 PIC X(4)  VALUE '0730'.
00478      12  ER-0737                 PIC X(4)  VALUE '0737'.
00479      12  ER-0760                 PIC X(4)  VALUE '0760'.
00480      12  ER-0768                 PIC X(4)  VALUE '0768'.
00481      12  ER-0769                 PIC X(4)  VALUE '0769'.
00482      12  ER-0770                 PIC X(4)  VALUE '0770'.
00483      12  ER-0802                 PIC X(4)  VALUE '0802'.
00484      12  ER-0803                 PIC X(4)  VALUE '0803'.
00485      12  ER-0819                 PIC X(4)  VALUE '0819'.
00486      12  ER-0834                 PIC X(4)  VALUE '0834'.
00487      12  ER-0836                 PIC X(4)  VALUE '0836'.
00488      12  ER-0844                 PIC X(4)  VALUE '0844'.
00489      12  ER-0845                 PIC X(4)  VALUE '0845'.
00490      12  ER-0846                 PIC X(4)  VALUE '0846'.
00491      12  ER-0849                 PIC X(4)  VALUE '0849'.
00492      12  ER-0851                 PIC X(4)  VALUE '0851'.
052506     12  ER-0872                 PIC X(4)  VALUE '0872'.
052506     12  ER-0873                 PIC X(4)  VALUE '0873'.
022106     12  ER-0874                 PIC X(4)  VALUE '0874'.
041710     12  ER-0879                 PIC X(4)  VALUE '0879'.
00493      12  ER-0919                 PIC X(4)  VALUE '0919'.
00494      12  ER-0921                 PIC X(4)  VALUE '0921'.
00495      12  ER-0923                 PIC X(4)  VALUE '0923'.
00496      12  ER-0925                 PIC X(4)  VALUE '0925'.
00497      12  ER-0944                 PIC X(4)  VALUE '0944'.
00498      12  ER-0945                 PIC X(4)  VALUE '0945'.
00499      12  ER-0946                 PIC X(4)  VALUE '0946'.
00500      12  ER-0947                 PIC X(4)  VALUE '0947'.
00501      12  ER-0948                 PIC X(4)  VALUE '0948'.
00502      12  ER-0949                 PIC X(4)  VALUE '0949'.
00503      12  ER-0950                 PIC X(4)  VALUE '0950'.
00504      12  ER-0951                 PIC X(4)  VALUE '0951'.
00505      12  ER-0954                 PIC X(4)  VALUE '0954'.
00506      12  ER-0956                 PIC X(4)  VALUE '0956'.
00507      12  ER-0967                 PIC X(4)  VALUE '0967'.
00508      12  ER-0968                 PIC X(4)  VALUE '0968'.
00509      12  ER-0974                 PIC X(4)  VALUE '0974'.
00510      12  ER-0975                 PIC X(4)  VALUE '0975'.
           12  ER-1561                 PIC X(4)  VALUE '1561'.
020413     12  ER-1566                 PIC X(4)  VALUE '1566'.
020413     12  ER-1567                 PIC X(4)  VALUE '1567'.
102413     12  ER-1569                 PIC X(4)  VALUE '1569'.
120115     12  er-1582                 pic x(4)  value '1582'.
           12  er-1657                 pic x(4)  value '1657'.
           12  er-1658                 pic x(4)  value '1658'.
           12  er-1667                 pic x(4)  value '1667'.
           12  er-1670                 pic x(4)  value '1670'.
052113     12  er-1671                 pic x(4)  value '1671'.
052113     12  er-1672                 pic x(4)  value '1672'.
052113     12  er-1673                 pic x(4)  value '1673'.
052113     12  er-1674                 pic x(4)  value '1674'.
           12  er-1677                 pic x(4)  value '1677'.
090821     12  er-1681                 pic x(4)  value '1681'.
00511      12  ER-1775                 PIC X(4)  VALUE '1775'.
00512      12  ER-1880                 PIC X(4)  VALUE '1880'.
100518     12  ER-1930                 PIC X(4)  VALUE '1930'.
022106     12  ER-2044                 PIC X(4)  VALUE '2044'.
00513      12  ER-3140                 PIC X(4)  VALUE '3140'.
00514      12  ER-3141                 PIC X(4)  VALUE '3141'.
00515      12  ER-3255                 PIC X(4)  VALUE '3255'.
082218     12  ER-3272                 PIC X(4)  VALUE '3272'.
040819     12  ER-3276                 PIC X(4)  VALUE '3276'.
040819     12  ER-3277                 PIC X(4)  VALUE '3277'.
040819     12  ER-3278                 PIC X(4)  VALUE '3278'.
040819     12  ER-3279                 PIC X(4)  VALUE '3279'.
043019     12  ER-3280                 PIC X(4)  VALUE '3280'.
00516      12  ER-3516                 PIC X(4)  VALUE '3516'.
00517      12  ER-3527                 PIC X(4)  VALUE '3527'.
00518      12  ER-3528                 PIC X(4)  VALUE '3528'.
00519      12  ER-3529                 PIC X(4)  VALUE '3529'.
00520      12  ER-3530                 PIC X(4)  VALUE '3530'.
00521      12  ER-3531                 PIC X(4)  VALUE '3531'.
00522      12  ER-3532                 PIC X(4)  VALUE '3532'.
00523      12  ER-3533                 PIC X(4)  VALUE '3533'.
00524      12  ER-3534                 PIC X(4)  VALUE '3534'.
00525      12  ER-3535                 PIC X(4)  VALUE '3535'.
00526      12  ER-3536                 PIC X(4)  VALUE '3536'.
00527      12  ER-3537                 PIC X(4)  VALUE '3537'.
00528      12  ER-3538                 PIC X(4)  VALUE '3538'.
00529      12  ER-3539                 PIC X(4)  VALUE '3539'.
00530      12  ER-3540                 PIC X(4)  VALUE '3540'.
00531      12  ER-3541                 PIC X(4)  VALUE '3541'.
00532      12  ER-3542                 PIC X(4)  VALUE '3542'.
00533      12  ER-3543                 PIC X(4)  VALUE '3543'.
00534      12  ER-3546                 PIC X(4)  VALUE '3546'.
112210     12  ER-3549                 PIC X(4)  VALUE '3549'.
112018     12  ER-3551                 PIC X(4)  VALUE '3551'.
           12  ER-3818                 PIC X(4)  VALUE '3818'.
00535      12  ER-7540                 PIC X(4)  VALUE '7540'.
061511     12  ER-7576                 PIC X(4)  VALUE '7576'.
020513     12  ER-7579                 PIC X(4)  VALUE '7579'.
032414     12  ER-7580                 PIC X(4)  VALUE '7580'.
032514     12  ER-7583                 PIC X(4)  VALUE '7583'.
100518     12  ER-7585                 PIC X(4)  VALUE '7585'.
00536      12  ER-7830                 PIC X(4)  VALUE '7830'.
00537      12  ER-7831                 PIC X(4)  VALUE '7831'.
00538      12  ER-7832                 PIC X(4)  VALUE '7832'.
00539      12  ER-7833                 PIC X(4)  VALUE '7833'.
00540      12  ER-7834                 PIC X(4)  VALUE '7834'.
00541      12  ER-7835                 PIC X(4)  VALUE '7835'.
00542      12  ER-7836                 PIC X(4)  VALUE '7836'.
00543      12  ER-7837                 PIC X(4)  VALUE '7837'.
00544      12  ER-7838                 PIC X(4)  VALUE '7838'.
00545      12  ER-7841                 PIC X(4)  VALUE '7841'.
00546      12  ER-7844                 PIC X(4)  VALUE '7844'.
00547      12  ER-7845                 PIC X(4)  VALUE '7845'.
00548      12  ER-8000                 PIC X(4)  VALUE '8000'.
00549      12  ER-8051                 PIC X(4)  VALUE '8051'.
00550      12  ER-8052                 PIC X(4)  VALUE '8052'.
00551      12  ER-8053                 PIC X(4)  VALUE '8053'.
00552      12  ER-8054                 PIC X(4)  VALUE '8054'.
00553      12  ER-8055                 PIC X(4)  VALUE '8055'.
00554      12  ER-8056                 PIC X(4)  VALUE '8056'.
00555      12  ER-8057                 PIC X(4)  VALUE '8057'.
00556      12  ER-8058                 PIC X(4)  VALUE '8058'.
00557      12  ER-8059                 PIC X(4)  VALUE '8059'.
00558      12  ER-8060                 PIC X(4)  VALUE '8060'.
00559      12  ER-8061                 PIC X(4)  VALUE '8061'.
00560      12  ER-8062                 PIC X(4)  VALUE '8062'.
00561      12  ER-8063                 PIC X(4)  VALUE '8063'.
00562      12  ER-8064                 PIC X(4)  VALUE '8064'.
00563      12  ER-8065                 PIC X(4)  VALUE '8065'.
00564      12  ER-8066                 PIC X(4)  VALUE '8066'.
00565      12  ER-8146                 PIC X(4)  VALUE '8146'.
00566      12  ER-8147                 PIC X(4)  VALUE '8147'.
00567      12  ER-8152                 PIC X(4)  VALUE '8152'.
00568      12  ER-8153                 PIC X(4)  VALUE '8153'.
00569      12  ER-8154                 PIC X(4)  VALUE '8154'.
00570      12  ER-8155                 PIC X(4)  VALUE '8155'.
00571      12  ER-8310                 PIC X(4)  VALUE '8310'.
00572      12  ER-8311                 PIC X(4)  VALUE '8311'.
00573      12  ER-8312                 PIC X(4)  VALUE '8312'.
00574      12  ER-8313                 PIC X(4)  VALUE '8313'.
00575      12  ER-8314                 PIC X(4)  VALUE '8314'.
00576      12  ER-8315                 PIC X(4)  VALUE '8315'.
00577      12  ER-8316                 PIC X(4)  VALUE '8316'.
00578      12  ER-8317                 PIC X(4)  VALUE '8317'.
CIDMOD
CIDMOD 01  CSO-WORK-FIELDS.
CIDMOD     05  ERROR-ON-OUTPUT-SW  PIC X     VALUE 'N'.
CIDMOD       88  ERROR-ON-OUTPUT             VALUE 'Y'.
041710
041710     05  WRK-6MO                 PIC S9(7)V99 VALUE +0.
041710     05  WRK-CERT-NOTE-ADD       PIC X VALUE SPACES.
041710         88  CERT-NOTE-ADDED           VALUE 'Y'.
041710     05  WRK-ORIG-NOTE-ADD       PIC X VALUE SPACES.
041710         88  ORIG-NOTE-ADDED           VALUE 'Y'.
041710     05  WRK-NOTE-SEQ            PIC S9(4) COMP  VALUE +0.
032813
032813     05  WRK-NEXT-AUDIT-CHK-NO   PIC S9(8) COMP  VALUE +0.
041710
041710 01  SC-NP6-CERT-NOTE.
041710     12  FILLER                  PIC X(12)
041710               VALUE 'REM BENEFIT '.
041710     12  SC-NP6-REM-BEN          PIC ZZZ,ZZZ.99.
041710     12  SC-NP6-COMM             PIC X(8)
041710               VALUE ' + 6 MO '.
041710     12  SC-NP6-6-MO             PIC ZZZ,ZZZ.99.
041710     12  FILLER                  PIC X(12)
041710               VALUE ' = AMT PAID '.
041710     12  SC-NP6-AMT-PAID         PIC ZZZ,ZZZ.99.
041710
041710 01  SC-NP6-ORIG-NOTE.
041710     12  FILLER                  PIC X(63)  VALUE
041710         'BENEFITS PAID CANNOT EXCEED ORIGINAL LIFE BENEFIT'.
       01  WS-PDEF-RECORD-SW           PIC X  VALUE ' '.
           88  PDEF-FOUND                   VALUE 'Y'.
090821 01  filler.
090821     05  ws-eracct-startbr-ind   pic x  value spaces.
090821         88  eracct-browse-started  value 'Y'.
090821     05  ws-lo-acct-dt           pic xx value low-values.
090821     05  ws-hi-acct-dt           pic xx value low-values.
090821     05  ws-acct-status          pic x value spaces.
090821         88  acct-cancelled          value '3'.
090821     05  WS-I-SAY-STOP-IND       PIC X  VALUE ' '.
090821         88  i-say-STOP            value 'S'.
       01  WS-ACCT-RECORD-SW           PIC X  VALUE ' '.
           88  ACCT-FOUND                   VALUE 'Y'.
       01  CTBL-KEY-SAVE               PIC X(5).
       01  CTBL-KEY.
           05  CTBL-COMPANY-CD         PIC X.
           05  CTBL-TABLE              PIC XXX.
           05  CTBL-BEN-TYPE           PIC X.
           05  CTBL-BEN-CODE           PIC XX.
       01  ERPDEF-KEY-SAVE             PIC X(18).
       01  ERPDEF-KEY.
           12  ERPDEF-COMPANY-CD       PIC X.
           12  ERPDEF-STATE            PIC XX.
           12  ERPDEF-PROD-CD          PIC XXX.
           12  F                       PIC X(7).
           12  ERPDEF-BEN-TYPE         PIC X.
           12  ERPDEF-BEN-CODE         PIC XX.
           12  ERPDEF-EXP-DT           PIC XX.
       01  ELCRTT-KEY.
           05  CTRLR-COMP-CD       PIC X.
           05  CTRLR-CARRIER       PIC X.
           05  CTRLR-GROUPING      PIC X(6).
           05  CTRLR-STATE         PIC X(2).
           05  CTRLR-ACCOUNT       PIC X(10).
           05  CTRLR-EFF-DT        PIC XX.
           05  CTRLR-CERT-NO       PIC X(11).
           05  CTRLR-REC-TYPE      PIC X.
00580  01  ACCESS-KEYS.
00581      12  ELARCH-KEY.
00582          16  ARCH-CO             PIC X.
00583          16  ARCH-NUMBER         PIC S9(08) COMP.
00584          16  ARCH-REC-TYPE       PIC X.
00585          16  ARCH-SEQ            PIC S9(04) COMP.
00586      12  ELMSTR-KEY.
00587          16  MSTR-COMP-CD        PIC X.
00588          16  MSTR-CARRIER        PIC X.
00589          16  MSTR-CLAIM-NO       PIC X(7).
00590          16  MSTR-CERT-NO        PIC X(11).
00591      12  ELCNTL-KEY.
00592          16  CNTL-COMP-ID        PIC X(3).
00593          16  CNTL-REC-TYPE       PIC X.
00594          16  CNTL-ACCESS         PIC X(4).
00595          16  CNTL-SEQ-NO         PIC S9(4)    COMP.
00596      12  ELCERT-KEY.
00597          16  CERT-COMP-CD        PIC X.
00598          16  CERT-CARRIER        PIC X.
00599          16  CERT-GROUPING       PIC X(6).
00600          16  CERT-STATE          PIC XX.
00601          16  CERT-ACCOUNT        PIC X(10).
00602          16  CERT-EFF-DT         PIC XX.
00603          16  CERT-CERT-NO        PIC X(11).
00604      12  ELTRLR-KEY.
00605          16  TRLR-COMP-CD        PIC X.
00606          16  TRLR-CARRIER        PIC X.
00607          16  TRLR-CLAIM-NO       PIC X(7).
00608          16  TRLR-CERT-NO        PIC X(11).
00609          16  TRLR-SEQ-NO         PIC S9(4)   COMP.
00610      12  ELACTQ-KEY.
00611          16  ACTQ-COMP-CD        PIC X.
00612          16  ACTQ-CARRIER        PIC X.
00613          16  ACTQ-CLAIM-NO       PIC X(7).
00614          16  ACTQ-CERT-NO        PIC X(11).
00615      12  W-NOTE-KEY.
00616          16  W-NOTE-COMP-CD      PIC X.
00617          16  W-NOTE-CERT-KEY.
00618              20  W-NOTE-CARRIER  PIC X.
00619              20  W-NOTE-GROUPING PIC X(6).
00620              20  W-NOTE-STATE    PIC XX.
00621              20  W-NOTE-ACCOUNT  PIC X(10).
00622              20  W-NOTE-EFF-DT   PIC XX.
00623              20  W-NOTE-CERT-NO  PIC X(11).
00624      12  WS-ERACCT-SAVE-KEY      PIC X(20).
00625      12  WS-ERACCT-HOLD-RECORD   PIC X(2000).
00626      12  ERACCT-KEY.
00627          16  ERACCT-PARTIAL-KEY.
00628              20  ACCT-COMP-CD    PIC X.
00629              20  ACCT-CARRIER    PIC X.
00630              20  ACCT-GROUPING   PIC X(6).
00631              20  ACCT-STATE      PIC XX.
00632              20  ACCT-ACCOUNT    PIC X(10).
00633          16  ACCT-EXP-DT         PIC XX.
00634          16  FILLER              PIC X(4) VALUE SPACES.
00635      12  ELCHKQ-KEY.
00636          16  CHKQ-COMP-CD        PIC X.
00637          16  CHKQ-CONTROL        PIC S9(8) COMP.
00638          16  CHKQ-SEQ-NO         PIC S9(4) COMP.
00639      12  ELBENE-KEY.
00640          16  BENE-COMP-CD         PIC X.
00641          16  BENE-RECORD-TYPE     PIC X.
00642          16  BENE-NUMBER          PIC X(10).
00643      12  ERCOMP-KEY.
00644          16  COMP-COMP-CD        PIC X.
00645          16  COMP-CARRIER        PIC X.
00646          16  COMP-GROUP          PIC X(06).
00647          16  COMP-FIN-RESP       PIC X(10).
00648          16  COMP-ACCOUNT        PIC X(10).
00649          16  COMP-TYPE           PIC X.
00650
00651      12  DLO035-KEY.
00652          16  DL35-PROCESS-TYPE   PIC X.
00653          16  DL35-CREDIT-CARD    PIC X(20).
00654          16  DL35-BEN-TYPE       PIC XX.
00655          16  DL35-PAYMENT-AMT    PIC 9(7)V99.
00656          16  DL35-RETURN-CODE    PIC XX.
00657
00658      EJECT
00659 *    COPY ELCDATE.
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
00660      EJECT
00661 *    COPY ELCLOGOF.
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
00662      EJECT
00663 *    COPY ELCCALC.
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
021406*    COPY ELCICALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCICALC.                           *
00005 *                            VMOD=2.001                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO THE CLAIM INTEREST        *
00008 *                 CALCULATION MODULE                             *
00011 *                                                                *
00012 *  PASSED TO ELCLMI                                              *
00013 *  -----------------                                             *
00014 *  STATE                                                         *
00015 *  PRODUCT                                                       *
00016 *  COVERAGE                                                      *
      *  EFFECTIVE DATE                                                *
00017 *  INCURRED DATE                                                 *
00018 *  ESTABLISHED DATE                                              *
00019 *  LAST PAID DATE                                                *
00020 *  REPORTED DATE                                                 *
00021 *  CLAIM PAYMENT AMOUNT                                          *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  RETURN CODE                                                   *
00026 *  CLAIM INTEREST DUE SWITCH Y/N                                 *
00027 *  CLAIM INTEREST AMOUNT                                         *
00029 *----------------------------------------------------------------*
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 100                                   *
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
120905* 120905    2004040700004  PEMA  NEW COPYBOOK
060608* 060608    2008040300002  PEMA  ADD EFF DT TO ELCLMI PASS AREA
010303******************************************************************
00170
00171  01  CLAIM-INT-PASS-AREA.
00172      12  CP-CLAIM-LENGTH           PIC S9(4)         VALUE +100
00173                                      COMP.
00174
00175      12  CI-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CI-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'.
00179        88  CP-ERROR-IN-SEL-CRITERIA                VALUE '1'.
00180        88  CP-NOT-QUAL-LEVEL-1                     VALUE '2'.
00181        88  CP-NOT-QUAL-LEVEL-2                     VALUE '3'.
00182        88  CP-AMT-LT-5000                          VALUE '4'.
00183        88  CP-ERROR-IN-DATES                       VALUE '5'.
00184        88  CP-ERROR-IN-ST-BREAKOUT                 VALUE '6'.
00185        88  CP-ERROR-IN-SCHEDULES                   VALUE '7'.
00185        88  CP-INTEREST-RATE-IS-ZERO                VALUE '8'.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
               16  CP-COMPANY-CD         PIC X.
00202          16  CP-STATE              PIC XX.
00203          16  CP-PRODUCT            PIC XX.
00204          16  CP-COVERAGE           PIC XX.
00205          16  CP-INC-DT             PIC XX.
00205          16  CP-EST-DT             PIC XX.
00205          16  CP-LSTPD-DT           PIC XX.
00205          16  CP-RPT-DT             PIC XX.
               16  CP-PRF-DT             PIC XX.
               16  CP-EFF-DT             PIC XX.
               16  CP-CLAIM-AMT          PIC S9(7)V99 COMP-3.
               16  CP-INT-RATE           PIC S99V9(5) COMP-3.
090803         16  FILLER                PIC X(37).
00363
00364 ***************    OUTPUT FROM ELCLMI   ************************
00365
00366          16  CP-CLM-INT-SW         PIC X         VALUE SPACES.
00367
00368          16  CP-CLM-INT-AMT        PIC S9(7)V99  VALUE +0 COMP-3.
               16  CP-CLM-INT-RATE       PIC S99V9(5)  VALUE +0 COMP-3.
               16  CP-CLM-INT-NODAYS     PIC S9(5)     VALUE +0 COMP-3.
010303         16  FILLER                PIC X(19).
00514 ******************************************************************
00664      EJECT
00665 *    COPY ELCATTR.
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
00666      EJECT
00667 *    COPY ELCEMIB.
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
00668      EJECT
00669 *    COPY ELCLNKLT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                           ELCLNKLT                             *
00004 *                            VMOD=2.003                          *
00005 *                                                                *
00006 *   THIS COPY BOOK IS USED TO PASS DATA TO THE LETTER GENERATOR  *
00007 *   PROGRAM (EL1523). THE AREA SHOULD BE INITIALIZED TO LOW-     *
00008 *   VALUES BEFORE ENTERING YOUR INFORMATION.                     *
00009 *                                                                *
00010 *   MOVE THE PROGRAM-INTERFACE-BLOCK TO W-1523-LINK-DATA BEFORE  *
00011 *   COMPLETING THE REQUIRED FIELDS.                              *
00012 *                                                                *
00013 *   THE CALLING PROGRAM SHOULD CHECK:                            *
00014 *   1. W-1523-ERROR-CODE WHERE 0000 INDICATES NO ERROR DETECTED. *
00015 *                              9999 INDICATES AN UNKNOWN ABEND   *
00016 *                                   WAS DETECTED.                *
00017 *                              ALL OTHER VALUES ARE NORMAL ERROR *
00018 *                              CODES FOUND IN THE ERROR FILE.    *
00019 ******************************************************************
00020  01  W-1523-LINKDATA.
00021      12  W-1523-COMMON-PI-DATA.
00022          16  W-1523-COMM-LENGTH  PIC S9(04) COMP   VALUE +1024.
00023          16  FILLER              PIC  X(67).
00024          16  W-1523-COMPANY-CD   PIC  X(01).
00025          16  FILLER              PIC  X(10).
00026          16  W-1523-CONTROL-IN-PROGRESS.
00027              20  W-1523-CARRIER  PIC  X(01).
00028              20  W-1523-GROUPING PIC  X(06).
00029              20  W-1523-STATE    PIC  X(02).
00030              20  W-1523-ACCOUNT  PIC  X(10).
00031              20  W-1523-CLAIM-CERT-GRP.
00032                  24  W-1523-CLAIM-NO
00033                                  PIC  X(07).
00034                  24  W-1523-CERT-NO.
00035                      28  W-1523-CERT-PRIME
00036                                  PIC  X(10).
00037                      28  W-1523-CERT-SFX
00038                                  PIC  X(01).
00039                  24  W-1523-CERT-EFF-DT
00040                                  PIC  X(02).
00041          16  FILLER              PIC X(265).
00042
00043      12  W-1523-WORK-AREA.
00044          16  W-1523-FORM-NUMBER  PIC  X(04).
00045          16  W-1523-NUMBER-COPIES
00046                                  PIC  9(01).
00047          16  W-1523-ADDR-TYPE    PIC  X(02).
00048          16  W-1523-FOLLOW-UP-DATE
00049                                  PIC  X(02).
00050          16  W-1523-RESEND-DATE  PIC  X(02).
00051          16  W-1523-ERROR-CODE   PIC  9(04).
00052              88  W-1523-NO-ERRORS-DETECTED VALUE 0000.
00053              88  W-1523-FATAL-ERROR
00054                                     VALUES  0006 0013 0042
00055                                             0154 0168 0169
00056                                             0176 9999 0172
00057                                             0179 0186 0281
00058                                             0332 2055 3697
00059                                             3698 3699 3770
00060                                             3771 3772 7675
00061                                             9106 9808 9883
00062                                             9887.
00063              88  W-1523-STOP-ERRORS
00064                                     VALUES  0013 0042 0154
00065                                             0168 0169 0172
00066                                             0281 0332 2055
00067                                             3698 3699 7675
00068                                             9106 9808 9883
00069                                             9999.
00070          16  W-1523-REASON       PIC  X(70).
00071          16  W-1523-ARCHIVE-NUMBER
00072                                  PIC  9(08).
00073      12  W-1523-POINTER-AREA.
00074          16  W-1523-ACCT-POINTER PIC S9(08) COMP.
00075          16  W-1523-ACTY-POINTER PIC S9(08) COMP.
00076          16  W-1523-ARCH-POINTER PIC S9(08) COMP.
00077          16  W-1523-VAR-POINTER  PIC S9(08) COMP.
00078          16  W-1523-PROD-POINTER PIC S9(08) COMP.
00670      EJECT
00671 *    COPY ELCNWA.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNWA.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 *            M O V E   N A M E   W O R K   A R E A.             *
00008 *                                                               *
00009 *****************************************************************.
00010
00011  01  WS-NAME-WORK-AREA.
00012      05  WS-INSURED-LAST-NAME        PIC X(15).
00013      05  WS-INSURED-1ST-NAME         PIC X(12).
00014      05  WS-INSURED-MID-INIT         PIC X.
00015
00016      05  WS-NAME-WORK.
00017          10  WS-NW                   PIC X
00018              OCCURS 30 TIMES INDEXED BY NWA-INDEX.
00019
00020      05  WS-NAME-WORK2.
00021          10  WS-NW2                  PIC X
00022              OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
00023                                         NWA-INDEX0.
00024
00025      05  WS-NAME-SW                  PIC S9          VALUE ZERO
00026                                      COMP-3.
00027
00672      EJECT
00673 *    COPY ELCDMO.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDMO.                             *
00004 *                            VMOD=2.004                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = DLO025 (DMO FILE MAINTENANCE PGRM)        *
00007 *        COMMUNICATION AREA                                      *
00008 *   FILE TYPE = NA                                               *
00009 *   RECORD SIZE = 110    RECFORM = FIXED                         *
00010 *                                                                *
00011 ******************************************************************
00012  01  DMO-COMMUNICATION-AREA.
00013      12  DM-RECORD-TYPE                  PIC  X(02).
00014              88  DM-ISSUE-TRAN                VALUE 'CC'.
00015              88  DM-CLAIM-STATUS-CHANGE       VALUE 'CS'.
00016              88  DM-CLAIM-PAYMENT             VALUE 'DR'.
00017      12  DM-DIST-CODE                    PIC  X(04).
00018      12  DM-MAIL-CODE                    PIC  X(05).
00019      12  DM-CREDIT-CARD-NUMBER           PIC  X(16).
00020      12  DM-INSURED-NAME                 PIC  X(30).
00021      12  DM-CLAIM-NO                     PIC  X(07).
00022      12  DM-CLAIM-TYPE                   PIC  X.
00023
00024      12  DM-STATUS-DATA-AREA.
00025          16  DM-CLAIM-STATUS             PIC  X.
00026              88  DM-OPEN-NO-PAYMENTS              VALUE '1'.
00027              88  DM-OPEN-WITH-PAYMENTS            VALUE '2'.
00028              88  DM-CLOSED                        VALUE '3'.
00029              88  DM-CLOSE-SETTLE-FINAL            VALUE '4'.
00030              88  DM-DEFAULT                       VALUE '9'.
00031          16  DM-STATUS-DATE              PIC  X(08).
00032 ******YYYYMMDD
00033          16  DM-STAT-CHANGE-TYPE         PIC  X.
00034              88  DM-MANUAL-CLOSE                  VALUE 'C'.
00035              88  DM-CLAIM-DENIED                  VALUE 'D'.
00036              88  DM-FINAL-PAYMENT                 VALUE 'F'.
00037              88  DM-INITIAL-PAYMENT               VALUE 'I'.
00038              88  DM-AUTO-CLOSE                    VALUE 'Q'.
00039              88  DM-RE-OPENED                     VALUE 'R'.
00040              88  DM-NEW-CLAIM-SETUP               VALUE 'S'.
00041              88  DM-VOIDED-PAYMENT                VALUE 'V'.
00042              88  DM-CLAIM-DELETED                 VALUE 'X'.
00043          16  DM-STAT-CARRIER             PIC X.
00044
00045      12  DM-DRAFT-DATA-AREA.
00046          16  DM-PAYMENT-TYPE             PIC  X.
00047              88  DM-VALID-CLAIM-TYPES VALUES 'L' 'D' 'U' 'A'.
00048          16  DM-PAYMENT-AMT              PIC  9(05)V9(02).
00049          16  DM-PAYMENT-DATE             PIC  X(08).
00050 ******YYYYMMDD
00051          16  DM-CERT-NO                  PIC  X(11).
00052          16  DM-TRLR-SEQ-NO              PIC  9(04).
00053          16  DM-CARRIER                  PIC  X.
00054
00055      12  DM-RETURN-CODE                  PIC  XX.
00674      EJECT
00675 *    COPY ELCDMDCD.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDMDCD.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = CODES VALIDATION PROGRAM (DLO023)              *
00007 *       COMMUNICATIONS AREA                                      *
00008 *                                                                *
00009 ******************************************************************
00010  01  CD-COMMUNICATION-AREA.
00011      12  CD-SYSTEM-ID            PIC  X(02).
00012          88 CD-CLAIMS                      VALUE 'CL'.
00013          88 CD-CREDIT                      VALUE 'CR'.
00014      12  CD-RECORD-TYPE          PIC  X(02).
00015          88 CD-DENIAL-CODE                 VALUE 'DN'.
00016          88 CD-EOB-CODE                    VALUE 'EO'.
00017          88 CD-ICD9-CODE                   VALUE 'I9'.
00018          88 CD-OCCUPATION-CODE             VALUE 'OC'.
00019          88 CD-PAYMENT-NOT-CODE            VALUE 'PN'.
00020          88 CD-CAUSE-CODE                  VALUE 'CS'.
00021          88 CD-COVERAGE-CAT-CODE           VALUE 'CV'.
00022          88 CD-RESIDENT-STATE-CODE         VALUE 'RS'.
00023      12  CD-RECORD-KEY           PIC  X(06).
00024          88 CD-SUCCESSFUL                  VALUE 'OK'.
00025      12  CD-RETURN-CODE          PIC  X(02).
00026      12  CD-CODE-DESCRIPTION     PIC  X(60).
00027      12  CD-GENERAL-DESCRIPTION1 PIC  X(20).
00028      12  CD-GENERAL-DESCRIPTION2 PIC  X(20).
00029      12  CD-GENERAL-DESCRIPTION3 PIC  X(20).
00676      EJECT
00677 *    COPY ELCDCTB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDCTB.                            *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *   DESCRIPTION = DISTRIBUTION CONTROL TABLE MAINTENANCE PROGRAM *
00007 *       COMMUNICATIONS AREA                                      *
00008 *                                                                *
00009 ******************************************************************
00010  01  DCT-COMMUNICATION-AREA.
00011      12  DCT-BILLING-BANK-ID      PIC  X(05).
00012      12  DCT-LOGIC-BENEFICIARY-ID PIC  X(10).
00013      12  DCT-CREDIT-CARD-NUMBER   PIC  X(16).
00014      12  DCT-PRODUCT-CODE         PIC  X(02).
00015      12  DCT-COLUMN-ID-REQUESTED  PIC  X(02).
00016      12  DCT-RETURN-CODE          PIC  X(02).
00017      12  DCT-MAIL-CODE            PIC  X(05).
00018      12  DCT-DISTRIBUTION-CODE    PIC  X(04).
00019      12  DCT-MSA-ACCT-NO          PIC  X(07).
00678      EJECT
00679  01  CD-RCRD-LENGTH                  PIC S9(4)  COMP VALUE +132.
00680  01  DCT-RCRD-LENGTH                 PIC S9(4)  COMP VALUE +53.
00681  01  DM-DMO-LENGTH                   PIC S9(4)  COMP VALUE +108.
00682  01  DL35-LENGTH                     PIC S9(4)  COMP VALUE +34.
00683                                  EJECT
00684 *    COPY ELCINTF.
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
00685      12  PI-REDEF     REDEFINES PI-PROGRAM-WORK-AREA.
00686          16  PI-SAVE-FORM        PIC X(12).
00687          16  PI-ACCT-KEY         PIC X(26).
00688          16  PI-PAYEE-NAME       PIC X(30).
00689          16  PI-DAILY-RATE       PIC S9(3)V99.
00690          16  PI-MONTH-END-SAVE   PIC XX.
00691          16  PI-BENEFIT-SAVE.
00692              20  PI-BEN-DAYS     PIC 99.
00693              20  PI-BEN-TYPE     PIC X.
00694          16  PI-MANUAL-SW        PIC 9.
00695          16  PI-SAVE-CURSOR      PIC S9(4) COMP.
00696          16  PI-PASS-SW          PIC X.
00697          16  PI-PFKEY-USED       PIC X.
00698          16  PI-CK-ASSIGN        PIC X.
00699          16  PI-FATAL-COUNT      PIC 99.
00700          16  PI-FORCE-COUNT      PIC 99.
00701          16  PI-SAVE-INPUT.
00702              20  PI-PMTTYPE      PIC X.
00703              20  PI-PAYEE.
00704                  24  PI-PAYEE-TYPE PIC X.
00705                  24  PI-PAYEE-SEQ  PIC X.
00706                  24  PI-PAYEE-SEQ-NUM REDEFINES
00707                    PI-PAYEE-SEQ    PIC S9.
00708              20  PI-PMTNOTE1     PIC X(60).
00709              20  PI-PMTNOTE2     PIC X(60).
00710              20  PI-OFFLINE      PIC X.
00711              20  PI-ECHECKNO     PIC X(7).
00712              20  PI-ECHECKNO-RDF REDEFINES PI-ECHECKNO.
00713                  28  PI-ECHECKNO-N     PIC X.
00714                  28  PI-ECHECKNO-6     PIC X(6).
00715              20  PI-ECHECKNO-RDF1 REDEFINES PI-ECHECKNO.
00716                  24  PI-ECHECKNO-CR    PIC XX.
00717                  24  PI-ECHECKNO-5     PIC X(5).
00718              20  PI-HOLDTIL      PIC 9(6).
00719              20  PI-EPYFROM      PIC 9(6).
00720              20  PI-EPYTHRU      PIC 9(6).
00721              20  PI-EDAYS        PIC S9(5).
00722              20  PI-EPYAMT       PIC S9(7)V99.
00723              20  PI-ERESV        PIC S9(7)V99.
00724              20  PI-EEXPENS      PIC 9(6)V99.
00725              20  PI-ETYPE        PIC X.
00726              20  PI-CASH         PIC X.
00727              20  PI-GROUPED      PIC X.
00728              20  PI-INT-RATE     PIC S99V9(5).
00729              20  PI-AIGFROM      PIC 9(6).
00730              20  PI-LOAN-NO      PIC X(20).
052506             20  PI-PROOF-DATE   PIC 9(6).
                   20  PI-PRINT-EOB-YN PIC X.
020413             20  PI-PRINT-CLM-FRM-YN PIC X.
020413             20  PI-PRINT-SURVEY-YN PIC X.
102413             20  PI-SPECIAL-RELEASE-YN PIC X.
120115             20  pi-int-to-rem-borr pic x.
00731          16  PI-SAVE-COMPUTED.
00732              20  PI-CCHECKNO     PIC X(7).
00733              20  PI-CCHECKNO-RDF REDEFINES PI-CCHECKNO.
00734                  28  PI-CCHECKNO-N     PIC X.
00735                  28  PI-CCHECKNO-6     PIC X(6).
00736              20  PI-CCHECKNO-RDF1 REDEFINES PI-CCHECKNO.
00737                  24  PI-CCHECKNO-CR    PIC XX.
00738                  24  PI-CCHECKNO-5     PIC X(5).
00739              20  PI-CPYFROM      PIC XX.
00740              20  PI-CPYTHRU      PIC XX.
00741              20  PI-CDAYS        PIC S9(5).
00742              20  PI-CPYAMT       PIC S9(7)V99.
00743              20  PI-CRESV        PIC 9(6)V99.
00744              20  PI-CEXPENS      PIC 9(6)V99.
00745              20  PI-PMT-APPR-SW  PIC X.
00746                  88 PI-PMT-APPROVED    VALUE 'G' 'Y'.
00747                  88 PI-PMT-GRADUATED   VALUE 'G'.
00748                  88 PI-PMT-NO-APPROVAL VALUE ' ' 'N'.
00749          16  PI-PREV-CLMNO       PIC X(7).
00750          16  PI-LF-COVERAGE-TYPE PIC X.
00751          16  PI-MAX-PMT-TOL      PIC S9(7)V99  COMP-3.
00752          16  PI-MAX-LF-PMT-TOL   PIC S9(7)V99  COMP-3.
00753          16  PI-PROVISIONAL-IND  PIC X.
00754              88 PI-PROV-PMT      VALUE 'P'.
00755          16  PI-PREV-TRLR-KEY    PIC X(22).
00756          16  PI-SPLIT-PMT-SW     PIC X.
00757          16  PI-MAX-EXP-PMT      PIC S9(7)V99 COMP-3.
00758          16  PI-ACTIVITY-CODE    PIC 99.
00759          16  PI-RESET-SW         PIC X.
00760          16  PI-SPECIAL-BEN-SW   PIC X.
00761              88 PI-AIG-SPECIAL-BENEFIT VALUE 'Y'.
00762          16  PI-REDUND-PMTAMT    PIC S9(7)V99.
00763          16  PI-EXP-DT           PIC XX.
00764          16  PI-LOAN-DUE-DAY     PIC 99.
00765          16  PI-3RD-PARTY-SW     PIC X.
00766          16  PI-LOAN-UPDATE-SW   PIC X.
00767
00768          16  PI-DMD-DATA.
00769              20  PI-SV-CERT-KEY       PIC X(21).
00770              20  PI-SV-CERT-NO        PIC X(11).
00771              20  PI-SV-BEN            PIC X(10).
00772              20  PI-SV-CCN            PIC X(16).
00773              20  PI-INTEREST-PAID-IND PIC X.
00774                  88  PI-INTEREST-PAID  VALUE 'Y'.
00775              20  PI-MAX-BENEFIT-AMT   PIC S9(5)V99 COMP-3.
00776              20  PI-MAX-BENEFIT-PYMT  PIC S9(5)V99 COMP-3.
00777              20  PI-MIN-PAYMENT-AMT   PIC S9(5)V99 COMP-3.
00778              20  PI-TIME-OF-LOSS-BAL  PIC S9(5)V99 COMP-3.
00779          16  PI-AT-EOB-CODES.
00780              20 PI-AT-EOB-CODE        PIC XXX   OCCURS 5.
021406         16  PI-INT-AMT               PIC S9(5)V99 COMP-3.
012507         16  PI-INT-DAYS              PIC S9(5) COMP-3.
082807         16  PI-INT-RATE-USED         PIC S99V9(5) COMP-3.
031808         16  PI-SAVE-ELAPSED-MONTHS   PIC 9(03).
031808         16  PI-SAVE-ODD-DAYS-OVER    PIC S9(04).
091808         16  PI-CHECK-STATE-FOR-AK    PIC X(02).
041710         16  PI-ORIG-BEN-AMT          PIC S9(7)V99 COMP-3.
041710         16  PI-REM-BEN-AMT           PIC S9(7)V99 COMP-3.
041710         16  PI-MO-BEN-AMT            PIC S9(7)V99 COMP-3.
041710         16  PI-LF-BENEFIT-CD         PIC X(2).
               16  PI-EOB-CODES-EXIST PIC X.
               16  PI-SET-NOTE2-MDT   PIC X.
061511         16  PI-VFY-2ND-BENE          PIC X.
013013         16  PI-CLM-TYPE              PIC X.
020413         16  PI-APPROVAL-LEVEL        PIC X.
032813         16  PI-REAUDIT-INTERVAL      PIC S9(5) COMP-3.
032813         16  PI-REAUDIT-NEEDED        PIC X.
               16  PI-DCC-PRODUCT-CODE      PIC XXX.
               16  PI-AH-BENEFIT-CD         PIC XX.
               16  pi-ah-term               pic s999 comp-3.
               16  pi-ah-benefit-amt        pic s9(7)v99 comp-3.
               16  pi-max-ext               pic 999.
               16  pi-max-ext-used          pic x.
052814         16  PI-APPROVAL-3550-NEEDED  PIC X.
052814         16  PI-JOINT-COV-IND         PIC X.
052814             88  PI-JOINT-COVERAGE       VALUE 'J'.
052814             88  PI-SINGLE-COVERAGE      VALUE ' '.
052814         16  PI-JOINT-INSURED-IND     PIC X.
052814             88  PI-JOINT-INSURED        VALUE 'Y'.
052814             88  PI-PRIMARY-INSURED      VALUE 'N'.
012016         16  pi-int-payees-name       pic x(30).
120115         16  PI-RB-JOINT-COV-IND      PIC X.
120115             88  PI-RB-JOINT-COVERAGE    VALUE 'J'.
120115             88  PI-RB-SINGLE-COVERAGE   VALUE ' '.
013017         16  pi-ach-payment           pic x.
043019         16  PI-DUPE-APPROVAL-NEEDED  PIC X.
052506*00781          16  FILLER                   PIC X(176).
031808*052506         16  FILLER                   PIC X(161).
091808*031808         16  FILLER                   PIC X(154).
041710*091808         16  FILLER                   PIC X(152).
061511*041710         16  FILLER                   PIC X(94).
043019         16  FILLER                   PIC X(33).
00782
00783      EJECT
00784 *    COPY ELCJPFX.
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
00785                              PIC X(750).
00786      EJECT
00787 *    COPY ELCAID.
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
00788  01  FILLER    REDEFINES DFHAID.
00789      12  FILLER              PIC X(8).
00790      12  PF-VALUES           PIC X       OCCURS 2.
00791      EJECT
00792 *    COPY EL156S.
       01  EL156AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEAL PIC S9(0004) COMP.
           05  RUNDTEAF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEAF.
               10  RUNDTEAA PIC  X(0001).
           05  RUNDTEAI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMAL PIC S9(0004) COMP.
           05  RUNTIMAF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMAF.
               10  RUNTIMAA PIC  X(0001).
           05  RUNTIMAI PIC  X(0005).
      *    -------------------------------
           05  SEQUL PIC S9(0004) COMP.
           05  SEQUF PIC  X(0001).
           05  FILLER REDEFINES SEQUF.
               10  SEQUA PIC  X(0001).
           05  SEQUI PIC  X(0010).
      *    -------------------------------
           05  CLMNOL PIC S9(0004) COMP.
           05  CLMNOF PIC  X(0001).
           05  FILLER REDEFINES CLMNOF.
               10  CLMNOA PIC  X(0001).
           05  CLMNOI PIC  X(0007).
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
           05  CERTNOI PIC  X(0010).
      *    -------------------------------
           05  SUFXL PIC S9(0004) COMP.
           05  SUFXF PIC  X(0001).
           05  FILLER REDEFINES SUFXF.
               10  SUFXA PIC  X(0001).
           05  SUFXI PIC  X(0001).
      *    -------------------------------
           05  CLMTYPL PIC S9(0004) COMP.
           05  CLMTYPF PIC  X(0001).
           05  FILLER REDEFINES CLMTYPF.
               10  CLMTYPA PIC  X(0001).
           05  CLMTYPI PIC  X(0006).
      *    -------------------------------
           05  CLMSTATL PIC S9(0004) COMP.
           05  CLMSTATF PIC  X(0001).
           05  FILLER REDEFINES CLMSTATF.
               10  CLMSTATA PIC  X(0001).
           05  CLMSTATI PIC  X(0006).
      *    -------------------------------
           05  LSTNMEL PIC S9(0004) COMP.
           05  LSTNMEF PIC  X(0001).
           05  FILLER REDEFINES LSTNMEF.
               10  LSTNMEA PIC  X(0001).
           05  LSTNMEI PIC  X(0015).
      *    -------------------------------
           05  FSTNMEL PIC S9(0004) COMP.
           05  FSTNMEF PIC  X(0001).
           05  FILLER REDEFINES FSTNMEF.
               10  FSTNMEA PIC  X(0001).
           05  FSTNMEI PIC  X(0012).
      *    -------------------------------
           05  MINITL PIC S9(0004) COMP.
           05  MINITF PIC  X(0001).
           05  FILLER REDEFINES MINITF.
               10  MINITA PIC  X(0001).
           05  MINITI PIC  X(0001).
      *    -------------------------------
           05  PRIMHDGL PIC S9(0004) COMP.
           05  PRIMHDGF PIC  X(0001).
           05  FILLER REDEFINES PRIMHDGF.
               10  PRIMHDGA PIC  X(0001).
           05  PRIMHDGI PIC  X(0012).
      *    -------------------------------
           05  PCERTNOL PIC S9(0004) COMP.
           05  PCERTNOF PIC  X(0001).
           05  FILLER REDEFINES PCERTNOF.
               10  PCERTNOA PIC  X(0001).
           05  PCERTNOI PIC  X(0010).
      *    -------------------------------
           05  PSUFXL PIC S9(0004) COMP.
           05  PSUFXF PIC  X(0001).
           05  FILLER REDEFINES PSUFXF.
               10  PSUFXA PIC  X(0001).
           05  PSUFXI PIC  X(0001).
      *    -------------------------------
           05  STATUSL PIC S9(0004) COMP.
           05  STATUSF PIC  X(0001).
           05  FILLER REDEFINES STATUSF.
               10  STATUSA PIC  X(0001).
           05  STATUSI PIC  X(0010).
      *    -------------------------------
           05  LOANHL PIC S9(0004) COMP.
           05  LOANHF PIC  X(0001).
           05  FILLER REDEFINES LOANHF.
               10  LOANHA PIC  X(0001).
           05  LOANHI PIC  X(0014).
      *    -------------------------------
           05  LOANNOL PIC S9(0004) COMP.
           05  LOANNOF PIC  X(0001).
           05  FILLER REDEFINES LOANNOF.
               10  LOANNOA PIC  X(0001).
           05  LOANNOI PIC  X(0025).
      *    -------------------------------
           05  DUEDAYHL PIC S9(0004) COMP.
           05  DUEDAYHF PIC  X(0001).
           05  FILLER REDEFINES DUEDAYHF.
               10  DUEDAYHA PIC  X(0001).
           05  DUEDAYHI PIC  X(0008).
      *    -------------------------------
           05  DUEDAYL PIC S9(0004) COMP.
           05  DUEDAYF PIC  X(0001).
           05  FILLER REDEFINES DUEDAYF.
               10  DUEDAYA PIC  X(0001).
           05  DUEDAYI PIC  X(0002).
      *    -------------------------------
           05  ZINTHL PIC S9(0004) COMP.
           05  ZINTHF PIC  X(0001).
           05  FILLER REDEFINES ZINTHF.
               10  ZINTHA PIC  X(0001).
           05  ZINTHI PIC  X(0009).
      *    -------------------------------
           05  ZINTL PIC S9(0004) COMP.
           05  ZINTF PIC  X(0001).
           05  FILLER REDEFINES ZINTF.
               10  ZINTA PIC  X(0001).
           05  ZINTI PIC  99V99999.
      *    -------------------------------
           05  PTHRHDGL PIC S9(0004) COMP.
           05  PTHRHDGF PIC  X(0001).
           05  FILLER REDEFINES PTHRHDGF.
               10  PTHRHDGA PIC  X(0001).
           05  PTHRHDGI PIC  X(0014).
      *    -------------------------------
           05  PDTHRUL PIC S9(0004) COMP.
           05  PDTHRUF PIC  X(0001).
           05  FILLER REDEFINES PDTHRUF.
               10  PDTHRUA PIC  X(0001).
           05  PDTHRUI PIC  X(0008).
      *    -------------------------------
           05  TOTPAIDL PIC S9(0004) COMP.
           05  TOTPAIDF PIC  X(0001).
           05  FILLER REDEFINES TOTPAIDF.
               10  TOTPAIDA PIC  X(0001).
           05  TOTPAIDI PIC  X(0010).
      *    -------------------------------
           05  COVERL PIC S9(0004) COMP.
           05  COVERF PIC  X(0001).
           05  FILLER REDEFINES COVERF.
               10  COVERA PIC  X(0001).
           05  COVERI PIC  X(0010).
      *    -------------------------------
           05  INCURL PIC S9(0004) COMP.
           05  INCURF PIC  X(0001).
           05  FILLER REDEFINES INCURF.
               10  INCURA PIC  X(0001).
           05  INCURI PIC  X(0008).
      *    -------------------------------
           05  LSTDTEL PIC S9(0004) COMP.
           05  LSTDTEF PIC  X(0001).
           05  FILLER REDEFINES LSTDTEF.
               10  LSTDTEA PIC  X(0001).
           05  LSTDTEI PIC  X(0008).
      *    -------------------------------
           05  CRTSTATL PIC S9(0004) COMP.
           05  CRTSTATF PIC  X(0001).
           05  FILLER REDEFINES CRTSTATF.
               10  CRTSTATA PIC  X(0001).
           05  CRTSTATI PIC  X(0010).
      *    -------------------------------
           05  EFFECTL PIC S9(0004) COMP.
           05  EFFECTF PIC  X(0001).
           05  FILLER REDEFINES EFFECTF.
               10  EFFECTA PIC  X(0001).
           05  EFFECTI PIC  X(0008).
      *    -------------------------------
           05  TERMSL PIC S9(0004) COMP.
           05  TERMSF PIC  X(0001).
           05  FILLER REDEFINES TERMSF.
               10  TERMSA PIC  X(0001).
           05  TERMSI PIC  X(0011).
      *    -------------------------------
           05  STACCTL PIC S9(0004) COMP.
           05  STACCTF PIC  X(0001).
           05  FILLER REDEFINES STACCTF.
               10  STACCTA PIC  X(0001).
           05  STACCTI PIC  X(0013).
      *    -------------------------------
           05  EXPDTEHL PIC S9(0004) COMP.
           05  EXPDTEHF PIC  X(0001).
           05  FILLER REDEFINES EXPDTEHF.
               10  EXPDTEHA PIC  X(0001).
           05  EXPDTEHI PIC  X(0014).
      *    -------------------------------
           05  EXPDTEL PIC S9(0004) COMP.
           05  EXPDTEF PIC  X(0001).
           05  FILLER REDEFINES EXPDTEF.
               10  EXPDTEA PIC  X(0001).
           05  EXPDTEI PIC  X(0008).
      *    -------------------------------
           05  BENECAPL PIC S9(0004) COMP.
           05  BENECAPF PIC  X(0001).
           05  FILLER REDEFINES BENECAPF.
               10  BENECAPA PIC  X(0001).
           05  BENECAPI PIC  X(0013).
      *    -------------------------------
           05  BENEL PIC S9(0004) COMP.
           05  BENEF PIC  X(0001).
           05  FILLER REDEFINES BENEF.
               10  BENEA PIC  X(0001).
           05  BENEI PIC  X(0009).
      *    -------------------------------
           05  FORMTYPL PIC S9(0004) COMP.
           05  FORMTYPF PIC  X(0001).
           05  FILLER REDEFINES FORMTYPF.
               10  FORMTYPA PIC  X(0001).
           05  FORMTYPI PIC  X(0001).
      *    -------------------------------
           05  PMTTYPEL PIC S9(0004) COMP.
           05  PMTTYPEF PIC  X(0001).
           05  FILLER REDEFINES PMTTYPEF.
               10  PMTTYPEA PIC  X(0001).
           05  PMTTYPEI PIC  X(0001).
      *    -------------------------------
           05  PAYEEL PIC S9(0004) COMP.
           05  PAYEEF PIC  X(0001).
           05  FILLER REDEFINES PAYEEF.
               10  PAYEEA PIC  X(0001).
           05  PAYEEI PIC  X(0002).
      *    -------------------------------
           05  CHECKNOL PIC S9(0004) COMP.
           05  CHECKNOF PIC  X(0001).
           05  FILLER REDEFINES CHECKNOF.
               10  CHECKNOA PIC  X(0001).
           05  CHECKNOI PIC  X(0007).
      *    -------------------------------
           05  OFFLINEL PIC S9(0004) COMP.
           05  OFFLINEF PIC  X(0001).
           05  FILLER REDEFINES OFFLINEF.
               10  OFFLINEA PIC  X(0001).
           05  OFFLINEI PIC  X(0001).
      *    -------------------------------
           05  HOLDTILL PIC S9(0004) COMP.
           05  HOLDTILF PIC  X(0001).
           05  FILLER REDEFINES HOLDTILF.
               10  HOLDTILA PIC  X(0001).
           05  HOLDTILI PIC  X(0008).
      *    -------------------------------
           05  NOTE1L PIC S9(0004) COMP.
           05  NOTE1F PIC  X(0001).
           05  FILLER REDEFINES NOTE1F.
               10  NOTE1A PIC  X(0001).
           05  NOTE1I PIC  X(0060).
      *    -------------------------------
           05  ITRB1L PIC S9(0004) COMP.
           05  ITRB1F PIC  X(0001).
           05  FILLER REDEFINES ITRB1F.
               10  ITRB1A PIC  X(0001).
           05  ITRB1I PIC  X(0008).
      *    -------------------------------
           05  NOTE2L PIC S9(0004) COMP.
           05  NOTE2F PIC  X(0001).
           05  FILLER REDEFINES NOTE2F.
               10  NOTE2A PIC  X(0001).
           05  NOTE2I PIC  X(0060).
      *    -------------------------------
           05  ITRB2L PIC S9(0004) COMP.
           05  ITRB2F PIC  X(0001).
           05  FILLER REDEFINES ITRB2F.
               10  ITRB2A PIC  X(0001).
           05  ITRB2I PIC  X(0010).
      *    -------------------------------
           05  EOBYNL PIC S9(0004) COMP.
           05  EOBYNF PIC  X(0001).
           05  FILLER REDEFINES EOBYNF.
               10  EOBYNA PIC  X(0001).
           05  EOBYNI PIC  X(0001).
      *    -------------------------------
           05  CLMFMYNL PIC S9(0004) COMP.
           05  CLMFMYNF PIC  X(0001).
           05  FILLER REDEFINES CLMFMYNF.
               10  CLMFMYNA PIC  X(0001).
           05  CLMFMYNI PIC  X(0001).
      *    -------------------------------
           05  SURVYYNL PIC S9(0004) COMP.
           05  SURVYYNF PIC  X(0001).
           05  FILLER REDEFINES SURVYYNF.
               10  SURVYYNA PIC  X(0001).
           05  SURVYYNI PIC  X(0001).
      *    -------------------------------
           05  SPRELYNL PIC S9(0004) COMP.
           05  SPRELYNF PIC  X(0001).
           05  FILLER REDEFINES SPRELYNF.
               10  SPRELYNA PIC  X(0001).
           05  SPRELYNI PIC  X(0001).
      *    -------------------------------
           05  PINTHL PIC S9(0004) COMP.
           05  PINTHF PIC  X(0001).
           05  FILLER REDEFINES PINTHF.
               10  PINTHA PIC  X(0001).
           05  PINTHI PIC  X(0004).
      *    -------------------------------
           05  PINTL PIC S9(0004) COMP.
           05  PINTF PIC  X(0001).
           05  FILLER REDEFINES PINTF.
               10  PINTA PIC  X(0001).
           05  PINTI PIC  X(0008).
      *    -------------------------------
           05  ITRBYNL PIC S9(0004) COMP.
           05  ITRBYNF PIC  X(0001).
           05  FILLER REDEFINES ITRBYNF.
               10  ITRBYNA PIC  X(0001).
           05  ITRBYNI PIC  X(0001).
      *    -------------------------------
           05  PROOFDTL PIC S9(0004) COMP.
           05  PROOFDTF PIC  X(0001).
           05  FILLER REDEFINES PROOFDTF.
               10  PROOFDTA PIC  X(0001).
           05  PROOFDTI PIC  X(0008).
      *    -------------------------------
           05  AIGFRMHL PIC S9(0004) COMP.
           05  AIGFRMHF PIC  X(0001).
           05  FILLER REDEFINES AIGFRMHF.
               10  AIGFRMHA PIC  X(0001).
           05  AIGFRMHI PIC  X(0012).
      *    -------------------------------
           05  AIGFROML PIC S9(0004) COMP.
           05  AIGFROMF PIC  X(0001).
           05  FILLER REDEFINES AIGFROMF.
               10  AIGFROMA PIC  X(0001).
           05  AIGFROMI PIC  X(0008).
      *    -------------------------------
           05  PMTHDGAL PIC S9(0004) COMP.
           05  PMTHDGAF PIC  X(0001).
           05  FILLER REDEFINES PMTHDGAF.
               10  PMTHDGAA PIC  X(0001).
           05  PMTHDGAI PIC  X(0066).
      *    -------------------------------
           05  EPYFROML PIC S9(0004) COMP.
           05  EPYFROMF PIC  X(0001).
           05  FILLER REDEFINES EPYFROMF.
               10  EPYFROMA PIC  X(0001).
           05  EPYFROMI PIC  X(0008).
      *    -------------------------------
           05  EPYTHRUL PIC S9(0004) COMP.
           05  EPYTHRUF PIC  X(0001).
           05  FILLER REDEFINES EPYTHRUF.
               10  EPYTHRUA PIC  X(0001).
           05  EPYTHRUI PIC  X(0008).
      *    -------------------------------
           05  EDAYSL PIC S9(0004) COMP.
           05  EDAYSF PIC  X(0001).
           05  FILLER REDEFINES EDAYSF.
               10  EDAYSA PIC  X(0001).
           05  EDAYSI PIC  S9(6).
      *    -------------------------------
           05  EPYAMTL PIC S9(0004) COMP.
           05  EPYAMTF PIC  X(0001).
           05  FILLER REDEFINES EPYAMTF.
               10  EPYAMTA PIC  X(0001).
           05  EPYAMTI PIC  S9(8)V99.
      *    -------------------------------
           05  ERESVL PIC S9(0004) COMP.
           05  ERESVF PIC  X(0001).
           05  FILLER REDEFINES ERESVF.
               10  ERESVA PIC  X(0001).
           05  ERESVI PIC  S9(7)V99.
      *    -------------------------------
           05  ETYPEL PIC S9(0004) COMP.
           05  ETYPEF PIC  X(0001).
           05  FILLER REDEFINES ETYPEF.
               10  ETYPEA PIC  X(0001).
           05  ETYPEI PIC  X(0001).
      *    -------------------------------
           05  EEXPENSL PIC S9(0004) COMP.
           05  EEXPENSF PIC  X(0001).
           05  FILLER REDEFINES EEXPENSF.
               10  EEXPENSA PIC  X(0001).
           05  EEXPENSI PIC  9(6)V99.
      *    -------------------------------
           05  CPYFROML PIC S9(0004) COMP.
           05  CPYFROMF PIC  X(0001).
           05  FILLER REDEFINES CPYFROMF.
               10  CPYFROMA PIC  X(0001).
           05  CPYFROMI PIC  X(0008).
      *    -------------------------------
           05  CPYTHRUL PIC S9(0004) COMP.
           05  CPYTHRUF PIC  X(0001).
           05  FILLER REDEFINES CPYTHRUF.
               10  CPYTHRUA PIC  X(0001).
           05  CPYTHRUI PIC  X(0008).
      *    -------------------------------
           05  CDAYSL PIC S9(0004) COMP.
           05  CDAYSF PIC  X(0001).
           05  FILLER REDEFINES CDAYSF.
               10  CDAYSA PIC  X(0001).
           05  CDAYSI PIC  9(6).
      *    -------------------------------
           05  CPYAMTL PIC S9(0004) COMP.
           05  CPYAMTF PIC  X(0001).
           05  FILLER REDEFINES CPYAMTF.
               10  CPYAMTA PIC  X(0001).
           05  CPYAMTI PIC  S9(8)V99.
      *    -------------------------------
           05  CRESVL PIC S9(0004) COMP.
           05  CRESVF PIC  X(0001).
           05  FILLER REDEFINES CRESVF.
               10  CRESVA PIC  X(0001).
           05  CRESVI PIC  9(6)V99.
      *    -------------------------------
           05  CEXPENSL PIC S9(0004) COMP.
           05  CEXPENSF PIC  X(0001).
           05  FILLER REDEFINES CEXPENSF.
               10  CEXPENSA PIC  X(0001).
           05  CEXPENSI PIC  9(6)V99.
      *    -------------------------------
           05  ERRMSG0L PIC S9(0004) COMP.
           05  ERRMSG0F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG0F.
               10  ERRMSG0A PIC  X(0001).
           05  ERRMSG0I PIC  X(0072).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0072).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0072).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  99.
      *    -------------------------------
           05  PF6TITL PIC S9(0004) COMP.
           05  PF6TITF PIC  X(0001).
           05  FILLER REDEFINES PF6TITF.
               10  PF6TITA PIC  X(0001).
           05  PF6TITI PIC  X(0016).
       01  EL156AO REDEFINES EL156AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEAO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMAO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQUO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTYPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMSTATO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FSTNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRIMHDGO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PSUFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUSO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANHO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANNOO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DUEDAYHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DUEDAYO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZINTHO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZINTO PIC  9.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PTHRHDGO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTPAIDO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVERO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCURO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTSTATO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFECTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERMSO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STACCTO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTEHO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENECAPO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENEO PIC  Z(6).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYEEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHECKNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OFFLINEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOLDTILO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOTE1O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITRB1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOTE2O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITRB2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOBYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMFMYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SURVYYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SPRELYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PINTHO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PINTO PIC  Z,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ITRBYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROOFDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIGFRMHO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIGFROMO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTHDGAO PIC  X(0066).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPYFROMO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPYTHRUO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EDAYSO PIC  Z(4)9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPYAMTO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERESVO PIC  Z(5).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ETYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEXPENSO PIC  Z(5).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPYFROMO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPYTHRUO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDAYSO PIC  Z(4)9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPYAMTO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRESVO PIC  Z(5).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEXPENSO PIC  Z(5).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG0O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF6TITO PIC  X(0016).
      *    -------------------------------
       01  EL156BI REDEFINES EL156AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEBL PIC S9(0004) COMP.
           05  RUNDTEBF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEBF.
               10  RUNDTEBA PIC  X(0001).
           05  RUNDTEBI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMBL PIC S9(0004) COMP.
           05  RUNTIMBF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMBF.
               10  RUNTIMBA PIC  X(0001).
           05  RUNTIMBI PIC  X(0005).
      *    -------------------------------
           05  PAYDTBL PIC S9(0004) COMP.
           05  PAYDTBF PIC  X(0001).
           05  FILLER REDEFINES PAYDTBF.
               10  PAYDTBA PIC  X(0001).
           05  PAYDTBI PIC  X(0008).
      *    -------------------------------
           05  PAYTYPBL PIC S9(0004) COMP.
           05  PAYTYPBF PIC  X(0001).
           05  FILLER REDEFINES PAYTYPBF.
               10  PAYTYPBA PIC  X(0001).
           05  PAYTYPBI PIC  X(0015).
      *    -------------------------------
           05  PAYAMTBL PIC S9(0004) COMP.
           05  PAYAMTBF PIC  X(0001).
           05  FILLER REDEFINES PAYAMTBF.
               10  PAYAMTBA PIC  X(0001).
           05  PAYAMTBI PIC  X(0011).
      *    -------------------------------
           05  LOANHBL PIC S9(0004) COMP.
           05  LOANHBF PIC  X(0001).
           05  FILLER REDEFINES LOANHBF.
               10  LOANHBA PIC  X(0001).
           05  LOANHBI PIC  X(0012).
      *    -------------------------------
           05  LOANNBL PIC S9(0004) COMP.
           05  LOANNBF PIC  X(0001).
           05  FILLER REDEFINES LOANNBF.
               10  LOANNBA PIC  X(0001).
           05  LOANNBI PIC  X(0025).
      *    -------------------------------
           05  CKNUMHDL PIC S9(0004) COMP.
           05  CKNUMHDF PIC  X(0001).
           05  FILLER REDEFINES CKNUMHDF.
               10  CKNUMHDA PIC  X(0001).
           05  CKNUMHDI PIC  X(0012).
      *    -------------------------------
           05  CKNUMBL PIC S9(0004) COMP.
           05  CKNUMBF PIC  X(0001).
           05  FILLER REDEFINES CKNUMBF.
               10  CKNUMBA PIC  X(0001).
           05  CKNUMBI PIC  X(0007).
      *    -------------------------------
           05  AGTHDGL PIC S9(0004) COMP.
           05  AGTHDGF PIC  X(0001).
           05  FILLER REDEFINES AGTHDGF.
               10  AGTHDGA PIC  X(0001).
           05  AGTHDGI PIC  X(0016).
      *    -------------------------------
           05  NAMEBL PIC S9(0004) COMP.
           05  NAMEBF PIC  X(0001).
           05  FILLER REDEFINES NAMEBF.
               10  NAMEBA PIC  X(0001).
           05  NAMEBI PIC  X(0030).
      *    -------------------------------
           05  NAMEB2L PIC S9(0004) COMP.
           05  NAMEB2F PIC  X(0001).
           05  FILLER REDEFINES NAMEB2F.
               10  NAMEB2A PIC  X(0001).
           05  NAMEB2I PIC  X(0030).
      *    -------------------------------
           05  ADDR1BL PIC S9(0004) COMP.
           05  ADDR1BF PIC  X(0001).
           05  FILLER REDEFINES ADDR1BF.
               10  ADDR1BA PIC  X(0001).
           05  ADDR1BI PIC  X(0030).
      *    -------------------------------
           05  ADDR1B2L PIC S9(0004) COMP.
           05  ADDR1B2F PIC  X(0001).
           05  FILLER REDEFINES ADDR1B2F.
               10  ADDR1B2A PIC  X(0001).
           05  ADDR1B2I PIC  X(0030).
      *    -------------------------------
           05  ADDR2BL PIC S9(0004) COMP.
           05  ADDR2BF PIC  X(0001).
           05  FILLER REDEFINES ADDR2BF.
               10  ADDR2BA PIC  X(0001).
           05  ADDR2BI PIC  X(0030).
      *    -------------------------------
           05  ADDR2B2L PIC S9(0004) COMP.
           05  ADDR2B2F PIC  X(0001).
           05  FILLER REDEFINES ADDR2B2F.
               10  ADDR2B2A PIC  X(0001).
           05  ADDR2B2I PIC  X(0030).
      *    -------------------------------
           05  CITYSTBL PIC S9(0004) COMP.
           05  CITYSTBF PIC  X(0001).
           05  FILLER REDEFINES CITYSTBF.
               10  CITYSTBA PIC  X(0001).
           05  CITYSTBI PIC  X(0030).
      *    -------------------------------
           05  CITYST2L PIC S9(0004) COMP.
           05  CITYST2F PIC  X(0001).
           05  FILLER REDEFINES CITYST2F.
               10  CITYST2A PIC  X(0001).
           05  CITYST2I PIC  X(0030).
      *    -------------------------------
           05  ZIPBL PIC S9(0004) COMP.
           05  ZIPBF PIC  X(0001).
           05  FILLER REDEFINES ZIPBF.
               10  ZIPBA PIC  X(0001).
           05  ZIPBI PIC  X(0009).
      *    -------------------------------
           05  ZIPB2L PIC S9(0004) COMP.
           05  ZIPB2F PIC  X(0001).
           05  FILLER REDEFINES ZIPB2F.
               10  ZIPB2A PIC  X(0001).
           05  ZIPB2I PIC  X(0009).
      *    -------------------------------
           05  INTHDBL PIC S9(0004) COMP.
           05  INTHDBF PIC  X(0001).
           05  FILLER REDEFINES INTHDBF.
               10  INTHDBA PIC  X(0001).
           05  INTHDBI PIC  X(0012).
      *    -------------------------------
           05  INTAMTBL PIC S9(0004) COMP.
           05  INTAMTBF PIC  X(0001).
           05  FILLER REDEFINES INTAMTBF.
               10  INTAMTBA PIC  X(0001).
           05  INTAMTBI PIC  X(0011).
      *    -------------------------------
           05  PMTHDGBL PIC S9(0004) COMP.
           05  PMTHDGBF PIC  X(0001).
           05  FILLER REDEFINES PMTHDGBF.
               10  PMTHDGBA PIC  X(0001).
           05  PMTHDGBI PIC  X(0034).
      *    -------------------------------
           05  CLMNOBL PIC S9(0004) COMP.
           05  CLMNOBF PIC  X(0001).
           05  FILLER REDEFINES CLMNOBF.
               10  CLMNOBA PIC  X(0001).
           05  CLMNOBI PIC  X(0007).
      *    -------------------------------
           05  CARRBL PIC S9(0004) COMP.
           05  CARRBF PIC  X(0001).
           05  FILLER REDEFINES CARRBF.
               10  CARRBA PIC  X(0001).
           05  CARRBI PIC  X(0001).
      *    -------------------------------
           05  CERTNOBL PIC S9(0004) COMP.
           05  CERTNOBF PIC  X(0001).
           05  FILLER REDEFINES CERTNOBF.
               10  CERTNOBA PIC  X(0001).
           05  CERTNOBI PIC  X(0010).
      *    -------------------------------
           05  SUFXBL PIC S9(0004) COMP.
           05  SUFXBF PIC  X(0001).
           05  FILLER REDEFINES SUFXBF.
               10  SUFXBA PIC  X(0001).
           05  SUFXBI PIC  X(0001).
      *    -------------------------------
           05  CLMTYPBL PIC S9(0004) COMP.
           05  CLMTYPBF PIC  X(0001).
           05  FILLER REDEFINES CLMTYPBF.
               10  CLMTYPBA PIC  X(0001).
           05  CLMTYPBI PIC  X(0006).
      *    -------------------------------
           05  INCURBL PIC S9(0004) COMP.
           05  INCURBF PIC  X(0001).
           05  FILLER REDEFINES INCURBF.
               10  INCURBA PIC  X(0001).
           05  INCURBI PIC  X(0008).
      *    -------------------------------
           05  PYFROMBL PIC S9(0004) COMP.
           05  PYFROMBF PIC  X(0001).
           05  FILLER REDEFINES PYFROMBF.
               10  PYFROMBA PIC  X(0001).
           05  PYFROMBI PIC  X(0008).
      *    -------------------------------
           05  PYTHRUBL PIC S9(0004) COMP.
           05  PYTHRUBF PIC  X(0001).
           05  FILLER REDEFINES PYTHRUBF.
               10  PYTHRUBA PIC  X(0001).
           05  PYTHRUBI PIC  X(0008).
      *    -------------------------------
           05  DAYSBL PIC S9(0004) COMP.
           05  DAYSBF PIC  X(0001).
           05  FILLER REDEFINES DAYSBF.
               10  DAYSBA PIC  X(0001).
           05  DAYSBI PIC  X(0005).
      *    -------------------------------
           05  BYBL PIC S9(0004) COMP.
           05  BYBF PIC  X(0001).
           05  FILLER REDEFINES BYBF.
               10  BYBA PIC  X(0001).
           05  BYBI PIC  X(0004).
      *    -------------------------------
           05  PROOFDBL PIC S9(0004) COMP.
           05  PROOFDBF PIC  X(0001).
           05  FILLER REDEFINES PROOFDBF.
               10  PROOFDBA PIC  X(0001).
           05  PROOFDBI PIC  X(0008).
      *    -------------------------------
           05  ACCTBL PIC S9(0004) COMP.
           05  ACCTBF PIC  X(0001).
           05  FILLER REDEFINES ACCTBF.
               10  ACCTBA PIC  X(0001).
           05  ACCTBI PIC  X(0010).
      *    -------------------------------
           05  STATEBL PIC S9(0004) COMP.
           05  STATEBF PIC  X(0001).
           05  FILLER REDEFINES STATEBF.
               10  STATEBA PIC  X(0001).
           05  STATEBI PIC  X(0002).
      *    -------------------------------
           05  GROUPBL PIC S9(0004) COMP.
           05  GROUPBF PIC  X(0001).
           05  FILLER REDEFINES GROUPBF.
               10  GROUPBA PIC  X(0001).
           05  GROUPBI PIC  X(0006).
      *    -------------------------------
           05  ACARRBL PIC S9(0004) COMP.
           05  ACARRBF PIC  X(0001).
           05  FILLER REDEFINES ACARRBF.
               10  ACARRBA PIC  X(0001).
           05  ACARRBI PIC  X(0001).
      *    -------------------------------
           05  ACCTNMBL PIC S9(0004) COMP.
           05  ACCTNMBF PIC  X(0001).
           05  FILLER REDEFINES ACCTNMBF.
               10  ACCTNMBA PIC  X(0001).
           05  ACCTNMBI PIC  X(0030).
      *    -------------------------------
           05  UNDISBL PIC S9(0004) COMP.
           05  UNDISBF PIC  X(0001).
           05  FILLER REDEFINES UNDISBF.
               10  UNDISBA PIC  X(0001).
           05  UNDISBI PIC  X(0010).
      *    -------------------------------
           05  RATEBL PIC S9(0004) COMP.
           05  RATEBF PIC  X(0001).
           05  FILLER REDEFINES RATEBF.
               10  RATEBA PIC  X(0001).
           05  RATEBI PIC  X(0006).
      *    -------------------------------
           05  ERMSG1BL PIC S9(0004) COMP.
           05  ERMSG1BF PIC  X(0001).
           05  FILLER REDEFINES ERMSG1BF.
               10  ERMSG1BA PIC  X(0001).
           05  ERMSG1BI PIC  X(0072).
      *    -------------------------------
           05  ENTPFBL PIC S9(0004) COMP.
           05  ENTPFBF PIC  X(0001).
           05  FILLER REDEFINES ENTPFBF.
               10  ENTPFBA PIC  X(0001).
           05  ENTPFBI PIC  99.
       01  EL156BO REDEFINES EL156AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEBO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMBO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYDTBO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYTYPBO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYAMTBO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANHBO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANNBO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKNUMHDO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKNUMBO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTHDGO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAMEBO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAMEB2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR1BO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR1B2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR2BO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR2B2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITYSTBO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITYST2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPBO PIC  ZZZZ99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPB2O PIC  ZZZZZZZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTHDBO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INTAMTBO PIC  ZZZZ,ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTHDGBO PIC  X(0034).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMNOBO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRBO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTNOBO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUFXBO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTYPBO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCURBO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PYFROMBO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PYTHRUBO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAYSBO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYBO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROOFDBO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTBO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEBO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPBO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRBO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTNMBO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UNDISBO PIC  ZZZ,Z99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATEBO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMSG1BO PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTPFBO PIC  X(0002).
      *    -------------------------------
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
00795  01  DFHCOMMAREA             PIC X(1024).
00796
00797 *    COPY ELCMSTR.
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
00798      EJECT
00799 *    COPY ELCCNTL.
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
00800      EJECT
00801 *    COPY ELCCERT.
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
00802      EJECT
00803 *    COPY ELCTRLR.
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
00804      EJECT
00805 *    COPY ELCACTQ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCACTQ.                            *
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY QUE FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 60     RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELACTQ             RKP=2,LEN=20          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCACTQ                          *
00017 ******************************************************************
00018
00019  01  ACTIVITY-QUE.
00020      12  AQ-RECORD-ID                PIC XX.
00021          88  VALID-AQ-ID                VALUE 'AQ'.
00022
00023      12  AQ-CONTROL-PRIMARY.
00024          16  AQ-COMPANY-CD           PIC X.
00025          16  AQ-CARRIER              PIC X.
00026          16  AQ-CLAIM-NO             PIC X(7).
00027          16  AQ-CERT-NO.
00028              20  AQ-CERT-PRIME       PIC X(10).
00029              20  AQ-CERT-SFX         PIC X.
00030
00031      12  AQ-PENDING-ACTIVITY-FLAGS.
00032          88  NO-PENDING-ACTIVITY        VALUE SPACES.
00033          16  AQ-PENDING-PAYMENT-FLAG PIC X.
00034              88  PENDING-PAYMENTS       VALUE '1'.
00035          16  AQ-PENDING-STATUS-FLAG  PIC X.
00036              88  PENDING-FULL-PRINT     VALUE '1'.
00037              88  PENDING-PART-PRINT     VALUE '2'.
00038          16  AQ-PENDING-LETTER-FLAG  PIC X.
00039              88  PENDING-LETTERS        VALUE '1'.
00040          16  AQ-PENDING-CLAIM-RESTORE PIC X.
00041              88  PENDING-RESTORE        VALUE 'C'.
00042              88  PENDING-RESTORE-LETTER VALUE 'L'.
00043
00044      12  FILLER                      PIC X(20).
00045
00046      12  AQ-RESEND-DATE              PIC XX.
00047      12  AQ-FOLLOWUP-DATE            PIC XX.
00048      12  AQ-PAYMENT-COUNTER          PIC S9        COMP-3.
00049      12  AQ-PMT-UNAPPROVED-COUNT     PIC S9        COMP-3.
00050      12  AQ-AUTO-LETTER              PIC X(4).
00051      12  FILLER                      PIC XX.
00052      12  AQ-LAST-UPDATED-BY          PIC S9(4)     COMP.
00053 *****************************************************************
00806      EJECT
00807 *    COPY ERCACCT.
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
00808      EJECT
00809 *    COPY ELCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCHKQ.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH1 = ELCHKQ2 (BY PAYEE)      RKP=9,LEN=26   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID         VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-CONTROL-BY-PAYEE.
DJNA           16  CQ-CONTROL-BY-NUMBER.
DJNA               20  CQ-COMPANY-CD-A1     PIC X.
DJNA               20  CQ-CONTROL-NUMBER-A1 PIC S9(8)      COMP.
00030          16  CQ-PAYEE-CARRIER        PIC X.
00031          16  CQ-PAYEE-GROUPING       PIC X(6).
00032          16  CQ-PAYEE-STATE          PIC XX.
00033          16  CQ-PAYEE-BENE-ACCT      PIC X(10).
00034          16  CQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00035
00036      12  CQ-DMD-CONTROL  REDEFINES  CQ-CONTROL-BY-PAYEE.
00037          16  CQ-DMD-COMPANY-CD-A2    PIC X.
00038          16  CQ-DMD-PAYEE-TYPE-A2    PIC X.
00039          16  CQ-DMD-BENE-CODE-A2     PIC X(10).
00040          16  CQ-DMD-CLAIM-NO-A2      PIC X(7).
00041          16  CQ-DMD-TIME-SEQ-A2      PIC S9(7)       COMP.
00042          16  FILLER                  PIC X(3).
00043
00044      12  CQ-ENTRY-TYPE               PIC X.
00045              88  CHECK-ON-QUE           VALUE 'Q'.
00046              88  ALIGNMENT-CHECK        VALUE 'A'.
00047              88  SPOILED-CHECK          VALUE 'S'.
00048              88  PAYMENT-ABORTED        VALUE 'X'.
00049
00050      12  CQ-CLAIM-MAST-CNTL.
00051          16  CQ-CARRIER              PIC X.
00052          16  CQ-CLAIM-NO             PIC X(7).
00053          16  CQ-CERT-NO.
00054              20  CQ-CERT-PRIME       PIC X(10).
00055              20  CQ-CERT-SFX         PIC X.
00056          16  CQ-CLAIM-TYPE           PIC X.
00057              88  CQ-LIFE-CLAIM          VALUE 'L'.
00058              88  CQ-AH-CLAIM            VALUE 'A'.
00059          16  CQ-CLAIM-SUB-TYPE       PIC X.
00060              88  CQ-FIXED-COVERAGE      VALUE '1'.
00061              88  CQ-O-B-COVERAGE        VALUE '2'.
00062              88  CQ-OPEN-END-COVERAGE   VALUE '3'.
00063
00064      12  CQ-PMT-TRLR-SEQUENCE        PIC S9(4)       COMP.
00065      12  CQ-CHECK-NUMBER             PIC X(7).
00066      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00067      12  CQ-PAYMENT-TYPE             PIC X.
00068              88  CQ-PARTIAL-PAYMENT        VALUE '1'.
00069              88  CQ-FINAL-PAYMENT          VALUE '2'.
00070              88  CQ-LUMP-SUM-PAYMENT       VALUE '3'.
00071              88  CQ-ADDITIONAL-PAYMENT     VALUE '4'.
00072              88  CQ-CHARGEABLE-EXPENSE     VALUE '5'.
00073              88  CQ-NON-CHARGEABLE-EXPENSE VALUE '6'.
00074              88  CQ-LIFE-PREMIUM-REFUND    VALUE '7'.
00075              88  CQ-AH-PREMIUM-REFUND      VALUE '8'.
00076      12  CQ-VOID-INDICATOR           PIC X.
00077              88  CHECK-IS-STOPPED          VALUE 'S'.
00078              88  CHECK-IS-VOID             VALUE 'V'.
00079      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00080      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00081      12  CQ-CHECK-BY-USER            PIC X(4).
00082      12  CQ-PRE-NUMBERING-SW         PIC X.
00083        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00084        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00085
00086      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00087      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00088      12  CQ-LEDGER-FLAG              PIC X(01).
00089      12  CQ-VOID-AFTER-LEDGER        PIC X(01).
00090      12  CQ-LAST-UPDATED-DT          PIC XX.
00091      12  CQ-LAST-UPDATED-HHMMSS      PIC S9(6)       COMP-3.
00092      12  CQ-APPLIED-TO-RCON-DT       PIC XX.
00093
00094      12  FILLER                      PIC X(04).
00095
00096 ******************************************************************
00810      EJECT
00811 *    COPY ELCBENE.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCBENE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.006                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BENEFICIARY FILE                          *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 500   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELBENE                   RKP=2,LEN=12    *
00013 *     ALTERNATE PATH1 = ELBENE2 (ALT BY NAME)    RKP=14,LEN=42   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 *                                                                *
CIDMOD*  NO  CID  MODS  TO  COPYBOOK  ELCBENE                          *
00018 ******************************************************************
013017*                   C H A N G E   L O G
013017*
013017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
013017*-----------------------------------------------------------------
013017*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
013017* EFFECTIVE    NUMBER
013017*-----------------------------------------------------------------
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
082317* 082317  CR2017082100003  PEMA  Add sub type
032019* 032019  CR2019011400002  PEMA  Add email address for ach report
013017******************************************************************
00019
00020  01  BENEFICIARY-MASTER.
00021      12  BE-RECORD-ID                PIC XX.
00022          88  VALID-BE-ID                VALUE 'BE'.
00023
00024      12  BE-CONTROL-PRIMARY.
00025          16  BE-COMPANY-CD           PIC X.
00026          16  BE-RECORD-TYPE          PIC X.
00027              88  BENEFICIARY-RECORD  VALUE 'B'.
00028              88  ADJUSTOR-RECORD     VALUE 'A'.
00029          16  BE-BENEFICIARY          PIC X(10).
00030      12  BE-CONTROL-BY-NAME.
00031          16  BE-COMPANY-CD-A1        PIC X.
00032          16  BE-RECORD-TYPE-A1       PIC X.
00033          16  BE-MAIL-TO-NAME-A1      PIC X(30).
00034          16  BE-ALTERNATE-PRIME-A1   PIC X(10).
00035
00036      12  BE-LAST-MAINT-DT            PIC XX.
00037      12  BE-LAST-MAINT-BY            PIC X(4).
00038      12  BE-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
00039
00040      12  BE-ADDRESS-DATA.
00041          16  BE-MAIL-TO-NAME         PIC X(30).
00042          16  BE-ADDRESS-LINE-1       PIC X(30).
00043          16  BE-ADDRESS-LINE-2       PIC X(30).
00044          16  BE-ADDRESS-LINE-3       PIC X(30).
00045          16  BE-CITY-STATE.
051810             20  BE-CITY             PIC X(28).
051810             20  BE-STATE            PIC XX.
00046          16  BE-ZIP-CODE.
00047              20  BE-ZIP-PRIME.
00048                  24  BE-ZIP-1ST      PIC X.
00049                      88  BE-CANADIAN-POST-CODE
00050                                          VALUE 'A' THRU 'Z'.
00051                  24  FILLER          PIC X(4).
00052              20  BE-ZIP-PLUS4        PIC X(4).
00053          16  BE-CANADIAN-POSTAL-CODE  REDEFINES  BE-ZIP-CODE.
00054              20  BE-CAN-POSTAL-1     PIC XXX.
00055              20  BE-CAN-POSTAL-2     PIC XXX.
00056              20  FILLER              PIC XXX.
00057          16  BE-PHONE-NO             PIC 9(11)     COMP-3.
00058          16  BE-GROUP-CHECKS-Y-N     PIC X.
00059
00060 ******************************************************************
00061 *    THE BE-CARRIER FIELD IS USED BY 'AIG' TO DETERMINE HOW TO   *
00062 *    SET THE CARRIER CODE IN THE PENDING CLAIM FILE.             *
00063 ******************************************************************
00064      12  BE-CARRIER                  PIC X.
00065
00066      12  BE-ADDRESS-DATA2.
00067          16  BE-MAIL-TO-NAME2        PIC X(30).
00068          16  BE-ADDRESS-LINE-12      PIC X(30).
00069          16  BE-ADDRESS-LINE-22      PIC X(30).
00070          16  BE-ADDRESS-LINE-32      PIC X(30).
00071          16  BE-CITY-STATE2.
051810             20  BE-CITY2            PIC X(28).
051810             20  BE-STATE2           PIC XX.
00072          16  BE-ZIP-CODE2.
00073              20  BE-ZIP-PRIME2.
00074                  24  BE-ZIP-1ST2     PIC X.
00075                      88  BE-CANADIAN-POST-CODE2
00076                                          VALUE 'A' THRU 'Z'.
00077                  24  FILLER          PIC X(4).
00078              20  BE-ZIP-PLUS42       PIC X(4).
00079          16  BE-CANADIAN-POSTAL-CODE2 REDEFINES  BE-ZIP-CODE2.
00080              20  BE-CAN-POSTAL-12    PIC XXX.
00081              20  BE-CAN-POSTAL-22    PIC XXX.
00082              20  FILLER              PIC XXX.
00083          16  BE-PHONE-NO2            PIC 9(11)     COMP-3.
               16  BE-ACH-DATA.
                   20  BE-ACH-YES-OR-NO    PIC X.
                       88  BE-ON-ACH       VALUE 'Y'.
                       88  BE-NOT-ON-ACH   VALUE 'N' ' '.
                   20  BE-ACH-ABA-ROUTING-NUMBER
                                           PIC X(15).
                   20  BE-ACH-BANK-ACCOUNT-NUMBER
                                           PIC X(20).
                   20  BE-ACH-SUB-TYPE     PIC XX.
032019             20  BE-ACH-EMAIL-YN     PIC X.
032019                 88  BE-EMAIL-ACH-RPT  VALUE 'Y'.
032019             20  be-ach-email-addr   PIC X(40).
00084          16  BE-BILLING-STMT-DATA.
032019*            20  BE-BSR-PHONE-NUM    PIC 9(11)     COMP-3.
00091              20  BE-BSR-FAX-NUM      PIC 9(11)     COMP-3.
00092              20  BE-OUTPUT-TYPE      PIC X.
00093                  88  BE-FAX-OUTPUT         VALUE 'F'.
00094                  88  BE-PRINT-OUTPUT       VALUE 'P' ' '.
00095
032019     12  filler                      PIC X(16).
00097 ******************************************************************
00812      EJECT
00813 *    COPY ELCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 090  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELARCH                        RKP=2,LEN=8     *
00013 *       ALTERNATE PATH1 = ELARCH2 (RECORD TYPE)  RKP=10,LEN=8    *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  THERE ARE CID MODS IN COPYBOOK ELCARCH                        *
00017 ******************************************************************
00018  01  LETTER-ARCHIVE.
00019      12  LA-RECORD-ID                PIC XX.
00020          88  VALID-LA-ID                VALUE 'LA'.
00021
00022      12  LA-CONTROL-PRIMARY.
00023          16  LA-COMPANY-CD           PIC X.
00024          16  LA-ARCHIVE-NO           PIC S9(8)     COMP.
00025          16  LA-RECORD-TYPE          PIC X.
00026              88  LA-HEADER-DATA         VALUE '1'.
00027              88  LA-ADDRESS-DATA        VALUE '2'.
00028              88  LA-TEXT-DATA           VALUE '3'.
00029              88  LA-FORM-CONTROL-HDR    VALUE '4'.
00030          16  LA-LINE-SEQ-NO          PIC S9(4)     COMP.
00031
00032      12  LA-CONTROL-BY-TYPE.
00033          16  LA-COMPANY-CD-A1        PIC X.
00034          16  LA-RECORD-TYPE-A1       PIC X.
00035          16  LA-ARCHIVE-NO-A1        PIC S9(8)     COMP.
00036          16  LA-LINE-SEQ-NO-A1       PIC S9(4)     COMP.
00037
00038      12  LA-TEXT-RECORD.
00039          16  LA-SKIP-CONTROL         PIC XX.
00040              88  NO-LINES-SKIPPED       VALUE SPACES.
00041              88  SKIP-TO-NEXT-PAGE      VALUE '99'.
00042          16  LA-TEXT-LINE            PIC X(70).
00043
00044      12  LA-ADDRESS-RECORD  REDEFINES  LA-TEXT-RECORD.
00045          16  FILLER                  PIC XX.
00046          16  LA-ADDRESS-LINE         PIC X(30).
00047          16  FILLER                  PIC X(40).
00048
00049      12  LA-HEADER-RECORD  REDEFINES  LA-TEXT-RECORD.
00050          16  FILLER                  PIC XX.
00051          16  LA-CARRIER              PIC X.
00052          16  LA-CLAIM-NO             PIC X(7).
00053          16  LA-CERT-NO.
00054              20  LA-CERT-PRIME       PIC X(10).
00055              20  LA-CERT-SFX         PIC X.
00056          16  LA-NO-OF-COPIES         PIC S9.
00057          16  LA-RESEND-DATE          PIC XX.
00058          16  LA-PROCESSOR-CD         PIC X(4).
00059          16  LA-CREATION-DT          PIC XX.
00060          16  LA-INITIAL-PRINT-DATE   PIC XX.
00061          16  LA-RESEND-PRINT-DATE    PIC XX.
00062          16  LA-CORR-TRLR-SEQ        PIC S9(4)    COMP.
00063          16  LA-1ST-RESEND-PRINT-DT  PIC XX.
CIDMOD*
00064 * -----  16  LA-DMD-ADDITIONAL-FIELDS.
00065 *   I        20  LA-DMD-LETTER-FORM      PIC X(4).
00066 *   I        20  LA-DMD-PROD-CODE        PIC XX.
00067 *   I        20  LA-DMD-RES-ST           PIC XX.
00068 *   I        20  LA-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.
00069 *   I        20  LA-DMD-LETTER-STATUS    PIC X.
00070 *  NEW           88  LA-DMD-LETTER-ONLINE   VALUE '1'.
00071 *  DMD           88  LA-DMD-LETTER-PURGED   VALUE '2'.
00072 *  CHGS          88  LA-DMD-LETTER-RELOADED VALUE '3'.
00073 *   I        20  LA-DMD-LETTER-PURGE-DT  PIC XX.
00074 *   I        20  LA-DMD-LETTER-RELOAD-DT PIC XX.
00075 *   I        20  LA-DMD-UND-CODE         PIC XX.
00076 *   I        20  LA-DMD-BEN-CODE         PIC XX.
00077 *   V    16  FILLER                  PIC X(15).
CIDMOD* -----
CIDMOD*
CIDMOD* REINSERTED  CSO  MODS
CIDMOD*
CIDMOD         16  FILLER.
CIDMOD             20  FILLER                  PIC X(29).
CIDMOD             20  LA-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  LA-CSO-LETTER-ONLINE   VALUE '1'.
CIDMOD                 88  LA-CSO-LETTER-PURGED   VALUE '2'.
CIDMOD                 88  LA-CSO-LETTER-RELOADED VALUE '3'.
CIDMOD             20  LA-CSO-LETTER-PURGE-DT  PIC XX.
CIDMOD             20  LA-CSO-LETTER-RELOAD-DT PIC XX.
CIDMOD*
00078
00079      12  LA-FORM-CONTROL-HEADER REDEFINES  LA-TEXT-RECORD.
00080          16  FILLER                  PIC XX.
00081          16  LA4-CARRIER             PIC X.
00082          16  LA4-CLAIM-NO            PIC X(7).
00083          16  LA4-CERT-NO.
00084              20  LA4-CERT-PRIME      PIC X(10).
00085              20  LA4-CERT-SFX        PIC X.
00086          16  LA4-NO-OF-COPIES        PIC S9.
00087          16  LA4-RESEND-DATE         PIC XX.
00088          16  LA4-PROCESSOR-CD        PIC X(4).
00089          16  LA4-CREATION-DT         PIC XX.
00090          16  LA4-INITIAL-PRINT-DATE  PIC XX.
00091          16  LA4-RESEND-PRINT-DATE   PIC XX.
00092          16  LA4-FORM-TRLR-SEQ       PIC S9(4)    COMP.
00093          16  LA4-FORM-TYPE           PIC X.
00094              88  LA4-INITIAL-FORM    VALUE '1'.
00095              88  LA4-PROGRESS-FORM   VALUE '2'.
00096          16  LA4-FORM-REM-PRINT-DT   PIC X(02).
00097          16  LA4-STATE               PIC X(02).
00098          16  FILLER                  PIC X(31).
00099 ******************************************************************
00814      EJECT
CIDMOD*    COPY ELCDAR.
00001 ******************************************************************
00002 *                                                                *
00003 *   FILE DESC. = DAILY ACTIVITY FILE, FOR PROCESSING NITELY      *
00004 *   FILE TYPE = VSAM,KSDS                                        *
00005 *   RECORD SIZE = 25   RECFORM = FIXED                           *
00006 *   BASE CLUSTER = DLYACTV                                       *
00007 *   LOG = YES                                                    *
00008 *   NARRATIVE - FILE IS BUILT DURING DAYTIME CICS PROCESSING AND *
00009 *               IS THEN PROCESSED BY CYCLE PROCESSING AT NIGHT.  *
00010 *               THIS IS USED TO BUILD THE LOGIC "F" EXTRACT      *
00011 *               RECORDS FOR THOSE CLAIMS WHICH HAVE HAD ACTIVITY *
00012 *               DURING THE DAY. THE EXTRACTS THEN GET READ IN    *
00013 *               BY PROGRAM "LGINFCE".                            *
00014 *                                                                *
00015 ******************************************************************
00016  01  DAILY-ACTIVITY-RECORD.
00017      05  DA-KEY.
00018          10  DA-COMP-CD          PIC X.
00019          10  DA-CARRIER          PIC X.
00020          10  DA-CLAIM-NO         PIC X(7).
00021          10  DA-CERT-NO.
00022              15  DA-CERT-PRIME   PIC X(10).
00023              15  DA-CERT-SFX     PIC X.
00024      05  DA-TRAILER-SEQ-NO       PIC S9(4)  COMP.
00025      05  DA-RECORD-TYPE          PIC X.
00026      05  FILLER                  PIC X(2).
00027 ******************************************************************
CIDMOD     EJECT
00815 *    COPY ERCCOMP.
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
00816      EJECT
00817 *    COPY ERCDMDNT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCDMDNT                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = DMD CERTIFICATE NOTES                *
00008 *                                                                *
00009 *        THIS COPYBOOK IS A REDEFINES OF ERCNOTE -               *
00010 *                                                                *
00011 *        FILE TYPE= VSAM,KSDS                                    *
00012 *        RECORD SIZE = 825    RECFORM = FIXED                    *
00013 *                                                                *
00014 *        BASE CLUSTER = ERNOTE        RKP=2,LEN=33               *
00015 *                                                                *
00016 *        LOG = YES                                               *
00017 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00018 *                                                                *
00019 ******************************************************************
00020
00021  01  CERTIFICATE-NOTE.
00022      12  CN-RECORD-ID                     PIC  XX.
00023          88  VALID-CN-ID                      VALUE 'CN'.
00024
00025      12  CN-CONTROL-PRIMARY.
00026          16  CN-COMPANY-CD                PIC X.
00027          16  CN-CERT-KEY.
00028              20  CN-CARRIER               PIC X.
00029              20  CN-GROUPING.
00030                  24  CN-GROUPING-PREFIX   PIC XXX.
00031                  24  CN-GROUPING-PRIME    PIC XXX.
00032              20  CN-STATE                 PIC XX.
00033              20  CN-ACCOUNT.
00034                  24  CN-ACCOUNT-PREFIX    PIC X(4).
00035                  24  CN-ACCOUNT-PRIME     PIC X(6).
00036              20  CN-CERT-EFF-DT           PIC XX.
00037              20  CN-CERT-NO.
00038                  24  CN-CERT-PRIME        PIC X(10).
00039                  24  CN-CERT-SFX          PIC X.
00040
00041      12  CN-BILLING-START-LINE-NO         PIC 99.
00042      12  CN-BILLING-END-LINE-NO           PIC 99.
00043
00044      12  CN-LINES.
00045          16  CN-LINE                      PIC X(77)  OCCURS 10.
00046
00047      12  CN-CSI-NOTES REDEFINES CN-LINES.
00048          16  CN-CSI-TEXT-NOTES            PIC X(77)  OCCURS 6.
00049          16  CN-CSI-GENERAL-DATA-AREA.
00050              20  CN-CSI-GENERAL-DATA      PIC X(77)  OCCURS 2.
00051
00052          16  CN-CSI-GENERAL-DATA-R REDEFINES
00053              CN-CSI-GENERAL-DATA-AREA.
00054              20  CN-CSI-GEN-NOC-KEY           PIC X(11).
00055              20  CN-CSI-GEN-PRI-INSD-1ST-NAME PIC X(15).
00056              20  CN-CSI-GEN-SEC-INSD-1ST-NAME PIC X(15).
00057              20  CN-CSI-GEN-INSD-WORK-PHONE   PIC X(10).
00058              20  CN-CSI-GEN-INFRM-1ST-NAME    PIC X(15).
00059              20  CN-CSI-GEN-INFRM-LAST-NAME   PIC X(20).
00060              20  CN-CSI-GEN-INFRM-MI          PIC X.
00061              20  CN-CSI-GEN-INFRM-PHONE       PIC X(10).
00062              20  CN-CSI-GEN-INFRM-REL         PIC X(15).
00063              20  FILLER                       PIC XX.
00064              20  CN-CSI-GEN-DATA-SOURCE       PIC XX.
00065              20  FILLER                       PIC X(38).
00066
00067          16  CN-CSI-PRODUCT-DATA-AREA.
00068              20  CN-CSI-PRODUCT-DATA      PIC X(77)  OCCURS 2.
00069
00070          16  CN-CSI-CREDIT-CARD-DATA REDEFINES
00071              CN-CSI-PRODUCT-DATA-AREA.
00072              20  CN-CSI-CC-BILL-BANK-ID   PIC X(6).
00073              20  CN-CSI-CC-CANCEL-CD      PIC XX.
00074              20  CN-CSI-CC-CANCEL-DT      PIC X(8).
00075              20  CN-CSI-CC-CARD-TYPE      PIC XX.
00076              20  CN-CSI-CC-CHANGE-AGE     PIC 999.
00077              20  CN-CSI-CC-DIAGNOSIS-CD   PIC X(6).
00078              20  FILLER                   PIC XX.
00079              20  CN-CSI-CC-INSURED-BAL    PIC S9(5)V99  COMP-3.
00080              20  CN-CSI-CC-INTEREST-AMT   PIC S9(5)V99  COMP-3.
00081              20  CN-CSI-CC-INTEREST-PAID  PIC X.
00082              20  CN-CSI-CC-ISSUE-ST       PIC XX.
00083              20  CN-CSI-CC-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
00084              20  CN-CSI-CC-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
00085              20  CN-CSI-CC-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
00086              20  CN-CSI-CC-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
00087              20  CN-CSI-CC-OLD-ACCT-NO    PIC X(20).
00088              20  CN-CSI-CC-POLICY-TYPE    PIC XXX.
00089              20  CN-CSI-CC-PREMIUM-AMT    PIC S999V99   COMP-3.
00090              20  CN-CSI-CC-PREMIUM-RT     PIC S999V999  COMP-3.
00091              20  CN-CSI-CC-PREV-CLAIM-NO  PIC X(7).
00092              20  CN-CSI-CC-SIGNED-DT      PIC X(8).
00093              20  CN-CSI-CC-SPECIAL-TERM   PIC S999      COMP-3.
00094              20  CN-CSI-CC-STMNT-DT       PIC X(8).
00095              20  CN-CSI-CC-TERM-AGE       PIC 999.
00096              20  CN-CSI-CC-TOL-BALANCE    PIC S9(5)V99  COMP-3.
00097              20  CN-CSI-CC-WAIV-PREM-FLAG PIC X.
00098              20  CN-CSI-CC-ISSUE-DT       PIC X(8).
00099              20  CN-CSI-CC-BEN-CALC-SW    PIC X.
00100              20  CN-CSI-CC-TERM-ROUND-SW  PIC X.
00101              20  FILLER                   PIC X(25).
00102
00103          16  CN-CSI-FAMILY-LEAVE-DATA REDEFINES
00104              CN-CSI-CREDIT-CARD-DATA.
00105              20  CN-CSI-FL-BILL-BANK-ID   PIC X(6).
00106              20  CN-CSI-FL-CANCEL-CD      PIC XX.
00107              20  CN-CSI-FL-CANCEL-DT      PIC X(8).
00108              20  CN-CSI-FL-CARD-TYPE      PIC XX.
00109              20  CN-CSI-FL-CHANGE-AGE     PIC 999.
00110              20  CN-CSI-FL-DIAGNOSIS-CD   PIC X(6).
00111              20  FILLER                   PIC XX.
00112              20  CN-CSI-FL-INSURED-BAL    PIC S9(5)V99  COMP-3.
00113              20  CN-CSI-FL-INTEREST-AMT   PIC S9(5)V99  COMP-3.
00114              20  CN-CSI-FL-INTEREST-PAID  PIC X.
00115              20  CN-CSI-FL-ISSUE-ST       PIC XX.
00116              20  CN-CSI-FL-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
00117              20  CN-CSI-FL-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
00118              20  CN-CSI-FL-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
00119              20  CN-CSI-FL-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
00120              20  CN-CSI-FL-OLD-ACCT-NO    PIC X(20).
00121              20  CN-CSI-FL-POLICY-TYPE    PIC XXX.
00122              20  CN-CSI-FL-PREMIUM-AMT    PIC S999V99   COMP-3.
00123              20  CN-CSI-FL-PREMIUM-RT     PIC S999V999  COMP-3.
00124              20  CN-CSI-FL-PREV-CLAIM-NO  PIC X(7).
00125              20  CN-CSI-FL-SIGNED-DT      PIC X(8).
00126              20  CN-CSI-FL-SPECIAL-TERM   PIC S999      COMP-3.
00127              20  CN-CSI-FL-STMNT-DT       PIC X(8).
00128              20  CN-CSI-FL-TERM-AGE       PIC 999.
00129              20  CN-CSI-FL-TOL-BALANCE    PIC S9(5)V99  COMP-3.
00130              20  CN-CSI-FL-WAIV-PREM-FLAG PIC X.
00131              20  CN-CSI-FL-ISSUE-DT       PIC X(8).
00132              20  CN-CSI-FL-BEN-CALC-SW    PIC X.
00133              20  CN-CSI-FL-TERM-ROUND-SW  PIC X.
00134              20  CN-CSI-FL-LAST-DAY-WRKED PIC X(8).
00135              20  FILLER                   PIC X(17).
00136
00137          16  CN-CSI-SENIOR-LIFE-DATA REDEFINES
00138              CN-CSI-FAMILY-LEAVE-DATA.
00139              20  CN-CSI-SL-BENE-DOB       PIC X(8).
00140              20  CN-CSI-SL-BENE-NAME      PIC X(27).
00141              20  CN-CSI-SL-BENE-REL       PIC X(8).
00142              20  CN-CSI-SL-BENE-SSN       PIC S9(9)     COMP-3.
00143              20  CN-CSI-SL-BILL-BANK-ID   PIC X(6).
00144              20  CN-CSI-SL-CANCEL-DT      PIC X(8).
00145              20  CN-CSI-SL-DIAGNOSIS-CD   PIC X(6).
00146              20  CN-CSI-SL-INT-CHECK-DT   PIC X(8).
00147              20  CN-CSI-SL-INT-CHECK-NO   PIC S9(7)     COMP-3.
00148              20  CN-CSI-SL-INT-ON-PROCEEDS
00149                                           PIC S9(5)V99  COMP-3.
00150              20  CN-CSI-SL-ISSUE-DT       PIC X(8).
00151              20  CN-CSI-SL-ISSUE-ST       PIC XX.
00152              20  CN-CSI-SL-LIFE-PROCEEDS  PIC S9(5)V99  COMP-3.
00153              20  CN-CSI-SL-LOAN-INT-DUE   PIC S9(5)V99  COMP-3.
00154              20  CN-CSI-SL-POLICY-BENEFITS
00155                                           PIC S9(5)V99  COMP-3.
00156              20  CN-CSI-SL-POLICY-TYPE    PIC XXX.
00157              20  CN-CSI-SL-PREM-AMT       PIC S9(5)V99  COMP-3.
00158              20  CN-CSI-SL-PREM-CHECK-DT  PIC X(8).
00159              20  CN-CSI-SL-PREM-CHECK-NO  PIC S9(7)     COMP-3.
00160              20  CN-CSI-SL-PREM-DUE       PIC S9(5)V99  COMP-3.
00161              20  CN-CSI-SL-PREM-MODE      PIC 99.
00162              20  CN-CSI-SL-PREM-REFUND    PIC S9(5)V99  COMP-3.
00163              20  CN-CSI-SL-PREM-SUSP-DT   PIC X(8).
00164              20  CN-CSI-SL-SIGNED-DT      PIC X(8).
00165              20  CN-CSI-SL-STATE-NOT      PIC X.
00166              20  FILLER                   PIC XX.
00167
00168          16  CN-CSI-PURCH-PROP-DATA REDEFINES
00169              CN-CSI-SENIOR-LIFE-DATA.
00170              20  CN-CSI-PP-CARD-TYPE      PIC XX.
00171              20  CN-CSI-PP-CHANGE-AGE     PIC 999.
00172              20  CN-CSI-PP-BEN-PAID-TO-DATE
00173                                           PIC S9(5)V99  COMP-3.
00174              20  CN-CSI-PP-BILL-BANK-ID   PIC X(6).
00175              20  CN-CSI-PP-CANCEL-CD      PIC XX.
00176              20  CN-CSI-PP-CANCEL-DT      PIC X(8).
00177              20  CN-CSI-PP-DIAGNOSIS-CD   PIC X(6).
00178              20  CN-CSI-PP-ISSUE-DT       PIC X(8).
00179              20  CN-CSI-PP-ISSUE-ST       PIC XX.
00180              20  CN-CSI-PP-MANUFACTURER   PIC X(17).
00181              20  CN-CSI-PP-MODEL-NO       PIC X(8).
00182              20  CN-CSI-PP-OLD-ACCT-NO    PIC X(20).
00183              20  CN-CSI-PP-POLICY-TYPE    PIC XXX.
00184              20  CN-CSI-PP-PREMIUM-RT     PIC S999V999  COMP-3.
00185              20  CN-CSI-PP-PREV-CLAIM-NO  PIC X(7).
00186              20  CN-CSI-PP-PURCHASE-DT    PIC X(8).
00187              20  CN-CSI-PP-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
00188              20  CN-CSI-PP-REPAIR         PIC X.
00189              20  CN-CSI-PP-REPLACE        PIC X.
00190              20  CN-CSI-PP-SERIAL-NO      PIC X(16).
00191              20  CN-CSI-PP-SIGNED-DT      PIC X(8).
00192              20  CN-CSI-PP-STMNT-DT       PIC X(8).
00193              20  CN-CSI-PP-TERM-AGE       PIC 999.
00194              20  FILLER                   PIC X(5).
00195
00196          16  CN-CSI-EXT-WARR-DATA REDEFINES
00197              CN-CSI-PURCH-PROP-DATA.
00198              20  CN-CSI-EW-CARD-TYPE      PIC XX.
00199              20  CN-CSI-EW-CHANGE-AGE     PIC 999.
00200              20  CN-CSI-EW-BILL-BANK-ID   PIC X(6).
00201              20  CN-CSI-EW-CANCEL-CD      PIC XX.
00202              20  CN-CSI-EW-CANCEL-DT      PIC X(8).
00203              20  CN-CSI-EW-DIAGNOSIS-CD   PIC X(6).
00204              20  CN-CSI-EW-ISSUE-DT       PIC X(8).
00205              20  CN-CSI-EW-ISSUE-ST       PIC XX.
00206              20  CN-CSI-EW-MANUFACTURER   PIC X(17).
00207              20  CN-CSI-EW-MODEL-NO       PIC X(8).
00208              20  CN-CSI-EW-OLD-ACCT-NO    PIC X(20).
00209              20  CN-CSI-EW-POLICY-TYPE    PIC XXX.
00210              20  CN-CSI-EW-PREMIUM-RT     PIC S999V999  COMP-3.
00211              20  CN-CSI-EW-PREV-CLAIM-NO  PIC X(7).
00212              20  CN-CSI-EW-PURCHASE-DT    PIC X(8).
00213              20  CN-CSI-EW-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
00214              20  CN-CSI-EW-REPAIR-COST    PIC S9(5)V99  COMP-3.
00215              20  CN-CSI-EW-REPLACE        PIC X.
00216              20  CN-CSI-EW-SERIAL-NO      PIC X(16).
00217              20  CN-CSI-EW-SIGNED-DT      PIC X(8).
00218              20  CN-CSI-EW-STMNT-DT       PIC X(8).
00219              20  CN-CSI-EW-TERM-AGE       PIC 999.
00220              20  CN-CSI-EW-WARRANTY-NO    PIC 99.
00221              20  FILLER                   PIC X(4).
00222
00223      12  CN-LAST-MAINT-DT                 PIC XX.
00224      12  CN-LAST-MAINT-HHMMSS             PIC S9(7)     COMP-3.
00225      12  CN-LAST-MAINT-USER               PIC X(4).
00226      12  FILLER                           PIC X(6).
00227
00228 ******************************************************************
00818      EJECT
041710*    COPY ERCCNOT.
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
      *                            COPY ERCPDEF.
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
022122* 022122  CR2021100800003  TANA  Add B and H claim types
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
          12  FILLER                       PIC X(6).
          12  PD-PRODUCT-DATA OCCURS 11.
              16  PD-PROD-CODE             PIC X.
                  88  PD-PROD-LIFE           VALUE 'L'.
                  88  PD-PROD-PROP           VALUE 'P'.
                  88  PD-PROD-AH             VALUE 'A'.
                  88  PD-PROD-IU             VALUE 'I'.
                  88  PD-PROD-GAP            VALUE 'G'.
052614            88  PD-PROD-FAML           VALUE 'F'.
100518            88  PD-PROD-OTH            VALUE 'O'.
022122            88  PD-PROD-BRV            VALUE 'B'.
022122            88  PD-PROD-HOSP           VALUE 'H'.
              16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
022122        16  PD-WAIT-PERIOD.
                  20  pd-wait-days         pic 99.
022122            20  PD-RET-ELIM          PIC X.
022122        16  FILLER                   PIC X.
021222*       16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
021222*       16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
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
          12  FILLER                       PIC X(7).
          12  PD-MAINT-INFORMATION.
              16  PD-LAST-MAINT-DT         PIC X(02).
              16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
              16  PD-LAST-MAINT-BY         PIC X(04).
      *                            COPY ELCCRTT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCRTT.                            *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 552  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
111204******************************************************************
111204*                   C H A N G E   L O G
111204*
111204* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111204*-----------------------------------------------------------------
111204*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111204* EFFECTIVE    NUMBER
111204*-----------------------------------------------------------------
111204* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
040109* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
012010* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
022715* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
012918* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
091318* 091318  CR2018073000001  PEMA  ADD Refund methods
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
111204******************************************************************
00021
00022  01  CERTIFICATE-TRAILERS.
00023      12  CS-RECORD-ID                      PIC XX.
00024          88  VALID-CS-ID                      VALUE 'CS'.
00025
00026      12  CS-CONTROL-PRIMARY.
00027          16  CS-COMPANY-CD                 PIC X.
00028          16  CS-CARRIER                    PIC X.
00029          16  CS-GROUPING                   PIC X(6).
00032          16  CS-STATE                      PIC XX.
00033          16  CS-ACCOUNT                    PIC X(10).
00036          16  CS-CERT-EFF-DT                PIC XX.
00037          16  CS-CERT-NO.
00038              20  CS-CERT-PRIME             PIC X(10).
00039              20  CS-CERT-SFX               PIC X.
               16  CS-TRAILER-TYPE               PIC X.
                   88  COMM-TRLR           VALUE 'A'.
061013             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
040109             88  CERT-DATA-TRLR      VALUE 'C'.
00040
040109     12  CS-DATA-AREA                      PIC X(516).
040109
040109     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
040109         16  CS-BANK-COMMISSION-AREA.
040109             20  CS-BANK-COMMS       OCCURS 10.
040109                 24  CS-AGT                PIC X(10).
040109                 24  CS-COM-TYP            PIC X.
040109                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
040109                 24  CS-RECALC-LV-INDIC    PIC X.
040109                 24  FILLER                PIC X(10).
040109         16  FILLER                        PIC X(256).
040109
061013     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
061013****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
               16  CS-MB-CLAIM-DATA OCCURS 24.
                   20  CS-CLAIM-NO               PIC X(7).
                   20  CS-CLAIM-TYPE             PIC X.
                       88  CS-AH-CLM               VALUE 'A'.
                       88  CS-IU-CLM               VALUE 'I'.
                       88  CS-GP-CLM               VALUE 'G'.
                       88  CS-LF-CLM               VALUE 'L'.
                       88  CS-PR-CLM               VALUE 'P'.
052614                 88  CS-FL-CLM               VALUE 'F'.
100518                 88  CS-OT-CLM               VALUE 'O'.
080322                 88  CS-BR-CLM               VALUE 'B'.
080322                 88  CS-HS-CLM               VALUE 'H'.
                   20  CS-INSURED-TYPE           PIC X.
                       88  CS-PRIM-INSURED          VALUE 'P'.
                       88  CS-CO-BORROWER           VALUE 'C'.
                   20  CS-BENEFIT-PERIOD         PIC 99.
                   20  CS-DAYS-PAID              PIC S9(5) COMP-3.
                   20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
                   20  CS-REMAINING-BENS         PIC S999 COMP-3.
               16  FILLER                        PIC X(12).
040109     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
040109         16  CS-VIN-NUMBER                 PIC X(17).
012010         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
121712         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
121712         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
022715         16  cs-agent-name.
022715             20  cs-agent-fname            pic x(20).
022715             20  cs-agent-mi               pic x.
022715             20  cs-agent-lname            pic x(25).
022715         16  cs-license-no                 pic x(15).
022715         16  cs-npn-number                 pic x(10).
022715         16  cs-agent-edit-status          pic x.
022715             88  cs-ae-refer-to-manager      value 'M'.
022715             88  cs-ae-cover-sheet           value 'C'.
022715             88  cs-ae-sig-form              value 'S'.
022715             88  cs-ae-verified              value 'V'.
022715             88  cs-unidentified-signature   value 'U'.
022715             88  cs-cert-returned            value 'R'.
022715             88  cs-accept-no-commission     value 'N'.
020816         16  cs-year                       pic 9999.
020816         16  cs-make                       pic x(20).
020816         16  cs-model                      pic x(20).
020816         16  cs-future                     pic x(20).
020816         16  cs-vehicle-odometer           pic s9(7) comp-3.
012918         16  cs-claim-verification-status  pic x.
012918             88  cs-clm-ver-eligible         value 'A'.
012918             88  cs-clm-ver-partial-elig     value 'B'.
012918             88  cs-clm-ver-not-eligible     value 'C'.
012918             88  cs-clm-ver-not-elig-opn-clm value 'D'.
012918             88  cs-clm-ver-not-part-elig-rw value 'E'.
012918             88  cs-clm-ver-ND-CERT          value 'F'.
012918             88  cs-clm-ver-spec-other       value 'G'.
012918             88  cs-clam-ver-pratial-corrected
012918                                             value 'H'.
012918             88  cs-clm-ver-no-matches       value 'I'.
012918             88  cs-clm-ver-not-elig-corrected
012918                                             value 'J'.
012918             88  cs-clm-ver-needs-review     value 'R'.
012918             88  cs-clm-ver-sent-to-claims   value 'W'.
091318         16  CS-LF-REFUND-METHOD           PIC X.
091318         16  CS-AH-REFUND-METHOD           PIC X.
020816         16  FILLER                        PIC X(353). *> was 420
121712*        16  FILLER                        PIC X(496).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                CONTROL-FILE CERTIFICATE-MASTER
                                ACTIVITY-TRAILERS ACTIVITY-QUE
                                ACCOUNT-MASTER CHECK-QUE
                                BENEFICIARY-MASTER LETTER-ARCHIVE
                                DAILY-ACTIVITY-RECORD
                                COMPENSATION-MASTER CERTIFICATE-NOTE
                                CERT-NOTE-FILE PRODUCT-MASTER
                                CERTIFICATE-TRAILERS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL156' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00820
00821      IF EIBCALEN = 0
00822          GO TO 8800-UNAUTHORIZED-ACCESS.
00823
00824      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00825      MOVE EIBTRMID               TO QID-TERM.
00826      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00827      MOVE '2'                    TO EMI-SWITCH2.
00828      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
00829      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
00830
00831      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00832      MOVE '5'                    TO DC-OPTION-CODE.
00833      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00834      MOVE DC-BIN-DATE-1          TO WS-TODAY-DATE
00835                                     SAVE-BIN-DATE.
00836      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.
00837      MOVE DC-GREG-DATE-1-YMD     TO SAVE-DATE-YMD.
00838
00839      IF SAVE-DATE-YY > 70
00840          MOVE 19                 TO SAVE-DATE-CC
00841      ELSE
00842          MOVE 20                 TO SAVE-DATE-CC.
00843
052506     IF PI-PROOF-DATE NOT = 0 AND LOW-VALUES
052506         MOVE PI-PROOF-DATE TO WS-PROOF-DATE
052506     END-IF.
052506
013013     IF PI-PRINT-EOB-YN = 'Y' OR 'S'
013013        MOVE PI-PRINT-EOB-YN     TO WS-PRINT-EOB-YN
           ELSE
              MOVE 'N'                 TO WS-PRINT-EOB-YN
           END-IF
020413
020413     IF PI-PRINT-CLM-FRM-YN = 'N'
020413         MOVE PI-PRINT-CLM-FRM-YN TO WS-PRINT-CLM-FRM-YN
020413     ELSE
020413         MOVE 'Y'                TO WS-PRINT-CLM-FRM-YN
020413     END-IF
020413
020413     IF PI-PRINT-SURVEY-YN = 'N'
020413         MOVE PI-PRINT-SURVEY-YN TO WS-PRINT-SURVEY-YN
020413     ELSE
020413         MOVE 'Y'                TO WS-PRINT-SURVEY-YN
020413     END-IF
102413
102413     IF PI-SPECIAL-RELEASE-YN = 'Y'
102413         MOVE PI-SPECIAL-RELEASE-YN TO WS-SPECIAL-RELEASE-YN
102413     ELSE
102413         MOVE 'N'                TO WS-SPECIAL-RELEASE-YN
102413     END-IF
00844      
      * EXEC CICS HANDLE CONDITION
00845 *        NOTOPEN   (9990-ABEND)
00846 *        PGMIDERR  (8600-PGRM-NOT-FOUND)
00847 *        MAPFAIL   (0400-FIRST-TIME)
00848 *        DUPREC    (6180-DUPREC)
00849 *        ERROR     (9990-ABEND)
00850 *    END-EXEC.
      *    MOVE '"$JL?%.               ! " #00008374' TO DFHEIV0
           MOVE X'22244A4C3F252E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303038333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00851
00852      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00853          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.
00854
00855      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00856          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00857              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00858              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00859              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00860              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00861              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00862              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00863              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00864              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00865              MOVE LOW-VALUES           TO EL156AI
00866              MOVE 'Y'                  TO FIRST-ENTRY
00867          ELSE
00868              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00869              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00870              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00871              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00872              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00873              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00874              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00875              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00876
00877      IF RETURNED-FROM NOT = SPACES
              IF (RETURNED-FROM = 'EL614')
                 MOVE PI-PROGRAM-WORK-AREA (1:60) TO WS-SAVED-EOB-CODES
                 IF WS-SAVED-EOB-CODES NOT = SPACES
                    MOVE 'Y'                TO WS-SET-NOTE2-MDT
                 ELSE
                    MOVE 'N'                TO WS-SET-NOTE2-MDT
                 END-IF
              END-IF
00878          PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0699-EXIT
00879             MOVE 'Y'             TO FIRST-ENTRY
                  IF RETURNED-FROM = 'EL614'
                     AND (PI-PASS-SW NOT = 'C')
                     IF WS-SAVED-EOB-CODES NOT = SPACES
                        MOVE WS-SAVED-EOB-CODES
                                       TO PI-PMTNOTE2
                                          WS-PMTNOTE2
                        MOVE 'Y'       TO PI-EOB-CODES-EXIST
                     ELSE
                        MOVE SPACES    TO PI-PMTNOTE2
                                          WS-PMTNOTE2
                                          PI-EOB-CODES-EXIST
                     END-IF
                  END-IF
00880             PERFORM 3050-REBUILD-ENTERED THRU 3059-EXIT
00881             PERFORM 7010-BUILD-MAP THRU 7023-BYPASS-REMAINING-TERM
                  IF RETURNED-FROM = 'EL614'
                     GO TO 1000-EDIT-DATA
                  END-IF
00882             IF (PMTTYPEI = LOW-VALUES OR SPACES) AND
00883                (PAYEEI   = LOW-VALUES OR SPACES)
00884                GO TO 8100-SEND-INITIAL-MAP
00885             ELSE
00886                GO TO 1000-EDIT-DATA.
00887
00888      IF EIBTRNID NOT = TRANS-ID
00889          GO TO 0400-FIRST-TIME.
00890
00891      IF EIBAID = DFHCLEAR
00892         IF PI-PASS-SW = 'C'
00893            PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0699-EXIT
00894            PERFORM 3050-REBUILD-ENTERED      THRU 3059-EXIT
00895            PERFORM 7010-BUILD-MAP THRU 7020-BYPASS-BENEFIT
00896            MOVE 'Y'            TO RETURNED-FROM-B
00897            GO TO 1000-EDIT-DATA
00898         ELSE
00899            GO TO 9400-CLEAR.
00900
00901      IF PI-PASS-SW = 'C'
00902          MOVE MAP-NAMEB          TO MAP-NAME
00903          IF EIBAID = DFHPF1 OR DFHPF13
00904              GO TO 5000-UPDATE
00905            ELSE
00906              IF EIBAID NOT = DFHENTER
00907                  MOVE LOW-VALUES TO EL156BI
00908                  MOVE MAP-NAMEB  TO MAP-NAME
00909                  MOVE ER-0029    TO EMI-ERROR
00910                  MOVE -1         TO  ENTPFBL
00911                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00912                  GO TO 8200-SEND-DATAONLY.
00913
00914      EJECT
00915  0200-RECEIVE.
00916      MOVE LOW-VALUES             TO EL156AI.
00917      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00918          MOVE ER-0008            TO EMI-ERROR
00919          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00920          IF PI-PASS-SW = 'C'
00921              MOVE -1             TO ENTPFBL
00922              GO TO 8200-SEND-DATAONLY
00923          ELSE
00924              MOVE -1             TO PMTTYPEL
00925              GO TO 8200-SEND-DATAONLY.
00926
00927      IF PI-COMPANY-ID = 'DMD'
00928          PERFORM 7700-GET-DCT THRU 7700-EXIT.
00929
00930      IF PI-PASS-SW = 'C'
00931          MOVE MAP-NAMEB          TO MAP-NAME.
00932
00933      
      * EXEC CICS RECEIVE
00934 *        MAP   (MAP-NAME)
00935 *        MAPSET(MAPSET-NAME)
      *        ASIS
00936 *        INTO  (EL156AI)
00937 *    END-EXEC.
           MOVE LENGTH OF
            EL156AI
             TO DFHEIV11
      *    MOVE '8"TAI  L              ''   #00008487' TO DFHEIV0
           MOVE X'382254414920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL156AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00938
00939      IF PI-PASS-SW NOT = 'C'
00940          GO TO 0250-MAPA.
00941
00942      IF ENTPFBL NOT > ZERO
00943          MOVE MAP-NAMEB          TO MAP-NAME
00944          MOVE ER-0029            TO EMI-ERROR
00945          MOVE -1                 TO  ENTPFBL
00946          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00947          GO TO 8200-SEND-DATAONLY.
00948
00949      IF (ENTPFBI NUMERIC) AND (ENTPFBI > 0 AND < 25)
00950          MOVE PF-VALUES (ENTPFBI) TO EIBAID
00951        ELSE
00952          MOVE MAP-NAMEB           TO MAP-NAME
00953          MOVE ER-0029             TO EMI-ERROR
00954          MOVE -1                  TO ENTPFBL
00955          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00956          GO TO 8200-SEND-DATAONLY.
00957
00958      IF EIBAID = DFHPF1 OR DFHPF13
00959          IF MODIFY-CAP
00960             GO TO 5000-UPDATE
00961           ELSE
00962             MOVE 'UPDATE'           TO SM-READ
00963             PERFORM 9995-SECURITY-VIOLATION
00964             MOVE MAP-NAMEB          TO MAP-NAME
00965             MOVE ER-0070            TO EMI-ERROR
00966             MOVE -1                 TO  ENTPFBL
00967             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00968             GO TO 8200-SEND-DATAONLY.
00969
00970  0250-MAPA.
00971      IF ENTERPFL = 0
00972          GO TO 0300-CHECK-PFKEYS.
00973
00974      IF EIBAID NOT = DFHENTER
00975          MOVE ER-0004            TO EMI-ERROR
00976          GO TO 0320-INPUT-ERROR.
00977
00978      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
00979          MOVE PF-VALUES (ENTERPFI) TO EIBAID
00980      ELSE
00981          MOVE ER-0029            TO EMI-ERROR
00982          GO TO 0320-INPUT-ERROR.
00983
00984  0300-CHECK-PFKEYS.
00985      IF EIBAID = DFHPF7 OR DFHPF8
00986          GO TO 8820-PF7-8.
00987
00988      IF EIBAID = DFHPF23
00989          GO TO 8810-PF23.
00990
00991      IF EIBAID = DFHPF24
00992          GO TO 9200-RETURN-MAIN-MENU.
00993
00994      IF EIBAID = DFHPF12
00995          GO TO 9500-PF12.
00996
00997      IF EIBAID = DFHPF1
00998         IF PI-PASS-SW = 'A' OR 'B'
00999           GO TO 0330-BROWSE-FORWARD.
01000
01001      IF EIBAID = DFHPF2
01002         IF PI-PASS-SW = 'A' OR 'B'
01003           GO TO 0350-BROWSE-BACKWARD.
01004
01005      IF EIBAID = DFHPF6
              IF PI-EOB-CODES-EXIST = 'Y'
                 MOVE PI-PMTNOTE2         TO WS-PMTNOTE2
              ELSE
                 MOVE SPACES              TO WS-PMTNOTE2
              END-IF
               PERFORM 3000-MOVE-INPUT-TO-SAVE-AREA THRU 3000-EXIT
               IF WS-SAVE-INPUT NOT = PI-SAVE-INPUT
                  MOVE WS-SAVE-INPUT   TO PI-SAVE-INPUT
               END-IF
01006          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
120115         IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121               OR 'FNL'
01008              MOVE 'EL614'        TO PGM-NAME
                   MOVE WS-PMTNOTE2    TO PI-PROGRAM-WORK-AREA
01009              GO TO 9300-XCTL
01010          ELSE
01011              MOVE XCTL-157       TO PGM-NAME
01012              GO TO 9300-XCTL.
01013
01014      IF EIBAID = DFHPF9
01015         PERFORM 0500-CREATE-TEMP-STORAGE  THRU 0599-EXIT
01016         MOVE XCTL-141            TO PGM-NAME
01017         GO TO 9300-XCTL.
01018
01019      IF EIBAID = DFHPF10
01020         PERFORM 0500-CREATE-TEMP-STORAGE  THRU 0599-EXIT
01021         MOVE XCTL-155            TO PGM-NAME
01022         GO TO 9300-XCTL.
01023
01024      MOVE SPACES                 TO ERRMSG0O
01025                                     ERRMSG1O
01026                                     ERRMSG2O.
01027
01028      IF EIBAID = DFHPF3 OR DFHPF4 OR DFHPF5
01029          IF PI-PASS-SW NOT = 'B'
01030              MOVE ER-0422        TO EMI-ERROR
01031              GO TO 0320-INPUT-ERROR
01032            ELSE
01033              GO TO 0700-TEST-FOR-CHANGES.
01034
01035      IF EIBAID = DFHPF11
01036         IF PI-PASS-SW = 'C'
01037            MOVE ER-0029          TO EMI-ERROR
01038            GO TO 0320-INPUT-ERROR.
01039
01040      IF EIBAID = DFHENTER OR DFHPF11
01041         NEXT SENTENCE
01042      ELSE
01043         MOVE ER-0029             TO EMI-ERROR
01044         GO TO 0320-INPUT-ERROR.
01045
01046      PERFORM 3000-MOVE-INPUT-TO-SAVE-AREA THRU 3000-EXIT.
01047
100314     if pi-epyfrom not = zeros
100314        if pi-epyfrom not = ws-epyfrom
100314           move zeros        to ws-edays
100314        end-if
100314     end-if
100314
100314     if pi-epythru not = zeros
100314        if pi-epythru not = ws-epythru
100314           move zeros        to ws-edays
100314        end-if
100314     end-if
100314
01048      IF WS-SAVE-INPUT NOT = PI-SAVE-INPUT
01049          MOVE WS-SAVE-INPUT      TO PI-SAVE-INPUT
01050          GO TO 1000-EDIT-DATA
01051      ELSE
01052          IF PI-FATAL-COUNT NOT = 0 OR
01053             PI-FORCE-COUNT NOT = 0
01054              GO TO 1000-EDIT-DATA
01055          ELSE
01056              GO TO 2000-EDIT-DONE.
01057
01058  0320-INPUT-ERROR.
01059      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01060
01061      IF ENTERPFL = 0
01062          MOVE -1                 TO PMTTYPEL
01063      ELSE
01064          MOVE AL-UNBON           TO ENTERPFA
01065          MOVE -1                 TO ENTERPFL.
01066
01067      GO TO 8200-SEND-DATAONLY.
01068
01069      EJECT
01070  0330-BROWSE-FORWARD.
01071
01072      MOVE CLMNOI            TO PI-CLAIM-NO.
01073      MOVE CARRI             TO PI-CARRIER.
01074      MOVE CERTNOI           TO PI-CERT-PRIME.
01075      MOVE SUFXI             TO PI-CERT-SFX.
01076
01077      MOVE PI-CARRIER        TO MSTR-CARRIER.
01078      MOVE PI-COMPANY-CD     TO MSTR-COMP-CD.
01079      MOVE PI-CLAIM-NO       TO MSTR-CLAIM-NO.
01080      MOVE PI-CERT-NO        TO MSTR-CERT-NO.
01081
01082      MOVE ELMSTR-KEY        TO WS-HOLD-ELMSTR-KEY.
01083
01084      
      * EXEC CICS HANDLE CONDITION
01085 *         NOTFND   (0380-CLAIM-END-FILE)
01086 *         ENDFILE  (0380-CLAIM-END-FILE)
01087 *    END-EXEC.
      *    MOVE '"$I''                  ! # #00008662' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303038363632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01088
01089      
      * EXEC CICS STARTBR
01090 *         DATASET    ('ELMSTR')
01091 *         RIDFLD     (ELMSTR-KEY)
01092 *         GTEQ
01093 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008667' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01094
01095      MOVE 'Y' TO WS-BROWSE-START-SW.
01096
01097  0335-READNEXT.
01098
01099      
      * EXEC CICS READNEXT
01100 *         DATASET    ('ELMSTR')
01101 *         RIDFLD     (ELMSTR-KEY)
01102 *         SET        (ADDRESS OF CLAIM-MASTER)
01103 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008677' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01104
01105      IF PI-COMPANY-CD NOT = MSTR-COMP-CD
01106         GO TO 0380-CLAIM-END-FILE.
01107
01108      IF ELMSTR-KEY = WS-HOLD-ELMSTR-KEY
01109         MOVE CL-CLAIM-NO         TO  PI-PREV-CLMNO
01110         GO TO 0335-READNEXT.
01111
01112      IF PI-COMPANY-ID = 'CRI'
01113          IF CL-CLAIM-NO  NOT =  PI-PREV-CLMNO
01114             GO TO 0380-CLAIM-END-FILE.
01115
01116      IF CL-SYSTEM-IDENTIFIER = 'CV'
01117          GO TO 0335-READNEXT.
01118
01119      IF PI-CARRIER-SECURITY > SPACES
01120          IF CL-CARRIER = PI-CARRIER-SECURITY
01121              NEXT SENTENCE
01122          ELSE
01123              GO TO 0335-READNEXT.
01124
01125      IF PI-ACCOUNT-SECURITY > SPACES
01126          IF CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY
01127              NEXT SENTENCE
01128          ELSE
01129              GO TO 0335-READNEXT.
01130
01131      MOVE MSTR-CARRIER      TO PI-CARRIER.
01132      MOVE MSTR-CLAIM-NO     TO PI-CLAIM-NO.
01133      MOVE MSTR-CERT-NO      TO PI-CERT-NO.
01134
01135      GO TO 0400-FIRST-TIME.
01136
01137  0350-BROWSE-BACKWARD.
01138
01139      MOVE CLMNOI            TO PI-CLAIM-NO
01140                                PI-PREV-CLMNO.
01141      MOVE CARRI             TO PI-CARRIER.
01142      MOVE CERTNOI           TO PI-CERT-PRIME.
01143      MOVE SUFXI             TO PI-CERT-SFX.
01144
01145      MOVE PI-CARRIER        TO MSTR-CARRIER.
01146
01147      MOVE PI-COMPANY-CD     TO MSTR-COMP-CD.
01148      MOVE PI-CLAIM-NO       TO MSTR-CLAIM-NO.
01149      MOVE PI-CERT-NO        TO MSTR-CERT-NO.
01150
01151      
      * EXEC CICS HANDLE CONDITION
01152 *         NOTFND   (0380-CLAIM-END-FILE)
01153 *         ENDFILE  (0380-CLAIM-END-FILE)
01154 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00008729' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303038373239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01155
01156      
      * EXEC CICS STARTBR
01157 *         DATASET    ('ELMSTR')
01158 *         RIDFLD     (ELMSTR-KEY)
01159 *         GTEQ
01160 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008734' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01161
01162      MOVE 'Y' TO WS-BROWSE-START-SW.
01163
01164      
      * EXEC CICS READNEXT
01165 *         DATASET    ('ELMSTR')
01166 *         RIDFLD     (ELMSTR-KEY)
01167 *         SET        (ADDRESS OF CLAIM-MASTER)
01168 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008742' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01169
01170      
      * EXEC CICS READPREV
01171 *         DATASET    ('ELMSTR')
01172 *         RIDFLD     (ELMSTR-KEY)
01173 *         SET        (ADDRESS OF CLAIM-MASTER)
01174 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00008748' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01175
01176  0360-BROWSE-BACKWARD.
01177
01178      
      * EXEC CICS READPREV
01179 *         DATASET    ('ELMSTR')
01180 *         RIDFLD     (ELMSTR-KEY)
01181 *         SET        (ADDRESS OF CLAIM-MASTER)
01182 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00008756' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01183
01184      IF PI-COMPANY-CD NOT = MSTR-COMP-CD
01185         GO TO 0380-CLAIM-END-FILE.
01186
01187      IF PI-COMPANY-ID = 'CRI'
01188          IF CL-CLAIM-NO  NOT =  PI-PREV-CLMNO
01189             GO TO 0380-CLAIM-END-FILE.
01190
01191      IF CL-SYSTEM-IDENTIFIER = 'CV'
01192          GO TO 0360-BROWSE-BACKWARD.
01193
01194      IF PI-CARRIER-SECURITY > SPACES
01195          IF CL-CARRIER = PI-CARRIER-SECURITY
01196              NEXT SENTENCE
01197          ELSE
01198              GO TO 0360-BROWSE-BACKWARD.
01199
01200      IF PI-ACCOUNT-SECURITY > SPACES
01201          IF CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY
01202              NEXT SENTENCE
01203          ELSE
01204              GO TO 0360-BROWSE-BACKWARD.
01205
01206      MOVE MSTR-CARRIER      TO PI-CARRIER.
01207      MOVE MSTR-CLAIM-NO     TO PI-CLAIM-NO.
01208      MOVE MSTR-CERT-NO      TO PI-CERT-NO.
01209
01210      GO TO 0400-FIRST-TIME.
01211
01212  0380-CLAIM-END-FILE.
01213
01214      MOVE ER-0130     TO EMI-ERROR.
01215      MOVE -1          TO CLMNOL.
01216      MOVE AL-UABON    TO CARRA
01217                          CLMNOA
01218                          CERTNOA
01219                          SUFXA.
01220
01221      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01222
01223      GO TO 8200-SEND-DATAONLY.
01224
01225  0400-FIRST-TIME.
01226
01227      IF WS-BROWSE-START-SW = 'Y'
01228         MOVE 'N'         TO WS-BROWSE-START-SW
01229         
      * EXEC CICS ENDBR
01230 *            DATASET    ('ELMSTR')
01231 *       END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008807' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01232
01233      IF PI-PROCESSOR-ID = 'LGXX'
01234          NEXT SENTENCE
01235      ELSE
01236          
      * EXEC CICS READQ TS
01237 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
01238 *            INTO    (SECURITY-CONTROL)
01239 *            LENGTH  (SC-COMM-LENGTH)
01240 *            ITEM    (SC-ITEM)
01241 *        END-EXEC
      *    MOVE '*$II   L              ''   #00008814' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01242          MOVE SC-CLAIMS-DISPLAY (10)  TO  PI-DISPLAY-CAP
01243          MOVE SC-CLAIMS-UPDATE  (10)  TO  PI-MODIFY-CAP.
01244
01245      MOVE LOW-VALUES             TO EL156AI.
01246      PERFORM 0800-DELETE-TS THRU 0800-EXIT.
01247      MOVE 'Y'                    TO FIRST-ENTRY.
01248      GO TO 7000-SHOW-CLAIM.
01249
01250  0500-CREATE-TEMP-STORAGE.
01251      MOVE EIBCPOSN               TO PI-SAVE-CURSOR.
01252
01253      
      * EXEC CICS WRITEQ TS
01254 *        QUEUE (QID)
01255 *        FROM  (PROGRAM-INTERFACE-BLOCK)
01256 *        LENGTH(PI-COMM-LENGTH)
01257 *    END-EXEC.
      *    MOVE '*"     L              ''   #00008831' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01258
01259      
      * EXEC CICS WRITEQ TS
01260 *        QUEUE (QID)
01261 *        FROM  (EL156AI)
01262 *        LENGTH(MAP-LENGTH)
01263 *    END-EXEC.
      *    MOVE '*"     L              ''   #00008837' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL156AI, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01264
01265  0599-EXIT.
01266       EXIT.
01267
01268      EJECT
01269  0600-RECOVER-TEMP-STORAGE.
01270      
      * EXEC CICS READQ TS
01271 *        QUEUE (QID)
01272 *        INTO  (PROGRAM-INTERFACE-BLOCK)
01273 *        LENGTH(PI-COMM-LENGTH)
01274 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00008848' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01275
01276      
      * EXEC CICS READQ TS
01277 *        QUEUE (QID)
01278 *        INTO  (EL156AI)
01279 *        LENGTH(MAP-LENGTH)
01280 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00008854' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL156AI, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01281
01282      PERFORM 0800-DELETE-TS THRU 0800-EXIT.
01283
01284      IF PMTTYPEI NOT = LOW-VALUES
01285          MOVE AL-UANON           TO PMTTYPEA.
01286
01287      IF PAYEEI NOT = LOW-VALUES
01288          MOVE AL-UANON           TO PAYEEA.
01289
01290      IF NOTE1I NOT = LOW-VALUES
01291          MOVE AL-UANON           TO NOTE1A.
01292
           IF (RETURNED-FROM = 'EL614')
              MOVE WS-SET-NOTE2-MDT    TO PI-SET-NOTE2-MDT
           END-IF
01293 *    IF (NOTE2I NOT = LOW-VALUES AND SPACES)
      *       AND (PI-EOB-CODES-EXIST = 'Y')
      *       AND (PI-SET-NOTE2-MDT = 'Y')
      *       MOVE ' '                 TO PI-SET-NOTE2-MDT
           IF PI-SET-NOTE2-MDT = 'Y'
01294          MOVE AL-SANON           TO NOTE2A
           END-IF
01295
01296      IF OFFLINEI NOT = LOW-VALUES
01297          MOVE AL-UANON           TO OFFLINEA.
01298
01299 *    IF GROUPEDI NOT = LOW-VALUES
01300 *        MOVE AL-UANON           TO GROUPEDA.
01301
01302 *    IF CASHI NOT = LOW-VALUES
01303 *        MOVE AL-UANON           TO CASHA.
01304
01305      IF CHECKNOI NOT = LOW-VALUES
01306          MOVE AL-UANON           TO CHECKNOA.
01307
01308      IF HOLDTILI NOT = LOW-VALUES
01309          MOVE AL-UANON           TO HOLDTILA.
01310
01311      IF EPYFROMI NOT = LOW-VALUES
01312          MOVE AL-UANON           TO EPYFROMA.
01313
01314      IF AIGFROMI NOT = LOW-VALUES
01315          MOVE AL-UANON           TO AIGFROMA.
01316
01317      IF EPYTHRUI NOT = LOW-VALUES
01318          MOVE AL-UANON           TO EPYTHRUA.
01319
01320      IF EDAYSI NOT = LOW-VALUES
01321          MOVE AL-UNNON           TO EDAYSA.
01322
01323      IF EPYAMTL NOT = +0
01324          MOVE AL-UNNON           TO EPYAMTA.
01325
01326      IF ERESVL NOT = +0
01327          MOVE AL-UNNON           TO ERESVA.
01328
01329      IF EEXPENSL NOT = +0
01330          MOVE AL-UNNON           TO EEXPENSA.
01331
01332      IF ETYPEI NOT = LOW-VALUES
01333          MOVE AL-UNNON           TO ETYPEA.
01334
052506     IF PROOFDTI NOT = 0
052506         MOVE AL-UNNON           TO PROOFDTA.
052506
013013     IF EOBYNI = 'Y' OR 'N' OR 'S'
              MOVE AL-UANON            TO EOBYNA
           END-IF
020413     IF CLMFMYNI = 'Y' OR 'N'
020413        MOVE AL-UANON            TO CLMFMYNA
020413     END-IF
020413
020413     IF SURVYYNI = 'Y' OR 'N'
111113        IF PI-APPROVAL-LEVEL = '4' OR '5'
020413           MOVE AL-UANON         TO SURVYYNA
020413        END-IF
020413     END-IF
020413
102413     IF SPRELYNI = 'Y' OR 'N'
102413        MOVE AL-UANON            TO SPRELYNA
102413     END-IF
102413
           .
01335  0699-EXIT.
01336       EXIT.
01337
01338      EJECT
01339  0700-TEST-FOR-CHANGES.
01340      IF (EIBAID = DFHPF4) AND (NOT FORCE-CAP)
01341          MOVE ER-0433            TO EMI-ERROR
01342          GO TO 0750-FUNCTION-ERROR.
01343
01344      PERFORM 3000-MOVE-INPUT-TO-SAVE-AREA THRU 3000-EXIT.
01345
01346      IF WS-SAVE-INPUT NOT = PI-SAVE-INPUT
01347          MOVE WS-SAVE-INPUT      TO PI-SAVE-INPUT
01348          MOVE 'A'                TO PI-PASS-SW
01349          GO TO 1000-EDIT-DATA.
01350
01351      IF PI-FATAL-COUNT NOT = 0
01352          MOVE ER-0499            TO EMI-ERROR
01353          GO TO 0750-FUNCTION-ERROR.
01354
01355      IF (PI-FORCE-COUNT NOT = 0) AND (EIBAID NOT = DFHPF4)
01356          MOVE ER-0500            TO EMI-ERROR
01357          GO TO 0750-FUNCTION-ERROR.
01358
01359      IF (EIBAID  = DFHPF5 AND PI-CPYAMT = ZEROS) OR
01360         ((EIBAID = DFHPF3 OR DFHPF4) AND PI-EPYAMT = ZEROS)
01361            MOVE ER-0542          TO EMI-ERROR
01362            MOVE -1               TO EPYAMTL
01363            GO TO 0750-FUNCTION-ERROR.
01364
01365      IF PI-OFFLINE = 'Y'  OR
01366         (PI-PMTTYPE = '4' AND PI-EPYAMT NEGATIVE)
01367         MOVE EIBAID              TO PI-PFKEY-USED
01368         GO TO 5000-UPDATE.
01369
01370      GO TO 6000-BUILD-SCREEN-B.
01371
01372  0750-FUNCTION-ERROR.
01373      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01374
01375      IF ENTERPFL > 0
01376          MOVE -1                 TO ENTERPFL
01377          MOVE AL-UNBON           TO ENTERPFA
01378          GO TO 8200-SEND-DATAONLY
01379      ELSE
01380          MOVE -1                 TO ENTERPFL
01381          MOVE AL-UNNOF           TO ENTERPFA
01382          GO TO 8200-SEND-DATAONLY.
01383
01384      EJECT
01385  0800-DELETE-TS.
01386      
      * EXEC CICS HANDLE CONDITION
01387 *        QIDERR(0800-EXIT)
01388 *    END-EXEC.
      *    MOVE '"$N                   ! % #00008993' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303038393933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01389
01390      
      * EXEC CICS DELETEQ TS
01391 *        QUEUE(QID)
01392 *    END-EXEC.
      *    MOVE '*&                    #   #00008997' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01393
01394  0800-EXIT.
01395       EXIT.
061013 0900-GET-DDF-LIMITS.
061013
           if cm-clp-state = zeros or spaces or low-values
              move cm-state            to cm-clp-state
           end-if
061013     MOVE ' '                    TO WS-PDEF-RECORD-SW
061013     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
061013     MOVE cm-clp-state           TO ERPDEF-STATE
061013     MOVE PI-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
070714     evaluate true
070714        when (cl-claim-type = 'L' or 'P')
070714           and (pi-lf-benefit-cd not = '00' and '  ' and 'DD'
070714             and 'CU')
070714           move 'L'              to erpdef-ben-type
070714           move pi-lf-benefit-cd to erpdef-ben-code
070714        when (cl-claim-type not = 'L' and 'P')
070714           and (pi-ah-benefit-cd not = '00' and '  ')
070714           move 'A'              to erpdef-ben-type
070714           move pi-ah-benefit-cd to erpdef-ben-code
070714        when (cl-claim-type not = 'L' and 'P')
070714           and (pi-ah-benefit-cd = '00' or '  ')
070714           move 'L'              to erpdef-ben-type
070714           move pi-lf-benefit-cd to erpdef-ben-code
070714        when (cl-claim-type = 'L' or 'P')
070714           and (pi-lf-benefit-cd = '00' or '  ' or 'DD' or 'CU')
070714           move 'A'              to erpdef-ben-type
070714           move pi-ah-benefit-cd to erpdef-ben-code
070714        when other
070714           move 'A'              to erpdef-ben-type
070714           move pi-ah-benefit-cd to erpdef-ben-code
070714     end-evaluate
      *    if (cl-claim-type = 'L' or 'P')
      *       and (pi-lf-benefit-cd not = '00' and '  ' and 'DD'
      *          and 'CU')
      *       move 'L'                 to erpdef-ben-type
      *       move pi-lf-benefit-cd    to erpdef-ben-code
      *    else
061013*       MOVE 'A'                 TO ERPDEF-BEN-TYPE
061013*       MOVE PI-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
      *    end-if
061013     MOVE CL-CERT-EFF-DT         TO ERPDEF-EXP-DT
061013
061013     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
061013
061013     
      * EXEC CICS STARTBR
061013*        DATASET  ('ERPDEF')
061013*        RIDFLD   (ERPDEF-KEY)
061013*        GTEQ
061013*        RESP     (WS-RESPONSE)
061013*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00009047' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303039303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     IF NOT WS-RESP-NORMAL
061013        GO TO 0900-EXIT
061013     END-IF
061013
061013     .
061013 0900-READNEXT.
061013
061013     
      * EXEC CICS READNEXT
061013*       DATASET  ('ERPDEF')
061013*       SET      (ADDRESS OF PRODUCT-MASTER)
061013*       RIDFLD   (ERPDEF-KEY)
061013*       RESP     (WS-RESPONSE)
061013*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00009061' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303039303631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     IF NOT WS-RESP-NORMAL
061013        GO TO 0900-ENDBR
061013     END-IF
061013
061013     IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
061013        IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
061013           MOVE 'Y'              TO WS-PDEF-RECORD-SW
061013        ELSE
061013           GO TO 0900-READNEXT
061013        END-IF
061013     ELSE
061013        GO TO 0900-ENDBR
061013     END-IF
061013
061013     IF CM-LOAN-TERM = ZEROS
061013        MOVE CP-ORIGINAL-TERM    TO CP-LOAN-TERM
061013     END-IF
061013
061013     .
061013 0900-ENDBR.
061013
061013     
      * EXEC CICS ENDBR
061013*       DATASET  ('ERPDEF')
061013*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009089' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     .
061013 0900-EXIT.
061013     EXIT.
01398  1000-EDIT-DATA.
01399
01400      MOVE 0 TO PI-FATAL-COUNT  PI-FORCE-COUNT  PI-BEN-DAYS
01401          PI-CDAYS  PI-CPYAMT  PI-CRESV  PI-CEXPENS PI-MANUAL-SW
021406         PI-INT-AMT PI-INT-DAYS PI-INT-RATE-USED
01402
01403      MOVE LOW-VALUES TO PI-CPYFROM  PI-CPYTHRU.
01404
01405      IF NOT MODIFY-CAP
01406          MOVE ER-0070            TO EMI-ERROR
01407          MOVE -1                 TO PMTTYPEL
01408          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01409          GO TO 2000-EDIT-DONE.
01410
01411      IF PI-PMTTYPE NOT = '1' AND '2' AND '3' AND '4' AND
01412                          '5' AND '6'
01413          MOVE ER-0420            TO EMI-ERROR
01414          MOVE -1                 TO PMTTYPEL
01415          MOVE AL-UABON           TO PMTTYPEA
01416          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01417      ELSE
01418          MOVE AL-UANON           TO PMTTYPEA.
01419
020413*    IF PI-OFFLINE = 'Y'
020413*       IF PI-GROUPED = 'Y'
020413*          MOVE ER-1880         TO EMI-ERROR
020413*          MOVE -1              TO GROUPEDL
020413*          MOVE AL-UABON        TO GROUPEDA
020413*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
020413*
020413*    IF GROUPEDL > +0
020413*       IF PI-GROUPED = 'Y' OR 'N'
020413*          MOVE AL-UANON TO GROUPEDA
020413*       ELSE
020413*          MOVE ER-3140            TO EMI-ERROR
020413*          MOVE -1                 TO GROUPEDL
020413*          MOVE AL-UABON           TO GROUPEDA
020413*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
020413*
020413*    IF CASHL > +0
020413*       IF PI-CASH = 'Y' OR 'N'
020413*          MOVE AL-UANON           TO CASHA
020413*       ELSE
020413*          MOVE ER-3141            TO EMI-ERROR
020413*          MOVE -1                 TO CASHL
020413*          MOVE AL-UABON           TO CASHA
020413*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01444
01445      IF LOANNOL > +0
01446          MOVE 'Y'                TO  PI-LOAN-UPDATE-SW.
01447
01448  1000-EDIT-PAYEE.
01449
01450      IF PI-PAYEE-SEQ NOT NUMERIC
01451          MOVE ER-0294            TO  EMI-ERROR
01452          MOVE -1                 TO  PAYEEL
01453          MOVE AL-UABON           TO  PAYEEA
01454          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01455          GO TO 1000-CONT-EDIT.
01456
01457      IF PI-PAYEE-TYPE =  'I' OR 'B' OR 'A' OR 'P' OR
01458                          'E' OR 'O' OR 'Q'
01459          NEXT SENTENCE
01460      ELSE
01461          MOVE ER-0294            TO  EMI-ERROR
01462          MOVE -1                 TO  PAYEEL
01463          MOVE AL-UABON           TO  PAYEEA
01464          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01465          GO TO 1000-CONT-EDIT.
01466
01467      IF ((PI-PAYEE-TYPE = 'I' OR 'E' OR 'O' OR 'P' OR 'Q')  AND
01468         (PI-PAYEE-SEQ NOT < '1' AND
01469          PI-PAYEE-SEQ NOT > '9'))
01470       OR
01471         ((PI-PAYEE-TYPE = 'A' OR 'B')  AND
01472          (PI-PAYEE-SEQ NOT > '9'))
01473          MOVE AL-UANON            TO PAYEEA
01474      ELSE
01475          MOVE ER-0294            TO EMI-ERROR
01476          MOVE -1                 TO PAYEEL
01477          MOVE AL-UABON           TO PAYEEA
01478          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01479
01480      IF PI-COMPANY-ID = 'DMD'
01481          IF PI-PAYEE-TYPE = 'B' AND
01482             PI-PAYEE-SEQ NOT = '0'
01483                MOVE ER-7841            TO EMI-ERROR
01484                MOVE -1                 TO PAYEEL
01485                MOVE AL-UABON           TO PAYEEA
01486                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01487
01488      IF PI-COMPANY-ID = 'JHL'
01489          IF PI-PAYEE = 'B0'
01490              NEXT SENTENCE
01491            ELSE
01492          
      * EXEC CICS HANDLE CONDITION
01493 *            ENDFILE(1950-NOT-FOUND)
01494 *            NOTFND (1950-NOT-FOUND)
01495 *        END-EXEC
      *    MOVE '"$''I                  ! & #00009191' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303039313931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01496          MOVE 'N'                    TO FOUND-TOL-SW
01497          MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
01498          MOVE '2'                    TO CNTL-REC-TYPE
01499          MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS
01500          MOVE +0                     TO CNTL-SEQ-NO
01501          MOVE 'PROC'                 TO FILE-SWITCH
01502          PERFORM 7930-READ-CONTROL THRU 7930-EXIT
01503          IF PI-PAYEE = 'I1' AND
01504             APPROVAL-LEVEL-2
01505              NEXT SENTENCE
01506            ELSE
01507          IF NOT PI-USER-ALMIGHTY-YES
01508              MOVE ER-3546        TO EMI-ERROR
01509              MOVE -1             TO PAYEEL
01510              MOVE AL-UABON       TO PAYEEA
01511              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01512
01513      IF PI-PAYEE-TYPE = 'E'
01514         IF PI-PMTTYPE = '5' OR '6'
01515             NEXT SENTENCE
01516         ELSE
01517             MOVE ER-3539            TO EMI-ERROR
01518             MOVE -1                 TO PAYEEL
01519             MOVE AL-UABON           TO PAYEEA
01520             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01521
01522  1000-CONT-EDIT.
01523
01524      IF PI-OFFLINE NOT = 'Y' AND 'N'
01525         IF PI-EPYAMT NOT NEGATIVE
01526             MOVE ER-0421         TO EMI-ERROR
01527             MOVE -1              TO OFFLINEL
01528             MOVE AL-UABON        TO OFFLINEA
01529             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01530         ELSE
01531             MOVE AL-UANON        TO OFFLINEA.
01532
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
              IF PI-PRINT-EOB-YN = 'Y' OR 'N'
                 CONTINUE
013013        ELSE
013013           IF PI-PRINT-EOB-YN = 'S' AND
100518              PI-CLM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
013013                CONTINUE
                 ELSE
                    MOVE ER-1561       TO EMI-ERROR
                    MOVE -1            TO EOBYNL
                    MOVE AL-UABON      TO EOBYNA
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
013013        END-IF
020413
020413        IF PI-PRINT-CLM-FRM-YN NOT = 'Y' AND 'N'
020413            MOVE ER-1566        TO EMI-ERROR
020413            MOVE -1             TO CLMFMYNL
020413            MOVE AL-UABON       TO CLMFMYNA
020413            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020413        END-IF
020413
020413        IF PI-PRINT-SURVEY-YN NOT = 'Y' AND 'N'
020413            MOVE ER-1567        TO EMI-ERROR
020413            MOVE -1             TO SURVYYNL
020413            MOVE AL-UABON       TO SURVYYNA
020413            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020413        END-IF
020413
102413        IF PI-SPECIAL-RELEASE-YN NOT = 'Y' AND 'N'
102413            MOVE ER-1569        TO EMI-ERROR
102413            MOVE -1             TO SPRELYNL
102413            MOVE AL-UABON       TO SPRELYNA
102413            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
102413        END-IF
102413
           END-IF
01533      IF PI-EPYAMT IS NEGATIVE
01534          IF PI-OFFLINE NOT = 'Y'
01535              MOVE ER-0836        TO  EMI-ERROR
01536              MOVE -1             TO  OFFLINEL
01537              MOVE AL-UABON       TO  OFFLINEA
01538              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
052506
052506     IF PI-PROOF-DATE = ZEROS OR LOW-VALUES OR SPACES
052506        MOVE ER-0872             TO EMI-ERROR
052506        MOVE -1                  TO PROOFDTL
052506        MOVE AL-UABON            TO PROOFDTA
052506        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
052506     ELSE
052506        MOVE '4'                TO DC-OPTION-CODE
052506        MOVE PI-PROOF-DATE      TO DC-GREG-DATE-1-MDY
052506        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
052506        IF DATE-CONVERSION-ERROR
052506            MOVE ER-0873        TO EMI-ERROR
052506            MOVE -1                  TO PROOFDTL
052506            MOVE AL-UABON            TO PROOFDTA
052506            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
052506        ELSE
012407            MOVE DC-BIN-DATE-1   TO WS-BIN-PROOF-DT
052506            IF DC-BIN-DATE-1 > WS-TODAY-DATE
052506                MOVE ER-0873    TO EMI-ERROR
052506                MOVE -1                  TO PROOFDTL
052506                MOVE AL-UABON            TO PROOFDTA
052506                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
052506            END-IF
052506        END-IF
052506     END-IF.
01539
01540      IF PI-COMPANY-ID = 'DMD'
01541         IF NOTE1L NOT = ZEROS
01542             MOVE ER-7844        TO  EMI-ERROR
01543             MOVE -1             TO  NOTE1L
01544             MOVE AL-UABON       TO  NOTE1A
01545             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01546
01547      IF PI-COMPANY-ID = 'DMD'
01548         IF NOTE2L NOT = ZEROS
01549           GO TO 1004-DMD-EOB-CHECKS.
01550
01551      IF NOTE1L NOT > +0
01552          GO TO 1005-CHECK-ERRORS.
01553
01554      IF PI-COMPANY-ID NOT = 'FLI' AND 'FLU' AND 'HAN' AND 'JHL'
01555          GO TO 1005-CHECK-ERRORS.
01556
01557      IF PI-COMPANY-ID = 'HAN' OR 'JHL'
01558          GO TO 1003-HAN-PMT-NOTES.
01559
01560      MOVE NOTE1I                 TO WS-FLI-PMTNOTE.
01561
01562      IF WS-PMTOPTION NUMERIC
01563         IF WS-PMTOPTION > '00' AND < '17'
01564            NEXT SENTENCE
01565         ELSE
01566            MOVE ER-0618          TO EMI-ERROR
01567            MOVE -1               TO NOTE1L
01568            MOVE AL-UABON         TO NOTE1A
01569            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01570      ELSE
01571         GO TO 1005-CHECK-ERRORS.
01572
01573      IF WS-PMTOPTION = '05' AND
01574         WS-DOCNAME = SPACES
01575           MOVE ER-0616             TO EMI-ERROR
01576           MOVE -1                  TO NOTE1L
01577           MOVE AL-UABON            TO NOTE1A
01578           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01579
01580      IF WS-PMTOPTION = '15' OR '16'
01581          
      * EXEC CICS BIF DEEDIT
01582 *            FIELD (WS-AMOUNT)
01583 *            LENGTH(08)
01584 *        END-EXEC
           MOVE 08
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009344' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-AMOUNT, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01585         IF (WS-NUM-AMT = ZEROS) OR
01586            (WS-AMOUNT NOT NUMERIC)
01587            MOVE ER-0617          TO EMI-ERROR
01588            MOVE -1               TO NOTE1L
01589            MOVE AL-UABON         TO NOTE1A
01590            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01591
01592      MOVE WS-FLI-PMTNOTE TO NOTE1I WS-PMTNOTE1 PI-PMTNOTE1.
01593
01594      GO TO 1005-CHECK-ERRORS.
01595
01596  1003-HAN-PMT-NOTES.
01597
01598      MOVE NOTE1I                 TO WS-HAN-PMTNOTE.
01599
01600      IF WS-HAN-PMT-CODE = SPACE
01601          MOVE SPACES             TO WS-HAN-PMT-NOTE
01602      ELSE
01603      IF WS-HAN-PMT-CODE = 'A'
01604          MOVE ':FINAL PAYMENT / MAXIMUM BENEFIT-PAID'
01605                                  TO WS-HAN-PMT-NOTE
01606      ELSE
01607      IF WS-HAN-PMT-CODE = 'B'
01608          MOVE ':FINAL PAYMENT / DECEASED'
01609                                  TO WS-HAN-PMT-NOTE
01610      ELSE
01611      IF WS-HAN-PMT-CODE = 'C'
01612          MOVE ':FINAL PAYMENT / NO LONGER DISABLED'
01613                                  TO WS-HAN-PMT-NOTE
01614      ELSE
01615      IF WS-HAN-PMT-CODE = 'D'
01616          MOVE ':FINAL PAYMENT / ADDL INFO NOT PROVIDED'
01617                                  TO WS-HAN-PMT-NOTE
01618      ELSE
01619      IF WS-HAN-PMT-CODE = 'E'
01620          MOVE ':FINAL PAYMENT / RETURNED TO WORK'
01621                                  TO WS-HAN-PMT-NOTE
01622      ELSE
01623      IF WS-HAN-PMT-CODE = 'F'
01624          MOVE ':FINAL PAYMENT / PAID TO MATURITY DATE'
01625                                  TO WS-HAN-PMT-NOTE
01626      ELSE
01627      IF WS-HAN-PMT-CODE = 'P'
01628          MOVE ':PARTIAL PAYMENT' TO WS-HAN-PMT-NOTE.
01629
01630      MOVE WS-HAN-PMTNOTE TO NOTE1I WS-PMTNOTE1 PI-PMTNOTE1.
01631      GO TO 1005-CHECK-ERRORS.
01632
01633  1004-DMD-EOB-CHECKS.
01634
01635      INITIALIZE PI-AT-EOB-CODES.
01636
01637      IF NOTE2I (20:21) > SPACES
01638          MOVE ER-7845          TO EMI-ERROR
01639          MOVE -1               TO NOTE2L
01640          MOVE AL-UABON         TO NOTE2A
01641          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01642          GO TO 1005-CHECK-ERRORS
01643       ELSE
01644          MOVE NOTE2I             TO W-EOB-CODES.
01645
01646      MOVE +1                     TO W-EOB-NDX.
01647
01648  1004-CHECK-EACH-EOB.
01649
01650      IF W-EOB-NDX > +5
01651              OR
01652         W-EOB-CODE (W-EOB-NDX) = SPACES
01653          GO TO 1004-CHECK-DUP-EOB.
01654
01655      IF W-EOB-FILLER (W-EOB-NDX) NOT = SPACES
01656          MOVE ER-0956            TO EMI-ERROR
01657          MOVE -1                 TO NOTE2L
01658          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01659          GO TO 1005-CHECK-ERRORS.
01660
01661      MOVE 'CL'                   TO CD-SYSTEM-ID.
01662      MOVE 'EO'                   TO CD-RECORD-TYPE.
01663      MOVE W-EOB-CODE (W-EOB-NDX) TO CD-RECORD-KEY.
01664      MOVE 'DLO023'               TO PGM-NAME.
01665
01666      
      * EXEC CICS LINK
01667 *        PROGRAM    (PGM-NAME)
01668 *        COMMAREA   (CD-COMMUNICATION-AREA)
01669 *        LENGTH     (CD-RCRD-LENGTH)
01670 *    END-EXEC.
      *    MOVE '."C                   (   #00009429' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 CD-COMMUNICATION-AREA, 
                 CD-RCRD-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01671
01672      IF CD-RETURN-CODE = 'OK'
01673          GO TO 1004-CONT.
01674
01675      MOVE -1                     TO NOTE2L.
01676
01677      IF CD-RETURN-CODE = '01'
01678          MOVE ER-8310            TO EMI-ERROR
01679          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01680          GO TO 1005-CHECK-ERRORS.
01681
01682      IF CD-RETURN-CODE = '02'
01683          MOVE ER-8311            TO EMI-ERROR
01684          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01685          GO TO 1005-CHECK-ERRORS.
01686
01687      IF CD-RETURN-CODE = '03'
01688          MOVE ER-8312            TO EMI-ERROR
01689          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01690          GO TO 1005-CHECK-ERRORS.
01691
01692      IF CD-RETURN-CODE = '04'
01693          MOVE ER-8313            TO EMI-ERROR
01694          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01695          GO TO 1005-CHECK-ERRORS.
01696
01697      IF CD-RETURN-CODE = '05'
01698          MOVE ER-8314            TO EMI-ERROR
01699          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01700          GO TO 1005-CHECK-ERRORS.
01701
01702      IF CD-RETURN-CODE = '06'
01703          MOVE ER-8315            TO EMI-ERROR
01704          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01705          GO TO 1005-CHECK-ERRORS.
01706
01707      IF CD-RETURN-CODE = 'E1'
01708          MOVE ER-8316            TO EMI-ERROR
01709          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01710          GO TO 1005-CHECK-ERRORS.
01711
01712      IF CD-RETURN-CODE = 'N1'
01713          MOVE ER-8317            TO EMI-ERROR
01714          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01715          GO TO 1005-CHECK-ERRORS.
01716
01717  1004-CONT.
01718      MOVE W-EOB-CODE (W-EOB-NDX) TO PI-AT-EOB-CODE (W-EOB-NDX).
01719
01720      ADD +1 TO W-EOB-NDX.
01721      GO TO 1004-CHECK-EACH-EOB.
01722
01723  1004-CHECK-DUP-EOB.
01724      IF ((PI-AT-EOB-CODE (1) NOT = SPACE) AND
01725          (PI-AT-EOB-CODE (1) = PI-AT-EOB-CODE (2) OR
01726                                PI-AT-EOB-CODE (3) OR
01727                                PI-AT-EOB-CODE (4) OR
01728                                PI-AT-EOB-CODE (5)))
01729                   OR
01730         ((PI-AT-EOB-CODE (2) NOT = SPACE) AND
01731          (PI-AT-EOB-CODE (2) = PI-AT-EOB-CODE (3) OR
01732                                PI-AT-EOB-CODE (4) OR
01733                                PI-AT-EOB-CODE (5)))
01734                   OR
01735         ((PI-AT-EOB-CODE (3) NOT = SPACE) AND
01736          (PI-AT-EOB-CODE (3) = PI-AT-EOB-CODE (4) OR
01737                                PI-AT-EOB-CODE (5)))
01738                   OR
01739         ((PI-AT-EOB-CODE (4) NOT = SPACE) AND
01740          (PI-AT-EOB-CODE (4) = PI-AT-EOB-CODE (5)))
01741             MOVE ER-8000            TO EMI-ERROR
01742             MOVE AL-UABON           TO NOTE2A
01743             MOVE -1                 TO NOTE2L
01744             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01745             GO TO 1005-CHECK-ERRORS.
01746
01747  1005-CHECK-ERRORS.
121802*    IF PI-COMPANY-ID NOT = 'DMD'
01749         GO TO 1006-CHECK-ERRORS.
01750
01751 *********  DMD CODE START ******************
121802*    IF CL-CLAIM-TYPE NOT = PI-AH-OVERRIDE-L1 AND 'I' AND 'G'
121802*       GO TO 1006-CHECK-ERRORS.
121802*
121802*    PERFORM 7700-GET-DCT THRU 7700-EXIT.
121802*
121802*    COMPUTE W-PAYMENT-TOTAL
121802*         = CL-TOTAL-PAID-AMT + PI-EPYAMT
121802*    IF (W-PAYMENT-TOTAL > PI-MAX-BENEFIT-PYMT)
121802*               AND
121802*       (PI-MAX-BENEFIT-PYMT > ZEROS)
121802*          EXEC CICS SYNCPOINT ROLLBACK END-EXEC
121802*          MOVE ER-0967    TO EMI-ERROR
121802*          MOVE -1         TO PMTTYPEL
121802*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
121802*
121802*    GO TO 1006-CHECK-ERRORS.
01768 *********  DMD CODE END ******************
01769
01770  1006-CHECK-ERRORS.
120115     if emi-forcable-ctr = zeros
120115        and emi-fatal-ctr = zeros
120115        continue
120115     else
120115        IF NOT EMI-NO-ERRORS
120115           GO TO 2000-EDIT-DONE
120115        end-if
120115     end-if
01774      
      * EXEC CICS HANDLE CONDITION
01775 *        NOTFND(1950-NOT-FOUND)
01776 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00009542' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303039353432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01777
01778      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
01779      MOVE PI-CARRIER             TO MSTR-CARRIER.
01780      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
01781      MOVE PI-CERT-NO             TO MSTR-CERT-NO.
01782      MOVE 'MSTR'                 TO FILE-SWITCH.
01783
01784      PERFORM 7900-READ-CLAIM THRU 7900-EXIT.
01785
01786      IF PI-COMPANY-ID = 'DMD'
01787          PERFORM 8010-DMD-ERROR-CHECKS THRU 8090-EXIT.
01788
012407     IF WS-BIN-PROOF-DT NOT = LOW-VALUES
012407        IF WS-BIN-PROOF-DT < CL-INCURRED-DT
012407           MOVE ER-0873    TO EMI-ERROR
012407           MOVE -1                  TO PROOFDTL
012407           MOVE AL-UABON            TO PROOFDTA
012407           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
012407        END-IF
012407     END-IF
           if cl-benefit-period = zeros
              move er-1667             to emi-error
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           end-if
061013     MOVE ELMSTR-KEY             TO ELTRLR-KEY
061013     MOVE +95                    TO TRLR-SEQ-NO
061013     
      * EXEC CICS READ
061013*       DATASET  ('ELTRLR')
061013*       SET      (ADDRESS OF ACTIVITY-TRAILERS)
061013*       RIDFLD   (ELTRLR-KEY)
061013*       RESP     (WS-RESPONSE)
061013*    END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00009571' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039353731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     if ws-resp-normal
061013        perform varying s1 from +1 by +1 until
061013           at-note-error-no (s1) = spaces
061013           if at-note-error-no (s1) = '1662'
061013              continue
061013           else
061013              move at-note-error-no (s1)
061013                                 to emi-error
061013              if at-note-error-no (s1) = '1653'
061013                 evaluate true
061013                    when cl-claim-type = 'L'
061013                       move '  LF  '
061013                                 to emi-claim-type
061013                    when cl-claim-type = 'I'
061013                       move '  IU  '
061013                                 to emi-claim-type
022122                    when cl-claim-type = 'F'
022122                       move ' FMLA '
022122                                 to emi-claim-type
022122                    when cl-claim-type = 'B'
022122                       move ' BRV  '
022122                                 to emi-claim-type
022122                    when cl-claim-type = 'H'
022122                       move ' HOSP '
061013                                 to emi-claim-type
100518                    when cl-claim-type = 'O'
100518                       move '  OT  '
100518                                 to emi-claim-type
022122                    when other
022122                       move '  AH  '
022122                                 to emi-claim-type
061013                 end-evaluate
061013              end-if
061013              PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013           end-if
061013        end-perform
061013     end-if
01789      IF CL-AUTO-PAY-SEQ NOT = ZEROS
01790          IF PI-PMTTYPE = '5' OR '6'
01791              NEXT SENTENCE
01792          ELSE
01793              MOVE ER-0555             TO EMI-ERROR
01794              MOVE -1                  TO PMTTYPEL
01795              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01796
01797      IF CL-LAST-CLOSE-REASON = '2'
01798          IF CL-CLAIM-STATUS = 'C'
01799              MOVE ER-0760        TO  EMI-ERROR
01800              MOVE -1             TO  PMTTYPEL
01801              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01802
01803      IF PI-ACTIVITY-CODE = 9
01804          MOVE ER-3538            TO  EMI-ERROR
01805          MOVE -1                 TO  PMTTYPEL
01806          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01807
01808      IF PI-COMPANY-ID NOT = 'AIG' AND 'AUK'
01809          GO TO 1008-READ-CERT.
01810 **********************AIG LOGIC STARTS *********************
01811      IF CL-CERT-PRIME = 'INCOMPLETE'
01812          MOVE ER-3540            TO  EMI-ERROR
01813          MOVE -1                 TO  PMTTYPEL
01814          MOVE AL-UABON           TO  PMTTYPEA
01815          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01816          GO TO 2000-EDIT-DONE.
01817
01818      IF PI-PMTTYPE = '4'
01819          IF PI-EPYAMT IS NEGATIVE
01820              IF PI-ECHECKNO-CR = 'CR'
01821                  NEXT SENTENCE
01822              ELSE
01823                  MOVE 'CR'       TO  PI-ECHECKNO-CR.
01824
01825      IF PI-PMTTYPE = '4'
01826          IF PI-ECHECKNO-CR = 'CR'
01827              IF PI-EPYAMT IS NEGATIVE
01828                  NEXT SENTENCE
01829              ELSE
01830                  MOVE ER-0834    TO  EMI-ERROR
01831                  MOVE -1         TO  EPYAMTL
01832                  MOVE AL-UNBON   TO  EPYAMTA
01833                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01834
01835      IF PI-PMTTYPE = '4'
01836          IF PI-EPYAMT IS NEGATIVE
01837              IF PI-OFFLINE = 'Y'
01838                  NEXT SENTENCE
01839              ELSE
01840                  MOVE 'Y'        TO  PI-OFFLINE
01841                                      WS-OFFLINE
01842                                      OFFLINEO
01843                  MOVE AL-UANON   TO  OFFLINEA.
01844
020413*    IF PI-PMTTYPE = '4'
020413*        IF PI-EPYAMT IS NEGATIVE
020413*            IF PI-CASH = 'Y'
020413*                NEXT SENTENCE
020413*            ELSE
020413*                MOVE 'Y'        TO  CASHI PI-CASH WS-CASH
020413*                MOVE AL-UANON   TO  CASHA
020413*                GO TO 1007-DEFAULT-AIG-CHECKNO.
020413*
020413*    IF PI-CASH NOT = 'Y' AND 'N'
020413*         IF PI-OFFLINE = 'Y'  AND
020413*            PI-ECHECKNO-CR = 'CR'
020413*              MOVE 'Y'          TO  CASHI PI-CASH WS-CASH
020413*              MOVE AL-UANON     TO  CASHA
020413*              GO TO 1007-DEFAULT-AIG-CHECKNO.
020413*
020413*    IF PI-CASH NOT = 'Y' AND 'N'
020413*        IF PI-PMTTYPE = '5' OR '6'
020413*            MOVE 'Y'            TO  CASHI PI-CASH WS-CASH
020413*            MOVE AL-UANON       TO  CASHA
020413*            GO TO 1007-DEFAULT-AIG-CHECKNO.
020413*
020413*    IF PI-CASH NOT = 'Y' AND 'N'
020413*       IF CL-ASSOCIATES = 'N' OR 'A'
020413*           MOVE 'Y'             TO  CASHI PI-CASH WS-CASH
020413*           MOVE AL-UANON        TO  CASHA
020413*       ELSE
020413*           MOVE 'N'             TO  CASHI PI-CASH WS-CASH
020413*           MOVE AL-UANON        TO  CASHA.
020413*
020413*    IF PI-PMTTYPE = '5' OR '6'
020413*        IF PI-CASH = 'N'
020413*            MOVE ER-0819        TO  EMI-ERROR
020413*            MOVE AL-UANON       TO  CASHA
020413*            MOVE -1             TO  CASHL
020413*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
020413*
020413*    IF CL-ASSOCIATES = 'N' OR 'A'
020413*       IF CASHI = 'N'
020413*        MOVE ER-3537            TO  EMI-ERROR
020413*        MOVE AL-UANON           TO  CASHA
020413*        MOVE -1                 TO  CASHL
020413*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01888
01889  1007-DEFAULT-AIG-CHECKNO.
01890
01891      IF CL-CAUSE-CD = SPACES OR LOW-VALUES
01892          MOVE ER-3532            TO  EMI-ERROR
01893          MOVE -1                 TO  PMTTYPEL
01894          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01895 **********************AIG LOGIC ENDS ***********************
01896
01897  1008-READ-CERT.
01898
01899      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
01900      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
01901      MOVE CL-CERT-GROUPING       TO CERT-GROUPING.
01902      MOVE CL-CERT-STATE          TO CERT-STATE.
01903      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
01904      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
01905      MOVE CL-CERT-NO             TO CERT-CERT-NO.
01906      MOVE 'CERT'                 TO FILE-SWITCH.
01907
01908      PERFORM 7970-READ-CERT THRU 7970-EXIT.
01909
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
01911         MOVE CM-LF-LOAN-EXPIRE-DT
                                       TO WS-EXP-DT PI-EXP-DT
                                          ws-extended-exp-dt
01912      ELSE
01913         MOVE CM-AH-LOAN-EXPIRE-DT
                                       TO WS-EXP-DT PI-EXP-DT
                                          ws-extended-exp-dt
           end-if
022122     move cl-insured-birth-dt    to dc-bin-date-1
022122     move cl-incurred-dt         to dc-bin-date-2
022122     move '1'                    to dc-option-code
022122     PERFORM 9700-LINK-DATE-CONVERT
022122                                 THRU 9700-EXIT
022122     compute ws-att-age =
022122        dc-elapsed-months / 12
022122
022122     MOVE '6'                    TO DC-OPTION-CODE
022122     move zeros                  to dc-elapsed-months
022122                                    dc-elapsed-days
022122     move low-values to dc-bin-date-1 dc-bin-date-2
061013     if pi-dcc-product-code not = spaces
061013        move cm-ah-benefit-cd    to pi-ah-benefit-cd
              move cm-lf-benefit-cd    to pi-lf-benefit-cd
061013        PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
061013        IF PDEF-FOUND
100518         AND CL-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
061013           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122              (P1 > +11)
022122              OR ((PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
022122                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
061013           END-PERFORM
022122           IF P1 < +12
                    MOVE '6'           TO DC-OPTION-CODE
                    MOVE WS-EXP-DT     TO DC-BIN-DATE-1
081817              IF CL-NO-OF-EXTENSIONS NUMERIC
081817                 MOVE CL-NO-OF-EXTENSIONS TO DC-ELAPSED-MONTHS
081817                                             PI-MAX-EXT
081817              ELSE
061013                 MOVE PD-MAX-EXTENSION (P1)
061013                                 TO DC-ELAPSED-MONTHS
081817              END-IF
                    PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
                    if no-conversion-error
                       move dc-bin-date-2 to ws-extended-exp-dt
                    end-if
022122              if pd-wait-days(p1) not numeric
022122                 move zeros      to pd-wait-days(p1)
022122              end-if
070714           else
070714              move er-1674       to emi-error
070714              PERFORM 9900-ERROR-FORMAT
070714                                 THRU 9900-EXIT
                 end-if
081817        ELSE
081817           IF CL-NO-OF-EXTENSIONS NUMERIC
081817              MOVE CL-NO-OF-EXTENSIONS TO DC-ELAPSED-MONTHS
081817                                          PI-MAX-EXT
081817           ELSE
081817              MOVE ZERO  TO DC-ELAPSED-MONTHS
081817           END-IF
081817           MOVE WS-EXP-DT     TO DC-BIN-DATE-1
081817           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
081817           if no-conversion-error
081817              move dc-bin-date-2 to ws-extended-exp-dt
081817           end-if
              end-if
022122        if pdef-found
022122           AND CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 or 'O'
022122           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122              (P1 > +11)
022122              OR ((PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
022122                AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
022122           END-PERFORM
022122           IF P1 < +12
022122              if pd-ben-pct(p1) not numeric
022122                 move zeros to pd-ben-pct(p1)
022122              end-if
022122           end-if
022122        end-if
081817     ELSE
081817        IF CL-NO-OF-EXTENSIONS NUMERIC
081817           MOVE CL-NO-OF-EXTENSIONS TO DC-ELAPSED-MONTHS
081817                                       PI-MAX-EXT
081817        ELSE
081817           MOVE ZERO  TO DC-ELAPSED-MONTHS
081817        END-IF
081817        MOVE WS-EXP-DT     TO DC-BIN-DATE-1
081817        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
081817        if no-conversion-error
081817           move dc-bin-date-2 to ws-extended-exp-dt
081817        end-if
           end-if
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*        PERFORM 3400-COMPUTE-EXPIRY THRU 3499-EXIT.
01917
01918      IF CL-INCURRED-DT < CM-CERT-EFF-DT
090821         move er-1681            to emi-error
01919 *        MOVE ER-0458            TO EMI-ERROR
01920          PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.
01921
           IF (CL-INCURRED-DT > WS-EXP-DT)
              AND (CL-INCURRED-DT < WS-EXTENDED-EXP-DT)
              MOVE ER-1677             TO EMI-ERROR
              PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT
           END-IF
           move ws-extended-exp-dt     to ws-exp-dt pi-exp-dt
01922      IF CL-INCURRED-DT NOT < WS-EXTENDED-EXP-DT
01923          MOVE ER-0459            TO EMI-ERROR
01924          PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.
090821     if cl-claim-type = PI-LIFE-OVERRIDE-L1 OR 'O'
090821        move cm-lf-benefit-cd    to ws-ben-cd
090821        move '4'                 to cntl-rec-type
090821     else
090821        move cm-ah-benefit-cd    to ws-ben-cd
090821        move '5'                 to cntl-rec-type
090821     end-if
090821     move ws-access              to cntl-access
090821     move pi-company-id          to cntl-comp-id
090821     move zeros                  to cntl-seq-no
090821     move 'BENE'                 to file-switch
090821     perform 7200-find-benefit   thru 7200-exit
090821     if not no-benefit-found
090821        move cf-special-calc-cd(sub-1)
090821                                 to ws-special-calc-cd
090821     end-if
090821
090821     perform 7990-get-lo-hi-acct-dates
090821                                 thru 7990-exit
090821     if (cl-incurred-dt >= ws-hi-acct-dt)
090821        and (acct-cancelled)
090821        and ((ws-special-calc-cd = 'O')
090821                 or
090821            (pi-company-id = 'DCC' AND cl-carrier = '7'))
090821        MOVE er-1681             TO EMI-ERROR
090821        PERFORM 1010-GENERAL-ERROR
090821                                 THRU 1010-EXIT
090821     end-if
090821
090821*    if (cl-incurred-dt < ws-lo-acct-dt)
090821*       MOVE er-1681             TO EMI-ERROR
090821*       PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT
090821*    end-if
01926      IF CL-REPORTED-DT = LOW-VALUES
01927         MOVE ER-0530             TO EMI-ERROR
01928         PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.
01929
01930      IF PI-COMPANY-ID = 'NCL'
01931         IF CM-CREDIT-INTERFACE-SW-1 = '2'
01932             MOVE ER-0770            TO EMI-ERROR
01933             PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.
01934
01935      IF PI-COMPANY-ID = 'NCL'
01936         IF (CM-SING-PRM AND CERT-WAS-CREATED-FOR-CLAIM)
01937             MOVE ER-0737            TO EMI-ERROR
01938             PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.
01939
01940      IF CM-ENTRY-STATUS = 'D' OR 'V'
01941          MOVE ER-0768            TO  EMI-ERROR
01942          PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.
01943
01944      IF CM-CREDIT-INTERFACE-SW-1 = '4'
01945          MOVE ER-0769            TO  EMI-ERROR
01946          PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.
01947
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
01949          GO TO 1020-DO-LIFE-EDITS
01950      ELSE
01951          GO TO 1030-DO-AH-EDITS.
01952
01953  1010-GENERAL-ERROR.
01954      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01955
01956      IF EMI-MESSAGE-FORMATTED
01957          MOVE -1                 TO PMTTYPEL.
01958
01959  1010-EXIT.
01960      EXIT.
01961
01962      EJECT
01963  1020-DO-LIFE-EDITS.
01964      IF CM-LF-BENEFIT-CD = '00'
01965          IF PI-PMTTYPE = '5' OR '6'
01966              MOVE ER-0658        TO EMI-ERROR
01967              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
01968          ELSE
120115            if (pi-company-id = 'DCC' or 'VPP')
070714               and (pi-dcc-product-code not = spaces)
070714               continue
070714            else
01969                MOVE ER-1775      TO EMI-ERROR
01970                PERFORM 1040-PMTTYPE-ERROR
070714                                 THRU 1040-EXIT
070714            end-if
070714         end-if
070714     end-if
01971
           move cm-ah-benefit-cd to pi-ah-benefit-cd
01972      IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
01973          MOVE ER-0460            TO EMI-ERROR
01974          PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
01975
041710     MOVE CM-LF-BENEFIT-CD      TO PI-LF-BENEFIT-CD.
01976      MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.
01977      MOVE WS-ACCESS              TO  CNTL-ACCESS.
01978      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01979      MOVE '4'                    TO  CNTL-REC-TYPE.
01980      MOVE ZEROS                  TO  CNTL-SEQ-NO.
01981      MOVE 'BENE'                 TO  FILE-SWITCH.
01982      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
01983      IF NO-BENEFIT-FOUND
01984          GO TO 1950-NOT-FOUND.
01985
01986      MOVE CF-LF-COVERAGE-TYPE (SUB-1)   TO  PI-LF-COVERAGE-TYPE.
01987
01988      IF CM-LF-DEATH-EXIT-DT = LOW-VALUES
100518       AND NOT CM-LF-LUMP-SUM-DISAB
01989          IF PI-PMTTYPE = '4'
01990              IF (PI-LIFE-OVERRIDE-L1 = 'P' OR
01991                 PI-LF-COVERAGE-TYPE = 'P')
100518               AND NOT CM-LF-LUMP-SUM-DISAB
01992                  NEXT SENTENCE
01993              ELSE
01994                  MOVE ER-0461        TO EMI-ERROR
01995                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
01996          ELSE
01997             NEXT SENTENCE
01998      ELSE
01999          IF PI-PMTTYPE = '2'
02000             IF NOT CM-O-B-COVERAGE
02001               AND CM-LF-CURRENT-STATUS = '7'
02002                 MOVE ER-0462     TO EMI-ERROR
02003                 PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
02004
02005      IF PI-PMTTYPE = '2'
02006          IF (CM-LF-CANCEL-DT      NOT = LOW-VALUES AND SPACES) OR
02007             (CM-LF-CANCEL-EXIT-DT NOT = LOW-VALUES AND SPACES)
02008              MOVE CM-LF-ITD-CANCEL-AMT  TO  WS-REFUND-AMT
02009              MOVE ER-0730            TO  EMI-ERROR
02010              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
02011              MOVE WS-REFUND-AMT      TO  EMI-TEXT-VARIABLE (1).
02012
02013      IF PI-PMTTYPE = '2'
02014          IF PI-LIFE-OVERRIDE-L1 = 'P' OR
02015             PI-LF-COVERAGE-TYPE = 'P'
02016              IF CL-PAID-THRU-DT = CL-INCURRED-DT
02017                  MOVE ER-0462    TO  EMI-ERROR
02018                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
02019
02020      IF CM-LF-CANCEL-DT NOT = LOW-VALUES
02021        AND CL-INCURRED-DT > CM-LF-CANCEL-DT
02022          MOVE ER-0466            TO EMI-ERROR
02023          PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
02024
02025      IF PI-COMPANY-ID = 'FLI' OR 'FLU'
02026          IF PI-PMTTYPE = '3'
02027              MOVE 0436           TO EMI-ERROR
02028              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
02029              GO TO 1045-EDIT-PAYEE-ADDR
02030          ELSE
02031              GO TO 1045-EDIT-PAYEE-ADDR.
02032
100518     IF CL-CLAIM-TYPE = 'O'
100518         IF PI-PMTTYPE = '1' OR '2'
100518             MOVE ER-1930          TO EMI-ERROR
100518             PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
100518         END-IF
100518     ELSE
02033      IF PI-LIFE-OVERRIDE-L1 = 'P' OR
02034         PI-LF-COVERAGE-TYPE = 'P'
02035          IF PI-PMTTYPE = '3'
02036              MOVE ER-0436          TO  EMI-ERROR
02037              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
02038          ELSE
02039              NEXT SENTENCE
02040      ELSE
02041          IF PI-PMTTYPE = '1' OR '3'
02042              MOVE ER-0436            TO EMI-ERROR
02043              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
041710*
041710     IF PI-COMPANY-ID = 'CID' AND
041710        CL-CERT-STATE = 'SC'  AND
041710        (CM-LF-BENEFIT-CD = '2I' OR '2J' OR '2K' OR '2L')
041710           MOVE ER-0879    TO  EMI-ERROR
041710           PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
                 move 'NET PAY + 6'    TO EMI-ERROR-TEXT (1) (18:11)
041710     END-IF.
           IF PI-COMPANY-ID = 'AHL'
              evaluate true
                 when CM-LF-BENEFIT-CD = '5I' OR '6J'
                    MOVE ER-0879       TO  EMI-ERROR
                    PERFORM 1040-PMTTYPE-ERROR
                                       THRU 1040-EXIT
                    move 'NET PAY + 6' TO EMI-ERROR-TEXT (1) (18:11)
                 when CM-LF-BENEFIT-CD = '5G' OR '5J' OR '5S' OR
                       '6D' OR '6H' OR '6R'
                    MOVE ER-0879       TO  EMI-ERROR
                    PERFORM 1040-PMTTYPE-ERROR
                                       THRU 1040-EXIT
                    move 'NET PAY + 2' TO EMI-ERROR-TEXT (1) (18:11)
                 when CM-LF-BENEFIT-CD = '5M' OR '6M'
                    MOVE ER-0879       TO  EMI-ERROR
                    PERFORM 1040-PMTTYPE-ERROR
                                       THRU 1040-EXIT
                    move 'NET PAY + 1' TO EMI-ERROR-TEXT (1) (18:11)
              end-evaluate
           END-IF
02045      GO TO 1045-EDIT-PAYEE-ADDR.
02046
02047      EJECT
02048  1030-DO-AH-EDITS.
           MOVE CM-AH-BENEFIT-CD       TO PI-AH-BENEFIT-CD
02049      IF CM-AH-BENEFIT-CD = '00'
02050          IF PI-PMTTYPE = '5' OR '6'
02051              MOVE ER-0658        TO EMI-ERROR
02052              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
02053          ELSE
02054              MOVE ER-1775        TO EMI-ERROR
02055              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
02056
02057      IF CL-TOTAL-PAID-AMT = ZEROS
02058          IF PI-PMTTYPE = '4'
02059              MOVE ER-0438        TO EMI-ERROR
02060              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
02061
02062      IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES
02063          IF PI-PMTTYPE = '3'
02064              MOVE ER-0463        TO EMI-ERROR
02065              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
02066          ELSE
02067              IF (CL-INCURRED-DT NOT < CM-LF-DEATH-EXIT-DT)
02068               AND CM-LF-CURRENT-STATUS = '7'
02069                  MOVE ER-0465    TO EMI-ERROR
02070                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
02071
02072      IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
02073          IF PI-PMTTYPE = '3'
02074              MOVE ER-0464        TO EMI-ERROR
02075              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
02076          ELSE
02077              IF CL-INCURRED-DT > CM-AH-SETTLEMENT-DT
02078                  MOVE ER-0469    TO EMI-ERROR
02079                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
02080
02081      IF CM-AH-CANCEL-DT NOT = LOW-VALUES
02082          IF PI-PMTTYPE = '3'
02083              IF CL-INCURRED-DT NOT = CM-AH-CANCEL-DT
02084                  MOVE ER-0467    TO EMI-ERROR
02085                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
02086              ELSE
02087                  NEXT SENTENCE
02088          ELSE
02089              IF CL-INCURRED-DT NOT < CM-AH-CANCEL-DT
02090                  MOVE ER-0468    TO EMI-ERROR
02091                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.
02092
112018     IF PI-PMTTYPE = '2'
112018        PERFORM WITH TEST AFTER
112018         VARYING SUB FROM +1 BY +5
112018          UNTIL SUB > +60
112018            OR PI-PMTNOTE2(SUB:1) = 'F'
112018            OR PI-PMTNOTE2(SUB:1) NOT > SPACES
112018        END-PERFORM
112018        IF PI-PMTNOTE2(SUB:1) NOT = 'F'
112018           MOVE ER-3551    TO EMI-ERROR
112018           PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
112018        END-IF
112018     END-IF.
02093      GO TO 1045-EDIT-PAYEE-ADDR.
02094
02095  1040-PMTTYPE-ERROR.
02096      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02097
02098      IF EMI-MESSAGE-FORMATTED
02099          MOVE -1                 TO PMTTYPEL
02100          MOVE AL-UABON           TO PMTTYPEA.
02101
02102  1040-EXIT.
02103      EXIT.
02104
02105      EJECT
02106  1045-EDIT-PAYEE-ADDR.
02107
02108      IF PI-OFFLINE = 'N' AND
02109         ((PI-PAYEE-TYPE = 'A' AND CL-ACCOUNT-ADDR-CNT
02110            < PI-PAYEE-SEQ-NUM)
02111         OR
02112          (PI-COMPANY-ID NOT = 'AIG' AND 'AUK') AND
02113          (PI-PAYEE-TYPE = 'B')                 AND
02114          (CL-BENIF-ADDR-CNT < PI-PAYEE-SEQ-NUM)
02115         OR
02116          (PI-PAYEE-TYPE = 'I' AND CL-INSURED-ADDR-CNT
02117            < PI-PAYEE-SEQ-NUM)
02118         OR
02119          (PI-PAYEE-TYPE = 'O' AND CL-OTHER-1-ADDR-CNT
02120            < PI-PAYEE-SEQ-NUM)
02121         OR
02122          (PI-PAYEE-TYPE = 'Q' AND CL-OTHER-2-ADDR-CNT
02123            < PI-PAYEE-SEQ-NUM)
02124         OR
02125          (PI-PAYEE-TYPE = 'P' AND CL-DOCTOR-ADDR-CNT
02126            < PI-PAYEE-SEQ-NUM)
02127         OR
02128          (PI-PAYEE-TYPE = 'E' AND CL-EMPLOYER-ADDR-CNT
02129            < PI-PAYEE-SEQ-NUM))
02130              MOVE ER-0437        TO EMI-ERROR
02131              MOVE -1             TO PAYEEL
02132              MOVE AL-UABON       TO PAYEEA
02133              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02134
02135      IF PI-OFFLINE    = 'N' AND
02136         PI-PAYEE-TYPE = 'B' AND
02137         PI-PAYEE-SEQ  = '0'
02138          PERFORM 7920-READ-BENE THRU 7920-EXIT
02139          IF BENE-FOUND-SW = 'N'
02140              MOVE ER-0437        TO EMI-ERROR
02141              MOVE -1             TO PAYEEL
02142              MOVE AL-UABON       TO PAYEEA
02143              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02144
02145      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
02146         IF PI-OFFLINE    = 'N'  AND
02147            PI-PAYEE-TYPE = 'B'  AND
02148            PI-PAYEE-SEQ  = '9'
02149              PERFORM 7920-READ-BENE THRU 7920-EXIT
02150              IF BENE-FOUND-SW = 'N'
02151                 MOVE ER-0437        TO EMI-ERROR
02152                 MOVE -1             TO PAYEEL
02153                 MOVE AL-UABON       TO PAYEEA
02154                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02155
121802*    IF PI-COMPANY-ID = 'CRI'
121802*        IF CL-CLAIM-TYPE = 'L'
121802*            MOVE 'N'                TO  WS-GROUPED
121802*                                        PI-GROUPED
121802*                                        GROUPEDI
121802*            MOVE AL-UANON           TO  GROUPEDA
121802*            GO TO 1050-EDIT-CHECK-NO
121802*        ELSE
121802*            MOVE CL-CERT-GROUPING   TO  WS-GROUPING
121802*            IF WS-GROUP-1 = 'C'
121802*                NEXT SENTENCE
121802*            ELSE
121802*                MOVE 'N'            TO  WS-GROUPED
121802*                                        PI-GROUPED
121802*                                        GROUPEDI
121802*                MOVE AL-UANON       TO  GROUPEDA
121802*                GO TO 1050-EDIT-CHECK-NO.
02173
020413*     IF PI-GROUPED = 'Y'
020413*        IF PI-PAYEE-TYPE = 'B' OR 'A'
020413*           NEXT SENTENCE
020413*        ELSE
020413*           MOVE ER-0437        TO EMI-ERROR
020413*           MOVE -1             TO PAYEEL
020413**02180            MOVE AL-UABON       TO PAYEEA
020413*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
020413*
020413*     IF (PI-PAYEE-TYPE = 'B' OR 'A') AND
020413*        (PI-GROUPED    = ' ')
020413*        NEXT SENTENCE
020413*     ELSE
020413*
02187      GO TO 1050-EDIT-CHECK-NO.
020413*     IF PI-COMPANY-ID = 'CRI'
020413*        MOVE 'N'           TO GROUPEDI
020413*                              PI-GROUPED
020413*                              WS-GROUPED
020413*        MOVE AL-UANON      TO GROUPEDA.
020413*
020413*     IF PI-COMPANY-ID = 'CRI'
020413*        IF PI-PAYEE-TYPE = 'B'
020413*           IF BENE-FOUND-SW = 'Y'
020413*              IF BE-GROUP-CHECKS-Y-N = 'Y'
020413*                 MOVE 'Y'           TO GROUPEDI
020413*                                       PI-GROUPED
020413*                                       WS-GROUPED
020413*                 MOVE AL-UANON      TO GROUPEDA
020413*              ELSE
020413*                 NEXT SENTENCE
020413*           ELSE
020413*              PERFORM 7920-READ-BENE THRU 7920-EXIT
020413*              IF BENE-FOUND-SW = 'Y'
020413*                 IF BE-GROUP-CHECKS-Y-N = 'Y'
020413*                    MOVE 'Y'      TO GROUPEDI
020413*                                     PI-GROUPED
020413*                                     WS-GROUPED
020413*                    MOVE AL-UANON TO GROUPEDA.
020413*
02213
02214      IF PI-COMPANY-ID NOT = 'CRI'
02215         GO TO 1050-EDIT-CHECK-NO.
02216
02217 *************** CRI CODE START ******************
02218      IF PI-PAYEE-TYPE NOT = 'A'
02219         GO TO 1050-EDIT-CHECK-NO.
02220
02221      MOVE LOW-VALUES             TO ERACCT-KEY.
02222      MOVE PI-COMPANY-CD          TO ACCT-COMP-CD.
02223      MOVE PI-CARRIER             TO ACCT-CARRIER.
02224      MOVE PI-GROUPING            TO ACCT-GROUPING.
02225      MOVE PI-STATE               TO ACCT-STATE.
02226      MOVE PI-ACCOUNT             TO ACCT-ACCOUNT.
02227      MOVE PI-CERT-EFF-DT         TO ACCT-EXP-DT.
02228      MOVE 'ACCT'                 TO FILE-SWITCH.
02229
02230      MOVE ERACCT-PARTIAL-KEY     TO WS-ERACCT-SAVE-KEY.
02231      MOVE SPACES                 TO WS-ERACCT-HOLD-RECORD.
02232
02233      
      * EXEC CICS HANDLE CONDITION
02234 *         NOTFND   (1950-NOT-FOUND)
02235 *         ENDFILE  (1950-NOT-FOUND)
02236 *    END-EXEC.
      *    MOVE '"$I''                  ! ( #00010248' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303130323438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02237
02238      
      * EXEC CICS STARTBR
02239 *         DATASET   ('ERACCT')
02240 *         RIDFLD    (ERACCT-KEY)
02241 *         GTEQ
02242 *    END-EXEC.
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010253' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02243
02244  1046-READNEXT-ERACCT.
090821     set eracct-browse-started to true
02245
02246      
      * EXEC CICS READNEXT
02247 *         DATASET   ('ERACCT')
02248 *         RIDFLD    (ERACCT-KEY)
02249 *         SET       (ADDRESS OF ACCOUNT-MASTER)
02250 *    END-EXEC.
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00010262' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
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
           
02251
02252      IF WS-ERACCT-SAVE-KEY NOT = ERACCT-PARTIAL-KEY
02253         IF WS-ERACCT-HOLD-RECORD = SPACES
02254            GO TO 1950-NOT-FOUND
02255         ELSE
02256            MOVE WS-ERACCT-HOLD-RECORD TO ACCOUNT-MASTER
02257            GO TO 1048-CONTINUE.
02258
02259      IF ACCT-EXP-DT = HIGH-VALUES
02260         NEXT SENTENCE
02261      ELSE
02262         MOVE ACCOUNT-MASTER TO WS-ERACCT-HOLD-RECORD
02263         GO TO 1046-READNEXT-ERACCT.
02264
02265  1048-CONTINUE.
02266
020413*    IF AM-GROUPED-CHECKS-Y-N = 'Y'
020413*       MOVE 'Y'                 TO GROUPEDI
020413*                                   PI-GROUPED
020413*                                   WS-GROUPED
020413*       MOVE AL-UANON            TO GROUPEDA.
02272 *************** CRI CODE END ******************
02273
02274      EJECT
02275  1050-EDIT-CHECK-NO.
090821     if eracct-browse-started
090821        
      * exec cics endbr
090821*          dataset('ERACCT')
090821*       end-exec
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010293' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821        move spaces              to ws-eracct-startbr-ind
090821     end-if
02277      IF PI-OFFLINE = 'Y'
02278          IF PI-ECHECKNO = SPACES
02279               MOVE ER-0439        TO EMI-ERROR
02280               GO TO 1090-CHECK-NO-ERROR
02281            ELSE
020413*        IF PI-COMPANY-ID = 'AIG' OR 'AUK'
020413*           IF PI-ECHECKNO-N = 'N' AND
020413*              PI-CASH       = 'Y'
020413*                MOVE ER-3535        TO EMI-ERROR
020413*                GO TO 1090-CHECK-NO-ERROR
020413*             ELSE
020413*                MOVE AL-UANON    TO CHECKNOA
020413*                GO TO 1100-EDIT-HOLDTIL
020413*         ELSE
02291             MOVE AL-UANON    TO CHECKNOA
02292             GO TO 1100-EDIT-HOLDTIL.
02293
02294      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02295      MOVE '6'                    TO CNTL-REC-TYPE.
02296
02297      IF CONTROL-IS-ACTUAL-CARRIER
02298          MOVE PI-CARRIER         TO WS-CARR
02299      ELSE
02300          MOVE PI-CARRIER-CONTROL-LEVEL TO WS-CARR.
02301
02302      IF PI-COMPANY-ID = 'FIA'
02303          MOVE PI-CARRIER         TO WS-CARR.
02304
02305      MOVE WS-CARR-ACCESS         TO CNTL-ACCESS.
02306      MOVE 0                      TO CNTL-SEQ-NO.
02307      MOVE 'CARR'                 TO FILE-SWITCH.
02308
02309      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.
02310
02311      EJECT
02312      IF PI-COMPANY-ID NOT = 'AIG' AND 'AUK'
02313          GO TO 1051-EDIT-CHECKNO-CONT.
02314
02315 **********************AIG LOGIC STARTS**********************
02316      IF PI-ECHECKNO = SPACES
02317          IF CHECK-NO-MANUAL
02318              MOVE ER-0441        TO EMI-ERROR
02319              GO TO 1090-CHECK-NO-ERROR
02320          ELSE
02321              MOVE AL-UANOF       TO CHECKNOA
02322              GO TO 1100-EDIT-HOLDTIL.
02323
02324      IF PI-ECHECKNO NOT = SPACES
02325          IF CHECK-NO-MANUAL
02326              MOVE AL-UANON       TO CHECKNOA
02327              GO TO 1100-EDIT-HOLDTIL.
02328
02329      IF PI-ECHECKNO-CR = 'CR'
02330         IF PI-OFFLINE = 'Y'
02331             MOVE AL-UANON       TO CHECKNOA
02332             GO TO 1100-EDIT-HOLDTIL
02333         ELSE
02334             MOVE ER-3534        TO EMI-ERROR
02335             GO TO 1090-CHECK-NO-ERROR.
02336
02337      IF PI-ECHECKNO-N = 'N'
02338         IF PI-CASH = 'N'
02339             MOVE AL-UANON       TO CHECKNOA
02340             GO TO 1100-EDIT-HOLDTIL
02341         ELSE
02342             MOVE ER-3535          TO EMI-ERROR
02343             GO TO 1090-CHECK-NO-ERROR.
02344
02345      MOVE ER-0442        TO EMI-ERROR
02346      GO TO 1090-CHECK-NO-ERROR.
02347 **********************AIG LOGIC ENDS************************
02348
02349  1051-EDIT-CHECKNO-CONT.
02350
02351      IF PI-COMPANY-ID = 'DMD'
02352         IF PI-ECHECKNO NOT = SPACES
02353            IF PI-ECHECKNO (6:2) NOT = 'TP'
02354               MOVE ER-0442        TO EMI-ERROR
02355               GO TO 1090-CHECK-NO-ERROR
02356             ELSE
02357               GO TO 1100-EDIT-HOLDTIL
02358           ELSE
02359            GO TO 1100-EDIT-HOLDTIL.
02360
02361      IF PI-ECHECKNO = SPACES
02362          IF CHECK-NO-MANUAL
02363              MOVE ER-0441        TO EMI-ERROR
02364              GO TO 1090-CHECK-NO-ERROR
02365          ELSE
02366              MOVE AL-UANOF       TO CHECKNOA
02367              GO TO 1100-EDIT-HOLDTIL
02368      ELSE
02369          IF CHECK-NO-MANUAL
02370              MOVE AL-UANON       TO CHECKNOA
02371              GO TO 1100-EDIT-HOLDTIL
02372          ELSE
02373              MOVE ER-0442        TO EMI-ERROR
02374              GO TO 1090-CHECK-NO-ERROR.
02375
02376  1090-CHECK-NO-ERROR.
02377      MOVE -1                     TO CHECKNOL.
02378      MOVE AL-UABON               TO CHECKNOA.
02379      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02380
02381      GO TO 2000-EDIT-DONE.
02382
02383      EJECT
02384  1100-EDIT-HOLDTIL.
040819     MOVE ZERO TO WS-TOT-AMOUNT-PAID
040819                  WS-UNPD-HOLD-UNTIL-DT
022718     SET HOLD-UNTIL-CHK TO TRUE
022718     MOVE CL-PAID-THRU-DT TO WS-HOLD-UNTIL-DT
022718     PERFORM 1166-CHECK-ALL-TRLRS THRU 1166-TRLR-EXIT.
040819     IF CL-TOTAL-PAID-AMT NOT = WS-TOT-AMOUNT-PAID
040819         MOVE ER-3277            TO EMI-ERROR
040819         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040819     END-IF
040819
022718     MOVE SPACE TO WS-HOLD-UNTIL-SW
040819
02385      MOVE AL-UANON               TO HOLDTILA.
02386
02387      IF PI-HOLDTIL = ZEROS
02388         IF PI-OFFLINE = 'Y'
02389            MOVE ER-0572          TO EMI-ERROR
02390            GO TO 1105-HOLDTIL-ERROR
02391         ELSE
02392            GO TO 1110-EDIT-PAID-FROM.
02393
02394      MOVE '4'                    TO DC-OPTION-CODE.
02395      MOVE PI-HOLDTIL             TO DC-GREG-DATE-1-MDY.
02396      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02397
02398      IF DATE-CONVERSION-ERROR
02399          MOVE ER-0446            TO EMI-ERROR
02400          GO TO 1105-HOLDTIL-ERROR
           END-IF
PEMTST     IF PI-OFFLINE = 'Y'
              IF DC-BIN-DATE-1 < SAVE-BIN-DATE
02399            MOVE ER-0446            TO EMI-ERROR
02400            GO TO 1105-HOLDTIL-ERROR
              END-IF
           END-IF
040819
040819     IF DC-BIN-DATE-1 < WS-UNPD-HOLD-UNTIL-DT
040819        MOVE ER-3278          TO EMI-ERROR
040819        GO TO 1105-HOLDTIL-ERROR
040819     END-IF
02401
02402      IF PI-OFFLINE = 'Y'
02403         GO TO 1110-EDIT-PAID-FROM.
02404
02405      MOVE DC-BIN-DATE-1          TO DC-BIN-DATE-2.
02406      MOVE WS-TODAY-DATE          TO DC-BIN-DATE-1.
02407      MOVE '1'                    TO DC-OPTION-CODE.
02408      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02409
022718*    IF DC-ELAPSED-DAYS > 30 OR DATE-CONVERSION-ERROR
022718     IF DC-ELAPSED-DAYS > 60 OR DATE-CONVERSION-ERROR
02411          MOVE ER-0447            TO EMI-ERROR
02412          GO TO 1105-HOLDTIL-ERROR.
02413
02414      GO TO 1110-EDIT-PAID-FROM.
02415
02416  1105-HOLDTIL-ERROR.
02417      MOVE -1                     TO HOLDTILL.
02418      MOVE AL-UNBON               TO HOLDTILA.
02419      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02420
02421      EJECT
02422  1110-EDIT-PAID-FROM.
02423      MOVE AL-UANON               TO EPYFROMA.
02424
02425      IF PI-EPYFROM = ZEROS
02426          GO TO 1120-CALCULATE-PAID-FROM.
02427
02428      IF PI-PMTTYPE = '5' OR '6'
02429          MOVE ER-0448            TO EMI-ERROR
02430          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02431          MOVE AL-UNBON           TO EPYFROMA
02432          MOVE -1                 TO EPYFROML
02433          GO TO 1150-EDIT-PAID-THRU.
02434
02435      MOVE '4'                    TO DC-OPTION-CODE.
02436      MOVE PI-EPYFROM             TO DC-GREG-DATE-1-MDY.
02437      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02438
02439      IF DATE-CONVERSION-ERROR
02440          MOVE ER-0449            TO EMI-ERROR
02441          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02442          MOVE AL-UNBON           TO EPYFROMA
02443          MOVE -1                 TO EPYFROML
02444      ELSE
02445          MOVE DC-BIN-DATE-1      TO WS-SV-EPYFROM.
02446
02447      EJECT
02448  1120-CALCULATE-PAID-FROM.
02449      IF PI-PMTTYPE = '5' OR '6'
02450          GO TO 1150-EDIT-PAID-THRU.
02451
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*       IF PI-PMTTYPE = '4'
121802*         GO TO 1150-EDIT-PAID-THRU.
02455
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
02457          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
02458          MOVE ' '                TO DC-OPTION-CODE
02459          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02460          MOVE DC-BIN-DATE-1      TO PI-CPYFROM
02461          GO TO 1145-TEST-PAID-FROM.
02462
02463      IF PI-PMTTYPE = '4'
02464         GO TO 1150-EDIT-PAID-THRU.
02465
02466      MOVE CM-AH-BENEFIT-CD       TO WS-BEN-CD.
02467      MOVE WS-ACCESS              TO CNTL-ACCESS.
02468      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02469      MOVE '5'                    TO CNTL-REC-TYPE.
02470      MOVE ZEROS                  TO CNTL-SEQ-NO.
02471      MOVE 'BENE'                 TO FILE-SWITCH.
02472
02473      IF WS-BEN-CD = ZEROS
02474          GO TO 1950-NOT-FOUND.
02475
02476      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
02477
02478      IF NO-BENEFIT-FOUND
02479          GO TO 1950-NOT-FOUND.
02480
02481      MOVE CF-BENEFIT-ALPHA   (SUB-1) TO PI-BENEFIT-SAVE.
02482      MOVE CF-SPECIAL-CALC-CD (SUB-1) TO WS-SPECIAL-CALC-CD.
022122     if pdef-found
022122        if pd-ret-elim(p1) = 'R' or 'E'
022122           move pd-ret-elim(p1)  to pi-ben-type
022122           move pd-wait-days(p1) to pi-ben-days
022122        end-if
022122     end-if
02484      IF PI-BEN-DAYS NOT NUMERIC
02485         MOVE ZEROS               TO PI-BEN-DAYS.
02486
02487      MOVE PI-BEN-DAYS            TO WS-BEN-DAYS.
02480
022718     IF WS-HOLD-UNTIL-DT > CL-PAID-THRU-DT
022718         MOVE WS-HOLD-UNTIL-DT     TO DC-BIN-DATE-1
022718         MOVE '6'                 TO DC-OPTION-CODE
022718         MOVE +0                  TO DC-ELAPSED-MONTHS
022718         MOVE +1                  TO DC-ELAPSED-DAYS
022718         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
022718         MOVE DC-BIN-DATE-2       TO PI-CPYFROM
022718         GO TO 1145-TEST-PAID-FROM.
02488
02489      IF (CL-PAID-THRU-DT NOT = LOW-VALUES AND SPACES)  AND
02490         (CL-TOTAL-PAID-AMT > ZEROS)
02491          MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1
02492          MOVE '6'                 TO DC-OPTION-CODE
02493          MOVE +0                  TO DC-ELAPSED-MONTHS
02494          MOVE +1                  TO DC-ELAPSED-DAYS
02495          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02496          MOVE DC-BIN-DATE-2       TO PI-CPYFROM
02497          GO TO 1145-TEST-PAID-FROM.
02498
02499 ******************************************************************
02500 ****** ON FIRST PAYMENTS, ELIM/RETRO EDITS TO BE MADE AGAINST THE
02501 ****** SPECIAL ENTERED DATE.
02502 ******************************************************************
02503
02504      IF PI-AIGFROM = ZEROS
02505          GO TO 1130-CONTINUE-PAID-FROM.
02506
02507      MOVE AL-UANON TO AIGFROMA.
02508
02509      MOVE '4'                    TO DC-OPTION-CODE.
02510      MOVE PI-AIGFROM             TO DC-GREG-DATE-1-MDY.
02511      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02512
02513      IF DATE-CONVERSION-ERROR
02514          MOVE ER-0449            TO EMI-ERROR
02515          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02516          MOVE AL-UNBON           TO AIGFROMA
02517          MOVE -1                 TO AIGFROML
02518          MOVE CL-INCURRED-DT TO DC-BIN-DATE-1
02519      ELSE
02520          MOVE 'Y'                TO WS-AIG-PAYFROM-SW
02521          MOVE DC-BIN-DATE-1      TO WS-SV-AIGFROM.
02522
02523      GO TO 1140-CONTINUE-PAID-FROM.
02524
02525  1130-CONTINUE-PAID-FROM.
02526
02527      IF (CL-PAID-THRU-DT   = LOW-VALUES OR SPACES) OR
02528         (CL-TOTAL-PAID-AMT = ZEROS)
02529            MOVE CL-INCURRED-DT TO DC-BIN-DATE-1.
02530
02531  1140-CONTINUE-PAID-FROM.
02532
02533      IF PI-BEN-TYPE = 'R'
02534          IF PI-BEN-DAYS = ZEROS
02535              NEXT SENTENCE
02536          ELSE
02537          IF PI-COMPANY-ID NOT = 'DMD'
02538              SUBTRACT 1 FROM PI-BEN-DAYS.
02539
02540      MOVE DC-BIN-DATE-1          TO WS-HOLD-BIN-DATE-1.
02541
02542      IF PI-COMPANY-ID = 'DMD'
02543          MOVE -1                 TO DC-ELAPSED-DAYS
02544          MOVE +0                 TO DC-ELAPSED-MONTHS
02545          MOVE '6'                TO DC-OPTION-CODE
02546          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02547          MOVE DC-BIN-DATE-2      TO DC-BIN-DATE-1
02548          DIVIDE PI-BEN-DAYS BY 30 GIVING DC-ELAPSED-MONTHS
02549                                REMAINDER DC-ELAPSED-DAYS
02550       ELSE
02551          MOVE PI-BEN-DAYS        TO DC-ELAPSED-DAYS
02552          MOVE +0                 TO DC-ELAPSED-MONTHS.
02553
02554      MOVE '6'                    TO DC-OPTION-CODE.
02555      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02556      MOVE DC-BIN-DATE-2          TO WS-RETRO-ELIM-DATE.
02557
02558      MOVE WS-HOLD-BIN-DATE-1     TO DC-BIN-DATE-1.
02559
02560      IF WS-RETRO-ELIM-DATE > WS-TODAY-DATE
02561          MOVE ER-0541            TO EMI-ERROR
02562          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02563          MOVE -1                 TO EPYFROML
02564          MOVE AL-UNBON           TO EPYFROMA.
02565
02566      IF (PI-COMPANY-ID = 'AIG' OR 'AUK')  AND
02567         (PI-BENEFIT-SAVE = '60E')
02568         IF (CL-PAID-THRU-DT   = LOW-VALUES OR SPACES) OR
02569            (CL-TOTAL-PAID-AMT = ZEROS)
02570              MOVE WS-HOLD-BIN-DATE-1  TO DC-BIN-DATE-1
02571              MOVE 30                  TO DC-ELAPSED-DAYS
02572              MOVE +0                  TO DC-ELAPSED-MONTHS
02573              MOVE '6'                 TO DC-OPTION-CODE
02574              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02575
02576      IF PI-BEN-TYPE = 'R'
02577          MOVE DC-BIN-DATE-1      TO PI-CPYFROM
02578      ELSE
02579          MOVE DC-BIN-DATE-2      TO PI-CPYFROM
02580          IF WS-SV-EPYFROM = LOW-VALUES
02581             MOVE DC-BIN-DATE-2   TO WS-SV-EPYFROM.
02582
02583      IF CL-PAID-THRU-DT = LOW-VALUES OR SPACES
02584          IF WS-USE-AIG-PAYFROM   OR
02585             WS-AIG-RECALC-PAYFROM
02586               MOVE PI-CPYFROM     TO  WS-SV-EPYFROM
02587                                       DC-BIN-DATE-1
02588               MOVE ' '            TO  DC-OPTION-CODE
02589               MOVE +0             TO  DC-ELAPSED-MONTHS
02590                                       DC-ELAPSED-DAYS
02591               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02592               IF NO-CONVERSION-ERROR
02593                   MOVE DC-GREG-DATE-1-MDY TO PI-EPYFROM.
02594
02595      IF PI-CPYFROM < CL-INCURRED-DT
02596         MOVE -1                  TO EPYFROML
02597         MOVE ER-3542             TO EMI-ERROR
02598         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02599
02600      IF CM-ENTRY-DT < CL-INCURRED-DT
02601         MOVE CM-ENTRY-DT            TO DC-BIN-DATE-1
02602         MOVE CL-INCURRED-DT         TO DC-BIN-DATE-2
02603         MOVE +0                     TO DC-ELAPSED-DAYS
02604         MOVE +0                     TO DC-ELAPSED-MONTHS
02605         MOVE '1'                    TO DC-OPTION-CODE
02606         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02607         ELSE
02608         MOVE ZEROS TO DC-ELAPSED-MONTHS.
02609
02610      IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'DMD'
02611          NEXT SENTENCE
02612      ELSE
02613          IF DC-ELAPSED-MONTHS < 2
02614              MOVE ER-3255        TO EMI-ERROR
02615              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02616
02617  1145-TEST-PAID-FROM.
02618      IF LOW-VALUES = WS-SV-EPYFROM OR PI-CPYFROM
02619          GO TO 1150-EDIT-PAID-THRU.
02620
02621      IF WS-SV-EPYFROM NOT = PI-CPYFROM
02622          MOVE PI-CPYFROM         TO DC-BIN-DATE-1
02623          MOVE ' '                TO DC-OPTION-CODE
02624          MOVE +0                 TO DC-ELAPSED-MONTHS
02625                                     DC-ELAPSED-DAYS
02626          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02627          IF NO-CONVERSION-ERROR
02628              MOVE DC-GREG-DATE-1-MDY TO EMI-DATE-FIELD
02629          ELSE
02630              MOVE SPACES             TO EMI-DATE-FIELD.
02631
022718     IF PI-HOLDTIL NOT = ZEROS
022718        CONTINUE
022718     ELSE
02632      IF WS-SV-EPYFROM NOT = PI-CPYFROM
02633          IF CL-TOTAL-PAID-AMT = ZEROS
02634              MOVE ER-3543            TO EMI-ERROR
02635              MOVE WS-SV-EPYFROM      TO PI-CPYFROM
02636              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02637              IF EMI-MESSAGE-FORMATTED
02638                 MOVE -1             TO EPYFROML
02639                 MOVE AL-UNBON       TO EPYFROMA
02640              ELSE
02641                 NEXT SENTENCE
02642          ELSE
02643              MOVE ER-0452            TO EMI-ERROR
02644              MOVE WS-SV-EPYFROM      TO PI-CPYFROM
02645              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02646              IF EMI-MESSAGE-FORMATTED
02647                 MOVE -1             TO EPYFROML
02648                 MOVE AL-UNBON       TO EPYFROMA.
02649
02650      EJECT
02651  1150-EDIT-PAID-THRU.
02652
02653      MOVE AL-UANON               TO EPYTHRUA.
02654
02655      IF PI-EPYTHRU = ZEROS
050616*        IF PI-PMTTYPE = '4' OR '5' OR '6'
050616         IF PI-PMTTYPE = '5' OR '6'
100518           OR (CL-CLAIM-TYPE NOT = 'L' AND 'O'
100518             AND PI-PMTTYPE = '4')
02657              GO TO 1200-EDIT-FIND-TOLERANCES
02658          ELSE
050616            IF (PI-PMTTYPE = '3') OR (PI-PMTTYPE = '4') OR
02660                (PI-PMTTYPE = '2' AND
100518                 CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O')
02662                GO TO 1152-CHECK-TYPE
02663             ELSE
02664                MOVE ER-0473      TO EMI-ERROR
02665                GO TO 1170-PAID-THRU-FATAL-ERROR
02666      ELSE
02667          IF PI-PMTTYPE = '5' OR '6'
02668              MOVE ER-0474        TO EMI-ERROR
02669              GO TO 1170-PAID-THRU-FATAL-ERROR.
02670
02671      MOVE '4'                    TO DC-OPTION-CODE.
02672      MOVE PI-EPYTHRU             TO DC-GREG-DATE-1-MDY.
02673      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02674
02675      IF DATE-CONVERSION-ERROR
02676          MOVE ER-0475            TO EMI-ERROR
02677          GO TO 1170-PAID-THRU-FATAL-ERROR.
02678
02679      MOVE DC-BIN-DATE-1       TO WS-SV-EPYTHRU.
02680
02681      IF PI-COMPANY-ID = 'DMD'
02682          IF PI-PMTTYPE NOT = '2'
02683                  AND
02684             WS-EXP-DT > WS-SV-EPYTHRU
02685              MOVE WS-SV-EPYTHRU    TO DC-BIN-DATE-1
02686              MOVE WS-EXP-DT        TO DC-BIN-DATE-2
02687              MOVE '1'              TO DC-OPTION-CODE
02688              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02689              IF DATE-CONVERSION-ERROR
02690                  MOVE ER-0944      TO EMI-ERROR
02691                  GO TO 1170-PAID-THRU-FATAL-ERROR
02692              ELSE
02693                  IF DC-ELAPSED-MONTHS = ZEROS AND
02694                     DC-ELAPSED-DAYS   = 1
02695                      MOVE ER-0945  TO EMI-ERROR
02696                      MOVE -1       TO EPYTHRUL
02697                      MOVE AL-UABON TO EPYTHRUA
02698                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02699
02700      IF PI-PMTTYPE = '4'
02701          NEXT SENTENCE
02702      ELSE
02703          IF WS-SV-EPYTHRU = WS-EXP-DT
02704              IF PI-PMTTYPE NOT = '2' AND '3'
02705                  MOVE ER-3541    TO EMI-ERROR
02706                  MOVE -1         TO PMTTYPEL
02707                  MOVE AL-UABON   TO PMTTYPEA
02708                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02709
02710      IF WS-SV-EPYTHRU < WS-SV-EPYFROM
02711          MOVE ER-0501            TO EMI-ERROR
02712          GO TO 1170-PAID-THRU-FATAL-ERROR.
02713
02714      IF WS-SV-EPYTHRU > WS-TODAY-DATE
121802        IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                             OR 'B' or 'H'
121802*          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*              MOVE ER-0540         TO EMI-ERROR
121802*              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*              MOVE -1              TO EPYTHRUL
121802*              MOVE AL-UNBON        TO EPYTHRUA
121802*          ELSE
121802*          IF PI-COMPANY-ID = 'DMD'
121802*              MOVE ER-7540         TO EMI-ERROR
121802*              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*              MOVE -1              TO EPYTHRUL
121802*              MOVE AL-UNBON        TO EPYTHRUA
121802*          ELSE
02728                IF PI-PMTTYPE NOT = '2' AND '3'
02729                   IF PI-COMPANY-ID = '???'
02730                       MOVE ER-7540  TO EMI-ERROR
02731                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02732                       MOVE -1       TO EPYTHRUL
02733                       MOVE AL-UNBON TO EPYTHRUA
02734                   ELSE
02735                       MOVE ER-0540  TO EMI-ERROR
02736                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02737                       MOVE -1       TO EPYTHRUL
02738                       MOVE AL-UNBON TO EPYTHRUA.
02739
043019     MOVE 'N' TO PI-DUPE-APPROVAL-NEEDED
02740      IF ((WS-SV-EPYTHRU < CL-PAID-THRU-DT)
02741             OR
02742         (WS-SV-EPYTHRU NOT = LOW-VALUES AND
02743          WS-SV-EPYTHRU < CL-PAID-THRU-DT)
02744             OR
02745         (PI-CPYFROM NOT = LOW-VALUES AND
02746          PI-CPYFROM < CL-PAID-THRU-DT))
043019       AND NOT PI-PMTTYPE = '4'
02747         PERFORM 1166-CHECK-ALL-TRLRS THRU 1166-TRLR-EXIT
040819     ELSE
040819     IF WS-SV-EPYTHRU NOT = LOW-VALUES
040819       AND WS-SV-EPYFROM NOT = LOW-VALUES
040819        SET CHECK-ALL-PMTS TO TRUE
040819        PERFORM 1166-CHECK-ALL-TRLRS THRU 1166-TRLR-EXIT.
040819
040819     IF PI-HOLDTIL = ZEROS
040819       AND UNPAID-HOLD-UNTIL-EXISTS
040819       AND NOT DUPE-VOID-PMT-EXISTS
040819        MOVE ER-3276    TO EMI-ERROR
040819        MOVE -1         TO HOLDTILL
040819        MOVE AL-UABON   TO HOLDTILA
040819        GO TO 1170-PAID-THRU-FATAL-ERROR
040819     END-IF.
02748
02749  1152-CHECK-TYPE.
121802     IF (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                           OR 'B' or 'H')
02751         OR (PI-PMTTYPE = '5' OR '6')
02752          GO TO 1160-EDIT-PAID-THRU-AH.
02753
02754  1155-EDIT-PAID-THRU-LIFE.
02755      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-CD.
02756      MOVE WS-ACCESS              TO CNTL-ACCESS.
02757      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02758      MOVE '4'                    TO CNTL-REC-TYPE.
02759      MOVE ZEROS                  TO CNTL-SEQ-NO.
02760      MOVE 'BENE'                 TO FILE-SWITCH.
02761
02762      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
02763
02764      IF NO-BENEFIT-FOUND
02765          GO TO 1950-NOT-FOUND.
02766
02767      MOVE CF-SPECIAL-CALC-CD (SUB-1) TO WS-SPECIAL-CALC-CD.
02768
02769      MOVE WS-EXP-DT              TO PI-CPYTHRU.
02770
02771      IF PI-LIFE-OVERRIDE-L1 = 'P' OR
02772         PI-LF-COVERAGE-TYPE = 'P'
02773          MOVE CL-INCURRED-DT     TO  PI-CPYTHRU
02774          GO TO 1200-EDIT-FIND-TOLERANCES.
02775
02776      IF PI-EPYTHRU = ZEROS
02777         MOVE PI-CPYTHRU          TO WS-SV-EPYTHRU.
02778
02779      IF WS-SV-EPYTHRU NOT = WS-EXP-DT
02780          MOVE ER-0476            TO EMI-ERROR
02781          PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02782
02783      MOVE CM-CERT-EFF-DT        TO DC-BIN-DATE-1.
02784 *    MOVE CM-LF-CRITICAL-PERIOD TO DC-ELAPSED-MONTHS.
           move cl-critical-period    to dc-elapsed-months
02785      MOVE +0                    TO DC-ELAPSED-DAYS.
02786      MOVE '6'                   TO DC-OPTION-CODE.
02787      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02788
02789      IF (NO-CONVERSION-ERROR AND
02790          WS-SPECIAL-CALC-CD = 'C')
02791         IF CL-INCURRED-DT > DC-BIN-DATE-2
02792             MOVE ER-0657 TO EMI-ERROR
02793             PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02794
02795      IF PI-COMPANY-ID = 'NCL'
02796          IF WS-SPECIAL-CALC-CD = 'C' AND
                  cl-critical-period = zeros
02797 *           CM-LF-CRITICAL-PERIOD = +0
02798              MOVE ER-0844       TO EMI-ERROR
02799              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02800
02801      GO TO 1200-EDIT-FIND-TOLERANCES.
02802
02803  1160-EDIT-PAID-THRU-AH.
02804      IF PI-EPYTHRU = ZEROS
02805         MOVE WS-EXP-DT           TO PI-CPYTHRU
02806                                     WS-SV-EPYTHRU
02807      ELSE
02808         MOVE WS-SV-EPYTHRU       TO PI-CPYTHRU.
02809
02810      IF WS-SV-EPYTHRU < PI-CPYFROM
02811          MOVE ER-0508            TO EMI-ERROR
02812          GO TO 1170-PAID-THRU-FATAL-ERROR.
02813
           perform 7975-read-acct      thru 7975-exit
           move zeros                  to pi-max-ext
           if PI-DCC-PRODUCT-CODE NOT = SPACES
              PERFORM 0900-GET-DDF-LIMITS
                                       THRU 0900-EXIT
              IF PDEF-FOUND
                 PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122              (P1 > +11)
022122              OR ((PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
022122              AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
                 END-PERFORM
022122           IF P1 < +12
                    MOVE PD-MAX-extension (P1)
                                       TO pi-max-ext
                 else
                    move er-1674       to emi-error
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              else
                 move er-1672          to emi-error
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           end-if
           move spaces                 to pi-max-ext-used
02814      IF PI-PMTTYPE = '3'
02815         MOVE WS-EXP-DT           TO PI-CPYTHRU
02816         IF WS-SV-EPYTHRU NOT = WS-EXP-DT
02817            MOVE ER-0477          TO EMI-ERROR
02818            PERFORM 1165-PAID-THRU-ERROR
                                       THRU 1165-EXIT
02819         ELSE
02820            continue
              end-if
02821      ELSE
02822         IF WS-SV-EPYTHRU > WS-EXP-DT
081817         IF PI-DCC-PRODUCT-CODE NOT > SPACES
                 move ws-exp-dt        to dc-bin-date-1
                 move ws-sv-epythru    to dc-bin-date-2
                 MOVE '1'              TO DC-OPTION-CODE
                 MOVE +0               TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS
                 PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
                 if no-conversion-error
                    if dc-elapsed-days > zeros
                       add +1 to dc-elapsed-months
                    end-if
                    if dc-elapsed-months > pi-max-ext
                       MOVE ER-0557    TO EMI-ERROR
                       PERFORM 1165-PAID-THRU-ERROR
                                       THRU 1165-EXIT
                    else
                       move 'Y'        to pi-max-ext-used
                       move er-1670    to emi-error
                       perform 9900-error-format
                                       thru 9900-exit
                    end-if
                 end-if
081817         ELSE
081817            MOVE ER-0557    TO EMI-ERROR
081817            PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT
081817         END-IF
              end-if
           end-if
02825
02826      IF (WS-RETRO-ELIM-DATE > WS-SV-EPYTHRU) AND
02827          PI-PMTTYPE NOT = '3'
02828          MOVE ER-0541        TO EMI-ERROR
02829          PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02830
02831      IF (CM-AH-CANCEL-DT NOT = LOW-VALUES) AND
02832         (PI-PMTTYPE NOT = '3')
02833          IF WS-SV-EPYTHRU > CM-AH-CANCEL-DT
02834              MOVE ER-0478        TO EMI-ERROR
02835              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02836
02837      IF (CM-AH-SETTLEMENT-DT NOT = LOW-VALUES) AND
02838         (PI-PMTTYPE NOT = '4')
02839          IF WS-SV-EPYTHRU > CM-AH-SETTLEMENT-DT
02840              MOVE ER-0479        TO EMI-ERROR
02841              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02842
02843      IF (CL-EST-END-OF-DISAB-DT NOT = LOW-VALUES) AND
02844          WS-SV-EPYTHRU > CL-EST-END-OF-DISAB-DT
02845              MOVE ER-0545        TO EMI-ERROR
02846              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02847
02848      IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES
02849          IF WS-SV-EPYTHRU > CM-LF-DEATH-EXIT-DT
02850              MOVE ER-0481        TO EMI-ERROR
02851              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02852
121802*    IF PI-COMPANY-ID = 'NCL'
121802*        IF WS-SPECIAL-CALC-CD = 'C' AND
121802*           CM-AH-CRITICAL-PERIOD = +0
121802*            MOVE ER-0844        TO  EMI-ERROR
121802*            PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02858
02859 *    IF (CM-AH-CRITICAL-PERIOD = ZEROS) OR
           if (cl-critical-period = zeros)
02860         or (WS-SPECIAL-CALC-CD NOT = 'C')
02861           GO TO 1200-EDIT-FIND-TOLERANCES.
02862
02863      MOVE PI-CPYFROM TO DC-BIN-DATE-1.
02864      MOVE PI-CPYTHRU TO DC-BIN-DATE-2.
02865      MOVE '1'        TO DC-OPTION-CODE.
02866      MOVE +0         TO DC-ELAPSED-DAYS
02867                         DC-ELAPSED-MONTHS.
02868      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02869
092310*    IF NO-CONVERSION-ERROR
092310*       COMPUTE ws-benefits-paid =
092310*           (DC-ELAPSED-DAYS + CL-NO-OF-DAYS-PAID) / 30.5
092310*       IF ws-benefits-paid > CM-AH-CRITICAL-PERIOD
092310*           MOVE ER-0657 TO EMI-ERROR
092310*           PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.
02876
02877      GO TO 1200-EDIT-FIND-TOLERANCES.
02878
02879  1165-PAID-THRU-ERROR.
02880      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02881
02882      IF EMI-MESSAGE-FORMATTED
02883          MOVE -1                 TO EPYTHRUL
02884          MOVE AL-UNBON           TO EPYTHRUA.
02885
02886  1165-EXIT.
02887      EXIT.
02888
02889  1166-CHECK-ALL-TRLRS.
02890
02891      IF PI-PMTTYPE = '4'
043019       AND NOT HOLD-UNTIL-CHK
043019       AND NOT CHECK-ALL-PMTS
02892         GO TO 1166-TRLR-EXIT.
02893
02894      MOVE ' ' TO WS-BROWSE-TRLR-SW.
02895
022718     IF HOLD-UNTIL-CHK
022718        CONTINUE
022718     ELSE
02896      IF WS-SV-EPYTHRU = LOW-VALUES  AND
02897         WS-SV-EPYFROM = LOW-VALUES
02898           GO TO 1166-END-CHECK.
02899
02900      
      * EXEC CICS HANDLE CONDITION
02901 *        ENDFILE   (1166-END-CHECK)
02902 *        NOTFND    (1166-END-CHECK)
02903 *    END-EXEC.
      *    MOVE '"$''I                  ! ) #00011047' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303131303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02904
02905      MOVE ELMSTR-KEY             TO ELTRLR-KEY.
02906      MOVE +100                   TO TRLR-SEQ-NO.
02907
02908      
      * EXEC CICS STARTBR
02909 *         DATASET    ('ELTRLR')
02910 *         RIDFLD     (ELTRLR-KEY)
02911 *         GTEQ
02912 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011055' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131303535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02913
02914      MOVE 'Y' TO WS-BROWSE-TRLR-SW.
02915
02916  1166-READ-TRLR-NEXT.
02917
02918      
      * EXEC CICS READNEXT
02919 *         DATASET    ('ELTRLR')
02920 *         RIDFLD     (ELTRLR-KEY)
02921 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
02922 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00011065' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303131303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02923
02924      IF TRLR-COMP-CD  = MSTR-COMP-CD  AND
02925         TRLR-CARRIER  = MSTR-CARRIER  AND
02926         TRLR-CLAIM-NO = MSTR-CLAIM-NO AND
02927         TRLR-CERT-NO  = MSTR-CERT-NO
02928           NEXT SENTENCE
02929       ELSE
02930           GO TO 1166-END-CHECK.
02931
02932 * **  BYPASS ALL NON-PAYMENT TRAILERS
02933
02934      IF AT-TRAILER-TYPE = '2'
02935         NEXT SENTENCE
02936      ELSE
02937         GO TO 1166-READ-TRLR-NEXT.
02938
02939 * **  BYPASS ALL NON-PARTIAL PAYMENTS
02940
043019     IF (HOLD-UNTIL-CHK
043019       OR CHECK-ALL-PMTS)
043019       AND AT-PAYMENT-TYPE <= '4'
043019        NEXT SENTENCE
043019     ELSE
02941      IF AT-PAYMENT-TYPE = '1' OR '2'
02942         NEXT SENTENCE
02943      ELSE
02944         GO TO 1166-READ-TRLR-NEXT.
02945
02951      IF ((AT-PAID-FROM-DT = LOW-VALUES OR SPACES) OR
02952         (AT-PAID-THRU-DT = LOW-VALUES OR SPACES))
043019        AND NOT HOLD-UNTIL-CHK
02953           GO TO 1166-READ-TRLR-NEXT.
02946 * **  BYPASS ALL VOID PAYMENTS
02947
02948      IF AT-VOID-DT NOT = LOW-VALUES
040819        IF WS-SV-EPYTHRU = AT-PAID-THRU-DT
040819          AND WS-SV-EPYFROM = AT-PAID-FROM-DT
040819            SET DUPE-VOID-PMT-EXISTS TO TRUE
040819        END-IF
02949         GO TO 1166-READ-TRLR-NEXT.
02950
022718     IF HOLD-UNTIL-CHK
022718        IF AT-PAID-THRU-DT > WS-HOLD-UNTIL-DT
022718          AND AT-TO-BE-WRITTEN-DT > ZERO
022718           MOVE AT-PAID-THRU-DT TO WS-HOLD-UNTIL-DT
022718        END-IF
040819        IF AT-TO-BE-WRITTEN-DT > ZERO
040819          AND AT-CHECK-WRITTEN-DT = LOW-VALUES
040819           SET UNPAID-HOLD-UNTIL-EXISTS TO TRUE
040819           IF AT-TO-BE-WRITTEN-DT > WS-UNPD-HOLD-UNTIL-DT
040819              MOVE AT-TO-BE-WRITTEN-DT TO WS-UNPD-HOLD-UNTIL-DT
040819           END-IF
040819        END-IF
040819        IF AT-AMOUNT-PAID NUMERIC
040819          AND (AT-CHECK-WRITTEN-DT > LOW-VALUES
040819             OR AT-TO-BE-WRITTEN-DT < ZERO)
040819          AND AT-PAYMENT-TYPE  <= '4'
040819           ADD AT-AMOUNT-PAID TO WS-TOT-AMOUNT-PAID
022718        END-IF
022718     END-IF
02954
022718     IF HOLD-UNTIL-CHK
022718        CONTINUE
022718     ELSE
02955      IF ((WS-SV-EPYTHRU < AT-PAID-FROM-DT) AND
02956          (WS-SV-EPYFROM < AT-PAID-FROM-DT))
02957             OR
02958         ((WS-SV-EPYTHRU > AT-PAID-THRU-DT) AND
02959          (WS-SV-EPYFROM > AT-PAID-THRU-DT))
02960         NEXT SENTENCE
02961      ELSE
040819         IF WS-SV-EPYTHRU = AT-PAID-THRU-DT
040819          AND WS-SV-EPYFROM = AT-PAID-FROM-DT
043019          AND (PI-OFFLINE NOT EQUAL 'Y')
043019          AND (CL-CLAIM-TYPE NOT = 'L' AND 'O')
043019            IF PI-PMTTYPE = '4'
043019               MOVE 'Y' TO PI-DUPE-APPROVAL-NEEDED
043019               MOVE ER-3280            TO EMI-ERROR
043019               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
043019               MOVE -1                 TO EPYTHRUL
043019               MOVE AL-UNBON           TO EPYTHRUA
043019            ELSE
040819               MOVE ER-3279            TO EMI-ERROR
040819               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040819               MOVE -1                 TO EPYTHRUL
040819               MOVE AL-UNBON           TO EPYTHRUA
040819         ELSE
02962          IF CL-TOTAL-PAID-AMT > ZEROS
040819           AND NOT CHECK-ALL-PMTS
02963              MOVE ER-0502            TO EMI-ERROR
02964              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02965              MOVE -1                 TO EPYTHRUL
02966              MOVE AL-UNBON           TO EPYTHRUA.
02967
02968      GO TO 1166-READ-TRLR-NEXT.
02969
02970  1166-END-CHECK.
02971
02972      IF WS-BROWSE-TRLR-SW = 'Y'
02973         
      * EXEC CICS ENDBR
02974 *            DATASET   ('ELTRLR')
02975 *       END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011169' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02976
02977      MOVE 'Y' TO WS-BROWSE-TRLR-SW.
02978
02979  1166-TRLR-EXIT.
02980      EXIT.
02981
02982      EJECT
02983  1170-PAID-THRU-FATAL-ERROR.
02984      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02985
02986      MOVE -1                     TO EPYTHRUL.
02987      MOVE AL-UNBON               TO EPYTHRUA.
02988
02989  1200-EDIT-FIND-TOLERANCES.
02990      
      * EXEC CICS HANDLE CONDITION
02991 *        ENDFILE(1950-NOT-FOUND)
02992 *        NOTFND (1950-NOT-FOUND)
02993 *    END-EXEC.
      *    MOVE '"$''I                  ! * #00011186' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303131313836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02994
02995      IF PI-PROCESSOR-ID = 'LGXX'
02996         GO TO 1240-FIND-ACCT-TOLERANCES.
02997
02998      MOVE 'N'                    TO FOUND-TOL-SW.
02999      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
03000      MOVE '2'                    TO CNTL-REC-TYPE.
03001      MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS.
03002      MOVE +0                     TO CNTL-SEQ-NO.
03003      MOVE 'PROC'                 TO FILE-SWITCH.
03004
03005      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.
03006
03007      MOVE CF-PROC-CALC-AMT-TOL   TO WS-PMT-TOL.
03008      MOVE CF-PROC-MAX-REG-PMT    TO WS-MAX-PMT-TOL
03009                                     PI-MAX-PMT-TOL.
03010      MOVE CF-PROC-MAX-REG-DAYS   TO WS-MAX-DAYS-TOL.
03011
03012      IF PI-COMPANY-ID = 'HER' OR 'KSM'
03013          MOVE CF-PROC-MAX-REG-PMT   TO  WS-HER-MO-PMT-AMT.
03014
03015      MOVE CF-PROC-CALC-DAYS-TOL  TO WS-DAYS-TOL.
03016      MOVE CF-PROC-MAX-LF-PMT     TO WS-MAX-LF-PMT-TOL
03017                                     PI-MAX-LF-PMT-TOL.
03018
03019      IF CF-PROC-MAX-EXP-PMT NOT NUMERIC
03020          MOVE ZEROS              TO CF-PROC-MAX-EXP-PMT.
03021
03022      MOVE CF-PROC-MAX-EXP-PMT    TO WS-MAX-EXP-PMT
03023                                     PI-MAX-EXP-PMT.
03024
03025  1240-FIND-ACCT-TOLERANCES.
03026
03027      IF WS-PMT-TOL NOT = +0
03028          MOVE 'Y'                TO FOUND-TOL-SW.
03029
03030      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
03031          MOVE PI-COMPANY-CD          TO ACCT-COMP-CD
03032          MOVE CL-CURRENT-CARRIER     TO ACCT-CARRIER
03033          MOVE CL-CURRENT-GROUPING    TO ACCT-GROUPING
03034          MOVE CL-CURRENT-STATE       TO ACCT-STATE
03035          MOVE CL-CURRENT-ACCOUNT     TO ACCT-ACCOUNT
03036          MOVE CL-CERT-EFF-DT         TO ACCT-EXP-DT
03037      ELSE
03038          MOVE PI-COMPANY-CD          TO ACCT-COMP-CD
03039          MOVE PI-CARRIER             TO ACCT-CARRIER
03040          MOVE PI-GROUPING            TO ACCT-GROUPING
03041          MOVE PI-STATE               TO ACCT-STATE
03042          MOVE PI-ACCOUNT             TO ACCT-ACCOUNT
03043          MOVE PI-CERT-EFF-DT         TO ACCT-EXP-DT.
03044
03045      MOVE 'ACCT'                 TO FILE-SWITCH.
03046
03047      MOVE ERACCT-PARTIAL-KEY     TO WS-ERACCT-SAVE-KEY.
03048      MOVE SPACES                 TO WS-ERACCT-HOLD-RECORD.
03049
03050      
      * EXEC CICS HANDLE CONDITION
03051 *         NOTFND   (1950-NOT-FOUND)
03052 *         ENDFILE  (1950-NOT-FOUND)
03053 *    END-EXEC.
      *    MOVE '"$I''                  ! + #00011246' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303131323436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03054
090821     move spaces to ws-eracct-startbr-ind
03055      
      * EXEC CICS STARTBR
03056 *        DATASET('ERACCT')
03057 *        RIDFLD (ERACCT-KEY)
03058 *        GTEQ
03059 *    END-EXEC.
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011252' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
090821     set eracct-browse-started to true
           .
03061  1245-FIND-ACCT-TOLERANCES-LOOP.
03062
03063      
      * EXEC CICS READNEXT
03064 *        DATASET('ERACCT')
03065 *        SET    (ADDRESS OF ACCOUNT-MASTER)
03066 *        RIDFLD (ERACCT-KEY)
03067 *    END-EXEC.
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00011261' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
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
           
03068
03069      IF WS-ERACCT-SAVE-KEY NOT = ERACCT-PARTIAL-KEY
03070         IF WS-ERACCT-HOLD-RECORD = SPACES
03071            GO TO 1950-NOT-FOUND
03072         ELSE
03073            MOVE WS-ERACCT-HOLD-RECORD TO ACCOUNT-MASTER
03074            GO TO 1250-CONTINUE.
03075
03076      IF ACCT-EXP-DT = HIGH-VALUES
03077         NEXT SENTENCE
03078      ELSE
03079         MOVE ACCOUNT-MASTER TO WS-ERACCT-HOLD-RECORD
03080         GO TO 1245-FIND-ACCT-TOLERANCES-LOOP.
03081
03082  1250-CONTINUE.
090821     if eracct-browse-started
090821        
      * exec cics endbr
090821*          dataset('ERACCT')
090821*       end-exec
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011282' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821        move spaces to ws-eracct-startbr-ind
090821     end-if
03084      MOVE AM-CONTROL-PRIMARY     TO PI-ACCT-KEY.
03085
03086      IF FOUND-TOL-SW = 'N'
03087          MOVE AM-TOL-CLM         TO WS-PMT-TOL.
03088
03089 ********* 1260-FIND-STATE-TOLERANCES.
03090
03091      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
03092      MOVE PI-STATE               TO WS-ST-ACCESS.
03093      MOVE WS-STATE-ACCESS        TO CNTL-ACCESS.
03094      MOVE '3'                    TO CNTL-REC-TYPE.
03095      MOVE +0                     TO CNTL-SEQ-NO.
03096      MOVE 'STAT'                 TO FILE-SWITCH.
03097
03098      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.
03099
03100      IF WS-PMT-TOL = +0
03101          MOVE CF-ST-TOL-CLAIM    TO WS-PMT-TOL.
03102
03103      MOVE CF-ST-CLAIM-REJECT-SW  TO WS-TOL-SEVERITY.
03104      MOVE CF-ST-SPLIT-PAYMENT    TO PI-SPLIT-PMT-SW.
03105      MOVE CF-STATE-ABBREVIATION  TO WS-STATE-ABBREV.
03106      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
061511     MOVE CF-ST-VFY-2ND-BENE     TO PI-VFY-2ND-BENE.
061511
032514****FOR ANY PAYMENT NOT MADE TO BENEFICIARY, CHECK FOR VERIFY IND
032514****AND FOR LIFE PAYMENTS CHECK PAYEE NAME NOT EQUAL TO DECEASED
032514     IF PI-PAYEE-TYPE <> 'B'
061511           PERFORM 3600-CHECK-VERIFICATION THRU 3600-EXIT
061511     END-IF.
03107
03108      IF PI-PMTTYPE      = '2'  AND
03109         PI-SPLIT-PMT-SW = 'Y'  AND
100518        (CL-CLAIM-TYPE   = PI-LIFE-OVERRIDE-L1 OR 'O')
03111           MOVE ER-3530                TO EMI-ERROR
03112           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03113
03114 ********* 1280-FIND-BENEFIT-TOLERANCES.
03115
121802     IF (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122         OR 'B' OR 'H')
03118         OR (PI-PMTTYPE = '5' OR '6')
03119          GO TO 1300-FIND-CARR-TOLERANCES.
03120
03121      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
03122      MOVE '4'                    TO CNTL-REC-TYPE.
03123      MOVE +0                     TO CNTL-SEQ-NO.
03124      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-CD.
03125      MOVE WS-ACCESS              TO CNTL-ACCESS.
03126      MOVE 'BENE'                 TO FILE-SWITCH.
03127
03128      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
03129
03130      IF NO-BENEFIT-FOUND
03131          GO TO 1950-NOT-FOUND.
03132
03133      MOVE CF-CO-EARNINGS-CALC (SUB-1)    TO  WS-EARNING-METHOD.
03134      MOVE CF-SPECIAL-CALC-CD  (SUB-1)    TO  WS-SPECIAL-CALC-CD.
03135      MOVE CF-LF-COVERAGE-TYPE (SUB-1)    TO  WS-LF-COVERAGE-TYPE.
03136
03137      
      * EXEC CICS HANDLE CONDITION
03138 *        NOTFND(1950-NOT-FOUND)
03139 *    END-EXEC.
      *    MOVE '"$I                   ! , #00011347' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303131333437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03140
03141  1300-FIND-CARR-TOLERANCES.
03142      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
03143      MOVE '6'                    TO CNTL-REC-TYPE.
03144
03145      IF CONTROL-IS-ACTUAL-CARRIER
03146          MOVE PI-CARRIER               TO WS-CARR
03147      ELSE
03148          MOVE PI-CARRIER-CONTROL-LEVEL TO WS-CARR.
03149
03150      MOVE WS-CARR-ACCESS         TO CNTL-ACCESS.
03151      MOVE +0                     TO CNTL-SEQ-NO.
03152      MOVE 'CARR'                 TO FILE-SWITCH.
03153
03154      PERFORM 7930-READ-CONTROL   THRU 7930-EXIT.
03155
03156      IF WS-DAYS-TOL = +0
03157          MOVE CF-CALC-DAYS-TOL   TO WS-DAYS-TOL.
03158
03159      IF WS-PMT-TOL = +0
03160          MOVE CF-CALC-AMT-TOL    TO WS-PMT-TOL.
03161
03162      IF WS-MAX-DAYS-TOL = +0
03163          MOVE CF-MAX-REG-DAYS    TO WS-MAX-DAYS-TOL.
03164
03165      IF WS-MAX-PMT-TOL = +0
03166          MOVE CF-MAX-REG-PMT     TO WS-MAX-PMT-TOL
03167                                     PI-MAX-PMT-TOL.
03168
03169      IF WS-MAX-LF-PMT-TOL = +0
03170         MOVE CF-MAX-REG-PMT      TO WS-MAX-LF-PMT-TOL
03171                                     PI-MAX-LF-PMT-TOL.
03172
03173      MOVE CF-CLAIM-CALC-METHOD   TO WS-CALC-METHOD.
03174
03175 ********* 1320-FIND-COMP-TOLERANCES.
03176
03177      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
03178      MOVE '1'                    TO CNTL-REC-TYPE.
03179      MOVE SPACES                 TO CNTL-ACCESS.
03180      MOVE +0                     TO CNTL-SEQ-NO.
03181      MOVE 'COMP'                 TO FILE-SWITCH.
03182
03183      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.
03184
03185      MOVE CF-CURRENT-MONTH-END   TO PI-MONTH-END-SAVE.
03186      MOVE CF-PAYMENT-APPROVAL-SW TO PI-PMT-APPR-SW.
03187
03188      IF WS-PMT-TOL NOT = +0
03189         IF WS-TOL-SEVERITY NOT = ' '
03190          GO TO 1360-EDIT-DAYS.
03191
03192      IF WS-PMT-TOL = +0
03193          MOVE CF-CO-TOL-CLAIM    TO WS-PMT-TOL.
03194
03195      IF WS-TOL-SEVERITY = ' '
03196          MOVE CF-CO-CLAIM-REJECT-SW TO WS-TOL-SEVERITY.
03197
03198  1360-EDIT-DAYS.
03199      IF PI-PMTTYPE NOT = '4'
03200         IF PI-EDAYS NEGATIVE OR
03201            PI-EPYAMT NEGATIVE
03202           MOVE ER-0513             TO EMI-ERROR
03203           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03204           MOVE -1                  TO PMTTYPEL.
03205
03206      IF PI-PMTTYPE = '5' OR '6'
03207         GO TO 1380-EDIT-EXPENSE-PMT-AMT.
03208
03209      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
03210         IF PI-PMTTYPE = '4'
03211           MOVE PI-EDAYS          TO PI-CDAYS.
03212
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
03214          GO TO 1440-COMPUTE-LIFE-REMAINING.
03215
03216      IF PI-PMTTYPE = '4'
03217         MOVE PI-EDAYS            TO PI-CDAYS
03218         GO TO 1400-EDIT-PAYMENT.
03219
03220      IF LOW-VALUES = PI-CPYFROM OR PI-CPYTHRU
03221         GO TO 1500-EDIT-EXPENSES.
03222
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*       IF CL-CLAIM-TYPE    = PI-AH-OVERRIDE-L1 AND
121802*          CM-AH-BENEFIT-CD = '23'              AND
121802*          CL-CERT-STATE    = '04'              AND
121802*         (PI-PMTTYPE = '1' OR '2' OR '3')
121802*            PERFORM 3200-CALC-AIG-DAYS THRU 3299-EXIT
121802*            GO TO 1362-CHECK-MAX-DAYS.
03230
03231      IF PI-EDAYS = ZEROS
121802*        IF PI-COMPANY-ID = 'DMD'
121802*            MOVE WS-SV-EPYFROM  TO DC-BIN-DATE-1
121802*            MOVE -1             TO DC-ELAPSED-DAYS
121802*            MOVE ZEROS          TO DC-ELAPSED-MONTHS
121802*            MOVE '6'            TO DC-OPTION-CODE
121802*            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
121802*            MOVE DC-BIN-DATE-2  TO DC-BIN-DATE-1
121802*            MOVE WS-SV-EPYTHRU  TO DC-BIN-DATE-2
121802*            MOVE '1'            TO DC-OPTION-CODE
121802*            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
121802*            IF NO-CONVERSION-ERROR
121802*                COMPUTE PI-EDAYS = DC-ELAPSED-MONTHS * 30
121802*                                 + DC-ODD-DAYS-OVER
121802*               MOVE 6           TO EDAYSL
121802*               MOVE AL-UNNON    TO EDAYSA
121802*            ELSE
121802*                NEXT SENTENCE
121802*       ELSE
03250             MOVE WS-SV-EPYFROM   TO DC-BIN-DATE-1
03251             MOVE WS-SV-EPYTHRU   TO DC-BIN-DATE-2
03252             MOVE '1'             TO DC-OPTION-CODE
03253             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03254             IF NO-CONVERSION-ERROR
03255                 MOVE DC-ELAPSED-DAYS  TO PI-EDAYS
03256                 ADD 1                 TO PI-EDAYS
03257                 MOVE AL-UNNON         TO EDAYSA.
03258
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        MOVE PI-CPYFROM         TO DC-BIN-DATE-1
121802*        MOVE -1                 TO DC-ELAPSED-DAYS
121802*        MOVE ZEROS              TO DC-ELAPSED-MONTHS
121802*        MOVE '6'                TO DC-OPTION-CODE
121802*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
121802*        MOVE DC-BIN-DATE-2      TO DC-BIN-DATE-1
121802*        MOVE PI-CPYTHRU         TO DC-BIN-DATE-2
121802*        MOVE '1'                TO DC-OPTION-CODE
121802*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
121802*    ELSE
03270          MOVE PI-CPYFROM         TO DC-BIN-DATE-1.
03271          MOVE PI-CPYTHRU         TO DC-BIN-DATE-2.
03272          MOVE '1'                TO DC-OPTION-CODE.
03273          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03274
03275      IF NO-CONVERSION-ERROR
03276         IF PI-COMPANY-ID = 'DMD'
03277             COMPUTE PI-CDAYS = DC-ELAPSED-MONTHS * 30
03278                              + DC-ODD-DAYS-OVER
03279         ELSE
03280             MOVE DC-ELAPSED-DAYS TO PI-CDAYS
03281             ADD 1                TO PI-CDAYS.
03282
03283      IF DC-ODD-DAYS-OVER NOT = ZEROS
03284         MOVE DC-ELAPSED-MONTHS   TO EXPENSE-MONTH-SAVE
03285         ADD 1                    TO EXPENSE-MONTH-SAVE.
03286
03287      IF PI-EDAYS NOT NUMERIC
03288          MOVE ER-0491            TO EMI-ERROR
03289          GO TO 1370-DAYS-ERROR.
03290
112210     IF CL-NO-OF-PMTS-MADE = 0
030512        AND (PI-COMPANY-ID = 'CID' OR 'AHL')
112210        AND CL-CERT-STATE = 'MI'
112210        AND (CM-AH-BENEFIT-CD = '20' OR '21' OR '22' OR '23')
112210            PERFORM 1390-CHECK-PAYMENT-DAYS THRU 1390-EXIT
112210     END-IF.
112210
03291  1362-CHECK-MAX-DAYS.
03292
03293      IF WS-MAX-DAYS-TOL = +999
03294          GO TO 1365-SKIP-MAX-DAYS-EDIT.
03295
03296      IF PI-COMPANY-ID = 'HER' OR 'KSM'
03297          IF PI-EDAYS = ZEROS
03298              COMPUTE WS-PMT-MOS ROUNDED = (PI-CDAYS / +30) + .005
03299          ELSE
03300              COMPUTE WS-PMT-MOS ROUNDED = (PI-EDAYS / +30) + .005.
03301
03302      IF WS-MAX-DAYS-TOL NOT = ZERO
03303         IF PI-EDAYS > WS-MAX-DAYS-TOL OR
03304            PI-CDAYS > WS-MAX-DAYS-TOL
03305              MOVE ER-0492          TO EMI-ERROR
03306              GO TO 1370-DAYS-ERROR.
03307
03308  1365-SKIP-MAX-DAYS-EDIT.
03309      IF PI-EDAYS = ZEROS
03310         GO TO 1400-EDIT-PAYMENT.
03311
03312      IF WS-DAYS-TOL NOT = ZERO
03313         IF ((PI-CDAYS - WS-DAYS-TOL) > PI-EDAYS) OR
03314            ((PI-CDAYS + WS-DAYS-TOL) < PI-EDAYS)
03315                 MOVE ER-0493     TO EMI-ERROR
03316                 GO TO 1370-DAYS-ERROR.
03317
03318      GO TO 1400-EDIT-PAYMENT.
03319
03320  1370-DAYS-ERROR.
03321      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03322
03323      IF EMI-MESSAGE-FORMATTED
03324          MOVE -1                 TO EDAYSL
03325          MOVE AL-UABON           TO EDAYSA.
03326
03327      GO TO 1400-EDIT-PAYMENT.
03328
03329  1380-EDIT-EXPENSE-PMT-AMT.
03330      IF PI-PMTTYPE = '5' OR '6'
03331          NEXT SENTENCE
03332      ELSE
03333          GO TO 1400-EDIT-PAYMENT.
03334
03335      IF WS-MAX-EXP-PMT NOT = ZEROS
03336         IF PI-EPYAMT > WS-MAX-EXP-PMT
03337             MOVE ER-3529            TO EMI-ERROR
03338             PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT.
03339
03340      GO TO 1500-EDIT-EXPENSES.
03341
03342      EJECT
112210
112210 1390-CHECK-PAYMENT-DAYS.
112210
112210     IF PI-EDAYS = 0
112210         MOVE PI-CDAYS TO WS-PD-DAYS
112210     ELSE
112210         MOVE PI-EDAYS TO WS-PD-DAYS
112210     END-IF.
112210
112210     IF CM-AH-BENEFIT-CD = '20' OR '21'
112210         IF WS-PD-DAYS = 14
112210            GO TO 1390-EXIT
112210         END-IF
112210     END-IF.
030512
030512     IF PI-COMPANY-ID = 'AHL'  AND
030512        CM-AH-BENEFIT-CD = '1K'
030512         IF WS-PD-DAYS = 14
030512            GO TO 1390-EXIT
030512         END-IF
030512     END-IF.
112210
112210     IF WS-PD-DAYS < 30
112210         MOVE ER-3549              TO EMI-ERROR
112210         PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
112210     END-IF.
112210
112210 1390-EXIT.
112210      EXIT.
112210
03343  1400-EDIT-PAYMENT.
121802*    IF PI-COMPANY-ID = 'DMD'
121802*       IF CL-CERT-NO (5:2) = '69' OR '70'
121802*           PERFORM 6500-READ-DLO035 THRU 6500-EXIT.
03347
120115     IF PI-COMPANY-ID = 'DCC' or 'VPP'
061013        MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
061013        MOVE PI-CARRIER          TO ACCT-CARRIER
061013        MOVE PI-GROUPING         TO ACCT-GROUPING
061013        MOVE PI-STATE            TO ACCT-STATE
061013        MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
061013        MOVE PI-CERT-EFF-DT      TO ACCT-EXP-DT
061013        MOVE ERACCT-PARTIAL-KEY  TO WS-ERACCT-SAVE-KEY
061013        MOVE SPACES              TO WS-ERACCT-HOLD-RECORD
061013
061013        
      * EXEC CICS READ
061013*          DATASET ('ERACCT')
061013*          RIDFLD  (ERACCT-KEY)
061013*          SET     (ADDRESS OF ACCOUNT-MASTER)
061013*          GTEQ
061013*          resp    (WS-RESPONSE)
061013*       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00011605' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303131363035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013        IF WS-RESP-NORMAL
061013           AND WS-ERACCT-SAVE-KEY = AM-CONTROL-PRIMARY (1:20)
061013           and pi-cert-eff-dt < am-expiration-dt
061013           and pi-cert-eff-dt >= am-effective-dt
061013           move am-dcc-product-code to pi-dcc-product-code
061013        else
061013           move spaces           to pi-dcc-product-code
061013        end-if
061013        if PI-DCC-PRODUCT-CODE NOT = SPACES
061013           PERFORM 0900-GET-DDF-LIMITS
061013                                 THRU 0900-EXIT
061013           IF PDEF-FOUND
061013              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122                 (P1 > 11)
022122                 OR ((PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
022122                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
061013              END-PERFORM
022122              IF P1 < +12
100314                 if pd-ben-pct (p1) not numeric
100314                    move zeros   to pd-ben-pct (p1)
100314                 end-if
100314                 if pd-ben-pct (p1) = zeros
100314                    move +1      to ws-work-ben-pct
100314                 else
100314                    move pd-ben-pct (p1)
100314                                 to ws-work-ben-pct
100314                 end-if
100314                 compute cm-ah-benefit-amt =
100314                    cm-ah-benefit-amt * ws-work-ben-pct
100314                 if cm-ah-benefit-amt > pd-max-amt (p1)
100314                    move pd-max-amt (p1) to cm-ah-benefit-amt
100314                 end-if
                    else
                       move er-1674    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013              END-IF
                 else
                    move er-1672       to emi-error
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013           END-IF
061013        end-if
061013     END-IF
03348      IF PI-PMTTYPE NOT = '4'
03349         GO TO 1405-CALC-PAYMENT.
03351      IF PI-EDAYS NOT NUMERIC
03352          MOVE PI-EPYAMT           TO PI-CPYAMT
03353          MOVE SPACES              TO PI-SPECIAL-BEN-SW
03354          GO TO 1430-CHECK-AH-TOLERANCE.
03355
121802*    IF PI-COMPANY-ID = 'FIA'
121802*       IF CERT-ACCOUNT = '0000011043'
121802*        COMPUTE PI-DAILY-RATE ROUNDED =
121802*                      (CM-AH-BENEFIT-AMT * 13) / 365
121802*        COMPUTE PI-CPYAMT ROUNDED =
121802*                          PI-DAILY-RATE * PI-EDAYS
121802*        MOVE SPACES TO PI-SPECIAL-BEN-SW
121802*        GO TO 1430-CHECK-AH-TOLERANCE.
03364
03365      IF WS-CALC-METHOD = '1' OR '4' OR '6'
03366          COMPUTE PI-DAILY-RATE ROUNDED = CM-AH-BENEFIT-AMT / 30
03367          COMPUTE PI-CPYAMT     ROUNDED = PI-DAILY-RATE * PI-EDAYS
03368          GO TO 1420-CHECK-AIG-BENEFIT.
03369
03370      IF WS-CALC-METHOD = '2' OR  '5'
03371          COMPUTE PI-DAILY-RATE ROUNDED =
03372                         (CM-AH-BENEFIT-AMT * 12) / 365
03373          COMPUTE PI-CPYAMT ROUNDED =
03374                            PI-DAILY-RATE * PI-EDAYS
03375         GO TO 1420-CHECK-AIG-BENEFIT.
03376
03377      IF WS-CALC-METHOD = '3'
03378          COMPUTE PI-DAILY-RATE =
03379                          (CM-AH-BENEFIT-AMT * 12) / 365
03380          COMPUTE PI-CPYAMT ROUNDED =
03381                            PI-DAILY-RATE * PI-EDAYS
03382         GO TO 1420-CHECK-AIG-BENEFIT.
03383      EJECT
03384 *    SAMPLE DATE RANGES AND DAYS PAID FOR OPTION 1 AND 6
03385
03386 ****************************************************************
03387 ****************************************************************
03388 *                  CALCULATED FROM      PAYMENT      PAYMENT
03389 *   DATES           DATE ROUTINE        OPT  1       OPT  6
03390 * FROM    THRU    MONTHS ODD ELAPSED   DAYS-PAID    DAYS-PAID
03391 *1-01-83  1-15-83    0    15     15        15           15
03392 *1-01-83  1-30-83    0    30     30        30           30
03393 *1-01-83  1-31-83    1     0     31        30           30
03394 *1-01-83  2-01-83    1     1     32        31           31
03395 *1-01-83  2-15-83    1    15     46        45           45
03396 *1-01-83  2-28-83    2     0     59        60           60
03397 *1-01-83  3-01-83    2     1     60        61           61
03398 *1-01-83  3-31-83    3     0     90        90           90
03399 *1-15-83  1-31-83    0    17     17        17           16
03400 *1-15-83  2-01-83    0    18     18        18           17
03401 *1-15-83  2-28-83    1    14     45        44           46
03402 *1-21-83  3-09-83    1    20     48        50           49
03403 *2-05-83  3-06-83    1     2     30        32           32
03404 *2-05-83  3-23-83    1    19     47        49           49
03405 *2-15-83  2-28-83    0    14     14        14           16
03406 *2-15-83  3-01-83    0    15     15        15           17
03407 *2-21-83  3-19-83    0    27     27        27           29
03408 *2-21-83  3-20-83    1     0     28        30           30
03409 *2-21-83  4-10-83    1    18     49        30           50
03410 *2-21-83  5-19-83    2    27     88        87           89
03411 *2-21-83  5-22-83    3     2     91        92           92
03412 *3-21-83  5-19-83    1    30     60        60           59
03413 ****************************************************************
03414      EJECT
03415  1405-CALC-PAYMENT.
03416      MOVE PI-CPYFROM             TO DC-BIN-DATE-1.
03417      MOVE ' '                    TO DC-OPTION-CODE.
03418      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03419      MOVE DC-DAYS-IN-MONTH       TO FROM-DAYS-IN-MONTH.
03420      MOVE DC-GREG-DATE-1-MDY     TO WORK-DATE-FROM.
03421
03422      MOVE PI-CPYTHRU             TO DC-BIN-DATE-1.
03423      MOVE ' '                    TO DC-OPTION-CODE.
03424      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03425      MOVE DC-GREG-DATE-1-MDY     TO WORK-DATE-THRU.
03426      MOVE DC-DAYS-IN-MONTH       TO THRU-DAYS-IN-MONTH.
03427
03428      MOVE PI-CPYFROM             TO DC-BIN-DATE-1.
03429      MOVE -1                     TO DC-ELAPSED-DAYS.
03430      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.
03431      MOVE '6'                    TO DC-OPTION-CODE.
03432      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03433      MOVE DC-DAYS-IN-MONTH       TO SAVE1-FROM-DAYS-IN-MONTH.
03434
03435      MOVE DC-BIN-DATE-2          TO DC-BIN-DATE-1.
03436      MOVE PI-CPYTHRU             TO DC-BIN-DATE-2.
03437      MOVE '1'                    TO DC-OPTION-CODE.
03438      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03439
03440      MOVE DC-ELAPSED-MONTHS      TO SAVE-ELAPSED-MONTHS.
03441      MOVE DC-ODD-DAYS-OVER       TO SAVE-ODD-DAYS-OVER.
03442      MOVE DC-ELAPSED-DAYS        TO SAVE-ELAPSED-DAYS.
03443
03444 ******************************************************************
03445 *   THE FOLLOWING STATEMENT ADJUSTS THE ODD DAYS OVER BY THE     *
03446 *     DIFFERENCE BETWEEN DAYS IN MONTH (FROM MONTH) AND          *
03447 *     DAYS IN MONTH (ONE MONTH PRIOR TO THRU MONTH)              *
03448 ******************************************************************
03449
03450      IF WS-CALC-METHOD = '1'           AND
03451         DC-ELAPSED-MONTHS > ZERO AND
03452         DC-ODD-DAYS-OVER NOT = ZEROS   AND
03453         WDF-DAY > WDT-DAY
03454           MOVE PI-CPYTHRU          TO DC-BIN-DATE-1
03455           MOVE ZEROS               TO DC-ELAPSED-DAYS
03456           MOVE -1                  TO DC-ELAPSED-MONTHS
03457           MOVE '6'                 TO DC-OPTION-CODE
03458           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03459           MOVE DC-DAYS-IN-MONTH    TO SAVE1-THRU-DAYS-IN-MONTH
03460           COMPUTE WS-SAVE-ODD-DAYS =
03461           (SAVE1-THRU-DAYS-IN-MONTH - SAVE1-FROM-DAYS-IN-MONTH)
03462           ADD WS-SAVE-ODD-DAYS     TO SAVE-ODD-DAYS-OVER.
03463
03464      MOVE SAVE-ELAPSED-MONTHS    TO DC-ELAPSED-MONTHS.
03465      MOVE SAVE-ODD-DAYS-OVER     TO DC-ODD-DAYS-OVER
03466      MOVE SAVE-ELAPSED-DAYS      TO DC-ELAPSED-DAYS.
031808
031808     MOVE SAVE-ELAPSED-MONTHS TO PI-SAVE-ELAPSED-MONTHS.
031808     MOVE SAVE-ODD-DAYS-OVER  TO PI-SAVE-ODD-DAYS-OVER.
03467
121802*    IF PI-COMPANY-ID = 'FIA'
121802*       IF CERT-ACCOUNT = '0000011043'
121802*        COMPUTE PI-DAILY-RATE ROUNDED =
121802*                          (CM-AH-BENEFIT-AMT * 13) / 365
121802*        COMPUTE PI-CPYAMT ROUNDED =
121802*                          PI-DAILY-RATE * DC-ELAPSED-DAYS
121802*        GO TO 1420-CHECK-AIG-BENEFIT.
03475
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*       IF CL-CLAIM-TYPE    = PI-AH-OVERRIDE-L1  AND
121802*          CM-AH-BENEFIT-CD = '23'               AND
121802*          CL-CERT-STATE    = '04'               AND
121802*         (PI-PMTTYPE = '1' OR '2' OR '3')
121802*            COMPUTE WS-DAILY-RATE ROUNDED =
121802*                               CM-AH-BENEFIT-AMT / 30
121802*            COMPUTE PI-CPYAMT ROUNDED =
121802*                               (WS-DAILY-RATE * PI-CDAYS)
121802*            MOVE WS-DAILY-RATE TO PI-DAILY-RATE
121802*            GO TO 1420-CHECK-AIG-BENEFIT.
03487
03488      IF WS-CALC-METHOD = '1'
03489          COMPUTE WS-DAILY-RATE ROUNDED = CM-AH-BENEFIT-AMT / 30
03490          COMPUTE PI-CPYAMT ROUNDED =
03491              (CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS) +
03492              (WS-DAILY-RATE     * DC-ODD-DAYS-OVER)
03493          MOVE WS-DAILY-RATE    TO PI-DAILY-RATE
03494          GO TO 1420-CHECK-AIG-BENEFIT.
03495
03496      IF WS-CALC-METHOD = '2'
03497          COMPUTE PI-DAILY-RATE ROUNDED =
03498                           (CM-AH-BENEFIT-AMT * 12) / 365
03499          COMPUTE PI-CPYAMT ROUNDED =
03500              (CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS) +
03501              (PI-DAILY-RATE     * DC-ODD-DAYS-OVER)
03502          GO TO 1420-CHECK-AIG-BENEFIT.
03503
03504      IF WS-CALC-METHOD = '4'
03505          MOVE CM-AH-BENEFIT-AMT   TO  WS-AH-BEN-AMT
03506          COMPUTE WS-DAILY-RATE ROUNDED = (WS-AH-BEN-AMT / 30)
03507          MOVE WS-DAILY-RATE TO PI-DAILY-RATE
03508          COMPUTE PI-CPYAMT ROUNDED =
03509                            WS-DAILY-RATE * DC-ELAPSED-DAYS
03510          GO TO 1420-CHECK-AIG-BENEFIT.
03511
03512      IF WS-CALC-METHOD = '5'
03513          COMPUTE PI-DAILY-RATE ROUNDED =
03514                             (CM-AH-BENEFIT-AMT * 12) / 365
03515          COMPUTE PI-CPYAMT ROUNDED =
03516                            PI-DAILY-RATE * DC-ELAPSED-DAYS
03517          GO TO 1420-CHECK-AIG-BENEFIT.
03518
03519      IF WS-CALC-METHOD NOT = '3'
03520         GO TO 1410-CHECK-PAYMENT.
03521
03522 ******   CALCULATION METHOD  3  ROUTINE
03523      COMPUTE PI-DAILY-RATE =
03524                  CM-AH-BENEFIT-AMT / FROM-DAYS-IN-MONTH.
03525
03526 ******CHECK TO SEE IF WITHIN SAME MONTH
03527      IF (WDF-MONTH = WDT-MONTH) AND (WDF-YEAR = WDT-YEAR)
03528         COMPUTE PI-CPYAMT ROUNDED = SAVE-ELAPSED-DAYS    *
03529                      (CM-AH-BENEFIT-AMT / FROM-DAYS-IN-MONTH)
03530          GO TO 1420-CHECK-AIG-BENEFIT.
03531
03532 ******CHECK TO SEE IF EVEN MONTHS AND NO ODD DAYS
03533      IF SAVE-ELAPSED-MONTHS NOT = ZEROS   AND
03534              SAVE-ODD-DAYS-OVER = ZEROS
03535       COMPUTE PI-CPYAMT  ROUNDED =
03536                   CM-AH-BENEFIT-AMT * SAVE-ELAPSED-MONTHS
03537       GO TO 1420-CHECK-AIG-BENEFIT.
03538
03539 ******BUMP THE FROM DATE WITH THE NUMBER OF MONTHS
03540 ******AND FIND THE NUMBER OF DAYS LEFT IN THIS MONTH.
03541      IF SAVE-ELAPSED-MONTHS NOT = ZERO
03542         ADD SAVE-ELAPSED-MONTHS  TO WDF-MONTH
03543         IF WDF-MONTH > 12
03544            ADD 1                 TO WDF-YEAR
03545            SUBTRACT 12         FROM WDF-MONTH.
03546
03547 ******CONVERT THE NEW FROM MONTH TO BINARY
03548      IF SAVE-ELAPSED-MONTHS NOT = ZERO
03549         MOVE WORK-DATE-FROM   TO DC-GREG-DATE-1-MDY
03550         MOVE '4'              TO DC-OPTION-CODE
03551         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03552         MOVE DC-DAYS-IN-MONTH TO FROM-DAYS-IN-MONTH.
03553
03554      IF WDF-MONTH = WDT-MONTH AND
03555         WDF-YEAR  = WDT-YEAR
03556           COMPUTE PI-CPYAMT ROUNDED = ((WDT-DAY  -  WDF-DAY)  *
03557                     (CM-AH-BENEFIT-AMT / FROM-DAYS-IN-MONTH)) +
03558                     (CM-AH-BENEFIT-AMT * SAVE-ELAPSED-MONTHS)
03559         ELSE
03560           COMPUTE FROM-PMT-DAYS = FROM-DAYS-IN-MONTH - WDF-DAY
03561           MOVE WDT-DAY              TO THRU-PMT-DAYS
03562           COMPUTE PI-CPYAMT ROUNDED =
03563        (FROM-PMT-DAYS * (CM-AH-BENEFIT-AMT / FROM-DAYS-IN-MONTH))
03564        +
03565        (THRU-PMT-DAYS * (CM-AH-BENEFIT-AMT / THRU-DAYS-IN-MONTH))
03566        +
03567        (CM-AH-BENEFIT-AMT * SAVE-ELAPSED-MONTHS).
03568
03569      GO TO 1420-CHECK-AIG-BENEFIT.
03570      EJECT
03571  1410-CHECK-PAYMENT.
03572 ******PAYMENT OPTION 6
03573 ******CHECK TO SEE IF LESS THAN 1 MONTH AND WITHIN SAME MONTH
03574      IF SAVE-ELAPSED-MONTHS > ZERO
03575         IF SAVE-ODD-DAYS-OVER = ZEROS
03576            GO TO 1412-CHECK-TO-DAY
03577         ELSE
03578            IF WDF-DAY > WDT-DAY
03579               IF WDF-MONTH = 02
03580                   PERFORM 1415-CHECK-LEAP-YEAR THRU 1415-EXIT
03581                   ADD 2  TO  SAVE-ODD-DAYS-OVER
03582                   GO TO 1412-CHECK-TO-DAY
03583               ELSE
03584                   IF FROM-DAYS-IN-MONTH = 31
03585                      SUBTRACT 1 FROM SAVE-ODD-DAYS-OVER
03586                      GO TO 1412-CHECK-TO-DAY
03587                   ELSE
03588                      GO TO 1412-CHECK-TO-DAY
03589            ELSE
03590                GO TO 1412-CHECK-TO-DAY.
03591
03592      IF WDF-MONTH NOT = WDT-MONTH
03593         IF WDF-MONTH = 02
03594            PERFORM 1415-CHECK-LEAP-YEAR THRU 1415-EXIT
03595            IF WDF-DAY = 29
03596               ADD 1              TO SAVE-ODD-DAYS-OVER
03597            ELSE
03598               ADD 2              TO SAVE-ODD-DAYS-OVER
03599         ELSE
03600            IF FROM-DAYS-IN-MONTH = 31
03601               SUBTRACT 1         FROM SAVE-ODD-DAYS-OVER.
03602
03603  1412-CHECK-TO-DAY.
03604         IF WDT-MONTH = 02 AND SAVE-ODD-DAYS-OVER NOT = 0
03605            IF WDT-DAY = 28
03606               ADD 2              TO SAVE-ODD-DAYS-OVER
03607            ELSE
03608               IF WDT-DAY = 29
03609                  ADD 1           TO SAVE-ODD-DAYS-OVER
03610               ELSE
03611                  NEXT SENTENCE
03612         ELSE
03613            IF WDT-DAY = 31 AND SAVE-ODD-DAYS-OVER NOT = 0
03614               SUBTRACT 1         FROM SAVE-ODD-DAYS-OVER.
03615
03616      COMPUTE WS-DAILY-RATE ROUNDED = CM-AH-BENEFIT-AMT / 30.
03617      MOVE WS-DAILY-RATE          TO PI-DAILY-RATE.
03618      COMPUTE PI-CPYAMT ROUNDED =
03619          (CM-AH-BENEFIT-AMT * SAVE-ELAPSED-MONTHS) +
03620          (WS-DAILY-RATE     * SAVE-ODD-DAYS-OVER).
03621
03622      GO TO 1420-CHECK-AIG-BENEFIT.
03623
03624  1415-CHECK-LEAP-YEAR.
03625      IF FROM-DAYS-IN-MONTH = 29
03626         SUBTRACT 1 FROM SAVE-ODD-DAYS-OVER.
03627
03628  1415-EXIT.
03629      EXIT.
03630
03631      EJECT
03632  1420-CHECK-AIG-BENEFIT.
03633
03634      IF WS-SPECIAL-CALC-CD = 'Q'
03635          GO TO 1425-CHECK-WHOLE-MONTH.
03636
121802*    IF (PI-COMPANY-ID = 'AIG' OR 'AUK')   AND
121802*       (CM-AH-BENEFIT-CD = '63' OR '66')  AND
121802*       (CL-NO-OF-PMTS-MADE < +6)          AND
121802*       (PI-PMTTYPE = '1' OR '2')
121802*        NEXT SENTENCE
121802*    ELSE
03643          MOVE SPACES TO PI-SPECIAL-BEN-SW.
03644          GO TO 1430-CHECK-AH-TOLERANCE.
03645
03646 ******************AIG LOGIC STARTS**********************
121802*    MOVE 'Y'          TO PI-SPECIAL-BEN-SW.
121802*    COMPUTE PI-CPYAMT = PI-CPYAMT * 1.5.
121802*    MOVE ER-3533      TO EMI-ERROR.
121802*    PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT.
121802*    GO TO 1430-CHECK-AH-TOLERANCE.
03652 ******************AIG LOGIC ENDS************************
03653
03654  1425-CHECK-WHOLE-MONTH.
03655 ******************************************************************
03656 ****    THIS PARAGRAPH COMPUTES THE 1ST PAYMENT FOR WHOLE     ****
03657 ****    MONTH DISABILITY CLAIMS.  THE PROCESS IS AS FOLLOWS:  ****
03658 ****      1.  IF THE PERIOD BEING PAID IS THE RETRO PERIOD    ****
03659 ****          A WHOLE MONTHS BENEFIT IS PAID.                 ****
03660 ****      2.  IF MORE THAN THE RETRO PERIOD IS BEING PAID THE ****
03661 ****          ADDITIONAL DAYS MUST BE IN WHOLE CALENDAR       ****
03662 ****          MONTHS.  PAYMENT = 1 MONTH (RETRO               ****
03663 ****          PERIOD) + NUMBER OF ADDITIONAL MONTHS BEING     ****
03664 ****          PAID * MONTHLY BENEFIT.                         ****
03665 ******************************************************************
03666
03667      MOVE SPACES                     TO  PI-SPECIAL-BEN-SW.
03668
03669      IF CL-NO-OF-PMTS-MADE > +0
03670          GO TO 1425-CONT-WHOLE-MONTH-PMTS.
03671
03672      IF PI-CDAYS < WS-BEN-DAYS
03673          GO TO 1430-CHECK-AH-TOLERANCE.
03674
03675      IF PI-CDAYS = WS-BEN-DAYS
03676          MOVE CM-AH-BENEFIT-AMT      TO  PI-CPYAMT
03677          GO TO 1430-CHECK-AH-TOLERANCE.
03678
121802*    IF PI-COMPANY-ID = 'ACM'
121802*       NEXT SENTENCE
121802*    ELSE
03682         GO TO 1425-NOT-ACM-W15C.
03683
121802*    MOVE PI-CPYFROM             TO  DC-BIN-DATE-1.
121802*    MOVE -1                     TO  DC-ELAPSED-DAYS.
121802*    MOVE +0                     TO  DC-ELAPSED-MONTHS.
121802*    MOVE '6'                    TO  DC-OPTION-CODE.
121802*    PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
121802*    IF NO-CONVERSION-ERROR
121802*       MOVE DC-BIN-DATE-2       TO  DC-BIN-DATE-1
121802*       MOVE PI-CPYTHRU          TO  DC-BIN-DATE-2
121802*       MOVE '1'                 TO  DC-OPTION-CODE
121802*       MOVE +0                  TO  DC-ELAPSED-DAYS
121802*       MOVE +0                  TO  DC-ELAPSED-MONTHS
121802*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
121802*       IF NO-CONVERSION-ERROR
121802*          IF (DC-ELAPSED-MONTHS > +0) OR
121802*             (DC-ELAPSED-DAYS NOT < WS-BEN-DAYS)
121802*             NEXT SENTENCE
121802*          ELSE
121802*             MOVE +0            TO  PI-CPYAMT
121802*             MOVE ER-0846       TO  EMI-ERROR
121802*             PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
121802*             GO TO 1430-CHECK-AH-TOLERANCE
121802*       ELSE
121802*           MOVE ER-0846         TO  EMI-ERROR
121802*           PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
121802*           GO TO 1430-CHECK-AH-TOLERANCE.
121802*
121802*    IF DC-ODD-DAYS-OVER NOT < WS-BEN-DAYS
121802*       ADD +1 TO DC-ELAPSED-MONTHS.
121802*
121802*    COMPUTE PI-CPYAMT ROUNDED =
121802*                CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS.
121802*
121802*    GO TO 1430-CHECK-AH-TOLERANCE.
03717
03718  1425-NOT-ACM-W15C.
03719
03720      MOVE PI-CPYFROM                 TO  DC-BIN-DATE-1.
03721      MOVE '6'                        TO  DC-OPTION-CODE.
03722      MOVE +0                         TO  DC-ELAPSED-MONTHS.
03723      COMPUTE DC-ELAPSED-DAYS = WS-BEN-DAYS - 1.
03724
03725      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03726      IF NO-CONVERSION-ERROR
03727          MOVE DC-BIN-DATE-2          TO  DC-BIN-DATE-1
03728      ELSE
03729          MOVE ER-0846                TO  EMI-ERROR
03730          PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
03731          GO TO 1430-CHECK-AH-TOLERANCE.
03732
03733      MOVE PI-CPYTHRU                 TO  DC-BIN-DATE-2.
03734      MOVE '1'                        TO  DC-OPTION-CODE.
03735      MOVE +0                         TO  DC-ELAPSED-MONTHS.
03736      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03737      IF NO-CONVERSION-ERROR
03738          IF DC-ODD-DAYS-OVER = +0
03739              NEXT SENTENCE
03740          ELSE
03741              MOVE ER-0846            TO  EMI-ERROR
03742              PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
03743              GO TO 1430-CHECK-AH-TOLERANCE
03744      ELSE
03745          MOVE ER-0846                TO  EMI-ERROR
03746          PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
03747          GO TO 1430-CHECK-AH-TOLERANCE.
03748
03749      COMPUTE PI-CPYAMT ROUNDED = CM-AH-BENEFIT-AMT +
03750          (CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS).
03751
03752      GO TO 1430-CHECK-AH-TOLERANCE.
03753
03754  1425-CONT-WHOLE-MONTH-PMTS.
03755 ******************************************************************
03756 ****    WHOLE MONTH DISABILITY EDIT.  IN ORDER TO QUALIFY     ****
03757 ****    FOR A PAYMENT TO BE MADE THE PAYMENT PERIOD MUST      ****
03758 ****    BE IN CALENDAR MONTH INCREMENTS.                      ****
03759 ******************************************************************
03760
03761      IF PI-COMPANY-ID = 'ACM'
03762         NEXT SENTENCE
03763      ELSE
03764         IF DC-ODD-DAYS-OVER = +0
03765             NEXT SENTENCE
03766         ELSE
03767             MOVE ER-0845            TO  EMI-ERROR
03768             PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
03769             GO TO 1430-CHECK-AH-TOLERANCE.
03770
03771      COMPUTE PI-CPYAMT ROUNDED =
03772                 CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS.
03773
03774  1430-CHECK-AH-TOLERANCE.
03775      IF PI-PMT-APPR-SW = 'G'
03776          GO TO 1430-CONT-CHECK-AH-TOLERANCE.
03777
121802*    IF PI-COMPANY-ID = 'HER' OR 'KSM'
121802*        COMPUTE WS-MAX-PMT-TOL = WS-MAX-PMT-TOL * WS-PMT-MOS
121802*        MOVE WS-MAX-PMT-TOL         TO  PI-MAX-PMT-TOL.
121802*
121802*    IF PI-COMPANY-ID = 'HER' OR 'KSM'
121802*      IF WDF-MONTH = 02
121802*        IF SAVE-ELAPSED-MONTHS = 0
121802*          NEXT SENTENCE
121802*        ELSE
121802*          IF SAVE-ODD-DAYS-OVER > 0
121802*            MOVE +0                     TO  WS-MAX-PMT-TOL
121802*            COMPUTE WS-PMT-MOS ROUNDED =
121802*                (SAVE-ODD-DAYS-OVER / +30) + .005
121802*            COMPUTE WS-MAX-PMT-TOL = WS-HER-MO-PMT-AMT *
121802*                                     WS-PMT-MOS
121802*            COMPUTE WS-MAX-PMT-TOL = WS-MAX-PMT-TOL +
121802*                (WS-HER-MO-PMT-AMT * SAVE-ELAPSED-MONTHS)
121802*          ELSE
121802*            MOVE +0                     TO  WS-MAX-PMT-TOL
121802*            MOVE SAVE-ELAPSED-MONTHS    TO  WS-PMT-MOS
121802*            COMPUTE WS-MAX-PMT-TOL = WS-HER-MO-PMT-AMT *
121802*                                     WS-PMT-MOS.
03800
03801      IF WS-MAX-PMT-TOL NOT = ZERO
03802         IF (PI-EPYAMT > WS-MAX-PMT-TOL) OR
03803            (PI-CPYAMT > WS-MAX-PMT-TOL)
03804             MOVE ER-0494         TO  EMI-ERROR
03805             PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
03806             GO TO 1500-EDIT-EXPENSES.
03807
03808  1430-CONT-CHECK-AH-TOLERANCE.
03809
03810      IF PI-EPYAMT = ZEROS OR   PI-EPYAMT NEGATIVE
03811         GO TO 1500-EDIT-EXPENSES.
03812
03813      IF CM-O-B-COVERAGE
03814         GO TO 1500-EDIT-EXPENSES.
03815
03816      IF PI-COMPANY-ID = 'NCL'
03817        IF FOUND-TOL-SW = 'Y'
03818          IF WS-PMT-TOL NOT = ZERO
03819            IF ((PI-CPYAMT - WS-PMT-TOL) > PI-EPYAMT) OR
03820               ((PI-CPYAMT + WS-PMT-TOL) < PI-EPYAMT)
03821                   MOVE PI-PMTNOTE1 TO WS-NCL-PMT-OPTION
03822                   IF WS-NCL-PMT-OPTION = SPACES
03823                       MOVE ER-0699 TO EMI-ERROR
03824                       PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
03825                   ELSE
03826                   IF WS-NCL-PMT-OPTION ALPHABETIC-UPPER
03827                       NEXT SENTENCE
03828                   ELSE
03829                   IF NOT WS-VALID-NCL-OPTION
03830                       MOVE ER-0699 TO EMI-ERROR
03831                       PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT.
03832
03833      IF WS-PMT-TOL NOT = ZERO
03834         IF ((PI-CPYAMT - WS-PMT-TOL) > PI-EPYAMT)
03835            or ((PI-CPYAMT + WS-PMT-TOL) < PI-EPYAMT)
03836            MOVE ER-0495          TO EMI-ERROR
03837            PERFORM 1490-PAYMENT-ERROR
                                       THRU 1490-EXIT
061013        end-if
061013     end-if
03838
03839      GO TO 1500-EDIT-EXPENSES.
03840
03841      EJECT
03842  1440-COMPUTE-LIFE-REMAINING.
03843
03844      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
03845         IF PI-PMTTYPE = '4'
03846           MOVE PI-EPYAMT TO PI-CPYAMT
03847           GO TO 1460-TEST-TYPE.
03848
03849      IF WS-LF-COVERAGE-TYPE = 'L'
03850         COMPUTE PI-CPYAMT = CM-LF-BENEFIT-AMT
03851                           - CL-TOTAL-PAID-AMT
03852         GO TO 1450-CHECK-MAX.
03853
03854      IF PI-LIFE-OVERRIDE-L1 = 'P' OR
03855         PI-LF-COVERAGE-TYPE = 'P'
03856           COMPUTE PI-CPYAMT = CM-LF-BENEFIT-AMT -
03857                               CM-LF-ITD-DEATH-AMT
03858           GO TO 1450-CHECK-MAX.
03859
03860      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
03861      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
03862      MOVE CL-INCURRED-DT         TO CP-VALUATION-DT.
03863      MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM.
03864      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
03865      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
03866      MOVE '4'                    TO CP-REM-TERM-METHOD.
03867      MOVE WS-EARNING-METHOD      TO CP-EARNING-METHOD.
03868      MOVE WS-SPECIAL-CALC-CD     TO CP-SPECIAL-CALC-CD.
03869
03870      PERFORM 9800-LINK-REM-TERM.
03871
03872      MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE.
03873      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
03874      MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM.
03875      MOVE CM-LF-BENEFIT-AMT      TO CP-ORIGINAL-BENEFIT.
03876      MOVE CM-LF-ALT-BENEFIT-AMT  TO CP-ALTERNATE-BENEFIT.
03877      MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM.
03878      MOVE CM-LOAN-APR            TO CP-LOAN-APR.
03879      MOVE CM-LOAN-TERM           TO CP-LOAN-TERM.
03880      MOVE CM-PAY-FREQUENCY       TO CP-PAY-FREQUENCY.
03881      MOVE WS-STATE-ABBREV        TO CP-STATE-STD-ABBRV.
03882      MOVE WS-EARNING-METHOD      TO CP-EARNING-METHOD.
03883      MOVE WS-SPECIAL-CALC-CD     TO CP-SPECIAL-CALC-CD.
CIDMOD     MOVE CM-RATE-CLASS          TO CP-CLASS-CODE
041710     MOVE CM-LF-BENEFIT-CD       TO CP-BENEFIT-CD.
041710     MOVE CM-PMT-EXTENSION-DAYS  TO CP-TERM-OR-EXT-DAYS.
041710     MOVE 'Y'                    TO CP-LF-CLAIM-CALC-SW.
022122     move zeros                  to ws-max-tot-ben
022122                                    ws-work-ben-pct
120115     IF PI-COMPANY-ID = 'DCC' or 'VPP'
061013        MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
061013        MOVE PI-CARRIER          TO ACCT-CARRIER
061013        MOVE PI-GROUPING         TO ACCT-GROUPING
061013        MOVE PI-STATE            TO ACCT-STATE
061013        MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
061013        MOVE PI-CERT-EFF-DT      TO ACCT-EXP-DT
061013        MOVE ERACCT-PARTIAL-KEY  TO WS-ERACCT-SAVE-KEY
061013        MOVE SPACES              TO WS-ERACCT-HOLD-RECORD
061013
061013        
      * EXEC CICS READ
061013*          DATASET ('ERACCT')
061013*          RIDFLD  (ERACCT-KEY)
061013*          SET     (ADDRESS OF ACCOUNT-MASTER)
061013*          GTEQ
061013*          resp    (WS-RESPONSE)
061013*       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00012213' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303132323133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013        IF WS-RESP-NORMAL
061013           AND (WS-ERACCT-SAVE-KEY = AM-CONTROL-PRIMARY (1:20))
061013           and (pi-cert-eff-dt < am-expiration-dt)
061013           and (pi-cert-eff-dt >= am-effective-dt)
061013           move am-dcc-product-code to pi-dcc-product-code
061013        else
061013           move spaces           to pi-dcc-product-code
061013        end-if
061013        if pi-dcc-product-code not = spaces
061013           move cm-ah-benefit-cd to pi-ah-benefit-cd
                 move cm-lf-benefit-cd to pi-lf-benefit-cd
061013           PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
022122           move cl-insured-birth-dt to dc-bin-date-1
022122           move cl-incurred-dt to dc-bin-date-2
022122           move '1' to dc-option-code
022122           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
022122           compute ws-att-age =
022122              dc-elapsed-months / 12
022122
022122           move zeros to dc-elapsed-months dc-elapsed-days
061013           IF PDEF-FOUND
061013              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122                 (P1 > +11)
022122                 OR (PD-PROD-CODE (P1) = cl-claim-type
022122                   AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
061013              END-PERFORM
022122              IF P1 < +12
061013                 MOVE PD-MAX-AMT (P1)
022122                                 TO ws-MAX-TOT-BEN
022122                 if pd-ben-pct (p1) not numeric
022122                    move zeros   to pd-ben-pct (p1)
022122                 end-if
022122                 if pd-ben-pct (p1) = zeros
022122                    move +1      to ws-work-ben-pct
022122                 else
022122                    move pd-ben-pct (p1)
022122                                 to ws-work-ben-pct
022122                 end-if
                    else
                       move er-1674    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013              END-IF
                 else
                    move er-1672       to emi-error
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013           END-IF
061013        end-if
061013     END-IF
03885      
      * EXEC CICS LINK
03886 *       PROGRAM  (LINK-REMAMT)
03887 *       COMMAREA (CALCULATION-PASS-AREA)
03888 *       LENGTH   (CP-COMM-LENGTH)
03889 *    END-EXEC.
      *    MOVE '."C                   (   #00012271' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-REMAMT, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
022122     if ws-work-ben-pct <> 0
022122        compute cp-remaining-amt =
022122           cp-remaining-amt * ws-work-ben-pct
022122     end-if
022122     if ws-max-tot-ben <> 0
022122        if cp-remaining-amt > ws-max-tot-ben
022122           move ws-max-tot-ben to cp-remaining-amt
022122        end-if
022122     end-if
03890
03891      COMPUTE PI-CPYAMT = CP-REMAINING-AMT - CL-TOTAL-PAID-AMT.
03892
041710     MOVE ' '                    TO CP-LF-CLAIM-CALC-SW.
041710     MOVE CP-SCNP-6MO-AMT        TO PI-MO-BEN-AMT.
041710
03893      IF PI-COMPANY-ID = 'BOA' AND
03894         PI-PMTTYPE    = '2'   AND
03895         PI-OFFLINE    = 'N'
03896            GO TO 1441-COMPUTE-BOA-INTEREST.
03897
03898      IF PI-COMPANY-ID = 'NCL' AND
03899         PI-PMTTYPE    = '2'   AND
03900         PI-OFFLINE    = 'N'
03901           GO TO 1442-COMPUTE-NCL-INTEREST
03902        ELSE
03903           GO TO 1445-CONTINUE.
03904
03905  1441-COMPUTE-BOA-INTEREST.
03906      MOVE +0             TO WS-CALC-INT.
03907
03908      MOVE CL-INCURRED-DT TO DC-BIN-DATE-1.
03909      MOVE WS-TODAY-DATE  TO DC-BIN-DATE-2.
03910      MOVE +0             TO DC-ELAPSED-MONTHS
03911                             DC-ELAPSED-DAYS.
03912      MOVE '1'            TO DC-OPTION-CODE.
03913      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03914      IF NO-CONVERSION-ERROR
03915          NEXT SENTENCE
03916      ELSE
03917          GO TO 1445-CONTINUE.
03918
03919      IF (PI-INT-RATE NOT > ZEROS)  AND
03920         (DC-ELAPSED-DAYS > +30)
03921          MOVE ER-3531            TO EMI-ERROR
03922          MOVE -1                 TO PINTL
03923          MOVE AL-UNBON           TO PINTA
03924          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03925
03926      IF PI-EPYAMT NOT > ZEROS
03927          GO TO 1445-CONTINUE.
03928
03929      IF (PI-INT-RATE NOT > ZEROS)  OR
03930         (DC-ELAPSED-DAYS NOT > +30)
03931          MOVE PI-EPYAMT TO PI-CPYAMT
03932          GO TO 1441-FORMAT-NOTE.
03933
03934      COMPUTE WS-CALC-INT ROUNDED =  PI-EPYAMT
03935                                  *  PI-INT-RATE
03936                                  *  DC-ELAPSED-DAYS
03937                                  /  365
03938                                  /  100.
03939
03940      COMPUTE PI-CPYAMT = PI-EPYAMT + WS-CALC-INT.
03941
03942  1441-FORMAT-NOTE.
03943
03944      MOVE SPACES        TO WS-BOANOTE1.
03945      MOVE 'BENEFIT -  ' TO WS-BOANOTE1-DESC.
03946      MOVE PI-EPYAMT     TO WS-BOANOTE1-EPYAMT.
03947      MOVE WS-BOANOTE1   TO WS-PMTNOTE1 PI-PMTNOTE1 NOTE1I.
03948      MOVE AL-UANON      TO NOTE1A.
03949
03950      IF WS-CALC-INT > ZEROS
03951          MOVE SPACES        TO WS-BOANOTE2
03952          MOVE 'INTEREST - ' TO WS-BOANOTE2-DESC
03953          MOVE WS-CALC-INT   TO WS-BOANOTE2-INT
03954          MOVE WS-BOANOTE2   TO WS-PMTNOTE2 PI-PMTNOTE2 NOTE2I
03955          MOVE AL-UANON      TO NOTE2A.
03956
03957      GO TO 1445-CONTINUE.
03958
03959  1442-COMPUTE-NCL-INTEREST.
03960      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
03961      MOVE PI-STATE               TO WS-ST-ACCESS.
03962      MOVE WS-STATE-ACCESS        TO CNTL-ACCESS.
03963      MOVE '3'                    TO CNTL-REC-TYPE.
03964      MOVE +0                     TO CNTL-SEQ-NO.
03965      MOVE 'STAT'                 TO FILE-SWITCH.
03966
03967      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.
03968
03969      MOVE +0                 TO WS-CALC-INT.
03970
03971      IF ST-STAT-FROM-INCURRED
03972          MOVE CL-INCURRED-DT TO WS-COMPUTE-FROM-DT
03973        ELSE
03974      IF ST-STAT-FROM-REPORTED
03975          MOVE CL-REPORTED-DT TO WS-COMPUTE-FROM-DT
03976        ELSE
03977          GO TO 1445-CONTINUE.
03978
03979      MOVE WS-COMPUTE-FROM-DT TO DC-BIN-DATE-1.
03980      MOVE WS-TODAY-DATE      TO DC-BIN-DATE-2.
03981      MOVE +0                 TO DC-ELAPSED-MONTHS
03982                                 DC-ELAPSED-DAYS.
03983      MOVE '1'                TO DC-OPTION-CODE.
03984      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03985      IF NO-CONVERSION-ERROR
03986          NEXT SENTENCE
03987      ELSE
03988          GO TO 1445-CONTINUE.
03989
03990      IF (CF-ST-STAT-INTEREST NOT > ZEROS) OR
03991         (DC-ELAPSED-DAYS NOT > CF-ST-NO-DAYS-ELAPSED)
03992          GO TO 1445-CONTINUE.
03993
03994      COMPUTE WS-CALC-INT ROUNDED =  PI-CPYAMT
03995                                  *  CF-ST-STAT-INTEREST.
03996
03997      MOVE SPACES              TO WS-NCLNOTE1.
03998      MOVE 'BENEFIT -  '       TO WS-NCLNOTE1-DESC.
03999      MOVE PI-CPYAMT           TO WS-NCLNOTE1-CPYAMT.
04000      MOVE WS-NCLNOTE1         TO WS-PMTNOTE1
04001                                  PI-PMTNOTE1
04002                                  NOTE1I.
04003      MOVE AL-UANON            TO NOTE1A.
04004
04005      IF WS-CALC-INT > ZEROS
04006          MOVE SPACES          TO WS-NCLNOTE2
04007          MOVE 'INTEREST - '   TO WS-NCLNOTE2-DESC
04008          MOVE WS-CALC-INT     TO WS-NCLNOTE2-INT
04009          MOVE WS-NCLNOTE2     TO WS-PMTNOTE2
04010                                  PI-PMTNOTE2
04011                                  NOTE2I
04012          MOVE AL-UANON        TO NOTE2A.
04013
04014      COMPUTE PI-CPYAMT = PI-CPYAMT + WS-CALC-INT.
04015
04016  1445-CONTINUE.
04017
04018      IF PI-EPYAMT = ZEROS OR PI-PMTTYPE = '4'
04019         GO TO 1460-TEST-TYPE.
04020
04021      IF CM-O-B-COVERAGE
04022         GO TO 1460-TEST-TYPE.
04023
04024      IF PI-COMPANY-ID = 'BOA' OR 'NCL'
04025          GO TO 1450-CHECK-MAX.
04026
120115     if (pi-company-id = 'DCC' or 'VPP')
061013        and (pdef-found)
100314        if pi-epyamt > pd-max-amt (p1)
100314           move er-1657          to emi-error
100314           perform 1490-payment-error
100314                                 thru 1490-exit
100314           go to 1460-test-type
100314        end-if
061013     else
04027         IF (PI-CPYAMT - (CP-REMAINING-AMT-PRV - CP-REMAINING-AMT)
04028            > PI-EPYAMT)
04029                          OR
04030            (PI-CPYAMT + (CP-REMAINING-AMT-PRV - CP-REMAINING-AMT)
04031            < PI-EPYAMT)
04032            MOVE ER-0594          TO EMI-ERROR
04033            PERFORM 1490-PAYMENT-ERROR
                                       THRU 1490-EXIT
04034            GO TO 1460-TEST-TYPE
              end-if
           end-if
           .
04036  1450-CHECK-MAX.
04037
04038      IF PI-LIFE-OVERRIDE-L1 = 'P' OR
04039         PI-LF-COVERAGE-TYPE = 'P'
04040          COMPUTE WS-BEN-AMT-LEFT = CM-LF-BENEFIT-AMT -
04041                                    CM-LF-ITD-DEATH-AMT
04042      ELSE
04043          COMPUTE WS-BEN-AMT-LEFT = CM-LF-BENEFIT-AMT -
04044                                    CL-TOTAL-PAID-AMT.
04045
04046      IF PI-EPYAMT > WS-BEN-AMT-LEFT
04047           MOVE ER-0543           TO EMI-ERROR
04048           PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT.
04049
04050  1460-TEST-TYPE.
04051
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
100518        IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
030514           COMPUTE WS-LF-BEN-AMT = CM-LF-BENEFIT-AMT +
030514                                   CM-LF-ALT-BENEFIT-AMT -
030514                                   CL-TOTAL-PAID-AMT
030514           IF PI-EPYAMT > WS-LF-BEN-AMT
030514               MOVE ER-0543      TO EMI-ERROR
030514               PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
030514           END-IF
030514        END-IF
030514     END-IF
030514
04052      IF PI-PMT-APPR-SW = 'G'
04053        NEXT SENTENCE
04054      ELSE
04055        IF WS-MAX-LF-PMT-TOL NOT = ZERO
04056          IF PI-EPYAMT = ZERO
04057              IF PI-CPYAMT > WS-MAX-LF-PMT-TOL
04058                  MOVE ER-0494    TO  EMI-ERROR
04059                  PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
04060                  GO TO 1500-EDIT-EXPENSES
04061              ELSE
04062                  NEXT SENTENCE
04063          ELSE
04064              IF PI-EPYAMT > WS-MAX-LF-PMT-TOL
04065                  MOVE ER-0494    TO  EMI-ERROR
04066                  PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
04067                  GO TO 1500-EDIT-EXPENSES.
04068
04069      IF PI-PMTTYPE NOT = '2' OR
04070         (WS-LF-COVERAGE-TYPE = 'L' OR 'P')
04071          GO TO 1500-EDIT-EXPENSES.
04072
04073      MOVE CP-REMAINING-AMT-PRV   TO WS-EDIT-PATTERN.
04074      MOVE ER-0526                TO EMI-ERROR.
04075      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04076
04077      IF EMI-ERROR-NUMBER (1) = '0526'
04078         MOVE WS-EDIT             TO EMI-TEXT-VARIABLE (1).
04079
04080      IF EMI-ERROR-NUMBER (2) = '0526'
04081         MOVE WS-EDIT             TO EMI-TEXT-VARIABLE (2).
04082
04083      GO TO 1500-EDIT-EXPENSES.
04084
04085  1490-PAYMENT-ERROR.
04086      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04087
04088      IF EMI-MESSAGE-FORMATTED
04089          MOVE -1                 TO EPYAMTL
04090          MOVE AL-UNBON           TO EPYAMTA.
04091
04092  1490-EXIT.
04093       EXIT.
04094      EJECT
04095  1500-EDIT-EXPENSES.
04096      IF PI-COMPANY-ID = 'MET' OR 'AIG' OR 'AUK' OR 'DMD'
04097          GO TO 1500-SKIP-ERROR-0556.
04098
04099  1500-CONTINUE-ON.
04100
04101      IF CM-O-B-COVERAGE
04102         MOVE ER-0556             TO EMI-ERROR
04103         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04104
04105  1500-SKIP-ERROR-0556.
04106      IF PI-PMTTYPE = '4' OR '5' OR '6'
04107         IF PI-EEXPENS NOT = ZEROS
04108            MOVE ER-0524          TO EMI-ERROR
04109            GO TO 1590-EXPENSE-ERROR
04110         ELSE
04111            GO TO 1600-EDIT-RESERVES.
04112
04113      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
04114      MOVE '6'                    TO CNTL-REC-TYPE.
04115
04116      IF CONTROL-IS-ACTUAL-CARRIER
04117          MOVE PI-CARRIER         TO WS-CARR
04118      ELSE
04119          MOVE PI-CARRIER-CONTROL-LEVEL TO WS-CARR.
04120
04121      MOVE WS-CARR-ACCESS         TO CNTL-ACCESS.
04122      MOVE +0                     TO CNTL-SEQ-NO.
04123      MOVE 'CARR'                 TO FILE-SWITCH.
04124
04125      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.
04126
04127      IF EXPENSE-CALC-MANUAL
04128         GO TO 1600-EDIT-RESERVES.
04129
04130      IF DOLLARS-PER-PMT
04131         MOVE CF-EXPENSE-DOLLAR   TO PI-CEXPENS
04132         GO TO 1600-EDIT-RESERVES.
04133
04134      IF DOLLARS-PER-MONTH AND
121802        (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                           OR 'B' or 'H')
04136        COMPUTE PI-CEXPENS = CF-EXPENSE-DOLLAR * EXPENSE-MONTH-SAVE
04137        GO TO 1600-EDIT-RESERVES.
04138
04139      IF PERCENT-OF-PAYMENT
04140         COMPUTE PI-CEXPENS = PI-CPYAMT * CF-EXPENSE-PERCENT.
04141
04142      GO TO 1600-EDIT-RESERVES.
04143
04144  1590-EXPENSE-ERROR.
04145      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04146
04147      IF EMI-MESSAGE-FORMATTED
04148         MOVE -1                  TO EEXPENSL
04149         MOVE AL-UNBON            TO EEXPENSA.
04150
04151      EJECT
04152  1600-EDIT-RESERVES.
04153      IF PI-PMTTYPE = '4' OR '5' OR '6'
04154         IF PI-ERESV = ZEROS
04155            GO TO 1700-EDIT-EXPENSE-TYPE
04156         ELSE
04157            MOVE ER-0525          TO EMI-ERROR
04158            PERFORM 1690-RESERVE-ERROR THRU 1690-EXIT
04159            GO TO 1700-EDIT-EXPENSE-TYPE.
04160
04161      MOVE ELMSTR-KEY             TO ELTRLR-KEY.
04162      MOVE ZEROS                  TO TRLR-SEQ-NO.
04163      PERFORM 7950-READ-TRAILER THRU 7950-EXIT.
04164
04165      IF NOT AT-MANUAL-RESERVES-USED
04166         IF PI-ERESV NOT = ZEROS
04167            MOVE ER-0518          TO EMI-ERROR
04168            PERFORM 1690-RESERVE-ERROR THRU 1690-EXIT.
04169
04170      IF AT-MANUAL-RESERVES-USED
04171         MOVE AT-CURRENT-MANUAL-RESERVE  TO PI-CRESV
04172         GO TO 1700-EDIT-EXPENSE-TYPE.
04173
04174      IF AT-FUTURE-RESERVES-USED
04175         ADD AT-CURRENT-MANUAL-RESERVE   TO WS-RESERVE-WORK.
04176
04177      IF AT-FUTURE-RESERVES-USED
04178         ADD AT-FUTURE-RESERVE    TO WS-RESERVE-WORK.
04179
04180      IF AT-PAY-TO-CURRENT-USED OR AT-LF-PTC-USED
04181         ADD AT-PAY-CURRENT-RESERVE TO WS-RESERVE-WORK.
04182
04183      IF AT-IBNR-RESERVES-USED
04184         ADD AT-IBNR-RESERVE      TO WS-RESERVE-WORK.
04185
04186      MOVE WS-RESERVE-WORK        TO PI-CRESV.
04187      GO TO 1700-EDIT-EXPENSE-TYPE.
04188
04189  1690-RESERVE-ERROR.
04190      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04191
04192      IF EMI-MESSAGE-FORMATTED
04193         MOVE AL-UNBON            TO ERESVA
04194         MOVE -1                  TO ERESVL.
04195
04196  1690-EXIT.
04197       EXIT.
04198
04199     EJECT
04200  1700-EDIT-EXPENSE-TYPE.
04201      IF PI-PMTTYPE NOT = '5' AND '6'
04202         GO TO 2000-EDIT-DONE.
04203
04204      IF PI-ETYPE = ' ' OR '1' OR '2' OR '3' OR '4' OR '5' OR
04205                    '6' OR '7' OR '8' OR '9'
04206         GO TO 2000-EDIT-DONE.
04207
04208  1705-EXPENSE-ERROR.
04209      MOVE ER-0573                TO EMI-ERROR.
04210      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04211
04212      IF EMI-MESSAGE-FORMATTED
04213         MOVE AL-UABON            TO ETYPEA
04214         MOVE -1                  TO ETYPEL.
04215
04216      GO TO 2000-EDIT-DONE.
04217      EJECT
04218  1950-NOT-FOUND.
04219      IF FILE-SWITCH = 'MSTR'
04220         MOVE ER-0204             TO EMI-ERROR.
04221
04222      IF FILE-SWITCH = 'CARR'
04223         MOVE ER-0252             TO EMI-ERROR.
04224
04225      IF FILE-SWITCH = 'COMP'
04226         MOVE ER-0254             TO EMI-ERROR.
04227
04228      IF FILE-SWITCH = 'STAT'
04229         MOVE ER-0149             TO EMI-ERROR.
04230
04231      IF FILE-SWITCH = 'PROC'
04232         MOVE ER-0019             TO EMI-ERROR.
04233
04234      IF FILE-SWITCH = 'ACCT'
04235         MOVE ER-0198             TO EMI-ERROR.
090821     if eracct-browse-started
090821        
      * exec cics endbr
090821*          dataset('ERACCT')
090821*       end-exec
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012660' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821        move spaces              to ws-eracct-startbr-ind
090821     end-if
04237      IF FILE-SWITCH = 'BENE'
100518        IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
04239            MOVE ER-0282          TO EMI-ERROR
04240         ELSE
04241            MOVE ER-0283          TO EMI-ERROR.
04242
04243      MOVE -1                     TO PMTTYPEL.
04244      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04245
04246      MOVE EMI-FORCABLE-CTR       TO  PI-FORCE-COUNT
04247                                      WS-FORCE-CTR.
04248
04249      MOVE EMI-FATAL-CTR          TO  PI-FATAL-COUNT
04250                                      WS-FATAL-CTR.
04251
04252      GO TO 8200-SEND-DATAONLY.
04253
04254      EJECT
04255  2000-EDIT-DONE.
061013     perform 2210-dcc-max-benes thru 2210-exit
120115     if pi-company-id not = 'DCC' and 'VPP'
              IF (WS-SPECIAL-CALC-CD = 'C')
                 and (cl-critical-period > 0)
092310           IF CM-AH-BENEFIT-AMT NOT = ZERO
092310              IF PI-EPYAMT > ZEROS
092310                 COMPUTE ws-benefits-paid = (CL-TOTAL-PAID-AMT +
092310                    PI-EPYAMT) / CM-AH-BENEFIT-AMT
092310              ELSE
092310                 COMPUTE ws-benefits-paid = (CL-TOTAL-PAID-AMT +
092310                    PI-CPYAMT) / CM-AH-BENEFIT-AMT
092310              END-IF
                    if ws-benefits-paid > cl-critical-period
092310                 MOVE ER-0657          TO EMI-ERROR
092310                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
092310              END-IF
092310           END-IF
              END-IF
           end-if
04256      IF PI-HOLDTIL NOT = ZEROS
04257         MOVE PI-HOLDTIL          TO HOLDTILO
04258         INSPECT HOLDTILO REPLACING ALL SPACES BY '/'
082218        MOVE ER-3272          TO EMI-ERROR
082218        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
082218     END-IF
04259
04260      IF PI-EPYFROM NOT = ZEROS
04261         MOVE PI-EPYFROM          TO EPYFROMO
04262         INSPECT EPYFROMI REPLACING ALL SPACES BY '/'.
04263
04264      IF PI-EPYTHRU NOT = ZEROS
04265         MOVE PI-EPYTHRU            TO EPYTHRUO
04266         INSPECT EPYTHRUI REPLACING ALL SPACES BY '/'
04267         IF PI-USES-PAID-TO
04268            MOVE PI-EPYTHRU         TO DC-GREG-DATE-1-MDY
04269            MOVE '4'                TO DC-OPTION-CODE
04270            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04271            IF NO-CONVERSION-ERROR
04272               MOVE +1              TO DC-ELAPSED-DAYS
04273               MOVE +0              TO DC-ELAPSED-MONTHS
04274               MOVE '6'             TO DC-OPTION-CODE
04275               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04276               IF NO-CONVERSION-ERROR
04277                  MOVE DC-GREG-DATE-1-EDIT TO EPYTHRUI.
04278
04279      MOVE PI-EDAYS               TO EDAYSO.
04280
04281      IF PI-AIGFROM NOT = ZEROS
04282         MOVE PI-AIGFROM          TO AIGFROMO
04283         INSPECT AIGFROMO REPLACING ALL SPACES BY '/'.
04284
04285      MOVE PI-EPYAMT              TO EPYAMTO.
04286      MOVE PI-ERESV               TO ERESVO.
04287      MOVE PI-EEXPENS             TO EEXPENSO.
04288
04289      MOVE PI-CPYFROM             TO DC-BIN-DATE-1.
04290      MOVE SPACES                 TO DC-OPTION-CODE.
04291      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04292
04293      IF DATE-CONVERSION-ERROR
04294         MOVE SPACES              TO CPYFROMI
04295      ELSE
04296         MOVE DC-GREG-DATE-1-EDIT TO CPYFROMI
04297         IF PI-EPYFROM = ZEROS
04298            MOVE AL-UANON             TO EPYFROMA
04299            MOVE DC-GREG-DATE-1-MDY   TO PI-EPYFROM
04300            MOVE DC-GREG-DATE-1-EDIT  TO EPYFROMI.
04301
04302      IF PI-USES-PAID-TO
04303         MOVE PI-CPYTHRU                    TO DC-BIN-DATE-1
04304         MOVE +1                            TO DC-ELAPSED-DAYS
04305         MOVE +0                            TO DC-ELAPSED-MONTHS
04306         MOVE '6'                           TO DC-OPTION-CODE
04307         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04308         IF NO-CONVERSION-ERROR
04309            MOVE DC-GREG-DATE-1-EDIT        TO CPYTHRUI
04310            IF PI-EPYTHRU = ZEROS
04311               MOVE AL-UANON                TO EPYTHRUA
04312               MOVE DC-GREG-DATE-1-EDIT     TO EPYTHRUI
04313               MOVE ' '                     TO DC-OPTION-CODE
04314               MOVE +0                      TO DC-ELAPSED-DAYS
04315                                               DC-ELAPSED-MONTHS
04316               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04317               IF NO-CONVERSION-ERROR
04318                   MOVE DC-GREG-DATE-1-MDY  TO  PI-EPYTHRU
04319                   GO TO 2001-CONTINUE
04320               ELSE
04321                   GO TO 2001-CONTINUE
04322            ELSE
04323               GO TO 2001-CONTINUE.
04324
04325      MOVE PI-CPYTHRU             TO DC-BIN-DATE-1.
04326      MOVE SPACES                 TO DC-OPTION-CODE.
04327      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04328
04329      IF DATE-CONVERSION-ERROR
04330         MOVE SPACES              TO CPYTHRUI
04331      ELSE
04332         MOVE DC-GREG-DATE-1-EDIT TO CPYTHRUI
04333         IF PI-EPYTHRU = ZEROS
04334            MOVE AL-UANON TO EPYTHRUA
04335            MOVE DC-GREG-DATE-1-MDY   TO PI-EPYTHRU
04336            MOVE DC-GREG-DATE-1-EDIT  TO EPYTHRUI.
04337
04338  2001-CONTINUE.
04339
04340      MOVE PI-CDAYS               TO CDAYSO.
04341
04342      IF PI-EDAYS = ZEROS
04343         MOVE AL-UNNON TO EDAYSA
04344         MOVE PI-CDAYS            TO EDAYSO PI-EDAYS.
04345
04346      MOVE PI-CPYAMT              TO CPYAMTO.
04347
04348      IF PI-EPYAMT = ZEROS
04349         MOVE AL-UNNON            TO EPYAMTA
04350         MOVE PI-CPYAMT           TO EPYAMTO  PI-EPYAMT.
04351
04352      MOVE PI-CRESV               TO CRESVO.
04353      MOVE PI-CEXPENS             TO CEXPENSO.
04354
04355      IF PI-EEXPENS = ZEROS
04356         MOVE AL-UNNON TO EEXPENSA
04357         MOVE PI-CEXPENS          TO EEXPENSO  PI-EEXPENS.
04358
062121     IF (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
022106        AND (CL-CLAIM-TYPE = 'L')
110807        AND (PI-OFFLINE NOT EQUAL 'Y')
022106        IF PI-PMTTYPE = '2' OR '4'
022106           PERFORM 2200-CALC-INTEREST
022106                                 THRU 2200-EXIT
022106        END-IF
022106     END-IF
04359      MOVE EMI-FORCABLE-CTR       TO PI-FORCE-COUNT WS-FORCE-CTR.
04360      MOVE EMI-FATAL-CTR          TO PI-FATAL-COUNT WS-FATAL-CTR.
04361      MOVE LAST-ERROR-LINE        TO ERRMSG0O.
04362
04363      IF EMI-FATAL-CTR = ZEROS
04364         MOVE 'B'                 TO PI-PASS-SW.
04365
04366      IF NOT EMI-MESSAGE-FORMATTED
04367          MOVE -1                 TO EPYFROML
04368      ELSE
04369          IF EMI-ERROR-NUMBER (1) = '0526' AND
04370             EMI-ERROR-NUMBER (2) = SPACES
04371              MOVE -1             TO EPYFROML.
04372
04373      GO TO 8200-SEND-DATAONLY.
04374
04375      EJECT
       2200-CALC-INTEREST.
071806     IF PI-PROOF-DATE NOT = ZEROS AND LOW-VALUES
071806        MOVE PI-PROOF-DATE       TO DC-GREG-DATE-1-MDY
071806        MOVE '4'                 TO DC-OPTION-CODE
071806        PERFORM 9700-LINK-DATE-CONVERT
071806        IF DATE-CONVERSION-ERROR
071806            MOVE LOW-VALUES      TO CP-PRF-DT
071806        ELSE
071806           MOVE DC-BIN-DATE-1    TO CP-PRF-DT
071806        END-IF
           END-IF
120115     if (pi-int-to-rem-borr <> 'Y' and 'N')
120115        if pi-rb-joint-coverage
120115           move 'Y'              to pi-int-to-rem-borr
120115        end-if
120115     end-if
           MOVE PI-COMPANY-CD          TO CP-COMPANY-CD
              IN CLAIM-INT-PASS-AREA
           MOVE CL-CERT-STATE          TO CP-STATE
              IN CLAIM-INT-PASS-AREA
           MOVE CM-LF-BENEFIT-CD       TO CP-PRODUCT
           MOVE 'LF'                   TO CP-COVERAGE
           MOVE CL-INCURRED-DT         TO CP-INC-DT
           MOVE CL-FILE-ESTABLISH-DT   TO CP-EST-DT
           MOVE LOW-VALUES             TO CP-LSTPD-DT
           MOVE CL-REPORTED-DT         TO CP-RPT-DT
           MOVE CL-CERT-EFF-DT         TO CP-EFF-DT
           MOVE PI-EPYAMT              TO CP-CLAIM-AMT
           
      * EXEC CICS LINK
      *       PROGRAM   ('ELCLMI')
      *       COMMAREA  (CLAIM-INT-PASS-AREA)
      *       LENGTH    (CP-CLAIM-LENGTH)
      *    END-EXEC
           MOVE 'ELCLMI' TO DFHEIV1
      *    MOVE '."C                   (   #00012862' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132383632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-INT-PASS-AREA, 
                 CP-CLAIM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE AL-PANOF               TO PINTHA
                                          PINTA
120115     if cp-clm-int-amt = zeros
120115        move ' ' to pi-int-to-rem-borr
120115     end-if
           IF NO-CI-ERROR
              MOVE CP-CLM-INT-AMT      TO PINTO
                                          PI-INT-AMT
              MOVE CP-CLM-INT-NODAYS   TO PI-INT-DAYS
              MOVE CP-CLM-INT-RATE     TO PI-INT-RATE-USED
              MOVE AL-PANOF            TO PINTHA
                                          PINTA
120115        if pi-rb-joint-coverage
120115           and pi-int-amt > zeros
120115           move '  INT TO'       to itrb1o
120115           move ' REM BORR?'     to itrb2o
120115           move pi-int-to-rem-borr
120115                                 to itrbyno
120115           move al-uanon         to itrbyna
120115           move +1               to itrbynl
120115        else
120115           move ' '              to pi-int-to-rem-borr
120115        end-if
           ELSE
              IF CI-RETURN-CODE = '8'
                 MOVE ER-3818          TO EMI-ERROR
                 MOVE -1               TO ZINTL
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 2200-EXIT
              ELSE
                 MOVE ZEROS            TO PINTO
                                          PI-INT-AMT
                                          PI-INT-DAYS
                                          PI-INT-RATE-USED
              END-IF
           END-IF
           IF PI-INT-AMT NOT = ZEROS
              IF (CL-SOC-SEC-NO (1:9) NUMERIC)
                 AND (CL-SOC-SEC-NO NOT = ZEROS)
                 CONTINUE
              ELSE
                 MOVE CL-SOC-SEC-NO (1:3)
                                       TO WS-SSN (1:3)
                 MOVE CL-SOC-SEC-NO (5:2)
                                       TO WS-SSN (4:2)
                 MOVE CL-SOC-SEC-NO (8:4)
                                       TO WS-SSN (6:4)
                 IF (WS-SSN NUMERIC)
                    AND (WS-SSN NOT = ZEROS)
                    CONTINUE
                 ELSE
                    MOVE ER-2044       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              END-IF
              MOVE ELMSTR-KEY          TO ELTRLR-KEY
120115        if pi-int-to-rem-borr = 'Y'
120115           move +61              to trlr-seq-no
120115        else
120115           MOVE +51              TO TRLR-SEQ-NO
120115        end-if
              
      * EXEC CICS READ
      *          DATASET  ('ELTRLR')
      *          SET      (ADDRESS OF ACTIVITY-TRAILERS)
      *          RIDFLD   (ELTRLR-KEY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00012930' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303132393330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF (NOT WS-RESP-NORMAL)
                          OR
                 ((WS-RESP-NORMAL)
                 AND ((AT-ADDRESS-LINE-1 = SPACES)
                 OR (AT-CITY-STATE = SPACES)))
                 MOVE ER-0874          TO EMI-ERROR
120115           if pi-rb-joint-coverage
120115              move -1            to itrbynl
120115           end-if
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
120115        else
120115           move at-mail-to-name  to pi-int-payees-name
              END-IF
           END-IF
           .
       2200-EXIT.
           EXIT.
061013 2210-dcc-max-benes.
061013
           move pi-ah-term             to ws-max-bens
           if cl-critical-period not = zeros and spaces
              move cl-critical-period  to ws-max-bens
           end-if
061013     move cm-ah-benefit-amt      to ws-monthly-benefit
061013     if pdef-found
              and (pd-max-amt (p1) not = zeros)
100314        if pd-ben-pct (p1) not numeric
100314           move zeros            to pd-ben-pct (p1)
100314        end-if
100314        if pd-ben-pct (p1) = zeros
100314           move +1               to ws-work-ben-pct
100314        else
100314           move pd-ben-pct (p1)  to ws-work-ben-pct
100314        end-if
100314*       compute ws-monthly-benefit =
100314*          ws-monthly-benefit * ws-work-ben-pct
100314*       if ws-monthly-benefit > pd-max-amt (p1)
100314*          move pd-max-amt (p1) to ws-monthly-benefit
100314*       end-if
061013     end-if
061013
061013     move cl-cert-key-data       to elcrtt-key (2:21)
061013     move cl-company-cd          to ctrlr-comp-cd
061013     move cl-cert-no             to ctrlr-cert-no
061013     move 'B'                    to ctrlr-rec-type
061013
061013     
      * EXEC CICS READ
061013*       DATASET  ('ELCRTT')
061013*       set      (address of CERTIFICATE-TRAILERS)
061013*       RIDFLD   (ELCRTT-KEY)
061013*       RESP     (WS-RESPONSE)
061013*    END-EXEC
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00012983' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303132393833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013     IF not WS-RESP-NORMAL
061013        go to 2210-exit
061013     end-if
061013
061013*    if instypei = 'P'
061013*       move +1 to s1
061013*    else
061013*       MOVE +2 to s1
061013*    end-if
061013
061013*    move +1 to s1
061013*    evaluate cl-claim-type
061013*       when 'A'
061013*          move +1 to s2
061013*       when 'I'
061013*          move +2 to s2
061013*       when 'G'
061013*          move +3 to s2
061013*       when 'L'
061013*          move +4 to s2
061013*       when 'P'
061013*          move +5 to s2
061013*    end-evaluate
061013
           move zeros                  to ws-prev-days-paid
                                          ws-benefits-paid
                                          ws-prev-amt-paid
           perform varying s1 from +1 by +1 until
              (s1 > +24)
              or (cs-claim-type (s1) = spaces)
120115        if pi-company-id not = 'DCC' and 'VPP'
                 if cs-claim-type (s1) = cl-claim-type
                    compute ws-prev-days-paid =
                       ws-prev-days-paid + cs-days-paid (s1)
                    compute ws-prev-amt-paid =
                       ws-prev-amt-paid + cs-total-paid (s1)
                 end-if
              else
                 if (cs-claim-type (s1) = cl-claim-type)
                    and (cs-benefit-period (s1) = cl-benefit-period)
                    and (cs-insured-type (s1) = cl-insured-type)
                    compute ws-prev-days-paid =
                       ws-prev-days-paid + cs-days-paid (s1)
                    compute ws-prev-amt-paid =
                       ws-prev-amt-paid + cs-total-paid (s1)
                 end-if
              end-if
           end-perform
           if pi-epyamt > zeros
              compute ws-benefits-paid =
                 (ws-prev-amt-paid + pi-epyamt)
                 / ws-monthly-benefit
           else
              compute ws-benefits-paid =
                 (ws-prev-amt-paid + pi-cpyamt)
                 / ws-monthly-benefit
           end-if
030515*    if pi-company-id = 'DCC'
100518     if cl-claim-type not = 'L' and 'O' and 'P'
              if ws-benefits-paid > ws-max-bens
061013           move er-1658          to emi-error
061013           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061013        end-if
           end-if
030515*    end-if
      *     if s1 < +25
061013*        if pi-epyamt > zeros
061013*           compute ws-benefits-paid =
061013*              (cs-total-paid (s1) + PI-EPYAMT)
061013*              / ws-monthly-benefit
061013*        else
061013*           compute ws-benefits-paid =
061013*              (cs-total-paid (s1) + PI-CPYAMT)
061013*              / ws-monthly-benefit
061013*        end-if
061013
061013*       if ws-benefits-paid > pd-crit-period (p1)
      *        if ws-benefits-paid > ws-max-bens
061013*           move er-1658          to emi-error
061013*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061013*        end-if
      *     end-if
061013     .
061013 2210-exit.
061013     exit.
061013
061013 2220-update-elcrtt.
061013
061013     
      * EXEC CICS GETMAIN
061013*       SET      (ADDRESS OF CERTIFICATE-TRAILERS)
061013*       LENGTH   (ELCRTT-LENGTH)
061013*       INITIMG  (GETMAIN-SPACE)
061013*    END-EXEC
      *    MOVE ',"IL                  $   #00013077' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133303737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCRTT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     move cl-cert-key-data       to elcrtt-key (2:21)
061013     move cl-company-cd          to ctrlr-comp-cd
061013     move cl-cert-no             to ctrlr-cert-no
061013     move 'B'                    to ctrlr-rec-type
061013
061013     
      * EXEC CICS READ
061013*       UPDATE
061013*       DATASET  (ELCRTT-DSID)
061013*       into     (CERTIFICATE-TRAILERS)
061013*       RIDFLD   (ELCRTT-KEY)
061013*       RESP     (WS-RESPONSE)
061013*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00013088' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303133303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     IF WS-RESP-NORMAL
061013        perform 2225-upd-crt-trlr thru 2225-exit
061013        
      * EXEC CICS REWRITE
061013*          DATASET  (ELCRTT-DSID)
061013*          from     (CERTIFICATE-TRAILERS)
061013*          RESP     (WS-RESPONSE)
061013*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00013098' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303133303938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013     else
061013        if ws-resp-notfnd
061013           move 'CS'             to certificate-trailers
061013           move cl-company-cd    to cs-company-cd
061013           move cl-cert-key-data to cs-control-primary (2:21)
061013           move cl-cert-no       to cs-cert-no
061013           move 'B'              to cs-trailer-type
061013           perform varying s1 from +1 by +1 until s1 > +24
061013              move zeros         to cs-benefit-period (s1)
061013                                    cs-days-paid (s1)
061013                                    cs-total-paid (s1)
                                          cs-remaining-bens (s1)
061013           end-perform
061013           perform 2225-upd-crt-trlr
061013                                 thru 2225-exit
061013           
      * EXEC CICS WRITE
061013*             DATASET  (ELCRTT-DSID)
061013*             from     (CERTIFICATE-TRAILERS)
061013*             RIDFLD   (cs-control-primary)
061013*             RESP     (WS-RESPONSE)
061013*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00013118' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303133313138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 cs-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013        end-if
061013     END-IF
061013
061013     .
061013 2220-exit.
061013     exit.
061013
061013 2225-upd-crt-trlr.
061013
061013*    if instypei = 'P'
061013*       move +1 to s1
061013*    else
061013*       MOVE +2 to s1
061013*    end-if
061013
061013*    move +1 to s1
061013*    evaluate cl-claim-type
061013*       when 'A'
061013*          move +1 to s2
061013*       when 'I'
061013*          move +2 to s2
061013*       when 'G'
061013*          move +3 to s2
061013*       when 'L'
061013*          move +4 to s2
061013*       when 'P'
061013*          move +5 to s2
061013*    end-evaluate
           perform varying s1 from +1 by +1 until
              (s1 > +24)
              or (cl-claim-no = cs-claim-no (s1))
           end-perform
           if s1 < +25
061013        compute cs-total-paid (s1) =
                 cs-total-paid (s1) + at-amount-paid
061013        compute cs-days-paid (s1) =
                 cs-days-paid (s1) +  at-days-in-period
      *       compute cs-remaining-bens (s1) =
      *          cs-remaining-bens (s1) - (cs-days-paid (s1) / 30)
              perform 2230-accum-bens-paid thru 2230-exit
           end-if
061013
061013     .
061013 2225-exit.
061013     exit.
       2230-accum-bens-paid.
100518     if cl-claim-type not = 'L' and 'O' and 'P'
              move pi-ah-term          to ws-max-bens
              if cl-critical-period not = zeros and spaces
                 move cl-critical-period
                                       to ws-max-bens
              end-if
           else
              move 01                  to ws-max-bens
           end-if
           move zeros to ws-tot-days-paid ws-tot-amt-paid
           perform varying s2 from +1 by +1 until
              (s2 > +24)
              or (cs-claim-no (s2) = spaces)
120115        if pi-company-id not = 'DCC' and 'VPP'
                 if cs-claim-type (s2) = cl-claim-type
                    compute ws-tot-days-paid =
                       ws-tot-days-paid + cs-days-paid (s2)
                    compute ws-tot-amt-paid =
                       ws-tot-amt-paid + cs-total-paid (s2)
                 end-if
              else
                 if (cs-benefit-period (s2) = cl-benefit-period)
                    and (cs-claim-type (s2) = cl-claim-type)
                    and (cs-insured-type (s2) = cl-insured-type)
                    compute ws-tot-days-paid =
                       ws-tot-days-paid + cs-days-paid (s2)
                    compute ws-tot-amt-paid =
                       ws-tot-amt-paid + cs-total-paid (s2)
                 end-if
              end-if
           end-perform
100518     if cl-claim-type = 'L' or 'P' or 'O'
              if ws-tot-amt-paid > zeros
                 move zeros            to cs-remaining-bens (s1)
              else
                 move 01               to cs-remaining-bens (s1)
              end-if
           else
              compute ws-pd-bens rounded =
                 ws-tot-amt-paid / pi-ah-benefit-amt
              compute cs-remaining-bens (s1) =
                 ws-max-bens - ws-pd-bens
              if cs-remaining-bens (s1) < zeros
                 move zeros            to cs-remaining-bens (s1)
              end-if
           end-if
           .
       2230-exit.
           exit.
04376  3000-MOVE-INPUT-TO-SAVE-AREA.
04377      IF PMTTYPEL > 0
04378          MOVE PMTTYPEI           TO WS-PMTTYPE.
04379
120115     if ws-pmttype = '4'
120115        if pi-rb-joint-coverage
120115           if payeel = +0
120115              move +2            to payeel
120115              move al-uanon      to payeea
120115              move 'Q1'          to payeeo
120115           else
120115              if payeei not = 'Q1' and 'q1'
120115                 move er-1582    to emi-error
120115                 PERFORM 9900-ERROR-FORMAT thru 9900-exit
120115              end-if
120115           end-if
120115        end-if
120115     end-if
04380      MOVE SPACES                 TO PI-PROVISIONAL-IND.
04381
04382      IF WS-PMTTYPE = '9'
04383          MOVE 'P'                TO PI-PROVISIONAL-IND
04384          MOVE '1'                TO WS-PMTTYPE.
04385
04386      IF PAYEEL > 0
               MOVE FUNCTION UPPER-CASE(PAYEEI) TO PAYEEI
04387          MOVE PAYEEI             TO WS-PAYEE.
04388
04389      IF NOTE1L > +0
04390          MOVE NOTE1I             TO WS-PMTNOTE1
04391                                     WS-FLI-PMTNOTE
04392                                     WS-HAN-PMTNOTE.
04393
04394      IF NOTE2L > +0
04395          MOVE NOTE2I             TO WS-PMTNOTE2.
04396
04397      IF OFFLINEL > 0
04398          MOVE OFFLINEI           TO WS-OFFLINE.
052506
052506     IF PROOFDTL > 0
052506         MOVE PROOFDTI            TO DATE-WORK
052506         PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT
052506         MOVE NUM-WORK            TO DC-GREG-DATE-1-MDY
052506         MOVE '4'                 TO DC-OPTION-CODE
052506         PERFORM 9700-LINK-DATE-CONVERT THRU  9700-EXIT
052506         IF NO-CONVERSION-ERROR
052506            MOVE NUM-WORK         TO PROOFDTO
052506            INSPECT PROOFDTI REPLACING ALL ' ' BY '/'
052506            MOVE NUM-WORK         TO WS-PROOF-DATE
052506         ELSE
052506            MOVE ZEROS            TO WS-PROOF-DATE
052506         END-IF
052506     END-IF.
04399
           IF EOBYNL > +0
013013         MOVE FUNCTION UPPER-CASE(EOBYNI) TO EOBYNI
013013         MOVE EOBYNI              TO WS-PRINT-EOB-YN
           END-IF
020413
020413     IF CLMFMYNL > +0
020413         MOVE FUNCTION UPPER-CASE(CLMFMYNI) TO CLMFMYNI
020413         MOVE CLMFMYNI            TO WS-PRINT-CLM-FRM-YN
020413     END-IF
020413
020413     IF SURVYYNL > +0
020413         MOVE FUNCTION UPPER-CASE(SURVYYNI) TO SURVYYNI
020413         MOVE SURVYYNI            TO WS-PRINT-SURVEY-YN
020413     END-IF
102413
102413     IF SPRELYNL > +0
102413         MOVE FUNCTION UPPER-CASE(SPRELYNI) TO SPRELYNI
102413         MOVE SPRELYNI            TO WS-SPECIAL-RELEASE-YN
102413     END-IF
120115     if itrbynl <> +0
120115        move function upper-case(itrbyni) to itrbyni
120115        move itrbyni             to ws-int-to-rem-borr
120115     end-if
020413*    IF GROUPEDL > +0
020413*        MOVE GROUPEDI           TO WS-GROUPED.
020413*
020413*    IF CASHL > +0
020413*       MOVE CASHI               TO WS-CASH.
04406      IF LOANNOL > +0
04407          MOVE LOANNOI            TO WS-LOAN-NO.
04408
04409      IF CHECKNOL > 0
04410          MOVE CHECKNOI           TO WS-CHECKNO, WS-AIG-CHECKNO.
04411
04412      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
04413          IF WS-AIG-CREDIT-ENTERED
04414              MOVE WS-A-CHECKNO-6-7 TO WS-A-CHECKNO-1-2
04415              MOVE ZEROS            TO WS-A-CHECKNO-3-7
04416              MOVE WS-AIG-CHECKNO   TO WS-CHECKNO CHECKNOO
04417              MOVE AL-UANON         TO CHECKNOA.
04418
04419      IF HOLDTILL > 0
04420          MOVE HOLDTILI           TO DATE-WORK
04421          PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT
04422          MOVE NUM-WORK           TO WS-HOLDTIL.
04423
           IF ZINTL > +0
              
      * EXEC CICS BIF DEEDIT
      *           FIELD   (ZINTI)
      *           LENGTH  (07)
      *       END-EXEC
           MOVE 07
             TO DFHEIV11
      *    MOVE '@"L                   #   #00013320' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303133333230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ZINTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF ZINTI NOT NUMERIC
                 MOVE ER-0419          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO ZINTA
                 MOVE -1               TO ZINTL
              ELSE
                 MOVE ZINTI            TO CP-INT-RATE
                                          WS-INT-RATE
                 MOVE CP-INT-RATE      TO ZINTO
                 MOVE AL-UNNON         TO ZINTA
              END-IF
           ELSE
              MOVE ZEROS               TO CP-INT-RATE
           END-IF
04424      IF EPYFROML > 0
04425          MOVE EPYFROMI           TO DATE-WORK
04426          PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT
04427          MOVE NUM-WORK           TO WS-EPYFROM.
04428
04429      IF AIGFROML > 0
04430          MOVE AIGFROMI           TO DATE-WORK
04431          PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT
04432          MOVE NUM-WORK           TO WS-AIGFROM
04433      ELSE
04434          IF (AIGFROMI = LOW-VALUES) AND
04435             (PI-AIGFROM NOT = ZEROS)
04436               MOVE 'Y' TO WS-RECALC-PAYFROM-SW.
04437
04438      IF EPYTHRUL > +0
04439         MOVE EPYTHRUI           TO DATE-WORK
04440         PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT
04441         MOVE NUM-WORK           TO WS-EPYTHRU
04442         MOVE WS-EPYTHRU         TO DC-GREG-DATE-1-MDY
04443         MOVE '4'                TO DC-OPTION-CODE
04444         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04445         IF NO-CONVERSION-ERROR
04446            IF PI-USES-PAID-TO
04447               MOVE '6'                TO DC-OPTION-CODE
04448               MOVE -1                 TO DC-ELAPSED-DAYS
04449               MOVE +0                 TO DC-ELAPSED-MONTHS
04450               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04451               IF NO-CONVERSION-ERROR
04452                  MOVE DC-BIN-DATE-2   TO DC-BIN-DATE-1
04453                  MOVE ' '             TO DC-OPTION-CODE
04454                  MOVE +0              TO DC-ELAPSED-DAYS
04455                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04456               IF NO-CONVERSION-ERROR
04457                  MOVE DC-GREG-DATE-1-MDY TO WS-EPYTHRU.
04458
04459 *****DEEDIT DAYS AND ADD ZERO IN ORDER TO SET A 'C' SIGN
04460 *****THAT WILL BE NEEDED LATER FOR A GROUP TEST.
04461      IF EDAYSL > 0
04462          
      * EXEC CICS BIF DEEDIT
04463 *            FIELD(EDAYSI)
04464 *            LENGTH(6)
04465 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00013376' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303133333736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EDAYSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04466          MOVE EDAYSI             TO WS-DY-WORK
04467          IF WS-DY-SIGN = SPACES
04468             MOVE WS-DY-NUM       TO WS-EDAYS
04469             ADD 0                TO WS-EDAYS
04470          ELSE
04471             MOVE EDAYSI          TO WS-EDAYS
04472             ADD 0                TO WS-EDAYS.
04473
04474 *****DEEDIT PAYMENT AND ADD ZERO IN ORDER TO SET A 'C' SIGN
04475 *****THAT WILL BE NEEDED LATER FOR A GROUP TEST.
04476      IF EPYAMTL > 0
04477          
      * EXEC CICS BIF DEEDIT
04478 *            FIELD(EPYAMTI)
04479 *            LENGTH(10)
04480 *        END-EXEC
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00013391' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303133333931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EPYAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04481          MOVE EPYAMTI            TO WS-PY-WORK
04482          IF WS-PY-SIGN = SPACES
04483             MOVE WS-PY-NUM       TO WS-EPYAMT
04484             ADD 0                TO WS-EPYAMT
04485          ELSE
04486             MOVE EPYAMTI         TO WS-EPYAMT
04487             ADD 0                TO WS-EPYAMT.
04488
04489      IF ERESVL > 0
04490          
      * EXEC CICS BIF DEEDIT
04491 *            FIELD(ERESVI)
04492 *            LENGTH(9)
04493 *        END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00013404' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERESVI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04494          MOVE ERESVI TO WS-ERESV.
04495
04496      IF EEXPENSL > 0
04497          
      * EXEC CICS BIF DEEDIT
04498 *            FIELD(EEXPENSI)
04499 *            LENGTH(8)
04500 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00013411' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EEXPENSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04501          MOVE EEXPENSI           TO WS-EEXPENS.
04502
04503      IF ETYPEL > ZEROS
04504          MOVE ETYPEI             TO WS-ETYPE.
04505
04506      IF PI-COMPANY-ID = 'BOA'
04507         IF PINTL > ZEROS
04508            
      * EXEC CICS BIF DEEDIT
04509 *            FIELD (PINTI)
04510 *            LENGTH(05)
04511 *          END-EXEC
           MOVE 05
             TO DFHEIV11
      *    MOVE '@"L                   #   #00013422' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PINTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04512            IF PINTI NUMERIC
04513                MOVE AL-UNNON     TO PINTA
04514                MOVE PINTI        TO WS-INT-RATE
04515                MOVE WS-INT-RATE  TO PINTO.
04516
04517  3000-EXIT.
04518      EXIT.
04519
04520      EJECT
04521  3050-REBUILD-ENTERED.
04522
04523      MOVE PI-SAVE-INPUT TO WS-SAVE-INPUT.
04524
04525      IF PI-PROV-PMT
04526          MOVE '9'           TO PMTTYPEO
04527      ELSE
04528          MOVE PI-PMTTYPE    TO PMTTYPEO.
04529
04530      MOVE PI-PAYEE          TO PAYEEO.
04531      MOVE PI-PMTNOTE1       TO NOTE1O.
04532      MOVE PI-PMTNOTE2       TO NOTE2O.
04533      MOVE PI-OFFLINE        TO OFFLINEO.
052506     IF PI-PROOF-DATE NOT = ZEROS
052506         MOVE PI-PROOF-DATE TO PROOFDTO
052506         INSPECT PROOFDTI REPLACING ALL SPACES BY '/'
052506     END-IF.
04534 *    MOVE PI-GROUPED        TO GROUPEDO.
04535
04536 *    MOVE PI-CASH           TO CASHO.
04537
04538      IF PI-INT-RATE > ZEROS
04539         MOVE PI-INT-RATE         TO PINTO
           end-if
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
              IF PI-INT-AMT > ZEROS
                 MOVE PI-INT-AMT       TO PINTO
                 MOVE AL-PANOF         TO PINTA
                                          PINTHA
120115           if pi-rb-joint-coverage
120115              move '  INT TO'    to itrb1o
120115              move ' REM BORR?'  to itrb2o
120115              move 'Y'           to itrbyno
120115              move al-uanon      to itrbyna
120115              move +1            to itrbynl
120115           end-if
              END-IF
           END-IF
           MOVE PI-PRINT-EOB-YN        TO EOBYNI
020413     MOVE PI-PRINT-CLM-FRM-YN    TO CLMFMYNI
020413     MOVE PI-PRINT-SURVEY-YN     TO SURVYYNI
102413     MOVE PI-SPECIAL-RELEASE-YN  TO SPRELYNI
04541      IF PI-HOLDTIL NOT = ZEROS
04542         MOVE PI-HOLDTIL          TO HOLDTILO
04543         INSPECT HOLDTILO REPLACING ALL SPACES BY '/'.
04544
04545      IF PI-EPYFROM NOT = ZEROS
04546         MOVE PI-EPYFROM          TO EPYFROMO
04547         INSPECT EPYFROMO REPLACING ALL SPACES BY '/'.
04548
04549      IF PI-AIGFROM NOT = ZEROS
04550         MOVE PI-AIGFROM          TO AIGFROMO
04551         INSPECT AIGFROMO REPLACING ALL SPACES BY '/'.
04552
04553      IF PI-EPYTHRU NOT = ZEROS
04554         MOVE PI-EPYTHRU          TO EPYTHRUO
04555         INSPECT EPYTHRUO REPLACING ALL SPACES BY '/'
04556         IF PI-USES-PAID-TO
04557            MOVE PI-EPYTHRU         TO DC-GREG-DATE-1-MDY
04558            MOVE '4'                TO DC-OPTION-CODE
04559            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04560            IF NO-CONVERSION-ERROR
04561               MOVE +1 TO DC-ELAPSED-DAYS
04562               MOVE +0 TO DC-ELAPSED-MONTHS
04563               MOVE '6' TO DC-OPTION-CODE
04564               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04565               IF NO-CONVERSION-ERROR
04566                  MOVE DC-GREG-DATE-1-EDIT TO EPYTHRUI.
04567
04568      IF PI-EDAYS NOT = ZEROS
04569         MOVE PI-EDAYS TO EDAYSO.
04570
04571      IF PI-EPYAMT NOT = ZEROS
04572         MOVE PI-EPYAMT TO EPYAMTO.
04573
04574      IF PI-ERESV NOT = ZEROS
04575         MOVE PI-ERESV TO ERESVO.
04576
04577      IF PI-EEXPENS NOT = ZEROS
04578         MOVE PI-EEXPENS TO EEXPENSO.
04579
04580      MOVE PI-ETYPE TO ETYPEO.
04581
04582      IF PI-ECHECKNO NOT = SPACES
04583         MOVE PI-ECHECKNO         TO CHECKNOO
04584      ELSE
04585         MOVE PI-CCHECKNO         TO CHECKNOO.
04586
04587      MOVE PI-CPYFROM             TO DC-BIN-DATE-1.
04588      MOVE SPACES                 TO DC-OPTION-CODE.
04589      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04590
04591      IF DATE-CONVERSION-ERROR
04592         MOVE SPACES              TO CPYFROMI
04593      ELSE
04594         MOVE DC-GREG-DATE-1-EDIT TO CPYFROMI.
04595
04596      IF PI-USES-PAID-TO
04597         MOVE PI-CPYTHRU          TO DC-BIN-DATE-1
04598         MOVE +1                  TO DC-ELAPSED-DAYS
04599         MOVE +0                  TO DC-ELAPSED-MONTHS
04600         MOVE '6'                 TO DC-OPTION-CODE
04601         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04602         IF NO-CONVERSION-ERROR
04603            MOVE DC-GREG-DATE-1-EDIT TO CPYTHRUI
04604            GO TO 3051-CONTINUE.
04605
04606      MOVE PI-CPYTHRU             TO DC-BIN-DATE-1.
04607      MOVE SPACES                 TO DC-OPTION-CODE.
04608      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04609
04610      IF DATE-CONVERSION-ERROR
04611         MOVE SPACES              TO CPYTHRUI
04612      ELSE
04613         MOVE DC-GREG-DATE-1-EDIT TO CPYTHRUI.
04614
04615  3051-CONTINUE.
04616
04617      IF PI-CDAYS NOT = ZEROS
04618         MOVE PI-CDAYS TO CDAYSO.
04619
04620      IF PI-CPYAMT NOT = ZEROS
04621         MOVE PI-CPYAMT TO CPYAMTO.
04622
04623      IF PI-CRESV NOT = ZEROS
04624         MOVE PI-CRESV TO CRESVO.
04625
04626      IF PI-CEXPENS NOT = ZEROS
04627         MOVE PI-CEXPENS TO CEXPENSO.
04628
04629  3059-EXIT.
04630      EXIT.
04631
04632  3100-DEEDIT-DATE.
04633      
      * EXEC CICS BIF DEEDIT
04634 *        FIELD(DATE-WORK)
04635 *        LENGTH(8)
04636 *    END-EXEC.
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00013569' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303133353639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DATE-WORK, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04637
04638  3100-EXIT.
04639      EXIT.
04640
04641  3200-CALC-AIG-DAYS.
04642
04643      MOVE 'N'                    TO WS-DUE-DAY-SW.
04644
04645      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.
04646      MOVE ' '                    TO DC-OPTION-CODE.
04647      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04648      IF NO-CONVERSION-ERROR
04649          MOVE DC-GREG-DATE-1-MDY     TO WS-TEMP-INC-MDY
04650      ELSE
04651          GO TO 3290-ERROR.
04652
04653      IF WS-SV-AIGFROM NOT = LOW-VALUES
04654          MOVE WS-SV-AIGFROM  TO DC-BIN-DATE-1
04655                                 WS-BIN-FROM-DT
04656      ELSE
04657          IF CL-TOTAL-PAID-AMT > ZEROS
04658              IF WS-SV-EPYFROM = LOW-VALUES
04659                  MOVE PI-CPYFROM     TO DC-BIN-DATE-1
04660                                         WS-BIN-FROM-DT
04661              ELSE
04662                  MOVE WS-SV-EPYFROM  TO DC-BIN-DATE-1
04663                                         WS-BIN-FROM-DT
04664          ELSE
04665              MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
04666                                         WS-BIN-FROM-DT.
04667
04668  3210-CONTINUE.
04669
04670      MOVE ' '                    TO DC-OPTION-CODE.
04671      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04672      IF DATE-CONVERSION-ERROR
04673          GO TO 3290-ERROR.
04674
04675      MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY.
04676
04677      IF PI-LOAN-DUE-DAY = 31
04678          MOVE 30              TO WS-PMT-DUE-DA
04679                                  WS-SAVE-DUE-DA
04680      ELSE
04681          MOVE PI-LOAN-DUE-DAY TO WS-PMT-DUE-DA
04682                                  WS-SAVE-DUE-DA.
04683
04684      IF (PI-LOAN-DUE-DAY > 28) AND
04685         (WS-PMT-DUE-MO = 2)
04686          MOVE 'Y'                TO WS-DUE-DAY-SW
04687          MOVE 28                 TO WS-PMT-DUE-DA.
04688
04689      MOVE WS-PMT-DUE-MDY         TO DC-GREG-DATE-1-MDY.
04690      MOVE '4'                    TO DC-OPTION-CODE.
04691      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04692      IF NO-CONVERSION-ERROR
04693          MOVE DC-BIN-DATE-1      TO WS-PMT-DUE-DT
04694      ELSE
04695          GO TO 3290-ERROR.
04696
04697 *--------------------------------------------------------------*
04698 *    IF THE 1ST LOAN PMT DATE LESS THAN INCURRED DATE          *
04699 *    AND THE EFFECTIVE DAY LESS THAN INCURRED DAY              *
04700 *    ADD ONE MONTH TO THE CURRENT PAYMENT DUE DATE             *
04701 *                                                              *
04702 *    EXAMPLE: CERT LOAN PMT DUE  - 05/03/87                    *
04703 *             INCURRED DATE      - 04/15/91                    *
04704 *             PMT DUE DATE       - 04/03/91                    *
04705 *             PMT DUE DATE       - 05/03/91                    *
04706 *--------------------------------------------------------------*
04707
04708      IF (CL-TOTAL-PAID-AMT NOT > ZEROS)  AND
04709         (WS-SV-AIGFROM = LOW-VALUES)
04710          IF (WS-PMT-DUE-MO = WS-TEMP-INC-MO) AND
04711             (WS-PMT-DUE-DA < WS-TEMP-INC-DA)
04712              MOVE WS-PMT-DUE-DT      TO DC-BIN-DATE-1
04713              PERFORM 3300-BUMP-DATE THRU 3399-EXIT
04714              IF NO-CONVERSION-ERROR
04715                  MOVE DC-BIN-DATE-2      TO WS-PMT-DUE-DT
04716                  MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY
04717              ELSE
04718                  GO TO 3290-ERROR.
04719
04720      IF (CL-TOTAL-PAID-AMT NOT > ZEROS)  AND
04721         (WS-SV-AIGFROM = LOW-VALUES)
04722          GO TO 3205-CONT-CALC-PMT.
04723
04724      MOVE WS-BIN-FROM-DT         TO DC-BIN-DATE-1.
04725      MOVE ' '                    TO DC-OPTION-CODE.
04726      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04727      IF NO-CONVERSION-ERROR
04728          MOVE DC-GREG-DATE-1-MDY     TO WS-FROM-DATE-MDY
04729      ELSE
04730          GO TO 3290-ERROR.
04731
04732      IF (WS-PMT-DUE-MO = WS-FROM-MO) AND
04733         (WS-PMT-DUE-DA < WS-FROM-DA)
04734           MOVE WS-PMT-DUE-DT      TO DC-BIN-DATE-1
04735           PERFORM 3300-BUMP-DATE THRU 3399-EXIT
04736           IF NO-CONVERSION-ERROR
04737               MOVE DC-BIN-DATE-2      TO WS-PMT-DUE-DT
04738               MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY
04739           ELSE
04740               GO TO 3290-ERROR.
04741
04742  3205-CONT-CALC-PMT.
04743
04744 *--------------------------------------------------------------*
04745 *    VERIFY THAT THE SYSTEM CALCULATED LOAN PAYMENT DATE FALLS *
04746 *    BETWEEN THE PAY FROM / THRU DATES.                        *
04747 *--------------------------------------------------------------*
04748
04749      IF CL-TOTAL-PAID-AMT = ZEROS AND
04750         PI-PMTTYPE = '2'
04751           NEXT SENTENCE
04752      ELSE
04753         IF (WS-PMT-DUE-DT < WS-BIN-FROM-DT) OR
04754            (WS-PMT-DUE-DT > WS-SV-EPYTHRU)
04755              MOVE WS-PMT-DUE-MDY     TO EMI-DATE-FIELD
04756              MOVE ER-3527            TO EMI-ERROR
04757              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04758              MOVE -1                 TO EPYTHRUL
04759              MOVE AL-UNBON           TO EPYTHRUA
04760                                         EPYFROMA
04761              GO TO 3299-EXIT.
04762
04763 *--------------------------------------------------------------*
04764 *    CALCULATE RETRO ELIMINATION DATE                          *
04765 *--------------------------------------------------------------*
04766
04767      IF WS-SV-AIGFROM NOT = LOW-VALUES
04768          MOVE WS-SV-AIGFROM  TO DC-BIN-DATE-1
04769      ELSE
04770          MOVE CL-INCURRED-DT TO DC-BIN-DATE-1.
04771
04772      MOVE PI-BEN-DAYS    TO DC-ELAPSED-DAYS.
04773      MOVE +0             TO DC-ELAPSED-MONTHS.
04774      MOVE '6'            TO DC-OPTION-CODE.
04775      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04776      IF NO-CONVERSION-ERROR
04777          MOVE DC-BIN-DATE-2 TO WS-RETRO-ELIM-DT
04778      ELSE
04779          GO TO 3290-ERROR.
04780
04781  3210-CHECK-PMT-DATE.
04782
04783 *--------------------------------------------------------------*
04784 *    VERIFY THAT THE CALCULATED LOAN DUE DATE FALLS AFTER      *
04785 *    THE ENTERED PAY FROM DATE                                 *
04786 *--------------------------------------------------------------*
04787
04788      IF WS-PMT-DUE-DT < WS-SV-EPYFROM
04789          NEXT SENTENCE
04790      ELSE
04791          GO TO 3220-COMPUTE-DAYS.
04792
04793      MOVE WS-PMT-DUE-DT      TO DC-BIN-DATE-1.
04794      PERFORM 3300-BUMP-DATE THRU 3399-EXIT.
04795      IF NO-CONVERSION-ERROR
04796          MOVE DC-BIN-DATE-2      TO WS-PMT-DUE-DT
04797          MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY
04798          GO TO 3210-CHECK-PMT-DATE
04799      ELSE
04800          GO TO 3290-ERROR.
04801
04802  3220-COMPUTE-DAYS.
04803
04804      MOVE WS-PMT-DUE-DT TO WS-NEXT-PMT-DUE-DT.
04805      MOVE ZEROS         TO WS-CDAYS.
04806      MOVE 'N'           TO WS-ELIMINATION-SW.
04807
04808 *--------------------------------------------------------------*
04809 *    COMPUTE CALIFORNIA DUE DAYS                               *
04810 *--------------------------------------------------------------*
04811
04812      IF CL-TOTAL-PAID-AMT > ZEROS
04813          MOVE 'Y' TO WS-ELIMINATION-SW
04814          GO TO 3240-FIRST-PMT-DUE
04815      ELSE
04816         IF WS-RETRO-ELIM-DT > WS-PMT-DUE-DT
04817             NEXT SENTENCE
04818         ELSE
04819             GO TO 3240-FIRST-PMT-DUE.
04820
04821  3230-NEXT-PMT-DUE.
04822
04823      MOVE WS-NEXT-PMT-DUE-DT TO DC-BIN-DATE-1.
04824      PERFORM 3300-BUMP-DATE THRU 3399-EXIT.
04825      IF NO-CONVERSION-ERROR
04826          MOVE DC-BIN-DATE-2 TO WS-NEXT-PMT-DUE-DT
04827          MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY
04828      ELSE
04829          GO TO 3290-ERROR.
04830
04831  3240-FIRST-PMT-DUE.
04832
04833      IF CL-TOTAL-PAID-AMT > ZEROS
04834          NEXT SENTENCE
04835      ELSE
04836         IF WS-RETRO-ELIM-DT > WS-NEXT-PMT-DUE-DT
04837             GO TO 3230-NEXT-PMT-DUE.
04838
04839      IF WS-NEXT-PMT-DUE-DT > WS-SV-EPYTHRU
04840          GO TO 3250-VERIFY-ELIMINATION.
04841
04842      ADD +30 TO WS-CDAYS.
04843      MOVE 'Y' TO WS-ELIMINATION-SW.
04844      GO TO 3230-NEXT-PMT-DUE.
04845
04846  3250-VERIFY-ELIMINATION.
04847
04848      IF ELIM-SATISFIED
04849          GO TO 3260-END-CALC.
04850
04851      IF PI-PMTTYPE = '1'
04852          MOVE ZEROS TO WS-CDAYS
04853          GO TO 3260-END-CALC.
04854
04855      IF WS-SV-AIGFROM NOT = LOW-VALUES
04856          MOVE WS-SV-AIGFROM  TO DC-BIN-DATE-1
04857      ELSE
04858          IF CL-TOTAL-PAID-AMT > ZEROS
04859              IF WS-SV-EPYFROM = LOW-VALUES
04860                  MOVE PI-CPYFROM     TO DC-BIN-DATE-1
04861              ELSE
04862                  MOVE WS-SV-EPYFROM  TO DC-BIN-DATE-1
04863          ELSE
04864              MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1.
04865
04866      MOVE WS-SV-EPYTHRU          TO DC-BIN-DATE-2.
04867      MOVE '1'                    TO DC-OPTION-CODE.
04868      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04869      IF NO-CONVERSION-ERROR
04870          COMPUTE WS-DAYS-OFF =  DC-ELAPSED-DAYS + 1
04871      ELSE
04872          GO TO 3290-ERROR.
04873
04874      IF CL-TOTAL-PAID-AMT > ZEROS
04875          MOVE WS-DAYS-OFF TO WS-CDAYS
04876      ELSE
04877          IF PI-BEN-DAYS > WS-DAYS-OFF
04878              MOVE ZEROS TO WS-CDAYS
04879          ELSE
04880              COMPUTE WS-CDAYS = WS-DAYS-OFF - PI-BEN-DAYS.
04881
04882  3260-END-CALC.
04883
04884      MOVE WS-CDAYS TO PI-CDAYS.
04885
04886      IF PI-EDAYS = ZEROS
04887          MOVE PI-CDAYS TO PI-EDAYS
04888          MOVE AL-UNNON TO EDAYSA.
04889
04890      GO TO 3299-EXIT.
04891
04892  3290-ERROR.
04893
04894      MOVE ER-3528            TO EMI-ERROR.
04895      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04896      MOVE -1                 TO CDAYSL.
04897      MOVE AL-UNBON           TO CDAYSA.
04898
04899  3299-EXIT.
04900      EXIT.
04901
04902  3300-BUMP-DATE.
04903
04904      IF WS-RESTORE-DUE-DAY
04905          MOVE 'N'              TO WS-DUE-DAY-SW
04906          MOVE WS-SAVE-DUE-DA   TO WS-PMT-DUE-DA
04907          ADD 1                 TO WS-PMT-DUE-MO
04908          MOVE WS-PMT-DUE-MDY   TO DC-GREG-DATE-1-MDY
04909          MOVE '4'              TO DC-OPTION-CODE
04910          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04911          IF NO-CONVERSION-ERROR
04912              MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY
04913              MOVE DC-BIN-DATE-1      TO DC-BIN-DATE-2
04914              GO TO 3399-EXIT
04915          ELSE
04916              MOVE '9'          TO DC-ERROR-CODE
04917              GO TO 3399-EXIT.
04918
04919      IF (WS-PMT-DUE-MO = 1) AND
04920         (WS-SAVE-DUE-DA > 28)
04921            MOVE 'Y' TO WS-DUE-DAY-SW.
04922
04923      MOVE +0                 TO DC-ELAPSED-DAYS.
04924      MOVE +1                 TO DC-ELAPSED-MONTHS.
04925      MOVE '6'                TO DC-OPTION-CODE.
04926      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04927
04928  3399-EXIT.
04929      EXIT.
04930
04931      EJECT
04932  3400-COMPUTE-EXPIRY.
04933
04934      MOVE CM-LOAN-1ST-PMT-DT    TO DC-BIN-DATE-1.
04935      MOVE +0                    TO DC-ELAPSED-DAYS
04936                                    DC-ODD-DAYS-OVER.
04937      MOVE '1'                   TO DC-END-OF-MONTH.
04938      MOVE '6'                   TO DC-OPTION-CODE.
04939
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
04941          COMPUTE DC-ELAPSED-MONTHS = CM-LF-ORIG-TERM - +1
04942      ELSE
04943          COMPUTE DC-ELAPSED-MONTHS = CM-AH-ORIG-TERM - +1.
04944
04945      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04946      IF NO-CONVERSION-ERROR
04947          MOVE DC-BIN-DATE-2  TO WS-EXP-DT PI-EXP-DT
04948      ELSE
04949          GO TO 3390-CONTINUE.
04950
04951      IF CL-CLAIM-TYPE NOT = (PI-LIFE-OVERRIDE-L1 OR 'O')
04952          GO TO 3390-CONTINUE.
04953
04954      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
04955      MOVE '4'                    TO CNTL-REC-TYPE.
04956      MOVE +0                     TO CNTL-SEQ-NO.
04957      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-CD.
04958      MOVE WS-ACCESS              TO CNTL-ACCESS.
04959      MOVE 'BENE'                 TO FILE-SWITCH.
04960
04961      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
04962
04963      IF NO-BENEFIT-FOUND
04964          GO TO 3390-CONTINUE.
04965
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
PEMMOD        IF (PI-OFFLINE = 'Y') OR
PEMMOD           (PI-PMTTYPE = '7')
CIDMOD           PERFORM 9870-OUTPUT-ACTIVITY-RECORD
PEMMOD                                     THRU 9870-EXIT
CIDMOD           IF ERROR-ON-OUTPUT
CIDMOD             MOVE -1                 TO ENTERPFL
CIDMOD             MOVE AL-UANON           TO ENTERPFA
CIDMOD             MOVE MAP-NAMEA          TO MAP-NAME
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
CIDMOD             GO TO 8200-SEND-DATAONLY.
CIDMOD
100518     IF (CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O')  AND
04967         (CF-CO-EARNINGS-CALC (SUB-1) = 'B' OR 'K' OR 'L') AND
04968         (CF-SPECIAL-CALC-CD  (SUB-1) = 'L')
04969           MOVE WS-EXP-DT TO DC-BIN-DATE-1
04970           MOVE +0        TO DC-ELAPSED-DAYS
04971           MOVE +1        TO DC-ELAPSED-MONTHS
04972           MOVE 6         TO DC-OPTION-CODE
04973           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04974           IF NO-CONVERSION-ERROR
04975               MOVE DC-BIN-DATE-2  TO WS-EXP-DT PI-EXP-DT.
04976
04977  3390-CONTINUE.
04978
121802      IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                           OR 'B' or 'H'
04980          MOVE CM-AH-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
04981       ELSE
04982          MOVE CM-LF-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
            END-IF
04983
04984       MOVE ' '                    TO DC-OPTION-CODE.
04985       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04986       MOVE DC-GREG-DATE-1-MDY     TO WS-TEMP-DATE-MDY.
04987       MOVE WS-TEMP-DA             TO PI-LOAN-DUE-DAY.
04988
04989  3499-EXIT.
04990      EXIT.
04991
04992      EJECT
061511
061511 3600-CHECK-VERIFICATION.
061511
061511     MOVE ELMSTR-KEY             TO ELTRLR-KEY.
061511
061511     MOVE PI-PAYEE-SEQ-NUM       TO TRLR-SEQ-NO.
061511
061511     IF PI-PAYEE-TYPE = 'I'
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'B'
061511        ADD +10                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'A'
061511        ADD +20                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'O'
061511        ADD +50                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'Q'
061511        ADD +60                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'P'
061511        ADD +30                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'E'
061511        ADD +40                  TO TRLR-SEQ-NO.
061511
061511 3600-READ-TRAILER.
061511
061511     
      * EXEC CICS HANDLE CONDITION
061511*         NOTFND(3600-CONTINUE)
061511*    END-EXEC.
      *    MOVE '"$I                   ! - #00013981' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303133393831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
061511
061511     PERFORM 7950-READ-TRAILER THRU 7950-EXIT.
061511
061511 3600-CONTINUE.
032514
100518     IF CL-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
032514         GO TO 3600-AH-VERIFY
032514     END-IF.
061511
032514     IF PI-VFY-2ND-BENE = 'L' OR 'B'
061511       IF AT-MAIL-TO-NAME (1:9) <> 'ESTATE OF'
061511         IF AT-VFY-2ND-BENE-VERIFIED <> 'Y'
061511           MOVE ER-7576            TO EMI-ERROR
061511           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061511           MOVE -1                 TO PAYEEL
061511           MOVE AL-UABON           TO PAYEEA
020513           GO TO 3600-EXIT
061511         END-IF
020513       END-IF
061511     END-IF.
061511
020513     MOVE AT-MAIL-TO-NAME TO WS-WORK-NAME.
020513     MOVE 1 TO WS-SUB2
020513     MOVE SPACES TO WS-PAYEE-NAME
020513     PERFORM VARYING WS-SUB FROM 1 BY 1
020513             UNTIL WS-SUB > 30
020513         IF WS-WORK-NAME-X (WS-SUB) > SPACES
020513            MOVE WS-WORK-NAME-X (WS-SUB) TO
020513                 WS-PAYEE-NAME-X (WS-SUB2)
020513            ADD +1 TO WS-SUB2
020513         END-IF
020513     END-PERFORM.
020513
020513     STRING CL-INSURED-1ST-NAME
020513            CL-INSURED-MID-INIT
020513            CL-INSURED-LAST-NAME INTO WS-WORK-NAME.
020513     MOVE 1 TO WS-SUB2
020513     MOVE SPACES TO WS-INSURED-NAME
020513     PERFORM VARYING WS-SUB FROM 1 BY 1
020513             UNTIL WS-SUB > 30
020513         IF WS-WORK-NAME-X (WS-SUB) > SPACES
020513            MOVE WS-WORK-NAME-X (WS-SUB) TO
020513                 WS-INSURED-NAME-X (WS-SUB2)
020513            ADD +1 TO WS-SUB2
020513         END-IF
020513     END-PERFORM.
020513
100518     IF CL-CLAIM-TYPE = 'O'
100518        IF WS-PAYEE-NAME (1:6) = 'ESTATE'
100518           MOVE ER-7585            TO EMI-ERROR
100518           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100518           MOVE -1                 TO PAYEEL
100518           MOVE AL-UABON           TO PAYEEA
100518        ELSE
100518           GO TO 3600-EXIT
100518        END-IF
100518     END-IF.
020513     IF WS-PAYEE-NAME = WS-INSURED-NAME
020513        MOVE ER-7579            TO EMI-ERROR
020513        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020513        MOVE -1                 TO PAYEEL
020513        MOVE AL-UABON           TO PAYEEA
020513     END-IF.
020513
032414     IF PI-PMTTYPE = '4'  AND
032414        WS-PAYEE-NAME (1:8) = 'ESTATEOF'  AND
032414        PI-PAYEE-TYPE <> 'I'
032414          MOVE ER-7580          TO EMI-ERROR
032414          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
032414          MOVE -1               TO PAYEEL
032414          MOVE AL-UABON         TO PAYEEA
032414     END-IF.
032414
032514     GO TO 3600-EXIT.
032514
032514 3600-AH-VERIFY.
032514
032514     IF PI-VFY-2ND-BENE = 'A' OR 'B'
032514        IF AT-VFY-2ND-BENE-VERIFIED <> 'Y'
032514           MOVE ER-7583            TO EMI-ERROR
032514           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
032514           MOVE -1                 TO PAYEEL
032514           MOVE AL-UABON           TO PAYEEA
032514        END-IF
032514     END-IF.
032514
032514
061511 3600-EXIT.
061511     EXIT.
061511
04993  5000-UPDATE.
04994
04995      PERFORM 7300-CHECK-AUTO-ACTIVITY THRU 7399-EXIT.
04996
04997      PERFORM 6200-UPDATE-CLAIM-MSTR THRU 6299-EXIT.
04998
04999      IF PI-PMTTYPE = '4' AND PI-EPYAMT NEGATIVE  OR
05000         PI-OFFLINE = 'Y'
05001         NEXT SENTENCE
05002      ELSE
05003         PERFORM 6700-UPDATE-ACTQ THRU 6799-EXIT.
05004
121802*    IF PI-COMPANY-ID = 'CRI'
121802*       IF PI-PMTTYPE = '1' OR  '2'
121802*          IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
121802*             PERFORM 6800-BUILD-FORM-TRAILER   THRU 6899-EXIT
121802*             PERFORM 6900-BUILD-ARCHIVE-HEADER THRU 6999-EXIT.
05010
121802*    IF PI-COMPANY-ID = 'RMC' OR 'LAP'
121802*       IF PI-PMTTYPE = '1'
121802*          IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
121802*             PERFORM 6800-BUILD-FORM-TRAILER THRU 6899-EXIT.
05015
05016      IF PI-PMTTYPE      = '2' AND
05017         PI-SPLIT-PMT-SW = 'Y' AND
100518        (CL-CLAIM-TYPE   = PI-LIFE-OVERRIDE-L1 OR 'O')
05019           MOVE ER-3530                TO EMI-ERROR
05020           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05021
062121     IF (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
100518        AND (CL-CLAIM-TYPE = 'L' OR 'O')
022106        IF PI-PMTTYPE = '2' OR '4'
022106           IF PI-INT-AMT NOT = ZEROS
022106              PERFORM 7110-BUILD-PMT-TRLR
022106                                 THRU 7130-EXIT
022106           ELSE
022106              IF PI-PMTTYPE = '2'
022106                 PERFORM 7140-BUILD-NOTE-TRLR
022106                                 THRU 7170-EXIT
022106              END-IF
022106           END-IF
022106        END-IF
022106     END-IF
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*        IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
121802*            MOVE 'N'                 TO  WS-LETTER-SW.
05025
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        IF CL-NO-OF-PMTS-MADE = +1
121802*                OR
121802*           PI-PMTTYPE = '2'
121802*            PERFORM 5300-CREATE-DMO THRU 5300-EXIT.
05031
05032      PERFORM 0800-DELETE-TS THRU 0800-EXIT.
05033
121802*    IF PI-COMPANY-ID = 'DMD'
121802*       IF CL-CERT-NO (5:2) = '69' OR '70'
121802*           PERFORM 6600-UPDATE-DLO035 THRU 6600-EXIT.
05037
05038      
      * EXEC CICS REWRITE
05039 *         DATASET('ELMSTR')
05040 *         FROM(CLAIM-MASTER)
05041 *     END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&& L                  %   #00014133' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303134313333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05042
05043      IF WS-LETTER-SW = 'N'
05044          GO TO 5000-END-UPDATE.
05045
05046  5000-GENERATE-AUTO-LETTER.
05047
05048      
      * EXEC CICS LINK
05049 *        PROGRAM    (LINK-1523)
05050 *        COMMAREA   (W-1523-LINKDATA)
05051 *        LENGTH     (W-1523-COMM-LENGTH)
05052 *    END-EXEC.
      *    MOVE '."C                   (   #00014143' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134313433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-1523, 
                 W-1523-LINKDATA, 
                 W-1523-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05053
05054      IF W-1523-FATAL-ERROR
05055          MOVE W-1523-ERROR-CODE      TO  EMI-ERROR
05056          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05057          MOVE ER-0802                TO  EMI-ERROR
05058          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05059
05060      IF W-1523-ERROR-CODE = 0191
05061          MOVE ER-0803                TO  EMI-ERROR
05062          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05063
05064      IF PI-COMPANY-ID      = 'HAN' AND
05065         PI-PMTTYPE         = '1'   AND
05066         PI-3RD-PARTY-SW    = 'Y'   AND
05067         W-1523-FORM-NUMBER = 'EOB1'
05068            MOVE LOW-VALUES              TO W-1523-LINKDATA
05069            MOVE PROGRAM-INTERFACE-BLOCK TO W-1523-COMMON-PI-DATA
05070            MOVE 'EOB3'                  TO W-1523-FORM-NUMBER
05071            GO TO 5000-GENERATE-AUTO-LETTER.
05072
05073  5000-END-UPDATE.
05074
05075      MOVE ER-0000                TO EMI-ERROR.
05076      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05077      GO TO 7000-SHOW-CLAIM.
05078
05079      EJECT
05080  5000-MOVE-NAME.
05081      MOVE CL-INSURED-LAST-NAME   TO LSTNMEO.
05082      MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.
05083      MOVE CL-INSURED-MID-INIT    TO MINITO.
05084
05085  5000-EXIT.
05086      EXIT.
05087                                  EJECT
05088  5300-CREATE-DMO.
05089
05090      MOVE CL-CERT-KEY-DATA       TO W-NOTE-CERT-KEY.
05091      MOVE PI-COMPANY-CD          TO W-NOTE-COMP-CD.
05092      MOVE CL-CERT-NO             TO W-NOTE-CERT-NO.
05093
05094      
      * EXEC CICS HANDLE CONDITION
05095 *         NOTFND   (5300-NOTE-NOT-FOUND)
05096 *    END-EXEC.
      *    MOVE '"$I                   ! . #00014189' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303134313839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05097
05098      
      * EXEC CICS READ
05099 *         DATASET('ERNOTE')
05100 *         SET    (ADDRESS OF CERTIFICATE-NOTE)
05101 *         RIDFLD (W-NOTE-KEY)
05102 *    END-EXEC.
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&"S        E          (   #00014193' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134313933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05103
05104      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.
05105      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.
05106      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.
05107
05108      IF CL-CERT-GROUPING (5:2) = ZEROS OR SPACES
05109          MOVE 'CC'                   TO DCT-PRODUCT-CODE
05110      ELSE
05111          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.
05112
05113      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.
05114      MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.
05115      MOVE 'DLO006'               TO PGM-NAME.
05116
05117      
      * EXEC CICS LINK
05118 *        PROGRAM    (PGM-NAME)
05119 *        COMMAREA   (DCT-COMMUNICATION-AREA)
05120 *        LENGTH     (DCT-RCRD-LENGTH)
05121 *    END-EXEC.
      *    MOVE '."C                   (   #00014212' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DCT-COMMUNICATION-AREA, 
                 DCT-RCRD-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05122
05123      IF DCT-RETURN-CODE = 'OK'
05124          GO TO 5300-CONT.
05125
05126      IF DCT-RETURN-CODE = '01' OR '02'
05127          GO TO 5300-EXIT.
05128
05129      IF DCT-RETURN-CODE = '03'
05130          MOVE ER-0951            TO EMI-ERROR
05131          GO TO 5300-BAD-EXIT.
05132
05133      IF DCT-RETURN-CODE = '04'
05134          MOVE ER-0946            TO EMI-ERROR
05135          GO TO 5300-BAD-EXIT.
05136
05137      IF DCT-RETURN-CODE = '05'
05138          MOVE ER-0947            TO EMI-ERROR
05139          GO TO 5300-BAD-EXIT.
05140
05141      IF DCT-RETURN-CODE = '06'
05142          MOVE ER-0921            TO EMI-ERROR
05143          GO TO 5300-BAD-EXIT.
05144
05145      IF DCT-RETURN-CODE = '07'
05146          MOVE ER-0849            TO EMI-ERROR
05147          GO TO 5300-BAD-EXIT.
05148
05149      IF DCT-RETURN-CODE = '08'
05150          MOVE ER-0948            TO EMI-ERROR
05151          GO TO 5300-BAD-EXIT.
05152
05153      IF DCT-RETURN-CODE = 'N1'
05154          MOVE ER-0950            TO EMI-ERROR
05155          GO TO 5300-BAD-EXIT.
05156
05157      IF DCT-RETURN-CODE = 'E1'
05158          MOVE ER-0974            TO EMI-ERROR
05159          GO TO 5300-BAD-EXIT.
05160
05161      IF DCT-RETURN-CODE = 'E2'
05162          MOVE ER-0975            TO EMI-ERROR
05163          GO TO 5300-BAD-EXIT.
05164
05165      IF DCT-RETURN-CODE NOT = 'OK'
05166          MOVE ER-0949            TO EMI-ERROR
05167          GO TO 5300-BAD-EXIT.
05168
05169  5300-CONT.
05170
05171      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.
05172      MOVE 'CS'                   TO DM-RECORD-TYPE.
05173      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.
05174      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.
05175      MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.
05176      MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.
05177      MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.
05178      MOVE SAVE-DATE-CCYYMMDD     TO DM-STATUS-DATE.
05179
05180      MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.
05181      MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.
05182      MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.
05183      PERFORM 5350-FORMAT-LAST-NAME-1ST THRU 5350-EXIT.
05184      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.
05185
05186      MOVE CL-CARRIER             TO DM-STAT-CARRIER.
05187
05188      IF PI-PMTTYPE = '2'
05189          MOVE 'F'                TO DM-STAT-CHANGE-TYPE
05190          MOVE '4'                TO DM-CLAIM-STATUS
05191      ELSE
05192          MOVE 'I'                TO DM-STAT-CHANGE-TYPE
05193          MOVE '2'                TO DM-CLAIM-STATUS.
05194
05195      MOVE 'DLO025'               TO PGM-NAME.
05196
05197      
      * EXEC CICS LINK
05198 *        PROGRAM    (PGM-NAME)
05199 *        COMMAREA   (DMO-COMMUNICATION-AREA)
05200 *        LENGTH     (DM-DMO-LENGTH)
05201 *    END-EXEC.
      *    MOVE '."C                   (   #00014292' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DMO-COMMUNICATION-AREA, 
                 DM-DMO-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05202
05203      IF DM-RETURN-CODE = 'OK'
05204          GO TO 5300-EXIT.
05205
05206      IF DM-RETURN-CODE = '01'
05207          MOVE ER-8051            TO EMI-ERROR
05208          GO TO 5300-BAD-EXIT.
05209
05210      IF DM-RETURN-CODE = '02'
05211          MOVE ER-8052            TO EMI-ERROR
05212          GO TO 5300-BAD-EXIT.
05213
05214      IF DM-RETURN-CODE = '03'
05215          MOVE ER-8053            TO EMI-ERROR
05216          GO TO 5300-BAD-EXIT.
05217
05218      IF DM-RETURN-CODE = '04'
05219          MOVE ER-8054            TO EMI-ERROR
05220          GO TO 5300-BAD-EXIT.
05221
05222      IF DM-RETURN-CODE = '05'
05223          MOVE ER-8055            TO EMI-ERROR
05224          GO TO 5300-BAD-EXIT.
05225
05226      IF DM-RETURN-CODE = '06'
05227          MOVE ER-8056            TO EMI-ERROR
05228          GO TO 5300-BAD-EXIT.
05229
05230      IF DM-RETURN-CODE = '07'
05231          MOVE ER-8057            TO EMI-ERROR
05232          GO TO 5300-BAD-EXIT.
05233
05234      IF DM-RETURN-CODE = '08'
05235          MOVE ER-8058            TO EMI-ERROR
05236          GO TO 5300-BAD-EXIT.
05237
05238      IF DM-RETURN-CODE = '09'
05239          MOVE ER-8059            TO EMI-ERROR
05240          GO TO 5300-BAD-EXIT.
05241
05242      IF DM-RETURN-CODE = '10'
05243          MOVE ER-8060            TO EMI-ERROR
05244          GO TO 5300-BAD-EXIT.
05245
05246      IF DM-RETURN-CODE = '11'
05247          MOVE ER-8061            TO EMI-ERROR
05248          GO TO 5300-BAD-EXIT.
05249
05250      IF DM-RETURN-CODE = '12'
05251          MOVE ER-8062            TO EMI-ERROR
05252          GO TO 5300-BAD-EXIT.
05253
05254      IF DM-RETURN-CODE = '13'
05255          MOVE ER-8063            TO EMI-ERROR
05256          GO TO 5300-BAD-EXIT.
05257
05258      IF DM-RETURN-CODE = '14'
05259          MOVE ER-8064            TO EMI-ERROR
05260          GO TO 5300-BAD-EXIT.
05261
05262      IF DM-RETURN-CODE = '15'
05263          MOVE ER-8065            TO EMI-ERROR
05264          GO TO 5300-BAD-EXIT.
05265
05266      IF DM-RETURN-CODE = '16'
05267          MOVE ER-8154            TO EMI-ERROR
05268          GO TO 5300-BAD-EXIT.
05269
05270      IF DM-RETURN-CODE = '17'
05271          MOVE ER-8155            TO EMI-ERROR
05272          GO TO 5300-BAD-EXIT.
05273
05274      IF DM-RETURN-CODE = 'N1'
05275          MOVE ER-8152            TO EMI-ERROR
05276          GO TO 5300-BAD-EXIT.
05277
05278      IF DM-RETURN-CODE = 'E1'
05279          MOVE ER-8153            TO EMI-ERROR
05280          GO TO 5300-BAD-EXIT.
05281
05282      MOVE ER-8066            TO EMI-ERROR.
05283      GO TO 5300-BAD-EXIT.
05284
05285  5300-NOTE-NOT-FOUND.
05286
05287      MOVE ER-0954                TO EMI-ERROR.
05288      GO TO 5300-BAD-EXIT.
05289
05290  5300-BAD-EXIT.
05291      
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC.
      *    MOVE '6"R                   !   #00014386' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303134333836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05292      IF MAP-NAME = 'EL156A'
05293          MOVE -1             TO ENTERPFL
05294        ELSE
05295          MOVE LOW-VALUES     TO EL156BI
05296          MOVE -1             TO ENTPFBL.
05297
05298      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05299      GO TO 8200-SEND-DATAONLY.
05300
05301  5300-EXIT.
05302      EXIT.
05303                                  EJECT
05304  5350-FORMAT-LAST-NAME-1ST.
05305 *****************************************************************
05306 *             M O V E   N A M E   R O U T I N E                 *
05307 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
05308 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
05309 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
05310 *                  FIELD                   VALUE                *
05311 *           W-NAME-LAST    (CL15)      SMITH                    *
05312 *           W-NAME-FIRST   (CL15)      JOHN                     *
05313 *           W-NAME-MIDDLE  (CL15)      ALLEN                    *
05314 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
05315 *     CONTAIN                                                   *
05316 *                SMITH, JOHN ALLEN                              *
05317 *     OR                                                        *
05318 *                SMITH, JOHN A.                                 *
05319 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
05320 *     COPYBOOK, ELCNWA.                                         *
05321 *****************************************************************.
05322
05323      MOVE SPACES                 TO  WS-NAME-WORK-AREA.
05324      MOVE ZERO                   TO  WS-NAME-SW.
05325      SET NWA-INDEX               TO +1.
05326
05327      IF W-NAME-LAST   = SPACES AND
05328         W-NAME-MIDDLE = SPACES
05329          MOVE +1                 TO WS-NAME-SW.
05330
05331      MOVE W-NAME-LAST            TO WS-NAME-WORK2.
05332      PERFORM 5360-MOVE-NAME THRU 5360-EXIT.
05333
05334      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
05335      PERFORM 5360-MOVE-NAME THRU 5360-EXIT.
05336
05337      SET NWA-INDEX UP BY +1.
05338
05339      IF W-NAME-MIDDLE NOT = SPACES
05340          IF W-NAME-MIDDLE-2 = SPACES
05341              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
05342              SET NWA-INDEX UP BY +1
05343              MOVE '.'            TO WS-NW (NWA-INDEX)
05344              SET NWA-INDEX UP BY +2
05345          ELSE
05346              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
05347              PERFORM 5360-MOVE-NAME THRU 5360-EXIT.
05348
05349  5350-EXIT.
05350      EXIT.
05351                                  EJECT
05352  5360-MOVE-NAME.
05353
05354      IF WS-NAME-SW > +1
05355          GO TO 5360-EXIT.
05356
05357      IF WS-NAME-WORK2 = SPACES
05358          GO TO 5360-EXIT.
05359
05360      SET NWA-INDEX2            TO +1.
05361      SET NWA-INDEX3            TO +2.
05362
05363  5360-MOVE-NAME-CYCLE.
05364
05365      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
05366
05367      IF NWA-INDEX < +30
05368          SET NWA-INDEX UP BY +1
05369      ELSE
05370          ADD +2                TO  WS-NAME-SW
05371          GO TO 5360-EXIT.
05372
05373      IF NWA-INDEX2 < +20
05374          SET NWA-INDEX3 UP BY +1
05375          SET NWA-INDEX2 UP BY +1.
05376
05377      IF WS-NW2 (NWA-INDEX2) = SPACES AND
05378         WS-NW2 (NWA-INDEX3) = SPACES
05379          IF WS-NAME-SW = ZERO
05380              MOVE ','            TO  WS-NW (NWA-INDEX)
05381              SET NWA-INDEX UP BY +2
05382              MOVE +1             TO  WS-NAME-SW
05383              GO TO 5360-EXIT
05384          ELSE
05385              GO TO 5360-EXIT.
05386
05387      GO TO 5360-MOVE-NAME-CYCLE.
05388
05389  5360-EXIT.
05390      EXIT.
05391                                  EJECT
05392  6000-BUILD-SCREEN-B.
05393      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
05394      MOVE PI-CARRIER             TO MSTR-CARRIER.
05395      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
05396      MOVE PI-CERT-NO             TO MSTR-CERT-NO.
05397
05398      PERFORM 7900-READ-CLAIM THRU 7900-EXIT.
05399
05400  6050-BUILD-SCREEN.
05401      PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT.
05402      MOVE LOW-VALUES             TO EL156BI.
05403      MOVE ELMSTR-KEY             TO ELTRLR-KEY.
05404
05405      MOVE PI-PAYEE-SEQ-NUM       TO TRLR-SEQ-NO.
05406
05407      IF PI-PAYEE-TYPE = 'I'
05408         GO TO 6060-READ-TRAILER.
05409
05410      IF PI-PAYEE-TYPE = 'B'
05411         ADD +10                  TO TRLR-SEQ-NO
05412         GO TO 6060-READ-TRAILER.
05413
05414      IF PI-PAYEE-TYPE = 'A'
05415         ADD +20                  TO TRLR-SEQ-NO
05416         GO TO 6060-READ-TRAILER.
05417
05418      IF PI-PAYEE-TYPE = 'O'
05419         ADD +50                  TO TRLR-SEQ-NO
05420         GO TO 6060-READ-TRAILER.
05421
05422      IF PI-PAYEE-TYPE = 'Q'
05423         ADD +60                  TO TRLR-SEQ-NO
05424         GO TO 6060-READ-TRAILER.
05425
05426      IF PI-PAYEE-TYPE = 'P'
05427         ADD +30                  TO TRLR-SEQ-NO
05428         GO TO 6060-READ-TRAILER.
05429
05430      IF PI-PAYEE-TYPE = 'E'
05431         ADD +40                  TO TRLR-SEQ-NO.
05432
05433  6060-READ-TRAILER.
05434
05435      IF PI-PAYEE-TYPE    = 'A' AND
05436         PI-PAYEE-SEQ-NUM = 0
05437           GO TO 6061-READ-ACCOUNT.
05438
05439      IF (PI-COMPANY-ID    = 'AIG' OR 'AUK')  AND
05440         (PI-PAYEE-TYPE    = 'B')             AND
05441         (PI-PAYEE-SEQ-NUM = 9)
05442           PERFORM 7920-READ-BENE THRU 7920-EXIT
05443           IF BENE-FOUND-SW = 'N'
05444             GO TO 6065-NOT-FOUND
05445           ELSE
05446             MOVE BE-MAIL-TO-NAME   TO PI-PAYEE-NAME  NAMEBO
05447             MOVE BE-ADDRESS-LINE-1 TO ADDR1BO
05448             MOVE BE-ADDRESS-LINE-2 TO ADDR2BO
05449 *           MOVE BE-CITY-STATE     TO CITYSTBO
                  STRING BE-CITY ' ' BE-STATE
                     DELIMITED BY '  ' INTO CITYSTBO
                  END-STRING
05450             MOVE BE-ZIP-CODE       TO ZIPBO
05451             GO TO 6061-READ-ACCOUNT.
05452
05453      IF PI-PAYEE-TYPE    = 'B' AND
05454         PI-PAYEE-SEQ-NUM = 0
05455           PERFORM 7920-READ-BENE THRU 7920-EXIT
05456           IF BENE-FOUND-SW = 'N'
05457             GO TO 6065-NOT-FOUND
05458           ELSE
05459             MOVE BE-MAIL-TO-NAME   TO PI-PAYEE-NAME  NAMEBO
05460             MOVE BE-ADDRESS-LINE-1 TO ADDR1BO
05461             MOVE BE-ADDRESS-LINE-2 TO ADDR2BO
05462 *           MOVE BE-CITY-STATE     TO CITYSTBO
                  STRING BE-CITY ' ' BE-STATE
                     DELIMITED BY '  ' INTO CITYSTBO
                  END-STRING
05463             MOVE BE-ZIP-CODE       TO ZIPBO
013017            if be-ach-yes-or-no = 'Y'
013017               move 'Y'          to pi-ach-payment
013017            else
013017               move 'N'          to pi-ach-payment
013017            end-if
05464             GO TO 6061-READ-ACCOUNT.
05465
05466      
      * EXEC CICS HANDLE CONDITION
05467 *         NOTFND(6065-NOT-FOUND)
05468 *     END-EXEC.
      *    MOVE '"$I                   ! / #00014572' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303134353732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05469
05470      PERFORM 7950-READ-TRAILER THRU 7950-EXIT.
05471
05472      MOVE AT-MAIL-TO-NAME        TO PI-PAYEE-NAME.
05473      MOVE AT-MAIL-TO-NAME        TO NAMEBO.
05474      MOVE AT-ADDRESS-LINE-1      TO ADDR1BO.
05475      MOVE AT-ADDRESS-LINE-2      TO ADDR2BO.
05476 *    MOVE AT-CITY-STATE          TO CITYSTBO.
           STRING AT-CITY ' ' AT-STATE
              DELIMITED BY '  ' INTO CITYSTBO
           END-STRING
05477      MOVE AT-ZIP-CODE            TO ZIPBO.
05478
05479  6061-READ-ACCOUNT.
05480      
      * EXEC CICS HANDLE CONDITION
05481 *         NOTFND(6068-ACCT-NOT-FND)
05482 *     END-EXEC.
      *    MOVE '"$I                   ! 0 #00014589' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303134353839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05483
05484      
      * EXEC CICS READ
05485 *         DATASET('ERACCT')
05486 *         SET    (ADDRESS OF ACCOUNT-MASTER)
05487 *         RIDFLD (PI-ACCT-KEY)
05488 *     END-EXEC.
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"S        E          (   #00014593' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134353933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05489
05490      MOVE AM-CARRIER             TO ACARRBO.
05491      MOVE AM-GROUPING            TO GROUPBO.
05492      MOVE AM-STATE               TO STATEBO.
05493      MOVE AM-ACCOUNT             TO ACCTBO.
05494      MOVE AM-NAME                TO ACCTNMBO.
05495
05496      IF PI-PAYEE-TYPE = 'A' AND
05497         PI-PAYEE-SEQ-NUM = 0
05498           MOVE AM-NAME             TO NAMEBO
05499                                       PI-PAYEE-NAME
05500           MOVE AM-ADDRS            TO ADDR1BO
05501           MOVE SPACES              TO ADDR2BO
05502 *         MOVE AM-CITY             TO CITYSTBO
                STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                   DELIMITED BY '  ' INTO CITYSTBO
                END-STRING
05503           MOVE AM-ZIP-PRIME        TO WS-ZIP
05504           MOVE AM-ZIP-PLUS4        TO WS-ZIP-EXT
05505           INSPECT WS-WORK-ZIP REPLACING ALL SPACES BY '0'
05506           MOVE WS-NUMERIC-ZIP      TO ZIPBO.
091808
111714***  We don't really need the code below because we don't do the
111714***  AK thing anymore.  However, we may have a state special
111714***  of some sort in the future so why not just leave it??
091808     IF CITYSTBO EQUAL SPACES
091808         MOVE 'XX' TO PI-CHECK-STATE-FOR-AK
091808         GO TO 6070-FORMAT-DATA
091808     END-IF.
091808
091808     MOVE CITYSTBO TO WS-PAYEE-CITY-STATE.
091808     MOVE 'N' TO WS-PAYEE-STATE-FOUND.
091808     MOVE 0 TO WS-BEG-SUB WS-END-SUB.
091808     PERFORM VARYING WS-SUB FROM 30 BY -1
091808          UNTIL WS-SUB = 0 OR PAYEE-STATE-FOUND
091808        IF WS-END-SUB = 0  AND
110513           (WS-PAYEE-CITY-ST (WS-SUB) EQUAL SPACES
110513              OR LOW-VALUES  OR '.')
091808            CONTINUE
091808        ELSE
091808            IF WS-END-SUB = 0
091808                MOVE WS-SUB TO WS-END-SUB
091808            ELSE
091808                IF WS-PAYEE-CITY-ST (WS-SUB) = SPACES
091808                    COMPUTE WS-BEG-SUB = WS-SUB + 1
091808                    MOVE 'Y' TO WS-PAYEE-STATE-FOUND
091808                END-IF
091808            END-IF
091808        END-IF
091808        IF WS-PAYEE-CITY-ST (WS-SUB) EQUAL ','
091808            COMPUTE WS-BEG-SUB = WS-SUB + 1
091808            MOVE 'Y' TO WS-PAYEE-STATE-FOUND
091808        END-IF
091808     END-PERFORM.
091808
091808     IF WS-BEG-SUB > 0
091808         COMPUTE WS-STATE-LENGTH = WS-END-SUB - WS-BEG-SUB + 1
091808         IF WS-STATE-LENGTH = 2
091808             MOVE WS-PAYEE-CITY-STATE (WS-BEG-SUB:WS-STATE-LENGTH)
091808                  TO PI-CHECK-STATE-FOR-AK
091808         ELSE
091808             IF WS-PAYEE-CITY-STATE (WS-BEG-SUB:WS-STATE-LENGTH) =
091808                'ALASKA'
091808                 MOVE 'AK' TO PI-CHECK-STATE-FOR-AK
091808             ELSE
091808                 MOVE 'XX' TO PI-CHECK-STATE-FOR-AK
091808             END-IF
091808         END-IF
091808     ELSE
091808         MOVE 'XX' TO PI-CHECK-STATE-FOR-AK
091808     END-IF.
05507
05508      GO TO 6070-FORMAT-DATA.
05509
05510  6065-NOT-FOUND.
05511      MOVE 'NO ADDRESS RECORD FOUND'  TO ADDR1BO.
05512      GO TO 6061-READ-ACCOUNT.
05513
05514  6068-ACCT-NOT-FND.
05515      MOVE 'NO ACCOUNT RECORD FOUND'  TO ACCTNMBO.
05516
05517  6070-FORMAT-DATA.
05518      IF PI-HOLDTIL = ZEROS
05519         MOVE SAVE-DATE           TO PAYDTBI
05520        ELSE
05521         MOVE PI-HOLDTIL          TO PAYDTBO
05522         INSPECT PAYDTBI REPLACING ALL SPACES BY '/'.
05523
052506     MOVE PI-PROOF-DATE          TO PROOFDBO.
052506     INSPECT PROOFDBI REPLACING ALL SPACES BY '/'.
052506
05524      IF EIBAID = DFHPF3 OR DFHPF4
05525         NEXT SENTENCE
05526      ELSE
05527        GO TO 6071-CONTINUE.
05528
05529      MOVE PI-EPYFROM          TO PYFROMBO.
05530      INSPECT PYFROMBI REPLACING ALL SPACES BY '/'.
05531
05532      IF NOT PI-USES-PAID-TO
05533         MOVE PI-EPYTHRU          TO PYTHRUBO
05534         INSPECT PYTHRUBI REPLACING ALL SPACES BY '/'
05535      ELSE
05536         MOVE PI-EPYTHRU       TO DC-GREG-DATE-1-MDY
05537         MOVE '4'              TO DC-OPTION-CODE
05538         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05539         IF NO-CONVERSION-ERROR
05540            MOVE +1            TO DC-ELAPSED-DAYS
05541            MOVE +0            TO DC-ELAPSED-MONTHS
05542            MOVE '6'           TO DC-OPTION-CODE
05543            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05544            IF NO-CONVERSION-ERROR
05545               MOVE DC-GREG-DATE-1-EDIT TO PYTHRUBI.
05546
05547      MOVE PI-EDAYS            TO DAYSBO.
05548      MOVE PI-EPYAMT           TO PAYAMTBO.
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
              IF PI-INT-AMT > ZEROS
                 MOVE PI-INT-AMT       TO INTAMTBO
              END-IF
           END-IF
05549      GO TO 6072-SET-TYPE.
05550
05551  6071-CONTINUE.
05552
05553      IF PI-CPYFROM NOT = LOW-VALUES
05554         MOVE PI-CPYFROM          TO DC-BIN-DATE-1
05555         MOVE SPACES              TO DC-OPTION-CODE
05556         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05557         MOVE DC-GREG-DATE-1-EDIT TO PYFROMBI.
05558
05559      IF PI-CPYTHRU NOT = LOW-VALUES
05560         IF NOT PI-USES-PAID-TO
05561            MOVE PI-CPYTHRU         TO DC-BIN-DATE-1
05562            MOVE SPACES             TO DC-OPTION-CODE
05563            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05564            MOVE DC-GREG-DATE-1-EDIT TO PYTHRUBI
05565         ELSE
05566            MOVE PI-CPYTHRU         TO DC-BIN-DATE-1
05567            MOVE '6'                TO DC-OPTION-CODE
05568            MOVE +1                 TO DC-ELAPSED-DAYS
05569            MOVE +0                 TO DC-ELAPSED-MONTHS
05570            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05571            MOVE DC-GREG-DATE-1-EDIT TO PYTHRUBI.
05572
05573      MOVE PI-CDAYS               TO DAYSBO.
05574      MOVE PI-CPYAMT              TO PAYAMTBO.
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
              IF PI-INT-AMT > ZEROS
                 MOVE PI-INT-AMT       TO INTAMTBO
              END-IF
           END-IF
           .
05576  6072-SET-TYPE.
05577      IF PI-PMTTYPE = '1'
05578         MOVE 'PARTIAL'           TO PAYTYPBO.
05579
05580      IF PI-PMTTYPE = '2'
05581         MOVE 'FINAL'             TO PAYTYPBO.
05582
05583      IF PI-PMTTYPE = '3'
05584         MOVE 'LUMP SUM'          TO PAYTYPBO.
05585
05586      IF PI-PMTTYPE = '4'
05587         MOVE 'ADDITIONAL'        TO PAYTYPBO.
05588
05589      IF PI-PMTTYPE = '5'
05590         MOVE 'CHARGEABLE'        TO PAYTYPBO.
05591
05592      IF PI-PMTTYPE = '6'
05593         MOVE 'NON-CHARGEABLE'    TO PAYTYPBO.
05594
05595      IF PI-PROV-PMT
05596         MOVE 'PROVISIONAL   '    TO PAYTYPBO.
05597
05598      IF PI-ECHECKNO NOT = SPACES
05599         MOVE PI-ECHECKNO         TO CKNUMBO
05600      ELSE
05601         MOVE PI-CCHECKNO         TO CKNUMBO.
05602
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
CIDMOD        MOVE ELMSTR-KEY          TO  ELTRLR-KEY
CIDMOD        MOVE +91                 TO  TRLR-SEQ-NO
CIDMOD        
      * EXEC CICS READ
CIDMOD*           DATASET  ('ELTRLR')
CIDMOD*           SET      (ADDRESS OF ACTIVITY-TRAILERS)
CIDMOD*           RIDFLD   (ELTRLR-KEY)
CIDMOD*           RESP     (WS-RESPONSE)
CIDMOD*       END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00014782' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303134373832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD
CIDMOD        IF WS-RESP-NORMAL
CIDMOD           MOVE AT-INFO-LINE-1   TO  LOANNBO
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO  LOANNBO
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
CIDMOD*       MOVE PI-LOAN-NO          TO LOANNBO
CIDMOD
05603      IF CKNUMBO = SPACES
05604          MOVE SPACES             TO CKNUMHDO.
05605
05606      MOVE CL-CLAIM-NO            TO CLMNOBO.
05607      MOVE CL-CARRIER             TO CARRBO.
05608      MOVE CL-CERT-PRIME          TO CERTNOBO.
05609      MOVE CL-CERT-SFX            TO SUFXBO.
05610
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                          OR 'B' or 'H'
              EVALUATE CL-CLAIM-TYPE
                 WHEN 'I'
121802              MOVE '  IU  '      TO CLMTYPBO
                 WHEN 'G'
                    MOVE ' GAP  '      TO CLMTYPBO
                 WHEN 'F'
                    MOVE ' FMLA '      TO CLMTYPBO
022122           WHEN 'B'
022122              MOVE ' BRV  '      TO CLMTYPBO
022122           WHEN 'H'
022122              MOVE ' HOSP '      TO CLMTYPBO
                 WHEN OTHER
121802              MOVE PI-AH-OVERRIDE-L6
                                       TO CLMTYPBO
121802        END-EVALUATE
05612         MOVE ZEROS               TO UNDISBO
121802*       MOVE PI-AH-OVERRIDE-L6   TO CLMTYPBO
05614         IF PI-PMTTYPE = '5' OR '6'
05615            GO TO 6073-GET-NOTIFY-DATA
05616         ELSE
05617            MOVE PI-DAILY-RATE    TO RATEBO
05618            GO TO 6073-GET-NOTIFY-DATA.
05619
05620      MOVE PI-LIFE-OVERRIDE-L6    TO CLMTYPBO.
05621
05622      IF (PI-PMTTYPE = '5' OR '6') OR EIBAID = DFHPF5
05623         MOVE ZEROS               TO UNDISBO
05624      ELSE
05625         COMPUTE UNDISBO = PI-CPYAMT - PI-EPYAMT.
05626
05627  6073-GET-NOTIFY-DATA.
05628
05629      MOVE 'N'                    TO  PI-3RD-PARTY-SW.
05630
05631      IF AM-NOTIF-OF-PAYMENTS NOT = 'Y'
05632          MOVE SPACES             TO  NAMEB2O
05633                                      ADDR1B2O
05634                                      ADDR2B2O
05635                                      CITYST2O
05637                                      AGTHDGO
CIDMOD         MOVE ZEROS              TO  ZIPB2O
05638          GO TO 6080-SET-INCURRED.
05639
05640      MOVE ELMSTR-KEY             TO  ELTRLR-KEY.
05641      MOVE +29                    TO  TRLR-SEQ-NO.
05642
05643      
      * EXEC CICS HANDLE CONDITION
05644 *        NOTFND   (6074-GET-COMP-DATA)
05645 *    END-EXEC.
      *    MOVE '"$I                   ! 1 #00014854' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303134383534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05646
05647      PERFORM 7950-READ-TRAILER THRU 7950-EXIT.
05648
05649      MOVE AT-MAIL-TO-NAME        TO  NAMEB2O.
05650      MOVE AT-ADDRESS-LINE-1      TO  ADDR1B2O.
05651      MOVE AT-ADDRESS-LINE-2      TO  ADDR2B2O.
05652 *    MOVE AT-CITY-STATE          TO  CITYST2O.
           STRING AT-CITY ' ' AT-STATE
              DELIMITED BY '  ' INTO CITYST2O
           END-STRING
05653      MOVE AT-ZIP-CODE            TO  ZIPB2O.
05654      GO TO 6080-SET-INCURRED.
05655
05656  6074-GET-COMP-DATA.
05657
05658      IF AM-3RD-PARTY-NOTIF-LEVEL IS NOT NUMERIC
05659          MOVE ZEROS              TO  AM-3RD-PARTY-NOTIF-LEVEL.
05660
05661      IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) = ZEROS OR SPACES
05662          MOVE 'NO ADDRESS AVAILABLE' TO  NAMEB2O
05663                                          ADDR1B2O
05664                                          ADDR2B2O
05665                                          CITYST2O
05666          MOVE ZEROS                  TO  ZIPB2O
05667          GO TO 6080-SET-INCURRED.
05668
05669      MOVE 'Y'                    TO  PI-3RD-PARTY-SW.
05670      MOVE AM-COMPANY-CD          TO  COMP-COMP-CD.
05671      MOVE AM-CARRIER             TO  COMP-CARRIER.
05672      MOVE AM-GROUPING            TO  COMP-GROUP.
05673      MOVE 'A'                    TO  COMP-TYPE.
05674      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
05675                                  TO  COMP-FIN-RESP.
05676
05677      IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO
05678          IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR
052814                                       'G' OR 'B' or 'S'
05680              MOVE 'G'            TO  COMP-TYPE
05681              MOVE LOW-VALUES     TO  COMP-ACCOUNT
05682          ELSE
05683              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
05684                                  TO  COMP-ACCOUNT
05685      ELSE
05686          MOVE 'G'                TO  COMP-TYPE
05687          MOVE LOW-VALUES         TO  COMP-ACCOUNT.
05688
05689      PERFORM 7800-READ-COMP-MSTR THRU 7800-COMP-EXIT.
05690
05691      IF COMP-REC-FOUND
05692          MOVE CO-ACCT-NAME       TO  NAMEB2O
05693          MOVE CO-ADDR-1          TO  ADDR1B2O
05694          MOVE CO-ADDR-2          TO  ADDR2B2O
05695 *        MOVE CO-ADDR-3          TO  CITYST2O
               STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
                  DELIMITED BY '  ' INTO CITYST2O
               END-STRING
05696          MOVE CO-ZIP             TO  ZIPB2O
05697      ELSE
05698          MOVE 'NO COMP ADDRESS AVAILABLE'
05699                                  TO  NAMEB2O
05700                                      ADDR1B2O
05701                                      ADDR2B2O
05702                                      CITYST2O
05703          MOVE ZEROS               TO ZIPB2O.
05704
05705  6080-SET-INCURRED.
05706      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.
05707      MOVE SPACES                 TO DC-OPTION-CODE.
05708      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
05709      MOVE DC-GREG-DATE-1-EDIT    TO INCURBO.
05710      MOVE PI-PROCESSOR-ID        TO BYBO.
05711      MOVE MAP-NAMEB              TO MAP-NAME.
05712      MOVE 'C'                    TO PI-PASS-SW.
05713      MOVE EIBAID                 TO PI-PFKEY-USED.
05714      MOVE -1                     TO ENTPFBL.
05715      GO TO 8100-SEND-INITIAL-MAP.
05716
05717      EJECT
05718  6100-BUILD-PAYMENT-TRAILER.
091808
05720      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
05721      MOVE '6'                    TO CNTL-REC-TYPE.
05722
05723      IF CONTROL-IS-ACTUAL-CARRIER
05724          MOVE PI-CARRIER         TO WS-CARR
05725      ELSE
05726          MOVE PI-CARRIER-CONTROL-LEVEL TO WS-CARR.
05727
05728      IF PI-COMPANY-ID = 'FIA'
05729          MOVE PI-CARRIER         TO  WS-CARR.
05730
05731      MOVE WS-CARR-ACCESS         TO CNTL-ACCESS.
05732      MOVE 0                      TO CNTL-SEQ-NO.
05733      MOVE 'CARR'                 TO FILE-SWITCH.
05734
05735      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.
05736
05737      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
05738          IF PI-OFFLINE     = 'Y' AND
05739             PI-ECHECKNO-CR = 'CR'
05740               GO TO 6110-READ-CONTROL
05741          ELSE
05742             IF PI-CASH = 'N'
05743                 GO TO 6110-READ-CONTROL.
05744
05745      IF CHECK-NO-MANUAL    OR
05746         CHECK-NO-AT-PRINT  OR
05747         PI-OFFLINE = 'Y'
05748           GO TO 6130-BUILD-TRAILER.
05750      IF PI-COMPANY-ID = 'FIA'
05751          NEXT SENTENCE
05752      ELSE
05753          IF NOT CONTROL-IS-ACTUAL-CARRIER
05754              GO TO 6110-READ-CONTROL.
05755
05756      PERFORM 7940-READ-CONTROL-UPDATE THRU 7940-EXIT.
05757
05758      IF CHECK-CNT-RESET-VALUE
05759         MOVE +1                  TO CF-CHECK-COUNTER
05760      ELSE
05761         ADD +1                   TO CF-CHECK-COUNTER.
05762
05763      MOVE CF-CHECK-COUNTER       TO WS-CHECK-NUMERIC.
05764
05765      IF CHECK-NO-CARR-SEQ
05766         MOVE PI-CARRIER          TO WS-CHECK-CARR.
05767
05768      MOVE WS-CHECK-AREA          TO PI-CCHECKNO.
032813
032813     MOVE 'N' TO PI-REAUDIT-NEEDED.
032813     IF CF-CHECK-COUNTER >= CF-CARRIER-NEXT-AUDIT-CHK-NO  AND
032813        CF-CARRIER-NEXT-AUDIT-CHK-NO > ZERO
032813         MOVE 'Y' TO PI-REAUDIT-NEEDED
032813         COMPUTE WRK-NEXT-AUDIT-CHK-NO =
032813            CF-CHECK-COUNTER + PI-REAUDIT-INTERVAL
032813         MOVE WRK-NEXT-AUDIT-CHK-NO TO
032813                      CF-CARRIER-NEXT-AUDIT-CHK-NO
032813     END-IF.
032813
05769      GO TO 6120-REWRITE.
091808
091808 6105-STATE-CHECK-NUMBER.
091808     IF CHECK-NO-MANUAL    OR
091808        CHECK-NO-AT-PRINT  OR
091808        PI-OFFLINE = 'Y'
091808           GO TO 6130-BUILD-TRAILER
091808     END-IF.
091808
091808     MOVE PI-COMPANY-ID      TO CNTL-COMP-ID.
091808     MOVE PI-CHECK-STATE-FOR-AK TO WS-ST-ACCESS.
091808     MOVE WS-STATE-ACCESS    TO CNTL-ACCESS.
091808     MOVE '3'                TO CNTL-REC-TYPE.
091808     MOVE +0                 TO CNTL-SEQ-NO.
091808     MOVE 'STAT'             TO FILE-SWITCH.
091808     PERFORM 7940-READ-CONTROL-UPDATE THRU 7940-EXIT.
091808
091808     IF CF-ST-CHECK-CNT-RESET
091808         MOVE +1             TO CF-ST-CHECK-COUNTER
091808     ELSE
091808         ADD +1              TO CF-ST-CHECK-COUNTER
091808     END-IF.
091808
091808     MOVE CF-ST-CHECK-COUNTER TO WS-CHECK-NUMERIC.
091808     MOVE WS-CHECK-AREA       TO PI-CCHECKNO.
091808     GO TO 6120-REWRITE.
05770
05771  6110-READ-CONTROL.
05772      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
05773      MOVE '1'                    TO CNTL-REC-TYPE.
05774      MOVE SPACES                 TO CNTL-ACCESS.
05775      MOVE 0                      TO CNTL-SEQ-NO.
05776
05777      PERFORM 7940-READ-CONTROL-UPDATE THRU 7940-EXIT.
05778
05779      IF CO-CHECK-COUNT-RESET
05780         MOVE +1                  TO CF-CO-CHECK-COUNTER
05781      ELSE
05782         ADD +1                   TO CF-CO-CHECK-COUNTER.
05783
05784      MOVE CF-CO-CHECK-COUNTER    TO WS-CHECK-NUMERIC.
05785
05786      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
05787          IF PI-OFFLINE     = 'Y'  AND
05788             PI-ECHECKNO-CR = 'CR'
05789               MOVE WS-A-CHECK-NUM-5   TO PI-ECHECKNO-5
05790               MOVE PI-ECHECKNO        TO PI-CCHECKNO CHECKNOO
05791               MOVE AL-UANON           TO CHECKNOA
05792               GO TO 6120-REWRITE
05793          ELSE
05794          IF PI-CASH = 'N'
05795              MOVE 'N'                TO PI-ECHECKNO-N
05796              MOVE WS-A-CHECK-NUM-6   TO PI-ECHECKNO-6
05797              MOVE PI-ECHECKNO        TO PI-CCHECKNO CHECKNOO
05798              MOVE AL-UANON           TO CHECKNOA
05799              GO TO 6120-REWRITE.
05800
05801      MOVE WS-CHECK-NUMERIC       TO PI-CCHECKNO.
05802
05803  6120-REWRITE.
05804      
      * EXEC CICS REWRITE
05805 *         DATASET('ELCNTL')
05806 *         FROM(CONTROL-FILE)
05807 *     END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %   #00015056' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05808
05809      MOVE 'Y'                    TO PI-CK-ASSIGN.
05810
05811      EJECT
05812  6130-BUILD-TRAILER.
05813
05814      
      * EXEC CICS GETMAIN
05815 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
05816 *         LENGTH    (200)
05817 *         INITIMG   (GETMAIN-SPACE)
05818 *     END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00015066' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05819
05820      MOVE LOW-VALUES             TO AT-PREV-LAST-PMT-DT
05821                                     AT-PREV-PAID-THRU-DT.
052506     MOVE LOW-VALUES             TO AT-PMT-PROOF-DT.
05822      MOVE ZEROS                  TO AT-PREV-LAST-PMT-AMT.
05823      MOVE ELMSTR-KEY             TO AT-CONTROL-PRIMARY.
05824      MOVE 'AT'                   TO AT-RECORD-ID.
05825      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.
05826
05827      IF (PI-PMTNOTE1 = SPACES OR LOW-VALUES) AND
05828         (PI-PMTNOTE2 = SPACES OR LOW-VALUES)
05829            MOVE +0               TO AT-PAYMENT-NOTE-SEQ-NO
05830      ELSE
05831            MOVE AT-SEQUENCE-NO   TO AT-PAYMENT-NOTE-SEQ-NO
05832            SUBTRACT +1         FROM AT-PAYMENT-NOTE-SEQ-NO.
05833
05834      IF PI-COMPANY-ID = 'LAP' OR 'RMC'
05835          IF AT-PAYMENT-NOTE-SEQ-NO = ZERO
05836              MOVE AT-SEQUENCE-NO TO AT-FORM-CTL-SEQ-NO
05837              SUBTRACT +1       FROM AT-FORM-CTL-SEQ-NO
05838            ELSE
05839              MOVE AT-PAYMENT-NOTE-SEQ-NO TO AT-FORM-CTL-SEQ-NO
05840              SUBTRACT +1               FROM AT-FORM-CTL-SEQ-NO.
05841
05842      MOVE '2'                    TO AT-TRAILER-TYPE.
05843      MOVE WS-TODAY-DATE          TO AT-RECORDED-DT
05844                                     AT-PAYMENT-LAST-MAINT-DT.
05845      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
05846                                     AT-PAYMENT-LAST-UPDATED-BY.
05847      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
05848      MOVE PI-PMTTYPE             TO AT-PAYMENT-TYPE.
05849      MOVE CL-CLAIM-TYPE          TO AT-CLAIM-TYPE.
05850      MOVE CL-CLAIM-PREM-TYPE     TO AT-CLAIM-PREM-TYPE.
05851
05852      IF PI-PMT-APPROVED
05853         IF PI-OFFLINE  = 'N'
05854            MOVE 'U'              TO AT-PAYMENT-APPROVAL-SW.
05855
05856      IF PI-PFKEY-USED = DFHPF5
05857         GO TO 6140-PF5-ENTERED.
05858
05859      MOVE PI-EEXPENS             TO AT-EXPENSE-PER-PMT.
05860      MOVE PI-EPYAMT              TO AT-AMOUNT-PAID.
05861
05862      IF PI-AIG-SPECIAL-BENEFIT
05863          IF WS-1ST-SPECIAL-PMT
05864              COMPUTE AT-AMOUNT-PAID ROUNDED = PI-EPYAMT / 1.5
05865              COMPUTE PI-REDUND-PMTAMT = PI-EPYAMT -
05866                                         AT-AMOUNT-PAID
05867          ELSE
05868              MOVE PI-REDUND-PMTAMT TO AT-AMOUNT-PAID.
05869
05870      MOVE PI-ETYPE               TO AT-EXPENSE-TYPE.
05871
05872      MOVE PI-EPYFROM             TO DC-GREG-DATE-1-MDY.
05873      MOVE '4'                    TO DC-OPTION-CODE.
05874      PERFORM 9700-LINK-DATE-CONVERT.
05875
05876      IF DATE-CONVERSION-ERROR
05877         MOVE LOW-VALUES          TO AT-PAID-FROM-DT
05878      ELSE
05879         MOVE DC-BIN-DATE-1       TO AT-PAID-FROM-DT.
05880
05881      MOVE PI-EPYTHRU             TO DC-GREG-DATE-1-MDY.
05882      MOVE '4'                    TO DC-OPTION-CODE.
05883      PERFORM 9700-LINK-DATE-CONVERT.
05884
05885      IF DATE-CONVERSION-ERROR
05886         MOVE LOW-VALUES          TO AT-PAID-THRU-DT
05887      ELSE
05888         MOVE DC-BIN-DATE-1       TO AT-PAID-THRU-DT.
05889
05890      MOVE PI-EDAYS               TO AT-DAYS-IN-PERIOD
013017     move pi-ach-payment         to at-ach-payment
05891      GO TO 6150-FINISH-MOVE.
05892
05893  6140-PF5-ENTERED.
05894      MOVE PI-CEXPENS             TO AT-EXPENSE-PER-PMT.
05895      MOVE PI-CPYAMT              TO AT-AMOUNT-PAID.
05896
05897      IF PI-AIG-SPECIAL-BENEFIT
05898          IF WS-1ST-SPECIAL-PMT
05899              COMPUTE AT-AMOUNT-PAID ROUNDED = PI-CPYAMT / 1.5
05900              COMPUTE PI-REDUND-PMTAMT = PI-CPYAMT -
05901                                         AT-AMOUNT-PAID
05902          ELSE
05903              MOVE PI-REDUND-PMTAMT TO AT-AMOUNT-PAID.
05904
05905      MOVE PI-CPYFROM             TO AT-PAID-FROM-DT.
05906      MOVE PI-CPYTHRU             TO AT-PAID-THRU-DT.
05907      MOVE PI-CDAYS               TO AT-DAYS-IN-PERIOD.
05908
05909  6150-FINISH-MOVE.
05910
05911      IF PI-ECHECKNO = SPACES
05912          MOVE PI-CCHECKNO    TO AT-CHECK-NO
05913      ELSE
05914          MOVE PI-ECHECKNO    TO AT-CHECK-NO.
05915
05916      MOVE PI-PAYEE               TO AT-PAYEE-TYPE-CD.
05917      MOVE PI-PAYEE-NAME          TO AT-PAYEES-NAME.
05918
020413*    MOVE PI-GROUPED             TO AT-GROUPED-PAYMENT.
020413*    MOVE PI-CASH                TO AT-CASH-PAYMENT.
05921
05922      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
05923          MOVE CL-ASSOCIATES      TO AT-ASSOCIATES.
05924
05925      IF PI-OFFLINE = 'N'
05926         MOVE '1'                 TO AT-PAYMENT-ORIGIN
05927         MOVE LOW-VALUES          TO AT-PMT-SELECT-DT
05928      ELSE
05929         MOVE PI-MONTH-END-SAVE   TO AT-PMT-SELECT-DT
05930         MOVE '3'                 TO AT-PAYMENT-ORIGIN.
05931
013017     move pi-ach-payment         to at-ach-payment
05932      MOVE SPACES                 TO AT-AIG-UNEMP-IND.
05933
05934      IF WS-2ND-SPECIAL-PMT
05935          MOVE 'Y'                TO AT-CASH-PAYMENT
05936          MOVE SPACES             TO AT-CHECK-NO
05937          MOVE '3'                TO AT-PAYMENT-ORIGIN
05938          MOVE PI-MONTH-END-SAVE  TO AT-PMT-SELECT-DT
05939          MOVE 'U'                TO AT-AIG-UNEMP-IND
05940          MOVE 'I'                TO AT-PAYEE-TYPE-CD.
05941
05942      MOVE LOW-VALUES             TO AT-CHECK-WRITTEN-DT
05943                                     AT-TO-BE-WRITTEN-DT.
05944
05945      IF PI-HOLDTIL NOT = ZEROS
05946         MOVE PI-HOLDTIL          TO DC-GREG-DATE-1-MDY
05947         MOVE '4'                 TO DC-OPTION-CODE
05948         PERFORM 9700-LINK-DATE-CONVERT
05949         IF PI-OFFLINE = 'N'
05950            MOVE DC-BIN-DATE-1    TO AT-TO-BE-WRITTEN-DT
031218           MOVE 'P'              TO WS-HOLD-UNTIL-SW
05951         ELSE
05952            MOVE DC-BIN-DATE-1    TO AT-CHECK-WRITTEN-DT.
05953
05954      MOVE PI-ERESV               TO AT-ADDL-RESERVE.
05955      MOVE ZEROS                  TO AT-CHECK-QUE-CONTROL
05956                                     AT-CHECK-QUE-SEQUENCE.
05957
05958      IF PI-PFKEY-USED = DFHPF4
05959         MOVE '1'                 TO AT-FORCE-CONTROL.
05960
05961      IF PI-PMTTYPE NOT = '4' AND '5' AND '6'
05962         MOVE CL-LAST-PMT-DT      TO AT-PREV-LAST-PMT-DT
05963         MOVE CL-PAID-THRU-DT     TO AT-PREV-PAID-THRU-DT
05964         MOVE CL-LAST-PMT-AMT     TO AT-PREV-LAST-PMT-AMT.
05965
05966      MOVE LOW-VALUES             TO AT-PMT-ACCEPT-DT
05967                                     AT-VOID-SELECT-DT
05968                                     AT-VOID-ACCEPT-DT
05969                                     AT-VOID-DT.
121802     IF (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122         OR 'B' OR 'H')
05972         AND (PI-PMTTYPE NOT = '4' AND '5' AND '6')
05973         MOVE PI-DAILY-RATE       TO AT-DAILY-RATE
05974         MOVE PI-BEN-DAYS         TO AT-ELIMINATION-DAYS
05975         MOVE PI-BEN-TYPE         TO AT-BENEFIT-TYPE
05976      ELSE
05977         MOVE ZEROS               TO AT-DAILY-RATE
05978                                     AT-ELIMINATION-DAYS
05979         MOVE SPACES              TO AT-BENEFIT-TYPE
           END-IF
052506     MOVE PI-PROOF-DATE          TO DC-GREG-DATE-1-MDY.
052506     MOVE '4'                    TO DC-OPTION-CODE.
052506     PERFORM 9700-LINK-DATE-CONVERT.
052506     IF DATE-CONVERSION-ERROR
052506         MOVE LOW-VALUES          TO AT-PMT-PROOF-DT
052506     ELSE
052506         MOVE DC-BIN-DATE-1       TO AT-PMT-PROOF-DT
052506     END-IF.
           MOVE PI-PRINT-EOB-YN        TO AT-PRINT-EOB-WITH-CHECK
020413     MOVE PI-PRINT-CLM-FRM-YN    TO AT-PRINT-CLM-FORM
020413     MOVE PI-PRINT-SURVEY-YN     TO AT-PRINT-SURVEY
102413     MOVE PI-SPECIAL-RELEASE-YN  TO AT-SPECIAL-RELEASE
032813     IF (PI-REAUDIT-NEEDED = 'Y')
              or (pi-max-ext-used = 'Y')
052814        OR (PI-APPROVAL-3550-NEEDED = 'Y')
043019        OR PI-DUPE-APPROVAL-NEEDED = 'Y'
110515         PERFORM 6155-READ-CONTROL
110515         PERFORM 6157-SET-APP-LEVEL
110515         IF AT-APPROVAL-LEVEL-REQD < '4'
111113            MOVE '4' TO AT-APPROVAL-LEVEL-REQD
110515         END-IF
032813         MOVE 'Y' TO WS-NEED-APPROVAL
032813         GO TO 6170-WRITE
032813     END-IF.
05981 *******************************************************
05982      IF NOT PI-PMT-GRADUATED
05983          GO TO 6170-WRITE.
110515     PERFORM 6155-READ-CONTROL.
05992
05993      IF PAID-FOR-LIFE
05994          IF AT-AMOUNT-PAID < PI-MAX-LF-PMT-TOL
05995              MOVE 'A'            TO  AT-PAYMENT-APPROVAL-SW
05996                                      WS-PMT-APPROVAL-SW
05997              GO TO 6170-WRITE.
05998
031808*05999      IF PAID-FOR-AH
031808     IF (PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP)
06000          IF AT-AMOUNT-PAID < PI-MAX-PMT-TOL
06001              MOVE 'A'            TO  AT-PAYMENT-APPROVAL-SW
06002                                      WS-PMT-APPROVAL-SW
06003              GO TO 6170-WRITE.
06004
06005      MOVE '1'                    TO AT-APPROVED-LEVEL.
06006
110515     PERFORM 6157-SET-APP-LEVEL.
031808
031808     MOVE AT-APPROVAL-LEVEL-REQD TO WS-SAVE-APPROVAL-LEVEL-REQD.
06022
06023      IF NOT PI-PMT-GRADUATED
06024          GO TO 6170-WRITE.
06025
06026      
      * EXEC CICS HANDLE CONDITION
06027 *        NOTFND   (6170-WRITE)
06028 *    END-EXEC.
      *    MOVE '"$I                   ! 2 #00015288' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303135323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06029
06030      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
06031      MOVE '2'                    TO CNTL-REC-TYPE.
06032      MOVE +0                     TO CNTL-SEQ-NO.
06033      MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS.
06034
06035      
      * EXEC CICS READ
06036 *         DATASET  ('ELCNTL')
06037 *         SET      (ADDRESS OF CONTROL-FILE)
06038 *         RIDFLD   (ELCNTL-KEY)
06039 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00015297' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135323937' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06040
06041      MOVE CF-APPROVAL-LEVEL TO WS-APPROVAL-LEVEL.
06042
06043      IF WS-APPROVAL-LEVEL = '1' OR '2' OR '3'
091813                         OR '4' OR '5'
06044          NEXT SENTENCE
06045      ELSE
031808         MOVE 'Y' TO WS-NEED-APPROVAL
06046          GO TO 6170-WRITE.
06047
06048      IF WS-APPROVAL-LEVEL < AT-APPROVAL-LEVEL-REQD
031808         MOVE 'Y' TO WS-NEED-APPROVAL
06049          IF WS-APPROVAL-LEVEL = '1'
06050              MOVE '2'        TO  AT-APPROVED-LEVEL
031808                            WS-SAVE-APPROVED-LEVEL
06051              GO TO 6170-WRITE
06052          ELSE
06053              IF WS-APPROVAL-LEVEL = '2'
06054                  MOVE '3'        TO  AT-APPROVED-LEVEL
031808                                WS-SAVE-APPROVED-LEVEL
06055                  GO TO 6170-WRITE
031808             ELSE IF WS-APPROVAL-LEVEL = '3'
031808                 MOVE '4'        TO  AT-APPROVED-LEVEL
031808                                WS-SAVE-APPROVED-LEVEL
091813                 GO TO 6170-WRITE
091813             ELSE IF WS-APPROVAL-LEVEL = '4'
091813                 MOVE '5'        TO  AT-APPROVED-LEVEL
091813                                WS-SAVE-APPROVED-LEVEL
06057                  GO TO 6170-WRITE.
06058
06059      MOVE 'A'            TO  AT-PAYMENT-APPROVAL-SW
06060                              WS-PMT-APPROVAL-SW.
06061      MOVE SPACES         TO  AT-APPROVED-LEVEL
06062                              AT-APPROVAL-LEVEL-REQD.
06063
06064 *******************************************************
110515 6155-READ-CONTROL.
05985      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
05986      MOVE '1'                    TO CNTL-REC-TYPE.
05987      MOVE SPACES                 TO CNTL-ACCESS.
05988      MOVE +0                     TO CNTL-SEQ-NO.
05989      MOVE 'COMP'                 TO FILE-SWITCH.
05990
05991      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.
110515 6157-SET-APP-LEVEL.
031808******A&H Limits are set as a monthly payment.  Calculate the mont
031808******equivalent of the amount paid today
031808     IF PI-SAVE-ELAPSED-MONTHS > 0 OR
031808        PI-SAVE-ODD-DAYS-OVER > 0
031808           COMPUTE WS-CALC-PMT-MONTHS = (PI-SAVE-ODD-DAYS-OVER
031808                               / 30) + PI-SAVE-ELAPSED-MONTHS
031808           COMPUTE WS-CALC-MO-AH-AMT ROUNDED = AT-AMOUNT-PAID /
031808                                  WS-CALC-PMT-MONTHS
031808     ELSE
031808           MOVE AT-AMOUNT-PAID TO WS-CALC-MO-AH-AMT
031808     END-IF.
031808*06007      IF ((PAID-FOR-AH) AND
031808     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808*06008         (CF-AH-PAY-APP-LEVEL-1 >= AT-AMOUNT-PAID))
031808        (CF-AH-PAY-APP-LEVEL-1 >= WS-CALC-MO-AH-AMT))
06009               OR
100518        ((PAID-FOR-LIFE OR PAID-FOR-OTH) AND
06011         (CF-LIFE-PAY-APP-LEVEL-1 >= AT-AMOUNT-PAID))
06012             MOVE '1'         TO AT-APPROVAL-LEVEL-REQD
06013        ELSE
031808*06014      IF ((PAID-FOR-AH)  AND
031808         IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808*06015         (CF-AH-PAY-APP-LEVEL-2 >= AT-AMOUNT-PAID))
031808           (CF-AH-PAY-APP-LEVEL-2 >= WS-CALC-MO-AH-AMT))
06016               OR
100518        ((PAID-FOR-LIFE OR PAID-FOR-OTH) AND
06018         (CF-LIFE-PAY-APP-LEVEL-2 >= AT-AMOUNT-PAID))
06019             MOVE '2'         TO AT-APPROVAL-LEVEL-REQD
06020          ELSE
031808*06021             MOVE '3'         TO AT-APPROVAL-LEVEL-REQD.
031808           IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617                OR PAID-FOR-FAM
022122                OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808               (CF-AH-PAY-APP-LEVEL-3 >= WS-CALC-MO-AH-AMT))
031808              OR
100518          ((PAID-FOR-LIFE OR PAID-FOR-OTH) AND
031808          (CF-LIFE-PAY-APP-LEVEL-3 >= AT-AMOUNT-PAID))
031808            MOVE '3'         TO AT-APPROVAL-LEVEL-REQD
031808       ELSE
091813           IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617                OR PAID-FOR-FAM
022122                OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
091813               (CF-AH-PAY-APP-LEVEL-4 >= WS-CALC-MO-AH-AMT))
091813              OR
100518          ((PAID-FOR-LIFE OR PAID-FOR-OTH) AND
091813          (CF-LIFE-PAY-APP-LEVEL-4 >= AT-AMOUNT-PAID))
091813            MOVE '4'         TO AT-APPROVAL-LEVEL-REQD
091813       ELSE
091813            MOVE '5'         TO AT-APPROVAL-LEVEL-REQD.
031808
031808     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808        (CF-AH-APP-DAY-LEVEL-1 >= AT-DAYS-IN-PERIOD))
031808        IF ('1' >= AT-APPROVAL-LEVEL-REQD)
031808            MOVE '1'         TO AT-APPROVAL-LEVEL-REQD
031808        END-IF
031808     ELSE
031808     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808        (CF-AH-APP-DAY-LEVEL-2 >= AT-DAYS-IN-PERIOD))
031808        IF ('2' >= AT-APPROVAL-LEVEL-REQD)
031808            MOVE '2'         TO AT-APPROVAL-LEVEL-REQD
031808        END-IF
031808     ELSE
031808     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808        (CF-AH-APP-DAY-LEVEL-3 >= AT-DAYS-IN-PERIOD))
031808        IF ('3' >= AT-APPROVAL-LEVEL-REQD)
031808            MOVE '3'         TO AT-APPROVAL-LEVEL-REQD
031808        END-IF
031808     ELSE
091813     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
091813        (CF-AH-APP-DAY-LEVEL-4 >= AT-DAYS-IN-PERIOD))
091813        IF ('4' >= AT-APPROVAL-LEVEL-REQD)
091813            MOVE '4'         TO AT-APPROVAL-LEVEL-REQD
091813        END-IF
091813     ELSE
031808     IF (PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS)
091813            MOVE '5'         TO AT-APPROVAL-LEVEL-REQD.
06065
06066  6170-WRITE.
06067
120115     if pi-company-id = 'DCC' or 'VPP'
061013        PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
061013     end-if
061013     perform 2220-update-elcrtt  thru 2220-exit
06068      IF PI-COMPANY-ID = 'DMD'
06069         IF PI-AT-EOB-CODES NOT = SPACES
06070            MOVE PI-AT-EOB-CODE (1) TO AT-EOB-CODE1
06071            MOVE PI-AT-EOB-CODE (2) TO AT-EOB-CODE2
06072            MOVE PI-AT-EOB-CODE (3) TO AT-EOB-CODE3
06073            MOVE PI-AT-EOB-CODE (4) TO AT-EOB-CODE4
06074            MOVE PI-AT-EOB-CODE (5) TO AT-EOB-CODE5.
06075
06076      
      * EXEC CICS HANDLE CONDITION
06077 *        DUPREC    (6180-DUPREC)
06078 *    END-EXEC.
      *    MOVE '"$%                   ! 3 #00015453' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303135343533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06079
06080      
      * EXEC CICS WRITE
06081 *         DATASET       ('ELTRLR')
06082 *         FROM          (ACTIVITY-TRAILERS)
06083 *         RIDFLD        (AT-CONTROL-PRIMARY)
06084 *     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00015457' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135343537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06085
06086      IF (PI-PMTNOTE1 = SPACES OR LOW-VALUES) AND
06087         (PI-PMTNOTE2 = SPACES OR LOW-VALUES)
061010*06088           GO TO 6199-EXIT.
061010           GO TO 6176-APPROVAL-NOTE.
06089
06090      
      * EXEC CICS GETMAIN
06091 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
06092 *         LENGTH    (200)
06093 *         INITIMG   (GETMAIN-SPACE)
06094 *     END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00015468' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303135343638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06095
06096      SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.
06097
06098      MOVE ELMSTR-KEY             TO AT-CONTROL-PRIMARY.
06099      MOVE 'AT'                   TO AT-RECORD-ID.
06100      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.
06101      MOVE '6'                    TO AT-TRAILER-TYPE.
06102      MOVE WS-TODAY-DATE          TO AT-RECORDED-DT
06103                                     AT-GEN-INFO-LAST-MAINT-DT
06104      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
06105                                     AT-GEN-INFO-LAST-UPDATED-BY
06106      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
06107
06108      MOVE PI-PMTNOTE1            TO AT-INFO-LINE-1.
06109      MOVE PI-PMTNOTE2            TO AT-INFO-LINE-2
           MOVE PI-EOB-CODES-EXIST     TO AT-EOB-CODES-EXIST
06110
06111      MOVE 'P'                    TO AT-INFO-TRAILER-TYPE.
06112
06113  6175-WRITE.
06114
06115      
      * EXEC CICS HANDLE CONDITION
06116 *        DUPREC    (6190-DUPREC)
06117 *    END-EXEC.
      *    MOVE '"$%                   ! 4 #00015494' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303135343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06118
06119      
      * EXEC CICS WRITE
06120 *         DATASET     ('ELTRLR')
06121 *         FROM        (ACTIVITY-TRAILERS)
06122 *         RIDFLD      (AT-CONTROL-PRIMARY)
06123 *     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00015498' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135343938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06124
061010 6176-APPROVAL-NOTE.
061010
061010     IF NOT APPROVAL-NEEDED
061010         GO TO 6199-EXIT.
061010
061010     
      * EXEC CICS GETMAIN
061010*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
061010*        LENGTH    (200)
061010*        INITIMG   (GETMAIN-SPACE)
061010*    END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00015509' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303135353039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
061010
061010     SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT
061010
061010     MOVE ELMSTR-KEY         TO AT-CONTROL-PRIMARY
061010     MOVE 'AT'               TO AT-RECORD-ID
061010     MOVE CL-TRAILER-SEQ-CNT TO AT-SEQUENCE-NO
061010     MOVE '6'                TO AT-TRAILER-TYPE
061010     MOVE WS-TODAY-DATE      TO AT-RECORDED-DT
061010                                AT-GEN-INFO-LAST-MAINT-DT
061010     MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY
061010                                AT-GEN-INFO-LAST-UPDATED-BY
061010     MOVE EIBTIME            TO AT-LAST-MAINT-HHMMSS
061010
032813     IF PI-REAUDIT-NEEDED = 'Y'
032813         MOVE WS-REAUDIT-NOTE TO AT-INFO-LINE-1
032813     ELSE
032813         MOVE WS-APPROV-NOTE  TO AT-INFO-LINE-1
032813     END-IF
052814     IF PI-APPROVAL-3550-NEEDED = 'Y'
052814         MOVE WS-3550-APPROV-NOTE TO AT-INFO-LINE-1
052814     END-IF
043019     IF PI-DUPE-APPROVAL-NEEDED = 'Y'
043019         MOVE WS-DUPE-APPROV-NOTE TO AT-INFO-LINE-1
043019     END-IF
043019
061010     MOVE SPACES             TO AT-INFO-LINE-2
061010
061010     MOVE 'R'                TO AT-INFO-TRAILER-TYPE.
061010 6177-WRITE.
061010
061010     
      * EXEC CICS HANDLE CONDITION
061010*        DUPREC    (6185-DUPREC)
061010*    END-EXEC.
      *    MOVE '"$%                   ! 5 #00015544' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303135353434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
061010
061010     
      * EXEC CICS WRITE
061010*         DATASET     ('ELTRLR')
061010*         FROM        (ACTIVITY-TRAILERS)
061010*         RIDFLD      (AT-CONTROL-PRIMARY)
061010*     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00015548' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135353438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
031808
031808
06125      GO TO 6199-EXIT.
06126
06127  6180-DUPREC.
06128
06129      SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.
06130      MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.
06131      GO TO 6170-WRITE.
031808
031808 6185-DUPREC.
031808     SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.
031808     MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.
031808     GO TO 6177-WRITE.
031808
06133  6190-DUPREC.
06134      SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.
06135      MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.
06136      GO TO 6175-WRITE.
06137
06138  6199-EXIT.
06139       EXIT.
06140
06141      EJECT
06142  6200-UPDATE-CLAIM-MSTR.
06143      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
06144      MOVE PI-CARRIER             TO MSTR-CARRIER.
06145      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
06146      MOVE PI-CERT-NO             TO MSTR-CERT-NO.
06147
06148      PERFORM 7910-READ-CLAIM-UPDATE THRU 7910-EXIT.
06149
06150      MOVE ZEROS                  TO W-INTEREST.
06151
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
PEMMOD        IF (PI-OFFLINE = 'Y') OR
PEMMOD           (PI-PMTTYPE = '7')
CIDMOD           PERFORM 9870-OUTPUT-ACTIVITY-RECORD
PEMMOD                                 THRU 9870-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
CIDMOD     IF NOT ERROR-ON-OUTPUT
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        
      * EXEC CICS REWRITE
CIDMOD*          DATASET('ELMSTR')
CIDMOD*          FROM(CLAIM-MASTER)
CIDMOD*       END-EXEC
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&& L                  %   #00015598' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303135353938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD
CIDMOD        MOVE -1                  TO ENTPFBL
CIDMOD        MOVE AL-UANON            TO ENTPFBA
CIDMOD        MOVE MAP-NAMEB           TO MAP-NAME
CIDMOD        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
CIDMOD        GO TO 8200-SEND-DATAONLY
CIDMOD     END-IF
CIDMOD
06152      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
06153
06154      MOVE SPACES TO WS-PAYMENT-TRAILER-SW.
06155
06156      IF PI-AIG-SPECIAL-BENEFIT
06157          MOVE '1' TO WS-PAYMENT-TRAILER-SW.
06158
06159      PERFORM 6100-BUILD-PAYMENT-TRAILER THRU 6199-EXIT.
06160
06161      IF PI-SPECIAL-BEN-SW = 'Y'
06162          MOVE '2' TO WS-PAYMENT-TRAILER-SW
06163          PERFORM 6130-BUILD-TRAILER THRU 6199-EXIT.
06164
06165      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
06166          PERFORM 6400-UPDATE-ZERO-TRAILER THRU 6499-EXIT
06167      ELSE
06168          IF PI-PMTTYPE NOT = '4'
06169             PERFORM 6400-UPDATE-ZERO-TRAILER THRU 6499-EXIT.
06170
06171      IF PI-PMTTYPE = '5' OR '6'
06172         GO TO  6250-SET-MAINT.
06173
06174      MOVE ZEROS                      TO  CL-LAPSE-REPORT-CODE
06175                                          CL-LAG-REPORT-CODE.
06176
06177      IF CL-ACTIVITY-CODE = 09
06178          GO TO 6200-CONT-UPDATE.
06179
06180      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
06181          IF CL-ACTIVITY-CODE = 11
06182              GO TO 6200-CONT-UPDATE.
06183
06184      IF WS-ACT-REC-FOUND-SW = 'N'
06185          GO TO 6200-CONT-UPDATE.
06186
06187      IF PI-RESET-SW = 'Y' OR PI-PROV-PMT
06188          MOVE WS-TODAY-DATE          TO  CL-ACTIVITY-MAINT-DT
06189          MOVE 'PMT'                  TO  CL-ACTIVITY-MAINT-TYPE
06190          MOVE WS-ACTIVITY-CODE       TO  CL-ACTIVITY-CODE.
06191
06192      IF WS-UPDATE-SW = 'Y'
06193          MOVE LOW-VALUES             TO  CL-NEXT-RESEND-DT
06194                                          CL-NEXT-FOLLOWUP-DT.
06195
06196  6200-CONT-UPDATE.
06197
06198      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
06199        IF CLAIM-IS-CLOSED
06200          IF PI-PMTTYPE = '2' OR '3' OR '4'
06201            IF WS-OPEN-CLOSE-SW = '2'
06202              MOVE WS-TODAY-DATE     TO  CL-LAST-REOPEN-DT
06203                                         CL-LAST-CLOSE-DT.
06204
06205      IF CLAIM-IS-CLOSED AND PI-PMTTYPE = '1'
06206         MOVE WS-TODAY-DATE       TO CL-LAST-REOPEN-DT
06207         MOVE 'O'                 TO CL-CLAIM-STATUS.
06208
06209      IF PI-PMTTYPE = ('2' OR '3') AND CLAIM-IS-OPEN
06210         MOVE WS-TODAY-DATE       TO CL-LAST-CLOSE-DT
06211         MOVE '1'                 TO CL-LAST-CLOSE-REASON
06212         MOVE 'C'                 TO CL-CLAIM-STATUS
06213         IF PI-PMTTYPE = '2'
121802           AND (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
022122              or 'F' OR 'B' OR 'H')
121802           CONTINUE
06216         ELSE
06217            PERFORM 6300-UPDATE-CERT THRU 6399-EXIT
              END-IF
06218      ELSE
06219         IF PI-PMTTYPE = '2' OR '3'
06220            IF PI-PMTTYPE = '2' AND
121802              (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
022122                 or 'F' OR 'B' OR 'H')
121802              CONTINUE
06223            ELSE
06224               PERFORM 6300-UPDATE-CERT THRU 6399-EXIT
                 END-IF
              END-IF
           END-IF
06225
06226      IF PI-PMTTYPE = '4'
06227         PERFORM 6300-UPDATE-CERT THRU 6399-EXIT.
06228
06229      IF PI-COMPANY-ID = 'CVL'
06230          IF PI-LOAN-UPDATE-SW = 'Y'
06231              MOVE PI-COMPANY-CD      TO  CERT-COMP-CD
06232              MOVE CL-CERT-CARRIER    TO  CERT-CARRIER
06233              MOVE CL-CERT-GROUPING   TO  CERT-GROUPING
06234              MOVE CL-CERT-STATE      TO  CERT-STATE
06235              MOVE CL-CERT-ACCOUNT    TO  CERT-ACCOUNT
06236              MOVE CL-CERT-EFF-DT     TO  CERT-EFF-DT
06237              MOVE CL-CERT-NO         TO  CERT-CERT-NO
06238              MOVE 'CERT'             TO  FILE-SWITCH
06239              PERFORM 7980-READ-CERT-UPDATE THRU 7980-EXIT
06240              MOVE PI-LOAN-NO         TO  CM-BENEFICIARY
06241              PERFORM 6350-REWRITE-CERT THRU 6399-EXIT.
06242
06243      IF PI-PFKEY-USED = DFHPF5 AND PI-PROCESSOR-ID NOT = 'FIX '
06244         GO TO 6220-USE-COMPUTED.
06245
06246      IF (PI-PMTTYPE = '4')  AND
06247         (PI-EPYAMT < ZEROS)
06248           COMPUTE CL-NO-OF-DAYS-PAID =
06249                        CL-NO-OF-DAYS-PAID - PI-EDAYS
06250           IF CL-NO-OF-DAYS-PAID < +0
06251               MOVE ZEROS TO CL-NO-OF-DAYS-PAID
06252           ELSE
06253               NEXT SENTENCE
06254      ELSE
06255           ADD PI-EDAYS           TO CL-NO-OF-DAYS-PAID.
06256
031218     IF HOLD-UNTIL-PMT
022718        CONTINUE
022718     ELSE
06257      IF PI-PROCESSOR-ID = 'FIX '
06258         IF CL-TOTAL-PAID-AMT NOT = ZEROS
06259            NEXT SENTENCE
06260         ELSE
06261            ADD PI-EPYAMT         TO CL-TOTAL-PAID-AMT
06262      ELSE
06263         ADD PI-EPYAMT            TO CL-TOTAL-PAID-AMT.
06264
06265      IF PI-PMTTYPE = '4'
06266         GO TO 6250-SET-MAINT.
06267
06270      MOVE PI-EPYTHRU             TO DC-GREG-DATE-1-MDY.
06271      MOVE '4'                    TO DC-OPTION-CODE.
06272      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
031218     IF HOLD-UNTIL-PMT
022718        CONTINUE
022718     ELSE
06273      IF DATE-CONVERSION-ERROR
06274         MOVE LOW-VALUES          TO CL-PAID-THRU-DT
06275      ELSE
022718        ADD 1                 TO CL-NO-OF-PMTS-MADE
06276         IF DC-BIN-DATE-1 > CL-PAID-THRU-DT
06277            MOVE DC-BIN-DATE-1    TO CL-PAID-THRU-DT
06278            IF PI-AIG-SPECIAL-BENEFIT
06279                COMPUTE CL-LAST-PMT-AMT = PI-EPYAMT / 1.5
06280            ELSE
06281                MOVE PI-EPYAMT        TO CL-LAST-PMT-AMT.
06282
06283      GO TO 6250-SET-MAINT.
06284
06285  6220-USE-COMPUTED.
06286
06287      IF (PI-PMTTYPE = '4')  AND
06288         (PI-CPYAMT < ZEROS)
06289           COMPUTE CL-NO-OF-DAYS-PAID =
06290                        CL-NO-OF-DAYS-PAID - PI-CDAYS
06291           IF CL-NO-OF-DAYS-PAID < +0
06292               MOVE ZEROS TO CL-NO-OF-DAYS-PAID
06293           ELSE
06294               NEXT SENTENCE
06295      ELSE
06296           ADD PI-CDAYS           TO CL-NO-OF-DAYS-PAID.
06297
031218     IF NOT HOLD-UNTIL-PMT
022718        ADD 1                    TO CL-NO-OF-PMTS-MADE
06298         ADD PI-CPYAMT            TO CL-TOTAL-PAID-AMT.
06299
06300      IF PI-PMTTYPE = '4'
06301         GO TO 6250-SET-MAINT.
06302
06303      IF PI-AIG-SPECIAL-BENEFIT
06304          COMPUTE CL-LAST-PMT-AMT = PI-CPYAMT / 1.5
06305      ELSE
06306          MOVE PI-CPYAMT        TO CL-LAST-PMT-AMT.
06307
06310      IF PI-CPYTHRU > CL-PAID-THRU-DT
031218       AND NOT HOLD-UNTIL-PMT
06311         MOVE PI-CPYTHRU          TO CL-PAID-THRU-DT.
06312
06313  6250-SET-MAINT.
06314      MOVE WS-TODAY-DATE          TO CL-LAST-MAINT-DT.
06315
06316      IF PI-PROCESSOR-ID = 'FIX '
06317         IF DC-BIN-DATE-1 = CL-PAID-THRU-DT
06318            MOVE WS-TODAY-DATE    TO CL-LAST-PMT-DT
06319         ELSE
06320            NEXT SENTENCE
06321      ELSE
06322         MOVE WS-TODAY-DATE       TO CL-LAST-PMT-DT.
06323
06324      MOVE PI-PROCESSOR-ID        TO CL-LAST-MAINT-USER.
06325      MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.
06326      MOVE '1'                    TO CL-LAST-MAINT-TYPE.
06327
06328  6299-EXIT.
06329       EXIT.
06330      EJECT
06331  6300-UPDATE-CERT.
041710
041710*  ADD CERTNOTE FOR SC NP+6
041710     IF PI-COMPANY-ID = 'CID' AND
041710       (PI-PMTTYPE = '2' OR '3') AND
041710        CL-CERT-STATE = 'SC'  AND
041710        (PI-LF-BENEFIT-CD = '2I' OR '2J' OR '2K' OR '2L')
041710           PERFORM 7180-ADD-CERT-NOTE THRU 7189-EXIT
041710     END-IF.
041710
030512     IF PI-COMPANY-ID = 'AHL' AND
030512       (PI-PMTTYPE = '2' OR '3') AND
030512        (PI-LF-BENEFIT-CD = '5I' OR '6J' OR '5M' OR '6M' OR
                 '5G' OR '5J' OR '5S' OR '6D' OR '6H' OR '6R')
030512           PERFORM 7180-ADD-CERT-NOTE THRU 7189-EXIT
030512     END-IF.
030512
06332      MOVE PI-COMPANY-CD    TO CERT-COMP-CD.
06333      MOVE CL-CERT-CARRIER  TO CERT-CARRIER.
06334      MOVE CL-CERT-GROUPING TO CERT-GROUPING.
06335      MOVE CL-CERT-STATE    TO CERT-STATE.
06336      MOVE CL-CERT-ACCOUNT  TO CERT-ACCOUNT.
06337      MOVE CL-CERT-EFF-DT   TO CERT-EFF-DT.
06338      MOVE CL-CERT-NO       TO CERT-CERT-NO.
06339      MOVE 'CERT'           TO FILE-SWITCH.
06340
06341      PERFORM 7980-READ-CERT-UPDATE THRU 7980-EXIT.
041710
041710     IF CERT-NOTE-ADDED
041710        IF CM-NOTE-SW = ' '
041710           MOVE '4'             TO CM-NOTE-SW
041710        ELSE
041710           IF CM-NOTE-SW = '1'
041710               MOVE '5'           TO CM-NOTE-SW
041710           ELSE
041710              IF CM-NOTE-SW = '2'
041710                 MOVE '6'         TO CM-NOTE-SW
041710              ELSE
041710                 IF CM-NOTE-SW = '3'
041710                    MOVE '7'      TO CM-NOTE-SW
041710                 END-IF
041710              END-IF
041710           END-IF
041710        END-IF
041710     END-IF.
06342
06343      IF PI-COMPANY-ID = 'CVL'
06344          MOVE PI-LOAN-NO   TO  CM-BENEFICIARY
06345          MOVE 'N'          TO  PI-LOAN-UPDATE-SW.
06346
06347      IF PI-PMTTYPE NOT = '4'
06348         GO TO 6340-UPDATE-CERT.
06349
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
06351         IF PI-PFKEY-USED = DFHPF5
06352            ADD PI-CPYAMT         TO CM-LF-ITD-DEATH-AMT
06353         ELSE
06354            ADD PI-EPYAMT         TO CM-LF-ITD-DEATH-AMT.
06355
121802*    IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
121802*        IF PI-COMPANY-ID = 'ACC' OR 'FDL'
121802*            IF PI-LIFE-OVERRIDE-L1 = 'P' OR
121802*               PI-LF-COVERAGE-TYPE = 'P'
121802*                GO TO 6350-REWRITE-CERT.
06361
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
06363        IF PI-LIFE-OVERRIDE-L1 = 'P' OR
06364           PI-LF-COVERAGE-TYPE = 'P'
06365            IF CM-LF-ITD-DEATH-AMT >= CM-LF-BENEFIT-AMT
06366                 MOVE CL-INCURRED-DT       TO CM-LF-DEATH-DT
06367                 MOVE PI-MONTH-END-SAVE    TO CM-LF-DEATH-EXIT-DT
06368                 MOVE CM-LF-CURRENT-STATUS TO CM-LF-STATUS-AT-DEATH
06369                 MOVE '7'                  TO CM-LF-CURRENT-STATUS
06370                 GO TO 6350-REWRITE-CERT
06371             ELSE
06372                 GO TO 6350-REWRITE-CERT.
06373
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
06375          GO TO 6350-REWRITE-CERT.
06376
06377      IF NOT CM-AH-LUMP-SUM-DISAB
06378         GO TO 6380-UNLOCK-CERT.
06379
06380      IF PI-PFKEY-USED = DFHPF5
06381         ADD PI-CPYAMT            TO CM-AH-ITD-LUMP-PMT
06382                                     CM-AH-ITD-AH-PMT
06383         MOVE PI-EPYTHRU          TO CM-AH-PAID-THRU-DT
06384         GO TO 6350-REWRITE-CERT
06385      ELSE
06386         ADD PI-EPYAMT            TO CM-AH-ITD-LUMP-PMT
06387                                     CM-AH-ITD-AH-PMT
06388         MOVE PI-EPYTHRU          TO CM-AH-PAID-THRU-DT
06389         GO TO 6350-REWRITE-CERT.
06390
06391  6340-UPDATE-CERT.
06392
100518     IF CL-CLAIM-TYPE = 'L' OR 'O'
06394          IF PI-PFKEY-USED = DFHPF5
06395              ADD PI-CPYAMT          TO CM-LF-ITD-DEATH-AMT
06396          ELSE
06397              ADD PI-EPYAMT          TO CM-LF-ITD-DEATH-AMT.
06398
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
06400          IF PI-COMPANY-ID = 'ACC' OR 'FDL'
06401              IF PI-LIFE-OVERRIDE-L1 = 'P' OR
06402                 PI-LF-COVERAGE-TYPE = 'P'
06403                  GO TO 6350-REWRITE-CERT.
06404
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
06406        IF PI-LIFE-OVERRIDE-L1 = 'P' OR
06407           PI-LF-COVERAGE-TYPE = 'P'
06408            IF CM-LF-ITD-DEATH-AMT >= CM-LF-BENEFIT-AMT
06409                 MOVE CL-INCURRED-DT       TO CM-LF-DEATH-DT
06410                 MOVE PI-MONTH-END-SAVE    TO CM-LF-DEATH-EXIT-DT
06411                 MOVE CM-LF-CURRENT-STATUS TO CM-LF-STATUS-AT-DEATH
06412                 MOVE '7'                  TO CM-LF-CURRENT-STATUS
06413                 GO TO 6350-REWRITE-CERT.
06414
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
06416          IF PI-LIFE-OVERRIDE-L1 = ('L' OR 'O') AND
06417             PI-LF-COVERAGE-TYPE NOT = 'P'
06418              MOVE CL-INCURRED-DT         TO CM-LF-DEATH-DT
06419              MOVE PI-MONTH-END-SAVE      TO CM-LF-DEATH-EXIT-DT
06420              MOVE CM-LF-CURRENT-STATUS   TO CM-LF-STATUS-AT-DEATH
06421              MOVE '7'                    TO CM-LF-CURRENT-STATUS
06422              GO TO 6350-REWRITE-CERT.
06423
100518     IF CL-CLAIM-TYPE = 'O'
100518         IF PI-LIFE-OVERRIDE-L1 = ('L' OR 'O') AND
100518            PI-LF-COVERAGE-TYPE NOT = 'P'
100518             MOVE CL-INCURRED-DT         TO CM-LF-CANCEL-DT
100518             MOVE PI-MONTH-END-SAVE      TO CM-LF-CANCEL-EXIT-DT
100518             MOVE CM-LF-CURRENT-STATUS   TO CM-LF-STATUS-AT-CANCEL
100518             MOVE '6'            TO CM-LF-CURRENT-STATUS
100518             GO TO 6350-REWRITE-CERT.
100518
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                          OR 'B' or 'H'
06425          MOVE CL-INCURRED-DT         TO CM-AH-SETTLEMENT-DT
06426          MOVE PI-MONTH-END-SAVE      TO CM-AH-SETTLEMENT-EXIT-DT
06427          MOVE CM-AH-CURRENT-STATUS   TO CM-AH-STATUS-AT-SETTLEMENT
06428          MOVE '6'                    TO CM-AH-CURRENT-STATUS
06429          MOVE PI-EPYTHRU             TO CM-AH-PAID-THRU-DT
06430          IF PI-PFKEY-USED = DFHPF5
06431              ADD PI-CPYAMT           TO CM-AH-ITD-LUMP-PMT
06432                                         CM-AH-ITD-AH-PMT
06433              GO TO 6350-REWRITE-CERT
06434          ELSE
06435              ADD PI-EPYAMT           TO CM-AH-ITD-LUMP-PMT
06436                                         CM-AH-ITD-AH-PMT
06437              GO TO 6350-REWRITE-CERT.
06438
06439  6350-REWRITE-CERT.
06440
06441      
      * EXEC CICS REWRITE
06442 *         DATASET  ('ELCERT')
06443 *         FROM(CERTIFICATE-MASTER)
06444 *     END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&& L                  %   #00015955' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303135393535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06445
06446      GO TO 6399-EXIT.
06447
06448  6380-UNLOCK-CERT.
06449      
      * EXEC CICS UNLOCK
06450 *         DATASET  ('ELCERT')
06451 *     END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&*                    #   #00015963' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303135393633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06452
06453  6399-EXIT.
06454       EXIT.
06455      EJECT
06456  6400-UPDATE-ZERO-TRAILER.
06457      MOVE ELMSTR-KEY             TO ELTRLR-KEY.
06458      MOVE ZEROS                  TO TRLR-SEQ-NO.
06459
06460      PERFORM 7960-READ-TRAILER-UPDATE THRU 7960-EXIT.
06461
06462      IF PI-EPYAMT NOT = ZEROS
06463          SUBTRACT PI-EPYAMT    FROM AT-CURRENT-MANUAL-RESERVE
06464      ELSE
06465          SUBTRACT PI-CPYAMT    FROM AT-CURRENT-MANUAL-RESERVE.
06466
06467      IF PI-ERESV NOT = ZEROS
06468         ADD PI-ERESV             TO AT-CURRENT-MANUAL-RESERVE.
06469
06470      IF AT-CURRENT-MANUAL-RESERVE NEGATIVE
06471         MOVE ZEROS               TO AT-CURRENT-MANUAL-RESERVE.
06472
06473      IF PI-PFKEY-USED = DFHPF3 OR DFHPF4
06474         ADD PI-EEXPENS           TO AT-ITD-CHARGEABLE-EXPENSE
06475      ELSE
06476         ADD PI-CEXPENS           TO AT-ITD-CHARGEABLE-EXPENSE.
06477
06478      IF PI-PMTTYPE = '5'
06479         IF PI-EPYAMT = ZEROS
06480            ADD PI-CPYAMT         TO AT-ITD-CHARGEABLE-EXPENSE
06481         ELSE
06482            ADD PI-EPYAMT         TO AT-ITD-CHARGEABLE-EXPENSE.
06483
06484      IF PI-PMTTYPE = '6'
06485         IF PI-EPYAMT = ZEROS
06486            ADD PI-CPYAMT         TO AT-ITD-PAID-EXPENSES
06487         ELSE
06488            ADD PI-EPYAMT         TO AT-ITD-PAID-EXPENSES.
06489
06490  6420-CHECK-OPEN-CLOSE.
06491      IF PI-PMTTYPE = '5' OR '6'
06492         GO TO 6450-REWRITE.
06493
06494      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
06495          IF PI-PMTTYPE = '2' OR '3' OR '4'
06496              IF CLAIM-IS-CLOSED
06497                  MOVE '1'        TO  WS-OPEN-CLOSE-SW
06498                  MOVE 1          TO  SUB-1
06499                  GO TO 6440-AIG-LOOP.
06500
06501      IF (PI-PMTTYPE = '1' OR '4') AND CLAIM-IS-OPEN
06502         GO TO 6450-REWRITE.
06503
06504      IF (PI-PMTTYPE = '2' OR '3') AND CLAIM-IS-CLOSED
06505         GO TO 6450-REWRITE.
06506
06507      MOVE 1                      TO SUB-1.
06508
06509  6430-LOOP.
06510      IF AT-OPEN-CLOSE-TYPE (SUB-1) = SPACES
06511         MOVE WS-TODAY-DATE       TO AT-OPEN-CLOSE-DATE (SUB-1)
06512         IF PI-PMTTYPE = '1' OR '4'
06513            MOVE 'O'              TO AT-OPEN-CLOSE-TYPE (SUB-1)
06514            MOVE 'FORCE'          TO AT-OPEN-CLOSE-REASON (SUB-1)
06515            GO TO 6450-REWRITE
06516         ELSE
06517            MOVE 'C'              TO AT-OPEN-CLOSE-TYPE (SUB-1)
06518            MOVE 'FINAL'          TO AT-OPEN-CLOSE-REASON (SUB-1)
06519            GO TO 6450-REWRITE.
06520
06521      IF SUB-1 = 6
06522       MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1)
06523       MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2)
06524       MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3)
06525       MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4)
06526       MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5)
06527       MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6)
06528       GO TO 6430-LOOP.
06529
06530      ADD 1                       TO SUB-1.
06531      GO TO 6430-LOOP.
06532
06533
06534  6440-AIG-LOOP.
06535
06536      IF AT-OPEN-CLOSE-TYPE (SUB-1) = SPACES
06537        MOVE WS-TODAY-DATE        TO AT-OPEN-CLOSE-DATE (SUB-1)
06538        IF WS-OPEN-CLOSE-SW = '1'
06539          MOVE 'O'                TO  AT-OPEN-CLOSE-TYPE (SUB-1)
06540          IF PI-PMTTYPE = '2'
06541            MOVE 'FINAL'          TO  AT-OPEN-CLOSE-REASON (SUB-1)
06542            MOVE '2'              TO  WS-OPEN-CLOSE-SW
06543          ELSE
06544            IF PI-PMTTYPE = '3'
06545              MOVE 'SETTL'        TO  AT-OPEN-CLOSE-REASON (SUB-1)
06546              MOVE '2'            TO  WS-OPEN-CLOSE-SW
06547            ELSE
06548              IF PI-PMTTYPE = '4'
06549                MOVE 'ADDL'       TO  AT-OPEN-CLOSE-REASON (SUB-1)
06550                MOVE '2'          TO  WS-OPEN-CLOSE-SW
06551              ELSE
06552                MOVE 'UNK '       TO  AT-OPEN-CLOSE-REASON (SUB-1)
06553                MOVE '2'          TO  WS-OPEN-CLOSE-SW
06554        ELSE
06555          MOVE 'C'                TO  AT-OPEN-CLOSE-TYPE (SUB-1)
06556          MOVE 'FORCE'            TO  AT-OPEN-CLOSE-REASON (SUB-1)
06557          GO TO 6450-REWRITE.
06558
06559      IF SUB-1 = 6
06560       MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1)
06561       MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2)
06562       MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3)
06563       MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4)
06564       MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5)
06565       MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6)
06566       GO TO 6440-AIG-LOOP.
06567
06568      ADD 1                       TO SUB-1.
06569      GO TO 6440-AIG-LOOP.
06570
06571  6450-REWRITE.
06572      MOVE PI-PROCESSOR-ID     TO AT-RESERVES-LAST-UPDATED-BY.
06573      MOVE EIBTIME             TO AT-LAST-MAINT-HHMMSS.
06574      MOVE WS-TODAY-DATE       TO AT-RESERVES-LAST-MAINT-DT.
06575
06576      IF PI-COMPANY-ID = 'DMD'
06577          MOVE PI-AT-EOB-CODE (1) TO AT-EOB-CODE1
06578          MOVE PI-AT-EOB-CODE (2) TO AT-EOB-CODE2
06579          MOVE PI-AT-EOB-CODE (3) TO AT-EOB-CODE3
06580          MOVE PI-AT-EOB-CODE (4) TO AT-EOB-CODE4
06581          MOVE PI-AT-EOB-CODE (5) TO AT-EOB-CODE5.
06582
06583      
      * EXEC CICS REWRITE
06584 *         DATASET  ('ELTRLR')
06585 *         FROM     (ACTIVITY-TRAILERS)
06586 *     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00016097' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303136303937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06587
06588  6499-EXIT.
06589       EXIT.
06590
06591  EJECT
06592  6500-READ-DLO035.
06593      MOVE 'E'                    TO DL35-PROCESS-TYPE.
06594      MOVE CL-CCN-A5              TO DL35-CREDIT-CARD.
06595      MOVE CL-CERT-NO (5:2)       TO DL35-BEN-TYPE.
06596      MOVE PI-EPYAMT              TO DL35-PAYMENT-AMT.
06597
06598      
      * EXEC CICS LINK
06599 *        PROGRAM    ('DLO035')
06600 *        COMMAREA   (DLO035-KEY)
06601 *        LENGTH     (DL35-LENGTH)
06602 *    END-EXEC.
           MOVE 'DLO035' TO DFHEIV1
      *    MOVE '."C                   (   #00016112' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136313132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO035-KEY, 
                 DL35-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06603
06604      IF DL35-RETURN-CODE = 'OK'
06605          GO TO 6500-EXIT.
06606
06607      IF DL35-RETURN-CODE = '01'
06608          MOVE ER-7830            TO EMI-ERROR
06609          GO TO 6500-BAD-EXIT.
06610
06611      IF DL35-RETURN-CODE = '02'
06612          MOVE ER-7831            TO EMI-ERROR
06613          GO TO 6500-BAD-EXIT.
06614
06615      IF DL35-RETURN-CODE = '03'
06616          MOVE ER-7832            TO EMI-ERROR
06617          GO TO 6500-BAD-EXIT.
06618
06619      IF DL35-RETURN-CODE = '04'
06620          MOVE ER-7833            TO EMI-ERROR
06621          GO TO 6500-BAD-EXIT.
06622
06623      IF DL35-RETURN-CODE = '05'
06624          MOVE ER-7834            TO EMI-ERROR
06625          GO TO 6500-BAD-EXIT.
06626
06627      IF DL35-RETURN-CODE = '06'
06628          MOVE ER-7835            TO EMI-ERROR
06629          GO TO 6500-BAD-EXIT.
06630
06631      IF DL35-RETURN-CODE = '07'
06632          MOVE ER-7836            TO EMI-ERROR
06633          GO TO 6500-BAD-EXIT.
06634
06635      IF DL35-RETURN-CODE = 'E1'
06636          MOVE ER-7837            TO EMI-ERROR
06637          GO TO 6500-BAD-EXIT.
06638
06639      IF DL35-RETURN-CODE NOT = 'OK'
06640          MOVE ER-7838            TO EMI-ERROR
06641          GO TO 6500-BAD-EXIT.
06642
06643  6500-EXIT.
06644       EXIT.
06645
06646  6500-BAD-EXIT.
06647      
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC.
      *    MOVE '6"R                   !   #00016161' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303136313631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06648      MOVE -1                 TO PMTTYPEL.
06649      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
06650      GO TO 8200-SEND-DATAONLY.
06651
06652  EJECT
06653  6600-UPDATE-DLO035.
06654      MOVE 'U'                    TO DL35-PROCESS-TYPE.
06655      MOVE CL-CCN-A5              TO DL35-CREDIT-CARD.
06656      MOVE CL-CERT-NO (5:2)       TO DL35-BEN-TYPE.
06657      MOVE PI-EPYAMT              TO DL35-PAYMENT-AMT.
06658
06659      
      * EXEC CICS LINK
06660 *        PROGRAM    ('DLO035')
06661 *        COMMAREA   (DLO035-KEY)
06662 *        LENGTH     (DL35-LENGTH)
06663 *    END-EXEC.
           MOVE 'DLO035' TO DFHEIV1
      *    MOVE '."C                   (   #00016173' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136313733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO035-KEY, 
                 DL35-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06664
06665      IF DL35-RETURN-CODE = 'OK'
06666          GO TO 6600-EXIT.
06667
06668      IF DL35-RETURN-CODE = '01'
06669          MOVE ER-7830            TO EMI-ERROR
06670          GO TO 6500-BAD-EXIT.
06671
06672      IF DL35-RETURN-CODE = '02'
06673          MOVE ER-7831            TO EMI-ERROR
06674          GO TO 6500-BAD-EXIT.
06675
06676      IF DL35-RETURN-CODE = '03'
06677          MOVE ER-7832            TO EMI-ERROR
06678          GO TO 6500-BAD-EXIT.
06679
06680      IF DL35-RETURN-CODE = '04'
06681          MOVE ER-7833            TO EMI-ERROR
06682          GO TO 6500-BAD-EXIT.
06683
06684      IF DL35-RETURN-CODE = '05'
06685          MOVE ER-7834            TO EMI-ERROR
06686          GO TO 6500-BAD-EXIT.
06687
06688      IF DL35-RETURN-CODE = '06'
06689          MOVE ER-7835            TO EMI-ERROR
06690          GO TO 6500-BAD-EXIT.
06691
06692      IF DL35-RETURN-CODE = '07'
06693          MOVE ER-7836            TO EMI-ERROR
06694          GO TO 6500-BAD-EXIT.
06695
06696      IF DL35-RETURN-CODE = 'E1'
06697          MOVE ER-7837            TO EMI-ERROR
06698          GO TO 6500-BAD-EXIT.
06699
06700      IF DL35-RETURN-CODE NOT = 'OK'
06701          MOVE ER-7838            TO EMI-ERROR
06702          GO TO 6500-BAD-EXIT.
06703
06704  6600-EXIT.
06705       EXIT.
06706
06707      EJECT
06708  6700-UPDATE-ACTQ.
06709 ***************************************************************
06710 *    THE ACTIVITY QUE FILE IS READ TO GET THE RECORD FOR      *
06711 *    THIS CLAIM.  A NEW RECORD WILL BE BUILT IF THERE ISNT    *
06712 *    ONE ALREADY THERE.                                       *
06713 ***************************************************************
06714
06715      
      * EXEC CICS HANDLE CONDITION
06716 *         NOTFND(6720-BUILD-NEW-RECORD)
06717 *     END-EXEC.
      *    MOVE '"$I                   ! 6 #00016229' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303136323239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06718
06719      
      * EXEC CICS READ
06720 *         DATASET('ELACTQ')
06721 *         RIDFLD (ELMSTR-KEY)
06722 *         SET    (ADDRESS OF ACTIVITY-QUE)
06723 *         UPDATE
06724 *     END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00016233' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136323333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06725
06726      MOVE '1'                    TO AQ-PENDING-PAYMENT-FLAG.
06727
06728      IF PI-AIG-SPECIAL-BENEFIT AND
06729         PI-CASH = 'Y'
06730          ADD 2                   TO AQ-PAYMENT-COUNTER
06731      ELSE
06732          ADD 1                   TO AQ-PAYMENT-COUNTER.
06733
06734      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC
06735          MOVE +0                 TO  AQ-PMT-UNAPPROVED-COUNT.
06736
06737      MOVE +0                     TO  WS-PMT-UNAPPROVED-CNT.
06738
06739      IF PI-PMT-GRADUATED
06740          IF WS-PMT-APPROVAL-SW = 'A'
06741              NEXT SENTENCE
06742          ELSE
06743             IF PI-AIG-SPECIAL-BENEFIT AND
06744                PI-CASH = 'Y'
06745                 ADD +2           TO  AQ-PMT-UNAPPROVED-COUNT
06746                                      WS-PMT-UNAPPROVED-CNT
06747             ELSE
06748                 ADD +1           TO  AQ-PMT-UNAPPROVED-COUNT
06749                                      WS-PMT-UNAPPROVED-CNT
06750      ELSE
06751          IF PI-PMT-APPR-SW = 'Y'
06752             IF PI-AIG-SPECIAL-BENEFIT AND
06753                PI-CASH = 'Y'
06754                 ADD +2           TO  AQ-PMT-UNAPPROVED-COUNT
06755                                      WS-PMT-UNAPPROVED-CNT
06756             ELSE
06757                 ADD +1           TO  AQ-PMT-UNAPPROVED-COUNT
06758                                      WS-PMT-UNAPPROVED-CNT.
06759
06760      IF WS-PMT-UNAPPROVED-CNT > ZERO
06761          MOVE ER-3536 TO EMI-ERROR
06762          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
06763
06764      MOVE +156                   TO  AQ-LAST-UPDATED-BY.
06765
06766      
      * EXEC CICS REWRITE
06767 *         DATASET('ELACTQ')
06768 *         FROM   (ACTIVITY-QUE)
06769 *     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00016280' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303136323830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06770
06771      GO TO 6799-EXIT.
06772
06773  6720-BUILD-NEW-RECORD.
06774      
      * EXEC CICS GETMAIN
06775 *         SET    (ADDRESS OF ACTIVITY-QUE)
06776 *         LENGTH (60)
06777 *         INITIMG(GETMAIN-SPACE)
06778 *     END-EXEC.
           MOVE 60
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00016288' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303136323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06779
06780      MOVE ZEROS                  TO AQ-PMT-UNAPPROVED-COUNT.
06781
06782      IF PI-AIG-SPECIAL-BENEFIT AND
06783         PI-CASH = 'Y'
06784          MOVE 2                  TO AQ-PAYMENT-COUNTER
06785      ELSE
06786          MOVE 1                  TO AQ-PAYMENT-COUNTER.
06787
06788      MOVE ELMSTR-KEY             TO AQ-CONTROL-PRIMARY.
06789      MOVE 'AQ'                   TO AQ-RECORD-ID.
06790      MOVE SPACES                 TO AQ-PENDING-ACTIVITY-FLAGS.
06791      MOVE LOW-VALUES             TO AQ-RESEND-DATE
06792                                     AQ-FOLLOWUP-DATE.
06793      MOVE +156                   TO AQ-LAST-UPDATED-BY.
06794      MOVE '1'                    TO AQ-PENDING-PAYMENT-FLAG.
06795
06796      IF PI-PMT-GRADUATED
06797          IF WS-PMT-APPROVAL-SW = 'A'
06798              NEXT SENTENCE
06799          ELSE
06800             IF PI-AIG-SPECIAL-BENEFIT AND
06801                PI-CASH = 'Y'
06802                 ADD +2           TO  AQ-PMT-UNAPPROVED-COUNT
06803             ELSE
06804                 ADD +1           TO  AQ-PMT-UNAPPROVED-COUNT
06805      ELSE
06806          IF PI-PMT-APPR-SW = 'Y'
06807             IF PI-AIG-SPECIAL-BENEFIT AND
06808                PI-CASH = 'Y'
06809                 ADD +2           TO  AQ-PMT-UNAPPROVED-COUNT
06810             ELSE
06811                 ADD +1           TO  AQ-PMT-UNAPPROVED-COUNT.
06812
06813      IF AQ-PMT-UNAPPROVED-COUNT > ZERO
06814          MOVE ER-3536 TO EMI-ERROR
06815          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
06816
06817      
      * EXEC CICS WRITE
06818 *         DATASET('ELACTQ')
06819 *         RIDFLD (ELMSTR-KEY)
06820 *         FROM   (ACTIVITY-QUE)
06821 *     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00016331' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303136333331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06822
06823  6799-EXIT.
06824       EXIT.
06825
06826      EJECT
06827  6800-BUILD-FORM-TRAILER.
06828
06829      
      * EXEC CICS GETMAIN
06830 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
06831 *         LENGTH   (200)
06832 *         INITIMG  (GETMAIN-SPACE)
06833 *    END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00016343' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303136333433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06834
06835      SUBTRACT +1               FROM CL-TRAILER-SEQ-CNT.
06836
06837      MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY.
06838      MOVE 'AT'                   TO AT-RECORD-ID.
06839      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.
06840
06841      MOVE 'A'                    TO AT-TRAILER-TYPE.
06842      MOVE WS-TODAY-DATE          TO AT-RECORDED-DT
06843                                     AT-FORM-LAST-MAINT-DT
06844                                     AT-FORM-SEND-ON-DT.
06845      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
06846                                     AT-FORM-LAST-UPDATED-BY.
06847      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
06848      MOVE WS-TODAY-DATE          TO DC-BIN-DATE-1.
06849      MOVE '6'                    TO DC-OPTION-CODE.
06850      MOVE +29                    TO DC-ELAPSED-DAYS.
06851      MOVE +0                     TO DC-ELAPSED-MONTHS.
06852      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
06853      MOVE DC-BIN-DATE-2          TO AT-FORM-FOLLOW-UP-DT.
06854
06855      IF PI-COMPANY-ID = 'RMC' OR 'LAP'
06856          MOVE  LOW-VALUES        TO AT-FORM-FOLLOW-UP-DT.
06857
06858      MOVE LOW-VALUES             TO AT-FORM-ANSWERED-DT
06859                                     AT-FORM-RE-SEND-DT
06860                                     AT-EMP-FORM-ANSWERED-DT
06861                                     AT-PHY-FORM-ANSWERED-DT
06862                                     AT-EMP-FORM-SEND-ON-DT
06863                                     AT-PHY-FORM-SEND-ON-DT
06864                                     AT-FORM-PRINTED-DT
06865                                     AT-FORM-REPRINT-DT.
06866      MOVE '2'                    TO AT-FORM-TYPE.
06867
06868      MOVE CL-INSURED-ADDR-CNT    TO AT-FORM-ADDR-SEQ-NO.
06869      MOVE 'I'                    TO AT-FORM-ADDRESS.
06870
06871      IF PI-COMPANY-ID = 'RMC' OR 'LAP'
06872          NEXT SENTENCE
06873      ELSE
06874          IF CL-PROG-FORM-TYPE NOT = 'S'
06875             MOVE WS-TODAY-DATE   TO AT-PHY-FORM-SEND-ON-DT.
06876
06877      
      * EXEC CICS WRITE
06878 *         DATASET  ('ELTRLR')
06879 *         FROM     (ACTIVITY-TRAILERS)
06880 *         RIDFLD   (AT-CONTROL-PRIMARY)
06881 *     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00016391' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303136333931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06882
06883  6899-EXIT.
06884       EXIT.
06885
06886      EJECT
06887  6900-BUILD-ARCHIVE-HEADER.
06888
06889      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
06890      MOVE '1'                    TO CNTL-REC-TYPE.
06891      MOVE +0                     TO CNTL-SEQ-NO.
06892      MOVE SPACES                 TO CNTL-ACCESS.
06893
06894      
      * EXEC CICS READ
06895 *         DATASET  ('ELCNTL')
06896 *         SET      (ADDRESS OF CONTROL-FILE)
06897 *         RIDFLD   (ELCNTL-KEY)
06898 *         UPDATE
06899 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00016408' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136343038' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06900
06901      ADD +1                      TO CF-CO-ARCHIVE-COUNTER.
06902      MOVE CF-CO-ARCHIVE-COUNTER  TO ARCH-NUMBER.
06903
06904      
      * EXEC CICS REWRITE
06905 *         FROM     (CONTROL-FILE)
06906 *         DATASET  ('ELCNTL')
06907 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %   #00016418' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303136343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06908
06909      
      * EXEC CICS HANDLE CONDITION
06910 *         DUPKEY   (6900-BUILD-ARCHIVE-HEADER)
06911 *         DUPREC   (6900-BUILD-ARCHIVE-HEADER)
06912 *    END-EXEC.
      *    MOVE '"$$%                  ! 7 #00016423' TO DFHEIV0
           MOVE X'222424252020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303136343233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06913
06914      
      * EXEC CICS GETMAIN
06915 *         SET     (ADDRESS OF LETTER-ARCHIVE)
06916 *         LENGTH  (ELARCH-LENGTH)
06917 *         INITIMG (GETMAIN-SPACE)
06918 *    END-EXEC.
      *    MOVE ',"IL                  $   #00016428' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303136343238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELARCH-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06919
06920      MOVE 'LA'                   TO LA-RECORD-ID.
06921      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO
06922                                     LA-ARCHIVE-NO-A1.
06923      MOVE '4'                    TO LA-RECORD-TYPE
06924                                     LA-RECORD-TYPE-A1.
06925      MOVE +0                     TO LA-LINE-SEQ-NO
06926                                     LA-LINE-SEQ-NO-A1.
06927      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
06928                                     LA-COMPANY-CD-A1.
06929      MOVE CL-CARRIER             TO LA4-CARRIER.
06930      MOVE CL-CLAIM-NO            TO LA4-CLAIM-NO.
06931      MOVE CL-CERT-NO             TO LA4-CERT-NO.
06932      MOVE CL-CERT-STATE          TO LA4-STATE.
06933      MOVE +0                     TO LA4-NO-OF-COPIES.
06934      MOVE PI-PROCESSOR-ID        TO LA4-PROCESSOR-CD.
06935      MOVE WS-TODAY-DATE          TO LA4-CREATION-DT.
06936      MOVE LOW-VALUES             TO LA4-RESEND-DATE
06937                                     LA4-INITIAL-PRINT-DATE
06938                                     LA4-RESEND-PRINT-DATE
06939                                     LA4-FORM-REM-PRINT-DT.
06940      MOVE CL-TRAILER-SEQ-CNT     TO LA4-FORM-TRLR-SEQ.
06941      MOVE '2'                    TO LA4-FORM-TYPE.
06942
06943      
      * EXEC CICS WRITE
06944 *         DATASET  ('ELARCH')
06945 *         FROM     (LETTER-ARCHIVE)
06946 *         RIDFLD   (LA-CONTROL-PRIMARY)
06947 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
           MOVE 'ELARCH' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00016457' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303136343537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06948
06949  6999-EXIT.
06950       EXIT.
06951
06952      EJECT
06953  7000-SHOW-CLAIM.
06954      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.
06955      MOVE 'A'                    TO PI-PASS-SW.
06956      MOVE LOW-VALUES             TO PI-CPYFROM
06957                                     PI-MONTH-END-SAVE
06958                                     PI-CPYTHRU.
06959
06960      MOVE ZEROS                  TO PI-FATAL-COUNT
06961                                     PI-DAILY-RATE
06962                                     PI-BEN-DAYS
06963                                     PI-MANUAL-SW
06964                                     PI-SAVE-CURSOR
06965                                     PI-FORCE-COUNT
06966                                     PI-HOLDTIL
06967                                     PI-EPYFROM
06968                                     PI-EPYTHRU
06969                                     PI-EDAYS
06970                                     PI-EPYAMT
06971                                     PI-ERESV
06972                                     PI-EEXPENS
06973                                     PI-CDAYS
06974                                     PI-CPYAMT
06975                                     PI-CRESV
06976                                     PI-CEXPENS
06977                                     PI-INT-RATE
06978                                     PI-AIGFROM.
052506     MOVE ZEROS                  TO PI-PROOF-DATE.
041710     MOVE ZEROS                  TO PI-ORIG-BEN-AMT
041710                                    PI-REM-BEN-AMT
041710                                    PI-MO-BEN-AMT
                                          pi-ah-term.
06979
06980      MOVE LOW-VALUES             TO EL156AI.
06981      MOVE MAP-NAMEA              TO MAP-NAME.
020413     
      * EXEC CICS HANDLE CONDITION
020413*        NOTFND   (7010-BUILD-MAP)
020413*    END-EXEC.
      *    MOVE '"$I                   ! 8 #00016501' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303136353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
020413
020413     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
020413     MOVE '2'                    TO CNTL-REC-TYPE.
020413     MOVE +0                     TO CNTL-SEQ-NO.
020413     MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS.
020413
020413     
      * EXEC CICS READ
020413*         DATASET  ('ELCNTL')
020413*         SET      (ADDRESS OF CONTROL-FILE)
020413*         RIDFLD   (ELCNTL-KEY)
020413*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00016510' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136353130' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
020413
020413     MOVE CF-APPROVAL-LEVEL TO PI-APPROVAL-LEVEL.
010413
111113     IF PI-APPROVAL-LEVEL = '4' OR '5'
020413         MOVE AL-UANON      TO SURVYYNA
020413     END-IF.
020413
020413
032813     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
032813     MOVE '1'                    TO CNTL-REC-TYPE.
032813     MOVE SPACES                 TO CNTL-ACCESS.
032813     MOVE +0                     TO CNTL-SEQ-NO.
032813
032813     
      * EXEC CICS READ
032813*         DATASET  ('ELCNTL')
032813*         SET      (ADDRESS OF CONTROL-FILE)
032813*         RIDFLD   (ELCNTL-KEY)
032813*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00016528' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136353238' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
032813
032813     MOVE CF-CO-REAUDIT-INTERVAL TO PI-REAUDIT-INTERVAL.
032813
06983  7010-BUILD-MAP.
06984
06985      
      * EXEC CICS HANDLE CONDITION
06986 *        NOTFND(7100-SHOW-RECORD-NOT-FOUND)
06987 *    END-EXEC.
      *    MOVE '"$I                   ! 9 #00016538' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303136353338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06988
06989      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
06990      MOVE PI-CARRIER             TO MSTR-CARRIER.
06991      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
06992      MOVE PI-CERT-NO             TO MSTR-CERT-NO.
06993      MOVE 'MSTR'                 TO FILE-SWITCH.
06994
06995      PERFORM 7900-READ-CLAIM THRU 7900-EXIT.
06996
06997      MOVE CL-CERT-KEY-DATA       TO PI-SV-CERT-KEY.
06998      MOVE CL-CERT-NO             TO PI-SV-CERT-NO.
06999      MOVE CL-BENEFICIARY         TO PI-SV-BEN.
07000      MOVE CL-CCN                 TO PI-SV-CCN.
07001
07002      MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.
07003      MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
07004      MOVE CL-CLAIM-NO            TO CLMNOO.
07005      MOVE CL-CARRIER             TO CARRO.
07006      MOVE CL-CERT-PRIME          TO CERTNOO.
07007      MOVE CL-CERT-SFX            TO SUFXO.
07008
062121     IF (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
100518        AND (CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O')
              CONTINUE
           ELSE
              MOVE AL-SADOF            TO ZINTHA
                                          ZINTA
                                          PINTHA
                                          PINTA
           END-IF
013013     MOVE CL-CLAIM-TYPE          TO PI-CLM-TYPE
121802     EVALUATE TRUE
121802     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
07010          MOVE PI-AH-OVERRIDE-L6   TO CLMTYPO
121802     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
07012          MOVE PI-LIFE-OVERRIDE-L6 TO CLMTYPO
07013
121802     WHEN CL-CLAIM-TYPE = 'I'
121802         MOVE '  IU  '           TO CLMTYPO
121802     WHEN CL-CLAIM-TYPE = 'G'
121802         MOVE ' GAP  '           TO CLMTYPO
121802     WHEN CL-CLAIM-TYPE = 'F'
121802         MOVE ' FMLA '           TO CLMTYPO
022122     WHEN CL-CLAIM-TYPE = 'B'
022122        MOVE ' BRV  '            TO CLMTYPO
022122     WHEN CL-CLAIM-TYPE = 'H'
022122        MOVE ' HOSP '            TO CLMTYPO
100518     WHEN CL-CLAIM-TYPE = 'O'
100518         MOVE ' OTH '           TO CLMTYPO
121802     END-EVALUATE.
100518     IF CL-CLAIM-TYPE = 'O'
100518        MOVE 'S' TO EOBYNO
100518                    PI-PRINT-EOB-YN
100518                    WS-PRINT-EOB-YN
100518     END-IF
07014      IF CLAIM-IS-OPEN
07015          MOVE ' OPEN'            TO CLMSTATO
07016      ELSE
07017          MOVE 'CLOSED'           TO CLMSTATO.
07018
07019      PERFORM 5000-MOVE-NAME  THRU 5000-EXIT.
07020
07021      IF CL-ASSOC-CERT-TOTAL = +0 OR +1
07022          MOVE SPACES             TO  PCERTNOO
07023                                      PSUFXO
07024                                      PRIMHDGO
07025                                      SEQUO
07026          MOVE AL-SADOF           TO  PCERTNOA
07027                                      PSUFXA
07028                                      PRIMHDGA
07029                                      SEQUA
07030      ELSE
07031          MOVE CL-ASSOC-CERT-SEQU     TO  WS-CUR-SEQU
07032          MOVE CL-ASSOC-CERT-TOTAL    TO  WS-OF-SEQU
07033          MOVE WS-CLAIM-SEQUENCE      TO  SEQUO
07034          MOVE CL-PRIME-CERT-PRIME    TO  PCERTNOO
07035          MOVE CL-PRIME-CERT-SFX      TO  PSUFXO
07036          MOVE 'PRIME CERT :'         TO  PRIMHDGO
07037          MOVE AL-SANON               TO  PCERTNOA
07038                                          PSUFXA
07039                                          PRIMHDGA
07040          MOVE AL-SABON               TO  SEQUA.
07041
07042      MOVE CL-ACTIVITY-CODE           TO  PI-ACTIVITY-CODE.
07043
07044      IF CL-NO-OF-PMTS-MADE = 0 AND
07045         CL-TOTAL-PAID-AMT = 0
07046          NEXT SENTENCE
07047      ELSE
07048          IF CL-PAID-THRU-DT NOT = LOW-VALUES
07049              IF NOT PI-USES-PAID-TO
07050                  MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1
07051                  MOVE ' '                 TO DC-OPTION-CODE
07052                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07053                  MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO
07054              ELSE
07055                  MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1
07056                  MOVE '6'                 TO DC-OPTION-CODE
07057                  MOVE +1                  TO DC-ELAPSED-DAYS
07058                  MOVE +0                  TO DC-ELAPSED-MONTHS
07059                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07060                  MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO.
07061
07062      IF MAP-NAME = 'EL156A'
07063         IF CL-PAID-THRU-DT NOT = LOW-VALUES AND SPACES
07064            IF CL-TOTAL-PAID-AMT NOT = ZEROS
07065              MOVE AL-SADOF TO    AIGFRMHA AIGFROMA
                 end-if
              end-if
           end-if
07067      IF CL-LAST-PMT-DT NOT = LOW-VALUES
07068          MOVE CL-LAST-PMT-DT     TO DC-BIN-DATE-1
07069          MOVE ' '                TO DC-OPTION-CODE
07070          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07071          MOVE DC-GREG-DATE-1-EDIT TO LSTDTEO.
07072
07073      IF CL-INCURRED-DT NOT = LOW-VALUES
07074          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
07075          MOVE ' '                TO DC-OPTION-CODE
07076          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07077          MOVE DC-GREG-DATE-1-EDIT    TO INCURO.
07078
07079      MOVE CL-TOTAL-PAID-AMT      TO TOTPAIDO.
07080
07081      IF CL-CERT-EFF-DT NOT = LOW-VALUES
07082          MOVE CL-CERT-EFF-DT     TO DC-BIN-DATE-1
07083          MOVE ' '                TO DC-OPTION-CODE
07084          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07085          MOVE DC-GREG-DATE-1-EDIT    TO EFFECTO.
07086
07087      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
07088          MOVE CL-CURRENT-ACCOUNT TO WS-WORK-ACCT
07089          MOVE CL-CURRENT-STATE   TO WS-WORK-STATE
07090      ELSE
07091          MOVE CL-CERT-ACCOUNT    TO WS-WORK-ACCT
07092          MOVE CL-CERT-STATE      TO WS-WORK-STATE.
07093
07094      MOVE WS-WORK-STATE-ACCT     TO STACCTO.
07095      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
07096      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
07097      MOVE CL-CERT-GROUPING       TO CERT-GROUPING
07098                                     PI-GROUPING.
07099      MOVE CL-CERT-STATE          TO CERT-STATE
07100                                     PI-STATE.
07101      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
07102                                     PI-ACCOUNT.
07103      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
07104      MOVE CL-CERT-NO             TO CERT-CERT-NO.
07105      MOVE 'CERT'                 TO FILE-SWITCH.
07106
07107      PERFORM 7970-READ-CERT THRU 7970-EXIT.
052814     IF CM-INSURED-LAST-NAME EQUAL CL-INSURED-LAST-NAME  AND
052814        CM-INSURED-FIRST-NAME EQUAL CL-INSURED-1ST-NAME  AND
052814        CM-INSURED-INITIAL2 EQUAL CL-INSURED-MID-INIT
052814            MOVE 'N' TO PI-JOINT-INSURED-IND
052814     ELSE
052814            MOVE 'Y' TO PI-JOINT-INSURED-IND
052814     END-IF.
052814
           perform 7975-read-acct      thru 7975-exit
07109      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
07110      MOVE PI-STATE               TO WS-ST-ACCESS.
07111      MOVE WS-STATE-ACCESS        TO CNTL-ACCESS.
07112      MOVE '3'                    TO CNTL-REC-TYPE.
07113      MOVE +0                     TO CNTL-SEQ-NO.
07114      MOVE 'STAT'                 TO FILE-SWITCH.
07115
07116      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.
07117
07118      MOVE CF-STATE-ABBREVIATION  TO WS-STATE-ABBREV.
07119      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
07120
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
07122         GO TO 7020-BYPASS-BENEFIT.
07123
07124      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
07125      MOVE '4'                    TO CNTL-REC-TYPE.
07126      MOVE +0                     TO CNTL-SEQ-NO.
07127      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-CD.
07128      MOVE WS-ACCESS              TO CNTL-ACCESS.
07129      MOVE 'BENE'                 TO FILE-SWITCH.
07130
07131      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
07132
07133      IF NO-BENEFIT-FOUND
07134         GO TO 7020-BYPASS-BENEFIT.
07135
07136      MOVE CF-CO-EARNINGS-CALC (SUB-1)    TO  WS-EARNING-METHOD.
07137      MOVE CF-SPECIAL-CALC-CD  (SUB-1)    TO  WS-SPECIAL-CALC-CD.
07138      MOVE CF-LF-COVERAGE-TYPE (SUB-1)    TO  WS-LF-COVERAGE-TYPE.
07139
07140  7020-BYPASS-BENEFIT.
07141
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*        PERFORM 3400-COMPUTE-EXPIRY THRU 3499-EXIT
121802*        MOVE PI-LOAN-DUE-DAY TO DUEDAYO.
07145
061013     move zeros                  to PI-MAX-BENEFIT-AMT
061013     move cm-ah-benefit-cd       to pi-ah-benefit-cd
           move cm-ah-orig-term        to pi-ah-term
           move cm-ah-benefit-amt      to pi-ah-benefit-amt
121802     IF CL-CLAIM-TYPE NOT = PI-AH-OVERRIDE-L1 AND 'I' AND
100314          'G' and 'F'
022122          AND 'B' AND 'H'
07147          GO TO 7021-GO-TO-LIFE.
022122     move cl-insured-birth-dt    to dc-bin-date-1
022122     move cl-incurred-dt         to dc-bin-date-2
022122     move '1'                    to dc-option-code
022122     PERFORM 9700-LINK-DATE-CONVERT
022122                                 THRU 9700-EXIT
022122     compute ws-att-age =
022122        dc-elapsed-months / 12
022122     move zeros to dc-elapsed-months dc-elapsed-days
061013
120115     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
061013        and (acct-found)
061013        AND (PI-DCC-PRODUCT-CODE not = '   ')
061013        PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
061013        IF PDEF-FOUND
061013           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122              (P1 > +11)
022122              OR ((PD-PROD-CODE (P1) = cl-claim-type)
022122              AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
061013           END-PERFORM
022122           IF P1 < +12
100314              if pd-ben-pct (p1) not numeric
100314                 move zeros      to pd-ben-pct (p1)
100314              end-if
100314              if pd-ben-pct (p1) = zeros
100314                 move +1         to ws-work-ben-pct
100314              else
100314                 move pd-ben-pct (p1)
100314                                 to ws-work-ben-pct
100314              end-if
                    compute cm-ah-benefit-amt =
                       cm-ah-benefit-amt * ws-work-ben-pct
061013              move pd-max-amt (p1) to pi-max-benefit-amt
022122              if pd-wait-days(p1) not numeric
022122                 move zeros      to pd-wait-days(p1)
022122              end-if
061013           END-IF
061013        END-IF
061013     end-if
07149      MOVE 'A'                    TO CP-BENEFIT-TYPE.
07150      MOVE 'MO. BENEFIT :'        TO BENECAPO.
061013     if (pi-max-benefit-amt not = zeros)
061013        and (cm-ah-benefit-amt > pi-max-benefit-amt)
061013        move pi-max-benefit-amt  to beneo
061013     else
061013        MOVE CM-AH-BENEFIT-AMT   TO BENEO
061013     end-if
07152      MOVE CM-POLICY-FORM-NO      TO PI-SAVE-FORM.
07153      MOVE CM-AH-ORIG-TERM        TO WST-ORIG
07154                                     CP-ORIGINAL-TERM.
07155      MOVE CM-AH-CURRENT-STATUS   TO WS-STATUS.
07156
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*        MOVE WS-EXP-DT            TO DC-BIN-DATE-1
121802*    ELSE
07160          MOVE CM-AH-LOAN-EXPIRE-DT TO DC-BIN-DATE-1.
07161
07162      MOVE ' '                    TO DC-OPTION-CODE.
07163      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
07164      MOVE DC-GREG-DATE-1-EDIT    TO EXPDTEO.
07165
07166      IF PI-COMPANY-ID = 'DMD'
07167          PERFORM 9830-DMD-REMAINING-TERM THRU 9830-EXIT
07168          GO TO 7023-BYPASS-REMAINING-TERM.
07169
07170      MOVE SPACES                 TO WST-REM-DAYS-GRP.
07171      MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT.
07172      GO TO 7022-AROUND-LIFE.
07173
07174  7021-GO-TO-LIFE.
07175
07176      MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE.
07177      MOVE CM-POLICY-FORM-NO      TO PI-SAVE-FORM.
07178      MOVE CM-LF-ORIG-TERM        TO WST-ORIG
07179                                     CP-ORIGINAL-TERM.
07180      MOVE WS-EARNING-METHOD      TO CP-EARNING-METHOD.
07181      MOVE WS-SPECIAL-CALC-CD     TO CP-SPECIAL-CALC-CD.
07182      MOVE CL-INCURRED-DT         TO CP-VALUATION-DT.
07183      MOVE CM-LF-CURRENT-STATUS   TO WS-STATUS.
07184
07185      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
07186          MOVE WS-EXP-DT            TO DC-BIN-DATE-1
07187      ELSE
07188          MOVE CM-LF-LOAN-EXPIRE-DT TO DC-BIN-DATE-1.
07189
07190      MOVE ' '                    TO DC-OPTION-CODE.
07191      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
07192      MOVE DC-GREG-DATE-1-EDIT    TO EXPDTEO.
07193
07194  7022-AROUND-LIFE.
07195
07196      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
07197      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
07198      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
07199      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
07200      MOVE '4'                    TO CP-REM-TERM-METHOD.
07201      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.
07202      MOVE CP-REMAINING-TERM-3    TO WST-REM.
07203
07204      MOVE SPACES                 TO WST-REM-DAYS-GRP.
07205
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122        OR 'B' OR 'H'
07207         IF CP-REMAINING-TERM-3 > CM-AH-ORIG-TERM
07208            MOVE CM-AH-ORIG-TERM  TO WST-REM.
07209
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
07211         IF CP-REMAINING-TERM-3 > CM-LF-ORIG-TERM
07212            MOVE CM-LF-ORIG-TERM  TO WST-REM.
07213
07214  7023-BYPASS-REMAINING-TERM.
022122     move zeros                  to ws-max-tot-ben
022122                                    ws-work-ben-pct
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
07217        IF CM-LF-CURRENT-STATUS = '6' OR '7' OR '8'
07218           MOVE ZEROS                  TO BENEO
07219        ELSE
07220           MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
07221           MOVE PI-COMPANY-ID          TO CP-COMPANY-ID
07222           MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM
07223           MOVE CM-LF-BENEFIT-AMT      TO CP-ORIGINAL-BENEFIT
07224           MOVE CM-LF-ALT-BENEFIT-AMT  TO CP-ALTERNATE-BENEFIT
07225           MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
07226           MOVE CM-LOAN-APR            TO CP-LOAN-APR
07227           MOVE CM-LOAN-TERM           TO CP-LOAN-TERM
07228           MOVE CM-PAY-FREQUENCY       TO CP-PAY-FREQUENCY
07229           MOVE WS-EARNING-METHOD      TO CP-EARNING-METHOD
07230           MOVE WS-SPECIAL-CALC-CD     TO CP-SPECIAL-CALC-CD
07231           MOVE WS-STATE-ABBREV        TO CP-STATE-STD-ABBRV
022122          move cl-insured-birth-dt
022122                                 to dc-bin-date-1
022122          move cl-incurred-dt    to dc-bin-date-2
022122          move '1'               to dc-option-code
022122          PERFORM 9700-LINK-DATE-CONVERT
022122                                 THRU 9700-EXIT
022122          compute ws-att-age =
022122              dc-elapsed-months / 12
022122           move zeros to dc-elapsed-months dc-elapsed-days
120115          IF (PI-COMPANY-ID = 'DCC' or 'VPP')
061013             AND (PI-DCC-PRODUCT-CODE not = '   ')
061013             PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
061013             IF PDEF-FOUND
061013                PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122                   (P1 > +11)
022122                   OR (PD-PROD-CODE (P1) = cl-claim-type
022122                        and PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
061013                END-PERFORM
022122                IF P1 < +12
022122                   MOVE PD-MAX-AMT (P1)
022122                                 TO ws-MAX-TOT-BEN
022122                   if pd-ben-pct (p1) not numeric
022122                      move zeros to pd-ben-pct (p1)
022122                   end-if
022122                   if pd-ben-pct (p1) = zeros
022122                      move +1    to ws-work-ben-pct
022122                   else
022122                      move pd-ben-pct (p1)
022122                                 to ws-work-ben-pct
022122                   end-if
061013                END-IF
061013             END-IF
061013          END-IF
07232           
      * EXEC CICS LINK
07233 *            PROGRAM  (LINK-REMAMT)
07234 *            COMMAREA (CALCULATION-PASS-AREA)
07235 *            LENGTH   (CP-COMM-LENGTH)
07236 *         END-EXEC
      *    MOVE '."C                   (   #00016909' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-REMAMT, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07237           MOVE CP-REMAINING-AMT       TO BENEO
             end-if
           end-if
061013     MOVE ELMSTR-KEY             TO ELTRLR-KEY
061013     MOVE +95                    TO TRLR-SEQ-NO
061013     
      * EXEC CICS READ
061013*       DATASET  ('ELTRLR')
061013*       SET      (ADDRESS OF ACTIVITY-TRAILERS)
061013*       RIDFLD   (ELTRLR-KEY)
061013*       RESP     (WS-RESPONSE)
061013*    END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00016919' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303136393139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     if ws-resp-normal
061013        perform varying s1 from +1 by +1 until
061013           at-note-error-no (s1) = spaces
061013           move at-note-error-no (s1)
061013                                 to emi-error
061013           if at-note-error-no (s1) = '1653'
061013              evaluate true
061013                 when cl-claim-type = 'L'
061013                    move '  LF  '
061013                                 to emi-claim-type
061013                 when cl-claim-type = 'I'
061013                    move '  IU  '
061013                                 to emi-claim-type
061013                 when cl-claim-type = 'F'
061013                    move ' FMLA '
061013                                 to emi-claim-type
022122                 when cl-claim-type = 'B'
022122                    move ' BRV  '
022122                                 to emi-claim-type
022122                 when cl-claim-type = 'H'
022122                    move ' HOSP '
022122                                 to emi-claim-type
100518                 when cl-claim-type = 'O'
100518                    move ' OTH '
100518                                 to emi-claim-type
061013                 when other
061013                    move '  AH  '
061013                                 to emi-claim-type
061013              end-evaluate
061013           end-if
061013           PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013        end-perform
061013     end-if
100518    IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
07240          IF PI-LIFE-OVERRIDE-L1 = 'P' OR
07241             WS-LF-COVERAGE-TYPE = 'P'
07242              COMPUTE WS-REMAINING-AMT = CM-LF-BENEFIT-AMT -
07243                                         CM-LF-ITD-DEATH-AMT
07244              MOVE WS-REMAINING-AMT  TO  BENEO.
07245
041710     MOVE CM-LF-BENEFIT-AMT      TO PI-ORIG-BEN-AMT.
041710     MOVE BENEO                  TO PI-REM-BEN-AMT.
07246      MOVE WS-TERMS               TO TERMSO.
07247
07248      IF WS-STATUS = '6' OR '7' OR '8'
07249          MOVE AL-SABOF           TO CRTSTATA.
07250
07251      IF WS-STATUS = '1' OR '4'
07252         IF CP-REMAINING-TERM-3 = ZEROS
07253            MOVE 'EXPIRED'        TO CRTSTATO
07254         ELSE
07255            MOVE 'ACTIVE  '       TO CRTSTATO.
07256
07257      IF WS-STATUS = '2'
07258          MOVE 'PEND    '         TO CRTSTATO.
07259
07260      IF WS-STATUS = '3'
07261          MOVE 'RESTORE '         TO CRTSTATO.
07262
07263      IF WS-STATUS = '5'
07264          MOVE 'REISSUE '         TO CRTSTATO.
07265
07266      IF WS-STATUS = '6'
07267          MOVE 'LMP DIS'          TO CRTSTATO.
07268
07269      IF WS-STATUS = '7'
07270          MOVE 'DEATH'            TO CRTSTATO.
07271
07272      IF WS-STATUS = '8'
07273          MOVE 'CANCEL'           TO CRTSTATO.
07274
07275      IF WS-STATUS = '9'
07276          MOVE 'RE-ONLY '         TO CRTSTATO.
07277
07278      IF WS-STATUS = 'V'
07279          MOVE 'VOID  '           TO CRTSTATO.
07280
07281      IF WS-STATUS = 'D'
07282          MOVE 'DECLINE '         TO CRTSTATO.
07283
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122         OR 'B' OR 'H'
07285          MOVE CM-AH-BENEFIT-CD   TO WS-BEN-CD
07286          MOVE '5'                TO CNTL-REC-TYPE
07287      ELSE
07288          MOVE CM-LF-BENEFIT-CD   TO WS-BEN-CD
07289          MOVE '4'                TO CNTL-REC-TYPE.
07290
07291      MOVE '** NONE **'           TO COVERO.
07292      MOVE AL-SABON               TO COVERA.
07293
07294      IF PI-COMPANY-ID = 'CVL'
07295          IF CM-BENEFICIARY NOT = SPACES AND LOW-VALUES
07296              MOVE CM-BENEFICIARY TO LOANNOO
07297              MOVE AL-UANON       TO LOANNOA.
07298
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
CIDMOD        MOVE ELMSTR-KEY          TO  ELTRLR-KEY
CIDMOD        MOVE +91                 TO  TRLR-SEQ-NO
CIDMOD        
      * EXEC CICS READ
CIDMOD*           DATASET  ('ELTRLR')
CIDMOD*           SET      (ADDRESS OF ACTIVITY-TRAILERS)
CIDMOD*           RIDFLD   (ELTRLR-KEY)
CIDMOD*           RESP     (WS-RESPONSE)
CIDMOD*       END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00017027' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303137303237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD
CIDMOD        IF WS-RESP-NORMAL
CIDMOD           MOVE AT-INFO-LINE-1   TO  LOANNOO
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO  LOANNOO
CIDMOD        END-IF
022208*052506
022208*052506        PERFORM 7250-FIND-LETTER-TRLR THRU 7250-EXIT
022208*052506
CIDMOD     END-IF
CIDMOD
07299      IF CL-PROG-FORM-TYPE = 'S' OR 'L'
07300         MOVE CL-PROG-FORM-TYPE   TO FORMTYPO.
07301
07302      IF WS-BEN-CD = ZEROS
07303          GO TO 7050-SHOW-CONTINUE.
07304
07305      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
07306      MOVE WS-ACCESS              TO CNTL-ACCESS.
07307      MOVE +0                     TO CNTL-SEQ-NO.
07308      MOVE 'BENE'                 TO FILE-SWITCH.
07309
07310      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
07312      IF BENEFIT-FOUND
052814        MOVE CF-JOINT-INDICATOR (SUB-1)
                                       TO PI-JOINT-COV-IND
                                          PI-RB-JOINT-COV-IND
07313         MOVE CF-BENEFIT-DESCRIP (SUB-1)
                                       TO COVERO
07314         MOVE AL-SANON            TO COVERA
           END-IF
120115     if (cntl-rec-type = '4')   *> Life claim
120115        and (cm-ah-benefit-cd not = zeros) *> AH coverage
120115        MOVE PI-COMPANY-ID    TO CNTL-COMP-ID
120115        MOVE CM-AH-BENEFIT-CD TO WS-BEN-CD
120115        MOVE '5'              TO CNTL-REC-TYPE
120115        MOVE WS-ACCESS        TO CNTL-ACCESS
120115        MOVE +0               TO CNTL-SEQ-NO
120115        MOVE 'BENE'           TO FILE-SWITCH
120115        PERFORM 7200-FIND-BENEFIT
120115                              THRU 7200-EXIT
120115        IF (BENEFIT-FOUND)
120115           and (pi-rb-joint-cov-ind) not = 'J'
120115           MOVE CF-JOINT-INDICATOR (SUB-1)
120115                              TO PI-rb-JOINT-COV-IND
120115        end-if
120115     end-if
           .
07317  7050-SHOW-CONTINUE.
052814
052814     IF PI-JOINT-INSURED AND NOT PI-JOINT-COVERAGE
052814        AND CL-TOTAL-PAID-AMT NOT GREATER THAN ZERO
052814            MOVE 'Y' TO PI-APPROVAL-3550-NEEDED
052814     END-IF
07318
07319      IF PI-COMPANY-ID = 'DMD'
07320          PERFORM 8010-DMD-ERROR-CHECKS THRU 8090-EXIT.
07321
07322      MOVE -1                     TO PMTTYPEL.
07323      GO TO 8100-SEND-INITIAL-MAP.
07324
07325  7100-SHOW-RECORD-NOT-FOUND.
07326      IF FILE-SWITCH = 'MSTR'
07327          MOVE ER-0204            TO EMI-ERROR.
07328
07329      IF FILE-SWITCH = 'CERT'
07330          MOVE ER-0206            TO EMI-ERROR.
07331
07332      IF FILE-SWITCH = 'STAT'
07333          MOVE ER-0149            TO EMI-ERROR.
07334
07335      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
07336
07337      MOVE -1                     TO PMTTYPEL.
07338      GO TO 8100-SEND-INITIAL-MAP
07339
           .
022106 7110-BUILD-PMT-TRLR.
           MOVE ELMSTR-KEY             TO ELTRLR-KEY
           MOVE +51                    TO TRLR-SEQ-NO
           
      * EXEC CICS READ
      *        DATASET  ('ELTRLR')
      *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
      *        RIDFLD   (ELTRLR-KEY)
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00017113' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303137313133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF WS-RESP-NORMAL
              MOVE AT-MAIL-TO-NAME     TO PI-PAYEE-NAME
           END-IF
022106     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT
022106     
      * EXEC CICS GETMAIN
022106*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
022106*         LENGTH    (200)
022106*         INITIMG   (GETMAIN-SPACE)
022106*     END-EXEC
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00017123' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303137313233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
022106     MOVE LOW-VALUES             TO AT-PREV-LAST-PMT-DT
022106                                    AT-PREV-PAID-THRU-DT
022106     MOVE ZEROS                  TO AT-PREV-LAST-PMT-AMT
022106     MOVE ELMSTR-KEY             TO AT-CONTROL-PRIMARY
022106     MOVE 'AT'                   TO AT-RECORD-ID
022106     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
022106     MOVE +0                     TO AT-PAYMENT-NOTE-SEQ-NO
022106     MOVE '2'                    TO AT-TRAILER-TYPE
022106     MOVE WS-TODAY-DATE          TO AT-RECORDED-DT
022106                                    AT-PAYMENT-LAST-MAINT-DT
022106     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
022106                                    AT-PAYMENT-LAST-UPDATED-BY
022106     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
022106     MOVE 'I'                    TO AT-PAYMENT-TYPE
022106     MOVE CL-CLAIM-TYPE          TO AT-CLAIM-TYPE
022106     MOVE CL-CLAIM-PREM-TYPE     TO AT-CLAIM-PREM-TYPE
022106     MOVE +0                     TO AT-EXPENSE-PER-PMT
022106     MOVE PI-INT-AMT             TO AT-AMOUNT-PAID
022106     MOVE LOW-VALUES             TO AT-PAID-FROM-DT
022106     MOVE LOW-VALUES             TO AT-PAID-THRU-DT
012506     MOVE PI-INT-DAYS            TO AT-DAYS-IN-PERIOD
082807     MOVE PI-INT-RATE-USED       TO AT-INT-RATE
120115     if pi-int-to-rem-borr = 'Y'
120115        move 'Q1'                to at-payee-type-cd
120115     else
120115        MOVE 'O1'                TO AT-PAYEE-TYPE-CD
120115     end-if
120115     MOVE PI-int-payees-name     TO AT-PAYEES-NAME
022106     MOVE PI-MONTH-END-SAVE      TO AT-PMT-SELECT-DT
022106     MOVE '3'                    TO AT-PAYMENT-ORIGIN
022106     MOVE SPACES                 TO AT-AIG-UNEMP-IND
022106     MOVE LOW-VALUES             TO AT-CHECK-WRITTEN-DT
022106                                    AT-TO-BE-WRITTEN-DT
022106     MOVE ZEROS                  TO AT-CHECK-QUE-CONTROL
022106                                    AT-CHECK-QUE-SEQUENCE
022106                                    AT-ADDL-RESERVE
022106     MOVE LOW-VALUES             TO AT-PMT-ACCEPT-DT
022106                                    AT-VOID-SELECT-DT
022106                                    AT-VOID-ACCEPT-DT
022106                                    AT-VOID-DT
052506                                    AT-PMT-PROOF-DT
                                          AT-INT-PMT-SELECT-DT
022106     MOVE ZEROS                  TO AT-DAILY-RATE
022106                                    AT-ELIMINATION-DAYS
022106     MOVE SPACES                 TO AT-BENEFIT-TYPE
022106*    EXEC CICS HANDLE CONDITION
022106*        DUPREC    (7120-DUPREC)
022106*    END-EXEC
           .
022106 7115-WRITE-INT-TRLR.
022106     
      * EXEC CICS WRITE
022106*         DATASET       ('ELTRLR')
022106*         FROM          (ACTIVITY-TRAILERS)
022106*         RIDFLD        (AT-CONTROL-PRIMARY)
      *         RESP          (WS-RESPONSE)
022106*     END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00017178' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303137313738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
            IF WS-RESP-DUPREC
               GO TO 7120-DUPREC
            END-IF
022106     GO TO 7130-EXIT
           .
022106 7120-DUPREC.
022106     SUBTRACT +1                 FROM CL-TRAILER-SEQ-CNT
022106     MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO
022106     GO TO 7115-WRITE-INT-TRLR
           .
022106 7130-EXIT.
022106      EXIT.
022106 7140-BUILD-NOTE-TRLR.
022106
022106     
      * EXEC CICS GETMAIN
022106*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
022106*         LENGTH    (200)
022106*         INITIMG   (GETMAIN-SPACE)
022106*     END-EXEC
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00017198' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303137313938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
022106     SUBTRACT +1                 FROM CL-TRAILER-SEQ-CNT
022106     MOVE ELMSTR-KEY             TO AT-CONTROL-PRIMARY
022106     MOVE 'AT'                   TO AT-RECORD-ID
022106     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
022106     MOVE '6'                    TO AT-TRAILER-TYPE
022106     MOVE WS-TODAY-DATE          TO AT-RECORDED-DT
022106                                    AT-GEN-INFO-LAST-MAINT-DT
022106     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
022106                                    AT-GEN-INFO-LAST-UPDATED-BY
022106     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
022106     MOVE 'NO INTEREST DUE'      TO AT-INFO-LINE-1
022106     .
022106 7150-WRITE.
022106     
      * EXEC CICS HANDLE CONDITION
022106*        DUPREC    (7160-DUPREC)
022106*    END-EXEC
      *    MOVE '"$%                   ! : #00017216' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303137323136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
022106     
      * EXEC CICS WRITE
022106*         DATASET     ('ELTRLR')
022106*         FROM        (ACTIVITY-TRAILERS)
022106*         RIDFLD      (AT-CONTROL-PRIMARY)
      *         RESP        (WS-RESPONSE)
022106*     END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00017219' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303137323139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
            IF WS-RESP-DUPREC
               GO TO 7160-DUPREC
            END-IF
022106     GO TO 7170-EXIT
022106     .
022106 7160-DUPREC.
022106     SUBTRACT +1                 FROM CL-TRAILER-SEQ-CNT
022106     MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO
022106     GO TO 7150-WRITE
022106     .
022106 7170-EXIT.
022106      EXIT.
041710
041710 7180-ADD-CERT-NOTE.
041710*ADD CERT NOTE FOR SC NP+6
041710
041710     
      * EXEC CICS GETMAIN
041710*         SET       (ADDRESS OF CERT-NOTE-FILE)
041710*         LENGTH    (150)
041710*         INITIMG   (GETMAIN-SPACE)
041710*     END-EXEC.
           MOVE 150
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00017241' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303137323431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERT-NOTE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
041710
041710     MOVE SPACES                 TO WRK-CERT-NOTE-ADD.
041710     MOVE SPACES                 TO WRK-ORIG-NOTE-ADD.
041710     MOVE +1                     TO WRK-NOTE-SEQ.
041710     MOVE LOW-VALUES             TO CERT-NOTE-FILE.
041710     MOVE PI-COMPANY-CD          TO CZ-COMPANY-CD.
041710     MOVE CL-CERT-CARRIER        TO CZ-CARRIER.
041710     MOVE CL-CERT-GROUPING       TO CZ-GROUPING.
041710     MOVE CL-CERT-STATE          TO CZ-STATE.
041710     MOVE CL-CERT-ACCOUNT        TO CZ-ACCOUNT.
041710     MOVE CL-CERT-EFF-DT         TO CZ-CERT-EFF-DT.
041710     MOVE CL-CERT-NO             TO CZ-CERT-NO.
041710     MOVE '2'                    TO CZ-RECORD-TYPE.
041710     MOVE WRK-NOTE-SEQ           TO CZ-NOTE-SEQUENCE.
041710     MOVE 'CZ'                   TO CZ-RECORD-ID.
041710     MOVE WS-TODAY-DATE          TO CZ-LAST-MAINT-DT.
041710     MOVE EIBTIME                TO CZ-LAST-MAINT-HHMMSS.
041710     MOVE PI-PROCESSOR-ID        TO CZ-LAST-MAINT-USER.
041710     MOVE PI-REM-BEN-AMT         TO SC-NP6-REM-BEN.
041710     MOVE PI-MO-BEN-AMT          TO SC-NP6-6-MO.
           MOVE ' + 6 MO '             TO SC-NP6-COMM
           IF PI-COMPANY-ID = 'AHL'
              IF PI-LF-BENEFIT-CD = '5I' OR '6J'
                 CONTINUE
              ELSE
                 IF PI-LF-BENEFIT-CD = '5M' OR '6M'
                    MOVE ' + 1 MO '    TO SC-NP6-COMM
                 ELSE
                    MOVE ' + 2 MO '    TO SC-NP6-COMM
                 END-IF
              END-IF
           END-IF
041710     MOVE PI-CPYAMT              TO SC-NP6-AMT-PAID.
041710     MOVE SC-NP6-CERT-NOTE       TO CZ-NOTE.
041710
041710 7181-WRITE.
041710
041710     
      * EXEC CICS WRITE
041710*         DATASET     ('ERCNOT')
041710*         FROM        (CERT-NOTE-FILE)
041710*         RIDFLD      (CZ-CONTROL-PRIMARY)
041710*         RESP        (WS-RESPONSE)
041710*    END-EXEC.
           MOVE LENGTH OF
            CERT-NOTE-FILE
             TO DFHEIV11
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00017283' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303137323833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERT-NOTE-FILE, 
                 DFHEIV11, 
                 CZ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
041710
041710     IF WS-RESP-DUPREC
041710         GO TO 7182-DUPREC
041710     END-IF.
041710
041710     SET CERT-NOTE-ADDED TO TRUE.
041710
041710     IF PI-CPYAMT EQUAL PI-ORIG-BEN-AMT
041710         PERFORM 7190-ADD-ORIG-NOTE THRU 7199-EXIT
041710     END-IF.
041710
041710     GO TO 7189-EXIT.
041710
041710 7182-DUPREC.
041710
041710     ADD +1                      TO  WRK-NOTE-SEQ.
041710     MOVE WRK-NOTE-SEQ           TO  CZ-NOTE-SEQUENCE.
041710     GO TO 7181-WRITE.
041710
041710 7189-EXIT.
041710      EXIT.
041710
041710 7190-ADD-ORIG-NOTE.
041710
041710     
      * EXEC CICS GETMAIN
041710*         SET       (ADDRESS OF CERT-NOTE-FILE)
041710*         LENGTH    (150)
041710*         INITIMG   (GETMAIN-SPACE)
041710*     END-EXEC.
           MOVE 150
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00017313' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303137333133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERT-NOTE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
041710
041710     ADD +1                      TO WRK-NOTE-SEQ.
041710     MOVE LOW-VALUES             TO CERT-NOTE-FILE.
041710     MOVE PI-COMPANY-CD          TO CZ-COMPANY-CD.
041710     MOVE CL-CERT-CARRIER        TO CZ-CARRIER.
041710     MOVE CL-CERT-GROUPING       TO CZ-GROUPING.
041710     MOVE CL-CERT-STATE          TO CZ-STATE.
041710     MOVE CL-CERT-ACCOUNT        TO CZ-ACCOUNT.
041710     MOVE CL-CERT-EFF-DT         TO CZ-CERT-EFF-DT.
041710     MOVE CL-CERT-NO             TO CZ-CERT-NO.
041710     MOVE '2'                    TO CZ-RECORD-TYPE.
041710     MOVE WRK-NOTE-SEQ           TO CZ-NOTE-SEQUENCE.
041710     MOVE 'CZ'                   TO CZ-RECORD-ID.
041710     MOVE WS-TODAY-DATE          TO CZ-LAST-MAINT-DT.
041710     MOVE EIBTIME                TO CZ-LAST-MAINT-HHMMSS.
041710     MOVE PI-PROCESSOR-ID        TO CZ-LAST-MAINT-USER.
041710     MOVE SC-NP6-ORIG-NOTE       TO CZ-NOTE.
041710
041710 7191-WRITE.
041710
041710     
      * EXEC CICS WRITE
041710*         DATASET     ('ERCNOT')
041710*         FROM        (CERT-NOTE-FILE)
041710*         RIDFLD      (CZ-CONTROL-PRIMARY)
041710*         RESP        (WS-RESPONSE)
041710*    END-EXEC.
           MOVE LENGTH OF
            CERT-NOTE-FILE
             TO DFHEIV11
           MOVE 'ERCNOT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00017338' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303137333338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERT-NOTE-FILE, 
                 DFHEIV11, 
                 CZ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
041710
041710     IF WS-RESP-DUPREC
041710         GO TO 7192-DUPREC
041710     END-IF.
041710
041710     GO TO 7199-EXIT.
041710
041710 7192-DUPREC.
041710
041710     ADD +1                      TO  WRK-NOTE-SEQ.
041710     MOVE WRK-NOTE-SEQ           TO  CZ-NOTE-SEQUENCE.
041710     GO TO 7191-WRITE.
041710
041710 7199-EXIT.
041710      EXIT.
07341  7200-FIND-BENEFIT.
07342      MOVE 'N' TO WS-BEN-SEARCH-SW.
07343
07344      
      * EXEC CICS HANDLE CONDITION
07345 *        ENDFILE(7200-EXIT)
07346 *        NOTFND(7200-EXIT)
07347 *    END-EXEC.
      *    MOVE '"$''I                  ! ; #00017362' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303137333632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07348
07349      
      * EXEC CICS READ
07350 *        DATASET('ELCNTL')
07351 *        SET(ADDRESS OF CONTROL-FILE)
07352 *        RIDFLD(ELCNTL-KEY)
07353 *        GTEQ
07354 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00017367' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137333637' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07355
07356      IF CNTL-COMP-ID  NOT = CF-COMPANY-ID  OR
07357         CNTL-REC-TYPE NOT = CF-RECORD-TYPE
07358            GO TO 7200-EXIT.
07359
07360      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT
07361          VARYING SUB-1 FROM 1 BY 1 UNTIL
07362             ((SUB-1 > 8) OR
07363             (CF-BENEFIT-CODE (SUB-1) = WS-BEN-CD)).
07364
07365      IF SUB-1 NOT = 9
07366          MOVE 'Y' TO WS-BEN-SEARCH-SW.
07367
07368      GO TO 7200-EXIT.
07369
07370  7200-BENEFIT-DUMMY.
07371
07372  7200-DUMMY-EXIT.
07373      EXIT.
07374
07375  7200-EXIT.
07376      EXIT.
07377      EJECT
022208*052506
022208*052506 7250-FIND-LETTER-TRLR.
022208*052506     EXEC CICS HANDLE CONDITION
022208*052506         ENDFILE  (7250-END)
022208*052506         NOTFND   (7250-END)
022208*052506     END-EXEC.
022208*052506
022208*052506     EXEC CICS STARTBR
022208*052506         DATASET   ('ELTRLR')
022208*052506         RIDFLD    (ELTRLR-KEY)
022208*052506         GTEQ
022208*052506     END-EXEC.
022208*052506
022208*052506     MOVE LOW-VALUES TO WS-MAX-LETTER-ANSWER-DT.
022208*052506
022208*052506 7250-FIND-LOOP.
022208*052506     EXEC CICS READNEXT
022208*052506         DATASET   ('ELTRLR')
022208*052506         RIDFLD    (ELTRLR-KEY)
022208*052506         SET       (ADDRESS OF ACTIVITY-TRAILERS)
022208*052506     END-EXEC.
022208*052506
022208*052506     IF PI-COMPANY-CD NOT = TRLR-COMP-CD  OR
022208*052506        PI-CARRIER    NOT = TRLR-CARRIER  OR
022208*052506        PI-CLAIM-NO   NOT = TRLR-CLAIM-NO OR
022208*052506        PI-CERT-NO    NOT = TRLR-CERT-NO
022208*052506        GO TO 7250-END
022208*052506     END-IF.
022208*052506
022208*052506     IF AT-TRAILER-TYPE NOT = '4'
022208*052506         GO TO 7250-FIND-LOOP
022208*052506     END-IF.
022208*052506
022208*052506     IF AT-LETTER-ANSWERED-DT GREATER THAN WS-MAX-LETTER-AN
022208*052506         MOVE AT-LETTER-ANSWERED-DT TO WS-MAX-LETTER-ANSWER
022208*052506     END-IF.
022208*052506
022208*052506     GO TO 7250-FIND-LOOP.
022208*052506
022208*052506 7250-END.
022208*052506
022208*052506     IF WS-MAX-LETTER-ANSWER-DT NOT = LOW-VALUES AND SPACES
022208*051506         MOVE SPACES             TO  DC-OPTION-CODE
022208*052506         MOVE WS-MAX-LETTER-ANSWER-DT TO  DC-BIN-DATE-1
022208*052506         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
022208*052506         MOVE DC-GREG-DATE-1-MDY  TO  PI-PROOF-DATE
022208*052506         MOVE PI-PROOF-DATE  TO  PROOFDTO
022208*052506         INSPECT PROOFDTI REPLACING ALL SPACES BY '/'
022208*052506     END-IF.
022208*052506
022208*052506     EXEC CICS ENDBR
022208*052506          DATASET    ('ELTRLR')
022208*052506     END-EXEC.
022208*052506
022208*052506 7250-EXIT.
022208*052506     EXIT.
022208*052506     EJECT
022208*052506
07378  7300-CHECK-AUTO-ACTIVITY.
07379
07380      IF PI-PMTTYPE = '1' OR '2' OR '3' OR '4'
07381          NEXT SENTENCE
07382      ELSE
07383          GO TO 7399-EXIT.
07384
07385      PERFORM 7400-CHECK-AUTO-ACTIVITY THRU 7400-EXIT.
07386
07387      IF WS-ACT-REC-FOUND-SW = 'N'
07388          GO TO 7399-EXIT.
07389
07390      IF PI-ACTIVITY-CODE = 09
07391          GO TO 7300-CONT-AUTO-ACTIVITY.
07392
07393      IF (PI-COMPANY-ID = 'AIG' OR 'AUK') AND
07394         (PI-ACTIVITY-CODE = 11)
07395          GO TO 7300-CONT-AUTO-ACTIVITY.
07396
07397      IF PI-PROVISIONAL-IND = 'P'
07398          IF PI-PMTTYPE = '1'
07399              IF CF-SYS-ACTIVE-SW (9) = ' ' OR 'N'
07400                  MOVE 'N'    TO  PI-RESET-SW
07401                  GO TO 7399-EXIT.
07402
07403      IF PI-PMTTYPE = '1'
07404          IF CF-SYS-ACTIVE-SW (2) = ' ' OR 'N'
07405              MOVE 'N'        TO  PI-RESET-SW
07406              GO TO 7399-EXIT.
07407
07408      IF PI-PMTTYPE = '2' OR '3'
07409          IF CF-SYS-ACTIVE-SW (3) = ' ' OR 'N'
07410              MOVE 'N'        TO  PI-RESET-SW
07411              GO TO 7399-EXIT.
07412
07413      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
07414          IF PI-PROVISIONAL-IND = 'P'
07415              NEXT SENTENCE
07416          ELSE
07417              PERFORM 7500-RESET-AUTO-ACTIVITY THRU 7599-EXIT.
07418
07419  7300-CONT-AUTO-ACTIVITY.
07420
07421      IF PI-PFKEY-USED = DFHPF3 OR DFHPF4
07422          MOVE PI-EPYTHRU     TO DC-GREG-DATE-1-MDY
07423          MOVE '4'            TO DC-OPTION-CODE
07424          MOVE +0             TO DC-ELAPSED-DAYS
07425                                 DC-ELAPSED-MONTHS
07426          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07427          MOVE DC-BIN-DATE-1  TO WS-CHK-THRU-DT
07428          IF PI-USES-PAID-TO
07429              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07430              IF NO-CONVERSION-ERROR
07431                  MOVE +1     TO DC-ELAPSED-DAYS
07432                  MOVE +0     TO DC-ELAPSED-MONTHS
07433                  MOVE '6'    TO DC-OPTION-CODE
07434                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07435                  IF NO-CONVERSION-ERROR
07436                      MOVE DC-BIN-DATE-2   TO WS-CHK-THRU-DT.
07437
07438      IF PI-PFKEY-USED = DFHPF5
07439          MOVE PI-CPYTHRU TO WS-CHK-THRU-DT
07440          IF PI-USES-PAID-TO
07441              MOVE PI-CPYTHRU          TO DC-BIN-DATE-1
07442              MOVE +1                  TO DC-ELAPSED-DAYS
07443              MOVE +0                  TO DC-ELAPSED-MONTHS
07444              MOVE '6'                 TO DC-OPTION-CODE
07445              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07446              IF NO-CONVERSION-ERROR
07447                  MOVE DC-BIN-DATE-2   TO WS-CHK-THRU-DT.
07448
07449      IF WS-CHK-THRU-DT < PI-EXP-DT
07450          NEXT SENTENCE
07451      ELSE
07452          MOVE 03                     TO  WS-ACTIVITY-CODE
07453          GO TO 7390-PROCESS-ACTIVITY.
07454
07455      IF PI-PROV-PMT
07456          MOVE 09                     TO  WS-ACTIVITY-CODE
07457      ELSE
07458         IF PI-PMTTYPE = '1'
07459             MOVE 02                  TO  WS-ACTIVITY-CODE
07460         ELSE
07461         IF PI-PMTTYPE = '2' OR '3'
07462             MOVE 07                  TO  WS-ACTIVITY-CODE
07463         ELSE
07464         IF PI-PMTTYPE = '4'
07465             MOVE 00                  TO  WS-ACTIVITY-CODE
07466         ELSE
07467             MOVE 'N'                 TO  PI-RESET-SW
07468             GO TO 7399-EXIT.
07469
07470  7390-PROCESS-ACTIVITY.
07471
07472      PERFORM 7450-FORMAT-AUTO-LETTER THRU 7450-EXIT.
07473
07474  7399-EXIT.
07475      EXIT.
07476
07477      EJECT
07478  7400-CHECK-AUTO-ACTIVITY.
07479
07480      
      * EXEC CICS HANDLE CONDITION
07481 *        NOTFND   (7400-NOT-FOUND)
07482 *    END-EXEC.
      *    MOVE '"$I                   ! < #00017556' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303137353536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07483
07484      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
07485      MOVE 'T'                    TO  CNTL-REC-TYPE.
07486      MOVE SPACES                 TO  CNTL-ACCESS.
07487      MOVE +0                     TO  CNTL-SEQ-NO.
07488
07489      
      * EXEC CICS READ
07490 *        DATASET   ('ELCNTL')
07491 *        RIDFLD    (ELCNTL-KEY)
07492 *        SET       (ADDRESS OF CONTROL-FILE)
07493 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00017565' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137353635' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07494
07495      MOVE 'Y'                        TO  WS-ACT-REC-FOUND-SW.
07496      GO TO 7400-EXIT.
07497
07498  7400-NOT-FOUND.
07499      MOVE 'N'                        TO  PI-RESET-SW
07500                                          WS-LETTER-SW
07501                                          WS-ACT-REC-FOUND-SW.
07502
07503      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
07504
07505  7400-EXIT.
07506      EXIT.
07507
07508      EJECT
07509  7450-FORMAT-AUTO-LETTER.
07510
07511      IF PI-ACTIVITY-CODE = ZEROS OR LOW-VALUES OR SPACES
07512          MOVE 'Y'                    TO PI-RESET-SW
07513          GO TO 7450-CONTINUE-PROCESS.
07514
07515      MOVE PI-ACTIVITY-CODE           TO  SUB.
07516
07517      IF SUB > 9
07518          SUBTRACT 9 FROM SUB
07519          MOVE CF-USER-RESET-SW (SUB)  TO PI-RESET-SW
07520      ELSE
07521          MOVE CF-SYS-RESET-SW (SUB)   TO PI-RESET-SW.
07522
07523  7450-CONTINUE-PROCESS.
07524
07525      IF WS-ACTIVITY-CODE = ZEROS
07526          MOVE 'N'                    TO  WS-LETTER-SW
07527          GO TO 7450-EXIT.
07528
07529      MOVE WS-ACTIVITY-CODE           TO  SUB.
07530
07531      IF CF-SYS-ACTIVE-SW (SUB) = ' ' OR 'N'
07532          MOVE 'N'                    TO  PI-RESET-SW
07533                                          WS-LETTER-SW
07534          GO TO 7450-EXIT.
07535
07536      IF CF-SYS-LETTER-ID (SUB) = SPACES OR LOW-VALUES
07537          MOVE 'N'                    TO  WS-LETTER-SW
07538          GO TO 7450-EXIT.
07539
07540      MOVE 'Y'                        TO  WS-LETTER-SW.
07541      MOVE LOW-VALUES                 TO  W-1523-LINKDATA.
07542      MOVE PROGRAM-INTERFACE-BLOCK    TO  W-1523-COMMON-PI-DATA.
07543
07544      MOVE CF-SYS-LETTER-ID  (SUB)    TO W-1523-FORM-NUMBER.
07545
07546      IF CF-SYS-RESEND-DAYS (SUB) NOT = ZEROS
07547          MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1
07548          MOVE '6'                    TO  DC-OPTION-CODE
07549          MOVE CF-SYS-RESEND-DAYS (SUB)   TO  DC-ELAPSED-DAYS
07550          MOVE +0                     TO  DC-ELAPSED-MONTHS
07551          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07552          IF NO-CONVERSION-ERROR
07553              MOVE DC-BIN-DATE-2      TO  W-1523-RESEND-DATE
07554          ELSE
07555               MOVE LOW-VALUES        TO  W-1523-RESEND-DATE.
07556
07557      IF CF-SYS-FOLLOW-UP-DAYS (SUB) NOT = ZEROS
07558          MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1
07559          MOVE '6'                    TO  DC-OPTION-CODE
07560          MOVE CF-SYS-FOLLOW-UP-DAYS (SUB) TO DC-ELAPSED-DAYS
07561          MOVE +0                     TO  DC-ELAPSED-MONTHS
07562          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
07563          IF NO-CONVERSION-ERROR
07564              MOVE DC-BIN-DATE-2      TO  W-1523-FOLLOW-UP-DATE
07565          ELSE
07566              MOVE LOW-VALUES         TO  W-1523-FOLLOW-UP-DATE.
07567
07568  7450-EXIT.
07569      EXIT.
07570
07571      EJECT
07572  7500-RESET-AUTO-ACTIVITY.
07573
07574      MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.
07575      MOVE PI-CARRIER             TO  TRLR-CARRIER.
07576      MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.
07577      MOVE PI-CERT-NO             TO  TRLR-CERT-NO.
07578      MOVE +100                   TO  TRLR-SEQ-NO.
07579
07580  7500-STARTBR-TRLR.
07581
07582      
      * EXEC CICS HANDLE CONDITION
07583 *        ENDFILE  (7590-END)
07584 *        NOTFND   (7590-END)
07585 *    END-EXEC.
      *    MOVE '"$''I                  ! = #00017658' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303137363538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07586
07587      
      * EXEC CICS STARTBR
07588 *        DATASET   ('ELTRLR')
07589 *        RIDFLD    (ELTRLR-KEY)
07590 *        GTEQ
07591 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00017663' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303137363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07592
07593      MOVE 'Y'                        TO  WS-BROWSE-SW.
07594
07595  7500-READ-NEXT.
07596
07597      
      * EXEC CICS READNEXT
07598 *        DATASET   ('ELTRLR')
07599 *        RIDFLD    (ELTRLR-KEY)
07600 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
07601 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00017673' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303137363733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07602
07603      IF PI-COMPANY-CD NOT = TRLR-COMP-CD  OR
07604         PI-CARRIER    NOT = TRLR-CARRIER  OR
07605         PI-CLAIM-NO   NOT = TRLR-CLAIM-NO OR
07606         PI-CERT-NO    NOT = TRLR-CERT-NO
07607          GO TO 7590-END.
07608
07609      IF ELTRLR-KEY = PI-PREV-TRLR-KEY
07610          GO TO 7500-READ-NEXT.
07611
07612      IF AT-TRAILER-TYPE NOT = '4'
07613          GO TO 7500-READ-NEXT.
07614
07615      IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES AND SPACES
07616          GO TO 7500-READ-NEXT.
07617
07618      IF (AT-AUTO-RE-SEND-DT   = LOW-VALUES OR SPACES) AND
07619         (AT-RECEIPT-FOLLOW-UP = LOW-VALUES OR SPACES)
07620          GO TO 7500-READ-NEXT.
07621
07622      IF AT-RECEIPT-FOLLOW-UP < SAVE-BIN-DATE AND
07623         AT-AUTO-RE-SEND-DT   < SAVE-BIN-DATE AND
07624         AT-RESEND-PRINT-DATE < SAVE-BIN-DATE
07625          GO TO 7500-READ-NEXT.
07626
07627      IF (AT-AUTO-RE-SEND-DT NOT = LOW-VALUES AND SPACES)
07628                          AND
07629         (AT-RESEND-PRINT-DATE NOT = LOW-VALUES AND SPACES)
07630                          AND
07631         (AT-RECEIPT-FOLLOW-UP = LOW-VALUES)
07632             GO TO 7500-READ-NEXT.
07633
07634      MOVE ELTRLR-KEY                 TO  PI-PREV-TRLR-KEY.
07635
07636      
      * EXEC CICS ENDBR
07637 *        DATASET   ('ELTRLR')
07638 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00017712' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303137373132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07639
07640      MOVE 'N'                        TO  WS-BROWSE-SW.
07641
07642      
      * EXEC CICS READ
07643 *        DATASET   ('ELTRLR')
07644 *        RIDFLD    (ELTRLR-KEY)
07645 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
07646 *        UPDATE
07647 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00017718' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137373138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07648
07649      MOVE 'Y'                        TO  WS-UPDATE-SW.
07650
07651      IF AT-AUTO-RE-SEND-DT NOT < SAVE-BIN-DATE
07652          MOVE LOW-VALUES             TO  AT-AUTO-RE-SEND-DT
07653      ELSE
07654          IF AT-RECEIPT-FOLLOW-UP NOT < SAVE-BIN-DATE
07655              NEXT SENTENCE
07656          ELSE
07657              GO TO 7500-REWRITE-TRLR.
07658
07659      MOVE LOW-VALUES                 TO  AT-RECEIPT-FOLLOW-UP.
07660
07661      MOVE PI-COMPANY-CD              TO  ARCH-CO.
07662      MOVE AT-LETTER-ARCHIVE-NO       TO  ARCH-NUMBER.
07663      MOVE '1'                        TO  ARCH-REC-TYPE.
07664      MOVE +0                         TO  ARCH-SEQ.
07665
07666      PERFORM 7600-READ-ARCH-UPDATE THRU 7699-EXIT.
07667
07668  7500-REWRITE-TRLR.
07669
07670      
      * EXEC CICS REWRITE
07671 *        DATASET   ('ELTRLR')
07672 *        FROM      (ACTIVITY-TRAILERS)
07673 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00017746' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303137373436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07674
07675      GO TO 7500-STARTBR-TRLR.
07676
07677  7590-END.
07678
07679      IF WS-BROWSE-SW = 'Y'
07680          MOVE 'N'                    TO  WS-BROWSE-SW
07681          
      * EXEC CICS ENDBR
07682 *            DATASET   ('ELTRLR')
07683 *        END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00017757' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303137373537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07684
07685  7599-EXIT.
07686      EXIT.
07687
07688      EJECT
07689  7600-READ-ARCH-UPDATE.
07690
07691      
      * EXEC CICS HANDLE CONDITION
07692 *        NOTFND   (7699-EXIT)
07693 *    END-EXEC.
      *    MOVE '"$I                   ! > #00017767' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303137373637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07694
07695      
      * EXEC CICS READ
07696 *        DATASET   ('ELARCH')
07697 *        RIDFLD    (ELARCH-KEY)
07698 *        SET       (ADDRESS OF LETTER-ARCHIVE)
07699 *        UPDATE
07700 *    END-EXEC.
           MOVE 'ELARCH' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00017771' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137373731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07701
07702      MOVE LOW-VALUES                 TO  LA-RESEND-DATE.
07703
07704      
      * EXEC CICS REWRITE
07705 *        DATASET   ('ELARCH')
07706 *        FROM      (LETTER-ARCHIVE)
07707 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
           MOVE 'ELARCH' TO DFHEIV1
      *    MOVE '&& L                  %   #00017780' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303137373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07708
07709  7699-EXIT.
07710      EXIT.
07711                                  EJECT
07712  7700-GET-DCT.
07713
07714      MOVE PI-SV-CERT-KEY         TO W-NOTE-CERT-KEY.
07715      MOVE PI-COMPANY-CD          TO W-NOTE-COMP-CD.
07716      MOVE PI-SV-CERT-NO          TO W-NOTE-CERT-NO.
07717      MOVE ZEROS                  TO PI-INTEREST-PAID-IND
07718                                     PI-MAX-BENEFIT-AMT
07719                                     PI-MAX-BENEFIT-PYMT
07720                                     PI-MIN-PAYMENT-AMT
07721                                     PI-TIME-OF-LOSS-BAL.
07722
07723      
      * EXEC CICS HANDLE CONDITION
07724 *         NOTFND (5300-NOTE-NOT-FOUND)
07725 *     END-EXEC.
      *    MOVE '"$I                   ! ? #00017799' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3F20233030303137373939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07726
07727      
      * EXEC CICS READ
07728 *         DATASET('ERNOTE')
07729 *         SET    (ADDRESS OF CERTIFICATE-NOTE)
07730 *         RIDFLD (W-NOTE-KEY)
07731 *     END-EXEC.
           MOVE 'ERNOTE' TO DFHEIV1
      *    MOVE '&"S        E          (   #00017803' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07732
07733      MOVE CN-CSI-CC-INTEREST-PAID TO PI-INTEREST-PAID-IND.
07734      MOVE CN-CSI-CC-MAX-BEN-LIMIT TO PI-MAX-BENEFIT-AMT.
07735      MOVE CN-CSI-CC-MAX-BENEFITS  TO PI-MAX-BENEFIT-PYMT.
07736      MOVE CN-CSI-CC-MIN-PAY-AMT   TO PI-MIN-PAYMENT-AMT.
07737      MOVE CN-CSI-CC-TOL-BALANCE   TO PI-TIME-OF-LOSS-BAL.
07738
07739      IF PI-PAYEE-TYPE NOT = 'B'
07740          GO TO 7700-EXIT.
07741
07742      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.
07743      MOVE PI-SV-BEN              TO DCT-LOGIC-BENEFICIARY-ID.
07744      MOVE PI-SV-CCN              TO DCT-CREDIT-CARD-NUMBER.
07745
07746      IF PI-SV-CERT-KEY (6:2) = ZERO OR SPACES
07747          MOVE 'CC'                 TO DCT-PRODUCT-CODE
07748      ELSE
07749          MOVE PI-SV-CERT-KEY (6:2) TO DCT-PRODUCT-CODE.
07750
07751      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.
07752      MOVE '01'                   TO DCT-COLUMN-ID-REQUESTED.
07753      MOVE 'DLO006'               TO PGM-NAME.
07754
07755      
      * EXEC CICS LINK
07756 *        PROGRAM    (PGM-NAME)
07757 *        COMMAREA   (DCT-COMMUNICATION-AREA)
07758 *        LENGTH     (DCT-RCRD-LENGTH)
07759 *    END-EXEC.
      *    MOVE '."C                   (   #00017831' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DCT-COMMUNICATION-AREA, 
                 DCT-RCRD-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07760
07761      IF DCT-RETURN-CODE = 'OK'
07762          GO TO 7700-CONT.
07763
07764      IF DCT-RETURN-CODE = '01' OR '02'
07765          GO TO 7700-SET-TO-YES.
07766
07767      IF DCT-RETURN-CODE = '03'
07768          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017844' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07769          MOVE ER-0951            TO EMI-ERROR
07770          MOVE -1                 TO PMTTYPEL
07771          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07772          GO TO 8200-SEND-DATAONLY.
07773
07774      IF DCT-RETURN-CODE = '04'
07775          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017851' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07776          MOVE ER-0946            TO EMI-ERROR
07777          MOVE -1                 TO PMTTYPEL
07778          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07779          GO TO 8200-SEND-DATAONLY.
07780
07781      IF DCT-RETURN-CODE = '05'
07782          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017858' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07783          MOVE ER-0947            TO EMI-ERROR
07784          MOVE -1                 TO PMTTYPEL
07785          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07786          GO TO 8200-SEND-DATAONLY.
07787
07788      IF DCT-RETURN-CODE = '06'
07789          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017865' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07790          MOVE ER-0921            TO EMI-ERROR
07791          MOVE -1                 TO PMTTYPEL
07792          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07793          GO TO 8200-SEND-DATAONLY.
07794
07795      IF DCT-RETURN-CODE = '07'
07796          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017872' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07797          MOVE ER-0919            TO EMI-ERROR
07798          MOVE -1                 TO PMTTYPEL
07799          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07800          GO TO 8200-SEND-DATAONLY.
07801
07802      IF DCT-RETURN-CODE = '08'
07803          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017879' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07804          MOVE ER-0948            TO EMI-ERROR
07805          MOVE -1                 TO PMTTYPEL
07806          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07807          GO TO 8200-SEND-DATAONLY.
07808
07809      IF DCT-RETURN-CODE = 'N1'
07810          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017886' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07811          MOVE ER-0950            TO EMI-ERROR
07812          MOVE -1                 TO PMTTYPEL
07813          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07814          GO TO 8200-SEND-DATAONLY.
07815
07816      IF DCT-RETURN-CODE = 'E1'
07817          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017893' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137383933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07818          MOVE ER-0974            TO EMI-ERROR
07819          MOVE -1                 TO PMTTYPEL
07820          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07821          GO TO 8200-SEND-DATAONLY.
07822
07823      IF DCT-RETURN-CODE = 'E2'
07824          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017900' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137393030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07825          MOVE ER-0975            TO EMI-ERROR
07826          MOVE -1                 TO PMTTYPEL
07827          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07828          GO TO 8200-SEND-DATAONLY.
07829
07830      IF DCT-RETURN-CODE NOT = 'OK'
07831          
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC
      *    MOVE '6"R                   !   #00017907' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303137393037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
07832          MOVE ER-0949            TO EMI-ERROR
07833          MOVE -1                 TO PMTTYPEL
07834          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07835          GO TO 8200-SEND-DATAONLY.
07836
07837  7700-CONT.
07838
020413*    MOVE 'N'                    TO CASHO.
020413*    MOVE +1                     TO CASHL.
020413*    MOVE AL-UANON               TO CASHA.
07842
07843      IF PI-OFFLINE = 'Y'
07844          GO TO 7700-EXIT.
07845
07846      MOVE DCT-DISTRIBUTION-CODE  TO W-DMD-CHECK-NO-1.
07847      MOVE CL-CERT-NO (4:1)       TO W-DMD-CHECK-NO-2.
07848      MOVE 'TP'                   TO W-DMD-CHECK-NO-3.
07849      MOVE W-DMD-CHECK-NO         TO CHECKNOO.
07850      MOVE +7                     TO CHECKNOL.
07851      MOVE AL-UANON               TO CHECKNOA.
07852      GO TO 7700-EXIT.
07853
07854  7700-SET-TO-YES.
07855
020413*    MOVE 'Y'                    TO CASHO.
020413*    MOVE +1                     TO CASHL.
020413*    MOVE AL-UANON               TO CASHA.
07859
07860  7700-EXIT.
07861      EXIT.
07862                                  EJECT
07863  7800-READ-COMP-MSTR.
07864
07865      
      * EXEC CICS HANDLE CONDITION
07866 *        NOTFND   (7800-COMP-NOTFND)
07867 *    END-EXEC.
      *    MOVE '"$I                   ! @ #00017941' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4020233030303137393431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07868
07869      
      * EXEC CICS READ
07870 *        DATASET   ('ERCOMP')
07871 *        RIDFLD    (ERCOMP-KEY)
07872 *        SET       (ADDRESS OF COMPENSATION-MASTER)
07873 *    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00017945' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137393435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07874
07875      MOVE 'Y'                    TO  WS-COMP-MSTR-SW.
07876      GO TO 7800-COMP-EXIT.
07877
07878  7800-COMP-NOTFND.
07879      MOVE 'N'                    TO  WS-COMP-MSTR-SW.
07880
07881  7800-COMP-EXIT.
07882      EXIT.
07883      EJECT
07884  7900-READ-CLAIM.
07885      
      * EXEC CICS READ
07886 *        DATASET  ('ELMSTR')
07887 *        SET      (ADDRESS OF CLAIM-MASTER)
07888 *        RIDFLD   (ELMSTR-KEY)
07889 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00017961' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137393631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07890
07891  7900-EXIT.
07892       EXIT.
07893
07894  7910-READ-CLAIM-UPDATE.
07895      
      * EXEC CICS READ
07896 *        DATASET  ('ELMSTR')
07897 *        SET      (ADDRESS OF CLAIM-MASTER)
07898 *        RIDFLD   (ELMSTR-KEY)
07899 *        UPDATE
07900 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00017971' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07901
07902  7910-EXIT.
07903       EXIT.
07904      EJECT
07905  7920-READ-BENE.
07906      MOVE PI-COMPANY-CD      TO BENE-COMP-CD.
07907      MOVE 'B'                TO BENE-RECORD-TYPE.
07908      MOVE CL-BENEFICIARY     TO BENE-NUMBER.
07909
07910      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
07911         IF PI-PAYEE-TYPE    = 'B' AND
07912            PI-PAYEE-SEQ-NUM = 9
07913              MOVE CL-CURRENT-GROUPING TO WS-AIG-GROUPING
07914              MOVE WS-A-BENE           TO BENE-NUMBER.
07915
07916      
      * EXEC CICS HANDLE CONDITION
07917 *        NOTFND   (7920-NOT-FOUND)
07918 *    END-EXEC.
      *    MOVE '"$I                   ! A #00017992' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4120233030303137393932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07919
07920      
      * EXEC CICS READ
07921 *        DATASET  ('ELBENE')
07922 *        SET      (ADDRESS OF BENEFICIARY-MASTER)
07923 *        RIDFLD   (ELBENE-KEY)
07924 *    END-EXEC.
           MOVE 'ELBENE' TO DFHEIV1
      *    MOVE '&"S        E          (   #00017996' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303137393936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07925
07926      MOVE 'Y' TO BENE-FOUND-SW.
07927      GO TO 7920-EXIT.
07928
07929  7920-NOT-FOUND.
07930      MOVE 'N'                    TO BENE-FOUND-SW.
07931
07932  7920-EXIT.
07933       EXIT.
07934      EJECT
07935  7930-READ-CONTROL.
07936      
      * EXEC CICS READ
07937 *        DATASET  ('ELCNTL')
07938 *        SET      (ADDRESS OF CONTROL-FILE)
07939 *        RIDFLD   (ELCNTL-KEY)
07940 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00018012' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138303132' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07941
07942  7930-EXIT.
07943       EXIT.
07944
07945  7940-READ-CONTROL-UPDATE.
07946      
      * EXEC CICS READ
07947 *        DATASET  ('ELCNTL')
07948 *        SET      (ADDRESS OF CONTROL-FILE)
07949 *        RIDFLD   (ELCNTL-KEY)
07950 *        UPDATE
07951 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00018022' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138303232' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07952
07953  7940-EXIT.
07954       EXIT.
07955      EJECT
07956  7950-READ-TRAILER.
07957      
      * EXEC CICS READ
07958 *        DATASET  ('ELTRLR')
07959 *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
07960 *        RIDFLD   (ELTRLR-KEY)
07961 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00018033' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138303333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07962
07963  7950-EXIT.
07964       EXIT.
07965
07966  7960-READ-TRAILER-UPDATE.
07967      
      * EXEC CICS READ
07968 *        DATASET  ('ELTRLR')
07969 *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
07970 *        RIDFLD   (ELTRLR-KEY)
07971 *        UPDATE
07972 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00018043' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138303433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07973
07974  7960-EXIT.
07975       EXIT.
07976      EJECT
07977  7970-READ-CERT.
07978      
      * EXEC CICS READ
07979 *        DATASET  ('ELCERT')
07980 *        SET      (ADDRESS OF CERTIFICATE-MASTER)
07981 *        RIDFLD   (ELCERT-KEY)
07982 *    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (   #00018054' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138303534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07983
07984  7970-EXIT.
07985       EXIT.
061013 7975-read-acct.
061013
061013     move spaces              to WS-ACCT-RECORD-SW
061013                                 pi-dcc-product-code
061013
061013     MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
061013     MOVE PI-CARRIER          TO ACCT-CARRIER
061013     MOVE PI-GROUPING         TO ACCT-GROUPING
061013     MOVE PI-STATE            TO ACCT-STATE
061013     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
061013     MOVE PI-CERT-EFF-DT      TO ACCT-EXP-DT
061013     MOVE ERACCT-PARTIAL-KEY  TO WS-ERACCT-SAVE-KEY
061013     MOVE SPACES              TO WS-ERACCT-HOLD-RECORD
061013
061013     
      * EXEC CICS READ
061013*       DATASET ('ERACCT')
061013*       RIDFLD  (ERACCT-KEY)
061013*       SET     (ADDRESS OF ACCOUNT-MASTER)
061013*       GTEQ
061013*       resp    (WS-RESPONSE)
061013*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00018076' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303138303736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013     IF WS-RESP-NORMAL
061013        AND WS-ERACCT-SAVE-KEY = AM-CONTROL-PRIMARY (1:20)
061013        and pi-cert-eff-dt < am-expiration-dt
061013        and pi-cert-eff-dt >= am-effective-dt
061013        move am-dcc-product-code to pi-dcc-product-code
061013        set acct-found to true
061013     end-if
061013
061013     .
061013 7975-exit.
061013     exit.
07987  7980-READ-CERT-UPDATE.
07988      
      * EXEC CICS READ
07989 *        DATASET  ('ELCERT')
07990 *        SET      (ADDRESS OF CERTIFICATE-MASTER)
07991 *        RIDFLD   (ELCERT-KEY)
07992 *        UPDATE
07993 *    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00018095' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138303935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
07994
07995  7980-EXIT.
07996       EXIT.
090821 7990-get-lo-hi-acct-dates.
090821
090821     MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
090821     MOVE PI-CARRIER          TO ACCT-CARRIER
090821     MOVE PI-GROUPING         TO ACCT-GROUPING
090821     MOVE PI-STATE            TO ACCT-STATE
090821     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
090821     MOVE low-values          TO ACCT-EXP-DT
090821     MOVE ERACCT-PARTIAL-KEY  TO WS-ERACCT-SAVE-KEY
090821     move spaces              to ws-i-say-stop-ind
090821                                 ws-eracct-startbr-ind
090821                                 ws-acct-status
090821
090821     
      * EXEC CICS STARTBR
090821*         DATASET    ('ERACCT')
090821*         RIDFLD     (ERACCT-KEY)
090821*         GTEQ
090821*         resp       (ws-response)
090821*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00018117' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303138313137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821     if ws-resp-normal
090821        set eracct-browse-started to true
090821     end-if
090821
090821     perform until i-say-stop
090821        
      * EXEC CICS READNEXT
090821*          DATASET ('ERACCT')
090821*          RIDFLD  (ERACCT-KEY)
090821*          SET     (ADDRESS OF ACCOUNT-MASTER)
090821*          resp    (WS-RESPONSE)
090821*       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00018128' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303138313238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821
090821        IF WS-RESP-NORMAL
090821           AND WS-ERACCT-SAVE-KEY = AM-CONTROL-PRIMARY (1:20)
090821           if ws-lo-acct-dt = low-values
090821              move am-effective-dt
090821                                 to ws-lo-acct-dt
090821           end-if
090821           if am-expiration-dt > ws-hi-acct-dt
090821              move am-expiration-dt
090821                                 to ws-hi-acct-dt
090821           end-if
090821           move am-status        to ws-acct-status
090821        else
090821           set i-say-stop to true
090821        end-if
090821     end-perform
090821
090821     if eracct-browse-started
090821        
      * exec cics endbr
090821*          dataset('ERACCT')
090821*       end-exec
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00018152' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303138313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821        move spaces to ws-eracct-startbr-ind
090821     end-if
090821
090821     .
090821 7990-exit.
090821     exit.
07998  8000-LOAD-ERROR-MESSAGES.
07999      IF PI-PASS-SW = 'C'
08000         MOVE EMI-LINE1           TO ERMSG1BO
08001      ELSE
08002         MOVE EMI-LINE1           TO ERRMSG1O
08003         MOVE EMI-LINE2           TO ERRMSG2O.
08004
08005  8000-EXIT.
08006      EXIT.
08007
08008      EJECT
08009
08010  8010-DMD-ERROR-CHECKS.
08011      PERFORM 7700-GET-DCT THRU 7700-EXIT.
08012
08013      IF CN-CSI-CC-ISSUE-DT = SPACES OR LOW-VALUES OR ZEROS
08014          GO TO 8051-ISSUE-ERROR.
08015
08016      MOVE CN-CSI-CC-ISSUE-DT     TO SAVE-DATE-CCYYMMDD.
08017      MOVE SAVE-DATE-YMD          TO DC-GREG-DATE-1-YMD.
08018      MOVE '3'                    TO DC-OPTION-CODE.
08019      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
08020      IF DC-BIN-DATE-1 > CL-INCURRED-DT
08021          GO TO 8051-ISSUE-ERROR.
08022
08023      IF CL-CERT-NO (4:1) = 'U' OR 'F'
08024        IF (CL-SOC-SEC-NO = SPACES OR LOW-VALUES OR ZEROS)
08025                            OR
08026           (CL-SSN-STATE   = CL-CERT-STATE  AND
08027            CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME)
08028              MOVE ER-0851         TO EMI-ERROR
08029              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
08030        END-IF
08031        IF CN-CSI-CC-PREMIUM-AMT = ZERO
08032              MOVE ER-8147         TO EMI-ERROR
08033              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08034
08035      GO TO 8090-EXIT.
08036
08037  8051-ISSUE-ERROR.
08038      MOVE ER-8146            TO EMI-ERROR.
08039      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08040      GO TO 8090-EXIT.
08041
08042  8090-EXIT.
08043       EXIT.
08044
08045  EJECT
08046  8100-SEND-INITIAL-MAP.
08047
08048      MOVE SAVE-DATE              TO RUNDTEAO.
08049      MOVE EIBTIME                TO TIME-IN.
08050      MOVE TIME-OUT               TO RUNTIMAO.
08051      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
08052
           IF MAP-NAME = 'EL156A'
120115        IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121              OR 'FNL'
                 MOVE 'PF6=EOB CODES'  TO PF6TITO
              END-IF
120115        IF PI-COMPANY-ID = 'DCC' or 'VPP'
032015           MOVE 'N'     TO SURVYYNO
032015        END-IF
           END-IF
020413*    IF MAP-NAME = 'EL156A'
020413*       IF PI-COMPANY-ID = 'CRI'
020413*          NEXT SENTENCE
020413*       ELSE
020413*          MOVE AL-SADOF TO  GROUPHA  GROUPEDA.
08064
08065      IF MAP-NAME = 'EL156A'
062121        IF PI-COMPANY-ID = 'BOA' OR 'CID' OR 'AHL' OR 'FNL'
08067            NEXT SENTENCE
08068         ELSE
08069            MOVE AL-SADOF TO  PINTA    PINTHA.
08070
08071      IF MAP-NAME = 'EL156A'
08072         IF PI-COMPANY-ID = 'AIG' OR 'AUK'
08073             NEXT SENTENCE
08074         ELSE
08075            MOVE AL-SADOF TO    DUEDAYHA DUEDAYA.
08076
020413*    IF MAP-NAME = 'EL156A'
020413*       IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'DMD'
020413*          NEXT SENTENCE
020413*       ELSE
020413*          MOVE AL-SADOF TO CASHHA CASHA.
08082
08083      IF MAP-NAME = 'EL156A'
030512        IF PI-COMPANY-ID = 'CVL' OR 'CID' OR 'DCC' OR 'AHL'
062121                        or 'VPP' OR 'FNL'
08085            NEXT SENTENCE
08086         ELSE
08087            MOVE AL-SADOF        TO  LOANHA
08088                                     LOANNOA.
08089
08090      IF MAP-NAME = 'EL156A'
08091         MOVE -1                 TO  ENTERPFL
08092         IF PI-USES-PAID-TO
08093            MOVE 'PAID TO DATE :' TO PTHRHDGO
08094            MOVE '  PAY TO  '    TO  HDGA-VRBLE
08095            MOVE PMT-HEAD-A      TO  PMTHDGAO
08096         ELSE
08097            NEXT SENTENCE
08098      ELSE
08099         IF MAP-NAME = 'EL156B'
08100            IF PI-USES-PAID-TO
08101               MOVE ' PAYS TO    ' TO HDGB-VRBLE
08102               MOVE PMT-HEAD-B     TO PMTHDGBO.
08103
08104      
      * EXEC CICS SEND
08105 *        MAP   (MAP-NAME)
08106 *        MAPSET(MAPSET-NAME)
08107 *        FROM  (EL156AI)
08108 *        ERASE
08109 *        CURSOR
08110 *    END-EXEC.
           MOVE LENGTH OF
            EL156AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00018271' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303138323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL156AI, 
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
           
08111
08112      GO TO 9100-RETURN-TRAN.
08113
08114  8200-SEND-DATAONLY.
08115
08116      IF FIRST-ENTRY = 'Y'
08117          GO TO 8100-SEND-INITIAL-MAP.
08118
08119      IF RETURNED-FROM-B = 'Y'
08120          GO TO 8100-SEND-INITIAL-MAP.
08121
08122      MOVE SAVE-DATE              TO RUNDTEAO.
08123      MOVE EIBTIME                TO TIME-IN.
08124      MOVE TIME-OUT               TO RUNTIMAO.
08125
08126      IF EIBAID = DFHPF11
08127         MOVE 'Y'                 TO EMI-ROLL-SWITCH
08128         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
08129         MOVE 'EL001   '          TO RETURNED-FROM
08130         PERFORM 3050-REBUILD-ENTERED THRU 3059-EXIT
08131         PERFORM 7010-BUILD-MAP THRU 7020-BYPASS-BENEFIT
08132         GO TO 8100-SEND-INITIAL-MAP.
08133
08134      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
08135
08136      IF MAP-NAME =  'EL156A'
08137         MOVE -1                 TO  ENTERPFL
08138         IF PI-USES-PAID-TO
08139            MOVE 'PAID TO DATE :' TO PTHRHDGO
08140            MOVE '  PAY TO  '    TO  HDGA-VRBLE
08141            MOVE PMT-HEAD-A      TO  PMTHDGAO
08142         ELSE
08143            NEXT SENTENCE
08144      ELSE
08145         IF MAP-NAME = 'EL156B'
08146            IF PI-USES-PAID-TO
08147               MOVE ' PAYS TO    ' TO HDGB-VRBLE
08148               MOVE PMT-HEAD-B     TO PMTHDGBO.
08149
08150      IF MAP-NAME =  'EL156A'
08151         IF PI-COMPANY-ID = 'BOA'
08152             IF PI-INT-RATE NUMERIC
08153                 MOVE PI-INT-RATE TO PINTO.
           IF MAP-NAME = 'EL156A'
120115        IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121              OR 'FNL'
                 MOVE 'PF6=EOB CODES'  TO PF6TITO
      *          MOVE 'Y'              TO EOBYNO
              END-IF
           END-IF
08160
08161      
      * EXEC CICS SEND
08162 *        MAP   (MAP-NAME)
08163 *        MAPSET(MAPSET-NAME)
08164 *        FROM  (EL156AI)
08165 *        DATAONLY
08166 *        CURSOR
08167 *    END-EXEC.
           MOVE LENGTH OF
            EL156AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00018329' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303138333239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL156AI, 
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
           
08168
08169      GO TO 9100-RETURN-TRAN.
08170
08171  8300-SEND-TEXT.
08172      
      * EXEC CICS SEND TEXT
08173 *        FROM(LOGOFF-TEXT)
08174 *        LENGTH(LOGOFF-LENGTH)
08175 *        ERASE
08176 *        FREEKB
08177 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00018340' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303138333430' TO DFHEIV0(25:11)
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
           
08178
08179      
      * EXEC CICS RETURN
08180 *    END-EXEC.
      *    MOVE '.(                    ''   #00018347' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303138333437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08181
08182  8500-FILE-NOTOPEN.
08183      IF FILE-SWITCH = 'MSTR'
08184          MOVE ER-0154            TO EMI-ERROR.
08185      IF FILE-SWITCH = 'TRLR'
08186          MOVE ER-0172            TO EMI-ERROR.
08187      IF FILE-SWITCH = 'CERT'
08188          MOVE ER-0169            TO EMI-ERROR.
08189      IF FILE-SWITCH = 'CNTL' OR 'CARR' OR 'BENE' OR
08190                       'PROC' OR 'STAT'
08191          MOVE ER-0042            TO EMI-ERROR.
08192      IF FILE-SWITCH = 'ACTQ'
08193          MOVE ER-0338            TO EMI-ERROR.
08194      IF FILE-SWITCH = 'ACCT'
08195          MOVE ER-0168            TO EMI-ERROR.
08196
08197      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08198      MOVE -1                     TO PMTTYPEL.
08199
08200      IF FIRST-ENTRY = 'Y'
08201          GO TO 8100-SEND-INITIAL-MAP.
08202
08203      GO TO 8200-SEND-DATAONLY.
08204
08205  8600-PGRM-NOT-FOUND.
08206
08207      MOVE ER-0923                TO EMI-ERROR.
08208      MOVE -1                     TO ENTPFBL.
08209      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08210
08211      IF FIRST-ENTRY = 'Y'
08212          GO TO 8100-SEND-INITIAL-MAP
08213      ELSE
08214          GO TO 8200-SEND-DATAONLY.
08215
08216  8800-UNAUTHORIZED-ACCESS.
08217      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
08218      GO TO 8300-SEND-TEXT.
08219
08220  8810-PF23.
08221      MOVE EIBAID                 TO PI-ENTRY-CD-1.
08222      MOVE XCTL-005               TO PGM-NAME.
08223      GO TO 9300-XCTL.
08224
08225  8820-PF7-8.
08226      MOVE XCTL-126               TO PI-CALLING-PROGRAM.
08227      MOVE SPACES                 TO PI-SAVED-PROGRAM-1
08228                                     PI-SAVED-PROGRAM-2
08229                                     PI-SAVED-PROGRAM-3
08230                                     PI-SAVED-PROGRAM-4
08231                                     PI-SAVED-PROGRAM-5
08232                                     PI-SAVED-PROGRAM-6
08233                                     PI-RETURN-TO-PROGRAM.
08234      IF EIBAID = DFHPF8
08235         MOVE XCTL-132            TO PGM-NAME
08236      ELSE
08237         MOVE XCTL-130            TO PGM-NAME.
08238
08239      GO TO 9300-XCTL.
08240
08241  9100-RETURN-TRAN.
08242      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
08243      MOVE '156A'                 TO PI-CURRENT-SCREEN-NO.
08244
08245      
      * EXEC CICS RETURN
08246 *        TRANSID(TRANS-ID)
08247 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
08248 *        LENGTH(PI-COMM-LENGTH)
08249 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00018413' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303138343133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08250
08251  9200-RETURN-MAIN-MENU.
08252      MOVE XCTL-126               TO PGM-NAME.
08253      GO TO 9300-XCTL.
08254
08255  9300-XCTL.
08256      
      * EXEC CICS XCTL
08257 *        PROGRAM(PGM-NAME)
08258 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
08259 *        LENGTH(PI-COMM-LENGTH)
08260 *    END-EXEC.
      *    MOVE '.$C                   %   #00018424' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303138343234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08261
08262  9400-CLEAR.
08263      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
08264      GO TO 9300-XCTL.
08265
08266  9500-PF12.
08267      MOVE XCTL-010               TO PGM-NAME.
08268      GO TO 9300-XCTL.
08269
08270  9700-LINK-DATE-CONVERT.
08271      MOVE LINK-ELDATCV           TO PGM-NAME.
08272      
      * EXEC CICS LINK
08273 *        PROGRAM   (PGM-NAME)
08274 *        COMMAREA  (DATE-CONVERSION-DATA)
08275 *        LENGTH    (DC-COMM-LENGTH)
08276 *    END-EXEC.
      *    MOVE '."C                   (   #00018440' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138343430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08277
08278  9700-EXIT.
08279      EXIT.
08280
08281  9800-LINK-REM-TERM.
08282      MOVE LINK-REMTERM TO PGM-NAME.
08283
08284      
      * EXEC CICS LINK
08285 *        PROGRAM   (PGM-NAME)
08286 *        COMMAREA  (CALCULATION-PASS-AREA)
08287 *        LENGTH    (CP-COMM-LENGTH)
08288 *    END-EXEC.
      *    MOVE '."C                   (   #00018452' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138343532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08289
08290  9800-EXIT.
08291      EXIT.
08292                                  EJECT
08293  9830-DMD-REMAINING-TERM.
08294
08295      DIVIDE CL-NO-OF-DAYS-PAID BY +30
08296          GIVING W-PAID-MONTHS REMAINDER W-PAID-DAYS.
08297
08298      COMPUTE W-TERM-IN-DAYS = CM-AH-ORIG-TERM * 30 +
08299                               CM-PMT-EXTENSION-DAYS.
08300
08301      COMPUTE W-REM-TERM-IN-DAYS = W-TERM-IN-DAYS -
08302                                   CL-NO-OF-DAYS-PAID.
08303
08304      DIVIDE W-REM-TERM-IN-DAYS BY +30
08305          GIVING W-REM REMAINDER W-REM-DAYS.
08306
08307      MOVE W-REM                  TO WST-REM.
08308
08309      IF W-REM-DAYS > ZEROS
08310          MOVE '/'                TO WST-SLASH
08311          MOVE W-REM-DAYS         TO WST-REM-DAYS
08312      ELSE
08313          MOVE SPACES             TO WST-REM-DAYS-GRP.
08314
08315  9830-EXIT.
08316      EXIT.
08317
CIDMOD 9870-OUTPUT-ACTIVITY-RECORD.
CIDMOD
CIDMOD     
      * EXEC CICS GETMAIN
CIDMOD*         SET(ADDRESS OF DAILY-ACTIVITY-RECORD)
CIDMOD*         LENGTH(25)
CIDMOD*         INITIMG(WS-BLANK)
CIDMOD*    END-EXEC.
           MOVE 25
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00018488' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303138343838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
CIDMOD     MOVE ELMSTR-KEY             TO DA-KEY.
CIDMOD     MOVE CL-TRAILER-SEQ-CNT     TO DA-TRAILER-SEQ-NO.
CIDMOD
CIDMOD     IF PI-PMTTYPE EQUAL '7'
CIDMOD         MOVE 'V'                TO DA-RECORD-TYPE
CIDMOD     ELSE
CIDMOD         MOVE 'P'                TO DA-RECORD-TYPE.
CIDMOD
CIDMOD     
      * EXEC CICS HANDLE CONDITION
CIDMOD*         NOTOPEN(9870-NOTOPEN)
CIDMOD*         DUPREC(9870-EXIT)
CIDMOD*    END-EXEC.
      *    MOVE '"$J%                  ! B #00018503' TO DFHEIV0
           MOVE X'22244A252020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4220233030303138353033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     
      * EXEC CICS WRITE
CIDMOD*         DATASET('DLYACTV')
CIDMOD*         RIDFLD(DA-KEY)
CIDMOD*         FROM(DAILY-ACTIVITY-RECORD)
CIDMOD*         LENGTH(25)
CIDMOD*    END-EXEC.
           MOVE 'DLYACTV' TO DFHEIV1
           MOVE 25
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00018508' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303138353038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DAILY-ACTIVITY-RECORD, 
                 DFHEIV11, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD     MOVE 'N'                    TO ERROR-ON-OUTPUT-SW.
CIDMOD     GO TO 9870-EXIT.
CIDMOD
CIDMOD 9870-NOTOPEN.
CIDMOD
CIDMOD     MOVE '2955'                 TO EMI-ERROR.
CIDMOD     MOVE 'Y'                    TO ERROR-ON-OUTPUT-SW.
CIDMOD
CIDMOD 9870-EXIT.
CIDMOD     EXIT.
CIDMOD
08318  9900-ERROR-FORMAT.
08319
08320      IF EMI-ERROR = ER-0923
08321          MOVE PGM-NAME           TO W-CALLED-NAME.
08322
08323      MOVE LINK-001      TO PGM-NAME.
08324      MOVE PI-COMPANY-ID TO EMI-CLIENT-ID.
08325
08326      
      * EXEC CICS LINK
08327 *        PROGRAM (PGM-NAME)
08328 *        COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
08329 *        LENGTH  (EMI-COMM-LENGTH)
08330 *    END-EXEC.
      *    MOVE '."C                   (   #00018533' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138353333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08331
08332      IF EMI-ERROR = ER-0923
08333          MOVE W-CALLED-NAME      TO EMI-TEXT-VARIABLE (1).
08334
08335  9900-EXIT.
08336      EXIT.
08337
08338  9990-ABEND.
08339      MOVE LINK-004               TO PGM-NAME.
08340      MOVE DFHEIBLK               TO EMI-LINE1.
08341      
      * EXEC CICS LINK
08342 *        PROGRAM   (PGM-NAME)
08343 *        COMMAREA  (EMI-LINE1)
08344 *        LENGTH    (64)
08345 *    END-EXEC.
           MOVE 64
             TO DFHEIV11
      *    MOVE '."C                   (   #00018548' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138353438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08346
08347      MOVE 'EL156A'               TO  MAP-NAME.
08348      MOVE LOW-VALUES             TO  EL156AO.
08349      GO TO 8100-SEND-INITIAL-MAP.
08350
08351  9995-SECURITY-VIOLATION.
08352 *    COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00018576' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303138353736' TO DFHEIV0(25:11)
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
08353

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL156' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     8600-PGRM-NOT-FOUND,
                     0400-FIRST-TIME,
                     6180-DUPREC,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0380-CLAIM-END-FILE,
                     0380-CLAIM-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0380-CLAIM-END-FILE,
                     0380-CLAIM-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0800-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1950-NOT-FOUND,
                     1950-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1950-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1950-NOT-FOUND,
                     1950-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1166-END-CHECK,
                     1166-END-CHECK
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 1950-NOT-FOUND,
                     1950-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 1950-NOT-FOUND,
                     1950-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 1950-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 3600-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 5300-NOTE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 6065-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 6068-ACCT-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 6074-GET-COMP-DATA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 6170-WRITE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 6180-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 6190-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 6185-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 6720-BUILD-NEW-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 6900-BUILD-ARCHIVE-HEADER,
                     6900-BUILD-ARCHIVE-HEADER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 7010-BUILD-MAP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 7100-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 7160-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 7200-EXIT,
                     7200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 7400-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 7590-END,
                     7590-END
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 7699-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 5300-NOTE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 7800-COMP-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 7920-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 34
               GO TO 9870-NOTOPEN,
                     9870-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL156' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
