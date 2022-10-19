00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL130 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 04/18/95 07:28:16.
00007 *                            VMOD=2.055.
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00024 *REMARKS.    TRANSACTION - EX19 - NEW CLAIM SETUP
041002*
041002******************************************************************
041002*                   C H A N G E   L O G
041002*
041002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
041002*-----------------------------------------------------------------
041002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
041002* EFFECTIVE    NUMBER
041002*-----------------------------------------------------------------
041002* 041002    2002040200004  SMVA  MAKE SURE CLAIM TYPE IS ALWAYS
041002*                               POPULATED -MOVE PI-LAST-CLAIM-TYPE
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
031405* 031405                   PEMA  ADD PROCESSING FOR NEW CLAM TYP G
050807* 050807    2007020800002  AJRA  AUTO POPULATE I1 ADDRESS FROM CER
071508* 071508    2008071000003  AJRA  ADD EDIT FOR SOC SEC NUMBER
012009* 012009    2007042600001  PEMA  RESTRICT CLAIM TYPE FOR CID
041309* 041309    2009031600001  AJRA  ADD VIN TO DIAG FOR BENE 55 AND 5
100809* 100809    2009081800004  AJRA  ADD MSG FOR BENE CD 2O - 2V
061511* 061511    2011042000002  AJRA  VERIFY 2ND BENEFICIARY SSN
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
020513* 020513  CR2011090100001  PEMA  FORCE ENTRY OF SEX CODE
020613* 020613    2012092400007  AJRA  ADD CAUSAL STATE MESSAGE
042413* 042413    2013042300001  AJRA  FIX SPECIAL MSG WHEN MULTIPLE CER
052113* 052113    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
032514* 032514    2013111100001  AJRA  VERIFY SSN FOR DISAB PMTS
040814* 040814    2014030500002  AJRA  ADD ICD CODES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
070714* 070714    2014052800001  PEMA  correct read on erpdef for DCC
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
101917* 101917  CR2017083000003  TANA  ADD CONTRACT CONTESTABLE MSG
121417* 121417  IR2017121200001  PEMA  Correct assignment of ben period
011118* 011118  CR2016052500002  TANA  Add Message for pre-existing
052918* 052918  CR2018031500002  TANA  Add Message for filing time limit
061418* 061418  IR2018053000003  TANA  Fix Causal state / filing limit
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
071720* 071720  CR2019112600001  TANA  Remove filing time limit error
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
090821* 090821  CR2021081200003  PEMA  Add error if inc > act cnc dt
110921* 110921  CR2021051200001  PEMA  Onbase Workflow project
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
102901******************************************************************
00025
00026      EJECT
00027  ENVIRONMENT DIVISION.
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030  77  FILLER  PIC X(32)  VALUE '********************************'.
00031  77  FILLER  PIC X(32)  VALUE '*    EL130 WORKING STORAGE     *'.
00032  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.055 **********'.
052113 77  A1                          PIC S999 COMP-3 VALUE +0.
052113 77  E1                          PIC S999 COMP-3 VALUE +0.
052113 77  s1                          pic s999 comp-3 value +0.
052113 77  s2                          pic s999 comp-3 value +0.
052113 77  P1                          PIC S999 COMP-3 VALUE +0.
052113 77  P2                          PIC S999 COMP-3 VALUE +0.
052113 77  WS-CRIT-PER-RECURRENT       PIC 99    VALUE zeros.
052113 77  WS-MAX-BENEFITS             PIC 99    VALUE ZEROS.
052113 77  WS-CRIT-PER-RTW-MOS         PIC 99    VALUE ZEROS.
052113 77  WS-EXCL-PERIOD              PIC S999 COMP-3 VALUE +0.
052113 77  WS-COV-ENDS                 PIC S999 COMP-3 VALUE +0.
052113 77  WS-ACC-PERIOD               PIC S999 COMP-3 VALUE +0.
052113 77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
052113 77  WS-ERPDEF-SW                PIC X     VALUE ' '.
052113     88  ERPDEF-FOUND                 VALUE 'Y'.
052113 77  ws-monthly-benefit          pic s9(11)v99 comp-3 value +0.
052113 77  WS-BENEFITS-PREV-PAID       PIC S9(4)V999 VALUE +0  COMP-3.
       77  ws-dcc-product-code         pic xxx value spaces.
       77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
       77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.
       77  ws-zero-bens-avail          pic x value ' '.
011118 77  WS-PRE-EXISTING-PER         PIC 99    VALUE ZEROS.
022122 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
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
00035
00036 *    COPY ELCSCRTY.
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
00037      EJECT
00038  01  WS-DATE-AREA.
00039      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00040      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00041      05  SAVE-SEND-DT        PIC XX      VALUE LOW-VALUES.
00042
052113 01  ws-dcc-error-line.
052113     05  filler occurs 15.
052113         10  ws-error-no        pic x(4).
00043  01  WS-CRI-SSN-WORK-AREA.
00044      05  WS-CRI-SOC-SEC-NO.
00045          10  WS-SS-REPT-CD       PIC X(5).
00046          10  WS-SS-LAST-NAME     PIC X(5).
00047          10  WS-SS-INITIAL       PIC X.
00048
00049      05  WS-CRI-REPORT-CODE.
00050          10  FILLER              PIC X(5).
00051          10  WS-CRI-RPT-CD-MOVE  PIC X(5).
00052
00053      05  WS-CRI-LAST-NAME.
00054          10  WS-CRI-NAME-MOVE    PIC X(5).
00055          10  FILLER              PIC X(10).
00056
090821 01  filler.
090821     05  ws-prev-inc-dt          pic xx value low-values.
090821     05  ws-mob-cert-ind         pic x value ' '.
090821         88  mob-cert        value 'M'.
090821     05  ws-eracct-startbr-ind   pic x  value spaces.
090821         88  eracct-browse-started  value 'Y'.
090821     05  ws-lo-acct-dt           pic xx value low-values.
090821     05  ws-hi-acct-dt           pic xx value low-values.
090821     05  ws-acct-status          pic x value spaces.
090821         88  acct-cancelled          value '3'.
090821     05  WS-I-SAY-STOP-IND       PIC X  VALUE ' '.
090821         88  i-say-STOP            value 'S'.
00057  01  STANDARD-AREAS.
090821     12  er-1679-text        pic x(60) value
090821       '1679-N CONTRACT IS NOT CONTESTABLE'.
090821     12  er-1682-text        pic x(60) value
090821       '1682-N INC DATE > MOB ACCOUNT CANCEL DATE'.
090821     12  er-1683-text        pic x(60) value
090821       '1683-N INC DATE < CERTIFICATE EFF DATE'.
00058      12  SC-ITEM             PIC S9(4)   VALUE +0001      COMP.
00059      12  GETMAIN-SPACE       PIC X       VALUE SPACES.
00060      12  MAP-NAME            PIC X(8)    VALUE 'EL130A'.
00061      12  MAPSET-NAME         PIC X(8)    VALUE 'EL130S'.
00062      12  TRANS-ID            PIC X(4)    VALUE 'EX19'.
00063      12  START-TRANS-ID      PIC X(4)    VALUE 'EX58'.
00064      12  THIS-PGM            PIC X(8)    VALUE 'EL130'.
00065      12  PGM-NAME            PIC X(8).
00066      12  TIME-IN             PIC S9(7).
00067      12  TIME-OUT-R  REDEFINES TIME-IN.
00068          16  FILLER          PIC X.
00069          16  TIME-OUT        PIC 99V99.
00070          16  FILLER          PIC X(2).
00071      12  XCTL-005            PIC X(8)    VALUE 'EL005'.
00072      12  XCTL-010            PIC X(8)    VALUE 'EL010'.
PEMMOD     12  XCTL-114            PIC X(8)    VALUE 'EL114'.
00073      12  XCTL-126            PIC X(8)    VALUE 'EL126'.
00074      12  XCTL-127            PIC X(8)    VALUE 'EL127'.
00075      12  XCTL-727            PIC X(8)    VALUE 'EL727'.
00076      12  XCTL-131            PIC X(8)    VALUE 'EL131'.
00077      12  XCTL-132            PIC X(8)    VALUE 'EL132'.
00078      12  XCTL-141            PIC X(8)    VALUE 'EL141'.
00079      12  XCTL-150            PIC X(8)    VALUE 'EL150'.
00080      12  XCTL-157            PIC X(8)    VALUE 'EL157'.
00081      12  XCTL-650            PIC X(8)    VALUE 'EL650'.
00082      12  XCTL-659            PIC X(8)    VALUE 'EL659'.
00083      12  XCTL-6592           PIC X(8)    VALUE 'EL6592'.
00084      12  XCTL-725            PIC X(8)    VALUE 'EL725'.
00085      12  LINK-001            PIC X(8)    VALUE 'EL001'.
00086      12  LINK-004            PIC X(8)    VALUE 'EL004'.
00087      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00088      12  LINK-ELRTRM         PIC X(8)    VALUE 'ELRTRM'.
00089      12  LINK-1523           PIC X(8)    VALUE 'EL1523'.
00090      12  ELCNTL-DSID         PIC X(8)    VALUE 'ELCNTL'.
00091      12  ELMSTR-DSID         PIC X(8)    VALUE 'ELMSTR'.
00092      12  ELMSTR5-DSID        PIC X(8)    VALUE 'ELMSTR5'.
00093      12  ELTRLR-DSID         PIC X(8)    VALUE 'ELTRLR'.
00094      12  ERACCT2-DSID        PIC X(8)    VALUE 'ERACCT2'.
00095      12  ELCERT-DSID         PIC X(8)    VALUE 'ELCERT'.
00096      12  ELCERT5-DSID        PIC X(8)    VALUE 'ELCERT5'.
00097      12  ELACTQ-DSID         PIC X(8)    VALUE 'ELACTQ'.
00098      12  ELARCH-DSID         PIC X(8)    VALUE 'ELARCH'.
00099      12  ELBENE-DSID         PIC X(8)    VALUE 'ELBENE'.
00100      12  ERREIN-DSID         PIC X(8)    VALUE 'ERREIN'.
CIDMOD     12  DLYACTV-DSID        PIC X(8)    VALUE 'DLYACTV'.
050807     12  ERMAIL-DSID         PIC X(8)    VALUE 'ERMAIL'.
041309     12  ELCRTT-DSID         PIC X(8)    VALUE 'ELCRTT'.
00101      12  FILE-ID             PIC X(8).
CIDMOD     12  DLYACTV-LENGTH      PIC S9(04)    COMP     VALUE +25.
00102      12  ELACTQ-LENGTH       PIC S9(04)    COMP     VALUE +60.
00103      12  ELARCH-LENGTH       PIC S9(04)    COMP     VALUE +90.
00104      12  ELCERT-LENGTH       PIC S9(04)    COMP     VALUE +450.
00105      12  ELMSTR-LENGTH       PIC S9(04)    COMP     VALUE +350.
00106      12  ELTRLR-LENGTH       PIC S9(04)    COMP     VALUE +200.
00107      12  ELMSTR-GENERIC-LENGTH PIC S9(4)   VALUE +9      COMP.
050807     12  ERMAIL-LENGTH       PIC S9(04)    COMP     VALUE +374.
052113     12  ELCRTT-LENGTH       PIC S9(04)    COMP     VALUE +552.
00108
00109  01  TERM-CALCULATION-WORK-AREA     COMP-3.
00110      12  M                   PIC S9(7)V99           VALUE ZEROS.
00111      12  L                   PIC S9(7)V99           VALUE ZEROS.
00112      12  N                   PIC S9(3)              VALUE ZEROS.
00113      12  N-STORE             PIC S9(3)              VALUE ZEROS.
00114      12  NV-STORE            PIC S9(3)              VALUE ZEROS.
00115      12  I                   PIC S99V9(5)           VALUE ZEROS.
00116      12  A-N                 PIC S9(7)V9(8)         VALUE ZEROS.
00117      12  IA-N                PIC S9(7)V9(8)         VALUE ZEROS.
00118      12  V                   PIC S9(3)V9(14)        VALUE ZEROS.
00119      12  R                   PIC S9(3)              VALUE ZEROS.
00120      12  M1                  PIC S9(7)V99           VALUE ZEROS.
00121      12  V-EX-N              PIC S9(3)V9(14)        VALUE ZEROS.
00122      12  TERM1               PIC S9(8)V9(9)         VALUE ZEROS.
00123      12  TERM2               PIC S9(8)V9(9)         VALUE ZEROS.
00124      12  TERM3               PIC S9(8)V9(9)         VALUE ZEROS.
00125      12  TERM4               PIC S9(3)V9(14)        VALUE ZEROS.
00126      12  LEFT-TOT-1          PIC S9(9)V9(8)         VALUE ZEROS.
00127      12  RIGHT-TOT-1         PIC S9(9)V9(8)         VALUE ZEROS.
00128      12  RIGHT-TOT-2         PIC S9(9)V9(8)         VALUE ZEROS.
00129
00130  01  TERM-CALC-WORK-AREA.
00131      12  WS-AH-RATE          PIC S999V9(5)          VALUE ZEROS.
00132      12  WS-LF-RATE          PIC S999V9(5)          VALUE ZEROS.
00133      12  WS-TERM             PIC S9(3)              VALUE ZEROS.
00134      12  WS-TERM-REM         PIC S9(3)V99           VALUE ZEROS.
00135      12  WS-REMAIN           PIC S99                VALUE ZEROS.
00136      12  V-EXPONENTS.
00137         14  V-EXPONENT       PIC S9(3)V9(14) COMP-3 OCCURS 250.
00138      12  V-EX-ONETIME        PIC 9                  VALUE 1.
00139
00140  01  MISC-WORK-AREAS.
PEMMOD     12  SAVE-BENEFICIARY    PIC X(10)   VALUE LOW-VALUES.
00141      12  FILE-SWITCH         PIC X(4)    VALUE SPACES.
00142      12  CLAIM-SWITCH        PIC X       VALUE 'N'.
00143      12  QID.
00144          16  QID-TERM        PIC X(4).
00145          16  FILLER          PIC X(4)    VALUE '130A'.
00146      12  RETURNED-FROM       PIC X(8)    VALUE SPACES.
00147      12  SAVE-AT-PRIMARY-KEY PIC X(22)   VALUE SPACES.
00148      12  WS-GROUP-NO.
00149          16  WS-GROUP-NO-1   PIC X(01)   VALUE SPACES.
00150          16  WS-GROUP-NO-2-6 PIC X(05)   VALUE SPACES.
00151
00152      12  WS-RESPONSE         PIC S9(8)   COMP.
00153          88  WS-RESP-NORMAL              VALUE +00.
00154          88  WS-RESP-ERROR               VALUE +01.
052113         88  WS-RESP-NOTFND              VALUE +13.
052113         88  WS-RESP-DUPKEY              VALUE +15.
052113         88  WS-RESP-NOTOPEN             VALUE +19.
052113         88  WS-RESP-ENDFILE             VALUE +20.
00156
00157 ***************************************************************
00158 *                                                             *
00159 *    QID-MAP-LENGTH MUST BE ADJUSTED EVERY TIME THAT ANY      *
00160 *    FIELDS ARE ADDED, DELETED, OR THE SIZE OF ANY FIELD      *
00161 *    CHANGES WITH IN MAP EL130A.                              *
00162 *                                                             *
00163 ***************************************************************
00164
00165      12  QID-MAP-LENGTH            PIC S9(4) VALUE +1084 COMP.
00166      12  WS-SAVE-CLAIM-MASTER.
00167          16  FILLER                    PIC XX.
00168          16  WS-SAVE-CLAIM-KEY.
00169              20  WS-SAVE-COMPANY-CD    PIC X.
00170              20  WS-SAVE-CARRIER       PIC X.
00171              20  WS-SAVE-CLAIM-NO      PIC X(7).
00172              20  WS-SAVE-CERT-NO       PIC X(11).
00173          16  FILLER                    PIC X(328).
00174      12  SAVE-CURRENT-DATE         PIC XX.
00175      12  SAVE-CONTROL              PIC X(39).
00176      12  SAVE-ERACCT-KEY           PIC X(26).
00177      12  SAVE-COUNTER              PIC S9(8)   COMP.
00178      12  SAVE-METHOD               PIC X.
00179      12  SAVE-COMPANY-ID           PIC XXX.
00180      12  SAVE-COMPANY-CD           PIC X.
00181      12  SAVE-JOURNAL-FILE-ID      PIC S9(4)   COMP.
00182      12  SAVE-CREDIT-USER          PIC X.
00183      12  SAVE-CLAIM-USER           PIC X.
00184      12  SAVE-LIFE-OVERRIDE-L1     PIC X.
00185      12  SAVE-LIFE-OVERRIDE-L2     PIC XX.
00186      12  SAVE-LIFE-OVERRIDE-L6     PIC X(6).
00187      12  SAVE-LIFE-OVERRIDE-L12    PIC X(12).
00188      12  SAVE-AH-OVERRIDE-L1       PIC X.
00189      12  SAVE-AH-OVERRIDE-L2       PIC XX.
00190      12  SAVE-AH-OVERRIDE-L6       PIC X(6).
00191      12  SAVE-AH-OVERRIDE-L12      PIC X(12).
00192      12  FILLER                    PIC X.
00193 *    12  SAVE-CLAIM-ACCESS-CONTROL PIC X.
00194      12  SAVE-CERT-ACCESS-CONTROL  PIC X.
00195      12  SAVE-CARRIER-CONTROL-LEVEL    PIC X.
00196      12  SAVE-EL127-TO-EL130-CNTRL.
00197          16  SAVE-CERT-SELECT-CNT    PIC S9(04) COMP.
00198          16  SAVE-CERT-PROCESSED     PIC S9(04) COMP.
00199          16  SAVE-CERT-CONTROLS-EL127 OCCURS 5 TIMES.
00200              20  SAVE-EL127-CARRIER  PIC X.
00201              20  SAVE-EL127-GROUPING PIC X(6).
00202              20  SAVE-EL127-STATE    PIC X(2).
00203              20  SAVE-EL127-ACCOUNT  PIC X(10).
00204              20  SAVE-EL127-CERT-NO  PIC X(11).
00205              20  SAVE-EL127-EFF-DT   PIC XX.
00206      12  SAVE-EL659-TO-EL130-CNTRL.
00207          16  SAVE-EL659-CARRIER    PIC X.
00208          16  SAVE-EL659-GROUPING   PIC X(6).
00209          16  SAVE-EL659-STATE      PIC XX.
00210          16  SAVE-EL659-ACCOUNT    PIC X(10).
00211      12  DIVIDE-QUOT               PIC 999.
00212      12  DIVIDE-REM                PIC 999.
00213      12  WS-CURRENT-MANUAL-RESERVE PIC S9(5)V99 COMP-3.
00214      12  WS-ITD-ADDITIONAL-RESERVE PIC S9(5)V99 COMP-3.
00215      12  WS-ITD-PAID-EXPENSE       PIC S9(3)V99 COMP-3.
00216      12  WS-ITD-CHARGABLE-EXPENSE  PIC S9(5)V99 COMP-3.
00217      12  WS-INITIAL-MANUAL-RESERVE PIC S9(5)V99 COMP-3.
00218      12  WS-RESERVE-CONTROLS       PIC X(9).
00219      12  WS-EXPENSE-CONTROLS       PIC X(7).
00220      12  WS-REIN-TABLE.
00221          16  WS-REIN-1             PIC X            VALUE SPACE.
00222          16  WS-REIN-2             PIC X            VALUE SPACE.
00223          16  WS-REIN-3             PIC X            VALUE SPACE.
00224      12  WS-BENE-CALC-METHOD       PIC X.
           12  ws-lf-joint-indicator     pic x.
           12  ws-ah-joint-indicator     pic x.
00225      12  WS-EARNINGS-CALC          PIC X.
00226      12  WS-LF-EARNINGS-CALC       PIC X.
00227          88 TEX-REG                VALUE '4'.
00228          88 NET-PAY                VALUE '5'.
00229      12  WS-AH-EARNINGS-CALC       PIC X.
00230      12  WS-SPECIAL-CALC-CD        PIC X(01).
00231      12  WS-LF-SPECIAL-CALC-CD     PIC X(01).
00232      12  WS-AH-SPECIAL-CALC-CD     PIC X(01).
00233      12  WS-DIAGNOSIS-DESCRIPT     PIC X(60).
040814     12  WS-ICD-CODE-1             PIC X(8).
040814     12  WS-ICD-CODE-2             PIC X(8).
00234      12  WS-CLAIM-NUMBER.
00235          16  WS-CN-PREFIX.
00236              20  WS-CN-PRF-A PIC X.
00237              20  WS-CN-PRF-B PIC X.
00238          16  WS-CN-NUMBER    PIC 9(5).
00239      12  WS-CLAIM-NUMBER-R1 REDEFINES WS-CLAIM-NUMBER PIC 9(7).
00240      12  WS-CLAIM-SEQU.
00241          16  FILLER          PIC X    VALUE '('.
00242          16  WS-CURRENT-SEQU PIC Z9.
00243          16  FILLER          PIC X(4) VALUE ' OF '.
00244          16  WS-OF-SEQU      PIC Z9.
00245          16  FILLER          PIC X    VALUE ')'.
00246      12  DATE-WORK.
00247        14  FILLER                PIC XX.
00248        14  DATE-MM               PIC XX.
00249        14  DATE-DD               PIC XX.
00250        14  DATE-YY               PIC XX.
00251      12  CURR-DATE REDEFINES DATE-WORK.
00252          16  CURR-MM.
00253            18  CURR-M1           PIC 9.
00254            18  CURR-M2           PIC 9.
00255          16  FILLER              PIC X(5).
00256          16  CURR-YEAR           PIC 9.
00257      12  NUM-WORK   REDEFINES DATE-WORK   PIC 9(8).
00258      12  SUB                     PIC 99.
00259      12  MY-DATE.
00260          16  MY-MM               PIC XX.
00261          16  FILLER              PIC X VALUE '/'.
00262          16  MY-YY               PIC XX.
00263
00264      12  WS-EDIT-BEN-CODE        PIC XX.
00265          88  INVALID-BENEFIT-CODE   VALUE '  ' '00'
00266                                           '90' THRU '99'.
00267
00268      12  WS-NUM-HOLD             PIC S9(4)     VALUE +0  COMP-3.
00269      12  WS-BEN-HOLD             PIC XX.
00270      12  WS-BEN-ALPHA-HOLD.
00271          15  WS-BEN-ALPHA-2      PIC XX.
00272          15  WS-BEN-ALPHA-1      PIC X.
00273      12  WS-FORM-HOLD            PIC X(12)  VALUE SPACES.
00274      12  WS-REC-TYPE             PIC X.
00275      12  HOLD-LOAN-BAL           PIC 9(7)V99   VALUE ZEROS.
00276      12  DEEDIT-FIELD            PIC 9(10)V99  VALUE ZEROS.
00277      12  ARCH-NUMBER             PIC S9(8)   COMP.
00278      12  WS-REIN-REC-FOUND-SW    PIC X(01)   VALUE ' '.
00279          88  REIN-REC-NOT-FOUND              VALUE 'N'.
00280          88  REIN-REC-FOUND                  VALUE 'Y'.
00281
00282      12  WS-REC-FOUND-SW         PIC X(01)   VALUE ' '.
00283      12  WS-LETTER-SW            PIC X(01)   VALUE 'N'.
00284
CIDMOD
CIDMOD 01  CSO-WORK-FIELDS.
CIDMOD     12  WS-BLANK            PIC X       VALUE ' '.
050807
050807     12  WS-MA-CONTROL-PRIMARY.
050807         16  WS-MA-COMPANY-CD        PIC  X.
050807         16  WS-MA-CARRIER           PIC  X.
050807         16  WS-MA-GROUPING          PIC  X(6).
050807         16  WS-MA-STATE             PIC  XX.
050807         16  WS-MA-ACCOUNT           PIC  X(10).
050807         16  WS-MA-CERT-EFF-DT       PIC  XX.
050807         16  WS-MA-CERT-NO.
050807             20  WS-MA-CERT-PRIME    PIC  X(10).
050807             20  WS-MA-CERT-SFX      PIC  X.
050807
050807     12  WS-INSURED-ADDR-CNT         PIC  S9(1) VALUE +0.
050807     12  WS-NO-INSURED-ADDRESS       PIC  X(01) VALUE 'N'.
071508     12  WS-SOC-SEC-NUMBER.
071508         16  WS-SOC-SEC-NO       PIC 9(9).
071508         16  WS-SOC-SEC-BLANK    PIC X(2).
071508     12  WS-SOC-SEC-REDEF REDEFINES WS-SOC-SEC-NUMBER.
071508         16  WS-SSN-1-3          PIC 9(3).
071508         16  WS-SSN-DASH1        PIC X(1).
071508         16  WS-SSN-4-5          PIC 9(2).
071508         16  WS-SSN-DASH2        PIC X(1).
071508         16  WS-SSN-6-9          PIC 9(4).
041309
041309     12  WS-DIAG-VIN-MSG.
041309         16  FILLER          PIC X(11)
041309             VALUE 'VIN NUMBER '.
041309         16  WS-DIAG-VIN     PIC X(17).
061511
061511     12  WS-VERIFY-NOTE      PIC X(34)
061511         VALUE '2ND BENE SSN VERIFICATION REQUIRED'.
020613
020613     12  WS-CAUSAL-NOTE      PIC X(40)
020613         VALUE 'VERIFY IF DX RELATED TO PRIOR CONDITION '.
032514
032514     12  WS-VFY-SSN-NOTE     PIC X(25)
032514         VALUE 'SSN VERIFICATION REQUIRED'.
061418
061418     12  WS-FILING-NOTE     PIC X(30)
061418         VALUE 'FILING TIME LIMIT EXCEEDED'.
CIDMOD
00285  01  MAP-DATE-AREAS.
00286      12  WS-EFFDT            PIC XX      VALUE LOW-VALUES.
00287      12  WS-EXPIRE           PIC XX      VALUE LOW-VALUES.
00288      12  WS-ADD-ON-DT        PIC XX      VALUE LOW-VALUES.
00289      12  WS-BIRTHDT          PIC XX      VALUE LOW-VALUES.
00290      12  WS-INCUR            PIC XX      VALUE LOW-VALUES.
00291      12  WS-REPORT           PIC XX      VALUE LOW-VALUES.
00292      12  WS-ESTEND           PIC XX      VALUE LOW-VALUES.
00293      12  WS-ACVCNDT          PIC XX      VALUE LOW-VALUES.
00294      12  WS-LCVCNDT          PIC XX      VALUE LOW-VALUES.
00295      12  WS-TODAY-DT         PIC XX      VALUE LOW-VALUES.
00296      12  WS-ENTRYDT          PIC XX      VALUE LOW-VALUES.
00297      12  WS-MANRSV           PIC S9(6)V99  VALUE ZEROS COMP-3.
00298      12  WS-ACVRATE          PIC S9(4)V99  VALUE ZEROS COMP-3.
00299      12  WS-ACVBENE          PIC S9(9)V99  VALUE ZEROS COMP-3.
00300      12  WS-LCVRATE          PIC S9(4)V99  VALUE ZEROS COMP-3.
00301      12  WS-LCVBENE          PIC S9(9)V99  VALUE ZEROS COMP-3.
00302      12  WS-APR              PIC S999V9999 VALUE ZEROS COMP-3.
00303      12  WS-PMTFREQ          PIC 99        VALUE ZEROS COMP-3.
00304      12  WS-ASSOC-CERT-TOTAL PIC S99       VALUE ZEROS.
00305          88  NO-ASSOCIATED-CERTS           VALUE ZEROS.
00306      12  WS-ASSOC-CERT-SEQU  PIC S99       VALUE ZEROS.
00307      12  WS-READNEXT-SWITCH  PIC S99       VALUE +1.
00308      12  WS-ASSOCIATED-CERTS PIC S9        VALUE ZEROS COMP-3.
00309      12  ONE-OR-MIN1         PIC S9        VALUE +1    COMP-3.
00310
00311  01  ACCESS-KEYS.
00312      12  ELACTQ-KEY.
00313          16  ELACTQ-COMPANY-CD       PIC X.
00314          16  ELACTQ-CARRIER          PIC X.
00315          16  ELACTQ-CLAIM-NO         PIC X(7).
00316          16  ELACTQ-CERT-NO          PIC X(11).
00317      12  ELMSTR-KEY.
00318          16  MSTR-COMP-CD            PIC X.
00319          16  MSTR-CARRIER            PIC X.
00320          16  MSTR-CLAIM-NO           PIC X(7).
00321          16  MSTR-CERT-NO.
00322              20  MSTR-CERT-NO-PRIME  PIC X(10).
00323              20  MSTR-CERT-NO-SUFX   PIC X.
00324      12  SAVE-ELMSTR-KEY.
00325          16  SAVE-COMP-CD            PIC X.
00326          16  SAVE-CARRIER            PIC X.
00327          16  SAVE-CLAIM-NO           PIC X(7).
00328          16  SAVE-CERT-NO.
00329              20  SAVE-CERT-NO-PRIME  PIC X(10).
00330              20  SAVE-CERT-NO-SUFX   PIC X.
00331      12  ELMSTR5-KEY.
00332          16  MSTR5-COMP-CD           PIC X.
00333          16  MSTR5-CERT-NO.
00334              20  MSTR5-CERT-NO-PRIME PIC X(10).
00335              20  MSTR5-CERT-NO-SUFX  PIC X.
00336      12  INCUR-DTE-DUPE-SW           PIC X VALUE 'N'.
00337          88  NO-CLAIMS-FOR-CERT            VALUE 'N'.
00338          88  INCURRED-DATE-MATCH           VALUE 'Y'.
00339          88  CLAIM-RECORD-FOUND            VALUE 'H'.
00340      12  ELCNTL-KEY.
00341          16  CNTL-COMP-ID    PIC X(3).
00342          16  CNTL-REC-TYPE   PIC X.
00343          16  CNTL-ACCESS.
00344              20  FILLER      PIC XX.
00345              20  CNTL-BENEFIT.
00346                  24  FILLER  PIC X.
00347                  24  CNTL-CARRIER PIC X.
00348          16  CNTL-SEQ-NO     PIC S9(4)    COMP.
00349      12  ELCERT-KEY.
00350          16  CERT-COMP-CD    PIC X.
00351          16  CERT-CARRIER    PIC X.
00352          16  CERT-GROUPING   PIC X(6).
00353          16  CERT-STATE      PIC X(2).
00354          16  CERT-ACCOUNT.
00355              20  CERT-ACCOUNT-PREFIX PIC X(4).
00356              20  CERT-ACCOUNT-PRIME  PIC X(6).
00357          16  CERT-EFF-DT     PIC XX.
00358          16  CERT-CERT-NO.
00359              20  CERT-CERT-NO-PRIME  PIC X(10).
00360              20  CERT-CERT-NO-SUFX   PIC X.
00361      12  ELCERT-KEY-5.
00362          16  CERT-COMP-CD-5      PIC X.
00363          16  CERT-CERT-5.
00364              20  CERT-CERT-5-PRIME   PIC X(10).
00365              20  CERT-CERT-5-SUFX    PIC X.
00366      12  ERACCT-KEY.
00367          16  ACCT-COMP-CD    PIC X.
00368          16  ACCT-CARRIER    PIC X.
00369          16  ACCT-GROUPING   PIC X(6).
00370          16  ACCT-STATE      PIC X(2).
00371          16  ACCT-ACCOUNT    PIC X(10).
00372          16  ACCT-EXP-DT     PIC XX.
00373      12  ELTRLR-KEY.
00374          16  TRLR-COMP-CD    PIC X.
00375          16  TRLR-CARRIER    PIC X.
00376          16  TRLR-CLAIM-NO   PIC X(7).
00377          16  TRLR-CERT-NO.
00378              20  TRLR-CERT-NO-PRIME  PIC X(10).
00379              20  TRLR-CERT-NO-SUFX   PIC X.
00380          16  TRLR-SEQ-NO     PIC S9(4)   COMP.
00381          16  TRLR-TYPE       PIC X.
00382      12  ELBENE-KEY.
00383          16  BENE-COMP-CD    PIC X.
00384          16  BENE-RCD-TYPE   PIC X.
00385          16  BENE-CODE       PIC X(10).
00386      12  ERREIN-KEY.
00387          16  REIN-COMP-CD        PIC X(01).
00388          16  REIN-TYPE           PIC X(01).
00389          16  REIN-TABLE-CO.
00390              20  REIN-CODE-1     PIC X(01).
00391              20  REIN-CODE-2     PIC X(01).
00392              20  REIN-CODE-3     PIC X(01).
00393          16  FILLER              PIC X(03).
041309     12  ELCRTT-KEY.
041309         16  CTRLR-COMP-CD       PIC X.
041309         16  CTRLR-CARRIER       PIC X.
041309         16  CTRLR-GROUPING      PIC X(6).
041309         16  CTRLR-STATE         PIC X(2).
041309         16  CTRLR-ACCOUNT       PIC X(10).
041309         16  CTRLR-EFF-DT        PIC XX.
041309         16  CTRLR-CERT-NO       PIC X(11).
041309         16  CTRLR-REC-TYPE      PIC X.
       01  ws-acct-found-sw            pic x value ' '.
           88  acct-found                value 'Y'.
052113 01  ERPDEF-KEY-SAVE             PIC X(18).
052113 01  ERPDEF-KEY.
052113     12  ERPDEF-COMPANY-CD       PIC X.
052113     12  ERPDEF-STATE            PIC XX.
052113     12  ERPDEF-PROD-CD          PIC XXX.
052113     12  F                       PIC X(7).
052113     12  ERPDEF-BEN-TYPE         PIC X.
052113     12  ERPDEF-BEN-CODE         PIC XX.
052113     12  ERPDEF-EXP-DT           PIC XX.
00395  01  ERROR-MESSAGES.
00396      12  ER-0000                 PIC X(4)  VALUE '0000'.
00397      12  ER-0004                 PIC X(4)  VALUE '0004'.
00398      12  ER-0019                 PIC X(4)  VALUE '0019'.
00399      12  ER-0023                 PIC X(4)  VALUE '0023'.
00400      12  ER-0029                 PIC X(4)  VALUE '0029'.
00401      12  ER-0033                 PIC X(4)  VALUE '0033'.
00402      12  ER-0068                 PIC X(4)  VALUE '0068'.
00403      12  ER-0070                 PIC X(4)  VALUE '0070'.
00404      12  ER-0194                 PIC X(4)  VALUE '0194'.
00405      12  ER-0202                 PIC X(4)  VALUE '0202'.
00406      12  ER-0203                 PIC X(4)  VALUE '0203'.
00407      12  ER-0204                 PIC X(4)  VALUE '0204'.
00408      12  ER-0205                 PIC X(4)  VALUE '0205'.
00409      12  ER-0206                 PIC X(4)  VALUE '0206'.
00410      12  ER-0207                 PIC X(4)  VALUE '0207'.
00411      12  ER-0213                 PIC X(4)  VALUE '0213'.
00412      12  ER-0214                 PIC X(4)  VALUE '0214'.
00413      12  ER-0217                 PIC X(4)  VALUE '0217'.
00414      12  ER-0219                 PIC X(4)  VALUE '0219'.
00415      12  ER-0220                 PIC X(4)  VALUE '0220'.
00416      12  ER-0221                 PIC X(4)  VALUE '0221'.
00417      12  ER-0222                 PIC X(4)  VALUE '0222'.
00418      12  ER-0223                 PIC X(4)  VALUE '0223'.
00419      12  ER-0224                 PIC X(4)  VALUE '0224'.
00420      12  ER-0225                 PIC X(4)  VALUE '0225'.
00421      12  ER-0226                 PIC X(4)  VALUE '0226'.
00422      12  ER-0227                 PIC X(4)  VALUE '0227'.
00423      12  ER-0229                 PIC X(4)  VALUE '0229'.
00424      12  ER-0230                 PIC X(4)  VALUE '0230'.
00425      12  ER-0231                 PIC X(4)  VALUE '0231'.
00426      12  ER-0232                 PIC X(4)  VALUE '0232'.
00427      12  ER-0233                 PIC X(4)  VALUE '0233'.
00428      12  ER-0234                 PIC X(4)  VALUE '0234'.
00429      12  ER-0235                 PIC X(4)  VALUE '0235'.
00430      12  ER-0236                 PIC X(4)  VALUE '0236'.
00431      12  ER-0237                 PIC X(4)  VALUE '0237'.
00432      12  ER-0238                 PIC X(4)  VALUE '0238'.
00433      12  ER-0239                 PIC X(4)  VALUE '0239'.
00434      12  ER-0240                 PIC X(4)  VALUE '0240'.
00435      12  ER-0241                 PIC X(4)  VALUE '0241'.
00436      12  ER-0243                 PIC X(4)  VALUE '0243'.
00437      12  ER-0244                 PIC X(4)  VALUE '0244'.
00438      12  ER-0245                 PIC X(4)  VALUE '0245'.
00439      12  ER-0246                 PIC X(4)  VALUE '0246'.
00440      12  ER-0247                 PIC X(4)  VALUE '0247'.
00441      12  ER-0248                 PIC X(4)  VALUE '0248'.
00442      12  ER-0250                 PIC X(4)  VALUE '0250'.
00443      12  ER-0251                 PIC X(4)  VALUE '0251'.
00444      12  ER-0252                 PIC X(4)  VALUE '0252'.
00445      12  ER-0253                 PIC X(4)  VALUE '0253'.
00446      12  ER-0254                 PIC X(4)  VALUE '0254'.
00447      12  ER-0257                 PIC X(4)  VALUE '0257'.
00448      12  ER-0258                 PIC X(4)  VALUE '0258'.
00449      12  ER-0260                 PIC X(4)  VALUE '0260'.
00450      12  ER-0261                 PIC X(4)  VALUE '0261'.
00451      12  ER-0263                 PIC X(4)  VALUE '0263'.
00452      12  ER-0264                 PIC X(4)  VALUE '0264'.
00453      12  ER-0265                 PIC X(4)  VALUE '0265'.
00454      12  ER-0266                 PIC X(4)  VALUE '0266'.
00455      12  ER-0267                 PIC X(4)  VALUE '0267'.
00456      12  ER-0334                 PIC X(4)  VALUE '0334'.
00457      12  ER-0337                 PIC X(4)  VALUE '0337'.
00458      12  ER-0412                 PIC X(4)  VALUE '0412'.
00459      12  ER-0413                 PIC X(4)  VALUE '0413'.
00460      12  ER-0428                 PIC X(4)  VALUE '0428'.
00461      12  ER-0429                 PIC X(4)  VALUE '0429'.
00462      12  ER-0430                 PIC X(4)  VALUE '0430'.
00463      12  ER-0433                 PIC X(4)  VALUE '0433'.
00464      12  ER-0434                 PIC X(4)  VALUE '0434'.
00465      12  ER-0458                 PIC X(4)  VALUE '0458'.
00466      12  ER-0489                 PIC X(4)  VALUE '0489'.
00467      12  ER-0509                 PIC X(4)  VALUE '0509'.
00468      12  ER-0510                 PIC X(4)  VALUE '0510'.
00469      12  ER-0511                 PIC X(4)  VALUE '0511'.
00470      12  ER-0512                 PIC X(4)  VALUE '0512'.
00471      12  ER-0516                 PIC X(4)  VALUE '0516'.
00472      12  ER-0517                 PIC X(4)  VALUE '0517'.
00473      12  ER-0518                 PIC X(4)  VALUE '0518'.
00474      12  ER-0519                 PIC X(4)  VALUE '0519'.
00475      12  ER-0520                 PIC X(4)  VALUE '0520'.
00476      12  ER-0521                 PIC X(4)  VALUE '0521'.
00477      12  ER-0522                 PIC X(4)  VALUE '0522'.
00478      12  ER-0523                 PIC X(4)  VALUE '0523'.
00479      12  ER-0544                 PIC X(4)  VALUE '0544'.
00480      12  ER-0546                 PIC X(4)  VALUE '0546'.
00481      12  ER-0558                 PIC X(4)  VALUE '0558'.
00482      12  ER-0560                 PIC X(4)  VALUE '0560'.
00483      12  ER-0562                 PIC X(4)  VALUE '0562'.
00484      12  ER-0565                 PIC X(4)  VALUE '0565'.
00485      12  ER-0569                 PIC X(4)  VALUE '0569'.
00486      12  ER-0693                 PIC X(4)  VALUE '0693'.
00487      12  ER-0715                 PIC X(4)  VALUE '0715'.
00488      12  ER-0840                 PIC X(4)  VALUE '0840'.
00489      12  ER-0841                 PIC X(4)  VALUE '0841'.
00490      12  ER-0842                 PIC X(4)  VALUE '0842'.
00491      12  ER-0843                 PIC X(4)  VALUE '0843'.
100809     12  ER-0878                 PIC X(4)  VALUE '0878'.
071508     12  ER-0887                 PIC X(4)  VALUE '0887'.
040814     12  ER-0992                 PIC X(4)  VALUE '0992'.
052113     12  ER-1651                 PIC X(4)  VALUE '1651'.
052113     12  ER-1652                 PIC X(4)  VALUE '1652'.
052113     12  ER-1653                 PIC X(4)  VALUE '1653'.
052113     12  ER-1654                 PIC X(4)  VALUE '1654'.
052113     12  ER-1655                 PIC X(4)  VALUE '1655'.
052113     12  er-1659                 pic x(4)  value '1659'.
052113     12  er-1660                 pic x(4)  value '1660'.
052113     12  er-1661                 pic x(4)  value '1661'.
052113     12  er-1671                 pic x(4)  value '1671'.
052113     12  er-1672                 pic x(4)  value '1672'.
052113     12  er-1673                 pic x(4)  value '1673'.
           12  er-1675                 pic x(4)  value '1675'.
           12  er-1676                 pic x(4)  value '1676'.
011118     12  ER-1677                 PIC X(4)  VALUE '1677'.
101917     12  ER-1679                 PIC X(4)  VALUE '1679'.
090821     12  er-1682                 pic x(4)  value '1682'.
090821     12  er-1683                 pic x(4)  value '1683'.
00492      12  ER-2280                 PIC X(4)  VALUE '2280'.
00493      12  ER-2370                 PIC X(4)  VALUE '2370'.
00494      12  ER-2371                 PIC X(4)  VALUE '2371'.
00495      12  ER-2378                 PIC X(4)  VALUE '2378'.
00496      12  ER-2380                 PIC X(4)  VALUE '2380'.
00497      12  ER-2566                 PIC X(4)  VALUE '2566'.
00498      12  ER-2848                 PIC X(4)  VALUE '2848'.
050807     12  ER-3548                 PIC X(4)  VALUE '3548'.
00499      12  ER-7008                 PIC X(4)  VALUE '7008'.
00500      12  ER-7319                 PIC X(4)  VALUE '7319'.
00501      12  ER-7321                 PIC X(4)  VALUE '7321'.
00502      12  ER-7352                 PIC X(4)  VALUE '7352'.
00503      12  ER-7353                 PIC X(4)  VALUE '7353'.
052918     12  ER-7572                 PIC X(4)  VALUE '7572'.
061511     12  ER-7575                 PIC X(4)  VALUE '7575'.
020613     12  ER-7577                 PIC X(4)  VALUE '7577'.
032514     12  ER-7582                 PIC X(4)  VALUE '7582'.
00504      12  ER-7634                 PIC X(4)  VALUE '7634'.
00505      12  ER-7635                 PIC X(4)  VALUE '7635'.
00506      12  ER-7636                 PIC X(4)  VALUE '7636'.
00507      12  ER-7637                 PIC X(4)  VALUE '7637'.
00508      12  ER-7638                 PIC X(4)  VALUE '7638'.
00509      12  ER-7639                 PIC X(4)  VALUE '7639'.
00510      12  ER-7640                 PIC X(4)  VALUE '7640'.
00511      12  ER-7641                 PIC X(4)  VALUE '7641'.
00512      12  ER-7642                 PIC X(4)  VALUE '7642'.
00513      12  ER-7643                 PIC X(4)  VALUE '7643'.
00514      12  ER-7646                 PIC X(4)  VALUE '7646'.
00515      12  ER-7647                 PIC X(4)  VALUE '7647'.
00516      12  ER-7648                 PIC X(4)  VALUE '7648'.
00517      12  ER-7649                 PIC X(4)  VALUE '7649'.
00518      12  ER-7651                 PIC X(4)  VALUE '7651'.
00519      12  ER-7670                 PIC X(4)  VALUE '7670'.
00520      12  ER-7671                 PIC X(4)  VALUE '7671'.
00521      12  ER-7686                 PIC X(4)  VALUE '7686'.
00522      12  ER-7688                 PIC X(4)  VALUE '7688'.
00523      12  ER-7689                 PIC X(4)  VALUE '7689'.
00524      12  ER-7691                 PIC X(4)  VALUE '7691'.
110921 01  work-flow-pass-area.
110921     05  pa-rec-type             pic x(4).
110921     05  pa-company-id           pic xxx.
110921     05  pa-rest-of-record       pic x(600).
052113*    COPY ERCPDEF.
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
00526 *    COPY ELCLNKLT.
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
00527      EJECT
00528 *    COPY ELCINTF.
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
00529      12  PI-REDEF     REDEFINES PI-PROGRAM-WORK-AREA.
00530          16  PI-MSTR-DT               PIC XX.
00531          16  PI-MSTR-WHEN             PIC S9(6)   COMP-3.
00532          16  PI-TRLR-DT               PIC XX.
00533          16  PI-TRLR-WHEN             PIC S9(6)   COMP-3.
00534          16  PI-LAST-CLAIM            PIC X(7).
00535          16  PI-LAST-CARR             PIC X.
00536          16  PI-LAST-CERT.
00537              20  PI-LAST-CERT-PRIME   PIC X(10).
00538              20  PI-LAST-CERT-SUFX    PIC X.
00539          16  PI-LAST-CLAIM-TYPE       PIC X.
00540          16  PI-CURSOR                PIC S9(4)   COMP.
00541          16  PI-SAVE-CERT.
00542              20  PI-SAVE-CERT-PRIME   PIC X(10).
00543              20  PI-SAVE-CERT-SUFX    PIC X.
00544          16  PI-SAVE-EFFDT            PIC X(2).
00545          16  PI-SAVE-ACCOUNT          PIC X(10).
00546          16  PI-INCURR-SW             PIC X.
00547          16  PI-PRT-OPT               PIC X.
00548          16  PI-ALT-PRT               PIC X(4).
00549          16  PI-CLAIM-DELETED-SWITCH  PIC X.
00550              88  CLAIM-DELETED-BY-EL131   VALUE 'D'.
00551          16  FILLER                   PIC X(272).
00552          16  PI-EL127-TO-EL130-CNTRL.
00553              20  PI-CERT-SELECT-CNT          PIC S9(04) COMP.
00554              20  PI-CERT-PROCESSED           PIC S9(04) COMP.
00555              20  PI-CERT-CONTROLS-EL127  OCCURS 5 TIMES.
00556                  24  PI-EL127-CARRIER        PIC X.
00557                  24  PI-EL127-GROUPING       PIC X(6).
00558                  24  PI-EL127-STATE          PIC X(2).
00559                  24  PI-EL127-ACCOUNT        PIC X(10).
00560                  24  PI-EL127-CERT-NO.
00561                      28  PI-EL127-CERT-PRIME PIC X(10).
00562                      28  PI-EL127-CERT-SUFX  PIC X.
00563                  24  PI-EL127-EFF-DT         PIC XX.
00564 *        16  FILLER                          PIC X(50).
00565          16  FILLER                          PIC X(31).
00566          16  PI-EL659-TO-EL130-CNTRL.
00567              20  PI-EL659-CARRIER            PIC X.
00568              20  PI-EL659-GROUPING           PIC X(6).
00569              20  PI-EL659-STATE              PIC XX.
00570              20  PI-EL659-ACCOUNT            PIC X(10).
00571
00572          16  FILLER                          PIC X(19).
00573
00574          16  PI-SAVE-INCUR-DT                PIC X(02).
00575          16  PI-LETTER-ERROR-CODE            PIC 9(04).
061511*         16  FILLER                          PIC X(65).
061511         16  PI-ST-VFY-2ND-BENE              PIC X.
020613         16  PI-ST-CAUSAL-STATE              PIC X.
052113         16  pi-dcc-max-benefits             pic s999 comp-3.
052113         16  pi-dcc-max-amt                  pic s9(9)v99 comp-3.
052113         16  pi-emi-sub                      pic 99.
052113         16  FILLER                          PIC X(53).
00577
00578      EJECT
00579 *    COPY EL130S.
       01  EL130AI.
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
           05  COMPL PIC S9(0004) COMP.
           05  COMPF PIC  X(0001).
           05  FILLER REDEFINES COMPF.
               10  COMPA PIC  X(0001).
           05  COMPI PIC  X(0003).
      *    -------------------------------
           05  SEQUL PIC S9(0004) COMP.
           05  SEQUF PIC  X(0001).
           05  FILLER REDEFINES SEQUF.
               10  SEQUA PIC  X(0001).
           05  SEQUI PIC  X(0010).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  CLMNOL PIC S9(0004) COMP.
           05  CLMNOF PIC  X(0001).
           05  FILLER REDEFINES CLMNOF.
               10  CLMNOA PIC  X(0001).
           05  CLMNOI PIC  X(0007).
      *    -------------------------------
           05  CLMCARRL PIC S9(0004) COMP.
           05  CLMCARRF PIC  X(0001).
           05  FILLER REDEFINES CLMCARRF.
               10  CLMCARRA PIC  X(0001).
           05  CLMCARRI PIC  X(0001).
      *    -------------------------------
           05  CLMTYPEL PIC S9(0004) COMP.
           05  CLMTYPEF PIC  X(0001).
           05  FILLER REDEFINES CLMTYPEF.
               10  CLMTYPEA PIC  X(0001).
           05  CLMTYPEI PIC  X(0001).
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
           05  INCURL PIC S9(0004) COMP.
           05  INCURF PIC  X(0001).
           05  FILLER REDEFINES INCURF.
               10  INCURA PIC  X(0001).
           05  INCURI PIC  X(0008).
      *    -------------------------------
           05  REPORTL PIC S9(0004) COMP.
           05  REPORTF PIC  X(0001).
           05  FILLER REDEFINES REPORTF.
               10  REPORTA PIC  X(0001).
           05  REPORTI PIC  X(0008).
      *    -------------------------------
           05  ICD1L PIC S9(0004) COMP.
           05  ICD1F PIC  X(0001).
           05  FILLER REDEFINES ICD1F.
               10  ICD1A PIC  X(0001).
           05  ICD1I PIC  X(0008).
      *    -------------------------------
           05  ICD2L PIC S9(0004) COMP.
           05  ICD2F PIC  X(0001).
           05  FILLER REDEFINES ICD2F.
               10  ICD2A PIC  X(0001).
           05  ICD2I PIC  X(0008).
      *    -------------------------------
           05  DIAGL PIC S9(0004) COMP.
           05  DIAGF PIC  X(0001).
           05  FILLER REDEFINES DIAGF.
               10  DIAGA PIC  X(0001).
           05  DIAGI PIC  X(0060).
      *    -------------------------------
           05  BENECDL PIC S9(0004) COMP.
           05  BENECDF PIC  X(0001).
           05  FILLER REDEFINES BENECDF.
               10  BENECDA PIC  X(0001).
           05  BENECDI PIC  X(0010).
      *    -------------------------------
           05  BIRTHDTL PIC S9(0004) COMP.
           05  BIRTHDTF PIC  X(0001).
           05  FILLER REDEFINES BIRTHDTF.
               10  BIRTHDTA PIC  X(0001).
           05  BIRTHDTI PIC  X(0008).
      *    -------------------------------
           05  SSNL PIC S9(0004) COMP.
           05  SSNF PIC  X(0001).
           05  FILLER REDEFINES SSNF.
               10  SSNA PIC  X(0001).
           05  SSNI PIC  X(0011).
      *    -------------------------------
           05  SEXL PIC S9(0004) COMP.
           05  SEXF PIC  X(0001).
           05  FILLER REDEFINES SEXF.
               10  SEXA PIC  X(0001).
           05  SEXI PIC  X(0001).
      *    -------------------------------
           05  INSTYPEL PIC S9(0004) COMP.
           05  INSTYPEF PIC  X(0001).
           05  FILLER REDEFINES INSTYPEF.
               10  INSTYPEA PIC  X(0001).
           05  INSTYPEI PIC  X(0001).
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
           05  INITL PIC S9(0004) COMP.
           05  INITF PIC  X(0001).
           05  FILLER REDEFINES INITF.
               10  INITA PIC  X(0001).
           05  INITI PIC  X(0001).
      *    -------------------------------
           05  LOANNOL PIC S9(0004) COMP.
           05  LOANNOF PIC  X(0001).
           05  FILLER REDEFINES LOANNOF.
               10  LOANNOA PIC  X(0001).
           05  LOANNOI PIC  X(0008).
      *    -------------------------------
           05  CRTLNMEL PIC S9(0004) COMP.
           05  CRTLNMEF PIC  X(0001).
           05  FILLER REDEFINES CRTLNMEF.
               10  CRTLNMEA PIC  X(0001).
           05  CRTLNMEI PIC  X(0015).
      *    -------------------------------
           05  CRTFNMEL PIC S9(0004) COMP.
           05  CRTFNMEF PIC  X(0001).
           05  FILLER REDEFINES CRTFNMEF.
               10  CRTFNMEA PIC  X(0001).
           05  CRTFNMEI PIC  X(0012).
      *    -------------------------------
           05  CRTINITL PIC S9(0004) COMP.
           05  CRTINITF PIC  X(0001).
           05  FILLER REDEFINES CRTINITF.
               10  CRTINITA PIC  X(0001).
           05  CRTINITI PIC  X(0001).
      *    -------------------------------
           05  LOANBALL PIC S9(0004) COMP.
           05  LOANBALF PIC  X(0001).
           05  FILLER REDEFINES LOANBALF.
               10  LOANBALA PIC  X(0001).
           05  LOANBALI PIC  9(7)V99.
      *    -------------------------------
           05  MANRSVL PIC S9(0004) COMP.
           05  MANRSVF PIC  X(0001).
           05  FILLER REDEFINES MANRSVF.
               10  MANRSVA PIC  X(0001).
           05  MANRSVI PIC  9(7)V99.
      *    -------------------------------
           05  SUPVL PIC S9(0004) COMP.
           05  SUPVF PIC  X(0001).
           05  FILLER REDEFINES SUPVF.
               10  SUPVA PIC  X(0001).
           05  SUPVI PIC  X(0001).
      *    -------------------------------
           05  PRICDL PIC S9(0004) COMP.
           05  PRICDF PIC  X(0001).
           05  FILLER REDEFINES PRICDF.
               10  PRICDA PIC  X(0001).
           05  PRICDI PIC  X(0001).
      *    -------------------------------
           05  PRODL PIC S9(0004) COMP.
           05  PRODF PIC  X(0001).
           05  FILLER REDEFINES PRODF.
               10  PRODA PIC  X(0001).
           05  PRODI PIC  X(0008).
      *    -------------------------------
           05  PRODCDL PIC S9(0004) COMP.
           05  PRODCDF PIC  X(0001).
           05  FILLER REDEFINES PRODCDF.
               10  PRODCDA PIC  X(0001).
           05  PRODCDI PIC  X(0001).
      *    -------------------------------
           05  FILETOL PIC S9(0004) COMP.
           05  FILETOF PIC  X(0001).
           05  FILLER REDEFINES FILETOF.
               10  FILETOA PIC  X(0001).
           05  FILETOI PIC  X(0004).
      *    -------------------------------
           05  PROCCDL PIC S9(0004) COMP.
           05  PROCCDF PIC  X(0001).
           05  FILLER REDEFINES PROCCDF.
               10  PROCCDA PIC  X(0001).
           05  PROCCDI PIC  X(0004).
      *    -------------------------------
           05  RELCLML PIC S9(0004) COMP.
           05  RELCLMF PIC  X(0001).
           05  FILLER REDEFINES RELCLMF.
               10  RELCLMA PIC  X(0001).
           05  RELCLMI PIC  X(0007).
      *    -------------------------------
           05  PRTOPTL PIC S9(0004) COMP.
           05  PRTOPTF PIC  X(0001).
           05  FILLER REDEFINES PRTOPTF.
               10  PRTOPTA PIC  X(0001).
           05  PRTOPTI PIC  X(0001).
      *    -------------------------------
           05  ALTPRTL PIC S9(0004) COMP.
           05  ALTPRTF PIC  X(0001).
           05  FILLER REDEFINES ALTPRTF.
               10  ALTPRTA PIC  X(0001).
           05  ALTPRTI PIC  X(0004).
      *    -------------------------------
           05  CERTMTL PIC S9(0004) COMP.
           05  CERTMTF PIC  X(0001).
           05  FILLER REDEFINES CERTMTF.
               10  CERTMTA PIC  X(0001).
           05  CERTMTI PIC  X(0001).
      *    -------------------------------
           05  CRTCARRL PIC S9(0004) COMP.
           05  CRTCARRF PIC  X(0001).
           05  FILLER REDEFINES CRTCARRF.
               10  CRTCARRA PIC  X(0001).
           05  CRTCARRI PIC  X(0001).
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
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0008).
      *    -------------------------------
           05  CRTSSNL PIC S9(0004) COMP.
           05  CRTSSNF PIC  X(0001).
           05  FILLER REDEFINES CRTSSNF.
               10  CRTSSNA PIC  X(0001).
           05  CRTSSNI PIC  X(0011).
      *    -------------------------------
           05  MEMCAPL PIC S9(0004) COMP.
           05  MEMCAPF PIC  X(0001).
           05  FILLER REDEFINES MEMCAPF.
               10  MEMCAPA PIC  X(0001).
           05  MEMCAPI PIC  X(0010).
      *    -------------------------------
           05  MEMBERL PIC S9(0004) COMP.
           05  MEMBERF PIC  X(0001).
           05  FILLER REDEFINES MEMBERF.
               10  MEMBERA PIC  X(0001).
           05  MEMBERI PIC  X(0012).
      *    -------------------------------
           05  REINCDL PIC S9(0004) COMP.
           05  REINCDF PIC  X(0001).
           05  FILLER REDEFINES REINCDF.
               10  REINCDA PIC  X(0001).
           05  REINCDI PIC  X(0001).
      *    -------------------------------
           05  ISSAGEL PIC S9(0004) COMP.
           05  ISSAGEF PIC  X(0001).
           05  FILLER REDEFINES ISSAGEF.
               10  ISSAGEA PIC  X(0001).
           05  ISSAGEI PIC  X(0002).
      *    -------------------------------
           05  APRL PIC S9(0004) COMP.
           05  APRF PIC  X(0001).
           05  FILLER REDEFINES APRF.
               10  APRA PIC  X(0001).
           05  APRI PIC  9(4)V9(4).
      *    -------------------------------
           05  PMTFREQL PIC S9(0004) COMP.
           05  PMTFREQF PIC  X(0001).
           05  FILLER REDEFINES PMTFREQF.
               10  PMTFREQA PIC  X(0001).
           05  PMTFREQI PIC  99.
      *    -------------------------------
           05  INDGRPL PIC S9(0004) COMP.
           05  INDGRPF PIC  X(0001).
           05  FILLER REDEFINES INDGRPF.
               10  INDGRPA PIC  X(0001).
           05  INDGRPI PIC  X(0001).
      *    -------------------------------
           05  PREMTYPL PIC S9(0004) COMP.
           05  PREMTYPF PIC  X(0001).
           05  FILLER REDEFINES PREMTYPF.
               10  PREMTYPA PIC  X(0001).
           05  PREMTYPI PIC  X(0001).
      *    -------------------------------
           05  ADDONDTL PIC S9(0004) COMP.
           05  ADDONDTF PIC  X(0001).
           05  FILLER REDEFINES ADDONDTF.
               10  ADDONDTA PIC  X(0001).
           05  ADDONDTI PIC  X(0008).
      *    -------------------------------
           05  JNTLNMEL PIC S9(0004) COMP.
           05  JNTLNMEF PIC  X(0001).
           05  FILLER REDEFINES JNTLNMEF.
               10  JNTLNMEA PIC  X(0001).
           05  JNTLNMEI PIC  X(0015).
      *    -------------------------------
           05  JNTFNMEL PIC S9(0004) COMP.
           05  JNTFNMEF PIC  X(0001).
           05  FILLER REDEFINES JNTFNMEF.
               10  JNTFNMEA PIC  X(0001).
           05  JNTFNMEI PIC  X(0002).
      *    -------------------------------
           05  JNTINITL PIC S9(0004) COMP.
           05  JNTINITF PIC  X(0001).
           05  FILLER REDEFINES JNTINITF.
               10  JNTINITA PIC  X(0001).
           05  JNTINITI PIC  X(0001).
      *    -------------------------------
           05  JNTAGEL PIC S9(0004) COMP.
           05  JNTAGEF PIC  X(0001).
           05  FILLER REDEFINES JNTAGEF.
               10  JNTAGEA PIC  X(0001).
           05  JNTAGEI PIC  X(0002).
      *    -------------------------------
           05  LCVDSCRL PIC S9(0004) COMP.
           05  LCVDSCRF PIC  X(0001).
           05  FILLER REDEFINES LCVDSCRF.
               10  LCVDSCRA PIC  X(0001).
           05  LCVDSCRI PIC  X(0006).
      *    -------------------------------
           05  LCVKINDL PIC S9(0004) COMP.
           05  LCVKINDF PIC  X(0001).
           05  FILLER REDEFINES LCVKINDF.
               10  LCVKINDA PIC  X(0001).
           05  LCVKINDI PIC  X(0003).
      *    -------------------------------
           05  LCVCDL PIC S9(0004) COMP.
           05  LCVCDF PIC  X(0001).
           05  FILLER REDEFINES LCVCDF.
               10  LCVCDA PIC  X(0001).
           05  LCVCDI PIC  X(0002).
      *    -------------------------------
           05  LCVOTRML PIC S9(0004) COMP.
           05  LCVOTRMF PIC  X(0001).
           05  FILLER REDEFINES LCVOTRMF.
               10  LCVOTRMA PIC  X(0001).
           05  LCVOTRMI PIC  999.
      *    -------------------------------
           05  LCVRTRML PIC S9(0004) COMP.
           05  LCVRTRMF PIC  X(0001).
           05  FILLER REDEFINES LCVRTRMF.
               10  LCVRTRMA PIC  X(0001).
           05  LCVRTRMI PIC  X(0003).
      *    -------------------------------
           05  LCVRATEL PIC S9(0004) COMP.
           05  LCVRATEF PIC  X(0001).
           05  FILLER REDEFINES LCVRATEF.
               10  LCVRATEA PIC  X(0001).
           05  LCVRATEI PIC  9999V99.
      *    -------------------------------
           05  LCVBENEL PIC S9(0004) COMP.
           05  LCVBENEF PIC  X(0001).
           05  FILLER REDEFINES LCVBENEF.
               10  LCVBENEA PIC  X(0001).
           05  LCVBENEI PIC  9(9)V99.
      *    -------------------------------
           05  LCVFORML PIC S9(0004) COMP.
           05  LCVFORMF PIC  X(0001).
           05  FILLER REDEFINES LCVFORMF.
               10  LCVFORMA PIC  X(0001).
           05  LCVFORMI PIC  X(0012).
      *    -------------------------------
           05  LCVCNDTL PIC S9(0004) COMP.
           05  LCVCNDTF PIC  X(0001).
           05  FILLER REDEFINES LCVCNDTF.
               10  LCVCNDTA PIC  X(0001).
           05  LCVCNDTI PIC  X(0008).
      *    -------------------------------
           05  LCVEXITL PIC S9(0004) COMP.
           05  LCVEXITF PIC  X(0001).
           05  FILLER REDEFINES LCVEXITF.
               10  LCVEXITA PIC  X(0001).
           05  LCVEXITI PIC  X(0008).
      *    -------------------------------
           05  LCVSTATL PIC S9(0004) COMP.
           05  LCVSTATF PIC  X(0001).
           05  FILLER REDEFINES LCVSTATF.
               10  LCVSTATA PIC  X(0001).
           05  LCVSTATI PIC  X(0006).
      *    -------------------------------
           05  ACVDSCRL PIC S9(0004) COMP.
           05  ACVDSCRF PIC  X(0001).
           05  FILLER REDEFINES ACVDSCRF.
               10  ACVDSCRA PIC  X(0001).
           05  ACVDSCRI PIC  X(0006).
      *    -------------------------------
           05  ACVKINDL PIC S9(0004) COMP.
           05  ACVKINDF PIC  X(0001).
           05  FILLER REDEFINES ACVKINDF.
               10  ACVKINDA PIC  X(0001).
           05  ACVKINDI PIC  X(0003).
      *    -------------------------------
           05  ACVCDL PIC S9(0004) COMP.
           05  ACVCDF PIC  X(0001).
           05  FILLER REDEFINES ACVCDF.
               10  ACVCDA PIC  X(0001).
           05  ACVCDI PIC  X(0002).
      *    -------------------------------
           05  ACVOTRML PIC S9(0004) COMP.
           05  ACVOTRMF PIC  X(0001).
           05  FILLER REDEFINES ACVOTRMF.
               10  ACVOTRMA PIC  X(0001).
           05  ACVOTRMI PIC  999.
      *    -------------------------------
           05  ACVRTRML PIC S9(0004) COMP.
           05  ACVRTRMF PIC  X(0001).
           05  FILLER REDEFINES ACVRTRMF.
               10  ACVRTRMA PIC  X(0001).
           05  ACVRTRMI PIC  X(0003).
      *    -------------------------------
           05  ACVRATEL PIC S9(0004) COMP.
           05  ACVRATEF PIC  X(0001).
           05  FILLER REDEFINES ACVRATEF.
               10  ACVRATEA PIC  X(0001).
           05  ACVRATEI PIC  9999V99.
      *    -------------------------------
           05  ACVBENEL PIC S9(0004) COMP.
           05  ACVBENEF PIC  X(0001).
           05  FILLER REDEFINES ACVBENEF.
               10  ACVBENEA PIC  X(0001).
           05  ACVBENEI PIC  9(9)V99.
      *    -------------------------------
           05  ACVFORML PIC S9(0004) COMP.
           05  ACVFORMF PIC  X(0001).
           05  FILLER REDEFINES ACVFORMF.
               10  ACVFORMA PIC  X(0001).
           05  ACVFORMI PIC  X(0012).
      *    -------------------------------
           05  ACVCNDTL PIC S9(0004) COMP.
           05  ACVCNDTF PIC  X(0001).
           05  FILLER REDEFINES ACVCNDTF.
               10  ACVCNDTA PIC  X(0001).
           05  ACVCNDTI PIC  X(0008).
      *    -------------------------------
           05  ACVEXITL PIC S9(0004) COMP.
           05  ACVEXITF PIC  X(0001).
           05  FILLER REDEFINES ACVEXITF.
               10  ACVEXITA PIC  X(0001).
           05  ACVEXITI PIC  X(0008).
      *    -------------------------------
           05  ACVSTATL PIC S9(0004) COMP.
           05  ACVSTATF PIC  X(0001).
           05  FILLER REDEFINES ACVSTATF.
               10  ACVSTATA PIC  X(0001).
           05  ACVSTATI PIC  X(0006).
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
           05  BCERT1L PIC S9(0004) COMP.
           05  BCERT1F PIC  X(0001).
           05  FILLER REDEFINES BCERT1F.
               10  BCERT1A PIC  X(0001).
           05  BCERT1I PIC  X(0010).
      *    -------------------------------
           05  BSUFX1L PIC S9(0004) COMP.
           05  BSUFX1F PIC  X(0001).
           05  FILLER REDEFINES BSUFX1F.
               10  BSUFX1A PIC  X(0001).
           05  BSUFX1I PIC  X(0001).
      *    -------------------------------
           05  BCERT2L PIC S9(0004) COMP.
           05  BCERT2F PIC  X(0001).
           05  FILLER REDEFINES BCERT2F.
               10  BCERT2A PIC  X(0001).
           05  BCERT2I PIC  X(0010).
      *    -------------------------------
           05  BSUFX2L PIC S9(0004) COMP.
           05  BSUFX2F PIC  X(0001).
           05  FILLER REDEFINES BSUFX2F.
               10  BSUFX2A PIC  X(0001).
           05  BSUFX2I PIC  X(0001).
      *    -------------------------------
           05  BCERT3L PIC S9(0004) COMP.
           05  BCERT3F PIC  X(0001).
           05  FILLER REDEFINES BCERT3F.
               10  BCERT3A PIC  X(0001).
           05  BCERT3I PIC  X(0010).
      *    -------------------------------
           05  BSUFX3L PIC S9(0004) COMP.
           05  BSUFX3F PIC  X(0001).
           05  FILLER REDEFINES BSUFX3F.
               10  BSUFX3A PIC  X(0001).
           05  BSUFX3I PIC  X(0001).
      *    -------------------------------
           05  BCERT4L PIC S9(0004) COMP.
           05  BCERT4F PIC  X(0001).
           05  FILLER REDEFINES BCERT4F.
               10  BCERT4A PIC  X(0001).
           05  BCERT4I PIC  X(0010).
      *    -------------------------------
           05  BSUFX4L PIC S9(0004) COMP.
           05  BSUFX4F PIC  X(0001).
           05  FILLER REDEFINES BSUFX4F.
               10  BSUFX4A PIC  X(0001).
           05  BSUFX4I PIC  X(0001).
      *    -------------------------------
           05  BCERT5L PIC S9(0004) COMP.
           05  BCERT5F PIC  X(0001).
           05  FILLER REDEFINES BCERT5F.
               10  BCERT5A PIC  X(0001).
           05  BCERT5I PIC  X(0010).
      *    -------------------------------
           05  BSUFX5L PIC S9(0004) COMP.
           05  BSUFX5F PIC  X(0001).
           05  FILLER REDEFINES BSUFX5F.
               10  BSUFX5A PIC  X(0001).
           05  BSUFX5I PIC  X(0001).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  99.
      *    -------------------------------
           05  PF5L PIC S9(0004) COMP.
           05  PF5F PIC  X(0001).
           05  FILLER REDEFINES PF5F.
               10  PF5A PIC  X(0001).
           05  PF5I PIC  X(0013).
       01  EL130AO REDEFINES EL130AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQUO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PSUFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCURO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPORTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ICD1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ICD2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DIAGO PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENECDO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIRTHDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SSNO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INSTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FSTNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANNOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTLNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTFNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANBALO PIC  ZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MANRSVO PIC  Z(5)9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUPVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRICDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRODO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRODCDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILETOO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RELCLMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTPRTO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTMTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTCARRO PIC  X(0001).
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
           05  EFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTSSNO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEMCAPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEMBERO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REINCDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ISSAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APRO PIC  9(3).9(4).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTFREQO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREMTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDONDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTLNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFNMEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVDSCRO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVKINDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVOTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVRTRMO PIC  ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVRATEO PIC  ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVBENEO PIC  ZZZZZZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVFORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVCNDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVEXITO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVSTATO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVDSCRO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVKINDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVOTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVRTRMO PIC  ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVRATEO PIC  ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVBENEO PIC  ZZZZZZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVFORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVCNDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVEXITO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVSTATO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUFX1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUFX2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUFX3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUFX4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUFX5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF5O PIC  X(0013).
      *    -------------------------------
00580      EJECT
00581 *    COPY ELCDATE.
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
00582      EJECT
00583 *    COPY ELCCALC.
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
00584      EJECT
00585 *    COPY ELCLOGOF.
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
00586      EJECT
00587 *    COPY ELCATTR.
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
00588      EJECT
00589 *    COPY ELCEMIB.
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
00590      EJECT
00591 *    COPY ELCJPFX.
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
00592          PIC X(512).
00593      EJECT
00594 *    COPY ELCAID.
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
00595
00596  01  FILLER    REDEFINES DFHAID.
00597      12  FILLER              PIC X(8).
00598      12  PF-VALUES           PIC X       OCCURS 12.
00599
00600      EJECT
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
00602
00603  01  DFHCOMMAREA             PIC X(1024).
00604
00605 *01 PARMLIST .
00606 *    02  FILLER              PIC S9(8)   COMP.
00607 *    02  ELMSTR-POINTER      PIC S9(8)   COMP.
00608 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.
00609 *    02  ELCERT-POINTER      PIC S9(8)   COMP.
00610 *    02  ERACCT-POINTER      PIC S9(8)   COMP.
00611 *    02  ELTRLR-POINTER      PIC S9(8)   COMP.
00612 *    02  ELBENE-POINTER      PIC S9(8)   COMP.
00613 *    02  ELACTQ-POINTER      PIC S9(8)   COMP.
00614 *    02  ELARCH-POINTER      PIC S9(8)   COMP.
00615 *    02  ERREIN-POINTER      PIC S9(8)   COMP.
00616      EJECT
00617 *    COPY ELCMSTR.
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
00618      EJECT
00619 *    COPY ELCCNTL.
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
00620      EJECT
00621 *    COPY ELCCERT.
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
00622      EJECT
00623 *    COPY ERCACCT.
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
00624      EJECT
00625 *    COPY ELCTRLR.
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
00626      EJECT
00627 *    COPY ELCBENE.
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
00628      EJECT
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
00629 *    COPY ELCACTQ.
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
00630      EJECT
00631 *    COPY ELCARCH.
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
00632      EJECT
00633 *    COPY ERCREIN.
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
00634      EJECT
050807*    COPY ERCMAIL.
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
050807     EJECT
041309*    COPY ELCCRTT.
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
041309     EJECT
00634
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                CONTROL-FILE CERTIFICATE-MASTER
                                ACCOUNT-MASTER ACTIVITY-TRAILERS
                                BENEFICIARY-MASTER
                                DAILY-ACTIVITY-RECORD ACTIVITY-QUE
                                LETTER-ARCHIVE REINSURANCE-RECORD
                                MAILING-DATA CERTIFICATE-TRAILERS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL130' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00636      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00637      MOVE '5'                    TO DC-OPTION-CODE.
00638      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00639      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00640      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00641
00642      IF EIBCALEN = ZERO
00643          GO TO 8800-UNAUTHORIZED-ACCESS
041002     END-IF.
00644
00645      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00646      MOVE EIBTRMID               TO QID-TERM.
00647      MOVE +2                     TO EMI-NUMBER-OF-LINES
052113     move spaces                 to emi-error-lines
00648      MOVE '2'                    TO EMI-SWITCH2.
00649      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
00650      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
00651
           
      * EXEC CICS HANDLE CONDITION
00653 *        QIDERR     (0100-TEST-ENTRY)
00654 *        MAPFAIL    (8100-SEND-INITIAL-MAP)
00655 *        PGMIDERR   (9600-PGMID-ERROR)
00656 *        TERMIDERR  (8820-TERM-ERROR)
00657 *        TRANSIDERR (8830-TRAN-ERROR)
00658 *        ERROR      (9990-ABEND)
00659 *    END-EXEC.
      *    MOVE '"$N?L[\.              ! " #00007296' TO DFHEIV0
           MOVE X'22244E3F4C5B5C2E20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303037323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00660
00661      IF PI-RETURN-TO-PROGRAM = THIS-PGM  OR
00662         PI-CALLING-PROGRAM   = XCTL-6592
00663          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM
00664      ELSE
00665          MOVE SPACES             TO RETURNED-FROM
041002     END-IF.
00666
00667      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00668          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00669              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00670              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00671              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00672              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00673              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00674              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00675              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00676              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00677          ELSE
00678              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00679              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00680              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00681              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00682              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00683              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00684              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00685              MOVE SPACES               TO PI-SAVED-PROGRAM-6
041002         END-IF
041002     END-IF.
00686
00687      IF EIBTRNID = TRANS-ID
00688          IF EIBAID = DFHCLEAR
00689              GO TO 9400-CLEAR
00690          ELSE
00691              GO TO 0200-RECEIVE
041002         END-IF
041002     END-IF.
00692
00693      IF RETURNED-FROM NOT = SPACES
00694          GO TO 0600-RECOVER-TEMP-STORAGE
041002     END-IF.
00695
00696      MOVE LOW-VALUES TO EL130AO.
052113     move zeros to pi-dcc-max-amt
052113                   pi-dcc-max-benefits
00697
00698
           
      * EXEC CICS DELETEQ TS
00699 *         QUEUE   (QID)
00700 *    END-EXEC.
      *    MOVE '*&                    #   #00007351' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00701
00702  0100-TEST-ENTRY.
00703      IF PI-RETURN-TO-PROGRAM = XCTL-127 OR XCTL-727
00704          GO TO 0650-FROM-EL127
00705      ELSE
00706          MOVE ZEROS              TO PI-CERT-SELECT-CNT
00707                                     PI-CERT-PROCESSED
00708                                     PI-LETTER-ERROR-CODE
041002     END-IF.
00709
00710      IF PI-RETURN-TO-PROGRAM = XCTL-132
00711          GO TO 0640-FROM-EL132
041002     END-IF.
00712
00713      GO TO 8100-SEND-INITIAL-MAP.
00714
00715      EJECT
00716  0200-RECEIVE.
00717      MOVE LOW-VALUES             TO EL130AI.
00718
00719      IF PI-PROCESSOR-ID = 'LGXX'
041002*        NEXT SENTENCE
041002         CONTINUE
00721      ELSE
               
      * EXEC CICS READQ TS
00723 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00724 *            INTO    (SECURITY-CONTROL)
00725 *            LENGTH  (SC-COMM-LENGTH)
00726 *            ITEM    (SC-ITEM)
00727 *        END-EXEC
      *    MOVE '*$II   L              ''   #00007378' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00728          MOVE SC-CLAIMS-DISPLAY (1)   TO  PI-DISPLAY-CAP
00729          MOVE SC-CLAIMS-UPDATE  (1)   TO  PI-MODIFY-CAP
00730          IF NOT DISPLAY-CAP
00731              MOVE 'READ'              TO  SM-READ
00732              PERFORM 9995-SECURITY-VIOLATION
00733              MOVE ER-0070             TO  EMI-ERROR
00734              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00735              GO TO 8100-SEND-INITIAL-MAP
041002         END-IF
041002     END-IF.
00736
00737      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00738          MOVE ER-7008 TO EMI-ERROR
00739          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00740          MOVE -1 TO MAINTL
00741          GO TO 8200-SEND-DATAONLY
041002     END-IF.
00742
00743
           
      * EXEC CICS RECEIVE
00744 *         MAP     (MAP-NAME)
00745 *         MAPSET  (MAPSET-NAME)
00746 *         INTO    (EL130AI)
00747 *    END-EXEC.
           MOVE LENGTH OF
            EL130AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00007403' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL130AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00748
00749      IF NOT PI-NO-CARRIER-SECURITY
00750         MOVE +1                  TO CLMCARRL
00751         MOVE PI-CARRIER-SECURITY TO CLMCARRI
041002     END-IF.
00752
00753      IF ENTERPFL = ZERO
00754          GO TO 0300-CHECK-PFKEYS
041002     END-IF.
00755
00756      IF EIBAID NOT = DFHENTER
00757          MOVE ER-0004            TO EMI-ERROR
00758          GO TO 0320-INPUT-ERROR
041002     END-IF.
00759
00760      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)
00761          MOVE PF-VALUES (ENTERPFI) TO EIBAID
00762      ELSE
00763          MOVE ER-0029            TO EMI-ERROR
00764          GO TO 0320-INPUT-ERROR
041002     END-IF.
00765
00766  0300-CHECK-PFKEYS.
00767      IF EIBAID = DFHPF23
00768          GO TO 8810-PF23
041002     END-IF.
00769
00770      IF EIBAID = DFHPF24
00771          GO TO 9200-RETURN-MAIN-MENU
041002     END-IF.
00772
00773      IF EIBAID = DFHPF12
00774          GO TO 9500-PF12
041002     END-IF.
00775
00776      IF EIBAID = DFHPF3
00777          MOVE SPACES              TO MAINTI
00778          MOVE ZEROS               TO MAINTL
00779          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
00780          MOVE XCTL-127            TO PGM-NAME
00781          GO TO 9300-XCTL
041002     END-IF.
00782
00783      IF EIBAID = DFHPF4
00784          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
00785          MOVE XCTL-132            TO PGM-NAME
00786          GO TO 9300-XCTL
041002     END-IF.
00787
00788      IF EIBAID = DFHPF13
00789          IF PI-ACCESS-TO-BOTH-SYSTEMS
00790              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
00791              MOVE PI-CARRIER          TO  PI-CR-CARRIER
00792              MOVE PI-GROUPING         TO  PI-CR-GROUPING
00793              MOVE PI-STATE            TO  PI-CR-STATE
00794              MOVE PI-ACCOUNT          TO  PI-CR-ACCOUNT
00795              MOVE XCTL-650            TO  PGM-NAME
00796              GO TO 9300-XCTL
00797          ELSE
00798              MOVE -1                  TO  ENTERPFL
00799              MOVE ER-2566             TO  EMI-ERROR
00800              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00801              GO TO 8100-SEND-INITIAL-MAP
041002         END-IF
041002     END-IF.
00802
00803      IF EIBAID = DFHPF5
00804          IF PI-CERT-PROCESSED LESS THAN PI-CERT-SELECT-CNT
00805              ADD +1 TO PI-CERT-PROCESSED
00806              GO TO 0650-FROM-EL127
00807          ELSE
00808              MOVE ER-7686         TO EMI-ERROR
00809              GO TO 0320-INPUT-ERROR
041002         END-IF
041002     END-IF.
00810
00811      IF EIBAID = DFHPF7 OR DFHPF8 OR DFHPF9
00812          GO TO 0800-TEST-PREVIOUS-CONTROL
041002     END-IF.
00813
00814      IF PI-HAS-CLAS-IC-CRDTCRD
00815          IF EIBAID = DFHPF10
00816              MOVE XCTL-725          TO PGM-NAME
00817              GO TO 9300-XCTL
041002         END-IF
041002     END-IF.
00818
00819      IF EIBAID = DFHPF14
00820          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
00821          MOVE SPACES              TO PI-EL659-TO-EL130-CNTRL
00822          MOVE XCTL-659            TO PGM-NAME
00823          GO TO 9300-XCTL
041002     END-IF.
00824
PEMMOD     IF EIBAID = DFHPF15
PEMMOD         PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
PEMMOD         MOVE XCTL-114            TO PGM-NAME
PEMMOD         GO TO 9300-XCTL
041002     END-IF.
PEMMOD
052113*    IF EIBAID = DFHPF11
052113*       GO TO 0600-RECOVER-TEMP-STORAGE
052113*    END-IF
00825      IF EIBAID = DFHENTER OR DFHPF6 OR DFHPF11
00826          GO TO 0330-EDIT-DATA
041002     END-IF.
00827
00828      MOVE ER-0029                TO EMI-ERROR.
00829
00830  0320-INPUT-ERROR.
00831      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00832
00833      MOVE AL-UNBON               TO ENTERPFA.
00834
00835      IF ENTERPFL = ZERO
00836          MOVE -1                 TO MAINTL
00837      ELSE
00838          MOVE -1                 TO ENTERPFL
041002     END-IF.
00839
00840      GO TO 8200-SEND-DATAONLY.
00841
00842      EJECT
00843  0330-EDIT-DATA.
00844
00845      IF MAINTI = 'S'
00846          IF PRTOPTL GREATER THAN 0
00847              IF PRTOPTI NOT = 'N' AND 'L'
00848                  MOVE AL-UABON   TO  PRTOPTA
00849                  MOVE -1         TO  PRTOPTL
00850                  MOVE ER-0334    TO  EMI-ERROR
00851                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00852                  GO TO 8200-SEND-DATAONLY
00853              ELSE
00854                  IF PRTOPTI = 'L'
00855                      GO TO 0400-CREATE-ELACTQ
00856                  ELSE
00857                      GO TO 0480-PRINT-NOW
041002                 END-IF
041002             END-IF
00858          ELSE
00859              GO TO 1000-SHOW-CLAIM
041002         END-IF
041002     END-IF.
00860
00861      IF NOT MODIFY-CAP
00862          MOVE 'UPDATE'       TO  SM-READ
00863          PERFORM 9995-SECURITY-VIOLATION
00864          MOVE ER-0070        TO  EMI-ERROR
00865          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00866          GO TO 8100-SEND-INITIAL-MAP
041002     END-IF.
00867
00868      IF MAINTI = 'D'
00869          GO TO 2000-DELETE-CLAIM
041002     END-IF.
00870
00871      IF MAINTI = 'A'
00872          GO TO 3000-ADD-CLAIM
041002     END-IF.
00873
00874      IF MAINTI = 'I'
00875          GO TO 5000-INCURRED-CHANGE
041002     END-IF.
00876
00877      MOVE ER-0023                TO EMI-ERROR.
00878      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00879      MOVE -1                     TO MAINTL.
00880      MOVE AL-UABON               TO MAINTA.
00881
00882      GO TO 8200-SEND-DATAONLY.
00883
00884      EJECT
00885  0400-CREATE-ELACTQ.
00886      IF CLMNOI   = PI-LAST-CLAIM      AND
00887         CLMCARRI = PI-LAST-CARR       AND
00888         CERTNOI  = PI-LAST-CERT-PRIME AND
00889         SUFXI    = PI-LAST-CERT-SUFX
00890          MOVE PI-LAST-CLAIM      TO PI-CLAIM-NO
00891          MOVE PI-LAST-CARR       TO PI-CARRIER
00892          MOVE PI-LAST-CERT       TO PI-CERT-NO
00893      ELSE
00894          MOVE ER-0207            TO EMI-ERROR
00895          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00896          MOVE -1                 TO MAINTL
00897          GO TO 8200-SEND-DATAONLY
041002     END-IF.
00898
00899      IF (NOT PI-NO-CARRIER-SECURITY OR
00900          NOT PI-NO-ACCOUNT-SECURITY)
00901          MOVE AL-UABON      TO  PRTOPTA
00902          MOVE -1            TO  PRTOPTL
00903          MOVE ER-2378       TO  EMI-ERROR
00904          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00905          GO TO 8200-SEND-DATAONLY
041002     END-IF.
00906
00907      MOVE PI-COMPANY-CD      TO ELACTQ-COMPANY-CD.
00908      MOVE PI-LAST-CARR       TO ELACTQ-CARRIER.
00909      MOVE PI-LAST-CLAIM      TO ELACTQ-CLAIM-NO.
00910      MOVE PI-LAST-CERT       TO ELACTQ-CERT-NO.
00911
00912
           
      * EXEC CICS HANDLE CONDITION
00913 *         NOTFND     (0450-ADD-ELACTQ-RECORD)
00914 *    END-EXEC.
      *    MOVE '"$I                   ! # #00007611' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303037363131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00915
00916
           
      * EXEC CICS READ
00917 *         UPDATE
00918 *         DATASET    (ELACTQ-DSID)
00919 *         SET        (ADDRESS OF ACTIVITY-QUE)
00920 *         RIDFLD     (ELACTQ-KEY)
00921 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007616' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00922
00923      MOVE '2'         TO AQ-PENDING-STATUS-FLAG.
00924      MOVE +130        TO AQ-LAST-UPDATED-BY.
00925
00926
           
      * EXEC CICS REWRITE
00927 *         DATASET     (ELACTQ-DSID)
00928 *         FROM        (ACTIVITY-QUE)
00929 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007627' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-DSID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00930
00931      GO TO 0470-STATUS-FINISH.
00932
00933  0450-ADD-ELACTQ-RECORD.
00934
           
      * EXEC CICS GETMAIN
00935 *        SET     (ADDRESS OF ACTIVITY-QUE)
00936 *        LENGTH  (ELACTQ-LENGTH)
00937 *        INITIMG (GETMAIN-SPACE)
00938 *    END-EXEC.
      *    MOVE ',"IL                  $   #00007636' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELACTQ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00939
00940      MOVE 'AQ'              TO AQ-RECORD-ID.
00941      MOVE ELACTQ-KEY        TO AQ-CONTROL-PRIMARY.
00942      MOVE +0                TO AQ-PAYMENT-COUNTER
00943                                AQ-PMT-UNAPPROVED-COUNT.
00944      MOVE LOW-VALUES        TO AQ-RESEND-DATE
00945                                AQ-FOLLOWUP-DATE.
00946      MOVE '2'               TO AQ-PENDING-STATUS-FLAG.
00947      MOVE +130              TO AQ-LAST-UPDATED-BY.
00948
00949
           
      * EXEC CICS WRITE
00950 *         DATASET     (ELACTQ-DSID)
00951 *         FROM        (ACTIVITY-QUE)
00952 *         RIDFLD      (ELACTQ-KEY)
00953 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007652' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-DSID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00954
00955  0470-STATUS-FINISH.
00956      MOVE AL-UANOF               TO  PRTOPTA
00957                                      ALTPRTA.
00958
00959      MOVE SPACES                 TO  PRTOPTO
00960                                      ALTPRTO.
00961
00962      MOVE ER-0000                TO EMI-ERROR.
00963      MOVE -1                     TO MAINTL.
00964      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00965      GO TO 8100-SEND-INITIAL-MAP.
00966
00967  0480-PRINT-NOW.
00968
           
      * EXEC CICS HANDLE CONDITION
00969 *        TERMIDERR   (8820-TERM-ERROR)
00970 *        TRANSIDERR  (8830-TRAN-ERROR)
00971 *    END-EXEC.
      *    MOVE '"$[\                  ! $ #00007672' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303037363732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00972
00973      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
00974      MOVE '1'                    TO  CNTL-REC-TYPE.
00975      MOVE SPACES                 TO  CNTL-ACCESS.
00976      MOVE +0                     TO  CNTL-SEQ-NO.
00977      MOVE 'CNTL'                 TO  FILE-SWITCH.
00978
00979      PERFORM 7970-READ-CNTL THRU 7970-EXIT.
00980
00981      IF CF-FORMS-PRINTER-ID = SPACES
00982          IF ALTPRTL NOT GREATER THAN 0
00983              MOVE ER-0337        TO  EMI-ERROR
00984              MOVE -1             TO  ALTPRTL
00985              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00986              GO TO 8200-SEND-DATAONLY
041002         END-IF
           END-IF.
00987
00988      IF ALTPRTL GREATER THAN 0
00989          MOVE ALTPRTI            TO  CF-FORMS-PRINTER-ID
041002     END-IF.
00990
00991      MOVE PI-LAST-CLAIM          TO  PI-CLAIM-NO.
00992      MOVE PI-LAST-CARR           TO  PI-CARRIER.
00993      MOVE PI-LAST-CERT           TO  PI-CERT-NO.
00994
00995
           
      * EXEC CICS START
00996 *        TRANSID   (START-TRANS-ID)
00997 *        TERMID    (CF-FORMS-PRINTER-ID)
00998 *        FROM      (PROGRAM-INTERFACE-BLOCK)
00999 *        LENGTH    (PI-COMM-LENGTH)
01000 *    END-EXEC.
      *    MOVE '0( LFT                1   #00007703' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 CF-FORMS-PRINTER-ID, 
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
           
01001
01002      MOVE AL-UANOF               TO  PRTOPTA
01003                                      ALTPRTA.
01004
01005      MOVE SPACES                 TO  PRTOPTO
01006                                      ALTPRTO.
01007
01008      MOVE ER-0000                TO  EMI-ERROR.
01009      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01010      MOVE -1                     TO  MAINTL.
01011      GO TO 8100-SEND-INITIAL-MAP.
01012
01013      EJECT
01014  0500-CREATE-TEMP-STORAGE.
01015      MOVE EIBCPOSN               TO PI-CURSOR.
01016
052113    move emi-line1               to errmsg1o
052113    move emi-line2               to errmsg2o
052113    move emi-sub                 to pi-emi-sub
01017
           
      * EXEC CICS WRITEQ TS
01018 *        QUEUE   (QID)
01019 *        FROM    (EL130AI)
01020 *        LENGTH  (QID-MAP-LENGTH)
01021 *    END-EXEC.
      *    MOVE '*"     L              ''   #00007729' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL130AI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01022
01023
           
      * EXEC CICS WRITEQ TS
01024 *        QUEUE    (QID)
01025 *        FROM     (PROGRAM-INTERFACE-BLOCK)
01026 *        LENGTH   (PI-COMM-LENGTH)
01027 *    END-EXEC.
      *    MOVE '*"     L              ''   #00007736' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01028
01029  0599-EXIT.
01030       EXIT.
01031
01032      EJECT
01033  0600-RECOVER-TEMP-STORAGE.
01034      MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.
01035      MOVE PI-EL127-TO-EL130-CNTRL
01036                                  TO SAVE-EL127-TO-EL130-CNTRL.
01037      MOVE PI-EL659-TO-EL130-CNTRL
01038                                  TO SAVE-EL659-TO-EL130-CNTRL.
01039      MOVE PI-COMPANY-ID          TO SAVE-COMPANY-ID.
01040      MOVE PI-COMPANY-CD          TO SAVE-COMPANY-CD.
01041      MOVE PI-JOURNAL-FILE-ID     TO SAVE-JOURNAL-FILE-ID.
01042      MOVE PI-CREDIT-USER         TO SAVE-CREDIT-USER.
01043      MOVE PI-CLAIM-USER          TO SAVE-CLAIM-USER.
01044      MOVE PI-LIFE-OVERRIDE-L1    TO SAVE-LIFE-OVERRIDE-L1.
01045      MOVE PI-LIFE-OVERRIDE-L2    TO SAVE-LIFE-OVERRIDE-L2.
01046      MOVE PI-LIFE-OVERRIDE-L6    TO SAVE-LIFE-OVERRIDE-L6.
01047      MOVE PI-LIFE-OVERRIDE-L12   TO SAVE-LIFE-OVERRIDE-L12.
01048      MOVE PI-AH-OVERRIDE-L1      TO SAVE-AH-OVERRIDE-L1.
01049      MOVE PI-AH-OVERRIDE-L2      TO SAVE-AH-OVERRIDE-L2.
01050      MOVE PI-AH-OVERRIDE-L6      TO SAVE-AH-OVERRIDE-L6.
01051      MOVE PI-AH-OVERRIDE-L12     TO SAVE-AH-OVERRIDE-L12.
01052      MOVE PI-CERT-ACCESS-CONTROL TO SAVE-CERT-ACCESS-CONTROL.
01053      MOVE PI-CARRIER-CONTROL-LEVEL  TO SAVE-CARRIER-CONTROL-LEVEL.
PEMMOD     MOVE PI-PROGRAM-WORK-AREA (1:10)
PEMMOD                                 TO SAVE-BENEFICIARY
01054
01055
           
      * EXEC CICS HANDLE CONDITION
01056 *        NOTFND  (0660-NOT-FOUND)
01057 *        QIDERR  (0670-QIDERR)
01058 *    END-EXEC.
      *    MOVE '"$IN                  ! % #00007771' TO DFHEIV0
           MOVE X'2224494E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303037373731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01059
01060
           
      * EXEC CICS READQ TS
01061 *        QUEUE    (QID)
01062 *        INTO     (EL130AI)
01063 *        LENGTH   (QID-MAP-LENGTH)
01064 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00007777' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL130AI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01065
01066      IF RETURNED-FROM = XCTL-131
01067         IF CLAIM-DELETED-BY-EL131
01068             MOVE LOW-VALUES      TO CERTNOI
01069                                     SUFXI
01070             MOVE ZEROS           TO CERTNOL
01071                                     SUFXL
041002         END-IF
041002     END-IF.
01072
01073      IF RETURNED-FROM = XCTL-131
01074          MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA
01075          MOVE ZEROS              TO PI-CERT-SELECT-CNT
01076                                     PI-CERT-PROCESSED
01077      ELSE
01078
               
      * EXEC CICS READQ TS
01079 *             QUEUE   (QID)
01080 *             INTO    (PROGRAM-INTERFACE-BLOCK)
01081 *             LENGTH  (PI-COMM-LENGTH)
01082 *        END-EXEC
      *    MOVE '*$I    L              ''   #00007798' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
041002     END-IF.
01084
           
      * EXEC CICS DELETEQ TS
01085 *        QUEUE   (QID)
01086 *    END-EXEC.
      *    MOVE '*&                    #   #00007805' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01087
01088      MOVE SAVE-CONTROL           TO PI-CONTROL-IN-PROGRESS.
01089
01090      IF RETURNED-FROM = XCTL-127 OR XCTL-132
01091          MOVE SAVE-COMPANY-CD        TO PI-COMPANY-CD
01092          MOVE SAVE-COMPANY-ID        TO PI-COMPANY-ID
01093          MOVE SAVE-JOURNAL-FILE-ID   TO PI-JOURNAL-FILE-ID
01094          MOVE SAVE-CREDIT-USER       TO PI-CREDIT-USER
01095          MOVE SAVE-CLAIM-USER        TO PI-CLAIM-USER
01096          MOVE SAVE-LIFE-OVERRIDE-L1  TO PI-LIFE-OVERRIDE-L1
01097          MOVE SAVE-LIFE-OVERRIDE-L2  TO PI-LIFE-OVERRIDE-L2
01098          MOVE SAVE-LIFE-OVERRIDE-L6  TO PI-LIFE-OVERRIDE-L6
01099          MOVE SAVE-LIFE-OVERRIDE-L12 TO PI-LIFE-OVERRIDE-L12
01100          MOVE SAVE-AH-OVERRIDE-L1    TO PI-AH-OVERRIDE-L1
01101          MOVE SAVE-AH-OVERRIDE-L2    TO PI-AH-OVERRIDE-L2
01102          MOVE SAVE-AH-OVERRIDE-L6    TO PI-AH-OVERRIDE-L6
01103          MOVE SAVE-AH-OVERRIDE-L12   TO PI-AH-OVERRIDE-L12
01104          MOVE SAVE-CERT-ACCESS-CONTROL
01105                                     TO PI-CERT-ACCESS-CONTROL
01106          MOVE SAVE-CARRIER-CONTROL-LEVEL
01107                                     TO PI-CARRIER-CONTROL-LEVEL
041002     END-IF.
01108
052113*    IF EIBAID = DFHPF11
052113*       move errmsg1i            to emi-line1
052113*       move errmsg2i            to emi-line2
052113*       move pi-emi-sub          to emi-sub
052113*       go to 8200-SEND-DATAONLY
052113*    end-if
01109      IF RETURNED-FROM = XCTL-127
01110          MOVE SAVE-EL127-TO-EL130-CNTRL
01111                                  TO PI-EL127-TO-EL130-CNTRL
041002     END-IF.
01112
01113      IF RETURNED-FROM = XCTL-6592
01114         MOVE SAVE-EL659-TO-EL130-CNTRL
01115                                  TO PI-EL659-TO-EL130-CNTRL
041002     END-IF.
01116
PEMMOD     IF RETURNED-FROM = XCTL-114
PEMMOD        IF SAVE-BENEFICIARY NOT = SPACES AND LOW-VALUES
PEMMOD           MOVE SAVE-BENEFICIARY TO BENECDI
PEMMOD           MOVE +10              TO BENECDL
PEMMOD           MOVE -1               TO BIRTHDTL
PEMMOD        END-IF
041002     END-IF.
01117      PERFORM 3993-MODIFY-SCREEN-ATTRB THRU 3993-EXIT.
01118
01119      IF MAINTL NOT = ZERO
01120          MOVE AL-UANON           TO MAINTA
041002     END-IF.
01121
01122      IF ENTERPFL NOT = ZERO
01123          MOVE AL-UNNON           TO ENTERPFA
041002     END-IF.
01124
01125      IF RETURNED-FROM = XCTL-127
01126          GO TO 0650-FROM-EL127
041002     END-IF.
01127
01128      IF RETURNED-FROM = XCTL-132
01129          GO TO 0640-FROM-EL132
041002     END-IF.
01130
01131      IF RETURNED-FROM = XCTL-650
01132          MOVE AL-SABON           TO PF5A
01133          PERFORM 0690-HIGHLIGHT-CERTS THRU 0690-EXIT
041002     END-IF.
01134
01135      IF RETURNED-FROM = XCTL-6592
01136          GO TO 0630-FROM-EL6592
041002     END-IF.
01137
01138      IF MAINTI = 'S' OR 'I'
01139         GO TO 1000-SHOW-CLAIM
041002     END-IF.
01140
01141      GO TO 8100-SEND-INITIAL-MAP.
01142
01143      EJECT
01144  0630-FROM-EL6592.
01145      MOVE PI-EL659-CARRIER       TO CRTCARRO.
01146      MOVE PI-EL659-GROUPING      TO GROUPO.
01147      MOVE PI-EL659-STATE         TO STATEO.
01148      MOVE PI-EL659-ACCOUNT       TO ACCOUNTO.
01149
01150      MOVE AL-UANON               TO CRTCARRA
01151                                     GROUPA
01152                                     STATEA
01153                                     ACCOUNTA.
01154      GO TO 8100-SEND-INITIAL-MAP.
01155
01156  0640-FROM-EL132.
01157      IF PI-CLAIM-NO = SPACES
01158          GO TO 0645-NO-CLAIM-SELECTED
041002     END-IF.
01159
01160      MOVE LOW-VALUES             TO EL130AI.
01161      MOVE 'MSTR'                 TO FILE-SWITCH.
01162      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
01163      MOVE PI-CARRIER             TO MSTR-CARRIER.
01164      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO
01165                                     CLMNOO.
01166      MOVE AL-UANON               TO CLMNOA.
01167      MOVE PI-CERT-NO             TO MSTR-CERT-NO.
01168
01169
           
      * EXEC CICS HANDLE CONDITION
01170 *        NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
01171 *    END-EXEC.
      *    MOVE '"$I                   ! & #00007915' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303037393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01172
01173      PERFORM 7950-READ-CLAIM THRU 7950-EXIT.
01174      MOVE 'X'                    TO CLAIM-SWITCH.
01175      PERFORM 7910-READ-NINETY THRU 7910-EXIT.
01176      MOVE AT-INFO-LINE-1         TO WS-DIAGNOSIS-DESCRIPT.
040814     MOVE AT-ICD-CODE-1          TO WS-ICD-CODE-1.
040814     MOVE AT-ICD-CODE-2          TO WS-ICD-CODE-2.
01177      PERFORM 7910-READ-ACTV THRU 7910-EXIT.
01178      PERFORM 7000-BUILD-OUTPUT-MAP THRU 7099-EXIT.
01179
01180      MOVE ER-0213                TO EMI-ERROR.
01181      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01182      MOVE PI-CLAIM-NO            TO EMI-TEXT-VARIABLE (1).
01183      GO TO 8100-SEND-INITIAL-MAP.
01184
01185  0645-NO-CLAIM-SELECTED.
01186
01187      IF MAINTI = 'S' OR 'I'
01188         GO TO 1000-SHOW-CLAIM
041002     END-IF.
01189
01190      GO TO 8100-SEND-INITIAL-MAP.
01191
01192  0650-FROM-EL127.
01193
01194      IF PI-CERT-SELECT-CNT GREATER THAN +6
01195          MOVE ZEROS              TO PI-CERT-SELECT-CNT
01196                                     PI-CERT-PROCESSED
041002     END-IF.
01197
01198      IF PI-RETURN-TO-PROGRAM = XCTL-727
01199          MOVE +1                 TO PI-CERT-SELECT-CNT
01200                                     PI-CERT-PROCESSED
01201          MOVE PI-CARRIER         TO PI-EL127-CARRIER (1)
01202          MOVE PI-GROUPING        TO PI-EL127-GROUPING (1)
01203          MOVE PI-STATE           TO PI-EL127-STATE   (1)
01204          MOVE PI-ACCOUNT         TO PI-EL127-ACCOUNT (1)
01205          MOVE PI-CERT-EFF-DT     TO PI-EL127-EFF-DT  (1)
01206          MOVE PI-CERT-NO         TO PI-EL127-CERT-NO (1)
041002     END-IF.
01207
01208      IF PI-CERT-SELECT-CNT = ZERO
01209          IF CERTNOI NOT = SPACES AND LOW-VALUES
01210              MOVE CERTNOI        TO PI-CERT-NO
01211              MOVE SUFXI          TO PI-CERT-SFX
01212              GO TO 3000-READ-CERT
01213          ELSE
01214              GO TO 8100-SEND-INITIAL-MAP
041002         END-IF
041002     END-IF.
01215
01216      MOVE AL-SABON               TO PF5A.
01217
01218      IF PI-CERT-PROCESSED = 1
01219          PERFORM 0680-PLUG-SELECTED-CERTS THRU 0680-EXIT
041002     END-IF.
01220
01221      PERFORM 0690-HIGHLIGHT-CERTS    THRU 0690-EXIT.
01222      PERFORM 3993-MODIFY-CLAIM-ATTRB THRU 3993-EXIT.
01223
01224      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
01225      MOVE PI-EL127-CARRIER (PI-CERT-PROCESSED)
01226                                  TO CERT-CARRIER
01227                                     PI-CARRIER.
01228      MOVE PI-EL127-GROUPING(PI-CERT-PROCESSED)
01229                                  TO CERT-GROUPING
01230                                     PI-GROUPING.
01231      MOVE PI-EL127-STATE   (PI-CERT-PROCESSED)
01232                                  TO CERT-STATE
01233                                     PI-STATE.
01234      MOVE PI-EL127-ACCOUNT (PI-CERT-PROCESSED)
01235                                  TO CERT-ACCOUNT
01236                                     PI-ACCOUNT.
01237      MOVE PI-EL127-EFF-DT  (PI-CERT-PROCESSED)
01238                                  TO CERT-EFF-DT
01239                                     PI-CERT-EFF-DT.
01240      MOVE PI-EL127-CERT-NO (PI-CERT-PROCESSED)
01241                                  TO CERT-CERT-NO
01242                                     PI-CERT-NO.
01243
01244      MOVE 'CERT'                 TO FILE-SWITCH.
01245      PERFORM 7940-READ-CERT THRU 7940-EXIT.
01246
01247      MOVE CERT-CERT-NO           TO PI-SAVE-CERT.
01248      MOVE CERT-ACCOUNT           TO PI-SAVE-ACCOUNT.
01249      MOVE CERT-EFF-DT            TO PI-SAVE-EFFDT.
01250
01251      IF CM-CLAIM-ATTACHED-COUNT NOT = ZERO
01252          MOVE PI-COMPANY-CD      TO MSTR5-COMP-CD
01253          MOVE CERT-CERT-NO       TO MSTR5-CERT-NO
01254          PERFORM 7500-BROWSE-FOR-DUPLICATE THRU 7500-EXIT
01255          IF INCURRED-DATE-MATCH
01256              MOVE ER-7352        TO EMI-ERROR
01257              MOVE -1             TO CERTNOL
01258              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01259          ELSE
01260              IF NO-CLAIMS-FOR-CERT
01261                  MOVE ER-7353    TO EMI-ERROR
01262                  MOVE -1         TO CERTNOL
01263                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01264              ELSE
01265                  MOVE ER-0558    TO EMI-ERROR
01266                  MOVE -1         TO CERTNOL
01267                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002             END-IF
041002         END-IF
041002     END-IF.
01268
01269      MOVE SPACES                 TO PI-PRT-OPT
01270                                     PI-ALT-PRT.
01271
01272      PERFORM 7000-BUILD-OUTPUT-MAP THRU 7099-EXIT.
01273
           if (ws-lf-joint-indicator not = 'J')
              and (ws-ah-joint-indicator not = 'J')
              move 'P'                 to INSTYPEO
              MOVE AL-UANON            TO INSTYPEA
           end-if
01274      IF LSTNMEL NOT GREATER ZERO
01275         AND CRTLNMEI NOT = (SPACES AND LOW-VALUES)
01276          MOVE CRTLNMEI           TO  LSTNMEO
01277          MOVE CRTFNMEI           TO  FSTNMEO
01278          MOVE CRTINITI           TO  INITO
01279          MOVE AL-UANON           TO  LSTNMEA
01280                                      FSTNMEA
01281                                      INITA
041002     END-IF.
01282
01283      IF SSNL GREATER ZERO
01284          GO TO 0655-SKIP-SOCIAL-SECURITY
041002     END-IF.
01285
01286      IF CRTSSNI = SPACES OR LOW-VALUES
01287          GO TO 0655-SKIP-SOCIAL-SECURITY
041002     END-IF.
01288
01289      IF CM-SSN-STATE   = CM-STATE AND
01290         CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME
01291             GO TO 0655-SKIP-SOCIAL-SECURITY
041002     END-IF.
01292
01293      MOVE CRTSSNI                TO SSNO
01294      MOVE AL-UANON               TO SSNA.
01295
01296  0655-SKIP-SOCIAL-SECURITY.
01297
01298      IF CLMCARRL NOT = ZERO  AND
01299         CLMCARRI NOT = CRTCARRI
01300         MOVE ER-0562             TO EMI-ERROR
01301         MOVE AL-UABON            TO CLMCARRA
01302         MOVE -1                  TO CLMCARRL
01303         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
01304
01305      IF CLMCARRL = ZERO
01306         MOVE AL-UANON            TO CLMCARRA
01307         MOVE CRTCARRI            TO CLMCARRI
041002     END-IF.
01308
01309      MOVE 'A'                    TO MAINTO.
01310      MOVE AL-UANON               TO MAINTA.
01311
01312      IF PI-RETURN-TO-PROGRAM = XCTL-127 OR XCTL-727
01313         GO TO 8100-SEND-INITIAL-MAP
01314      ELSE
01315         GO TO 8150-SEND-MAP-CURSOR
041002     END-IF.
01316
01317  0660-NOT-FOUND.
01318      MOVE ER-0214                TO EMI-ERROR.
01319      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01320      GO TO 8100-SEND-INITIAL-MAP.
01321
01322  0670-QIDERR.
01323      MOVE ER-0033                TO EMI-ERROR.
01324      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01325      GO TO 8100-SEND-INITIAL-MAP.
01326
01327      EJECT
01328  0680-PLUG-SELECTED-CERTS.
01329
01330      MOVE AL-SANON               TO BCERT1A
01331                                     BSUFX1A
01332                                     BCERT2A
01333                                     BSUFX2A
01334                                     BCERT3A
01335                                     BSUFX3A
01336                                     BCERT4A
01337                                     BSUFX4A
01338                                     BCERT5A
01339                                     BSUFX5A.
01340
01341      IF PCERTNOO = SPACES OR LOW-VALUES
01342          MOVE PI-EL127-CERT-PRIME (1)
01343                                  TO PCERTNOO
01344          MOVE PI-EL127-CERT-SUFX  (1)
01345                                  TO PSUFXO
041002     END-IF.
01346
01347      MOVE PI-EL127-CERT-PRIME (1)
01348                                  TO BCERT1O.
01349      MOVE PI-EL127-CERT-SUFX  (1)
01350                                  TO BSUFX1O.
01351
01352      IF PI-CERT-SELECT-CNT GREATER THAN 1
01353          MOVE PI-EL127-CERT-PRIME (2)
01354                                  TO BCERT2O
01355          MOVE PI-EL127-CERT-SUFX  (2)
01356                                  TO BSUFX2O
01357      ELSE
01358          MOVE LOW-VALUES         TO BCERT2O
01359                                     BSUFX2O
041002     END-IF.
01360
01361      IF PI-CERT-SELECT-CNT GREATER THAN 2
01362          MOVE PI-EL127-CERT-PRIME (3)
01363                                  TO BCERT3O
01364          MOVE PI-EL127-CERT-SUFX  (3)
01365                                  TO BSUFX3O
01366      ELSE
01367          MOVE LOW-VALUES         TO BCERT3O
01368                                     BSUFX3O
041002     END-IF.
01369
01370      IF PI-CERT-SELECT-CNT GREATER THAN 3
01371          MOVE PI-EL127-CERT-PRIME (4)
01372                                  TO BCERT4O
01373          MOVE PI-EL127-CERT-SUFX  (4)
01374                                  TO BSUFX4O
01375      ELSE
01376          MOVE LOW-VALUES         TO BCERT4O
01377                                     BSUFX4O
041002     END-IF.
01378
01379      IF PI-CERT-SELECT-CNT GREATER THAN 4
01380          MOVE PI-EL127-CERT-PRIME (5)
01381                                  TO BCERT5O
01382          MOVE PI-EL127-CERT-SUFX  (5)
01383                                  TO BSUFX5O
01384      ELSE
01385          MOVE LOW-VALUES         TO BCERT5O
01386                                     BSUFX5O
041002     END-IF.
01387
01388  0680-EXIT.
01389      EXIT.
01390
01391      EJECT
01392  0690-HIGHLIGHT-CERTS.
01393
01394      MOVE AL-UANON               TO PCERTNOA
01395                                     PSUFXA.
01396
01397      IF BCERT1O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
01398         BSUFX1O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
01399             MOVE AL-SABON        TO BCERT1A
01400                                     BSUFX1A
01401      ELSE
01402          MOVE AL-SANON           TO BCERT1A
01403                                     BSUFX1A
041002     END-IF.
01404
01405      IF BCERT2O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
01406         BSUFX2O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
01407             MOVE AL-SABON        TO BCERT2A
01408                                     BSUFX2A
01409      ELSE
01410          MOVE AL-SANON           TO BCERT2A
01411                                     BSUFX2A
041002     END-IF.
01412
01413      IF BCERT3O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
01414         BSUFX3O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
01415             MOVE AL-SABON        TO BCERT3A
01416                                     BSUFX3A
01417      ELSE
01418          MOVE AL-SANON           TO BCERT3A
01419                                     BSUFX3A
041002     END-IF.
01420
01421      IF BCERT4O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
01422         BSUFX4O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
01423             MOVE AL-SABON        TO BCERT4A
01424                                     BSUFX4A
01425      ELSE
01426          MOVE AL-SANON           TO BCERT4A
01427                                     BSUFX4A
041002     END-IF.
01428
01429      IF BCERT5O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
01430         BSUFX5O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
01431             MOVE AL-SABON        TO BCERT5A
01432                                     BSUFX5A
01433      ELSE
01434          MOVE AL-SANON           TO BCERT5A
01435                                     BSUFX5A
041002     END-IF.
01436
01437  0690-EXIT.
01438      EXIT.
01439
01440      EJECT
01441  0800-TEST-PREVIOUS-CONTROL.
01442      IF CLMNOI   = PI-LAST-CLAIM       AND
01443         CLMCARRI = PI-LAST-CARR        AND
01444         CERTNOI  = PI-LAST-CERT-PRIME  AND
01445         SUFXI    = PI-LAST-CERT-SUFX
041002*        NEXT SENTENCE
041002         CONTINUE
01447      ELSE
01448          MOVE ER-0207            TO EMI-ERROR
01449          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01450          MOVE -1                 TO MAINTL
01451          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01452
01453      PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT.
01454
01455      MOVE PI-LAST-CLAIM          TO PI-CLAIM-NO.
01456      MOVE PI-LAST-CARR           TO PI-CARRIER.
01457      MOVE PI-LAST-CERT           TO PI-CERT-NO.
01458
01459      IF EIBAID = DFHPF7
01460          MOVE XCTL-157           TO PGM-NAME
041002     END-IF.
01461
01462      IF EIBAID = DFHPF8
01463          MOVE XCTL-141           TO PGM-NAME
041002     END-IF.
01464
01465      IF EIBAID = DFHPF9
01466         MOVE SPACES              TO PI-SAVED-PROGRAM-1
01467                                     PI-SAVED-PROGRAM-2
01468                                     PI-SAVED-PROGRAM-3
01469                                     PI-SAVED-PROGRAM-4
01470                                     PI-SAVED-PROGRAM-5
01471                                     PI-SAVED-PROGRAM-6
01472         MOVE XCTL-126            TO PI-RETURN-TO-PROGRAM
01473         MOVE XCTL-132            TO PI-CALLING-PROGRAM
01474         MOVE XCTL-150            TO PGM-NAME
041002     END-IF.
01475
01476      GO TO 9300-XCTL.
01477
01478      EJECT
01479  1000-SHOW-CLAIM.
01480
01481      IF CLMNOL = ZERO
01482          MOVE -1                 TO CLMNOL
01483          MOVE AL-UABON           TO CLMNOA
01484          MOVE ER-0202            TO EMI-ERROR
01485          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01486      ELSE
01487          MOVE AL-UANON           TO CLMNOA
041002     END-IF.
01488
01489      IF PI-NO-CARRIER-SECURITY
01490         IF CLMCARRL = ZERO
01491              MOVE -1             TO CLMCARRL
01492              MOVE AL-UABON       TO CLMCARRA
01493              MOVE ER-0194        TO EMI-ERROR
01494              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01495          ELSE
01496              MOVE AL-UANON       TO CLMCARRA
041002         END-IF
041002     END-IF.
01497
01498      IF SUFXL = ZERO
01499          MOVE SPACES             TO SUFXI
01500          MOVE AL-UANON           TO SUFXA
01501      ELSE
01502          MOVE AL-UANON           TO SUFXA
041002     END-IF.
01503
01504      IF NOT EMI-NO-ERRORS
01505          GO TO 1050-SEND-CHECK
041002     END-IF.
01506
01507
           
      * EXEC CICS HANDLE CONDITION
01508 *        NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
01509 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00008297' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303038323937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01510
01511      MOVE 'MSTR'                 TO FILE-SWITCH
01512      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
01513
01514      MOVE CLMCARRI               TO MSTR-CARRIER.
01515      MOVE CLMNOI                 TO MSTR-CLAIM-NO.
01516
01517      IF CERTNOL = ZERO     OR
01518         CERTNOI = SPACES   OR
01519         CERTNOI = LOW-VALUES
01520          PERFORM 7600-BROWSE-CLAIM THRU 7699-EXIT
PEMMOD         MOVE AL-SANON        TO CERTNOA
PEMMOD                                 SUFXA
01523      ELSE
01524          PERFORM 7610-BROWSE-CLAIM-LOOP THRU 7699-EXIT
041002     END-IF.
01525
01526      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
01527      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
01528
01529
           
      * EXEC CICS HANDLE CONDITION
01530 *        NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
01531 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00008321' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303038333231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01532
01533      PERFORM 7950-READ-CLAIM THRU 7950-EXIT.
01534
01535      IF NOT PI-NO-ACCOUNT-SECURITY
01536          IF PI-ACCOUNT-SECURITY NOT = CL-CERT-ACCOUNT
01537            MOVE ER-2371          TO EMI-ERROR
01538            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01539            MOVE -1               TO MAINTL
01540            GO TO 8200-SEND-DATAONLY
041002         END-IF
041002     END-IF.
01541
01542
           
      * EXEC CICS HANDLE CONDITION
01543 *        NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
01544 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00008337' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303038333337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01545
01546      MOVE 'Y'                    TO CLAIM-SWITCH.
01547      PERFORM 7910-READ-NINETY THRU 7910-EXIT.
01548      MOVE AT-INFO-LINE-1         TO WS-DIAGNOSIS-DESCRIPT.
040814     MOVE AT-ICD-CODE-1          TO WS-ICD-CODE-1.
040814     MOVE AT-ICD-CODE-2          TO WS-ICD-CODE-2.
01549      PERFORM 7910-READ-ACTV THRU 7910-EXIT.
01550
01551  1000-SHOW-END-OF-PERFORM.
01552
01553      MOVE MSTR-COMP-CD           TO CERT-COMP-CD.
01554      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
01555      MOVE CL-CERT-GROUPING       TO CERT-GROUPING.
01556      MOVE CL-CERT-STATE          TO CERT-STATE.
01557      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
01558      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
01559      MOVE CL-CERT-NO             TO CERT-CERT-NO.
01560
01561      MOVE 'CERT'                 TO FILE-SWITCH.
01562
01563      PERFORM 7940-READ-CERT THRU 7940-EXIT.
01564
01565      MOVE CL-LAST-MAINT-DT       TO PI-MSTR-DT.
01566      MOVE CL-LAST-MAINT-HHMMSS   TO PI-MSTR-WHEN.
01567      MOVE AT-RECORDED-DT         TO PI-TRLR-DT.
01568      MOVE AT-LAST-MAINT-HHMMSS   TO PI-TRLR-WHEN.
01569      MOVE CLMNOI                 TO PI-LAST-CLAIM.
01570      MOVE CERTNOI                TO PI-LAST-CERT-PRIME.
01571      MOVE SUFXI                  TO PI-LAST-CERT-SUFX.
01572      MOVE CLMCARRI               TO PI-LAST-CARR.
01573
01574      PERFORM 7000-BUILD-OUTPUT-MAP THRU 7099-EXIT.
01575
01576      IF MAINTI = 'I'
01577          MOVE AL-SANOF TO  FSTNMEA   INITA     SEXA
01578                  BIRTHDTA  LSTNMEA   RELCLMA
01579                  LOANNOA   LOANBALA  BENECDA
01580          MOVE AL-SANON TO  SSNA
040814         MOVE SPACES   TO  INCURI    REPORTI
01582          MOVE 'Y'      TO PI-INCURR-SW
01583          MOVE ER-0522  TO EMI-ERROR
01584          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01585      ELSE
01586          MOVE ' '      TO PI-INCURR-SW
041002     END-IF.
01587
01588      MOVE -1                     TO MAINTL.
01589      MOVE MSTR-CARRIER           TO PI-CARRIER.
01590      MOVE CERT-GROUPING          TO PI-GROUPING.
01591      MOVE CERT-STATE             TO PI-STATE.
01592      MOVE CERT-ACCOUNT           TO PI-ACCOUNT.
01593      MOVE MSTR-CLAIM-NO          TO PI-CLAIM-NO.
01594      MOVE MSTR-CERT-NO           TO PI-CERT-NO.
01595      MOVE CERT-EFF-DT            TO PI-CERT-EFF-DT.
01596
01597      IF RETURNED-FROM NOT = SPACES
01598          GO TO 8100-SEND-INITIAL-MAP
01599      ELSE
01600          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01601
01602  1025-SHOW-RECORD-NOT-FOUND.
01603
01604      IF FILE-SWITCH = 'MSTR'
01605          MOVE ER-0204            TO EMI-ERROR
01606      ELSE
01607          IF FILE-SWITCH = 'TRLR'
01608              MOVE ER-0205        TO EMI-ERROR
01609          ELSE
01610              MOVE ER-0206        TO EMI-ERROR
041002         END-IF
041002     END-IF.
01611
01612      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01613
01614  1050-SEND-CHECK.
01615
01616      IF RETURNED-FROM NOT = SPACES
01617          GO TO 8100-SEND-INITIAL-MAP
01618      ELSE
01619          MOVE -1                 TO MAINTL
01620          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01621
01623  2000-DELETE-CLAIM.
01624      IF NOT MODIFY-CAP
01625          MOVE ER-0070            TO EMI-ERROR
01626          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01627          MOVE -1                 TO MAINTL
01628          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01629
01630      IF CLMNOI   NOT = PI-LAST-CLAIM       OR
01631         CLMCARRI NOT = PI-LAST-CARR        OR
01632         CERTNOI  NOT = PI-LAST-CERT-PRIME  OR
01633         SUFXI    NOT = PI-LAST-CERT-SUFX
01634          MOVE ER-0207            TO EMI-ERROR
01635          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01636          MOVE -1                 TO MAINTL
01637          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01638
01639
           
      * EXEC CICS HANDLE CONDITION
01640 *        NOTFND   (2050-DELETE-NOT-FOUND)
01641 *    END-EXEC.
      *    MOVE '"$I                   ! * #00008443' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303038343433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01642
01643      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
01644
01645      MOVE CLMCARRI               TO MSTR-CARRIER.
01646      MOVE CLMNOI                 TO MSTR-CLAIM-NO.
01647      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
01648      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
01649
01650      MOVE 'MSTR'                 TO FILE-SWITCH.
01651      PERFORM 7920-READ-CLAIM-UPDATE THRU 7920-EXIT.
01652
01653      MOVE 'Y'                    TO CLAIM-SWITCH.
01654
01655      IF PI-MSTR-DT   NOT = CL-LAST-MAINT-DT  OR
01656         PI-MSTR-WHEN NOT = CL-LAST-MAINT-HHMMSS
01657          MOVE ER-0068        TO EMI-ERROR
01658          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01659          MOVE -1             TO MAINTL
01660          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01661
01662      IF CL-TRAILER-SEQ-CNT NOT = 4095
01663          MOVE ER-0569            TO EMI-ERROR
01664          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01665          MOVE -1                 TO MAINTL
01666          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01667
01668      IF CL-CLAIM-PAYMENT-STATUS NOT NUMERIC
01669          MOVE ZEROS              TO CL-CLAIM-PAYMENT-STATUS
041002     END-IF.
01670
01671      IF CL-PURGED-DT NOT = LOW-VALUES
01672          MOVE ER-7691            TO EMI-ERROR
01673          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01674          MOVE -1                 TO MAINTL
01675          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01676
01677      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
01678      MOVE MSTR-CARRIER           TO TRLR-CARRIER.
01679      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
01680      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
01681      MOVE +0                     TO TRLR-SEQ-NO.
01682      MOVE '1'                    TO TRLR-TYPE.
01683
01684      MOVE 'TRLR'                 TO FILE-SWITCH.
01685
01686
           
      * EXEC CICS READ
01687 *        UPDATE
01688 *        DATASET  (ELTRLR-DSID)
01689 *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
01690 *        RIDFLD   (ELTRLR-KEY)
01691 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008495' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01692
01693      IF PI-TRLR-DT   NOT = AT-RECORDED-DT  OR
01694         PI-TRLR-WHEN NOT = AT-LAST-MAINT-HHMMSS
01695          MOVE ER-0068        TO EMI-ERROR
01696          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01697          MOVE -1             TO MAINTL
01698          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01699
01700
           
      * EXEC CICS DELETE
01701 *        DATASET   (ELTRLR-DSID)
01702 *    END-EXEC.
      *    MOVE '&(                    &   #00008511' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01703
01704      MOVE +90                    TO TRLR-SEQ-NO.
01705      MOVE '6'                    TO TRLR-TYPE.
01706
01707
           
      * EXEC CICS READ
01708 *        UPDATE
01709 *        DATASET  (ELTRLR-DSID)
01710 *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
01711 *        RIDFLD   (ELTRLR-KEY)
01712 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008519' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01713
01714
           
      * EXEC CICS DELETE
01715 *        DATASET   (ELTRLR-DSID)
01716 *    END-EXEC.
      *    MOVE '&(                    &   #00008527' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01717
CIDMOD     MOVE +91                    TO TRLR-SEQ-NO.
CIDMOD     MOVE '6'                    TO TRLR-TYPE.
CIDMOD
CIDMOD
           
      * EXEC CICS READ
CIDMOD*        UPDATE
CIDMOD*        DATASET  (ELTRLR-DSID)
CIDMOD*        SET      (ADDRESS OF ACTIVITY-TRAILERS)
CIDMOD*        RIDFLD   (ELTRLR-KEY)
CIDMOD*        RESP     (WS-RESPONSE)
CIDMOD*    END-EXEC
      *    MOVE '&"S        EU         (  N#00008535' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
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
CIDMOD     IF WS-RESP-NORMAL
              
      * EXEC CICS DELETE
CIDMOD*          DATASET   (ELTRLR-DSID)
CIDMOD*       END-EXEC
      *    MOVE '&(                    &   #00008544' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
041002     END-IF.
01717
01718      MOVE MSTR-COMP-CD           TO CERT-COMP-CD.
01719      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
01720      MOVE CL-CERT-GROUPING       TO CERT-GROUPING.
01721      MOVE CL-CERT-STATE          TO CERT-STATE.
01722      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
01723      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
01724      MOVE CL-CERT-NO             TO CERT-CERT-NO.
01725
01726      MOVE 'CERT'                 TO FILE-SWITCH.
01727
01728      PERFORM 7930-READ-CERT-UPDATE THRU 7930-EXIT.
01729
01730      SUBTRACT +1 FROM CM-CLAIM-ATTACHED-COUNT GIVING WS-NUM-HOLD.
01731
01732      IF WS-NUM-HOLD NOT = +0
01733          GO TO 2005-REWRITE-CERT
041002     END-IF.
01734
01735      IF NOT CERT-WAS-CREATED
01736          MOVE SPACE              TO CM-CLAIM-INTERFACE-SW
01737          GO TO 2005-REWRITE-CERT
041002     END-IF.
01738
01739
           
      * EXEC CICS DELETE
01740 *        DATASET   (ELCERT-DSID)
01741 *    END-EXEC.
      *    MOVE '&(                    &   #00008573' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01742
01743      GO TO 2010-DELETE-CL.
01744
01745  2005-REWRITE-CERT.
01746
01747      SUBTRACT +1 FROM CM-CLAIM-ATTACHED-COUNT.
01748
01749
           
      * EXEC CICS HANDLE CONDITION
01750 *         DUPKEY   (2010-DELETE-CL)
01751 *    END-EXEC.
      *    MOVE '"$$                   ! + #00008584' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303038353834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01752
01753
           
      * EXEC CICS REWRITE
01754 *        DATASET  (ELCERT-DSID)
01755 *        FROM     (CERTIFICATE-MASTER)
01756 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008589' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01757
01759  2010-DELETE-CL.
01760
01761
           
      * EXEC CICS DELETE
01762 *        DATASET  (ELMSTR-DSID)
01763 *    END-EXEC.
      *    MOVE '&(                    &   #00008597' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01764
01765      MOVE -1                     TO ONE-OR-MIN1.
01766      PERFORM 7700-CHECK-SEQUENCE THRU 7799-EXIT.
01767
01768      IF WS-ASSOC-CERT-TOTAL NOT = ZERO
01769          MOVE CL-CONTROL-PRIMARY TO ELMSTR-KEY
01770                                     WS-SAVE-CLAIM-KEY
01771          MOVE +1                 TO ONE-OR-MIN1
01772          PERFORM 7710-RESEQUENCE-CLAIMS THRU 7799-EXIT
041002     END-IF.
01773
01774      MOVE ER-0000                TO EMI-ERROR.
01775      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01776      MOVE LOW-VALUES             TO EL130AO.
01777      MOVE -1                     TO MAINTL.
01778      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.
01779      MOVE ZEROS                  TO PI-CERT-SELECT-CNT
01780                                     PI-CERT-PROCESSED
01781                                     PI-LETTER-ERROR-CODE.
01782      GO TO 8100-SEND-INITIAL-MAP.
01783
01784  2050-DELETE-NOT-FOUND.
01785      IF FILE-SWITCH = 'MSTR'
01786          MOVE -1                 TO CLMNOL
01787          MOVE ER-0204            TO EMI-ERROR
01788      ELSE
01789          IF FILE-SWITCH = 'TRLR'
01790              MOVE -1             TO MAINTL
01791              MOVE ER-0205        TO EMI-ERROR
01792          ELSE
01793              MOVE -1             TO CERTNOL
01794              MOVE ER-0206        TO EMI-ERROR
041002         END-IF
041002     END-IF.
01795
01796      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01797      GO TO 8200-SEND-DATAONLY.
01798
01800  3000-ADD-CLAIM.
01801      IF NOT MODIFY-CAP
01802          MOVE ER-0070            TO EMI-ERROR
01803          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01804          MOVE -1                 TO MAINTL
01805          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01806
01807      PERFORM 6000-EDIT-CLAIM-DATA THRU 6000-EXIT.
01808
01809      IF CERTMTI = 'S'
01810          IF CERTNOI  = PI-SAVE-CERT-PRIME  AND
01811             SUFXI    = PI-SAVE-CERT-SUFX   AND
01812             WS-EFFDT = PI-SAVE-EFFDT       AND
01813             ACCOUNTI = PI-SAVE-ACCOUNT
01814              IF EMI-ERROR = 0
01815                  GO TO 3010-TEST-FOR-ERRORS
01816              ELSE
01817                  MOVE PI-COMPANY-CD    TO  CERT-COMP-CD
01818                  MOVE PI-CARRIER       TO  CERT-CARRIER
01819                  MOVE PI-GROUPING      TO  CERT-GROUPING
01820                  MOVE PI-STATE         TO  CERT-STATE
01821                  MOVE PI-ACCOUNT       TO  CERT-ACCOUNT
01822                  MOVE PI-CERT-EFF-DT   TO  CERT-EFF-DT
01823                  MOVE PI-CERT-NO       TO  CERT-CERT-NO
01824
                       
      * EXEC CICS HANDLE CONDITION
01825 *                    NOTFND   (3010-TEST-FOR-ERRORS)
01826 *                END-EXEC
      *    MOVE '"$I                   ! , #00008664' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303038363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01827                  PERFORM 7940-READ-CERT THRU 7940-EXIT
01828                  PERFORM 7050-BUILD-MAP-CERT-DATA THRU 7099-EXIT
01829                  GO TO 3010-TEST-FOR-ERRORS
041002             END-IF
041002         END-IF
041002     END-IF.
01830
01831      IF CERTMTI = 'A'
01832          IF NOT PI-NO-CARRIER-SECURITY OR
01833              NOT PI-NO-ACCOUNT-SECURITY
01834              MOVE ER-2370          TO EMI-ERROR
01835              MOVE -1               TO MAINTL
01836              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01837              GO TO 8200-SEND-DATAONLY
01838          ELSE
01839              PERFORM 6300-REQUIRED-CERT-EDIT THRU 6399-EXIT
01840              GO TO 3010-TEST-FOR-ERRORS
041002         END-IF
041002     END-IF.
01841
01842      IF CERTNOL = ZEROS
01843         GO TO 3010-TEST-FOR-ERRORS
041002     END-IF.
01844
01845  3000-READ-CERT.
01846
01847      MOVE PI-COMPANY-CD          TO CERT-COMP-CD-5
01848                                     MSTR5-COMP-CD.
01849      MOVE CERTNOI                TO CERT-CERT-5-PRIME
01850                                     MSTR5-CERT-NO-PRIME.
01851      MOVE SUFXI                  TO CERT-CERT-5-SUFX
01852                                     MSTR5-CERT-NO-SUFX.
01853
01854
           
      * EXEC CICS HANDLE CONDITION
01855 *         DUPKEY   (3008-DUP-ERROR)
01856 *         NOTFND   (3009-NOT-FOUND)
01857 *    END-EXEC
      *    MOVE '"$$I                  ! - #00008701' TO DFHEIV0
           MOVE X'222424492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303038373031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01858
01859      PERFORM 7980-READ-CERT5 THRU 7980-EXIT.
01860
01861      IF NOT PI-NO-ACCOUNT-SECURITY
01862          IF PI-ACCOUNT-SECURITY NOT = CM-ACCOUNT
01863              MOVE ER-2371          TO EMI-ERROR
01864              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01865              MOVE -1               TO MAINTL
01866              GO TO 8200-SEND-DATAONLY
041002         END-IF
041002     END-IF.
01867
01868      MOVE CM-CERT-NO             TO PI-SAVE-CERT.
01869      MOVE CM-ACCOUNT             TO PI-SAVE-ACCOUNT.
01870      MOVE CM-CERT-EFF-DT         TO PI-SAVE-EFFDT.
01871      MOVE CM-COMPANY-CD          TO PI-COMPANY-CD.
01872      MOVE CM-CARRIER             TO PI-CARRIER.
01873      MOVE CM-GROUPING            TO PI-GROUPING.
01874      MOVE CM-STATE               TO PI-STATE.
01875      MOVE CM-ACCOUNT             TO PI-ACCOUNT.
01876      MOVE CM-CERT-EFF-DT         TO PI-CERT-EFF-DT.
01877      MOVE CM-CERT-NO             TO PI-CERT-NO.
01878      PERFORM 7050-BUILD-MAP-CERT-DATA THRU 7099-EXIT.
01879
01880      IF CRTLNMEI NOT = SPACES AND LOW-VALUES
01881          IF LSTNMEO = SPACES OR LOW-VALUES
01882              MOVE CRTLNMEI       TO  LSTNMEO
01883              MOVE AL-UANON       TO  LSTNMEA
041002         END-IF
041002     END-IF.
01884
01885      MOVE 'S'                    TO CERTMTO.
PEMMOD*    MOVE AL-UANON               TO CERTMTA  CERTNOA  CLMCARRA
PEMMOD     MOVE AL-SANON               TO CERTNOA.
PEMMOD     MOVE AL-UANON               TO CERTMTA   CLMCARRA
01887                                  EFFDTA ACCOUNTA STATEA   GROUPA.
01888      IF EMI-NO-ERRORS
01889          MOVE -1                  TO MAINTL
041002     END-IF.
01890
01891      IF CM-CLAIM-ATTACHED-COUNT NOT = ZERO
01892          PERFORM 7500-BROWSE-FOR-DUPLICATE THRU 7500-EXIT
01893          IF INCURRED-DATE-MATCH
01894              MOVE ER-7352         TO EMI-ERROR
01895              MOVE -1              TO CERTNOL
01896              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01897          ELSE
01898              IF NO-CLAIMS-FOR-CERT
01899                  MOVE ER-7353     TO EMI-ERROR
01900                  MOVE -1          TO CERTNOL
01901                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01902              ELSE
01903                  MOVE ER-0558     TO EMI-ERROR
01904                  MOVE -1          TO CERTNOL
01905                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01906                  GO TO 3010-TEST-FOR-ERRORS
041002             END-IF
041002         END-IF
041002     END-IF.
01907
01908  3007-SEND-SCREEN.
01909      IF RETURNED-FROM = XCTL-132 OR XCTL-127
01910          GO TO 8150-SEND-MAP-CURSOR
01911      ELSE
01912          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01913
01914  3008-DUP-ERROR.
01915      MOVE ER-0560                TO EMI-ERROR.
01916      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01917      MOVE -1                     TO CERTNOL.
01918      GO TO 3007-SEND-SCREEN.
01919
01920  3009-NOT-FOUND.
01921      MOVE ER-0214                TO EMI-ERROR.
01922      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01923      MOVE -1                     TO CERTNOL.
01924      GO TO 3007-SEND-SCREEN.
01925
01927  3010-TEST-FOR-ERRORS.
01928
01929      PERFORM 6200-EDIT-CERT-DATA           THRU 6200-EXIT.
070714     perform 6500-get-acct    thru 6500-exit
01930      PERFORM 6400-TEST-CLAIM-REASONABILITY THRU 6499-EXIT.
070714*     perform 6500-get-acct    thru 6500-exit
061013
061013     MOVE ZEROS                  TO WS-MONTHS-BETWEEN
061013     move +0                     to e1
061013     move spaces                 to ws-dcc-error-line
           move ' '                    to ws-zero-bens-avail
061013
           if ws-dcc-product-code not = spaces
061013        PERFORM 3997-GET-ERPDEF  THRU 3997-EXIT
061013        IF ERPDEF-FOUND
061013           MOVE CM-CERT-EFF-DT   TO DC-BIN-DATE-1
061013           MOVE WS-INCUR         TO DC-BIN-DATE-2
061013           MOVE '1'              TO DC-OPTION-CODE
061013           MOVE +0               TO DC-ELAPSED-MONTHS
061013                                    DC-ELAPSED-DAYS
061013           PERFORM 9700-LINK-DATE-CONVERT
061013                                 THRU 9700-EXIT
061013           IF NO-CONVERSION-ERROR
061013              MOVE DC-ELAPSED-MONTHS
061013                                 TO WS-MONTHS-BETWEEN
022122              IF DC-odd-days-over > 1
061013                 ADD 1 TO WS-MONTHS-BETWEEN
061013              END-IF
061013           ELSE
061013              MOVE ZEROS         TO WS-MONTHS-BETWEEN
061013           END-IF
061013
061013           evaluate true
061013              when (ws-excl-period not = zeros)
061013                 and (ws-months-between <= ws-excl-period)
061013                 MOVE -1         TO MAINTL
061013                 add +1          to e1
                       move 'Y'        to ws-zero-bens-avail
061013                 MOVE ER-1651    TO EMI-ERROR
061013                                    ws-error-no (e1)
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013              when (ws-cov-ends not = zeros)
061013                 and (ws-months-between > ws-cov-ends)
061013                 MOVE -1         TO MAINTL
061013                 add +1          to e1
                       move 'Y'        to ws-zero-bens-avail
061013                 MOVE ER-1653    TO EMI-ERROR
061013                                    ws-error-no (e1)
061013                 evaluate true
061013                    when clmtypei = 'L'
061013                       move '  LF  ' to emi-claim-type
061013                    when clmtypei = 'I'
061013                       move '  IU  ' to emi-claim-type
061013                    when clmtypei = 'F'
061013                       move '  FL  ' to emi-claim-type
022122                    when clmtypei = 'H'
022122                       move '  HS  ' to emi-claim-type
022122                    when clmtypei = 'B'
022122                       move '  BR  ' to emi-claim-type
061013                    when other
061013                       move '  AH  ' to emi-claim-type
061013                 end-evaluate
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013              when (ws-acc-period not = zeros)
061013                 and (ws-months-between <= ws-acc-period)
061013                 MOVE -1         TO MAINTL
061013                 add +1          to e1
061013                 MOVE ER-1652    TO EMI-ERROR
061013                                    ws-error-no (e1)
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013                 add +1          to e1
061013                 move er-1655    to emi-error
061013                                    ws-error-no (e1)
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013              when clmtypei = 'I'
061013                 add +1          to e1
061013                 move er-1661    to emi-error
061013                                    ws-error-no (e1)
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013           end-evaluate
011118           IF (WS-PRE-EXISTING-PER NOT = ZEROS)
011118              AND (WS-MONTHS-BETWEEN <= WS-PRE-EXISTING-PER)
011118              MOVE -1         TO MAINTL
011118              ADD +1          TO E1
011118              MOVE ER-1677    TO EMI-ERROR
011118                                 WS-ERROR-NO (E1)
011118              PERFORM 9900-ERROR-FORMAT
011118                              THRU 9900-EXIT
011118           END-IF
061013        END-IF
061013*       perform 3996-read-cert-claim-trailer
061013*                                thru 3996-exit
061013     END-IF
           perform 3996-read-cert-claim-trailer
                                       thru 3996-exit
01932      IF CERTMTI NOT = 'A'
01933          IF CM-CLAIM-ATTACHED-COUNT NOT = ZERO
01934              MOVE PI-COMPANY-CD       TO MSTR5-COMP-CD
01935              MOVE CERTNOI             TO MSTR5-CERT-NO-PRIME
01936              MOVE SUFXI               TO MSTR5-CERT-NO-SUFX
01937              PERFORM 7500-BROWSE-FOR-DUPLICATE THRU 7500-EXIT
01938              IF INCURRED-DATE-MATCH
01939                  MOVE ER-7352         TO EMI-ERROR
01940                  MOVE -1              TO CERTNOL
01941                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01942              ELSE
01943                  IF NO-CLAIMS-FOR-CERT
01944                      MOVE ER-7353     TO EMI-ERROR
01945                      MOVE -1          TO CERTNOL
01946                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01947                  ELSE
01948                      MOVE ER-0558     TO EMI-ERROR
01949                      MOVE -1          TO CERTNOL
01950                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002                 END-IF
041002             END-IF
041002         END-IF
041002     END-IF.
01951
01952      IF EIBAID = DFHPF6  AND NOT FORCE-CAP
01953          MOVE ER-0433             TO EMI-ERROR
01954          MOVE -1                  TO MAINTL
01955          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01956          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01957
01958      IF (EMI-NO-ERRORS)
061013        OR (ZEROS = EMI-FATAL-CTR AND EMI-FORCABLE-CTR)
01959          GO TO 3015-TRY-TO-BUILD
041002     END-IF.
01960
01961      IF EIBAID = DFHPF6
01962          IF EMI-FATAL-CTR NOT EQUAL ZERO
01963              MOVE ER-0434         TO EMI-ERROR
01964              MOVE -1              TO MAINTL
01965              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01966              GO TO 8200-SEND-DATAONLY
01967          ELSE
01968              MOVE +1              TO INCURL
01969              GO TO 3015-TRY-TO-BUILD
041002         END-IF
041002     END-IF.
01970
01971      IF RETURNED-FROM = XCTL-132 OR XCTL-127
01972          GO TO 8150-SEND-MAP-CURSOR
01973      ELSE
01974          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01975
01976  3015-TRY-TO-BUILD.
01977      IF CLMNOL EQUAL ZERO
01978          GO TO 3030-BUILD-CONT
041002     END-IF.
01979
01980
           
      * EXEC CICS HANDLE CONDITION
01981 *        NOTFND   (3030-BUILD-CONT)
01982 *    END-EXEC.
      *    MOVE '"$I                   ! . #00008944' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303038393434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01983
01984      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
01985
01986      MOVE CLMCARRI               TO MSTR-CARRIER.
01987      MOVE CLMNOI                 TO MSTR-CLAIM-NO.
01988      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
01989      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
01990
01991      MOVE 'MSTR'                 TO FILE-SWITCH.
01992
01993      PERFORM 7950-READ-CLAIM THRU 7950-EXIT.
01994
01995      MOVE ER-0221                TO EMI-ERROR.
01996
01997      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01998      MOVE -1                     TO MAINTL.
01999      GO TO 8200-SEND-DATAONLY.
02000
02001  3030-BUILD-CONT.
02002      IF (CERTMTI NOT = 'A')
020816        and (pi-company-id not = 'DCC' and 'VPP')
02006          GO TO 3060-BUILD-CONT
041002     END-IF.
02007
02008
           
      * EXEC CICS HANDLE CONDITION
02009 *        NOTFND   (3033-ACCT-NOT-FOUND)
02010 *        ENDFILE  (3033-ACCT-NOT-FOUND)
02011 *    END-EXEC.
      *    MOVE '"$I''                  ! / #00008972' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303038393732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02012
02013      MOVE SPACES                 TO ERACCT-KEY.
02014
02015      IF CARR-GROUP-ST-ACCNT-CNTL
02016          MOVE PI-COMPANY-CD      TO ACCT-COMP-CD
02017          MOVE CRTCARRI           TO ACCT-CARRIER
02018          MOVE GROUPI             TO ACCT-GROUPING
02019          MOVE STATEI             TO ACCT-STATE
02020          MOVE ACCOUNTI           TO ACCT-ACCOUNT
02021          GO TO 3031-MOVE-ACCT-KEY
041002     END-IF.
02022
02023      MOVE PI-COMPANY-CD          TO ACCT-COMP-CD.
02024      MOVE ACCOUNTI               TO ACCT-ACCOUNT.
02025
02026      IF PI-CERT-ACCESS-CONTROL = ' ' OR '2'
02027          MOVE STATEI             TO ACCT-STATE
041002     END-IF.
02028
02029      IF PI-CERT-ACCESS-CONTROL = '2' OR '4'
02030          MOVE CRTCARRI           TO ACCT-CARRIER
041002     END-IF.
02031
02032  3031-MOVE-ACCT-KEY.
02033      MOVE ERACCT-KEY             TO SAVE-ERACCT-KEY.
02034
02035
           
      * EXEC CICS STARTBR
02036 *        DATASET     (ERACCT2-DSID)
02037 *        RIDFLD      (SAVE-ERACCT-KEY)
02038 *        GENERIC
02039 *        KEYLENGTH   (20)
02040 *    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00009003' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 SAVE-ERACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02041
02042
           
      * EXEC CICS HANDLE CONDITION
02043 *        ENDFILE   (3032-NOT-IN-DT-RANGE)
02044 *    END-EXEC.
      *    MOVE '"$''                   ! 0 #00009011' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303039303131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02045
02046  3031-READ-ACCT-LOOP.
02047
           
      * EXEC CICS READNEXT
02048 *        DATASET   (ERACCT2-DSID)
02049 *        SET       (ADDRESS OF ACCOUNT-MASTER)
02050 *        RIDFLD    (SAVE-ERACCT-KEY)
02051 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009017' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 SAVE-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02052
02053      IF ACCT-COMP-CD  NOT = AM-COMPANY-CD-A1  OR
02054         ACCT-CARRIER  NOT = AM-VG-CARRIER     OR
02055         ACCT-GROUPING NOT = AM-VG-GROUPING    OR
02056         ACCT-STATE    NOT = AM-VG-STATE       OR
02057         ACCT-ACCOUNT  NOT = AM-VG-ACCOUNT
02058          GO TO 3033-ACCT-NOT-FOUND
041002     END-IF.
02059
02060      IF WS-EFFDT NOT LESS AM-EFFECTIVE-DT  AND
02061         WS-EFFDT LESS AM-EXPIRATION-DT
02062          MOVE PI-COMPANY-CD    TO CERT-COMP-CD
02063          MOVE AM-CARRIER       TO CERT-CARRIER
02064          MOVE AM-GROUPING      TO CERT-GROUPING
02065          MOVE AM-STATE         TO CERT-STATE
02066          MOVE AM-ACCOUNT       TO CERT-ACCOUNT
02067          MOVE WS-EFFDT         TO CERT-EFF-DT
02068          MOVE CERTNOI          TO CERT-CERT-NO-PRIME
02069          MOVE SUFXI            TO CERT-CERT-NO-SUFX
02070          MOVE AM-REI-TABLE     TO WS-REIN-TABLE
02071          GO TO 3034-CHECK-EMPLOYER-STATEMENT
041002     END-IF.
02072
02073      IF WS-EFFDT NOT LESS AM-EXPIRATION-DT
02074          GO TO 3031-READ-ACCT-LOOP
041002     END-IF.
02075
02076  3032-NOT-IN-DT-RANGE.
02077      MOVE ER-0226                TO EMI-ERROR.
02078      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02079      MOVE -1                     TO EFFDTL.
02080      GO TO 8200-SEND-DATAONLY.
02081
02082  3033-ACCT-NOT-FOUND.
02083      MOVE ER-0226 TO EMI-ERROR.
02084      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02085      MOVE -1                     TO ACCOUNTL.
02086      GO TO 8200-SEND-DATAONLY.
02087
02088  3034-CHECK-EMPLOYER-STATEMENT.
02089
02090      IF AM-EMPLOYER-STMT-USED = '1' OR '2' OR '3' OR 'Y'
041002*        NEXT SENTENCE
041002         CONTINUE
02092      ELSE
02093          GO TO  3034-CHECK-ACCOUNT-LIMITS
041002     END-IF.
02094
02095      MOVE CERT-EFF-DT            TO  DC-BIN-DATE-1.
02096      MOVE WS-INCUR               TO  DC-BIN-DATE-2.
02097      MOVE '1'                    TO  DC-OPTION-CODE.
02098      MOVE +0                     TO  DC-ELAPSED-MONTHS
02099                                      DC-ELAPSED-DAYS.
02100      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02101
02102      IF (AM-EMPLOYER-STMT-USED = '1' AND
02103          DC-ELAPSED-DAYS LESS THAN +31)
02104                       OR
02105         (AM-EMPLOYER-STMT-USED = '2' AND
02106          DC-ELAPSED-DAYS LESS THAN +61)
02107                       OR
02108         (AM-EMPLOYER-STMT-USED = '3' AND
02109          DC-ELAPSED-DAYS LESS THAN +91)
02110             MOVE 'Y'             TO  AM-EMPLOYER-STMT-USED
041002     END-IF.
02111
02112  3034-CHECK-ACCOUNT-LIMITS.
02113
02117      IF CERTMTI NOT = 'A'
02118          GO TO 3060-BUILD-CONT
041002     END-IF.
02119
02120      IF AM-STATUS NOT = '0'
02121          MOVE ER-0225                TO EMI-ERROR
02122          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02123          MOVE -1                     TO ACCOUNTL
041002     END-IF.
02124
02125      IF EIBAID = DFHPF6
02126          IF EMI-FATAL-CTR NOT = ZERO
02127              MOVE ER-0434         TO EMI-ERROR
02128              MOVE -1              TO MAINTL
02129              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02130              GO TO 8200-SEND-DATAONLY
041002         END-IF
041002     END-IF.
02131
121802*    IF PI-COMPANY-ID NOT = 'CRI'
121802*        GO TO 3035-END-BROWSE
121802*    END-IF.
02201
02202  3035-END-BROWSE.
02203
           
      * EXEC CICS ENDBR
02204 *        DATASET    (ERACCT2-DSID)
02205 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009115' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02206
02207  3040-CHECK-CERT-ALREADY-ON.
02208
           
      * EXEC CICS HANDLE CONDITION
02209 *        NOTFND   (3070-BUILD-CONT)
02210 *    END-EXEC.
      *    MOVE '"$I                   ! 1 #00009121' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303039313231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02211
02212      PERFORM 7940-READ-CERT THRU 7940-EXIT.
02213
02214      MOVE ER-0229                TO EMI-ERROR.
02215      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02216      MOVE -1                     TO MAINTL.
02217      GO TO 8200-SEND-DATAONLY.
02218
02219  3060-BUILD-CONT.
02220      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
02221      MOVE CRTCARRI               TO CERT-CARRIER.
02222      MOVE GROUPI                 TO CERT-GROUPING.
02223      MOVE STATEI                 TO CERT-STATE.
02224      MOVE PI-SAVE-ACCOUNT        TO CERT-ACCOUNT.
02225      MOVE CERTNOI                TO CERT-CERT-NO-PRIME.
02226      MOVE SUFXI                  TO CERT-CERT-NO-SUFX.
02227      MOVE PI-SAVE-EFFDT          TO CERT-EFF-DT.
02228
02229
           
      * EXEC CICS HANDLE CONDITION
02230 *        NOTFND  (3065-CERT-NOT-FOUND)
02231 *    END-EXEC.
      *    MOVE '"$I                   ! 2 #00009143' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303039313433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02232
02233      PERFORM 7930-READ-CERT-UPDATE THRU 7930-EXIT.
02234
02235      GO TO 3070-BUILD-CONT.
02236
02237  3065-CERT-NOT-FOUND.
02238      MOVE ER-0244                TO EMI-ERROR.
02239      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02240      MOVE -1                     TO CERTNOL.
02241      GO TO 8200-SEND-DATAONLY.
02242
02243  3070-BUILD-CONT.
02244      IF CLMNOL NOT = ZERO
02245          GO TO 3200-BUILD-ZERO-TRAILER
041002     END-IF.
02246
02247      MOVE SPACES                 TO ELCNTL-KEY.
02248
02249      IF CONTROL-IS-ACTUAL-CARRIER
02250          MOVE PI-CARRIER         TO CNTL-CARRIER
02251      ELSE
02252          MOVE PI-CARRIER-CONTROL-LEVEL
02253                                  TO CNTL-CARRIER
041002     END-IF.
02254
02255      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02256      MOVE '6'                    TO CNTL-REC-TYPE.
02257      MOVE +0                     TO CNTL-SEQ-NO.
02258
02259
           
      * EXEC CICS HANDLE CONDITION
02260 *        NOTFND   (3100-CARRIER-NOT-FOUND)
02261 *    END-EXEC.
      *    MOVE '"$I                   ! 3 #00009176' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303039313736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02262
02263
           
      * EXEC CICS READ
02264 *        UPDATE
02265 *        DATASET   (ELCNTL-DSID)
02266 *        SET       (ADDRESS OF CONTROL-FILE)
02267 *        RIDFLD    (ELCNTL-KEY)
02268 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00009181' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02269
02270      IF CLAIM-NO-MANUAL
02271          GO TO 3120-CLAIM-MUST-BE-INPUT
041002     END-IF.
02272
02273      MOVE CF-CLAIM-NO-METHOD     TO SAVE-METHOD.
02274      MOVE CF-CLAIM-COUNTER       TO SAVE-COUNTER.
02275
020816     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' OR 'VPP'
062121           or 'FNL'
041002         CONTINUE
02278      ELSE
02279          GO TO 3080-ASSIGN-LOOP
041002     END-IF.
02280
02281
           
      * EXEC CICS UNLOCK
02282 *        DATASET   (ELCNTL-DSID)
02283 *    END-EXEC.
      *    MOVE '&*                    #   #00009203' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02284
02285      MOVE '1'                    TO CNTL-REC-TYPE.
02286      MOVE SPACES                 TO CNTL-ACCESS.
02287
030612     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
02290
02291
           
      * EXEC CICS HANDLE CONDITION
02292 *        NOTFND   (3110-COMPANY-NOT-FOUND)
02293 *    END-EXEC.
      *    MOVE '"$I                   ! 4 #00009213' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303039323133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02294
02295      PERFORM 7960-READ-CNTL-UPDATE THRU 7960-EXIT.
02296
02297      MOVE CF-CO-CLAIM-COUNTER    TO SAVE-COUNTER.
02298
02300  3080-ASSIGN-LOOP.
02301      IF (SAVE-COUNTER = +99999   AND SAVE-METHOD = '2') OR
02302         (SAVE-COUNTER = +9999999 AND SAVE-METHOD = '3')
02303          MOVE +0                 TO SAVE-COUNTER
041002     END-IF.
02304
02305      ADD +1 TO SAVE-COUNTER.
02306
02307      IF SAVE-METHOD  = '3'
02308          MOVE SAVE-COUNTER        TO WS-CLAIM-NUMBER-R1
02309          GO TO 3090-SET-COMP
041002     END-IF.
02310
02311      IF SAVE-METHOD  = '4'
02312          MOVE SAVE-COUNTER       TO WS-CLAIM-NUMBER-R1
02313          MOVE CLMCARRI           TO WS-CN-PRF-A
02314          GO TO 3090-SET-COMP
041002     END-IF.
02315
02316      MOVE ZEROS                  TO WS-CLAIM-NUMBER.
02317      MOVE SAVE-COUNTER           TO WS-CN-NUMBER.
02318      MOVE SAVE-DATE              TO CURR-DATE.
02319
02320      IF CURR-MM LESS '10'
02321          MOVE CURR-M2             TO WS-CN-PRF-B
041002     END-IF.
02322
02323      IF CURR-MM = '10'
02324         MOVE 'A'                 TO WS-CN-PRF-B
041002     END-IF.
02325      IF CURR-MM = '11'
02326         MOVE 'B'                 TO WS-CN-PRF-B
041002     END-IF.
02327      IF CURR-MM = '12'
02328         MOVE 'C'                 TO WS-CN-PRF-B
041002     END-IF.
02329
02330      MOVE CURR-YEAR              TO WS-CN-PRF-A.
02331
02338  3090-SET-COMP.
02339
02340      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
02341      MOVE WS-CLAIM-NUMBER        TO MSTR-CLAIM-NO
02342                                     CLMNOO.
02343      MOVE AL-UANON               TO CLMNOA.
02344      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
02345      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
02346      MOVE CLMCARRI               TO MSTR-CARRIER.
02347
02348
           
      * EXEC CICS HANDLE CONDITION
02349 *        NOTFND  (3200-BUILD-ZERO-TRAILER)
02350 *    END-EXEC.
      *    MOVE '"$I                   ! 5 #00009271' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303039323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02351
02352      PERFORM 7950-READ-CLAIM THRU 7950-EXIT.
02353
02354      GO TO 3080-ASSIGN-LOOP.
02355
02356  3100-CARRIER-NOT-FOUND.
02357      MOVE ER-0252                TO EMI-ERROR.
02358      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02359      MOVE -1                     TO CLMCARRL.
02360      GO TO 8200-SEND-DATAONLY.
02361
02362  3110-COMPANY-NOT-FOUND.
02363      MOVE ER-0254                TO EMI-ERROR.
02364      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02365      MOVE -1                     TO MAINTL.
02366      GO TO 8200-SEND-DATAONLY.
02367
02368  3120-CLAIM-MUST-BE-INPUT.
02369      MOVE ER-0264                TO EMI-ERROR.
02370      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02371      MOVE -1                     TO CLMNOL.
02372      GO TO 8200-SEND-DATAONLY.
02373
02375  3200-BUILD-ZERO-TRAILER.
02376
           
      * EXEC CICS HANDLE CONDITION
02377 *        NOTFND
02378 *    END-EXEC.
      *    MOVE '"$�                   ! 6 #00009299' TO DFHEIV0
           MOVE X'2224B9202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303039323939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02379
02380
           
      * EXEC CICS GETMAIN
02381 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
02382 *        LENGTH  (ELTRLR-LENGTH)
02383 *    END-EXEC.
      *    MOVE '," L                  $   #00009304' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELTRLR-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02384
02385      MOVE SPACES                 TO ACTIVITY-TRAILERS.
02386      MOVE 'AT'                   TO AT-RECORD-ID.
02387
02388      MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
02389      MOVE CLMCARRI               TO AT-CARRIER.
02390      MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
02391      MOVE CERT-CERT-NO           TO AT-CERT-NO.
02392      MOVE +0                     TO AT-SEQUENCE-NO.
02393      MOVE '1'                    TO AT-TRAILER-TYPE.
02394
02395      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
02396      MOVE '5'                    TO DC-OPTION-CODE.
02397      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02398      MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
02399                                     AT-RESERVES-LAST-MAINT-DT.
02400
02401      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
02402                                     AT-RESERVES-LAST-UPDATED-BY.
02403
02404      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
02405
02406      MOVE WS-RESERVE-CONTROLS    TO AT-RESERVE-CONTROLS.
02407
02408      MOVE +0                     TO AT-FUTURE-RESERVE
02409                                     AT-IBNR-RESERVE
02410                                     AT-PAY-CURRENT-RESERVE
02411                                     AT-INITIAL-MANUAL-RESERVE
02412                                     AT-CURRENT-MANUAL-RESERVE
02413                                     AT-ITD-LIFE-REFUNDS
02414                                     AT-ITD-AH-REFUNDS
02415                                     AT-ITD-ADDITIONAL-RESERVE.
02416
02417      MOVE LOW-VALUES             TO AT-LAST-COMPUTED-DT.
02418
02419      IF MANRSVL NOT = ZERO
02420          MOVE WS-MANRSV          TO AT-INITIAL-MANUAL-RESERVE
02421                                     AT-CURRENT-MANUAL-RESERVE
041002     END-IF.
02422
02423      MOVE WS-EXPENSE-CONTROLS    TO AT-EXPENSE-CONTROLS.
02424
02425      MOVE +0                     TO AT-ITD-PAID-EXPENSES
02426                                     AT-ITD-CHARGEABLE-EXPENSE.
02427
02428      MOVE WS-TODAY-DT            TO AT-OPEN-CLOSE-DATE (1).
02429      MOVE 'O'                    TO AT-OPEN-CLOSE-TYPE (1).
02430      MOVE 'NEW'                  TO AT-OPEN-CLOSE-REASON (1).
02431
02432
           
      * EXEC CICS WRITE
02433 *        DATASET   (ELTRLR-DSID)
02434 *        FROM      (ACTIVITY-TRAILERS)
02435 *        RIDFLD    (AT-CONTROL-PRIMARY)
02436 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009358' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02437
02438      MOVE AT-CONTROL-PRIMARY  TO  SAVE-AT-PRIMARY-KEY.
02439
02440      EJECT
050807
050807 3250-BUILD-I1-ADDRESS-TRAILER.
050807
050807     MOVE 'N'                    TO WS-NO-INSURED-ADDRESS.
050807     MOVE +0                     TO WS-INSURED-ADDR-CNT.
050807     MOVE PI-COMPANY-CD          TO WS-MA-COMPANY-CD.
050807     MOVE CRTCARRI               TO WS-MA-CARRIER.
050807     MOVE GROUPI                 TO WS-MA-GROUPING.
050807     MOVE STATEI                 TO WS-MA-STATE.
050807     MOVE PI-SAVE-ACCOUNT        TO WS-MA-ACCOUNT.
050807     MOVE CERTNOI                TO WS-MA-CERT-PRIME.
050807     MOVE SUFXI                  TO WS-MA-CERT-SFX.
050807     MOVE PI-SAVE-EFFDT          TO WS-MA-CERT-EFF-DT.
050807
050807     
      * EXEC CICS HANDLE CONDITION
050807*        NOTFND   (3290-NO-INSURED-ADDRESS)
050807*    END-EXEC.
      *    MOVE '"$I                   ! 7 #00009381' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303039333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
050807
050807     
      * EXEC CICS READ
050807*        EQUAL
050807*        DATASET   (ERMAIL-DSID)
050807*        SET       (ADDRESS OF MAILING-DATA)
050807*        RIDFLD    (WS-MA-CONTROL-PRIMARY)
050807*    END-EXEC.
      *    MOVE '&"S        E          (   #00009385' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-MA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
050807
050807     IF MA-INSURED-LAST-NAME NOT GREATER THAN SPACES
050807         GO TO 3290-NO-INSURED-ADDRESS
050807     END-IF.
050807
050807     MOVE SPACES                 TO ACTIVITY-TRAILERS.
050807     MOVE 'AT'                   TO AT-RECORD-ID.
050807
050807     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
050807     MOVE CLMCARRI               TO AT-CARRIER.
050807     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
050807     MOVE CERT-CERT-NO           TO AT-CERT-NO.
050807     MOVE +1                     TO AT-SEQUENCE-NO.
050807     MOVE '5'                    TO AT-TRAILER-TYPE.
050807
050807     MOVE 'I'                    TO AT-ADDRESS-TYPE.
050807     IF MA-INSURED-MIDDLE-INIT > SPACES
050807        STRING MA-INSURED-FIRST-NAME DELIMITED BY '  '
050807               ' ' DELIMITED BY SIZE
050807               MA-INSURED-MIDDLE-INIT DELIMITED BY SIZE
050807               ' ' DELIMITED BY SIZE
050807               MA-INSURED-LAST-NAME DELIMITED BY SIZE
050807        INTO AT-MAIL-TO-NAME
050807     ELSE
050807        STRING MA-INSURED-FIRST-NAME DELIMITED BY '  '
050807               ' ' DELIMITED BY SIZE
050807               MA-INSURED-LAST-NAME DELIMITED BY SIZE
050807        INTO AT-MAIL-TO-NAME
050807     END-IF.
050807     MOVE MA-ADDRESS-LINE-1      TO AT-ADDRESS-LINE-1.
050807     MOVE MA-ADDRESS-LINE-2      TO AT-ADDRESS-LINE-2.
050807*    MOVE MA-CITY-STATE          TO AT-CITY-STATE.
           MOVE MA-CITY                TO AT-CITY
           MOVE MA-ADDR-STATE          TO AT-STATE
050807     MOVE MA-ZIP                 TO AT-ZIP
050807     MOVE MA-PHONE-NO            TO AT-PHONE-NO
050807
050807     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
050807                                    AT-ADDRESS-LAST-MAINT-DT.
050807
050807     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
050807                                    AT-ADDRESS-LAST-UPDATED-BY.
050807
050807     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
050807
050807     
      * EXEC CICS WRITE
050807*        DATASET   (ELTRLR-DSID)
050807*        FROM      (ACTIVITY-TRAILERS)
050807*        RIDFLD    (AT-CONTROL-PRIMARY)
050807*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009436' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
050807
050807     ADD +1                      TO WS-INSURED-ADDR-CNT.
050807     GO TO 3300-BUILD-NINETY-TRAILER.
050807
050807 3290-NO-INSURED-ADDRESS.
050807      MOVE 'Y' TO WS-NO-INSURED-ADDRESS.
050807
02441  3300-BUILD-NINETY-TRAILER.
02442
02443      MOVE SPACES                 TO ACTIVITY-TRAILERS.
02444      MOVE 'AT'                   TO AT-RECORD-ID.
02445
02446      MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
02447      MOVE CLMCARRI               TO AT-CARRIER.
02448
02449      MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
02450      MOVE CERT-CERT-NO           TO AT-CERT-NO.
02451      MOVE +90                    TO AT-SEQUENCE-NO.
02452      MOVE '6'                    TO AT-TRAILER-TYPE.
02453
02454      IF DIAGL NOT = ZERO
02455          MOVE DIAGI              TO AT-INFO-LINE-1
041002     END-IF.
040814
040814     IF ICD1L NOT = ZERO
040814         MOVE ICD1I              TO AT-ICD-CODE-1
040814     END-IF.
040814
040814     IF ICD2L NOT = ZERO
040814         MOVE ICD2I              TO AT-ICD-CODE-2
040814     END-IF.
02456
02457      MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
02458                                     AT-GEN-INFO-LAST-MAINT-DT.
02459
02460      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
02461                                     AT-GEN-INFO-LAST-UPDATED-BY.
02462      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
02463
02464
           
      * EXEC CICS WRITE
02465 *        DATASET   (ELTRLR-DSID)
02466 *        FROM      (ACTIVITY-TRAILERS)
02467 *        RIDFLD    (AT-CONTROL-PRIMARY)
02468 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009481' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02469
02470      EJECT
02471  3400-CHECK-CERT-MAINT.
02472
02473      IF CERTMTI = 'A'
02474          GO TO 3600-BUILD-NEW-CERT
041002     END-IF.
02475
02476      IF NO-CLAIM-ATTACHED
02477          MOVE '1'                TO CM-CLAIM-INTERFACE-SW
041002     END-IF.
02478
02479      ADD +1                      TO CM-CLAIM-ATTACHED-COUNT.
02480
02481      IF CM-LF-CURRENT-STATUS = '2' OR
02482         CM-AH-CURRENT-STATUS = '2'
02483          GO TO 3400-REWRITE-CERT
041002     END-IF.
02484
02485      PERFORM 3700-BUILD-CERT THRU 3700-EXIT.
02486
02487  3400-REWRITE-CERT.
02488
02489
           
      * EXEC CICS HANDLE CONDITION
02490 *        DUPKEY (3410-DUP-KEY)
02491 *    END-EXEC.
      *    MOVE '"$$                   ! 8 #00009510' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303039353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02492
02493
           
      * EXEC CICS REWRITE
02494 *        DATASET   (ELCERT-DSID)
02495 *        FROM      (CERTIFICATE-MASTER)
02496 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009515' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02497
02498  3410-DUP-KEY.
02499      GO TO 3800-BUILD-NEW-CLAIM.
02500
02501  3600-BUILD-NEW-CERT.
02502
           
      * EXEC CICS GETMAIN
02503 *        SET      (ADDRESS OF CERTIFICATE-MASTER)
02504 *        LENGTH   (ELCERT-LENGTH)
02505 *    END-EXEC.
      *    MOVE '," L                  $   #00009525' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCERT-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02506
02507      MOVE SPACES                 TO CERTIFICATE-MASTER.
02508
02509      MOVE 'CM'                   TO CM-RECORD-ID.
02510
02511      MOVE PI-COMPANY-CD          TO CM-COMPANY-CD
02512                                     CM-COMPANY-CD-A1
02513                                     CM-COMPANY-CD-A2
02514                                     CM-COMPANY-CD-A4
02515                                     CM-COMPANY-CD-A5.
02516
02517      MOVE CERT-CARRIER           TO CM-CARRIER.
02518      MOVE CERT-GROUPING          TO CM-GROUPING.
02519      MOVE CERT-STATE             TO CM-STATE.
02520      MOVE CERT-ACCOUNT           TO CM-ACCOUNT.
02521      MOVE CERT-CERT-NO           TO CM-CERT-NO  CM-CERT-NO-A4.
02522      MOVE CERT-EFF-DT            TO CM-CERT-EFF-DT.
02523
02524      MOVE ZERO      TO CM-LF-ORIG-TERM       CM-LF-BENEFIT-AMT
02525                        CM-INSURED-ISSUE-AGE  CM-INSURED-JOINT-AGE
02526                        CM-LF-BENEFIT-CD      CM-AH-BENEFIT-CD
02527                        CM-LF-PREMIUM-AMT     CM-LF-REMAINING-AMT
02528                        CM-LF-ITD-CANCEL-AMT  CM-LF-ITD-DEATH-AMT
02529                        CM-LIVES              CM-LOAN-TERM
02530                        CM-AH-ITD-AH-PMT      CM-AH-NSP-PREMIUM-AMT
02531                        CM-AH-DEV-PCT         CM-AH-CRITICAL-PERIOD
02532                        CM-LF-TERM-IN-DAYS    CM-LF-ALT-PREMIUM-AMT
02533                        CM-LF-DEV-PCT         CM-LF-NSP-PREMIUM-AMT
02534                        CM-LF-CRITICAL-PERIOD CM-LF-ALT-BENEFIT-AMT
02535                        CM-LF-PREMIUM-RATE    CM-AH-PREMIUM-RATE
02536                        CM-LF-ALT-PREMIUM-RATE
02537                        CM-AH-ORIG-TERM       CM-AH-BENEFIT-AMT
02538                        CM-AH-PREMIUM-AMT     CM-AH-ITD-CANCEL-AMT
02539                        CM-AH-ITD-LUMP-PMT    CM-LOAN-APR
02540                        CM-LOAN-BALANCE       CM-PAY-FREQUENCY
02541                        CM-LIFE-COMM-PCT      CM-AH-COMM-PCT.
02542
02543      MOVE LOW-VALUES             TO CM-AH-LOAN-EXPIRE-DT
02544                                     CM-AH-CANCEL-DT
02545                                     CM-AH-SETTLEMENT-DT
02546                                     CM-AH-CANCEL-EXIT-DT
02547                                     CM-AH-SETTLEMENT-EXIT-DT
02548                                     CM-LF-LOAN-EXPIRE-DT
02549                                     CM-LF-CANCEL-DT
02550                                     CM-LF-DEATH-DT
02551                                     CM-LF-CANCEL-EXIT-DT
02552                                     CM-LF-DEATH-EXIT-DT
02553                                     CM-LAST-ADD-ON-DT
02554                                     CM-ENTRY-DT.
02555
02556      MOVE '2'                    TO CM-CLAIM-INTERFACE-SW.
02557      MOVE +1                     TO CM-CLAIM-ATTACHED-COUNT.
02558      MOVE LOW-VALUES             TO CM-LAST-MONTH-END.
02559
02560      MOVE CRTLNMEI               TO CM-INSURED-LAST-NAME
02561                                     CM-PART-LAST-NAME-A2
02562                                     CM-PART-LAST-NAME-A5.
02563
02564      MOVE CRTFNMEI               TO CM-INSURED-INITIAL1
02565                                     CM-INSURED-INITIAL1-A2
02566                                     CM-INSURED-INITIAL1-A5
02567                                     CM-INSURED-FIRST-NAME.
02568
02569      IF CRTINITL GREATER ZERO
02570          MOVE CRTINITI           TO CM-INSURED-INITIAL2
02571                                     CM-INSURED-INITIAL2-A2
02572                                     CM-INSURED-INITIAL2-A5
02573      ELSE
02574          MOVE SPACES             TO CM-INSURED-INITIAL2
02575                                     CM-INSURED-INITIAL2-A2
02576                                     CM-INSURED-INITIAL2-A5
041002     END-IF.
02577
02578      IF CM-INSURED-INITIALS = SPACES
02579          MOVE '**'               TO CM-INSURED-INITIALS
041002     END-IF.
02580
02581      IF MEMBERL GREATER ZERO
02582          MOVE MEMBERI              TO CM-MEMBER-NO
02583      ELSE
02584          MOVE CERT-STATE           TO CM-MEMB-STATE
02585          MOVE CERT-ACCOUNT-PRIME   TO CM-MEMB-ACCOUNT
02586          MOVE CM-INSURED-INITIALS  TO CM-INSURED-INITIALS-A5
02587          MOVE CM-INSURED-LAST-NAME TO CM-PART-LAST-NAME-A5
041002     END-IF.
02588
02589      IF JNTLNMEL GREATER ZERO
02590          MOVE JNTLNMEI           TO CM-JT-LAST-NAME
041002     END-IF.
02591
02592      IF JNTFNMEL GREATER ZERO
02593          MOVE JNTFNMEI           TO CM-JT-FIRST-NAME
041002     END-IF.
02594
02595      IF JNTINITL GREATER ZERO
02596          MOVE JNTINITI           TO CM-JT-INITIAL
041002     END-IF.
02597
02598      PERFORM 3700-BUILD-CERT THRU 3700-EXIT.
02599
02600
           
      * EXEC CICS HANDLE CONDITION
02601 *        DUPKEY   (3690-CERT-WRITTEN)
02602 *    END-EXEC.
      *    MOVE '"$$                   ! 9 #00009630' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303039363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02603
02604
           
      * EXEC CICS WRITE
02605 *        DATASET   (ELCERT-DSID)
02606 *        FROM      (CERTIFICATE-MASTER)
02607 *        RIDFLD    (CM-CONTROL-PRIMARY)
02608 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009635' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02609
02610  3690-CERT-WRITTEN.
02611
02612      GO TO 3800-BUILD-NEW-CLAIM.
02613
02614  3690-EXIT.
02615       EXIT.
02616
02617      EJECT
02618  3700-BUILD-CERT.
02619
02620      IF CERTMTI NOT = 'A'
02621          GO TO 3700-FINISH-CERT-BUILD
041002     END-IF.
02622
02623      IF CERTMTI = 'A'
02624          IF LCVCDL = ZEROS
02625              MOVE ZEROS          TO CM-LF-BENEFIT-CD
02626                                     CM-LF-ORIG-TERM
02627                                     CM-LF-BENEFIT-AMT
02628                                     CM-LF-PREMIUM-RATE
02629              GO TO 3700-BUILD-AH-COVERAGE-SIDE
041002         END-IF
041002     END-IF.
02630
02631      IF LCVCDL GREATER ZERO
02632          MOVE LCVCDI             TO CM-LF-BENEFIT-CD
041002     END-IF.
02633
02634      IF LCVOTRML GREATER ZERO
02635          MOVE LCVOTRMI           TO CM-LF-ORIG-TERM
041002     END-IF.
02636
02637      IF LCVBENEL GREATER ZERO
02638          MOVE WS-LCVBENE         TO CM-LF-BENEFIT-AMT
041002     END-IF.
02639
02640      IF LCVRATEL GREATER ZERO
02641          MOVE WS-LCVRATE         TO CM-LF-PREMIUM-RATE
041002     END-IF.
02642
02643      IF WS-EXPIRE NOT = LOW-VALUES
02644          MOVE WS-EXPIRE          TO CM-LF-LOAN-EXPIRE-DT
041002     END-IF.
02645
02646      IF LCVFORML GREATER ZERO
02647          MOVE LCVFORMI           TO CM-POLICY-FORM-NO
041002     END-IF.
02648
02649      IF LCVCNDTL = ZERO
02650         GO TO 3700-BUILD-AH-COVERAGE-SIDE
041002     END-IF.
02651
02652      IF WS-LCVCNDT NOT = LOW-VALUES
02653         MOVE '8'                 TO CM-LF-CURRENT-STATUS
02654         MOVE WS-LCVCNDT          TO CM-LF-CANCEL-DT
02655         GO TO 3700-BUILD-AH-COVERAGE-SIDE
041002     END-IF.
02656
02657      IF CM-LF-CURRENT-STATUS = '7'
02658         MOVE CM-LF-STATUS-AT-DEATH TO CM-LF-CURRENT-STATUS
02659         MOVE SPACES              TO CM-LF-STATUS-AT-DEATH
02660         MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT
02661                                     CM-LF-DEATH-DT
02662         GO TO 3700-BUILD-AH-COVERAGE-SIDE
041002     END-IF.
02663
02664      IF CM-LF-CURRENT-STATUS = '8'
02665         MOVE '1'                 TO CM-LF-CURRENT-STATUS
02666         MOVE LOW-VALUES          TO CM-LF-CANCEL-DT
041002     END-IF.
02667
02668  3700-BUILD-AH-COVERAGE-SIDE.
02669
02670      IF CERTMTI = 'A'
02671          IF ACVCDL = ZEROS
02672              MOVE ZEROS          TO CM-AH-BENEFIT-CD
02673                                     CM-AH-ORIG-TERM
02674                                     CM-AH-BENEFIT-AMT
02675                                     CM-AH-PREMIUM-RATE
02676              GO TO 3700-COVERAGE-BUILT
041002         END-IF
041002     END-IF.
02677
02678      IF ACVCDL GREATER ZERO
02679          MOVE ACVCDI             TO CM-AH-BENEFIT-CD
041002     END-IF.
02680
02681      IF ACVOTRML GREATER ZERO
02682          MOVE ACVOTRMI           TO CM-AH-ORIG-TERM
041002     END-IF.
02683
02684      IF ACVBENEL GREATER ZERO
02685          MOVE WS-ACVBENE         TO CM-AH-BENEFIT-AMT
041002     END-IF.
02686
02687      IF ACVRATEL GREATER ZERO
02688          MOVE WS-ACVRATE         TO CM-AH-PREMIUM-RATE
041002     END-IF.
02689
02690      IF WS-EXPIRE NOT = LOW-VALUES
02691          MOVE WS-EXPIRE          TO CM-AH-LOAN-EXPIRE-DT
041002     END-IF.
02692
02693      IF ACVFORML GREATER ZERO
02694          MOVE ACVFORMI           TO CM-POLICY-FORM-NO
041002     END-IF.
02695
02696      IF ACVCNDTL = ZERO
02697         GO TO 3700-COVERAGE-BUILT
041002     END-IF.
02698
02699      IF WS-ACVCNDT NOT = LOW-VALUES
02700         MOVE '8'                 TO CM-AH-CURRENT-STATUS
02701         MOVE WS-ACVCNDT          TO CM-AH-CANCEL-DT
02702         GO TO 3700-COVERAGE-BUILT
041002     END-IF.
02703
02704      IF CM-AH-CURRENT-STATUS = '6'
02705         MOVE CM-AH-STATUS-AT-SETTLEMENT TO CM-AH-CURRENT-STATUS
02706         MOVE SPACES              TO CM-AH-STATUS-AT-SETTLEMENT
02707         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT
02708         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-DT
02709         GO TO 3700-COVERAGE-BUILT
041002     END-IF.
02710
02711      IF CM-AH-CURRENT-STATUS = '8'
02712         MOVE '1'                 TO CM-AH-CURRENT-STATUS
02713         MOVE LOW-VALUES          TO CM-AH-CANCEL-DT
041002     END-IF.
02714
02715  3700-COVERAGE-BUILT.
02716      IF ISSAGEL GREATER ZERO
02717          MOVE ISSAGEI            TO CM-INSURED-ISSUE-AGE
041002     END-IF.
02718
02719      IF JNTAGEL GREATER ZERO
02720          MOVE JNTAGEI            TO CM-INSURED-JOINT-AGE
041002     END-IF.
02721
02722      IF SEXL GREATER ZERO
02723          MOVE SEXI               TO CM-INSURED-SEX
041002     END-IF.
02724
02725      IF APRL GREATER ZERO
02726          MOVE WS-APR             TO CM-LOAN-APR
041002     END-IF.
02727
02728      IF PMTFREQL GREATER ZERO
02729          MOVE WS-PMTFREQ         TO CM-PAY-FREQUENCY
041002     END-IF.
02730
02731      IF INDGRPL GREATER ZERO
02732          MOVE INDGRPI            TO CM-IND-GRP-TYPE
041002     END-IF.
02733
02734      IF CERTMTI = 'A'
02735          MOVE WS-REIN-TABLE      TO CM-REIN-TABLE
041002     END-IF.
02736
02737      IF REINCDL GREATER ZERO
02738          MOVE REINCDI            TO CM-SPECIAL-REIN-CODE
02739                                     WS-REIN-1
02740                                     WS-REIN-2
02741                                     WS-REIN-3
02742          MOVE WS-REIN-TABLE      TO CM-REIN-TABLE
041002     END-IF.
02743
02744      IF WS-ADD-ON-DT NOT = LOW-VALUES
02748          MOVE WS-ADD-ON-DT   TO CM-LAST-ADD-ON-DT
041002     END-IF.
02749
02750      IF CRTSSNL GREATER ZERO
02751          MOVE CRTSSNI            TO CM-SOC-SEC-NO
02752      ELSE
02753          IF SSNL GREATER ZERO
02754              MOVE SSNI           TO CM-SOC-SEC-NO
041002         END-IF
041002     END-IF.
02755
02756      IF CRTSSNL = ZERO AND
02757         SSNL    = ZERO
02758          MOVE CM-STATE             TO CM-SSN-STATE
02759          MOVE CM-ACCOUNT-PRIME     TO CM-SSN-ACCOUNT
02760          MOVE CM-INSURED-INITIALS  TO CM-INSURED-INITIALS-A2
02761          MOVE CM-INSURED-LAST-NAME TO CM-PART-LAST-NAME-A2
041002     END-IF.
02762
02770      IF CM-LF-CURRENT-STATUS = SPACES
02771          MOVE '1'                TO CM-LF-CURRENT-STATUS
041002     END-IF.
02772
02773      IF CM-AH-CURRENT-STATUS = SPACES
02774          MOVE '1'                TO CM-AH-CURRENT-STATUS
041002     END-IF.
02775
02776      IF CERTMTI = 'A'
02777          MOVE SAVE-BIN-DATE      TO CM-ENTRY-DT
041002     END-IF.
02778
02779  3700-FINISH-CERT-BUILD.
02780
02781      IF LOANNOL GREATER ZERO
02782          MOVE LOANNOI            TO CM-LOAN-NUMBER
041002     END-IF.
02783
02784      IF LOANBALL GREATER ZERO
02785          MOVE HOLD-LOAN-BAL      TO CM-LOAN-BALANCE
041002     END-IF.
02786
02787      IF PREMTYPL GREATER ZERO
02788          MOVE PREMTYPI           TO CM-PREMIUM-TYPE
041002     END-IF.
02789
02790  3700-EXIT.
02791       EXIT.
02792
02793      EJECT
02794  3800-BUILD-NEW-CLAIM.
CIDMOD
CIDMOD*************************************************************
CIDMOD*****           START OF ACTIVITY FILE PROCESSING          **
CIDMOD*************************************************************
062121     IF PI-COMPANY-ID EQUAL 'CID' OR 'AHL' OR 'FNL'
CIDMOD         PERFORM 6700-OUTPUT-ACTIVITY-RECORD THRU
CIDMOD                 6700-EXIT
CIDMOD     END-IF.
CIDMOD*************************************************************
CIDMOD*****            END OF ACTIVITY FILE PROCESSING           **
CIDMOD*************************************************************
CIDMOD
02814
02795
           
      * EXEC CICS GETMAIN
02796 *        SET     (ADDRESS OF CLAIM-MASTER)
02797 *        LENGTH  (ELMSTR-LENGTH)
02798 *    END-EXEC.
      *    MOVE '," L                  $   #00009873' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELMSTR-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02799
02800      MOVE SPACES TO CLAIM-MASTER.
02801
02802      MOVE LOW-VALUES   TO       CL-INSURED-BIRTH-DT CL-INCURRED-DT
02803          CL-NEXT-FOLLOWUP-DT    CL-REPORTED-DT      CL-LAST-PMT-DT
02804          CL-EST-END-OF-DISAB-DT CL-PAID-THRU-DT   CL-LAST-CLOSE-DT
02805          CL-LAST-REOPEN-DT      CL-PURGED-DT        CL-RESTORED-DT
02806          CL-NEXT-AUTO-PAY-DT    CL-NEXT-RESEND-DT
02807          CL-LAST-ADD-ON-DT      CL-FILE-ESTABLISH-DT
02808          CL-LAST-MAINT-DT       CL-HISTORY-ARCHIVE-DT
061013         CL-ACTIVITY-MAINT-DT   cl-benefit-expiration-dt
02810
02811      MOVE +0 TO CL-LAST-PMT-AMT      CL-TOTAL-PAID-AMT
02812              CL-NO-OF-PMTS-MADE      CL-NO-OF-DAYS-PAID
02813              CL-LAST-INC-DT-CHANGE   CL-AUTO-PAY-SEQ
02814              CL-INSURED-ADDR-CNT     CL-ACCOUNT-ADDR-CNT
02815              CL-BENIF-ADDR-CNT       CL-EMPLOYER-ADDR-CNT
02816              CL-DOCTOR-ADDR-CNT      CL-OTHER-1-ADDR-CNT
02817              CL-OTHER-2-ADDR-CNT     CL-FATAL-ERROR-CNT
02818              CL-CLAIM-PAYMENT-STATUS CL-LAST-MAINT-HHMMSS
02819              CL-FORCEABLE-ERROR-CNT.
050807
050807     MOVE WS-INSURED-ADDR-CNT    TO CL-INSURED-ADDR-CNT.
02820
02821      MOVE ZEROS                  TO CL-ACTIVITY-CODE
02822                                     CL-LAPSE-REPORT-CODE
02823                                     CL-LAG-REPORT-CODE
061013                                    CL-CRITICAL-PERIOD
061013*                                   CL-CRIT-PER-RTW-MOS
           move 01                     to cl-benefit-period
02824
02825      MOVE EMI-FORCABLE-CTR       TO CL-FORCEABLE-ERROR-CNT.
02826      MOVE +4095                  TO CL-TRAILER-SEQ-CNT.
02827
02828      IF BENECDL NOT = ZEROS
02829         MOVE BENECDI             TO CL-BENEFICIARY
041002     END-IF.
02830
02831      MOVE 'CL'                   TO CL-RECORD-ID.
02832      MOVE 'CR'                   TO CL-SYSTEM-IDENTIFIER.
02833
02834      MOVE PI-COMPANY-CD  TO  CL-COMPANY-CD     CL-COMPANY-CD-A1
02835                              CL-COMPANY-CD-A2  CL-COMPANY-CD-A4
02836                              CL-COMPANY-CD-A5.
02837
02838      MOVE CLMCARRI               TO CL-CARRIER.
02839
02840      IF CLMNOL = ZERO
02841          MOVE WS-CLAIM-NUMBER    TO CL-CLAIM-NO
02842      ELSE
02843          MOVE CLMNOI             TO CL-CLAIM-NO
041002     END-IF.
02844
02845      MOVE CERTNOI                TO CL-CERT-PRIME
02846                                     CL-CERT-A4-PRIME.
02847
02848      MOVE SUFXI                  TO CL-CERT-SFX
02849                                     CL-CERT-A4-SFX.
02850
02851      MOVE PCERTNOI               TO CL-PRIME-CERT-PRIME.
02852
02853      MOVE PSUFXI                 TO CL-PRIME-CERT-SFX.
02854
02855      MOVE CL-PRIME-CERT-NO       TO PI-PRIMARY-CERT-NO.
02856
02857      MOVE LSTNMEI                TO CL-INSURED-LAST-NAME.
02858
02859      MOVE WS-TODAY-DT            TO CL-FILE-ESTABLISH-DT.
02860      MOVE ' '                    TO CL-LAST-MAINT-TYPE.
02861
02862      PERFORM 3995-BUILD-CLAIM-RECORD THRU 3995-EXIT.
02863
02864      IF CERTMTI = 'A'
02865          MOVE '2'                TO CL-CERT-ORIGIN
02866      ELSE
02867          MOVE '1'                TO CL-CERT-ORIGIN
041002     END-IF.
02868
02869      MOVE CERT-CARRIER        TO CL-CERT-CARRIER   PI-CARRIER.
02870      MOVE CERT-GROUPING       TO CL-CERT-GROUPING  PI-GROUPING.
02871      MOVE CERT-STATE          TO CL-CERT-STATE     PI-STATE.
02872      MOVE CERT-ACCOUNT        TO CL-CERT-ACCOUNT   PI-ACCOUNT.
02873      MOVE WS-EFFDT            TO CL-CERT-EFF-DT    PI-CERT-EFF-DT.
02874
02875  3820-REWRITE-CONTROL-FILE.
02876
02877      IF CLMNOL GREATER ZERO
02878          GO TO 3830-SEQUENCE-CLAIM-RECORDS
041002     END-IF.
02879
02880      IF CF-RECORD-TYPE = '6'
02881          MOVE SAVE-COUNTER    TO  CF-CLAIM-COUNTER
02882      ELSE
02883          MOVE SAVE-COUNTER    TO  CF-CO-CLAIM-COUNTER
041002     END-IF.
02884
02885
           
      * EXEC CICS REWRITE
02886 *        DATASET   (ELCNTL-DSID)
02887 *        FROM      (CONTROL-FILE)
02888 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009974' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02889
02890  3830-SEQUENCE-CLAIM-RECORDS.
02891
02892      MOVE CLAIM-MASTER           TO WS-SAVE-CLAIM-MASTER.
02893      MOVE CL-CONTROL-PRIMARY     TO ELMSTR-KEY.
02894      PERFORM 7700-CHECK-SEQUENCE THRU 7799-EXIT.
02895
02896      IF WS-ASSOC-CERT-TOTAL = ZERO
02897          MOVE 1                  TO CL-ASSOC-CERT-SEQU
02898                                     CL-ASSOC-CERT-TOTAL
02899          GO TO 3840-WRITE-CLAIM-MASTER
041002     END-IF.
02900
02901      IF CL-CONTROL-PRIMARY LESS THAN WS-SAVE-CLAIM-KEY
02902          MOVE CL-CONTROL-PRIMARY TO ELMSTR-KEY
02903      ELSE
02904          MOVE WS-SAVE-CLAIM-KEY  TO ELMSTR-KEY
041002     END-IF.
02905
02906      MOVE WS-SAVE-CLAIM-MASTER   TO CLAIM-MASTER.
02907      MOVE ZERO                   TO CL-ASSOC-CERT-SEQU.
02908      MOVE WS-ASSOC-CERT-TOTAL    TO CL-ASSOC-CERT-TOTAL.
02909
02910  3840-WRITE-CLAIM-MASTER.
02911
02926      PERFORM 6600-CHECK-AUTO-ACTIVITY THRU 6600-EXIT.
02927      IF WS-REC-FOUND-SW = 'Y'
02928          MOVE 01                 TO  CL-ACTIVITY-CODE
02929          MOVE SAVE-BIN-DATE      TO  CL-ACTIVITY-MAINT-DT
02930          MOVE 'ADD '             TO  CL-ACTIVITY-MAINT-TYPE
041002     END-IF.
02931
02932
           
      * EXEC CICS HANDLE CONDITION
02933 *         DUPKEY    (3850-CLAIM-MSTR-WRITTEN)
02934 *    END-EXEC.
      *    MOVE '"$$                   ! : #00010011' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303130303131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02935
02936 ** POPULATE THE CREDIT-CARD NO WITH THE CERT NO.
02937      MOVE CL-CERT-NO             TO CL-CCN-A5.
110921     move 'CLMS'                 to pa-rec-type
110921     move pi-company-id          to pa-company-id
110921     move claim-master           to pa-rest-of-record
110921
110921     
      * EXEC CICS LINK
110921*        PROGRAM  ('WF001')
110921*        COMMAREA (work-flow-pass-area)
110921*        LENGTH   (604)
110921*    END-EXEC
           MOVE 'WF001' TO DFHEIV1
           MOVE 604
             TO DFHEIV11
      *    MOVE '."C                   (   #00010021' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 work-flow-pass-area, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * EXEC CICS WRITE
02940 *        DATASET   (ELMSTR-DSID)
02941 *        FROM      (CLAIM-MASTER)
02942 *        RIDFLD    (CL-CONTROL-PRIMARY)
02943 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010026' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 CL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02944
02945  3850-CLAIM-MSTR-WRITTEN.
02946
02947      IF WS-ASSOC-CERT-TOTAL NOT = ZERO
02948          PERFORM 7710-RESEQUENCE-CLAIMS THRU 7799-EXIT
041002     END-IF.
02949
02950      EJECT
02951  3990-ADD-DONE.
PEMTST*    MOVE 1                     TO EMI-SWITCH1  EMI-SWITCH-AREA-1
PEMTST*                                  EMI-SUB      EMI-SWITCH-AREA-2.
PEMTST*    MOVE SPACES                TO EMI-ACTION-SWITCH
PEMTST*                                  EMI-ERROR-LINES.
02956
02957      IF CLMNOL = ZERO
               move spaces             to emi-claim-no
02958          MOVE ER-0265            TO EMI-ERROR
02959          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02960          MOVE WS-CLAIM-NUMBER    TO EMI-TEXT-VARIABLE (EMI-SUB)
02961                                     PI-LAST-CLAIM
02962      ELSE
02963          MOVE CLMNOI             TO PI-LAST-CLAIM
042413                                    MSTR-CLAIM-NO
02964          MOVE ER-0000            TO EMI-ERROR
02965          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
061511
100518     IF (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O')
032514       AND (PI-ST-VFY-2ND-BENE = 'L' OR 'B')
061511         PERFORM 3994-BUILD-NINETY-THREE-TRAILER THRU 3994-EXIT
061511         MOVE ER-7575            TO EMI-ERROR
061511         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061511     END-IF.
032514
100518     IF CLMTYPEI NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
032514       AND (PI-ST-VFY-2ND-BENE = 'A' OR 'B')
032514         PERFORM 3994-BUILD-NINETY-THREE-TRAILER THRU 3994-EXIT
032514         MOVE ER-7582            TO EMI-ERROR
032514         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
032514     END-IF.
050807
101917     IF PI-STATE = 'VA' OR 'PA' OR 'GA'
101917        PERFORM 3991-CHECK-2-YEAR-CONTESTABLE THRU 3991-EXIT
101917     END-IF.
050807     IF WS-NO-INSURED-ADDRESS = 'Y'
050807         MOVE ER-3548            TO EMI-ERROR
050807         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
050807     END-IF.
020613
020613     IF PI-ST-CAUSAL-STATE = 'B'  OR
020613       (PI-ST-CAUSAL-STATE = 'L' AND
100518             (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'))   OR
020613       (PI-ST-CAUSAL-STATE = 'A' AND
020614              CLMTYPEI = PI-AH-OVERRIDE-L1)
020613         PERFORM 3994B-BUILD-NINETY-FOUR-TRAILER THRU 3994B-EXIT
020613         MOVE ER-7577            TO EMI-ERROR
020613         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020613     END-IF.
020816     if pi-company-id = 'DCC' OR 'VPP'
061013        PERFORM 3994C-BUILD-NINETY-FIVE-TRAILER
061013                                 THRU 3994C-EXIT
061013     end-if
071720*052918IF CLMTYPEI = PI-AH-OVERRIDE-L1
071720*052918  AND PI-COMPANY-ID = 'CID'
071720*052918   SET ELAPSED-BETWEEN-BIN TO TRUE
071720*052918   MOVE ZERO               TO DC-ELAPSED-MONTHS
071720*052918                              DC-ELAPSED-DAYS
071720*052918
071720*052918   MOVE CL-INCURRED-DT  TO DC-BIN-DATE-1
071720*052918   MOVE WS-TODAY-DT TO DC-BIN-DATE-2
071720*052918   PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
071720*052918   IF DC-ODD-DAYS-OVER > ZERO
071720*052918      ADD 1 TO DC-ELAPSED-MONTHS
071720*052918   END-IF
071720*052918
071720*052918   IF PI-STATE = 'HI'
071720*052918     AND DC-ELAPSED-MONTHS <= 18
071720*052918      CONTINUE
071720*052918   ELSE
071720*052918      IF DC-ELAPSED-MONTHS > 15
071720*052918         MOVE ER-7572            TO EMI-ERROR
071720*052918         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
071720*052918         PERFORM 3992-BUILD-TRAILER THRU 3992-EXIT
071720*052918      END-IF
071720*052918   END-IF
071720*052918
071720*052918END-IF.
090821     if clmtypei = 'L' or 'O'
090821        move cm-lf-benefit-cd    to ws-ben-hold
090821        move '4'                 to ws-rec-type
090821        perform 7100-read-benefit thru 7199-exit
090821        if ws-ben-alpha-hold <> spaces
090821           move ws-special-calc-cd
090821                                 to ws-lf-special-calc-cd
090821        end-if
090821     else
090821        move cm-ah-benefit-cd    to ws-ben-hold
090821        move '5'                 to ws-rec-type
090821        perform 7100-read-benefit thru 7199-exit
090821        if ws-ben-alpha-hold <> spaces
090821           move ws-special-calc-cd
090821                                 to ws-ah-special-calc-cd
090821        end-if
090821     end-if
090821
090821     move ' '                    to ws-mob-cert-ind
090821     if pi-company-id = 'CID'
090821        if ((CLMTYPEI = PI-LIFE-OVERRIDE-L1 or 'O')
090821           and (ws-lf-special-calc-cd = 'O'))
090821                      or
090821           ((clmtypei <> pi-life-override-l1 and 'O')
090821           and (ws-ah-special-calc-cd = 'O'))
090821           set mob-cert to true
090821        end-if
090821     else
090821        if (pi-company-id = 'DCC')
090821           and (cm-carrier = '7')
090821           set mob-cert to true
090821        end-if
090821     end-if
090821
090821     perform 7990-get-lo-hi-acct-dates
090821                                 thru 7990-exit
090821
090821     if (ws-incur >= ws-hi-acct-dt)
090821        and (acct-cancelled)
090821        and (mob-cert)
090821        MOVE er-1682             TO EMI-ERROR
090821        MOVE -1                  TO INCURL
090821        MOVE AL-UABON            TO INCURA
090821        PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821        PERFORM 3992-BUILD-TRAILER
090821                                 THRU 3992-EXIT
090821     end-if
090821
090821     if ws-incur < cm-cert-eff-dt
090821        MOVE er-1683             TO EMI-ERROR
090821        MOVE -1                  TO INCURL
090821        MOVE AL-UABON            TO INCURA
090821        PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821        PERFORM 3992-BUILD-TRAILER
090821                                 THRU 3992-EXIT
090821     end-if
           .
       3990-continue.
02967      MOVE CLMNOI                     TO  PI-CLAIM-NO.
02968      MOVE CLMCARRI                   TO  PI-CARRIER.
02969      MOVE CERTNOI                    TO  PI-CERT-PRIME.
02970      MOVE SUFXI                      TO  PI-CERT-SFX.
02971
02972      IF WS-LETTER-SW = 'Y'
02973          PERFORM 6600-START-AUTO-LETTER-WRITER THRU 6600-EXIT
02974          IF W-1523-ERROR-CODE NOT = ZEROS
02975              MOVE W-1523-ERROR-CODE  TO  EMI-ERROR
02976              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
02977
061013     perform 3998-update-cert-claim-trailer
061013                                 thru 3998-exit
02978      MOVE CLMTYPEI               TO PI-LAST-CLAIM-TYPE.
02979      MOVE CERTNOI                TO PI-LAST-CERT-PRIME.
02980      MOVE SUFXI                  TO PI-LAST-CERT-SUFX.
02981      MOVE CLMCARRI               TO PI-LAST-CARR.
02982
02983      IF PRTOPTL GREATER THAN 0
02984          MOVE PRTOPTI            TO PI-PRT-OPT
041002     END-IF.
02985      IF ALTPRTL GREATER THAN 0
02986          MOVE ALTPRTI            TO PI-ALT-PRT
041002     END-IF.
02987
02988      IF PI-CERT-PROCESSED NOT LESS THAN PI-CERT-SELECT-CNT
02989          MOVE LOW-VALUES         TO EL130AI
02990          MOVE PI-LAST-CERT-PRIME TO CERTNOO
PEMMOD         MOVE AL-SANON           TO CERTNOA
02992          MOVE PI-LAST-CERT-SUFX  TO SUFXO
PEMMOD         MOVE AL-SANON           TO SUFXA
02994          MOVE PI-LAST-CARR       TO CLMCARRO
02995          MOVE AL-UANON           TO CLMCARRA
02996          MOVE PI-LAST-CLAIM      TO CLMNOO
02997          MOVE AL-UANON           TO CLMNOA
02998          MOVE PI-LAST-CLAIM-TYPE TO CLMTYPEO
02999          MOVE AL-UANON           TO CLMTYPEA
03000      ELSE
03001          PERFORM 0690-HIGHLIGHT-CERTS THRU 0690-EXIT
03002          PERFORM 3993-MODIFY-CLAIM-ATTRB THRU 3993-EXIT
041002     END-IF.
03003
03004      IF PI-PRT-OPT = 'N' OR 'L'
03005          MOVE PI-PRT-OPT         TO PRTOPTO
03006          MOVE 1                  TO PRTOPTL
041002     END-IF.
03007
03008      IF PI-ALT-PRT NOT = SPACES AND LOW-VALUES
03009          MOVE PI-ALT-PRT         TO ALTPRTO
03010          MOVE 4                  TO ALTPRTL
041002     END-IF.
03011
03012      IF PRTOPTL GREATER THAN 0
03013          IF PRTOPTI = 'L'
03014              GO TO 0400-CREATE-ELACTQ
03015          ELSE
03016              GO TO 0480-PRINT-NOW
041002         END-IF
041002     END-IF.
061013     move 'S'                    to mainti
061013     PERFORM 0500-CREATE-TEMP-STORAGE
061013                                 THRU 0599-EXIT
03018      IF PI-CERT-PROCESSED NOT LESS THAN PI-CERT-SELECT-CNT
03019          GO TO 8100-SEND-INITIAL-MAP
03020      ELSE
03021          MOVE LOW-VALUES         TO MAINTO
03022          MOVE -1                 TO MAINTL
03023          GO TO 8200-SEND-DATAONLY
041002     END-IF.
03024
03025      EJECT
101917 3991-CHECK-2-YEAR-CONTESTABLE.
101917     SET ELAPSED-BETWEEN-BIN TO TRUE
101917     MOVE ZERO               TO DC-ELAPSED-MONTHS
101917                                DC-ELAPSED-DAYS
101917
101917     MOVE PI-SAVE-EFFDT  TO DC-BIN-DATE-1.
101917     MOVE CL-INCURRED-DT TO DC-BIN-DATE-2.
101917     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
101917     IF DC-ODD-DAYS-OVER > ZERO
101917        ADD 1 TO DC-ELAPSED-MONTHS
101917     END-IF
101917
101917     IF DC-ELAPSED-MONTHS <= 24
101917        MOVE CL-REPORTED-DT TO DC-BIN-DATE-2
101917        MOVE ZERO           TO DC-ELAPSED-MONTHS
101917                               DC-ELAPSED-DAYS
101917        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
101917        IF DC-ODD-DAYS-OVER > ZERO
101917           ADD 1 TO DC-ELAPSED-MONTHS
101917        END-IF
101917        IF DC-ELAPSED-MONTHS > 24
101917           MOVE ER-1679            TO EMI-ERROR
101917           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
101917           PERFORM 3992-BUILD-TRAILER THRU 3992-EXIT
101917        END-IF
101917     END-IF.
101917
101917 3991-EXIT.
101917     EXIT.
101917 3992-BUILD-TRAILER.
101917
101917     MOVE SPACES             TO AT-GENERAL-INFO-TR
101917     INITIALIZE AT-GENERAL-INFO-TR
101917     MOVE ELMSTR-KEY         TO AT-CONTROL-PRIMARY
101917     MOVE 'AT'               TO AT-RECORD-ID
090821     evaluate true
090821        when emi-error = er-1679
090821           move er-1679-text     to at-info-line-1
090821        when emi-error = er-1682
090821           move er-1682-text     to at-info-line-1
090821        when emi-error = er-1683
090821           move er-1683-text     to at-info-line-1
090821     end-evaluate
090821     move +97                to at-sequence-no
061418*     IF EMI-ERROR = ER-7572
061418*        MOVE +97             TO AT-SEQUENCE-NO
061418*     ELSE
101917*        MOVE +96             TO AT-SEQUENCE-NO
061418*     END-IF
101917     MOVE '6'                TO AT-TRAILER-TYPE
101917     MOVE WS-TODAY-DT        TO AT-RECORDED-DT
101917                                AT-GEN-INFO-LAST-MAINT-DT
101917     MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY
101917                                AT-GEN-INFO-LAST-UPDATED-BY
101917     MOVE EIBTIME            TO AT-LAST-MAINT-HHMMSS
101917
061418*     IF EMI-ERROR = ER-7572
061418*        MOVE WS-FILING-NOTE TO AT-INFO-LINE-1
061418*     ELSE
101917*        MOVE EMI-LINE2 TO AT-INFO-LINE-1
061418*     END-IF
101917     MOVE SPACES             TO AT-INFO-LINE-2.
101917
101917 3992-WRITE.
101917
101917     
      * EXEC CICS HANDLE CONDITION
101917*        DUPREC    (3992-DUPREC)
101917*    END-EXEC.
      *    MOVE '"$%                   ! ; #00010316' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303130333136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
101917
101917     
      * EXEC CICS WRITE
101917*         DATASET     ('ELTRLR')
101917*         FROM        (ACTIVITY-TRAILERS)
101917*         RIDFLD      (AT-CONTROL-PRIMARY)
101917*     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00010320' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
101917
101917     GO TO 3992-EXIT.
101917
101917 3992-DUPREC.
101917     SUBTRACT +1 FROM AT-SEQUENCE-NO.
101917     GO TO 3992-WRITE.
101917
101917 3992-EXIT.
101917     EXIT.
03026  3993-MODIFY-SCREEN-ATTRB.
03027
03028      IF CERTMTL GREATER ZERO
03029          MOVE AL-UANON           TO CERTMTA
041002     END-IF.
03030
03031      IF EFFDTL GREATER ZERO
03032          MOVE AL-UANON           TO EFFDTA
041002     END-IF.
03033
03034      IF ACCOUNTL GREATER ZERO
03035          MOVE AL-UANON           TO ACCOUNTA
041002     END-IF.
03036
03037      IF STATEL GREATER ZERO
03038          MOVE AL-UANON           TO STATEA
041002     END-IF.
03039
03040      IF CRTCARRL GREATER ZERO
03041          MOVE AL-UANON           TO CRTCARRA
041002     END-IF.
03042
03043      IF GROUPL GREATER ZERO
03044          MOVE AL-UANON           TO GROUPA
041002     END-IF.
03045
03046      IF CRTLNMEL GREATER ZERO
03047          MOVE AL-UANON           TO CRTLNMEA
041002     END-IF.
03048
03049      IF CRTFNMEL GREATER ZERO
03050          MOVE AL-UANON           TO CRTFNMEA
041002     END-IF.
03051
03052      IF CRTINITL GREATER ZERO
03053          MOVE AL-UANON           TO CRTINITA
041002     END-IF.
03054
03055      IF ISSAGEL GREATER ZERO
03056          MOVE AL-UNNON           TO ISSAGEA
041002     END-IF.
03057
03058      IF JNTLNMEL GREATER ZERO
03059          MOVE AL-UANON           TO JNTLNMEA
041002     END-IF.
03060
03061      IF JNTFNMEL GREATER ZERO
03062          MOVE AL-UANON           TO JNTFNMEA
041002     END-IF.
03063
03064      IF JNTINITL GREATER ZERO
03065          MOVE AL-UANON           TO JNTINITA
041002     END-IF.
03066
03067      IF JNTAGEL GREATER ZERO
03068          MOVE AL-UNNON           TO JNTAGEA
041002     END-IF.
03069
03070      IF CRTSSNL GREATER ZERO
03071          MOVE AL-UANON           TO CRTSSNA
041002     END-IF.
03072
03073      IF ACVDSCRL GREATER ZERO
03074          MOVE AL-SANON           TO ACVDSCRA
041002     END-IF.
03075
03076      IF ACVKINDL GREATER ZERO
03077          MOVE AL-SANON           TO ACVKINDA
041002     END-IF.
03078
03079      IF ACVCDL GREATER ZERO
03080          MOVE AL-UANON           TO ACVCDA
041002     END-IF.
03081
03082      IF ACVOTRML GREATER ZERO
03083          MOVE AL-UNNON           TO ACVOTRMA
041002     END-IF.
03084
03085      IF ACVRTRML GREATER ZERO
03086          MOVE AL-UNNON           TO ACVRTRMA
041002     END-IF.
03087
03088      IF ACVRATEL GREATER ZERO
03089          MOVE AL-UNNON           TO ACVRATEA
041002     END-IF.
03090
03091      IF ACVBENEL GREATER ZERO
03092          MOVE AL-UNNON           TO ACVBENEA
041002     END-IF.
03093
03094      IF ACVFORML GREATER ZERO
03095          MOVE AL-UANON           TO ACVFORMA
041002     END-IF.
03096
03097      IF ACVCNDTL GREATER ZERO
03098          MOVE AL-UANON           TO ACVCNDTA
041002     END-IF.
03099
03100      IF ACVEXITL GREATER ZERO
03101          MOVE AL-UANON           TO ACVEXITA
041002     END-IF.
03102
03103      IF ACVSTATL GREATER ZERO
03104          MOVE AL-UANON           TO ACVSTATA
041002     END-IF.
03105
03106      IF LCVDSCRL GREATER ZERO
03107          MOVE AL-SANON           TO LCVDSCRA
041002     END-IF.
03108
03109      IF LCVKINDL GREATER ZERO
03110          MOVE AL-SANON           TO LCVKINDA
041002     END-IF.
03111
03112      IF LCVCDL GREATER ZERO
03113          MOVE AL-UANON           TO LCVCDA
041002     END-IF.
03114
03115      IF LCVOTRML GREATER ZERO
03116          MOVE AL-UNNON           TO LCVOTRMA
041002     END-IF.
03117
03118      IF LCVRTRML GREATER ZERO
03119          MOVE AL-UNNON           TO LCVRTRMA
041002     END-IF.
03120
03121      IF LCVRATEL GREATER ZERO
03122          MOVE AL-UNNON           TO LCVRATEA
041002     END-IF.
03123
03124      IF LCVBENEL GREATER ZERO
03125          MOVE AL-UNNON           TO LCVBENEA
041002     END-IF.
03126
03127      IF LCVFORML GREATER ZERO
03128          MOVE AL-UANON           TO LCVFORMA
041002     END-IF.
03129
03130      IF LCVCNDTL GREATER ZERO
03131          MOVE AL-UANON           TO LCVCNDTA
041002     END-IF.
03132
03133      IF LCVEXITL GREATER ZERO
03134          MOVE AL-UANON           TO LCVEXITA
041002     END-IF.
03135
03136      IF LCVSTATL GREATER ZERO
03137          MOVE AL-UANON           TO LCVSTATA
041002     END-IF.
03138
03139      IF MEMCAPL GREATER ZERO
03140          MOVE AL-UANON           TO MEMCAPA
041002     END-IF.
03141
03142      IF APRL GREATER ZERO
03143          MOVE AL-UNNON           TO APRA
041002     END-IF.
03144
03145      IF PMTFREQL GREATER ZERO
03146          MOVE AL-UNNON           TO PMTFREQA
041002     END-IF.
03147
03148      IF INDGRPL GREATER ZERO
03149          MOVE AL-UANON           TO INDGRPA
041002     END-IF.
03150
03151      IF PREMTYPL GREATER ZERO
03152          MOVE AL-UANON           TO PREMTYPA
041002     END-IF.
03153
03154      IF REINCDL GREATER ZERO
03155          MOVE AL-UANON           TO REINCDA
041002     END-IF.
03156
03157      IF ADDONDTL GREATER ZERO
03158          MOVE AL-UANON           TO ADDONDTA
041002     END-IF.
03159
03160      IF MEMBERL GREATER ZERO
03161          MOVE AL-UANON           TO MEMBERA
041002     END-IF.
03162
03163      IF BCERT1L GREATER ZERO
03164          MOVE AL-SANON           TO BCERT1A
03165                                     BSUFX1A
041002     END-IF.
03166
03167      IF BCERT2L GREATER ZERO
03168          MOVE AL-SANON           TO BCERT2A
03169                                     BSUFX2A
041002     END-IF.
03170
03171      IF BCERT3L GREATER ZERO
03172          MOVE AL-SANON           TO BCERT3A
03173                                     BSUFX3A
041002     END-IF.
03174
03175      IF BCERT4L GREATER ZERO
03176          MOVE AL-SANON           TO BCERT4A
03177                                     BSUFX4A
041002     END-IF.
03178
03179      IF BCERT5L GREATER ZERO
03180          MOVE AL-SANON           TO BCERT5A
03181                                     BSUFX5A
041002     END-IF.
03182
03183  3993-MODIFY-CLAIM-ATTRB.
03184
03185      IF PCERTNOL GREATER ZERO
03186          MOVE AL-UANON           TO PCERTNOA
041002     END-IF.
03187
03188      IF PSUFXL GREATER ZERO
03189          MOVE AL-UANON           TO PSUFXA
041002     END-IF.
03190
03191      IF LSTNMEL GREATER ZERO
03192          MOVE AL-UANON           TO LSTNMEA
041002     END-IF.
03193
03194      IF FSTNMEL GREATER ZERO
03195          MOVE AL-UANON           TO FSTNMEA
041002     END-IF.
03196
03197      IF INITL GREATER ZERO
03198          MOVE AL-UANON           TO INITA
041002     END-IF.
03199
03200      IF SEXL GREATER ZERO
03201          MOVE AL-UANON           TO SEXA
041002     END-IF.
03202
03206      IF BIRTHDTL GREATER ZERO
03207          MOVE AL-UANON           TO BIRTHDTA
041002     END-IF.
03208
03209      IF SSNL GREATER ZERO
03210          MOVE AL-UANON           TO SSNA
041002     END-IF.
03211
03212      IF INCURL GREATER ZERO
03213          MOVE AL-UANON           TO INCURA
041002     END-IF.
03214
03215      IF REPORTL GREATER ZERO
03216          MOVE AL-UANON           TO REPORTA
041002     END-IF.
03217
040714*    IF ESTENDL GREATER ZERO
040714*        MOVE AL-UANON           TO ESTENDA
040714*    END-IF.
03220
03221      IF DIAGL GREATER ZERO
03222          MOVE AL-UANON           TO DIAGA
041002     END-IF.
040714*
040814     IF ICD1L GREATER ZERO
040814         MOVE AL-UANON           TO ICD1A
040814     END-IF.
040814
040814     IF ICD2L GREATER ZERO
040814         MOVE AL-UANON           TO ICD2A
040814     END-IF.
03223
040714*    IF CAUSEL GREATER ZERO
040714*        MOVE AL-UANON           TO CAUSEA
040714*    END-IF.
03226
03227      IF MANRSVL GREATER ZERO
03228          MOVE AL-UNNON           TO MANRSVA
041002     END-IF.
03229
03230      IF RELCLML GREATER ZERO
03231          MOVE AL-UANON           TO RELCLMA
041002     END-IF.
03232
03233      IF LOANNOL GREATER ZERO
03234          MOVE AL-UANON           TO LOANNOA
041002     END-IF.
03235
03236      IF LOANBALL GREATER ZERO
03237          MOVE AL-UNNON           TO LOANBALA
041002     END-IF.
03238
03239      IF PROCCDL GREATER ZERO
03240          MOVE AL-UANON           TO PROCCDA
041002     END-IF.
03241
03242      IF BENECDL GREATER THAN ZERO
03243          MOVE AL-UANON           TO  BENECDA
041002     END-IF.
03244
03245      IF PRICDL GREATER ZERO
03246          MOVE AL-UANON           TO PRICDA
041002     END-IF.
03247
03252      IF SUPVL GREATER ZERO
03253          MOVE AL-UANON           TO SUPVA
041002     END-IF.
03254
03255      IF FILETOL GREATER ZERO
03256          MOVE AL-UANON           TO FILETOA
041002     END-IF.
03257
03258
03259  3993-MODIFY-CLAIM-KEY-ATTRB.
03260
03261      IF CLMNOL GREATER ZERO
03262          MOVE AL-UANON           TO CLMNOA
041002     END-IF.
03263
03264      IF CLMCARRL GREATER ZERO
03265          MOVE AL-UANON           TO CLMCARRA
041002     END-IF.
03266
03267      IF CLMTYPEL GREATER ZERO
03268          MOVE AL-UANON           TO CLMTYPEA
041002     END-IF.
03269
PEMMOD     IF CERTNOL GREATER ZERO
PEMMOD         MOVE AL-SANON           TO CERTNOA
041002     END-IF.
03272
03273      IF SUFXL GREATER ZERO
PEMMOD         MOVE AL-SANON           TO SUFXA
041002     END-IF.
03275
03276  3993-EXIT.
03277      EXIT.
03278
03279      EJECT
061511 3994-BUILD-NINETY-THREE-TRAILER.
061511
061511     MOVE SPACES                 TO ACTIVITY-TRAILERS.
061511     MOVE 'AT'                   TO AT-RECORD-ID.
061511
061511     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
061511     MOVE CLMCARRI               TO AT-CARRIER.
061511
061511     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
061511     MOVE CERT-CERT-NO           TO AT-CERT-NO.
061511     MOVE +93                    TO AT-SEQUENCE-NO.
061511     MOVE '6'                    TO AT-TRAILER-TYPE.
061511     MOVE 'M'                    TO AT-INFO-TRAILER-TYPE.
061511
100518     IF CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'
032514         MOVE WS-VERIFY-NOTE     TO AT-INFO-LINE-1
032514     ELSE
032514         MOVE WS-VFY-SSN-NOTE    TO AT-INFO-LINE-1
032514     END-IF
061511
061511     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
061511                                    AT-GEN-INFO-LAST-MAINT-DT.
061511
061511     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
061511                                    AT-GEN-INFO-LAST-UPDATED-BY.
061511     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
061511
061511
061511     
      * EXEC CICS WRITE
061511*        DATASET   (ELTRLR-DSID)
061511*        FROM      (ACTIVITY-TRAILERS)
061511*        RIDFLD    (AT-CONTROL-PRIMARY)
061511*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010694' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130363934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
061511
061511 3994-EXIT.
061511     EXIT.
061511
020613 3994B-BUILD-NINETY-FOUR-TRAILER.
020613
020613     MOVE SPACES                 TO ACTIVITY-TRAILERS.
020613     MOVE 'AT'                   TO AT-RECORD-ID.
020613
020613     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
020613     MOVE CLMCARRI               TO AT-CARRIER.
020613
020613     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
020613     MOVE CERT-CERT-NO           TO AT-CERT-NO.
020613     MOVE +94                    TO AT-SEQUENCE-NO.
020613     MOVE '6'                    TO AT-TRAILER-TYPE.
020613     MOVE 'M'                    TO AT-INFO-TRAILER-TYPE.
020613
020613     MOVE WS-CAUSAL-NOTE         TO AT-INFO-LINE-1.
020613
020613     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
020613                                    AT-GEN-INFO-LAST-MAINT-DT.
020613
020613     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
020613                                    AT-GEN-INFO-LAST-UPDATED-BY.
020613     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
020613
020613
020613     
      * EXEC CICS WRITE
020613*        DATASET   (ELTRLR-DSID)
020613*        FROM      (ACTIVITY-TRAILERS)
020613*        RIDFLD    (AT-CONTROL-PRIMARY)
020613*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010727' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
020613
020613 3994B-EXIT.
020613     EXIT.
061013 3994C-BUILD-NINETY-FIVE-TRAILER.
061013
061013     MOVE SPACES                 TO ACTIVITY-TRAILERS
061013     MOVE 'AT'                   TO AT-RECORD-ID
061013
061013     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD
061013     MOVE CLMCARRI               TO AT-CARRIER
061013
061013     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO
061013     MOVE CERT-CERT-NO           TO AT-CERT-NO
061013     MOVE +95                    TO AT-SEQUENCE-NO
061013     MOVE '6'                    TO AT-TRAILER-TYPE
061013     MOVE 'E'                    TO AT-INFO-TRAILER-TYPE
061013
061013     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
061013                                    AT-GEN-INFO-LAST-MAINT-DT
061013
061013     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
061013                                    AT-GEN-INFO-LAST-UPDATED-BY
061013     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
061013
061013     perform varying e1 from +1 by +1 until
061013        ws-error-no (e1) = spaces
061013        move ws-error-no (e1)    to at-note-error-no (e1)
061013     end-perform
061013
061013     if at-info-line-1 not = spaces
061013        
      * EXEC CICS WRITE
061013*          DATASET  (ELTRLR-DSID)
061013*          FROM     (ACTIVITY-TRAILERS)
061013*          RIDFLD   (AT-CONTROL-PRIMARY)
061013*       END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010762' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130373632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013     end-if
061013
061013     .
061013 3994C-EXIT.
061013     EXIT.
03280  3995-BUILD-CLAIM-RECORD.
03282      IF PROCCDL GREATER ZERO
03283          MOVE PROCCDI            TO CL-PROCESSOR-ID
03284      ELSE
03285          MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID
041002     END-IF.
03286
03287      IF FSTNMEL GREATER ZERO
03288          MOVE FSTNMEI            TO CL-INSURED-1ST-NAME
041002     END-IF.
03289
03290      IF INITL GREATER ZERO
03291          MOVE INITI              TO CL-INSURED-MID-INIT
041002     END-IF.
03292
03293      IF BIRTHDTL GREATER ZERO
03294          MOVE WS-BIRTHDT         TO CL-INSURED-BIRTH-DT
041002     END-IF.
03295
03296      IF SEXL GREATER ZERO
03297          MOVE SEXI               TO CL-INSURED-SEX-CD
041002     END-IF.
03298
03302      MOVE 'O'                    TO CL-CLAIM-STATUS.
03303
041002*    IF CLMTYPEL GREATER ZERO
041002     MOVE CLMTYPEI               TO CL-CLAIM-TYPE.
041002*    END-IF.
03306
03307      IF PREMTYPL GREATER ZERO
03308          MOVE PREMTYPI           TO CL-CLAIM-PREM-TYPE
041002     END-IF.
03309
03310      IF INCURL GREATER ZERO
03311          MOVE WS-INCUR           TO CL-INCURRED-DT
041002     END-IF.
03312
020816     IF (PI-COMPANY-ID = 'DCC' OR 'VPP')
061013        AND (ERPDEF-FOUND)
061013        MOVE WS-MAX-BENEFITS     TO CL-CRITICAL-PERIOD
061013*       MOVE WS-CRIT-PER-RECURRENT
061013*                                TO CL-CRIT-PER-RECURRENT
061013*       MOVE WS-CRIT-PER-RTW-MOS TO CL-CRIT-PER-RTW-MOS
              if ws-crit-per-recurrent > 01
                 move zeros            to cl-benefit-period
              end-if
061013     ELSE
100518        IF CL-CLAIM-TYPE = 'L' OR 'P' OR 'O'
061013           MOVE CM-LF-CRITICAL-PERIOD
061013                                 TO CL-CRITICAL-PERIOD
061013        ELSE
061013           MOVE CM-AH-CRITICAL-PERIOD
061013                                 TO CL-CRITICAL-PERIOD
061013        END-IF
061013     END-IF
061013
061013     IF CL-CRITICAL-PERIOD NOT = ZEROS
061013        COMPUTE DC-ELAPSED-MONTHS =
061013           CL-CRITICAL-PERIOD - WS-BENEFITS-PREV-PAID
061013        MOVE CL-INCURRED-DT      TO DC-BIN-DATE-1
061013        MOVE ZEROS               TO DC-ELAPSED-DAYS
061013        MOVE '6'                 TO DC-OPTION-CODE
061013        PERFORM 9700-LINK-DATE-CONVERT
061013                                 THRU 9700-EXIT
061013        MOVE DC-BIN-DATE-2       TO CL-BENEFIT-EXPIRATION-DT
061013     END-IF
061013
061013     if instypel > zero
061013        move instypei            to cl-insured-type
061013     end-if
03313      IF REPORTL GREATER ZERO
03314          MOVE WS-REPORT          TO CL-REPORTED-DT
041002     END-IF.
03315
03316      IF EIBAID = DFHPF6
03317          IF WS-REPORT = LOW-VALUES  OR   REPORTL = ZEROS
03318              MOVE WS-TODAY-DT    TO CL-REPORTED-DT
03319                  IF EMI-FORCABLE-CTR = +1
03320                      MOVE ZEROS  TO CL-FORCEABLE-ERROR-CNT
041002                 END-IF
041002         END-IF
041002     END-IF.
03321
040814*    IF ESTENDL GREATER ZERO
040814*       MOVE WS-ESTEND           TO CL-EST-END-OF-DISAB-DT
040814*    END-IF.
03324
03325      IF ADDONDTL GREATER ZERO
03329          MOVE WS-ADD-ON-DT   TO CL-LAST-ADD-ON-DT
041002     END-IF.
03330
040814*    IF CAUSEL GREATER ZERO
040814*        MOVE CAUSEI             TO CL-CAUSE-CD
040814*    END-IF.
03333
03334      IF SSNL GREATER ZERO
03335          MOVE SSNI               TO CL-SOC-SEC-NO
03336      ELSE
03337          MOVE CERT-STATE         TO CL-SSN-STATE
03338          MOVE CERT-ACCOUNT-PRIME TO CL-SSN-ACCOUNT
03339          MOVE CL-INSURED-LAST-NAME TO CL-SSN-LN3
041002     END-IF.
03340
03341      IF PRICDL GREATER ZERO
03342          MOVE PRICDI             TO CL-PRIORITY-CD
041002     END-IF.
03343
03348      IF SUPVL GREATER ZERO
03349          MOVE SUPVI              TO CL-SUPV-ATTN-CD
041002     END-IF.
03350
03351      MOVE WS-TODAY-DT            TO CL-LAST-MAINT-DT.
03352      MOVE CL-PROCESSOR-ID        TO CL-LAST-MAINT-USER.
03353      MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.
03354
03355      IF RELCLML GREATER ZERO
03356          MOVE RELCLMI            TO CL-RELATED-CLAIM-NO
041002     END-IF.
03357
03358      IF FILETOL GREATER ZERO
03359          MOVE FILETOI            TO CL-FILE-LOCATION
041002     END-IF.
03360
03361  3995-EXIT.
03362       EXIT.
061013 3996-read-cert-claim-trailer.
061013
061013     move cm-ah-benefit-amt      to ws-monthly-benefit
061013     if (pi-dcc-max-amt < ws-monthly-benefit)
              and (pi-dcc-max-amt not = zeros)
061013        move pi-dcc-max-amt      to ws-monthly-benefit
061013     end-if
061013
061013     MOVE CM-COMPANY-CD          TO CTRLR-COMP-CD
061013     MOVE CM-CARRIER             TO CTRLR-CARRIER
061013     MOVE CM-GROUPING            TO CTRLR-GROUPING
061013     MOVE CM-STATE               TO CTRLR-STATE
061013     MOVE CM-ACCOUNT             TO CTRLR-ACCOUNT
061013     MOVE CM-CERT-EFF-DT         TO CTRLR-EFF-DT
061013     MOVE CM-CERT-NO             TO CTRLR-CERT-NO
061013     MOVE 'B'                    TO CTRLR-REC-TYPE
061013     
      * EXEC CICS READ
061013*       DATASET  (ELCRTT-DSID)
061013*       set     (address of CERTIFICATE-TRAILERS)
061013*       RIDFLD   (ELCRTT-KEY)
061013*       RESP     (WS-RESPONSE)
061013*    END-EXEC
      *    MOVE '&"S        E          (  N#00010913' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303130393133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
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
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This may need some work here. I think I should accumulate ***
      ***  all the claims with the same claim type?????????          ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
061013     IF WS-RESP-NORMAL
              move zeros to ws-tot-days-paid ws-tot-amt-paid
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cs-claim-no (s1) = spaces)
                 if cs-claim-type (s1) = clmtypei
                    move er-1659       to ws-error-no (e1)
                    compute ws-tot-days-paid =
                       ws-tot-days-paid + cs-days-paid (s1)
                    compute ws-tot-amt-paid =
                       ws-tot-amt-paid + cs-total-paid (s1)
                 end-if
              end-perform
      *        compute ws-pd-bens rounded = ws-tot-days-paid / 30
      *        if (ws-pd-bens >= pi-dcc-max-benefits)
      *           and (pi-dcc-max-benefits not = zeros)
      *           add +1                to e1
      *           MOVE ER-1660          TO EMI-ERROR
      *                                    ws-error-no (e1)
      *           PERFORM 9900-ERROR-FORMAT
      *                                 THRU 9900-EXIT
      *        end-if
           end-if
061013**       if instypei = 'P'
061013**          move +1 to s1
061013**       else
061013**          MOVE +2 to s1
061013**       end-if
061013*
061013*        move +1 to s1
061013*        evaluate clmtypei
061013*           when 'A'
061013*              move +1 to s2
061013*           when 'I'
061013*              move +2 to s2
061013*           when 'G'
061013*              move +3 to s2
061013*           when 'L'
061013*              move +4 to s2
061013*           when 'P'
061013*              move +5 to s2
061013*        end-evaluate
061013*        if cs-claim-type (s1 s2) = clmtypei
061013*           if cs-no-of-claims (s1 s2) > zeros
061013*              move er-1659       to ws-error-no (e1)
061013*           end-if
061013*           compute WS-BENEFITS-PREV-PAID =
061013*              cs-total-paid (s1 s2) / ws-monthly-benefit
061013*           if (WS-BENEFITS-PREV-PAID >= pi-dcc-max-benefits)
061013*              and (pi-dcc-max-benefits not = zeros)
061013*              add +1             to e1
061013*              MOVE ER-1660       TO EMI-ERROR
061013*                                    ws-error-no (e1)
061013*              PERFORM 9900-ERROR-FORMAT
061013*                                 THRU 9900-EXIT
061013*           end-if
061013*        end-if
061013*     end-if
061013
061013     .
061013 3996-exit.
061013     exit.
061013
061013 3997-GET-ERPDEF.
061013
           if cm-clp-state = spaces or low-values or zeros
              move cm-state            to cm-clp-state
           end-if
061013     MOVE SPACES                 TO WS-ERPDEF-SW
061013     MOVE ZEROS                  TO WS-MAX-BENEFITS
061013                                    WS-CRIT-PER-RTW-MOS
                                          WS-CRIT-PER-RECURRENT
061013
061013     MOVE PI-COMPANY-CD       TO ERPDEF-KEY
061013     MOVE CM-clp-state        TO ERPDEF-STATE
061013     MOVE ws-dcc-product-code TO ERPDEF-PROD-CD
070714***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
070714***                                                            ***
070714***  If they set up a claim where there is no coverage on the  ***
070714***  addendum with the intention of denying.  We need to do    ***
070714***  some funky stuff here.                                    ***
070714***                                                            ***
070714***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
070714
070714     evaluate true
070714        when (clmtypei = 'L' or 'P')
070714           and (cm-lf-benefit-cd not = '00' and '  ' and 'DD'
070714              and 'CU')
070714           move 'L'              to erpdef-ben-type
070714           move cm-lf-benefit-cd to erpdef-ben-code
070714        when (clmtypei not = 'L' and 'P')
070714           and (cm-ah-benefit-cd not = '00' and '  ')
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714        when (clmtypei not = 'L' and 'P')
070714           and (cm-ah-benefit-cd = '00' or '  ')
070714           move 'L'              to erpdef-ben-type
070714           move cm-lf-benefit-cd to erpdef-ben-code
070714        when (clmtypei = 'L' or 'P')
070714           and (cm-lf-benefit-cd = '00' or '  ' or 'DD' or 'CU')
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714        when other
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714     end-evaluate
022122     move cl-insured-birth-dt    to dc-bin-date-1
022122     move cl-incurred-dt         to dc-bin-date-2
022122     move '1'                    to dc-option-code
022122     PERFORM 9700-LINK-DATE-CONVERT
022122                                 THRU 9700-EXIT
022122     compute ws-att-age =
022122        dc-elapsed-months / 12
022122
022122     move zeros                  to dc-elapsed-months
022122                                    dc-elapsed-days
022122     move low-values to dc-bin-date-1 dc-bin-date-2
061013*    MOVE 'A'                 TO ERPDEF-BEN-TYPE
061013*    MOVE CM-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
061013     MOVE CM-CERT-EFF-DT      TO ERPDEF-EXP-DT
061013     MOVE ERPDEF-KEY          TO ERPDEF-KEY-SAVE
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
      *    MOVE '&,         G          &  N#00011047' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303131303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     IF WS-RESP-NORMAL
061013        
      * EXEC CICS READNEXT
061013*          DATASET  ('ERPDEF')
061013*          INTO     (PRODUCT-MASTER)
061013*          RIDFLD   (ERPDEF-KEY)
061013*          RESP     (WS-RESPONSE)
061013*       END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV12
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00011055' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303131303535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PRODUCT-MASTER, 
                 DFHEIV12, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013        IF WS-RESP-NORMAL
061013           IF (ERPDEF-KEY-SAVE (1:16) =
061013              PD-CONTROL-PRIMARY (1:16))
061013              AND (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
061013
061013              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
022122                 (A1 > +11)
061013                 OR ((PD-PROD-CODE (A1) = CLMTYPEI)
PEMTST                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
061013              END-PERFORM
022122              IF A1 < +12
                       SET ERPDEF-FOUND TO TRUE
061013                 move pd-max-amt (a1)
061013                              to pi-dcc-max-amt
061013                 MOVE PD-CRIT-PERIOD (A1)
061013                              TO WS-MAX-BENEFITS
061013                                 pi-dcc-max-benefits
                       if pd-rec-crit-period (a1) not numeric
                          move 01      to pd-rec-crit-period (a1)
                       end-if
061013                 MOVE PD-REC-CRIT-PERIOD (A1)
061013                              TO WS-CRIT-PER-RECURRENT
061013                 IF PD-RTW-MOS (A1) NUMERIC
061013                    MOVE PD-RTW-MOS (A1)
061013                              TO WS-CRIT-PER-RTW-MOS
061013                 ELSE
061013                    MOVE 0    TO WS-CRIT-PER-RTW-MOS
061013                 END-IF
061013                 IF PD-EXCLUSION-PERIOD-DAYS (A1) NUMERIC
061013                    MOVE PD-EXCLUSION-PERIOD-DAYS (A1)
061013                                 TO WS-EXCL-PERIOD
061013                 END-IF
061013                 IF PD-COVERAGE-ENDS-MOS (A1) NUMERIC
061013                    MOVE PD-COVERAGE-ENDS-MOS (A1)
061013                                 TO WS-COV-ENDS
061013                 END-IF
061013                 IF PD-ACCIDENT-ONLY-MOS (A1) NUMERIC
061013                    MOVE PD-ACCIDENT-ONLY-MOS (A1)
061013                                 TO WS-ACC-PERIOD
061013                 END-IF
011118                 IF PD-PRE-EXIST-EXCL-TYPE (A1) NUMERIC
011118                    MOVE PD-PRE-EXIST-EXCL-TYPE (A1)
011118                                 TO WS-PRE-EXISTING-PER
011118                 END-IF
                    else
070714                 MOVE -1         TO MAINTL
                       move er-1673    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013              END-IF
                 else
                    MOVE -1            TO MAINTL
                    move er-1671       to emi-error
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013           END-IF
061013        END-IF
061013     END-IF
061013
061013     .
061013 3997-EXIT.
061013     EXIT.
061013
061013 3998-update-cert-claim-trailer.
061013
061013     
      * EXEC CICS GETMAIN
061013*       SET      (ADDRESS OF CERTIFICATE-TRAILERS)
061013*       LENGTH   (ELCRTT-LENGTH)
061013*       INITIMG  (GETMAIN-SPACE)
061013*    END-EXEC
      *    MOVE ',"IL                  $   #00011127' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCRTT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     MOVE CM-COMPANY-CD    TO CTRLR-COMP-CD
061013     MOVE CM-CARRIER       TO CTRLR-CARRIER
061013     MOVE CM-GROUPING      TO CTRLR-GROUPING
061013     MOVE CM-STATE         TO CTRLR-STATE
061013     MOVE CM-ACCOUNT       TO CTRLR-ACCOUNT
061013     MOVE CM-CERT-EFF-DT   TO CTRLR-EFF-DT
061013     MOVE CM-CERT-NO       TO CTRLR-CERT-NO
061013     MOVE 'B'              TO CTRLR-REC-TYPE
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
      *    MOVE '&"IL       EU         (  N#00011141' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303131313431' TO DFHEIV0(25:11)
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
061013     IF WS-RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cs-claim-no (s1) = spaces)
              end-perform
              if s1 < +25
061013           perform 3999-upd-crt-trlr thru 3999-exit
061013           
      * EXEC CICS REWRITE
061013*             DATASET  (ELCRTT-DSID)
061013*             from     (CERTIFICATE-TRAILERS)
061013*             RESP     (WS-RESPONSE)
061013*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00011155' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303131313535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              end-if
061013     else
061013        if ws-resp-notfnd
061013           move 'CS'             to certificate-trailers
061013           move cm-control-primary to cs-control-primary
061013           move 'B'              to cs-trailer-type
061013           perform varying s1 from +1 by +1 until s1 > +24
061013              move zeros         to cs-days-paid (s1)
061013                                    cs-total-paid (s1)
                                          cs-benefit-period (s1)
                                          cs-remaining-bens (s1)
061013           end-perform
                 move +1               to s1
061013           perform 3999-upd-crt-trlr
061013                                 thru 3999-exit
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
      *    MOVE '&$ L                  ''  N#00011175' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303131313735' TO DFHEIV0(25:11)
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
061013 3998-exit.
061013     exit.
061013
061013 3999-upd-crt-trlr.
           move clmnoi                 to cs-claim-no       (s1)
061013     move clmtypei               to cs-claim-type     (s1)
           move instypei               to cs-insured-type   (s1)
121417     if ws-crit-per-recurrent > 01
121417        move zeros               to cs-benefit-period(s1)
121417     else
121417        move 01                  to cs-benefit-period(s1)
121417     end-if
121417*    move cl-benefit-period      to cs-benefit-period (s1)
pemtst*    move cl-critical-period     to cs-remaining-bens (s1)
pemtst     move zeros                  to cs-remaining-bens (s1)
061013     .
061013 3999-exit.
061013     exit.
03365  4000-CALCULATE-CERT-TERM.
03366
03367      PERFORM 4100-CALC-GROSS-TERM THRU 4200-EXIT.
03368
03369      MOVE N                      TO WS-TERM.
03370
03371      IF WS-TERM GREATER 120
03372          MOVE 120                TO WS-TERM
041002     END-IF.
03373
03374  4000-CALC-TERM-EXIT.
03375       EXIT.
03376
03377  4100-CALC-GROSS-TERM.
03378      MOVE HOLD-LOAN-BAL           TO L.
03379      MOVE WS-ACVBENE              TO M.
03380      COMPUTE N = L / M.
03381      IF N LESS 1
03382          MOVE 1  TO N
041002     END-IF.
03383
03384      IF LCVCDL GREATER ZERO
03385          COMPUTE WS-LF-RATE = WS-LCVRATE / +1000
03386      ELSE
03387          MOVE ZEROS               TO WS-LF-RATE
041002     END-IF.
03388
03389      IF ACVCDL GREATER ZERO
03390          COMPUTE WS-AH-RATE = WS-ACVRATE / +1000
03391      ELSE
03392          MOVE ZEROS               TO WS-AH-RATE
041002     END-IF.
03393
03394      COMPUTE I = WS-APR / +1200.
03395
03396  4105-LOOP.
03397      IF N GREATER 240
03398          GO TO 4200-EXIT
041002     END-IF.
03399
03400      PERFORM 4210-CALC-LEFT-RIGHTONE THRU 4220-EXIT.
03401
03402      IF LEFT-TOT-1 GREATER RIGHT-TOT-1
03403          ADD 1 TO N
03404          GO TO 4105-LOOP
041002     END-IF.
03405
03406  4200-EXIT.
03407       EXIT.
03408
03409  4210-CALC-LEFT-RIGHTONE.
03410       MOVE L         TO LEFT-TOT-1.
03411       SUBTRACT 1 FROM N.
03412       PERFORM 4300-CALC-A-N THRU 4300-EXIT.
03413       PERFORM 4350-CALC-IA-N THRU 4400-EXIT.
03414       ADD 1 TO N.
03415
03416  4211-CALC-TERM1.
03417       MOVE N         TO TERM1.
03418       MULTIPLY M BY TERM1.
03419
03420  4212-LOOP.
03421       COMPUTE TERM1 = (WS-AH-RATE + WS-LF-RATE) * TERM1.
03422       ADD 1 TO A-N.
03423       MULTIPLY A-N BY TERM1.
03424       SUBTRACT 1 FROM A-N.
03425       ADD TERM1 TO LEFT-TOT-1.
03426
03427  4213-CALC-TERM2.
03428       MOVE M         TO TERM2.
03429       COMPUTE TERM2 = (WS-AH-RATE + WS-LF-RATE) * TERM2.
03430       MULTIPLY IA-N BY TERM2.
03431       SUBTRACT TERM2 FROM LEFT-TOT-1.
03432
03433  4215-CALC-RIGHTONE.
03434       MOVE M         TO RIGHT-TOT-1.
03435       PERFORM 4300-CALC-A-N THRU 4300-EXIT.
03436       MULTIPLY A-N BY RIGHT-TOT-1.
03437
03438  4220-EXIT.
03439       EXIT.
03440
03441  4300-CALC-A-N.
03442      IF N LESS 1
03443          MOVE 0     TO A-N
03444          GO TO 4300-EXIT
041002     END-IF.
03445
03446      IF I = 0
03447          MOVE .00001 TO I
041002     END-IF.
03448
03449      ADD 1 TO I.
03450      DIVIDE I INTO 1 GIVING V.
03451      SUBTRACT 1 FROM I.
03452      PERFORM 4450-CALC-V-EX-N THRU 4490-EXIT.
03453      SUBTRACT V-EX-N FROM 1 GIVING TERM3.
03454      DIVIDE I INTO TERM3 GIVING A-N.
03455
03456  4300-EXIT.
03457       EXIT.
03458
03459  4350-CALC-IA-N.
03460      IF N LESS 1
03461          MOVE 0      TO IA-N
03462          GO TO 4400-EXIT
041002     END-IF.
03463
03464      ADD 1 TO N.
03465      PERFORM 4450-CALC-V-EX-N THRU 4490-EXIT.
03466      SUBTRACT 1 FROM N.
03467      MULTIPLY N BY V-EX-N GIVING TERM3.
03468      SUBTRACT TERM3 FROM A-N GIVING TERM3.
03469      SUBTRACT V FROM 1 GIVING TERM4.
03470      DIVIDE TERM4 INTO TERM3 GIVING IA-N.
03471
03472  4400-EXIT.
03473       EXIT.
03474
03475  4450-CALC-V-EX-N.
03476      IF N LESS 1
03477          MOVE 1    TO V-EX-N
03478          GO TO 4490-EXIT
041002     END-IF.
03479
03480      MOVE N        TO NV-STORE.
03481
03482      IF V-EX-ONETIME = 1  OR
03483         V NOT = V-EXPONENT (1)
03484           PERFORM 4470-BUILD-V-EX-TABLE THRU 4480-EXIT
041002     END-IF.
03485
03486  4460-LOOP.
03487      IF N GREATER 248
03488          MOVE 248 TO N
041002     END-IF.
03489
03490      IF N = 1
03491          MOVE V               TO V-EX-N
03492      ELSE
03493          MOVE V-EXPONENT (N)  TO V-EX-N
041002     END-IF.
03494
03495      GO TO 4490-EXIT.
03496
03497  4470-BUILD-V-EX-TABLE.
03498      MOVE 2      TO N.
03499      MOVE V      TO V-EXPONENT (1)
03500                     V-EX-N.
03501
03502  4471-LOOP.
03503      MULTIPLY V BY V-EX-N.
03504      MOVE V-EX-N   TO V-EXPONENT (N).
03505
03506      ADD 1 TO N.
03507      IF N LESS 248
03508          GO TO 4471-LOOP
041002     END-IF.
03509
03510      MOVE NV-STORE     TO N.
03511      MOVE ZERO         TO V-EX-ONETIME.
03512
03513  4480-EXIT.
03514       EXIT.
03515
03516  4490-EXIT.
03517       EXIT.
03518
03519  4500-CALC-NET-TERM.
03520      MOVE HOLD-LOAN-BAL           TO L.
03521      MOVE WS-ACVBENE              TO M.
03522      DIVIDE L BY M GIVING N REMAINDER WS-REMAIN.
03523
03524      IF WS-REMAIN GREATER ZERO
03525          ADD 1 TO N
041002     END-IF.
03526
03527      IF N = 0
03528          MOVE 1 TO N
041002     END-IF.
03529
03530  4700-EXIT.
03531       EXIT.
03532
03533  4999-CALC-TERM-EXIT.
03534      EXIT.
03535
03536      EJECT
03537  5000-INCURRED-CHANGE.
03538      IF NOT MODIFY-CAP
03539          MOVE ER-0070            TO EMI-ERROR
03540          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03541          MOVE -1                 TO MAINTL
03542          GO TO 8200-SEND-DATAONLY
041002     END-IF.
03543
03544      IF CLMNOI       NOT = PI-LAST-CLAIM       OR
03545         CLMCARRI     NOT = PI-LAST-CARR        OR
03546         CERTNOI      NOT = PI-LAST-CERT-PRIME  OR
03547         SUFXI        NOT = PI-LAST-CERT-SUFX   OR
03548         PI-INCURR-SW NOT = 'Y'
03549           GO TO 1000-SHOW-CLAIM
041002     END-IF.
03550
03551      PERFORM 6000-EDIT-CLAIM-DATA THRU 6000-EXIT.
03552      PERFORM 6200-EDIT-CERT-DATA  THRU 6200-EXIT.
03553      PERFORM 6400-TEST-CLAIM-REASONABILITY THRU 6499-EXIT.
03554
03555      IF EMI-NO-ERRORS
03556          GO TO 5000-TRY-TO-BUILD
041002     END-IF.
03557
03558      IF EIBAID = DFHPF6  AND NOT FORCE-CAP
03559         MOVE ER-0433             TO EMI-ERROR
03560         MOVE -1                  TO MAINTL
03561         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03562         GO TO 8200-SEND-DATAONLY
041002     END-IF.
03563
03564      IF EIBAID = DFHPF6
03565          IF EMI-FATAL-CTR NOT EQUAL ZERO
03566             MOVE ER-0434         TO EMI-ERROR
03567             MOVE -1              TO MAINTL
03568             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03569             GO TO 8200-SEND-DATAONLY
03570          ELSE
03571             MOVE +1              TO INCURL
03572             GO TO 5000-TRY-TO-BUILD
041002         END-IF
041002     END-IF.
03573
03574      GO TO 8200-SEND-DATAONLY.
03575
03576  5000-TRY-TO-BUILD.
03577
03578      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
03579
03580      MOVE CLMCARRI               TO MSTR-CARRIER.
03581
03582      MOVE CLMNOI                 TO MSTR-CLAIM-NO.
03583      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
03584      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
03585
03586      PERFORM 7920-READ-CLAIM-UPDATE THRU 7920-EXIT.
03587
03588      IF CL-CLAIM-PAYMENT-STATUS NOT NUMERIC
03589          MOVE ZEROS              TO CL-CLAIM-PAYMENT-STATUS
041002     END-IF.
03590
03591      IF CL-PURGED-DT NOT = LOW-VALUES
03592
               
      * EXEC CICS UNLOCK
03593 *            DATASET   (ELMSTR-DSID)
03594 *        END-EXEC
      *    MOVE '&*                    #   #00011454' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03595          MOVE ER-7691            TO EMI-ERROR
03596          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03597          MOVE -1                 TO MAINTL
03598          GO TO 8200-SEND-DATAONLY
041002     END-IF.
03599
03600      IF CL-AUTO-PAY-SEQ NOT = ZERO
03601
              
      * EXEC CICS UNLOCK
03602 *            DATASET   (ELMSTR-DSID)
03603 *       END-EXEC
      *    MOVE '&*                    #   #00011465' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03604          MOVE ER-0523             TO EMI-ERROR
03605          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03606          MOVE -1                  TO MAINTL
03607          GO TO 8200-SEND-DATAONLY
041002     END-IF.
03608
03609      PERFORM 7900-READ-ACTV-UPDATE THRU 7900-EXIT.
03610
03611      MOVE AT-ITD-PAID-EXPENSES       TO WS-ITD-PAID-EXPENSE.
03612      MOVE AT-ITD-CHARGEABLE-EXPENSE  TO WS-ITD-CHARGABLE-EXPENSE.
03613      MOVE AT-INITIAL-MANUAL-RESERVE  TO WS-INITIAL-MANUAL-RESERVE.
03614      MOVE AT-CURRENT-MANUAL-RESERVE  TO WS-CURRENT-MANUAL-RESERVE.
03615      MOVE AT-ITD-ADDITIONAL-RESERVE  TO WS-ITD-ADDITIONAL-RESERVE.
03616
03617      IF MANRSVL GREATER ZERO
03618         MOVE WS-MANRSV           TO AT-INITIAL-MANUAL-RESERVE
03619                                     AT-CURRENT-MANUAL-RESERVE
041002     END-IF.
03620      MOVE 1                      TO SUB.
03621
03622  5010-LOOP.
03623      IF SUB = 1
03624         MOVE WS-TODAY-DT         TO AT-OPEN-CLOSE-DATE   (SUB)
03625         MOVE 'O'                 TO AT-OPEN-CLOSE-TYPE   (SUB)
03626         MOVE 'NEW'               TO AT-OPEN-CLOSE-REASON (SUB)
03627      ELSE
03628         MOVE LOW-VALUES          TO AT-OPEN-CLOSE-DATE   (SUB)
03629         MOVE SPACES              TO AT-OPEN-CLOSE-TYPE   (SUB)
03630                                     AT-OPEN-CLOSE-REASON (SUB)
041002     END-IF.
03631
03632      IF SUB NOT = 6
03633         ADD 1                    TO SUB
03634         GO TO 5010-LOOP
041002     END-IF.
03635
03636  5020-UPDATE.
03637
03638      PERFORM 7915-REWRITE-TRAILER THRU 7915-EXIT.
03639
03640      IF DIAGL = ZERO
040814       AND ICD1L = ZERO
040814       AND ICD2L = ZERO
03641          PERFORM 7910-READ-NINETY THRU 7910-EXIT
03642          MOVE AT-INFO-LINE-1     TO WS-DIAGNOSIS-DESCRIPT
040814         MOVE AT-ICD-CODE-1      TO WS-ICD-CODE-1
040814         MOVE AT-ICD-CODE-2      TO WS-ICD-CODE-2
03643          GO TO 5025-BYPASS-UPDATE
041002     END-IF.
03644
03645      PERFORM 7900-READ-NINETY-UPDATE THRU 7900-EXIT.
03646
03647      MOVE AT-INFO-LINE-1         TO WS-DIAGNOSIS-DESCRIPT.
040814     IF DIAGL NOT = ZERO
040814         MOVE DIAGI              TO AT-INFO-LINE-1
040814     END-IF.
040814
040814     MOVE AT-ICD-CODE-1          TO WS-ICD-CODE-1.
040814     IF ICD1L NOT = ZERO
040814         MOVE ICD1I              TO AT-ICD-CODE-1
040814     END-IF.
040814
040814     MOVE AT-ICD-CODE-2          TO WS-ICD-CODE-2.
040814     IF ICD2L NOT = ZERO
040814         MOVE ICD2I              TO AT-ICD-CODE-2
040814     END-IF.
03649
03650      PERFORM 7915-REWRITE-TRAILER THRU 7915-EXIT.
03651
03652  5025-BYPASS-UPDATE.
03653
03654
           
      * EXEC CICS GETMAIN
03655 *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
03656 *        LENGTH   (ELTRLR-LENGTH)
03657 *    END-EXEC.
      *    MOVE '," L                  $   #00011540' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELTRLR-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03658
03659      MOVE SPACES                 TO ACTIVITY-TRAILERS.
03660      MOVE 'AT'                   TO AT-RECORD-ID.
03661
03662      MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
03663      MOVE MSTR-CARRIER           TO AT-CARRIER.
03664      MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
03665      MOVE MSTR-CERT-NO           TO AT-CERT-NO.
03666      MOVE CL-TRAILER-SEQ-CNT     TO AT-TRAILER-CNT-AT-CHG.
03667      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
03668      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.
03669      MOVE '9'                    TO AT-TRAILER-TYPE.
03670      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
03671      MOVE '5'                    TO DC-OPTION-CODE.
03672      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03673      MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT.
03674
03675      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY.
03676      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
03677
03678      MOVE CL-INCURRED-DT         TO AT-OLD-INCURRED-DT
090821                                    ws-prev-inc-dt
03679      MOVE CL-REPORTED-DT         TO AT-OLD-REPORTED-DT.
03680      MOVE CL-FILE-ESTABLISH-DT   TO AT-OLD-ESTABLISHED-DT.
03681      MOVE CL-TOTAL-PAID-AMT      TO AT-OLD-TOTAL-PAID.
03682      MOVE CL-NO-OF-DAYS-PAID     TO AT-OLD-DAYS-PAID.
03683      MOVE CL-NO-OF-PMTS-MADE     TO AT-OLD-NO-OF-PMTS.
03684      MOVE CL-PAID-THRU-DT        TO AT-OLD-PAID-THRU-DT.
03685      MOVE CL-LAST-PMT-DT         TO AT-LAST-PMT-MADE-DT.
03686      MOVE WS-DIAGNOSIS-DESCRIPT  TO AT-OLD-DIAG-DESCRIP.
03687      MOVE CL-CAUSE-CD            TO AT-OLD-DIAG-CODE.
040814     MOVE WS-ICD-CODE-1          TO AT-OLD-ICD-CODE-1.
040814     MOVE WS-ICD-CODE-2          TO AT-OLD-ICD-CODE-2.
03688
03689      MOVE WS-ITD-PAID-EXPENSE    TO AT-OLD-ITD-PAID-EXPENSE.
03690      MOVE WS-ITD-CHARGABLE-EXPENSE   TO AT-OLD-CHARGABLE-EXPENSE.
03691      MOVE WS-INITIAL-MANUAL-RESERVE  TO AT-OLD-INIT-MAN-RESV.
03692      MOVE WS-CURRENT-MANUAL-RESERVE  TO AT-OLD-CURRENT-MAN-RESV.
03693      MOVE WS-ITD-ADDITIONAL-RESERVE  TO AT-OLD-ADDL-MAN-RESV.
03694
03695
           
      * EXEC CICS WRITE
03696 *        DATASET   (ELTRLR-DSID)
03697 *        FROM      (ACTIVITY-TRAILERS)
03698 *        RIDFLD    (AT-CONTROL-PRIMARY)
03699 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011585' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131353835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03700
03701      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
03702      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
03703      MOVE CL-CERT-GROUPING       TO CERT-GROUPING.
03704      MOVE CL-CERT-STATE          TO CERT-STATE.
03705      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
03706      MOVE CL-CERT-NO             TO CERT-CERT-NO.
03707      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
03708
03709
           
      * EXEC CICS HANDLE CONDITION
03710 *        NOTFND   (5050-CERT-NOT-FOUND)
03711 *    END-EXEC.
      *    MOVE '"$I                   ! < #00011600' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303131363030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03712
03713      PERFORM 7930-READ-CERT-UPDATE THRU 7930-EXIT.
03714
03715      PERFORM 3700-BUILD-CERT THRU 3700-EXIT.
03716
03717
           
      * EXEC CICS HANDLE CONDITION
03718 *         DUPKEY   (5030-CERT-WRITTEN)
03719 *    END-EXEC.
      *    MOVE '"$$                   ! = #00011609' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303131363039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03720
03721
           
      * EXEC CICS REWRITE
03722 *        DATASET  (ELCERT-DSID)
03723 *        FROM     (CERTIFICATE-MASTER)
03724 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011614' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131363134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03725
03726  5030-CERT-WRITTEN.
03727
03728      MOVE LOW-VALUES             TO CL-LAST-PMT-DT
03729                                     CL-INCURRED-DT
03730                                     CL-REPORTED-DT
03731                                     CL-EST-END-OF-DISAB-DT
03732                                     CL-PAID-THRU-DT.
03733
03734      MOVE ZEROS                  TO CL-TOTAL-PAID-AMT
03735                                     CL-NO-OF-PMTS-MADE
03736                                     CL-NO-OF-DAYS-PAID.
03737
03738      IF CLAIM-IS-CLOSED
03739         MOVE WS-TODAY-DT         TO CL-LAST-REOPEN-DT
03740         MOVE 'O'                 TO CL-CLAIM-STATUS
041002     END-IF.
03741
03742      PERFORM 3995-BUILD-CLAIM-RECORD THRU 3995-EXIT.
03743      MOVE '5'                    TO CL-LAST-MAINT-TYPE.
03744      MOVE EMI-FORCABLE-CTR       TO CL-FORCEABLE-ERROR-CNT.
03745
03746
           
      * EXEC CICS HANDLE CONDITION
03747 *         DUPKEY   (5035-CLAIM-WRITTEN)
03748 *    END-EXEC.
      *    MOVE '"$$                   ! > #00011641' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303131363431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03749
03750
           
      * EXEC CICS REWRITE
03751 *        DATASET   (ELMSTR-DSID)
03752 *        FROM      (CLAIM-MASTER)
03753 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011646' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131363436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03754
03755  5035-CLAIM-WRITTEN.
090821     if clmtypei = 'L' or 'O'
090821        move cm-lf-benefit-cd    to ws-ben-hold
090821        move '4'                 to ws-rec-type
090821        perform 7100-read-benefit thru 7199-exit
090821        if ws-ben-alpha-hold <> spaces
090821           move ws-special-calc-cd
090821                                 to ws-lf-special-calc-cd
090821        end-if
090821     else
090821        move cm-ah-benefit-cd    to ws-ben-hold
090821        move '5'                 to ws-rec-type
090821        perform 7100-read-benefit thru 7199-exit
090821        if ws-ben-alpha-hold <> spaces
090821           move ws-special-calc-cd
090821                                 to ws-ah-special-calc-cd
090821        end-if
090821     end-if
090821
090821     move ' '                    to ws-mob-cert-ind
090821     if pi-company-id = 'CID'
090821        if ((CLMTYPEI = PI-LIFE-OVERRIDE-L1 or 'O')
090821           and (ws-lf-special-calc-cd = 'O'))
090821                      or
090821           ((clmtypei <> pi-life-override-l1 and 'O')
090821           and (ws-ah-special-calc-cd = 'O'))
090821           set mob-cert to true
090821        end-if
090821     else
090821        if (pi-company-id = 'DCC')
090821           and (cm-carrier = '7')
090821           set mob-cert to true
090821        end-if
090821     end-if
090821
090821     perform 7990-get-lo-hi-acct-dates
090821                                 thru 7990-exit
090821
      ** if the prev inc dt >= hi dt then probably
      ** have error already set
           if ws-prev-inc-dt >= ws-hi-acct-dt
              continue
           else
090821        if (ws-incur >= ws-hi-acct-dt)
090821           and (acct-cancelled)
090821           and (mob-cert)
090821           MOVE er-1682          TO EMI-ERROR
090821           MOVE -1               TO INCURL
090821           MOVE AL-UABON         TO INCURA
090821           PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821           PERFORM 3992-BUILD-TRAILER
090821                                 THRU 3992-EXIT
090821        end-if
           end-if
090821
           if ws-prev-inc-dt < cm-cert-eff-dt
              continue
           else
090821        if ws-incur < cm-cert-eff-dt
090821           MOVE er-1683          TO EMI-ERROR
090821           MOVE -1               TO INCURL
090821           MOVE AL-UABON         TO INCURA
090821           PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821           PERFORM 3992-BUILD-TRAILER
090821                                 THRU 3992-EXIT
090821        end-if
           end-if
           .
       5035-continue.
03756
03757      MOVE ER-0000                TO EMI-ERROR.
03758      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03759
03760      MOVE CLMNOI                 TO PI-LAST-CLAIM.
03761      MOVE CERTNOI                TO PI-LAST-CERT-PRIME.
03762      MOVE SUFXI                  TO PI-LAST-CERT-SUFX.
03763      MOVE CLMCARRI               TO PI-LAST-CARR.
03764
03765      IF PRTOPTL GREATER ZERO
03766          MOVE PRTOPTI            TO PI-PRT-OPT
041002     END-IF.
03767
03768      IF ALTPRTL GREATER ZERO
03769          MOVE ALTPRTI            TO PI-ALT-PRT
041002     END-IF.
03770
03771      MOVE LOW-VALUES             TO EL130AI.
03772
03773      MOVE PI-LAST-CERT-PRIME     TO CERTNOO.
03774      MOVE PI-LAST-CERT-SUFX      TO SUFXO.
03775      MOVE PI-LAST-CARR           TO CLMCARRO.
03776      MOVE PI-LAST-CLAIM          TO CLMNOO.
03777
03778      IF PI-PRT-OPT = 'N' OR 'L'
03779          MOVE PI-PRT-OPT         TO PRTOPTO
03780          MOVE 1                  TO PRTOPTL
041002     END-IF.
03781
03782      IF PI-ALT-PRT NOT = SPACES AND LOW-VALUES
03783          MOVE PI-ALT-PRT         TO ALTPRTO
03784          MOVE 4                  TO ALTPRTL
041002     END-IF.
03785
03786      MOVE AL-UANON               TO CLMNOA CLMCARRA.
PEMMOD     MOVE AL-SANON               TO CERTNOA SUFXA.
03788
03789      MOVE SPACES                 TO PI-INCURR-SW.
03790
03791      IF PRTOPTL GREATER ZERO
03792          IF PRTOPTI = 'L'
03793              GO TO 0400-CREATE-ELACTQ
03794          ELSE
03795              GO TO 0480-PRINT-NOW
041002         END-IF
041002     END-IF.
03796
03797      GO TO 8100-SEND-INITIAL-MAP.
03798
03799  5050-CERT-NOT-FOUND.
03800
03801      MOVE ER-0244                TO EMI-ERROR.
03802      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03803      MOVE -1                     TO CERTNOL.
03804      GO TO 8200-SEND-DATAONLY.
03805
03806      EJECT
03807  5500-BUILD-FORM-TRAILER.
03808 *************************************************************
03809 *           THIS CODE IS FOR 'CRI' ONLY         04/18/88
03810 *************************************************************
03811
03812      MOVE WS-TODAY-DT            TO SAVE-SEND-DT.
03813
03814      IF  WS-BEN-ALPHA-2 = '30'
03815          MOVE WS-TODAY-DT        TO DC-BIN-DATE-1
03816          MOVE '6'                TO DC-OPTION-CODE
03817          IF CL-CARRIER = 'C'
03818              MOVE +14            TO DC-ELAPSED-DAYS
03819              MOVE +0             TO DC-ELAPSED-MONTHS
03820              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03821              MOVE DC-BIN-DATE-2  TO SAVE-SEND-DT
03822          ELSE
03823              MOVE +21            TO DC-ELAPSED-DAYS
03824              MOVE +0             TO DC-ELAPSED-MONTHS
03825              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03826              MOVE DC-BIN-DATE-2      TO  SAVE-SEND-DT
041002         END-IF
041002     END-IF.
03827
03828
           
      * EXEC CICS GETMAIN
03829 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
03830 *         LENGTH   (ELTRLR-LENGTH)
03831 *         INITIMG  (GETMAIN-SPACE)
03832 *    END-EXEC.
      *    MOVE ',"IL                  $   #00011803' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELTRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03833
03834      MOVE SAVE-AT-PRIMARY-KEY    TO AT-CONTROL-PRIMARY.
03835      MOVE 'AT'                   TO AT-RECORD-ID.
03836
03837      SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.
03838      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.
03839      MOVE 'A'                    TO AT-TRAILER-TYPE.
03840      MOVE WS-TODAY-DT            TO AT-RECORDED-DT
03841                                     AT-FORM-LAST-MAINT-DT.
03842      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
03843                                     AT-FORM-LAST-UPDATED-BY.
03844      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
03845      MOVE SAVE-SEND-DT           TO AT-FORM-SEND-ON-DT.
03846      MOVE LOW-VALUES             TO AT-FORM-FOLLOW-UP-DT
03847                                     AT-FORM-RE-SEND-DT
03848                                     AT-FORM-ANSWERED-DT
03849                                     AT-EMP-FORM-ANSWERED-DT
03850                                     AT-PHY-FORM-ANSWERED-DT
03851                                     AT-EMP-FORM-SEND-ON-DT
03852                                     AT-FORM-PRINTED-DT
03853                                     AT-FORM-REPRINT-DT.
03854      MOVE '1'                    TO AT-FORM-TYPE.
03855
03856      MOVE SPACES                 TO AT-INSTRUCT-LN-1
03857                                     AT-INSTRUCT-LN-2
03858                                     AT-INSTRUCT-LN-3.
03859
03860      MOVE CL-INSURED-ADDR-CNT    TO AT-FORM-ADDR-SEQ-NO.
03861      MOVE 'I'                    TO AT-FORM-ADDRESS.
03862
03863      MOVE SAVE-SEND-DT           TO AT-PHY-FORM-SEND-ON-DT.
03864
03865      IF AM-EMPLOYER-STMT-USED = 'Y'
03866         MOVE SAVE-SEND-DT       TO AT-EMP-FORM-SEND-ON-DT
041002     END-IF.
03867
03868
           
      * EXEC CICS WRITE
03869 *         DATASET  (ELTRLR-DSID)
03870 *         FROM     (ACTIVITY-TRAILERS)
03871 *         RIDFLD   (AT-CONTROL-PRIMARY)
03872 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011845' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03873
03874  5599-EXIT.
03875      EJECT
03876 *************************************************************
03877 *           THIS CODE IS FOR 'CRI' ONLY         04/18/88
03878 *************************************************************
03879
03880  5700-BUILD-ARCHIVE-HEADER.
03881
03882      MOVE '1'                    TO CNTL-REC-TYPE.
03883      MOVE ZEROS                  TO CNTL-SEQ-NO.
03884      MOVE SPACES                 TO CNTL-ACCESS.
03885
03886
           
      * EXEC CICS READ
03887 *         DATASET  (ELCNTL-DSID)
03888 *         SET      (ADDRESS OF CONTROL-FILE)
03889 *         RIDFLD   (ELCNTL-KEY)
03890 *         UPDATE
03891 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00011864' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03892
03893      ADD 1 TO CF-CO-ARCHIVE-COUNTER.
03894
03895      MOVE CF-CO-ARCHIVE-COUNTER  TO ARCH-NUMBER.
03896
03897
           
      * EXEC CICS REWRITE
03898 *         FROM     (CONTROL-FILE)
03899 *         DATASET  (ELCNTL-DSID)
03900 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011876' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03901
03902
           
      * EXEC CICS HANDLE CONDITION
03903 *         NOTOPEN  (9990-ABEND)
03904 *         DUPKEY   (5799-EXIT)
03905 *    END-EXEC.
      *    MOVE '"$J$                  ! ? #00011882' TO DFHEIV0
           MOVE X'22244A242020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3F20233030303131383832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03906
03907
           
      * EXEC CICS GETMAIN
03908 *         SET     (ADDRESS OF LETTER-ARCHIVE)
03909 *         LENGTH  (ELARCH-LENGTH)
03910 *    END-EXEC.
      *    MOVE '," L                  $   #00011888' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELARCH-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03911
03912      MOVE SPACES                 TO LETTER-ARCHIVE.
03913      MOVE 'LA'                   TO LA-RECORD-ID.
03914      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO
03915                                     LA-ARCHIVE-NO-A1.
03916      MOVE '4'                    TO LA-RECORD-TYPE
03917                                     LA-RECORD-TYPE-A1.
03918      MOVE ZEROS                  TO LA-LINE-SEQ-NO
03919                                     LA-LINE-SEQ-NO-A1.
03920      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
03921                                     LA-COMPANY-CD-A1.
03922      MOVE CL-CARRIER             TO LA4-CARRIER.
03923      MOVE CL-CLAIM-NO            TO LA4-CLAIM-NO.
03924      MOVE CL-CERT-NO             TO LA4-CERT-NO.
03925      MOVE CL-CERT-STATE          TO LA4-STATE.
03926      MOVE  ZEROS                 TO LA4-NO-OF-COPIES.
03927      MOVE PI-PROCESSOR-ID        TO LA4-PROCESSOR-CD.
03928      MOVE WS-TODAY-DT            TO LA4-CREATION-DT.
03929      MOVE LOW-VALUES             TO LA4-INITIAL-PRINT-DATE
03930                                     LA4-RESEND-PRINT-DATE
03931                                     LA4-FORM-REM-PRINT-DT
03932                                     LA4-RESEND-DATE.
03933      MOVE CL-TRAILER-SEQ-CNT     TO LA4-FORM-TRLR-SEQ.
03934      MOVE '1'                    TO LA4-FORM-TYPE.
03935
03936
           
      * EXEC CICS WRITE
03937 *         DATASET  (ELARCH-DSID)
03938 *         FROM     (LETTER-ARCHIVE)
03939 *         RIDFLD   (LA-CONTROL-PRIMARY)
03940 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011918' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELARCH-DSID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03941
03942  5799-EXIT.
03943      EJECT
03944  6000-EDIT-CLAIM-DATA.
03945
03946      MOVE SAVE-DATE              TO DC-GREG-DATE-1-EDIT.
03947      MOVE '2'                    TO DC-OPTION-CODE.
03948      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03949      MOVE DC-BIN-DATE-1          TO WS-TODAY-DT.
03950
01119      IF MAINTL NOT = ZERO
01120          MOVE AL-UANON           TO MAINTA
041002     END-IF
01121
03951      IF CLMNOL GREATER ZERO
03952          IF CLMNOI = SPACES OR LOW-VALUES
03953              MOVE -1             TO CLMNOL
03954              MOVE AL-UABON       TO CLMNOA
03955              MOVE ER-7688        TO EMI-ERROR
03956              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03957          ELSE
03958              MOVE AL-UANON       TO CLMNOA
041002         END-IF
041002     END-IF.
03959
PEMMOD     IF CERTNOL = ZERO
PEMMOD         MOVE -1                 TO CERTNOL
PEMMOD         MOVE AL-SABON           TO CERTNOA
PEMMOD         MOVE ER-0203            TO EMI-ERROR
PEMMOD         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
PEMMOD     ELSE
PEMMOD         MOVE AL-SANON           TO CERTNOA
041002     END-IF.
03967
PEMMOD     IF SUFXL = ZERO
PEMMOD         MOVE SPACES             TO SUFXI
PEMMOD         MOVE AL-SANON           TO SUFXA
PEMMOD     ELSE
PEMMOD         MOVE AL-SANON           TO SUFXA
041002     END-IF.
03973
03974      IF CLMCARRL = ZERO
03975          MOVE -1                 TO CLMCARRL
03976          MOVE ER-0194            TO EMI-ERROR
03977          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03978      ELSE
03979          MOVE CLMCARRI           TO PI-CARRIER
03980          IF PI-NO-CARRIER-SECURITY
03981              MOVE AL-UANON       TO CLMCARRA
041002         END-IF
041002     END-IF.
03982
03983      IF CLMTYPEL > ZERO
020816        IF ((PI-COMPANY-ID = 'DCC' OR 'VPP')
100518           AND (CLMTYPEI NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
052614              AND PI-AH-OVERRIDE-L1 AND 'I' AND 'G' AND 'F'
022122              AND 'B' AND 'H'))
012009                        OR
062121           ((PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
012009           AND (CLMTYPEI NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
012009              AND PI-AH-OVERRIDE-L1))
03986               MOVE -1             TO CLMTYPEL
03987               MOVE AL-UABON       TO CLMTYPEA
03988               MOVE ER-7634        TO EMI-ERROR
03989               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03990         ELSE
03991            MOVE AL-UANON       TO CLMTYPEA
041002        END-IF
03992      ELSE
03993         IF MAINTI = 'A'
03994            MOVE -1             TO CLMTYPEL
03995            MOVE AL-UABON       TO CLMTYPEA
03996            MOVE ER-0546        TO EMI-ERROR
03997            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002        END-IF
041002     END-IF
           if instypel > ZERO
              if instypei = 'P' OR 'C'
                 move al-uanon         to instypea
              else
                 MOVE -1               TO INSTYPEL
                 MOVE AL-UABON         TO INSTYPEA
                 MOVE ER-1654          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           ELSE
              IF MAINTI = 'A'
                 MOVE -1               TO INSTYPEL
                 MOVE AL-UABON         TO INSTYPEA
                 MOVE ER-1654          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
           if (instypel <> zeros)
              and (emi-error not = er-1654)
              and (instypei = 'C')
              and (crtfnmei = fstnmei)
              move er-1675             to emi-error
              MOVE -1                  TO INSTYPEL
              MOVE AL-UABON            TO INSTYPEA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
          end-if
           if (instypel <> zeros)
              and (emi-error not = er-1654)
              and (instypei = 'P')
              and (crtfnmei <> fstnmei)
              move er-1676             to emi-error
              MOVE -1                  TO INSTYPEL
              MOVE AL-UABON            TO INSTYPEA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
          end-if
03999      IF PCERTNOL = ZERO
04000         MOVE CERTNOI             TO PCERTNOI
04001         MOVE CERTNOL             TO PCERTNOL
04002         MOVE AL-UANON            TO PCERTNOA
04003      ELSE
04004         MOVE AL-UANON            TO PCERTNOA
041002     END-IF.
04005
04006      IF PSUFXL = ZERO
04007         MOVE SUFXI               TO PSUFXI
04008         MOVE SUFXL               TO PSUFXL
04009         MOVE AL-UANON            TO PSUFXA
04010      ELSE
04011         MOVE AL-UANON            TO PSUFXA
041002     END-IF.
04012
04013      IF MAINTI = 'I'
04014          GO TO 6020-EDIT
041002     END-IF.
04015
04016      IF LSTNMEL = ZERO AND CERTMTL NOT = ZEROS
04017          MOVE -1                 TO LSTNMEL
04018          MOVE ER-0236            TO EMI-ERROR
04019          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04020      ELSE
04021          MOVE AL-UANON           TO LSTNMEA
041002     END-IF.
           if fstnmel not = zeros
              move al-uanon            to fstnmea
           end-if
020513     IF SEXL > ZEROS
020513        IF SEXI = 'M' OR 'F'
                 move al-uanon         to sexa
020513           CONTINUE
020513        ELSE
020513           MOVE -1               TO SEXL
020513           MOVE AL-UABON         TO SEXA
020513           MOVE ER-0219          TO EMI-ERROR
020513           PERFORM 9900-ERROR-FORMAT
020513                                 THRU 9900-EXIT
020513        END-IF
020513     ELSE
020513        IF MAINTI = 'A'
020513           MOVE -1               TO SEXL
020513           MOVE AL-UABON         TO SEXA
020513           MOVE ER-0219          TO EMI-ERROR
020513           PERFORM 9900-ERROR-FORMAT
020513                                 THRU 9900-EXIT
020513        END-IF
020513     END-IF
071508     IF SSNL GREATER THAN ZERO
071508         MOVE SSNI TO WS-SOC-SEC-NUMBER
071508         IF WS-SOC-SEC-NO NUMERIC AND
071508           (WS-SOC-SEC-BLANK = SPACES OR LOW-VALUES)
071508             NEXT SENTENCE
071508         ELSE
071508            IF WS-SSN-1-3 NUMERIC AND WS-SSN-4-5 NUMERIC AND
071508               WS-SSN-6-9 NUMERIC AND WS-SSN-DASH1 = '-' AND
071508               WS-SSN-DASH2 = '-'
071508                 NEXT SENTENCE
071508            ELSE
071508                 MOVE ER-0887        TO  EMI-ERROR
071508                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
071508                 MOVE AL-UABON       TO  SSNA
071508                 MOVE -1             TO  SSNL
071508            END-IF
071508        END-IF
071508     END-IF.
071508
04031      IF BIRTHDTL = ZERO
04032          GO TO 6020-EDIT
041002     END-IF.
04033
04034      IF BIRTHDTL GREATER ZERO
04035          MOVE BIRTHDTI           TO DATE-WORK
04036          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04037          MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
04038          MOVE '4'                TO DC-OPTION-CODE
04039          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04040          IF DATE-CONVERSION-ERROR
04041              MOVE AL-UABON       TO BIRTHDTA
04042              MOVE -1             TO BIRTHDTL
04043              MOVE ER-0220        TO EMI-ERROR
04044              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04045              GO TO 6020-EDIT
041002         END-IF
041002     END-IF.
04046
04047 ******************************************************************
04048 **   IF CALCULATED BIRTH DATE GREATER THAN TODAYS DATE          **
04049 **   USE THE CENTURY ADJUSTMENT SWITCH IN THE DATE ROUTINE      **
04050 **   TO SUBTRACT 100 YEARS TO OBTAIN THE CORRECT BIRTH DATE.    **
04051 ******************************************************************
04052
04053      IF DC-BIN-DATE-1 GREATER THAN SAVE-BIN-DATE
04054          MOVE BIRTHDTI           TO  DATE-WORK
04055          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04056          MOVE NUM-WORK           TO  DC-GREG-DATE-1-MDY
04057          MOVE '4'                TO  DC-OPTION-CODE
04058          MOVE '1'                TO  DC-CENTURY-ADJUSTMENT
04059          MOVE +0                 TO  DC-ELAPSED-MONTHS
04060                                      DC-ELAPSED-DAYS
04061          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04062          IF DATE-CONVERSION-ERROR
04063              MOVE AL-UABON       TO  BIRTHDTA
04064              MOVE -1             TO  BIRTHDTL
04065              MOVE ER-0220        TO  EMI-ERROR
04066              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04067              GO TO 6020-EDIT
041002         END-IF
041002     END-IF.
04068
04069      MOVE AL-UANON               TO  BIRTHDTA.
04070      MOVE DC-GREG-DATE-1-EDIT    TO  BIRTHDTI.
04071      MOVE DC-BIN-DATE-1          TO  WS-BIRTHDT.
04072      MOVE ' '                    TO  DC-CENTURY-ADJUSTMENT.
04073
04074  6020-EDIT.
04075      IF INCURL GREATER ZERO
04076          MOVE INCURI             TO DATE-WORK
04077          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04078          MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
04079          MOVE '4'                TO DC-OPTION-CODE
04080          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04081          IF DATE-CONVERSION-ERROR
04082              MOVE AL-UABON       TO INCURA
04083              MOVE -1             TO INCURL
04084              MOVE ER-0222        TO EMI-ERROR
04085              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04086          ELSE
04087              MOVE AL-UANON       TO INCURA
04088              MOVE DC-GREG-DATE-1-EDIT  TO INCURI
04089              MOVE DC-BIN-DATE-1  TO WS-INCUR
04090                                     PI-SAVE-INCUR-DT
04091              IF WS-INCUR GREATER THAN WS-TODAY-DT
04092                  MOVE ER-0511     TO EMI-ERROR
04093                  MOVE -1          TO INCURL
04094                  MOVE AL-UABON    TO INCURA
04095                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04096              ELSE
041002*                NEXT SENTENCE
041002                 CONTINUE
041002             END-IF
               END-IF
04098      ELSE
04099          MOVE ER-0516         TO EMI-ERROR
04100          MOVE AL-UABON        TO INCURA
04101          MOVE -1              TO INCURL
04102          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04103
04104      IF REPORTL GREATER ZERO
04105          MOVE REPORTI            TO DATE-WORK
04106          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04107          MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
04108          MOVE '4'                TO DC-OPTION-CODE
04109          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04110          IF DATE-CONVERSION-ERROR
04111              MOVE AL-UABON       TO REPORTA
04112              MOVE -1             TO REPORTL
04113              MOVE ER-0223        TO EMI-ERROR
04114              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04115          ELSE
04116              MOVE AL-UANON       TO REPORTA
04117              MOVE DC-GREG-DATE-1-EDIT  TO REPORTI
04118              MOVE DC-BIN-DATE-1  TO WS-REPORT
04119              IF WS-REPORT GREATER THAN WS-TODAY-DT
04120                  MOVE ER-0512     TO EMI-ERROR
04121                  MOVE -1          TO REPORTL
04122                  MOVE AL-UABON    TO REPORTA
04123                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04124              ELSE
041002*                NEXT SENTENCE
041002                 CONTINUE
041002             END-IF
041002         END-IF
04126      ELSE
04127          MOVE ER-0517         TO EMI-ERROR
04128          MOVE -1              TO REPORTL
04129          MOVE AL-UABON        TO REPORTA
04130          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04131
04132      IF WS-REPORT NOT = LOW-VALUES
04133          IF WS-INCUR GREATER THAN WS-REPORT
04134              MOVE AL-UABON       TO REPORTA
04135              MOVE -1             TO REPORTL
04136              MOVE ER-0509        TO EMI-ERROR
04137              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04138
040814     IF ICD1L GREATER THAN ZERO
040814         IF (ICD1I GREATER THAN SPACES) AND
040814            (ICD1I(4:1) NOT = '.')
040814             IF ICD1I(4:1) = ' '
040814                 MOVE '.' TO ICD1I(4:1)
040814             ELSE
040814                 IF ICD1I(8:1) = ' '
040814                     MOVE ICD1I(7:1) TO ICD1I(8:1)
040814                     MOVE ICD1I(6:1) TO ICD1I(7:1)
040814                     MOVE ICD1I(5:1) TO ICD1I(6:1)
040814                     MOVE ICD1I(4:1) TO ICD1I(5:1)
040814                     MOVE '.'        TO ICD1I(4:1)
040814                 END-IF
040814             END-IF
040814         END-IF
040814         IF (ICD1I GREATER THAN SPACES) AND
040814            (ICD1I(1:1) NOT > ' ' OR
040814             ICD1I(2:1) NOT > ' ' OR
040814             ICD1I(3:1) NOT > ' ' OR
040814             ICD1I(4:1) NOT = '.')
040814              MOVE ER-0992        TO  EMI-ERROR
040814              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814              MOVE AL-UABON       TO ICD1A
040814              MOVE -1             TO ICD1L
040814         END-IF
040814     END-IF.
040814
040814     IF ICD2L GREATER THAN ZERO
040814         IF (ICD2I GREATER THAN SPACES) AND
040814            (ICD2I(4:1) NOT = '.')
040814             IF ICD2I(4:1) = ' '
040814                 MOVE '.' TO ICD2I(4:1)
040814             ELSE
040814                 IF ICD2I(8:1) = ' '
040814                     MOVE ICD2I(7:1) TO ICD2I(8:1)
040814                     MOVE ICD2I(6:1) TO ICD2I(7:1)
040814                     MOVE ICD2I(5:1) TO ICD2I(6:1)
040814                     MOVE ICD2I(4:1) TO ICD2I(5:1)
040814                     MOVE '.'        TO ICD2I(4:1)
040814                 END-IF
040814             END-IF
040814         END-IF
040814         IF (ICD2I GREATER THAN SPACES) AND
040814            (ICD2I(1:1) NOT > ' ' OR
040814             ICD2I(2:1) NOT > ' ' OR
040814             ICD2I(3:1) NOT > ' ' OR
040814             ICD2I(4:1) NOT = '.')
040814              MOVE ER-0992        TO  EMI-ERROR
040814              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814              MOVE AL-UABON       TO ICD2A
040814              MOVE -1             TO ICD2L
040814         END-IF
040814     END-IF
040814
040814*    IF ESTENDL GREATER ZERO
040814*        MOVE ESTENDI            TO DATE-WORK
040814*        PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
040814*        MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
040814*        MOVE '4'                TO DC-OPTION-CODE
040814*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
040814*        IF DATE-CONVERSION-ERROR
040814*            MOVE AL-UABON       TO ESTENDA
040814*            MOVE -1             TO ESTENDL
040814*            MOVE ER-0224        TO EMI-ERROR
040814*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814*        ELSE
040814*            MOVE AL-UANON       TO ESTENDA
040814*            MOVE DC-GREG-DATE-1-EDIT  TO ESTENDI
040814*            MOVE DC-BIN-DATE-1  TO WS-ESTEND
040814*            IF CLMTYPEI = PI-LIFE-OVERRIDE-L1
040814*               MOVE AL-UABON    TO ESTENDA
040814*               MOVE -1          TO ESTENDL
040814*               MOVE ER-0544     TO EMI-ERROR
040814*               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814*            END-IF
040814*        END-IF
040814*    END-IF.
040814*
040814*    IF WS-ESTEND NOT = LOW-VALUES
040814*        IF WS-INCUR GREATER THAN WS-ESTEND
040814*            MOVE AL-UABON       TO ESTENDA
040814*            MOVE -1             TO ESTENDL
040814*            MOVE ER-0510        TO EMI-ERROR
040814*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814*        END-IF
040814*    END-IF.
04166
04167      IF PRTOPTL GREATER ZERO
04168          IF PRTOPTI NOT = 'N' AND 'L'
04169              MOVE AL-UABON       TO PRTOPTA
04170              MOVE -1             TO PRTOPTL
04171              MOVE ER-0334        TO EMI-ERROR
04172              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04173
04174      IF MANRSVL GREATER ZERO
               
      * EXEC CICS BIF DEEDIT
04176 *            FIELD   (MANRSVI)
04177 *            LENGTH  (9)
04178 *        END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012327' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MANRSVI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04179          IF MANRSVI NOT NUMERIC
04180              MOVE AL-UNBON       TO MANRSVA
04181              MOVE -1             TO MANRSVL
04182              MOVE ER-0489        TO EMI-ERROR
04183              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04184          ELSE
04185              MOVE AL-UNNON       TO MANRSVA
04186              MOVE MANRSVI        TO WS-MANRSV  MANRSVO
041002         END-IF
041002     END-IF.
04187
04188      MOVE SPACES                 TO ELCNTL-KEY.
04189
04190      IF CONTROL-IS-ACTUAL-CARRIER
04191          MOVE PI-CARRIER         TO CNTL-CARRIER
04192      ELSE
04193          MOVE PI-CARRIER-CONTROL-LEVEL TO CNTL-CARRIER
041002     END-IF.
04194
04195      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
04196      MOVE '6'                    TO CNTL-REC-TYPE.
04197      MOVE +0                     TO CNTL-SEQ-NO.
04198
04199
           
      * EXEC CICS HANDLE CONDITION
04200 *        NOTFND   (3100-CARRIER-NOT-FOUND)
04201 *    END-EXEC.
      *    MOVE '"$I                   ! @ #00012355' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4020233030303132333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04202
04203      PERFORM 7970-READ-CNTL THRU 7970-EXIT.
04204
04205      IF NOT CF-MANUAL-RESERVES-USED AND
04206            WS-MANRSV GREATER ZERO
04207          MOVE ER-0518             TO EMI-ERROR
04208          MOVE -1                  TO MANRSVL
04209          MOVE AL-UNBON            TO MANRSVA
04210          MOVE ZEROS               TO WS-MANRSV
04211          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04212
04213      MOVE CF-RESERVE-CONTROLS    TO WS-RESERVE-CONTROLS.
04214      MOVE CF-EXPENSE-CONTROLS    TO WS-EXPENSE-CONTROLS.
04215
04216      IF SUPVL GREATER ZERO
04217          IF SUPVI NOT = ' ' AND 'Y' AND 'N'
04218              MOVE -1             TO SUPVL
04219              MOVE AL-UABON       TO SUPVA
04220              MOVE ER-0230        TO EMI-ERROR
04221              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04222          ELSE
04223              MOVE AL-UANON       TO SUPVA
041002         END-IF
041002     END-IF.
04224
04225      IF PROCCDL = ZERO
04226         GO TO 6050-CONTINUE-EDITS
041002     END-IF.
04227
04228      MOVE SPACES                 TO ELCNTL-KEY.
04229      MOVE PROCCDI                TO CNTL-ACCESS.
04230
04231      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
04232      MOVE '2'                    TO CNTL-REC-TYPE.
04233      MOVE +0                     TO CNTL-SEQ-NO.
04234
04235
           
      * EXEC CICS HANDLE CONDITION
04236 *        NOTFND   (6040-NO-PROCESSOR-RECORD)
04237 *    END-EXEC.
      *    MOVE '"$I                   ! A #00012396' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4120233030303132333936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04238
04239      PERFORM 7970-READ-CNTL THRU 7970-EXIT.
04240
04241      GO TO 6050-CONTINUE-EDITS.
04242
04243  6040-NO-PROCESSOR-RECORD.
04244      MOVE ER-0019                TO EMI-ERROR.
04245      MOVE -1                     TO PROCCDL.
04246      MOVE AL-UABON               TO PROCCDA.
04247      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04248
04249  6050-CONTINUE-EDITS.
04250      IF (BENECDL = ZERO) OR (BENECDI = SPACES OR LOW-VALUES)
04251            GO TO 6070-CONTINUE-EDITS
041002     END-IF.
04252
04253      MOVE BENECDI                TO BENE-CODE.
04254      MOVE 'B'                    TO BENE-RCD-TYPE.
04255      MOVE PI-COMPANY-CD          TO BENE-COMP-CD.
04256
04257
           
      * EXEC CICS HANDLE CONDITION
04258 *        NOTFND   (6060-NO-BENEFICIARY-RECORD)
04259 *    END-EXEC.
      *    MOVE '"$I                   ! B #00012420' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4220233030303132343230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04260
04261      
      * EXEC CICS READ
04262 *         DATASET   (ELBENE-DSID)
04263 *         SET       (ADDRESS OF BENEFICIARY-MASTER)
04264 *         RIDFLD    (ELBENE-KEY)
04265 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012424' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04266
04267      GO TO 6070-CONTINUE-EDITS.
04268
04269  6060-NO-BENEFICIARY-RECORD.
04270      MOVE ER-0565                TO EMI-ERROR.
04271      MOVE -1                     TO BENECDL.
04272      MOVE AL-UABON               TO BENECDA.
04273      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04274
04275
04276  6070-CONTINUE-EDITS.
04277      IF CERTMTL GREATER ZERO
04278          IF CERTMTI NOT = 'A' AND 'S'
04279              MOVE -1             TO CERTMTL
04280              MOVE AL-UABON       TO CERTMTA
04281              MOVE ER-0267        TO EMI-ERROR
04282              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04283          ELSE
04284              MOVE AL-UANON       TO CERTMTA
041002         END-IF
04285      ELSE
04286          GO TO 6000-EXIT
041002     END-IF.
04287
04288      IF EFFDTL GREATER ZERO
04289          MOVE EFFDTI             TO DATE-WORK
04290          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04291          MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
04292          MOVE '4'                TO DC-OPTION-CODE
04293          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04294          IF DATE-CONVERSION-ERROR
04295              MOVE AL-UABON       TO EFFDTA
04296              MOVE -1             TO EFFDTL
04297              MOVE ER-0231        TO EMI-ERROR
04298              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04299          ELSE
04300              MOVE AL-UANON       TO EFFDTA
04301              MOVE DC-GREG-DATE-1-EDIT  TO EFFDTI
04302              MOVE DC-BIN-DATE-1  TO WS-EFFDT
041002         END-IF
04303      ELSE
04304          MOVE ER-0231            TO EMI-ERROR
04305          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04306          MOVE -1                 TO EFFDTL
04307          MOVE AL-UABON           TO EFFDTA
041002     END-IF.
04308
04309      IF ACCOUNTL = ZERO
04310          MOVE ER-0232            TO EMI-ERROR
04311          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04312          MOVE AL-UABON           TO ACCOUNTA
04313          MOVE -1                 TO ACCOUNTL
04314      ELSE
04315          MOVE AL-UANON           TO ACCOUNTA
041002     END-IF.
04316
04317      IF (PI-CERT-ACCESS-CONTROL = ' ' OR '1' OR '2')  AND
04318         (STATEL = ZERO)
04319          MOVE ER-0233        TO EMI-ERROR
04320          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04321          MOVE AL-UABON       TO STATEA
04322          MOVE -1             TO STATEL
04323      ELSE
04324          MOVE AL-UANON       TO STATEA
041002     END-IF.
04325
04326      IF (PI-CERT-ACCESS-CONTROL = '1' OR '2' OR '4') AND
04327         (CRTCARRL = ZERO)
04328          MOVE ER-0234        TO EMI-ERROR
04329          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04330          MOVE AL-UABON       TO CRTCARRA
04331          MOVE -1             TO CRTCARRL
04332      ELSE
04333          MOVE AL-UANON       TO CRTCARRA
041002     END-IF.
04334
04335      IF PI-CERT-ACCESS-CONTROL = '1' AND
04336         GROUPL = ZERO
04337          MOVE ER-0235            TO EMI-ERROR
04338          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04339          MOVE AL-UABON           TO GROUPA
04340          MOVE -1                 TO GROUPL
04341      ELSE
04342          MOVE AL-UANON           TO GROUPA
041002     END-IF.
04343
04344      IF CRTLNMEL = ZERO
04345          IF LSTNMEI = SPACES OR LOW-VALUES
04346              MOVE ER-0236            TO  EMI-ERROR
04347              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04348              MOVE AL-UABON           TO  CRTLNMEA
04349              MOVE -1                 TO  CRTLNMEL
04350          ELSE
04351              MOVE LSTNMEI            TO  CRTLNMEI
04352              MOVE AL-UANON           TO  CRTLNMEA
041002         END-IF
04353      ELSE
04354          MOVE AL-UANON               TO  CRTLNMEA
041002     END-IF.
04355
04356      IF CRTFNMEL = ZERO
04357          IF FSTNMEI = SPACES OR LOW-VALUES
041002*            NEXT SENTENCE
041002             CONTINUE
04359          ELSE
04360              MOVE FSTNMEI            TO  CRTFNMEI
04361              MOVE AL-UANON           TO  CRTFNMEA
041002         END-IF
041002     END-IF.
04362
04363      IF CRTINITL = ZERO
04364          IF INITI = SPACES OR LOW-VALUES
041002*            NEXT SENTENCE
041002             CONTINUE
04366          ELSE
04367              MOVE INITI              TO  CRTINITI
04368              MOVE AL-UANON           TO  CRTINITA
041002         END-IF
041002     END-IF.
04369
04370  6000-EXIT.
04371      EXIT.
04372
04373      EJECT
04374  6200-EDIT-CERT-DATA.
04375      IF ISSAGEL GREATER ZERO
04376          IF ISSAGEI NOT NUMERIC
04377              MOVE ER-0237        TO EMI-ERROR
04378              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04379              MOVE -1             TO ISSAGEL
04380              MOVE AL-UNBON       TO ISSAGEA
04381          ELSE
04382              MOVE AL-UNNON       TO ISSAGEA
041002         END-IF
041002     END-IF.
04383
04384      IF JNTAGEL GREATER ZERO
04385          IF JNTAGEI NOT NUMERIC
04386              MOVE ER-0238        TO EMI-ERROR
04387              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04388              MOVE -1             TO JNTAGEL
04389              MOVE AL-UNBON       TO JNTAGEA
04390          ELSE
04391              MOVE AL-UNNON       TO JNTAGEA
041002         END-IF
041002     END-IF.
04392
04393      IF LCVCDL GREATER ZERO
04394          MOVE LCVCDI             TO WS-EDIT-BEN-CODE
04395          IF INVALID-BENEFIT-CODE
04396              MOVE ER-7635        TO EMI-ERROR
04397              MOVE -1             TO LCVCDL
04398              MOVE AL-UABON       TO LCVCDA
04399              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04400          ELSE
04401              MOVE LCVCDI         TO WS-BEN-HOLD
04402              MOVE '4'            TO WS-REC-TYPE
04403              PERFORM 7100-READ-BENEFIT THRU 7199-EXIT
04404              IF WS-BEN-ALPHA-HOLD = SPACES
04405                 MOVE ER-7635     TO EMI-ERROR
04406                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04407                 MOVE -1          TO LCVCDL
04408                 MOVE AL-UABON    TO LCVCDA
04409              ELSE
04410                 MOVE WS-BEN-ALPHA-HOLD  TO LCVKINDO
04411                 MOVE WS-EARNINGS-CALC   TO WS-LF-EARNINGS-CALC
04412                 MOVE WS-SPECIAL-CALC-CD TO WS-LF-SPECIAL-CALC-CD
04413                 MOVE AL-UANON           TO LCVCDA
041002             END-IF
041002         END-IF
041002     END-IF.
04414
04415      IF ACVCDL GREATER ZERO
04416          MOVE ACVCDI             TO WS-EDIT-BEN-CODE
04417          IF INVALID-BENEFIT-CODE
04418              MOVE ER-7635        TO EMI-ERROR
04419              MOVE -1             TO ACVCDL
04420              MOVE AL-UABON       TO ACVCDA
04421              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04422          ELSE
04423              MOVE ACVCDI         TO WS-BEN-HOLD
04424              MOVE '5'            TO WS-REC-TYPE
04425              PERFORM 7100-READ-BENEFIT THRU 7199-EXIT
04426              IF WS-BEN-ALPHA-HOLD = SPACES
04427                 MOVE ER-7635     TO EMI-ERROR
04428                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04429                 MOVE -1          TO ACVCDL
04430                 MOVE AL-UABON    TO ACVCDA
04431              ELSE
04432                 MOVE WS-BEN-ALPHA-HOLD  TO ACVKINDO
04433                 MOVE WS-EARNINGS-CALC   TO WS-AH-EARNINGS-CALC
04434                 MOVE WS-SPECIAL-CALC-CD TO WS-AH-SPECIAL-CALC-CD
04435                 MOVE AL-UANON           TO ACVCDA
041002             END-IF
041002         END-IF
041002     END-IF.
04436
04437      IF LCVOTRML GREATER ZERO
04438          IF LCVOTRMI NOT NUMERIC
04439              MOVE ER-7636        TO EMI-ERROR
04440              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04441              MOVE -1             TO LCVOTRML
04442              MOVE AL-UNBON       TO LCVOTRMA
04443          ELSE
04444              MOVE AL-UNNON       TO LCVOTRMA
041002         END-IF
041002     END-IF.
04445
04446      IF ACVOTRML GREATER ZERO
04447          IF ACVOTRMI NOT NUMERIC
04448              MOVE ER-7636        TO EMI-ERROR
04449              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04450              MOVE -1             TO ACVOTRML
04451              MOVE AL-UNBON       TO ACVOTRMA
04452          ELSE
04453              MOVE AL-UNNON       TO ACVOTRMA
041002         END-IF
041002     END-IF.
04454
04455      IF LCVRATEL GREATER ZERO
               
      * EXEC CICS BIF DEEDIT
04457 *            FIELD    (LCVRATEI)
04458 *            LENGTH   (06)
04459 *        END-EXEC
           MOVE 06
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012649' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04460          IF LCVRATEI NOT NUMERIC
04461              MOVE AL-UNBON       TO LCVRATEA
04462              MOVE -1             TO LCVRATEL
04463              MOVE ER-2280        TO EMI-ERROR
04464              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04465          ELSE
04466              MOVE AL-UNNON       TO LCVRATEA
04467              MOVE LCVRATEI       TO WS-LCVRATE LCVRATEO
041002         END-IF
041002     END-IF.
04478
04479      IF ACVRATEL GREATER ZERO
               
      * EXEC CICS BIF DEEDIT
04481 *            FIELD    (ACVRATEI)
04482 *            LENGTH   (06)
04483 *        END-EXEC
           MOVE 06
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012665' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04484          IF ACVRATEI NOT NUMERIC
04485              MOVE AL-UNBON       TO ACVRATEA
04486              MOVE -1             TO ACVRATEL
04487              MOVE ER-2280        TO EMI-ERROR
04488              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04489          ELSE
04490              MOVE AL-UNNON       TO ACVRATEA
04491              MOVE ACVRATEI       TO WS-ACVRATE ACVRATEO
041002         END-IF
041002     END-IF.
04502
04503      IF LCVBENEL GREATER ZERO
               
      * EXEC CICS BIF DEEDIT
04505 *            FIELD    (LCVBENEI)
04506 *            LENGTH   (11)
04507 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012681' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVBENEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04508          IF LCVBENEI NOT NUMERIC
04509              MOVE AL-UNBON       TO LCVBENEA
04510              MOVE -1             TO LCVBENEL
04511              MOVE ER-7637        TO EMI-ERROR
04512              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04513          ELSE
04514              MOVE AL-UNNON       TO LCVBENEA
04515              MOVE LCVBENEI       TO WS-LCVBENE LCVBENEO
041002         END-IF
041002     END-IF.
04516
04517      IF ACVBENEL GREATER ZERO
               
      * EXEC CICS BIF DEEDIT
04519 *            FIELD    (ACVBENEI)
04520 *            LENGTH   (11)
04521 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012697' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVBENEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04522          IF ACVBENEI NOT NUMERIC
04523              MOVE AL-UNBON       TO ACVBENEA
04524              MOVE -1             TO ACVBENEL
04525              MOVE ER-7637        TO EMI-ERROR
04526              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04527          ELSE
04528              MOVE AL-UNNON       TO ACVBENEA
04529              MOVE ACVBENEI       TO WS-ACVBENE ACVBENEO
041002         END-IF
041002     END-IF.
04543
04544      IF LCVCNDTL GREATER ZERO
04545          IF LCVCNDTI NOT = SPACES
04546              MOVE LCVCNDTI        TO DATE-WORK
04547              PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04548              MOVE NUM-WORK        TO DC-GREG-DATE-1-MDY
04549              MOVE '4'             TO DC-OPTION-CODE
04550              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04551              IF DATE-CONVERSION-ERROR
04552                  MOVE AL-UABON    TO LCVCNDTA
04553                  MOVE -1          TO LCVCNDTL
04554                  MOVE ER-7638     TO EMI-ERROR
04555                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04556              ELSE
04557                  MOVE AL-UANON    TO LCVCNDTA
04558                  MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTI
04559                  MOVE DC-BIN-DATE-1 TO WS-LCVCNDT
041002             END-IF
041002         END-IF
041002     END-IF.
04560
04561      IF ACVCNDTL GREATER ZERO
04562          IF ACVCNDTI NOT = SPACES
04563              MOVE ACVCNDTI        TO DATE-WORK
04564              PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04565              MOVE NUM-WORK        TO DC-GREG-DATE-1-MDY
04566              MOVE '4'             TO DC-OPTION-CODE
04567              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04568              IF DATE-CONVERSION-ERROR
04569                  MOVE AL-UABON    TO ACVCNDTA
04570                  MOVE -1          TO ACVCNDTL
04571                  MOVE ER-7638     TO EMI-ERROR
04572                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04573              ELSE
04574                  MOVE AL-UANON    TO ACVCNDTA
04575                  MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTI
04576                  MOVE DC-BIN-DATE-1 TO WS-ACVCNDT
041002             END-IF
041002         END-IF
041002     END-IF.
04577
04578      IF ADDONDTL GREATER ZERO
04579          MOVE ADDONDTI        TO DATE-WORK
04580          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04581          MOVE NUM-WORK        TO DC-GREG-DATE-1-MDY
04582          MOVE '4'             TO DC-OPTION-CODE
04583          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04584          IF DATE-CONVERSION-ERROR
04585              MOVE AL-UABON    TO ADDONDTA
04586              MOVE -1          TO ADDONDTL
04587              MOVE ER-7651     TO EMI-ERROR
04588              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04589          ELSE
04590              MOVE AL-UANON    TO ADDONDTA
04591              MOVE DC-GREG-DATE-1-EDIT TO ADDONDTI
04592              MOVE DC-BIN-DATE-1 TO WS-ADD-ON-DT
041002         END-IF
041002     END-IF.
04602
04603      IF APRL GREATER ZERO
               
      * EXEC CICS BIF DEEDIT
04605 *            FIELD    (APRI)
04606 *            LENGTH   (8)
04607 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012771' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132373731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APRI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04608          IF APRI NOT NUMERIC
04609              MOVE AL-UNBON       TO APRA
04610              MOVE -1             TO APRL
04611              MOVE ER-0248        TO EMI-ERROR
04612              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04613          ELSE
04614              MOVE AL-UNNON       TO APRA
04615              MOVE APRI           TO WS-APR APRO
041002         END-IF
041002     END-IF.
04626
04627      IF PMTFREQL GREATER ZERO
               
      * EXEC CICS BIF DEEDIT
04629 *            FIELD   (PMTFREQI)
04630 *            LENGTH  (2)
04631 *        END-EXEC
           MOVE 2
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012787' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132373837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PMTFREQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04632          IF PMTFREQI NOT NUMERIC
04633              MOVE -1             TO PMTFREQL
04634              MOVE AL-UNBON       TO PMTFREQA
04635              MOVE ER-0258        TO EMI-ERROR
04636              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04637          ELSE
04638              MOVE AL-UNNON       TO PMTFREQA
04639              MOVE PMTFREQI       TO WS-PMTFREQ  PMTFREQO
041002         END-IF
041002     END-IF.
04640
04641      IF INDGRPL GREATER ZERO
04642          IF INDGRPI = 'I' OR 'G'
04643              MOVE AL-UANON       TO INDGRPA
04644          ELSE
04645              MOVE -1 TO INDGRPL
04646              MOVE AL-UABON       TO INDGRPA
04647              MOVE ER-0260        TO EMI-ERROR
04648              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04649
04650      IF LOANBALL GREATER ZERO
04651          MOVE LOANBALI           TO DEEDIT-FIELD
               
      * EXEC CICS BIF DEEDIT
04653 *            FIELD   (DEEDIT-FIELD)
04654 *            LENGTH  (12)
04655 *        END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012815' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04656          IF DEEDIT-FIELD NOT NUMERIC
04657              MOVE AL-UNBON       TO LOANBALA
04658              MOVE -1             TO LOANBALL
04659              MOVE ER-0247        TO EMI-ERROR
04660              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04661          ELSE
04662              MOVE AL-UNNON       TO LOANBALA
04663              MOVE DEEDIT-FIELD   TO HOLD-LOAN-BAL  LOANBALO
041002         END-IF
041002     END-IF.
04674
04675      IF PREMTYPL GREATER ZERO
04676          IF (PREMTYPI = '1' OR '2' OR '3')
04677              MOVE AL-UANON       TO PREMTYPA
04678          ELSE
04679              MOVE -1             TO PREMTYPL
04680              MOVE AL-UABON       TO PREMTYPA
04681              MOVE ER-0227        TO EMI-ERROR
04682              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04683
04684      IF REINCDL GREATER ZERO
04685          MOVE LOW-VALUES         TO  ERREIN-KEY
04686          MOVE PI-COMPANY-CD      TO  REIN-COMP-CD
04687          MOVE REINCDI            TO  REIN-CODE-1
04688                                      REIN-CODE-2
04689                                      REIN-CODE-3
04690          MOVE 'A'                TO  REIN-TYPE
04691          PERFORM 7992-READ-REIN THRU 7992-EXIT
04692          IF REIN-REC-NOT-FOUND
04693              MOVE ER-0266        TO  EMI-ERROR
04694              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04695              MOVE -1             TO  REINCDL
04696              MOVE AL-UABON       TO  REINCDA
04697          ELSE
04698              MOVE AL-UANON       TO  REINCDA
041002         END-IF
041002     END-IF.
04699
04700      IF CERTMTI NOT = 'A'
04701         GO TO 6200-EXIT
041002     END-IF.
04702
04728      IF PREMTYPI = '1'
04729          IF LCVCDL GREATER ZERO
04730              IF WS-LF-SPECIAL-CALC-CD = 'O'
04731                  MOVE ER-0841        TO  EMI-ERROR
04732                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04733                  MOVE -1             TO  PREMTYPL
04734                  MOVE AL-UABON       TO  PREMTYPA
041002             END-IF
               END-IF
041002     END-IF.
04735
04736      IF PREMTYPI = '1'
04737          IF ACVCDL GREATER ZERO
04738              IF WS-AH-SPECIAL-CALC-CD = 'O'
04739                  MOVE ER-0841        TO  EMI-ERROR
04740                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04741                  MOVE -1             TO  PREMTYPL
04742                  MOVE AL-UABON       TO  PREMTYPA
041002             END-IF
               END-IF
041002     END-IF.
04743
04744      IF PREMTYPI = '2'
04745          IF LCVCDL GREATER ZERO
04746              IF (PI-COMPANY-ID = 'HAN') AND
04747                 (WS-LF-SPECIAL-CALC-CD = 'O' OR 'M')
04748                         OR
04749                  WS-LF-SPECIAL-CALC-CD = 'O'
041002*                NEXT SENTENCE
04750                  CONTINUE
04751              ELSE
04752                  MOVE ER-0840        TO  EMI-ERROR
04753                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04754                  MOVE -1             TO  PREMTYPL
04755                  MOVE AL-UABON       TO  PREMTYPA
04756              END-IF
041002         END-IF
041002     END-IF.
04756
04757      IF PREMTYPI = '2'
04758          IF ACVCDL GREATER ZERO
04759              IF (PI-COMPANY-ID = 'HAN')  AND
04760                 (WS-AH-SPECIAL-CALC-CD = 'O' OR 'M')
04761                         OR
04762                  WS-AH-SPECIAL-CALC-CD = 'O'
041002*                NEXT SENTENCE
041002                 CONTINUE
04764              ELSE
04765                  MOVE ER-0840        TO  EMI-ERROR
04766                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04767                  MOVE -1             TO  PREMTYPL
04768                  MOVE AL-UABON       TO  PREMTYPA
041002             END-IF
041002         END-IF
041002     END-IF.
04769
04788      IF LCVCDL = ZEROS
04789         GO TO 6200-EXIT
041002     END-IF.
04790
04791      IF PMTFREQL NOT = ZEROS  AND NOT TEX-REG
04792          IF PMTFREQI NOT = ZEROS
04793              MOVE ER-0428        TO EMI-ERROR
04794              MOVE -1             TO PMTFREQL
04795              MOVE AL-UNBON       TO PMTFREQA
04796              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04797              GO TO 6200-CHECK-APR
041002         END-IF
041002     END-IF.
04798
04799      IF PMTFREQL = ZEROS AND TEX-REG
04800          MOVE ER-0429            TO EMI-ERROR
04801          MOVE -1                 TO PMTFREQL
04802          MOVE AL-UNBON           TO PMTFREQA
04803          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04804          GO TO 6200-CHECK-APR
041002     END-IF.
04805
04806      IF LCVOTRMI NOT NUMERIC
04807          GO TO 6200-CHECK-APR
041002     END-IF.
04808
04809      IF WS-PMTFREQ NOT = ZERO
04810          DIVIDE WS-PMTFREQ INTO LCVOTRMI
04811          GIVING DIVIDE-QUOT
04812          REMAINDER DIVIDE-REM
04813          IF DIVIDE-REM GREATER ZERO
04814              MOVE ER-0430            TO EMI-ERROR
04815              MOVE -1                 TO PMTFREQL
04816              MOVE AL-UNBON           TO PMTFREQA
04817              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04818
04819  6200-CHECK-APR.
04820      IF NET-PAY AND APRL = ZEROS
04821         MOVE ER-0257             TO EMI-ERROR
04822         MOVE -1                  TO APRL
04823         MOVE AL-UABON            TO APRA
04824         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04825
04826  6200-EXIT.
04827      EXIT.
04828
04829      EJECT
04830  6300-REQUIRED-CERT-EDIT.
04831
052614     IF CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
04833         GO TO 6300-REQUIRED-AH-EDIT
041002     END-IF
04834
04835      IF LCVCDL = ZERO
04836         MOVE ER-7639             TO EMI-ERROR
04837         MOVE -1                  TO LCVCDL
04838         MOVE AL-UABON            TO LCVCDA
04839         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04840
04841      IF LCVOTRML = ZERO
04842         MOVE ER-7670             TO EMI-ERROR
04843         MOVE -1                  TO LCVOTRML
04844         MOVE AL-UNBON            TO LCVOTRMA
04845         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04846
04847      IF LCVBENEL = ZERO
04848         MOVE ER-7671             TO EMI-ERROR
04849         MOVE -1                  TO LCVBENEL
04850         MOVE AL-UNBON            TO LCVBENEA
04851         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04852
04853      GO TO 6399-EXIT.
04854
04855  6300-REQUIRED-AH-EDIT.
04856
04857      IF ACVCDL = ZERO
04858         MOVE ER-7639             TO EMI-ERROR
04859         MOVE -1                  TO ACVCDL
04860         MOVE AL-UABON            TO ACVCDA
04861         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04862
04863      IF ACVOTRML = ZERO
04864          MOVE ER-7670             TO EMI-ERROR
04865          MOVE -1                  TO ACVOTRML
04866          MOVE AL-UNBON            TO ACVOTRMA
04867          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04868
04869      IF ACVBENEL = ZERO
04870         MOVE ER-7671             TO EMI-ERROR
04871         MOVE -1                  TO ACVBENEL
04872         MOVE AL-UNBON            TO ACVBENEA
04873         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04874
04875  6399-EXIT.
04876       EXIT.
04877
04878      EJECT
04879  6400-TEST-CLAIM-REASONABILITY.
04881      IF WS-INCUR = LOW-VALUES
04882         GO TO 6499-EXIT
041002     END-IF.
04883
04884      IF WS-EFFDT NOT = LOW-VALUES
04885          IF WS-INCUR LESS WS-EFFDT
04886              MOVE ER-0458          TO EMI-ERROR
04887              MOVE -1               TO INCURL
04888              MOVE AL-UABON         TO INCURA
04889              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04890
04891      IF CERTMTI = 'A'
04892         GO TO 6460-TEST-NEW-CERT
041002     END-IF.
04893
04894      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
04895      MOVE CRTCARRI               TO CERT-CARRIER.
04896      MOVE GROUPI                 TO CERT-GROUPING.
04897      MOVE STATEI                 TO CERT-STATE.
04898      MOVE PI-SAVE-ACCOUNT        TO CERT-ACCOUNT.
04899      MOVE PI-SAVE-CERT           TO CERT-CERT-NO.
04900      MOVE PI-SAVE-EFFDT          TO CERT-EFF-DT.
04901
04902
           
      * EXEC CICS HANDLE CONDITION
04903 *         DUPKEY   (6499-EXIT)
04904 *         NOTFND   (6499-EXIT)
04905 *    END-EXEC.
      *    MOVE '"$$I                  ! C #00013053' TO DFHEIV0
           MOVE X'222424492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4320233030303133303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04906
04907      PERFORM 7940-READ-CERT THRU 7940-EXIT.
04908
121802     IF (CLMTYPEI = PI-AH-OVERRIDE-L1  OR
052614        'I' OR 'G' OR 'F'
022122         OR 'B' OR 'H')
04910         MOVE CM-AH-ORIG-TERM     TO DC-ELAPSED-MONTHS
04911      ELSE
04912         MOVE CM-LF-ORIG-TERM     TO DC-ELAPSED-MONTHS
041002     END-IF.
04913
04914      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
04915      MOVE '6'                    TO DC-OPTION-CODE.
04916      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
04917
04918      IF (WS-INCUR GREATER THAN DC-BIN-DATE-2)
070714        and (dc-elapsed-months > zeros)
04919         MOVE ER-0519             TO EMI-ERROR
04920         MOVE -1                  TO INCURL
04921         MOVE AL-UABON            TO INCURA
04922         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04923
04924      IF CLMCARRI NOT = CRTCARRI
04925         MOVE ER-0562             TO EMI-ERROR
04926         MOVE AL-UABON            TO CLMCARRA
04927         MOVE -1                  TO CLMCARRL
04928         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04929
020816     if (pi-company-id = 'DCC' OR 'VPP')
070714        and (ws-dcc-product-code <> spaces)
070714        continue
070714     else
052614      IF ((CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122          OR 'B' OR 'H') AND
04931         CM-AH-BENEFIT-CD = '00')
                        OR
100518        ((CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O') AND
04933         CM-LF-BENEFIT-CD = '00')
04934          MOVE ER-0521         TO EMI-ERROR
04935          MOVE AL-UABON        TO CLMTYPEA
04936          MOVE -1              TO CLMTYPEL
04937          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002      END-IF
070714     end-if
031405*    IF CLMTYPEI = 'I' AND
031405*       CM-AH-CRITICAL-PERIOD = +0
031405*        MOVE ER-0521         TO EMI-ERROR
031405*        MOVE AL-UABON        TO CLMTYPEA
031405*        MOVE -1              TO CLMTYPEL
031405*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
031405*    END-IF
04938
121802*    IF PI-COMPANY-ID = 'CRI'
121802*        IF MAINTI = 'A'
121802*            IF CERT-PEND-ISSUE-ERROR  OR
121802*               CERT-PEND-ISSUE-RETURNED
121802*                MOVE -1                 TO CERTNOL
121802*                MOVE AL-UABON           TO CERTNOA
121802*                MOVE ER-7640            TO EMI-ERROR
121802*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*            END-IF
121802*        END-IF
121802*    END-IF.
04947
04948      IF (CM-AH-CANCEL-DT  NOT  =  LOW-VALUES AND
052614         (CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122          OR 'B' OR 'H'))
04950          IF WS-INCUR GREATER THAN CM-AH-CANCEL-DT
04951              GO TO 6450-DATE-ERROR
041002         END-IF
041002     END-IF.
04952
04953      IF (CM-LF-CANCEL-DT  NOT  =  LOW-VALUES AND
100518         (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'))
04955          IF WS-INCUR GREATER THAN CM-LF-CANCEL-DT
04956              GO TO 6450-DATE-ERROR
041002         END-IF
041002     END-IF.
04957
04958      IF CM-LF-DEATH-DT NOT = LOW-VALUES
04959          IF WS-INCUR GREATER THAN CM-LF-DEATH-DT
04960              GO TO 6450-DATE-ERROR
041002         END-IF
041002     END-IF.
04961
04962      GO TO 6499-EXIT.
04963
04964  6450-DATE-ERROR.
04965      MOVE ER-0520                TO EMI-ERROR.
04966      MOVE -1                     TO INCURL.
04967      MOVE AL-UABON               TO INCURA.
04968      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04969
121802*    IF (PI-COMPANY-ID = 'FIM' OR 'LGX') AND
121802*       (EMI-SEVERITY-SAVE = 'X')
121802*        ADD 1                TO EMI-FORCABLE-CTR
121802*        SUBTRACT 1 FROM EMI-FATAL-CTR
121802*    END-IF.
04974
04975      GO TO 6499-EXIT.
04976
04977      EJECT
04978  6460-TEST-NEW-CERT.
04979
04980      IF NOT EMI-NO-ERRORS
04981         GO TO 6499-EXIT
041002     END-IF.
04982
04983      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.
04984
052614     IF ((CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
022122          OR 'F' OR 'B' OR 'H') AND
04986          ACVOTRML GREATER ZERO)
04987         MOVE ACVOTRMI       TO DC-ELAPSED-MONTHS
041002     END-IF.
04988
100518     IF (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O') AND
04990          LCVOTRML GREATER ZERO
04991         MOVE LCVOTRMI       TO DC-ELAPSED-MONTHS
041002     END-IF.
04992
04993      MOVE WS-EFFDT               TO DC-BIN-DATE-1.
04994
121802*    IF PI-COMPANY-ID = 'HAN'
121802*        IF ADDONDTL GREATER ZERO
121802*            MOVE WS-ADD-ON-DT   TO  DC-BIN-DATE-1
121802*            IF CLMTYPEI = 'A'
121802*                IF ACVOTRMI NOT = ZEROS
121802*                    COMPUTE DC-ELAPSED-MONTHS = (ACVOTRMI - 1)
121802*                ELSE
041002*                    NEXT SENTENCE
121802*                    CONTINUE
121802*                END-IF
121802*            ELSE
121802*                IF LCVOTRMI NOT = ZEROS
121802*                    COMPUTE DC-ELAPSED-MONTHS = (LCVOTRMI - 1)
121802*                END-IF
121802*            END-IF
121802*        END-IF
121802*    END-IF.
05006
05007      MOVE '6'                    TO DC-OPTION-CODE.
05008      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
05009
05010      IF NO-CONVERSION-ERROR
05011          MOVE DC-BIN-DATE-2      TO WS-EXPIRE
041002     END-IF.
05012
05013      IF WS-INCUR GREATER THAN WS-EXPIRE
05014         MOVE ER-0519             TO EMI-ERROR
05015         MOVE -1                  TO INCURL
05016         MOVE AL-UABON            TO INCURA
05017         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
05018
05019      IF CLMCARRI NOT = CRTCARRI
05020         MOVE ER-0562             TO EMI-ERROR
05021         MOVE AL-UABON            TO CLMCARRA
05022         MOVE -1                  TO CLMCARRL
05023         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
05024
020816     if (pi-company-id = 'DCC' OR 'VPP')
070714        and (ws-dcc-product-code <> spaces)
070714        continue
070714     else
052614        IF (CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122            OR 'B' OR 'H')
05026            and (ACVCDL = ZEROS)
05027            MOVE ER-0521          TO EMI-ERROR
05028            MOVE AL-UABON         TO CLMTYPEA
05029            MOVE -1               TO CLMTYPEL
05030            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002        END-IF
070714     end-if
05031
020816     if (pi-company-id = 'DCC' OR 'VPP')
070714        and (ws-dcc-product-code <> spaces)
070714        continue
070714     else
100518        IF (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O')
05033            and (LCVCDL = ZEROS)
05034            MOVE ER-0521          TO EMI-ERROR
05035            MOVE AL-UABON         TO CLMTYPEA
05036            MOVE -1               TO CLMTYPEL
05037            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002        END-IF
070714     end-if
05038
100518     IF CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'
05040          IF LCVCNDTL GREATER ZERO
05041              IF WS-INCUR GREATER THAN WS-LCVCNDT
05042                  GO TO 6450-DATE-ERROR
041002             END-IF
041002         END-IF
041002     END-IF.
05043
052614     IF CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
05045          IF ACVCNDTL GREATER ZERO
05046              IF WS-INCUR GREATER THAN WS-ACVCNDT
05047                  GO TO 6450-DATE-ERROR
041002             END-IF
041002         END-IF
041002     END-IF.
05048
05049  6499-EXIT.
05050      EXIT.
061013 6500-get-acct.
061013
061013     MOVE SPACES                 TO ERACCT-KEY
061013                                    ws-dcc-product-code
                                          ws-acct-found-sw
061013
061013     IF CARR-GROUP-ST-ACCNT-CNTL
061013        MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
061013        MOVE CRTCARRI            TO ACCT-CARRIER
061013        MOVE GROUPI              TO ACCT-GROUPING
061013        MOVE STATEI              TO ACCT-STATE
061013        MOVE ACCOUNTI            TO ACCT-ACCOUNT
061013        GO TO 6500-startbr
061013     END-IF
061013
061013     MOVE PI-COMPANY-CD          TO ACCT-COMP-CD
061013     MOVE ACCOUNTI               TO ACCT-ACCOUNT
061013
061013     IF PI-CERT-ACCESS-CONTROL = ' ' OR '2'
061013        MOVE STATEI             TO ACCT-STATE
061013     END-IF
061013
061013     IF PI-CERT-ACCESS-CONTROL = '2' OR '4'
061013        MOVE CRTCARRI           TO ACCT-CARRIER
061013     END-IF
061013
061013     .
061013 6500-startbr.
061013
061013     MOVE ERACCT-KEY             TO SAVE-ERACCT-KEY
061013
061013     
      * EXEC CICS STARTBR
061013*       DATASET     (ERACCT2-DSID)
061013*       RIDFLD      (SAVE-ERACCT-KEY)
061013*       GENERIC
061013*       KEYLENGTH   (20)
061013*       resp        (ws-response)
061013*    END-EXEC
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &  N#00013298' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303133323938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 SAVE-ERACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     if not ws-resp-normal
061013        go to 6500-exit
061013     end-if
061013
061013     .
061013 6500-readnext.
061013
061013     
      * EXEC CICS READNEXT
061013*       DATASET   (ERACCT2-DSID)
061013*       SET       (ADDRESS OF ACCOUNT-MASTER)
061013*       RIDFLD    (SAVE-ERACCT-KEY)
061013*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013313' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303133333133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 SAVE-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     IF (ACCT-COMP-CD  NOT = AM-COMPANY-CD-A1)
061013        or (ACCT-CARRIER  NOT = AM-VG-CARRIER )
061013        or (ACCT-GROUPING NOT = AM-VG-GROUPING)
061013        or (ACCT-STATE    NOT = AM-VG-STATE   )
061013        or (ACCT-ACCOUNT  NOT = AM-VG-ACCOUNT )
061013        go to 6500-endbr
061013     END-IF
061013
061013     IF (WS-EFFDT >= AM-EFFECTIVE-DT)
061013        and (WS-EFFDT < AM-EXPIRATION-DT)
              set acct-found to true
061013        MOVE AM-dcc-product-code to ws-dcc-product-code
061013        GO TO 6500-endbr
061013     END-IF
061013
061013     IF WS-EFFDT >= AM-EXPIRATION-DT
061013         GO TO 6500-readnext
061013     END-IF
061013
061013     .
061013 6500-endbr.
061013
061013     
      * EXEC CICS ENDBR
061013*        DATASET    (ERACCT2-DSID)
061013*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013341' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133333431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     .
061013 6500-exit.
061013     exit.
05053  6600-CHECK-AUTO-ACTIVITY.
05054
           
      * EXEC CICS HANDLE CONDITION
05056 *        NOTFND   (6600-NOT-FOUND)
05057 *    END-EXEC.
      *    MOVE '"$I                   ! D #00013350' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4420233030303133333530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05058
05059      MOVE PI-COMPANY-ID              TO  CNTL-COMP-ID.
05060      MOVE 'T'                        TO  CNTL-REC-TYPE.
05061      MOVE SPACES                     TO  CNTL-ACCESS.
05062      MOVE +0                         TO  CNTL-SEQ-NO.
05063
05064      PERFORM 7970-READ-CNTL THRU 7970-EXIT.
05065
05066      IF CF-SYS-ACTIVE-SW (1) = 'N' OR ' '
05067          MOVE 'N'                    TO  WS-REC-FOUND-SW
05068                                          WS-LETTER-SW
05069          GO TO 6600-EXIT
041002     END-IF.
05070
05071      IF CF-SYS-LETTER-ID (1) = SPACES OR LOW-VALUES
05072          MOVE 'N'                     TO  WS-LETTER-SW
05073      ELSE
05074          MOVE 'Y'                     TO  WS-LETTER-SW
041002     END-IF.
05075
05076      MOVE 'Y'                         TO  WS-REC-FOUND-SW.
05077      GO TO 6600-EXIT.
05078
05079  6600-START-AUTO-LETTER-WRITER.
05080
05081      MOVE LOW-VALUES                  TO  W-1523-LINKDATA.
05082      MOVE PROGRAM-INTERFACE-BLOCK     TO  W-1523-COMMON-PI-DATA.
05083
05084      MOVE CF-SYS-LETTER-ID (1)        TO  W-1523-FORM-NUMBER.
05085
05086      IF CF-SYS-RESEND-DAYS (1) NOT = ZEROS
05087          MOVE SAVE-BIN-DATE           TO  DC-BIN-DATE-1
05088          MOVE '6'                     TO  DC-OPTION-CODE
05089          MOVE CF-SYS-RESEND-DAYS (1)  TO  DC-ELAPSED-DAYS
05090          MOVE +0                      TO  DC-ELAPSED-MONTHS
05091          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05092          IF NO-CONVERSION-ERROR
05093              MOVE DC-BIN-DATE-2       TO  W-1523-RESEND-DATE
05094          ELSE
05095              MOVE LOW-VALUES          TO  W-1523-RESEND-DATE
041002         END-IF
041002     END-IF.
05096
05097      IF CF-SYS-FOLLOW-UP-DAYS (1) NOT = ZEROS
05098          MOVE SAVE-BIN-DATE           TO  DC-BIN-DATE-1
05099          MOVE '6'                     TO  DC-OPTION-CODE
05100          MOVE CF-SYS-FOLLOW-UP-DAYS (1)   TO  DC-ELAPSED-DAYS
05101          MOVE +0                      TO  DC-ELAPSED-MONTHS
05102          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05103          IF NO-CONVERSION-ERROR
05104              MOVE DC-BIN-DATE-2       TO  W-1523-FOLLOW-UP-DATE
05105          ELSE
05106              MOVE LOW-VALUES          TO  W-1523-FOLLOW-UP-DATE
041002         END-IF
041002     END-IF.
05107
05108
           
      * EXEC CICS LINK
05109 *        PROGRAM    (LINK-1523)
05110 *        COMMAREA   (W-1523-LINKDATA)
05111 *        LENGTH     (W-1523-COMM-LENGTH)
05112 *    END-EXEC.
      *    MOVE '."C                   (   #00013410' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-1523, 
                 W-1523-LINKDATA, 
                 W-1523-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05113
05114      GO TO 6600-EXIT.
05115
05116  6600-NOT-FOUND.
05117      MOVE 'N'                         TO  WS-REC-FOUND-SW.
05118
05119  6600-EXIT.
05120      EXIT.
05121
CIDMOD/
CIDMOD*************************************************************
CIDMOD*****        START OF ACTIVITY FILE OUTPUT PROCESSING      **
CIDMOD*************************************************************
CIDMOD
CIDMOD 6700-OUTPUT-ACTIVITY-RECORD.
CIDMOD
CIDMOD
           
      * EXEC CICS GETMAIN
CIDMOD*        SET (ADDRESS OF DAILY-ACTIVITY-RECORD)
CIDMOD*        LENGTH (DLYACTV-LENGTH)
CIDMOD*        INITIMG (WS-BLANK)
CIDMOD*    END-EXEC.
      *    MOVE ',"IL                  $   #00013432' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DLYACTV-LENGTH, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
CIDMOD     MOVE PI-COMPANY-CD          TO DA-COMP-CD.
CIDMOD     MOVE CLMCARRI               TO DA-CARRIER.
CIDMOD     IF CLMNOL EQUAL ZERO
CIDMOD         MOVE WS-CLAIM-NUMBER    TO DA-CLAIM-NO
CIDMOD     ELSE
CIDMOD         MOVE CLMNOI             TO DA-CLAIM-NO
CIDMOD     END-IF.
CIDMOD     MOVE CERTNOI                TO DA-CERT-PRIME.
CIDMOD     MOVE SUFXI                  TO DA-CERT-SFX.
CIDMOD     MOVE +0                     TO DA-TRAILER-SEQ-NO.
CIDMOD     MOVE 'A'                    TO DA-RECORD-TYPE.
CIDMOD
           
      * EXEC CICS HANDLE CONDITION
CIDMOD*        NOTOPEN (6700-NOTOPEN-ERROR)
CIDMOD*        DUPREC (6700-EXIT)
CIDMOD*    END-EXEC.
      *    MOVE '"$J%                  ! E #00013451' TO DFHEIV0
           MOVE X'22244A252020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4520233030303133343531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
           
      * EXEC CICS WRITE
CIDMOD*        DATASET (DLYACTV-DSID)
CIDMOD*        RIDFLD (DA-KEY)
CIDMOD*        FROM (DAILY-ACTIVITY-RECORD)
CIDMOD*        LENGTH (DLYACTV-LENGTH)
CIDMOD*    END-EXEC.
      *    MOVE '&$ L                  ''   #00013456' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DLYACTV-DSID, 
                 DAILY-ACTIVITY-RECORD, 
                 DLYACTV-LENGTH, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD     GO TO 6700-EXIT.
CIDMOD
CIDMOD 6700-NOTOPEN-ERROR.
CIDMOD     MOVE '2955'                 TO EMI-ERROR.
CIDMOD     MOVE -1                     TO MAINTL.
CIDMOD     MOVE AL-UANON               TO MAINTA.
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU
CIDMOD             9900-EXIT.
CIDMOD     GO TO 8200-SEND-DATAONLY.
CIDMOD
CIDMOD 6700-EXIT.
CIDMOD     EXIT.
CIDMOD
CIDMOD*************************************************************
CIDMOD*****         END OF ACTIVITY FILE OUTPUT PROCESSING        *
CIDMOD*************************************************************
CIDMOD/
05123  6990-DEEDIT-DATE.
05124
           
      * EXEC CICS BIF DEEDIT
05125 *        FIELD   (DATE-WORK)
05126 *        LENGTH  (8)
05127 *    END-EXEC.
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00013481' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DATE-WORK, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05128  6990-EXIT.
05129      EXIT.
05130
05131      EJECT
05132  7000-BUILD-OUTPUT-MAP.
05133
05134      IF CLAIM-SWITCH = 'N'
05135          GO TO 7050-BUILD-MAP-CERT-DATA
041002     END-IF.
05136
05137      IF CL-ASSOC-CERT-TOTAL = +0 OR +1
05138          MOVE SPACES                 TO SEQUO
05139          MOVE AL-SABOF               TO SEQUA
05140      ELSE
05141          MOVE CL-ASSOC-CERT-SEQU     TO WS-CURRENT-SEQU
05142          MOVE CL-ASSOC-CERT-TOTAL    TO WS-OF-SEQU
05143          MOVE WS-CLAIM-SEQU          TO SEQUO
05144          MOVE AL-SABON               TO SEQUA
041002     END-IF.
05145
05146      MOVE CL-CARRIER             TO CLMCARRO.
05147      MOVE CL-CLAIM-TYPE          TO CLMTYPEO.
05148      MOVE CL-PRIME-CERT-PRIME    TO PCERTNOO.
05149      MOVE CL-PRIME-CERT-SFX      TO PSUFXO.
05150      MOVE CL-INSURED-LAST-NAME   TO LSTNMEO.
05151      MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.
05152      MOVE CL-INSURED-MID-INIT    TO INITO.
05153      MOVE CL-INSURED-SEX-CD      TO SEXO.
           move cl-insured-type        to instypeo
05154 *    MOVE CL-INSURED-OCC-CD      TO OCCCDO.
05155
05156      MOVE AL-UANON               TO CLMCARRA
05157                                     CLMTYPEA
05158                                     PCERTNOA
05159                                     PSUFXA
05160                                     LSTNMEA
05161                                     FSTNMEA
05162                                     INITA
05163                                     SEXA
05165
05166      MOVE CL-PRIME-CERT-NO       TO PI-PRIMARY-CERT-NO.
05167
05168      IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES
05169          MOVE ' '                 TO DC-OPTION-CODE
05170          MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
05171          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05172          IF NOT DATE-CONVERSION-ERROR
05173              MOVE DC-GREG-DATE-1-EDIT TO BIRTHDTO
05174              MOVE AL-UANON            TO BIRTHDTA
041002         END-IF
041002     END-IF.
05175
05176      MOVE CL-SOC-SEC-NO          TO SSNO.
05177      MOVE AL-UANON               TO SSNA.
05178
05179      IF CL-INCURRED-DT NOT = LOW-VALUES
05180          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
05181          MOVE ' '                TO DC-OPTION-CODE
05182          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05183          IF NOT DATE-CONVERSION-ERROR
05184              MOVE DC-GREG-DATE-1-EDIT TO INCURO
05185              MOVE AL-UANON            TO INCURA
041002         END-IF
041002     END-IF.
05186
05187      IF CL-REPORTED-DT NOT = LOW-VALUES
05188          MOVE ' '                TO DC-OPTION-CODE
05189          MOVE CL-REPORTED-DT     TO DC-BIN-DATE-1
05190          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05191          IF NOT DATE-CONVERSION-ERROR
05192              MOVE DC-GREG-DATE-1-EDIT TO REPORTO
05193              MOVE AL-UANON            TO REPORTA
041002         END-IF
041002     END-IF.
05194
040814*    IF CL-EST-END-OF-DISAB-DT NOT = LOW-VALUES
040814*        MOVE ' '                 TO DC-OPTION-CODE
040814*        MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1
040814*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
040814*        IF NOT DATE-CONVERSION-ERROR
040814*            MOVE DC-GREG-DATE-1-EDIT TO ESTENDO
040814*            MOVE AL-UANON            TO ESTENDA
040814*        END-IF
040814*    END-IF.
05202
05203      MOVE WS-DIAGNOSIS-DESCRIPT  TO DIAGO.
040814     MOVE WS-ICD-CODE-1          TO ICD1O.
040814     MOVE WS-ICD-CODE-2          TO ICD2O.
040814*    MOVE CL-CAUSE-CD            TO CAUSEO.
05205      MOVE AT-CURRENT-MANUAL-RESERVE TO MANRSVO.
05206      MOVE CL-RELATED-CLAIM-NO    TO RELCLMO.
05207      MOVE CL-PROCESSOR-ID        TO PROCCDO.
05208      MOVE CL-PRIORITY-CD         TO PRICDO.
05209      MOVE AL-UANON               TO DIAGA
040814                                    ICD1A
040814                                    ICD2A
040814*                                   CAUSEA
05211                                     RELCLMA
05212                                     PROCCDA
05213                                     PRICDA.
05214      MOVE AL-UNNON               TO MANRSVA.
05215
121802*    IF PI-COMPANY-ID  = 'ACC'  OR  'FDL' OR 'LGX'
121802*        MOVE CL-PRODUCT-CD          TO PRODCDO
121802*        MOVE AL-UANON               TO PRODCDA
121802*    END-IF.
05219
05220      MOVE CL-SUPV-ATTN-CD        TO SUPVO.
05221      MOVE CL-FILE-LOCATION       TO FILETOO.
05222      MOVE CL-BENEFICIARY         TO BENECDO.
05223      MOVE AL-UANON               TO SUPVA
05224                                     FILETOA
05225                                     BENECDA.
05226
05227      IF CLAIM-SWITCH = 'X'
05228          GO TO 7099-EXIT
041002     END-IF.
05229
05230      EJECT
05231  7050-BUILD-MAP-CERT-DATA.
05232      MOVE CM-CERT-PRIME          TO CERTNOO.
05233      MOVE CM-CERT-SFX            TO SUFXO.
PEMMOD     MOVE AL-SANON               TO CERTNOA
PEMMOD                                    SUFXA.
05236
05237      IF CM-CERT-EFF-DT NOT = LOW-VALUES
05238          MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1
05239          MOVE ' '                TO DC-OPTION-CODE
05240          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05241          IF NOT DATE-CONVERSION-ERROR
05242              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
041002         END-IF
041002     END-IF.
05243
05244      MOVE CM-ACCOUNT             TO ACCOUNTO.
05245      MOVE CM-STATE               TO STATEO.
05246      MOVE CM-CARRIER             TO CRTCARRO.
05247      MOVE CM-GROUPING            TO GROUPO.
05248      MOVE CM-INSURED-LAST-NAME   TO CRTLNMEO.
05249      MOVE CM-INSURED-FIRST-NAME  TO CRTFNMEO.
05250      MOVE CM-INSURED-INITIAL2    TO CRTINITO.
05251      MOVE CM-INSURED-ISSUE-AGE   TO ISSAGEO.
05252      MOVE CM-JT-LAST-NAME        TO JNTLNMEO.
05253      MOVE CM-JT-FIRST-NAME       TO JNTFNMEO.
05254      MOVE CM-JT-INITIAL          TO JNTINITO.
05255      MOVE CM-INSURED-JOINT-AGE   TO JNTAGEO.
05256      MOVE AL-UANON               TO ACCOUNTA
05257                                     STATEA
05258                                     CRTCARRA
05259                                     GROUPA
05260                                     CRTLNMEA
05261                                     CRTFNMEA
05262                                     CRTINITA
05263                                     JNTLNMEA
05264                                     JNTFNMEA
05265                                     JNTINITA.
05266      MOVE AL-UNNON               TO ISSAGEA
05267                                     JNTAGEA.
05268
05269      IF CM-SSN-STATE   = CM-STATE AND
05270         CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME
041002*        NEXT SENTENCE
041002         CONTINUE
05272      ELSE
05273          MOVE CM-SOC-SEC-NO      TO CRTSSNO
041002     END-IF.
05274
05275      MOVE AL-UANON               TO CRTSSNA.
05276
05277      MOVE SAVE-DATE              TO DC-GREG-DATE-1-EDIT.
05278      MOVE '2'                    TO DC-OPTION-CODE.
05279      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
05280      MOVE DC-BIN-DATE-1          TO SAVE-CURRENT-DATE.
05281
05282 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
05283      MOVE SPACES                 TO ELCNTL-KEY.
05284      MOVE CM-STATE               TO CNTL-ACCESS.
05285      MOVE '3'                    TO CNTL-REC-TYPE.
05286      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
05287      MOVE ZEROS                  TO CNTL-SEQ-NO.
05288
05289
           
      * EXEC CICS READ
05290 *        DATASET  (ELCNTL-DSID)
05291 *        SET      (ADDRESS OF CONTROL-FILE)
05292 *        RIDFLD   (ELCNTL-KEY)
05293 *        RESP     (WS-RESPONSE)
05294 *    END-EXEC.
      *    MOVE '&"S        E          (  N#00013667' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303133363637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05295
05296      IF WS-RESP-NOTFND
05297           MOVE ER-2848           TO EMI-ERROR
05298           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05299           GO TO 8100-SEND-INITIAL-MAP
041002     END-IF.
05300
05301      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
061511     MOVE CF-ST-VFY-2ND-BENE TO PI-ST-VFY-2ND-BENE.
020613     MOVE CF-ST-CAUSAL-STATE TO PI-ST-CAUSAL-STATE.
05302      EJECT
05303      IF CM-LF-BENEFIT-CD = '00'
05304          MOVE ZEROS              TO LCVOTRMO
05305                                     LCVRTRMO
05306                                     LCVRATEO
05307                                     LCVBENEO
05308          MOVE SPACES             TO LCVDSCRO
05309                                     LCVCDO
05310                                     LCVKINDO
05311                                     LCVFORMO
05312                                     LCVCNDTO
05313                                     LCVEXITO
05314                                     LCVSTATO
05315          IF CLMTYPEL GREATER THAN ZERO AND
100518            CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'
05317                 MOVE ER-0521     TO EMI-ERROR
05318                 MOVE AL-UABON    TO CLMTYPEA
05319                 MOVE -1          TO CLMTYPEL
05320                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05321                 GO TO 7060-FILL-AH-COVERAGE
05322          ELSE
05323              GO TO 7060-FILL-AH-COVERAGE
041002         END-IF
041002     END-IF.
05324
05325      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
05326      MOVE AL-UANON               TO LCVDSCRA.
05327      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-HOLD.
05328      MOVE '4'                    TO WS-REC-TYPE.
05329      PERFORM 7100-READ-BENEFIT THRU 7199-EXIT.
05330      MOVE WS-BEN-ALPHA-HOLD      TO LCVKINDO.
05331      MOVE CM-LF-BENEFIT-CD       TO LCVCDO.
05332      MOVE AL-UANON               TO LCVCDA.
05333      MOVE CM-LF-ORIG-TERM        TO LCVOTRMO
05334                                     CP-ORIGINAL-TERM.
05335      MOVE AL-UNNON               TO LCVOTRMA.
05336      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
05337      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
05338      MOVE SAVE-CURRENT-DATE      TO CP-VALUATION-DT.
05339      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
05340      MOVE '4'                    TO CP-REM-TERM-METHOD.
05341      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
05342
05343      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.
05344      MOVE CP-REMAINING-TERM-3    TO LCVRTRMO.
05345
05346      IF CM-LF-PREMIUM-RATE NUMERIC
05347          MOVE CM-LF-PREMIUM-RATE TO LCVRATEO
05348      ELSE
05349          MOVE ZEROS              TO LCVRATEO
041002     END-IF.
05350
05351      MOVE AL-UNNON               TO LCVRATEA.
05352
05353      IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC
05354          MOVE ZEROS              TO CM-LF-ALT-BENEFIT-AMT
041002     END-IF.
05355
05356      COMPUTE LCVBENEO = CM-LF-BENEFIT-AMT + CM-LF-ALT-BENEFIT-AMT.
05357
05358      MOVE AL-UNNON               TO LCVBENEA.
05359
05360      IF CM-POLICY-FORM-NO NOT = SPACES
05361          MOVE CM-POLICY-FORM-NO  TO LCVFORMO
05362      ELSE
05363          MOVE WS-FORM-HOLD       TO LCVFORMO
041002     END-IF.
05364
05365      MOVE SPACES                 TO WS-FORM-HOLD.
05366      MOVE 'L'                    TO WS-REC-TYPE.
05367      PERFORM 7400-READ-STATE THRU 7499-EXIT.
05368
05369      IF WS-FORM-HOLD NOT = SPACES
05370          MOVE WS-FORM-HOLD       TO LCVFORMO
05371          MOVE AL-UANON           TO LCVFORMA
041002     END-IF.
05372
05373      IF CM-LF-CURRENT-STATUS = '8'
05374          IF CM-LF-CANCEL-DT NOT = LOW-VALUES
05375              MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
05376              MOVE ' '             TO DC-OPTION-CODE
05377              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05378              IF NOT DATE-CONVERSION-ERROR
05379                  MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO
05380                  MOVE AL-UANON            TO LCVCNDTA
041002             END-IF
041002         END-IF
041002     END-IF.
05381
05382      IF CM-LF-CURRENT-STATUS = '7'
05383          IF CM-LF-DEATH-DT NOT = LOW-VALUES
05384              MOVE CM-LF-DEATH-DT TO DC-BIN-DATE-1
05385              MOVE ' '            TO DC-OPTION-CODE
05386              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05387              IF NOT DATE-CONVERSION-ERROR
05388                  MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO
05389                  MOVE AL-UANON   TO LCVCNDTA
041002             END-IF
041002         END-IF
041002     END-IF.
05390
05391      IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES
05392          IF CM-LF-DEATH-EXIT-DT NOT = SPACES
05393              MOVE ' '            TO DC-OPTION-CODE
05394              MOVE CM-LF-DEATH-EXIT-DT
05395                                  TO DC-BIN-DATE-1
05396              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05397              IF NOT DATE-CONVERSION-ERROR
05398                  MOVE DC-GREG-DATE-1-EDIT
05399                                  TO LCVEXITO
041002             END-IF
041002         END-IF
041002     END-IF.
05400
05401      IF CM-LF-CURRENT-STATUS = '1' OR = '4'
05402          IF CP-REMAINING-TERM-3 = ZEROS
05403              MOVE 'EXPIRED'      TO LCVSTATO
05404          ELSE
05405              MOVE 'ACTIVE'       TO LCVSTATO
041002         END-IF
041002     END-IF.
05406
05407      IF CM-LF-CURRENT-STATUS = '2'
05408         MOVE 'PEND  '            TO LCVSTATO
041002     END-IF.
05409      IF CM-LF-CURRENT-STATUS = '3'
05410         MOVE 'RESTORE'           TO LCVSTATO
041002     END-IF.
05411      IF CM-LF-CURRENT-STATUS = '5'
05412         MOVE 'REISSUE'           TO LCVSTATO
041002     END-IF.
05413      IF CM-LF-CURRENT-STATUS = '6'
05414         MOVE 'LMP DIS'           TO LCVSTATO
041002     END-IF.
05415      IF CM-LF-CURRENT-STATUS = '7'
05416         MOVE 'DEATH  '           TO LCVSTATO
041002     END-IF.
05417      IF CM-LF-CURRENT-STATUS = '8'
05418         MOVE 'CANCEL '           TO LCVSTATO
041002     END-IF.
05419      IF CM-LF-CURRENT-STATUS = '9'
05420         MOVE 'RE-ONLY'           TO LCVSTATO
041002     END-IF.
05421      IF CM-LF-CURRENT-STATUS = 'V'
05422         MOVE 'VOID   '           TO LCVSTATO
041002     END-IF.
05423      IF CM-LF-CURRENT-STATUS = 'D'
05424         MOVE 'DECLINE'           TO LCVSTATO
041002     END-IF.
100809
100809     IF PI-COMPANY-ID = 'CID'
100809         IF CM-LF-BENEFIT-CD >= '2O' AND
100809            CM-LF-BENEFIT-CD <= '2V'
100809             MOVE ER-0878       TO EMI-ERROR
100809             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100809         END-IF
100809     END-IF.
05425
05426      EJECT
05427  7060-FILL-AH-COVERAGE.
05428
05429      IF CM-AH-BENEFIT-CD = '00'
05430          MOVE ZEROS              TO ACVOTRMO
05431                                     ACVRTRMO
05432                                     ACVRATEO
05433                                     ACVBENEO
05434          MOVE SPACES             TO ACVDSCRO
05435                                     ACVCDO
05436                                     ACVKINDO
05437                                     ACVFORMO
05438                                     ACVCNDTO
05439                                     ACVEXITO
05440                                     ACVSTATO
05441          IF (CLMTYPEL GREATER THAN ZERO AND
052614             (CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122              OR 'B' OR 'H'))
05443              MOVE ER-0521     TO EMI-ERROR
05444              MOVE AL-UABON    TO CLMTYPEA
05445              MOVE -1          TO CLMTYPEL
05446              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05447              GO TO 7070-BUILD-MAP-FINISH
05448          ELSE
05449              GO TO 7070-BUILD-MAP-FINISH
041002         END-IF
041002     END-IF.
05450
05451      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
05452      MOVE CM-AH-BENEFIT-CD       TO ACVCDO WS-BEN-HOLD.
05453      MOVE AL-UANON               TO ACVDSCRA
05454                                     ACVCDA.
05455      MOVE '5'                    TO WS-REC-TYPE.
05456      PERFORM 7100-READ-BENEFIT THRU 7199-EXIT.
05457      MOVE WS-BEN-ALPHA-HOLD      TO ACVKINDO.
05458
05459      MOVE CM-AH-ORIG-TERM        TO ACVOTRMO
05460                                     CP-ORIGINAL-TERM.
05461      MOVE AL-UNNON               TO ACVOTRMA.
05462
05463      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
05464      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
05465      MOVE SAVE-CURRENT-DATE      TO CP-VALUATION-DT.
05466      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
05467      MOVE '4'                    TO CP-REM-TERM-METHOD.
05468      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
05469
05470      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.
05471      MOVE CP-REMAINING-TERM-3    TO ACVRTRMO.
05472
05473      IF CM-AH-PREMIUM-RATE NUMERIC
05474          MOVE CM-AH-PREMIUM-RATE TO ACVRATEO
05475      ELSE
05476          MOVE ZEROS              TO ACVRATEO
041002     END-IF.
05477
05478      MOVE CM-AH-BENEFIT-AMT      TO ACVBENEO.
05479      MOVE AL-UNNON               TO ACVRATEA
05480                                     ACVBENEA.
05481
05482      IF CM-POLICY-FORM-NO NOT = SPACES
05483          MOVE CM-POLICY-FORM-NO  TO ACVFORMO
05484      ELSE
05485          MOVE WS-FORM-HOLD       TO ACVFORMO
05486          MOVE SPACES TO WS-FORM-HOLD
05487          MOVE 'A'                TO WS-REC-TYPE
05488          PERFORM 7400-READ-STATE THRU 7499-EXIT
05489          IF WS-FORM-HOLD NOT = SPACES
05490              MOVE WS-FORM-HOLD   TO ACVFORMO
041002         END-IF
041002     END-IF.
05491
05492      MOVE AL-UANON               TO ACVFORMA.
05493
05494      IF CM-AH-CURRENT-STATUS = '8'
05495          IF CM-AH-CANCEL-DT NOT = LOW-VALUES
05496              MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
05497              MOVE ' '             TO DC-OPTION-CODE
05498              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05499              IF NOT DATE-CONVERSION-ERROR
05500                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO
05501                 MOVE AL-UANON    TO ACVCNDTA
041002             END-IF
041002         END-IF
041002     END-IF.
05502
05503      IF CM-AH-CURRENT-STATUS = '6' OR '7'
05504          IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
05505              MOVE CM-AH-SETTLEMENT-DT TO DC-BIN-DATE-1
05506              MOVE ' '             TO DC-OPTION-CODE
05507              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05508              IF NOT DATE-CONVERSION-ERROR
05509                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO
05510                 MOVE AL-UANON            TO ACVCNDTA
041002             END-IF
041002         END-IF
041002     END-IF.
05511
05512      IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES
05513          MOVE ' '                TO DC-OPTION-CODE
05514          MOVE CM-AH-SETTLEMENT-EXIT-DT TO DC-BIN-DATE-1
05515          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05516          IF NOT DATE-CONVERSION-ERROR
05517              MOVE DC-GREG-DATE-1-EDIT TO ACVEXITO
041002         END-IF
041002     END-IF.
05518
05519      IF CM-AH-CURRENT-STATUS = '1' OR = '4'
05520          IF CP-REMAINING-TERM-3 = ZEROS
05521              MOVE 'EXPIRED'      TO ACVSTATO
05522          ELSE
05523              MOVE 'ACTIVE'       TO ACVSTATO
041002         END-IF
041002     END-IF.
05524
05525      IF CM-AH-CURRENT-STATUS = '2'
05526          MOVE 'PEND  '           TO ACVSTATO
041002     END-IF.
05527      IF CM-AH-CURRENT-STATUS = '3'
05528          MOVE 'RESTORE'          TO ACVSTATO
041002     END-IF.
05529      IF CM-AH-CURRENT-STATUS = '5'
05530          MOVE 'REISSUE'          TO ACVSTATO
041002     END-IF.
05531      IF CM-AH-CURRENT-STATUS = '6'
05532          MOVE 'LMP DIS'          TO ACVSTATO
041002     END-IF.
05533      IF CM-AH-CURRENT-STATUS = '7'
05534          MOVE 'DEATH  '          TO ACVSTATO
041002     END-IF.
05535      IF CM-AH-CURRENT-STATUS = '8'
05536          MOVE 'CANCEL '          TO ACVSTATO
041002     END-IF.
05537      IF CM-AH-CURRENT-STATUS = '9'
05538          MOVE 'RE-ONLY'          TO ACVSTATO
041002     END-IF.
05539      IF CM-AH-CURRENT-STATUS = 'V'
05540          MOVE 'VOID   '          TO ACVSTATO
041002     END-IF.
05541      IF CM-AH-CURRENT-STATUS = 'D'
05542          MOVE 'DECLINE'          TO ACVSTATO
041002     END-IF.
041309
020816     IF PI-COMPANY-ID = 'DCC' OR 'VPP'
041309        IF CM-ACCOUNT (9:2) = 'BI'
041309           MOVE CM-COMPANY-CD    TO CTRLR-COMP-CD
041309           MOVE CM-CARRIER       TO CTRLR-CARRIER
041309           MOVE CM-GROUPING      TO CTRLR-GROUPING
041309           MOVE CM-STATE         TO CTRLR-STATE
041309           MOVE CM-ACCOUNT       TO CTRLR-ACCOUNT
041309           MOVE CM-CERT-EFF-DT   TO CTRLR-EFF-DT
041309           MOVE CM-CERT-NO       TO CTRLR-CERT-NO
041309           MOVE 'C'              TO CTRLR-REC-TYPE
041309           
      * EXEC CICS READ
041309*            DATASET  (ELCRTT-DSID)
041309*            SET      (ADDRESS OF CERTIFICATE-TRAILERS)
041309*            RIDFLD   (ELCRTT-KEY)
041309*            RESP     (WS-RESPONSE)
041309*          END-EXEC
      *    MOVE '&"S        E          (  N#00013994' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303133393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
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
041309           IF WS-RESP-NORMAL
041309               MOVE CS-VIN-NUMBER   TO WS-DIAG-VIN
041309               MOVE WS-DIAG-VIN-MSG TO DIAGO
041309               MOVE AL-UANON        TO DIAGA
041309           END-IF
041309        END-IF
041309     END-IF.
041309
05544      EJECT
05545  7070-BUILD-MAP-FINISH.
05546      MOVE CM-LOAN-APR            TO APRO.
05547      MOVE CM-PAY-FREQUENCY       TO PMTFREQO.
05548      MOVE CM-IND-GRP-TYPE        TO INDGRPO.
05549      MOVE CM-PREMIUM-TYPE        TO PREMTYPO.
05550      MOVE AL-UNNON               TO APRA
05551                                     PMTFREQA.
05552      MOVE AL-UANON               TO INDGRPA
05553                                     PREMTYPA.
05554
05555      IF (CM-SPECIAL-REIN-CODE NOT = SPACES AND LOW-VALUES)
05556          MOVE CM-SPECIAL-REIN-CODE   TO REINCDO
05557          MOVE AL-UANON               TO REINCDA
041002     END-IF.
05558
05559      IF CM-LAST-ADD-ON-DT = SPACES OR LOW-VALUES
041002*        NEXT SENTENCE
041002         CONTINUE
05561      ELSE
05562          MOVE CM-LAST-ADD-ON-DT  TO DC-BIN-DATE-1
05563          MOVE ' '                TO DC-OPTION-CODE
05564          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05565          IF NOT DATE-CONVERSION-ERROR
05566              MOVE DC-GREG-DATE-1-EDIT TO ADDONDTO
05567              MOVE AL-UANON            TO ADDONDTA
041002         END-IF
041002     END-IF.
05568
05569      IF CLAIM-SWITCH = 'N'
05570          IF SEXL = ZEROS
05571              MOVE CM-INSURED-SEX TO SEXO
020513*            MOVE AL-UANON       TO SEXA
041002         END-IF
041002     END-IF.
05573
05574      IF CM-MEMB-ACCOUNT NOT = CM-ACCOUNT-PRIME
05575          MOVE CM-MEMBER-NO       TO MEMBERO
05576          MOVE AL-UANON           TO MEMBERA
041002     END-IF.
05577
05578      IF MAINTI = 'A'
05579          IF (LOANNOL = ZEROS OR
05580              EIBAID = DFHPF5)
05581              MOVE CM-LOAN-NUMBER TO LOANNOO
05582              MOVE AL-UANON       TO LOANNOA
05583          ELSE
041002*            NEXT SENTENCE
041002             CONTINUE
041002         END-IF
05585      ELSE
05586          MOVE CM-LOAN-NUMBER     TO LOANNOO
05587          MOVE AL-UANON           TO LOANNOA
041002     END-IF.
05588
05589      IF MAINTI = 'A'
05590          IF (LOANBALL = ZEROS OR
05591              EIBAID = DFHPF5)
05592              MOVE CM-LOAN-BALANCE TO LOANBALO
05593              MOVE AL-UNNON       TO LOANBALA
05594          ELSE
041002*            NEXT SENTENCE
041002             CONTINUE
041002         END-IF
05596      ELSE
05597          MOVE CM-LOAN-BALANCE    TO LOANBALO
05598          MOVE AL-UNNON           TO LOANBALA
041002     END-IF.
05599
05600      IF CM-PREMIUM-TYPE NOT = '3'
05601          IF NOT CERT-WAS-CREATED-FOR-CLAIM
05602              PERFORM 7200-PROTECT-CERT-FIELDS
05603          ELSE
05604              PERFORM 7300-RESET-KEY-ATTRBS
041002         END-IF
05605      ELSE
05606          PERFORM 7300-RESET-KEY-ATTRBS
041002     END-IF.
05607
121802*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'
041002*        NEXT SENTENCE
121802*        CONTINUE
121802*    ELSE
05611          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA.
121802*    END-IF.
05612
05613  7099-EXIT.
05614      EXIT.
05615
05616      EJECT
05617  7100-READ-BENEFIT.
05618
           
      * EXEC CICS HANDLE CONDITION
05619 *         NOTFND   (7120-NOT-FOUND)
05620 *    END-EXEC.
      *    MOVE '"$I                   ! F #00014100' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4620233030303134313030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05621
05622      MOVE SPACES                 TO ELCNTL-KEY.
05623      MOVE WS-BEN-HOLD            TO CNTL-BENEFIT.
05624      MOVE WS-REC-TYPE            TO CNTL-REC-TYPE.
05625      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
05626      MOVE ZEROS                  TO CNTL-SEQ-NO.
05627
05628  7105-READ-FILE.
05629      PERFORM 7975-READ-CNTL-GTEQ THRU 7975-EXIT.
05630
05631      IF PI-COMPANY-ID NOT = CF-COMPANY-ID  OR
05632         WS-REC-TYPE   NOT = CF-RECORD-TYPE
05633           GO TO 7120-NOT-FOUND
041002     END-IF.
05634
05635      MOVE 1                      TO SUB.
05636  7110-LOOP.
05637      IF SUB = 9
05638          GO TO 7120-NOT-FOUND
041002     END-IF.
05639
05640      IF WS-BEN-HOLD NOT = CF-BENEFIT-CODE (SUB)
05641          ADD 1                   TO SUB
05642          GO TO 7110-LOOP
041002     END-IF.
05643
           if ws-rec-type = '4'
              move cf-joint-indicator (sub)
                                       to ws-lf-joint-indicator
           else
              move cf-joint-indicator (sub)
                                       to ws-ah-joint-indicator
           end-if
05644      MOVE CF-BENEFIT-ALPHA (SUB)    TO WS-BEN-ALPHA-HOLD.
05645      MOVE CF-CO-EARNINGS-CALC (SUB) TO WS-EARNINGS-CALC.
05646      MOVE CF-SPECIAL-CALC-CD (SUB)  TO WS-SPECIAL-CALC-CD.
05647
05648      GO TO 7199-EXIT.
05649
05650  7120-NOT-FOUND.
05651      MOVE SPACES                 TO WS-BEN-ALPHA-HOLD
05652                                     WS-FORM-HOLD.
05653
05654  7199-EXIT.
05655       EXIT.
05656
05657      EJECT
05658  7200-PROTECT-CERT-FIELDS.
05659      MOVE AL-SANON           TO CERTMTA EFFDTA ACCOUNTA STATEA
05660                                 CRTCARRA GROUPA CRTLNMEA CRTFNMEA
05661                                 LCVFORMA ACVFORMA JNTLNMEA
05662                                 JNTFNMEA JNTINITA.
05663
05664      IF CM-LF-CURRENT-STATUS = '2' OR
05665         CM-AH-CURRENT-STATUS = '2'
05666          MOVE AL-SANON           TO LOANNOA LOANBALA PREMTYPA
041002     END-IF.
05667
05668      MOVE AL-SANOF TO
05669             CRTINITA   ISSAGEA    JNTAGEA    CRTSSNA   ADDONDTA
05670             LCVDSCRA   LCVCDA     LCVOTRMA   LCVBENEA  LCVCNDTA
05671             ACVDSCRA   ACVCDA     ACVOTRMA   ACVBENEA  ACVCNDTA
05672             APRA       PMTFREQA   INDGRPA    MEMBERA   REINCDA
05673             LCVRATEA   ACVRATEA.
05674
05675      MOVE 'S'                    TO CERTMTI.
05676
05677  7300-RESET-KEY-ATTRBS.
05678      MOVE AL-UANON           TO CERTMTA EFFDTA ACCOUNTA STATEA
05679                                 CRTCARRA GROUPA CRTLNMEA CRTFNMEA
05680                                 LCVFORMA ACVFORMA JNTLNMEA
05681                                 JNTFNMEA JNTINITA.
05682
05683      IF NOT CERT-WAS-CREATED-FOR-CLAIM
05684          MOVE AL-SANOF           TO REINCDA
041002     END-IF.
05685
05686      MOVE 'S'                    TO CERTMTI.
05687
05688      EJECT
05689  7400-READ-STATE.
05690
           
      * EXEC CICS HANDLE CONDITION
05691 *         NOTFND   (7499-EXIT)
05692 *    END-EXEC.
      *    MOVE '"$I                   ! G #00014185' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4720233030303134313835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05693
05694      MOVE SPACES                 TO ELCNTL-KEY.
05695      MOVE CM-STATE               TO CNTL-ACCESS.
05696      MOVE '3'                    TO CNTL-REC-TYPE.
05697      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
05698      MOVE ZEROS                  TO CNTL-SEQ-NO.
05699
05700      PERFORM 7970-READ-CNTL THRU 7970-EXIT.
05701
05702      MOVE 1                      TO SUB.
05703
05704  7410-LOOP.
05705      IF SUB = 21
05706         GO TO 7499-EXIT
041002     END-IF.
05707
05708 *    IF (WS-BEN-HOLD  = CF-ST-BENEFIT-CD (SUB))  AND
05709 *       (WS-REC-TYPE  = CF-ST-BENEFIT-KIND (SUB))
05710 *        MOVE CF-ST-FORM-NO (SUB) TO WS-FORM-HOLD
05711 *        GO TO 7499-EXIT.
05712
05713      ADD 1                       TO SUB.
05714      GO TO 7410-LOOP.
05715
05716  7499-EXIT.
05717       EXIT.
05718
05719      EJECT
05720  7500-BROWSE-FOR-DUPLICATE.
05721
05722      MOVE 'N'                    TO INCUR-DTE-DUPE-SW.
05723
05724
           
      * EXEC CICS HANDLE CONDITION
05725 *        NOTFND   (7500-EXIT)
05726 *        DUPKEY   (7500-DUPLICATE-KEY)
05727 *        ENDFILE  (7500-EXIT)
05728 *    END-EXEC.
      *    MOVE '"$I$''                 ! H #00014221' TO DFHEIV0
           MOVE X'222449242720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4820233030303134323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05729
05730
           
      * EXEC CICS STARTBR
05731 *        DATASET    (ELMSTR5-DSID)
05732 *        RIDFLD     (ELMSTR5-KEY)
05733 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00014228' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR5-DSID, 
                 ELMSTR5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05734
05735
           
      * EXEC CICS HANDLE CONDITION
05736 *        ENDFILE  (7500-END-BROWSE)
05737 *    END-EXEC.
      *    MOVE '"$''                   ! I #00014234' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4920233030303134323334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05738
05739  7500-READ-CLAIM-LOOP.
05740
05741
           
      * EXEC CICS READNEXT
05742 *        DATASET   (ELMSTR5-DSID)
05743 *        SET       (ADDRESS OF CLAIM-MASTER)
05744 *        RIDFLD    (ELMSTR5-KEY)
05745 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00014241' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR5-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05746
05747  7500-DUPLICATE-KEY.
05748
05749      IF CL-COMPANY-CD     NOT = CM-COMPANY-CD  OR
05750         CL-CERT-CARRIER   NOT = CM-CARRIER     OR
05751         CL-CERT-GROUPING  NOT = CM-GROUPING    OR
05752         CL-CERT-STATE     NOT = CM-STATE       OR
05753         CL-CERT-ACCOUNT   NOT = CM-ACCOUNT     OR
05754         CL-CERT-EFF-DT    NOT = CM-CERT-EFF-DT OR
05755         CL-CERT-NO        NOT = CM-CERT-NO
05756            GO TO 7500-CHECK-MATCH-COUNT
041002     END-IF.
05757
05758      MOVE 'H'                    TO INCUR-DTE-DUPE-SW.
05759
05760      IF CL-INCURRED-DT = PI-SAVE-INCUR-DT
05761         MOVE 'Y'                 TO INCUR-DTE-DUPE-SW
05762         GO TO 7500-END-BROWSE
05763      ELSE
05764         GO TO 7500-READ-CLAIM-LOOP
041002     END-IF.
05765
05766  7500-CHECK-MATCH-COUNT.
05767
05768      IF CL-COMPANY-CD = CM-COMPANY-CD AND
05769         CL-CERT-NO    = CM-CERT-NO
05770           GO TO 7500-READ-CLAIM-LOOP
041002     END-IF.
05771
05772  7500-END-BROWSE.
05773
05774
           
      * EXEC CICS ENDBR
05775 *        DATASET   (ELMSTR5-DSID)
05776 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014278' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR5-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05777
05778  7500-EXIT.
05779      EXIT.
05780
05781      EJECT
05782  7550-FALL-THRU-PARA.
05783
05784
           
      * EXEC CICS DUMP
05785 *         DUMPCODE ('E130')
05786 *         TASK
05787 *    END-EXEC.
           MOVE 'E130' TO DFHEIV5
      *    MOVE '<"   T                $   #00014289' TO DFHEIV0
           MOVE X'3C2220202054202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05788
05789
           
      * EXEC CICS RETURN
05790 *    END-EXEC.
      *    MOVE '.(                    ''   #00014295' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05791
05792  7600-BROWSE-CLAIM.
05793
05794
           
      * EXEC CICS READ
05795 *        DATASET(ELMSTR-DSID)
05796 *        SET    (ADDRESS OF CLAIM-MASTER)
05797 *        RIDFLD (ELMSTR-KEY)
05798 *        GENERIC
05799 *        EQUAL
05800 *        KEYLENGTH(ELMSTR-GENERIC-LENGTH)
05801 *    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00014301' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134333031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 ELMSTR-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05802
05803      MOVE CL-CERT-PRIME          TO CERTNOI.
05804      MOVE CL-CERT-SFX            TO SUFXI.
05805
05806  7610-BROWSE-CLAIM-LOOP.
05807
05808      MOVE LOW-VALUES             TO BCERT1O
05809                                     BSUFX1O
05810                                     BCERT2O
05811                                     BSUFX2O
05812                                     BCERT3O
05813                                     BSUFX3O
05814                                     BCERT4O
05815                                     BSUFX4O
05816                                     BCERT5O
05817                                     BSUFX5O.
05818
05819      MOVE ELMSTR-KEY             TO SAVE-ELMSTR-KEY.
05820
05821
           
      * EXEC CICS HANDLE CONDITION
05822 *        ENDFILE  (7630-END-BROWSE)
05823 *    END-EXEC.
      *    MOVE '"$''                   ! J #00014329' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4A20233030303134333239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05824
05825
           
      * EXEC CICS STARTBR
05826 *        DATASET    (ELMSTR-DSID)
05827 *        RIDFLD     (ELMSTR-KEY)
05828 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00014334' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303134333334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05829
05830      MOVE +1                     TO WS-ASSOCIATED-CERTS.
05831
05832  7620-READ-CLAIM-LOOP.
05833
05834
           
      * EXEC CICS READNEXT
05835 *        DATASET   (ELMSTR-DSID)
05836 *        SET       (ADDRESS OF CLAIM-MASTER)
05837 *        RIDFLD    (ELMSTR-KEY)
05838 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00014344' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303134333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
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
           
05839
05840      IF CL-COMPANY-CD  NOT = PI-COMPANY-CD  OR
05841         CL-CARRIER     NOT = CLMCARRI       OR
05842         CL-CLAIM-NO    NOT = CLMNOI
05843           GO TO 7630-END-BROWSE
041002     END-IF.
05844
05845      IF WS-ASSOCIATED-CERTS = 1
05846          MOVE CL-CERT-PRIME      TO BCERT1O
05847          MOVE CL-CERT-SFX        TO BSUFX1O
041002     END-IF.
05848
05849      IF WS-ASSOCIATED-CERTS = 2
05850          MOVE CL-CERT-PRIME      TO BCERT2O
05851          MOVE CL-CERT-SFX        TO BSUFX2O
041002     END-IF.
05852
05853      IF WS-ASSOCIATED-CERTS = 3
05854          MOVE CL-CERT-PRIME      TO BCERT3O
05855          MOVE CL-CERT-SFX        TO BSUFX3O
041002     END-IF.
05856
05857      IF WS-ASSOCIATED-CERTS = 4
05858          MOVE CL-CERT-PRIME      TO BCERT4O
05859          MOVE CL-CERT-SFX        TO BSUFX4O
041002     END-IF.
05860
05861      IF WS-ASSOCIATED-CERTS = 5
05862          MOVE CL-CERT-PRIME      TO BCERT5O
05863          MOVE CL-CERT-SFX        TO BSUFX5O
05864      ELSE
05865          ADD +1                  TO WS-ASSOCIATED-CERTS
05866          GO TO 7620-READ-CLAIM-LOOP
041002     END-IF.
05867
05868  7630-END-BROWSE.
05869
05870
           
      * EXEC CICS ENDBR
05871 *        DATASET   (ELMSTR-DSID)
05872 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014387' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303134333837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05873
05874  7640-HIGHLIGHT-CERT-DISPLAYED.
05875
05876      MOVE SAVE-ELMSTR-KEY        TO ELMSTR-KEY.
05877      MOVE AL-SANON               TO BCERT1A
05878                                     BSUFX1A
05879                                     BCERT2A
05880                                     BSUFX2A
05881                                     BCERT3A
05882                                     BSUFX3A
05883                                     BCERT4A
05884                                     BSUFX4A
05885                                     BCERT5A
05886                                     BSUFX5A.
05887
05888      IF BCERT1O = CERTNOI AND
05889         BSUFX1O = SUFXI
05890             MOVE AL-SABON        TO BCERT1A
05891                                     BSUFX1A
041002     END-IF.
05892
05893      IF BCERT2O = CERTNOI AND
05894         BSUFX2O = SUFXI
05895             MOVE AL-SABON        TO BCERT2A
05896                                     BSUFX2A
041002     END-IF.
05897
05898      IF BCERT3O = CERTNOI AND
05899         BSUFX3O = SUFXI
05900             MOVE AL-SABON        TO BCERT3A
05901                                     BSUFX3A
041002     END-IF.
05902
05903      IF BCERT4O = CERTNOI AND
05904         BSUFX4O = SUFXI
05905             MOVE AL-SABON        TO BCERT4A
05906                                     BSUFX4A
041002     END-IF.
05907
05908      IF BCERT5O = CERTNOI AND
05909         BSUFX5O = SUFXI
05910             MOVE AL-SABON        TO BCERT5A
05911                                     BSUFX5A
041002     END-IF.
05912
05913  7699-EXIT.
05914      EXIT.
05915
05916      EJECT
05917  7700-CHECK-SEQUENCE.
05918
05919
           
      * EXEC CICS HANDLE CONDITION
05920 *        ENDFILE  (7799-EXIT)
05921 *        NOTFND   (7799-EXIT)
05922 *    END-EXEC.
      *    MOVE '"$''I                  ! K #00014442' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4B20233030303134343432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05923
05924
           
      * EXEC CICS READ
05925 *        DATASET(ELMSTR-DSID)
05926 *        SET    (ADDRESS OF CLAIM-MASTER)
05927 *        RIDFLD (ELMSTR-KEY)
05928 *        GENERIC
05929 *        EQUAL
05930 *        KEYLENGTH(ELMSTR-GENERIC-LENGTH)
05931 *    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00014448' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 ELMSTR-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05932
05933      COMPUTE WS-ASSOC-CERT-TOTAL =
05934              CL-ASSOC-CERT-TOTAL + ONE-OR-MIN1.
05935
05936      GO TO 7799-EXIT.
05937
05938  7710-RESEQUENCE-CLAIMS.
05939
05940
           
      * EXEC CICS HANDLE CONDITION
05941 *        ENDFILE  (7790-END-BROWSE)
05942 *    END-EXEC.
      *    MOVE '"$''                   ! L #00014465' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4C20233030303134343635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05943
05944
           
      * EXEC CICS STARTBR
05945 *        DATASET    (ELMSTR-DSID)
05946 *        RIDFLD     (ELMSTR-KEY)
05947 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00014470' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303134343730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05948
05949      ADD +1 TO WS-ASSOC-CERT-SEQU
05950                WS-READNEXT-SWITCH.
05951
05952  7720-READ-CLAIM-LOOP.
05953
05954
           
      * EXEC CICS READNEXT
05955 *        DATASET   (ELMSTR-DSID)
05956 *        SET       (ADDRESS OF CLAIM-MASTER)
05957 *        RIDFLD    (ELMSTR-KEY)
05958 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00014481' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303134343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
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
           
05959
05960      IF WS-READNEXT-SWITCH = 1
05961          ADD 1 TO WS-READNEXT-SWITCH
05962          GO TO 7720-READ-CLAIM-LOOP
041002     END-IF.
05963
05964  7730-END-BROWSE.
05965
05966
           
      * EXEC CICS ENDBR
05967 *        DATASET   (ELMSTR-DSID)
05968 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014495' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303134343935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05969
05970  7740-READ-CLAIM-UPDATE.
05971
05972      IF CL-COMPANY-CD NOT = WS-SAVE-COMPANY-CD OR
05973         CL-CARRIER    NOT = WS-SAVE-CARRIER    OR
05974         CL-CLAIM-NO   NOT = WS-SAVE-CLAIM-NO
05975           GO TO 7799-EXIT
05976      ELSE
05977           MOVE ZERO              TO WS-READNEXT-SWITCH
041002     END-IF.
05978
05979
           
      * EXEC CICS READ
05980 *        DATASET(ELMSTR-DSID)
05981 *        SET    (ADDRESS OF CLAIM-MASTER)
05982 *        RIDFLD (ELMSTR-KEY)
05983 *        UPDATE
05984 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00014510' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05985
05986      MOVE WS-ASSOC-CERT-TOTAL    TO CL-ASSOC-CERT-TOTAL.
05987      MOVE WS-ASSOC-CERT-SEQU     TO CL-ASSOC-CERT-SEQU.
05988
05989
           
      * EXEC CICS REWRITE
05990 *         DATASET     (ELMSTR-DSID)
05991 *         FROM        (CLAIM-MASTER)
05992 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00014521' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303134353231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05993
05994      GO TO 7710-RESEQUENCE-CLAIMS.
05995
05996  7790-END-BROWSE.
05997
05998
           
      * EXEC CICS ENDBR
05999 *        DATASET   (ELMSTR-DSID)
06000 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014531' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303134353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06001
06002  7799-EXIT.
06003      EXIT.
06004
06005      EJECT
06006 ************************************************
06007 *  I/O REQUESTS AGAINST ACTIVITY TRAILER FILE  *
06008 ************************************************
06009
06010  7900-READ-ACTV-UPDATE.
06011      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
06012      MOVE MSTR-CARRIER           TO TRLR-CARRIER.
06013      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
06014      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
06015      MOVE +0                     TO TRLR-SEQ-NO.
06016      MOVE '1'                    TO TRLR-TYPE.
06017      GO TO 7900-READ-TRAILER-UPDATE.
06018
06019  7900-READ-NINETY-UPDATE.
06020      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
06021      MOVE MSTR-CARRIER           TO TRLR-CARRIER.
06022      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
06023      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
06024      MOVE +90                    TO TRLR-SEQ-NO.
06025      MOVE '6'                    TO TRLR-TYPE.
06026
06027  7900-READ-TRAILER-UPDATE.
06028
           
      * EXEC CICS READ  UPDATE
06029 *        DATASET    (ELTRLR-DSID)
06030 *        SET        (ADDRESS OF ACTIVITY-TRAILERS)
06031 *        RIDFLD     (ELTRLR-KEY)
06032 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00014562' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134353632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06033
06034  7900-EXIT.
06035       EXIT.
06036
06037  7910-READ-ACTV.
06038      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
06039      MOVE MSTR-CARRIER           TO TRLR-CARRIER.
06040      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
06041      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
06042      MOVE +0                     TO TRLR-SEQ-NO.
06043      MOVE '1'                    TO TRLR-TYPE.
06044      GO TO 7910-READ-TRAILER.
06045
06046  7910-READ-NINETY.
06047      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
06048      MOVE MSTR-CARRIER           TO TRLR-CARRIER.
06049      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
06050      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
06051      MOVE +90                    TO TRLR-SEQ-NO.
06052      MOVE '6'                    TO TRLR-TYPE.
06053
06054  7910-READ-TRAILER.
06055      MOVE 'TRLR'                 TO FILE-SWITCH.
06056
06057
           
      * EXEC CICS READ
06058 *        DATASET   (ELTRLR-DSID)
06059 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
06060 *        RIDFLD    (ELTRLR-KEY)
06061 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014592' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134353932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06062
06063  7910-EXIT.
06064       EXIT.
06065
06066  7915-REWRITE-TRAILER.
06067
           
      * EXEC CICS REWRITE
06068 *        DATASET   (ELTRLR-DSID)
06069 *        FROM      (ACTIVITY-TRAILERS)
06070 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00014603' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06071
06072  7915-EXIT.
06073       EXIT.
06074
06075      EJECT
06076 ********************************************
06077 *  I/O REQUESTS AGAINST CLAIM MASTER FILE  *
06078 ********************************************
06079
06080  7920-READ-CLAIM-UPDATE.
06081
06082
           
      * EXEC CICS READ
06083 *        UPDATE
06084 *        DATASET   (ELMSTR-DSID)
06085 *        SET       (ADDRESS OF CLAIM-MASTER)
06086 *        RIDFLD    (ELMSTR-KEY)
06087 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00014619' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06088
06089  7920-EXIT.
06090       EXIT.
06091
06154  7930-READ-CERT-UPDATE.
06155
           
      * EXEC CICS READ
06156 *        DATASET  (ELCERT-DSID)
06157 *        SET      (ADDRESS OF CERTIFICATE-MASTER)
06158 *        RIDFLD   (ELCERT-KEY)
06159 *        UPDATE
06160 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00014631' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06161
06162  7930-EXIT.
06163       EXIT.
06164
06165  7940-READ-CERT.
06166
           
      * EXEC CICS READ
06167 *        DATASET  (ELCERT-DSID)
06168 *        SET      (ADDRESS OF CERTIFICATE-MASTER)
06169 *        RIDFLD   (ELCERT-KEY)
06170 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014643' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06171
06172  7940-EXIT.
06173       EXIT.
06092  7950-READ-CLAIM.
06093
           
      * EXEC CICS READ
06094 *        DATASET   (ELMSTR-DSID)
06095 *        SET       (ADDRESS OF CLAIM-MASTER)
06096 *        RIDFLD    (ELMSTR-KEY)
06097 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014653' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06098
06099  7950-EXIT.
06100       EXIT.
06101
06102      EJECT
06103 ***************************************
06104 *  I/O REQUESTS AGAINST CONTROL FILE  *
06105 ***************************************
06106
06107  7960-READ-CNTL-UPDATE.
06108
           
      * EXEC CICS READ
06109 *        UPDATE
06110 *        DATASET  (ELCNTL-DSID)
06111 *        SET      (ADDRESS OF CONTROL-FILE)
06112 *        RIDFLD   (ELCNTL-KEY)
06113 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00014669' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06114
06115  7960-EXIT.
06116       EXIT.
06117
06118  7970-READ-CNTL.
06119
           
      * EXEC CICS READ
06120 *        DATASET  (ELCNTL-DSID)
06121 *        SET      (ADDRESS OF CONTROL-FILE)
06122 *        RIDFLD   (ELCNTL-KEY)
06123 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014681' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06124
06125  7970-EXIT.
06126       EXIT.
06127
06128  7975-READ-CNTL-GTEQ.
06129
           
      * EXEC CICS READ
06130 *         DATASET   (ELCNTL-DSID)
06131 *         SET       (ADDRESS OF CONTROL-FILE)
06132 *         RIDFLD    (ELCNTL-KEY)
06133 *         GTEQ
06134 *     END-EXEC.
      *    MOVE '&"S        G          (   #00014692' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06135
06136  7975-EXIT.
06137       EXIT.
06138
06139      EJECT
06140 **************************************************
06141 *  I/O REQUESTS AGAINST CERTIFICATE MASTER FILE  *
06142 **************************************************
06143
06144  7980-READ-CERT5.
06145
            
      * EXEC CICS READ
06146 *         DATASET  (ELCERT5-DSID)
06147 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
06148 *         RIDFLD   (ELCERT-KEY-5)
06149 *     END-EXEC.
      *    MOVE '&"S        E          (   #00014709' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134373039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT5-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY-5, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06150
06151  7980-EXIT.
06152       EXIT.
06153
090821 7990-get-lo-hi-acct-dates.
090821     MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
090821     MOVE PI-CARRIER          TO ACCT-CARRIER
090821     MOVE PI-GROUPING         TO ACCT-GROUPING
090821     MOVE PI-STATE            TO ACCT-STATE
090821     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
090821     MOVE low-values          TO ACCT-EXP-DT
090821     MOVE ERACCT-KEY          TO SAVE-ERACCT-KEY
090821
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
      *    MOVE '&,         G          &  N#00014731' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303134373331' TO DFHEIV0(25:11)
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
      *    MOVE '&.S                   )  N#00014742' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303134373432' TO DFHEIV0(25:11)
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
090821           AND save-eracct-key(1:20) =
090821                       AM-CONTROL-PRIMARY (1:20)
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
      *    MOVE '&2                    $   #00014767' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303134373637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821     end-if
090821
090821     .
090821 7990-exit.
090821     exit.
06176 **************************************************
06177 *  I/O REQUESTS AGAINST BENEFICIARY MASTER FILE  *
06178 **************************************************
06179
06180  7991-READ-BENE.
06181
           
      * EXEC CICS READ
06182 *         DATASET   (ELBENE-DSID)
06183 *         SET       (ADDRESS OF BENEFICIARY-MASTER)
06184 *         RIDFLD    (ELBENE-KEY)
06185 *     END-EXEC.
      *    MOVE '&"S        E          (   #00014781' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134373831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06186
06187  7991-EXIT.
06188       EXIT.
06189
06190      EJECT
06191 **************************************************
06192 *  I/O REQUESTS AGAINST REINSURANCE MASTER FILE  *
06193 **************************************************
06194
06195  7992-READ-REIN.
06196
06197
           
      * EXEC CICS HANDLE CONDITION
06198 *        NOTFND   (7992-NOT-FOUND)
06199 *    END-EXEC.
      *    MOVE '"$I                   ! M #00014798' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4D20233030303134373938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06200
06201
           
      * EXEC CICS READ
06202 *        DATASET   (ERREIN-DSID)
06203 *        SET       (ADDRESS OF REINSURANCE-RECORD)
06204 *        RIDFLD    (ERREIN-KEY)
06205 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014803' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREIN-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06206
06207      MOVE 'Y'                       TO  WS-REIN-REC-FOUND-SW.
06208
06209      GO TO 7992-EXIT.
06210
06211  7992-NOT-FOUND.
06212      MOVE 'N'                       TO  WS-REIN-REC-FOUND-SW.
06213
06214  7992-EXIT.
06215      EXIT.
06216
06217      EJECT
06218  8000-LOAD-ERROR-MESSAGES.
06219      MOVE SPACES                 TO ERRMSG1O  ERRMSG2O.
06220
06221      IF EMI-NO-ERRORS
06222          GO TO 8000-EXIT
041002     END-IF.
06223
061013     perform varying e1 from +1 by +1 until e1 > +3
061013        if emi-error-number (e1) = '1651' or '1652' or '1653'
061013                                or '1655' or '1660'
061013           move 'W'              to emi-severity (e1)
061013        end-if
061013     end-perform
06224      IF EMI-NUMBER-OF-LINES = 1
06225          MOVE EMI-LINE1          TO ERRMSG1O
06226          GO TO 8000-EXIT
041002     END-IF.
06227
06228      IF EMI-NUMBER-OF-LINES = 2
06229           MOVE EMI-LINE1         TO ERRMSG1O
06230           MOVE EMI-LINE2         TO ERRMSG2O
06231           GO TO 8000-EXIT
041002     END-IF.
06232
06233      MOVE EMI-LINE1              TO ERRMSG1O.
06234
06235  8000-EXIT.
06236      EXIT.
06237
06238  8100-SEND-INITIAL-MAP.
06239      MOVE SAVE-DATE              TO RUNDTEO.
06240      MOVE EIBTIME                TO TIME-IN.
06241      MOVE TIME-OUT               TO RUNTIMEO.
           IF EIBAID = DFHPF11
              MOVE 'Y'                 TO EMI-ROLL-SWITCH
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           end-if
PEMMOD     IF RETURNED-FROM = XCTL-114
PEMMOD        CONTINUE
PEMMOD     ELSE
PEMMOD        MOVE -1                  TO MAINTL
PEMMOD     END-IF.
06243
121802*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL' AND 'LGX'
06245          MOVE SPACES             TO PRODO.
06246          MOVE AL-SANOF           TO PRODCDA.
121802*    END-IF.
06247
06248      MOVE PI-COMPANY-ID          TO COMPO.
06249      MOVE AL-PABOF               TO COMPA.
06250      MOVE PI-MEMBER-CAPTION      TO MEMCAPO.
06251      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
06252      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
06253
06254      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
06255
06256      IF NOT PI-NO-CARRIER-SECURITY
06257         MOVE PI-CARRIER-SECURITY TO CLMCARRO
06258         MOVE AL-SABOF            TO CLMCARRA
041002     END-IF.
06259
PEMMOD     IF PI-PROCESSOR-ID = 'PEMA'
PEMMOD        MOVE AL-UANON            TO CERTNOA
PEMMOD                                    SUFXA
PEMMOD     END-IF.
121802*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'
041002*        NEXT SENTENCE
121802*        CONTINUE
121802*    ELSE
06263          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA
121802*    END-IF.
06264
06265
           
      * EXEC CICS SEND
06266 *        MAP     (MAP-NAME)
06267 *        MAPSET  (MAPSET-NAME)
06268 *        FROM    (EL130AO)
06269 *        ERASE
06270 *        CURSOR
06271 *    END-EXEC.
           MOVE LENGTH OF
            EL130AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00014893' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303134383933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL130AO, 
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
           
06272
06273      GO TO 9100-RETURN-TRAN.
06274
06275  8150-SEND-MAP-CURSOR.
06276      MOVE SAVE-DATE              TO RUNDTEO.
06277      MOVE EIBTIME                TO TIME-IN.
06278      MOVE TIME-OUT               TO RUNTIMEO.
           IF EIBAID = DFHPF11
              MOVE 'Y'                 TO EMI-ROLL-SWITCH
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           end-if
06279      MOVE PI-COMPANY-ID          TO COMPO.
06280      MOVE AL-PABOF               TO COMPA
06281      MOVE PI-MEMBER-CAPTION      TO MEMCAPO.
06282      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
06283      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
06284
121802*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL' AND 'LGX'
06286          MOVE SPACES             TO PRODO.
06287          MOVE AL-SANOF           TO PRODCDA.
121802*    END-IF.
06288
06289      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
06290
06291      IF NOT PI-NO-CARRIER-SECURITY
06292          MOVE PI-CARRIER-SECURITY TO CLMCARRO
06293          MOVE AL-SABOF            TO CLMCARRA
041002     END-IF.
06294
PEMMOD     IF PI-PROCESSOR-ID = 'PEMA'
PEMMOD        MOVE AL-UANON            TO CERTNOA
PEMMOD                                    SUFXA
PEMMOD     END-IF.
121802*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'
041002*        NEXT SENTENCE
121802*        CONTINUE
121802*    ELSE
06298          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA
121802*    END-IF.
06299
06300
           
      * EXEC CICS SEND
06301 *        MAP      (MAP-NAME)
06302 *        MAPSET   (MAPSET-NAME)
06303 *        FROM     (EL130AO)
06304 *        CURSOR   (PI-CURSOR)
06305 *        ERASE
06306 *    END-EXEC.
           MOVE LENGTH OF
            EL130AO
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00014941' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303134393431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL130AO, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 PI-CURSOR, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06307
06308      GO TO 9100-RETURN-TRAN.
06309
06310  8200-SEND-DATAONLY.
061013     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
06316
           IF EIBAID = DFHPF11
              MOVE 'Y'                 TO EMI-ROLL-SWITCH
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              go to 8100-send-initial-map
           end-if
06311 *    IF EIBAID = DFHPF11
06312 *       MOVE 'Y'                 TO EMI-ROLL-SWITCH
061013*       move pi-last-claim       to emi-claim-no
06313 *       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061013*       go to 8100-send-initial-map
041002*    END-IF.
           .
06317  8200-BYPASS-ERROR.
06318      MOVE SAVE-DATE              TO RUNDTEO.
06319      MOVE EIBTIME                TO TIME-IN.
06320      MOVE TIME-OUT               TO RUNTIMEO.
06321      MOVE PI-COMPANY-ID          TO COMPO.
06322      MOVE AL-PABOF               TO COMPA
06323      MOVE PI-MEMBER-CAPTION      TO MEMCAPO.
06324
121802*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL' AND 'LGX'
06326          MOVE SPACES             TO PRODO.
06327          MOVE AL-SANOF           TO PRODCDA.
121802*    END-IF.
06328
06329      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
06330      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
06331
PEMMOD     IF PI-PROCESSOR-ID = 'PEMA'
PEMMOD        MOVE AL-UANON            TO CERTNOA
PEMMOD                                    SUFXA
PEMMOD     END-IF.
06332      IF NOT PI-NO-CARRIER-SECURITY
06333         MOVE PI-CARRIER-SECURITY TO CLMCARRO
06334         MOVE AL-SABOF            TO CLMCARRA
041002     END-IF.
06335
121802*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'
041002*        NEXT SENTENCE
121802*        CONTINUE
121802*    ELSE
06339          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA
121802*    END-IF.
06340
121802*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL'
06342          MOVE AL-SANOF           TO PRODCDA.
121802*    END-IF.
06343
06344
           
      * EXEC CICS SEND
06345 *        MAP      (MAP-NAME)
06346 *        MAPSET   (MAPSET-NAME)
06347 *        FROM     (EL130AO)
06348 *        DATAONLY
06349 *        CURSOR
06350 *    END-EXEC.
           MOVE LENGTH OF
            EL130AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00015003' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL130AO, 
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
           
06351
06352      GO TO 9100-RETURN-TRAN.
06353
06354  8300-SEND-TEXT.
06355
           
      * EXEC CICS SEND TEXT
06356 *        FROM     (LOGOFF-TEXT)
06357 *        LENGTH   (LOGOFF-LENGTH)
06358 *        ERASE
06359 *        FREEKB
06360 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00015015' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303135' TO DFHEIV0(25:11)
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
           
06361
06362
           
      * EXEC CICS RETURN
06363 *    END-EXEC.
      *    MOVE '.(                    ''   #00015023' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06364
06365  8800-UNAUTHORIZED-ACCESS.
06366      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
06367      GO TO 8300-SEND-TEXT.
06368
06369  8810-PF23.
06370      MOVE EIBAID TO PI-ENTRY-CD-1.
06371      MOVE XCTL-005               TO PGM-NAME.
06372      GO TO 9300-XCTL.
06373
06374  8820-TERM-ERROR.
06375      MOVE ER-0412                TO EMI-ERROR.
06376      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
06377      MOVE -1                     TO MAINTL.
06378      GO TO 8200-SEND-DATAONLY.
06379
06380  8830-TRAN-ERROR.
06381      MOVE ER-0413                TO EMI-ERROR.
06382      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
06383      MOVE -1                     TO MAINTL.
06384      GO TO 8200-SEND-DATAONLY.
06385
06386  9000-RETURN-CICS.
06387
           
      * EXEC CICS RETURN
06388 *    END-EXEC.
      *    MOVE '.(                    ''   #00015049' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06389
06390  9100-RETURN-TRAN.
06391      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
06392      MOVE '130A'                 TO PI-CURRENT-SCREEN-NO.
06393
06394
           
      * EXEC CICS RETURN
06395 *        TRANSID   (TRANS-ID)
06396 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
06397 *        LENGTH    (PI-COMM-LENGTH)
06398 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00015057' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06399
06400  9200-RETURN-MAIN-MENU.
06401      MOVE XCTL-126               TO PGM-NAME.
06402      GO TO 9300-XCTL.
06403
06404  9300-XCTL.
06405
           
      * EXEC CICS XCTL
06406 *        PROGRAM   (PGM-NAME)
06407 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
06408 *        LENGTH    (PI-COMM-LENGTH)
06409 *    END-EXEC.
      *    MOVE '.$C                   %   #00015069' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06410
06411  9400-CLEAR.
06412      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
06413      GO TO 9300-XCTL.
06414
06415  9500-PF12.
06416      MOVE XCTL-010               TO PGM-NAME.
06417      GO TO 9300-XCTL.
06418
06419  9600-PGMID-ERROR.
06420
           
      * EXEC CICS HANDLE CONDITION
06421 *        PGMIDERR  (8300-SEND-TEXT)
06422 *    END-EXEC.
      *    MOVE '"$L                   ! N #00015085' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4E20233030303135303835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06423
06424      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
06425      MOVE ' '                    TO PI-ENTRY-CD-1.
06426      MOVE XCTL-005               TO PGM-NAME.
06427      MOVE PGM-NAME               TO LOGOFF-PGM.
06428      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
06429      GO TO 9300-XCTL.
06430
06431  9700-LINK-DATE-CONVERT.
06432      MOVE LINK-ELDATCV           TO PGM-NAME.
06433
           
      * EXEC CICS LINK
06434 *        PROGRAM    (PGM-NAME)
06435 *        COMMAREA   (DATE-CONVERSION-DATA)
06436 *        LENGTH     (DC-COMM-LENGTH)
06437 *    END-EXEC.
      *    MOVE '."C                   (   #00015099' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06438
06439  9700-EXIT.
06440      EXIT.
06441
06442  9800-LINK-REM-TERM.
06443      MOVE LINK-ELRTRM            TO PGM-NAME.
06444
           
      * EXEC CICS LINK
06445 *        PROGRAM    (PGM-NAME)
06446 *        COMMAREA   (CALCULATION-PASS-AREA)
06447 *        LENGTH     (CP-COMM-LENGTH)
06448 *    END-EXEC.
      *    MOVE '."C                   (   #00015111' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135313131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06449
06450  9800-EXIT.
06451      EXIT.
06452
06453  9900-ERROR-FORMAT.
      *    if emi-error = 1651 or 1652 or 1653 or 1655
      *       display ' 9900 error ' emi-error
      *       display emi-message-area (1)
      *       display emi-message-area (2)
      *       display emi-message-area (3)
      *    end-if
06454      IF NOT EMI-ERRORS-COMPLETE
06455          MOVE LINK-001           TO PGM-NAME
               
      * EXEC CICS LINK
06457 *            PROGRAM   (PGM-NAME)
06458 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
06459 *            LENGTH    (EMI-COMM-LENGTH)
06460 *        END-EXEC
      *    MOVE '."C                   (   #00015129' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135313239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013         if emi-error = 1651 or 1652 or 1653 or 1655 or 1660
061013            subtract 1 from emi-fatal-ctr
061013         end-if
041002     END-IF.
06461
           .
06462  9900-EXIT.
06463      EXIT.
06464
06465  9990-ABEND.
06466      MOVE -1                     TO MAINTL.
06467      MOVE LINK-004               TO PGM-NAME.
06468      MOVE DFHEIBLK               TO EMI-LINE1.
06469
           
      * EXEC CICS LINK
06470 *        PROGRAM   (PGM-NAME)
06471 *        COMMAREA  (EMI-LINE1)
06472 *        LENGTH    (72)
06473 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00015148' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135313438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06474
06475      MOVE EMI-LINE1              TO ERRMSG2O.
06476
06477      GO TO 8200-BYPASS-ERROR.
06478
06479
06480      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL130' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
06481
06482  9995-SECURITY-VIOLATION.
06483 *             COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00015179' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135313739' TO DFHEIV0(25:11)
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
06484
06485  9995-EXIT.
06486      EXIT.
06487

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL130' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 0100-TEST-ENTRY,
                     8100-SEND-INITIAL-MAP,
                     9600-PGMID-ERROR,
                     8820-TERM-ERROR,
                     8830-TRAN-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0450-ADD-ELACTQ-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8820-TERM-ERROR,
                     8830-TRAN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0660-NOT-FOUND,
                     0670-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1025-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1025-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1025-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1025-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 2050-DELETE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 2010-DELETE-CL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 3010-TEST-FOR-ERRORS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 3008-DUP-ERROR,
                     3009-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 3030-BUILD-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 3033-ACCT-NOT-FOUND,
                     3033-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 3032-NOT-IN-DT-RANGE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 3070-BUILD-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 3065-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 3100-CARRIER-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 3110-COMPANY-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 3200-BUILD-ZERO-TRAILER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 9999-DFHBACK
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 3290-NO-INSURED-ADDRESS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 3410-DUP-KEY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 3690-CERT-WRITTEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 3850-CLAIM-MSTR-WRITTEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 3992-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 5050-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 5030-CERT-WRITTEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 5035-CLAIM-WRITTEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 9990-ABEND,
                     5799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 3100-CARRIER-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 6040-NO-PROCESSOR-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 34
               GO TO 6060-NO-BENEFICIARY-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 35
               GO TO 6499-EXIT,
                     6499-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 36
               GO TO 6600-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 37
               GO TO 6700-NOTOPEN-ERROR,
                     6700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 38
               GO TO 7120-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 39
               GO TO 7499-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 40
               GO TO 7500-EXIT,
                     7500-DUPLICATE-KEY,
                     7500-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 41
               GO TO 7500-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 42
               GO TO 7630-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 43
               GO TO 7799-EXIT,
                     7799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 44
               GO TO 7790-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 45
               GO TO 7992-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 46
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL130' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
