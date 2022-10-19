00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL131 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 04/03/95 18:43:40.
00007 *                            VMOD=2.046.
00008 *
00009 *AUTHOR.        LOGIC, INC.
00010 *               DALLAS, TEXAS.
00024 *REMARKS. TRANSACTION EX24 - CLAIM MAINTENANCE.
00023 *
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID, POPULATE COMPO
062602* 062602    2002030700006  PEMA  Add note type of 'S'
062602*                                  (special review)
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
071508* 071508    2008071000003  AJRA  ADD EDIT FOR SOC SEC NUMBER
012009* 012009    2007042600001  PEMA  RESTRICT CLAIM TYPE FOR CID
032612* 032612    2011110200001  PEMA  AHL CHANGES
040412* 040412    2012040400003  AJRA  DO NOT DELETE CERT ON AHL
032613* 032613    2013032500002  AJRA  PROTECT CERT#,CARR,GRP,ST,ACCT,EF
052113* 052113    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
060413* 060413    2013060300001  AJRA  CHECK FOR AUTO PAY WHEN BENE REMO
071613* 071613    2013071200001  PEMA  REMOVE INC & RPT DATE EDITS.
080613* 080613    2013071600001  PEMA  ALLOW INC DTE CHANGE
082013* 082013    2013040900001  AJRA  ADD PF15 BENEFICIARY MAINT
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
111113* 111113    2013110600002  AJRA  CHG LEVEL 5 RESTRICTIONS TO LEVEL
040814* 040814    2014030500002  AJRA  ADD ICD CODES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
070714* 070714    2014052800001  PEMA  correct read on erpdef for DCC
031715* 031715  CR2015022700002  PEMA  CHECK CHG IN DIAGNOSIS
031815* 031815    2015021700001  TANA  BENE EDIT IF A PAYMENT PENDING
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
040616* 040617  CR2017022200003  TANA  INCREASE CHG INC DT TO 180 DAYS
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD FIELD
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
101917* 101917  CR2017083000003  TANA  ADD CONTRACT CONTESTABLE MSG
021418* 021418  CR2017110200001  TANA  EDIT NAME CHANGE AGST INSRD TYPE
052918* 052918  CR2018031500002  TANA  Add Message for filing time limit
061418* 061418  IR2018053000003  TANA  Fix Causal state / filing limit
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
011020* 011020  IR2019121100004  PEMA  ADD last name to compare.
071720* 071720  CR2019112600001  TANA  Remove filing time limit error
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
090821* 090821  CR2021081200003  PEMA  Add error if inc > act cnc dt
080322* 080322  CR2021100800003  TANA  Add B and H claim types
101501******************************************************************
00025
00026      EJECT
00027  ENVIRONMENT DIVISION.
00028
00029  DATA DIVISION.
00030
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00036
00037  77  FILLER  PIC X(32)  VALUE '********************************'.
00038  77  FILLER  PIC X(32)  VALUE '*   EL131  WORKING STORAGE     *'.
00039  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.046 *********'.
052113 77  s1                          pic s999 comp-3 value+0.
052113 77  s2                          pic s999 comp-3 value+0.
052113 77  a1                          pic s999 comp-3 value+0.
052113 77  WS-CRITICAL-PERIOD          PIC 99    VALUE ZEROS.
052113 77  WS-CRIT-PER-RTW-MOS         PIC 99    VALUE ZEROS.
052113 77  WS-EXCL-PERIOD              PIC S999 COMP-3 VALUE +0.
       77  ws-pre-exsist               pic s999 comp-3 value +0.
052113 77  WS-COV-ENDS                 PIC S999 COMP-3 VALUE +0.
052113 77  WS-ACC-PERIOD               PIC S999 COMP-3 VALUE +0.
       77  ws-max-extension            pic s999 comp-3 value +0.
       77  ws-max-moben                pic s9(7)v99 comp-3 value +0.
052113 77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
052113 77  WS-ERPDEF-SW                PIC X     VALUE ' '.
052113     88  ERPDEF-FOUND                 VALUE 'Y'.
031815 77  WS-TRLR-FILE-EOF            PIC X     VALUE ' '.
031815     88  TRLR-FILE-EOF                VALUE 'Y'.
031815 77  WS-PAYMENT-PENDING          PIC X     VALUE ' '.
031815     88  PAYMENT-PENDING              VALUE 'Y'.
062217 77  WS-AUTH-RCVD                PIC X     VALUE ' '.
062217     88  AUTH-RCVD                    VALUE 'Y'.
062217     88  NO-AUTH-RCVD                 VALUE 'N'.
       77  ws-eracct-sw                pic x  value ' '.
           88  acct-found                value 'Y'.
       77  ws-benper-sw                pic x   value ' '.
           88  good-benper               value 'Y'.
       77  ws-claim-type               pic x value ' '.
       77  ws-ben-per                  pic 99 value 00.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.
       77  ws-max-bens                 pic s9(5) comp-3 value +0.
090821 77  sub                         pic s999 comp-3 value +0.
080322 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
00040
00032  01  LCP-TIME-OF-DAY-XX.
00033      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00034      05  FILLER                    PIC 99.
00035  01  LCP-CICS-TIME                 PIC 9(15).
00041 *                            COPY ELCSCTM.
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
00042
00043 *                            COPY ELCSCRTY.
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
00044
00045 *                            COPY ELCNWA.
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
00046
052113 01  ws-dcc-error-line.
052113     05  filler occurs 15.
052113         10  ws-error-no        pic x(4).
00047  01  WS-DATE-AREA.
00048      12  SAVE-DATE              PIC X(8)    VALUE SPACES.
00049      12  SAVE-BIN-DATE          PIC XX      VALUE SPACES.
00050      12  SAVE-DATE-CCYYMMDD.
00051          16  SAVE-DATE-CC       PIC XX      VALUE SPACES.
00052          16  SAVE-DATE-YMD.
00053              20  SAVE-DATE-YY   PIC XX      VALUE SPACES.
00054              20  FILLER         PIC X(4)    VALUE SPACES.
CIDMOD
CIDMOD 01  WS-BLANK                   PIC X       VALUE ' '.
CIDMOD
CIDMOD 01  CSO-WORK-FIELDS.
CIDMOD     05  WS-HOLD-CLAIM-STATUS    PIC X.
           05  WS-CRIT-PER-RECURRENT   PIC 99    VALUE 00.
           05  WS-CRIT-PER-ALPHA REDEFINES WS-CRIT-PER-RECURRENT.
               10  WS-RECURRENT-YN     PIC X.
               10  FILLER              PIC X.
00055
00056  01  LITERALS-NUMBERS.
00057      12  SC-ITEM                 PIC S9(4)   VALUE +1    COMP.
00058      12  NINETY                  PIC S9(4)   VALUE +90   COMP.
00059      12  ELMSTR-GENERIC-LENGTH   PIC S9(4)   VALUE +9    COMP.
00060      12  GETMAIN-SPACE           PIC X       VALUE SPACE.
00061      12  THIS-PGM                PIC X(8)    VALUE 'EL131'.
00062      12  TRAN-ID                 PIC X(4)    VALUE 'EX24'.
00063      12  MAP-ID                  PIC X(4)    VALUE '131A'.
082013     12  MAP-LENGTH              PIC S9(4)   VALUE +1060  COMP.
00064
00065      12  XCTL-EL001              PIC X(8)    VALUE 'EL001'.
00066      12  XCTL-EL004              PIC X(8)    VALUE 'EL004'.
00067      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
00068      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
082013     12  XCTL-EL114              PIC X(8)    VALUE 'EL114'.
00069      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
00070      12  XCTL-EL1273             PIC X(8)    VALUE 'EL1273'.
00071      12  XCTL-EL141              PIC X(8)    VALUE 'EL141'.
00072      12  XCTL-EL142              PIC X(8)    VALUE 'EL142'.
00073      12  XCTL-EL400DMD           PIC X(8)    VALUE 'EL400DMD'.
00074
00075      12  LINK-1523               PIC X(8)    VALUE 'EL1523'.
00076
00077      12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.
00078
00079      12  RTRM-FILE-ID            PIC X(8)    VALUE 'ELRTRM'.
00080      12  CLMS-FILE-ID            PIC X(8)    VALUE 'ELMSTR'.
00081      12  NOTE-FILE-ID            PIC X(8)    VALUE 'ERNOTE'.
00082      12  CERT-FILE-ID            PIC X(8)    VALUE 'ELCERT'.
00083      12  TRLR-FILE-ID            PIC X(8)    VALUE 'ELTRLR'.
00084      12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.
00085      12  ACTQ-FILE-ID            PIC X(8)    VALUE 'ELACTQ'.
00086      12  CHKQ-FILE-ID            PIC X(8)    VALUE 'ELCHKQ'.
00087      12  ACCT-FILE-ID            PIC X(8)    VALUE 'ERACCT'.
00088      12  ARCH-FILE-ID            PIC X(8)    VALUE 'ELARCH'.
00089      12  ALPH-FILE-ID            PIC X(8)    VALUE 'ELALPH'.
101917     12  WS-CONTEST-NOTE      PIC X(44)
101917         VALUE 'CONTRACT NOW CONTESTABLE - INC/RPT DT CHGD'.
052918     12  WS-FILE-LIM-NOTE     PIC X(44)
052918         VALUE 'FILING TIME LIMIT OK - INCRD DT CHGD'.
061418
061418     12  WS-FILING-NOTE     PIC X(30)
061418         VALUE 'FILING TIME LIMIT EXCEEDED'.
00090
00091      12  MISC-SUB                PIC S9(3)   COMP-3  VALUE +0.
00092      12  ONE-OR-MIN1             PIC S9      COMP-3  VALUE +1.
00093      12  WS-READNEXT-SWITCH      PIC S99     VALUE +1.
00094      12  WS-ASSOC-CERT-TOTAL     PIC S99             VALUE ZEROS.
00095          88  NO-ASSOCIATED-CERTS                     VALUE ZEROS.
00096      12  WS-ASSOC-CERT-SEQU      PIC S99             VALUE ZEROS.
00097      12  WS-ASSOCIATED-CERTS     PIC S9      COMP-3  VALUE +0.
00098      12  WS-DIAGNOSIS            PIC X(60)   VALUE SPACES.
040814     12  WS-ICD-CODE-1           PIC X(8).
040814     12  WS-ICD-CODE-2           PIC X(8).
00099      12  WS-CLAIM-SEQU.
00100          16  FILLER              PIC X VALUE '('.
00101          16  WS-CURRENT-SEQU     PIC Z9.
00102          16  FILLER              PIC X(04) VALUE ' OF '.
00103          16  WS-OF-SEQU          PIC Z9.
00104          16  FILLER              PIC X VALUE ')'.
00105      12  WS-EDIT-BEN-CODE        PIC XX.
00106          88  INVALID-BENEFIT-CODE   VALUE '  ' '00'
00107                                           '90' THRU '99'.
00108
00109      12  WS-EDIT-AMT             PIC ZZZ,ZZZ.99.
00110      12  WS-EDIT-NUMBER          PIC ZZZ9.
00111
00152      12  WS-RESPONSE             PIC S9(8)   COMP.
00153          88  WS-RESP-NORMAL              VALUE +00.
052113         88  WS-RESP-ERROR               VALUE +01.
052113         88  WS-RESP-NOTFND              VALUE +13.
052113         88  WS-RESP-DUPKEY              VALUE +15.
052113         88  WS-RESP-NOTOPEN             VALUE +19.
052113         88  WS-RESP-ENDFILE             VALUE +20.
00115
00116      12  WS-MOS-PAID             PIC 999.
00117      12  WS-ODD-DAYS             PIC 99.
00118      12  WS-REM-MOS              PIC 999.
00119      12  WS-REM-DAYS             PIC 99.
00120      12  WS-TERM-IN-DAYS         PIC 9(4).
00121      12  WS-REM-TERM-IN-DAYS     PIC 9(4).
00122
00123      12  WS-BROWSE-SW            PIC X       VALUE 'N'.
00124      12  WS-UPDATE-SW            PIC X       VALUE 'N'.
00125      12  WS-OPEN-CLOSE-SW        PIC X       VALUE ' '.
00126      12  WS-RESET-SW             PIC X       VALUE 'N'.
00127      12  WS-REC-FOUND-SW         PIC X       VALUE 'N'.
00128      12  WS-DMO-LENGTH           PIC S9(4)   VALUE +108 COMP.
00129      12  WS-DCT-LENGTH           PIC S9(4)   VALUE +53 COMP.
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
090821     05  er-1679-text        pic x(60) value
090821       '1679-N CONTRACT IS NOT CONTESTABLE'.
090821     05  er-1682-text        pic x(60) value
090821       '1682-N INC DATE > MOB ACCOUNT CANCEL DATE'.
090821     05  er-1683-text        pic x(60) value
090821       '1683-N INC DATE < CERTIFICATE EFF DATE'.
090821     05  ws-rec-type             pic x.
090821     05  ws-ben-hold             pic xx.
090821     05  ws-special-calc-cd      pic x.
00131  01  DMD-DATE-YYYYMMDD.
00132      12  DMD-DECADE          PIC XX      VALUE SPACES.
00133      12  DMD-YYMMDD.
00134          16  DMD-YY          PIC XX      VALUE SPACES.
00135          16  DMD-MM          PIC XX      VALUE SPACES.
00136          16  DMD-DD          PIC XX      VALUE SPACES.
00137
00138  01  DMD-DATE-MMDDYYYY.
00139      12  DMD-MDY-MM          PIC XX      VALUE SPACES.
00140      12  DMD-MDY-SLASH1      PIC X       VALUE '/'.
00141      12  DMD-MDY-DD          PIC XX      VALUE SPACES.
00142      12  DMD-MDY-SLASH2      PIC X       VALUE '/'.
00143      12  DMD-MDY-DECADE      PIC XX      VALUE SPACES.
00144      12  DMD-MDY-YY          PIC XX      VALUE SPACES.
00145
00146  01  WS-MAIL-CODE.
00147      12  FILLER              PIC X.
00148      12  DMD-MAIL-CODE.
00149          16  WS-MAIL-4       PIC X(4).
00150          16  WS-MAIL-5       PIC X.
00151      12  FILLER              PIC X(4).
00152
00153      12  W-NAME-LAST             PIC  X(15).
00154      12  W-NAME-FIRST            PIC  X(15).
00155      12  W-NAME-MIDDLE.
00156          16  FILLER              PIC  X.
00157          16  W-NAME-MIDDLE-2     PIC  X.
00158          16  FILLER              PIC  X(13).
00159
00160 *    COPY ELCDCTB.
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
00161      EJECT
00162  01  ERROR-SWITCHES.
00163      12  ERROR-SWITCH            PIC X.
00164          88  SCREEN-ERROR                    VALUE 'X'.
00165      12  MSTR-KEY-SWITCH         PIC X.
00166          88  MSTR-KEY-CHANGED                VALUE 'X'.
00167      12  CERT-KEY-SWITCH         PIC X.
00168 *********SET TO X IF ALTERNATE INDEXES WILL CHANGE
00169          88  CERT-ALT-KEY-CHANGED            VALUE 'X'.
00170 *********SET TO Y IF PRIMARY KEY WILL CHANGE
00171          88  CERT-KEY-CHANGED                VALUE 'Y'.
00172      12  MSTR-SWITCH             PIC X.
00173          88  MSTR-UPDATES                    VALUE 'X'.
           12  crtt-switch             pic x value spaces.
               88  crtt-update             value 'X'.
00174      12  CERT-SWITCH             PIC X.
00175          88  CERT-UPDATES                    VALUE 'X'.
00176      12  UPDATE-SWITCH           PIC X.
00177          88  UPDATES-PRESENT                 VALUE 'X'.
00178      12  TRLR-SWITCH             PIC X.
00179          88  TRLR-UPDATE-REQUIRED            VALUE 'X'.
00180          88  UPDATE-MADE                     VALUE 'Y'.
00181      12  NINETY-TRLR-SWITCH      PIC X.
00182          88  NINETY-TRLR-UPDATED             VALUE 'X'.
052113     12  NINETY5-TRLR-SWITCH      PIC X  VALUE ' '.
052113         88  NINETY5-TRLR-UPDATED             VALUE 'X'.
00183      12  EARNINGS-CALC-SWITCH    PIC X.
00184          88  TEX-REG                         VALUE '4'.
00185          88  NET-PAY                         VALUE '5'.
CIDMOD     12  DLYACTV-SW              PIC X       VALUE 'N'.
CIDMOD         88  DLYACTV-RECORD-NEEDED           VALUE 'Y'.
00186      EJECT
00187  01  EDIT-WORK-AREA.
00188      12  COUNT-2                 PIC 9.
00189      12  CALL-PGM                PIC X(8).
00190      12  TRANS-ID                PIC X(4).
00191      12  CHECK-PFKEYS            PIC 99.
00192      12  CHECK-MAINT             PIC X.
00193          88  ADD-ALPHA-RECORD                VALUE 'A'.
00194          88  CHANGE-CLAIM                    VALUE 'C'.
00195          88  DELETE-CLAIM                    VALUE 'D'.
00196          88  CHANGE-NAME                     VALUE 'N'.
00197          88  VALID-OPTIONS                   VALUE 'A' 'C'
00198                                                    'D' 'N'.
00199
00200      12  HOLD-BENEFIT            PIC XX.
00201      12  HOLD-FREQ               PIC XX.
052113     12  HOLD-REPORTED           PIC XX  VALUE LOW-VALUES.
052113     12  HOLD-INCUR              PIC XX  VALUE LOW-VALUES.
00204      12  HOLD-PDTHRU             PIC XX.
00205      12  HOLD-ADDON              PIC XX.
00206      12  HOLD-NODAYS             PIC 9(4).
00207      12  HOLD-NOPMTS             PIC 9(3).
00208      12  HOLD-PDAMT              PIC 9(7)V99    COMP-3.
00209      12  HOLD-LOANBAL            PIC 9(7)V99    COMP-3  VALUE 0.
00210      12  HOLD-APR                PIC 9(3)V9(4)  COMP-3  VALUE 0.
00211      12  HOLD-BIRTH              PIC XX.
00212      12  HOLD-END                PIC XX.
00213      12  HOLD-EFF                PIC XX.
00214      12  HOLD-ENTRY              PIC XX.
00215      12  HOLD-LF-CV-BEN          PIC S9(9)V99   COMP-3  VALUE +0.
00216      12  HOLD-LF-RATE            PIC S9(4)V99   COMP-3  VALUE +0.
00217      12  HOLD-AH-CV-BEN          PIC S9(9)V99   COMP-3  VALUE +0.
00218      12  HOLD-AH-RATE            PIC S9(4)V99   COMP-3  VALUE +0.
00219      12  HOLD-LF-CV-CAN          PIC XX.
00220      12  HOLD-AH-CV-CAN          PIC XX.
00221      12  DIVIDE-QUOT             PIC 9(3).
00222      12  DIVIDE-REM              PIC 9(3).
00223
00224      12  WS-ALPHA-DATE.
00225          16  WS-ALPHA-YEAR.
00226              20  WS-ALPHA-YY-1   PIC 99.
00227              20  WS-ALPHA-YY-2   PIC 99.
00228          16  WS-ALPHA-MM         PIC 99.
00229          16  WS-ALPHA-DD         PIC 99.
00230
00231      12  BUILD-VALID-DATE.
00232          16  BUILD-MONTH         PIC 99.
00233          16  BUILD-DAY           PIC 99.
00234          16  BUILD-YEAR          PIC 99.
00235      12  DEEDIT-MMYY-INPUT.
00236          16  FILLER              PIC X.
00237          16  DEEDIT-MONTH        PIC XX.
00238          16  DEEDIT-YEAR         PIC XX.
00239      12  DEEDIT-BEN              PIC 9(9)V99.
00240      12  DEEDIT-DATE-INPUT.
00241          16  FILLER              PIC XX.
00242          16  DEEDIT-DATE         PIC X(6).
00243      12  WORK-DATE-MDY.
00244          16  MONTH-WORK          PIC XX.
00245          16  FILLER              PIC X(4).
00246          16  YEAR-WORK           PIC XX.
00247      12  WORK-DATE-MY.
00248          16  WORK-MONTH          PIC XX.
00249          16  WORK-SLASH          PIC X       VALUE '/'.
00250          16  WORK-YEAR           PIC XX.
00251      12  HOLD-DATE.
00252          16  HOLD-MONTH          PIC 99.
00253          16  FILLER              PIC X(4).
00254          16  HOLD-YEAR           PIC 99.
00255      12  SPLIT-INFO-LINE-1.
00256          16  SPLIT-INFO-DESC     PIC X(15).
00257          16  FILLER              PIC X(5)    VALUE ' OLD='.
00258          16  SPLIT-INFO-OLD      PIC X(40).
00259      12  EFF-TERM-MO             PIC 9(4).
00260      12  CURR-MO                 PIC 9(4).
00261      12  WS-BIN-CURRENT-DT       PIC XX.
00262      12  WS-CALC-METHOD          PIC X.
00263      12  WS-INITIALS.
00264          16  WS-INIT-1           PIC X.
00265          16  WS-INIT-2           PIC X.
00266      12  WS-REIN-TABLE.
00267          16  WS-REIN-1           PIC X.
00268          16  WS-REIN-2           PIC X.
00269          16  WS-REIN-3           PIC X.
00270      12  PI-KEY.
00271          16  PI-TERM             PIC X(4).
00272          16  PI-QUAL             PIC X(4).
00273      12  CHECK-KEY               PIC X(20).
00274      12  SAVE-ACCT-KEY           PIC X(20).
00275      12  SKIP-ATTRIBUTE          PIC X      VALUE SPACES.
071508     12  WS-SOC-SEC-NUMBER.
071508         16  WS-SOC-SEC-NO       PIC 9(9).
071508         16  WS-SOC-SEC-BLANK    PIC X(2).
071508     12  WS-SOC-SEC-REDEF REDEFINES WS-SOC-SEC-NUMBER.
071508         16  WS-SSN-1-3          PIC 9(3).
071508         16  WS-SSN-DASH1        PIC X(1).
071508         16  WS-SSN-4-5          PIC 9(2).
071508         16  WS-SSN-DASH2        PIC X(1).
071508         16  WS-SSN-6-9          PIC 9(4).
060413     12  AUTO-PAY-TO-BENE        PIC X     VALUE 'N'.
082013     12  WS-RETURNED-FROM        PIC X(8)  VALUE SPACES.
082013
082013 01  WS-SAVE-FIELDS.
082013     12  WS-SAVE-BENEFICIARY     PIC X(10) VALUE LOW-VALUES.
082013     12  WS-SAVE-MAINTI          PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-MAINTL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-STATUSI         PIC X(6) VALUE LOW-VALUES.
082013     12  WS-SAVE-STATUSL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-REPI            PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-REPL            PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-CAUSEI          PIC X(6) VALUE LOW-VALUES.
082013     12  WS-SAVE-CAUSEL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-ENDI            PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-ENDL            PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-DIAGI           PIC X(60) VALUE LOW-VALUES.
082013     12  WS-SAVE-DIAGL           PIC S9(4) COMP VALUE +0.
040814     12  WS-SAVE-ICD1I           PIC X(8)  VALUE LOW-VALUES.
040814     12  WS-SAVE-ICD1L           PIC S9(4) COMP VALUE +0.
040814     12  WS-SAVE-ICD2I           PIC X(8)  VALUE LOW-VALUES.
040814     12  WS-SAVE-ICD2L           PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-BENEI           PIC X(10) VALUE LOW-VALUES.
082013     12  WS-SAVE-BENEL           PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-BIRTHI          PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-BIRTHL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-SOCIALI         PIC X(11) VALUE LOW-VALUES.
082013     12  WS-SAVE-SOCIALL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-SEXI            PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-SEXL            PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-MLNAMEI         PIC X(15) VALUE LOW-VALUES.
082013     12  WS-SAVE-MLNAMEL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-MFNAMEI         PIC X(12) VALUE LOW-VALUES.
082013     12  WS-SAVE-MFNAMEL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-MMINITI         PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-MMINITL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-LOANNOI         PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-LOANNOL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-LOANBALI        PIC 9(7)V99 VALUE ZEROS.
082013     12  WS-SAVE-LOANBALL        PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-PROCI           PIC X(4) VALUE LOW-VALUES.
082013     12  WS-SAVE-PROCL           PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-SUPVI           PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-SUPVL           PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-PRICDI          PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-PRICDL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-FILETOI         PIC X(4) VALUE LOW-VALUES.
082013     12  WS-SAVE-FILETOL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-PDTHRUI         PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-PDTHRUL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-PDAMTI          PIC 9(7)V99 VALUE ZEROS.
082013     12  WS-SAVE-PDAMTL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-NODAYSI         PIC 9(5) VALUE ZEROS.
082013     12  WS-SAVE-NODAYSL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-NOPMTSI         PIC 9(4) VALUE ZEROS.
082013     12  WS-SAVE-NOPMTSL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-FORMTYPI        PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-FORMTYPL        PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-OCCI            PIC X(6) VALUE LOW-VALUES.
082013     12  WS-SAVE-OCCL            PIC S9(4) COMP VALUE +0.
00276
00277  01  TERM-CALCULATION-WORK-AREA     COMP-3.
00278      12  M                   PIC S9(7)V99           VALUE ZEROS.
00279      12  L                   PIC S9(7)V99           VALUE ZEROS.
00280      12  N                   PIC S9(3)              VALUE ZEROS.
00281      12  N-STORE             PIC S9(3)              VALUE ZEROS.
00282      12  NV-STORE            PIC S9(3)              VALUE ZEROS.
00283      12  I                   PIC S99V9(5)           VALUE ZEROS.
00284      12  A-N                 PIC S9(7)V9(8)         VALUE ZEROS.
00285      12  IA-N                PIC S9(7)V9(8)         VALUE ZEROS.
00286      12  V                   PIC S9(3)V9(14)        VALUE ZEROS.
00287      12  R                   PIC S9(3)              VALUE ZEROS.
00288      12  M1                  PIC S9(7)V99           VALUE ZEROS.
00289      12  V-EX-N              PIC S9(3)V9(14)        VALUE ZEROS.
00290      12  TERM1               PIC S9(8)V9(9)         VALUE ZEROS.
00291      12  TERM2               PIC S9(8)V9(9)         VALUE ZEROS.
00292      12  TERM3               PIC S9(8)V9(9)         VALUE ZEROS.
00293      12  TERM4               PIC S9(3)V9(14)        VALUE ZEROS.
00294      12  LEFT-TOT-1          PIC S9(9)V9(8)         VALUE ZEROS.
00295      12  RIGHT-TOT-1         PIC S9(9)V9(8)         VALUE ZEROS.
00296      12  RIGHT-TOT-2         PIC S9(9)V9(8)         VALUE ZEROS.
00297
00298  01  TERM-CALC-WORK-AREA.
00299      12  WS-AH-RATE          PIC S999V9(5)          VALUE ZEROS.
00300      12  WS-LF-RATE          PIC S999V9(5)          VALUE ZEROS.
00301      12  WS-TERM             PIC S9(3)              VALUE ZEROS.
00302      12  WS-TERM-REM         PIC S9(3)V99           VALUE ZEROS.
00303      12  WS-REMAIN           PIC S99                VALUE ZEROS.
00304      12  V-EXPONENTS.
00305         14  V-EXPONENT       PIC S9(3)V9(14) COMP-3 OCCURS 250.
00306      12  V-EX-ONETIME        PIC 9                  VALUE 1.
00307
00308  01  TIME-IN.
00309      12  UN-HOURS                PIC XX.
00310      12  UN-MINUTES              PIC XX.
00311      12  FILLER                  PIC XX.
00312
00313  01  TIME-OUT.
00314      12  FOR-HOURS               PIC XX.
00315      12  FILLER                  PIC X       VALUE '.'.
00316      12  FOR-MINUTES             PIC XX.
00317
00318  01  ERROR-NUMBERS.
00319      12  ER-0000                 PIC X(4)    VALUE '0000'.
00320      12  ER-0004                 PIC X(4)    VALUE '0004'.
00321      12  ER-0008                 PIC X(4)    VALUE '0008'.
00322      12  ER-0023                 PIC X(4)    VALUE '0023'.
00323      12  ER-0029                 PIC X(4)    VALUE '0029'.
00324      12  ER-0068                 PIC X(4)    VALUE '0068'.
00325      12  ER-0070                 PIC X(4)    VALUE '0070'.
00326      12  ER-0149                 PIC X(4)    VALUE '0149'.
00327      12  ER-0154                 PIC X(4)    VALUE '0154'.
00328      12  ER-0169                 PIC X(4)    VALUE '0169'.
00329      12  ER-0192                 PIC X(4)    VALUE '0192'.
00330      12  ER-0199                 PIC X(4)    VALUE '0199'.
00331      12  ER-0203                 PIC X(4)    VALUE '0203'.
00332      12  ER-0204                 PIC X(4)    VALUE '0204'.
00333      12  ER-0205                 PIC X(4)    VALUE '0205'.
00334      12  ER-0206                 PIC X(4)    VALUE '0206'.
00335      12  ER-0208                 PIC X(4)    VALUE '0208'.
00336      12  ER-0210                 PIC X(4)    VALUE '0210'.
00337      12  ER-0215                 PIC X(4)    VALUE '0215'.
00338      12  ER-0219                 PIC X(4)    VALUE '0219'.
00339      12  ER-0220                 PIC X(4)    VALUE '0220'.
00340      12  ER-0222                 PIC X(4)    VALUE '0222'.
00341      12  ER-0223                 PIC X(4)    VALUE '0223'.
00342      12  ER-0224                 PIC X(4)    VALUE '0224'.
00343      12  ER-0227                 PIC X(4)    VALUE '0227'.
00344      12  ER-0230                 PIC X(4)    VALUE '0230'.
00345      12  ER-0232                 PIC X(4)    VALUE '0232'.
00346      12  ER-0233                 PIC X(4)    VALUE '0233'.
00347      12  ER-0236                 PIC X(4)    VALUE '0236'.
00348      12  ER-0237                 PIC X(4)    VALUE '0237'.
00349      12  ER-0238                 PIC X(4)    VALUE '0238'.
00350      12  ER-0240                 PIC X(4)    VALUE '0240'.
00351      12  ER-0241                 PIC X(4)    VALUE '0241'.
00352      12  ER-0243                 PIC X(4)    VALUE '0243'.
00353      12  ER-0244                 PIC X(4)    VALUE '0244'.
00354      12  ER-0246                 PIC X(4)    VALUE '0246'.
00355      12  ER-0247                 PIC X(4)    VALUE '0247'.
00356      12  ER-0248                 PIC X(4)    VALUE '0248'.
00357      12  ER-0250                 PIC X(4)    VALUE '0250'.
00358      12  ER-0251                 PIC X(4)    VALUE '0251'.
00359      12  ER-0253                 PIC X(4)    VALUE '0253'.
00360      12  ER-0256                 PIC X(4)    VALUE '0256'.
00361      12  ER-0257                 PIC X(4)    VALUE '0257'.
00362      12  ER-0258                 PIC X(4)    VALUE '0258'.
00363      12  ER-0259                 PIC X(4)    VALUE '0259'.
00364      12  ER-0260                 PIC X(4)    VALUE '0260'.
00365      12  ER-0261                 PIC X(4)    VALUE '0261'.
00366      12  ER-0262                 PIC X(4)    VALUE '0262'.
00367      12  ER-0263                 PIC X(4)    VALUE '0263'.
00368      12  ER-0273                 PIC X(4)    VALUE '0273'.
00369      12  ER-0274                 PIC X(4)    VALUE '0274'.
00370      12  ER-0276                 PIC X(4)    VALUE '0276'.
00371      12  ER-0282                 PIC X(4)    VALUE '0282'.
00372      12  ER-0283                 PIC X(4)    VALUE '0283'.
00373      12  ER-0309                 PIC X(4)    VALUE '0309'.
00374      12  ER-0310                 PIC X(4)    VALUE '0310'.
00375      12  ER-0311                 PIC X(4)    VALUE '0311'.
00376      12  ER-0333                 PIC X(4)    VALUE '0333'.
00377      12  ER-0360                 PIC X(4)    VALUE '0360'.
00378      12  ER-0377                 PIC X(4)    VALUE '0377'.
00379      12  ER-0402                 PIC X(4)    VALUE '0402'.
00380      12  ER-0416                 PIC X(4)    VALUE '0416'.
00381      12  ER-0426                 PIC X(4)    VALUE '0426'.
00382      12  ER-0427                 PIC X(4)    VALUE '0427'.
00383      12  ER-0428                 PIC X(4)    VALUE '0428'.
00384      12  ER-0429                 PIC X(4)    VALUE '0429'.
00385      12  ER-0430                 PIC X(4)    VALUE '0430'.
052113     12  ER-0458                 PIC X(4)    VALUE '0458'.
00386      12  ER-0475                 PIC X(4)    VALUE '0475'.
00387      12  ER-0491                 PIC X(4)    VALUE '0491'.
00388      12  ER-0509                 PIC X(4)    VALUE '0509'.
00389      12  ER-0510                 PIC X(4)    VALUE '0510'.
052113     12  ER-0511                 PIC X(4)    VALUE '0511'.
052113     12  ER-0512                 PIC X(4)    VALUE '0512'.
00390      12  ER-0519                 PIC X(4)    VALUE '0519'.
00391      12  ER-0520                 PIC X(4)    VALUE '0520'.
00392      12  ER-0521                 PIC X(4)    VALUE '0521'.
00393      12  ER-0547                 PIC X(4)    VALUE '0547'.
00394      12  ER-0548                 PIC X(4)    VALUE '0548'.
00395      12  ER-0549                 PIC X(4)    VALUE '0549'.
00396      12  ER-0565                 PIC X(4)    VALUE '0565'.
00397      12  ER-0598                 PIC X(4)    VALUE '0598'.
00398      12  ER-0639                 PIC X(4)    VALUE '0639'.
00399      12  ER-0797                 PIC X(4)    VALUE '0797'.
00400      12  ER-0802                 PIC X(4)    VALUE '0802'.
00401      12  ER-0803                 PIC X(4)    VALUE '0803'.
00402      12  ER-0849                 PIC X(4)    VALUE '0849'.
00403      12  ER-0885                 PIC X(4)    VALUE '0885'.
071508     12  ER-0887                 PIC X(4)    VALUE '0887'.
00404      12  ER-0919                 PIC X(4)    VALUE '0919'.
00405      12  ER-0921                 PIC X(4)    VALUE '0921'.
00406      12  ER-0938                 PIC X(4)    VALUE '0938'.
00407      12  ER-0946                 PIC X(4)    VALUE '0946'.
00408      12  ER-0947                 PIC X(4)    VALUE '0947'.
00409      12  ER-0948                 PIC X(4)    VALUE '0948'.
00410      12  ER-0949                 PIC X(4)    VALUE '0949'.
00411      12  ER-0950                 PIC X(4)    VALUE '0950'.
00412      12  ER-0951                 PIC X(4)    VALUE '0951'.
00413      12  ER-0954                 PIC X(4)    VALUE '0954'.
00414      12  ER-0974                 PIC X(4)    VALUE '0974'.
00415      12  ER-0975                 PIC X(4)    VALUE '0975'.
040814     12  ER-0992                 PIC X(4)    VALUE '0992'.
031715     12  er-1581                 pic x(4)    value '1581'.
052113     12  ER-1651                 PIC X(4)    VALUE '1651'.
052113     12  ER-1652                 PIC X(4)    VALUE '1652'.
052113     12  ER-1653                 PIC X(4)    VALUE '1653'.
052113     12  ER-1654                 PIC X(4)    VALUE '1654'.
052113     12  ER-1655                 PIC X(4)    VALUE '1655'.
052113     12  er-1656                 pic x(4)    value '1656'.
052113     12  ER-1661                 PIC X(4)    VALUE '1661'.
052113     12  ER-1662                 PIC X(4)    VALUE '1662'.
080613     12  ER-1663                 PIC X(4)    VALUE '1663'.
           12  er-1664                 pic x(4)    value '1664'.
           12  er-1665                 pic x(4)    value '1665'.
           12  er-1666                 pic x(4)    value '1666'.
           12  er-1668                 pic x(4)    value '1668'.
           12  er-1669                 pic x(4)    value '1669'.
021418     12  er-1675                 pic x(4)    value '1675'.
           12  er-1676                 pic x(4)    value '1676'.
           12  er-1677                 pic x(4)    value '1677'.
081817     12  ER-1678                 PIC X(4)    VALUE '1678'.
101917     12  ER-1679                 PIC X(4)    VALUE '1679'.
090821     12  er-1682                 pic x(4)    value '1682'.
090821     12  er-1683                 pic x(4)    value '1683'.
031815     12  ER-1772                 PIC X(4)    VALUE '1772'.
060413     12  ER-1773                 PIC X(4)    VALUE '1773'.
081817     12  ER-1778                 PIC X(4)    VALUE '1778'.
00416      12  ER-2280                 PIC X(4)    VALUE '2280'.
00417      12  ER-2878                 PIC X(4)    VALUE '2878'.
052918     12  ER-7572                 PIC X(4)    VALUE '7572'.
00418      12  ER-7641                 PIC X(4)    VALUE '7641'.
00419      12  ER-7642                 PIC X(4)    VALUE '7642'.
00420      12  ER-7650                 PIC X(4)    VALUE '7650'.
00421      12  ER-7651                 PIC X(4)    VALUE '7651'.
00422      12  ER-7687                 PIC X(4)    VALUE '7687'.
00423      12  ER-7689                 PIC X(4)    VALUE '7689'.
00424      12  ER-7690                 PIC X(4)    VALUE '7690'.
00425      12  ER-7691                 PIC X(4)    VALUE '7691'.
00426      12  ER-8003                 PIC X(4)    VALUE '8003'.
00426      12  ER-8051                 PIC X(4)    VALUE '8051'.
00427      12  ER-8052                 PIC X(4)    VALUE '8052'.
00428      12  ER-8053                 PIC X(4)    VALUE '8053'.
00429      12  ER-8054                 PIC X(4)    VALUE '8054'.
00430      12  ER-8055                 PIC X(4)    VALUE '8055'.
00431      12  ER-8056                 PIC X(4)    VALUE '8056'.
00432      12  ER-8057                 PIC X(4)    VALUE '8057'.
00433      12  ER-8058                 PIC X(4)    VALUE '8058'.
00434      12  ER-8059                 PIC X(4)    VALUE '8059'.
00435      12  ER-8060                 PIC X(4)    VALUE '8060'.
00436      12  ER-8061                 PIC X(4)    VALUE '8061'.
00437      12  ER-8062                 PIC X(4)    VALUE '8062'.
00438      12  ER-8063                 PIC X(4)    VALUE '8063'.
00439      12  ER-8064                 PIC X(4)    VALUE '8064'.
00440      12  ER-8065                 PIC X(4)    VALUE '8065'.
00441      12  ER-8066                 PIC X(4)    VALUE '8066'.
00442      12  ER-8152                 PIC X(4)    VALUE '8152'.
00443      12  ER-8153                 PIC X(4)    VALUE '8153'.
00444      12  ER-8154                 PIC X(4)    VALUE '8154'.
00445      12  ER-8155                 PIC X(4)    VALUE '8155'.
052113 01  ERPDEF-KEY-SAVE             PIC X(18).
052113 01  ERPDEF-KEY.
052113     12  ERPDEF-COMPANY-CD       PIC X.
052113     12  ERPDEF-STATE            PIC XX.
052113     12  ERPDEF-PROD-CD          PIC XXX.
052113     12  F                       PIC X(7).
052113     12  ERPDEF-BEN-TYPE         PIC X.
052113     12  ERPDEF-BEN-CODE         PIC XX.
052113     12  ERPDEF-EXP-DT           PIC XX.
00447  01  MSTR-KEY.
00448      12  COMPANY-CODE            PIC X.
00449      12  CARRIER-CODE            PIC X.
00450      12  CLAIM-NO                PIC X(7).
00451      12  CERT-NO.
00452          16  CERT-NO-PRIME       PIC X(10).
00453          16  CERT-NO-SUFX        PIC X.
00454
00455  01  WS-SAVE-CLAIM-KEY.
00456      12  WS-SAVE-COMPANY-CD      PIC X.
00457      12  WS-SAVE-CARRIER         PIC X.
00458      12  WS-SAVE-CLAIM-NO        PIC X(7).
00459      12  WS-SAVE-CERT-NO.
00460          16  WS-CERT-NO-PRIME    PIC X(10).
00461          16  WS-CERT-NO-SUFX     PIC X.
00462
00463  01  CERT-KEY.
00464      12  CERT-COMPANY-CODE       PIC X.
00465      12  CERT-CARRIER            PIC X.
00466      12  CERT-GROUP              PIC X(6).
00467      12  CERT-STATE              PIC XX.
00468      12  CERT-ACCOUNT            PIC X(10).
00469      12  CERT-DATE               PIC XX.
00470      12  CERT-CERT.
00471          16  CERT-CERT-PRIME     PIC X(10).
00472          16  CERT-CERT-SUFX      PIC X.
00473
052113 01  ELCRTT-KEY.
052113     05  CTRLR-COMP-CD       PIC X.
052113     05  CTRLR-CARRIER       PIC X.
052113     05  CTRLR-GROUPING      PIC X(6).
052113     05  CTRLR-STATE         PIC X(2).
052113     05  CTRLR-ACCOUNT       PIC X(10).
052113     05  CTRLR-EFF-DT        PIC XX.
052113     05  CTRLR-CERT-NO       PIC X(11).
052113     05  CTRLR-REC-TYPE      PIC X.
00474  01  NOTE-KEY.
00475      12  NOTE-COMP-CD            PIC X.
00476      12  NOTE-CERT-KEY.
00477          16  NOTE-CARRIER        PIC X.
00478          16  NOTE-GROUP          PIC X(6).
00479          16  NOTE-STATE          PIC XX.
00480          16  NOTE-ACCOUNT        PIC X(10).
00481          16  NOTE-DATE           PIC XX.
00482          16  NOTE-CERT-NO        PIC X(11).
00483
00484  01  TRLR-KEY.
00485      12  TRLR-MAIN-KEY.
00486          16  TRLR-COMPANY-CD     PIC X.
00487          16  TRLR-CARRIER        PIC X.
00488          16  TRLR-CLAIM-NO       PIC X(07).
00489          16  TRLR-CERT-NO        PIC X(11).
00490      12  TRLR-SEQ-NO             PIC S9(4)   COMP.
00491
00492  01  BENEFIT-KEY.
00493      12  BEN-CO-ID               PIC X(3).
00494      12  BEN-REC-TYPE            PIC X.
00495      12  FILLER                  PIC XX.
00496      12  BEN-ACC-CD              PIC XX.
00497      12  BEN-SEQ-NO              PIC S9(4)   COMP.
00498
00499  01  CNTL-KEY.
00500      12  CNTL-CO-ID              PIC X(3).
00501      12  CNTL-REC-TYPE           PIC X.
00502      12  CNTL-PROC-ID            PIC X(4).
00503      12  CNTL-STATE-ACCESS REDEFINES CNTL-PROC-ID.
00504          16  CNTL-STATE-NUMBER   PIC XX.
00505          16  FILLER              PIC XX.
00506      12  CNTL-CARRIER-ACCESS REDEFINES CNTL-PROC-ID.
00507          16  FILLER              PIC X(3).
00508          16  CNTL-CARRIER        PIC X.
090821     12  CNTL-bene-ACCESS REDEFINES CNTL-PROC-ID.
090821         16  filler              PIC XX.
090821         16  cntl-benefit        PIC XX.
00509      12  CNTL-SEQ-NO             PIC S9(4)   COMP.
00510
00511  01  ACTQ-KEY                    PIC X(20).
00512
00513  01  WS-LAST-TRLR-KEY            PIC X(22)  VALUE SPACES.
00514
00515  01  CHKQ-KEY.
00516      12  CHKQ-COMPANY-CODE       PIC X.
00517      12  CHKQ-CONTROL-NO         PIC S9(8)  COMP.
00518      12  CHKQ-SEQ-NO             PIC S9(4)  COMP.
00519
00520  01  ARCH-KEY.
00521      12  ARCH-COMPANY-CODE       PIC X.
00522      12  ARCH-ARCHIVE-NO         PIC S9(8)  COMP.
00523      12  ARCH-RECORD-TYPE        PIC X.
00524      12  ARCH-SEQ-NO             PIC S9(4)  COMP.
00525
00526  01  ACCT-KEY.
00527      05  ACCT-PARTIAL-KEY.
00528          12  ACCT-COMPANY-CODE   PIC X.
00529          12  ACCT-CARRIER        PIC X.
00530          12  ACCT-GROUP          PIC X(6).
00531          12  ACCT-STATE          PIC XX.
00532          12  ACCT-ACCOUNT        PIC X(10).
00533      05  ACCT-DATE               PIC XX.
00534
00535  01  WS-ELBENE-KEY.
00536      12  WS-ELBENE-COMPANY-CD    PIC X.
00537      12  WS-ELBENE-RECORD-TYPE   PIC X.
00538      12  WS-ELBENE-ID            PIC X(10).
00539
00540  01  ELALPH-KEY.
00541      12  ELALPH-COMPANY-CD       PIC X.
00542      12  ELALPH-SOURCE           PIC X.
00543      12  ELALPH-NAME.
00544          16  ELALPH-LAST-NAME    PIC X(15).
00545          16  ELALPH-FIRST-NAME.
00546              20  ELALPH-FIRST-INIT   PIC X.
00547              20  FILLER              PIC X(11).
00548          16  ELALPH-MIDDLE-INIT  PIC X.
00549      12  ELALPH-DATE             PIC X(08).
00550      12  ELALPH-TIME             PIC S9(04)    COMP.
00551
00552  01  COMP-LENGTHS.
00553      12  CNTL-GENERIC-LENGTH     PIC S9(4)   COMP VALUE +8.
00554      12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE +0.
00555      12  DATE-LENGTH             PIC S9(4)   COMP VALUE +8.
00556      12  MO-YR-LENGTH            PIC S9(4)   COMP VALUE +5.
00557      12  TERM-LENGTH             PIC S9(4)   COMP VALUE +3.
00558      12  BEN-LENGTH              PIC S9(4)   COMP VALUE +11.
00559      12  FREQ-LENGTH             PIC S9(4)   COMP VALUE +2.
00560      12  MSTR-LENGTH             PIC S9(4)   COMP VALUE +350.
00561      12  CERT-LENGTH             PIC S9(4)   COMP VALUE +450.
00562      12  TRLR-LENGTH             PIC S9(4)   COMP VALUE +200.
00563      12  ACTQ-LENGTH             PIC S9(4)   COMP VALUE +60.
00564      12  CHKQ-LENGTH             PIC S9(4)   COMP VALUE +100.
00565      12  ARCH-LENGTH             PIC S9(4)   COMP VALUE +90.
00566      12  ALPH-LENGTH             PIC S9(4)   COMP VALUE +128.
00567      EJECT
00568 *                                COPY ELCLNKLT.
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
00569      EJECT
00570 *                                COPY ELCINTF.
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
00571      12  EL131-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00572          16  PI-NO-PMTS                    PIC 9(4).
00573          16  PI-CERT-SWITCH                PIC X.
00574              88  CREATED-CERT              VALUE '2'.
00575          16  PI-PREM-TYPE                  PIC X.
00576          16  PI-SAVE-CERT-NO.
00577              20  PI-SAVE-CERT-NO-PRIME     PIC X(10).
00578              20  PI-SAVE-CERT-NO-SUFX      PIC X.
00579          16  FILLER                        PIC X(46).
00580          16  PI-CLAIM-DELETED-SWITCH       PIC X(1).
00581          16  PI-PAYMENT-AMT                PIC S9(7)V99   COMP-3.
00582          16  PI-REC-FOUND-SW               PIC X.
00583          16  PI-LETTER-SW                  PIC X.
00584          16  PI-PREV-TRLR-KEY              PIC X(22).
052113         16  pi-claim-type                 pic x.
060413         16  PI-AUTO-PAY-SEQ               PIC S9(4)      COMP.
080613         16  pi-approval-level             pic x.
080613         16  FILLER                        PIC X(543).
00586
00587  01  SAVE-RECORD                 PIC X(450).
00588
00589      EJECT
00590 *                                COPY ELCLOGOF SUPPRESS.
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
00591
00592 *                                COPY ELCATTR SUPPRESS.
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
00593
00594 *                                COPY ELCAID SUPPRESS.
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
00595  01  FILLER REDEFINES DFHAID.
00596      12  FILLER                  PIC X(8).
00597      12  AID-KEYS OCCURS 24 TIMES.
00598          16  FILLER              PIC X.
00599
00600 *                                COPY EL131S.
       01  EL131AI.
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
           05  SEQUL PIC S9(0004) COMP.
           05  SEQUF PIC  X(0001).
           05  FILLER REDEFINES SEQUF.
               10  SEQUA PIC  X(0001).
           05  SEQUI PIC  X(0010).
      *    -------------------------------
           05  COMPL PIC S9(0004) COMP.
           05  COMPF PIC  X(0001).
           05  FILLER REDEFINES COMPF.
               10  COMPA PIC  X(0001).
           05  COMPI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  LABEL1L PIC S9(0004) COMP.
           05  LABEL1F PIC  X(0001).
           05  FILLER REDEFINES LABEL1F.
               10  LABEL1A PIC  X(0001).
           05  LABEL1I PIC  X(0011).
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
           05  CCNOL PIC S9(0004) COMP.
           05  CCNOF PIC  X(0001).
           05  FILLER REDEFINES CCNOF.
               10  CCNOA PIC  X(0001).
           05  CCNOI PIC  X(0016).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  CLAIML PIC S9(0004) COMP.
           05  CLAIMF PIC  X(0001).
           05  FILLER REDEFINES CLAIMF.
               10  CLAIMA PIC  X(0001).
           05  CLAIMI PIC  X(0007).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  CERTL PIC S9(0004) COMP.
           05  CERTF PIC  X(0001).
           05  FILLER REDEFINES CERTF.
               10  CERTA PIC  X(0001).
           05  CERTI PIC  X(0010).
      *    -------------------------------
           05  SUFXL PIC S9(0004) COMP.
           05  SUFXF PIC  X(0001).
           05  FILLER REDEFINES SUFXF.
               10  SUFXA PIC  X(0001).
           05  SUFXI PIC  X(0001).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  BENPERL PIC S9(0004) COMP.
           05  BENPERF PIC  X(0001).
           05  FILLER REDEFINES BENPERF.
               10  BENPERA PIC  X(0001).
           05  BENPERI PIC  99.
      *    -------------------------------
           05  INSTYPEL PIC S9(0004) COMP.
           05  INSTYPEF PIC  X(0001).
           05  FILLER REDEFINES INSTYPEF.
               10  INSTYPEA PIC  X(0001).
           05  INSTYPEI PIC  X(0001).
      *    -------------------------------
           05  STATUSL PIC S9(0004) COMP.
           05  STATUSF PIC  X(0001).
           05  FILLER REDEFINES STATUSF.
               10  STATUSA PIC  X(0001).
           05  STATUSI PIC  X(0006).
      *    -------------------------------
           05  INCL PIC S9(0004) COMP.
           05  INCF PIC  X(0001).
           05  FILLER REDEFINES INCF.
               10  INCA PIC  X(0001).
           05  INCI PIC  X(0008).
      *    -------------------------------
           05  REPL PIC S9(0004) COMP.
           05  REPF PIC  X(0001).
           05  FILLER REDEFINES REPF.
               10  REPA PIC  X(0001).
           05  REPI PIC  X(0008).
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
           05  ACCSWL PIC S9(0004) COMP.
           05  ACCSWF PIC  X(0001).
           05  FILLER REDEFINES ACCSWF.
               10  ACCSWA PIC  X(0001).
           05  ACCSWI PIC  X(0001).
      *    -------------------------------
           05  BENEL PIC S9(0004) COMP.
           05  BENEF PIC  X(0001).
           05  FILLER REDEFINES BENEF.
               10  BENEA PIC  X(0001).
           05  BENEI PIC  X(0010).
      *    -------------------------------
           05  BIRTHL PIC S9(0004) COMP.
           05  BIRTHF PIC  X(0001).
           05  FILLER REDEFINES BIRTHF.
               10  BIRTHA PIC  X(0001).
           05  BIRTHI PIC  X(0008).
      *    -------------------------------
           05  SOCIALL PIC S9(0004) COMP.
           05  SOCIALF PIC  X(0001).
           05  FILLER REDEFINES SOCIALF.
               10  SOCIALA PIC  X(0001).
           05  SOCIALI PIC  X(0011).
      *    -------------------------------
           05  SEXL PIC S9(0004) COMP.
           05  SEXF PIC  X(0001).
           05  FILLER REDEFINES SEXF.
               10  SEXA PIC  X(0001).
           05  SEXI PIC  X(0001).
      *    -------------------------------
           05  MLNAMEL PIC S9(0004) COMP.
           05  MLNAMEF PIC  X(0001).
           05  FILLER REDEFINES MLNAMEF.
               10  MLNAMEA PIC  X(0001).
           05  MLNAMEI PIC  X(0015).
      *    -------------------------------
           05  MFNAMEL PIC S9(0004) COMP.
           05  MFNAMEF PIC  X(0001).
           05  FILLER REDEFINES MFNAMEF.
               10  MFNAMEA PIC  X(0001).
           05  MFNAMEI PIC  X(0012).
      *    -------------------------------
           05  MMINITL PIC S9(0004) COMP.
           05  MMINITF PIC  X(0001).
           05  FILLER REDEFINES MMINITF.
               10  MMINITA PIC  X(0001).
           05  MMINITI PIC  X(0001).
      *    -------------------------------
           05  AUTHRCVL PIC S9(0004) COMP.
           05  AUTHRCVF PIC  X(0001).
           05  FILLER REDEFINES AUTHRCVF.
               10  AUTHRCVA PIC  X(0001).
           05  AUTHRCVI PIC  X(0001).
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
           05  PROCL PIC S9(0004) COMP.
           05  PROCF PIC  X(0001).
           05  FILLER REDEFINES PROCF.
               10  PROCA PIC  X(0001).
           05  PROCI PIC  X(0004).
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
           05  CRITPL PIC S9(0004) COMP.
           05  CRITPF PIC  X(0001).
           05  FILLER REDEFINES CRITPF.
               10  CRITPA PIC  X(0001).
           05  CRITPI PIC  X(0002).
      *    -------------------------------
           05  EXTENSL PIC S9(0004) COMP.
           05  EXTENSF PIC  X(0001).
           05  FILLER REDEFINES EXTENSF.
               10  EXTENSA PIC  X(0001).
           05  EXTENSI PIC  99.
      *    -------------------------------
           05  FILETOL PIC S9(0004) COMP.
           05  FILETOF PIC  X(0001).
           05  FILLER REDEFINES FILETOF.
               10  FILETOA PIC  X(0001).
           05  FILETOI PIC  X(0004).
      *    -------------------------------
           05  PTHRHDGL PIC S9(0004) COMP.
           05  PTHRHDGF PIC  X(0001).
           05  FILLER REDEFINES PTHRHDGF.
               10  PTHRHDGA PIC  X(0001).
           05  PTHRHDGI PIC  X(0010).
      *    -------------------------------
           05  PDTHRUL PIC S9(0004) COMP.
           05  PDTHRUF PIC  X(0001).
           05  FILLER REDEFINES PDTHRUF.
               10  PDTHRUA PIC  X(0001).
           05  PDTHRUI PIC  X(0008).
      *    -------------------------------
           05  PDAMTL PIC S9(0004) COMP.
           05  PDAMTF PIC  X(0001).
           05  FILLER REDEFINES PDAMTF.
               10  PDAMTA PIC  X(0001).
           05  PDAMTI PIC  9(7)V99.
      *    -------------------------------
           05  NODAYSL PIC S9(0004) COMP.
           05  NODAYSF PIC  X(0001).
           05  FILLER REDEFINES NODAYSF.
               10  NODAYSA PIC  X(0001).
           05  NODAYSI PIC  9(5).
      *    -------------------------------
           05  NOPMTSL PIC S9(0004) COMP.
           05  NOPMTSF PIC  X(0001).
           05  FILLER REDEFINES NOPMTSF.
               10  NOPMTSA PIC  X(0001).
           05  NOPMTSI PIC  9(4).
      *    -------------------------------
           05  FORMTYPL PIC S9(0004) COMP.
           05  FORMTYPF PIC  X(0001).
           05  FILLER REDEFINES FORMTYPF.
               10  FORMTYPA PIC  X(0001).
           05  FORMTYPI PIC  X(0001).
      *    -------------------------------
           05  ESTL PIC S9(0004) COMP.
           05  ESTF PIC  X(0001).
           05  FILLER REDEFINES ESTF.
               10  ESTA PIC  X(0001).
           05  ESTI PIC  X(0008).
      *    -------------------------------
           05  AUIDL PIC S9(0004) COMP.
           05  AUIDF PIC  X(0001).
           05  FILLER REDEFINES AUIDF.
               10  AUIDA PIC  X(0001).
           05  AUIDI PIC  X(0004).
      *    -------------------------------
           05  OCCL PIC S9(0004) COMP.
           05  OCCF PIC  X(0001).
           05  FILLER REDEFINES OCCF.
               10  OCCA PIC  X(0001).
           05  OCCI PIC  X(0006).
      *    -------------------------------
           05  MNTDTL PIC S9(0004) COMP.
           05  MNTDTF PIC  X(0001).
           05  FILLER REDEFINES MNTDTF.
               10  MNTDTA PIC  X(0001).
           05  MNTDTI PIC  X(0008).
      *    -------------------------------
           05  MNTTYPEL PIC S9(0004) COMP.
           05  MNTTYPEF PIC  X(0001).
           05  FILLER REDEFINES MNTTYPEF.
               10  MNTTYPEA PIC  X(0001).
           05  MNTTYPEI PIC  X(0006).
      *    -------------------------------
           05  CERTCARL PIC S9(0004) COMP.
           05  CERTCARF PIC  X(0001).
           05  FILLER REDEFINES CERTCARF.
               10  CERTCARA PIC  X(0001).
           05  CERTCARI PIC  X(0001).
      *    -------------------------------
           05  CERTGRPL PIC S9(0004) COMP.
           05  CERTGRPF PIC  X(0001).
           05  FILLER REDEFINES CERTGRPF.
               10  CERTGRPA PIC  X(0001).
           05  CERTGRPI PIC  X(0006).
      *    -------------------------------
           05  CERTSTL PIC S9(0004) COMP.
           05  CERTSTF PIC  X(0001).
           05  FILLER REDEFINES CERTSTF.
               10  CERTSTA PIC  X(0001).
           05  CERTSTI PIC  X(0002).
      *    -------------------------------
           05  CERTACTL PIC S9(0004) COMP.
           05  CERTACTF PIC  X(0001).
           05  FILLER REDEFINES CERTACTF.
               10  CERTACTA PIC  X(0001).
           05  CERTACTI PIC  X(0010).
      *    -------------------------------
           05  CERTEFFL PIC S9(0004) COMP.
           05  CERTEFFF PIC  X(0001).
           05  FILLER REDEFINES CERTEFFF.
               10  CERTEFFA PIC  X(0001).
           05  CERTEFFI PIC  X(0008).
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
           05  REINCDL PIC S9(0004) COMP.
           05  REINCDF PIC  X(0001).
           05  FILLER REDEFINES REINCDF.
               10  REINCDA PIC  X(0001).
           05  REINCDI PIC  X(0001).
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
           05  JNTFNMEI PIC  X(0012).
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
           05  ADDONDTL PIC S9(0004) COMP.
           05  ADDONDTF PIC  X(0001).
           05  FILLER REDEFINES ADDONDTF.
               10  ADDONDTA PIC  X(0001).
           05  ADDONDTI PIC  X(0008).
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
           05  LABEL2L PIC S9(0004) COMP.
           05  LABEL2F PIC  X(0001).
           05  FILLER REDEFINES LABEL2F.
               10  LABEL2A PIC  X(0001).
           05  LABEL2I PIC  X(0014).
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
           05  MSG1L PIC S9(0004) COMP.
           05  MSG1F PIC  X(0001).
           05  FILLER REDEFINES MSG1F.
               10  MSG1A PIC  X(0001).
           05  MSG1I PIC  X(0072).
      *    -------------------------------
           05  MSG2L PIC S9(0004) COMP.
           05  MSG2F PIC  X(0001).
           05  FILLER REDEFINES MSG2F.
               10  MSG2A PIC  X(0001).
           05  MSG2I PIC  X(0072).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  99.
      *    -------------------------------
           05  PFKEY6L PIC S9(0004) COMP.
           05  PFKEY6F PIC  X(0001).
           05  FILLER REDEFINES PFKEY6F.
               10  PFKEY6A PIC  X(0001).
           05  PFKEY6I PIC  X(0016).
       01  EL131AO REDEFINES EL131AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQUO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LABEL1O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PSUFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCNOO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENPERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INSTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUSO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPO PIC  X(0008).
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
           05  ACCSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIRTHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SOCIALO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MFNAMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MMINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUTHRCVO PIC  X(0001).
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
           05  PROCO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUPVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRICDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRITPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTENSO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILETOO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PTHRHDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDAMTO PIC  Z(06).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NODAYSO PIC  ZZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOPMTSO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OCCO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTTYPEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTCARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTACTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTEFFO PIC  X(0008).
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
           05  REINCDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTLNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTFNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JNTAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDONDTO PIC  X(0008).
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
           05  LCVRTRMO PIC  999.
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
           05  ACVRTRMO PIC  999.
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
           05  LABEL2O PIC  X(0014).
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
           05  MSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSG2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEY6O PIC  X(0016).
      *    -------------------------------
00601
00602 *                                COPY ELCEMIB SUPPRESS.
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
00603
00604 *                                COPY ELCJPFX SUPPRESS.
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
00605                                  PIC X(750).
00606
00607 *                                COPY ELCCALC.
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
00608
00609 *                                COPY ELCDATE.
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
00610
00611 *                                COPY ELCDMO.
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
052113*                                COPY ERCPDEF.
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
052113*                                COPY ELCCRTT.
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
00612
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
00614  01  DFHCOMMAREA                 PIC X(1024).
00615
00616 *                                COPY ELCMSTR.
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
00617
00618 *                                COPY ELCCERT.
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
00619
00620 *                                COPY ELCTRLR.
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
00621
00622 *                                COPY ELCCNTL.
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
00623
00624 *                                COPY ELCACTQ.
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
00625
00626 *                                COPY ERCACCT.
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
00627
00628 *                                COPY ELCCHKQ.
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
00629
00630 *                                COPY ELCARCH.
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
00631
00632 *                                COPY ELCBENE.
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
00633
CIDMOD*                                COPY ELCDAR.
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
00633
00634 *                                COPY ELCALPH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCALPH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ALPHA CROSS REFERENCE FILE                *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 128  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELALPH                         RKP=2,LEN=42   *
00013 *       ALTERNATE PATH1 = ELALPH2 (FULL CONTROL)  RKP=44,LEN=57  *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCALPH                          *
00017 ******************************************************************
00018
00019  01  ALPHA-INDEX.
00020      12  AI-RECORD-ID                         PIC XX.
00021          88  VALID-AI-ID                  VALUE 'AI'.
00022      12  AI-CONTROL-PRIMARY.
00023          16  AI-COMPANY-CD                     PIC X.
00024          16  AI-SOURCE                         PIC X.
00025              88  AI-CLAIM-SYSTEM          VALUE 'C'.
00026              88  AI-ADMIN-SYSTEM          VALUE 'A'.
00027          16  AI-NAME.
00028              20  AI-LAST-NAME                  PIC X(15).
00029              20  AI-FIRST-NAME.
00030                  24  AI-FIRST-INITIAL          PIC X.
00031                  24  FILLER                    PIC X(11).
00032              20  AI-MIDDLE-INIT                PIC X.
00033          16  AI-DATE                           PIC X(8).
00034          16  AI-TIME                           PIC S9(07) COMP-3.
00035
00036      12  AI-CONTROL-BY-ADMIN-KEY.
00037          16  AI-CM-COMPANY-CD                  PIC X.
00038          16  AI-CM-SOURCE                      PIC X.
00039          16  AI-CM-CARRIER                     PIC X.
00040          16  AI-CM-GROUPING.
00041              20  AI-CM-GROUPING-PREFIX         PIC X(3).
00042              20  AI-CM-GROUPING-PRIME          PIC X(3).
00043          16  AI-CM-STATE                       PIC XX.
00044          16  AI-CM-PRODUCER.
00045              20  AI-CM-PRODUCER-PREFIX         PIC X(4).
00046              20  AI-CM-PRODUCER-PRIME          PIC X(6).
00047          16  AI-CM-CERT-EFF-DT                 PIC XX.
00048          16  AI-CM-CERTIFICATE-NUMBER.
00049              20  AI-CM-CERT-PRIME              PIC X(10).
00050              20  AI-CM-CERT-SFX                PIC X.
00051          16  AI-CM-DATE                        PIC X(8).
00052          16  AI-CM-TIME                        PIC S9(7) COMP-3.
00053          16  FILLER                            PIC X(11).
00054
00055      12  AI-CONTROL-BY-CLAIM-KEY REDEFINES
00056          AI-CONTROL-BY-ADMIN-KEY.
00057          16  AI-CL-COMPANY-CD                  PIC X.
00058          16  AI-CL-SOURCE                      PIC X.
00059          16  AI-CL-CARRIER                     PIC X.
00060          16  AI-CL-CLAIM-NUMBER                PIC X(7).
00061          16  AI-CL-CERTIFICATE-NUMBER.
00062              20  AI-CL-CERT-PRIME              PIC X(10).
00063              20  AI-CL-CERT-SFX                PIC X.
00064          16  AI-CL-DATE                        PIC X(8).
00065          16  AI-CL-INCURRED-DATE               PIC XX.
00066          16  AI-CL-CLOSE-DATE                  PIC XX.
00067          16  AI-CL-TIME                        PIC S9(7)  COMP-3.
00068          16  AI-CREDIT-CARD-NUMBER.
00069              20  AI-CCN.
00070                  24  AI-CCN-PREFIX             PIC X(4).
00071                  24  AI-CCN-PRIME              PIC X(12).
00072              20  AI-CCN-FILLER                 PIC X(4).
00073
00074      12  AI-MAINT-INFO.
00075          16  AI-LAST-MAINT-BY                  PIC X(4).
00076          16  AI-LAST-MAINT-DT                  PIC XX.
00077          16  AI-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00078
00079      12  AI-CLAIM-PAID-THRU-DT                 PIC XX.
00080
00081      12  FILLER                                PIC X(15).
00082
00635
00636 *                                COPY ERCDMDNT.
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
00637
00638      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                CERTIFICATE-MASTER ACTIVITY-TRAILERS
                                CONTROL-FILE ACTIVITY-QUE
                                ACCOUNT-MASTER CHECK-QUE
                                LETTER-ARCHIVE BENEFICIARY-MASTER
                                DAILY-ACTIVITY-RECORD ALPHA-INDEX
                                CERTIFICATE-NOTE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL131' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00640
00641      IF EIBCALEN = ZERO
00642          GO TO 8800-UNAUTHORIZED-ACCESS.
00643
00644      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00645      MOVE '5'                    TO DC-OPTION-CODE.
00646      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
00647      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00648      MOVE DC-GREG-DATE-1-YMD     TO  SAVE-DATE-YMD.
00649      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00650
00651      IF SAVE-DATE-YY GREATER 70
00652          MOVE 19                 TO SAVE-DATE-CC
00653       ELSE
00654          MOVE 20                 TO SAVE-DATE-CC.
00655
00656      
      * EXEC CICS HANDLE CONDITION
00657 *        PGMIDERR (8820-XCTL-ERROR)
00658 *        NOTFND   (8150-ENTERED-CLAIM-NOTFOUND)
00659 *        ERROR    (9990-ABEND)
00660 *    END-EXEC.
      *    MOVE '"$LI.                 ! " #00007594' TO DFHEIV0
           MOVE X'22244C492E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303037353934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00661
00662      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00663      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
00664      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
00665
00666      INITIALIZE    EDIT-WORK-AREA.
00667      MOVE '/'                    TO WORK-SLASH.
00668      MOVE SPACES                 TO ERROR-SWITCHES.
00669
00670      MOVE TRAN-ID                TO TRANS-ID.
00671      MOVE EIBTRMID               TO PI-TERM.
00672      MOVE MAP-ID                 TO PI-QUAL.
00673
00674      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00675
00676      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00677          MOVE LOW-VALUES         TO EL131AO
00678          MOVE ER-0008            TO EMI-ERROR
00679          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00680          MOVE -1                 TO MAINTL
00681          GO TO 8110-SEND-DATA.
00682
00683      IF THIS-PGM NOT = PI-CALLING-PROGRAM
00684          MOVE LOW-VALUES         TO EL131AO
082013         MOVE PI-CALLING-PROGRAM TO WS-RETURNED-FROM
00685          PERFORM 0100-UPDATE-PI THRU 0120-UPDATE-PI-EXIT
00686          MOVE 'X'                TO PI-RETURN-CD-1
00687          PERFORM 5000-BUILD-MAP THRU 5000-EXIT
00688          MOVE -1                 TO MAINTL
082013         IF WS-RETURNED-FROM = XCTL-EL114
082013             PERFORM 4200-LOAD-CHANGES THRU 4200-EXIT
082013         END-IF
00689          GO TO 8100-SEND-MAP.
00690
00691      IF EIBAID = DFHCLEAR
00692          GO TO 8200-RETURN-PRIOR.
00693
00694      IF PI-PROCESSOR-ID = 'LGXX'
00695          NEXT SENTENCE
00696      ELSE
00697          
      * EXEC CICS READQ TS
00698 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00699 *            INTO    (SECURITY-CONTROL)
00700 *            LENGTH  (SC-COMM-LENGTH)
00701 *            ITEM    (SC-ITEM)
00702 *        END-EXEC
      *    MOVE '*$II   L              ''   #00007639' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00703          MOVE SC-CLAIMS-DISPLAY (5)    TO  PI-DISPLAY-CAP
00704          MOVE SC-CLAIMS-UPDATE  (5)    TO  PI-MODIFY-CAP
00705          IF NOT DISPLAY-CAP
00706              MOVE 'READ'               TO  SM-READ
00707              PERFORM 9995-SECURITY-VIOLATION
00708              MOVE ER-0070              TO  EMI-ERROR
00709              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00710              GO TO 8100-SEND-MAP.
00711
00712      
      * EXEC CICS RECEIVE
00713 *        MAP    ('EL131A')
00714 *        MAPSET ('EL131S')
00715 *    END-EXEC.
           MOVE 'EL131A' TO DFHEIV1
           MOVE 'EL131S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00007654' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL131AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00716
00717      IF PFKEYL GREATER THAN ZERO
00718          PERFORM 0200-TRANS-PF THRU 0210-EXIT.
00719
00720      IF SCREEN-ERROR
00721          MOVE ER-0004 TO EMI-ERROR
00722          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00723          MOVE -1 TO MAINTL
00724          GO TO 8110-SEND-DATA.
00725
00726      IF EIBAID = DFHPF3
00727          GO TO 8500-TRLR-MNT.
00728
00729      IF EIBAID = DFHPF4
00730          GO TO 8600-ADDR-MNT.
00731
00732      IF EIBAID = DFHPF5
00733          GO TO 8700-CERT-MNT.
00734
00735      IF PI-COMPANY-ID = 'DMD'
00736        IF EIBAID = DFHPF6
00737          GO TO 8750-DMD-CLM-FIX.
00738
00739      IF EIBAID = DFHPF7
00740          GO TO 4000-FORCE-ERRORS.
00741
00742      IF EIBAID = DFHPF12
00743          GO TO 8300-GET-HELP.
082013
082013     IF EIBAID = DFHPF15
082013         GO TO 8725-BENEFICIARY-MNT
082013     END-IF.
00744
00745      IF EIBAID = DFHPF23
00746          GO TO 8810-PF23-ENTERED.
00747
00748      IF EIBAID = DFHPF24
00749          GO TO 8400-RETURN-MASTER.
00750
00751      IF EIBAID NOT = DFHENTER
00752          MOVE ER-0029            TO EMI-ERROR
00753          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00754          MOVE -1                 TO MAINTL
00755          GO TO 8110-SEND-DATA.
00756
00757      IF PI-RETURN-CD-1 =  'X'
00758          MOVE ER-0311            TO EMI-ERROR
00759          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00760          MOVE -1                 TO MAINTL
00761          GO TO 8110-SEND-DATA.
00762
00763      IF MAINTI = 'A'
00764          GO TO 0500-ADD-ALPHA-ONLY.
00765
00766      IF MAINTI = 'N'
00767          GO TO 0600-CHANGE-NAME.
00768
00769      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
021418     PERFORM 1030-EDIT-NAME THRU 1030-EXIT.
00770
00771      IF SCREEN-ERROR
00772          GO TO 8110-SEND-DATA.
00773
00774      IF NOT UPDATES-PRESENT AND NOT DELETE-CLAIM
00775          MOVE ER-0276            TO EMI-ERROR
00776          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00777          MOVE -1                 TO MAINTL
00778          GO TO 8110-SEND-DATA.
00779
00780      PERFORM 2000-UPDATE-CLAIM THRU 2000-EXIT.
00781
00782      IF SCREEN-ERROR
00783          GO TO 8110-SEND-DATA.
00784
00785      MOVE 'W'                    TO EMI-ACTION-SWITCH.
00786
00787      PERFORM 5000-BUILD-MAP THRU 5000-EXIT.
00788
00789      IF EMI-FORCABLE-CTR GREATER THAN ZERO
00790          GO TO 8110-SEND-DATA.
00791
00792      IF EMI-FATAL-CTR GREATER THAN ZERO
00793          GO TO 8110-SEND-DATA.
00794
00795      MOVE SPACE                  TO EMI-ACTION-SWITCH.
00796
00797      IF WS-OPEN-CLOSE-SW = 'Y'
00798          IF PI-LETTER-SW = 'Y'
00799              MOVE PI-COMPANY-ID  TO  CNTL-CO-ID
00800              MOVE 'T'            TO  CNTL-REC-TYPE
00801              MOVE SPACES         TO  CNTL-PROC-ID
00802              MOVE +0             TO  CNTL-SEQ-NO
00803              
      * EXEC CICS READ
00804 *                DATASET   (CNTL-FILE-ID)
00805 *                RIDFLD    (CNTL-KEY)
00806 *                SET       (ADDRESS OF CONTROL-FILE)
00807 *            END-EXEC
      *    MOVE '&"S        E          (   #00007750' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00808              PERFORM 7850-AUTO-LETTER-WRITER THRU 7850-EXIT.
00809
00810      MOVE ER-0000                TO EMI-ERROR.
00811      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00812
00813      MOVE SPACE                  TO MAINTO.
00814      MOVE -1                     TO MAINTL.
00815      GO TO 8100-SEND-MAP.
00816
00817      EJECT
00818  0100-UPDATE-PI.
082013
082013     MOVE LOW-VALUES             TO WS-SAVE-BENEFICIARY.
082013     IF WS-RETURNED-FROM = XCTL-EL114
082013***sometimes there is crap in positions 7-10 when no code was sele
082013        IF PI-PROGRAM-WORK-AREA (1:5) > SPACES
082013          MOVE PI-PROGRAM-WORK-AREA (1:10) TO WS-SAVE-BENEFICIARY
082013        END-IF
082013     END-IF.
082013
00819      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00820          GO TO 0110-UPDATE-UP.
00821
00822      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.
00823      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.
00824      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.
00825      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.
00826      MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.
00827      MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.
00828      MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.
00829      MOVE THIS-PGM               TO PI-CALLING-PROGRAM.
00830      GO TO 0120-UPDATE-PI-EXIT.
00831
00832  0110-UPDATE-UP.
00833      MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM.
00834      MOVE PI-SAVED-PROGRAM-1 TO PI-RETURN-TO-PROGRAM.
00835      MOVE PI-SAVED-PROGRAM-2 TO PI-SAVED-PROGRAM-1.
00836      MOVE PI-SAVED-PROGRAM-3 TO PI-SAVED-PROGRAM-2.
00837      MOVE PI-SAVED-PROGRAM-4 TO PI-SAVED-PROGRAM-3.
00838      MOVE PI-SAVED-PROGRAM-5 TO PI-SAVED-PROGRAM-4.
00839      MOVE PI-SAVED-PROGRAM-6 TO PI-SAVED-PROGRAM-5.
00840      MOVE SPACES             TO PI-SAVED-PROGRAM-6.
00841
00842      
      * EXEC CICS HANDLE CONDITION
00843 *         QIDERR    (0130-TS-ERROR)
00844 *         ITEMERR   (0130-TS-ERROR)
00845 *    END-EXEC.
      *    MOVE '"$N<                  ! # #00007798' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303037373938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00846
082013     IF WS-RETURNED-FROM = XCTL-EL114
082013        
      * EXEC CICS READQ TS
082013*            QUEUE    (PI-KEY)
082013*            INTO     (EL131AI)
082013*            LENGTH   (MAP-LENGTH)
082013*       END-EXEC
      *    MOVE '*$I    L              ''   #00007804' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 EL131AI, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
082013
082013        IF WS-SAVE-BENEFICIARY > SPACES
082013            MOVE WS-SAVE-BENEFICIARY TO BENEI
082013            MOVE +10            TO BENEL
082013            MOVE LOW-VALUES TO WS-SAVE-BENEFICIARY
082013        END-IF
082013        PERFORM 4100-SAVE-CHANGES THRU 4100-EXIT
082013     END-IF.
082013
00847      
      * EXEC CICS READQ TS
00848 *         QUEUE    (PI-KEY)
00849 *         INTO     (PROGRAM-INTERFACE-BLOCK)
00850 *         LENGTH   (PI-COMM-LENGTH)
00851 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00007818' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00852
00853      
      * EXEC CICS DELETEQ TS
00854 *         QUEUE    (PI-KEY)
00855 *    END-EXEC.
      *    MOVE '*&                    #   #00007824' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00856
00857  0120-UPDATE-PI-EXIT.
00858      EXIT.
00859
00860  0130-TS-ERROR.
00861      MOVE LOW-VALUES             TO EL131AO.
00862      MOVE ER-0192                TO EMI-ERROR.
00863
00864      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00865
00866      GO TO 8100-SEND-MAP.
00867
00868  0200-TRANS-PF.
00869      IF EIBAID NOT = DFHENTER
00870          MOVE 'X'                TO ERROR-SWITCH
00871          GO TO 0210-EXIT.
00872
00873      IF PFKEYI NOT NUMERIC
00874          MOVE 'X'                TO ERROR-SWITCH
00875          GO TO 0210-EXIT.
00876
00877      MOVE PFKEYI                 TO CHECK-PFKEYS.
00878
00879      IF CHECK-PFKEYS LESS 1 OR GREATER 24
00880          MOVE 'X' TO ERROR-SWITCH
00881          GO TO 0210-EXIT.
00882
00883      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
00884
00885  0210-EXIT.
00886      EXIT.
00887
00888      EJECT
00889  0500-ADD-ALPHA-ONLY.
00890 ******************************************************************
00891 *    ADD NAME TO ALPHA FILE.  NO CHANGES ARE MADE TO THE CLAIM   *
00892 *    RECORD.                                                     *
00893 ******************************************************************
00894
00895      IF MLNAMEL = +0 AND
00896         MFNAMEL = +0 AND
00897         MMINITL = +0
00898          MOVE ER-0797                TO  EMI-ERROR
00899          MOVE -1                     TO  MAINTL
00900          MOVE AL-UABON               TO  MAINTA
00901          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00902          GO TO 8110-SEND-DATA.
00903
00904      MOVE PI-COMPANY-CD              TO  COMPANY-CODE.
00905      MOVE PI-CARRIER                 TO  CARRIER-CODE.
00906      MOVE PI-CLAIM-NO                TO  CLAIM-NO.
00907      MOVE PI-CERT-NO                 TO  CERT-NO.
00908
00909      
      * EXEC CICS READ
00910 *        DATASET   (CLMS-FILE-ID)
00911 *        RIDFLD    (MSTR-KEY)
00912 *        SET       (ADDRESS OF CLAIM-MASTER)
00913 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007880' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00914
00915      
      * EXEC CICS GETMAIN
00916 *        SET       (ADDRESS OF ALPHA-INDEX)
00917 *        LENGTH    (ALPH-LENGTH)
00918 *        INITIMG   (GETMAIN-SPACE)
00919 *    END-EXEC.
      *    MOVE ',"IL                  $   #00007886' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ALPH-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00920
00921      MOVE 'AI'                       TO  AI-RECORD-ID.
00922      MOVE CL-COMPANY-CD              TO  AI-COMPANY-CD
00923                                          AI-CL-COMPANY-CD.
00924      MOVE 'C'                        TO  AI-SOURCE
00925                                          AI-CL-SOURCE.
00926
00927      IF MLNAMEL IS GREATER THAN +0
00928          MOVE MLNAMEI                TO  AI-LAST-NAME
00929      ELSE
00930          MOVE CL-INSURED-LAST-NAME   TO  AI-LAST-NAME.
00931
00932      IF MFNAMEL IS GREATER THAN +0
00933          MOVE MFNAMEI                TO  AI-FIRST-NAME
00934      ELSE
00935          MOVE CL-INSURED-1ST-NAME    TO  AI-FIRST-NAME.
00936
00937      IF MMINITL IS GREATER THAN +0
00938          MOVE MMINITI                TO  AI-MIDDLE-INIT
00939      ELSE
00940          MOVE CL-INSURED-MID-INIT    TO  AI-MIDDLE-INIT.
00941
00942      MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.
00943      MOVE '5'                        TO  DC-OPTION-CODE.
00944      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
00945      MOVE DC-GREG-DATE-1-MDY         TO  BUILD-VALID-DATE.
00946      IF BUILD-YEAR IS GREATER THAN 50
00947          MOVE '19'                   TO  WS-ALPHA-YY-1
00948          MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2
00949          MOVE BUILD-MONTH            TO  WS-ALPHA-MM
00950          MOVE BUILD-DAY              TO  WS-ALPHA-DD
00951      ELSE
00952          MOVE '20'                   TO  WS-ALPHA-YY-1
00953          MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2
00954          MOVE BUILD-MONTH            TO  WS-ALPHA-MM
00955          MOVE BUILD-DAY              TO  WS-ALPHA-DD.
00956
00957      MOVE WS-ALPHA-DATE              TO  AI-DATE
00958                                          AI-CL-DATE.
00959
00960      MOVE EIBTIME                    TO  AI-TIME
00961                                          AI-CL-TIME.
00962
00963      MOVE CL-COMPANY-CD              TO  AI-CL-COMPANY-CD.
00964      MOVE 'C'                        TO  AI-CL-SOURCE.
00965      MOVE CL-CARRIER                 TO  AI-CL-CARRIER.
00966      MOVE CL-CLAIM-NO                TO  AI-CL-CLAIM-NUMBER.
00967      MOVE CL-CERT-NO                 TO  AI-CL-CERTIFICATE-NUMBER.
00968      MOVE CL-INCURRED-DT             TO  AI-CL-INCURRED-DATE.
00969      MOVE LOW-VALUES                 TO  AI-CL-CLOSE-DATE.
00970
00971      MOVE PI-PROCESSOR-ID            TO  AI-LAST-MAINT-BY.
00972      MOVE SAVE-BIN-DATE              TO  AI-LAST-MAINT-DT.
00973      MOVE EIBTIME                    TO  AI-LAST-MAINT-HHMMSS.
00974
00975      
      * EXEC CICS WRITE
00976 *        DATASET   (ALPH-FILE-ID)
00977 *        RIDFLD    (AI-CONTROL-PRIMARY)
00978 *        FROM      (ALPHA-INDEX)
00979 *    END-EXEC.
           MOVE LENGTH OF
            ALPHA-INDEX
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007946' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALPH-FILE-ID, 
                 ALPHA-INDEX, 
                 DFHEIV11, 
                 AI-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00980
00981      MOVE LOW-VALUES                 TO  EL131AO.
00982      PERFORM 5000-BUILD-MAP THRU 5000-EXIT.
00983      MOVE ER-0000                    TO  EMI-ERROR.
00984      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00985      MOVE -1                         TO  MAINTL.
00986      GO TO 8100-SEND-MAP.
00987
00988      EJECT
00989  0600-CHANGE-NAME.
00990 ******************************************************************
00991 *    MOVE NAME IN THE CLAIM RECORD TO THE ALPHA FILE AND REPLACE *
00992 *    IT WITH THE NAME THAT IS KEYED ON THE SCREEN.               *
00993 ******************************************************************
00994
00995      IF MLNAMEL = +0 AND
00996         MFNAMEL = +0 AND
00997         MMINITL = +0
00998          MOVE ER-0276                TO  EMI-ERROR
00999          MOVE -1                     TO  MAINTL
01000          MOVE AL-UABON               TO  MAINTA
01001          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01002          GO TO 8110-SEND-DATA.
01003
01004      MOVE PI-COMPANY-CD              TO  COMPANY-CODE.
01005      MOVE PI-CARRIER                 TO  CARRIER-CODE.
01006      MOVE PI-CLAIM-NO                TO  CLAIM-NO.
01007      MOVE PI-CERT-NO                 TO  CERT-NO.
01008
01009      
      * EXEC CICS READ
01010 *        DATASET   (CLMS-FILE-ID)
01011 *        RIDFLD    (MSTR-KEY)
01012 *        SET       (ADDRESS OF CLAIM-MASTER)
01013 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007980' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01014
01015      
      * EXEC CICS GETMAIN
01016 *        SET       (ADDRESS OF ALPHA-INDEX)
01017 *        LENGTH    (ALPH-LENGTH)
01018 *        INITIMG   (GETMAIN-SPACE)
01019 *    END-EXEC.
      *    MOVE ',"IL                  $   #00007986' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ALPH-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01020
021418     MOVE CL-COMPANY-CD          TO CERT-COMPANY-CODE
021418     MOVE CL-CERT-CARRIER        TO CERT-CARRIER
021418     MOVE CL-CERT-GROUPING       TO CERT-GROUP
021418     MOVE CL-CERT-STATE          TO CERT-STATE
021418     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
021418     MOVE CL-CERT-EFF-DT         TO CERT-DATE
021418     MOVE CL-CERT-NO             TO CERT-CERT
021418
021418     
      * EXEC CICS READ
021418*       DATASET   (CERT-FILE-ID)
021418*       RIDFLD    (CERT-KEY)
021418*       SET       (ADDRESS OF CERTIFICATE-MASTER)
021418*       RESP      (WS-RESPONSE)
021418*    END-EXEC
      *    MOVE '&"S        E          (  N#00008000' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038303030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
021418
021418     IF INSTYPEL > ZEROS
021418        MOVE INSTYPEI TO CL-INSURED-TYPE
021418     END-IF
021418
021418     IF MFNAMEL > +0
021418        IF CL-INSURED-TYPE = 'C'
021418          AND (CM-INSURED-FIRST-NAME = MFNAMEI(1:10))
021418           MOVE ER-1675             TO EMI-ERROR
021418           MOVE -1                  TO MFNAMEL
021418           MOVE AL-UABON            TO MFNAMEA
021418           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
021418           GO TO 8110-SEND-DATA
021418        ELSE
021418          IF CL-INSURED-TYPE = 'P'
021418            AND (CM-INSURED-FIRST-NAME <> MFNAMEI(1:10))
021418             MOVE ER-1676             TO EMI-ERROR
021418             MOVE -1                  TO MFNAMEL
021418             MOVE AL-UABON            TO MFNAMEA
021418             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
021418             GO TO 8110-SEND-DATA
021418          END-IF
021418        END-IF
021418     END-IF
01021      MOVE 'AI'                       TO  AI-RECORD-ID.
01022      MOVE CL-COMPANY-CD              TO  AI-COMPANY-CD
01023                                          AI-CL-COMPANY-CD.
01024      MOVE 'C'                        TO  AI-SOURCE
01025                                          AI-CL-SOURCE.
01026
01027      MOVE CL-INSURED-LAST-NAME       TO  AI-LAST-NAME.
01028      MOVE CL-INSURED-1ST-NAME        TO  AI-FIRST-NAME.
01029      MOVE CL-INSURED-MID-INIT        TO  AI-MIDDLE-INIT.
01030
01031      MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.
01032      MOVE '5'                        TO  DC-OPTION-CODE.
01033      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
01034      MOVE DC-GREG-DATE-1-MDY         TO  BUILD-VALID-DATE.
01035      IF BUILD-YEAR IS GREATER THAN 50
01036          MOVE '19'                   TO  WS-ALPHA-YY-1
01037          MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2
01038          MOVE BUILD-MONTH            TO  WS-ALPHA-MM
01039          MOVE BUILD-DAY              TO  WS-ALPHA-DD
01040      ELSE
01041          MOVE '20'                   TO  WS-ALPHA-YY-1
01042          MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2
01043          MOVE BUILD-MONTH            TO  WS-ALPHA-MM
01044          MOVE BUILD-DAY              TO  WS-ALPHA-DD.
01045
01046      MOVE WS-ALPHA-DATE              TO  AI-DATE
01047                                          AI-CL-DATE.
01048
01049      MOVE EIBTIME                    TO  AI-TIME
01050                                          AI-CL-TIME.
01051
01052      MOVE CL-COMPANY-CD              TO  AI-CL-COMPANY-CD.
01053      MOVE 'C'                        TO  AI-CL-SOURCE.
01054      MOVE CL-CARRIER                 TO  AI-CL-CARRIER.
01055      MOVE CL-CLAIM-NO                TO  AI-CL-CLAIM-NUMBER.
01056      MOVE CL-CERT-NO                 TO  AI-CL-CERTIFICATE-NUMBER.
01057      MOVE CL-INCURRED-DT             TO  AI-CL-INCURRED-DATE.
01058      MOVE LOW-VALUES                 TO  AI-CL-CLOSE-DATE.
01059
01060      MOVE PI-PROCESSOR-ID            TO  AI-LAST-MAINT-BY.
01061      MOVE SAVE-BIN-DATE              TO  AI-LAST-MAINT-DT.
01062      MOVE EIBTIME                    TO  AI-LAST-MAINT-HHMMSS.
01063
062121     IF PI-COMPANY-ID = 'CID' or 'AHL' or 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02826
01064      
      * EXEC CICS WRITE
01065 *        DATASET   (ALPH-FILE-ID)
01066 *        RIDFLD    (AI-CONTROL-PRIMARY)
01067 *        FROM      (ALPHA-INDEX)
01068 *    END-EXEC.
           MOVE LENGTH OF
            ALPHA-INDEX
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008077' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALPH-FILE-ID, 
                 ALPHA-INDEX, 
                 DFHEIV11, 
                 AI-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01069
01070      
      * EXEC CICS READ
01071 *        DATASET   (CLMS-FILE-ID)
01072 *        RIDFLD    (MSTR-KEY)
01073 *        SET       (ADDRESS OF CLAIM-MASTER)
01074 *        UPDATE
01075 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008083' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01076
062602     if (cl-priority-cd = '8')
062602        and (pi-processor-id not = 'PEMA'and 'JMS '
052113             AND 'AMWA' AND 'KMSB')
062602        MOVE ER-8003             TO EMI-ERROR
062602        PERFORM 9900-ERROR-FORMAT
062602                                 THRU 9900-EXIT
062602        MOVE -1                  TO MAINTL
062602        GO TO 8110-SEND-DATA
062602     end-if
062602
01077      IF MLNAMEL IS GREATER THAN +0
01078          MOVE MLNAMEI                TO  CL-INSURED-LAST-NAME.
01079
01080      IF MFNAMEL IS GREATER THAN +0
01081          MOVE MFNAMEI                TO  CL-INSURED-1ST-NAME.
01082
01083      IF MMINITL IS GREATER THAN +0
01084          MOVE MMINITI                TO  CL-INSURED-MID-INIT.
01085
01086      MOVE PI-PROCESSOR-ID            TO  CL-LAST-MAINT-USER.
01087      MOVE SAVE-BIN-DATE              TO  CL-LAST-MAINT-DT.
01088      MOVE EIBTIME                    TO  CL-LAST-MAINT-HHMMSS.
01089      MOVE '3'                        TO  CL-LAST-MAINT-TYPE.
01090
062121     IF PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02826
01091      
      * EXEC CICS HANDLE CONDITION
01092 *        DUPKEY   (0600-CONTINUE-NAME-UPDATE)
01093 *    END-EXEC.
      *    MOVE '"$$                   ! $ #00008118' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303038313138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01094
01095      
      * EXEC CICS REWRITE
01096 *        DATASET   (CLMS-FILE-ID)
01097 *        FROM      (CLAIM-MASTER)
01098 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008122' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01099
01100  0600-CONTINUE-NAME-UPDATE.
01101
01102      MOVE LOW-VALUES                 TO  EL131AO.
01103      PERFORM 5000-BUILD-MAP THRU 5000-EXIT.
01104      MOVE ER-0000                    TO  EMI-ERROR.
01105      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01106      MOVE -1                         TO  MAINTL.
01107      GO TO 8100-SEND-MAP.
01108
01109      EJECT
01110  1000-EDIT-SCREEN.
01111
01112      MOVE MAINTI                 TO CHECK-MAINT.
01113
01114      IF NOT VALID-OPTIONS
01115          MOVE 'X'                TO ERROR-SWITCH
01116          MOVE -1                 TO MAINTL
01117          MOVE AL-UABON           TO MAINTA
01118          MOVE ER-0023            TO EMI-ERROR
01119          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01120          GO TO 1010-EXIT.
01121
01122      IF NOT MODIFY-CAP
01123          MOVE 'UPDATE'           TO  SM-READ
01124          PERFORM 9995-SECURITY-VIOLATION
01125          MOVE ER-0070               TO  EMI-ERROR
01126          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01127          GO TO 8100-SEND-MAP.
01128
01129      IF DELETE-CLAIM
01130          MOVE AL-UANON TO MAINTA
01131          GO TO 1010-EXIT.
01132
01133      MOVE AL-UANON               TO MAINTA.
01134
01135      IF CCNOI GREATER THAN LOW-VALUES
01136          MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH.
01137
01138      IF PI-COMPANY-ID = 'CSL'
01139         CONTINUE
01140      ELSE
01141         IF TYPEL > ZERO
062121           IF ((PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL')
012009              AND (TYPEI = PI-AH-OVERRIDE-L1 OR
100518                    PI-LIFE-OVERRIDE-L1 OR 'O'))
012009                       OR
020816              ((PI-COMPANY-ID = 'DCC' OR 'VPP')
012009              AND (TYPEI = PI-AH-OVERRIDE-L1 OR
100518                PI-LIFE-OVERRIDE-L1 OR 'I' OR 'G' OR 'F' OR 'O'
080322                                    OR 'B' OR 'H' ))
01143               MOVE AL-UANON      TO TYPEA
01144               MOVE 'X'           TO UPDATE-SWITCH MSTR-SWITCH
01145            ELSE
01146               MOVE ER-0199       TO EMI-ERROR
01147               PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
01148               MOVE AL-UABON      TO TYPEA
01149               MOVE -1            TO TYPEL
01150               MOVE 'X'           TO ERROR-SWITCH
                 END-IF
01151         ELSE
01152            MOVE AL-UANOF         TO TYPEA
              END-IF
           END-IF
01154      IF STATUSI GREATER THAN LOW-VALUES
01155          IF STATUSI = 'OPEN' OR 'CLOSED' OR 'O' OR 'C'
01156              MOVE 'Y'            TO WS-OPEN-CLOSE-SW
01157              MOVE AL-UANON       TO STATUSA
01158              MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH TRLR-SWITCH
CIDMOD             MOVE 'Y'            TO DLYACTV-SW
01159          ELSE
01160              MOVE ER-0333        TO EMI-ERROR
01161              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01162              MOVE AL-UABON       TO STATUSA
01163              MOVE -1             TO STATUSL
01164              MOVE 'X'            TO ERROR-SWITCH
CIDMOD         END-IF
01165      ELSE
01166          MOVE SPACE              TO TRLR-SWITCH
CIDMOD         MOVE 'N'                TO DLYACTV-SW
01167          MOVE AL-UANOF           TO STATUSA.
01168
01169      IF PROCI GREATER THAN LOW-VALUES
01170          PERFORM 1120-EDIT-PROC THRU 1120-EXIT
01171      ELSE
01172          MOVE AL-UANOF TO PROCA.
01173
01174      IF SEXI GREATER THAN LOW-VALUES
01175          IF SEXI = 'F' OR 'M'
01176              MOVE AL-UANON       TO SEXA
01177              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01178            ELSE
01179              MOVE ER-0219        TO EMI-ERROR
01180              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01181              MOVE AL-UABON       TO SEXA
01182              MOVE -1             TO SEXL
01183              MOVE 'X'            TO ERROR-SWITCH
01184        ELSE
01185          MOVE AL-UANOF           TO SEXA.
01186
01187      IF BIRTHL = +0
01188          MOVE LOW-VALUES         TO  HOLD-BIRTH
01189          MOVE AL-UANOF           TO  BIRTHA
01190          GO TO 1000-CONTINUE-EDITS.
01191
01192      IF BIRTHI = SPACES
01193          MOVE LOW-VALUES         TO  HOLD-BIRTH
01194          MOVE 'X'                TO  UPDATE-SWITCH
01195                                      MSTR-SWITCH
01196          GO TO 1000-CONTINUE-EDITS.
01197
01198      MOVE BIRTHI                 TO  DEEDIT-DATE-INPUT.
01199      PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT.
01200      MOVE DEEDIT-DATE            TO  DC-GREG-DATE-1-MDY.
01201      MOVE '4'                    TO  DC-OPTION-CODE.
01202      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
01203      IF DATE-CONVERSION-ERROR
01204          MOVE ER-0220            TO  EMI-ERROR
01205          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01206          MOVE AL-UABON           TO  BIRTHA
01207          MOVE -1                 TO  BIRTHL
01208          MOVE 'X'                TO  ERROR-SWITCH
01209          GO TO 1000-CONTINUE-EDITS.
01210
01211 ******************************************************************
01212 **   IF CALCULATED BIRTH DATE IS GREATER THAN TODAYS DATE       **
01213 **   USE THE CENTURY ADJUSTMENT SWITCH IN THE DATE ROUTINE      **
01214 **   TO SUBTRACT 100 YEARS TO OBTAIN THE CORRECT BIRTH DATE.    **
01215 ******************************************************************
01216      IF DC-BIN-DATE-1 IS GREATER THAN SAVE-BIN-DATE
01217          MOVE BIRTHI             TO  DEEDIT-DATE-INPUT
01218          PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT
01219          MOVE DEEDIT-DATE        TO  DC-GREG-DATE-1-MDY
01220          MOVE '4'                TO  DC-OPTION-CODE
01221          MOVE '1'                TO  DC-CENTURY-ADJUSTMENT
01222          MOVE +0                 TO  DC-ELAPSED-MONTHS
01223                                      DC-ELAPSED-DAYS
01224          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01225          IF DATE-CONVERSION-ERROR
01226              MOVE ER-0220        TO  EMI-ERROR
01227              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01228              MOVE AL-UABON       TO  BIRTHA
01229              MOVE -1             TO  BIRTHL
01230              MOVE 'X'            TO  ERROR-SWITCH
01231              GO TO 1000-CONTINUE-EDITS.
01232
01233      MOVE AL-UANON               TO  BIRTHA.
01234      MOVE 'X'                    TO  UPDATE-SWITCH
01235                                      MSTR-SWITCH.
01236      MOVE DC-GREG-DATE-1-EDIT    TO  BIRTHO.
01237      MOVE DC-BIN-DATE-1          TO  HOLD-BIRTH.
01238      MOVE ' '                    TO  DC-CENTURY-ADJUSTMENT.
01239
01240  1000-CONTINUE-EDITS.
01241
071508     IF SOCIALI GREATER THAN LOW-VALUES
071508         MOVE SOCIALI TO WS-SOC-SEC-NUMBER
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
071508                 MOVE AL-UABON       TO  SOCIALA
071508                 MOVE -1             TO  SOCIALL
071508                 MOVE 'X'            TO  ERROR-SWITCH
071508            END-IF
071508        END-IF
071508     END-IF.
031715     if critpl > zeros
031715        and critpi numeric
031715        move al-uanon            to critpa
031715        move 'X'                 to update-switch
031715     end-if
01242      IF SOCIALI GREATER THAN LOW-VALUES
01243          MOVE AL-UANON           TO SOCIALA
01244          MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH MSTR-KEY-SWITCH
01245      ELSE
01246          MOVE AL-UANOF           TO SOCIALA.
01247
01248      IF OCCI GREATER THAN LOW-VALUES
01249          MOVE AL-UANON           TO OCCA
01250          MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
01251      ELSE
01252          MOVE AL-UANOF           TO OCCA.
01253
01254      IF BENEL NOT GREATER THAN ZERO
01255             GO TO 1005-CONTINUE-EDITS.
01256
01257      IF BENEI = SPACES
060413        IF PI-AUTO-PAY-SEQ NOT = ZEROS
060413           MOVE 'N'             TO AUTO-PAY-TO-BENE
060413           PERFORM 2800-CHECK-AUTO-PAY THRU 2800-EXIT
060413           IF AUTO-PAY-TO-BENE = 'Y'
060413               MOVE ER-1773      TO  EMI-ERROR
060413               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
060413               MOVE 'X'          TO ERROR-SWITCH
060413               MOVE AL-UABON     TO BENEA
060413               MOVE -1           TO BENEL
060413           ELSE
060413               MOVE 'X'          TO UPDATE-SWITCH MSTR-SWITCH
060413           END-IF
060413        ELSE
031815           PERFORM 1050-EDIT-BENE THRU 1050-EXIT
031815           IF PAYMENT-PENDING
031815               MOVE ER-1772      TO  EMI-ERROR
031815               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
031815               MOVE 'X'          TO ERROR-SWITCH
031815               MOVE AL-UABON     TO BENEA
031815               MOVE -1           TO BENEL
031815           END-IF
060413           MOVE 'X'              TO UPDATE-SWITCH MSTR-SWITCH
060413        END-IF
01259         GO TO 1005-CONTINUE-EDITS.
01260
01261      
      * EXEC CICS HANDLE CONDITION
01262 *        NOTFND (1004-BENE-NOT-FOUND)
01263 *    END-EXEC.
      *    MOVE '"$I                   ! % #00008347' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303038333437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01264
01265      MOVE PI-COMPANY-CD          TO  WS-ELBENE-COMPANY-CD.
01266      MOVE 'B'                    TO  WS-ELBENE-RECORD-TYPE.
01267      MOVE BENEI                  TO  WS-ELBENE-ID.
01268
01269      
      * EXEC CICS READ
01270 *        DATASET ('ELBENE')
01271 *        RIDFLD  (WS-ELBENE-KEY)
01272 *        SET     (ADDRESS OF BENEFICIARY-MASTER)
01273 *    END-EXEC.
           MOVE 'ELBENE' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008355' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01274
01275      MOVE AL-UANON               TO  BENEA.
01276      MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH.
01277
01278      GO TO 1005-CONTINUE-EDITS.
01279
01280  1004-BENE-NOT-FOUND.
01281      MOVE ER-0565                TO  EMI-ERROR.
01282
01283      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01284      MOVE 'X'                    TO ERROR-SWITCH.
01285      MOVE AL-UABON               TO BENEA.
01286      MOVE -1                     TO BENEL.
01287
01288  1005-CONTINUE-EDITS.
01289
01290      IF DIAGI GREATER THAN LOW-VALUES
01291          MOVE AL-UANON           TO DIAGA
01292          MOVE 'X'                TO UPDATE-SWITCH
01293                                     NINETY-TRLR-SWITCH
01294      ELSE
01295          MOVE AL-UANOF           TO DIAGA.
040814
040814     IF ICD1I GREATER THAN LOW-VALUES
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
040814              MOVE 'X'            TO ERROR-SWITCH
040814              MOVE AL-UABON       TO ICD1A
040814              MOVE -1             TO ICD1L
040814         ELSE
040814             MOVE AL-UANON       TO ICD1A
040814             MOVE 'X'            TO UPDATE-SWITCH
040814                                    NINETY-TRLR-SWITCH
040814         END-IF
040814     ELSE
040814         MOVE AL-UANOF           TO ICD1A
040814     END-IF.
040814
040814     IF ICD2I GREATER THAN LOW-VALUES
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
040814              MOVE ER-0992        TO EMI-ERROR
040814              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814              MOVE 'X'            TO ERROR-SWITCH
040814              MOVE AL-UABON       TO ICD2A
040814              MOVE -1             TO ICD2L
040814         ELSE
040814             MOVE AL-UANON       TO ICD2A
040814             MOVE 'X'            TO UPDATE-SWITCH
040814                                    NINETY-TRLR-SWITCH
040814         END-IF
040814     ELSE
040814         MOVE AL-UANOF           TO ICD2A
040814     END-IF.
01296
040814*    IF CAUSEI GREATER THAN LOW-VALUES
040814*        MOVE AL-UANON           TO CAUSEA
040814*        MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
040814*    ELSE
040814*        MOVE AL-UANOF           TO CAUSEA.
01302
040814*    IF ENDL GREATER THAN ZERO
040814*        MOVE LOW-VALUES         TO HOLD-END
040814*        IF ENDI = SPACES
040814*            MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
040814*        ELSE
040814*            MOVE ENDI           TO DEEDIT-DATE-INPUT
040814*            EXEC CICS BIF DEEDIT
040814*                FIELD    (DEEDIT-DATE-INPUT)
040814*                LENGTH   (DATE-LENGTH)
040814*            END-EXEC
040814*            MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
040814*            MOVE '4'            TO DC-OPTION-CODE
040814*            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
040814*            IF NOT DATE-CONVERSION-ERROR
040814*                MOVE DC-BIN-DATE-1      TO HOLD-END
040814*                MOVE AL-UANON   TO ENDA
040814*                MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
040814*                MOVE DC-GREG-DATE-1-EDIT TO ENDO
040814*            ELSE
040814*                MOVE ER-0224    TO EMI-ERROR
040814*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814*                MOVE AL-UABON   TO ENDA
040814*                MOVE -1         TO ENDL
040814*                MOVE 'X'        TO ERROR-SWITCH
040814*      ELSE
040814*          MOVE AL-UANOF         TO ENDA.
01329
01330      IF INCL GREATER THAN ZERO
01331          MOVE LOW-VALUES         TO HOLD-INCUR
01332          IF INCI = SPACES
01333              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01334          ELSE
01335              MOVE INCI           TO DEEDIT-DATE-INPUT
01336              
      * EXEC CICS BIF DEEDIT
01337 *                FIELD    (DEEDIT-DATE-INPUT)
01338 *                LENGTH   (DATE-LENGTH)
01339 *            END-EXEC
      *    MOVE '@"L                   #   #00008490' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01340              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
01341              MOVE '4'            TO DC-OPTION-CODE
01342              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01343              IF NOT DATE-CONVERSION-ERROR
01344                  MOVE DC-BIN-DATE-1      TO HOLD-INCUR
01345                  MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
01346                  MOVE AL-UANON           TO INCA
01347                  MOVE DC-GREG-DATE-1-EDIT TO INCO
01348              ELSE
01349                  MOVE ER-0222    TO EMI-ERROR
01350                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01351                  MOVE AL-UABON   TO INCA
01352                  MOVE -1         TO INCL
01353                  MOVE 'X'        TO ERROR-SWITCH
090821             end-if
090821         end-if
01354      ELSE
01355         IF PI-NO-PMTS = ZEROS
01356            MOVE AL-UANOF     TO INCA
090821        end-if
090821     end-if
01357
01358      IF REPL GREATER THAN ZERO
01359          MOVE LOW-VALUES         TO HOLD-REPORTED
01360          IF REPI = SPACES
01361              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01362          ELSE
01363              MOVE REPI           TO DEEDIT-DATE-INPUT
01364              
      * EXEC CICS BIF DEEDIT
01365 *                FIELD    (DEEDIT-DATE-INPUT)
01366 *                LENGTH   (DATE-LENGTH)
01367 *            END-EXEC
      *    MOVE '@"L                   #   #00008522' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01368              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
01369              MOVE '4'            TO DC-OPTION-CODE
01370              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01371              IF NOT DATE-CONVERSION-ERROR
01372                  MOVE DC-BIN-DATE-1      TO HOLD-REPORTED
01373                  MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
01374                  MOVE AL-UANON   TO REPA
01375                  MOVE DC-GREG-DATE-1-EDIT TO REPO
01376              ELSE
01377                  MOVE ER-0223    TO EMI-ERROR
01378                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01379                  MOVE AL-UABON   TO REPA
01380                  MOVE -1         TO REPL
01381                  MOVE 'X'        TO ERROR-SWITCH
01382        ELSE
01383          MOVE AL-UANOF TO REPA.
01384
01385      IF PDTHRUI GREATER THAN LOW-VALUES
01386         MOVE LOW-VALUES         TO HOLD-PDTHRU
01387         IF PDTHRUI = SPACES
01388            MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01389         ELSE
01390            MOVE PDTHRUI        TO DEEDIT-DATE-INPUT
01391            
      * EXEC CICS BIF DEEDIT
01392 *               FIELD    (DEEDIT-DATE-INPUT)
01393 *               LENGTH   (DATE-LENGTH)
01394 *          END-EXEC
      *    MOVE '@"L                   #   #00008549' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01395            MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
01396            MOVE '4'            TO DC-OPTION-CODE
01397            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01398            IF NOT DATE-CONVERSION-ERROR
01399               MOVE DC-BIN-DATE-1      TO HOLD-PDTHRU
01400               MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
01401               MOVE AL-UANON   TO PDTHRUA
01402               MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO
01403               IF PI-USES-PAID-TO
01404                  MOVE '6'            TO DC-OPTION-CODE
01405                  MOVE -1             TO DC-ELAPSED-DAYS
01406                  MOVE +0             TO DC-ELAPSED-MONTHS
01407                  PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01408                  IF NO-CONVERSION-ERROR
01409                     MOVE DC-BIN-DATE-2 TO HOLD-PDTHRU
01410                  ELSE
01411                     NEXT SENTENCE
01412               ELSE
01413                  NEXT SENTENCE
01414            ELSE
01415               MOVE ER-0475    TO EMI-ERROR
01416               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01417               MOVE AL-UABON   TO PDTHRUA
01418               MOVE -1         TO PDTHRUL
01419               MOVE 'X'        TO ERROR-SWITCH
01420      ELSE
01421         IF MODIFY-CAP
01422            MOVE AL-UANOF        TO PDTHRUA.
01423
01424      IF PDAMTL GREATER THAN +0
01425          MOVE ZEROS              TO HOLD-PDAMT
01426          
      * EXEC CICS BIF DEEDIT
01427 *            FIELD    (PDAMTI)
01428 *            LENGTH   (9)
01429 *        END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008584' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PDAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01430          IF PDAMTI NUMERIC
01431              MOVE PDAMTI         TO HOLD-PDAMT PDAMTO
01432              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01433              MOVE AL-UNNON       TO PDAMTA
01434          ELSE
01435              MOVE ER-0547        TO EMI-ERROR
01436              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01437              MOVE AL-UNBON       TO PDAMTA
01438              MOVE -1             TO PDAMTL
01439              MOVE 'X'            TO ERROR-SWITCH
01440      ELSE
01441          IF MODIFY-CAP
01442             MOVE AL-UNNOF        TO PDAMTA.
01443
01444      IF NODAYSI GREATER THAN LOW-VALUES
01445          MOVE ZEROS              TO HOLD-NODAYS
01446          
      * EXEC CICS BIF DEEDIT
01447 *            FIELD (NODAYSI)
01448 *            LENGTH (5)
01449 *        END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008604' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NODAYSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01450          IF NODAYSI NUMERIC
01451              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01452              MOVE AL-UNNON       TO NODAYSA
01453              MOVE NODAYSI        TO  HOLD-NODAYS  NODAYSO
01454          ELSE
01455              MOVE ER-0491        TO EMI-ERROR
01456              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01457              MOVE AL-UNBON       TO NODAYSA
01458              MOVE -1             TO NODAYSL
01459              MOVE 'X'            TO ERROR-SWITCH
01460      ELSE
01461          IF MODIFY-CAP
01462             MOVE AL-UNNOF        TO NODAYSA.
01463
01464      IF NOPMTSI GREATER THAN LOW-VALUES
01465          MOVE ZEROS              TO HOLD-NOPMTS
01466          
      * EXEC CICS BIF DEEDIT
01467 *            FIELD (NOPMTSI)
01468 *            LENGTH (4)
01469 *        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008624' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NOPMTSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01470          IF NOPMTSI NUMERIC
01471              MOVE NOPMTSI        TO HOLD-NOPMTS  NOPMTSO
01472              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01473              MOVE AL-UNNON       TO NOPMTSA
01474            ELSE
01475              MOVE ER-0548        TO EMI-ERROR
01476              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01477              MOVE AL-UNBON       TO NOPMTSA
01478              MOVE -1             TO NOPMTSL
01479              MOVE 'X'            TO ERROR-SWITCH
01480        ELSE
01481          IF MODIFY-CAP
01482             MOVE AL-UNNOF        TO NOPMTSA.
01483
01484      IF FORMTYPI GREATER THAN LOW-VALUES
01485          IF FORMTYPI = ' ' OR 'L' OR 'S'
01486              MOVE AL-UANON       TO FORMTYPA
01487              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01488            ELSE
01489              MOVE ER-7650        TO EMI-ERROR
01490              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01491              MOVE AL-UABON       TO FORMTYPA
01492              MOVE -1             TO FORMTYPL
01493              MOVE 'X'            TO ERROR-SWITCH
01494        ELSE
01495          MOVE AL-UANOF           TO FORMTYPA.
01496
01497      IF PRICDL GREATER THAN ZERO
01498          IF (PRICDI = ' ') OR
01499             (PRICDI GREATER THAN ZERO   AND
01500              PRICDI NOT GREATER THAN '9')
01501              MOVE AL-UANON       TO PRICDA
01502              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01503          ELSE
01504              MOVE ER-0274        TO EMI-ERROR
01505              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01506              MOVE AL-UABON       TO PRICDA
01507              MOVE -1             TO PRICDL
01508              MOVE 'X'            TO ERROR-SWITCH
01509      ELSE
01510          MOVE AL-UANOF TO PRICDA.
01511
01512      IF PI-COMPANY-ID = 'DMD'
01513      IF PRICDL GREATER THAN ZERO
01514        IF SYSTEM-MODIFY-CAP  OR
01515           PI-PROCESSOR-ID = 'LGXX'
01516              NEXT SENTENCE
01517            ELSE
01518             IF CL-PRIORITY-CD = '9'  OR
01519                PRICDI         = '9'
01520                   MOVE ER-0938        TO EMI-ERROR
01521                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01522                   MOVE AL-UABON       TO PRICDA
01523                   MOVE -1             TO PRICDL
01524                   MOVE 'X'            TO ERROR-SWITCH.
01525
01526      IF FILETOL GREATER THAN ZERO
01527          MOVE AL-UANON           TO FILETOA
01528          MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
01529      ELSE
01530          MOVE AL-UANOF TO FILETOA.
01531
01532      IF SUPVL GREATER THAN ZERO
01533          IF SUPVI = 'Y' OR 'N' OR SPACE
01534              MOVE AL-UANON       TO SUPVA
01535              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
01536          ELSE
01537              MOVE ER-0230        TO EMI-ERROR
01538              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01539              MOVE AL-UABON       TO SUPVA
01540              MOVE -1             TO SUPVL
01541              MOVE 'X'            TO ERROR-SWITCH
01542      ELSE
01543          MOVE AL-UANOF           TO SUPVA.
01544
01545      IF MLNAMEL GREATER THAN ZERO
01546          IF MLNAMEI GREATER THAN SPACES
01547             MOVE AL-UANON        TO MLNAMEA
01548             MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH
01549                                      MSTR-KEY-SWITCH
01550          ELSE
01551             MOVE ER-0236         TO EMI-ERROR
01552             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01553             MOVE AL-UABON        TO MLNAMEA
01554             MOVE -1              TO MLNAMEL
01555             MOVE 'X'             TO ERROR-SWITCH
01556      ELSE
01557          MOVE AL-UANOF           TO MLNAMEA.
01558
01559      IF MFNAMEI GREATER THAN LOW-VALUES
01560          MOVE AL-UANON           TO MFNAMEA
01561          MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
01562      ELSE
01563          MOVE AL-UANOF           TO MFNAMEA.
01564
01565      IF MMINITI GREATER THAN LOW-VALUES
01566          MOVE AL-UANON           TO MMINITA
01567          MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
01568      ELSE
01569          MOVE AL-UANOF           TO MMINITA.
01570
01571      IF CRTLNMEI GREATER THAN LOW-VALUES
01572          IF CRTLNMEI NOT = SPACES
01573             MOVE AL-UANON        TO CRTLNMEA
01574             MOVE 'X'             TO  UPDATE-SWITCH CERT-SWITCH
01575                                      CERT-KEY-SWITCH
01576         ELSE
01577             MOVE ER-0236         TO EMI-ERROR
01578             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01579             MOVE AL-UABON        TO CRTLNMEA
01580             MOVE -1              TO CRTLNMEL
01581             MOVE 'X'             TO ERROR-SWITCH
01582      ELSE
01583          MOVE AL-UANOF           TO CRTLNMEA.
01584
01585      IF CRTFNMEI GREATER THAN LOW-VALUES
01586          MOVE AL-UANON           TO CRTFNMEA
01587          MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH
01588      ELSE
01589          MOVE AL-UANOF           TO CRTFNMEA.
01590
01591      IF CRTINITI GREATER THAN LOW-VALUES
01592          MOVE AL-UANON           TO CRTINITA
01593          MOVE 'X' TO UPDATE-SWITCH CERT-SWITCH CERT-KEY-SWITCH
01594      ELSE
01595          MOVE AL-UANOF           TO CRTINITA.
01596
01597      IF ISSAGEL GREATER THAN ZERO
01598          IF ISSAGEI NUMERIC
01599             MOVE AL-UANON        TO ISSAGEA
01600             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
01601          ELSE
01602             MOVE ER-0237         TO EMI-ERROR
01603             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01604             MOVE AL-UABON        TO ISSAGEA
01605             MOVE -1              TO ISSAGEL
01606             MOVE 'X'             TO ERROR-SWITCH
01607      ELSE
01608          MOVE AL-UANOF           TO ISSAGEA.
01609
01610      IF JNTFNMEI GREATER THAN LOW-VALUES
01611          MOVE AL-UANON           TO JNTFNMEA
01612          MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH
01613      ELSE
01614          MOVE AL-UANOF           TO JNTFNMEA.
01615
01616      IF JNTLNMEI GREATER THAN LOW-VALUES
01617          MOVE AL-UANON           TO JNTLNMEA
01618          MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH
01619      ELSE
01620          MOVE AL-UANOF           TO JNTLNMEA.
01621
01622      IF JNTINITI GREATER THAN LOW-VALUES
01623          MOVE AL-UANON           TO JNTINITA
01624          MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH
01625      ELSE
01626          MOVE AL-UANOF           TO JNTINITA.
01627
01628      IF JNTAGEL GREATER THAN ZERO
01629          IF JNTAGEI NUMERIC
01630             MOVE AL-UANON        TO JNTAGEA
01631             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
01632          ELSE
01633             MOVE ER-0238         TO EMI-ERROR
01634             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01635             MOVE AL-UABON        TO JNTAGEA
01636             MOVE -1              TO JNTAGEL
01637             MOVE 'X'             TO ERROR-SWITCH
01638      ELSE
01639          MOVE AL-UANOF           TO JNTAGEA.
01640
01641      IF ADDONDTI GREATER THAN LOW-VALUES
01642          MOVE LOW-VALUES         TO HOLD-ADDON
01643          IF ADDONDTI = SPACES
01644              MOVE 'X'  TO UPDATE-SWITCH MSTR-SWITCH CERT-SWITCH
01645          ELSE
01646              MOVE ADDONDTI       TO DEEDIT-DATE-INPUT
01647              
      * EXEC CICS BIF DEEDIT
01648 *                FIELD    (DEEDIT-DATE-INPUT)
01649 *                LENGTH   (DATE-LENGTH)
01650 *            END-EXEC
      *    MOVE '@"L                   #   #00008805' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01651              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
01652              MOVE '4'            TO DC-OPTION-CODE
01653              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01654              IF NOT DATE-CONVERSION-ERROR
01655                  MOVE DC-BIN-DATE-1      TO HOLD-ADDON
01656                  MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
01657                                                   CERT-SWITCH
01658                  MOVE AL-UANON   TO ADDONDTA
01659                  MOVE DC-GREG-DATE-1-EDIT TO ADDONDTO
01660              ELSE
01661                  MOVE ER-7651    TO EMI-ERROR
01662                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01663                  MOVE AL-UABON   TO ADDONDTA
01664                  MOVE -1         TO ADDONDTL
01665                  MOVE 'X'        TO ERROR-SWITCH
01666        ELSE
01667          MOVE AL-UANOF TO ADDONDTA.
01668
01669      IF LCVCDI GREATER THAN LOW-VALUES
01670          MOVE LCVCDI             TO WS-EDIT-BEN-CODE
01671          PERFORM 1240-EDIT-LIFE-CODE THRU 1240-EXIT
01672      ELSE
01673          MOVE AL-UANOF           TO LCVCDA.
01674
01675      IF ACVCDI GREATER THAN LOW-VALUES
01676          MOVE ACVCDI             TO WS-EDIT-BEN-CODE
01677          PERFORM 1310-EDIT-AH-CODE THRU 1310-EXIT
01678      ELSE
01679          MOVE AL-UANOF           TO ACVCDA.
01680
01681      IF LCVOTRMI GREATER THAN LOW-VALUES
01682          
      * EXEC CICS BIF DEEDIT
01683 *            FIELD (LCVOTRMI)
01684 *            LENGTH(3)
01685 *        END-EXEC
           MOVE 3
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008840' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVOTRMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01686          IF LCVOTRMI IS NUMERIC
01687              MOVE LCVOTRMI       TO LCVOTRMO
01688              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
01689              MOVE AL-UANON       TO LCVOTRMA
01690          ELSE
01691              MOVE ER-0241        TO EMI-ERROR
01692              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01693              MOVE AL-UNBON       TO LCVOTRMA
01694              MOVE 'X'            TO ERROR-SWITCH
01695              MOVE -1             TO LCVOTRML
01696      ELSE
01697          MOVE AL-UANOF           TO LCVOTRMA.
01698
01699      IF ACVOTRMI GREATER THAN LOW-VALUES
01700          
      * EXEC CICS BIF DEEDIT
01701 *            FIELD (ACVOTRMI)
01702 *            LENGTH(3)
01703 *        END-EXEC
           MOVE 3
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008858' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVOTRMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01704          IF ACVOTRMI IS NUMERIC
01705              MOVE ACVOTRMI       TO ACVOTRMO
01706              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
01707              MOVE AL-UANON       TO ACVOTRMA
01708          ELSE
01709              MOVE ER-0241        TO EMI-ERROR
01710              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01711              MOVE AL-UNBON       TO ACVOTRMA
01712              MOVE 'X'            TO ERROR-SWITCH
01713              MOVE -1             TO ACVOTRML
01714      ELSE
01715          MOVE AL-UANOF           TO ACVOTRMA.
01716
01717      IF LCVRATEL GREATER THAN +0
01718          
      * EXEC CICS BIF DEEDIT
01719 *            FIELD (LCVRATEI)
01720 *            LENGTH(6)
01721 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008876' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01722          IF LCVRATEI IS NUMERIC
01723              MOVE LCVRATEI       TO HOLD-LF-RATE  LCVRATEO
01724              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
01725              MOVE AL-UNNON       TO LCVRATEA
01726          ELSE
01727              MOVE ER-2280        TO EMI-ERROR
01728              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01729              MOVE AL-UNBON       TO LCVRATEA
01730              MOVE 'X'            TO ERROR-SWITCH
01731              MOVE -1             TO LCVRATEL
01732      ELSE
01733          MOVE AL-UNNOF           TO LCVRATEA.
01734
01735      IF ACVRATEL GREATER THAN +0
01736          
      * EXEC CICS BIF DEEDIT
01737 *            FIELD (ACVRATEI)
01738 *            LENGTH(6)
01739 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008894' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01740          IF ACVRATEI IS NUMERIC
01741              MOVE ACVRATEI       TO HOLD-AH-RATE  ACVRATEO
01742              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
01743              MOVE AL-UNNON       TO ACVRATEA
01744          ELSE
01745              MOVE ER-2280        TO EMI-ERROR
01746              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01747              MOVE AL-UNBON       TO ACVRATEA
01748              MOVE 'X'            TO ERROR-SWITCH
01749              MOVE -1             TO ACVRATEL
01750      ELSE
01751          MOVE AL-UNNOF           TO ACVRATEA.
01752
01753      IF LCVBENEL GREATER THAN ZERO
01754          
      * EXEC CICS BIF DEEDIT
01755 *            FIELD (LCVBENEI)
01756 *            LENGTH (11)
01757 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008912' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVBENEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01758          IF LCVBENEI IS NUMERIC
01759             MOVE LCVBENEI        TO HOLD-LF-CV-BEN  LCVBENEO
01760             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
01761             MOVE AL-UNNON        TO LCVBENEA
01762          ELSE
01763             MOVE ER-0243         TO EMI-ERROR
01764             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01765             MOVE AL-UNBON        TO LCVBENEA
01766             MOVE -1              TO LCVBENEL
01767             MOVE 'X'             TO ERROR-SWITCH
01768      ELSE
01769          MOVE AL-UANOF           TO LCVBENEA.
01770
01771      IF ACVBENEL GREATER THAN ZERO
01772          
      * EXEC CICS BIF DEEDIT
01773 *            FIELD (ACVBENEI)
01774 *            LENGTH (11)
01775 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008930' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVBENEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01776          IF ACVBENEI IS NUMERIC
01777             MOVE ACVBENEI        TO HOLD-AH-CV-BEN  ACVBENEO
01778             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
01779             MOVE AL-UNNON        TO ACVBENEA
01780          ELSE
01781             MOVE ER-0243         TO EMI-ERROR
01782             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01783             MOVE AL-UNBON        TO ACVBENEA
01784             MOVE -1              TO ACVBENEL
01785             MOVE 'X'             TO ERROR-SWITCH
01786      ELSE
01787          MOVE AL-UANOF           TO ACVBENEA.
01788
01789      IF LCVFORMI GREATER THAN LOW-VALUES
01790         MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH
01791         MOVE AL-UANON            TO LCVFORMA
01792      ELSE
01793         MOVE AL-UANOF            TO LCVFORMA.
01794
01795      IF ACVFORMI GREATER THAN LOW-VALUES
01796         MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH
01797         MOVE AL-UANON            TO ACVFORMA
01798      ELSE
01799         MOVE AL-UANOF            TO ACVFORMA.
01800
01801      IF LCVCNDTL GREATER THAN ZERO
01802          MOVE LOW-VALUES         TO HOLD-LF-CV-CAN
01803          IF LCVCNDTI = SPACES
01804              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
01805          ELSE
01806              MOVE LCVCNDTI       TO DEEDIT-DATE-INPUT
01807              PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT
01808              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
01809              MOVE '4'            TO DC-OPTION-CODE
01810              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01811              IF NOT DATE-CONVERSION-ERROR
01812                  MOVE DC-BIN-DATE-1      TO HOLD-LF-CV-CAN
01813                  MOVE AL-UANON   TO LCVCNDTA
01814                  MOVE 'X'        TO UPDATE-SWITCH CERT-SWITCH
01815                  MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO
01816              ELSE
01817                  MOVE ER-0246    TO EMI-ERROR
01818                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01819                  MOVE AL-UABON   TO LCVCNDTA
01820                  MOVE -1         TO LCVCNDTL
01821                  MOVE 'X'        TO ERROR-SWITCH
01822      ELSE
01823          MOVE LOW-VALUES         TO HOLD-LF-CV-CAN
01824          MOVE AL-UANOF           TO LCVCNDTA.
01825
01826      IF ACVCNDTL GREATER THAN ZERO
01827          MOVE LOW-VALUES         TO HOLD-AH-CV-CAN
01828          IF ACVCNDTI = SPACES
01829              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
01830          ELSE
01831              MOVE ACVCNDTI       TO DEEDIT-DATE-INPUT
01832              PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT
01833              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
01834              MOVE '4'            TO DC-OPTION-CODE
01835              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01836              IF NOT DATE-CONVERSION-ERROR
01837                  MOVE DC-BIN-DATE-1      TO HOLD-AH-CV-CAN
01838                  MOVE AL-UANON   TO ACVCNDTA
01839                  MOVE 'X'        TO UPDATE-SWITCH CERT-SWITCH
01840                  MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO
01841              ELSE
01842                  MOVE ER-0246    TO EMI-ERROR
01843                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01844                  MOVE AL-UABON   TO ACVCNDTA
01845                  MOVE -1         TO ACVCNDTL
01846                  MOVE 'X'        TO ERROR-SWITCH
01847      ELSE
01848          MOVE LOW-VALUES         TO HOLD-AH-CV-CAN
01849          MOVE AL-UANOF           TO ACVCNDTA.
01850
01851      IF APRL GREATER THAN +0
01852          
      * EXEC CICS BIF DEEDIT
01853 *            FIELD    (APRI)
01854 *            LENGTH   (8)
01855 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009010' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APRI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01856          IF APRI NUMERIC
01857             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
01858             MOVE APRI            TO HOLD-APR APRO
01859             MOVE AL-UANON        TO APRA
01860           ELSE
01861             MOVE ER-0259         TO EMI-ERROR
01862             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01863             MOVE AL-UABON        TO APRA
01864             MOVE -1              TO APRL
01865             MOVE 'X'             TO ERROR-SWITCH
01866      ELSE
01867          MOVE AL-UANOF           TO APRA.
01868
01869      IF PMTFREQL GREATER THAN ZERO
01870          
      * EXEC CICS BIF DEEDIT
01871 *            FIELD    (PMTFREQI)
01872 *            LENGTH   (2)
01873 *        END-EXEC
           MOVE 2
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009028' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PMTFREQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01874          IF PMTFREQI NUMERIC
01875             MOVE PMTFREQI        TO  HOLD-FREQ PMTFREQO
01876             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
01877             MOVE AL-UANON        TO PMTFREQA
01878           ELSE
01879             MOVE ER-0427         TO EMI-ERROR
01880             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01881             MOVE AL-UABON        TO PMTFREQA
01882             MOVE -1              TO PMTFREQL
01883             MOVE 'X'             TO ERROR-SWITCH
01884      ELSE
01885          MOVE AL-UANOF           TO PMTFREQA.
01886
01887      IF INDGRPL GREATER THAN ZERO
01888          IF INDGRPI = 'I' OR 'G' OR SPACE
01889              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
01890              MOVE AL-UANON       TO INDGRPA
01891           ELSE
01892              MOVE ER-0260        TO EMI-ERROR
01893              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01894              MOVE AL-UABON       TO INDGRPA
01895              MOVE -1             TO INDGRPL
01896              MOVE 'X'            TO ERROR-SWITCH
01897      ELSE
01898          MOVE AL-UANOF           TO INDGRPA.
01899
01900      IF PREMTYPL GREATER THAN ZERO
01901          IF PREMTYPI = '1' OR '2' OR '3'
01902             MOVE PREMTYPI        TO PI-PREM-TYPE
01903             MOVE AL-UNNON        TO PREMTYPA
01904             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
01905           ELSE
01906             MOVE ER-0227         TO EMI-ERROR
01907             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01908             MOVE AL-UNBON        TO PREMTYPA
01909             MOVE -1              TO PREMTYPL
01910             MOVE 'X'             TO ERROR-SWITCH
01911      ELSE
01912          MOVE AL-UANOF           TO PREMTYPA.
01913
01914      IF LOANBALL GREATER THAN ZERO
01915          
      * EXEC CICS BIF DEEDIT
01916 *             FIELD    (LOANBALI)
01917 *             LENGTH   (9)
01918 *        END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009073' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOANBALI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01919          IF LOANBALI NUMERIC
01920              MOVE LOANBALI       TO  HOLD-LOANBAL LOANBALO
01921              MOVE 'X' TO UPDATE-SWITCH CERT-SWITCH MSTR-SWITCH
01922              MOVE AL-UNNON TO LOANBALA
01923          ELSE
01924              MOVE ER-0639        TO EMI-ERROR
01925              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01926              MOVE AL-UNBON       TO LOANBALA
01927              MOVE -1             TO LOANBALL
01928              MOVE 'X'            TO ERROR-SWITCH
01929      ELSE
01930          MOVE AL-UNNOF           TO LOANBALA.
01931
062217*    IF LOANNOL GREATER THAN ZERO
062217*          MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH
062217*          MOVE AL-UANON            TO LOANNOA
062217*       ELSE
062217*          MOVE AL-UANOF            TO LOANNOA.
01937
01938      IF REINCDI GREATER THAN LOW-VALUES
01939         MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH
01940         MOVE AL-UANON            TO REINCDA
01941      ELSE
01942         MOVE AL-UANOF            TO REINCDA.
01943
01944      IF CERTI GREATER THAN LOW-VALUES
01945          PERFORM 1460-EDIT-CERT-NO THRU 1460-EXIT
01946      ELSE
01947          MOVE AL-UANOF           TO CERTA.
01948
01949      IF CERTEFFI GREATER THAN LOW-VALUES
01950          PERFORM 1470-EDIT-EFF THRU 1470-EXIT
01951      ELSE
01952          MOVE AL-UANOF           TO CERTEFFA.
01953
01954      IF CERTACTI GREATER THAN LOW-VALUES
01955          PERFORM 1480-EDIT-ACCT THRU 1480-EXIT
01956      ELSE
01957          MOVE AL-UANOF           TO CERTACTA.
01958
01959      IF CERTSTI GREATER THAN LOW-VALUES
01960          PERFORM 1490-EDIT-STATE THRU 1490-EXIT
01961      ELSE
01962          MOVE AL-UANOF           TO CERTSTA.
01963
01964      IF CERTCARI GREATER THAN LOW-VALUES
01965          PERFORM 1500-EDIT-CARR THRU 1500-EXIT
01966      ELSE
01967          MOVE AL-UANOF           TO CERTCARA.
01901
CIDMOD     IF DLYACTV-RECORD-NEEDED
CIDMOD         PERFORM 1150-OUTPUT-ACTIVITY-RECORD THRU
CIDMOD                 1150-EXIT
CIDMOD     END-IF.
CIDMOD
01969      IF CERTEFFI GREATER LOW-VALUES OR
01970         CERTACTI GREATER LOW-VALUES OR
01971         CERTSTI  GREATER LOW-VALUES OR
01972         CERTCARI GREATER LOW-VALUES OR
01973         CERTGRPI GREATER LOW-VALUES
01974         PERFORM 1620-VERIFY-ACCT THRU 1620-EXIT.
01975
121802*    IF PI-COMPANY-ID = 'CRI' OR 'PEM' OR 'NCL'
121802*      IF PI-PREM-TYPE NOT = '1'  AND
121802*         PI-NO-PMTS = ZEROS
121802*          IF LOANBALL GREATER +0 OR
121802*             ACVBENEL GREATER +0
121802*               PERFORM 1620-VERIFY-ACCT THRU 1620-EXIT.
           if not acct-found
              perform 1620-VERIFY-ACCT THRU 1620-EXIT
           end-if
           MOVE PI-COMPANY-CD          TO COMPANY-CODE
           MOVE PI-CARRIER             TO CARRIER-CODE
           MOVE PI-CLAIM-NO            TO CLAIM-NO
           MOVE PI-CERT-NO             TO CERT-NO
           
      * EXEC CICS READ
      *        DATASET   (CLMS-FILE-ID)
      *        RIDFLD    (MSTR-KEY)
      *        SET       (ADDRESS OF CLAIM-MASTER)
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        E          (  N#00009152' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE CL-COMPANY-CD          TO CERT-COMPANY-CODE
           MOVE CL-CERT-CARRIER        TO CERT-CARRIER
           MOVE CL-CERT-GROUPING       TO CERT-GROUP
           MOVE CL-CERT-STATE          TO CERT-STATE
           MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
           MOVE CL-CERT-EFF-DT         TO CERT-DATE
           MOVE CL-CERT-NO             TO CERT-CERT
           
      * EXEC CICS READ
      *       DATASET   (CERT-FILE-ID)
      *       RIDFLD    (CERT-KEY)
      *       SET       (ADDRESS OF CERTIFICATE-MASTER)
      *       RESP      (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        E          (  N#00009165' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if mfnamei > low-values
              move mfnamei to w-name-first
           else
              move cl-insured-1st-name to w-name-first
           end-if
           if mlnamei > low-values
              move mlnamei to w-name-last
           else
              move cl-insured-last-name to w-name-last
           end-if
           if instypel > zeros
              evaluate true
                 when cl-no-of-days-paid > zeros
                    move er-1668       to emi-error
                    perform 9900-error-format
                                       thru 9900-exit
                    move al-uabon      to instypea
                    move -1            to instypel
                    move 'X'           to error-switch
                 when instypei = 'C'
                    if (w-name-first (1:10) = cm-insured-first-name)
011020                 and (w-name-last = cm-insured-last-name)
                       move er-1666    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                       move al-uabon   to instypea
                       move -1         to instypel
                       move 'X'        to error-switch
                    end-if
                 when instypei = 'P'
                    if (w-name-first (1:10) <> cm-insured-first-name)
                       or (w-name-last <> cm-insured-last-name)
                       move er-1676    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                       move al-uabon   to instypea
                       move -1         to instypel
                       move 'X'        to error-switch
                    end-if
              end-evaluate
           end-if
      *    if (instypel > zeros)
      *       and (cl-no-of-days-paid > zero)
      *       move er-1668             to emi-error
      *       perform 9900-error-format thru 9900-exit
      *       move al-uabon            to instypea
      *       move -1                  to instypel
      *       move 'X'                 to error-switch
      *    end-if
      *
      *    if instypel > zero
      *       if instypei = 'C'
      *          if w-name-first (1:10) = cm-insured-first-name
      *             move er-1666          to emi-error
      *             PERFORM 9900-ERROR-FORMAT
      *                                   THRU 9900-EXIT
      *             move al-uabon         to instypea
      *             move -1               to instypel
      *             move 'X'              to error-switch
      *          end-if
      *       end-if
      *    end-if
020816     if (pi-company-id = 'DCC' OR 'VPP')
              and (acct-found)
              display ' about to perform 2120 '
              perform 2120-check-pdef  thru 2120-exit
              display ' back from 2120- ' ws-pre-exsist
081817        IF EXTENSL > ZERO
081817           IF EXTENSI NUMERIC
081817              IF EXTENSI >  WS-MAX-EXTENSION
081817                AND ERPDEF-FOUND
081817                 MOVE ER-1678          TO EMI-ERROR
081817                 PERFORM 9900-ERROR-FORMAT
081817                                       THRU 9900-EXIT
081817                 MOVE AL-UABON         TO EXTENSA
081817                 MOVE -1               TO EXTENSL
081817                 MOVE 'X'              TO ERROR-SWITCH
081817              ELSE
081817                 MOVE 'X'              TO UPDATE-SWITCH
081817              END-IF
081817           ELSE
081817              MOVE ER-1778          TO EMI-ERROR
081817              PERFORM 9900-ERROR-FORMAT
081817                                    THRU 9900-EXIT
081817              MOVE AL-UABON         TO EXTENSA
081817              MOVE -1               TO EXTENSL
081817              MOVE 'X'              TO ERROR-SWITCH
081817           END-IF
081817        END-IF
           end-if
           if benperl > zeros
              move al-uanon            to benpera
           end-if
           if benperl > zeros
              if benperi numeric
                 perform 1015-edit-benefit-period
                                       thru 1015-exit
              else
                 move er-1669          to emi-error
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 move al-uabon         to benpera
                 move -1               to benperl
                 move 'X'              to error-switch
              end-if
          end-if
           if instypel > zeros
              if instypei = 'P' OR 'C'
                 move 'X'              to update-switch
                                          mstr-switch
                 move al-uanon         to instypea
              else
                  move er-1654          to emi-error
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 move al-uabon         to instypea
                 move -1               to instypel
                 move 'X'              to error-switch
              end-if
          end-if
052113     if accswl > zeros
052113        if accswi = 'Y' OR 'N'
052113           MOVE 'X'              TO UPDATE-SWITCH
052113                                    MSTR-SWITCH
052113           MOVE AL-UANON         TO accswa
052113*       else
052113*          move er-1656          to emi-error
052113*          PERFORM 9900-ERROR-FORMAT
052113*                                THRU 9900-EXIT
052113*          move al-uabon         to accswa
052113*          move -1               to accswl
052113*          move 'X'              to error-switch
052113*          move al-uanof         to accswa
052113        end-if
052113     end-if
01983      IF CERTL = ZEROS AND
01984         SUFXL = ZEROS
01985           GO TO 1010-EXIT.
01986
01987      IF CERTL GREATER THAN ZEROS
01988          IF CERTI NOT = PI-SAVE-CERT-NO-PRIME
01989              MOVE CERTI          TO PI-SAVE-CERT-NO-PRIME
01990              MOVE 'Y'            TO SKIP-ATTRIBUTE
01991              MOVE 'X'            TO ERROR-SWITCH
01992              PERFORM 7000-RESET-ATTRIBUTE THRU 7000-EXIT.
01993
01994      IF SUFXL GREATER THAN ZEROS
01995          IF SUFXI NOT = PI-SAVE-CERT-NO-SUFX
01996              MOVE SUFXI          TO PI-SAVE-CERT-NO-SUFX
01997              MOVE 'Y'            TO SKIP-ATTRIBUTE
01998              MOVE 'X'            TO ERROR-SWITCH
01999              PERFORM 7000-RESET-ATTRIBUTE THRU 7000-EXIT.
02000
02001  1010-EXIT.
02002      EXIT.
02003
       1015-edit-benefit-period.
           move ' '                    to ws-benper-sw
           evaluate true
              when not erpdef-found
                 set good-benper       to true
              when pd-recurring-yn (a1) = 'Y'
                 set good-benper       to true
              when pd-recurring-yn (a1) = 'N'
                 if benperi < 02
                    set good-benper    to true
                 end-if
              when pd-rec-crit-period (a1) numeric
                 if benperi <= pd-rec-crit-period (a1)
                    set good-benper    to true
                 end-if
           end-evaluate
           MOVE CERT-KEY               TO ELCRTT-KEY
           MOVE 'B'                    TO CTRLR-REC-TYPE
           
      * EXEC CICS READ
      *       DATASET   ('ELCRTT')
      *       RIDFLD    (ELCRTT-KEY)
      *       INTO      (CERTIFICATE-TRAILERS)
      *       RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00009345' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039333435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF WS-RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or ((cs-claim-type (s1) = cl-claim-type)
                 and (cs-insured-type (s1) = cl-insured-type)
                 and (cs-benefit-period (s1) > benperi))
              end-perform
              if s1 < +25
                 move er-1665          to emi-error
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 move al-uabon         to benpera
                 move -1               to benperl
                 move 'X'              to error-switch
                 go to 1015-exit
      *       else
      *          set good-benper       to true
              end-if
           end-if
           if good-benper
              move 'X'                 to update-switch
                                          mstr-switch
              move al-uanon            to benpera
           else
              move er-1669             to emi-error
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              move al-uabon            to benpera
              move -1                  to benperl
              move 'X'                 to error-switch
           end-if
           .
       1015-exit.
           exit.
021418 1030-EDIT-NAME.
021418     IF INSTYPEL > ZEROS
021418        MOVE INSTYPEI TO CL-INSURED-TYPE
021418     END-IF
021418     IF MFNAMEL > +0
021418        IF CL-INSURED-TYPE = 'C'
021418          AND (CM-INSURED-FIRST-NAME = MFNAMEI(1:10))
021418           MOVE ER-1675             TO EMI-ERROR
021418           MOVE -1                  TO MFNAMEL
021418           MOVE AL-UABON            TO MFNAMEA
021418           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
021418           MOVE 'X'                 TO ERROR-SWITCH
021418        ELSE
021418          IF CL-INSURED-TYPE = 'P'
021418            AND (CM-INSURED-FIRST-NAME <> MFNAMEI(1:10))
021418             MOVE ER-1676             TO EMI-ERROR
021418             MOVE -1                  TO MFNAMEL
021418             MOVE AL-UABON            TO MFNAMEA
021418             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
021418             MOVE 'X'                 TO ERROR-SWITCH
021418          END-IF
021418        END-IF
021418     END-IF.
021418
021418 1030-EXIT.
021418     EXIT.
031815 1050-EDIT-BENE.
031815     MOVE PI-COMPANY-CD      TO TRLR-COMPANY-CD.
031815     MOVE PI-CARRIER         TO TRLR-CARRIER.
031815     MOVE PI-CLAIM-NO        TO TRLR-CLAIM-NO.
031815     MOVE PI-CERT-NO         TO TRLR-CERT-NO.
031815     MOVE 1000               TO TRLR-SEQ-NO.
031815
031815     
      * EXEC CICS HANDLE CONDITION
031815*         NOTFND    (1052-DONE)
031815*         ENDFILE   (1052-DONE)
031815*    END-EXEC.
      *    MOVE '"$I''                  ! & #00009418' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303039343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
031815
031815     
      * EXEC CICS STARTBR
031815*        DATASET (TRLR-FILE-ID)
031815*        RIDFLD  (TRLR-KEY)
031815*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009423' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
031815
031815
031815     MOVE SPACES TO WS-TRLR-FILE-EOF
031815                    WS-PAYMENT-PENDING
031815     PERFORM 1052-READ-TRLR THRU 1052-EXIT
031815       UNTIL  TRLR-FILE-EOF
031815         OR PAYMENT-PENDING
031815
031815     
      * EXEC CICS ENDBR
031815*        DATASET (TRLR-FILE-ID)
031815*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009435' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
031815 1050-EXIT.
031815     EXIT.
031815
031815 1052-READ-TRLR.
031815     
      * EXEC CICS READNEXT
031815*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
031815*         DATASET  (TRLR-FILE-ID)
031815*         RIDFLD   (TRLR-KEY)
031815*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009442' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
031815
031815     IF PI-COMPANY-CD   = TRLR-COMPANY-CD
031815       AND PI-CARRIER  = TRLR-CARRIER
031815       AND PI-CLAIM-NO = TRLR-CLAIM-NO
031815       AND PI-CERT-NO  = TRLR-CERT-NO
031815        CONTINUE
031815     ELSE
031815        GO TO 1052-DONE
031815     END-IF.
031815
031815     IF PAYMENT-TR
031815       AND BENEFICIARY-PAID
031815       AND AT-VOID-DT = LOW-VALUES
031815       AND AT-CHECK-WRITTEN-DT NOT > SPACES
031815         SET PAYMENT-PENDING TO TRUE
031815     END-IF.
062217
062217     IF CORRESPONDENCE-TR
062217        IF AT-AUTH-RCVD = 'Y'
062217           SET AUTH-RCVD TO TRUE
062217        ELSE
062217        IF AT-AUTH-RCVD = 'N'
062217           SET NO-AUTH-RCVD TO TRUE
062217        END-IF
062217        END-IF
062217     END-IF.
031815
031815     GO TO 1052-EXIT.
031815
031815 1052-DONE.
031815     SET TRLR-FILE-EOF TO TRUE.
031815 1052-EXIT.
031815     EXIT.
031815     EJECT
02005  1120-EDIT-PROC.
02006      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
02007      MOVE '2'                    TO CNTL-REC-TYPE.
02008      MOVE PROCI                  TO CNTL-PROC-ID.
02009      MOVE ZERO                   TO CNTL-SEQ-NO.
02010
02011      
      * EXEC CICS HANDLE CONDITION
02012 *         NOTFND    (1120-CNTL-NOTFND)
02013 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00009487' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303039343837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02014
02015      
      * EXEC CICS READ
02016 *         SET      (ADDRESS OF CONTROL-FILE)
02017 *         DATASET  (CNTL-FILE-ID)
02018 *         RIDFLD   (CNTL-KEY)
02019 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009491' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02020
02021      MOVE AL-UANON               TO PROCA.
02022      MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH MSTR-KEY-SWITCH.
02023      GO TO 1120-EXIT.
02024
02025  1120-CNTL-NOTFND.
02026      MOVE ER-0273                TO EMI-ERROR.
02027      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02028      MOVE AL-UABON               TO PROCA.
02029      MOVE -1                     TO PROCL.
02030      MOVE 'X'                    TO ERROR-SWITCH.
02031
02032  1120-EXIT.
02033      EXIT.
CIDMOD
CIDMOD*************************************************************
CIDMOD*****    BEGIN PROCESSING FOR OUTPUTTING DLYACTV RECORD    **
CIDMOD*************************************************************
CIDMOD
CIDMOD 1150-OUTPUT-ACTIVITY-RECORD.
CIDMOD
CIDMOD     
      * EXEC CICS GETMAIN
CIDMOD*        SET (ADDRESS OF DAILY-ACTIVITY-RECORD)
CIDMOD*        LENGTH (25)
CIDMOD*        INITIMG (WS-BLANK)
CIDMOD*    END-EXEC.
           MOVE 25
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00009517' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
CIDMOD     MOVE PI-COMPANY-CD          TO DA-COMP-CD.
CIDMOD     MOVE PI-CARRIER             TO DA-CARRIER.
CIDMOD     MOVE PI-CLAIM-NO            TO DA-CLAIM-NO.
CIDMOD     MOVE PI-CERT-NO             TO DA-CERT-NO.
CIDMOD     MOVE ZEROS                  TO DA-TRAILER-SEQ-NO.
CIDMOD     MOVE WS-HOLD-CLAIM-STATUS   TO DA-RECORD-TYPE.
CIDMOD
CIDMOD     
      * EXEC CICS HANDLE CONDITION
CIDMOD*        NOTOPEN (1150-NOTOPEN-ERROR)
CIDMOD*        DUPREC (1150-EXIT)
CIDMOD*    END-EXEC.
      *    MOVE '"$J%                  ! ( #00009531' TO DFHEIV0
           MOVE X'22244A252020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303039353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD     MOVE DAILY-ACTIVITY-RECORD  TO JP-RECORD-AREA.
CIDMOD     MOVE 'DLYACTV'              TO JP-FILE-ID.
CIDMOD     MOVE 'A'                    TO JP-RECORD-TYPE.
CIDMOD     MOVE +48                    TO JOURNAL-LENGTH.
CIDMOD
CIDMOD     
      * EXEC CICS WRITE
CIDMOD*        DATASET ('DLYACTV')
CIDMOD*        RIDFLD (DA-KEY)
CIDMOD*        FROM (DAILY-ACTIVITY-RECORD)
CIDMOD*        LENGTH (25)
CIDMOD*    END-EXEC.
           MOVE 'DLYACTV' TO DFHEIV1
           MOVE 25
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009540' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DAILY-ACTIVITY-RECORD, 
                 DFHEIV11, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     GO TO 1150-EXIT.
CIDMOD
CIDMOD 1150-NOTOPEN-ERROR.
CIDMOD
CIDMOD     MOVE '2955'                 TO EMI-ERROR.
CIDMOD     MOVE -1                     TO MAINTL.
CIDMOD     MOVE AL-UANON               TO MAINTA.
CIDMOD     MOVE 'X'                    TO ERROR-SWITCH.
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU
CIDMOD             9900-EXIT.
CIDMOD
CIDMOD 1150-EXIT.
CIDMOD     EXIT.
CIDMOD
CIDMOD*************************************************************
CIDMOD*****    END OF PROCESSING FOR OUTPUTTING DLYACTV RECORD    *
CIDMOD*************************************************************
CIDMOD
02035  1240-EDIT-LIFE-CODE.
02036
02037      IF INVALID-BENEFIT-CODE
02038          GO TO 1240-SET-ERROR.
02039
02040      MOVE SPACES                 TO BENEFIT-KEY.
02041      MOVE PI-COMPANY-ID          TO BEN-CO-ID.
02042      MOVE '4'                    TO BEN-REC-TYPE.
02043      MOVE LCVCDI                 TO BEN-ACC-CD HOLD-BENEFIT.
02044      MOVE ZERO                   TO BEN-SEQ-NO.
02045
02046      
      * EXEC CICS READ GTEQ
02047 *         DATASET  (CNTL-FILE-ID)
02048 *         RIDFLD   (BENEFIT-KEY)
02049 *         SET      (ADDRESS OF CONTROL-FILE)
02050 *    END-EXEC.
      *    MOVE '&"S        G          (   #00009576' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02051
02052      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID OR
02053         CF-RECORD-TYPE NOT = '4'
02054          GO TO 1240-SET-ERROR.
02055
02056      MOVE ZERO                   TO COUNT-2.
02057      PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.
02058
02059      IF SCREEN-ERROR
02060          GO TO 1240-SET-ERROR.
02061
02062  1240-SET-CODE-SWITCHES.
02063      MOVE 'X'                    TO UPDATE-SWITCH CERT-SWITCH.
02064      MOVE AL-UANON               TO LCVCDA.
02065      GO TO 1240-EXIT.
02066
02067  1240-SET-ERROR.
02068      MOVE ER-0240                TO EMI-ERROR.
02069      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02070      MOVE 'X'                    TO ERROR-SWITCH.
02071      MOVE AL-UABON               TO LCVCDA.
02072      MOVE -1                     TO LCVCDL.
02073
02074  1240-EXIT.
02075      EXIT.
02076
02077  1310-EDIT-AH-CODE.
02078      IF INVALID-BENEFIT-CODE
02079          GO TO 1310-SET-ERROR.
02080
02081      MOVE SPACES                 TO BENEFIT-KEY.
02082      MOVE PI-COMPANY-ID          TO BEN-CO-ID.
02083      MOVE '5'                    TO BEN-REC-TYPE.
02084      MOVE ACVCDI                 TO BEN-ACC-CD HOLD-BENEFIT.
02085      MOVE ZERO                   TO BEN-SEQ-NO.
02086
02087      
      * EXEC CICS READ GTEQ
02088 *         DATASET  (CNTL-FILE-ID)
02089 *         RIDFLD   (BENEFIT-KEY)
02090 *         SET      (ADDRESS OF CONTROL-FILE)
02091 *    END-EXEC.
      *    MOVE '&"S        G          (   #00009617' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02092
02093      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID OR
02094         CF-RECORD-TYPE NOT = '5'
02095          GO TO 1310-SET-ERROR.
02096
02097      MOVE ZERO                   TO COUNT-2.
02098      PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.
02099      IF SCREEN-ERROR
02100          GO TO 1310-SET-ERROR.
02101
02102  1310-SET-CODE-SWITCHES.
02103      MOVE 'X'                    TO UPDATE-SWITCH CERT-SWITCH.
02104      MOVE AL-UANON               TO ACVCDA.
02105      GO TO 1310-EXIT.
02106
02107  1310-SET-ERROR.
02108      MOVE ER-0250                TO EMI-ERROR.
02109      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02110      MOVE AL-UABON               TO ACVCDA.
02111      MOVE 'X'                    TO ERROR-SWITCH.
02112      MOVE -1                     TO ACVCDL.
02113
02114  1310-EXIT.
02115      EXIT.
02116
02117      EJECT
02118  1460-EDIT-CERT-NO.
02119      MOVE CERTI                  TO WS-CERT-NO-PRIME.
02120      MOVE SUFXI                  TO WS-CERT-NO-SUFX.
02121
02122      IF WS-SAVE-CERT-NO = SPACES
02123          MOVE ER-0203            TO EMI-ERROR
02124          GO TO 1460-CERT-NO-ERROR.
02125
02126      IF WS-CERT-NO-PRIME = SPACES AND
02127         WS-CERT-NO-SUFX GREATER THAN SPACE
02128             MOVE ER-0210         TO EMI-ERROR
02129             GO TO 1460-CERT-NO-ERROR.
02130
02131      MOVE 'X'                    TO UPDATE-SWITCH.
02132      MOVE 'Y'                    TO CERT-KEY-SWITCH.
02133      MOVE AL-UANON               TO CERTA.
02134
02135      IF SUFXL IS GREATER THAN 0
02136          MOVE AL-UANON           TO SUFXA.
02137
02138      GO TO 1460-EXIT.
02139
02140  1460-CERT-NO-ERROR.
02141      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02142      MOVE AL-UABON               TO SUFXA.
02143      MOVE AL-UABON               TO CERTA.
02144      MOVE -1                     TO CERTL.
02145      MOVE 'X'                    TO ERROR-SWITCH.
02146
02147  1460-EXIT.
02148      EXIT.
02149
02150  1470-EDIT-EFF.
02151      IF CERTEFFI = SPACES
02152          GO TO 1470-DATE-ERROR.
02153
02154      MOVE CERTEFFI               TO DEEDIT-DATE-INPUT.
02155      PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT.
02156      IF DEEDIT-DATE = ZERO
02157          GO TO 1470-DATE-ERROR.
02158
02159      MOVE DEEDIT-DATE            TO DC-GREG-DATE-1-MDY.
02160      MOVE '4'                    TO DC-OPTION-CODE.
02161      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
02162      IF NOT DATE-CONVERSION-ERROR
02163          MOVE DC-BIN-DATE-1      TO HOLD-EFF
02164          MOVE AL-UANON           TO CERTEFFA
02165          MOVE 'X'                TO UPDATE-SWITCH
02166          MOVE 'Y'                TO CERT-KEY-SWITCH
02167          MOVE DC-GREG-DATE-1-EDIT TO CERTEFFO
02168          GO TO 1470-EXIT.
02169
02170  1470-DATE-ERROR.
02171      MOVE ER-0215                TO EMI-ERROR.
02172      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02173      MOVE AL-UABON               TO CERTEFFA.
02174      MOVE -1                     TO CERTEFFL.
02175      MOVE 'X'                    TO ERROR-SWITCH.
02176
02177  1470-EXIT.
02178      EXIT.
02179
02180      EJECT
02181  1480-EDIT-ACCT.
02182      IF CERTACTI = SPACES
02183          MOVE ER-0232            TO EMI-ERROR
02184          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02185          MOVE AL-UABON           TO CERTACTA
02186          MOVE -1                 TO CERTACTL
02187          MOVE 'X'                TO ERROR-SWITCH
02188      ELSE
02189          MOVE 'X'                TO UPDATE-SWITCH
02190          MOVE 'Y'                TO CERT-KEY-SWITCH.
02191
02192  1480-EXIT.
02193      EXIT.
02194
02195  1490-EDIT-STATE.
02196      IF CERTSTI = SPACES
02197          MOVE ER-0233            TO EMI-ERROR
02198          GO TO 1490-STATE-ERROR.
02199
02200      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
02201      MOVE '3'                    TO CNTL-REC-TYPE.
02202      MOVE SPACES                 TO CNTL-STATE-ACCESS.
02203      MOVE CERTSTI                TO CNTL-STATE-NUMBER.
02204      MOVE ZERO                   TO CNTL-SEQ-NO.
02205
02206      
      * EXEC CICS HANDLE CONDITION
02207 *         NOTFND (1490-STATE-NOTFND)
02208 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00009736' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303039373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02209
02210      
      * EXEC CICS READ
02211 *         SET        (ADDRESS OF CONTROL-FILE)
02212 *         DATASET    ('ELCNTL')
02213 *         RIDFLD     (CNTL-KEY)
02214 *         KEYLENGTH  (CNTL-GENERIC-LENGTH)
02215 *         GENERIC
02216 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S  KG    E          (   #00009740' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 CNTL-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02217
02218      MOVE AL-UANON               TO CERTSTA.
02219      MOVE 'X'                    TO UPDATE-SWITCH.
02220      MOVE 'Y'                    TO CERT-KEY-SWITCH.
02221      GO TO 1490-EXIT.
02222
02223  1490-STATE-NOTFND.
02224      MOVE ER-0149                TO EMI-ERROR.
02225
02226  1490-STATE-ERROR.
02227      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02228      MOVE AL-UABON               TO CERTSTA.
02229      MOVE -1                     TO CERTSTL.
02230      MOVE 'X'                    TO ERROR-SWITCH.
02231
02232  1490-EXIT.
02233      EXIT.
02234      EJECT
02235  1500-EDIT-CARR.
02236      IF CERTCARI = SPACES
02237          GO TO 1500-CARR-NOTFND.
02238
02239      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
02240      MOVE '6'                    TO CNTL-REC-TYPE.
02241      MOVE SPACES                 TO CNTL-CARRIER-ACCESS.
02242      MOVE CERTCARI               TO CNTL-CARRIER.
02243      MOVE ZERO                   TO CNTL-SEQ-NO.
02244
02245      
      * EXEC CICS HANDLE CONDITION
02246 *         NOTFND   (1500-CARR-NOTFND)
02247 *    END-EXEC.
      *    MOVE '"$I                   ! * #00009775' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303039373735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02248
02249      
      * EXEC CICS READ
02250 *         SET        (ADDRESS OF CONTROL-FILE)
02251 *         DATASET    ('ELCNTL')
02252 *         RIDFLD     (CNTL-KEY)
02253 *         KEYLENGTH  (CNTL-GENERIC-LENGTH)
02254 *         GENERIC
02255 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S  KG    E          (   #00009779' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 CNTL-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02256
02257      MOVE AL-UANON               TO CERTCARA.
02258      MOVE 'X'                    TO UPDATE-SWITCH.
02259      MOVE 'Y'                    TO CERT-KEY-SWITCH.
02260      GO TO 1500-EXIT.
02261
02262  1500-CARR-NOTFND.
02263      MOVE ER-0360                TO EMI-ERROR.
02264      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02265      MOVE AL-UABON               TO CERTCARA.
02266      MOVE -1                     TO CERTCARL.
02267      MOVE 'X'                    TO ERROR-SWITCH.
02268
02269  1500-EXIT.
02270      EXIT.
02271      EJECT
02272  1510-DEEDIT-DATE.
02273      
      * EXEC CICS BIF DEEDIT
02274 *         FIELD    (DEEDIT-DATE-INPUT)
02275 *         LENGTH   (DATE-LENGTH)
02276 *    END-EXEC.
      *    MOVE '@"L                   #   #00009803' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02277
02278  1510-EXIT.
02279      EXIT.
02280
02281      EJECT
02282  1620-VERIFY-ACCT.
02283
           move ' '                    to ws-eracct-sw
02284      MOVE PI-COMPANY-CD          TO ACCT-COMPANY-CODE.
02285      MOVE PI-CARRIER             TO ACCT-CARRIER.
02286      MOVE PI-GROUPING            TO ACCT-GROUP.
02287      MOVE PI-STATE               TO ACCT-STATE.
02288      MOVE PI-ACCOUNT             TO ACCT-ACCOUNT.
02289
02290      IF CERTEFFI GREATER THAN LOW-VALUES
02291          MOVE HOLD-EFF           TO ACCT-DATE
02292      ELSE
02293          MOVE PI-CERT-EFF-DT     TO HOLD-EFF ACCT-DATE.
02294
02295      IF CERTACTI GREATER THAN LOW-VALUES
02296          MOVE CERTACTI           TO  ACCT-ACCOUNT.
02297      IF CERTSTI GREATER THAN LOW-VALUES
02298          MOVE CERTSTI            TO  ACCT-STATE.
02299      IF CERTCARI GREATER THAN LOW-VALUES
02300          MOVE CERTCARI           TO  ACCT-CARRIER.
02301      IF CERTGRPI GREATER THAN LOW-VALUES
02302          MOVE CERTGRPI           TO ACCT-GROUP.
02303
02304      MOVE ACCT-PARTIAL-KEY       TO SAVE-ACCT-KEY.
02305
02306      
      * EXEC CICS HANDLE CONDITION
02307 *         NOTFND  (1620-ACCT-NOTFND)
02308 *    END-EXEC.
      *    MOVE '"$I                   ! + #00009837' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303039383337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02309
02310      
      * EXEC CICS STARTBR
02311 *         DATASET   (ACCT-FILE-ID)
02312 *         RIDFLD    (ACCT-KEY)
02313 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009841' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02314
02315  1620-ACCT-LOOP.
02316
02317      
      * EXEC CICS READNEXT
02318 *         DATASET  (ACCT-FILE-ID)
02319 *         RIDFLD   (ACCT-KEY)
02320 *         SET      (ADDRESS OF ACCOUNT-MASTER)
02321 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009848' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
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
           
02322
02323      IF SAVE-ACCT-KEY NOT = ACCT-PARTIAL-KEY
02324          GO TO 1620-ACCT-NOTFND.
02325
02326      IF HOLD-EFF NOT LESS AM-EFFECTIVE-DT  AND
02327         HOLD-EFF     LESS AM-EXPIRATION-DT
02328          NEXT SENTENCE
02329      ELSE
02330          GO TO 1620-ACCT-LOOP.
02331
           set acct-found to true
02332      IF PI-COMPANY-ID NOT = 'CRI' AND 'PEM' AND 'NCL'
02333          GO TO 1620-ACCT-END-BROWSE.
02334
02335      IF LOANBALL GREATER THAN +0
02336          IF AM-MAX-TOT-BEN NUMERIC  AND
02337             AM-MAX-TOT-BEN GREATER THAN ZERO
02338              IF HOLD-LOANBAL GREATER THAN AM-MAX-TOT-BEN
02339                  MOVE AL-UNBON     TO LOANBALA
02340                  MOVE -1           TO LOANBALL
02341                  MOVE 'X'          TO ERROR-SWITCH
02342                  MOVE ER-7641      TO EMI-ERROR
02343                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02344
02345      IF ACVBENEL GREATER THAN +0
02346          IF AM-MAX-MON-BEN NUMERIC  AND
02347             AM-MAX-MON-BEN GREATER THAN ZERO
02348              IF HOLD-AH-CV-BEN GREATER THAN AM-MAX-MON-BEN
02349                  MOVE AL-UNBON     TO ACVBENEA
02350                  MOVE -1           TO ACVBENEL
02351                  MOVE 'X'          TO ERROR-SWITCH
02352                  MOVE ER-7642      TO EMI-ERROR
02353                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02354
02355  1620-ACCT-END-BROWSE.
02356
02357      
      * EXEC CICS ENDBR
02358 *         DATASET (ACCT-FILE-ID)
02359 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009889' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02360
02361      GO TO 1620-EXIT.
02362
02363  1620-ACCT-NOTFND.
02364
02365      MOVE -1                     TO MAINTL.
02366      MOVE 'X'                    TO ERROR-SWITCH.
02367      MOVE ER-0426                TO EMI-ERROR.
02368      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02369
02370  1620-EXIT.
02371      EXIT.
02372
02373      EJECT
02374  2000-UPDATE-CLAIM.
02375
02376      MOVE SPACES                 TO ERROR-SWITCH.
02377      MOVE PI-COMPANY-CD          TO COMPANY-CODE.
02378      MOVE PI-CARRIER             TO CARRIER-CODE.
02379      MOVE PI-CLAIM-NO            TO CLAIM-NO.
02380      MOVE PI-CERT-NO             TO CERT-NO.
02381
02382      IF DELETE-CLAIM
02383          GO TO 3000-DELETE-CLAIM.
02384
02385      IF  NOT MODIFY-CAP
02386          MOVE ER-0070            TO EMI-ERROR
02387          MOVE 'X'                TO ERROR-SWITCH
02388          MOVE -1                 TO MAINTL
02389          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02390          GO TO 2000-EXIT.
02391
02392      MOVE MSTR-KEY               TO TRLR-MAIN-KEY ACTQ-KEY.
02393      MOVE ZERO                   TO TRLR-SEQ-NO.
02394
02395      
      * EXEC CICS READ UPDATE
02396 *         SET       (ADDRESS OF CLAIM-MASTER)
02397 *         DATASET   (CLMS-FILE-ID)
02398 *         RIDFLD    (MSTR-KEY)
02399 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00009927' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02400
080613     if (incl > zeros)
080613        and (cl-no-of-pmts-made > 0)
080613        and (hold-incur > low-values)
080613        and (hold-incur not = cl-incurred-dt)
080613        if hold-incur > cl-incurred-dt
080613           move cl-incurred-dt   to dc-bin-date-1
080613           move hold-incur       to dc-bin-date-2
080613        else
080613           move hold-incur       to dc-bin-date-1
080613           move cl-incurred-dt   to dc-bin-date-2
080613        end-if
080613        move '1'                 to dc-option-code
080613        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
040616        if dc-elapsed-days > +180
080613           MOVE ER-1663             TO EMI-ERROR
080613           PERFORM 9900-ERROR-FORMAT
080613                                 THRU 9900-EXIT
080613           MOVE -1                  TO MAINTL
080613           GO TO 8110-SEND-DATA
080613        end-if
080613     end-if
062602     IF (CL-PRIORITY-CD = '8')
062602        AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
052113             AND 'AMWA' AND 'KMSB')
062602        MOVE ER-8003             TO EMI-ERROR
062602        PERFORM 9900-ERROR-FORMAT
062602                                 THRU 9900-EXIT
062602        MOVE -1                  TO MAINTL
062602        GO TO 8110-SEND-DATA
052113     END-IF
062602
02401      IF PI-UPDATE-BY NOT = CL-LAST-MAINT-USER
02402          MOVE ER-0068            TO EMI-ERROR
02403          PERFORM 2010-RELEASE-REC THRU 2010-EXIT
02404          GO TO 2000-EXIT.
02405
02406      IF PI-UPDATE-HHMMSS NOT = CL-LAST-MAINT-HHMMSS
02407          MOVE ER-0068            TO EMI-ERROR
02408          PERFORM 2010-RELEASE-REC THRU 2010-EXIT
02409          GO TO 2000-EXIT.
02410
02411      IF CL-CLAIM-PAYMENT-STATUS NOT NUMERIC
02412          MOVE ZEROS              TO CL-CLAIM-PAYMENT-STATUS.
02413
02414      IF CL-PURGED-DT NOT = LOW-VALUES
02415          MOVE ER-7691            TO EMI-ERROR
02416          PERFORM 2010-RELEASE-REC THRU 2010-EXIT
02417          GO TO 2000-EXIT.
02418
02419      IF CERT-KEY-CHANGED
02420          PERFORM 2060-CHANGE-CERT THRU 2060-EXIT.
02421
02422      IF SCREEN-ERROR
02423          MOVE ER-0068            TO EMI-ERROR
02424          PERFORM 2010-RELEASE-REC THRU 2010-EXIT
02425          GO TO 2000-EXIT.
02426
02427      IF NINETY-TRLR-UPDATED
02428          PERFORM 2425-UPDATE-NINETY-TRLR THRU 2425-EXIT.
02429
02430      IF MSTR-KEY-CHANGED
02431          PERFORM 2030-KEY-UPDATE THRU 2030-EXIT
02432         ELSE
02433          PERFORM 2020-NORMAL-UPDATE THRU 2020-EXIT.
02434
02435      IF TRLR-UPDATE-REQUIRED
02436          PERFORM 2300-UPDATE-TRLR THRU 2300-EXIT.
02437
02438  2000-CHECK-CERT.
02439      IF NOT CERT-UPDATES
02440          GO TO 2000-EXIT.
02441
02442      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
02443      MOVE PI-CARRIER             TO CERT-CARRIER.
02444      MOVE PI-GROUPING            TO CERT-GROUP.
02445      MOVE PI-STATE               TO CERT-STATE.
02446      MOVE PI-ACCOUNT             TO CERT-ACCOUNT.
02447      MOVE PI-CERT-EFF-DT         TO CERT-DATE.
02448      MOVE PI-CERT-NO             TO CERT-CERT.
02449
02450      IF CERT-ALT-KEY-CHANGED
02451          PERFORM 2050-KEY-UPDATE THRU 2050-EXIT
02452      ELSE
02453          PERFORM 2040-NORMAL-UPDATE THRU 2040-EXIT.
02454
02455  2000-EXIT.
02456      EXIT.
02457      EJECT
02458  2010-RELEASE-REC.
02459      
      * EXEC CICS UNLOCK
02460 *         DATASET  (CLMS-FILE-ID)
02461 *    END-EXEC
      *    MOVE '&*                    #   #00010022' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02462
02463      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02464      MOVE 'X'                    TO ERROR-SWITCH.
02465
02466  2010-EXIT.
02467      EXIT.
02468
02469  2020-NORMAL-UPDATE.
02470      PERFORM 2600-CREATE-MAINT-NOTE THRU 2600-EXIT.
02471
02472      PERFORM 2100-MOVE-MSTR THRU 2100-EXIT.
02473
02474      IF WS-OPEN-CLOSE-SW = 'Y'
02475        PERFORM  7800-CHECK-AUTO-ACTIVITY THRU 7800-EXIT
02476        IF PI-REC-FOUND-SW = 'Y'
02477          IF CL-ACTIVITY-CODE = 09
02478            NEXT SENTENCE
02479          ELSE
02480            IF WS-RESET-SW = 'Y'
02481              IF CL-CLAIM-STATUS = 'C'
02482                MOVE 07               TO  CL-ACTIVITY-CODE
02483                MOVE SAVE-BIN-DATE    TO  CL-ACTIVITY-MAINT-DT
02484                MOVE 'CLOS'           TO  CL-ACTIVITY-MAINT-TYPE
02485              ELSE
02486                MOVE 08               TO  CL-ACTIVITY-CODE
02487                MOVE SAVE-BIN-DATE    TO  CL-ACTIVITY-MAINT-DT
02488                MOVE 'OPEN'           TO  CL-ACTIVITY-MAINT-TYPE.
02489
062121     IF PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02826
020816     if pi-company-id = 'DCC' OR 'VPP'
              perform 1620-VERIFY-ACCT THRU 1620-EXIT
052113        perform 2120-check-pdef  thru 2120-exit
052113     end-if
           if ((benperl <> zeros)
              and (benperi numeric))
                        or
                  (instypel <> zeros)
              perform 2140-update-elcrtt thru 2140-exit
           end-if
02490      
      * EXEC CICS HANDLE CONDITION
02491 *         DUPKEY   (2020-EXIT)
02492 *    END-EXEC.
      *    MOVE '"$$                   ! , #00010067' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303130303637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02493
02494      
      * EXEC CICS REWRITE
02495 *         DATASET   (CLMS-FILE-ID)
02496 *         FROM      (CLAIM-MASTER)
02497 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010071' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02498
02499  2020-EXIT.
02500      EXIT.
02501      EJECT
02502  2030-KEY-UPDATE.
02503      MOVE CLAIM-MASTER           TO JP-RECORD-AREA.
02504
02505      
      * EXEC CICS DELETE
02506 *        DATASET  (CLMS-FILE-ID)
02507 *    END-EXEC.
      *    MOVE '&(                    &   #00010082' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02508
02509      
      * EXEC CICS GETMAIN
02510 *         SET      (ADDRESS OF CLAIM-MASTER)
02511 *         LENGTH   (MSTR-LENGTH)
02512 *         INITIMG  (GETMAIN-SPACE)
02513 *    END-EXEC.
      *    MOVE ',"IL                  $   #00010086' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 MSTR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02514
02515      MOVE JP-RECORD-AREA         TO CLAIM-MASTER.
02516
02517      PERFORM 2600-CREATE-MAINT-NOTE THRU 2600-EXIT.
02518
02519      PERFORM 2100-MOVE-MSTR THRU 2100-EXIT.
02520
02521      IF WS-OPEN-CLOSE-SW = 'Y'
02522        PERFORM 7800-CHECK-AUTO-ACTIVITY THRU 7800-EXIT
02523          IF PI-REC-FOUND-SW = 'Y'
02524            IF CL-ACTIVITY-CODE = 09
02525              NEXT SENTENCE
02526            ELSE
02527              IF WS-RESET-SW = 'Y'
02528                IF CL-CLAIM-STATUS = 'C'
02529                  MOVE 07             TO  CL-ACTIVITY-CODE
02530                  MOVE SAVE-BIN-DATE  TO  CL-ACTIVITY-MAINT-DT
02531                  MOVE 'CLOS'         TO  CL-ACTIVITY-MAINT-TYPE
02532                ELSE
02533                  MOVE 08             TO  CL-ACTIVITY-CODE
02534                  MOVE SAVE-BIN-DATE  TO  CL-ACTIVITY-MAINT-DT
02535                  MOVE 'OPEN'         TO  CL-ACTIVITY-MAINT-TYPE.
02536
02537      
      * EXEC CICS HANDLE CONDITION
02538 *         DUPKEY    (2030-EXIT)
02539 *    END-EXEC.
      *    MOVE '"$$                   ! - #00010114' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303130313134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02540
02541 ** POPULATE THE CREDIT-CARD NO WITH THE CERT NO.
02542 *    MOVE CL-CERT-NO             TO CL-CCN-A5.
02826
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02543
02544      
      * EXEC CICS WRITE
02545 *         DATASET   (CLMS-FILE-ID)
02546 *         RIDFLD    (MSTR-KEY)
02547 *         FROM      (CLAIM-MASTER)
02548 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010125' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02549
02550  2030-EXIT.
02551      EXIT.
02552
02553      EJECT
02554  2040-NORMAL-UPDATE.
02555      
      * EXEC CICS READ UPDATE
02556 *         DATASET  (CERT-FILE-ID)
02557 *         RIDFLD   (CERT-KEY)
02558 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
02559 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00010136' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02560
02561      PERFORM 2200-MOVE-CERT THRU 2200-EXIT.
02562
02563      
      * EXEC CICS HANDLE CONDITION
02564 *         DUPKEY   (2040-EXIT)
02565 *    END-EXEC.
      *    MOVE '"$$                   ! . #00010144' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303130313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02566
02567      
      * EXEC CICS REWRITE
02568 *         DATASET  (CERT-FILE-ID)
02569 *         FROM     (CERTIFICATE-MASTER)
02570 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010148' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02571
02572  2040-EXIT.
02573      EXIT.
02574
02575  2050-KEY-UPDATE.
02576      
      * EXEC CICS READ UPDATE
02577 *         DATASET  (CERT-FILE-ID)
02578 *         RIDFLD   (CERT-KEY)
02579 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
02580 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00010157' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02581
02582      MOVE CERTIFICATE-MASTER     TO JP-RECORD-AREA.
02583
02584      
      * EXEC CICS DELETE
02585 *         DATASET   (CERT-FILE-ID)
02586 *    END-EXEC.
      *    MOVE '&(                    &   #00010165' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02587
02588      
      * EXEC CICS GETMAIN
02589 *         SET       (ADDRESS OF CERTIFICATE-MASTER)
02590 *         LENGTH    (CERT-LENGTH)
02591 *         INITIMG   (GETMAIN-SPACE)
02592 *    END-EXEC.
      *    MOVE ',"IL                  $   #00010169' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 CERT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02593
02594      MOVE JP-RECORD-AREA         TO CERTIFICATE-MASTER.
02595
02596      PERFORM 2200-MOVE-CERT THRU 2200-EXIT.
02597
02598      
      * EXEC CICS HANDLE CONDITION
02599 *         DUPKEY (2050-EXIT)
02600 *    END-EXEC.
      *    MOVE '"$$                   ! / #00010179' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303130313739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02601
02602      
      * EXEC CICS WRITE
02603 *         DATASET  (CERT-FILE-ID)
02604 *         RIDFLD   (CERT-KEY)
02605 *         FROM     (CERTIFICATE-MASTER)
02606 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010183' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02607
02608  2050-EXIT.
02609      EXIT.
02610      EJECT
02611  2060-CHANGE-CERT.
02612      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
02613      MOVE PI-CARRIER             TO CERT-CARRIER.
02614      MOVE PI-GROUPING            TO CERT-GROUP.
02615      MOVE PI-STATE               TO CERT-STATE.
02616      MOVE PI-ACCOUNT             TO CERT-ACCOUNT.
02617      MOVE PI-CERT-EFF-DT         TO CERT-DATE.
02618      MOVE PI-CERT-NO             TO CERT-CERT.
02619      IF CERTEFFI GREATER THAN LOW-VALUES
02620          MOVE HOLD-EFF           TO CERT-DATE.
02621      IF CERTACTI GREATER THAN LOW-VALUES
02622          MOVE CERTACTI           TO CERT-ACCOUNT.
02623      IF CERTSTI GREATER THAN LOW-VALUES
02624          MOVE CERTSTI            TO CERT-STATE.
02625      IF CERTCARI GREATER THAN LOW-VALUES
02626          MOVE CERTCARI           TO CERT-CARRIER.
02627      IF CERTGRPI GREATER THAN LOW-VALUES
02628          MOVE CERTGRPI           TO CERT-GROUP.
02629      IF CERTI GREATER THAN LOW-VALUES
02630          MOVE CERTI              TO CERT-CERT-PRIME.
02631      IF SUFXI GREATER THAN LOW-VALUES
02632          MOVE SUFXI              TO CERT-CERT-SUFX.
02633
02634      
      * EXEC CICS HANDLE CONDITION
02635 *         NOTFND   (2060-CERT-NOTFND)
02636 *    END-EXEC.
      *    MOVE '"$I                   ! 0 #00010215' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303130323135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02637
02638      
      * EXEC CICS READ UPDATE
02639 *         DATASET  (CERT-FILE-ID)
02640 *         RIDFLD   (CERT-KEY)
02641 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
02642 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00010219' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02643
02644      ADD 1                       TO CM-CLAIM-ATTACHED-COUNT.
02645
02646      IF CM-CLAIM-INTERFACE-SW = SPACES
02647         MOVE '1'   TO CM-CLAIM-INTERFACE-SW PI-CERT-SWITCH.
02648
02649      
      * EXEC CICS HANDLE CONDITION
02650 *         DUPKEY   (2060-DUPKEY)
02651 *    END-EXEC.
      *    MOVE '"$$                   ! 1 #00010230' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303130323330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02652
02653      
      * EXEC CICS REWRITE
02654 *         DATASET  (CERT-FILE-ID)
02655 *         FROM     (CERTIFICATE-MASTER)
02656 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010234' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02657
02658  2060-DUPKEY.
02659
02660      PERFORM 2070-CHECK-OLD THRU 2070-EXIT.
02661
02662  2060-CHANGE-TRLR-ACTQ.
02663      IF SCREEN-ERROR
02664          GO TO 2060-EXIT.
02665
02666      IF CERTI    GREATER LOW-VALUES OR
02667         SUFXI    GREATER LOW-VALUES OR
02668         CERTCARI GREATER LOW-VALUES
02669          PERFORM 2400-UPDATE-TRLR THRU 2400-EXIT
02670          PERFORM 2430-UPDATE-ACTQ THRU 2430-EXIT.
02671
02672      MOVE SPACES                 TO CERT-KEY-SWITCH
02673                                     CERT-SWITCH
02674                                     TRLR-SWITCH
02675                                     NINETY-TRLR-SWITCH.
02676      MOVE 'X' TO MSTR-KEY-SWITCH MSTR-SWITCH.
02677      GO TO 2060-EXIT.
02678
02679  2060-CERT-NOTFND.
02680      PERFORM 2070-CHECK-OLD THRU 2070-EXIT.
02681
02682      IF WS-REC-FOUND-SW = 'N'
02683          MOVE ER-0206            TO  EMI-ERROR
02684          MOVE -1                 TO  MAINTL
02685          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02686          
      * EXEC CICS UNLOCK
02687 *            DATASET   (CLMS-FILE-ID)
02688 *        END-EXEC
      *    MOVE '&*                    #   #00010267' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02689          GO TO 8110-SEND-DATA.
02690
02691      PERFORM 2080-CREATE-CERT THRU 2080-EXIT.
02692
02693      GO TO 2060-CHANGE-TRLR-ACTQ.
02694
02695  2060-EXIT.
02696      EXIT.
02697      EJECT
02698  2070-CHECK-OLD.
02699
02700      
      * EXEC CICS HANDLE CONDITION
02701 *        NOTFND   (2070-NOT-FOUND)
02702 *    END-EXEC.
      *    MOVE '"$I                   ! 2 #00010281' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303130323831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02703
02704      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
02705      MOVE PI-CARRIER             TO CERT-CARRIER.
02706      MOVE PI-GROUPING            TO CERT-GROUP.
02707      MOVE PI-STATE               TO CERT-STATE.
02708      MOVE PI-ACCOUNT             TO CERT-ACCOUNT.
02709      MOVE PI-CERT-EFF-DT         TO CERT-DATE.
02710      MOVE PI-CERT-NO             TO CERT-CERT.
02711
02712      
      * EXEC CICS READ UPDATE
02713 *         DATASET  (CERT-FILE-ID)
02714 *         RIDFLD   (CERT-KEY)
02715 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
02716 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00010293' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02717
02718      MOVE 'Y'                    TO WS-REC-FOUND-SW.
02719      MOVE CERTIFICATE-MASTER     TO SAVE-RECORD.
02720      SUBTRACT 1                  FROM CM-CLAIM-ATTACHED-COUNT.
02721
02722      IF CM-CLAIM-ATTACHED-COUNT LESS THAN ZERO
02723          MOVE ZERO               TO CM-CLAIM-ATTACHED-COUNT.
02724
02725      IF CM-CLAIM-ATTACHED-COUNT = ZERO
02726        AND
02727         CERT-WAS-CREATED-FOR-CLAIM
02728          MOVE CERTIFICATE-MASTER TO JP-RECORD-AREA
02729          
      * EXEC CICS DELETE
02730 *             DATASET   (CERT-FILE-ID)
02731 *        END-EXEC
      *    MOVE '&(                    &   #00010310' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02732          GO TO 2070-EXIT.
02733
02734      IF CM-CLAIM-ATTACHED-COUNT = ZERO
02735          MOVE SPACE TO CM-CLAIM-INTERFACE-SW PI-CERT-SWITCH.
02736
02737      
      * EXEC CICS HANDLE CONDITION
02738 *         DUPKEY   (2070-EXIT)
02739 *    END-EXEC.
      *    MOVE '"$$                   ! 3 #00010318' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303130333138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02740
02741      
      * EXEC CICS REWRITE
02742 *         DATASET  (CERT-FILE-ID)
02743 *         FROM     (CERTIFICATE-MASTER)
02744 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010322' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02745
02746      GO TO 2070-EXIT.
02747
02748  2070-NOT-FOUND.
02749      MOVE 'N'                        TO  WS-REC-FOUND-SW.
02750
02751  2070-EXIT.
02752      EXIT.
02753      EJECT
02754  2080-CREATE-CERT.
02755      
      * EXEC CICS GETMAIN
02756 *         SET       (ADDRESS OF CERTIFICATE-MASTER)
02757 *         LENGTH    (CERT-LENGTH)
02758 *         INITIMG   (GETMAIN-SPACE)
02759 *    END-EXEC.
      *    MOVE ',"IL                  $   #00010336' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 CERT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02760
02761      MOVE SAVE-RECORD            TO CERTIFICATE-MASTER.
02762
02763      IF CERTEFFI GREATER THAN LOW-VALUES
02764          MOVE HOLD-EFF           TO CERT-DATE CM-CERT-EFF-DT
02765                                     DC-BIN-DATE-1
02766          MOVE '6'                TO DC-OPTION-CODE
02767          MOVE +1                 TO DC-ELAPSED-MONTHS
02768          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
02769          IF NO-CONVERSION-ERROR
02770              MOVE DC-BIN-DATE-2  TO CM-LOAN-1ST-PMT-DT.
02771
02772      IF CERTACTI GREATER THAN LOW-VALUES
02773          MOVE CERTACTI           TO CERT-ACCOUNT CM-ACCOUNT.
02774      IF CERTSTI GREATER THAN LOW-VALUES
02775          MOVE CERTSTI            TO CERT-STATE CM-STATE.
02776      IF CERTCARI GREATER THAN LOW-VALUES
02777          MOVE CERTCARI           TO CERT-CARRIER CM-CARRIER.
02778      IF CERTGRPI GREATER THAN LOW-VALUES
02779          MOVE CERTGRPI           TO CERT-GROUP CM-GROUPING.
02780      IF CERTI GREATER THAN LOW-VALUES
02781          MOVE CERTI              TO CERT-CERT-PRIME
02782                                     CM-CERT-PRIME
02783          MOVE CM-CERT-NO         TO CM-CERT-NO-A4.
02784
02785      IF SUFXI GREATER THAN LOW-VALUES
02786          MOVE SUFXI              TO CERT-CERT-SUFX
02787                                     CM-CERT-SFX
02788          MOVE CM-CERT-NO         TO CM-CERT-NO-A4.
02789
02790      PERFORM 2200-MOVE-CERT THRU 2200-EXIT.
02791
02792      MOVE LOW-VALUES             TO CM-LAST-MONTH-END.
02793
02794      MOVE SPACES                 TO CM-ENTRY-BATCH
02795                                     CM-LF-EXIT-BATCH
02796                                     CM-AH-EXIT-BATCH
02797                                     CM-CREDIT-INTERFACE-SW-1
02798                                     CM-CREDIT-INTERFACE-SW-2.
02799
02800      IF CM-LF-CURRENT-STATUS = '2' OR '3' OR '4' OR '5' OR
02801                                '9' OR 'D' OR 'V'
02802          MOVE '1'                TO CM-LF-CURRENT-STATUS.
02803
02804      IF CM-AH-CURRENT-STATUS = '2' OR '3' OR '4' OR '5' OR
02805                                '9' OR 'D' OR 'V'
02806          MOVE '1'                TO CM-AH-CURRENT-STATUS.
02807
02808      MOVE '2'                    TO CM-CLAIM-INTERFACE-SW
02809                                     CL-CERT-ORIGIN PI-CERT-SWITCH.
02810      MOVE 1                      TO CM-CLAIM-ATTACHED-COUNT.
02811
02812      
      * EXEC CICS HANDLE CONDITION
02813 *         DUPKEY (2080-EXIT)
02814 *    END-EXEC.
      *    MOVE '"$$                   ! 4 #00010393' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303130333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02815
02816      
      * EXEC CICS WRITE
02817 *         DATASET  (CERT-FILE-ID)
02818 *         RIDFLD   (CERT-KEY)
02819 *         FROM     (CERTIFICATE-MASTER)
02820 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010397' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02821
02822  2080-EXIT.
02823      EXIT.
02824      EJECT
02825  2100-MOVE-MSTR.
02827      IF TYPEI GREATER THAN LOW-VALUES
02828          MOVE TYPEI              TO CL-CLAIM-TYPE.
02829
02830      IF STATUSI = LOW-VALUES
02831          GO TO 2100-NO-STATUS-CHANGE.
02832
02833      IF STATUSI = 'OPEN' OR 'O'
02834          IF CL-CLAIM-STATUS = 'O'
02835              MOVE SPACES         TO TRLR-SWITCH
02836                  GO TO 2100-NO-STATUS-CHANGE.
02837
02838      IF STATUSI = 'CLOSED' OR 'C'
02839          IF CL-CLAIM-STATUS = 'C'
02840              MOVE SPACES         TO TRLR-SWITCH
02841                  GO TO 2100-NO-STATUS-CHANGE.
02842
02843      IF PI-COMPANY-ID = 'DMD'
02844         IF STATUSI = 'OPEN' OR 'O'
02845            IF CL-LAST-CLOSE-REASON = 'C' OR 'E'
02846               MOVE ZERO       TO STATUSL
02847               MOVE LOW-VALUES TO STATUSI
02848               GO TO 2100-NO-STATUS-CHANGE.
02849
02850      IF PI-COMPANY-ID = 'DMD'
02851          PERFORM 8000-CREATE-DMO-REC THRU 8000-EXIT.
02852
02853      MOVE STATUSI                TO CL-CLAIM-STATUS.
02854
02855  2100-NO-STATUS-CHANGE.
02856
02857      IF CCNOI GREATER THAN LOW-VALUES
02858          MOVE CCNOI              TO CL-CCN.
02859
02860      IF PROCI GREATER THAN LOW-VALUES
02861          MOVE PROCI              TO CL-PROCESSOR-ID.
02862
052113     if (accswi > low-values)
052113        and (accswi = 'Y' OR 'N')
052113        move accswi              to cl-accident-claim-sw
052113     end-if
           if benperi > low-values
              and benperi numeric
              move benperi             to cl-benefit-period
           end-if
           if instypei > low-values
              move instypei            to cl-insured-type
           end-if
02863      IF SEXI GREATER THAN LOW-VALUES
02864          MOVE SEXI               TO CL-INSURED-SEX-CD.
02865
02866      IF BIRTHI GREATER THAN LOW-VALUES
02867          MOVE HOLD-BIRTH         TO CL-INSURED-BIRTH-DT.
02868
02869      IF SOCIALI GREATER THAN LOW-VALUES
02870        AND
02871         SOCIALI = SPACES
02872          MOVE CL-CERT-STATE      TO CL-SSN-STATE
02873          MOVE CL-CERT-ACCOUNT-PRIME   TO CL-SSN-ACCOUNT
02874          MOVE CL-INSURED-LAST-NAME   TO CL-SSN-LN3
02875      ELSE
02876          IF SOCIALI GREATER THAN LOW-VALUES
02877              MOVE SOCIALI        TO CL-SOC-SEC-NO.
02878
02879      IF OCCI GREATER THAN LOW-VALUES
02880          MOVE OCCI               TO CL-INSURED-OCC-CD.
02881
02882      IF BENEL GREATER THAN ZERO
02883          MOVE BENEI              TO CL-BENEFICIARY.
02884
02885      IF DIAGI GREATER THAN LOW-VALUES
02886          MOVE DIAGI              TO WS-DIAGNOSIS.
040814
040814     IF ICD1I GREATER THAN LOW-VALUES
040814         MOVE ICD1I              TO WS-ICD-CODE-1
040814     END-IF.
040814
040814     IF ICD2I GREATER THAN LOW-VALUES
040814         MOVE ICD2I              TO WS-ICD-CODE-2
040814     END-IF.
02887
02888      IF PREMTYPI GREATER THAN LOW-VALUES
02889          MOVE PREMTYPI           TO CL-CLAIM-PREM-TYPE.
02890
040814*    IF CAUSEI GREATER THAN LOW-VALUES
040814*        MOVE CAUSEI             TO CL-CAUSE-CD.
02893
040814*    IF ENDI GREATER THAN LOW-VALUES
040814*        MOVE HOLD-END           TO CL-EST-END-OF-DISAB-DT.
02896
02897      IF PDTHRUI GREATER THAN LOW-VALUES
02898         MOVE HOLD-PDTHRU         TO CL-PAID-THRU-DT.
02899
02900      IF PDAMTL GREATER THAN +0
02901         MOVE HOLD-PDAMT          TO CL-TOTAL-PAID-AMT.
02902
02903      IF NODAYSI GREATER THAN LOW-VALUES
02904         MOVE HOLD-NODAYS         TO CL-NO-OF-DAYS-PAID.
02905
02906      IF NOPMTSI GREATER THAN LOW-VALUES
02907         MOVE HOLD-NOPMTS         TO CL-NO-OF-PMTS-MADE.
02908
02909      IF FORMTYPI GREATER THAN LOW-VALUES
02910         MOVE FORMTYPI            TO CL-PROG-FORM-TYPE.
02911
02912      IF INCI GREATER THAN LOW-VALUES
02913         MOVE HOLD-INCUR          TO CL-INCURRED-DT.
02914
02915      IF REPI GREATER THAN LOW-VALUES
02916         MOVE HOLD-REPORTED       TO CL-REPORTED-DT.
02917
02918      IF ADDONDTI GREATER THAN LOW-VALUES
02919         MOVE HOLD-ADDON          TO CL-LAST-ADD-ON-DT.
02920
02921      IF PRICDI GREATER THAN LOW-VALUES
02922          MOVE PRICDI             TO CL-PRIORITY-CD.
02923
052113     IF CRITPL  > ZEROS
052113        MOVE CRITPI              TO CL-CRITICAL-PERIOD
052113     END-IF
052113
081817     IF EXTENSL  > ZEROS
081817        MOVE EXTENSI             TO CL-NO-OF-EXTENSIONS
081817     END-IF
052113*    IF CRITPTL > ZEROS
052113*       MOVE CRITPTI             TO CL-CRIT-PER-RECURRENT
052113*    END-IF
052113
052113*    IF RTWMOSL > ZEROS
052113*       MOVE RTWMOSI             TO CL-CRIT-PER-RTW-MOS
052113*    END-IF
02924      IF FILETOI GREATER THAN LOW-VALUES
02925          MOVE FILETOI            TO CL-FILE-LOCATION.
02926
02927      IF SUPVI GREATER THAN LOW-VALUES
02928          MOVE SUPVI              TO CL-SUPV-ATTN-CD.
02929
02930      IF MLNAMEI GREATER THAN LOW-VALUES
02931          MOVE MLNAMEI            TO CL-INSURED-LAST-NAME.
02932
02933      IF MFNAMEI GREATER THAN LOW-VALUES
02934          MOVE MFNAMEI            TO CL-INSURED-1ST-NAME.
02935
02936      IF MMINITI GREATER THAN LOW-VALUES
02937          MOVE MMINITI            TO CL-INSURED-MID-INIT.
02938
02939      IF CERTEFFI GREATER THAN LOW-VALUES
02940          MOVE HOLD-EFF           TO CL-CERT-EFF-DT.
02941
02942      IF CERTACTI GREATER THAN LOW-VALUES
02943          MOVE CERTACTI           TO CL-CERT-ACCOUNT.
02944
02945      IF CERTSTI GREATER THAN LOW-VALUES
02946          MOVE CERTSTI            TO CL-CERT-STATE.
02947
02948      IF CERTCARI GREATER THAN LOW-VALUES
02949          MOVE CERTCARI           TO CL-CERT-CARRIER CARRIER-CODE
02950                                     CL-CARRIER.
02951
02952      IF CERTGRPI GREATER THAN LOW-VALUES
02953          MOVE CERTGRPI           TO CL-CERT-GROUPING.
02954
02955      IF CERTI GREATER THAN LOW-VALUES
02956          MOVE CERTI              TO CERT-NO-PRIME.
02957
02958      IF SUFXI GREATER THAN LOW-VALUES
02959          MOVE SUFXI              TO CERT-NO-SUFX.
02960
02961      MOVE CERT-NO                TO CL-CERT-NO
02962                                     CL-CERT-NO-A4.
02963
02964      IF PI-COMPANY-ID = 'FLA'
02965           NEXT SENTENCE
02966      ELSE
02967          MOVE SAVE-BIN-DATE      TO CL-LAST-MAINT-DT
02968          MOVE PI-PROCESSOR-ID    TO CL-LAST-MAINT-USER
02969          MOVE EIBTIME            TO CL-LAST-MAINT-HHMMSS
02970          MOVE '3'                TO CL-LAST-MAINT-TYPE.
02971
02972      IF TRLR-UPDATE-REQUIRED
02973         MOVE CL-CONTROL-PRIMARY TO TRLR-KEY
02974         MOVE ZEROS              TO TRLR-SEQ-NO
02975         
      * EXEC CICS READ
02976 *            DATASET   (TRLR-FILE-ID)
02977 *            RIDFLD    (TRLR-KEY)
02978 *            SET       (ADDRESS OF ACTIVITY-TRAILERS)
02979 *       END-EXEC
      *    MOVE '&"S        E          (   #00010588' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02980         IF AT-TRAILER-TYPE = '1'
02981            MOVE +1 TO MISC-SUB
02982            PERFORM 2100-BUMP-OPEN-CLOSE-HIST UNTIL
02983            MISC-SUB GREATER THAN +6 OR
02984            AT-OPEN-CLOSE-TYPE (MISC-SUB) = SPACES
02985            IF MISC-SUB GREATER THAN +1
02986               SUBTRACT +1 FROM MISC-SUB
02987               IF AT-OPEN-CLOSE-TYPE (MISC-SUB) = STATUSI
02988                  GO TO 2100-BYPASS-LAST-UPDATE.
02989
02990      IF TRLR-UPDATE-REQUIRED AND
02991         CLAIM-IS-OPEN
02992          MOVE SAVE-BIN-DATE      TO CL-LAST-REOPEN-DT
02993      ELSE
02994          IF TRLR-UPDATE-REQUIRED AND
02995             CLAIM-IS-CLOSED
02996              MOVE SAVE-BIN-DATE  TO CL-LAST-CLOSE-DT
02997              MOVE '4'            TO CL-LAST-CLOSE-REASON.
02998
02999  2100-BYPASS-LAST-UPDATE.
03000      MOVE EMI-FORCABLE-CTR       TO CL-FORCEABLE-ERROR-CNT.
03001      MOVE EMI-FATAL-CTR          TO CL-FATAL-ERROR-CNT.
03002
03003      GO TO 2100-EXIT.
03004
03005  2100-BUMP-OPEN-CLOSE-HIST.
03006      ADD +1                      TO MISC-SUB.
03007
03008  2100-EXIT.
03009      EXIT.
03010      EJECT
052113 2120-check-pdef.
052113     move +0                     to s1
052113     MOVE ZEROS                  TO WS-MONTHS-BETWEEN
052113     move spaces                 to ws-dcc-error-line
052113     PERFORM 3997-GET-ERPDEF  THRU 3997-EXIT
052113     IF ERPDEF-FOUND
052113        perform 2130-check-trlr  thru 2130-exit
052113        move +1                  to s1
052113*       if cl-accident-claim-sw = ' '
052113*          move er-1655             to ws-error-no (s1)
052113*          add +1 to s1
052113*       end-if
052113        MOVE CL-CERT-EFF-DT      TO DC-BIN-DATE-1
052113        MOVE cl-incurred-dt      TO DC-BIN-DATE-2
              if (hold-incur > low-values)
                 and (incl > zeros)
                 and (hold-incur not = cl-incurred-dt)
                 move hold-incur       to dc-bin-date-2
              end-if
052113        MOVE '1'                 TO DC-OPTION-CODE
052113        MOVE +0                  TO DC-ELAPSED-MONTHS
052113                                    DC-ELAPSED-DAYS
052113        PERFORM 9800-CONVERT-DATE
052113                                 THRU 9800-EXIT
052113        IF NO-CONVERSION-ERROR
052113           MOVE DC-ELAPSED-MONTHS
052113                                 TO WS-MONTHS-BETWEEN
052113           IF DC-ELAPSED-DAYS > 1
052113              ADD 1 TO WS-MONTHS-BETWEEN
052113           END-IF
052113        ELSE
                 display ' dte conv error ' dc-error-code
052113           MOVE ZEROS            TO WS-MONTHS-BETWEEN
052113        END-IF
052113        IF (WS-EXCL-PERIOD = ZEROS)
052113           OR (WS-MONTHS-BETWEEN = ZEROS)
052113           CONTINUE
052113        ELSE
052113           IF WS-MONTHS-BETWEEN  <= WS-EXCL-PERIOD
052113              MOVE ER-1651       TO ws-error-no (s1)
052113              add +1             to s1
052113           END-IF
052113        END-IF
              display ' mos diff ' ws-months-between ' '
                 ws-pre-exsist
052113        IF (WS-pre-exsist = ZEROS)
052113           OR (WS-MONTHS-BETWEEN = ZEROS)
052113           CONTINUE
052113        ELSE
052113           IF WS-months-between <= WS-pre-exsist
052113              MOVE ER-1677       TO ws-error-no (s1)
052113              add +1             to s1
052113           END-IF
052113        END-IF
052113        IF (WS-COV-ENDS = ZEROS)
052113           OR (WS-MONTHS-BETWEEN = ZEROS)
052113           CONTINUE
052113        ELSE
052113           IF WS-MONTHS-BETWEEN > WS-COV-ENDS
052113              MOVE -1            TO MAINTL
052113              MOVE ER-1653       TO ws-error-no (s1)
052113              add +1 to s1
052113           END-IF
052113        END-IF
052113        IF (WS-ACC-PERIOD = ZEROS)
052113           OR (WS-MONTHS-BETWEEN = ZEROS)
052113           CONTINUE
052113        ELSE
052113           IF (WS-MONTHS-BETWEEN <= WS-ACC-PERIOD)
052113              and (accswi = spaces or low-values)
052113*             and (cl-accident-claim-sw = ' ')
052113              move er-1655       to ws-error-no (s1)
052113              add +1             to s1
052113           end-if
052113           IF WS-MONTHS-BETWEEN <= WS-ACC-PERIOD
052113*             if (cl-accident-claim-sw not = 'Y')
052113              if (accswi not = 'Y')
052113                 MOVE ER-1652    TO ws-error-no (s1)
052113              else
052113                 move er-1662    to ws-error-no (s1)
052113              end-if
052113              MOVE -1            TO MAINTL
052113              add +1             to s1
052113           END-IF
052113        END-IF
              if cl-claim-type  = 'I'
                 move er-1661          to ws-error-no (s1)
                 add +1                to s1
              end-if
052113        perform 2130-check-trlr  thru 2130-exit
052113     END-IF
052113
052113     .
052113 2120-exit.
052113     exit.
052113
052113 2130-check-trlr.
052113
052113     evaluate true
052113        when s1 = +0
052113           move cl-control-primary
052113                                 to trlr-key
052113           MOVE +95              TO TRLR-SEQ-NO
052113           
      * EXEC CICS READ
052113*             update
052113*             DATASET  ('ELTRLR')
052113*             SET      (ADDRESS OF ACTIVITY-TRAILERS)
052113*             RIDFLD   (TRLR-KEY)
052113*             RESP     (WS-RESPONSE)
052113*          END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00010727' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303130373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113           if ws-resp-normal
052113              move spaces        to at-info-line-1
052113           end-if
052113        when s1 = +1
052113           if ws-resp-normal
052113              
      * exec cics rewrite
052113*                dataset   ('ELTRLR')
052113*                from      (activity-trailers)
052113*             end-exec
           MOVE LENGTH OF
            activity-trailers
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00010739' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130373339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 activity-trailers, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113           end-if
052113        when s1 > +1
052113           if ws-resp-normal
052113              move ws-dcc-error-line
052113                                 to at-info-line-1
052113              
      * exec cics rewrite
052113*                dataset   ('ELTRLR')
052113*                from      (activity-trailers)
052113*             end-exec
           MOVE LENGTH OF
            activity-trailers
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00010748' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130373438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 activity-trailers, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113           else
052113              
      * EXEC CICS GETMAIN
052113*                SET      (ADDRESS OF ACTIVITY-TRAILERS)
052113*                LENGTH   (TRLR-LENGTH)
052113*                INITIMG  (GETMAIN-SPACE)
052113*             END-EXEC
      *    MOVE ',"IL                  $   #00010753' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130373533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113              move 'AT'          to at-record-id
052113              move cl-control-primary
052113                                 to at-control-primary
052113              move +95           to at-sequence-no
052113              move '6'           to at-trailer-type
052113              move ws-dcc-error-line
052113                                 to at-info-line-1
052113              move 'E'           to at-info-trailer-type
052113              move save-bin-date to at-recorded-dt
052113                                    at-gen-info-last-maint-dt
052113              move pi-processor-id
052113                                 to at-recorded-by
052113                                    at-gen-info-last-updated-by
052113              move eibtime       to at-last-maint-hhmmss
052113              
      * exec cics write
052113*                dataset   ('ELTRLR')
052113*                from      (activity-trailers)
052113*                ridfld    (trlr-key)
052113*             end-exec
           MOVE LENGTH OF
            activity-trailers
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00010772' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303130373732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 activity-trailers, 
                 DFHEIV11, 
                 trlr-key, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113              perform varying s1 from +1 by +1 until
052113                 ws-error-no (s1) = spaces
052113                 move ws-error-no (s1)
052113                                 to emi-error
052113                 PERFORM 9900-ERROR-FORMAT
052113                                 THRU 9900-EXIT
052113              end-perform
052113           end-if
052113     end-evaluate
052113
052113     .
052113 2130-exit.
052113     exit.
       2140-update-elcrtt.
           move ' '                    to crtt-switch
052113     MOVE CERT-KEY               TO ELCRTT-KEY
052113     MOVE 'B'                    TO CTRLR-REC-TYPE
052113     
      * EXEC CICS READ
052113*       UPDATE
052113*       DATASET   ('ELCRTT')
052113*       RIDFLD    (ELCRTT-KEY)
052113*       INTO      (CERTIFICATE-TRAILERS)
052113*       RESP      (WS-RESPONSE)
052113*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00010794' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303130373934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113     IF WS-RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cs-claim-no (s1) = cl-claim-no)
              end-perform
              if (s1 < +25)
                 if (benperl <> zeros)
                    and (cs-benefit-period (s1) not = benperi)
                    move benperi       to cs-benefit-period (s1)
                    set crtt-update    to true
                end-if
                if (instypel <> zeros)
                   and (instypei not = cs-insured-type (s1))
                   move instypei      to cs-insured-type (s1)
                   set crtt-update    to true
                end-if
                move cs-claim-type (s1) to ws-claim-type
                move cs-benefit-period (s1) to ws-ben-per
              end-if
              if crtt-update
                 perform 2150-accum    thru 2150-exit
052113           
      * EXEC CICS REWRITE
052113*             DATASET   ('ELCRTT')
052113*             FROM      (CERTIFICATE-TRAILERS)
052113*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&& L                  %   #00010822' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130383232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              else
                 
      * exec cics unlock
      *             dataset   ('ELCRTT')
      *          end-exec
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&*                    #   #00010827' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303130383237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              end-if
           end-if
           .
       2140-exit.
           exit.
       2150-accum.
           if not erpdef-found
              perform 3997-get-erpdef  thru 3997-exit
           end-if
           if (erpdef-found)
              and (ws-max-moben not = zeros)
              and (ws-max-moben < cm-ah-benefit-amt)
              continue
           else
              move cm-ah-benefit-amt   to ws-max-moben
           end-if
           move cm-ah-orig-term        to ws-max-bens
           if cl-critical-period not = zeros and spaces
              move cl-critical-period  to ws-max-bens
           end-if
           move zeros to ws-tot-days-paid ws-tot-amt-paid
           perform varying s2 from +1 by +1 until
              (s2 > +24)
              or (cs-claim-no (s2) = spaces)
              if (cs-benefit-period (s2) = ws-ben-per)
                 and (cs-claim-type (s2) = ws-claim-type)
                 and (cs-insured-type (s2) = cl-insured-type)
      *          compute ws-tot-days-paid =
      *             ws-tot-days-paid + cs-days-paid (s2)
                 compute ws-tot-amt-paid =
                    ws-tot-amt-paid + cs-total-paid (s2)
              end-if
           end-perform
           if s2 < +25
              compute ws-pd-bens rounded =
                 ws-tot-amt-paid / ws-max-moben
              compute cs-remaining-bens (s1) =
                 ws-max-bens - ws-pd-bens
              if cs-remaining-bens (s1) < zeros
                 move zeros            to cs-remaining-bens (s1)
              end-if
           end-if
           .
       2150-exit.
           exit.
03011  2200-MOVE-CERT.
03012      IF CRTLNMEI GREATER THAN LOW-VALUES
03013          MOVE CRTLNMEI           TO CM-INSURED-LAST-NAME.
03014
03015      IF CRTFNMEI GREATER THAN LOW-VALUES
03016          MOVE CRTFNMEI           TO CM-INSURED-FIRST-NAME
03017          MOVE CM-INSURED-1ST-INIT TO CM-INSURED-INITIAL1.
03018
03019      IF CRTINITI GREATER THAN LOW-VALUES
03020          MOVE CRTINITI           TO CM-INSURED-INITIAL2.
03021
03022      IF CM-INSURED-INITIALS = SPACES
03023          MOVE '**'               TO CM-INSURED-INITIALS.
03024
03025      IF ISSAGEI GREATER THAN LOW-VALUES
03026          MOVE ISSAGEI            TO CM-INSURED-ISSUE-AGE.
03027
03028      IF JNTFNMEI GREATER THAN LOW-VALUES
03029          MOVE JNTFNMEI           TO CM-JT-FIRST-NAME.
03030
03031      IF JNTLNMEI GREATER THAN LOW-VALUES
03032          MOVE JNTLNMEI           TO CM-JT-LAST-NAME.
03033
03034      IF JNTINITI GREATER THAN LOW-VALUES
03035          MOVE JNTINITI           TO CM-JT-INITIAL.
03036
03037      IF JNTAGEI GREATER THAN LOW-VALUES
03038          MOVE JNTAGEI            TO CM-INSURED-JOINT-AGE.
03039
03040      IF ADDONDTI GREATER THAN LOW-VALUES
03041         MOVE HOLD-ADDON          TO CM-LAST-ADD-ON-DT.
03042
03043      IF LCVCDI GREATER THAN LOW-VALUES
03044         IF LCVCDI = SPACES OR ZEROS
03045            MOVE ZEROS            TO CM-LF-BENEFIT-CD
03046         ELSE
03047            MOVE LCVCDI           TO CM-LF-BENEFIT-CD.
03048
03049      IF LCVOTRMI GREATER THAN LOW-VALUES
03050          MOVE LCVOTRMI           TO CM-LF-ORIG-TERM.
03051
03052      IF LCVRATEL GREATER THAN +0
03053          MOVE HOLD-LF-RATE       TO CM-LF-PREMIUM-RATE.
03054
03055      IF LCVBENEL GREATER THAN +0
03056          MOVE HOLD-LF-CV-BEN     TO CM-LF-BENEFIT-AMT.
03057
03058      IF LCVFORMI GREATER THAN LOW-VALUES
03059          MOVE LCVFORMI           TO CM-POLICY-FORM-NO.
03060
03061      IF LCVCNDTI = LOW-VALUES
03062         GO TO 2210-SET-STATUS.
03063
03064      IF HOLD-LF-CV-CAN NOT = LOW-VALUES
03065         MOVE '8'                 TO CM-LF-CURRENT-STATUS
03066         MOVE HOLD-LF-CV-CAN      TO CM-LF-CANCEL-DT
03067         GO TO 2210-SET-STATUS.
03068
03069      IF CM-LF-CURRENT-STATUS = '7'
03070         MOVE CM-LF-STATUS-AT-DEATH   TO CM-LF-CURRENT-STATUS
03071         MOVE SPACES              TO CM-LF-STATUS-AT-DEATH
03072         MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT
03073                                     CM-LF-DEATH-DT
03074         GO TO 2210-SET-STATUS.
03075
03076      IF CM-LF-CURRENT-STATUS = '8'
03077         MOVE '1'                 TO CM-LF-CURRENT-STATUS
03078         MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT
03079                                     CM-LF-CANCEL-DT.
03080
03081  2210-SET-STATUS.
03082
03083      IF CM-LF-CURRENT-STATUS = SPACES
03084          MOVE '1'                TO CM-LF-CURRENT-STATUS.
03085
03086      IF CM-LF-ORIG-TERM GREATER THAN ZERO
03087          MOVE '6'                TO DC-OPTION-CODE
03088          MOVE CM-LF-ORIG-TERM    TO DC-ELAPSED-MONTHS
03089          MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1
03090          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
03091          IF NO-CONVERSION-ERROR
03092              MOVE DC-BIN-DATE-2  TO CM-LF-LOAN-EXPIRE-DT
03093          ELSE
03094              MOVE LOW-VALUES     TO CM-LF-LOAN-EXPIRE-DT
03095      ELSE
03096          MOVE LOW-VALUES         TO CM-LF-LOAN-EXPIRE-DT.
03097
03098  2215-PROCESS-AH-SIDE.
03099
03100      IF ACVCDI GREATER THAN LOW-VALUES
03101         IF ACVCDI = SPACES OR ZEROS
03102            MOVE ZEROS            TO CM-AH-BENEFIT-CD
03103         ELSE
03104            MOVE ACVCDI           TO CM-AH-BENEFIT-CD.
03105
03106      IF ACVOTRMI GREATER THAN LOW-VALUES
03107          MOVE ACVOTRMI           TO CM-AH-ORIG-TERM.
03108
03109      IF ACVRATEL GREATER THAN +0
03110          MOVE HOLD-AH-RATE       TO CM-AH-PREMIUM-RATE.
03111
03112      IF ACVBENEL GREATER THAN +0
03113          MOVE HOLD-AH-CV-BEN     TO CM-AH-BENEFIT-AMT
03114                                     PI-PAYMENT-AMT.
03115
03116      IF ACVFORMI GREATER THAN LOW-VALUES
03117          MOVE ACVFORMI           TO CM-POLICY-FORM-NO.
03118
03119      IF ACVCNDTI = LOW-VALUES
03120         GO TO 2220-SET-STATUS.
03121
03122      IF HOLD-AH-CV-CAN NOT = LOW-VALUES
03123         MOVE '8'                 TO CM-AH-CURRENT-STATUS
03124         MOVE HOLD-AH-CV-CAN      TO CM-AH-CANCEL-DT
03125         GO TO 2220-SET-STATUS.
03126
03127      IF CM-AH-CURRENT-STATUS = '6'
03128         MOVE CM-AH-STATUS-AT-SETTLEMENT
03129                                  TO CM-AH-CURRENT-STATUS
03130         MOVE SPACES              TO CM-AH-STATUS-AT-SETTLEMENT
03131         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT
03132                                     CM-AH-SETTLEMENT-DT
03133         GO TO 2220-SET-STATUS.
03134
03135      IF CM-AH-CURRENT-STATUS = '8'
03136         MOVE '1'                 TO CM-AH-CURRENT-STATUS
03137         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT
03138                                     CM-AH-CANCEL-DT.
03139
03140  2220-SET-STATUS.
03141      IF CM-AH-CURRENT-STATUS = SPACES
03142          MOVE '1'                TO CM-AH-CURRENT-STATUS.
03143
03144      IF CM-AH-ORIG-TERM GREATER THAN ZERO
03145          MOVE '6'                TO DC-OPTION-CODE
03146          MOVE CM-AH-ORIG-TERM    TO DC-ELAPSED-MONTHS
03147          MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1
03148          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
03149          IF NO-CONVERSION-ERROR
03150              MOVE DC-BIN-DATE-2  TO CM-AH-LOAN-EXPIRE-DT
03151          ELSE
03152              MOVE LOW-VALUES     TO CM-AH-LOAN-EXPIRE-DT
03153      ELSE
03154          MOVE LOW-VALUES         TO CM-AH-LOAN-EXPIRE-DT.
03155
03156  2225-FINISH-CERT.
03157      IF APRL GREATER THAN +0
03158          MOVE HOLD-APR           TO CM-LOAN-APR.
03159
03160      IF PMTFREQI GREATER THAN LOW-VALUES
03161          MOVE PMTFREQI           TO CM-PAY-FREQUENCY.
03162
03163      IF INDGRPI GREATER THAN LOW-VALUES
03164          MOVE INDGRPI            TO CM-IND-GRP-TYPE.
03165
062217*    IF LOANNOI GREATER THAN LOW-VALUES
062217*       MOVE LOANNOI             TO CM-LOAN-NUMBER.
03168
03169      IF LOANBALL GREATER THAN +0
03170         MOVE HOLD-LOANBAL        TO CM-LOAN-BALANCE.
03171
03172      IF PREMTYPI GREATER THAN LOW-VALUES
03173          MOVE PREMTYPI           TO CM-PREMIUM-TYPE.
03174
03175      IF REINCDI GREATER THAN LOW-VALUES
03176          MOVE REINCDI            TO CM-SPECIAL-REIN-CODE
03177                                     WS-REIN-1
03178                                     WS-REIN-2
03179                                     WS-REIN-3
03180          MOVE WS-REIN-TABLE      TO CM-REIN-TABLE.
03181
03182      MOVE LOW-VALUES             TO CM-AH-PAID-THRU-DT.
03183
03184      MOVE ZEROS                  TO CM-LOAN-TERM
03185                                     CM-LIFE-COMM-PCT
03186                                     CM-AH-COMM-PCT.
03187
121802*    IF PI-COMPANY-ID = 'CRI' OR 'PEM' OR 'NCL'
121802*      IF CM-PREMIUM-TYPE NOT = '1'  AND
121802*         PI-NO-PMTS = ZEROS
121802*          IF LOANBALL GREATER THAN +0          OR
121802*             ACVBENEL GREATER THAN +0          OR
121802*             APRL     GREATER THAN +0          OR
121802*             LCVRATEL GREATER THAN +0          OR
121802*             ACVRATEL GREATER THAN +0
121802*               PERFORM 6000-CALCULATE-CERT-TERM THRU 6000-EXIT.
03197
03198  2200-EXIT.
03199      EXIT.
03200      EJECT
03201  2300-UPDATE-TRLR.
052113     MOVE ZERO                   TO TRLR-SEQ-NO.
03203      
      * EXEC CICS READ UPDATE
03204 *         DATASET  (TRLR-FILE-ID)
03205 *         RIDFLD   (TRLR-KEY)
03206 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
03207 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00011067' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131303637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03208
03209      MOVE ZERO                   TO COUNT-2.
03210
03211      PERFORM 2310-INS-STATUS THRU 2310-EXIT
03212          UNTIL UPDATE-MADE OR COUNT-2 GREATER THAN 6.
03213
03214      IF NOT UPDATE-MADE
03215          PERFORM 2320-SHIFT-STATUS THRU 2320-EXIT.
03216
03217      
      * EXEC CICS REWRITE
03218 *         DATASET  (TRLR-FILE-ID)
03219 *         FROM     (ACTIVITY-TRAILERS)
03220 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011081' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131303831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03221
03222  2300-EXIT.
03223      EXIT.
03224
03225  2310-INS-STATUS.
03226      ADD 1                       TO COUNT-2.
03227      IF AT-OPEN-CLOSE-TYPE (COUNT-2) = SPACE
03228          PERFORM 2330-ADD-UPDATE THRU 2330-EXIT
03229          MOVE 'Y'                TO TRLR-SWITCH.
03230
03231  2310-EXIT.
03232      EXIT.
03233
03234  2320-SHIFT-STATUS.
03235      IF AT-OPEN-CLOSE-TYPE (6) = STATUSI
03236         MOVE 6                   TO COUNT-2
03237         GO TO 2320-EXIT.
03238
03239      MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1).
03240      MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2).
03241      MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3).
03242      MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4).
03243      MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5).
03244      MOVE 6       TO COUNT-2.
03245      PERFORM 2330-ADD-UPDATE THRU 2330-EXIT.
03246
03247  2320-EXIT.
03248      EXIT.
03249
03250  2330-ADD-UPDATE.
03251
03252      IF COUNT-2 GREATER THAN 1
03253         COMPUTE MISC-SUB = (COUNT-2 - 1)
03254         IF AT-OPEN-CLOSE-TYPE (MISC-SUB) = STATUSI
03255            GO TO 2330-EXIT.
03256
03257      MOVE SAVE-BIN-DATE         TO AT-OPEN-CLOSE-DATE (COUNT-2)
03258                                    AT-RESERVES-LAST-MAINT-DT
03259
03260      MOVE PI-PROCESSOR-ID       TO AT-RESERVES-LAST-UPDATED-BY
03261      MOVE EIBTIME               TO AT-LAST-MAINT-HHMMSS
03262      MOVE STATUSI               TO AT-OPEN-CLOSE-TYPE (COUNT-2)
03263      MOVE 'ALTER'               TO AT-OPEN-CLOSE-REASON (COUNT-2).
03264
03265  2330-EXIT.
03266      EXIT.
03267
03268      EJECT
03269  2400-UPDATE-TRLR.
03270      
      * EXEC CICS HANDLE CONDITION
03271 *         NOTFND    (2400-TRLR-NOTFND)
03272 *         ENDFILE   (2410-EXIT)
03273 *    END-EXEC.
      *    MOVE '"$I''                  ! 5 #00011134' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303131313334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03274
03275      
      * EXEC CICS STARTBR
03276 *        DATASET (TRLR-FILE-ID)
03277 *        RIDFLD  (TRLR-KEY)
03278 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011139' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03279
03280      
      * EXEC CICS GETMAIN
03281 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
03282 *         LENGTH    (TRLR-LENGTH)
03283 *         INITIMG   (GETMAIN-SPACE)
03284 *    END-EXEC.
      *    MOVE ',"IL                  $   #00011144' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03285
03286      PERFORM 2410-CHANGE-TRLR THRU 2410-EXIT.
03287
03288      
      * EXEC CICS ENDBR
03289 *        DATASET (TRLR-FILE-ID)
03290 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011152' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03291
03292      GO TO 2400-EXIT.
03293
03294  2400-TRLR-NOTFND.
03295      MOVE ER-0205                TO EMI-ERROR.
03296      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03297      MOVE 'X'                    TO ERROR-SWITCH.
03298
03299  2400-EXIT.
03300      EXIT.
03301
03302      EJECT
03303  2410-CHANGE-TRLR.
03304      
      * EXEC CICS READNEXT
03305 *        DATASET (TRLR-FILE-ID)
03306 *        RIDFLD  (TRLR-KEY)
03307 *        INTO    (ACTIVITY-TRAILERS)
03308 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00011168' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV12, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03309
03310      IF AT-CONTROL-PRIMARY = WS-LAST-TRLR-KEY
03311         GO TO 2410-CHANGE-TRLR.
03312
03313      MOVE AT-CONTROL-PRIMARY     TO WS-LAST-TRLR-KEY
03314                                     CHECK-KEY.
03315
03316      IF CHECK-KEY NOT = MSTR-KEY
03317          GO TO 2410-EXIT.
03318
03319      
      * EXEC CICS ENDBR
03320 *        DATASET (TRLR-FILE-ID)
03321 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011183' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03322
03323      
      * EXEC CICS READ UPDATE
03324 *        DATASET (TRLR-FILE-ID)
03325 *        RIDFLD  (TRLR-KEY)
03326 *        INTO    (ACTIVITY-TRAILERS)
03327 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00011187' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03328
03329      
      * EXEC CICS DELETE
03330 *        DATASET (TRLR-FILE-ID)
03331 *    END-EXEC.
      *    MOVE '&(                    &   #00011193' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131313933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03332
03333      IF AT-SEQUENCE-NO = ZERO AND
03334         TRLR-UPDATE-REQUIRED
03335          PERFORM 2420-UPDATE-TRLR THRU 2420-EXIT.
03336
03337      IF CERTI GREATER THAN LOW-VALUES
03338         MOVE CERTI               TO AT-CERT-PRIME.
03339
03340      IF SUFXI GREATER THAN LOW-VALUES
03341         MOVE SUFXI               TO AT-CERT-SFX.
03342
03343      IF CERTCARI GREATER THAN LOW-VALUES
03344         MOVE CERTCARI            TO AT-CARRIER.
03345
03346      IF PAYMENT-TR
03347         IF AT-CHECK-QUE-CONTROL NOT = ZEROS
03348            AND AT-CHECK-QUE-CONTROL NOT = 99999999
03349               PERFORM 2460-UPDATE-CHECKQ THRU 2460-EXIT.
03350
03351      IF CORRESPONDENCE-TR
03352         AND AT-LETTER-ARCHIVE-NO NOT = ZEROS
03353            PERFORM 2475-UPDATE-LETTER THRU 2475-EXIT.
03354
03355      
      * EXEC CICS WRITE
03356 *         DATASET  (TRLR-FILE-ID)
03357 *         RIDFLD   (AT-CONTROL-PRIMARY)
03358 *         FROM     (ACTIVITY-TRAILERS)
03359 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011219' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03360
03361      
      * EXEC CICS STARTBR
03362 *         DATASET  (TRLR-FILE-ID)
03363 *         RIDFLD   (TRLR-KEY)
03364 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011225' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03365
03366      GO TO 2410-CHANGE-TRLR.
03367
03368  2410-EXIT.
03369      EXIT.
03370
03371  2420-UPDATE-TRLR.
03372      MOVE ZERO TO COUNT-2.
03373      PERFORM 2310-INS-STATUS THRU 2310-EXIT
03374          UNTIL UPDATE-MADE OR COUNT-2 GREATER THAN 6.
03375
03376      IF NOT UPDATE-MADE
03377          PERFORM 2320-SHIFT-STATUS THRU 2320-EXIT.
03378
03379  2420-EXIT.
03380      EXIT.
03381
03382      EJECT
03383  2425-UPDATE-NINETY-TRLR.
03384
03385      MOVE +90                    TO TRLR-SEQ-NO.
03386
03387      
      * EXEC CICS HANDLE CONDITION
03388 *         NOTFND    (2425-NINETY-NOTFND)
03389 *         ENDFILE   (2425-NINETY-NOTFND)
03390 *    END-EXEC.
      *    MOVE '"$I''                  ! 6 #00011251' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303131323531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03391
03392      
      * EXEC CICS READ UPDATE
03393 *        DATASET (TRLR-FILE-ID)
03394 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
03395 *        RIDFLD  (TRLR-KEY)
03396 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00011256' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
031715     if (diagi > low-values)
031715        and (diagi not = at-info-line-1)
031715        and (cl-total-paid-amt > zeros)
031715        move cl-cert-eff-dt      to dc-bin-date-1
031715        move cl-paid-thru-dt     to dc-bin-date-2
031715        move '1'                 to dc-option-code
031715        perform 9800-convert-date
031715                                 thru 9800-exit
031715        if no-conversion-error
031715           and dc-elapsed-months < +24
031715           and (not emi-bypass-forcables)
031715           move er-1581          to emi-error
031715           perform 9900-error-format
031715                                thru 9900-exit
031715           move -1              to diagl
031715           go to 8110-send-data
031715        end-if
031715     end-if
03398      MOVE AT-INFO-LINE-1        TO WS-DIAGNOSIS.
040814     MOVE AT-ICD-CODE-1         TO WS-ICD-CODE-1.
040814     MOVE AT-ICD-CODE-2         TO WS-ICD-CODE-2.
03399
040814     IF DIAGI GREATER THAN LOW-VALUES
040814         MOVE DIAGI             TO AT-INFO-LINE-1
040814     END-IF.
040814
040814     IF ICD1I GREATER THAN LOW-VALUES
040814         MOVE ICD1I             TO AT-ICD-CODE-1
040814     END-IF.
040814
040814     IF ICD2I GREATER THAN LOW-VALUES
040814         MOVE ICD2I             TO AT-ICD-CODE-2
040814     END-IF.
03401
03402      
      * EXEC CICS REWRITE
03403 *        DATASET (TRLR-FILE-ID)
03404 *        FROM    (ACTIVITY-TRAILERS)
03405 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011295' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03406
040814     IF DIAGI GREATER THAN LOW-VALUES
040814       AND DIAGI NOT = WS-DIAGNOSIS
03408          MOVE 'DIAGNOSIS'          TO SPLIT-INFO-DESC
03409          MOVE WS-DIAGNOSIS         TO SPLIT-INFO-OLD
03410          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
040814
040814     IF ICD1I GREATER THAN LOW-VALUES
040814       AND ICD1I NOT = WS-ICD-CODE-1
040814         MOVE 'ICD CODE 1'         TO SPLIT-INFO-DESC
040814         MOVE WS-ICD-CODE-1        TO SPLIT-INFO-OLD
040814         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT
040814     END-IF.
040814
040814     IF ICD2I GREATER THAN LOW-VALUES
040814       AND ICD2I NOT = WS-ICD-CODE-2
040814         MOVE 'ICD CODE 2'         TO SPLIT-INFO-DESC
040814         MOVE WS-ICD-CODE-2        TO SPLIT-INFO-OLD
040814         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT
040814     END-IF.
03411
03412      GO TO 2425-EXIT.
03413
03414  2425-NINETY-NOTFND.
03415
03416      
      * EXEC CICS GETMAIN
03417 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
03418 *         LENGTH    (TRLR-LENGTH)
03419 *         INITIMG   (GETMAIN-SPACE)
03420 *    END-EXEC.
      *    MOVE ',"IL                  $   #00011324' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03421
03422      MOVE +90                  TO TRLR-SEQ-NO.
03423      MOVE TRLR-KEY             TO AT-CONTROL-PRIMARY.
03424
03425      MOVE 'AT'                 TO AT-RECORD-ID
03426      MOVE '6'                  TO AT-TRAILER-TYPE
03427      MOVE SAVE-BIN-DATE        TO AT-RECORDED-DT
03428                                   AT-GEN-INFO-LAST-MAINT-DT
03429      MOVE PI-PROCESSOR-ID      TO AT-RECORDED-BY
03430                                   AT-GEN-INFO-LAST-UPDATED-BY
03431      MOVE EIBTIME              TO AT-LAST-MAINT-HHMMSS
03432      MOVE DIAGI                TO AT-INFO-LINE-1.
040814     MOVE ICD1I                TO AT-ICD-CODE-1.
040814     MOVE ICD2I                TO AT-ICD-CODE-2.
03433
03434      
      * EXEC CICS WRITE
03435 *        DATASET (TRLR-FILE-ID)
03436 *        FROM    (ACTIVITY-TRAILERS)
03437 *        RIDFLD  (TRLR-KEY)
03438 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011344' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03439
03440  2425-EXIT.
03441      EXIT.
03442
03443      EJECT
03444  2430-UPDATE-ACTQ.
03445      
      * EXEC CICS HANDLE CONDITION
03446 *         NOTFND    (2430-EXIT)
03447 *         ENDFILE   (2440-EXIT)
03448 *    END-EXEC.
      *    MOVE '"$I''                  ! 7 #00011355' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303131333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03449
03450      
      * EXEC CICS STARTBR
03451 *         DATASET  (ACTQ-FILE-ID)
03452 *         RIDFLD   (ACTQ-KEY)
03453 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011360' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03454
03455      
      * EXEC CICS GETMAIN
03456 *         SET      (ADDRESS OF ACTIVITY-QUE)
03457 *         LENGTH   (ACTQ-LENGTH)
03458 *         INITIMG  (GETMAIN-SPACE)
03459 *    END-EXEC.
      *    MOVE ',"IL                  $   #00011365' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACTQ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03460
03461      PERFORM 2440-CHANGE-ACTQ THRU 2440-EXIT.
03462
03463  2430-EXIT.
03464      EXIT.
03465
03466  2440-CHANGE-ACTQ.
03467
03468      
      * EXEC CICS READNEXT
03469 *         DATASET  (ACTQ-FILE-ID)
03470 *         RIDFLD   (ACTQ-KEY)
03471 *         INTO     (ACTIVITY-QUE)
03472 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00011378' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV12, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03473
03474      
      * EXEC CICS ENDBR
03475 *         DATASET (ACTQ-FILE-ID)
03476 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011384' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03477
03478      IF ACTQ-KEY NOT = MSTR-KEY
03479          GO TO 2440-EXIT.
03480
03481      
      * EXEC CICS READ UPDATE
03482 *         DATASET  (ACTQ-FILE-ID)
03483 *         RIDFLD   (AQ-CONTROL-PRIMARY)
03484 *         SET      (ADDRESS OF ACTIVITY-QUE)
03485 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00011391' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03486
03487      MOVE ACTIVITY-QUE           TO JP-RECORD-AREA
03488
03489      
      * EXEC CICS DELETE
03490 *         DATASET (ACTQ-FILE-ID)
03491 *    END-EXEC.
      *    MOVE '&(                    &   #00011399' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03492
03493      MOVE JP-RECORD-AREA         TO ACTIVITY-QUE.
03494      IF CERTI GREATER THAN LOW-VALUES
03495         MOVE CERTI               TO AQ-CERT-PRIME.
03496      IF SUFXI GREATER THAN LOW-VALUES
03497         MOVE SUFXI               TO AQ-CERT-SFX.
03498      IF CERTCARI GREATER THAN LOW-VALUES
03499         MOVE CERTCARI            TO AQ-CARRIER.
03500
03501      
      * EXEC CICS HANDLE CONDITION
03502 *         DUPREC   (2440-EXIT)
03503 *    END-EXEC.
      *    MOVE '"$%                   ! 8 #00011411' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303131343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03504
03505      
      * EXEC CICS WRITE
03506 *         DATASET  (ACTQ-FILE-ID)
03507 *         RIDFLD   (AQ-CONTROL-PRIMARY)
03508 *         FROM     (ACTIVITY-QUE)
03509 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011415' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03510
03511  2440-EXIT.
03512      EXIT.
03513
03514      EJECT
03515  2460-UPDATE-CHECKQ.
03516      
      * EXEC CICS HANDLE CONDITION
03517 *         NOTFND   (2460-EXIT)
03518 *    END-EXEC.
      *    MOVE '"$I                   ! 9 #00011426' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303131343236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03519
03520      MOVE AT-CHECK-QUE-CONTROL   TO CHKQ-CONTROL-NO.
03521      MOVE AT-CHECK-QUE-SEQUENCE  TO CHKQ-SEQ-NO.
03522      MOVE PI-COMPANY-CD          TO CHKQ-COMPANY-CODE.
03523
03524      
      * EXEC CICS READ UPDATE
03525 *         DATASET   (CHKQ-FILE-ID)
03526 *         RIDFLD    (CHKQ-KEY)
03527 *         SET       (ADDRESS OF CHECK-QUE)
03528 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00011434' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CHKQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CHKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03529
03530      IF CERTI GREATER THAN LOW-VALUES
03531         MOVE CERTI               TO CQ-CERT-PRIME.
03532      IF SUFXI GREATER THAN LOW-VALUES
03533         MOVE SUFXI               TO CQ-CERT-SFX.
03534      IF CERTCARI GREATER THAN LOW-VALUES
03535         MOVE CERTCARI            TO CQ-CARRIER.
03536
03537      
      * EXEC CICS REWRITE
03538 *         DATASET  (CHKQ-FILE-ID)
03539 *         FROM     (CHECK-QUE)
03540 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011447' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CHKQ-FILE-ID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03541
03542  2460-EXIT.
03543      EXIT.
03544
03545      EJECT
03546  2475-UPDATE-LETTER.
03547      
      * EXEC CICS HANDLE CONDITION
03548 *         NOTFND   (2475-EXIT)
03549 *    END-EXEC.
      *    MOVE '"$I                   ! : #00011457' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303131343537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03550
03551      MOVE AT-LETTER-ARCHIVE-NO   TO ARCH-ARCHIVE-NO.
03552      MOVE '1'                    TO ARCH-RECORD-TYPE.
03553      MOVE ZEROS                  TO ARCH-SEQ-NO.
03554      MOVE PI-COMPANY-CD          TO ARCH-COMPANY-CODE.
03555
03556      
      * EXEC CICS READ UPDATE
03557 *         DATASET   (ARCH-FILE-ID)
03558 *         RIDFLD    (ARCH-KEY)
03559 *         SET       (ADDRESS OF LETTER-ARCHIVE)
03560 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00011466' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03561
03562 * COMPARE LETTER HEADER INFORMATION TO ASSURE THAT IT BELONGS
03563 * TO THIS MASTER-KEY INFORMATION.
03564
03565      IF LA-CARRIER  = CARRIER-CODE AND
03566         LA-CLAIM-NO = CLAIM-NO     AND
03567         LA-CERT-NO  = CERT-NO
03568         NEXT SENTENCE
03569      ELSE
03570         GO TO 2475-UNLOCK.
03571
03572      IF CERTI GREATER THAN LOW-VALUES
03573         MOVE CERTI               TO LA-CERT-PRIME.
03574      IF SUFXI GREATER THAN LOW-VALUES
03575         MOVE SUFXI               TO LA-CERT-SFX.
03576      IF CERTCARI GREATER THAN LOW-VALUES
03577         MOVE CERTCARI            TO LA-CARRIER.
03578
03579      
      * EXEC CICS REWRITE
03580 *         DATASET  (ARCH-FILE-ID)
03581 *         FROM     (LETTER-ARCHIVE)
03582 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011489' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03583
03584      GO TO 2475-EXIT.
03585
03586  2475-UNLOCK.
03587      
      * EXEC CICS UNLOCK
03588 *         DATASET  (ARCH-FILE-ID)
03589 *    END-EXEC.
      *    MOVE '&*                    #   #00011497' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03590
03591  2475-EXIT.
03592      EXIT.
03593      EJECT
03594  2600-CREATE-MAINT-NOTE.
03595      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
03596      MOVE '1'                    TO CNTL-REC-TYPE.
03597      MOVE SPACES                 TO CNTL-PROC-ID.
03598      MOVE ZERO                   TO CNTL-SEQ-NO.
03599
03600      
      * EXEC CICS READ
03601 *         SET        (ADDRESS OF CONTROL-FILE)
03602 *         DATASET    ('ELCNTL')
03603 *         RIDFLD     (CNTL-KEY)
03604 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00011510' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03605
03606      IF CO-NO-USE-AUDIT-CHANGES
03607          GO TO 2600-EXIT.
03608
03609      IF PROCL GREATER ZERO
03610      IF PROCI NOT = LOW-VALUES
03611      IF PROCI NOT = CL-PROCESSOR-ID
03612          MOVE 'PROCESSOR'          TO SPLIT-INFO-DESC
03613          MOVE CL-PROCESSOR-ID      TO SPLIT-INFO-OLD
03614          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03615
03616      IF SEXL GREATER ZERO
03617      IF SEXI NOT = LOW-VALUES
03618      IF SEXI NOT = CL-INSURED-SEX-CD
03619          MOVE 'SEX'                TO SPLIT-INFO-DESC
03620          MOVE CL-INSURED-SEX-CD    TO SPLIT-INFO-OLD
03621          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03622
03623      IF BIRTHL GREATER ZERO
03624      IF HOLD-BIRTH NOT = LOW-VALUES
03625      IF HOLD-BIRTH NOT = CL-INSURED-BIRTH-DT
03626          MOVE 'BIRTH'              TO SPLIT-INFO-DESC
03627          MOVE CL-INSURED-BIRTH-DT  TO DC-BIN-DATE-1
03628          MOVE SPACES               TO DC-OPTION-CODE
03629          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
03630          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
03631          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03632
03633      IF SOCIALL GREATER ZERO
03634      IF SOCIALI NOT = LOW-VALUES
03635      IF SOCIALI NOT = CL-SOC-SEC-NO
03636          MOVE 'SOC SEC NO'         TO SPLIT-INFO-DESC
03637          MOVE CL-SOC-SEC-NO        TO SPLIT-INFO-OLD
03638          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03639
03640      IF OCCL GREATER ZERO
03641      IF OCCI NOT = LOW-VALUES
03642      IF OCCI NOT = CL-INSURED-OCC-CD
03643          MOVE 'OCCUPATION'         TO SPLIT-INFO-DESC
03644          MOVE CL-INSURED-OCC-CD    TO SPLIT-INFO-OLD
03645          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03646
03647      IF BENEL GREATER ZERO
03648      IF BENEI NOT = LOW-VALUES
03649      IF BENEI NOT = CL-BENEFICIARY
03650          MOVE 'BENEFICIARY'        TO SPLIT-INFO-DESC
03651          MOVE CL-BENEFICIARY      TO SPLIT-INFO-OLD
03652          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03653
03654      IF PREMTYPL GREATER ZERO
03655      IF PREMTYPI NOT = LOW-VALUES
03656      IF PREMTYPI NOT = CL-CLAIM-PREM-TYPE
03657          MOVE 'PREM TYPE'          TO SPLIT-INFO-DESC
03658          MOVE CL-CLAIM-PREM-TYPE   TO SPLIT-INFO-OLD
03659          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03660
040814*    IF CAUSEL GREATER ZERO
040814*    IF CAUSEI NOT = LOW-VALUES
040814*    IF CAUSEI NOT = CL-CAUSE-CD
040814*        MOVE 'CAUSE CD'           TO SPLIT-INFO-DESC
040814*        MOVE CL-CAUSE-CD          TO SPLIT-INFO-OLD
040814*        PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03667
040814*    IF ENDL GREATER ZERO
040814*    IF HOLD-END NOT = LOW-VALUES
040814*    IF HOLD-END NOT = CL-EST-END-OF-DISAB-DT
040814*        MOVE 'END DT'             TO SPLIT-INFO-DESC
040814*        MOVE CL-EST-END-OF-DISAB-DT
040814*                                  TO DC-BIN-DATE-1
040814*        MOVE SPACES               TO DC-OPTION-CODE
040814*        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
040814*        MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
040814*        PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03678
03679      IF PDTHRUL GREATER ZERO
03680      IF HOLD-PDTHRU NOT = LOW-VALUES
03681      IF HOLD-PDTHRU NOT = CL-PAID-THRU-DT
03682          MOVE 'PAID THRU'          TO SPLIT-INFO-DESC
03683          MOVE CL-PAID-THRU-DT      TO DC-BIN-DATE-1
03684          MOVE SPACES               TO DC-OPTION-CODE
03685          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
03686          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
03687          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03688
03689      IF PDAMTL GREATER ZERO
03690      IF HOLD-PDAMT NOT = ZEROS
03691      IF HOLD-PDAMT NOT = CL-TOTAL-PAID-AMT
03692          MOVE 'PAID AMOUNT'        TO SPLIT-INFO-DESC
03693          MOVE CL-TOTAL-PAID-AMT    TO WS-EDIT-AMT
03694          MOVE WS-EDIT-AMT          TO SPLIT-INFO-OLD
03695          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03696
03697      IF NODAYSL GREATER ZERO
03698      IF HOLD-NODAYS NOT = ZEROS
03699      IF HOLD-NODAYS NOT = CL-NO-OF-DAYS-PAID
03700          MOVE 'DAYS PAID'          TO SPLIT-INFO-DESC
03701          MOVE CL-NO-OF-DAYS-PAID   TO WS-EDIT-NUMBER
03702          MOVE WS-EDIT-NUMBER       TO SPLIT-INFO-OLD
03703          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03704
03705      IF NOPMTSL GREATER ZERO
03706      IF HOLD-NOPMTS NOT = ZEROS
03707      IF HOLD-NOPMTS NOT = CL-NO-OF-PMTS-MADE
03708          MOVE 'PMTS MADE'          TO SPLIT-INFO-DESC
03709          MOVE CL-NO-OF-PMTS-MADE   TO WS-EDIT-NUMBER
03710          MOVE WS-EDIT-NUMBER       TO SPLIT-INFO-OLD
03711          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03712
03713      IF FORMTYPL GREATER ZERO
03714      IF FORMTYPI NOT = LOW-VALUES
03715      IF FORMTYPI NOT = CL-PROG-FORM-TYPE
03716          MOVE 'FORM TYPE'          TO SPLIT-INFO-DESC
03717          MOVE CL-PROG-FORM-TYPE    TO SPLIT-INFO-OLD
03718          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03719
03720      IF REPL GREATER ZERO
03721      IF HOLD-REPORTED NOT = LOW-VALUES
03722      IF HOLD-REPORTED NOT = CL-REPORTED-DT
03723          MOVE 'REPORTED DT'        TO SPLIT-INFO-DESC
03724          MOVE CL-REPORTED-DT       TO DC-BIN-DATE-1
03725          MOVE SPACES               TO DC-OPTION-CODE
03726          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
03727          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
03728          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03729
03730      IF ADDONDTL GREATER ZERO
03731      IF HOLD-ADDON NOT = LOW-VALUES
03732      IF HOLD-ADDON NOT = CL-LAST-ADD-ON-DT
03733          MOVE 'ADD ON DT'          TO SPLIT-INFO-DESC
03734          MOVE CL-LAST-ADD-ON-DT    TO DC-BIN-DATE-1
03735          MOVE SPACES               TO DC-OPTION-CODE
03736          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
03737          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
03738          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03739
03740      IF PRICDL GREATER ZERO
03741      IF PRICDI NOT = LOW-VALUES
03742      IF PRICDI NOT = CL-PRIORITY-CD
03743          MOVE 'PRIORITY CODE'      TO SPLIT-INFO-DESC
03744          MOVE CL-PRIORITY-CD       TO SPLIT-INFO-OLD
03745          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03746
03747      IF FILETOL GREATER ZERO
03748      IF FILETOI NOT = LOW-VALUES
03749      IF FILETOI NOT = CL-FILE-LOCATION
03750          MOVE 'FILE TO'            TO SPLIT-INFO-DESC
03751          MOVE CL-FILE-LOCATION     TO SPLIT-INFO-OLD
03752          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03753
03754      IF SUPVL GREATER ZERO
03755      IF SUPVI NOT = LOW-VALUES
03756      IF SUPVI NOT = CL-SUPV-ATTN-CD
03757          MOVE 'SUPV (Y/N)'         TO SPLIT-INFO-DESC
03758          MOVE CL-SUPV-ATTN-CD      TO SPLIT-INFO-OLD
03759          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
052113*    IF (CRITPTL > ZERO)
052113*       AND (CRITPTI NOT = LOW-VALUES)
052113*       AND (CRITPTI NOT = CL-CRIT-PER-RECURRENT)
052113*       MOVE 'CRIT PER RECURR'   TO SPLIT-INFO-DESC
052113*       MOVE CL-CRIT-PER-RECURRENT
052113*                                TO SPLIT-INFO-OLD
052113*       PERFORM 2650-WRITE-MAINT-NOTE
052113*                                THRU 2650-EXIT
052113*    END-IF
052113
052113*    IF (RTWMOSL > ZERO)
052113*       AND (RTWMOSI NOT = LOW-VALUES)
052113*       AND (RTWMOSI NOT = CL-CRIT-PER-RTW-MOS)
052113*       MOVE 'CRIT PER RTW MO'   TO SPLIT-INFO-DESC
052113*       MOVE CL-CRIT-PER-RTW-MOS TO SPLIT-INFO-OLD
052113*       PERFORM 2650-WRITE-MAINT-NOTE
052113*                                THRU 2650-EXIT
052113*    END-IF
03761      IF MLNAMEL GREATER ZERO
03762      IF MLNAMEI NOT = LOW-VALUES
03763      IF MLNAMEI NOT = CL-INSURED-LAST-NAME
03764          MOVE 'INS LAST NAME'      TO SPLIT-INFO-DESC
03765          MOVE CL-INSURED-LAST-NAME TO SPLIT-INFO-OLD
03766          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03767
03768      IF MFNAMEL GREATER ZERO
03769      IF MFNAMEI NOT = LOW-VALUES
03770      IF MFNAMEI NOT = CL-INSURED-1ST-NAME
03771          MOVE 'INS 1ST NAME'       TO SPLIT-INFO-DESC
03772          MOVE CL-INSURED-1ST-NAME  TO SPLIT-INFO-OLD
03773          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03774
03775      IF MMINITL GREATER ZERO
03776      IF MMINITI NOT = LOW-VALUES
03777      IF MMINITI NOT = CL-INSURED-MID-INIT
03778          MOVE 'INS MID INITL'      TO SPLIT-INFO-DESC
03779          MOVE CL-INSURED-MID-INIT  TO SPLIT-INFO-OLD
03780          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03781
03782      IF CERTEFFL GREATER ZERO
03783      IF HOLD-EFF NOT = LOW-VALUES
03784      IF HOLD-EFF NOT = CL-CERT-EFF-DT
03785          MOVE 'CERT EFF DT'        TO SPLIT-INFO-DESC
03786          MOVE CL-CERT-EFF-DT       TO DC-BIN-DATE-1
03787          MOVE SPACES               TO DC-OPTION-CODE
03788          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
03789          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
03790          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03791
03792      IF CERTACTL GREATER ZERO
03793      IF CERTACTI NOT = LOW-VALUES
03794      IF CERTACTI NOT = CL-CERT-ACCOUNT
03795          MOVE 'CERT ACCOUNT'       TO SPLIT-INFO-DESC
03796          MOVE CL-CERT-ACCOUNT      TO SPLIT-INFO-OLD
03797          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03798
03799      IF CERTSTL GREATER ZERO
03800      IF CERTSTI NOT = LOW-VALUES
03801      IF CERTSTI NOT = CL-CERT-STATE
03802          MOVE 'CERT STATE'         TO SPLIT-INFO-DESC
03803          MOVE CL-CERT-STATE        TO SPLIT-INFO-OLD
03804          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03805
03806      IF CERTCARL GREATER ZERO
03807      IF CERTCARI NOT = LOW-VALUES
03808      IF CERTCARI NOT = CL-CERT-CARRIER
03809          MOVE 'CERT CARRIER'       TO SPLIT-INFO-DESC
03810          MOVE CL-CERT-CARRIER      TO SPLIT-INFO-OLD
03811          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03812
03813      IF CERTGRPL GREATER ZERO
03814      IF CERTGRPI NOT = LOW-VALUES
03815      IF CERTGRPI NOT = CL-CERT-GROUPING
03816          MOVE 'CERT GROUPING'      TO SPLIT-INFO-DESC
03817          MOVE CL-CERT-GROUPING     TO SPLIT-INFO-OLD
03818          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
03819
03820      IF CERTL GREATER ZERO OR
03821         SUFXL GREATER ZERO
03822      IF (CERTI NOT = LOW-VALUES AND
03823                      CL-CERT-PRIME)
03824                 OR
03825         (SUFXI NOT = LOW-VALUES AND
03826                      CL-CERT-SFX)
03827          MOVE 'CERT NO'            TO SPLIT-INFO-DESC
03828          MOVE CL-CERT-NO           TO SPLIT-INFO-OLD
03829          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
080613     IF incl > zeros
080613        IF (HOLD-INCUR NOT = LOW-VALUES)
080613           AND (HOLD-INCUR NOT = CL-INCURRED-DT)
080613           MOVE 'INC DTE'        TO SPLIT-INFO-DESC
080613           MOVE CL-INCURRED-DT   TO DC-BIN-DATE-1
080613           MOVE SPACES           TO DC-OPTION-CODE
080613           PERFORM 9800-CONVERT-DATE
080613                                 THRU 9800-EXIT
080613           MOVE DC-GREG-DATE-1-EDIT
080613                                 TO SPLIT-INFO-OLD
080613           PERFORM 2650-WRITE-MAINT-NOTE
080613                                 THRU 2650-EXIT
080613        END-IF
080613     END-IF
101917     IF PI-STATE = 'VA' OR 'PA' OR 'GA'
101917       IF (REPL GREATER ZERO
101917         AND HOLD-REPORTED NOT = LOW-VALUES
101917         AND HOLD-REPORTED NOT = CL-REPORTED-DT)
101917        OR (incl > zeros
101917          AND (HOLD-INCUR NOT = LOW-VALUES)
101917             AND (HOLD-INCUR NOT = CL-INCURRED-DT))
101917          PERFORM 2610-CHECK-2-YEAR-CONTESTABLE THRU 2610-EXIT
101917       END-IF
101917     END-IF
090821     if incl > zeros
090821        move cl-incurred-dt      to ws-prev-inc-dt
090821        perform 7990-get-lo-hi-acct-dates
090821                                 thru 7990-exit
090821        if (hold-incur >= ws-hi-acct-dt)
090821           and (acct-cancelled)
090821           and (mob-cert)
090821           MOVE er-1682          TO EMI-ERROR
090821           MOVE -1               TO INCL
090821           MOVE AL-UABON         TO INCA
090821           PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821           PERFORM 2620-CHECK-TRLR
090821                                 THRU 2620-EXIT
090821        end-if
090821        if hold-incur < cm-cert-eff-dt
090821           MOVE er-1683          TO EMI-ERROR
090821           MOVE -1               TO INCL
090821           MOVE AL-UABON         TO INCA
090821           PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821           PERFORM 2620-CHECK-TRLR
090821                                 THRU 2620-EXIT
090821        end-if
090821     end-if
071720*052918IF incl > zeros
071720*052918  AND PI-COMPANY-ID = 'CID'
071720*052918   IF (HOLD-INCUR NOT = LOW-VALUES)
071720*052918      AND (HOLD-INCUR NOT = CL-INCURRED-DT)
071720*052918      IF TYPEI = PI-AH-OVERRIDE-L1
071720*052918         SET ELAPSED-BETWEEN-BIN TO TRUE
071720*052918         MOVE ZERO               TO DC-ELAPSED-MONTHS
071720*052918                                    DC-ELAPSED-DAYS
071720*052918
071720*052918         MOVE HOLD-INCUR  TO DC-BIN-DATE-1
071720*052918         MOVE SAVE-BIN-DATE TO DC-BIN-DATE-2
071720*052918         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
071720*052918         IF DC-ODD-DAYS-OVER > ZERO
071720*052918            ADD 1 TO DC-ELAPSED-MONTHS
071720*052918         END-IF
071720*052918
071720*052918         IF PI-STATE = 'HI'
071720*052918           AND DC-ELAPSED-MONTHS <= 18
071720*052918            CONTINUE
071720*052918         ELSE
071720*052918            IF DC-ELAPSED-MONTHS > 15
071720*052918               MOVE ER-7572            TO EMI-ERROR
071720*052918               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
071720*052918            END-IF
071720*052918         END-IF
071720*052918         PERFORM 2620-CHECK-TRLR THRU 2620-EXIT
071720*052918      END-IF
071720*052918   END-IF
071720*052918END-IF
           .
03831  2600-EXIT.
03832       EXIT.
101917 2610-CHECK-2-YEAR-CONTESTABLE.
101917     SET ELAPSED-BETWEEN-BIN TO TRUE
101917     MOVE ZERO               TO DC-ELAPSED-MONTHS
101917                                DC-ELAPSED-DAYS
101917
101917     MOVE HOLD-EFF TO DC-BIN-DATE-1.
101917     IF HOLD-INCUR > SPACES
101917        MOVE HOLD-INCUR TO DC-BIN-DATE-2
101917     ELSE
101917        MOVE CL-INCURRED-DT TO DC-BIN-DATE-2
101917     END-IF
101917     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
101917     IF DC-ODD-DAYS-OVER > ZERO
101917        ADD 1 TO DC-ELAPSED-MONTHS
101917     END-IF
101917
101917     IF DC-ELAPSED-MONTHS <= 24
101917        IF HOLD-REPORTED > SPACES
101917           MOVE HOLD-REPORTED TO DC-BIN-DATE-2
101917        ELSE
101917           MOVE CL-REPORTED-DT TO DC-BIN-DATE-2
101917        END-IF
101917        MOVE ZERO           TO DC-ELAPSED-MONTHS
101917                               DC-ELAPSED-DAYS
101917        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
101917        IF DC-ODD-DAYS-OVER > ZERO
101917           ADD 1 TO DC-ELAPSED-MONTHS
101917        END-IF
101917        IF DC-ELAPSED-MONTHS > 24
101917           MOVE ER-1679            TO EMI-ERROR
101917           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061418        ELSE
061418           MOVE 1                  TO EMI-ERROR
101917        END-IF
101917     END-IF.
101917
101917     PERFORM 2615-CHECK-TRLR THRU 2615-EXIT.
101917
101917 2610-EXIT.
101917     EXIT.
101917 2615-CHECK-TRLR.
101917
101917     MOVE CL-CONTROL-PRIMARY   TO TRLR-KEY
101917     MOVE +96              TO TRLR-SEQ-NO
101917     
      * EXEC CICS READ
101917*       UPDATE
101917*       DATASET  ('ELTRLR')
101917*       SET      (ADDRESS OF ACTIVITY-TRAILERS)
101917*       RIDFLD   (TRLR-KEY)
101917*       RESP     (WS-RESPONSE)
101917*    END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00011882' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303131383832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
101917     IF WS-RESP-NORMAL
101917        IF EMI-ERROR =  ER-1679
101917           IF EMI-ERROR-NUMBER(1) = ER-1679
101917              MOVE EMI-LINE1 TO AT-INFO-LINE-1
101917           ELSE
101917           IF EMI-ERROR-NUMBER(2) = ER-1679
101917              MOVE EMI-LINE2 TO AT-INFO-LINE-1
101917           ELSE
101917           IF EMI-ERROR-NUMBER(3) = ER-1679
101917              MOVE EMI-LINE3 TO AT-INFO-LINE-1
101917           END-IF
101917           END-IF
101917           END-IF
101917           
      * EXEC CICS REWRITE
101917*             DATASET   ('ELTRLR')
101917*             FROM      (ACTIVITY-TRAILERS)
101917*          END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00011902' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
101917        ELSE
061418           IF EMI-ERROR = 1
101917              MOVE WS-CONTEST-NOTE TO AT-INFO-LINE-1
061418              MOVE ZERO TO EMI-ERROR
101917              
      * EXEC CICS REWRITE
101917*                DATASET   ('ELTRLR')
101917*                FROM      (ACTIVITY-TRAILERS)
101917*             END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00011910' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061418           ELSE
061418              
      * EXEC CICS DELETE
061418*                DATASET   ('ELTRLR')
061418*             END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&(                    &   #00011915' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061418           END-IF
101917        END-IF
101917     ELSE
061418     IF EMI-ERROR =  ER-1679
101917        
      * EXEC CICS GETMAIN
101917*          SET      (ADDRESS OF ACTIVITY-TRAILERS)
101917*          LENGTH   (TRLR-LENGTH)
101917*          INITIMG  (GETMAIN-SPACE)
101917*       END-EXEC
      *    MOVE ',"IL                  $   #00011922' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
101917        MOVE 'AT'          TO AT-RECORD-ID
101917        MOVE CL-CONTROL-PRIMARY
101917                           TO AT-CONTROL-PRIMARY
101917        MOVE +96           TO AT-SEQUENCE-NO
101917        MOVE '6'           TO AT-TRAILER-TYPE
101917        MOVE SPACES             TO AT-GENERAL-INFO-TR
101917        INITIALIZE AT-GENERAL-INFO-TR
101917        IF EMI-ERROR-NUMBER(1) = ER-1679
101917           MOVE EMI-LINE1 TO AT-INFO-LINE-1
101917        ELSE
101917        IF EMI-ERROR-NUMBER(2) = ER-1679
101917           MOVE EMI-LINE2 TO AT-INFO-LINE-1
101917        ELSE
101917        IF EMI-ERROR-NUMBER(3) = ER-1679
101917           MOVE EMI-LINE3 TO AT-INFO-LINE-1
101917        END-IF
101917        END-IF
101917        END-IF
101917        MOVE SPACE           TO AT-INFO-TRAILER-TYPE
101917        MOVE SAVE-BIN-DATE TO AT-RECORDED-DT
101917                              AT-GEN-INFO-LAST-MAINT-DT
101917        MOVE PI-PROCESSOR-ID
101917                           TO AT-RECORDED-BY
101917                              AT-GEN-INFO-LAST-UPDATED-BY
101917        MOVE EIBTIME       TO AT-LAST-MAINT-HHMMSS
101917        
      * EXEC CICS WRITE
101917*          DATASET   ('ELTRLR')
101917*          FROM      (ACTIVITY-TRAILERS)
101917*          RIDFLD    (TRLR-KEY)
101917*       END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00011952' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061418     END-IF
101917     END-IF
101917     .
101917 2615-EXIT.
101917     EXIT.
101917
090821 2620-CHECK-TRLR.
090821
090821     MOVE SPACES                 TO AT-GENERAL-INFO-TR
090821     INITIALIZE AT-GENERAL-INFO-TR
090821     MOVE MSTR-KEY               TO AT-CONTROL-PRIMARY
090821     MOVE 'AT'                   TO AT-RECORD-ID
090821     evaluate true
090821        when emi-error = er-1679
090821           move er-1679-text     to at-info-line-1
090821        when emi-error = er-1682
090821           move er-1682-text     to at-info-line-1
090821        when emi-error = er-1683
090821           move er-1683-text     to at-info-line-1
090821     end-evaluate
090821     move +97                    to at-sequence-no
090821     MOVE '6'                    TO AT-TRAILER-TYPE
090821     MOVE save-bin-date          TO AT-RECORDED-DT
090821                                    AT-GEN-INFO-LAST-MAINT-DT
090821     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
090821                                    AT-GEN-INFO-LAST-UPDATED-BY
090821     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
090821
090821     MOVE SPACES                 TO AT-INFO-LINE-2
090821
090821     .
090821 2620-WRITE.
090821
090821     
      * EXEC CICS HANDLE CONDITION
090821*        DUPREC    (2620-DUPREC)
090821*    END-EXEC.
      *    MOVE '"$%                   ! ; #00011990' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303131393930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
090821
090821     
      * EXEC CICS WRITE
090821*         DATASET     ('ELTRLR')
090821*         FROM        (ACTIVITY-TRAILERS)
090821*         RIDFLD      (AT-CONTROL-PRIMARY)
090821*     END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00011994' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821
090821     GO TO 2620-EXIT
090821
090821     .
090821 2620-DUPREC.
090821
090821     SUBTRACT +1 FROM AT-SEQUENCE-NO
090821     GO TO 2620-WRITE
090821
090821     .
090821 2620-EXIT.
090821     EXIT.
071720*0529182620-CHECK-TRLR.
071720*052918
071720*052918MOVE CL-CONTROL-PRIMARY   TO TRLR-KEY
071720*052918MOVE +97                  TO TRLR-SEQ-NO
071720*052918EXEC CICS READ
071720*052918   UPDATE
071720*052918   DATASET  ('ELTRLR')
071720*052918   SET      (ADDRESS OF ACTIVITY-TRAILERS)
071720*052918   RIDFLD   (TRLR-KEY)
071720*052918   RESP     (WS-RESPONSE)
071720*052918END-EXEC
071720*052918IF WS-RESP-NORMAL
071720*052918   IF EMI-ERROR =  ER-7572
071720*052918      MOVE WS-FILING-NOTE TO AT-INFO-LINE-1
071720*052918      EXEC CICS REWRITE
071720*052918         DATASET   ('ELTRLR')
071720*052918         FROM      (ACTIVITY-TRAILERS)
071720*052918      END-EXEC
071720*052918   ELSE
071720*052918      MOVE WS-FILE-LIM-NOTE TO AT-INFO-LINE-1
071720*052918         EXEC CICS REWRITE
071720*052918            DATASET   ('ELTRLR')
071720*052918            FROM      (ACTIVITY-TRAILERS)
071720*052918         END-EXEC
071720*052918   END-IF
071720*052918ELSE
071720*052918   IF EMI-ERROR =  ER-7572
071720*052918      EXEC CICS GETMAIN
071720*052918         SET      (ADDRESS OF ACTIVITY-TRAILERS)
071720*052918         LENGTH   (TRLR-LENGTH)
071720*052918         INITIMG  (GETMAIN-SPACE)
071720*052918      END-EXEC
071720*052918      MOVE 'AT'          TO AT-RECORD-ID
071720*052918      MOVE CL-CONTROL-PRIMARY
071720*052918                         TO AT-CONTROL-PRIMARY
071720*052918      MOVE +97           TO AT-SEQUENCE-NO
071720*052918      MOVE '6'           TO AT-TRAILER-TYPE
071720*052918      MOVE SPACES             TO AT-GENERAL-INFO-TR
071720*052918      INITIALIZE AT-GENERAL-INFO-TR
071720*052918      MOVE WS-FILING-NOTE TO AT-INFO-LINE-1
071720*052918      MOVE SPACE           TO AT-INFO-TRAILER-TYPE
071720*052918      MOVE SAVE-BIN-DATE TO AT-RECORDED-DT
071720*052918                            AT-GEN-INFO-LAST-MAINT-DT
071720*052918      MOVE PI-PROCESSOR-ID
071720*052918                         TO AT-RECORDED-BY
071720*052918                            AT-GEN-INFO-LAST-UPDATED-BY
071720*052918      MOVE EIBTIME       TO AT-LAST-MAINT-HHMMSS
071720*052918      EXEC CICS WRITE
071720*052918         DATASET   ('ELTRLR')
071720*052918         FROM      (ACTIVITY-TRAILERS)
071720*052918         RIDFLD    (TRLR-KEY)
071720*052918      END-EXEC
071720*052918   END-IF
071720*052918END-IF
071720*052918.
071720*0529182620-EXIT.
071720*052918EXIT.
03833
03834  2650-WRITE-MAINT-NOTE.
03835      
      * EXEC CICS GETMAIN
03836 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
03837 *         LENGTH   (TRLR-LENGTH)
03838 *         INITIMG  (GETMAIN-SPACE)
03839 *    END-EXEC.
      *    MOVE ',"IL                  $   #00012070' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03840
03841      
      * EXEC CICS HANDLE CONDITION
03842 *         DUPREC   (2650-EXIT)
03843 *    END-EXEC.
      *    MOVE '"$%                   ! < #00012076' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303132303736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03844
03845      MOVE SPACES             TO ACTIVITY-TRAILERS.
03846
03847      IF CERTL GREATER ZERO
03848          MOVE CERTI          TO CL-CERT-PRIME.
03849
03850      IF SUFXL GREATER ZERO
03851          MOVE SUFXI          TO CL-CERT-SFX.
03852
03853      MOVE CL-CONTROL-PRIMARY TO AT-CONTROL-PRIMARY.
03854
03855      SUBTRACT 1 FROM  CL-TRAILER-SEQ-CNT.
03856      MOVE CL-TRAILER-SEQ-CNT TO AT-SEQUENCE-NO.
03857
03858      MOVE 'AT'               TO AT-RECORD-ID.
03859
03860      MOVE '6'                TO AT-TRAILER-TYPE.
03861      MOVE SAVE-BIN-DATE      TO AT-RECORDED-DT
03862                                 AT-GEN-INFO-LAST-MAINT-DT.
03863      MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY
03864                                 AT-GEN-INFO-LAST-UPDATED-BY.
03865      MOVE EIBTIME            TO AT-LAST-MAINT-HHMMSS.
03866      MOVE SPLIT-INFO-LINE-1  TO AT-INFO-LINE-1.
03867      MOVE 'M'                TO AT-INFO-TRAILER-TYPE.
03868
03869      
      * EXEC CICS WRITE
03870 *         DATASET  (TRLR-FILE-ID)
03871 *         RIDFLD   (AT-CONTROL-PRIMARY)
03872 *         FROM     (ACTIVITY-TRAILERS)
03873 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00012104' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03874
03875  2650-EXIT.
03876       EXIT.
03877
060413 2800-CHECK-AUTO-PAY.
060413
060413     MOVE PI-COMPANY-CD      TO TRLR-COMPANY-CD.
060413     MOVE PI-CARRIER         TO TRLR-CARRIER.
060413     MOVE PI-CLAIM-NO        TO TRLR-CLAIM-NO.
060413     MOVE PI-CERT-NO         TO TRLR-CERT-NO.
060413     MOVE PI-AUTO-PAY-SEQ    TO TRLR-SEQ-NO.
060413
060413     
      * EXEC CICS HANDLE CONDITION
060413*         NOTFND    (2800-EXIT)
060413*         ENDFILE   (2800-EXIT)
060413*    END-EXEC.
      *    MOVE '"$I''                  ! = #00012121' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303132313231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
060413
060413     
      * EXEC CICS READ
060413*        DATASET (TRLR-FILE-ID)
060413*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
060413*        RIDFLD  (TRLR-KEY)
060413*    END-EXEC.
      *    MOVE '&"S        E          (   #00012126' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
060413
060413     IF BENEFICIARY-PAID-AUTO
060413         MOVE 'Y'              TO AUTO-PAY-TO-BENE
060413     END-IF.
060413
060413 2800-EXIT.
060413      EXIT.
060413
03878      EJECT
03879  3000-DELETE-CLAIM.
03880      IF  NOT MODIFY-CAP
03881          MOVE ER-0070            TO EMI-ERROR
03882          MOVE -1                 TO MAINTL
03883          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03884          GO TO 8110-SEND-DATA.
03885
03886      
      * EXEC CICS READ
03887 *         SET      (ADDRESS OF CLAIM-MASTER)
03888 *         DATASET  (CLMS-FILE-ID)
03889 *         RIDFLD   (MSTR-KEY)
03890 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012147' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03891
03892      IF CL-1ST-TRL-AVAIL
03893          PERFORM 3010-DEL-MSTR-TRLR THRU 3010-EXIT
03894      ELSE
03895         MOVE MSTR-KEY            TO TRLR-MAIN-KEY
03896         MOVE 0                   TO TRLR-SEQ-NO
03897          
      * EXEC CICS STARTBR
03898 *            DATASET (TRLR-FILE-ID)
03899 *            RIDFLD  (TRLR-KEY)
03900 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012158' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03901          PERFORM 3060-READ-TRLR THRU 3060-EXIT
03902          
      * EXEC CICS ENDBR
03903 *            DATASET (TRLR-FILE-ID)
03904 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012163' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03905         IF SCREEN-ERROR
03906             MOVE ER-0208         TO EMI-ERROR
03907             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03908             MOVE -1              TO MAINTL
03909             GO TO 8110-SEND-DATA
03910         ELSE
03911             PERFORM 3010-DEL-MSTR-TRLR THRU 3010-EXIT.
03912
03913      IF SCREEN-ERROR
03914          MOVE -1                 TO MAINTL
03915          GO TO 8110-SEND-DATA.
03916
03917      MOVE -1                     TO ONE-OR-MIN1.
03918      PERFORM 7700-CHECK-SEQUENCE THRU 7799-EXIT.
03919
03920      IF WS-ASSOC-CERT-TOTAL NOT = ZERO
03921          MOVE CL-CONTROL-PRIMARY TO MSTR-KEY
03922                                     WS-SAVE-CLAIM-KEY
03923          MOVE +1                 TO ONE-OR-MIN1
03924          PERFORM 7710-RESEQUENCE-CLAIMS THRU 7799-EXIT.
03925
03926      MOVE 'D'                    TO PI-CLAIM-DELETED-SWITCH.
03927
03928      MOVE LOW-VALUES             TO EL131AO.
03929      MOVE ER-0000                TO EMI-ERROR.
03930      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03931      MOVE 'X'                    TO PI-RETURN-CD-1.
03932      MOVE -1                     TO MAINTL.
03933      GO TO 8100-SEND-MAP.
03934
03935  3010-DEL-MSTR-TRLR.
03936      
      * EXEC CICS READ UPDATE
03937 *         SET       (ADDRESS OF CLAIM-MASTER)
03938 *         DATASET   (CLMS-FILE-ID)
03939 *         RIDFLD    (MSTR-KEY)
03940 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00012197' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03941
062602     if (cl-priority-cd = '8')
062602        and (pi-processor-id not = 'PEMA' and 'JMS '
062602             AND 'AMWA')
062602        MOVE ER-8003             TO EMI-ERROR
062602        PERFORM 9900-ERROR-FORMAT
062602                                 THRU 9900-EXIT
062602        MOVE -1                  TO MAINTL
062602        GO TO 8110-SEND-DATA
062602     end-if
062602
03942      MOVE MSTR-KEY               TO TRLR-MAIN-KEY ACTQ-KEY.
03943      MOVE ZERO                   TO TRLR-SEQ-NO.
03944
03945      
      * EXEC CICS HANDLE CONDITION
03946 *         NOTFND    (3010-DEL-ACTQ)
03947 *         ENDFILE   (3040-EXIT)
03948 *    END-EXEC.
      *    MOVE '"$I''                  ! > #00012216' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303132323136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03949
03950      PERFORM 3040-DELETE-TRLR THRU 3040-EXIT.
03951
03952  3010-DEL-ACTQ.
03953
03954      
      * EXEC CICS HANDLE CONDITION
03955 *         NOTFND    (3010-NO-ACTIVITY)
03956 *         ENDFILE   (3050-EXIT)
03957 *    END-EXEC.
      *    MOVE '"$I''                  ! ? #00012225' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3F20233030303132323235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03958
03959      
      * EXEC CICS STARTBR
03960 *         DATASET  (ACTQ-FILE-ID)
03961 *         RIDFLD   (ACTQ-KEY)
03962 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012230' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03963
03964      
      * EXEC CICS GETMAIN
03965 *         SET      (ADDRESS OF ACTIVITY-QUE)
03966 *         LENGTH   (ACTQ-LENGTH)
03967 *         INITIMG  (GETMAIN-SPACE)
03968 *    END-EXEC.
      *    MOVE ',"IL                  $   #00012235' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACTQ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03969
03970      PERFORM 3050-DELETE-ACTQ THRU 3050-EXIT.
03971
03972      
      * EXEC CICS ENDBR
03973 *         DATASET (ACTQ-FILE-ID)
03974 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012243' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03975
03976  3010-NO-ACTIVITY.
03977
03978      
      * EXEC CICS HANDLE CONDITION
03979 *        NOTFND   (3010-CERT-NOT-FOUND)
03980 *    END-EXEC.
      *    MOVE '"$I                   ! @ #00012249' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4020233030303132323439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03981
03982      MOVE CL-COMPANY-CD          TO CERT-COMPANY-CODE.
03983      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
03984      MOVE CL-CERT-GROUPING       TO CERT-GROUP.
03985      MOVE CL-CERT-STATE          TO CERT-STATE.
03986      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
03987      MOVE CL-CERT-EFF-DT         TO CERT-DATE.
03988      MOVE CL-CERT-NO             TO CERT-CERT.
03989
03990      
      * EXEC CICS READ
03991 *        DATASET   (CERT-FILE-ID)
03992 *        RIDFLD    (CERT-KEY)
03993 *        SET       (ADDRESS OF CERTIFICATE-MASTER)
03994 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012261' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03995
03996      IF CERT-WAS-CREATED AND CM-CLAIM-ATTACHED-COUNT = 1
062121       AND PI-COMPANY-ID NOT EQUAL 'AHL' AND 'FNL'
03997          PERFORM 3030-DELETE-CERT THRU 3030-EXIT
03998      ELSE
03999          PERFORM 3020-UPDATE-CERT THRU 3020-EXIT.
04000
052113 3010-UPDATE-CERT-TRLR.
052113
052113     MOVE CERT-KEY               TO ELCRTT-KEY
052113     MOVE 'B'                    TO CTRLR-REC-TYPE
052113     
      * EXEC CICS READ
052113*       UPDATE
052113*       DATASET   ('ELCRTT')
052113*       RIDFLD    (ELCRTT-KEY)
052113*       INTO      (CERTIFICATE-TRAILERS)
052113*       RESP      (WS-RESPONSE)
052113*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00012277' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303132323737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113     IF WS-RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cs-claim-no (s1) = cl-claim-no)
              end-perform
              if s1 < +25
                 move spaces           to cs-claim-no (s1)
                                          cs-claim-type (s1)
                                          cs-insured-type (s1)
                 move zeros            to cs-days-paid (s1)
                                          cs-total-paid (s1)
                                          cs-benefit-period (s1)
                 if s1 not = +24
                    compute s2 = s1 + +1
                    perform 3015-bump  thru 3015-exit
                 end-if
052113           
      * EXEC CICS REWRITE
052113*             DATASET   ('ELCRTT')
052113*             FROM      (CERTIFICATE-TRAILERS)
052113*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&& L                  %   #00012300' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              end-if
           end-if
           .
04001  3010-CERT-NOT-FOUND.
04002
04003      
      * EXEC CICS DELETE
04004 *        DATASET  (CLMS-FILE-ID)
04005 *    END-EXEC.
      *    MOVE '&(                    &   #00012309' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04006
04007  3010-EXIT.
04008       EXIT.
04009
       3015-bump.
           perform varying s2 from s2 by +1 until s2 > +24
              move cs-mb-claim-data (s2)
                                       to cs-mb-claim-data (s1)
              move spaces              to cs-mb-claim-data (s2)
              add +1 to s1
           end-perform
           .
       3015-exit.
           exit.
04010  3020-UPDATE-CERT.
04011      
      * EXEC CICS READ UPDATE
04012 *         DATASET  (CERT-FILE-ID)
04013 *         RIDFLD   (CERT-KEY)
04014 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
04015 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00012327' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04016
04017      SUBTRACT 1       FROM CM-CLAIM-ATTACHED-COUNT.
04018
04019      
      * EXEC CICS HANDLE CONDITION
04020 *         DUPKEY   (3020-EXIT)
04021 *    END-EXEC.
      *    MOVE '"$$                   ! A #00012335' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4120233030303132333335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04022
04023      
      * EXEC CICS REWRITE
04024 *         DATASET  (CERT-FILE-ID)
04025 *         FROM     (CERTIFICATE-MASTER)
04026 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00012339' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04027
04028  3020-EXIT.
04029      EXIT.
04030
04031  3030-DELETE-CERT.
04032      
      * EXEC CICS READ UPDATE
04033 *         DATASET  (CERT-FILE-ID)
04034 *         RIDFLD   (CERT-KEY)
04035 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
04036 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00012348' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04037
04038      
      * EXEC CICS DELETE
04039 *         DATASET   (CERT-FILE-ID)
04040 *    END-EXEC.
      *    MOVE '&(                    &   #00012354' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04041
04042  3030-EXIT.
04043      EXIT.
04044
04045  3040-DELETE-TRLR.
04046      
      * EXEC CICS READ
04047 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
04048 *         DATASET  (TRLR-FILE-ID)
04049 *         RIDFLD   (TRLR-KEY)
04050 *         GTEQ
04051 *    END-EXEC.
      *    MOVE '&"S        G          (   #00012362' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04052
04053      IF PI-COMPANY-CD = AT-COMPANY-CD
04054         MOVE AT-CONTROL-PRIMARY  TO TRLR-KEY
04055         IF TRLR-MAIN-KEY GREATER THAN CL-CONTROL-PRIMARY
04056            GO TO 3040-EXIT
04057         ELSE
04058            NEXT SENTENCE
04059      ELSE
04060         GO TO 3040-EXIT.
04061
04062      
      * EXEC CICS DELETE
04063 *         DATASET  (TRLR-FILE-ID)
04064 *         RIDFLD   (TRLR-KEY)
04065 *    END-EXEC.
      *    MOVE '&(  R                 &   #00012378' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04066
04067      GO TO 3040-DELETE-TRLR.
04068
04069  3040-EXIT.
04070      EXIT.
04071
04072  3050-DELETE-ACTQ.
04073      
      * EXEC CICS READNEXT
04074 *         DATASET  (ACTQ-FILE-ID)
04075 *         RIDFLD   (ACTQ-KEY)
04076 *         INTO     (ACTIVITY-QUE)
04077 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00012389' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV12, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04078
04079      IF ACTQ-KEY GREATER THAN CL-CONTROL-PRIMARY
04080          GO TO 3050-EXIT.
04081
04082      
      * EXEC CICS ENDBR
04083 *         DATASET  (ACTQ-FILE-ID)
04084 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012398' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04085
04086      
      * EXEC CICS DELETE
04087 *         DATASET   (ACTQ-FILE-ID)
04088 *         RIDFLD    (ACTQ-KEY)
04089 *    END-EXEC.
      *    MOVE '&(  R                 &   #00012402' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04090
04091      
      * EXEC CICS STARTBR
04092 *         DATASET   (ACTQ-FILE-ID)
04093 *         RIDFLD    (ACTQ-KEY)
04094 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012407' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04095
04096      GO TO 3050-DELETE-ACTQ.
04097
04098  3050-EXIT.
04099      EXIT.
04100      EJECT
04101  3060-READ-TRLR.
04102      
      * EXEC CICS HANDLE CONDITION
04103 *         NOTFND    (3060-EXIT)
04104 *         ENDFILE   (3060-EXIT)
04105 *    END-EXEC.
      *    MOVE '"$I''                  ! B #00012418' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4220233030303132343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04106
04107  3060-LOOP.
04108      
      * EXEC CICS READNEXT
04109 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
04110 *         DATASET  (TRLR-FILE-ID)
04111 *         RIDFLD   (TRLR-KEY)
04112 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00012424' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04113
04114      IF TRLR-MAIN-KEY GREATER THAN CL-CONTROL-PRIMARY
04115          GO TO 3060-EXIT.
04116
04117      IF FORM-CONTROL-TR
04118          IF AT-RECORDED-DT = SAVE-BIN-DATE
04119              NEXT SENTENCE
04120          ELSE
04121              MOVE 'X'            TO ERROR-SWITCH
04122              GO TO 3060-EXIT
04123      ELSE
04124          IF PAYMENT-TR OR CORRESPONDENCE-TR
04125             MOVE 'X'                 TO ERROR-SWITCH
04126             GO TO 3060-EXIT.
04127
04128      GO TO 3060-LOOP.
04129
04130  3060-EXIT.
04131      EXIT.
04132      EJECT
052113 3997-GET-ERPDEF.
052113
052113     move cl-company-cd          to cert-company-code
052113     move cl-cert-key-data       to cert-key (2:21)
052113     move cl-cert-no             to cert-cert
052113
052113     
      * EXEC CICS READ
052113*        DATASET   (CERT-FILE-ID)
052113*        RIDFLD    (CERT-KEY)
052113*        SET       (ADDRESS OF CERTIFICATE-MASTER)
052113*        resp      (ws-response)
052113*    END-EXEC
      *    MOVE '&"S        E          (  N#00012455' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303132343535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113
052113     MOVE SPACES                 TO WS-CRIT-PER-ALPHA
052113                                    WS-ERPDEF-SW
052113     MOVE ZEROS                  TO WS-CRITICAL-PERIOD
052113                                    WS-CRIT-PER-RTW-MOS
052113
           if cm-clp-state = spaces or low-values or zeros
              move cm-state            to cm-clp-state
           end-if
052113     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
052113     MOVE CM-clp-state           TO ERPDEF-STATE
052113     MOVE am-dcc-product-code    TO ERPDEF-PROD-CD
070714     evaluate true
100518        when (cl-claim-type = 'L' or 'P' OR 'O')
070714           and (cm-lf-benefit-cd not = '00' and '  ' and 'DD'
070714             and 'CU')
070714           move 'L'              to erpdef-ben-type
070714           move cm-lf-benefit-cd to erpdef-ben-code
100518        when (cl-claim-type not = 'L' and 'P' AND 'O')
070714           and (cm-ah-benefit-cd not = '00' and '  ')
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
100518        when (cl-claim-type not = 'L' and 'P' AND 'O')
070714           and (cm-ah-benefit-cd = '00' or '  ')
070714           move 'L'              to erpdef-ben-type
070714           move cm-lf-benefit-cd to erpdef-ben-code
100518        when (cl-claim-type = 'L' or 'P' OR 'O')
070714           and (cm-lf-benefit-cd = '00' or '  ' or 'DD' or 'CU')
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714        when other
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714     end-evaluate
070714
070714*    if (cl-claim-type = 'L' or 'P')
070714*       and (cm-lf-benefit-cd not = '00' and '  ' and 'DD'
070714*          and 'CU')
070714*       move 'L'                 to erpdef-ben-type
070714*       move cm-lf-benefit-cd    to erpdef-ben-code
070714*    else
070714*       MOVE 'A'                 TO ERPDEF-BEN-TYPE
070714*       MOVE CM-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
070714*    end-if
PEMTST     move cl-insured-birth-dt    to dc-bin-date-1
           move cl-incurred-dt         to dc-bin-date-2
           move '1'                    to dc-option-code
           PERFORM 9800-CONVERT-DATE   THRU 9800-EXIT
           compute ws-att-age =
              dc-elapsed-months / 12
081817     MOVE '6'                    TO DC-OPTION-CODE
           move zeros                  to dc-elapsed-months
                                          dc-elapsed-days
           move low-values to dc-bin-date-1 dc-bin-date-2
052113     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
052113     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
052113
052113     
      * EXEC CICS STARTBR
052113*        DATASET  ('ERPDEF')
052113*        RIDFLD   (ERPDEF-KEY)
052113*        GTEQ
052113*        RESP     (WS-RESPONSE)
052113*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00012518' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303132353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113
052113     IF WS-RESP-NORMAL
052113        
      * EXEC CICS READNEXT
052113*          DATASET  ('ERPDEF')
052113*          INTO     (PRODUCT-MASTER)
052113*          RIDFLD   (ERPDEF-KEY)
052113*          RESP     (WS-RESPONSE)
052113*       END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV12
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00012526' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303132353236' TO DFHEIV0(25:11)
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
052113        IF WS-RESP-NORMAL
052113           IF (ERPDEF-KEY-SAVE (1:16) =
052113              PD-CONTROL-PRIMARY (1:16))
052113              AND (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
052113              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
052113                 (A1 > +11)
052113                 OR ((PD-PROD-CODE (A1) = cl-claim-type)
                        AND (PD-MAX-ATT-AGE (a1) >= WS-ATT-AGE))
052113              END-PERFORM
052113              IF A1 < +12
052113                 SET ERPDEF-FOUND TO TRUE
052113                 MOVE PD-CRIT-PERIOD (A1)
052113                              TO WS-CRITICAL-PERIOD
052113                 MOVE PD-REC-CRIT-PERIOD (A1)
052113                              TO WS-CRIT-PER-RECURRENT
052113                 IF PD-RTW-MOS (A1) NUMERIC
052113                    MOVE PD-RTW-MOS (A1)
052113                              TO WS-CRIT-PER-RTW-MOS
052113                 ELSE
052113                    MOVE 0    TO WS-CRIT-PER-RTW-MOS
052113                 END-IF
052113                 IF PD-EXCLUSION-PERIOD-DAYS (A1) NUMERIC
052113                    MOVE PD-EXCLUSION-PERIOD-DAYS (A1)
052113                                 TO WS-EXCL-PERIOD
052113                 END-IF
052113                 IF PD-COVERAGE-ENDS-MOS (A1) NUMERIC
052113                    MOVE PD-COVERAGE-ENDS-MOS (A1)
052113                                 TO WS-COV-ENDS
052113                 END-IF
052113                 IF PD-ACCIDENT-ONLY-MOS (A1) NUMERIC
052113                    MOVE PD-ACCIDENT-ONLY-MOS (A1)
052113                                 TO WS-ACC-PERIOD
052113                 END-IF
                       if pd-max-extension (a1) numeric
                          move pd-max-extension (a1)
                                       to ws-max-extension
                       end-if
                       if pd-max-amt (a1) numeric
                          move pd-max-amt (a1)
                                       to ws-max-moben
                       end-if
                       if pd-pre-exist-excl-type(a1) numeric
                          move pd-pre-exist-excl-type(a1)
                                       to ws-pre-exsist
                       end-if
052113              END-IF
052113           END-IF
052113        END-IF
052113     END-IF
052113
052113     .
052113 3997-EXIT.
052113     EXIT.
04133  4000-FORCE-ERRORS.
04134      IF PI-RETURN-CD-1 = 'X'
04135          MOVE ER-0311            TO EMI-ERROR
04136          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04137          MOVE -1                 TO MAINTL
04138          GO TO 8110-SEND-DATA.
04139
04140      IF NOT FORCE-CAP
04141          MOVE ER-0416            TO EMI-ERROR
04142          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04143          MOVE -1                 TO MAINTL
04144          GO TO 8110-SEND-DATA.
04145
04146      MOVE 'F'                    TO EMI-ACTION-SWITCH.
04147      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
04148      IF EMI-FATAL-CTR GREATER THAN ZERO
04149          GO TO 4000-FATAL-ERRORS.
04150
04151      PERFORM 2000-UPDATE-CLAIM THRU 2000-EXIT.
04152
04153      IF EMI-FATAL-CTR GREATER THAN ZERO
04154          GO TO 4000-FATAL-ERRORS.
04155
04156      PERFORM 5000-BUILD-MAP THRU 5000-EXIT.
04157
04158      IF EMI-FATAL-CTR GREATER THAN ZERO
04159          GO TO 4000-FATAL-ERRORS.
04160
04161      MOVE SPACE                  TO EMI-ACTION-SWITCH MAINTO.
04162      MOVE AL-UANOF               TO CERTA.
04163      MOVE ER-0000 TO EMI-ERROR.
04164      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04165
04166  4000-FATAL-ERRORS.
04167      MOVE -1                     TO MAINTL.
04168      GO TO 8110-SEND-DATA.
04169      EJECT
082013
082013 4100-SAVE-CHANGES.
082013     IF MAINTL > 0
082013         MOVE MAINTI  TO WS-SAVE-MAINTI
082013         MOVE MAINTL  TO WS-SAVE-MAINTL
082013     END-IF
082013     IF STATUSL > 0
082013         MOVE STATUSI  TO WS-SAVE-STATUSI
082013         MOVE STATUSL  TO WS-SAVE-STATUSL
082013     END-IF
082013     IF REPL > 0
082013         MOVE REPI  TO WS-SAVE-REPI
082013         MOVE REPL  TO WS-SAVE-REPL
082013     END-IF
040814*    IF CAUSEL > 0
040814*        MOVE CAUSEI  TO WS-SAVE-CAUSEI
040814*        MOVE CAUSEL  TO WS-SAVE-CAUSEL
040814*    END-IF
040814*    IF ENDL > 0
040814*        MOVE ENDI  TO WS-SAVE-ENDI
040814*        MOVE ENDL  TO WS-SAVE-ENDL
040814*    END-IF
082013     IF DIAGL > 0
082013         MOVE DIAGI  TO WS-SAVE-DIAGI
082013         MOVE DIAGL  TO WS-SAVE-DIAGL
082013     END-IF
040814     IF ICD1L > 0
040814         MOVE ICD1I  TO WS-SAVE-ICD1I
040814         MOVE ICD1L  TO WS-SAVE-ICD1L
040814     END-IF
040814     IF ICD2L > 0
040814         MOVE ICD2I  TO WS-SAVE-ICD2I
040814         MOVE ICD2L  TO WS-SAVE-ICD2L
040814     END-IF
082013     IF BENEL > 0
082013         MOVE BENEI  TO WS-SAVE-BENEI
082013         MOVE BENEL  TO WS-SAVE-BENEL
082013     END-IF
082013     IF BIRTHL > 0
082013         MOVE BIRTHI  TO WS-SAVE-BIRTHI
082013         MOVE BIRTHL  TO WS-SAVE-BIRTHL
082013     END-IF
082013     IF SOCIALL > 0
082013         MOVE SOCIALI  TO WS-SAVE-SOCIALI
082013         MOVE SOCIALL  TO WS-SAVE-SOCIALL
082013     END-IF
082013     IF SEXL > 0
082013         MOVE SEXI  TO WS-SAVE-SEXI
082013         MOVE SEXL  TO WS-SAVE-SEXL
082013     END-IF
082013     IF MLNAMEL > 0
082013         MOVE MLNAMEI  TO WS-SAVE-MLNAMEI
082013         MOVE MLNAMEL  TO WS-SAVE-MLNAMEL
082013     END-IF
082013     IF MFNAMEL > 0
082013         MOVE MFNAMEI  TO WS-SAVE-MFNAMEI
082013         MOVE MFNAMEL  TO WS-SAVE-MFNAMEL
082013     END-IF
082013     IF MMINITL > 0
082013         MOVE MMINITI  TO WS-SAVE-MMINITI
082013         MOVE MMINITL  TO WS-SAVE-MMINITL
082013     END-IF
062217*    IF LOANNOL > 0
062217*        MOVE LOANNOI  TO WS-SAVE-LOANNOI
062217*        MOVE LOANNOL  TO WS-SAVE-LOANNOL
062217*    END-IF
082013     IF LOANBALL > 0
082013         MOVE LOANBALI  TO WS-SAVE-LOANBALI
082013         MOVE LOANBALL  TO WS-SAVE-LOANBALL
082013     END-IF
082013     IF PROCL > 0
082013         MOVE PROCI  TO WS-SAVE-PROCI
082013         MOVE PROCL  TO WS-SAVE-PROCL
082013     END-IF
082013     IF SUPVL > 0
082013         MOVE SUPVI  TO WS-SAVE-SUPVI
082013         MOVE SUPVL  TO WS-SAVE-SUPVL
082013     END-IF
082013     IF PRICDL > 0
082013         MOVE PRICDI  TO WS-SAVE-PRICDI
082013         MOVE PRICDL  TO WS-SAVE-PRICDL
082013     END-IF
082013     IF FILETOL > 0
082013         MOVE FILETOI  TO WS-SAVE-FILETOI
082013         MOVE FILETOL  TO WS-SAVE-FILETOL
082013     END-IF
082013     IF PDTHRUL > 0
082013         MOVE PDTHRUI  TO WS-SAVE-PDTHRUI
082013         MOVE PDTHRUL  TO WS-SAVE-PDTHRUL
082013     END-IF
082013     IF PDAMTL > 0
082013         MOVE PDAMTI  TO WS-SAVE-PDAMTI
082013         MOVE PDAMTL  TO WS-SAVE-PDAMTL
082013     END-IF
082013     IF NODAYSL > 0
082013         MOVE NODAYSI  TO WS-SAVE-NODAYSI
082013         MOVE NODAYSL  TO WS-SAVE-NODAYSL
082013     END-IF
082013     IF NOPMTSL > 0
082013         MOVE NOPMTSI  TO WS-SAVE-NOPMTSI
082013         MOVE NOPMTSL  TO WS-SAVE-NOPMTSL
082013     END-IF
082013     IF FORMTYPL > 0
082013         MOVE FORMTYPI  TO WS-SAVE-FORMTYPI
082013         MOVE FORMTYPL  TO WS-SAVE-FORMTYPL
082013     END-IF
082013     IF OCCL > 0
082013         MOVE OCCI  TO WS-SAVE-OCCI
082013         MOVE OCCL  TO WS-SAVE-OCCL
082013     END-IF.
082013 4100-EXIT.
082013      EXIT.
082013
082013 4200-LOAD-CHANGES.
082013     IF WS-SAVE-MAINTL > 0
082013         MOVE WS-SAVE-MAINTI  TO MAINTI
082013         MOVE WS-SAVE-MAINTL  TO MAINTL
082013         MOVE AL-UANON        TO MAINTA
082013     END-IF
082013     IF WS-SAVE-STATUSL > 0
082013         MOVE WS-SAVE-STATUSI  TO STATUSI
082013         MOVE WS-SAVE-STATUSL  TO STATUSL
082013         MOVE AL-UANON         TO STATUSA
082013     END-IF
082013     IF WS-SAVE-REPL > 0
082013         MOVE WS-SAVE-REPI  TO REPI
082013         MOVE WS-SAVE-REPL  TO REPL
082013         MOVE AL-UANON      TO REPA
082013     END-IF
040814*    IF WS-SAVE-CAUSEL > 0
040814*        MOVE WS-SAVE-CAUSEI  TO CAUSEI
040814*        MOVE WS-SAVE-CAUSEL  TO CAUSEL
040814*        MOVE AL-UANON        TO CAUSEA
040814*    END-IF
040814*    IF WS-SAVE-ENDL > 0
040814*        MOVE WS-SAVE-ENDI  TO ENDI
040814*        MOVE WS-SAVE-ENDL  TO ENDL
040814*        MOVE AL-UANON      TO ENDA
040814*    END-IF
082013     IF WS-SAVE-DIAGL > 0
082013         MOVE WS-SAVE-DIAGI  TO DIAGI
082013         MOVE WS-SAVE-DIAGL  TO DIAGL
082013         MOVE AL-UANON       TO DIAGA
082013     END-IF
040814     IF WS-SAVE-ICD1L > 0
040814         MOVE WS-SAVE-ICD1I  TO ICD1I
040814         MOVE WS-SAVE-ICD1L  TO ICD1L
040814         MOVE AL-UANON       TO ICD1A
040814     END-IF
040814     IF WS-SAVE-ICD2L > 0
040814         MOVE WS-SAVE-ICD2I  TO ICD2I
040814         MOVE WS-SAVE-ICD2L  TO ICD2L
040814         MOVE AL-UANON       TO ICD2A
040814     END-IF
082013     IF WS-SAVE-BENEL > 0
082013         MOVE WS-SAVE-BENEI  TO BENEI
082013         MOVE WS-SAVE-BENEL  TO BENEL
082013         MOVE AL-UANON       TO BENEA
082013     END-IF
082013     IF WS-SAVE-BIRTHL > 0
082013         MOVE WS-SAVE-BIRTHI  TO BIRTHI
082013         MOVE WS-SAVE-BIRTHL  TO BIRTHL
082013         MOVE AL-UANON        TO BIRTHA
082013     END-IF
082013     IF WS-SAVE-SOCIALL > 0
082013         MOVE WS-SAVE-SOCIALI  TO SOCIALI
082013         MOVE WS-SAVE-SOCIALL  TO SOCIALL
082013         MOVE AL-UANON         TO SOCIALA
082013     END-IF
082013     IF WS-SAVE-SEXL > 0
082013         MOVE WS-SAVE-SEXI  TO SEXI
082013         MOVE WS-SAVE-SEXL  TO SEXL
082013         MOVE AL-UANON      TO SEXA
082013     END-IF
082013     IF WS-SAVE-MLNAMEL > 0
082013         MOVE WS-SAVE-MLNAMEI  TO MLNAMEI
082013         MOVE WS-SAVE-MLNAMEL  TO MLNAMEL
082013         MOVE AL-UANON         TO MLNAMEA
082013     END-IF
082013     IF WS-SAVE-MFNAMEL > 0
082013         MOVE WS-SAVE-MFNAMEI  TO MFNAMEI
082013         MOVE WS-SAVE-MFNAMEL  TO MFNAMEL
082013         MOVE AL-UANON         TO MFNAMEA
082013     END-IF
082013     IF WS-SAVE-MMINITL > 0
082013         MOVE WS-SAVE-MMINITI  TO MMINITI
082013         MOVE WS-SAVE-MMINITL  TO MMINITL
082013         MOVE AL-UANON         TO MMINITA
082013     END-IF
062217*    IF WS-SAVE-LOANNOL > 0
062217*        MOVE WS-SAVE-LOANNOI  TO LOANNOI
062217*        MOVE WS-SAVE-LOANNOL  TO LOANNOL
062217*        MOVE AL-UANON         TO LOANNOA
062217*    END-IF
082013     IF WS-SAVE-LOANBALL > 0
082013         MOVE WS-SAVE-LOANBALI  TO LOANBALI
082013         MOVE WS-SAVE-LOANBALL  TO LOANBALL
082013         MOVE AL-UANON          TO LOANBALA
082013     END-IF
082013     IF WS-SAVE-PROCL > 0
082013         MOVE WS-SAVE-PROCI  TO PROCI
082013         MOVE WS-SAVE-PROCL  TO PROCL
082013         MOVE AL-UANON       TO PROCA
082013     END-IF
082013     IF WS-SAVE-SUPVL > 0
082013         MOVE WS-SAVE-SUPVI  TO SUPVI
082013         MOVE WS-SAVE-SUPVL  TO SUPVL
082013         MOVE AL-UANON       TO SUPVA
082013     END-IF
082013     IF WS-SAVE-PRICDL > 0
082013         MOVE WS-SAVE-PRICDI  TO PRICDI
082013         MOVE WS-SAVE-PRICDL  TO PRICDL
082013         MOVE AL-UANON        TO PRICDA
082013     END-IF
082013     IF WS-SAVE-FILETOL > 0
082013         MOVE WS-SAVE-FILETOI  TO FILETOI
082013         MOVE WS-SAVE-FILETOL  TO FILETOL
082013         MOVE AL-UANON         TO FILETOA
082013     END-IF
082013     IF WS-SAVE-PDTHRUL > 0
082013         MOVE WS-SAVE-PDTHRUI  TO PDTHRUI
082013         MOVE WS-SAVE-PDTHRUL  TO PDTHRUL
082013         MOVE AL-UANON         TO PDTHRUA
082013     END-IF
082013     IF WS-SAVE-PDAMTL > 0
082013         MOVE WS-SAVE-PDAMTI  TO PDAMTI
082013         MOVE WS-SAVE-PDAMTL  TO PDAMTL
082013         MOVE AL-UANON        TO PDAMTA
082013     END-IF
082013     IF WS-SAVE-NODAYSL > 0
082013         MOVE WS-SAVE-NODAYSI  TO NODAYSI
082013         MOVE WS-SAVE-NODAYSL  TO NODAYSL
082013         MOVE AL-UANON         TO NODAYSA
082013     END-IF
082013     IF WS-SAVE-NOPMTSL > 0
082013         MOVE WS-SAVE-NOPMTSI  TO NOPMTSI
082013         MOVE WS-SAVE-NOPMTSL  TO NOPMTSL
082013         MOVE AL-UANON         TO NOPMTSA
082013     END-IF
082013     IF WS-SAVE-FORMTYPL > 0
082013         MOVE WS-SAVE-FORMTYPI  TO FORMTYPI
082013         MOVE WS-SAVE-FORMTYPL  TO FORMTYPL
082013         MOVE AL-UANON          TO FORMTYPA
082013     END-IF
082013     IF WS-SAVE-OCCL > 0
082013         MOVE WS-SAVE-OCCI  TO OCCI
082013         MOVE WS-SAVE-OCCL  TO OCCL
082013         MOVE AL-UANON      TO OCCA
082013     END-IF.
082013 4200-EXIT.
082013      EXIT.
082013
04170  5000-BUILD-MAP.
04171      IF PI-RETURN-CD-1 = SPACE
04172          PERFORM 5010-RESET-PI-AREA THRU 5010-EXIT.
04173
04174      MOVE SPACE                  TO PI-RETURN-CD-1.
04175
04176      MOVE LOW-VALUES             TO EL131AI.
04177
04178      MOVE PI-COMPANY-CD          TO COMPANY-CODE.
04179      MOVE PI-CARRIER             TO CARRIER-CODE.
04180      MOVE PI-CLAIM-NO            TO CLAIM-NO.
04181      MOVE PI-CERT-NO             TO CERT-NO.
04182
04183  5000-READ-CLAIM-MASTER.
04184
04185      
      * EXEC CICS READ
04186 *         SET      (ADDRESS OF CLAIM-MASTER)
04187 *         DATASET  (CLMS-FILE-ID)
04188 *         RIDFLD   (MSTR-KEY)
04189 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012889' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04190
04191      PERFORM 5020-MOVE-MSTR THRU 5020-EXIT.
04192
04193  5000-READ-DIAGNOSIS-TRAILER.
04194
04195      
      * EXEC CICS HANDLE CONDITION
04196 *         NOTFND (5000-TRLR-NOT-FOUND)
04197 *    END-EXEC.
      *    MOVE '"$I                   ! C #00012899' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4320233030303132383939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04198
04199      MOVE MSTR-KEY               TO TRLR-KEY.
04200      MOVE NINETY                 TO TRLR-SEQ-NO.
04201
04202      
      * EXEC CICS READ
04203 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
04204 *         DATASET  (TRLR-FILE-ID)
04205 *         RIDFLD   (TRLR-KEY)
04206 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012906' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132393036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04207
04208      MOVE AT-INFO-LINE-1         TO DIAGO.
040814     MOVE AT-ICD-CODE-1          TO ICD1O.
040814     MOVE AT-ICD-CODE-2          TO ICD2O.
04209
04210  5000-READ-CERTIFICATE-MASTER.
04211
04212      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
04213      MOVE CL-CERT-CARRIER        TO CERT-CARRIER
04214      MOVE CL-CERT-GROUPING       TO CERT-GROUP PI-GROUPING.
04215      MOVE CL-CERT-STATE          TO CERT-STATE PI-STATE.
04216      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT PI-ACCOUNT.
04217      MOVE CL-CERT-NO             TO CERT-CERT PI-CERT-NO.
04218      MOVE CL-CERT-EFF-DT         TO CERT-DATE PI-CERT-EFF-DT.
04219
04220      
      * EXEC CICS HANDLE CONDITION
04221 *         NOTFND (5000-CERT-NOT-FOUND)
04222 *    END-EXEC.
      *    MOVE '"$I                   ! D #00012926' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4420233030303132393236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04223
04224      
      * EXEC CICS READ
04225 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
04226 *         DATASET  (CERT-FILE-ID)
04227 *         RIDFLD   (CERT-KEY)
04228 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012930' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132393330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04229
04230      PERFORM 5030-MOVE-CERT THRU 5030-EXIT.
062217     PERFORM 5100-LOAD-CORR-TRLR THRU 5100-EXIT.
04231      PERFORM 7600-BROWSE-CLAIM THRU 7699-EXIT.
04232
04233      GO TO 5000-EXIT.
04234
04235  5000-TRLR-NOT-FOUND.
04236      MOVE ER-7687                TO EMI-ERROR.
04237      MOVE -1                     TO DIAGL.
04238      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04239      GO TO 5000-EXIT.
04240
04241  5000-CERT-NOT-FOUND.
04242      MOVE ER-0206                TO EMI-ERROR.
04243      MOVE -1                     TO CERTL.
04244      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04245
04246  5000-EXIT.
04247      EXIT.
04248      EJECT
04249  5010-RESET-PI-AREA.
04250      IF CERTI GREATER THAN LOW-VALUES
04251          MOVE CERTI              TO PI-CERT-PRIME.
04252
04253      IF SUFXI GREATER THAN LOW-VALUES
04254          MOVE SUFXI              TO PI-CERT-SFX.
04255
04256      IF CERTEFFI GREATER THAN LOW-VALUES
04257          MOVE HOLD-EFF           TO PI-CERT-EFF-DT.
04258
04259      IF CERTACTI GREATER THAN LOW-VALUES
04260          MOVE CERTACTI           TO PI-ACCOUNT.
04261
04262      IF CERTSTI GREATER THAN LOW-VALUES
04263          MOVE CERTSTI            TO PI-STATE.
04264
04265      IF CERTCARI GREATER THAN LOW-VALUES
04266          MOVE CERTCARI           TO PI-CARRIER.
04267
04268      IF CERTGRPI GREATER THAN LOW-VALUES
04269          MOVE CERTGRPI           TO PI-GROUPING.
04270
04271  5010-EXIT.
04272      EXIT.
04273
04274      EJECT
04275  5020-MOVE-MSTR.
04276
04277      MOVE CL-ASSOC-CERT-SEQU     TO WS-CURRENT-SEQU.
04278      MOVE CL-ASSOC-CERT-TOTAL    TO WS-OF-SEQU.
04279      MOVE WS-CLAIM-SEQU          TO SEQUO.
04280      MOVE AL-SABON               TO SEQUA.
04281      MOVE CL-CLAIM-NO            TO CLAIMO PI-CLAIM-NO.
04282      MOVE CL-CLAIM-TYPE          TO TYPEO
052113                                    pi-claim-type
04283
04284      IF CL-TOTAL-PAID-AMT  = +0 AND
04285         CL-LAST-PMT-AMT    = +0 AND
04286         CL-NO-OF-PMTS-MADE = +0
04287           MOVE AL-UANON          TO TYPEA.
04288
04289      MOVE CL-PRIME-CERT-PRIME    TO PCERTNOO.
04290      MOVE CL-PRIME-CERT-SFX      TO PSUFXO.
04291      MOVE CL-CERT-PRIME          TO CERTO.
04292      MOVE CL-CERT-SFX            TO SUFXO.
04293      MOVE CL-CERT-CARRIER        TO CARRO.
04294
04295      IF CL-CLAIM-STATUS = 'C'
04296          MOVE 'CLOSED'           TO STATUSO
04297      ELSE
04298          MOVE 'OPEN  '           TO STATUSO.
04299
052113     move cl-accident-claim-sw   to accswo
           if cl-benefit-period not numeric
              move zeros               to cl-benefit-period
           end-if
           move cl-benefit-period      to benpero
           move cl-insured-type        to instypeo
04300      MOVE CL-PROCESSOR-ID        TO PROCO.
04301
04302      MOVE CL-FILE-ESTABLISHED-BY TO AUIDO.
04303      MOVE CL-CCN                 TO CCNOO.
04304
04305      MOVE CL-INSURED-LAST-NAME   TO MLNAMEO.
04306      MOVE CL-INSURED-1ST-NAME    TO MFNAMEO.
04307      MOVE CL-INSURED-MID-INIT    TO MMINITO.
04308      MOVE CL-INSURED-SEX-CD      TO SEXO.
04309
04310      IF CL-INSURED-BIRTH-DT GREATER THAN LOW-VALUES
04311          MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
04312          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
04313          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04314          MOVE DC-GREG-DATE-1-EDIT TO BIRTHO
04315      ELSE
04316          MOVE SPACES             TO BIRTHO.
04317
04318      IF CL-SSN-STATE   = CL-CERT-STATE  AND
04319         CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME
04320          NEXT SENTENCE
04321        ELSE
04322          MOVE CL-SOC-SEC-NO      TO SOCIALO.
04323
04324      IF PI-COMPANY-ID = 'DMD'
04325        IF CL-CERT-PRIME (4:1) = 'U' OR 'F'
04326           IF SOCIALO = SPACES OR LOW-VALUES
04327             MOVE ER-0885         TO EMI-ERROR
04328             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04329
04330      MOVE CL-INSURED-OCC-CD      TO OCCO.
040814*    MOVE CL-CAUSE-CD            TO CAUSEO.
04332
040814*    IF CL-EST-END-OF-DISAB-DT GREATER THAN LOW-VALUES
040814*        MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1
040814*        MOVE SPACES                 TO DC-OPTION-CODE
040814*        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
040814*        MOVE DC-GREG-DATE-1-EDIT TO ENDO
040814*    ELSE
040814*        MOVE SPACES             TO ENDO.
04340
04341      IF CL-PAID-THRU-DT GREATER THAN LOW-VALUES
04342         MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
04343         MOVE SPACES             TO DC-OPTION-CODE
04344         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04345         MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO
04346         IF PI-USES-PAID-TO
04347            MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
04348            MOVE '6'                TO DC-OPTION-CODE
04349            MOVE +1                 TO DC-ELAPSED-DAYS
04350            MOVE +0                 TO DC-ELAPSED-MONTHS
04351            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04352            MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO
04353         ELSE
04354            NEXT SENTENCE
04355      ELSE
04356         MOVE SPACES             TO PDTHRUO.
04357
04358      MOVE CL-TOTAL-PAID-AMT      TO PDAMTO.
04359      MOVE CL-NO-OF-DAYS-PAID     TO NODAYSO.
04360      MOVE CL-NO-OF-PMTS-MADE     TO NOPMTSO.
04361      MOVE CL-PROG-FORM-TYPE      TO FORMTYPO.
04362
04363      IF MODIFY-CAP
04364         MOVE AL-UNNOF            TO PDAMTA
04365                                     NODAYSA
04366                                     NOPMTSA
04367         MOVE AL-UANOF            TO PDTHRUA.
080613     MOVE PI-COMPANY-ID          TO CNTL-CO-ID
080613     MOVE '2'                    TO CNTL-REC-TYPE
080613     MOVE +0                     TO CNTL-SEQ-NO
080613     MOVE PI-PROCESSOR-ID        TO CNTL-PROC-ID
080613
080613     
      * EXEC CICS READ
080613*         DATASET  (CNTL-FILE-ID)
080613*         SET      (ADDRESS OF CONTROL-FILE)
080613*         RIDFLD   (CNTL-KEY)
080613*         RESP     (WS-RESPONSE)
080613*    END-EXEC
      *    MOVE '&"S        E          (  N#00013087' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303133303837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
080613
080613     IF WS-RESP-NORMAL
080613        MOVE CF-APPROVAL-LEVEL   TO PI-APPROVAL-LEVEL
080613     end-if
04369      IF CL-INCURRED-DT GREATER THAN LOW-VALUES
04370          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
04371          MOVE SPACES             TO DC-OPTION-CODE
04372          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04373          MOVE DC-GREG-DATE-1-EDIT TO INCO
04374      ELSE
04375          MOVE SPACES             TO INCO.
04376
04377      IF (CL-NO-OF-PMTS-MADE = ZEROS)
111113        OR (PI-APPROVAL-LEVEL = '4' OR '5')
04378         MOVE AL-UANOF            TO INCA
080613     end-if
04380      MOVE CL-NO-OF-PMTS-MADE     TO PI-NO-PMTS.
04381
04382      IF CL-REPORTED-DT GREATER THAN LOW-VALUES
04383          MOVE CL-REPORTED-DT     TO DC-BIN-DATE-1
04384          MOVE SPACES             TO DC-OPTION-CODE
04385          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04386          MOVE DC-GREG-DATE-1-EDIT TO REPO
04387      ELSE
04388          MOVE SPACES             TO REPO.
04389
04390      IF CL-FILE-ESTABLISH-DT GREATER THAN LOW-VALUES
04391          MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1
04392          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
04393          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04394          MOVE DC-GREG-DATE-1-EDIT TO ESTO
04395      ELSE
04396          MOVE SPACES             TO ESTO.
04397
04398      IF CL-LAST-MAINT-DT GREATER THAN LOW-VALUES
04399          MOVE CL-LAST-MAINT-DT   TO DC-BIN-DATE-1
04400          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
04401          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04402          MOVE DC-GREG-DATE-1-EDIT TO MNTDTO
04403      ELSE
04404          MOVE SPACES             TO MNTDTO.
04405
04406
04407      IF CL-LAST-MAINT-TYPE = SPACE
04408          MOVE 'SET-UP'           TO MNTTYPEO
04409      ELSE
04410      IF CL-LAST-MAINT-TYPE = '1'
04411          MOVE 'PAYMNT'           TO MNTTYPEO
04412      ELSE
04413      IF CL-LAST-MAINT-TYPE = '2'
04414          MOVE 'LETTER'           TO MNTTYPEO
04415      ELSE
04416      IF CL-LAST-MAINT-TYPE = '3'
04417          MOVE 'UPDATE'           TO MNTTYPEO
04418      ELSE
04419      IF CL-LAST-MAINT-TYPE = '4'
04420          MOVE 'RESTOR'           TO MNTTYPEO
04421      ELSE
04422      IF CL-LAST-MAINT-TYPE = '5'
04423          MOVE 'INC DT'           TO MNTTYPEO
04424      ELSE
04425      IF CL-LAST-MAINT-TYPE = '6'
04426          MOVE ' CONV'            TO MNTTYPEO
04427      ELSE
04428      IF CL-LAST-MAINT-TYPE = 'C'
04429          MOVE 'CHGBEN'           TO MNTTYPEO
04430      ELSE
04431      IF CL-LAST-MAINT-TYPE = 'E'
04432          MOVE 'ERRCOR'           TO MNTTYPEO
04433      ELSE
04434          MOVE SPACES             TO MNTTYPEO.
04435
04436      MOVE CL-BENEFICIARY         TO BENEO.
04437      MOVE CL-FILE-LOCATION       TO FILETOO.
052113     IF CL-CRITICAL-PERIOD NOT NUMERIC
052113        MOVE ZEROS               TO CL-CRITICAL-PERIOD
052113     END-IF
052113     MOVE CL-CRITICAL-PERIOD     TO CRITPO
081817     IF CL-NO-OF-EXTENSIONS NOT NUMERIC
081817        MOVE ZEROS               TO CL-NO-OF-EXTENSIONS
081817     END-IF
081817     MOVE CL-NO-OF-EXTENSIONS    TO EXTENSO
052113*    MOVE CL-CRIT-PER-RECURRENT
052113*                                TO CRITPTO
052113*    IF CL-CRIT-PER-RTW-MOS NOT NUMERIC
052113*       MOVE ZEROS               TO CL-CRIT-PER-RTW-MOS
052113*    END-IF
052113*    MOVE CL-CRIT-PER-RTW-MOS    TO RTWMOSO
04438      MOVE CL-PRIORITY-CD         TO PRICDO.
04439      MOVE CL-SUPV-ATTN-CD        TO SUPVO.
04440      MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.
04441      MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
060413     MOVE CL-AUTO-PAY-SEQ        TO PI-AUTO-PAY-SEQ.
04442
04443  5020-EXIT.
04444      EXIT.
04445      EJECT
04446  5030-MOVE-CERT.
04447
04448      IF CM-CERT-EFF-DT GREATER THAN LOW-VALUES
04449          MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1
04450          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
04451          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04452          MOVE DC-GREG-DATE-1-EDIT    TO CERTEFFO HOLD-DATE
04453        ELSE
04454          MOVE SPACES             TO CERTEFFO
04455          MOVE ZERO               TO HOLD-DATE.
04456
04457      MOVE CM-ACCOUNT             TO CERTACTO.
04458      MOVE CM-STATE               TO CERTSTO PI-STATE.
04459      MOVE CM-GROUPING            TO CERTGRPO.
04460      MOVE CM-CARRIER             TO CERTCARO.
04461      MOVE CM-INSURED-LAST-NAME   TO CRTLNMEO.
04462      MOVE CM-INSURED-INITIAL2    TO CRTINITO.
04463      MOVE CM-INSURED-FIRST-NAME  TO CRTFNMEO.
04464      MOVE CM-INSURED-ISSUE-AGE   TO ISSAGEO.
04465      MOVE CM-JT-LAST-NAME        TO JNTLNMEO.
04466      MOVE CM-JT-FIRST-NAME       TO JNTFNMEO.
04467      MOVE CM-JT-INITIAL          TO JNTINITO.
04468      MOVE CM-INSURED-JOINT-AGE   TO JNTAGEO.
04469
04470 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
04471      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
04472      MOVE '3'                    TO CNTL-REC-TYPE.
04473      MOVE SPACES                 TO CNTL-STATE-ACCESS.
04474      MOVE CM-STATE               TO CNTL-STATE-NUMBER.
04475      MOVE ZERO                   TO CNTL-SEQ-NO.
04476
04477      
      * EXEC CICS READ
04478 *         SET        (ADDRESS OF CONTROL-FILE)
04479 *         DATASET    ('ELCNTL')
04480 *         RIDFLD     (CNTL-KEY)
04481 *         RESP       (WS-RESPONSE)
04482 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00013221' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303133323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04483
04484      IF WS-RESP-NOTFND
04485         MOVE ZERO                TO CP-FREE-LOOK
04486      ELSE
04487         MOVE CF-ST-FREE-LOOK-PERIOD
04488                                  TO CP-FREE-LOOK.
04489
04490      IF CM-LF-BENEFIT-CD = '00'
04491         GO TO 5030-MOVE-CERT-AH.
04492
04493      EJECT
04494      MOVE EIBDATE                TO DC-JULIAN-YYDDD
04495      MOVE '5'                    TO DC-OPTION-CODE
04496      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04497      MOVE DC-BIN-DATE-1          TO WS-BIN-CURRENT-DT.
04498      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
04499      MOVE CM-LF-BENEFIT-CD       TO LCVCDO HOLD-BENEFIT.
04500      MOVE CM-LF-ORIG-TERM        TO LCVOTRMO  CP-ORIGINAL-TERM.
04501      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT
04502      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
04503      MOVE CL-INCURRED-DT         TO CP-VALUATION-DT
04504      MOVE '4'                    TO CP-REM-TERM-METHOD
04505      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID
04506      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
04507      PERFORM 9700-LINK-RTRM-FILE-ID THRU 9700-EXIT.
04508
04509      MOVE CP-REMAINING-TERM-3    TO LCVRTRMO.
04510      IF CM-LF-PREMIUM-RATE NUMERIC
04511          MOVE CM-LF-PREMIUM-RATE TO LCVRATEO
04512      ELSE
04513          MOVE ZEROS              TO LCVRATEO.
04514
04515      IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC
04516          MOVE ZEROS              TO CM-LF-ALT-BENEFIT-AMT.
04517
04518      COMPUTE LCVBENEO = CM-LF-BENEFIT-AMT + CM-LF-ALT-BENEFIT-AMT.
04519
04520      MOVE CM-POLICY-FORM-NO      TO LCVFORMO.
04521
04522      IF CM-LF-CURRENT-STATUS = '8'
04523         IF CM-LF-CANCEL-DT NOT = LOW-VALUES
04524             MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
04525             MOVE ' '             TO DC-OPTION-CODE
04526             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04527             IF NOT DATE-CONVERSION-ERROR
04528                 MOVE DC-GREG-DATE-1-EDIT     TO LCVCNDTO.
04529
04530      IF CM-LF-CURRENT-STATUS = '7'
04531         IF CM-LF-DEATH-DT NOT = LOW-VALUES
04532             MOVE CM-LF-DEATH-DT  TO DC-BIN-DATE-1
04533             MOVE ' '             TO DC-OPTION-CODE
04534             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04535             IF NOT DATE-CONVERSION-ERROR
04536                 MOVE DC-GREG-DATE-1-EDIT     TO LCVCNDTO.
04537
04538      IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES
04539          MOVE ' '                TO DC-OPTION-CODE
04540          MOVE CM-LF-DEATH-EXIT-DT TO DC-BIN-DATE-1
04541          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04542          IF NOT DATE-CONVERSION-ERROR
04543              MOVE DC-GREG-DATE-1-EDIT TO HOLD-DATE
04544              MOVE HOLD-MONTH     TO WORK-MONTH
04545              MOVE HOLD-YEAR      TO WORK-YEAR
04546              MOVE WORK-DATE-MY   TO LCVEXITO.
04547
04548      IF CM-LF-CURRENT-STATUS = '1' OR = '4'
04549         IF CP-REMAINING-TERM-3 = ZEROS
04550            MOVE 'EXPIRED'        TO LCVSTATO
04551         ELSE
04552            MOVE 'ACTIVE'         TO LCVSTATO.
04553
04554      IF CM-LF-CURRENT-STATUS = '2'
04555         MOVE 'PEND   '           TO LCVSTATO.
04556      IF CM-LF-CURRENT-STATUS = '3'
04557         MOVE 'RESTORE'           TO LCVSTATO.
04558      IF CM-LF-CURRENT-STATUS = '5'
04559         MOVE 'REISSUE'           TO LCVSTATO.
04560      IF CM-LF-CURRENT-STATUS = '6'
100518        MOVE 'LMPBEN'            TO LCVSTATO.
04562      IF CM-LF-CURRENT-STATUS = '7'
04563         MOVE 'DEATH  '           TO LCVSTATO.
04564      IF CM-LF-CURRENT-STATUS = '8'
04565         MOVE 'CANCEL '           TO LCVSTATO.
04566      IF CM-LF-CURRENT-STATUS = '9'
04567         MOVE 'RE-ONLY'           TO LCVSTATO.
04568      IF CM-LF-CURRENT-STATUS = 'V'
04569         MOVE 'VOID   '           TO LCVSTATO.
04570      IF CM-LF-CURRENT-STATUS = 'D'
04571         MOVE 'DECLINE'           TO LCVSTATO.
04572
04573      MOVE SPACES                 TO BENEFIT-KEY ERROR-SWITCH.
04574      MOVE PI-COMPANY-ID          TO BEN-CO-ID.
04575      MOVE '4'                    TO BEN-REC-TYPE.
04576      MOVE CM-LF-BENEFIT-CD       TO BEN-ACC-CD.
04577      MOVE ZERO                   TO BEN-SEQ-NO.
04578
04579      
      * EXEC CICS READ GTEQ
04580 *         DATASET  (CNTL-FILE-ID)
04581 *         RIDFLD   (BENEFIT-KEY)
04582 *         SET      (ADDRESS OF CONTROL-FILE)
04583 *    END-EXEC.
      *    MOVE '&"S        G          (   #00013323' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133333233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04584
04585      IF CF-COMPANY-ID NOT = PI-COMPANY-ID OR
04586         CF-RECORD-TYPE NOT = '4'
04587          MOVE ER-0240            TO EMI-ERROR
04588          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04589          GO TO 5030-MOVE-CERT-FINISH.
04590
04591      MOVE ZERO                   TO COUNT-2.
04592      PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.
04593
04594      IF SCREEN-ERROR
04595          MOVE ER-0282            TO EMI-ERROR
04596          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04597         ELSE
04598          MOVE CF-BENEFIT-ALPHA (COUNT-2) TO LCVKINDO.
04599
04600  5030-MOVE-CERT-AH.
04601
04602      IF CM-LF-BENEFIT-CD = '00'
04603          IF CM-AH-BENEFIT-CD = '00'
04604              GO TO 5030-MOVE-CERT-FINISH.
04605
04606      IF CM-AH-BENEFIT-CD = '00'
04607         MOVE SPACES             TO ACVKINDO
04608         COMPUTE PI-PAYMENT-AMT =
04609                     CM-LF-BENEFIT-AMT / CM-LF-ORIG-TERM
04610         GO TO 5030-MOVE-CERT-FINISH.
04611
04612      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
04613      MOVE CM-AH-BENEFIT-CD       TO ACVCDO HOLD-BENEFIT.
04614      MOVE DC-GREG-DATE-1-EDIT    TO HOLD-DATE.
04615      MOVE CM-AH-ORIG-TERM        TO ACVOTRMO CP-ORIGINAL-TERM.
04616      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT
04617      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
04618      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
04619      MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT.
04620
04621 **DMD **********************************************
04622      IF PI-COMPANY-ID = 'DMD'
04623          COMPUTE WS-TERM-IN-DAYS = CM-AH-ORIG-TERM * 30 +
04624                                    CM-PMT-EXTENSION-DAYS
04625          COMPUTE WS-REM-TERM-IN-DAYS = WS-TERM-IN-DAYS -
04626                                       CL-NO-OF-DAYS-PAID
04627          DIVIDE WS-REM-TERM-IN-DAYS BY +30
04628              GIVING WS-REM-MOS REMAINDER WS-REM-DAYS
04629          MOVE WS-REM-MOS         TO ACVRTRMO
04630                                     CP-REMAINING-TERM-3
052113*        MOVE WS-REM-DAYS        TO DAYSPO
04632          GO TO 5030-SKIP-RTRM.
04633 **DMD **********************************************
04634
04635      MOVE '4'                    TO CP-REM-TERM-METHOD.
04636      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
04637      PERFORM 9700-LINK-RTRM-FILE-ID THRU 9700-EXIT.
04638      MOVE CP-REMAINING-TERM-3    TO ACVRTRMO.
04639
04640  5030-SKIP-RTRM.
04641      IF CM-AH-PREMIUM-RATE NUMERIC
04642          MOVE CM-AH-PREMIUM-RATE TO ACVRATEO
04643      ELSE
04644          MOVE ZEROS              TO ACVRATEO.
04645
04646      MOVE CM-AH-BENEFIT-AMT      TO ACVBENEO  PI-PAYMENT-AMT.
04647      MOVE CM-POLICY-FORM-NO      TO ACVFORMO.
04648
04649      IF CM-AH-CURRENT-STATUS = '8'
04650         IF CM-AH-CANCEL-DT NOT = LOW-VALUES
04651             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
04652             MOVE ' '             TO DC-OPTION-CODE
04653             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04654             IF NOT DATE-CONVERSION-ERROR
04655                 MOVE DC-GREG-DATE-1-EDIT     TO ACVCNDTO.
04656
04657      IF CM-AH-CURRENT-STATUS = '6' OR '7'
04658         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
04659             MOVE CM-AH-SETTLEMENT-DT         TO DC-BIN-DATE-1
04660             MOVE ' '             TO DC-OPTION-CODE
04661             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04662             IF NOT DATE-CONVERSION-ERROR
04663                 MOVE DC-GREG-DATE-1-EDIT     TO ACVCNDTO.
04664
04665      IF CM-AH-CURRENT-STATUS = '1' OR = '4'
04666         IF CP-REMAINING-TERM-3 = ZEROS
04667            MOVE 'EXPIRED'        TO ACVSTATO
04668         ELSE
04669            MOVE 'ACTIVE'         TO ACVSTATO.
04670
04671      IF CM-AH-CURRENT-STATUS = '2'
04672         MOVE 'PEND   '           TO ACVSTATO.
04673      IF CM-AH-CURRENT-STATUS = '3'
04674         MOVE 'RESTORE'           TO ACVSTATO.
04675      IF CM-AH-CURRENT-STATUS = '5'
04676         MOVE 'REISSUE'           TO ACVSTATO.
04677      IF CM-AH-CURRENT-STATUS = '6'
04678         MOVE 'LMP DIS'           TO ACVSTATO.
04679      IF CM-AH-CURRENT-STATUS = '7'
04680         MOVE 'DEATH  '           TO ACVSTATO.
04681      IF CM-AH-CURRENT-STATUS = '8'
04682         MOVE 'CANCEL '           TO ACVSTATO.
04683      IF CM-AH-CURRENT-STATUS = '9'
04684         MOVE 'RE-ONLY'           TO ACVSTATO.
04685      IF CM-AH-CURRENT-STATUS = 'V'
04686         MOVE 'VOID   '           TO ACVSTATO.
04687      IF CM-AH-CURRENT-STATUS = '9'
04688         MOVE 'DECLINE'           TO ACVSTATO.
04689
04690      IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES
04691          MOVE ' '                TO DC-OPTION-CODE
04692          MOVE CM-AH-SETTLEMENT-EXIT-DT TO DC-BIN-DATE-1
04693          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04694          IF NOT DATE-CONVERSION-ERROR
04695              MOVE DC-GREG-DATE-1-EDIT     TO HOLD-DATE
04696              MOVE HOLD-MONTH     TO WORK-MONTH
04697              MOVE HOLD-YEAR      TO WORK-YEAR
04698              MOVE WORK-DATE-MY   TO ACVEXITO.
04699
04700      MOVE SPACES                 TO BENEFIT-KEY ERROR-SWITCH.
04701
04702      MOVE PI-COMPANY-ID          TO BEN-CO-ID.
04703      MOVE '5'                    TO BEN-REC-TYPE.
04704      MOVE CM-AH-BENEFIT-CD       TO BEN-ACC-CD.
04705      MOVE ZERO                   TO BEN-SEQ-NO.
04706
04707      
      * EXEC CICS READ GTEQ
04708 *         DATASET  (CNTL-FILE-ID)
04709 *         RIDFLD   (BENEFIT-KEY)
04710 *         SET      (ADDRESS OF CONTROL-FILE)
04711 *    END-EXEC.
      *    MOVE '&"S        G          (   #00013451' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04712
04713      IF CF-COMPANY-ID NOT = PI-COMPANY-ID OR
04714         CF-RECORD-TYPE NOT = '5'
04715          MOVE ER-0250            TO EMI-ERROR
04716          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04717          GO TO 5030-MOVE-CERT-FINISH.
04718
04719      MOVE ZERO                   TO COUNT-2.
04720      PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.
04721
04722      IF SCREEN-ERROR
04723          MOVE ER-0283            TO EMI-ERROR
04724          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04725        ELSE
04726          MOVE CF-BENEFIT-ALPHA (COUNT-2) TO ACVKINDO.
04727
04728  5030-MOVE-CERT-FINISH.
04729
04730      MOVE CM-LOAN-APR            TO APRO.
04731      MOVE CM-PAY-FREQUENCY       TO PMTFREQO.
04732      MOVE CM-IND-GRP-TYPE        TO INDGRPO.
04733      MOVE CM-PREMIUM-TYPE        TO PREMTYPO  PI-PREM-TYPE.
062217*    MOVE CM-LOAN-NUMBER         TO LOANNOO
04735      MOVE CM-LOAN-BALANCE        TO LOANBALO.
04736      MOVE CM-SPECIAL-REIN-CODE   TO REINCDO.
04737
04738      IF CM-LAST-ADD-ON-DT NOT = SPACES  AND  ZEROS
04739          MOVE CM-LAST-ADD-ON-DT  TO DC-BIN-DATE-1
04740          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
04741          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
04742          MOVE DC-GREG-DATE-1-EDIT  TO ADDONDTO
04743      ELSE
04744          MOVE SPACES               TO ADDONDTO.
04745
04746      MOVE CM-CLAIM-INTERFACE-SW  TO PI-CERT-SWITCH.
04747
04748  5030-EXIT.
04749      EXIT.
04750      EJECT
04751  5040-FIND-BENEFIT.
04752      ADD 1                       TO COUNT-2.
04753      IF COUNT-2 GREATER THAN 8
04754          GO TO 5050-BENEFIT-NOTFND.
04755
04756      IF CF-BENEFIT-CODE (COUNT-2) = HOLD-BENEFIT
04757          GO TO 5060-EXIT.
04758
04759      IF CF-BENEFIT-CODE (COUNT-2) GREATER THAN HOLD-BENEFIT
04760          GO TO 5050-BENEFIT-NOTFND.
04761
04762      GO TO 5040-FIND-BENEFIT.
04763
04764  5050-BENEFIT-NOTFND.
04765      MOVE 'X'                    TO ERROR-SWITCH.
04766
04767  5060-EXIT.
04768      EXIT.
062217 5100-LOAD-CORR-TRLR.
062217     MOVE PI-COMPANY-CD      TO TRLR-COMPANY-CD.
062217     MOVE PI-CARRIER         TO TRLR-CARRIER.
062217     MOVE PI-CLAIM-NO        TO TRLR-CLAIM-NO.
062217     MOVE PI-CERT-NO         TO TRLR-CERT-NO.
062217     MOVE 1000               TO TRLR-SEQ-NO.
062217
062217     
      * EXEC CICS HANDLE CONDITION
062217*         NOTFND    (1052-DONE)
062217*         ENDFILE   (1052-DONE)
062217*    END-EXEC.
      *    MOVE '"$I''                  ! E #00013520' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4520233030303133353230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
062217
062217     
      * EXEC CICS STARTBR
062217*        DATASET (TRLR-FILE-ID)
062217*        RIDFLD  (TRLR-KEY)
062217*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00013525' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303133353235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062217
062217
062217     MOVE SPACES TO WS-TRLR-FILE-EOF
062217                    WS-AUTH-RCVD
062217     PERFORM 1052-READ-TRLR THRU 1052-EXIT
062217       UNTIL  TRLR-FILE-EOF
062217         OR WS-AUTH-RCVD > SPACES.
062217
062217     
      * EXEC CICS ENDBR
062217*        DATASET (TRLR-FILE-ID)
062217*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013537' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133353337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
062217
062217     IF AUTH-RCVD
062217        MOVE 'Y' TO AUTHRCVO
062217     ELSE
062217        MOVE 'N' TO AUTHRCVO
062217     END-IF.
062217
062217
062217 5100-EXIT.
062217     EXIT.
04769
04770      EJECT
04771  6000-CALCULATE-CERT-TERM.
04772
04773      IF CM-LF-PREMIUM-RATE NOT NUMERIC
04774          MOVE ZEROS              TO CM-LF-PREMIUM-RATE.
04775      IF CM-AH-PREMIUM-RATE NOT NUMERIC
04776          MOVE ZEROS              TO CM-AH-PREMIUM-RATE.
04777      IF CM-LOAN-APR NOT NUMERIC
04778          MOVE ZEROS              TO CM-LOAN-APR.
04779
04780 *    IF WS-CALC-METHOD  = 'G'
04781          PERFORM 6100-CALC-GROSS-TERM THRU 6200-EXIT
04782 *     ELSE
04783 *        PERFORM 6500-CALC-NET-TERM   THRU 6700-EXIT.
04784
04785      IF N GREATER 120
04786          MOVE 120                TO N.
04787
04788      MOVE '6'                    TO DC-OPTION-CODE.
04789      MOVE N                      TO DC-ELAPSED-MONTHS.
04790      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
04791      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
04792
04793      IF CM-LF-BENEFIT-CD NOT = ZERO
04794          MOVE N                  TO CM-LF-ORIG-TERM
04795          COMPUTE CM-LF-BENEFIT-AMT = PI-PAYMENT-AMT * N
04796          IF NO-CONVERSION-ERROR
04797              MOVE DC-BIN-DATE-2  TO CM-LF-LOAN-EXPIRE-DT.
04798
04799      IF CM-AH-BENEFIT-CD NOT = ZERO
04800          MOVE N                  TO CM-AH-ORIG-TERM
04801          IF NO-CONVERSION-ERROR
04802              MOVE DC-BIN-DATE-2  TO CM-AH-LOAN-EXPIRE-DT.
04803
04804  6000-EXIT.
04805       EXIT.
04806
04807  6100-CALC-GROSS-TERM.
04808      MOVE CM-LOAN-BALANCE         TO L.
04809      MOVE PI-PAYMENT-AMT          TO M.
04810
04811      COMPUTE N = L / M.
04812      IF N LESS 1
04813          MOVE 1  TO N.
04814
04815      IF CM-LF-BENEFIT-CD NOT = ZERO
04816          COMPUTE WS-LF-RATE = CM-LF-PREMIUM-RATE / +1000
04817      ELSE
04818          MOVE ZEROS               TO WS-LF-RATE.
04819
04820      IF CM-AH-BENEFIT-CD NOT = ZERO
04821          COMPUTE WS-AH-RATE = CM-AH-PREMIUM-RATE / +1000
04822      ELSE
04823          MOVE ZEROS               TO WS-AH-RATE.
04824
04825      COMPUTE I = CM-LOAN-APR / +1200.
04826
04827  6105-LOOP.
04828      IF N GREATER 240
04829          GO TO 6200-EXIT.
04830      PERFORM 6210-CALC-LEFT-RIGHTONE THRU 6220-EXIT.
04831      IF LEFT-TOT-1 GREATER RIGHT-TOT-1
04832          ADD 1 TO N
04833          GO TO 6105-LOOP.
04834
04835  6200-EXIT.
04836       EXIT.
04837
04838  6210-CALC-LEFT-RIGHTONE.
04839       MOVE L         TO LEFT-TOT-1.
04840       SUBTRACT 1 FROM N.
04841       PERFORM 6300-CALC-A-N THRU 6300-EXIT.
04842       PERFORM 6350-CALC-IA-N THRU 6400-EXIT.
04843       ADD 1 TO N.
04844  6211-CALC-TERM1.
04845       MOVE N         TO TERM1.
04846       MULTIPLY M BY TERM1.
04847  6212-LOOP.
04848       COMPUTE TERM1 = (WS-AH-RATE + WS-LF-RATE) * TERM1.
04849       ADD 1 TO A-N.
04850       MULTIPLY A-N BY TERM1.
04851       SUBTRACT 1 FROM A-N.
04852       ADD TERM1 TO LEFT-TOT-1.
04853  6213-CALC-TERM2.
04854       MOVE M         TO TERM2.
04855       COMPUTE TERM2 = (WS-AH-RATE + WS-LF-RATE) * TERM2.
04856       MULTIPLY IA-N BY TERM2.
04857       SUBTRACT TERM2 FROM LEFT-TOT-1.
04858  6215-CALC-RIGHTONE.
04859       MOVE M         TO RIGHT-TOT-1.
04860       PERFORM 6300-CALC-A-N THRU 6300-EXIT.
04861       MULTIPLY A-N BY RIGHT-TOT-1.
04862  6220-EXIT.
04863       EXIT.
04864
04865  6300-CALC-A-N.
04866      IF N LESS 1
04867          MOVE 0     TO A-N
04868          GO TO 6300-EXIT.
04869      IF I = 0
04870          MOVE .00001 TO I.
04871      ADD 1 TO I.
04872      DIVIDE I INTO 1 GIVING V.
04873      SUBTRACT 1 FROM I.
04874      PERFORM 6450-CALC-V-EX-N THRU 6490-EXIT.
04875      SUBTRACT V-EX-N FROM 1 GIVING TERM3.
04876      DIVIDE I INTO TERM3 GIVING A-N.
04877  6300-EXIT.
04878       EXIT.
04879
04880  6350-CALC-IA-N.
04881      IF N LESS 1
04882          MOVE 0      TO IA-N
04883          GO TO 6400-EXIT.
04884      ADD 1 TO N.
04885      PERFORM 6450-CALC-V-EX-N THRU 6490-EXIT.
04886      SUBTRACT 1 FROM N.
04887      MULTIPLY N BY V-EX-N GIVING TERM3.
04888      SUBTRACT TERM3 FROM A-N GIVING TERM3.
04889      SUBTRACT V FROM 1 GIVING TERM4.
04890      DIVIDE TERM4 INTO TERM3 GIVING IA-N.
04891  6400-EXIT.
04892       EXIT.
04893
04894  6450-CALC-V-EX-N.
04895      IF N LESS 1
04896          MOVE 1    TO V-EX-N
04897          GO TO 6490-EXIT.
04898      MOVE N        TO NV-STORE.
04899      IF V-EX-ONETIME = 1  OR
04900         V NOT = V-EXPONENT (1)
04901           PERFORM 6470-BUILD-V-EX-TABLE THRU 6480-EXIT.
04902  6460-LOOP.
04903      IF N GREATER 248
04904          MOVE 248 TO N.
04905      IF N = 1
04906          MOVE V               TO V-EX-N
04907       ELSE
04908          MOVE V-EXPONENT (N)  TO V-EX-N.
04909      GO TO 6490-EXIT.
04910
04911  6470-BUILD-V-EX-TABLE.
04912      MOVE 2      TO N.
04913      MOVE V      TO V-EXPONENT (1)
04914                     V-EX-N.
04915  6471-LOOP.
04916      MULTIPLY V BY V-EX-N.
04917      MOVE V-EX-N   TO V-EXPONENT (N).
04918      ADD 1 TO N.
04919      IF N LESS 248
04920          GO TO 6471-LOOP.
04921      MOVE NV-STORE     TO N.
04922      MOVE 0            TO V-EX-ONETIME.
04923  6480-EXIT.
04924       EXIT.
04925
04926  6490-EXIT.
04927       EXIT.
04928
04929  6500-CALC-NET-TERM.
04930      MOVE CM-LOAN-BALANCE         TO L.
04931      MOVE PI-PAYMENT-AMT          TO M.
04932
04933      DIVIDE L BY M GIVING N REMAINDER WS-REMAIN.
04934      IF WS-REMAIN GREATER ZERO
04935          ADD 1 TO N.
04936      IF N = 0
04937          MOVE 1 TO N.
04938
04939  6700-EXIT.
04940       EXIT.
04941
04942  6999-CALC-TERM-EXIT.
04943      EXIT.
04944
04945      EJECT
04946  7000-RESET-ATTRIBUTE.
04947
04948      MOVE AL-UANON               TO CERTEFFA  CERTACTA
04949                                     CERTSTA   CERTCARA
04950                                     CERTGRPA.
04951      MOVE -1                     TO CERTCARL.
04952
04953  7000-EXIT.
04954      EXIT.
04955
04957  7600-BROWSE-CLAIM.
04958
04959      
      * EXEC CICS READ
04960 *        DATASET(CLMS-FILE-ID)
04961 *        SET    (ADDRESS OF CLAIM-MASTER)
04962 *        RIDFLD (MSTR-KEY)
04963 *        GENERIC
04964 *        EQUAL
04965 *        KEYLENGTH(ELMSTR-GENERIC-LENGTH)
04966 *    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00013739' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133373339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 ELMSTR-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04967
04968      MOVE CL-CONTROL-PRIMARY     TO MSTR-KEY.
04969
032612     if cl-assoc-cert-total = 0
032612        move 1 to cl-assoc-cert-total
032612     end-if
032612     IF CL-ASSOC-CERT-SEQU = 0
032612        MOVE 1 TO CL-ASSOC-CERT-SEQU
032612     END-IF
032612
04970      IF CL-ASSOC-CERT-TOTAL = 1
04971          MOVE AL-SADOF           TO BCERT1A
04972                                     BSUFX1A
04973                                     BCERT2A
04974                                     BSUFX2A
04975                                     BCERT3A
04976                                     BSUFX3A
04977                                     BCERT4A
04978                                     BSUFX4A
04979                                     BCERT5A
04980                                     BSUFX5A
04981                                     PCERTNOA
04982                                     PSUFXA
04983                                     LABEL1A
04984                                     LABEL2A
04985          GO TO 7699-EXIT.
04986
04987  7610-BROWSE-CLAIM-LOOP.
04988
04989      MOVE LOW-VALUES             TO BCERT1O
04990                                     BSUFX1O
04991                                     BCERT2O
04992                                     BSUFX2O
04993                                     BCERT3O
04994                                     BSUFX3O
04995                                     BCERT4O
04996                                     BSUFX4O
04997                                     BCERT5O
04998                                     BSUFX5O.
04999
05000      
      * EXEC CICS HANDLE CONDITION
05001 *        ENDFILE  (7630-END-BROWSE)
05002 *    END-EXEC.
      *    MOVE '"$''                   ! F #00013787' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4620233030303133373837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05003
05004      
      * EXEC CICS STARTBR
05005 *        DATASET    (CLMS-FILE-ID)
05006 *        RIDFLD     (MSTR-KEY)
05007 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00013791' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303133373931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05008
05009      MOVE +1                     TO WS-ASSOCIATED-CERTS.
05010
05011  7620-READ-CLAIM-LOOP.
05012
05013      
      * EXEC CICS READNEXT
05014 *        DATASET   (CLMS-FILE-ID)
05015 *        SET       (ADDRESS OF CLAIM-MASTER)
05016 *        RIDFLD    (MSTR-KEY)
05017 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013800' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303133383030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05018
05019      IF CL-COMPANY-CD  NOT = PI-COMPANY-CD   OR
05020         CL-CARRIER     NOT = CARRI           OR
05021         CL-CLAIM-NO    NOT = CLAIMI
05022         GO TO 7630-END-BROWSE.
05023
05024      IF WS-ASSOCIATED-CERTS = 1
05025          MOVE CL-CERT-PRIME      TO BCERT1O
05026          MOVE CL-CERT-SFX        TO BSUFX1O.
05027
05028      IF WS-ASSOCIATED-CERTS = 2
05029          MOVE CL-CERT-PRIME      TO BCERT2O
05030          MOVE CL-CERT-SFX        TO BSUFX2O.
05031
05032      IF WS-ASSOCIATED-CERTS = 3
05033          MOVE CL-CERT-PRIME      TO BCERT3O
05034          MOVE CL-CERT-SFX        TO BSUFX3O.
05035
05036      IF WS-ASSOCIATED-CERTS = 4
05037          MOVE CL-CERT-PRIME      TO BCERT4O
05038          MOVE CL-CERT-SFX        TO BSUFX4O.
05039
05040      IF WS-ASSOCIATED-CERTS = 5
05041          MOVE CL-CERT-PRIME      TO BCERT5O
05042          MOVE CL-CERT-SFX        TO BSUFX5O
05043      ELSE
05044          ADD +1                  TO WS-ASSOCIATED-CERTS
05045          GO TO 7620-READ-CLAIM-LOOP.
05046
05047  7630-END-BROWSE.
05048
05049      
      * EXEC CICS ENDBR
05050 *        DATASET   (CLMS-FILE-ID)
05051 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013836' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133383336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05052
05053  7640-HIGHLIGHT-CERT-DISPLAYED.
05054
05055      MOVE AL-SANON               TO BCERT1A
05056                                     BSUFX1A
05057                                     BCERT2A
05058                                     BSUFX2A
05059                                     BCERT3A
05060                                     BSUFX3A
05061                                     BCERT4A
05062                                     BSUFX4A
05063                                     BCERT5A
05064                                     BSUFX5A.
05065
05066      IF BCERT1O = CERTI AND
05067         BSUFX1O = SUFXI
05068             MOVE AL-SABON        TO BCERT1A
05069                                     BSUFX1A.
05070
05071      IF BCERT2O = CERTI AND
05072         BSUFX2O = SUFXI
05073             MOVE AL-SABON        TO BCERT2A
05074                                     BSUFX2A.
05075
05076      IF BCERT3O = CERTI AND
05077         BSUFX3O = SUFXI
05078             MOVE AL-SABON        TO BCERT3A
05079                                     BSUFX3A.
05080
05081      IF BCERT4O = CERTI AND
05082         BSUFX4O = SUFXI
05083             MOVE AL-SABON        TO BCERT4A
05084                                     BSUFX4A.
05085
05086      IF BCERT5O = CERTI AND
05087         BSUFX5O = SUFXI
05088             MOVE AL-SABON        TO BCERT5A
05089                                     BSUFX5A.
05090
05091  7699-EXIT.
05092      EXIT.
05093
05094      EJECT
05095  7700-CHECK-SEQUENCE.
05096
05097      
      * EXEC CICS HANDLE CONDITION
05098 *        ENDFILE  (7799-EXIT)
05099 *        NOTFND   (7799-EXIT)
05100 *    END-EXEC.
      *    MOVE '"$''I                  ! G #00013884' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4720233030303133383834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05101
05102      
      * EXEC CICS READ
05103 *        DATASET(CLMS-FILE-ID)
05104 *        SET    (ADDRESS OF CLAIM-MASTER)
05105 *        RIDFLD (MSTR-KEY)
05106 *        GENERIC
05107 *        KEYLENGTH(ELMSTR-GENERIC-LENGTH)
05108 *    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00013889' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 ELMSTR-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05109
05110      COMPUTE WS-ASSOC-CERT-TOTAL =
05111              CL-ASSOC-CERT-TOTAL + ONE-OR-MIN1.
05112
05113      GO TO 7799-EXIT.
05114
05115  7710-RESEQUENCE-CLAIMS.
05116
05117      
      * EXEC CICS HANDLE CONDITION
05118 *        ENDFILE  (7799-EXIT)
05119 *    END-EXEC.
      *    MOVE '"$''                   ! H #00013904' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4820233030303133393034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05120
05121      
      * EXEC CICS STARTBR
05122 *        DATASET(CLMS-FILE-ID)
05123 *        RIDFLD (MSTR-KEY)
05124 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00013908' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303133393038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05125
05126      COMPUTE WS-ASSOC-CERT-SEQU =
05127              WS-ASSOC-CERT-SEQU + 1.
05128
05129      COMPUTE WS-READNEXT-SWITCH =
05130              WS-READNEXT-SWITCH + 1.
05131
05132  7720-READ-CLAIM-LOOP.
05133
05134      
      * EXEC CICS READNEXT
05135 *        DATASET(CLMS-FILE-ID)
05136 *        SET    (ADDRESS OF CLAIM-MASTER)
05137 *        RIDFLD (MSTR-KEY)
05138 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013921' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303133393231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05139
05140      IF WS-READNEXT-SWITCH = 1
05141          ADD 1                   TO WS-READNEXT-SWITCH
05142          GO TO 7720-READ-CLAIM-LOOP.
05143
05144  7730-END-BROWSE.
05145
05146      
      * EXEC CICS ENDBR
05147 *        DATASET(CLMS-FILE-ID)
05148 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013933' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133393333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05149
05150  7740-READ-CLAIM-UPDATE.
05151
05152      IF CL-COMPANY-CD  NOT = WS-SAVE-COMPANY-CD  OR
05153         CL-CARRIER     NOT = WS-SAVE-CARRIER     OR
05154         CL-CLAIM-NO    NOT = WS-SAVE-CLAIM-NO
05155         GO TO 7799-EXIT
05156      ELSE
05157         MOVE ZERO                TO WS-READNEXT-SWITCH.
05158
05159      
      * EXEC CICS READ
05160 *        DATASET(CLMS-FILE-ID)
05161 *        SET    (ADDRESS OF CLAIM-MASTER)
05162 *        RIDFLD (MSTR-KEY)
05163 *        UPDATE
05164 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00013946' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133393436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05165
062602     if (cl-priority-cd = '8')
062602        and (pi-processor-id not = 'PEMA' and 'JMS '
062602             AND 'AMWA')
062602        MOVE ER-8003             TO EMI-ERROR
062602        PERFORM 9900-ERROR-FORMAT
062602                                 THRU 9900-EXIT
062602        MOVE -1                  TO MAINTL
062602        GO TO 8110-SEND-DATA
062602     end-if
062602
062121     IF PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02826
05166      MOVE WS-ASSOC-CERT-TOTAL    TO CL-ASSOC-CERT-TOTAL.
05167      MOVE WS-ASSOC-CERT-SEQU     TO CL-ASSOC-CERT-SEQU.
05168
05169      
      * EXEC CICS REWRITE
05170 *         DATASET(CLMS-FILE-ID)
05171 *         FROM   (CLAIM-MASTER)
05172 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00013970' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303133393730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05173
05174      GO TO 7710-RESEQUENCE-CLAIMS.
05175
05176  7799-EXIT.
05177      EXIT.
05178
05179      EJECT
05180  7800-CHECK-AUTO-ACTIVITY.
05181
05182      
      * EXEC CICS HANDLE CONDITION
05183 *        NOTFND   (7800-NOT-FOUND)
05184 *    END-EXEC.
      *    MOVE '"$I                   ! I #00013983' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4920233030303133393833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05185
05186      MOVE PI-COMPANY-ID              TO  CNTL-CO-ID.
05187      MOVE 'T'                        TO  CNTL-REC-TYPE.
05188      MOVE SPACES                     TO  CNTL-PROC-ID.
05189      MOVE +0                         TO  CNTL-SEQ-NO.
05190
05191      
      * EXEC CICS READ
05192 *        DATASET   (CNTL-FILE-ID)
05193 *        RIDFLD    (CNTL-KEY)
05194 *        SET       (ADDRESS OF CONTROL-FILE)
05195 *    END-EXEC.
      *    MOVE '&"S        E          (   #00013992' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133393932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05196
05197      IF CL-ACTIVITY-CODE IS NOT NUMERIC
05198          MOVE ZEROS                  TO  CL-ACTIVITY-CODE.
05199
05200      IF CL-ACTIVITY-CODE NOT = ZEROS
05201          MOVE CL-ACTIVITY-CODE                 TO  MISC-SUB
05202          IF MISC-SUB IS GREATER THAN +9
05203              SUBTRACT +9 FROM MISC-SUB
05204              MOVE CF-USER-RESET-SW (MISC-SUB)  TO  WS-RESET-SW
05205          ELSE
05206              MOVE CF-SYS-RESET-SW  (MISC-SUB)  TO  WS-RESET-SW
05207      ELSE
05208          MOVE 'Y'                              TO  WS-RESET-SW.
05209
05210      IF STATUSI = 'C' OR 'CLOSED'
05211          IF CF-SYS-ACTIVE-SW (7) = ' ' OR 'N'
05212              MOVE 'N'                TO  PI-REC-FOUND-SW
05213                                          PI-LETTER-SW
05214              GO TO 7800-EXIT.
05215
05216      IF STATUSI = 'C' OR 'CLOSED'
05217          IF CF-SYS-LETTER-ID (7) = SPACES OR LOW-VALUES
05218              MOVE 'N'                TO  PI-LETTER-SW
05219          ELSE
05220              MOVE 'Y'                TO  PI-LETTER-SW.
05221
05222      IF STATUSI = 'O' OR 'OPEN'
05223          IF CF-SYS-ACTIVE-SW (8) = ' ' OR 'N'
05224              MOVE 'N'                TO  PI-REC-FOUND-SW
05225                                          PI-LETTER-SW
05226              GO TO 7800-EXIT.
05227
05228      IF STATUSI = 'O' OR 'OPEN'
05229          IF CF-SYS-LETTER-ID (8) = SPACES OR LOW-VALUES
05230              MOVE 'N'                TO  PI-LETTER-SW
05231          ELSE
05232              MOVE 'Y'                TO  PI-LETTER-SW.
05233
05234      MOVE 'Y'                        TO  PI-REC-FOUND-SW.
05235      GO TO 7800-EXIT.
05236
05237  7800-NOT-FOUND.
05238
05239      MOVE 'N'                        TO  PI-REC-FOUND-SW
05240                                          PI-LETTER-SW.
05241
05242  7800-EXIT.
05243      EXIT.
05244
05245  7850-AUTO-LETTER-WRITER.
05246
05247      MOVE LOW-VALUES                 TO  W-1523-LINKDATA.
05248      MOVE PROGRAM-INTERFACE-BLOCK    TO  W-1523-COMMON-PI-DATA.
05249
05250      IF STATUSI = 'C' OR 'CLOSED'
05251          MOVE CF-SYS-LETTER-ID (7)   TO  W-1523-FORM-NUMBER
05252      ELSE
05253          MOVE CF-SYS-LETTER-ID (8)   TO  W-1523-FORM-NUMBER.
05254
05255      IF STATUSI = 'C' OR 'CLOSED'
05256          IF CF-SYS-RESEND-DAYS (7) NOT = ZEROS
05257              MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1
05258              MOVE '6'                TO  DC-OPTION-CODE
05259              MOVE CF-SYS-RESEND-DAYS (7) TO  DC-ELAPSED-DAYS
05260              MOVE +0                 TO  DC-ELAPSED-MONTHS
05261              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
05262              IF NO-CONVERSION-ERROR
05263                  MOVE DC-BIN-DATE-2  TO  W-1523-RESEND-DATE
05264              ELSE
05265                  MOVE LOW-VALUES     TO  W-1523-RESEND-DATE.
05266
05267      IF STATUSI = 'C' OR 'CLOSED'
05268          IF CF-SYS-FOLLOW-UP-DAYS (7) NOT = ZEROS
05269              MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1
05270              MOVE '6'                TO  DC-OPTION-CODE
05271              MOVE CF-SYS-FOLLOW-UP-DAYS (7) TO  DC-ELAPSED-DAYS
05272              MOVE +0                 TO  DC-ELAPSED-MONTHS
05273              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
05274              IF NO-CONVERSION-ERROR
05275                  MOVE DC-BIN-DATE-2  TO  W-1523-FOLLOW-UP-DATE
05276              ELSE
05277                  MOVE LOW-VALUES     TO  W-1523-FOLLOW-UP-DATE.
05278
05279      IF STATUSI = 'O' OR 'OPEN'
05280          IF CF-SYS-RESEND-DAYS (8) NOT = ZEROS
05281              MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1
05282              MOVE '6'                TO  DC-OPTION-CODE
05283              MOVE CF-SYS-RESEND-DAYS (8) TO  DC-ELAPSED-DAYS
05284              MOVE +0                 TO  DC-ELAPSED-MONTHS
05285              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
05286              IF NO-CONVERSION-ERROR
05287                  MOVE DC-BIN-DATE-2  TO  W-1523-RESEND-DATE
05288              ELSE
05289                  MOVE LOW-VALUES     TO  W-1523-RESEND-DATE.
05290
05291      IF STATUSI = 'O' OR 'OPEN'
05292          IF CF-SYS-FOLLOW-UP-DAYS (8) NOT = ZEROS
05293              MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1
05294              MOVE '6'                TO  DC-OPTION-CODE
05295              MOVE CF-SYS-FOLLOW-UP-DAYS (8) TO  DC-ELAPSED-DAYS
05296              MOVE +0                 TO  DC-ELAPSED-MONTHS
05297              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
05298              IF NO-CONVERSION-ERROR
05299                  MOVE DC-BIN-DATE-2  TO  W-1523-FOLLOW-UP-DATE
05300              ELSE
05301                  MOVE LOW-VALUES     TO  W-1523-FOLLOW-UP-DATE.
05302
05303      
      * EXEC CICS LINK
05304 *        PROGRAM    (LINK-1523)
05305 *        COMMAREA   (W-1523-LINKDATA)
05306 *        LENGTH     (W-1523-COMM-LENGTH)
05307 *    END-EXEC.
      *    MOVE '."C                   (   #00014104' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-1523, 
                 W-1523-LINKDATA, 
                 W-1523-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05308
05309      IF W-1523-ERROR-CODE = ZEROS
05310          GO TO 7850-EXIT.
05311
05312      IF W-1523-FATAL-ERROR
05313          MOVE ER-0802                TO  EMI-ERROR
05314          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05315
05316      IF W-1523-ERROR-CODE = 0191
05317          MOVE ER-0803                TO  EMI-ERROR
05318          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05319
05320  7850-EXIT.
05321      EXIT.
05322
090821 7900-READ-BENEFIT.
090821
090821     move spaces                 to cntl-key
090821     move pi-company-id          to cntl-co-id
090821     if cl-claim-type = 'L' or 'O' or 'P'
090821        move cm-lf-benefit-cd    to cntl-benefit
090821                                    ws-ben-hold
090821        move '4'                 to cntl-rec-type
090821                                    ws-rec-type
090821     else
090821        move cm-ah-benefit-cd    to cntl-benefit
090821                                    ws-ben-hold
090821        move '5'                 to cntl-rec-type
090821                                    ws-rec-type
090821     end-if
090821     MOVE ZEROS                  TO CNTL-SEQ-NO
090821
090821     .
090821 7900-READ-FILE.
090821
090821     PERFORM 7975-READ-CNTL-GTEQ THRU 7975-EXIT
090821
090821     if not WS-RESP-NORMAL
090821        go to 7900-not-found
090821     end-if
090821
090821     IF (PI-COMPANY-ID NOT = CF-COMPANY-ID)  OR
090821        (WS-REC-TYPE   NOT = CF-RECORD-TYPE)
090821        GO TO 7900-NOT-FOUND
090821     END-IF
090821
090821     MOVE 1                      TO SUB
090821
090821     .
090821 7900-LOOP.
090821     IF SUB = 9
090821         GO TO 7900-NOT-FOUND
090821     END-IF
090821
090821     IF WS-BEN-HOLD <> CF-BENEFIT-CODE (SUB)
090821        ADD 1                    TO SUB
090821        GO TO 7900-LOOP
090821     END-IF
090821
090821     MOVE CF-SPECIAL-CALC-CD (SUB)  TO WS-SPECIAL-CALC-CD.
090821     if (ws-special-calc-cd = 'O')
090821                OR
090821        (pi-company-id = 'DCC' and cl-carrier = '7')
090821        set mob-cert to true
090821     end-if
090821     GO TO 7900-EXIT
090821
090821     .
090821 7900-NOT-FOUND.
090821     MOVE SPACES                 TO WS-special-calc-cd
090821
090821     .
090821 7900-EXIT.
090821      EXIT.
090821
090821 7975-READ-CNTL-GTEQ.
090821
090821     
      * EXEC CICS READ
090821*         DATASET   ('ELCNTL')
090821*         SET       (ADDRESS OF CONTROL-FILE)
090821*         RIDFLD    (CNTL-KEY)
090821*         resp      (ws-response)
090821*         GTEQ
090821*     END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00014186' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303134313836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
090821
090821 7975-EXIT.
090821      EXIT.
090821 7990-get-lo-hi-acct-dates.
090821
090821     MOVE PI-COMPANY-CD       TO ACCT-company-code
090821     MOVE PI-CARRIER          TO ACCT-CARRIER
090821     MOVE PI-GROUPING         TO ACCT-GROUP
090821     MOVE PI-STATE            TO ACCT-STATE
090821     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
090821     MOVE low-values          TO ACCT-date
090821     MOVE ACCT-KEY            TO SAVE-ACCT-KEY
090821
090821     move spaces              to ws-i-say-stop-ind
090821                                 ws-eracct-startbr-ind
090821                                 ws-acct-status
090821
090821     
      * EXEC CICS STARTBR
090821*         DATASET    ('ERACCT')
090821*         RIDFLD     (ACCT-KEY)
090821*         GTEQ
090821*         resp       (ws-response)
090821*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00014210' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303134323130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821
090821     if ws-resp-normal
090821        set eracct-browse-started to true
090821     end-if
090821
090821     perform until i-say-stop
090821        
      * EXEC CICS READNEXT
090821*          DATASET ('ERACCT')
090821*          RIDFLD  (ACCT-KEY)
090821*          SET     (ADDRESS OF ACCOUNT-MASTER)
090821*          resp    (WS-RESPONSE)
090821*       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00014222' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303134323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACCT-KEY, 
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
090821           AND save-acct-key(1:20) =
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
      *    MOVE '&2                    $   #00014247' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
090821     end-if
090821
090821     perform 7900-READ-BENEFIT   thru 7900-exit
090821
090821     .
090821 7990-exit.
090821     exit.
05324  8000-CREATE-DMO-REC.
05325      MOVE PI-COMPANY-CD          TO NOTE-COMP-CD.
05326      MOVE CL-CERT-KEY-DATA       TO NOTE-CERT-KEY.
05327      MOVE CL-CERT-NO             TO NOTE-CERT-NO.
05328
05329      
      * EXEC CICS HANDLE CONDITION
05330 *         NOTFND   (8000-NOTE-NOT-FOUND)
05331 *    END-EXEC.
      *    MOVE '"$I                   ! J #00014262' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4A20233030303134323632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05332
05333      
      * EXEC CICS READ
05334 *         DATASET(NOTE-FILE-ID)
05335 *         SET    (ADDRESS OF CERTIFICATE-NOTE)
05336 *         RIDFLD (NOTE-KEY)
05337 *    END-EXEC.
      *    MOVE '&"S        E          (   #00014266' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NOTE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05338
05339      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.
05340      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.
05341      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.
05342
05343      IF CL-CERT-GROUPING (5:2) = SPACES OR ZEROS
05344          MOVE 'CC'               TO DCT-PRODUCT-CODE
05345      ELSE
05346          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.
05347
05348      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.
05349      MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.
05350
05351      
      * EXEC CICS LINK
05352 *        PROGRAM    ('DLO006')
05353 *        COMMAREA   (DCT-COMMUNICATION-AREA)
05354 *        LENGTH     (WS-DCT-LENGTH)
05355 *    END-EXEC.
           MOVE 'DLO006' TO DFHEIV1
      *    MOVE '."C                   (   #00014284' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134323834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DCT-COMMUNICATION-AREA, 
                 WS-DCT-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05356
05357      IF  DCT-RETURN-CODE = 'OK'
05358          GO TO 8000-CONT.
05359
05360      IF DCT-RETURN-CODE = '01' OR '02'
05361          GO TO 8000-EXIT.
05362
05363      IF DCT-RETURN-CODE = '03'
05364          MOVE ER-0951            TO EMI-ERROR
05365          MOVE -1                 TO MAINTL
05366          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05367          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05368          GO TO 8110-SEND-DATA.
05369
05370      IF DCT-RETURN-CODE = '04'
05371          MOVE ER-0946            TO EMI-ERROR
05372          MOVE -1                 TO MAINTL
05373          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05374          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05375          GO TO 8110-SEND-DATA.
05376
05377      IF DCT-RETURN-CODE = '05'
05378          MOVE ER-0947            TO EMI-ERROR
05379          MOVE -1                 TO MAINTL
05380          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05381          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05382          GO TO 8110-SEND-DATA.
05383
05384      IF DCT-RETURN-CODE = '06'
05385          MOVE ER-0921            TO EMI-ERROR
05386          MOVE -1                 TO MAINTL
05387          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05388          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05389          GO TO 8110-SEND-DATA.
05390
05391      IF DCT-RETURN-CODE = '07'
05392          MOVE ER-0919            TO EMI-ERROR
05393          MOVE -1                 TO MAINTL
05394          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05395          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05396          GO TO 8110-SEND-DATA.
05397
05398      IF DCT-RETURN-CODE = '08'
05399          MOVE ER-0948            TO EMI-ERROR
05400          MOVE -1                 TO MAINTL
05401          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05402          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05403          GO TO 8110-SEND-DATA.
05404
05405      IF DCT-RETURN-CODE = 'N1'
05406          MOVE ER-0950            TO EMI-ERROR
05407          MOVE -1                 TO MAINTL
05408          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05409          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05410          GO TO 8110-SEND-DATA.
05411
05412      IF DCT-RETURN-CODE = 'E1'
05413          MOVE ER-0974            TO EMI-ERROR
05414          MOVE -1                 TO MAINTL
05415          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05416          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05417          GO TO 8110-SEND-DATA.
05418
05419      IF DCT-RETURN-CODE = 'E2'
05420          MOVE ER-0975            TO EMI-ERROR
05421          MOVE -1                 TO MAINTL
05422          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05423          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05424          GO TO 8110-SEND-DATA.
05425
05426      IF DCT-RETURN-CODE NOT = 'OK'
05427          MOVE ER-0949            TO EMI-ERROR
05428          MOVE -1                 TO MAINTL
05429          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05430          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05431          GO TO 8110-SEND-DATA.
05432
05433  8000-CONT.
05434
05435      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.
05436      MOVE 'CS'                   TO DM-RECORD-TYPE.
05437      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.
05438      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.
05439      MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.
05440      MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.
05441      MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.
05442      MOVE SAVE-DATE-CCYYMMDD     TO DM-STATUS-DATE.
05443
05444      MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.
05445      MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.
05446      MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.
05447      PERFORM 8050-FORMAT-LAST-NAME-1ST THRU 8050-EXIT.
05448      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.
05449
05450      IF STATUSI = 'OPEN' OR 'O'
05451          MOVE 'R'                TO DM-STAT-CHANGE-TYPE
05452       ELSE
05453          MOVE 'C'                TO DM-STAT-CHANGE-TYPE.
05454
05455      IF STATUSI = 'OPEN' OR 'O'
05456         IF CL-NO-OF-PMTS-MADE = 0
05457          MOVE '1'                TO DM-CLAIM-STATUS
05458       ELSE
05459          MOVE '2'                TO DM-CLAIM-STATUS.
05460
05461      IF STATUSI = 'CLOSED' OR 'C'
05462         IF NOT FINAL-PAID
05463          MOVE '3'                TO DM-CLAIM-STATUS
05464       ELSE
05465          MOVE '4'                TO DM-CLAIM-STATUS.
05466
05467      MOVE CL-CARRIER             TO DM-STAT-CARRIER.
05468
05469      
      * EXEC CICS LINK
05470 *        PROGRAM    ('DLO025')
05471 *        COMMAREA   (DMO-COMMUNICATION-AREA)
05472 *        LENGTH     (WS-DMO-LENGTH)
05473 *    END-EXEC.
           MOVE 'DLO025' TO DFHEIV1
      *    MOVE '."C                   (   #00014402' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DMO-COMMUNICATION-AREA, 
                 WS-DMO-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05474
05475      IF  DM-RETURN-CODE = 'OK'
05476          GO TO 8000-EXIT.
05477
05478      IF DM-RETURN-CODE = '01'
05479          MOVE ER-8051            TO EMI-ERROR
05480          MOVE -1                 TO MAINTL
05481          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05482          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05483          GO TO 8110-SEND-DATA.
05484
05485      IF DM-RETURN-CODE = '02'
05486          MOVE ER-8052            TO EMI-ERROR
05487          MOVE -1                 TO MAINTL
05488          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05489          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05490          GO TO 8110-SEND-DATA.
05491
05492      IF DM-RETURN-CODE = '03'
05493          MOVE ER-8053            TO EMI-ERROR
05494          MOVE -1                 TO MAINTL
05495          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05496          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05497          GO TO 8110-SEND-DATA.
05498
05499      IF DM-RETURN-CODE = '04'
05500          MOVE ER-8054            TO EMI-ERROR
05501          MOVE -1                 TO MAINTL
05502          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05503          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05504          GO TO 8110-SEND-DATA.
05505
05506      IF DM-RETURN-CODE = '05'
05507          MOVE ER-8055            TO EMI-ERROR
05508          MOVE -1                 TO MAINTL
05509          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05510          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05511          GO TO 8110-SEND-DATA.
05512
05513      IF DM-RETURN-CODE = '06'
05514          MOVE ER-8056            TO EMI-ERROR
05515          MOVE -1                 TO MAINTL
05516          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05517          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05518          GO TO 8110-SEND-DATA.
05519
05520      IF DM-RETURN-CODE = '07'
05521          MOVE ER-8057            TO EMI-ERROR
05522          MOVE -1                 TO MAINTL
05523          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05524          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05525          GO TO 8110-SEND-DATA.
05526
05527      IF DM-RETURN-CODE = '08'
05528          MOVE ER-8058            TO EMI-ERROR
05529          MOVE -1                 TO MAINTL
05530          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05531          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05532          GO TO 8110-SEND-DATA.
05533
05534      IF DM-RETURN-CODE = '09'
05535          MOVE ER-8059            TO EMI-ERROR
05536          MOVE -1                 TO MAINTL
05537          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05538          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05539          GO TO 8110-SEND-DATA.
05540
05541      IF DM-RETURN-CODE = '10'
05542          MOVE ER-8060            TO EMI-ERROR
05543          MOVE -1                 TO MAINTL
05544          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05545          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05546          GO TO 8110-SEND-DATA.
05547
05548      IF DM-RETURN-CODE = '11'
05549          MOVE ER-8061            TO EMI-ERROR
05550          MOVE -1                 TO MAINTL
05551          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05552          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05553          GO TO 8110-SEND-DATA.
05554
05555      IF DM-RETURN-CODE = '12'
05556          MOVE ER-8062            TO EMI-ERROR
05557          MOVE -1                 TO MAINTL
05558          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05559          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05560          GO TO 8110-SEND-DATA.
05561
05562      IF DM-RETURN-CODE = '13'
05563          MOVE ER-8063            TO EMI-ERROR
05564          MOVE -1                 TO MAINTL
05565          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05566          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05567          GO TO 8110-SEND-DATA.
05568
05569      IF DM-RETURN-CODE = '14'
05570          MOVE ER-8064            TO EMI-ERROR
05571          MOVE -1                 TO MAINTL
05572          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05573          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05574          GO TO 8110-SEND-DATA.
05575
05576      IF DM-RETURN-CODE = '15'
05577          MOVE ER-8065            TO EMI-ERROR
05578          MOVE -1                 TO MAINTL
05579          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05580          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05581          GO TO 8110-SEND-DATA.
05582
05583      IF DM-RETURN-CODE = '16'
05584          MOVE ER-8154            TO EMI-ERROR
05585          MOVE -1                 TO MAINTL
05586          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05587          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05588          GO TO 8110-SEND-DATA.
05589
05590      IF DM-RETURN-CODE = '17'
05591          MOVE ER-8155            TO EMI-ERROR
05592          MOVE -1                 TO MAINTL
05593          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05594          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05595          GO TO 8110-SEND-DATA.
05596
05597      IF DM-RETURN-CODE = 'N1'
05598          MOVE ER-8152            TO EMI-ERROR
05599          MOVE -1                 TO MAINTL
05600          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05601          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05602          GO TO 8110-SEND-DATA.
05603
05604      IF DM-RETURN-CODE = 'E1'
05605          MOVE ER-8153            TO EMI-ERROR
05606          MOVE -1                 TO MAINTL
05607          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05608          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
05609          GO TO 8110-SEND-DATA.
05610
05611      MOVE ER-8066                TO EMI-ERROR.
05612      MOVE -1                     TO MAINTL.
05613      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05614      PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT.
05615      GO TO 8110-SEND-DATA.
05616
05617  8000-NOTE-NOT-FOUND.
05618      
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC.
      *    MOVE '6"R                   !   #00014551' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303134353531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05619      MOVE ER-0954                TO EMI-ERROR.
05620      MOVE -1                     TO MAINTL.
05621      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05622      GO TO 8110-SEND-DATA.
05623
05624  8000-EXIT.
05625      EXIT.
05626  EJECT
05627  8050-FORMAT-LAST-NAME-1ST.
05628 *****************************************************************
05629 *             M O V E   N A M E   R O U T I N E                 *
05630 *                                                               *
05631 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
05632 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
05633 *     FIELDS IN THE FOLLOWING WORKING-STORAGE FIELDS.           *
05634 *                                                               *
05635 *                  FIELD               VALUE                    *
05636 *                  -----               -----                    *
05637 *           W-NAME-LAST    (CL15)      SMITH                    *
05638 *           W-NAME-FIRST   (CL15)      JOHN                     *
05639 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
05640 *                                                               *
05641 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK WILL CONTAIN       *
05642 *                SMITH, JOHN ALLEN                              *
05643 *                     OR                                        *
05644 *                SMITH, JOHN A.                                 *
05645 *                                                               *
05646 *     TO USE THIS ROUTINE YOU NEED THE WORKING-STORAGE          *
05647 *     COPYBOOK, ELCNWA.                                         *
05648 *****************************************************************.
05649
05650      MOVE SPACES                 TO WS-NAME-WORK-AREA.
05651      MOVE ZERO                   TO WS-NAME-SW.
05652      SET NWA-INDEX               TO +1.
05653
05654      IF W-NAME-LAST   = SPACES  AND
05655         W-NAME-MIDDLE = SPACES
05656           MOVE +1                TO WS-NAME-SW.
05657
05658      MOVE W-NAME-LAST            TO WS-NAME-WORK2.
05659      PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
05660
05661      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
05662      PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
05663
05664      SET NWA-INDEX UP BY +1.
05665
05666      IF W-NAME-MIDDLE NOT = SPACES
05667          IF W-NAME-MIDDLE-2 = SPACES
05668              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
05669              SET NWA-INDEX UP BY +1
05670              MOVE '.'            TO WS-NW (NWA-INDEX)
05671              SET NWA-INDEX UP BY +2
05672          ELSE
05673              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
05674              PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
05675
05676  8050-EXIT.
05677      EXIT.
05678
05679  EJECT
05680  8060-MOVE-NAME.
05681      IF WS-NAME-SW GREATER THAN +1
05682          GO TO 8060-EXIT.
05683
05684      IF WS-NAME-WORK2 = SPACES
05685          GO TO 8060-EXIT.
05686
05687      SET NWA-INDEX2            TO +1.
05688      SET NWA-INDEX3            TO +2.
05689
05690  8060-MOVE-NAME-CYCLE.
05691      MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
05692
05693      IF NWA-INDEX LESS THAN +30
05694          SET NWA-INDEX UP BY +1
05695      ELSE
05696          ADD +2 TO  WS-NAME-SW
05697          GO TO 8060-EXIT.
05698
05699      IF NWA-INDEX2 LESS THAN +20
05700          SET NWA-INDEX3 UP BY +1
05701          SET NWA-INDEX2 UP BY +1.
05702
05703      IF WS-NW2 (NWA-INDEX2) = SPACES  AND
05704         WS-NW2 (NWA-INDEX3) = SPACES
05705          IF WS-NAME-SW = ZERO
05706              MOVE ','            TO WS-NW (NWA-INDEX)
05707              SET NWA-INDEX UP BY +2
05708              MOVE +1             TO WS-NAME-SW
05709              GO TO 8060-EXIT
05710          ELSE
05711              GO TO 8060-EXIT.
05712
05713      GO TO 8060-MOVE-NAME-CYCLE.
05714
05715  8060-EXIT.
05716      EXIT.
05717
05718      EJECT
05719
05720  8070-UNLOCK-CLAIM-MSTR.
05721      
      * EXEC CICS UNLOCK
05722 *        DATASET   (CLMS-FILE-ID)
05723 *    END-EXEC.
      *    MOVE '&*                    #   #00014654' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05724
05725  8070-EXIT.
05726       EXIT.
05727
05728      EJECT
05729  8100-SEND-MAP.
05730      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
05731
           move al-uanon               to accswa
05732      IF NOT PI-NO-CARRIER-SECURITY
05733         MOVE AL-SANOF            TO CERTCARA.
05734      IF NOT PI-NO-ACCOUNT-SECURITY
05735         MOVE AL-SANOF            TO CERTACTA.
05736
05737      IF PI-PROCESSOR-ID = 'LGXX'
05738         MOVE AL-UANON            TO CCNOA.
05739
05740      IF PI-COMPANY-ID = 'DMD'
05741         NEXT SENTENCE
05742      ELSE
05743         MOVE AL-SADOF            TO PFKEY6A.
081817     IF NOT (PI-COMPANY-ID = 'DCC' OR 'VPP')
081817        MOVE AL-SANOF TO EXTENSA
081817     END-IF
05744
05745      IF PI-USES-PAID-TO
05746         MOVE 'PAID TO  :' TO PTHRHDGO.
05747
05748      
      * EXEC CICS SEND
05749 *         MAP     ('EL131A')
05750 *         MAPSET  ('EL131S')
05751 *         ERASE
05752 *         FREEKB
05753 *         CURSOR
05754 *    END-EXEC.
           MOVE 'EL131A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL131S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00014685' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303134363835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL131AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05755
05756      GO TO 9000-RETURN-TRANS.
05757
05758  8110-SEND-DATA.
05759      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
05760
05761      IF PI-USES-PAID-TO
05762         MOVE 'PAID TO  :' TO PTHRHDGO.
05763
05764      IF PI-PROCESSOR-ID = 'LGXX'
05765         MOVE AL-UANON            TO CCNOA.
05766
05767      IF PI-COMPANY-ID = 'DMD'
05768         NEXT SENTENCE
05769      ELSE
05770         MOVE AL-SADOF            TO PFKEY6A.
05771
05772      
      * EXEC CICS SEND
05773 *         MAP      ('EL131A')
05774 *         MAPSET   ('EL131S')
05775 *         DATAONLY
05776 *         FREEKB
05777 *         CURSOR
05778 *    END-EXEC.
           MOVE 'EL131A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL131S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00014709' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303134373039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL131AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05779
05780      GO TO 9000-RETURN-TRANS.
05781
05782  8120-FORMAT-TIME-DATE.
052113     perform 8140-check-for-prev-errors
052113                                 thru 8140-exit
05784      IF SKIP-ATTRIBUTE = 'Y'
05785         MOVE SPACES              TO SKIP-ATTRIBUTE
05786         MOVE ER-0598             TO EMI-ERROR
05787         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05788         GO TO 8125-SKIP-ATTRIBUTE.
05789
032613*     IF PI-COMPANY-ID = 'DMD'
032613      IF PI-PROCESSOR-ID <> 'EMER'
05791          MOVE AL-PANOF           TO CERTEFFA  CERTACTA  CERTSTA
032613                                    CERTCARA  CERTGRPA CERTA SUFXA
05793
05794      MOVE AL-PANOF               TO CRTLNMEA  CRTFNMEA  CRTINITA
05795                                     ISSAGEA   JNTLNMEA  JNTFNMEA
05796                                     JNTINITA  JNTAGEA   APRA
05797                                     LCVCDA    LCVOTRMA  LCVRTRMA
05798                                     LCVBENEA  LCVFORMA  LCVCNDTA
05799                                     LCVEXITA  LCVSTATA  LCVKINDA
05800                                     ACVCDA    ACVOTRMA  ACVRTRMA
05801                                     ACVBENEA  ACVFORMA  ACVCNDTA
05802                                     ACVEXITA  ACVSTATA  ACVKINDA
05803                                     PMTFREQA  INDGRPA   PREMTYPA
05804                                     REINCDA   LCVRATEA  ACVRATEA
05805                                     ADDONDTA.
05806
121802*    IF PI-PROCESSOR-ID NOT = 'LGXX'
121802*       IF PI-COMPANY-ID = 'DMD'
121802*          MOVE AL-SANOF         TO TYPEA     CERTA     CERTSTA
121802*                                   SUFXA
121802*                                   PDAMTA    NODAYSA   NOPMTSA
121802*                                   INCA      OCCA
121802*          IF NOT SYSTEM-MODIFY-CAP
121802*              MOVE AL-SANOF         TO PDTHRUA.
05815
05816      IF PI-COMPANY-ID NOT = 'CRI' AND 'PEM' AND 'NCL'
05817          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA.
05818
05819  8125-SKIP-ATTRIBUTE.
05820
05821      MOVE SAVE-DATE              TO DATEO.
05822      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
05823 *    END-EXEC
      *    MOVE '0"A                   "   #00014761' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303134373631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
05824      
      * EXEC CICS FORMATTIME
05825 *              ABSTIME(LCP-CICS-TIME)
05826 *              TIME(LCP-TIME-OF-DAY-XX)
05827 *    END-EXEC
      *    MOVE 'j$(     (             #   #00014763' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303134373633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
05828      MOVE  LCP-TIME-OF-DAY-68 TO TIME-IN.
05829      MOVE UN-HOURS               TO FOR-HOURS.
05830      MOVE UN-MINUTES             TO FOR-MINUTES.
05831      MOVE TIME-OUT               TO TIMEO.
101501     MOVE PI-COMPANY-ID          TO COMPO.
101501     MOVE PI-PROCESSOR-ID        TO USERIDO.
05832      MOVE MAP-ID                 TO PI-CURRENT-SCREEN-NO.
05833      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
05834      MOVE EMI-MESSAGE-AREA (1)   TO MSG1O.
052113     MOVE EMI-MESSAGE-AREA (2)   TO MSG2O.
05835
05836  8130-EXIT.
05837      EXIT.
05838
052113 8140-check-for-prev-errors.
052113
052113     MOVE PI-COMPANY-CD          TO trlr-company-cd
052113     MOVE PI-CARRIER             TO trlr-carrier
052113     MOVE PI-CLAIM-NO            TO trlr-claim-no
052113     MOVE PI-CERT-NO             TO trlr-cert-no
052113     MOVE +95                    TO TRLR-SEQ-NO
052113
052113     
      * EXEC CICS READ
052113*       DATASET  ('ELTRLR')
052113*       SET      (ADDRESS OF ACTIVITY-TRAILERS)
052113*       RIDFLD   (TRLR-KEY)
052113*       RESP     (WS-RESPONSE)
052113*    END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00014789' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303134373839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
052113
052113     if ws-resp-normal
052113        perform varying s1 from +1 by +1 until
052113           at-note-error-no (s1) = spaces
052113           if at-note-error-no (s1) = '1652'
052113              continue
052113           else
052113              move at-note-error-no (s1)
052113                                 to emi-error
052113              move -1 to maintl
052113              if at-note-error-no (s1) = '1653'
052113                 evaluate true
052113                    when pi-claim-type = 'L'
052113                       move '  LF  '
052113                        to emi-claim-type
052113                    when pi-claim-type = 'I'
052113                       move '  IU  '
052113                        to emi-claim-type
052614                    WHEN PI-CLAIM-TYPE = 'F'
052614                       MOVE '  FL  '
052614                        TO EMI-CLAIM-TYPE
080322                    WHEN PI-CLAIM-TYPE = 'B'
080322                       MOVE '  BR  '
080322                        TO EMI-CLAIM-TYPE
080322                    WHEN PI-CLAIM-TYPE = 'H'
080322                       MOVE '  HS  '
080322                        TO EMI-CLAIM-TYPE
100518                    WHEN PI-CLAIM-TYPE = 'O'
100518                       MOVE '  OT  '
100518                        TO EMI-CLAIM-TYPE
052113                    when other
052113                       move '  AH  '
052113                        to emi-claim-type
052113                 end-evaluate
052113              end-if
052113              PERFORM 9900-ERROR-FORMAT
052113                                 THRU 9900-EXIT
052113           end-if
052113*          if at-note-error-no (s1) = '1653'
052113*             perform varying emi-sub from 1 by 1 until
052113*                emi-sub > 3
052113*                if emi-error-number (emi-sub) = '1653'
052113*                   evaluate true
052113*                      when pi-claim-type = 'L'
052113*                         move '  LF  '
052113*                          to emi-claim-type
052113*                      when pi-claim-type = 'I'
052113*                         move '  IU  '
052113*                          to emi-claim-type
052113*                      when other
052113*                         move '  AH  '
052113*                          to emi-claim-type
052113*                   end-evaluate
052113*                end-if
052113*             end-perform
052113*          end-if
052113        end-perform
052113     else
052113        display ' resp not normal ' ws-response ' '
052113        trlr-key (2:19)
052113     end-if
052113
052113     .
052113 8140-exit.
052113     exit.
05839  8150-ENTERED-CLAIM-NOTFOUND.
05840
05841      MOVE ER-0204                TO EMI-ERROR.
05842      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05843      MOVE -1                     TO MAINTL.
05844      GO TO 8100-SEND-MAP.
05845
05846  8200-RETURN-PRIOR.
05847      MOVE SPACE                  TO PI-RETURN-CD-1.
05848      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.
05849      GO TO 9200-XCTL.
05850
05851  8300-GET-HELP.
05852      MOVE XCTL-EL010             TO CALL-PGM.
05853      GO TO 9200-XCTL.
05854
05855  8400-RETURN-MASTER.
05856      MOVE SPACE                  TO PI-RETURN-CD-1.
05857      MOVE XCTL-EL126             TO CALL-PGM.
05858      GO TO 9200-XCTL.
05859
05860  8500-TRLR-MNT.
05861      IF PI-RETURN-CD-1 = 'X'
05862          MOVE ER-0311            TO EMI-ERROR
05863          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05864          MOVE -1                 TO MAINTL
05865          GO TO 8110-SEND-DATA.
05866
05867      
      * EXEC CICS WRITEQ TS
05868 *         QUEUE     (PI-KEY)
05869 *         FROM      (PROGRAM-INTERFACE-BLOCK)
05870 *         LENGTH    (PI-COMM-LENGTH)
05871 *    END-EXEC.
      *    MOVE '*"     L              ''   #00014888' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303134383838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05872
05873      MOVE XCTL-EL142             TO CALL-PGM.
05874      GO TO 9200-XCTL.
05875
05876  8600-ADDR-MNT.
05877      IF PI-RETURN-CD-1 = 'X'
05878          MOVE ER-0311            TO EMI-ERROR
05879          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05880          MOVE -1                 TO MAINTL
05881          GO TO 8110-SEND-DATA.
05882
05883      
      * EXEC CICS WRITEQ TS
05884 *         QUEUE     (PI-KEY)
05885 *         FROM      (PROGRAM-INTERFACE-BLOCK)
05886 *         LENGTH    (PI-COMM-LENGTH)
05887 *    END-EXEC.
      *    MOVE '*"     L              ''   #00014904' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303134393034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05888
05889      MOVE XCTL-EL141             TO CALL-PGM.
05890      GO TO 9200-XCTL.
05891
05892  8700-CERT-MNT.
05893      IF PI-RETURN-CD-1 = 'X'
05894          MOVE ER-7690            TO EMI-ERROR
05895          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05896          MOVE -1                 TO MAINTL
05897          GO TO 8110-SEND-DATA.
05898
05899      
      * EXEC CICS WRITEQ TS
05900 *         QUEUE     (PI-KEY)
05901 *         FROM      (PROGRAM-INTERFACE-BLOCK)
05902 *         LENGTH    (PI-COMM-LENGTH)
05903 *    END-EXEC.
      *    MOVE '*"     L              ''   #00014920' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303134393230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05904
05905      MOVE XCTL-EL1273            TO CALL-PGM.
05906      GO TO 9200-XCTL.
05907
082013 8725-BENEFICIARY-MNT.
082013     IF PI-RETURN-CD-1 = 'X'
082013         MOVE ER-0311            TO EMI-ERROR
082013         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
082013         MOVE -1                 TO MAINTL
082013         GO TO 8110-SEND-DATA.
082013
082013     
      * EXEC CICS WRITEQ TS
082013*         QUEUE     (PI-KEY)
082013*         FROM      (EL131AI)
082013*         LENGTH    (MAP-LENGTH)
082013*    END-EXEC.
      *    MOVE '*"     L              ''   #00014936' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303134393336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 EL131AI, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
082013
082013     
      * EXEC CICS WRITEQ TS
082013*         QUEUE     (PI-KEY)
082013*         FROM      (PROGRAM-INTERFACE-BLOCK)
082013*         LENGTH    (PI-COMM-LENGTH)
082013*    END-EXEC.
      *    MOVE '*"     L              ''   #00014942' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303134393432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
082013
082013     MOVE XCTL-EL114             TO CALL-PGM.
082013     GO TO 9200-XCTL.
082013
05908  8750-DMD-CLM-FIX.
05909      
      * EXEC CICS WRITEQ TS
05910 *         QUEUE     (PI-KEY)
05911 *         FROM      (PROGRAM-INTERFACE-BLOCK)
05912 *         LENGTH    (PI-COMM-LENGTH)
05913 *    END-EXEC.
      *    MOVE '*"     L              ''   #00014952' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303134393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05914
05915      MOVE XCTL-EL400DMD          TO CALL-PGM.
05916      GO TO 9200-XCTL.
05917
05918  8800-UNAUTHORIZED-ACCESS.
05919      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
05920      GO TO 8990-SEND-TEXT.
05921
05922  8810-PF23-ENTERED.
05923      MOVE EIBAID                 TO PI-ENTRY-CD-1.
05924      MOVE XCTL-EL005             TO CALL-PGM.
05925      GO TO 9200-XCTL.
05926
05927  8820-XCTL-ERROR.
05928      
      * EXEC CICS HANDLE CONDITION
05929 *        PGMIDERR (8990-SEND-TEXT)
05930 *    END-EXEC.
      *    MOVE '"$L                   ! K #00014971' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4B20233030303134393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05931
05932      MOVE SPACE                 TO PI-ENTRY-CD-1.
05933      MOVE CALL-PGM              TO PI-CALLING-PROGRAM  LOGOFF-PGM
05934      MOVE PGMIDERR-MSG          TO LOGOFF-FILL.
05935      MOVE XCTL-EL005            TO CALL-PGM.
05936      GO TO 9200-XCTL.
05937
05938  8990-SEND-TEXT.
05939      
      * EXEC CICS SEND TEXT
05940 *         FROM      (LOGOFF-TEXT)
05941 *         LENGTH    (LOGOFF-LENGTH)
05942 *         ERASE
05943 *         FREEKB
05944 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00014982' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303134393832' TO DFHEIV0(25:11)
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
           
05945
05946      GO TO 9100-RETURN-CICS.
05947
05948      EJECT
05949  9000-RETURN-TRANS.
05950      
      * EXEC CICS RETURN
05951 *         TRANSID   (TRANS-ID)
05952 *         COMMAREA  (PROGRAM-INTERFACE-BLOCK)
05953 *         LENGTH    (PI-COMM-LENGTH)
05954 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00014993' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303134393933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05955
05956      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL131' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
05957
05958  9100-RETURN-CICS.
05959      
      * EXEC CICS RETURN
05960 *    END-EXEC.
      *    MOVE '.(                    ''   #00015002' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05961
05962      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL131' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
05963
05964  9200-XCTL.
05965      
      * EXEC CICS XCTL
05966 *         PROGRAM    (CALL-PGM)
05967 *         COMMAREA   (PROGRAM-INTERFACE-BLOCK)
05968 *         LENGTH     (PI-COMM-LENGTH)
05969 *    END-EXEC.
      *    MOVE '.$C                   %   #00015008' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05970
05971  9700-LINK-RTRM-FILE-ID.
05972      
      * EXEC CICS LINK
05973 *        PROGRAM  (RTRM-FILE-ID)
05974 *        COMMAREA (CALCULATION-PASS-AREA)
05975 *        LENGTH   (CP-COMM-LENGTH)
05976 *    END-EXEC.
      *    MOVE '."C                   (   #00015015' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RTRM-FILE-ID, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05977
05978  9700-EXIT.
05979       EXIT.
05980
05981  9800-CONVERT-DATE.
05982      MOVE SPACES TO DC-ERROR-CODE.
05983      
      * EXEC CICS LINK
05984 *         PROGRAM    (DATE-CONV)
05985 *         COMMAREA   (DATE-CONVERSION-DATA)
05986 *         LENGTH     (DC-COMM-LENGTH)
05987 *    END-EXEC.
      *    MOVE '."C                   (   #00015026' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DATE-CONV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05988
05989  9800-EXIT.
05990      EXIT.
05991
05992  9900-ERROR-FORMAT.
05993      
      * EXEC CICS LINK
05994 *         PROGRAM    (XCTL-EL001)
05995 *         COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
05996 *         LENGTH     (EMI-COMM-LENGTH)
05997 *    END-EXEC.
      *    MOVE '."C                   (   #00015036' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 XCTL-EL001, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05998
05999  9900-EXIT.
06000      EXIT.
06001
06002  9990-ABEND.
06003      MOVE DFHEIBLK               TO EMI-LINE1.
06004      
      * EXEC CICS LINK
06005 *         PROGRAM   (XCTL-EL004)
06006 *         COMMAREA  (EMI-LINE1)
06007 *         LENGTH    (72)
06008 *     END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00015047' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 XCTL-EL004, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06009
06010      MOVE -1                     TO  MAINTL
06011
06012      GO TO 8110-SEND-DATA.
06013
06014  9995-SECURITY-VIOLATION.
06015 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00015075' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135303735' TO DFHEIV0(25:11)
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
06016

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL131' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-XCTL-ERROR,
                     8150-ENTERED-CLAIM-NOTFOUND,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0130-TS-ERROR,
                     0130-TS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0600-CONTINUE-NAME-UPDATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1004-BENE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1052-DONE,
                     1052-DONE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1120-CNTL-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1150-NOTOPEN-ERROR,
                     1150-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1490-STATE-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 1500-CARR-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 1620-ACCT-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 2020-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 2030-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 2040-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 2050-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 2060-CERT-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 2060-DUPKEY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 2070-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 2070-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 2080-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 2400-TRLR-NOTFND,
                     2410-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 2425-NINETY-NOTFND,
                     2425-NINETY-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 2430-EXIT,
                     2440-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 2440-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 2460-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 2475-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 2620-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 2650-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 2800-EXIT,
                     2800-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 3010-DEL-ACTQ,
                     3040-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 3010-NO-ACTIVITY,
                     3050-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 3010-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 3020-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 34
               GO TO 3060-EXIT,
                     3060-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 35
               GO TO 5000-TRLR-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 36
               GO TO 5000-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 37
               GO TO 1052-DONE,
                     1052-DONE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 38
               GO TO 7630-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 39
               GO TO 7799-EXIT,
                     7799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 40
               GO TO 7799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 41
               GO TO 7800-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 42
               GO TO 8000-NOTE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 43
               GO TO 8990-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL131' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
