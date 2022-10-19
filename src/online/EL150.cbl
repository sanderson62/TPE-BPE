00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL150 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 05/09/95 16:19:31.
00007 *                            VMOD=2.053
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00024 *REMARKS.    TRANSACTION - EX23 - STATUS DISPLAY AND DISPOSITION
00023 *
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101501*                              ADJUST REDEFINES EL150AI FILLER
062602* 062602    2002030700006  PEMA  Add note type of 'S'
062602*                                  (special review)
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST CALC
050107* 050107    2007041300002  AJRA  PREVENT CHECK FOR NON COVERED CLA
080106* 080106    2006052500001  AJRA  ADD NOTE TYPE N
041807* 041807    2006032200004  AJRA  ADD NOTE TYPE R
082707* 082707    2007032100001  PEMA  ADDITIONAL INTEREST CHANGES
102809* 102809    2008100900003  AJRA  PF19 TO NEW CERT NOTES SCREEN
042110* 042110  CR2008100900001  PEMA  ADD DENIAL TYPE
113010* 113010    2009122800001  AJRA  DISPLAY STOP DATE
061511* 061511    2011042000002  AJRA  VERIFY 2ND BENEFICIARY SSN
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
020613* 020613  CR2012092400007  AJRA  VERIFY CAUSAL STATE
041613* 041613  CR2013031200002  AJRA  ADD MAIL RECEIVED ACTION TYPE
061013* 061013    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
031714* 031714    2014031200001  AJRA  ALLOW LEVEL 4 & 5 TO UPDATE TOT I
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  Add pct of benefit funcionality
021615* 021615  CR2014062700002  PEMA  ADD XCTL TO EL1504
010816* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
040416* 040416  CR2016021500002  TANA  ADD PF21 XCTL TO EL1284
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD
082218* 082218  CR2018051400001  TANA  Hold and Pay
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
102418* 102418  CR2018083000001  TANA  ADD NEW CALL TYPE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
080322* 080322  CR2021100800003  TANA  Add B and H claim types
101501******************************************************************
00025
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL150 WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.053 *********'.
061013 77  a1                          pic s999 comp-3 value +0.
061013 77  p1                          pic s999 comp-3 value +0.
100314 77  ws-work-ben-pct             pic s9v999 comp-3 value +0.
080322 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
080322 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.
080322 77  ws-MAX-TOT-BEN              pic s9(7)v99 comp-3 value +0.
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
CIDMOD 01  TS-AREA.
CIDMOD     05  TS-PI-AREA          PIC X(1024) VALUE LOW-VALUES.
CIDMOD     05  TS-PI-SAVE-CLOAN    PIC X(25)   VALUE LOW-VALUES.
00040  01  WS-DATE-AREA.
00041      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00042      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00043
061013 01  ws-save-error-interface-block pic x(400) value low-values.
00044  01  STANDARD-AREAS.
CIDMOD     12  TS-LENGTH           PIC S9(4)  COMP VALUE +1049.
00045      12  WS-EARNING-METHOD   PIC X      VALUE SPACES.
00046      12  WS-INDEX            PIC S9(04) COMP VALUE +0.
00047      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
061013     12  crtt-map            pic x(8)    value 'EL150T'.
061013     12  SAD-MAP             PIC X(8)    VALUE 'EL150A'.
00048      12  MAP-NAME            PIC X(8)    VALUE 'EL150A'.
00049      12  MAPSET-NAME         PIC X(8)    VALUE 'EL150S'.
00050      12  TRANS-ID            PIC X(4)    VALUE 'EX23'.
00051      12  THIS-PGM            PIC X(8)    VALUE 'EL150'.
00052      12  START-TRANS-ID      PIC X(4)    VALUE 'EX58'.
00053      12  PGM-NAME            PIC X(8).
00054      12  TIME-IN             PIC S9(7).
00055      12  TIME-OUT-R  REDEFINES TIME-IN.
00056          16  FILLER          PIC X.
00057          16  TIME-OUT        PIC 99V99.
00058          16  FILLER          PIC XX.
00059      12  XCTL-005            PIC X(8)    VALUE 'EL005'.
00060      12  XCTL-010            PIC X(8)    VALUE 'EL010'.
00061      12  XCTL-126            PIC X(8)    VALUE 'EL126'.
00062      12  XCTL-1273           PIC X(8)    VALUE 'EL1273'.
00063      12  XCTL-EM1273         PIC X(8)    VALUE 'EM1273'.
00064      12  XCTL-131            PIC X(8)    VALUE 'EL131'.
00065      12  XCTL-132            PIC X(8)    VALUE 'EL132'.
00066      12  XCTL-EM131          PIC X(8)    VALUE 'EM131'.
00067      12  XCTL-141            PIC X(8)    VALUE 'EL141'.
00068      12  XCTL-142            PIC X(8)    VALUE 'EL142'.
00069      12  XCTL-1501           PIC X(8)    VALUE 'EL1501'.
00070      12  XCTL-1502           PIC X(8)    VALUE 'EL1502'.
00071      12  XCTL-151            PIC X(8)    VALUE 'EL151'.
00072      12  XCTL-152            PIC X(8)    VALUE 'EL152'.
00073      12  XCTL-153            PIC X(8)    VALUE 'EL153'.
00074      12  XCTL-154            PIC X(8)    VALUE 'EL154'.
00075      12  XCTL-156            PIC X(8)    VALUE 'EL156'.
00076      12  XCTL-EM156          PIC X(8)    VALUE 'EM156'.
00077      12  XCTL-EM1561         PIC X(8)    VALUE 'EM1561'.
           12  XCTL-1503           PIC X(8)    VALUE 'EL1503'.
021615     12  XCTL-1504           PIC X(8)    VALUE 'EL1504'.
00078      12  XCTL-155            PIC X(8)    VALUE 'EL155'.
00079      12  XCTL-157            PIC X(8)    VALUE 'EL157'.
00080      12  XCTL-158            PIC X(8)    VALUE 'EL158'.
00081      12  XCTL-162            PIC X(8)    VALUE 'EL162'.
00082      12  XCTL-1276           PIC X(8)    VALUE 'EL1276'.
00083      12  XCTL-EM1276         PIC X(8)    VALUE 'EM1276'.
102809     12  XCTL-1279           PIC X(8)    VALUE 'EL1279'.
040416     12  XCTL-1284           PIC X(8)    VALUE 'EL1284'.
00084      12  LINK-001            PIC X(8)    VALUE 'EL001'.
00085      12  LINK-004            PIC X(8)    VALUE 'EL004'.
00086      12  LINK-1523           PIC X(8)    VALUE 'EL1523'.
00087      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00088      12  LINK-ELRTRM         PIC X(8)    VALUE 'ELRTRM'.
00089      12  FILE-ID             PIC X(8).
00090      12  EMPHST-FILE-ID      PIC X(8)    VALUE 'MPPHST'.
00091      12  SC-ITEM             PIC S9(4)   VALUE +0001   COMP.
00092
00093  01  WS-HISTORY-AREA                 PIC X(50).
00094  01  WS-FLD1            REDEFINES WS-HISTORY-AREA.
00095      12  FILLER                 PIC X(38).
00096      12  WS-NUMERIC-FLD1        PIC S9(10)V99.
00097  01  WS-FLD2            REDEFINES WS-HISTORY-AREA.
00098      12  FILLER                 PIC X(45).
00099      12  WS-NUMERIC-FLD2        PIC S99V999.
00100  01  WS-FLD3            REDEFINES WS-HISTORY-AREA.
00101      12  FILLER                 PIC X(35).
00102      12  WS-NUMERIC-FLD3        PIC S9(15).
00103  01  WS-FLD4            REDEFINES WS-HISTORY-AREA.
00104      12  FILLER                 PIC X(36).
00105      12  WS-NUMERIC-FLD4        PIC S9(10)V9999.
00106
00107  01  MISC-WORK-AREAS.
00108      12  QID.
00109          16  QID-TERM        PIC X(4).
00110          16  FILLER          PIC X(4)    VALUE '150A'.
00111      12  MAP-LENGTH          PIC S9(4)   VALUE +1920   COMP.
00112      12  PASS-SWITCH         PIC X       VALUE 'A'.
00113      12  SV-LAST-BY          PIC X(4)    VALUE SPACES.
00114      12  SV-LAST-HHMMSS      PIC S9(6)   VALUE +0      COMP-3.
00115      12  SUB-1               PIC S9(4)   VALUE +0    COMP.
00116      12  DISPLAY-CNT         PIC S9(4)   VALUE +1    COMP.
00117      12  FILE-SWITCH         PIC X(4)    VALUE SPACES.
00118      12  WS-SUB              PIC 9       VALUE 0.
00119      12  RETURNED-FROM       PIC X(8)    VALUE SPACES.
00120      12  DIRECTION-SWITCH    PIC X       VALUE 'N'.
00121      12  WS-STATUS           PIC X.
00122      12  WS-LF-COVERAGE-TYPE PIC X       VALUE SPACE.
00123      12  WS-REMAINING-AMT    PIC S9(9)V99 VALUE +0.
00124      12  W-REMAINDER         PIC S9(3)    VALUE +0.
00125      12  WS-TERMS.
00126          16  WST-ORIG        PIC ZZ9.
00127          16  WST-ORIG-DAYS-GRP.
00128              20  WST-SLASH1  PIC X       VALUE '/'.
00129              20  WST-EXT-DAYS
00130                              PIC Z9.
00131          16  FILLER          PIC X       VALUE '/'.
00132          16  WST-REM         PIC ZZ9.
00133          16  WST-REM-DAYS-GRP.
00134              20  WST-SLASH2  PIC X       VALUE '/'.
00135              20  WST-REM-DAYS
00136                              PIC Z9.
00137      12  W-DAYS-PAID-X       PIC ZZZZ9.
00138      12  W-TOTAL-PAID-AMT-X  PIC ZZZZ,ZZZ.99.
00139      12  W-PAID-MONTHS       PIC S9(03)  COMP-3.
00140      12  W-PAID-DAYS         PIC S9(03)  COMP-3.
00141
00142      12  W-REM               PIC S999    COMP-3.
00143      12  W-REM-DAYS          PIC S999    COMP-3.
00144      12  W-TERM-IN-DAYS      PIC S9(4)   COMP-3.
00145      12  W-REM-TERM-IN-DAYS  PIC S9(4)   COMP-3.
00146
00147      12  W-PAYMENTS.
00148          16  W-PYMTS         PIC ZZ9.
00149          16  FILLER          PIC X       VALUE '/'.
00150          16  W-ADD-DAYS      PIC ZZ9.
00151      12  WS-ACCESS.
00152          16  FILLER          PIC XX      VALUE SPACES.
00153          16  WS-BEN-CD       PIC XX.
00154      12  WS-STATE-ACCESS.
00155          16  WS-ST-ACCESS    PIC XX.
00156          16  FILLER          PIC XX      VALUE SPACES.
00157      12  WS-CLAIM-SEQUENCE.
00158          16  FILLER          PIC X       VALUE '('.
00159          16  WS-CUR-SEQU     PIC Z9      VALUE ZEROS.
00160          16  FILLER          PIC X(04)   VALUE ' OF '.
00161          16  WS-OF-SEQU      PIC Z9      VALUE ZEROS.
00162          16  FILLER          PIC X       VALUE ')'.
00163
00164      12  WS-PURGED-MESSAGE.
00165          16  WS-PURGED-MSG       PIC X(18)    VALUE SPACES.
00166          16  WS-PURGED-DATE      PIC X(08)    VALUE SPACES.
00167
00168      12  SAVE-CONTROL        PIC X(39).
00169      12  WS-ERACCT-SAVE-KEY      PIC X(20) VALUE SPACES.
00170      12  WS-ERACCT-HOLD-RECORD   PIC X(2000) VALUE SPACES.
00171
00172      12  SUB                 PIC 99      VALUE ZEROS.
00173
00174      12  WS-ACTIVITY-CODE    PIC 99      VALUE ZEROS.
00175          88  VALID-ACTIVITY-CODE         VALUE 00, 10 THRU 17.
00176
00177      12  WS-ACT-REC-FOUND-SW PIC X       VALUE 'N'.
00178      12  WS-LETTER-SW        PIC X       VALUE 'N'.
00179      12  WS-BROWSE-SW        PIC X       VALUE 'N'.
00180      12  WS-BROWSE-START-SW  PIC X       VALUE ' '.
00181      12  WS-UPDATE-SW        PIC X       VALUE 'N'.
00182
00183      12  WS-ACT-USER-DESC.
00184          16  WS-DESC-1-3     PIC X(03)   VALUE SPACES.
00185          16  FILLER          PIC X(17)   VALUE SPACES.
00186
00187      12  WS-CERT-READ-SW     PIC X       VALUE 'N'.
00188      12  WS-LOAN-EXPIRE-DT   PIC X(08)   VALUE SPACES.
00189      12  W-CLAIM-MASTER-SAVE PIC X(350)  VALUE SPACES.
00190
00191      12  WS-PMT-AMTD.
00192          16  WS-PMT-AMT      PIC Z(4).99-.
00193
00194      12  WS-RESPONSE         PIC S9(8)   COMP.
00195          88  WS-RESP-NORMAL              VALUE +00.
00196          88  WS-RESP-NOTFND              VALUE +13.
00197
061013 01  WS-PDEF-RECORD-SW           PIC X  VALUE ' '.
061013     88  PDEF-FOUND                   VALUE 'Y'.
061013 01  ERPDEF-KEY-SAVE             PIC X(18).
061013 01  ERPDEF-KEY.
061013     12  ERPDEF-COMPANY-CD       PIC X.
061013     12  ERPDEF-STATE            PIC XX.
061013     12  ERPDEF-PROD-CD          PIC XXX.
061013     12  F                       PIC X(7).
061013     12  ERPDEF-BEN-TYPE         PIC X.
061013     12  ERPDEF-BEN-CODE         PIC XX.
061013     12  ERPDEF-EXP-DT           PIC XX.
061013
061013 01  ELCRTT-KEY.
061013     05  CTRLR-COMP-CD       PIC X.
061013     05  CTRLR-CARRIER       PIC X.
061013     05  CTRLR-GROUPING      PIC X(6).
061013     05  CTRLR-STATE         PIC X(2).
061013     05  CTRLR-ACCOUNT       PIC X(10).
061013     05  CTRLR-EFF-DT        PIC XX.
061013     05  CTRLR-CERT-NO       PIC X(11).
061013     05  CTRLR-REC-TYPE      PIC X.
00198  01  ACCESS-KEYS.
00199      12  ERACCT-KEY.
00200          16  ERACCT-PARTIAL-KEY.
00201              20  ACCT-COMP-CD    PIC X.
00202              20  ACCT-CARRIER    PIC X.
00203              20  ACCT-GROUPING   PIC X(6).
00204              20  ACCT-STATE      PIC XX.
00205              20  ACCT-ACCOUNT    PIC X(10).
00206          16  ACCT-EXP-DT         PIC XX.
00207          16  FILLER              PIC X(04).
00208      12  ELMSTR-KEY.
00209          16  MSTR-COMP-CD    PIC X.
00210          16  MSTR-CARRIER    PIC X.
00211          16  MSTR-CLAIM-NO   PIC X(7).
00212          16  MSTR-CERT-NO.
00213              20  MSTR-CERT-NO-PRIME  PIC X(10).
00214              20  MSTR-CERT-NO-SUFX   PIC X.
00215      12  W-ELRETR-KEY.
00216          16  W-RET-COMP-CD       PIC  X.
00217          16  W-RET-CARRIER       PIC  X.
00218          16  W-RET-CLAIM-NO      PIC  X(07).
00219          16  W-RET-CERT-NO.
00220              20  W-RET-CERT-PRIME
00221                                  PIC  X(10).
00222              20  W-RET-CERT-SUFX PIC  X.
00223      12  ELCNTL-KEY.
00224          16  CNTL-COMP-ID    PIC X(3).
00225          16  CNTL-REC-TYPE   PIC X.
00226          16  CNTL-ACCESS.
00227              20  FILLER      PIC XX.
00228              20  CNTL-BEN-NO PIC XX.
00229          16  CNTL-SEQ-NO     PIC S9(4)    COMP.
00230      12  ELCERT-KEY.
00231          16  CERT-COMP-CD    PIC X.
00232          16  CERT-CARRIER    PIC X.
00233          16  CERT-GROUPING   PIC X(6).
00234          16  CERT-STATE      PIC XX.
00235          16  CERT-ACCOUNT    PIC X(10).
00236          16  CERT-EFF-DT     PIC XX.
00237          16  CERT-CERT-NO.
00238              20  CERT-CERT-NO-PRIME PIC X(10).
00239              20  CERT-CERT-NO-SUFX  PIC X.
00240      12  ELTRLR-KEY.
00241          16  TRLR-COMP-CD    PIC X.
00242          16  TRLR-CARRIER    PIC X.
00243          16  TRLR-CLAIM-NO   PIC X(7).
00244          16  TRLR-CERT-NO.
00245              20  TRLR-CERT-NO-PRIME PIC X(10).
00246              20  TRLR-CERT-NO-SUFX  PIC X.
00247          16  TRLR-SEQ-NO     PIC S9(4)   COMP.
00240      12  ws-ELTRLR-KEY.
00241          16  ws-TRLR-COMP-CD    PIC X.
00242          16  ws-TRLR-CARRIER    PIC X.
00243          16  ws-TRLR-CLAIM-NO   PIC X(7).
00244          16  ws-TRLR-CERT-NO.
00245              20  ws-TRLR-CERT-NO-PRIME PIC X(10).
00246              20  ws-TRLR-CERT-NO-SUFX  PIC X.
00247          16  ws-TRLR-SEQ-NO     PIC S9(4)   COMP.
00248      12  ELACTQ-KEY.
00249          16  ACTQ-COMP-CD    PIC X.
00250          16  ACTQ-CARRIER    PIC X.
00251          16  ACTQ-CLAIM-NO   PIC X(7).
00252          16  ACTQ-CERT-NO.
00253              20  ACTQ-CERT-NO-PRIME PIC X(10).
00254              20  ACTQ-CERT-NO-SUFX  PIC X.
00255      12  ELARCH-KEY.
00256          16  ARCH-COMP-CD        PIC X.
00257          16  ARCH-ARCHIVE-NO     PIC S9(08)  COMP.
00258          16  ARCH-RECORD-TYPE    PIC X.
00259          16  ARCH-SEQ-NO         PIC S9(04)  COMP.
00260
00261      12  EMPLCY-KEY.
00262          16  PLCY-COMP-CD            PIC X.
00263          16  PLCY-CARRIER            PIC X.
00264          16  PLCY-GROUPING           PIC X(06).
00265          16  PLCY-STATE              PIC XX.
00266          16  PLCY-PRODUCER           PIC X(10).
00267          16  PLCY-EFF-DT             PIC XX.
00268          16  PLCY-REFERENCE-NO.
00269              20  PLCY-REFNO-PRIME    PIC X(18).
00270              20  PLCY-REFNO-SFX      PIC XX.
00271
00272      12  EMPHST-KEY.
00273          16  EMPHST-COMPANY-CD       PIC X.
00274          16  EMPHST-CARRIER          PIC X.
00275          16  EMPHST-GROUPING         PIC X(06).
00276          16  EMPHST-STATE            PIC XX.
00277          16  EMPHST-PRODUCER         PIC X(10).
00278          16  EMPHST-POLICY-EFF-DT    PIC XX.
00279          16  EMPHST-REFERENCE-NUMBER.
00280              20  EMPHST-REFNO-PRIME  PIC X(18).
00281              20  EMPHST-REFNO-SFX    PIC XX.
00282          16  EMPHST-RECORD-TYPE      PIC XX.
00283          16  EMPHST-FIELD-TYPE       PIC XX.
00284          16  EMPHST-SEQUENCE-NO      PIC S9(04) COMP.
00285
00286      12  EMPLAN-KEY.
00287          16  PLAN-COMP-CD            PIC X.
00288          16  PLAN-CARRIER            PIC X.
00289          16  PLAN-GROUPING           PIC X(06).
00290          16  PLAN-STATE              PIC XX.
00291          16  PLAN-PRODUCER           PIC X(10).
00292          16  PLAN-PLAN-CODE          PIC XX.
00293          16  PLAN-REV-NO             PIC 9(03).
00294
00295      12  EMPROD-KEY.
00296          16  EMPROD-PARTIAL-KEY.
00297              20  PROD-COMP-CD        PIC X.
00298              20  PROD-CARRIER        PIC X.
00299              20  PROD-GROUPING       PIC X(06).
00300              20  PROD-STATE          PIC XX.
00301              20  PROD-PRODUCER       PIC X(10).
00302          16  PROD-EXP-DT             PIC XX.
00303
00304  01  TRAILER-DISPLAY-WORK-AREA.
00305      12  DISPLAY-ACTION.
00306          16  DISP-ACT-A      PIC X(10).
00307          16  DISP-ACT-B      PIC X(4).
00308      12  FILLER              PIC X.
00309      12  DISPLAY-BY          PIC X(4).
00310      12  FILLER              PIC X.
00311      12  DISPLAY-DATE        PIC X(8).
00312      12  FILLER              PIC X.
00313      12  DISPLAY-SEQ         PIC Z(4).
00314      12  FILLER              PIC X.
00315      12  DISPLAY-TEXT        PIC X(45).
00316      EJECT
00317  01  ERROR-MESSAGES.
00318      05  ER-0000                 PIC X(4) VALUE '0000'.
00319      05  ER-0004                 PIC X(4) VALUE '0004'.
00320      05  ER-0008                 PIC X(4) VALUE '0008'.
00321      05  ER-0029                 PIC X(4) VALUE '0029'.
00322      05  ER-0033                 PIC X(4) VALUE '0033'.
00323      05  ER-0042                 PIC X(4) VALUE '0042'.
00324      05  ER-0068                 PIC X(4) VALUE '0068'.
00325      05  ER-0070                 PIC X(4) VALUE '0070'.
00326      05  ER-0130                 PIC X(4) VALUE '0130'.
00327      05  ER-0154                 PIC X(4) VALUE '0154'.
00328      05  ER-0168                 PIC X(4) VALUE '0168'.
00329      05  ER-0169                 PIC X(4) VALUE '0169'.
00330      05  ER-0172                 PIC X(4) VALUE '0172'.
00331      05  ER-0190                 PIC X(4) VALUE '0190'.
00332      05  ER-0204                 PIC X(4) VALUE '0204'.
00333      05  ER-0206                 PIC X(4) VALUE '0206'.
00334      05  ER-0303                 PIC X(4) VALUE '0303'.
00335      05  ER-0334                 PIC X(4) VALUE '0334'.
00336      05  ER-0335                 PIC X(4) VALUE '0335'.
00337      05  ER-0336                 PIC X(4) VALUE '0336'.
00338      05  ER-0337                 PIC X(4) VALUE '0337'.
00339      05  ER-0338                 PIC X(4) VALUE '0338'.
00340      05  ER-0376                 PIC X(4) VALUE '0376'.
00341      05  ER-0412                 PIC X(4) VALUE '0412'.
00342      05  ER-0413                 PIC X(4) VALUE '0413'.
050107     05  ER-0433                 PIC X(4) VALUE '0433'.
050107     05  ER-0500                 PIC X(4) VALUE '0500'.
00343      05  ER-0802                 PIC X(4) VALUE '0802'.
00344      05  ER-0803                 PIC X(4) VALUE '0803'.
00345      05  ER-0804                 PIC X(4) VALUE '0804'.
00346      05  ER-0926                 PIC X(4) VALUE '0926'.
00347      05  ER-0927                 PIC X(4) VALUE '0927'.
00348      05  ER-0928                 PIC X(4) VALUE '0928'.
00349      05  ER-0929                 PIC X(4) VALUE '0929'.
00350      05  ER-0931                 PIC X(4) VALUE '0931'.
00351      05  ER-0932                 PIC X(4) VALUE '0932'.
00352      05  ER-0933                 PIC X(4) VALUE '0933'.
00353      05  ER-0934                 PIC X(4) VALUE '0934'.
00354      05  ER-0935                 PIC X(4) VALUE '0935'.
00355      05  ER-0936                 PIC X(4) VALUE '0936'.
00356      05  ER-0937                 PIC X(4) VALUE '0937'.
00357      05  ER-2378                 PIC X(4) VALUE '2378'.
00358      05  ER-2379                 PIC X(4) VALUE '2379'.
00359      05  ER-2848                 PIC X(4) VALUE '2848'.
00360      05  ER-3516                 PIC X(4) VALUE '3516'.
00361      05  ER-3526                 PIC X(4) VALUE '3526'.
00362      05  ER-3545                 PIC X(4) VALUE '3545'.
050107     05  ER-3550                 PIC X(4) VALUE '3550'.
00363      05  ER-9483                 PIC X(4) VALUE '9483'.
00364      05  ER-9808                 PIC X(4) VALUE '9808'.
00365      05  ER-9883                 PIC X(4) VALUE '9883'.
00366      05  ER-9886                 PIC X(4) VALUE '9886'.
00367
00368  01  TEXT-WORK-AREAS.
00369      12  PAYMENT-TEXT.
00370          16  PMT-VAR         PIC X(8).
00371          16  PMT-PD-THRU     PIC X(8).
013017         16  pmt-check-head  pic x(7)    value spaces.
013017*        16  FILLER          PIC X(7)    VALUE ' CHECK='.
00373          16  PMT-CHECK-NO    PIC X(7).
00374          16  FILLER          PIC X(5)    VALUE ' AMT='.
00375          16  PMT-AMOUNT      PIC Z(6).99-.
00376
00377      12  AUTO-PMT-TEXT.
00378          16  FILLER          PIC X(6)    VALUE 'START='.
00379          16  AUTO-START      PIC X(8).
00380          16  FILLER          PIC X(5)    VALUE ' END='.
00381          16  AUTO-END        PIC X(8).
00382          16  FILLER          PIC X(8)    VALUE ' AMOUNT='.
00383          16  AUTO-AMOUNT     PIC Z(6).99-.
00384
00385      12  AUTO-TERM-TEXT.
00386          16  FILLER          PIC X(11)   VALUE 'TERMINATED='.
00387          16  AUTO-TERMINATED PIC X(8).
00388          16  FILLER          PIC X(8)    VALUE SPACES.
00389          16  FILLER          PIC X(8)    VALUE ' AMOUNT='.
00390          16  AUTO-TERM-AMT   PIC Z(6).99-.
00391
00392      12  CORRESPONDENCE-TEXT.
00393          16  FILLER          PIC X(3)    VALUE 'TO='.
00394          16  CORR-TO         PIC X(8).
113010         16  CORR-RECVD-LIT  PIC X(8)    VALUE '  RECVD='.
00396          16  CORR-RECVD      PIC X(8).
00397          16  CORR-VAR        PIC X(9).
00398          16  CORR-RESENT     PIC X(8).
00399          16  CORR-ARCH-NO REDEFINES CORR-RESENT PIC 9(8).
00400
00401      12  PROMPT-TEXT.
00402          16  FILLER          PIC X(4)    VALUE 'END='.
00403          16  PROMPT-END      PIC X(8).
00404          16  FILLER          PIC XX      VALUE SPACES.
00405          16  PROMPT-MSG      PIC X(31).
00406
00407      12  DENIAL-TEXT.
00408          16  FILLER          PIC X(5)    VALUE 'DATE='.
00409          16  DENIAL-DATE     PIC X(8).
00410          16  FILLER          PIC X(6)    VALUE ' CODE='.
00411          16  DENIAL-CODE     PIC X(4).
00412          16  FILLER          PIC XX      VALUE SPACES.
00413          16  DENIAL-MSG      PIC X(20).
00414
00415      12  RECONSIDERED-TEXT.
00416          16  FILLER          PIC X(7)    VALUE 'DENIED '.
00417          16  DENIED-DATE     PIC X(8).
00418          16  FILLER          PIC X(15)   VALUE '  RECONSIDERED '.
00419          16  RECONSIDERED-DATE  PIC X(8).
00420
00421      12  INCUR-TEXT.
00422          16  FILLER          PIC X(6)    VALUE 'INCUR='.
00423          16  INCUR-DT        PIC X(8).
00424          16  PDTHRU-HEAD     PIC X(9)    VALUE ' PD THRU='.
00425          16  INCUR-PDTHRU    PIC X(8).
00426          16  FILLER          PIC X(4)    VALUE ' PD='.
00427          16  INCUR-PAID      PIC Z(6).99-.
00428
00429      12  LOAN-TEXT.
00430          16  FILLER          PIC X(5)    VALUE 'LOAN='.
00431          16  LOAN-NUMBER     PIC X(8).
00432          16  FILLER          PIC X(5)    VALUE ' BAL='.
00433          16  LOAN-BALANCE    PIC Z(7).99-.
00434          16  FILLER          PIC X(4)    VALUE ' ME='.
00435          16  LOAN-MEMBER     PIC X(12).
00436
00437      12  ASSOC-LOAN-TEXT.
00438          16  FILLER          PIC X(11)   VALUE
00439                  'ORIG/CUR : '.
00440          16  ASSOC-ORIG-LOAN PIC X(08)   VALUE SPACES.
00441          16  FILLER          PIC X       VALUE SPACES.
00442          16  ASSOC-CUR-LOAN  PIC X(12)   VALUE SPACES.
00443          16  FILLER          PIC XX      VALUE SPACES.
00444          16  ASSOC-LOAN-TYPE PIC XX      VALUE SPACES.
00445          16  FILLER          PIC X(09)   VALUE SPACES.
00446
00447      12  ASSOC-CV-LOAN-TEXT.
00448          16  FILLER          PIC X(11)   VALUE
00449                  'LOAN NO. : '.
00450          16  ASSOC-CV-LOAN   PIC X(20)   VALUE SPACES.
00451          16  FILLER          PIC X(14)   VALUE SPACES.
00452
00453  01  PAYMENT-DESCRIPTION-TABLE.
00454      12  FILLER              PIC X(14)   VALUE 'PARTIAL PMT   '.
00455      12  FILLER              PIC X(14)   VALUE 'FINAL PMT     '.
00456      12  FILLER              PIC X(14)   VALUE 'LUMP SUM PMT  '.
00457      12  FILLER              PIC X(14)   VALUE 'ADDITIONAL PMT'.
00458      12  FILLER              PIC X(14)   VALUE 'CHARGEABLE EXP'.
00459      12  FILLER              PIC X(14)   VALUE 'NON-CHG EXP   '.
00460      12  FILLER              PIC X(14)   VALUE 'LIFE PRM RFND '.
00461      12  FILLER              PIC X(14)   VALUE 'A/H PRM RFND  '.
00462      12  FILLER              PIC X(14)   VALUE 'ENTRY CORRECT '.
00463  01  PAYMENT-DESC-R   REDEFINES PAYMENT-DESCRIPTION-TABLE.
022106     12  PAY-DESC            PIC X(14)   OCCURS 9.
00465
00466  01  CV-PAYMENT-DESCRIPTION-TABLE.
00467      12  FILLER              PIC X(14)   VALUE 'FULL DEATH    '.
00468      12  FILLER              PIC X(14)   VALUE 'HALF DEATH    '.
00469      12  FILLER              PIC X(14)   VALUE 'FULL AD&D     '.
00470      12  FILLER              PIC X(14)   VALUE 'HALF AD&D     '.
00471      12  FILLER              PIC X(14)   VALUE 'FULL RIDER    '.
00472      12  FILLER              PIC X(14)   VALUE 'HALF RIDER    '.
00473      12  FILLER              PIC X(14)   VALUE 'NON-CHG EXP   '.
00474      12  FILLER              PIC X(14)   VALUE 'ADDITIONAL    '.
00475  01  CV-PAYMENT-DESC-R REDEFINES CV-PAYMENT-DESCRIPTION-TABLE.
00476      12  CV-PAY-DESC         PIC X(14)   OCCURS 2.
00477
00478  01  CORRESPONDENCE-TO-DESCRIPTIONS.
00479      12  FILLER              PIC X(8)    VALUE 'INSURED '.
00480      12  FILLER              PIC X(8)    VALUE 'BENEF   '.
00481      12  FILLER              PIC X(8)    VALUE 'ACCOUNT '.
00482      12  FILLER              PIC X(8)    VALUE 'DOCTOR  '.
00483      12  FILLER              PIC X(8)    VALUE 'EMPLOYER'.
00484      12  FILLER              PIC X(8)    VALUE 'OTHER1  '.
00485      12  FILLER              PIC X(8)    VALUE 'OTHER2  '.
00486  01  CORR-TO-DESC   REDEFINES CORRESPONDENCE-TO-DESCRIPTIONS.
00487      12  CORR-DESC           PIC X(8)    OCCURS 2.
00488
00489  01  W-CONFIDENTIAL-TEXT.
00490      12  W-CONF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00491      12  W-CONF-TEXT.
00492          16  FILLER.
00493              20  W-CONF-PGM  PIC X(8)    VALUE 'EL150'.
00494              20  FILLER      PIC X       VALUE SPACES.
00495              20  FILLER      PIC X(71)   VALUE
00496      '******THIS IS A CONFIDENTIAL CLAIM!!!!!*****'.
00497          16  FILLER          PIC X(8)    VALUE SPACES.
00498          16  FILLER          PIC X(71)   VALUE
00499       '******PRESS ENTER TO RETURN TO SCREEN.******'.
00500          16  FILLER          PIC X(26)   VALUE SPACES.
00501 *        16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00502 *        16  FILLER          PIC X       VALUE QUOTE.
00503 *        16  W-CONF-SYS-MSG  PIC X(17)
00504 *          VALUE 'S CLAS-IC SYSTEM '.
00505
00506      EJECT
061013*                            copy ERCPDEF.
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
00507 *                            COPY ELCNWA.
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
00508      EJECT
00509 *                            COPY ELCDATE.
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
00510      EJECT
00511 *                            COPY ELCCALC.
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
00512      EJECT
00513 *                            COPY ELCLOGOF.
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
00514      EJECT
00515 *                            COPY ELCATTR.
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
00516      EJECT
00517 *                            COPY ELCMSTR.
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
00518      EJECT
00519 *                            COPY ELCEMIB.
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
00520      EJECT
00521 *                            COPY ELCLNKLT.
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
00522      EJECT
00523 *                            COPY ELCINTF.
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
00524      12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.
00525          16  PI-PASS-AREA        PIC  S9(4) COMP.
00526          16  PI-EL142-PRIORITY   PIC  X.
061013         16  pi-save-map         pic x(8).
061013         16  pi-max-benefit      pic s9(7)v99 comp-3.
031714         16  PI-APPROVAL-LEVEL   PIC X.
031714         16  f                   pic x(197).
061013*        16  FILLER              PIC  X(211).
00528          16  PI-CURRENT-FILE     PIC  X(08).
00529          16  PI-ORIGINAL-ASS-CERT
00530                                  PIC  X(20).
00531          16  PI-TOTAL-PAID       PIC S9(07)V99 COMP-3.
00532          16  PI-DAYS-PAID        PIC S9(05) COMP-3.
00533          16  PI-LAST-SEQ-NO      PIC S9(03) COMP-3.
00534          16  PI-LAST-PF-KEY-IND  PIC  X.
00535              88  PI-PF15-LAST            VALUE 'F'.
00536              88  PI-PF16-LAST            VALUE 'S'.
00537          16  PI-ASSOCIATED-PROCESS-IND
00538                                  PIC  X.
00539              88  PI-ASS-PROCESSING       VALUE 'Y'.
00540              88  PI-NOT-ASS-PROCESSING   VALUE SPACES LOW-VALUES.
00541          16  PI-ASS-NDX          PIC S9(04) COMP.
00542          16  PI-ASSOCIATED-CERTS-TABLE.
00543              20  PI-ASS-CERTS OCCURS 24 TIMES
00544                                  PIC  X(11).
050107*00545          16  FILLER              PIC  X(10).
050107         16  FILLER              PIC  X(05).
050107         16  PI-FORCE-COUNT      PIC  99.
050107         16  PI-PFKEY-USED       PIC  X.
050107         16  PI-JOINT-COV-IND    PIC  X.
050107             88  PI-JOINT-COVERAGE       VALUE 'J'.
050107             88  PI-SINGLE-COVERAGE      VALUE ' '.
050107         16  PI-JOINT-INSURED-IND PIC X.
050107             88  PI-JOINT-INSURED        VALUE 'Y'.
050107             88  PI-PRIMARY-INSURED      VALUE 'N'.
00546          16  PI-PRIORITY-CD      PIC  X.
00547              88  PI-PRIORITY-9           VALUE '9'.
00548          16  PI-STATUS-IND       PIC  X.
00549              88  PI-CLOSED               VALUE 'C'.
00550              88  PI-OPEN                 VALUE 'O'.
00551          16  PI-LAST-CLOSE-REASON-IND
00552                                  PIC  X.
00553              88  PI-BENEFIT-CHANGE       VALUE 'C'.
00554              88  PI-ERROR-CORRECTION     VALUE 'E'.
00555          16  PI-PRIORITY-IND     PIC  X.
00556              88  PI-SUPERVISOR-ONLY      VALUE 'S'.
00557              88  PI-CONFIDENTIAL-UP      VALUE 'C'.
00558          16  PI-RETRIEVED-DATA-IND
00559                                  PIC  X.
00560              88  PI-RETRIEVED-DATA       VALUE 'Y'.
00561 *        16  PI-SAVE             PIC  X(227).
00562          16  PI-SAVE-TYPE        PIC  X.
00563          16  PI-SAVE-FILETO      PIC  X(4).
00564          16  PI-SAVE-PRTOPT      PIC  X.
00565          16  PI-SAVE-FORMAT      PIC  X.
00566          16  PI-SAVE-FORM        PIC  X(12).
00567          16  PI-PREV-DISP        PIC  X.
00568          16  PI-PREV-DIR         PIC  X.
00569          16  PI-SAVE-CURSOR      PIC S9(04) COMP.
00570          16  PI-SAVE-CNT         PIC S9(04) COMP.
00571          16  PI-SAVE-LOW         PIC S9(04) COMP.
00572          16  PI-SAVE-HIGH        PIC S9(04) COMP.
00573          16  PI-PREV-CLAIM.
00574              20  PI-PREV-COMP-CD PIC X.
00575              20  PI-PREV-CARRIER PIC X.
00576              20  PI-PREV-CLAIM-NO PIC X(7).
00577              20  PI-PREV-CERT-NO.
00578                  24  PI-PREV-CERT-NO-PRIME
00579                                  PIC X(10).
00580                  24  PI-CERT-NO-SUFX   PIC X.
00581          16  PI-PREV-PRIME-CERT-NO
00582                                  PIC X(11).
00583          16  PI-PURGED-SW        PIC  X.
00584              88  CLAIM-IS-PURGED         VALUE 'Y'.
00585          16  PI-PREV-CLMNO       PIC  X(07).
00586          16  PI-SAVE-MCRFLM      PIC  X(10).
00587          16  PI-PREV-TRLR-KEY    PIC  X(22).
00588          16  PI-SAVE-ACT-CD      PIC  XX.
00589          16  PI-SAVE-ASSOC       PIC  X.
00590          16  PI-SAVE-SYS-ID      PIC  XX.
CIDMOD*        16  PI-SAVE-CLOAN       PIC  X(25).
00591      EJECT
00592 *                            COPY ELCJPFX.
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
00593                              PIC X(750).
00594      EJECT
00595 *                            COPY ELCAID.
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
00596  01  FILLER    REDEFINES DFHAID.
00597      12  FILLER              PIC X(8).
00598      12  PF-VALUES           PIC X       OCCURS 24 TIMES.
00599      EJECT
00600 *                            COPY EL150S.
       01  EL150AI.
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
           05  SEQUL PIC S9(0004) COMP.
           05  SEQUF PIC  X(0001).
           05  FILLER REDEFINES SEQUF.
               10  SEQUA PIC  X(0001).
           05  SEQUI PIC  X(0010).
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
           05  RETMSTL PIC S9(0004) COMP.
           05  RETMSTF PIC  X(0001).
           05  FILLER REDEFINES RETMSTF.
               10  RETMSTA PIC  X(0001).
           05  RETMSTI PIC  X(0015).
      *    -------------------------------
           05  CRDCARDL PIC S9(0004) COMP.
           05  CRDCARDF PIC  X(0001).
           05  FILLER REDEFINES CRDCARDF.
               10  CRDCARDA PIC  X(0001).
           05  CRDCARDI PIC  X(0016).
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
           05  PRIMCRTL PIC S9(0004) COMP.
           05  PRIMCRTF PIC  X(0001).
           05  FILLER REDEFINES PRIMCRTF.
               10  PRIMCRTA PIC  X(0001).
           05  PRIMCRTI PIC  X(0010).
      *    -------------------------------
           05  PRIMSFXL PIC S9(0004) COMP.
           05  PRIMSFXF PIC  X(0001).
           05  FILLER REDEFINES PRIMSFXF.
               10  PRIMSFXA PIC  X(0001).
           05  PRIMSFXI PIC  X(0001).
      *    -------------------------------
           05  NOTIFYL PIC S9(0004) COMP.
           05  NOTIFYF PIC  X(0001).
           05  FILLER REDEFINES NOTIFYF.
               10  NOTIFYA PIC  X(0001).
           05  NOTIFYI PIC  X(0013).
      *    -------------------------------
           05  CLMTOTHL PIC S9(0004) COMP.
           05  CLMTOTHF PIC  X(0001).
           05  FILLER REDEFINES CLMTOTHF.
               10  CLMTOTHA PIC  X(0001).
           05  CLMTOTHI PIC  X(0027).
      *    -------------------------------
           05  AMTPDL PIC S9(0004) COMP.
           05  AMTPDF PIC  X(0001).
           05  FILLER REDEFINES AMTPDF.
               10  AMTPDA PIC  X(0001).
           05  AMTPDI PIC  X(0011).
      *    -------------------------------
           05  DAYSPDHL PIC S9(0004) COMP.
           05  DAYSPDHF PIC  X(0001).
           05  FILLER REDEFINES DAYSPDHF.
               10  DAYSPDHA PIC  X(0001).
           05  DAYSPDHI PIC  X(0010).
      *    -------------------------------
           05  DAYSPDL PIC S9(0004) COMP.
           05  DAYSPDF PIC  X(0001).
           05  FILLER REDEFINES DAYSPDF.
               10  DAYSPDA PIC  X(0001).
           05  DAYSPDI PIC  X(0005).
      *    -------------------------------
           05  YESNOSWL PIC S9(0004) COMP.
           05  YESNOSWF PIC  X(0001).
           05  FILLER REDEFINES YESNOSWF.
               10  YESNOSWA PIC  X(0001).
           05  YESNOSWI PIC  X(0001).
      *    -------------------------------
           05  PDTHRHDL PIC S9(0004) COMP.
           05  PDTHRHDF PIC  X(0001).
           05  FILLER REDEFINES PDTHRHDF.
               10  PDTHRHDA PIC  X(0001).
           05  PDTHRHDI PIC  X(0014).
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
           05  COVERDL PIC S9(0004) COMP.
           05  COVERDF PIC  X(0001).
           05  FILLER REDEFINES COVERDF.
               10  COVERDA PIC  X(0001).
           05  COVERDI PIC  X(0012).
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
           05  NOPMTSL PIC S9(0004) COMP.
           05  NOPMTSF PIC  X(0001).
           05  FILLER REDEFINES NOPMTSF.
               10  NOPMTSA PIC  X(0001).
           05  NOPMTSI PIC  X(0007).
      *    -------------------------------
           05  PRMTYPDL PIC S9(0004) COMP.
           05  PRMTYPDF PIC  X(0001).
           05  FILLER REDEFINES PRMTYPDF.
               10  PRMTYPDA PIC  X(0001).
           05  PRMTYPDI PIC  X(0012).
      *    -------------------------------
           05  PRMTYPEL PIC S9(0004) COMP.
           05  PRMTYPEF PIC  X(0001).
           05  FILLER REDEFINES PRMTYPEF.
               10  PRMTYPEA PIC  X(0001).
           05  PRMTYPEI PIC  X(0008).
      *    -------------------------------
           05  NXTAUTOL PIC S9(0004) COMP.
           05  NXTAUTOF PIC  X(0001).
           05  FILLER REDEFINES NXTAUTOF.
               10  NXTAUTOA PIC  X(0001).
           05  NXTAUTOI PIC  X(0008).
      *    -------------------------------
           05  ESTABDTL PIC S9(0004) COMP.
           05  ESTABDTF PIC  X(0001).
           05  FILLER REDEFINES ESTABDTF.
               10  ESTABDTA PIC  X(0001).
           05  ESTABDTI PIC  X(0008).
      *    -------------------------------
           05  CRTSTADL PIC S9(0004) COMP.
           05  CRTSTADF PIC  X(0001).
           05  FILLER REDEFINES CRTSTADF.
               10  CRTSTADA PIC  X(0001).
           05  CRTSTADI PIC  X(0012).
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
           05  TOTINTL PIC S9(0004) COMP.
           05  TOTINTF PIC  X(0001).
           05  FILLER REDEFINES TOTINTF.
               10  TOTINTA PIC  X(0001).
           05  TOTINTI PIC  S9(7)V99.
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0003).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  EXPIREL PIC S9(0004) COMP.
           05  EXPIREF PIC  X(0001).
           05  FILLER REDEFINES EXPIREF.
               10  EXPIREA PIC  X(0001).
           05  EXPIREI PIC  X(0008).
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
           05  TERMSL PIC S9(0004) COMP.
           05  TERMSF PIC  X(0001).
           05  FILLER REDEFINES TERMSF.
               10  TERMSA PIC  X(0001).
           05  TERMSI PIC  X(0013).
      *    -------------------------------
           05  DIAGL PIC S9(0004) COMP.
           05  DIAGF PIC  X(0001).
           05  FILLER REDEFINES DIAGF.
               10  DIAGA PIC  X(0001).
           05  DIAGI PIC  X(0056).
      *    -------------------------------
           05  FILETOL PIC S9(0004) COMP.
           05  FILETOF PIC  X(0001).
           05  FILLER REDEFINES FILETOF.
               10  FILETOA PIC  X(0001).
           05  FILETOI PIC  X(0004).
      *    -------------------------------
           05  PRTOPTL PIC S9(0004) COMP.
           05  PRTOPTF PIC  X(0001).
           05  FILLER REDEFINES PRTOPTF.
               10  PRTOPTA PIC  X(0001).
           05  PRTOPTI PIC  X(0001).
      *    -------------------------------
           05  FORMATL PIC S9(0004) COMP.
           05  FORMATF PIC  X(0001).
           05  FILLER REDEFINES FORMATF.
               10  FORMATA PIC  X(0001).
           05  FORMATI PIC  X(0001).
      *    -------------------------------
           05  ALTPRTL PIC S9(0004) COMP.
           05  ALTPRTF PIC  X(0001).
           05  FILLER REDEFINES ALTPRTF.
               10  ALTPRTA PIC  X(0001).
           05  ALTPRTI PIC  X(0004).
      *    -------------------------------
           05  CLOANL PIC S9(0004) COMP.
           05  CLOANF PIC  X(0001).
           05  FILLER REDEFINES CLOANF.
               10  CLOANA PIC  X(0001).
           05  CLOANI PIC  X(0025).
      *    -------------------------------
           05  DENTYPL PIC S9(0004) COMP.
           05  DENTYPF PIC  X(0001).
           05  FILLER REDEFINES DENTYPF.
               10  DENTYPA PIC  X(0001).
           05  DENTYPI PIC  X(0029).
      *    -------------------------------
           05  ACTCDL PIC S9(0004) COMP.
           05  ACTCDF PIC  X(0001).
           05  FILLER REDEFINES ACTCDF.
               10  ACTCDA PIC  X(0001).
           05  ACTCDI PIC  99.
      *    -------------------------------
           05  ACTDTL PIC S9(0004) COMP.
           05  ACTDTF PIC  X(0001).
           05  FILLER REDEFINES ACTDTF.
               10  ACTDTA PIC  X(0001).
           05  ACTDTI PIC  X(0008).
      *    -------------------------------
           05  ACTTYPL PIC S9(0004) COMP.
           05  ACTTYPF PIC  X(0001).
           05  FILLER REDEFINES ACTTYPF.
               10  ACTTYPA PIC  X(0001).
           05  ACTTYPI PIC  X(0004).
      *    -------------------------------
           05  ASSHDGL PIC S9(0004) COMP.
           05  ASSHDGF PIC  X(0001).
           05  FILLER REDEFINES ASSHDGF.
               10  ASSHDGA PIC  X(0001).
           05  ASSHDGI PIC  X(0006).
      *    -------------------------------
           05  ASSOCL PIC S9(0004) COMP.
           05  ASSOCF PIC  X(0001).
           05  FILLER REDEFINES ASSOCF.
               10  ASSOCA PIC  X(0001).
           05  ASSOCI PIC  X(0001).
      *    -------------------------------
           05  TRLR1L PIC S9(0004) COMP.
           05  TRLR1F PIC  X(0001).
           05  FILLER REDEFINES TRLR1F.
               10  TRLR1A PIC  X(0001).
           05  TRLR1I PIC  X(0079).
      *    -------------------------------
           05  TRLR2L PIC S9(0004) COMP.
           05  TRLR2F PIC  X(0001).
           05  FILLER REDEFINES TRLR2F.
               10  TRLR2A PIC  X(0001).
           05  TRLR2I PIC  X(0079).
      *    -------------------------------
           05  TRLR3L PIC S9(0004) COMP.
           05  TRLR3F PIC  X(0001).
           05  FILLER REDEFINES TRLR3F.
               10  TRLR3A PIC  X(0001).
           05  TRLR3I PIC  X(0079).
      *    -------------------------------
           05  TRLR4L PIC S9(0004) COMP.
           05  TRLR4F PIC  X(0001).
           05  FILLER REDEFINES TRLR4F.
               10  TRLR4A PIC  X(0001).
           05  TRLR4I PIC  X(0079).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  99.
      *    -------------------------------
           05  PFKEY11L PIC S9(0004) COMP.
           05  PFKEY11F PIC  X(0001).
           05  FILLER REDEFINES PFKEY11F.
               10  PFKEY11A PIC  X(0001).
           05  PFKEY11I PIC  X(0017).
       01  EL150AO REDEFINES EL150AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQUO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETMSTO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRDCARDO PIC  X(0016).
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
           05  PRIMCRTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRIMSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOTIFYO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTOTHO PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMTPDO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAYSPDHO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAYSPDO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YESNOSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTHRHDO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTPAIDO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVERDO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVERO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCURO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOPMTSO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRMTYPDO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRMTYPEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NXTAUTOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTABDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTSTADO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTSTATO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFECTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTINTO PIC  -(6).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPIREO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENECAPO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENEO PIC  Z(6).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERMSO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DIAGO PIC  X(0056).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILETOO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMATO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTPRTO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLOANO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DENTYPO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTTYPO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASSHDGO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASSOCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRLR1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRLR2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRLR3O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRLR4O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEY11O PIC  X(0017).
      *    -------------------------------
       01  EL150TI REDEFINES EL150AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTL PIC S9(0004) COMP.
           05  RUNDTF PIC  X(0001).
           05  FILLER REDEFINES RUNDTF.
               10  RUNDTA PIC  X(0001).
           05  RUNDTI PIC  X(0008).
      *    -------------------------------
           05  RUNTIML PIC S9(0004) COMP.
           05  RUNTIMF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMF.
               10  RUNTIMA PIC  X(0001).
           05  RUNTIMI PIC  X(0005).
      *    -------------------------------
           05  CLMTYP1L PIC S9(0004) COMP.
           05  CLMTYP1F PIC  X(0001).
           05  FILLER REDEFINES CLMTYP1F.
               10  CLMTYP1A PIC  X(0001).
           05  CLMTYP1I PIC  X(0002).
      *    -------------------------------
           05  BANK1L PIC S9(0004) COMP.
           05  BANK1F PIC  X(0001).
           05  FILLER REDEFINES BANK1F.
               10  BANK1A PIC  X(0001).
           05  BANK1I PIC  X(0003).
      *    -------------------------------
           05  NOCLMS1L PIC S9(0004) COMP.
           05  NOCLMS1F PIC  X(0001).
           05  FILLER REDEFINES NOCLMS1F.
               10  NOCLMS1A PIC  X(0001).
           05  NOCLMS1I PIC  X(0003).
      *    -------------------------------
           05  NODAYS1L PIC S9(0004) COMP.
           05  NODAYS1F PIC  X(0001).
           05  FILLER REDEFINES NODAYS1F.
               10  NODAYS1A PIC  X(0001).
           05  NODAYS1I PIC  X(0005).
      *    -------------------------------
           05  TAMT1L PIC S9(0004) COMP.
           05  TAMT1F PIC  X(0001).
           05  FILLER REDEFINES TAMT1F.
               10  TAMT1A PIC  X(0001).
           05  TAMT1I PIC  X(0012).
      *    -------------------------------
           05  CLMTYP2L PIC S9(0004) COMP.
           05  CLMTYP2F PIC  X(0001).
           05  FILLER REDEFINES CLMTYP2F.
               10  CLMTYP2A PIC  X(0001).
           05  CLMTYP2I PIC  X(0002).
      *    -------------------------------
           05  BANK2L PIC S9(0004) COMP.
           05  BANK2F PIC  X(0001).
           05  FILLER REDEFINES BANK2F.
               10  BANK2A PIC  X(0001).
           05  BANK2I PIC  X(0003).
      *    -------------------------------
           05  NOCLMS2L PIC S9(0004) COMP.
           05  NOCLMS2F PIC  X(0001).
           05  FILLER REDEFINES NOCLMS2F.
               10  NOCLMS2A PIC  X(0001).
           05  NOCLMS2I PIC  X(0003).
      *    -------------------------------
           05  NODAYS2L PIC S9(0004) COMP.
           05  NODAYS2F PIC  X(0001).
           05  FILLER REDEFINES NODAYS2F.
               10  NODAYS2A PIC  X(0001).
           05  NODAYS2I PIC  X(0005).
      *    -------------------------------
           05  TAMT2L PIC S9(0004) COMP.
           05  TAMT2F PIC  X(0001).
           05  FILLER REDEFINES TAMT2F.
               10  TAMT2A PIC  X(0001).
           05  TAMT2I PIC  X(0012).
      *    -------------------------------
           05  CLMTYP3L PIC S9(0004) COMP.
           05  CLMTYP3F PIC  X(0001).
           05  FILLER REDEFINES CLMTYP3F.
               10  CLMTYP3A PIC  X(0001).
           05  CLMTYP3I PIC  X(0002).
      *    -------------------------------
           05  BANK3L PIC S9(0004) COMP.
           05  BANK3F PIC  X(0001).
           05  FILLER REDEFINES BANK3F.
               10  BANK3A PIC  X(0001).
           05  BANK3I PIC  X(0003).
      *    -------------------------------
           05  NOCLMS3L PIC S9(0004) COMP.
           05  NOCLMS3F PIC  X(0001).
           05  FILLER REDEFINES NOCLMS3F.
               10  NOCLMS3A PIC  X(0001).
           05  NOCLMS3I PIC  X(0003).
      *    -------------------------------
           05  NODAYS3L PIC S9(0004) COMP.
           05  NODAYS3F PIC  X(0001).
           05  FILLER REDEFINES NODAYS3F.
               10  NODAYS3A PIC  X(0001).
           05  NODAYS3I PIC  X(0005).
      *    -------------------------------
           05  TAMT3L PIC S9(0004) COMP.
           05  TAMT3F PIC  X(0001).
           05  FILLER REDEFINES TAMT3F.
               10  TAMT3A PIC  X(0001).
           05  TAMT3I PIC  X(0012).
      *    -------------------------------
           05  CLMTYP4L PIC S9(0004) COMP.
           05  CLMTYP4F PIC  X(0001).
           05  FILLER REDEFINES CLMTYP4F.
               10  CLMTYP4A PIC  X(0001).
           05  CLMTYP4I PIC  X(0002).
      *    -------------------------------
           05  BANK4L PIC S9(0004) COMP.
           05  BANK4F PIC  X(0001).
           05  FILLER REDEFINES BANK4F.
               10  BANK4A PIC  X(0001).
           05  BANK4I PIC  X(0003).
      *    -------------------------------
           05  NOCLMS4L PIC S9(0004) COMP.
           05  NOCLMS4F PIC  X(0001).
           05  FILLER REDEFINES NOCLMS4F.
               10  NOCLMS4A PIC  X(0001).
           05  NOCLMS4I PIC  X(0003).
      *    -------------------------------
           05  NODAYS4L PIC S9(0004) COMP.
           05  NODAYS4F PIC  X(0001).
           05  FILLER REDEFINES NODAYS4F.
               10  NODAYS4A PIC  X(0001).
           05  NODAYS4I PIC  X(0005).
      *    -------------------------------
           05  TAMT4L PIC S9(0004) COMP.
           05  TAMT4F PIC  X(0001).
           05  FILLER REDEFINES TAMT4F.
               10  TAMT4A PIC  X(0001).
           05  TAMT4I PIC  X(0012).
      *    -------------------------------
           05  CLMTYP5L PIC S9(0004) COMP.
           05  CLMTYP5F PIC  X(0001).
           05  FILLER REDEFINES CLMTYP5F.
               10  CLMTYP5A PIC  X(0001).
           05  CLMTYP5I PIC  X(0002).
      *    -------------------------------
           05  BANK5L PIC S9(0004) COMP.
           05  BANK5F PIC  X(0001).
           05  FILLER REDEFINES BANK5F.
               10  BANK5A PIC  X(0001).
           05  BANK5I PIC  X(0003).
      *    -------------------------------
           05  NOCLMS5L PIC S9(0004) COMP.
           05  NOCLMS5F PIC  X(0001).
           05  FILLER REDEFINES NOCLMS5F.
               10  NOCLMS5A PIC  X(0001).
           05  NOCLMS5I PIC  X(0003).
      *    -------------------------------
           05  NODAYS5L PIC S9(0004) COMP.
           05  NODAYS5F PIC  X(0001).
           05  FILLER REDEFINES NODAYS5F.
               10  NODAYS5A PIC  X(0001).
           05  NODAYS5I PIC  X(0005).
      *    -------------------------------
           05  TAMT5L PIC S9(0004) COMP.
           05  TAMT5F PIC  X(0001).
           05  FILLER REDEFINES TAMT5F.
               10  TAMT5A PIC  X(0001).
           05  TAMT5I PIC  X(0012).
       01  EL150TO REDEFINES EL150AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTYP1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BANK1O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOCLMS1O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NODAYS1O PIC  99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAMT1O PIC  9,999,999.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTYP2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BANK2O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOCLMS2O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NODAYS2O PIC  99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAMT2O PIC  9,999,999.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTYP3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BANK3O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOCLMS3O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NODAYS3O PIC  99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAMT3O PIC  9,999,999.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTYP4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BANK4O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOCLMS4O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NODAYS4O PIC  99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAMT4O PIC  9,999,999.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTYP5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BANK5O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOCLMS5O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NODAYS5O PIC  99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAMT5O PIC  9,999,999.99.
      *    -------------------------------
00601  01  EL150AI-R REDEFINES EL150AI.
CIDMOD*    12  FILLER              PIC X(665).
101501*    12  FILLER              PIC X(706).
042110     12  FILLER              PIC X(738).
00603      12  EL150AI-OCCURS         OCCURS 4 TIMES.
00604          16  FILLER          PIC X(3).
00605          16  MAP-DISPLAY     PIC X(79).
00606      12  FILLER              PIC X(40).
00607
00608      EJECT
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
00610  01  DFHCOMMAREA             PIC X(1024).
00611
00612  01  CLAIM-MASTER-L          PIC X(350).
00613      EJECT
00614 *                            COPY ELCCNTL.
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
00615      EJECT
00616 *                            COPY ELCCERT.
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
00617      EJECT
00618 *                            COPY ELCTRLR.
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
00619      EJECT
00620 *                            COPY ELCACTQ.
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
00621      EJECT
00622 *                            COPY ERCACCT.
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
00623      EJECT
00624 *                            COPY ELCARCH.
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
061013*                            copy ELCCRTT.
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
00625      EJECT
00626 *                COPY MTCPLCY REPLACING ==:TAG:== BY ==PM==.
00001 ******************************************************************
00002 *                                                                *
00003 *                           MTCPLCY                              *
00004 *                            VMOD=1.003                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = POLICY MASTER                             *
00007 *                                                                *
00008 *   FILE TYPE = VSAM,KSDS                                        *
00009 *   RECORD SIZE = 1200 RECFORM = FIXED                           *
00010 *                                                                *
00011 *   BASE CLUSTER = MPPLCY                         RKP=2,LEN=42   *
00012 *       ALTERNATE PATH2 = ** NOT USED **                         *
00013 *       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
00014 *       ALTERNATE PATH4 = MPPLCY4 (BY REF. NO.)   RKP=60,LEN=25  *
00015 *       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT )   RKP=85,LEN=27  *
00016 *       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT )   RKP=112,LEN=15 *
00017 *       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO.)   RKP=127,LEN=27 *
00018 *                                                                *
00019 *   LOG = YES                                                    *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 ******************************************************************
00022 **WARNING*********************************************************
00023 **ANY CHANGES TO THIS COPY BOOK MAY NEED CORRESPONDING CHANGES****
00024 **TO THE FOLLOWING COPY BOOKS: MPCPOLUP                          *
00025 **                             MPCPHSTD                          *
00026 **                             MPCPHSTC                          *
00027 **                             MPCPHSTT                          *
00028 **                                                               *
00029 ******************************************************************
00030
00031  01  POLICY-MASTER.
00032      12  PM-RECORD-ID                   PIC XX.
00033          88  VALID-PM-ID                      VALUE 'PM'.
00034
00035 ******************************************************************
00036 *   BASE CLUSTER = MPPLCY         (BASE KEY)      RKP=2,LEN=42   *
00037 ******************************************************************
00038
00039      12  PM-CONTROL-PRIMARY.
00040          16  PM-PRODUCER-PRIMARY.
00041              20  PM-PROD-PRIMARY.
00042                  24  PM-COMPANY-CD      PIC X.
00043                  24  PM-CGSP-KEY.
00044                      28  PM-CARRIER     PIC X.
00045                      28  PM-GROUPING.
00046                          32  PM-GROUPING-PREFIX
00047                                            PIC X(3).
00048                          32  PM-GROUPING-PRIME
00049                                            PIC X(3).
00050                      28  PM-STATE       PIC XX.
00051                      28  PM-PRODUCER.
00052                          32  PM-PRODUCER-PREFIX
00053                                            PIC X(4).
00054                          32  PM-PRODUCER-PRIME
00055                                            PIC X(6).
00056              20  PM-POLICY-EFF-DT       PIC XX.
00057          16  PM-REFERENCE-NUMBER.
00058              20  PM-REFNO-PRIME         PIC X(18).
00059              20  PM-REFNO-SFX           PIC XX.
00060
00061 ******************************************************************
00062 *       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
00063 ******************************************************************
00064
00065      12  PM-CONTROL-BY-SSN.
00066          16  PM-COMPANY-CD-A3           PIC X.
00067          16  PM-SOC-SEC-NO.
00068              20  PM-SSN-STATE           PIC XX.
00069              20  PM-SSN-PRODUCER        PIC X(6).
00070              20  PM-SSN-LN3.
00071                  25  PM-INSURED-INITIALS-A3.
00072                      30 PM-INSURED-INITIAL1-A3 PIC X.
00073                      30 PM-INSURED-INITIAL2-A3 PIC X.
00074                  25 PM-PART-LAST-NAME-A3      PIC X.
00075          16  PM-DATE-A3                  PIC XX.
00076          16  PM-TIME-A3                  PIC S9(04)   COMP.
00077
00078 ******************************************************************
00079 *       ALTERNATE PATH4 = MPPLCY4 (BY REFRENCE)   RKP=60,LEN=25  *
00080 ******************************************************************
00081
00082      12  PM-CONTROL-BY-POLICY-NO.
00083          16  PM-COMPANY-CD-A4           PIC X.
00084          16  PM-POLICY-NO-A4.
00085              20  PM-POLICY-PRIME-A4     PIC X(18).
00086              20  PM-POLICY-SFX-A4       PIC XX.
00087          16  PM-DATE-A4                 PIC XX.
00088          16  PM-TIME-A4                 PIC S9(04)   COMP.
00089
00090 ******************************************************************
00091 *       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT NO) RKP=85,LEN=27  *
00092 ******************************************************************
00093
00094      12  PM-CONTROL-BY-ACCOUNT.
00095          16  PM-COMPANY-CD-A5           PIC X.
00096          16  PM-BANK-ACCOUNT-NUMBER     PIC X(20).
00097          16  PM-DATE-A5                 PIC XX.
00098          16  PM-TIME-A5                 PIC S9(07)   COMP.
00099
00100 ******************************************************************
00101 *       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT NO) RKP=112,LEN=15 *
00102 ******************************************************************
00103
00104      12  PM-CONTROL-BY-TRANSIT.
00105          16  PM-COMPANY-CD-A6           PIC X.
00106          16  PM-BANK-TRANSIT-NUMBER.
00107              20  PM-FEDERAL-NUMBER      PIC X(4).
00108              20  PM-BANK-NUMBER         PIC X(4).
00109          16  PM-DATE-A6                 PIC XX.
00110          16  PM-TIME-A6                 PIC S9(07)   COMP.
00111
00112 ******************************************************************
00113 *       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO)    RKP=127,LEN=27 *
00114 ******************************************************************
00115
00116      12  PM-CONTROL-BY-LOAN-NO.
00117          16  PM-COMPANY-CD-A7           PIC X.
00118          16  PM-LOAN-NUMBER             PIC X(20).
00119          16  PM-DATE-A7                 PIC XX.
00120          16  PM-TIME-A7                 PIC S9(07)   COMP.
00121
00122 ******************************************************************
00123 *                 FILE SYNCHRONIZATION DATA                      *
00124 ******************************************************************
00125
00126      12  FILLER                            PIC X(05).
00127      12  PM-FILE-SYNCH-DATA.
00128          16  PM-LAST-CHANGE-DT          PIC XX.
00129          16  PM-LAST-CHANGE-TIME        PIC S9(7)    COMP.
00130          16  PM-LAST-CHANGE-PROCESSOR   PIC X(4).
00131      12  FILLER                            PIC X(05).
00132
00133 ******************************************************************
00134 *                    INSUREDS PROFILE DATA                       *
00135 ******************************************************************
00136
00137      12  PM-INSURED-PROFILE-DATA.
00138          16  PM-INSURED-NAME.
00139              20  PM-INSURED-LAST-NAME  PIC X(15).
00140              20  PM-INSURED-FIRST-NAME.
00141                  24  PM-INSURED-1ST-INIT PIC X.
00142                  24  FILLER               PIC X(9).
00143              20  PM-INSURED-MIDDLE-INIT PIC X.
00144          16  PM-INSURED-ADDRESS.
00145              20  PM-ADDRESS-LINE-1      PIC X(30).
00146              20  PM-ADDRESS-LINE-2      PIC X(30).
00147              20  PM-CITY                PIC X(25).
00148              20  PM-RESIDENT-STATE      PIC XX.
00149              20  PM-ZIP-CD.
00150                  24  PM-ZIP-FIRST-FIVE  PIC X(5).
00151                  24  PM-ZIP-PLUS-FOUR   PIC X(4).
00152          16  PM-INSURED-PERSONAL.
00153              20  PM-INSURED-OCC-CLASS   PIC X.
00154                  88  PM-PREFERRED         VALUE '1'.
00155                  88  PM-STANDARD          VALUE '2'.
00156                  88  PM-HAZARDOUS         VALUE '3'.
00157                  88  PM-VERY-HAZARDOUS    VALUE '4'.
00158                  88  PM-EXTREME-HAZARDOUS VALUE '5'.
00159                  88  PM-NOT-OCC           VALUE '6'.
00160                  88  PM-OCC-UNKNOWN       VALUE '9'.
00161              20  PM-INSURED-OCC-CD      PIC X(3).
00162              20  PM-INSURED-OCC-CD-NUM REDEFINES
00163                  PM-INSURED-OCC-CD      PIC 9(3).
00164              20  PM-INSURED-SEX         PIC X.
00165                  88  PM-INSURED-SEX-MALE   VALUE 'M'.
00166                  88  PM-INSURED-SEX-FEMALE VALUE 'F'.
00167              20  PM-INSURED-BIRTH-DT    PIC XX.
00168              20  PM-INSURED-ISSUE-AGE   PIC S9(3)     COMP-3.
00169              20  PM-INSURED-HEIGHT-FT   PIC S9(3)     COMP-3.
00170              20  PM-INSURED-HEIGHT-IN   PIC S9(3)     COMP-3.
00171              20  PM-INSURED-WEIGHT      PIC S9(3)     COMP-3.
00172              20  PM-INSURED-BIRTH-STATE PIC XX.
00173              20  PM-INSURED-PHONE-NO    PIC X(13).
00174              20  PM-INSURED-RATED-AGE   PIC S9(3)     COMP-3.
00175          16  PM-INS-LANGUAGE-IND        PIC X(01).
00176              88  PM-ENGLISH                        VALUE 'E'.
00177              88  PM-FRENCH                         VALUE 'F'.
00178              88  PM-SPANISH                        VALUE 'S'.
00179          16  PM-INSURED-TOT-BENEFIT     PIC S9(7)V99  COMP-3.
00180
00181          16  PM-INSURED-AGE-IND         PIC X(01).
00182              88  PM-INSURED-AGE-75-REACHED         VALUE 'Y'.
00183      12  FILLER                            PIC X(13).
00184
00185 ******************************************************************
00186 *                JOINT INSUREDS PROFILE DATA                     *
00187 ******************************************************************
00188
00189      12  PM-JOINT-PROFILE-DATA.
00190          16  PM-JOINT-NAME.
00191              20  PM-JOINT-LAST-NAME     PIC X(15).
00192              20  PM-JOINT-FIRST-NAME.
00193                  24  PM-JOINT-1ST-INIT  PIC X.
00194                  24  FILLER                PIC X(9).
00195              20  PM-JOINT-MIDDLE-INIT   PIC X.
00196          16  PM-JOINT-SOC-SEC-NO.
00197              20  PM-JT-SSN-STATE        PIC XX.
00198              20  PM-JT-SSN-PRODUCER     PIC X(6).
00199              20  PM-JT-SSN-LN3.
00200                  25  PM-JT-INSURED-INITIALS-A3.
00201                      30 PM-JT-INSURED-INITIAL1-A3 PIC X.
00202                      30 PM-JT-INSURED-INITIAL2-A3 PIC X.
00203                  25 PM-JT-PART-LAST-NAME-A3     PIC X.
00204          16  PM-JOINT-PERSONAL.
00205              20  PM-JOINT-OCC-CLASS     PIC X.
00206                  88 PM-JNT-PREFERRED       VALUE '1'.
00207                  88 PM-JNT-STANDARD        VALUE '2'.
00208                  88 PM-JNT-HAZARDOUS       VALUE '3'.
00209                  88 PM-JNT-VERY-HAZARDOUS  VALUE '4'.
00210                  88 PM-JNT-EXTREME-HAZARDOUS VALUE '5'.
00211                  88 PM-JNT-NOT-OCC         VALUE '6'.
00212                  88 PM-JNT-OCC-UNKNOWN     VALUE '9'.
00213              20  PM-JOINT-OCC-CD        PIC X(3).
00214              20  PM-JOINT-SEX           PIC X.
00215                  88  PM-JOINT-SEX-MALE     VALUE 'M'.
00216                  88  PM-JOINT-SEX-FEMALE   VALUE 'F'.
00217              20  PM-JOINT-BIRTH-DT      PIC XX.
00218              20  PM-JOINT-ISSUE-AGE     PIC S9(3)     COMP-3.
00219              20  PM-JOINT-HEIGHT-FT     PIC S9(3)     COMP-3.
00220              20  PM-JOINT-HEIGHT-IN     PIC S9(3)     COMP-3.
00221              20  PM-JOINT-WEIGHT        PIC S9(3)     COMP-3.
00222              20  PM-JOINT-BIRTH-STATE   PIC XX.
00223              20  PM-JOINT-RATED-AGE     PIC S9(3)     COMP-3.
00224          16  PM-JOINT-TOT-BENEFIT       PIC S9(7)V99  COMP-3.
00225          16  PM-JOINT-AGE-IND           PIC X(01).
00226              88  PM-JOINT-AGE-75-REACHED           VALUE 'Y'.
00227
00228      12  FILLER                            PIC X(12).
00229
00230 ******************************************************************
00231 *                  INSURANCE COVERAGE DATA                       *
00232 ******************************************************************
00233
00234      12  PM-INS-COVERAGE-DATA.
00235          16  PM-FREE-PERIOD             PIC S9(03)    COMP-3.
00236          16  PM-LOAN-TERM               PIC S9(3)     COMP-3.
00237          16  PM-LOAN-APR                PIC S9V9999   COMP-3.
00238          16  PM-LOAN-DT                 PIC XX.
00239          16  PM-LOAN-PYMT               PIC S9(5)V99  COMP-3.
00240          16  PM-LOAN-BALC               PIC S9(7)V99  COMP-3.
00241          16  PM-INS-BENEFIT-MONTHS      PIC S9(3)     COMP-3.
00242          16  PM-INS-MONTH-BENEFIT       PIC S9(7)V99  COMP-3.
00243          16  PM-INS-TOTAL-BENEFIT       PIC S9(7)V99  COMP-3.
00244          16  PM-INS-PLAN-TYPE           PIC X.
00245              88  PM-AH-MORT-PLAN           VALUE 'A'.
00246              88  PM-AD-D-MORT-PLAN         VALUE 'E'.
00247              88  PM-DISMEM-MORT-PLAN       VALUE 'D'.
00248              88  PM-LIFE-MORT-PLAN         VALUE 'L'.
00249          16  PM-INS-PLAN-CD             PIC XX.
00250          16  PM-INS-PLAN-REVISION       PIC X(3).
00251          16  PM-INS-POLICY-FORM         PIC X(12).
00252          16  PM-INS-MSTR-POLICY.
00253              20  PM-FREE-TYPE           PIC X(04).
00254              20  FILLER                    PIC X(08).
00255          16  PM-INS-MSTR-APP.
00256              20  FILLER                    PIC X(11).
00257              20  PM-INS-B-C-TYPE        PIC X(01).
00258          16  PM-INS-RATE-CD             PIC X(5).
00259          16  PM-INS-SEX-RATING          PIC X.
00260              88  PM-NOT-SEX-RATED           VALUE '1'.
00261              88  PM-SEX-RATED               VALUE '2'.
00262          16  PM-INS-SUBSTANDARD-PCT     PIC S9V9999   COMP-3.
00263          16  PM-INS-SUBSTANDARD-TYPE    PIC X.
00264          16  PM-INS-TERMINATION-DT      PIC XX.
00265          16  PM-INS-MONTH-PREMIUM   PIC S9(5)V999999  COMP-3.
00266          16  PM-INS-CALC-MO-PREM    PIC S9(5)V999999  COMP-3.
00267          16  PM-REINSURANCE-TABLE       PIC X(3).
00268          16  PM-MORTALITY-CD            PIC X(4).
00269          16  PM-INS-TYPE                PIC X.
00270              88  PM-INDIVIDUAL             VALUES ARE '1' 'I'.
00271              88  PM-GROUP                  VALUES ARE '2' 'G'.
00272          16  PM-LOAN-OFFICER            PIC X(5).
00273          16  PM-POLICY-FEE              PIC S9(3)V99 COMP-3.
00274          16  PM-DEPENDENT-COUNT         PIC S99      COMP-3.
00275          16  PM-CWA-AMOUNT              PIC S9(5)V99  COMP-3.
00276          16  PM-LAST-AUTO-RERATE-DT     PIC XX.
00277          16  PM-PREM-FINANCED-SW        PIC X.
00278              88  PM-PREM-FINANCED           VALUE 'Y'.
00279              88  PM-PREM-NOT-FINANCED       VALUE 'N'.
00280
00281          16  PM-INS-TERM-LETTER-IND     PIC X.
00282              88  PM-TERM-INITIALIZED        VALUE 'Y'.
00283          16  PM-INS-UNDERWRITER-MAX-BEN PIC S9(7)V99  COMP-3.
00284      12  FILLER                            PIC X(11).
00285
00286 ******************************************************************
00287 *                    POLICY BILLING DATA                         *
00288 ******************************************************************
00289
00290      12  PM-BILLING-DATA.
00291          16  PM-BILLING-MODE            PIC X(1).
00292              88  PM-ANNUAL                 VALUE '1'.
00293              88  PM-SEMI-ANNUAL            VALUE '2'.
00294              88  PM-QUARTERLY              VALUE '3'.
00295              88  PM-MONTHLY                VALUE '4'.
00296              88  PM-BI-MONTHLY             VALUE '5'.
00297              88  PM-SINGLE-PREM            VALUE '6'.
00298          16  PM-BILLING-SCHEDULE        PIC X(1).
00299          16  PM-BILLING-SW              PIC X(1).
00300              88  PM-FIRST-BILLING          VALUE 'Y'.
00301              88  PM-PAID-IN-ADVANCE        VALUE 'A'.
00302              88  PM-POLICY-FEE-REFUNDED    VALUE 'F'.
00303          16  PM-BILLING-TYPE            PIC X(1).
00304              88  PM-LIST-BILL              VALUE '1'.
00305              88  PM-TAPE-BILL              VALUE '2'.
00306              88  PM-TAPE-LIST-BILL         VALUE '3'.
00307              88  PM-GROUP-BILL       VALUE ARE '1' '2' '3'.
00308              88  PM-DIRECT-BILL            VALUE '4'.
00309              88  PM-PAC-BILL         VALUE ARE '5' 'C' 'S'.
00310              88  PM-CHARGE-CARD-BILL       VALUE '6'.
00311              88  PM-INDIV-BILL
00312                                   VALUE ARE '4' '5' '6' 'C' 'S'.
00313              88  PM-GRP-PLCY-BILL          VALUE '7'.
00314              88  PM-GRP-PLCY-PAC           VALUE '8'.
00315              88  PM-GRP-PLCY-CR-CRD        VALUE '9'.
00316              88  PM-GRP-PLCY         VALUE ARE '7' '8' '9'.
00317              88  PM-GRP-PROD               VALUE 'A'.
00318              88  PM-EFT-CHECKING           VALUE 'C'.
00319              88  PM-EFT-SAVINGS            VALUE 'S'.
00320          16  PM-PAYMENT-AMT             PIC S9(5)V99  COMP-3.
00321          16  PM-OVER-SHORT-AMT          PIC S9(5)V99  COMP-3.
00322          16  PM-LAST-BILL-DT            PIC XX.
00323          16  PM-LAST-BILL-AMT           PIC S9(5)V99  COMP-3.
00324          16  PM-BILL-TO-DT              PIC XX.
00325          16  PM-LAST-PYMT-DT            PIC XX.
00326          16  PM-PAID-TO-DT              PIC XX.
00327          16  PM-PYMT-INVOICE-NUMBER     PIC X(6).
00328          16  PM-MONTHS-PAID             PIC S9(3)     COMP-3.
00329          16  PM-TOTAL-PREM-RECVD        PIC S9(7)V99  COMP-3.
00330          16  PM-BILLING-GROUPING-CODE   PIC X(6).
00331          16  PM-CHARGE-CARD-EXP-DT      PIC X(2).
00332          16  PM-CHARGE-CARD-TYPE        PIC X(2).
00333              88  PM-VISA                   VALUE 'VI'.
00334              88  PM-MSTR-CARD              VALUE 'MC'.
00335              88  PM-DINERS-CLUB            VALUE 'DN'.
00336              88  PM-DISCOVER               VALUE 'DS'.
00337              88  PM-CARTE-BLANCHE          VALUE 'CB'.
00338              88  PM-AMERICAN-EXPRESS       VALUE 'AE'.
00339          16  PM-BILL-INVOICE-NUMBER     PIC X(6).
00340          16  PM-BILL-DAY                PIC S99       COMP-3.
00341          16  PM-RES-PREM-TAX        PIC S9(3)V999999  COMP-3.
00342      12  FILLER                            PIC X(15).
00343
00344 ******************************************************************
00345 *                     CLAIM PAYMENT DATA                         *
00346 ******************************************************************
00347
00348      12  PM-CLAIM-PAYMENT-DATA.
00349          16  PM-CLAIM-BENEFICIARY-NAME  PIC X(25).
00350          16  PM-CLAIM-INTERFACE-SW      PIC X.
00351              88  PM-NO-CLAIM-ATTACHED      VALUE SPACE.
00352              88  PM-POLICY-AND-CLAIM-ONLINE VALUE '1'.
00353              88  PM-POLICY-CREATED-FOR-CLAIM VALUE '2'.
00354              88  PM-CLAIM-CLOSED           VALUE '3'.
00355              88  PM-ACTIVE-CLAIM           VALUE '1' '2'.
00356              88  PM-CLAIM-ATTACHED         VALUE '1' '2' '3'.
00357          16  PM-CLAIM-INCURRED-DT       PIC XX.
00358          16  PM-CLAIM-PAID-TO-DT        PIC XX.
00359          16  PM-CLAIM-PAYMENT-CNT       PIC S9(3)     COMP-3.
00360          16  PM-CLAIM-LAST-PAYMENT-AMT  PIC S9(7)V99  COMP-3.
00361          16  PM-CLAIM-EXPENSES-ITD      PIC S9(7)V99  COMP-3.
00362          16  PM-CLAIM-PAYMENTS-ITD      PIC S9(7)V99  COMP-3.
00363          16  PM-CLAIM-ACCUMULATOR       PIC S9(7)V99  COMP-3.
00364          16  PM-CLAIM-ATTACH-CNT        PIC S9(3)     COMP-3.
00365          16  PM-CLAIM-LIFE-ITD          PIC S9(7)V99  COMP-3.
00366          16  PM-CLAIM-AH-ITD            PIC S9(7)V99  COMP-3.
00367          16  PM-CLAIM-RIDER-ITD         PIC S9(7)V99  COMP-3.
00368
00369      12  FILLER                            PIC X(03).
00370
00371 ******************************************************************
00372 *                POLICY STATUS AND DISPOSITION                   *
00373 ******************************************************************
00374
00375      12  PM-STATUS-DISPOSITION-DATA.
00376          16  PM-ISSUE-EOM-DT            PIC XX.
00377          16  PM-REPLACEMENT-SWITCH      PIC X.
00378          16  PM-APPL-SIGN-DT            PIC XX.
00379          16  PM-UNDERWRITER             PIC X(3).
00380          16  PM-ENTRY-PROCESSOR         PIC X(4).
00381          16  PM-ENTRY-STATUS            PIC X.
00382              88  PM-NORMAL                 VALUE '1'.
00383              88  PM-TAKE-OVER              VALUE '2'.
00384              88  PM-CONVERSION             VALUE '4'.
00385              88  PM-RE-ISSUE               VALUE '5'.
00386              88  PM-REINSURANCE-ONLY       VALUE '9'.
00387          16  PM-ENTRY-DT                PIC XX.
00388          16  PM-ENTRY-TIME              PIC S9(7) COMP-3.
00389          16  PM-EXIT-DT                 PIC XX.
00390          16  PM-CURRENT-STATUS          PIC X.
00391              88  PM-LAPSE                  VALUE '0'.
00392              88  PM-ACTIVE                 VALUE '1'.
00393              88  PM-PENDING-ISSUE          VALUE '2'.
00394              88  PM-DECLINED               VALUE '3'.
00395              88  PM-PENDING-CANCEL         VALUE '4'.
00396              88  PM-PENDING-ISSUE-ERROR    VALUE '5'.
00397              88  PM-CLAIM-APPLIED          VALUE '6'.
00398              88  PM-CANCEL                 VALUE '7'.
00399              88  PM-PENDING-UNWTR-REVW     VALUE '8'.
00400              88  PM-PENDING-CANCEL-ERROR   VALUE '9'.
00401              88  PM-CANCEL-TRANSFER        VALUE 'C'.
00402              88  PM-CLAIM-SETTLEMENT       VALUE 'F'.
00403              88  PM-TERMINATE              VALUE 'T'.
00404 ** NOTE TYPE 1 IS ANYTHING THAT IS OR HAS BEEN ACTIVE.  TYPE 2 IS
00405 ** EVERYTHING ELSE.  IF YOU ADD A STATUS ADD THE VALUE TO ONE OF
00406 ** THESE GROUPS.
00407              88  PM-TYPE-STAT-1
00408                      VALUES ARE '0' '1' '4' '6' '7' '9'
00409                                 'C' 'F' 'T'.
00410              88  PM-TYPE-STAT-2
00411                      VALUES ARE '2' '3' '5' '8'.
00412              88  PM-BILLABLE-STATUS VALUES ARE '0' '1' '6'.
00413              88  PM-PENDING-STATUS
00414                                 VALUES ARE '2' '4' '5' '8' '9'.
00415              88  PM-PENDING-ISSUE-STATUS
00416                                 VALUES ARE '2' '5' '8'.
00417              88  PM-CANCEL-STATUS
00418                                 VALUES ARE '4' '7' '9' 'C'.
00419          16  PM-CANCEL-CAUSE-CD         PIC X(3).
00420          16  PM-CANCEL-DT               PIC XX.
00421          16  PM-REFUND-AMT              PIC S9(5)V99  COMP-3.
00422          16  PM-CALC-REFUND-AMT         PIC S9(5)V99  COMP-3.
00423          16  PM-DECLINE-CD              PIC X(3).
00424          16  PM-DECLINE-DT              PIC XX.
00425          16  PM-LAST-LAPSE-DT           PIC XX.
00426          16  PM-LAST-REINSTATE-DT       PIC XX.
00427          16  PM-SECURITY-ACCESS-CODE    PIC X.
00428          16  PM-PREV-CONTROL-PRIMARY.
00429              20  PM-PREV-COMPANY-CD          PIC X.
00430              20  PM-PREV-CARRIER             PIC X.
00431              20  PM-PREV-GROUPING.
00432                  24  PM-PREV-GROUPING-PREFIX PIC X(3).
00433                  24  PM-PREV-GROUPING-PRIME  PIC X(3).
00434              20  PM-PREV-STATE               PIC XX.
00435              20  PM-PREV-PRODUCER.
00436                  24  PM-PREV-PRODUCER-PREFIX PIC X(4).
00437                  24  PM-PREV-PRODUCER-PRIME  PIC X(6).
00438              20  PM-PREV-POLICY-EFF-DT       PIC XX.
00439              20  PM-PREV-REFERENCE-NUMBER.
00440                  24  PM-PREV-REFNO-PRIME     PIC X(18).
00441                  24  PM-PREV-REFNO-SFX       PIC XX.
00442          16  PM-ACTION-DT               PIC XX.
00443          16  PM-ACTION-CODE             PIC X(3).
00444          16  PM-ACTION-DT-2             PIC XX.
00445          16  PM-ACTION-CODE-2           PIC X(3).
00446          16  PM-ACTION-DT-3             PIC XX.
00447          16  PM-ACTION-CODE-3           PIC X(3).
00448          16  PM-ACTION-DT-4             PIC XX.
00449          16  PM-ACTION-CODE-4           PIC X(3).
00450          16  PM-ACTION-DT-5             PIC XX.
00451          16  PM-ACTION-CODE-5           PIC X(3).
00452
00453          16  PM-KEY-CHANGE              PIC X.
00454                  88  PM-NO-KEY-CHG   VALUES ARE ' ' 'N'.
00455                  88  PM-KEY-CHG           VALUE 'Y'.
00456          16  PM-KEY-CHANGE-DT           PIC XX.
00457
00458          16  PM-RTI-INDICATOR           PIC X.
00459          16  PM-REASON-CODE             PIC X(3).
00460          16  PM-IN-OUT-PROCESSING-IND   PIC X(1).
00461              88  PM-IN-OUT-PROCESSING   VALUE 'Y'.
00462              88  PM-NOT-IN-OUT-PROCESSING
00463                                            VALUE SPACES.
00464
00465      12  FILLER                            PIC X(12).
00466
00467 ******************************************************************
00468 *                 AGENT AND COMMISSION DATA                      *
00469 ******************************************************************
00470
00471      12  PM-COMMISSION-DATA.
00472          16  PM-REMIT-TO                PIC S9(3) COMP-3.
00473          16  PM-COMM-CHANGE-SW          PIC X.
00474                  88  PM-COMMISSION-CHANGE  VALUE 'Y'.
00475          16  PM-AGENT-INFORMATION OCCURS  5 TIMES.
00476              20  PM-AGENT-NUMBER        PIC X(10).
00477              20  PM-AGENT-TYPE          PIC X.
00478                  88  PM-PRODUCER-LEVEL-AGENT
00479                                               VALUES ARE 'C' 'D'.
00480                  88  PM-AGENT-GROSS        VALUE 'C'.
00481                  88  PM-AGENT-REINS        VALUE 'R'.
00482                  88  PM-AGENT-GROSS-REINS  VALUE 'D'.
00483                  88  PM-OVERWRITE-GROSS    VALUE 'O'.
00484                  88  PM-OVERWRITE-GROSS-REINS VALUE 'P'.
00485                  88  PM-OVERWRITE-REINS    VALUE 'T'.
00486                  88  PM-REINS-ONLY         VALUE 'W'.
00487              20  PM-COMMISSION-BILL-PAID PIC X(1).
00488                  88  PM-GENERATE-BILL      VALUE 'B'.
00489                  88  PM-GENERATE-PAID      VALUE 'P'.
00490              20  PM-AGENT-COMP-1ST-YEAR PIC S99V999.
00491              20  PM-COMP-1ST-YEAR-TYPE  PIC X(1).
00492                  88  PM-COMP-1ST-YEAR-PERCENT VALUE '1'.
00493                  88  PM-COMP-1ST-YEAR-DOLLARS VALUE '2'.
00494                  88  PM-COMP-1ST-YEAR-NOT-USED VALUE '3'.
00495              20  PM-RENEWAL-DATA.
00496                  24  PM-AGENT-RENEWAL-DATA OCCURS 6 TIMES.
00497                      28  PM-RENEW-MONTHS  PIC S999    COMP-3.
00498                      28  PM-RENEW-COMMISSION
00499                                              PIC S99V999 COMP-3.
00500                      28  PM-RENEW-TYPE    PIC X(1).
00501                          88  PM-COMP-RENEW-PERCENT   VALUE '1'.
00502                          88  PM-COMP-RENEW-DOLLARS   VALUE '2'.
00503                          88  PM-COMP-RENEW-NOT-USED  VALUE '3'.
00504              20  PM-COMP-RECALC-FLAG    PIC X(1).
00505                  88  PM-BYPASS-RECALC      VALUE 'N'.
00506      12  FILLER                            PIC X(20).
00507 ******************************************************************
00508 *             CUSTOMER DATA                                      *
00509 ******************************************************************
00510      12  PM-CUSTOMER-ID                 PIC X(20).
00511 ******************************************************************
00512      12  FILLER                            PIC X(43).
00513 ******************************************************************
00627      EJECT
00628 *                COPY MTCPLAN REPLACING ==:TAG:== BY ==PP==.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MTCPLAN                             *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   MORTGAGE SYSTEM PRODUCER PLAN MASTER FILE.                   *
00007 *                                                                *
00008 *   THIS COPYBOOK IS USED FOR THE ONLINE                         *
00009 *   PLAN CODE MASTER FILE.                                       *
00010 *                                                                *
00011 *   FILE DESCRIPTION = PRODUCER PLAN MASTER                      *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 450  RECFORM = FIX                             *
00015 *                                                                *
00016 *   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
00017 *       ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25   *
00018 *                                                                *
00019 *   LOG = NO                                                     *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 *                                                                *
00022 *                                                                *
00023 ******************************************************************
00024
00025  01  PRODUCER-PLANS.
00026      12  PP-RECORD-ID                   PIC  X(02).
00027          88  VALID-PP-ID                      VALUE 'PP'.
00028
00029 ******************************************************************
00030 *   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
00031 ******************************************************************
00032
00033      12  PP-CONTROL-PRIMARY.
00034          16  PP-PROD-PRIMARY.
00035              20  PP-COMPANY-CD          PIC  X(01).
00036              20  PP-CONTROL-A.
00037                  24  PP-CARRIER         PIC  X(01).
00038                  24  PP-GROUPING.
00039                      28  PP-GROUPING-PREFIX
00040                                            PIC  X(03).
00041                      28  PP-GROUPING-PRIME PIC X(03).
00042                  24  PP-STATE           PIC  X(02).
00043                  24  PP-PRODUCER.
00044                      28  PP-PRODUCER-PREFIX
00045                                            PIC  X(04).
00046                      28  PP-PRODUCER-PRIME PIC X(06).
00047          16  PP-PRODUCER-PLAN.
00048              20  PP-PLAN-CODE           PIC  X(02).
00049              20  PP-PLAN-REVISION       PIC  9(03).
00050      12  FILLER                            PIC  X(20).
00051
00052 ******************************************************************
00053 *      ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25    *
00054 ******************************************************************
00055
00056      12  PP-CONTROL-BY-VAR-GRP.
00057          16  PP-COMPANY-CD-A1           PIC  X(01).
00058          16  PP-VG-CARRIER              PIC  X(01).
00059          16  PP-VG-GROUPING             PIC  X(06).
00060          16  PP-VG-STATE                PIC  X(02).
00061          16  PP-VG-PRODUCER             PIC  X(10).
00062          16  PP-VG-PLAN-CODE            PIC  X(02).
00063          16  PP-VG-PLAN-REVISION        PIC  X(03).
00064      12  FILLER                            PIC  X(20).
00065
00066 ******************************************************************
00067 *                PRODUCER SECURITY DATA                          *
00068 ******************************************************************
00069
00070      12  PP-SECURITY-ACCESS-CODE        PIC  X(01).
00071      12  PP-POLICY-CNT                  PIC S9(07)    COMP-3.
00072
00073 ******************************************************************
00074 *                FILE SYNCHRONIZATION DATA                       *
00075 ******************************************************************
00076
00077      12  PP-MAINT-INFORMATION.
00078          16  PP-LAST-MAINT-DATE         PIC  X(02).
00079          16  PP-LAST-MAINT-HHMMSS       PIC S9(07)    COMP-3.
00080          16  PP-LAST-MAINT-USER         PIC  X(04).
00081      12  FILLER                            PIC  X(10).
00082
00083 ******************************************************************
00084 *                   CRITICAL FILE DATES                          *
00085 ******************************************************************
00086
00087      12  PP-PLAN-DATES.
00088          16  PP-PLAN-EFFECT-DATE        PIC  X(02).
00089          16  PP-PLAN-EXPIRE-DATE        PIC  X(02).
00090
00091      12  FILLER                            PIC  X(10).
00092
00093 ******************************************************************
00094 *                GENERAL INFORMATION                             *
00095 ******************************************************************
00096
00097      12  PP-GENERAL-INFORMATION.
00098          16  PP-ALPHA-SEARCH-SW         PIC  X(01).
00099              88  PP-MIB-ALPHA-ALL           VALUE '1'.
00100              88  PP-MIB-ALPHA-NONE          VALUE '2'.
00101              88  PP-MIB-ALPHA-EXCEEDED      VALUE '3'.
00102              88  PP-CLIENT-ALPHA-ALL        VALUE 'A'.
00103              88  PP-CLIENT-ALPHA-NONE       VALUE 'B'.
00104              88  PP-CLIENT-ALPHA-EXCEEDED   VALUE 'C'.
00105              88  PP-BOTH-ALPHA-ALL          VALUE 'X'.
00106              88  PP-BOTH-ALPHA-NONE         VALUE 'Y'.
00107              88  PP-BOTH-ALPHA-EXCEEDED     VALUE 'Z'.
00108              88  PP-ALPHA-SEARCH-VALID VALUES ARE '1' '2' '3'
00109                                                      'A' 'B' 'C'
00110                                                      'X' 'Y' 'Z'.
00111          16  PP-BENEFIT-TYPE            PIC  X(01).
00112              88  PP-BENEFIT-IS-LEVEL         VALUE '1'.
00113              88  PP-BENEFIT-REDUCES          VALUE '2'.
00114          16  PP-DAYS-TO-1ST-NOTICE      PIC  9(02).
00115          16  PP-DAYS-TO-2ND-NOTICE      PIC  9(02).
00116          16  PP-DAYS-TO-3RD-NOTICE      PIC  9(02).
00117          16  PP-DAYS-TO-4TH-NOTICE      PIC  9(02).
00118          16  PP-EFF-DT-RULE-SW          PIC  X(01).
00119              88  PP-EFF-DT-ENTER            VALUE 'E'.
00120              88  PP-EFF-DT-MONTH            VALUE 'M'.
00121              88  PP-EFF-DT-QTR              VALUE 'Q'.
00122              88  PP-EFF-DT-SEMI             VALUE 'S'.
00123              88  PP-EFF-DT-ANN              VALUE 'A'.
00124          16  PP-FREE-EXAM-DAYS          PIC S9(03)   COMP-3.
00125          16  PP-GRACE-PERIOD            PIC S9(03)   COMP-3.
00126          16  PP-HEALTH-QUESTIONS        PIC  9(01).
00127          16  PP-NUMBER-LAPSE-NOTICES    PIC S9(03)   COMP-3.
00128          16  PP-MIB-SEARCH-SW           PIC  X(01).
00129              88  PP-MIB-SEARCH-ALL          VALUE '1'.
00130              88  PP-MIB-SEARCH-NONE         VALUE '2'.
00131              88  PP-MIB-SEARCH-EXCEEDED     VALUE '3'.
00132              88  PP-MIB-SEARCH-VALID   VALUES ARE '1' '2' '3'.
00133          16  PP-PLAN-ABBREV             PIC  X(03).
00134          16  PP-PLAN-AGES.
00135              20  PP-MINIMUM-AGE         PIC S9(03)   COMP-3.
00136              20  PP-MAXIMUM-AGE         PIC S9(03)   COMP-3.
00137              20  PP-MAXIMUM-ATTAIN-AGE  PIC S9(03)   COMP-3.
00138          16  PP-PLAN-BENEFITS.
00139              20  PP-CLAIM-CAP           PIC S9(07)V99 COMP-3.
00140              20  PP-MINIMUM-BENEFIT     PIC S9(07)V99 COMP-3.
00141              20  PP-MAXIMUM-BENEFIT     PIC S9(07)V99 COMP-3.
00142              20  PP-MAXIMUM-MONTHLY-BENEFIT
00143                                            PIC S9(07)V99 COMP-3.
00144          16  PP-PLAN-DESCRIPTION        PIC  X(10).
00145          16  PP-POLICY-FEE              PIC S9(03)V9(02)
00146                                                         COMP-3.
00147          16  PP-PLAN-IND-GRP            PIC  X(01).
00148          16  PP-PLAN-SNGL-JNT           PIC  X(01).
00149              88  PP-COMBINED-PLAN          VALUE 'C'.
00150              88  PP-JNT-PLAN               VALUE 'J'.
00151              88  PP-SNGL-PLAN              VALUE 'S'.
00152          16  PP-PLAN-TERMS.
00153              20  PP-MINIMUM-TERM        PIC S9(03)   COMP-3.
00154              20  PP-MAXIMUM-TERM        PIC S9(03)   COMP-3.
00155          16  PP-PLAN-TYPE               PIC  X(01).
00156              88  PP-AH-MORT-PLAN           VALUE 'A'.
00157              88  PP-AD-D-MORT-PLAN         VALUE 'E'.
00158              88  PP-DISMEM-MORT-PLAN       VALUE 'D'.
00159              88  PP-LIFE-MORT-PLAN         VALUE 'L'.
00160          16  PP-PREMIUM-TOLERANCES.
00161              20  PP-PREM-TOLERANCE      PIC S9(03)   COMP-3.
00162              20  PP-PREM-TOLERANCE-PCT  PIC SV9(03)  COMP-3.
00163          16  PP-RATE-CODE               PIC  X(05).
00164          16  PP-REOCCURRING-DISABILITY-PRD PIC S9(03) COMP-3.
00165          16  PP-REPLACEMENT-LAW-SW      PIC  X(01).
00166              88  PP-NO-REPLACE             VALUE '1'.
00167              88  PP-REPLACE-APPLIES        VALUE '2'.
00168              88  PP-VALID-REPLACEMENT-LAW  VALUE '1' '2'.
00169          16  PP-RETRO-RETENTION         PIC S9V9(04) COMP-3.
00170          16  PP-RERATE-CNTL             PIC  X(01).
00171              88  PP-RERATE-WITH-ISSUE-AGE    VALUE '1'.
00172              88  PP-RERATE-WITH-CURRENT-AGE  VALUE '2'.
00173              88  PP-DO-NOT-RERATE            VALUE '3' ' '.
00174              88  PP-AUTO-RECALC              VALUE '4'.
00175          16  PP-SEX-RATING              PIC  X(01).
00176              88  PP-NOT-SEX-RATED          VALUE '1'.
00177              88  PP-SEX-RATED              VALUE '2'.
00178          16  PP-SUBSTANDARD-DATA.
00179              20  PP-SUBSTANDARD-PERCENT PIC S9(01)V9(04).
00180              20  PP-SUBSTANDARD-TYPE    PIC  X(01).
00181                  88  PP-PCT-OF-BENEFIT     VALUE '1'.
00182                  88  PP-PCT-OF-PREMIUM     VALUE '2'.
00183                  88  PP-NOT-APPLICABLE     VALUE '3'.
00184          16  PP-YEARS-TO-NEXT-RERATE    PIC  9(02).
00185          16  PP-DEPENDANT-COVERAGE      PIC  X(01).
00186              88  PP-DEP-COVERED            VALUE 'Y'.
00187              88  PP-DEP-NOT-COVERED        VALUE 'N' ' '.
00188          16  PP-REFUND-CALC             PIC  X(01).
00189              88  PP-RFND-MP-REFUND  VALUES ARE ' ' LOW-VALUES.
00190              88  PP-RFND-BY-R78            VALUE '1'.
00191              88  PP-RFND-BY-PRO-RATA       VALUE '2'.
00192              88  PP-RFND-AS-CALIF          VALUE '3'.
00193              88  PP-RFND-AS-TEXAS          VALUE '4'.
00194              88  PP-RFND-IS-NET-PAY        VALUE '5'.
00195              88  PP-RFND-ANTICIPATION      VALUE '6'.
00196              88  PP-RFND-MEAN              VALUE '8'.
00197              88  PP-VALID-REFUND    VALUES ARE ' ' '1' '2' '3'
00198                                                   '4' '5' '6' '8'
00199                                                   LOW-VALUES.
00200          16  PP-ALT-RATE-CODE           PIC  X(05).
00201
00202      12  FILLER                            PIC  X(39).
00203
00204 ******************************************************************
00205 *                     PLAN FORMS AND LETTERS                     *
00206 ******************************************************************
00207
00208      12  PP-PLAN-MASTER-FORMS.
00209          16  PP-POLICY-FORM             PIC  X(12).
00210          16  PP-MASTER-APPLICATION      PIC  X(12).
00211          16  PP-MASTER-POLICY           PIC  X(12).
00212      12  PP-DELINQUENCY-NOTICE-FORMS.
00213          16  PP-1ST-NOTICE-FORM         PIC  X(04).
00214          16  PP-2ND-NOTICE-FORM         PIC  X(04).
00215          16  PP-3RD-NOTICE-FORM         PIC  X(04).
00216          16  PP-4TH-NOTICE-FORM         PIC  X(04).
00217      12  FILLER                            PIC  X(32).
00218      12  PP-TERMINATION-FORM            PIC  X(04).
00219      12  FILLER                            PIC  X(08).
00220      12  PP-ISSUE-LETTER                PIC  X(04).
00221
00222      12  FILLER                            PIC  X(80).
00223 ******************************************************************
00629      EJECT
061013*                COPY MTCPROD REPLACING ==:TAG:== BY ==PDM==.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MTCPROD                             *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   MORTGAGE SYSTEM PRODUCER MASTER FILE                         *
00007 *                                                                *
00008 *   THIS COPYBOOK IS USED FOR THE ONLINE                         *
00009 *   VSAM PRODUCER MASTER FILE.                                   *
00010 *                                                                *
00011 *   FILE DESCRIPTION = PRODUCER MASTER FILE                      *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 2000 RECFORM = FIXED                           *
00015 *                                                                *
00016 *   BASE CLUSTER NAME = MPPROD                    RKP=02,LEN=22  *
00017 *       ALTERNATE PATH1 = MPPROD2 (ALT GROUPING)  RKP=48,LEN=22  *
00018 *       ALTERNATE PATH2 = MPPROD3 (PRODUCER NAME) RKP=90,LEN=56  *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
00025
00026  01  PRODUCER-MASTER.
00027      12  PDM-RECORD-ID              PIC  X(02).
00028          88  PDM-VALID-ID                VALUE 'PD'.
00029
00030 ******************************************************************
00031 *   BASE CLUSTER NAME = MPPROD                    RKP=2,LEN=22   *
00032 ******************************************************************
00033
00034      12  PDM-CONTROL-PRIMARY-BATCH.
00035          16  FILLER                   PIC  X(20).
00036          16  PDM-EXPIRE-DT.
00037              20  PDM-EXPIRE-DT-YY   PIC  9(02).
00038              20  PDM-EXPIRE-DT-MM   PIC  9(02).
00039              20  PDM-EXPIRE-DT-DD   PIC  9(02).
00040      12  FILLER REDEFINES PDM-CONTROL-PRIMARY-BATCH.
00041          16  PDM-CONTROL-PRIMARY.
00042              20  PDM-COMPANY-CD     PIC  X(01).
00043              20  PDM-MSTR-CNTRL.
00044                  24  PDM-CONTROL-A.
00045                      28  PDM-CARRIER PIC X(01).
00046                      28  PDM-GROUPING.
00047                          32 PDM-GROUPING-PREFIX
00048                                       PIC  X(03).
00049                          32 PDM-GROUPING-PRIME
00050                                       PIC  X(03).
00051                      28  PDM-STATE  PIC  X(02).
00052                      28  PDM-PRODUCER.
00053                          32  PDM-PRODUCER-PREFIX
00054                                       PIC  X(04).
00055                          32  PDM-PRODUCER-PRIME
00056                                       PIC  X(06).
00057                  24  PDM-CNTRL-B.
00058                      28  PDM-EXPIRE-DATE
00059                                       PIC  X(02).
00060          16  FILLER REDEFINES PDM-CONTROL-PRIMARY.
00061              20  FILLER               PIC  X(01).
00062              20  PDM-CGSPE-KEY      PIC  X(21).
00063          16  FILLER                   PIC  X(04).
00064      12  FILLER                       PIC  X(20).
00065
00066 ******************************************************************
00067 *      ALTERNATE PATH1 = MPPROD2 (ALT GROUPING) RKP=48,LEN=22    *
00068 ******************************************************************
00069
00070      12  PDM-CONTROL-BY-VAR-GRP.
00071          16  PDM-VG-CCGSP-KEYLET.
00072              20  PDM-COMPANY-CD-A1  PIC  X(01).
00073              20  PDM-VG-CARRIER     PIC  X(01).
00074              20  PDM-VG-GROUPING    PIC  X(06).
00075              20  PDM-VG-STATE       PIC  X(02).
00076              20  PDM-VG-PRODUCER    PIC  X(10).
00077          16  PDM-VG-DATE.
00078              24  PDM-VG-EXPIRE-DATE PIC  X(02).
00079      12  FILLER                       PIC  X(20).
00080
00081
00082 ******************************************************************
00083 *      ALTERNATE PATH2 = MPPROD3 (NAME)         RKP=90,LEN=56    *
00084 ******************************************************************
00085
00086      12  PDM-CONTROL-BY-NAME.
00087          16  PDM-COMPANY-CD-A2      PIC  X(01).
00088          16  PDM-NAME-A2            PIC  X(30).
00089          16  PDM-CGSPE-KEY-A2.
00090              20  PDM-CARRIER-A2     PIC  X(01).
00091              20  PDM-GROUPING-A2    PIC  X(06).
00092              20  PDM-STATE-A2       PIC  X(02).
00093              20  PDM-PRODUCER-A2    PIC  X(10).
00094              20  PDM-EXPIRE-DATE-A2 PIC  X(02).
00095          16  PDM-CURRENT-DATE-BIN-A2 PIC X(02).
00096          16  PDM-CURRENT-TIME-BIN-A2 PIC S9(04) COMP.
00097      12  FILLER                       PIC  X(20).
00098
00099 ******************************************************************
00100 *                FILE SYNCHRONIZATION DATA                       *
00101 ******************************************************************
00102
00103      12  PDM-MAINT-INFORMATION.
00104          16  PDM-LAST-MAINT-DATE    PIC  X(02).
00105          16  PDM-LAST-MAINT-HHMMSS  PIC S9(07) COMP-3.
00106          16  PDM-LAST-MAINT-USER    PIC  X(04).
00107
00108 ******************************************************************
00109 *                PRODUCER SECURITY DATA                          *
00110 ******************************************************************
00111
00112      12  PDM-SECURITY-ACCESS-CODE   PIC  X(01).
00113
00114 ******************************************************************
00115 *                DATES                                           *
00116 ******************************************************************
00117
00118      12  PDM-ANNIVERSARY-DATE       PIC  X(02).
00119
00120      12  PDM-AR-HI-DATE.
00121          16  PDM-AR-HI-POLICY-DATE  PIC  X(02).
00122          16  FILLER                   PIC  X(04).
00123      12  PDM-AR-HI-POLICY-DT REDEFINES PDM-AR-HI-DATE.
00124          16  PDM-AR-HI-POLICY-DT-YY PIC  9(02).
00125          16  PDM-AR-HI-POLICY-DT-MM PIC  9(02).
00126          16  PDM-AR-HI-POLICY-DT-DD PIC  9(02).
00127
00128      12  PDM-ENTRY-DATE             PIC  X(02).
00129
00130      12  PDM-EFFECT-DTE.
00131          16  PDM-EFFECT-DATE        PIC  X(02).
00132          16  FILLER                   PIC  X(04).
00133      12  PDM-EFFECT-DT REDEFINES PDM-EFFECT-DTE.
00134          16  PDM-EFFECT-DT-YY       PIC  9(02).
00135          16  PDM-EFFECT-DT-MM       PIC  9(02).
00136          16  PDM-EFFECT-DT-DD       PIC  9(02).
00137
00138      12  PDM-HI-DATE.
00139          16  PDM-HI-POLICY-DATE     PIC  X(02).
00140          16  FILLER                   PIC  X(04).
00141      12  PDM-HI-POLICY-DT REDEFINES PDM-HI-DATE.
00142          16  PDM-HI-POLICY-DT-YY    PIC  9(02).
00143          16  PDM-HI-POLICY-DT-MM    PIC  9(02).
00144          16  PDM-HI-POLICY-DT-DD    PIC  9(02).
00145
00146      12  PDM-INACTIVE-DATE          PIC  X(02).
00147
00148      12  PDM-LO-DATE.
00149          16  PDM-LO-POLICY-DATE     PIC  X(02).
00150          16  FILLER                   PIC  X(04).
00151      12  PDM-LO-POLICY-DT REDEFINES PDM-LO-DATE.
00152          16  PDM-LO-POLICY-DT-YY    PIC  9(02).
00153          16  PDM-LO-POLICY-DT-MM    PIC  9(02).
00154          16  PDM-LO-POLICY-DT-DD    PIC  9(02).
00155
00156      12  PDM-POLICIES-PURGED-DATE   PIC  X(02).
00157
00158      12  PDM-PREV-DATES.
00159          16  PDM-PREV-EFF-DATE      PIC  X(02).
00160          16  FILLER                   PIC  X(04).
00161          16  PDM-PREV-EXP-DATE      PIC  X(02).
00162          16  FILLER                   PIC  X(04).
00163      12  PDM-PREV-DTS REDEFINES PDM-PREV-DATES.
00164          16  PDM-PREV-EFF-DT.
00165              20  PDM-PREV-EFF-DT-YY PIC  9(02).
00166              20  PDM-PREV-EFF-DT-MM PIC  9(02).
00167              20  PDM-PREV-EFF-DT-DD PIC  9(02).
00168          16  PDM-PREV-EXP-DT.
00169              20  PDM-PREV-EXP-DT-YY PIC  9(02).
00170              20  PDM-PREV-EXP-DT-MM PIC  9(02).
00171              20  PDM-PREV-EXP-DT-DD PIC  9(02).
00172
00173      12  PDM-1ST-PROD-DATE          PIC  X(02).
00174
00175      12  FILLER                       PIC  X(20).
00176
00177 ******************************************************************
00178 *                MORTGAGE BILLING DATA                           *
00179 ******************************************************************
00180
00181      12  PDM-CONTACT                PIC  X(30).
00182      12  PDM-BILLING-MONTHS.
00183          16  PDM-BILLING-MONTH-ANNUAL PIC 9(02).
00184          16  PDM-BILLING-MONTH-SEMIANN PIC 9(02).
00185      12  PDM-BILLING-ADVANCE-ARREARS PIC X(01).
00186          88  PDM-BILL-ADVANCE           VALUE '1'.
00187          88  PDM-BILL-ARREARS           VALUE '2'.
00188      12  PDM-BILLING-MODE           PIC  X(01).
00189          88  PDM-ANNUAL-BILL            VALUE '1'.
00190          88  PDM-SEMI-ANNUAL-BILL       VALUE '2'.
00191          88  PDM-QUARTERLY-BILL         VALUE '3'.
00192          88  PDM-MONTHLY-BILL           VALUE '4'.
00193          88  PDM-BI-MONTHLY-BILL        VALUE '5'.
00194          88  PDM-SINGLE-PREM-BILL       VALUE '6'.
00195      12  PDM-BILLING-GROUPING-CODE  PIC  X(06).
00196      12  PDM-BILLING-SCHEDULE       PIC  X(01).
00197          88  PDM-BILL-1ST-WEEK          VALUE '1'.
00198          88  PDM-BILL-2ND-WEEK          VALUE '2'.
00199          88  PDM-BILL-3RD-WEEK          VALUE '3'.
00200          88  PDM-BILL-4TH-WEEK          VALUE '4'.
00201          88  PDM-BILL-5TH-WEEK          VALUE '5'.
00202          88  PDM-HOLD-BILL              VALUE '6'.
00203          88  PDM-NO-BILL                VALUE '7'.
00204      12  PDM-BILLING-SEQUENCE       PIC  X(01).
00205          88  PDM-BILL-NAME-SEQU         VALUE '1'.
00206          88  PDM-BILL-LOAN-SEQU         VALUE '2'.
00207          88  PDM-BILL-PLCY-SEQU         VALUE '3'.
00208      12  PDM-BILLING-TYPE           PIC  X(01).
00209          88  PDM-LIST-BILL              VALUE '1'.
00210          88  PDM-TAPE-BILL              VALUE '2'.
00211          88  PDM-TAPE-LIST-BILL         VALUE '3'.
00212          88  PDM-GROUP-BILL         VALUES ARE '1' '2' '3'.
00213          88  PDM-DIRECT-BILL            VALUE '4'.
00214          88  PDM-PAC                VALUES ARE '5' 'C' 'S'.
00215          88  PDM-CREDIT-CARD            VALUE '6'.
00216          88  PDM-INDIV-BILL
00217                               VALUES ARE '4' '5' '6' 'C' 'S'.
00218          88  PDM-GROUP-BY-POLICY        VALUE '7'.
00219          88  PDM-GROUP-BY-POLICY-PAC    VALUE '8'.
00220          88  PDM-GROUP-BY-POLICY-CRDC   VALUE '9'.
00221          88  PDM-GROUP-BY-BILL          VALUE '7' '8' '9'.
00222          88  PDM-GROUP-BY-PROD          VALUE 'A'.
00223          88  PDM-EFT-CHECKING           VALUE 'C'.
00224          88  PDM-EFT-SAVINGS            VALUE 'S'.
00225      12  PDM-DATE-PAID              PIC  X(02).
00226      12  PDM-LAST-BILLING-DATE      PIC  X(02).
00227      12  PDM-LAST-BILL-TO-DATE      PIC  X(02).
00228      12  PDM-MAX-MONTHS-BILL        PIC S9(03)  COMP-3.
00229      12  PDM-PAID-TO-DATE           PIC  X(02).
00230      12  PDM-PREV-BILLING-DATE      PIC  X(02).
00231      12  PDM-PREV-BILL-TO-DATE      PIC  X(02).
00232
00233      12  FILLER                       PIC  X(20).
00234
00235 ******************************************************************
00236 *                PERSONAL DATA                                   *
00237 ******************************************************************
00238
00239      12  PDM-ADDRS                  PIC  X(30).
00240      12  PDM-CITY                   PIC  X(30).
00241      12  PDM-CITY-CODE              PIC  X(04).
00242      12  PDM-COUNTY-CODE            PIC  X(03).
00243      12  PDM-NAME                   PIC  X(30).
00244      12  PDM-PARRISH-CODE           PIC  X(03).
00245      12  PDM-PERSON                 PIC  X(30).
00246      12  PDM-TEL-NO.
00247          16  PDM-AREA-CODE          PIC  9(03).
00248          16  PDM-TEL-PRE            PIC  9(03).
00249          16  PDM-TEL-NBR            PIC  9(04).
00250      12  PDM-ZIP.
00251          16  PDM-ZIP-PRIME          PIC  X(05).
00252          16  PDM-ZIP-PLUS4          PIC  X(04).
00253      12  PDM-LANGUAGE-IND           PIC  X(01).
00254          88  PDM-ENGLISH                       VALUE 'E'.
00255          88  PDM-FRENCH                        VALUE 'F'.
00256          88  PDM-SPANISH                       VALUE 'S'.
00257
00258      12  FILLER                       PIC  X(19).
00259
00260 ******************************************************************
00261 *                REINSURANCE DATA                                *
00262 ******************************************************************
00263
00264      12  PDM-REINS-TBL-CODE         PIC  X(03).
00265      12  PDM-REIN-RECALC            PIC  X(01).
00266
00267      12  PDM-REI-AH-FEE             PIC S9(01)V9(04) COMP-3.
00268      12  PDM-REI-AH-PE              PIC  X(01).
00269      12  PDM-REI-AH-TAX             PIC S9(01)V9(04) COMP-3.
00270
00271      12  PDM-REI-GROUP-A            PIC  X(06).
00272      12  PDM-REI-GROUP-B            PIC  X(06).
00273
00274      12  PDM-REI-LF-FEE             PIC S9(01)V9(04) COMP-3.
00275      12  PDM-REI-LF-PE              PIC  X(01).
00276      12  PDM-REI-LF-TAX             PIC S9(01)V9(04) COMP-3.
00277
00278      12  PDM-REI-MORT               PIC  X(04).
00279      12  PDM-REI-PRT-OW             PIC  X(01).
00280      12  PDM-REI-PRT-ST             PIC  X(01).
00281
00282      12  PDM-REI-ADD-FEE            PIC S9(01)V9(04) COMP-3.
00283      12  PDM-REI-ADD-PE             PIC  X(01).
00284      12  PDM-REI-ADD-TAX            PIC S9(01)V9(04) COMP-3.
00285
00286      12  PDM-REI-DIS-FEE            PIC S9(01)V9(04) COMP-3.
00287      12  PDM-REI-DIS-PE             PIC  X(01).
00288      12  PDM-REI-DIS-TAX            PIC S9(01)V9(04) COMP-3.
00289
00290      12  FILLER                       PIC  X(10).
00291 ******************************************************************
00292 *                RETRO DATA                                      *
00293 ******************************************************************
00294
00295      12  PDM-RET-AH                 PIC S9(01)V9(04) COMP-3.
00296      12  PDM-RET-GRP                PIC  X(06).
00297      12  PDM-RET-LF                 PIC S9(01)V9(04) COMP-3.
00298      12  PDM-RET-MIN-LOSS-A         PIC SV9(03)      COMP-3.
00299      12  PDM-RET-MIN-LOSS-L         PIC SV9(03)      COMP-3.
00300      12  PDM-RET-P-E                PIC  X(01).
00301      12  PDM-RET-ST-TAX-USE         PIC  X(01).
00302          88  PDM-CHARGE-ST-TAXES-ON-RETRO   VALUE 'Y' 'E' 'P'.
00303          88  PDM-TAXES-NOT-IN-RETRO         VALUE 'N' ' '.
00304      12  PDM-RET-Y-N                PIC  X(01).
00305      12  PDM-RET-ADD                PIC S9(01)V9(04) COMP-3.
00306      12  PDM-RET-MIN-LOSS-ADD       PIC SV9(03)      COMP-3.
00307      12  PDM-RET-DIS                PIC S9(01)V9(04) COMP-3.
00308      12  PDM-RET-MIN-LOSS-DIS       PIC SV9(03)      COMP-3.
00309
00310      12  FILLER                       PIC  X(10).
00311
00312 ******************************************************************
00313 *                     MANAGEMENT OPTIONS                         *
00314 ******************************************************************
00315
00316      12  PDM-DEFAULT-UNWTR-CODE     PIC  X(03).
00317      12  PDM-LAPSE-NOTICE-CNTL      PIC  X(01).
00318      12  PDM-CORRESPONDENCE-CNTL    PIC  X(01).
00319      12  PDM-RETAIN-BILLING-DATA-MTHS PIC S9(03) COMP-3.
00320      12  PDM-RETAIN-CLAIM-DATA-MTHS PIC S9(03)  COMP-3.
00321      12  PDM-RETAIN-COMMISSION-MTHS PIC S9(03)  COMP-3.
00322      12  PDM-RETAIN-DELINQUENCY-MTHS PIC S9(03) COMP-3.
00323      12  PDM-RETAIN-INSD-PROFILE-MTHS PIC S9(03) COMP-3.
00324      12  PDM-RETAIN-INS-COVERAGE-MTHS PIC S9(03) COMP-3.
00325      12  PDM-RETAIN-STATUS-DISP-MTHS PIC S9(03) COMP-3.
00326      12  PDM-NUM-BILLING-CYCLES-RETAINED
00327                                       PIC S9(03)  COMP-3.
00328      12  PDM-RETAIN-UNDERWRITER-HST-MTHS
00329                                       PIC S9(03)  COMP-3.
00330
00331      12  FILLER                       PIC X(098).
00332
00333
00334 ******************************************************************
00335 *                MISCELLANEOUS DATA                              *
00336 ******************************************************************
00337
00338      12  PDM-AH-RPT021-EXP-PCT      PIC S9(03)V9(04) COMP-3.
00339      12  PDM-AUTO-REFUND-SW         PIC  X(01).
00340          88  PDM-AUTO-REFUNDS-USED          VALUE 'Y'.
00341          88  PDM-AUTO-REFUNDS-NOT-USED      VALUE 'N' ' '.
00342      12  PDM-BUSINESS-TYPE          PIC  9(02).
00343      12  PDM-CAL-TABLE              PIC  X(02).
00344      12  PDM-COMMENTS.
00345          16  PDM-COMMENT-LINE       PIC  X(50)
00346                                            OCCURS 5 TIMES.
00347      12  PDM-EMPLOYER-STMT-USED     PIC  X(01).
00348      12  PDM-GROUPED-CHECKS-Y-N     PIC  X(01).
00349      12  PDM-IG                     PIC  X(01).
00350          88  PDM-HAS-INDIVIDUAL             VALUE 'I'
00351                                                     '1'.
00352          88  PDM-HAS-GROUP                  VALUE 'G'
00353                                                     '2'.
00354      12  PDM-LF-RPT021-EXP-PCT      PIC S9(03)V9(04) COMP-3.
00355      12  PDM-REPORT-CODE-1          PIC  X(10).
00356      12  PDM-REPORT-CODE-2          PIC  X(10).
00357      12  PDM-RPT045A-SWITCH         PIC  X(01).
00358          88  PDM-RPT045A-OFF             VALUE 'N'.
00359      12  PDM-SPECIAL-BILLING-FREQ   PIC  X(01).
00360          88  PDM-HAS-SPECIAL-BILL-FREQ      VALUE 'Y'.
00361          88  PDM-NO-SPECIAL-BILL-FREQ       VALUE 'N' ' '.
00362      12  PDM-STATUS                 PIC  X(01).
00363          88  PDM-STATUS-ACTIVE              VALUE '0'.
00364          88  PDM-STATUS-INACTIVE            VALUE '1'.
00365      12  PDM-STD-AH-TYPE            PIC  X(02).
00366      12  PDM-TAX-NUMBER             PIC  X(11).
00367      12  PDM-TOL-CLM                PIC S9(03)V9(02) COMP-3.
00368      12  PDM-USER-FIELDS.
00369          16  PDM-USER-FLD-1         PIC  X(02).
00370          16  PDM-USER-FLD-2         PIC  X(02).
00371          16  PDM-USER-FLD-3         PIC  X(02).
00372          16  PDM-USER-FLD-4         PIC  X(02).
00373          16  PDM-USER-FLD-5         PIC  X(02).
00374      12  PDM-USER-SELECT-OPTIONS.
00375          16  PDM-USER-SELECT-1      PIC  X(10).
00376          16  PDM-USER-SELECT-2      PIC  X(10).
00377          16  PDM-USER-SELECT-3      PIC  X(10).
00378          16  PDM-USER-SELECT-4      PIC  X(10).
00379          16  PDM-USER-SELECT-5      PIC  X(10).
00380      12  PDM-DIS-RPT021-EXP-PCT     PIC S9(03)V9(04) COMP-3.
00381      12  PDM-ADD-RPT021-EXP-PCT     PIC S9(03)V9(04) COMP-3.
00382      12  FILLER                       PIC  X(20).
00383
00384 ******************************************************************
00385 *                CLIENT USE AREAS                                *
00386 ******************************************************************
00387
00388      12  PDM-CLIENT-USE-AREA-1      PIC  X(30).
00389      12  PDM-CLIENT-USE-AREA-2      PIC  X(30).
00390      12  PDM-CLIENT-USE-AREA-3      PIC  X(11).
00391      12  PDM-CLIENT-USE-AREA-4      PIC  X(30).
00392      12  PDM-CLIENT-USE-AREA-5      PIC  X(30).
00393      12  PDM-CLIENT-USE-AREA-6      PIC  X(11).
00394      12  PDM-CLIENT-USE-AREA-7      PIC  X(30).
00395      12  PDM-CLIENT-USE-AREA-8      PIC  X(30).
00396      12  PDM-CLIENT-USE-AREA-9      PIC  X(11).
00397
00398 ******************************************************************
00399 *                TRANSFER DATA                                   *
00400 ******************************************************************
00401      12  PDM-TRANSFERRED-FROM.
00402          16  PDM-TRNFROM-CARRIER    PIC  X(01).
00403          16  PDM-TRNFROM-GROUPING.
00404              20  PDM-TRNFROM-GRP-PREFIX
00405                                       PIC  X(03).
00406              20  PDM-TRNFROM-GRP-PRIME PIC X(03).
00407          16  PDM-TRNFROM-STATE      PIC  X(02).
00408          16  PDM-TRNFROM-PRODUCER.
00409              20  PDM-TRNFROM-PROD-PREFIX
00410                                       PIC  X(04).
00411              20  PDM-TRNFROM-PROD-PRIME
00412                                       PIC  X(06).
00413          16  PDM-TRNFROM-DATE       PIC  X(02).
00414      12  PDM-TRANSFERRED-TO.
00415          16  PDM-TRNTO-CARRIER      PIC  X(01).
00416          16  PDM-TRNTO-GROUPING.
00417              20  PDM-TRNTO-GRP-PREFIX PIC X(03).
00418              20  PDM-TRNTO-GRP-PRIME PIC X(03).
00419          16  PDM-TRNTO-STATE        PIC  X(02).
00420          16  PDM-TRNTO-PRODUCER.
00421              20  PDM-TRNTO-PROD-PREFIX PIC X(04).
00422              20  PDM-TRNTO-PROD-PRIME PIC X(06).
00423          16  PDM-TRNTO-DATE         PIC  X(02).
00424      12  FILLER                       PIC  X(20).
00425
00426 ******************************************************************
00427 *                MORTGAGE PLANS SOLD                             *
00428 ******************************************************************
00429
00430      12  PDM-PLANS-SOLD.
00431          16  PDM-PRODUCER-PLANS OCCURS 40 TIMES
00432                                 INDEXED BY PDM-PLAN-NDX
00433                                            PDM-PLAN-NDX2.
00434              20  PDM-INDIVIDUAL-PLAN.
00435                  24  PDM-PLAN-CODE  PIC  X(02).
00436                  24  PDM-PLAN-REVISION PIC X(03).
00437              20  PDM-IBNR-PERCENT   PIC S9(01)V9(04) COMP-3.
00438      12  FILLER                       PIC  X(54).
00439
00440 ******************************************************************
00441 *                 AGENT AND COMMISSION DATA                      *
00442 ******************************************************************
00443
00444      12  PDM-COMMISSION-INFORMATION.
00445          16  PDM-REMIT-TO           PIC S9(03)   COMP-3.
00446          16  PDM-RECALCULATION-SW   PIC  X(01).
00447              88  PDM-RECALC-DETAIL          VALUE 'Y'.
00448              88  PDM-RECALC-NO-DETAIL       VALUE 'I'.
00449              88  PDM-IGNORE-RECALC          VALUE 'N'.
00450              88  PDM-VALID-RECALCULATION-SW VALUE 'Y' 'I' 'N'.
00451          16  PDM-AGENT-DATA.
00452              20  PDM-AGENT-ENTRY    OCCURS 5 TIMES
00453                                     INDEXED BY PDM-AGENT-NDX
00454                                                PDM-AGENT-NDX2.
00455                  24  PDM-AGENT-NUMBER PIC X(10).
00456                  24  PDM-AGENT-TYPE PIC  X(01).
00457                      88  PDM-AGENT-TYPE-A   VALUE 'C' 'D'.
00458                      88  PDM-AGENT-TYPE-G   VALUE 'O' 'R'
00459                                                     'P' 'T'
00460                                                     'W'.
00461                      88  PDM-AGENT-GROSS    VALUE 'C'.
00462                      88  PDM-AGENT-REINS    VALUE 'R'.
00463                      88  PDM-AGENT-GROSS-REINS VALUE 'D'.
00464                      88  PDM-OVERWRITE-GROSS VALUE 'O'.
00465                      88  PDM-OVERWRITE-GROSS-REINS
00466                                           VALUE 'P'.
00467                      88  PDM-OVERWRITE-REINS VALUE 'T'.
00468                      88  PDM-REINS-ONLY     VALUE 'W'.
00469                      88  PDM-VALID-AGENT-TYPE VALUE 'C' 'R'
00470                                                 'D' 'O' 'P'
00471                                                 'T' 'W'.
00472                  24  PDM-COMMISSION-BILLED-PAID
00473                                       PIC  X(01).
00474                      88  PDM-AGENT-BILLED   VALUE 'B'.
00475                      88  PDM-AGENT-PAID     VALUE 'P'.
00476                  24  PDM-COMP-RECALC-FLAG
00477                                       PIC  X(01).
00478                      88  PDM-BYPASS-RECALC  VALUE 'N'.
00479                      88  PDM-VALID-RECALC-FLAG VALUE ' ' 'N'.
00480      12  FILLER                       PIC  X(55).
00481
00482 ******************************************************************
00483 *                BANK DATA                                       *
00484 ******************************************************************
00485
00486      12  PDM-BANK-ACCOUNT-NUMBER    PIC  X(20).
00487      12  PDM-BANK-TRANSIT-NUMBER.
00488          16  PDM-FEDERAL-NUMBER     PIC  X(04).
00489          16  PDM-BANK-NUMBER        PIC  X(04).
00490      12  PDM-CHARGE-CARD-EXP-DT     PIC  X(02).
00491      12  PDM-CHARGE-CARD-TYPE       PIC  X(02).
00492          88  PDM-AMERICAN-EXPRESS              VALUE 'AE'.
00493          88  PDM-CARTE-BLANCHE                 VALUE 'CB'.
00494          88  PDM-DINERS-CLUB                   VALUE 'DN'.
00495          88  PDM-DISCOVER                      VALUE 'DS'.
00496          88  PDM-MASTER-CARD                   VALUE 'MC'.
00497          88  PDM-VISA                          VALUE 'VI'.
00498      12  PDM-SIGNATURE-NAME         PIC  X(25).
00499      12  PDM-AUTHORIZATION-SW       PIC  X(01).
00500 ******************************************************************
00501 *                GENERIC FILLER                                  *
00502 ******************************************************************
00503
00504      12  FILLER                       PIC  X(66).
00505
00506 ******************************************************************
00631      EJECT
00632 *                            COPY ELCRETR.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCRETR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER RETRIEVE FILE                *
00008 *                                                                *
00009 *   **** NOTE -- THIS FILE IS IDENTICAL TO CLAIM MASTER (ELMSTR) *
00010 *   ****      ANY CHANGES TO THIS COPYBOOK OR ELCMSTR MUST BE    *
00011 *   ****      DUPLICATED IN THE OTHER.                           *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00015 *                                                                *
00016 *   BASE CLUSTER = ELRETR                         RKP=2,LEN=20   *
00017 *       ALTERNATE PATH1 = ELRETR2 (BY NAME)       RKP=22,LEN=29  *
00018 *       ALTERNATE PATH2 = ELRETR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00019 *       ALTERNATE PATH3 = ELRETR5 (BY CERT NO)    RKP=63,LEN=12  *
00020 *       ALTERNATE PATH4 = ELRETR6 (BY CREDIT CARD NO)
00021 *                                                 RKP=75,LEN=21  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  RETRIEVE-MASTER.
00027      12  RL-RECORD-ID                PIC XX.
00028          88  VALID-RL-ID         VALUE 'RL'.
00029
00030      12  RL-CONTROL-PRIMARY.
00031          16  RL-COMPANY-CD           PIC X.
00032          16  RL-CARRIER              PIC X.
00033          16  RL-CLAIM-NO             PIC X(7).
00034          16  RL-CERT-NO.
00035              20  RL-CERT-PRIME       PIC X(10).
00036              20  RL-CERT-SFX         PIC X.
00037
00038      12  RL-CONTROL-BY-NAME.
00039          16  RL-COMPANY-CD-A1        PIC X.
00040          16  RL-INSURED-LAST-NAME    PIC X(15).
00041          16  RL-INSURED-NAME.
00042              20  RL-INSURED-1ST-NAME PIC X(12).
00043              20  RL-INSURED-MID-INIT PIC X.
00044
00045      12  RL-CONTROL-BY-SSN.
00046          16  RL-COMPANY-CD-A2        PIC X.
00047          16  RL-SOC-SEC-NO.
00048              20  RL-SSN-STATE        PIC XX.
00049              20  RL-SSN-ACCOUNT      PIC X(6).
00050              20  RL-SSN-LN3          PIC X(3).
00051
00052      12  RL-CONTROL-BY-CERT-NO.
00053          16  RL-COMPANY-CD-A4        PIC X.
00054          16  RL-CERT-NO-A4.
00055              20  RL-CERT-A4-PRIME    PIC X(10).
00056              20  RL-CERT-A4-SFX      PIC X.
00057
00058      12  RL-CONTROL-BY-CCN.
00059          16  RL-COMPANY-CD-A5        PIC X.
00060          16  RL-CCN-A5.
00061              20  RL-CCN-NO.
00062                  24  RL-CCN-PREFIX-A5 PIC X(4).
00063                  24  RL-CCN-PRIME-A5 PIC X(12).
00064              20  RL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  RL-INSURED-PROFILE-DATA.
00067          16  RL-INSURED-BIRTH-DT     PIC XX.
00068          16  RL-INSURED-SEX-CD       PIC X.
00069              88  RL-INSURED-IS-MALE     VALUE 'M'.
00070              88  RL-INSURED-IS-FEMALE   VALUE 'F'.
00071              88  RL-INSURED-SEX-UNKNOWN VALUE ' '.
00072          16  RL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  RL-PROCESSING-INFO.
00076          16  RL-PROCESSOR-ID         PIC X(4).
00077          16  RL-CLAIM-STATUS         PIC X.
00078              88  RL-CLAIM-IS-OPEN       VALUE 'O'.
00079              88  RL-CLAIM-IS-CLOSED     VALUE 'C'.
00080          16  RL-CLAIM-TYPE           PIC X.
00081 *            88  RL-AH-CLAIM            VALUE 'A'.
00082 *            88  RL-LIFE-CLAIM          VALUE 'L'.
00083 *            88  RL-PROPERTY-CLAIM      VALUE 'P'.
00084 *            88  RL-UNEMPLOYMENT-CLAIM  VALUE 'U'.
00085          16  RL-CLAIM-PREM-TYPE      PIC X.
00086              88  RL-SINGLE-PREMIUM         VALUE '1'.
00087              88  RL-O-B-COVERAGE           VALUE '2'.
00088              88  RL-OPEN-END-COVERAGE      VALUE '3'.
00089          16  RL-INCURRED-DT          PIC XX.
00090          16  RL-REPORTED-DT          PIC XX.
00091          16  RL-FILE-ESTABLISH-DT    PIC XX.
00092          16  RL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  RL-LAST-PMT-DT          PIC XX.
00094          16  RL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  RL-PAID-THRU-DT         PIC XX.
00096          16  RL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  RL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  RL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  RL-PMT-CALC-METHOD      PIC X.
00100              88  RL-360-DAY-YR          VALUE '1'.
00101              88  RL-365-DAY-YR          VALUE '2'.
00102              88  RL-FULL-MONTHS         VALUE '3'.
00103          16  RL-CAUSE-CD             PIC X(6).
00104
00105          16  RL-PRIME-CERT-NO.
00106              20  RL-PRIME-CERT-PRIME PIC X(10).
00107              20  RL-PRIME-CERT-SFX   PIC X.
00108
00109          16  RL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  RL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  RL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  RL-MICROFILM-NO         PIC X(10).
00114          16  RL-PROG-FORM-TYPE       PIC X.
00115          16  RL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  RL-LAST-REOPEN-DT       PIC XX.
00118          16  RL-LAST-CLOSE-DT        PIC XX.
00119          16  RL-LAST-CLOSE-REASON    PIC X.
00120              88  RL-FINAL-PAID          VALUE '1'.
00121              88  RL-CLAIM-DENIED        VALUE '2'.
00122              88  RL-AUTO-CLOSE          VALUE '3'.
00123              88  RL-MANUAL-CLOSE        VALUE '4'.
00124          16  RL-ASSOC-CERT-SEQU      PIC S99.
00125          16  RL-ASSOC-CERT-TOTAL     PIC S99.
00126          16  RL-CLAIM-PAYMENT-STATUS PIC 9.
00127              88  RL-PAYMENT-IN-PREP     VALUE 1 THRU 9.
00128          16  FILLER                  PIC X(5).
00129
00130      12  RL-CERTIFICATE-DATA.
00131          16  RL-CERT-ORIGIN          PIC X.
00132              88  RL-CERT-WAS-ONLINE     VALUE '1'.
00133              88  RL-CERT-WAS-CREATED    VALUE '2'.
00134              88  RL-COVERAGE-WAS-ADDED  VALUE '3'.
00135          16  RL-CERT-KEY-DATA.
00136              20  RL-CERT-CARRIER     PIC X.
00137              20  RL-CERT-GROUPING    PIC X(6).
00138              20  RL-CERT-STATE       PIC XX.
00139              20  RL-CERT-ACCOUNT.
00140                  24  RL-CERT-ACCOUNT-PREFIX PIC X(4).
00141                  24  RL-CERT-ACCOUNT-PRIME  PIC X(6).
00142              20  RL-CERT-EFF-DT      PIC XX.
00143
00144      12  RL-STATUS-CONTROLS.
00145          16  RL-PRIORITY-CD          PIC X.
00146              88  RL-HIGHEST-PRIORITY    VALUE '9'.
00147          16  RL-SUPV-ATTN-CD         PIC X.
00148              88  RL-SUPV-NOT-REQUIRED   VALUE ' ' 'N'.
00149              88  RL-SUPV-IS-REQUIRED    VALUE 'Y'.
00150          16  RL-PURGED-DT            PIC XX.
00151          16  RL-RESTORED-DT          PIC XX.
00152          16  RL-NEXT-AUTO-PAY-DT     PIC XX.
00153          16  RL-NEXT-RESEND-DT       PIC XX.
00154          16  RL-NEXT-FOLLOWUP-DT     PIC XX.
00155          16  FILLER                  PIC XX.
00156          16  RL-LAST-MAINT-DT        PIC XX.
00157          16  RL-LAST-MAINT-USER      PIC X(4).
00158          16  RL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00159          16  RL-LAST-MAINT-TYPE      PIC X.
00160              88  RL-CLAIM-SET-UP           VALUE ' '.
00161              88  RL-PAYMENT-MADE           VALUE '1'.
00162              88  RL-LETTER-SENT            VALUE '2'.
00163              88  RL-MASTER-WAS-ALTERED     VALUE '3'.
00164              88  RL-MASTER-WAS-RESTORED    VALUE '4'.
00165              88  RL-INCURRED-DATE-CHANGED  VALUE '5'.
00166              88  RL-FILE-CONVERTED         VALUE '6'.
00167          16  RL-RELATED-CLAIM-NO     PIC X(7).
00168          16  RL-HISTORY-ARCHIVE-DT   PIC XX.
00169          16  RL-BENEFICIARY          PIC X(10).
00170          16  RL-FILE-ESTABLISHED-BY  PIC X(4).
00171          16  FILLER                  PIC X(6).
00172
00173      12  RL-TRAILER-CONTROLS.
00174          16  RL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00175              88  RL-1ST-TRL-AVAIL       VALUE +4095.
00176              88  RL-LAST-TRL-AVAIL      VALUE +100.
00177              88  RL-RESV-EXP-HIST-TRLR  VALUE +0.
00178          16  RL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00179          16  FILLER                  PIC XX.
00180          16  RL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00181          16  RL-ADDRESS-TRAILER-CNT.
00182              20  RL-INSURED-ADDR-CNT  PIC S9.
00183                  88  RL-NO-INSURED-AVAILABLE VALUE ZERO.
00184              20  RL-ACCOUNT-ADDR-CNT  PIC S9.
00185                  88  RL-ACCOUNT-IS-ONLINE    VALUE ZERO.
00186              20  RL-BENIF-ADDR-CNT    PIC S9.
00187                  88  RL-BENEFICIARY-IS-ONLINE VALUE ZERO.
00188              20  RL-EMPLOYER-ADDR-CNT PIC S9.
00189                  88  RL-NO-EMPLOY-AVAILABLE   VALUE ZERO.
00190              20  RL-DOCTOR-ADDR-CNT   PIC S9.
00191                  88  RL-NO-DOCTOR-AVAILABLE   VALUE ZERO.
00192              20  RL-OTHER-1-ADDR-CNT  PIC S9.
00193                  88  RL-NO-OTHER-1-ADDRESSES  VALUE ZERO.
00194              20  RL-OTHER-2-ADDR-CNT  PIC S9.
00195                  88  RL-NO-OTHER-2-ADDRESSES  VALUE ZERO.
00196
00197      12  RL-CV-REFERENCE-NO.
00198          16  RL-CV-REFNO-PRIME       PIC X(18).
00199          16  RL-CV-REFNO-SFX         PIC XX.
00200
00201      12  RL-FILE-LOCATION            PIC X(4).
00202
00203      12  RL-PROCESS-ERRORS.
00204          16  RL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00205              88  RL-NO-FATAL-ERRORS     VALUE ZERO.
00206          16  RL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00207              88  RL-NO-FORCABLE-ERRORS  VALUE ZERO.
00208
00209      12  RL-PRODUCT-CD               PIC X.
00210
00211      12  RL-CURRENT-KEY-DATA.
00212          16  RL-CURRENT-CARRIER      PIC X.
00213          16  RL-CURRENT-GROUPING     PIC X(6).
00214          16  RL-CURRENT-STATE        PIC XX.
00215          16  RL-CURRENT-ACCOUNT      PIC X(10).
00216
00217      12  RL-ASSOCIATES               PIC X.
00218          88  RL-ASSOC-NO-INTERFACE      VALUE 'A'.
00219          88  RL-ASSOC-INTERFACE         VALUE 'I'.
00220          88  RL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00221          88  RL-NON-ASSOC-INTERFACE     VALUE 'M'.
00222
00223      12  RL-ACTIVITY-CODE            PIC 99.
00224      12  RL-ACTIVITY-MAINT-DT        PIC XX.
00225      12  RL-ACTIVITY-MAINT-TYPE      PIC X(4).
00226
00227      12  RL-LAPSE-REPORT-CODE        PIC 9.
00228      12  RL-LAG-REPORT-CODE          PIC 9.
00229      12  RL-LOAN-TYPE                PIC XX.
00230      12  RL-LEGAL-STATE              PIC XX.
00231
00232      12  FILLER                      PIC X(5).
00633      EJECT
00634 *                COPY MTCPHST REPLACING ==:TAG:== BY ==PH==.
00001 ******************************************************************
00002 *                                                                *
00003 *                           MTCPHST                              *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = POLICY HISTORY FILE                       *
00007 *                                                                *
00008 *   FILE TYPE = VSAM,KSDS                                        *
00009 *   RECORD SIZE = 132  RECFORM = FIXED                           *
00010 *                                                                *
00011 *   BASE CLUSTER = MPPHST                         RKP=2,LEN=46   *
00012 *       ALTERNATE (NONE)                                         *
00013 *                                                                *
00014 *   LOG = YES                                                    *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00017
00018  01  POLICY-HISTORY.
00019      12 PH-RECORD-ID                    PIC XX.
00020          88  VALID-PH-ID                      VALUE 'PH'.
00021
00022 ******************************************************************
00023 *   BASE CLUSTER = MPPHST         (BASE KEY)      RKP=2,LEN=46   *
00024 ******************************************************************
00025
00026      12 PH-CONTROL-PRIMARY.
00027          16 PH-COMPANY-CD               PIC X.
00028          16 PH-CARRIER                  PIC X.
00029          16 PH-GROUPING.
00030              20 PH-GROUPING-PREFIX      PIC X(3).
00031              20 PH-GROUPING-PRIME       PIC X(3).
00032          16 PH-STATE                    PIC XX.
00033          16 PH-PRODUCER.
00034              20 PH-PRODUCER-PREFIX      PIC X(4).
00035              20 PH-PRODUCER-PRIME       PIC X(6).
00036          16 PH-POLICY-EFF-DT            PIC XX.
00037          16 PH-POLICY-NO.
00038              20 PH-POLICY-PRIME         PIC X(18).
00039              20 PH-POLICY-SFX           PIC XX.
00040          16 PH-RECORD-TYPE              PIC X(2).
00041          16 PH-FIELD-TYPE               PIC X(2).
00042          16 PH-SEQUENCE-NO              PIC S9(4)    COMP.
00043              88 PH-1ST-HISTORY-REC         VALUE +4095.
00044      12  FILLER                            PIC X(16).
00045
00046 ******************************************************************
00047 *                 FILE SYNCHRONIZATION DATA                      *
00048 ******************************************************************
00049
00050      12 PH-FILE-SYNCH-DATA.
00051          16 PH-TRANSACTION-ID           PIC X(4).
00052          16 PH-CHANGE-DT                PIC XX.
00053          16 PH-CHANGE-TIME              PIC S9(7)  COMP-3.
00054          16 PH-CHANGE-PROCESSOR         PIC X(4).
00055          16 PH-MONTH-END-DT             PIC XX.
00056
00057 ******************************************************************
00058 *                 POLICY HISTORY RECORD BODY                     *
00059 ******************************************************************
00060
00061      12 PH-RECORD-BODY                  PIC X(50).
00062
00063 ******************************************************************
00064 *                       NUMERIC DATA                             *
00065 ******************************************************************
00066      12  FILLER           REDEFINES PH-RECORD-BODY.
00067          20 PH-NUMERIC-FLD1             PIC S9(10)V99.
00068          20  FILLER                        PIC X(38).
00069
00070      12  FILLER           REDEFINES PH-RECORD-BODY.
00071          20 PH-NUMERIC-FLD2             PIC S9(2)V999.
00072          20  FILLER                        PIC X(45).
00073
00074      12  FILLER           REDEFINES PH-RECORD-BODY.
00075          20 PH-NUMERIC-FLD3             PIC S9(15).
00076          20  FILLER                        PIC X(35).
00077
00078 ******************************************************************
00635      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER-L
                                CONTROL-FILE CERTIFICATE-MASTER
                                ACTIVITY-TRAILERS ACTIVITY-QUE
                                ACCOUNT-MASTER LETTER-ARCHIVE
                                CERTIFICATE-TRAILERS POLICY-MASTER
                                PRODUCER-PLANS PRODUCER-MASTER
                                RETRIEVE-MASTER POLICY-HISTORY.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL150' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00638      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00639      MOVE '5'                    TO DC-OPTION-CODE.
00640      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00641      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00642      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00643
00644      IF EIBCALEN = 0
00645          GO TO 8800-UNAUTHORIZED-ACCESS.
00646
00647      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00648
00649      IF PI-COMPANY-ID NOT = 'DMD'
00650          MOVE SPACES             TO WST-REM-DAYS-GRP
00651                                     WST-ORIG-DAYS-GRP.
00652
00653      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
00654      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
00655
00656      MOVE EIBTRMID               TO QID-TERM.
00657
00658      
      * EXEC CICS HANDLE CONDITION
00659 *        QIDERR   (1000-SHOW-CLAIM)
00660 *        MAPFAIL  (0100-FIRST-TIME-IN)
00661 *        NOTOPEN  (8500-FILE-NOTOPEN)
00662 *        PGMIDERR (9600-PGMID-ERROR)
00663 *        ERROR    (9990-ABEND)
00664 *    END-EXEC.
      *    MOVE '"$N?JL.               ! " #00008244' TO DFHEIV0
           MOVE X'22244E3F4A4C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303038323434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00665
00666      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00667          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.
00668
00669      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00670          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00671              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00672              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00673              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00674              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00675              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00676              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00677              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00678              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00679          ELSE
00680              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00681              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00682              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00683              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00684              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00685              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00686              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00687              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00688
00689      IF (RETURNED-FROM NOT = SPACES)
061013        and (pi-save-map not = crtt-map)
00690          GO TO 0600-RECOVER-TEMP-STORAGE.
00691
00692      IF EIBAID = DFHCLEAR
00693          GO TO 9400-CLEAR.
00694
00695      IF PI-PROCESSOR-ID = 'LGXX'
00696          NEXT SENTENCE
00697      ELSE
00698          
      * EXEC CICS READQ TS
00699 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00700 *            INTO    (SECURITY-CONTROL)
00701 *            LENGTH  (SC-COMM-LENGTH)
00702 *            ITEM    (SC-ITEM)
00703 *        END-EXEC
      *    MOVE '*$II   L              ''   #00008285' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00704          MOVE SC-CLAIMS-DISPLAY (16)  TO  PI-DISPLAY-CAP
00705          MOVE SC-CLAIMS-UPDATE  (16)  TO  PI-MODIFY-CAP
00706          IF NOT DISPLAY-CAP
00707              MOVE 'READ'              TO  SM-READ
00708              PERFORM 9995-SECURITY-VIOLATION
00709              MOVE ER-0070             TO  EMI-ERROR
00710              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00711              GO TO 8100-SEND-INITIAL-MAP.
00712
00713      IF EIBTRNID = TRANS-ID
00714          GO TO 0200-RECEIVE.
00715
00716  0100-FIRST-TIME-IN.
00717
00718      MOVE LOW-VALUES             TO EL150AO.
00719
00720      IF NOT PI-CONFIDENTIAL-UP
00721          MOVE LOW-VALUES         TO PI-REDEF.
00722
00723      MOVE 'ELMSTR'               TO PI-CURRENT-FILE.
00724      MOVE ZEROS                  TO PI-TOTAL-PAID
00725                                     PI-DAYS-PAID
00726                                     PI-LAST-SEQ-NO.
050107
050107     MOVE 0                      TO PI-FORCE-COUNT.
031714
031714     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
031714     MOVE '2'                    TO CNTL-REC-TYPE.
031714     MOVE +0                     TO CNTL-SEQ-NO.
031714     MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS.
031714     MOVE 'CNTL'                 TO FILE-SWITCH.
031714
031714     
      * EXEC CICS READ
031714*         DATASET  ('ELCNTL')
031714*         SET      (ADDRESS OF CONTROL-FILE)
031714*         RIDFLD   (ELCNTL-KEY)
031714*         RESP     (WS-RESPONSE)
031714*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00008323' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038333233' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
031714
031714     IF WS-RESP-NORMAL
031714        MOVE CF-APPROVAL-LEVEL TO PI-APPROVAL-LEVEL
031714     END-IF.
031714
00727
00728      
      * EXEC CICS DELETEQ TS
00729 *        QUEUE(QID)
00730 *    END-EXEC.
      *    MOVE '*&                    #   #00008335' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00731
00732      GO TO 1000-SHOW-CLAIM.
00733
00734      EJECT
00735  0200-RECEIVE.
00736      MOVE 'B'                    TO PASS-SWITCH.
00737
00738      MOVE LOW-VALUES             TO EL150AI.
00739
00740      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00741          MOVE ER-0008            TO EMI-ERROR
00742          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00743          MOVE -1                 TO FILETOL
00744          GO TO 8200-SEND-DATAONLY.
00745
061013     if pi-save-map = crtt-map
061013        move pi-save-map to map-name
061013     end-if
00746      
      * EXEC CICS RECEIVE
00747 *        MAP      (MAP-NAME)
00748 *        MAPSET   (MAPSET-NAME)
00749 *        INTO     (EL150AI)
00750 *    END-EXEC.
           MOVE LENGTH OF
            EL150AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00008356' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00751
061013     if pi-save-map = crtt-map
061013        move spaces              to pi-save-map
061013        move sad-map             to map-name
061013        go to 0100-first-time-in
061013     end-if
00752      IF ENTERPFL = 0
00753          GO TO 0300-CHECK-PFKEYS.
00754
00755      IF EIBAID NOT = DFHENTER
00756          MOVE ER-0004            TO EMI-ERROR
00757          GO TO 0320-INPUT-ERROR.
00758
00759      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
00760          MOVE PF-VALUES (ENTERPFI) TO EIBAID
00761      ELSE
00762          MOVE ER-0029            TO EMI-ERROR
00763          GO TO 0320-INPUT-ERROR.
00764
00765  0300-CHECK-PFKEYS.
00766      IF EIBAID = DFHPF23
00767          GO TO 8810-PF23.
00768
00769      IF EIBAID = DFHPF24
00770          GO TO 9200-RETURN-MAIN-MENU.
00771
00772      IF EIBAID = DFHPF12
00773          GO TO 9500-PF12.
00774
050107     IF (EIBAID = DFHPF22) AND (NOT FORCE-CAP)
050107         MOVE ER-0433            TO EMI-ERROR
050107         GO TO 0320-INPUT-ERROR.
050107
050107     IF (PI-FORCE-COUNT NOT = 0) AND (EIBAID NOT = DFHPF22)
050107         MOVE ER-0500            TO EMI-ERROR
050107         GO TO 0320-INPUT-ERROR.
050107
00775      IF PI-RETRIEVED-DATA
00776          IF EIBAID = DFHPF1  OR DFHPF2 OR DFHPF15 OR
00777                      DFHPF16 OR DFHPF21
00778              NEXT SENTENCE
00779          ELSE
00780              MOVE ER-0926        TO EMI-ERROR
00781              GO TO 0320-INPUT-ERROR.
00782
040416*    IF EIBAID = DFHPF21
040416*        IF NOT PI-RETRIEVED-DATA
040416*            PERFORM 7650-CREATE-ACTQ THRU 7650-EXIT
040416*            MOVE ER-0927        TO EMI-ERROR
040416*            GO TO 0320-INPUT-ERROR
040416*        ELSE
040416*            GO TO 7700-CREATE-CLAIM.
00790
00791      IF CLAIM-IS-PURGED
00792          IF EIBAID = DFHPF1  OR DFHPF2  OR DFHPF3  OR DFHPF4  OR
00793                      DFHPF6  OR DFHPF7  OR DFHPF8  OR DFHPF9  OR
00794                      DFHPF10 OR DFHPF11 OR DFHPF13 OR DFHPF14 OR
00795                      DFHPF17 OR DFHPF18 OR DFHPF20
00796                        MOVE -1            TO  ENTERPFL
00797                        MOVE 'CLAIM IS PURGED, PFKEYS ARE INVALID'
00798                                           TO  MAP-DISPLAY (1)
00799                          GO TO 8200-SEND-DATAONLY.
00800
00801      IF (PI-SUPERVISOR-ONLY)
062602        AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
062602             AND 'AMWA')
062602*       NOT SYSTEM-MODIFY-CAP
062602*        IF EIBAID = DFHPF3  OR DFHPF6  OR DFHPF7  OR DFHPF8  OR
062602         IF EIBAID = DFHPF4  OR DFHPF6  OR DFHPF7  OR DFHPF8  OR
062602                     DFHPF9  or
00805                      DFHPF11 OR DFHPF13 OR DFHPF14 OR DFHPF17 OR
00806                      DFHPF21 OR DFHENTER
00807              MOVE -1             TO ENTERPFL
00808              MOVE ER-0931        TO EMI-ERROR
00809              GO TO 0320-INPUT-ERROR.
00810
00811      IF EIBAID = DFHPF13 OR DFHPF19 OR DFHPF20
00812          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00813              MOVE ER-0029            TO  EMI-ERROR
00814              GO TO 0320-INPUT-ERROR.
00815
00816      IF EIBAID = DFHPF5 OR DFHPF18 OR DFHPF19 OR DFHPF20
00817          IF PI-COMPANY-ID = 'DMD'
00818              AND
00819             NOT SYSTEM-MODIFY-CAP
00820             IF PI-PRIORITY-9
00821                MOVE ER-0931            TO  EMI-ERROR
00822                GO TO 0320-INPUT-ERROR.
00823
050107     IF EIBAID = DFHPF7 OR DFHPF6
050107         IF PI-JOINT-INSURED AND NOT PI-JOINT-COVERAGE
050107            AND PI-TOTAL-PAID NOT GREATER THAN +0
050107             MOVE EIBAID             TO PI-PFKEY-USED
050107             MOVE ER-3550            TO  EMI-ERROR
050107             GO TO 0320-INPUT-ERROR.
050107
050107     IF EIBAID = DFHPF22 AND PI-PFKEY-USED = DFHPF7
050107                 GO TO 0500-CREATE-TEMP-STORAGE.
050107     IF EIBAID = DFHPF22 AND PI-PFKEY-USED = DFHPF6
050107         IF PI-SAVE-TYPE = 'L'
050107             MOVE ER-0376        TO EMI-ERROR
050107             GO TO 0320-INPUT-ERROR
050107         ELSE
050107             GO TO 0500-CREATE-TEMP-STORAGE.
050107
00824      IF EIBAID = DFHPF3  OR DFHPF4  OR DFHPF5  OR DFHPF7  OR
00825                  DFHPF8  OR DFHPF9  OR DFHPF10 OR DFHPF13 OR
00826                  DFHPF14 OR DFHPF17 OR DFHPF18 OR DFHPF19 OR
040416                 DFHPF20 or dfhpf11 OR DFHPF21
00828                  GO TO 0500-CREATE-TEMP-STORAGE.
00829
00830      IF EIBAID = DFHPF6
00831          IF PI-SAVE-TYPE = 'L'
00832              MOVE ER-0376        TO EMI-ERROR
00833              GO TO 0320-INPUT-ERROR
00834          ELSE
00835              GO TO 0500-CREATE-TEMP-STORAGE.
00836
00837      MOVE SPACES                 TO ERRMSG1O.
00838
00839      IF EIBAID = DFHPF15
00840          GO TO 7000-START-BROWSE.
00841
00842      IF EIBAID = DFHPF16
00843          GO TO 7500-START-BROWSE.
00844
00845      IF EIBAID = DFHPF1
00846          MOVE 'F'                TO DIRECTION-SWITCH
00847          GO TO 1600-ROLL-TRAILERS.
00848
00849      IF EIBAID = DFHPF2
00850          MOVE 'B'                TO DIRECTION-SWITCH
00851          GO TO 1600-ROLL-TRAILERS.
00852
00853      IF EIBAID = DFHENTER
00854          GO TO 0330-EDIT-DATA.
00855
      *      string "EIBAID = " eibaid into map-display (1)
      *      GO TO 8200-SEND-DATAONLY.
00856      MOVE ER-0029                TO EMI-ERROR.
00857
00858  0320-INPUT-ERROR.
00859      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
050107
050107     MOVE EMI-FORCABLE-CTR       TO  PI-FORCE-COUNT.
00860
00861      IF ENTERPFL = 0
00862          MOVE -1                 TO FILETOL
00863      ELSE
00864          MOVE AL-UNBON           TO ENTERPFA
00865          MOVE -1                 TO ENTERPFL.
00866
00867      GO TO 8200-SEND-DATAONLY.
00868
00869  0330-EDIT-DATA.
050107
050107     MOVE 0                      TO PI-FORCE-COUNT.
050107
00870      IF NOT MODIFY-CAP
00871          MOVE 'UPDATE'           TO  SM-READ
00872          PERFORM 9995-SECURITY-VIOLATION
00873          MOVE ER-0070            TO  EMI-ERROR
00874          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00875          GO TO 8100-SEND-INITIAL-MAP.
00876
00877      IF CLMNOL  > 0 OR
00878         CARRL   > 0 OR
00879         CERTNOL > 0 OR
00880         SUFXL   > 0
00881             GO TO 1000-SHOW-CLAIM.
00882
CIDMOD     IF YESNOSWL > 0
CIDMOD         MOVE AL-UANON           TO YESNOSWA.
CIDMOD
082707*    IF MCRFLML > 0
082707*        MOVE AL-UANON           TO MCRFLMA.
00885
00886      IF FILETOL > 0
00887          MOVE AL-UANON           TO FILETOA.
00888
CIDMOD     IF CLOANL > 0
CIDMOD        MOVE AL-UANON            TO CLOANA
CIDMOD     END-IF
CIDMOD
031714     IF TOTINTL > 0
031714         MOVE AL-UNNON           TO TOTINTA
031714     END-IF
031714
00889      IF PRTOPTL > 0
00890         IF PRTOPTI NOT = 'N' AND 'L'
00891            MOVE AL-UABON         TO PRTOPTA
00892            MOVE -1               TO PRTOPTL
00893            MOVE ER-0334 TO EMI-ERROR
00894            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00895         ELSE
00896            MOVE AL-UANON         TO PRTOPTA.
00897
00898      IF PRTOPTL > 0
00899          IF (NOT PI-NO-CARRIER-SECURITY OR
00900              NOT PI-NO-ACCOUNT-SECURITY)
00901              IF PRTOPTI NOT = 'N'
00902                  MOVE AL-UABON   TO PRTOPTA
00903                  MOVE -1         TO PRTOPTL
00904                  MOVE ER-2378    TO EMI-ERROR
00905                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00906
00907      IF FORMATL > 0
00908          IF FORMATI NOT = 'F' AND 'P'
00909              MOVE AL-UABON       TO FORMATA
00910              MOVE -1             TO FORMATL
00911              MOVE ER-0335        TO EMI-ERROR
00912              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00913          ELSE
00914              MOVE AL-UANON       TO FORMATA.
00915
00916      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00917          IF ASSOCL > +0
00918              IF ASSOCI = 'A' OR 'I' OR 'N' OR 'M'
00919                  MOVE AL-UANON   TO  ASSOCA
00920              ELSE
00921                  MOVE AL-UABON   TO  ASSOCA
00922                  MOVE -1         TO  ASSOCL
00923                  MOVE ER-0804    TO  EMI-ERROR
00924                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00925
00926  0335-EDIT-ACTIVITY-CODE.
00927 ******************************************************************
00928 *    THIS PARAGRAPH DETERMINES IF THE COMPANY IS CURRENTLY USING *
00929 *    THE AUTOMATIC ACTIVITY FUNCTIONS OF THE SYSTEM AND, IF SO,  *
00930 *    WHAT FUNCTIONS ARE TO BE PERFORMED BASED ON THE CODES SET   *
00931 *    IN THE AUTOMATIC ACTIVITY RECORD.                           *
00932 ******************************************************************
00933
00934      IF ACTCDL = +0
00935          GO TO 0340-EDIT-DATA.
00936
00937      IF ACTCDI IS NOT NUMERIC
00938          MOVE ZEROS              TO  ACTCDI
00939          MOVE AL-UABON           TO  ACTCDA
00940          MOVE -1                 TO  ACTCDL
00941          MOVE ER-3526            TO  EMI-ERROR
00942          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00943          GO TO 0340-EDIT-DATA.
00944
00945      MOVE ACTCDI                 TO  WS-ACTIVITY-CODE.
00946      IF NOT VALID-ACTIVITY-CODE
00947          MOVE AL-UABON           TO  ACTCDA
00948          MOVE -1                 TO  ACTCDL
00949          MOVE ER-3526            TO  EMI-ERROR
00950          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00951          GO TO 0340-EDIT-DATA.
00952
00953      MOVE 'CNTL'                 TO  FILE-SWITCH.
00954      PERFORM 6000-CHECK-AUTO-ACTIVITY THRU 6000-EXIT.
00955
00956      IF WS-ACT-REC-FOUND-SW = 'N'
00957           MOVE +0                TO  ACTCDL
00958           GO TO 0340-EDIT-DATA.
00959
00960      IF ACTCDI = ZEROS
00961          NEXT SENTENCE
00962      ELSE
00963          MOVE ACTCDI             TO  SUB
00964          SUBTRACT +9 FROM SUB
00965          IF CF-USER-ACTIVE-SW (SUB) = ' ' OR 'N'
00966              MOVE AL-UABON       TO  ACTCDA
00967              MOVE -1             TO  ACTCDL
00968              MOVE ER-3545        TO  EMI-ERROR
00969              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00970              GO TO 0340-EDIT-DATA.
00971
00972      IF ACTCDI = ZEROS
00973          IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK'
00974              PERFORM 6100-RESET-AUTO-ACTIVITY THRU 6100-EXIT
00975              MOVE AL-UANOF       TO  ACTCDA
00976              GO TO 0340-EDIT-DATA.
00977
00978      MOVE 'Y'                    TO  WS-LETTER-SW.
00979      MOVE AL-UANOF               TO  ACTCDA.
00980
00981  0340-EDIT-DATA.
00982
00983      IF EMI-NO-ERRORS
00984          IF PRTOPTL NOT = FORMATL
00985              MOVE AL-UABON       TO PRTOPTA  FORMATA
00986              MOVE -1             TO PRTOPTL
00987              MOVE ER-0336 TO EMI-ERROR
00988              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00989
00990      IF PRTOPTL > 0
00991         IF (NOT PI-NO-CARRIER-SECURITY OR
00992             NOT PI-NO-ACCOUNT-SECURITY)
00993            IF ALTPRTL NOT > 0
00994               MOVE AL-UABON      TO ALTPRTA
00995               MOVE -1            TO ALTPRTL
00996               MOVE ER-2379       TO EMI-ERROR
00997               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00998
00999      IF NOT EMI-NO-ERRORS
01000          IF EMI-FORCABLE-CTR = ZEROS AND
01001             EMI-FATAL-CTR = ZEROS
01002              NEXT SENTENCE
01003          ELSE
01004              GO TO 8200-SEND-DATAONLY.
01005
01006      IF FILETOL NOT > 0        AND
082707*       MCRFLML NOT > 0        AND
01008         ACTCDL  NOT > 0        AND
CIDMOD        ASSOCL  NOT > 0        AND
CIDMOD        CLOANL  NOT > 0        AND
031714        TOTINTL NOT > 0        AND
CIDMOD        YESNOSWL NOT > 0
01010          GO TO 0350-PROCESS-OPTIONS.
01011
01012      
      * EXEC CICS HANDLE CONDITION
01013 *        NOTFND   (0450-NOT-FOUND)
01014 *    END-EXEC.
      *    MOVE '"$I                   ! # #00008675' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303038363735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01015
01016      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
01017      MOVE PI-CARRIER             TO MSTR-CARRIER.
01018      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
01019      MOVE PI-CERT-NO             TO MSTR-CERT-NO.
01020      MOVE 'MSTR'                 TO FILE-SWITCH.
01021
01022      
      * EXEC CICS READ
01023 *        UPDATE
01024 *        DATASET   ('ELMSTR')
01025 *        RIDFLD    (ELMSTR-KEY)
01026 *        INTO      (CLAIM-MASTER)
01027 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00008685' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01028
01029      IF ACTCDL > +0
01030          IF CL-LAST-MAINT-USER NOT = PI-UPDATE-BY
01031              MOVE ER-0068        TO  EMI-ERROR
01032              MOVE -1             TO  ACTCDL
01033              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01034              GO TO 8200-SEND-DATAONLY
01035          ELSE
01036              NEXT SENTENCE
01037      ELSE
01038          IF (CL-LAST-MAINT-USER NOT = PI-UPDATE-BY) OR
01039             (CL-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS)
01040                MOVE ER-0068      TO EMI-ERROR
01041                MOVE -1           TO FILETOL
01042                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01043                GO TO 8200-SEND-DATAONLY.
01044
01045      MOVE 'ELMSTR  '             TO FILE-ID.
01046
082707*    IF MCRFLML > 0
082707*        MOVE MCRFLMI            TO CL-MICROFILM-NO.
01049
01050      IF FILETOL > 0
01051          IF FILETOI  = CL-FILE-LOCATION
01052              MOVE ZEROS          TO FILETOL
01053          ELSE
01054              MOVE FILETOI        TO CL-FILE-LOCATION.
01055
031714     IF TOTINTL > 0
031714         
      * EXEC CICS BIF DEEDIT
031714*            FIELD(TOTINTI)
031714*            LENGTH(9)
031714*        END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008720' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TOTINTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
031714         IF TOTINTI  = CL-TOTAL-INT-PAID
031714             MOVE ZEROS          TO TOTINTL
031714         ELSE
031714             MOVE TOTINTI        TO CL-TOTAL-INT-PAID
031714     END-IF.
031714
CIDMOD     IF CLOANL > 0
CIDMOD        IF CLOANI NOT = SPACES
CIDMOD           MOVE CL-CONTROL-PRIMARY
CIDMOD                                 TO ELTRLR-KEY
CIDMOD           MOVE +91              TO TRLR-SEQ-NO
CIDMOD           MOVE 'TRLR'           TO FILE-SWITCH
CIDMOD           
      * EXEC CICS READ
CIDMOD*             UPDATE
CIDMOD*             DATASET   ('ELTRLR')
CIDMOD*             RIDFLD    (ELTRLR-KEY)
CIDMOD*             SET       (ADDRESS OF ACTIVITY-TRAILERS)
CIDMOD*             RESP      (WS-RESPONSE)
CIDMOD*          END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00008736' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038373336' TO DFHEIV0(25:11)
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
CIDMOD           IF WS-RESP-NORMAL
CIDMOD              MOVE CLOANI        TO AT-INFO-LINE-1
CIDMOD              MOVE PI-PROCESSOR-ID
CIDMOD                                 TO AT-GEN-INFO-LAST-UPDATED-BY
CIDMOD              MOVE SAVE-BIN-DATE TO AT-GEN-INFO-LAST-MAINT-DT
CIDMOD              MOVE EIBTIME       TO AT-LAST-MAINT-HHMMSS
CIDMOD              
      * EXEC CICS REWRITE
CIDMOD*                DATASET   ('ELTRLR')
CIDMOD*                FROM      (ACTIVITY-TRAILERS)
CIDMOD*             END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00008749' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD           ELSE
CIDMOD              IF WS-RESP-NOTFND
01141                  
      * EXEC CICS GETMAIN
01142 *                   SET       (ADDRESS OF ACTIVITY-TRAILERS)
01143 *                   LENGTH    (200)
01144 *                   INITIMG   (GETMAIN-SPACE)
01145 *                END-EXEC
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008755' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD                 MOVE 'AT'       TO AT-RECORD-ID
CIDMOD                 MOVE CL-CONTROL-PRIMARY
CIDMOD                                 TO AT-CONTROL-PRIMARY
CIDMOD                 MOVE +91        TO AT-SEQUENCE-NO
CIDMOD                 MOVE CLOANI     TO AT-INFO-LINE-1
CIDMOD                 MOVE '6'        TO AT-TRAILER-TYPE
CIDMOD                 MOVE PI-PROCESSOR-ID
CIDMOD                                 TO AT-RECORDED-BY
CIDMOD                                    AT-GEN-INFO-LAST-UPDATED-BY
CIDMOD                 MOVE SAVE-BIN-DATE
CIDMOD                                 TO AT-RECORDED-DT
CIDMOD                                    AT-GEN-INFO-LAST-MAINT-DT
CIDMOD                 MOVE EIBTIME    TO AT-LAST-MAINT-HHMMSS
CIDMOD                 
      * EXEC CICS WRITE
CIDMOD*                   DATASET   ('ELTRLR')
CIDMOD*                   FROM      (ACTIVITY-TRAILERS)
CIDMOD*                   RIDFLD    (AT-CONTROL-PRIMARY)
CIDMOD*                END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00008773' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD              END-IF
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
CIDMOD     IF YESNOSWI EQUAL TO CL-YESNOSW
CIDMOD         MOVE ZEROS    TO YESNOSWL
CIDMOD     ELSE
CIDMOD         MOVE YESNOSWI TO CL-YESNOSW
CIDMOD     END-IF.
00800
01056      IF WS-ACT-REC-FOUND-SW = 'Y'
01057          IF ACTCDL > +0
01058              MOVE ACTCDI             TO  CL-ACTIVITY-CODE
01059              MOVE SAVE-BIN-DATE      TO  CL-ACTIVITY-MAINT-DT
01060                                          DC-BIN-DATE-1
01061              MOVE ' '                TO  DC-OPTION-CODE
01062              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01063              MOVE DC-GREG-DATE-1-EDIT    TO  ACTDTO.
01064
01065      IF WS-ACT-REC-FOUND-SW = 'Y'
01066          IF ACTCDL > +0
01067              IF ACTCDI = ZEROS
01068                  MOVE SPACES         TO  CL-ACTIVITY-MAINT-TYPE
01069                                          ACTTYPO
01070              ELSE
01071                  MOVE WS-ACTIVITY-CODE   TO  SUB
01072                  SUBTRACT 9 FROM SUB
01073                  MOVE CF-USER-ACTIVITY-DESC (SUB)
01074                                      TO  WS-ACT-USER-DESC
01075                  MOVE WS-DESC-1-3    TO  CL-ACTIVITY-MAINT-TYPE
01076                                          ACTTYPO.
01077
01078      IF WS-ACT-REC-FOUND-SW = 'Y'
01079          IF ACTCDL > +0
01080              IF WS-UPDATE-SW = 'Y'
01081                  MOVE LOW-VALUES     TO  CL-NEXT-RESEND-DT
01082                                          CL-NEXT-FOLLOWUP-DT.
01083
01084      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01085          IF ASSOCL > +0
01086              MOVE ASSOCI         TO  CL-ASSOCIATES.
01087
01088      IF PI-COMPANY-ID = 'FLA'
01089          NEXT SENTENCE
01090      ELSE
01091          MOVE '3'                TO  CL-LAST-MAINT-TYPE
01092          MOVE PI-PROCESSOR-ID    TO  CL-LAST-MAINT-USER
01093                                      SV-LAST-BY
01094          MOVE EIBTIME            TO  CL-LAST-MAINT-HHMMSS
01095                                      SV-LAST-HHMMSS
01096          MOVE SAVE-BIN-DATE      TO  CL-LAST-MAINT-DT.
01097
01098  0350-PROCESS-OPTIONS.
01099      IF PRTOPTL NOT > 0
01100          GO TO 0400-WRITE-CLAIM.
01101
01102      IF PRTOPTI = 'N'
01103          GO TO 0380-START-PRINTER.
01104
01105      
      * EXEC CICS HANDLE CONDITION
01106 *        NOTFND(0360-CREATE-NEW-ACTQ)
01107 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00008838' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303038383338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01108
01109      MOVE PI-COMPANY-CD          TO ACTQ-COMP-CD.
01110      MOVE PI-CARRIER             TO ACTQ-CARRIER.
01111      MOVE PI-CLAIM-NO            TO ACTQ-CLAIM-NO.
01112      MOVE PI-CERT-NO             TO ACTQ-CERT-NO.
01113
01114      MOVE 'ACTQ'                 TO FILE-SWITCH.
01115      MOVE 'ELACTQ  '             TO FILE-ID.
01116
01117      
      * EXEC CICS READ
01118 *        UPDATE
01119 *        DATASET   ('ELACTQ')
01120 *        SET       (ADDRESS OF ACTIVITY-QUE)
01121 *        RIDFLD    (ELACTQ-KEY)
01122 *    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00008850' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01123
01124      IF FORMATI = 'F'
01125          MOVE '1'                TO AQ-PENDING-STATUS-FLAG
01126      ELSE
01127          MOVE '2'                TO AQ-PENDING-STATUS-FLAG.
01128
01129      MOVE +150                   TO AQ-LAST-UPDATED-BY.
01130
01131      MOVE 'ELACTQ  '             TO FILE-ID.
01132
01133      
      * EXEC CICS REWRITE
01134 *        DATASET   ('ELACTQ')
01135 *        FROM      (ACTIVITY-QUE)
01136 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00008866' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01137
01138      GO TO 0400-WRITE-CLAIM.
01139
01140  0360-CREATE-NEW-ACTQ.
01141      
      * EXEC CICS GETMAIN
01142 *        SET       (ADDRESS OF ACTIVITY-QUE)
01143 *        LENGTH    (60)
01144 *        INITIMG   (GETMAIN-SPACE)
01145 *    END-EXEC.
           MOVE 60
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008874' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01146
01147      MOVE 'AQ'                   TO AQ-RECORD-ID.
01148      MOVE PI-COMPANY-CD          TO AQ-COMPANY-CD.
01149      MOVE PI-CARRIER             TO AQ-CARRIER.
01150      MOVE PI-CLAIM-NO            TO AQ-CLAIM-NO.
01151      MOVE PI-CERT-NO             TO AQ-CERT-NO.
01152
01153      IF FORMATI = 'F'
01154          MOVE '1'                TO AQ-PENDING-STATUS-FLAG
01155      ELSE
01156          MOVE '2'                TO AQ-PENDING-STATUS-FLAG.
01157
01158      MOVE +0                     TO AQ-PAYMENT-COUNTER
01159                                     AQ-PMT-UNAPPROVED-COUNT.
01160
01161      MOVE LOW-VALUES             TO AQ-RESEND-DATE
01162                                     AQ-FOLLOWUP-DATE.
01163
01164      MOVE +150                   TO AQ-LAST-UPDATED-BY.
01165
01166      MOVE 'ACTQ'                 TO FILE-SWITCH.
01167
01168      MOVE 'ELACTQ  '             TO FILE-ID.
01169
01170      
      * EXEC CICS WRITE
01171 *        DATASET   ('ELACTQ')
01172 *        FROM      (ACTIVITY-QUE)
01173 *        RIDFLD    (AQ-CONTROL-PRIMARY)
01174 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00008903' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01175
01176      GO TO 0400-WRITE-CLAIM.
01177
01178  0380-START-PRINTER.
01179      
      * EXEC CICS HANDLE CONDITION
01180 *         TERMIDERR    (0385-TERMID-ERROR)
01181 *         TRANSIDERR   (0390-TRANS-ERROR)
01182 *    END-EXEC.
      *    MOVE '"$[\                  ! % #00008912' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303038393132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01183
01184      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
01185      MOVE '1'                    TO CNTL-REC-TYPE.
01186      MOVE SPACES                 TO CNTL-ACCESS.
01187      MOVE +0                     TO CNTL-SEQ-NO.
01188      MOVE 'CNTL'                 TO FILE-SWITCH.
01189
01190      
      * EXEC CICS READ
01191 *        DATASET   ('ELCNTL')
01192 *        SET       (ADDRESS OF CONTROL-FILE)
01193 *        RIDFLD    (ELCNTL-KEY)
01194 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008923' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393233' TO DFHEIV0(25:11)
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
           
01195
01196      IF CF-FORMS-PRINTER-ID = SPACES
01197          MOVE ER-0337            TO EMI-ERROR
01198          MOVE -1                 TO FILETOL
01199          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01200          GO TO 8200-SEND-DATAONLY.
01201
01202      IF FORMATI = 'F'
01203          MOVE '2'                TO PI-ENTRY-CD-1
01204      ELSE
01205          MOVE '1'                TO PI-ENTRY-CD-1.
01206
01207      IF PI-PROCESSOR-PRINTER NOT = SPACES
01208          MOVE PI-PROCESSOR-PRINTER  TO  CF-FORMS-PRINTER-ID.
01209
01210      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.
01211
01212      IF ALTPRTL > 0
01213         MOVE ALTPRTI             TO CF-FORMS-PRINTER-ID
01214                                     PI-ALT-DMD-PRT-ID.
01215
01216      IF PI-COMPANY-ID = 'DMD' OR 'XXX'
01217 *       MOVE EIBTRMID            TO CF-FORMS-PRINTER-ID
01218          
      * EXEC CICS START
01219 *            TRANSID   (START-TRANS-ID)
01220 *            FROM      (PROGRAM-INTERFACE-BLOCK)
01221 *            LENGTH    (PI-COMM-LENGTH)
PEMMOD*            TERMID    ('A155')
PEMMOD*            TERMID    (CF-FORMS-PRINTER-ID)
01223 *        END-EXEC
      *    MOVE '0( LF                 1   #00008951' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01224      ELSE
01225          
      * EXEC CICS START
01226 *            TRANSID   (START-TRANS-ID)
pemuni*            TERMID    (CF-FORMS-PRINTER-ID)
pemuni*            TERMID    ('PRN1')
01228 *            FROM      (PROGRAM-INTERFACE-BLOCK)
01229 *            LENGTH    (PI-COMM-LENGTH)
01230 *        END-EXEC.
      *    MOVE '0( LFT                1   #00008959' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393539' TO DFHEIV0(25:11)
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
           
01231
01232       GO TO 0400-WRITE-CLAIM.
01233
01234  0385-TERMID-ERROR.
01235      MOVE ER-0412                TO EMI-ERROR.
01236      MOVE -1                     TO ALTPRTL.
01237      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01238      GO TO 8200-SEND-DATAONLY.
01239
01240  0390-TRANS-ERROR.
01241      MOVE ER-0413                TO EMI-ERROR.
01242      MOVE -1                     TO FILETOL.
01243      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01244      GO TO 8200-SEND-DATAONLY.
01245
01246  0400-WRITE-CLAIM.
01247      IF FILETOL = 0  AND
082707*       MCRFLML = 0  AND
01249         ACTCDL  = 0  AND
CIDMOD        ASSOCL  = 0  AND
CIDMOD        CLOANL  = 0  AND
031714        TOTINTL = 0  AND
CIDMOD        YESNOSWL = 0
01251          GO TO 0440-UPDATE-DONE.
01252
01253      
      * EXEC CICS HANDLE CONDITION
01254 *        NOTFND (0450-NOT-FOUND)
01255 *        DUPKEY (0440-UPDATE-DONE)
01256 *    END-EXEC.
      *    MOVE '"$I$                  ! & #00008991' TO DFHEIV0
           MOVE X'222449242020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303038393931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01257
01258      MOVE 'ELMSTR  '             TO FILE-ID.
01259
01260      
      * EXEC CICS REWRITE
01261 *        DATASET   ('ELMSTR')
01262 *        FROM      (CLAIM-MASTER)
01263 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&& L                  %   #00008998' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01264
01265  0440-UPDATE-DONE.
01266      IF FILETOL  > 0 OR
082707*       MCRFLML  > 0 OR
01268         PRTOPTL  > 0 OR
01269         FORMATL  > 0 OR
CIDMOD        YESNOSWL > 0 OR
CIDMOD        CLOANL   > 0 OR
031714        TOTINTL  > 0 OR
01270         ACTCDL   > 0
01271             MOVE SV-LAST-BY             TO PI-UPDATE-BY
01272             MOVE SV-LAST-HHMMSS         TO PI-UPDATE-HHMMSS.
01273
01274      PERFORM 6050-AUTO-LETTER-WRITER THRU 6050-EXIT.
01275
01276      MOVE ER-0000                TO EMI-ERROR.
01277      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01278
01279      IF CLAIM-IS-PURGED
082707        CONTINUE
082707*        MOVE -1                 TO  MCRFLML
01281      ELSE
01282          MOVE -1                 TO  FILETOL
082707     END-IF
01283
01284      GO TO 8200-SEND-DATAONLY.
01285
01286  0450-NOT-FOUND.
01287      IF FILE-SWITCH = 'MSTR'
01288          MOVE ER-0204            TO EMI-ERROR
01289      ELSE
01290          MOVE ER-0190            TO EMI-ERROR.
01291
01292      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01293
01294      MOVE -1                     TO FILETOL.
01295      GO TO 8200-SEND-DATAONLY.
01296
01297      EJECT
01298  0500-CREATE-TEMP-STORAGE.
01299
01300      IF PI-SAVE-SYS-ID = 'CR'
01301         IF PI-CLOSED
01302            IF PI-BENEFIT-CHANGE OR PI-ERROR-CORRECTION
01303               IF EIBAID = DFHPF7
01304                  MOVE ER-0934            TO EMI-ERROR
01305                  MOVE -1                 TO FILETOL
01306                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01307                  GO TO 8200-SEND-DATAONLY
01308               ELSE
01309               IF EIBAID = DFHPF8
01310                  MOVE ER-0935            TO EMI-ERROR
01311                  MOVE -1                 TO FILETOL
01312                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01313                  GO TO 8200-SEND-DATAONLY.
01314
01315      MOVE EIBCPOSN               TO PI-SAVE-CURSOR.
01316
01317      MOVE LOW-VALUES TO PI-SAVE-FILETO  PI-SAVE-PRTOPT
01318                         PI-SAVE-FORMAT  PI-SAVE-MCRFLM
01319                         PI-SAVE-ACT-CD  PI-SAVE-ASSOC
CIDMOD                        TS-PI-SAVE-CLOAN.
01320
01321      MOVE FILETOI                TO PI-SAVE-FILETO.
082707*    MOVE MCRFLMI                TO PI-SAVE-MCRFLM.
01323      MOVE PRTOPTI                TO PI-SAVE-PRTOPT.
01324      MOVE FORMATI                TO PI-SAVE-FORMAT.
01325      MOVE ACTCDI                 TO PI-SAVE-ACT-CD.
CIDMOD     MOVE CLOANI                 TO TS-PI-SAVE-CLOAN
01326
01327      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01328          MOVE ASSOCI             TO PI-SAVE-ASSOC.
01329
CIDMOD     MOVE PROGRAM-INTERFACE-BLOCK TO TS-PI-AREA
CIDMOD
01330      
      * EXEC CICS WRITEQ TS
01331 *        QUEUE    (QID)
CIDMOD*        FROM     (PROGRAM-INTERFACE-BLOCK)
01332 *        FROM     (TS-AREA)
CIDMOD*        LENGTH   (PI-COMM-LENGTH)
01333 *        LENGTH   (TS-LENGTH)
01334 *    END-EXEC.
      *    MOVE '*"     L              ''   #00009077' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-AREA, 
                 TS-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01335
01336  0500-PF-CHECKS.
01337
01338      IF EIBAID = DFHPF3
01339          IF PI-SAVE-SYS-ID = 'CV'
01340              MOVE XCTL-EM131     TO PGM-NAME
01341              GO TO 9300-XCTL
01342          ELSE
01343              MOVE XCTL-131       TO PGM-NAME
01344              GO TO 9300-XCTL.
01345
01346      IF EIBAID = DFHPF4
01347          MOVE XCTL-153           TO PGM-NAME
01348          GO TO 9300-XCTL.
01349
01350      IF EIBAID = DFHPF5
01351          IF PI-SAVE-SYS-ID = 'CV'
01352              MOVE THIS-PGM       TO PI-RETURN-TO-PROGRAM
01353              MOVE XCTL-EM1273    TO PGM-NAME
01354              GO TO 9300-XCTL
01355          ELSE
01356              MOVE XCTL-1273      TO PGM-NAME
01357              GO TO 9300-XCTL.
01358
01359      IF EIBAID = DFHPF6
050107       OR (EIBAID = DFHPF22 AND PI-PFKEY-USED = DFHPF6)
01360          MOVE XCTL-154           TO PGM-NAME
01361          GO TO 9300-XCTL.
01362
01363      IF EIBAID = DFHPF7
050107       OR (EIBAID = DFHPF22 AND PI-PFKEY-USED = DFHPF7)
01364          IF PI-SAVE-SYS-ID = 'CV'
01365              IF PI-SAVE-TYPE = PI-LIFE-OVERRIDE-L1
01366                  MOVE XCTL-EM156    TO  PGM-NAME
01367                  GO TO 9300-XCTL
01368              ELSE
01369                  MOVE XCTL-EM1561   TO  PGM-NAME
01370                  GO TO 9300-XCTL
01371          ELSE
01372              MOVE XCTL-156          TO PGM-NAME
01373              GO TO 9300-XCTL.
01374
01375      IF EIBAID = DFHPF8
01376          MOVE XCTL-151           TO PGM-NAME
01377          GO TO 9300-XCTL.
01378
01379      IF EIBAID = DFHPF9
01380          MOVE XCTL-152           TO PGM-NAME
01381          GO TO 9300-XCTL.
01382
01383      IF EIBAID = DFHPF10
01384          MOVE PI-SAVE-LOW        TO PI-PASS-AREA
01385          MOVE PI-PRIORITY-CD     TO PI-EL142-PRIORITY
01386          MOVE XCTL-142           TO PGM-NAME
01387          GO TO 9300-XCTL.
01388
01389      IF EIBAID = DFHPF11
01390         IF PI-COMPANY-ID = 'DMD'
01391            MOVE XCTL-155           TO PGM-NAME
01392            GO TO 9300-XCTL
01393          ELSE
                 MOVE XCTL-1503          TO PGM-NAME
01395            GO TO 9300-XCTL.
01396
01397      IF EIBAID = DFHPF13
01398          MOVE PI-SAVE-FORM       TO PI-PASS-AREA
021615         MOVE XCTL-1504          TO PGM-NAME
01400          GO TO 9300-XCTL.
01401
01402      IF EIBAID = DFHPF14
01403          MOVE XCTL-141           TO PGM-NAME
01404          GO TO 9300-XCTL.
01405
01406      IF EIBAID = DFHPF17
01407          MOVE XCTL-162           TO PGM-NAME
01408          GO TO 9300-XCTL.
01409
01410      IF EIBAID = DFHPF18
062602         MOVE PI-PRIORITY-CD     TO PI-EL142-PRIORITY
01411          MOVE XCTL-1501          TO PGM-NAME
01412          GO TO 9300-XCTL.
01413
01414      IF EIBAID = DFHPF19
01415          IF PI-SAVE-SYS-ID = 'CV'
01416              MOVE THIS-PGM       TO PI-RETURN-TO-PROGRAM
01417              MOVE XCTL-EM1276    TO PGM-NAME
01418              GO TO 9300-XCTL
01419          ELSE
01420          IF PI-COMPANY-ID = 'DMD'
01421              MOVE 'EL401DMD'     TO PGM-NAME
01422              GO TO 9300-XCTL
01423           ELSE
102809             MOVE XCTL-1279      TO PGM-NAME
01425              GO TO 9300-XCTL.
01426
01427      IF EIBAID = DFHPF20
062602         MOVE PI-PRIORITY-CD     TO PI-EL142-PRIORITY
01428          MOVE XCTL-1502          TO PGM-NAME
01429          GO TO 9300-XCTL.
040416     IF EIBAID = DFHPF21
040416         MOVE XCTL-1284          TO PGM-NAME
040416         GO TO 9300-XCTL.
01430      EJECT
01431  0600-RECOVER-TEMP-STORAGE.
01432      MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.
01433
01434      
      * EXEC CICS HANDLE CONDITION
01435 *        QIDERR   (0690-QIDERR)
01436 *    END-EXEC
      *    MOVE '"$N                   ! '' #00009190' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303039313930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01437
01438      
      * EXEC CICS READQ TS
01439 *        QUEUE    (QID)
CIDMOD*        INTO     (PROGRAM-INTERFACE-BLOCK)
01440 *        INTO     (TS-AREA)
CIDMOD*        LENGTH   (PI-COMM-LENGTH)
01441 *        LENGTH   (TS-LENGTH)
01442 *    END-EXEC
      *    MOVE '*$I    L              ''   #00009194' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-AREA, 
                 TS-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01443
CIDMOD     MOVE TS-PI-AREA        TO PROGRAM-INTERFACE-BLOCK
CIDMOD
01444      
      * EXEC CICS DELETEQ TS
01445 *        QUEUE   (QID)
01446 *    END-EXEC.
      *    MOVE '*&                    #   #00009204' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01447
01448      IF RETURNED-FROM NOT = XCTL-EM1273
01449          MOVE SAVE-CONTROL       TO PI-CONTROL-IN-PROGRESS.
01450
01451      MOVE LOW-VALUES             TO EL150AO.
01452
082707*    IF PI-SAVE-MCRFLM NOT = LOW-VALUES
082707*        MOVE PI-SAVE-MCRFLM     TO MCRFLMO.
01455
01456      IF PI-SAVE-FILETO NOT = LOW-VALUES
01457          MOVE PI-SAVE-FILETO     TO FILETOO.
01458
CIDMOD     IF TS-PI-SAVE-CLOAN NOT = LOW-VALUES
CIDMOD        MOVE TS-PI-SAVE-CLOAN   TO CLOANO
CIDMOD     END-IF
01458
01459      IF PI-SAVE-PRTOPT NOT = LOW-VALUES
01460          MOVE PI-SAVE-PRTOPT     TO PRTOPTO
01461          MOVE AL-UANON           TO PRTOPTA.
01462
01463      IF PI-SAVE-FORMAT NOT = LOW-VALUES
01464          MOVE PI-SAVE-FORMAT     TO FORMATO
01465          MOVE AL-UANON           TO FORMATA.
01466
01467      IF PI-SAVE-ACT-CD NOT = LOW-VALUES
01468          MOVE PI-SAVE-ACT-CD     TO ACTCDO.
01469
01470      IF PI-SAVE-ASSOC  NOT = LOW-VALUES
01471          MOVE PI-SAVE-ASSOC      TO ASSOCO.
01472
01473      MOVE 'N'                    TO DIRECTION-SWITCH.
050107
050107     MOVE SPACES TO PI-PFKEY-USED.
050107     MOVE 0      TO PI-FORCE-COUNT.
01474
01475      GO TO 1000-SHOW-CLAIM.
01476
01477  0690-QIDERR.
01478      MOVE ER-0033                TO EMI-ERROR.
01479      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01480      MOVE LOW-VALUES             TO EL150AO.
01481      MOVE -1                     TO FILETOL.
01482      GO TO 8100-SEND-INITIAL-MAP.
01483
01484      EJECT
01485  1000-SHOW-CLAIM.
01486
01487      IF CLMNOL > 0
01488          MOVE CLMNOI             TO PI-CLAIM-NO.
01489
01490      IF CARRL > 0
01491          MOVE CARRI              TO PI-CARRIER.
01492
01493      IF CERTNOL > 0
01494          MOVE CERTNOI            TO PI-CERT-PRIME.
01495
01496      IF SUFXL > 0
01497          MOVE SUFXI              TO PI-CERT-SFX.
01498
01499      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
01500      MOVE PI-CARRIER             TO MSTR-CARRIER.
01501      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
01502      MOVE PI-CERT-NO             TO MSTR-CERT-NO.
01503      MOVE 'MSTR'                 TO FILE-SWITCH.
01504
01505  1000-READ-CLAIM.
01506
01507      
      * EXEC CICS HANDLE CONDITION
01508 *        NOTFND   (1100-SHOW-RECORD-NOT-FOUND)
01509 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00009274' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303039323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01510
01511      
      * EXEC CICS READ
01512 *        DATASET   (PI-CURRENT-FILE)
01513 *        RIDFLD    (ELMSTR-KEY)
01514 *        INTO      (CLAIM-MASTER)
01515 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00009278' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01516
01517  1000-CONTINUE-WITH-RETRIEVED.
01518
010816     if (pi-company-id = 'CID' OR 'DCC' OR 'AHL' or 'CAP'
062121           OR 'FNL')
062602        and (cl-priority-cd = '8')
062602        move 'S'                 to pi-priority-ind
062602     end-if
062602
01519      IF PI-COMPANY-ID = 'DMD'
01520          IF HIGHEST-PRIORITY
01521              MOVE 'S'            TO PI-PRIORITY-IND
01522              MOVE ER-0933        TO EMI-ERROR
01523              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01524          ELSE
01525              IF NOT CONFIDENTIAL-DATA
01526                  MOVE LOW-VALUES TO PI-PRIORITY-IND.
01527
01528      IF PI-COMPANY-ID = 'DMD'
01529              AND
01530         PI-ASS-PROCESSING
01531              AND
01532         PI-PREV-PRIME-CERT-NO = CL-PRIME-CERT-NO
01533          NEXT SENTENCE
01534      ELSE
01535          MOVE ELMSTR-KEY         TO PI-PREV-CLAIM
01536          MOVE CL-PRIME-CERT-NO   TO PI-PREV-PRIME-CERT-NO.
01537
01538      MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.
01539      MOVE CL-PRIORITY-CD         TO PI-PRIORITY-CD.
01540
01541      IF CL-LAST-MAINT-HHMMSS IS NOT NUMERIC
01542          MOVE EIBTIME            TO PI-UPDATE-HHMMSS
01543      ELSE
01544          MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
01545
01546      MOVE CL-TRAILER-SEQ-CNT     TO PI-SAVE-CNT.
CIDMOD
CIDMOD     IF CL-YESNOSW = 'Y' OR 'N'
CIDMOD         MOVE CL-YESNOSW         TO YESNOSWO
CIDMOD     ELSE
CIDMOD         MOVE 'Y'                TO YESNOSWO
CIDMOD     END-IF.
CIDMOD
01547      MOVE CL-CLAIM-NO            TO CLMNOO
01548                                     PI-CLAIM-NO.
01549      MOVE CL-CARRIER             TO CARRO
01550                                     PI-CARRIER.
01551      MOVE CL-CERT-PRIME          TO CERTNOO
01552                                     PI-CERT-PRIME.
01553      MOVE CL-CERT-SFX            TO SUFXO
01554                                     PI-CERT-SFX.
01555      MOVE CL-CCN                 TO CRDCARDO
01556
01557      MOVE CL-SYSTEM-IDENTIFIER   TO PI-SAVE-SYS-ID.
01558
01559      IF CL-SYSTEM-IDENTIFIER = 'CV'
01560          MOVE CL-CV-REFERENCE-NO TO PI-MP-REFERENCE-NO.
01561
01562      IF CL-ASSOC-CERT-TOTAL = +0 OR +1
01563          MOVE SPACES                 TO PRIMCRTO
01564                                         PRIMSFXO
01565                                         PRIMHDGO
01566                                         SEQUO
01567          MOVE AL-SADOF               TO PRIMCRTA
01568                                         PRIMSFXA
01569                                         PRIMHDGA
01570                                         SEQUA
01571      ELSE
01572          MOVE CL-ASSOC-CERT-SEQU     TO WS-CUR-SEQU
01573          MOVE CL-ASSOC-CERT-TOTAL    TO WS-OF-SEQU
01574          MOVE WS-CLAIM-SEQUENCE      TO SEQUO
01575          MOVE CL-PRIME-CERT-PRIME    TO PRIMCRTO
01576          MOVE CL-PRIME-CERT-SFX      TO PRIMSFXO
01577          MOVE 'PRIME CERT :'         TO PRIMHDGO
01578          MOVE AL-SABON               TO PRIMCRTA
01579                                         PRIMSFXA
01580                                         PRIMHDGA
01581                                         SEQUA.
01582
01583      MOVE CL-CLAIM-TYPE          TO PI-SAVE-TYPE.
01584      MOVE CL-CERT-GROUPING       TO PI-GROUPING.
01585      MOVE CL-CERT-STATE          TO PI-STATE.
01586
01587      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01588          MOVE CL-CERT-ACCOUNT    TO  PI-ACCOUNT
01589          MOVE CL-CURRENT-ACCOUNT TO  ACCTO
01590      ELSE
01591          MOVE CL-CERT-ACCOUNT    TO  PI-ACCOUNT
01592                                      ACCTO.
01593
01594      MOVE CL-CERT-EFF-DT         TO PI-CERT-EFF-DT.
01595
121802     EVALUATE TRUE
121802        WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
121802           MOVE PI-AH-OVERRIDE-L6
                                       TO CLMTYPO
121802
121802        WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
121802           MOVE PI-LIFE-OVERRIDE-L6
                                       TO CLMTYPO
121802
121802        WHEN CL-CLAIM-TYPE = 'I'
121802           MOVE '  IU  '         TO CLMTYPO
121802
121802        WHEN CL-CLAIM-TYPE = 'G'
121802           MOVE ' GAP  '         TO CLMTYPO
052614
052614        WHEN CL-CLAIM-TYPE = 'F'
052614           MOVE ' FAM  '         TO CLMTYPO
080322
080322        WHEN CL-CLAIM-TYPE = 'B'
080322           MOVE ' BRV  '         TO CLMTYPO
080322        WHEN CL-CLAIM-TYPE = 'H'
080322           MOVE ' HSP '          TO CLMTYPO
100518
100518        WHEN CL-CLAIM-TYPE = 'O'
100518           MOVE ' OTH  '         TO CLMTYPO
121802
121802     END-EVALUATE
01602      IF CLAIM-IS-OPEN
01603          MOVE ' OPEN'            TO CLMSTATO
01604      ELSE
01605          MOVE 'CLOSED'           TO CLMSTATO.
01606
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK'
121802*        IF (CLAIM-IS-CLOSED)  AND
121802*           (CL-LAST-CLOSE-REASON = '2')
121802*            MOVE 'DENIED'       TO CLMSTATO.
01611
01612      MOVE CL-INSURED-LAST-NAME   TO LSTNMEO.
01613      MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.
01614      MOVE CL-INSURED-MID-INIT    TO MINITO.
01615
01616      IF CL-NO-OF-PMTS-MADE = +0 AND
01617         CL-TOTAL-PAID-AMT = +0
01618          NEXT SENTENCE
01619      ELSE
01620          IF CL-PAID-THRU-DT NOT = LOW-VALUES
01621              MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-1
01622              MOVE ' '                    TO  DC-OPTION-CODE
01623              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01624              MOVE DC-GREG-DATE-1-EDIT    TO  PDTHRUO
01625              IF PI-USES-PAID-TO
01626                  MOVE CL-PAID-THRU-DT    TO  DC-BIN-DATE-1
01627                  MOVE '6'                TO  DC-OPTION-CODE
01628                  MOVE +1                 TO  DC-ELAPSED-DAYS
01629                  MOVE +0                 TO  DC-ELAPSED-MONTHS
01630                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01631                  MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO.
01632
01633      IF CL-INCURRED-DT NOT = LOW-VALUES
01634          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
01635          MOVE ' '                TO DC-OPTION-CODE
01636          MOVE +0                 TO DC-ELAPSED-DAYS
01637                                     DC-ELAPSED-MONTHS
01638          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01639          MOVE DC-GREG-DATE-1-EDIT TO INCURO.
01640
042110     IF CL-DENIAL-TYPE NOT = SPACES AND LOW-VALUES
042110        EVALUATE CL-DENIAL-TYPE
042110           WHEN '1'
042110              MOVE 'DENIAL STATUS - DENIAL       '
042110                                 TO DENTYPO
042110           WHEN '2'
042110              MOVE 'DENIAL STATUS - RESCISSION   '
042110                                 TO DENTYPO
042110           WHEN '3'
042110              MOVE 'DENIAL STATUS - REFORMATION  '
042110                                 TO DENTYPO
042110           WHEN '4'
042110              MOVE 'DENIAL STATUS - REF TO RESC  '
042110                                 TO DENTYPO
042110           WHEN '5'
042110              MOVE 'DENIAL STATUS - RECONSIDERED '
042110                                 TO DENTYPO
042110        END-EVALUATE
042110        MOVE AL-SANOF            TO DENTYPA
042110     END-IF
01641      IF CL-NEXT-AUTO-PAY-DT NOT = LOW-VALUES
01642          MOVE CL-NEXT-AUTO-PAY-DT TO DC-BIN-DATE-1
01643          MOVE ' '                TO DC-OPTION-CODE
01644          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01645          MOVE DC-GREG-DATE-1-EDIT TO NXTAUTOO
01646          IF PI-USES-PAID-TO
01647              MOVE CL-NEXT-AUTO-PAY-DT   TO  DC-BIN-DATE-1
01648              MOVE '6'                   TO  DC-OPTION-CODE
01649              MOVE +1                    TO  DC-ELAPSED-DAYS
01650              MOVE +0                    TO  DC-ELAPSED-MONTHS
01651              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01652              IF NO-CONVERSION-ERROR
01653                  MOVE DC-GREG-DATE-1-EDIT   TO  NXTAUTOO
01654              ELSE
01655                  MOVE LOW-VALUES            TO  NXTAUTOO.
01656
01657      IF CL-FILE-ESTABLISH-DT NOT = LOW-VALUES
01658          MOVE CL-FILE-ESTABLISH-DT      TO  DC-BIN-DATE-1
01659          MOVE ' '                       TO  DC-OPTION-CODE
01660          MOVE +0                        TO  DC-ELAPSED-DAYS
01661                                             DC-ELAPSED-MONTHS
01662          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01663          IF NO-CONVERSION-ERROR
01664              MOVE DC-GREG-DATE-1-EDIT   TO  ESTABDTO
01665          ELSE
01666              MOVE SPACES                TO  ESTABDTO.
01667
082707*    MOVE CL-MICROFILM-NO        TO MCRFLMO.
01669      MOVE CL-FILE-LOCATION       TO FILETOO.
01670
01671      IF CL-CERT-EFF-DT NOT = LOW-VALUES
01672          MOVE CL-CERT-EFF-DT     TO DC-BIN-DATE-1
01673          MOVE ' '                TO DC-OPTION-CODE
01674          MOVE +0                 TO DC-ELAPSED-DAYS
01675                                     DC-ELAPSED-MONTHS
01676          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01677          MOVE DC-GREG-DATE-1-EDIT TO EFFECTO.
01678
082707     IF CL-TOTAL-INT-PAID NOT NUMERIC
082707        MOVE +0                  TO CL-TOTAL-INT-PAID
082707     END-IF
           MOVE CL-TOTAL-INT-PAID      TO TOTINTO
01679      MOVE CL-TOTAL-PAID-AMT      TO TOTPAIDO.
050107     MOVE CL-TOTAL-PAID-AMT      TO PI-TOTAL-PAID.
01680
01681      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01682          MOVE CL-CURRENT-STATE   TO  STATEO
01683      ELSE
01684      IF PI-COMPANY-ID = 'DMD'
01685          MOVE PI-COMPANY-CD          TO CERT-COMP-CD
01686          MOVE CL-CERT-CARRIER        TO CERT-CARRIER
01687          MOVE CL-CERT-GROUPING       TO CERT-GROUPING
01688          MOVE CL-CERT-STATE          TO CERT-STATE
01689          MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
01690          MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT
01691          MOVE CL-CERT-NO             TO CERT-CERT-NO
01692          MOVE 'CERT'                 TO FILE-SWITCH
01693          
      * EXEC CICS READ
01694 *            DATASET   ('ELCERT')
01695 *            SET       (ADDRESS OF CERTIFICATE-MASTER)
01696 *            RIDFLD    (ELCERT-KEY)
01697 *        END-EXEC
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009519' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353139' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01698          MOVE CM-RESIDENT-STATE  TO  STATEO
01699      ELSE
01700          MOVE CL-CERT-STATE      TO  STATEO.
01701
01702      MOVE CL-ACTIVITY-CODE       TO  ACTCDO.
01703      MOVE CL-ACTIVITY-MAINT-TYPE TO  ACTTYPO.
01704
01705      IF CL-ACTIVITY-MAINT-DT = SPACES OR LOW-VALUES
01706          MOVE SPACES                     TO  ACTDTO
01707      ELSE
01708          MOVE CL-ACTIVITY-MAINT-DT       TO  DC-BIN-DATE-1
01709          MOVE ' '                        TO  DC-OPTION-CODE
01710          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01711          IF NO-CONVERSION-ERROR
01712              MOVE DC-GREG-DATE-1-EDIT    TO  ACTDTO
01713          ELSE
01714              MOVE SPACES                 TO  ACTDTO.
01715
01716      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01717          MOVE 'ASSOC:'               TO  ASSHDGO
01718          MOVE AL-SABON               TO  ASSHDGA
01719          MOVE CL-ASSOCIATES          TO  ASSOCO
01720      ELSE
01721          MOVE SPACES                 TO  ASSHDGO
01722          MOVE AL-SADOF               TO  ASSOCA.
01723
01724      MOVE CL-CLAIM-STATUS        TO PI-STATUS-IND.
01725      MOVE CL-LAST-CLOSE-REASON   TO PI-LAST-CLOSE-REASON-IND.
01726
01727      IF PI-COMPANY-ID = 'DMD'
01728              AND
01729          ((PI-ASS-PROCESSING  AND
01730            PI-PREV-PRIME-CERT-NO = CL-PRIME-CERT-NO)
01731                  OR
01732          ((PI-CLOSED AND
01733           (PI-BENEFIT-CHANGE OR PI-ERROR-CORRECTION))
01734                  OR
01735            CL-CERT-NO NOT = CL-PRIME-CERT-NO))
01736          PERFORM 1200-GET-ASS-TOTALS THRU 1200-EXIT
01737      ELSE
01738          MOVE LOW-VALUES             TO PI-LAST-PF-KEY-IND
01739                                         PI-ASSOCIATED-CERTS-TABLE
01740                                         PI-ASSOCIATED-PROCESS-IND
01741          MOVE SPACES                 TO AMTPDO
01742                                         DAYSPDO
01743                                         CLMTOTHO
01744                                         DAYSPDHO
050107*01745          MOVE ZEROS                  TO PI-TOTAL-PAID
050107         MOVE ZEROS                  TO
01746                                         PI-DAYS-PAID
01747                                         PI-LAST-SEQ-NO
01748                                         PI-ASS-NDX.
01749
01750      
      * EXEC CICS HANDLE CONDITION
01751 *        NOTFND    (1005-END-BUILD-DIAGNOSIS)
01752 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00009577' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303039353737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01753
01754      MOVE CL-CONTROL-PRIMARY  TO ELTRLR-KEY.
01755      MOVE +90                 TO TRLR-SEQ-NO.
01756      MOVE 'TRLR'              TO FILE-SWITCH.
01757
01758      
      * EXEC CICS READ
01759 *        DATASET   ('ELTRLR')
01760 *        RIDFLD    (ELTRLR-KEY)
01761 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
01762 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009585' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353835' TO DFHEIV0(25:11)
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
           
01763
01764      MOVE AT-INFO-LINE-1         TO DIAGO.
01765
01766  1005-END-BUILD-DIAGNOSIS.
01767
CIDMOD     MOVE CL-CONTROL-PRIMARY  TO ELTRLR-KEY.
CIDMOD     MOVE +91                 TO TRLR-SEQ-NO.
CIDMOD     MOVE 'TRLR'              TO FILE-SWITCH.
CIDMOD
CIDMOD     
      * EXEC CICS READ
CIDMOD*        DATASET   ('ELTRLR')
CIDMOD*        RIDFLD    (ELTRLR-KEY)
CIDMOD*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
CIDMOD*        RESP      (WS-RESPONSE)
CIDMOD*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00009599' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039353939' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     IF WS-RESP-NORMAL
CIDMOD        MOVE AT-INFO-LINE-1      TO CLOANO
CIDMOD     ELSE
CIDMOD        MOVE SPACES              TO CLOANO
CIDMOD     END-IF
CIDMOD
01768      IF CL-SYSTEM-IDENTIFIER = 'CV'
01769          PERFORM 1500-READ-CONVENIENCE-FILES THRU 1500-EXIT
01770          GO TO 1050-SHOW-CONTINUE.
01771
01772      
      * EXEC CICS HANDLE CONDITION
01773 *        NOTFND   (1100-SHOW-RECORD-NOT-FOUND)
01774 *    END-EXEC.
      *    MOVE '"$I                   ! * #00009616' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303039363136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01775
01776      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
01777      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
01778      MOVE CL-CERT-GROUPING       TO CERT-GROUPING.
01779      MOVE CL-CERT-STATE          TO CERT-STATE.
01780      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
01781      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
01782      MOVE CL-CERT-NO             TO CERT-CERT-NO.
01783      MOVE 'CERT'                 TO FILE-SWITCH.
01784
01785      
      * EXEC CICS READ
01786 *        DATASET   ('ELCERT')
01787 *        SET       (ADDRESS OF CERTIFICATE-MASTER)
01788 *        RIDFLD    (ELCERT-KEY)
01789 *    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009629' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363239' TO DFHEIV0(25:11)
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
           
01790
01791      MOVE 'Y'                    TO WS-CERT-READ-SW.
01792      MOVE LOW-VALUES             TO ERACCT-KEY.
01793      MOVE PI-COMPANY-CD          TO ACCT-COMP-CD.
01794      MOVE CL-CERT-CARRIER        TO ACCT-CARRIER.
01795      MOVE CL-CERT-GROUPING       TO ACCT-GROUPING.
01796      MOVE CL-CERT-STATE          TO ACCT-STATE.
01797      MOVE CL-CERT-ACCOUNT        TO ACCT-ACCOUNT.
01798      MOVE CL-CERT-EFF-DT         TO ACCT-EXP-DT.
01799      MOVE 'ACCT'                 TO FILE-SWITCH.
01800
01801      MOVE ERACCT-PARTIAL-KEY     TO WS-ERACCT-SAVE-KEY.
01802      MOVE SPACES                 TO WS-ERACCT-HOLD-RECORD.
01803
01804      
      * EXEC CICS HANDLE CONDITION
01805 *         NOTFND   (1005-NO-ERACCT)
01806 *         ENDFILE  (1005-NO-ERACCT)
01807 *    END-EXEC.
      *    MOVE '"$I''                  ! + #00009648' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303039363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01808
01809      
      * EXEC CICS STARTBR
01810 *         DATASET   ('ERACCT')
01811 *         RIDFLD    (ERACCT-KEY)
01812 *         GTEQ
01813 *    END-EXEC.
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009653' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01814
01815  1005-READ-NEXT-ACCOUNT.
01816
01817      
      * EXEC CICS READNEXT
01818 *         DATASET   ('ERACCT')
01819 *         SET       (ADDRESS OF ACCOUNT-MASTER)
01820 *         RIDFLD    (ERACCT-KEY)
01821 *    END-EXEC.
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009661' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363631' TO DFHEIV0(25:11)
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
           
01822
01823      IF WS-ERACCT-SAVE-KEY NOT = ERACCT-PARTIAL-KEY
01824         IF WS-ERACCT-HOLD-RECORD = SPACES
01825            GO TO 1005-NO-ERACCT
01826         ELSE
01827            MOVE WS-ERACCT-HOLD-RECORD TO ACCOUNT-MASTER
01828            GO TO 1005-CONTINUE.
01829
01830      IF ACCT-EXP-DT = HIGH-VALUES
01831         NEXT SENTENCE
01832      ELSE
01833         MOVE ACCOUNT-MASTER      TO WS-ERACCT-HOLD-RECORD
01834         GO TO 1005-READ-NEXT-ACCOUNT.
01835
01836  1005-CONTINUE.
01837
01838      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
01839         MOVE ZEROS               TO AM-3RD-PARTY-NOTIF-LEVEL.
01840
01841      IF AM-3RD-PARTY-NOTIF-LEVEL > ZERO
01842         MOVE 'THIRD PARTY'       TO NOTIFYO.
01843
01844  1005-NO-ERACCT.
01845
050107     IF CM-INSURED-LAST-NAME EQUAL CL-INSURED-LAST-NAME  AND
050107        CM-INSURED-FIRST-NAME EQUAL CL-INSURED-1ST-NAME  AND
050107        CM-INSURED-INITIAL2 EQUAL CL-INSURED-MID-INIT
050107            MOVE 'N' TO PI-JOINT-INSURED-IND
050107     ELSE
050107            MOVE 'Y' TO PI-JOINT-INSURED-IND
050107     END-IF.
050107
01846      IF CM-SING-PRM
01847          MOVE 'SING PRM'         TO PRMTYPEO
01848      ELSE
01849          IF CM-O-B-COVERAGE
01850              MOVE 'OUT BAL'      TO PRMTYPEO
01851          ELSE
01852              MOVE 'OPEN END'     TO PRMTYPEO.
01853
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
01855         GO TO 1006-CALC-LIFE.
01856
01857      MOVE 'MO. BENEFIT :'       TO BENECAPO.
01858      MOVE CM-CERT-EFF-DT        TO CP-CERT-EFF-DT.
01859      MOVE CM-LOAN-1ST-PMT-DT    TO CP-FIRST-PAY-DATE.
01860
01861      IF PI-COMPANY-ID = 'DMD'
01862          PERFORM 9830-DMD-REMAINING-TERM THRU 9830-EXIT
01863          MOVE W-PAYMENTS         TO NOPMTSO
01864          GO TO 1005-RTRM-CONT
01865      ELSE
01866          MOVE SAVE-BIN-DATE      TO CP-VALUATION-DT
01867          MOVE SPACES             TO W-PAYMENTS
01868          MOVE CL-NO-OF-PMTS-MADE TO W-PYMTS
01869          MOVE W-PAYMENTS         TO NOPMTSO.
01870
01871      IF CM-AH-ORIG-TERM = +0
01872         MOVE +1                 TO CP-ORIGINAL-TERM
01873      ELSE
01874         MOVE CM-AH-ORIG-TERM    TO CP-ORIGINAL-TERM.
01875
01876      MOVE PI-REM-TRM-CALC-OPTION
01877                                 TO CP-REM-TRM-CALC-OPTION.
01878      MOVE '4'                   TO CP-REM-TERM-METHOD.
01879      MOVE PI-COMPANY-ID         TO CP-COMPANY-ID
01880      PERFORM 8900-GET-FREE-LOOK THRU 8900-EXIT.
01881      PERFORM 9800-LINK-REM-TERM.
01882      MOVE CP-REMAINING-TERM-3   TO WST-REM.
01883
01884  1005-RTRM-CONT.
061013     move zeros                  to pi-max-benefit
010816     if pi-company-id = 'DCC' or 'CAP'
061013        and am-dcc-product-code not = '   '
061013        move ' '                 to ws-pdef-record-sw
061013        perform 3997-get-erpdef  thru 3997-exit
061013        if pdef-found
080322           move cl-insured-birth-dt
080322                                 to dc-bin-date-1
080322           move cl-incurred-dt   to dc-bin-date-2
080322           move '1'              to dc-option-code
080322           PERFORM 9700-LINK-DATE-CONVERT
080322                                 THRU 9700-EXIT
080322           compute ws-att-age =
080322              dc-elapsed-months / 12
080322           move zeros to dc-elapsed-months dc-elapsed-days
061013           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
080322              (P1 > +11)
061013              OR (PD-PROD-CODE (P1) = cl-claim-type
080322                   AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
061013           END-PERFORM
080322           IF P1 < +12
100314              if pd-ben-pct (p1) not numeric
100314                 move zeros      to pd-ben-pct (p1)
100314              end-if
100314              if pd-ben-pct (p1) = zeros
100314                 move +1         to ws-work-ben-pct
100314              else
100314                 move pd-ben-pct (p1)
100314                                 to ws-work-ben-pct
100314              end-if
100314              compute cm-ah-benefit-amt =
100314                 cm-ah-benefit-amt * ws-work-ben-pct
061013              MOVE PD-MAX-AMT (P1)
061013                                 TO pi-max-benefit
061013           END-IF
061013        end-if
061013     end-if
061013     if (pi-max-benefit not = zeros)
061013        and (cm-ah-benefit-amt > pi-max-benefit)
061013        move pi-max-benefit      to beneo
061013     else
061013        MOVE CM-AH-BENEFIT-AMT   TO BENEO
061013     end-if
01888      MOVE CM-POLICY-FORM-NO     TO PI-SAVE-FORM.
01889      MOVE CM-STATE              TO PI-CR-STATE.
01890      MOVE WS-TERMS              TO TERMSO.
01891      MOVE CM-AH-ORIG-TERM       TO WST-ORIG
01892                                    DC-ELAPSED-MONTHS
01893      MOVE WS-TERMS              TO TERMSO.
01894      MOVE CM-AH-CURRENT-STATUS TO WS-STATUS.
01895
01896      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01897          MOVE CM-LOAN-1ST-PMT-DT     TO  DC-BIN-DATE-1
01898          MOVE '6'                    TO  DC-OPTION-CODE
01899          MOVE '1'                    TO  DC-END-OF-MONTH
01900          MOVE +0                     TO  DC-ELAPSED-DAYS
01901                                          DC-ODD-DAYS-OVER
01902          COMPUTE DC-ELAPSED-MONTHS = CM-AH-ORIG-TERM - +1
01903          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01904          IF NO-CONVERSION-ERROR
01905              MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
01906          ELSE
01907              MOVE SPACES                 TO  EXPIREO
01908      ELSE
01909          MOVE CM-AH-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
01910          MOVE ' '                    TO  DC-OPTION-CODE
01911          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01912          IF NO-CONVERSION-ERROR
01913              MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
01914          ELSE
01915              MOVE SPACES                 TO  EXPIREO.
01916
01917      GO TO 1009-CONTINUE-BUILD.
01918
01919  1006-CALC-LIFE.
01920
01921      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
01922      MOVE '4'                    TO CNTL-REC-TYPE.
01923      MOVE SPACES                 TO CNTL-ACCESS.
01924      MOVE CM-LF-BENEFIT-CD       TO CNTL-BEN-NO.
01925      MOVE +0                     TO CNTL-SEQ-NO.
01926      MOVE 'CNTL'                 TO FILE-SWITCH.
01927
01928      
      * EXEC CICS HANDLE CONDITION
01929 *         NOTFND    (1008-NOTFND)
01930 *         ENDFILE   (1008-NOTFND)
01931 *    END-EXEC.
      *    MOVE '"$I''                  ! , #00009820' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303039383230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01932
01933      
      * EXEC CICS READ
01934 *        DATASET   ('ELCNTL')
01935 *        SET       (ADDRESS OF CONTROL-FILE)
01936 *        RIDFLD    (ELCNTL-KEY)
01937 *        GTEQ
01938 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00009825' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383235' TO DFHEIV0(25:11)
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
           
01939
01940      IF PI-COMPANY-ID NOT = CF-COMPANY-ID OR
01941         CF-RECORD-TYPE NOT = '4'
01942           GO TO 1008-NOTFND.
01943
01944      MOVE +1 TO WS-INDEX.
01945
01946  1007-TABLE-LOOKUP.
01947
01948      IF CM-LF-BENEFIT-CD = CF-BENEFIT-CODE (WS-INDEX)
01949         MOVE CF-LF-COVERAGE-TYPE (WS-INDEX) TO CP-BENEFIT-TYPE
01950                                                WS-LF-COVERAGE-TYPE
01951         MOVE CF-CO-EARNINGS-CALC (WS-INDEX) TO WS-EARNING-METHOD
01952         MOVE CF-SPECIAL-CALC-CD (WS-INDEX)  TO CP-SPECIAL-CALC-CD
01953         GO TO 1008-CONTINUE.
01954
01955      IF CF-BENEFIT-CODE (WS-INDEX) NOT < CF-HI-BEN-IN-REC
01956         GO TO 1008-NOTFND.
01957
01958      IF WS-INDEX < +8
01959         ADD +1 TO WS-INDEX
01960         GO TO 1007-TABLE-LOOKUP.
01961
01962  1008-NOTFND.
01963
01964      MOVE ZEROS TO BENEO.
01965      GO TO 1009-CONTINUE-BUILD.
01966
01967  1008-CONTINUE.
01968
01969      MOVE CM-CERT-EFF-DT        TO CP-CERT-EFF-DT.
01970      MOVE CM-LOAN-1ST-PMT-DT    TO CP-FIRST-PAY-DATE.
01971      MOVE CL-INCURRED-DT        TO CP-VALUATION-DT.
01972
01973      IF CM-LF-ORIG-TERM = +0
01974         MOVE +1                TO CP-ORIGINAL-TERM
01975      ELSE
01976         MOVE CM-LF-ORIG-TERM   TO CP-ORIGINAL-TERM.
01977
01978      MOVE CM-LF-ORIG-TERM      TO WST-ORIG
01979                                   DC-ELAPSED-MONTHS.
01980      MOVE CM-LF-CURRENT-STATUS TO WS-STATUS.
01981
01982      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01983          MOVE CM-LOAN-1ST-PMT-DT     TO  DC-BIN-DATE-1
01984          MOVE '6'                    TO  DC-OPTION-CODE
01985          MOVE '1'                    TO  DC-END-OF-MONTH
01986          MOVE +0                     TO  DC-ELAPSED-DAYS
01987                                          DC-ODD-DAYS-OVER
01988          COMPUTE DC-ELAPSED-MONTHS = CM-LF-ORIG-TERM - +1
01989          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01990          IF NO-CONVERSION-ERROR
01991              MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
01992          ELSE
01993              MOVE SPACES                 TO  EXPIREO
01994      ELSE
01995          MOVE CM-LF-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
01996          MOVE ' '                    TO  DC-OPTION-CODE
01997          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01998          IF NO-CONVERSION-ERROR
01999              MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
02000          ELSE
02001              MOVE SPACES                 TO  EXPIREO.
02002
02003      MOVE PI-REM-TRM-CALC-OPTION
02004                                 TO CP-REM-TRM-CALC-OPTION.
02005      MOVE '4'                   TO CP-REM-TERM-METHOD.
02006      MOVE WS-EARNING-METHOD     TO CP-EARNING-METHOD.
02007      MOVE PI-COMPANY-ID         TO CP-COMPANY-ID.
02008      PERFORM 8900-GET-FREE-LOOK THRU 8900-EXIT.
02009      PERFORM 9800-LINK-REM-TERM.
02010
02011      MOVE CP-REMAINING-TERM-3    TO WST-REM.
02012      MOVE SPACES                 TO WST-REM-DAYS-GRP
02013                                     WST-ORIG-DAYS-GRP.
02014      MOVE WS-TERMS               TO TERMSO.
02015
02016      MOVE SPACES                 TO W-PAYMENTS
02017      MOVE CL-NO-OF-PMTS-MADE     TO W-PYMTS
02018      MOVE W-PAYMENTS             TO NOPMTSO.
02019
02020  1009-CALC-REM-AMT.
02021
02022      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02023      MOVE PI-STATE               TO WS-ST-ACCESS.
02024      MOVE WS-STATE-ACCESS        TO CNTL-ACCESS.
02025      MOVE '3'                    TO CNTL-REC-TYPE.
02026      MOVE +0                     TO CNTL-SEQ-NO.
02027      MOVE 'CNTL'                 TO  FILE-SWITCH.
02028
02029      
      * EXEC CICS HANDLE CONDITION
02030 *        NOTFND   (1009-CONTINUE-BUILD)
02031 *        ENDFILE  (1009-CONTINUE-BUILD)
02032 *    END-EXEC.
      *    MOVE '"$I''                  ! - #00009921' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303039393231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02033
02034      
      * EXEC CICS READ
02035 *        DATASET   ('ELCNTL')
02036 *        SET       (ADDRESS OF CONTROL-FILE)
02037 *        RIDFLD    (ELCNTL-KEY)
02038 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009926' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393236' TO DFHEIV0(25:11)
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
           
02039
02040      MOVE CF-STATE-ABBREVIATION  TO CP-STATE-STD-ABBRV.
CIDMOD     MOVE CM-RATE-CLASS          TO CP-CLASS-CODE
02041
080322     move zeros                  to ws-max-tot-ben
080322                                    ws-work-ben-pct
02042      IF CM-LF-CURRENT-STATUS = '6' OR '7' OR '8'
02043         MOVE ZEROS                 TO BENEO
02044      ELSE
02045         MOVE CP-REMAINING-TERM-3   TO CP-REMAINING-TERM
02046         MOVE CM-LF-BENEFIT-AMT     TO CP-ORIGINAL-BENEFIT
02047         MOVE CM-LF-ALT-BENEFIT-AMT TO CP-ALTERNATE-BENEFIT
02048         MOVE CM-LOAN-APR           TO CP-LOAN-APR
02049         MOVE CM-LOAN-TERM          TO CP-LOAN-TERM
02050         MOVE CM-PAY-FREQUENCY      TO CP-PAY-FREQUENCY
010816        if pi-company-id = 'DCC' or 'CAP'
061013           and am-dcc-product-code not = '   '
061013           move ' '              to ws-pdef-record-sw
061013           perform 3997-get-erpdef
061013                                 thru 3997-exit
061013           if pdef-found
080322              move cl-insured-birth-dt
080322                                 to dc-bin-date-1
080322              move cl-incurred-dt
080322                                 to dc-bin-date-2
080322              move '1'           to dc-option-code
080322              PERFORM 9700-LINK-DATE-CONVERT
080322                                 THRU 9700-EXIT
080322              compute ws-att-age =
080322                dc-elapsed-months / 12
080322              move zeros to dc-elapsed-months dc-elapsed-days
080322
061013              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
080322                 (P1 > +11)
061013                 OR (PD-PROD-CODE (P1) = 'L'
080322                   AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
061013              END-PERFORM
080322              IF P1 < +12
061013                 MOVE PD-MAX-AMT (P1)
080322                                 TO ws-MAX-TOT-BEN
080322                 if pd-ben-pct (p1) not numeric
080322                    move zeros   to pd-ben-pct (p1)
080322                 end-if
080322                 if pd-ben-pct (p1) = zeros
080322                    move +1      to ws-work-ben-pct
080322                 else
080322                    move pd-ben-pct (p1)
080322                                 to ws-work-ben-pct
080322                 end-if
061013              END-IF
061013           end-if
061013        end-if
02051         PERFORM 9850-LINK-REM-AMT THRU 9850-EXIT
02052         MOVE CP-REMAINING-AMT      TO BENEO
02053      end-if
080322     if ws-work-ben-pct <> zeros
080322        compute cp-remaining-amt =
080322           cp-remaining-amt * ws-work-ben-pct
080322     end-if
080322     if ws-max-tot-ben <> zeros
080322        if cp-remaining-amt > ws-max-tot-ben
080322           move ws-max-tot-ben   to cp-remaining-amt
080322        end-if
080322     end-if
080322     MOVE CP-REMAINING-AMT      TO BENEO
02054      IF PI-LIFE-OVERRIDE-L1 = 'P' OR
02055         WS-LF-COVERAGE-TYPE = 'P'
02056          COMPUTE WS-REMAINING-AMT = CM-LF-BENEFIT-AMT -
02057                                     CM-LF-ITD-DEATH-AMT
02058          MOVE WS-REMAINING-AMT    TO  BENEO.
02059
02060  1009-CONTINUE-BUILD.
02061
02062      MOVE CM-POLICY-FORM-NO  TO PI-SAVE-FORM.
02063
02064      IF WS-STATUS = '6' OR '7' OR '8'
02065         MOVE AL-SABOF              TO CRTSTATA.
02066
02067      IF WS-STATUS = '1' OR '4'
02068         IF CP-REMAINING-TERM-3 = ZEROS
02069            MOVE 'EXPIRED'        TO CRTSTATO
02070         ELSE
02071            MOVE 'ACTIVE  '       TO CRTSTATO.
02072
02073      IF WS-STATUS = '2'
02074          MOVE 'PEND    '         TO CRTSTATO.
02075
02076      IF WS-STATUS = '3'
02077          MOVE 'RESTORE '         TO CRTSTATO.
02078
02079      IF WS-STATUS = '5'
02080          MOVE 'REISSUE '         TO CRTSTATO.
02081
02082      IF WS-STATUS = '6'
100518        IF CM-LF-BENEFIT-CD = ZEROS  OR  SPACES
02083            MOVE 'LMP DIS'        TO CRTSTATO
100518        ELSE
100518           MOVE 'LMP BEN'        TO CRTSTATO.
02084
02085      IF WS-STATUS = '7'
02086          MOVE 'DEATH'            TO CRTSTATO.
02087
02088      IF WS-STATUS = '8'
02089          MOVE 'CANCEL'           TO CRTSTATO.
02090
02091      IF WS-STATUS = '9'
02092          MOVE 'RE-ONLY '         TO CRTSTATO.
02093
02094      IF WS-STATUS = 'D'
02095          MOVE 'DECLINE'          TO CRTSTATO.
02096
02097      IF WS-STATUS = 'V'
02098          MOVE 'VOID'             TO CRTSTATO.
02099
052614     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
080322                                          OR 'B' OR 'H'
02101         MOVE CM-AH-BENEFIT-CD    TO WS-BEN-CD
02102         MOVE '5'                 TO CNTL-REC-TYPE
           END-IF
02103
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
02105          MOVE CM-LF-BENEFIT-CD   TO WS-BEN-CD
02106          MOVE '4'                TO CNTL-REC-TYPE.
02107
02108      MOVE '** NONE **'           TO COVERO.
02109
02110      IF WS-BEN-CD = ZERO
02111          MOVE AL-SABOF           TO COVERA
02112          MOVE SPACES             TO CRTSTATO
02113          GO TO 1050-SHOW-CONTINUE.
02114
02115  1010-SHOW-CONTINUE.
02116      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02117      MOVE WS-ACCESS              TO CNTL-ACCESS.
02118      MOVE +0                     TO CNTL-SEQ-NO.
02119      MOVE 'CNTL'                 TO FILE-SWITCH.
02120
02121      
      * EXEC CICS HANDLE CONDITION
02122 *        ENDFILE   (1050-SHOW-CONTINUE)
02123 *        NOTFND    (1050-SHOW-CONTINUE)
02124 *    END-EXEC.
      *    MOVE '"$''I                  ! . #00010068' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303130303638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02125
02126      
      * EXEC CICS READ
02127 *        DATASET   ('ELCNTL')
02128 *        SET       (ADDRESS OF CONTROL-FILE)
02129 *        RIDFLD    (ELCNTL-KEY)
02130 *        GTEQ
02131 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00010073' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303733' TO DFHEIV0(25:11)
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
           
02132
02133      IF (CNTL-COMP-ID  NOT = CF-COMPANY-ID) OR
02134         (CNTL-REC-TYPE NOT = CF-RECORD-TYPE)
02135            GO TO 1050-SHOW-CONTINUE.
02136
02137      PERFORM 1030-BENEFIT-DUMMY THRU 1030-EXIT
02138          VARYING SUB-1 FROM 1 BY 1 UNTIL
02139             ((SUB-1 > 8) OR
02140             (CF-BENEFIT-CODE (SUB-1) = WS-BEN-CD)).
02141
02142      IF SUB-1 NOT = 9
02143          MOVE CF-BENEFIT-DESCRIP (SUB-1) TO COVERO
050107         MOVE CF-JOINT-INDICATOR (SUB-1) TO PI-JOINT-COV-IND
02144        ELSE
02145          IF CRTSTATO = 'ACTIVE'
02146              MOVE SPACES         TO  CRTSTATO.
02147
02148      GO TO 1050-SHOW-CONTINUE.
02149
02150  1030-BENEFIT-DUMMY.
02151  1030-EXIT.
02152      EXIT.
02153
02154  1050-SHOW-CONTINUE.
02155
02156      IF CL-PURGED-DT NOT = LOW-VALUES
02157          MOVE 'Y'                        TO  PI-PURGED-SW
02158          MOVE AL-SANOF                   TO  FILETOA    PRTOPTA
02159                                              FORMATA    ALTPRTA
02160                                              ACTCDA     ASSOCA
CIDMOD                                             CLOANA
02161          MOVE 'CLAIM IS PURGED - '       TO  WS-PURGED-MSG
02162          MOVE CL-PURGED-DT               TO  DC-BIN-DATE-1
02163          MOVE ' '                        TO  DC-OPTION-CODE
02164          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02165          IF NO-CONVERSION-ERROR
02166              MOVE DC-GREG-DATE-1-EDIT    TO  WS-PURGED-DATE
02167              MOVE WS-PURGED-MESSAGE      TO  MAP-DISPLAY (1)
02168          ELSE
02169              MOVE SPACES                 TO  WS-PURGED-DATE
02170              MOVE WS-PURGED-MESSAGE      TO  MAP-DISPLAY (1)
02171      ELSE
02172          MOVE 'N'                        TO  PI-PURGED-SW
02173          PERFORM 2000-BUILD-TRAILER-DISPLAY THRU 2999-EXIT.
02174
02175      IF PI-COMPANY-ID = 'DMD'
02176         MOVE COVERO                  TO  CRTSTATO
02177         IF CL-LAST-PMT-AMT NOT = ZERO
02178            MOVE CL-LAST-PMT-DT       TO  DC-BIN-DATE-1
02179            MOVE ' '                  TO  DC-OPTION-CODE
02180            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02181            MOVE DC-GREG-DATE-1-EDIT  TO  PRMTYPEO
02182            MOVE CL-LAST-PMT-AMT      TO  WS-PMT-AMT
02183            MOVE WS-PMT-AMTD          TO  COVERO
02184          ELSE
02185            MOVE SPACES               TO  COVERO
02186                                          PRMTYPEO.
02187
02188      IF RETURNED-FROM NOT = SPACES
02189          GO TO 8250-SEND-WITH-CURSOR.
02190
02191      IF CLAIM-IS-PURGED
082707        CONTINUE
082707*        MOVE -1                 TO  MCRFLML
02193      ELSE
02194          MOVE -1                 TO  FILETOL
082707     END-IF
02195
02196      IF PI-COMPANY-ID = 'DMD'
02197          IF CONFIDENTIAL-DATA
02198              IF PI-CONFIDENTIAL-UP
02199                  MOVE LOW-VALUE  TO PI-PRIORITY-IND
02200                  MOVE ER-0932    TO EMI-ERROR
02201                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02202              ELSE
02203                  MOVE 'C'        TO PI-PRIORITY-IND
02204                  
      * EXEC CICS SEND TEXT
02205 *                    FROM     (W-CONF-TEXT)
02206 *                    LENGTH   (W-CONF-LENGTH)
02207 *                    ERASE
02208 *                    FREEKB
02209 *                END-EXEC
      *    MOVE '8&      T  E F  H   F -   #00010155' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CONF-TEXT, 
                 W-CONF-LENGTH, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02210                  GO TO 9100-RETURN-TRAN.
02211
02212      GO TO 8100-SEND-INITIAL-MAP.
02213
02214  1100-SHOW-RECORD-NOT-FOUND.
02215
02216      IF FILE-SWITCH = 'MSTR'
02217          IF PI-CURRENT-FILE = 'ELRETR'
02218              MOVE ER-0204        TO EMI-ERROR
02219          ELSE
02220              MOVE 'Y'            TO PI-RETRIEVED-DATA-IND
02221              MOVE 'ELRETR'       TO PI-CURRENT-FILE
02222              GO TO 1000-READ-CLAIM
02223      ELSE
02224          MOVE ER-0206            TO EMI-ERROR.
02225
02226      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02227
030612     IF FILE-SWITCH = 'CERT' AND
062121      (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
02229         GO TO 1050-SHOW-CONTINUE.
02230
02231      MOVE -1                     TO CLMNOL.
02232
02233      MOVE PI-CARRIER             TO CARRO.
02234      MOVE PI-CLAIM-NO            TO CLMNOO.
02235      MOVE PI-CERT-PRIME          TO CERTNOO.
02236      MOVE PI-CERT-SFX            TO SUFXO.
02237
02238      MOVE AL-UABON               TO CLMNOA   CARRA
CIDMOD                                    YESNOSWA
02239                                     CERTNOA  SUFXA.
02240
02241      GO TO 8100-SEND-INITIAL-MAP.
02242      EJECT
02243  1200-GET-ASS-TOTALS.
02244
02245      MOVE CL-ASSOC-CERT-SEQU     TO PI-ASS-NDX.
02246
02247      IF RETURNED-FROM NOT = 'EL156' AND 'EL1501' AND 'EL1502'
02248          IF PI-ASS-PROCESSING
02249                  AND
02250             PI-ASS-CERTS (PI-ASS-NDX) = CL-CERT-NO
02251                  AND
02252             PI-PREV-PRIME-CERT-NO = CL-PRIME-CERT-NO
02253              GO TO 1200-SET-UP-FIELDS.
02254
02255      MOVE CL-ASSOC-CERT-SEQU      TO PI-LAST-SEQ-NO.
02256
02257      IF EIBAID = DFHPF15 OR DFHPF16
02258          
      * EXEC CICS ENDBR
02259 *            DATASET   (PI-CURRENT-FILE)
02260 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010211' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02261
02262      MOVE CLAIM-MASTER           TO W-CLAIM-MASTER-SAVE.
02263      MOVE ZEROS                  TO PI-DAYS-PAID
02264                                     PI-TOTAL-PAID.
02265      MOVE LOW-VALUES             TO PI-ASSOCIATED-CERTS-TABLE
02266                                     PI-LAST-PF-KEY-IND
02267                                     PI-ASSOCIATED-PROCESS-IND
02268      MOVE SPACES                 TO MSTR-CERT-NO.
02269
02270      IF RETURNED-FROM = 'EL156'
02271              AND
02272         PI-PREV-PRIME-CERT-NO = CL-PRIME-CERT-NO
02273          MOVE SPACES             TO RETURNED-FROM
02274      ELSE
02275          MOVE SPACES             TO RETURNED-FROM
02276          MOVE CL-CONTROL-PRIMARY TO PI-ORIGINAL-ASS-CERT.
02277
02278      
      * EXEC CICS HANDLE CONDITION
02279 *        NOTFND   (1220-NO-CLAIM-RECORD-FND)
02280 *        ENDFILE  (1200-ENDBR)
02281 *    END-EXEC.
      *    MOVE '"$I''                  ! / #00010231' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303130323331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02282
02283      
      * EXEC CICS STARTBR
02284 *         DATASET   (PI-CURRENT-FILE)
02285 *         RIDFLD    (ELMSTR-KEY)
02286 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010236' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02287
02288  1200-GET-NEXT-RECORD.
02289
02290      
      * EXEC CICS READNEXT
02291 *         DATASET   (PI-CURRENT-FILE)
02292 *         RIDFLD    (ELMSTR-KEY)
02293 *         INTO      (CLAIM-MASTER)
02294 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00010243' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02295
02296      IF PI-PREV-COMP-CD  NOT = CL-COMPANY-CD  OR
02297         PI-PREV-CARRIER  NOT = CL-CARRIER     OR
02298         PI-PREV-CLAIM-NO NOT = CL-CLAIM-NO
02299          GO TO 1200-ENDBR.
02300
02301      IF CL-PRIME-CERT-NO NOT = PI-PREV-PRIME-CERT-NO
02302          GO TO 1200-GET-NEXT-RECORD.
02303
02304      ADD CL-TOTAL-PAID-AMT       TO PI-TOTAL-PAID.
02305      ADD CL-NO-OF-DAYS-PAID      TO PI-DAYS-PAID.
02306
02307      MOVE CL-ASSOC-CERT-SEQU     TO PI-ASS-NDX.
02308      MOVE CL-CERT-NO             TO PI-ASS-CERTS (PI-ASS-NDX).
02309
02310      GO TO 1200-GET-NEXT-RECORD.
02311
02312  1200-ENDBR.
02313
02314      
      * EXEC CICS ENDBR
02315 *        DATASET   (PI-CURRENT-FILE)
02316 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010267' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02317
02318      MOVE W-CLAIM-MASTER-SAVE    TO CLAIM-MASTER.
02319      MOVE PI-LAST-SEQ-NO         TO PI-ASS-NDX.
02320
02321  1200-SET-UP-FIELDS.
02322
02323      MOVE 'Y'                    TO PI-ASSOCIATED-PROCESS-IND.
02324      MOVE PI-DAYS-PAID           TO W-DAYS-PAID-X.
02325      MOVE PI-TOTAL-PAID          TO W-TOTAL-PAID-AMT-X.
02326      MOVE W-TOTAL-PAID-AMT-X     TO AMTPDO.
02327      MOVE W-DAYS-PAID-X          TO DAYSPDO.
02328      MOVE 'CLAIM TOTALS...AMOUNT PD:'
02329                                  TO CLMTOTHO.
02330      MOVE 'DAYS PAID:'           TO DAYSPDHO.
02331
02332  1200-EXIT.
02333      EXIT.
02334
02335  1220-NO-CLAIM-RECORD-FND.
02336
02337      MOVE ER-0204                TO EMI-ERROR.
02338
02339      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02340
030612     IF FILE-SWITCH = 'CERT' AND
062121       (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
02342          GO TO 1050-SHOW-CONTINUE.
02343
02344      MOVE -1                     TO CLMNOL.
02345
02346      MOVE PI-CARRIER             TO CARRO.
02347      MOVE PI-CLAIM-NO            TO CLMNOO.
02348      MOVE PI-CERT-PRIME          TO CERTNOO.
02349      MOVE PI-CERT-SFX            TO SUFXO.
02350
02351      MOVE AL-UABON               TO CLMNOA   CARRA
02352                                     CERTNOA  SUFXA.
02353
02354      GO TO 8100-SEND-INITIAL-MAP.
02355      EJECT
02356  1500-READ-CONVENIENCE-FILES.
02357
02358      
      * EXEC CICS HANDLE CONDITION
02359 *        NOTFND   (1500-SHOW-RECORD-NOT-FOUND)
02360 *    END-EXEC.
      *    MOVE '"$I                   ! 0 #00010312' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303130333132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02361
02362      MOVE PI-COMPANY-CD              TO  PLCY-COMP-CD.
02363      MOVE CL-CERT-CARRIER            TO  PLCY-CARRIER.
02364      MOVE CL-CERT-GROUPING           TO  PLCY-GROUPING.
02365      MOVE CL-CERT-STATE              TO  PLCY-STATE.
02366      MOVE CL-CERT-ACCOUNT            TO  PLCY-PRODUCER.
02367      MOVE CL-CERT-EFF-DT             TO  PLCY-EFF-DT.
02368      MOVE CL-CV-REFERENCE-NO         TO  PLCY-REFERENCE-NO.
02369      MOVE 'PLCY'                     TO  FILE-SWITCH.
02370
02371      
      * EXEC CICS READ
02372 *        DATASET   ('MPPLCY')
02373 *        SET       (ADDRESS OF POLICY-MASTER)
02374 *        RIDFLD    (EMPLCY-KEY)
02375 *    END-EXEC.
           MOVE 'MPPLCY' TO DFHEIV1
      *    MOVE '&"S        E          (   #00010325' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLCY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02376
02377      MOVE 'Y'                        TO  WS-CERT-READ-SW.
02378
02379      PERFORM 1700-READ-EMPHST THRU 1700-EXIT.
02380
02381  1500-READ-EMPLAN.
02382
02383      MOVE PM-COMPANY-CD              TO  PLAN-COMP-CD.
02384      MOVE PM-CARRIER                 TO  PLAN-CARRIER.
02385      MOVE PM-GROUPING                TO  PLAN-GROUPING.
02386      MOVE PM-STATE                   TO  PLAN-STATE.
02387      MOVE PM-PRODUCER                TO  PLAN-PRODUCER.
02388      MOVE PM-INS-PLAN-CD             TO  PLAN-PLAN-CODE.
02389      MOVE PM-INS-PLAN-REVISION       TO  PLAN-REV-NO.
02390      MOVE 'PLAN'                     TO  FILE-SWITCH.
02391
02392      
      * EXEC CICS HANDLE CONDITION
02393 *        NOTFND   (1500-NO-PLAN-RECORD)
02394 *    END-EXEC.
      *    MOVE '"$I                   ! 1 #00010346' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303130333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02395
02396      
      * EXEC CICS READ
02397 *        DATASET   ('MPPLAN')
02398 *        RIDFLD    (EMPLAN-KEY)
02399 *        SET       (ADDRESS OF PRODUCER-PLANS)
02400 *    END-EXEC.
           MOVE 'MPPLAN' TO DFHEIV1
      *    MOVE '&"S        E          (   #00010350' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-PLANS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02401
02402      MOVE PP-PLAN-ABBREV             TO  COVERO.
02403      MOVE PP-REFUND-CALC             TO  CP-EARNING-METHOD
02404                                          CP-RATING-METHOD.
02405      IF PP-BENEFIT-IS-LEVEL
02406          MOVE 'L'                    TO  CP-BENEFIT-TYPE
02407      ELSE
02408          MOVE 'R'                    TO  CP-BENEFIT-TYPE.
02409
02410      MOVE PM-LOAN-TERM               TO  CP-ORIGINAL-TERM
02411                                          CP-LOAN-TERM.
02412      MOVE SAVE-BIN-DATE              TO  CP-VALUATION-DT.
02413      MOVE PM-POLICY-EFF-DT           TO  CP-CERT-EFF-DT.
02414      MOVE PM-STATE                   TO  CP-STATE.
02415      MOVE 'A'                        TO  CP-SPECIAL-CALC-CD.
02416      MOVE '2'                        TO  CP-PROCESS-TYPE.
02417      MOVE PI-COMPANY-ID              TO  CP-COMPANY-ID.
02418      MOVE PI-COMPANY-CD              TO  CP-COMPANY-CD.
02419      MOVE '1'                        TO  CP-REM-TRM-CALC-OPTION.
02420
02421      IF PM-AH-MORT-PLAN
02422          MOVE '3'                    TO  CP-REM-TERM-METHOD
02423          MOVE PM-LOAN-DT             TO  CP-FIRST-PAY-DATE
02424      ELSE
02425          MOVE '2'                    TO  CP-REM-TERM-METHOD
02426          MOVE PM-POLICY-EFF-DT       TO  CP-FIRST-PAY-DATE.
02427
02428      PERFORM 8900-GET-FREE-LOOK THRU 8900-EXIT.
02429      PERFORM 9800-LINK-REM-TERM.
02430
02431      IF PM-AH-MORT-PLAN
02432        MOVE CP-REMAINING-TERM-1    TO  WST-REM
02433      ELSE
02434        IF PI-COMPANY-ID = 'CIG' OR 'CUK'
02435          COMPUTE CP-REMAINING-TERM-3 = CP-REMAINING-TERM-3 + 1
02436          MOVE CP-REMAINING-TERM-3    TO  WST-REM
02437        ELSE
02438          MOVE CP-REMAINING-TERM-3    TO  WST-REM.
02439
02440      GO TO 1500-SHOW-CONTINUE.
02441
02442  1500-NO-PLAN-RECORD.
02443
02444      MOVE ZEROS                      TO  WST-REM.
02445      MOVE '** NONE **'               TO  COVERO.
02446
02447  1500-SHOW-CONTINUE.
02448
02449      MOVE PM-LOAN-TERM               TO  WST-ORIG.
02450      MOVE WS-TERMS                   TO  TERMSO.
02451
02452      IF PM-AH-MORT-PLAN
02453          MOVE 'MO. BENEFIT :'        TO  BENECAPO
02454          MOVE PM-INS-MONTH-BENEFIT   TO  BENEO
02455      ELSE
02456          MOVE PM-INS-TOTAL-BENEFIT   TO  BENEO.
02457
02458      IF PM-ANNUAL
02459          MOVE 'ANNUAL'              TO  PRMTYPEO
02460        ELSE
02461      IF PM-SEMI-ANNUAL
02462          MOVE 'SEMI ANL'            TO  PRMTYPEO
02463        ELSE
02464      IF PM-QUARTERLY
02465          MOVE 'QTRLY'               TO  PRMTYPEO
02466        ELSE
02467      IF PM-BI-MONTHLY
02468          MOVE 'BI MONTH'            TO  PRMTYPEO
02469        ELSE
02470      IF PM-SINGLE-PREM
02471          MOVE 'SING PRM'            TO  PRMTYPEO.
02472
02473      MOVE PM-POLICY-EFF-DT           TO  DC-BIN-DATE-1.
02474      MOVE '6'                        TO  DC-OPTION-CODE.
02475      MOVE PM-LOAN-TERM               TO  DC-ELAPSED-MONTHS.
02476      MOVE +0                         TO  DC-ELAPSED-DAYS.
02477      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02478      IF NO-CONVERSION-ERROR
02479          MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
02480      ELSE
02481          MOVE SPACES                 TO  EXPIREO.
02482
02483      IF PM-CURRENT-STATUS = '0'
02484          MOVE 'LAPSED'               TO  CRTSTATO.
02485      IF PM-CURRENT-STATUS = '1'
02486          MOVE 'ACTIVE'               TO  CRTSTATO.
02487      IF PM-CURRENT-STATUS = '2'
02488          MOVE 'PENDING'              TO  CRTSTATO.
02489      IF PM-CURRENT-STATUS = '3'
02490          MOVE 'DECLINE'              TO  CRTSTATO.
02491      IF PM-CURRENT-STATUS = '4' OR '9'
02492          MOVE 'PND CANC'             TO  CRTSTATO.
02493      IF PM-CURRENT-STATUS = '5'
02494          MOVE 'PND ISS'              TO  CRTSTATO.
02495      IF PM-CURRENT-STATUS = '6'
02496          MOVE 'CLAIM'                TO  CRTSTATO.
02497      IF PM-CURRENT-STATUS = '7'
02498          MOVE 'CANCEL'               TO  CRTSTATO.
02499      IF PM-CURRENT-STATUS = '8'
02500          MOVE 'PND UNDW'             TO  CRTSTATO.
02501      IF PM-CURRENT-STATUS = 'C'
02502          MOVE 'TRANSFER'             TO  CRTSTATO.
02503      IF PM-CURRENT-STATUS = 'F'
02504          MOVE 'SETTLE'               TO  CRTSTATO.
02505      IF PM-CURRENT-STATUS = 'T'
02506          MOVE 'TRMNAT'               TO  CRTSTATO.
02507
02508      GO TO 1500-EXIT.
02509
02510  1500-SHOW-RECORD-NOT-FOUND.
02511
02512      IF FILE-SWITCH = 'PLCY'
02513          MOVE ER-9483            TO  EMI-ERROR.
02514
02515      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02516      MOVE -1                     TO  CLMNOL.
02517      MOVE PI-CARRIER             TO  CARRO.
02518      MOVE PI-CLAIM-NO            TO  CLMNOL.
02519      MOVE PI-CERT-PRIME          TO  CERTNOO.
02520      MOVE PI-CERT-SFX            TO  SUFXO.
02521
02522      MOVE AL-UABON               TO  CLMNOA    CARRA
02523                                      CERTNOA   SUFXA.
02524      GO TO 8100-SEND-INITIAL-MAP.
02525
02526  1500-EXIT.
02527      EXIT.
02528
02529      EJECT
02530  1600-ROLL-TRAILERS.
02531      MOVE SPACES                 TO MAP-DISPLAY (1)
02532                                     MAP-DISPLAY (2)
02533                                     MAP-DISPLAY (3)
02534                                     MAP-DISPLAY (4).
02535
02536      PERFORM 2000-BUILD-TRAILER-DISPLAY THRU 2999-EXIT.
02537
02538      MOVE -1                     TO FILETOL.
02539      GO TO 8200-SEND-DATAONLY.
02540
02541      EJECT
02542 ***************************************************************
02543 *    I/O REQUESTS AGAINST THE CONVENIENCE POLICY HISTORY FILE *
02544 ***************************************************************
02545  1700-READ-EMPHST.
02546
02547      
      * EXEC CICS HANDLE CONDITION
02548 *        ENDFILE  (1700-ENDBROWSE)
02549 *        NOTFND   (1700-ENDBROWSE)
02550 *    END-EXEC.
      *    MOVE '"$''I                  ! 2 #00010501' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303130353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02551
02552      MOVE PM-COMPANY-CD          TO  EMPHST-COMPANY-CD.
02553      MOVE PM-CARRIER             TO  EMPHST-CARRIER.
02554      MOVE PM-GROUPING            TO  EMPHST-GROUPING.
02555      MOVE PM-STATE               TO  EMPHST-STATE.
02556      MOVE PM-PRODUCER            TO  EMPHST-PRODUCER.
02557      MOVE PM-POLICY-EFF-DT       TO  EMPHST-POLICY-EFF-DT.
02558      MOVE PM-REFERENCE-NUMBER    TO  EMPHST-REFERENCE-NUMBER.
02559      MOVE ZEROS                  TO  EMPHST-SEQUENCE-NO.
02560
02561      IF PM-LIFE-MORT-PLAN
02562          MOVE '02'                   TO  EMPHST-RECORD-TYPE
02563          MOVE '21'                   TO  EMPHST-FIELD-TYPE
02564      ELSE
02565          MOVE '04'                   TO  EMPHST-RECORD-TYPE
02566          MOVE '07'                   TO  EMPHST-FIELD-TYPE.
02567
02568      
      * EXEC CICS STARTBR
02569 *        DATASET   (EMPHST-FILE-ID)
02570 *        RIDFLD    (EMPHST-KEY)
02571 *        GTEQ
02572 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010522' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPHST-FILE-ID, 
                 EMPHST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02573
02574      MOVE 'Y'                    TO  WS-BROWSE-START-SW.
02575
02576  1700-READ-NEXT.
02577
02578      
      * EXEC CICS READNEXT
02579 *        DATASET   (EMPHST-FILE-ID)
02580 *        SET       (ADDRESS OF POLICY-HISTORY)
02581 *        RIDFLD    (EMPHST-KEY)
02582 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00010532' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPHST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPHST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-HISTORY TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02583
02584      IF PM-COMPANY-CD       = PH-COMPANY-CD         AND
02585         PM-CARRIER          = PH-CARRIER            AND
02586         PM-GROUPING         = EMPHST-GROUPING       AND
02587         PM-STATE            = EMPHST-STATE          AND
02588         PM-PRODUCER         = EMPHST-PRODUCER       AND
02589         PM-POLICY-EFF-DT    = EMPHST-POLICY-EFF-DT  AND
02590         PM-REFERENCE-NUMBER = EMPHST-REFERENCE-NUMBER
02591          NEXT SENTENCE
02592      ELSE
02593          GO TO 1700-ENDBROWSE.
02594
02595      IF PM-LIFE-MORT-PLAN
02596          IF PH-RECORD-TYPE = '02' AND
02597             PH-FIELD-TYPE  = '21'
02598              NEXT SENTENCE
02599          ELSE
02600              GO TO 1700-ENDBROWSE.
02601
02602      IF NOT PM-LIFE-MORT-PLAN
02603          IF PH-RECORD-TYPE = '04' AND
02604             PH-FIELD-TYPE  = '07'
02605              NEXT SENTENCE
02606          ELSE
02607              GO TO 1700-ENDBROWSE.
02608
02609      IF PH-CHANGE-DT > CL-INCURRED-DT
02610          GO TO 1700-READ-NEXT.
02611
02612      MOVE PH-RECORD-BODY         TO  WS-HISTORY-AREA.
02613
02614      
      * EXEC CICS BIF
02615 *        DEEDIT
02616 *        FIELD (WS-HISTORY-AREA)
02617 *        LENGTH ('50')
02618 *    END-EXEC.
           MOVE '50'
             TO DFHEIV11
      *    MOVE '@"L                   #   #00010568' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-HISTORY-AREA, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02619
02620      IF PM-LIFE-MORT-PLAN
02621          MOVE WS-NUMERIC-FLD1    TO  PM-INSURED-TOT-BENEFIT
02622      ELSE
02623          MOVE WS-NUMERIC-FLD1    TO  PM-INS-MONTH-BENEFIT.
02624
02625  1700-ENDBROWSE.
02626
02627      
      * EXEC CICS ENDBR
02628 *        DATASET   (EMPHST-FILE-ID)
02629 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010581' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPHST-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02630
02631  1700-EXIT.
02632       EXIT.
02633
02634      EJECT
02635  2000-BUILD-TRAILER-DISPLAY.
02636
02637      IF DIRECTION-SWITCH = 'F'
02638         IF PI-PREV-DIR = 'B' AND PI-PREV-DISP = 'N'
02639            MOVE PI-SAVE-LOW    TO TRLR-SEQ-NO
02640         ELSE
02641            MOVE PI-SAVE-HIGH   TO TRLR-SEQ-NO
02642            ADD +1              TO TRLR-SEQ-NO
02643      ELSE
02644         IF DIRECTION-SWITCH = 'B'
02645            IF PI-PREV-DIR = 'F' AND PI-PREV-DISP = 'N'
02646               MOVE PI-SAVE-HIGH TO TRLR-SEQ-NO
02647            ELSE
02648               MOVE PI-SAVE-LOW TO TRLR-SEQ-NO
02649         ELSE
02650            IF DIRECTION-SWITCH = 'R'
02651               MOVE PI-SAVE-LOW TO TRLR-SEQ-NO
02652               MOVE 'F'        TO DIRECTION-SWITCH
02653            ELSE
02654               IF PI-COMPANY-ID = 'AIG' OR 'AUK'
02655                   MOVE +95     TO  TRLR-SEQ-NO
02656                   MOVE +0      TO  PI-SAVE-LOW
02657                                    PI-SAVE-HIGH
02658                   MOVE 'N'     TO  PI-PREV-DISP
02659                   MOVE 'F'     TO  DIRECTION-SWITCH
02660               ELSE
02661                   MOVE +93     TO  TRLR-SEQ-NO
02662                   MOVE +0      TO  PI-SAVE-LOW
02663                                    PI-SAVE-HIGH
02664                   MOVE 'N'     TO  PI-PREV-DISP
02665                   MOVE 'F'     TO  DIRECTION-SWITCH.
02666
02667      IF DIRECTION-SWITCH = 'F'
02668          MOVE +1                 TO DISPLAY-CNT
02669      ELSE
02670          MOVE +4                 TO DISPLAY-CNT.
02671
02672      MOVE PI-COMPANY-CD          TO TRLR-COMP-CD.
02673      MOVE PI-CARRIER             TO TRLR-CARRIER.
02674      MOVE PI-CLAIM-NO            TO TRLR-CLAIM-NO.
02675      MOVE PI-CERT-NO             TO TRLR-CERT-NO.
02676      MOVE 'TRLR'                 TO FILE-SWITCH.
02677
02678      IF PI-SAVE-SYS-ID = 'CV'
02679          NEXT SENTENCE
02680      ELSE
02681          GO TO 2005-BUILD-CR-LOAN-DATA.
02682
02683      IF WS-CERT-READ-SW = 'Y'
02684        IF PI-COMPANY-ID = 'CIG' OR 'CUK'
02685          IF CL-SYSTEM-IDENTIFIER = 'CV'
02686            MOVE PM-INS-TERMINATION-DT    TO  DC-BIN-DATE-1
02687            MOVE ' '                      TO  DC-OPTION-CODE
02688            MOVE +0                       TO  DC-ELAPSED-MONTHS
02689                                              DC-ELAPSED-DAYS
02690            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02691            IF NO-CONVERSION-ERROR
02692                MOVE DC-GREG-DATE-1-EDIT  TO  WS-LOAN-EXPIRE-DT
02693            ELSE
02694                MOVE SPACES               TO  WS-LOAN-EXPIRE-DT.
02695
02696      IF WS-CERT-READ-SW = 'Y'
02697        IF PI-COMPANY-ID = 'CIG' OR 'CUK'
02698          MOVE SPACES                 TO  TRAILER-DISPLAY-WORK-AREA
02699          MOVE 'LOAN INFO'            TO  DISPLAY-ACTION
02700          MOVE ZEROS                  TO  DISPLAY-SEQ
02701          MOVE WS-LOAN-EXPIRE-DT      TO  DISPLAY-DATE
02702          MOVE PM-LOAN-NUMBER         TO  ASSOC-CV-LOAN
02703          MOVE ASSOC-CV-LOAN-TEXT     TO  DISPLAY-TEXT
02704          MOVE TRAILER-DISPLAY-WORK-AREA
02705                                      TO  MAP-DISPLAY (DISPLAY-CNT)
02706          ADD +1                      TO  DISPLAY-CNT.
02707
02708      GO TO 2010-CONTINUE-TRAILER-DISPLAY.
02709
02710  2005-BUILD-CR-LOAN-DATA.
02711
02712      IF WS-CERT-READ-SW = 'Y'
02713         IF PI-COMPANY-ID = 'AIG' OR 'AUK'
02714           IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
02715             MOVE CM-LF-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1
02716             MOVE ' '                     TO  DC-OPTION-CODE
02717             MOVE +0                      TO  DC-ELAPSED-MONTHS
02718                                              DC-ELAPSED-DAYS
02719             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02720             IF NO-CONVERSION-ERROR
02721               MOVE DC-GREG-DATE-1-EDIT   TO  WS-LOAN-EXPIRE-DT
02722             ELSE
02723               MOVE SPACES                TO  WS-LOAN-EXPIRE-DT
02724           ELSE
02725             MOVE CM-AH-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1
02726             MOVE ' '                     TO  DC-OPTION-CODE
02727             MOVE +0                      TO  DC-ELAPSED-MONTHS
02728                                              DC-ELAPSED-DAYS
02729             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02730             IF NO-CONVERSION-ERROR
02731               MOVE DC-GREG-DATE-1-EDIT   TO  WS-LOAN-EXPIRE-DT
02732             ELSE
02733               MOVE SPACES                TO  WS-LOAN-EXPIRE-DT.
02734
02735      IF WS-CERT-READ-SW = 'Y'
02736        IF PI-COMPANY-ID = 'AIG' OR 'AUK'
02737          MOVE SPACES             TO  TRAILER-DISPLAY-WORK-AREA
02738          MOVE ZEROS              TO  DISPLAY-SEQ
02739          MOVE 'LOAN DATA'        TO  DISPLAY-ACTION
02740          MOVE WS-LOAN-EXPIRE-DT  TO  DISPLAY-DATE
02741          MOVE CM-LOAN-NUMBER     TO  ASSOC-ORIG-LOAN
02742          MOVE CM-MEMBER-NO       TO  ASSOC-CUR-LOAN
02743          MOVE CL-LOAN-TYPE       TO  ASSOC-LOAN-TYPE
02744          MOVE ASSOC-LOAN-TEXT    TO  DISPLAY-TEXT
02745          MOVE TRAILER-DISPLAY-WORK-AREA
02746                                  TO  MAP-DISPLAY (DISPLAY-CNT)
02747          ADD +1                  TO  DISPLAY-CNT
02748        ELSE
02749          IF CM-OPEN-END
02750            MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
02751            MOVE 'LOAN INFO'      TO DISPLAY-ACTION
02752            MOVE CM-LOAN-NUMBER   TO LOAN-NUMBER
02753            MOVE CM-LOAN-BALANCE  TO LOAN-BALANCE
02754            MOVE CM-MEMBER-NO     TO LOAN-MEMBER
02755            MOVE LOAN-TEXT        TO DISPLAY-TEXT
02756            MOVE TRAILER-DISPLAY-WORK-AREA
02757                                  TO MAP-DISPLAY (DISPLAY-CNT)
02758            ADD +1                TO DISPLAY-CNT.
02759
010816     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'CAP'
062121           OR 'FNL'
              PERFORM 6150-READ-TRLR92 THRU 6150-EXIT
              if ws-resp-normal
062602           MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
03087            MOVE 'SPECIAL REVIEW' TO DISPLAY-ACTION
062602           move at-recorded-by   to display-by
                 move at-recorded-dt   to dc-bin-date-1
03091            MOVE ' '              TO DC-OPTION-CODE
03092            PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
03093            MOVE DC-GREG-DATE-1-EDIT
                                       TO DISPLAY-DATE
03095            MOVE ws-TRLR-SEQ-NO   TO DISPLAY-SEQ
03096            MOVE AT-INFO-LINE-1   TO DISPLAY-TEXT
03097            MOVE TRAILER-DISPLAY-WORK-AREA
                                       TO MAP-DISPLAY (1)
                 if direction-switch = 'F'
02747               ADD +1             TO  DISPLAY-CNT
                 end-if
              end-if
061511        PERFORM 6160-READ-TRLR93 THRU 6160-EXIT
061511        if ws-resp-normal
061511           MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
061511           MOVE 'VERIFY SSN    ' TO DISPLAY-ACTION
061511           move at-recorded-by   to display-by
061511           move at-recorded-dt   to dc-bin-date-1
061511           MOVE ' '              TO DC-OPTION-CODE
061511           PERFORM 9700-LINK-DATE-CONVERT
061511                                 THRU 9700-EXIT
061511           MOVE DC-GREG-DATE-1-EDIT
061511                                 TO DISPLAY-DATE
061511           MOVE ws-TRLR-SEQ-NO   TO DISPLAY-SEQ
061511           MOVE AT-INFO-LINE-1   TO DISPLAY-TEXT
061511           IF MAP-DISPLAY (1) (1:14) = 'SPECIAL REVIEW'
061511               MOVE TRAILER-DISPLAY-WORK-AREA
061511                                 TO MAP-DISPLAY (2)
061511           ELSE
061511               MOVE TRAILER-DISPLAY-WORK-AREA
061511                                 TO MAP-DISPLAY (1)
061511           END-IF
061511           if direction-switch = 'F'
061511              ADD +1             TO  DISPLAY-CNT
061511           end-if
061511        end-if
020613        PERFORM 6170-READ-TRLR94 THRU 6170-EXIT
020613        if ws-resp-normal
020613           MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
020613           MOVE 'CAUSAL STATE  ' TO DISPLAY-ACTION
020613           MOVE AT-RECORDED-BY   TO DISPLAY-BY
020613           MOVE AT-RECORDED-DT   TO DC-BIN-DATE-1
020613           MOVE ' '              TO DC-OPTION-CODE
020613           PERFORM 9700-LINK-DATE-CONVERT
020613                                 THRU 9700-EXIT
020613           MOVE DC-GREG-DATE-1-EDIT
020613                                 TO DISPLAY-DATE
020613           MOVE WS-TRLR-SEQ-NO   TO DISPLAY-SEQ
020613           MOVE AT-INFO-LINE-1   TO DISPLAY-TEXT
020613           IF MAP-DISPLAY (1) (1:14) = 'SPECIAL REVIEW'
020613            OR 'VERIFY SSN    '
020613               IF MAP-DISPLAY (2) (1:14) = 'VERIFY SSN    '
020613                   MOVE TRAILER-DISPLAY-WORK-AREA
020613                                 TO MAP-DISPLAY (3)
020613               ELSE
020613                  MOVE TRAILER-DISPLAY-WORK-AREA
020613                                 TO MAP-DISPLAY (2)
020613               END-IF
020613           ELSE
020613               MOVE TRAILER-DISPLAY-WORK-AREA
020613                                 TO MAP-DISPLAY (1)
020613           END-IF
020613           IF DIRECTION-SWITCH = 'F'
020613              ADD +1             TO  DISPLAY-CNT
020613           END-IF
020613        END-IF
           end-if
010816     IF PI-COMPANY-ID = 'DCC' or 'CAP'
061013        PERFORM 6180-READ-TRLR95 THRU 6180-EXIT
061013        if ws-resp-normal
061013           move error-message-interface-block
061013                                 to ws-save-error-interface-block
061013           MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
061013           MOVE 'NOTE'           TO DISPLAY-ACTION
061013           move at-recorded-by   to display-by
061013           move at-recorded-dt   to dc-bin-date-1
061013           MOVE ' '              TO DC-OPTION-CODE
061013           PERFORM 9700-LINK-DATE-CONVERT
061013                                 THRU 9700-EXIT
061013           MOVE DC-GREG-DATE-1-EDIT
061013                                 TO DISPLAY-DATE
061013           MOVE ws-TRLR-SEQ-NO   TO DISPLAY-SEQ
061013           perform varying a1 from +1 by +1 until
061013              at-note-error-no (a1) = spaces
061013              or display-cnt > +4
061013              move at-note-error-no (a1)
061013                                 to emi-error
061013              if at-note-error-no (a1) = '1653'
061013                 evaluate true
061013                    when pi-save-type = 'L'
061013                       move '  LF  '
061013                              to emi-claim-type
061013                    when pi-save-type = 'I'
061013                       move '  IU  '
061013                              to emi-claim-type
052614                    when pi-save-type = 'F'
052614                       move '  FL  '
052614                              to emi-claim-type
080322                    WHEN pi-save-type = 'B'
080322                       MOVE ' BR  '         TO emi-claim-type
080322                    WHEN pi-save-type = 'H'
080322                       MOVE ' HS '          TO emi-claim-type
080322
100518                    when pi-save-type = 'O'
100518                       move '  OT  '
100518                              to emi-claim-type
061013                    when other
061013                       move '  AH  '
061013                              to emi-claim-type
061013                 end-evaluate
061013              end-if
061013              PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013              move emi-line1 (8:64)
061013                             to TRAILER-DISPLAY-WORK-AREA (16:64)
061013              move trailer-display-work-area
061013                                 to map-display (display-cnt)
061013              if direction-switch = 'F'
061013                 ADD +1          TO  DISPLAY-CNT
061013              end-if
061013              move ws-save-error-interface-block
061013                                 to error-message-interface-block
061013           end-perform
061013        end-if
061013     end-if
           .
02760  2010-CONTINUE-TRAILER-DISPLAY.
02761
02762      
      * EXEC CICS HANDLE CONDITION
02763 *        ENDFILE   (2950-NO-MORE-TRAILERS)
02764 *        NOTFND    (2950-NO-MORE-TRAILERS)
02765 *    END-EXEC.
      *    MOVE '"$''I                  ! 3 #00010851' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303130383531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02766
02767      
      * EXEC CICS STARTBR
02768 *        DATASET   ('ELTRLR')
02769 *        GTEQ
02770 *        RIDFLD    (ELTRLR-KEY)
02771 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010856' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130383536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02772
02773      IF DIRECTION-SWITCH = 'B'
02774          IF (PI-PREV-DISP = 'Y') OR (PI-PREV-DIR = 'B')
02775              
      * EXEC CICS READPREV
02776 *                DATASET ('ELTRLR')
02777 *                RIDFLD  (ELTRLR-KEY)
02778 *                SET     (ADDRESS OF ACTIVITY-TRAILERS)
02779 *            END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00010864' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130383634' TO DFHEIV0(25:11)
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
           
02780
02781      MOVE 'N'                    TO PI-PREV-DISP.
02782
02783      MOVE DIRECTION-SWITCH       TO PI-PREV-DIR.
02784
02785  2020-READ-TRAILER-LOOP.
02786      IF DIRECTION-SWITCH = 'F'
02787          
      * EXEC CICS READNEXT
02788 *            DATASET   ('ELTRLR')
02789 *            SET       (ADDRESS OF ACTIVITY-TRAILERS)
02790 *            RIDFLD    (ELTRLR-KEY)
02791 *        END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00010876' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130383736' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02792      ELSE
02793          
      * EXEC CICS READPREV
02794 *            DATASET   ('ELTRLR')
02795 *            SET       (ADDRESS OF ACTIVITY-TRAILERS)
02796 *            RIDFLD    (ELTRLR-KEY)
02797 *        END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00010882' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130383832' TO DFHEIV0(25:11)
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
           
02798
02799      IF (PI-COMPANY-CD NOT = TRLR-COMP-CD)  OR
02800         (PI-CARRIER    NOT = TRLR-CARRIER)  OR
02801         (PI-CLAIM-NO   NOT = TRLR-CLAIM-NO) OR
02802         (PI-CERT-NO    NOT = TRLR-CERT-NO)
02803             GO TO 2950-NO-MORE-TRAILERS.
02804
062602*    IF TRLR-SEQ-NO = 90
061511     IF TRLR-SEQ-NO = 90 or 91 or 92 or 93
020613       OR 94 or 95
02806          GO TO 2020-READ-TRAILER-LOOP.
02807
02808      IF PI-COMPANY-ID = 'LAP'
02809          IF TRLR-SEQ-NO > +100 AND < +301
02810              GO TO 2020-READ-TRAILER-LOOP.
02811
02812      IF RESERVE-EXPENSE-TR OR ADDRESS-TR
02813          GO TO 2020-READ-TRAILER-LOOP.
02814
02815      IF PAYMENT-TR
02816          GO TO 2100-PAYMENT-TRAILER.
02817
02818      IF AUTO-PAY-TR
02819         GO TO 2200-AUTO-PAYMENT-TRAILER.
02820
02821      IF CORRESPONDENCE-TR
02822          GO TO 2300-CORRESPONDENCE-TRAILER.
02823
02824      IF GENERAL-INFO-TR
02825          GO TO 2400-GENERAL-INFO-TRAILER.
02826
02827      IF AUTO-PROMPT-TR
02828          GO TO 2500-AUTO-PROMPT-TRAILER.
02829
02830      IF DENIAL-TR
02831          GO TO 2600-DENIAL-TRAILER.
02832
02833      IF INCURRED-CHG-TR
02834          GO TO 2700-INCURRED-CHANGE-TRAILER.
02835
02836      IF FORM-CONTROL-TR
02837          GO TO 2710-FORM-CONTROL-TRAILER.
02838
02839      GO TO 2020-READ-TRAILER-LOOP.
02840
02841      EJECT
02842  2100-PAYMENT-TRAILER.
02843
02844      IF AT-PAYMENT-NOTE
02845          GO TO 2020-READ-TRAILER-LOOP.
02846
02847      MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
02848
02849      IF AT-PAYMENT-TYPE = 'T'
02850          MOVE 'TRANSFER PMT'     TO DISPLAY-ACTION
02851          GO TO 2100-CONT.
02852
02853      IF PI-SAVE-SYS-ID = 'CV'
02854          IF AT-CLAIM-TYPE = 'L'
02855              MOVE AT-CV-PMT-CODE             TO  WS-SUB
02856              IF WS-SUB < 1 OR > 8
02857                  MOVE 1                      TO  WS-SUB
02858                  MOVE CV-PAY-DESC (WS-SUB)   TO  DISPLAY-ACTION
02859              ELSE
02860                  MOVE CV-PAY-DESC (WS-SUB)   TO  DISPLAY-ACTION.
02861
082218     IF AT-TO-BE-WRITTEN-DT > LOW-VALUES
082218       AND AT-CHECK-WRITTEN-DT = LOW-VALUES
082218       AND AT-VOID-DT = LOW-VALUES
082218        MOVE 'HOLD & PAY'        TO  DISPLAY-ACTION
082218     ELSE
022106     IF AT-PAYMENT-TYPE = 'I'
022106        MOVE 'INTEREST PMT'      TO DISPLAY-ACTION
022106     ELSE
02862      IF (PI-SAVE-SYS-ID = 'CV' AND AT-CLAIM-TYPE = 'A')
02863                   OR
02864          PI-SAVE-SYS-ID NOT = 'CV'
02865          MOVE AT-PAYMENT-TYPE           TO  WS-SUB
02866          IF WS-SUB < 1 OR > 6
02867              MOVE 2                     TO  WS-SUB
02868              MOVE PAY-DESC (WS-SUB)     TO  DISPLAY-ACTION
02869          ELSE
02870              MOVE PAY-DESC (WS-SUB)     TO  DISPLAY-ACTION.
02871
02872  2100-CONT.
02873
02874      MOVE AT-RECORDED-BY         TO DISPLAY-BY.
02875      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
02876      MOVE ' '                    TO DC-OPTION-CODE.
02877      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02878      MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
02879      MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
02880
013017     if at-ach-payment = 'Y'
013017        move '  ACH  '           to pmt-check-head
013017     else
013017        move ' CHECK='           to pmt-check-head
013017     end-if
02881      IF AT-VOID-DT NOT = LOW-VALUES
02882         MOVE AT-VOID-DT         TO DC-BIN-DATE-1
02883         MOVE ' '                TO DC-OPTION-CODE
02884         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02885         MOVE DC-GREG-DATE-1-EDIT TO PMT-PD-THRU
02886         IF AT-VOID-TYPE = 'S'
02887             MOVE 'STOP DT='     TO PMT-VAR
02888         ELSE
02889             MOVE 'VOIDED ='     TO PMT-VAR
02890      ELSE
02891         IF AT-PAID-THRU-DT NOT = LOW-VALUES
02892            MOVE AT-PAID-THRU-DT TO DC-BIN-DATE-1
02893            MOVE ' '             TO DC-OPTION-CODE
02894            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02895            MOVE DC-GREG-DATE-1-EDIT TO PMT-PD-THRU
02896            MOVE 'PD THRU='     TO PMT-VAR
02897            IF PI-USES-PAID-TO
02898               MOVE AT-PAID-THRU-DT TO DC-BIN-DATE-1
02899               MOVE '6'             TO DC-OPTION-CODE
02900               MOVE +1              TO DC-ELAPSED-DAYS
02901               MOVE +0              TO DC-ELAPSED-MONTHS
02902               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02903               MOVE DC-GREG-DATE-1-EDIT TO PMT-PD-THRU
02904               MOVE 'PAID TO='      TO PMT-VAR
02905            ELSE
02906               NEXT SENTENCE
02907         ELSE
02908            IF CHARGEABLE-EXPENSE OR NON-CHARGEABLE-EXPENSE
02909               MOVE 'EXP TYP='       TO PMT-VAR
02910               MOVE AT-EXPENSE-TYPE  TO PMT-PD-THRU
02911            ELSE
02912               MOVE SPACES      TO PMT-VAR  PMT-PD-THRU.
02913
02914      MOVE AT-CHECK-NO               TO PMT-CHECK-NO.
02915      MOVE AT-AMOUNT-PAID            TO PMT-AMOUNT.
02916      MOVE PAYMENT-TEXT              TO DISPLAY-TEXT.
02917      MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
02918
02919      GO TO 2800-INCR-DISPLAY-CNT.
02920
02921      EJECT
02922  2200-AUTO-PAYMENT-TRAILER.
02923      MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
02924      MOVE 'AUTO PMT SETUP'       TO DISPLAY-ACTION.
02925      MOVE AT-RECORDED-BY         TO DISPLAY-BY.
02926
02927      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
02928      MOVE ' '                    TO DC-OPTION-CODE.
02929      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02930      MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
02931
02932      MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
02933
02934      IF PI-COMPANY-ID = 'DMD'
02935         IF AT-TERMINATED-DT NOT = SPACES AND ZEROS
02936                                          AND LOW-VALUES
02937            MOVE AT-TERMINATED-DT          TO DC-BIN-DATE-1
02938            MOVE ' '                       TO DC-OPTION-CODE
02939            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02940            MOVE DC-GREG-DATE-1-EDIT       TO AUTO-TERMINATED
02941            MOVE AT-REGULAR-PMT-AMT        TO AUTO-TERM-AMT
02942            MOVE AUTO-TERM-TEXT            TO DISPLAY-TEXT
02943            MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY
02944                                             (DISPLAY-CNT)
02945            GO TO 2800-INCR-DISPLAY-CNT.
02946
02947      MOVE AT-SCHEDULE-START-DT   TO DC-BIN-DATE-1.
02948      MOVE ' '                    TO DC-OPTION-CODE.
02949      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02950      MOVE DC-GREG-DATE-1-EDIT    TO AUTO-START.
02951
02952      IF PI-USES-PAID-TO
02953          MOVE AT-SCHEDULE-END-DT        TO  DC-BIN-DATE-1
02954          MOVE '6'                       TO  DC-OPTION-CODE
02955          MOVE +1                        TO  DC-ELAPSED-DAYS
02956          MOVE +0                        TO  DC-ELAPSED-MONTHS
02957          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02958          IF NO-CONVERSION-ERROR
02959              MOVE DC-GREG-DATE-1-EDIT   TO  AUTO-END
02960          ELSE
02961              MOVE SPACES                TO  AUTO-END
02962      ELSE
02963          MOVE AT-SCHEDULE-END-DT        TO  DC-BIN-DATE-1
02964          MOVE ' '                       TO  DC-OPTION-CODE
02965          MOVE +0                        TO  DC-ELAPSED-DAYS
02966                                             DC-ELAPSED-MONTHS
02967          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02968          IF NO-CONVERSION-ERROR
02969              MOVE DC-GREG-DATE-1-EDIT   TO  AUTO-END
02970          ELSE
02971              MOVE SPACES                TO  AUTO-END.
02972
02973      MOVE AT-REGULAR-PMT-AMT        TO AUTO-AMOUNT.
02974      MOVE AUTO-PMT-TEXT             TO DISPLAY-TEXT.
02975      MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
02976
02977      GO TO 2800-INCR-DISPLAY-CNT.
02978
02979      EJECT
02980  2300-CORRESPONDENCE-TRAILER.
02981      MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
02982      MOVE 'LETTER'               TO DISP-ACT-A.
02983      MOVE AT-STD-LETTER-FORM     TO DISP-ACT-B.
02984      MOVE AT-RECORDED-BY         TO DISPLAY-BY.
02985
02986      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
02987      MOVE ' '                    TO DC-OPTION-CODE.
02988      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02989      MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
02990
02991      MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
02992
02993      IF AT-ADDRESEE-TYPE = 'I'
02994          MOVE 1                  TO WS-SUB
02995       ELSE
02996      IF AT-ADDRESEE-TYPE = 'B'
02997          MOVE 2                  TO WS-SUB
02998       ELSE
02999      IF AT-ADDRESEE-TYPE = 'A'
03000          MOVE 3                  TO WS-SUB
03001       ELSE
03002      IF AT-ADDRESEE-TYPE = 'P'
03003          MOVE 4                  TO WS-SUB
03004       ELSE
03005      IF AT-ADDRESEE-TYPE = 'E'
03006          MOVE 5                  TO WS-SUB
03007       ELSE
03008      IF AT-ADDRESEE-TYPE = 'O'
03009          MOVE 6                  TO WS-SUB
03010       ELSE
03011      IF AT-ADDRESEE-TYPE = 'Q'
03012          MOVE 7                  TO WS-SUB
03013       ELSE
03014          MOVE 0                  TO WS-SUB.
03015
03016      IF WS-SUB = 0
03017          MOVE SPACES             TO  CORR-TO
03018      ELSE
03019          MOVE CORR-DESC (WS-SUB) TO  CORR-TO.
03020
113010     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
113010        AND (AT-LETTER-ANSWERED-DT = LOW-VALUES  OR
113010             AT-LETTER-ANSWERED-DT > AT-STOP-LETTER-DT)
113010          MOVE '   STOP='         TO CORR-RECVD-LIT
113010          MOVE AT-STOP-LETTER-DT  TO DC-BIN-DATE-1
113010          MOVE ' '                TO DC-OPTION-CODE
113010          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
113010          MOVE DC-GREG-DATE-1-EDIT TO CORR-RECVD
113010     ELSE
041613        IF AT-LETTER-SENT-DT = LOW-VALUES OR SPACES
041613            MOVE 'MAIL RCVD'    TO DISP-ACT-A
041613        END-IF
113010        MOVE '  RECVD='         TO CORR-RECVD-LIT
113010        IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES
113010            MOVE AT-LETTER-ANSWERED-DT TO DC-BIN-DATE-1
113010            MOVE ' '                TO DC-OPTION-CODE
113010            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
113010            MOVE DC-GREG-DATE-1-EDIT TO CORR-RECVD
113010        ELSE
113010            MOVE SPACES             TO CORR-RECVD
113010        END-IF
113010     END-IF.
03028
03029      IF AT-RESEND-PRINT-DATE NOT = LOW-VALUES
03030          MOVE '  RESENT='          TO CORR-VAR
03031          MOVE AT-RESEND-PRINT-DATE TO DC-BIN-DATE-1
03032          MOVE ' '                  TO DC-OPTION-CODE
03033          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03034          MOVE DC-GREG-DATE-1-EDIT  TO CORR-RESENT
03035      ELSE
03036          IF AT-AUTO-RE-SEND-DT NOT = LOW-VALUES
03037              MOVE '  RESEND='         TO CORR-VAR
03038              MOVE AT-AUTO-RE-SEND-DT  TO DC-BIN-DATE-1
03039              MOVE ' '                 TO DC-OPTION-CODE
03040              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03041              MOVE DC-GREG-DATE-1-EDIT TO CORR-RESENT
03042          ELSE
03043             IF AT-LETTER-ARCHIVE-NO NOT = ZEROS
03044                MOVE 'ARCH. NO='          TO CORR-VAR
03045                MOVE AT-LETTER-ARCHIVE-NO TO CORR-ARCH-NO
03046             ELSE
062217                IF AT-AUTH-RCVD >  SPACES
062217                   MOVE ' AUTH RCV'  TO CORR-VAR
062217                   MOVE SPACES       TO CORR-RESENT
062217                   MOVE AT-AUTH-RCVD TO CORR-RESENT(2:1)
062217                ELSE
03047                    MOVE SPACES       TO CORR-RESENT  CORR-VAR.
03048
CIDMOD*    IF PI-COMPANY-ID = 'DMD'
CIDMOD*        IF AT-DMD-LETTER-PURGE-DT NOT = SPACES AND LOW-VALUES
CIDMOD*            MOVE '  PURGED='             TO CORR-VAR
CIDMOD*            MOVE AT-DMD-LETTER-PURGE-DT  TO DC-BIN-DATE-1
CIDMOD*            MOVE ' '                     TO DC-OPTION-CODE
CIDMOD*            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
CIDMOD*            MOVE DC-GREG-DATE-1-EDIT     TO CORR-RESENT.
03056
03057      MOVE CORRESPONDENCE-TEXT       TO DISPLAY-TEXT.
03058      MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
03059
03060      GO TO 2800-INCR-DISPLAY-CNT.
03061
03062      EJECT
03063  2400-GENERAL-INFO-TRAILER.
03064
03065      IF AT-PAYMENT-NOTE
03066          GO TO 2020-READ-TRAILER-LOOP.
03067
03068      IF AT-CONTINUED-NOTE
03069          GO TO 2020-READ-TRAILER-LOOP.
03070
03071      MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
03072
03073      IF AT-MAINT-NOTE
03074          MOVE 'MAINT NOTES'      TO DISPLAY-ACTION
03075      ELSE
03076          IF AT-CALL-NOTE
03077              MOVE 'CALL'         TO DISP-ACT-A
102418             EVALUATE TRUE
102418               WHEN AT-PHONE-CALL-IN
03079                  MOVE 'IN'       TO DISP-ACT-B
102418               WHEN AT-PHONE-CALL-OUT
102418                 MOVE 'OUT'      TO DISP-ACT-B
102418               WHEN OTHER
102418                 MOVE 'NEW'      TO DISP-ACT-B
102418             END-EVALUATE
03082          ELSE
03083              IF AT-CERT-CHANGE
03084                  MOVE 'CERT CHANGE'
03085                                  TO DISPLAY-ACTION
041807             ELSE IF AT-APPROVAL-NOTE
041807                 MOVE 'APPROVL REVIEW' TO DISPLAY-ACTION
080106             ELSE IF AT-NOTE-FILE-NOTE
080106                 MOVE 'NOTE & FILE' TO DISPLAY-ACTION
03086              ELSE
03087                  MOVE 'NOTE'     TO DISPLAY-ACTION.
03088
03089      MOVE AT-RECORDED-BY         TO DISPLAY-BY.
03090      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
03091      MOVE ' '                    TO DC-OPTION-CODE.
03092      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03093      MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
03094
03095      MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
03096      MOVE AT-INFO-LINE-1         TO DISPLAY-TEXT.
03097      MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
03098      GO TO 2800-INCR-DISPLAY-CNT.
03099
03100      EJECT
03101  2500-AUTO-PROMPT-TRAILER.
03102      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
03103      MOVE '5'                    TO DC-OPTION-CODE.
03104      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03105
03106      IF DC-BIN-DATE-1 > AT-PROMPT-END-DT
03107          GO TO 2020-READ-TRAILER-LOOP.
03108
03109      MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
03110      MOVE 'REMINDER'             TO DISPLAY-ACTION.
03111      MOVE AT-RECORDED-BY         TO DISPLAY-BY.
03112
03113      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
03114      MOVE ' '                    TO DC-OPTION-CODE.
03115      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03116      MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
03117
03118      MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
03119
03120      MOVE AT-PROMPT-END-DT       TO DC-BIN-DATE-1.
03121      MOVE ' '                    TO DC-OPTION-CODE.
03122      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03123      MOVE DC-GREG-DATE-1-EDIT    TO PROMPT-END.
03124
03125      MOVE AT-PROMPT-LINE-1       TO PROMPT-MSG.
03126      MOVE PROMPT-TEXT            TO DISPLAY-TEXT.
03127      MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
03128      GO TO 2800-INCR-DISPLAY-CNT.
03129
03130      EJECT
03131  2600-DENIAL-TRAILER.
03132      MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
03133      MOVE 'DENIAL'               TO DISPLAY-ACTION.
03134      MOVE AT-RECORDED-BY         TO DISPLAY-BY.
03135
03136      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
03137      MOVE ' '                    TO DC-OPTION-CODE.
03138      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03139      MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
03140
03141      MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
03142
03143      IF AT-RETRACTION-DT NOT = LOW-VALUES
03144          MOVE AT-DENIAL-DT       TO DC-BIN-DATE-1
03145          MOVE ' '                TO DC-OPTION-CODE
03146          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03147          MOVE DC-GREG-DATE-1-EDIT TO DENIED-DATE
03148          MOVE AT-RETRACTION-DT   TO DC-BIN-DATE-1
03149          MOVE ' '                TO DC-OPTION-CODE
03150          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03151          MOVE DC-GREG-DATE-1-EDIT TO RECONSIDERED-DATE
03152          MOVE RECONSIDERED-TEXT   TO DISPLAY-TEXT
03153          MOVE TRAILER-DISPLAY-WORK-AREA
03154                                  TO MAP-DISPLAY (DISPLAY-CNT)
03155          GO TO 2800-INCR-DISPLAY-CNT.
03156
03157      MOVE AT-DENIAL-DT           TO DC-BIN-DATE-1.
03158      MOVE ' '                    TO DC-OPTION-CODE.
03159      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03160      MOVE DC-GREG-DATE-1-EDIT    TO DENIAL-DATE.
03161
03162      MOVE AT-DENIAL-REASON-CODE  TO DENIAL-CODE.
03163      MOVE AT-DENIAL-INFO-1       TO DENIAL-MSG.
042110     IF CL-DENIAL-TYPE NOT = SPACES AND LOW-VALUES
042110        EVALUATE CL-DENIAL-TYPE
042110           WHEN '1'
042110              MOVE 'DENIAL STATUS - DENIAL       '
042110                                 TO DENTYPO
042110           WHEN '2'
042110              MOVE 'DENIAL STATUS - RESCISSION   '
042110                                 TO DENTYPO
042110           WHEN '3'
042110              MOVE 'DENIAL STATUS - REFORMATION  '
042110                                 TO DENTYPO
042110           WHEN '4'
042110              MOVE 'DENIAL STATUS - REF TO RESC  '
042110                                 TO DENTYPO
042110           WHEN '5'
042110              MOVE 'DENIAL STATUS - RECONSIDERED '
042110                                 TO DENTYPO
042110        END-EVALUATE
042110        MOVE AL-SANOF            TO DENTYPA
042110     END-IF
03164      MOVE DENIAL-TEXT            TO DISPLAY-TEXT.
03165      MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
03166      GO TO 2800-INCR-DISPLAY-CNT.
03167
03168      EJECT
03169  2700-INCURRED-CHANGE-TRAILER.
03170      MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
03171      MOVE 'INCUR DATE CHG'       TO DISPLAY-ACTION.
03172      MOVE AT-RECORDED-BY         TO DISPLAY-BY.
03173
03174      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
03175      MOVE ' '                    TO DC-OPTION-CODE.
03176      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03177      MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
03178
03179      MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
03180
03181      IF AT-OLD-INCURRED-DT NOT = LOW-VALUES
03182         MOVE AT-OLD-INCURRED-DT  TO DC-BIN-DATE-1
03183         MOVE ' '                 TO DC-OPTION-CODE
03184         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03185         IF DATE-CONVERSION-ERROR
03186            MOVE SPACES           TO INCUR-DT
03187         ELSE
03188            MOVE DC-GREG-DATE-1-EDIT  TO INCUR-DT
03189      ELSE
03190         MOVE SPACES              TO INCUR-DT.
03191
03192      IF PI-USES-PAID-TO
03193         MOVE ' PD  TO ='         TO PDTHRU-HEAD.
03194
03195      IF AT-OLD-PAID-THRU-DT NOT = LOW-VALUES
03196         MOVE AT-OLD-PAID-THRU-DT TO DC-BIN-DATE-1
03197         MOVE ' '                 TO DC-OPTION-CODE
03198         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03199         IF DATE-CONVERSION-ERROR
03200            MOVE SPACES           TO INCUR-PDTHRU
03201         ELSE
03202            MOVE DC-GREG-DATE-1-EDIT  TO INCUR-PDTHRU
03203            IF PI-USES-PAID-TO
03204               MOVE AT-OLD-PAID-THRU-DT TO DC-BIN-DATE-1
03205               MOVE '6'                 TO DC-OPTION-CODE
03206               MOVE +1                  TO DC-ELAPSED-DAYS
03207               MOVE +0                  TO DC-ELAPSED-MONTHS
03208               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03209               IF NO-CONVERSION-ERROR
03210                  MOVE DC-GREG-DATE-1-EDIT  TO INCUR-PDTHRU
03211               ELSE
03212                  NEXT SENTENCE
03213            ELSE
03214               NEXT SENTENCE
03215      ELSE
03216         MOVE SPACES              TO INCUR-PDTHRU.
03217
03218      MOVE AT-OLD-TOTAL-PAID      TO INCUR-PAID.
03219      MOVE INCUR-TEXT             TO DISPLAY-TEXT.
03220      MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
03221      GO TO 2800-INCR-DISPLAY-CNT.
03222
03223      EJECT
03224  2710-FORM-CONTROL-TRAILER.
03225      MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
03226      MOVE 'FORM CTL'             TO DISP-ACT-A.
03227      IF INITIAL-FORM
03228         MOVE 'INIT'              TO DISP-ACT-B
03229      ELSE
03230         MOVE 'PROG'              TO DISP-ACT-B.
03231
03232      MOVE AT-RECORDED-BY         TO DISPLAY-BY.
03233
03234      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
03235      MOVE ' '                    TO DC-OPTION-CODE.
03236      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03237      MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
03238
03239      MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
03240
03241      IF AT-FORM-ADDRESS = 'I'
03242          MOVE 1                  TO WS-SUB
03243       ELSE
03244      IF AT-FORM-ADDRESS = 'A'
03245          MOVE 3                  TO WS-SUB
03246       ELSE
03247      IF AT-FORM-ADDRESS = 'O'
03248          MOVE 6                  TO WS-SUB
03249       ELSE
03250      IF AT-FORM-ADDRESS = 'Q'
03251          MOVE 7                  TO WS-SUB
03252       ELSE
03253          MOVE 0                  TO WS-SUB.
03254
03255      IF WS-SUB = 0
03256          MOVE SPACES             TO  CORR-TO
03257      ELSE
03258          MOVE CORR-DESC (WS-SUB) TO  CORR-TO.
03259
03260      IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES
03261          MOVE AT-FORM-ANSWERED-DT TO DC-BIN-DATE-1
03262          MOVE ' '                 TO DC-OPTION-CODE
03263          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03264          MOVE DC-GREG-DATE-1-EDIT TO CORR-RECVD
03265      ELSE
03266          MOVE SPACES              TO CORR-RECVD.
03267
03268      IF AT-FORM-PRINTED-DT  = LOW-VALUES
03269          MOVE '    SEND='         TO CORR-VAR
03270          MOVE AT-FORM-SEND-ON-DT  TO DC-BIN-DATE-1
03271          MOVE ' '                 TO DC-OPTION-CODE
03272          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03273          MOVE DC-GREG-DATE-1-EDIT TO CORR-RESENT
03274      ELSE
03275          MOVE '    SENT='        TO CORR-VAR
03276          IF AT-FORM-REPRINT-DT  = LOW-VALUES
03277             MOVE AT-FORM-PRINTED-DT TO DC-BIN-DATE-1
03278             MOVE ' '                TO DC-OPTION-CODE
03279             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03280             MOVE DC-GREG-DATE-1-EDIT TO CORR-RESENT
03281          ELSE
03282             MOVE AT-FORM-REPRINT-DT TO DC-BIN-DATE-1
03283             MOVE ' '                TO DC-OPTION-CODE
03284             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03285             MOVE DC-GREG-DATE-1-EDIT    TO CORR-RESENT.
03286
03287      MOVE CORRESPONDENCE-TEXT       TO DISPLAY-TEXT.
03288      MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
03289
03290      GO TO 2800-INCR-DISPLAY-CNT.
03291
03292  2800-INCR-DISPLAY-CNT.
03293      IF LCP-ONCTR-01 =  0
03294          ADD 1 TO LCP-ONCTR-01
03295          MOVE 'Y'                TO PI-PREV-DISP
03296          MOVE TRLR-SEQ-NO        TO PI-SAVE-LOW  PI-SAVE-HIGH.
03297
03298      IF DIRECTION-SWITCH = 'F'
03299          MOVE TRLR-SEQ-NO        TO PI-SAVE-HIGH
03300          ADD +1                  TO DISPLAY-CNT
03301          IF DISPLAY-CNT > +4
03302              GO TO 2999-EXIT
03303          ELSE
03304              NEXT SENTENCE
03305      ELSE
03306          MOVE TRLR-SEQ-NO        TO PI-SAVE-LOW
03307          SUBTRACT +1 FROM DISPLAY-CNT
062602         if map-display (1) (1:14) = 'SPECIAL REVIEW'
061511           OR MAP-DISPLAY (1) (1:14) = 'VERIFY SSN    '
020613           OR MAP-DISPLAY (1) (1:14) = 'CAUSAL STATE  '
061511            IF MAP-DISPLAY (2) (1:14) = 'VERIFY SSN    '
020613              OR MAP-DISPLAY (2) (1:14) = 'CAUSAL STATE  '
020613                 IF MAP-DISPLAY (3) (1:14) = 'CAUSAL STATE  '
020613                     IF DISPLAY-CNT < +4
020613                        GO TO 2999-EXIT
020613                     END-IF
020613                 END-IF
061511                 IF DISPLAY-CNT < +3
061511                   GO TO 2999-EXIT
061511                 END-IF
061511            END-IF
062602            IF DISPLAY-CNT < +2
062602               GO TO 2999-EXIT
062602            END-IF
062602         ELSE
03308             IF DISPLAY-CNT < +1
03309                GO TO 2999-EXIT.
03310
03311      GO TO 2020-READ-TRAILER-LOOP.
03312
03313  2950-NO-MORE-TRAILERS.
03314      MOVE ER-0303                TO EMI-ERROR.
03315      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03316
03317  2999-EXIT.
03318      EXIT.
03319
03320      EJECT
061013 3997-GET-ERPDEF.
061013
061013     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
061013     MOVE CM-STATE               TO ERPDEF-STATE
010816     if cm-clp-state not = cm-state and spaces and zeros
010816        move cm-clp-state        to erpdef-state
010816     end-if
061013     MOVE am-dcc-product-code    TO ERPDEF-PROD-CD
061013     MOVE 'A'                    TO ERPDEF-BEN-TYPE
061013     MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
061013     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
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
      *    MOVE '&,         G          &  N#00011504' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303131353034' TO DFHEIV0(25:11)
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
061013        GO TO 3997-EXIT
061013     END-IF
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
      *    MOVE '&.IL                  )  N#00011516' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303131353136' TO DFHEIV0(25:11)
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
061013        IF WS-RESP-NORMAL
061013           IF (ERPDEF-KEY-SAVE (1:16) =
061013              PD-CONTROL-PRIMARY (1:16))
061013              AND (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
061013              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
080322                 (A1 > +11)
061013                 OR (PD-PROD-CODE (A1) = cl-claim-type)
061013              END-PERFORM
080322              IF A1 < +12
061013                 SET PDEF-FOUND TO TRUE
061013              end-if
061013           END-IF
061013        END-IF
061013     END-IF
061013
061013     .
061013 3997-ENDBR.
061013
061013     
      * EXEC CICS ENDBR
061013*       DATASET  ('ERPDEF')
061013*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011540' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     .
061013 3997-EXIT.
061013     EXIT.
03321  6000-CHECK-AUTO-ACTIVITY.
03322 ******************************************************************
03323 *    READ THE CONTROL FILE TO VERIFY THAT AN AUTOMATIC           *
03324 *    ACTIVITY RECORD EXISTS.                                     *
03325 ******************************************************************
03326
03327      
      * EXEC CICS HANDLE CONDITION
03328 *        NOTFND   (6000-NOT-FOUND)
03329 *    END-EXEC.
      *    MOVE '"$I                   ! 4 #00011553' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303131353533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03330
03331      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID
03332      MOVE 'T'                    TO  CNTL-REC-TYPE
03333      MOVE SPACES                 TO  CNTL-ACCESS
03334      MOVE +0                     TO  CNTL-SEQ-NO
03335      MOVE 'CNTL'                 TO  FILE-SWITCH.
03336
03337      
      * EXEC CICS READ
03338 *        DATASET   ('ELCNTL')
03339 *        RIDFLD    (ELCNTL-KEY)
03340 *        SET       (ADDRESS OF CONTROL-FILE)
03341 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00011563' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131353633' TO DFHEIV0(25:11)
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
           
03342
03343      MOVE 'Y'                    TO  WS-ACT-REC-FOUND-SW.
03344      GO TO 6000-EXIT.
03345
03346  6000-NOT-FOUND.
03347      MOVE 'N'                    TO  WS-ACT-REC-FOUND-SW.
03348      MOVE AL-UABON               TO  ACTCDA.
03349      MOVE -1                     TO  ACTCDL.
03350      MOVE ER-3516                TO  EMI-ERROR
03351      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03352
03353  6000-EXIT.
03354      EXIT.
03355
03356      EJECT
03357  6050-AUTO-LETTER-WRITER.
03358 ******************************************************************
03359 *    THE FOLLOWING ACTIVITIES WILL BE PERFORMED BASED ON THE     *
03360 *    CODES ENTERED IN THE AUTOMATIC ACTIVITY RECORD.             *
03361 *       1.  IF SPECIFIED, THE AUTOMATIC LETTER WRITER PROGRAM    *
03362 *           WILL BE LINKED TO AND A LETTER WILL BE GENERATED.    *
03363 *           A.  ANY RESEND OR FOLLOW UP DATES SPECIFIED IN THE   *
03364 *               ACTIVITY RECORD WILL BE STORED IN THE LETTER     *
03365 ******************************************************************
03366
03367      IF WS-ACTIVITY-CODE = ZERO
03368          GO TO 6050-EXIT.
03369
03370      MOVE WS-ACTIVITY-CODE       TO  SUB.
03371      SUBTRACT 9 FROM SUB.
03372
03373  6050-EDIT-USER-ACTIVITY-LOOP.
03374
03375      IF (CF-USER-ACTIVE-SW (SUB) = ' ' OR 'N') OR
03376         (CF-USER-LETTER-ID (SUB) = SPACES OR LOW-VALUES)
03377              GO TO 6050-EXIT.
03378
03379  6050-START-AUTO-LETTER-WRITER.
03380
03381      MOVE LOW-VALUES                 TO  W-1523-LINKDATA.
03382      MOVE PROGRAM-INTERFACE-BLOCK    TO  W-1523-COMMON-PI-DATA.
03383
03384      MOVE CF-USER-LETTER-ID (SUB)    TO W-1523-FORM-NUMBER.
03385
03386      IF CF-USER-RESEND-DAYS (SUB) NOT = ZEROS
03387          MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1
03388          MOVE '6'                    TO  DC-OPTION-CODE
03389          MOVE CF-USER-RESEND-DAYS (SUB)  TO  DC-ELAPSED-DAYS
03390          MOVE +0                     TO  DC-ELAPSED-MONTHS
03391          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03392          IF NO-CONVERSION-ERROR
03393              MOVE DC-BIN-DATE-2      TO  W-1523-RESEND-DATE
03394          ELSE
03395              MOVE LOW-VALUES         TO  W-1523-RESEND-DATE.
03396
03397      IF CF-USER-FOLLOW-UP-DAYS (SUB) NOT = ZEROS
03398          MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1
03399          MOVE '6'                    TO  DC-OPTION-CODE
03400          MOVE CF-USER-FOLLOW-UP-DAYS (SUB)   TO  DC-ELAPSED-DAYS
03401          MOVE +0                     TO  DC-ELAPSED-MONTHS
03402          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
03403          IF NO-CONVERSION-ERROR
03404              MOVE DC-BIN-DATE-2      TO  W-1523-FOLLOW-UP-DATE
03405          ELSE
03406              MOVE LOW-VALUES         TO  W-1523-FOLLOW-UP-DATE.
03407
03408 ******************************************************************
03409 *    LINK TO THE AUTOMATIC LETTER WRITER PROGRAM.                *
03410 ******************************************************************
03411
03412      
      * EXEC CICS LINK
03413 *        PROGRAM    (LINK-1523)
03414 *        COMMAREA   (W-1523-LINKDATA)
03415 *        LENGTH     (W-1523-COMM-LENGTH)
03416 *    END-EXEC.
      *    MOVE '."C                   (   #00011638' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131363338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-1523, 
                 W-1523-LINKDATA, 
                 W-1523-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03417
03418 ******************************************************************
03419 *    CHECK THE ERROR CODES FROM THE AUTOMATIC LETTER WRITER      *
03420 *    PROGRAM.                                                    *
03421 *        1.  IF FATAL ERRORS ARE DETECTED BY THE AUTO LETTER     *
03422 *            WRITER PROGRAM, THE LETTER IS NOT CREATED AND AN    *
03423 *            ERROR MESSAGE (#0802) IS DISPLAYED INDICATING THAT  *
03424 *            THE LETTER WAS NOT CREATED.                         *
03425 *        2.  IF THE AUTO LETTER WRITER PROGRAM IS UNABLE TO      *
03426 *            RESOLVE A VARIABLE, THE LETTER IS CREATED AND AN    *
03427 *            ERROR MESSAGE (#0803) INDICATING THAT THE LETTER    *
03428 *            WAS CREATED WITH ERRORS IS DISPLAYED.               *
03429 ******************************************************************
03430
03431      IF W-1523-ERROR-CODE = ZEROS
03432          GO TO 6050-EXIT.
03433
03434      IF W-1523-FATAL-ERROR
03435          MOVE ER-0802                TO  EMI-ERROR
03436          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03437
03438      IF W-1523-ERROR-CODE = 0191
03439          MOVE ER-0803                TO  EMI-ERROR
03440          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03441
03442  6050-EXIT.
03443      EXIT.
03444
03445      EJECT
03446  6100-RESET-AUTO-ACTIVITY.
03447 ******************************************************************
03448 *    RESET ALL LETTER ACTIVITY AWAITING FURTHER ACTION (RESEND,  *
03449 *    FOLLOW-UP, ETC) IF THE AUTOMATIC ACTIVITY CODE IS SET       *
03450 *    TO ZEROS.                                                   *
03451 ******************************************************************
03452
03453      MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.
03454      MOVE PI-CARRIER             TO  TRLR-CARRIER.
03455      MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.
03456      MOVE PI-CERT-NO             TO  TRLR-CERT-NO.
03457      MOVE +100                   TO  TRLR-SEQ-NO.
03458      MOVE 'TRLR'                 TO  FILE-SWITCH.
03459
03460  6100-STARTBR-TRLR.
03461
03462      
      * EXEC CICS HANDLE CONDITION
03463 *        ENDFILE  (6100-END-RESET)
03464 *        NOTFND   (6100-END-RESET)
03465 *    END-EXEC.
      *    MOVE '"$''I                  ! 5 #00011688' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303131363838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03466
03467      
      * EXEC CICS STARTBR
03468 *        DATASET   ('ELTRLR')
03469 *        RIDFLD    (ELTRLR-KEY)
03470 *        GTEQ
03471 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011693' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03472
03473      MOVE 'Y'                        TO  WS-BROWSE-SW.
03474
03475  6100-READ-NEXT.
03476
03477      
      * EXEC CICS READNEXT
03478 *        DATASET   ('ELTRLR')
03479 *        RIDFLD    (ELTRLR-KEY)
03480 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
03481 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00011703' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373033' TO DFHEIV0(25:11)
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
           
03482
03483      IF (PI-COMPANY-CD NOT = TRLR-COMP-CD)   OR
03484         (PI-CARRIER    NOT = TRLR-CARRIER)   OR
03485         (PI-CLAIM-NO   NOT = TRLR-CLAIM-NO)  OR
03486         (PI-CERT-NO    NOT = TRLR-CERT-NO)
03487          GO TO 6100-END-RESET.
03488
03489      IF ELTRLR-KEY = PI-PREV-TRLR-KEY
03490          GO TO 6100-READ-NEXT.
03491
03492      IF AT-TRAILER-TYPE NOT = '4'
03493          GO TO 6100-READ-NEXT.
03494
03495      IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES AND SPACES
03496          GO TO 6100-READ-NEXT.
03497
03498      IF (AT-AUTO-RE-SEND-DT = LOW-VALUES OR SPACES)
03499        AND
03500         (AT-RECEIPT-FOLLOW-UP = LOW-VALUES OR SPACES)
03501          GO TO 6100-READ-NEXT.
03502
03503      IF AT-RECEIPT-FOLLOW-UP < SAVE-BIN-DATE AND
03504         AT-AUTO-RE-SEND-DT   < SAVE-BIN-DATE AND
03505         AT-RESEND-PRINT-DATE < SAVE-BIN-DATE
03506          GO TO 6100-READ-NEXT.
03507
03508      IF (AT-AUTO-RE-SEND-DT NOT = LOW-VALUES AND SPACES)
03509        AND
03510         (AT-RESEND-PRINT-DATE NOT = LOW-VALUES AND SPACES)
03511        AND
03512         (AT-RECEIPT-FOLLOW-UP = LOW-VALUES)
03513          GO TO 6100-READ-NEXT.
03514
03515  6100-END-BROWSE.
03516
03517      MOVE ELTRLR-KEY                 TO  PI-PREV-TRLR-KEY.
03518
03519      
      * EXEC CICS ENDBR
03520 *        DATASET   ('ELTRLR')
03521 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011745' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03522
03523      MOVE 'N'                        TO  WS-BROWSE-SW.
03524
03525  6100-READ-TRLR-UPDATE.
03526
03527      
      * EXEC CICS READ
03528 *        DATASET   ('ELTRLR')
03529 *        RIDFLD    (ELTRLR-KEY)
03530 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
03531 *        UPDATE
03532 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00011753' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373533' TO DFHEIV0(25:11)
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
           
03533
03534      MOVE 'Y'                        TO  WS-UPDATE-SW.
03535
03536      IF AT-AUTO-RE-SEND-DT IS NOT < SAVE-BIN-DATE
03537          MOVE LOW-VALUES             TO  AT-AUTO-RE-SEND-DT
03538          MOVE PI-COMPANY-CD          TO  ARCH-COMP-CD
03539          MOVE AT-LETTER-ARCHIVE-NO   TO  ARCH-ARCHIVE-NO
03540          MOVE '1'                    TO  ARCH-RECORD-TYPE
03541          MOVE +0                     TO  ARCH-SEQ-NO
03542          PERFORM 6200-READ-ARCH-UPDATE THRU 6200-EXIT.
03543
03544      IF AT-RECEIPT-FOLLOW-UP NOT < SAVE-BIN-DATE
03545          MOVE LOW-VALUES             TO  AT-RECEIPT-FOLLOW-UP.
03546
03547  6100-REWRITE-TRLR.
03548
03549      
      * EXEC CICS REWRITE
03550 *        DATASET   ('ELTRLR')
03551 *        FROM      (ACTIVITY-TRAILERS)
03552 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00011775' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03553
03554      GO TO 6100-STARTBR-TRLR.
03555
03556  6100-END-RESET.
03557
03558      IF WS-BROWSE-SW = 'Y'
03559          MOVE 'N'                    TO  WS-BROWSE-SW
03560          
      * EXEC CICS ENDBR
03561 *            DATASET   ('ELTRLR')
03562 *        END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011786' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03563
03564  6100-EXIT.
03565      EXIT.
03566
03567      EJECT
062602 6150-READ-TRLR92.
062602
03453      MOVE PI-COMPANY-CD          TO  WS-TRLR-COMP-CD.
03454      MOVE PI-CARRIER             TO  WS-TRLR-CARRIER.
03455      MOVE PI-CLAIM-NO            TO  WS-TRLR-CLAIM-NO.
03456      MOVE PI-CERT-NO             TO  WS-TRLR-CERT-NO.
03457      MOVE +092                   TO  WS-TRLR-SEQ-NO.
03458      MOVE 'TRLR'                 TO  FILE-SWITCH.
03459
03477      
      * EXEC CICS READ
03478 *        DATASET   ('ELTRLR')
03479 *        RIDFLD    (WS-ELTRLR-KEY)
03480 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
062602*        RESP      (WS-RESPONSE)
03481 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011803' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303131383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03482
           .
062602 6150-EXIT.
062602     EXIT.
03566
061511 6160-READ-TRLR93.
061511
061511     MOVE PI-COMPANY-CD          TO  WS-TRLR-COMP-CD.
061511     MOVE PI-CARRIER             TO  WS-TRLR-CARRIER.
061511     MOVE PI-CLAIM-NO            TO  WS-TRLR-CLAIM-NO.
061511     MOVE PI-CERT-NO             TO  WS-TRLR-CERT-NO.
061511     MOVE +093                   TO  WS-TRLR-SEQ-NO.
061511     MOVE 'TRLR'                 TO  FILE-SWITCH.
061511
061511     
      * EXEC CICS READ
061511*        DATASET   ('ELTRLR')
061511*        RIDFLD    (WS-ELTRLR-KEY)
061511*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
061511*        RESP      (WS-RESPONSE)
061511*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011823' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303131383233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
061511
061511     .
061511 6160-EXIT.
061511     EXIT.
061511
020613 6170-READ-TRLR94.
020613
020613     MOVE PI-COMPANY-CD          TO  WS-TRLR-COMP-CD.
020613     MOVE PI-CARRIER             TO  WS-TRLR-CARRIER.
020613     MOVE PI-CLAIM-NO            TO  WS-TRLR-CLAIM-NO.
020613     MOVE PI-CERT-NO             TO  WS-TRLR-CERT-NO.
020613     MOVE +094                   TO  WS-TRLR-SEQ-NO.
020613     MOVE 'TRLR'                 TO  FILE-SWITCH.
020613
020613     
      * EXEC CICS READ
020613*        DATASET   ('ELTRLR')
020613*        RIDFLD    (WS-ELTRLR-KEY)
020613*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
020613*        RESP      (WS-RESPONSE)
020613*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011843' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303131383433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
020613
020613     .
020613 6170-EXIT.
020613     EXIT.
020613
061013 6180-READ-TRLR95.
061013
061013     MOVE PI-COMPANY-CD          TO  WS-TRLR-COMP-CD.
061013     MOVE PI-CARRIER             TO  WS-TRLR-CARRIER.
061013     MOVE PI-CLAIM-NO            TO  WS-TRLR-CLAIM-NO.
061013     MOVE PI-CERT-NO             TO  WS-TRLR-CERT-NO.
061013     MOVE +095                   TO  WS-TRLR-SEQ-NO.
061013     MOVE 'TRLR'                 TO  FILE-SWITCH.
061013
061013     
      * EXEC CICS READ
061013*        DATASET   ('ELTRLR')
061013*        RIDFLD    (WS-ELTRLR-KEY)
061013*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
061013*        RESP      (WS-RESPONSE)
061013*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011863' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303131383633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
061013
061013     .
061013 6180-EXIT.
061013     EXIT.
020613
03568  6200-READ-ARCH-UPDATE.
03569 ******************************************************************
03570 *  READ AND UPDATE THE RESEND DATE ON THE LETTER ARCHIVE RECORD  *
03571 ******************************************************************
03572
03573      
      * EXEC CICS HANDLE CONDITION
03574 *        NOTFND   (6200-EXIT)
03575 *    END-EXEC.
      *    MOVE '"$I                   ! 6 #00011879' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303131383739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03576
03577      
      * EXEC CICS READ
03578 *        DATASET   ('ELARCH')
03579 *        RIDFLD    (ELARCH-KEY)
03580 *        SET       (ADDRESS OF LETTER-ARCHIVE)
03581 *        UPDATE
03582 *    END-EXEC.
           MOVE 'ELARCH' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00011883' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383833' TO DFHEIV0(25:11)
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
           
03583
03584      MOVE LOW-VALUES                 TO  LA-RESEND-DATE.
03585
03586  6200-REWRITE-ARCH.
03587      
      * EXEC CICS REWRITE
03588 *        DATASET   ('ELARCH')
03589 *        FROM      (LETTER-ARCHIVE)
03590 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
           MOVE 'ELARCH' TO DFHEIV1
      *    MOVE '&& L                  %   #00011893' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03591
03592  6200-EXIT.
03593      EXIT.
03594
03595      EJECT
03596  7000-START-BROWSE.
03597
03598      IF PI-COMPANY-ID = 'DMD'
03599              AND
03600         PI-ASS-PROCESSING
03601          GO TO 7200-BROWSING-ASS-FWRD.
03602
03603      MOVE LOW-VALUES             TO  ELMSTR-KEY.
03604      MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.
03605
03606      IF CLMNOL > 0
03607          MOVE CLMNOI             TO  MSTR-CLAIM-NO.
03608      IF CARRL > 0
03609          MOVE CARRI              TO  MSTR-CARRIER.
03610      IF CERTNOL > 0
03611          MOVE CERTNOI            TO  MSTR-CERT-NO-PRIME.
03612      IF SUFXL > 0
03613          MOVE SUFXI              TO  MSTR-CERT-NO-SUFX.
03614
03615      IF PI-PREV-CLAIM = LOW-VALUES
03616          NEXT SENTENCE
03617      ELSE
03618          IF CLMNOL  = 0 AND
03619             CARRL   = 0 AND
03620             CERTNOL = 0 AND
03621             SUFXL   = 0
03622                  MOVE PI-PREV-CLAIM  TO  ELMSTR-KEY.
03623
03624  7000-START-READING.
03625
03626      
      * EXEC CICS HANDLE CONDITION
03627 *        ENDFILE  (8700-END-FILE)
03628 *        NOTFND   (8600-NOT-FOUND)
03629 *    END-EXEC.
      *    MOVE '"$''I                  ! 7 #00011932' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303131393332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03630
CIDMOD     IF YESNOSWL > 0
CIDMOD         MOVE YESNOSWI TO CL-YESNOSW
CIDMOD     END-IF.
CIDMOD
03631      MOVE 'MSTR'                 TO FILE-SWITCH.
03632
03633      
      * EXEC CICS STARTBR
03634 *        DATASET   (PI-CURRENT-FILE)
03635 *        RIDFLD    (ELMSTR-KEY)
03636 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011943' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03637
03638  7000-READ-NEXT.
03639
03640      
      * EXEC CICS READNEXT
03641 *        DATASET   (PI-CURRENT-FILE)
03642 *        RIDFLD    (ELMSTR-KEY)
03643 *        INTO      (CLAIM-MASTER)
03644 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00011950' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03645
03646      IF ELMSTR-KEY = PI-PREV-CLAIM
03647          MOVE CL-CLAIM-NO        TO  PI-PREV-CLMNO
03648          GO TO 7000-READ-NEXT.
03649
03650      IF PI-COMPANY-CD NOT = CL-COMPANY-CD
03651          GO TO 8700-END-FILE.
03652
03653  7000-PICK-UP-LOGIC.
03654
03655      IF PI-COMPANY-ID  =  'CRI'
03656          IF CL-CLAIM-NO NOT = PI-PREV-CLMNO
03657              GO TO 8700-END-FILE.
03658
03659      IF PI-CARRIER-SECURITY > SPACES
03660          IF CL-CARRIER = PI-CARRIER-SECURITY
03661              NEXT SENTENCE
03662          ELSE
03663              GO TO 7000-READ-NEXT.
03664
03665      IF PI-ACCOUNT-SECURITY > SPACES
03666          IF CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY
03667              NEXT SENTENCE
03668          ELSE
03669              GO TO 7000-READ-NEXT.
03670
03671      GO TO 1000-READ-CLAIM.
03672                                  EJECT
03673  7200-BROWSING-ASS-FWRD.
03674
03675      COMPUTE PI-ASS-NDX = PI-ASS-NDX + 1.
03676
03677      IF PI-ASS-NDX > +24
03678              OR
03679         PI-ASS-CERTS (PI-ASS-NDX) NOT > LOW-VALUE
03680          IF PI-PF15-LAST
03681              MOVE PI-ORIGINAL-ASS-CERT
03682                                  TO ELMSTR-KEY
03683              MOVE ZEROS          TO PI-DAYS-PAID
03684                                     PI-TOTAL-PAID
03685              MOVE LOW-VALUES     TO PI-ASSOCIATED-CERTS-TABLE
03686                                     PI-LAST-PF-KEY-IND
03687                                     PI-ASSOCIATED-PROCESS-IND
03688              GO TO 7000-START-READING
03689          ELSE
03690              COMPUTE PI-ASS-NDX = PI-ASS-NDX - 1
03691              MOVE 'F'            TO PI-LAST-PF-KEY-IND
03692              MOVE -1             TO ENTERPFL
03693              MOVE ER-0936        TO EMI-ERROR
03694              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03695              GO TO 8200-SEND-DATAONLY.
03696
03697      MOVE PI-PREV-CLAIM          TO ELMSTR-KEY.
03698      MOVE PI-ASS-CERTS (PI-ASS-NDX)
03699                                  TO MSTR-CERT-NO.
03700      MOVE 'MSTR'                 TO FILE-SWITCH.
03701
03702      
      * EXEC CICS HANDLE CONDITION
03703 *        NOTFND   (0450-NOT-FOUND)
03704 *    END-EXEC.
      *    MOVE '"$I                   ! 8 #00012012' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303132303132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03705
03706      
      * EXEC CICS READ
03707 *        DATASET   (PI-CURRENT-FILE)
03708 *        RIDFLD    (ELMSTR-KEY)
03709 *        INTO      (CLAIM-MASTER)
03710 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00012016' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03711
03712      GO TO 7000-PICK-UP-LOGIC.
03713
03714  7200-EXIT.
03715      EXIT.
03716
03717      EJECT
03718  7500-START-BROWSE.
03719
03720      IF PI-COMPANY-ID = 'DMD'
03721              AND
03722         PI-ASS-PROCESSING
03723          GO TO 7600-BROWSING-ASS-BWRD.
03724
03725      MOVE LOW-VALUES             TO  ELMSTR-KEY.
03726      MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.
03727
03728      IF CLMNOL > 0
03729          MOVE CLMNOI             TO  MSTR-CLAIM-NO.
03730      IF CARRL > 0
03731          MOVE CARRI              TO  MSTR-CARRIER.
03732      IF CERTNOL > 0
03733          MOVE CERTNOI            TO  MSTR-CERT-NO-PRIME.
03734      IF SUFXL > 0
03735          MOVE SUFXI              TO  MSTR-CERT-NO-SUFX.
03736
03737      IF PI-PREV-CLAIM = LOW-VALUES
03738          NEXT SENTENCE
03739      ELSE
03740          IF CLMNOL  = 0 AND
03741             CARRL   = 0 AND
03742             CERTNOL = 0 AND
03743             SUFXL   = 0
03744                  MOVE PI-PREV-CLAIM  TO  ELMSTR-KEY.
03745
03746  7500-START-READING.
03747
03748      
      * EXEC CICS HANDLE CONDITION
03749 *        ENDFILE  (8700-END-FILE)
03750 *        NOTFND   (8600-NOT-FOUND)
03751 *    END-EXEC.
      *    MOVE '"$''I                  ! 9 #00012058' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303132303538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03752
03753      MOVE 'MSTR'                 TO  FILE-SWITCH.
03754
03755      
      * EXEC CICS STARTBR
03756 *        DATASET   (PI-CURRENT-FILE)
03757 *        RIDFLD    (ELMSTR-KEY)
03758 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012065' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03759
03760  7500-READ-PREV.
03761
03762      
      * EXEC CICS READPREV
03763 *        DATASET   (PI-CURRENT-FILE)
03764 *        RIDFLD    (ELMSTR-KEY)
03765 *        INTO      (CLAIM-MASTER)
03766 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0IL                  )   #00012072' TO DFHEIV0
           MOVE X'2630494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03767
03768      IF ELMSTR-KEY = PI-PREV-CLAIM
03769          MOVE CL-CLAIM-NO        TO  PI-PREV-CLMNO
03770          GO TO 7500-READ-PREV.
03771
03772      IF PI-COMPANY-CD NOT = CL-COMPANY-CD
03773          GO TO 8700-END-FILE.
03774
03775  7500-PICK-UP-LOGIC.
03776
03777      IF PI-COMPANY-ID  =  'CRI'
03778          IF CL-CLAIM-NO NOT = PI-PREV-CLMNO
03779              GO TO 8700-END-FILE.
03780
03781      IF PI-CARRIER-SECURITY > SPACES
03782          IF CL-CARRIER = PI-CARRIER-SECURITY
03783              NEXT SENTENCE
03784          ELSE
03785              GO TO 7500-READ-PREV.
03786
03787      IF PI-ACCOUNT-SECURITY > SPACES
03788          IF CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY
03789              NEXT SENTENCE
03790          ELSE
03791              GO TO 7500-READ-PREV.
03792
03793      GO TO 1000-READ-CLAIM.
03794                                  EJECT
03795  7600-BROWSING-ASS-BWRD.
03796
03797      COMPUTE PI-ASS-NDX = PI-ASS-NDX - 1.
03798
03799      IF PI-ASS-NDX < +1
03800          IF PI-PF16-LAST
03801              MOVE PI-ORIGINAL-ASS-CERT
03802                                  TO ELMSTR-KEY
03803              MOVE ZEROS          TO PI-DAYS-PAID
03804                                     PI-TOTAL-PAID
03805              MOVE LOW-VALUES     TO PI-ASSOCIATED-CERTS-TABLE
03806                                     PI-LAST-PF-KEY-IND
03807                                     PI-ASSOCIATED-PROCESS-IND
03808              GO TO 7500-START-READING
03809          ELSE
03810              COMPUTE PI-ASS-NDX = PI-ASS-NDX + 1
03811              MOVE 'S'            TO PI-LAST-PF-KEY-IND
03812              MOVE -1             TO ENTERPFL
03813              MOVE ER-0937        TO EMI-ERROR
03814              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03815              GO TO 8200-SEND-DATAONLY.
03816
03817      MOVE PI-PREV-CLAIM          TO ELMSTR-KEY.
03818      MOVE PI-ASS-CERTS (PI-ASS-NDX)
03819                                  TO MSTR-CERT-NO.
03820      MOVE 'MSTR'                 TO FILE-SWITCH.
03821
03822      
      * EXEC CICS HANDLE CONDITION
03823 *        NOTFND   (0450-NOT-FOUND)
03824 *    END-EXEC.
      *    MOVE '"$I                   ! : #00012132' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303132313332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03825
03826      
      * EXEC CICS READ
03827 *        DATASET   (PI-CURRENT-FILE)
03828 *        RIDFLD    (ELMSTR-KEY)
03829 *        INTO      (CLAIM-MASTER)
03830 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00012136' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03831
03832      GO TO 7500-PICK-UP-LOGIC.
03833
03834  7600-EXIT.
03835      EXIT.
03836                                  EJECT
03837  7650-CREATE-ACTQ.
03838
03839      
      * EXEC CICS HANDLE CONDITION
03840 *        NOTFND(7650-CREATE-NEW-ACTQ)
03841 *    END-EXEC.
      *    MOVE '"$I                   ! ; #00012149' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303132313439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03842
03843      MOVE PI-COMPANY-CD          TO ACTQ-COMP-CD.
03844      MOVE PI-CARRIER             TO ACTQ-CARRIER.
03845      MOVE PI-CLAIM-NO            TO ACTQ-CLAIM-NO.
03846      MOVE PI-CERT-NO             TO ACTQ-CERT-NO.
03847
03848      MOVE 'ACTQ'                 TO FILE-SWITCH.
03849      MOVE 'ELACTQ  '             TO FILE-ID.
03850
03851      
      * EXEC CICS READ
03852 *        UPDATE
03853 *        DATASET   ('ELACTQ')
03854 *        SET       (ADDRESS OF ACTIVITY-QUE)
03855 *        RIDFLD    (ELACTQ-KEY)
03856 *    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00012161' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03857
03858      MOVE 'L'                    TO AQ-PENDING-CLAIM-RESTORE.
03859
03860      MOVE +150                   TO AQ-LAST-UPDATED-BY.
03861
03862      MOVE 'ELACTQ  '             TO FILE-ID.
03863
03864      
      * EXEC CICS REWRITE
03865 *        DATASET   ('ELACTQ')
03866 *        FROM      (ACTIVITY-QUE)
03867 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00012174' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03868
03869      GO TO 7650-EXIT.
03870
03871  7650-CREATE-NEW-ACTQ.
03872
03873      
      * EXEC CICS GETMAIN
03874 *        SET       (ADDRESS OF ACTIVITY-QUE)
03875 *        LENGTH    (60)
03876 *        INITIMG   (GETMAIN-SPACE)
03877 *    END-EXEC.
           MOVE 60
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00012183' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03878
03879      MOVE 'AQ'                   TO AQ-RECORD-ID.
03880      MOVE PI-COMPANY-CD          TO AQ-COMPANY-CD.
03881      MOVE PI-CARRIER             TO AQ-CARRIER.
03882      MOVE PI-CLAIM-NO            TO AQ-CLAIM-NO.
03883      MOVE PI-CERT-NO             TO AQ-CERT-NO.
03884
03885      MOVE 'L'                    TO AQ-PENDING-CLAIM-RESTORE.
03886
03887      MOVE +0                     TO AQ-PAYMENT-COUNTER
03888                                     AQ-PMT-UNAPPROVED-COUNT.
03889
03890      MOVE LOW-VALUES             TO AQ-RESEND-DATE
03891                                     AQ-FOLLOWUP-DATE.
03892
03893      MOVE +150                   TO AQ-LAST-UPDATED-BY.
03894
03895      MOVE 'ACTQ'                 TO FILE-SWITCH.
03896
03897      MOVE 'ELACTQ  '             TO FILE-ID.
03898
03899      
      * EXEC CICS WRITE
03900 *        DATASET   ('ELACTQ')
03901 *        FROM      (ACTIVITY-QUE)
03902 *        RIDFLD    (AQ-CONTROL-PRIMARY)
03903 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00012209' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03904
03905  7650-EXIT.
03906      EXIT.
03907                                   EJECT
03908  7700-CREATE-CLAIM.
03909
03910      
      * EXEC CICS HANDLE CONDITION
03911 *        NOTFND   (7700-RETR-RECORD-NOT-FOUND)
03912 *        DUPREC   (7700-DUPLICATE-CLAIM)
03913 *    END-EXEC.
      *    MOVE '"$I%                  ! < #00012220' TO DFHEIV0
           MOVE X'222449252020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303132323230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03914
03915      IF CLMNOL > 0
03916          MOVE CLMNOI             TO PI-CLAIM-NO.
03917
03918      IF CARRL > 0
03919          MOVE CARRI              TO PI-CARRIER.
03920
03921      IF CERTNOL > 0
03922          MOVE CERTNOI            TO PI-CERT-PRIME.
03923
03924      IF SUFXL > 0
03925          MOVE SUFXI              TO PI-CERT-SFX.
03926
03927      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
03928      MOVE PI-CARRIER             TO MSTR-CARRIER.
03929      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
03930      MOVE PI-CERT-NO             TO MSTR-CERT-NO.
03931      MOVE +1                     TO PI-ASS-NDX.
03932
03933  7700-CONT-MOVE.
03934
03935      IF PI-ASS-PROCESSING
03936          MOVE PI-ASS-CERTS (PI-ASS-NDX)
03937                                  TO MSTR-CERT-NO.
03938
03939      MOVE ELMSTR-KEY             TO W-ELRETR-KEY.
03940
03941      
      * EXEC CICS READ
03942 *        DATASET   ('ELRETR')
03943 *        SET       (ADDRESS OF RETRIEVE-MASTER)
03944 *        RIDFLD    (W-ELRETR-KEY)
03945 *    END-EXEC.
           MOVE 'ELRETR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00012251' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ELRETR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RETRIEVE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03946
03947      
      * EXEC CICS GETMAIN
03948 *        SET       (ADDRESS OF CLAIM-MASTER-L)
03949 *        LENGTH    (350)
03950 *        INITIMG   (GETMAIN-SPACE)
03951 *    END-EXEC.
           MOVE 350
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00012257' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CLAIM-MASTER-L TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03952
03953      MOVE RETRIEVE-MASTER        TO CLAIM-MASTER.
03954      MOVE 'CL'                   TO CL-RECORD-ID.
03955      MOVE SAVE-BIN-DATE          TO CL-RESTORED-DT.
03956
03957      
      * EXEC CICS WRITE
03958 *        DATASET   ('ELMSTR')
03959 *        FROM      (CLAIM-MASTER)
03960 *        RIDFLD    (CL-CONTROL-PRIMARY)
03961 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00012267' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 CL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03962
03963      
      * EXEC CICS DELETE
03964 *        FILE      ('ELRETR')
03965 *        RIDFLD    (RL-CONTROL-PRIMARY)
03966 *    END-EXEC.
           MOVE 'ELRETR' TO DFHEIV1
      *    MOVE '&(  R                 &   #00012273' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 RL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03967
03968      IF PI-ASS-PROCESSING
03969          ADD +1                  TO PI-ASS-NDX
03970          IF PI-ASS-NDX < +25
03971                  AND
03972             PI-ASS-CERTS (PI-ASS-NDX) > SPACES
03973              GO TO 7700-CONT-MOVE.
03974
03975      MOVE 'MSTR'                 TO FILE-SWITCH.
03976      MOVE 'ELMSTR'               TO PI-CURRENT-FILE.
03977      MOVE LOW-VALUES             TO PI-RETRIEVED-DATA-IND.
03978      MOVE XCTL-126               TO PI-CALLING-PROGRAM.
03979      MOVE XCTL-132               TO PGM-NAME.
03980      GO TO 9300-XCTL.
03981
03982  7700-RETR-RECORD-NOT-FOUND.
03983
03984      MOVE ER-0929                TO EMI-ERROR.
03985      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03986      MOVE -1                     TO CLMNOL.
03987      MOVE PI-CARRIER             TO CARRO.
03988      MOVE PI-CLAIM-NO            TO CLMNOO.
03989      MOVE PI-CERT-PRIME          TO CERTNOO.
03990      MOVE PI-CERT-SFX            TO SUFXO.
03991      MOVE AL-UABON               TO CLMNOA   CARRA
03992                                     CERTNOA  SUFXA.
03993      GO TO 8100-SEND-INITIAL-MAP.
03994
03995  7700-DUPLICATE-CLAIM.
03996
03997      MOVE ER-0928                TO EMI-ERROR.
03998      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03999      MOVE -1                     TO CLMNOL.
04000      MOVE PI-CARRIER             TO CARRO.
04001      MOVE PI-CLAIM-NO            TO CLMNOO.
04002      MOVE PI-CERT-PRIME          TO CERTNOO.
04003      MOVE PI-CERT-SFX            TO SUFXO.
04004      MOVE AL-UABON               TO CLMNOA   CARRA
04005                                     CERTNOA  SUFXA.
04006      GO TO 8100-SEND-INITIAL-MAP.
04007
04008  7700-EXIT.
04009      EXIT.
04010                                   EJECT
04011  8000-LOAD-ERROR-MESSAGES.
04012      IF EMI-NO-ERRORS
04013          GO TO 8000-EXIT.
04014
04015      IF EMI-NUMBER-OF-LINES = 1
04016          MOVE EMI-LINE1          TO ERRMSG1O
04017          GO TO 8000-EXIT.
04018
04019      MOVE EMI-LINE1              TO ERRMSG1O.
04020
04021  8000-EXIT.
04022      EXIT.
04023
04024  8100-SEND-INITIAL-MAP.
04025
04026      MOVE SAVE-DATE              TO RUNDTEO.
04027      MOVE EIBTIME                TO TIME-IN.
04028      MOVE TIME-OUT               TO RUNTIMEO.
101501     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID        TO USERIDO.
04029      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
04030
04031      IF PI-USES-PAID-TO
04032         MOVE 'PAID  TO  DT :'     TO PDTHRHDO.
04033
04034      IF PI-RETRIEVED-DATA
04035          MOVE 'RETRIEVE MASTER'  TO RETMSTO.
04036
04037      IF PI-COMPANY-ID = 'DMD'
04038          MOVE 'PF11=FORM REF'    TO PFKEY11O
04039          MOVE 'LST PMT AMT:'     TO COVERDO
04040          MOVE 'LST PMT DT :'     TO PRMTYPDO
04041          MOVE 'COVERAGE   :'     TO CRTSTADO.
031714
031714     IF PI-APPROVAL-LEVEL = '4' OR '5'
031714         MOVE AL-UNNOF         TO TOTINTA
031714     END-IF
04042
04043      
      * EXEC CICS SEND
04044 *        MAP      (MAP-NAME)
04045 *        MAPSET   (MAPSET-NAME)
04046 *        FROM     (EL150AO)
04047 *        ERASE
04048 *        CURSOR
04049 *    END-EXEC.
           MOVE LENGTH OF
            EL150AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00012359' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150AO, 
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
           
04050
04051      GO TO 9100-RETURN-TRAN.
04052
           .
04053  8200-SEND-DATAONLY.
04054      MOVE SAVE-DATE              TO RUNDTEO.
04055      MOVE EIBTIME                TO TIME-IN.
04056      MOVE TIME-OUT               TO RUNTIMEO.
101501     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID        TO USERIDO.
04057      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
04058
04059      MOVE PI-CARRIER             TO CARRO.
04060      MOVE PI-CLAIM-NO            TO CLMNOO.
04061      MOVE PI-CERT-PRIME          TO CERTNOO.
04062      MOVE PI-CERT-SFX            TO SUFXO.
04063
04064      IF PI-USES-PAID-TO
04065         MOVE 'PAID  TO  DT :'     TO PDTHRHDO.
04066
04067      IF PI-RETRIEVED-DATA
04068          MOVE 'RETRIEVE MASTER'  TO RETMSTO.
04069
04070      IF PI-COMPANY-ID = 'DMD'
04071          MOVE 'PF11=FORM REF'    TO PFKEY11O
04072          MOVE 'LST PMT AMT:'     TO COVERDO
04073          MOVE 'LST PMT DT :'     TO PRMTYPDO
04074          MOVE 'COVERAGE   :'     TO CRTSTADO.
031714
031714     IF PI-APPROVAL-LEVEL = '4' OR '5'
031714         MOVE AL-UNNOF         TO TOTINTA
031714     END-IF
04075
04076      
      * EXEC CICS SEND
04077 *        MAP      (MAP-NAME)
04078 *        MAPSET   (MAPSET-NAME)
04079 *        FROM     (EL150AO)
04080 *        DATAONLY
04081 *        CURSOR
04082 *    END-EXEC.
           MOVE LENGTH OF
            EL150AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00012399' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150AO, 
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
           
04083
04084      GO TO 9100-RETURN-TRAN.
04085
04086  8250-SEND-WITH-CURSOR.
04087      MOVE SAVE-DATE              TO RUNDTEO.
04088      MOVE EIBTIME                TO TIME-IN.
04089      MOVE TIME-OUT               TO RUNTIMEO.
101501     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID        TO USERIDO.
04090      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
04091
04092      IF PI-USES-PAID-TO
04093         MOVE 'PAID  TO  DT :'     TO PDTHRHDO.
04094
04095      IF PI-COMPANY-ID = 'DMD'
04096          MOVE 'PF11=FORM REF'    TO PFKEY11O
04097          MOVE 'LST PMT AMT:'     TO COVERDO
04098          MOVE 'LST PMT DT :'     TO PRMTYPDO
04099          MOVE 'COVERAGE   :'     TO CRTSTADO.
031714
031714     IF PI-APPROVAL-LEVEL = '4' OR '5'
031714         MOVE AL-UNNOF         TO TOTINTA
031714     END-IF
04100
04101      
      * EXEC CICS SEND
04102 *        MAP      (MAP-NAME)
04103 *        MAPSET   (MAPSET-NAME)
04104 *        FROM     (EL150AO)
04105 *        ERASE
04106 *        CURSOR   (PI-SAVE-CURSOR)
04107 *    END-EXEC.
           MOVE LENGTH OF
            EL150AO
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00012430' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150AO, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 PI-SAVE-CURSOR, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04108
04109      GO TO 9100-RETURN-TRAN.
04110
04111  8300-SEND-TEXT.
04112      
      * EXEC CICS SEND TEXT
04113 *        FROM     (LOGOFF-TEXT)
04114 *        LENGTH   (LOGOFF-LENGTH)
04115 *        ERASE
04116 *        FREEKB
04117 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00012441' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343431' TO DFHEIV0(25:11)
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
           
04118
04119      
      * EXEC CICS RETURN
04120 *    END-EXEC.
      *    MOVE '.(                    ''   #00012448' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04121
04122  8500-FILE-NOTOPEN.
04123      IF FILE-SWITCH = 'MSTR'
04124          MOVE ER-0154            TO EMI-ERROR.
04125
04126      IF FILE-SWITCH = 'TRLR'
04127          MOVE ER-0172            TO EMI-ERROR.
04128
04129      IF FILE-SWITCH = 'CERT'
04130          MOVE ER-0169            TO EMI-ERROR.
04131
04132      IF FILE-SWITCH = 'CNTL'
04133          MOVE ER-0042            TO EMI-ERROR.
04134
04135      IF FILE-SWITCH = 'ACTQ'
04136          MOVE ER-0338            TO EMI-ERROR.
04137
04138      IF FILE-SWITCH = 'ACCT'
04139          MOVE ER-0168            TO EMI-ERROR.
04140
04141      IF FILE-SWITCH = 'PLCY'
04142          MOVE ER-9883            TO EMI-ERROR.
04143
04144      IF FILE-SWITCH = 'PLAN'
04145          MOVE ER-9808            TO EMI-ERROR.
04146
04147      IF FILE-SWITCH = 'PROD'
04148          MOVE ER-9886            TO EMI-ERROR.
04149
04150      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04151
030612     IF FILE-SWITCH = 'CERT' AND
062121       (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
04153         GO TO 1050-SHOW-CONTINUE.
04154
04155      MOVE -1                     TO FILETOL.
04156
04157      IF PASS-SWITCH = 'A'
04158          GO TO 8100-SEND-INITIAL-MAP
04159      ELSE
04160          GO TO 8200-SEND-DATAONLY.
04161
04162  8600-NOT-FOUND.
04163      MOVE AL-UABON               TO CLMNOA    CARRA
04164                                     CERTNOA   SUFXA.
04165      MOVE -1                     TO ENTERPFL.
04166      MOVE ER-0204                TO EMI-ERROR.
04167      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04168      GO TO 8200-SEND-DATAONLY.
04169
04170  8700-END-FILE.
04171
04172      MOVE -1                     TO ENTERPFL.
04173      MOVE ER-0130                TO EMI-ERROR.
04174      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04175      GO TO 8200-SEND-DATAONLY.
04176
04177  8800-UNAUTHORIZED-ACCESS.
04178      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
04179      GO TO 8300-SEND-TEXT.
04180
04181  8810-PF23.
04182      MOVE EIBAID                 TO PI-ENTRY-CD-1.
04183      MOVE XCTL-005               TO PGM-NAME.
04184      GO TO 9300-XCTL.
04185
04186  8900-GET-FREE-LOOK.
04187      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
04188      MOVE SPACES                 TO CNTL-ACCESS.
04189      MOVE PI-STATE               TO CNTL-ACCESS.
04190      MOVE '3'                    TO CNTL-REC-TYPE.
04191      MOVE +0                     TO CNTL-SEQ-NO.
04192
04193      
      * EXEC CICS READ
04194 *        DATASET   ('ELCNTL')
04195 *        SET       (ADDRESS OF CONTROL-FILE)
04196 *        RIDFLD    (ELCNTL-KEY)
04197 *        RESP      (WS-RESPONSE)
04198 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00012523' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303132353233' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04199
04200      IF WS-RESP-NOTFND
04201         MOVE ER-2848             TO EMI-ERROR
04202         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04203         GO TO 8100-SEND-INITIAL-MAP
04204      ELSE
04205         MOVE CF-ST-FREE-LOOK-PERIOD
04206                                  TO CP-FREE-LOOK.
04207
04208  8900-EXIT.
04209      EXIT.
04210
04211  9100-RETURN-TRAN.
04212      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
04213      MOVE '150A'                 TO PI-CURRENT-SCREEN-NO.
04214
04215      
      * EXEC CICS RETURN
04216 *        TRANSID    (TRANS-ID)
04217 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
04218 *        LENGTH     (PI-COMM-LENGTH)
04219 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00012545' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303132353435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04220
04221  9200-RETURN-MAIN-MENU.
04222      MOVE XCTL-126               TO PGM-NAME.
04223      GO TO 9300-XCTL.
04224
04225  9300-XCTL.
04226      
      * EXEC CICS XCTL
04227 *        PROGRAM    (PGM-NAME)
04228 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
04229 *        LENGTH     (PI-COMM-LENGTH)
04230 *    END-EXEC.
      *    MOVE '.$C                   %   #00012556' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303132353536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04231
04232  9400-CLEAR.
04233      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
04234      GO TO 9300-XCTL.
04235
04236  9500-PF12.
04237      MOVE XCTL-010               TO PGM-NAME.
04238      GO TO 9300-XCTL.
04239
04240  9600-PGMID-ERROR.
04241      
      * EXEC CICS HANDLE CONDITION
04242 *        PGMIDERR   (8300-SEND-TEXT)
04243 *    END-EXEC.
      *    MOVE '"$L                   ! = #00012571' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303132353731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04244
04245      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
04246      MOVE ' '                    TO PI-ENTRY-CD-1.
04247      MOVE XCTL-005               TO PGM-NAME.
04248      MOVE PGM-NAME               TO LOGOFF-PGM.
04249      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
04250      GO TO 9300-XCTL.
04251
04252  9700-LINK-DATE-CONVERT.
04253      MOVE LINK-ELDATCV           TO PGM-NAME.
04254
04255      
      * EXEC CICS LINK
04256 *        PROGRAM    (PGM-NAME)
04257 *        COMMAREA   (DATE-CONVERSION-DATA)
04258 *        LENGTH     (DC-COMM-LENGTH)
04259 *    END-EXEC.
      *    MOVE '."C                   (   #00012585' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132353835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04260
04261  9700-EXIT.
04262      EXIT.
04263
04264  9800-LINK-REM-TERM.
04265      MOVE LINK-ELRTRM            TO PGM-NAME.
04266
04267      
      * EXEC CICS LINK
04268 *        PROGRAM    (PGM-NAME)
04269 *        COMMAREA   (CALCULATION-PASS-AREA)
04270 *        LENGTH     (CP-COMM-LENGTH)
04271 *    END-EXEC.
      *    MOVE '."C                   (   #00012597' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132353937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04272
04273  9800-EXIT.
04274      EXIT.
04275                                  EJECT
04276  9830-DMD-REMAINING-TERM.
04277
04278      DIVIDE CL-NO-OF-DAYS-PAID BY +30
04279          GIVING W-PAID-MONTHS REMAINDER W-PAID-DAYS.
04280
04281      MOVE W-PAID-MONTHS          TO W-PYMTS.
04282      MOVE W-PAID-DAYS            TO W-ADD-DAYS.
04283
04284      COMPUTE W-TERM-IN-DAYS = CM-AH-ORIG-TERM * 30
04285                             + CM-PMT-EXTENSION-DAYS.
04286
04287      COMPUTE W-REM-TERM-IN-DAYS = W-TERM-IN-DAYS
04288                                 - CL-NO-OF-DAYS-PAID.
04289
04290      DIVIDE W-REM-TERM-IN-DAYS BY +30
04291          GIVING W-REM REMAINDER W-REM-DAYS.
04292
04293      MOVE W-REM                  TO WST-REM.
04294
04295      IF CM-PMT-EXTENSION-DAYS > ZEROS
04296          MOVE '/'                TO WST-SLASH1
04297          MOVE CM-PMT-EXTENSION-DAYS TO WST-EXT-DAYS
04298      ELSE
04299          MOVE SPACES             TO WST-ORIG-DAYS-GRP.
04300
04301      IF W-REM-DAYS > ZEROS
04302          MOVE '/'                TO WST-SLASH2
04303          MOVE W-REM-DAYS         TO WST-REM-DAYS
04304      ELSE
04305          MOVE SPACES             TO WST-REM-DAYS-GRP.
04306
04307  9830-EXIT.
04308      EXIT.
04309
04310  9850-LINK-REM-AMT.
04311      MOVE 'ELRAMT  '             TO PGM-NAME.
04312
04313      
      * EXEC CICS LINK
04314 *        PROGRAM    (PGM-NAME)
04315 *        COMMAREA   (CALCULATION-PASS-AREA)
04316 *        LENGTH     (CP-COMM-LENGTH)
04317 *    END-EXEC.
      *    MOVE '."C                   (   #00012643' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04318
04319  9850-EXIT.
04320      EXIT.
04321
04322  9900-ERROR-FORMAT.
04323      IF NOT EMI-ERRORS-COMPLETE
04324          MOVE LINK-001           TO PGM-NAME
04325          
      * EXEC CICS LINK
04326 *            PROGRAM    (PGM-NAME)
04327 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
04328 *            LENGTH     (EMI-COMM-LENGTH)
04329 *        END-EXEC.
      *    MOVE '."C                   (   #00012655' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04330
04331  9900-EXIT.
04332      EXIT.
04333
04334  9990-ABEND.
04335      MOVE -1 TO FILETOL.
04336      MOVE LINK-004 TO PGM-NAME.
04337
04338      MOVE DFHEIBLK               TO EMI-LINE1
04339      
      * EXEC CICS LINK
04340 *        PROGRAM   (PGM-NAME)
04341 *        COMMAREA  (EMI-LINE1)
04342 *        LENGTH    (72)
04343 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00012669' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04344
04345      MOVE EMI-LINE1              TO ERRMSG1O.
04346      GO TO 8200-SEND-DATAONLY.
04347
04348  EJECT
04349  9995-SECURITY-VIOLATION.
04350 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00012697' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363937' TO DFHEIV0(25:11)
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
04351

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL150' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 1000-SHOW-CLAIM,
                     0100-FIRST-TIME-IN,
                     8500-FILE-NOTOPEN,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0450-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0360-CREATE-NEW-ACTQ
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0385-TERMID-ERROR,
                     0390-TRANS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0450-NOT-FOUND,
                     0440-UPDATE-DONE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 0690-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1100-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1005-END-BUILD-DIAGNOSIS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 1100-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 1005-NO-ERACCT,
                     1005-NO-ERACCT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 1008-NOTFND,
                     1008-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 1009-CONTINUE-BUILD,
                     1009-CONTINUE-BUILD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 1050-SHOW-CONTINUE,
                     1050-SHOW-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 1220-NO-CLAIM-RECORD-FND,
                     1200-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 1500-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 1500-NO-PLAN-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 1700-ENDBROWSE,
                     1700-ENDBROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 2950-NO-MORE-TRAILERS,
                     2950-NO-MORE-TRAILERS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 6000-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 6100-END-RESET,
                     6100-END-RESET
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 6200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 8700-END-FILE,
                     8600-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 0450-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 8700-END-FILE,
                     8600-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 0450-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 7650-CREATE-NEW-ACTQ
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 7700-RETR-RECORD-NOT-FOUND,
                     7700-DUPLICATE-CLAIM
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL150' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
