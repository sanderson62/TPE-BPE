00001  IDENTIFICATION DIVISION.                                         06/06/97
00002                                                                   EL160
00003  PROGRAM-ID.                 EL160 .                                 LV018
00004 *              PROGRAM CONVERTED BY                                  CL*17
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*17
00006 *              CONVERSION DATE 05/16/95 15:27:49.                    CL*17
00007 *                            VMOD=2.018.                             CL*18
00008 *                                                                 EL160
00008 *                                                                 EL160
00009 *AUTHOR.        LOGIC, INC.                                          CL*17
00010 *               DALLAS, TEXAS.                                       CL*17
00011                                                                   EL160
00024 *REMARKS. TRANSACTION EX33 - CLAIM AUDIT.                            CL**3
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00025                                                                   EL160
00026      EJECT                                                        EL160
00027  ENVIRONMENT DIVISION.                                            EL160
00028                                                                   EL160
00029  DATA DIVISION.                                                   EL160
00030                                                                   EL160
00031  WORKING-STORAGE SECTION.                                         EL160
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL160
00033  77  FILLER  PIC X(32)  VALUE '*   EL160  WORKING STORAGE     *'. EL160
00034  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.018 **********'.    CL*18
00035                                                                      CL*17
00036  01  LCP-TIME-OF-DAY-XX.                                             CL*17
00037      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL*17
00038      05  FILLER                    PIC 99.                           CL*17
00039  01  LCP-CICS-TIME                 PIC 9(15).                        CL*17
00040                                                                   EL160
00041      COPY ELCSCTM.                                                   CL*12
00042                                                                   EL160
00043      COPY ELCSCRTY.                                                  CL*12
00044                                                                   EL160
00045  01  WS-DATE-AREA.                                                EL160
00046      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL160
00047      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL160
00048                                                                   EL160
00049  01  LITERALS-NUMBERS.                                            EL160
00050      12  SC-ITEM                 PIC S9(4)   VALUE +0001  COMP.   EL160
00051      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.       EL160
00052      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.       EL160
00053      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.       EL160
00054      12  XCTL-EL1602             PIC X(8)    VALUE 'EL1602'.      EL160
00055      12  THIS-PGM                PIC X(8)    VALUE 'EL160'.       EL160
00056      12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.     EL160
00057      12  REM-TERM-PGM            PIC X(8)    VALUE 'ELRTRM '.     EL160
00058      12  THIS-TRAN               PIC X(4)    VALUE 'EX33'.        EL160
00059      12  LIT-MAP                 PIC X(4)    VALUE '160A'.        EL160
00060      12  LIT-MAP-2               PIC X(4)    VALUE '160B'.        EL160
00061      12  MAX-TS-PAGES            PIC 9999    VALUE 250.              CL**7
00062      12  ALL-NINES               PIC 9(7)V99  VALUE 9999999.99.   EL160
00063                                                                   EL160
00064  01  EDIT-WORK-AREA.                                              EL160
00065      12  WS-SEX-SELECTION        PIC X VALUE SPACE.                  CL*10
00066          88  MALE-SELECTION       VALUE 'M'.                         CL*10
00067          88  FEMALE-SELECTION     VALUE 'F'.                         CL*10
00068      12  CALL-PGM                PIC X(8).                        EL160
00069      12  TRANS-ID                PIC X(4).                        EL160
00070      12  CHECK-PFKEYS            PIC 99.                          EL160
00071      12  TEST-RESP               PIC X.                           EL160
00072      12  DAYS-PAID               PIC ZZZZ9.                       EL160
00073      12  PMTS-MADE               PIC ZZZZZ9.                      EL160
00074      12  EDIT-DOLLARS-8          PIC ZZZZZ.99.                    EL160
00075      12  EDIT-DOLLARS-9          PIC ZZZZZZ.99.                   EL160
00076      12  EDIT-APR                PIC ZZ9.9999.                    EL160
00077      12  COUNT-2                 PIC 99.                          EL160
00078      12  HOLD-BENEFIT            PIC XX.                             CL**3
00079                                                                      CL*18
00080      12  WS-RESPONSE             PIC S9(8) COMP.                     CL*18
00081          88  WS-RESP-NORMAL                VALUE +00.                CL*18
00082          88  WS-RESP-NOTFND                VALUE +13.                CL*18
00083                                                                   EL160
00084      12  TEST-DATE.                                               EL160
00085          16  FILLER              PIC XX.                          EL160
00086          16  BIF-DATE            PIC X(6).                        EL160
00087                                                                   EL160
00088      12  TEST-AMT.                                                EL160
00089          16  FILLER              PIC X.                           EL160
00090          16  BIF-AMT             PIC X(9).                        EL160
00091          16  AMT-BIF REDEFINES BIF-AMT PIC 9(7)V99.               EL160
00092                                                                      CL*17
00093      12  W-FILE-ID               PIC X(8) VALUE 'ELMSTR'.            CL*17
00094      12  W-VALID-FILE-IND        PIC X(8).                           CL*17
00095          88  W-VALID-FILE                  VALUE ' ' 'M' 'R'.        CL*17
00096          88  W-RETRIEVE                    VALUE 'R'.                CL*17
00097          88  W-MASTER                      VALUE ' ' 'M'.            CL*17
00098                                                                   EL160
00099      12  WORK-DATE-MDY.                                           EL160
00100          16  MONTH-WORK          PIC XX.                          EL160
00101          16  FILLER              PIC X(4).                        EL160
00102          16  YEAR-WORK           PIC XX.                          EL160
00103                                                                   EL160
00104      12  WORK-DATE-MY.                                            EL160
00105          16  WORK-MONTH          PIC XX.                          EL160
00106          16  FILLER              PIC X       VALUE '/'.           EL160
00107          16  WORK-YEAR           PIC XX.                          EL160
00108                                                                   EL160
00109      12  HOLD-TERM-REM.                                           EL160
00110          16  HOLD-ORIG-TERM      PIC ZZ9.                         EL160
00111          16  FILLER              PIC X       VALUE '/'.           EL160
00112          16  HOLD-REM            PIC ZZ9.                         EL160
00113                                                                   EL160
00114      12  CURRENT-DATE-BIN        PIC X(2).                        EL160
00115                                                                   EL160
00116      12  WS-FORM-SAVE            PIC X(12).                       EL160
00117                                                                      CL*16
00118      12  WS-AGE                  PIC 9(04).                          CL*16
00119      12  WS-AGE-R REDEFINES WS-AGE.                                  CL*16
00120          16  WS-AGE-1-2          PIC 9(02).                          CL*16
00121          16  WS-AGE-3-4          PIC 9(02).                          CL*16
00122                                                                      CL**9
00123      12  WS-PAID-TO-HDG          PIC X(29)   VALUE                   CL**9
00124          'CAUSE CD  EST. END   PAID  TO'.                            CL**9
00125                                                                   EL160
00126  01  CNTL-WORK-AREA.                                              EL160
00127      12  CARRIER-CNTL            PIC X.                           EL160
00128      12  INC-DATE-LOW-CNTL       PIC XX.                          EL160
00129      12  INC-DATE-HIGH-CNTL      PIC XX.                          EL160
00130      12  GROUP-CNTL              PIC X(06).                       EL160
00131      12  LST-PMT-LOW-CNTL        PIC XX.                          EL160
00132      12  LST-PMT-HIGH-CNTL       PIC XX.                          EL160
00133      12  STATE-CNTL              PIC XX.                          EL160
00134      12  MO-OPEN-LOW-CNTL        PIC S9(4)       COMP.            EL160
00135      12  MO-OPEN-HIGH-CNTL       PIC S9(4)       COMP.            EL160
00136      12  ACCOUNT-CNTL            PIC X(10).                       EL160
00137      12  AMT-PAID-LOW-CNTL       PIC S9(7)V99    COMP-3.          EL160
00138      12  AMT-PAID-HIGH-CNTL      PIC S9(7)V99   COMP-3.           EL160
00139      12  TYPE-CNTL               PIC X.                           EL160
00140      12  CAUSE-CD-LOW-CNTL       PIC X(6).                        EL160
00141      12  CAUSE-CD-HIGH-CNTL      PIC X(6).                        EL160
00142      12  DEN-CNTL                PIC X.                           EL160
00143      12  REP-DATE-LOW-CNTL       PIC XX.                          EL160
00144      12  REP-DATE-HIGH-CNTL      PIC XX.                          EL160
00145      12  PROC-CNTL               PIC X(4).                        EL160
00146      12  LST-PAID-LOW-CNTL       PIC S9(7)V99   COMP-3.           EL160
00147      12  LST-PAID-HIGH-CNTL      PIC S9(7)V99   COMP-3.           EL160
00148      12  PREM-CNTL               PIC X.                           EL160
00149      12  MNT-DATE-LOW-CNTL       PIC XX.                          EL160
00150      12  MNT-DATE-HIGH-CNTL      PIC XX.                          EL160
00151      12  REQ-CNTL                PIC X.                           EL160
00152      12  EST-DATE-LOW-CNTL       PIC XX.                          EL160
00153      12  EST-DATE-HIGH-CNTL      PIC XX.                          EL160
00154      12  SUPR-CNTL               PIC X.                           EL160
00155      12  FOL-DATE-LOW-CNTL       PIC XX.                          EL160
00156      12  FOL-DATE-HIGH-CNTL      PIC XX.                          EL160
00157      12  CERT-CNTL               PIC X.                           EL160
00158      12  DAYS-LOW-CNTL           PIC S9(4)       COMP.            EL160
00159      12  DAYS-HIGH-CNTL          PIC S9(4)       COMP.            EL160
00160      12  PRI-CNTL                PIC X.                           EL160
00161      12  AUTO-CNTL               PIC X.                           EL160
00162      12  OPCL-CNTL               PIC X.                           EL160
00163                                                                   EL160
00164  01  TIME-IN.                                                     EL160
00165      12  UN-HOURS                PIC XX.                          EL160
00166      12  UN-MINUTES              PIC XX.                          EL160
00167      12  FILLER                  PIC X(4).                        EL160
00168                                                                   EL160
00169  01  TIME-OUT.                                                    EL160
00170      12  FOR-HOURS               PIC XX.                          EL160
00171      12  FILLER                  PIC X       VALUE '.'.           EL160
00172      12  FOR-MINUTES             PIC XX.                          EL160
00173                                                                   EL160
00174  01  ERROR-NUMBERS.                                               EL160
00175      12  ER-0008                 PIC X(4)    VALUE '0008'.        EL160
00176      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL160
00177      12  ER-0042                 PIC X(4)    VALUE '0042'.        EL160
00178      12  ER-0046                 PIC X(4)    VALUE '0046'.        EL160
00179      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL160
00180      12  ER-0142                 PIC X(4)    VALUE '0142'.        EL160
00181      12  ER-0143                 PIC X(4)    VALUE '0143'.        EL160
00182      12  ER-0154                 PIC X(4)    VALUE '0154'.        EL160
00183      12  ER-0169                 PIC X(4)    VALUE '0169'.        EL160
00184      12  ER-0172                 PIC X(4)    VALUE '0172'.        EL160
00185      12  ER-0192                 PIC X(4)    VALUE '0192'.        EL160
00186      12  ER-0199                 PIC X(4)    VALUE '0199'.        EL160
00187      12  ER-0205                 PIC X(4)    VALUE '0205'.        EL160
00188      12  ER-0206                 PIC X(4)    VALUE '0206'.        EL160
00189      12  ER-0219                 PIC X(4)    VALUE '0219'.           CL*10
00190      12  ER-0227                 PIC X(4)    VALUE '0227'.        EL160
00191      12  ER-0273                 PIC X(4)    VALUE '0273'.        EL160
00192      12  ER-0274                 PIC X(4)    VALUE '0274'.        EL160
00193      12  ER-0282                 PIC X(4)    VALUE '0282'.        EL160
00194      12  ER-0283                 PIC X(4)    VALUE '0283'.        EL160
00195      12  ER-0304                 PIC X(4)    VALUE '0304'.        EL160
00196      12  ER-0306                 PIC X(4)    VALUE '0306'.        EL160
00197      12  ER-0307                 PIC X(4)    VALUE '0307'.        EL160
00198      12  ER-0308                 PIC X(4)    VALUE '0308'.        EL160
00199      12  ER-0334                 PIC X(4)    VALUE '0334'.        EL160
00200      12  ER-0335                 PIC X(4)    VALUE '0335'.        EL160
00201      12  ER-0419                 PIC X(4)    VALUE '0419'.        EL160
00202      12  ER-0767                 PIC X(4)    VALUE '0767'.           CL*14
00203      12  ER-0970                 PIC X(4)    VALUE '0970'.           CL*17
00204      12  ER-2381                 PIC X(4)    VALUE '2381'.        EL160
00205      12  ER-2848                 PIC X(4)    VALUE '2848'.           CL*18
00206      12  ER-9483                 PIC X(4)    VALUE '9483'.           CL*16
00207      12  ER-9811                 PIC X(4)    VALUE '9811'.           CL*16
00208                                                                   EL160
00209  01  HOLD-KEY.                                                    EL160
00210      12  HOLD-TERM               PIC X(4).                        EL160
00211      12  KEY-QUAL                PIC X(4).                        EL160
00212                                                                   EL160
00213  01  ERROR-SWITCHES.                                              EL160
00214      12  ERROR-SWITCH            PIC X.                           EL160
00215          88  SCREEN-ERROR                    VALUE 'X'.           EL160
00216                                                                   EL160
00217      12  BUILD-SWITCH            PIC X.                           EL160
00218          88  NO-RECORDS                      VALUE 'X'.           EL160
00219          88  BUILD-COMPLETE                  VALUE 'Y'.           EL160
00220          88  SCREEN-HAS-ERRORS               VALUE 'X'.           EL160
00221                                                                   EL160
00222      12  PROC-SWITCH             PIC X.                           EL160
00223          88  PROC-SELECTED                   VALUE 'X'.           EL160
00224                                                                   EL160
00225  01  MSTR-KEY.                                                    EL160
00226      12  MSTR-COMPANY-CODE       PIC X.                           EL160
00227      12  MSTR-CARRIER            PIC X.                           EL160
00228      12  REST-OF-KEY             PIC X(18).                       EL160
00229                                                                   EL160
00230  01  MSTR-KEY-4.                                                  EL160
00231      12  MSTR-COMPANY-CODE-4     PIC X.                           EL160
00232      12  MSTR-USER-ID-4          PIC X(4).                        EL160
00233                                                                   EL160
00234  01  CERT-KEY.                                                    EL160
00235      12  CERT-COMPANY-CODE       PIC X.                           EL160
00236      12  CERT-CARRIER            PIC X.                           EL160
00237      12  CERT-GROUP              PIC X(6).                        EL160
00238      12  CERT-STATE              PIC XX.                          EL160
00239      12  CERT-ACCOUNT            PIC X(10).                       EL160
00240      12  CERT-DATE               PIC XX.                          EL160
00241      12  CERT-CERT               PIC X(11).                       EL160
00242                                                                   EL160
00243  01  TRLR-KEY.                                                    EL160
00244      12  TRLR-MAIN-KEY           PIC X(20).                       EL160
00245      12  TRLR-SEQ-NO             PIC 9(4)    COMP.                EL160
00246                                                                   EL160
00247  01  CNTL-KEY.                                                    EL160
00248      12  COMPANY-ID              PIC X(3).                        EL160
00249      12  RECORD-TYPE             PIC X.                           EL160
00250      12  ACCESS-CD-GENL          PIC X(4).                        EL160
00251      12  SEQUENCE-NO             PIC 9(4)    COMP.                EL160
00252                                                                   EL160
00253  01  BENEFIT-KEY.                                                 EL160
00254      12  BEN-CO-ID               PIC X(3).                        EL160
00255      12  BEN-REC-TYPE            PIC X.                           EL160
00256      12  FILLER                  PIC XX.                          EL160
00257      12  BEN-ACC-CD              PIC XX.                          EL160
00258      12  BEN-SEQ-NO              PIC S9(4)   COMP.                EL160
00259                                                                      CL*16
00260  01  EMPLCY-KEY.                                                     CL*16
00261      12  EMPLCY-COMPANY-CD       PIC X(01).                          CL*16
00262      12  EMPLCY-CARRIER          PIC X(01).                          CL*16
00263      12  EMPLCY-GROUPING         PIC X(06).                          CL*16
00264      12  EMPLCY-STATE            PIC X(02).                          CL*16
00265      12  EMPLCY-PRODUCER         PIC X(10).                          CL*16
00266      12  EMPLCY-EFF-DT           PIC X(02).                          CL*16
00267      12  EMPLCY-REFERENCE-NO     PIC X(20).                          CL*16
00268                                                                      CL*16
00269  01  EMPLAN-KEY.                                                     CL*16
00270      12  EMPLAN-COMPANY-CD       PIC X(01).                          CL*16
00271      12  EMPLAN-CARRIER          PIC X(01).                          CL*16
00272      12  EMPLAN-GROUPING         PIC X(06).                          CL*16
00273      12  EMPLAN-STATE            PIC X(02).                          CL*16
00274      12  EMPLAN-PRODUCER         PIC X(10).                          CL*16
00275      12  EMPLAN-PLAN-CODE        PIC X(02).                          CL*16
00276      12  EMPLAN-REV-NO           PIC 9(03).                          CL*16
00277                                                                   EL160
00278  01  COMP-LENGTHS.                                                EL160
00279      12  COUNT-1                 PIC S9(4)   COMP.                EL160
00280      12  EL160A-LENGTH           PIC S9(4)   COMP VALUE +515.        CL*17
00281      12  EL160B-LENGTH           PIC S9(4)   COMP VALUE +881.        CL*17
00282      12  DATE-LENGTH             PIC S9(4)   COMP VALUE +8.       EL160
00283      12  MO-DAY-LENGTH           PIC S9(4)   COMP VALUE +3.       EL160
00284      12  AMT-LENGTH              PIC S9(4)   COMP VALUE +10.      EL160
00285                                                                   EL160
00286      EJECT                                                        EL160
00287      COPY ELCINTF.                                                   CL*12
00288                                                                   EL160
00289      12  EL160-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.          EL160
00290          16  PI-TS-COUNT         PIC S9(4)   COMP.                EL160
00291          16  PI-TS-COUNT-1       PIC S9(4)   COMP.                EL160
00292          16  PI-EL160-KEY        PIC X(8).                        EL160
00293          16  PI-EL1602-KEY       PIC X(8).                        EL160
00294          16  PI-PRINT-OPTION     PIC X.                           EL160
00295          16  PI-FORMAT-OPTION    PIC X.                           EL160
00296          16  PI-PRINT-ID         PIC X(4).                        EL160
00297          16  PI-ALT-PRINT-ID     PIC X(4).                        EL160
00298          16  PI-FILE-ID-IND      PIC X.                              CL*17
00299              88  PI-RETRIEVAL-FILE           VALUE 'R'.              CL*17
00300              88  PI-MASTER-FILE              VALUE 'M'.              CL*17
00301          16  FILLER              PIC X(609).                         CL*17
00302                                                                   EL160
00303      EJECT                                                        EL160
00304      COPY ELCLOGOF SUPPRESS.                                         CL*12
00305                                                                   EL160
00306      COPY ELCCALC.                                                   CL*12
00307      EJECT                                                        EL160
00308      COPY EL160S.                                                    CL*12
00309      EJECT                                                           CL*17
00310      COPY ELCMSTR.                                                   CL*17
00311      EJECT                                                        EL160
00312      COPY ELCDATE.                                                   CL*12
00313      EJECT                                                        EL160
00314      COPY ELCATTR.                                                   CL*12
00315                                                                   EL160
00316      COPY ELCAID.                                                    CL*12
00317                                                                   EL160
00318  01  FILLER REDEFINES DFHAID.                                     EL160
00319      12  FILLER                  PIC X(8).                        EL160
00320      12  AID-KEYS OCCURS 24 TIMES.                                EL160
00321          16  FILLER              PIC X.                           EL160
00322                                                                   EL160
00323      COPY ELCEMIB.                                                   CL*12
00324                                                                   EL160
00325      EJECT                                                        EL160
00326  LINKAGE SECTION.                                                 EL160
00327                                                                   EL160
00328  01  DFHCOMMAREA                 PIC X(1024).                     EL160
00329                                                                   EL160
00330  01  CLAIM-MASTER-L              PIC X(350).                         CL*17
00331                                                                   EL160
00332      EJECT                                                        EL160
00333      COPY ELCCERT.                                                   CL*12
00334      EJECT                                                        EL160
00335      COPY ELCTRLR.                                                   CL*12
00336      EJECT                                                        EL160
00337      COPY ELCCNTL.                                                   CL*12
00338      EJECT                                                           CL*16
00339      COPY MPCPLCY.                                                   CL*16
00340      EJECT                                                           CL*16
00341      COPY MPCPLAN.                                                   CL*16
00342      EJECT                                                        EL160
00343      COPY ELCRETR.                                                   CL*17
00344      EJECT                                                           CL*17
00345  PROCEDURE DIVISION.                                              EL160
00346                                                                   EL160
00347      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL160
00348      MOVE '5'                   TO DC-OPTION-CODE.                EL160
00349      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                    EL160
00350      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL160
00351      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL160
00352                                                                   EL160
00353      IF EIBCALEN = ZERO                                           EL160
00354          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL160
00355                                                                   EL160
00356      EXEC CICS HANDLE CONDITION                                   EL160
00357          PGMIDERR (8820-XCTL-ERROR)                               EL160
00358          ERROR    (9990-ABEND)                                    EL160
00359      END-EXEC.                                                    EL160
00360                                                                   EL160
00361      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL160
00362                                                                   EL160
00363      MOVE 2                      TO EMI-NUMBER-OF-LINES.          EL160
00364                                                                   EL160
00365      MOVE THIS-TRAN              TO TRANS-ID.                     EL160
00366                                                                   EL160
00367      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL160
00368          MOVE LOW-VALUES TO EL160AO                               EL160
00369          MOVE ER-0008    TO EMI-ERROR                             EL160
00370          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00371          MOVE -1         TO CARRSL                                   CL*17
00372          GO TO 8110-SEND-DATA.                                    EL160
00373                                                                   EL160
00374      IF THIS-PGM NOT = PI-CALLING-PROGRAM                         EL160
00375          MOVE LOW-VALUES TO EL160AO                               EL160
00376          PERFORM 0500-BUILD-TS-KEY THRU 0510-EXIT                 EL160
00377          GO TO 0100-UPDATE-PI.                                    EL160
00378                                                                   EL160
00379      IF EIBAID = DFHCLEAR                                         EL160
00380          GO TO 8200-RETURN-PRIOR.                                 EL160
00381                                                                   EL160
00382      IF PI-PROCESSOR-ID = 'LGXX'                                  EL160
00383          NEXT SENTENCE                                            EL160
00384      ELSE                                                         EL160
00385          EXEC CICS READQ TS                                       EL160
00386              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL160
00387              INTO    (SECURITY-CONTROL)                           EL160
00388              LENGTH  (SC-COMM-LENGTH)                             EL160
00389              ITEM    (SC-ITEM)                                    EL160
00390          END-EXEC                                                 EL160
00391          MOVE SC-CLAIMS-DISPLAY (3)    TO  PI-DISPLAY-CAP         EL160
00392          MOVE SC-CLAIMS-UPDATE  (3)    TO  PI-MODIFY-CAP          EL160
00393          IF NOT DISPLAY-CAP                                       EL160
00394              MOVE 'READ'               TO  SM-READ                EL160
00395              PERFORM 9995-SECURITY-VIOLATION                      EL160
00396              MOVE ER-0070              TO  EMI-ERROR              EL160
00397              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL160
00398              MOVE -1                   TO CARRSL                     CL*17
00399              GO TO 8100-SEND-MAP.                                 EL160
00400                                                                   EL160
00401      EXEC CICS RECEIVE                                            EL160
00402          MAP    ('EL160A')                                        EL160
00403          MAPSET ('EL160S')                                        EL160
00404      END-EXEC.                                                    EL160
00405                                                                   EL160
00406      IF PFKEYL > ZERO                                             EL160
00407          PERFORM 0200-TRANS-PF THRU 0210-EXIT.                    EL160
00408                                                                   EL160
00409      IF SCREEN-ERROR                                              EL160
00410          MOVE ER-0008 TO EMI-ERROR                                EL160
00411          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00412          MOVE -1 TO CARRSL                                           CL*17
00413          GO TO 8110-SEND-DATA.                                    EL160
00414                                                                   EL160
00415      IF EIBAID = DFHPF12                                          EL160
00416          GO TO 8300-GET-HELP.                                     EL160
00417                                                                   EL160
00418      IF EIBAID = DFHPF23                                          EL160
00419          GO TO 8810-PF23-ENTERED.                                 EL160
00420                                                                   EL160
00421      IF EIBAID = DFHPF24                                          EL160
00422          GO TO 8400-RETURN-MASTER.                                EL160
00423                                                                   EL160
00424      IF EIBAID NOT = DFHENTER                                     EL160
00425          MOVE ER-0029 TO EMI-ERROR                                EL160
00426          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00427          MOVE -1 TO CARRSL                                        EL160
00428          GO TO 8110-SEND-DATA.                                    EL160
00429                                                                   EL160
00430      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.                     EL160
00431                                                                   EL160
00432      IF SCREEN-HAS-ERRORS                                         EL160
00433          GO TO 8110-SEND-DATA.                                    EL160
00434                                                                   EL160
00435      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                           CL**7
00436          MOVE 2500               TO  MAX-TS-PAGES.                   CL*15
00437                                                                      CL*15
00438      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')       CL*16
00439          MOVE 2500               TO  MAX-TS-PAGES.                   CL**7
00440                                                                      CL**7
00441      PERFORM 2000-BUILD-TS THRU 2010-EXIT.                        EL160
00442                                                                   EL160
00443      MOVE COUNT-1 TO PI-TS-COUNT-1.                               EL160
00444                                                                   EL160
00445      IF PI-TS-COUNT-1 = ZEROES                                    EL160
00446          MOVE ER-0142 TO EMI-ERROR                                EL160
00447          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00448          MOVE -1 TO CARRSL                                        EL160
00449          GO TO 8110-SEND-DATA.                                    EL160
00450                                                                   EL160
00451      PERFORM 5240-WRITE-TS-160A THRU 5250-EXIT.                   EL160
00452                                                                   EL160
00453      MOVE XCTL-EL1602 TO CALL-PGM.                                EL160
00454                                                                   EL160
00455      GO TO 9200-XCTL.                                             EL160
00456                                                                   EL160
00457      EJECT                                                        EL160
00458                                                                   EL160
00459  0100-UPDATE-PI.                                                  EL160
00460      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           EL160
00461          GO TO 0110-UPDATE-UP.                                    EL160
00462                                                                   EL160
00463      MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6.             EL160
00464      MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5.             EL160
00465      MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4.             EL160
00466      MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3.             EL160
00467      MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2.             EL160
00468      MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1.             EL160
00469      MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM.           EL160
00470      MOVE THIS-PGM             TO PI-CALLING-PROGRAM.             EL160
00471                                                                   EL160
00472      PERFORM 0430-DELETE-TS THRU 0450-EXIT.                       EL160
00473                                                                   EL160
00474      MOVE -1 TO CARRSL.                                           EL160
00475      GO TO 8100-SEND-MAP.                                         EL160
00476                                                                   EL160
00477  0110-UPDATE-UP.                                                  EL160
00478      MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM.             EL160
00479      MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM.           EL160
00480      MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1.             EL160
00481      MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2.             EL160
00482      MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3.             EL160
00483      MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4.             EL160
00484      MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5.             EL160
00485      MOVE SPACES               TO PI-SAVED-PROGRAM-6.             EL160
00486                                                                   EL160
00487      GO TO 0400-FIND-SCREEN.                                      EL160
00488                                                                   EL160
00489  0200-TRANS-PF.                                                   EL160
00490      IF EIBAID NOT = DFHENTER                                     EL160
00491          MOVE 'X' TO ERROR-SWITCH                                 EL160
00492          GO TO 0210-EXIT.                                         EL160
00493                                                                   EL160
00494      IF PFKEYI NOT NUMERIC                                        EL160
00495          MOVE 'X' TO ERROR-SWITCH                                 EL160
00496          GO TO 0210-EXIT.                                         EL160
00497                                                                   EL160
00498      MOVE PFKEYI TO CHECK-PFKEYS.                                 EL160
00499                                                                   EL160
00500      IF CHECK-PFKEYS < 1 OR > 24                                  EL160
00501          MOVE 'X' TO ERROR-SWITCH                                 EL160
00502          GO TO 0210-EXIT.                                         EL160
00503                                                                   EL160
00504      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL160
00505                                                                   EL160
00506  0210-EXIT.                                                       EL160
00507      EXIT.                                                        EL160
00508                                                                   EL160
00509  0400-FIND-SCREEN.                                                EL160
00510      EXEC CICS HANDLE CONDITION                                   EL160
00511          ITEMERR (0410-TS-NOTFND)                                 EL160
00512          QIDERR  (0420-FIND-SCREEN-EXIT)                          EL160
00513      END-EXEC.                                                    EL160
00514                                                                   EL160
00515      EXEC CICS READQ TS                                           EL160
00516          QUEUE  (PI-EL160-KEY)                                    EL160
00517          INTO   (EL160AI)                                         EL160
00518          LENGTH (EL160A-LENGTH)                                   EL160
00519      END-EXEC.                                                    EL160
00520                                                                   EL160
00521      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.                     EL160
00522                                                                   EL160
00523      PERFORM 0430-DELETE-TS THRU 0450-EXIT.                       EL160
00524                                                                   EL160
00525      GO TO 0420-FIND-SCREEN-EXIT.                                 EL160
00526                                                                   EL160
00527  0410-TS-NOTFND.                                                  EL160
00528      MOVE ER-0192                TO EMI-ERROR.                       CL*17
00529      MOVE -1                     TO CARRSL.                          CL*17
00530      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL160
00531                                                                   EL160
00532  0420-FIND-SCREEN-EXIT.                                           EL160
00533      MOVE -1 TO CARRSL.                                           EL160
00534      GO TO 8100-SEND-MAP.                                         EL160
00535                                                                   EL160
00536  0430-DELETE-TS.                                                  EL160
00537      EXEC CICS HANDLE CONDITION                                   EL160
00538          QIDERR (0440-DELETE-CONT)                                EL160
00539      END-EXEC.                                                    EL160
00540                                                                   EL160
00541      EXEC CICS DELETEQ TS                                         EL160
00542          QUEUE (PI-EL1602-KEY)                                    EL160
00543      END-EXEC.                                                    EL160
00544                                                                   EL160
00545  0440-DELETE-CONT.                                                EL160
00546      EXEC CICS HANDLE CONDITION                                   EL160
00547          QIDERR (0450-EXIT)                                       EL160
00548      END-EXEC.                                                    EL160
00549                                                                   EL160
00550      EXEC CICS DELETEQ TS                                         EL160
00551          QUEUE (PI-EL160-KEY)                                     EL160
00552      END-EXEC.                                                    EL160
00553                                                                   EL160
00554  0450-EXIT.                                                       EL160
00555      EXIT.                                                        EL160
00556                                                                   EL160
00557  0500-BUILD-TS-KEY.                                               EL160
00558      MOVE ZERO      TO COUNT-1.                                   EL160
00559      MOVE EIBTRMID  TO HOLD-TERM.                                 EL160
00560      MOVE LIT-MAP   TO KEY-QUAL.                                  EL160
00561      MOVE HOLD-KEY  TO PI-EL160-KEY.                              EL160
00562      MOVE LIT-MAP-2 TO KEY-QUAL.                                  EL160
00563      MOVE HOLD-KEY  TO PI-EL1602-KEY.                             EL160
00564                                                                   EL160
00565  0510-EXIT.                                                       EL160
00566      EXIT.                                                        EL160
00567                                                                   EL160
00568      EJECT                                                        EL160
00569  1000-EDIT-SCREEN.                                                EL160
00570      MOVE SPACES TO CNTL-WORK-AREA.                               EL160
00571                                                                      CL*17
00572      IF CFILEIDL > ZEROS                                             CL*17
00573          MOVE CFILEIDI           TO W-VALID-FILE-IND                 CL*17
00574          IF W-VALID-FILE                                             CL*17
00575              IF W-RETRIEVE                                           CL*17
00576                  MOVE 'ELRETR'   TO W-FILE-ID                        CL*17
00577                  MOVE 'R'        TO PI-FILE-ID-IND                   CL*17
00578              ELSE                                                    CL*17
00579                  MOVE 'ELMSTR'   TO W-FILE-ID                        CL*17
00580                  MOVE 'M'        TO PI-FILE-ID-IND                   CL*17
00581          ELSE                                                        CL*17
00582              MOVE ER-0970        TO EMI-ERROR                        CL*17
00583              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*17
00584              MOVE AL-UABON       TO CFILEIDA                         CL*17
00585              MOVE -1             TO CFILEIDL                         CL*17
00586              GO TO 8110-SEND-DATA                                    CL*17
00587      ELSE                                                            CL*17
00588          MOVE 'M'                TO CFILEIDO                         CL*17
00589                                     PI-FILE-ID-IND                   CL*17
00590          MOVE +1                 TO CFILEIDL                         CL*17
00591          MOVE 'ELMSTR'           TO W-FILE-ID.                       CL*17
00592                                                                      CL*10
00593      IF ASEXI > LOW-VALUES                                           CL*10
00594          MOVE ASEXI TO WS-SEX-SELECTION                              CL*10
00595          IF WS-SEX-SELECTION = 'M' OR 'F'                            CL*10
00596              MOVE AL-UANON       TO ASEXA                            CL*10
00597          ELSE                                                        CL*10
00598             MOVE ER-0219         TO EMI-ERROR                        CL*10
00599             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL*10
00600             MOVE AL-UABON        TO ASEXA                            CL*10
00601             MOVE -1              TO ASEXL                            CL*10
00602      ELSE                                                            CL*10
00603         MOVE AL-UANOF            TO ASEXA.                           CL*10
00604                                                                   EL160
00605      IF CARRSI > LOW-VALUES                                       EL160
00606          MOVE CARRSI   TO CARRIER-CNTL                            EL160
00607          MOVE AL-UANON TO CARRSA                                  EL160
00608      ELSE                                                         EL160
00609          MOVE AL-UANOF TO CARRSA                                  EL160
00610          MOVE SPACES   TO CARRIER-CNTL.                           EL160
00611                                                                   EL160
00612      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00613                                                                   EL160
00614      IF INCLSI > SPACES                                           EL160
00615          MOVE INCLSI TO TEST-DATE                                 EL160
00616          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
00617          IF NO-CONVERSION-ERROR                                   EL160
00618             MOVE DC-GREG-DATE-1-EDIT TO INCLSI                    EL160
00619             MOVE DC-BIN-DATE-1       TO INC-DATE-LOW-CNTL         EL160
00620             MOVE AL-UANON            TO INCLSA                    EL160
00621          ELSE                                                     EL160
00622             MOVE -1                  TO INCLSL                    EL160
00623             MOVE AL-UABON            TO INCLSA                    EL160
00624             MOVE LOW-VALUES          TO INC-DATE-LOW-CNTL         EL160
00625      ELSE                                                         EL160
00626          MOVE AL-UANOF               TO INCLSA                    EL160
00627          MOVE LOW-VALUES             TO INC-DATE-LOW-CNTL.        EL160
00628                                                                   EL160
00629      IF INCHSI > SPACES                                           EL160
00630          MOVE INCHSI TO TEST-DATE                                 EL160
00631          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
00632          IF NO-CONVERSION-ERROR                                   EL160
00633             MOVE DC-GREG-DATE-1-EDIT TO INCHSI                    EL160
00634             MOVE DC-BIN-DATE-1       TO INC-DATE-HIGH-CNTL        EL160
00635             MOVE AL-UANON            TO INCHSA                    EL160
00636          ELSE                                                     EL160
00637             MOVE -1                  TO INCHSL                    EL160
00638             MOVE AL-UABON            TO INCHSA                    EL160
00639             MOVE HIGH-VALUES         TO INC-DATE-HIGH-CNTL        EL160
00640      ELSE                                                         EL160
00641          MOVE AL-UANOF               TO INCHSA                    EL160
00642          MOVE HIGH-VALUES            TO INC-DATE-HIGH-CNTL.       EL160
00643                                                                   EL160
00644      IF INC-DATE-HIGH-CNTL < INC-DATE-LOW-CNTL                    EL160
00645          MOVE 'X'      TO BUILD-SWITCH                            EL160
00646          MOVE ER-0308  TO EMI-ERROR                               EL160
00647          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00648          MOVE -1       TO INCLSL                                  EL160
00649          MOVE AL-UABON TO INCLSA INCHSA.                          EL160
00650                                                                   EL160
00651      IF GRPSI > LOW-VALUES                                        EL160
00652          MOVE GRPSI    TO GROUP-CNTL                              EL160
00653          MOVE AL-UANON TO GRPSA                                   EL160
00654      ELSE                                                         EL160
00655          MOVE AL-UANOF TO GRPSA.                                  EL160
00656                                                                   EL160
00657      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00658                                                                   EL160
00659      IF PMTDLSI > SPACES                                          EL160
00660          MOVE PMTDLSI TO TEST-DATE                                EL160
00661          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
00662          IF NO-CONVERSION-ERROR                                   EL160
00663             MOVE DC-GREG-DATE-1-EDIT TO PMTDLSI                   EL160
00664             MOVE DC-BIN-DATE-1       TO LST-PMT-LOW-CNTL          EL160
00665             MOVE AL-UANON            TO PMTDLSA                   EL160
00666          ELSE                                                     EL160
00667             MOVE -1                  TO PMTDLSL                   EL160
00668             MOVE AL-UABON            TO PMTDLSA                   EL160
00669             MOVE LOW-VALUES          TO LST-PMT-LOW-CNTL          EL160
00670      ELSE                                                         EL160
00671          MOVE AL-UANOF               TO PMTDLSA                   EL160
00672          MOVE LOW-VALUES             TO LST-PMT-LOW-CNTL.         EL160
00673                                                                   EL160
00674      IF PMTDHSI > SPACES                                          EL160
00675          MOVE PMTDHSI TO TEST-DATE                                EL160
00676          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
00677          IF NO-CONVERSION-ERROR                                   EL160
00678             MOVE DC-GREG-DATE-1-EDIT TO PMTDHSI                   EL160
00679             MOVE DC-BIN-DATE-1       TO LST-PMT-HIGH-CNTL         EL160
00680             MOVE AL-UANON            TO PMTDHSA                   EL160
00681          ELSE                                                     EL160
00682             MOVE -1                  TO PMTDHSL                   EL160
00683             MOVE AL-UABON            TO PMTDHSA                   EL160
00684             MOVE HIGH-VALUES         TO LST-PMT-HIGH-CNTL         EL160
00685      ELSE                                                         EL160
00686          MOVE AL-UANOF               TO PMTDHSA                   EL160
00687          MOVE HIGH-VALUES            TO LST-PMT-HIGH-CNTL.        EL160
00688                                                                   EL160
00689      IF LST-PMT-HIGH-CNTL < LST-PMT-LOW-CNTL                      EL160
00690          MOVE 'X'      TO BUILD-SWITCH                            EL160
00691          MOVE ER-0308  TO EMI-ERROR                               EL160
00692          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00693          MOVE -1       TO PMTDLSL                                 EL160
00694          MOVE AL-UABON TO PMTDLSA PMTDHSA.                        EL160
00695                                                                   EL160
00696      IF STATESI > LOW-VALUES                                      EL160
00697          MOVE STATESI  TO STATE-CNTL                              EL160
00698          MOVE AL-UANON TO STATESA                                 EL160
00699      ELSE                                                         EL160
00700          MOVE AL-UANOF TO STATESA.                                EL160
00701                                                                   EL160
00702      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00703                                                                   EL160
00704      IF OPENLSI = LOW-VALUES                                      EL160
00705          MOVE AL-UANOF     TO OPENLSA                             EL160
00706          MOVE ZEROS        TO MO-OPEN-LOW-CNTL                    EL160
00707      ELSE                                                         EL160
00708          IF OPENLSI NOT NUMERIC                                   EL160
00709              MOVE 'X'      TO ERROR-SWITCH BUILD-SWITCH           EL160
00710              MOVE ZEROS    TO MO-OPEN-LOW-CNTL                    EL160
00711          ELSE                                                     EL160
00712              MOVE OPENLSI  TO MO-OPEN-LOW-CNTL                    EL160
00713              MOVE AL-UANON TO OPENLSA.                            EL160
00714                                                                   EL160
00715      IF SCREEN-ERROR                                              EL160
00716          MOVE -1       TO OPENLSL                                 EL160
00717          MOVE AL-UABON TO OPENLSA                                 EL160
00718          MOVE ER-0306  TO EMI-ERROR                               EL160
00719          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL160
00720                                                                   EL160
00721      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00722                                                                   EL160
00723      IF OPENHSI = LOW-VALUES                                      EL160
00724          MOVE AL-UANOF     TO OPENHSA                             EL160
00725          MOVE 9999         TO MO-OPEN-HIGH-CNTL                   EL160
00726      ELSE                                                         EL160
00727          IF OPENHSI NOT NUMERIC                                   EL160
00728              MOVE 'X'      TO ERROR-SWITCH BUILD-SWITCH           EL160
00729              MOVE 9999 TO MO-OPEN-HIGH-CNTL                       EL160
00730          ELSE                                                     EL160
00731              MOVE OPENHSI  TO MO-OPEN-HIGH-CNTL                   EL160
00732              MOVE AL-UANON TO OPENHSA.                            EL160
00733                                                                   EL160
00734      IF SCREEN-ERROR                                              EL160
00735          MOVE -1       TO OPENHSL                                 EL160
00736          MOVE AL-UABON TO OPENHSA                                 EL160
00737          MOVE ER-0306  TO EMI-ERROR                               EL160
00738          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL160
00739                                                                   EL160
00740      IF MO-OPEN-HIGH-CNTL < MO-OPEN-LOW-CNTL                      EL160
00741          MOVE 'X'      TO BUILD-SWITCH                            EL160
00742          MOVE ER-0308  TO EMI-ERROR                               EL160
00743          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00744          MOVE -1       TO OPENLSL                                 EL160
00745          MOVE AL-UABON TO OPENLSA OPENHSA.                        EL160
00746                                                                   EL160
00747      IF ACCTSI > LOW-VALUES                                       EL160
00748          MOVE ACCTSI   TO ACCOUNT-CNTL                            EL160
00749          MOVE AL-UANON TO ACCTSA                                  EL160
00750      ELSE                                                         EL160
00751          MOVE AL-UANOF TO ACCTSA.                                 EL160
00752                                                                   EL160
00753      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00754                                                                   EL160
00755      IF AMTLSI > LOW-VALUES                                       EL160
00756          MOVE AMTLSI TO TEST-AMT                                  EL160
00757          PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT                   EL160
00758          MOVE -1       TO AMTLSL                                     CL*17
00759      ELSE                                                         EL160
00760          MOVE AL-UANOF TO AMTLSA                                  EL160
00761          MOVE ZEROS  TO AMT-PAID-LOW-CNTL.                        EL160
00762                                                                   EL160
00763      IF SCREEN-ERROR                                              EL160
00764          MOVE -1           TO AMTLSL                              EL160
00765          MOVE AL-UABON     TO AMTLSA                              EL160
00766          MOVE ZEROS        TO AMT-PAID-LOW-CNTL                   EL160
00767      ELSE                                                         EL160
00768          IF AMTLSI > LOW-VALUES                                   EL160
00769              MOVE AMT-BIF  TO AMT-PAID-LOW-CNTL AMTLSO            EL160
00770              MOVE AL-UANON TO AMTLSA.                             EL160
00771                                                                   EL160
00772      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00773                                                                   EL160
00774      IF AMTHSI > LOW-VALUES                                       EL160
00775          MOVE AMTHSI       TO TEST-AMT                            EL160
00776          PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT                   EL160
00777          MOVE -1           TO AMTHSL                                 CL*17
00778      ELSE                                                         EL160
00779          MOVE AL-UANOF     TO AMTHSA                              EL160
00780          MOVE ALL-NINES    TO AMT-PAID-HIGH-CNTL.                 EL160
00781                                                                   EL160
00782      IF SCREEN-ERROR                                              EL160
00783          MOVE -1           TO AMTHSL                              EL160
00784          MOVE AL-UABON     TO AMTHSA                              EL160
00785          MOVE ALL-NINES    TO AMT-PAID-HIGH-CNTL                  EL160
00786      ELSE                                                         EL160
00787          IF AMTHSI > LOW-VALUES                                   EL160
00788              MOVE AMT-BIF  TO AMT-PAID-HIGH-CNTL AMTHSO           EL160
00789              MOVE AL-UANON TO AMTHSA.                             EL160
00790                                                                   EL160
00791      IF AMT-PAID-HIGH-CNTL < AMT-PAID-LOW-CNTL                    EL160
00792          MOVE 'X'          TO BUILD-SWITCH                        EL160
00793          MOVE ER-0308      TO EMI-ERROR                           EL160
00794          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00795          MOVE -1           TO AMTLSL                              EL160
00796          MOVE AL-UABON     TO AMTLSA AMTHSA.                      EL160
00797                                                                   EL160
00798      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00799                                                                   EL160
00800      IF TYPESI > LOW-VALUES                                       EL160
121802        IF TYPESI = PI-LIFE-OVERRIDE-L1 OR
121802                    PI-AH-OVERRIDE-L1   OR
052614*                   'I' OR 'G' OR 'F'
100518*                   'I' OR 'G' OR 'F' OR 'O'
080322                    'I' OR 'G' OR 'F' OR 'O' OR 'B' OR 'H'
00802            MOVE AL-UANON         TO TYPESA                        EL160
00803            MOVE TYPESI           TO TYPE-CNTL                     EL160
00804         ELSE                                                      EL160
00805            MOVE AL-UABON         TO TYPESA                        EL160
00806            MOVE -1               TO TYPESL                        EL160
00807            MOVE 'X'              TO BUILD-SWITCH                  EL160
00808            MOVE ER-0199          TO EMI-ERROR                     EL160
00809            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL160
00810      ELSE                                                         EL160
00811         MOVE AL-UANOF            TO TYPESA.                       EL160
00812                                                                   EL160
00813      IF CAUSELSI > SPACES                                         EL160
00814          MOVE CAUSELSI   TO CAUSE-CD-LOW-CNTL                     EL160
00815      ELSE                                                         EL160
00816          MOVE LOW-VALUES TO CAUSE-CD-LOW-CNTL.                    EL160
00817                                                                   EL160
00818      IF CAUSEHSI > SPACES                                         EL160
00819          MOVE CAUSEHSI    TO CAUSE-CD-HIGH-CNTL                   EL160
00820      ELSE                                                         EL160
00821          MOVE HIGH-VALUES TO CAUSE-CD-HIGH-CNTL.                  EL160
00822                                                                   EL160
00823      IF CAUSE-CD-HIGH-CNTL < CAUSE-CD-LOW-CNTL                    EL160
00824          MOVE 'X'       TO BUILD-SWITCH                           EL160
00825          MOVE ER-0308   TO EMI-ERROR                              EL160
00826          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00827          MOVE -1        TO CAUSELSL                               EL160
00828          MOVE AL-UABON  TO CAUSELSA CAUSEHSA.                     EL160
00829                                                                   EL160
00830      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00831                                                                   EL160
00832      IF DENSI > LOW-VALUES                                        EL160
00833          MOVE DENSI        TO TEST-RESP                           EL160
00834          IF TEST-RESP = 'Y' OR 'N' OR SPACE                       EL160
00835              MOVE AL-UANON TO DENSA                               EL160
00836              MOVE DENSI    TO DEN-CNTL                            EL160
00837          ELSE                                                     EL160
00838             MOVE 'X'       TO BUILD-SWITCH                        EL160
00839             MOVE ER-0046   TO EMI-ERROR                           EL160
00840             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL160
00841             MOVE AL-UABON  TO DENSA                               EL160
00842             MOVE -1        TO DENSL                               EL160
00843      ELSE                                                         EL160
00844         MOVE AL-UANOF      TO DENSA.                              EL160
00845                                                                   EL160
00846      IF REPLSI > SPACES                                           EL160
00847          MOVE REPLSI TO TEST-DATE                                 EL160
00848          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
00849          IF NO-CONVERSION-ERROR                                   EL160
00850             MOVE DC-GREG-DATE-1-EDIT TO REPLSI                    EL160
00851             MOVE DC-BIN-DATE-1       TO REP-DATE-LOW-CNTL         EL160
00852             MOVE AL-UANON            TO REPLSA                    EL160
00853            ELSE                                                   EL160
00854             MOVE -1                  TO REPLSL                    EL160
00855             MOVE AL-UABON            TO REPLSA                    EL160
00856             MOVE LOW-VALUES          TO REP-DATE-LOW-CNTL         EL160
00857      ELSE                                                         EL160
00858          MOVE AL-UANOF               TO REPLSA                    EL160
00859          MOVE LOW-VALUES             TO REP-DATE-LOW-CNTL.        EL160
00860                                                                   EL160
00861      IF REPHSI > SPACES                                           EL160
00862          MOVE REPHSI                 TO TEST-DATE                 EL160
00863          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
00864          IF NO-CONVERSION-ERROR                                   EL160
00865             MOVE DC-GREG-DATE-1-EDIT TO REPHSI                    EL160
00866             MOVE DC-BIN-DATE-1       TO REP-DATE-HIGH-CNTL        EL160
00867             MOVE AL-UANON            TO REPHSA                    EL160
00868            ELSE                                                   EL160
00869             MOVE -1                  TO REPHSL                    EL160
00870             MOVE AL-UABON            TO REPHSA                    EL160
00871             MOVE HIGH-VALUES         TO REP-DATE-HIGH-CNTL        EL160
00872      ELSE                                                         EL160
00873          MOVE AL-UANOF               TO REPHSA                    EL160
00874          MOVE HIGH-VALUES            TO REP-DATE-HIGH-CNTL.       EL160
00875                                                                   EL160
00876      IF REP-DATE-HIGH-CNTL < REP-DATE-LOW-CNTL                    EL160
00877          MOVE 'X'      TO BUILD-SWITCH                            EL160
00878          MOVE ER-0308  TO EMI-ERROR                               EL160
00879          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00880          MOVE -1       TO REPLSL                                  EL160
00881          MOVE AL-UABON TO REPLSA REPHSA.                          EL160
00882                                                                   EL160
00883      MOVE SPACE            TO ERROR-SWITCH.                       EL160
00884                                                                   EL160
00885      IF PROCSI > LOW-VALUES                                       EL160
00886          PERFORM 1060-CHECK-PROC THRU 1080-EXIT.                  EL160
00887                                                                   EL160
00888      IF SCREEN-ERROR                                              EL160
00889          MOVE AL-UABON     TO PROCSA                              EL160
00890          MOVE -1           TO PROCSL                              EL160
00891      ELSE                                                         EL160
00892          IF PROCSI = LOW-VALUES                                   EL160
00893              MOVE AL-UANOF TO PROCSA                              EL160
00894          ELSE                                                     EL160
00895              MOVE AL-UANON TO PROCSA                              EL160
00896              MOVE PROCSI   TO PROC-CNTL.                          EL160
00897                                                                   EL160
00898      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00899                                                                   EL160
00900      IF PMTLSI > LOW-VALUES                                       EL160
00901          MOVE PMTLSI   TO TEST-AMT                                EL160
00902          PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT                   EL160
00903          MOVE -1       TO PMTLSL                                     CL*17
00904      ELSE                                                         EL160
00905          MOVE AL-UANOF TO PMTHSA                                  EL160
00906          MOVE ZEROES   TO LST-PAID-LOW-CNTL.                      EL160
00907                                                                   EL160
00908      IF SCREEN-ERROR                                              EL160
00909          MOVE -1       TO PMTLSL                                  EL160
00910          MOVE AL-UABON TO PMTLSA                                  EL160
00911          MOVE ZEROES   TO LST-PAID-LOW-CNTL                       EL160
00912      ELSE                                                         EL160
00913          IF PMTLSI > LOW-VALUES                                   EL160
00914              MOVE AMT-BIF  TO LST-PAID-LOW-CNTL PMTLSO            EL160
00915              MOVE AL-UANON TO PMTLSA.                             EL160
00916                                                                   EL160
00917      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00918                                                                   EL160
00919      IF PMTHSI > LOW-VALUES                                       EL160
00920          MOVE PMTHSI    TO TEST-AMT                               EL160
00921          PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT                   EL160
00922      ELSE                                                         EL160
00923          MOVE AL-UANOF  TO PMTHSA                                 EL160
00924          MOVE ALL-NINES TO LST-PAID-HIGH-CNTL.                    EL160
00925                                                                   EL160
00926      IF SCREEN-ERROR                                              EL160
00927          MOVE -1        TO PMTHSL                                 EL160
00928          MOVE AL-UABON  TO PMTHSA                                 EL160
00929          MOVE ALL-NINES TO LST-PAID-HIGH-CNTL                     EL160
00930      ELSE                                                         EL160
00931          IF PMTHSI > LOW-VALUES                                   EL160
00932              MOVE AMT-BIF  TO LST-PAID-HIGH-CNTL PMTHSO           EL160
00933              MOVE AL-UANON TO PMTHSA.                             EL160
00934                                                                   EL160
00935      IF LST-PAID-HIGH-CNTL < LST-PAID-LOW-CNTL                    EL160
00936          MOVE 'X'      TO BUILD-SWITCH                            EL160
00937          MOVE ER-0308  TO EMI-ERROR                               EL160
00938          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00939          MOVE -1       TO PMTLSL                                  EL160
00940          MOVE AL-UABON TO PMTLSA PMTHSA.                          EL160
00941                                                                   EL160
00942      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00943                                                                   EL160
00944      IF PREMSI > LOW-VALUES                                       EL160
00945         IF PREMSI = SPACE OR                                      EL160
00946           (PREMSI > '0' AND < '4')                                EL160
00947              MOVE AL-UANON       TO PREMSA                        EL160
00948              MOVE PREMSI         TO PREM-CNTL                     EL160
00949         ELSE                                                      EL160
00950            MOVE 'X'              TO BUILD-SWITCH                  EL160
00951            MOVE ER-0227          TO EMI-ERROR                     EL160
00952            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL160
00953            MOVE AL-UABON         TO PREMSA                        EL160
00954            MOVE -1               TO PREMSL                        EL160
00955      ELSE                                                         EL160
00956         MOVE AL-UANOF            TO PREMSA.                       EL160
00957                                                                   EL160
00958      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00959                                                                   EL160
00960      IF MNTLSI > SPACES                                           EL160
00961          MOVE MNTLSI TO TEST-DATE                                 EL160
00962          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
00963          IF NO-CONVERSION-ERROR                                   EL160
00964             MOVE DC-GREG-DATE-1-EDIT         TO MNTLSI            EL160
00965             MOVE DC-BIN-DATE-1   TO MNT-DATE-LOW-CNTL             EL160
00966             MOVE AL-UANON        TO MNTLSA                        EL160
00967            ELSE                                                   EL160
00968             MOVE -1              TO MNTLSL                        EL160
00969             MOVE AL-UABON        TO MNTLSA                        EL160
00970             MOVE LOW-VALUES      TO MNT-DATE-LOW-CNTL             EL160
00971      ELSE                                                         EL160
00972          MOVE AL-UANOF   TO MNTLSA                                EL160
00973          MOVE LOW-VALUES TO MNT-DATE-LOW-CNTL.                    EL160
00974                                                                   EL160
00975      IF MNTHSI > SPACES                                           EL160
00976          MOVE MNTHSI TO TEST-DATE                                 EL160
00977          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
00978          IF NO-CONVERSION-ERROR                                   EL160
00979             MOVE DC-GREG-DATE-1-EDIT TO MNTHSI                    EL160
00980             MOVE DC-BIN-DATE-1       TO MNT-DATE-HIGH-CNTL        EL160
00981             MOVE AL-UANON            TO MNTHSA                    EL160
00982            ELSE                                                   EL160
00983             MOVE -1                  TO MNTHSL                    EL160
00984             MOVE AL-UABON            TO MNTHSA                    EL160
00985             MOVE HIGH-VALUES         TO MNT-DATE-HIGH-CNTL        EL160
00986      ELSE                                                         EL160
00987          MOVE AL-UANOF TO MNTHSA                                  EL160
00988          MOVE HIGH-VALUES TO MNT-DATE-HIGH-CNTL.                  EL160
00989                                                                   EL160
00990      IF MNT-DATE-HIGH-CNTL < MNT-DATE-LOW-CNTL                    EL160
00991          MOVE 'X'       TO BUILD-SWITCH                           EL160
00992          MOVE ER-0308   TO EMI-ERROR                              EL160
00993          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
00994          MOVE -1        TO MNTLSL                                 EL160
00995          MOVE AL-UABON  TO MNTLSA MNTHSA.                         EL160
00996                                                                   EL160
00997      MOVE SPACE TO ERROR-SWITCH.                                  EL160
00998                                                                   EL160
00999      IF REQSI > LOW-VALUES                                        EL160
01000          MOVE REQSI TO TEST-RESP                                  EL160
01001          IF TEST-RESP = 'Y' OR 'N' OR SPACE                       EL160
01002              MOVE AL-UANON       TO REQSA                         EL160
01003              MOVE REQSI          TO REQ-CNTL                      EL160
01004          ELSE                                                     EL160
01005             MOVE 'X'             TO  BUILD-SWITCH                 EL160
01006             MOVE ER-0046         TO EMI-ERROR                     EL160
01007             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL160
01008             MOVE AL-UABON        TO REQSA                         EL160
01009             MOVE -1              TO REQSL                         EL160
01010      ELSE                                                         EL160
01011         MOVE AL-UANOF            TO REQSA.                        EL160
01012                                                                   EL160
01013      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01014                                                                   EL160
01015      IF ESTLSI > SPACES                                           EL160
01016          MOVE ESTLSI TO TEST-DATE                                 EL160
01017          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
01018          IF NO-CONVERSION-ERROR                                   EL160
01019             MOVE DC-GREG-DATE-1-EDIT TO ESTLSI                    EL160
01020             MOVE DC-BIN-DATE-1       TO EST-DATE-LOW-CNTL         EL160
01021             MOVE AL-UANON            TO ESTLSA                    EL160
01022             ELSE                                                  EL160
01023             MOVE -1                  TO ESTLSL                    EL160
01024             MOVE AL-UABON            TO ESTLSA                    EL160
01025             MOVE LOW-VALUES          TO EST-DATE-LOW-CNTL         EL160
01026      ELSE                                                         EL160
01027          MOVE AL-UANOF   TO ESTLSA                                EL160
01028          MOVE LOW-VALUES TO EST-DATE-LOW-CNTL.                    EL160
01029                                                                   EL160
01030      IF ESTHSI > SPACES                                           EL160
01031          MOVE ESTHSI TO TEST-DATE                                 EL160
01032          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
01033          IF NO-CONVERSION-ERROR                                   EL160
01034             MOVE DC-GREG-DATE-1-EDIT TO ESTHSI                    EL160
01035             MOVE DC-BIN-DATE-1       TO EST-DATE-HIGH-CNTL        EL160
01036             MOVE AL-UANON            TO ESTHSA                    EL160
01037             ELSE                                                  EL160
01038             MOVE -1                  TO ESTHSL                    EL160
01039             MOVE AL-UABON            TO ESTHSA                    EL160
01040             MOVE HIGH-VALUES         TO EST-DATE-HIGH-CNTL        EL160
01041      ELSE                                                         EL160
01042          MOVE AL-UANOF               TO ESTHSA                    EL160
01043          MOVE HIGH-VALUES            TO EST-DATE-HIGH-CNTL.       EL160
01044                                                                   EL160
01045      IF MNT-DATE-HIGH-CNTL < MNT-DATE-LOW-CNTL                    EL160
01046          MOVE 'X'      TO BUILD-SWITCH                            EL160
01047          MOVE ER-0308  TO EMI-ERROR                               EL160
01048          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
01049          MOVE -1       TO MNTLSL                                  EL160
01050          MOVE AL-UABON TO MNTLSA MNTHSA.                          EL160
01051                                                                   EL160
01052      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01053                                                                   EL160
01054      IF SUPRSI > LOW-VALUES                                       EL160
01055          MOVE SUPRSI TO TEST-RESP                                 EL160
01056          IF TEST-RESP = 'Y' OR 'N' OR SPACE                       EL160
01057              MOVE AL-UANON       TO SUPRSA                        EL160
01058              MOVE SUPRSI         TO SUPR-CNTL                     EL160
01059          ELSE                                                     EL160
01060             MOVE 'X'             TO  BUILD-SWITCH                 EL160
01061             MOVE ER-0046         TO EMI-ERROR                     EL160
01062             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL160
01063             MOVE AL-UABON        TO SUPRSA                        EL160
01064             MOVE -1              TO SUPRSL                        EL160
01065      ELSE                                                         EL160
01066         MOVE AL-UANOF            TO SUPRSA.                       EL160
01067                                                                   EL160
01068      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01069                                                                   EL160
01070      IF FOLLSI > SPACES                                           EL160
01071          MOVE FOLLSI TO TEST-DATE                                 EL160
01072          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
01073          IF NO-CONVERSION-ERROR                                   EL160
01074             MOVE DC-GREG-DATE-1-EDIT TO FOLLSI                    EL160
01075             MOVE DC-BIN-DATE-1       TO FOL-DATE-LOW-CNTL         EL160
01076             MOVE AL-UANON            TO FOLLSA                    EL160
01077            ELSE                                                   EL160
01078             MOVE -1                  TO FOLLSL                    EL160
01079             MOVE AL-UABON            TO FOLLSA                    EL160
01080             MOVE LOW-VALUES          TO FOL-DATE-LOW-CNTL         EL160
01081      ELSE                                                         EL160
01082          MOVE AL-UANOF               TO FOLLSA                    EL160
01083          MOVE LOW-VALUES             TO FOL-DATE-LOW-CNTL.        EL160
01084                                                                   EL160
01085      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01086                                                                   EL160
01087      IF FOLHSI > SPACES                                           EL160
01088          MOVE FOLHSI TO TEST-DATE                                 EL160
01089          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT                  EL160
01090          IF NO-CONVERSION-ERROR                                   EL160
01091             MOVE DC-GREG-DATE-1-EDIT TO FOLHSI                    EL160
01092             MOVE DC-BIN-DATE-1       TO FOL-DATE-HIGH-CNTL        EL160
01093             MOVE AL-UANON            TO FOLHSA                    EL160
01094             ELSE                                                  EL160
01095             MOVE -1                  TO FOLHSL                    EL160
01096             MOVE AL-UABON            TO FOLHSA                    EL160
01097             MOVE HIGH-VALUES         TO FOL-DATE-HIGH-CNTL        EL160
01098      ELSE                                                         EL160
01099          MOVE AL-UANOF               TO FOLHSA                    EL160
01100          MOVE HIGH-VALUES            TO FOL-DATE-HIGH-CNTL.       EL160
01101                                                                   EL160
01102      IF FOL-DATE-HIGH-CNTL < FOL-DATE-LOW-CNTL                    EL160
01103          MOVE 'X'     TO BUILD-SWITCH                             EL160
01104          MOVE ER-0308 TO EMI-ERROR                                EL160
01105          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
01106          MOVE -1      TO FOLLSL                                   EL160
01107          MOVE AL-UABON TO FOLLSA FOLHSA.                          EL160
01108                                                                   EL160
01109      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01110                                                                   EL160
01111      IF CERTSI > LOW-VALUES                                       EL160
01112          MOVE CERTSI TO TEST-RESP                                 EL160
01113          IF TEST-RESP = 'Y' OR 'N' OR SPACE                       EL160
01114              MOVE AL-UANON       TO CERTSA                        EL160
01115              MOVE CERTSI         TO CERT-CNTL                     EL160
01116          ELSE                                                     EL160
01117             MOVE 'X'             TO  BUILD-SWITCH                 EL160
01118             MOVE ER-0046         TO EMI-ERROR                     EL160
01119             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL160
01120             MOVE AL-UABON        TO CERTSA                        EL160
01121             MOVE -1              TO CERTSL                        EL160
01122      ELSE                                                         EL160
01123         MOVE AL-UANOF            TO CERTSA.                       EL160
01124                                                                   EL160
01125      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01126                                                                   EL160
01127      IF DAYSLSI = LOW-VALUES                                      EL160
01128          MOVE AL-UANOF TO DAYSLSA                                 EL160
01129          MOVE ZEROES   TO DAYS-LOW-CNTL                           EL160
01130      ELSE                                                         EL160
01131          IF DAYSLSI NOT NUMERIC                                   EL160
01132              MOVE ZEROS TO DAYS-LOW-CNTL                          EL160
01133              MOVE 'X' TO ERROR-SWITCH BUILD-SWITCH                EL160
01134          ELSE                                                     EL160
01135              MOVE DAYSLSI  TO DAYS-LOW-CNTL                       EL160
01136              MOVE AL-UANON TO DAYSLSA.                            EL160
01137                                                                   EL160
01138      IF SCREEN-ERROR                                              EL160
01139          MOVE -1       TO DAYSLSL                                 EL160
01140          MOVE AL-UABON TO DAYSLSA                                 EL160
01141          MOVE ER-0307  TO EMI-ERROR                               EL160
01142          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL160
01143                                                                   EL160
01144      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01145                                                                   EL160
01146      IF DAYSHSI = LOW-VALUES                                      EL160
01147          MOVE AL-UANOF     TO DAYSHSA                             EL160
01148          MOVE 9999         TO DAYS-HIGH-CNTL                      EL160
01149      ELSE                                                         EL160
01150          IF DAYSHSI NOT NUMERIC                                   EL160
01151              MOVE 9999 TO DAYS-HIGH-CNTL                          EL160
01152              MOVE 'X'      TO ERROR-SWITCH BUILD-SWITCH           EL160
01153          ELSE                                                     EL160
01154              MOVE DAYSHSI  TO DAYS-HIGH-CNTL                      EL160
01155              MOVE AL-UANON TO DAYSHSA.                            EL160
01156                                                                   EL160
01157      IF SCREEN-ERROR                                              EL160
01158          MOVE -1       TO DAYSHSL                                 EL160
01159          MOVE AL-UABON TO DAYSHSA                                 EL160
01160          MOVE ER-0307  TO EMI-ERROR                               EL160
01161          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL160
01162                                                                   EL160
01163      IF DAYS-HIGH-CNTL < DAYS-LOW-CNTL                            EL160
01164          MOVE 'X'     TO BUILD-SWITCH                             EL160
01165          MOVE ER-0308 TO EMI-ERROR                                EL160
01166          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL160
01167          MOVE -1       TO DAYSLSL                                 EL160
01168          MOVE AL-UABON TO DAYSLSA DAYSHSA.                        EL160
01169                                                                   EL160
01170      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01171                                                                   EL160
01172      IF PRISI > LOW-VALUES                                        EL160
01173          IF PRISI = SPACE OR (PRISI > '0' AND                     EL160
01174                               NOT   > '9')                        EL160
01175              MOVE AL-UANON       TO PRISA                         EL160
01176              MOVE PRISI          TO PRI-CNTL                      EL160
01177          ELSE                                                     EL160
01178             MOVE 'X'             TO  BUILD-SWITCH                 EL160
01179             MOVE ER-0274         TO EMI-ERROR                     EL160
01180             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL160
01181             MOVE AL-UABON        TO PRISA                         EL160
01182             MOVE -1              TO PRISL                         EL160
01183      ELSE                                                         EL160
01184         MOVE AL-UANOF            TO PRISA.                        EL160
01185                                                                   EL160
01186                                                                   EL160
01187      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01188                                                                   EL160
01189      IF AUTOSI > LOW-VALUES                                       EL160
01190          MOVE AUTOSI TO TEST-RESP                                 EL160
01191          IF TEST-RESP = 'Y' OR 'N' OR SPACE                       EL160
01192              MOVE AL-UANON        TO AUTOSA                       EL160
01193              MOVE AUTOSI          TO AUTO-CNTL                    EL160
01194          ELSE                                                     EL160
01195             MOVE 'X'             TO  BUILD-SWITCH                 EL160
01196             MOVE ER-0046         TO EMI-ERROR                     EL160
01197             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL160
01198             MOVE AL-UABON        TO AUTOSA                        EL160
01199             MOVE -1              TO AUTOSL                        EL160
01200      ELSE                                                         EL160
01201         MOVE AL-UANOF            TO AUTOSA.                       EL160
01202                                                                   EL160
01203      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01204                                                                   EL160
01205      IF OPCLSI > LOW-VALUES                                       EL160
01206          MOVE OPCLSI TO TEST-RESP                                 EL160
01207          IF TEST-RESP = 'O' OR 'C' OR 'R' OR SPACE                   CL*14
01208              MOVE AL-UANON       TO OPCLSA                        EL160
01209              MOVE OPCLSI         TO OPCL-CNTL                     EL160
01210          ELSE                                                     EL160
01211             MOVE 'X'             TO  BUILD-SWITCH                 EL160
01212             MOVE ER-0767         TO EMI-ERROR                        CL*14
01213             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL160
01214             MOVE AL-UABON        TO OPCLSA                        EL160
01215             MOVE -1              TO OPCLSL                        EL160
01216      ELSE                                                         EL160
01217         MOVE AL-UANOF            TO OPCLSA.                       EL160
01218                                                                   EL160
01219                                                                   EL160
01220      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01221                                                                   EL160
01222      IF PRTOPTI > LOW-VALUES                                      EL160
01223          IF PRTOPTI = 'N' OR 'L'                                     CL**8
01224              MOVE AL-UANON       TO PRTOPTA                       EL160
01225              MOVE PRTOPTI        TO PI-PRINT-OPTION               EL160
01226          ELSE                                                     EL160
01227             MOVE 'X'             TO BUILD-SWITCH                  EL160
01228             MOVE ER-0334         TO EMI-ERROR                     EL160
01229             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL160
01230             MOVE AL-UABON        TO PRTOPTA                       EL160
01231             MOVE -1              TO PRTOPTL                       EL160
01232      ELSE                                                         EL160
01233         MOVE 'L'                 TO PI-PRINT-OPTION                  CL**8
01234         MOVE AL-UANOF            TO PRTOPTA.                      EL160
01235                                                                   EL160
01236      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01237                                                                   EL160
01238      IF FMTOPTI > LOW-VALUES                                      EL160
01239          IF FMTOPTI = 'F' OR 'P'                                  EL160
01240              MOVE AL-UANON       TO FMTOPTA                       EL160
01241              MOVE FMTOPTI        TO PI-FORMAT-OPTION              EL160
01242          ELSE                                                     EL160
01243             MOVE 'X'             TO BUILD-SWITCH                  EL160
01244             MOVE ER-0335         TO EMI-ERROR                     EL160
01245             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL160
01246             MOVE AL-UABON        TO FMTOPTA                       EL160
01247             MOVE -1              TO FMTOPTL                       EL160
01248      ELSE                                                         EL160
01249         MOVE AL-UANOF            TO FMTOPTA.                      EL160
01250                                                                   EL160
01251      IF ALTPRTI > LOW-VALUES                                      EL160
01252          MOVE AL-UANON                   TO  ALTPRTA                 CL**7
01253          MOVE ALTPRTI                    TO  PI-ALT-PRINT-ID         CL**7
01254      ELSE                                                         EL160
01255          IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES              CL**7
01256              MOVE PI-PROCESSOR-PRINTER   TO  PI-ALT-PRINT-ID         CL**7
01257          ELSE                                                        CL**7
01258              MOVE SPACES                 TO  PI-ALT-PRINT-ID.        CL**7
01259                                                                   EL160
01260      IF SCREEN-HAS-ERRORS                                         EL160
01261          GO TO 1010-EXIT.                                         EL160
01262                                                                   EL160
01263      MOVE SAVE-BIN-DATE TO CURRENT-DATE-BIN.                      EL160
01264                                                                   EL160
01265  1010-EXIT.                                                       EL160
01266      EXIT.                                                        EL160
01267      EJECT                                                        EL160
01268                                                                   EL160
01269  1060-CHECK-PROC.                                                 EL160
01270      IF PROCSI = SPACES                                           EL160
01271          GO TO 1080-EXIT.                                         EL160
01272                                                                   EL160
01273      MOVE PI-COMPANY-ID TO COMPANY-ID.                            EL160
01274      MOVE '2'           TO RECORD-TYPE.                           EL160
01275      MOVE PROCSI        TO ACCESS-CD-GENL.                        EL160
01276      MOVE ZEROS         TO SEQUENCE-NO.                           EL160
01277                                                                   EL160
01278      EXEC CICS HANDLE CONDITION                                   EL160
01279          NOTFND  (1070-PROC-NOTFND)                               EL160
01280          NOTOPEN (6030-CNTL-NOT-OPEN)                             EL160
01281      END-EXEC.                                                    EL160
01282                                                                   EL160
01283      PERFORM 5120-READ-CNTL THRU 5130-EXIT.                       EL160
01284                                                                   EL160
01285      MOVE 'X' TO PROC-SWITCH.                                     EL160
01286      GO TO 1080-EXIT.                                             EL160
01287                                                                   EL160
01288  1070-PROC-NOTFND.                                                EL160
01289                                                                      CL*17
01290      MOVE 'X' TO ERROR-SWITCH BUILD-SWITCH.                       EL160
01291      MOVE ER-0273 TO EMI-ERROR.                                   EL160
01292      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL160
01293      MOVE -1 TO CARRSL.                                              CL*17
01294                                                                   EL160
01295  1080-EXIT.                                                       EL160
01296      EXIT.                                                        EL160
01297                                                                   EL160
01298  1130-DEEDIT-DATE.                                                EL160
01299      EXEC CICS BIF DEEDIT                                         EL160
01300          FIELD  (TEST-DATE)                                       EL160
01301          LENGTH (DATE-LENGTH)                                     EL160
01302      END-EXEC.                                                    EL160
01303                                                                   EL160
01304      MOVE '4'      TO DC-OPTION-CODE.                             EL160
01305      MOVE BIF-DATE TO DC-GREG-DATE-1-MDY.                         EL160
01306      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                    EL160
01307      IF DATE-CONVERSION-ERROR                                     EL160
01308          MOVE 'X'     TO BUILD-SWITCH                             EL160
01309          MOVE ER-0304 TO EMI-ERROR                                EL160
01310          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL160
01311                                                                   EL160
01312  1140-EXIT.                                                       EL160
01313      EXIT.                                                        EL160
01314                                                                   EL160
01315  1170-DEEDIT-AMT.                                                 EL160
01316      EXEC CICS BIF DEEDIT                                         EL160
01317          FIELD  (TEST-AMT)                                        EL160
01318          LENGTH (AMT-LENGTH)                                      EL160
01319      END-EXEC.                                                    EL160
01320                                                                   EL160
01321      IF TEST-AMT NUMERIC                                          EL160
01322          GO TO 1170-EXIT.                                         EL160
01323                                                                   EL160
01324      MOVE 'X'     TO ERROR-SWITCH BUILD-SWITCH.                   EL160
01325      MOVE ER-0419 TO EMI-ERROR.                                   EL160
01326      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL160
01327                                                                   EL160
01328  1170-EXIT.                                                       EL160
01329      EXIT.                                                        EL160
01330      EJECT                                                        EL160
01331  2000-BUILD-TS.                                                   EL160
01332      MOVE ZEROS                  TO COUNT-1.                      EL160
01333      MOVE PI-COMPANY-CD          TO MSTR-COMPANY-CODE.            EL160
01334      MOVE CARRIER-CNTL           TO MSTR-CARRIER                  EL160
01335      MOVE SPACES                 TO REST-OF-KEY BUILD-SWITCH.     EL160
01336                                                                   EL160
01337      PERFORM 5000-START-BROWSE THRU 5020-EXIT.                    EL160
01338                                                                   EL160
01339      IF NO-RECORDS                                                EL160
01340          GO TO 2010-EXIT.                                         EL160
01341                                                                   EL160
01342      PERFORM 5030-READ-FILE THRU 5050-EXIT.                       EL160
01343                                                                   EL160
01344      PERFORM 3000-EDIT-RECORDS THRU 3020-EXIT                     EL160
01345          UNTIL BUILD-COMPLETE.                                    EL160
01346                                                                   EL160
01347      PERFORM 5060-END-BROWSE THRU 5070-EXIT.                      EL160
01348                                                                   EL160
01349  2010-EXIT.                                                       EL160
01350      EXIT.                                                        EL160
01351      EJECT                                                        EL160
01352  3000-EDIT-RECORDS.                                               EL160
01353      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01354                                                                      CL*17
01355      IF  PI-COMPANY-ID = 'DMD'                                       CL*17
01356              AND                                                     CL*17
01357          SETUP-ERRORS                                                CL*17
01358          GO TO 3010-GET-NEXT.                                        CL*17
01359                                                                      CL*10
01360      IF WS-SEX-SELECTION NOT = SPACES                                CL*10
01361         IF WS-SEX-SELECTION NOT = CL-INSURED-SEX-CD                  CL*10
01362             GO TO 3010-GET-NEXT.                                     CL*10
01363                                                                   EL160
01364      IF PROC-CNTL NOT = SPACES                                    EL160
01365         IF PROC-CNTL NOT = CL-PROCESSOR-ID                        EL160
01366             GO TO 3010-GET-NEXT.                                  EL160
01367                                                                   EL160
01368      IF CARRIER-CNTL NOT = SPACES                                 EL160
01369         IF CARRIER-CNTL NOT = CL-CARRIER                             CL**4
01370             MOVE 'Y'             TO BUILD-SWITCH                  EL160
01371             GO TO 3020-EXIT.                                      EL160
01372                                                                   EL160
01373      IF GROUP-CNTL NOT = SPACES                                   EL160
01374         IF GROUP-CNTL NOT = CL-CERT-GROUPING                      EL160
01375          GO TO 3010-GET-NEXT.                                     EL160
01376                                                                   EL160
01377      IF STATE-CNTL NOT = SPACES                                   EL160
01378         IF STATE-CNTL NOT = CL-CERT-STATE                         EL160
01379          GO TO 3010-GET-NEXT.                                     EL160
01380                                                                   EL160
01381      IF ACCOUNT-CNTL NOT = SPACES                                 EL160
01382          IF ACCOUNT-CNTL NOT = CL-CERT-ACCOUNT                    EL160
01383          GO TO 3010-GET-NEXT.                                     EL160
01384                                                                   EL160
01385      IF TYPE-CNTL NOT = SPACES                                    EL160
01386          IF TYPE-CNTL NOT = CL-CLAIM-TYPE                         EL160
01387          GO TO 3010-GET-NEXT.                                     EL160
01388                                                                   EL160
01389      IF DEN-CNTL NOT = SPACES                                     EL160
01390          PERFORM 3080-CHECK-DEN THRU 3080-EXIT.                   EL160
01391                                                                   EL160
01392      IF SCREEN-ERROR                                              EL160
01393          GO TO 3010-GET-NEXT.                                     EL160
01394                                                                   EL160
01395      IF PREM-CNTL NOT = SPACES                                    EL160
01396         IF PREM-CNTL NOT = CL-CLAIM-PREM-TYPE                     EL160
01397          GO TO 3010-GET-NEXT.                                     EL160
01398                                                                   EL160
01399      IF REQ-CNTL NOT = SPACES                                     EL160
01400          PERFORM 3110-CHECK-REQ THRU 3110-EXIT.                   EL160
01401                                                                   EL160
01402      IF SCREEN-ERROR                                              EL160
01403          GO TO 3010-GET-NEXT.                                     EL160
01404                                                                   EL160
01405      IF SUPR-CNTL NOT = SPACES                                    EL160
01406          PERFORM 3120-CHECK-SUPR THRU 3120-EXIT.                  EL160
01407                                                                   EL160
01408      IF SCREEN-ERROR                                              EL160
01409          GO TO 3010-GET-NEXT.                                     EL160
01410                                                                   EL160
01411      IF CERT-CNTL NOT = SPACES                                    EL160
01412          PERFORM 3130-CHECK-CERT THRU 3130-EXIT.                  EL160
01413                                                                   EL160
01414      IF SCREEN-ERROR                                              EL160
01415          GO TO 3010-GET-NEXT.                                     EL160
01416                                                                   EL160
01417      IF PRI-CNTL NOT = SPACES                                     EL160
01418         IF PRI-CNTL NOT = CL-PRIORITY-CD                          EL160
01419          GO TO 3010-GET-NEXT.                                     EL160
01420                                                                   EL160
01421      IF AUTO-CNTL NOT = SPACES                                    EL160
01422          PERFORM 3150-CHECK-AUTO THRU 3150-EXIT.                  EL160
01423                                                                   EL160
01424      IF SCREEN-ERROR                                              EL160
01425          GO TO 3010-GET-NEXT.                                     EL160
01426                                                                   EL160
01427      IF OPCL-CNTL NOT = SPACES                                    EL160
01428          PERFORM 3180-CHECK-OPCL THRU 3180-EXIT.                  EL160
01429                                                                   EL160
01430      IF SCREEN-ERROR                                              EL160
01431          GO TO 3010-GET-NEXT.                                     EL160
01432                                                                   EL160
01433      IF CL-INCURRED-DT < INC-DATE-LOW-CNTL                        EL160
01434          GO TO 3010-GET-NEXT.                                     EL160
01435                                                                   EL160
01436      IF CL-INCURRED-DT > INC-DATE-HIGH-CNTL                       EL160
01437          GO TO 3010-GET-NEXT.                                     EL160
01438                                                                   EL160
01439      IF CL-LAST-PMT-DT < LST-PMT-LOW-CNTL                         EL160
01440          GO TO 3010-GET-NEXT.                                     EL160
01441                                                                   EL160
01442      IF CL-LAST-PMT-DT > LST-PMT-HIGH-CNTL                        EL160
01443          GO TO 3010-GET-NEXT.                                     EL160
01444                                                                   EL160
01445      MOVE SPACE TO ERROR-SWITCH.                                  EL160
01446                                                                   EL160
01447      IF OPENLSI = LOW-VALUES                                      EL160
01448          NEXT SENTENCE                                            EL160
01449      ELSE                                                         EL160
01450          PERFORM 3160-CALC-ELAPSED-MONTHS THRU 3160-EXIT          EL160
01451          IF DC-ELAPSED-MONTHS <       MO-OPEN-LOW-CNTL OR         EL160
01452             DC-ELAPSED-MONTHS > MO-OPEN-HIGH-CNTL                 EL160
01453                 GO TO 3010-GET-NEXT.                              EL160
01454                                                                   EL160
01455      IF SCREEN-ERROR                                              EL160
01456          GO TO 3010-GET-NEXT.                                     EL160
01457                                                                   EL160
01458      IF AMTLSI > LOW-VALUES                                       EL160
01459          IF CL-TOTAL-PAID-AMT < AMT-PAID-LOW-CNTL                 EL160
01460              GO TO 3010-GET-NEXT.                                 EL160
01461                                                                   EL160
01462      IF AMTHSI > LOW-VALUES                                       EL160
01463          IF CL-TOTAL-PAID-AMT > AMT-PAID-HIGH-CNTL                EL160
01464              GO TO 3010-GET-NEXT.                                 EL160
01465                                                                   EL160
01466      IF CAUSELSI > SPACES                                         EL160
01467          IF CL-CAUSE-CD < CAUSE-CD-LOW-CNTL                       EL160
01468              GO TO 3010-GET-NEXT.                                 EL160
01469                                                                   EL160
01470      IF CAUSEHSI > SPACES                                         EL160
01471          IF CL-CAUSE-CD > CAUSE-CD-HIGH-CNTL                      EL160
01472              GO TO 3010-GET-NEXT.                                 EL160
01473                                                                   EL160
01474      IF REPLSI > SPACES                                           EL160
01475          IF CL-REPORTED-DT < REP-DATE-LOW-CNTL                    EL160
01476              GO TO 3010-GET-NEXT.                                 EL160
01477                                                                   EL160
01478      IF REPHSI > SPACES                                           EL160
01479          IF CL-REPORTED-DT > REP-DATE-HIGH-CNTL                   EL160
01480              GO TO 3010-GET-NEXT.                                 EL160
01481                                                                   EL160
01482      IF PMTLSI > LOW-VALUES                                       EL160
01483          IF CL-LAST-PMT-AMT < LST-PAID-LOW-CNTL                   EL160
01484              GO TO 3010-GET-NEXT.                                 EL160
01485                                                                   EL160
01486      IF PMTHSI > LOW-VALUES                                       EL160
01487          IF CL-LAST-PMT-AMT > LST-PAID-HIGH-CNTL                  EL160
01488              GO TO 3010-GET-NEXT.                                 EL160
01489                                                                   EL160
01490      IF MNTLSI > SPACES                                           EL160
01491          IF CL-LAST-MAINT-DT < MNT-DATE-LOW-CNTL                  EL160
01492              GO TO 3010-GET-NEXT.                                 EL160
01493                                                                   EL160
01494      IF MNTHSI > SPACES                                           EL160
01495          IF CL-LAST-MAINT-DT > MNT-DATE-HIGH-CNTL                 EL160
01496              GO TO 3010-GET-NEXT.                                 EL160
01497                                                                   EL160
01498      IF ESTLSI > SPACES                                           EL160
01499          IF CL-FILE-ESTABLISH-DT < EST-DATE-LOW-CNTL              EL160
01500              GO TO 3010-GET-NEXT.                                 EL160
01501                                                                   EL160
01502      IF ESTHSI > SPACES                                           EL160
01503          IF CL-FILE-ESTABLISH-DT > EST-DATE-HIGH-CNTL             EL160
01504              GO TO 3010-GET-NEXT.                                 EL160
01505                                                                   EL160
01506      IF FOLLSI > SPACES                                           EL160
01507          IF CL-NEXT-FOLLOWUP-DT < FOL-DATE-LOW-CNTL               EL160
01508              GO TO 3010-GET-NEXT.                                 EL160
01509                                                                   EL160
01510      IF FOLHSI > SPACES                                           EL160
01511          IF CL-NEXT-FOLLOWUP-DT > FOL-DATE-HIGH-CNTL              EL160
01512              GO TO 3010-GET-NEXT.                                 EL160
01513                                                                   EL160
01514      PERFORM 3170-CALC-ELAPSED-DAYS THRU 3170-EXIT.               EL160
01515                                                                   EL160
01516      IF SCREEN-ERROR                                              EL160
01517          GO TO 3010-GET-NEXT.                                     EL160
01518                                                                   EL160
01519      IF DC-ELAPSED-DAYS < DAYS-LOW-CNTL                           EL160
01520          GO TO 3010-GET-NEXT.                                     EL160
01521                                                                   EL160
01522      IF DC-ELAPSED-DAYS > DAYS-HIGH-CNTL                          EL160
01523          GO TO 3010-GET-NEXT.                                     EL160
01524                                                                   EL160
01525      PERFORM 4000-BUILD-160B THRU 4000-EXIT.                      EL160
01526                                                                   EL160
01527  3010-GET-NEXT.                                                   EL160
01528      PERFORM 5030-READ-FILE THRU 5050-EXIT.                       EL160
01529                                                                   EL160
01530      IF CL-COMPANY-CD NOT = PI-COMPANY-CD                         EL160
01531          MOVE 'Y' TO BUILD-SWITCH.                                EL160
01532                                                                   EL160
01533      IF COUNT-1 = MAX-TS-PAGES                                    EL160
01534         MOVE 'Y'                 TO BUILD-SWITCH.                 EL160
01535                                                                   EL160
01536  3020-EXIT.                                                       EL160
01537      EXIT.                                                        EL160
01538                                                                   EL160
01539  3080-CHECK-DEN.                                                  EL160
01540      IF DEN-CNTL = 'N'    AND                                        CL*13
01541         NOT CLAIM-DENIED                                             CL*13
01542          GO TO 3080-EXIT.                                            CL*13
01543                                                                      CL*13
01544      IF DEN-CNTL = 'Y'    AND                                     EL160
01545         CLAIM-DENIED                                              EL160
01546          GO TO 3080-EXIT.                                         EL160
01547                                                                   EL160
01548 ******************************************************************   CL*13
01549 *    IF DENIED CLAIMS ARE SELECTED AND THE LAST CLOSE REASON ON  *   CL*13
01550 *    THE CLAIM MASTER IS NOT 'DENIED', THE PROGRAM READS THE     *   CL*13
01551 *    ZERO TRAILER AND CHECKS THE OPEN/CLOSE HISTORY TO SEE IF    *   CL*13
01552 *    THE CLAIM HAS EVER BEEN DENIED TO DETERMINE IF IT MEETS     *   CL*13
01553 *    THE SELECTION CRITERIA.                                     *   CL*13
01554 ******************************************************************   CL*13
01555                                                                      CL*13
01556      IF DEN-CNTL = 'Y'                                               CL*13
01557        IF CL-PURGED-DT EQUAL LOW-VALUES                              CL*13
01558          MOVE CL-CONTROL-PRIMARY     TO  TRLR-MAIN-KEY               CL*13
01559          MOVE +0                     TO  TRLR-SEQ-NO                 CL*13
01560          PERFORM 5100-READ-TRLR THRU 5110-EXIT                       CL*13
01561          IF ERROR-SWITCH IS EQUAL TO 'X'                             CL*13
01562            GO TO 3080-EXIT                                           CL*13
01563          ELSE                                                        CL*13
01564            IF (AT-OPEN-CLOSE-REASON (1) = 'DENIE' OR 'DENIL') OR     CL*13
01565               (AT-OPEN-CLOSE-REASON (2) = 'DENIE' OR 'DENIL') OR     CL*13
01566               (AT-OPEN-CLOSE-REASON (3) = 'DENIE' OR 'DENIL') OR     CL*13
01567               (AT-OPEN-CLOSE-REASON (4) = 'DENIE' OR 'DENIL') OR     CL*13
01568               (AT-OPEN-CLOSE-REASON (5) = 'DENIE' OR 'DENIL') OR     CL*13
01569               (AT-OPEN-CLOSE-REASON (6) = 'DENIE' OR 'DENIL')        CL*13
01570                GO TO 3080-EXIT.                                      CL*13
01571                                                                   EL160
01572      MOVE 'X' TO ERROR-SWITCH.                                    EL160
01573                                                                   EL160
01574  3080-EXIT.                                                       EL160
01575      EXIT.                                                        EL160
01576                                                                   EL160
01577  3110-CHECK-REQ.                                                  EL160
01578      IF REQ-CNTL = 'Y' AND                                        EL160
01579         CL-NEXT-FOLLOWUP-DT > LOW-VALUES                          EL160
01580          GO TO 3110-EXIT.                                         EL160
01581                                                                   EL160
01582      IF REQ-CNTL = 'N' AND                                        EL160
01583         CL-NEXT-FOLLOWUP-DT = LOW-VALUES                          EL160
01584          GO TO 3110-EXIT.                                         EL160
01585                                                                   EL160
01586      MOVE 'X' TO ERROR-SWITCH.                                    EL160
01587                                                                   EL160
01588  3110-EXIT.                                                       EL160
01589      EXIT.                                                        EL160
01590                                                                   EL160
01591  3120-CHECK-SUPR.                                                 EL160
01592      IF SUPR-CNTL = 'Y' AND                                       EL160
01593         CL-SUPV-ATTN-CD = 'Y'                                     EL160
01594          GO TO 3120-EXIT.                                         EL160
01595                                                                   EL160
01596      IF SUPR-CNTL = 'N' AND                                       EL160
01597         (CL-SUPV-ATTN-CD = 'N' OR SPACE)                          EL160
01598          GO TO 3120-EXIT.                                         EL160
01599                                                                   EL160
01600      MOVE 'X' TO ERROR-SWITCH.                                    EL160
01601                                                                   EL160
01602  3120-EXIT.                                                       EL160
01603      EXIT.                                                        EL160
01604                                                                   EL160
01605  3130-CHECK-CERT.                                                 EL160
01606      IF CERT-CNTL = 'Y' AND                                       EL160
01607         CERT-WAS-CREATED                                          EL160
01608          GO TO 3130-EXIT.                                         EL160
01609                                                                   EL160
01610      IF CERT-CNTL = 'N' AND                                       EL160
01611         NOT CERT-WAS-CREATED                                      EL160
01612          GO TO 3130-EXIT.                                         EL160
01613                                                                   EL160
01614      MOVE 'X' TO ERROR-SWITCH.                                    EL160
01615                                                                   EL160
01616  3130-EXIT.                                                       EL160
01617      EXIT.                                                        EL160
01618                                                                   EL160
01619  3150-CHECK-AUTO.                                                 EL160
01620      IF AUTO-CNTL = 'Y' AND                                       EL160
01621         CL-AUTO-PAY-SEQ > ZERO                                    EL160
01622          GO TO 3150-EXIT.                                         EL160
01623                                                                   EL160
01624      IF AUTO-CNTL = 'N' AND                                       EL160
01625         CL-AUTO-PAY-SEQ = ZERO                                    EL160
01626          GO TO 3150-EXIT.                                         EL160
01627                                                                   EL160
01628      MOVE 'X' TO ERROR-SWITCH.                                    EL160
01629                                                                   EL160
01630  3150-EXIT.                                                       EL160
01631      EXIT.                                                        EL160
01632                                                                   EL160
01633  3160-CALC-ELAPSED-MONTHS.                                        EL160
01634      MOVE CURRENT-DATE-BIN      TO DC-BIN-DATE-2.                 EL160
01635                                                                   EL160
01636      IF CL-LAST-REOPEN-DT = LOW-VALUES                            EL160
01637          MOVE CL-REPORTED-DT    TO DC-BIN-DATE-1                  EL160
01638      ELSE                                                         EL160
01639          MOVE CL-LAST-REOPEN-DT TO DC-BIN-DATE-1.                 EL160
01640                                                                   EL160
01641      MOVE '1'          TO DC-OPTION-CODE.                         EL160
01642      MOVE SPACE        TO DC-ERROR-CODE.                          EL160
01643      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                    EL160
01644      IF DATE-CONVERSION-ERROR                                     EL160
01645          MOVE 'X' TO ERROR-SWITCH.                                EL160
01646                                                                   EL160
01647  3160-EXIT.                                                       EL160
01648      EXIT.                                                        EL160
01649                                                                   EL160
01650  3170-CALC-ELAPSED-DAYS.                                          EL160
01651      IF CL-LAST-PMT-DT = LOW-VALUES                               EL160
01652          MOVE ZERO TO DC-ELAPSED-DAYS                             EL160
01653          GO TO 3170-EXIT.                                         EL160
01654                                                                   EL160
01655      MOVE CURRENT-DATE-BIN TO DC-BIN-DATE-2.                      EL160
01656      MOVE CL-LAST-PMT-DT   TO DC-BIN-DATE-1.                      EL160
01657      MOVE '1'              TO DC-OPTION-CODE.                     EL160
01658      MOVE SPACE            TO DC-ERROR-CODE.                      EL160
01659      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                    EL160
01660      IF DATE-CONVERSION-ERROR                                     EL160
01661          MOVE 'X' TO ERROR-SWITCH.                                EL160
01662                                                                   EL160
01663  3170-EXIT.                                                       EL160
01664      EXIT.                                                        EL160
01665                                                                   EL160
01666  3180-CHECK-OPCL.                                                 EL160
01667      IF OPCL-CNTL = CL-CLAIM-STATUS                               EL160
01668          GO TO 3180-EXIT.                                         EL160
01669                                                                      CL*14
01670      IF OPCL-CNTL = 'R' AND CLAIM-IS-OPEN AND                        CL*14
01671         CL-LAST-REOPEN-DT  NOT = LOW-VALUES                          CL*14
01672            GO TO 3180-EXIT.                                          CL*14
01673                                                                   EL160
01674      MOVE 'X' TO ERROR-SWITCH.                                    EL160
01675                                                                   EL160
01676  3180-EXIT.                                                       EL160
01677      EXIT.                                                        EL160
01678      EJECT                                                        EL160
01679  4000-BUILD-160B.                                                 EL160
01680                                                                      CL**3
01681      MOVE LOW-VALUES             TO EL160BO.                      EL160
01682      MOVE SPACES                 TO PI-CONTROL-IN-PROGRESS.       EL160
01683      PERFORM 4010-MOVE-MSTR THRU 4010-EXIT.                       EL160
01684      MOVE CL-CONTROL-PRIMARY     TO TRLR-MAIN-KEY.                EL160
01685      MOVE +0                     TO TRLR-SEQ-NO.                     CL**3
01686      MOVE SPACE                  TO ERROR-SWITCH.                 EL160
01687                                                                   EL160
01688      IF CL-PURGED-DT NOT EQUAL LOW-VALUES                            CL**5
01689         MOVE 'CLAIM IS PURGED ' TO CAUSEO                            CL**5
01690         MOVE AL-SABOF           TO CAUSEA                            CL**6
01691         GO TO 4000-BYPASS-TRLRS.                                     CL**5
01692                                                                      CL**5
01693      PERFORM 5100-READ-TRLR THRU 5110-EXIT.                       EL160
01694                                                                      CL**3
01695      IF SCREEN-ERROR                                              EL160
01696         MOVE ER-0205            TO SCNERRO                           CL**3
01697      ELSE                                                         EL160
01698         MOVE CL-CONTROL-PRIMARY     TO TRLR-MAIN-KEY                 CL**3
01699         MOVE +90                    TO TRLR-SEQ-NO                   CL**3
01700         MOVE SPACE                  TO ERROR-SWITCH                  CL**3
01701         PERFORM 5100-READ-TRLR THRU 5110-EXIT                        CL**3
01702         IF NOT SCREEN-ERROR                                          CL**3
01703            IF AT-TRAILER-TYPE EQUAL '6'                              CL**3
01704               MOVE AT-INFO-LINE-1       TO CAUSEO                    CL**3
01705               PERFORM 4020-MOVE-TRLR THRU 4020-EXIT.                 CL**3
01706                                                                      CL**5
01707  4000-BYPASS-TRLRS.                                                  CL**5
01708                                                                   EL160
01709      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*16
01710          GO TO 4000-READ-EMPLCY.                                     CL*16
01711                                                                      CL*16
01712      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.            EL160
01713      MOVE CL-CERT-CARRIER        TO CERT-CARRIER PI-CARRIER.      EL160
01714      MOVE CL-CERT-GROUPING       TO CERT-GROUP.                   EL160
01715      MOVE CL-CERT-STATE          TO CERT-STATE.                   EL160
01716      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.                 EL160
01717      MOVE CL-CERT-NO             TO CERT-CERT PI-CERT-NO.         EL160
01718      MOVE CL-CERT-EFF-DT         TO CERT-DATE.                    EL160
01719      MOVE SPACE                  TO ERROR-SWITCH.                 EL160
01720                                                                   EL160
01721      PERFORM 5080-READ-CERT THRU 5090-EXIT.                       EL160
01722                                                                   EL160
01723      IF SCREEN-ERROR                                              EL160
01724          MOVE ER-0206            TO SCNERRO                       EL160
01725      ELSE                                                         EL160
01726          PERFORM 4030-MOVE-CERT THRU 4030-EXIT.                   EL160
01727                                                                   EL160
01728      MOVE PI-CONTROL-IN-PROGRESS TO PIKEYO.                       EL160
01729      MOVE PI-UPDATE-BY           TO USERSAVO.                     EL160
01730      MOVE PI-UPDATE-HHMMSS       TO TIMESAVO.                     EL160
01731                                                                      CL*16
01732      PERFORM 5140-WRITE-TS THRU 5150-EXIT.                           CL*16
01733                                                                      CL*16
01734      GO TO 4000-EXIT.                                                CL*16
01735                                                                      CL*16
01736  4000-READ-EMPLCY.                                                   CL*16
01737                                                                      CL*16
01738      MOVE CL-COMPANY-CD          TO  EMPLCY-COMPANY-CD.              CL*16
01739      MOVE CL-CERT-CARRIER        TO  EMPLCY-CARRIER                  CL*16
01740                                      PI-CARRIER.                     CL*16
01741      MOVE CL-CERT-GROUPING       TO  EMPLCY-GROUPING.                CL*16
01742      MOVE CL-CERT-STATE          TO  EMPLCY-STATE.                   CL*16
01743      MOVE CL-CERT-ACCOUNT        TO  EMPLCY-PRODUCER.                CL*16
01744      MOVE CL-CERT-EFF-DT         TO  EMPLCY-EFF-DT.                  CL*16
01745      MOVE CL-CV-REFERENCE-NO     TO  EMPLCY-REFERENCE-NO             CL*16
01746                                      PI-MP-REFERENCE-NO.             CL*16
01747      MOVE CL-CERT-NO             TO  PI-CERT-NO.                     CL*16
01748      MOVE SPACE                  TO  ERROR-SWITCH.                   CL*16
01749                                                                      CL*16
01750      PERFORM 5095-READ-EMPLCY THRU 5095-EXIT.                        CL*16
01751                                                                      CL*16
01752      IF SCREEN-ERROR                                                 CL*16
01753          MOVE ER-9483            TO  SCNERRO                         CL*16
01754          GO TO 4000-EXIT.                                            CL*16
01755                                                                      CL*16
01756      MOVE PM-COMPANY-CD          TO  EMPLAN-COMPANY-CD.              CL*16
01757      MOVE PM-CARRIER             TO  EMPLAN-CARRIER.                 CL*16
01758      MOVE PM-GROUPING            TO  EMPLAN-GROUPING.                CL*16
01759      MOVE PM-STATE               TO  EMPLAN-STATE.                   CL*16
01760      MOVE PM-PRODUCER            TO  EMPLAN-PRODUCER.                CL*16
01761      MOVE PM-INS-PLAN-CD         TO  EMPLAN-PLAN-CODE.               CL*16
01762      MOVE PM-INS-PLAN-REVISION   TO  EMPLAN-REV-NO.                  CL*16
01763                                                                      CL*16
01764      PERFORM 5096-READ-EMPLAN THRU 5096-EXIT.                        CL*16
01765                                                                      CL*16
01766      IF SCREEN-ERROR                                                 CL*16
01767          MOVE ER-9811            TO  SCNERRO                         CL*16
01768      ELSE                                                            CL*16
01769          PERFORM 4030-MOVE-EMPLCY THRU 4030-EMPLCY-EXIT.             CL*16
01770                                                                      CL*16
01771      MOVE PI-CONTROL-IN-PROGRESS TO  PIKEYO.                         CL*16
01772      MOVE PI-UPDATE-BY           TO  USERSAVO.                       CL*16
01773      MOVE PI-UPDATE-HHMMSS       TO  TIMESAVO.                       CL*16
01774                                                                   EL160
01775      PERFORM 5140-WRITE-TS THRU 5150-EXIT.                        EL160
01776                                                                   EL160
01777  4000-EXIT.                                                       EL160
01778      EXIT.                                                        EL160
01779      EJECT                                                        EL160
01780  4010-MOVE-MSTR.                                                  EL160
01781      MOVE CL-CLAIM-NO            TO CLAIMO PI-CLAIM-NO.           EL160
01782      MOVE CL-CLAIM-TYPE          TO TYPEO.                        EL160
01783      MOVE CL-CERT-PRIME          TO CERTO.                        EL160
01784      MOVE CL-CERT-SFX            TO CERTSXO.                      EL160
01785      MOVE CL-CCN                 TO CREDCDO.                         CL*17
01786      MOVE CL-CERT-CARRIER        TO CARRO.                        EL160
01787      MOVE CL-CLAIM-STATUS        TO STATUSO.                      EL160
01788      MOVE CL-PROCESSOR-ID        TO PROCO.                        EL160
01789      MOVE CL-INSURED-LAST-NAME   TO MLNAMEO.                      EL160
01790      MOVE CL-INSURED-1ST-NAME    TO MFNAMEO.                      EL160
01791      MOVE CL-INSURED-MID-INIT    TO MMINITO.                      EL160
01792      MOVE CL-INSURED-SEX-CD      TO SEXO.                         EL160
01793                                                                   EL160
01794      IF CL-INSURED-BIRTH-DT > LOW-VALUES                          EL160
01795          MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1                EL160
01796          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL160
01797          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                 EL160
01798          MOVE DC-GREG-DATE-1-EDIT TO BIRTHO                       EL160
01799      ELSE                                                         EL160
01800          MOVE SPACES TO BIRTHO.                                   EL160
01801                                                                   EL160
01802      IF CL-SSN-STATE   = CL-CERT-STATE AND                        EL160
01803         CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME                       CL**8
01804          MOVE SPACES        TO SOCIALO                            EL160
01805      ELSE                                                         EL160
01806          MOVE CL-SOC-SEC-NO TO SOCIALO.                           EL160
01807                                                                   EL160
01808      MOVE CL-INSURED-OCC-CD    TO OCCO.                           EL160
01809      MOVE CL-BENEFICIARY       TO CBENEO.                            CL*15
01810      MOVE CL-CAUSE-CD          TO CCAUSCDO.                       EL160
01811                                                                   EL160
01812      IF CL-EST-END-OF-DISAB-DT > LOW-VALUES                       EL160
01813          MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1             EL160
01814          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL160
01815          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                 EL160
01816          MOVE DC-GREG-DATE-1-EDIT TO ENDO                         EL160
01817      ELSE                                                         EL160
01818          MOVE SPACES TO ENDO.                                     EL160
01819                                                                   EL160
01820      IF CL-PAID-THRU-DT > LOW-VALUES                              EL160
01821         IF NOT PI-USES-PAID-TO                                       CL**3
01822            MOVE CL-PAID-THRU-DT      TO  DC-BIN-DATE-1               CL**9
01823            MOVE SPACES               TO  DC-OPTION-CODE              CL**9
01824                                          DC-ERROR-CODE               CL**9
01825            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                  CL**3
01826            MOVE DC-GREG-DATE-1-EDIT  TO  PDTHRUO                     CL**9
01827         ELSE                                                         CL**3
01828            MOVE WS-PAID-TO-HDG       TO  BHEADO                      CL**9
01829            MOVE CL-PAID-THRU-DT      TO  DC-BIN-DATE-1               CL**9
01830            MOVE '6'                  TO  DC-OPTION-CODE              CL**9
01831            MOVE +1                   TO  DC-ELAPSED-DAYS             CL**9
01832            MOVE +0                   TO  DC-ELAPSED-MONTHS           CL**9
01833            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                  CL**3
01834            MOVE DC-GREG-DATE-1-EDIT  TO  PDTHRUO                     CL**9
01835      ELSE                                                         EL160
01836         MOVE SPACES                  TO  PDTHRUO                     CL**9
01837         IF PI-USES-PAID-TO                                           CL**9
01838            MOVE WS-PAID-TO-HDG       TO  BHEADO.                     CL**9
01839                                                                   EL160
01840      MOVE CL-TOTAL-PAID-AMT  TO PDAMTO.                           EL160
01841      MOVE CL-NO-OF-DAYS-PAID TO NODAYSO.                          EL160
01842      MOVE CL-NO-OF-PMTS-MADE TO NOPMTSO.                          EL160
01843                                                                   EL160
01844      IF CL-INCURRED-DT > LOW-VALUES                               EL160
01845          MOVE CL-INCURRED-DT TO DC-BIN-DATE-1                     EL160
01846          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL160
01847          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                 EL160
01848          MOVE DC-GREG-DATE-1-EDIT TO INCO                         EL160
01849      ELSE                                                         EL160
01850          MOVE SPACES TO INCO.                                     EL160
01851                                                                   EL160
01852      IF CL-REPORTED-DT > LOW-VALUES                               EL160
01853          MOVE CL-REPORTED-DT TO DC-BIN-DATE-1                     EL160
01854          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL160
01855          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                 EL160
01856          MOVE DC-GREG-DATE-1-EDIT TO REPO                         EL160
01857      ELSE                                                         EL160
01858          MOVE SPACES TO REPO.                                     EL160
01859                                                                   EL160
01860      IF CL-FILE-ESTABLISH-DT > LOW-VALUES                         EL160
01861          MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1               EL160
01862          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL160
01863          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                 EL160
01864          MOVE DC-GREG-DATE-1-EDIT TO ESTO                         EL160
01865      ELSE                                                         EL160
01866          MOVE SPACES TO ESTO.                                     EL160
01867                                                                   EL160
01868 *    IF CL-LAST-PMT-DT > LOW-VALUES                               EL160
01869 *        MOVE CL-LAST-PMT-DT TO DC-BIN-DATE-1                     EL160
01870 *        MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL160
01871 *        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                 EL160
01872 *        MOVE DC-GREG-DATE-1-EDIT TO LSTPMTO                      EL160
01873 *    ELSE                                                         EL160
01874 *        MOVE SPACES TO LSTPMTO.                                  EL160
01875                                                                   EL160
01876 *    MOVE CL-LAST-PMT-AMT TO LSTAMTO.                             EL160
01877                                                                   EL160
01878      IF CL-LAST-MAINT-DT > LOW-VALUES                             EL160
01879          MOVE CL-LAST-MAINT-DT TO DC-BIN-DATE-1                   EL160
01880          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL160
01881          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                 EL160
01882          MOVE DC-GREG-DATE-1-EDIT TO MNTDTO                       EL160
01883      ELSE                                                         EL160
01884          MOVE SPACES TO MNTDTO.                                   EL160
01885                                                                   EL160
01886      IF CL-LAST-MAINT-TYPE = SPACE                                EL160
01887          MOVE 'SET UP' TO MNTTYPEO                                EL160
01888      ELSE                                                         EL160
01889          IF CL-LAST-MAINT-TYPE = '1'                              EL160
01890              MOVE 'PAYMNT' TO MNTTYPEO                            EL160
01891          ELSE                                                     EL160
01892              IF CL-LAST-MAINT-TYPE = '2'                          EL160
01893                  MOVE 'LETTER' TO MNTTYPEO                        EL160
01894              ELSE                                                 EL160
01895                  IF CL-LAST-MAINT-TYPE = '3'                      EL160
01896                      MOVE 'UPDATE' TO MNTTYPEO                    EL160
01897                  ELSE                                             EL160
01898                      IF CL-LAST-MAINT-TYPE = '4'                  EL160
01899                          MOVE 'RESTOR' TO MNTTYPEO                EL160
01900                      ELSE                                         EL160
01901                          IF CL-LAST-MAINT-TYPE = '5'              EL160
01902                              MOVE 'INC DT' TO MNTTYPEO            EL160
01903                          ELSE                                     EL160
01904                              IF CL-LAST-MAINT-TYPE = '6'          EL160
01905                                  MOVE ' CONV'  TO MNTTYPEO        EL160
01906                              ELSE                                 EL160
01907                                  MOVE SPACES TO MNTTYPEO.         EL160
01908                                                                   EL160
01909      MOVE CL-PRIORITY-CD       TO PRICDO.                         EL160
01910      MOVE CL-SUPV-ATTN-CD      TO SUPVO.                          EL160
01911      MOVE CL-FILE-LOCATION     TO FILEO.                          EL160
01912      MOVE CL-LAST-MAINT-USER   TO PI-UPDATE-BY.                   EL160
01913      MOVE CL-LAST-MAINT-HHMMSS TO PI-UPDATE-HHMMSS.               EL160
01914                                                                   EL160
01915  4010-EXIT.                                                       EL160
01916      EXIT.                                                        EL160
01917      EJECT                                                        EL160
01918  4020-MOVE-TRLR.                                                  EL160
01919                                                                   EL160
01920  4020-EXIT.                                                       EL160
01921      EXIT.                                                        EL160
01922      EJECT                                                        EL160
01923  4030-MOVE-CERT.                                                  EL160
01924      IF CM-CERT-EFF-DT > LOW-VALUES                               EL160
01925          MOVE CM-CERT-EFF-DT TO DC-BIN-DATE-1                     EL160
01926          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL160
01927          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                 EL160
01928          MOVE DC-GREG-DATE-1-EDIT TO CERTEFFO                     EL160
01929      ELSE                                                         EL160
01930          MOVE SPACES              TO CERTEFFO.                    EL160
01931                                                                   EL160
01932      MOVE CM-ACCOUNT              TO CERTACTO.                    EL160
01933      MOVE CM-STATE                TO CERTSTO PI-STATE.            EL160
01934      MOVE CM-GROUPING             TO CERTGRPO.                    EL160
01935      MOVE CM-CARRIER              TO CERTCARO.                    EL160
01936      MOVE CM-INSURED-LAST-NAME    TO CLNAMEO.                     EL160
01937      MOVE CM-INSURED-FIRST-NAME   TO CFNAMEO.                     EL160
01938      MOVE CM-INSURED-INITIAL2     TO CINITO.                      EL160
01939      MOVE CM-JT-LAST-NAME         TO CJLNAMEO.                    EL160
01940      MOVE CM-JT-FIRST-NAME        TO CJFAMEO.                     EL160
01941      MOVE CM-JT-INITIAL           TO CJINITO.                     EL160
01942                                                                   EL160
01943      MOVE CM-INSURED-ISSUE-AGE    TO INSAGEO.                     EL160
01944      MOVE CM-INSURED-JOINT-AGE    TO JAGEO.                       EL160
01945      IF CM-SSN-STATE   = CM-STATE AND                             EL160
01946         CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME                            CL**8
01947          MOVE SPACES       TO SOCSECO                             EL160
01948      ELSE                                                         EL160
01949         MOVE CM-SOC-SEC-NO TO SOCSECO.                            EL160
01950                                                                   EL160
01951      PERFORM 9300-GET-FREE-LOOK THRU 9300-EXIT.                      CL*18
01952                                                                      CL*18
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
121802         CONTINUE
01955      ELSE                                                            CL**7
01956          GO TO 4030-MOVE-CERT-AH.                                 EL160
01957                                                                      CL**7
01958      IF CM-LF-BENEFIT-CD = '00'                                      CL**7
01959          GO TO 4030-MOVE-REST-OF-CERT.                               CL**7
01960                                                                   EL160
01961      MOVE CM-LF-BENEFIT-CD       TO CVCDO HOLD-BENEFIT            EL160
01962      MOVE PI-LIFE-OVERRIDE-L6    TO CVDESCRO.                     EL160
01963      MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM              EL160
01964                                     CVOTRMO.                      EL160
01965      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.               EL160
01966      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.               CL**2
01967      MOVE CL-INCURRED-DT         TO CP-VALUATION-DT.                 CL**7
01968      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.          CL*11
01969      MOVE '4'                    TO CP-REM-TERM-METHOD.           EL160
01970      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.                EL160
01971      PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.                   EL160
01972      MOVE CP-REMAINING-TERM-3    TO CVRTRMO.                      EL160
01973      MOVE CM-LF-BENEFIT-AMT      TO CVOBENEO.                     EL160
01974      MOVE CM-POLICY-FORM-NO      TO CVFORMO.                      EL160
01975                                                                   EL160
01976      IF CM-LF-CURRENT-STATUS = '8'                                EL160
01977         IF CM-LF-CANCEL-DT NOT = LOW-VALUES                       EL160
01978             MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1                 EL160
01979             MOVE ' '             TO DC-OPTION-CODE                EL160
01980             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL160
01981             IF NOT DATE-CONVERSION-ERROR                          EL160
01982                 MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.             EL160
01983                                                                   EL160
01984      IF CM-LF-CURRENT-STATUS = '7'                                EL160
01985         IF CM-LF-DEATH-DT NOT = LOW-VALUES                        EL160
01986             MOVE CM-LF-DEATH-DT     TO DC-BIN-DATE-1              EL160
01987             MOVE SPACE              TO DC-OPTION-CODE             EL160
01988             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL160
01989             IF NOT DATE-CONVERSION-ERROR                          EL160
01990                 MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.             EL160
01991                                                                   EL160
01992      IF CM-LF-CURRENT-STATUS = '8'                                EL160
01993         IF CM-LF-CANCEL-EXIT-DT NOT = LOW-VALUES                  EL160
01994             MOVE CM-LF-CANCEL-EXIT-DT TO DC-BIN-DATE-1            EL160
01995             MOVE SPACE                TO DC-OPTION-CODE           EL160
01996             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL160
01997             IF NOT DATE-CONVERSION-ERROR                          EL160
01998                 MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.              EL160
01999                                                                   EL160
02000      IF CM-LF-CURRENT-STATUS = '7'                                EL160
02001         IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES                   EL160
02002             MOVE CM-LF-DEATH-EXIT-DT  TO DC-BIN-DATE-1            EL160
02003             MOVE SPACE                TO DC-OPTION-CODE           EL160
02004             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL160
02005             IF NOT DATE-CONVERSION-ERROR                          EL160
02006                 MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.              EL160
02007                                                                   EL160
02008      IF CM-LF-CURRENT-STATUS = '1' OR '4'                         EL160
02009         IF CP-REMAINING-TERM-3 = ZERO                             EL160
02010            MOVE 'EXPIRED'           TO CVSTATO                    EL160
02011         ELSE                                                      EL160
02012            MOVE 'ACTIVE'            TO CVSTATO.                   EL160
02013                                                                   EL160
02014      IF CM-LF-CURRENT-STATUS = '2'                                EL160
02015         MOVE 'PEND   '           TO CVSTATO.                      EL160
02016      IF CM-LF-CURRENT-STATUS = '3'                                EL160
02017         MOVE 'RESTORE'           TO CVSTATO.                      EL160
02018      IF CM-LF-CURRENT-STATUS = '5'                                EL160
02019         MOVE 'REISSUE'           TO CVSTATO.                      EL160
02020      IF CM-LF-CURRENT-STATUS = '6'                                EL160
02021         MOVE 'LMP DIS'           TO CVSTATO.                      EL160
02022      IF CM-LF-CURRENT-STATUS = '7'                                EL160
02023         MOVE 'DEATH  '           TO CVSTATO.                      EL160
02024      IF CM-LF-CURRENT-STATUS = '8'                                EL160
02025         MOVE 'CANCEL '           TO CVSTATO.                      EL160
02026      IF CM-LF-CURRENT-STATUS = '9'                                EL160
02027         MOVE 'RE-ONLY'           TO CVSTATO.                      EL160
02028      IF CM-LF-CURRENT-STATUS = 'V'                                   CL*12
02029         MOVE 'VOID   '           TO CVSTATO.                         CL*12
02030      IF CM-LF-CURRENT-STATUS = 'D'                                   CL*12
02031         MOVE 'DECLINE'           TO CVSTATO.                         CL*12
02032                                                                   EL160
02033      MOVE SPACES           TO BENEFIT-KEY ERROR-SWITCH.           EL160
02034      MOVE PI-COMPANY-ID    TO BEN-CO-ID.                          EL160
02035      MOVE '4'              TO BEN-REC-TYPE.                       EL160
02036      MOVE CM-LF-BENEFIT-CD TO BEN-ACC-CD.                         EL160
02037      MOVE ZEROS            TO BEN-SEQ-NO.                         EL160
02038                                                                   EL160
02039      PERFORM 5260-READ-BENEFIT THRU 5280-EXIT.                    EL160
02040                                                                   EL160
02041      IF SCREEN-ERROR                                              EL160
02042          MOVE ER-0282 TO SCNERRO                                  EL160
02043          GO TO 4030-MOVE-CERT-AH.                                 EL160
02044                                                                   EL160
02045      MOVE ZEROS TO COUNT-2.                                       EL160
02046      PERFORM 4040-FIND-BENEFIT THRU 4060-EXIT.                    EL160
02047      IF SCREEN-ERROR                                              EL160
02048          MOVE ER-0282 TO SCNERRO                                  EL160
02049          GO TO 4030-MOVE-CERT-AH.                                 EL160
02050                                                                   EL160
02051      MOVE CF-BENEFIT-ALPHA (COUNT-2) TO CVKINDO.                  EL160
02052                                                                   EL160
02053  4030-MOVE-CERT-AH.                                               EL160
02054                                                                      CL**7
052614     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
080322                                          OR 'B' OR 'H'
121802         CONTINUE
02057      ELSE                                                            CL**7
02058          GO TO 4030-MOVE-REST-OF-CERT.                               CL**7
02059                                                                      CL**7
02060      IF CM-AH-BENEFIT-CD = '00'                                      CL**3
02061          GO TO 4030-MOVE-REST-OF-CERT.                            EL160
02062                                                                   EL160
02063      MOVE PI-AH-OVERRIDE-L6      TO CVDESCRO.                     EL160
02064                                                                   EL160
02065      MOVE CM-AH-BENEFIT-CD TO CVCDO HOLD-BENEFIT.                 EL160
02066      MOVE CM-AH-ORIG-TERM        TO CP-ORIGINAL-TERM              EL160
02067                                     CVOTRMO.                      EL160
02068      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.               EL160
02069      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.               CL**2
02070      MOVE CURRENT-DATE-BIN       TO CP-VALUATION-DT.              EL160
02071      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.          CL*11
02072      MOVE '4'                    TO CP-REM-TERM-METHOD.           EL160
02073      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.                EL160
02074                                                                   EL160
02075      PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.                   EL160
02076                                                                   EL160
02077      MOVE CP-REMAINING-TERM-3    TO CVRTRMO.                      EL160
02078      MOVE CM-AH-BENEFIT-AMT      TO CVOBENEO.                     EL160
02079      MOVE CM-POLICY-FORM-NO      TO CVFORMO.                      EL160
02080                                                                   EL160
02081      IF CM-AH-CURRENT-STATUS = '8'                                EL160
02082         IF CM-AH-CANCEL-DT NOT = LOW-VALUES                       EL160
02083             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1                 EL160
02084             MOVE ' '             TO DC-OPTION-CODE                EL160
02085             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL160
02086             IF NOT DATE-CONVERSION-ERROR                          EL160
02087                 MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.             EL160
02088                                                                   EL160
02089      IF CM-AH-CURRENT-STATUS = '6'                                EL160
02090         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES                   EL160
02091             MOVE CM-AH-SETTLEMENT-DT     TO DC-BIN-DATE-1         EL160
02092             MOVE ' '             TO DC-OPTION-CODE                EL160
02093             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL160
02094             IF NOT DATE-CONVERSION-ERROR                          EL160
02095                 MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.             EL160
02096                                                                   EL160
02097      IF CM-AH-CURRENT-STATUS = '8'                                EL160
02098         IF CM-AH-CANCEL-EXIT-DT NOT = LOW-VALUES                  EL160
02099             MOVE CM-AH-CANCEL-EXIT-DT TO DC-BIN-DATE-1            EL160
02100             MOVE ' '             TO DC-OPTION-CODE                EL160
02101             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL160
02102             IF NOT DATE-CONVERSION-ERROR                          EL160
02103                 MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.              EL160
02104                                                                   EL160
02105      IF CM-AH-CURRENT-STATUS = '6'                                EL160
02106         IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES              EL160
02107             MOVE CM-AH-SETTLEMENT-EXIT-DT     TO DC-BIN-DATE-1    EL160
02108             MOVE ' '             TO DC-OPTION-CODE                EL160
02109             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL160
02110             IF NOT DATE-CONVERSION-ERROR                          EL160
02111                 MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.              EL160
02112                                                                   EL160
02113      IF CM-AH-CURRENT-STATUS = '1' OR = '4'                       EL160
02114         IF CP-REMAINING-TERM-3 = ZEROS                            EL160
02115            MOVE 'EXPIRED'           TO CVSTATO                    EL160
02116         ELSE                                                      EL160
02117            MOVE 'ACTIVE'            TO CVSTATO.                   EL160
02118                                                                   EL160
02119      IF CM-AH-CURRENT-STATUS = '2'                                EL160
02120         MOVE 'PEND   '           TO CVSTATO.                      EL160
02121      IF CM-AH-CURRENT-STATUS = '3'                                EL160
02122         MOVE 'RESTORE'           TO CVSTATO.                      EL160
02123      IF CM-AH-CURRENT-STATUS = '5'                                EL160
02124         MOVE 'REISSUE'           TO CVSTATO.                      EL160
02125      IF CM-AH-CURRENT-STATUS = '6'                                EL160
02126         MOVE 'LMP DIS'           TO CVSTATO.                      EL160
02127      IF CM-AH-CURRENT-STATUS = '7'                                EL160
02128         MOVE 'DEATH  '           TO CVSTATO.                      EL160
02129      IF CM-AH-CURRENT-STATUS = '8'                                EL160
02130         MOVE 'CANCEL '           TO CVSTATO.                      EL160
02131      IF CM-AH-CURRENT-STATUS = '9'                                EL160
02132         MOVE 'RE-ONLY'           TO CVSTATO.                      EL160
02133      IF CM-AH-CURRENT-STATUS = 'V'                                   CL*12
02134         MOVE 'VOID   '           TO CVSTATO.                         CL*12
02135      IF CM-AH-CURRENT-STATUS = 'D'                                   CL*12
02136         MOVE 'DECLINE'           TO CVSTATO.                         CL*12
02137                                                                   EL160
02138      MOVE SPACES           TO BENEFIT-KEY ERROR-SWITCH.           EL160
02139      MOVE PI-COMPANY-ID    TO BEN-CO-ID.                          EL160
02140      MOVE '5'              TO BEN-REC-TYPE.                       EL160
02141      MOVE CM-AH-BENEFIT-CD TO BEN-ACC-CD.                         EL160
02142      MOVE ZEROES           TO BEN-SEQ-NO.                         EL160
02143                                                                   EL160
02144      PERFORM 5260-READ-BENEFIT THRU 5280-EXIT.                    EL160
02145                                                                   EL160
02146      IF SCREEN-ERROR                                              EL160
02147          PERFORM 4070-CHECK-ERROR THRU 4080-EXIT                  EL160
02148          GO TO 4030-MOVE-REST-OF-CERT.                            EL160
02149                                                                   EL160
02150      MOVE ZEROS TO COUNT-2.                                       EL160
02151      PERFORM 4040-FIND-BENEFIT THRU 4060-EXIT.                    EL160
02152                                                                   EL160
02153      IF SCREEN-ERROR                                              EL160
02154          PERFORM 4070-CHECK-ERROR THRU 4080-EXIT                  EL160
02155          GO TO 4030-MOVE-REST-OF-CERT.                            EL160
02156                                                                   EL160
02157      MOVE CF-BENEFIT-ALPHA (COUNT-2) TO CVKINDO.                  EL160
02158                                                                   EL160
02159  4030-MOVE-REST-OF-CERT.                                          EL160
02160      MOVE CM-LOAN-NUMBER         TO LOANNOO.                      EL160
02161      MOVE CM-LOAN-BALANCE        TO LOANBALO.                     EL160
02162      MOVE CM-LOAN-APR            TO CAPRO.                        EL160
02163      MOVE CM-IND-GRP-TYPE        TO CINDGRPO.                     EL160
02164                                                                   EL160
02165      IF CM-SING-PRM                                               EL160
02166          MOVE 'SP'               TO CPREMTPO                      EL160
02167      ELSE                                                         EL160
02168          IF CM-O-B-COVERAGE                                       EL160
02169              MOVE 'OB'           TO CPREMTPO                      EL160
02170          ELSE                                                     EL160
02171              IF CM-OPEN-END                                       EL160
02172                  MOVE 'OE'       TO CPREMTPO                      EL160
02173              ELSE                                                 EL160
02174                  MOVE SPACES     TO CPREMTPO.                     EL160
02175                                                                   EL160
02176      MOVE CM-REIN-TABLE          TO CREINCDO.                     EL160
02177                                                                   EL160
02178  4030-EXIT.                                                       EL160
02179      EXIT.                                                           CL*16
02180      EJECT                                                           CL*16
02181  4030-MOVE-EMPLCY.                                                   CL*16
02182                                                                      CL*16
02183      IF PM-POLICY-EFF-DT > LOW-VALUES                                CL*16
02184          MOVE PM-POLICY-EFF-DT       TO  DC-BIN-DATE-1               CL*16
02185          MOVE SPACES                 TO  DC-OPTION-CODE              CL*16
02186                                          DC-ERROR-CODE               CL*16
02187          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*16
02188          MOVE DC-GREG-DATE-1-EDIT    TO  CERTEFFO                    CL*16
02189      ELSE                                                            CL*16
02190          MOVE SPACES                 TO  CERTEFFO.                   CL*16
02191                                                                      CL*16
02192      MOVE PM-PRODUCER             TO  CERTACTO.                      CL*16
02193      MOVE PM-STATE                TO  CERTSTO PI-STATE.              CL*16
02194      MOVE PM-GROUPING             TO  CERTGRPO.                      CL*16
02195      MOVE PM-CARRIER              TO  CERTCARO.                      CL*16
02196      MOVE PM-INSURED-LAST-NAME    TO  CLNAMEO.                       CL*16
02197      MOVE PM-INSURED-FIRST-NAME   TO  CFNAMEO.                       CL*16
02198      MOVE PM-INSURED-MIDDLE-INIT  TO  CINITO.                        CL*16
02199      MOVE PM-JOINT-LAST-NAME      TO  CJLNAMEO.                      CL*16
02200      MOVE PM-JOINT-FIRST-NAME     TO  CJFAMEO.                       CL*16
02201      MOVE PM-JOINT-MIDDLE-INIT    TO  CJINITO.                       CL*16
02202                                                                      CL*16
02203      MOVE PM-INSURED-ISSUE-AGE    TO  WS-AGE.                        CL*16
02204      MOVE WS-AGE-3-4              TO  INSAGEO.                       CL*16
02205      MOVE PM-JOINT-ISSUE-AGE      TO  WS-AGE.                        CL*16
02206      MOVE WS-AGE-3-4              TO  JAGEO.                         CL*16
02207                                                                      CL*16
02208      IF PM-SSN-STATE   = PM-STATE AND                                CL*16
02209         PM-SSN-PRODUCER = PM-PRODUCER-PRIME                          CL*16
02210          MOVE SPACES              TO  SOCSECO                        CL*16
02211      ELSE                                                            CL*16
02212          MOVE PM-SOC-SEC-NO       TO  SOCSECO.                       CL*16
02213                                                                      CL*18
02214      PERFORM 9300-GET-FREE-LOOK THRU 9300-EXIT.                      CL*18
02215                                                                      CL*16
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
121802         CONTINUE
02218      ELSE                                                            CL*16
02219          GO TO 4030-MOVE-EMPLCY-AH.                                  CL*16
02220                                                                      CL*16
02221      IF PP-BENEFIT-IS-LEVEL                                          CL*16
02222          MOVE 'L'                TO  CP-BENEFIT-TYPE                 CL*16
02223      ELSE                                                            CL*16
02224          MOVE 'R'                TO  CP-BENEFIT-TYPE.                CL*16
02225                                                                      CL*16
02226      MOVE PP-REFUND-CALC         TO  CP-EARNING-METHOD               CL*16
02227                                      CP-RATING-METHOD.               CL*16
02228                                                                      CL*16
02229      MOVE PI-LIFE-OVERRIDE-L6    TO  CVDESCRO.                       CL*16
02230      MOVE PM-INS-PLAN-CD         TO  CVCDO                           CL*16
02231                                      HOLD-BENEFIT.                   CL*16
02232      MOVE PM-LOAN-TERM           TO  CP-ORIGINAL-TERM                CL*16
02233                                      CVOTRMO.                        CL*16
02234      MOVE PM-POLICY-EFF-DT       TO  CP-CERT-EFF-DT                  CL*16
02235                                      CP-FIRST-PAY-DATE.              CL*16
02236      MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT.                CL*16
02237      MOVE '1'                    TO  CP-REM-TRM-CALC-OPTION.         CL*16
02238      MOVE '2'                    TO  CP-REM-TERM-METHOD              CL*16
02239                                      CP-PROCESS-TYPE.                CL*16
02240      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.                  CL*16
02241      MOVE PM-COMPANY-CD          TO  CP-COMPANY-CD.                  CL*16
02242                                                                      CL*16
02243      PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.                      CL*16
02244                                                                      CL*16
02245      IF (PI-COMPANY-ID IS EQUAL TO 'CIG' OR 'CUK')                   CL*16
02246          COMPUTE CP-REMAINING-TERM-3 = CP-REMAINING-TERM-3 + 1       CL*16
02247          MOVE CP-REMAINING-TERM-3    TO  CVRTRMO                     CL*16
02248      ELSE                                                            CL*16
02249          MOVE CP-REMAINING-TERM-3    TO  CVRTRMO.                    CL*16
02250                                                                      CL*16
02251      MOVE PM-INS-TOTAL-BENEFIT   TO  CVOBENEO.                       CL*16
02252                                                                      CL*16
02253      GO TO 4030-MOVE-REST-OF-EMPLCY.                                 CL*16
02254                                                                      CL*16
02255  4030-MOVE-EMPLCY-AH.                                                CL*16
02256                                                                      CL*16
02257      MOVE PI-AH-OVERRIDE-L6      TO CVDESCRO.                        CL*16
02258                                                                      CL*16
02259      IF PP-BENEFIT-IS-LEVEL                                          CL*16
02260          MOVE 'L'                TO  CP-BENEFIT-TYPE                 CL*16
02261      ELSE                                                            CL*16
02262          MOVE 'R'                TO  CP-BENEFIT-TYPE.                CL*16
02263                                                                      CL*16
02264      MOVE PP-REFUND-CALC         TO  CP-EARNING-METHOD               CL*16
02265                                      CP-RATING-METHOD.               CL*16
02266                                                                      CL*16
02267      MOVE PI-AH-OVERRIDE-L6      TO  CVDESCRO.                       CL*16
02268      MOVE PM-INS-PLAN-CD         TO  CVCDO                           CL*16
02269                                      HOLD-BENEFIT.                   CL*16
02270      MOVE PM-LOAN-TERM           TO  CP-ORIGINAL-TERM                CL*16
02271                                      CVOTRMO.                        CL*16
02272      MOVE PM-POLICY-EFF-DT       TO  CP-CERT-EFF-DT.                 CL*16
02273      MOVE PM-LOAN-DT             TO  CP-FIRST-PAY-DATE.              CL*16
02274      MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT.                CL*16
02275      MOVE '1'                    TO  CP-REM-TRM-CALC-OPTION.         CL*16
02276      MOVE '2'                    TO  CP-PROCESS-TYPE.                CL*16
02277      MOVE '3'                    TO  CP-REM-TERM-METHOD.             CL*16
02278      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.                  CL*16
02279      MOVE PM-COMPANY-CD          TO  CP-COMPANY-CD.                  CL*16
02280                                                                      CL*16
02281      PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.                      CL*16
02282                                                                      CL*16
02283      MOVE CP-REMAINING-TERM-1    TO  CVRTRMO.                        CL*16
02284                                                                      CL*16
02285      MOVE PM-INS-MONTH-BENEFIT   TO  CVOBENEO.                       CL*16
02286                                                                      CL*16
02287  4030-MOVE-REST-OF-EMPLCY.                                           CL*16
02288                                                                      CL*16
02289      MOVE PM-INS-POLICY-FORM     TO  CVFORMO.                        CL*16
02290                                                                      CL*16
02291      IF PM-CURRENT-STATUS IS EQUAL TO '7'                            CL*16
02292          IF PM-CANCEL-DT IS NOT EQUAL TO LOW-VALUES                  CL*16
02293              MOVE PM-CANCEL-DT           TO  DC-BIN-DATE-1           CL*16
02294              MOVE SPACE                  TO  DC-OPTION-CODE          CL*16
02295              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                CL*16
02296              IF NOT DATE-CONVERSION-ERROR                            CL*16
02297                  MOVE DC-GREG-DATE-1-EDIT TO  CVCNCDTO.              CL*16
02298                                                                      CL*16
02299      IF (PM-EXIT-DT IS NOT EQUAL TO LOW-VALUES AND SPACES)           CL*16
02300          MOVE ' '                        TO  DC-OPTION-CODE          CL*16
02301          MOVE PM-EXIT-DT                 TO  DC-BIN-DATE-1           CL*16
02302          MOVE SPACE                      TO  DC-OPTION-CODE          CL*16
02303          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*16
02304          IF NOT DATE-CONVERSION-ERROR                                CL*16
02305              MOVE DC-GREG-DATE-1-EDIT    TO  CVEXITO.                CL*16
02306                                                                      CL*16
02307                                                                      CL*16
02308      IF PM-CURRENT-STATUS IS EQUAL TO '0'                            CL*16
02309          MOVE 'LAPSED'           TO  CVSTATO.                        CL*16
02310      IF PM-CURRENT-STATUS IS EQUAL TO '1'                            CL*16
02311          MOVE 'ACTIVE'           TO  CVSTATO.                        CL*16
02312      IF PM-CURRENT-STATUS IS EQUAL TO '2'                            CL*16
02313         MOVE 'PEND   '           TO  CVSTATO.                        CL*16
02314      IF PM-CURRENT-STATUS IS EQUAL TO '3'                            CL*16
02315         MOVE 'DECLINE'           TO  CVSTATO.                        CL*16
02316      IF (PM-CURRENT-STATUS IS EQUAL TO '4' OR '9')                   CL*16
02317         MOVE 'PNDCNC'            TO  CVSTATO.                        CL*16
02318      IF PM-CURRENT-STATUS IS EQUAL TO '5'                            CL*16
02319         MOVE 'PNDISS'            TO  CVSTATO.                        CL*16
02320      IF PM-CURRENT-STATUS IS EQUAL TO '6'                            CL*16
02321         MOVE 'CLAIM'             TO  CVSTATO.                        CL*16
02322      IF PM-CURRENT-STATUS IS EQUAL TO '7'                            CL*16
02323         MOVE 'CANCEL '           TO  CVSTATO.                        CL*16
02324      IF PM-CURRENT-STATUS IS EQUAL TO '8'                            CL*16
02325         MOVE 'PNDUNW '           TO  CVSTATO.                        CL*16
02326      IF PM-CURRENT-STATUS IS EQUAL TO 'C'                            CL*16
02327         MOVE 'TRNSFR '           TO  CVSTATO.                        CL*16
02328      IF PM-CURRENT-STATUS IS EQUAL TO 'F'                            CL*16
02329         MOVE 'SETTLE '           TO  CVSTATO.                        CL*16
02330      IF PM-CURRENT-STATUS IS EQUAL TO 'T'                            CL*16
02331         MOVE 'TRMNAT '           TO  CVSTATO.                        CL*16
02332                                                                      CL*16
02333      MOVE PP-PLAN-ABBREV         TO  CVKINDO.                        CL*16
02334                                                                      CL*16
02335      MOVE PM-LOAN-NUMBER         TO  LOANNOO.                        CL*16
02336      MOVE PM-LOAN-BALC           TO  LOANBALO.                       CL*16
02337      MOVE PM-LOAN-APR            TO  CAPRO.                          CL*16
02338      MOVE PM-INS-TYPE            TO  CINDGRPO.                       CL*16
02339      MOVE PM-BILLING-MODE        TO  CPREMTPO.                       CL*16
02340                                                                      CL*16
02341  4030-EMPLCY-EXIT.                                                   CL*16
02342      EXIT.                                                        EL160
02343      EJECT                                                        EL160
02344  4040-FIND-BENEFIT.                                               EL160
02345      ADD 1 TO COUNT-2.                                            EL160
02346                                                                   EL160
02347      IF COUNT-2 > 8                                               EL160
02348          GO TO 4050-BENEFIT-NOTFND.                               EL160
02349                                                                   EL160
02350      IF CF-BENEFIT-CODE (COUNT-2) = HOLD-BENEFIT                     CL**3
02351          GO TO 4060-EXIT.                                         EL160
02352                                                                   EL160
02353      IF CF-BENEFIT-CODE (COUNT-2) > HOLD-BENEFIT                     CL**3
02354          GO TO 4050-BENEFIT-NOTFND.                               EL160
02355                                                                   EL160
02356      GO TO 4040-FIND-BENEFIT.                                     EL160
02357                                                                   EL160
02358  4050-BENEFIT-NOTFND.                                             EL160
02359      MOVE 'X' TO ERROR-SWITCH.                                    EL160
02360                                                                   EL160
02361  4060-EXIT.                                                       EL160
02362      EXIT.                                                        EL160
02363      EJECT                                                        EL160
02364                                                                   EL160
02365  4070-CHECK-ERROR.                                                EL160
02366      IF SCNERRO > SPACES                                          EL160
02367          GO TO 4080-EXIT.                                         EL160
02368                                                                   EL160
02369      MOVE ER-0283 TO SCNERRO.                                     EL160
02370                                                                   EL160
02371  4080-EXIT.                                                       EL160
02372      EXIT.                                                        EL160
02373                                                                      CL*16
02374      EJECT                                                        EL160
02375  5000-START-BROWSE.                                               EL160
02376      EXEC CICS HANDLE CONDITION                                   EL160
02377          NOTFND (5010-BAD-KEY)                                    EL160
02378          NOTOPEN (6000-MSTR-NOT-OPEN)                             EL160
02379      END-EXEC.                                                    EL160
02380                                                                   EL160
02381      EXEC CICS STARTBR                                            EL160
02382          DATASET (W-FILE-ID)                                         CL*17
02383          RIDFLD (MSTR-KEY)                                        EL160
02384      END-EXEC.                                                    EL160
02385                                                                   EL160
02386      GO TO 5020-EXIT.                                             EL160
02387                                                                   EL160
02388  5010-BAD-KEY.                                                    EL160
02389      MOVE 'X' TO BUILD-SWITCH.                                    EL160
02390                                                                   EL160
02391  5020-EXIT.                                                       EL160
02392      EXIT.                                                        EL160
02393                                                                   EL160
02394  5030-READ-FILE.                                                  EL160
02395      EXEC CICS HANDLE CONDITION                                   EL160
02396          ENDFILE (5040-END-OF-FILE)                               EL160
02397      END-EXEC.                                                    EL160
02398                                                                   EL160
02399      EXEC CICS READNEXT                                           EL160
02400          INTO    (CLAIM-MASTER)                                      CL*17
02401          DATASET (W-FILE-ID)                                         CL*17
02402          RIDFLD  (MSTR-KEY)                                          CL*17
02403      END-EXEC.                                                    EL160
02404                                                                   EL160
02405      GO TO 5050-EXIT.                                             EL160
02406                                                                   EL160
02407  5040-END-OF-FILE.                                                EL160
02408      MOVE 'Y' TO BUILD-SWITCH.                                    EL160
02409                                                                   EL160
02410  5050-EXIT.                                                       EL160
02411      EXIT.                                                        EL160
02412                                                                   EL160
02413  5060-END-BROWSE.                                                 EL160
02414      EXEC CICS ENDBR                                              EL160
02415          DATASET (W-FILE-ID)                                         CL*17
02416      END-EXEC.                                                    EL160
02417                                                                   EL160
02418  5070-EXIT.                                                       EL160
02419      EXIT.                                                        EL160
02420                                                                   EL160
02421  5080-READ-CERT.                                                  EL160
02422      EXEC CICS HANDLE CONDITION                                   EL160
02423          NOTFND (5085-CERT-NOTFND)                                EL160
02424      END-EXEC.                                                    EL160
02425                                                                   EL160
02426      EXEC CICS READ                                               EL160
02427          SET (ADDRESS OF CERTIFICATE-MASTER)                         CL*17
02428          DATASET ('ELCERT')                                       EL160
02429          RIDFLD (CERT-KEY)                                        EL160
02430      END-EXEC.                                                    EL160
02431                                                                   EL160
02432      GO TO 5090-EXIT.                                             EL160
02433                                                                   EL160
02434  5085-CERT-NOTFND.                                                EL160
02435      MOVE 'X' TO ERROR-SWITCH.                                    EL160
02436                                                                   EL160
02437  5090-EXIT.                                                       EL160
02438      EXIT.                                                           CL*16
02439                                                                      CL*16
02440  5095-READ-EMPLCY.                                                   CL*16
02441      EXEC CICS HANDLE CONDITION                                      CL*16
02442          NOTFND (5095-EMPLCY-NOTFND)                                 CL*16
02443      END-EXEC.                                                       CL*16
02444                                                                      CL*16
02445      EXEC CICS READ                                                  CL*16
02446          SET       (ADDRESS OF POLICY-MASTER)                        CL*17
02447          DATASET   ('MPPLCY')                                        CL*16
02448          RIDFLD    (EMPLCY-KEY)                                      CL*16
02449      END-EXEC.                                                       CL*16
02450                                                                      CL*16
02451      GO TO 5095-EXIT.                                                CL*16
02452                                                                      CL*16
02453  5095-EMPLCY-NOTFND.                                                 CL*16
02454      MOVE 'X' TO ERROR-SWITCH.                                       CL*16
02455                                                                      CL*16
02456  5095-EXIT.                                                          CL*16
02457      EXIT.                                                           CL*16
02458                                                                      CL*16
02459  5096-READ-EMPLAN.                                                   CL*16
02460      EXEC CICS HANDLE CONDITION                                      CL*16
02461          NOTFND (5096-EMPLAN-NOTFND)                                 CL*16
02462      END-EXEC.                                                       CL*16
02463                                                                      CL*16
02464      EXEC CICS READ                                                  CL*16
02465          SET       (ADDRESS OF PRODUCER-PLANS)                       CL*17
02466          DATASET   ('MPPLAN')                                        CL*16
02467          RIDFLD    (EMPLAN-KEY)                                      CL*16
02468      END-EXEC.                                                       CL*16
02469                                                                      CL*16
02470      GO TO 5096-EXIT.                                                CL*16
02471                                                                      CL*16
02472  5096-EMPLAN-NOTFND.                                                 CL*16
02473      MOVE 'X' TO ERROR-SWITCH.                                       CL*16
02474                                                                      CL*16
02475  5096-EXIT.                                                          CL*16
02476      EXIT.                                                        EL160
02477                                                                   EL160
02478  5100-READ-TRLR.                                                  EL160
02479      EXEC CICS HANDLE CONDITION                                   EL160
02480          NOTFND (5105-TRLR-NOTFND)                                EL160
02481      END-EXEC.                                                    EL160
02482                                                                   EL160
02483      EXEC CICS READ                                               EL160
02484          SET (ADDRESS OF ACTIVITY-TRAILERS)                          CL*17
02485          DATASET ('ELTRLR')                                       EL160
02486          RIDFLD (TRLR-KEY)                                        EL160
02487      END-EXEC.                                                    EL160
02488                                                                   EL160
02489      GO TO 5110-EXIT.                                             EL160
02490                                                                   EL160
02491  5105-TRLR-NOTFND.                                                EL160
02492      MOVE 'X' TO ERROR-SWITCH.                                    EL160
02493                                                                   EL160
02494  5110-EXIT.                                                       EL160
02495      EXIT.                                                        EL160
02496                                                                   EL160
02497  5120-READ-CNTL.                                                  EL160
02498      EXEC CICS READ                                               EL160
02499          SET (ADDRESS OF CONTROL-FILE)                               CL*17
02500          DATASET ('ELCNTL')                                       EL160
02501          RIDFLD (CNTL-KEY)                                        EL160
02502      END-EXEC.                                                    EL160
02503                                                                   EL160
02504      MOVE CF-FORMS-PRINTER-ID TO PI-PRINT-ID.                     EL160
02505                                                                   EL160
02506  5130-EXIT.                                                       EL160
02507      EXIT.                                                        EL160
02508                                                                   EL160
02509  5140-WRITE-TS.                                                   EL160
02510      ADD 1 TO COUNT-1.                                            EL160
02511      EXEC CICS WRITEQ TS                                          EL160
02512          QUEUE (PI-EL1602-KEY)                                    EL160
02513          FROM (EL160BO)                                           EL160
02514          LENGTH (EL160B-LENGTH)                                   EL160
02515          ITEM (COUNT-1)                                           EL160
02516      END-EXEC.                                                    EL160
02517                                                                   EL160
02518  5150-EXIT.                                                       EL160
02519      EXIT.                                                        EL160
02520                                                                   EL160
02521  5240-WRITE-TS-160A.                                              EL160
02522      EXEC CICS WRITEQ TS                                          EL160
02523          QUEUE (PI-EL160-KEY)                                     EL160
02524          FROM (EL160AO)                                           EL160
02525          LENGTH (EL160A-LENGTH)                                   EL160
02526      END-EXEC.                                                    EL160
02527                                                                   EL160
02528  5250-EXIT.                                                       EL160
02529      EXIT.                                                        EL160
02530                                                                   EL160
02531  5260-READ-BENEFIT.                                               EL160
02532      EXEC CICS HANDLE CONDITION                                   EL160
02533          NOTFND (5270-BENEFIT-NOTFND)                             EL160
02534      END-EXEC.                                                    EL160
02535                                                                   EL160
02536      EXEC CICS READ                                               EL160
02537          SET (ADDRESS OF CONTROL-FILE)                               CL*17
02538          DATASET ('ELCNTL')                                       EL160
02539          RIDFLD (BENEFIT-KEY)                                     EL160
02540          GTEQ                                                     EL160
02541      END-EXEC.                                                    EL160
02542                                                                   EL160
02543      IF CF-RECORD-TYPE = BEN-REC-TYPE                             EL160
02544          GO TO 5280-EXIT.                                         EL160
02545                                                                   EL160
02546  5270-BENEFIT-NOTFND.                                             EL160
02547      MOVE 'X' TO ERROR-SWITCH.                                    EL160
02548                                                                   EL160
02549  5280-EXIT.                                                       EL160
02550      EXIT.                                                        EL160
02551      EJECT                                                        EL160
02552  6000-MSTR-NOT-OPEN.                                              EL160
02553      MOVE ER-0154 TO EMI-ERROR.                                   EL160
02554      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL160
02555      MOVE -1 TO CARRSL.                                           EL160
02556      GO TO 8110-SEND-DATA.                                        EL160
02557                                                                   EL160
02558  6030-CNTL-NOT-OPEN.                                              EL160
02559      MOVE ER-0042 TO EMI-ERROR.                                   EL160
02560      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL160
02561      MOVE -1 TO CARRSL.                                           EL160
02562      GO TO 8110-SEND-DATA.                                        EL160
02563                                                                   EL160
02564      EJECT                                                        EL160
02565  8100-SEND-MAP.                                                   EL160
02566      IF  PI-CARRIER-SECURITY > SPACES                             EL160
02567          MOVE PI-CARRIER-SECURITY TO CARRSO                       EL160
02568          MOVE AL-SANON            TO CARRSA.                      EL160
02569                                                                   EL160
02570      IF  PI-ACCOUNT-SECURITY > SPACES                             EL160
02571          MOVE PI-ACCOUNT-SECURITY TO ACCTSO                       EL160
02572          MOVE AL-SANON            TO ACCTSA.                      EL160
02573                                                                   EL160
02574      IF  PI-ACCOUNT-SECURITY > SPACES        OR                   EL160
02575          PI-CARRIER-SECURITY > SPACES                             EL160
02576          MOVE 'N'                TO PRTOPTO                       EL160
02577          MOVE AL-SANON           TO PRTOPTA                       EL160
02578          MOVE ER-2381         TO EMI-ERROR                        EL160
02579          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL160
02580                                                                   EL160
02581      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.                EL160
02582                                                                   EL160
02583      EXEC CICS SEND                                               EL160
02584          MAP ('EL160A')                                           EL160
02585          MAPSET ('EL160S')                                        EL160
02586          ERASE                                                    EL160
02587          FREEKB                                                   EL160
02588          CURSOR                                                   EL160
02589      END-EXEC.                                                    EL160
02590                                                                   EL160
02591      GO TO 9000-RETURN-TRANS.                                     EL160
02592                                                                   EL160
02593  8110-SEND-DATA.                                                  EL160
02594      IF  PI-CARRIER-SECURITY > SPACES                             EL160
02595          MOVE PI-CARRIER-SECURITY TO CARRSO                       EL160
02596          MOVE AL-SANON            TO CARRSA.                      EL160
02597                                                                   EL160
02598      IF  PI-ACCOUNT-SECURITY > SPACES                             EL160
02599          MOVE PI-ACCOUNT-SECURITY TO ACCTSO                       EL160
02600          MOVE AL-SANON            TO ACCTSA.                      EL160
02601                                                                   EL160
02602      IF  PI-ACCOUNT-SECURITY > SPACES        OR                   EL160
02603          PI-CARRIER-SECURITY > SPACES                             EL160
02604          MOVE 'N'                TO PRTOPTO                       EL160
02605          MOVE AL-SANON           TO PRTOPTA                       EL160
02606          MOVE ER-2381         TO EMI-ERROR                        EL160
02607          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL160
02608                                                                   EL160
02609      PERFORM 8120-FORMAT-TIME-DATE                                EL160
02610              THRU 8130-EXIT.                                      EL160
02611                                                                   EL160
02612      EXEC CICS SEND                                               EL160
02613          MAP ('EL160A')                                           EL160
02614          MAPSET ('EL160S')                                        EL160
02615          DATAONLY                                                 EL160
02616          FREEKB                                                   EL160
02617          CURSOR                                                   EL160
02618      END-EXEC.                                                    EL160
02619                                                                   EL160
02620      GO TO 9000-RETURN-TRANS.                                     EL160
02621                                                                   EL160
02622  8120-FORMAT-TIME-DATE.                                           EL160
02623      MOVE SAVE-DATE      TO DATEO.                                EL160
02624      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL*17
02625      END-EXEC                                                        CL*17
02626      EXEC CICS FORMATTIME                                            CL*17
02627                ABSTIME(LCP-CICS-TIME)                                CL*17
02628                TIME(LCP-TIME-OF-DAY-XX)                              CL*17
02629      END-EXEC                                                        CL*17
02630      MOVE  LCP-TIME-OF-DAY-68 TO TIME-IN.                            CL*17
02631      MOVE UN-HOURS       TO FOR-HOURS.                            EL160
02632      MOVE UN-MINUTES     TO FOR-MINUTES.                          EL160
02633      MOVE TIME-OUT       TO TIMEO.                                EL160
02634      MOVE LIT-MAP        TO PI-CURRENT-SCREEN-NO.                 EL160
02635      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL160
02636      MOVE EMI-MESSAGE-AREA (1) TO MSG1O.                          EL160
02637                                                                   EL160
02638  8130-EXIT.                                                       EL160
02639      EXIT.                                                        EL160
02640                                                                   EL160
02641  8200-RETURN-PRIOR.                                               EL160
02642      MOVE PI-RETURN-TO-PROGRAM TO CALL-PGM.                       EL160
02643      GO TO 9200-XCTL.                                             EL160
02644                                                                   EL160
02645  8300-GET-HELP.                                                   EL160
02646      MOVE XCTL-EL010 TO CALL-PGM.                                 EL160
02647      GO TO 9200-XCTL.                                             EL160
02648                                                                   EL160
02649  8400-RETURN-MASTER.                                              EL160
02650      MOVE XCTL-EL126 TO CALL-PGM.                                 EL160
02651      GO TO 9200-XCTL.                                             EL160
02652                                                                   EL160
02653  8800-UNAUTHORIZED-ACCESS.                                        EL160
02654      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL160
02655      GO TO 8990-SEND-TEXT.                                        EL160
02656                                                                   EL160
02657  8810-PF23-ENTERED.                                               EL160
02658      MOVE EIBAID TO PI-ENTRY-CD-1.                                EL160
02659      MOVE XCTL-EL005 TO CALL-PGM.                                 EL160
02660      GO TO 9200-XCTL.                                             EL160
02661                                                                   EL160
02662  8820-XCTL-ERROR.                                                 EL160
02663      EXEC CICS HANDLE CONDITION                                   EL160
02664          PGMIDERR (8990-SEND-TEXT)                                EL160
02665      END-EXEC.                                                    EL160
02666                                                                   EL160
02667      MOVE SPACE        TO PI-ENTRY-CD-1.                          EL160
02668      MOVE CALL-PGM     TO PI-CALLING-PROGRAM LOGOFF-PGM           EL160
02669      MOVE XCTL-EL005 TO CALL-PGM.                                 EL160
02670      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL160
02671      GO TO 9200-XCTL.                                             EL160
02672                                                                   EL160
02673  8990-SEND-TEXT.                                                  EL160
02674      EXEC CICS SEND TEXT                                          EL160
02675          FROM (LOGOFF-TEXT)                                       EL160
02676          LENGTH (LOGOFF-LENGTH)                                   EL160
02677          ERASE                                                    EL160
02678          FREEKB                                                   EL160
02679      END-EXEC.                                                    EL160
02680                                                                   EL160
02681      GO TO 9100-RETURN-CICS.                                      EL160
02682      EJECT                                                        EL160
02683  9000-RETURN-TRANS.                                               EL160
02684      EXEC CICS RETURN                                             EL160
02685          TRANSID (TRANS-ID)                                       EL160
02686          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL160
02687          LENGTH (PI-COMM-LENGTH)                                  EL160
02688      END-EXEC.                                                    EL160
02689      GOBACK.                                                      EL160
02690                                                                   EL160
02691  9100-RETURN-CICS.                                                EL160
02692      EXEC CICS RETURN                                             EL160
02693      END-EXEC.                                                    EL160
02694      GOBACK.                                                      EL160
02695                                                                   EL160
02696  9200-XCTL.                                                       EL160
02697      EXEC CICS XCTL                                               EL160
02698          PROGRAM (CALL-PGM)                                       EL160
02699          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL160
02700          LENGTH (PI-COMM-LENGTH)                                  EL160
02701      END-EXEC.                                                    EL160
02702                                                                      CL*18
02703  9300-GET-FREE-LOOK.                                                 CL*18
02704                                                                      CL*18
02705      MOVE PI-COMPANY-ID TO COMPANY-ID.                               CL*18
02706      MOVE '3'           TO RECORD-TYPE.                              CL*18
02707      MOVE PI-STATE      TO ACCESS-CD-GENL.                           CL*18
02708      MOVE ZEROS         TO SEQUENCE-NO.                              CL*18
02709                                                                      CL*18
02710      EXEC CICS READ                                                  CL*18
02711          SET (ADDRESS OF CONTROL-FILE)                               CL*18
02712          DATASET ('ELCNTL')                                          CL*18
02713          RIDFLD (CNTL-KEY)                                           CL*18
02714          RESP   (WS-RESPONSE)                                        CL*18
02715      END-EXEC.                                                       CL*18
02716                                                                      CL*18
02717      IF WS-RESP-NOTFND                                               CL*18
02718         MOVE ER-2848   TO EMI-ERROR                                  CL*18
02719         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL*18
02720         GO TO 8110-SEND-DATA                                         CL*18
02721      ELSE                                                            CL*18
02722         MOVE CF-ST-FREE-LOOK-PERIOD                                  CL*18
02723                         TO CP-FREE-LOOK.                             CL*18
02724                                                                      CL*18
02725  9300-EXIT.                                                          CL*18
02726      EXIT.                                                           CL*18
02727                                                                   EL160
02728  9700-LINK-REM-TERM.                                              EL160
02729      EXEC CICS LINK                                               EL160
02730          PROGRAM (REM-TERM-PGM)                                   EL160
02731          COMMAREA (CALCULATION-PASS-AREA)                         EL160
02732          LENGTH (CP-COMM-LENGTH)                                  EL160
02733      END-EXEC.                                                    EL160
02734                                                                   EL160
02735  9700-EXIT.                                                       EL160
02736      EXIT.                                                        EL160
02737                                                                   EL160
02738  9800-CONVERT-DATE.                                               EL160
02739      EXEC CICS LINK                                               EL160
02740          PROGRAM    (DATE-CONV)                                   EL160
02741          COMMAREA   (DATE-CONVERSION-DATA)                        EL160
02742          LENGTH     (DC-COMM-LENGTH)                              EL160
02743      END-EXEC.                                                    EL160
02744                                                                   EL160
02745  9800-EXIT.                                                       EL160
02746      EXIT.                                                        EL160
02747                                                                   EL160
02748  9900-ERROR-FORMAT.                                               EL160
02749      IF EMI-ERRORS-COMPLETE                                       EL160
02750          GO TO 9900-EXIT.                                         EL160
02751                                                                   EL160
02752      EXEC CICS LINK                                               EL160
02753          PROGRAM ('EL001')                                        EL160
02754          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL160
02755          LENGTH (EMI-COMM-LENGTH)                                 EL160
02756      END-EXEC.                                                    EL160
02757                                                                   EL160
02758  9900-EXIT.                                                       EL160
02759      EXIT.                                                        EL160
02760                                                                   EL160
02761  9990-ABEND.                                                      EL160
02762      MOVE DFHEIBLK TO EMI-LINE1.                                  EL160
02763                                                                   EL160
02764      EXEC CICS LINK                                               EL160
02765          PROGRAM   ('EL004')                                      EL160
02766          COMMAREA  (EMI-LINE1)                                    EL160
02767          LENGTH    (72)                                           EL160
02768      END-EXEC.                                                    EL160
02769                                                                   EL160
02770      GO TO 8110-SEND-DATA.                                        EL160
02771                                                                   EL160
02772  9995-SECURITY-VIOLATION.                                         EL160
02773                              COPY ELCSCTP.                        EL160
02774                                                                   EL160
02775  9995-EXIT.                                                       EL160
02776      EXIT.                                                        EL160
02777                                                                   EL160
