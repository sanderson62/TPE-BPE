00001  ID DIVISION.                                                     EL1279
00002                                                                   EL1279
00003  PROGRAM-ID.                 EL1279.                              EL1279
00004 *              PROGRAM CONVERTED BY                               EL1279
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL1279
00006 *              CONVERSION DATE 04/06/95 08:37:08.                 EL1279
00007 *                            VMOD=2.005.                          EL1279
00008 *                                                                 EL1279
00009 *AUTHOR.           LOGIC,INC.                                     EL1279
00010 *                  DALLAS,TEXAS.                                  EL1279
00011                                                                   EL1279
00012 *DATE-COMPILED.                                                   EL1279
00013 *SECURITY.   *****************************************************EL1279
00014 *            *                                                   *EL1279
00015 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *      EL1279
00016 *            *                                                   *EL1279
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL1279
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL1279
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL1279
00020 *            *                                                   *EL1279 
00021 *            *****************************************************EL1279 
00022 *                                                                 EL1279
00023 *REMARKS.     TRANSACTION - EXXF - CERTIFICATE NOTE MAINTENANCE.  EL1279
00024 *                                                                 EL1279
00025 ******************************************************************EL1279
00026 *                   C H A N G E   L O G                           EL1279
00027 *                                                                 EL1279
00028 * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.                EL1279
00029 *-----------------------------------------------------------------EL1279
00030 *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE            EL1279
00031 * EFFECTIVE    NUMBER                                             EL1279
00032 *-----------------------------------------------------------------EL1279
100109* 100109   2008101400003   AJRA  NEW CERT NOTE SCREEN             EL1279
010416* 010416   2015072900002   TANA  ADD PF7 TO CLAIM MEMO SCREEN     EL1279
102020* 102020 IR2020101300001   PEMA  Correct billing notes
00034 ******************************************************************EL1279
00035 *                                                                 EL1279
00036  ENVIRONMENT DIVISION.                                            EL1279
00037                                                                   EL1279
00038      EJECT                                                        EL1279
00039  DATA DIVISION.                                                   EL1279
00040  WORKING-STORAGE SECTION.                                         EL1279
00041  77  FILLER  PIC X(32)  VALUE '********************************'. EL1279
00042  77  FILLER  PIC X(32)  VALUE '*     EL1279 WORKING STORAGE   *'. EL1279
00043  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.005 ***********'. EL1279
00044                                                                   EL1279
00045      COPY ELCSCTM.                                                EL1279
00046      COPY ELCSCRTY.                                               EL1279
00047                                                                   EL1279
00048      EJECT                                                        EL1279
00049  01  STANDARD-AREAS.                                              EL1279
00050      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         EL1279
00051      12  MAP-NAME                PIC X(8)    VALUE 'EL1279A'.     EL1279
00052      12  MAPSET-NAME             PIC X(8)    VALUE 'EL1279S '.    EL1279
00053      12  SCRN-NUMBER             PIC X(4)    VALUE '127I'.        EL1279
00054      12  TRANS-ID                PIC X(4)    VALUE 'EXXF'.        EL1279
00055      12  THIS-PGM                PIC X(8)    VALUE 'EL1279'.      EL1279
00056      12  PGM-NAME                PIC X(8).                        EL1279
00061      12  PGM-EL1276              PIC X(8)    VALUE 'EL1276'.      EL1279 
102020     12  pgm-el6311              pic x(8)    value 'EL6311'.
102020     12  pgm-el690               pic x(8)    value 'EL690'.
102020     12  pgm-el691               pic x(8)    value 'EL691'.
00062                                                                   EL1279
010416     12  PGM-EL1284              PIC X(8)    VALUE 'EL1284'.      EL1279
00063      12  ERCNOT-FILE-ID          PIC X(8)    VALUE 'ERCNOT'.      EL1279
00064      12  ELCERT-FILE-ID          PIC X(8)    VALUE 'ELCERT'.      EL1279
00065      12  QID.                                                     EL1279
00066          16  QID-TERM            PIC X(4).                        EL1279
00067          16  FILLER              PIC X(4)    VALUE '127I'.        EL1279
00068      12  QID-ITEM                PIC S9(4)   COMP VALUE +0.       EL1279
00069      12  SC-ITEM                 PIC S9(4)   COMP VALUE +1.       EL1279
00070      12  WS-LINE                 PIC 9(3)    VALUE 0.    	        EL1279 
00074      12  WS-BLANK-LINES          PIC S9(4)   COMP VALUE +0.       EL1279
00075                                                                   EL1279
00076  01  WORK-AREA.                                                   EL1279
00077      12  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL1279
00078      12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL1279
00079                                                                   EL1279
00080      12  ERCNOT-LENGTH           PIC S9(4)   COMP VALUE +150.     EL1279
00081      12  ERCNOT-KEY-LENGTH       PIC S9(4)   COMP VALUE +36.      EL1279
00082      12  ERCNOT-START-LENGTH     PIC S9(4)   COMP VALUE +34.      EL1279
00083      12  ERCNOT-KEY.                                              EL1279
00084          16  ERCNOT-PARTIAL-KEY.                                  EL1279
00085              20 ERCNOT-COMPANY-CD    PIC X.                       EL1279
00086              20 ERCNOT-CARRIER       PIC X.                       EL1279
00087              20 ERCNOT-GROUPING      PIC X(06).                   EL1279
00088              20 ERCNOT-STATE         PIC XX.                      EL1279
00089              20 ERCNOT-ACCOUNT       PIC X(10).                   EL1279
00090              20 ERCNOT-EFF-DT        PIC XX.                      EL1279
00091              20 ERCNOT-CERT-NO.                                   EL1279
00092                 25 ERCNOT-CERT-PRIME PIC X(10).                   EL1279
00093                 25 ERCNOT-CERT-SFX   PIC X.                       EL1279
00094              20 ERCNOT-REC-TYP       PIC X.                       EL1279
00095          16 ERCNOT-SEQ           PIC S9(4) COMP.                  EL1279
00096      12  SV-PRIOR-KEY.                                            EL1279
00097          20 SV-COMPANY-CD            PIC X.                       EL1279
00098          20 SV-CARRIER               PIC X.                       EL1279
00099          20 SV-GROUPING              PIC X(06).                   EL1279
00100          20 SV-STATE                 PIC XX.                      EL1279
00101          20 SV-ACCOUNT               PIC X(10).                   EL1279
00102          20 SV-EFF-DT                PIC XX.                      EL1279
00103          20 SV-CERT-NO.                                           EL1279
00104             25 SV-CERT-PRIME         PIC X(10).                   EL1279
00105             25 SV-CERT-SFX           PIC X(1).                    EL1279
00106          20 SV-REC-TYP               PIC X.                       EL1279
00107      12  ELCERT-KEY.                                              EL1279
00108          16  ELCERT-COMPANY-CD        PIC X.                      EL1279
00109          16  ELCERT-CARRIER           PIC X.                      EL1279
00110          16  ELCERT-GROUPING          PIC X(6).                   EL1279
00111          16  ELCERT-STATE             PIC XX.                     EL1279
00112          16  ELCERT-ACCOUNT           PIC X(10).                  EL1279
00113          16  ELCERT-EFF-DT            PIC XX.                     EL1279
00114          16  ELCERT-CERT-NO.                                      EL1279
00115              20  ELCERT-CERT-PRIME    PIC X(10).                  EL1279
00116              20  ELCERT-CERT-SFX      PIC X.                      EL1279
00117                                                                   EL1279
00118      12  TIME-IN                 PIC S9(7).                       EL1279
00119      12  TIME-SPLIT REDEFINES TIME-IN.                            EL1279
00120          16  FILLER              PIC X.                           EL1279
00121          16  TIME-OUT            PIC 99V99.                       EL1279
00122          16  FILLER              PIC X(2).                        EL1279
00123      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL1279
00124      12  XCTL-126                PIC X(8)    VALUE 'EL126'.       EL1279
00125      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL1279
00126      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL1279
00127      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL1279
00128      12  MAX-LINES               PIC 999     VALUE 300.           EL1279
00129      12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 10.            EL1279
00130      12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.            EL1279
00131      12  TS-GROUP-WORK           PIC 9(5)    VALUE 0  COMP-3.     EL1279
00132      12  TS-LENGTH               PIC S9(4)   VALUE +3650 COMP.    EL1279
00133      12  ROLL-COUNTER            PIC S999    VALUE +0 COMP-3.     EL1279
00134      12  TEMP-CURR-LINE          PIC S9(3)   COMP-3.              EL1279
00135      12  WS-SUB                  PIC S9(3)   COMP-3.              EL1279
00136      12  WS-SUB1                 PIC S9(3)   COMP-3.              EL1279
00137      12  WS-SCREEN-LINE          PIC X       VALUE 'N'.           EL1279
00138          88 SCREEN-LINE-FOUND                VALUE 'Y'.           EL1279
00139                                                                   EL1279
00140      12  SCR-CLAIM-NOTE-TYPE.                                     EL1279
00141          16  FILLER              PIC X(50)   VALUE                EL1279
00142          '***********    C L A I M   C E R T   N O T E S    '.    EL1279
00143          16  FILLER              PIC X(13)   VALUE                EL1279
00144          '*************'.                                         EL1279
00145      12  SCR-CERT-NOTE-TYPE.                                      EL1279
00146          16  FILLER              PIC X(50)   VALUE                EL1279
00147          '***********  C E R T I F I C A T E     N O T E S  '.    EL1279
00148          16  FILLER              PIC X(13)   VALUE                EL1279
00149          '*************'.                                         EL1279
00150      12  SCR-PF6-CLAIM           PIC X(15)   VALUE                EL1279
00151          'PF6=CLAIM NOTES'.                                       EL1279
00152      12  SCR-PF6-CERT            PIC X(15)   VALUE                EL1279
00153          'PF6=CERT NOTES'.                                        EL1279
00154      12  SCR-CLAIM-YN            PIC X(12)   VALUE                EL1279
00155          'CLAIM NOTES:'.                                          EL1279
00156      12  SCR-CERT-YN             PIC X(12)   VALUE                EL1279
00157          ' CERT NOTES:'.                                          EL1279
00158                                                                   EL1279
00159      EJECT                                                        EL1279
00160  01  ERROR-MESSAGES.                                              EL1279
00161      12  ER-0000             PIC X(04)       VALUE '0000'.        EL1279
00162      12  ER-0004             PIC X(04)       VALUE '0004'.        EL1279
00163      12  ER-0006             PIC X(04)       VALUE '0006'.        EL1279
00164      12  ER-0008             PIC X(04)       VALUE '0008'.        EL1279
00165      12  ER-0023             PIC X(04)       VALUE '0023'.        EL1279
00166      12  ER-0029             PIC X(04)       VALUE '0029'.        EL1279
00167      12  ER-0030             PIC X(04)       VALUE '0030'.        EL1279
00168      12  ER-0031             PIC X(04)       VALUE '0031'.        EL1279
00169      12  ER-0032             PIC X(04)       VALUE '0032'.        EL1279
00170      12  ER-0033             PIC X(04)       VALUE '0033'.        EL1279
00171      12  ER-0041             PIC X(04)       VALUE '0041'.        EL1279
00172      12  ER-0044             PIC X(04)       VALUE '0044'.        EL1279
00173      12  ER-0045             PIC X(04)       VALUE '0045'.        EL1279
00174      12  ER-0047             PIC X(04)       VALUE '0047'.        EL1279
00175      12  ER-0048             PIC X(04)       VALUE '0048'.        EL1279
00176      12  ER-0049             PIC X(04)       VALUE '0049'.        EL1279
00177      12  ER-0050             PIC X(04)       VALUE '0050'.        EL1279
00178      12  ER-0051             PIC X(04)       VALUE '0051'.        EL1279
00179      12  ER-0066             PIC X(04)       VALUE '0066'.        EL1279
00180      12  ER-0067             PIC X(04)       VALUE '0067'.        EL1279
00181      12  ER-0069             PIC X(04)       VALUE '0069'.        EL1279
00182      12  ER-0070             PIC X(04)       VALUE '0070'.        EL1279
00183      12  ER-0140             PIC X(04)       VALUE '0140'.        EL1279
00184      12  ER-2954             PIC X(04)       VALUE '2954'.        EL1279
00185      EJECT                                                        EL1279
00186                         COPY ELCLOGOF.                            EL1279
00187      EJECT                                                        EL1279
00188                         COPY ELCAID.                              EL1279
00189  01  FILLER  REDEFINES DFHAID.                                    EL1279
00190      12  FILLER                  PIC X(8).                        EL1279
00191      12  PF-VALUES OCCURS 24 TIMES       PIC X.                   EL1279
00192      EJECT                                                        EL1279
00193                         COPY ELCEMIB.                             EL1279
00194      EJECT                                                        EL1279
00195                         COPY ELCINTF.                             EL1279
00196      EJECT                                                        EL1279
00197      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL1279
00198          16  FILLER              PIC X(318).                      EL1279 
00199          16  PI-TOTAL-LINES      PIC S9(3).                       EL1279
00200          16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.              EL1279
00201          16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.                EL1279
00202          16  PI-UPDATE-SW        PIC X.                           EL1279
00203              88  PI-CHANGES-MADE             VALUE '1'.           EL1279
00207          16  PI-PF5-PRESSED      PIC X.                           EL1279
00208              88 PF5-PRESSED                  VALUE 'Y'.           EL1279
00209          16  PI-PF6-PRESSED      PIC X.                           EL1279
00210              88 PF6-PRESSED                  VALUE 'Y'.           EL1279
00211          16  PI-CURR-NOTE-REC-TYPE PIC X.                         EL1279
00212              88 PI-CERT-NOTE                 VALUE '1'.           EL1279
00213              88 PI-CLAIM-NOTE                VALUE '2'.           EL1279
00214          16  PI-BILLING-NOTES-EXIST PIC X.                        EL1279
00215          16  PI-CERT-NOTES-EXIST    PIC X.                        EL1279
00216          16  PI-CLAIM-NOTES-EXIST   PIC X.                        EL1279
00217          16  PI-SET-NOTE-CHANGE  PIC X.                           EL1279
00218              88 PI-CHANGE-IN-NOTE-TYPE       VALUE 'Y'.
102020         16  pi-iss-can-trans    pic x.
102020             88  pi-from-issue     value '1'.
102020             88  pi-from-cancel    value '2'.
00219      EJECT                                                        EL1279
00220                            COPY ELCATTR.                          EL1279
00221      EJECT                                                        EL1279
00222                            COPY ELCDATE.                          EL1279
00223      EJECT                                                        EL1279
00224                            COPY EL1279S.                          EL1279
00225      EJECT                                                        EL1279
00226  01  EL1279R REDEFINES EL1279AI.                                  EL1279
00227      12  FILLER                  PIC X(202).                      EL1279
00228      12  SC-ALL-LINES.                                            EL1279
00229       14 SC-LINES OCCURS 10 TIMES INDEXED BY SC-INDX.             EL1279
00230          16  SC-LINL             PIC S9(4)   COMP.                EL1279
00231          16  SC-LINA             PIC X.                           EL1279
00232          16  SC-LIN              PIC ZZ9.                         EL1279
00233          16  SC-TEXTL            PIC S9(4)   COMP.                EL1279
00234          16  SC-TEXTA            PIC X.                           EL1279
00235          16  SC-TEXT             PIC X(63).                       EL1279
00233          16  SC-MTBYL            PIC S9(4)   COMP.                EL1279
00234          16  SC-MTBYA            PIC X.                           EL1279
00235          16  SC-MTBY             PIC X(4).                        EL1279
00233          16  SC-MTDTL            PIC S9(4)   COMP.                EL1279
00234          16  SC-MTDTA            PIC X.                           EL1279
00235          16  SC-MTDT             PIC X(6).                        EL1279
00236      12  FILLER                  PIC X(116).                      EL1279
00237      EJECT                                                        EL1279
00238  01  RECORD-TABLE                PIC X(21900) VALUE SPACES.       EL1279
00239  01  REC-TABLE  REDEFINES RECORD-TABLE.                           EL1279
00240      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX PIC X(3650).  EL1279
00241  01  REC-ENTRIES REDEFINES RECORD-TABLE.                          EL1279
00242      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.    EL1279
00243          16  REC-TEXT                    PIC X(63).               EL1279
00244          16  REC-LAST-MAINT-BY           PIC XXXX.                EL1279
00245          16  REC-LAST-MAINT-DT           PIC XX.                  EL1279
00246          16  REC-LAST-MAINT-HHMMSS       PIC S9(7) COMP-3.        EL1279
00247                                                                   EL1279
00248  01  TS-WORK-AREA                        PIC X(3650).             EL1279
00249      EJECT                                                        EL1279
00250                                                                   EL1279
00251  LINKAGE SECTION.                                                 EL1279
00252  01  DFHCOMMAREA                 PIC X(1500).                     EL1279
00253                                                                   EL1279
00254           COPY ERCCNOT.                                           EL1279
00255                                                                   EL1279
00256           COPY ELCCERT.                                           EL1279
00257                                                                   EL1279
00258  PROCEDURE DIVISION.                                              EL1279

00260      MOVE EIBTRMID               TO QID-TERM.                     EL1279
00261      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1279
00262      MOVE '5'                    TO DC-OPTION-CODE.               EL1279
00263      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1279
00264      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL1279
00265      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL1279
00266                                                                   EL1279
00267      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK                  EL1279
00268      IF EIBCALEN = ZEROS                                          EL1279
00269          GO TO 8800-UNAUTHORIZED-ACCESS                           EL1279
00270      END-IF.                                                      EL1279
00271                                                                   EL1279
102020     IF PI-CALLING-PROGRAM = pgm-el6311 or pgm-el690 or pgm-el691
102020        move pi-program-work-area(1:1)
102020                                 to pi-iss-can-trans
102020     end-if

00272      IF PI-CALLING-PROGRAM NOT = THIS-PGM AND PGM-EL1276
00274          PERFORM 4900-SET-NOTES-FLAG THRU 4900-EXIT               EL1279
00275          IF CLAIM-SESSION                                         EL1279
00276              SET PI-CLAIM-NOTE TO TRUE                            EL1279
00277          ELSE                                                     EL1279
00278              SET PI-CERT-NOTE TO TRUE                             EL1279
00279          END-IF                                                   EL1279
00280      END-IF.                                                      EL1279
00281                                                                   EL1279
00282      IF PI-CALLING-PROGRAM = PGM-EL1276                           EL1279
00283          MOVE THIS-PGM           TO PI-CALLING-PROGRAM            EL1279
00284          MOVE 'N'                TO PI-PF5-PRESSED                EL1279
00285          MOVE 'N'                TO PI-PF6-PRESSED                EL1279
00286      END-IF.                                                      EL1279
00287                                                                   EL1279
00288      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL1279
00289         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                    EL1279
00290            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6        EL1279
00291            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5        EL1279
00292            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4        EL1279
00293            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3        EL1279
00294            MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2        EL1279
00295            MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1        EL1279
00296            MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM      EL1279
00297            MOVE THIS-PGM TO PI-CALLING-PROGRAM                    EL1279
00298         ELSE                                                      EL1279
00299            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM        EL1279
00300            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM      EL1279
00301            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1        EL1279
00302            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2        EL1279
00303            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3        EL1279
00304            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4        EL1279
00305            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5        EL1279
00306            MOVE SPACES               TO PI-SAVED-PROGRAM-6        EL1279
00307         END-IF                                                    EL1279
00308      END-IF.                                                      EL1279
00309                                                                   EL1279
00310  1000-START.                                                      EL1279
00311                                                                   EL1279
00312      MOVE LOW-VALUES TO EL1279AI.                                 EL1279
00313      MOVE SPACES                 TO  ERCNOT-KEY                   EL1279
00314                                      ELCERT-KEY.                  EL1279
00315                                                                   EL1279
00316      MOVE PI-COMPANY-CD          TO  ERCNOT-COMPANY-CD            EL1279
00317                                      ELCERT-COMPANY-CD.           EL1279
00318      MOVE PI-CARRIER             TO  ERCNOT-CARRIER               EL1279
00319                                      ELCERT-CARRIER.              EL1279
00320      MOVE PI-GROUPING            TO  ERCNOT-GROUPING              EL1279
00321                                      ELCERT-GROUPING.             EL1279
00322      MOVE PI-STATE               TO  ERCNOT-STATE                 EL1279
00323                                      ELCERT-STATE.                EL1279
00324      MOVE PI-ACCOUNT             TO  ERCNOT-ACCOUNT               EL1279
00325                                      ELCERT-ACCOUNT.              EL1279
00326      MOVE PI-CERT-EFF-DT         TO  ERCNOT-EFF-DT                EL1279
00327                                      ELCERT-EFF-DT.               EL1279
00328      MOVE PI-CERT-PRIME          TO  ERCNOT-CERT-PRIME            EL1279
00329                                      ELCERT-CERT-PRIME.           EL1279
00330      MOVE PI-CERT-SFX            TO  ERCNOT-CERT-SFX              EL1279
00331                                      ELCERT-CERT-SFX.             EL1279
00332      MOVE PI-CURR-NOTE-REC-TYPE  TO  ERCNOT-REC-TYP               EL1279
00333      MOVE ZEROS                  TO  ERCNOT-SEQ.                  EL1279
00334      MOVE ERCNOT-PARTIAL-KEY     TO  SV-PRIOR-KEY.                EL1279
00335                                                                   EL1279
00336      IF EIBTRNID NOT = TRANS-ID                                   EL1279
00337         MOVE '0'                 TO  PI-UPDATE-SW                 EL1279
00338         IF PI-PROCESSOR-ID NOT = 'LGXX'                           EL1279
00339             EXEC CICS READQ TS                                    EL1279
00340                      QUEUE (PI-SECURITY-TEMP-STORE-ID)            EL1279
00341                      INTO (SECURITY-CONTROL)                      EL1279
00342                      LENGTH (SC-COMM-LENGTH)                      EL1279
00343                      ITEM  (SC-ITEM)                              EL1279
00344             END-EXEC                                              EL1279
00345           MOVE SC-CREDIT-DISPLAY (32) TO PI-DISPLAY-CAP           EL1279
00346           MOVE SC-CREDIT-UPDATE (32)  TO PI-MODIFY-CAP            EL1279
00347         END-IF                                                    EL1279
00348      END-IF.                                                      EL1279
00349                                                                   EL1279
00350      IF NOT DISPLAY-CAP                                           EL1279
00351          MOVE 'READ'             TO  SM-READ                      EL1279
00352          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL1279
00353          MOVE ER-0070            TO  EMI-ERROR                    EL1279
00354          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1279
00355          GO TO 8100-SEND-INITIAL-MAP                              EL1279
00356      END-IF.                                                      EL1279
00357                                                                   EL1279
00358      EXEC CICS HANDLE AID                                         EL1279
00359           CLEAR(9400-CLEAR)                                       EL1279
00360      END-EXEC.                                                    EL1279
00361                                                                   EL1279
00362      EXEC CICS HANDLE CONDITION                                   EL1279
00363           ERROR(9990-ABEND)                                       EL1279
00364           PGMIDERR(9600-PGMID-ERROR)                              EL1279
00365      END-EXEC.                                                    EL1279
00366                                                                   EL1279
00367      IF EIBTRNID NOT = TRANS-ID                                   EL1279
00368          GO TO 7000-BUILD-TABLE                                   EL1279
00369      END-IF.                                                      EL1279
00370                                                                   EL1279
00371      IF PI-CHANGE-IN-NOTE-TYPE                                    EL1279
00372          GO TO 7000-BUILD-TABLE                                   EL1279
00373      END-IF.                                                      EL1279
00374                                                                   EL1279
00375      EJECT                                                        EL1279
00376  2000-RECEIVE.                                                    EL1279
00377      IF EIBAID = DFHPA1 OR                                        EL1279
00378         EIBAID = DFHPA2 OR                                        EL1279
00379         EIBAID = DFHPA3                                           EL1279
00380            MOVE ER-0008 TO EMI-ERROR                              EL1279
00381            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1279
00382            GO TO 8200-SEND-DATAONLY                               EL1279
00383      END-IF.                                                      EL1279
00384                                                                   EL1279
00385      EXEC CICS RECEIVE                                            EL1279
00386           MAP(MAP-NAME)                                           EL1279
00387           MAPSET(MAPSET-NAME)                                     EL1279
00388           INTO(EL1279AI)                                          EL1279
00389      END-EXEC.                                                    EL1279
00390                                                                   EL1279
00391      IF PFENTERL = ZEROS                                          EL1279
00392         GO TO 2001-CHECK-PFKEYS                                   EL1279
00393      END-IF.                                                      EL1279
00394                                                                   EL1279
00395      IF EIBAID NOT = DFHENTER                                     EL1279
00396         MOVE ER-0004             TO EMI-ERROR                     EL1279
00397         GO TO 2002-INPUT-ERROR                                    EL1279
00398      END-IF.                                                      EL1279
00399                                                                   EL1279
00400      IF PFENTERI NUMERIC AND                                      EL1279
00401         (PFENTERI > 00 AND  < 25)                                 EL1279
00402         MOVE PF-VALUES (PFENTERI) TO EIBAID                       EL1279
00403      ELSE                                                         EL1279
00404         MOVE ER-0029 TO EMI-ERROR                                 EL1279
00405         GO TO 2002-INPUT-ERROR                                    EL1279
00406      END-IF.                                                      EL1279
00407                                                                   EL1279
00408  2001-CHECK-PFKEYS.                                               EL1279
00409      IF EIBAID = DFHPF23                                          EL1279
00410         GO TO 9000-RETURN-CICS                                    EL1279
00411      END-IF.                                                      EL1279
00412                                                                   EL1279
00413      IF EIBAID = DFHPF24                                          EL1279
00414         GO TO 9200-RETURN-MAIN-MENU                               EL1279
00415      END-IF.                                                      EL1279
00416                                                                   EL1279
00417      IF EIBAID = DFHPF12                                          EL1279
00418         GO TO 9500-PF12                                           EL1279
00419      END-IF.                                                      EL1279
00420                                                                   EL1279
00421      IF EIBAID = DFHPF5                                           EL1279
00422         IF PI-CHANGES-MADE                                        EL1279
00423             SET PF5-PRESSED          TO TRUE                      EL1279
00424             MOVE -1                  TO FUNCTL                    EL1279
00425             MOVE SPACES              TO PFENTERO                  EL1279
00426             MOVE AL-UNNOF            TO PFENTERA                  EL1279
00427             MOVE ER-0045             TO EMI-ERROR                 EL1279
00428             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL1279
00429             GO TO 8200-SEND-DATAONLY                              EL1279
00430          ELSE                                                     EL1279
00431             MOVE PGM-EL1276         TO PGM-NAME                   EL1279
00432             GO TO 9300-XCTL                                       EL1279
00433          END-IF                                                   EL1279
00434      END-IF.                                                      EL1279
00435                                                                   EL1279
00436      IF EIBAID = DFHPF6                                           EL1279
00437         IF PI-CHANGES-MADE                                        EL1279
00438             SET PF6-PRESSED          TO TRUE                      EL1279
00439             MOVE -1                  TO FUNCTL                    EL1279
00440             MOVE SPACES              TO PFENTERO                  EL1279
00441             MOVE AL-UNNOF            TO PFENTERA                  EL1279
00442             MOVE ER-0045             TO EMI-ERROR                 EL1279
00443             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL1279
00444             GO TO 8200-SEND-DATAONLY                              EL1279
00445          ELSE                                                     EL1279
00446             IF PI-CLAIM-NOTE                                      EL1279
00447                 SET PI-CERT-NOTE    TO TRUE                       EL1279
00448             ELSE                                                  EL1279
00449                 SET PI-CLAIM-NOTE   TO TRUE                       EL1279
00450             END-IF                                                EL1279
00451             SET PI-CHANGE-IN-NOTE-TYPE TO TRUE                    EL1279
00452             GO TO 1000-START                                      EL1279
00453          END-IF                                                   EL1279
00454      END-IF.                                                      EL1279
010416                                                                  EL1279
010416     IF EIBAID = DFHPF7                                           EL1279
010416         MOVE PGM-EL1284          TO PGM-NAME                     EL1279
010416         GO TO 9300-XCTL.                                         EL1279
00455                                                                   EL1279
00456      IF FUNCTL NOT = ZEROS AND EIBAID NOT = DFHENTER              EL1279
00457         IF FUNCTI = 'A' OR = SPACES                               EL1279
00458            NEXT SENTENCE                                          EL1279
00459         ELSE                                                      EL1279
00460            MOVE ER-0050          TO EMI-ERROR                     EL1279
00461            MOVE -1 TO FUNCTL                                      EL1279
00462            MOVE AL-UABON TO FUNCTA PFENTERA                       EL1279
00463            GO TO 2002-INPUT-ERROR                                 EL1279
00464         END-IF                                                    EL1279
00465      END-IF.                                                      EL1279
00466                                                                   EL1279
00467      IF EIBAID = DFHPF1                                           EL1279
00468         MOVE NUM-LINES-PER-SCREEN TO ROLL-COUNTER                 EL1279
00469         GO TO 7400-PAGE-ROUTINE                                   EL1279
00470      END-IF.                                                      EL1279
00471                                                                   EL1279
00472      IF EIBAID = DFHPF2                                           EL1279
00473         SUBTRACT NUM-LINES-PER-SCREEN FROM ROLL-COUNTER           EL1279
00474         GO TO 7400-PAGE-ROUTINE                                   EL1279
00475      END-IF.                                                      EL1279
00476                                                                   EL1279
00477      IF EIBAID = DFHPF3                                           EL1279
00478         MOVE 5                   TO ROLL-COUNTER                  EL1279
00479         GO TO 7400-PAGE-ROUTINE                                   EL1279
00480      END-IF.                                                      EL1279
00481                                                                   EL1279
00482      IF EIBAID = DFHPF4                                           EL1279
00483         MOVE -5                  TO ROLL-COUNTER                  EL1279
00484         GO TO 7400-PAGE-ROUTINE                                   EL1279
00485      END-IF.                                                      EL1279
00486                                                                   EL1279
00487      IF EIBAID = DFHENTER                                         EL1279
00488         GO TO 2003-EDIT-DATA                                      EL1279
00489      END-IF.                                                      EL1279
00490                                                                   EL1279
00491      MOVE ER-0029                TO EMI-ERROR.                    EL1279
00492                                                                   EL1279
00493  2002-INPUT-ERROR.                                                EL1279
00494      MOVE -1                     TO PFENTERL                      EL1279
00495      MOVE AL-UNBON               TO PFENTERA                      EL1279
00496      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL1279
00497      GO TO 8200-SEND-DATAONLY.                                    EL1279
00498                                                                   EL1279
00499  2003-EDIT-DATA.                                                  EL1279
00500                                                                   EL1279
00501      IF FUNCTI = 'L'                                              EL1279
00502          NEXT SENTENCE                                            EL1279
00503      ELSE                                                         EL1279
00504          IF NOT MODIFY-CAP                                        EL1279
00505              MOVE 'UPDATE'       TO  SM-READ                      EL1279
00506              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL1279
00507              MOVE ER-0070        TO  EMI-ERROR                    EL1279
00508              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL1279
00509              GO TO 8100-SEND-INITIAL-MAP                          EL1279
00510          END-IF                                                   EL1279
00511      END-IF.                                                      EL1279
00512                                                                   EL1279
00513      IF FUNCTL = ZEROS OR FUNCTI = SPACES                         EL1279
00514         GO TO 4000-CHANGE-ROUTINE                                 EL1279
00515      END-IF.                                                      EL1279
00516                                                                   EL1279
00517      IF (FUNCTI = 'S' OR = 'D' OR = 'Q' OR                        EL1279
00518                 = 'I' OR = 'A' OR = 'L')                          EL1279
00519          NEXT SENTENCE                                            EL1279
00520      ELSE                                                         EL1279
00521          MOVE ER-0023            TO EMI-ERROR                     EL1279
00522          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1279
00523          MOVE AL-UABON           TO FUNCTA                        EL1279
00524          MOVE -1                 TO FUNCTL                        EL1279
00525          GO TO 8200-SEND-DATAONLY                                 EL1279
00526      END-IF.                                                      EL1279
00527                                                                   EL1279
00528      IF FUNCTI = 'D'  OR = 'I' OR = 'L'                           EL1279
00529         PERFORM 2500-LINE-CHECK THRU 2599-EXIT                    EL1279
00530      ELSE                                                         EL1279
00531         IF LINE1L NOT = ZEROS OR                                  EL1279
00532            LINE2L NOT = ZEROS                                     EL1279
00533            MOVE ER-0030          TO EMI-ERROR                     EL1279
00534            MOVE -1               TO LINE1L                        EL1279
00535            MOVE AL-UNBON         TO LINE1A LINE2A                 EL1279
00536            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1279
00537            GO TO 8200-SEND-DATAONLY                               EL1279
00538         END-IF                                                    EL1279
00539      END-IF.                                                      EL1279
00540                                                                   EL1279
00541      IF FUNCTI = 'A'                                              EL1279
00542         GO TO 5000-ADD-NEW-LINES                                  EL1279
00543      END-IF.                                                      EL1279
00544      IF FUNCTI = 'Q'                                              EL1279
00545         GO TO 9410-RETURN                                         EL1279
00546      END-IF.                                                      EL1279
00547      IF FUNCTI = 'S'                                              EL1279
00548         GO TO 4500-SAVE-DATA                                      EL1279
00549      END-IF.                                                      EL1279
00550      IF PI-TOTAL-LINES = 0                                        EL1279
00551         MOVE ER-0048             TO EMI-ERROR                     EL1279
00552         MOVE -1                  TO FUNCTL                        EL1279
00553         MOVE AL-UNBON            TO FUNCTA                        EL1279
00554         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
00555         GO TO 8200-SEND-DATAONLY                                  EL1279
00556      END-IF.                                                      EL1279
00557      IF FUNCTI = 'L'                                              EL1279
00558         GO TO 5500-LOOKUP                                         EL1279
00559      END-IF.                                                      EL1279
00560      IF FUNCTI = 'D'                                              EL1279
00561         GO TO 3000-DELETE-LINES                                   EL1279
00562      END-IF.                                                      EL1279
00563                                                                   EL1279
00564      GO TO 3500-INSERT-LINES.                                     EL1279
00565      EJECT                                                        EL1279
00566  2500-LINE-CHECK.                                                 EL1279
00567      IF LINE1L = ZEROS AND                                        EL1279
00568         LINE2L = ZEROS                                            EL1279
00569         MOVE ER-0069             TO EMI-ERROR                     EL1279
00570         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
00571         MOVE -1                  TO LINE1L                        EL1279
00572         GO TO 8200-SEND-DATAONLY                                  EL1279
00573      END-IF.                                                      EL1279
00574                                                                   EL1279
00575      IF LINE1L NOT = ZEROS                                        EL1279
00576         IF LINE1I = ZERO AND FUNCTI EQUAL 'L'                     EL1279
00577             MOVE 1               TO LINE1I                        EL1279
00578         END-IF                                                    EL1279
00579         IF LINE1I = ZERO AND FUNCTI EQUAL 'D'                     EL1279
00580            MOVE ER-0049          TO EMI-ERROR                     EL1279
00581            MOVE AL-UNBON         TO LINE1A                        EL1279
00582            MOVE -1               TO LINE1L                        EL1279
00583            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1279
00584            GO TO 8200-SEND-DATAONLY                               EL1279
00585         END-IF                                                    EL1279
00586         IF LINE1I NOT NUMERIC OR                                  EL1279
00587            LINE1I > PI-TOTAL-LINES                                EL1279
00588            MOVE ER-0031          TO EMI-ERROR                     EL1279
00589            MOVE AL-UNBON         TO LINE1A                        EL1279
00590            MOVE -1               TO LINE1L                        EL1279
00591            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1279
00592            GO TO 8200-SEND-DATAONLY                               EL1279
00593         ELSE                                                      EL1279
00594            IF LINE2L = ZEROS                                      EL1279
00595               MOVE 1             TO LINE2I                        EL1279
00596               IF FUNCTI = 'I'                                     EL1279
00597                   GO TO 2510-MAX-CHECK                            EL1279
00598               ELSE                                                EL1279
00599                   NEXT SENTENCE                                   EL1279
00600               END-IF                                              EL1279
00601            ELSE                                                   EL1279
00602               IF FUNCTI = 'I'                                     EL1279
00603                  GO TO 2510-MAX-CHECK                             EL1279
00604               ELSE                                                EL1279
00605                  IF LINE2I NOT NUMERIC                            EL1279
00606                     MOVE AL-UNBON TO LINE2A                       EL1279
00607                     MOVE ER-0032  TO EMI-ERROR                    EL1279
00608                     MOVE -1       TO LINE2L                       EL1279
00609                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT      EL1279
00610                     GO TO 8200-SEND-DATAONLY                      EL1279
00611                  ELSE                                             EL1279
00612                     NEXT SENTENCE                                 EL1279
00613                  END-IF                                           EL1279
00614               END-IF                                              EL1279
00615            END-IF                                                 EL1279
00616         END-IF                                                    EL1279
00617      ELSE                                                         EL1279
00618         IF LINE2L = ZEROS                                         EL1279
00619            NEXT SENTENCE                                          EL1279
00620         ELSE                                                      EL1279
00621            MOVE -1               TO LINE2L                        EL1279
00622            MOVE ER-0041          TO EMI-ERROR                     EL1279
00623            MOVE AL-UNBON         TO LINE2A                        EL1279
00624            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1279
00625            GO TO 8200-SEND-DATAONLY                               EL1279
00626         END-IF                                                    EL1279
00627      END-IF.                                                      EL1279
00628      GO TO 2599-EXIT.                                             EL1279
00629  2510-MAX-CHECK.                                                  EL1279
00630      IF LINE2I NOT NUMERIC                                        EL1279
00631         MOVE -1                  TO LINE2L                        EL1279
00632         MOVE ER-0032             TO EMI-ERROR                     EL1279
00633         MOVE AL-UNBON            TO LINE2A                        EL1279
00634         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
00635         GO TO 8200-SEND-DATAONLY                                  EL1279
00636      ELSE                                                         EL1279
00637         COMPUTE ROLL-COUNTER = LINE2I + PI-TOTAL-LINES            EL1279
00638         IF ROLL-COUNTER GREATER THAN MAX-LINES                    EL1279
00639            MOVE -1               TO LINE2L                        EL1279
00640            MOVE ER-0044          TO EMI-ERROR                     EL1279
00641            MOVE AL-UNBON         TO LINE2A                        EL1279
00642            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1279
00643            GO TO 8200-SEND-DATAONLY                               EL1279
00644         END-IF                                                    EL1279
00645      END-IF.                                                      EL1279
00646  2599-EXIT.                                                       EL1279
00647       EXIT.                                                       EL1279
00648      EJECT                                                        EL1279
00649  3000-DELETE-LINES.                                               EL1279
00650      IF LINE2L = ZEROS AND LINE2I = 1                             EL1279
00651         MOVE LINE1I              TO LINE2I                        EL1279
00652      END-IF.                                                      EL1279
00653                                                                   EL1279
00654      IF LINE2I > PI-TOTAL-LINES OR < LINE1I                       EL1279
00655         MOVE ER-0049             TO EMI-ERROR                     EL1279
00656         MOVE AL-UNBON            TO LINE2A                        EL1279
00657         MOVE -1                  TO LINE2L                        EL1279
00658         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
00659         GO TO 8200-SEND-DATAONLY                                  EL1279
00660      END-IF.                                                      EL1279
00661                                                                   EL1279
00662      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1279
00663      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1279
00664              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1279
00665              SC-INDX > NUM-LINES-PER-SCREEN                       EL1279
00666      SET TB-INDX TO LINE1I.                                       EL1279
00667                                                                   EL1279
00668      IF NOT EMI-NO-ERRORS                                         EL1279
00669         GO TO 8200-SEND-DATAONLY                                  EL1279
00670      END-IF.                                                      EL1279
00671                                                                   EL1279
00672      SET TB-INDX TO LINE1I                                        EL1279
00673      COMPUTE ROLL-COUNTER = LINE2I - LINE1I + 1.                  EL1279
00674                                                                   EL1279
00675      IF LINE2I NOT = PI-TOTAL-LINES                               EL1279
00676         SET TB-INDX1 TO LINE2I                                    EL1279
00677         SET TB-INDX1 UP BY 1                                      EL1279
00678         PERFORM 3100-DELETE-TABLE-ENTRIES                         EL1279
00679                 UNTIL TB-INDX1 > PI-TOTAL-LINES                   EL1279
00680      END-IF.                                                      EL1279
00681                                                                   EL1279
00682      PERFORM 3150-BLANK-TABLE-ENTRIES                             EL1279
00683              ROLL-COUNTER TIMES.                                  EL1279
00684      SUBTRACT ROLL-COUNTER FROM PI-TOTAL-LINES.                   EL1279
00685                                                                   EL1279
00686      IF PI-CURRENT-LINE > PI-TOTAL-LINES                          EL1279
00687         MOVE PI-TOTAL-LINES      TO PI-CURRENT-LINE               EL1279
00688         SUBTRACT 1 FROM PI-CURRENT-LINE                           EL1279
00689      END-IF.                                                      EL1279
00690                                                                   EL1279
00691      SET TB-INDX  TO PI-CURRENT-LINE                              EL1279
00692      MOVE LOW-VALUES             TO EL1279AI                      EL1279
00693                                                                   EL1279
00694      IF PI-CURRENT-LINE > ZERO                                    EL1279
00695          PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                EL1279
00696              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1279
00697              SC-INDX > NUM-LINES-PER-SCREEN                       EL1279
00698          PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT               EL1279
00699      END-IF.                                                      EL1279
00700                                                                   EL1279
00701      MOVE '1'                    TO PI-UPDATE-SW.                 EL1279
00702      IF PI-TOTAL-LINES = ZEROS                                    EL1279
00703         MOVE ZEROS               TO PI-CURRENT-LINE               EL1279
00704      END-IF.                                                      EL1279
00706      GO TO 8100-SEND-INITIAL-MAP.                                 EL1279
00707      EJECT                                                        EL1279
00708                                                                   EL1279
00709  3100-DELETE-TABLE-ENTRIES.                                       EL1279
00710      MOVE REC-ENT (TB-INDX1)     TO REC-ENT (TB-INDX)             EL1279
00711      SET TB-INDX TB-INDX1 UP BY 1.                                EL1279
00712                                                                   EL1279
00713  3150-BLANK-TABLE-ENTRIES.                                        EL1279
00714      MOVE SPACES               TO REC-ENT (TB-INDX).              EL1279
00715      MOVE SAVE-BIN-DATE        TO REC-LAST-MAINT-DT (TB-INDX).    EL1279
00716      MOVE EIBTIME              TO REC-LAST-MAINT-HHMMSS (TB-INDX).EL1279
00717      MOVE PI-PROCESSOR-ID      TO REC-LAST-MAINT-BY (TB-INDX).    EL1279
00718      SET TB-INDX UP BY 1.                                         EL1279
00719      EJECT                                                        EL1279
00720  3500-INSERT-LINES.                                               EL1279
00721                                                                   EL1279
00722      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1279
00723      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1279
00724              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1279
00725              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1279
00726                                                                   EL1279
00727      IF NOT EMI-NO-ERRORS                                         EL1279
00728         GO TO 8200-SEND-DATAONLY                                  EL1279
00729      END-IF.                                                      EL1279
00730                                                                   EL1279
00731      SET TB-INDX TO PI-TOTAL-LINES.                               EL1279
00732      ADD LINE2I TO PI-TOTAL-LINES.                                EL1279
00733      SET TB-INDX1 TO PI-TOTAL-LINES.                              EL1279
00734      PERFORM 3600-INSERT-TABLE-ENTRIES                            EL1279
00735              UNTIL TB-INDX = LINE1I.                              EL1279
00736      SET TB-INDX UP BY 1.                                         EL1279
00737                                                                   EL1279
00738      IF LINE1I EQUAL ZERO                                         EL1279
00739          SET PI-CURRENT-LINE TO 1                                 EL1279
00740      ELSE                                                         EL1279
00741          SET PI-CURRENT-LINE TO LINE1I                            EL1279
00742      END-IF.                                                      EL1279
00743                                                                   EL1279
00744      COMPUTE ROLL-COUNTER = PI-CURRENT-LINE +                     EL1279
00745                             NUM-LINES-PER-SCREEN.                 EL1279
00746      IF TB-INDX NOT LESS THAN ROLL-COUNTER OR                     EL1279
00747                     LESS THAN PI-CURRENT-LINE                     EL1279
00748         SET SC-INDX TO 1                                          EL1279
00749         SET SC-INDX DOWN BY 1                                     EL1279
00750      ELSE                                                         EL1279
00751         SET ROLL-COUNTER TO TB-INDX                               EL1279
00752         COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE     EL1279
00753                   + 1                                             EL1279
00754         SET SC-INDX TO ROLL-COUNTER                               EL1279
00755      END-IF.                                                      EL1279
00756                                                                   EL1279
00757      PERFORM 3150-BLANK-TABLE-ENTRIES LINE2I TIMES.               EL1279
00758      SET TB-INDX TO PI-CURRENT-LINE.                              EL1279
00759      MOVE LOW-VALUES             TO EL1279AI.                     EL1279
00760                                                                   EL1279
00761      IF SC-INDX NOT = ZERO                                        EL1279
00762         MOVE -1 TO SC-TEXTL (SC-INDX)                             EL1279
00763      END-IF.                                                      EL1279
00764                                                                   EL1279
00765      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1279
00766             VARYING SC-INDX FROM 1 BY 1 UNTIL                     EL1279
00767             SC-INDX > NUM-LINES-PER-SCREEN.                       EL1279
00768      PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT.                  EL1279
00769      MOVE '1'                    TO PI-UPDATE-SW.                 EL1279
00770      GO TO 8100-SEND-INITIAL-MAP.                                 EL1279
00771                                                                   EL1279
00772  3600-INSERT-TABLE-ENTRIES.                                       EL1279
00773      MOVE REC-ENT (TB-INDX)      TO REC-ENT (TB-INDX1).           EL1279
00774      SET TB-INDX TB-INDX1 DOWN BY 1.                              EL1279
00775      EJECT                                                        EL1279
00776                                                                   EL1279
00777  4000-CHANGE-ROUTINE.                                             EL1279
00778      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1279
00779      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1279
00780              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1279
00781              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1279
00782                                                                   EL1279
00783      IF NOT EMI-NO-ERRORS                                         EL1279
00784         GO TO 8200-SEND-DATAONLY                                  EL1279
00785      END-IF.                                                      EL1279
00786                                                                   EL1279
00787      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1279
00788      MOVE SPACES                 TO ERRMSGBO.                     EL1279
00789      GO TO 8200-SEND-DATAONLY.                                    EL1279
00790                                                                   EL1279
00791      EJECT                                                        EL1279
00792  4500-SAVE-DATA.                                                  EL1279
00793      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1279
00794      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1279
00795              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1279
00796              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1279
00797      IF NOT EMI-NO-ERRORS                                         EL1279
00798         GO TO 8200-SEND-DATAONLY                                  EL1279
00799      END-IF.                                                      EL1279
00800                                                                   EL1279
00801      EXEC CICS HANDLE CONDITION                                   EL1279
00802           NOTFND(4610-ENDBR)                                      EL1279
00803           NOTOPEN(6000-NOT-OPEN)                                  EL1279
00804           ENDFILE(4610-ENDBR)                                     EL1279
00805      END-EXEC.                                                    EL1279
00806                                                                   EL1279
00807  4610-LOOP.                                                       EL1279
00808      EXEC CICS READ                                               EL1279
00809          DATASET (ERCNOT-FILE-ID)                                 EL1279
00810          RIDFLD  (ERCNOT-KEY)                                     EL1279
00811          SET     (ADDRESS OF CERT-NOTE-FILE)                      EL1279
00812          GTEQ                                                     EL1279
00813      END-EXEC.                                                    EL1279
00814                                                                   EL1279
00815      MOVE CZ-CONTROL-PRIMARY     TO ERCNOT-KEY.                   EL1279
00816                                                                   EL1279
00817      IF ERCNOT-PARTIAL-KEY NOT = SV-PRIOR-KEY                     EL1279
00818          MOVE SV-PRIOR-KEY       TO ERCNOT-PARTIAL-KEY            EL1279
00819          GO TO 4610-ENDBR                                         EL1279
00820      END-IF.                                                      EL1279
00821                                                                   EL1279
00822      EXEC CICS DELETE                                             EL1279
00823          DATASET (ERCNOT-FILE-ID)                                 EL1279
00824          RIDFLD  (ERCNOT-KEY)                                     EL1279
00825      END-EXEC.                                                    EL1279
00826                                                                   EL1279
00827      GO TO 4610-LOOP.                                             EL1279
00828  4610-ENDBR.                                                      EL1279
00829      EXEC CICS GETMAIN                                            EL1279
00830           LENGTH(ERCNOT-LENGTH)                                   EL1279
00831           SET(ADDRESS OF CERT-NOTE-FILE)                          EL1279
00832           INITIMG(GETMAIN-SPACE)                                  EL1279
00833      END-EXEC.                                                    EL1279
00834                                                                   EL1279
00835      MOVE ZERO                   TO  ERCNOT-SEQ.                  EL1279
00836      MOVE ZERO                   TO  WS-BLANK-LINES.              EL1279
00837                                                                   EL1279
00838      PERFORM 4700-WRITE-FILE THRU 4799-EXIT                       EL1279
00839              VARYING TB-INDX FROM 1 BY 1 UNTIL                    EL1279
00840              TB-INDX > PI-TOTAL-LINES.                            EL1279
00841                                                                   EL1279
00842      SUBTRACT WS-BLANK-LINES FROM PI-TOTAL-LINES.                 EL1279
00843                                                                   EL1279
00844      PERFORM 4800-CERTIFICATE-UPDATE THRU 4899-EXIT.              EL1279
00845                                                                   EL1279
00846      GO TO 9410-RETURN.                                           EL1279
00847                                                                   EL1279
00848  4700-WRITE-FILE.                                                 EL1279
00849      IF PI-CERT-NOTE AND REC-TEXT (TB-INDX)  EQUAL SPACES         EL1279
00850          ADD +1                  TO  WS-BLANK-LINES               EL1279
00851          GO TO 4799-EXIT                                          EL1279
00852      END-IF.                                                      EL1279
00853                                                                   EL1279
00854      MOVE SPACES                 TO  CERT-NOTE-FILE.              EL1279
00855      ADD 1                       TO  ERCNOT-SEQ.                  EL1279
00856      MOVE ERCNOT-KEY             TO  CZ-CONTROL-PRIMARY.          EL1279
00857      MOVE  'CZ'                  TO  CZ-RECORD-ID.                EL1279
00858      MOVE REC-TEXT (TB-INDX)     TO  CZ-NOTE.                     EL1279
00859      MOVE REC-LAST-MAINT-BY (TB-INDX)                             EL1279
00860                                  TO  CZ-LAST-MAINT-USER.          EL1279
00861      MOVE REC-LAST-MAINT-HHMMSS (TB-INDX)                         EL1279
00862                                  TO  CZ-LAST-MAINT-HHMMSS.        EL1279
00863      MOVE REC-LAST-MAINT-DT (TB-INDX)                             EL1279
00864                                  TO  CZ-LAST-MAINT-DT.            EL1279
00865                                                                   EL1279
00866      EXEC CICS WRITE                                              EL1279
00867           DATASET(ERCNOT-FILE-ID)                                 EL1279
00868           FROM(CERT-NOTE-FILE)                                    EL1279
00869           RIDFLD(ERCNOT-KEY)                                      EL1279
00870      END-EXEC.                                                    EL1279
00871  4799-EXIT.                                                       EL1279
00872       EXIT.                                                       EL1279
00873                                                                   EL1279
00874      EJECT                                                        EL1279
00875  4800-CERTIFICATE-UPDATE.                                         EL1279
00876                                                                   EL1279
00877      EXEC CICS HANDLE CONDITION                                   EL1279
00878          NOTFND   (4899-EXIT)                                     EL1279
00879      END-EXEC.                                                    EL1279
00880                                                                   EL1279
00881      EXEC CICS READ                                               EL1279
00882      EQUAL                                                        EL1279
00883      DATASET   (ELCERT-FILE-ID)                                   EL1279
00884      SET       (ADDRESS OF CERTIFICATE-MASTER)                    EL1279
00885      RIDFLD    (ELCERT-KEY)                                       EL1279
00886      UPDATE                                                       EL1279
00887      END-EXEC.                                                    EL1279
00888                                                                   EL1279
00889      IF PI-TOTAL-LINES > 0                                        EL1279
00890         IF PI-CERT-NOTE                                           EL1279
00891            MOVE 'Y'           TO PI-CERT-NOTES-EXIST              EL1279
00892            IF CM-NOTE-SW = ' '                                    EL1279
00893               MOVE '1'        TO CM-NOTE-SW                       EL1279
00894            ELSE                                                   EL1279
00895              IF CM-NOTE-SW = '2'                                  EL1279
00896                 MOVE '3'      TO CM-NOTE-SW                       EL1279
00897              ELSE                                                 EL1279
00898                IF CM-NOTE-SW = '4'                                EL1279
00899                   MOVE '5'    TO CM-NOTE-SW                       EL1279
00900                ELSE                                               EL1279
00901                  IF CM-NOTE-SW = '6'                              EL1279
00902                     MOVE '7'  TO CM-NOTE-SW                       EL1279
00903                  END-IF                                           EL1279
00904                END-IF                                             EL1279
00905              END-IF                                               EL1279
00906            END-IF                                                 EL1279
00907         ELSE                                                      EL1279
00908            MOVE 'Y'           TO PI-CLAIM-NOTES-EXIST             EL1279
00909            IF CM-NOTE-SW = ' '                                    EL1279
00910               MOVE '4'        TO CM-NOTE-SW                       EL1279
00911            ELSE                                                   EL1279
00912              IF CM-NOTE-SW = '1'                                  EL1279
00913                 MOVE '5'      TO CM-NOTE-SW                       EL1279
00914              ELSE                                                 EL1279
00915                IF CM-NOTE-SW = '2'                                EL1279
00916                   MOVE '6'    TO CM-NOTE-SW                       EL1279
00917                ELSE                                               EL1279
00918                  IF CM-NOTE-SW = '3'                              EL1279
00919                     MOVE '7'  TO CM-NOTE-SW                       EL1279
00920                  END-IF                                           EL1279
00921                END-IF                                             EL1279
00922              END-IF                                               EL1279
00923            END-IF                                                 EL1279
00924         END-IF                                                    EL1279
00925      ELSE                                                         EL1279
00926         IF PI-CERT-NOTE                                           EL1279
00927            MOVE 'N'           TO PI-CERT-NOTES-EXIST              EL1279
00928            IF CM-NOTE-SW = '1'                                    EL1279
00929               MOVE ' '        TO CM-NOTE-SW                       EL1279
00930            ELSE                                                   EL1279
00931              IF CM-NOTE-SW = '3'                                  EL1279
00932                 MOVE '2'      TO CM-NOTE-SW                       EL1279
00933              ELSE                                                 EL1279
00934                IF CM-NOTE-SW = '5'                                EL1279
00935                   MOVE '4'    TO CM-NOTE-SW                       EL1279
00936                ELSE                                               EL1279
00937                  IF CM-NOTE-SW = '7'                              EL1279
00938                     MOVE '6'  TO CM-NOTE-SW                       EL1279
00939                  END-IF                                           EL1279
00940                END-IF                                             EL1279
00941              END-IF                                               EL1279
00942            END-IF                                                 EL1279
00943         ELSE                                                      EL1279
00944            MOVE 'N'           TO PI-CLAIM-NOTES-EXIST             EL1279
00945            IF CM-NOTE-SW = '4'                                    EL1279
00946               MOVE ' '        TO CM-NOTE-SW                       EL1279
00947            ELSE                                                   EL1279
00948              IF CM-NOTE-SW = '5'                                  EL1279
00949                 MOVE '1'      TO CM-NOTE-SW                       EL1279
00950              ELSE                                                 EL1279
00951                IF CM-NOTE-SW = '6'                                EL1279
00952                   MOVE '2'    TO CM-NOTE-SW                       EL1279
00953                ELSE                                               EL1279
00954                  IF CM-NOTE-SW = '7'                              EL1279
00955                     MOVE '3'  TO CM-NOTE-SW                       EL1279
00956                  END-IF                                           EL1279
00957                END-IF                                             EL1279
00958              END-IF                                               EL1279
00959            END-IF                                                 EL1279
00960         END-IF                                                    EL1279
00961      END-IF.                                                      EL1279
00962                                                                   EL1279
00963                                                                   EL1279
00964      EXEC CICS REWRITE                                            EL1279
00965          FROM      (CERTIFICATE-MASTER)                           EL1279
00966          DATASET   (ELCERT-FILE-ID)                               EL1279
00967      END-EXEC.                                                    EL1279
00968                                                                   EL1279
00969  4899-EXIT.                                                       EL1279
00970       EXIT.                                                       EL1279
00971                                                                   EL1279
00972  4900-SET-NOTES-FLAG.                                             EL1279
00973                                                                   EL1279
00974      EXEC CICS HANDLE CONDITION                                   EL1279
00975          NOTFND   (4900-EXIT)                                     EL1279
00976      END-EXEC.                                                    EL1279
00977                                                                   EL1279
00978      MOVE SPACES                 TO  ELCERT-KEY.                  EL1279
00979      MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.           EL1279
00980      MOVE PI-CARRIER             TO  ELCERT-CARRIER.              EL1279
00981      MOVE PI-GROUPING            TO  ELCERT-GROUPING.             EL1279
00982      MOVE PI-STATE               TO  ELCERT-STATE.                EL1279
00983      MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.              EL1279
00984      MOVE PI-CERT-EFF-DT         TO  ELCERT-EFF-DT.               EL1279
00985      MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.           EL1279
00986      MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.             EL1279
00987                                                                   EL1279
00988      EXEC CICS READ                                               EL1279
00989      EQUAL                                                        EL1279
00990      DATASET   (ELCERT-FILE-ID)                                   EL1279
00991      SET       (ADDRESS OF CERTIFICATE-MASTER)                    EL1279
00992      RIDFLD    (ELCERT-KEY)                                       EL1279
00993      END-EXEC.                                                    EL1279
00994                                                                   EL1279
00995      MOVE 'N'                    TO PI-BILLING-NOTES-EXIST        EL1279
00996                                     PI-CERT-NOTES-EXIST           EL1279
00997                                     PI-CLAIM-NOTES-EXIST.         EL1279
00998      IF CM-NOTE-SW EQUAL '2' OR '3' OR '6' OR '7'                 EL1279
00999           MOVE 'Y'               TO PI-BILLING-NOTES-EXIST        EL1279
01000      END-IF.                                                      EL1279
01001      IF CM-NOTE-SW EQUAL '1' OR '3' OR '5' OR '7'                 EL1279
01002           MOVE 'Y'               TO PI-CERT-NOTES-EXIST           EL1279
01003      END-IF.                                                      EL1279
01004      IF CM-NOTE-SW EQUAL '4' OR '5' OR '6' OR '7'                 EL1279
01005           MOVE 'Y'               TO PI-CLAIM-NOTES-EXIST          EL1279
01006      END-IF.                                                      EL1279
01007                                                                   EL1279
01008                                                                   EL1279
01009  4900-EXIT.                                                       EL1279
01010       EXIT.                                                       EL1279
01011                                                                   EL1279
01012      EJECT                                                        EL1279
01013  5000-ADD-NEW-LINES.                                              EL1279
01014      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1279
01015      MOVE 'N'                    TO WS-SCREEN-LINE.               EL1279
01016      MOVE 1                      TO WS-SUB.                       EL1279
01017      MOVE ZERO                   TO WS-SUB1.                      EL1279
01018      PERFORM VARYING SC-INDX FROM NUM-LINES-PER-SCREEN BY -1      EL1279
01019              UNTIL SCREEN-LINE-FOUND OR SC-INDX < 1               EL1279
01020          IF SC-TEXT (SC-INDX) >  SPACES                           EL1279
01021              SET WS-SUB          TO SC-INDX                       EL1279
01022              MOVE 'Y'            TO WS-SCREEN-LINE                EL1279
01023          END-IF                                                   EL1279
01024      END-PERFORM.                                                 EL1279
01025      IF PI-TOTAL-LINES = 0                                        EL1279
01026          MOVE 1                  TO WS-SUB1                       EL1279
01027      ELSE                                                         EL1279
01028          MOVE 2                  TO WS-SUB1                       EL1279
01029      END-IF.                                                      EL1279
01030      PERFORM VARYING SC-INDX FROM WS-SUB1 BY 1                    EL1279
01031              UNTIL SC-INDX GREATER THAN WS-SUB                    EL1279
01032          IF SC-TEXTL (SC-INDX) EQUAL ZEROS                        EL1279
01033              MOVE 1      TO SC-TEXTL (SC-INDX)                    EL1279
01034              MOVE SPACES TO SC-TEXT (SC-INDX)                     EL1279
                   MOVE LOW-VALUES TO SC-MTBY (SC-INDX)
                                      SC-MTDT (SC-INDX)
01035          END-IF                                                   EL1279
01036      END-PERFORM.                                                 EL1279
01037      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1279
01038              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1279
01039              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1279
01040                                                                   EL1279
01041      IF NOT EMI-NO-ERRORS                                         EL1279
01042         GO TO 8200-SEND-DATAONLY                                  EL1279
01043      END-IF.                                                      EL1279
01044                                                                   EL1279
01045      MOVE PI-TOTAL-LINES         TO  PI-CURRENT-LINE.             EL1279
01046      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1279
01047      MOVE LOW-VALUES             TO  EL1279AI.                    EL1279
01048      SET TB-INDX TO PI-CURRENT-LINE.                              EL1279
01049      MOVE 'A'                    TO FUNCTI.                       EL1279
01050      MOVE -1                     TO  SC-TEXTL (2).                EL1279
01051      MOVE AL-UANON               TO  FUNCTA.                      EL1279
01052      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1279
01053              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1279
01054              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1279
01055      MOVE '1'                    TO PI-UPDATE-SW.                 EL1279
01056      GO TO 8100-SEND-INITIAL-MAP.                                 EL1279
01057      EJECT                                                        EL1279
01058  5500-LOOKUP.                                                     EL1279
01059      PERFORM 7500-READ-TS THRU 7599-EXIT.                         EL1279
01060      SET TB-INDX TO PI-CURRENT-LINE.                              EL1279
01061      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1279
01062              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1279
01063              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1279
01064                                                                   EL1279
01065      IF NOT EMI-NO-ERRORS                                         EL1279
01066         GO TO 8200-SEND-DATAONLY                                  EL1279
01067      END-IF.                                                      EL1279
01068                                                                   EL1279
01069      MOVE LINE1I                 TO  PI-CURRENT-LINE.             EL1279
01070      SET TB-INDX                 TO PI-CURRENT-LINE.              EL1279
01071      MOVE LOW-VALUES             TO  EL1279AI.                    EL1279
01072      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1279
01073              VARYING SC-INDX FROM 1 BY 1                          EL1279
01074              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                EL1279
01075      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1279
01076      GO TO 8100-SEND-INITIAL-MAP.                                 EL1279
01077      EJECT                                                        EL1279
01078  6000-NOT-OPEN.                                                   EL1279
01079      MOVE ER-2954                TO  EMI-ERROR.                   EL1279
01080      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1279
01081                                                                   EL1279
01082      IF EIBAID = DFHCLEAR                                         EL1279
01083          GO TO 9410-RETURN                                        EL1279
01084      ELSE                                                         EL1279
01085          GO TO 8100-SEND-INITIAL-MAP                              EL1279
01086      END-IF.                                                      EL1279
01087      EJECT                                                        EL1279
01088  7000-BUILD-TABLE.                                                EL1279
01089                                                                   EL1279
01090      SET TB-INDX TO 1.                                            EL1279
01091      MOVE ZEROS                  TO  PI-TOTAL-LINES               EL1279
01092                                      PI-CURRENT-LINE              EL1279
01093                                      PI-TEMP-STOR-ITEMS           EL1279
01094                                      PI-UPDATE-SW.                EL1279
01095      MOVE LOW-VALUES             TO  EL1279AI.                    EL1279
01099                                                                   EL1279
01100 ****IF TEMP STORAGE EXISTS, DELETE IT.                            EL1279
01101      IF PI-CHANGE-IN-NOTE-TYPE                                    EL1279
01102          MOVE 'N' TO PI-SET-NOTE-CHANGE                           EL1279
01103      ELSE                                                         EL1279
01104          PERFORM 7500-READ-TS THRU 7599-EXIT                      EL1279
01105      END-IF.                                                      EL1279
01106                                                                   EL1279
01107      IF PI-TEMP-STOR-ITEMS NOT = ZERO                             EL1279
01108         PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT              EL1279
01109      END-IF.                                                      EL1279
01110                                                                   EL1279
01111      EXEC CICS HANDLE CONDITION                                   EL1279
01112           NOTFND(7010-ENDBR)                                      EL1279
01113           NOTOPEN(6000-NOT-OPEN)                                  EL1279
01114           ENDFILE(7010-ENDBR)                                     EL1279
01115      END-EXEC.                                                    EL1279
01116                                                                   EL1279
01117      EXEC CICS STARTBR                                            EL1279
01118           DATASET(ERCNOT-FILE-ID)                                 EL1279
01119           RIDFLD(ERCNOT-KEY)                                      EL1279
01120           KEYLENGTH(ERCNOT-START-LENGTH)                          EL1279
01121           GENERIC                                                 EL1279
01122           GTEQ                                                    EL1279
01123      END-EXEC.                                                    EL1279
01124                                                                   EL1279
01125  7001-LOOP.                                                       EL1279
01126      EXEC CICS READNEXT                                           EL1279
01127           SET(ADDRESS OF CERT-NOTE-FILE)                          EL1279
01128           DATASET(ERCNOT-FILE-ID)                                 EL1279
01129           RIDFLD(ERCNOT-KEY)                                      EL1279
01130      END-EXEC.                                                    EL1279
01131                                                                   EL1279
01132      IF CZ-COMPANY-CD NOT = SV-COMPANY-CD                         EL1279
01133          GO TO 7010-ENDBR                                         EL1279
01134      END-IF.                                                      EL1279
01135                                                                   EL1279
01136      IF (CZ-CARRIER = SV-CARRIER)                                 EL1279
01137         AND (CZ-GROUPING = SV-GROUPING)                           EL1279
01138         AND (CZ-STATE = SV-STATE)                                 EL1279
01139         AND (CZ-ACCOUNT = SV-ACCOUNT)                             EL1279
01140         AND (CZ-CERT-EFF-DT = SV-EFF-DT)                          EL1279
01141         AND (CZ-CERT-NO = SV-CERT-NO)                             EL1279
01142         AND (CZ-RECORD-TYPE = PI-CURR-NOTE-REC-TYPE)              EL1279
01143           MOVE CZ-NOTE TO REC-TEXT (TB-INDX)                      EL1279
01144           MOVE CZ-LAST-MAINT-USER TO REC-LAST-MAINT-BY (TB-INDX)  EL1279
01145           MOVE CZ-LAST-MAINT-DT TO REC-LAST-MAINT-DT (TB-INDX)    EL1279
01146           MOVE CZ-LAST-MAINT-HHMMSS TO                            EL1279
01147                          REC-LAST-MAINT-HHMMSS (TB-INDX)          EL1279
01155           SET TB-INDX UP BY 1                                     EL1279
01156           GO TO 7001-LOOP                                         EL1279
01157      END-IF.                                                      EL1279
01158  7010-ENDBR.                                                      EL1279
01159      IF TB-INDX = 1                                               EL1279
01160          MOVE ER-0006            TO EMI-ERROR                     EL1279
01161          MOVE 'A'                TO FUNCTI                        EL1279
01162          MOVE -1                 TO SC-TEXTL (1)                  EL1279
01163          MOVE AL-UANON           TO FUNCTA                        EL1279
01164          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1279
01165          MOVE ZEROS              TO PI-TOTAL-LINES                EL1279
01166          GO TO 8100-SEND-INITIAL-MAP                              EL1279
01167      END-IF.                                                      EL1279
01168                                                                   EL1279
01169      EXEC CICS ENDBR                                              EL1279
01170           DATASET(ERCNOT-FILE-ID)                                 EL1279
01171      END-EXEC.                                                    EL1279
01172                                                                   EL1279
01173      SET TB-INDX DOWN BY 1.                                       EL1279
01174      SET PI-TOTAL-LINES TO TB-INDX.                               EL1279
01175      MOVE 1                      TO PI-CURRENT-LINE.              EL1279
01176                                                                   EL1279
01177  7050-FORMAT-LINES.                                               EL1279
01178      SET TB-INDX TO PI-CURRENT-LINE.                              EL1279
01179      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1279
01180              VARYING SC-INDX FROM 1                               EL1279
01181              BY 1 UNTIL SC-INDX > NUM-LINES-PER-SCREEN.           EL1279
01182      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1279
01183      GO TO 8100-SEND-INITIAL-MAP.                                 EL1279
01184      EJECT                                                        EL1279
01185  7100-FORMAT-SCREEN.                                              EL1279
01186      IF TB-INDX > PI-TOTAL-LINES                                  EL1279
01187         IF FUNCTI NOT = 'A'                                       EL1279
01188            MOVE AL-PANON         TO SC-TEXTA (SC-INDX)            EL1279
01189         END-IF                                                    EL1279
01190      END-IF.                                                      EL1279
01191                                                                   EL1279
01192      IF TB-INDX > PI-TOTAL-LINES                                  EL1279
01193          GO TO 7100-EXIT                                          EL1279
01194      END-IF.                                                      EL1279
01195                                                                   EL1279
01196      SET WS-LINE TO TB-INDX.                                      EL1279
01201      MOVE WS-LINE TO SC-LIN (SC-INDX).                            EL1279
01202      MOVE REC-TEXT (TB-INDX)     TO SC-TEXT (SC-INDX).            EL1279
           MOVE REC-LAST-MAINT-BY (TB-INDX) TO SC-MTBY (SC-INDX). 
           MOVE REC-LAST-MAINT-DT (TB-INDX) TO DC-BIN-DATE-1.
           MOVE ' '                    TO DC-OPTION-CODE.
01433      PERFORM 9700-LINK-DATE-CONVERT.                              EL1279
01434      MOVE DC-GREG-DATE-1-MDY     TO SC-MTDT (SC-INDX).  
01203      SET ROLL-COUNTER TO TB-INDX.                                 EL1279
01204                                                                   EL1279
01205      IF NOT MODIFY-CAP                                            EL1279
01206          MOVE AL-PANOF           TO SC-TEXTA (SC-INDX)            EL1279
01207          SET TB-INDX UP BY 1                                      EL1279
01208          GO TO 7100-EXIT                                          EL1279
01209      END-IF.                                                      EL1279
01210                                                                   EL1279
01211      SET TB-INDX UP BY 1.                                         EL1279
01212                                                                   EL1279
01213  7100-EXIT.                                                       EL1279
01214       EXIT.                                                       EL1279
01215                                                                   EL1279
01235  7200-PUT-TEMP-STOR.                                              EL1279
01236      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL1279
01237      SET TS-INDX TO 1.                                            EL1279
01238      MOVE 0                      TO PI-TEMP-STOR-ITEMS.           EL1279
01239      PERFORM 7300-WRITE-TS THRU 7399-EXIT                         EL1279
01240              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP  EL1279
01241              UNTIL TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES.    EL1279
01242  7249-EXIT.                                                       EL1279
01243       EXIT.                                                       EL1279
01244  7250-DELETE-TEMP-STOR.                                           EL1279
01245      EXEC CICS HANDLE CONDITION                                   EL1279
01246           QIDERR(7299-EXIT)                                       EL1279
01247      END-EXEC.                                                    EL1279
01248      EXEC CICS DELETEQ TS                                         EL1279
01249           QUEUE(QID)                                              EL1279
01250      END-EXEC.                                                    EL1279
01251  7299-EXIT.                                                       EL1279
01252      EXIT.                                                        EL1279
01253      EJECT                                                        EL1279
01254  7300-WRITE-TS.                                                   EL1279
01255      MOVE TS-GROUP (TS-INDX)     TO TS-WORK-AREA.                 EL1279
01256      SET TS-INDX UP BY 1.                                         EL1279
01257      ADD 1 TO PI-TEMP-STOR-ITEMS.                                 EL1279
01258      EXEC CICS WRITEQ TS                                          EL1279
01259           FROM(TS-WORK-AREA)                                      EL1279
01260           QUEUE(QID)                                              EL1279
01261           LENGTH(TS-LENGTH)                                       EL1279
01262           ITEM(PI-TEMP-STOR-ITEMS)                                EL1279
01263      END-EXEC.                                                    EL1279
01264  7399-EXIT.                                                       EL1279
01265      EXIT.                                                        EL1279
01266      EJECT                                                        EL1279
01267  7400-PAGE-ROUTINE.                                               EL1279
01268                                                                   EL1279
01269      IF PFENTERL NOT = ZEROS                                      EL1279
01270         MOVE -1                  TO PFENTERL                      EL1279
01271         ELSE                                                      EL1279
01272         MOVE -1                  TO FUNCTL                        EL1279
01273      END-IF.                                                      EL1279
01274                                                                   EL1279
01275      IF PI-TOTAL-LINES = 0                                        EL1279
01276         MOVE ER-0047             TO EMI-ERROR                     EL1279
01277         MOVE -1                  TO FUNCTL                        EL1279
01278         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
01279         GO TO 8200-SEND-DATAONLY                                  EL1279
01280      END-IF.                                                      EL1279
01281                                                                   EL1279
01282      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.     EL1279
01283                                                                   EL1279
01284      IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS         EL1279
01285         MOVE ER-0067             TO EMI-ERROR                     EL1279
01286         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
01287         MOVE 1 TO TEMP-CURR-LINE                                  EL1279
01288      END-IF.                                                      EL1279
01289                                                                   EL1279
01290      IF TEMP-CURR-LINE GREATER THAN PI-TOTAL-LINES                EL1279
01291         MOVE ER-0066             TO EMI-ERROR                     EL1279
01292         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
01293         COMPUTE TEMP-CURR-LINE = PI-TOTAL-LINES + 1               EL1279
01294                                - NUM-LINES-PER-SCREEN             EL1279
01295         IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS      EL1279
01296            MOVE 1                TO TEMP-CURR-LINE                EL1279
01297         END-IF                                                    EL1279
01298      END-IF.                                                      EL1279
01299                                                                   EL1279
01300      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1279
01301      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1279
01302              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1279
01303              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1279
01304                                                                   EL1279
01305      IF EMI-ERROR = ER-0066 OR = ER-0067 OR = ZEROS               EL1279
01306         NEXT SENTENCE                                             EL1279
01307      ELSE                                                         EL1279
01308         GO TO 8200-SEND-DATAONLY                                  EL1279
01309      END-IF.                                                      EL1279
01310                                                                   EL1279
01311      MOVE TEMP-CURR-LINE         TO PI-CURRENT-LINE.              EL1279
01312      SET TB-INDX TO PI-CURRENT-LINE.                              EL1279
01313      MOVE LOW-VALUES             TO EL1279AI.                     EL1279
01314      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1279
01315              VARYING SC-INDX FROM 1 BY 1                          EL1279
01316              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                EL1279
01317      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1279
01318      GO TO 8100-SEND-INITIAL-MAP.                                 EL1279
01319      EJECT                                                        EL1279
01320                                                                   EL1279
01321  7450-SET-INDX.                                                   EL1279
01322      IF PI-CURRENT-LINE = 0 AND PI-TOTAL-LINES = 0                EL1279
01323         SET TB-INDX TO 1                                          EL1279
01324      ELSE                                                         EL1279
01325         PERFORM 7500-READ-TS THRU 7599-EXIT                       EL1279
01326         IF PI-CURRENT-LINE = 0                                    EL1279
01327            SET TB-INDX TO 1                                       EL1279
01328         ELSE                                                      EL1279
01329            SET TB-INDX TO PI-CURRENT-LINE                         EL1279
01330         END-IF                                                    EL1279
01331      END-IF.                                                      EL1279
01332  7450-EXIT.                                                       EL1279
01333       EXIT.                                                       EL1279
01334      EJECT                                                        EL1279
01335  7500-READ-TS.                                                    EL1279
01336      EXEC CICS HANDLE CONDITION                                   EL1279
01337           QIDERR(7590-TS-QIDERR)                                  EL1279
01338           ITEMERR(7585-QID-ITEMERR)                               EL1279
01339      END-EXEC.                                                    EL1279
01340      SET TS-INDX TO 1.                                            EL1279
01341      MOVE 1                      TO QID-ITEM.                     EL1279
01342  7501-LOOP.                                                       EL1279
01343      EXEC CICS READQ TS                                           EL1279
01344           INTO(TS-WORK-AREA)                                      EL1279
01345           QUEUE(QID)                                              EL1279
01346           LENGTH(TS-LENGTH)                                       EL1279
01347           ITEM(QID-ITEM)                                          EL1279
01348      END-EXEC.                                                    EL1279
01349      MOVE TS-WORK-AREA           TO TS-GROUP (TS-INDX).           EL1279
01350      SET TS-INDX UP BY 1.                                         EL1279
01351      ADD 1 TO QID-ITEM.                                           EL1279
01352      GO TO 7501-LOOP.                                             EL1279
01353                                                                   EL1279
01354  7585-QID-ITEMERR.                                                EL1279
01355      IF EIBTRNID NOT = TRANS-ID                                   EL1279
01356         SUBTRACT 1 FROM QID-ITEM                                  EL1279
01357         MOVE QID-ITEM            TO PI-TEMP-STOR-ITEMS            EL1279
01358      END-IF.                                                      EL1279
01359      GO TO 7599-EXIT.                                             EL1279
01360                                                                   EL1279
01361  7590-TS-QIDERR.                                                  EL1279
01362      IF EIBTRNID = TRANS-ID                                       EL1279
01363         AND EIBAID = DFHCLEAR                                     EL1279
01364            GO TO 9410-RETURN                                      EL1279
01365      END-IF.                                                      EL1279
01366      IF EIBTRNID = TRANS-ID                                       EL1279
01367         MOVE ER-0033             TO EMI-ERROR                     EL1279
01368         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
01369         GO TO 8100-SEND-INITIAL-MAP                               EL1279
01370      END-IF.                                                      EL1279
01371                                                                   EL1279
01372  7599-EXIT.                                                       EL1279
01373       EXIT.                                                       EL1279
01374                                                                   EL1279
01375      EJECT                                                        EL1279
01376  7600-UPDATE-TABLE-FROM-SCREEN.                                   EL1279
01377                                                                   EL1279
01378      IF SC-TEXTL (SC-INDX) NOT = ZEROS                            EL1279
01379          IF TB-INDX NOT > PI-TOTAL-LINES                          EL1279
01380              PERFORM 7700-MOVE-DATA THRU 7700-EXIT                EL1279
01381              SET TB-INDX UP BY 1                                  EL1279
01382          ELSE                                                     EL1279
01383              IF PI-TOTAL-LINES = MAX-LINES                        EL1279
01384                  MOVE ER-0051    TO EMI-ERROR                     EL1279
01385                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL1279
01386                  GO TO 8200-SEND-DATAONLY                         EL1279
01387              ELSE                                                 EL1279
01388                  PERFORM 7700-MOVE-DATA THRU 7700-EXIT            EL1279
01389                  SET TB-INDX UP BY 1                              EL1279
01390                  ADD 1 TO PI-TOTAL-LINES                          EL1279
01391              END-IF                                               EL1279
01392          END-IF                                                   EL1279
01393      ELSE                                                         EL1279
01394         IF TB-INDX NOT > PI-TOTAL-LINES                           EL1279
01395            SET TB-INDX UP BY 1                                    EL1279
01396         END-IF                                                    EL1279
01397      END-IF.                                                      EL1279
01398                                                                   EL1279
01399  7699-EXIT.                                                       EL1279
01400       EXIT.                                                       EL1279
01401                                                                   EL1279
01402  7700-MOVE-DATA.                                                  EL1279
01403      MOVE '1'                    TO PI-UPDATE-SW.                 EL1279
01404                                                                   EL1279
01405      IF SC-TEXTL (SC-INDX) NOT = ZEROS                            EL1279
01406         MOVE SC-TEXT (SC-INDX)  TO REC-TEXT (TB-INDX)             EL1279
01407         MOVE PI-PROCESSOR-ID    TO REC-LAST-MAINT-BY (TB-INDX)    EL1279
01409         MOVE EIBTIME            TO REC-LAST-MAINT-HHMMSS (TB-INDX)EL1279
01411         MOVE SAVE-BIN-DATE      TO REC-LAST-MAINT-DT (TB-INDX)    EL1279
01413      END-IF.                                                      EL1279
01414                                                                   EL1279
01415  7700-EXIT.                                                       EL1279
01416       EXIT.                                                       EL1279
01417      EJECT                                                        EL1279
01418  8100-SEND-INITIAL-MAP.                                           EL1279
01419      MOVE SAVE-DATE              TO DATEO.                        EL1279
01420      MOVE EIBTIME                TO TIME-IN.                      EL1279
01421      MOVE TIME-OUT               TO TIMEO.                        EL1279
01422      MOVE PI-COMPANY-ID          TO CMPNYIDO.                     EL1279
01423      MOVE PI-PROCESSOR-ID        TO USERIDO.                      EL1279
01424      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGBO.                     EL1279
01425      MOVE PI-CARRIER             TO FCARRIRO.                     EL1279
01426      MOVE PI-GROUPING            TO FGROUPO.                      EL1279
01427      MOVE PI-STATE               TO FSTO.                         EL1279
01428      MOVE PI-ACCOUNT             TO FACOUNTO.                     EL1279
01429      MOVE PI-CERT-PRIME          TO FCERTO.                       EL1279
01430      MOVE PI-CERT-SFX            TO FCRTSFXO.                     EL1279
01431      MOVE  ' '                   TO DC-OPTION-CODE.               EL1279
01432      MOVE PI-CERT-EFF-DT         TO DC-BIN-DATE-1.                EL1279
01433      PERFORM 9700-LINK-DATE-CONVERT.                              EL1279
01434      MOVE DC-GREG-DATE-1-EDIT    TO FEFFDTO.                      EL1279
01435      MOVE PI-TOTAL-LINES         TO TOTO.                         EL1279
01443      IF PI-CLAIM-NOTE                                             EL1279
01444          MOVE SCR-CLAIM-NOTE-TYPE TO FTYPEO                       EL1279
01445          MOVE SCR-PF6-CERT        TO PF6NOTEO                     EL1279
01446          MOVE SCR-CERT-YN         TO FTYPEYNO                     EL1279
01447          IF PI-CERT-NOTES-EXIST = 'Y'                             EL1279
01448              MOVE 'YES'           TO FCERTYNO                     EL1279
01449          ELSE                                                     EL1279
01450              MOVE 'NO '           TO FCERTYNO                     EL1279
01451          END-IF                                                   EL1279
01452      ELSE                                                         EL1279
01453          MOVE SCR-CERT-NOTE-TYPE TO FTYPEO                        EL1279
01454          MOVE SCR-PF6-CLAIM      TO PF6NOTEO                      EL1279
01455          MOVE SCR-CLAIM-YN       TO FTYPEYNO                      EL1279
01456          IF PI-CLAIM-NOTES-EXIST = 'Y'                            EL1279
01457              MOVE 'YES'           TO FCERTYNO                     EL1279
01458          ELSE                                                     EL1279
01459              MOVE 'NO '           TO FCERTYNO                     EL1279
01460          END-IF                                                   EL1279
01461      END-IF.                                                      EL1279
01462      IF PI-BILLING-NOTES-EXIST = 'Y'                              EL1279
01463          MOVE 'YES'           TO FBILLYNO                         EL1279
01464      ELSE                                                         EL1279
01465          MOVE 'NO '           TO FBILLYNO                         EL1279
01466      END-IF                                                       EL1279
01467                                                                   EL1279
01468      MOVE -1                     TO FUNCTL.                       EL1279
01469                                                                   EL1279
01470      EXEC CICS SEND                                               EL1279
01471           MAP(MAP-NAME)                                           EL1279
01472           MAPSET(MAPSET-NAME)                                     EL1279
01473           FROM(EL1279AO)                                          EL1279
01474           ERASE                                                   EL1279
01475           CURSOR                                                  EL1279
01476      END-EXEC.                                                    EL1279
01477                                                                   EL1279
01478      GO TO 9100-RETURN-TRAN.                                      EL1279
01479                                                                   EL1279
01480  8200-SEND-DATAONLY.                                              EL1279
01481      MOVE EIBTIME                TO TIME-IN.                      EL1279
01482      MOVE TIME-OUT               TO TIMEO.                        EL1279
01483      MOVE PI-COMPANY-ID          TO CMPNYIDO.                     EL1279
01484      MOVE PI-PROCESSOR-ID        TO USERIDO.                      EL1279
01485      MOVE PI-TOTAL-LINES         TO TOTO.                         EL1279
01486                                                                   EL1279
01487      IF NOT EMI-NO-ERRORS                                         EL1279
01488         MOVE EMI-MESSAGE-AREA (1) TO ERRMSGBO                     EL1279
01489      ELSE                                                         EL1279
01490         MOVE -1                  TO FUNCTL                        EL1279
01491      END-IF.                                                      EL1279
01492                                                                   EL1279
01493      EXEC CICS SEND                                               EL1279
01494           MAP(MAP-NAME)                                           EL1279
01495           MAPSET(MAPSET-NAME)                                     EL1279
01496           FROM(EL1279AO)                                          EL1279
01497           DATAONLY                                                EL1279
01498           CURSOR                                                  EL1279
01499      END-EXEC.                                                    EL1279
01500                                                                   EL1279
01501      GO TO 9100-RETURN-TRAN.                                      EL1279
01502                                                                   EL1279
01503  8300-SEND-TEXT.                                                  EL1279
01504      EXEC CICS SEND TEXT                                          EL1279
01505           FROM(LOGOFF-TEXT)                                       EL1279
01506           ERASE                                                   EL1279
01507           FREEKB                                                  EL1279
01508           LENGTH(LOGOFF-LENGTH)                                   EL1279
01509      END-EXEC.                                                    EL1279
01510                                                                   EL1279
01511      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL1279
01512                                                                   EL1279
01513      EXEC CICS RETURN                                             EL1279
01514      END-EXEC.                                                    EL1279
01515                                                                   EL1279
01516  8800-UNAUTHORIZED-ACCESS.                                        EL1279
01517      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL1279
01518      GO TO 8300-SEND-TEXT.                                        EL1279
01519                                                                   EL1279
01520  9000-RETURN-CICS.                                                EL1279
01521      IF PI-CHANGES-MADE                                           EL1279
01522         MOVE ER-0045             TO EMI-ERROR                     EL1279
01523         MOVE -1                  TO FUNCTL                        EL1279
01524         MOVE SPACES              TO PFENTERO                      EL1279
01525         MOVE AL-UNNOF            TO PFENTERA                      EL1279
01526         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
01527         GO TO 8200-SEND-DATAONLY                                  EL1279
01528      END-IF.                                                      EL1279
01529                                                                   EL1279
01530      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL1279
01531      MOVE XCTL-005               TO PGM-NAME.                     EL1279
01532      GO TO 9300-XCTL.                                             EL1279
01533                                                                   EL1279
01534  9100-RETURN-TRAN.                                                EL1279
01535      MOVE SCRN-NUMBER            TO PI-CURRENT-SCREEN-NO.         EL1279
01536      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL1279
01537      EXEC CICS RETURN                                             EL1279
01538           TRANSID(TRANS-ID)                                       EL1279
01539           COMMAREA(PROGRAM-INTERFACE-BLOCK)                       EL1279
01540           LENGTH(PI-COMM-LENGTH)                                  EL1279
01541      END-EXEC.                                                    EL1279
01542                                                                   EL1279
01543                                                                   EL1279
01544  9200-RETURN-MAIN-MENU.                                           EL1279
01545      IF PI-CHANGES-MADE                                           EL1279
01546         MOVE -1                  TO FUNCTL                        EL1279
01547         MOVE SPACES              TO PFENTERO                      EL1279
01548         MOVE AL-UNNOF            TO PFENTERA                      EL1279
01549         MOVE ER-0045             TO EMI-ERROR                     EL1279
01550         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
01551         GO TO 8200-SEND-DATAONLY                                  EL1279
01552      END-IF.                                                      EL1279
01553                                                                   EL1279
01554      MOVE XCTL-126               TO PGM-NAME.                     EL1279
01555                                                                   EL1279
01556  9300-XCTL.                                                       EL1279
01557      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL1279
01558      EXEC CICS XCTL                                               EL1279
01559           PROGRAM  (PGM-NAME)                                     EL1279
01560           COMMAREA (PROGRAM-INTERFACE-BLOCK)                      EL1279
01561           LENGTH   (PI-COMM-LENGTH)                               EL1279
01562      END-EXEC.                                                    EL1279
01563                                                                   EL1279
01564  9400-CLEAR.                                                      EL1279
01565                                                                   EL1279
01566      IF PI-CHANGES-MADE                                           EL1279
01567          MOVE ER-0045            TO EMI-ERROR                     EL1279
01568          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL1279
01569          IF PI-CURRENT-LINE GREATER THAN ZERO                     EL1279
01570              PERFORM 7500-READ-TS THRU 7599-EXIT                  EL1279
01571              SET TB-INDX TO PI-CURRENT-LINE                       EL1279
01572              PERFORM 7100-FORMAT-SCREEN  THRU  7100-EXIT          EL1279
01573                  VARYING SC-INDX FROM 1 BY 1 UNTIL                EL1279
01574                  SC-INDX GREATER NUM-LINES-PER-SCREEN             EL1279
01575          END-IF                                                   EL1279
01576          GO TO 8100-SEND-INITIAL-MAP                              EL1279
01577      END-IF.                                                      EL1279
01578                                                                   EL1279
01579  9410-RETURN.                                                     EL1279
01580      IF PF5-PRESSED                                               EL1279
01581          MOVE PGM-EL1276           TO PGM-NAME                    EL1279
01582      ELSE                                                         EL1279
01583          IF PF6-PRESSED                                           EL1279
01584              IF PI-CLAIM-NOTE                                     EL1279
01585                  SET PI-CERT-NOTE  TO TRUE                        EL1279
01586              ELSE                                                 EL1279
01587                  SET PI-CLAIM-NOTE TO TRUE                        EL1279
01588              END-IF                                               EL1279
01589              SET PI-CHANGE-IN-NOTE-TYPE TO TRUE                   EL1279
01590              MOVE 'N'              TO PI-PF6-PRESSED              EL1279
01591              GO TO 1000-START                                     EL1279
01592          ELSE                                                     EL1279
01593              MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME                EL1279
01594          END-IF                                                   EL1279
01595      END-IF.                                                      EL1279
01596      GO TO 9300-XCTL.                                             EL1279
01597                                                                   EL1279
01598  9500-PF12.                                                       EL1279
01599      IF PI-CHANGES-MADE                                           EL1279
01600         MOVE -1                  TO FUNCTL                        EL1279
01601         MOVE SPACES              TO PFENTERO                      EL1279
01602         MOVE AL-UNNOF            TO PFENTERA                      EL1279
01603         MOVE ER-0045             TO EMI-ERROR                     EL1279
01604         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1279
01605         GO TO 8200-SEND-DATAONLY                                  EL1279
01606      END-IF.                                                      EL1279
01607                                                                   EL1279
01608      MOVE 'EL010'                TO PGM-NAME.                     EL1279
01609      GO TO 9300-XCTL.                                             EL1279
01610                                                                   EL1279
01611      EJECT                                                        EL1279
01612                                                                   EL1279
01613      EJECT                                                        EL1279
01614  9600-PGMID-ERROR.                                                EL1279
01615      EXEC CICS HANDLE CONDITION                                   EL1279
01616           PGMIDERR(8300-SEND-TEXT)                                EL1279
01617      END-EXEC.                                                    EL1279
01618                                                                   EL1279
01619      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL1279
01620      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL1279
01621      MOVE XCTL-005               TO  PGM-NAME.                    EL1279
01622      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL1279
01623      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL1279
01624                                                                   EL1279
01625      GO TO 9300-XCTL.                                             EL1279
01626                                                                   EL1279
01627  9700-LINK-DATE-CONVERT.                                          EL1279
01628                                                                   EL1279
01629      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL1279
01630      EXEC CICS LINK                                               EL1279
01631          PROGRAM    (PGM-NAME)                                    EL1279
01632          COMMAREA   (DATE-CONVERSION-DATA)                        EL1279
01633          LENGTH     (DC-COMM-LENGTH)                              EL1279
01634      END-EXEC.                                                    EL1279
01635                                                                   EL1279
01636  9700-EXIT.                                                       EL1279
01637      EXIT.                                                        EL1279
01638                                                                   EL1279
01639  9900-ERROR-FORMAT.                                               EL1279
01640      IF NOT EMI-ERRORS-COMPLETE                                   EL1279
01641         MOVE LINK-001            TO  PGM-NAME                     EL1279
01642         EXEC CICS LINK                                            EL1279
01643              PROGRAM(PGM-NAME)                                    EL1279
01644              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL1279
01645              LENGTH(EMI-COMM-LENGTH)                              EL1279
01646         END-EXEC                                                  EL1279
01647      END-IF.                                                      EL1279
01648  9900-EXIT.                                                       EL1279
01649      EXIT.                                                        EL1279
01650                                                                   EL1279
01651  9990-ABEND.                                                      EL1279
01652      MOVE LINK-004               TO  PGM-NAME.                    EL1279
01653      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL1279
01654                                                                   EL1279
01655      EXEC CICS LINK                                               EL1279
01656          PROGRAM   (PGM-NAME)                                     EL1279
01657          COMMAREA  (EMI-LINE1)                                    EL1279
01658          LENGTH    (72)                                           EL1279
01659      END-EXEC.                                                    EL1279
01660                                                                   EL1279
01661      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGBO.                    EL1279
01662      MOVE -1                     TO  FUNCTL.                      EL1279
01663                                                                   EL1279
01664      GO TO 8100-SEND-INITIAL-MAP.                                 EL1279
01665                                                                   EL1279
01666      GOBACK.                                                      EL1279
01667                                                                   EL1279
01668  9995-SECURITY-VIOLATION.                                         EL1279
01669                                  COPY ELCSCTP.                    EL1279
01670                                                                   EL1279
01671  9995-EXIT.                                                       EL1279
01672      EXIT.                                                        EL1279











