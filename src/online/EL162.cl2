00001  IDENTIFICATION DIVISION.                                         11/20/97
00002                                                                   EL162
00003  PROGRAM-ID.                 EL162 .                                 LV015
00004 *              PROGRAM CONVERTED BY                                  CL**9
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**9
00006 *              CONVERSION DATE 05/02/95 14:07:28.                    CL**9
00007 *                            VMOD=2.015                              CL*15
00008 *                                                                    CL*13
00009 *AUTHOR.     LOGIC,INC.                                              CL**9
00010 *            DALLAS, TEXAS.                                          CL**9
00011                                                                   EL162
00024 *REMARKS.    TRANSACTION - EX17 - RECORD MAIL RECEIVED               CL**4
00025 *            USED TO REVIEW PENDING MAIL AND TO RECORD               CL**4
00026 *            INCOMING MAIL.                                          CL**4
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
121406* 121406    2006081100001  PEMA  ADD RECEIVE DATE EDIT
051107* 051107    2006052500001  AJRA  POPULATE UBY WITH USER ID
100610* 100610    2009122800001  AJRA  BYPASS LETTER W/ RESEND PRINTED DT
102610* 102610    2009122800001  AJRA	 REPLACE VERIFY W/ STOP DATE
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
010416* 010416    2015072900002  TANA  ADD PF7 TO CLAIM MEMO SCREEN
062217* 062217    2017050300002  TANA  ADD AUTH RCVD FIELD
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022619* 022619  CR2019021100002  PEMA  ADD EDIT TO RECEIVE DATE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00027                                                                   EL162
00028      EJECT                                                        EL162
00029  ENVIRONMENT DIVISION.                                            EL162
00030  DATA DIVISION.                                                   EL162
00031  WORKING-STORAGE SECTION.                                         EL162
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL162
00033  77  FILLER  PIC X(32)  VALUE '*    EL162 WORKING STORAGE     *'. EL162
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.015 *********'.    CL*15
00035                                                                   EL162
00036                              COPY ELCSCTM.                           CL**8
00037                                                                   EL162
00038                              COPY ELCSCRTY.                          CL**8
00039                                                                   EL162
00040  01  WS-DATE-AREA.                                                EL162
00041      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL162
00042      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.               CL*12
00043      05  CURRENT-MINUS1-SAVE PIC XX      VALUE SPACES.               CL*12

022619 01  ws-response                 PIC S9(8)   COMP.
022619     88  resp-normal                  VALUE +00.
022619     88  resp-notfnd                  VALUE +13.
022619     88  resp-duprec                  value +14.
022619     88  resp-dupkey                  value +15.
022619     88  resp-notopen                 VALUE +19.
022619     88  resp-endfile                 VALUE +20.

00045  01  STANDARD-AREAS.                                              EL162
00046      12  SC-ITEM                 PIC S9(4)   VALUE +1  COMP.      EL162
00047      12  MAP-NAME.                                                EL162
00048          16  MAP-PRE             PIC XX      VALUE 'EL'.          EL162
00049          16  MAP-NUMBER          PIC X(4)    VALUE '162A'.        EL162
00050          16  FILLER              PIC XX      VALUE SPACES.        EL162
00051      12  MAPSET-NAME             PIC X(8)    VALUE 'EL162S'.      EL162
00052      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         EL162
00053      12  TRANS-ID                PIC X(4)    VALUE 'EX17'.        EL162
00054      12  PGM-NAME                PIC X(8).                        EL162
00055      12  TIME-IN                 PIC S9(7).                       EL162
00056      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL162
00057          16  FILLER              PIC X.                           EL162
00058          16  TIME-OUT            PIC 99V99.                       EL162
00059          16  FILLER              PIC XX.                             CL*12
00060      12  XCTL-005                PIC X(5)    VALUE 'EL005'.       EL162
00061      12  XCTL-010                PIC X(5)    VALUE 'EL010'.       EL162
00062      12  XCTL-126                PIC X(5)    VALUE 'EL126'.       EL162
00063      12  LINK-001                PIC X(5)    VALUE 'EL001'.       EL162
00064      12  LINK-004                PIC X(5)    VALUE 'EL004'.       EL162
00065      12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.        CL**9
00066      12  THIS-PGM                PIC X(8)    VALUE 'EL162'.       EL162
00067      12  PGM-EL141               PIC X(8)    VALUE 'EL141'.       EL162
00068      12  PGM-EL132               PIC X(8)    VALUE 'EL132'.       EL162
00069      12  PGM-EL150               PIC X(8)    VALUE 'EL150'.          CL**6
010416     12  PGM-EL1284              PIC X(8)    VALUE 'EL1284'.
00070      12  ELMSTR-FILE-ID          PIC X(8)    VALUE 'ELMSTR'.         CL**9
00071      12  ELTRLR-FILE-ID          PIC X(8)    VALUE 'ELTRLR'.         CL**9
00072      12  ERACCT-FILE-ID          PIC X(8)    VALUE 'ERACCT'.         CL**9
00073      12  SUB                     PIC S999    COMP-3.              EL162
00074      EJECT                                                        EL162
00075  01  ERROR-MESSAGES.                                              EL162
00076      12  ER-ZEROS                PIC X(4)    VALUE '0000'.        EL162
00077      12  ER-0004                 PIC X(4)    VALUE '0004'.        EL162
00078      12  ER-0008                 PIC X(4)    VALUE '0008'.        EL162
00079      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL162
00080      12  ER-0050                 PIC X(4)    VALUE '0050'.        EL162
00081      12  ER-0021                 PIC X(4)    VALUE '0021'.        EL162
00082      12  ER-0042                 PIC X(4)    VALUE '0042'.        EL162
00083      12  ER-0066                 PIC X(4)    VALUE '0066'.        EL162
00084      12  ER-0067                 PIC X(4)    VALUE '0067'.        EL162
00085      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL162
00086      12  ER-0133                 PIC X(4)    VALUE '0133'.        EL162
00087      12  ER-0168                 PIC X(4)    VALUE '0168'.        EL162
00088      12  ER-0172                 PIC X(4)    VALUE '0172'.        EL162
00089      12  ER-0179                 PIC X(4)    VALUE '0179'.        EL162
062217     12  ER-0287                 PIC X(4)    VALUE '0287'.        EL162
00090      12  ER-0318                 PIC X(4)    VALUE '0318'.        EL162
00091      12  ER-0319                 PIC X(4)    VALUE '0319'.        EL162
00092      12  ER-0320                 PIC X(4)    VALUE '0320'.        EL162
00093      12  ER-0321                 PIC X(4)    VALUE '0321'.        EL162
00094      12  ER-0322                 PIC X(4)    VALUE '0322'.        EL162
00095      12  ER-0514                 PIC X(4)    VALUE '0514'.        EL162
00096      12  ER-0539                 PIC X(4)    VALUE '0539'.        EL162
102610     12  ER-0895                 PIC X(4)    VALUE '0895'.
102610     12  ER-0896                 PIC X(4)    VALUE '0896'.
022619     12  er-1825                 pic x(4)    value '1825'.
00097      12  ER-7839                 PIC X(4)    VALUE '7839'.           CL*12
00098                                                                   EL162
00099      12  BIN-CURRENT-DATE        PIC XX.                          EL162
00100      12  INDX-WORK               PIC 9(4).                        EL162
00101      12  REC-DATA-SW             PIC X       VALUE ' '.           EL162
00102          88  REC-DATA                        VALUE '1'.           EL162
00103      12  VER-DATA-SW             PIC X       VALUE ' '.           EL162
00104          88  VER-DATA                        VALUE '1'.           EL162
00105      12  UNSOL-DATA-SW           PIC X       VALUE ' '.           EL162
00106          88  UNSOL-DATA                      VALUE '1'.           EL162
00107          88  FILE-DATA                       VALUE '2'.           EL162
00108      12  ADDR-TYPE               PIC X.                           EL162
00109      12  ADDR-SEQ                PIC S9(4) COMP.                  EL162
00110      12  ACTV-LENGTH             PIC S9(4) COMP VALUE +200.          CL**3
00111      12  USENT-SAVE              PIC XX    VALUE LOW-VALUES.      EL162
00112      12  URECDTE-SAVE            PIC XX    VALUE LOW-VALUES.      EL162
00113      12  REC-DATE-SAVE OCCURS 4 TIMES INDEXED BY INDX     PIC XX. EL162
102610     12  STOP-DATE-SAVE OCCURS 4 TIMES INDEXED BY INDX2   PIC XX.
00114      12  DEEDIT-FIELD            PIC X(15).                       EL162
00115      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15).    EL162
00116                                                                   EL162
00117      12  ACCT-KEY.                                                EL162
00118          16  ACCT-PARTIAL-KEY.                                    EL162
00119              20  ACCT-CO         PIC X.                              CL*12
00120              20  ACCT-CARRIER    PIC X.                              CL*12
00121              20  ACCT-GROUPING   PIC X(6).                           CL*12
00122              20  ACCT-STATE      PIC XX.                             CL*12
00123              20  ACCT-ACCOUNT    PIC X(10).                          CL*12
00124          16  ACCT-EXP-DATE       PIC XX.                             CL*12
00125      12  FILLER                  PIC S9(9)  VALUE +0 COMP.        EL162
00126      12  ACCT-SAVE-KEY           PIC X(20).                       EL162
00127      12  WS-ZIP.                                                     CL**7
00128          16  WS-ZIP-CODE         PIC X(5).                           CL**7
00129          16  WS-DASH             PIC X     VALUE '-'.                CL*12
00130          16  WS-ZIP-PLUS4        PIC X(4).                           CL**7
00131      12  WS-CANADIAN-POSTAL-CODES REDEFINES WS-ZIP.                  CL**7
00132          16  WS-CAN-POSTAL-CD-1  PIC X(3).                           CL**7
00133          16  WS-DASH-CAN         PIC X.                              CL*12
00134          16  WS-CAN-POSTAL-CD-2  PIC X(3).                           CL**7
00135          16  WS-CAN-FILLER       PIC X(3).                           CL**7
00136      12  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.          EL162
00137      12  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE                 EL162
00138                                  PIC 9(10).                       EL162
00139      12  WS-REASON               PIC X(70).                          CL*11
00140                                                                   EL162
00141      EJECT                                                           CL**8
00142                              COPY ELCDATE.                           CL**8
00143                                                                   EL162
00144      EJECT                                                        EL162
00145                              COPY ELCLOGOF.                          CL**8
00146                                                                   EL162
00147      EJECT                                                        EL162
00148                              COPY ELCATTR.                           CL**8
00149                                                                   EL162
00150      EJECT                                                        EL162
00151                              COPY ELCEMIB.                           CL**8
00152      EJECT                                                        EL162
00153                              COPY ELCINTF.                           CL**8
00154      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL162
00155          16  PI-NEXT-TRLR-SUB    PIC S999  COMP-3.                EL162
00156          16  PI-ACTV-SAVE        PIC X(20).                       EL162
00157          16  PI-ACTV-SAVE-SEQ    PIC S9(04) COMP.                    CL**6
00158          16  PI-ACTV-KEY.                                         EL162
00159            18  PI-ACTV-PARTIAL.                                   EL162
00160              20  PI-ACTV-COMP-CD     PIC X.                       EL162
00161              20  PI-ACTV-CARRIER     PIC X.                       EL162
00162              20  PI-ACTV-CLAIM       PIC X(7).                    EL162
00163              20  PI-ACTV-CERT-NO.                                 EL162
00164                  22  PI-ACTV-CERT-NO-PRIME PIC X(10).             EL162
00165                  22  PI-ACTV-CERT-NO-SUFX  PIC X.                 EL162
00166            18  PI-ACTV-SEQ           PIC S9(4) COMP.              EL162
00167                                                                   EL162
00168          16  PI-DISPLAYED-TRAILER-CODES.                          EL162
00169              18  PI-TRLRS OCCURS 50 TIMES    PIC S9(4) COMP.      EL162
00170          16  PI-CLAM-KEY.                                         EL162
00171              20  PI-CLAM-COMP-CD     PIC X.                       EL162
00172              20  PI-CLAM-CARRIER     PIC X.                       EL162
00173              20  PI-CLAM-CLAIM       PIC X(7).                    EL162
00174              20  PI-CLAM-CERT-NO.                                 EL162
00175                  24  PI-CLAM-CERT-NO-PRIME  PIC X(10).            EL162
00176                  24  PI-CLAM-CERT-NO-SUFX   PIC X.                EL162
00177          16  PI-DMD-FORCE-ERROR      PIC X.                          CL*15
00178              88  PI-DMD-FORCED             VALUE 'X'.
022619         16  pi-receive-dt-cnt       pic 9.
022619         16  pi-prev-rec-dt          pic xx.
022619         16  FILLER                  PIC X(470).                     CL*15
00180                                                                   EL162
00181      EJECT                                                        EL162
00182                              COPY ELCAID.                            CL**8
00183  01  FILLER    REDEFINES DFHAID.                                  EL162
00184      12  FILLER              PIC X(8).                            EL162
00185      12  PF-VALUES           PIC X       OCCURS 2.                EL162
00186                                                                   EL162
00187      EJECT                                                        EL162
00188                              COPY EL162S.                            CL**8
00189                                                                   EL162
00190  01  MAP-REDEF REDEFINES EL162AI.                                 EL162
00191      12  FILLER              PIC X(93).                           EL162
00192      12  TRL-LINES.                                               EL162
00193        14  TRAILER-LINES  OCCURS 4 TIMES INDEXED BY SC-INDX.      EL162
00194          16  SC-RECL         PIC S9(4)  COMP.                     EL162
00195          16  SC-RECA         PIC X.                               EL162
00196          16  SC-REC          PIC X(8).                            EL162
00197          16  SC-REC-O REDEFINES SC-REC PIC 99B99B99.              EL162
102610         16  SC-STOPL        PIC S9(4)  COMP.
102610         16  SC-STOPA        PIC X.
102610         16  SC-STOP         PIC X(8).
102610         16  SC-STOP-O REDEFINES SC-STOP PIC 99B99B99.
00201          16  FILLER          PIC X(3).                            EL162
00202          16  SC-SENT         PIC X(8).                            EL162
00203          16  FILLER          PIC X(3).                            EL162
00204          16  SC-BY           PIC X(4).                            EL162
00205          16  FILLER          PIC X(3).                            EL162
00206          16  SC-RESEND       PIC X(8).                            EL162
00207          16  FILLER          PIC X(3).                            EL162
00208          16  SC-FOLLOWUP     PIC X(8).                            EL162
00209          16  FILLER          PIC X(3).                            EL162
00210          16  SC-FORM         PIC X(4).                            EL162
00211          16  FILLER          PIC X(3).                            EL162
00212          16  SC-TO           PIC X(8).                            EL162
00213          16  FILLER          PIC X(3).                            EL162
00214          16  SC-ARCHIVE      PIC X(8).                            EL162
00215          16  FILLER          PIC X(3).                            EL162
00216          16  SC-TRLRNO       PIC 9(4).                            EL162
00217          16  FILLER          PIC X(3).                            EL162
00218          16  SC-TYPE         PIC X.                                  CL*12
00219          16  FILLER          PIC X(3).                               CL**6
00220          16  SC-REASON       PIC X(70).                           EL162
00221                                                                   EL162
00222      EJECT                                                        EL162
00223  LINKAGE SECTION.                                                 EL162
00224                                                                   EL162
00225  01  DFHCOMMAREA             PIC X(1024).                         EL162
00226                                                                   EL162
00227 *01 PARMLIST .                                                       CL**9
00228 *    02  FILLER              PIC S9(8)   COMP.                       CL**9
00229 *    02  CLAM-POINTER        PIC S9(8)   COMP.                       CL**9
00230 *    02  ACTV-POINTER        PIC S9(8)   COMP.                       CL**9
00231 *    02  ACCT-POINTER        PIC S9(8)   COMP.                       CL**9
00232                                                                   EL162
00233      EJECT                                                        EL162
00234                              COPY ELCMSTR.                           CL**8
00235                                                                      CL**8
00236      EJECT                                                           CL**8
00237                              COPY ELCTRLR.                           CL**8
00238                                                                   EL162
00239      EJECT                                                        EL162
00240                              COPY ERCACCT.                           CL**8
00241                                                                   EL162
00242      EJECT                                                        EL162
00243  PROCEDURE DIVISION.                                              EL162
00244                                                                   EL162
00245      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL162
00246      MOVE '5'                   TO DC-OPTION-CODE.                EL162
00247      PERFORM 9700-DATE-LINK.                                      EL162
00248      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL162
00249      MOVE DC-BIN-DATE-1         TO  BIN-CURRENT-DATE.                CL*13
00250                                                                      CL*12
00251      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                    CL*14
00252                                                                      CL*14
022619     MOVE -1                     TO DC-ELAPSED-MONTHS
022619     MOVE +0                     TO DC-ELAPSED-DAYS
022619     MOVE '6'                    TO DC-OPTION-CODE
022619     PERFORM 9700-DATE-LINK
022619     MOVE DC-BIN-DATE-2          TO CURRENT-MINUS1-SAVE
00259                                                                   EL162
00260      IF EIBCALEN NOT GREATER THAN ZERO                            EL162
00261          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL162
00262                                                                   EL162
00263      IF PI-CALLING-PROGRAM = PGM-EL150                               CL**6
00264          MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6             CL**6
00265          MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5             CL**6
00266          MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4             CL**6
00267          MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3             CL**6
00268          MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2             CL**6
00269          MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1             CL**6
00270          MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM           CL**6
00271          MOVE THIS-PGM             TO PI-CALLING-PROGRAM             CL**6
00272          MOVE +1                   TO SUB                            CL**6
00273          PERFORM  2000-ZERO-PI-TRLR-TABLE THRU 2010-EXIT             CL**6
00274          MOVE LOW-VALUES           TO EL162AO                        CL**6
00275                                       PI-DISPLAYED-TRAILER-CODES     CL*13
00276          MOVE 1                    TO PI-ACTV-SEQ                    CL**6
00277                                       PI-NEXT-TRLR-SUB               CL*13
00278          GO TO 7000-BUILD-SCREEN.                                    CL**6
00279                                                                      CL**6
00280      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL162
00281          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL162
00282 ************ FIRST TIME IN FROM MENU, FORCE THE CLAIM             EL162
00283 ************ TO BE DESIGNATED THRU EL132.                         EL162
00284              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL162
00285              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL162
00286              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL162
00287              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL162
00288              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL162
00289              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL162
00290              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL162
00291              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL162
00292              MOVE PGM-EL132            TO PGM-NAME                EL162
00293              GO TO 9300-XCTL                                      EL162
00294          ELSE                                                     EL162
00295              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL162
00296              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL162
00297              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL162
00298              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL162
00299              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL162
00300              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL162
00301              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL162
00302              MOVE SPACES               TO PI-SAVED-PROGRAM-6      EL162
00303              MOVE +1                   TO SUB                        CL*12
00304              PERFORM  2000-ZERO-PI-TRLR-TABLE THRU 2010-EXIT         CL**6
00305              MOVE LOW-VALUES           TO EL162AO                    CL*12
00306                                        PI-DISPLAYED-TRAILER-CODES    CL*13
00307              MOVE 1                    TO PI-ACTV-SEQ             EL162
00308                                           PI-NEXT-TRLR-SUB           CL*13
00309              GO TO 7000-BUILD-SCREEN.                                CL**2
00310                                                                   EL162
00311      IF EIBAID = DFHCLEAR                                            CL**2
00312          GO TO 9400-CLEAR.                                           CL**2
00313                                                                      CL**2
00314      EXEC CICS HANDLE CONDITION                                   EL162
00315          PGMIDERR(9600-PGMID-ERROR)                               EL162
00316          ERROR   (9990-ABEND)                                        CL*12
00317      END-EXEC.                                                       CL*12
00318                                                                   EL162
00319      IF PI-PROCESSOR-ID = 'LGXX'                                  EL162
00320          GO TO 0200-RECEIVE.                                      EL162
00321                                                                   EL162
00322      EXEC CICS READQ TS                                           EL162
00323          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL162
00324          INTO    (SECURITY-CONTROL)                               EL162
00325          LENGTH  (SC-COMM-LENGTH)                                 EL162
00326          ITEM    (SC-ITEM)                                        EL162
00327      END-EXEC.                                                    EL162
00328                                                                   EL162
00329      MOVE SC-CLAIMS-DISPLAY (2)    TO  PI-DISPLAY-CAP.            EL162
00330      MOVE SC-CLAIMS-UPDATE  (2)    TO  PI-MODIFY-CAP.             EL162
00331                                                                   EL162
00332      IF NOT MODIFY-CAP                                            EL162
00333          MOVE 'UPDATE'             TO  SM-READ                    EL162
00334          PERFORM 9995-SECURITY-VIOLATION                          EL162
00335          MOVE ER-0070              TO  EMI-ERROR                  EL162
00336          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL162
00337          GO TO 8100-SEND-INITIAL-MAP.                             EL162
00338                                                                   EL162
00339      EJECT                                                        EL162
00340  0200-RECEIVE.                                                    EL162
00341                                                                   EL162
00342      MOVE LOW-VALUES TO EL162AI.                                  EL162
00343                                                                   EL162
00344      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL162
00345          MOVE ER-0008            TO EMI-ERROR                     EL162
00346          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL162
00347          GO TO 8200-SEND-DATAONLY.                                EL162
00348                                                                   EL162
00349      EXEC CICS RECEIVE                                            EL162
00350          MAP   (MAP-NAME)                                            CL*12
00351          MAPSET(MAPSET-NAME)                                      EL162
00352          INTO  (EL162AI)                                             CL*12
00353      END-EXEC.                                                       CL*12
00354                                                                   EL162
00355      IF ENTERPFL = ZERO                                           EL162
00356          GO TO 0300-CHECK-PFKEYS.                                 EL162
00357                                                                   EL162
00358      IF EIBAID NOT = DFHENTER                                     EL162
00359          MOVE ER-0004            TO EMI-ERROR                     EL162
00360          GO TO 0320-INPUT-ERROR.                                  EL162
00361                                                                   EL162
00362      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)      CL**4
00363          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      EL162
00364      ELSE                                                         EL162
00365          MOVE ER-0029            TO EMI-ERROR                     EL162
00366          GO TO 0320-INPUT-ERROR.                                  EL162
00367                                                                   EL162
00368  0300-CHECK-PFKEYS.                                               EL162
00369      IF EIBAID = DFHPF23                                          EL162
00370          GO TO 8810-PF23.                                         EL162
00371                                                                   EL162
00372      IF EIBAID = DFHPF24                                          EL162
00373          GO TO 9200-RETURN-MAIN-MENU.                             EL162
00374                                                                   EL162
00375      IF EIBAID = DFHPF12                                          EL162
00376          GO TO 9500-PF12.                                         EL162
00377                                                                   EL162
00378      IF PI-COMPANY-ID = 'DMD'                                        CL*15
00379          MOVE SPACE              TO PI-DMD-FORCE-ERROR.              CL*15
00380                                                                      CL*15
00381      IF EIBAID = DFHPF4                                              CL*15
00382          IF PI-COMPANY-ID NOT = 'DMD'                                CL*15
00383              MOVE ER-0029        TO EMI-ERROR                        CL*15
00384              GO TO 0320-INPUT-ERROR                                  CL*15
00385          ELSE                                                        CL*15
00386              MOVE 'X'            TO PI-DMD-FORCE-ERROR               CL*15
00387              MOVE DFHENTER       TO EIBAID                           CL*15
00388              GO TO 0330-EDIT-DATA.                                   CL*15
00389                                                                      CL*15
00390      IF EIBAID = DFHPF5                                              CL*12
00391          IF PI-RETURN-TO-PROGRAM = 'EL150'                           CL*12
00392              MOVE ER-0029        TO EMI-ERROR                        CL*10
00393              GO TO 0320-INPUT-ERROR                                  CL*10
00394          ELSE                                                        CL*10
00395              MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA             CL*10
00396              MOVE PGM-EL132      TO PGM-NAME                         CL*10
00397              GO TO 9300-XCTL.                                        CL*10
00398                                                                   EL162
00399      IF PI-CLAIM-NO = SPACES                                      EL162
00400         MOVE ER-0319             TO EMI-ERROR                     EL162
00401         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL162
00402         GO TO 8100-SEND-INITIAL-MAP.                              EL162
00403                                                                   EL162
102610     IF SC-RECL (1) > 0 OR SC-STOPL (1) > 0 OR
102610        SC-RECL (2) > 0 OR SC-STOPL (2) > 0 OR
102610        SC-RECL (3) > 0 OR SC-STOPL (3) > 0 OR
102620        SC-RECL (4) > 0 OR SC-STOPL (4) > 0 OR
00408         UREL > 0  OR USENTL > 0  OR
051107        UFORML > 0  OR URECDTEL > 0  
00410           IF EIBAID NOT = DFHENTER                                   CL*13
00411              MOVE ER-0050            TO EMI-ERROR                    CL*13
00412              GO TO 0320-INPUT-ERROR.                                 CL*13
00413                                                                   EL162
00414      IF EIBAID = DFHPF1                                           EL162
00415          GO TO 6000-BROWSE-FORWARD.                               EL162
00416                                                                   EL162
00417      IF EIBAID = DFHPF2                                           EL162
00418          GO TO 6100-BROWSE-BACKWARD.                              EL162
00419                                                                   EL162
00420      IF EIBAID = DFHPF6                                           EL162
00421          MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA          EL162
00422          MOVE PGM-EL141          TO PGM-NAME                      EL162
00423          GO TO 9300-XCTL.                                         EL162
00419                                                                   EL162
010416     IF EIBAID = DFHPF7                                           EL162
010416         MOVE PGM-EL1284          TO PGM-NAME                     EL162
010416         GO TO 9300-XCTL.                                         EL162
00424                                                                   EL162
00425      IF EIBAID = DFHENTER                                         EL162
00426          GO TO 0330-EDIT-DATA.                                    EL162
00427                                                                   EL162
00428      MOVE ER-0029                TO EMI-ERROR.                    EL162
00429                                                                   EL162
00430  0320-INPUT-ERROR.                                                EL162
00431      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL162
00432                                                                   EL162
00433      MOVE AL-UNBON        TO ENTERPFA.                               CL*13
00434                                                                   EL162
00435      IF ENTERPFL = 0                                              EL162
00436          MOVE -1          TO URECDTEL                                CL*13
00437      ELSE                                                         EL162
00438          MOVE -1          TO ENTERPFL.                               CL*13
00439                                                                   EL162
00440      GO TO 8200-SEND-DATAONLY.                                    EL162
00441                                                                   EL162
00442      EJECT                                                        EL162
00443  0330-EDIT-DATA.                                                  EL162
00444      SET SC-INDX  INDX           TO 1.                            EL162
102610     SET INDX2                   TO 1.

022619     perform 3000-BUILD-KEYS
022619     EXEC CICS READ
022619        DATASET (ELMSTR-FILE-ID)
022619        RIDFLD  (PI-CLAM-KEY)
022619        SET     (ADDRESS OF CLAIM-MASTER)
022619        resp    (ws-response)
022619     END-EXEC
022619
022619     if not resp-normal
022619        go to 8860-CLAIM-NOT-FOUND
022619     end-if

022619     .
00446  0331-LOOP.                                                       EL162

00447      IF SC-RECL (SC-INDX) > ZERO
00448         MOVE SC-REC (SC-INDX)    TO DEEDIT-FIELD
00449         PERFORM 8600-DEEDIT
00450         MOVE '1'                 TO REC-DATA-SW
00451         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
00452         MOVE '4'                 TO DC-OPTION-CODE
00453         PERFORM 9700-DATE-LINK
00454         IF DATE-CONVERSION-ERROR
00455            MOVE ER-0021          TO EMI-ERROR
00456            MOVE -1               TO SC-RECL (SC-INDX)
00457            MOVE AL-UABON         TO SC-RECA (SC-INDX)
00458            PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
00459         ELSE
121406           IF DC-BIN-DATE-1 > BIN-CURRENT-DATE
121406              MOVE ER-0539       TO EMI-ERROR
121406              MOVE -1            TO SC-RECL (SC-INDX)
121406              MOVE AL-UABON      TO SC-RECA (SC-INDX)
121406              PERFORM 9900-ERROR-FORMAT
121406                                 THRU 9900-EXIT
                 ELSE
00460               MOVE DC-BIN-DATE-1 TO REC-DATE-SAVE (INDX)
00461               MOVE DEEDIT-FIELD-V0
                                       TO SC-REC-O (SC-INDX)
00462               INSPECT SC-REC (SC-INDX) CONVERTING ' ' TO '/'
121406           END-IF
121406        END-IF
00463      ELSE
00464         MOVE LOW-VALUES          TO REC-DATE-SAVE (INDX)
121406     END-IF

00466      IF PI-COMPANY-ID = 'DMD'                                        CL*13
00467        IF SC-RECL (SC-INDX) > 0                                      CL*13
00468          IF REC-DATE-SAVE (INDX) < CURRENT-MINUS1-SAVE               CL*13
00469          IF NOT PI-DMD-FORCED                                        CL*15
00470            MOVE ER-7839        TO EMI-ERROR                          CL*13
00471            MOVE -1             TO SC-RECL (SC-INDX)                  CL*13
00472            MOVE AL-UABON       TO SC-RECA (SC-INDX)                  CL*13
00473            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  CL*13
00474          END-IF                                                      CL*15
00475          ELSE                                                        CL*13
00476          IF REC-DATE-SAVE (INDX) > BIN-CURRENT-DATE                  CL*13
00477            MOVE ER-0539          TO EMI-ERROR                        CL*13
00478            MOVE -1               TO SC-RECL (SC-INDX)                CL*13
00479            MOVE AL-UABON         TO SC-RECA (SC-INDX)                CL*13
00480            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*13
00481                                                                      CL*13
102610     IF SC-STOPL (SC-INDX) > ZERO
102610        MOVE SC-STOP (SC-INDX)    TO DEEDIT-FIELD
102610        PERFORM 8600-DEEDIT
102610        MOVE '1'                 TO REC-DATA-SW
102610        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
102610        MOVE '4'                 TO DC-OPTION-CODE
102610        PERFORM 9700-DATE-LINK
102610        IF DATE-CONVERSION-ERROR
102610           MOVE ER-0021          TO EMI-ERROR
102610           MOVE -1               TO SC-STOPL (SC-INDX)
102610           MOVE AL-UABON         TO SC-STOPA (SC-INDX)
102610           PERFORM 9900-ERROR-FORMAT
102610                                 THRU 9900-EXIT
102610        ELSE
102610           IF DC-BIN-DATE-1 > BIN-CURRENT-DATE
102610              MOVE ER-0895       TO EMI-ERROR
102610              MOVE -1            TO SC-STOPL (SC-INDX)
102610              MOVE AL-UABON      TO SC-STOPA (SC-INDX)
102610              PERFORM 9900-ERROR-FORMAT
102610                                 THRU 9900-EXIT
102610           ELSE
102610              MOVE DC-BIN-DATE-1 TO STOP-DATE-SAVE (INDX2)
102610              MOVE DEEDIT-FIELD-V0 TO SC-STOP-O (SC-INDX)
102610              INSPECT SC-STOP (SC-INDX) CONVERTING ' ' TO '/'
102610           END-IF
102610        END-IF
102610     ELSE
102610        MOVE LOW-VALUES          TO STOP-DATE-SAVE (INDX2)
102610     END-IF.
00493                                                                   EL162
00494      SET SC-INDX                                                     CL*12
102610            INDX2
00495             INDX UP BY 1.                                            CL*12
00496                                                                   EL162
00497      IF SC-INDX NOT = 5                                           EL162
00498         GO TO 0331-LOOP.                                          EL162

00502      IF URECDTEL > 0                                                 CL*13
00503         MOVE URECDTEI            TO DEEDIT-FIELD                  EL162
00504         PERFORM 8600-DEEDIT                                       EL162
00505         MOVE '1'                 TO UNSOL-DATA-SW                 EL162
00506         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL162
00507         MOVE '4'                 TO DC-OPTION-CODE                EL162
00508         PERFORM 9700-DATE-LINK                                    EL162
00509         IF DATE-CONVERSION-ERROR                                  EL162
00510            MOVE ER-0021          TO EMI-ERROR                     EL162
00511            MOVE -1               TO URECDTEL                      EL162
00512            MOVE AL-UABON         TO URECDTEA                      EL162
00513            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL162
00514         ELSE                                                      EL162
00515            MOVE DC-BIN-DATE-1    TO URECDTE-SAVE                     CL*13
00516            MOVE DEEDIT-FIELD-V0  TO URECDTEO                         CL*13
00517            INSPECT URECDTEI CONVERTING ' ' TO '/'
022619           if (urecdte-save < cl-incurred-dt)
022619              or (urecdte-save < cl-reported-dt)
022619              or (urecdte-save < cl-file-establish-dt)
022619              or (urecdte-save < cl-cert-eff-dt)
022619              or (urecdte-save < CURRENT-MINUS1-SAVE)
022619              if (pi-receive-dt-cnt > 0)
022619                 and (urecdte-save = pi-prev-rec-dt)
022619                 move 0          to pi-receive-dt-cnt
022619                 move low-values to pi-prev-rec-dt
022619              else
022619                 move 1          to pi-receive-dt-cnt
022619                 move urecdte-save
022619                                 to pi-prev-rec-dt
022619                 move er-1825    to emi-error
022619                 move -1         to urecdtel
022619                 move al-uabon   to urecdtea
022619                 perform 9900-error-format
022619                                 thru 9900-exit
022619              end-if
022619           end-if
022619        end-if
022619     end-if

00519      IF PI-COMPANY-ID = 'DMD'                                        CL*13
00520        IF URECDTEL > 0                                               CL*13
00521          IF URECDTE-SAVE < CURRENT-MINUS1-SAVE                       CL*13
00522          IF NOT PI-DMD-FORCED                                        CL*15
00523            MOVE ER-7839        TO EMI-ERROR                          CL*13
00524            MOVE -1             TO URECDTEL                           CL*13
00525            MOVE AL-UABON       TO URECDTEA                           CL*13
00526            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  CL*13
00527          END-IF                                                      CL*15
00528          ELSE                                                        CL*13
00529          IF URECDTE-SAVE > BIN-CURRENT-DATE                          CL*13
00530            MOVE ER-0539          TO EMI-ERROR                        CL*13
00531            MOVE -1               TO URECDTEL                         CL*13
00532            MOVE AL-UABON         TO URECDTEA                         CL*13
00533            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*13
00534                                                                      CL*13
00535      IF USENTL > 0                                                   CL*13
00536         MOVE USENTI              TO DEEDIT-FIELD                  EL162
00537         PERFORM 8600-DEEDIT                                       EL162
00538         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL162
00539         MOVE '4'                 TO DC-OPTION-CODE                EL162
00540         PERFORM 9700-DATE-LINK                                    EL162
00541         IF DATE-CONVERSION-ERROR                                  EL162
00542            MOVE ER-0021          TO EMI-ERROR                     EL162
00543            MOVE -1               TO USENTL                        EL162
00544            MOVE AL-UABON         TO USENTA                        EL162
00545            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL162
00546           ELSE                                                    EL162
00547            MOVE DC-BIN-DATE-1    TO USENT-SAVE                    EL162
00548            MOVE DEEDIT-FIELD-V0  TO USENTO                        EL162
00549            INSPECT USENTI CONVERTING ' ' TO '/'                      CL**9
00550         ELSE                                                      EL162
00551            MOVE LOW-VALUES       TO USENT-SAVE.                      CL*13
00552                                                                   EL162
00553      IF USENT-SAVE   NOT = LOW-VALUES AND                         EL162
00554         URECDTE-SAVE NOT = LOW-VALUES                             EL162
00555           IF USENT-SAVE GREATER THAN URECDTE-SAVE                 EL162
00556              MOVE ER-0514        TO EMI-ERROR                     EL162
00557              MOVE -1             TO URECDTEL                      EL162
00558              MOVE AL-UABON       TO URECDTEA                      EL162
00559              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL162
00560                                                                   EL162
00561      IF URECDTE-SAVE NOT = LOW-VALUES                             EL162
00562           IF URECDTE-SAVE GREATER THAN BIN-CURRENT-DATE           EL162
00563              MOVE ER-0539        TO EMI-ERROR                     EL162
00564              MOVE -1             TO URECDTEL                      EL162
00565              MOVE AL-UABON       TO URECDTEA                      EL162
00566              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL162
00567                                                                   EL162
00568      IF URECDTEL = ZEROS                                             CL*13
00569         IF USENTL > ZEROS  OR                                        CL*13
051107*00570            UBYL   > ZEROS  OR                                        CL*13
00571            UFORML > ZEROS  OR                                        CL*13
00572            UREL   > ZEROS                                            CL*13
00573               MOVE ER-0321          TO EMI-ERROR                     CL*13
00574               MOVE -1               TO URECDTEL                      CL*13
00575               PERFORM 9900-ERROR-FORMAT THRU  9900-EXIT.             CL*13

062217     IF AUTHRCVL > 0
062217        IF AUTHRCVI = 'N' OR 'Y'
062217           CONTINUE
062217        ELSE
062217           MOVE ER-0287        TO EMI-ERROR
062217           MOVE -1             TO AUTHRCVL
062217           MOVE AL-UABON       TO AUTHRCVA
062217           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062217        END-IF
062217     END-IF.

00576                                                                   EL162
00577      IF NOT EMI-NO-ERRORS                                         EL162
00578         GO TO 8200-SEND-DATAONLY.                                 EL162
00579                                                                   EL162
00580      IF NOT MODIFY-CAP                                            EL162
00581          MOVE ER-0070            TO EMI-ERROR                     EL162
00582          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL162
00583          MOVE LOW-VALUES TO EL162AO                               EL162
00584          GO TO 8100-SEND-INITIAL-MAP.                             EL162
00585                                                                   EL162
00586      EJECT                                                        EL162
00587      PERFORM 3000-BUILD-KEYS.                                     EL162
00588                                                                   EL162
00589      IF REC-DATA                                                  EL162
00590         PERFORM 4000-UPDATE-RECEIVE-DATES THRU 4099-EXIT          EL162
00591                 VARYING SC-INDX FROM 1 BY 1 UNTIL                 EL162
00592                 SC-INDX = 5.                                      EL162
00593                                                                   EL162
00594      PERFORM 4100-CREATE-NEW-TRLR THRU 4199-EXIT.                    CL*12
00600                                                                   EL162
00601      MOVE ER-ZEROS               TO EMI-ERROR.                    EL162
00602      MOVE SPACES                 TO ADDR1I ADDR2I ADDR3I ADDR4I      CL*12
00603                                     ZIPI PHONEI.                     CL*12
00604                                                                   EL162
00605      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL162
00606                                                                   EL162
00607      MOVE -1                     TO SC-RECL (1).                  EL162
00608      GO TO 8200-SEND-DATAONLY.                                    EL162
00609                                                                      CL*13
00610      EJECT                                                        EL162
00611  2000-ZERO-PI-TRLR-TABLE.                                            CL**6
00612                                                                      CL**6
00613      MOVE +0                     TO PI-TRLRS (SUB).                  CL*12
00614      ADD +1 TO SUB.                                                  CL*12
00615                                                                      CL**6
00616      IF SUB > +50                                                    CL*13
00617         MOVE +0                  TO SUB                              CL*12
022619        move 0                   to pi-receive-dt-cnt
022619        move low-values          to pi-prev-rec-dt
00618         GO TO 2010-EXIT.                                             CL**6
00619                                                                      CL**6
00620      GO TO 2000-ZERO-PI-TRLR-TABLE.                                  CL**6
00621                                                                      CL**6
00622  2010-EXIT.                                                          CL**6
00623      EXIT.                                                           CL**6

022619 2200-edit-receive-date.
022619***  The ELMSTR has been read at this point and each of the
022619***  received dates will be edited here.
022619
022619     move sc-trlrno(sc-indx)     to pi-actv-seq
022619
022619     EXEC CICS READ
022619        DATASET (ELTRLR-FILE-ID)
022619        RIDFLD  (PI-ACTV-KEY)
022619        SET     (ADDRESS OF ACTIVITY-TRAILERS)
022619        resp    (ws-response)
022619     END-EXEC
022619
022619     if not resp-normal
022619        display 'error eltrlr read ' ws-response
022619        go to 2200-exit
022619     end-if
022619     if not correspondence-tr
022619        display ' is not corres trailer '
022619        go to 2200-exit
022619     end-if
022619
022619     .
022619 2200-exit.
022619     exit.

00625  3000-BUILD-KEYS.                                                 EL162
00626      MOVE PI-CLAIM-NO            TO PI-CLAM-CLAIM                 EL162
00627                                     PI-ACTV-CLAIM                 EL162
00628                                     CLAIMNOI.                     EL162
00629                                                                   EL162
00630      MOVE PI-CERT-NO             TO PI-CLAM-CERT-NO               EL162
00631                                     PI-ACTV-CERT-NO               EL162
00632                                     CERTNOI.                      EL162
00633                                                                   EL162
00634      MOVE PI-COMPANY-CD          TO PI-CLAM-COMP-CD               EL162
00635                                     PI-ACTV-COMP-CD.              EL162
00636                                                                   EL162
00637      MOVE PI-CARRIER             TO PI-CLAM-CARRIER               EL162
00638                                     PI-ACTV-CARRIER               EL162
00639                                     CARRI.                        EL162
00640      EJECT                                                        EL162
00641  4000-UPDATE-RECEIVE-DATES.                                       EL162
00642                                                                      CL**6
102610     IF SC-RECL (SC-INDX) = ZERO AND SC-STOPL (SC-INDX)
00644         GO TO 4099-EXIT.                                          EL162
00645                                                                   EL162
00646      MOVE SC-TRLRNO (SC-INDX)    TO PI-ACTV-SEQ.                  EL162
00647                                                                   EL162
102610*00648      IF SC-RECL (SC-INDX) = ZERO                                  EL162
102610*00649          EXEC CICS READ                                           EL162
102610*00650               DATASET (ELTRLR-FILE-ID)                               CL*12
102610*00651               RIDFLD  (PI-ACTV-KEY)                                  CL*12
102610*00652               SET     (ADDRESS OF ACTIVITY-TRAILERS)                 CL*12
102610*00653          END-EXEC                                                    CL*12
102610*00654         ELSE                                                      EL162
00655          EXEC CICS READ                                           EL162
00656               DATASET(ELTRLR-FILE-ID)                                CL**9
00657               RIDFLD (PI-ACTV-KEY)                                   CL*12
00658               SET    (ADDRESS OF ACTIVITY-TRAILERS)                  CL*12
00659               UPDATE                                              EL162
00660          END-EXEC.                                                   CL*12
00661                                                                   EL162
102610     SET INDX2 TO SC-INDX.
102610
102610     IF SC-STOPL (SC-INDX) = ZEROS
102610         GO TO 4000-BYPASS-STOP-DATE
102610     END-IF.
102610
102610     IF (CORRESPONDENCE-TR AND
102610         STOP-DATE-SAVE (INDX2) LESS THAN AT-LETTER-SENT-DT)
102610              OR
102610        (FORM-CONTROL-TR AND
102610         STOP-DATE-SAVE (INDX2) LESS THAN AT-FORM-SEND-ON-DT)
102610           MOVE ER-0896          TO EMI-ERROR
102610           MOVE -1               TO SC-STOPL (SC-INDX)
102610           MOVE AL-UABON         TO SC-STOPA (SC-INDX)
102610           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
102610           EXEC CICS SYNCPOINT
102610                ROLLBACK
102610           END-EXEC
102610           GO TO 8200-SEND-DATAONLY
102610     END-IF.
102610
102610     IF CORRESPONDENCE-TR
102610        MOVE STOP-DATE-SAVE (INDX2) TO AT-STOP-LETTER-DT
102610        MOVE PI-PROCESSOR-ID     TO AT-CORR-LAST-UPDATED-BY
102610        MOVE BIN-CURRENT-DATE    TO AT-CORR-LAST-MAINT-DT
102610     END-IF.
102610
102610     IF FORM-CONTROL-TR
102610        MOVE STOP-DATE-SAVE (INDX2) TO AT-STOP-FORM-DT
102610        MOVE PI-PROCESSOR-ID     TO AT-FORM-LAST-UPDATED-BY
102610        MOVE BIN-CURRENT-DATE    TO AT-FORM-LAST-MAINT-DT
102610     END-IF.
102610     MOVE AL-UANOF               TO SC-STOPA (SC-INDX).
102610
102610 4000-BYPASS-STOP-DATE.
00671                                                                   EL162
00672      IF SC-RECL (SC-INDX) = ZEROS                                 EL162
102610*00673         GO TO 4099-EXIT.                                          EL162
102610        GO TO 4090-REWRITE
102610     END-IF.
00674                                                                   EL162
00675      SET INDX TO SC-INDX.                                         EL162
00676                                                                   EL162
00677      IF (CORRESPONDENCE-TR AND                                    EL162
00678          REC-DATE-SAVE (INDX)  LESS THAN AT-LETTER-SENT-DT)       EL162
00679               OR                                                     CL*12
00680         (FORM-CONTROL-TR AND                                         CL*12
00681          REC-DATE-SAVE (INDX)  LESS THAN AT-FORM-SEND-ON-DT)      EL162
00682            MOVE ER-0514          TO EMI-ERROR                     EL162
00683            MOVE -1               TO SC-RECL (SC-INDX)             EL162
00684            MOVE AL-UABON         TO SC-RECA (SC-INDX)             EL162
00685            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL162
00686            EXEC CICS SYNCPOINT                                    EL162
00687                 ROLLBACK                                          EL162
00688            END-EXEC                                                  CL*12
00689            GO TO 8200-SEND-DATAONLY.                              EL162
00690                                                                   EL162
00691      IF CORRESPONDENCE-TR                                         EL162
00692         MOVE REC-DATE-SAVE (INDX)   TO AT-LETTER-ANSWERED-DT         CL**6
00693         MOVE PI-PROCESSOR-ID        TO AT-CORR-LAST-UPDATED-BY       CL**6
00694         MOVE BIN-CURRENT-DATE       TO AT-CORR-LAST-MAINT-DT.        CL*13
00695                                                                   EL162
00696      IF FORM-CONTROL-TR                                           EL162
00697         MOVE PI-PROCESSOR-ID        TO AT-FORM-LAST-UPDATED-BY       CL**6
00698         MOVE BIN-CURRENT-DATE       TO AT-FORM-LAST-MAINT-DT         CL*13
00699         IF SC-TYPE (SC-INDX) = 'C'                                   CL*13
00700            MOVE REC-DATE-SAVE (INDX)   TO AT-FORM-ANSWERED-DT        CL**6
00701         ELSE                                                         CL**6
00702         IF SC-TYPE (SC-INDX) = 'E'                                   CL*13
00703            MOVE REC-DATE-SAVE (INDX)   TO AT-EMP-FORM-ANSWERED-DT    CL**6
00704         ELSE                                                         CL**6
00705         IF SC-TYPE (SC-INDX) = 'P'                                   CL*13
00706            MOVE REC-DATE-SAVE (INDX)   TO AT-PHY-FORM-ANSWERED-DT.   CL**6
00707                                                                   EL162
00708      MOVE AL-UANOF               TO SC-RECA (SC-INDX).            EL162
00709                                                                   EL162
102610 4090-REWRITE.
102610
00710      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         EL162
00711                                                                   EL162
00712      IF PI-COMPANY-ID = 'DMD'                                        CL*12
00713          MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY.                  CL*12
00714                                                                      CL*12
00715      EXEC CICS REWRITE                                            EL162
00716           FROM   (ACTIVITY-TRAILERS)                                 CL*12
00717           DATASET(ELTRLR-FILE-ID)                                    CL**9
00718      END-EXEC.                                                       CL*12
00719                                                                   EL162
00720  4099-EXIT.                                                       EL162
00721       EXIT.                                                       EL162
00722                                                                   EL162
00723      EJECT                                                        EL162
00724  4100-CREATE-NEW-TRLR.                                            EL162
00725                                                                      CL**6
00726      EXEC CICS READ                                               EL162
00727           DATASET(ELMSTR-FILE-ID)                                    CL**9
00728           SET    (ADDRESS OF CLAIM-MASTER)                           CL**9
00729           RIDFLD (PI-CLAM-KEY)                                    EL162
00730           UPDATE                                                  EL162
00731      END-EXEC.                                                       CL*12
00732                                                                   EL162
00733      IF FILEL > ZEROS                                                CL*13
00734         MOVE FILEI               TO CL-FILE-LOCATION.             EL162
00735                                                                   EL162
00736      IF NOT UNSOL-DATA                                            EL162
00737         GO TO 4150-REWRITE-CLAIM.                                 EL162
00738                                                                   EL162
00739      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.                          EL162
00740                                                                   EL162
00741      EXEC CICS GETMAIN                                            EL162
00742           SET    (ADDRESS OF ACTIVITY-TRAILERS)                      CL*12
00743           INITIMG(GETMAIN-SPACE)                                  EL162
00744           LENGTH (ACTV-LENGTH)                                       CL*12
00745      END-EXEC.                                                       CL*12
00746                                                                   EL162
00747      MOVE 'AT'                   TO AT-RECORD-ID.                 EL162
00748      MOVE  4                     TO AT-TRAILER-TYPE.              EL162
00749      MOVE BIN-CURRENT-DATE       TO AT-RECORDED-DT                   CL**6
00750                                     AT-CORR-LAST-MAINT-DT            CL**6
00751                                                                   EL162
00752      IF UBYL > +0                                                    CL*13
00753         MOVE UBYI                TO AT-RECORDED-BY                EL162
00754                                     AT-CORR-LAST-UPDATED-BY          CL**6
00755      ELSE                                                            CL**6
00756         MOVE PI-PROCESSOR-ID     TO AT-RECORDED-BY                   CL**6
00757                                     AT-CORR-LAST-UPDATED-BY.         CL**6
00758                                                                   EL162
00759      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         EL162
00760      MOVE PI-ACTV-KEY            TO AT-CONTROL-PRIMARY.           EL162
00761      MOVE PI-CLAIM-NO            TO AT-CLAIM-NO.                  EL162
00762      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.               EL162
00763      MOVE USENT-SAVE             TO AT-LETTER-SENT-DT.            EL162
00764      MOVE LOW-VALUES             TO AT-RECEIPT-FOLLOW-UP             CL*12
00765                                     AT-AUTO-RE-SEND-DT               CL*12
00766                                     AT-INITIAL-PRINT-DATE            CL*12
00767                                     AT-RESEND-PRINT-DATE.            CL*12
00768      MOVE URECDTE-SAVE           TO AT-LETTER-ANSWERED-DT.        EL162
00769      MOVE ZEROS                  TO AT-LETTER-ARCHIVE-NO.         EL162
00770      MOVE '1'                    TO AT-LETTER-ORIGIN.             EL162
00771                                                                   EL162
00772      IF UFORML > 0                                                   CL*13
00773         MOVE UFORMI              TO AT-STD-LETTER-FORM            EL162
00774        ELSE                                                       EL162
00775         MOVE SPACES              TO AT-STD-LETTER-FORM.           EL162
00776                                                                   EL162
00777      IF UREL > 0                                                     CL*13
00778         MOVE UREI                TO AT-REASON-TEXT                EL162
00779        ELSE                                                       EL162
00780         MOVE SPACES              TO AT-REASON-TEXT.               EL162

062217     IF AUTHRCVL > 0
062217        MOVE AUTHRCVI            TO AT-AUTH-RCVD
062217     ELSE
062217        MOVE SPACES              TO AT-AUTH-RCVD
062217     END-IF.

00781                                                                   EL162
00782      MOVE SPACES                 TO AT-ADDRESSEE-NAME                CL*13
00783                                     AT-ADDRESEE-TYPE.                CL*13
00784      MOVE ZEROS                  TO AT-ADDRESS-REC-SEQ-NO.        EL162
00785      MOVE 'U'                    TO AT-CORR-SOL-UNSOL.               CL**3
00786                                                                   EL162
00787      EXEC CICS WRITE                                              EL162
00788           DATASET(ELTRLR-FILE-ID)                                    CL**9
00789           FROM   (ACTIVITY-TRAILERS)                                 CL*12
00790           RIDFLD (AT-CONTROL-PRIMARY)                                CL*12
00791      END-EXEC.                                                       CL*12
00792                                                                   EL162
00793      MOVE SPACES                 TO URECDTEI                      EL162
00794                                     USENTI                        EL162
051107*00795                                     UBYI                          EL162
00796                                     UFORMI                        EL162
00797                                     UREI.                         EL162
00798                                                                   EL162
00799      MOVE AL-UANOF               TO URECDTEA                      EL162
00800                                     USENTA                        EL162
051107*00801                                     UBYA                          EL162
00802                                     UFORMA                        EL162
00803                                     UREA.                         EL162
051107
051107     MOVE PI-PROCESSOR-ID        TO UBYI.
00804                                                                   EL162
00805  4150-REWRITE-CLAIM.                                              EL162
00806      EXEC CICS HANDLE CONDITION                                   EL162
00807          DUPKEY (4199-EXIT)                                          CL**8
00808      END-EXEC.                                                       CL*12
00809                                                                   EL162
00810      MOVE PI-PROCESSOR-ID   TO  CL-LAST-MAINT-USER.                  CL*13
00811      MOVE EIBTIME           TO  CL-LAST-MAINT-HHMMSS.                CL*13
00812      MOVE BIN-CURRENT-DATE  TO  CL-LAST-MAINT-DT.                    CL*13
00813      MOVE '2'               TO  CL-LAST-MAINT-TYPE.                  CL*13
00814                                                                      CL**9
00815      IF PI-COMPANY-ID = 'DMD'                                        CL*12
00816          MOVE 11                 TO CL-ACTIVITY-CODE                 CL*10
00817          MOVE BIN-CURRENT-DATE   TO CL-ACTIVITY-MAINT-DT             CL*13
00818          MOVE 'CORR'             TO CL-ACTIVITY-MAINT-TYPE           CL*10
00819          MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID.                 CL*10
00820                                                                   EL162
00821      EXEC CICS REWRITE                                            EL162
00822           DATASET(ELMSTR-FILE-ID)                                    CL**9
00823           FROM   (CLAIM-MASTER)                                      CL*12
00824      END-EXEC.                                                       CL*12
00825                                                                   EL162
00826  4199-EXIT.                                                       EL162
00827       EXIT.                                                       EL162
00828                                                                   EL162
00829      EJECT                                                        EL162
00830  4200-LOCATE-ADDRESS.                                             EL162
00831      IF ADDR-TYPE = '3' AND ADDR-SEQ = ZEROS                      EL162
00832         GO TO 4210-READ-ACCOUNT.                                  EL162
00833                                                                   EL162
00834      IF ADDR-TYPE = SPACES OR ADDR-SEQ = ZEROS                    EL162
00835         GO TO 4290-ERROR.                                         EL162
00836                                                                   EL162
00837      MOVE ADDR-SEQ               TO PI-ACTV-SEQ.                  EL162
00838                                                                   EL162
00839      EXEC CICS HANDLE CONDITION                                   EL162
00840           NOTFND(4290-ERROR)                                      EL162
00841      END-EXEC.                                                       CL*12
00842                                                                   EL162
00843      EXEC CICS READ                                               EL162
00844           DATASET(ELTRLR-FILE-ID)                                    CL**9
00845           RIDFLD (PI-ACTV-KEY)                                    EL162
00846           SET    (ADDRESS OF ACTIVITY-TRAILERS)                      CL**9
00847      END-EXEC.                                                       CL*12
00848                                                                   EL162
00849      MOVE AT-MAIL-TO-NAME        TO ADDR1O.                       EL162
00850      MOVE AT-ADDRESS-LINE-1      TO ADDR2O.                       EL162
00851      MOVE AT-ADDRESS-LINE-2      TO ADDR3O.                       EL162
00852 *    MOVE AT-CITY-STATE          TO ADDR4O.                       EL162
           STRING AT-CITY ' ' AT-STATE
              DELIMITED BY '  ' INTO ADDR4O
           END-STRING
00853      MOVE AT-PHONE-NO            TO PHONEO.                       EL162
00854      INSPECT PHONEI CONVERTING ' ' TO '-'.                           CL**9
00855                                                                      CL**7
00856      IF AT-CANADIAN-POST-CODE                                        CL*12
00857          MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1               CL**7
00858          MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2               CL**7
00859          MOVE SPACES             TO WS-DASH-CAN                      CL**7
00860                                     WS-CAN-FILLER                    CL**7
00861          MOVE WS-CANADIAN-POSTAL-CODES                               CL**7
00862                                  TO ZIPO                             CL**7
00863      ELSE                                                            CL**7
00864          MOVE AT-ZIP-CODE        TO WS-ZIP-CODE                      CL**7
00865          IF AT-ZIP-PLUS4 = SPACES OR ZEROS                           CL*12
00866              MOVE SPACES         TO WS-ZIP-PLUS4                     CL**7
00867                                     WS-DASH                          CL**7
00868              MOVE WS-ZIP         TO ZIPO                             CL**7
00869          ELSE                                                        CL**7
00870              MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4                     CL**7
00871              MOVE '-'            TO WS-DASH                          CL**7
00872              MOVE WS-ZIP         TO ZIPO.                            CL**7
00873                                                                      CL**7
00874      GO TO 4299-EXIT.                                             EL162
00875                                                                   EL162
00876  4210-READ-ACCOUNT.                                               EL162
00877      MOVE PI-COMPANY-CD          TO ACCT-CO.                      EL162
00878      MOVE PI-CARRIER             TO ACCT-CARRIER.                 EL162
00879      MOVE PI-GROUPING            TO ACCT-GROUPING.                EL162
00880      MOVE PI-STATE               TO ACCT-STATE.                   EL162
00881      MOVE PI-ACCOUNT             TO ACCT-ACCOUNT.                 EL162
00882      MOVE PI-CERT-EFF-DT         TO ACCT-EXP-DATE.                EL162
00883                                                                   EL162
00884      EXEC CICS HANDLE CONDITION                                   EL162
00885           NOTOPEN(8880-ACCT-NOT-OPEN)                             EL162
00886           NOTFND (4250-ACCT-NOT-FOUND)                            EL162
00887      END-EXEC.                                                       CL*12
00888                                                                   EL162
00889      EXEC CICS STARTBR                                            EL162
00890           RIDFLD   (ACCT-KEY)                                     EL162
00891           DATASET  (ERACCT-FILE-ID)                                  CL**9
00892           KEYLENGTH(13)                                           EL162
00893           GENERIC                                                 EL162
00894      END-EXEC.                                                       CL*12
00895                                                                   EL162
00896      MOVE ACCT-PARTIAL-KEY       TO ACCT-SAVE-KEY.                EL162
00897                                                                   EL162
00898  4230-READNEXT.                                                   EL162
00899      EXEC CICS READNEXT                                           EL162
00900           DATASET(ERACCT-FILE-ID)                                    CL**9
00901           SET    (ADDRESS OF ACCOUNT-MASTER)                         CL**9
00902           RIDFLD (ACCT-KEY)                                       EL162
00903      END-EXEC.                                                       CL*12
00904                                                                   EL162
00905      IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY                      EL162
00906         GO TO 4250-ACCT-NOT-FOUND.                                EL162
00907                                                                   EL162
00908      IF PI-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT AND             CL*12
00909         PI-CERT-EFF-DT     LESS THAN AM-EXPIRATION-DT                CL*12
00910         NEXT SENTENCE                                             EL162
00911       ELSE                                                        EL162
00912         GO TO 4230-READNEXT.                                      EL162
00913                                                                   EL162
00914      MOVE AM-NAME                TO ADDR1O.                       EL162
00915      MOVE AM-PERSON              TO ADDR2O.                       EL162
00916      MOVE AM-ADDRS               TO ADDR3O.                       EL162
00917 *    MOVE AM-CITY                TO ADDR4O.                       EL162
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
              DELIMITED BY '  ' INTO ADDR4O
           END-STRING
00918                                                                   EL162
00919      MOVE AM-TEL-NO              TO WS-WORK-PHONE.                EL162
00920      INSPECT WS-WORK-PHONE CONVERTING ' ' TO '0'.                    CL**9
00921      MOVE WS-NUMERIC-PHONE       TO PHONEO.                       EL162
00922      INSPECT PHONEI CONVERTING ' ' TO '-'.                           CL**9
00923                                                                      CL**7
00924      IF AM-CANADIAN-POST-CODE                                        CL*12
00925          MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1               CL**7
00926          MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2               CL**7
00927          MOVE SPACES             TO WS-DASH-CAN                      CL**7
00928                                     WS-CAN-FILLER                    CL**7
00929          MOVE WS-CANADIAN-POSTAL-CODES                               CL**7
00930                                  TO ZIPO                             CL**7
00931      ELSE                                                            CL**7
00932          MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE                      CL**7
00933          IF AM-ZIP-PLUS4 = SPACES OR ZEROS                           CL*12
00934              MOVE SPACES         TO WS-ZIP-PLUS4                     CL**7
00935                                     WS-DASH                          CL**7
00936              MOVE WS-ZIP         TO ZIPO                             CL**7
00937          ELSE                                                        CL**7
00938              MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4                     CL**7
00939              MOVE '-'            TO WS-DASH                          CL**7
00940              MOVE WS-ZIP         TO ZIPO.                            CL**7
00941                                                                   EL162
00942      PERFORM 4240-ENDBR.                                          EL162
00943                                                                   EL162
00944      GO TO 4299-EXIT.                                             EL162
00945                                                                   EL162
00946  4240-ENDBR.                                                      EL162
00947      EXEC CICS ENDBR                                              EL162
00948           DATASET(ERACCT-FILE-ID)                                    CL**9
00949      END-EXEC.                                                       CL*12
00950                                                                   EL162
00951  4250-ACCT-NOT-FOUND.                                             EL162
00952      PERFORM 4240-ENDBR.                                          EL162
00953      MOVE ER-0179                TO EMI-ERROR.                    EL162
00954      MOVE -1                     TO SC-RECL (1).                  EL162
00955      MOVE SPACES                 TO ADDR1I ADDR2I ADDR3I ADDR4I      CL*12
00956                                     ZIPI PHONEI.                     CL*12
00957                                                                   EL162
00958      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL162
00959                                                                   EL162
00960  4290-ERROR.                                                      EL162
00961      MOVE ER-0322                TO EMI-ERROR.                    EL162
00962      MOVE -1                     TO SC-RECL (1).                  EL162
00963      MOVE SPACES                 TO ADDR1I ADDR2I ADDR3I ADDR4I      CL*13
00964                                     ZIPI PHONEI.                     CL*13
00965                                                                   EL162
00966      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL162
00967                                                                   EL162
00968  4299-EXIT.                                                       EL162
00969       EXIT.                                                       EL162
00970                                                                   EL162
00971      EJECT                                                        EL162
00972  6000-BROWSE-FORWARD.                                             EL162
00973      IF PI-ACTV-SAVE NOT = PI-ACTV-PARTIAL                        EL162
00974         MOVE ER-0066             TO EMI-ERROR                     EL162
00975         MOVE -1                  TO SC-RECL (1)                   EL162
00976         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL162
00977         GO TO 8200-SEND-DATAONLY.                                 EL162
00978                                                                   EL162
00979      MOVE LOW-VALUES             TO EL162AI.                      EL162
00980      ADD 1 TO PI-ACTV-SEQ.                                           CL*12
00981                                                                      CL*13
00982      GO TO 7000-BUILD-SCREEN.                                     EL162
00983                                                                   EL162
00984  6100-BROWSE-BACKWARD.                                            EL162
00985                                                                      CL**6
00986      IF PI-NEXT-TRLR-SUB > 8                                         CL*13
00987         SUBTRACT 4 FROM PI-NEXT-TRLR-SUB.                         EL162
00988                                                                   EL162
00989      MOVE PI-NEXT-TRLR-SUB       TO SUB.                          EL162
00990                                                                   EL162
00991      IF SUB = 1 OR 5                                                 CL*13
00992         MOVE ER-0067             TO EMI-ERROR                     EL162
00993         MOVE -1                  TO SC-RECL (1)                   EL162
00994         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL162
00995         MOVE  1                  TO SUB                           EL162
00996         MOVE  +1                 TO PI-NEXT-TRLR-SUB                 CL**6
00997      ELSE                                                         EL162
00998         SUBTRACT 4 FROM SUB                                          CL**6
00999         MOVE SUB                 TO PI-NEXT-TRLR-SUB.                CL*12
01000                                                                   EL162
01001      MOVE LOW-VALUES             TO EL162AI                          CL*12
01002                                     TRL-LINES.                       CL*12
01003      MOVE PI-ACTV-SAVE           TO PI-ACTV-PARTIAL.              EL162
01004                                                                      CL**6
01005      MOVE PI-TRLRS (SUB)         TO PI-ACTV-SEQ.                     CL**6
01006                                                                      CL**6
01007      GO TO 7000-BUILD-SCREEN.                                        CL**6
01008                                                                   EL162
01009      EJECT                                                        EL162
01010  6200-FORMAT-DATA.                                                EL162
01011                                                                      CL**6
01012      IF FORM-CONTROL-TR                                           EL162
01013         GO TO 6250-FORMAT-FORM.                                   EL162
01014                                                                   EL162
01015      IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES                    EL162
01016         MOVE AT-LETTER-ANSWERED-DT TO DC-BIN-DATE-1               EL162
01017         MOVE SPACES                TO DC-OPTION-CODE                 CL*13
01018         PERFORM 9700-DATE-LINK                                    EL162
01019         MOVE DC-GREG-DATE-1-EDIT   TO SC-REC (SC-INDX).              CL*13
102610
102610     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
102610        MOVE AT-STOP-LETTER-DT     TO DC-BIN-DATE-1
102610        MOVE SPACES                TO DC-OPTION-CODE
102610        PERFORM 9700-DATE-LINK
102610        MOVE DC-GREG-DATE-1-EDIT   TO SC-STOP (SC-INDX)
102610     END-IF.
01020                                                                   EL162
01021      IF AT-LETTER-SENT-DT NOT = LOW-VALUES                        EL162
01022         MOVE AT-LETTER-SENT-DT   TO DC-BIN-DATE-1                 EL162
01023         MOVE SPACES              TO DC-OPTION-CODE                EL162
01024         PERFORM 9700-DATE-LINK                                    EL162
01025         MOVE DC-GREG-DATE-1-EDIT TO SC-SENT (SC-INDX).            EL162
01026                                                                   EL162
01027      IF AT-AUTO-RE-SEND-DT NOT = LOW-VALUES                       EL162
01028         MOVE AT-AUTO-RE-SEND-DT  TO DC-BIN-DATE-1                 EL162
01029         MOVE SPACES              TO DC-OPTION-CODE                EL162
01030         PERFORM 9700-DATE-LINK                                    EL162
01031         MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).          EL162
01032                                                                   EL162
01033      IF AT-RECEIPT-FOLLOW-UP NOT = LOW-VALUES                     EL162
01034         MOVE AT-RECEIPT-FOLLOW-UP TO DC-BIN-DATE-1                EL162
01035         MOVE SPACES               TO DC-OPTION-CODE                  CL*13
01036         PERFORM 9700-DATE-LINK                                    EL162
01037         MOVE DC-GREG-DATE-1-EDIT  TO SC-FOLLOWUP (SC-INDX).          CL*13
01038                                                                   EL162
01039      MOVE AT-STD-LETTER-FORM     TO SC-FORM (SC-INDX).            EL162
01040                                                                   EL162
01041      IF INSURED-ADDRESEE                                          EL162
01042         MOVE 'INSURED'           TO SC-TO (SC-INDX)               EL162
01043      ELSE                                                            CL**6
01044      IF BENEFICIARY-ADDRESEE                                         CL**6
01045         MOVE 'BENEFI '           TO SC-TO (SC-INDX)                  CL**6
01046      ELSE                                                            CL**6
01047      IF ACCOUNT-ADDRESEE                                             CL**6
01048         MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)                  CL**6
01049      ELSE                                                            CL**6
01050      IF PHYSICIAN-ADDRESEE                                           CL**6
01051         MOVE 'DOCTOR'            TO SC-TO (SC-INDX)                  CL**6
01052      ELSE                                                            CL**6
01053      IF EMPLOYER-ADDRESEE                                            CL**6
01054         MOVE 'EMPLOYER'          TO SC-TO (SC-INDX)                  CL**6
01055      ELSE                                                            CL**6
01056      IF OTHER-ADDRESEE-1                                             CL**6
01057         MOVE 'OTHER-1'           TO SC-TO (SC-INDX)                  CL**6
01058      ELSE                                                            CL**6
01059         MOVE 'OTHER-2'           TO SC-TO (SC-INDX).                 CL*13
01060                                                                   EL162
01061      IF AT-LETTER-ARCHIVE-NO NOT = +0                                CL*13
01062         MOVE AT-LETTER-ARCHIVE-NO TO SC-ARCHIVE (SC-INDX)            CL*13
01063      ELSE                                                            CL**6
01064         MOVE SPACES              TO SC-ARCHIVE (SC-INDX).         EL162
01065                                                                   EL162
01066      MOVE 'L'                    TO SC-TYPE   (SC-INDX).             CL**6
01067                                                                      CL*11
01068      MOVE AT-REASON-TEXT         TO SC-REASON (SC-INDX).          EL162
01069                                                                      CL*11
CIDMOD*    IF PI-COMPANY-ID = 'DMD'                                        CL*11
CIDMOD*       IF AT-DMD-LETTER-PURGED                                      CL*11
CIDMOD*          MOVE AT-REASON-TEXT          TO WS-REASON                 CL*11
CIDMOD*          MOVE AT-DMD-LETTER-PURGE-DT  TO DC-BIN-DATE-1             CL*11
CIDMOD*          MOVE SPACES                  TO DC-OPTION-CODE            CL*11
CIDMOD*          PERFORM 9700-DATE-LINK                                    CL*11
CIDMOD*          MOVE 'PURGED -'              TO WS-REASON (51:9)          CL*11
CIDMOD*          MOVE DC-GREG-DATE-1-EDIT     TO WS-REASON (60:11)         CL*11
CIDMOD*          MOVE WS-REASON               TO SC-REASON (SC-INDX).      CL*11
CIDMOD*                                                                    CL*11
01080      MOVE AT-RECORDED-BY         TO SC-BY     (SC-INDX).          EL162
01081      MOVE AT-SEQUENCE-NO         TO SC-TRLRNO (SC-INDX).          EL162
01082      MOVE AL-UANOF               TO SC-RECA   (SC-INDX)              CL*12
102610                                    SC-STOPA  (SC-INDX).
01084                                                                   EL162
01085      GO TO 6299-EXIT.                                             EL162
01086                                                                   EL162
01087  6250-FORMAT-FORM.                                                EL162
102610
102610     IF AT-STOP-FORM-DT NOT = LOW-VALUES AND SPACES
102610        MOVE AT-STOP-FORM-DT       TO DC-BIN-DATE-1
102610        MOVE SPACES                TO DC-OPTION-CODE
102610        PERFORM 9700-DATE-LINK
102610        MOVE DC-GREG-DATE-1-EDIT   TO SC-STOP (SC-INDX)
102610     END-IF.
01088                                                                      CL**6
01089      IF AT-FORM-SEND-ON-DT NOT = LOW-VALUES                          CL*13
01090         IF (AT-FORM-ANSWERED-DT = LOW-VALUES)                        CL*13
01091                          OR                                          CL*13
01092            (AT-FORM-ANSWERED-DT NOT = LOW-VALUES  AND                CL*13
01093             AT-FORM-ANSWERED-DT = BIN-CURRENT-DATE)                  CL*13
01094              NEXT SENTENCE                                           CL*13
01095          ELSE                                                        CL*13
01096              GO TO 6260-CHECK-EMPLOYER-FORM                          CL*13
01097      ELSE                                                            CL**6
01098         GO TO 6260-CHECK-EMPLOYER-FORM.                              CL**6
01099                                                                      CL**6
01100      IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES                      EL162
01101         MOVE AT-FORM-ANSWERED-DT TO DC-BIN-DATE-1                 EL162
01102         MOVE SPACES              TO DC-OPTION-CODE                EL162
01103         PERFORM 9700-DATE-LINK                                    EL162
01104         MOVE DC-GREG-DATE-1-EDIT TO SC-REC (SC-INDX).             EL162
01105                                                                   EL162
01106      IF AT-FORM-SEND-ON-DT NOT = LOW-VALUES                       EL162
01107         MOVE AT-FORM-SEND-ON-DT  TO DC-BIN-DATE-1                 EL162
01108         MOVE SPACES              TO DC-OPTION-CODE                EL162
01109         PERFORM 9700-DATE-LINK                                    EL162
01110         MOVE DC-GREG-DATE-1-EDIT TO SC-SENT (SC-INDX).            EL162
01111                                                                   EL162
01112      IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES                       EL162
01113         MOVE AT-FORM-RE-SEND-DT  TO DC-BIN-DATE-1                 EL162
01114         MOVE SPACES              TO DC-OPTION-CODE                EL162
01115         PERFORM 9700-DATE-LINK                                    EL162
01116         MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).          EL162
01117                                                                   EL162
01118      IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES                     EL162
01119         MOVE AT-FORM-FOLLOW-UP-DT   TO DC-BIN-DATE-1              EL162
01120         MOVE SPACES                 TO DC-OPTION-CODE             EL162
01121         PERFORM 9700-DATE-LINK                                    EL162
01122         MOVE DC-GREG-DATE-1-EDIT TO SC-FOLLOWUP (SC-INDX).        EL162
01123                                                                   EL162
01124      IF AT-FORM-TYPE = '1'                                        EL162
01125         MOVE 'INIT'              TO SC-FORM (SC-INDX)             EL162
01126      ELSE                                                         EL162
01127         MOVE 'PROG'              TO SC-FORM (SC-INDX).            EL162
01128                                                                   EL162
01129      IF FORM-TO-INSURED                                           EL162
01130         MOVE 'INSURED'           TO SC-TO (SC-INDX)               EL162
01131      ELSE                                                            CL*12
01132      IF FORM-TO-ACCOUNT                                              CL*12
01133         MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)                  CL*12
01134      ELSE                                                            CL*12
01135      IF FORM-TO-OTHER-1                                              CL*12
01136         MOVE 'OTHER-1'           TO SC-TO (SC-INDX)                  CL*12
01137      ELSE                                                            CL*12
01138      IF FORM-TO-OTHER-2                                              CL*12
01139         MOVE 'OTHER-2'           TO SC-TO (SC-INDX).                 CL*12
01140                                                                   EL162
01141      MOVE 'CLAIMANT'             TO SC-ARCHIVE (SC-INDX).            CL**6
01142      MOVE 'C'                    TO SC-TYPE    (SC-INDX).            CL**6
01143      MOVE AT-SEQUENCE-NO         TO PI-TRLRS (PI-NEXT-TRLR-SUB)      CL**6
01144      MOVE AT-INSTRUCT-LN-1       TO SC-REASON  (SC-INDX).            CL**6
01145      MOVE AT-RECORDED-BY         TO SC-BY      (SC-INDX).            CL**6
01146      MOVE AT-SEQUENCE-NO         TO SC-TRLRNO  (SC-INDX).            CL**6
01147      MOVE AL-UANOF               TO SC-RECA    (SC-INDX).            CL**6
102610     MOVE AL-UANOF               TO SC-STOPA   (SC-INDX).
01149                                                                      CL**6
01150  6260-CHECK-EMPLOYER-FORM.                                           CL**6
01151                                                                      CL**6
01152      IF AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES                      CL*13
01153         IF (AT-EMP-FORM-ANSWERED-DT = LOW-VALUES)                    CL*13
01154                           OR                                         CL*13
01155            (AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES AND             CL*13
01156             AT-EMP-FORM-ANSWERED-DT = BIN-CURRENT-DATE)              CL*13
01157               NEXT SENTENCE                                          CL*13
01158           ELSE                                                       CL*13
01159               GO TO 6280-CHECK-PHYSICIAN-FORM                        CL*13
01160      ELSE                                                            CL**6
01161         GO TO 6280-CHECK-PHYSICIAN-FORM.                             CL**6
01162                                                                      CL**6
01163      IF SC-TYPE (SC-INDX) = 'C'                                      CL*13
01164         SET SC-INDX UP BY 1                                          CL**6
01165         ADD +1 TO PI-NEXT-TRLR-SUB.                                  CL**6
01166                                                                      CL**6
01167      IF AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES                     CL**6
01168         MOVE AT-EMP-FORM-ANSWERED-DT TO DC-BIN-DATE-1                CL**6
01169         MOVE SPACES                  TO DC-OPTION-CODE               CL*13
01170         PERFORM 9700-DATE-LINK                                       CL**6
01171         MOVE DC-GREG-DATE-1-EDIT     TO SC-REC (SC-INDX).            CL*13
01172                                                                      CL**6
01173      IF AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES                      CL**6
01174         MOVE AT-EMP-FORM-SEND-ON-DT TO DC-BIN-DATE-1                 CL*13
01175         MOVE SPACES                 TO DC-OPTION-CODE                CL*13
01176         PERFORM 9700-DATE-LINK                                       CL**6
01177         MOVE DC-GREG-DATE-1-EDIT    TO SC-SENT (SC-INDX).            CL*13
01178                                                                      CL**6
01179      IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES                          CL**6
01180         MOVE AT-FORM-RE-SEND-DT  TO DC-BIN-DATE-1                    CL**6
01181         MOVE SPACES              TO DC-OPTION-CODE                   CL**6
01182         PERFORM 9700-DATE-LINK                                       CL**6
01183         MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).             CL**6
01184                                                                      CL**6
01185      IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES                        CL**6
01186         MOVE AT-FORM-FOLLOW-UP-DT   TO DC-BIN-DATE-1                 CL**6
01187         MOVE SPACES                 TO DC-OPTION-CODE                CL**6
01188         PERFORM 9700-DATE-LINK                                       CL**6
01189         MOVE DC-GREG-DATE-1-EDIT    TO SC-FOLLOWUP (SC-INDX).        CL*13
01190                                                                      CL**6
01191      IF AT-FORM-TYPE = '1'                                           CL**6
01192         MOVE 'INIT'              TO SC-FORM (SC-INDX)                CL**6
01193      ELSE                                                            CL**6
01194         MOVE 'PROG'              TO SC-FORM (SC-INDX).               CL**6
01195                                                                      CL**6
01196      IF FORM-TO-INSURED                                              CL**6
01197         MOVE 'INSURED'           TO SC-TO (SC-INDX)                  CL**6
01198      ELSE                                                            CL**6
01199      IF FORM-TO-ACCOUNT                                              CL**6
01200         MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)                  CL**6
01201      ELSE                                                            CL**6
01202      IF FORM-TO-OTHER-1                                              CL**6
01203         MOVE 'OTHER-1'           TO SC-TO (SC-INDX)                  CL**6
01204      ELSE                                                            CL**6
01205      IF FORM-TO-OTHER-2                                              CL**6
01206         MOVE 'OTHER-2'           TO SC-TO (SC-INDX).                 CL**6
01207                                                                      CL**6
01208      MOVE 'EMPLOYER'             TO SC-ARCHIVE (SC-INDX).            CL**6
01209      MOVE 'E'                    TO SC-TYPE    (SC-INDX).            CL**6
01210      MOVE AT-SEQUENCE-NO         TO PI-TRLRS (PI-NEXT-TRLR-SUB)      CL**6
01211      MOVE AT-INSTRUCT-LN-1       TO SC-REASON  (SC-INDX).            CL**6
01212      MOVE AT-RECORDED-BY         TO SC-BY      (SC-INDX).            CL**6
01213      MOVE AT-SEQUENCE-NO         TO SC-TRLRNO  (SC-INDX).            CL**6
01214      MOVE AL-UANOF               TO SC-RECA    (SC-INDX)             CL*13
102610                                    SC-STOPA   (SC-INDX).
01216                                                                      CL**6
01217  6280-CHECK-PHYSICIAN-FORM.                                          CL**6
01218                                                                      CL**6
01219      IF AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES                      CL*13
01220         IF (AT-PHY-FORM-ANSWERED-DT = LOW-VALUES)                    CL*13
01221                          OR                                          CL*13
01222            (AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES AND             CL*13
01223             AT-PHY-FORM-ANSWERED-DT = BIN-CURRENT-DATE)              CL*13
01224               NEXT SENTENCE                                          CL*13
01225           ELSE                                                       CL*13
01226               GO TO 6299-EXIT                                        CL*13
01227      ELSE                                                            CL**6
01228         GO TO 6299-EXIT.                                             CL**6
01229                                                                      CL**6
01230      IF SC-TYPE (SC-INDX) = 'C' OR 'E'                               CL*13
01231         SET SC-INDX UP BY 1                                          CL**6
01232         ADD +1 TO PI-NEXT-TRLR-SUB.                                  CL**6
01233                                                                      CL**6
01234      IF AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES                     CL**6
01235         MOVE AT-PHY-FORM-ANSWERED-DT TO DC-BIN-DATE-1                CL**6
01236         MOVE SPACES                  TO DC-OPTION-CODE               CL*13
01237         PERFORM 9700-DATE-LINK                                       CL**6
01238         MOVE DC-GREG-DATE-1-EDIT     TO SC-REC (SC-INDX).            CL*13
01239                                                                      CL**6
01240      IF AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES                      CL**6
01241         MOVE AT-PHY-FORM-SEND-ON-DT TO DC-BIN-DATE-1                 CL*13
01242         MOVE SPACES                 TO DC-OPTION-CODE                CL*13
01243         PERFORM 9700-DATE-LINK                                       CL**6
01244         MOVE DC-GREG-DATE-1-EDIT    TO SC-SENT (SC-INDX).            CL*13
01245                                                                      CL**6
01246      IF AT-FORM-RE-SEND-DT NOT = LOW-VALUES                          CL**6
01247         MOVE AT-FORM-RE-SEND-DT  TO DC-BIN-DATE-1                    CL**6
01248         MOVE SPACES              TO DC-OPTION-CODE                   CL**6
01249         PERFORM 9700-DATE-LINK                                       CL**6
01250         MOVE DC-GREG-DATE-1-EDIT TO SC-RESEND (SC-INDX).             CL**6
01251                                                                      CL**6
01252      IF AT-FORM-FOLLOW-UP-DT NOT = LOW-VALUES                        CL**6
01253         MOVE AT-FORM-FOLLOW-UP-DT   TO DC-BIN-DATE-1                 CL**6
01254         MOVE SPACES                 TO DC-OPTION-CODE                CL**6
01255         PERFORM 9700-DATE-LINK                                       CL**6
01256         MOVE DC-GREG-DATE-1-EDIT    TO SC-FOLLOWUP (SC-INDX).        CL*13
01257                                                                      CL**6
01258      IF AT-FORM-TYPE = '1'                                           CL**6
01259         MOVE 'INIT'              TO SC-FORM (SC-INDX)                CL**6
01260      ELSE                                                            CL**6
01261         MOVE 'PROG'              TO SC-FORM (SC-INDX).               CL**6
01262                                                                      CL**6
01263      IF FORM-TO-INSURED                                              CL**6
01264         MOVE 'INSURED'           TO SC-TO (SC-INDX)                  CL**6
01265      ELSE                                                            CL**6
01266      IF FORM-TO-ACCOUNT                                              CL**6
01267         MOVE 'ACCOUNT'           TO SC-TO (SC-INDX)                  CL**6
01268      ELSE                                                            CL**6
01269      IF FORM-TO-OTHER-1                                              CL**6
01270         MOVE 'OTHER-1'           TO SC-TO (SC-INDX)                  CL**6
01271      ELSE                                                            CL**6
01272      IF FORM-TO-OTHER-2                                              CL**6
01273         MOVE 'OTHER-2'           TO SC-TO (SC-INDX).                 CL**6
01274                                                                      CL**6
01275      MOVE ' DOCTOR '             TO SC-ARCHIVE (SC-INDX).            CL**6
01276      MOVE 'P'                    TO SC-TYPE    (SC-INDX).            CL**6
01277      MOVE AT-SEQUENCE-NO         TO PI-TRLRS (PI-NEXT-TRLR-SUB)      CL**6
01278      MOVE AT-INSTRUCT-LN-1       TO SC-REASON  (SC-INDX).         EL162
01279      MOVE AT-RECORDED-BY         TO SC-BY      (SC-INDX).         EL162
01280      MOVE AT-SEQUENCE-NO         TO SC-TRLRNO  (SC-INDX).         EL162
01281      MOVE AL-UANOF               TO SC-RECA    (SC-INDX)             CL*12
102610                                    SC-STOPA   (SC-INDX).
01283                                                                   EL162
01284  6299-EXIT.                                                       EL162
01285       EXIT.                                                       EL162
01286                                                                   EL162
01287      EJECT                                                        EL162
01288  7000-BUILD-SCREEN.                                               EL162
01289                                                                      CL**6
01290      IF PI-CLAIM-NO = SPACES                                      EL162
01291         MOVE ER-0319             TO EMI-ERROR                     EL162
01292         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL162
01293         GO TO 8100-SEND-INITIAL-MAP.                              EL162
01294                                                                   EL162
01295      PERFORM 3000-BUILD-KEYS.                                     EL162
051107
051107     MOVE PI-PROCESSOR-ID        TO UBYI.
01296                                                                   EL162
01297      EXEC CICS HANDLE CONDITION                                   EL162
01298           NOTOPEN(8870-CLAIM-NOT-OPEN)                            EL162
01299           NOTFND (8860-CLAIM-NOT-FOUND)                           EL162
01300      END-EXEC.                                                       CL*12
01301                                                                   EL162
01302      EXEC CICS READ                                               EL162
01303           DATASET(ELMSTR-FILE-ID)                                    CL**9
01304           RIDFLD (PI-CLAM-KEY)                                    EL162
01305           SET    (ADDRESS OF CLAIM-MASTER)                           CL**9
01306      END-EXEC.                                                       CL*12
01307                                                                   EL162
121802     EVALUATE TRUE
121802     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
01309         MOVE PI-LIFE-OVERRIDE-L6 TO TYPEI

121802     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
01311         MOVE PI-AH-OVERRIDE-L6   TO TYPEI

121802     WHEN CL-CLAIM-TYPE = 'I'
121802        MOVE ' IU   '            TO TYPEI

121802     WHEN CL-CLAIM-TYPE = 'G'
121802        MOVE ' GAP  '            TO TYPEI
052614
052614     WHEN CL-CLAIM-TYPE = 'F'
052614        MOVE ' FAM  '            TO TYPEI
080322
080322     when cl-claim-type = 'B'
080322        move ' BRV  '            TO TYPEI
080322     when cl-claim-type = 'H'
080322        move ' HOSP '            TO TYPEI
100518
100518     WHEN CL-CLAIM-TYPE = 'O'
100518        MOVE ' OTH  '            TO TYPEI

121802     END-EVALUATE
01312                                                                   EL162
01313      MOVE CL-INSURED-LAST-NAME   TO LASTNMEI.                     EL162
01314      MOVE CL-FILE-LOCATION       TO FILEI.                        EL162
01315      MOVE PI-ACTV-PARTIAL        TO PI-ACTV-SAVE.                 EL162
01316                                                                   EL162
01317      IF CL-1ST-TRL-AVAIL                                          EL162
01318         GO TO 7100-NOT-FOUND.                                     EL162
01319                                                                   EL162
01320  7010-START-BROWSE.                                               EL162
01321                                                                      CL**6
01322      EXEC CICS HANDLE CONDITION                                   EL162
01323           NOTOPEN(8820-ACTV-NOT-OPEN)                             EL162
01324           NOTFND (7100-NOT-FOUND)                                 EL162
01325           ENDFILE(7100-NOT-FOUND)                                 EL162
01326      END-EXEC.                                                       CL*12
01327                                                                   EL162
01328      EXEC CICS STARTBR                                            EL162
01329           DATASET(ELTRLR-FILE-ID)                                    CL**9
01330           RIDFLD (PI-ACTV-KEY)                                       CL*12
01331           GTEQ                                                    EL162
01332      END-EXEC.                                                       CL*12
01333                                                                   EL162
01334      SET SC-INDX TO 1.                                            EL162
01335                                                                   EL162
01336  7020-READ-NEXT.                                                  EL162
01337                                                                      CL**6
01338      EXEC CICS READNEXT                                           EL162
01339           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL**9
01340           RIDFLD   (PI-ACTV-KEY)                                     CL**6
01341           DATASET  (ELTRLR-FILE-ID)                                  CL**9
01342      END-EXEC.                                                       CL*12
01343                                                                   EL162
01344      IF PI-ACTV-PARTIAL NOT = PI-ACTV-SAVE                        EL162
01345         GO TO 7100-NOT-FOUND.                                     EL162
01346                                                                   EL162
01347      IF AT-TRAILER-TYPE = '4' OR  'A'                             EL162
01348         NEXT SENTENCE                                             EL162
01349      ELSE                                                         EL162
01350         GO TO 7020-READ-NEXT.                                     EL162
01351                                                                   EL162
01352      IF CORRESPONDENCE-TR                                         EL162
01353         IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES AND             EL162
01354            AT-LETTER-ANSWERED-DT NOT = BIN-CURRENT-DATE              CL**4
01355              GO TO 7020-READ-NEXT.                                   CL*13
100610
100610     IF CORRESPONDENCE-TR AND
100610        AT-RESEND-PRINT-DATE NOT = LOW-VALUES AND
100610        AT-RESEND-LETTER-FORM NOT = LOW-VALUES
100610             GO TO 7020-READ-NEXT
100610     END-IF.
102610
102610     IF CORRESPONDENCE-TR AND
102610        AT-STOP-LETTER-DT NOT = LOW-VALUES AND
102610        AT-STOP-LETTER-DT NOT = SPACES AND
102610        AT-STOP-LETTER-DT NOT = BIN-CURRENT-DATE
102610*        AT-STOP-LETTER-DT = AT-LETTER-SENT-DT
102610             GO TO 7020-READ-NEXT
102610     END-IF.
01356                                                                   EL162
01357      IF NOT FORM-CONTROL-TR                                          CL**6
01358         GO TO 7030-CONTINUE.                                         CL**6
01359                                                                      CL**6
01360      IF ((AT-FORM-ANSWERED-DT NOT = LOW-VALUES AND                   CL**6
01361           AT-FORM-ANSWERED-DT NOT = BIN-CURRENT-DATE) OR             CL*13
01362           AT-FORM-SEND-ON-DT = LOW-VALUES)                           CL*13
01363        AND                                                           CL**6
01364         ((AT-EMP-FORM-ANSWERED-DT NOT = LOW-VALUES AND               CL**6
01365           AT-EMP-FORM-ANSWERED-DT NOT = BIN-CURRENT-DATE) OR         CL*13
01366           AT-EMP-FORM-SEND-ON-DT = LOW-VALUES)                       CL*13
01367        AND                                                           CL**6
01368         ((AT-PHY-FORM-ANSWERED-DT NOT = LOW-VALUES AND               CL**6
01369           AT-PHY-FORM-ANSWERED-DT NOT = BIN-CURRENT-DATE) OR         CL*13
01370           AT-PHY-FORM-SEND-ON-DT = LOW-VALUES)                       CL*13
01371              GO TO 7020-READ-NEXT.                                   CL*13
01372                                                                      CL**6
01373      IF (AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES) AND                CL*13
01374         (AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES)                    CL*13
01375         IF SC-INDX > 2                                               CL*13
01376            MOVE PI-ACTV-SAVE-SEQ TO PI-ACTV-SEQ                      CL**6
01377            GO TO 7040-END-BUILD-SCREEN.                              CL**6
01378                                                                      CL**6
01379      IF (AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES) OR                 CL*13
01380         (AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES)                    CL*13
01381         IF SC-INDX > 3                                               CL*13
01382            MOVE PI-ACTV-SAVE-SEQ TO PI-ACTV-SEQ                      CL**6
01383            GO TO 7040-END-BUILD-SCREEN.                              CL**6
01384                                                                      CL**6
01385  7030-CONTINUE.                                                      CL**6
01386                                                                   EL162
01387      PERFORM 6200-FORMAT-DATA THRU 6299-EXIT.                     EL162
01388                                                                      CL**6
01389      MOVE PI-ACTV-SEQ             TO PI-ACTV-SAVE-SEQ.               CL**6
01390                                                                   EL162
01391      MOVE AT-SEQUENCE-NO          TO PI-TRLRS (PI-NEXT-TRLR-SUB).    CL*13
01392                                                                   EL162
01393      ADD 1   TO PI-NEXT-TRLR-SUB.                                 EL162
01394      SET SC-INDX UP BY 1.                                         EL162
01395                                                                   EL162
01396      IF SC-INDX NOT = 5                                           EL162
01397         GO TO 7020-READ-NEXT.                                     EL162
01398                                                                      CL**6
01399  7040-END-BUILD-SCREEN.                                              CL**6
01400                                                                      CL**6
01401      IF SC-INDX = +2                                                 CL*13
01402         ADD +3 TO PI-NEXT-TRLR-SUB                                   CL**6
01403      ELSE                                                            CL**6
01404      IF SC-INDX = +3                                                 CL*13
01405         ADD +2 TO PI-NEXT-TRLR-SUB                                   CL**6
01406      ELSE                                                            CL**6
01407      IF SC-INDX = +4                                                 CL*13
01408         ADD +1 TO PI-NEXT-TRLR-SUB.                                  CL**6
01409                                                                   EL162
01410      MOVE -1                     TO SC-RECL (1).                  EL162
01411      GO TO 8100-SEND-INITIAL-MAP.                                 EL162
01412                                                                   EL162
01413  7100-NOT-FOUND.                                                  EL162
01414      IF PI-NEXT-TRLR-SUB = 1                                      EL162
01415         MOVE ER-0320             TO EMI-ERROR                     EL162
01416         MOVE -1                  TO URECDTEL                      EL162
01417         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL162
01418         GO TO 8100-SEND-INITIAL-MAP.                              EL162
01419                                                                   EL162
01420      MOVE -1                     TO SC-RECL (1).                  EL162
01421                                                                   EL162
01422 *    IF ANY LINES WERE PRINTED THEN                               EL162
01423 *    SET THE SUB TO ALWAYS BE 1 GREATER THAN A MULTIPLE OF 4.     EL162
01424                                                                   EL162
01425      SET INDX-WORK TO SC-INDX.                                    EL162
01426                                                                   EL162
01427      IF SC-INDX NOT = 1                                           EL162
01428         COMPUTE PI-NEXT-TRLR-SUB = (4 - INDX-WORK) + 1 +          EL162
01429                                     PI-NEXT-TRLR-SUB.             EL162
01430                                                                   EL162
01431      IF EIBAID NOT = DFHPF1                                          CL*13
01432         GO TO 8100-SEND-INITIAL-MAP.                              EL162
01433                                                                   EL162
01434      MOVE ER-0066                TO EMI-ERROR.                    EL162
01435      MOVE -1                     TO SC-RECL (1).                  EL162
01436                                                                   EL162
01437      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL162
01438                                                                   EL162
01439      GO TO 8100-SEND-INITIAL-MAP.                                 EL162
01440                                                                   EL162
01441      EJECT                                                        EL162
01442  8100-SEND-INITIAL-MAP.                                           EL162
01443      MOVE SAVE-DATE    TO RUNDTEO.                                EL162
01444      MOVE EIBTIME      TO TIME-IN.                                EL162
01445      MOVE TIME-OUT     TO RUNTIMEO.                               EL162
01446                                                                   EL162
01447      IF NOT EMI-NO-ERRORS                                         EL162
01448          SET EMI-INDX TO 1                                        EL162
01449          MOVE EMI-MESSAGE-AREA (EMI-INDX) TO ERRMSG1O.            EL162
01450                                                                   EL162
01451      IF PI-COMPANY-ID NOT = 'DMD'                                    CL*15
01452          MOVE SPACES             TO PF4KEYO.                         CL*15
01453                                                                      CL*15
01454      IF PI-RETURN-TO-PROGRAM = 'EL150'                               CL*12
01455          MOVE SPACES             TO PF5KEYO.                         CL*15
01456                                                                      CL*10
01457      EXEC CICS SEND                                               EL162
01458          MAP   (MAP-NAME)                                            CL*12
01459          MAPSET(MAPSET-NAME)                                      EL162
01460          FROM  (EL162AO)                                             CL*12
01461          ERASE                                                    EL162
01462          CURSOR                                                   EL162
01463      END-EXEC.                                                       CL*12
01464                                                                   EL162
01465      GO TO 9100-RETURN-TRAN.                                      EL162
01466                                                                   EL162
01467  8200-SEND-DATAONLY.                                              EL162
01468      MOVE SAVE-DATE            TO RUNDTEO.                           CL*13
01469      MOVE EIBTIME              TO TIME-IN.                           CL*13
01470      MOVE TIME-OUT             TO RUNTIMEO.                          CL*13
01471      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL162
01472                                                                      CL*10
01473      IF PI-COMPANY-ID NOT = 'DMD'                                    CL*15
01474          MOVE SPACES             TO PF4KEYO.                         CL*15
01475                                                                      CL*15
01476      IF PI-RETURN-TO-PROGRAM = 'EL150'                               CL*12
01477          MOVE SPACES             TO PF5KEYO.                         CL*15
01478                                                                   EL162
01479      EXEC CICS SEND                                               EL162
01480          MAP   (MAP-NAME)                                            CL*12
01481          MAPSET(MAPSET-NAME)                                      EL162
01482          FROM  (EL162AO)                                             CL*12
01483          DATAONLY                                                 EL162
01484          CURSOR                                                   EL162
01485      END-EXEC.                                                       CL*12
01486                                                                   EL162
01487      GO TO 9100-RETURN-TRAN.                                      EL162
01488                                                                   EL162
01489  8300-SEND-TEXT.                                                  EL162
01490      EXEC CICS SEND TEXT                                          EL162
01491          FROM  (LOGOFF-TEXT)                                         CL*12
01492          LENGTH(LOGOFF-LENGTH)                                    EL162
01493          ERASE                                                    EL162
01494          FREEKB                                                   EL162
01495      END-EXEC.                                                       CL*12
01496                                                                   EL162
01497      EXEC CICS RETURN                                             EL162
01498      END-EXEC.                                                       CL*12
01499                                                                   EL162
01500      EJECT                                                        EL162
01501  8600-DEEDIT.                                                     EL162
01502      EXEC CICS BIF DEEDIT                                         EL162
01503          FIELD (DEEDIT-FIELD)                                        CL*12
01504          LENGTH(15)                                                  CL*12
01505      END-EXEC.                                                       CL*12
01506                                                                   EL162
01507  8800-UNAUTHORIZED-ACCESS.                                        EL162
01508      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL162
01509      GO TO 8300-SEND-TEXT.                                        EL162
01510                                                                   EL162
01511  8810-PF23.                                                       EL162
01512      MOVE EIBAID                   TO PI-ENTRY-CD-1.              EL162
01513      MOVE XCTL-005 TO PGM-NAME.                                   EL162
01514      GO TO 9300-XCTL.                                             EL162
01515                                                                   EL162
01516  8820-ACTV-NOT-OPEN.                                              EL162
01517      MOVE ER-0172                TO EMI-ERROR.                    EL162
01518      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL162
01519      GO TO 8200-SEND-DATAONLY.                                    EL162
01520                                                                   EL162
01521  8860-CLAIM-NOT-FOUND.                                            EL162
01522      MOVE ER-0319                TO EMI-ERROR.                    EL162
01523      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL162
01524      GO TO 8100-SEND-INITIAL-MAP.                                 EL162
01525                                                                   EL162
01526  8870-CLAIM-NOT-OPEN.                                             EL162
01527      MOVE ER-0042                TO EMI-ERROR.                    EL162
01528      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL162
01529      GO TO 8200-SEND-DATAONLY.                                    EL162
01530                                                                   EL162
01531  8880-ACCT-NOT-OPEN.                                              EL162
01532      MOVE ER-0168                TO EMI-ERROR                     EL162
01533      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL162
01534      GO TO 8200-SEND-DATAONLY.                                    EL162
01535                                                                   EL162
01536  9100-RETURN-TRAN.                                                EL162
01537      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL162
01538      MOVE MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.           EL162
01539      EXEC CICS RETURN                                             EL162
01540          TRANSID(TRANS-ID)                                        EL162
01541          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL162
01542          LENGTH(PI-COMM-LENGTH)                                   EL162
01543      END-EXEC.                                                       CL*13
01544                                                                   EL162
01545  9200-RETURN-MAIN-MENU.                                           EL162
01546      MOVE XCTL-126 TO PGM-NAME.                                   EL162
01547      GO TO 9300-XCTL.                                             EL162
01548                                                                   EL162
01549  9300-XCTL.                                                       EL162
01550      EXEC CICS XCTL                                               EL162
01551          PROGRAM(PGM-NAME)                                        EL162
01552          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL162
01553          LENGTH(PI-COMM-LENGTH)                                   EL162
01554      END-EXEC.                                                       CL*12
01555                                                                   EL162
01556  9400-CLEAR.                                                      EL162
01557      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL162
01558      GO TO 9300-XCTL.                                             EL162
01559                                                                   EL162
01560  9500-PF12.                                                       EL162
01561      MOVE XCTL-010 TO PGM-NAME.                                   EL162
01562      GO TO 9300-XCTL.                                             EL162
01563                                                                   EL162
01564  9600-PGMID-ERROR.                                                EL162
01565      EXEC CICS HANDLE CONDITION                                   EL162
01566          PGMIDERR(8300-SEND-TEXT)                                 EL162
01567      END-EXEC.                                                       CL*12
01568                                                                   EL162
01569      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                        CL*12
01570      MOVE ' '          TO PI-ENTRY-CD-1.                             CL*12
01571      MOVE XCTL-005     TO PGM-NAME.                                  CL*12
01572      MOVE PGM-NAME     TO LOGOFF-PGM.                                CL*12
01573      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL162
01574      GO TO 9300-XCTL.                                             EL162
01575                                                                   EL162
01576  9700-DATE-LINK.                                                  EL162
01577      MOVE LINK-ELDATCV           TO PGM-NAME.                        CL**9
01578                                                                   EL162
01579      EXEC CICS LINK                                               EL162
01580           PROGRAM (PGM-NAME)                                         CL*12
01581           COMMAREA(DATE-CONVERSION-DATA)                          EL162
01582           LENGTH  (DC-COMM-LENGTH)                                   CL*12
01583      END-EXEC.                                                       CL*12
01584                                                                   EL162
01585      EJECT                                                        EL162
01586  9900-ERROR-FORMAT.                                               EL162
01587      IF NOT EMI-ERRORS-COMPLETE                                   EL162
01588          MOVE LINK-001 TO PGM-NAME                                EL162
01589          EXEC CICS LINK                                           EL162
01590              PROGRAM (PGM-NAME)                                   EL162
01591              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL162
01592              LENGTH  (EMI-COMM-LENGTH)                            EL162
01593          END-EXEC.                                                   CL*12
01594                                                                   EL162
01595  9900-EXIT.                                                       EL162
01596      EXIT.                                                        EL162
01597                                                                   EL162
01598  9990-ABEND.                                                      EL162
01599      MOVE LINK-004 TO PGM-NAME.                                   EL162
01600      MOVE DFHEIBLK TO EMI-LINE1.                                  EL162
01601                                                                   EL162
01602      EXEC CICS LINK                                               EL162
01603          PROGRAM (PGM-NAME)                                          CL*12
01604          COMMAREA(EMI-LINE1)                                      EL162
01605          LENGTH  (72)                                                CL*12
01606      END-EXEC.                                                       CL*12
01607                                                                   EL162
01608      GO TO 8200-SEND-DATAONLY.                                    EL162
01609                                                                   EL162
01610  9995-SECURITY-VIOLATION.                                         EL162
01611                              COPY ELCSCTP.                        EL162
01612                                                                   EL162
