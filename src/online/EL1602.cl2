00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL1602
00003  PROGRAM-ID.                 EL1602.                                 LV007
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 05/16/95 15:31:54.                    CL**4
00007 *                            VMOD=2.007.                             CL**7
00008 *                                                                 EL1602
00008 *                                                                 EL1602
00009 *AUTHOR.        LOGIC, INC.                                          CL**4
00010 *               DALLAS, TEXAS.                                       CL**4
00011                                                                   EL1602
00024 *REMARKS. TRANSACTION EX34 - CLAIM DISPLAY.                          CL**3
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00025      EJECT                                                        EL1602
00026  ENVIRONMENT DIVISION.                                            EL1602
00027  DATA DIVISION.                                                   EL1602
00028  WORKING-STORAGE SECTION.                                         EL1602
00029  77  FILLER  PIC X(32)  VALUE '********************************'. EL1602
00030  77  FILLER  PIC X(32)  VALUE '*   EL1602 WORKING STORAGE     *'. EL1602
00031  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.007 **********'.    CL**7
00032                                                                      CL**4
00033  01  LCP-TIME-OF-DAY-XX.                                             CL**4
00034      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**4
00035      05  FILLER                    PIC 99.                           CL**4
00036  01  LCP-CICS-TIME                 PIC 9(15).                        CL**4
00037                                                                   EL1602
00038  01  WS-TS-AREA.                                                  EL1602
00039      12  FILLER                  PIC X(145).                         CL**5
00040      12  WS-TS-CLAIM             PIC X(7).                        EL1602
00041      12  FILLER                  PIC X(3).                        EL1602
00042      12  WS-TS-TYPE              PIC X.                           EL1602
00043      12  FILLER                  PIC X(3).                        EL1602
00044      12  WS-TS-CERT              PIC X(10).                       EL1602
00045      12  FILLER                  PIC X(3).                        EL1602
00046      12  WS-TS-CERT-SFX          PIC X.                           EL1602
00047      12  FILLER                  PIC X(3).                        EL1602
00048      12  WS-TS-CARR              PIC X.                           EL1602
00049      12  FILLER                  PIC X(3).                        EL1602
00050      12  WS-TS-STATUS            PIC X.                           EL1602
00051      12  FILLER                  PIC X(10).                       EL1602
00052      12  WS-TS-FILE              PIC X(4).                        EL1602
00053      12  FILLER                  PIC X(3).                        EL1602
00054      12  WS-TS-CCN               PIC X(16).                          CL**5
00055      12  FILLER                  PIC X(3).                           CL**5
00056      12  WS-TS-LNAME             PIC X(15).                       EL1602
00057      12  FILLER                  PIC X(3).                        EL1602
00058      12  WS-TS-FNAME             PIC X(15).                       EL1602
00059      12  FILLER                  PIC X(3).                        EL1602
00060      12  WS-TS-MINIT             PIC X.                           EL1602
00061      12  FILLER                  PIC X(608).                         CL**5
00062                                                                   EL1602
00063      EJECT                                                        EL1602
00064  01  WS-HEADING1.                                                 EL1602
00065      12  FILLER                  PIC X(24)   VALUE SPACES.        EL1602
00066      12  WS-H1-TITLE             PIC X(18)   VALUE                EL1602
00067          'CLAIM AUDIT REPORT'.                                    EL1602
00068      12  FILLER                  PIC X(12)   VALUE SPACES.        EL1602
00069      12  WS-H1-REPORT-NUMBER     PIC X(7)    VALUE 'EL -160'.     EL1602
00070      12  FILLER                  PIC X(71)   VALUE SPACES.        EL1602
00071                                                                   EL1602
00072  01  WS-HEADING2.                                                 EL1602
00073      12  FILLER                  PIC X(5)    VALUE SPACES.        EL1602
00074      12  FILLER                  PIC X(50)   VALUE                EL1602
00075          'CAR CLAIM     CERT      TYPE   STATUS   FILE  NAME'.       CL**4
00076      12  FILLER                  PIC X(77)   VALUE SPACES.        EL1602
00077                                                                   EL1602
00078  01  WS-DETAIL1.                                                  EL1602
00079      12  FILLER                  PIC X(6)    VALUE SPACES.        EL1602
00080      12  WS-D1-CARR              PIC X       VALUE SPACES.        EL1602
00081      12  FILLER                  PIC X       VALUE SPACES.        EL1602
00082      12  WS-D1-CLAIM             PIC X(7)    VALUE SPACES.        EL1602
00083      12  FILLER                  PIC X       VALUE SPACES.        EL1602
00084      12  WS-D1-CERT.                                              EL1602
00085          16  WS-D1-CERT-PRIME    PIC X(10)   VALUE SPACES.        EL1602
00086          16  WS-D1-CERT-SFX      PIC X       VALUE SPACES.        EL1602
00087      12  FILLER                  PIC X       VALUE SPACES.        EL1602
00088      12  WS-D1-TYPE              PIC X(6)    VALUE SPACES.        EL1602
00089      12  FILLER                  PIC XX      VALUE SPACES.        EL1602
00090      12  WS-D1-STATUS            PIC X(6)    VALUE SPACES.        EL1602
00091      12  FILLER                  PIC X(3)    VALUE SPACES.        EL1602
00092      12  WS-D1-FILE              PIC X(4)   VALUE SPACES.         EL1602
00093      12  FILLER                  PIC XX      VALUE SPACES.        EL1602
00094      12  WS-D1-NAME              PIC X(30)   VALUE SPACES.        EL1602
00095      12  FILLER                  PIC X(54)   VALUE SPACES.        EL1602
00096                                                                   EL1602
00097      EJECT                                                        EL1602
00098                                  COPY ELCREPT.                       CL**3
00099      EJECT                                                        EL1602
00100  01  WS-DATE-AREA.                                                EL1602
00101      12  SAVE-DATE               PIC X(8)   VALUE SPACES.         EL1602
00102      12  SAVE-BIN-DATE           PIC XX     VALUE SPACES.         EL1602
00103                                                                   EL1602
00104  01  LITERALS-NUMBERS.                                            EL1602
00105      12  LIT-SP                  PIC XX      VALUE 'SP'.          EL1602
00106      12  LIT-OB                  PIC XX      VALUE 'OB'.          EL1602
00107      12  LIT-OE                  PIC XX      VALUE 'OE'.          EL1602
00108      12  LIT-TYPE-L              PIC X(4)    VALUE 'LIFE'.        EL1602
00109      12  LIT-TYPE-A              PIC X(4)    VALUE 'A/H'.         EL1602
00110      12  LIT-CLOSED              PIC X(6)    VALUE 'CLOSED'.      EL1602
00111      12  LIT-OPEN                PIC X(6)    VALUE ' OPEN'.       EL1602
00112      12  LIT-SET-UP              PIC X(6)    VALUE 'SET UP'.      EL1602
00113      12  LIT-PMT                 PIC X(6)    VALUE 'PAYMNT'.      EL1602
00114      12  LIT-LETTER              PIC X(6)    VALUE 'LETTER'.      EL1602
00115      12  LIT-UPDATE              PIC X(6)    VALUE 'UPDATE'.      EL1602
00116      12  LIT-RESTORE             PIC X(6)    VALUE 'RESTOR'.      EL1602
00117      12  LIT-INC-CHG             PIC X(6)    VALUE 'INC DT'.      EL1602
00118      12  LIT-CONV                PIC X(6)    VALUE ' CONV'.       EL1602
00119      12  LIT-SIGN-OFF            PIC X(8)    VALUE 'EL005'.       EL1602
00120      12  LIT-HELP                PIC X(8)    VALUE 'EL010'.       EL1602
00121      12  LIT-MASTER              PIC X(8)    VALUE 'EL126'.       EL1602
00122      12  LIT-ACT                 PIC X(8)    VALUE 'EL142'.       EL1602
00123      12  LIT-PROG                PIC X(8)    VALUE 'EL1602'.      EL1602
00124      12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.     EL1602
00125      12  LIT-TRAN                PIC X(4)    VALUE 'EX34'.        EL1602
00126      12  LIT-MAP                 PIC X(4)    VALUE '160B'.        EL1602
00127      12  LIT-SCREEN              PIC X(4)    VALUE '160C'.        EL1602
00128      12  NUM-ONE                 PIC 99      VALUE 1.             EL1602
00129      12  NUM-TWENTY-FOUR         PIC 99      VALUE 24.            EL1602
00130      12  START-TRANS-ID          PIC X(4)    VALUE 'EX58'.        EL1602
00131      12  FILE-SWITCH             PIC X(4)    VALUE SPACES.        EL1602
00132      12  WS-PRINT-SW             PIC S9      VALUE +0.            EL1602
00133          88  PRINT-IN-PROCESS                VALUE +1.            EL1602
00134      12  WS-FIRST-TIME-SW        PIC XX      VALUE LOW-VALUES.    EL1602
00135          88  FIRST-TIME-THRU                 VALUE LOW-VALUES.    EL1602
00136      12  REPT-FILE-ID            PIC X(8)   VALUE 'ELREPT  '.     EL1602
00137      12  GETMAIN-SPACE           PIC X      VALUE SPACE.          EL1602
00138      12  MAX-TS-PAGES            PIC 9999    VALUE 251.              CL**3
00139      12  W-FILE-ID               PIC X(8)    VALUE 'ELMSTR'.         CL**4
00140                                                                   EL1602
00141  01  FILLER          COMP-3.                                      EL1602
00142      12  WS-LINE-COUNT           PIC S9(3)       VALUE +99.       EL1602
00143      12  WS-RECORD-COUNT         PIC S9(9)       VALUE ZERO.      EL1602
00144      12  WS-LINE-NUMBER          PIC S9(4)       VALUE +0.           CL**3
00145                                                                   EL1602
00146      EJECT                                                        EL1602
00147  01  EDIT-WORK-AREA.                                              EL1602
00148      12  CALL-PGM                PIC X(8).                        EL1602
00149      12  TRANS-ID                PIC X(4).                        EL1602
00150      12  CHECK-PFKEYS            PIC 99.                          EL1602
00151      12  CONV-COUNT              PIC 9(4).                        EL1602
00152      12  EDIT-COUNT              PIC ZZZ9.                           CL**3
00153      12  PI-KEY.                                                  EL1602
00154          16  CLAS-TERM           PIC X(4).                        EL1602
00155          16  CLAS-QUAL           PIC X(4).                        EL1602
00156      12  HOLD-PROC               PIC X(4).                        EL1602
00157      12  HOLD-PRI                PIC X.                           EL1602
00158      12  HOLD-SUPV               PIC X.                           EL1602
00159      12  HOLD-FILE               PIC X(4).                        EL1602
00160      12  DAYS-PAID               PIC ZZZZ9.                       EL1602
00161      12  PMTS-MADE               PIC ZZZZZ9.                      EL1602
00162      12  EDIT-DOLLARS-9          PIC ZZZZZZ.99.                   EL1602
00163                                                                      CL**3
00164  01  WS-OLD-CLAIM-RECORD         PIC X(350).                         CL**3
00165                                                                   EL1602
00166  01  CNTL-KEY.                                                    EL1602
00167      12  COMPANY-ID              PIC X(3).                        EL1602
00168      12  RECORD-TYPE             PIC X.                           EL1602
00169      12  CNTL-PROC               PIC X(4).                        EL1602
00170      12  SEQ-NO                  PIC 9(4)    COMP.                EL1602
00171                                                                   EL1602
00172  01  MSTR-KEY.                                                    EL1602
00173      12  MSTR-COMPANY-CODE       PIC X.                           EL1602
00174      12  MSTR-CARRIER            PIC X.                           EL1602
00175      12  MSTR-CLAIM-NO           PIC X(7).                        EL1602
00176      12  MSTR-CERT-NO            PIC X(11).                       EL1602
00177                                                                   EL1602
00178  01  ELACTQ-KEY.                                                  EL1602
00179      12  ACTQ-COMP-CD            PIC X.                           EL1602
00180      12  ACTQ-CARRIER            PIC X.                           EL1602
00181      12  ACTQ-CLAIM-NO           PIC X(7).                        EL1602
00182      12  ACTQ-CERT-NO            PIC X(11).                       EL1602
00183                                                                   EL1602
00184  01  TIME-UNFORMATTED.                                            EL1602
00185      12  UN-HOURS                PIC XX.                          EL1602
00186      12  UN-MINUTES              PIC XX.                          EL1602
00187      12  FILLER                  PIC X(4).                        EL1602
00188                                                                   EL1602
00189  01  TIME-FORMATTED.                                              EL1602
00190      12  FOR-HOURS               PIC XX.                          EL1602
00191      12  FILLER                  PIC X       VALUE '.'.           EL1602
00192      12  FOR-MINUTES             PIC XX.                          EL1602
00193      EJECT                                                        EL1602
00194  01  ERROR-NUMBERS.                                               EL1602
00195      12  ER-0000                 PIC X(4)    VALUE '0000'.        EL1602
00196      12  ER-0008                 PIC X(4)    VALUE '0008'.        EL1602
00197      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL1602
00198      12  ER-0042                 PIC X(4)    VALUE '0042'.        EL1602
00199      12  ER-0068                 PIC X(4)    VALUE '0068'.        EL1602
00200      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL1602
00201      12  ER-0190                 PIC X(4)    VALUE '0190'.        EL1602
00202      12  ER-0609                 PIC X(4)    VALUE '0609'.        EL1602
00203      12  ER-0130                 PIC X(4)    VALUE '0130'.        EL1602
00204      12  ER-0131                 PIC X(4)    VALUE '0131'.        EL1602
00205      12  ER-0192                 PIC X(4)    VALUE '0192'.        EL1602
00206      12  ER-0230                 PIC X(4)    VALUE '0230'.        EL1602
00207      12  ER-0273                 PIC X(4)    VALUE '0273'.        EL1602
00208      12  ER-0274                 PIC X(4)    VALUE '0274'.        EL1602
00209      12  ER-0276                 PIC X(4)    VALUE '0276'.        EL1602
00210      12  ER-0337                 PIC X(4)    VALUE '0337'.        EL1602
00211      12  ER-0412                 PIC X(4)    VALUE '0412'.        EL1602
00212      12  ER-0413                 PIC X(4)    VALUE '0413'.        EL1602
00213      12  ER-0515                 PIC X(4)    VALUE '0515'.        EL1602
00214      12  ER-0971                 PIC X(4)    VALUE '0971'.           CL**4
00215      12  ER-0972                 PIC X(4)    VALUE '0972'.           CL**4
00216      12  ER-2379                 PIC X(4)    VALUE '2379'.        EL1602
00217                                                                   EL1602
00218  01  ERROR-SWITCHES.                                              EL1602
00219      12  ERROR-SWITCH            PIC X.                           EL1602
00220          88  SCREEN-ERROR                    VALUE 'X'.           EL1602
00221      12  UPDATE-SWITCH           PIC X.                           EL1602
00222          88  NO-UPDATES                      VALUE SPACE.         EL1602
00223      12  KEY-SWITCH              PIC X.                           EL1602
00224          88  KEY-CHANGE                      VALUE 'X'.           EL1602
00225      12  WS-SAVE-PRINT-OPTION    PIC X       VALUE SPACE.         EL1602
00226                                                                   EL1602
00227  01  COMP-LENGTHS.                                                EL1602
00228      12  LIT-IC                  PIC S9(4)   COMP VALUE -1.       EL1602
00229      12  EL1602-LENGTH           PIC S9(4)   COMP VALUE +881.        CL**4
00230      12  MSTR-LENGTH             PIC S9(4)   COMP VALUE +350.        CL**3
00231      12  WS-SAVE-TS-COUNT        PIC S9(4)   COMP VALUE +0.       EL1602
00232      12  WS-TS-ITEM-NO           PIC S9(4)   COMP VALUE +0.       EL1602
00233      EJECT                                                        EL1602
00234                                  COPY ELCATTR.                       CL**3
00235      EJECT                                                        EL1602
00236                                  COPY ELCDATE.                       CL**3
00237      EJECT                                                        EL1602
00238                                  COPY ELCLOGOF.                      CL**3
00239      EJECT                                                           CL**4
00240                                  COPY ELCMSTR.                       CL**4
00241      EJECT                                                        EL1602
00242                                  COPY ELCAID.                        CL**3
00243  01  FILLER REDEFINES DFHAID.                                     EL1602
00244      12  FILLER                  PIC X(8).                        EL1602
00245      12  AID-KEYS OCCURS 24 TIMES.                                EL1602
00246          16  FILLER              PIC X.                           EL1602
00247      EJECT                                                        EL1602
00248                                  COPY EL160S.                        CL**3
00249      EJECT                                                        EL1602
00250                                  COPY ELCINTF.                       CL**3
00251      12  EL160-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.          EL1602
00252          16  PI-TS-COUNT         PIC S9(4)   COMP.                EL1602
00253          16  PI-TS-COUNT-1       PIC S9(4)   COMP.                EL1602
00254          16  PI-EL160-KEY        PIC X(8).                        EL1602
00255          16  PI-EL1602-KEY       PIC X(8).                        EL1602
00256          16  PI-PRINT-OPTION     PIC X.                           EL1602
00257          16  PI-FORMAT-OPTION    PIC X.                           EL1602
00258          16  PI-PRINT-ID         PIC X(4).                        EL1602
00259          16  PI-ALT-PRINT-ID     PIC X(4).                        EL1602
00260          16  PI-FILE-ID-IND      PIC X(1).                           CL**4
00261              88  PI-RETRIEVAL-FILE           VALUE 'R'.              CL**4
00262              88  PI-MASTER-FILE              VALUE 'M'.              CL**4
00263          16  FILLER              PIC X(609).                         CL**4
00264      EJECT                                                        EL1602
00265                                  COPY ELCEMIB.                       CL**3
00266      EJECT                                                        EL1602
00267  LINKAGE SECTION.                                                 EL1602
00268  01  DFHCOMMAREA                 PIC X(1024).                     EL1602
00269                                                                   EL1602
00270  01  CLAIM-MASTER-L              PIC X(0350).                        CL**4
00271      EJECT                                                           CL**3
00272                                  COPY ELCCNTL.                       CL**3
00273      EJECT                                                        EL1602
00274                                  COPY ELCACTQ.                       CL**3
00275      EJECT                                                        EL1602
00276                                  COPY ELCRETR.                       CL**4
00277      EJECT                                                           CL**4
00278  PROCEDURE DIVISION.                                              EL1602
00279                                                                   EL1602
00280      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL1602
00281      MOVE '5'                   TO DC-OPTION-CODE.                EL1602
00282      PERFORM 9800-CONVERT-DATE THRU 9800-CONVERT-DATE-EXIT.       EL1602
00283      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL1602
00284      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL1602
00285                                                                   EL1602
00286      IF EIBCALEN = ZERO                                           EL1602
00287          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL1602
00288                                                                   EL1602
00289      EXEC CICS HANDLE CONDITION                                   EL1602
00290          PGMIDERR (8820-XCTL-ERROR)                               EL1602
00291          ERROR (9990-ABEND)                                       EL1602
00292      END-EXEC.                                                    EL1602
00293                                                                   EL1602
00294      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL1602
00295      MOVE LIT-TRAN               TO TRANS-ID.                     EL1602
00296                                                                      CL**4
00297      IF PI-RETRIEVAL-FILE                                            CL**4
00298          MOVE 'ELRETR'           TO W-FILE-ID.                       CL**4
00299                                                                   EL1602
00300      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL1602
00301          MOVE LOW-VALUES         TO EL160BO                       EL1602
00302          MOVE ER-0008            TO EMI-ERROR                     EL1602
00303          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT    EL1602
00304          MOVE LIT-IC             TO NOSCRNL                       EL1602
00305          GO TO 8110-SEND-DATA.                                    EL1602
00306                                                                   EL1602
00307      MOVE EIBTRMID   TO CLAS-TERM.                                EL1602
00308      MOVE LIT-SCREEN TO CLAS-QUAL.                                EL1602
00309                                                                   EL1602
00310      IF LIT-PROG NOT = PI-CALLING-PROGRAM                         EL1602
00311          GO TO 0100-UPDATE-PI.                                    EL1602
00312                                                                   EL1602
00313      IF EIBAID = DFHCLEAR                                         EL1602
00314          GO TO 8200-RETURN-PRIOR.                                 EL1602
00315                                                                   EL1602
00316      EXEC CICS RECEIVE                                            EL1602
00317          MAP ('EL160B')                                           EL1602
00318          MAPSET ('EL160S')                                        EL1602
00319      END-EXEC.                                                    EL1602
00320                                                                   EL1602
00321      MOVE SPACES TO ERROR-SWITCHES.                               EL1602
00322                                                                   EL1602
00323      IF PFKEYBL GREATER THAN ZERO                                 EL1602
00324          PERFORM 0200-TRANS-PF THRU 0210-TRANS-PF-EXIT.           EL1602
00325                                                                   EL1602
00326      IF SCREEN-ERROR                                              EL1602
00327          MOVE ER-0008            TO EMI-ERROR                     EL1602
00328          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT    EL1602
00329          MOVE AL-UNBON           TO PFKEYBA                       EL1602
00330          MOVE LIT-IC             TO PFKEYBL                       EL1602
00331          GO TO 8110-SEND-DATA.                                    EL1602
00332                                                                   EL1602
00333      IF EIBAID = DFHPF1 OR DFHPF2                                 EL1602
00334          GO TO 1100-CHECK-PFKEYS.                                 EL1602
00335      IF EIBAID = DFHPF5 OR DFHPF6                                 EL1602
00336          GO TO 1100-CHECK-PFKEYS.                                 EL1602
00337      IF EIBAID = DFHPF3                                           EL1602
00338          GO TO 8200-RETURN-PRIOR.                                 EL1602
00339      IF EIBAID = DFHPF4                                           EL1602
00340          GO TO 8500-GET-ACT.                                      EL1602
00341      IF EIBAID = DFHPF7                                           EL1602
00342          GO TO 0500-CHECK-IN-PROGRESS.                            EL1602
00343      IF EIBAID = DFHPF12                                          EL1602
00344          GO TO 8300-GET-HELP.                                     EL1602
00345      IF EIBAID = DFHPF23                                          EL1602
00346          GO TO 8810-PF23-ENTERED.                                 EL1602
00347      IF EIBAID = DFHPF24                                          EL1602
00348          GO TO 8400-RETURN-MASTER.                                EL1602
00349                                                                   EL1602
00350      IF EIBAID = DFHENTER                                         EL1602
00351          NEXT SENTENCE                                            EL1602
00352      ELSE                                                         EL1602
00353          MOVE ER-0029            TO EMI-ERROR                     EL1602
00354          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT    EL1602
00355          MOVE AL-UNBON           TO PFKEYBA                       EL1602
00356          MOVE LIT-IC             TO PFKEYBL                       EL1602
00357          GO TO 8110-SEND-DATA.                                    EL1602
00358                                                                   EL1602
00359      PERFORM 4000-CHECK-UPDATE THRU 4000-CHECK-UPDATE-EXIT.       EL1602
00360      IF SCREEN-ERROR                                              EL1602
00361          GO TO 8110-SEND-DATA.                                    EL1602
00362                                                                   EL1602
00363      IF NOSCRNL NOT = ZEROS                                       EL1602
00364         GO TO 1000-BROWSE.                                        EL1602
00365                                                                   EL1602
00366      IF EIBAID = DFHPF1 OR DFHPF2                                 EL1602
00367          GO TO 1100-CHECK-PFKEYS.                                 EL1602
00368                                                                   EL1602
00369      MOVE LIT-IC TO NOSCRNL.                                      EL1602
00370      MOVE ER-0000 TO EMI-ERROR.                                   EL1602
00371      PERFORM 9900-ERROR-FORMAT                                    EL1602
00372              THRU 9900-ERROR-FORMAT-EXIT.                         EL1602
00373      GO TO 8110-SEND-DATA.                                        EL1602
00374      EJECT                                                        EL1602
00375  0100-UPDATE-PI.                                                  EL1602
00376      IF PI-RETURN-TO-PROGRAM = LIT-PROG                           EL1602
00377          GO TO 0110-UPDATE-UP.                                    EL1602
00378                                                                   EL1602
00379      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.           EL1602
00380      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.           EL1602
00381      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.           EL1602
00382      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.           EL1602
00383      MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.           EL1602
00384      MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.           EL1602
00385      MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.         EL1602
00386      MOVE LIT-PROG               TO PI-CALLING-PROGRAM.           EL1602
00387      MOVE ZEROS                  TO PI-TS-COUNT.                  EL1602
00388      GO TO 1100-CHECK-PFKEYS.                                     EL1602
00389                                                                   EL1602
00390  0110-UPDATE-UP.                                                  EL1602
00391      MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM.           EL1602
00392      MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM.         EL1602
00393      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1.           EL1602
00394      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2.           EL1602
00395      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3.           EL1602
00396      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4.           EL1602
00397      MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5.           EL1602
00398      MOVE SPACES                 TO PI-SAVED-PROGRAM-6.           EL1602
00399                                                                   EL1602
00400      EXEC CICS HANDLE CONDITION                                   EL1602
00401          QIDERR       (0130-TS-ERROR)                             EL1602
00402          ITEMERR      (0130-TS-ERROR)                             EL1602
00403      END-EXEC.                                                    EL1602
00404                                                                   EL1602
00405      EXEC CICS READQ TS                                           EL1602
00406          QUEUE      (PI-KEY)                                      EL1602
00407          INTO       (PROGRAM-INTERFACE-BLOCK)                     EL1602
00408          LENGTH     (PI-COMM-LENGTH)                              EL1602
00409      END-EXEC.                                                    EL1602
00410                                                                   EL1602
00411      EXEC CICS DELETEQ TS                                         EL1602
00412          QUEUE      (PI-KEY)                                      EL1602
00413      END-EXEC.                                                    EL1602
00414                                                                   EL1602
00415      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.           EL1602
00416      MOVE LIT-IC                 TO NOSCRNL.                      EL1602
00417      GO TO 8100-SEND-MAP.                                         EL1602
00418                                                                   EL1602
00419  0130-TS-ERROR.                                                   EL1602
00420      MOVE LOW-VALUES             TO EL160BO.                      EL1602
00421      MOVE ER-0192                TO EMI-ERROR.                    EL1602
00422      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.       EL1602
00423      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL1602
00424      MOVE EMI-MESSAGE-AREA (1)   TO MSGBO.                        EL1602
00425      GO TO 8100-SEND-MAP.                                         EL1602
00426                                                                   EL1602
00427  0200-TRANS-PF.                                                   EL1602
00428      IF EIBAID NOT = DFHENTER                                     EL1602
00429          MOVE 'X'                TO ERROR-SWITCH                  EL1602
00430          GO TO 0210-TRANS-PF-EXIT.                                EL1602
00431                                                                   EL1602
00432      IF PFKEYBI NOT NUMERIC                                       EL1602
00433          MOVE 'X'                TO ERROR-SWITCH                  EL1602
00434          GO TO 0210-TRANS-PF-EXIT.                                EL1602
00435                                                                   EL1602
00436      MOVE PFKEYBI TO CHECK-PFKEYS.                                EL1602
00437                                                                   EL1602
00438      IF CHECK-PFKEYS LESS THAN NUM-ONE                            EL1602
00439        OR                                                         EL1602
00440         CHECK-PFKEYS GREATER THAN NUM-TWENTY-FOUR                 EL1602
00441          MOVE 'X'                TO ERROR-SWITCH                  EL1602
00442          GO TO 0210-TRANS-PF-EXIT.                                EL1602
00443                                                                   EL1602
00444      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL1602
00445                                                                   EL1602
00446  0210-TRANS-PF-EXIT.                                              EL1602
00447      EXIT.                                                        EL1602
00448      EJECT                                                        EL1602
00449  0500-CHECK-IN-PROGRESS.                                          EL1602
00450      EXEC CICS  HANDLE CONDITION                                  EL1602
00451             NOTFND   (0510-WRITE-INITIAL-TRAILER)                 EL1602
00452      END-EXEC.                                                    EL1602
00453                                                                   EL1602
00454      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL1602
00455      MOVE 'RF'                   TO  RF-RECORD-ID.                EL1602
00456      MOVE '2'                    TO  RF-RECORD-TYPE.              EL1602
00457      MOVE 'EL160'                TO  RF-REPORT-ID.                EL1602
00458      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL1602
00459                                                                   EL1602
00460      EXEC CICS READ                                               EL1602
00461          DATASET    (REPT-FILE-ID)                                EL1602
00462          INTO       (REPORT-SAVE-FILE)                            EL1602
00463          RIDFLD     (RF-CONTROL-PRIMARY)                          EL1602
00464      END-EXEC.                                                       CL**4
00465                                                                   EL1602
00466      MOVE ER-0000                TO EMI-ERROR                     EL1602
00467      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT        EL1602
00468      MOVE -1                     TO PFKEYBL                       EL1602
00469      GO TO 8110-SEND-DATA.                                        EL1602
00470                                                                   EL1602
00471  0510-WRITE-INITIAL-TRAILER.                                      EL1602
00472      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL1602
00473      MOVE 'RF'                   TO  RF-RECORD-ID.                EL1602
00474      MOVE '2'                    TO  RF-RECORD-TYPE.              EL1602
00475      MOVE 'EL160'                TO  RF-REPORT-ID.                EL1602
00476      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL1602
00477                                                                   EL1602
00478      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL1602
00479      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**4
00480      END-EXEC                                                        CL**4
00481      EXEC CICS FORMATTIME                                            CL**4
00482                ABSTIME(LCP-CICS-TIME)                                CL**4
00483                TIME(LCP-TIME-OF-DAY-XX)                              CL**4
00484      END-EXEC                                                        CL**4
00485      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**4
00486      MOVE 'STARTED'              TO  RF-CURRENT-DATE.             EL1602
00487                                                                   EL1602
00488      EXEC CICS WRITE                                              EL1602
00489          DATASET (REPT-FILE-ID)                                   EL1602
00490          FROM    (REPORT-SAVE-FILE)                               EL1602
00491          RIDFLD  (RF-CONTROL-PRIMARY)                             EL1602
00492      END-EXEC.                                                       CL**4
00493                                                                   EL1602
00494  0520-DELETE-REC.                                                 EL1602
00495      MOVE 1                      TO RF-LINE-NUMBER.               EL1602
00496      EXEC CICS  HANDLE CONDITION                                  EL1602
00497             NOTFND   (0540-DELETE-REC)                            EL1602
00498      END-EXEC.                                                    EL1602
00499                                                                   EL1602
00500  0530-DELETE-1.                                                   EL1602
00501      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.                EL1602
00502      MOVE 'RF'                   TO RF-RECORD-ID.                 EL1602
00503      MOVE '1'                    TO RF-RECORD-TYPE.               EL1602
00504      MOVE 'EL160'                TO RF-REPORT-ID.                 EL1602
00505                                                                   EL1602
00506      EXEC CICS DELETE                                             EL1602
00507          DATASET (REPT-FILE-ID)                                   EL1602
00508          RIDFLD  (RF-CONTROL-PRIMARY)                             EL1602
00509          KEYLENGTH (11)                                           EL1602
00510      END-EXEC.                                                    EL1602
00511                                                                   EL1602
00512      ADD 1 TO RF-LINE-NUMBER.                                     EL1602
00513      GO TO 0530-DELETE-1.                                         EL1602
00514                                                                   EL1602
00515  0540-DELETE-REC.                                                 EL1602
00516      EXEC CICS  HANDLE CONDITION                                  EL1602
00517             NOTFND   (0560-READ-TEMP-STORAGE)                     EL1602
00518      END-EXEC.                                                    EL1602
00519                                                                   EL1602
00520  0550-DELETE-2.                                                   EL1602
00521      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.                EL1602
00522      MOVE 'RF'                   TO RF-RECORD-ID.                 EL1602
00523      MOVE '2'                    TO RF-RECORD-TYPE.               EL1602
00524      MOVE 'EL160'                TO RF-REPORT-ID.                 EL1602
00525                                                                   EL1602
00526      EXEC CICS DELETE                                             EL1602
00527          DATASET (REPT-FILE-ID)                                   EL1602
00528          RIDFLD  (RF-CONTROL-PRIMARY)                             EL1602
00529          KEYLENGTH (11)                                           EL1602
00530      END-EXEC.                                                    EL1602
00531                                                                   EL1602
00532      ADD 1 TO RF-LINE-NUMBER.                                     EL1602
00533      GO TO 0550-DELETE-2.                                         EL1602
00534                                                                   EL1602
00535  EJECT                                                            EL1602
00536  0560-READ-TEMP-STORAGE.                                          EL1602
00537      EXEC CICS HANDLE CONDITION                                   EL1602
00538           ITEMERR (0570-DELETE-INITIAL-2)                         EL1602
00539      END-EXEC.                                                    EL1602
00540                                                                   EL1602
00541      MOVE +0 TO WS-TS-ITEM-NO.                                    EL1602
00542                                                                   EL1602
00543  0565-READ-NEXT-TEMP.                                             EL1602
00544      ADD +1 TO WS-TS-ITEM-NO.                                     EL1602
00545                                                                   EL1602
00546      EXEC CICS READQ TS                                           EL1602
00547           QUEUE    (PI-EL1602-KEY)                                EL1602
00548           INTO     (WS-TS-AREA)                                   EL1602
00549           LENGTH   (EL1602-LENGTH)                                EL1602
00550           ITEM     (WS-TS-ITEM-NO)                                EL1602
00551      END-EXEC.                                                    EL1602
00552                                                                   EL1602
00553      IF WS-LINE-COUNT GREATER THAN +55                            EL1602
00554         PERFORM 0800-PRINT-HEADINGS THRU 0800-EXIT.               EL1602
00555                                                                   EL1602
00556      MOVE WS-TS-CARR             TO WS-D1-CARR.                   EL1602
00557      MOVE WS-TS-CLAIM            TO WS-D1-CLAIM.                  EL1602
00558      MOVE WS-TS-CERT             TO WS-D1-CERT-PRIME.             EL1602
00559      MOVE WS-TS-CERT-SFX         TO WS-D1-CERT-SFX.               EL1602
00560                                                                   EL1602
121802     EVALUATE TRUE

00561         WHEN WS-TS-TYPE = PI-LIFE-OVERRIDE-L1
00562            MOVE PI-LIFE-OVERRIDE-L6
                                       TO WS-D1-TYPE

121802        WHEN WS-TS-TYPE = PI-AH-OVERRIDE-L1
00564            MOVE PI-AH-OVERRIDE-L6
                                       TO WS-D1-TYPE

121802        WHEN WS-TS-TYPE = 'I'
121802           MOVE '  IU  '         TO WS-D1-TYPE

121802        WHEN WS-TS-TYPE = 'G'
121802           MOVE ' GAP  '         TO WS-D1-TYPE
052614
052614        WHEN WS-TS-TYPE = 'F'
052614           MOVE ' FAM  '         TO WS-D1-TYPE
080322        WHEN WS-TS-TYPE = 'B'
080322           MOVE ' BRV  '         TO WS-D1-TYPE
080322        WHEN WS-TS-TYPE = 'H'
080322           MOVE ' HOSP '         TO WS-D1-TYPE
100518
100518        WHEN WS-TS-TYPE = 'O'
100518           MOVE ' OTH  '         TO WS-D1-TYPE

121802     END-EVALUATE
00565                                                                   EL1602
00566      IF WS-TS-STATUS = 'O'                                        EL1602
00567         MOVE ' OPEN '            TO WS-D1-STATUS                  EL1602
00568      ELSE                                                         EL1602
00569         MOVE 'CLOSED'            TO WS-D1-STATUS.                 EL1602
00570                                                                   EL1602
00571      MOVE WS-TS-FILE             TO WS-D1-FILE.                   EL1602
00572                                                                   EL1602
00573      MOVE WS-TS-LNAME            TO WS-D1-NAME.                   EL1602
00574                                                                   EL1602
00575      MOVE WS-DETAIL1             TO RF-DATA-133.                  EL1602
00576      MOVE ' '                    TO RF-CTL-CHAR-133.              EL1602
00577      PERFORM 0600-PRT-LINE THRU 0600-EXIT.                        EL1602
00578                                                                   EL1602
00579      GO TO 0565-READ-NEXT-TEMP.                                   EL1602
00580                                                                   EL1602
00581  0570-DELETE-INITIAL-2.                                           EL1602
00582      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.                EL1602
00583      MOVE 'RF'                   TO RF-RECORD-ID.                 EL1602
00584      MOVE '2'                    TO RF-RECORD-TYPE.               EL1602
00585      MOVE 'EL160'                TO RF-REPORT-ID.                 EL1602
00586      MOVE ZEROS                  TO RF-LINE-NUMBER.               EL1602
00587                                                                   EL1602
00588      EXEC CICS DELETE                                             EL1602
00589           DATASET     (REPT-FILE-ID)                              EL1602
00590           RIDFLD      (RF-CONTROL-PRIMARY)                        EL1602
00591           KEYLENGTH   (11)                                        EL1602
00592      END-EXEC.                                                    EL1602
00593                                                                   EL1602
00594  0580-WRITE-TRAILER.                                              EL1602
00595      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.                EL1602
00596      MOVE 'RF'                   TO RF-RECORD-ID.                 EL1602
00597      MOVE 'EL160'                TO RF-REPORT-ID.                 EL1602
00598      MOVE '2'                    TO RF-RECORD-TYPE.               EL1602
00599      ADD +1                      TO WS-LINE-NUMBER.               EL1602
00600      MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.               EL1602
00601      MOVE SPACES                 TO RF-TRAILER-RECORD.            EL1602
00602                                                                      CL**4
00603      EXEC CICS ASKTIME                                               CL**4
00604          ABSTIME(LCP-CICS-TIME)                                      CL**4
00605      END-EXEC.                                                       CL**4
00606                                                                      CL**4
00607      EXEC CICS FORMATTIME                                            CL**4
00608          ABSTIME(LCP-CICS-TIME)                                      CL**4
00609          TIME(LCP-TIME-OF-DAY-XX)                                    CL**4
00610      END-EXEC.                                                       CL**4
00611                                                                      CL**4
00612      MOVE LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                   CL**4
00613      MOVE SAVE-DATE              TO RF-CURRENT-DATE.              EL1602
00614                                                                   EL1602
00615      EXEC CICS WRITE                                              EL1602
00616          DATASET (REPT-FILE-ID)                                   EL1602
00617          FROM    (REPORT-SAVE-FILE)                               EL1602
00618          RIDFLD  (RF-CONTROL-PRIMARY)                             EL1602
00619      END-EXEC.                                                       CL**4
00620                                                                   EL1602
00621      MOVE ER-0000                TO EMI-ERROR.                    EL1602
00622      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.       EL1602
00623      MOVE -1                     TO PFKEYBL.                      EL1602
00624      GO TO 8110-SEND-DATA.                                        EL1602
00625                                                                   EL1602
00626  0600-PRT-LINE.                                                   EL1602
00627      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.                EL1602
00628      MOVE 'RF'                   TO RF-RECORD-ID.                 EL1602
00629      MOVE '1'                    TO RF-RECORD-TYPE.               EL1602
00630      MOVE 'EL160'                TO RF-REPORT-ID.                 EL1602
00631      ADD +1                      TO WS-LINE-NUMBER.               EL1602
00632      MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.               EL1602
00633                                                                   EL1602
00634      EXEC CICS WRITE                                              EL1602
00635          DATASET     (REPT-FILE-ID)                                  CL**4
00636          FROM        (REPORT-SAVE-FILE)                              CL**4
00637          RIDFLD      (RF-CONTROL-PRIMARY)                            CL**4
00638      END-EXEC.                                                    EL1602
00639                                                                   EL1602
00640      ADD +1 TO WS-LINE-COUNT.                                     EL1602
00641  0600-EXIT.                                                       EL1602
00642      EXIT.                                                        EL1602
00643                                                                   EL1602
00644  0800-PRINT-HEADINGS.                                             EL1602
00645      MOVE WS-HEADING1            TO RF-DATA-133.                  EL1602
00646      MOVE '1'                    TO RF-CTL-CHAR-133.              EL1602
00647      PERFORM 0600-PRT-LINE THRU 0600-EXIT.                        EL1602
00648                                                                   EL1602
00649      MOVE WS-HEADING2            TO RF-DATA-133.                  EL1602
00650      MOVE '-'                    TO RF-CTL-CHAR-133.              EL1602
00651      PERFORM 0600-PRT-LINE THRU 0600-EXIT.                        EL1602
00652                                                                   EL1602
00653      MOVE SPACES                 TO RF-DATA-133.                  EL1602
00654      MOVE '0'                    TO RF-CTL-CHAR-133.              EL1602
00655      PERFORM 0600-PRT-LINE THRU 0600-EXIT.                        EL1602
00656                                                                   EL1602
00657      MOVE +6                     TO WS-LINE-COUNT.                EL1602
00658                                                                   EL1602
00659  0800-EXIT.                                                       EL1602
00660      EXIT.                                                        EL1602
00661      EJECT                                                        EL1602
00662                                                                   EL1602
00663  1000-BROWSE.                                                     EL1602
00664                                                                      CL**3
00665      IF NOSCRNL = ZEROS                                           EL1602
00666         GO TO 1100-CHECK-PFKEYS.                                  EL1602
00667                                                                   EL1602
00668      EXEC CICS BIF DEEDIT                                            CL**3
00669          FIELD   (NOSCRNI)                                           CL**3
00670          LENGTH  (4)                                                 CL**3
00671      END-EXEC.                                                       CL**3
00672                                                                      CL**3
00673      IF (PI-PROCESSOR-ID = 'LGXX') OR                                CL**6
00674         (PI-COMPANY-ID   = 'AIG' OR 'AUK')                           CL**6
00675          MOVE 2501               TO  MAX-TS-PAGES.                   CL**3
00676                                                                      CL**3
00677      IF (NOSCRNI GREATER '00' AND LESS MAX-TS-PAGES)                 CL**3
00678         NEXT SENTENCE                                             EL1602
00679      ELSE                                                         EL1602
00680         MOVE ER-0515             TO EMI-ERROR                     EL1602
00681         MOVE AL-UNBON            TO NOSCRNA                       EL1602
00682         MOVE -1                  TO NOSCRNL                       EL1602
00683         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT     EL1602
00684         GO TO 8110-SEND-DATA.                                     EL1602
00685                                                                   EL1602
00686      IF NOSCRNI GREATER THAN PI-TS-COUNT-1                        EL1602
00687         MOVE ER-0515             TO EMI-ERROR                     EL1602
00688         MOVE AL-UNBON            TO NOSCRNA                       EL1602
00689         MOVE -1                  TO NOSCRNL                       EL1602
00690         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT     EL1602
00691         GO TO 8110-SEND-DATA.                                     EL1602
00692                                                                   EL1602
00693      MOVE NOSCRNI                TO PI-TS-COUNT                   EL1602
00694      GO TO 1150-GET-TEMP-STOR.                                    EL1602
00695                                                                   EL1602
00696  1100-CHECK-PFKEYS.                                               EL1602
00697 ********MODIFICATION TO ENABLE PRINTING OF AUDIT*******           EL1602
00698 *                                                                 EL1602
00699 ******************************************************************EL1602
00700 *       PI-PRINT-OPTION MUST BE 'N' FOR CARRIER AND ACCOUNT      *EL1602
00701 *       AND MUST HAVE AN ALTERNATE PRINTER-ID.                   *EL1602
00702 ******************************************************************EL1602
00703                                                                      CL**4
00704      IF PI-RETRIEVAL-FILE                                            CL**4
00705          IF EIBAID = DFHPF5                                          CL**4
00706              MOVE ER-0971        TO EMI-ERROR                        CL**4
00707              PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT   CL**4
00708              MOVE -1             TO PFKEYBL                          CL**4
00709              GO TO 8110-SEND-DATA                                    CL**4
00710          ELSE                                                        CL**4
00711              IF EIBAID = DFHPF6                                      CL**4
00712                  MOVE ER-0972    TO EMI-ERROR                        CL**4
00713                  PERFORM 9900-ERROR-FORMAT                           CL**4
00714                      THRU 9900-ERROR-FORMAT-EXIT                     CL**4
00715                  MOVE -1         TO PFKEYBL                          CL**4
00716                  GO TO 8110-SEND-DATA.                               CL**4
00717                                                                   EL1602
00718      IF PI-CARRIER-SECURITY GREATER THAN SPACES OR                EL1602
00719         PI-ACCOUNT-SECURITY GREATER THAN SPACES                   EL1602
00720            IF EIBAID = DFHPF5                                     EL1602
00721               IF PI-ALT-PRINT-ID GREATER THAN SPACES              EL1602
00722                  NEXT SENTENCE                                    EL1602
00723               ELSE                                                EL1602
00724                  MOVE ER-2379    TO EMI-ERROR                     EL1602
00725                  PERFORM 9900-ERROR-FORMAT THRU                   EL1602
00726                  9900-ERROR-FORMAT-EXIT                           EL1602
00727                  MOVE -1         TO PFKEYBL                       EL1602
00728                  GO TO 8110-SEND-DATA.                            EL1602
00729                                                                   EL1602
00730      IF EIBAID = DFHPF5                                           EL1602
00731          PERFORM 1200-PROCESS-OPTIONS                             EL1602
00732             THRU 1299-EXIT                                        EL1602
00733          GO TO 1150-GET-TEMP-STOR.                                EL1602
00734                                                                   EL1602
00735      IF EIBAID = DFHPF6 AND                                       EL1602
00736         PI-PRINT-OPTION = 'N'                                     EL1602
00737          MOVE ER-0609            TO EMI-ERROR                     EL1602
00738          PERFORM 9900-ERROR-FORMAT                                EL1602
00739                 THRU 9900-ERROR-FORMAT-EXIT                       EL1602
00740          MOVE -1                 TO PFKEYBL                       EL1602
00741          GO TO 8110-SEND-DATA.                                    EL1602
00742                                                                   EL1602
00743      IF EIBAID = DFHPF6                                           EL1602
00744          MOVE PI-TS-COUNT        TO WS-SAVE-TS-COUNT              EL1602
00745          MOVE +1                 TO PI-TS-COUNT  WS-PRINT-SW      EL1602
00746          PERFORM 1200-PROCESS-OPTIONS THRU 1299-EXIT              EL1602
00747             UNTIL PI-TS-COUNT GREATER PI-TS-COUNT-1               EL1602
00748          MOVE WS-SAVE-TS-COUNT   TO PI-TS-COUNT                   EL1602
00749          GO TO 1150-GET-TEMP-STOR.                                EL1602
00750 *******************************************************           EL1602
00751                                                                   EL1602
00752      IF EIBAID = DFHPF2                                           EL1602
00753         SUBTRACT +1 FROM PI-TS-COUNT                              EL1602
00754      ELSE                                                         EL1602
00755         ADD +1        TO PI-TS-COUNT.                             EL1602
00756                                                                   EL1602
00757  1150-GET-TEMP-STOR.                                              EL1602
00758      MOVE SPACE TO ERROR-SWITCH.                                  EL1602
00759                                                                   EL1602
00760      IF PI-TS-COUNT LESS THAN NUM-ONE                             EL1602
00761          MOVE 'X'                TO ERROR-SWITCH                  EL1602
00762          MOVE ER-0131         TO EMI-ERROR                        EL1602
00763          PERFORM 9900-ERROR-FORMAT                                EL1602
00764              THRU 9900-ERROR-FORMAT-EXIT                          EL1602
00765          MOVE NUM-ONE            TO PI-TS-COUNT.                  EL1602
00766                                                                   EL1602
00767      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.           EL1602
00768                                                                   EL1602
00769      IF SCREEN-ERROR                                              EL1602
00770          MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO            EL1602
00771          MOVE EMI-MESSAGE-AREA (1) TO MSGBO                       EL1602
00772      ELSE                                                         EL1602
00773          IF SCNERRI = SPACES OR LOW-VALUES                        EL1602
00774              MOVE SCNERRI TO PI-LAST-ERROR-NO                     EL1602
00775              MOVE SPACES TO MSGBO                                 EL1602
00776          ELSE                                                     EL1602
00777              MOVE SCNERRI TO EMI-ERROR PI-LAST-ERROR-NO           EL1602
00778              PERFORM 9900-ERROR-FORMAT                            EL1602
00779                  THRU 9900-ERROR-FORMAT-EXIT                      EL1602
00780              MOVE EMI-MESSAGE-AREA (1) TO MSGBO.                  EL1602
00781                                                                   EL1602
00782      MOVE LIT-IC                 TO NOSCRNL.                      EL1602
00783      GO TO 8100-SEND-MAP.                                         EL1602
00784      EJECT                                                        EL1602
00785                                                                   EL1602
00786  1200-PROCESS-OPTIONS.                                            EL1602
00787      MOVE SPACE TO ERROR-SWITCH.                                  EL1602
00788      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.           EL1602
00789                                                                   EL1602
00790      IF SCREEN-ERROR                                              EL1602
00791          MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO            EL1602
00792          MOVE EMI-MESSAGE-AREA (1) TO MSGBO                       EL1602
00793      ELSE                                                         EL1602
00794          IF SCNERRI = SPACES OR LOW-VALUES                        EL1602
00795              MOVE SCNERRI TO PI-LAST-ERROR-NO                     EL1602
00796              MOVE SPACES TO MSGBO                                 EL1602
00797          ELSE                                                     EL1602
00798              MOVE SCNERRI TO EMI-ERROR PI-LAST-ERROR-NO           EL1602
00799              PERFORM 9900-ERROR-FORMAT                            EL1602
00800                  THRU 9900-ERROR-FORMAT-EXIT                      EL1602
00801              MOVE EMI-MESSAGE-AREA (1) TO MSGBO.                  EL1602
00802                                                                   EL1602
00803      MOVE PIKEYI                 TO PI-CONTROL-IN-PROGRESS.       EL1602
00804                                                                   EL1602
00805      IF PI-PRINT-OPTION = SPACE                                   EL1602
00806          MOVE 'N' TO PI-PRINT-OPTION.                             EL1602
00807                                                                   EL1602
00808      IF PI-FORMAT-OPTION = SPACE                                  EL1602
00809          MOVE 'P' TO PI-FORMAT-OPTION.                            EL1602
00810                                                                   EL1602
00811      IF PI-PRINT-OPTION = 'N'                                     EL1602
00812        AND EIBAID NOT = DFHPF6                                    EL1602
00813          GO TO 1260-START-PRINTER.                                EL1602
00814                                                                   EL1602
00815      EXEC CICS HANDLE CONDITION                                   EL1602
00816          NOTFND(1230-CREATE-NEW-ACTQ)                             EL1602
00817          END-EXEC.                                                EL1602
00818                                                                   EL1602
00819      MOVE PI-COMPANY-CD          TO ACTQ-COMP-CD.                 EL1602
00820      MOVE PI-CARRIER             TO ACTQ-CARRIER.                 EL1602
00821      MOVE PI-CLAIM-NO            TO ACTQ-CLAIM-NO.                EL1602
00822      MOVE PI-CERT-NO             TO ACTQ-CERT-NO.                 EL1602
00823                                                                   EL1602
00824      MOVE 'ACTQ'                 TO FILE-SWITCH.                  EL1602
00825                                                                   EL1602
00826      EXEC CICS READ                                               EL1602
00827          UPDATE                                                   EL1602
00828          DATASET  ('ELACTQ')                                      EL1602
00829          SET      (ADDRESS OF ACTIVITY-QUE)                          CL**4
00830          RIDFLD   (ELACTQ-KEY)                                    EL1602
00831      END-EXEC.                                                       CL**4
00832                                                                   EL1602
00833      IF PI-FORMAT-OPTION = 'F'                                    EL1602
00834          MOVE '1'                TO AQ-PENDING-STATUS-FLAG        EL1602
00835      ELSE                                                         EL1602
00836          MOVE '2'                TO AQ-PENDING-STATUS-FLAG.       EL1602
00837                                                                   EL1602
00838      EXEC CICS REWRITE                                            EL1602
00839          DATASET ('ELACTQ')                                       EL1602
00840          FROM    (ACTIVITY-QUE)                                   EL1602
00841      END-EXEC.                                                       CL**4
00842                                                                   EL1602
00843      IF PRINT-IN-PROCESS                                          EL1602
00844          ADD +1 TO PI-TS-COUNT.                                   EL1602
00845                                                                   EL1602
00846      GO TO 1299-EXIT.                                             EL1602
00847                                                                   EL1602
00848  1230-CREATE-NEW-ACTQ.                                            EL1602
00849      EXEC CICS GETMAIN                                            EL1602
00850          SET     (ADDRESS OF ACTIVITY-QUE)                           CL**4
00851          LENGTH  (60)                                             EL1602
00852          INITIMG (GETMAIN-SPACE)                                  EL1602
00853      END-EXEC.                                                       CL**4
00854                                                                   EL1602
00855      MOVE 'AQ'                   TO AQ-RECORD-ID.                 EL1602
00856      MOVE PI-COMPANY-CD          TO AQ-COMPANY-CD.                EL1602
00857      MOVE PI-CARRIER             TO AQ-CARRIER.                   EL1602
00858      MOVE PI-CLAIM-NO            TO AQ-CLAIM-NO.                  EL1602
00859      MOVE PI-CERT-NO             TO AQ-CERT-NO.                   EL1602
00860                                                                   EL1602
00861      IF PI-FORMAT-OPTION = 'F'                                    EL1602
00862          MOVE '1'                TO AQ-PENDING-STATUS-FLAG        EL1602
00863      ELSE                                                         EL1602
00864          MOVE '2'                TO AQ-PENDING-STATUS-FLAG.       EL1602
00865                                                                   EL1602
00866      MOVE +0                     TO AQ-PAYMENT-COUNTER.           EL1602
00867      MOVE 'ACTQ'                 TO FILE-SWITCH.                  EL1602
00868                                                                   EL1602
00869      EXEC CICS WRITE                                              EL1602
00870          DATASET ('ELACTQ')                                       EL1602
00871          FROM    (ACTIVITY-QUE)                                   EL1602
00872          RIDFLD  (AQ-CONTROL-PRIMARY)                             EL1602
00873      END-EXEC.                                                       CL**4
00874                                                                   EL1602
00875      IF PRINT-IN-PROCESS                                          EL1602
00876          ADD +1 TO PI-TS-COUNT.                                   EL1602
00877                                                                   EL1602
00878      GO TO 1299-EXIT.                                             EL1602
00879                                                                   EL1602
00880  1260-START-PRINTER.                                              EL1602
00881      IF PI-FORMAT-OPTION = 'F'                                    EL1602
00882          MOVE '2' TO PI-ENTRY-CD-1                                EL1602
00883      ELSE                                                         EL1602
00884          MOVE '1' TO PI-ENTRY-CD-1.                               EL1602
00885                                                                   EL1602
00886  1265-START-PRINTER.                                              EL1602
00887      IF FIRST-TIME-THRU                                           EL1602
00888          IF PI-ALT-PRINT-ID = SPACES                              EL1602
00889              GO TO 1270-GET-PRINT-ID                              EL1602
00890          ELSE                                                     EL1602
00891              MOVE PI-ALT-PRINT-ID TO PI-PRINT-ID.                 EL1602
00892                                                                   EL1602
00893      EXEC CICS HANDLE CONDITION                                   EL1602
00894          NOTOPEN (1275-CNTL-NOT-OPEN)                             EL1602
00895          NOTFND  (1280-NOT-FOUND)                                 EL1602
00896          TERMIDERR  (1270-GET-PRINT-ID)                           EL1602
00897          TRANSIDERR (1285-TRANS-ERROR)                            EL1602
00898      END-EXEC.                                                       CL**4
00899                                                                   EL1602
030612     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL'                      CL**7
00901 *        MOVE EIBTRMID       TO PI-PRINT-ID                          CL**7
00902          EXEC CICS START                                             CL**7
00903              TRANSID (START-TRANS-ID)                                CL**7
00904 *            TERMID  (PI-PRINT-ID)                                   CL**7
00905              FROM    (PROGRAM-INTERFACE-BLOCK)                       CL**7
00906              LENGTH  (PI-COMM-LENGTH)                                CL**7
00907          END-EXEC                                                    CL**7
00908      ELSE                                                            CL**7
00909          EXEC CICS START                                             CL**7
00910              TRANSID (START-TRANS-ID)                                CL**7
00911              TERMID  (PI-PRINT-ID)                                   CL**7
00912              FROM    (PROGRAM-INTERFACE-BLOCK)                       CL**7
00913              LENGTH  (PI-COMM-LENGTH)                                CL**7
00914          END-EXEC.                                                   CL**7
00915                                                                   EL1602
00916      GO TO 1299-EXIT.                                             EL1602
00917                                                                   EL1602
00918  1270-GET-PRINT-ID.                                               EL1602
00919      IF FIRST-TIME-THRU                                           EL1602
00920          MOVE HIGH-VALUES TO WS-FIRST-TIME-SW                     EL1602
00921      ELSE                                                         EL1602
00922          MOVE ER-0412 TO EMI-ERROR                                EL1602
00923          PERFORM 9900-ERROR-FORMAT                                EL1602
00924             THRU 9900-ERROR-FORMAT-EXIT                           EL1602
00925          GO TO 8110-SEND-DATA.                                    EL1602
00926                                                                   EL1602
00927      MOVE PI-COMPANY-ID          TO COMPANY-ID.                   EL1602
00928      MOVE '1'                    TO RECORD-TYPE.                  EL1602
00929      MOVE SPACES                 TO CNTL-PROC.                    EL1602
00930      MOVE +0                     TO SEQ-NO.                       EL1602
00931      MOVE 'CNTL'                 TO FILE-SWITCH.                  EL1602
00932                                                                   EL1602
00933      EXEC CICS READ                                               EL1602
00934          DATASET ('ELCNTL')                                       EL1602
00935          SET     (ADDRESS OF CONTROL-FILE)                           CL**4
00936          RIDFLD  (CNTL-KEY)                                       EL1602
00937      END-EXEC.                                                       CL**4
00938                                                                   EL1602
00939      IF CF-FORMS-PRINTER-ID = SPACES                              EL1602
00940          MOVE ER-0337 TO EMI-ERROR                                EL1602
00941          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT    EL1602
00942          GO TO 8110-SEND-DATA                                     EL1602
00943      ELSE                                                         EL1602
00944          MOVE CF-FORMS-PRINTER-ID TO PI-PRINT-ID                  EL1602
00945          GO TO 1265-START-PRINTER.                                EL1602
00946                                                                   EL1602
00947  1275-CNTL-NOT-OPEN.                                              EL1602
00948      MOVE ER-0042 TO EMI-ERROR.                                   EL1602
00949      PERFORM 9900-ERROR-FORMAT                                    EL1602
00950         THRU 9900-ERROR-FORMAT-EXIT.                              EL1602
00951      GO TO 8110-SEND-DATA.                                        EL1602
00952                                                                   EL1602
00953  1280-NOT-FOUND.                                                  EL1602
00954      MOVE ER-0190 TO EMI-ERROR.                                   EL1602
00955      PERFORM 9900-ERROR-FORMAT                                    EL1602
00956         THRU 9900-ERROR-FORMAT-EXIT.                              EL1602
00957                                                                      CL**4
00958      GO TO 8110-SEND-DATA.                                        EL1602
00959                                                                   EL1602
00960  1285-TRANS-ERROR.                                                EL1602
00961      MOVE ER-0413 TO EMI-ERROR.                                   EL1602
00962      PERFORM 9900-ERROR-FORMAT                                    EL1602
00963         THRU 9900-ERROR-FORMAT-EXIT.                              EL1602
00964                                                                      CL**4
00965      GO TO 8110-SEND-DATA.                                        EL1602
00966                                                                   EL1602
00967  1299-EXIT.                                                       EL1602
00968      EXIT.                                                        EL1602
00969      EJECT                                                        EL1602
00970                                                                   EL1602
00971  3000-GET-RECORD.                                                 EL1602
00972      IF PI-TS-COUNT GREATER THAN PI-TS-COUNT-1                    EL1602
00973         GO TO 3010-RECORD-NOT-FOUND.                              EL1602
00974                                                                   EL1602
00975      EXEC CICS HANDLE CONDITION                                   EL1602
00976          ITEMERR (3010-RECORD-NOT-FOUND)                          EL1602
00977      END-EXEC.                                                    EL1602
00978                                                                      CL**4
00979      EXEC CICS READQ TS                                           EL1602
00980          QUEUE (PI-EL1602-KEY)                                    EL1602
00981          INTO (EL160BO)                                           EL1602
00982          LENGTH (EL1602-LENGTH)                                   EL1602
00983          ITEM (PI-TS-COUNT)                                       EL1602
00984      END-EXEC.                                                    EL1602
00985                                                                      CL**4
00986      GO TO 3020-GET-RECORD-EXIT.                                  EL1602
00987                                                                   EL1602
00988  3010-RECORD-NOT-FOUND.                                           EL1602
00989      MOVE 'X'                    TO ERROR-SWITCH.                 EL1602
00990      MOVE ER-0130             TO EMI-ERROR.                       EL1602
00991      PERFORM 9900-ERROR-FORMAT                                    EL1602
00992              THRU 9900-ERROR-FORMAT-EXIT.                         EL1602
00993      SUBTRACT NUM-ONE            FROM PI-TS-COUNT.                EL1602
00994      GO TO 3000-GET-RECORD.                                       EL1602
00995                                                                   EL1602
00996  3020-GET-RECORD-EXIT.                                            EL1602
00997      EXIT.                                                        EL1602
00998      EJECT                                                        EL1602
00999  4000-CHECK-UPDATE.                                               EL1602
01000      MOVE SPACE                  TO UPDATE-SWITCH.                EL1602
01001                                                                      CL**4
01002      IF PRICDL GREATER THAN ZEROES                                EL1602
01003          PERFORM 4100-CHECK-PRI                                   EL1602
01004              THRU 4100-CHECK-PRI-EXIT                             EL1602
01005      ELSE                                                         EL1602
01006          MOVE AL-UANOF           TO PRICDA.                       EL1602
01007                                                                      CL**4
01008      IF SUPVL GREATER THAN ZEROES                                 EL1602
01009          PERFORM 4110-CHECK-SUPR                                  EL1602
01010              THRU 4110-CHECK-SUPR-EXIT                            EL1602
01011      ELSE                                                         EL1602
01012          MOVE AL-UANOF           TO SUPVA.                        EL1602
01013                                                                      CL**4
01014      IF FILEL GREATER THAN ZEROES                                 EL1602
01015          PERFORM 4120-CHECK-FILE                                  EL1602
01016              THRU 4120-CHECK-FILE-EXIT                            EL1602
01017      ELSE                                                         EL1602
01018          MOVE AL-UANOF TO FILEA.                                  EL1602
01019                                                                      CL**4
01020      IF PROCL GREATER THAN ZEROES                                 EL1602
01021          PERFORM 4130-CHECK-PROC                                  EL1602
01022              THRU 4150-CHECK-PROC-EXIT                            EL1602
01023      ELSE                                                         EL1602
01024          MOVE AL-UANOF TO PROCA.                                  EL1602
01025                                                                   EL1602
01026      IF NOT NO-UPDATES                                            EL1602
01027         GO TO 4000-CHECK-CAP.                                     EL1602
01028                                                                   EL1602
01029      IF NOSCRNL NOT = ZEROS                                       EL1602
01030         GO TO 1000-BROWSE.                                        EL1602
01031                                                                   EL1602
01032      IF EIBAID = DFHPF1 OR DFHPF2 OR                              EL1602
01033                  DFHPF5 OR DFHPF6                                 EL1602
01034         MOVE NUM-ONE TO PI-TS-COUNT                               EL1602
01035         GO TO 1100-CHECK-PFKEYS.                                  EL1602
01036                                                                   EL1602
01037      IF  EIBAID = DFHENTER                                        EL1602
01038          MOVE 'X'                TO ERROR-SWITCH                  EL1602
01039          MOVE ER-0276         TO EMI-ERROR                        EL1602
01040          PERFORM 9900-ERROR-FORMAT                                EL1602
01041              THRU 9900-ERROR-FORMAT-EXIT                          EL1602
01042          MOVE LIT-IC             TO NOSCRNL                       EL1602
01043          GO TO 4000-CHECK-UPDATE-EXIT.                            EL1602
01044                                                                      CL**4
01045  4000-CHECK-CAP.                                                  EL1602
01046      IF NOT MODIFY-CAP                                            EL1602
01047          MOVE 'X'                TO ERROR-SWITCH                  EL1602
01048          MOVE ER-0070         TO EMI-ERROR                        EL1602
01049          PERFORM 9900-ERROR-FORMAT                                EL1602
01050              THRU 9900-ERROR-FORMAT-EXIT                          EL1602
01051          MOVE LIT-IC             TO NOSCRNL                       EL1602
01052          GO TO 4000-CHECK-UPDATE-EXIT.                            EL1602
01053                                                                   EL1602
01054      PERFORM 4200-UPDATE-MSTR THRU 4210-UPDATE-MSTR-EXIT.         EL1602
01055                                                                      CL**4
01056      IF SCREEN-ERROR                                              EL1602
01057          PERFORM 5000-UPDATE-TS                                   EL1602
01058              THRU 5000-UPDATE-TS-EXIT                             EL1602
01059          MOVE LIT-IC             TO NOSCRNL                       EL1602
01060          MOVE ER-0068         TO EMI-ERROR                        EL1602
01061          PERFORM 9900-ERROR-FORMAT                                EL1602
01062              THRU 9900-ERROR-FORMAT-EXIT.                         EL1602
01063                                                                      CL**4
01064  4000-CHECK-UPDATE-EXIT.                                          EL1602
01065      EXIT.                                                        EL1602
01066      EJECT                                                        EL1602
01067  4100-CHECK-PRI.                                                  EL1602
01068      IF PRICDI GREATER THAN ZERO                                  EL1602
01069        AND                                                        EL1602
01070         PRICDI NOT GREATER THAN '9'                               EL1602
01071          MOVE AL-UANON TO PRICDA                                  EL1602
01072          MOVE 'X'                TO UPDATE-SWITCH                 EL1602
01073          GO TO 4100-CHECK-PRI-EXIT.                               EL1602
01074                                                                      CL**4
01075      MOVE LIT-IC                 TO PRICDL.                       EL1602
01076      MOVE AL-UABON               TO PRICDA.                       EL1602
01077      MOVE ER-0274             TO EMI-ERROR.                       EL1602
01078      PERFORM 9900-ERROR-FORMAT                                    EL1602
01079          THRU 9900-ERROR-FORMAT-EXIT.                             EL1602
01080                                                                      CL**4
01081  4100-CHECK-PRI-EXIT.                                             EL1602
01082      EXIT.                                                        EL1602
01083                                                                      CL**4
01084  4110-CHECK-SUPR.                                                 EL1602
01085      IF SUPVI = 'Y' OR 'N' OR SPACES                              EL1602
01086          MOVE 'X'                TO UPDATE-SWITCH                 EL1602
01087          MOVE AL-UANON           TO SUPVA                         EL1602
01088          GO TO 4110-CHECK-SUPR-EXIT.                              EL1602
01089                                                                      CL**4
01090      MOVE LIT-IC                 TO SUPVL.                        EL1602
01091      MOVE AL-UABON               TO SUPVA.                        EL1602
01092      MOVE ER-0230             TO EMI-ERROR.                       EL1602
01093      PERFORM 9900-ERROR-FORMAT                                    EL1602
01094          THRU 9900-ERROR-FORMAT-EXIT.                             EL1602
01095                                                                      CL**4
01096  4110-CHECK-SUPR-EXIT.                                            EL1602
01097      EXIT.                                                        EL1602
01098                                                                      CL**4
01099  4120-CHECK-FILE.                                                 EL1602
01100      IF FILEI NOT = SPACES                                        EL1602
01101          MOVE 'X'                TO UPDATE-SWITCH                 EL1602
01102          GO TO 4120-CHECK-FILE-EXIT.                              EL1602
01103                                                                   EL1602
01104  4120-CHECK-FILE-EXIT.                                            EL1602
01105      EXIT.                                                        EL1602
01106                                                                      CL**4
01107  4130-CHECK-PROC.                                                 EL1602
01108      EXEC CICS HANDLE CONDITION                                   EL1602
01109          NOTFND (4140-PROC-ERROR)                                 EL1602
01110      END-EXEC.                                                    EL1602
01111                                                                      CL**4
01112      MOVE PI-COMPANY-ID          TO COMPANY-ID.                   EL1602
01113      MOVE '2'                    TO RECORD-TYPE.                  EL1602
01114      MOVE PROCI                  TO CNTL-PROC.                    EL1602
01115      MOVE ZEROES                 TO SEQ-NO.                       EL1602
01116                                                                      CL**4
01117      EXEC CICS READ                                               EL1602
01118          SET (ADDRESS OF CONTROL-FILE)                               CL**4
01119          DATASET ('ELCNTL')                                       EL1602
01120          RIDFLD (CNTL-KEY)                                        EL1602
01121      END-EXEC.                                                    EL1602
01122                                                                      CL**4
01123      MOVE 'X'                    TO KEY-SWITCH UPDATE-SWITCH.     EL1602
01124      GO TO 4150-CHECK-PROC-EXIT.                                  EL1602
01125                                                                      CL**4
01126  4140-PROC-ERROR.                                                 EL1602
01127                                                                   EL1602
01128      MOVE ER-0273 TO EMI-ERROR.                                   EL1602
01129      MOVE LIT-IC                 TO PROCL.                        EL1602
01130      MOVE AL-UABON               TO PROCA.                        EL1602
01131      PERFORM 9900-ERROR-FORMAT                                    EL1602
01132          THRU 9900-ERROR-FORMAT-EXIT.                             EL1602
01133                                                                      CL**4
01134  4150-CHECK-PROC-EXIT.                                            EL1602
01135      EXIT.                                                        EL1602
01136      EJECT                                                        EL1602
01137  4200-UPDATE-MSTR.                                                EL1602
01138      MOVE PIKEYI         TO PI-CONTROL-IN-PROGRESS.               EL1602
01139      MOVE SPACES         TO MSTR-KEY.                             EL1602
01140      MOVE PI-COMPANY-CD  TO MSTR-COMPANY-CODE.                    EL1602
01141                                                                   EL1602
01142      MOVE PI-CARRIER     TO MSTR-CARRIER.                            CL**2
01143                                                                   EL1602
01144      MOVE PI-CLAIM-NO    TO MSTR-CLAIM-NO.                        EL1602
01145      MOVE PI-CERT-NO     TO MSTR-CERT-NO.                         EL1602
01146                                                                   EL1602
01147      EXEC CICS READ                                               EL1602
01148          INTO    (CLAIM-MASTER)                                      CL**4
01149          DATASET (W-FILE-ID)                                         CL**4
01150          RIDFLD  (MSTR-KEY)                                          CL**4
01151          UPDATE                                                   EL1602
01152      END-EXEC.                                                    EL1602
01153                                                                   EL1602
01154      MOVE USERSAVI TO PI-UPDATE-BY.                               EL1602
01155      MOVE TIMESAVI TO PI-UPDATE-HHMMSS.                           EL1602
01156                                                                   EL1602
01157      IF PI-UPDATE-BY NOT = CL-LAST-MAINT-USER                     EL1602
01158          PERFORM 4220-UNLOCK-MSTR                                 EL1602
01159              THRU 4230-UNLOCK-MSTR-EXIT                           EL1602
01160          MOVE 'X'                TO ERROR-SWITCH                  EL1602
01161          GO TO 4210-UPDATE-MSTR-EXIT.                             EL1602
01162                                                                   EL1602
01163      IF PI-UPDATE-HHMMSS NOT = CL-LAST-MAINT-HHMMSS               EL1602
01164          PERFORM 4220-UNLOCK-MSTR                                 EL1602
01165              THRU 4230-UNLOCK-MSTR-EXIT                           EL1602
01166          MOVE 'X'                TO ERROR-SWITCH                  EL1602
01167          GO TO 4210-UPDATE-MSTR-EXIT.                             EL1602
01168                                                                   EL1602
01169      IF KEY-CHANGE                                                EL1602
01170          PERFORM 4300-CHANGE-KEY                                  EL1602
01171              THRU 4310-CHANGE-KEY-EXIT                            EL1602
01172      ELSE                                                         EL1602
01173          PERFORM 4320-UPDATE-RECORD                               EL1602
01174              THRU 4330-UPDATE-RECORD-EXIT.                        EL1602
01175                                                                   EL1602
01176      MOVE PROCI                  TO HOLD-PROC.                    EL1602
01177      MOVE PRICDI                 TO HOLD-PRI.                     EL1602
01178      MOVE SUPVI                  TO HOLD-SUPV.                    EL1602
01179      MOVE FILEI                  TO HOLD-FILE.                    EL1602
01180                                                                   EL1602
01181      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.           EL1602
01182                                                                   EL1602
01183      IF HOLD-PROC NOT = LOW-VALUES                                EL1602
01184          MOVE HOLD-PROC     TO PROCO.                             EL1602
01185      IF HOLD-PRI NOT = LOW-VALUES                                    CL**4
01186          MOVE HOLD-PRI      TO PRICDO.                            EL1602
01187      IF HOLD-SUPV NOT = LOW-VALUES                                EL1602
01188          MOVE HOLD-SUPV     TO SUPVO.                             EL1602
01189      IF HOLD-FILE NOT = LOW-VALUES                                EL1602
01190          MOVE HOLD-FILE     TO FILEO.                             EL1602
01191                                                                   EL1602
01192      MOVE SAVE-DATE         TO MNTDTO.                            EL1602
01193      MOVE LIT-UPDATE        TO MNTTYPEO.                          EL1602
01194      MOVE EIBTIME           TO TIMESAVO.                          EL1602
01195      MOVE PI-PROCESSOR-ID   TO USERSAVO.                          EL1602
01196                                                                   EL1602
01197      PERFORM 4400-REWRITE-TS THRU 4410-REWRITE-TS-EXIT.           EL1602
01198                                                                   EL1602
01199      MOVE AL-UANOF TO PROCA                                       EL1602
01200                       PRICDA                                      EL1602
01201                       SUPVA                                       EL1602
01202                       FILEA.                                      EL1602
01203                                                                      CL**4
01204  4210-UPDATE-MSTR-EXIT.                                           EL1602
01205      EXIT.                                                        EL1602
01206                                                                      CL**4
01207  4220-UNLOCK-MSTR.                                                EL1602
01208      EXEC CICS UNLOCK                                             EL1602
01209          DATASET (W-FILE-ID)                                         CL**4
01210      END-EXEC.                                                    EL1602
01211                                                                      CL**4
01212  4230-UNLOCK-MSTR-EXIT.                                           EL1602
01213      EXIT.                                                        EL1602
01214      EJECT                                                        EL1602
01215  4300-CHANGE-KEY.                                                 EL1602
01216      MOVE CLAIM-MASTER           TO  WS-OLD-CLAIM-RECORD.            CL**3
01217                                                                      CL**3
01218      EXEC CICS DELETE                                             EL1602
01219          DATASET (W-FILE-ID)                                         CL**4
01220      END-EXEC.                                                    EL1602
01221                                                                   EL1602
01222 *    EXEC CICS GETMAIN                                               CL**4
01223 *        SET (ADDRESS OF CLAIM-MASTER)                               CL**4
01224 *        LENGTH (MSTR-LENGTH)                                        CL**4
01225 *        INITIMG (GETMAIN-SPACE)                                     CL**4
01226 *    END-EXEC.                                                       CL**4
01227                                                                   EL1602
01228      MOVE WS-OLD-CLAIM-RECORD    TO  CLAIM-MASTER.                   CL**3
01229                                                                   EL1602
01230      IF PROCI NOT = LOW-VALUES                                    EL1602
01231          MOVE PROCI          TO CL-PROCESSOR-ID.                  EL1602
01232      IF PRICDI NOT = LOW-VALUES                                   EL1602
01233          MOVE PRICDI         TO CL-PRIORITY-CD.                   EL1602
01234      IF SUPVI NOT = LOW-VALUES                                    EL1602
01235          MOVE SUPVI          TO CL-SUPV-ATTN-CD.                  EL1602
01236      IF FILEI NOT = LOW-VALUES                                    EL1602
01237          MOVE FILEI          TO CL-FILE-LOCATION.                 EL1602
01238                                                                   EL1602
01239      MOVE SAVE-BIN-DATE      TO CL-LAST-MAINT-DT.                 EL1602
01240      MOVE PI-PROCESSOR-ID    TO CL-LAST-MAINT-USER.               EL1602
01241      MOVE EIBTIME            TO CL-LAST-MAINT-HHMMSS.             EL1602
01242      MOVE '3'                TO CL-LAST-MAINT-TYPE.               EL1602
01243                                                                   EL1602
01244      EXEC CICS HANDLE CONDITION                                   EL1602
01245          DUPKEY (4310-CHANGE-KEY-EXIT)                               CL**3
01246      END-EXEC.                                                       CL**4
01247                                                                   EL1602
01248      EXEC CICS WRITE                                              EL1602
01249          FROM    (CLAIM-MASTER)                                      CL**4
01250          DATASET (W-FILE-ID)                                         CL**4
01251          RIDFLD  (MSTR-KEY)                                          CL**4
01252      END-EXEC.                                                    EL1602
01253                                                                   EL1602
01254  4310-CHANGE-KEY-EXIT.                                            EL1602
01255      EXIT.                                                        EL1602
01256                                                                      CL**4
01257  4320-UPDATE-RECORD.                                              EL1602
01258      EXEC CICS HANDLE CONDITION                                   EL1602
01259          DUPKEY (4330-UPDATE-RECORD-EXIT)                            CL**3
01260      END-EXEC.                                                       CL**4
01261                                                                   EL1602
01262      IF PRICDI NOT = LOW-VALUES                                   EL1602
01263          MOVE PRICDI TO CL-PRIORITY-CD.                           EL1602
01264      IF SUPVI NOT = LOW-VALUES                                    EL1602
01265          MOVE SUPVI TO CL-SUPV-ATTN-CD.                           EL1602
01266      IF FILEI NOT = LOW-VALUES                                    EL1602
01267          MOVE FILEI TO CL-FILE-LOCATION.                          EL1602
01268                                                                   EL1602
01269      MOVE SAVE-BIN-DATE          TO CL-LAST-MAINT-DT.             EL1602
01270      MOVE PI-PROCESSOR-ID        TO CL-LAST-MAINT-USER.           EL1602
01271      MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.         EL1602
01272      MOVE '3'                    TO CL-LAST-MAINT-TYPE.           EL1602
01273                                                                   EL1602
01274      EXEC CICS REWRITE                                            EL1602
01275          FROM    (CLAIM-MASTER)                                      CL**4
01276          DATASET (W-FILE-ID)                                         CL**4
01277      END-EXEC.                                                    EL1602
01278                                                                   EL1602
01279  4330-UPDATE-RECORD-EXIT.                                         EL1602
01280      EXIT.                                                        EL1602
01281      EJECT                                                        EL1602
01282  4400-REWRITE-TS.                                                 EL1602
01283      EXEC CICS WRITEQ TS                                          EL1602
01284          QUEUE (PI-EL1602-KEY)                                    EL1602
01285          FROM (EL160BO)                                           EL1602
01286          LENGTH (EL1602-LENGTH)                                   EL1602
01287          ITEM (PI-TS-COUNT)                                       EL1602
01288          REWRITE                                                  EL1602
01289      END-EXEC.                                                    EL1602
01290                                                                      CL**4
01291  4410-REWRITE-TS-EXIT.                                            EL1602
01292      EXIT.                                                        EL1602
01293                                                                      CL**4
01294      EJECT                                                        EL1602
01295  5000-UPDATE-TS.                                                  EL1602
01296      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.           EL1602
01297      PERFORM 5010-MOVE-MSTR THRU 5010-MOVE-MSTR-EXIT.             EL1602
01298      PERFORM 4400-REWRITE-TS THRU 4410-REWRITE-TS-EXIT.           EL1602
01299                                                                      CL**4
01300  5000-UPDATE-TS-EXIT.                                             EL1602
01301      EXIT.                                                        EL1602
01302                                                                      CL**4
01303  5010-MOVE-MSTR.                                                  EL1602
01304      MOVE CL-CLAIM-NO            TO CLAIMO PI-CLAIM-NO.           EL1602
01305      MOVE CL-CLAIM-TYPE          TO TYPEO.                        EL1602
01306      MOVE CL-CERT-PRIME          TO CERTO.                        EL1602
01307      MOVE CL-CERT-SFX            TO CERTSXO.                      EL1602
01308                                                                      CL**4
01309      MOVE CL-CCN                 TO CREDCDO.                         CL**4
01310                                                                   EL1602
01311      MOVE CL-CERT-CARRIER        TO CARRO.                        EL1602
01312      MOVE CL-CLAIM-STATUS        TO STATUSO.                      EL1602
01313      MOVE CL-PROCESSOR-ID        TO PROCO.                        EL1602
01314      MOVE CL-INSURED-LAST-NAME   TO MLNAMEO.                      EL1602
01315      MOVE CL-INSURED-1ST-NAME    TO MFNAMEO.                      EL1602
01316      MOVE CL-INSURED-MID-INIT    TO MMINITO.                      EL1602
01317      MOVE CL-INSURED-SEX-CD      TO SEXO.                         EL1602
01318                                                                   EL1602
01319      IF CL-INSURED-BIRTH-DT GREATER THAN LOW-VALUES               EL1602
01320          MOVE CL-INSURED-BIRTH-DT   TO DC-BIN-DATE-1              EL1602
01321          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL1602
01322          PERFORM 9800-CONVERT-DATE                                EL1602
01323              THRU 9800-CONVERT-DATE-EXIT                          EL1602
01324          MOVE DC-GREG-DATE-1-EDIT TO BIRTHO                       EL1602
01325      ELSE                                                         EL1602
01326          MOVE SPACES             TO BIRTHO.                       EL1602
01327                                                                   EL1602
01328      MOVE CL-SOC-SEC-NO          TO SOCIALO.                      EL1602
01329      MOVE CL-INSURED-OCC-CD      TO OCCO.                         EL1602
01330                                                                   EL1602
01331      IF SINGLE-PREMIUM                                            EL1602
01332          MOVE LIT-SP             TO PREMSO                        EL1602
01333      ELSE                                                         EL1602
01334          IF O-B-COVERAGE                                          EL1602
01335              MOVE LIT-OB         TO PREMSO                        EL1602
01336          ELSE                                                     EL1602
01337              IF OPEN-END-COVERAGE                                 EL1602
01338                  MOVE LIT-OE     TO PREMSO                        EL1602
01339              ELSE                                                 EL1602
01340                  MOVE SPACES     TO PREMSO.                       EL1602
01341                                                                   EL1602
01342      MOVE CL-CAUSE-CD            TO CAUSEO.                       EL1602
01343                                                                   EL1602
01344      IF CL-EST-END-OF-DISAB-DT GREATER THAN LOW-VALUES            EL1602
01345          MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1             EL1602
01346          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL1602
01347          PERFORM 9800-CONVERT-DATE                                EL1602
01348              THRU 9800-CONVERT-DATE-EXIT                          EL1602
01349          MOVE DC-GREG-DATE-1-EDIT TO ENDO                         EL1602
01350      ELSE                                                         EL1602
01351          MOVE SPACES             TO ENDO.                         EL1602
01352                                                                   EL1602
01353      IF CL-PAID-THRU-DT GREATER THAN LOW-VALUES                   EL1602
01354          MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1                 EL1602
01355          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL1602
01356          PERFORM 9800-CONVERT-DATE                                EL1602
01357              THRU 9800-CONVERT-DATE-EXIT                          EL1602
01358          MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO                      EL1602
01359      ELSE                                                         EL1602
01360          MOVE SPACES TO PDTHRUO.                                  EL1602
01361                                                                   EL1602
01362      MOVE CL-TOTAL-PAID-AMT      TO PDAMTO.                       EL1602
01363      MOVE CL-NO-OF-DAYS-PAID     TO NODAYSO.                      EL1602
01364      MOVE CL-NO-OF-PMTS-MADE     TO NOPMTSO.                      EL1602
01365                                                                   EL1602
01366      IF CL-INCURRED-DT GREATER THAN LOW-VALUES                    EL1602
01367          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1                 EL1602
01368          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL1602
01369          PERFORM 9800-CONVERT-DATE                                EL1602
01370              THRU 9800-CONVERT-DATE-EXIT                          EL1602
01371          MOVE DC-GREG-DATE-1-EDIT TO INCO                         EL1602
01372      ELSE                                                         EL1602
01373          MOVE SPACES             TO INCO.                         EL1602
01374                                                                   EL1602
01375      IF CL-REPORTED-DT GREATER THAN LOW-VALUES                    EL1602
01376          MOVE CL-REPORTED-DT     TO DC-BIN-DATE-1                 EL1602
01377          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL1602
01378          PERFORM 9800-CONVERT-DATE                                EL1602
01379              THRU 9800-CONVERT-DATE-EXIT                          EL1602
01380          MOVE DC-GREG-DATE-1-EDIT TO REPO                         EL1602
01381      ELSE                                                         EL1602
01382          MOVE SPACES             TO REPO.                         EL1602
01383                                                                   EL1602
01384      IF CL-FILE-ESTABLISH-DT GREATER THAN LOW-VALUES              EL1602
01385          MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1               EL1602
01386          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL1602
01387          PERFORM 9800-CONVERT-DATE                                EL1602
01388              THRU 9800-CONVERT-DATE-EXIT                          EL1602
01389          MOVE DC-GREG-DATE-1-EDIT TO ESTO                         EL1602
01390      ELSE                                                         EL1602
01391          MOVE SPACES             TO ESTO.                         EL1602
01392                                                                   EL1602
01393 *    IF CL-LAST-PMT-DT GREATER THAN LOW-VALUES                    EL1602
01394 *        MOVE CL-LAST-PMT-DT TO DC-BIN-DATE-1                     EL1602
01395 *        MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL1602
01396 *        PERFORM 9800-CONVERT-DATE                                EL1602
01397 *            THRU 9800-CONVERT-DATE-EXIT                          EL1602
01398 *        MOVE DC-GREG-DATE-1-EDIT TO LSTPMTO                      EL1602
01399 *    ELSE                                                         EL1602
01400 *        MOVE SPACES             TO LSTPMTO.                      EL1602
01401 *    MOVE CL-LAST-PMT-AMT        TO LSTAMTO.                      EL1602
01402                                                                   EL1602
01403      IF CL-LAST-MAINT-DT GREATER THAN LOW-VALUES                  EL1602
01404          MOVE CL-LAST-MAINT-DT TO DC-BIN-DATE-1                   EL1602
01405          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE              EL1602
01406          PERFORM 9800-CONVERT-DATE                                EL1602
01407              THRU 9800-CONVERT-DATE-EXIT                          EL1602
01408          MOVE DC-GREG-DATE-1-EDIT TO MNTDTO                       EL1602
01409      ELSE                                                         EL1602
01410          MOVE SPACES             TO MNTDTO.                       EL1602
01411                                                                   EL1602
01412      IF CL-LAST-MAINT-TYPE = SPACE                                EL1602
01413         MOVE LIT-SET-UP         TO MNTTYPEO                       EL1602
01414      ELSE                                                         EL1602
01415      IF CL-LAST-MAINT-TYPE = '1'                                  EL1602
01416         MOVE LIT-PMT             TO MNTTYPEO                      EL1602
01417      ELSE                                                         EL1602
01418      IF CL-LAST-MAINT-TYPE = '2'                                  EL1602
01419         MOVE LIT-LETTER          TO MNTTYPEO                      EL1602
01420      ELSE                                                         EL1602
01421      IF CL-LAST-MAINT-TYPE = '3'                                  EL1602
01422         MOVE LIT-UPDATE          TO MNTTYPEO                      EL1602
01423      ELSE                                                         EL1602
01424      IF CL-LAST-MAINT-TYPE = '4'                                  EL1602
01425         MOVE LIT-RESTORE         TO MNTTYPEO                      EL1602
01426      ELSE                                                         EL1602
01427      IF CL-LAST-MAINT-TYPE = '5'                                  EL1602
01428         MOVE LIT-INC-CHG         TO MNTTYPEO                      EL1602
01429      ELSE                                                         EL1602
01430      IF CL-LAST-MAINT-TYPE = '6'                                  EL1602
01431         MOVE LIT-CONV            TO MNTTYPEO                      EL1602
01432      ELSE                                                         EL1602
01433         MOVE SPACES              TO MNTTYPEO.                     EL1602
01434                                                                   EL1602
01435      MOVE CL-PRIORITY-CD         TO PRICDO.                       EL1602
01436      MOVE CL-SUPV-ATTN-CD        TO SUPVO.                        EL1602
01437      MOVE CL-FILE-LOCATION       TO FILEO.                        EL1602
01438      MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL1602
01439      MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL1602
01440      MOVE PI-CONTROL-IN-PROGRESS TO PIKEYO.                       EL1602
01441      MOVE PI-UPDATE-BY           TO USERSAVO.                     EL1602
01442      MOVE PI-UPDATE-HHMMSS       TO TIMESAVO.                     EL1602
01443                                                                      CL**4
01444  5010-MOVE-MSTR-EXIT.                                             EL1602
01445      EXIT.                                                        EL1602
01446      EJECT                                                        EL1602
01447  8100-SEND-MAP.                                                   EL1602
01448      PERFORM 8120-FORMAT-TIME-DATE                                EL1602
01449              THRU 8130-FORMAT-TIME-DATE-EXIT.                     EL1602
01450                                                                      CL**4
01451      IF PI-RETRIEVAL-FILE                                            CL**4
01452          MOVE '- CLAIM MASTER (RETRIEVAL) -'                         CL**4
01453                                  TO TITLEO                           CL**4
01454          MOVE AL-SANOF           TO PROCA                            CL**6
01455                                     FILEA                            CL**4
01456                                     PRICDA                           CL**4
01457                                     SUPVA.                           CL**4
01458                                                                   EL1602
01459      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL1602
01460      MOVE EMI-MESSAGE-AREA (1) TO MSGBO.                          EL1602
01461                                                                   EL1602
01462      EXEC CICS SEND                                               EL1602
01463          MAP ('EL160B')                                           EL1602
01464          MAPSET ('EL160S')                                        EL1602
01465          ERASE                                                    EL1602
01466          FREEKB                                                   EL1602
01467          CURSOR                                                   EL1602
01468      END-EXEC.                                                    EL1602
01469                                                                      CL**4
01470      GO TO 9000-RETURN-TRANS.                                     EL1602
01471                                                                      CL**4
01472  8110-SEND-DATA.                                                  EL1602
01473      PERFORM 8120-FORMAT-TIME-DATE                                EL1602
01474              THRU 8130-FORMAT-TIME-DATE-EXIT.                     EL1602
01475                                                                      CL**4
01476      IF PI-RETRIEVAL-FILE                                            CL**4
01477          MOVE '- CLAIM MASTER (RETRIEVAL) -'                         CL**4
01478                                  TO TITLEO                           CL**4
01479          MOVE AL-SANOF           TO PROCA                            CL**6
01480                                     FILEA                            CL**4
01481                                     PRICDA                           CL**4
01482                                     SUPVA.                           CL**4
01483                                                                      CL**4
01484      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL1602
01485      MOVE EMI-MESSAGE-AREA (1) TO MSGBO.                          EL1602
01486                                                                      CL**4
01487      EXEC CICS SEND                                               EL1602
01488          MAP ('EL160B')                                           EL1602
01489          MAPSET ('EL160S')                                        EL1602
01490          DATAONLY                                                 EL1602
01491          FREEKB                                                   EL1602
01492          CURSOR                                                   EL1602
01493      END-EXEC.                                                    EL1602
01494                                                                      CL**4
01495      GO TO 9000-RETURN-TRANS.                                     EL1602
01496                                                                      CL**4
01497  8120-FORMAT-TIME-DATE.                                           EL1602
01498      MOVE SAVE-DATE      TO DATEBO.                               EL1602
01499                                                                      CL**4
01500      EXEC CICS ASKTIME                                               CL**4
01501          ABSTIME(LCP-CICS-TIME)                                      CL**4
01502      END-EXEC.                                                       CL**4
01503                                                                      CL**4
01504      EXEC CICS FORMATTIME                                            CL**4
01505          ABSTIME(LCP-CICS-TIME)                                      CL**4
01506          TIME(LCP-TIME-OF-DAY-XX)                                    CL**4
01507      END-EXEC.                                                       CL**4
01508                                                                      CL**4
01509      MOVE LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.                    CL**4
01510      MOVE UN-HOURS       TO FOR-HOURS.                            EL1602
01511      MOVE UN-MINUTES     TO FOR-MINUTES.                          EL1602
01512      MOVE TIME-FORMATTED TO TIMEBO.                               EL1602
01513      MOVE LIT-MAP        TO PI-CURRENT-SCREEN-NO.                 EL1602
01514                                                                   EL1602
01515      IF EMI-ERROR NOT = 0515                                      EL1602
01516         MOVE PI-TS-COUNT         TO CONV-COUNT                    EL1602
01517         MOVE CONV-COUNT          TO EDIT-COUNT                    EL1602
01518         MOVE EDIT-COUNT          TO NOSCRNO.                      EL1602
01519                                                                   EL1602
01520      MOVE PI-TS-COUNT-1 TO CONV-COUNT.                            EL1602
01521      MOVE CONV-COUNT    TO EDIT-COUNT.                            EL1602
01522      MOVE EDIT-COUNT    TO TOTSCRNO.                              EL1602
01523                                                                      CL**4
01524  8130-FORMAT-TIME-DATE-EXIT.                                      EL1602
01525      EXIT.                                                        EL1602
01526                                                                      CL**4
01527  8200-RETURN-PRIOR.                                               EL1602
01528      MOVE PI-RETURN-TO-PROGRAM TO CALL-PGM.                       EL1602
01529      GO TO 9200-XCTL.                                             EL1602
01530                                                                      CL**4
01531  8300-GET-HELP.                                                   EL1602
01532      MOVE LIT-HELP TO CALL-PGM.                                   EL1602
01533      GO TO 9200-XCTL.                                             EL1602
01534                                                                      CL**4
01535  8400-RETURN-MASTER.                                              EL1602
01536      PERFORM 8700-DELETEQ THRU 8700-DELETEQ-EXIT.                 EL1602
01537      MOVE LIT-MASTER TO CALL-PGM.                                 EL1602
01538      GO TO 9200-XCTL.                                             EL1602
01539                                                                      CL**4
01540  8500-GET-ACT.                                                    EL1602
01541      MOVE PIKEYI TO PI-CONTROL-IN-PROGRESS.                       EL1602
01542                                                                      CL**4
01543      EXEC CICS WRITEQ TS                                          EL1602
01544          QUEUE (PI-KEY)                                           EL1602
01545          FROM (PROGRAM-INTERFACE-BLOCK)                           EL1602
01546          LENGTH (PI-COMM-LENGTH)                                  EL1602
01547      END-EXEC.                                                    EL1602
01548                                                                      CL**4
01549      MOVE LIT-ACT TO CALL-PGM.                                    EL1602
01550      GO TO 9200-XCTL.                                             EL1602
01551                                                                      CL**4
01552  8700-DELETEQ.                                                    EL1602
01553       EXEC CICS DELETEQ                                           EL1602
01554          QUEUE (PI-EL160-KEY)                                     EL1602
01555      END-EXEC.                                                    EL1602
01556       EXEC CICS DELETEQ                                           EL1602
01557          QUEUE (PI-EL1602-KEY)                                    EL1602
01558      END-EXEC.                                                    EL1602
01559                                                                      CL**4
01560  8700-DELETEQ-EXIT.                                               EL1602
01561      EXIT.                                                        EL1602
01562                                                                      CL**4
01563  8800-UNAUTHORIZED-ACCESS.                                        EL1602
01564      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL1602
01565      GO TO 8990-SEND-TEXT.                                        EL1602
01566                                                                      CL**4
01567  8810-PF23-ENTERED.                                               EL1602
01568      PERFORM 8700-DELETEQ THRU 8700-DELETEQ-EXIT.                 EL1602
01569      MOVE EIBAID TO PI-ENTRY-CD-1.                                EL1602
01570      MOVE LIT-SIGN-OFF TO CALL-PGM.                               EL1602
01571      GO TO 9200-XCTL.                                             EL1602
01572                                                                      CL**4
01573  8820-XCTL-ERROR.                                                 EL1602
01574      EXEC CICS HANDLE CONDITION                                   EL1602
01575          PGMIDERR (8990-SEND-TEXT)                                EL1602
01576      END-EXEC.                                                    EL1602
01577                                                                   EL1602
01578      MOVE SPACE        TO PI-ENTRY-CD-1.                          EL1602
01579      MOVE CALL-PGM     TO PI-CALLING-PROGRAM   LOGOFF-PGM         EL1602
01580      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL1602
01581      MOVE LIT-SIGN-OFF TO CALL-PGM.                               EL1602
01582      GO TO 9200-XCTL.                                             EL1602
01583                                                                      CL**4
01584  8990-SEND-TEXT.                                                  EL1602
01585      EXEC CICS SEND TEXT                                          EL1602
01586          FROM (LOGOFF-TEXT)                                       EL1602
01587          LENGTH (LOGOFF-LENGTH)                                   EL1602
01588          ERASE                                                    EL1602
01589          FREEKB                                                   EL1602
01590      END-EXEC.                                                    EL1602
01591                                                                      CL**4
01592      GO TO 9100-RETURN-CICS.                                      EL1602
01593      EJECT                                                        EL1602
01594  9000-RETURN-TRANS.                                               EL1602
01595      EXEC CICS RETURN                                             EL1602
01596          TRANSID (TRANS-ID)                                       EL1602
01597          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1602
01598          LENGTH (PI-COMM-LENGTH)                                  EL1602
01599      END-EXEC.                                                    EL1602
01600                                                                      CL**4
01601      GOBACK.                                                      EL1602
01602                                                                      CL**4
01603  9100-RETURN-CICS.                                                EL1602
01604      EXEC CICS RETURN                                             EL1602
01605      END-EXEC.                                                    EL1602
01606                                                                      CL**4
01607      GOBACK.                                                      EL1602
01608                                                                      CL**4
01609  9200-XCTL.                                                       EL1602
01610      EXEC CICS XCTL                                               EL1602
01611          PROGRAM (CALL-PGM)                                       EL1602
01612          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1602
01613          LENGTH (PI-COMM-LENGTH)                                  EL1602
01614      END-EXEC.                                                    EL1602
01615                                                                      CL**4
01616  9800-CONVERT-DATE.                                               EL1602
01617      MOVE SPACE TO DC-ERROR-CODE.                                 EL1602
01618                                                                      CL**4
01619      EXEC CICS LINK                                               EL1602
01620          PROGRAM   (DATE-CONV)                                    EL1602
01621          COMMAREA  (DATE-CONVERSION-DATA)                         EL1602
01622          LENGTH    (DC-COMM-LENGTH)                               EL1602
01623      END-EXEC.                                                    EL1602
01624                                                                      CL**4
01625  9800-CONVERT-DATE-EXIT.                                          EL1602
01626      EXIT.                                                        EL1602
01627                                                                      CL**4
01628  9900-ERROR-FORMAT.                                               EL1602
01629      IF EMI-ERRORS-COMPLETE                                       EL1602
01630          GO TO 9900-ERROR-FORMAT-EXIT.                            EL1602
01631                                                                      CL**4
01632      EXEC CICS LINK                                               EL1602
01633          PROGRAM ('EL001')                                        EL1602
01634          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL1602
01635          LENGTH (EMI-COMM-LENGTH)                                 EL1602
01636      END-EXEC.                                                    EL1602
01637                                                                      CL**4
01638  9900-ERROR-FORMAT-EXIT.                                          EL1602
01639      EXIT.                                                        EL1602
01640                                                                      CL**4
01641  9990-ABEND.                                                      EL1602
01642      EXEC CICS LINK                                               EL1602
01643          PROGRAM('EL004')                                         EL1602
01644          COMMAREA(DFHEIBLK)                                       EL1602
01645          LENGTH(64)                                               EL1602
01646      END-EXEC.                                                    EL1602
01647                                                                   EL1602
01648      GO TO 9100-RETURN-CICS.                                      EL1602
