00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   EL1042
00003  PROGRAM-ID.                 EL1042.                                 LV022
00004 *              PROGRAM CONVERTED BY                                  CL*19
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*19
00006 *              CONVERSION DATE 03/21/95 13:28:51.                    CL*19
00007 *                            VMOD=2.021                              CL*21
00008 *                                                                 EL1042
00008 *                                                                 EL1042
00009 *AUTHOR.           LOGIC,INC.                                        CL*19
00010 *                  DALLAS,TEXAS.                                     CL*19
00011                                                                   EL1042
00012 *DATE-COMPILED.                                                      CL*19
00013 *            *****************************************************   CL*19
00014 *            *                                                   *   CL*19
00015 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *         CL*19
00016 *            *                                                   *   CL*19
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*19
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*19
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*19
00020 *            *                                                   *   CL*19
00021 *            *****************************************************   CL*19
00022                                                                   EL1042
00023 *REMARKS. TRANSACTION EX14 - TEXT FILE MAINTENANCE                   CL*19
00024 *        THIS PROGRAM IS USED TO PERFORM MAINTENANCE TO THE HELP,    CL*19
00025 *        LETTER, AND FORM FILES, DEPENDING ON THE OPTION             CL*19
00026 *        SPECIFIED FROM THE TEXT MAINT MENU (EL104).                 CL*19
020410******************************************************************
020410*                   C H A N G E   L O G
020410*
020410* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020410*-----------------------------------------------------------------
020410*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020410* EFFECTIVE    NUMBER
020410*-----------------------------------------------------------------
020410* 020410  CR2009061500002  AJRA  FIX HELP FILE SELECTION
082211* 082211  CR2011022800001  AJRA  NAPERSOFT ACCT SERVICES
020410******************************************************************
00027                                                                   EL1042
00028      EJECT                                                        EL1042
00029  ENVIRONMENT DIVISION.                                            EL1042
00030  DATA DIVISION.                                                   EL1042
00031  WORKING-STORAGE SECTION.                                         EL1042
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL1042
00033  77  FILLER  PIC X(32)  VALUE '*   EL1042 WORKING STORAGE     *'. EL1042
00034  77  FILLER  PIC X(32)  VALUE '******* VMOD=2.021 *************'.    CL*21
00035                                                                   EL1042
00036                              COPY ELCSCTM.                           CL*13
00037                              COPY ELCSCRTY.                          CL*13
00038                              COPY MPCSCRT.                           CL**3
00039                                                                   EL1042
00040     EJECT                                                         EL1042
00041                                                                   EL1042
00042  01  WS-DATE-AREA.                                                EL1042
00043      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL1042
00044      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL1042
00045                                                                      CL*17
00046  01  W-LAST-SAVE-AREA.                                               CL*17
00047      05  W-SAVE-LAST-MAINT-BY    PIC X(4)    VALUE SPACES.           CL*17
00048      05  W-SAVE-LAST-MAINT-DT    PIC X(8)    VALUE SPACES.           CL*17
00049                                                                      CL*16
00050  01  W-HOLD-LINE                 PIC  X(73).                         CL*16
00051                                                                   EL1042
00052  01  WS-CONSTANTS.                                                EL1042
00053      12  LOWER-CASE PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.    CL*19
00054      12  UPPER-CASE PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.    CL*19
00055      12  W-APPL-SCRTY-NDX        PIC S9(04)  COMP VALUE +7.          CL*14
00056      12  MAP-NAME.                                                EL1042
00057          16  MAP-NAME-PRE        PIC X(2)    VALUE 'EL'.          EL1042
00058          16  MAP-NUMBER          PIC X(4)    VALUE '104B'.        EL1042
00059          16  MAP-NAME-FILL       PIC X(2)    VALUE SPACES.        EL1042
00060                                                                   EL1042
00061      12  W-SC-ITEM               PIC S9(4) COMP VALUE +1.            CL**3
00062      12  MAPSET-NAME             PIC X(8)    VALUE 'EL104S'.      EL1042
00063      12  TRANS-ID                PIC X(4)    VALUE 'EX14'.        EL1042
00064      12  PGM-NAME                PIC X(8).                        EL1042
00065      12  PGM-EL152               PIC X(8)    VALUE 'EL152'.       EL1042
00066      12  PGM-EM152               PIC X(8)    VALUE 'EM152'.          CL**3
00067      12  PGM-EL689               PIC X(8)    VALUE 'EL689'.       EL1042
00068      12  THIS-PGM                PIC X(8)    VALUE 'EL1042'.      EL1042
00069      12  XCTL-CLAIM              PIC X(8)    VALUE 'EL126'.       EL1042
00070      12  XCTL-CREDIT             PIC X(8)    VALUE 'EL626'.       EL1042
00071      12  XCTL-WARRANTY           PIC X(8)    VALUE 'WA126'.       EL1042
00072      12  XCTL-MORTGAGE           PIC X(8)    VALUE 'EM626'.          CL**3
00073      12  XCTL-GEN-LEDGER         PIC X(8)    VALUE 'GL800'.       EL1042
00074      12  FILE-ID                 PIC X(8).                        EL1042
00075      12  LETTER-ID               PIC X(8)    VALUE 'ELLETR'.      EL1042
00076      12  WS-8126-ERROR-SW        PIC X       VALUE 'N'.              CL*19
00077      12  W-LINE-SQUEEZE-IND      PIC X(01).                          CL**5
00078          88  W-LINE-SQUEEZE-VALID-VALUE      VALUE ' ' 'A'           CL**5
00079              'C' 'D' 'E' 'F' 'G' 'H' 'K' 'N' 'P' 'Q' 'R' 'S'         CL*12
00080              'T' 'U' 'Z' '1' '2' '3' '4' '5'.                        CL*15
00081      12  FORM-ID                 PIC X(8)    VALUE 'ELFORM'.      EL1042
00082      12  HELP-ID                 PIC X(8)    VALUE 'ELHELP'.      EL1042
00083      12  TS-NAME.                                                 EL1042
00084          16  FILLER              PIC X(4)    VALUE '104A'.        EL1042
00085          16  TS-TERM             PIC X(4).                        EL1042
00086      12  TS-ITEM                 PIC S9(4)   COMP VALUE +0.       EL1042
00087      12  FILE-LENGTH             PIC S9(4)   COMP VALUE +100.     EL1042
00088      12  FILE-KEY.                                                EL1042
00089          16  FILE-PARTIAL-KEY.                                    EL1042
00090            18  CO-CD             PIC X.                           EL1042
00091            18  CNTL-AREA         PIC X(12).                       EL1042
00092          16  SEQ                 PIC S9(4)   COMP.                EL1042
00093      12  OLD-KEY-SAVE            PIC X(13).                       EL1042
00094                                                                   EL1042
00095      12  TIME-IN                 PIC S9(7).                       EL1042
00096      12  FILLER REDEFINES TIME-IN.                                EL1042
00097          16  FILLER              PIC X.                           EL1042
00098          16  TIME-OUT            PIC 99V99.                       EL1042
00099          16  FILLER              PIC 9(2).                        EL1042
00100      12  MAX-LINES               PIC 999     VALUE 300.           EL1042
00101      12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 15.               CL*18
00102      12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.            EL1042
00103      12  TS-GROUP-WORK           PIC 9(5)    VALUE 0  COMP-3.     EL1042
00104      12  TS-LENGTH               PIC S9(4)   VALUE +3650 COMP.       CL**5
00105      12  ROLL-COUNTER            PIC S999    VALUE +0 COMP-3.     EL1042
00106      12  TEMP-CURR-LINE          PIC S9(3)   COMP-3.              EL1042
00107      EJECT                                                        EL1042
00108                                  COPY ELCLOGOF.                      CL*13
00109      EJECT                                                        EL1042
00110                                  COPY ELCAID.                        CL*13
00111  01  FILLER  REDEFINES DFHAID.                                    EL1042
00112      12  FILLER                  PIC X(8).                        EL1042
00113      12  PF-VALUES OCCURS 24 TIMES       PIC X.                   EL1042
00114      EJECT                                                        EL1042
00115                                      COPY ELCEMIB.                   CL*13
00116                                                                      CL*22
00117  01  WS-ERROR-MESSAGE-AREA.                                          CL*13
00118      12  ER-0000                     PIC 9(4)   VALUE 0000.          CL*18
00119      12  ER-0004                     PIC 9(4)   VALUE 0004.          CL*18
00120      12  ER-0006                     PIC 9(4)   VALUE 0006.          CL*18
00121      12  ER-0008                     PIC 9(4)   VALUE 0008.          CL*18
00122      12  ER-0013                     PIC 9(4)   VALUE 0013.          CL*18
00123      12  ER-0014                     PIC 9(4)   VALUE 0014.          CL*18
00124      12  ER-0015                     PIC 9(4)   VALUE 0015.          CL*18
00125      12  ER-0023                     PIC 9(4)   VALUE 0023.          CL*18
00126      12  ER-0029                     PIC 9(4)   VALUE 0029.          CL*18
00127      12  ER-0030                     PIC 9(4)   VALUE 0030.          CL*18
00128      12  ER-0031                     PIC 9(4)   VALUE 0031.          CL*18
00129      12  ER-0032                     PIC 9(4)   VALUE 0032.          CL*18
00130      12  ER-0033                     PIC 9(4)   VALUE 0033.          CL*18
00131      12  ER-0041                     PIC 9(4)   VALUE 0041.          CL*18
00132      12  ER-0044                     PIC 9(4)   VALUE 0044.          CL*18
00133      12  ER-0045                     PIC 9(4)   VALUE 0045.          CL*18
00134      12  ER-0047                     PIC 9(4)   VALUE 0047.          CL*18
00135      12  ER-0048                     PIC 9(4)   VALUE 0048.          CL*18
00136      12  ER-0049                     PIC 9(4)   VALUE 0049.          CL*18
00137      12  ER-0050                     PIC 9(4)   VALUE 0050.          CL*18
00138      12  ER-0051                     PIC 9(4)   VALUE 0051.          CL*18
00139      12  ER-0066                     PIC 9(4)   VALUE 0066.          CL*18
00140      12  ER-0067                     PIC 9(4)   VALUE 0067.          CL*18
00141      12  ER-0069                     PIC 9(4)   VALUE 0069.          CL*18
00142      12  ER-0070                     PIC 9(4)   VALUE 0070.          CL*18
00143      12  ER-0140                     PIC 9(4)   VALUE 0140.          CL*18
00144      12  ER-0141                     PIC 9(4)   VALUE 0141.          CL*18
00145      12  ER-0212                     PIC 9(4)   VALUE 0212.          CL*18
00146      12  ER-7375                     PIC 9(4)   VALUE 7375.          CL*18
00147      12  ER-8126                     PIC 9(4)   VALUE 8126.          CL*19
00148      12  ER-9097                     PIC 9(4)   VALUE 9097.          CL*18
00149      12  ER-9348                     PIC 9(4)   VALUE 9348.          CL*18
00150      12  ER-9349                     PIC 9(4)   VALUE 9349.          CL*18
00151  01  EMI-SAVE-AREA                   PIC X(400).                     CL*13
00152      EJECT                                                        EL1042
00153  01  FILLER                          PIC X(22)                       CL**5
00154                               VALUE 'INTERFACE AREA STARTS:'.        CL**5
00155                                      COPY ELCINTF.                   CL*13
00156      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL1042
00157          COPY ELC1042.                                               CL*11
00158          16  FILLER                  PIC X(573).                     CL*19
00159                                                                      CL*19
00160  01  FILLER                          PIC X(20)                       CL**5
00161                               VALUE ':INTERFACE AREA ENDS'.          CL**5
00162      EJECT                                                        EL1042
00163                                      COPY ELCATTR.                   CL*13
00164      EJECT                                                        EL1042
00165                                      COPY ELCDATE.                   CL*13
00166      EJECT                                                        EL1042
00167  01  FILLER                          PIC X(16)                       CL**5
00168                               VALUE 'MAP AREA STARTS:'.              CL**5
00169                                      COPY EL104S.                    CL*13
00170      EJECT                                                        EL1042
00171  01  EL104BR REDEFINES EL104BI.                                   EL1042
00172      12  FILLER                  PIC X(108).                         CL*18
00173      12  SC-ALL-LINES.                                            EL1042
00174       14 SC-LINES OCCURS 15 TIMES INDEXED BY SC-INDX.                CL*18
00175          16  SC-LINL             PIC S9(4)   COMP.                EL1042
00176          16  SC-LINA             PIC X.                           EL1042
00177          16  SC-LIN              PIC Z99.                         EL1042
00178          16  SC-TEXTL            PIC S9(4)   COMP.                EL1042
00179          16  SC-TEXTA            PIC X.                           EL1042
00180          16  SC-TEXT             PIC X(70).                       EL1042
00181          16  SC-PCL              PIC S9(4)   COMP.                EL1042
00182          16  SC-PCA              PIC X.                           EL1042
00183          16  SC-PC               PIC XX.                          EL1042
00184          16  SC-SQL              PIC S9(4)   COMP.                   CL**5
00185          16  SC-SQA              PIC X.                              CL**5
00186          16  SC-SQ               PIC X.                              CL**5
00187                                                                   EL1042
00188  01  FILLER                          PIC X(14)                       CL**5
00189                               VALUE ':MAP AREA ENDS'.                CL**5
00190  01  RECORD-TABLE                PIC X(21900) VALUE SPACES.          CL**5
00191  01  REC-TABLE  REDEFINES RECORD-TABLE.                           EL1042
00192      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX PIC X(3650).     CL**5
00193  01  REC-ENTRIES REDEFINES RECORD-TABLE.                          EL1042
00194      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.    EL1042
00195          16  REC-TEXT            PIC X(70).                       EL1042
00196          16  REC-PC              PIC XX.                          EL1042
00197          16  REC-SQ              PIC X.                              CL**5
00198  01  TS-WORK-AREA                PIC X(3650).                        CL**5
00199      EJECT                                                        EL1042
00200                                                                   EL1042
00201  LINKAGE SECTION.                                                 EL1042
00202  01  DFHCOMMAREA                 PIC X(1024).                     EL1042
00203 *01 PARMLIST .                                                       CL*19
00204 *    02  FILLER                  PIC S9(8)  COMP.                    CL*19
00205 *    02  TXT-ADDR                PIC S9(8)  COMP.                    CL*19
00206      EJECT                                                        EL1042
00207                                  COPY ELCTEXT.                       CL*13
00208      EJECT                                                        EL1042
00209  PROCEDURE DIVISION.                                              EL1042
00210                                                                      CL**4
00211      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL1042
00212                                                                   EL1042
00213      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL1042
00214      MOVE '5'                   TO DC-OPTION-CODE.                EL1042
00215      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1042
00216      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL1042
00217      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL1042
00218                                                                   EL1042
00219      IF  EIBCALEN = ZEROS                                            CL**4
00220          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL1042
00221                                                                   EL1042
00222      MOVE EIBTRMID        TO TS-TERM.                             EL1042
00223      MOVE PI-COMM-CONTROL TO CNTL-AREA.                           EL1042
00224      MOVE PI-COMPANY-CD   TO CO-CD.                               EL1042
00225      MOVE ZEROS           TO SEQ.                                 EL1042
00226      MOVE LOW-VALUES      TO EL104BI.                             EL1042
00227      MOVE ERROR-MESSAGE-INTERFACE-BLOCK   TO EMI-SAVE-AREA.          CL*13
00228      EXEC CICS HANDLE CONDITION                                      CL*13
00229           ERROR(9990-ABEND)                                          CL*13
00230           PGMIDERR(9600-PGMID-ERROR)                                 CL*13
00231      END-EXEC.                                                       CL*13
00232                                                                      CL*13
00233      IF  PI-LANGUAGE-IS-FR                                           CL*18
00234          MOVE 'EL104BF'          TO MAP-NAME.                        CL*18
00235                                                                   EL1042
00236      IF  PI-ENTRY-CD-1 = '1'                                         CL**4
00237          MOVE LETTER-ID TO FILE-ID                                EL1042
00238                                                                      CL**4
00239      ELSE                                                         EL1042
00240          IF  PI-ENTRY-CD-1 = '2'                                     CL**4
00241              MOVE FORM-ID TO FILE-ID                              EL1042
00242                                                                      CL**4
00243          ELSE                                                     EL1042
00244              MOVE HELP-ID TO FILE-ID.                             EL1042
020410
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO CO-CD
020410     END-IF.
00245                                                                   EL1042
00246      MOVE FILE-PARTIAL-KEY TO OLD-KEY-SAVE.                       EL1042
00247                                                                   EL1042
00248      IF  PI-CALLING-PROGRAM NOT = THIS-PGM                           CL**4
00249         IF  PI-RETURN-TO-PROGRAM NOT = THIS-PGM                      CL**4
00250            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6        EL1042
00251            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5        EL1042
00252            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4        EL1042
00253            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3        EL1042
00254            MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2        EL1042
00255            MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1        EL1042
00256            MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM      EL1042
00257            MOVE THIS-PGM             TO PI-CALLING-PROGRAM           CL**4
00258            MOVE 'N'                  TO PI-1042-SCREEN-SENT-IND      CL**4
00259            PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT           CL*10
00260                                                                      CL**4
00261         ELSE                                                         CL**4
00262            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM        EL1042
00263            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM      EL1042
00264            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1        EL1042
00265            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2        EL1042
00266            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3        EL1042
00267            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4        EL1042
00268            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5        EL1042
00269            MOVE SPACES               TO PI-SAVED-PROGRAM-6.       EL1042
00270                                                                   EL1042
00271      IF  EIBTRNID = 'MX27'                                           CL**3
00272              OR                                                      CL**3
00273          EIBTRNID = 'EX27'                                           CL**3
00274              OR                                                      CL**3
00275          EIBTRNID = 'EXH3'                                           CL**3
00276          PERFORM 7500-READ-TS  THRU  7599-EXIT                       CL**3
00277          GO TO 7050-FORMAT-LINES.                                    CL**3
00278                                                                   EL1042
00279      GO TO 7000-BUILD-TABLE.                                         CL*13
00280                                                                   EL1042
00281                                                                   EL1042
00282  0100-PA.                                                            CL*13
00283      MOVE ER-0008 TO EMI-ERROR                                       CL*13
00284      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                        CL*13
00285      GO TO 8200-SEND-DATAONLY.                                       CL*13
00286      EJECT                                                        EL1042
00287  0200-RECEIVE.                                                    EL1042
00288      EXEC CICS HANDLE AID                                            CL*13
00289           CLEAR   (9400-CLEAR)                                       CL*13
00290           PA1     (0100-PA)                                          CL*13
00291           PA2     (0100-PA)                                          CL*13
00292           PA3     (0100-PA)                                          CL*13
00293           END-EXEC.                                                  CL*13
00294                                                                      CL*13
00295      MOVE EMI-SAVE-AREA   TO ERROR-MESSAGE-INTERFACE-BLOCK           CL*13
00296      MOVE PI-COMM-CONTROL TO CNTL-AREA.                              CL*13
00297      MOVE PI-COMPANY-CD   TO CO-CD.                                  CL*13
020410     IF FILE-ID = HELP-ID
020410         MOVE LOW-VALUES TO CO-CD
020410     END-IF.
00298      MOVE ZEROS           TO SEQ.                                    CL*13
00299      MOVE ZEROS           TO ROLL-COUNTER.                           CL*13
00300      MOVE LOW-VALUES      TO EL104BI.                             EL1042
00301                                                                   EL1042
00302      EXEC CICS SYNCPOINT                                             CL*13
00303           END-EXEC.                                                  CL*13
00304                                                                      CL*13
00305      IF LOWER-CASE-LETTERS-USED                                      CL*13
00306         EXEC CICS RECEIVE                                            CL*13
00307              MAP   (MAP-NAME)                                        CL*13
00308              MAPSET(MAPSET-NAME)                                     CL*13
00309              INTO  (EL104BI)                                         CL*13
00310              ASIS                                                    CL*13
00311              END-EXEC                                                CL*13
00312          ELSE                                                        CL*13
00313         EXEC CICS RECEIVE                                            CL*13
00314              MAP   (MAP-NAME)                                        CL*13
00315              MAPSET(MAPSET-NAME)                                     CL*13
00316              INTO  (EL104BI)                                         CL*13
00317              END-EXEC.                                               CL*13
00318                                                                      CL*14
00319      IF  NOT DISPLAY-CAP                                             CL*14
00320          MOVE 'READ'             TO SM-READ                          CL*14
00321          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT              CL*14
00322          MOVE ER-9097            TO EMI-ERROR                        CL*14
00323          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*14
00324          GO TO 8100-SEND-INITIAL-MAP.                                CL*14
00325                                                                   EL1042
00326      IF  PFENTRBL = ZEROS                                            CL**4
00327         GO TO 2001-CHECK-PFKEYS.                                  EL1042
00328                                                                   EL1042
00329      IF  EIBAID NOT = DFHENTER                                       CL**4
00330         MOVE ER-0004 TO EMI-ERROR                                 EL1042
00331         GO TO 2002-INPUT-ERROR.                                   EL1042
00332                                                                   EL1042
00333      IF  (PFENTRBI NUMERIC) AND (PFENTRBI GREATER 0 AND LESS 25)     CL**4
00334         MOVE PF-VALUES (PFENTRBI) TO EIBAID                       EL1042
00335        ELSE                                                       EL1042
00336         MOVE ER-0029 TO EMI-ERROR                                 EL1042
00337         GO TO 2002-INPUT-ERROR.                                   EL1042
00338                                                                   EL1042
00339  2001-CHECK-PFKEYS.                                               EL1042
00340      IF  EIBAID = DFHPF23                                            CL**4
00341         GO TO 9000-RETURN-CICS.                                   EL1042
00342                                                                   EL1042
00343      IF  EIBAID = DFHPF24                                            CL**4
00344         GO TO 9200-RETURN-MAIN-MENU.                              EL1042
00345                                                                   EL1042
00346      IF  EIBAID = DFHPF12                                            CL**4
00347         GO TO 9500-PF12.                                          EL1042

00349      IF  FUNCTL NOT = ZEROS AND EIBAID NOT = DFHENTER                CL**4
00350                                                                      CL*18
00351         IF  FUNCTI = 'A' OR 'a' OR SPACES                            CL*13
00352            NEXT SENTENCE                                          EL1042
00353           ELSE                                                    EL1042
00354            MOVE ER-0050 TO EMI-ERROR                              EL1042
00355            MOVE -1 TO FUNCTL                                      EL1042
00356            MOVE AL-UABON TO FUNCTA PFENTRBA                       EL1042
00357            GO TO 2002-INPUT-ERROR.                                EL1042
00358                                                                   EL1042
00359      IF  EIBAID = DFHPF1                                             CL**4
00360         MOVE NUM-LINES-PER-SCREEN TO ROLL-COUNTER                 EL1042
00361         GO TO 7400-PAGE-ROUTINE.                                  EL1042
00362                                                                   EL1042
00363      IF  EIBAID = DFHPF2                                             CL**4
00364         SUBTRACT NUM-LINES-PER-SCREEN FROM ROLL-COUNTER           EL1042
00365         GO TO 7400-PAGE-ROUTINE.                                  EL1042
00366                                                                   EL1042
00367      IF  EIBAID = DFHPF3                                             CL**4
00368         MOVE 5 TO ROLL-COUNTER                                    EL1042
00369         GO TO 7400-PAGE-ROUTINE.                                  EL1042
00370                                                                   EL1042
00371      IF  EIBAID = DFHPF4                                             CL**4
00372         MOVE -5 TO ROLL-COUNTER                                   EL1042
00373         GO TO 7400-PAGE-ROUTINE.                                  EL1042

           IF EIBAID = DFHPF5
              GO TO 9450-PF5
           END-IF

00375      IF  EIBAID = DFHENTER                                           CL**4
00376         GO TO 2003-EDIT-DATA.                                     EL1042
00377                                                                   EL1042
00378      MOVE ER-0029 TO EMI-ERROR.                                   EL1042
00379                                                                      CL**5
00380  2002-INPUT-ERROR.                                                EL1042
00381      MOVE -1       TO PFENTRBL.                                   EL1042
00382      MOVE AL-UNBON TO PFENTRBA.                                   EL1042
00383      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1042
00384      GO TO 8200-SEND-DATAONLY.                                    EL1042
00385                                                                   EL1042
00386  2003-EDIT-DATA.                                                  EL1042
00387                                                                      CL*17
00388      IF  (PI-LANGUAGE-IS-FR                                          CL*18
00389                   AND                                                CL*18
00390              (FUNCTI = 'Q' OR 'q' OR 'V' OR 'v'))                    CL*18
00391              OR                                                      CL*18
00392          FUNCTI = 'Q' OR 'q' OR 'L' OR 'l'                           CL*18
00393          NEXT SENTENCE                                            EL1042
00394                                                                      CL*17
00395      ELSE                                                            CL*17
00396          IF  NOT MODIFY-CAP                                          CL*17
00397              MOVE 'UPDATE'       TO SM-READ                          CL*17
00398              PERFORM 9995-SECURITY-VIOLATION                         CL*17
00399              MOVE ER-0070        TO EMI-ERROR                        CL*17
00400              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*17
00401              GO TO 8200-SEND-DATAONLY.                               CL*17
00402                                                                   EL1042
00403      IF  FUNCTL = ZEROS OR FUNCTI = SPACES                           CL**4
00404          GO TO 4000-CHANGE-ROUTINE                                   CL*18
00405                                                                      CL*18
00406      ELSE                                                            CL*18
00407          IF  (PI-LANGUAGE-IS-FR                                      CL*18
00408                       AND                                            CL*18
00409                  (FUNCTI = 'G' OR 'V' OR 'Q' OR                      CL*18
00410                            'I' OR 'A' OR 'E'  OR                     CL*18
00411                            'C' OR                                    CL*18
00412                            'g' OR 'v' OR 'q' OR                      CL*18
00413                            'i' OR 'a' OR 'e'  OR                     CL*18
00414                            'c'))                                     CL*18
00415                  OR                                                  CL*18
00416              (FUNCTI = 'S' OR 'D' OR 'Q' OR                          CL*18
00417                       'I' OR 'A' OR 'L'  OR                          CL*18
00418                        'c' OR 'C' OR                                 CL*18
00419                        's' OR 'd' OR 'q' OR                          CL*18
00420                       'i' OR 'a' OR 'l')                             CL*18
00421              NEXT SENTENCE                                           CL*18
00422          ELSE                                                        CL*18
00423              MOVE ER-0023 TO EMI-ERROR                               CL*18
00424              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*18
00425              MOVE AL-UABON TO FUNCTA                                 CL*18
00426              MOVE -1 TO FUNCTL                                       CL*18
00427              GO TO 8200-SEND-DATAONLY.                               CL*18
00428                                                                   EL1042
00429      IF  FUNCTI = 'c' OR 'C'                                         CL*16
00430          PERFORM 2600-C-LINE-CHECK THRU 2600-EXIT                    CL*16
00431                                                                      CL*16
00432      ELSE                                                            CL*16
00433          IF  (PI-LANGUAGE-IS-FR                                      CL*18
00434                       AND                                            CL*18
00435                  (FUNCTI = 'E' OR 'I' OR 'V' OR                      CL*18
00436                          'e' OR 'i' OR 'v'))                         CL*18
00437                                                                      CL*18
00438                  OR                                                  CL*18
00439              FUNCTI = 'D' OR 'I' OR 'L' OR                           CL*18
00440                      'd' OR 'i' OR 'l'                               CL*16
00441              PERFORM 2500-LINE-CHECK THRU 2599-EXIT                  CL*16
00442                                                                      CL*16
00443          ELSE                                                        CL*16
00444              IF  LINE1L NOT = ZEROS                                  CL*18
00445                      OR                                              CL*18
00446                  LINE2L NOT = ZEROS                                  CL*16
00447                  MOVE ER-0030 TO EMI-ERROR                           CL*16
00448                  MOVE -1 TO LINE1L                                   CL*16
00449                  MOVE AL-UNBON TO LINE1A LINE2A                      CL*16
00450                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*16
00451                  GO TO 8200-SEND-DATAONLY.                           CL*16
00452                                                                   EL1042
00453      IF  FUNCTI = 'A' OR 'a'                                         CL*13
00454          GO TO 5000-ADD-NEW-LINES.                                   CL*18
00455                                                                   EL1042
00456      IF  FUNCTI = 'Q' OR 'q'                                         CL*13
00457          GO TO 9410-RETURN.                                          CL*18
00458                                                                   EL1042
00459      IF  (PI-LANGUAGE-IS-FR                                          CL*18
00460                   AND                                                CL*18
00461              (FUNCTI = 'g' OR 'G'))                                  CL*18
00462              OR                                                      CL*18
00463          FUNCTI = 's' OR 'S'                                         CL*18
00464          GO TO 4500-SAVE-DATA.                                       CL*18
00465                                                                   EL1042
00466      IF  PI-TOTAL-LINES = 0                                          CL**4
00467         MOVE ER-0048 TO EMI-ERROR                                 EL1042
00468         MOVE -1 TO FUNCTL                                         EL1042
00469         MOVE AL-UNBON TO FUNCTA                                   EL1042
00470         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
00471         GO TO 8200-SEND-DATAONLY.                                 EL1042
00472                                                                   EL1042
00473      IF  (PI-LANGUAGE-IS-FR                                          CL*18
00474                   AND                                                CL*18
00475              (FUNCTI = 'V' OR 'v'))                                  CL*18
00476              OR                                                      CL*18
00477          FUNCTI = 'L' OR 'l'                                         CL*18
00478          GO TO 5500-LOOKUP.                                          CL*18
00479                                                                   EL1042
00480      IF  FUNCTI = 'D' OR 'd'                                         CL*13
00481          GO TO 3000-DELETE-LINES.                                    CL*18
00482                                                                      CL*16
00483      IF  FUNCTI = 'c' OR 'C'                                         CL*16
00484          GO TO 3700-COPY-LINES.                                      CL*18
00485                                                                   EL1042
00486      GO TO 3500-INSERT-LINES.                                     EL1042
00487      EJECT                                                        EL1042
00488  2500-LINE-CHECK.                                                 EL1042
00489      IF  LINE1L = ZEROS AND                                          CL**4
00490         LINE2L = ZEROS                                            EL1042
00491         MOVE ER-0069 TO EMI-ERROR                                 EL1042
00492         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
00493         MOVE -1 TO LINE1L                                         EL1042
00494         GO TO 8200-SEND-DATAONLY.                                 EL1042
00495                                                                   EL1042
00496      IF  LINE1L NOT = ZEROS                                          CL**4
00497         IF  LINE1I NOT NUMERIC OR                                    CL**4
00498            LINE1I GREATER PI-TOTAL-LINES                          EL1042
00499            MOVE ER-0031 TO EMI-ERROR                              EL1042
00500            MOVE AL-UNBON TO LINE1A                                EL1042
00501            MOVE -1 TO LINE1L                                      EL1042
00502            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1042
00503            GO TO 8200-SEND-DATAONLY                               EL1042
00504           ELSE                                                    EL1042
00505            IF  LINE2L = ZEROS                                        CL**4
00506               MOVE 1 TO LINE2I                                    EL1042
00507              ELSE                                                 EL1042
00508               IF  FUNCTI = 'I' OR 'i'                                CL*13
00509                  GO TO 2510-MAX-CHECK                             EL1042
00510                 ELSE                                              EL1042
00511                  IF  LINE2I NOT NUMERIC                              CL**4
00512                     MOVE AL-UNBON TO LINE2A                       EL1042
00513                     MOVE ER-0032 TO EMI-ERROR                     EL1042
00514                     MOVE -1      TO LINE2L                           CL*10
00515                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT      EL1042
00516                     GO TO 8200-SEND-DATAONLY                      EL1042
00517                     ELSE                                          EL1042
00518                     NEXT SENTENCE                                 EL1042
00519        ELSE                                                       EL1042
00520         IF  LINE2L = ZEROS                                           CL**4
00521            NEXT SENTENCE                                          EL1042
00522           ELSE                                                    EL1042
00523            MOVE -1 TO LINE2L                                      EL1042
00524            MOVE ER-0041 TO EMI-ERROR                              EL1042
00525            MOVE AL-UNBON TO LINE2A                                EL1042
00526            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1042
00527            GO TO 8200-SEND-DATAONLY.                              EL1042
00528                                                                   EL1042
00529      GO TO 2599-EXIT.                                             EL1042
00530                                                                   EL1042
00531  2510-MAX-CHECK.                                                  EL1042
00532      IF  LINE2I NOT NUMERIC                                          CL**4
00533         MOVE -1 TO LINE2L                                         EL1042
00534         MOVE ER-0032 TO EMI-ERROR                                 EL1042
00535         MOVE AL-UNBON TO LINE2A                                   EL1042
00536         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
00537         GO TO 8200-SEND-DATAONLY                                  EL1042
00538        ELSE                                                       EL1042
00539         COMPUTE ROLL-COUNTER = LINE2I + PI-TOTAL-LINES            EL1042
00540         IF  ROLL-COUNTER GREATER THAN MAX-LINES                      CL**4
00541            MOVE -1 TO LINE2L                                      EL1042
00542            MOVE ER-0044 TO EMI-ERROR                              EL1042
00543            MOVE AL-UNBON TO LINE2A                                EL1042
00544            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1042
00545            GO TO 8200-SEND-DATAONLY.                              EL1042
00546                                                                   EL1042
00547  2599-EXIT.                                                       EL1042
00548       EXIT.                                                          CL*16
00549      EJECT                                                           CL*16
00550                                                                      CL*16
00551  2600-C-LINE-CHECK.                                                  CL*16
00552                                                                      CL*16
00553      IF  LINE1L = ZEROS                                              CL*16
00554          MOVE ER-0069 TO EMI-ERROR                                   CL*16
00555          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*16
00556          MOVE -1 TO LINE1L                                           CL*16
00557          GO TO 8200-SEND-DATAONLY.                                   CL*16
00558                                                                      CL*16
00559      IF  LINE1I NOT NUMERIC                                          CL*16
00560              OR                                                      CL*16
00561          LINE1I GREATER PI-TOTAL-LINES                               CL*16
00562          MOVE ER-0031 TO EMI-ERROR                                   CL*16
00563          MOVE AL-UNBON TO LINE1A                                     CL*16
00564          MOVE -1 TO LINE1L                                           CL*16
00565          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*16
00566          GO TO 8200-SEND-DATAONLY.                                   CL*16
00567                                                                      CL*16
00568      IF  LINE2L GREATER THAN ZEROS                                   CL*16
00569          IF  LINE2I NOT NUMERIC                                      CL*16
00570                  OR                                                  CL*16
00571              LINE2I GREATER PI-TOTAL-LINES                           CL*16
00572              MOVE ER-0032 TO EMI-ERROR                               CL*16
00573              MOVE AL-UNBON TO LINE2A                                 CL*16
00574              MOVE -1 TO LINE2L                                       CL*16
00575              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*16
00576              GO TO 8200-SEND-DATAONLY.                               CL*16
00577                                                                      CL*16
00578  2600-EXIT.                                                          CL*16
00579       EXIT.                                                       EL1042
00580      EJECT                                                        EL1042
00581  3000-DELETE-LINES.                                               EL1042
00582      IF  LINE2L = ZEROS AND LINE2I = 1                               CL**4
00583         MOVE LINE1I TO LINE2I.                                    EL1042
00584                                                                   EL1042
00585      IF  PI-RETURN-TO-PROGRAM = PGM-EM152                            CL**3
00586              OR                                                      CL**3
00587          PI-RETURN-TO-PROGRAM = PGM-EL152                            CL**3
00588              OR                                                      CL**3
00589          PI-RETURN-TO-PROGRAM = PGM-EL689                            CL**3
00590                                                                      CL**3
00591          IF  LINE1I LESS 8                                           CL**3
00592                   AND                                                CL**7
00593              PI-CREATE-LABELS                                        CL**7
00594              MOVE ER-0212 TO EMI-ERROR                               CL**3
00595              MOVE AL-UNBON TO LINE1A                                 CL**3
00596              MOVE -1 TO LINE1L                                       CL**3
00597              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**3
00598              GO TO 8200-SEND-DATAONLY.                               CL**3
00599                                                                   EL1042
00600      IF  LINE2I GREATER PI-TOTAL-LINES OR LESS LINE1I                CL**4
00601         MOVE ER-0049 TO EMI-ERROR                                 EL1042
00602         MOVE AL-UNBON TO LINE2A                                   EL1042
00603         MOVE -1 TO LINE2L                                         EL1042
00604         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
00605         GO TO 8200-SEND-DATAONLY.                                 EL1042
00606                                                                   EL1042
00607      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1042
00608                                                                   EL1042
00609                                                                      CL**5
00610      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1042
00611              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1042
00612              SC-INDX GREATER NUM-LINES-PER-SCREEN.                EL1042
00613                                                                   EL1042
00614      IF  NOT EMI-NO-ERRORS                                           CL**4
00615         GO TO 8200-SEND-DATAONLY.                                 EL1042
00616                                                                   EL1042
00617      SET TB-INDX TO LINE1I.                                       EL1042
00618      COMPUTE ROLL-COUNTER = LINE2I - LINE1I + 1.                  EL1042
00619                                                                   EL1042
00620      IF  LINE2I NOT = PI-TOTAL-LINES                                 CL**4
00621         SET TB-INDX1 TO LINE2I                                    EL1042
00622         SET TB-INDX1 UP BY 1                                      EL1042
00623         PERFORM 3100-DELETE-TABLE-ENTRIES                         EL1042
00624                 UNTIL TB-INDX1 GREATER PI-TOTAL-LINES.            EL1042
00625                                                                   EL1042
00626      PERFORM 3150-BLANK-TABLE-ENTRIES                             EL1042
00627              ROLL-COUNTER TIMES.                                  EL1042
00628                                                                   EL1042
00629      SUBTRACT ROLL-COUNTER FROM PI-TOTAL-LINES.                   EL1042
00630                                                                   EL1042
00631      IF  PI-CURRENT-LINE GREATER PI-TOTAL-LINES                      CL**4
00632         MOVE PI-TOTAL-LINES       TO PI-CURRENT-LINE                 CL*10
00633         SUBTRACT 1 FROM PI-CURRENT-LINE.                          EL1042
00634                                                                   EL1042
00635      SET TB-INDX  TO PI-CURRENT-LINE.                             EL1042
00636      MOVE LOW-VALUES TO EL104BI.                                  EL1042
00637                                                                   EL1042
00638      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1042
00639             VARYING SC-INDX FROM 1 BY 1 UNTIL                     EL1042
00640             SC-INDX GREATER NUM-LINES-PER-SCREEN.                 EL1042
00641                                                                   EL1042
00642      MOVE 1 TO PI-UPDATE-SW.                                      EL1042
00643                                                                   EL1042
00644      IF  PI-TOTAL-LINES = ZEROS                                      CL**4
00645         MOVE ZEROS TO PI-CURRENT-LINE.                            EL1042
00646                                                                   EL1042
00647      GO TO 8100-SEND-INITIAL-MAP.                                 EL1042
00648      EJECT                                                        EL1042
00649  3100-DELETE-TABLE-ENTRIES.                                       EL1042
00650      MOVE REC-ENT (TB-INDX1) TO REC-ENT (TB-INDX).                EL1042
00651      SET TB-INDX TB-INDX1 UP BY 1.                                EL1042
00652                                                                   EL1042
00653  3150-BLANK-TABLE-ENTRIES.                                        EL1042
00654      MOVE SPACES TO REC-ENT (TB-INDX).                            EL1042
00655      SET TB-INDX UP BY 1.                                         EL1042
00656      EJECT                                                        EL1042
00657  3500-INSERT-LINES.                                               EL1042
00658                                                                      CL**3
00659      IF  PI-RETURN-TO-PROGRAM = PGM-EM152                            CL**3
00660              OR                                                      CL**3
00661          PI-RETURN-TO-PROGRAM = PGM-EL152                            CL**3
00662              OR                                                      CL**3
00663          PI-RETURN-TO-PROGRAM = PGM-EL689                            CL**3
00664                                                                      CL**3
00665          IF  LINE1I LESS 7                                           CL**3
00666                  AND                                                 CL**7
00667              PI-CREATE-LABELS                                        CL**7
00668              MOVE ER-0212  TO EMI-ERROR                              CL**3
00669              MOVE AL-UNBON TO LINE1A                                 CL**3
00670              MOVE -1       TO LINE1L                                 CL**3
00671              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**3
00672              GO TO 8200-SEND-DATAONLY.                               CL**3
00673                                                                   EL1042
00674      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1042
00675                                                                   EL1042
00676      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1042
00677              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1042
00678              SC-INDX GREATER NUM-LINES-PER-SCREEN.                EL1042
00679                                                                   EL1042
00680      IF  NOT EMI-NO-ERRORS                                           CL**4
00681         GO TO 8200-SEND-DATAONLY.                                 EL1042
00682                                                                   EL1042
00683      SET TB-INDX  TO PI-TOTAL-LINES.                              EL1042
00684      ADD LINE2I   TO PI-TOTAL-LINES.                              EL1042
00685      SET TB-INDX1 TO PI-TOTAL-LINES.                              EL1042
00686                                                                   EL1042
00687      PERFORM 3600-INSERT-TABLE-ENTRIES                            EL1042
00688              UNTIL TB-INDX = LINE1I.                              EL1042
00689                                                                   EL1042
00690      SET TB-INDX UP BY 1.                                         EL1042
00691                                                                   EL1042
00692      COMPUTE ROLL-COUNTER =                                       EL1042
00693                       PI-CURRENT-LINE + NUM-LINES-PER-SCREEN.     EL1042
00694                                                                   EL1042
00695      IF  TB-INDX NOT LESS ROLL-COUNTER OR                            CL**4
00696                     LESS PI-CURRENT-LINE                          EL1042
00697         SET SC-INDX TO 1                                          EL1042
00698         SET SC-INDX DOWN BY 1                                     EL1042
00699        ELSE                                                       EL1042
00700         SET ROLL-COUNTER TO TB-INDX                               EL1042
00701         COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE + 1 EL1042
00702         SET SC-INDX TO ROLL-COUNTER.                              EL1042
00703                                                                   EL1042
00704      PERFORM 3150-BLANK-TABLE-ENTRIES LINE2I TIMES.               EL1042
00705      SET TB-INDX     TO PI-CURRENT-LINE.                          EL1042
00706      MOVE LOW-VALUES TO EL104BI.                                  EL1042
00707                                                                   EL1042
00708      IF  SC-INDX NOT = ZERO                                          CL**4
00709         MOVE -1 TO SC-TEXTL (SC-INDX).                            EL1042
00710                                                                   EL1042
00711      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1042
00712             VARYING SC-INDX FROM 1 BY 1 UNTIL                     EL1042
00713             SC-INDX GREATER NUM-LINES-PER-SCREEN.                 EL1042
00714                                                                   EL1042
00715      MOVE 1 TO PI-UPDATE-SW.                                      EL1042
00716      GO TO 8100-SEND-INITIAL-MAP.                                 EL1042
00717                                                                   EL1042
00718  3600-INSERT-TABLE-ENTRIES.                                       EL1042
00719      MOVE REC-ENT (TB-INDX)       TO REC-ENT (TB-INDX1).             CL*10
00720      SET TB-INDX TB-INDX1 DOWN BY 1.                              EL1042
00721      EJECT                                                           CL*16
00722  3700-COPY-LINES.                                                    CL*16
00723                                                                      CL*16
00724      IF  PI-RETURN-TO-PROGRAM = PGM-EM152                            CL*16
00725              OR                                                      CL*16
00726          PI-RETURN-TO-PROGRAM = PGM-EL152                            CL*16
00727              OR                                                      CL*16
00728          PI-RETURN-TO-PROGRAM = PGM-EL689                            CL*16
00729                                                                      CL*16
00730          IF  LINE1I LESS 7                                           CL*16
00731                  AND                                                 CL*16
00732              PI-CREATE-LABELS                                        CL*16
00733              MOVE ER-0212        TO EMI-ERROR                        CL*16
00734              MOVE AL-UNBON       TO LINE1A                           CL*16
00735              MOVE -1             TO LINE1L                           CL*16
00736              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*16
00737              GO TO 8200-SEND-DATAONLY.                               CL*16
00738                                                                      CL*16
00739      PERFORM 7450-SET-INDX THRU 7450-EXIT.                           CL*16
00740                                                                      CL*16
00741      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT            CL*16
00742              VARYING                                                 CL*16
00743          SC-INDX FROM 1 BY 1                                         CL*16
00744              UNTIL                                                   CL*16
00745          SC-INDX GREATER NUM-LINES-PER-SCREEN.                       CL*16
00746                                                                      CL*16
00747      IF  NOT EMI-NO-ERRORS                                           CL*16
00748         GO TO 8200-SEND-DATAONLY.                                    CL*16
00749                                                                      CL*16
00750      SET TB-INDX                  TO LINE1I.                         CL*16
00751      MOVE REC-ENT (TB-INDX)       TO W-HOLD-LINE.                    CL*16
00752      SET TB-INDX                  TO PI-TOTAL-LINES.                 CL*16
00753      ADD +1                       TO PI-TOTAL-LINES.                 CL*16
00754      SET TB-INDX1                 TO PI-TOTAL-LINES.                 CL*16
00755                                                                      CL*16
00756      IF  LINE2L NOT GREATER THAN ZEROS                               CL*16
00757          MOVE LINE1I              TO LINE2I.                         CL*16
00758                                                                      CL*16
00759      PERFORM 3720-INSERT-TABLE-ENTRIES                               CL*16
00760              UNTIL                                                   CL*16
00761          TB-INDX = LINE2I.                                           CL*16
00762                                                                      CL*16
00763      SET TB-INDX UP BY 1.                                            CL*16
00764                                                                      CL*16
00765      COMPUTE ROLL-COUNTER                                            CL*16
00766          = PI-CURRENT-LINE + NUM-LINES-PER-SCREEN.                   CL*16
00767                                                                      CL*16
00768      IF  TB-INDX NOT LESS ROLL-COUNTER                               CL*16
00769              OR                                                      CL*16
00770          TB-INDX LESS PI-CURRENT-LINE                                CL*16
00771          SET SC-INDX              TO 1                               CL*16
00772          SET SC-INDX DOWN BY 1                                       CL*16
00773                                                                      CL*16
00774      ELSE                                                            CL*16
00775          SET ROLL-COUNTER TO TB-INDX                                 CL*16
00776          COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE + 1   CL*16
00777          SET SC-INDX             TO ROLL-COUNTER.                    CL*16
00778                                                                      CL*16
00779      MOVE W-HOLD-LINE            TO REC-ENT (TB-INDX).               CL*16
00780      SET TB-INDX                 TO PI-CURRENT-LINE.                 CL*16
00781      MOVE LOW-VALUES             TO EL104BI.                         CL*16
00782                                                                      CL*16
00783      IF  SC-INDX NOT = ZERO                                          CL*16
00784         MOVE -1                  TO SC-TEXTL (SC-INDX).              CL*16
00785                                                                      CL*16
00786      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                       CL*16
00787              VARYING                                                 CL*16
00788          SC-INDX FROM 1 BY 1                                         CL*16
00789              UNTIL                                                   CL*16
00790          SC-INDX GREATER NUM-LINES-PER-SCREEN.                       CL*16
00791                                                                      CL*16
00792      MOVE 1                      TO PI-UPDATE-SW.                    CL*16
00793      GO TO 8100-SEND-INITIAL-MAP.                                    CL*16
00794                                                                      CL*16
00795  3720-INSERT-TABLE-ENTRIES.                                          CL*16
00796                                                                      CL*16
00797      MOVE REC-ENT (TB-INDX)      TO REC-ENT (TB-INDX1).              CL*16
00798      SET TB-INDX TB-INDX1 DOWN BY 1.                                 CL*16
00799                                                                      CL*16
00800      EJECT                                                        EL1042
00801  4000-CHANGE-ROUTINE.                                             EL1042
00802                                                                      CL**6
00803      IF  ARCHBL GREATER THAN ZEROS                                   CL*15
00804              AND                                                     CL*15
00805          (CREDIT-SESSION                                             CL*20
00806                   OR                                                 CL*20
00807              MORTGAGE-SESSION)                                       CL*20
00808                                                                      CL*15
00809          IF  ARCHBI EQUAL 'Y'                                        CL*15
00810                  OR                                                  CL*15
00811              ARCHBI EQUAL SPACES                                     CL*15
00812              MOVE ARCHBI         TO PI-1042-ARCHIVE-IND              CL*15
00813                                                                      CL*15
00814          ELSE                                                        CL*15
00815              MOVE ER-7375        TO EMI-ERROR                        CL*15
00816              MOVE -1             TO ARCHBL                           CL*15
00817              MOVE AL-UABON       TO ARCHBA                           CL*15
00818              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*15
00819                                                                      CL*15
00820      IF  ARCHBL GREATER THAN ZEROS                                   CL*19
00821              AND                                                     CL*19
00822          CLAIM-SESSION                                               CL*20
00823              AND                                                     CL*20
00824          PI-COMPANY-ID = 'DMD'                                       CL*19
00825                                                                      CL*19
00826          IF ARCHBI = 'B' OR ' ' OR 'b'                               CL*21
00827              INSPECT ARCHBI CONVERTING LOWER-CASE TO                 CL*19
00828                                        UPPER-CASE                    CL*19
00829              MOVE ARCHBI         TO PI-1042-ARCHIVE-IND              CL*19
00830              MOVE 1              TO PI-UPDATE-SW                     CL*19
00831              MOVE AL-UANON       TO ARCHBA                           CL*19
00832          ELSE                                                        CL*19
00833              MOVE ER-8126        TO EMI-ERROR                        CL*19
00834              MOVE -1             TO ARCHBL                           CL*19
00835              MOVE AL-UABON       TO ARCHBA                           CL*19
00836              MOVE 'Y'            TO WS-8126-ERROR-SW                 CL*19
00837              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*19
00838              GO TO 8200-SEND-DATAONLY.                               CL*19
00839                                                                      CL*19
00840      IF  FORMSQBL GREATER THAN ZEROS                                 CL*10
00841              AND                                                     CL*15
00842          (MORTGAGE-SESSION                                           CL*15
00843                  OR                                                  CL*15
00844              CREDIT-SESSION)                                         CL*15
00845                                                                      CL**6
00846          IF  FORMSQBI EQUAL 'Y'                                      CL*10
00847                  OR                                                  CL*10
00848              FORMSQBI EQUAL SPACES                                   CL*10
00849              MOVE FORMSQBI       TO PI-FORM-SQUEEZE-CONTROL          CL*10
00850                                                                      CL**5
00851          ELSE                                                        CL**5
00852              MOVE ER-9349        TO EMI-ERROR                        CL*10
00853              MOVE -1             TO FORMSQBL                         CL*10
00854              MOVE AL-UABON       TO FORMSQBA                         CL*10
00855              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*15
00856                                                                      CL**5
00857      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1042
00858      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1042
00859              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1042
00860              SC-INDX GREATER NUM-LINES-PER-SCREEN.                EL1042
00861                                                                   EL1042
00862      IF  NOT EMI-NO-ERRORS                                           CL**4
00863         GO TO 8200-SEND-DATAONLY.                                 EL1042
00864                                                                   EL1042
00865      MOVE SPACES TO ERRMSGBO                                      EL1042
00866      GO TO 8200-SEND-DATAONLY.                                    EL1042
00867                                                                   EL1042
00868      EJECT                                                        EL1042
00869  4500-SAVE-DATA.                                                  EL1042
00870                                                                      CL**6
00871      IF  ARCHBL GREATER THAN ZEROS                                   CL*15
00872              AND                                                     CL*15
00873          CREDIT-SESSION                                              CL*15
00874                                                                      CL*15
00875          IF  ARCHBI EQUAL 'Y'                                        CL*15
00876                  OR                                                  CL*15
00877              ARCHBI EQUAL SPACES                                     CL*15
00878              MOVE ARCHBI       TO PI-1042-ARCHIVE-IND                CL*15
00879                                                                      CL*15
00880          ELSE                                                        CL*15
00881              MOVE ER-7375      TO EMI-ERROR                          CL*15
00882              MOVE -1           TO ARCHBL                             CL*15
00883              MOVE AL-UABON     TO ARCHBA                             CL*15
00884              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*15
00885                                                                      CL*19
00886      IF  ARCHBL GREATER THAN ZEROS                                   CL*19
00887              AND                                                     CL*19
00888          CLAIM-SESSION                                               CL*19
00889              AND                                                     CL*19
00890          PI-COMPANY-ID = 'DMD'                                       CL*19
00891                                                                      CL*19
00892          IF ARCHBI = 'B' OR ' ' OR 'b'                               CL*21
00893              INSPECT ARCHBI CONVERTING LOWER-CASE TO                 CL*19
00894                                        UPPER-CASE                    CL*19
00895              MOVE ARCHBI         TO PI-1042-ARCHIVE-IND              CL*19
00896              MOVE 1              TO PI-UPDATE-SW                     CL*19
00897              MOVE AL-UANON      TO ARCHBA                            CL*19
00898          ELSE                                                        CL*19
00899              MOVE ER-8126        TO EMI-ERROR                        CL*19
00900              MOVE -1             TO ARCHBL                           CL*19
00901              MOVE AL-UABON       TO ARCHBA                           CL*19
00902              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*19
00903              GO TO 8200-SEND-DATAONLY.                               CL*19
00904                                                                      CL*15
00905      IF  FORMSQBL GREATER THAN ZEROS                                 CL**5
00906              AND                                                     CL*15
00907          (MORTGAGE-SESSION                                           CL*15
00908                  OR                                                  CL*15
00909              CREDIT-SESSION)                                         CL*15
00910                                                                      CL**6
00911          IF  FORMSQBI EQUAL 'Y'                                      CL**5
00912                  OR                                                  CL**5
00913              FORMSQBI EQUAL SPACES                                   CL**5
00914              MOVE FORMSQBI     TO PI-FORM-SQUEEZE-CONTROL            CL**5
00915                                                                      CL**5
00916          ELSE                                                        CL**5
00917              MOVE ER-9349      TO EMI-ERROR                          CL**5
00918              MOVE -1           TO FORMSQBL                           CL*10
00919              MOVE AL-UABON     TO FORMSQBA                           CL*10
00920              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*15
00921                                                                      CL**5
00922      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1042
00923                                                                   EL1042
00924      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1042
00925              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1042
00926              SC-INDX GREATER NUM-LINES-PER-SCREEN.                EL1042
00927                                                                   EL1042
00928      IF  NOT EMI-NO-ERRORS                                           CL**4
00929         GO TO 8200-SEND-DATAONLY.                                 EL1042
00930                                                                   EL1042
00931      IF  PI-RETURN-TO-PROGRAM = PGM-EM152                            CL**3
00932              OR                                                      CL**3
00933          PI-RETURN-TO-PROGRAM = PGM-EL152                            CL**3
00934              OR                                                      CL**3
00935          PI-RETURN-TO-PROGRAM = PGM-EL689                            CL**3
00936                                                                      CL**3
00937          PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT                   CL**3
00938          GO TO 9410-RETURN.                                          CL**3
00939                                                                   EL1042
00940      EXEC CICS HANDLE CONDITION                                   EL1042
00941           NOTFND  (4610-ENDBR)                                    EL1042
00942           NOTOPEN (6000-NOT-OPEN)                                 EL1042
00943           ENDFILE (4610-ENDBR)                                    EL1042
00944       END-EXEC.                                                   EL1042
00945                                                                   EL1042
00946      EXEC CICS DELETE                                             EL1042
00947           DATASET   (FILE-ID)                                     EL1042
00948           RIDFLD    (FILE-KEY)                                    EL1042
00949           KEYLENGTH (13)                                          EL1042
00950           GENERIC                                                 EL1042
00951       END-EXEC.                                                   EL1042
00952                                                                   EL1042
00953  4610-ENDBR.                                                      EL1042
00954      EXEC CICS GETMAIN                                            EL1042
00955           SET(ADDRESS OF TEXT-FILES)                                 CL*19
00956           LENGTH(FILE-LENGTH)                                     EL1042
00957       END-EXEC.                                                   EL1042
00958                                                                   EL1042
00959      PERFORM 4700-WRITE-FILE THRU 4799-EXIT                       EL1042
00960              VARYING TB-INDX FROM 1 BY 1 UNTIL                    EL1042
00961              TB-INDX GREATER PI-TOTAL-LINES.                      EL1042
00962                                                                   EL1042
00963      GO TO 9410-RETURN.                                           EL1042
00964                                                                   EL1042
00965  4700-WRITE-FILE.                                                 EL1042
00966      ADD 1 TO SEQ.                                                EL1042
00967      MOVE FILE-KEY TO TX-CONTROL-PRIMARY.                         EL1042
00968                                                                   EL1042
00969      IF  PI-ENTRY-CD-1 = '1'                                         CL**4
00970          MOVE  'TL' TO TEXT-FILE-ID                               EL1042
00971      ELSE                                                         EL1042
00972          IF  PI-ENTRY-CD-1 = '2'                                     CL**4
00973              MOVE 'TF' TO TEXT-FILE-ID                            EL1042
00974          ELSE                                                     EL1042
00975              MOVE 'TH' TO TEXT-FILE-ID.                           EL1042
00976                                                                   EL1042
00977      MOVE REC-PC (TB-INDX)   TO TX-PROCESS-CONTROL.               EL1042
00978      MOVE REC-SQ (TB-INDX)   TO TX-LINE-SQUEEZE-CONTROL.             CL**5
00979      MOVE REC-TEXT (TB-INDX) TO TX-TEXT-LINE.                     EL1042
00980      MOVE PI-PROCESSOR-ID    TO TX-LAST-MAINTENANCED-BY              CL*17
00981                                 MNTBYO.                              CL*17
00982      MOVE SAVE-BIN-DATE      TO TX-LAST-MAINTENANCED-DT.             CL*17
00983      MOVE SAVE-DATE          TO MNTONO.                              CL*17
00984                                                                      CL*15
00985      IF  MORTGAGE-SESSION                                            CL*15
00986              OR                                                      CL*15
00987          CREDIT-SESSION                                              CL*15
00988          MOVE PI-FORM-SQUEEZE-CONTROL                                CL*15
00989                              TO TX-FORM-SQUEEZE-CONTROL              CL*15
00990          MOVE PI-1042-ARCHIVE-IND                                    CL*19
00991                              TO TX-ARCHIVE-SW.                       CL*19
00992                                                                      CL*19
00993      IF  PI-COMPANY-ID = 'DMD'                                       CL*20
00994              AND                                                     CL*20
00995          CLAIM-SESSION                                               CL*19
00996          MOVE PI-1042-ARCHIVE-IND                                    CL*15
00997                              TO TX-BSR-CODE.                         CL*20
00998                                                                   EL1042
00999      IF  TX-PROCESS-CONTROL = SPACES                                 CL**4
01000         MOVE ZEROS TO TX-PROCESS-CONTROL.                         EL1042
01001                                                                   EL1042
01002      EXEC CICS WRITE                                              EL1042
01003           DATASET (FILE-ID)                                       EL1042
01004           FROM    (TEXT-FILES)                                    EL1042
01005           RIDFLD  (FILE-KEY)                                      EL1042
01006       END-EXEC.                                                   EL1042
01007  4799-EXIT.                                                       EL1042
01008       EXIT.                                                       EL1042
01009                                                                   EL1042
01010      EJECT                                                        EL1042
01011  5000-ADD-NEW-LINES.                                              EL1042
01012                                                                      CL**5
01013      IF  ARCHBL GREATER THAN ZEROS                                   CL*15
01014              AND                                                     CL*15
01015          (CREDIT-SESSION                                             CL*20
01016                  OR                                                  CL*20
01017              MORTGAGE-SESSION)                                       CL*20
01018                                                                      CL*15
01019          IF  ARCHBI EQUAL 'Y'                                        CL*15
01020                  OR                                                  CL*15
01021              ARCHBI EQUAL SPACES                                     CL*15
01022              MOVE ARCHBI       TO PI-1042-ARCHIVE-IND                CL*15
01023                                                                      CL*15
01024          ELSE                                                        CL*15
01025              MOVE ER-7375      TO EMI-ERROR                          CL*15
01026              MOVE -1           TO ARCHBL                             CL*15
01027              MOVE AL-UABON     TO ARCHBA                             CL*15
01028              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*15
01029 *                                                                    CL*15
01030 *    ELSE                                                            CL*15
01031 *        MOVE SPACES           TO PI-1042-ARCHIVE-IND.               CL*15
01032                                                                      CL*19
01033      IF  ARCHBL GREATER THAN ZEROS                                   CL*19
01034              AND                                                     CL*19
01035          CLAIM-SESSION                                               CL*19
01036              AND                                                     CL*19
01037          PI-COMPANY-ID = 'DMD'                                       CL*19
01038                                                                      CL*19
01039          IF ARCHBI = 'B' OR ' ' OR 'b'                               CL*21
01040              INSPECT ARCHBI CONVERTING LOWER-CASE TO                 CL*19
01041                                        UPPER-CASE                    CL*19
01042              MOVE ARCHBI         TO PI-1042-ARCHIVE-IND              CL*19
01043              MOVE 1              TO PI-UPDATE-SW                     CL*19
01044              MOVE AL-UANON      TO ARCHBA                            CL*19
01045          ELSE                                                        CL*19
01046              MOVE ER-8126        TO EMI-ERROR                        CL*19
01047              MOVE -1             TO ARCHBL                           CL*19
01048              MOVE AL-UABON       TO ARCHBA                           CL*19
01049              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*19
01050              GO TO 8200-SEND-DATAONLY.                               CL*19
01051                                                                      CL*19
01052                                                                      CL*15
01053      IF  FORMSQBL GREATER THAN ZEROS                                 CL**5
01054              AND                                                     CL*15
01055          (MORTGAGE-SESSION                                           CL*15
01056                  OR                                                  CL*15
01057              CREDIT-SESSION)                                         CL*15
01058                                                                      CL**6
01059          IF  FORMSQBI EQUAL 'Y'                                      CL**5
01060                  OR                                                  CL**5
01061              FORMSQBI EQUAL SPACES                                   CL**5
01062              MOVE FORMSQBI       TO PI-FORM-SQUEEZE-CONTROL          CL*10
01063                                                                      CL**5
01064          ELSE                                                        CL**5
01065              MOVE ER-9349        TO EMI-ERROR                        CL*10
01066              MOVE -1             TO FORMSQBL                         CL*10
01067              MOVE AL-UABON       TO FORMSQBA                         CL*10
01068              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*16
01069 *                                                                    CL*15
01070 *    ELSE                                                            CL*15
01071 *        MOVE SPACES             TO PI-FORM-SQUEEZE-CONTROL.         CL*15
01072                                                                      CL**5
01073      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1042
01074                                                                   EL1042
01075      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1042
01076              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1042
01077              SC-INDX GREATER NUM-LINES-PER-SCREEN                 EL1042
01078                                                                   EL1042
01079      IF  NOT EMI-NO-ERRORS                                           CL**4
01080         GO TO 8200-SEND-DATAONLY.                                 EL1042
01081                                                                   EL1042
01082      IF  PI-TOTAL-LINES EQUAL ZEROS                                  CL*10
01083          MOVE LOW-VALUES TO EL104BI                                  CL*10
01084          MOVE AL-UANON   TO FUNCTA                                   CL*10
01085          MOVE 'A'        TO FUNCTI                                   CL*18
01086          GO TO 8100-SEND-INITIAL-MAP.                                CL*10
01087                                                                      CL*10
01088      MOVE PI-TOTAL-LINES                                             CL*10
01089                      TO PI-CURRENT-LINE.                             CL*10
01090                                                                      CL*10
01091      MOVE LOW-VALUES TO EL104BI.                                  EL1042
01092      SET TB-INDX     TO PI-CURRENT-LINE.                          EL1042
01093      MOVE 'A'        TO FUNCTI.                                   EL1042
01094      MOVE -1         TO SC-TEXTL (2).                             EL1042
01095      MOVE AL-UANON   TO FUNCTA.                                   EL1042
01096                                                                   EL1042
01097      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1042
01098              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1042
01099              SC-INDX GREATER NUM-LINES-PER-SCREEN.                EL1042
01100                                                                   EL1042
01101      GO TO 8100-SEND-INITIAL-MAP.                                 EL1042
01102      EJECT                                                        EL1042
01103  5500-LOOKUP.                                                     EL1042
01104      SET TB-INDX TO PI-CURRENT-LINE.                              EL1042
01105                                                                   EL1042
01106      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1042
01107              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1042
01108              SC-INDX GREATER NUM-LINES-PER-SCREEN.                EL1042
01109                                                                   EL1042
01110      IF  NOT EMI-NO-ERRORS                                           CL**4
01111         GO TO 8200-SEND-DATAONLY.                                 EL1042
01112                                                                   EL1042
01113      MOVE LINE1I TO PI-CURRENT-LINE.                              EL1042
01114      SET TB-INDX TO PI-CURRENT-LINE.                              EL1042
01115      MOVE LOW-VALUES TO EL104BI.                                  EL1042
01116                                                                   EL1042
01117      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1042
01118              VARYING SC-INDX FROM 1 BY 1                          EL1042
01119              UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN.          EL1042
01120                                                                   EL1042
01121      GO TO 8100-SEND-INITIAL-MAP.                                 EL1042
01122      EJECT                                                        EL1042
01123  6000-NOT-OPEN.                                                   EL1042
01124      IF  PI-ENTRY-CD-1 = '1'                                         CL**4
01125          MOVE ER-0013 TO EMI-ERROR                                EL1042
01126      ELSE                                                         EL1042
01127          IF  PI-ENTRY-CD-1 = '2'                                     CL**4
01128              MOVE ER-0014 TO EMI-ERROR                            EL1042
01129          ELSE                                                     EL1042
01130              MOVE ER-0015 TO EMI-ERROR.                           EL1042
01131                                                                   EL1042
01132      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1042
01133      GO TO 8200-SEND-DATAONLY.                                    EL1042
01134      EJECT                                                        EL1042
01135  7000-BUILD-TABLE.                                                EL1042
01136      SET TB-INDX TO 1.                                            EL1042
01137      MOVE ZEROS TO PI-TOTAL-LINES                                 EL1042
01138                    PI-CURRENT-LINE                                EL1042
01139                    PI-TEMP-STOR-ITEMS                             EL1042
01140                    PI-UPDATE-SW.                                  EL1042
01141                                                                   EL1042
01142      MOVE LOW-VALUES TO EL104BI.                                  EL1042
01143      PERFORM 7500-READ-TS THRU 7599-EXIT.                         EL1042
01144                                                                   EL1042
01145      IF  PI-TEMP-STOR-ITEMS NOT = ZERO                               CL**4
01146         MOVE ER-0140 TO EMI-ERROR                                 EL1042
01147         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
01148         MULTIPLY PI-TEMP-STOR-ITEMS BY TS-NUM-REC-IN-GROUP GIVING EL1042
01149                  PI-TOTAL-LINES                                   EL1042
01150         MOVE 1 TO PI-CURRENT-LINE                                 EL1042
01151         SET TB-INDX TO 1                                          EL1042
01152         PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                 EL1042
01153                 VARYING SC-INDX FROM 1                            EL1042
01154                 BY 1 UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN   EL1042
01155         GO TO 8100-SEND-INITIAL-MAP.                              EL1042
01156                                                                   EL1042
01157      EXEC CICS HANDLE CONDITION                                   EL1042
01158           NOTFND (7010-ENDBR)                                     EL1042
01159           NOTOPEN(6000-NOT-OPEN)                                  EL1042
01160           ENDFILE(7010-ENDBR)                                     EL1042
01161       END-EXEC.                                                   EL1042
01162                                                                   EL1042
01163      EXEC CICS STARTBR                                            EL1042
01164           DATASET(FILE-ID)                                        EL1042
01165           RIDFLD (FILE-KEY)                                       EL1042
01166           KEYLENGTH(13)                                           EL1042
01167           GENERIC                                                 EL1042
01168           GTEQ                                                    EL1042
01169       END-EXEC.                                                   EL1042
01170                                                                   EL1042
01171  7001-LOOP.                                                       EL1042
01172      EXEC CICS READNEXT                                           EL1042
01173           SET    (ADDRESS OF TEXT-FILES)                             CL*19
01174           DATASET(FILE-ID)                                        EL1042
01175           RIDFLD (FILE-KEY)                                       EL1042
01176       END-EXEC.                                                   EL1042
01177                                                                   EL1042
01178      IF  FILE-PARTIAL-KEY NOT EQUAL OLD-KEY-SAVE                     CL*17
01179          GO TO 7010-ENDBR.                                           CL*17
01180                                                                      CL*17
01181      MOVE TX-PROCESS-CONTROL     TO REC-PC (TB-INDX).                CL*17
01182      PERFORM 7150-TEST-SQUEEZE-LINE THRU 7150-EXIT.                  CL*17
01183      MOVE TX-TEXT-LINE           TO REC-TEXT (TB-INDX).              CL*17
01184      MOVE TX-FORM-SQUEEZE-CONTROL                                    CL*17
01185                                  TO PI-FORM-SQUEEZE-CONTROL.         CL*17
01186                                                                      CL*20
01187      IF  PI-COMPANY-ID = 'DMD'                                       CL*20
01188              AND                                                     CL*20
01189          CLAIM-SESSION                                               CL*20
01190          MOVE TX-BSR-CODE        TO PI-1042-ARCHIVE-IND              CL*20
01191                                                                      CL*20
01192      ELSE                                                            CL*20
01193          MOVE TX-ARCHIVE-SW      TO PI-1042-ARCHIVE-IND.             CL*20
01194                                                                      CL*17
01195      IF  TX-LAST-MAINTENANCED-BY GREATER THAN SPACES                 CL*17
01196          MOVE TX-LAST-MAINTENANCED-BY                                CL*17
01197                                  TO W-SAVE-LAST-MAINT-BY             CL*17
01198          MOVE TX-LAST-MAINTENANCED-DT                                CL*17
01199                                  TO DC-BIN-DATE-1                    CL*17
01200          MOVE ' '                TO DC-OPTION-CODE                   CL*17
01201          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*17
01202                                                                      CL*17
01203          IF  NO-CONVERSION-ERROR                                     CL*17
01204              MOVE DC-GREG-DATE-1-EDIT                                CL*17
01205                                  TO W-SAVE-LAST-MAINT-DT             CL*17
01206                                                                      CL*17
01207          ELSE                                                        CL*17
01208              MOVE SPACES         TO W-SAVE-LAST-MAINT-DT             CL*17
01209                                     W-SAVE-LAST-MAINT-BY.            CL*17
01210                                                                      CL*17
01211      SET TB-INDX UP BY 1                                             CL*17
01212      GO TO 7001-LOOP.                                                CL*17
01213                                                                   EL1042
01214  7010-ENDBR.                                                      EL1042
01215      IF  TB-INDX = 1                                                 CL**4
01216         MOVE ER-0006  TO EMI-ERROR                                EL1042
01217         MOVE 'A'      TO FUNCTI                                   EL1042
01218         MOVE -1       TO SC-TEXTL (1)                             EL1042
01219         MOVE AL-UANON TO FUNCTA                                   EL1042
01220         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
01221         GO TO 8100-SEND-INITIAL-MAP.                              EL1042
01222                                                                   EL1042
01223      EXEC CICS ENDBR                                              EL1042
01224           DATASET(FILE-ID)                                        EL1042
01225       END-EXEC.                                                   EL1042
01226                                                                   EL1042
01227      SET TB-INDX DOWN BY 1.                                       EL1042
01228      SET PI-TOTAL-LINES TO TB-INDX.                               EL1042
01229      MOVE 1 TO PI-CURRENT-LINE.                                   EL1042
01230                                                                   EL1042
01231  7050-FORMAT-LINES.                                               EL1042
01232                                                                      CL**6
01233      SET TB-INDX TO PI-CURRENT-LINE.                              EL1042
01234                                                                   EL1042
01235      IF  MORTGAGE-SESSION                                            CL*20
01236              OR                                                      CL*20
01237          CREDIT-SESSION                                              CL*20
01238          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'                           CL*15
01239              MOVE PI-1042-ARCHIVE-IND                                CL*19
01240                                 TO ARCHBO.                           CL*19
01241                                                                      CL*19
01242      IF  CLAIM-SESSION                                               CL*19
01243              AND                                                     CL*19
01244          PI-COMPANY-ID = 'DMD'                                       CL*19
01245          IF PI-1042-ARCHIVE-IND = 'B' OR ' '                         CL*21
01246              MOVE AL-UANON      TO ARCHBA                            CL*19
01247              MOVE PI-1042-ARCHIVE-IND                                CL*15
01248                                 TO ARCHBO
090505         END-IF
090505     END-IF
01249                                                                      CL*20
090505*        ELSE                                                        CL*20
090505*            MOVE SPACES        TO PI-1042-ARCHIVE-IND               CL*20
090505*                                  ARCHBO.                           CL*20
01253                                                                      CL*15
01254      IF  MORTGAGE-SESSION                                            CL*15
01255              OR                                                      CL*15
01256          CREDIT-SESSION                                              CL*15
01257          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'                       CL*15
01258              MOVE PI-FORM-SQUEEZE-CONTROL                            CL*15
01259                                 TO FORMSQBO.                         CL**5
01260                                                                      CL*17
01261      MOVE W-SAVE-LAST-MAINT-DT  TO MNTONO.                           CL*17
01262      MOVE W-SAVE-LAST-MAINT-BY  TO MNTBYO.                           CL*17
01263                                                                      CL**6
01264      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1042
01265              VARYING SC-INDX FROM 1                               EL1042
01266              BY 1 UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN.     EL1042
01267                                                                   EL1042
01268      GO TO 8100-SEND-INITIAL-MAP.                                 EL1042
01269      EJECT                                                        EL1042
01270  7100-FORMAT-SCREEN.                                              EL1042

           IF REC-SQ (TB-INDX) = 'Z'
              MOVE AL-PANOF            TO SC-TEXTA (SC-INDX)
           END-IF

01272      IF  TB-INDX NOT GREATER PI-TOTAL-LINES                          CL**4
01273          MOVE REC-TEXT (TB-INDX) TO SC-TEXT (SC-INDX)                CL*11
01274          MOVE REC-PC (TB-INDX)   TO SC-PC (SC-INDX)                  CL*15
01275          PERFORM 7151-TEST-SQUEEZE-LINE THRU 7151-EXIT               CL*11
01276          SET ROLL-COUNTER        TO TB-INDX                          CL*11
01277          MOVE ROLL-COUNTER       TO SC-LIN (SC-INDX)                 CL*15
01278          SET TB-INDX UP BY 1                                         CL*11
01279                                                                      CL*15
01280      ELSE                                                            CL*11
01281          IF  FUNCTI = 'A' OR 'a'                                     CL*13
01282                                                                      CL*15
01283              IF  CLAIM-SESSION                                       CL*18
01284                      AND                                             CL*18
01285                  TB-INDX GREATER THAN +1                             CL*18
01286                  MOVE AL-PANOF   TO SC-SQA (SC-INDX)                 CL*18
01287                                                                      CL*18
01288              ELSE                                                    CL*18
01289                  NEXT SENTENCE                                       CL*18
01290                                                                      CL*15
01291          ELSE                                                        CL*15
01292              MOVE AL-PANOF       TO SC-TEXTA (SC-INDX)               CL*15
01293                                     SC-PCA (SC-INDX)                 CL*15
01294                                     SC-SQA (SC-INDX).                CL*15
01295                                                                   EL1042
01296      IF  PI-RETURN-TO-PROGRAM = PGM-EM152                            CL**3
01297              OR                                                      CL**3
01298          PI-RETURN-TO-PROGRAM = PGM-EL152                            CL**3
01299              OR                                                      CL**3
01300          PI-RETURN-TO-PROGRAM = PGM-EL689                            CL**3
01301                                                                      CL**3
01302          MOVE SPACES   TO SC-PC (SC-INDX)                            CL**3
01303          MOVE AL-PANOF TO SC-PCA (SC-INDX)                           CL**5
01304          MOVE SPACES   TO SC-SQ (SC-INDX)                            CL**5
01305          MOVE AL-PANOF TO SC-SQA (SC-INDX).                          CL**5
01306                                                                   EL1042
01307  7100-EXIT.                                                       EL1042
01308       EXIT.                                                       EL1042
01309                                                                      CL**8
01310  7150-TEST-SQUEEZE-LINE.                                             CL**8
01311                                                                      CL**8
01312      MOVE TX-LINE-SQUEEZE-CONTROL                                    CL**8
01313                                  TO W-LINE-SQUEEZE-IND.              CL*10
01314                                                                      CL**8
01315      IF  W-LINE-SQUEEZE-VALID-VALUE                                  CL**8
01316          MOVE TX-LINE-SQUEEZE-CONTROL                                CL**8
01317                              TO REC-SQ (TB-INDX)                     CL**8
01318                                                                      CL**8
01319      ELSE                                                            CL**8
01320          MOVE SPACES         TO REC-SQ (TB-INDX).                    CL**8
01321                                                                      CL**8
01322                                                                      CL*15
01323  7150-EXIT.                                                          CL**8
01324       EXIT.                                                          CL*11
01325                                                                      CL*11
01326  7151-TEST-SQUEEZE-LINE.                                             CL*11
01327                                                                      CL*11
01328      MOVE REC-SQ (TB-INDX)   TO W-LINE-SQUEEZE-IND.                  CL*11
01329                                                                      CL*11
01330      IF  W-LINE-SQUEEZE-VALID-VALUE                                  CL*11
01331          NEXT SENTENCE                                               CL*11
01332                                                                      CL*11
01333      ELSE                                                            CL*11
01334          MOVE SPACES         TO REC-SQ (TB-INDX).                    CL*11
01335                                                                      CL*15
01336      IF  MORTGAGE-SESSION                                            CL*15
01337              OR                                                      CL*15
01338          CREDIT-SESSION                                              CL*15
01339          MOVE REC-SQ (TB-INDX)                                       CL*15
01340                              TO SC-SQ (SC-INDX)                      CL*15
01341                                                                      CL*15
01342      ELSE                                                            CL*15
01343          IF  CLAIM-SESSION                                           CL*15
01344                  AND                                                 CL*15
01345              TB-INDX EQUAL +1                                        CL*15
01346              MOVE REC-SQ (TB-INDX)                                   CL*15
01347                              TO SC-SQ (SC-INDX)                      CL*15
01348                                                                      CL*15
01349          ELSE                                                        CL*15
01350              MOVE ZEROS      TO SC-SQL (SC-INDX)                     CL*15
01351              MOVE AL-SADOF   TO SC-SQA (SC-INDX)                     CL*15
01352              MOVE LOW-VALUES TO SC-SQ  (SC-INDX).                    CL*15
01353                                                                      CL*11
01354  7151-EXIT.                                                          CL*11
01355       EXIT.                                                          CL**8
01356                                                                   EL1042
01357  7200-PUT-TEMP-STOR.                                              EL1042
01358      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL1042
01359      SET TS-INDX TO 1.                                            EL1042
01360      MOVE 0 TO PI-TEMP-STOR-ITEMS.                                EL1042
01361                                                                   EL1042
01362      PERFORM 7300-WRITE-TS THRU 7399-EXIT                         EL1042
01363              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP  EL1042
01364              UNTIL TS-GROUP-WORK NOT LESS PI-TOTAL-LINES.         EL1042
01365  7249-EXIT.                                                       EL1042
01366       EXIT.                                                       EL1042
01367                                                                   EL1042
01368  7250-DELETE-TEMP-STOR.                                           EL1042
01369      EXEC CICS HANDLE CONDITION                                   EL1042
01370           QIDERR(7299-EXIT)                                       EL1042
01371       END-EXEC.                                                   EL1042
01372                                                                   EL1042
01373      EXEC CICS DELETEQ TS                                         EL1042
01374           QUEUE(TS-NAME)                                          EL1042
01375       END-EXEC.                                                   EL1042
01376                                                                   EL1042
01377  7299-EXIT.                                                       EL1042
01378      EXIT.                                                        EL1042
01379      EJECT                                                        EL1042
01380  7300-WRITE-TS.                                                   EL1042
01381      MOVE TS-GROUP (TS-INDX) TO TS-WORK-AREA.                     EL1042
01382      SET TS-INDX UP BY 1.                                         EL1042
01383      ADD 1 TO PI-TEMP-STOR-ITEMS.                                 EL1042
01384                                                                   EL1042
01385      EXEC CICS WRITEQ TS                                          EL1042
01386           FROM  (TS-WORK-AREA)                                    EL1042
01387           QUEUE (TS-NAME)                                         EL1042
01388           LENGTH(TS-LENGTH)                                       EL1042
01389           ITEM  (PI-TEMP-STOR-ITEMS)                              EL1042
01390       END-EXEC.                                                   EL1042
01391                                                                   EL1042
01392  7399-EXIT.                                                       EL1042
01393      EXIT.                                                        EL1042
01394      EJECT                                                        EL1042
01395  7400-PAGE-ROUTINE.                                               EL1042
01396      IF  PFENTRBL NOT = ZEROS                                        CL**4
01397         MOVE -1 TO PFENTRBL                                       EL1042
01398        ELSE                                                       EL1042
01399         MOVE -1 TO FUNCTL.                                        EL1042
01400                                                                   EL1042
01401      IF  PI-TOTAL-LINES = 0                                          CL**4
01402         MOVE ER-0047 TO EMI-ERROR                                 EL1042
01403         MOVE -1 TO FUNCTL                                         EL1042
01404         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
01405         GO TO 8200-SEND-DATAONLY.                                 EL1042
01406                                                                   EL1042
01407      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.     EL1042
01408                                                                   EL1042
01409      IF  TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS           CL**4
01410         MOVE ER-0067 TO EMI-ERROR                                 EL1042
01411         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
01412         MOVE 1 TO TEMP-CURR-LINE.                                 EL1042
01413                                                                   EL1042
01414      IF  TEMP-CURR-LINE GREATER PI-TOTAL-LINES                       CL**4
01415         MOVE ER-0066 TO EMI-ERROR                                 EL1042
01416         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
01417         COMPUTE TEMP-CURR-LINE = PI-TOTAL-LINES + 1               EL1042
01418                                - NUM-LINES-PER-SCREEN             EL1042
01419         IF  TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS        CL**4
01420            MOVE 1 TO TEMP-CURR-LINE.                              EL1042
01421                                                                   EL1042
01422      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1042
01423                                                                      CL*15
01424      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1042
01425              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1042
01426              SC-INDX GREATER NUM-LINES-PER-SCREEN.                EL1042
01427                                                                   EL1042
01428      IF  EMI-ERROR = ER-0066 OR ER-0067 OR ZEROS                     CL**4
01429         NEXT SENTENCE                                             EL1042
01430        ELSE                                                       EL1042
01431         GO TO 8200-SEND-DATAONLY.                                 EL1042
01432                                                                   EL1042
01433      MOVE TEMP-CURR-LINE TO PI-CURRENT-LINE.                      EL1042
01434      SET TB-INDX         TO PI-CURRENT-LINE.                      EL1042
01435      MOVE LOW-VALUES     TO EL104BI.                              EL1042
01436                                                                   EL1042
01437      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1042
01438              VARYING SC-INDX FROM 1 BY 1                          EL1042
01439              UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN           EL1042
01440                                                                      CL**6
01441      IF  CREDIT-SESSION                                              CL*15
01442          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'                           CL*15
01443              MOVE PI-1042-ARCHIVE-IND                                CL*19
01444                                 TO ARCHBO.                           CL*19
01445                                                                      CL*19
01446      IF  CLAIM-SESSION                                               CL*19
01447              AND                                                     CL*19
01448          PI-COMPANY-ID = 'DMD'                                       CL*19
01449          IF PI-1042-ARCHIVE-IND = 'B' OR ' '                         CL*21
01450              MOVE AL-UANON      TO ARCHBA                            CL*19
01451              MOVE PI-1042-ARCHIVE-IND                                CL*15
01452                                 TO ARCHBO.                           CL*15
01453                                                                      CL*15
01454      IF  MORTGAGE-SESSION                                            CL*15
01455              OR                                                      CL*15
01456          CREDIT-SESSION                                              CL*15
01457          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'                       CL*15
01458              MOVE PI-FORM-SQUEEZE-CONTROL                            CL*15
01459                                  TO FORMSQBO.                        CL*15
01460                                                                   EL1042
01461      GO TO 8100-SEND-INITIAL-MAP.                                 EL1042
01462      EJECT                                                        EL1042
01463  7450-SET-INDX.                                                   EL1042
01464      IF  PI-CURRENT-LINE = 0                                         CL**4
01465         SET TB-INDX TO 1                                          EL1042
01466        ELSE                                                       EL1042
01467         SET TB-INDX TO PI-CURRENT-LINE.                           EL1042
01468                                                                   EL1042
01469  7450-EXIT.                                                       EL1042
01470       EXIT.                                                       EL1042
01471      EJECT                                                        EL1042
01472  7500-READ-TS.                                                    EL1042
01473      EXEC CICS HANDLE CONDITION                                   EL1042
01474           QIDERR (7590-TS-QIDERR)                                 EL1042
01475           ITEMERR(7585-TS-ITEMERR)                                EL1042
01476       END-EXEC.                                                   EL1042
01477                                                                   EL1042
01478      IF  CREDIT-SESSION                                              CL*15
01479          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'                           CL*15
01480              MOVE PI-1042-ARCHIVE-IND                                CL*19
01481                                 TO ARCHBO.                           CL*19
01482                                                                      CL*19
01483      IF  CLAIM-SESSION                                               CL*19
01484              AND                                                     CL*19
01485          PI-COMPANY-ID = 'DMD'                                       CL*19
01486          IF PI-1042-ARCHIVE-IND = 'B' OR ' '                         CL*21
01487              MOVE AL-UANON      TO ARCHBA                            CL*19
01488              MOVE PI-1042-ARCHIVE-IND                                CL*15
01489                                 TO ARCHBO.                           CL*15
01490                                                                      CL*15
01491      IF  MORTGAGE-SESSION                                            CL*15
01492              OR                                                      CL*15
01493          CREDIT-SESSION                                              CL*15
01494          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'                       CL*15
01495              MOVE PI-FORM-SQUEEZE-CONTROL                            CL*15
01496                                  TO FORMSQBO.                        CL*15
01497                                                                      CL**5
01498      SET TS-INDX TO 1.                                            EL1042
01499      MOVE 1 TO TS-ITEM.                                           EL1042
01500                                                                   EL1042
01501  7501-LOOP.                                                       EL1042
01502      EXEC CICS READQ TS                                           EL1042
01503           INTO  (TS-WORK-AREA)                                    EL1042
01504           QUEUE (TS-NAME)                                         EL1042
01505           LENGTH(TS-LENGTH)                                       EL1042
01506           ITEM  (TS-ITEM)                                         EL1042
01507       END-EXEC.                                                   EL1042
01508                                                                   EL1042
01509      MOVE TS-WORK-AREA TO TS-GROUP (TS-INDX).                     EL1042
01510      SET TS-INDX UP BY 1.                                         EL1042
01511      ADD 1 TO TS-ITEM.                                            EL1042
01512      GO TO 7501-LOOP.                                             EL1042
01513                                                                   EL1042
01514  7585-TS-ITEMERR.                                                 EL1042
01515      IF  EIBTRNID NOT = TRANS-ID                                     CL**4
01516         SUBTRACT 1 FROM TS-ITEM                                   EL1042
01517         MOVE TS-ITEM TO PI-TEMP-STOR-ITEMS.                       EL1042
01518                                                                   EL1042
01519      GO TO 7599-EXIT.                                             EL1042
01520                                                                   EL1042
01521  7590-TS-QIDERR.                                                  EL1042
01522                                                                      CL*13
01523      MOVE ZEROS TO PI-TEMP-STOR-ITEMS.                               CL*13
01524                                                                   EL1042
01525  7599-EXIT.                                                       EL1042
01526       EXIT.                                                       EL1042
01527      EJECT                                                        EL1042
01528  7600-UPDATE-TABLE-FROM-SCREEN.                                   EL1042
01529                                                                      CL**9
01530      IF  SC-TEXTL (SC-INDX) NOT = ZEROS                              CL**5
01531              OR                                                      CL**5
01532          SC-PCL   (SC-INDX) NOT = ZEROS                              CL**5
01533              OR                                                      CL**5
01534          SC-SQL   (SC-INDX) NOT = ZEROS                              CL**5
01535                                                                      CL**5
01536          IF  TB-INDX NOT GREATER PI-TOTAL-LINES                      CL**5
01537              PERFORM 7700-MOVE-DATA THRU 7700-EXIT                   CL**5
01538              SET TB-INDX UP BY 1                                     CL**5
01539                                                                      CL**5
01540          ELSE                                                        CL**5
01541              IF  PI-TOTAL-LINES = MAX-LINES                          CL**5
01542                  MOVE ER-0051 TO EMI-ERROR                           CL**5
01543                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**5
01544                  GO TO 8200-SEND-DATAONLY                            CL**5
01545                                                                      CL**5
01546              ELSE                                                 EL1042
01547                  PERFORM 7700-MOVE-DATA THRU 7700-EXIT               CL**5
01548                  SET TB-INDX UP BY 1                                 CL**5
01549                  ADD 1 TO PI-TOTAL-LINES                             CL**5
01550                                                                      CL**5
01551      ELSE                                                            CL**5
01552          IF  TB-INDX NOT GREATER PI-TOTAL-LINES                      CL**5
01553              SET TB-INDX UP BY 1.                                    CL**5
01554                                                                   EL1042
01555  7699-EXIT.                                                       EL1042
01556       EXIT.                                                       EL1042
01557                                                                   EL1042
01558  7700-MOVE-DATA.                                                  EL1042
01559                                                                   EL1042
01560      IF  SC-TEXTL (SC-INDX) NOT = ZEROS                              CL**4
01561          MOVE 1                 TO PI-UPDATE-SW                      CL*15
01562          MOVE SC-TEXT (SC-INDX) TO REC-TEXT (TB-INDX).               CL*15
01563                                                                   EL1042
01564      IF  PI-RETURN-TO-PROGRAM = PGM-EM152                            CL**3
01565              OR                                                      CL**3
01566          PI-RETURN-TO-PROGRAM = PGM-EL152                            CL**3
01567              OR                                                      CL**3
01568          PI-RETURN-TO-PROGRAM = PGM-EL689                            CL**3
01569          GO TO 7700-EXIT.                                            CL**3
01570                                                                   EL1042
01571      IF  SC-PCL (SC-INDX) NOT = ZEROS                                CL**4
01572          IF  SC-PC (SC-INDX) = SPACES OR                             CL*15
01573              SC-PC (SC-INDX) NUMERIC                                 CL*15
01574              MOVE SC-PC (SC-INDX)                                    CL*15
01575                                  TO REC-PC (TB-INDX)                 CL*15
01576              MOVE 1              TO PI-UPDATE-SW                     CL*15
01577                                                                      CL**5
01578          ELSE                                                        CL*15
01579              MOVE -1             TO SC-PCL (SC-INDX)                 CL*15
01580              MOVE ER-0141        TO EMI-ERROR                        CL*15
01581              MOVE AL-UABON       TO SC-PCA (SC-INDX)                 CL*15
01582              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*15
01583                                                                      CL*15
01584      IF  SC-SQL (SC-INDX) NOT EQUAL ZEROS                            CL*15
01585              AND                                                     CL*15
01586          SC-SQ (SC-INDX) NOT EQUAL LOW-VALUES                        CL*15
01587          MOVE SC-SQ (SC-INDX)    TO W-LINE-SQUEEZE-IND               CL*15
01588                                                                      CL**5
01589          IF  W-LINE-SQUEEZE-VALID-VALUE                              CL*10
01590              MOVE 1              TO PI-UPDATE-SW                     CL*15
01591              MOVE SC-SQ (SC-INDX)                                    CL*10
01592                                  TO REC-SQ (TB-INDX)                 CL*15
01593                                                                      CL*10
01594          ELSE                                                        CL*10
01595              MOVE -1             TO SC-SQL (SC-INDX)                 CL*15
01596              MOVE ER-9348        TO EMI-ERROR                        CL*15
01597              MOVE AL-UABON       TO SC-SQA (SC-INDX)                 CL*15
01598              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*10
01599                                                                   EL1042
01600  7700-EXIT.                                                       EL1042
01601       EXIT.                                                       EL1042
01602      EJECT                                                        EL1042
01603  8100-SEND-INITIAL-MAP.                                           EL1042
01604                                                                      CL**4
01605      MOVE 'Y'                  TO PI-1042-SCREEN-SENT-IND.           CL**4
01606      MOVE PI-COMPANY-ID        TO COMPANYO.                          CL*15
01607      MOVE SAVE-DATE            TO DATEBO.                         EL1042
01608      MOVE EIBTIME              TO TIME-IN.                        EL1042
01609      MOVE TIME-OUT             TO TIMEBO.                         EL1042
01610      MOVE EMI-MESSAGE-AREA (1) TO ERRMSGBO.                       EL1042
01611                                                                   EL1042
01612      IF  PI-ENTRY-CD-1 = '1'                                         CL**4
01613          MOVE 'LETTERS' TO TEXTTPBO                               EL1042
01614      ELSE                                                         EL1042
01615          IF  PI-ENTRY-CD-1 = '2'                                     CL**4
01616              MOVE 'FORMS' TO TEXTTPBO                             EL1042
01617          ELSE                                                     EL1042
01618              MOVE 'HELP' TO TEXTTPBO.                             EL1042
01619                                                                      CL**9
01620      MOVE ' ARCHIVE:'                 TO ARCHTBO.                    CL*19
01621                                                                      CL*20
01622      IF  CLAIM-SESSION   AND                                         CL*19
01623          PI-COMPANY-ID = 'DMD'                                       CL*19
01624              MOVE '     BSR:'         TO ARCHTBO                     CL*19
01625              MOVE +13                 TO FORMSTBL                    CL*19
01626              MOVE +1                  TO LINESQTL                    CL*19
01627              MOVE AL-SADOF            TO FORMSTBA                    CL*19
01628                                          SC-SQA (02)                 CL*19
01629                                          SC-SQA (03)                 CL*19
01630                                          SC-SQA (04)                 CL*19
01631                                          SC-SQA (05)                 CL*19
01632                                          SC-SQA (06)                 CL*19
01633                                          SC-SQA (07)                 CL*19
01634                                          SC-SQA (08)                 CL*19
01635                                          SC-SQA (09)                 CL*19
01636                                          SC-SQA (10)                 CL*19
01637                                          SC-SQA (11)                 CL*19
01638                                          SC-SQA (12)                 CL*19
01639                                          SC-SQA (13)                 CL*19
01640                                          SC-SQA (14)                 CL*19
01641                                          SC-SQA (15)                 CL*19
01642      ELSE                                                            CL*19
01643                                                                      CL*19
01644      IF  MORTGAGE-SESSION                                            CL**9
01645          MOVE +13                     TO FORMSTBL                    CL**9
01646          MOVE AL-SANOF                TO FORMSTBA                    CL*10
01647          MOVE +1                      TO LINESQTL                    CL**9
01648          MOVE AL-SANOF                TO LINESQTA                    CL*10
01649          MOVE AL-UANON                TO FORMSQBA                    CL*10
01650          MOVE AL-UANON                TO ARCHBA                      CL*20
01651                                          ARCHTBA                     CL*15
01652                                                                      CL*15
01653      ELSE                                                            CL*15
01654          IF  CLAIM-SESSION                                           CL*15
01655              MOVE +13                 TO FORMSTBL                    CL*15
01656              MOVE +1                  TO LINESQTL                    CL*15
01657              MOVE AL-SADOF            TO FORMSTBA                    CL*15
01658                                          ARCHBA                      CL*15
01659                                          ARCHTBA                     CL*19
01660                                          SC-SQA (02)                 CL*15
01661                                          SC-SQA (03)                 CL*15
01662                                          SC-SQA (04)                 CL*15
01663                                          SC-SQA (05)                 CL*15
01664                                          SC-SQA (06)                 CL*15
01665                                          SC-SQA (07)                 CL*15
01666                                          SC-SQA (08)                 CL*15
01667                                          SC-SQA (09)                 CL*15
01668                                          SC-SQA (10)                 CL*10
01669                                          SC-SQA (11)                 CL*10
01670                                          SC-SQA (12)                 CL*10
01671                                          SC-SQA (13)                 CL*10
01672                                          SC-SQA (14)                 CL*10
01673                                          SC-SQA (15)                 CL*10
01674                                                                      CL**9
01675          ELSE                                                        CL*15
01676              MOVE +13                 TO FORMSTBL                    CL*15
01677              MOVE AL-SANOF            TO FORMSTBA                    CL*15
01678              MOVE +1                  TO LINESQTL                    CL*15
01679              MOVE AL-SANOF            TO LINESQTA                    CL*15
01680              MOVE AL-UANON            TO FORMSQBA                    CL*15
01681                                          ARCHBA                      CL*15
01682              MOVE +9                  TO ARCHTBL                     CL*15
01683              MOVE AL-SANOF            TO ARCHTBA.                    CL*15
01684                                                                      CL**6
01685      IF  CREDIT-SESSION                                              CL*15
01686              OR                                                      CL*20
01687          MORTGAGE-SESSION                                            CL*20
01688                                                                      CL*15
01689          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'                           CL*15
01690              MOVE +1            TO ARCHBL                            CL*19
01691              MOVE AL-UANON      TO ARCHBA                            CL*19
01692              MOVE PI-1042-ARCHIVE-IND                                CL*19
01693                                 TO ARCHBO.                           CL*19
01694                                                                      CL*19
01695      IF  CLAIM-SESSION                                               CL*19
01696              AND                                                     CL*19
01697          PI-COMPANY-ID = 'DMD'                                       CL*19
01698          IF PI-1042-ARCHIVE-IND = 'B' OR ' '                         CL*21
01699              MOVE +1            TO ARCHBL                            CL*15
01700              MOVE AL-UANON      TO ARCHBA                            CL*15
01701              MOVE PI-1042-ARCHIVE-IND                                CL*15
01702                                 TO ARCHBO.                           CL*15
01703                                                                      CL*15
01704      IF  MORTGAGE-SESSION                                            CL*15
01705              OR                                                      CL*15
01706          CREDIT-SESSION                                              CL*15
01707                                                                      CL*15
01708          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'                       CL*15
01709              MOVE PI-FORM-SQUEEZE-CONTROL                            CL*15
01710                                  TO FORMSQBO                         CL*15
01711              MOVE +1             TO FORMSQBL                         CL*15
01712              MOVE AL-UANON       TO FORMSQBA.                        CL*15
01713                                                                   EL1042
01714      MOVE PI-COMM-CONTROL TO CONTRLBI.                            EL1042
01715      MOVE PI-TOTAL-LINES  TO TOTI.                                EL1042
01716      MOVE -1              TO FUNCTL.                              EL1042
01717                                                                   EL1042
01718      EXEC CICS SEND                                               EL1042
01719           MAPSET(MAPSET-NAME)                                     EL1042
01720           MAP   (MAP-NAME)                                        EL1042
01721           FROM  (EL104BO)                                         EL1042
01722           ERASE                                                   EL1042
01723           CURSOR                                                  EL1042
01724       END-EXEC.                                                   EL1042
01725                                                                   EL1042
01726      GO TO 0200-RECEIVE.                                             CL*13
01727                                                                   EL1042
01728  8200-SEND-DATAONLY.                                              EL1042
01729                                                                      CL**4
01730      IF  PI-1042-SCREEN-NOT-SENT                                     CL**4
01731          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
01732                                                                      CL**4
01733      MOVE PI-COMPANY-ID        TO COMPANYO.                          CL*15
01734      MOVE EIBTIME        TO TIME-IN.                              EL1042
01735      MOVE TIME-OUT       TO TIMEBO.                               EL1042
01736      MOVE PI-TOTAL-LINES TO TOTI.                                 EL1042
01737                                                                      CL**9
01738      MOVE ' ARCHIVE:'                 TO ARCHTBO.                    CL*19
01739                                                                      CL*20
01740      IF  CLAIM-SESSION   AND                                         CL*19
01741          PI-COMPANY-ID = 'DMD'                                       CL*19
01742              MOVE '     BSR:'         TO ARCHTBO                     CL*19
01743              MOVE +13                 TO FORMSTBL                    CL*19
01744              MOVE +1                  TO LINESQTL                    CL*19
01745              MOVE AL-SADOF            TO FORMSTBA                    CL*19
01746                                          SC-SQA (02)                 CL*19
01747                                          SC-SQA (03)                 CL*19
01748                                          SC-SQA (04)                 CL*19
01749                                          SC-SQA (05)                 CL*19
01750                                          SC-SQA (06)                 CL*19
01751                                          SC-SQA (07)                 CL*19
01752                                          SC-SQA (08)                 CL*19
01753                                          SC-SQA (09)                 CL*19
01754                                          SC-SQA (10)                 CL*19
01755                                          SC-SQA (11)                 CL*19
01756                                          SC-SQA (12)                 CL*19
01757                                          SC-SQA (13)                 CL*19
01758                                          SC-SQA (14)                 CL*19
01759                                          SC-SQA (15)                 CL*19
01760      ELSE                                                            CL*19
01761                                                                      CL*19
01762      IF  MORTGAGE-SESSION                                            CL**9
01763          MOVE +13                     TO FORMSTBL                    CL**9
01764          MOVE AL-SANOF                TO FORMSTBA                    CL*10
01765          MOVE +1                      TO LINESQTL                    CL**9
01766          MOVE AL-SANOF                TO LINESQTA                    CL*10
01767          MOVE AL-UANON                TO FORMSQBA                    CL*10
01768          MOVE AL-UANON                TO ARCHBA                      CL*20
01769                                          ARCHTBA                     CL*15
01770                                                                      CL*15
01771      ELSE                                                            CL*15
01772          IF  CLAIM-SESSION                                           CL*15
01773              MOVE +13                 TO FORMSTBL                    CL*15
01774              MOVE +1                  TO LINESQTL                    CL*15
01775              MOVE AL-SADOF            TO FORMSTBA                    CL*15
01776                                          ARCHBA                      CL*15
01777                                          ARCHTBA                     CL*19
01778                                          SC-SQA (02)                 CL*15
01779                                          SC-SQA (03)                 CL*15
01780                                          SC-SQA (04)                 CL*15
01781                                          SC-SQA (05)                 CL*15
01782                                          SC-SQA (06)                 CL*15
01783                                          SC-SQA (07)                 CL*15
01784                                          SC-SQA (08)                 CL*15
01785                                          SC-SQA (09)                 CL*15
01786                                          SC-SQA (10)                 CL*10
01787                                          SC-SQA (11)                 CL*10
01788                                          SC-SQA (12)                 CL*10
01789                                          SC-SQA (13)                 CL*10
01790                                          SC-SQA (14)                 CL*10
01791                                          SC-SQA (15)                 CL*10
01792                                                                      CL**9
01793          ELSE                                                        CL*15
01794              MOVE +13                 TO FORMSTBL                    CL*15
01795              MOVE AL-SANOF            TO FORMSTBA                    CL*15
01796              MOVE +1                  TO LINESQTL                    CL*15
01797              MOVE AL-SANOF            TO LINESQTA                    CL*15
01798              MOVE AL-UANON            TO FORMSQBA                    CL*15
01799                                          ARCHBA                      CL*15
01800              MOVE +9                  TO ARCHTBL                     CL*15
01801              MOVE AL-SANOF            TO ARCHTBA.                    CL*15
01802                                                                      CL**9
01803      IF  CREDIT-SESSION                                              CL*15
01804              OR                                                      CL*20
01805          MORTGAGE-SESSION                                            CL*20
01806                                                                      CL**9
01807          IF  PI-1042-ARCHIVE-IND EQUAL 'Y'                           CL*15
01808              MOVE +1            TO ARCHBL                            CL*19
01809              MOVE AL-UANON      TO ARCHBA                            CL*19
01810              MOVE PI-1042-ARCHIVE-IND                                CL*19
01811                                 TO ARCHBO.                           CL*19
01812                                                                      CL*19
01813      IF  CLAIM-SESSION                                               CL*19
01814              AND                                                     CL*19
01815          PI-COMPANY-ID = 'DMD'                                       CL*19
01816              AND                                                     CL*19
01817          WS-8126-ERROR-SW = 'N'                                      CL*19
01818          IF PI-1042-ARCHIVE-IND = 'B' OR ' '                         CL*21
01819              MOVE +1            TO ARCHBL                            CL*15
01820              MOVE AL-UANON      TO ARCHBA                            CL*15
01821              MOVE PI-1042-ARCHIVE-IND                                CL*15
01822                                 TO ARCHBO.                           CL*15
01823                                                                      CL*15
01824      IF  MORTGAGE-SESSION                                            CL*15
01825              OR                                                      CL*15
01826          CREDIT-SESSION                                              CL*15
01827                                                                      CL*15
01828          IF  PI-FORM-SQUEEZE-CONTROL EQUAL 'Y'                       CL*15
01829              MOVE PI-FORM-SQUEEZE-CONTROL                            CL*15
01830                                  TO FORMSQBO                         CL*15
01831              MOVE +1             TO FORMSQBL                         CL*15
01832              MOVE AL-UANON       TO FORMSQBA.                        CL*15
01833                                                                   EL1042
01834      IF  NOT EMI-NO-ERRORS                                           CL**4
01835          MOVE EMI-MESSAGE-AREA (1) TO ERRMSGBO                       CL**4
01836      ELSE                                                            CL**4
01837          MOVE -1 TO FUNCTL.                                          CL**4
01838                                                                   EL1042
01839      EXEC CICS SEND                                               EL1042
01840           MAPSET(MAPSET-NAME)                                     EL1042
01841           MAP   (MAP-NAME)                                        EL1042
01842           DATAONLY                                                EL1042
01843           CURSOR                                                  EL1042
01844           FROM  (EL104BO)                                         EL1042
01845      END-EXEC.                                                       CL**4
01846                                                                   EL1042
01847      GO TO 0200-RECEIVE.                                             CL*13
01848                                                                   EL1042
01849  8300-SEND-TEXT.                                                  EL1042
01850                                                                      CL**4
01851      EXEC CICS SEND TEXT                                          EL1042
01852           FROM  (LOGOFF-TEXT)                                     EL1042
01853           ERASE                                                   EL1042
01854           FREEKB                                                  EL1042
01855           LENGTH(LOGOFF-LENGTH)                                   EL1042
01856      END-EXEC.                                                       CL**4
01857                                                                   EL1042
01858      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL1042
01859                                                                   EL1042
01860      EXEC CICS RETURN                                             EL1042
01861      END-EXEC.                                                       CL**4
01862                                                                   EL1042
01863  8800-UNAUTHORIZED-ACCESS.                                        EL1042
01864                                                                      CL**4
01865      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL1042
01866      GO TO 8300-SEND-TEXT.                                        EL1042
01867                                                                   EL1042
01868  9000-RETURN-CICS.                                                EL1042
01869                                                                      CL**4
01870      IF  ANY-UPDATES                                                 CL**4
01871          MOVE ER-0045  TO EMI-ERROR                                  CL**4
01872          MOVE -1       TO FUNCTL                                     CL**4
01873          MOVE SPACES   TO PFENTRBO                                   CL**4
01874          MOVE AL-UNNOF TO PFENTRBA                                   CL**4
01875          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
01876          GO TO 8200-SEND-DATAONLY.                                   CL**4
01877                                                                   EL1042
01878      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                      CL*13
01879                                                                      CL*13
01880                                                                      CL*13
01881      MOVE EIBAID  TO PI-ENTRY-CD-1.                               EL1042
01882      MOVE 'EL005' TO PGM-NAME.                                    EL1042
01883      GO TO 9300-XCTL.                                             EL1042
01884                                                                   EL1042
01885  9100-RETURN-TRAN.                                                EL1042
01886                                                                   EL1042
01887      EXEC CICS RETURN                                             EL1042
01888           TRANSID (TRANS-ID)                                      EL1042
01889           COMMAREA(PROGRAM-INTERFACE-BLOCK)                       EL1042
01890           LENGTH  (PI-COMM-LENGTH)                                EL1042
01891      END-EXEC.                                                       CL**4
01892                                                                   EL1042
01893  9200-RETURN-MAIN-MENU.                                           EL1042
01894                                                                   EL1042
01895      IF  ANY-UPDATES                                                 CL**4
01896          MOVE -1       TO FUNCTL                                     CL**4
01897          MOVE SPACES   TO PFENTRBO                                   CL**4
01898          MOVE AL-UNNOF TO PFENTRBA                                   CL**4
01899          MOVE ER-0045  TO EMI-ERROR                                  CL**4
01900          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
01901          GO TO 8200-SEND-DATAONLY.                                   CL**4
01902                                                                      CL**4
01903      IF  CLAIM-SESSION                                               CL**4
01904          MOVE XCTL-CLAIM      TO PGM-NAME                            CL**4
01905                                                                      CL**4
01906      ELSE                                                         EL1042
01907          IF  CREDIT-SESSION                                          CL**4
01908              MOVE XCTL-CREDIT TO PGM-NAME                            CL**4
01909                                                                      CL**4
01910          ELSE                                                        CL**4
01911              IF  WARRANTY-SESSION                                    CL**4
01912                  MOVE XCTL-WARRANTY TO PGM-NAME                      CL**4
01913                                                                      CL**4
01914              ELSE                                                    CL**4
01915                  IF  MORTGAGE-SESSION                                CL**4
01916                      MOVE XCTL-MORTGAGE   TO PGM-NAME                CL**4
01917                                                                      CL**4
01918                  ELSE                                                CL**4
01919                      MOVE XCTL-GEN-LEDGER TO PGM-NAME.               CL**4
01920                                                                   EL1042
01921  9300-XCTL.                                                       EL1042
01922                                                                      CL**3
01923      MOVE MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.              CL*13
01924      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.                  CL*13
01925                                                                      CL*13
01926      MOVE TRANS-ID             TO EIBTRNID                           CL*13
01927                                                                      CL*13
01928      IF  PI-RETURN-TO-PROGRAM NOT EQUAL PGM-EM152                    CL**3
01929              AND                                                     CL**3
01930          PI-RETURN-TO-PROGRAM NOT EQUAL PGM-EL152                    CL**3
01931              AND                                                     CL**3
01932          PI-RETURN-TO-PROGRAM NOT EQUAL PGM-EL689                    CL**3
01933                                                                      CL**3
01934          PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.               CL**3
01935                                                                   EL1042
01936      EXEC CICS XCTL                                               EL1042
01937           PROGRAM (PGM-NAME)                                      EL1042
01938           COMMAREA(PROGRAM-INTERFACE-BLOCK)                       EL1042
01939           LENGTH  (PI-COMM-LENGTH)                                EL1042
01940       END-EXEC.                                                   EL1042
01941                                                                   EL1042
01942  9400-CLEAR.                                                      EL1042
01943      IF  ANY-UPDATES                                                 CL**4
01944         MOVE ER-0045  TO EMI-ERROR                                EL1042
01945         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
01946         SET TB-INDX TO PI-CURRENT-LINE                            EL1042
01947         PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                 EL1042
01948                 VARYING SC-INDX FROM 1 BY 1 UNTIL                 EL1042
01949                 SC-INDX GREATER NUM-LINES-PER-SCREEN              EL1042
01950         GO TO 8100-SEND-INITIAL-MAP.                              EL1042
01951                                                                   EL1042
01952  9410-RETURN.                                                     EL1042
01953         MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                    EL1042
01954         GO TO 9300-XCTL.                                          EL1042
01955                                                                   EL1042
       9450-PF5.

           IF ANY-UPDATES
              MOVE -1                  TO FUNCTL
              MOVE SPACES              TO PFENTRBO
              MOVE AL-UNNOF            TO PFENTRBA
              MOVE ER-0045             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF

082211     IF CREDIT-SESSION
082211         MOVE 'EL1044'           TO PGM-NAME
082211     ELSE
082211         MOVE 'EL1041'           TO PGM-NAME
082211     END-IF           
           GO TO 9300-XCTL

           .
01956  9500-PF12.                                                       EL1042
01957      IF  ANY-UPDATES                                                 CL**4
01958         MOVE -1       TO FUNCTL                                   EL1042
01959         MOVE SPACES   TO PFENTRBO                                 EL1042
01960         MOVE AL-UNNOF TO PFENTRBA                                 EL1042
01961         MOVE ER-0045  TO EMI-ERROR                                EL1042
01962         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1042
01963         GO TO 8200-SEND-DATAONLY.                                 EL1042
01964                                                                      CL*13
01965                                                                   EL1042
01966      MOVE 'EL010' TO PGM-NAME.                                    EL1042
01967      GO TO 9300-XCTL.                                             EL1042
01968      EJECT                                                        EL1042
01969  9600-PGMID-ERROR.                                                EL1042
01970      EXEC CICS HANDLE CONDITION                                   EL1042
01971           PGMIDERR(8300-SEND-TEXT)                                EL1042
01972       END-EXEC.                                                   EL1042
01973                                                                   EL1042
01974      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL1042
01975      MOVE SPACES       TO PI-ENTRY-CD-1.                          EL1042
01976      MOVE 'EL005'      TO PGM-NAME.                               EL1042
01977      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL1042
01978      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL1042
01979      GO TO 9300-XCTL.                                             EL1042
01980                                                                   EL1042
01981  9700-LINK-DATE-CONVERT.                                          EL1042
01982      EXEC CICS LINK                                               EL1042
01983          PROGRAM    ('ELDATCV')                                   EL1042
01984          COMMAREA   (DATE-CONVERSION-DATA)                        EL1042
01985          LENGTH     (DC-COMM-LENGTH)                              EL1042
01986      END-EXEC.                                                    EL1042
01987                                                                   EL1042
01988  9700-EXIT.                                                       EL1042
01989      EXIT.                                                        EL1042
01990                                                                   EL1042
01991  9900-ERROR-FORMAT.                                               EL1042
01992                                                                      CL*18
01993      MOVE PI-LANGUAGE-TYPE       TO EMI-LANGUAGE-IND.                CL*18
01994                                                                      CL*18
01995      IF  NOT EMI-ERRORS-COMPLETE                                     CL**4
01996         EXEC CICS LINK                                            EL1042
01997              PROGRAM('EL001')                                     EL1042
01998              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL1042
01999              LENGTH(EMI-COMM-LENGTH)                              EL1042
02000          END-EXEC.                                                EL1042
02001                                                                   EL1042
02002  9900-EXIT.                                                       EL1042
02003       EXIT.                                                       EL1042
02004                                   EJECT                              CL**3
02005  9910-INITIALIZE-SECURITY.                                           CL**3
02006 ******************************************************************   CL**3
02007 *    THIS IS A COMBINED SECURITY PROCESSOR.                      *   CL*10
02008 *                                                                *   CL**3
02009 ******************************************************************   CL**3
02010                                                                   EL1042
02011      IF  PI-PROCESSOR-ID EQUAL 'LGXX'                                CL*10
02012          GO TO 9910-EXIT.                                            CL*10
02013                                                                      CL*10
02014      IF  MORTGAGE-SESSION                                            CL*10
02015          MOVE '125E'                 TO SC-QUID-SYSTEM               CL**3
02016          MOVE EIBTRMID               TO SC-QUID-TERMINAL             CL**3
02017                                                                      CL**3
02018          EXEC CICS READQ TS                                          CL**3
02019              QUEUE  (SC-QUID-KEY)                                    CL**3
02020              INTO   (SECURITY-CONTROL-E)                             CL**3
02021              LENGTH (SC-COMM-LENGTH-E)                               CL**3
02022              ITEM   (SC-ITEM)                                        CL**3
02023          END-EXEC                                                    CL**3
02024                                                                      CL**3
02025          MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)                       CL**3
02026                                      TO PI-DISPLAY-CAP               CL**3
02027          MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)                        CL**3
02028                                      TO PI-MODIFY-CAP                CL**3
02029                                                                      CL**3
02030          GO TO 9910-EXIT                                             CL*14
02031                                                                      CL*10
02032      ELSE                                                            CL*10
02033          EXEC CICS READQ TS                                          CL*10
02034              QUEUE  (PI-SECURITY-TEMP-STORE-ID)                      CL*10
02035              INTO   (SECURITY-CONTROL)                               CL*10
02036              LENGTH (SC-COMM-LENGTH)                                 CL*10
02037              ITEM   (W-SC-ITEM)                                      CL*10
02038          END-EXEC                                                    CL*10
02039                                                                      CL*10
02040          MOVE SC-CREDIT-DISPLAY (03)  TO PI-DISPLAY-CAP              CL*10
02041          MOVE SC-CREDIT-UPDATE  (03)  TO PI-MODIFY-CAP.              CL*14
02042                                                                      CL**3
02043  9910-EXIT.                                                          CL**3
02044      EXIT.                                                           CL**3
02045                                   EJECT                              CL**3
02046  9990-ABEND.                                                      EL1042
02047      MOVE DFHEIBLK               TO EMI-LINE1.                    EL1042
02048      EXEC CICS LINK                                               EL1042
02049          PROGRAM   ('EL004')                                      EL1042
02050          COMMAREA  (EMI-LINE1)                                    EL1042
02051          LENGTH    (72)                                           EL1042
02052      END-EXEC.                                                    EL1042
02053                                                                   EL1042
02054      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGBO.                     EL1042
02055      GO TO 8200-SEND-DATAONLY.                                    EL1042
02056                                                                   EL1042
02057      GOBACK.                                                      EL1042
02058                                                                   EL1042
02059  9995-SECURITY-VIOLATION.                                         EL1042
02060                              COPY ELCSCTP.                        EL1042
02061                                                                   EL1042
02062  9995-EXIT.                                                       EL1042
02063      EXIT.                                                        EL1042
02064                                                                   EL1042
