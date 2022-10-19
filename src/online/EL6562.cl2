00001  ID DIVISION.                                                     04/15/98
00002                                                                   EL6562
00003  PROGRAM-ID.                 EL6562.                                 LV015
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/12/96 15:49:07.                    CL**7
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE               CL*15
00008 *                            VMOD=2.008                              CL*11
00008 *                                                                    CL*11
00009                                                                   EL6562
00010 *AUTHOR.     LOGIC,INC.                                              CL**7
00011 *            DALLAS, TEXAS.                                          CL**7
00012                                                                   EL6562
00013 *DATE-COMPILED.                                                      CL**7
00014 *SECURITY.   *****************************************************   CL**7
00015 *            *                                                   *   CL**7
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00017 *            *                                                   *   CL**7
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00019 *                                                                *   CL**7
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00022 *            *                                                   *   CL**7
00023 *            *****************************************************   CL**7
00024                                                                   EL6562
00025 *REMARKS.    TRANSACTION - EXE3 - RATE MASTER MAINTENANCE            CL**7
00026 *                                 (RATES INPUT)                      CL**7
00027      EJECT                                                        EL6562
081413******************************************************************
081413*                   C H A N G E   L O G
081413*
081413* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081413*-----------------------------------------------------------------
081413*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081413* EFFECTIVE    NUMBER
081413*-----------------------------------------------------------------
081413* 081413    2013080700002  PEMA ADD JOURNALING OF ERRATE FILE
081413******************************************************************
00028  ENVIRONMENT DIVISION.                                            EL6562
00029  DATA DIVISION.                                                   EL6562
00030  WORKING-STORAGE SECTION.                                         EL6562
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6562
00032  77  FILLER  PIC X(32)  VALUE '*    EL6562 WORKING STORAGE    *'. EL6562
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.008 *********'.    CL*11
00034                                                                   EL6562
00035  01  WS-DATE-AREA.                                                EL6562
00036      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL6562
00037      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL6562
00038                                                                      CL*11
00039 ***  Y2K PROJ 7744                                                   CL*14
00040  01  WS-RATE-HOLD-AREA.                                              CL*11
00041      05  WS-HOLD-RATE-EXP-AL PIC X(11).                              CL*11
00042      05  WS-HOLD-RATE-EXP-DT REDEFINES                               CL*11
00043          WS-HOLD-RATE-EXP-AL PIC 9(11).                              CL*11
00044 ***  Y2K PROJ 7744                                                   CL*14
00045                                                                   EL6562
00046  01  STANDARD-AREAS.                                              EL6562
00047      05  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL6562
00048      05  TRANS-ID            PIC X(4)    VALUE 'EXE3'.            EL6562
00049      05  THIS-PGM            PIC X(8)    VALUE 'EL6562'.          EL6562
00050      05  WS-MAPNAME          PIC X(8)    VALUE 'EL6562A'.         EL6562
00051      05  WS-MAPSET-NAME      PIC X(8)    VALUE 'EL6562S'.         EL6562
00052      05  PGM-NAME            PIC X(8).                            EL6562
00053      05  SUB1                PIC S999    VALUE +0.                EL6562
00054      05  SUB2                PIC S999    VALUE +0.                EL6562
00055      05  SUB3                PIC S99     VALUE +0.                EL6562
00056          88  EVEN-SUB                    VALUE +2 +4 +6 +8.       EL6562
00057          88  ODD-SUB                     VALUE +1 +3 +5 +7.       EL6562
00058      05  SUB4                PIC S99     VALUE +0.                EL6562
00059      05  SC-ITEM             PIC S9(4)   VALUE +1     COMP.       EL6562
00060      05  WS-COMP-CD-R.                                            EL6562
00061          10  FILLER          PIC X.                               EL6562
00062          10  WS-COMP-CD-X    PIC X.                               EL6562
00063      05  WS-COMP-CD   REDEFINES WS-COMP-CD-R  PIC S9(4)     COMP. EL6562
00064      05  TIME-IN             PIC S9(7).                           EL6562
00065      05  TIME-OUT-R  REDEFINES TIME-IN.                           EL6562
00066          10  FILLER          PIC X.                               EL6562
00067          10  TIME-OUT        PIC 99V99.                           EL6562
00068          10  FILLER          PIC X(2).                            EL6562
00069      05  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL6562
00070      05  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL6562
00071      05  XCTL-626            PIC X(8)    VALUE 'EL126'.           EL6562
00072      05  XCTL-656            PIC X(8)    VALUE 'EL656'.           EL6562
00073      05  XCTL-6561           PIC X(8)    VALUE 'EL6561'.          EL6562
00074      05  XCTL-6563           PIC X(8)    VALUE 'EL6563'.          EL6562
00075      05  XCTL-6564           PIC X(8)    VALUE 'EL6564'.          EL6562
00076      05  LINK-001            PIC X(8)    VALUE 'EL001'.           EL6562
00077      05  LINK-004            PIC X(8)    VALUE 'EL004'.           EL6562
00078      05  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL6562
00079                                                                   EL6562
00080      05  ER-0000             PIC X(4)    VALUE  '0000'.           EL6562
00081      05  ER-0004             PIC X(4)    VALUE  '0004'.           EL6562
00082      05  ER-0008             PIC X(4)    VALUE  '0008'.           EL6562
00083      05  ER-0029             PIC X(4)    VALUE  '0029'.           EL6562
00084      05  ER-0068             PIC X(4)    VALUE  '0068'.           EL6562
00085      05  ER-0070             PIC X(4)    VALUE  '0070'.           EL6562
00086      05  ER-0142             PIC X(4)    VALUE  '0142'.           EL6562
00087      05  ER-2055             PIC X(4)    VALUE  '2055'.           EL6562
00088      05  ER-2056             PIC X(4)    VALUE  '2056'.           EL6562
00089      05  ER-2271             PIC X(4)    VALUE  '2271'.           EL6562
00090      05  ER-2272             PIC X(4)    VALUE  '2272'.           EL6562
00091      05  ER-2280             PIC X(4)    VALUE  '2280'.           EL6562
00092      05  ER-2285             PIC X(4)    VALUE  '2285'.           EL6562
00093      05  ER-2296             PIC X(4)    VALUE  '2296'.           EL6562
00094                                                                   EL6562
00095      05  RATE-FILE-ID        PIC X(8)    VALUE 'ERRATE'.          EL6562
00096      05  OE-RATE-FILE-ID     PIC X(8)    VALUE 'OERATE'.          EL6562
00097      05  FILE-ID             PIC X(8)    VALUE  SPACES.           EL6562
00098      05  CNTL-FILE-ID        PIC X(8)    VALUE 'ELCNTL'.          EL6562
00099      05  BIN-CURRENT-SAVE    PIC XX      VALUE SPACES.            EL6562
00100                                                                   EL6562
00101      05  DEEDIT-FIELD            PIC X(15).                       EL6562
00102      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6562
00103      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(10)V99999.EL6562
00104                                                                   EL6562
00105      05  ERRATE-LENGTH       PIC S9(4)   VALUE +1765 COMP.        EL6562
00106                                                                   EL6562
00107      05  WS-PREV-AGE         PIC 99       VALUE ZERO.             EL6562
00108      05  WS-PREV-TRM         PIC 999      VALUE ZERO.             EL6562
00109      05  WS-PREV-AMT         PIC S9(7)V99 VALUE ZERO.             EL6562
00110      05  WS-TERM.                                                    CL**5
00111          10  WS-FROM         PIC 999.                                CL**5
00112          10  FILLER          PIC X        VALUE '-'.                 CL**5
00113          10  WS-TO           PIC 999.                                CL**5
00114                                                                      CL**5
00115      05  WS-SAVE-KEY         PIC X(28)    VALUE SPACES.              CL**5
00116      05  WS-SAVE-STRUCTURE   PIC X(7)     VALUE SPACES.              CL**5
00117                                                                      CL**5
00118      05  ELCNTL-KEY.                                                 CL**5
00119          10  CNTL-COMP-ID    PIC X(3)    VALUE SPACES.               CL**5
00120          10  CNTL-REC-TYPE   PIC X       VALUE SPACES.               CL**5
00121          10  CNTL-ACCESS     PIC X(4)    VALUE SPACES.               CL**5
00122          10  CNTL-SEQ-NO     PIC S9(4)   VALUE +0  COMP.             CL**5
00123                                                                   EL6562
00124      05  MISC-SAVE-AREAS.                                            CL**5
00125          10  WS-SAVE-RATE    OCCURS 48 TIMES                         CL**5
00126                              PIC S9(2)V9(5).                         CL**5
00127                                                                      CL**5
081413 01  filler.
081413     12  WS-RESPONSE             PIC S9(8)   COMP.                    
081413         88  WS-RESP-NORMAL              VALUE +00.               
081413         88  WS-RESP-ERROR               VALUE +01.               
081413         88  WS-RESP-NOTFND              VALUE +13.               
081413         88  WS-RESP-DUPKEY              VALUE +15.
081413         88  WS-RESP-NOTOPEN             VALUE +19.
081413         88  WS-RESP-ENDFILE             VALUE +20.
081413
00129                            COPY ELCSCTM.                             CL**5
00130  EJECT                                                               CL**5
00131                            COPY ELCSCRTY.                            CL**5
00132      EJECT                                                        EL6562
00133                                    COPY ELCDATE.                     CL**5
00134      EJECT                                                        EL6562
00135                                    COPY ELCLOGOF.                    CL**5
00136      EJECT                                                        EL6562
00137                                    COPY ELCATTR.                     CL**5
00138      EJECT                                                           CL**5
00139                                    COPY ELCEMIB.                     CL**5
00140      EJECT                                                           CL**5
00141                                    COPY ELCINTF.                     CL**5
00142      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.                CL**5
00143          16  PI-FILE-ID                    PIC XX.                   CL**5
00144          16  PI-MAINT                      PIC X.                    CL**5
00145                                                                      CL**5
00146          16  PI-ERRATE-KEY.                                          CL**5
00147              20  PI-RATE-COMPANY-CD         PIC X.                   CL**5
00148              20  PI-RATE-STATE-CODE.                                 CL**5
00149                  24  PI-RATE-CODE           PIC XX.                  CL**5
00150                  24  PI-RATE-CLASS          PIC XX.                  CL**5
00151                  24  PI-RATE-DEV            PIC X(3).                CL**5
00152              20  PI-RATE-L-AH-CODE.                                  CL**5
00153                  24  PI-RATE-L-AH           PIC X.                   CL**5
00154                  24  PI-RATE-LAH-NUM        PIC XX.                  CL**5
00155              20  PI-RATE-LIMITS.                                     CL**5
00156                  24  PI-RATE-HIGH-AGE       PIC 99.                  CL**5
00157                  24  PI-RATE-HIGH-AMT       PIC 9(6).                CL**5
00158                  24  PI-RATE-FUTURE         PIC XX.                  CL**5
00159                  24  PI-RATE-SEX            PIC X.                   CL**5
00160              20  PI-RATE-EXPIRY-DATE        PIC 9(11) COMP-3.        CL*10
00161                                                                      CL*10
00162          16  PI-SAVE-ERRATE-KEY             PIC X(28).               CL**5
00163                                                                      CL**5
00164          16  PI-BROWSE-SW                  PIC X.                    CL**5
00165              88  BROWSE-STARTED                  VALUE 'Y'.          CL**5
00166          16  PI-ERRATE-EOF-SW              PIC X.                    CL**5
00167              88  ERRATE-EOF                      VALUE 'Y'.          CL**5
00168                                                                      CL**5
00169          16  PI-SUB                        PIC S999.                 CL**5
00170          16  PI-YEAR                       PIC S99.                  CL**5
00171          16  FILLER                        PIC X(574).               CL*13
00172                                                                   EL6562
00173      EJECT                                                        EL6562
00174                              COPY ELCJPFX.                           CL**5
081413                             PIC X(2000).                            CL**5
00176                                                                      CL**5
00177      EJECT                                                           CL**5
00178                              COPY ELCAID.                            CL**5
00179                                                                   EL6562
00180  01  FILLER    REDEFINES DFHAID.                                     CL**5
00181      05  FILLER              PIC X(8).                               CL**5
00182      05  PF-VALUES           PIC X       OCCURS 2.                   CL**5
00183                                                                      CL**5
00184      EJECT                                                           CL**5
00185                              COPY EL6562S.                           CL**5
00186                                                                      CL**5
00187  01  EL6562AO-R   REDEFINES EL6562AI.                                CL**5
00188      05  FILLER             PIC X(123).                              CL**5
00189      05  SCREEN-TABLE       OCCURS 8 TIMES                           CL**5
00190                             INDEXED BY ST-INDX.                      CL**5
00191          10  YEAR-L         PIC S9(4)         COMP.                  CL**5
00192          10  YEAR-A         PIC X.                                   CL**5
00193          10  YEAR           PIC 99.                                  CL**5
00194          10  YEAR-1         REDEFINES                                CL**5
00195              YEAR           PIC XX.                                  CL**5
00196          10  MONTH-L        PIC S9(4)         COMP.                  CL**5
00197          10  MONTH-A        PIC X.                                   CL**5
00198          10  MONTH          PIC X(7).                                CL**5
00199                                                                      CL**5
00200          10  RATE-TABLE     OCCURS 6 TIMES                           CL**5
00201                             INDEXED BY RT-INDX.                      CL**5
00202                                                                      CL**5
00203              15  RATE-L     PIC S9(4)         COMP.                  CL**5
00204              15  RATE-A     PIC X.                                   CL**5
00205              15  RATE       PIC S9(8).                               CL**5
00206              15  RATE1      REDEFINES                                CL**5
00207                  RATE       PIC ZZ.99999.                            CL**5
00208              15  RATE2      REDEFINES                                CL**5
00209                  RATE       PIC X(8).                                CL**5
00210                                                                   EL6562
00211      05  FILLER             PIC X(5).                                CL**5
00212      05  FILLER             PIC X(75).                               CL**7
00213                                                                   EL6562
00214      EJECT                                                           CL**5
00215                                                                   EL6562
00216  LINKAGE SECTION.                                                    CL**5
00217                                                                      CL**5
00218  01  DFHCOMMAREA             PIC X(1024).                            CL**5
00219                                                                      CL**5
00220      EJECT                                                        EL6562
00221                              COPY ERCRATE.                           CL**5
00222      EJECT                                                           CL**5
00223                              COPY ELCCNTL SUPPRESS.                  CL**5
00224      EJECT                                                           CL**5
00225  PROCEDURE DIVISION.                                                 CL**5
00226                                                                      CL**5
00227      MOVE EIBDATE               TO DC-JULIAN-YYDDD.                  CL**5
00228      MOVE '5'                   TO DC-OPTION-CODE.                   CL**5
00229      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL**5
00230      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                       CL**5
00231      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                   CL**5
00232                                                                      CL**5
00233      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                    CL**5
00234                                                                   EL6562
00235  0100-START.                                                         CL**5
00236      IF EIBCALEN = 0                                                 CL**5
00237          GO TO 8800-UNAUTHORIZED-ACCESS.                             CL**5
00238                                                                      CL**5
00239      IF PI-CALLING-PROGRAM NOT = THIS-PGM                            CL**5
00240          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                      CL**5
00241              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6         CL**5
00242              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5         CL**5
00243              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4         CL**5
00244              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3         CL**5
00245              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2         CL**5
00246              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1         CL**5
00247              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM       CL**5
00248              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL**5
00249          ELSE                                                        CL**5
00250              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM         CL**5
00251              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM       CL**5
00252              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1         CL**5
00253              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2         CL**5
00254              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3         CL**5
00255              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4         CL**5
00256              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5         CL**5
00257              MOVE SPACES               TO PI-SAVED-PROGRAM-6.        CL**5
00258                                                                      CL**5
00259      EXEC CICS HANDLE CONDITION                                      CL**5
00260          NOTOPEN  (9990-ABEND)                                       CL**5
00261          NOTFND   (8880-NOT-FOUND)                                   CL**5
00262          PGMIDERR (9600-PGMID-ERROR)                                 CL**5
00263          ERROR    (9990-ABEND)                                       CL**5
00264          END-EXEC.                                                   CL**5
00265                                                                      CL**5
00266      IF PI-FILE-ID = 'OE'                                            CL**5
00267         MOVE OE-RATE-FILE-ID TO RATE-FILE-ID.                        CL**5
00268                                                                      CL**5
00269      IF EIBTRNID NOT = TRANS-ID                                      CL**5
00270          MOVE LOW-VALUES TO EL6562AI                                 CL**5
00271          MOVE ZEROS      TO MISC-SAVE-AREAS                          CL**5
00272          MOVE +1         TO PI-SUB                                   CL**5
00273                             PI-YEAR                                  CL**5
00274          GO TO 3000-BUILD-INITIAL-SCREEN.                            CL**5
00275                                                                      CL**5
00276      IF EIBAID = DFHCLEAR                                            CL**5
00277          GO TO 9400-CLEAR.                                           CL**5
00278                                                                      CL**5
00279      EJECT                                                           CL**5
00280                                                                      CL**5
00281  0200-RECEIVE.                                                       CL**5
00282      MOVE LOW-VALUES TO EL6562AI.                                    CL**5
00283                                                                      CL**5
00284      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                          CL**5
00285          MOVE ER-0008 TO EMI-ERROR                                   CL**5
00286          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00287          MOVE -1   TO COMMENTL                                       CL**5
00288          GO TO 8200-SEND-DATAONLY.                                   CL**5
00289                                                                      CL**5
00290      EXEC CICS RECEIVE                                               CL**5
00291          MAP    (WS-MAPNAME)                                         CL**5
00292          MAPSET (WS-MAPSET-NAME)                                     CL**5
00293          INTO   (EL6562AI)                                           CL**5
00294          END-EXEC.                                                   CL**5
00295                                                                      CL**5
00296      IF PFENTERL = 0                                                 CL**5
00297          GO TO 0300-CHECK-PFENTERS.                                  CL**5
00298                                                                   EL6562
00299      IF EIBAID NOT = DFHENTER                                        CL**5
00300          MOVE ER-0004 TO EMI-ERROR                                   CL**5
00301          GO TO 0310-INPUT-ERROR.                                     CL**5
00302                                                                      CL**5
00303      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)      CL**5
00304          MOVE PF-VALUES (PFENTERI) TO EIBAID                         CL**5
00305          GO TO 0300-CHECK-PFENTERS                                   CL**5
00306      ELSE                                                            CL**5
00307          MOVE ER-0029 TO EMI-ERROR                                   CL**5
00308          GO TO 0310-INPUT-ERROR.                                     CL**5
00309                                                                      CL**5
00310      EJECT                                                           CL**5
00311                                                                   EL6562
00312  0300-CHECK-PFENTERS.                                                CL**5
00313      IF EIBAID = DFHPF23                                             CL**5
00314          GO TO 8810-PF23.                                            CL**5
00315                                                                      CL**5
00316      IF EIBAID = DFHPF24                                             CL**5
00317          GO TO 9200-RETURN-MAIN-MENU.                                CL**5
00318                                                                      CL**5
00319      IF EIBAID = DFHPF12                                             CL**5
00320          GO TO 9500-PF12.                                            CL**5
00321                                                                      CL**5
00322      IF EIBAID = DFHPF1                                              CL**5
00323          MOVE ZEROS TO MISC-SAVE-AREAS                               CL**5
00324          ADD +48    TO PI-SUB                                        CL**5
00325          ADD +4     TO PI-YEAR                                       CL**5
00326          GO TO 3000-BUILD-INITIAL-SCREEN.                            CL**5
00327                                                                      CL**5
00328      IF EIBAID = DFHPF2                                              CL**5
00329          MOVE ZEROS TO MISC-SAVE-AREAS                               CL**5
00330          SUBTRACT +48 FROM PI-SUB                                    CL**5
00331          SUBTRACT +4  FROM PI-YEAR                                   CL**5
00332          GO TO 3000-BUILD-INITIAL-SCREEN.                            CL**5
00333                                                                      CL**5
00334      IF EIBAID = DFHPF3                                              CL**5
00335          GO TO 7100-NEXT-PLAN.                                       CL**5
00336                                                                   EL6562
00337      IF EIBAID = DFHPF5                                              CL**5
00338          MOVE XCTL-6561 TO PGM-NAME                                  CL**5
00339          GO TO 9300-XCTL.                                            CL**5
00340                                                                      CL**5
00341      IF EIBAID = DFHPF7                                              CL**5
00342          IF NOT MODIFY-CAP                                           CL**5
00343              MOVE 'UPDATE'       TO SM-READ                          CL**5
00344              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**5
00345              MOVE ER-0070 TO EMI-ERROR                               CL**5
00346              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**5
00347              GO TO 8200-SEND-DATAONLY                                CL**5
00348          ELSE                                                        CL**5
00349              MOVE XCTL-6563 TO PGM-NAME                              CL**5
00350              GO TO 9300-XCTL.                                        CL**5
00351                                                                      CL**5
00352      IF EIBAID = DFHPF8                                              CL**5
00353          IF NOT MODIFY-CAP                                           CL**5
00354              MOVE 'UPDATE'       TO SM-READ                          CL**5
00355              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**5
00356              MOVE ER-0070 TO EMI-ERROR                               CL**5
00357              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**5
00358              GO TO 8200-SEND-DATAONLY                                CL**5
00359          ELSE                                                        CL**5
00360              MOVE XCTL-6564 TO PGM-NAME                              CL**5
00361              GO TO 9300-XCTL.                                        CL**5
00362                                                                      CL**5
00363      IF EIBAID = DFHENTER                                            CL**5
00364          GO TO 1000-CHANGE.                                          CL**5
00365                                                                      CL**5
00366      MOVE ER-0029 TO EMI-ERROR.                                      CL**5
00367                                                                      CL**5
00368  0310-INPUT-ERROR.                                                   CL**5
00369      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**5
00370      MOVE AL-UNBON TO PFENTERA.                                      CL**5
00371      MOVE -1 TO PFENTERL.                                            CL**5
00372                                                                   EL6562
00373      GO TO 8200-SEND-DATAONLY.                                       CL**5
00374                                                                      CL**5
00375      EJECT                                                           CL**5
00376                                                                      CL**5
00377  1000-CHANGE.                                                        CL**5
00378      IF NOT MODIFY-CAP                                               CL**5
00379          MOVE 'UPDATE'       TO SM-READ                              CL**5
00380          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT              CL**5
00381          MOVE ER-0070        TO  EMI-ERROR                           CL**5
00382          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00383          GO TO 8100-SEND-INITIAL-MAP.                                CL**5
00384                                                                      CL**5
00385      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                           CL**5
00386          NEXT SENTENCE                                               CL**5
00387      ELSE                                                            CL**5
00388          MOVE ER-2056  TO EMI-ERROR                                  CL**5
00389          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00390          MOVE -1    TO COMMENTL                                      CL**5
00391          GO TO 8200-SEND-DATAONLY.                                   CL**5
00392                                                                   EL6562
00393      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                        CL**5
00394                                                                      CL**5
00395      PERFORM 5000-EDIT THRU 5099-EXIT.                               CL**5
00396                                                                      CL**5
00397      IF EMI-NO-ERRORS                                                CL**5
00398          NEXT SENTENCE                                               CL**5
00399      ELSE                                                            CL**5
00400          GO TO 8200-SEND-DATAONLY.                                   CL**5
00401                                                                   EL6562
00402      PERFORM 7750-READ-ERRATE-UPDATE THRU 7750-EXIT.                 CL**5
00403                                                                   EL6562
00404      MOVE RATE-RECORD TO JP-RECORD-AREA.                             CL**5
00405                                                                      CL**5
00406      PERFORM 4000-UPDATE THRU 4099-EXIT.                             CL**5
00407                                                                      CL**5
00408      IF RT-LAST-MAINT-USER   = PI-UPDATE-BY OR                       CL**5
00409         RT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                      CL**5
00410          NEXT SENTENCE                                               CL**5
00411      ELSE                                                            CL**5
00412          EXEC CICS UNLOCK                                            CL**5
00413               DATASET  (RATE-FILE-ID)                                CL**5
00414          END-EXEC                                                    CL**5
00415          MOVE ER-0068 TO EMI-ERROR                                   CL**5
00416          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00417          GO TO 3000-BUILD-INITIAL-SCREEN.                            CL**5
00418                                                                      CL**5
00419      MOVE PI-PROCESSOR-ID     TO RT-LAST-MAINT-USER.                 CL**5
00420      MOVE EIBTIME             TO RT-LAST-MAINT-HHMMSS.               CL**5
00421                                                                      CL**5
00422      MOVE SAVE-BIN-DATE       TO RT-LAST-MAINT-DT                    CL**5
00423                                  BIN-CURRENT-SAVE.                   CL**5
00424      MOVE 'B'                 TO JP-RECORD-TYPE.                     CL**5
00425      MOVE RATE-FILE-ID        TO FILE-ID.                            CL**5
081413     PERFORM 8400-LOG-JOURNAL-RECORD                                 CL**5
081413                                 thru 8400-exit
00427                                                                      CL**5
00428      EXEC CICS REWRITE                                               CL**5
00429          DATASET  (RATE-FILE-ID)                                     CL**5
00430          FROM     (RATE-RECORD)                                      CL**5
00431      END-EXEC.                                                       CL**5
00432                                                                      CL**5
00433      MOVE RATE-RECORD         TO JP-RECORD-AREA.                     CL**5
00434      MOVE 'C'                 TO JP-RECORD-TYPE                      CL**5
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00436      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.                  CL**5
00437      MOVE ER-0000 TO EMI-ERROR.                                      CL**5
00438      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**5
00439                                                                   EL6562
00440  1299-EXIT.                                                          CL**5
00441      EXIT.                                                           CL**5
00442      EJECT                                                           CL**5
00443                                                                      CL**5
00444  3000-BUILD-INITIAL-SCREEN.                                          CL**5
00445      IF PI-SUB GREATER +360 OR                                       CL**5
00446         PI-SUB LESS THAN +0                                          CL**5
00447             MOVE +1 TO PI-SUB                                        CL**5
00448                        PI-YEAR.                                      CL**5
00449                                                                      CL**5
00450      MOVE LOW-VALUES                    TO  EL6562AO.                CL**5
00451      MOVE ZEROS                         TO  MISC-SAVE-AREAS.         CL**5
00452      MOVE PI-COMPANY-CD                 TO  PI-RATE-COMPANY-CD.      CL**5
00453      MOVE PI-SUB                        TO  SUB1.                    CL**5
00454      MOVE PI-YEAR                       TO  SUB4.                    CL**5
00455      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                        CL**5
00456                                                                      CL**5
00457      MOVE PI-ERRATE-KEY                 TO PI-SAVE-ERRATE-KEY.       CL**5
00458                                                                      CL**5
00459  3025-SET-UP-SCREEN.                                                 CL**5
00460      MOVE RT-ST-CODE                    TO STATEO.                   CL**5
00461      MOVE RT-ST-CLASS                   TO CLASSO.                   CL**5
00462      MOVE RT-ST-DEV                     TO DEVO.                     CL**5
00463                                                                      CL*10
00464 ***  Y2K PROJ 7744                                                   CL*14
00465      MOVE RT-EXPIRY-DATE                TO WS-HOLD-RATE-EXP-DT       CL*11
00466      MOVE WS-HOLD-RATE-EXP-AL(6:6)      TO EXPIREO                   CL*11
00467 ***  Y2K PROJ 7744                                                   CL*14
00468                                                                      CL*10
00469      MOVE RT-L-AH                       TO TYPEO.                    CL**5
00470      MOVE RT-LAH-NUM                    TO PLANO.                    CL**5
00471      MOVE RT-RATE-COMMENT               TO COMMENTO.                 CL**5
00472      MOVE AL-UANON                      TO COMMENTA.                 CL**5
00473      MOVE RT-HIGH-AGE                   TO RTAGEO.                   CL**5
00474                                                                      CL**5
00475      MOVE +1     TO SUB2                                             CL**5
00476                     SUB3.                                            CL**5
00477      SET ST-INDX TO +1.                                              CL**5
00478      SET RT-INDX TO +1.                                              CL**5
00479                                                                      CL**5
00480  3050-SET-UP.                                                        CL**5
00481      IF ST-INDX GREATER +8                                           CL**5
00482          MOVE +1     TO SUB1                                         CL**5
00483                         SUB2                                         CL**5
00484                         SUB3                                         CL**5
00485                         SUB4                                         CL**5
00486          SET ST-INDX TO +1                                           CL**5
00487          SET RT-INDX TO +1                                           CL**5
00488          GO TO 8100-SEND-INITIAL-MAP.                                CL**5
00489                                                                      CL**5
00490      MOVE SUB4 TO YEAR (ST-INDX).                                    CL**5
00491                                                                      CL**5
00492      IF SUB4 GREATER +30                                             CL**5
00493          MOVE SPACES       TO MONTH    (ST-INDX)                     CL**5
00494                               YEAR-1   (ST-INDX)                     CL**5
00495          MOVE AL-SADOF     TO MONTH-A  (ST-INDX)                     CL**5
00496                               YEAR-A   (ST-INDX)                     CL**5
00497      ELSE                                                            CL**5
00498          IF EVEN-SUB                                                 CL**5
00499              COMPUTE WS-FROM = (SUB4 * 12) - 5                       CL**5
00500              COMPUTE WS-TO   = WS-FROM + 5                           CL**5
00501              ADD +1 TO SUB4                                          CL**5
00502              MOVE WS-TERM  TO MONTH   (ST-INDX)                      CL**5
00503              MOVE AL-SADOF TO YEAR-A  (ST-INDX)                      CL**5
00504              MOVE AL-SANON TO MONTH-A (ST-INDX)                      CL**5
00505          ELSE                                                        CL**5
00506              COMPUTE WS-FROM = (SUB4 * 12) - 11                      CL**5
00507              COMPUTE WS-TO   = WS-FROM + 5                           CL**5
00508              MOVE WS-TERM  TO MONTH   (ST-INDX)                      CL**5
00509              MOVE AL-SANON TO YEAR-A  (ST-INDX)                      CL**5
00510                               MONTH-A (ST-INDX).                     CL**5
00511                                                                      CL**5
00512  3075-SET-UP-RATES.                                                  CL**5
00513      IF SUB1 GREATER +360                                            CL**5
00514          MOVE SPACES             TO RATE2 (ST-INDX RT-INDX)          CL**5
00515          MOVE AL-SADOF           TO RATE-A (ST-INDX RT-INDX)         CL**5
00516      ELSE                                                            CL**5
00517          MOVE AL-UNNON           TO RATE-A (ST-INDX RT-INDX)         CL**5
00518          IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                         CL**5
00519              MOVE RT-AH-RATE (SUB1) TO RATE1 (ST-INDX RT-INDX)       CL**5
00520                                        WS-SAVE-RATE (SUB2)           CL**5
00521          ELSE                                                        CL**5
00522              IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                   CL**5
00523                  MOVE RT-L-RATE (SUB1)  TO RATE1 (ST-INDX RT-INDX)   CL**5
00524                                            WS-SAVE-RATE (SUB2).      CL**5
00525                                                                      CL**5
00526                                                                      CL**5
00527      ADD +1 TO SUB1                                                  CL**5
00528                SUB2.                                                 CL**5
00529      SET RT-INDX UP BY +1.                                           CL**5
00530                                                                   EL6562
00531      IF RT-INDX GREATER +6                                           CL**5
00532          SET RT-INDX TO +1                                           CL**5
00533          SET ST-INDX UP BY +1                                        CL**5
00534          ADD +1  TO  SUB3                                            CL**5
00535          GO TO 3050-SET-UP                                           CL**5
00536      ELSE                                                            CL**5
00537          GO TO 3075-SET-UP-RATES.                                    CL**5
00538                                                                      CL**5
00539                                                                   EL6562
00540  3099-EXIT.                                                          CL**5
00541      EXIT.                                                           CL**5
00542      EJECT                                                           CL**5
00543                                                                      CL**5
00544  4000-UPDATE.                                                        CL**5
00545       IF COMMENTL GREATER ZERO                                       CL**5
00546           MOVE COMMENTI TO RT-RATE-COMMENT.                          CL**5
00547                                                                      CL**5
00548       MOVE +1     TO SUB1.                                           CL**5
00549       MOVE PI-SUB TO SUB2.                                           CL**5
00550                                                                      CL**5
00551  4025-CONT.                                                          CL**5
00552      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                           CL**5
00553          MOVE WS-SAVE-RATE (SUB1) TO RT-L-RATE (SUB2)                CL**5
00554      ELSE                                                         EL6562
00555          MOVE WS-SAVE-RATE (SUB1) TO RT-AH-RATE (SUB2).              CL**5
00556                                                                   EL6562
00557      ADD +1 TO SUB1                                                  CL**5
00558                SUB2.                                                 CL**5
00559                                                                      CL**5
00560      IF SUB1 GREATER +48 OR SUB2 GREATER +360                        CL**5
00561         NEXT SENTENCE                                                CL**5
00562      ELSE                                                            CL**5
00563          GO TO 4025-CONT.                                            CL**5
00564                                                                      CL**5
00565  4099-EXIT.                                                          CL**5
00566      EXIT.                                                           CL**5
00567      EJECT                                                           CL**5
00568                                                                      CL**5
00569  5000-EDIT.                                                          CL**5
00570       SET ST-INDX TO +1.                                             CL**5
00571       SET RT-INDX TO +1.                                             CL**5
00572       MOVE +1 TO SUB1.                                               CL**5
00573                                                                      CL**5
00574  5050-CONT.                                                          CL**5
00575      IF ST-INDX GREATER +8                                           CL**5
00576          SET ST-INDX TO +1                                           CL**5
00577          SET RT-INDX TO +1                                           CL**5
00578          MOVE +1 TO SUB1                                             CL**5
00579          GO TO 5099-EXIT.                                            CL**5
00580                                                                      CL**5
00581      IF RATE-L (ST-INDX RT-INDX) GREATER ZERO                        CL**5
00582          MOVE RATE (ST-INDX RT-INDX) TO DEEDIT-FIELD                 CL**5
00583          PERFORM 7500-DEEDIT THRU 7500-EXIT                          CL**5
00584          IF DEEDIT-FIELD-V0 NUMERIC                                  CL**5
00585              MOVE DEEDIT-FIELD-V1 TO WS-SAVE-RATE (SUB1)             CL**5
00586                                      RATE1 (ST-INDX RT-INDX)         CL**5
00587              MOVE AL-UNNON        TO RATE-A (ST-INDX RT-INDX)        CL**5
00588          ELSE                                                        CL**5
00589              MOVE -1       TO RATE-L (ST-INDX RT-INDX)               CL**5
00590              MOVE AL-UNBON TO RATE-A (ST-INDX RT-INDX)               CL**5
00591              MOVE ER-2280  TO EMI-ERROR                              CL**5
00592              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**5
00593      ELSE                                                            CL**5
00594          MOVE ZEROS        TO RATE1 (ST-INDX RT-INDX)                CL**5
00595                               WS-SAVE-RATE (SUB1)                    CL**5
00596          MOVE AL-UNNON     TO RATE-A (ST-INDX RT-INDX).              CL**5
00597                                                                      CL**5
00598      SET RT-INDX UP BY +1.                                           CL**5
00599      ADD +1 TO SUB1.                                                 CL**5
00600                                                                      CL**5
00601      IF RT-INDX GREATER +6                                           CL**5
00602          SET ST-INDX UP BY +1                                        CL**5
00603          SET RT-INDX TO +1.                                          CL**5
00604                                                                      CL**5
00605      GO TO 5050-CONT.                                                CL**5
00606                                                                      CL**5
00607  5099-EXIT.                                                          CL**5
00608      EXIT.                                                           CL**5
00609      EJECT                                                           CL**5
00610                                                                      CL**5
00611  7100-NEXT-PLAN.                                                     CL**5
00612      MOVE SPACES             TO PI-ERRATE-EOF-SW.                    CL**5
00613                                                                      CL**5
00614      MOVE PI-COMPANY-CD      TO PI-RATE-COMPANY-CD.                  CL**5
00615      MOVE PI-ERRATE-KEY      TO PI-SAVE-ERRATE-KEY.                  CL**5
00616      MOVE PI-RATE-STATE-CODE TO WS-SAVE-STRUCTURE.                   CL**5
00617                                                                   EL6562
00618      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                       CL**5
00619                                                                   EL6562
00620  7125-READ-NEXT.                                                     CL**5
00621      EXEC CICS HANDLE CONDITION                                      CL**5
00622          ENDFILE  (7150-ENDFILE)                                     CL**5
00623          NOTFND   (7175-NOTFOUND)                                    CL**5
00624      END-EXEC.                                                       CL**5
00625                                                                   EL6562
00626      PERFORM 7850-READNEXT THRU 7850-EXIT.                           CL**5
00627                                                                      CL**5
00628      IF ERRATE-EOF                                                   CL**5
00629          IF BROWSE-STARTED                                           CL**5
00630              PERFORM 7950-END-BROWSE THRU 7950-EXIT                  CL**5
00631              MOVE LOW-VALUES TO PI-ERRATE-KEY                        CL**5
00632              GO TO 7100-NEXT-PLAN.                                   CL**5
00633                                                                      CL**5
00634      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                           CL**5
00635          GO TO 7125-READ-NEXT.                                       CL**5
00636                                                                      CL**5
00637      IF WS-SAVE-STRUCTURE = LOW-VALUES                               CL**5
00638          MOVE PI-RATE-STATE-CODE TO WS-SAVE-STRUCTURE.               CL**5
00639                                                                      CL**5
00640      IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE                       CL**5
00641          NEXT SENTENCE                                               CL**5
00642      ELSE                                                            CL**5
00643          PERFORM 7950-END-BROWSE THRU 7950-EXIT                      CL**5
00644          MOVE PI-SAVE-ERRATE-KEY TO PI-ERRATE-KEY                    CL**5
00645          MOVE LOW-VALUES         TO PI-RATE-L-AH-CODE                CL**5
00646          MOVE ZEROS              TO PI-RATE-EXPIRY-DATE              CL*12
00647          MOVE ER-2285            TO EMI-ERROR                        CL**5
00648          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00649          GO TO 7100-NEXT-PLAN.                                       CL**5
00650                                                                   EL6562
00651  7130-DISPLAY-SCREEN.                                                CL**5
00652      IF BROWSE-STARTED                                               CL**6
00653          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                     CL**6
00654                                                                      CL**6
00655      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                    CL**5
00656      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.                CL**5
00657      MOVE PI-ERRATE-KEY          TO PI-SAVE-ERRATE-KEY.              CL**5
00658      MOVE +1                     TO PI-SUB  PI-YEAR.                 CL**5
00659                                                                      CL**5
00660      GO TO 3000-BUILD-INITIAL-SCREEN.                                CL**5
00661                                                                      CL**5
00662  7150-ENDFILE.                                                       CL**5
00663      IF BROWSE-STARTED                                               CL**5
00664          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                     CL**5
00665                                                                      CL**5
00666      MOVE ER-2271 TO EMI-ERROR.                                      CL**5
00667      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**5
00668      MOVE LOW-VALUES TO EL6562AO                                     CL**5
00669                         PI-ERRATE-KEY.                               CL**5
00670      GO TO 7100-NEXT-PLAN.                                           CL**5
00671                                                                      CL**5
00672  7175-NOTFOUND.                                                      CL**5
00673      IF BROWSE-STARTED                                               CL**5
00674          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                     CL**5
00675                                                                      CL**5
00676      GO TO 8880-NOT-FOUND.                                           CL**5
00677                                                                      CL**5
00678  7199-EXIT.                                                          CL**5
00679      EXIT.                                                           CL**5
00680      EJECT                                                           CL**5
00681                                                                      CL**5
00682  7500-DEEDIT.                                                        CL**5
00683      EXEC CICS BIF                                                   CL**5
00684           DEEDIT                                                     CL**5
00685           FIELD  (DEEDIT-FIELD)                                      CL**5
00686           LENGTH (15)                                                CL**5
00687      END-EXEC.                                                       CL**5
00688                                                                      CL**5
00689  7500-EXIT.                                                          CL**5
00690      EXIT.                                                           CL**5
00691      EJECT                                                           CL**5
00692                                                                      CL**5
00693  7650-READ-ERRATE.                                                   CL**5
00694      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                     CL**5
00695                                                                   EL6562
00696      EXEC CICS READ                                                  CL**5
00697           DATASET  (RATE-FILE-ID)                                    CL**5
00698           SET      (ADDRESS OF RATE-RECORD)                          CL**7
00699           RIDFLD   (PI-ERRATE-KEY)                                   CL**5
00700      END-EXEC.                                                       CL**5
00701                                                                      CL**5
00702      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                    CL**5
00703      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.                CL**5
00704                                                                      CL**5
00705  7650-EXIT.                                                          CL**5
00706      EXIT.                                                           CL**5
00707      EJECT                                                           CL**5
00708                                                                      CL**5
00709  7750-READ-ERRATE-UPDATE.                                            CL**5
00710      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                     CL**5
00711                                                                      CL**5
00712      EXEC CICS READ                                                  CL**5
00713           DATASET  (RATE-FILE-ID)                                    CL**5
00714           SET      (ADDRESS OF RATE-RECORD)                          CL**7
00715           RIDFLD   (PI-ERRATE-KEY)                                   CL**5
00716           UPDATE                                                     CL**5
00717      END-EXEC.                                                       CL**5
00718                                                                      CL**5
00719  7750-EXIT.                                                          CL**5
00720      EXIT.                                                           CL**5
00721      EJECT                                                           CL**5
00722                                                                   EL6562
00723  7800-START-BROWSE.                                                  CL**5
00724      EXEC CICS STARTBR                                               CL**5
00725           DATASET  (RATE-FILE-ID)                                    CL**5
00726           RIDFLD   (PI-ERRATE-KEY)                                   CL**5
00727      END-EXEC.                                                       CL**5
00728                                                                      CL**5
00729      MOVE 'Y' TO PI-BROWSE-SW.                                       CL**5
00730                                                                      CL**5
00731  7800-EXIT.                                                          CL**5
00732      EXIT.                                                           CL**5
00733      EJECT                                                           CL**5
00734                                                                      CL**5
00735  7850-READNEXT.                                                      CL**5
00736      EXEC CICS READNEXT                                              CL**5
00737           DATASET  (RATE-FILE-ID)                                    CL**5
00738           SET      (ADDRESS OF RATE-RECORD)                          CL**7
00739           RIDFLD   (PI-ERRATE-KEY)                                   CL**5
00740      END-EXEC.                                                       CL**5
00741                                                                      CL**5
00742      IF PI-COMPANY-CD NOT = RT-COMPANY-CD                            CL**5
00743          MOVE LOW-VALUES TO EL6562AO                                 CL**5
00744          MOVE ER-2271    TO EMI-ERROR                                CL**5
00745          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
00746          MOVE 'Y'        TO PI-ERRATE-EOF-SW.                        CL**5
00747                                                                      CL**5
00748  7850-EXIT.                                                          CL**5
00749      EXIT.                                                           CL**5
00750      EJECT                                                           CL**5
00751                                                                      CL**5
00752  7950-END-BROWSE.                                                    CL**5
00753      EXEC CICS ENDBR                                                 CL**5
00754           DATASET  (RATE-FILE-ID)                                    CL**5
00755      END-EXEC.                                                       CL**5
00756                                                                      CL**5
00757      MOVE SPACE TO PI-BROWSE-SW.                                     CL**5
00758                                                                      CL**5
00759  7950-EXIT.                                                          CL**5
00760      EXIT.                                                           CL**5
00761      EJECT                                                           CL**5
00762                                                                      CL**5
00763  8000-UPDATE-MAINT-DATE.                                             CL**5
00764      MOVE SPACES                 TO ELCNTL-KEY.                      CL**5
00765                                                                   EL6562
00766      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL**5
00767      MOVE '1'                    TO CNTL-REC-TYPE.                   CL**5
00768      MOVE +0                     TO CNTL-SEQ-NO.                     CL**5
00769                                                                      CL**5
00770      EXEC CICS HANDLE CONDITION                                      CL**5
00771          NOTFND   (8000-EXIT)                                        CL**5
00772      END-EXEC.                                                       CL**5
00773                                                                      CL**5
00774      EXEC CICS READ                                                  CL**5
00775          UPDATE                                                      CL**5
00776          DATASET   (CNTL-FILE-ID)                                    CL**5
00777          SET       (ADDRESS OF CONTROL-FILE)                         CL**7
00778          RIDFLD    (ELCNTL-KEY)                                      CL**5
00779      END-EXEC.                                                       CL**5
00780                                                                      CL**5
00781      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**5
00782      MOVE 'B'                    TO JP-RECORD-TYPE.                  CL**5
00783      MOVE CNTL-FILE-ID           TO FILE-ID.                         CL**5
00784 *    PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**5
00785                                                                      CL**5
00786      MOVE BIN-CURRENT-SAVE       TO CF-RATES-FILE-MAINT-DT.          CL**5
00787                                                                      CL**5
00788      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**5
00789      MOVE 'C'                    TO JP-RECORD-TYPE.                  CL**5
00790      MOVE CNTL-FILE-ID           TO FILE-ID.                         CL**5
00791                                                                      CL**5
00792      EXEC CICS REWRITE                                               CL**5
00793          DATASET   (CNTL-FILE-ID)                                    CL**5
00794          FROM      (CONTROL-FILE)                                    CL**5
00795      END-EXEC.                                                       CL**5
00796                                                                      CL**5
00797 *    PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**5
00798                                                                      CL**5
00799  8000-EXIT.                                                          CL**5
00800       EXIT.                                                          CL**5
00801      EJECT                                                           CL**5
00802                                                                      CL**5
00803  8100-SEND-INITIAL-MAP.                                              CL**5
00804      MOVE EIBTIME              TO TIME-IN.                           CL**5
00805      MOVE SAVE-DATE            TO RUNDATEO.                          CL**5
00806      MOVE TIME-OUT             TO RUNTIMEO.                          CL**5
00807      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                          CL**5
00808      MOVE -1 TO COMMENTL.                                            CL**5
00809      EXEC CICS SEND                                                  CL**5
00810          MAP   (WS-MAPNAME)                                          CL**5
00811          MAPSET(WS-MAPSET-NAME)                                      CL**5
00812          FROM  (EL6562AO)                                            CL**5
00813          ERASE                                                       CL**5
00814          CURSOR                                                      CL**5
00815      END-EXEC.                                                       CL**5
00816                                                                      CL**5
00817      GO TO 9100-RETURN-TRAN.                                         CL**5
00818                                                                      CL**5
00819  8200-SEND-DATAONLY.                                                 CL**5
00820      MOVE EIBTIME              TO TIME-IN.                           CL**5
00821      MOVE SAVE-DATE            TO RUNDATEO.                          CL**5
00822      MOVE TIME-OUT             TO RUNTIMEO.                          CL**5
00823      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                          CL**5
00824      EXEC CICS SEND                                                  CL**5
00825          MAP   (WS-MAPNAME)                                          CL**5
00826          MAPSET(WS-MAPSET-NAME)                                      CL**5
00827          FROM  (EL6562AO)                                            CL**5
00828          DATAONLY                                                    CL**5
00829          CURSOR                                                      CL**5
00830      END-EXEC.                                                       CL**5
00831                                                                      CL**5
00832      GO TO 9100-RETURN-TRAN.                                         CL**5
00833                                                                   EL6562
00834  8300-SEND-TEXT.                                                     CL**5
00835      EXEC CICS SEND TEXT                                             CL**5
00836          FROM  (LOGOFF-TEXT)                                         CL**5
00837          LENGTH(LOGOFF-LENGTH)                                       CL**5
00838          ERASE                                                       CL**5
00839          FREEKB                                                      CL**5
00840      END-EXEC.                                                       CL**5
00841                                                                   EL6562
00842      EXEC CICS RETURN                                                CL**5
00843      END-EXEC.                                                       CL**5
00844                                                                      CL**5
081413 8400-LOG-JOURNAL-RECORD.
081413
081413     if pi-journal-file-id = 0
081413        go to 8400-exit
081413     end-if
081413
081413     move eibdate                to jp-date
081413     move eibtime                to jp-time
081413     move RATE-FILE-ID           TO JP-FILE-ID
081413     MOVE PI-PROCESSOR-ID        TO JP-USER-ID
081413     MOVE 02                     TO PI-JOURNAL-FILE-ID
081413     MOVE THIS-PGM               TO JP-PROGRAM-ID
081413
081413     EXEC CICS JOURNAL
081413        JFILEID   (PI-JOURNAL-FILE-ID)
081413        JTYPEID   ('EL')
081413        FROM      (JOURNAL-RECORD)
081413        LENGTH    (2000)
081413        resp      (ws-response)
081413     END-EXEC
081413
081413     if ws-resp-normal
081413        continue
081413     else
081413        display ' error-el656-journal ' ws-response
081413     end-if
081413
081413     .
081413 8400-exit.
081413     exit.

00858  8800-UNAUTHORIZED-ACCESS.                                           CL**5
00859      MOVE UNACCESS-MSG TO LOGOFF-MSG.                                CL**5
00860      GO TO 8300-SEND-TEXT.                                           CL**5
00861                                                                      CL**5
00862  8810-PF23.                                                          CL**5
00863      MOVE EIBAID   TO PI-ENTRY-CD-1.                                 CL**5
00864      MOVE XCTL-005 TO PGM-NAME.                                      CL**5
00865      GO TO 9300-XCTL.                                                CL**5
00866                                                                   EL6562
00867  8880-NOT-FOUND.                                                     CL**5
00868      MOVE ER-0142 TO EMI-ERROR.                                      CL**5
00869      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**5
00870      MOVE -1   TO COMMENTL.                                          CL**5
00871                                                                      CL**5
00872      IF EIBTRNID NOT = TRANS-ID                                      CL**5
00873          GO TO 8100-SEND-INITIAL-MAP.                                CL**5
00874                                                                      CL**5
00875      GO TO 8200-SEND-DATAONLY.                                       CL**5
00876                                                                      CL**5
00877  9100-RETURN-TRAN.                                                   CL**5
00878      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.                  CL**5
00879      MOVE '656C'               TO PI-CURRENT-SCREEN-NO.              CL**5
00880      EXEC CICS RETURN                                                CL**5
00881          TRANSID (TRANS-ID)                                          CL**5
00882          COMMAREA(PROGRAM-INTERFACE-BLOCK)                           CL**5
00883          LENGTH  (PI-COMM-LENGTH)                                    CL**5
00884      END-EXEC.                                                       CL**5
00885                                                                      CL**5
00886  9200-RETURN-MAIN-MENU.                                              CL**5
00887      MOVE XCTL-626 TO PGM-NAME.                                      CL**5
00888      GO TO 9300-XCTL.                                                CL**5
00889                                                                      CL**5
00890  9300-XCTL.                                                          CL**5
00891      EXEC CICS XCTL                                                  CL**5
00892          PROGRAM (PGM-NAME)                                          CL**5
00893          COMMAREA(PROGRAM-INTERFACE-BLOCK)                           CL**5
00894          LENGTH  (PI-COMM-LENGTH)                                    CL**5
00895      END-EXEC.                                                       CL**5
00896                                                                   EL6562
00897  9400-CLEAR.                                                         CL**5
00898      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                          CL**5
00899      GO TO 9300-XCTL.                                                CL**5
00900                                                                      CL**5
00901  9500-PF12.                                                          CL**5
00902      MOVE XCTL-010 TO PGM-NAME.                                      CL**5
00903      GO TO 9300-XCTL.                                                CL**5
00904                                                                      CL**5
00905  9600-PGMID-ERROR.                                                   CL**5
00906      EXEC CICS HANDLE CONDITION                                      CL**5
00907          PGMIDERR(8300-SEND-TEXT)                                    CL**5
00908      END-EXEC.                                                       CL**5
00909                                                                      CL**5
00910      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                        CL**5
00911      MOVE ' '          TO PI-ENTRY-CD-1.                             CL**5
00912      MOVE XCTL-005     TO PGM-NAME.                                  CL**5
00913      MOVE PGM-NAME     TO LOGOFF-PGM.                                CL**5
00914      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                               CL**5
00915      GO TO 9300-XCTL.                                                CL**5
00916                                                                      CL**5
00917  9700-LINK-DATE-CONVERT.                                             CL**5
00918      MOVE LINK-ELDATCV TO PGM-NAME.                                  CL**5
00919                                                                      CL**5
00920      EXEC CICS LINK                                                  CL**5
00921          PROGRAM (PGM-NAME)                                          CL**5
00922          COMMAREA(DATE-CONVERSION-DATA)                              CL**5
00923          LENGTH  (DC-COMM-LENGTH)                                    CL**5
00924      END-EXEC.                                                       CL**5
00925                                                                      CL**5
00926  9700-EXIT.                                                          CL**5
00927      EXIT.                                                           CL**5
00928                                                                      CL**5
00929  9900-ERROR-FORMAT.                                                  CL**5
00930      IF NOT EMI-ERRORS-COMPLETE                                      CL**5
00931          MOVE LINK-001 TO PGM-NAME                                   CL**5
00932          EXEC CICS LINK                                              CL**5
00933              PROGRAM (PGM-NAME)                                      CL**5
00934              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)                 CL**5
00935              LENGTH  (EMI-COMM-LENGTH)                               CL**5
00936          END-EXEC.                                                   CL**5
00937                                                                      CL**5
00938  9900-EXIT.                                                          CL**5
00939      EXIT.                                                           CL**5
00940                                                                      CL**5
00941  9990-ABEND.                                                         CL**5
00942      MOVE LINK-004 TO PGM-NAME.                                      CL**5
00943      MOVE DFHEIBLK               TO EMI-LINE1                        CL**5
00944      EXEC CICS LINK                                                  CL**5
00945          PROGRAM   (PGM-NAME)                                        CL**5
00946          COMMAREA  (EMI-LINE1)                                       CL**5
00947          LENGTH    (72)                                              CL**5
00948      END-EXEC.                                                       CL**5
00949                                                                      CL**5
00950      GO TO 8200-SEND-DATAONLY.                                       CL**5
00951                                                                      CL**5
00952      GOBACK.                                                         CL**5
00953                                                                      CL**5
00954  9995-SECURITY-VIOLATION.                                            CL**5
00955                              COPY ELCSCTP.                           CL**5
00956                                                                      CL**5
00957  9995-EXIT.                                                          CL**5
00958       EXIT.                                                          CL**5
