00001  ID DIVISION.                                                     04/21/98
00002                                                                   EL6561
00003  PROGRAM-ID.                 EL6561.                                 LV037
00004 *              PROGRAM CONVERTED BY                                  CL*16
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*16
00006 *              CONVERSION DATE 02/12/96 13:22:43.                    CL*16
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE               CL*34
00008 *                            VMOD=2.021                              CL*33
00009                                                                   EL6561
00010 *AUTHOR.     LOGIC,INC.                                              CL*16
00011 *            DALLAS, TEXAS.                                          CL*16
00012                                                                   EL6561
00013 *DATE-COMPILED.                                                      CL*16
00014 *SECURITY.   *****************************************************   CL*16
00015 *            *                                                   *   CL*16
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*16
00017 *            *                                                   *   CL*16
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*16
00019 *                                                                *   CL*16
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*16
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*16
00022 *            *                                                   *   CL*16
00023 *            *****************************************************   CL*16
00024                                                                   EL6561
00025 *REMARKS.    TRANSACTION - EXE2 - RATE MASTER MAINTENANCE            CL*16
00026 *                                 LIFE AND A&H LIMITS.               CL*16
00027      EJECT                                                        EL6561
081413******************************************************************
081413*                   C H A N G E   L O G
081413*
081413* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081413*-----------------------------------------------------------------
081413*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081413* EFFECTIVE    NUMBER
081413*-----------------------------------------------------------------
081413* 081413    2013080700002  PEMA ADD JOURNALING OF ERRATE FILE
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
012820* 012820  CR2020012800001  PEMA  Addd minimum Loan term field
022420* 022420  IR2020022400002  PEMA  fix minimum loan term
081413******************************************************************
00028  ENVIRONMENT DIVISION.                                            EL6561
00029  DATA DIVISION.                                                   EL6561
00030  WORKING-STORAGE SECTION.                                         EL6561
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6561
00032  77  FILLER  PIC X(32)  VALUE '*    EL6561 WORKING STORAGE    *'. EL6561
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.021 *********'.    CL*33
00034                                                                   EL6561
00035  01  WS-DATE-AREA.                                                EL6561
00036      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL6561
00037      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.               CL**6
00038                                                                   EL6561
00039 ***  Y2K PROJ 7744                                                   CL*33
00040  01  WS-RATE-HOLD-AREA.                                              CL*24
00041      05  WS-HOLD-RATE-EXP-AL PIC X(11).                              CL*24
00042      05  WS-HOLD-RATE-EXP-DT REDEFINES                               CL*24
00043          WS-HOLD-RATE-EXP-AL PIC 9(11).                              CL*24
00044 ***  Y2K PROJ 7744                                                   CL*33
00045                                                                      CL*24
00046  01  STANDARD-AREAS.                                              EL6561
00047      05  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL6561
00048      05  WS-MAPNAME          PIC X(8)    VALUE 'EL6561A'.         EL6561
00049      05  WS-MAPSET-NAME      PIC X(8)    VALUE 'EL6561S'.         EL6561
00050      05  TRANS-ID            PIC X(4)    VALUE 'EXE2'.            EL6561
00051      05  THIS-PGM            PIC X(8)    VALUE 'EL6561'.          EL6561
00052      05  PGM-NAME            PIC X(8).                            EL6561
00053      05  SUB1                PIC S999    VALUE +0.                EL6561
00054      05  SUB2                PIC S99     VALUE +0.                EL6561
00055      05  SC-ITEM             PIC S9(4)   VALUE +1     COMP.       EL6561
00056      05  WS-COMP-CD-R.                                            EL6561
00057          10  FILLER          PIC X.                               EL6561
00058          10  WS-COMP-CD-X    PIC X.                               EL6561
00059      05  WS-COMP-CD   REDEFINES WS-COMP-CD-R  PIC S9(4)     COMP. EL6561
00060      05  TIME-IN             PIC S9(7).                           EL6561
00061      05  TIME-OUT-R  REDEFINES TIME-IN.                           EL6561
00062          10  FILLER          PIC X.                               EL6561
00063          10  TIME-OUT        PIC 99V99.                           EL6561
00064          10  FILLER          PIC XX.                                 CL**6
00065      05  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL6561
00066      05  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL6561
00067      05  XCTL-626            PIC X(8)    VALUE 'EL126'.           EL6561
00068      05  XCTL-6562           PIC X(8)    VALUE 'EL6562'.          EL6561
00069      05  LINK-001            PIC X(8)    VALUE 'EL001'.           EL6561
00070      05  LINK-004            PIC X(8)    VALUE 'EL004'.           EL6561
00071                                                                   EL6561
00072      05  ER-0000             PIC X(4)    VALUE  '0000'.           EL6561
00073      05  ER-0004             PIC X(4)    VALUE  '0004'.           EL6561
00074      05  ER-0008             PIC X(4)    VALUE  '0008'.           EL6561
00075      05  ER-0029             PIC X(4)    VALUE  '0029'.           EL6561
00076      05  ER-0068             PIC X(4)    VALUE  '0068'.           EL6561
00077      05  ER-0070             PIC X(4)    VALUE  '0070'.           EL6561
00078      05  ER-0142             PIC X(4)    VALUE  '0142'.           EL6561
00079      05  ER-2055             PIC X(4)    VALUE  '2055'.           EL6561
00080      05  ER-2056             PIC X(4)    VALUE  '2056'.           EL6561
00081      05  ER-2184             PIC X(4)    VALUE  '2184'.           EL6561
00082      05  ER-2185             PIC X(4)    VALUE  '2185'.           EL6561
00083      05  ER-2186             PIC X(4)    VALUE  '2186'.           EL6561
00084      05  ER-2187             PIC X(4)    VALUE  '2187'.           EL6561
00085      05  ER-2237             PIC X(4)    VALUE  '2237'.              CL**3
00086      05  ER-2273             PIC X(4)    VALUE  '2273'.           EL6561
00087      05  ER-2274             PIC X(4)    VALUE  '2274'.           EL6561
00088      05  ER-2275             PIC X(4)    VALUE  '2275'.           EL6561
00089      05  ER-2276             PIC X(4)    VALUE  '2276'.           EL6561
00090      05  ER-2277             PIC X(4)    VALUE  '2277'.           EL6561
00091      05  ER-2278             PIC X(4)    VALUE  '2278'.           EL6561
00092      05  ER-2279             PIC X(4)    VALUE  '2279'.           EL6561
00093      05  ER-2280             PIC X(4)    VALUE  '2280'.              CL**3
00094      05  ER-2285             PIC X(4)    VALUE  '2285'.           EL6561
00095 ***  Y2K PROJ 7744                                                   CL*33
00096      05  ER-2296             PIC X(4)    VALUE  '2296'.              CL*31
00097 ***  Y2K PROJ 7744                                                   CL*33
00098      05  ER-2546             PIC X(4)    VALUE  '2546'.              CL**5
00099      05  ER-2873             PIC X(4)    VALUE  '2873'.              CL*19
00100      05  ER-2874             PIC X(4)    VALUE  '2874'.              CL*19
00101      05  ER-2875             PIC X(4)    VALUE  '2875'.              CL*19
00102      05  ER-9407             PIC X(4)    VALUE  '9407'.              CL**9
00103                                                                   EL6561
00104      05  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL6561
00105      05  RATE-FILE-ID        PIC X(8)    VALUE 'ERRATE'.          EL6561
00106      05  OE-RATE-FILE-ID     PIC X(8)    VALUE 'OERATE'.          EL6561
00107      05  CNTL-FILE-ID        PIC X(8)    VALUE 'ELCNTL'.          EL6561
00108      05  FILE-ID             PIC X(8)    VALUE  SPACES.           EL6561
00109      05  BIN-CURRENT-SAVE    PIC XX      VALUE  SPACES.           EL6561
00110                                                                   EL6561
081413 01  filler.
081413     12  WS-RESPONSE             PIC S9(8)   COMP.                    
081413         88  WS-RESP-NORMAL              VALUE +00.               
081413         88  WS-RESP-ERROR               VALUE +01.               
081413         88  WS-RESP-NOTFND              VALUE +13.               
081413         88  WS-RESP-DUPKEY              VALUE +15.
081413         88  WS-RESP-NOTOPEN             VALUE +19.
081413         88  WS-RESP-ENDFILE             VALUE +20.

00111  01  MISC-WORK-AREAS.                                             EL6561
00112      05  WS-PHONE-IN         PIC 9(10).                           EL6561
00113      05  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.                    EL6561
00114          10  WSPI-AREA       PIC XXX.                                CL**6
00115          10  WSPI-PFX        PIC XXX.                                CL**6
00116          10  WSPI-SFX        PIC X(4).                            EL6561
00117      05  WS-PHONE-OUT.                                            EL6561
00118          10  WSPO-AREA       PIC XXX.                                CL**6
00119          10  FILLER          PIC X       VALUE '-'.               EL6561
00120          10  WSPO-PFX        PIC XXX.                                CL**6
00121          10  FILLER          PIC X       VALUE '-'.               EL6561
00122          10  WSPO-SFX        PIC X(4).                            EL6561
00123                                                                   EL6561
00124      05  DEEDIT-FIELD            PIC X(15).                       EL6561
00125      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6561
00126      05  DEEDIT-FIELD-V2  REDEFINES DEEDIT-FIELD PIC S9(13)V99.      CL**9
00127      05  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).    CL**5
00128                                                                   EL6561
00129      05  ERRATE-LENGTH       PIC S9(4)    VALUE +1765 COMP.       EL6561
00130      05  MORTCD-LENGTH       PIC S9(4)    VALUE +4    COMP.          CL**3
00131      05  SV-CLMTOL           PIC 999V99   VALUE ZEROS.               CL**6
00132      05  DATE-TEST-AREA      PIC 9(6).                            EL6561
00133      05  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.              EL6561
00134          10  DATE-TEST-MM    PIC 99.                              EL6561
00135          10  DATE-TEST-DD    PIC 99.                              EL6561
00136          10  DATE-TEST-YY    PIC 99.                              EL6561
00137      05  DIVIDE-RESULT       PIC 99.                              EL6561
00138      05  DIVIDE-REMAINDER    PIC 9.                               EL6561
00139                                                                   EL6561
00140      05  WS-PREV-AGE         PIC 99       VALUE ZERO.             EL6561
00141      05  WS-PREV-TRM         PIC 999      VALUE ZERO.             EL6561
00142      05  WS-PREV-AMT         PIC S9(7)V99 VALUE ZERO.             EL6561
00143      05  WS-DAILY-RATE       PIC S99V9(5) VALUE ZERO.                CL**3
00144      05  WS-DISCOUNT-RATE    PIC S99V9(5) VALUE ZERO.                CL**5
010716     05  WS-CANCEL-FEE       PIC S9(03)V99 VALUE +0.                 CL**9
00146      05  WS-COMPOSITE-RATE   PIC S99V9(5) VALUE ZERO.                CL**6
00147      05  WS-DISCOUNT-OB-RATE PIC S99V9(5) VALUE ZERO.                CL**5
00148      05  WS-DISCOUNT-OPTION  PIC X        VALUE SPACES.              CL**5
00149                                                                   EL6561
00150      05  WS-SAVE-KEY         PIC X(28)    VALUE SPACES.           EL6561
00151      05  WS-SAVE-STRUCTURE   PIC X(7)     VALUE SPACES.           EL6561
00152                                                                   EL6561
00153      05  WS-FIRST-TIME-SW    PIC X        VALUE 'Y'.              EL6561
00154          88  FIRST-TIME                   VALUE 'Y'.              EL6561
00155                                                                   EL6561
00156      05  ELCNTL-KEY.                                              EL6561
00157          10  CNTL-COMP-ID    PIC XXX     VALUE SPACES.               CL**6
00158          10  CNTL-REC-TYPE   PIC X       VALUE SPACES.            EL6561
00159          10  CNTL-ACCESS     PIC X(4)    VALUE SPACES.            EL6561
00160          10  CNTL-SEQ-NO     PIC S9(4)   VALUE +0  COMP.          EL6561
00161                                                                   EL6561
00162      05  MISC-SAVE-AREAS.                                         EL6561
00163          10  WS-MAX-AGE               PIC 99.                     EL6561
00164          10  WS-MORT-CODE             PIC X(4).                   EL6561
00165          10  WS-EXCEPTIONS            OCCURS 8 TIMES.             EL6561
00166              15  WS-LIMIT-AGE         PIC 99.                     EL6561
00167              15  WS-LIMIT-TERM        PIC S999.                      CL**6
00168              15  WS-LIMIT-MO-BEN      PIC S9(5).                  EL6561
00169              15  WS-LIMIT-TO-BEN      PIC S9(7).
012820             15  ws-limit-min-term    pic s999.
00170                                                                   EL6561
00171  EJECT                                                            EL6561
00172                            COPY ELCSCTM.                             CL*10
00173  EJECT                                                            EL6561
00174                            COPY ELCSCRTY.                            CL*10
00175      EJECT                                                        EL6561
00176                                    COPY ELCDATE.                     CL*10
00177      EJECT                                                        EL6561
00178                                    COPY ELCLOGOF.                    CL*10
00179      EJECT                                                        EL6561
00180                                    COPY ELCATTR.                     CL*10
00181      EJECT                                                        EL6561
00182                                    COPY ELCEMIB.                     CL*10
00183      EJECT                                                        EL6561
00184                                    COPY ELCINTF.                     CL*10
00185      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL6561
00186          16  PI-FILE-ID                    PIC XX.                EL6561
00187          16  PI-MAINT                      PIC X.                 EL6561
00188              88  VALID-MAINT-TYPE                VALUE 'S' 'A' 'C'EL6561
00189                                                    'D' 'P' 'E'.   EL6561
00190              88  ADD-FUNCTION                    VALUE 'A'.       EL6561
00191              88  SHOW-FUNCTION                   VALUE 'S'.       EL6561
00192              88  DELETE-FUNCTION                 VALUE 'D'.       EL6561
00193              88  CHANGE-FUNCTION                 VALUE 'C'.       EL6561
00194              88  COPY-FUNCTION                   VALUE 'P'.       EL6561
00195              88  DEVIATE-FUNCTION                VALUE 'E'.       EL6561
00196                                                                   EL6561
00197          16  PI-ERRATE-KEY.                                       EL6561
00198              20  PI-RATE-COMPANY-CD         PIC X.                EL6561
00199              20  PI-RATE-STATE-CODE.                              EL6561
00200                  24  PI-RATE-CODE           PIC XX.               EL6561
00201                  24  PI-RATE-CLASS          PIC XX.               EL6561
00202                  24  PI-RATE-DEV            PIC XXX.                 CL**6
00203              20  PI-RATE-L-AH-CODE.                               EL6561
00204                  24  PI-RATE-L-AH           PIC X.                EL6561
00205                  24  PI-RATE-LAH-NUM        PIC XX.                  CL**4
00206              20  PI-RATE-LIMITS.                                  EL6561
00207                  24  PI-RATE-HIGH-AGE       PIC 99.               EL6561
00208                  24  PI-RATE-HIGH-AMT       PIC 9(6).             EL6561
00209                  24  PI-RATE-FUTURE         PIC XX.               EL6561
00210                  24  PI-RATE-SEX            PIC X.                EL6561
00211              20  PI-RATE-EXPIRY-DATE        PIC 9(11) COMP-3.        CL*23
00212                                                                   EL6561
00213          16  PI-SAVE-ERRATE-KEY             PIC X(28).            EL6561
00214                                                                   EL6561
00215          16  PI-BROWSE-SW                  PIC X.                 EL6561
00216              88  BROWSE-STARTED                  VALUE 'Y'.       EL6561
00217          16  PI-ERRATE-EOF-SW              PIC X.                 EL6561
00218              88  ERRATE-EOF                      VALUE 'Y'.       EL6561
00219          16  PI-SHOW-SW                    PIC X.                 EL6561
00220              88  SHOWN-ONCE                      VALUE 'Y'.       EL6561
00221          16  PI-RETURN-SW                  PIC X.                 EL6561
00222          16  PI-DISCOUNT-OPTION            PIC X.                    CL*19
00223          16  PI-DISCOUNT-RATE              PIC S99V9(5) COMP-3.      CL*19
00224          16  PI-OB-RATE                    PIC S99V9(5) COMP-3.      CL*19
00225          16  FILLER                        PIC X(568).               CL*27
00226                                                                   EL6561
00227      EJECT                                                        EL6561
00228                              COPY ELCJPFX.                           CL*10
081413                             PIC X(2000).
00230                                                                   EL6561
00231      EJECT                                                        EL6561
00232                              COPY ELCAID.                            CL*10
00233                                                                   EL6561
00234  01  FILLER    REDEFINES DFHAID.                                  EL6561
00235      05  FILLER              PIC X(8).                            EL6561
00236      05  PF-VALUES           PIC X       OCCURS 2.                EL6561
00237                                                                   EL6561
00238      EJECT                                                        EL6561
00239                              COPY EL6561S.                           CL*10
00240                                                                   EL6561
00241  01  FILLER-R    REDEFINES EL6561AI.                              EL6561
00242      05  FILLER             PIC X(195).
022420     05  filler             pic x(33).
00243      05  SCREEN-TABLE       OCCURS 8 TIMES                        EL6561
00244                             INDEXED BY ST-INDX.                   EL6561
00245          10  RATE-TO-AGE-L  PIC S9(4)         COMP.               EL6561
00246          10  RATE-TO-AGE-A  PIC X.                                EL6561
00247          10  RATE-TO-AGE    PIC 99.                               EL6561
00248                                                                   EL6561
00249          10  RATE-TO-TRM-L  PIC S9(4)         COMP.               EL6561
00250          10  RATE-TO-TRM-A  PIC X.                                EL6561
00251          10  RATE-TO-TRM    PIC 999.                                 CL**6
00252                                                                   EL6561
00253          10  RATE-MO-BEN-L  PIC S9(4)         COMP.               EL6561
00254          10  RATE-MO-BEN-A  PIC X.                                EL6561
00255          10  RATE-MO-BEN    PIC 9(6).                             EL6561
00256          10  RATE-MO-BEN1   REDEFINES                             EL6561
00257              RATE-MO-BEN    PIC ZZ,ZZZ.                           EL6561
00258                                                                   EL6561
00259          10  RATE-TO-BEN-L  PIC S9(4)         COMP.               EL6561
00260          10  RATE-TO-BEN-A  PIC X.                                EL6561
00261          10  RATE-TO-BEN    PIC 9(9).                             EL6561
00262          10  RATE-TO-BEN1   REDEFINES                             EL6561
00263              RATE-TO-BEN    PIC Z,ZZZ,ZZZ.                        EL6561

012820         10  RATE-TO-MIN-TRM-L  PIC S9(4)     COMP.
012820         10  RATE-TO-MIN-TRM-A  PIC X.
012820         10  RATE-TO-MIN-TRM    PIC 999.

00264      05  FILLER             PIC X(75).                               CL*16
00265                                                                   EL6561
00266      05  FILLER             PIC X(5).                             EL6561
00267                                                                   EL6561
00268      EJECT                                                        EL6561
00269                                                                   EL6561
00270  LINKAGE SECTION.                                                 EL6561
00271                                                                   EL6561
00272  01  DFHCOMMAREA             PIC X(1024).                         EL6561
00273                                                                   EL6561
00274      EJECT                                                        EL6561
00275                              COPY ERCRATE.                           CL*10
00276      EJECT                                                        EL6561
00277                              COPY ELCCNTL.                           CL*10
00278      EJECT                                                        EL6561
00279  PROCEDURE DIVISION.                                              EL6561
00280                                                                   EL6561
00281      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL6561
00282      MOVE '5'                   TO DC-OPTION-CODE.                EL6561
00283      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6561
00284      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL6561
00285      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL6561
00286      INITIALIZE PI-DISCOUNT-RATE                                     CL*19
00287                 PI-OB-RATE.                                          CL*19
00288                                                                   EL6561
00289      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL6561
00290                                                                   EL6561
00291  0100-START.                                                      EL6561
00292      IF EIBCALEN = 0                                              EL6561
00293          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6561
00294                                                                   EL6561
00295      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6561
00296          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6561
00297              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6561
00298              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6561
00299              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6561
00300              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6561
00301              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6561
00302              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6561
00303              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6561
00304              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL6561
00305          ELSE                                                     EL6561
00306              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6561
00307              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6561
00308              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6561
00309              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6561
00310              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6561
00311              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6561
00312              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6561
00313              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL6561
00314                                                                   EL6561
00315      EXEC CICS HANDLE CONDITION                                   EL6561
00316          NOTOPEN  (9990-ABEND)                                    EL6561
00317          NOTFND   (8880-NOT-FOUND)                                EL6561
00318          PGMIDERR (9600-PGMID-ERROR)                              EL6561
00319          ERROR    (9990-ABEND)                                    EL6561
00320      END-EXEC.                                                    EL6561
00321                                                                   EL6561
00322      IF PI-FILE-ID = 'OE'                                         EL6561
00323         MOVE OE-RATE-FILE-ID TO RATE-FILE-ID.                     EL6561
00324                                                                   EL6561
00325      IF EIBTRNID NOT = TRANS-ID                                   EL6561
00326          MOVE LOW-VALUES TO EL6561AI                              EL6561
00327          GO TO 3000-BUILD-SCREEN-B.                               EL6561
00328                                                                   EL6561
00329      IF EIBAID = DFHCLEAR                                         EL6561
00330          MOVE 'Y' TO PI-RETURN-SW                                 EL6561
00331          GO TO 9400-CLEAR.                                        EL6561
00332                                                                   EL6561
00333      EJECT                                                        EL6561
00334                                                                   EL6561
00335  0200-RECEIVE.                                                    EL6561
00336      MOVE LOW-VALUES TO EL6561AI.                                 EL6561
00337                                                                   EL6561
00338      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6561
00339          MOVE ER-0008 TO EMI-ERROR                                EL6561
00340          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*32
00341          MOVE -1   TO MAXAGEL                                     EL6561
00342          GO TO 8200-SEND-DATAONLY.                                EL6561
00343                                                                   EL6561
00344      EXEC CICS RECEIVE                                            EL6561
00345          MAP    (WS-MAPNAME)                                      EL6561
00346          MAPSET (WS-MAPSET-NAME)                                  EL6561
00347          INTO   (EL6561AI)                                        EL6561
00348      END-EXEC.                                                    EL6561
00349                                                                   EL6561
00350      IF PFENTERL = 0                                              EL6561
00351          GO TO 0300-CHECK-PFKEYS.                                 EL6561
00352                                                                   EL6561
00353      IF EIBAID NOT = DFHENTER                                     EL6561
00354          MOVE ER-0004 TO EMI-ERROR                                EL6561
00355          GO TO 0310-INPUT-ERROR.                                  EL6561
00356                                                                   EL6561
00357      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)   EL6561
00358          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL6561
00359      ELSE                                                         EL6561
00360          MOVE ER-0029 TO EMI-ERROR                                EL6561
00361          GO TO 0310-INPUT-ERROR.                                  EL6561
00362                                                                   EL6561
00363      EJECT                                                        EL6561
00364                                                                   EL6561
00365  0300-CHECK-PFKEYS.                                               EL6561
00366      IF EIBAID = DFHPF23                                          EL6561
00367          GO TO 8810-PF23.                                         EL6561
00368                                                                   EL6561
00369      IF EIBAID = DFHPF24                                          EL6561
00370          GO TO 9200-RETURN-MAIN-MENU.                             EL6561
00371                                                                   EL6561
00372      IF EIBAID = DFHPF12                                          EL6561
00373          GO TO 9500-PF12.                                         EL6561
00374                                                                   EL6561
00375      IF EIBAID = DFHPF1                                           EL6561
00376          GO TO 7100-NEXT-PLAN.                                    EL6561
00377                                                                   EL6561
00378      IF EIBAID = DFHPF2                                           EL6561
00379          GO TO 7200-PRIOR-PLAN.                                   EL6561
00380                                                                   EL6561
00381      IF EIBAID = DFHPF5                                           EL6561
00382          MOVE XCTL-6562 TO PGM-NAME                               EL6561
00383          GO TO 9300-XCTL.                                         EL6561
00384                                                                   EL6561
00385      IF EIBAID = DFHENTER                                         EL6561
00386          GO TO 1000-CHANGE.                                       EL6561
00387                                                                   EL6561
00388      MOVE ER-0029 TO EMI-ERROR.                                   EL6561
00389                                                                   EL6561
00390  0310-INPUT-ERROR.                                                EL6561
00391      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6561
00392                                                                   EL6561
00393      MOVE AL-UNBON TO PFENTERA                                    EL6561
00394      MOVE -1       TO PFENTERL                                    EL6561
00395                                                                   EL6561
00396      GO TO 8200-SEND-DATAONLY.                                    EL6561
00397                                                                   EL6561
00398      EJECT                                                        EL6561
00399                                                                   EL6561
00400  1000-CHANGE.                                                     EL6561
00401      IF NOT MODIFY-CAP                                            EL6561
00402          MOVE 'UPDATE'       TO SM-READ                           EL6561
00403          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL6561
00404          MOVE ER-0070        TO  EMI-ERROR                        EL6561
00405          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6561
00406          GO TO 8100-SEND-INITIAL-MAP.                             EL6561
00407                                                                   EL6561
00408      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                        EL6561
00409          NEXT SENTENCE                                            EL6561
00410      ELSE                                                         EL6561
00411          MOVE ER-2056  TO EMI-ERROR                               EL6561
00412          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6561
00413          MOVE -1    TO MAXAGEL                                    EL6561
00414          GO TO 8200-SEND-DATAONLY.                                EL6561
00415                                                                   EL6561
00416      PERFORM 5000-EDIT-SCREEN-B THRU 5099-EXIT.                   EL6561
00417                                                                   EL6561
00418      IF EMI-NO-ERRORS                                             EL6561
00419          NEXT SENTENCE                                            EL6561
00420      ELSE                                                         EL6561
00421          GO TO 8200-SEND-DATAONLY.                                EL6561
00422                                                                   EL6561
00423      PERFORM 7750-READ-ERRATE-UPDATE THRU 7750-EXIT.              EL6561
00424                                                                   EL6561
00425      MOVE RATE-RECORD TO JP-RECORD-AREA.                          EL6561
00426                                                                   EL6561
00427      PERFORM 4000-UPDATE-SCREEN-B THRU 4099-EXIT.                 EL6561
00428                                                                   EL6561
00429      IF RT-LAST-MAINT-USER   = PI-UPDATE-BY OR                    EL6561
00430         RT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6561
00431          NEXT SENTENCE                                            EL6561
00432      ELSE                                                         EL6561
00433          EXEC CICS UNLOCK                                         EL6561
00434               DATASET  (RATE-FILE-ID)                             EL6561
00435          END-EXEC                                                 EL6561
00436          MOVE ER-0068 TO EMI-ERROR                                EL6561
00437          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6561
00438          GO TO 3000-BUILD-SCREEN-B.                               EL6561
00439                                                                   EL6561
00440      MOVE PI-PROCESSOR-ID     TO RT-LAST-MAINT-USER.              EL6561
00441      MOVE EIBTIME             TO RT-LAST-MAINT-HHMMSS.            EL6561
00442                                                                   EL6561
00443      MOVE SAVE-BIN-DATE       TO RT-LAST-MAINT-DT                 EL6561
00444                                  BIN-CURRENT-SAVE.                EL6561
00445      MOVE 'B'                 TO JP-RECORD-TYPE                   EL6561
00446      MOVE RATE-FILE-ID        TO FILE-ID.                         EL6561
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00448      MOVE RATE-RECORD         TO JP-RECORD-AREA.                  EL6561
00449                                                                   EL6561
00450      EXEC CICS REWRITE                                            EL6561
00451          DATASET  (RATE-FILE-ID)                                  EL6561
00452          FROM     (RATE-RECORD)                                   EL6561
00453      END-EXEC.                                                    EL6561
00454                                                                   EL6561
00455      MOVE 'C'                 TO JP-RECORD-TYPE                   EL6561
00456      MOVE RATE-FILE-ID        TO FILE-ID.                         EL6561
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00458      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6561
00459      MOVE ER-0000 TO EMI-ERROR.                                   EL6561
00460      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6561
00461                                                                   EL6561
00462  1099-EXIT.                                                       EL6561
00463      EXIT.                                                        EL6561
00464      EJECT                                                        EL6561
00465                                                                   EL6561
00466  3000-BUILD-SCREEN-B.                                             EL6561
00467      MOVE LOW-VALUES                TO  EL6561AO.                 EL6561
00468      MOVE ZEROS                     TO  MISC-SAVE-AREAS.          EL6561
00469      MOVE PI-COMPANY-CD             TO  PI-RATE-COMPANY-CD.       EL6561
00470                                                                   EL6561
00471      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.                     EL6561
00472                                                                   EL6561
00473      MOVE PI-ERRATE-KEY             TO PI-SAVE-ERRATE-KEY.        EL6561
00474                                                                   EL6561
00475  3025-SET-UP-SCREEN-B.                                            EL6561

081413     MOVE RT-ST-CODE             TO STATEO
081413     MOVE RT-ST-CLASS            TO CLASSO
081413     MOVE RT-ST-DEV              TO DEVO

00476      MOVE RT-LAH-NUM                TO PLANCODO.                  EL6561
00477      MOVE RT-MAX-AGE                TO MAXAGEO.                   EL6561
00478      MOVE RT-HIGH-AGE               TO RTAGEO.                    EL6561
00479      MOVE RT-HIGH-AMT               TO AMOUNTO.                      CL*15
00480                                                                      CL*24
00481 ***  Y2K PROJ 7744                                                   CL*33
00482      MOVE RT-EXPIRY-DATE            TO WS-HOLD-RATE-EXP-DT           CL*24
00483      MOVE WS-HOLD-RATE-EXP-AL(6:6)  TO EXPIREO                       CL*24
00484 ***  Y2K PROJ 7744                                                   CL*33
00485                                                                      CL**7
00486      IF PI-COMPANY-ID = 'CSL' OR  'LGX'                              CL**7
00487          NEXT SENTENCE                                               CL**7
00488      ELSE                                                            CL**7
00489          MOVE AL-SADOF    TO  DAYRTDA                                CL**7
00490          MOVE AL-SANOF    TO  DAYRTA                                 CL**7
00491          GO TO 3025-SKIP-DAILY-RATE.                                 CL**7
00492                                                                      CL**7
00493      IF RT-DAILY-RATE NUMERIC                                        CL**3
00494         IF RT-DAILY-RATE NOT = +0                                    CL**6
00495            MOVE RT-DAILY-RATE       TO DAYRTO.                       CL**5
00496                                                                      CL**7
00497  3025-SKIP-DAILY-RATE.                                               CL**7
00499      IF RT-DISCOUNT-RATE NUMERIC                                     CL**5
00500          MOVE RT-DISCOUNT-RATE   TO PI-DISCOUNT-RATE                 CL*19
00501          IF RT-DISCOUNT-RATE NOT = +0                                CL*19
00502              MOVE RT-DISCOUNT-RATE                                   CL*19
00503                                  TO DISRATO                          CL*19
00504          END-IF                                                      CL*19
00506      ELSE                                                            CL*19
00507          MOVE ZEROS              TO PI-DISCOUNT-RATE                 CL*19
00508      END-IF.
00508                                                                      CL**5
010716     IF RT-CANCEL-FEE NUMERIC                                        CL**9
010716        IF RT-CANCEL-FEE NOT EQUAL +0                                CL**9
010716           MOVE RT-CANCEL-FEE       TO CANFEEO.                      CL**9
00512                                                                      CL**9
00513      IF RT-COMPOSITE-RATE NUMERIC                                    CL**6
00514         IF RT-COMPOSITE-RATE NOT = +0                                CL**6
00515            MOVE RT-COMPOSITE-RATE   TO COMPRTO.                      CL**6
00516                                                                      CL**6
00517      IF RT-DISCOUNT-OB-RATE NUMERIC                                  CL**5
00518          MOVE RT-DISCOUNT-OB-RATE                                    CL*19
00519                                  TO PI-OB-RATE                       CL*19
00520          IF RT-DISCOUNT-OB-RATE NOT = +0                             CL*19
00521              MOVE RT-DISCOUNT-OB-RATE                                CL*19
00522                                  TO OBRATEO                          CL*19
00523          END-IF                                                      CL*19
00525      ELSE                                                            CL*19
00526          MOVE ZEROS              TO PI-OB-RATE                       CL*19
00527      END-IF.
00527                                                                      CL**5
00528      IF RT-DISCOUNT-OPTION = '1' OR '2' OR '3' OR '4' OR             CL*19
00529                              '5' OR '6' or 'Q'                       CL*19
00530         MOVE RT-DISCOUNT-OPTION     TO DISOPTO                       CL**5
00531                                        PI-DISCOUNT-OPTION            CL*19
00532      ELSE                                                            CL**5
00533         MOVE SPACES                 TO DISOPTO.                      CL**5
00534                                                                   EL6561
00535      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL6561
00536          MOVE -1                    TO MORTL                      EL6561
00537          MOVE PI-LIFE-OVERRIDE-L6   TO LIMITSO                    EL6561
00538          MOVE PI-LIFE-OVERRIDE-L1   TO COVERO                     EL6561
00539          MOVE RT-LIFE-MORT-CODE     TO MORTO                      EL6561
00540          MOVE AL-SADOF              TO MAXDA                      EL6561
00541          MOVE AL-UANON              TO MORTA                      EL6561
00542          MOVE AL-SANOF              TO MORTDA
022420                                       MINHA.
00543                                                                   EL6561
00544      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL6561
00545          MOVE -1                    TO MAXAGEL                    EL6561
00546          MOVE PI-AH-OVERRIDE-L6     TO LIMITSO                    EL6561
00547          MOVE PI-AH-OVERRIDE-L1     TO COVERO                     EL6561
00548          MOVE AL-SANOF              TO MAXDA                      EL6561
00549          MOVE AL-SADOF              TO MORTA                      EL6561
00550                                        MORTDA
022420                                       MINHA.
00551                                                                   EL6561
00552      MOVE +1     TO SUB1.                                         EL6561
00553      SET ST-INDX TO +1.                                           EL6561
00554                                                                   EL6561
00555  3050-SET-UP-LIMITS.                                              EL6561
00556                                                                      CL*11
022420     if pi-rate-l-ah = pi-life-override-l1
012820        if rt-l-min-term(sub1) not numeric
012820           move zeros to rt-l-min-term(sub1)
012820        end-if
022420     end-if

00557      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL6561
00558         IF RT-AH-AGE (SUB1) = ZEROS                                  CL*10
022420           move al-sadof to rate-to-min-trm-a(st-indx)
00560          ELSE                                                     EL6561
00561              MOVE RT-AH-AGE   (SUB1)  TO  RATE-TO-AGE   (ST-INDX) EL6561
00562              MOVE RT-AH-TERM  (SUB1)  TO  RATE-TO-TRM   (ST-INDX) EL6561
00563              MOVE RT-AH-BEN-M (SUB1)  TO  RATE-MO-BEN1  (ST-INDX) EL6561
00564              MOVE RT-AH-BEN-F (SUB1)  TO  RATE-TO-BEN1  (ST-INDX)
022420             move al-sadof to rate-to-min-trm-a(st-indx)
00565              MOVE AL-UNNON            TO  RATE-TO-AGE-A (ST-INDX) EL6561
00566                                           RATE-TO-TRM-A (ST-INDX) EL6561
00567                                           RATE-TO-BEN-A (ST-INDX) EL6561
00568                                           RATE-MO-BEN-A (ST-INDX).
00569                                                                      CL*11
00570      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL6561
00571          IF RT-L-EX-AGE (SUB1) = ZEROS                            EL6561
00572              MOVE AL-SADOF            TO  RATE-MO-BEN-A (ST-INDX) EL6561
00573          ELSE                                                     EL6561
00574              MOVE RT-L-EX-AGE  (SUB1) TO  RATE-TO-AGE   (ST-INDX) EL6561
00575              MOVE RT-L-EX-TERM (SUB1) TO  RATE-TO-TRM   (ST-INDX) EL6561
00576              MOVE RT-L-EX-FACE (SUB1) TO  RATE-TO-BEN1  (ST-INDX)
012820             move rt-l-min-term(sub1) to  rate-to-min-trm(st-indx)
00577              MOVE AL-SADOF            TO  RATE-MO-BEN-A (ST-INDX)
00578              MOVE AL-UNNON            TO  RATE-TO-AGE-A (ST-INDX) EL6561
00579                                           RATE-TO-TRM-A (ST-INDX) EL6561
00580                                           RATE-TO-BEN-A (ST-INDX)
012820                                      rate-to-min-trm-a(st-indx).
00581                                                                   EL6561
00582      ADD +1 TO SUB1.                                              EL6561
00583      SET ST-INDX UP BY +1.                                        EL6561
00584                                                                   EL6561
00585      IF SUB1 GREATER +8                                           EL6561
00586          MOVE +1     TO SUB1                                      EL6561
00587          SET ST-INDX TO +1                                        EL6561
00588      ELSE                                                         EL6561
00589          GO TO 3050-SET-UP-LIMITS.                                EL6561
00590                                                                   EL6561
00591      MOVE AL-UNNON                      TO MAXAGEA.               EL6561
00592      GO TO 8100-SEND-INITIAL-MAP.                                 EL6561
00593                                                                   EL6561
00594  3099-EXIT.                                                       EL6561
00595      EXIT.                                                        EL6561
00596      EJECT                                                        EL6561
00597                                                                   EL6561
00598  4000-UPDATE-SCREEN-B.                                            EL6561
00599                                                                      CL**9
010716     IF RT-CANCEL-FEE NOT NUMERIC                                    CL**9
010716        MOVE +0                 TO RT-CANCEL-FEE.                    CL**9
00602                                                                      CL**9
00603      IF MAXAGEL GREATER ZERO                                      EL6561
00604          MOVE WS-MAX-AGE        TO RT-MAX-AGE.                    EL6561
00605                                                                   EL6561
00606      IF MORTL GREATER ZERO                                        EL6561
00607          MOVE WS-MORT-CODE      TO RT-LIFE-MORT-CODE.             EL6561
00608                                                                      CL**3
00609      IF DAYRTL GREATER ZERO                                          CL**3
00610         MOVE WS-DAILY-RATE      TO RT-DAILY-RATE.                    CL**5
00611                                                                      CL**5
00612      IF DISRATL GREATER +0                                           CL**5
00613         MOVE WS-DISCOUNT-RATE   TO RT-DISCOUNT-RATE.                 CL**5
00614                                                                      CL**9
010716     IF CANFEEL GREATER +0                                           CL**9
010716        MOVE WS-CANCEL-FEE      TO RT-CANCEL-FEE.                    CL**9
00617                                                                      CL**6
00618      IF COMPRTL GREATER +0                                           CL**6
00619         MOVE WS-COMPOSITE-RATE  TO RT-COMPOSITE-RATE.                CL**6
00620                                                                      CL**6
00621      IF RT-COMPOSITE-RATE NOT NUMERIC                                CL**6
00622          MOVE ZERO              TO RT-COMPOSITE-RATE.                CL**6
00623      IF RT-COMPOSITE-RATE GREATER +0                                 CL**6
00624         MOVE '1'                TO RT-COMPOSITE-OPTION               CL**6
00625        ELSE                                                          CL**6
00626         MOVE ' '                TO RT-COMPOSITE-OPTION.              CL**6
00627                                                                      CL**5
00628      IF OBRATEL GREATER +0                                           CL**5
00629         MOVE WS-DISCOUNT-OB-RATE                                     CL**5
00630                                 TO RT-DISCOUNT-OB-RATE.              CL**5
00631                                                                      CL**5
00632      IF DISOPTL GREATER +0                                           CL**5
00633         MOVE WS-DISCOUNT-OPTION TO RT-DISCOUNT-OPTION.               CL**5
00634                                                                      CL**5
00635                                                                   EL6561
00636      MOVE +1     TO SUB1                                          EL6561
00637                     SUB2.                                         EL6561
00638                                                                   EL6561
00639  4025-UPDATE-LIMITS.                                              EL6561
00640                                                                      CL*10
00641      IF SUB1 GREATER +8                                           EL6561
00642          GO TO 4050-ZERO-RECORD.                                  EL6561
00643                                                                   EL6561
00644      IF WS-LIMIT-AGE (SUB1) = ZERO                                EL6561
00645         IF PI-COMPANY-ID NOT EQUAL 'CRI'                             CL*10
00646          ADD +1 TO SUB1                                           EL6561
00647          GO TO 4025-UPDATE-LIMITS.                                EL6561
00648                                                                   EL6561
00649      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL6561
00650          MOVE WS-LIMIT-AGE    (SUB1) TO RT-AH-AGE   (SUB2)        EL6561
00651          MOVE WS-LIMIT-TERM   (SUB1) TO RT-AH-TERM  (SUB2)        EL6561
00652          MOVE WS-LIMIT-MO-BEN (SUB1) TO RT-AH-BEN-M (SUB2)        EL6561
00653          MOVE WS-LIMIT-TO-BEN (SUB1) TO RT-AH-BEN-F (SUB2)
012820         move zeros to rt-l-min-term(sub2).
00654                                                                   EL6561
00655      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL6561
00656          MOVE WS-LIMIT-AGE    (SUB1) TO RT-L-EX-AGE  (SUB2)       EL6561
00657          MOVE WS-LIMIT-TERM   (SUB1) TO RT-L-EX-TERM (SUB2)       EL6561
00658          MOVE WS-LIMIT-TO-BEN (SUB1) TO RT-L-EX-FACE (SUB2)
012820         move ws-limit-min-term(sub1) to rt-l-min-term(sub2).
00659                                                                   EL6561
00660      ADD +1 TO SUB1                                               EL6561
00661                SUB2.                                              EL6561
00662                                                                   EL6561
00663      GO TO 4025-UPDATE-LIMITS.                                    EL6561
00664                                                                   EL6561
00665  4050-ZERO-RECORD.                                                EL6561
00666      IF SUB2 GREATER +8                                           EL6561
00667          MOVE +1 TO SUB1                                          EL6561
00668                     SUB2                                          EL6561
00669          GO TO 4099-EXIT.                                         EL6561
00670                                                                   EL6561
00671      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL6561
00672          MOVE ZEROS     TO RT-AH-AGE   (SUB2)                     EL6561
00673                            RT-AH-TERM  (SUB2)                     EL6561
00674                            RT-AH-BEN-M (SUB2)                     EL6561
00675                            RT-AH-BEN-F (SUB2)                     EL6561
00676      ELSE                                                         EL6561
00677          IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                    EL6561
00678              MOVE ZEROS TO RT-L-EX-AGE  (SUB2)                    EL6561
00679                            RT-L-EX-TERM (SUB2)                    EL6561
00680                            RT-L-EX-FACE (SUB2).                   EL6561
00681                                                                   EL6561
00682      ADD +1 TO SUB2.                                              EL6561
00683      GO TO 4050-ZERO-RECORD.                                      EL6561
00684                                                                   EL6561
00685  4099-EXIT.                                                       EL6561
00686      EXIT.                                                        EL6561
00687      EJECT                                                        EL6561
00688                                                                   EL6561
00689  5000-EDIT-SCREEN-B.                                              EL6561
00690      IF PLANCODL GREATER ZERO                                     EL6561
00691          IF PLANCODI NUMERIC                                      EL6561
00692              MOVE PLANCODI TO PI-RATE-LAH-NUM                     EL6561
00693          ELSE                                                     EL6561
00694              MOVE -1       TO PLANCODL                            EL6561
00695              MOVE AL-UNBON TO PLANCODA                            EL6561
00696              MOVE ER-2184  TO EMI-ERROR                           EL6561
00697              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6561
00698                                                                   EL6561
00699      IF COVERL GREATER ZERO                                       EL6561
00700          IF COVERI = PI-LIFE-OVERRIDE-L1 OR                       EL6561
00701                      PI-AH-OVERRIDE-L1                            EL6561
00702              MOVE COVERI   TO PI-RATE-L-AH                        EL6561
00703          ELSE                                                     EL6561
00704              MOVE -1       TO COVERL                              EL6561
00705              MOVE AL-UNBON TO COVERA                              EL6561
00706              MOVE ER-2185  TO EMI-ERROR                           EL6561
00707              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6561
00708                                                                   EL6561
00709 ***  Y2K PROJ 7744                                                   CL*33
00710      IF EXPIREL GREATER ZERO                                      EL6561
00711          IF EXPIREI NUMERIC                                       EL6561
00712              MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT                    CL*36
00713              MOVE EXPIREI  TO WS-HOLD-RATE-EXP-AL(6:6)               CL*28
00714                               DC-GREG-DATE-1-YMD                     CL*25
00715              MOVE '3'      TO DC-OPTION-CODE                         CL*25
00716              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*25
00717              IF NO-CONVERSION-ERROR                                  CL*25
00718                  MOVE DC-ALPHA-CENTURY TO                            CL*35
00719                       WS-HOLD-RATE-EXP-AL(4:2)                       CL*25
00720                  MOVE WS-HOLD-RATE-EXP-DT TO                         CL*25
00721                                           PI-RATE-EXPIRY-DATE        CL*25
00722              ELSE                                                    CL*25
00723                  IF WS-HOLD-RATE-EXP-DT(6:6) = 999999                CL*35
00724                      MOVE WS-HOLD-RATE-EXP-DT TO                     CL*35
00725                                           PI-RATE-EXPIRY-DATE        CL*35
00726                  ELSE                                                CL*35
00727                      MOVE -1       TO EXPIREL                        CL*35
00728                      MOVE AL-UNBON TO EXPIREA                        CL*35
00729                      MOVE ER-2296  TO EMI-ERROR                      CL*35
00730                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL*35
00731                  END-IF                                              CL*35
00732              END-IF                                                  CL*35
00733          ELSE                                                     EL6561
00734              MOVE -1       TO EXPIREL                             EL6561
00735              MOVE AL-UNBON TO EXPIREA                             EL6561
00736              MOVE ER-2296  TO EMI-ERROR                              CL*30
00737              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*25
00738          END-IF                                                      CL*25
00739      END-IF.                                                         CL*25
00740 ***  Y2K PROJ 7744                                                   CL*33
00741                                                                   EL6561
00742      IF RTAGEL GREATER ZERO                                       EL6561
00743          IF RTAGEI NUMERIC                                        EL6561
00744              MOVE RTAGEI TO PI-RATE-HIGH-AGE                      EL6561
00745          ELSE                                                     EL6561
00746              MOVE -1       TO RTAGEL                              EL6561
00747              MOVE AL-UNBON TO RTAGEA                              EL6561
00748              MOVE ER-2187  TO EMI-ERROR                           EL6561
00749              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6561
00750                                                                   EL6561
00751      IF PI-ERRATE-KEY NOT = PI-SAVE-ERRATE-KEY                    EL6561
00752          GO TO 7100-NEXT-PLAN.                                    EL6561
00753                                                                   EL6561
00754      MOVE ZEROS  TO MISC-SAVE-AREAS.                              EL6561
00755      MOVE SPACES TO WS-MORT-CODE.                                 EL6561
00756                                                                   EL6561
00757      IF MORTL GREATER ZERO                                        EL6561
00758          PERFORM 7400-EDIT-MORTALITY THRU 7499-EXIT.              EL6561
00759                                                                   EL6561
00760      IF MAXAGEL GREATER ZERO                                      EL6561
00761          IF MAXAGEI NUMERIC                                       EL6561
00762              MOVE AL-UNNON TO MAXAGEA                             EL6561
00763              MOVE MAXAGEI  TO WS-MAX-AGE                          EL6561
00764          ELSE                                                     EL6561
00765              MOVE -1 TO MAXAGEL                                   EL6561
00766              MOVE AL-UNBON TO MAXAGEA                             EL6561
00767              MOVE ER-2273  TO EMI-ERROR                           EL6561
00768              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**3
00769                                                                      CL**3
00770      IF PI-COMPANY-ID  = 'CSL'  OR  'LGX'                            CL**7
00771          IF DAYRTL GREATER ZERO                                      CL**7
00772              MOVE DAYRTI      TO  DEEDIT-FIELD                       CL**9
00773              PERFORM 7500-DEEDIT                                     CL**7
00774              IF DEEDIT-FIELD-V5 NUMERIC                              CL**7
00775                  MOVE DEEDIT-FIELD-V5 TO WS-DAILY-RATE               CL**7
00776                                            DAYRTO                    CL**7
00777                  MOVE AL-UNNON TO DAYRTA                             CL**7
00778              ELSE                                                    CL**7
00779                  MOVE -1 TO DAYRTL                                   CL**7
00780                  MOVE AL-UNBON TO DAYRTA                             CL**7
00781                  MOVE ER-2280 TO EMI-ERROR                           CL**7
00782                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL**7
00783                                                                      CL**5
00784      IF DISRATL GREATER ZERO                                         CL**5
00785         MOVE DISRATI         TO  DEEDIT-FIELD                        CL**9
00786         PERFORM 7500-DEEDIT                                          CL**5
00787         IF DEEDIT-FIELD-V5 NUMERIC                                   CL**5
00788            MOVE DEEDIT-FIELD-V5  TO  WS-DISCOUNT-RATE                CL**5
00789                                      DISRATO                         CL**5
00790                                      PI-DISCOUNT-RATE                CL*19
00791            MOVE AL-UNNON TO DISRATA                                  CL**5
00792         ELSE                                                         CL**5
00793            MOVE -1 TO DISRATL                                        CL**5
00794            MOVE AL-UNBON TO DISRATA                                  CL**5
00795            MOVE ER-2280  TO EMI-ERROR                                CL**5
00796            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**5
00797                                                                      CL**5
010716     IF CANFEEL GREATER +0                                           CL**9
010716        MOVE CANFEEI         TO  DEEDIT-FIELD                        CL**9
010716        PERFORM 7500-DEEDIT                                          CL**9
010716        IF DEEDIT-FIELD-V2 NUMERIC                                   CL**9
010716           MOVE DEEDIT-FIELD-V2  TO  WS-CANCEL-FEE                   CL**9
010716                                     CANFEEO                         CL**9
010716           MOVE AL-UNNON TO CANFEEA                                  CL**9
010716        ELSE                                                         CL**9
010716           MOVE -1 TO CANFEEL                                        CL**9
010716           MOVE AL-UNBON TO CANFEEA                                  CL**9
010716           MOVE ER-9407  TO EMI-ERROR                                CL**9
010716           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**9
00810                                                                      CL**9
00811      IF COMPRTL GREATER ZERO                                         CL**6
00812         MOVE COMPRTI         TO  DEEDIT-FIELD                        CL**9
00813         PERFORM 7500-DEEDIT                                          CL**6
00814         IF DEEDIT-FIELD-V5 NUMERIC                                   CL**6
00815            MOVE DEEDIT-FIELD-V5  TO  WS-COMPOSITE-RATE               CL**6
00816                                      COMPRTO                         CL**6
00817            MOVE AL-UNNON TO COMPRTA                                  CL**6
00818         ELSE                                                         CL**6
00819            MOVE -1 TO COMPRTL                                        CL**6
00820            MOVE AL-UNBON TO COMPRTA                                  CL**6
00821            MOVE ER-2280  TO EMI-ERROR                                CL**6
00822            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**6
00823                                                                      CL**6
00824      IF OBRATEL GREATER ZERO                                         CL**5
00825         MOVE OBRATEI         TO  DEEDIT-FIELD                        CL**9
00826         PERFORM 7500-DEEDIT                                          CL**5
00827         IF DEEDIT-FIELD-V5 NUMERIC                                   CL**5
00828            MOVE DEEDIT-FIELD-V5  TO  WS-DISCOUNT-OB-RATE             CL**5
00829                                      OBRATEO                         CL**5
00830                                      PI-OB-RATE                      CL*19
00831            MOVE AL-UNNON TO OBRATEA                                  CL**5
00832         ELSE                                                         CL**5
00833            MOVE -1 TO OBRATEL                                        CL**5
00834            MOVE AL-UNBON TO OBRATEA                                  CL**5
00835            MOVE ER-2280  TO EMI-ERROR                                CL**5
00836            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**5
00837                                                                      CL**5
00838      IF DISOPTL GREATER ZERO                                         CL**5
00839          NEXT SENTENCE                                               CL*20
00840      ELSE                                                            CL*20
00841          GO TO 5010-OPTION-EDITS-DONE.                               CL*20
00842                                                                      CL*20
00843      IF DISOPTI = ' ' OR '1' OR '2' OR '3' OR '4' OR                 CL*20
00844                   '5' OR '6' or 'Q'
00845          MOVE DISOPTI            TO WS-DISCOUNT-OPTION               CL*20
00846                                     PI-DISCOUNT-OPTION               CL*20
00847          MOVE AL-UANON           TO DISOPTA                          CL*20
00848      ELSE                                                            CL*20
00849          MOVE -1                 TO DISOPTL                          CL*20
00850          MOVE AL-UABON           TO DISOPTA                          CL*20
00851          MOVE ER-2546            TO EMI-ERROR                        CL*20
00852          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*20
00853          GO TO 5010-OPTION-EDITS-DONE.                               CL*20
00854                                                                      CL*19
00855      IF PI-DISCOUNT-OPTION = '6'                                     CL*19
00856          IF PI-OB-RATE NOT = ZEROS                                   CL*19
00857              MOVE -1             TO OBRATEL                          CL*19
00858              MOVE AL-UABON       TO OBRATEA                          CL*19
00859              MOVE ER-2873        TO EMI-ERROR                        CL*19
00860              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*19
00861                                                                      CL*20
00862  5010-OPTION-EDITS-DONE.                                             CL*20
00863                                                                   EL6561
00864      MOVE +1 TO SUB1.                                             EL6561
00865      SET ST-INDX TO +1.                                           EL6561
00866                                                                   EL6561
00867  5025-EDIT-LIMITS.                                                EL6561
00868      IF ST-INDX GREATER +8                                        EL6561
00869          MOVE +1 TO SUB1                                          EL6561
00870          SET ST-INDX TO +1                                        EL6561
00871          GO TO 5099-EXIT.                                         EL6561
00872                                                                   EL6561
00873      IF RATE-TO-AGE-L (ST-INDX) GREATER ZERO                      EL6561
00874          IF RATE-TO-AGE (ST-INDX) NUMERIC                         EL6561
00875              IF RATE-TO-AGE (ST-INDX) LESS WS-PREV-AGE            EL6561
00876                  IF RATE-TO-AGE (ST-INDX) = ZERO                  EL6561
00877                      MOVE AL-UNNON TO RATE-TO-AGE-A (ST-INDX)     EL6561
00878                      MOVE ZEROS    TO WS-LIMIT-AGE (SUB1)         EL6561
00879                  ELSE                                             EL6561
00880                      MOVE -1       TO RATE-TO-AGE-L (ST-INDX)     EL6561
00881                      MOVE AL-UNBON TO RATE-TO-AGE-A (ST-INDX)     EL6561
00882                      MOVE ER-2274  TO EMI-ERROR                   EL6561
00883                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL6561
00884              ELSE                                                 EL6561
00885                  MOVE AL-UNNON TO RATE-TO-AGE-A (ST-INDX)         EL6561
00886                  MOVE RATE-TO-AGE (ST-INDX) TO WS-LIMIT-AGE (SUB1)EL6561
00887                                                WS-PREV-AGE        EL6561
00888          ELSE                                                     EL6561
00889              MOVE -1       TO RATE-TO-AGE-L (ST-INDX)             EL6561
00890              MOVE AL-UNBON TO RATE-TO-AGE-A (ST-INDX)             EL6561
00891              MOVE ER-2275  TO EMI-ERROR                           EL6561
00892              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6561
00893                                                                   EL6561
00894      IF RATE-TO-TRM-L (ST-INDX) GREATER ZERO                      EL6561
00895          IF RATE-TO-TRM (ST-INDX) NUMERIC                         EL6561
00896              MOVE AL-UNNON TO RATE-TO-TRM-A (ST-INDX)             EL6561
00897              MOVE RATE-TO-TRM (ST-INDX) TO WS-LIMIT-TERM (SUB1)   EL6561
00898          ELSE                                                     EL6561
00899              MOVE -1       TO RATE-TO-TRM-L (ST-INDX)             EL6561
00900              MOVE AL-UNBON TO RATE-TO-TRM-A (ST-INDX)             EL6561
00901              MOVE ER-2276  TO EMI-ERROR                           EL6561
00902              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6561
00903      ELSE                                                         EL6561
00904          IF RATE-TO-AGE-L (ST-INDX) GREATER ZERO                  EL6561
00905              MOVE -1       TO RATE-TO-TRM-L (ST-INDX)             EL6561
00906              MOVE AL-UNBON TO RATE-TO-TRM-A (ST-INDX)             EL6561
00907              MOVE ER-2276  TO EMI-ERROR                           EL6561
00908              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6561
00909                                                                   EL6561
012820     IF RATE-TO-min-TRM-L (ST-INDX) > ZERO
012820        IF RATE-TO-min-TRM (ST-INDX) NUMERIC
012820           MOVE AL-UNNON         TO RATE-TO-min-TRM-A (ST-INDX)
012820           MOVE RATE-TO-min-TRM (ST-INDX)
012820                                 TO WS-LIMIT-min-TERM (SUB1)
012820        ELSE
012820           MOVE -1       TO RATE-TO-min-TRM-L (ST-INDX)
012820           MOVE AL-UNBON TO RATE-TO-min-TRM-A (ST-INDX)
012820           MOVE ER-2276  TO EMI-ERROR
012820           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
012820     ELSE
012820        IF RATE-TO-AGE-L (ST-INDX) GREATER ZERO
022420           IF PI-RATE-L-AH = PI-life-OVERRIDE-L1
012820              MOVE -1       TO RATE-TO-min-TRM-L (ST-INDX)
012820              MOVE AL-UNBON TO RATE-TO-min-TRM-A (ST-INDX)
012820              MOVE ER-2276  TO EMI-ERROR
012820              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 end-if
012820        end-if
012820     end-if

00910      IF RATE-MO-BEN-L (ST-INDX) GREATER ZERO                      EL6561
00911          MOVE RATE-MO-BEN (ST-INDX) TO DEEDIT-FIELD               EL6561
00912          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL6561
00913          IF DEEDIT-FIELD-V0 NUMERIC                               EL6561
00914              MOVE AL-UNNON TO RATE-MO-BEN-A (ST-INDX)             EL6561
00915              MOVE DEEDIT-FIELD-V0 TO WS-LIMIT-MO-BEN (SUB1)       EL6561
00916                                      RATE-MO-BEN1 (ST-INDX)       EL6561
00917          ELSE                                                     EL6561
00918              MOVE -1       TO RATE-MO-BEN-L (ST-INDX)             EL6561
00919              MOVE AL-UNBON TO RATE-MO-BEN-A (ST-INDX)             EL6561
00920              MOVE ER-2277  TO EMI-ERROR                           EL6561
00921              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6561
00922      ELSE                                                         EL6561
00923          IF RATE-TO-AGE-L (ST-INDX) GREATER ZERO                  EL6561
00924              IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                  EL6561
00925                  MOVE -1       TO RATE-MO-BEN-L (ST-INDX)         EL6561
00926                  MOVE AL-UNBON TO RATE-MO-BEN-A (ST-INDX)         EL6561
00927                  MOVE ER-2277  TO EMI-ERROR                       EL6561
00928                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL6561
00929                                                                   EL6561
00930      IF RATE-TO-BEN-L (ST-INDX) GREATER ZERO                      EL6561
00931          MOVE RATE-TO-BEN (ST-INDX) TO DEEDIT-FIELD               EL6561
00932          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL6561
00933          IF DEEDIT-FIELD-V0 NUMERIC                               EL6561
00934              MOVE AL-UNNON TO RATE-TO-BEN-A (ST-INDX)             EL6561
00935              MOVE DEEDIT-FIELD-V0 TO WS-LIMIT-TO-BEN (SUB1)       EL6561
00936                                      RATE-TO-BEN1 (ST-INDX)       EL6561
00937          ELSE                                                     EL6561
00938              MOVE -1       TO RATE-TO-BEN-L (ST-INDX)             EL6561
00939              MOVE AL-UNBON TO RATE-TO-BEN-A (ST-INDX)             EL6561
00940              MOVE ER-2278  TO EMI-ERROR                           EL6561
00941              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6561
00942      ELSE                                                         EL6561
00943          IF RATE-TO-AGE-L (ST-INDX) GREATER ZERO                  EL6561
00944              MOVE -1       TO RATE-TO-BEN-L (ST-INDX)             EL6561
00945              MOVE AL-UNBON TO RATE-TO-BEN-A (ST-INDX)             EL6561
00946              MOVE ER-2278  TO EMI-ERROR                           EL6561
00947              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6561
00948                                                                   EL6561
00949      ADD +1 TO SUB1.                                              EL6561
00950      SET ST-INDX UP BY +1.                                        EL6561
00951                                                                   EL6561
00952      IF ST-INDX GREATER +8                                        EL6561
00953          MOVE +1 TO SUB1                                          EL6561
00954          SET ST-INDX TO +1                                        EL6561
00955      ELSE                                                         EL6561
00956          GO TO 5025-EDIT-LIMITS.                                  EL6561
00957                                                                   EL6561
00958  5099-EXIT.                                                       EL6561
00959      EXIT.                                                        EL6561
00960      EJECT                                                        EL6561
00961  7100-NEXT-PLAN.                                                  EL6561
00962      MOVE SPACES             TO PI-ERRATE-EOF-SW.                 EL6561
00963      MOVE PI-ERRATE-KEY      TO PI-SAVE-ERRATE-KEY.               EL6561
00964      MOVE PI-RATE-STATE-CODE TO WS-SAVE-STRUCTURE.                EL6561
00965                                                                   EL6561
00966      MOVE PI-COMPANY-CD      TO PI-RATE-COMPANY-CD.               EL6561
00967                                                                   EL6561
00968      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL6561
00969                                                                   EL6561
00970      EXEC CICS HANDLE CONDITION                                   EL6561
00971          ENDFILE  (7150-ENDFILE)                                  EL6561
00972          NOTFND   (7175-NOTFOUND)                                 EL6561
00973      END-EXEC.                                                    EL6561
00974                                                                   EL6561
00975  7125-READ-NEXT.                                                  EL6561
00976      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL6561
00977                                                                   EL6561
00978      IF ERRATE-EOF                                                EL6561
00979          GO TO 7150-ENDFILE.                                      EL6561
00980                                                                   EL6561
00981      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                        EL6561
00982          GO TO 7125-READ-NEXT.                                    EL6561
00983                                                                   EL6561
00984      IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE                    EL6561
00985          NEXT SENTENCE                                            EL6561
00986      ELSE                                                         EL6561
00987          GO TO 7150-ENDFILE.                                      EL6561
00988                                                                   EL6561
00989      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL6561
00990      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL6561
00991      MOVE PI-ERRATE-KEY          TO PI-SAVE-ERRATE-KEY.           EL6561
00992      MOVE LOW-VALUES             TO EL6561AO.                     EL6561
00993                                                                   EL6561
00994      GO TO 3025-SET-UP-SCREEN-B.                                  EL6561
00995                                                                   EL6561
00996  7150-ENDFILE.                                                    EL6561
00997      IF BROWSE-STARTED                                            EL6561
00998          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL6561
00999                                                                   EL6561
01000      MOVE PI-SAVE-ERRATE-KEY     TO PI-ERRATE-KEY.                EL6561
01001      MOVE LOW-VALUES             TO PI-RATE-L-AH-CODE.               CL*26
01002 ***  Y2K PROJ 7744                                                   CL*33
01003      MOVE ZEROS                  TO PI-RATE-EXPIRY-DATE.             CL*26
01004 ***  Y2K PROJ 7744                                                   CL*33
01005      MOVE ER-2285                TO EMI-ERROR.                    EL6561
01006      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6561
01007      GO TO 7100-NEXT-PLAN.                                        EL6561
01008                                                                   EL6561
01009  7175-NOTFOUND.                                                   EL6561
01010      IF BROWSE-STARTED                                            EL6561
01011          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL6561
01012                                                                   EL6561
01013      GO TO 8880-NOT-FOUND.                                        EL6561
01014                                                                   EL6561
01015  7199-EXIT.                                                       EL6561
01016      EXIT.                                                        EL6561
01017      EJECT                                                        EL6561
01018                                                                   EL6561
01019  7200-PRIOR-PLAN.                                                 EL6561
01020      MOVE SPACES             TO  PI-ERRATE-EOF-SW.                EL6561
01021                                                                   EL6561
01022      MOVE PI-COMPANY-CD      TO  PI-RATE-COMPANY-CD.              EL6561
01023                                                                   EL6561
01024      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL6561
01025                                                                   EL6561
01026      EXEC CICS HANDLE CONDITION                                   EL6561
01027          ENDFILE  (7150-ENDFILE)                                  EL6561
01028          NOTFND   (7275-NOTFOUND)                                 EL6561
01029      END-EXEC.                                                    EL6561
01030                                                                   EL6561
01031      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL6561
01032                                                                   EL6561
01033      IF ERRATE-EOF                                                EL6561
01034          GO TO 7150-ENDFILE.                                      EL6561
01035                                                                   EL6561
01036      MOVE PI-ERRATE-KEY      TO PI-SAVE-ERRATE-KEY.               EL6561
01037      MOVE PI-RATE-STATE-CODE TO WS-SAVE-STRUCTURE.                EL6561
01038                                                                   EL6561
01039  7225-READ-PREV.                                                  EL6561
01040      PERFORM 7900-READPREV THRU 7900-EXIT.                        EL6561
01041                                                                   EL6561
01042      IF ERRATE-EOF                                                EL6561
01043          GO TO 7150-ENDFILE.                                      EL6561
01044                                                                   EL6561
01045      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY                        EL6561
01046          GO TO 7225-READ-PREV.                                    EL6561
01047                                                                   EL6561
01048      IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE                    EL6561
01049          NEXT SENTENCE                                            EL6561
01050      ELSE                                                         EL6561
01051          GO TO 7150-ENDFILE.                                      EL6561
01052                                                                   EL6561
01053      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL6561
01054      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL6561
01055      MOVE PI-ERRATE-KEY          TO PI-SAVE-ERRATE-KEY.           EL6561
01056      MOVE LOW-VALUES             TO EL6561AO.                     EL6561
01057                                                                   EL6561
01058      GO TO 3025-SET-UP-SCREEN-B.                                  EL6561
01059                                                                   EL6561
01060  7275-NOTFOUND.                                                   EL6561
01061      IF BROWSE-STARTED                                            EL6561
01062          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL6561
01063                                                                   EL6561
01064      GO TO 8880-NOT-FOUND.                                        EL6561
01065                                                                   EL6561
01066  7299-EXIT.                                                       EL6561
01067      EXIT.                                                        EL6561
01068      EJECT                                                        EL6561
01069                                                                   EL6561
01070  7400-EDIT-MORTALITY.                                             EL6561
01071      IF  MORTI = SPACES                                              CL**9
01072          GO TO 7499-EXIT.                                         EL6561
01073                                                                   EL6561
01074      IF  MORTI = 'ZERO'                                              CL**9
01075          MOVE 'ZERO'             TO  WS-MORT-CODE                    CL**3
01076          GO TO 7499-EXIT.                                            CL**9
01077                                                                   EL6561
01078      MOVE SPACES                 TO  ELCNTL-KEY.                     CL**9
01079      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                   CL**3
01080      MOVE '7'                    TO  CNTL-REC-TYPE.                  CL**3
01081      MOVE +0                     TO  CNTL-SEQ-NO.                    CL**9
01082                                                                   EL6561
01083      EXEC CICS HANDLE CONDITION                                      CL**3
01084          NOTFND  (7490-NOT-FOUND)                                    CL**3
01085          ENDFILE (7491-END-OF-FILE)                                  CL**3
01086      END-EXEC.                                                       CL**3
01087                                                                      CL**3
01088  7409-READ-NEXT.                                                     CL**9
01089                                                                   EL6561
01090      EXEC CICS READ                                                  CL**9
01091           DATASET  (CNTL-FILE-ID)                                    CL**3
01092           SET      (ADDRESS OF CONTROL-FILE)                         CL*16
01093           RIDFLD   (ELCNTL-KEY)                                      CL**3
01094      END-EXEC.                                                       CL**3
01095                                                                      CL**3
01096      IF  PI-COMPANY-ID  NOT = CF-COMPANY-ID OR                       CL**9
01097          CF-RECORD-TYPE NOT = '7'                                    CL**3
01098          GO TO 7490-NOT-FOUND                                     EL6561
01099                                                                      CL**9
01100      ELSE                                                         EL6561
01101          MOVE +1                 TO SUB2                             CL**3
01102          GO TO 7410-SEARCH-MORTAL-TABLE.                          EL6561
01103                                                                   EL6561
01104  7410-SEARCH-MORTAL-TABLE.                                           CL**9
01105                                                                      CL**9
01106      IF  CF-MORT-TABLE-CODE (SUB2) = MORTI                           CL**9
01107          MOVE MORTI TO WS-MORT-CODE                                  CL**9
01108          GO TO 7499-EXIT.                                            CL**9
01109                                                                      CL**9
01110      IF  CF-MORT-TABLE-CODE (SUB2) = LOW-VALUES                      CL**9
01111              OR                                                      CL**9
01112          CF-MORT-TABLE-CODE (SUB2) GREATER THAN MORTI                CL**9
01113          GO TO 7490-NOT-FOUND.                                       CL**9
01114                                                                      CL**9
01115      ADD +1                      TO SUB2.                            CL**9
01116                                                                      CL**9
01117      IF  SUB2 GREATER +9                                             CL**9
01118          ADD +1                  TO  CNTL-SEQ-NO                     CL**9
01119          GO TO 7409-READ-NEXT                                        CL**9
01120                                                                      CL**9
01121      ELSE                                                            CL**9
01122          GO TO 7410-SEARCH-MORTAL-TABLE.                             CL**9
01123                                                                      CL**9
01124  7490-NOT-FOUND.                                                  EL6561
01125                                                                      CL**3
01126      MOVE -1                     TO MORTL.                           CL**3
01127      MOVE AL-UABON               TO MORTA.                           CL**3
01128      MOVE ER-2279                TO EMI-ERROR                        CL**3
01129      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**6
01130      GO TO 7499-EXIT.                                                CL**3
01131                                                                      CL**3
01132  7491-END-OF-FILE.                                                   CL**3
01133                                                                      CL**3
01134      MOVE -1                     TO MORTL.                           CL**3
01135      MOVE AL-UABON               TO MORTA.                           CL**3
01136      MOVE ER-2237                TO EMI-ERROR                        CL**3
01137      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**6
01138      GO TO 7499-EXIT.                                                CL**3
01139                                                                   EL6561
01140  7499-EXIT.                                                       EL6561
01141      EXIT.                                                        EL6561
01142      EJECT                                                        EL6561
01143                                                                   EL6561
01144  7500-DEEDIT.                                                     EL6561
01145      EXEC CICS BIF                                                EL6561
01146           DEEDIT                                                  EL6561
01147           FIELD  (DEEDIT-FIELD)                                   EL6561
01148           LENGTH (15)                                             EL6561
01149      END-EXEC.                                                    EL6561
01150                                                                   EL6561
01151  7500-EXIT.                                                       EL6561
01152      EXIT.                                                        EL6561
01153      EJECT                                                        EL6561
01154                                                                   EL6561
01155  7600-READ-ERRATE-GTEQ.                                           EL6561
01156      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                  EL6561
01157                                                                   EL6561
01158      EXEC CICS READ                                               EL6561
01159           DATASET  (RATE-FILE-ID)                                 EL6561
01160           SET      (ADDRESS OF RATE-RECORD)                          CL*16
01161           RIDFLD   (PI-ERRATE-KEY)                                EL6561
01162           GTEQ                                                    EL6561
01163      END-EXEC.                                                    EL6561
01164                                                                   EL6561
01165      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL6561
01166      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL6561
01167                                                                   EL6561
01168      MOVE RT-CONTROL-PRIMARY     TO PI-ERRATE-KEY.                EL6561
01169                                                                   EL6561
01170  7600-EXIT.                                                       EL6561
01171      EXIT.                                                        EL6561
01172      EJECT                                                        EL6561
01173                                                                   EL6561
01174  7650-READ-ERRATE.                                                EL6561
01175      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                  EL6561
01176                                                                   EL6561
01177      EXEC CICS READ                                               EL6561
01178           DATASET  (RATE-FILE-ID)                                 EL6561
01179           SET      (ADDRESS OF RATE-RECORD)                          CL*16
01180           RIDFLD   (PI-ERRATE-KEY)                                EL6561
01181      END-EXEC.                                                    EL6561
01182                                                                   EL6561
01183      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL6561
01184      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL6561
01185                                                                   EL6561
01186  7650-EXIT.                                                       EL6561
01187      EXIT.                                                        EL6561
01188      EJECT                                                        EL6561
01189                                                                   EL6561
01190  7700-ERRATE-GETMAIN.                                             EL6561
01191      EXEC CICS GETMAIN                                            EL6561
01192           SET     (ADDRESS OF RATE-RECORD)                           CL*16
01193           LENGTH  (ERRATE-LENGTH)                                 EL6561
01194           INITIMG (GETMAIN-SPACE)                                 EL6561
01195           END-EXEC.                                               EL6561
01196                                                                   EL6561
01197  7700-EXIT.                                                       EL6561
01198      EXIT.                                                        EL6561
01199      EJECT                                                        EL6561
01200                                                                   EL6561
01201  7750-READ-ERRATE-UPDATE.                                         EL6561
01202      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                  EL6561
01203                                                                   EL6561
01204      EXEC CICS READ                                               EL6561
01205           DATASET  (RATE-FILE-ID)                                 EL6561
01206           SET      (ADDRESS OF RATE-RECORD)                          CL*16
01207           RIDFLD   (PI-ERRATE-KEY)                                EL6561
01208           UPDATE                                                  EL6561
01209      END-EXEC.                                                    EL6561
01210                                                                   EL6561
01211  7750-EXIT.                                                       EL6561
01212      EXIT.                                                        EL6561
01213      EJECT                                                        EL6561
01214                                                                   EL6561
01215  7800-START-BROWSE.                                               EL6561
01216      EXEC CICS STARTBR                                            EL6561
01217           DATASET  (RATE-FILE-ID)                                 EL6561
01218           RIDFLD   (PI-ERRATE-KEY)                                EL6561
01219      END-EXEC.                                                    EL6561
01220                                                                   EL6561
01221      MOVE 'Y' TO PI-BROWSE-SW.                                    EL6561
01222                                                                   EL6561
01223  7800-EXIT.                                                       EL6561
01224      EXIT.                                                        EL6561
01225      EJECT                                                        EL6561
01226                                                                   EL6561
01227  7850-READNEXT.                                                   EL6561
01228      EXEC CICS READNEXT                                           EL6561
01229           DATASET  (RATE-FILE-ID)                                 EL6561
01230           SET      (ADDRESS OF RATE-RECORD)                          CL*16
01231           RIDFLD   (PI-ERRATE-KEY)                                EL6561
01232      END-EXEC.                                                    EL6561
01233                                                                   EL6561
01234      IF PI-COMPANY-CD NOT = RT-COMPANY-CD                         EL6561
01235          MOVE LOW-VALUES TO EL6561AO                              EL6561
01236          MOVE 'Y'        TO PI-ERRATE-EOF-SW.                     EL6561
01237                                                                   EL6561
01238  7850-EXIT.                                                       EL6561
01239      EXIT.                                                        EL6561
01240      EJECT                                                        EL6561
01241                                                                   EL6561
01242  7900-READPREV.                                                   EL6561
01243      EXEC CICS READPREV                                           EL6561
01244           DATASET  (RATE-FILE-ID)                                 EL6561
01245           SET      (ADDRESS OF RATE-RECORD)                          CL*16
01246           RIDFLD   (PI-ERRATE-KEY)                                EL6561
01247      END-EXEC.                                                    EL6561
01248                                                                   EL6561
01249      IF PI-COMPANY-CD NOT = RT-COMPANY-CD                         EL6561
01250          MOVE LOW-VALUES TO EL6561AO                              EL6561
01251          MOVE 'Y'        TO PI-ERRATE-EOF-SW.                     EL6561
01252                                                                   EL6561
01253  7900-EXIT.                                                       EL6561
01254      EXIT.                                                        EL6561
01255      EJECT                                                        EL6561
01256                                                                   EL6561
01257  7950-END-BROWSE.                                                 EL6561
01258      EXEC CICS ENDBR                                              EL6561
01259           DATASET  (RATE-FILE-ID)                                 EL6561
01260           END-EXEC.                                               EL6561
01261                                                                   EL6561
01262      MOVE SPACE TO PI-BROWSE-SW.                                  EL6561
01263                                                                   EL6561
01264  7950-EXIT.                                                       EL6561
01265      EXIT.                                                        EL6561
01266      EJECT                                                        EL6561
01267  8000-UPDATE-MAINT-DATE.                                          EL6561
01268      MOVE SPACES                 TO ELCNTL-KEY.                   EL6561
01269                                                                   EL6561
01270      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL6561
01271      MOVE '1'                    TO CNTL-REC-TYPE.                EL6561
01272      MOVE +0                     TO CNTL-SEQ-NO.                  EL6561
01273                                                                   EL6561
01274      EXEC CICS HANDLE CONDITION                                   EL6561
01275          NOTFND   (8000-EXIT)                                     EL6561
01276      END-EXEC.                                                    EL6561
01277                                                                   EL6561
01278      EXEC CICS READ                                               EL6561
01279          UPDATE                                                   EL6561
01280          DATASET   (CNTL-FILE-ID)                                 EL6561
01281          SET       (ADDRESS OF CONTROL-FILE)                         CL*16
01282          RIDFLD    (ELCNTL-KEY)                                   EL6561
01283      END-EXEC.                                                    EL6561
01284                                                                   EL6561
01285      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL6561
01286      MOVE 'B'                    TO JP-RECORD-TYPE.               EL6561
01287      MOVE CNTL-FILE-ID           TO FILE-ID.                      EL6561
081413*    PERFORM 8400-LOG-JOURNAL-RECORD
081413*                                thru 8400-exit
01289                                                                   EL6561
01290      MOVE BIN-CURRENT-SAVE       TO CF-RATES-FILE-MAINT-DT.       EL6561
01291                                                                   EL6561
01292      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL6561
01293      MOVE 'C'                    TO JP-RECORD-TYPE.               EL6561
01294      MOVE CNTL-FILE-ID           TO FILE-ID.                      EL6561
01295                                                                   EL6561
01296      EXEC CICS REWRITE                                            EL6561
01297          DATASET   (CNTL-FILE-ID)                                 EL6561
01298          FROM      (CONTROL-FILE)                                 EL6561
01299      END-EXEC.                                                    EL6561
01300                                                                   EL6561
081413*    PERFORM 8400-LOG-JOURNAL-RECORD
081413*                                thru 8400-exit
           .
01303  8000-EXIT.                                                       EL6561
01304       EXIT.                                                       EL6561
01305      EJECT                                                        EL6561
01306                                                                   EL6561
01307  8100-SEND-INITIAL-MAP.                                           EL6561
01308      MOVE EIBTIME              TO TIME-IN.                        EL6561
01309      MOVE SAVE-DATE            TO RUNDATEO.                       EL6561
01310      MOVE TIME-OUT             TO RUNTIMEO.                       EL6561
01311      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL6561
01312      MOVE -1 TO MAXAGEL.                                          EL6561
01313      EXEC CICS SEND                                               EL6561
01314          MAP   (WS-MAPNAME)                                       EL6561
01315          MAPSET(WS-MAPSET-NAME)                                   EL6561
01316          FROM  (EL6561AO)                                         EL6561
01317          ERASE                                                    EL6561
01318          CURSOR                                                   EL6561
01319      END-EXEC.                                                    EL6561
01320                                                                   EL6561
01321      GO TO 9100-RETURN-TRAN.                                      EL6561
01322                                                                   EL6561
01323  8200-SEND-DATAONLY.                                              EL6561
01324      MOVE EIBTIME              TO TIME-IN.                        EL6561
01325      MOVE SAVE-DATE            TO RUNDATEO.                       EL6561
01326      MOVE TIME-OUT             TO RUNTIMEO.                       EL6561
01327      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL6561
01328      EXEC CICS SEND                                               EL6561
01329          MAP   (WS-MAPNAME)                                       EL6561
01330          MAPSET(WS-MAPSET-NAME)                                   EL6561
01331          FROM  (EL6561AO)                                         EL6561
01332          DATAONLY                                                 EL6561
01333          CURSOR                                                   EL6561
01334      END-EXEC.                                                    EL6561
01335                                                                   EL6561
01336      GO TO 9100-RETURN-TRAN.                                      EL6561
01337                                                                   EL6561
01338  8300-SEND-TEXT.                                                  EL6561
01339      EXEC CICS SEND TEXT                                          EL6561
01340          FROM  (LOGOFF-TEXT)                                      EL6561
01341          LENGTH(LOGOFF-LENGTH)                                    EL6561
01342          ERASE                                                    EL6561
01343          FREEKB                                                   EL6561
01344      END-EXEC.                                                    EL6561
01345                                                                   EL6561
01346      EXEC CICS RETURN                                             EL6561
01347          END-EXEC.                                                EL6561

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
081413        display ' error-el6561-journal ' ws-response
081413     end-if
081413
081413     .
081413 8400-exit.
081413     exit.

01361  8800-UNAUTHORIZED-ACCESS.                                        EL6561
01362      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL6561
01363      GO TO 8300-SEND-TEXT.                                        EL6561
01364                                                                   EL6561
01365  8810-PF23.                                                       EL6561
01366      MOVE EIBAID   TO PI-ENTRY-CD-1.                              EL6561
01367      MOVE XCTL-005 TO PGM-NAME.                                   EL6561
01368      GO TO 9300-XCTL.                                             EL6561
01369                                                                   EL6561
01370  8880-NOT-FOUND.                                                  EL6561
01371      MOVE ER-0142 TO EMI-ERROR.                                   EL6561
01372      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6561
01373      MOVE -1   TO MAXAGEL.                                        EL6561
01374                                                                   EL6561
01375      IF EIBTRNID NOT = TRANS-ID                                   EL6561
01376          GO TO 8100-SEND-INITIAL-MAP.                             EL6561
01377                                                                   EL6561
01378      GO TO 8200-SEND-DATAONLY.                                    EL6561
01379                                                                   EL6561
01380  9100-RETURN-TRAN.                                                EL6561
01381      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL6561
01382      MOVE '656B'               TO PI-CURRENT-SCREEN-NO.              CL**2
01383      EXEC CICS RETURN                                             EL6561
01384          TRANSID (TRANS-ID)                                       EL6561
01385          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL6561
01386          LENGTH  (PI-COMM-LENGTH)                                 EL6561
01387     END-EXEC.                                                     EL6561
01388                                                                   EL6561
01389  9200-RETURN-MAIN-MENU.                                           EL6561
01390      MOVE XCTL-626 TO PGM-NAME.                                   EL6561
01391      GO TO 9300-XCTL.                                             EL6561
01392                                                                   EL6561
01393  9300-XCTL.                                                       EL6561
01394      EXEC CICS XCTL                                               EL6561
01395          PROGRAM (PGM-NAME)                                       EL6561
01396          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL6561
01397          LENGTH  (PI-COMM-LENGTH)                                 EL6561
01398      END-EXEC.                                                    EL6561
01399                                                                   EL6561
01400  9400-CLEAR.                                                      EL6561
01401      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL6561
01402      GO TO 9300-XCTL.                                             EL6561
01403                                                                   EL6561
01404  9500-PF12.                                                       EL6561
01405      MOVE XCTL-010 TO PGM-NAME.                                   EL6561
01406      GO TO 9300-XCTL.                                             EL6561
01407                                                                   EL6561
01408  9600-PGMID-ERROR.                                                EL6561
01409      EXEC CICS HANDLE CONDITION                                   EL6561
01410          PGMIDERR(8300-SEND-TEXT)                                 EL6561
01411      END-EXEC.                                                    EL6561
01412                                                                   EL6561
01413      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL6561
01414      MOVE ' '          TO PI-ENTRY-CD-1.                          EL6561
01415      MOVE XCTL-005     TO PGM-NAME.                               EL6561
01416      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL6561
01417      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL6561
01418      GO TO 9300-XCTL.                                             EL6561
01419                                                                   EL6561
01420  9700-LINK-DATE-CONVERT.                                          EL6561
01421      MOVE LINK-ELDATCV TO PGM-NAME.                               EL6561
01422                                                                   EL6561
01423      EXEC CICS LINK                                               EL6561
01424          PROGRAM (PGM-NAME)                                       EL6561
01425          COMMAREA(DATE-CONVERSION-DATA)                           EL6561
01426          LENGTH  (DC-COMM-LENGTH)                                 EL6561
01427      END-EXEC.                                                    EL6561
01428                                                                   EL6561
01429  9700-EXIT.                                                       EL6561
01430      EXIT.                                                        EL6561
01431                                                                   EL6561
01432  9900-ERROR-FORMAT.                                               EL6561
01433      IF NOT EMI-ERRORS-COMPLETE                                   EL6561
01434          MOVE LINK-001 TO PGM-NAME                                EL6561
01435          EXEC CICS LINK                                           EL6561
01436              PROGRAM (PGM-NAME)                                   EL6561
01437              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL6561
01438              LENGTH  (EMI-COMM-LENGTH)                            EL6561
01439          END-EXEC.                                                EL6561
01440                                                                   EL6561
01441  9900-EXIT.                                                       EL6561
01442      EXIT.                                                        EL6561
01443                                                                   EL6561
01444  9990-ABEND.                                                      EL6561
01445      MOVE LINK-004     TO PGM-NAME.                               EL6561
01446      MOVE DFHEIBLK     TO EMI-LINE1                               EL6561
01447      EXEC CICS LINK                                               EL6561
01448          PROGRAM   (PGM-NAME)                                     EL6561
01449          COMMAREA  (EMI-LINE1)                                    EL6561
01450          LENGTH    (72)                                           EL6561
01451      END-EXEC.                                                    EL6561
01452                                                                   EL6561
01453      GO TO 8200-SEND-DATAONLY.                                    EL6561
01454                                                                   EL6561
01455      GOBACK.                                                      EL6561
01456                                                                   EL6561
01457  9995-SECURITY-VIOLATION.                                         EL6561
01458                              COPY ELCSCTP.                        EL6561
01459                                                                   EL6561
01460  9995-EXIT.                                                       EL6561
01461       EXIT.                                                       EL6561
