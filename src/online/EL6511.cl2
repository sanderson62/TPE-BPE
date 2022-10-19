00001  ID DIVISION.                                                     04/15/98
00003  PROGRAM-ID.                 EL6511.                                 LV026
00004 *              PROGRAM CONVERTED BY                                  CL*18
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*18
00006 *              CONVERSION DATE 02/12/96 13:05:16.                    CL*18
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE               CL*26
00008 *                            VMOD=2.019.                             CL*24
00009                                                                   EL6511
00010 *AUTHOR.     LOGIC,INC.                                              CL*18
00011 *            DALLAS, TEXAS.                                          CL*18
00012                                                                   EL6511
00013 *DATE-COMPILED.                                                      CL*18
00014 *SECURITY.   *****************************************************   CL*18
00015 *            *                                                   *   CL*18
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*18
00017 *            *                                                   *   CL*18
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*18
00019 *                                                                *   CL*18
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*18
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*18
00022 *            *                                                   *   CL*18
00023 *            *****************************************************   CL*18
00024                                                                   EL6511
00025 *REMARKS.    TRANSACTION - EXD2 - REINSURANCE MASTER MAINT           CL*18
00026 *                                 COMPANY MAINTENANCE.
110601******************************************************************
110601*                   C H A N G E   L O G
110601*
110601* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110601*-----------------------------------------------------------------
110601*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110601* EFFECTIVE    NUMBER
110601*-----------------------------------------------------------------
110601* 110601    2001100100006  SMVA  ADD NEW REPORT SWITCH FOR ECS152
032707* 032707    2007032100006  PEMA  ADD EXCISE TAX CAPABILITY
111413* 111413  CR2013102900003  PEMA  ADD MORT BASIS FOR LF TAX
110601******************************************************************
00027                                                                   EL6511
00028      EJECT                                                        EL6511
00029  ENVIRONMENT DIVISION.                                            EL6511
00030  DATA DIVISION.                                                   EL6511
00031  WORKING-STORAGE SECTION.                                         EL6511
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL6511
00033  77  FILLER  PIC X(32)  VALUE '*    EL6511 WORKING STORAGE    *'. EL6511
00034  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.019 ***********'.    CL*24
00035                                                                   EL6511
00036  01  WS-DATE-AREA.                                                EL6511
00037      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL6511
00038      05  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.    EL6511
00039                                                                   EL6511
00040  01  STANDARD-AREAS.                                              EL6511
00041      05  GETMAIN-SPACE               PIC X       VALUE SPACE.     EL6511
00042      05  MAP-B-NAME                  PIC X(8)    VALUE 'EL6511B'. EL6511
00043      05  MAP-C-NAME                  PIC X(8)    VALUE 'EL6511C'. EL6511
00044      05  MAPSET-NAME                 PIC X(8)    VALUE 'EL6511S'. EL6511
00045      05  TRANS-ID                    PIC X(4)    VALUE 'EXD2'.    EL6511
00046      05  THIS-PGM                    PIC X(8)    VALUE 'EL6511'.  EL6511
00047      05  PGM-NAME                    PIC X(8).                    EL6511
00048      05  TIME-IN                     PIC S9(7).                   EL6511
00049      05  TIME-OUT-R  REDEFINES TIME-IN.                           EL6511
00050          10  FILLER                  PIC X.                       EL6511
00051          10  TIME-OUT                PIC 99V99.                   EL6511
00052          10  FILLER                  PIC X(2).                    EL6511
00053      05  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL6511
00054      05  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL6511
00055      05  XCTL-626                    PIC X(8)    VALUE 'EL126'.   EL6511
00056      05  XCTL-651                    PIC X(8)    VALUE 'EL651'.   EL6511
00057      05  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL6511
00058      05  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL6511
00059      05  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL6511
00060      05  FILE-ID                     PIC X(8)    VALUE  SPACES.   EL6511
00061      05  REIN-FILE-ID                PIC X(8)    VALUE 'ERREIN'.  EL6511
00062      05  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.  EL6511
00063      05  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.    EL6511
00064                                                                   EL6511
00065      05  WS-FEE-METHOD               PIC X       VALUE SPACES.    EL6511
00066          88  VALID-FEE-METHOD                                     EL6511
00067                        VALUES ARE ' ' '1' '2' 'P'.                   CL**3
00068      05  WS-FEE-BASIS                PIC X       VALUE SPACES.    EL6511
00069          88  VALID-FEE-BASIS                                      EL6511
00070               VALUES ARE ' ' '1' '2' '3' '4' '5' '6' '7' '8'.        CL**3
00071                                                                   EL6511
00072      05  WS-SAVE-KEY.                                             EL6511
00073          10  WS-SAVE-CO-CD           PIC X       VALUE SPACE.     EL6511
00074          10  WS-SAVE-CODE            PIC X       VALUE SPACE.     EL6511
00075          10  WS-SAVE-COMP            PIC X(3)    VALUE SPACES.    EL6511
00076          10  WS-SAVE-COMP-SUB        PIC X(3)    VALUE SPACES.    EL6511
00077                                                                   EL6511
00078      05  WS-CARRIER-FOUND-SW         PIC X       VALUE SPACE.     EL6511
00079          88  CARRIER-FOUND                       VALUE 'Y'.       EL6511
00080                                                                   EL6511
00081      05  CARRIER-ACCESS.                                          EL6511
00082          10  FILLER                  PIC X(3)    VALUE SPACES.    EL6511
00083          10  WS-CARRIER              PIC X       VALUE SPACES.    EL6511
00084                                                                   EL6511
00085  01  MISC-WORK-AREAS.                                             EL6511
00086      05  WS-DATE.                                                    CL*14
00087          10 WS-MO                    PIC XX.                         CL*14
00088          10 WS-DA                    PIC XX.                         CL*14
00089          10 WS-YR                    PIC XX.                         CL*14
00090      05  WS-PHONE-IN                 PIC 9(10).                   EL6511
00091      05  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.                    EL6511
00092          10  WSPI-AREA               PIC X(3).                    EL6511
00093          10  WSPI-PFX                PIC X(3).                    EL6511
00094          10  WSPI-SFX                PIC X(4).                    EL6511
00095      05  WS-PHONE-OUT.                                            EL6511
00096          10  WSPO-AREA               PIC X(3).                    EL6511
00097          10  FILLER                  PIC X       VALUE '-'.       EL6511
00098          10  WSPO-PFX                PIC X(3).                    EL6511
00099          10  FILLER                  PIC X       VALUE '-'.       EL6511
00100          10  WSPO-SFX                PIC X(4).                    EL6511
00101                                                                   EL6511
00102      05  DEEDIT-FIELD                PIC X(15).                   EL6511
00103      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6511
00104      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(13)V99.   EL6511
00105                                                                   EL6511
00106      05  WS-TOT-PERCENT              PIC S9V9999 VALUE +0.        EL6511
00107      05  SUB1                        PIC S9(4)   VALUE +0    COMP.EL6511
00108      05  SUB2                        PIC S9(4)   VALUE +0    COMP.EL6511
00109      05  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.EL6511
00110      05  ERREIN-LENGTH               PIC S9(4)   VALUE +4023 COMP.EL6511
00111      05  ELCNTL-LENGTH               PIC S9(4)   VALUE +773  COMP.   CL*12
00112      05  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.EL6511
00113      05  MORTCD-LENGTH               PIC S9(4)   VALUE +4    COMP.   CL**5
00114 **   05  DATE-TEST-AREA              PIC 9(6).                    EL6511
00115 **   05  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.              EL6511
00116 **       10  DATE-TEST-MM            PIC 99.                      EL6511
00117 **       10  DATE-TEST-DD            PIC 99.                      EL6511
00118 **       10  DATE-TEST-YY            PIC 99.                      EL6511
00119 **   05  WS-DISP-DATE                PIC X(8)    VALUE '00/00/00'.EL6511
00120 **   05  WS-INC-DATE                 PIC X(6)    VALUE ZEROS.     EL6511
00121 **   05  WS-ERN-START                PIC X(6)    VALUE ZEROS.        CL*17
00122 **   05  WS-ERN-STOP                 PIC X(6)    VALUE ZEROS.        CL*17
LGC192     05  WS-INC-DATE                 PIC 9(11)  COMP-3            EL6511
LGC192                                                VALUE ZEROS.
LGC192     05  WS-ERN-START                PIC 9(11)  COMP-3               CL*17
LGC192                                                VALUE ZEROS.
LGC192     05  WS-ERN-STOP                 PIC 9(11)  COMP-3               CL*17
LGC192                                                VALUE ZEROS.
00123      05  DIVIDE-RESULT               PIC 99.                      EL6511
00124      05  DIVIDE-REMAINDER            PIC 9.                       EL6511
00125      05  WS-LF-THRU1                 PIC S9(07)V99   VALUE +0.    EL6511
00126      05  WS-LF-THRU2                 PIC S9(07)V99   VALUE +0.    EL6511
00127      05  WS-LF-THRU3                 PIC S9(07)V99   VALUE +0.    EL6511
00128      05  WS-LF-THRU4                 PIC S9(07)V99   VALUE +0.    EL6511
00129      05  WS-LF-THRU5                 PIC S9(07)V99   VALUE +0.    EL6511
00130      05  WS-LF-THRU6                 PIC S9(07)V99   VALUE +0.    EL6511
00131      05  WS-AH-THRU1                 PIC S9(07)V99   VALUE +0.    EL6511
00132      05  WS-AH-THRU2                 PIC S9(07)V99   VALUE +0.    EL6511
00133      05  WS-AH-THRU3                 PIC S9(07)V99   VALUE +0.    EL6511
00134      05  WS-AH-THRU4                 PIC S9(07)V99   VALUE +0.    EL6511
00135      05  WS-AH-THRU5                 PIC S9(07)V99   VALUE +0.    EL6511
00136      05  WS-AH-THRU6                 PIC S9(07)V99   VALUE +0.    EL6511
00137      05  WS-CUSTODIAL-BAL            PIC S9(07)V99   VALUE +0.       CL*16
00138      05  WS-LF-CLM-MAX               PIC S9(07)V99   VALUE +0.    EL6511
00139      05  WS-AH-CLM-MAX               PIC S9(07)V99   VALUE +0.    EL6511
00140                                                                   EL6511
00141      05  WS-CHECK-METHOD             PIC X       VALUE SPACE.     EL6511
00142          88  VALID-LIFE-METHOD              VALUE 'P' 'E' 'M' ' '.EL6511
00143          88  VALID-AH-METHOD                VALUE 'P' 'E' ' '.    EL6511
00144                                                                   EL6511
00145      05  WS-CHECK-ZERO-FEE           PIC X       VALUE SPACE.     EL6511
00146          88  VALID-ZERO-FEE                 VALUE 'Y' 'N' 'P'     EL6511
00147                                                   'E' ' '.        EL6511
00148      05  WS-CHECK-TAX                PIC X      VALUE SPACE.      EL6511
00149          88  VALID-TAX                          VALUE 'P' 'E'
111413                                                  'M' ' '.
00150                                                                   EL6511
00151      05  WS-CHECK-TAX-OPTION         PIC X      VALUE SPACE.      EL6511
00152          88  VALID-TAX-OPTION                   VALUE 'Y' 'N'     EL6511
00153                                                       'F' ' '.    EL6511
00154                                                                   EL6511
00155      05  WS-CHECK-CLAIM              PIC X      VALUE SPACE.      EL6511
00156          88  VALID-CLAIM                        VALUE 'P' 'I' 'X' EL6511
00157                                                       'Y' ' '.    EL6511
00158      05  WS-CHECK-COMMISSION         PIC X      VALUE SPACE.      EL6511
00159          88  VALID-COMMISSION                   VALUE 'P' 'E' ' '.EL6511
00160                                                                   EL6511
00161      05  WS-CHECK-PRINT-OPT          PIC X      VALUE SPACE.         CL*11
00162          88  VALID-PRINT-OPT                    VALUE 'Y' 'F' 'A'    CL*11
00163                                                       'N' ' '.    EL6511
00164      05  WS-CHECK-RPT-OPTION         PIC X      VALUE SPACE.      EL6511
00165          88  VALID-REPORT-OPTION                VALUE ' ' 'Y' 'N'.   CL**3
00166                                                                   EL6511
00167      05  WS-CESSION-TYPE             PIC X      VALUE SPACE.         CL**2
00168          88  VALID-CESSION-TYPE                 VALUE ' ' 'C' 'A'    CL**2
00169                                                       'P'.           CL**2
00170      EJECT                                                        EL6511
00171      05  ERROR-MESSAGES.                                          EL6511
00172          10  ER-0000                 PIC X(4)    VALUE '0000'.    EL6511
00173          10  ER-0004                 PIC X(4)    VALUE '0004'.    EL6511
00174          10  ER-0008                 PIC X(4)    VALUE '0008'.    EL6511
00175          10  ER-0029                 PIC X(4)    VALUE '0029'.    EL6511
00176          10  ER-0050                 PIC X(4)    VALUE '0050'.    EL6511
00177          10  ER-0068                 PIC X(4)    VALUE '0068'.    EL6511
00178          10  ER-0070                 PIC X(4)    VALUE '0070'.    EL6511
00179          10  ER-0142                 PIC X(4)    VALUE '0142'.    EL6511
00180          10  ER-0589                 PIC X(4)    VALUE '0589'.       CL**2
00181          10  ER-0648                 PIC X(4)    VALUE '0648'.    EL6511
00182          10  ER-0649                 PIC X(4)    VALUE '0649'.    EL6511
00183          10  ER-0650                 PIC X(4)    VALUE '0650'.    EL6511
00184          10  ER-0651                 PIC X(4)    VALUE '0651'.    EL6511
00185          10  ER-0652                 PIC X(4)    VALUE '0652'.    EL6511
00186          10  ER-0653                 PIC X(4)    VALUE '0653'.    EL6511
00187          10  ER-0763                 PIC X(4)    VALUE '0763'.
032707         10  ER-0875                 PIC X(4)    VALUE '0875'.
00188          10  ER-2039                 PIC X(4)    VALUE '2039'.    EL6511
00189          10  ER-2055                 PIC X(4)    VALUE '2055'.    EL6511
00190          10  ER-2056                 PIC X(4)    VALUE '2056'.    EL6511
00191          10  ER-2067                 PIC X(4)    VALUE '2067'.    EL6511
00192          10  ER-2103                 PIC X(4)    VALUE '2103'.    EL6511
00193          10  ER-2139                 PIC X(4)    VALUE '2139'.    EL6511
00194          10  ER-2140                 PIC X(4)    VALUE '2140'.    EL6511
00195          10  ER-2143                 PIC X(4)    VALUE '2143'.    EL6511
00196          10  ER-2144                 PIC X(4)    VALUE '2144'.    EL6511
00197          10  ER-2145                 PIC X(4)    VALUE '2145'.    EL6511
00198          10  ER-2146                 PIC X(4)    VALUE '2146'.    EL6511
00199          10  ER-2147                 PIC X(4)    VALUE '2147'.    EL6511
00200          10  ER-2148                 PIC X(4)    VALUE '2148'.    EL6511
00201          10  ER-2149                 PIC X(4)    VALUE '2149'.    EL6511
00202          10  ER-2208                 PIC X(4)    VALUE '2208'.    EL6511
00203          10  ER-2237                 PIC X(4)    VALUE '2237'.       CL**5
00204          10  ER-2301                 PIC X(4)    VALUE '2301'.    EL6511
00205          10  ER-2302                 PIC X(4)    VALUE '2302'.    EL6511
00206          10  ER-2303                 PIC X(4)    VALUE '2303'.    EL6511
00207          10  ER-2304                 PIC X(4)    VALUE '2304'.    EL6511
00208          10  ER-2305                 PIC X(4)    VALUE '2305'.    EL6511
00209          10  ER-2306                 PIC X(4)    VALUE '2306'.    EL6511
00210          10  ER-2307                 PIC X(4)    VALUE '2307'.    EL6511
00211          10  ER-2308                 PIC X(4)    VALUE '2308'.    EL6511
00212          10  ER-2309                 PIC X(4)    VALUE '2309'.    EL6511
00213          10  ER-2312                 PIC X(4)    VALUE '2312'.    EL6511
00214          10  ER-2313                 PIC X(4)    VALUE '2313'.       CL*17
00215          10  ER-2314                 PIC X(4)    VALUE '2314'.    EL6511
00216          10  ER-2315                 PIC X(4)    VALUE '2315'.    EL6511
00217          10  ER-2340                 PIC X(4)    VALUE '2340'.    EL6511
00218          10  ER-2355                 PIC X(4)    VALUE '2355'.    EL6511
00219          10  ER-2386                 PIC X(4)    VALUE '2386'.    EL6511
00220          10  ER-2391                 PIC X(4)    VALUE '2391'.    EL6511
00221          10  ER-7098                 PIC X(4)    VALUE '7098'.    EL6511
00222          10  ER-7349                 PIC X(4)    VALUE '7349'.       CL*17
00223          10  ER-7350                 PIC X(4)    VALUE '7350'.    EL6511
00224          10  ER-7351                 PIC X(4)    VALUE '7351'.    EL6511
00225          10  ER-7805                 PIC X(4)    VALUE '7805'.       CL*10
00226                                                                   EL6511
00227      05  ELCNTL-KEY.                                              EL6511
00228          10  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.    EL6511
00229          10  CNTL-REC-TYPE           PIC X       VALUE SPACES.    EL6511
00230          10  CNTL-ACCESS             PIC X(4)    VALUE SPACES.    EL6511
00231          10  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.  EL6511
00232                                                                   EL6511
00233 /                                                                    CL*22
00234                            COPY ELCREINV.                            CL*22
00235  EJECT                                                               CL*22
00236                            COPY ELCSCTM.                             CL*22
00237  EJECT                                                            EL6511
00238                            COPY ELCSCRTY.                            CL*10
00239      EJECT                                                        EL6511
00240                                      COPY ELCDATE.                   CL*10
00241      EJECT                                                        EL6511
00242                                      COPY ELCLOGOF.                  CL*10
00243      EJECT                                                        EL6511
00244                                      COPY ELCATTR.                   CL*10
00245      EJECT                                                        EL6511
00246                                      COPY ELCEMIB.                   CL*10
00247      EJECT                                                        EL6511
00248                                      COPY ELCINTF.                   CL*10
00249      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL6511
00250          16  PI-CHECK-MAINT-TYPE     PIC X.                       EL6511
00251              88  VALID-MAINT-TYPE                VALUE 'S' 'A'    EL6511
00252                                                        'C' 'D'.   EL6511
00253              88  ADD-FUNCTION                    VALUE 'A'.       EL6511
00254              88  SHOW-FUNCTION                   VALUE 'S'.       EL6511
00255              88  DELETE-FUNCTION                 VALUE 'D'.       EL6511
00256              88  CHANGE-FUNCTION                 VALUE 'C'.       EL6511
00257                                                                   EL6511
00258          16  PI-PREV-MAINTYP         PIC X.                       EL6511
00259                                                                   EL6511
00260          16  PI-ERREIN-KEY.                                       EL6511
00261              20  PI-CRR-COMPANY-CD   PIC X.                       EL6511
00262              20  PI-CRR-CODE         PIC X.                       EL6511
00263              20  PI-CRR-TABLE        PIC X(3).                    EL6511
00264              20  PI-CRR-TABLE-SUB    PIC X(3).                    EL6511
00265                                                                      CL**8
00266          16  PI-START-LEVEL          PIC 9(2).                       CL**8
00267                                                                   EL6511
00268          16  PI-SAVE-ERREIN-KEY      PIC X(8).                    EL6511
00269                                                                   EL6511
00270          16  PI-FIRST-TIME-SW        PIC X.                       EL6511
00271              88  FIRST-TIME                      VALUE 'Y'.       EL6511
00272          16  PI-ENTRY-SW             PIC X.                       EL6511
00273              88  NOT-FIRST-ENTRY            VALUE 'Y'.            EL6511
00274          16  PI-BROWSE-SW            PIC X.                       EL6511
00275              88  BROWSE-STARTED             VALUE 'Y'.            EL6511
00276          16  PI-ERREIN-EOF-SW        PIC X.                       EL6511
00277              88  ERREIN-EOF                 VALUE 'Y'.            EL6511
00278          16  PI-EXCESS-SW            PIC X.                       EL6511
00279              88  EXCESS-LEVEL-EXISTS        VALUE 'X'.            EL6511
00280          16  PI-COMPANY-ADD-SW       PIC X.                       EL6511
00281              88  COMPANY-RECORD-ADDED       VALUE 'Y'.            EL6511
00282                                                                   EL6511
00283          16  PI-SUB                  PIC S99.                     EL6511
00284          16  PI-LAST-LEVEL           PIC S99.                     EL6511
00285                                                                   EL6511
00286          16  PI-SAVE-TABLE           PIC X(3).                    EL6511
00287          16  PI-SAVE-COMPANY         PIC X(3).                    EL6511
00288          16  PI-SAVE-COMP-SUB        PIC X(3).                    EL6511
00289          16  PI-MAPNAME              PIC X(8).                    EL6511
00290              88  PI-MAP-B                   VALUE 'EL6511B '.     EL6511
00291              88  PI-MAP-C                   VALUE 'EL6511C '.     EL6511
00292                                                                   EL6511
00293          16  FILLER                  PIC X(593).                     CL*18
00294      EJECT                                                        EL6511
00295                                      COPY ELCJPFX.                   CL*10
00296                                      PIC X(4000).                 EL6511
00297                                                                   EL6511
00298      EJECT                                                        EL6511
00299                                      COPY ELCAID.                    CL*10
00300                                                                   EL6511
00301  01  FILLER    REDEFINES DFHAID.                                  EL6511
00302      05  FILLER                      PIC X(8).                    EL6511
00303      05  PF-VALUES                   PIC X       OCCURS 2.        EL6511
00304                                                                   EL6511
00305      EJECT                                                        EL6511
00306                                      COPY EL6511S.                   CL*10
00307                                                                   EL6511
00308  01  FILLER                  REDEFINES                            EL6511
00309      EL6511CI.                                                    EL6511
00310      12  FILLER                      PIC X(47).                   EL6511
00311      12  DESC-OCCURS OCCURS 18 TIMES                              EL6511
00312                      INDEXED BY DO-INDX.                          EL6511
00313          16  DESCL                   PIC S9(4)       COMP.        EL6511
00314          16  DESCA                   PIC X.                       EL6511
00315          16  DESCO                   PIC X(79).                   EL6511
00316      12  FILLER                      PIC X(85).                   EL6511
00317                                                                   EL6511
00318      EJECT                                                        EL6511
00319                                                                   EL6511
00320  LINKAGE SECTION.                                                 EL6511
00321                                                                   EL6511
00322  01  DFHCOMMAREA                     PIC X(1024).                 EL6511
00323                                                                   EL6511
00324 *01 PARMLIST .                                                       CL*18
00325 *    02  FILLER                      PIC S9(8)   COMP.               CL*18
00326 *    02  ERREIN-POINTER              PIC S9(8)   COMP.               CL*18
00327 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.               CL*18
00328      EJECT                                                        EL6511
00329                                      COPY ERCREIN.                   CL*10
00330      EJECT                                                        EL6511
00331                                      COPY ELCCNTL.                   CL*10
00332      EJECT                                                        EL6511
00333  PROCEDURE DIVISION.                                              EL6511
00334      CONTINUE.                                                       CL*18
00335                                                                   EL6511
00336      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6511
00337      MOVE '5'                    TO  DC-OPTION-CODE.              EL6511
00338      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6511
00339      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL6511
00340      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL6511
00341                                                                   EL6511
00342      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6511
00343                                                                   EL6511
00344  1000-START.                                                      EL6511
00345      IF EIBCALEN = 0                                              EL6511
00346          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6511
00347                                                                   EL6511
00348      MOVE 1                      TO  EMI-NUMBER-OF-LINES.         EL6511
00349                                                                   EL6511
00350      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6511
00351          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6511
00352              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6511
00353              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6511
00354              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6511
00355              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6511
00356              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6511
00357              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6511
00358              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6511
00359              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL6511
00360          ELSE                                                     EL6511
00361              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6511
00362              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6511
00363              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6511
00364              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6511
00365              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6511
00366              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6511
00367              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6511
00368              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL6511
00369                                                                   EL6511
00370      EXEC CICS HANDLE CONDITION                                   EL6511
00371          NOTOPEN  (9990-ABEND)                                    EL6511
00372          NOTFND   (8880-NOT-FOUND)                                EL6511
00373          PGMIDERR (9600-PGMID-ERROR)                              EL6511
00374          ERROR    (9990-ABEND)                                    EL6511
00375          END-EXEC.                                                EL6511
00376                                                                   EL6511
00377      IF EIBTRNID NOT = TRANS-ID                                   EL6511
00378          IF PI-MAP-C                                              EL6511
00379              MOVE LOW-VALUES     TO  EL6511CI                     EL6511
00380              MOVE 'B'            TO  PI-CRR-CODE                  EL6511
00381              GO TO 5500-BUILD-INITIAL-SCREEN                      EL6511
00382          ELSE                                                     EL6511
00383              MOVE LOW-VALUES     TO  EL6511BI                     EL6511
00384              MOVE 'B'            TO  PI-CRR-CODE                  EL6511
00385              GO TO 5000-BUILD-INITIAL-SCREEN.                     EL6511
00386                                                                   EL6511
00387      IF EIBAID = DFHCLEAR                                         EL6511
00388          MOVE LOW-VALUES         TO  PI-ERREIN-KEY                EL6511
00389          MOVE PI-SAVE-TABLE      TO  PI-CRR-TABLE                 EL6511
00390          MOVE 'A'                TO  PI-CRR-CODE                  EL6511
00391          GO TO 9400-CLEAR.                                        EL6511
00392                                                                   EL6511
00393      IF PI-MAP-C                                                  EL6511
00394          GO TO 3000-RECEIVE.                                      EL6511
00395                                                                   EL6511
00396      EJECT                                                        EL6511
00397  2000-RECEIVE.                                                    EL6511
00398      MOVE LOW-VALUES             TO  EL6511BI.                    EL6511
00399      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6511
00400          MOVE ER-0008            TO  EMI-ERROR                    EL6511
00401          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6511
00402          MOVE -1                 TO  MAINTYPL                     EL6511
00403          GO TO 8200-SEND-DATAONLY.                                EL6511
00404                                                                   EL6511
00405      EXEC CICS RECEIVE                                            EL6511
00406          MAP    (MAP-B-NAME)                                      EL6511
00407          MAPSET (MAPSET-NAME)                                     EL6511
00408          INTO   (EL6511BI)                                        EL6511
00409      END-EXEC.                                                    EL6511
00410                                                                   EL6511
00411      IF BENTERL = 0                                               EL6511
00412          GO TO 2100-CHECK-BENTERS.                                EL6511
00413                                                                   EL6511
00414      IF EIBAID NOT = DFHENTER                                     EL6511
00415          MOVE ER-0004            TO  EMI-ERROR                    EL6511
00416          GO TO 2200-INPUT-ERROR.                                  EL6511
00417                                                                   EL6511
00418      IF (BENTERI NUMERIC) AND (BENTERI > 0 AND < 25)              EL6511
00419          MOVE PF-VALUES (BENTERI)    TO  EIBAID                   EL6511
00420      ELSE                                                         EL6511
00421          MOVE ER-0029                TO  EMI-ERROR                EL6511
00422          GO TO 2200-INPUT-ERROR.                                  EL6511
00423                                                                   EL6511
00424      EJECT                                                        EL6511
00425                                                                   EL6511
00426  2100-CHECK-BENTERS.                                              EL6511
00427      IF EIBAID = DFHPF23                                          EL6511
00428          GO TO 8810-PF23.                                         EL6511
00429                                                                   EL6511
00430      IF EIBAID = DFHPF24                                          EL6511
00431          GO TO 9200-RETURN-MAIN-MENU.                             EL6511
00432                                                                   EL6511
00433      IF EIBAID = DFHPF12                                          EL6511
00434          GO TO 9500-PF12.                                         EL6511
00435                                                                   EL6511
00436      IF MAINTYPL GREATER ZERO                                     EL6511
00437          IF MAINTYPI NOT = SPACE                                  EL6511
00438              IF EIBAID NOT = DFHENTER                             EL6511
00439                  MOVE ER-0050    TO  EMI-ERROR                    EL6511
00440                  GO TO 2200-INPUT-ERROR.                          EL6511
00441                                                                   EL6511
00442      IF EIBAID = DFHPF1                                           EL6511
00443          GO TO 7500-PAGE-FORWARD.                                 EL6511
00444                                                                   EL6511
00445      IF EIBAID = DFHPF2                                           EL6511
00446          GO TO 7600-PAGE-BACKWARD.                                EL6511
00447                                                                   EL6511
00448      IF EIBAID = DFHPF7                                           EL6511
00449          MOVE LOW-VALUES         TO  PI-ERREIN-KEY                EL6511
00450          MOVE PI-SAVE-TABLE      TO  PI-CRR-TABLE                 EL6511
00451          MOVE 'A'                TO  PI-CRR-CODE                  EL6511
00452          MOVE XCTL-651           TO  PGM-NAME                     EL6511
00453          GO TO 9300-XCTL.                                         EL6511
00454                                                                   EL6511
00455      IF EIBAID = DFHPF9                                           EL6511
00456          MOVE 'EL6511C'          TO  PI-MAPNAME                   EL6511
00457          MOVE 'B'                TO  PI-CRR-CODE                  EL6511
00458          GO TO 5500-BUILD-INITIAL-SCREEN.                         EL6511
00459                                                                   EL6511
00460      IF EIBAID = DFHENTER                                         EL6511
00461          GO TO 4000-EDIT-MAINT.                                   EL6511
00462                                                                   EL6511
00463      MOVE ER-0029                TO  EMI-ERROR.                   EL6511
00464                                                                   EL6511
00465  2200-INPUT-ERROR.                                                EL6511
00466      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6511
00467      MOVE AL-UNBON               TO  BENTERA.                     EL6511
00468      MOVE -1                     TO  BENTERL.                     EL6511
00469      GO TO 8200-SEND-DATAONLY.                                    EL6511
00470                                                                   EL6511
00471      EJECT                                                        EL6511
00472                                                                   EL6511
00473  3000-RECEIVE.                                                    EL6511
00474      MOVE LOW-VALUES             TO  EL6511CI.                    EL6511
00475      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6511
00476          MOVE ER-0008            TO  EMI-ERROR                    EL6511
00477          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6511
00478          MOVE -1                 TO  DESC1L                       EL6511
00479          GO TO 8210-SEND-DATAONLY.                                EL6511
00480                                                                   EL6511
00481      EXEC CICS RECEIVE                                            EL6511
00482          MAP    (MAP-C-NAME)                                      EL6511
00483          MAPSET (MAPSET-NAME)                                     EL6511
00484          INTO   (EL6511CI)                                        EL6511
00485      END-EXEC.                                                    EL6511
00486                                                                   EL6511
00487      IF CENTERL = 0                                               EL6511
00488          GO TO 3100-CHECK-CENTERS.                                EL6511
00489                                                                   EL6511
00490      IF EIBAID NOT = DFHENTER                                     EL6511
00491          MOVE ER-0004            TO  EMI-ERROR                    EL6511
00492          GO TO 3200-INPUT-ERROR.                                  EL6511
00493                                                                   EL6511
00494      IF (CENTERI NUMERIC) AND (CENTERI > 0 AND < 25)              EL6511
00495          MOVE PF-VALUES (CENTERI)    TO  EIBAID                   EL6511
00496      ELSE                                                         EL6511
00497          MOVE ER-0029                TO  EMI-ERROR                EL6511
00498          GO TO 3200-INPUT-ERROR.                                  EL6511
00499                                                                   EL6511
00500      EJECT                                                        EL6511
00501                                                                   EL6511
00502  3100-CHECK-CENTERS.                                              EL6511
00503      IF EIBAID = DFHPF23                                          EL6511
00504          GO TO 8810-PF23.                                         EL6511
00505                                                                   EL6511
00506      IF EIBAID = DFHPF24                                          EL6511
00507          GO TO 9200-RETURN-MAIN-MENU.                             EL6511
00508                                                                   EL6511
00509      IF EIBAID = DFHPF12                                          EL6511
00510          GO TO 9500-PF12.                                         EL6511
00511                                                                   EL6511
00512      IF EIBAID = DFHPF1                                           EL6511
00513          GO TO 7500-PAGE-FORWARD.                                 EL6511
00514                                                                   EL6511
00515      IF EIBAID = DFHPF2                                           EL6511
00516          GO TO 7600-PAGE-BACKWARD.                                EL6511
00517                                                                   EL6511
00518      IF EIBAID = DFHPF7                                           EL6511
00519          MOVE LOW-VALUES         TO  PI-ERREIN-KEY                EL6511
00520          MOVE PI-SAVE-TABLE      TO  PI-CRR-TABLE                 EL6511
00521          MOVE 'A'                TO  PI-CRR-CODE                  EL6511
00522          MOVE XCTL-651           TO  PGM-NAME                     EL6511
00523          GO TO 9300-XCTL.                                         EL6511
00524                                                                   EL6511
00525      IF EIBAID = DFHPF8                                           EL6511
00526          MOVE 'EL6511B'          TO  PI-MAPNAME                   EL6511
00527          MOVE 'B'                TO  PI-CRR-CODE                  EL6511
00528          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL6511
00529                                                                   EL6511
00530      IF EIBAID = DFHENTER                                         EL6511
00531          GO TO 4400-CHANGE.                                       EL6511
00532                                                                   EL6511
00533      MOVE ER-0029                TO  EMI-ERROR.                   EL6511
00534                                                                   EL6511
00535  3200-INPUT-ERROR.                                                EL6511
00536      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6511
00537      MOVE AL-UNBON               TO  CENTERA.                     EL6511
00538      MOVE -1                     TO  CENTERL.                     EL6511
00539      GO TO 8210-SEND-DATAONLY.                                    EL6511
00540                                                                   EL6511
00541      EJECT                                                        EL6511
00542  4000-EDIT-MAINT.                                                 EL6511
00543      IF MAINTYPL GREATER THAN ZERO                                EL6511
00544          MOVE MAINTYPI           TO  PI-CHECK-MAINT-TYPE          EL6511
00545          IF VALID-MAINT-TYPE                                      EL6511
00546              MOVE AL-UANON       TO  MAINTYPA                     EL6511
00547          ELSE                                                     EL6511
00548              MOVE -1             TO  MAINTYPL                     EL6511
00549              MOVE AL-UABON       TO  MAINTYPA                     EL6511
00550              MOVE ER-2039        TO  EMI-ERROR                    EL6511
00551              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6511
00552      ELSE                                                         EL6511
00553          MOVE -1                 TO  MAINTYPL                     EL6511
00554          MOVE AL-UABON           TO  MAINTYPA                     EL6511
00555          MOVE ER-2039            TO  EMI-ERROR                    EL6511
00556          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6511
00557                                                                   EL6511
00558      IF NOT MODIFY-CAP                                            EL6511
00559          IF SHOW-FUNCTION                                         EL6511
00560              NEXT SENTENCE                                        EL6511
00561          ELSE                                                     EL6511
00562              MOVE 'UPDATE'       TO SM-READ                       EL6511
00563              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL6511
00564              MOVE ER-0070        TO  EMI-ERROR                    EL6511
00565              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6511
00566              GO TO 8100-SEND-INITIAL-MAP.                         EL6511
00567                                                                   EL6511
00568      IF COMPANYL GREATER ZERO                                     EL6511
00569          MOVE ZERO TO TALLY                                          CL*18
00570          INSPECT COMPANYI TALLYING TALLY FOR ALL SPACES              CL*18
00571          IF TALLY GREATER ZERO                                    EL6511
00572              MOVE -1             TO  COMPANYL                     EL6511
00573              MOVE AL-UABON       TO  COMPANYA                     EL6511
00574              MOVE ER-2340        TO  EMI-ERROR                    EL6511
00575              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6511
00576          ELSE                                                     EL6511
00577              MOVE AL-UANON       TO  COMPANYA                     EL6511
00578              MOVE COMPANYI       TO  PI-CRR-TABLE                 EL6511
00579      ELSE                                                         EL6511
00580          MOVE -1                 TO  COMPANYL                     EL6511
00581          MOVE AL-UABON           TO  COMPANYA                     EL6511
00582          MOVE ER-2140            TO  EMI-ERROR                    EL6511
00583          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6511
00584                                                                   EL6511
00585      IF COMPSUBL IS GREATER THAN ZERO                             EL6511
00586          MOVE COMPSUBI           TO  PI-CRR-TABLE-SUB             EL6511
00587          MOVE AL-UANON           TO  COMPSUBA                     EL6511
00588      ELSE                                                         EL6511
00589          MOVE ZEROS              TO  PI-CRR-TABLE-SUB.            EL6511
00590                                                                   EL6511
00591      IF NOT MODIFY-CAP                                            EL6511
00592          IF SHOW-FUNCTION                                         EL6511
00593              NEXT SENTENCE                                        EL6511
00594          ELSE                                                     EL6511
00595              MOVE 'UPDATE'       TO SM-READ                       EL6511
00596              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL6511
00597              MOVE ER-0070        TO  EMI-ERROR                    EL6511
00598              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6511
00599              GO TO 8100-SEND-INITIAL-MAP.                         EL6511
00600                                                                   EL6511
00601      IF EMI-NO-ERRORS                                             EL6511
00602          NEXT SENTENCE                                            EL6511
00603      ELSE                                                         EL6511
00604          GO TO 8200-SEND-DATAONLY.                                EL6511
00605                                                                   EL6511
00606      IF CHANGE-FUNCTION                                           EL6511
00607          GO TO 4400-CHANGE.                                       EL6511
00608                                                                   EL6511
00609      IF DELETE-FUNCTION                                           EL6511
00610          GO TO 4600-DELETE.                                       EL6511
00611                                                                   EL6511
00612      IF SHOW-FUNCTION                                             EL6511
00613          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL6511
00614                                                                   EL6511
00615      IF ADD-FUNCTION                                              EL6511
00616          GO TO 4200-ADD.                                          EL6511
00617                                                                   EL6511
00618      MOVE -1                     TO  MAINTYPL.                    EL6511
00619      MOVE ER-2056                TO  EMI-ERROR.                   EL6511
00620      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6511
00621      GO TO 8200-SEND-DATAONLY.                                    EL6511
00622                                                                   EL6511
00623  4000-EXIT.                                                       EL6511
00624      EXIT.                                                        EL6511
00625      EJECT                                                        EL6511
00626                                                                   EL6511
00627  4200-ADD.                                                        EL6511
00628      PERFORM 6400-EDIT THRU 6400-EXIT.                            EL6511
00629                                                                   EL6511
00630      IF EMI-NO-ERRORS                                             EL6511
00631          NEXT SENTENCE                                            EL6511
00632      ELSE                                                         EL6511
00633          GO TO 8200-SEND-DATAONLY.                                EL6511
00634                                                                   EL6511
00635      EXEC CICS HANDLE CONDITION                                   EL6511
00636          NOTOPEN  (9990-ABEND)                                    EL6511
00637          NOTFND   (4250-CONT)                                     EL6511
00638      END-EXEC.                                                    EL6511
00639                                                                   EL6511
00640      PERFORM 7200-READ-ERREIN THRU 7200-EXIT.                     EL6511
00641                                                                   EL6511
00642      MOVE ER-2139                TO  EMI-ERROR.                   EL6511
00643      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6511
00644      MOVE -1                     TO  MAINTYPL.                    EL6511
00645      GO TO 8200-SEND-DATAONLY.                                    EL6511
00646                                                                   EL6511
00647  4250-CONT.                                                       EL6511
00648      PERFORM 7300-ERREIN-GETMAIN THRU 7300-EXIT.                  EL6511
00649                                                                   EL6511
00650      MOVE ZEROS                  TO  RE-LF-FEE                    EL6511
00651                                      RE-AH-FEE                    EL6511
00652                                      RE-AH-PR-PCT                 EL6511
00653                                      RE-AH-78-PCT                 EL6511
00654                                      RE-LF-PR-PCT                 EL6511
00655                                      RE-LF-78-PCT                 EL6511
00656                                      RE-LF-IBNR-PCT               EL6511
00657                                      RE-AH-IBNR-PCT               EL6511
00658                                      RE-LF-CLM-PCT                EL6511
00659                                      RE-LF-CLM-MAX                EL6511
00660                                      RE-AH-CLM-PCT                EL6511
00661                                      RE-AH-CLM-MAX                EL6511
00662                                      RE-LF-FEE-RANGE-PCT (1)      EL6511
00663                                      RE-LF-FEE-RANGE-PCT (2)      EL6511
00664                                      RE-LF-FEE-RANGE-PCT (3)      EL6511
00665                                      RE-LF-FEE-RANGE-PCT (4)      EL6511
00666                                      RE-LF-FEE-RANGE-PCT (5)      EL6511
00667                                      RE-LF-FEE-RANGE-PCT (6)      EL6511
00668                                      RE-LF-FEE-THRU-AMT (1)       EL6511
00669                                      RE-LF-FEE-THRU-AMT (2)       EL6511
00670                                      RE-LF-FEE-THRU-AMT (3)       EL6511
00671                                      RE-LF-FEE-THRU-AMT (4)       EL6511
00672                                      RE-LF-FEE-THRU-AMT (5)       EL6511
00673                                      RE-LF-FEE-THRU-AMT (6)       EL6511
00674                                      RE-AH-FEE-RANGE-PCT (1)      EL6511
00675                                      RE-AH-FEE-RANGE-PCT (2)      EL6511
00676                                      RE-AH-FEE-RANGE-PCT (3)      EL6511
00677                                      RE-AH-FEE-RANGE-PCT (4)      EL6511
00678                                      RE-AH-FEE-RANGE-PCT (5)      EL6511
00679                                      RE-AH-FEE-RANGE-PCT (6)      EL6511
00680                                      RE-AH-FEE-THRU-AMT (1)       EL6511
00681                                      RE-AH-FEE-THRU-AMT (2)       EL6511
00682                                      RE-AH-FEE-THRU-AMT (3)       EL6511
00683                                      RE-AH-FEE-THRU-AMT (4)       EL6511
00684                                      RE-AH-FEE-THRU-AMT (5)       EL6511
00685                                      RE-AH-FEE-THRU-AMT (6)       EL6511
00686                                      RE-CLM-INCURRED-LIM          EL6511
00687                                      RE-CUSTODIAL-BAL                CL*16
00688                                      RE-EARNING-START-DT             CL*17
00689                                      RE-EARNING-STOP-DT
032707                                     RE-EXCISE-TAX
00690                                                                   EL6511
00691      IF PI-CARRIER-SECURITY GREATER SPACE                         EL6511
00692          MOVE PI-CARRIER-SECURITY    TO  RE-COMP-CARRIER-SECURITY.EL6511
00693                                                                   EL6511
00694      PERFORM 6000-CHECK-FOR-UPDATE THRU 6000-EXIT.                EL6511
00695                                                                   EL6511
00696      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.          EL6511
00697      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.        EL6511
00698      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6511
00699                                                                   EL6511
00700      MOVE '5'                    TO  DC-OPTION-CODE.              EL6511
00701      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL6511
00702                                                                   EL6511
00703      EXEC CICS LINK                                               EL6511
00704          PROGRAM   (PGM-NAME)                                     EL6511
00705          COMMAREA  (DATE-CONVERSION-DATA)                         EL6511
00706          LENGTH    (DC-COMM-LENGTH)                               EL6511
00707      END-EXEC.                                                    EL6511
00708                                                                   EL6511
00709      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT             EL6511
00710                                      BIN-CURRENT-SAVE.            EL6511
00711      MOVE PI-COMPANY-CD          TO  RE-COMPANY-CD.               EL6511
00712      MOVE 'RE'                   TO  RE-RECORD-ID.                EL6511
00713 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL6511
00714 *    MOVE 'A'                    TO  JP-RECORD-TYPE.              EL6511
00715 *    MOVE ERREIN-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6511
00716 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL6511
00717                                                                   EL6511
00718      EXEC CICS WRITE                                              EL6511
00719          DATASET (REIN-FILE-ID)                                   EL6511
00720          FROM    (REINSURANCE-RECORD)                             EL6511
00721          RIDFLD  (RE-CONTROL-PRIMARY)                             EL6511
00722          END-EXEC.                                                EL6511
00723                                                                   EL6511
00724 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6511
00725      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6511
00726      MOVE ER-0000                TO  EMI-ERROR.                   EL6511
00727      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6511
00728                                                                   EL6511
00729      MOVE LOW-VALUES             TO  EL6511BO.                    EL6511
00730                                                                   EL6511
00731      MOVE PI-CRR-TABLE           TO  COMPANYO.                    EL6511
00732      MOVE PI-CRR-TABLE-SUB       TO  COMPSUBO.                    EL6511
00733      MOVE AL-UANON               TO  COMPANYA                     EL6511
00734                                      COMPSUBA.                    EL6511
00735                                                                   EL6511
00736      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL6511
00737                                                                   EL6511
00738  4200-EXIT.                                                       EL6511
00739      EXIT.                                                        EL6511
00740      EJECT                                                        EL6511
00741                                                                   EL6511
00742  4400-CHANGE.                                                     EL6511
00743      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY                        EL6511
00744          NEXT SENTENCE                                            EL6511
00745      ELSE                                                         EL6511
00746          MOVE ER-2056            TO  EMI-ERROR                    EL6511
00747          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6511
00748          IF PI-MAP-B                                              EL6511
00749              MOVE -1             TO  MAINTYPL                     EL6511
00750              GO TO 8200-SEND-DATAONLY                             EL6511
00751          ELSE                                                     EL6511
00752              MOVE -1             TO  DESC1L                       EL6511
00753              GO TO 8210-SEND-DATAONLY.                            EL6511
00754                                                                   EL6511
00755      PERFORM 7200-READ-ERREIN THRU 7200-EXIT.                     EL6511
00756                                                                   EL6511
00757      IF PI-MAP-B                                                  EL6511
00758          PERFORM 6400-EDIT THRU 6400-EXIT.                        EL6511
00759                                                                   EL6511
00760      IF EMI-NO-ERRORS                                             EL6511
00761          NEXT SENTENCE                                            EL6511
00762      ELSE                                                         EL6511
00763          IF PI-MAP-C                                              EL6511
00764              GO TO 8210-SEND-DATAONLY                             EL6511
00765          ELSE                                                     EL6511
00766              GO TO 8200-SEND-DATAONLY.                            EL6511
00767                                                                   EL6511
00768      PERFORM 7400-READ-ERREIN-UPDATE THRU 7400-EXIT               EL6511
00769                                                                   EL6511
00770 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA               EL6511
00771                                                                   EL6511
00772      IF PI-MAP-C                                                  EL6511
00773          PERFORM 6200-CHECK-FOR-UPDATE THRU 6200-EXIT             EL6511
00774      ELSE                                                         EL6511
00775          PERFORM 6000-CHECK-FOR-UPDATE THRU 6000-EXIT.            EL6511
00776                                                                   EL6511
00777      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR                      EL6511
00778         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6511
00779          NEXT SENTENCE                                            EL6511
00780      ELSE                                                         EL6511
00781          EXEC CICS UNLOCK                                         EL6511
00782               DATASET  (REIN-FILE-ID)                             EL6511
00783               END-EXEC                                            EL6511
00784          MOVE ER-0068            TO  EMI-ERROR                    EL6511
00785          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6511
00786          IF PI-MAP-C                                              EL6511
00787              GO TO 5500-BUILD-INITIAL-SCREEN                      EL6511
00788          ELSE                                                     EL6511
00789              GO TO 5000-BUILD-INITIAL-SCREEN.                     EL6511
00790                                                                   EL6511
00791      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.          EL6511
00792      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.        EL6511
00793      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6511
00794      MOVE '5'                    TO  DC-OPTION-CODE.              EL6511
00795      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL6511
00796                                                                   EL6511
00797      EXEC CICS LINK                                               EL6511
00798          PROGRAM   (PGM-NAME)                                     EL6511
00799          COMMAREA  (DATE-CONVERSION-DATA)                         EL6511
00800          LENGTH    (DC-COMM-LENGTH)                               EL6511
00801      END-EXEC.                                                    EL6511
00802                                                                   EL6511
00803      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT             EL6511
00804                                      BIN-CURRENT-SAVE.            EL6511
00805 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL6511
00806 *    MOVE 'B'                    TO  JP-RECORD-TYPE.              EL6511
00807 *    MOVE ERREIN-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6511
00808 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6511
00809 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL6511
00810                                                                   EL6511
00811      EXEC CICS REWRITE                                            EL6511
00812          DATASET  (REIN-FILE-ID)                                  EL6511
00813          FROM     (REINSURANCE-RECORD)                            EL6511
00814          END-EXEC.                                                EL6511
00815                                                                   EL6511
00816 *    MOVE 'C'                    TO  JP-RECORD-TYPE.              EL6511
00817 *    MOVE ERREIN-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6511
00818 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL6511
00819 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6511
00820      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6511
00821      MOVE ER-0000                TO  EMI-ERROR.                   EL6511
00822      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6511
00823                                                                   EL6511
00824      IF PI-MAP-C                                                  EL6511
00825          MOVE LOW-VALUES         TO  EL6511CO                     EL6511
00826          GO TO 5500-BUILD-INITIAL-SCREEN                          EL6511
00827      ELSE                                                         EL6511
00828          MOVE LOW-VALUES         TO  EL6511BO                     EL6511
00829          MOVE PI-CRR-TABLE       TO  COMPANYO                     EL6511
00830          MOVE PI-CRR-TABLE-SUB   TO  COMPSUBO                     EL6511
00831          MOVE AL-UANON           TO  COMPANYA                     EL6511
00832                                      COMPSUBA                     EL6511
00833          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL6511
00834                                                                   EL6511
00835  4400-EXIT.                                                       EL6511
00836      EXIT.                                                        EL6511
00837      EJECT                                                        EL6511
00838  4600-DELETE.                                                     EL6511
00839      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY                        EL6511
00840          NEXT SENTENCE                                            EL6511
00841      ELSE                                                         EL6511
00842          MOVE ER-2056            TO  EMI-ERROR                    EL6511
00843          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6511
00844          MOVE -1                 TO  MAINTYPL                     EL6511
00845          GO TO 8200-SEND-DATAONLY.                                EL6511
00846                                                                   EL6511
00847      PERFORM 7400-READ-ERREIN-UPDATE THRU 7400-EXIT.              EL6511
00848                                                                   EL6511
00849 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL6511
00850                                                                   EL6511
00851      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR                      EL6511
00852         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6511
00853          NEXT SENTENCE                                            EL6511
00854      ELSE                                                         EL6511
00855          EXEC CICS UNLOCK                                         EL6511
00856               DATASET  (REIN-FILE-ID)                             EL6511
00857          END-EXEC                                                 EL6511
00858          MOVE ER-0068            TO  EMI-ERROR                    EL6511
00859          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6511
00860          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL6511
00861                                                                   EL6511
00862      EXEC CICS DELETE                                             EL6511
00863           DATASET  (REIN-FILE-ID)                                 EL6511
00864      END-EXEC.                                                    EL6511
00865                                                                   EL6511
00866 *    MOVE 'D'                    TO  JP-RECORD-TYPE.              EL6511
00867 *    MOVE ERREIN-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6511
00868 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL6511
00869 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6511
00870                                                                   EL6511
00871      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6511
00872      MOVE '5'                    TO  DC-OPTION-CODE.              EL6511
00873      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL6511
00874                                                                   EL6511
00875      EXEC CICS LINK                                               EL6511
00876          PROGRAM   (PGM-NAME)                                     EL6511
00877          COMMAREA  (DATE-CONVERSION-DATA)                         EL6511
00878          LENGTH    (DC-COMM-LENGTH)                               EL6511
00879      END-EXEC.                                                    EL6511
00880                                                                   EL6511
00881      MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE.            EL6511
00882      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6511
00883                                                                   EL6511
00884      MOVE ER-0000                TO  EMI-ERROR.                   EL6511
00885      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6511
00886                                                                   EL6511
00887      MOVE LOW-VALUES             TO  EL6511BO.                    EL6511
00888                                                                   EL6511
00889      MOVE PI-CRR-TABLE           TO  COMPANYO.                    EL6511
00890      MOVE PI-CRR-TABLE-SUB       TO  COMPSUBO.                    EL6511
00891      MOVE AL-UANON               TO  COMPANYA                     EL6511
00892                                      COMPSUBA.                    EL6511
00893                                                                   EL6511
00894      GO TO 8100-SEND-INITIAL-MAP.                                 EL6511
00895                                                                   EL6511
00896  4600-EXIT.                                                       EL6511
00897      EXIT.                                                        EL6511
00898      EJECT                                                        EL6511
00899                                                                   EL6511
00900  5000-BUILD-INITIAL-SCREEN.                                       EL6511
00901      MOVE LOW-VALUES             TO  EL6511BO.                    EL6511
00902                                                                   EL6511
00903      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.           EL6511
00904                                                                   EL6511
00905      IF PI-CRR-TABLE = LOW-VALUES                                 EL6511
00906          GO TO 8100-SEND-INITIAL-MAP.                             EL6511
00907                                                                   EL6511
00908      PERFORM 7200-READ-ERREIN THRU 7200-EXIT.                     EL6511
00909                                                                   EL6511
00910 ******************************************************************EL6511
00911 *    ADD CODE TO SHOW RECORDS WHICH HAVE A MATCHING CARRIER IF   *EL6511
00912 *    USER HAS CARRIER SECURITY.                                  *EL6511
00913 ******************************************************************EL6511
00914                                                                   EL6511
00915      IF PI-CARRIER-SECURITY GREATER SPACE                         EL6511
00916          IF PI-CARRIER-SECURITY = RE-COMP-CARRIER-SECURITY        EL6511
00917              NEXT SENTENCE                                        EL6511
00918          ELSE                                                     EL6511
00919              GO TO 8880-NOT-FOUND.                                EL6511
00920                                                                   EL6511
00921  5050-SET-UP-SCREEN.                                              EL6511
00922                                                                      CL*16
00923      IF PI-COMPANY-ID  =  'NCL' OR 'LGX'                             CL*16
00924         IF RE-CUSTODIAL-BAL NOT NUMERIC                              CL*16
00925             MOVE ZEROS              TO  RE-CUSTODIAL-BAL             CL*16
00926         ELSE                                                         CL*16
00927             IF RE-CUSTODIAL-BAL  =  ZEROS                            CL*16
00928                 NEXT SENTENCE                                        CL*16
00929             ELSE                                                     CL*16
00930                 MOVE RE-CUSTODIAL-BAL   TO  CUSTAMTO                 CL*16
00931                 MOVE AL-UNNON           TO  CUSTAMTA                 CL*16
00932      ELSE                                                            CL*16
00933          MOVE AL-SANOF              TO  CUSTAMTA                     CL*16
00934          MOVE AL-SADOF              TO  CUSTDSCA.                    CL*16
00935                                                                      CL*16
00936      IF RE-LF-FEE = ZEROS                                         EL6511
00937          NEXT SENTENCE                                            EL6511
00938      ELSE                                                         EL6511
00939          MOVE RE-LF-FEE          TO  LIFEFEEO                     EL6511
00940          MOVE AL-UNNON           TO  LIFEFEEA.                    EL6511
00941                                                                   EL6511
00942      IF RE-AH-FEE = ZEROS                                         EL6511
00943          NEXT SENTENCE                                            EL6511
00944      ELSE                                                         EL6511
00945          MOVE RE-AH-FEE          TO  AHFEEO                       EL6511
00946          MOVE AL-UNNON           TO  AHFEEA.                      EL6511
00947                                                                   EL6511
00948      IF RE-AH-PR-PCT NUMERIC                                         CL**4
00949          MOVE RE-AH-PR-PCT       TO  AHPRO                        EL6511
00950          MOVE AL-UNNON           TO  AHPRA.                       EL6511
00951                                                                   EL6511
00952      IF RE-AH-78-PCT NUMERIC                                         CL**4
00953          MOVE RE-AH-78-PCT       TO  AHR78O                       EL6511
00954          MOVE AL-UNNON           TO  AHR78A.                      EL6511
00955                                                                   EL6511
00956      IF RE-LF-PR-PCT NOT NUMERIC                                  EL6511
00957          MOVE ZEROS              TO RE-LF-PR-PCT.                 EL6511
00958                                                                   EL6511
00959      IF RE-LF-PR-PCT NUMERIC                                         CL**4
00960          MOVE RE-LF-PR-PCT       TO  LFPRO                        EL6511
00961          MOVE AL-UNNON           TO  LFPRA.                       EL6511
00962                                                                   EL6511
00963      IF RE-LF-78-PCT NOT NUMERIC                                  EL6511
00964          MOVE ZEROS              TO RE-LF-78-PCT.                 EL6511
00965                                                                   EL6511
00966      IF RE-LF-78-PCT NUMERIC                                         CL**4
00967          MOVE RE-LF-78-PCT       TO  LFR78O                       EL6511
00968          MOVE AL-UNNON           TO  LFR78A.                      EL6511
00969                                                                   EL6511
00970      IF RE-LF-IBNR-PCT NOT = ZEROS                                EL6511
00971          MOVE RE-LF-IBNR-PCT     TO  LFIBNRO                      EL6511
00972          MOVE AL-UNNON           TO  LFIBNRA.                     EL6511
00973                                                                   EL6511
00974      IF RE-AH-IBNR-PCT NOT = ZEROS                                EL6511
00975          MOVE RE-AH-IBNR-PCT     TO  AHIBNRO                      EL6511
00976          MOVE AL-UNNON           TO  AHIBNRA.                     EL6511
00977                                                                   EL6511
00978      IF RE-LF-CLM-PCT NUMERIC                                     EL6511
00979         MOVE RE-LF-CLM-PCT       TO  LCLMPCTO                     EL6511
00980         MOVE AL-UNNON            TO  LCLMPCTA.                    EL6511
00981                                                                   EL6511
           IF RE-EXCISE-TAX NOT NUMERIC
              MOVE ZEROS               TO RE-EXCISE-TAX
           END-IF

032707     MOVE RE-EXCISE-TAX          TO EXTAXO
032707     MOVE AL-UNNON               TO EXTAXA

00982      IF RE-LF-CLM-MAX NUMERIC                                     EL6511
00983         MOVE RE-LF-CLM-MAX       TO  LCLMMAXO                     EL6511
00984         MOVE AL-UNNON            TO  LCLMMAXA.                    EL6511
00985                                                                   EL6511
00986      IF RE-AH-CLM-PCT NUMERIC                                     EL6511
00987         MOVE RE-AH-CLM-PCT       TO  ACLMPCTO                     EL6511
00988         MOVE AL-UNNON            TO  ACLMPCTA.                    EL6511
00989                                                                   EL6511
00990      IF RE-AH-CLM-MAX NUMERIC                                     EL6511
00991         MOVE RE-AH-CLM-MAX       TO  ACLMMAXO                     EL6511
00992         MOVE AL-UNNON            TO  ACLMMAXA.                    EL6511
00993                                                                   EL6511
00994      IF (RE-LF-FEE-RANGE-PCT (1) NUMERIC)                         EL6511
00995         MOVE RE-LF-FEE-RANGE-PCT (1) TO  LFPCT1O                  EL6511
00996         MOVE AL-UNNON                TO  LFPCT1A.                 EL6511
00997                                                                   EL6511
00998      IF (RE-LF-FEE-RANGE-PCT (2) NUMERIC)                         EL6511
00999         MOVE RE-LF-FEE-RANGE-PCT (2) TO  LFPCT2O                  EL6511
01000         MOVE AL-UNNON                TO  LFPCT2A.                 EL6511
01001                                                                   EL6511
01002      IF (RE-LF-FEE-RANGE-PCT (3) NUMERIC)                         EL6511
01003         MOVE RE-LF-FEE-RANGE-PCT (3) TO  LFPCT3O                  EL6511
01004         MOVE AL-UNNON                TO  LFPCT3A.                 EL6511
01005                                                                   EL6511
01006      IF (RE-LF-FEE-RANGE-PCT (4) NUMERIC)                         EL6511
01007         MOVE RE-LF-FEE-RANGE-PCT (4) TO  LFPCT4O                  EL6511
01008         MOVE AL-UNNON                TO  LFPCT4A.                 EL6511
01009                                                                   EL6511
01010      IF (RE-LF-FEE-RANGE-PCT (5) NUMERIC)                         EL6511
01011         MOVE RE-LF-FEE-RANGE-PCT (5) TO  LFPCT5O                  EL6511
01012         MOVE AL-UNNON                TO  LFPCT5A.                 EL6511
01013                                                                   EL6511
01014      IF (RE-LF-FEE-RANGE-PCT (6) NUMERIC)                         EL6511
01015         MOVE RE-LF-FEE-RANGE-PCT (6) TO  LFPCT6O                  EL6511
01016         MOVE AL-UNNON                TO  LFPCT6A.                 EL6511
01017                                                                   EL6511
01018      IF (RE-LF-FEE-THRU-AMT  (1) NUMERIC)                         EL6511
01019         MOVE RE-LF-FEE-THRU-AMT (1)  TO  LFTHRU1O                 EL6511
01020         MOVE AL-UNNON                TO  LFTHRU1A.                EL6511
01021                                                                   EL6511
01022      IF (RE-LF-FEE-THRU-AMT  (2) NUMERIC)                         EL6511
01023         MOVE RE-LF-FEE-THRU-AMT (2)  TO  LFTHRU2O                 EL6511
01024         MOVE AL-UNNON                TO  LFTHRU2A.                EL6511
01025                                                                   EL6511
01026      IF (RE-LF-FEE-THRU-AMT  (3) NUMERIC)                         EL6511
01027         MOVE RE-LF-FEE-THRU-AMT (3)  TO  LFTHRU3O                 EL6511
01028         MOVE AL-UNNON                TO  LFTHRU3A.                EL6511
01029                                                                   EL6511
01030      IF (RE-LF-FEE-THRU-AMT  (4) NUMERIC)                         EL6511
01031         MOVE RE-LF-FEE-THRU-AMT (4)  TO  LFTHRU4O                 EL6511
01032         MOVE AL-UNNON                TO  LFTHRU4A.                EL6511
01033                                                                   EL6511
01034      IF (RE-LF-FEE-THRU-AMT  (5) NUMERIC)                         EL6511
01035         MOVE RE-LF-FEE-THRU-AMT (5)  TO  LFTHRU5O                 EL6511
01036         MOVE AL-UNNON                TO  LFTHRU5A.                EL6511
01037                                                                   EL6511
01038      IF (RE-LF-FEE-THRU-AMT  (6) NUMERIC)                         EL6511
01039         MOVE RE-LF-FEE-THRU-AMT (6)  TO  LFTHRU6O                 EL6511
01040         MOVE AL-UNNON                TO  LFTHRU6A.                EL6511
01041                                                                   EL6511
01042      IF (RE-AH-FEE-RANGE-PCT (1) NUMERIC)                         EL6511
01043         MOVE RE-AH-FEE-RANGE-PCT (1) TO  AHPCT1O                  EL6511
01044         MOVE AL-UNNON                TO  AHPCT1A.                 EL6511
01045                                                                   EL6511
01046      IF (RE-AH-FEE-RANGE-PCT (2) NUMERIC)                         EL6511
01047         MOVE RE-AH-FEE-RANGE-PCT (2) TO  AHPCT2O                  EL6511
01048         MOVE AL-UNNON                TO  AHPCT2A.                 EL6511
01049                                                                   EL6511
01050      IF (RE-AH-FEE-RANGE-PCT (3) NUMERIC)                         EL6511
01051         MOVE RE-AH-FEE-RANGE-PCT (3) TO  AHPCT3O                  EL6511
01052         MOVE AL-UNNON                TO  AHPCT3A.                 EL6511
01053                                                                   EL6511
01054      IF (RE-AH-FEE-RANGE-PCT (4) NUMERIC)                         EL6511
01055         MOVE RE-AH-FEE-RANGE-PCT (4) TO  AHPCT4O                  EL6511
01056         MOVE AL-UNNON                TO  AHPCT4A.                 EL6511
01057                                                                   EL6511
01058      IF (RE-AH-FEE-RANGE-PCT (5) NUMERIC)                         EL6511
01059         MOVE RE-AH-FEE-RANGE-PCT (5) TO  AHPCT5O                  EL6511
01060         MOVE AL-UNNON                TO  AHPCT5A.                 EL6511
01061                                                                   EL6511
01062      IF (RE-AH-FEE-RANGE-PCT (6) NUMERIC)                         EL6511
01063         MOVE RE-AH-FEE-RANGE-PCT (6) TO  AHPCT6O                  EL6511
01064         MOVE AL-UNNON                TO  AHPCT6A.                 EL6511
01065                                                                   EL6511
01066      IF (RE-AH-FEE-THRU-AMT  (1) NUMERIC)                         EL6511
01067         MOVE RE-AH-FEE-THRU-AMT (1)  TO  AHTHRU1O                 EL6511
01068         MOVE AL-UNNON                TO  AHTHRU1A.                EL6511
01069                                                                   EL6511
01070      IF (RE-AH-FEE-THRU-AMT  (2) NUMERIC)                         EL6511
01071         MOVE RE-AH-FEE-THRU-AMT (2)  TO  AHTHRU2O                 EL6511
01072         MOVE AL-UNNON                TO  AHTHRU2A.                EL6511
01073                                                                   EL6511
01074      IF (RE-AH-FEE-THRU-AMT  (3) NUMERIC)                         EL6511
01075         MOVE RE-AH-FEE-THRU-AMT (3)  TO  AHTHRU3O                 EL6511
01076         MOVE AL-UNNON                TO  AHTHRU3A.                EL6511
01077                                                                   EL6511
01078      IF (RE-AH-FEE-THRU-AMT  (4) NUMERIC)                         EL6511
01079         MOVE RE-AH-FEE-THRU-AMT (4)  TO  AHTHRU4O                 EL6511
01080         MOVE AL-UNNON                TO  AHTHRU4A.                EL6511
01081                                                                   EL6511
01082      IF (RE-AH-FEE-THRU-AMT  (5) NUMERIC)                         EL6511
01083         MOVE RE-AH-FEE-THRU-AMT (5)  TO  AHTHRU5O                 EL6511
01084         MOVE AL-UNNON                TO  AHTHRU5A.                EL6511
01085                                                                   EL6511
01086      IF (RE-AH-FEE-THRU-AMT  (6) NUMERIC)                         EL6511
01087         MOVE RE-AH-FEE-THRU-AMT (6)  TO  AHTHRU6O                 EL6511
01088         MOVE AL-UNNON                TO  AHTHRU6A.                EL6511
01089                                                                   EL6511
01090 ***  Y2K PROJ 7744                                                   CL*25
01091      IF RE-CLM-INCURRED-LIM = ZEROS                                  CL*19
01092          CONTINUE                                                    CL*20
01093      ELSE                                                         EL6511
LGC192         MOVE RE-CLM-INCURRED-LIM
LGC192                                 TO  WS-RE-CLM-INCURRED-LIM-N
01094          MOVE RE-CLM-YR          TO  WS-YR                           CL*14
01095          MOVE RE-CLM-MO          TO  WS-MO                           CL*14
01096          MOVE RE-CLM-DA          TO  WS-DA                           CL*14
01097          MOVE WS-DATE            TO  CLINCDTO                        CL*14
01098          MOVE AL-UNNON           TO  CLINCDTA                        CL*20
01099      END-IF.                                                         CL*20
01100                                                                      CL*14
01101      IF RE-EARNING-START-DT = ZEROS                                  CL*19
01102          CONTINUE                                                    CL*20
01103      ELSE                                                            CL*14
LGC190         MOVE RE-EARNING-START-DT
LGC190                                 TO  WS-RE-EARNING-START-DT-N
01104          MOVE RE-EARN-YR         TO  WS-YR                           CL*14
01105          MOVE RE-EARN-MO         TO  WS-MO                           CL*14
01106          MOVE RE-EARN-DA         TO  WS-DA                           CL*14
01107          MOVE WS-DATE            TO  ERBEGDTO                        CL*17
01108          MOVE AL-UNNON           TO  ERBEGDTA                        CL*20
01109      END-IF.                                                         CL*20
01110                                                                   EL6511
01111      IF RE-EARNING-STOP-DT NOT NUMERIC                               CL*17
01112          MOVE ZEROS              TO  RE-EARNING-STOP-DT              CL*21
01113      END-IF.                                                         CL*21
01114                                                                      CL*17
01115      IF RE-EARNING-STOP-DT NOT = ZEROS                               CL*17
LGC190         MOVE RE-EARNING-STOP-DT
LGC190                                 TO  WS-RE-EARNING-STOP-DT-N
01116          MOVE RE-EARN-STOP-YR    TO  WS-YR                           CL*17
01117          MOVE RE-EARN-STOP-MO    TO  WS-MO                           CL*17
01118          MOVE RE-EARN-STOP-DA    TO  WS-DA                           CL*17
01119          MOVE WS-DATE            TO  ERENDDTO                        CL*17
01120          MOVE AL-UNNON           TO  ERENDDTA                        CL*21
01121      END-IF.                                                         CL*21
01122 ***  Y2K PROJ 7744                                                   CL*25
01123                                                                      CL*17
01124      MOVE RE-EARN-STOP-CODE      TO  ERENDCDO.                       CL*17
01125      MOVE AL-UANON               TO  ERENDCDA.                       CL*17
01126                                                                      CL*17
01127      MOVE PI-LIFE-OVERRIDE-L2    TO  LFCVTYPO.                       CL*17
01128      MOVE PI-AH-OVERRIDE-L2      TO  AHCVTYPO.                       CL*17
CIDMOD     MOVE PI-LIFE-OVERRIDE-L6    TO  LFCVTYPO.                         000
CIDMOD     MOVE PI-AH-OVERRIDE-L6      TO  AHCVTYPO.                         000
01129      MOVE AL-SANON               TO  LFCVTYPA                     EL6511
01130                                      AHCVTYPA.                    EL6511
01131      MOVE RE-LF-FEE-METHOD       TO  LFMETHO.                     EL6511
01132      MOVE RE-LF-FEE-BASIS        TO  LFBASISO.                    EL6511
01133      MOVE RE-AH-FEE-METHOD       TO  AHMETHO.                     EL6511
01134      MOVE RE-AH-FEE-BASIS        TO  AHBASISO.                    EL6511
01135      MOVE AL-UANON               TO  LFMETHA                      EL6511
01136                                      LFBASISA                     EL6511
01137                                      AHMETHA                      EL6511
01138                                      AHBASISA.                    EL6511
01139      MOVE RE-NAME                TO  CONAMEO.                     EL6511
01140      MOVE RE-CEDE-NAME           TO  CEDNAMEO.                    EL6511
01141      MOVE RE-CEDING-TYPE-FLAG    TO  CESTYPEO.                       CL**2
01142      MOVE RE-GL-CENTER           TO  GLCNTRO.                        CL*13
01143      MOVE RE-LF-PE               TO  CMLIFEO.                     EL6511
01144      MOVE RE-AH-PE               TO  CMAHO.                       EL6511
01145      MOVE RE-MORT-CODE           TO  MORTCDO.                     EL6511
01146      MOVE RE-MORT-SW             TO  MORTSWO.                     EL6511
01147      MOVE RE-ZERO-LF-FEE         TO  FEELIFEO.                    EL6511
01148      MOVE RE-ZERO-AH-FEE         TO  FEEAHO.                      EL6511
01149      MOVE RE-PRT-ST              TO  PRTAXO.                      EL6511
01150      MOVE RE-LF-COMM             TO  COMLIFEO.                    EL6511
01151      MOVE RE-AH-COMM             TO  COMAHO.                      EL6511
01152      MOVE RE-PRT-OW              TO  PRTAXOWO.                    EL6511
01153      MOVE RE-PRT-CRSV            TO  PRTCRSVO.                       CL*11
01154      MOVE RE-CEDING-STMT-OPT-A   TO  RPTAO.                          CL**3
01155      MOVE RE-CEDING-STMT-OPT-B   TO  RPTBO.                          CL**3
01156      MOVE RE-CEDING-STMT-OPT-C   TO  RPTCO.                          CL**3
01157      MOVE RE-CEDING-STMT-OPT-D   TO  RPTDO.                          CL**3
01158      MOVE RE-CEDING-STMT-OPT-E   TO  RPTEO.
110601     MOVE RE-STATE-EXHIBIT-OPT-F TO  RPTFO.                          CL**3
01159      MOVE RE-CLAIM-CODE          TO  CLAIMO.                      EL6511
01160      MOVE RE-LF-TAX              TO  TAXLIFEO.                    EL6511
01161      MOVE RE-AH-TAX              TO  TAXAHO.                      EL6511
01162      MOVE RE-COMP-CARRIER-SECURITY                                EL6511
01163                                  TO  BCARO.                       EL6511
01164      MOVE RE-REINS-GROUPING-CODE TO  REIGRPO.                     EL6511
01165                                                                   EL6511
01166      MOVE RE-LAST-MAINT-DT       TO  DC-BIN-DATE-1.                  CL**3
01167      MOVE SPACE                  TO  DC-OPTION-CODE.                 CL**3
01168      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL**3
01169      MOVE DC-GREG-DATE-1-EDIT    TO  ALUDATEO.                       CL**3
01170      MOVE RE-LAST-MAINT-HHMMSS   TO  TIME-IN.                        CL**3
01171      MOVE TIME-OUT               TO  ALUTIMEO.                       CL**3
01172      MOVE RE-LAST-MAINT-USER     TO  ALUBYO.                         CL**3
01173                                                                      CL**3
01174                                                                      CL**3
01175      MOVE AL-UANON               TO  CONAMEA                      EL6511
01176                                      REIGRPA                      EL6511
01177                                      PRTAXA                       EL6511
01178                                      PRTAXOWA                     EL6511
01179                                      PRTCRSVA                        CL*11
01180                                      RPTAA                           CL**3
01181                                      RPTBA                           CL**3
01182                                      RPTCA                           CL**3
01183                                      RPTDA                           CL**3
01184                                      RPTEA
110601                                     RPTFA                           CL**3
01185                                      CEDNAMEA                     EL6511
01186                                      CESTYPEA                        CL**2
01187                                      CMLIFEA                      EL6511
01188                                      CMAHA                        EL6511
01189                                      MORTCDA                      EL6511
01190                                      MORTSWA                      EL6511
01191                                      FEELIFEA                     EL6511
01192                                      FEEAHA                       EL6511
01193                                      COMLIFEA                     EL6511
01194                                      COMAHA                       EL6511
01195                                      CLAIMA                       EL6511
01196                                      TAXLIFEA                     EL6511
01197                                      TAXAHA                       EL6511
01198                                      COMPANYA                     EL6511
01199                                      COMPSUBA
032707                                     EXTAXA
01200                                                                      CL*13
01201      IF PI-COMPANY-ID = 'NCL' OR 'LGX'                               CL*13
01202         MOVE AL-UANON            TO  GLCNTRA.                        CL*13
01203                                                                      CL*13
01204      IF PI-NO-CARRIER-SECURITY                                    EL6511
01205          MOVE AL-UANON           TO  BCARA                        EL6511
01206      ELSE                                                         EL6511
01207          MOVE AL-SANOF           TO  BCARA.                       EL6511
01208                                                                   EL6511
01209      MOVE PI-CRR-TABLE           TO  COMPANYO.                    EL6511
01210      MOVE PI-CRR-TABLE-SUB       TO  COMPSUBO.                    EL6511
01211      MOVE PI-CHECK-MAINT-TYPE    TO  PI-PREV-MAINTYP.             EL6511
01212                                                                   EL6511
01213      GO TO 8100-SEND-INITIAL-MAP.                                 EL6511
01214                                                                   EL6511
01215      EJECT                                                        EL6511
01216  5500-BUILD-INITIAL-SCREEN.                                       EL6511
01217      MOVE LOW-VALUES             TO  EL6511CO.                    EL6511
01218                                                                   EL6511
01219      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.           EL6511
01220                                                                   EL6511
01221      IF PI-CRR-TABLE = LOW-VALUES                                 EL6511
01222          GO TO 8110-SEND-INITIAL-MAP.                             EL6511
01223                                                                   EL6511
01224      PERFORM 7200-READ-ERREIN THRU 7200-EXIT.                     EL6511
01225                                                                   EL6511
01226 ******************************************************************EL6511
01227 *    ADD CODE TO SHOW RECORDS WHICH HAVE A MATCHING CARRIER IF   *EL6511
01228 *    USER HAS CARRIER SECURITY.                                  *EL6511
01229 ******************************************************************EL6511
01230                                                                   EL6511
01231      IF PI-CARRIER-SECURITY GREATER SPACE                         EL6511
01232          IF PI-CARRIER-SECURITY = RE-COMP-CARRIER-SECURITY        EL6511
01233              NEXT SENTENCE                                        EL6511
01234          ELSE                                                     EL6511
01235              GO TO 8880-NOT-FOUND.                                EL6511
01236                                                                   EL6511
01237  5550-SET-UP-SCREEN.                                              EL6511
01238      MOVE PI-CRR-TABLE           TO  COMPO.                       EL6511
01239      MOVE PI-CRR-TABLE-SUB       TO  SUBO.                        EL6511
01240      MOVE RE-COMP-CARRIER-SECURITY                                EL6511
01241                                  TO  BCARO.                       EL6511
01242                                                                   EL6511
01243      MOVE +0                     TO  SUB1.                        EL6511
01244      SET DO-INDX                 TO  +1.                          EL6511
01245      SET DO-INDX DOWN BY +1.                                      EL6511
01246                                                                   EL6511
01247  5525-CONT.                                                       EL6511
01248      SET DO-INDX UP BY +1.                                        EL6511
01249      ADD +1                      TO  SUB1.                        EL6511
01250                                                                   EL6511
01251      IF DO-INDX GREATER +18                                       EL6511
01252          GO TO 5550-SET-ATTR.                                     EL6511
01253                                                                   EL6511
01254      MOVE RE-DESC (SUB1)         TO  DESCO (DO-INDX).             EL6511
01255      MOVE AL-UANON               TO  DESCA (DO-INDX).             EL6511
01256                                                                   EL6511
01257      GO TO 5525-CONT.                                             EL6511
01258                                                                   EL6511
01259  5550-SET-ATTR.                                                   EL6511
01260      MOVE RE-COMP-CARRIER-SECURITY                                EL6511
01261                                  TO  CCARO.                       EL6511
01262      MOVE -1                     TO  DESC1L.                      EL6511
01263      MOVE AL-SANON               TO  COMPA                        EL6511
01264                                      SUBA.                        EL6511
01265      GO TO 8110-SEND-INITIAL-MAP.                                 EL6511
01266                                                                   EL6511
01267      EJECT                                                        EL6511
01268  6000-CHECK-FOR-UPDATE.                                           EL6511
01269       IF CHANGE-FUNCTION                                          EL6511
01270           GO TO 6010-CONT.                                        EL6511
01271                                                                   EL6511
01272       IF COMPANYL GREATER ZERO                                    EL6511
01273           MOVE COMPANYI          TO  RE-COMP-PRIME.               EL6511
01274                                                                   EL6511
01275       IF COMPSUBL GREATER ZERO                                    EL6511
01276           MOVE COMPSUBI          TO  RE-COMP-SUB.                 EL6511
01277                                                                   EL6511
01278  6010-CONT.                                                       EL6511
01279                                                                      CL*16
01280       IF PI-COMPANY-ID  =  'NCL' OR 'LGX'                            CL*16
01281          IF CUSTAMTL  GREATER  ZERO                                  CL*16
01282              MOVE WS-CUSTODIAL-BAL  TO  RE-CUSTODIAL-BAL.            CL*16
01283                                                                   EL6511
01284       IF REIGRPL GREATER ZERO                                     EL6511
01285           MOVE REIGRPI           TO  RE-REINS-GROUPING-CODE.      EL6511
01286                                                                   EL6511
01287       IF BCARL GREATER ZERO                                       EL6511
01288           MOVE BCARI             TO  RE-COMP-CARRIER-SECURITY.    EL6511
01289                                                                   EL6511
01290       IF CONAMEL GREATER ZERO                                     EL6511
01291           MOVE CONAMEI           TO  RE-NAME.                     EL6511
01292                                                                   EL6511
01293       IF CEDNAMEL GREATER ZERO                                    EL6511
01294           MOVE CEDNAMEI          TO  RE-CEDE-NAME.                EL6511
01295                                                                      CL**2
01296       IF CESTYPEL GREATER ZERO                                       CL**2
01297           MOVE CESTYPEI          TO  RE-CEDING-TYPE-FLAG.            CL**2
01298                                                                      CL*13
01299       IF GLCNTRL  GREATER ZERO                                       CL*13
01300          MOVE GLCNTRI            TO  RE-GL-CENTER.                   CL*13
01301                                                                   EL6511
01302       IF CMLIFEL GREATER ZERO                                     EL6511
01303           MOVE CMLIFEI           TO  RE-LF-PE.                    EL6511
01304                                                                   EL6511
01305       IF CMAHL GREATER ZERO                                       EL6511
01306           MOVE CMAHI             TO  RE-AH-PE.                    EL6511
01307                                                                   EL6511
01308       IF AHPRL GREATER ZERO                                       EL6511
01309           MOVE AHPRI             TO  RE-AH-PR-PCT.                EL6511
01310                                                                   EL6511
01311       IF AHR78L GREATER ZERO                                      EL6511
01312           MOVE AHR78I            TO  RE-AH-78-PCT.                EL6511
01313                                                                   EL6511
01314       IF LFPRL GREATER ZERO                                       EL6511
01315           MOVE LFPRI             TO  RE-LF-PR-PCT.                EL6511
01316                                                                   EL6511
01317       IF LFR78L GREATER ZERO                                      EL6511
01318           MOVE LFR78I            TO  RE-LF-78-PCT.                EL6511
01319                                                                   EL6511
01320       IF MORTCDL GREATER ZERO                                     EL6511
01321           MOVE MORTCDI           TO  RE-MORT-CODE.                EL6511
01322                                                                   EL6511
01323       IF MORTSWL GREATER ZERO                                     EL6511
01324           MOVE MORTSWI           TO  RE-MORT-SW.                  EL6511
01325                                                                   EL6511
01326       IF LFIBNRL GREATER ZERO                                     EL6511
01327           MOVE LFIBNRI           TO  RE-LF-IBNR-PCT.              EL6511
01328                                                                   EL6511
01329       IF AHIBNRL GREATER ZERO                                     EL6511
01330           MOVE AHIBNRI           TO  RE-AH-IBNR-PCT.              EL6511
01331                                                                   EL6511
01332       IF LIFEFEEL GREATER ZERO                                    EL6511
01333           MOVE LIFEFEEI          TO  RE-LF-FEE.                   EL6511
01334                                                                   EL6511
01335       IF AHFEEL GREATER ZERO                                      EL6511
01336           MOVE AHFEEI            TO  RE-AH-FEE.                   EL6511
01337                                                                   EL6511
01338       IF FEELIFEL GREATER ZERO                                    EL6511
01339           MOVE FEELIFEI          TO  RE-ZERO-LF-FEE.              EL6511
01340                                                                   EL6511
01341       IF FEEAHL GREATER ZERO                                      EL6511
01342           MOVE FEEAHI            TO  RE-ZERO-AH-FEE.              EL6511
01343                                                                   EL6511
01344       IF PRTAXL GREATER ZERO                                      EL6511
01345           MOVE PRTAXI            TO  RE-PRT-ST.                   EL6511
01346                                                                   EL6511
01347       IF COMLIFEL GREATER ZERO                                    EL6511
01348           MOVE COMLIFEI          TO  RE-LF-COMM.                  EL6511
01349                                                                   EL6511
01350       IF COMAHL GREATER ZERO                                      EL6511
01351           MOVE COMAHI            TO  RE-AH-COMM.                  EL6511
01352                                                                   EL6511
01353       IF PRTAXOWL GREATER ZERO                                    EL6511
01354           MOVE PRTAXOWI          TO  RE-PRT-OW.                   EL6511
01355                                                                      CL*11
01356       IF PRTCRSVL GREATER ZERO                                       CL*11
01357           MOVE PRTCRSVI          TO  RE-PRT-CRSV.                    CL*11
01358                                                                   EL6511
01359       IF RPTAL GREATER ZERO                                          CL**3
01360           MOVE RPTAI             TO  RE-CEDING-STMT-OPT-A.           CL**3
01361                                                                      CL**3
01362       IF RPTBL GREATER ZERO                                          CL**3
01363           MOVE RPTBI             TO  RE-CEDING-STMT-OPT-B.           CL**3
01364                                                                      CL**3
01365       IF RPTCL GREATER ZERO                                          CL**3
01366           MOVE RPTCI             TO  RE-CEDING-STMT-OPT-C.           CL**3
01367                                                                      CL**3
01368       IF RPTDL GREATER ZERO                                          CL**3
01369           MOVE RPTDI             TO  RE-CEDING-STMT-OPT-D.           CL**3
01370                                                                      CL**3
01371       IF RPTEL GREATER ZERO                                          CL**3
01372           MOVE RPTEI             TO  RE-CEDING-STMT-OPT-E.
110601
110601      IF RPTFL GREATER ZERO
110601          MOVE RPTFI             TO  RE-STATE-EXHIBIT-OPT-F.         CL**3
01373                                                                   EL6511
01374       IF CLAIML GREATER ZERO                                      EL6511
01375           MOVE CLAIMI            TO  RE-CLAIM-CODE.               EL6511
01376                                                                   EL6511
01377       IF TAXLIFEL GREATER ZERO                                    EL6511
01378           MOVE TAXLIFEI          TO  RE-LF-TAX.                   EL6511
01379                                                                   EL6511
01380       IF TAXAHL GREATER ZERO                                      EL6511
01381           MOVE TAXAHI            TO  RE-AH-TAX.                   EL6511
01382                                                                   EL6511
01383      IF LCLMPCTL GREATER THAN ZERO                                EL6511
01384         MOVE LCLMPCTI            TO  RE-LF-CLM-PCT.               EL6511
01385                                                                   EL6511
032707     IF EXTAXL > 0
032707        MOVE EXTAXI              TO  RE-EXCISE-TAX
032707     END-IF

01386      IF LCLMMAXL GREATER THAN ZERO                                EL6511
01387         MOVE WS-LF-CLM-MAX       TO  RE-LF-CLM-MAX.               EL6511
01388                                                                   EL6511
01389      IF ACLMPCTL GREATER THAN ZERO                                EL6511
01390         MOVE ACLMPCTI            TO  RE-AH-CLM-PCT.               EL6511
01391                                                                   EL6511
01392      IF ACLMMAXL GREATER THAN ZERO                                EL6511
01393         MOVE WS-AH-CLM-MAX       TO  RE-AH-CLM-MAX.               EL6511
01394                                                                   EL6511
01395      IF LFMETHL GREATER THAN ZERO                                 EL6511
01396         MOVE LFMETHI             TO  RE-LF-FEE-METHOD.            EL6511
01397                                                                   EL6511
01398      IF LFBASISL GREATER THAN ZERO                                EL6511
01399         MOVE LFBASISI            TO  RE-LF-FEE-BASIS.             EL6511
01400                                                                   EL6511
01401      IF AHMETHL GREATER THAN ZERO                                 EL6511
01402         MOVE AHMETHI             TO  RE-AH-FEE-METHOD.            EL6511
01403                                                                   EL6511
01404      IF AHBASISL GREATER THAN ZERO                                EL6511
01405         MOVE AHBASISI            TO  RE-AH-FEE-BASIS.             EL6511
01406                                                                   EL6511
01407      IF LFPCT1L GREATER THAN ZERO                                    CL**7
01408         MOVE LFPCT1I             TO  RE-LF-FEE-RANGE-PCT (1).     EL6511
01409                                                                   EL6511
01410      IF LFPCT2L GREATER THAN ZERO                                 EL6511
01411         MOVE LFPCT2I             TO  RE-LF-FEE-RANGE-PCT (2).     EL6511
01412                                                                   EL6511
01413      IF LFPCT3L GREATER THAN ZERO                                 EL6511
01414         MOVE LFPCT3I             TO  RE-LF-FEE-RANGE-PCT (3).     EL6511
01415                                                                   EL6511
01416      IF LFPCT4L GREATER THAN ZERO                                 EL6511
01417         MOVE LFPCT4I             TO  RE-LF-FEE-RANGE-PCT (4).     EL6511
01418                                                                   EL6511
01419      IF LFPCT5L GREATER THAN ZERO                                 EL6511
01420         MOVE LFPCT5I             TO  RE-LF-FEE-RANGE-PCT (5).     EL6511
01421                                                                   EL6511
01422      IF LFPCT6L GREATER THAN ZERO                                 EL6511
01423         MOVE LFPCT6I             TO  RE-LF-FEE-RANGE-PCT (6).     EL6511
01424                                                                   EL6511
01425      IF LFTHRU1L GREATER THAN ZERO                                EL6511
01426         MOVE WS-LF-THRU1         TO  RE-LF-FEE-THRU-AMT (1).      EL6511
01427                                                                   EL6511
01428      IF LFTHRU2L GREATER THAN ZERO                                EL6511
01429         MOVE WS-LF-THRU2         TO  RE-LF-FEE-THRU-AMT (2).      EL6511
01430                                                                   EL6511
01431      IF LFTHRU3L GREATER THAN ZERO                                EL6511
01432         MOVE WS-LF-THRU3         TO  RE-LF-FEE-THRU-AMT (3).      EL6511
01433                                                                   EL6511
01434      IF LFTHRU4L GREATER THAN ZERO                                EL6511
01435         MOVE WS-LF-THRU4         TO  RE-LF-FEE-THRU-AMT (4).      EL6511
01436                                                                   EL6511
01437      IF LFTHRU5L GREATER THAN ZERO                                EL6511
01438         MOVE WS-LF-THRU5         TO  RE-LF-FEE-THRU-AMT (5).      EL6511
01439                                                                   EL6511
01440      IF LFTHRU6L GREATER THAN ZERO                                EL6511
01441         MOVE WS-LF-THRU6         TO  RE-LF-FEE-THRU-AMT (6).      EL6511
01442                                                                   EL6511
01443      IF AHPCT1L GREATER THAN ZERO                                 EL6511
01444         MOVE AHPCT1I             TO  RE-AH-FEE-RANGE-PCT (1).     EL6511
01445                                                                   EL6511
01446      IF AHPCT2L GREATER THAN ZERO                                 EL6511
01447         MOVE AHPCT2I             TO  RE-AH-FEE-RANGE-PCT (2).     EL6511
01448                                                                   EL6511
01449      IF AHPCT3L GREATER THAN ZERO                                 EL6511
01450         MOVE AHPCT3I             TO  RE-AH-FEE-RANGE-PCT (3).     EL6511
01451                                                                   EL6511
01452      IF AHPCT4L GREATER THAN ZERO                                 EL6511
01453         MOVE AHPCT4I             TO  RE-AH-FEE-RANGE-PCT (4).     EL6511
01454                                                                   EL6511
01455      IF AHPCT5L GREATER THAN ZERO                                 EL6511
01456         MOVE AHPCT5I             TO  RE-AH-FEE-RANGE-PCT (5).     EL6511
01457                                                                   EL6511
01458      IF AHPCT6L GREATER THAN ZERO                                 EL6511
01459         MOVE AHPCT6I             TO  RE-AH-FEE-RANGE-PCT (6).     EL6511
01460                                                                   EL6511
01461      IF AHTHRU1L GREATER THAN ZERO                                EL6511
01462         MOVE WS-AH-THRU1         TO  RE-AH-FEE-THRU-AMT (1).      EL6511
01463                                                                   EL6511
01464      IF AHTHRU2L GREATER THAN ZERO                                EL6511
01465         MOVE WS-AH-THRU2         TO  RE-AH-FEE-THRU-AMT (2).      EL6511
01466                                                                   EL6511
01467      IF AHTHRU3L GREATER THAN ZERO                                EL6511
01468         MOVE WS-AH-THRU3         TO  RE-AH-FEE-THRU-AMT (3).      EL6511
01469                                                                   EL6511
01470      IF AHTHRU4L GREATER THAN ZERO                                EL6511
01471         MOVE WS-AH-THRU4         TO  RE-AH-FEE-THRU-AMT (4).      EL6511
01472                                                                   EL6511
01473      IF AHTHRU5L GREATER THAN ZERO                                EL6511
01474         MOVE WS-AH-THRU5         TO  RE-AH-FEE-THRU-AMT (5).      EL6511
01475                                                                   EL6511
01476      IF AHTHRU6L GREATER THAN ZERO                                EL6511
01477         MOVE WS-AH-THRU6         TO  RE-AH-FEE-THRU-AMT (6).      EL6511
01478                                                                   EL6511
01479      IF CLINCDTL GREATER ZERO                                        CL*17
01480          MOVE WS-INC-DATE        TO  RE-CLM-INCURRED-LIM.            CL*17
01481                                                                   EL6511
01482      IF ERBEGDTL GREATER ZERO                                        CL*17
01483          MOVE WS-ERN-START       TO  RE-EARNING-START-DT.            CL*17
01484                                                                      CL*15
01485      IF ERENDDTL GREATER ZERO                                        CL*17
01486          MOVE WS-ERN-STOP        TO  RE-EARNING-STOP-DT.             CL*17
01487                                                                      CL*17
01488      IF ERENDCDL GREATER THAN ZERO                                   CL*17
01489         MOVE ERENDCDI            TO  RE-EARN-STOP-CODE.              CL*17
01490                                                                      CL*17
01491      MOVE 'B'                    TO  RE-CODE.                        CL*17
01492                                                                   EL6511
01493  6000-EXIT.                                                       EL6511
01494      EXIT.                                                        EL6511
01495      EJECT                                                        EL6511
01496  6200-CHECK-FOR-UPDATE.                                           EL6511
01497      MOVE +0                     TO  SUB1.                        EL6511
01498      SET DO-INDX                 TO  +1.                          EL6511
01499      SET DO-INDX DOWN BY +1.                                      EL6511
01500                                                                   EL6511
01501  6225-CONT.                                                       EL6511
01502      SET DO-INDX UP BY +1.                                        EL6511
01503      ADD +1                      TO  SUB1.                        EL6511
01504                                                                   EL6511
01505      IF DO-INDX GREATER +18                                       EL6511
01506          GO TO 6200-EXIT.                                         EL6511
01507                                                                   EL6511
01508      MOVE DESCO (DO-INDX)        TO  RE-DESC (SUB1).              EL6511
01509                                                                   EL6511
01510      GO TO 6225-CONT.                                             EL6511
01511                                                                   EL6511
01512  6200-EXIT.                                                       EL6511
01513      EXIT.                                                        EL6511
01514      EJECT                                                        EL6511
01515  6400-EDIT.                                                       EL6511
01516      IF PI-CARRIER-SECURITY GREATER SPACE                         EL6511
01517          MOVE PI-CARRIER-SECURITY    TO  BCARI                    EL6511
01518          MOVE AL-SANON               TO  BCARA.                   EL6511
01519                                                                   EL6511
01520      IF REIGRPL GREATER THAN ZERO                                 EL6511
01521          MOVE AL-UANON           TO  REIGRPA.                     EL6511
01522                                                                      CL*10
01523      IF ADD-FUNCTION OR CHANGE-FUNCTION                              CL*10
01524          IF (COMPANYI EQUAL ZEROS) AND                               CL*10
01525                (COMPSUBI EQUAL ZEROS)                                CL*10
01526                  MOVE -1         TO  COMPANYL                        CL*10
01527                  MOVE AL-UABON   TO  COMPANYA                        CL*10
01528                                      COMPSUBA                        CL*10
01529                  MOVE ER-7805    TO  EMI-ERROR                       CL*10
01530                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL*10
01531                                                                   EL6511
01532      IF BCARL GREATER ZERO                                        EL6511
01533          MOVE BCARI              TO  WS-CARRIER                   EL6511
01534          PERFORM 7700-CHECK-CARRIER THRU 7700-EXIT                EL6511
01535          IF CARRIER-FOUND                                         EL6511
01536              MOVE AL-UANON       TO  BCARA                        EL6511
01537          ELSE                                                     EL6511
01538              IF BCARI = SPACES                                    EL6511
01539                  MOVE AL-UANON   TO  BCARA                        EL6511
01540              ELSE                                                 EL6511
01541                  MOVE -1         TO  BCARL                        EL6511
01542                  MOVE AL-UABON   TO  BCARA                        EL6511
01543                  MOVE ER-2208    TO  EMI-ERROR                    EL6511
01544                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL6511
01545                                                                   EL6511
01546      IF CONAMEL GREATER ZERO                                      EL6511
01547          MOVE AL-UANON           TO  CONAMEA.                     EL6511
01548                                                                      CL**2
01549      IF CESTYPEL GREATER ZERO                                        CL**2
01550          MOVE CESTYPEI        TO  WS-CESSION-TYPE                    CL**2
01551          IF VALID-CESSION-TYPE                                       CL**2
01552              MOVE AL-UANON    TO  CESTYPEA                           CL**2
01553          ELSE                                                        CL**2
01554              MOVE AL-UABON    TO  CESTYPEA                           CL**2
01555              MOVE -1          TO  CESTYPEL                           CL**2
01556              MOVE ER-0589     TO  EMI-ERROR                          CL**2
01557              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**2
01558                                                                   EL6511
01559      IF CMLIFEL GREATER ZERO                                      EL6511
01560          MOVE CMLIFEI            TO  WS-CHECK-METHOD              EL6511
01561          IF VALID-LIFE-METHOD                                     EL6511
01562              MOVE AL-UANON       TO  CMLIFEA                      EL6511
01563          ELSE                                                     EL6511
01564              MOVE -1             TO  CMLIFEL                      EL6511
01565              MOVE AL-UABON       TO  CMLIFEA                      EL6511
01566              MOVE ER-2143        TO  EMI-ERROR                    EL6511
01567              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01568                                                                   EL6511
01569      MOVE +1                     TO  CMAHL.                          CL**6
01570      IF CMAHL GREATER ZERO                                        EL6511
01571          MOVE CMAHI              TO  WS-CHECK-METHOD              EL6511
01572          IF VALID-AH-METHOD                                       EL6511
01573              MOVE AL-UANON       TO  CMAHA                        EL6511
01574          ELSE                                                     EL6511
01575              MOVE -1             TO  CMAHL                        EL6511
01576              MOVE AL-UABON       TO  CMAHA                        EL6511
01577              MOVE ER-2144        TO  EMI-ERROR                    EL6511
01578              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01579                                                                   EL6511
01580      IF LIFEFEEL GREATER ZERO                                     EL6511
01581          IF LIFEFEEI NUMERIC                                      EL6511
01582              MOVE AL-UANON       TO  LIFEFEEA                     EL6511
01583          ELSE                                                     EL6511
01584              MOVE -1             TO  LIFEFEEL                     EL6511
01585              MOVE AL-UABON       TO  LIFEFEEA                     EL6511
01586              MOVE ER-2147        TO  EMI-ERROR                    EL6511
01587              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01588                                                                   EL6511
01589      IF AHFEEL GREATER ZERO                                       EL6511
01590          IF AHFEEI NUMERIC                                        EL6511
01591              MOVE AL-UANON       TO  AHFEEA                       EL6511
01592          ELSE                                                     EL6511
01593              MOVE -1             TO  AHFEEL                       EL6511
01594              MOVE AL-UABON       TO  AHFEEA                       EL6511
01595              MOVE ER-2148        TO  EMI-ERROR                    EL6511
01596              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01597                                                                   EL6511
01598      MOVE +1                     TO LFPRL  LFR78L                    CL**4
01599                                     AHPRL  AHR78L.                   CL**4
01600                                                                   EL6511
01601      IF LFPRL GREATER ZERO                                        EL6511
01602          IF LFPRI NUMERIC                                         EL6511
01603              MOVE AL-UNNON       TO  LFPRA                           CL**4
01604              ADD LFPRI           TO  WS-TOT-PERCENT                  CL**4
01605          ELSE                                                     EL6511
01606              MOVE -1             TO  LFPRL                        EL6511
01607              MOVE AL-UABON       TO  LFPRA                        EL6511
01608              MOVE ER-2145        TO  EMI-ERROR                    EL6511
01609              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01610                                                                   EL6511
01611      IF LFR78L GREATER ZERO                                       EL6511
01612          IF LFR78I NUMERIC                                        EL6511
01613              MOVE AL-UNNON       TO  LFR78A                          CL**4
01614              ADD LFR78I          TO  WS-TOT-PERCENT                  CL**4
01615          ELSE                                                     EL6511
01616              MOVE -1             TO  LFR78L                       EL6511
01617              MOVE AL-UABON       TO  LFR78A                       EL6511
01618              MOVE ER-2145        TO  EMI-ERROR                       CL**4
01619              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**4
01620                                                                      CL**4
01621      IF WS-TOT-PERCENT NOT = +0  AND                                 CL**4
01622         WS-TOT-PERCENT NOT = +1                                      CL**4
01623          MOVE -1                 TO  LFPRL                           CL**4
01624          MOVE AL-UNBON           TO  LFPRA                           CL**4
01625                                      LFR78A                          CL**4
01626          MOVE ER-2308            TO  EMI-ERROR                       CL**4
01627          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**4
01628                                                                      CL**4
01629      MOVE ZEROS                  TO WS-TOT-PERCENT.                  CL**4
01630                                                                      CL**4
01631      IF AHPRL GREATER ZERO                                           CL**4
01632          IF AHPRI NUMERIC                                            CL**4
01633              MOVE AL-UNNON       TO  AHPRA                           CL**4
01634              ADD AHPRI           TO  WS-TOT-PERCENT                  CL**4
01635          ELSE                                                        CL**4
01636              MOVE -1             TO  AHPRL                           CL**4
01637              MOVE AL-UABON       TO  AHPRA                           CL**4
01638              MOVE ER-2146        TO  EMI-ERROR                    EL6511
01639              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01640                                                                      CL**4
01641      IF AHR78L GREATER ZERO                                          CL**4
01642          IF AHR78I NUMERIC                                           CL**4
01643              MOVE AL-UNNON       TO  AHR78A                          CL**4
01644              ADD AHR78I          TO  WS-TOT-PERCENT                  CL**4
01645          ELSE                                                        CL**4
01646              MOVE -1             TO  AHR78L                          CL**4
01647              MOVE AL-UABON       TO  AHR78A                          CL**4
01648              MOVE ER-2146        TO  EMI-ERROR                       CL**4
01649              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**4
01650                                                                      CL**4
01651      IF WS-CHECK-METHOD = 'P'  OR  'E'                               CL**6
01652          IF WS-TOT-PERCENT NOT = +1                                  CL**6
01653              MOVE -1             TO  AHPRL                           CL**6
01654              MOVE AL-UNBON       TO  AHPRA                           CL**6
01655                                      AHR78A                          CL**4
01656              MOVE ER-2308        TO  EMI-ERROR                       CL**6
01657              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**6
01658                                                                   EL6511
01659      IF PRTAXL GREATER ZERO                                       EL6511
01660          MOVE PRTAXI             TO  WS-CHECK-TAX-OPTION          EL6511
01661          IF VALID-TAX-OPTION                                      EL6511
01662              MOVE AL-UANON       TO  PRTAXA                       EL6511
01663          ELSE                                                     EL6511
01664              MOVE -1             TO  PRTAXL                       EL6511
01665              MOVE AL-UABON       TO  PRTAXA                       EL6511
01666              MOVE ER-2301        TO  EMI-ERROR                    EL6511
01667              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01668                                                                   EL6511
01669      IF PRTAXOWL GREATER ZERO                                     EL6511
01670          MOVE PRTAXOWI           TO  WS-CHECK-PRINT-OPT              CL*11
01671          IF VALID-PRINT-OPT                                          CL*11
01672              MOVE AL-UANON       TO  PRTAXOWA                     EL6511
01673          ELSE                                                     EL6511
01674              MOVE -1             TO  PRTAXOWL                     EL6511
01675              MOVE AL-UABON       TO  PRTAXOWA                     EL6511
01676              MOVE ER-2306        TO  EMI-ERROR                       CL*11
01677              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*11
01678                                                                      CL*11
01679      IF PRTCRSVL GREATER ZERO                                        CL*11
01680          MOVE PRTCRSVI           TO  WS-CHECK-PRINT-OPT              CL*11
01681          IF VALID-PRINT-OPT                                          CL*11
01682              MOVE AL-UANON       TO  PRTCRSVA                        CL*11
01683          ELSE                                                        CL*11
01684              MOVE -1             TO  PRTCRSVL                        CL*11
01685              MOVE AL-UABON       TO  PRTCRSVA                        CL*11
01686              MOVE ER-2306        TO  EMI-ERROR                    EL6511
01687              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01688                                                                   EL6511
01689      IF RPTAL GREATER ZERO                                           CL**3
01690          MOVE RPTAI              TO  WS-CHECK-RPT-OPTION             CL**3
01691          IF VALID-REPORT-OPTION                                   EL6511
01692              MOVE AL-UANON       TO  RPTAA                           CL**3
01693          ELSE                                                     EL6511
01694              MOVE -1             TO  RPTAL                           CL**3
01695              MOVE AL-UABON       TO  RPTAA                           CL**3
01696              MOVE ER-2391        TO  EMI-ERROR                       CL**3
01697              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**3
01698                                                                      CL**3
01699      IF RPTBL GREATER ZERO                                           CL**3
01700          MOVE RPTBI              TO  WS-CHECK-RPT-OPTION             CL**3
01701          IF VALID-REPORT-OPTION                                      CL**3
01702              MOVE AL-UANON       TO  RPTBA                           CL**3
01703          ELSE                                                        CL**3
01704              MOVE -1             TO  RPTBL                           CL**3
01705              MOVE AL-UABON       TO  RPTBA                           CL**3
01706              MOVE ER-2391        TO  EMI-ERROR                       CL**3
01707              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**3
01708                                                                      CL**3
01709      IF RPTCL GREATER ZERO                                           CL**3
01710          MOVE RPTCI              TO  WS-CHECK-RPT-OPTION             CL**3
01711          IF VALID-REPORT-OPTION                                      CL**3
01712              MOVE AL-UANON       TO  RPTCA                           CL**3
01713          ELSE                                                        CL**3
01714              MOVE -1             TO  RPTCL                           CL**3
01715              MOVE AL-UABON       TO  RPTCA                           CL**3
01716              MOVE ER-2391        TO  EMI-ERROR                       CL**3
01717              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**3
01718                                                                      CL**3
01719      IF RPTDL GREATER ZERO                                           CL**3
01720          MOVE RPTDI              TO  WS-CHECK-RPT-OPTION             CL**3
01721          IF VALID-REPORT-OPTION                                      CL**3
01722              MOVE AL-UANON       TO  RPTDA                           CL**3
01723          ELSE                                                        CL**3
01724              MOVE -1             TO  RPTDL                           CL**3
01725              MOVE AL-UABON       TO  RPTDA                           CL**3
01726              MOVE ER-2391        TO  EMI-ERROR                       CL**3
01727              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**3
01728                                                                      CL**3
01729      IF RPTEL GREATER ZERO                                           CL**3
01730          MOVE RPTEI              TO  WS-CHECK-RPT-OPTION             CL**3
01731          IF VALID-REPORT-OPTION                                      CL**3
01732              MOVE AL-UANON       TO  RPTEA                           CL**3
01733          ELSE                                                        CL**3
01734              MOVE -1             TO  RPTEL                           CL**3
01735              MOVE AL-UABON       TO  RPTEA                           CL**3
01736              MOVE ER-2391        TO  EMI-ERROR                    EL6511
01737              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
110601
110601     IF RPTFL GREATER ZERO
110601         MOVE RPTFI              TO  WS-CHECK-RPT-OPTION
110601         IF VALID-REPORT-OPTION
110601             MOVE AL-UANON       TO  RPTFA
110601         ELSE
110601             MOVE -1             TO  RPTFL
110601             MOVE AL-UABON       TO  RPTFA
110601             MOVE ER-2391        TO  EMI-ERROR
110601             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01738                                                                   EL6511
01739      IF MORTCDL GREATER ZERO                                      EL6511
01740          PERFORM 7100-EDIT-MORTALITY THRU 7199-EXIT.              EL6511
01741                                                                   EL6511
01742      IF MORTSWL GREATER ZERO                                      EL6511
01743          IF MORTSWI = 'Y' OR 'N' OR ' '                           EL6511
01744              MOVE AL-UANON       TO  MORTSWA                      EL6511
01745          ELSE                                                     EL6511
01746              MOVE -1             TO  MORTSWL                      EL6511
01747              MOVE AL-UABON       TO  MORTSWA                      EL6511
01748              MOVE ER-7098        TO  EMI-ERROR                    EL6511
01749              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01750                                                                   EL6511
01751      IF LFIBNRL GREATER ZERO                                      EL6511
01752          IF LFIBNRI NUMERIC                                       EL6511
01753              MOVE AL-UNNON       TO  LFIBNRA                      EL6511
01754          ELSE                                                     EL6511
01755              MOVE -1             TO  LFIBNRL                      EL6511
01756              MOVE AL-UNBON       TO  LFIBNRA                      EL6511
01757              MOVE ER-2314        TO  EMI-ERROR                    EL6511
01758              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01759                                                                   EL6511
01760      IF AHIBNRL GREATER ZERO                                      EL6511
01761          IF AHIBNRI NUMERIC                                       EL6511
01762              MOVE AL-UNNON       TO  AHIBNRA                      EL6511
01763          ELSE                                                     EL6511
01764              MOVE -1             TO  AHIBNRL                      EL6511
01765              MOVE AL-UNBON       TO  AHIBNRA                      EL6511
01766              MOVE ER-2315        TO  EMI-ERROR                    EL6511
01767              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01768                                                                   EL6511
01769      IF CLAIML GREATER ZERO                                       EL6511
01770          MOVE CLAIMI             TO  WS-CHECK-CLAIM               EL6511
01771          IF VALID-CLAIM                                           EL6511
01772              MOVE AL-UANON       TO  CLAIMA                       EL6511
01773          ELSE                                                     EL6511
01774              MOVE -1             TO  CLAIML                       EL6511
01775              MOVE AL-UABON       TO  CLAIMA                       EL6511
01776              MOVE ER-2307        TO  EMI-ERROR                    EL6511
01777              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01778                                                                   EL6511
01779      IF FEELIFEL GREATER ZERO                                     EL6511
01780          MOVE FEELIFEI           TO  WS-CHECK-ZERO-FEE            EL6511
01781          IF VALID-ZERO-FEE                                        EL6511
01782              MOVE AL-UANON       TO  FEELIFEA                     EL6511
01783          ELSE                                                     EL6511
01784              MOVE -1             TO  FEELIFEL                     EL6511
01785              MOVE AL-UABON       TO  FEELIFEA                     EL6511
01786              MOVE ER-2149        TO  EMI-ERROR                    EL6511
01787              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01788                                                                   EL6511
01789      IF FEEAHL GREATER ZERO                                       EL6511
01790          MOVE FEEAHI             TO  WS-CHECK-ZERO-FEE            EL6511
01791          IF VALID-ZERO-FEE                                        EL6511
01792              MOVE AL-UANON       TO  FEEAHA                       EL6511
01793          ELSE                                                     EL6511
01794              MOVE -1             TO  FEEAHL                       EL6511
01795              MOVE AL-UABON       TO  FEEAHA                       EL6511
01796              MOVE ER-2309        TO  EMI-ERROR                    EL6511
01797              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01798                                                                   EL6511
01799      IF COMLIFEL GREATER ZERO                                     EL6511
01800          MOVE COMLIFEI           TO  WS-CHECK-COMMISSION          EL6511
01801          IF VALID-COMMISSION                                      EL6511
01802              MOVE AL-UANON       TO  COMLIFEA                     EL6511
01803          ELSE                                                     EL6511
01804              MOVE -1             TO  COMLIFEL                     EL6511
01805              MOVE AL-UABON       TO  COMLIFEA                     EL6511
01806              MOVE ER-2304        TO  EMI-ERROR                    EL6511
01807              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01808                                                                   EL6511
01809      IF COMAHL GREATER ZERO                                       EL6511
01810          MOVE COMAHI             TO  WS-CHECK-COMMISSION          EL6511
01811          IF VALID-COMMISSION                                      EL6511
01812              MOVE AL-UANON       TO  COMAHA                       EL6511
01813          ELSE                                                     EL6511
01814              MOVE -1             TO  COMAHL                       EL6511
01815              MOVE AL-UABON       TO  COMAHA                       EL6511
01816              MOVE ER-2305        TO  EMI-ERROR                    EL6511
01817              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01818                                                                   EL6511
01819      IF TAXLIFEL GREATER ZERO                                     EL6511
01820          MOVE TAXLIFEI           TO  WS-CHECK-TAX                 EL6511
01821          IF VALID-TAX                                             EL6511
01822              MOVE AL-UANON       TO  TAXLIFEA                     EL6511
01823          ELSE                                                     EL6511
01824              MOVE -1             TO  TAXLIFEL                     EL6511
01825              MOVE AL-UABON       TO  TAXLIFEA                     EL6511
01826              MOVE ER-2302        TO  EMI-ERROR                    EL6511
01827              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01828                                                                   EL6511
01829      IF TAXAHL GREATER ZERO                                       EL6511
01830          MOVE TAXAHI             TO  WS-CHECK-TAX                 EL6511
01831          IF VALID-TAX                                             EL6511
01832              MOVE AL-UANON       TO  TAXAHA                       EL6511
01833          ELSE                                                     EL6511
01834              MOVE -1             TO  TAXAHL                       EL6511
01835              MOVE AL-UABON       TO  TAXAHA                       EL6511
01836              MOVE ER-2303        TO  EMI-ERROR                    EL6511
01837              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01838                                                                   EL6511
01839      IF LCLMPCTL GREATER ZERO                                     EL6511
01840          IF LCLMPCTI NUMERIC                                      EL6511
01841              IF LCLMPCTI GREATER +1                               EL6511
01842                  MOVE -1         TO  LCLMPCTL                     EL6511
01843                  MOVE AL-UNBON   TO  LCLMPCTA                     EL6511
01844                  MOVE ER-0649    TO  EMI-ERROR                    EL6511
01845                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6511
01846              ELSE                                                 EL6511
01847                  MOVE AL-UNNON   TO  LCLMPCTA                     EL6511
01848          ELSE                                                     EL6511
01849              MOVE -1             TO  LCLMPCTL                     EL6511
01850              MOVE AL-UNBON       TO  LCLMPCTA                     EL6511
01851              MOVE ER-0648        TO  EMI-ERROR                    EL6511
01852              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01853                                                                      CL*16
032707     IF EXTAXL > 0
032707        IF EXTAXI NUMERIC
032707           IF EXTAXI > +1
032707              MOVE -1            TO EXTAXL
032707              MOVE AL-UNBON      TO EXTAXA
032707              MOVE ER-0875       TO EMI-ERROR
032707              PERFORM 9900-ERROR-FORMAT
032707                                 THRU 9900-EXIT
032707           ELSE
032707              MOVE AL-UNNON      TO EXTAXA
032707           END-IF
032707        ELSE
032707           MOVE -1               TO EXTAXL
032707           MOVE AL-UNBON         TO EXTAXA
032707           MOVE ER-0875          TO EMI-ERROR
032707           PERFORM 9900-ERROR-FORMAT
032707                                 THRU 9900-EXIT
032707        END-IF
032707     END-IF
01853                                                                      CL*16
01854      IF PI-COMPANY-ID  =  'NCL' OR 'LGX'                             CL*16
01855         IF CUSTAMTL GREATER ZERO                                     CL*16
01856             MOVE CUSTAMTI        TO  DEEDIT-FIELD                    CL*16
01857             PERFORM 7450-DEEDIT THRU 7450-EXIT                       CL*16
01858             IF DEEDIT-FIELD-V0 IS NOT GREATER +999999999             CL*16
01859                 MOVE DEEDIT-FIELD-V1 TO  CUSTAMTO                    CL*16
01860                                          WS-CUSTODIAL-BAL            CL*16
01861                 MOVE AL-UNNON        TO  CUSTAMTA                    CL*16
01862             ELSE                                                     CL*16
01863                 MOVE -1              TO  CUSTAMTL                    CL*16
01864                 MOVE AL-UNBON        TO  CUSTAMTA                    CL*16
01865                 MOVE ER-0763         TO  EMI-ERROR                   CL*16
01866                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            CL*16
01867                                                                   EL6511
01868      IF LCLMMAXL GREATER ZERO                                     EL6511
01869          MOVE LCLMMAXI           TO  DEEDIT-FIELD                 EL6511
01870          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
01871          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
01872              MOVE DEEDIT-FIELD-V1    TO  LCLMMAXO                 EL6511
01873                                          WS-LF-CLM-MAX            EL6511
01874              MOVE AL-UNNON           TO  LCLMMAXA                 EL6511
01875          ELSE                                                     EL6511
01876              MOVE -1             TO  LCLMMAXL                     EL6511
01877              MOVE AL-UNBON       TO  LCLMMAXA                     EL6511
01878              MOVE ER-0650        TO  EMI-ERROR                    EL6511
01879              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01880                                                                   EL6511
01881      IF ACLMPCTL GREATER ZERO                                     EL6511
01882          IF ACLMPCTI NUMERIC                                      EL6511
01883              IF ACLMPCTI GREATER +1                               EL6511
01884                  MOVE -1         TO  ACLMPCTL                     EL6511
01885                  MOVE AL-UNBON   TO  ACLMPCTA                     EL6511
01886                  MOVE ER-0652    TO  EMI-ERROR                    EL6511
01887                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6511
01888              ELSE                                                 EL6511
01889                  MOVE AL-UNNON   TO  ACLMPCTA                     EL6511
01890          ELSE                                                     EL6511
01891              MOVE -1             TO  ACLMPCTL                     EL6511
01892              MOVE AL-UNBON       TO  ACLMPCTA                     EL6511
01893              MOVE ER-0651        TO  EMI-ERROR                    EL6511
01894              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
01895                                                                   EL6511
01896      IF ACLMMAXL GREATER ZERO                                     EL6511
01897          MOVE ACLMMAXI           TO  DEEDIT-FIELD                 EL6511
01898          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
01899          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
01900              MOVE DEEDIT-FIELD-V1    TO  ACLMMAXO                 EL6511
01901                                          WS-AH-CLM-MAX            EL6511
01902              MOVE AL-UNNON           TO  ACLMMAXA                 EL6511
01903          ELSE                                                     EL6511
01904              MOVE -1             TO  ACLMMAXL                     EL6511
01905               MOVE AL-UNBON      TO  ACLMMAXA                     EL6511
01906               MOVE ER-0653       TO  EMI-ERROR                    EL6511
01907               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           EL6511
01908                                                                   EL6511
01909      IF CLINCDTL GREATER ZERO                                     EL6511
01910          MOVE CLINCDTI           TO  DEEDIT-FIELD                 EL6511
01911          EXEC CICS BIF                                            EL6511
01912              DEEDIT                                               EL6511
01913              FIELD  (DEEDIT-FIELD)                                EL6511
01914              LENGTH (15)                                          EL6511
01915              END-EXEC                                             EL6511
01916          IF DEEDIT-FIELD-V0 GREATER ZERO                          EL6511
01917              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY       EL6511
01918              MOVE '4'                TO  DC-OPTION-CODE           EL6511
01919              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL6511
01920              IF NO-CONVERSION-ERROR                               EL6511
01921                  MOVE AL-UANON               TO  CLINCDTA         EL6511
01922 **               MOVE DC-GREG-DATE-1-YMD     TO  WS-INC-DATE      EL6511
LGC192                 MOVE DC-GREG-DATE-CYMD      TO  WS-INC-DATE      EL6511
01923                  MOVE DC-GREG-DATE-1-MDY     TO  CLINCDTO            CL*15
01924              ELSE                                                 EL6511
01925                  MOVE -1         TO  CLINCDTL                     EL6511
01926                  MOVE AL-UABON   TO  CLINCDTA                     EL6511
01927                  MOVE ER-2312    TO  EMI-ERROR                    EL6511
01928                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6511
01929          ELSE                                                     EL6511
01930              MOVE ZEROS          TO  WS-INC-DATE                  EL6511
01931              MOVE AL-UANON       TO  CLINCDTA.                    EL6511
01932                                                                      CL*15
01933      IF ERBEGDTL GREATER ZERO                                        CL*17
01934          MOVE ERBEGDTI           TO  DEEDIT-FIELD                    CL*17
01935          EXEC CICS BIF                                               CL*15
01936              DEEDIT                                                  CL*15
01937              FIELD  (DEEDIT-FIELD)                                   CL*15
01938              LENGTH (15)                                             CL*15
01939              END-EXEC                                                CL*15
01940          IF DEEDIT-FIELD-V0 GREATER ZERO                             CL*15
01941              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY          CL*15
01942              MOVE '4'                TO  DC-OPTION-CODE              CL*15
01943              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*15
01944              IF NO-CONVERSION-ERROR                                  CL*15
01945                  MOVE AL-UANON               TO  ERBEGDTA            CL*17
01946 **               MOVE DC-GREG-DATE-1-YMD     TO  WS-ERN-START        CL*17
LGC192                 MOVE DC-GREG-DATE-CYMD      TO  WS-ERN-START        CL*17
01947                  MOVE DC-GREG-DATE-1-MDY     TO  ERBEGDTO            CL*17
01948              ELSE                                                    CL*15
01949                  MOVE -1         TO  ERBEGDTL                        CL*17
01950                  MOVE AL-UABON   TO  ERBEGDTA                        CL*17
01951                  MOVE ER-2312    TO  EMI-ERROR                       CL*15
01952                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*15
01953          ELSE                                                        CL*15
01954              MOVE ZEROS          TO  WS-ERN-START                    CL*17
01955              MOVE AL-UANON       TO  ERBEGDTA.                       CL*17
01956                                                                      CL*17
01957      IF ERENDDTL GREATER ZERO                                        CL*17
01958          IF ERENDDTI = '999999'                                      CL*17
01959              MOVE ZEROS          TO  ERENDDTI.                       CL*17
01960                                                                      CL*17
01961      IF ERENDDTL GREATER ZERO                                        CL*17
01962          MOVE ERENDDTI           TO  DEEDIT-FIELD                    CL*17
01963          EXEC CICS BIF                                               CL*17
01964              DEEDIT                                                  CL*17
01965              FIELD  (DEEDIT-FIELD)                                   CL*17
01966              LENGTH (15)                                             CL*17
01967              END-EXEC                                                CL*17
01968          IF DEEDIT-FIELD-V0 GREATER ZERO                             CL*17
01969              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY          CL*17
01970              MOVE '4'                TO  DC-OPTION-CODE              CL*17
01971              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*17
01972              IF NO-CONVERSION-ERROR                                  CL*17
01973                  MOVE AL-UANON               TO  ERENDDTA            CL*17
01974 **               MOVE DC-GREG-DATE-1-YMD     TO  WS-ERN-STOP         CL*17
LGC192                 MOVE DC-GREG-DATE-CYMD      TO  WS-ERN-STOP         CL*17
01975                  MOVE DC-GREG-DATE-1-MDY     TO  ERENDDTO            CL*17
01976              ELSE                                                    CL*17
01977                  MOVE -1         TO  ERENDDTL                        CL*17
01978                  MOVE AL-UABON   TO  ERENDDTA                        CL*17
01979                  MOVE ER-2313    TO  EMI-ERROR                       CL*17
01980                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*17
01981          ELSE                                                        CL*17
01982              MOVE ZEROS          TO  WS-ERN-STOP                     CL*17
01983              MOVE AL-UANON       TO  ERENDDTA.                       CL*17
01984                                                                      CL*17
01985      IF ERENDCDL GREATER ZERO                                        CL*17
01986          IF ERENDCDI = 'A' OR 'B' OR 'L' OR ' '                      CL*17
01987              MOVE AL-UANON       TO  ERENDCDA                        CL*17
01988          ELSE                                                        CL*17
01990              MOVE AL-UABON       TO  ERENDCDA                        CL*17
01991              MOVE ER-7349        TO  EMI-ERROR                       CL*17
01992              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*17
01993                                                                   EL6511
01994      IF LFMETHL GREATER THAN ZERO                                 EL6511
01995          MOVE LFMETHI            TO  WS-FEE-METHOD                EL6511
01996          IF VALID-FEE-METHOD                                      EL6511
01997              MOVE AL-UANON       TO  LFMETHA                      EL6511
01998          ELSE                                                     EL6511
01999              MOVE -1             TO  LFMETHL                      EL6511
02000              MOVE AL-UABON       TO  LFMETHA                      EL6511
02001              MOVE ER-7350        TO  EMI-ERROR                    EL6511
02002              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02003                                                                   EL6511
02004      IF LFBASISL GREATER THAN ZERO                                EL6511
02005          MOVE LFBASISI           TO  WS-FEE-BASIS                 EL6511
02006          IF VALID-FEE-BASIS                                       EL6511
02007              MOVE AL-UANON       TO  LFBASISA                     EL6511
02008          ELSE                                                     EL6511
02009              MOVE -1             TO  LFBASISL                     EL6511
02010              MOVE AL-UABON       TO  LFBASISA                     EL6511
02011              MOVE ER-7351        TO  EMI-ERROR                    EL6511
02012              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02013                                                                   EL6511
02014      IF AHMETHL GREATER THAN ZERO                                 EL6511
02015          MOVE AHMETHI            TO  WS-FEE-METHOD                EL6511
02016          IF VALID-FEE-METHOD                                      EL6511
02017              MOVE AL-UANON       TO  AHMETHA                      EL6511
02018          ELSE                                                     EL6511
02019              MOVE -1             TO  AHMETHL                      EL6511
02020              MOVE AL-UABON       TO  AHMETHA                      EL6511
02021              MOVE ER-7350        TO  EMI-ERROR                    EL6511
02022              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02023                                                                   EL6511
02024      IF AHBASISL GREATER THAN ZERO                                EL6511
02025          MOVE AHBASISI           TO  WS-FEE-BASIS                 EL6511
02026          IF VALID-FEE-BASIS                                       EL6511
02027              MOVE AL-UANON       TO  AHBASISA                     EL6511
02028          ELSE                                                     EL6511
02029              MOVE -1             TO  AHBASISL                     EL6511
02030              MOVE AL-UABON       TO  AHBASISA                     EL6511
02031              MOVE ER-7351        TO  EMI-ERROR                    EL6511
02032              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02033                                                                   EL6511
02034      IF LFPCT1L GREATER ZERO                                      EL6511
02035          IF LFPCT1I NUMERIC                                       EL6511
02036              MOVE AL-UNNON       TO  LFPCT1A                      EL6511
02037          ELSE                                                     EL6511
02038              MOVE -1             TO  LFPCT1L                      EL6511
02039              MOVE AL-UNBON       TO  LFPCT1A                      EL6511
02040              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02041              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02042                                                                   EL6511
02043      IF LFPCT2L GREATER ZERO                                      EL6511
02044          IF LFPCT2I NUMERIC                                       EL6511
02045              MOVE AL-UNNON       TO  LFPCT2A                      EL6511
02046          ELSE                                                     EL6511
02047              MOVE -1             TO  LFPCT2L                      EL6511
02048              MOVE AL-UNBON       TO  LFPCT2A                      EL6511
02049              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02050              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02051                                                                   EL6511
02052      IF LFPCT3L GREATER ZERO                                      EL6511
02053          IF LFPCT3I NUMERIC                                       EL6511
02054              MOVE AL-UNNON       TO  LFPCT3A                      EL6511
02055          ELSE                                                     EL6511
02056              MOVE -1             TO  LFPCT3L                      EL6511
02057              MOVE AL-UNBON       TO  LFPCT3A                      EL6511
02058              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02059              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02060                                                                   EL6511
02061      IF LFPCT4L GREATER ZERO                                      EL6511
02062          IF LFPCT4I NUMERIC                                       EL6511
02063              MOVE AL-UNNON       TO  LFPCT4A                      EL6511
02064          ELSE                                                     EL6511
02065              MOVE -1             TO  LFPCT4L                      EL6511
02066              MOVE AL-UNBON       TO  LFPCT4A                      EL6511
02067              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02068              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02069                                                                   EL6511
02070      IF LFPCT5L GREATER ZERO                                      EL6511
02071          IF LFPCT5I NUMERIC                                       EL6511
02072              MOVE AL-UNNON       TO  LFPCT5A                      EL6511
02073          ELSE                                                     EL6511
02074              MOVE -1             TO  LFPCT5L                      EL6511
02075              MOVE AL-UNBON       TO  LFPCT5A                      EL6511
02076              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02077              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02078                                                                   EL6511
02079      IF LFPCT6L GREATER ZERO                                      EL6511
02080          IF LFPCT6I NUMERIC                                       EL6511
02081              MOVE AL-UNNON       TO  LFPCT6A                      EL6511
02082          ELSE                                                     EL6511
02083              MOVE -1             TO  LFPCT6L                      EL6511
02084              MOVE AL-UNBON       TO  LFPCT6A                      EL6511
02085              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02086              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02087                                                                   EL6511
02088      IF LFTHRU1L GREATER ZERO                                     EL6511
02089          MOVE LFTHRU1I           TO  DEEDIT-FIELD                 EL6511
02090          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02091          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02092              MOVE DEEDIT-FIELD-V1    TO  LFTHRU1O                 EL6511
02093                                          WS-LF-THRU1              EL6511
02094              MOVE AL-UNNON           TO  LFTHRU1A                 EL6511
02095          ELSE                                                     EL6511
02096              MOVE -1             TO  LFTHRU1L                     EL6511
02097              MOVE AL-UNBON       TO  LFTHRU1A                     EL6511
02098              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02099              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02100                                                                   EL6511
02101      IF LFTHRU2L GREATER ZERO                                     EL6511
02102          MOVE LFTHRU2I           TO  DEEDIT-FIELD                 EL6511
02103          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02104          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02105              MOVE DEEDIT-FIELD-V1    TO  LFTHRU2O                 EL6511
02106                                          WS-LF-THRU2              EL6511
02107              MOVE AL-UNNON           TO  LFTHRU2A                 EL6511
02108          ELSE                                                     EL6511
02109              MOVE -1             TO  LFTHRU2L                     EL6511
02110              MOVE AL-UNBON       TO  LFTHRU2A                     EL6511
02111              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02112              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02113                                                                   EL6511
02114      IF LFTHRU3L GREATER ZERO                                     EL6511
02115          MOVE LFTHRU3I           TO  DEEDIT-FIELD                 EL6511
02116          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02117          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02118              MOVE DEEDIT-FIELD-V1    TO  LFTHRU3O                 EL6511
02119                                          WS-LF-THRU3              EL6511
02120              MOVE AL-UNNON           TO  LFTHRU3A                 EL6511
02121          ELSE                                                     EL6511
02122              MOVE -1             TO  LFTHRU3L                     EL6511
02123              MOVE AL-UNBON       TO  LFTHRU3A                     EL6511
02124              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02125              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02126                                                                   EL6511
02127      IF LFTHRU4L GREATER ZERO                                     EL6511
02128          MOVE LFTHRU4I           TO  DEEDIT-FIELD                 EL6511
02129          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02130          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02131              MOVE DEEDIT-FIELD-V1    TO  LFTHRU4O                 EL6511
02132                                          WS-LF-THRU4              EL6511
02133              MOVE AL-UNNON           TO  LFTHRU4A                 EL6511
02134          ELSE                                                     EL6511
02135              MOVE -1             TO  LFTHRU4L                     EL6511
02136              MOVE AL-UNBON       TO  LFTHRU4A                     EL6511
02137              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02138              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02139                                                                   EL6511
02140      IF LFTHRU5L GREATER ZERO                                     EL6511
02141          MOVE LFTHRU5I           TO  DEEDIT-FIELD                 EL6511
02142          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02143          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02144              MOVE DEEDIT-FIELD-V1    TO  LFTHRU5O                 EL6511
02145                                          WS-LF-THRU5              EL6511
02146              MOVE AL-UNNON           TO  LFTHRU5A                 EL6511
02147          ELSE                                                     EL6511
02148              MOVE -1             TO  LFTHRU5L                     EL6511
02149              MOVE AL-UNBON       TO  LFTHRU5A                     EL6511
02150              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02151              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02152                                                                   EL6511
02153      IF LFTHRU6L GREATER ZERO                                     EL6511
02154          MOVE LFTHRU6I           TO  DEEDIT-FIELD                 EL6511
02155          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02156          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02157              MOVE DEEDIT-FIELD-V1    TO  LFTHRU6O                 EL6511
02158                                          WS-LF-THRU6              EL6511
02159              MOVE AL-UNNON           TO  LFTHRU6A                 EL6511
02160          ELSE                                                     EL6511
02161              MOVE -1             TO  LFTHRU6L                     EL6511
02162              MOVE AL-UNBON       TO  LFTHRU6A                     EL6511
02163              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02164              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02165                                                                   EL6511
02166      IF AHPCT1L GREATER ZERO                                      EL6511
02167          IF AHPCT1I NUMERIC                                       EL6511
02168              MOVE AL-UNNON       TO  AHPCT1A                      EL6511
02169          ELSE                                                     EL6511
02170              MOVE -1             TO  AHPCT1L                      EL6511
02171              MOVE AL-UNBON       TO  AHPCT1A                      EL6511
02172              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02173              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02174                                                                   EL6511
02175      IF AHPCT2L GREATER ZERO                                      EL6511
02176          IF AHPCT2I NUMERIC                                       EL6511
02177              MOVE AL-UNNON       TO  AHPCT2A                      EL6511
02178          ELSE                                                     EL6511
02179              MOVE -1             TO  AHPCT2L                      EL6511
02180              MOVE AL-UNBON       TO  AHPCT2A                      EL6511
02181              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02182              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02183                                                                   EL6511
02184      IF AHPCT3L GREATER ZERO                                      EL6511
02185          IF AHPCT3I NUMERIC                                       EL6511
02186              MOVE AL-UNNON       TO  AHPCT3A                      EL6511
02187          ELSE                                                     EL6511
02188              MOVE -1             TO  AHPCT3L                      EL6511
02189              MOVE AL-UNBON       TO  AHPCT3A                      EL6511
02190              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02191              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02192                                                                   EL6511
02193      IF AHPCT4L GREATER ZERO                                      EL6511
02194          IF AHPCT4I NUMERIC                                       EL6511
02195              MOVE AL-UNNON       TO  AHPCT4A                      EL6511
02196           ELSE                                                    EL6511
02197              MOVE -1             TO  AHPCT4L                      EL6511
02198              MOVE AL-UNBON       TO  AHPCT4A                      EL6511
02199              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02200              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02201                                                                   EL6511
02202      IF AHPCT5L GREATER ZERO                                      EL6511
02203          IF AHPCT5I NUMERIC                                       EL6511
02204              MOVE AL-UNNON       TO  AHPCT5A                      EL6511
02205          ELSE                                                     EL6511
02206              MOVE -1             TO  AHPCT5L                      EL6511
02207              MOVE AL-UNBON       TO  AHPCT5A                      EL6511
02208              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02209              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02210                                                                   EL6511
02211      IF AHPCT6L GREATER ZERO                                      EL6511
02212          IF AHPCT6I NUMERIC                                       EL6511
02213              MOVE AL-UNNON       TO  AHPCT6A                      EL6511
02214          ELSE                                                     EL6511
02215              MOVE -1             TO  AHPCT6L                      EL6511
02216              MOVE AL-UNBON       TO  AHPCT6A                      EL6511
02217              MOVE ER-2315        TO  EMI-ERROR                    EL6511
02218              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02219                                                                   EL6511
02220      IF AHTHRU1L GREATER ZERO                                     EL6511
02221          MOVE AHTHRU1I           TO  DEEDIT-FIELD                 EL6511
02222          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02223          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02224              MOVE DEEDIT-FIELD-V1    TO  AHTHRU1O                 EL6511
02225                                          WS-AH-THRU1              EL6511
02226              MOVE AL-UNNON           TO  AHTHRU1A                 EL6511
02227          ELSE                                                     EL6511
02228              MOVE -1             TO  AHTHRU1L                     EL6511
02229              MOVE AL-UNBON       TO  AHTHRU1A                     EL6511
02230              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02231              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02232                                                                   EL6511
02233      IF AHTHRU2L GREATER ZERO                                     EL6511
02234          MOVE AHTHRU2I           TO  DEEDIT-FIELD                 EL6511
02235          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02236          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02237              MOVE DEEDIT-FIELD-V1    TO  AHTHRU2O                 EL6511
02238                                          WS-AH-THRU2              EL6511
02239              MOVE AL-UNNON           TO  AHTHRU2A                 EL6511
02240          ELSE                                                     EL6511
02241              MOVE -1             TO  AHTHRU2L                     EL6511
02242              MOVE AL-UNBON       TO  AHTHRU2A                     EL6511
02243              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02244              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02245                                                                   EL6511
02246      IF AHTHRU3L GREATER ZERO                                     EL6511
02247          MOVE AHTHRU3I           TO  DEEDIT-FIELD                 EL6511
02248          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02249          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02250              MOVE DEEDIT-FIELD-V1    TO  AHTHRU3O                 EL6511
02251                                          WS-AH-THRU3              EL6511
02252              MOVE AL-UNNON           TO  AHTHRU3A                 EL6511
02253          ELSE                                                     EL6511
02254              MOVE -1             TO  AHTHRU3L                     EL6511
02255              MOVE AL-UNBON       TO  AHTHRU3A                     EL6511
02256              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02257              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02258                                                                   EL6511
02259      IF AHTHRU4L GREATER ZERO                                     EL6511
02260          MOVE AHTHRU4I           TO  DEEDIT-FIELD                 EL6511
02261          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02262          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02263              MOVE DEEDIT-FIELD-V1    TO  AHTHRU4O                 EL6511
02264                                          WS-AH-THRU4              EL6511
02265              MOVE AL-UNNON           TO  AHTHRU4A                 EL6511
02266          ELSE                                                     EL6511
02267              MOVE -1             TO  AHTHRU4L                     EL6511
02268              MOVE AL-UNBON       TO  AHTHRU4A                     EL6511
02269              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02270              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02271                                                                   EL6511
02272      IF AHTHRU5L GREATER ZERO                                     EL6511
02273          MOVE AHTHRU5I           TO  DEEDIT-FIELD                 EL6511
02274          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02275          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02276              MOVE DEEDIT-FIELD-V1    TO  AHTHRU5O                 EL6511
02277                                          WS-AH-THRU5              EL6511
02278              MOVE AL-UNNON           TO  AHTHRU5A                 EL6511
02279          ELSE                                                     EL6511
02280              MOVE -1             TO  AHTHRU5L                     EL6511
02281              MOVE AL-UNBON       TO  AHTHRU5A                     EL6511
02282              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02283              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02284                                                                   EL6511
02285      IF AHTHRU6L GREATER ZERO                                     EL6511
02286          MOVE AHTHRU6I           TO  DEEDIT-FIELD                 EL6511
02287          PERFORM 7450-DEEDIT THRU 7450-EXIT                       EL6511
02288          IF DEEDIT-FIELD-V0 NUMERIC                               EL6511
02289              MOVE DEEDIT-FIELD-V1    TO  AHTHRU6O                 EL6511
02290                                          WS-AH-THRU6              EL6511
02291              MOVE AL-UNNON           TO  AHTHRU6A                 EL6511
02292          ELSE                                                     EL6511
02293              MOVE -1             TO  AHTHRU6L                     EL6511
02294              MOVE AL-UNBON       TO  AHTHRU6A                     EL6511
02295              MOVE ER-2355        TO  EMI-ERROR                    EL6511
02296              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6511
02297                                                                   EL6511
02298      IF CEDNAMEL GREATER ZERO                                     EL6511
02299          MOVE AL-UANON           TO  CEDNAMEA.                    EL6511
02300                                                                   EL6511
02301  6400-EXIT.                                                       EL6511
02302      EXIT.                                                        EL6511
02303      EJECT                                                        EL6511
02304  7100-EDIT-MORTALITY.                                             EL6511
02305      IF  MORTCDI = SPACES                                            CL**9
02306          GO TO 7199-EXIT.                                         EL6511
02307                                                                   EL6511
02308      MOVE SPACES                 TO  ELCNTL-KEY.                     CL**9
02309      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6511
02310      MOVE '7'                    TO  CNTL-REC-TYPE.               EL6511
02311      MOVE +0                     TO  CNTL-SEQ-NO.                    CL**9
02312                                                                   EL6511
02313      EXEC CICS HANDLE CONDITION                                   EL6511
02314          NOTFND  (7190-NOT-FOUND)                                 EL6511
02315          ENDFILE (7192-END-OF-FILE)                                  CL**9
02316      END-EXEC.                                                    EL6511
02317                                                                   EL6511
02318  7109-READ-NEXT.                                                     CL**9
02319                                                                      CL**5
02320      EXEC CICS READ                                                  CL**9
02321           DATASET  (CNTL-FILE-ID)                                 EL6511
02322           SET      (ADDRESS OF CONTROL-FILE)                         CL*18
02323           RIDFLD   (ELCNTL-KEY)                                   EL6511
02324      END-EXEC.                                                    EL6511
02325                                                                   EL6511
02326      CONTINUE.                                                       CL*18
02327                                                                   EL6511
02328      IF  PI-COMPANY-ID NOT EQUAL TO CF-COMPANY-ID                    CL**9
02329              OR                                                      CL**9
02330          CF-RECORD-TYPE NOT EQUAL '7'                                CL**9
02331          GO TO 7190-NOT-FOUND                                     EL6511
02332                                                                      CL**9
02333      ELSE                                                         EL6511
02334          MOVE +1                 TO  SUB2                            CL**5
02335          GO TO 7110-SEARCH-MORTAL-TABLE.                          EL6511
02336                                                                      CL**5
02337  7110-SEARCH-MORTAL-TABLE.                                           CL**9
02338                                                                      CL**5
02339      IF  CF-MORT-TABLE-CODE (SUB2) = MORTCDI                         CL**9
02340          GO TO 7199-EXIT.                                            CL**9
02341                                                                      CL**9
02342      IF  CF-MORT-TABLE-CODE (SUB2) = LOW-VALUES                      CL**9
02343              OR                                                      CL**9
02344          CF-MORT-TABLE-CODE (SUB2) GREATER THAN MORTCDI              CL**9
02345          GO TO 7190-NOT-FOUND.                                       CL**9
02346                                                                      CL**9
02347      ADD +1                      TO  SUB2.                           CL**9
02348                                                                      CL**9
02349      IF  SUB2 GREATER +9                                             CL**9
02350          ADD +1                  TO  CNTL-SEQ-NO                     CL**9
02351          GO TO 7109-READ-NEXT                                        CL**9
02352                                                                      CL**9
02353      ELSE                                                            CL**9
02354          GO TO 7110-SEARCH-MORTAL-TABLE.                             CL**9
02355                                                                   EL6511
02356  7190-NOT-FOUND.                                                  EL6511
02357                                                                      CL**9
02358      MOVE -1                     TO  MORTCDL.                     EL6511
02359      MOVE AL-UABON               TO  MORTCDA.                     EL6511
02360      MOVE ER-2103                TO  EMI-ERROR                    EL6511
02361      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**9
02362      GO TO 7199-EXIT.                                                CL**5
02363                                                                   EL6511
02364  7192-END-OF-FILE.                                                   CL**5
02365                                                                      CL**9
02366      MOVE -1                     TO  MORTCDL.                        CL**5
02367      MOVE AL-UABON               TO  MORTCDA.                        CL**5
02368      MOVE ER-2237                TO  EMI-ERROR                       CL**5
02369      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**9
02370      GO TO 7199-EXIT.                                                CL**9
02371                                                                      CL**9
02372  7199-EXIT.                                                       EL6511
02373      EXIT.                                                        EL6511
02374      EJECT                                                        EL6511
02375  7200-READ-ERREIN.                                                EL6511
02376      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.           EL6511
02377      MOVE 'B'                    TO  PI-CRR-CODE.                 EL6511
02378                                                                   EL6511
02379      EXEC CICS READ                                               EL6511
02380           DATASET  (REIN-FILE-ID)                                 EL6511
02381           SET      (ADDRESS OF REINSURANCE-RECORD)                   CL*18
02382           RIDFLD   (PI-ERREIN-KEY)                                EL6511
02383      END-EXEC.                                                    EL6511
02384                                                                   EL6511
02385      CONTINUE.                                                       CL*18
02386                                                                   EL6511
02387      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL6511
02388      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL6511
02389                                                                   EL6511
02390      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.          EL6511
02391                                                                   EL6511
02392  7200-EXIT.                                                       EL6511
02393      EXIT.                                                        EL6511
02394      EJECT                                                        EL6511
02395  7300-ERREIN-GETMAIN.                                             EL6511
02396      EXEC CICS GETMAIN                                            EL6511
02397           SET     (ADDRESS OF REINSURANCE-RECORD)                    CL*18
02398           LENGTH  (ERREIN-LENGTH)                                 EL6511
02399           INITIMG (GETMAIN-SPACE)                                 EL6511
02400      END-EXEC.                                                    EL6511
02401                                                                   EL6511
02402      CONTINUE.                                                       CL*18
02403                                                                   EL6511
02404  7300-EXIT.                                                       EL6511
02405      EXIT.                                                        EL6511
02406      EJECT                                                        EL6511
02407  7400-READ-ERREIN-UPDATE.                                         EL6511
02408      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.           EL6511
02409      MOVE 'B'                    TO  PI-CRR-CODE.                 EL6511
02410                                                                   EL6511
02411      EXEC CICS READ                                               EL6511
02412           DATASET  (REIN-FILE-ID)                                 EL6511
02413           SET      (ADDRESS OF REINSURANCE-RECORD)                   CL*18
02414           RIDFLD   (PI-ERREIN-KEY)                                EL6511
02415           UPDATE                                                  EL6511
02416      END-EXEC.                                                    EL6511
02417                                                                   EL6511
02418      CONTINUE.                                                       CL*18
02419                                                                   EL6511
02420  7400-EXIT.                                                       EL6511
02421      EXIT.                                                        EL6511
02422      EJECT                                                        EL6511
02423  7450-DEEDIT.                                                     EL6511
02424      EXEC CICS BIF                                                EL6511
02425           DEEDIT                                                  EL6511
02426           FIELD  (DEEDIT-FIELD)                                   EL6511
02427           LENGTH (15)                                             EL6511
02428      END-EXEC.                                                    EL6511
02429                                                                   EL6511
02430  7450-EXIT.                                                       EL6511
02431      EXIT.                                                        EL6511
02432      EJECT                                                        EL6511
02433  7500-PAGE-FORWARD.                                               EL6511
02434      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.            EL6511
02435                                                                   EL6511
02436      IF PI-MAP-C                                                  EL6511
02437          GO TO 7510-MAP-C.                                        EL6511
02438                                                                   EL6511
02439      IF COMPANYL GREATER ZERO                                     EL6511
02440          MOVE ZERO TO TALLY                                          CL*18
02441          INSPECT COMPANYI TALLYING TALLY FOR ALL SPACES              CL*18
02442          IF TALLY GREATER ZERO                                    EL6511
02443              MOVE -1             TO  COMPANYL                     EL6511
02444              MOVE AL-UABON       TO  COMPANYA                     EL6511
02445              MOVE ER-2340        TO  EMI-ERROR                    EL6511
02446              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6511
02447          ELSE                                                     EL6511
02448              MOVE SPACE          TO  PI-FIRST-TIME-SW             EL6511
02449              MOVE AL-UANON       TO  COMPANYA                     EL6511
02450              MOVE COMPANYI       TO  PI-CRR-TABLE                 EL6511
02451      ELSE                                                         EL6511
02452          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL6511
02453          MOVE LOW-VALUES         TO  PI-ERREIN-KEY.               EL6511
02454                                                                   EL6511
02455      IF PI-FIRST-TIME-SW = 'Y'                                    EL6511
02456          NEXT SENTENCE                                            EL6511
02457      ELSE                                                         EL6511
02458          IF COMPSUBL IS GREATER THAN 0                            EL6511
02459              MOVE AL-SANON       TO  COMPSUBA                     EL6511
02460              MOVE COMPSUBI       TO  PI-CRR-TABLE-SUB             EL6511
02461          ELSE                                                     EL6511
02462              MOVE ZEROS          TO  PI-CRR-TABLE-SUB.            EL6511
02463                                                                   EL6511
02464      MOVE PI-ERREIN-KEY          TO  WS-SAVE-KEY.                 EL6511
02465                                                                   EL6511
02466      GO TO 7520-START.                                            EL6511
02467                                                                   EL6511
02468  7510-MAP-C.                                                      EL6511
02469      IF COMPL GREATER ZERO                                        EL6511
02470          MOVE AL-SANON           TO  COMPA                        EL6511
02471          MOVE COMPI              TO  PI-CRR-TABLE                 EL6511
02472          MOVE SPACE              TO  PI-FIRST-TIME-SW             EL6511
02473      ELSE                                                         EL6511
02474          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL6511
02475          MOVE LOW-VALUES         TO  PI-ERREIN-KEY.               EL6511
02476                                                                   EL6511
02477      IF PI-FIRST-TIME-SW  = 'Y'                                   EL6511
02478          NEXT SENTENCE                                            EL6511
02479      ELSE                                                         EL6511
02480          IF SUBL IS GREATER THAN 0                                EL6511
02481              MOVE AL-SANON       TO  SUBA                         EL6511
02482              MOVE SUBI           TO  PI-CRR-TABLE-SUB             EL6511
02483          ELSE                                                     EL6511
02484              MOVE ZEROS          TO  PI-CRR-TABLE-SUB.            EL6511
02485                                                                   EL6511
02486      MOVE PI-ERREIN-KEY          TO  WS-SAVE-KEY.                 EL6511
02487                                                                   EL6511
02488  7520-START.                                                      EL6511
02489      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.           EL6511
02490      MOVE 'B'                    TO  PI-CRR-CODE.                 EL6511
02491                                                                   EL6511
02492      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL6511
02493                                                                   EL6511
02494      EXEC CICS HANDLE CONDITION                                   EL6511
02495          ENDFILE  (7550-ENDFILE)                                  EL6511
02496          NOTFND   (7575-NOTFOUND)                                 EL6511
02497      END-EXEC.                                                    EL6511
02498                                                                   EL6511
02499  7530-READ-NEXT.                                                  EL6511
02500      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL6511
02501                                                                   EL6511
02502      IF ERREIN-EOF                                                EL6511
02503          GO TO 7550-ENDFILE.                                      EL6511
02504                                                                   EL6511
02505      IF PI-ERREIN-KEY = WS-SAVE-KEY                               EL6511
02506          GO TO 7530-READ-NEXT.                                    EL6511
02507                                                                   EL6511
02508      IF PI-CARRIER-SECURITY GREATER SPACE                         EL6511
02509          IF PI-CARRIER-SECURITY = RE-COMP-CARRIER-SECURITY        EL6511
02510              NEXT SENTENCE                                        EL6511
02511          ELSE                                                     EL6511
02512              GO TO 7530-READ-NEXT.                                EL6511
02513                                                                   EL6511
02514      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL6511
02515      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL6511
02516      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.          EL6511
02517                                                                   EL6511
02518      IF PI-MAP-C                                                  EL6511
02519          MOVE LOW-VALUES         TO  EL6511CO                     EL6511
02520          GO TO 5550-SET-UP-SCREEN                                 EL6511
02521      ELSE                                                         EL6511
02522          MOVE LOW-VALUES         TO  EL6511BO                     EL6511
02523          GO TO 5050-SET-UP-SCREEN.                                EL6511
02524                                                                   EL6511
02525  7540-CHECK-MAP.                                                  EL6511
02526      IF FIRST-TIME                                                EL6511
02527          IF PI-MAP-C                                              EL6511
02528              MOVE LOW-VALUES     TO  EL6511CO                     EL6511
02529              MOVE -1             TO  DESC1L                       EL6511
02530              GO TO 8110-SEND-INITIAL-MAP                          EL6511
02531          ELSE                                                     EL6511
02532              MOVE LOW-VALUES     TO  EL6511BO                     EL6511
02533              MOVE -1             TO  COMPANYL                     EL6511
02534              GO TO 8100-SEND-INITIAL-MAP                          EL6511
02535      ELSE                                                         EL6511
02536          IF PI-MAP-C                                              EL6511
02537              MOVE LOW-VALUES     TO  EL6511CO                     EL6511
02538          ELSE                                                     EL6511
02539              MOVE LOW-VALUES     TO  EL6511BO.                    EL6511
02540                                                                   EL6511
02541      GO TO 7500-PAGE-FORWARD.                                     EL6511
02542                                                                   EL6511
02543  7550-ENDFILE.                                                    EL6511
02544      IF BROWSE-STARTED                                            EL6511
02545          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL6511
02546                                                                   EL6511
02547      IF FIRST-TIME                                                EL6511
02548          MOVE ER-2386            TO  EMI-ERROR                    EL6511
02549      ELSE                                                         EL6511
02550          MOVE ER-2067            TO  EMI-ERROR.                   EL6511
02551                                                                   EL6511
02552      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6511
02553      GO TO 7540-CHECK-MAP.                                        EL6511
02554                                                                   EL6511
02555  7575-NOTFOUND.                                                   EL6511
02556      IF BROWSE-STARTED                                            EL6511
02557          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL6511
02558                                                                   EL6511
02559      GO TO 8880-NOT-FOUND.                                        EL6511
02560                                                                   EL6511
02561  7599-EXIT.                                                       EL6511
02562      EXIT.                                                        EL6511
02563      EJECT                                                        EL6511
02564                                                                   EL6511
02565  7600-PAGE-BACKWARD.                                              EL6511
02566      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.            EL6511
02567                                                                   EL6511
02568      IF PI-MAP-C                                                  EL6511
02569          GO TO 7610-MAP-C.                                        EL6511
02570                                                                   EL6511
02571      IF COMPANYL GREATER ZERO                                     EL6511
02572          MOVE ZERO TO TALLY                                          CL*18
02573          INSPECT COMPANYI TALLYING TALLY FOR ALL SPACES              CL*18
02574          IF TALLY GREATER ZERO                                    EL6511
02575              MOVE -1             TO  COMPANYL                     EL6511
02576              MOVE AL-UABON       TO  COMPANYA                     EL6511
02577              MOVE ER-2340        TO  EMI-ERROR                    EL6511
02578              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6511
02579          ELSE                                                     EL6511
02580              MOVE AL-UANON       TO  COMPANYA                     EL6511
02581              MOVE COMPANYI       TO  PI-CRR-TABLE                 EL6511
02582      ELSE                                                         EL6511
02583          GO TO 7675-NOTFOUND.                                     EL6511
02584                                                                   EL6511
02585      IF COMPSUBL IS GREATER THAN 0                                EL6511
02586          MOVE AL-UANON           TO  COMPSUBA                     EL6511
02587          MOVE COMPSUBI           TO  PI-CRR-TABLE-SUB             EL6511
02588      ELSE                                                         EL6511
02589          MOVE ZEROS              TO  PI-CRR-TABLE-SUB.            EL6511
02590                                                                   EL6511
02591      GO TO 7620-START.                                            EL6511
02592                                                                   EL6511
02593  7610-MAP-C.                                                      EL6511
02594      IF COMPL GREATER ZERO                                        EL6511
02595          MOVE AL-SANON           TO  COMPA                        EL6511
02596          MOVE COMPI              TO  PI-CRR-TABLE                 EL6511
02597      ELSE                                                         EL6511
02598          GO TO 7675-NOTFOUND.                                     EL6511
02599                                                                   EL6511
02600      IF SUBL IS GREATER THAN 0                                    EL6511
02601          MOVE AL-SANON           TO  SUBA                         EL6511
02602          MOVE SUBI               TO  PI-CRR-TABLE-SUB             EL6511
02603      ELSE                                                         EL6511
02604          MOVE ZEROS              TO  PI-CRR-TABLE-SUB.            EL6511
02605                                                                   EL6511
02606  7620-START.                                                      EL6511
02607      MOVE 'B'                    TO  PI-CRR-CODE.                 EL6511
02608      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.           EL6511
02609                                                                   EL6511
02610      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL6511
02611                                                                   EL6511
02612      EXEC CICS HANDLE CONDITION                                   EL6511
02613          ENDFILE  (7650-ENDFILE)                                  EL6511
02614          NOTFND   (7675-NOTFOUND)                                 EL6511
02615       END-EXEC.                                                   EL6511
02616                                                                   EL6511
02617      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL6511
02618                                                                   EL6511
02619      IF ERREIN-EOF                                                EL6511
02620          IF BROWSE-STARTED                                        EL6511
02621              PERFORM 7950-END-BROWSE THRU 7950-EXIT               EL6511
02622              GO TO 7630-CHECK-MAP.                                EL6511
02623                                                                   EL6511
02624  7625-READ-PREV.                                                  EL6511
02625      PERFORM 7900-READPREV THRU 7900-EXIT.                        EL6511
02626      PERFORM 7900-READPREV THRU 7900-EXIT.                        EL6511
02627                                                                   EL6511
02628      IF ERREIN-EOF                                                EL6511
02629          GO TO 7650-ENDFILE.                                      EL6511
02630                                                                   EL6511
02631      IF PI-ERREIN-KEY = WS-SAVE-KEY                               EL6511
02632          GO TO 7625-READ-PREV.                                    EL6511
02633                                                                   EL6511
02634      IF PI-CARRIER-SECURITY GREATER SPACE                         EL6511
02635         IF PI-CARRIER-SECURITY NOT = RE-COMP-CARRIER-SECURITY     EL6511
02636              GO TO 7625-READ-PREV.                                EL6511
02637                                                                   EL6511
02638      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL6511
02639      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL6511
02640      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.          EL6511
02641                                                                   EL6511
02642      IF PI-MAP-C                                                  EL6511
02643          MOVE RE-COMP-PRIME      TO  COMPO                        EL6511
02644          MOVE RE-COMP-SUB        TO  SUBO                         EL6511
02645          MOVE AL-SANON           TO  COMPA                        EL6511
02646                                      SUBA                         EL6511
02647          GO TO 5550-SET-UP-SCREEN                                 EL6511
02648      ELSE                                                         EL6511
02649          MOVE RE-COMP-PRIME      TO  COMPANYO                     EL6511
02650          MOVE RE-COMP-SUB        TO  COMPSUBO                     EL6511
02651          MOVE AL-UANON           TO  COMPANYA                     EL6511
02652                                      COMPSUBA                     EL6511
02653          GO TO 5050-SET-UP-SCREEN.                                EL6511
02654                                                                   EL6511
02655  7630-CHECK-MAP.                                                  EL6511
02656      IF PI-MAP-C                                                  EL6511
02657          MOVE LOW-VALUES         TO  EL6511CO                     EL6511
02658      ELSE                                                         EL6511
02659          MOVE LOW-VALUES         TO  EL6511BO.                    EL6511
02660                                                                   EL6511
02661      GO TO 7500-PAGE-FORWARD.                                     EL6511
02662                                                                   EL6511
02663  7650-ENDFILE.                                                    EL6511
02664      IF BROWSE-STARTED                                            EL6511
02665          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL6511
02666                                                                   EL6511
02667      MOVE ER-2067                TO  EMI-ERROR                    EL6511
02668      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6511
02669      GO TO 7630-CHECK-MAP.                                        EL6511
02670                                                                   EL6511
02671  7675-NOTFOUND.                                                   EL6511
02672      IF BROWSE-STARTED                                            EL6511
02673          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL6511
02674                                                                   EL6511
02675      GO TO 8880-NOT-FOUND.                                        EL6511
02676                                                                   EL6511
02677  7699-EXIT.                                                       EL6511
02678      EXIT.                                                        EL6511
02679      EJECT                                                        EL6511
02680                                                                   EL6511
02681  7700-CHECK-CARRIER.                                              EL6511
02682      MOVE SPACES                 TO  WS-CARRIER-FOUND-SW          EL6511
02683                                      ELCNTL-KEY.                  EL6511
02684      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6511
02685      MOVE '6'                    TO  CNTL-REC-TYPE.               EL6511
02686      MOVE CARRIER-ACCESS         TO  CNTL-ACCESS.                 EL6511
02687      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6511
02688                                                                   EL6511
02689      EXEC CICS HANDLE CONDITION                                   EL6511
02690          NOTFND   (7700-EXIT)                                     EL6511
02691      END-EXEC.                                                    EL6511
02692                                                                   EL6511
02693      EXEC CICS READ                                               EL6511
02694          DATASET   (CNTL-FILE-ID)                                 EL6511
02695          SET       (ADDRESS OF CONTROL-FILE)                         CL*18
02696          RIDFLD    (ELCNTL-KEY)                                   EL6511
02697      END-EXEC.                                                    EL6511
02698                                                                   EL6511
02699      MOVE 'Y'                    TO  WS-CARRIER-FOUND-SW.         EL6511
02700                                                                   EL6511
02701  7700-EXIT.                                                       EL6511
02702      EXIT.                                                        EL6511
02703      EJECT                                                        EL6511
02704  7800-START-BROWSE.                                               EL6511
02705      EXEC CICS STARTBR                                            EL6511
02706           DATASET  (REIN-FILE-ID)                                 EL6511
02707           RIDFLD   (PI-ERREIN-KEY)                                EL6511
02708      END-EXEC.                                                    EL6511
02709                                                                   EL6511
02710      MOVE 'Y'                    TO  PI-BROWSE-SW.                EL6511
02711                                                                   EL6511
02712  7800-EXIT.                                                       EL6511
02713      EXIT.                                                        EL6511
02714      EJECT                                                        EL6511
02715                                                                   EL6511
02716  7850-READNEXT.                                                   EL6511
02717      EXEC CICS READNEXT                                           EL6511
02718           DATASET  (REIN-FILE-ID)                                 EL6511
02719           SET      (ADDRESS OF REINSURANCE-RECORD)                   CL*18
02720           RIDFLD   (PI-ERREIN-KEY)                                EL6511
02721      END-EXEC.                                                    EL6511
02722                                                                   EL6511
02723      CONTINUE.                                                       CL*18
02724                                                                   EL6511
02725      IF PI-COMPANY-CD NOT = RE-COMPANY-CD OR                      EL6511
02726         PI-CRR-CODE NOT = 'B'                                     EL6511
02727          MOVE 'Y'                TO  PI-ERREIN-EOF-SW.            EL6511
02728                                                                   EL6511
02729  7850-EXIT.                                                       EL6511
02730      EXIT.                                                        EL6511
02731      EJECT                                                        EL6511
02732  7900-READPREV.                                                   EL6511
02733      EXEC CICS READPREV                                           EL6511
02734           DATASET  (REIN-FILE-ID)                                 EL6511
02735           SET      (ADDRESS OF REINSURANCE-RECORD)                   CL*18
02736           RIDFLD   (PI-ERREIN-KEY)                                EL6511
02737      END-EXEC.                                                    EL6511
02738                                                                   EL6511
02739      CONTINUE.                                                       CL*18
02740                                                                   EL6511
02741      IF PI-COMPANY-CD NOT = RE-COMPANY-CD OR                      EL6511
02742         PI-CRR-CODE NOT = 'B'                                     EL6511
02743          MOVE 'Y'                TO  PI-ERREIN-EOF-SW.            EL6511
02744                                                                   EL6511
02745  7900-EXIT.                                                       EL6511
02746      EXIT.                                                        EL6511
02747      EJECT                                                        EL6511
02748  7950-END-BROWSE.                                                 EL6511
02749      EXEC CICS ENDBR                                              EL6511
02750           DATASET  (REIN-FILE-ID)                                 EL6511
02751      END-EXEC.                                                    EL6511
02752                                                                   EL6511
02753      MOVE SPACE                  TO  PI-BROWSE-SW.                EL6511
02754                                                                   EL6511
02755  7950-EXIT.                                                       EL6511
02756      EXIT.                                                        EL6511
02757      EJECT                                                        EL6511
02758  8000-UPDATE-MAINT-DATE.                                          EL6511
02759      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6511
02760                                                                   EL6511
02761      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6511
02762      MOVE '1'                    TO  CNTL-REC-TYPE.               EL6511
02763      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6511
02764                                                                   EL6511
02765      EXEC CICS HANDLE CONDITION                                   EL6511
02766          NOTFND   (8000-EXIT)                                     EL6511
02767      END-EXEC.                                                    EL6511
02768                                                                   EL6511
02769      EXEC CICS READ                                               EL6511
02770          UPDATE                                                   EL6511
02771          DATASET   (CNTL-FILE-ID)                                 EL6511
02772          SET       (ADDRESS OF CONTROL-FILE)                         CL*18
02773          RIDFLD    (ELCNTL-KEY)                                   EL6511
02774      END-EXEC.                                                    EL6511
02775                                                                   EL6511
02776      CONTINUE.                                                       CL*18
02777                                                                   EL6511
02778      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL6511
02779      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL6511
02780      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL6511
02781      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6511
02782      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6511
02783                                                                   EL6511
02784      MOVE BIN-CURRENT-SAVE       TO  CF-REINSURANCE-TAB-MAINT-DT. EL6511
02785                                                                   EL6511
02786      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL6511
02787      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL6511
02788      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL6511
02789                                                                   EL6511
02790      EXEC CICS REWRITE                                            EL6511
02791          DATASET   (CNTL-FILE-ID)                                 EL6511
02792          FROM      (CONTROL-FILE)                                 EL6511
02793      END-EXEC.                                                    EL6511
02794                                                                   EL6511
02795      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6511
02796                                                                   EL6511
02797  8000-EXIT.                                                       EL6511
02798       EXIT.                                                       EL6511
02799      EJECT                                                        EL6511
02800  8100-SEND-INITIAL-MAP.                                           EL6511
02801      MOVE SAVE-DATE              TO  BDATEO.                      EL6511
02802      MOVE EIBTIME                TO  TIME-IN.                     EL6511
02803      MOVE TIME-OUT               TO  BTIMEO.                      EL6511
02804      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6511
02805      MOVE -1                     TO  MAINTYPL.                    EL6511
02806                                                                   EL6511
02807      PERFORM 8250-SET-SCREEN-HEADINGS THRU 8250-EXIT.             EL6511
02808                                                                   EL6511
02809      EXEC CICS SEND                                               EL6511
02810          MAP     (MAP-B-NAME)                                     EL6511
02811          MAPSET  (MAPSET-NAME)                                    EL6511
02812          FROM    (EL6511BO)                                       EL6511
02813          ERASE                                                    EL6511
02814          CURSOR                                                   EL6511
02815      END-EXEC.                                                    EL6511
02816                                                                   EL6511
02817      GO TO 9100-RETURN-TRAN.                                      EL6511
02818                                                                   EL6511
02819  8110-SEND-INITIAL-MAP.                                           EL6511
02820      MOVE SAVE-DATE              TO  CDATEO.                      EL6511
02821      MOVE EIBTIME                TO  TIME-IN.                     EL6511
02822      MOVE TIME-OUT               TO  CTIMEO.                      EL6511
02823      MOVE EMI-MESSAGE-AREA (1)   TO  CERRMSGO.                    EL6511
02824      MOVE -1                     TO  DESC1L.                      EL6511
02825                                                                   EL6511
02826      EXEC CICS SEND                                               EL6511
02827          MAP     (MAP-C-NAME)                                     EL6511
02828          MAPSET  (MAPSET-NAME)                                    EL6511
02829          FROM    (EL6511CO)                                       EL6511
02830          ERASE                                                    EL6511
02831          CURSOR                                                   EL6511
02832      END-EXEC.                                                    EL6511
02833                                                                   EL6511
02834      GO TO 9100-RETURN-TRAN.                                      EL6511
02835                                                                   EL6511
02836  8200-SEND-DATAONLY.                                              EL6511
02837      MOVE SAVE-DATE              TO  BDATEO.                      EL6511
02838      MOVE EIBTIME                TO  TIME-IN.                     EL6511
02839      MOVE TIME-OUT               TO  BTIMEO.                      EL6511
02840      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6511
02841                                                                   EL6511
02842      PERFORM 8250-SET-SCREEN-HEADINGS THRU 8250-EXIT.             EL6511
02843                                                                   EL6511
02844      EXEC CICS SEND                                               EL6511
02845          MAP     (MAP-B-NAME)                                     EL6511
02846          MAPSET  (MAPSET-NAME)                                    EL6511
02847          FROM    (EL6511BO)                                       EL6511
02848          DATAONLY                                                 EL6511
02849          CURSOR                                                   EL6511
02850      END-EXEC.                                                    EL6511
02851                                                                   EL6511
02852      GO TO 9100-RETURN-TRAN.                                      EL6511
02853                                                                   EL6511
02854  8210-SEND-DATAONLY.                                              EL6511
02855      MOVE SAVE-DATE              TO  CDATEO.                      EL6511
02856      MOVE EIBTIME                TO  TIME-IN.                     EL6511
02857      MOVE TIME-OUT               TO  CTIMEO.                      EL6511
02858      MOVE EMI-MESSAGE-AREA (1)   TO  CERRMSGO.                    EL6511
02859                                                                   EL6511
02860      EXEC CICS SEND                                               EL6511
02861          MAP     (MAP-C-NAME)                                     EL6511
02862          MAPSET  (MAPSET-NAME)                                    EL6511
02863          FROM    (EL6511CO)                                       EL6511
02864          DATAONLY                                                 EL6511
02865          CURSOR                                                   EL6511
02866      END-EXEC.                                                    EL6511
02867                                                                   EL6511
02868      GO TO 9100-RETURN-TRAN.                                      EL6511
02869                                                                   EL6511
02870  8250-SET-SCREEN-HEADINGS.                                        EL6511
02871      MOVE PI-LIFE-OVERRIDE-L1    TO  FEELFHDO                     EL6511
02872                                      COMLFHDO                     EL6511
02873                                      TAXLFHDO.                    EL6511
02874      MOVE PI-AH-OVERRIDE-L1      TO  FEEAHHDO                     EL6511
02875                                      COMAHHDO                     EL6511
02876                                      TAXAHHDO.                    EL6511
02877      MOVE PI-LIFE-OVERRIDE-L2    TO  LIBNRHDO                        CL*17
02878                                      LFCVTYPO.                       CL*17
02879      MOVE PI-AH-OVERRIDE-L2      TO  AIBNRHDO                        CL*17
02880                                      AHCVTYPO.                       CL*17
02881      MOVE PI-LIFE-OVERRIDE-L6    TO  LFCLMHDO                     EL6511
02882                                      LFFEEHGO                     EL6511
02883                                      CMLFHDO.                     EL6511
02884      MOVE PI-AH-OVERRIDE-L6      TO  AHCLMHDO                     EL6511
02885                                      CMAHHDO                      EL6511
02886                                      AHFEEHGO.                    EL6511
02887                                                                   EL6511
02888      MOVE AL-SANON               TO  FEELFHDA  COMLFHDA  TAXLFHDA EL6511
02889                                      AHCVTYPA  COMAHHDA  TAXAHHDA EL6511
02890                                      LFCVTYPA  CMLFHDA   LIBNRHDA EL6511
02891                                      CMAHHDA   AIBNRHDA  LFCLMHDA EL6511
02892                                      LFFEEHGA  AHCLMHDA  AHFEEHGA.EL6511
02893                                                                      CL*13
02894      IF PI-COMPANY-ID = 'NCL' OR 'LGX'                               CL*13
02895         NEXT SENTENCE                                                CL*13
02896      ELSE                                                            CL*13
02897         MOVE AL-SADOF            TO  GLHDRA                          CL*13
02898                                      GLCNTRA                         CL*16
02899                                      CUSTDSCA                        CL*16
02900         MOVE AL-SANOF            TO  CUSTAMTA.                       CL*16
02901                                                                   EL6511
02902  8250-EXIT.                                                       EL6511
02903      EXIT.                                                        EL6511
02904                                                                   EL6511
02905  8300-SEND-TEXT.                                                  EL6511
02906      EXEC CICS SEND TEXT                                          EL6511
02907          FROM    (LOGOFF-TEXT)                                    EL6511
02908          LENGTH  (LOGOFF-LENGTH)                                  EL6511
02909          ERASE                                                    EL6511
02910          FREEKB                                                   EL6511
02911      END-EXEC.                                                    EL6511
02912                                                                   EL6511
02913      EXEC CICS RETURN                                             EL6511
02914      END-EXEC.                                                    EL6511
02915                                                                   EL6511
02916  8400-LOG-JOURNAL-RECORD.                                         EL6511
02917      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL6511
02918      MOVE FILE-ID                TO  JP-FILE-ID.                  EL6511
02919      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL6511
02920 *    IF PI-JOURNAL-FILE-ID NOT = ZERO                             EL6511
02921 *        EXEC CICS JOURNAL                                        EL6511
02922 *            JFILEID  (PI-JOURNAL-FILE-ID)                        EL6511
02923 *            JTYPEID  ('EL')                                      EL6511
02924 *            FROM     (JOURNAL-RECORD)                            EL6511
02925 *            LENGTH   (WS-JOURNAL-FILE-LENGTH)                    EL6511
02926 *        END-EXEC.                                                EL6511
02927                                                                   EL6511
02928  8800-UNAUTHORIZED-ACCESS.                                        EL6511
02929      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL6511
02930      GO TO 8300-SEND-TEXT.                                        EL6511
02931                                                                   EL6511
02932  8810-PF23.                                                       EL6511
02933      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6511
02934      MOVE XCTL-005               TO  PGM-NAME.                    EL6511
02935      GO TO 9300-XCTL.                                             EL6511
02936                                                                   EL6511
02937  8870-NOTOPEN.                                                    EL6511
02938      MOVE ER-2055                TO  EMI-ERROR.                   EL6511
02939      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6511
02940      MOVE -1                     TO  BENTERL.                     EL6511
02941      IF EIBTRNID NOT = TRANS-ID                                   EL6511
02942          GO TO 8100-SEND-INITIAL-MAP.                             EL6511
02943                                                                   EL6511
02944      GO TO 8200-SEND-DATAONLY.                                    EL6511
02945                                                                   EL6511
02946  8880-NOT-FOUND.                                                  EL6511
02947      MOVE ER-0142                TO  EMI-ERROR.                   EL6511
02948      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6511
02949      MOVE -1                     TO  MAINTYPL.                    EL6511
02950      IF EIBTRNID NOT = TRANS-ID                                   EL6511
02951          GO TO 8100-SEND-INITIAL-MAP.                             EL6511
02952                                                                   EL6511
02953      GO TO 8200-SEND-DATAONLY.                                    EL6511
02954                                                                   EL6511
02955  9000-RETURN-CICS.                                                EL6511
02956      EXEC CICS RETURN                                             EL6511
02957      END-EXEC.                                                    EL6511
02958                                                                   EL6511
02959  9100-RETURN-TRAN.                                                EL6511
02960      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6511
02961      MOVE '651B'                 TO  PI-CURRENT-SCREEN-NO.           CL**2
02962      EXEC CICS RETURN                                             EL6511
02963          TRANSID   (TRANS-ID)                                     EL6511
02964          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6511
02965          LENGTH    (PI-COMM-LENGTH)                               EL6511
02966      END-EXEC.                                                    EL6511
02967                                                                   EL6511
02968  9200-RETURN-MAIN-MENU.                                           EL6511
02969      MOVE XCTL-626               TO  PGM-NAME.                    EL6511
02970      GO TO 9300-XCTL.                                             EL6511
02971                                                                   EL6511
02972  9300-XCTL.                                                       EL6511
02973      EXEC CICS XCTL                                               EL6511
02974          PROGRAM   (PGM-NAME)                                     EL6511
02975          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6511
02976          LENGTH    (PI-COMM-LENGTH)                               EL6511
02977      END-EXEC.                                                    EL6511
02978                                                                   EL6511
02979  9400-CLEAR.                                                      EL6511
02980      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL6511
02981      GO TO 9300-XCTL.                                             EL6511
02982                                                                   EL6511
02983  9500-PF12.                                                       EL6511
02984      MOVE XCTL-010               TO  PGM-NAME.                    EL6511
02985      GO TO 9300-XCTL.                                             EL6511
02986                                                                   EL6511
02987  9600-PGMID-ERROR.                                                EL6511
02988      EXEC CICS HANDLE CONDITION                                   EL6511
02989          PGMIDERR  (8300-SEND-TEXT)                               EL6511
02990      END-EXEC.                                                    EL6511
02991                                                                   EL6511
02992      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL6511
02993      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL6511
02994      MOVE XCTL-005               TO  PGM-NAME.                    EL6511
02995      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL6511
02996      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6511
02997      GO TO 9300-XCTL.                                             EL6511
02998                                                                   EL6511
02999  9700-LINK-DATE-CONVERT.                                          EL6511
03000      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL6511
03001                                                                   EL6511
03002      EXEC CICS LINK                                               EL6511
03003          PROGRAM   (PGM-NAME)                                     EL6511
03004          COMMAREA  (DATE-CONVERSION-DATA)                         EL6511
03005          LENGTH    (DC-COMM-LENGTH)                               EL6511
03006      END-EXEC.                                                    EL6511
03007                                                                   EL6511
03008  9700-EXIT.                                                       EL6511
03009      EXIT.                                                        EL6511
03010                                                                   EL6511
03011  9900-ERROR-FORMAT.                                               EL6511
03012      IF NOT EMI-ERRORS-COMPLETE                                   EL6511
03013          MOVE LINK-001           TO  PGM-NAME                     EL6511
03014          EXEC CICS LINK                                           EL6511
03015              PROGRAM   (PGM-NAME)                                 EL6511
03016              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL6511
03017              LENGTH    (EMI-COMM-LENGTH)                          EL6511
03018          END-EXEC.                                                EL6511
03019                                                                   EL6511
03020  9900-EXIT.                                                       EL6511
03021      EXIT.                                                        EL6511
03022                                                                   EL6511
03023  9990-ABEND.                                                      EL6511
03024      MOVE LINK-004               TO  PGM-NAME.                    EL6511
03025      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL6511
03026      EXEC CICS LINK                                               EL6511
03027          PROGRAM   (PGM-NAME)                                     EL6511
03028          COMMAREA  (EMI-LINE1)                                    EL6511
03029          LENGTH    (72)                                           EL6511
03030      END-EXEC.                                                    EL6511
03031                                                                   EL6511
03032      IF PI-MAP-B                                                  EL6511
03033          GO TO 8200-SEND-DATAONLY                                 EL6511
03034      ELSE                                                         EL6511
03035          GO TO 8210-SEND-DATAONLY.                                EL6511
03036                                                                   EL6511
03037      GOBACK.                                                      EL6511
03038                                                                   EL6511
03039  9995-SECURITY-VIOLATION.                                         EL6511
03040                              COPY ELCSCTP.                        EL6511
03041                                                                   EL6511
03042  9995-EXIT.                                                       EL6511
03043       EXIT.                                                       EL6511
