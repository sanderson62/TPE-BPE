00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   ECS021T6
00003  PROGRAM-ID.                ECS021T6.                                LV009
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 11/28/95 11:04:16.                    CL**7
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             CL**8
00008 *                           VMOD=2.008.                              CL**9
00009                                                                   ECS021T6
00010                                                                   ECS021T6
00011 *AUTHOR.        LOGIC, INC.                                          CL**7
00012 *               DALLAS, TEXAS.                                       CL**7
00013                                                                   ECS021T6
00014                                                                   ECS021T6
00015 *DATE-COMPILED.                                                      CL**7
00016                                                                   ECS021T6
00017 *SECURITY.   *****************************************************   CL**7
00018 *            *                                                   *   CL**7
00019 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00020 *            *                                                   *   CL**7
00021 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00022 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00023 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00024 *            *                                                   *   CL**7
00025 *            *****************************************************   CL**7
00026                                                                   ECS021T6
00027 *REMARKS.                                                            CL**3
00028 *        THIS SUBMODULE WORKS IN CONJUNCTION WITH ECS021             CL**3
00029 *        TO ACCUMULATE FIGURES AND CREATE THE VARIOUS BREAK          CL**3
00030 *        LEVELS IN THE PROFIT ANALYSIS REPORT.                       CL**3
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 075 TO 900
092602******************************************************************
00031                                                                   ECS021T6
00032  ENVIRONMENT DIVISION.                                            ECS021T6
00033  INPUT-OUTPUT SECTION.                                            ECS021T6
00034  FILE-CONTROL.                                                    ECS021T6
00035                                                                   ECS021T6
00036  EJECT                                                            ECS021T6
00037  DATA DIVISION.                                                   ECS021T6
00038  FILE SECTION.                                                    ECS021T6
00039                                                                   ECS021T6
00040                                                                   ECS021T6
00041  EJECT                                                            ECS021T6
00042  WORKING-STORAGE SECTION.                                         ECS021T6
00043  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**7
00044                                                                   ECS021T6
00045  77  FILLER  PIC X(32) VALUE '********************************'.  ECS021T6
00046  77  FILLER  PIC X(32) VALUE '     ECS021T6 WORKING STORAGE   '.  ECS021T6
00047  77  FILLER  PIC X(32) VALUE '********* VMOD=2.008 ***********'.     CL**8
00048                                                                   ECS021T6
00049  77  NET-COUNT               PIC S9(7)     COMP-3.                   CL**5
00050  77  DTE-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T6
00051  77  TOT-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T6
00052  77  BEN-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T6
00053  77  BRK-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T6
00054  77  REQ-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T6
00055  77  ONE                     PIC  99              VALUE 1.        ECS021T6
00056  77  THREE                   PIC  99              VALUE 3.        ECS021T6
00057  77  SIXTEEN                 PIC S9(4)     COMP   VALUE +16.      ECS021T6
00058  77  THREE-HUNDRED           PIC S9(4)     COMP   VALUE +300.     ECS021T6
092602 77  NINE-HUNDRED            PIC S9(4)     COMP   VALUE +900.     ECS021T6
00059                                                                   ECS021T6
00060  01  WS-ABEND-STORAGE.                                            ECS021T6
00061      12  WS-RETURN-CODE          PIC S9(4)  VALUE +0 COMP.        ECS021T6
00062      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         ECS021T6
00063      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          ECS021T6
00064      12  WS-ZERO                 PIC S9     VALUE +0 COMP-3.      ECS021T6
00065      12  WS-ABEND-CODE           PIC S9(4).                       ECS021T6
00066      12  WORK-ABEND-CODE  REDEFINES  WS-ABEND-CODE.               ECS021T6
00067          16  WAC-1               PIC X.                           ECS021T6
00068          16  WAC-2               PIC X.                           ECS021T6
00069          16  WAC-3-4             PIC 99.                          ECS021T6
00070                                                                   ECS021T6
00071  01  WS-PROGRAM-ACTIONS.                                          ECS021T6
00072      12  W-BUILD-ZERO-TABLE      PIC X(02) VALUE 'BZ'.            ECS021T6
00073      12  W-ZERO-TABLE            PIC X(02) VALUE 'ZT'.            ECS021T6
00074      12  W-APPLY-SORT-RCD        PIC X(02) VALUE 'AS'.            ECS021T6
00075      12  W-ADD-LINK-TABLES       PIC X(02) VALUE 'AT'.            ECS021T6
00076      12  W-MOVE-TO-LINK          PIC X(02) VALUE 'MT'.            ECS021T6
00077                                                                   ECS021T6
00078      EJECT                                                        ECS021T6
00079  01  TABLE-IDENTIFIER.                                            ECS021T6
00080      12  FILLER                       PIC X(28)                   ECS021T6
00081                        VALUE '**** START OF TABLE TWO ****'.      ECS021T6
00082                                                                   ECS021T6
00083  01  BREAK-TABLE.                                                 ECS021T6
092602     12  TABLE-ACCUMULATORS     OCCURS  900 TIMES.                   CL**6
00085          16  TBL-BENEFIT-TYPE             PIC X.                  ECS021T6
092602         16  TBL-BENEFIT-CODE             PIC XX.                 ECS021T6
00087          16  TBL-PERIOD     OCCURS   15  TIMES  COMP-3.           ECS021T6
00088              20  TBL-ISS-CNT          PIC S9(7).                  ECS021T6
00089              20  TBL-CNC-CNT          PIC S9(7).                  ECS021T6
00090              20  TBL-ISS-PREM         PIC S9(11)V99.              ECS021T6
00091              20  TBL-CNC-PREM         PIC S9(11)V99.              ECS021T6
00092              20  TBL-NET-COMPEN       PIC S9(11)V99.              ECS021T6
00093              20  TBL-CLM-CNT          PIC S9(7).                  ECS021T6
00094              20  TBL-CLM-AMT          PIC S9(11)V99.              ECS021T6
00095              20  TBL-LOSS-RESV        PIC S9(11)V99.              ECS021T6
00096              20  TBL-EARND-PREM       PIC S9(9)V99.               ECS021T6
00097              20  TBL-PRM-INFRC        PIC S9(9)V99.               ECS021T6
00098              20  TBL-INFRC-CNT        PIC S9(9).                  ECS021T6
00099              20  TBL-AVG-AGE          PIC S9(9).                     CL**6
00100              20  TBL-AVG-ORG-TRM      PIC S9(9).                     CL**6
00101              20  TBL-WGHT-AGE         PIC S9(9).                     CL**6
00102              20  TBL-WGHT-ORG-TRM     PIC S9(9).                     CL**6
00103              20  TBL-EXP-PCT          PIC S9(3)V9(4).             ECS021T6
00104              20  TBL-ADDED-TO-CNT     PIC S9(5).                  ECS021T6
00105                                                                   ECS021T6
00106      EJECT                                                        ECS021T6
092602*01  BREAK-EXTENSION-1.                                           ECS021T6
092602*    12  TABLE-EXTENSION-1      OCCURS   75 TIMES.                   CL**6
092602*        16  FILLER                       PIC X.                  ECS021T6
092602*        16  FILLER                       PIC XX.                 ECS021T6
092602*        16  TBL-EXTENSION-1    OCCURS   15  TIMES  COMP-3.       ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(9)V99.               ECS021T6
092602*            20  FILLER               PIC S9(9)V99.               ECS021T6
092602*            20  FILLER               PIC S9(9).                  ECS021T6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(3)V9(4).             ECS021T6
092602*            20  FILLER               PIC S9(5).                  ECS021T6
092602*                                                                 ECS021T6
092602*    EJECT                                                        ECS021T6
092602*01  BREAK-EXTENSION-2.                                           ECS021T6
092602*    12  TABLE-EXTENSION-2      OCCURS   75 TIMES.                   CL**6
092602*        16  FILLER                       PIC X.                  ECS021T6
092602*        16  FILLER                       PIC XX.                 ECS021T6
092602*        16  TBL-EXTENSION-2    OCCURS   15  TIMES  COMP-3.       ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(9)V99.               ECS021T6
092602*            20  FILLER               PIC S9(9)V99.               ECS021T6
092602*            20  FILLER               PIC S9(9).                  ECS021T6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(3)V9(4).                CL**6
092602*            20  FILLER               PIC S9(5).                     CL**6
092602*                                                                    CL**6
092602*    EJECT                                                           CL**6
092602* 01  BREAK-EXTENSION-3.                                              CL**6
092602*     12  TABLE-EXTENSION-3      OCCURS   75 TIMES.                   CL**6
092602*         16  FILLER                       PIC X.                     CL**6
092602*         16  FILLER                       PIC XX.                    CL**6
092602*         16  TBL-EXTENSION-3    OCCURS   15  TIMES  COMP-3.          CL**6
092602*            20  FILLER               PIC S9(7).                     CL**2
092602*            20  FILLER               PIC S9(7).                     CL**2
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(7).                     CL**2
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(9)V99.                  CL**6
092602*            20  FILLER               PIC S9(9)V99.                  CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(3)V9(4).             ECS021T6
092602*            20  FILLER               PIC S9(5).                  ECS021T6
00177                                                                   ECS021T6
00178      EJECT                                                        ECS021T6
00179  01  TOTAL-IDENTIFIER.                                            ECS021T6
00180      12  FILLER                       PIC X(28)                   ECS021T6
00181                        VALUE '**** START OF TOTAL TWO ****'.      ECS021T6
00182                                                                   ECS021T6
00183  01  BREAK-TOTAL-TABLE.                                           ECS021T6
00184      12  TABLE-TOTALS           OCCURS  3 TIMES.                  ECS021T6
00185          16  TOT-PERIOD     OCCURS   15  TIMES  COMP-3.           ECS021T6
00186              20  TOT-ISS-CNT          PIC S9(7).                  ECS021T6
00187              20  TOT-CNC-CNT          PIC S9(7).                  ECS021T6
00188              20  TOT-SINGLE-ELEM      PIC S9(7).                  ECS021T6
00189              20  TOT-JOINT-RETRO      PIC S9(7).                  ECS021T6
00190              20  TOT-LIFE-LEVEL       PIC S9(7).                  ECS021T6
00191              20  TOT-ISS-PREM         PIC S9(11)V99.              ECS021T6
00192              20  TOT-CNC-PREM         PIC S9(11)V99.              ECS021T6
00193              20  TOT-NET-COMPEN       PIC S9(11)V99.              ECS021T6
00194              20  TOT-LF-CLM-CNT       PIC S9(7).                  ECS021T6
00195              20  TOT-LF-CLM-AMT       PIC S9(11)V99.              ECS021T6
00196              20  TOT-AH-CLM-CNT       PIC S9(7).                  ECS021T6
00197              20  TOT-AH-CLM-AMT       PIC S9(11)V99.              ECS021T6
00198              20  TOT-LOSS-RESV        PIC S9(11)V99.              ECS021T6
00199              20  TOT-EARND-PREM       PIC S9(9)V99.               ECS021T6
00200              20  TOT-PRM-INFRC        PIC S9(9)V99.               ECS021T6
00201              20  TOT-INFRC-CNT        PIC S9(9).                  ECS021T6
00202              20  TOT-AVG-AGE          PIC S9(9).                     CL**6
00203              20  TOT-AVG-ORG-TRM      PIC S9(9).                     CL**6
00204              20  TOT-WGHT-AGE         PIC S9(9).                     CL**6
00205              20  TOT-WGHT-ORG-TRM     PIC S9(9).                     CL**6
00206              20  TOT-EXP-PCT          PIC S999V9(4).              ECS021T6
00207              20  TOT-ADDED-TO-CNT     PIC S9(5).                  ECS021T6
00208                                                                   ECS021T6
00209      EJECT                                                        ECS021T6
00210                                                                   ECS021T6
00211  01  ZERO-IDENTIFIER.                                             ECS021T6
00212      12  FILLER                       PIC X(28)                   ECS021T6
00213                        VALUE '**** START OF ZERO TABLE****'.      ECS021T6
00214                                                                   ECS021T6
00215  01  ZERO-TABLE.                                                  ECS021T6
00216      12  ZERO-ACCUMULATORS.                                       ECS021T6
00217          16  ZERO-BENEFIT-TYPE            PIC X    VALUE SPACES.  ECS021T6
00218          16  ZERO-BENEFIT-CODE            PIC XX   VALUE ZEROS.   ECS021T6
00219          16  ZERO-PERIOD       OCCURS     15 TIMES COMP-3.        ECS021T6
00220              20  ZERO-ISS-CNT         PIC S9(7).                  ECS021T6
00221              20  ZERO-CNC-CNT         PIC S9(7).                  ECS021T6
00222              20  ZERO-ISS-PREM        PIC S9(11)V99.              ECS021T6
00223              20  ZERO-CNC-PREM        PIC S9(11)V99.              ECS021T6
00224              20  ZERO-NET-COMPEN      PIC S9(11)V99.              ECS021T6
00225              20  ZERO-CLM-CNT         PIC S9(7).                  ECS021T6
00226              20  ZERO-CLM-AMT         PIC S9(11)V99.              ECS021T6
00227              20  ZERO-LOSS-RESV       PIC S9(11)V99.              ECS021T6
00228              20  ZERO-EARND-PREM      PIC S9(9)V99.               ECS021T6
00229              20  ZERO-PRM-INFRC       PIC S9(9)V99.               ECS021T6
00230              20  ZERO-INFRC-CNT       PIC S9(9).                  ECS021T6
00231              20  ZERO-AVG-AGE         PIC S9(9).                     CL**6
00232              20  ZERO-AVG-ORG-TRM     PIC S9(9).                     CL**6
00233              20  ZERO-WGHT-AGE        PIC S9(9).                     CL**6
00234              20  ZERO-WGHT-ORG-TRM    PIC S9(9).                     CL**6
00235              20  ZERO-EXP-PCT         PIC S9(3)V9(4).             ECS021T6
00236              20  ZERO-ADDED-TO-CNT    PIC S9(5).                  ECS021T6
00237                                                                   ECS021T6
00238  01  ZERO-ENTRY-IDENTIFIER.                                       ECS021T6
00239      12  FILLER                   PIC X(28)                       ECS021T6
00240                            VALUE '****START OF ZERO ENTRY ****'.  ECS021T6
00241                                                                   ECS021T6
00242  01  ZERO-TABLE-ENTRIES.                                          ECS021T6
00243      12  ZERO-ACCUMS              COMP-3.                         ECS021T6
00244          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T6
00245          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T6
00246          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T6
00247          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T6
00248          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T6
00249          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T6
00250          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T6
00251          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T6
00252          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021T6
00253          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021T6
00254          16  FILLER               PIC S9(9)       VALUE +0.       ECS021T6
00255          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00256          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00257          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00258          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00259          16  FILLER               PIC S9(3)V9(4)  VALUE +0.       ECS021T6
00260          16  FILLER               PIC S9(5)       VALUE +0.       ECS021T6
00261                                                                   ECS021T6
00262      EJECT                                                        ECS021T6
00263                                                                   ECS021T6
00264  01  ZERO-TOTAL-IDENTIFIER.                                       ECS021T6
00265      12  FILLER                   PIC X(28)                       ECS021T6
00266                            VALUE '****START OF ZERO TOTAL ****'.  ECS021T6
00267                                                                   ECS021T6
00268  01  ZERO-TOTAL-TABLE.                                            ECS021T6
00269      12  ZERO-TOTAL-ACCUMS  OCCURS  15 TIMES  COMP-3.             ECS021T6
00270          16  FILLER                   PIC S9(7).                  ECS021T6
00271          16  FILLER                   PIC S9(7).                  ECS021T6
00272          16  FILLER                   PIC S9(7).                  ECS021T6
00273          16  FILLER                   PIC S9(7).                  ECS021T6
00274          16  FILLER                   PIC S9(7).                  ECS021T6
00275          16  FILLER                   PIC S9(11)V99.              ECS021T6
00276          16  FILLER                   PIC S9(11)V99.              ECS021T6
00277          16  FILLER                   PIC S9(11)V99.              ECS021T6
00278          16  FILLER                   PIC S9(7).                  ECS021T6
00279          16  FILLER                   PIC S9(11)V99.              ECS021T6
00280          16  FILLER                   PIC S9(7).                  ECS021T6
00281          16  FILLER                   PIC S9(11)V99.              ECS021T6
00282          16  FILLER                   PIC S9(11)V99.              ECS021T6
00283          16  FILLER                   PIC S9(9)V99.               ECS021T6
00284          16  FILLER                   PIC S9(9)V99.               ECS021T6
00285          16  FILLER                   PIC S9(9).                  ECS021T6
00286          16  FILLER                   PIC S9(9).                     CL**6
00287          16  FILLER                   PIC S9(9).                     CL**6
00288          16  FILLER                   PIC S9(9).                     CL**6
00289          16  FILLER                   PIC S9(9).                     CL**6
00290          16  FILLER                   PIC S999V9(4).              ECS021T6
00291          16  FILLER                   PIC S9(5).                  ECS021T6
00292                                                                   ECS021T6
00293  01  ZERO-TOTAL-ENTRY         COMP-3.                             ECS021T6
00294      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T6
00295      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T6
00296      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T6
00297      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T6
00298      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T6
00299      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T6
00300      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T6
00301      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T6
00302      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T6
00303      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T6
00304      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T6
00305      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T6
00306      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T6
00307      12  FILLER                       PIC S9(9)V99   VALUE +0.    ECS021T6
00308      12  FILLER                       PIC S9(9)V99   VALUE +0.    ECS021T6
00309      12  FILLER                       PIC S9(9)      VALUE +0.    ECS021T6
00310      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00311      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00312      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00313      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00314      12  FILLER                       PIC S999V9(4)  VALUE +0.    ECS021T6
00315      12  FILLER                       PIC S9(5)      VALUE +0.    ECS021T6
00316                                                                   ECS021T6
00317                                                                   ECS021T6
00318      EJECT                                                        ECS021T6
00319  LINKAGE SECTION.                                                 ECS021T6
00320                                                                   ECS021T6
00321 *************************************************                 ECS021T6
00322 *  ACTION REQUESTS:                             *                 ECS021T6
00323 *                                               *                 ECS021T6
00324 *  BZ - BUILD ZERO TABLE (1ST TIME PROCESSING)  *                 ECS021T6
00325 *  ZT - ZERO INTERNAL TABLES                    *                 ECS021T6
00326 *  AS - APPLY SORT RECORD TO INTERNAL TABLES    *                 ECS021T6
00327 *  AT - ADD LINKAGE TABLES TO INTERNAL TABLES   *                 ECS021T6
00328 *  MT - MOVE TABLES TO LINK TABLES              *                 ECS021T6
00329 *************************************************                 ECS021T6
00330                                                                   ECS021T6
00331  01  REQUEST-TABLE.                                               ECS021T6
00332      12  PROCESSING-REQUEST    OCCURS 10   PIC X(2).              ECS021T6
00333      12  NUMBER-OF-REQUESTS                PIC S9(04) COMP.       ECS021T6
00334                                                                   ECS021T6
00335  01  SW-RECORD.                                                   ECS021T6
00336      12  SW-REPORT-CONTROL-KEY.                                   ECS021T6
00337          16  SW-BREAK-FIELD-1             PIC X(10).              ECS021T6
00338          16  SW-BREAK-FIELD-2             PIC X(10).              ECS021T6
00339          16  SW-BREAK-FIELD-3             PIC X(10).              ECS021T6
00340          16  SW-BREAK-FIELD-4             PIC X(10).              ECS021T6
00341          16  SW-BREAK-FIELD-5             PIC X(10).              ECS021T6
00342          16  SW-BREAK-FIELD-6             PIC X(10).              ECS021T6
00343          16  SW-ACCT-NAME                 PIC X(30).              ECS021T6
121707         16  SW-ACCT-STATUS               PIC X.
00344          16  SW-GA-NAME                   PIC X(30).                 CL**3
00345          16  SW-STATE-NAME                PIC X(25).                 CL**4
00346          16  SW-PROD-DATE                 PIC X(6).               ECS021T6
00347                                                                   ECS021T6
00348      12  SW-RECORD-DATA            OCCURS 23.                        CL**6
00349          16  SW-BENEFIT-TYPE              PIC X.                  ECS021T6
00350          16  SW-BENEFIT-CODE              PIC 99.                 ECS021T6
00351          16  SW-BEN-TBL-POS               PIC S999 COMP.          ECS021T6
00352                                                                   ECS021T6
00353          16  SW-PERIOD    COMP-3   OCCURS 15.                     ECS021T6
00354              24  SW-ISS-CNT               PIC S9(7).              ECS021T6
00355              24  SW-CNC-CNT               PIC S9(7).              ECS021T6
00356              24  SW-ISS-PREM              PIC S9(11)V99.          ECS021T6
00357              24  SW-CNC-PREM              PIC S9(11)V99.          ECS021T6
00358              24  SW-NET-COMPEN            PIC S9(11)V99.          ECS021T6
00359              24  SW-CLM-CNT               PIC S9(7).              ECS021T6
00360              24  SW-CLM-AMT               PIC S9(11)V99.          ECS021T6
00361              24  SW-LOSS-RESV             PIC S9(11)V99.          ECS021T6
00362              24  SW-PRM-EARND             PIC S9(9)V99.           ECS021T6
00363              24  SW-PRM-INFRC             PIC S9(9)V99.           ECS021T6
00364              24  SW-INFRC-CNT             PIC S9(9).              ECS021T6
00365              24  SW-AVG-AGE               PIC S9(9).                 CL**6
00366              24  SW-AVG-ORG-TRM           PIC S9(9).                 CL**6
00367              24  SW-WGHT-AGE              PIC S9(9).                 CL**6
00368              24  SW-WGHT-ORG-TRM          PIC S9(9).                 CL**6
00369              24  SW-EXP-PCT               PIC S999V9(4).          ECS021T6
00370              24  SW-ADDED-TO-CNT          PIC S9(5).              ECS021T6
00371                                                                   ECS021T6
00372      EJECT                                                        ECS021T6
00373  01  LINK-TABLE.                                                  ECS021T6
092602     12  LINK-ACCUMULATORS   OCCURS  900 TIMES.                      CL**6
00375          16  LINK-BENEFIT-TYPE        PIC X.                      ECS021T6
00376          16  LINK-BENEFIT-CODE        PIC 99.                     ECS021T6
00377          16  LINK-PERIOD    OCCURS   15  TIMES  COMP-3.           ECS021T6
00378              20  LINK-ISS-CNT         PIC S9(7).                  ECS021T6
00379              20  LINK-CNC-CNT         PIC S9(7).                  ECS021T6
00380              20  LINK-ISS-PREM        PIC S9(11)V99.              ECS021T6
00381              20  LINK-CNC-PREM        PIC S9(11)V99.              ECS021T6
00382              20  LINK-NET-COMPEN      PIC S9(11)V99.              ECS021T6
00383              20  LINK-CLM-CNT         PIC S9(7).                  ECS021T6
00384              20  LINK-CLM-AMT         PIC S9(11)V99.              ECS021T6
00385              20  LINK-LOSS-RESV       PIC S9(11)V99.              ECS021T6
00386              20  LINK-EARND-PREM      PIC S9(9)V99.               ECS021T6
00387              20  LINK-PRM-INFRC       PIC S9(9)V99.               ECS021T6
00388              20  LINK-INFRC-CNT       PIC S9(9).                  ECS021T6
00389              20  LINK-AVG-AGE         PIC S9(9).                     CL**6
00390              20  LINK-AVG-ORG-TRM     PIC S9(9).                     CL**6
00391              20  LINK-WGHT-AGE        PIC S9(9).                     CL**6
00392              20  LINK-WGHT-ORG-TRM    PIC S9(9).                     CL**6
00393              20  LINK-EXP-PCT         PIC S999V9(4).              ECS021T6
00394              20  LINK-ADDED-TO-CNT    PIC S9(5).                  ECS021T6
00395                                                                   ECS021T6
092602*01  LINK-EXTENSION-1.                                            ECS021T6
092602*    12  LINK-TBL-EXTENSION-1    OCCURS   75 TIMES.                  CL**6
092602*        16  FILLER                       PIC X.                  ECS021T6
092602*        16  FILLER                       PIC 99.                 ECS021T6
092602*        16  LINK-EXT-1          OCCURS   15  TIMES  COMP-3.      ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(9)V99.               ECS021T6
092602*            20  FILLER               PIC S9(9)V99.               ECS021T6
092602*            20  FILLER               PIC S9(9).                  ECS021T6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(3)V9(4).             ECS021T6
092602*            20  FILLER               PIC S9(5).                  ECS021T6
092602*                                                                 ECS021T6
092602*01  LINK-EXTENSION-2.                                            ECS021T6
092602*    12  LINK-TBL-EXTENSION-2    OCCURS   75 TIMES.                  CL**6
092602*        16  FILLER                       PIC X.                  ECS021T6
092602*        16  FILLER                       PIC 99.                 ECS021T6
092602*        16  LINK-EXT-2          OCCURS   15  TIMES  COMP-3.      ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(7).                  ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(11)V99.              ECS021T6
092602*            20  FILLER               PIC S9(9)V99.               ECS021T6
092602*            20  FILLER               PIC S9(9)V99.               ECS021T6
092602*            20  FILLER               PIC S9(9).                  ECS021T6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(3)V9(4).                CL**6
092602*            20  FILLER               PIC S9(5).                     CL**6
092602*                                                                    CL**6
092602*01  LINK-EXTENSION-3.                                               CL**6
092602*    12  LINK-TBL-EXTENSION-3    OCCURS   75 TIMES.                  CL**6
092602*        16  FILLER                       PIC X.                     CL**6
092602*        16  FILLER                       PIC 99.                    CL**6
092602*        16  LINK-EXT-3          OCCURS   15  TIMES  COMP-3.         CL**6
092602*            20  FILLER               PIC S9(7).                     CL**2
092602*            20  FILLER               PIC S9(7).                     CL**2
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(7).                     CL**2
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(11)V99.                 CL**6
092602*            20  FILLER               PIC S9(9)V99.                  CL**6
092602*            20  FILLER               PIC S9(9)V99.                  CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(9).                     CL**6
092602*            20  FILLER               PIC S9(3)V9(4).             ECS021T6
092602*            20  FILLER               PIC S9(5).                  ECS021T6
00464                                                                   ECS021T6
00465                                                                   ECS021T6
00466      EJECT                                                        ECS021T6
00467  01  LINK-TOTAL-TABLE.                                            ECS021T6
00468      12  LINK-TOTALS     OCCURS  3 TIMES.                         ECS021T6
00469          16  LINK-T-PERIOD  COMP-3 OCCURS 15 TIMES.               ECS021T6
00470              20  LINK-T-ISS-CNT       PIC S9(7).                  ECS021T6
00471              20  LINK-T-CNC-CNT       PIC S9(7).                  ECS021T6
00472              20  LINK-T-SINGLE-ELEM   PIC S9(7).                  ECS021T6
00473              20  LINK-T-JOINT-RETRO   PIC S9(7).                  ECS021T6
00474              20  LINK-T-LIFE-LEVEL    PIC S9(7).                  ECS021T6
00475              20  LINK-T-ISS-PREM      PIC S9(11)V99.              ECS021T6
00476              20  LINK-T-CNC-PREM      PIC S9(11)V99.              ECS021T6
00477              20  LINK-T-NET-COMPEN    PIC S9(11)V99.              ECS021T6
00478              20  LINK-T-LF-CLM-CNT    PIC S9(7).                  ECS021T6
00479              20  LINK-T-LF-CLM-AMT    PIC S9(11)V99.              ECS021T6
00480              20  LINK-T-AH-CLM-CNT    PIC S9(7).                  ECS021T6
00481              20  LINK-T-AH-CLM-AMT    PIC S9(11)V99.              ECS021T6
00482              20  LINK-T-LOSS-RESV     PIC S9(11)V99.              ECS021T6
00483              20  LINK-T-EARND-PREM    PIC S9(9)V99.               ECS021T6
00484              20  LINK-T-PRM-INFRC     PIC S9(9)V99.               ECS021T6
00485              20  LINK-T-INFRC-CNT     PIC S9(9).                  ECS021T6
00486              20  LINK-T-AVG-AGE       PIC S9(9).                     CL**6
00487              20  LINK-T-AVG-ORG-TRM   PIC S9(9).                     CL**6
00488              20  LINK-T-WGHT-AGE      PIC S9(9).                     CL**6
00489              20  LINK-T-WGHT-ORG-TRM  PIC S9(9).                     CL**6
00490              20  LINK-T-EXP-PCT       PIC S999V9(4).              ECS021T6
00491              20  LINK-T-ADDED-TO-CNT  PIC S9(5).                  ECS021T6
00492                                                                   ECS021T6
00493      EJECT                                                        ECS021T6
00494  01  CLASIC-SYSTEM-CODES.                                         ECS021T6
00495      12  DTE-COLC-ID                 PIC  X(4).                   ECS021T6
00496      12  DTE-CLASIC-COMPANY-CD       PIC  X.                      ECS021T6
00497      12  DTE-CLASIC-COMPANY-NUMBER   PIC  999.                    ECS021T6
00498      12  DTE-CLASIC-CLAIM-ACCESS     PIC  X.                      ECS021T6
00499      12  CLASIC-REIN-MAINT           PIC  XX.                     ECS021T6
00500      12  CLASIC-COMP-MAINT           PIC  XX.                     ECS021T6
00501      12  CLASIC-ACCT-MAINT           PIC  XX.                     ECS021T6
00502      12  CLASIC-CTBL-MAINT           PIC  XX.                     ECS021T6
00503      12  CLASIC-RATE-MAINT           PIC  XX.                     ECS021T6
00504      12  CLASIC-CREDIT-EOM-DT        PIC  XX.                     ECS021T6
00505      12  CLASIC-CLAIMS-EOM-DT        PIC  XX.                     ECS021T6
00506                                                                   ECS021T6
00507      12  LIFE-OVERRIDE-L1            PIC  X.                      ECS021T6
00508      12  LIFE-OVERRIDE-L2            PIC  XX.                     ECS021T6
00509      12  LIFE-OVERRIDE-L6            PIC  X(6).                   ECS021T6
00510      12  LIFE-OVERRIDE-L12           PIC  X(12).                  ECS021T6
00511                                                                   ECS021T6
00512      12  AH-OVERRIDE-L1              PIC  X.                      ECS021T6
00513      12  AH-OVERRIDE-L2              PIC  XX.                     ECS021T6
00514      12  AH-OVERRIDE-L6              PIC  X(6).                   ECS021T6
00515      12  AH-OVERRIDE-L12             PIC  X(12).                  ECS021T6
00516                                                                   ECS021T6
00517      12  CLAS-REPORT-CD1-CAPTION     PIC  X(10).                  ECS021T6
00518      12  CLAS-REPORT-CD2-CAPTION     PIC  X(10).                  ECS021T6
00519                                                                   ECS021T6
00520      12  CLASIC-MORTG-EOM-DT         PIC  XX.                     ECS021T6
00521      12  CLASIC-AR-EOM-DT            PIC  XX.                     ECS021T6
00522                                                                   ECS021T6
00523      12  FILLER                      PIC  X(11).                  ECS021T6
00524                                                                   ECS021T6
00525                                                                   ECS021T6
00526  01  CLAS-INS-TYPES.                                              ECS021T6
092602     12 CLAS-ALL-TYPES               OCCURS 900 TIMES.            ECS021T6
00528          16  CLAS-I-BEN              PIC  XX.                     ECS021T6
00529          16  CLAS-I-AB3.                                          ECS021T6
00530              20  FILLER              PIC  X.                      ECS021T6
00531              20  CLAS-I-AB2.                                      ECS021T6
00532                  24  FILLER          PIC  X.                      ECS021T6
00533                  24  CLAS-I-AB1      PIC  X.                      ECS021T6
00534          16  CLAS-I-AB10.                                         ECS021T6
00535              20  FILLER              PIC  X(9).                   ECS021T6
00536              20  CLAS-I-REIN-YN      PIC  X.                      ECS021T6
00537          16  CLAS-I-COMMENT          PIC  X(10).                  ECS021T6
00538          16  CLAS-I-JOINT            PIC  X.                      ECS021T6
00539          16  CLAS-I-RL-AH            PIC  X.                      ECS021T6
00540          16  CLAS-I-CALC-TYPE.                                    ECS021T6
00541              20  CLAS-I-BAL          PIC  X.                      ECS021T6
00542          16  CLAS-I-EP               PIC  X.                      ECS021T6
00543          16  FILLER                  PIC  X(9).                   ECS021T6
00544                                                                   ECS021T6
00545  01  CLAS-INDEX-TBL.                                              ECS021T6
00546      12  CLAX-ID                     PIC X(4).                    ECS021T6
00547      12  CLAS-STARTC                 PIC S9(4) COMP.              ECS021T6
00548      12  CLAS-MAXC                   PIC S9(4) COMP.              ECS021T6
00549      12  CLAS-STARTL                 PIC S9(4) COMP.              ECS021T6
00550      12  CLAS-MAXL                   PIC S9(4) COMP.              ECS021T6
00551      12  CLAS-STARTA                 PIC S9(4) COMP.              ECS021T6
00552      12  CLAS-MAXA                   PIC S9(4) COMP.              ECS021T6
00553      12  CLAS-STARTM                 PIC S9(4) COMP.              ECS021T6
00554      12  CLAS-MAXM                   PIC S9(4) COMP.              ECS021T6
00555      12  CLAS-STARTB                 PIC S9(4) COMP.              ECS021T6
00556      12  CLAS-MAXB                   PIC S9(4) COMP.              ECS021T6
00557      12  CLAS-STARTS                 PIC S9(4) COMP.              ECS021T6
00558      12  CLAS-MAXS                   PIC S9(4) COMP.              ECS021T6
00559      12  CLAS-STARTE                 PIC S9(4) COMP.              ECS021T6
00560      12  CLAS-MAXE                   PIC S9(4) COMP.              ECS021T6
00561      12  CLAS-STARTCN                PIC S9(4) COMP.              ECS021T6
00562      12  CLAS-MAXCN                  PIC S9(4) COMP.              ECS021T6
00563                                                                   ECS021T6
00564      EJECT                                                        ECS021T6
00565  PROCEDURE DIVISION USING REQUEST-TABLE                           ECS021T6
00566                           SW-RECORD                               ECS021T6
00567                           LINK-TABLE                              ECS021T6
092602*                         LINK-EXTENSION-1                        ECS021T6
092602*                         LINK-EXTENSION-2                        ECS021T6
092602*                         LINK-EXTENSION-3                           CL**6
00571                           LINK-TOTAL-TABLE                        ECS021T6
00572                           CLASIC-SYSTEM-CODES                     ECS021T6
00573                           CLAS-INS-TYPES                          ECS021T6
00574                           CLAS-INDEX-TBL.                         ECS021T6
00575                                                                   ECS021T6
00576  1000-PROCESS-REQUESTS.                                           ECS021T6
00577                                                                   ECS021T6
00578      PERFORM 2000-READ-REQUEST-TABLE THRU 2000-EXIT               ECS021T6
00579          VARYING REQ-IDX  FROM  ONE BY ONE                        ECS021T6
00580              UNTIL REQ-IDX GREATER THAN NUMBER-OF-REQUESTS.       ECS021T6
00581                                                                   ECS021T6
00582      GOBACK.                                                      ECS021T6
00583                                                                   ECS021T6
00584  1000-EXIT.                                                       ECS021T6
00585      EXIT.                                                        ECS021T6
00586                                                                   ECS021T6
00587  2000-READ-REQUEST-TABLE.                                         ECS021T6
00588                                                                   ECS021T6
00589      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-BUILD-ZERO-TABLE     ECS021T6
00590          PERFORM 9000-INITIALIZE-ZERO-TABLE THRU 9000-EXIT        ECS021T6
00591          PERFORM 8000-ZERO-ENTIRE-TABLE THRU 8000-EXIT            ECS021T6
00592          GO TO 2000-EXIT.                                         ECS021T6
00593                                                                   ECS021T6
00594      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-ZERO-TABLE           ECS021T6
00595          PERFORM 8000-ZERO-ENTIRE-TABLE THRU 8000-EXIT            ECS021T6
00596          GO TO 2000-EXIT.                                         ECS021T6
00597                                                                   ECS021T6
00598      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-APPLY-SORT-RCD       ECS021T6
00599          PERFORM 5000-APPLY-SORT-RECORD THRU 5000-EXIT            ECS021T6
00600          GO TO 2000-EXIT.                                         ECS021T6
00601                                                                   ECS021T6
00602      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-ADD-LINK-TABLES      ECS021T6
00603          PERFORM 3000-ADD-TABLE-RTN THRU 3000-EXIT                ECS021T6
00604          PERFORM 4000-ADD-TOTAL-RTN THRU 4000-EXIT                ECS021T6
00605          GO TO 2000-EXIT.                                         ECS021T6
00606                                                                   ECS021T6
00607      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-MOVE-TO-LINK         ECS021T6
00608          PERFORM 7000-MOVE-TABLES THRU 7000-EXIT.                 ECS021T6
00609                                                                   ECS021T6
00610  2000-EXIT.                                                       ECS021T6
00611      EXIT.                                                        ECS021T6
00612                                                                   ECS021T6
00613      EJECT                                                        ECS021T6
00614 *******************************************                       ECS021T6
00615 *  (2000) ADDS TABLE (2) TO LINK TABLE    *                       ECS021T6
00616 *******************************************                       ECS021T6
00617                                                                   ECS021T6
00618  3000-ADD-TABLE-RTN.                                              ECS021T6
00619      MOVE +1                     TO BEN-IDX.                      ECS021T6
00620                                                                   ECS021T6
00621  3000-ZERO-ACCESS-LOOP.                                           ECS021T6
00622      IF BEN-IDX GREATER CLAS-MAXA                                 ECS021T6
00623          GO TO 3000-EXIT.                                         ECS021T6
00624                                                                   ECS021T6
00625      IF LINK-BENEFIT-CODE (BEN-IDX) = ZERO                        ECS021T6
00626          ADD +1                  TO BEN-IDX                       ECS021T6
00627          GO TO 3000-ZERO-ACCESS-LOOP.                             ECS021T6
00628                                                                   ECS021T6
00629      MOVE LINK-BENEFIT-TYPE (BEN-IDX)                             ECS021T6
00630                                  TO TBL-BENEFIT-TYPE (BEN-IDX).   ECS021T6
00631      MOVE LINK-BENEFIT-CODE (BEN-IDX)                             ECS021T6
00632                                  TO TBL-BENEFIT-CODE (BEN-IDX).   ECS021T6
00633                                                                   ECS021T6
00634      MOVE +1                     TO DTE-IDX.                      ECS021T6
00635                                                                   ECS021T6
00636  3000-ACTIVE-BENEFIT-LOOP.                                        ECS021T6
00637                                                                   ECS021T6
00638      ADD LINK-ISS-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T6
00639                               TBL-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T6
00640      ADD LINK-CNC-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T6
00641                               TBL-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T6
00642      ADD LINK-ISS-PREM    (BEN-IDX DTE-IDX) TO                    ECS021T6
00643                               TBL-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T6
00644      ADD LINK-CNC-PREM    (BEN-IDX DTE-IDX) TO                    ECS021T6
00645                               TBL-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T6
00646      ADD LINK-NET-COMPEN  (BEN-IDX DTE-IDX) TO                    ECS021T6
00647                               TBL-NET-COMPEN   (BEN-IDX DTE-IDX). ECS021T6
00648      ADD LINK-CLM-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T6
00649                               TBL-CLM-CNT      (BEN-IDX DTE-IDX). ECS021T6
00650      ADD LINK-CLM-AMT     (BEN-IDX DTE-IDX) TO                    ECS021T6
00651                               TBL-CLM-AMT      (BEN-IDX DTE-IDX). ECS021T6
00652      ADD LINK-LOSS-RESV   (BEN-IDX DTE-IDX) TO                    ECS021T6
00653                               TBL-LOSS-RESV    (BEN-IDX DTE-IDX). ECS021T6
00654      ADD LINK-EARND-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T6
00655                               TBL-EARND-PREM   (BEN-IDX DTE-IDX). ECS021T6
00656      ADD LINK-PRM-INFRC   (BEN-IDX DTE-IDX) TO                    ECS021T6
00657                               TBL-PRM-INFRC    (BEN-IDX DTE-IDX). ECS021T6
00658      ADD LINK-INFRC-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T6
00659                               TBL-INFRC-CNT    (BEN-IDX DTE-IDX). ECS021T6
00660      ADD LINK-AVG-AGE      (BEN-IDX DTE-IDX) TO                   ECS021T6
00661                               TBL-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T6
00662      ADD LINK-AVG-ORG-TRM (BEN-IDX DTE-IDX) TO                    ECS021T6
00663                               TBL-AVG-ORG-TRM (BEN-IDX DTE-IDX).  ECS021T6
00664      ADD LINK-WGHT-AGE    (BEN-IDX DTE-IDX) TO                    ECS021T6
00665                               TBL-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T6
00666      ADD LINK-WGHT-ORG-TRM(BEN-IDX DTE-IDX) TO                    ECS021T6
00667                               TBL-WGHT-ORG-TRM(BEN-IDX DTE-IDX).  ECS021T6
00668      ADD LINK-EXP-PCT     (BEN-IDX DTE-IDX) TO                    ECS021T6
00669                               TBL-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T6
00670      ADD LINK-ADDED-TO-CNT(BEN-IDX DTE-IDX) TO                    ECS021T6
00671                               TBL-ADDED-TO-CNT(BEN-IDX DTE-IDX).  ECS021T6
00672                                                                   ECS021T6
00673      ADD +1 TO DTE-IDX.                                           ECS021T6
00674                                                                   ECS021T6
00675      IF DTE-IDX LESS +16                                          ECS021T6
00676          GO TO 3000-ACTIVE-BENEFIT-LOOP                           ECS021T6
00677      ELSE                                                         ECS021T6
00678          ADD +1 TO BEN-IDX                                        ECS021T6
00679          GO TO 3000-ZERO-ACCESS-LOOP.                             ECS021T6
00680                                                                   ECS021T6
00681  3000-EXIT.                                                       ECS021T6
00682       EXIT.                                                       ECS021T6
00683                                                                   ECS021T6
00684      EJECT                                                        ECS021T6
00685 *******************************************************           ECS021T6
00686 *  (3050) DUMPS TOTAL TABLE (2) INTO TOTAL TABLE (1)  *           ECS021T6
00687 *******************************************************           ECS021T6
00688  4000-ADD-TOTAL-RTN.                                              ECS021T6
00689      MOVE +1                     TO BEN-IDX                       ECS021T6
00690                                     DTE-IDX.                      ECS021T6
00691                                                                   ECS021T6
00692  4000-TOTAL-BENEFITS-LOOP.                                        ECS021T6
00693      ADD LINK-T-ISS-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T6
00694                               TOT-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T6
00695      ADD LINK-T-CNC-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T6
00696                               TOT-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T6
00697      ADD LINK-T-SINGLE-ELEM (BEN-IDX DTE-IDX) TO                  ECS021T6
00698                               TOT-SINGLE-ELEM (BEN-IDX DTE-IDX).     CL**2
00699      ADD LINK-T-JOINT-RETRO (BEN-IDX DTE-IDX) TO                  ECS021T6
00700                               TOT-JOINT-RETRO (BEN-IDX DTE-IDX).     CL**2
00701      ADD LINK-T-LIFE-LEVEL (BEN-IDX DTE-IDX) TO                   ECS021T6
00702                               TOT-LIFE-LEVEL (BEN-IDX DTE-IDX).   ECS021T6
00703      ADD LINK-T-ISS-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T6
00704                               TOT-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T6
00705      ADD LINK-T-CNC-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T6
00706                               TOT-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T6
00707      ADD LINK-T-NET-COMPEN (BEN-IDX DTE-IDX) TO                   ECS021T6
00708                               TOT-NET-COMPEN (BEN-IDX DTE-IDX).   ECS021T6
00709      ADD LINK-T-LF-CLM-CNT (BEN-IDX DTE-IDX) TO                   ECS021T6
00710                               TOT-LF-CLM-CNT (BEN-IDX DTE-IDX).   ECS021T6
00711      ADD LINK-T-LF-CLM-AMT (BEN-IDX DTE-IDX) TO                   ECS021T6
00712                               TOT-LF-CLM-AMT (BEN-IDX DTE-IDX).   ECS021T6
00713      ADD LINK-T-AH-CLM-CNT (BEN-IDX DTE-IDX) TO                   ECS021T6
00714                               TOT-AH-CLM-CNT (BEN-IDX DTE-IDX).   ECS021T6
00715      ADD LINK-T-AH-CLM-AMT (BEN-IDX DTE-IDX) TO                   ECS021T6
00716                               TOT-AH-CLM-AMT (BEN-IDX DTE-IDX).   ECS021T6
00717      ADD LINK-T-LOSS-RESV (BEN-IDX DTE-IDX) TO                    ECS021T6
00718                               TOT-LOSS-RESV (BEN-IDX DTE-IDX).    ECS021T6
00719      ADD LINK-T-EARND-PREM (BEN-IDX DTE-IDX) TO                   ECS021T6
00720                               TOT-EARND-PREM (BEN-IDX DTE-IDX).   ECS021T6
00721      ADD LINK-T-PRM-INFRC (BEN-IDX DTE-IDX) TO                    ECS021T6
00722                               TOT-PRM-INFRC (BEN-IDX DTE-IDX).    ECS021T6
00723      ADD LINK-T-INFRC-CNT (BEN-IDX DTE-IDX) TO                    ECS021T6
00724                               TOT-INFRC-CNT (BEN-IDX DTE-IDX).    ECS021T6
00725      ADD LINK-T-AVG-AGE    (BEN-IDX DTE-IDX) TO                   ECS021T6
00726                               TOT-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T6
00727      ADD LINK-T-AVG-ORG-TRM (BEN-IDX DTE-IDX) TO                  ECS021T6
00728                               TOT-AVG-ORG-TRM (BEN-IDX DTE-IDX).     CL**2
00729      ADD LINK-T-WGHT-AGE  (BEN-IDX DTE-IDX) TO                    ECS021T6
00730                               TOT-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T6
00731      ADD LINK-T-WGHT-ORG-TRM(BEN-IDX DTE-IDX) TO                  ECS021T6
00732                               TOT-WGHT-ORG-TRM(BEN-IDX DTE-IDX).     CL**2
00733      ADD LINK-T-EXP-PCT   (BEN-IDX DTE-IDX) TO                    ECS021T6
00734                               TOT-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T6
00735      ADD LINK-T-ADDED-TO-CNT(BEN-IDX DTE-IDX) TO                  ECS021T6
00736                               TOT-ADDED-TO-CNT(BEN-IDX DTE-IDX).     CL**2
00737                                                                   ECS021T6
00738      ADD +1 TO DTE-IDX.                                           ECS021T6
00739                                                                   ECS021T6
00740      IF DTE-IDX LESS +16                                          ECS021T6
00741          GO TO 4000-TOTAL-BENEFITS-LOOP.                          ECS021T6
00742                                                                   ECS021T6
00743      IF BEN-IDX LESS +3                                           ECS021T6
00744          ADD +1                  TO BEN-IDX                       ECS021T6
00745          MOVE +1                 TO DTE-IDX                       ECS021T6
00746          GO TO 4000-TOTAL-BENEFITS-LOOP.                          ECS021T6
00747                                                                   ECS021T6
00748  4000-EXIT.                                                       ECS021T6
00749       EXIT.                                                       ECS021T6
00750                                                                   ECS021T6
00751      EJECT                                                        ECS021T6
00752 **************************************************************    ECS021T6
00753 *  THE FOLLOWING INDEXES ARE USED IN ROUTINES 5000 AND 6000  *    ECS021T6
00754 *                                                            *    ECS021T6
00755 *  1. BRK-IDX - USED TO INDEX BENEFIT LEVEL OF SORT RECORD   *    ECS021T6
00756 *                 VARIED FROM 1 TO 23                        *       CL**6
00757 *  2. BEN-IDX - USED TO INDEX BENEFIT LEVEL OF ACCUMULATED   *    ECS021T6
00758 *                 BREAK TABLES VARIED FROM 1 TO 75           *       CL**6
00759 *  3. TOT-IDX - USED TO INDEX BENEFIT TYPE OF TOTALS         *    ECS021T6
00760 *                 TABLES VARIED FROM 1 TO 2                  *    ECS021T6
00761 *  4. DTE-IDX - USED TO INDEX DATE RANGES OF ALL TABLES      *    ECS021T6
00762 *                 AND RECORDS VARIED FROM 1 TO 15            *    ECS021T6
00763 **************************************************************    ECS021T6
00764                                                                   ECS021T6
00765  5000-APPLY-SORT-RECORD.                                          ECS021T6
00766      MOVE +1                     TO BRK-IDX.                      ECS021T6
00767                                                                   ECS021T6
00768  5000-BENEFIT-LOOP.                                               ECS021T6
00769      IF SW-BENEFIT-CODE   (BRK-IDX) = ZEROS                       ECS021T6
00770          GO TO 5000-EXIT.                                         ECS021T6
00771                                                                   ECS021T6
00772      MOVE SW-BEN-TBL-POS  (BRK-IDX)                               ECS021T6
00773                                  TO BEN-IDX.                      ECS021T6
00774      MOVE SW-BENEFIT-CODE (BRK-IDX)                               ECS021T6
00775                                  TO TBL-BENEFIT-CODE (BEN-IDX).   ECS021T6
00776      MOVE SW-BENEFIT-TYPE (BRK-IDX)                               ECS021T6
00777                                  TO TBL-BENEFIT-TYPE (BEN-IDX).   ECS021T6
00778      MOVE +1                     TO DTE-IDX.                      ECS021T6
00779                                                                   ECS021T6
00780  5000-DATE-RANGE-LOOP.                                            ECS021T6
00781      ADD SW-ISS-CNT     (BRK-IDX DTE-IDX)                         ECS021T6
00782                            TO TBL-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T6
00783      ADD SW-CNC-CNT     (BRK-IDX DTE-IDX)                         ECS021T6
00784                            TO TBL-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T6
00785      ADD SW-ISS-PREM    (BRK-IDX DTE-IDX)                         ECS021T6
00786                            TO TBL-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T6
00787      ADD SW-CNC-PREM    (BRK-IDX DTE-IDX)                         ECS021T6
00788                            TO TBL-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T6
00789      ADD SW-NET-COMPEN  (BRK-IDX DTE-IDX)                         ECS021T6
00790                            TO TBL-NET-COMPEN   (BEN-IDX DTE-IDX). ECS021T6
00791      ADD SW-CLM-CNT     (BRK-IDX DTE-IDX)                         ECS021T6
00792                            TO TBL-CLM-CNT      (BEN-IDX DTE-IDX). ECS021T6
00793      ADD SW-CLM-AMT     (BRK-IDX DTE-IDX)                         ECS021T6
00794                            TO TBL-CLM-AMT      (BEN-IDX DTE-IDX). ECS021T6
00795      ADD SW-LOSS-RESV   (BRK-IDX DTE-IDX)                         ECS021T6
00796                            TO TBL-LOSS-RESV    (BEN-IDX DTE-IDX). ECS021T6
00797      ADD SW-PRM-EARND   (BRK-IDX DTE-IDX)                         ECS021T6
00798                            TO TBL-EARND-PREM   (BEN-IDX DTE-IDX). ECS021T6
00799      ADD SW-PRM-INFRC   (BRK-IDX DTE-IDX)                         ECS021T6
00800                            TO TBL-PRM-INFRC    (BEN-IDX DTE-IDX). ECS021T6
00801      ADD SW-INFRC-CNT   (BRK-IDX DTE-IDX)                         ECS021T6
00802                            TO TBL-INFRC-CNT    (BEN-IDX DTE-IDX). ECS021T6
00803      ADD SW-AVG-AGE     (BRK-IDX DTE-IDX)                         ECS021T6
00804                            TO TBL-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T6
00805      ADD SW-AVG-ORG-TRM (BRK-IDX DTE-IDX)                         ECS021T6
00806                            TO TBL-AVG-ORG-TRM (BEN-IDX DTE-IDX).  ECS021T6
00807      ADD SW-WGHT-AGE    (BRK-IDX DTE-IDX)                         ECS021T6
00808                            TO TBL-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T6
00809      ADD SW-WGHT-ORG-TRM(BRK-IDX DTE-IDX)                         ECS021T6
00810                            TO TBL-WGHT-ORG-TRM(BEN-IDX DTE-IDX).  ECS021T6
00811      ADD SW-EXP-PCT     (BRK-IDX DTE-IDX)                         ECS021T6
00812                            TO TBL-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T6
00813      ADD SW-ADDED-TO-CNT(BRK-IDX DTE-IDX)                         ECS021T6
00814                            TO TBL-ADDED-TO-CNT(BEN-IDX DTE-IDX).  ECS021T6
00815                                                                   ECS021T6
00816      ADD +1                TO DTE-IDX.                            ECS021T6
00817                                                                   ECS021T6
00818      IF DTE-IDX LESS +16                                          ECS021T6
00819          GO TO 5000-DATE-RANGE-LOOP.                              ECS021T6
00820                                                                   ECS021T6
00821      PERFORM 6000-BENEFIT-TYPE-TOTAL THRU 6000-EXIT.              ECS021T6
00822                                                                   ECS021T6
00823      ADD +1                TO BRK-IDX.                            ECS021T6
00824                                                                   ECS021T6
00825      IF BRK-IDX LESS +24                                             CL**6
00826          GO TO 5000-BENEFIT-LOOP.                                 ECS021T6
00827                                                                   ECS021T6
00828  5000-EXIT.                                                       ECS021T6
00829       EXIT.                                                       ECS021T6
00830                                                                   ECS021T6
00831      EJECT                                                        ECS021T6
00832 **************************************************************    ECS021T6
00833 *  THE FOLLOWING INDEXES ARE USED IN ROUTINES 5000 AND 6000  *    ECS021T6
00834 *                                                            *    ECS021T6
00835 *  1. BRK-IDX - USED TO INDEX BENEFIT LEVEL OF SORT RECORD   *    ECS021T6
00836 *                 VARIED FROM 1 TO 23                        *       CL**6
00837 *  2. BEN-IDX - USED TO INDEX BENEFIT LEVEL OF ACCUMULATED   *    ECS021T6
00838 *                 BREAK TABLES VARIED FROM 1 TO 75           *       CL**6
00839 *  3. TOT-IDX - USED TO INDEX BENEFIT TYPE OF TOTALS         *    ECS021T6
00840 *                 TABLES VARIED FROM 1 TO 2                  *    ECS021T6
00841 *  4. DTE-IDX - USED TO INDEX DATE RANGES OF ALL TABLES      *    ECS021T6
00842 *                 AND RECORDS VARIED FROM 1 TO 15            *    ECS021T6
00843 **************************************************************    ECS021T6
00844                                                                   ECS021T6
00845  6000-BENEFIT-TYPE-TOTAL.                                         ECS021T6
00846                                                                   ECS021T6
00847      MOVE +1                     TO DTE-IDX.                      ECS021T6
00848                                                                   ECS021T6
00849  6000-DATE-RANGE-LOOP.                                            ECS021T6
00850      COMPUTE NET-COUNT = SW-ISS-CNT (BRK-IDX DTE-IDX) -              CL**5
00851          SW-CNC-CNT (BRK-IDX DTE-IDX).                               CL**5
00852                                                                   ECS021T6
00853      IF SW-BENEFIT-TYPE (BRK-IDX) = LIFE-OVERRIDE-L1              ECS021T6
00854          MOVE +1                 TO TOT-IDX                       ECS021T6
00855          IF CLAS-I-RL-AH (BEN-IDX) = 'R'                          ECS021T6
00856              IF CLAS-I-JOINT (BEN-IDX) = 'J'                      ECS021T6
00857                  ADD NET-COUNT TO                                    CL**5
00858                      TOT-JOINT-RETRO (TOT-IDX DTE-IDX)            ECS021T6
00859              ELSE                                                 ECS021T6
00860                  ADD NET-COUNT TO                                    CL**5
00861                      TOT-SINGLE-ELEM (TOT-IDX DTE-IDX)            ECS021T6
00862          ELSE                                                     ECS021T6
00863              ADD NET-COUNT TO                                        CL**5
00864                  TOT-LIFE-LEVEL (TOT-IDX DTE-IDX)                 ECS021T6
00865      ELSE                                                         ECS021T6
00866          MOVE +2                 TO TOT-IDX                       ECS021T6
00867          IF CLAS-I-AB1 (BEN-IDX) = 'E'                            ECS021T6
00868              ADD NET-COUNT TO                                        CL**5
00869                  TOT-SINGLE-ELEM (TOT-IDX DTE-IDX)                ECS021T6
00870          ELSE                                                     ECS021T6
00871              IF CLAS-I-AB1 (BEN-IDX) = 'R'                        ECS021T6
00872                  ADD NET-COUNT TO                                    CL**5
00873                      TOT-JOINT-RETRO (TOT-IDX DTE-IDX).           ECS021T6
00874                                                                   ECS021T6
00875      ADD SW-ISS-CNT    (BRK-IDX DTE-IDX)                          ECS021T6
00876                            TO TOT-ISS-CNT     (TOT-IDX DTE-IDX)   ECS021T6
00877                               TOT-ISS-CNT     (3 DTE-IDX).        ECS021T6
00878      ADD SW-CNC-CNT    (BRK-IDX DTE-IDX)                          ECS021T6
00879                            TO TOT-CNC-CNT     (TOT-IDX DTE-IDX)   ECS021T6
00880                               TOT-CNC-CNT     (3 DTE-IDX).        ECS021T6
00881      ADD SW-ISS-PREM   (BRK-IDX DTE-IDX)                          ECS021T6
00882                            TO TOT-ISS-PREM    (TOT-IDX DTE-IDX)   ECS021T6
00883                               TOT-ISS-PREM    (3 DTE-IDX).        ECS021T6
00884      ADD SW-CNC-PREM   (BRK-IDX DTE-IDX)                          ECS021T6
00885                            TO TOT-CNC-PREM    (TOT-IDX DTE-IDX)   ECS021T6
00886                               TOT-CNC-PREM    (3 DTE-IDX).        ECS021T6
00887      ADD SW-NET-COMPEN (BRK-IDX DTE-IDX)                          ECS021T6
00888                            TO TOT-NET-COMPEN (TOT-IDX DTE-IDX)    ECS021T6
00889                               TOT-NET-COMPEN (3 DTE-IDX).         ECS021T6
00890                                                                   ECS021T6
00891      IF SW-BENEFIT-TYPE (BRK-IDX) = LIFE-OVERRIDE-L1              ECS021T6
00892          ADD SW-CLM-CNT (BRK-IDX DTE-IDX)                         ECS021T6
00893                            TO TOT-LF-CLM-CNT   (TOT-IDX DTE-IDX)  ECS021T6
00894                               TOT-LF-CLM-CNT   (3 DTE-IDX)        ECS021T6
00895          ADD SW-CLM-AMT (BRK-IDX DTE-IDX)                         ECS021T6
00896                            TO TOT-LF-CLM-AMT   (TOT-IDX DTE-IDX)  ECS021T6
00897                               TOT-LF-CLM-AMT   (3 DTE-IDX).       ECS021T6
00898                                                                   ECS021T6
00899      IF SW-BENEFIT-TYPE (BRK-IDX) = AH-OVERRIDE-L1                ECS021T6
00900          ADD SW-CLM-CNT (BRK-IDX DTE-IDX)                         ECS021T6
00901                            TO TOT-AH-CLM-CNT   (TOT-IDX DTE-IDX)  ECS021T6
00902                               TOT-AH-CLM-CNT   (3 DTE-IDX)        ECS021T6
00903          ADD SW-CLM-AMT (BRK-IDX DTE-IDX)                         ECS021T6
00904                            TO TOT-AH-CLM-AMT   (TOT-IDX DTE-IDX)  ECS021T6
00905                               TOT-AH-CLM-AMT   (3 DTE-IDX).       ECS021T6
00906                                                                   ECS021T6
00907      ADD SW-LOSS-RESV   (BRK-IDX DTE-IDX)                         ECS021T6
00908                            TO TOT-LOSS-RESV    (TOT-IDX DTE-IDX)  ECS021T6
00909                               TOT-LOSS-RESV    (3 DTE-IDX).       ECS021T6
00910      ADD SW-PRM-EARND   (BRK-IDX DTE-IDX)                         ECS021T6
00911                            TO TOT-EARND-PREM   (TOT-IDX DTE-IDX)  ECS021T6
00912                               TOT-EARND-PREM   (3 DTE-IDX).       ECS021T6
00913      ADD SW-PRM-INFRC   (BRK-IDX DTE-IDX)                         ECS021T6
00914                            TO TOT-PRM-INFRC    (TOT-IDX DTE-IDX)  ECS021T6
00915                               TOT-PRM-INFRC    (3 DTE-IDX).       ECS021T6
00916      ADD SW-INFRC-CNT   (BRK-IDX DTE-IDX)                         ECS021T6
00917                            TO TOT-INFRC-CNT    (TOT-IDX DTE-IDX)  ECS021T6
00918                               TOT-INFRC-CNT    (3 DTE-IDX).       ECS021T6
00919      ADD SW-AVG-AGE     (BRK-IDX DTE-IDX)                         ECS021T6
00920                            TO TOT-AVG-AGE      (TOT-IDX DTE-IDX)  ECS021T6
00921                               TOT-AVG-AGE      (3 DTE-IDX).       ECS021T6
00922      ADD SW-AVG-ORG-TRM (BRK-IDX DTE-IDX)                         ECS021T6
00923                            TO TOT-AVG-ORG-TRM (TOT-IDX DTE-IDX)   ECS021T6
00924                               TOT-AVG-ORG-TRM (3 DTE-IDX).        ECS021T6
00925      ADD SW-WGHT-AGE    (BRK-IDX DTE-IDX)                         ECS021T6
00926                            TO TOT-WGHT-AGE     (TOT-IDX DTE-IDX)  ECS021T6
00927                               TOT-WGHT-AGE     (3 DTE-IDX).       ECS021T6
00928      ADD SW-WGHT-ORG-TRM(BRK-IDX DTE-IDX)                         ECS021T6
00929                            TO TOT-WGHT-ORG-TRM(TOT-IDX DTE-IDX)   ECS021T6
00930                               TOT-WGHT-ORG-TRM(3 DTE-IDX).        ECS021T6
00931      ADD SW-EXP-PCT     (BRK-IDX DTE-IDX)                         ECS021T6
00932                            TO TOT-EXP-PCT      (TOT-IDX DTE-IDX)  ECS021T6
00933                               TOT-EXP-PCT      (3 DTE-IDX).       ECS021T6
00934      ADD SW-ADDED-TO-CNT (BRK-IDX DTE-IDX)                        ECS021T6
00935                            TO TOT-ADDED-TO-CNT(TOT-IDX DTE-IDX)   ECS021T6
00936                               TOT-ADDED-TO-CNT(3 DTE-IDX).        ECS021T6
00937                                                                   ECS021T6
00938      ADD +1 TO DTE-IDX.                                           ECS021T6
00939                                                                   ECS021T6
00940      IF DTE-IDX LESS +16                                          ECS021T6
00941          GO TO 6000-DATE-RANGE-LOOP.                              ECS021T6
00942                                                                   ECS021T6
00943  6000-EXIT.                                                       ECS021T6
00944       EXIT.                                                       ECS021T6
00945                                                                   ECS021T6
00946      EJECT                                                        ECS021T6
00947  7000-MOVE-TABLES.                                                ECS021T6
00948                                                                   ECS021T6
00949      MOVE BREAK-TABLE            TO LINK-TABLE.                   ECS021T6
092602*    MOVE BREAK-EXTENSION-1      TO LINK-EXTENSION-1.             ECS021T6
092602*    MOVE BREAK-EXTENSION-2      TO LINK-EXTENSION-2.             ECS021T6
092602*    MOVE BREAK-EXTENSION-3      TO LINK-EXTENSION-3.                CL**6
00953      MOVE BREAK-TOTAL-TABLE      TO LINK-TOTAL-TABLE.             ECS021T6
00954                                                                   ECS021T6
00955  7000-EXIT.                                                       ECS021T6
00956       EXIT.                                                       ECS021T6
00957                                                                   ECS021T6
00958      EJECT                                                        ECS021T6
00959 ***********************************                               ECS021T6
00960 *     (8000) ZERO TABLE TWO       *                               ECS021T6
00961 ***********************************                               ECS021T6
00962                                                                   ECS021T6
00963  8000-ZERO-ENTIRE-TABLE.                                          ECS021T6
00964                                                                   ECS021T6
00965       MOVE +1                    TO BEN-IDX.                      ECS021T6
00966                                                                   ECS021T6
00967  8000-ZERO-BENEFIT-LOOP.                                          ECS021T6
00968                                                                   ECS021T6
00969       MOVE ZERO-TABLE            TO                               ECS021T6
00970                                  TABLE-ACCUMULATORS (BEN-IDX).    ECS021T6
00971                                                                   ECS021T6
092602*     IF BEN-IDX LESS THAN THREE-HUNDRED                          ECS021T6
00972       IF BEN-IDX LESS THAN  NINE-HUNDRED                          ECS021T6
00973           ADD +1                 TO BEN-IDX                       ECS021T6
00974               GO TO 8000-ZERO-BENEFIT-LOOP.                       ECS021T6
00975                                                                   ECS021T6
00976  8000-ZERO-TOTAL-TABLE.                                           ECS021T6
00977                                                                   ECS021T6
00978       MOVE +1                    TO TOT-IDX.                      ECS021T6
00979                                                                   ECS021T6
00980  8000-ZERO-TOTAL-LOOP.                                            ECS021T6
00981                                                                   ECS021T6
00982       MOVE ZERO-TOTAL-TABLE      TO TABLE-TOTALS (TOT-IDX).       ECS021T6
00983                                                                   ECS021T6
00984       IF TOT-IDX LESS THAN THREE                                  ECS021T6
00985           ADD +1                 TO TOT-IDX                       ECS021T6
00986               GO TO 8000-ZERO-TOTAL-LOOP.                         ECS021T6
00987                                                                   ECS021T6
00988  8000-EXIT.                                                       ECS021T6
00989      EXIT.                                                        ECS021T6
00990                                                                   ECS021T6
00991      EJECT                                                        ECS021T6
00992 *******************************************                       ECS021T6
00993 *     (9000) INITIALIZE ZERO-ACCUMS       *                       ECS021T6
00994 *******************************************                       ECS021T6
00995                                                                   ECS021T6
00996  9000-INITIALIZE-ZERO-TABLE.                                      ECS021T6
00997                                                                   ECS021T6
00998       MOVE +1                    TO DTE-IDX.                      ECS021T6
00999                                                                   ECS021T6
01000  9000-ZERO-DATE-LOOP.                                             ECS021T6
01001                                                                   ECS021T6
01002       MOVE ZERO-TABLE-ENTRIES    TO ZERO-PERIOD (DTE-IDX).        ECS021T6
01003                                                                   ECS021T6
01004       IF DTE-IDX LESS THAN SIXTEEN                                ECS021T6
01005           ADD +1                 TO DTE-IDX                       ECS021T6
01006               GO TO 9000-ZERO-DATE-LOOP.                          ECS021T6
01007                                                                   ECS021T6
01008       MOVE +1                    TO DTE-IDX.                      ECS021T6
01009                                                                   ECS021T6
01010  9000-ZERO-TOTAL-DATE-LOOP.                                       ECS021T6
01011                                                                   ECS021T6
01012       MOVE ZERO-TOTAL-ENTRY      TO ZERO-TOTAL-ACCUMS (DTE-IDX).  ECS021T6
01013                                                                   ECS021T6
01014       IF DTE-IDX LESS THAN SIXTEEN                                ECS021T6
01015           ADD +1                 TO DTE-IDX                       ECS021T6
01016               GO TO 9000-ZERO-TOTAL-DATE-LOOP.                    ECS021T6
01017                                                                   ECS021T6
01018  9000-EXIT.                                                       ECS021T6
01019      EXIT.                                                        ECS021T6
01020                                                                   ECS021T6
01021      EJECT                                                        ECS021T6
01022  ABEND-PGM SECTION.                                               ECS021T6
01023            COPY ELCABEND SUPPRESS.                                ECS021T6
