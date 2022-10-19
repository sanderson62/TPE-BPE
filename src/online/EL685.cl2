00001  IDENTIFICATION DIVISION.                                         11/27/96
00002                                                                   EL685
00003  PROGRAM-ID.                 EL685 .                                 LV018
00004 *              PROGRAM CONVERTED BY                                  CL*13
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*13
00006 *              CONVERSION DATE 07/20/95 07:57:46.                    CL*13
00007 *                            VMOD=2.018                              CL*18
00008 *                                                                 EL685
00009 *AUTHOR.    LOGIC, INC.                                              CL*13
00010 *           DALLAS, TEXAS.                                           CL*13
00011                                                                   EL685
00012 *DATE-COMPILED.                                                      CL*13
00013                                                                   EL685
00014 *SECURITY.   *****************************************************   CL*13
00015 *            *                                                   *   CL*13
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*13
00017 *            *                                                   *   CL*13
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*13
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*13
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*13
00021 *            *                                                   *   CL*13
00022 *            *****************************************************   CL*13
00023                                                                   EL685
00024 *    SKIP3                                                           CL*13
00025 *REMARKS.    TRANSACTION -  EXG1                                     CL**3
00026                                                                   EL685
00027 *        THIS PROGRAM PRODUCES A REPORT SHOWING ALL CHECKS THAT      CL**3
00028 *    ARE WAITING TO BE PRINTED.                                      CL**3
00029                                                                   EL685
00030 *        THIS PROGRAM MAY BE ENTERED INTO THRU THE CREDIT OR         CL**4
00031 *                   ACCOUNTS RECEIVABLE SYSTEMS.                     CL**4
00032                                                                      CL**4
00033 *    SCREENS     - EL685A - CHECKS TO BE PRINTED                     CL*10
00034 *                  EL685B - A/R CHECKS TO BE PRINTED                 CL*10
00035 *                  EL850C - CHECKS PRINTED                           CL*10
00036 *                  EL685D - A/R CHECKS PRINTED                       CL*10
00037                                                                   EL685
00038 *    ENTERED BY  - EL671  - REPORT MENU                              CL**3
00039 *                    OR                                              CL**4
00040 *                  EL850  - ACCOUNTS RECEIVABLE MENU                 CL**4
00041                                                                   EL685
00042 *    EXIT TO     - EL671  - RESULT OF CLEAR OR END OF JOB            CL**3
00043 *                    OR                                              CL**4
00044 *                  EL850  - RESULT OF CLEAR OR END OF JOB            CL**4
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
030612******************************************************************
00045                                                                   EL685
00046                                                                   EL685
00047      EJECT                                                        EL685
00048  ENVIRONMENT DIVISION.                                            EL685
00049                                                                   EL685
00050  DATA DIVISION.                                                   EL685
00051                                                                   EL685
00052  WORKING-STORAGE SECTION.                                         EL685
00053                                                                   EL685
00054  77  FILLER  PIC X(32)  VALUE '********************************'. EL685
00055  77  FILLER  PIC X(32)  VALUE '*   EL685  WORKING STORAGE     *'. EL685
00056  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.018 *********'.    CL*18
00057                                                                   EL685
00058  01  FILLER                          COMP-3.                      EL685
00059      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.  EL685
00060                                                                   EL685
00061      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.  EL685
00062      05  WS-TIME                     REDEFINES                    EL685
00063          WS-TIME-WORK                PIC S9(3)V9(4).              EL685
00064                                                                   EL685
00065  01  FILLER                          COMP SYNC.                   EL685
00066      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1920. EL685
00067                                                                   EL685
00068  01  FILLER.                                                      EL685
00069      05  CNTL-KEY.                                                EL685
00070          10  CNTL-CO                 PIC X(3).                    EL685
00071          10  CNTL-RECORD-TYPE        PIC X           VALUE '1'.   EL685
00072          10  CNTL-GENL               PIC X(4).                    EL685
00073          10  CNTL-SEQ                PIC S9(4) VALUE +0      COMP.EL685
00074                                                                   EL685
00075      05  WS-START-CNTLNO             PIC S9(8)   VALUE +0    COMP.EL685
00076                                                                   EL685
00077      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL685S'.EL685
00078      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL685A'.EL685
00079                                                                   EL685
00080      05  FILLER                      REDEFINES                    EL685
00081          WS-MAP-NAME.                                             EL685
00082          20  FILLER                  PIC XX.                      EL685
00083          20  WS-MAP-NUMBER           PIC X(6).                    EL685
00084                                                                   EL685
00085      05  WS-PROGRAM-ID               PIC X(8)      VALUE 'EL685'. EL685
00086                                                                   EL685
00087      05  WS-CHECK-QUEUE-DSID         PIC X(8) VALUE 'ERCHKQ'.     EL685
00088      05  WS-COMCK-QUEUE-DSID         PIC X(8) VALUE 'ERCMKQ'.        CL**4
00089      05  WS-CONTROL-DSID             PIC X(8) VALUE 'ELCNTL'.     EL685
00090                                                                   EL685
00091      05  WS-TRANS-ID                 PIC X(4) VALUE 'EXG1'.       EL685
00092      05  WS-PRINT-TRAN-ID            PIC X(4) VALUE 'EXG9'.       EL685
00093      05  WS-PRINTER-ID               PIC X(4).                    EL685
00094                                                                   EL685
00095      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70    EL685
00096                                      COMP                         EL685
00097                                      SYNCHRONIZED.                EL685
00098                                                                   EL685
00099      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.EL685
00100                                                                   EL685
00101      05  WS-SUCCESSFUL-MESSAGE       PIC X(79)       VALUE        EL685
00102          '0000 TRANSACTION SUCCESSFUL'.                           EL685
00103                                                                   EL685
00104      05  WS-PAYEE-DESC               PIC X(08)       VALUE           CL**4
00105          'PAYEE   '.                                                 CL**4
00106                                                                   EL685
00107      05  WS-PAYEE-SEQ-DESC           PIC X(07)       VALUE           CL**4
00108          'SEQ    '.                                                  CL**4
00109                                                                      CL**4
00110      05  WS-FINRESP-DESC             PIC X(08)       VALUE           CL**4
00111          'FIN.RESP'.                                                 CL**4
00112                                                                      CL**4
00113      05  WS-ACCOUNT-DESC             PIC X(07)       VALUE           CL**4
00114          'ACCOUNT'.                                                  CL**4
00115                                                                      CL**4
00116      05  WS-TO-BE-PRINTED-DESC       PIC X(28)       VALUE           CL**4
00117          '-   CHECKS TO BE PRINTED   -'.                             CL**4
00118                                                                      CL**4
00119      05  WS-CHECKS-PRINTED-DESC      PIC X(28)       VALUE           CL**4
00120          '-      PRINTED CHECKS      -'.                             CL**4
00121                                                                      CL**4
00122      05  WS-AR-TO-BE-PRINTED-DESC    PIC X(28)       VALUE           CL**4
00123          '- A/R CHECKS TO BE PRINTED -'.                             CL**4
00124                                                                      CL**4
00125      05  WS-AR-CHECKS-PRINTED-DESC   PIC X(28)       VALUE           CL**4
00126          '-    A/R PRINTED CHECKS    -'.                             CL**4
00127                                                                   EL685
00128      05  WS-TO-BE-PRINTED-PFDESC     PIC X(29)       VALUE        EL685
00129          'PF4=LIST PRINTED CHECKS      '.                         EL685
00130                                                                   EL685
00131      05  WS-CHECKS-PRINTED-PFDESC    PIC X(29)       VALUE        EL685
00132          'PF4=LIST TO BE PRINTED CHECKS'.                         EL685
00133                                                                   EL685
00134      05  WS-SAVE-CHECK-MODE          PIC X           VALUE SPACE. EL685
00135                                                                   EL685
00136      05  WS-SAVE-AR-MODE             PIC X           VALUE SPACE.    CL**4
00137                                                                      CL**4
00138      05  WS-TEMP-STORAGE-KEY.                                     EL685
00139          10  WS-TS-TERM-ID           PIC X(4).                    EL685
00140          10  FILLER                  PIC X(4)        VALUE '685'. EL685
00141                                                                   EL685
00142      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO   EL685
00143                                      COMP                         EL685
00144                                      SYNCHRONIZED.                EL685
00145                                                                   EL685
00146  01  HOLD-CHECK-RECORD.                                              CL*13
00147      12  HOLD-RECORD-ID              PIC XX.                         CL*13
00148          88  VALID-HOLD-ID                   VALUE 'MQ'.             CL*13
00149                                                                      CL*13
00150      12  HOLD-CONTROL-PRIMARY.                                       CL*13
00151          16  HOLD-COMPANY-CD         PIC X.                          CL*13
00152          16  HOLD-CONTROL-NUMBER     PIC S9(8)   COMP VALUE ZEROS.   CL*14
00153          16  HOLD-SEQUENCE-NUMBER    PIC S9(4)   COMP VALUE ZEROS.   CL*14
00154                                                                      CL*13
00155      12  HOLD-CONTROL-BY-PAYEE.                                      CL*13
00156          16  HOLD-COMPANY-CD-A1      PIC X.                          CL*13
00157          16  HOLD-CSR-A1             PIC X(4).                       CL*13
00158          16  HOLD-CARRIER-A1         PIC X.                          CL*13
00159          16  HOLD-GROUPING-A1        PIC X(6).                       CL*13
00160          16  HOLD-PAYEE-A1           PIC X(10).                      CL*13
00161          16  HOLD-PAYEE-SEQ-A1       PIC S9(4)   COMP VALUE ZEROS.   CL*14
00162          16  HOLD-CONTROL-NUMBER-A1  PIC S9(8)   COMP VALUE ZEROS.   CL*14
00163          16  HOLD-SEQUENCE-NUMBER-A1 PIC S9(4)   COMP VALUE ZEROS.   CL*14
00164                                                                      CL*13
00165      12  HOLD-ENTRY-TYPE             PIC X.                          CL*13
00166                                                                      CL*13
00167      12  FILLER                      PIC X(10).                      CL*13
00168                                                                      CL*13
00169      12  HOLD-CREDIT-CHEK-CNTL.                                      CL*13
00170          16  HOLD-CHEK-CSR           PIC X(4).                       CL*13
00171          16  HOLD-CHEK-CARRIER       PIC X.                          CL*13
00172          16  HOLD-CHEK-GROUPING      PIC X(6).                       CL*13
00173          16  HOLD-CHEK-PAYEE         PIC X(10).                      CL*13
00174          16  HOLD-CHEK-PAYEE-SEQ     PIC S9(4)   COMP VALUE ZEROS.   CL*14
00175          16  HOLD-CHEK-SEQ-NO        PIC S9(4)   COMP VALUE ZEROS.   CL*14
00176                                                                      CL*13
00177      12  FILLER                      PIC X(10).                      CL*13
00178                                                                      CL*13
00179      12  HOLD-PAYEE-INFO.                                            CL*13
00180          16  HOLD-PAYEE-NAME         PIC X(30).                      CL*13
00181          16  HOLD-PAYEE-ADDRESS-1    PIC X(30).                      CL*13
00182          16  HOLD-PAYEE-ADDRESS-2    PIC X(30).                      CL*13
00183          16  HOLD-PAYEE-CITY-ST      PIC X(30).                      CL*13
00184          16  HOLD-PAYEE-ZIP-CODE.                                    CL*13
00185              20  HOLD-PAYEE-ZIP.                                     CL*13
00186                  24  FILLER          PIC X(1).                       CL*13
00187                  24  FILLER          PIC X(4).                       CL*13
00188              20  HOLD-PAYEE-ZIP-EXT  PIC X(4).                       CL*13
00189                                                                      CL*13
00190      12  HOLD-CREDIT-PYAJ-CNTL.                                      CL*13
00191          16  HOLD-PYAJ-CARRIER       PIC X.                          CL*13
00192          16  HOLD-PYAJ-GROUPING      PIC X(6).                       CL*13
00193          16  HOLD-PYAJ-FIN-RESP      PIC X(10).                      CL*13
00194          16  FILLER                  PIC X(6).                       CL*13
00195                                                                      CL*13
00196      12  HOLD-CHECK-NUMBER           PIC X(6).                       CL*13
00197      12  HOLD-CHECK-AMOUNT           PIC S9(7)V99                    CL*14
00198                                                COMP-3 VALUE ZEROS.   CL*14
00199      12  HOLD-NUMBER-OF-CK-STUBS     PIC S9(3)                       CL*14
00200                                                COMP-3 VALUE ZEROS.   CL*14
00201      12  HOLD-VOID-DT                PIC XX.                         CL*13
00202      12  HOLD-TIMES-PRINTED          PIC S9(4)   COMP VALUE ZEROS.   CL*14
00203      12  HOLD-PRINT-AT-HHMM          PIC S9(4)   COMP VALUE ZEROS.   CL*14
00204      12  HOLD-CHECK-BY-USER          PIC X(4).                       CL*13
00205      12  HOLD-PRE-NUMBERING-SW       PIC X.                          CL*13
00206                                                                      CL*13
00207      12  HOLD-CHECK-WRITTEN-DT       PIC XX.                         CL*13
00208      12  HOLD-LAST-MAINT-BY          PIC X(4).                       CL*13
00209      12  HOLD-LAST-MAINT-HHMMSS      PIC S9(6) COMP-3 VALUE ZEROS.   CL*14
00210      12  HOLD-LAST-MAINT-DT          PIC XX.                         CL*13
00211      12  HOLD-CHECK-RELEASE-DT       PIC XX.                         CL*13
00212      12  HOLD-RECORD-TYPE            PIC X.                          CL*13
00213                                                                      CL*13
00214      12  HOLD-DETAIL-INFORMATION.                                    CL*13
00215          16  HOLD-DETAIL-INFO      OCCURS 15 TIMES.                  CL*13
00216              20  HOLD-CHECK-STUB-LINE.                               CL*13
00217                  24  HOLD-STUB-COMMENT      PIC X(23).               CL*13
00218                  24  HOLD-ACCT-AGENT        PIC X(10).               CL*13
00219                  24  HOLD-INVOICE           PIC X(6).                CL*13
00220                  24  HOLD-REFERENCE         PIC X(12).               CL*13
00221                  24  HOLD-LEDGER-NO         PIC X(14).               CL*13
00222                  24  HOLD-PYAJ-AMT          PIC S9(7)V99             CL*14
00223                                                COMP-3 VALUE ZEROS.   CL*14
00224                  24  HOLD-PYAJ-REC-TYPE     PIC X.                   CL*13
00225                  24  HOLD-PYAJ-SEQ          PIC S9(8)                CL*14
00226                                                COMP VALUE ZEROS.     CL*14
00227                  24  HOLD-PAYMENT-TYPE      PIC X.                   CL*13
00228                  24  HOLD-PYAJ-PMT-APPLIED  PIC X.                   CL*13
00229                  24  HOLD-LAST-MAINT-APPLIED PIC X.                  CL*13
00230                  24  HOLD-NON-AR-ITEM       PIC X.                   CL*13
00231                  24  FILLER                 PIC X(19).               CL*13
00232                                                                      CL*13
00233      12  HOLD-CHECK-STUB-TEXT REDEFINES HOLD-DETAIL-INFORMATION.     CL*13
00234          16  HOLD-CHECK-TEXT-ITEMS OCCURS 3 TIMES.                   CL*13
00235              20  HOLD-STUB-TEXT      PIC X(70).                      CL*13
00236          16  HOLD-STUB-FILLER        PIC X(1260).                    CL*13
00237                                                                      CL*13
00238      12  HOLD-CREDIT-SELECT-DATE     PIC XX.                         CL*13
00239      12  HOLD-CREDIT-ACCEPT-DATE     PIC XX.                         CL*13
00240                                                                      CL*13
00241      12  HOLD-AR-STATEMENT-DT        PIC XX.                         CL*13
00242      12  HOLD-CO-TYPE                PIC X.                          CL*13
00243                                                                      CL*13
00244      12  HOLD-STARTING-CHECK-NUMBER  PIC X(06).                      CL*13
00245      12  FILLER                      PIC X(41).                      CL*13
00246 ******************************************************************   CL*13
00247                                                                      CL**4
00248      EJECT                                                        EL685
00249                              COPY ELCINTF.                           CL**8
00250                                                                   EL685
00251      12  FILLER                      REDEFINES                    EL685
00252          PI-PROGRAM-WORK-AREA.                                    EL685
00253          16  PI-CHECK-QUE-KEY.                                    EL685
00254              20  PI-CK-COMPANY-CODE  PIC X.                       EL685
00255              20  PI-CK-CONTROL-NO    PIC S9(8)          COMP.     EL685
00256              20  PI-CK-SEQUENCE-NO   PIC S9(4)          COMP.     EL685
00257                                                                   EL685
00258          16  PI-PREV-CHECK-QUE-KEY.                               EL685
00259              20  PI-PREV-CK-COMPANY-CODE     PIC X.               EL685
00260              20  PI-PREV-CK-CONTROL-NO       PIC S9(8) COMP.      EL685
00261              20  PI-PREV-CK-SEQUENCE-NO      PIC S9(4) COMP.      EL685
00262                                                                   EL685
00263                                                                   EL685
00264          16  PI-TEMP-STORAGE-ITEM    PIC S9(4)                    EL685
00265                                      COMP                         EL685
00266                                      SYNCHRONIZED.                EL685
00267                                                                   EL685
00268          16  PI-END-OF-FILE          PIC S9                       EL685
00269                                      COMP-3.                      EL685
00270                                                                   EL685
00271          16 PI-CONTROL-TOTALS.                                    EL685
00272             20 PI-CONTROL-TOT        PIC S9(7)V99 COMP-3.         EL685
00273             20 PI-CONTROL-GRAND-TOT  PIC S9(7)V99 COMP-3.         EL685
00274             20 PI-CONTROL-SAVE-CONTROL                            EL685
00275                                      PIC S9(8) COMP.              EL685
00276          16 PI-FIRST-TIME-SW         PIC X.                          CL*13
00277             88 PI-FIRST-TIME         VALUE 'Y'.                   EL685
00278             88 PI-NOT-FIRST-TIME     VALUE 'N'.                   EL685
00279          16 PI-EOF-SWT               PIC X.                       EL685
00280             88 PI-EOF                VALUE 'Y'.                   EL685
00281             88 PI-NOT-EOF            VALUE 'N'.                   EL685
00282          16 PI-SEND-TOT-SWT          PIC X.                       EL685
00283             88 PI-SEND-TOT           VALUE 'Y'.                   EL685
00284             88 PI-NOT-SEND-TOT       VALUE 'N'.                   EL685
00285          16 PI-CHECK-MODE            PIC X.                          CL**4
00286             88  CHECKS-PRINTED       VALUE 'Y'.                   EL685
00287             88  CHECKS-TO-BE-PRINTED VALUE SPACE.                 EL685
00288          16 PI-CURRENT-DATE-BIN      PIC XX.                         CL**4
00289          16 PI-CURRENT-DATE          PIC X(8).                       CL**4
00290          16 PI-AR-MODE               PIC X.                          CL**4
00291             88 PI-CALLED-FROM-AR-MENU VALUE 'A'.                     CL**4
00292          16 PI-PREV-DISPLAY-SWT      PIC X.                          CL**5
00293             88 PI-TOTAL-SCREEN       VALUE 'T'.                      CL**5
00294          16 PI-START-CONTROL-NO      PIC S9(08)  COMP.               CL**9
00295          16 PI-FIRST-TOTAL-SW        PIC X.                          CL*13
00296             88 PI-FIRST-TOTAL        VALUE 'Y'.                      CL*13
00297          16 PI-PAGING-SW             PIC X.                          CL*14
00298             88 PI-PAGE-FORWARD       VALUE 'Y'.                      CL*14
00299          16 FILLER                   PIC X(587).                     CL*14
00300      EJECT                                                        EL685
00301              COPY EL685S.                                            CL**8
00302                                                                   EL685
00303  01  FILLER                          REDEFINES                       CL**4
00304      EL685BI.                                                        CL**4
00305                                                                      CL**4
00306      05  FILLER                      PIC X(78).                      CL*10
00307                                                                      CL**4
00308      05  FILLER                      OCCURS 18 TIMES                 CL**4
00309                                      INDEXED BY EL685B-INDEX.        CL**4
00310                                                                      CL**4
00311          15  EL685B-CONTROL-LENGTH   PIC S9(4)                       CL**4
00312                                      COMP.                           CL**4
00313          15  EL685B-CONTROL-ATTRB    PIC X.                          CL**4
00314          15  EL685B-CONTROL          PIC 9(7).                       CL**4
00315                                                                      CL**4
00316          15  EL685B-CHECK-NO-LENGTH  PIC S9(4)                       CL**4
00317                                      COMP.                           CL**4
00318          15  EL685B-CHECK-NO-ATTRB   PIC X.                          CL**4
00319          15  EL685B-CHECK-NO         PIC X(6).                       CL**4
00320                                                                      CL**4
00321          15  EL685B-CHK-DT-LENGTH    PIC S9(4)                       CL**4
00322                                      COMP.                           CL**4
00323          15  EL685B-CHK-DT-ATTRB     PIC X.                          CL**4
00324          15  EL685B-CHK-DT           PIC X(08).                      CL**4
00325                                                                      CL**4
00326          15  EL685B-CARRIER-LENGTH   PIC S9(4)                       CL**4
00327                                      COMP.                           CL**4
00328          15  EL685B-CARRIER-ATTRB    PIC X.                          CL**4
00329          15  EL685B-CARRIER          PIC X.                          CL**4
00330                                                                      CL**4
00331          15  EL685B-GROUPING-LENGTH  PIC S9(4)                       CL**4
00332                                      COMP.                           CL**4
00333          15  EL685B-GROUPING-ATTRB   PIC X.                          CL**4
00334          15  EL685B-GROUPING         PIC X(6).                       CL**4
00335          15  EL685B-GROUPING-RDF                                     CL**4
00336              REDEFINES EL685B-GROUPING.                              CL**4
00337              20  EL685B-DESC-ONE     PIC X(6).                       CL**4
00338                                                                      CL**4
00339          15  EL685B-PAYEE-LENGTH     PIC S9(4)                       CL**4
00340                                      COMP.                           CL**4
00341          15  EL685B-PAYEE-ATTRB      PIC X.                          CL**4
00342          15  EL685B-PAYEE            PIC X(10).                      CL**4
00343          15  EL685B-PAYEE-RDF                                        CL**4
00344              REDEFINES EL685B-PAYEE.                                 CL**4
00345              20  EL685B-RDF-AREA.                                    CL**4
00346                  25  EL685B-DESC-TWO PIC X(5).                       CL**4
00347                  25  FILLER          PIC X(5).                       CL**4
00348                                                                      CL**4
00349          15  EL685B-PAYEE-SEQ-LENGTH PIC S9(4)                       CL**4
00350                                      COMP.                           CL**4
00351          15  EL685B-PAYEE-SEQ-ATTRB  PIC X.                          CL**4
00352          15  EL685B-PAYEE-SEQ        PIC ZZZ9.                       CL**4
00353                                                                      CL**4
00354          15  EL685B-PAYEE-NA-LENGTH  PIC S9(4)                       CL**4
00355                                      COMP.                           CL**4
00356          15  EL685B-PAYEE-NA-ATTRB   PIC X.                          CL**4
00357          15  EL685B-PAYEE-NA         PIC X(12).                      CL**4
00358                                                                      CL**4
00359          15  EL685B-AMT-LENGTH       PIC S9(4)                       CL**4
00360                                      COMP.                           CL**4
00361          15  EL685B-AMT-ATTRB        PIC X.                          CL**4
00362          15  EL685B-AMT              PIC Z,ZZZ,ZZ9.99-.              CL**4
00363                                                                      CL**6
00364  01  FILLER                          REDEFINES                       CL**6
00365      EL685AI.                                                        CL**6
00366                                                                      CL**6
00367      05  FILLER                      PIC X(100).                     CL*11
00368                                                                      CL**6
00369      05  FILLER                      OCCURS 18 TIMES                 CL**6
00370                                      INDEXED BY EL685A-INDEX.        CL**6
00371                                                                      CL**6
00372          15  EL685A-CONTROL-LENGTH   PIC S9(4)                       CL**6
00373                                      COMP.                           CL**6
00374          15  EL685A-CONTROL-ATTRB    PIC X.                          CL**6
00375          15  EL685A-CONTROL          PIC 9(7).                       CL**6
00376                                                                      CL**6
00377          15  EL685A-CHECK-NO-LENGTH  PIC S9(4)                       CL**6
00378                                      COMP.                           CL**6
00379          15  EL685A-CHECK-NO-ATTRB   PIC X.                          CL**6
00380          15  EL685A-CHECK-NO         PIC X(7).                       CL**6
00381                                                                      CL**6
00382          15  EL685A-PMT-TYPE-LENGTH  PIC S9(4)                       CL**6
00383                                      COMP.                           CL**6
00384          15  EL685A-PMT-TYPE-ATTRB   PIC X.                          CL**6
00385          15  EL685A-PMT-TYPE         PIC X(11).                      CL**6
00386                                                                      CL**6
00387          15  EL685A-CARRIER-LENGTH   PIC S9(4)                       CL**6
00388                                      COMP.                           CL**6
00389          15  EL685A-CARRIER-ATTRB    PIC X.                          CL**6
00390          15  EL685A-CARRIER          PIC X.                          CL**6
00391                                                                      CL**6
00392          15  EL685A-GROUPING-LENGTH  PIC S9(4)                       CL**6
00393                                      COMP.                           CL**6
00394          15  EL685A-GROUPING-ATTRB   PIC X.                          CL**6
00395          15  EL685A-GROUPING         PIC X(6).                       CL**6
00396                                                                      CL**6
00397          15  EL685A-FIN-RESP-LENGTH  PIC S9(4)                       CL**6
00398                                      COMP.                           CL**6
00399          15  EL685A-FIN-RESP-ATTRB   PIC X.                          CL**6
00400          15  EL685A-FIN-RESP         PIC X(10).                      CL**6
00401          15  EL685A-FIN-RESP-RDF                                     CL**6
00402              REDEFINES EL685A-FIN-RESP.                              CL**6
00403              20  EL685A-PAYEE        PIC X(10).                      CL**6
00404                                                                      CL**6
00405          15  EL685A-ACCOUNT-LENGTH   PIC S9(4)                       CL**6
00406                                      COMP.                           CL**6
00407          15  EL685A-ACCOUNT-ATTRB    PIC X.                          CL**6
00408          15  EL685A-ACCOUNT          PIC X(10).                      CL**6
00409          15  EL685A-ACCOUNT-RDF                                      CL**6
00410              REDEFINES EL685A-ACCOUNT.                               CL**6
00411              20  EL685A-PAYEE-SEQ    PIC ZZZ9-.                      CL**6
00412              20  FILLER              PIC X(05).                      CL**6
00413          15  EL685A-AMT-LENGTH       PIC S9(4)                       CL**6
00414                                      COMP.                           CL**6
00415          15  EL685A-AMT-ATTRB        PIC X.                          CL**6
00416          15  EL685A-AMT              PIC Z,ZZZ,ZZ9.99-.              CL**6
00417                                                                   EL685
00418      EJECT                                                        EL685
00419                                  COPY ELCEMIB.                       CL**8
00420      EJECT                                                        EL685
00421                                  COPY ELCDATE.                       CL**8
00422      EJECT                                                        EL685
00423                                  COPY ELCLOGOF.                      CL**8
00424      EJECT                                                        EL685
00425                                  COPY ELCATTR.                       CL**8
00426      EJECT                                                        EL685
00427                                  COPY ELCAID.                        CL**8
00428                                                                   EL685
00429  01  FILLER                      REDEFINES                        EL685
00430      DFHAID.                                                      EL685
00431                                                                   EL685
00432      05  FILLER                      PIC X(8).                    EL685
00433                                                                   EL685
00434      05  PF-VALUES                   PIC X                        EL685
00435          OCCURS 24 TIMES.                                         EL685
00436      EJECT                                                        EL685
00437  LINKAGE SECTION.                                                 EL685
00438                                                                   EL685
00439  01  DFHCOMMAREA                     PIC X(1024).                 EL685
00440                                                                   EL685
S0441      EJECT                                                        EL685
00442                                      COPY ERCCHKQ.                   CL**8
00443      EJECT                                                        EL685
00444                                      COPY ELCCNTL.                   CL**8
00445      EJECT                                                           CL**4
00446                                      COPY ERCCMKQ.                   CL**8
00447      EJECT                                                        EL685
00448  PROCEDURE DIVISION.                                              EL685
00449                                                                   EL685
00450      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL685
00451                                                                   EL685
00452 *    NOTE ******************************************************* EL685
00453 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL685
00454 *         *  FROM ANOTHER MODULE.                               * EL685
00455 *         *******************************************************.EL685
00456                                                                   EL685
00457      IF EIBCALEN NOT GREATER THAN ZERO                            EL685
00458          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL685
00459          GO TO 8300-SEND-TEXT.                                    EL685
00460                                                                   EL685
00461      EXEC CICS HANDLE CONDITION                                   EL685
00462          PGMIDERR (9600-PGMIDERR)                                 EL685
00463          ERROR    (9990-ERROR)                                       CL*13
00464      END-EXEC.                                                       CL*13
00465                                                                   EL685
00466      EJECT                                                        EL685
00467  0010-MAIN-LOGIC.                                                 EL685
00468      IF PI-CALLED-FROM-AR-MENU                                       CL**4
00469          MOVE 'EL685B' TO WS-MAP-NAME.                               CL**4
00470                                                                      CL**4
00471      IF PI-CALLING-PROGRAM NOT = WS-PROGRAM-ID                    EL685
00472          IF PI-RETURN-TO-PROGRAM NOT = WS-PROGRAM-ID              EL685
00473              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL685
00474              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL685
00475              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL685
00476              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL685
00477              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL685
00478              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL685
00479              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL685
00480              MOVE WS-PROGRAM-ID        TO  PI-CALLING-PROGRAM     EL685
00481            ELSE                                                   EL685
00482              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL685
00483              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL685
00484              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL685
00485              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL685
00486              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL685
00487              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL685
00488              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL685
00489              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL685
00490      ELSE                                                            CL*14
00491          GO TO 0020-MAIN-LOGIC.                                   EL685
00492                                                                   EL685
00493                                                                   EL685
00494  0015-MAIN-LOGIC.                                                 EL685
00495                                                                   EL685
00496                                                                   EL685
00497 *    NOTE ******************************************************* EL685
00498 *         *                                                     * EL685
00499 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL685
00500 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL685
00501 *         *                                                     * EL685
00502 *         *******************************************************.EL685
00503                                                                   EL685
00504      EXEC CICS HANDLE CONDITION                                   EL685
00505          QIDERR (0015-NEXT-SENTENCE)                                 CL*13
00506      END-EXEC.                                                       CL*13
00507                                                                   EL685
00508      MOVE EIBTRMID               TO  WS-TS-TERM-ID.                  CL*14
00509                                                                   EL685
00510      EXEC CICS DELETEQ TS                                         EL685
00511          QUEUE (WS-TEMP-STORAGE-KEY)                                 CL*13
00512      END-EXEC.                                                       CL*13
00513                                                                   EL685
00514  0015-NEXT-SENTENCE.                                              EL685
00515                                                                   EL685
00516      MOVE PI-AR-MODE             TO  WS-SAVE-AR-MODE.                CL**4
00517      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA         EL685
00518                                                                   EL685
00519      MOVE LOW-VALUES             TO  PI-CHECK-QUE-KEY             EL685
00520                                      PI-PREV-CHECK-QUE-KEY        EL685
00521                                                                   EL685
00522      MOVE ZERO                   TO  PI-END-OF-FILE               EL685
00523                                      PI-TEMP-STORAGE-ITEM         EL685
00524                                      PI-CONTROL-TOT               EL685
00525                                      PI-CONTROL-SAVE-CONTROL      EL685
00526                                      PI-START-CONTROL-NO             CL**9
00527                                      PI-CONTROL-GRAND-TOT.        EL685
00528      MOVE WS-SAVE-CHECK-MODE     TO  PI-CHECK-MODE.               EL685
00529      MOVE WS-SAVE-AR-MODE        TO  PI-AR-MODE.                     CL**4
00530      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CODE              CL*14
00531      MOVE 'Y'                    TO  PI-FIRST-TIME-SW                CL*14
00532      MOVE 'Y'                    TO  PI-FIRST-TOTAL-SW               CL*14
00533      MOVE 'N'                    TO  PI-SEND-TOT-SWT                 CL*14
00534      MOVE 'N'                    TO  PI-EOF-SWT                      CL*14
00535      INITIALIZE HOLD-CHECK-RECORD.                                   CL*17
00536                                                                   EL685
00537      IF WS-START-CNTLNO GREATER ZERO                              EL685
00538          MOVE WS-START-CNTLNO    TO  PI-CK-CONTROL-NO.            EL685
00539                                                                   EL685
00540      MOVE EIBDATE                TO  DC-JULIAN-YYDDD              EL685
00541      MOVE '5'                    TO  DC-OPTION-CODE               EL685
00542      PERFORM 8500-DATE-CONVERSION                                 EL685
00543      MOVE DC-BIN-DATE-1          TO  PI-CURRENT-DATE-BIN          EL685
00544      MOVE DC-GREG-DATE-1-EDIT    TO  PI-CURRENT-DATE.             EL685
00545                                                                   EL685
00546      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.                        EL685
00547                                                                   EL685
00548      EJECT                                                        EL685
00549  0020-MAIN-LOGIC.                                                 EL685
00550                                                                   EL685
00551                                                                   EL685
00552 *    NOTE ******************************************************* EL685
00553 *         *                                                     * EL685
00554 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL685
00555 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL685
00556 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL685
00557 *         *                                                     * EL685
00558 *         *******************************************************.EL685
00559                                                                   EL685
00560      IF EIBAID = DFHCLEAR                                         EL685
00561          GO TO 9400-CLEAR.                                        EL685
00562                                                                   EL685
00563      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL685
00564          MOVE +8                 TO  EMI-ERROR                    EL685
00565          MOVE -1                 TO  APFKL                        EL685
00566          PERFORM 8200-SEND-DATAONLY.                              EL685
00567                                                                   EL685
00568      IF PI-CALLED-FROM-AR-MENU                                       CL**4
00569          EXEC CICS RECEIVE                                           CL**4
00570              INTO   (EL685BI)                                        CL**4
00571              MAPSET (WS-MAPSET-NAME)                                 CL**4
00572              MAP    (WS-MAP-NAME)                                    CL*13
00573          END-EXEC                                                    CL*13
00574       ELSE                                                           CL**4
00575          EXEC CICS RECEIVE                                           CL**4
00576              INTO   (EL685AI)                                        CL**4
00577              MAPSET (WS-MAPSET-NAME)                                 CL**4
00578              MAP    (WS-MAP-NAME)                                    CL*13
00579          END-EXEC.                                                   CL*13
00580                                                                   EL685
00581      IF PI-CALLED-FROM-AR-MENU       AND                             CL**4
00582            BPFKL IS GREATER THAN ZERO                                CL**4
00583            IF EIBAID NOT = DFHENTER                                  CL**4
00584                MOVE +4             TO  EMI-ERROR                     CL**4
00585                MOVE AL-UNBOF       TO  BPFKA                         CL**4
00586                MOVE -1             TO  BPFKL                         CL**4
00587                PERFORM 8200-SEND-DATAONLY                            CL**4
00588            ELSE                                                      CL**4
00589              IF BPFKO IS NUMERIC                                     CL**4
00590                AND BPFKO IS GREATER THAN ZERO                        CL**4
00591                AND BPFKO IS LESS THAN '25'                           CL**4
00592                  MOVE PF-VALUES (BPFKI)  TO  EIBAID                  CL**4
00593                ELSE                                                  CL**4
00594                  MOVE +29            TO  EMI-ERROR                   CL**4
00595                  MOVE AL-UNBOF       TO  BPFKA                       CL**4
00596                  MOVE -1             TO  BPFKL                       CL**4
00597                  PERFORM 8200-SEND-DATAONLY.                         CL**4
00598                                                                      CL**4
00599      IF NOT PI-CALLED-FROM-AR-MENU   AND                             CL**4
00600            APFKL IS GREATER THAN ZERO                                CL**4
00601            IF EIBAID NOT = DFHENTER                                  CL**4
00602                MOVE +4             TO  EMI-ERROR                     CL**4
00603                MOVE AL-UNBOF       TO  APFKA                         CL**4
00604                MOVE -1             TO  APFKL                         CL**4
00605                PERFORM 8200-SEND-DATAONLY                            CL**4
00606            ELSE                                                   EL685
00607              IF APFKO IS NUMERIC                                  EL685
00608                AND APFKO IS GREATER THAN ZERO                     EL685
00609                AND APFKO IS LESS THAN '25'                        EL685
00610                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL685
00611                ELSE                                               EL685
00612                  MOVE +29            TO  EMI-ERROR                EL685
00613                  MOVE AL-UNBOF       TO  APFKA                    EL685
00614                  MOVE -1             TO  APFKL                    EL685
00615                  PERFORM 8200-SEND-DATAONLY.                      EL685
00616                                                                   EL685
00617      IF EIBAID IS = DFHPF12                                       EL685
00618          MOVE 'EL010   '         TO  WS-PROGRAM-ID                EL685
00619          GO TO 9300-XCTL.                                         EL685
00620                                                                   EL685
00621      IF EIBAID IS = DFHPF23                                       EL685
00622          GO TO 9000-RETURN-CICS.                                  EL685
00623                                                                   EL685
00624      IF EIBAID IS = DFHPF24                                       EL685
00625          MOVE 'EL126   '         TO  WS-PROGRAM-ID                EL685
00626          GO TO 9300-XCTL.                                         EL685
00627                                                                   EL685
00628      IF EIBAID = (DFHENTER OR DFHPF1 OR DFHPF2                    EL685
00629                            OR DFHPF3 OR DFHPF4)                   EL685
00630          NEXT SENTENCE                                            EL685
00631        ELSE                                                       EL685
00632          MOVE +8                 TO  EMI-ERROR                    EL685
00633          MOVE -1                 TO  APFKL                        EL685
00634          PERFORM 8200-SEND-DATAONLY.                              EL685
00635                                                                   EL685
00636      EJECT                                                        EL685
00637  0100-MAIN-LOGIC.                                                 EL685
00638                                                                      CL**9
00639      IF PI-CALLED-FROM-AR-MENU         AND                           CL**9
00640         EIBAID = DFHPF3                                              CL**9
00641          IF BCNTLNOL IS GREATER THAN +0                              CL**9
00642              MOVE BCNTLNOI           TO  PI-START-CONTROL-NO         CL**9
00643              GO TO 0130-MAIN-LOGIC                                EL685
00644          ELSE                                                     EL685
00645              GO TO 0130-MAIN-LOGIC.                                  CL**9
00646                                                                      CL**9
00647      IF NOT PI-CALLED-FROM-AR-MENU         AND                       CL**9
00648         EIBAID = DFHPF3                                              CL**9
00649          IF CNTLNOL IS GREATER THAN +0                               CL**9
00650              MOVE CNTLNOI            TO  PI-START-CONTROL-NO         CL**9
00651              GO TO 0130-MAIN-LOGIC                                   CL**9
00652          ELSE                                                        CL**9
00653              GO TO 0130-MAIN-LOGIC.                                  CL**9
00654                                                                   EL685
00655      IF NOT PI-CALLED-FROM-AR-MENU     AND                           CL**4
00656         EIBAID = DFHPF4                                              CL**4
00657          IF CHECKS-TO-BE-PRINTED                                  EL685
00658              MOVE 'Y' TO WS-SAVE-CHECK-MODE                       EL685
00659              IF CNTLNOL GREATER ZERO                              EL685
00660                  IF CNTLNOI NUMERIC                               EL685
00661                      MOVE CNTLNOI TO WS-START-CNTLNO              EL685
00662                      GO TO 0015-MAIN-LOGIC                        EL685
00663                  ELSE                                             EL685
00664                      GO TO 0015-MAIN-LOGIC                        EL685
00665              ELSE                                                 EL685
00666                  GO TO 0015-MAIN-LOGIC                            EL685
00667          ELSE                                                     EL685
00668              IF CHECKS-PRINTED                                    EL685
00669                  MOVE SPACE TO WS-SAVE-CHECK-MODE                 EL685
00670                  IF CNTLNOL IS GREATER THAN +0                       CL**9
00671                      IF CNTLNOI IS NUMERIC                           CL**9
00672                          MOVE CNTLNOI  TO  WS-START-CNTLNO           CL**9
00673                          GO TO 0015-MAIN-LOGIC                       CL**9
00674                      ELSE                                            CL**9
00675                          GO TO 0015-MAIN-LOGIC                       CL**9
00676                  ELSE                                                CL**9
00677                      GO TO 0015-MAIN-LOGIC.                          CL**9
00678                                                                   EL685
00679      IF PI-CALLED-FROM-AR-MENU         AND                           CL**4
00680         EIBAID = DFHPF4                                              CL**4
00681          IF CHECKS-TO-BE-PRINTED                                     CL**4
00682              MOVE 'Y' TO WS-SAVE-CHECK-MODE                          CL**4
00683              IF BCNTLNOL GREATER ZERO                                CL**4
00684                  IF BCNTLNOI NUMERIC                                 CL**4
00685                      MOVE BCNTLNOI TO WS-START-CNTLNO                CL**4
00686                      GO TO 0015-MAIN-LOGIC                           CL**4
00687                  ELSE                                                CL**4
00688                      GO TO 0015-MAIN-LOGIC                           CL**4
00689              ELSE                                                    CL**4
00690                  GO TO 0015-MAIN-LOGIC                               CL**4
00691          ELSE                                                        CL**4
00692              IF CHECKS-PRINTED                                       CL**4
00693                  MOVE SPACE TO WS-SAVE-CHECK-MODE                    CL**4
00694                  IF BCNTLNOL IS GREATER THAN +0                      CL**9
00695                      IF BCNTLNOI IS NUMERIC                          CL**9
00696                          MOVE BCNTLNOI   TO  WS-START-CNTLNO         CL**9
00697                          GO TO 0015-MAIN-LOGIC                       CL**9
00698                      ELSE                                            CL**9
00699                          GO TO 0015-MAIN-LOGIC                       CL**9
00700                  ELSE                                                CL**9
00701                      GO TO 0015-MAIN-LOGIC.                          CL**9
00702                                                                      CL**4
00703      IF EIBAID = DFHPF1 OR DFHPF2                                 EL685
00704          GO TO 0110-MAIN-LOGIC.                                   EL685
00705                                                                   EL685
00706      IF NOT PI-CALLED-FROM-AR-MENU    AND                            CL**4
00707         CNTLNOL GREATER ZERO                                         CL**4
00708          IF CNTLNOI NUMERIC                                       EL685
00709              MOVE CNTLNOI TO WS-START-CNTLNO                      EL685
00710              MOVE PI-CHECK-MODE TO WS-SAVE-CHECK-MODE                CL**4
00711              GO TO 0015-MAIN-LOGIC.                                  CL**4
00712                                                                      CL**4
00713      IF PI-CALLED-FROM-AR-MENU    AND                                CL**4
00714         BCNTLNOL GREATER ZERO                                        CL**4
00715          IF BCNTLNOI NUMERIC                                         CL**4
00716              MOVE BCNTLNOI TO WS-START-CNTLNO                        CL**4
00717              MOVE PI-CHECK-MODE TO WS-SAVE-CHECK-MODE             EL685
00718              GO TO 0015-MAIN-LOGIC.                               EL685
00719                                                                   EL685
00720      IF PI-END-OF-FILE NOT = ZERO                                 EL685
00721          PERFORM 9400-CLEAR.                                      EL685
00722                                                                   EL685
00723      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.                        EL685
00724                                                                   EL685
00725  0110-MAIN-LOGIC.                                                 EL685
00726      IF PI-CALLED-FROM-AR-MENU                                       CL**4
00727          MOVE BPAGEI                 TO  WS-TEMP-STORAGE-ITEM        CL**4
00728      ELSE                                                            CL**4
00729          MOVE APAGEI                 TO  WS-TEMP-STORAGE-ITEM.       CL**4
00730                                                                   EL685
00731      IF EIBAID = DFHPF1 AND                                       EL685
00732         PI-SEND-TOT     AND                                       EL685
00733         PI-EOF          AND                                       EL685
00734         WS-TEMP-STORAGE-ITEM = PI-TEMP-STORAGE-ITEM               EL685
00735         IF NOT PI-CALLED-FROM-AR-MENU                                CL**4
00736             MOVE LOW-VALUE TO EL685AI                                CL**4
00737             MOVE WS-TEMP-STORAGE-ITEM TO APAGEO                      CL**4
00738             MOVE +375                   TO  EMI-ERROR                CL**4
00739             SET EL685A-INDEX TO 1                                    CL**4
00740             MOVE 'CONTL TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)     CL**4
00741             MOVE PI-CONTROL-TOT TO EL685A-AMT (EL685A-INDEX)         CL**4
00742             SET EL685A-INDEX UP BY +1                                CL**4
00743             MOVE PI-CONTROL-GRAND-TOT TO EL685A-AMT (EL685A-INDEX)   CL**4
00744             MOVE 'GRAND TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)     CL**4
00745             MOVE -1 TO APFKL                                         CL**4
00746             MOVE 'T' TO PI-PREV-DISPLAY-SWT                          CL**5
00747             GO TO 8100-SEND-INITIAL-MAP                              CL**4
00748         ELSE                                                         CL**4
00749             MOVE LOW-VALUE TO EL685BI                                CL**4
00750             MOVE WS-TEMP-STORAGE-ITEM TO BPAGEO                      CL**4
00751             MOVE +375                   TO  EMI-ERROR                CL**4
00752             SET EL685B-INDEX TO 1                                    CL**4
00753             MOVE ' CONTL'      TO EL685B-DESC-ONE   (EL685B-INDEX)   CL**4
00754             MOVE 'TOTAL'       TO EL685B-DESC-TWO   (EL685B-INDEX)   CL**4
00755             MOVE PI-CONTROL-TOT TO EL685B-AMT (EL685B-INDEX)         CL**4
00756             SET EL685B-INDEX UP BY +1                                CL**4
00757             MOVE PI-CONTROL-GRAND-TOT TO EL685B-AMT (EL685B-INDEX)   CL**4
00758             MOVE ' GRAND'      TO EL685B-DESC-ONE   (EL685B-INDEX)   CL**4
00759             MOVE 'TOTAL'       TO EL685B-DESC-TWO   (EL685B-INDEX)   CL**4
00760             MOVE -1 TO BPFKL                                         CL**4
00761             MOVE 'T' TO PI-PREV-DISPLAY-SWT                          CL**5
00762             GO TO 8100-SEND-INITIAL-MAP.                             CL**4
00763                                                                   EL685
00764      IF EIBAID = DFHPF1                                           EL685
00765         IF WS-TEMP-STORAGE-ITEM LESS THAN PI-TEMP-STORAGE-ITEM    EL685
00766          ADD +1  TO  WS-TEMP-STORAGE-ITEM                         EL685
00767          GO TO 0120-MAIN-LOGIC                                    EL685
00768         ELSE                                                      EL685
00769          IF PI-NOT-EOF                                            EL685
00770             GO TO 4000-BROWSE-CHECK-QUEUE-FILE                    EL685
00771          ELSE                                                     EL685
00772             NEXT SENTENCE                                         EL685
00773      ELSE                                                         EL685
00774         NEXT SENTENCE.                                            EL685
00775                                                                   EL685
00776      IF EIBAID = DFHPF2                                           EL685
00777        IF WS-TEMP-STORAGE-ITEM GREATER THAN +1                       CL**5
00778            IF PI-TOTAL-SCREEN                                        CL**5
00779                MOVE SPACES TO PI-PREV-DISPLAY-SWT                    CL**5
00780                GO TO 0120-MAIN-LOGIC                                 CL**5
00781            ELSE                                                      CL**5
00782                SUBTRACT +1 FROM WS-TEMP-STORAGE-ITEM                 CL**5
00783                GO TO 0120-MAIN-LOGIC                                 CL**5
00784        ELSE                                                          CL**5
00785            IF PI-TOTAL-SCREEN                                        CL**5
00786                MOVE SPACES TO PI-PREV-DISPLAY-SWT                    CL**5
00787                GO TO 0120-MAIN-LOGIC.                                CL**5
00788                                                                   EL685
00789      MOVE +312                   TO  EMI-ERROR                    EL685
00790      IF PI-CALLED-FROM-AR-MENU                                       CL**4
00791          MOVE -1                     TO  BPFKL                       CL**4
00792      ELSE                                                            CL**4
00793          MOVE -1                     TO  APFKL.                      CL**4
00794      PERFORM 8200-SEND-DATAONLY.                                  EL685
00795      EJECT                                                        EL685
00796  0120-MAIN-LOGIC.                                                 EL685
00797      MOVE EIBTRMID               TO  WS-TS-TERM-ID.               EL685
00798                                                                   EL685
00799      IF PI-CALLED-FROM-AR-MENU                                       CL**4
00800          EXEC CICS READQ TS                                          CL**4
00801              QUEUE  (WS-TEMP-STORAGE-KEY)                            CL**4
00802              ITEM   (WS-TEMP-STORAGE-ITEM)                           CL**4
00803              INTO   (EL685BI)                                        CL**4
00804              LENGTH (WS-TS-LENGTH)                                   CL*13
00805          END-EXEC                                                    CL*13
00806      ELSE                                                            CL**4
00807          EXEC CICS READQ TS                                          CL**4
00808              QUEUE  (WS-TEMP-STORAGE-KEY)                            CL**4
00809              ITEM   (WS-TEMP-STORAGE-ITEM)                           CL**4
00810              INTO   (EL685AI)                                        CL**4
00811              LENGTH (WS-TS-LENGTH)                                   CL*13
00812          END-EXEC.                                                   CL*13
00813                                                                   EL685
00814      IF PI-CALLED-FROM-AR-MENU                                       CL**4
00815          MOVE WS-TEMP-STORAGE-ITEM  TO  BPAGEO                       CL**4
00816      ELSE                                                            CL**4
00817          MOVE WS-TEMP-STORAGE-ITEM  TO  APAGEO.                      CL**4
00818                                                                   EL685
00819      PERFORM 8100-SEND-INITIAL-MAP.                               EL685
00820      EJECT                                                        EL685
00821  0130-MAIN-LOGIC.                                                 EL685
00822      EXEC CICS HANDLE CONDITION                                   EL685
00823           TERMIDERR    (0130-TERMID-ERROR)                        EL685
00824           TRANSIDERR   (0130-TRANS-ERROR)                         EL685
00825           END-EXEC.                                               EL685
00826                                                                   EL685
00827      MOVE SPACES                     TO PI-ALT-DMD-PRT-ID.           CL*16
00828      IF PI-CALLED-FROM-AR-MENU                                       CL**4
00829          IF BPRINTRL NOT = ZEROS                                     CL**4
00830             MOVE BPRINTRI            TO WS-PRINTER-ID                CL**4
00831                                         PI-ALT-DMD-PRT-ID            CL*16
00832             GO TO 0130-START                                         CL**4
00833          ELSE                                                        CL**4
00834             NEXT SENTENCE                                            CL**4
00835      ELSE                                                            CL**4
00836          IF PRINTERL NOT = ZEROS                                     CL**4
00837             MOVE PRINTERI            TO WS-PRINTER-ID                CL**4
00838                                         PI-ALT-DMD-PRT-ID            CL*16
00839             GO TO 0130-START.                                        CL**4
00840                                                                      CL**3
00841      IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES                  CL**3
00842          MOVE PI-PROCESSOR-PRINTER   TO  WS-PRINTER-ID               CL**3
00843          GO TO 0130-START.                                           CL**3
00844                                                                   EL685
00845      MOVE PI-COMPANY-ID          TO CNTL-CO                       EL685
00846      MOVE '1'                    TO CNTL-RECORD-TYPE              EL685
00847      MOVE SPACES                 TO CNTL-GENL                     EL685
00848      MOVE ZEROS                  TO CNTL-SEQ                      EL685
00849      EXEC CICS READ                                               EL685
00850           DATASET   (WS-CONTROL-DSID)                             EL685
00851           SET       (ADDRESS OF CONTROL-FILE)                        CL*13
00852           RIDFLD    (CNTL-KEY)                                    EL685
00853      END-EXEC.                                                       CL*13
00854                                                                   EL685
00855      MOVE CF-FORMS-PRINTER-ID    TO WS-PRINTER-ID.                EL685
00856                                                                   EL685
00857  0130-START.                                                      EL685
00858                                                                      CL*16
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*16
00860 *        MOVE EIBTRMID       TO WS-PRINTER-ID                        CL*16
00861          EXEC CICS START                                             CL*16
00862               INTERVAL(0)                                            CL*16
00863               TRANSID    (WS-PRINT-TRAN-ID)                          CL*16
00864               FROM       (PROGRAM-INTERFACE-BLOCK)                   CL*16
00865               LENGTH     (PI-COMM-LENGTH)                            CL*16
00866 *             TERMID     (WS-PRINTER-ID)                             CL*16
00867          END-EXEC                                                    CL*16
00868      ELSE                                                            CL*16
00869          EXEC CICS START                                             CL*16
00870               INTERVAL(0)                                            CL*16
00871               TRANSID    (WS-PRINT-TRAN-ID)                          CL*16
00872               FROM       (PROGRAM-INTERFACE-BLOCK)                   CL*16
00873               LENGTH     (PI-COMM-LENGTH)                            CL*16
00874               TERMID     (WS-PRINTER-ID)                             CL*16
00875          END-EXEC.                                                   CL*16
00876                                                                   EL685
00877      MOVE 0567                   TO EMI-ERROR                     EL685
00878      MOVE -1                     TO APFKL                         EL685
00879      GO TO 8200-SEND-DATAONLY.                                    EL685
00880                                                                   EL685
00881                                                                   EL685
00882  0130-TERMID-ERROR.                                               EL685
00883      MOVE 0412                   TO EMI-ERROR                     EL685
00884      MOVE -1                     TO APFKL                         EL685
00885      GO TO 8200-SEND-DATAONLY.                                    EL685
00886  0130-TRANS-ERROR.                                                EL685
00887      MOVE 0413                   TO EMI-ERROR                     EL685
00888      MOVE -1                     TO APFKL                         EL685
00889      GO TO 8200-SEND-DATAONLY.                                    EL685
00890                                                                   EL685
00891      EJECT                                                        EL685
00892  4000-BROWSE-CHECK-QUEUE-FILE SECTION.                            EL685
00893                                                                      CL**4
00894      IF PI-CALLED-FROM-AR-MENU                                       CL**4
00895          PERFORM 5000-BROWSE-COMM-CHECK-QUEUE                        CL**4
00896          GO TO 4990-EXIT.                                            CL**4
00897                                                                   EL685
00898      EXEC CICS HANDLE CONDITION                                   EL685
00899          NOTFND   (8400-NOTFND)                                   EL685
00900      END-EXEC.                                                       CL*13
00901                                                                   EL685
00902      MOVE LOW-VALUES             TO  EL685AI                      EL685
00903                                                                   EL685
00904      EXEC CICS STARTBR                                            EL685
00905          DATASET (WS-CHECK-QUEUE-DSID)                            EL685
00906          RIDFLD  (PI-CHECK-QUE-KEY)                               EL685
00907          GTEQ                                                        CL*13
00908      END-EXEC.                                                       CL*13
00909                                                                   EL685
00910      SET EL685A-INDEX TO +1.                                      EL685
00911                                                                   EL685
00912  4100-READNEXT.                                                   EL685
00913      MOVE PI-CHECK-QUE-KEY           TO  PI-PREV-CHECK-QUE-KEY    EL685
00914                                                                   EL685
00915      EXEC CICS HANDLE CONDITION                                      CL*12
00916          ENDFILE  (4800-END-OF-FILE)                                 CL*12
00917      END-EXEC.                                                       CL*13
00918                                                                      CL*12
00919      EXEC CICS READNEXT                                           EL685
00920          DATASET (WS-CHECK-QUEUE-DSID)                            EL685
00921          RIDFLD  (PI-CHECK-QUE-KEY)                               EL685
00922          SET     (ADDRESS OF CHECK-QUE) END-EXEC                     CL*13
00923                                                                   EL685
00924      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL685
00925          GO TO 4800-END-OF-FILE.                                  EL685
00926                                                                   EL685
00927      IF CQ-ENTRY-TYPE NOT = 'Q'                                   EL685
00928          GO TO 4100-READNEXT.                                     EL685
00929                                                                   EL685
00930      IF CQ-VOID-INDICATOR = 'V'                                      CL*11
00931          GO TO 4100-READNEXT.                                        CL*11
00932                                                                      CL*11
00933      IF CQ-CHECK-AMOUNT = ZEROS                                      CL*11
00934          GO TO 4100-READNEXT.                                        CL*11
00935                                                                      CL*11
00936      IF CQ-TIMES-PRINTED NOT = ZERO                               EL685
00937          IF CHECKS-PRINTED                                        EL685
00938              NEXT SENTENCE                                        EL685
00939          ELSE                                                     EL685
00940              GO TO 4100-READNEXT                                  EL685
00941      ELSE                                                         EL685
00942          IF CHECKS-TO-BE-PRINTED                                  EL685
00943              NEXT SENTENCE                                        EL685
00944          ELSE                                                     EL685
00945              GO TO 4100-READNEXT.                                 EL685
00946                                                                   EL685
00947      IF PI-FIRST-TIME                                                CL**9
00948          IF CNTLNOL IS EQUAL TO +0                                   CL**9
00949              MOVE PI-CK-CONTROL-NO   TO  PI-START-CONTROL-NO.        CL**9
CIDMOD                                                                       000
CIDMOD     EXEC CICS                                                         000
CIDMOD          ASKTIME                                                      000
CIDMOD     END-EXEC.                                                         000
00950                                                                      CL**9
00951      IF EL685A-INDEX LESS THAN +18                                EL685
00952         IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER        EL685
00953            IF PI-FIRST-TIME                                       EL685
00954               MOVE CQ-CONTROL-NUMBER TO PI-CONTROL-SAVE-CONTROL   EL685
00955            ELSE                                                   EL685
00956               MOVE 'CONTL TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)EL685
00957               MOVE PI-CONTROL-TOT TO EL685A-AMT (EL685A-INDEX)    EL685
00958               SET EL685A-INDEX UP BY +1                           EL685
00959               MOVE ZEROS TO PI-CONTROL-TOT                        EL685
00960               MOVE CQ-CONTROL-NUMBER TO PI-CONTROL-SAVE-CONTROL.  EL685
00961                                                                   EL685
00962      MOVE 'N'                    TO PI-FIRST-TIME-SW.                CL*13
00963                                                                   EL685
00964      IF EL685A-INDEX GREATER THAN +17                             EL685
00965         MOVE +1 TO WS-READNEXT-SW.                                EL685
00966                                                                   EL685
00967      IF WS-READNEXT-SW GREATER THAN ZERO                          EL685
00968          GO TO 4900-ENDBROWSE.                                    EL685
00969                                                                   EL685
00970      ADD CQ-CHECK-AMOUNT        TO  PI-CONTROL-TOT                EL685
00971                                     PI-CONTROL-GRAND-TOT.         EL685
00972      MOVE CQ-CONTROL-NUMBER     TO  EL685A-CONTROL  (EL685A-INDEX)EL685
00973      MOVE CQ-CHECK-NUMBER       TO  EL685A-CHECK-NO (EL685A-INDEX)EL685
00974      MOVE CQ-CHECK-AMOUNT       TO  EL685A-AMT      (EL685A-INDEX)EL685
00975                                                                   EL685
00976      IF CQ-BILLING-CREDIT                                         EL685
00977         MOVE 'BILL CREDIT'     TO  EL685A-PMT-TYPE (EL685A-INDEX) EL685
00978      ELSE                                                         EL685
00979          IF CQ-REFUND-PMT                                         EL685
00980             MOVE 'REFUND PMT'  TO  EL685A-PMT-TYPE (EL685A-INDEX) EL685
00981          ELSE                                                     EL685
00982             MOVE 'CHECK MAINT' TO  EL685A-PMT-TYPE (EL685A-INDEX).EL685
00983                                                                   EL685
00984      IF CQ-CHECK-MAINT-PMT OR CQ-REFUND-PMT                          CL**9
00985        IF PI-COMPANY-ID = 'LAP'  OR  'RMC'                           CL*11
00986          MOVE CQ-CHEK-GROUPING  TO  EL685A-GROUPING (EL685A-INDEX)   CL*11
00987          MOVE CQ-CHEK-CARRIER   TO  EL685A-CARRIER  (EL685A-INDEX)   CL*11
00988          MOVE CQ-CHEK-ACCOUNT   TO  EL685A-FIN-RESP (EL685A-INDEX)   CL*11
00989          MOVE CQ-CHEK-CERT-NO   TO  EL685A-ACCOUNT  (EL685A-INDEX)   CL*11
00990        ELSE                                                          CL*11
00991          MOVE CQ-CHEK-GROUPING  TO  EL685A-GROUPING (EL685A-INDEX)EL685
00992          MOVE CQ-CHEK-CARRIER   TO  EL685A-CARRIER  (EL685A-INDEX)EL685
00993          MOVE CQ-CHEK-FIN-RESP  TO  EL685A-FIN-RESP (EL685A-INDEX)   CL**9
00994          MOVE CQ-CHEK-ACCOUNT   TO  EL685A-ACCOUNT  (EL685A-INDEX)EL685
00995      ELSE                                                         EL685
00996          MOVE CQ-PYAJ-GROUPING  TO  EL685A-GROUPING (EL685A-INDEX)EL685
00997          MOVE CQ-PYAJ-CARRIER   TO  EL685A-CARRIER  (EL685A-INDEX)EL685
00998          MOVE CQ-PYAJ-FIN-RESP  TO  EL685A-FIN-RESP (EL685A-INDEX)EL685
00999          MOVE CQ-PYAJ-ACCOUNT   TO  EL685A-ACCOUNT (EL685A-INDEX).EL685
01000                                                                   EL685
01001      IF EL685A-INDEX LESS THAN +18                                EL685
01002          SET EL685A-INDEX UP BY +1                                EL685
01003          GO TO 4100-READNEXT.                                     EL685
01004                                                                   EL685
01005      MOVE +1                     TO  WS-READNEXT-SW.              EL685
01006      GO TO 4100-READNEXT.                                         EL685
01007                                                                   EL685
01008  4800-END-OF-FILE.                                                EL685
CIDMOD     EXEC CICS                                                         000
CIDMOD          ASKTIME                                                      000
CIDMOD     END-EXEC.                                                         000
01009                                                                   EL685
01010      MOVE +1                     TO  PI-END-OF-FILE.              EL685
01011                                                                   EL685
01012      MOVE 'Y'                    TO PI-EOF-SWT.                   EL685
01013      IF EL685A-INDEX GREATER +16                                  EL685
01014         MOVE 'Y' TO PI-SEND-TOT-SWT                               EL685
01015         GO TO 4900-ENDBROWSE.                                     EL685
01016                                                                   EL685
01017      MOVE +375                   TO  EMI-ERROR.                   EL685
01018      IF EL685A-INDEX LESS THAN +18                                EL685
01019               MOVE 'CONTL TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)EL685
01020               MOVE PI-CONTROL-TOT TO EL685A-AMT (EL685A-INDEX)    EL685
01021               SET EL685A-INDEX UP BY +1                           EL685
01022      ELSE                                                         EL685
01023         NEXT SENTENCE.                                            EL685
01024      IF EL685A-INDEX LESS THAN +18                                EL685
01025         SET EL685A-INDEX UP BY +1                                 EL685
01026         MOVE PI-CONTROL-GRAND-TOT TO EL685A-AMT (EL685A-INDEX)    EL685
01027         MOVE 'GRAND TOTAL' TO EL685A-PMT-TYPE (EL685A-INDEX)      EL685
01028      ELSE                                                         EL685
01029         NEXT SENTENCE.                                            EL685
01030                                                                   EL685
01031  4900-ENDBROWSE.                                                  EL685
01032      MOVE -1                     TO  APFKL                        EL685
01033                                                                   EL685
01034      MOVE EIBTRMID               TO  WS-TS-TERM-ID                EL685
01035                                                                   EL685
01036      EXEC CICS WRITEQ TS                                          EL685
01037          QUEUE  (WS-TEMP-STORAGE-KEY)                             EL685
01038          ITEM   (PI-TEMP-STORAGE-ITEM)                            EL685
01039          FROM   (EL685AI)                                         EL685
01040          LENGTH (WS-TS-LENGTH)                                       CL*13
01041      END-EXEC.                                                       CL*13
01042                                                                   EL685
01043      MOVE PI-TEMP-STORAGE-ITEM  TO  APAGEO                        EL685
01044                                                                   EL685
01045      PERFORM 8100-SEND-INITIAL-MAP.                               EL685
01046                                                                   EL685
01047  4990-EXIT.                                                       EL685
01048      EXIT.                                                        EL685
01049                                                                   EL685
01050      EJECT                                                        EL685
01051  5000-BROWSE-COMM-CHECK-QUEUE SECTION.                               CL**4
01052                                                                      CL**4
01053      EXEC CICS HANDLE CONDITION                                      CL**4
01054          NOTFND   (8400-NOTFND)                                      CL**4
01055      END-EXEC.                                                       CL*13
01056                                                                      CL**4
01057      MOVE LOW-VALUES             TO  EL685BI                         CL**4
01058                                                                      CL**4
01059      EXEC CICS STARTBR                                               CL**4
01060          DATASET (WS-COMCK-QUEUE-DSID)                               CL**4
01061          RIDFLD  (PI-CHECK-QUE-KEY)                                  CL**4
01062          GTEQ                                                        CL*13
01063      END-EXEC.                                                       CL*13
01064                                                                      CL**4
01065      SET EL685B-INDEX TO +1.                                         CL**4
01066                                                                      CL**4
01067  5100-READNEXT.                                                      CL**4
01068                                                                      CL*12
01069      EXEC CICS HANDLE CONDITION                                      CL*12
01070          ENDFILE  (5800-END-OF-FILE)                                 CL*12
01071      END-EXEC.                                                       CL*13
01072                                                                      CL**4
01073      EXEC CICS READNEXT                                              CL**4
01074          DATASET (WS-COMCK-QUEUE-DSID)                               CL**4
01075          RIDFLD  (PI-CHECK-QUE-KEY)                                  CL**4
01076          SET     (ADDRESS OF COMMISSION-CHECK-QUE)                   CL*13
01077      END-EXEC.                                                       CL*13
01078                                                                      CL**4
01079      IF MQ-COMPANY-CD NOT = PI-COMPANY-CD                            CL**4
01080          GO TO 5800-END-OF-FILE.                                     CL**4
01081                                                                      CL**7
01082      IF MQ-VOID-DT NOT = LOW-VALUES                                  CL*13
01083          GO TO 5100-READNEXT.                                        CL**7
01084                                                                      CL**4
01085      IF MQ-ENTRY-TYPE = 'Q' OR 'P'                                   CL*17
01086          NEXT SENTENCE                                               CL*17
01087      ELSE                                                            CL*17
01088          GO TO 5100-READNEXT.                                        CL**4
01089                                                                      CL*15
01090      IF CHECKS-TO-BE-PRINTED                                         CL*13
01091          IF MQ-CHECK-AMOUNT = ZEROS                                  CL*13
01092              GO TO 5100-READNEXT.                                    CL*13
01093                                                                      CL**4
01094      IF MQ-TIMES-PRINTED NOT = ZERO                                  CL**4
01095          IF CHECKS-PRINTED                                           CL**4
01096              NEXT SENTENCE                                           CL**4
01097          ELSE                                                        CL**4
01098              GO TO 5100-READNEXT                                     CL**4
01099      ELSE                                                            CL**4
01100          IF CHECKS-TO-BE-PRINTED                                     CL**4
01101              NEXT SENTENCE                                           CL**4
01102          ELSE                                                        CL**4
01103              GO TO 5100-READNEXT.                                    CL**4
01104                                                                      CL*18
01105      IF PI-PAGE-FORWARD                                              CL*18
01106          MOVE COMMISSION-CHECK-QUE                                   CL*18
01107                                  TO  HOLD-CHECK-RECORD               CL*18
01108          MOVE 'N'                TO  PI-PAGING-SW                    CL*18
01109          IF CHECKS-PRINTED                                           CL*18
01110              GO TO 5100-READNEXT.                                    CL*18
01111                                                                      CL*18
CIDMOD     IF PI-FIRST-TIME                                                  000
CIDMOD         IF BCNTLNOL IS EQUAL TO +0                                    000
CIDMOD             MOVE PI-CK-CONTROL-NO   TO  PI-START-CONTROL-NO           000
CIDMOD         END-IF                                                        000
CIDMOD     END-IF.                                                           000
CIDMOD
01112      IF MQ-COMPANY-CD-A1  = HOLD-COMPANY-CD-A1  AND                  CL*18
01113         MQ-CONTROL-NUMBER = HOLD-CONTROL-NUMBER AND                  CL*18
01114         MQ-CARRIER-A1     = HOLD-CARRIER-A1     AND                  CL*18
01115         MQ-GROUPING-A1    = HOLD-GROUPING-A1    AND                  CL*18
01116         MQ-PAYEE-A1       = HOLD-PAYEE-A1       AND                  CL*18
01117         MQ-PAYEE-SEQ-A1   = HOLD-PAYEE-SEQ-A1                        CL*18
01118          NEXT SENTENCE                                               CL*18
01119      ELSE                                                            CL*18
01120          MOVE PI-CHECK-QUE-KEY   TO  PI-PREV-CHECK-QUE-KEY.          CL*18
01121                                                                      CL**9
01122      IF CHECKS-PRINTED                                               CL*13
01123          GO TO 5200-PRINTED.                                         CL*13
01124                                                                      CL*13
01125  5100-TO-BE-PRINTED.                                                 CL*13
01126                                                                      CL*13
01127      IF PI-FIRST-TOTAL                                               CL*13
01128          IF BCNTLNOL IS EQUAL TO +0                                  CL**9
01129              MOVE PI-CK-CONTROL-NO                                   CL*13
01130                                  TO  PI-START-CONTROL-NO.            CL*13
01131                                                                      CL**4
CIDMOD     EXEC CICS                                                         000
CIDMOD          ASKTIME                                                      000
CIDMOD     END-EXEC.                                                         000
CIDMOD
01132      IF EL685B-INDEX LESS THAN +18                                   CL**4
01133         IF PI-CONTROL-SAVE-CONTROL NOT = MQ-CONTROL-NUMBER           CL**4
01134            IF PI-FIRST-TOTAL                                         CL*13
01135                MOVE MQ-CONTROL-NUMBER                                CL*13
01136                                  TO PI-CONTROL-SAVE-CONTROL          CL*13
01137                MOVE PI-CK-CONTROL-NO                                 CL*13
01138                                  TO  PI-START-CONTROL-NO             CL*13
01139            ELSE                                                      CL**4
01140               MOVE ' CONTL'      TO EL685B-DESC-ONE (EL685B-INDEX)   CL*13
01141               MOVE 'TOTAL'       TO EL685B-DESC-TWO (EL685B-INDEX)   CL*13
01142               MOVE PI-CONTROL-TOT                                    CL*13
01143                                  TO EL685B-AMT (EL685B-INDEX)        CL*13
01144               SET EL685B-INDEX UP BY +1                              CL**4
01145               MOVE ZEROS         TO PI-CONTROL-TOT                   CL*13
01146               MOVE MQ-CONTROL-NUMBER                                 CL*13
01147                                  TO PI-CONTROL-SAVE-CONTROL.         CL*13
01148                                                                      CL**4
01149      MOVE 'N'                    TO PI-FIRST-TOTAL-SW.               CL*13
01150                                                                      CL**4
01151      IF EL685B-INDEX GREATER THAN +17                                CL**4
01152         MOVE +1 TO WS-READNEXT-SW.                                   CL**4
01153                                                                      CL**4
01154      IF WS-READNEXT-SW GREATER THAN ZERO                             CL**4
01155          MOVE PI-PREV-CHECK-QUE-KEY                                  CL*14
01156                                  TO  PI-CHECK-QUE-KEY                CL*14
01157          MOVE 'Y'                TO  PI-PAGING-SW                    CL*14
01158          GO TO 5900-WRITE-REPORT.                                    CL*13
01159                                                                      CL**4
01160      ADD MQ-CHECK-AMOUNT      TO  PI-CONTROL-TOT                     CL**4
01161                                   PI-CONTROL-GRAND-TOT.              CL**4
01162      MOVE MQ-CONTROL-NUMBER   TO  EL685B-CONTROL   (EL685B-INDEX)    CL**4
01163                                                                      CL*17
01164      IF ACH-PAYMENT                                                  CL*17
01165          MOVE ' ACH  '                                               CL*17
01166                               TO  EL685B-CHECK-NO  (EL685B-INDEX)    CL*17
01167      ELSE                                                            CL*17
01168          MOVE MQ-CHECK-NUMBER                                        CL*17
01169                               TO  EL685B-CHECK-NO  (EL685B-INDEX).   CL*17
01170                                                                      CL*17
01171      MOVE MQ-CHECK-AMOUNT     TO  EL685B-AMT       (EL685B-INDEX)    CL**4
01172      MOVE MQ-CHEK-CARRIER     TO  EL685B-CARRIER   (EL685B-INDEX)    CL**4
01173      MOVE MQ-CHEK-GROUPING    TO  EL685B-GROUPING  (EL685B-INDEX)    CL**4
01174      MOVE MQ-CHEK-PAYEE       TO  EL685B-PAYEE     (EL685B-INDEX)    CL**4
01175      MOVE MQ-PAYEE-SEQ-A1     TO  EL685B-PAYEE-SEQ (EL685B-INDEX)    CL**4
01176      MOVE MQ-PAYEE-NAME       TO  EL685B-PAYEE-NA  (EL685B-INDEX)    CL**4
01177                                                                      CL**4
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
CIDMOD         MOVE MQ-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1                 000
CIDMOD         MOVE ' '                    TO  DC-OPTION-CODE                000
CIDMOD         PERFORM 8500-DATE-CONVERSION                                  000
CIDMOD         MOVE DC-GREG-DATE-1-EDIT TO
CIDMOD                                  EL685B-CHK-DT (EL685B-INDEX)         000
CIDMOD     ELSE
CIDMOD         MOVE SPACES              TO  EL685B-CHK-DT(EL685B-INDEX)    CL*17
CIDMOD     END-IF.
01179                                                                      CL**4
01180      IF EL685B-INDEX LESS THAN +18                                   CL**4
01181          SET EL685B-INDEX UP BY +1                                   CL**4
01182          GO TO 5100-READNEXT.                                        CL**4
01183                                                                      CL**4
01184      MOVE +1                     TO  WS-READNEXT-SW.                 CL**4
01185      GO TO 5100-READNEXT.                                            CL**4
01186                                                                      CL**4
01187  5200-PRINTED.                                                       CL*13
01188                                                                      CL*13
01189      IF PI-FIRST-TIME                                                CL*13
01190          MOVE COMMISSION-CHECK-QUE                                   CL*13
01191                                  TO  HOLD-CHECK-RECORD               CL*13
01192          IF BCNTLNOL IS EQUAL TO +0                                  CL*13
01193              MOVE PI-CK-CONTROL-NO                                   CL*13
01194                                  TO  PI-START-CONTROL-NO             CL*13
01195              MOVE 'N'            TO  PI-FIRST-TIME-SW                CL*18
01196              GO TO 5100-READNEXT.                                    CL*18
01197                                                                      CL*13
01198      IF MQ-COMPANY-CD-A1  = HOLD-COMPANY-CD-A1  AND                  CL*13
01199         MQ-CONTROL-NUMBER = HOLD-CONTROL-NUMBER AND                  CL*13
01200         MQ-CARRIER-A1     = HOLD-CARRIER-A1     AND                  CL*13
01201         MQ-GROUPING-A1    = HOLD-GROUPING-A1    AND                  CL*13
01202         MQ-PAYEE-A1       = HOLD-PAYEE-A1       AND                  CL*13
01203         MQ-PAYEE-SEQ-A1   = HOLD-PAYEE-SEQ-A1                        CL*13
01204         IF MQ-TEXT                                                   CL*18
01205             IF MQ-CHECK-NUMBER = HOLD-CHECK-NUMBER                   CL*18
01206                 GO TO 5100-READNEXT                                  CL*18
01207             ELSE                                                     CL*18
01208                 MOVE HOLD-CHECK-AMOUNT                               CL*18
01209                                  TO  MQ-CHECK-AMOUNT                 CL*18
01210                 MOVE ZEROS       TO  HOLD-CHECK-AMOUNT               CL*18
01211         ELSE                                                         CL*18
01212             IF MQ-CHECK-NUMBER = HOLD-CHECK-NUMBER                   CL*18
01213                 MOVE COMMISSION-CHECK-QUE                            CL*18
01214                                  TO  HOLD-CHECK-RECORD               CL*18
01215                 GO TO 5100-READNEXT                                  CL*18
01216             ELSE                                                     CL*18
01217                 GO TO 5210-CONTINUE.                                 CL*18
01218                                                                      CL*13
01219      IF MQ-CHECK-NUMBER NOT = HOLD-CHECK-NUMBER                      CL*13
01220          NEXT SENTENCE                                               CL*13
01221      ELSE                                                            CL*13
01222          MOVE COMMISSION-CHECK-QUE                                   CL*13
01223                                  TO  HOLD-CHECK-RECORD               CL*13
01224          GO TO 5100-READNEXT.                                        CL*13
01225                                                                      CL*13
01226  5210-CONTINUE.                                                      CL*13
01227                                                                      CL*13
01228 *    IF EL685B-INDEX LESS THAN +18                                   CL*18
01229         IF PI-CONTROL-SAVE-CONTROL NOT = HOLD-CONTROL-NUMBER         CL*13
01230            IF PI-FIRST-TOTAL                                         CL*13
01231                MOVE HOLD-CONTROL-NUMBER                              CL*13
01232                                  TO PI-CONTROL-SAVE-CONTROL          CL*13
01233                MOVE PI-CK-CONTROL-NO                                 CL*13
01234                                  TO  PI-START-CONTROL-NO             CL*13
01235                MOVE 'N'          TO  PI-FIRST-TOTAL-SW               CL*13
01236            ELSE                                                      CL*13
01237               MOVE ' CONTL'      TO EL685B-DESC-ONE (EL685B-INDEX)   CL*13
01238               MOVE 'TOTAL'       TO EL685B-DESC-TWO (EL685B-INDEX)   CL*13
01239               MOVE PI-CONTROL-TOT                                    CL*13
01240                                  TO EL685B-AMT (EL685B-INDEX)        CL*13
01241               SET EL685B-INDEX UP BY +1                              CL*13
01242               MOVE ZEROS         TO PI-CONTROL-TOT                   CL*13
01243               MOVE HOLD-CONTROL-NUMBER                               CL*13
01244                                  TO PI-CONTROL-SAVE-CONTROL.         CL*13
01245                                                                      CL*13
01246      ADD HOLD-CHECK-AMOUNT       TO  PI-CONTROL-TOT                  CL*13
01247                                      PI-CONTROL-GRAND-TOT.           CL*13
01248      MOVE HOLD-CONTROL-NUMBER TO  EL685B-CONTROL   (EL685B-INDEX).   CL*13
01249                                                                      CL*17
01250      IF HOLD-ENTRY-TYPE = 'P'                                        CL*17
01251          MOVE ' ACH  '                                               CL*17
01252                               TO  EL685B-CHECK-NO  (EL685B-INDEX)    CL*17
01253      ELSE                                                            CL*17
01254          MOVE HOLD-CHECK-NUMBER                                      CL*17
01255                               TO  EL685B-CHECK-NO  (EL685B-INDEX).   CL*17
01256                                                                      CL*17
01257      MOVE HOLD-CHECK-AMOUNT   TO  EL685B-AMT       (EL685B-INDEX).   CL*13
01258      MOVE HOLD-CHEK-CARRIER   TO  EL685B-CARRIER   (EL685B-INDEX).   CL*13
01259      MOVE HOLD-CHEK-GROUPING  TO  EL685B-GROUPING  (EL685B-INDEX).   CL*13
01260      MOVE HOLD-CHEK-PAYEE     TO  EL685B-PAYEE     (EL685B-INDEX).   CL*13
01261      MOVE HOLD-PAYEE-SEQ-A1   TO  EL685B-PAYEE-SEQ (EL685B-INDEX).   CL*13
01262      MOVE HOLD-PAYEE-NAME     TO  EL685B-PAYEE-NA  (EL685B-INDEX).   CL*13
01263                                                                      CL*13
01264      IF HOLD-CHECK-WRITTEN-DT = ZEROS OR LOW-VALUES OR SPACES        CL*17
01265          MOVE SPACES                                                 CL*17
01266                               TO  EL685B-CHK-DT (EL685B-INDEX)       CL*17
01267      ELSE                                                            CL*17
01268          MOVE HOLD-CHECK-WRITTEN-DT                                  CL*17
01269                               TO  DC-BIN-DATE-1                      CL*17
01270          MOVE ' '             TO  DC-OPTION-CODE                     CL*17
01271          PERFORM 8500-DATE-CONVERSION                                CL*17
01272          MOVE DC-GREG-DATE-1-EDIT                                    CL*17
01273                               TO  EL685B-CHK-DT (EL685B-INDEX).      CL*17
01274                                                                      CL*13
01275      MOVE COMMISSION-CHECK-QUE   TO  HOLD-CHECK-RECORD.              CL*13
01276                                                                      CL*13
01277      IF EL685B-INDEX LESS THAN +18                                   CL*13
01278          SET EL685B-INDEX UP BY +1                                   CL*13
01279          GO TO 5100-READNEXT.                                        CL*13
01280                                                                      CL*13
01281      IF EL685B-INDEX GREATER THAN +17                                CL*18
01282          MOVE 'Y'                TO  PI-PAGING-SW                    CL*18
01283          GO TO 5900-WRITE-REPORT.                                    CL*18
01284                                                                      CL*18
01285      GO TO 5100-READNEXT.                                            CL*13
01286                                                                      CL*13
01287  5800-END-OF-FILE.                                                   CL**4
01288                                                                      CL*13
01289      IF CHECKS-PRINTED                                               CL*13
01290          GO TO 5800-END-PRINTED.                                     CL*17
01291                                                                      CL*17
01292      IF EL685B-INDEX LESS THAN +18                                   CL*17
01293          SET EL685B-INDEX UP BY +1.                                  CL*17
CIDMOD                                                                       000
CIDMOD     EXEC CICS                                                         000
CIDMOD          ASKTIME                                                      000
CIDMOD     END-EXEC.                                                         000
01294                                                                      CL*17
01295      MOVE +1                     TO  PI-END-OF-FILE.                 CL*17
01296      MOVE 'Y'                    TO  PI-EOF-SWT.                     CL*17
01297                                                                      CL*17
01298      IF EL685B-INDEX GREATER +16                                     CL*17
01299         MOVE 'Y'                 TO PI-SEND-TOT-SWT                  CL*17
01300         GO TO 5900-WRITE-REPORT.                                     CL*17
01301                                                                      CL*17
01302      MOVE +375                   TO  EMI-ERROR.                      CL*17
01303                                                                      CL*17
01304      IF EL685B-INDEX LESS THAN +18                                   CL*17
01305               MOVE ' CONTL'     TO EL685B-DESC-ONE (EL685B-INDEX)    CL*17
01306               MOVE 'TOTAL'      TO  EL685B-DESC-TWO (EL685B-INDEX)   CL*17
01307               MOVE PI-CONTROL-TOT                                    CL*17
01308                                  TO EL685B-AMT (EL685B-INDEX)        CL*17
01309               SET EL685B-INDEX UP BY +1                              CL*17
01310      ELSE                                                            CL*17
01311         NEXT SENTENCE.                                               CL*17
01312                                                                      CL*17
01313      IF EL685B-INDEX LESS THAN +18                                   CL*17
01314         SET EL685B-INDEX UP BY +1                                    CL*17
01315         MOVE PI-CONTROL-GRAND-TOT                                    CL*17
01316                                  TO EL685B-AMT (EL685B-INDEX)        CL*17
01317         MOVE ' GRAND'            TO EL685B-DESC-ONE (EL685B-INDEX)   CL*17
01318         MOVE 'TOTAL'             TO EL685B-DESC-TWO (EL685B-INDEX)   CL*17
01319      ELSE                                                            CL*17
01320         NEXT SENTENCE.                                               CL*17
01321                                                                      CL*17
01322      GO TO 5900-WRITE-REPORT.                                        CL*17
01323                                                                      CL*17
01324  5800-END-PRINTED.                                                   CL*17
01325                                                                      CL*17
01326      IF EL685B-INDEX LESS THAN +18                                   CL*17
01327         IF PI-CONTROL-SAVE-CONTROL NOT = HOLD-CONTROL-NUMBER         CL*17
01328            IF PI-FIRST-TOTAL                                         CL*17
01329                MOVE HOLD-CONTROL-NUMBER                              CL*17
01330                                  TO PI-CONTROL-SAVE-CONTROL          CL*17
01331                MOVE PI-CK-CONTROL-NO                                 CL*17
01332                                  TO  PI-START-CONTROL-NO             CL*17
01333                MOVE 'N'          TO  PI-FIRST-TOTAL-SW               CL*17
01334            ELSE                                                      CL*17
01335               MOVE ' CONTL'      TO EL685B-DESC-ONE (EL685B-INDEX)   CL*17
01336               MOVE 'TOTAL'       TO EL685B-DESC-TWO (EL685B-INDEX)   CL*17
01337               MOVE PI-CONTROL-TOT                                    CL*17
01338                                  TO EL685B-AMT (EL685B-INDEX)        CL*17
01339               SET EL685B-INDEX UP BY +1                              CL*17
01340               MOVE ZEROS         TO PI-CONTROL-TOT                   CL*17
01341               MOVE HOLD-CONTROL-NUMBER                               CL*17
01342                                  TO PI-CONTROL-SAVE-CONTROL.         CL*17
01343                                                                      CL*17
01344      ADD HOLD-CHECK-AMOUNT       TO  PI-CONTROL-TOT                  CL*13
01345                                      PI-CONTROL-GRAND-TOT            CL*13
01346      MOVE HOLD-CONTROL-NUMBER TO  EL685B-CONTROL   (EL685B-INDEX)    CL*13
01347                                                                      CL*17
01348      IF HOLD-ENTRY-TYPE = 'P'                                        CL*17
01349          MOVE ' ACH  '                                               CL*17
01350                               TO  EL685B-CHECK-NO  (EL685B-INDEX)    CL*17
01351      ELSE                                                            CL*17
01352          MOVE HOLD-CHECK-NUMBER                                      CL*17
01353                               TO  EL685B-CHECK-NO  (EL685B-INDEX).   CL*17
01354                                                                      CL*17
01355      MOVE HOLD-CHECK-AMOUNT   TO  EL685B-AMT       (EL685B-INDEX)    CL*13
01356      MOVE HOLD-CHEK-CARRIER   TO  EL685B-CARRIER   (EL685B-INDEX)    CL*13
01357      MOVE HOLD-CHEK-GROUPING  TO  EL685B-GROUPING  (EL685B-INDEX)    CL*13
01358      MOVE HOLD-CHEK-PAYEE     TO  EL685B-PAYEE     (EL685B-INDEX)    CL*13
01359      MOVE HOLD-PAYEE-SEQ-A1   TO  EL685B-PAYEE-SEQ (EL685B-INDEX)    CL*13
01360      MOVE HOLD-PAYEE-NAME     TO  EL685B-PAYEE-NA  (EL685B-INDEX)    CL*13
01361      MOVE HOLD-CHECK-WRITTEN-DT                                      CL*17
01362                               TO  DC-BIN-DATE-1                      CL*17
01363      MOVE ' '                 TO  DC-OPTION-CODE                     CL*17
01364      PERFORM 8500-DATE-CONVERSION                                    CL*13
01365      MOVE DC-GREG-DATE-1-EDIT                                        CL*17
01366                               TO  EL685B-CHK-DT (EL685B-INDEX).      CL*17
01367                                                                      CL*17
01368      IF EL685B-INDEX LESS THAN +18                                   CL*13
01369          SET EL685B-INDEX UP BY +1.                                  CL*13
01370                                                                      CL**4
01371      MOVE +1                     TO  PI-END-OF-FILE.                 CL**4
01372      MOVE 'Y'                    TO  PI-EOF-SWT.                     CL*13
01373                                                                      CL**4
01374      IF EL685B-INDEX GREATER +16                                     CL**4
01375         MOVE 'Y'                 TO PI-SEND-TOT-SWT                  CL*13
01376         GO TO 5900-WRITE-REPORT.                                     CL*13
01377                                                                      CL**4
01378      MOVE +375                   TO  EMI-ERROR.                      CL**4
01379                                                                      CL*13
01380      IF EL685B-INDEX LESS THAN +18                                   CL**4
01381               MOVE ' CONTL'     TO EL685B-DESC-ONE (EL685B-INDEX)    CL*13
01382               MOVE 'TOTAL'      TO  EL685B-DESC-TWO (EL685B-INDEX)   CL*13
01383               MOVE PI-CONTROL-TOT                                    CL*13
01384                                  TO EL685B-AMT (EL685B-INDEX)        CL*13
01385               SET EL685B-INDEX UP BY +1                              CL**4
01386      ELSE                                                            CL**4
01387         NEXT SENTENCE.                                               CL**4
01388                                                                      CL*13
01389      IF EL685B-INDEX LESS THAN +18                                   CL**4
01390         SET EL685B-INDEX UP BY +1                                    CL**4
01391         MOVE PI-CONTROL-GRAND-TOT                                    CL*13
01392                                  TO EL685B-AMT (EL685B-INDEX)        CL*13
01393         MOVE ' GRAND'            TO EL685B-DESC-ONE (EL685B-INDEX)   CL*13
01394         MOVE 'TOTAL'             TO EL685B-DESC-TWO (EL685B-INDEX)   CL*13
01395      ELSE                                                            CL**4
01396         NEXT SENTENCE.                                               CL**4
01397                                                                      CL**4
01398  5900-WRITE-REPORT.                                                  CL*13
01399      MOVE -1                     TO  BPFKL                           CL**4
01400                                                                      CL**4
01401      MOVE EIBTRMID               TO  WS-TS-TERM-ID                   CL**4
01402                                                                      CL**4
01403      EXEC CICS WRITEQ TS                                             CL**4
01404          QUEUE  (WS-TEMP-STORAGE-KEY)                                CL**4
01405          ITEM   (PI-TEMP-STORAGE-ITEM)                               CL**4
01406          FROM   (EL685BI)                                            CL**4
01407          LENGTH (WS-TS-LENGTH)                                       CL*13
01408      END-EXEC.                                                       CL*13
01409                                                                      CL**4
01410      MOVE PI-TEMP-STORAGE-ITEM   TO  BPAGEO.                         CL*13
01411                                                                      CL**4
01412      PERFORM 8100-SEND-INITIAL-MAP.                                  CL**4
01413                                                                      CL**4
01414  5990-EXIT.                                                          CL**4
01415      EXIT.                                                           CL**4
01416                                                                      CL**4
01417      EJECT                                                           CL**4
01418  8100-SEND-INITIAL-MAP SECTION.                                   EL685
01419                                                                   EL685
01420      IF PI-COMPANY-ID = 'LAP'  OR  'RMC'                             CL*11
01421          IF NOT PI-CALLED-FROM-AR-MENU                               CL*11
01422              MOVE ' ACCOUNT'     TO ADESC1O                          CL*13
01423              MOVE 'CERT NO.'     TO ADESC2O.                         CL*13
01424                                                                      CL*11
01425      IF CHECKS-TO-BE-PRINTED                                      EL685
01426          IF PI-CALLED-FROM-AR-MENU                                   CL**4
01427              MOVE WS-TO-BE-PRINTED-PFDESC     TO BPFDESCO            CL**4
01428              MOVE WS-AR-TO-BE-PRINTED-DESC    TO BTITLEO             CL**4
01429              MOVE 'EL685B'                    TO ASCREENO            CL*10
01430          ELSE                                                        CL**4
01431              MOVE WS-TO-BE-PRINTED-PFDESC     TO APFDESCO            CL**4
01432              MOVE WS-TO-BE-PRINTED-DESC       TO ATITLEO             CL**4
01433              MOVE 'EL685A'                    TO ASCREENO            CL*10
01434      ELSE                                                         EL685
01435          IF PI-CALLED-FROM-AR-MENU                                   CL**4
01436              MOVE WS-CHECKS-PRINTED-PFDESC    TO BPFDESCO            CL**4
01437              MOVE WS-AR-CHECKS-PRINTED-DESC   TO BTITLEO             CL**4
01438              MOVE 'EL685D'                    TO BSCREENO            CL*10
01439          ELSE                                                        CL**4
01440              MOVE WS-CHECKS-PRINTED-PFDESC    TO APFDESCO            CL**4
01441              MOVE WS-CHECKS-PRINTED-DESC      TO ATITLEO             CL*10
01442              MOVE 'EL685C'                    TO ASCREENO.           CL*10
01443                                                                   EL685
01444      IF EMI-ERROR NOT = ZERO                                      EL685
01445          PERFORM 9900-ERROR-FORMAT.                               EL685
01446                                                                   EL685
01447      MOVE EIBTIME              TO  WS-TIME-WORK.                     CL**4
01448      IF PI-CALLED-FROM-AR-MENU                                       CL**4
01449          MOVE PI-CURRENT-DATE      TO  BDATEO                        CL**4
01450          MOVE WS-TIME              TO  BTIMEO                        CL**4
01451          MOVE EMI-MESSAGE-AREA (1) TO  BEMSG1O                       CL**4
01452      ELSE                                                            CL**4
01453          MOVE PI-CURRENT-DATE      TO  ADATEO                        CL**4
01454          MOVE WS-TIME              TO  ATIMEO                        CL**4
01455          MOVE EMI-MESSAGE-AREA (1) TO  AEMSG1O.                      CL**4
01456                                                                   EL685
01457      IF PI-CALLED-FROM-AR-MENU                                       CL**4
01458          EXEC CICS SEND                                              CL**4
01459              FROM   (EL685BI)                                        CL**4
01460              MAPSET (WS-MAPSET-NAME)                                 CL**4
01461              MAP    (WS-MAP-NAME)                                    CL**4
01462              CURSOR ERASE                                            CL*13
01463          END-EXEC                                                    CL*13
01464      ELSE                                                            CL**4
01465          EXEC CICS SEND                                              CL**4
01466              FROM   (EL685AI)                                        CL**4
01467              MAPSET (WS-MAPSET-NAME)                                 CL**4
01468              MAP    (WS-MAP-NAME)                                    CL**4
01469              CURSOR ERASE                                            CL*13
01470          END-EXEC.                                                   CL*13
01471                                                                   EL685
01472      PERFORM 9100-RETURN-TRAN.                                    EL685
01473                                                                   EL685
01474  8100-EXIT.                                                       EL685
01475      EXIT.                                                        EL685
01476                                                                   EL685
01477      EJECT                                                        EL685
01478  8200-SEND-DATAONLY SECTION.                                      EL685
01479                                                                      CL*11
01480      IF PI-COMPANY-ID = 'LAP'  OR  'RMC'                             CL*11
01481          IF NOT PI-CALLED-FROM-AR-MENU                               CL*11
01482              MOVE ' ACCOUNT'                  TO ADESC1O             CL*11
01483              MOVE 'CERT NO.'                  TO ADESC2O.            CL*11
01484                                                                   EL685
01485      IF CHECKS-TO-BE-PRINTED                                      EL685
01486          IF PI-CALLED-FROM-AR-MENU                                   CL**4
01487              MOVE WS-TO-BE-PRINTED-PFDESC     TO BPFDESCO            CL**4
01488              MOVE WS-AR-TO-BE-PRINTED-DESC    TO BTITLEO             CL**4
01489              MOVE 'EL685B'                    TO BSCREENO            CL*10
01490          ELSE                                                        CL**4
01491              MOVE WS-TO-BE-PRINTED-PFDESC     TO APFDESCO            CL**4
01492              MOVE WS-TO-BE-PRINTED-DESC       TO ATITLEO             CL**4
01493              MOVE 'EL685A'                    TO ASCREENO            CL*10
01494      ELSE                                                         EL685
01495          IF PI-CALLED-FROM-AR-MENU                                   CL**4
01496              MOVE WS-CHECKS-PRINTED-PFDESC    TO BPFDESCO            CL**4
01497              MOVE WS-AR-CHECKS-PRINTED-DESC   TO BTITLEO             CL**4
01498              MOVE 'EL685D'                    TO BSCREENO            CL*10
01499          ELSE                                                        CL**4
01500              MOVE WS-CHECKS-PRINTED-PFDESC    TO APFDESCO            CL**4
01501              MOVE WS-CHECKS-PRINTED-DESC      TO ATITLEO             CL*10
01502              MOVE 'EL685C'                    TO ASCREENO.           CL*10
01503                                                                   EL685
01504      IF EMI-ERROR NOT = ZERO                                      EL685
01505          PERFORM 9900-ERROR-FORMAT.                               EL685
01506                                                                   EL685
01507      MOVE EIBTIME                TO  WS-TIME-WORK.                EL685
01508      IF PI-CALLED-FROM-AR-MENU                                       CL**4
01509          MOVE PI-CURRENT-DATE        TO  BDATEO                      CL**4
01510          MOVE WS-TIME                TO  BTIMEO                      CL**4
01511          MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O                     CL**4
01512      ELSE                                                            CL**4
01513          MOVE PI-CURRENT-DATE        TO  ADATEO                      CL**4
01514          MOVE WS-TIME                TO  ATIMEO                      CL**4
01515          MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                    CL**4
01516                                                                   EL685
01517      IF PI-CALLED-FROM-AR-MENU                                       CL**4
01518          EXEC CICS SEND DATAONLY                                     CL**4
01519              FROM   (EL685AI)                                        CL**4
01520              MAPSET (WS-MAPSET-NAME)                                 CL**4
01521              MAP    (WS-MAP-NAME)                                    CL**4
01522              CURSOR                                                  CL*13
01523          END-EXEC                                                    CL*13
01524      ELSE                                                            CL**4
01525          EXEC CICS SEND DATAONLY                                     CL**4
01526              FROM   (EL685BI)                                        CL**4
01527              MAPSET (WS-MAPSET-NAME)                                 CL**4
01528              MAP    (WS-MAP-NAME)                                    CL**4
01529              CURSOR                                                  CL*13
01530          END-EXEC.                                                   CL*13
01531                                                                   EL685
01532      PERFORM 9100-RETURN-TRAN.                                    EL685
01533                                                                   EL685
01534  8100-EXIT.                                                       EL685
01535      EXIT.                                                        EL685
01536                                                                   EL685
01537      EJECT                                                        EL685
01538  8300-SEND-TEXT SECTION.                                          EL685
01539                                                                   EL685
01540      EXEC CICS SEND TEXT                                          EL685
01541          FROM   (LOGOFF-TEXT)                                     EL685
01542          LENGTH (LOGOFF-LENGTH)                                   EL685
01543          ERASE  FREEKB                                               CL*13
01544      END-EXEC,                                                       CL*13
01545                                                                   EL685
01546      EXEC CICS RETURN                                             EL685
01547          END-EXEC.                                                EL685
01548                                                                   EL685
01549                                                                   EL685
01550  8300-EXIT.                                                       EL685
01551      EXIT.                                                        EL685
01552                                                                   EL685
01553  8400-NOTFND.                                                     EL685
01554      MOVE 0586 TO EMI-ERROR.                                      EL685
01555      PERFORM 9900-ERROR-FORMAT                                    EL685
01556         THRU 9900-EXIT.                                           EL685
01557                                                                      CL**4
01558      IF  PI-CALLED-FROM-AR-MENU                                      CL**4
01559          MOVE -1 TO BPFKL                                            CL**4
01560      ELSE                                                            CL**4
01561          MOVE -1 TO APFKL.                                           CL**4
01562                                                                      CL**4
01563      GO TO 8100-SEND-INITIAL-MAP.                                 EL685
01564                                                                   EL685
01565  8400-EXIT.                                                       EL685
01566      EXIT.                                                        EL685
01567                                                                   EL685
01568      EJECT                                                        EL685
01569  8500-DATE-CONVERSION SECTION.                                    EL685
01570                                                                   EL685
01571      EXEC CICS LINK                                               EL685
01572          PROGRAM  ('ELDATCV')                                        CL**2
01573          COMMAREA (DATE-CONVERSION-DATA)                          EL685
01574          LENGTH   (DC-COMM-LENGTH)                                   CL*13
01575      END-EXEC.                                                       CL*13
01576                                                                   EL685
01577  8500-EXIT.                                                       EL685
01578      EXIT.                                                        EL685
01579                                                                   EL685
01580      EJECT                                                        EL685
01581  9000-RETURN-CICS SECTION.                                        EL685
01582                                                                   EL685
01583      MOVE 'EL005   '             TO  WS-PROGRAM-ID                EL685
01584      MOVE EIBAID                 TO  PI-ENTRY-CD-1                EL685
01585      PERFORM 9300-XCTL.                                           EL685
01586                                                                   EL685
01587  9000-EXIT.                                                       EL685
01588      EXIT.                                                        EL685
01589                                                                   EL685
01590  9100-RETURN-TRAN SECTION.                                        EL685
01591                                                                   EL685
01592      MOVE EMI-ERROR-NUMBER (1)  TO  PI-LAST-ERROR-NO              EL685
01593      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO         EL685
01594                                                                   EL685
01595      EXEC CICS RETURN                                             EL685
01596          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL685
01597          LENGTH   (PI-COMM-LENGTH)                                EL685
01598          TRANSID  (WS-TRANS-ID)                                      CL*13
01599      END-EXEC.                                                       CL*13
01600                                                                   EL685
01601  9100-EXIT.                                                       EL685
01602      EXIT.                                                        EL685
01603                                                                   EL685
01604  9300-XCTL SECTION.                                               EL685
01605      EXEC CICS HANDLE CONDITION                                   EL685
01606          QIDERR (9300-NEXT-SENTENCE)                                 CL*13
01607      END-EXEC.                                                       CL*13
01608                                                                   EL685
01609      MOVE EIBTRMID               TO  WS-TS-TERM-ID                EL685
01610                                                                   EL685
01611      EXEC CICS DELETEQ TS                                         EL685
01612          QUEUE (WS-TEMP-STORAGE-KEY)                                 CL*13
01613      END-EXEC.                                                       CL*13
01614                                                                   EL685
01615  9300-NEXT-SENTENCE.                                              EL685
01616                                                                   EL685
01617      MOVE DFHENTER               TO  EIBAID                       EL685
01618                                                                   EL685
01619      EXEC CICS XCTL                                               EL685
01620          PROGRAM  (WS-PROGRAM-ID)                                 EL685
01621          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL685
01622          LENGTH   (PI-COMM-LENGTH)                                   CL*13
01623      END-EXEC.                                                       CL*13
01624                                                                   EL685
01625  9300-EXIT.                                                       EL685
01626      EXIT.                                                        EL685
01627                                                                   EL685
01628  9400-CLEAR SECTION.                                              EL685
01629                                                                   EL685
01630      MOVE PI-RETURN-TO-PROGRAM  TO  WS-PROGRAM-ID                 EL685
01631      PERFORM 9300-XCTL.                                           EL685
01632                                                                   EL685
01633  9400-EXIT.                                                       EL685
01634      EXIT.                                                        EL685
01635                                                                   EL685
01636  9600-PGMIDERR SECTION.                                           EL685
01637                                                                   EL685
01638      EXEC CICS HANDLE CONDITION                                   EL685
01639          PGMIDERR (8300-SEND-TEXT)                                   CL*13
01640      END-EXEC.                                                       CL*13
01641                                                                   EL685
01642      MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM           EL685
01643                                      LOGOFF-PGM                   EL685
01644                                                                   EL685
01645      MOVE 'EL005   '             TO  WS-PROGRAM-ID                EL685
01646      MOVE SPACES                 TO  PI-ENTRY-CD-1                EL685
01647                                                                   EL685
01648      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL                  EL685
01649                                                                   EL685
01650      PERFORM 9300-XCTL.                                           EL685
01651                                                                   EL685
01652  9600-EXIT.                                                       EL685
01653      EXIT.                                                        EL685
01654                                                                   EL685
01655      EJECT                                                        EL685
01656  9900-ERROR-FORMAT SECTION.                                       EL685
01657                                                                   EL685
01658      EXEC CICS LINK                                               EL685
01659          PROGRAM  ('EL001')                                       EL685
01660          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL685
01661          LENGTH   (EMI-COMM-LENGTH)                                  CL*13
01662      END-EXEC.                                                       CL*13
01663                                                                   EL685
01664  9900-EXIT.                                                       EL685
01665      EXIT.                                                        EL685
01666                                                                   EL685
01667  9990-ERROR SECTION.                                              EL685
01668                                                                   EL685
01669      MOVE DFHEIBLK TO EMI-LINE1.                                  EL685
01670      EXEC CICS LINK                                               EL685
01671          PROGRAM  ('EL004')                                       EL685
01672          COMMAREA (EMI-LINE1)                                     EL685
01673          LENGTH   (72)                                               CL*13
01674      END-EXEC.                                                       CL*13
01675                                                                   EL685
01676      IF PI-CALLED-FROM-AR-MENU                                       CL**4
01677          MOVE -1 TO BPFKL                                            CL**4
01678      ELSE                                                            CL**4
01679          MOVE -1 TO APFKL.                                           CL**4
01680      PERFORM 8100-SEND-INITIAL-MAP.                               EL685
01681      GO TO 9100-RETURN-TRAN.                                      EL685
01682                                                                   EL685
01683  9990-EXIT.                                                       EL685
01684      EXIT.                                                        EL685
01685                                                                   EL685
01686  9999-LAST-PARAGRAPH SECTION.                                     EL685
01687                                                                   EL685
01688      GOBACK.                                                      EL685
01689                                                                   EL685
