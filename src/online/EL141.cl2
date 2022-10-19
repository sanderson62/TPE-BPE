00001  IDENTIFICATION DIVISION.                                         05/29/96
00002                                                                   EL141
00003  PROGRAM-ID.                 EL141 .                                 LV023
00004 *              PROGRAM CONVERTED BY                                  CL*20
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*20
00006 *              CONVERSION DATE 07/15/94 10:58:41.                    CL*20
00007 *                            VMOD=2.023                              CL*23
00008 *                                                                 EL141
00008 *                                                                 EL141
00009 *AUTHOR.    LOGIC, INC.                                              CL*20
00010 *           DALLAS, TEXAS.                                           CL*20
00011                                                                   EL141
00012 *DATE-COMPILED.                                                      CL*20
00013                                                                   EL141
00014 *SECURITY.   *****************************************************   CL*20
00015 *            *                                                   *   CL*20
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*20
00017 *            *                                                   *   CL*20
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*20
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*20
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *   CL*20
00021 *            *                                                   *   CL*20
00022 *            *****************************************************   CL*20
00023 *REMARKS.   TRANSACTION - EX18                                       CL*20
00024                                                                   EL141
00025 *    SCREENS     - EL141S - ADDRESS MAINTENANCE                      CL**3
00026                                                                   EL141
00027 *    ENTERED BY  - EL132 - NEW CLAIM SETUP                           CL**3
00028 *                - EL156 - PAYMENTS                                  CL**3
00029 *                - EL162 - MAIL RECORDING                            CL**3
00030 *                - EL150 - STATUS AND DISPOSITION                    CL*17
00031 *                - EL130 - NEW CLAIM SET-UP                          CL*17
00032 *                - EM130 - CONVENIENCE NEW CLAIM SET-UP              CL*18
00033                                                                   EL141
00034 *    EXIT TO     - EL101 - MAINTENANCE MENU                          CL**3
00035                                                                   EL141
00036 *    INPUT FILE  - ELMSTR - CLAIM MASTER                             CL**3
00037 *                - ELTRLR - ACTIVITY TRAILERS                        CL**3
00038 *                - ERACCT - ACCOUNT MASTER                           CL**3
00039 *                - MPPROD - CONVENIENCE PRODUCER MASTER              CL*18
00040                                                                   EL141
00041 *    OUTPUT FILE - ELMSTR - CLAIM MASTER                             CL**3
00042 *                - ELTRLR - ACTIVITY TRAILERS                        CL**3
00043                                                                   EL141
00044 *    COMMAREA    - PASSED CLAIM NUMBER FROM PROG INTERFACE BLK       CL**3
00045                                                                   EL141
00046 *    ERROR-CODES ACCESSED - 23, 135, 270, 20, 277, 278, 132, 52      CL**3
00047 *                           133, 136, 137, 50, 29, 08                CL**3
00048 *    NARRATIVE   - PROVIDE COMPLETE MAINTENANCE FUNCTIONS,           CL**3
00049 *                  (ADD,CHANGE,DELETE,SHOW) FOR THE ADDRESS-         CL**3
00050 *                  TRAILER RECORDS OF THE CLAIM MASTER FILE/         CL**3
00051 *                  FOR MAINTENANCE FUNCTIONS(A, C, D), THE           CL**3
00052 *                  CL-TRAILER-SEQ-CNT AND CL-ADDRESS-TRAILER-SEQ,    CL**3
00053 *                  FIELDS OF THE CLAIM MASTER ALONG WITH             CL**3
00054 *                  MODIFIED ( TIME, DATE, PERSON) ARE UPDATED/       CL**3
00055 *                  ACCOUNT MASTER FILE IS ONLY  SCANNED FOR          CL**3
00056 *                  ACCOUNT ADDRESS INFORMATION WHEN ACCT ADDRESS     CL**3
00057 *                  SEQ NO OF CLMSTR FOR TRAILER TYPE (5) IS ZEROS.   CL**3
00058 *                  ON ENTRY, SHOW EXISTING ADDRESS TRAILERS          CL**3
00059 *                  BY HIGHLIGHTING THE 1-7 FIELDS ON MAP             CL**3
00060                                                                   EL141
102901******************************************************************
102901*                   C H A N G E   L O G
102901*
102901* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102901*-----------------------------------------------------------------
102901*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102901* EFFECTIVE    NUMBER
102901*-----------------------------------------------------------------
102901* 031102    2002022100003  SMVA  ADD CERT# TO EL141A SCREEN HEADER
061511* 061511    2011042000002  AJRA  VERIFY 2ND BENEFICIARY SSN
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
100713* 100713    2013100300003  AJRA  ALLOW LEVEL 4 TO VERIFY 2ND BENE
032514* 032514    2013111100001  AJRA  VERIFY SSN FOR DISAB PMTS
102901******************************************************************

00061      EJECT                                                        EL141
00062  ENVIRONMENT DIVISION.                                            EL141
00063                                                                   EL141
00064  DATA DIVISION.                                                   EL141
00065                                                                   EL141
00066  WORKING-STORAGE SECTION.                                         EL141
00067                                                                   EL141
00068  77  FILLER  PIC X(32)  VALUE '********************************'. EL141
00069  77  FILLER  PIC X(32)  VALUE '*   EL141  WORKING STORAGE     *'. EL141
00070  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.023 *********'.    CL*23
00071                                                                   EL141
00072      COPY ELCSCTM.                                                   CL*14
00073                                                                   EL141
00074      COPY ELCSCRTY.                                                  CL*14
00075                                                                   EL141
00076      EJECT                                                           CL*19
00077  01  WS-DATE-AREA.                                                EL141
00078      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL141
00079      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL141
00080                                                                      CL*21
00081 * DLO004                                                             CL*21
00082  01  DL04-COMM-LENGTH              PIC S9(4)  COMP VALUE +43.        CL*21
00083  01  WS-DLO-VALIDATE-STATE-ZIP.                                      CL*21
00084      12  DL04-ADDR-LINE            PIC X(30).                        CL*21
00085      12  DL04-ZIP-CODE             PIC X(9).                         CL*21
00086      12  DL04-RETURN-CODE          PIC XX.                           CL*21
00087      12  DL04-STATE-FOUND          PIC XX.                           CL*21
00088                                                                      CL*13
00089  01  MISC-WORK-AREAS.                                                CL*13
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           12  FILE-ID-ELCNTL          PIC X(8)    VALUE 'ELCNTL'.
           12  ELCNTL-KEY.
               16  ELCNTL-COMPANY-ID   PIC X(3)  VALUE SPACES.
               16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
               16  ELCNTL-ACCESS.
                   20  FILLER          PIC XX.
                   20  FILLER          PIC XX.
               16  ELCNTL-SEQ          PIC S9(4) VALUE +0 COMP.

00091      12  WS-ZIP-CODE.                                                CL*13
00092          16  WS-ZIP-1            PIC X.                              CL*13
00093              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.              CL*13
00094          16  WS-ZIP-2-3          PIC XX.                             CL*13
00095          16  WS-ZIP-4            PIC X.                              CL*13
00096          16  WS-ZIP-5            PIC X.                              CL*13
00097          16  WS-ZIP-6            PIC X.                              CL*13
00098          16  FILLER              PIC X(4).                           CL*13
00099      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.                        CL*13
00100          16  WS-ZIP-AM-1-CODE    PIC X(5).                           CL*13
00101          16  WS-ZIP-AM-1-PLUS4   PIC X(4).                           CL*13
00102          16  FILLER              PIC X.                              CL*13
00103      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.                        CL*13
00104          16  WS-ZIP-AM-2-CODE    PIC X(5).                           CL*13
00105          16  WS-ZIP-AM-2-DASH    PIC X.                              CL*13
00106          16  WS-ZIP-AM-2-PLUS4   PIC X(4).                           CL*13
00107      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.                       CL*13
00108          16  WS-ZIP-CAN-1-POST1  PIC XXX.                            CL*13
00109          16  WS-ZIP-CAN-1-POST2  PIC XXX.                            CL*13
00110          16  FILLER              PIC X(4).                           CL*13
00111      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.                       CL*13
00112          16  WS-ZIP-CAN-2-POST1  PIC XXX.                            CL*13
00113          16  FILLER              PIC X.                              CL*13
00114          16  WS-ZIP-CAN-2-POST2  PIC XXX.                            CL*13
00115          16  FILLER              PIC XXX.                            CL*13
00116                                                                   EL141
00117      EJECT                                                           CL*19
00118  01  AT-SAVE-TABLE.                                                  CL*20
00119    03  AT-SAVE-TABLE-OCCURS OCCURS 9 TIMES.                          CL*20
00120      12  AT-SAVE-CONTROL-PRIMARY.                                    CL*20
00121          16  AT-SAVE-COMPANY-CD          PIC X.                      CL*20
00122          16  AT-SAVE-CARRIER             PIC X.                      CL*20
00123          16  AT-SAVE-CLAIM-NO            PIC X(7).                   CL*20
00124          16  AT-SAVE-CERT-NO.                                        CL*20
00125              20  AT-SAVE-CERT-PRIME      PIC X(10).                  CL*20
00126              20  AT-SAVE-CERT-SFX        PIC X.                      CL*20
00127          16  AT-SAVE-SEQUENCE-NO         PIC S9(4)     COMP.         CL*20
00128      12  AT-SAVE-TRAILER-TYPE            PIC X.                      CL*20
00129      12  AT-SAVE-RECORDED-DT             PIC XX.                     CL*20
00130      12  AT-SAVE-RECORDED-BY             PIC X(4).                   CL*20
00131      12  AT-SAVE-LAST-MAINT-HHMMSS       PIC S9(6)     COMP-3.       CL*20
00132      12  AT-SAVE-ADDRESS-TYPE            PIC X.                      CL*20
00133      12  AT-SAVE-MAIL-TO-NAME            PIC X(30).                  CL*20
00134      12  AT-SAVE-ADDRESS-LINE-1          PIC X(30).                  CL*20
00135      12  AT-SAVE-ADDRESS-LINE-2          PIC X(30).                  CL*20
00136      12  AT-SAVE-CITY                    PIC X(28).                  CL*20
           12  AT-SAVE-STATE                   PIC XX.
00137      12  AT-SAVE-ZIP.                                                CL*20
00138          16  AT-SAVE-ZIP-CODE.                                       CL*20
00139              20  AT-SAVE-ZIP-1ST         PIC X.                      CL*20
00140              20  FILLER                  PIC X(4).                   CL*20
00141          16  AT-SAVE-ZIP-PLUS4           PIC X(4).                   CL*20
00142      12  AT-SAVE-PHONE-NO                PIC 9(11)     COMP-3.       CL*20
00143      12  AT-SAVE-ADDRESS-LAST-MAINT-DT   PIC XX.                     CL*20
00144      12  AT-SAVE-ADDRESS-LST-UPDATED-BY  PIC X(4).                   CL*20
00145      12  AT-SAVE-STATUS                  PIC X.                      CL*20
061511     12  AT-SAVE-VFY-2ND-BENE-SSN        PIC X(9).
061511     12  AT-SAVE-VFY-2ND-BENE-VERIFIED   PIC X.
00146                                                                      CL*20
00147      EJECT                                                           CL*20
00148  01  ERROR-MESSAGES.                                              EL141
00149      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL141
00150      12  ER-0004                 PIC X(4)  VALUE '0004'.             CL*17
00151      12  ER-0008                 PIC X(4)  VALUE '0008'.             CL*17
00152      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL141
00153      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL141
00154      12  ER-0050                 PIC X(4)  VALUE '0050'.          EL141
00155      12  ER-0052                 PIC X(4)  VALUE '0052'.          EL141
00156      12  ER-0053                 PIC X(4)  VALUE '0053'.          EL141
00157      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL141
00158      12  ER-0132                 PIC X(4)  VALUE '0132'.          EL141
00159      12  ER-0133                 PIC X(4)  VALUE '0133'.          EL141
00160      12  ER-0135                 PIC X(4)  VALUE '0135'.          EL141
00161      12  ER-0136                 PIC X(4)  VALUE '0136'.          EL141
00162      12  ER-0137                 PIC X(4)  VALUE '0137'.          EL141
00163      12  ER-0145                 PIC X(4)  VALUE '0145'.             CL*19
00164      12  ER-0154                 PIC X(4)  VALUE '0154'.          EL141
00165      12  ER-0168                 PIC X(4)  VALUE '0168'.          EL141
00166      12  ER-0172                 PIC X(4)  VALUE '0172'.          EL141
00167      12  ER-0265                 PIC X(4)  VALUE '0265'.             CL*17
00168      12  ER-0270                 PIC X(4)  VALUE '0270'.          EL141
00169      12  ER-0277                 PIC X(4)  VALUE '0277'.          EL141
00170      12  ER-0278                 PIC X(4)  VALUE '0278'.          EL141
00171      12  ER-0315                 PIC X(4)  VALUE '0315'.          EL141
00172      12  ER-0799                 PIC X(4)  VALUE '0799'.             CL*17
00173      12  ER-0853                 PIC X(4)  VALUE '0853'.             CL*22
00174      12  ER-0854                 PIC X(4)  VALUE '0854'.             CL*22
00175      12  ER-0855                 PIC X(4)  VALUE '0855'.             CL*22
00176      12  ER-0856                 PIC X(4)  VALUE '0856'.             CL*22
00177      12  ER-0857                 PIC X(4)  VALUE '0857'.             CL*22
00178      12  ER-0858                 PIC X(4)  VALUE '0858'.             CL*22
00179      12  ER-0859                 PIC X(4)  VALUE '0859'.             CL*22
00180      12  ER-0860                 PIC X(4)  VALUE '0860'.             CL*22
00181      12  ER-0886                 PIC X(4)  VALUE '0886'.             CL*21
00182      12  ER-0862                 PIC X(4)  VALUE '0862'.             CL*23
00183      12  ER-0863                 PIC X(4)  VALUE '0863'.             CL*23
061511     12  ER-1563                 PIC X(4)  VALUE '1563'.
00184      12  ER-1878                 PIC X(4)  VALUE '1878'.             CL**4
00185      12  ER-1879                 PIC X(4)  VALUE '1879'.             CL**4
00186      12  ER-2166                 PIC X(4)  VALUE '2166'.          EL141
           12  ER-2209                 PIC X(4)  VALUE '2209'.
00187      12  ER-2599                 PIC X(4)  VALUE '2599'.          EL141
061511     12  ER-3040                 PIC X(4)  VALUE '3040'.
00188      12  ER-7008                 PIC X(4)  VALUE '7008'.          EL141
032514     12  ER-7584                 PIC X(4)  VALUE '7584'.
00189      12  ER-7675                 PIC X(4)  VALUE '7675'.             CL*17
00190      12  ER-8131                 PIC X(4)  VALUE '8131'.             CL*20
00191      12  ER-8133                 PIC X(4)  VALUE '8133'.             CL*20
00192      12  ER-8134                 PIC X(4)  VALUE '8134'.             CL*20
00193      12  ER-9616                 PIC X(4)  VALUE '9616'.             CL*18
00194      12  ER-9886                 PIC X(4)  VALUE '9886'.             CL*18
00195                                                                   EL141
00196      EJECT                                                           CL*19
00197  01  WS-SCRATCH-AREA.                                             EL141
00198      05  WS-COMP-MSTR                PIC X VALUE SPACES.             CL**9
00199      05  SC-ITEM                     PIC S9(4) VALUE +0001 COMP.  EL141
00200      05  GETMAIN-SPACE               PIC X     VALUE SPACE.       EL141
00201      05  WS-TRLR-LENGTH              PIC S9(4) VALUE +200 COMP.   EL141
00202                                                                   EL141
00203      05  WS-MAP-NAME                 PIC X(08)   VALUE 'EL141A'.     CL*17
00204      05  WS-MAPSET-NAME              PIC X(08)   VALUE 'EL141S'.     CL*17
00205                                                                   EL141
00206      05  ELBENE-FILE-ID              PIC X(08)   VALUE 'ELBENE'.     CL*17
00207      05  ELMSTR-FILE-ID              PIC X(08)   VALUE 'ELMSTR'.     CL*17
00208      05  ELTRLR-FILE-ID              PIC X(08)   VALUE 'ELTRLR'.     CL*17
00209      05  ERACCT-FILE-ID              PIC X(08)   VALUE 'ERACCT'.     CL*17
00210      05  ERCOMP-FILE-ID              PIC X(08)   VALUE 'ERCOMP'.     CL*17
00211      05  EMPROD-FILE-ID              PIC X(08)   VALUE 'MPPROD'.     CL*18
00212                                                                      CL*17
00213      05  LINK-001                    PIC X(08)   VALUE 'EL001'.      CL*17
00214      05  LINK-004                    PIC X(08)   VALUE 'EL004'.      CL*17
00215      05  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'.    CL*17
00216                                                                      CL*17
00217      05  XCTL-005                    PIC X(08)   VALUE 'EL005'.      CL*17
00218      05  XCTL-010                    PIC X(08)   VALUE 'EL010'.      CL*17
00219      05  XCTL-114                    PIC X(08)   VALUE 'EL114'.      CL*17
00220      05  XCTL-126                    PIC X(08)   VALUE 'EL126'.      CL*17
00221                                                                   EL141
00222      05  WS-TRANS-ID                 PIC X(4)   VALUE 'EX18'.     EL141
00223                                                                   EL141
00224      05  WS-SEQ-CONVERT              PIC S9(4)   VALUE ZEROS.     EL141
00225      05  NDX                         PIC S99     COMP-3.             CL*20
00226                                                                   EL141
00227      05  THIS-PGM                    PIC X(08)   VALUE 'EL141'.      CL*17
00228      05  PGM-NAME                    PIC X(08)   VALUE SPACES.       CL*17
00229                                                                   EL141
00230      05  WS-PHONE                    PIC X(12).                   EL141
00231      05  FILLER        REDEFINES WS-PHONE.                        EL141
00232          10  FILLER                  PIC 9.                       EL141
00233          10  WS-PHONE-NUM            PIC 9(11).                   EL141
00234                                                                   EL141
00235      05  WS-PHONE-BRKDN              PIC 9(11).                   EL141
00236      05  FILLE    REDEFINES WS-PHONE-BRKDN.                       EL141
00237          10  FILLER                  PIC 9.                       EL141
00238          10  WS-PH-1                 PIC 999.                     EL141
00239          10  WS-PH-2                 PIC 999.                     EL141
00240          10  WS-PH-3                 PIC 9999.                    EL141
00241                                                                   EL141
00242      05  WS-PHONE-EDIT.                                           EL141
00243          10  WS-PH-ED-1              PIC XXX.                     EL141
00244          10  FILLER                  PIC X   VALUE '-'.           EL141
00245          10  WS-PH-ED-2              PIC XXX.                     EL141
00246          10  FILLER                  PIC X   VALUE '-'.           EL141
00247          10  WS-PH-ED-3              PIC XXXX.                    EL141
00248                                                                   EL141
00249      05  WS-CURR-DATE-BIN            PIC XX      VALUE LOW-VALUES.EL141
00250      05  WS-SAVE-CERT-NO             PIC X(10).                      CL*20
00251      05  WS-SAVE-SEQUENCE-NO         PIC S9(4) COMP.                 CL*20
00252      05  WS-SAVE-TRLR-KEY            PIC X(22).                      CL*20
00253      05  WS-SAVE-MSTR-KEY            PIC X(20).                      CL*20
00254      05  WS-START-MSTR-KEY           PIC X(20).                      CL*20
00255      05  WS-SAVE-TRLR-SEQ            PIC S9(1).                      CL*20
00256      05  WS-BEGIN-CNT                PIC S9(2).                      CL*20
00257      05  WS-INCREMENT-NO             PIC 99.                         CL*20
00258      05  WS-WORK-SEQ                 PIC 99.                         CL*20
00259      05  WS-NEW-SEQ                  PIC 99.                         CL*20
00260                                                                      CL*20
00261      05  WS-REWRITE-MSTR-SW          PIC X  VALUE 'N'.               CL*20
00262          88  REWRITE-MSTR                   VALUE 'Y'.               CL*20
00263                                                                      CL*20
00264      05  WS-TRAILER-SW               PIC X  VALUE 'N'.               CL*20
00265          88  TRLR-FOUND                     VALUE 'Y'.               CL*20
00266          88  TRLR-NOT-FOUND                 VALUE 'Y'.               CL*20
00267                                                                   EL141
00268      05  WS-MAINT-FUNC-SW            PIC X  VALUE SPACE.          EL141
00269          88  VALID-FUNCTION-ENTERED  VALUES ARE 'A' 'S' 'C' 'D'.  EL141
00270          88  CHANGE-REQUIRED         VALUE 'C'.                   EL141
00271          88  DELETE-REQUIRED         VALUE 'D'.                   EL141
00272          88  ADD-REQUIRED            VALUE 'A'.                   EL141
00273          88  LOOKUP-REQUIRED         VALUE 'S'.                   EL141
00274                                                                   EL141
00275      05  WS-ADDR-TYPE-SW.                                            CL**4
00276          10  WS-ADDR-TYPE            PIC X VALUE SPACES.             CL**9
00277              88  VALID-TYPE-ENTERED                                  CL**4
00278                   VALUES ARE 'I' 'B' 'A' 'P' 'E' 'O' 'Q'.            CL**4
00279              88  BENIF-ADDRESS                                       CL**4
00280                       VALUE      'B'.                                CL**4
00281              88  ACCT-ADDRESS                                        CL**4
00282                       VALUE      'A'.                                CL**4
00283          10  WS-ADDR-SEQ             PIC X VALUE SPACES.             CL**9
00284          10  WS-ADDR-SEQ-NUM REDEFINES                               CL**4
00285              WS-ADDR-SEQ             PIC S9.                         CL**9
00286                                                                      CL*20
00287      05  WS-ADDR-TRAILER-CNT.                                        CL*20
00288          10  WS-ADDR-I-CNT          PIC S9.                          CL*20
00289          10  WS-ADDR-A-CNT          PIC S9.                          CL*20
00290          10  WS-ADDR-B-CNT          PIC S9.                          CL*20
00291          10  WS-ADDR-E-CNT          PIC S9.                          CL*20
00292          10  WS-ADDR-P-CNT          PIC S9.                          CL*20
00293          10  WS-ADDR-O-CNT          PIC S9.                          CL*20
00294          10  WS-ADDR-Q-CNT          PIC S9.                          CL*20
00295                                                                   EL141
00296      05  WS-ADDR-SW                  PIC X VALUE SPACES.             CL**9
00297      05  WS-INPUT-ERROR-SW           PIC X  VALUE SPACE.          EL141
00298          88  ZIP-ERROR               VALUE 'Z'.                   EL141
00299          88  PHON-ERROR              VALUE 'P'.                   EL141
00300          88  NODATA-ERROR            VALUE 'N'.                   EL141
00301                                                                   EL141
00302      05  WS-TRAILER-SW               PIC X  VALUE SPACE.          EL141
00303          88  NO-TRAILER              VALUE 'N'.                      CL*17
00304                                                                   EL141
00305 ******************************************************************EL141
00306 ***  DATE AND TIME WORK AREAS                                     EL141
00307 ******************************************************************EL141
00308      05  TIME-IN                     PIC 9(7).                    EL141
00309      05  WS-TIME  REDEFINES TIME-IN.                              EL141
00310          10  FILLER                  PIC 9.                       EL141
00311          10  WS-HOUR                 PIC 99.                      EL141
00312          10  WS-MINUTE               PIC 99.                      EL141
00313          10  FILLER                  PIC 99.                      EL141
00314                                                                   EL141
00315      05  TIME-OUT.                                                EL141
00316          10  WS-TRANS-HOUR           PIC XX  VALUE SPACE.         EL141
00317          10  FILLER                  PIC X   VALUE '.'.           EL141
00318          10  WS-TRANS-MINUTE         PIC XX  VALUE SPACE.         EL141
00319                                                                   EL141
00320      EJECT                                                           CL*19
00321 ***  KEY TO DATASETS                                              EL141
00322 ******************************************************************EL141
00323                                                                      CL**7
00324      05  ERCOMP-KEY.                                                 CL*17
00325          10  ERCOMP-COMPANY-CD           PIC X.                      CL*17
00326          10  ERCOMP-CARRIER              PIC X.                      CL*17
00327          10  ERCOMP-GROUPING             PIC X(6).                   CL*17
00328          10  ERCOMP-RESP-NO              PIC X(10).                  CL*17
00329          10  ERCOMP-ACCOUNT              PIC X(10).                  CL*17
00330          10  ERCOMP-TYPE                 PIC X.                      CL*17
00331                                                                      CL**7
00332      05  ELTRLR-KEY.                                                 CL*17
00333          10  ELMSTR-KEY.                                             CL*17
00334              15  ELTRLR-COMPANY-CD       PIC X.                      CL*17
00335              15  ELTRLR-CARRIER          PIC X.                      CL*17
00336              15  ELTRLR-CLAIM-NO         PIC X(7).                   CL*17
00337              15  ELTRLR-CERT-NO.                                     CL*17
00338                  20  ELTRLR-CERT-PRIME   PIC X(10).                  CL*17
00339                  20  ELTRLR-CERT-SFX     PIC X.                      CL*17
00340          10  ELTRLR-SEQUENCE-NO      PIC   S9(4) COMP.               CL*17
00341                                                                   EL141
00342      05  ERACCT-KEY.                                                 CL*17
00343        07  ERACCT-PARTIAL-KEY.                                       CL*17
00344          10  ERACCT-COMPANY-CD           PIC X.                      CL*17
00345          10  ERACCT-CARRIER              PIC X.                      CL*17
00346          10  ERACCT-GROUPING             PIC X(6).                   CL*17
00347          10  ERACCT-STATE                PIC XX.                     CL*17
00348          10  ERACCT-ACCOUNT              PIC X(10).                  CL*17
00349        07  ERACCT-EFF-DT                 PIC XX.                     CL*17
00350        07  FILLER                        PIC S9(9) VALUE +0 COMP. EL141
00351      05  ERACCT-KEY-SAVE                PIC X(20).                   CL*17
00352      05  WS-HOLD-ERACCT-RECORD          PIC X(2000).                 CL*17
00353                                                                   EL141
00354      05  EMPROD-KEY.                                                 CL*18
00355        07  EMPROD-PARTIAL-KEY.                                       CL*18
00356          10  EMPROD-COMPANY-CD           PIC X(01).                  CL*18
00357          10  EMPROD-CARRIER              PIC X(01).                  CL*18
00358          10  EMPROD-GROUPING             PIC X(06).                  CL*18
00359          10  EMPROD-STATE                PIC X(02).                  CL*18
00360          10  EMPROD-PRODUCER             PIC X(10).                  CL*18
00361        07  EMPROD-EXP-DT                 PIC X(02).                  CL*18
00362                                                                      CL*18
00363      05  ELBENE-KEY.                                                 CL*17
00364          10  ELBENE-COMPANY-CD    PIC X.                             CL*17
00365          10  ELBENE-RECORD-TYPE  PIC X.                              CL*17
00366          10  ELBENE-BENEFICIARY  PIC X(10).                          CL*17
00367      EJECT                                                        EL141
00368 ******************************************************************EL141
00369 *** STANDARD COPY AREAS                                           EL141
00370 ******************************************************************EL141
00371      COPY ELCAID.                                                    CL*14
00372  01  PF-AID REDEFINES DFHAID.                                     EL141
00373      05  FILLER                      PIC X(8).                    EL141
00374      05  PF-VALUES  OCCURS 24        PIC X.                       EL141
00375      EJECT                                                        EL141
00376      COPY ELCINTF.                                                   CL*14
00377      12  PI-REDEFINES REDEFINES PI-PROGRAM-WORK-AREA.                CL*17
00378          16  FILLER                  PIC X(571).                     CL*17
00379          16  PI-MAINT                PIC X(01).                      CL*17
00380          16  PI-AIG-ADD-SW           PIC X(01).                      CL*17
00381          16  PI-LETTER-ERROR-CODE    PIC 9(04).                      CL*17
00382          16  PI-PREV-MAINT-CODE      PIC X(01).                      CL*19
00383          16  PI-PREV-ADDR-TYPE       PIC X(02).                      CL*19
061511*         16  PI-FILLER               PIC X(60).                      CL*20
061511         16  PI-ST-VFY-2ND-BENE      PIC X.
061511         16  PI-APPROVAL-LEVEL       PIC X.
061511         16  PI-FILLER               PIC X(58).
00385                                                                      CL*17
00386      EJECT                                                        EL141
00387      COPY ELCATTR.                                                   CL*14
00388      EJECT                                                        EL141
00389      COPY ELCDATE.                                                   CL*14
00390      EJECT                                                        EL141
00391      COPY ELCEMIB.                                                   CL*14
00392      EJECT                                                        EL141
00393      COPY ELCLOGOF.                                                  CL*14
00394      EJECT                                                        EL141
00395      COPY EL141S.                                                    CL*20
00396      EJECT                                                        EL141
00397  LINKAGE SECTION.                                                 EL141
00398  01  DFHCOMMAREA                     PICTURE X(1024).             EL141
00399      EJECT                                                        EL141
00400      COPY ELCMSTR.                                                   CL*14
00401      EJECT                                                        EL141
00402      COPY ELCTRLR.                                                   CL*14
00403      EJECT                                                        EL141
00404      COPY ERCACCT.                                                   CL*14
00405      EJECT                                                        EL141
00406      COPY ELCBENE.                                                   CL*14

00408      COPY ERCCOMP.                                                   CL*14

00410      COPY MPCPROD.                                                   CL*18

                                       COPY ELCCNTL.

00412  PROCEDURE DIVISION.                                              EL141
00413                                                                   EL141
00414      MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.            CL*17
00415      MOVE '5'                        TO  DC-OPTION-CODE.             CL*17
00416      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT.                    CL*17
00417      MOVE DC-GREG-DATE-1-EDIT        TO  SAVE-DATE.                  CL*17
00418      MOVE DC-BIN-DATE-1              TO  SAVE-BIN-DATE.              CL*17
00419      MOVE +2                         TO  EMI-NUMBER-OF-LINES.        CL*17
00420      MOVE ZEROS                      TO  NDX.                        CL*20
00421                                                                   EL141
00422  0001-PROCESSING-EXITS.                                           EL141
00423      MOVE DFHCOMMAREA                TO  PROGRAM-INTERFACE-BLOCK.    CL*17
00424                                                                   EL141
00425      IF EIBCALEN NOT GREATER THAN ZEROS                           EL141
00426          GO TO 9000-UNAUTHERR.                                       CL*17
00427                                                                   EL141
00428      EXEC CICS  HANDLE CONDITION                                  EL141
00429          ERROR      (9990-ERROR)                                     CL*17
00430          PGMIDERR   (9600-PGMIDERR)                                  CL*17
00431      END-EXEC.                                                    EL141
00432                                                                   EL141
00433      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL141
00434          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL141
00435              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL141
00436              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL141
00437              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL141
00438              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL141
00439              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL141
00440              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL141
00441              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL141
00442              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL141
00443          ELSE                                                     EL141
00444              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL141
00445              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL141
00446              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL141
00447              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL141
00448              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL141
00449              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL141
00450              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL141
00451              MOVE SPACES               TO PI-SAVED-PROGRAM-6.        CL*17
00452                                                                   EL141
00453      IF EIBTRNID EQUAL WS-TRANS-ID                                   CL*17
00454          IF EIBAID EQUAL DFHCLEAR                                    CL*17
00455              GO TO 9400-CLEAR                                        CL*17
00456          ELSE                                                        CL*17
00457              GO TO 0200-RECEIVE.                                     CL*17
00458                                                                      CL*10
00459  0100-FIRST-TIME-IN.                                                 CL*17
00460 ******************************************************************   CL*17
00461 *    ON FIRST ENTRY INTO THE PROGRAM, THE CLAIM RECORD IS READ   *   CL*17
00462 *    AND THE CURRENT ADDRESS RECORDS THAT EXIST FOR THE CLAIM    *   CL*17
00463 *    ARE HIGHLIGHTED AND THE NUMBER OF ADDRESS RECORDS THAT      *   CL*17
00464 *    EXIST FOR EACH ADDRESS TYPE ARE DISPLAYED.                  *   CL*17
00465 ******************************************************************   CL*17
00466                                                                      CL*17
00467      MOVE LOW-VALUES                 TO  EL141AO.                    CL*17
00468      MOVE -1                         TO  MFMAINTL.                   CL*17
00469                                                                      CL*19
00470      MOVE SPACES                     TO  PI-PREV-MAINT-CODE          CL*19
00471                                          PI-PREV-ADDR-TYPE.          CL*19
00472                                                                      CL*17
00473      MOVE '141A'                     TO  PI-CURRENT-SCREEN-NO.       CL*17
00474      MOVE SAVE-BIN-DATE              TO  WS-CURR-DATE-BIN.           CL*17
00475                                                                      CL*17
00476      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.          CL*17
00477      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.             CL*17
00478      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.            CL*17
00479      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.             CL*17
00480                                                                      CL*17
00481      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')       CL*18
00482          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')    CL*18
00483              IF PI-MAINT IS EQUAL TO 'A'                             CL*17
00484                  MOVE 'S'            TO  MFMAINTI                    CL*17
00485                                          WS-MAINT-FUNC-SW            CL*17
00486                  MOVE 'I1'           TO  MADDRTPI                    CL*17
00487                                          WS-ADDR-TYPE-SW             CL*17
00488                  GO TO 1000-SHOW-ADDRESS.                            CL*17
00489                                                                      CL*17
00490      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.                        CL*17
061511     PERFORM 6700-GET-VFY-BENE-IND THRU 6700-EXIT.
061511     PERFORM 6750-GET-APPROVAL-LEVEL THRU 6750-EXIT.
00491      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
00492                                                                      CL*17
00493      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
00494                                                                      CL*17
00495      EJECT                                                           CL*17
00496  0200-RECEIVE.                                                       CL*17
00497                                                                      CL*17
00498      MOVE LOW-VALUES                 TO  EL141AI.                    CL*17
00499                                                                   EL141
00500      IF PI-PROCESSOR-ID = 'LGXX'                                  EL141
00501          NEXT SENTENCE                                            EL141
00502      ELSE                                                         EL141
00503          EXEC CICS READQ TS                                       EL141
00504              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL141
00505              INTO    (SECURITY-CONTROL)                           EL141
00506              LENGTH  (SC-COMM-LENGTH)                             EL141
00507              ITEM    (SC-ITEM)                                    EL141
00508          END-EXEC                                                 EL141
00509          MOVE SC-CLAIMS-DISPLAY (14) TO  PI-DISPLAY-CAP           EL141
00510          MOVE SC-CLAIMS-UPDATE  (14) TO  PI-MODIFY-CAP            EL141
00511          IF NOT DISPLAY-CAP                                       EL141
00512              MOVE 'READ'             TO  SM-READ                  EL141
00513              PERFORM 9995-SECURITY-VIOLATION                      EL141
00514              MOVE ER-0070            TO  EMI-ERROR                EL141
00515              MOVE -1                 TO  MFMAINTL                    CL*17
00516              MOVE AL-UABON           TO  MFMAINTA                    CL*17
00517              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*17
00518              GO TO 8100-SEND-INITIAL-MAP.                            CL*17
00519                                                                   EL141
00520      IF EIBAID IS EQUAL TO DFHPA1 OR DFHPA2 OR DFHPA3                CL*17
00521          MOVE LOW-VALUES             TO  EL141AO                     CL*17
00522          MOVE ER-0008                TO  EMI-ERROR                   CL*17
00523          MOVE -1                     TO  MPFNUMBL                    CL*17
00524          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*17
00525          GO TO 8200-SEND-DATAONLY.                                   CL*17
00526                                                                   EL141
00527      EXEC CICS RECEIVE                                               CL*17
00528          MAP      (WS-MAP-NAME)                                      CL*17
00529          MAPSET   (WS-MAPSET-NAME)                                   CL*17
00530          INTO     (EL141AI)                                          CL*17
00531      END-EXEC.                                                       CL*17
00532                                                                   EL141
00533      IF MPFNUMBL IS EQUAL TO +0                                      CL*17
00534          GO TO 0300-CHECK-PFKEYS.                                    CL*17
00535                                                                      CL*17
00536      IF EIBAID IS NOT EQUAL TO DFHENTER                              CL*17
00537          MOVE ER-0004                TO  EMI-ERROR                   CL*17
00538          GO TO 0320-INPUT-ERROR.                                     CL*17
00539                                                                      CL*17
00540      IF (MPFNUMBO NUMERIC) AND (MPFNUMBO GREATER 0 AND LESS 25)      CL*17
00541          MOVE PF-VALUES (MPFNUMBI)   TO  EIBAID                      CL*17
00542      ELSE                                                            CL*17
00543          MOVE ER-0029                TO  EMI-ERROR                   CL*17
00544          GO TO 0320-INPUT-ERROR.                                     CL*17
00545                                                                      CL*17
00546  0300-CHECK-PFKEYS.                                                  CL*17
00547                                                                      CL*17
00548      IF EIBAID IS EQUAL TO DFHPF12                                   CL*17
00549          MOVE XCTL-010               TO  PGM-NAME                    CL*17
00550          GO TO 9300-XCTL.                                            CL*17
00551                                                                      CL*17
00552      IF EIBAID IS EQUAL TO DFHPF23                                   CL*17
00553          MOVE EIBAID                 TO  PI-ENTRY-CD-1               CL*17
00554          MOVE XCTL-005               TO  PGM-NAME                    CL*17
00555          GO TO 9300-XCTL.                                            CL*17
00556                                                                      CL*17
00557      IF EIBAID IS EQUAL TO DFHPF24                                   CL*17
00558          MOVE XCTL-126               TO  PGM-NAME                    CL*17
00559          GO TO 9300-XCTL.                                            CL*17
00560                                                                      CL*17
00561      IF EIBAID IS EQUAL TO DFHPF1                                    CL*17
00562          MOVE XCTL-114               TO  PGM-NAME                    CL*17
00563          GO TO 9300-XCTL.                                            CL*17
00564                                                                      CL*17
00565      IF EIBAID IS EQUAL TO DFHPF2                                    CL*20
00566         GO TO  0330-EDIT-DATA.                                       CL*20
00567                                                                      CL*20
00568      IF EIBAID IS EQUAL TO DFHENTER                                  CL*17
00569          GO TO 0330-EDIT-DATA.                                       CL*17
00570                                                                      CL*17
00571      MOVE ER-0029                    TO  EMI-ERROR.                  CL*17
00572                                                                      CL*17
00573  0320-INPUT-ERROR.                                                   CL*17
00574                                                                      CL*17
00575      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
00576      MOVE -1                         TO  MPFNUMBL.                   CL*17
00577                                                                      CL*17
00578      GO TO 8200-SEND-DATAONLY.                                       CL*17
00579                                                                      CL*17
00580  0330-EDIT-DATA.                                                     CL*17
00581                                                                      CL*17
00582       MOVE SPACES                    TO  MERMSG1O                    CL*17
00583                                          MAPNOT1O                    CL*17
00584                                          MAPNOT2O.                   CL*17
00585                                                                      CL*17
00586      MOVE  MFMAINTI                  TO  WS-MAINT-FUNC-SW.           CL*17
00587                                                                      CL*19
00588      MOVE  MADDRTPI                  TO  WS-ADDR-TYPE-SW.            CL*17
00589                                                                   EL141
00590      IF VALID-FUNCTION-ENTERED                                    EL141
00591          NEXT SENTENCE                                            EL141
00592      ELSE                                                         EL141
00593          MOVE ER-0023                TO  EMI-ERROR                   CL*17
00594          MOVE -1                     TO  MFMAINTL                    CL*17
00595          MOVE AL-UABON               TO  MFMAINTA                    CL*17
00596          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*17
00597          GO TO 8200-SEND-DATAONLY.                                   CL*17
00598                                                                   EL141
00599      IF VALID-TYPE-ENTERED                                        EL141
00600          NEXT SENTENCE                                            EL141
00601      ELSE                                                         EL141
00602          MOVE ER-0136                TO  EMI-ERROR                   CL*17
00603          MOVE -1                     TO  MADDRTPL                    CL*17
00604          MOVE AL-UABON               TO  MADDRTPA                    CL*17
00605          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*17
00606          GO TO 8200-SEND-DATAONLY.                                   CL*17
00607                                                                   EL141
00608      IF (WS-ADDR-SEQ NUMERIC)                                        CL**4
00609        AND                                                           CL**4
00610         (WS-ADDR-SEQ NOT LESS THAN '0' AND                           CL**4
00611                      NOT GREATER THAN '9')                           CL**4
00612         NEXT SENTENCE                                                CL**4
00613      ELSE                                                            CL**4
00614         MOVE ER-0136                 TO  EMI-ERROR                   CL*17
00615         MOVE -1                      TO  MADDRTPL                    CL*17
00616         MOVE AL-UABON                TO  MADDRTPA                    CL*17
00617         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL*17
00618         GO TO 8200-SEND-DATAONLY.                                    CL*17
00619                                                                      CL**4
00620      IF WS-ADDR-SEQ EQUAL '0'                                        CL**4
00621         IF WS-ADDR-TYPE EQUAL 'A' OR 'B'                             CL**4
00622             NEXT SENTENCE                                            CL*17
00623         ELSE                                                         CL**4
00624             MOVE ER-0136                 TO  EMI-ERROR               CL*17
00625             MOVE -1                      TO  MADDRTPL                CL*17
00626             MOVE AL-UABON                TO  MADDRTPA                CL*17
00627             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL*17
00628             GO TO 8200-SEND-DATAONLY.                                CL*17
00629                                                                      CL**4
00630  0330-EDIT-CONTINUE.                                                 CL*20
00631      IF LOOKUP-REQUIRED                                              CL*20
00632          NEXT SENTENCE                                               CL*20
00633      ELSE                                                            CL*20
00634          IF (WS-ADDR-TYPE EQUAL 'A' ) AND                            CL*20
00635             (WS-ADDR-SEQ EQUAL '0' OR '9')                           CL*20
00636              MOVE ER-0277       TO  EMI-ERROR                        CL*20
00637              MOVE -1            TO  MFMAINTL                         CL*20
00638              MOVE AL-UABON      TO  MFMAINTA                         CL*20
00639              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*20
00640              GO TO 8200-SEND-DATAONLY                                CL*20
00641          ELSE                                                        CL*20
00642             IF (WS-ADDR-TYPE EQUAL 'B' ) AND                         CL*20
00643                (WS-ADDR-SEQ EQUAL '0' OR '9')                        CL*20
00644                  MOVE ER-0277   TO  EMI-ERROR                        CL*20
00645                  MOVE -1        TO  MFMAINTL                         CL*20
00646                  MOVE AL-UABON  TO  MFMAINTA                         CL*20
00647                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*20
00648                  GO TO 8200-SEND-DATAONLY.                           CL*20
00649                                                                      CL*20
00650      IF NOT MODIFY-CAP                                            EL141
00651         IF LOOKUP-REQUIRED                                           CL**4
00652            NEXT SENTENCE                                             CL**4
00653         ELSE                                                         CL**4
00654            MOVE 'UPDATE'                 TO  SM-READ                 CL*17
00655            PERFORM 9995-SECURITY-VIOLATION                           CL**4
00656            MOVE ER-0070                  TO  EMI-ERROR               CL*17
00657            MOVE -1                       TO  MFMAINTL                CL*17
00658            MOVE AL-UABON                 TO  MFMAINTA                CL*17
00659            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  CL*17
00660            GO TO 8100-SEND-INITIAL-MAP.                              CL*17
00661                                                                      CL*20
00662      IF EIBAID IS EQUAL TO DFHPF2                                    CL*20
00663          GO TO 5000-UPDATE-VIA-PF2.                                  CL*20
00664                                                                   EL141
00665      IF CHANGE-REQUIRED                                           EL141
00666          GO TO 2000-CHANGE-ADDRESS.                                  CL*17
00667                                                                      CL**7
00668      IF ADD-REQUIRED                                                 CL**7
00669          GO TO 3000-ADD-ADDRESS.                                     CL*17
00670                                                                      CL**7
00671      IF DELETE-REQUIRED                                              CL**7
00672         GO TO 4000-DELETE-ADDRESS.                                   CL*17
00673                                                                      CL**7
00674      EJECT                                                           CL*19
00675  1000-SHOW-ADDRESS.                                                  CL*17
00676                                                                      CL*17
00677      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.          CL*17
00678      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.             CL*17
00679      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.            CL*17
00680      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.             CL*17
00681                                                                      CL*17
00682      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.                        CL*17
00683                                                                      CL**7
00684      IF WS-ADDR-TYPE EQUAL 'B'                                       CL**7
00685         IF WS-ADDR-SEQ EQUAL '0' OR '9'                              CL*20
00686            GO TO 1100-READ-BENEFICIARY-RECORD.                       CL*17
00687                                                                      CL**7
00688      IF WS-ADDR-TYPE EQUAL 'A'                                       CL**7
00689         IF WS-ADDR-SEQ EQUAL '0'                                     CL**7
00690             IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                 CL*18
00691                 GO TO 1250-READ-PRODUCER                             CL*18
00692             ELSE                                                     CL*18
00693                 GO TO 1200-READ-ACCT.                                CL*18
00694                                                                      CL**7
00695      IF WS-ADDR-TYPE EQUAL 'I'                                       CL**7
00696         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
00697      ELSE                                                            CL**7
00698      IF WS-ADDR-TYPE EQUAL 'B'                                       CL**7
00699         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
00700         ADD +10                      TO  ELTRLR-SEQUENCE-NO          CL*17
00701      ELSE                                                            CL**7
00702      IF WS-ADDR-TYPE EQUAL 'A'                                       CL**7
00703         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
00704         ADD +20                      TO  ELTRLR-SEQUENCE-NO          CL*17
00705      ELSE                                                            CL**7
00706      IF WS-ADDR-TYPE EQUAL 'P'                                       CL**7
00707         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
00708         ADD +30                      TO  ELTRLR-SEQUENCE-NO          CL*17
00709      ELSE                                                            CL**7
00710      IF WS-ADDR-TYPE EQUAL 'E'                                       CL**7
00711         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
00712         ADD +40                      TO  ELTRLR-SEQUENCE-NO          CL*17
00713      ELSE                                                            CL**7
00714      IF WS-ADDR-TYPE EQUAL 'O'                                       CL**7
00715         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
00716         ADD +50                      TO  ELTRLR-SEQUENCE-NO          CL*17
00717      ELSE                                                            CL**7
00718      IF WS-ADDR-TYPE EQUAL 'Q'                                       CL**7
00719         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
00720         ADD +60                      TO  ELTRLR-SEQUENCE-NO.         CL*17
00721                                                                      CL**7
00722      EXEC CICS HANDLE CONDITION                                      CL**7
00723          NOTFND   (1010-NOTFND-ELTRLR)                               CL*17
00724          NOTOPEN  (1020-NOTOPEN-ELTRLR)                              CL*17
00725      END-EXEC.                                                       CL**7
00726                                                                      CL**7
00727      PERFORM 7050-READ-ELTRLR THRU 7050-EXIT.                        CL*17
00728                                                                      CL**7
00729      MOVE 'T'                        TO  WS-ADDR-SW.                 CL*17
00730      GO TO 1500-CONTINUE-SHOW.                                       CL*17
00731                                                                      CL**7
00732  1010-NOTFND-ELTRLR.                                                 CL*17
00733                                                                      CL**7
00734      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')       CL*18
00735          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')    CL*18
00736              IF PI-MAINT IS EQUAL TO 'A'                             CL*17
00737                  MOVE ER-0265        TO  EMI-ERROR                   CL*17
00738                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*17
00739                  MOVE PI-CLAIM-NO    TO  EMI-TEXT-VARIABLE (1).      CL*17
00740                                                                      CL**7
00741      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')       CL*18
00742          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')    CL*18
00743              IF PI-MAINT IS EQUAL TO 'A'                             CL*17
00744                  MOVE 'A'            TO  MFMAINTI                    CL*17
00745                                          WS-MAINT-FUNC-SW            CL*17
00746                  MOVE 'S'            TO  PI-MAINT                    CL*17
00747                  MOVE ER-0799        TO  EMI-ERROR                   CL*17
00748                  MOVE -1             TO  MFMAINTL                    CL*17
00749                  MOVE AL-UABON       TO  MFMAINTA                    CL*17
00750                                          MADDRTPA                    CL*17
00751                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*17
00752                  GO TO 8100-SEND-INITIAL-MAP.                        CL*17
00753                                                                      CL**7
00754      IF WS-ADDR-TYPE IS EQUAL TO 'A'                                 CL*17
00755          IF WS-ADDR-SEQ IS EQUAL TO '9'                              CL*17
00756              IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                CL*18
00757                  GO TO 1250-READ-PRODUCER                            CL*18
00758              ELSE                                                    CL*18
00759                  GO TO 1200-READ-ACCT.                               CL*18
00760                                                                      CL**7
00761      MOVE LOW-VALUES                 TO  EL141AO.                    CL*17
00762      MOVE ER-0135                    TO  EMI-ERROR.                  CL*17
00763      MOVE -1                         TO  MADDRTPL.                   CL*17
00764      MOVE AL-UABON                   TO  MADDRTPA                    CL*17
00765                                          MFMAINTA.                   CL*17
00766      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.                   CL*17
00767      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.                   CL*17
00768      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
00769      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
00770      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
00771                                                                      CL**7
00772  1020-NOTOPEN-ELTRLR.                                                CL*17
00773                                                                      CL*17
00774      MOVE LOW-VALUES                 TO  EL141AI.                    CL*17
00775      MOVE ER-0172                    TO  EMI-ERROR.                  CL*17
00776      MOVE -1                         TO  MFMAINTL.                   CL*17
00777      MOVE AL-UABON                   TO  MFMAINTA.                   CL*17
00778      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.                   CL*17
00779      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
00780      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
00781      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
00782                                                                      CL*17
00783      EJECT                                                           CL*17
00784  1100-READ-BENEFICIARY-RECORD.                                       CL*17
00785                                                                      CL**7
00786      EXEC CICS HANDLE CONDITION                                      CL**7
00787          NOTFND    (1110-NO-BENE)                                    CL*17
00788          NOTOPEN   (1120-BENE-NOTOPEN)                               CL*17
00789      END-EXEC.                                                       CL**7
00790                                                                      CL**7
00791      MOVE PI-COMPANY-CD              TO  ELBENE-COMPANY-CD.          CL*17
00792      MOVE 'B'                        TO  ELBENE-RECORD-TYPE.         CL*17
00793      MOVE CL-BENEFICIARY             TO  ELBENE-BENEFICIARY.         CL*17
00794                                                                      CL**7
00795      EXEC CICS READ                                                  CL**7
00796          DATASET (ELBENE-FILE-ID)                                    CL*17
00797          SET     (ADDRESS OF BENEFICIARY-MASTER)                     CL*20
00798          RIDFLD  (ELBENE-KEY)                                        CL*17
00799      END-EXEC.                                                       CL**7
00800                                                                      CL**7
00801      MOVE 'B'                        TO  WS-ADDR-SW.                 CL*17
00802      GO TO 1500-CONTINUE-SHOW.                                       CL*17
00803                                                                      CL**7
00804  1110-NO-BENE.                                                       CL*17
00805                                                                      CL**7
00806      MOVE LOW-VALUES                 TO  EL141AO.                    CL*17
00807      MOVE ER-2599                    TO  EMI-ERROR.                  CL*17
00808      MOVE -1                         TO  MADDRTPL.                   CL*17
00809      MOVE AL-UABON                   TO  MADDRTPA.                   CL*17
00810      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.                   CL*17
00811      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
00812      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
00813      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
00814                                                                      CL*17
00815  1120-BENE-NOTOPEN.                                                  CL*17
00816      MOVE LOW-VALUES                 TO  EL141AO.                    CL*17
00817      MOVE ER-7675                    TO  EMI-ERROR.                  CL*17
00818      MOVE -1                         TO  MFMAINTL.                   CL*17
00819      MOVE AL-UABON                   TO  MFMAINTA.                   CL*17
00820      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.                   CL*17
00821      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
00822      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
00823      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
00824                                                                      CL**7
00825      EJECT                                                           CL**7
00826  1200-READ-ACCT.                                                     CL*17
00827 ******************************************************************   CL*17
00828 *    READ THE ACCOUNT MASTER RECORD IF THE ADDRESS TYPE          *   CL*17
00829 *    SELECTED IS A0 (ZERO).                                      *   CL*17
00830 ******************************************************************   CL*17
00831                                                                      CL**7
00832      MOVE PI-COMPANY-CD              TO  ERACCT-COMPANY-CD.          CL*17
00833      MOVE PI-CARRIER                 TO  ERACCT-CARRIER.             CL*17
00834      MOVE CL-CERT-GROUPING           TO  ERACCT-GROUPING.            CL*17
00835      MOVE CL-CERT-STATE              TO  ERACCT-STATE.               CL*17
00836      MOVE CL-CERT-ACCOUNT            TO  ERACCT-ACCOUNT.             CL*17
00837      MOVE CL-CERT-EFF-DT             TO  ERACCT-EFF-DT.              CL*17
00838      MOVE SPACES                     TO  WS-HOLD-ERACCT-RECORD.      CL*17
00839                                                                      CL**7
00840      EXEC CICS  HANDLE CONDITION                                     CL**7
00841          NOTFND  (1230-NO-ACCT)                                      CL*17
00842          NOTOPEN (1240-ACCT-NOTOPEN)                                 CL*17
00843      END-EXEC.                                                       CL**7
00844                                                                      CL**7
00845      EXEC CICS  STARTBR                                              CL**7
00846          RIDFLD   (ERACCT-KEY)                                       CL*17
00847          DATASET  (ERACCT-FILE-ID)                                   CL*17
00848          GTEQ                                                        CL*17
00849      END-EXEC.                                                       CL**7
00850                                                                      CL**7
00851      MOVE ERACCT-PARTIAL-KEY         TO  ERACCT-KEY-SAVE.            CL*17
00852                                                                      CL**7
00853  1210-READNEXT-ACCT.                                                 CL*17
00854                                                                      CL**7
00855      EXEC CICS  HANDLE CONDITION                                     CL**7
00856          ENDFILE (1230-NO-ACCT)                                      CL*17
00857          NOTFND  (1210-READNEXT-ACCT)                                CL*17
00858      END-EXEC.                                                       CL**7
00859                                                                      CL**7
00860      EXEC CICS  READNEXT                                             CL**7
00861          SET      (ADDRESS OF ACCOUNT-MASTER)                        CL*20
00862          DATASET  (ERACCT-FILE-ID)                                   CL*17
00863          RIDFLD   (ERACCT-KEY)                                       CL*17
00864      END-EXEC.                                                       CL**7
00865                                                                      CL**7
00866      MOVE 'A'                        TO  WS-ADDR-SW.                 CL*17
00867                                                                      CL**7
00868      IF ERACCT-PARTIAL-KEY NOT = ERACCT-KEY-SAVE                     CL*17
00869          IF WS-HOLD-ERACCT-RECORD IS EQUAL TO SPACES                 CL*17
00870              GO TO 1230-NO-ACCT                                      CL*17
00871          ELSE                                                        CL*15
00872              MOVE WS-HOLD-ERACCT-RECORD  TO  ACCOUNT-MASTER          CL*17
00873              GO TO 1300-CHECK-COMP.                                  CL*17
00874                                                                      CL*15
00875      IF ERACCT-EFF-DT IS EQUAL TO HIGH-VALUES                        CL*17
00876          GO TO 1300-CHECK-COMP                                       CL*17
00877      ELSE                                                            CL**7
00878          MOVE ACCOUNT-MASTER         TO  WS-HOLD-ERACCT-RECORD       CL*17
00879          GO TO 1210-READNEXT-ACCT.                                   CL*17
00880                                                                      CL**7
00881  1230-NO-ACCT.                                                       CL*17
00882                                                                      CL**7
00883      MOVE LOW-VALUES                 TO  EL141AI.                    CL*17
00884      MOVE ER-0278                    TO  EMI-ERROR.                  CL*17
00885      MOVE -1                         TO  MADDRTPL.                   CL*17
00886      MOVE AL-UABON                   TO  MADDRTPA.                   CL*17
00887      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.                   CL*17
00888      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
00889      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
00890      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
00891                                                                      CL**7
00892  1240-ACCT-NOTOPEN.                                                  CL*17
00893                                                                      CL**7
00894      MOVE LOW-VALUES                 TO  EL141AI.                    CL*17
00895      MOVE ER-0168                    TO  EMI-ERROR.                  CL*17
00896      MOVE -1                         TO  MADDRTPL.                   CL*18
00897      MOVE AL-UABON                   TO  MADDRTPA.                   CL*18
00898      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.                   CL*18
00899      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*18
00900      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*18
00901      GO TO 8100-SEND-INITIAL-MAP.                                    CL*18
00902                                                                      CL*18
00903      EJECT                                                           CL*18
00904  1250-READ-PRODUCER.                                                 CL*18
00905 ******************************************************************   CL*18
00906 *    READ THE CONVENIENCE PRODUCER MASTER RECORD IF THE ADDRESS  *   CL*18
00907 *    TYPE SELECTED IS A0 (ZERO).                                 *   CL*18
00908 ******************************************************************   CL*18
00909                                                                      CL*18
00910      MOVE PI-COMPANY-CD              TO  EMPROD-COMPANY-CD.          CL*18
00911      MOVE PI-CARRIER                 TO  EMPROD-CARRIER.             CL*18
00912      MOVE CL-CERT-GROUPING           TO  EMPROD-GROUPING.            CL*18
00913      MOVE CL-CERT-STATE              TO  EMPROD-STATE.               CL*18
00914      MOVE CL-CERT-ACCOUNT            TO  EMPROD-PRODUCER.            CL*18
00915      MOVE CL-CERT-EFF-DT             TO  EMPROD-EXP-DT.              CL*18
00916      MOVE SPACES                     TO  WS-HOLD-ERACCT-RECORD.      CL*18
00917                                                                      CL*18
00918      EXEC CICS  HANDLE CONDITION                                     CL*18
00919          NOTFND  (1280-NO-PRODUCER)                                  CL*18
00920          NOTOPEN (1290-PRODUCER-NOTOPEN)                             CL*18
00921      END-EXEC.                                                       CL*18
00922                                                                      CL*18
00923      EXEC CICS  STARTBR                                              CL*18
00924          RIDFLD   (EMPROD-KEY)                                       CL*18
00925          DATASET  (EMPROD-FILE-ID)                                   CL*18
00926          GTEQ                                                        CL*18
00927      END-EXEC.                                                       CL*18
00928                                                                      CL*18
00929      MOVE EMPROD-PARTIAL-KEY         TO  ERACCT-KEY-SAVE.            CL*18
00930                                                                      CL*18
00931  1260-READNEXT-PRODUCER.                                             CL*18
00932                                                                      CL*18
00933      EXEC CICS  HANDLE CONDITION                                     CL*18
00934          ENDFILE (1280-NO-PRODUCER)                                  CL*18
00935          NOTFND  (1260-READNEXT-PRODUCER)                            CL*18
00936      END-EXEC.                                                       CL*18
00937                                                                      CL*18
00938      EXEC CICS  READNEXT                                             CL*18
00939          SET      (ADDRESS OF PRODUCER-MASTER)                       CL*20
00940          DATASET  (EMPROD-FILE-ID)                                   CL*18
00941          RIDFLD   (EMPROD-KEY)                                       CL*18
00942      END-EXEC.                                                       CL*18
00943                                                                      CL*18
00944      MOVE 'P'                        TO  WS-ADDR-SW.                 CL*18
00945                                                                      CL*18
00946      IF EMPROD-PARTIAL-KEY NOT = ERACCT-KEY-SAVE                     CL*18
00947          IF WS-HOLD-ERACCT-RECORD IS EQUAL TO SPACES                 CL*18
00948              GO TO 1230-NO-ACCT                                      CL*18
00949          ELSE                                                        CL*18
00950              MOVE WS-HOLD-ERACCT-RECORD  TO  PRODUCER-MASTER         CL*18
00951              GO TO 1500-CONTINUE-SHOW.                               CL*18
00952                                                                      CL*18
00953      IF EMPROD-EXP-DT IS EQUAL TO HIGH-VALUES                        CL*18
00954          GO TO 1500-CONTINUE-SHOW                                    CL*18
00955      ELSE                                                            CL*18
00956          MOVE PRODUCER-MASTER        TO  WS-HOLD-ERACCT-RECORD       CL*18
00957          GO TO 1260-READNEXT-PRODUCER.                               CL*18
00958                                                                      CL*18
00959  1280-NO-PRODUCER.                                                   CL*18
00960                                                                      CL*18
00961      MOVE LOW-VALUES                 TO  EL141AI.                    CL*18
00962      MOVE ER-0278                    TO  EMI-ERROR.                  CL*18
00963      MOVE -1                         TO  MADDRTPL.                   CL*18
00964      MOVE AL-UABON                   TO  MADDRTPA.                   CL*18
00965      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.                   CL*18
00966      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*18
00967      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*18
00968      GO TO 8100-SEND-INITIAL-MAP.                                    CL*18
00969                                                                      CL*18
00970  1290-PRODUCER-NOTOPEN.                                              CL*18
00971                                                                      CL*18
00972      MOVE LOW-VALUES                 TO  EL141AI.                    CL*18
00973      MOVE ER-9616                    TO  EMI-ERROR.                  CL*18
00974      MOVE -1                         TO  MADDRTPL.                   CL*17
00975      MOVE AL-UABON                   TO  MADDRTPA.                   CL*17
00976      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.                   CL*17
00977      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
00978      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
00979      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
00980                                                                      CL**7
00981  1300-CHECK-COMP.                                                    CL*17
00982 ******************************************************************   CL*17
00983 *    READ THE COMPENSATION MASTER IF THE ADDRESS TYPE SELECTED   *   CL*17
00984 *    IS A9 AND NO ACTIVITY TRAILER (SEQ # 29) EXISTS.            *   CL*17
00985 ******************************************************************   CL*17
00986                                                                      CL**7
00987      IF WS-ADDR-TYPE EQUAL 'A'                                       CL**7
00988        AND                                                           CL**7
00989         WS-ADDR-SEQ EQUAL '0'                                        CL**7
00990         GO TO 1500-CONTINUE-SHOW.                                    CL*17
00991                                                                      CL**7
00992      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                   CL*17
00993          IF WS-HOLD-ERACCT-RECORD IS EQUAL TO SPACES                 CL*17
00994              GO TO 1230-NO-ACCT                                      CL*17
00995          ELSE                                                        CL*17
00996              MOVE WS-HOLD-ERACCT-RECORD  TO  ACCOUNT-MASTER          CL*17
00997              GO TO 1500-CONTINUE-SHOW.                               CL*17
00998                                                                      CL*17
00999  1310-READ-COMP.                                                     CL*17
01000                                                                      CL**7
01001      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC                         CL**7
01002          MOVE ZEROS                  TO  AM-3RD-PARTY-NOTIF-LEVEL.   CL*17
01003                                                                      CL**7
01004      IF AM-3RD-PARTY-NOTIF-LEVEL EQUAL ZEROS                         CL**7
01005          MOVE LOW-VALUES             TO  EL141AO                     CL*17
01006          MOVE ER-0135                TO  EMI-ERROR                   CL*17
01007          MOVE -1                     TO  MADDRTPL                    CL*17
01008          MOVE AL-UABON               TO  MADDRTPA                    CL*17
01009          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*17
01010          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT              CL*17
01011          GO TO 8100-SEND-INITIAL-MAP.                                CL*17
01012                                                                      CL**7
01013      IF AM-3RD-PARTY-NOTIF-LEVEL EQUAL ZEROS                         CL**7
01014         MOVE 01                      TO  AM-3RD-PARTY-NOTIF-LEVEL.   CL*17
01015                                                                      CL**7
01016      IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) EQUAL SPACES OR ZEROS      CL**7
01017          MOVE ER-0135                TO  EMI-ERROR                   CL*17
01018          MOVE -1                     TO  MADDRTPL                    CL*17
01019          MOVE AL-UABON               TO  MADDRTPA                    CL*17
01020          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*17
01021          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT              CL*17
01022          GO TO 8100-SEND-INITIAL-MAP.                                CL*17
01023                                                                      CL**7
01024      MOVE PI-COMPANY-CD              TO  ERCOMP-COMPANY-CD.          CL*17
01025      MOVE AM-CARRIER                 TO  ERCOMP-CARRIER.             CL*17
01026      MOVE AM-GROUPING                TO  ERCOMP-GROUPING.            CL*17
01027      MOVE 'A'                        TO  ERCOMP-TYPE.                CL*17
01028      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)                          CL**7
01029                                      TO  ERCOMP-RESP-NO.             CL*17
01030                                                                      CL**9
01031      IF AM-3RD-PARTY-NOTIF-LEVEL IS EQUAL TO AM-REMIT-TO             CL**8
01032          IF AM-COM-TYP (AM-REMIT-TO) EQUAL TO 'O' OR 'P' OR          CL*20
01033                                               'G' OR 'B' or 'S'      CL*20
01034              MOVE 'G'                TO  ERCOMP-TYPE                 CL*17
01035              MOVE LOW-VALUES         TO  ERCOMP-ACCOUNT              CL*17
01036          ELSE                                                        CL*12
01037              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)                  CL*15
01038                                      TO  ERCOMP-ACCOUNT              CL*17
01039      ELSE                                                            CL**8
01040          MOVE 'G'                    TO  ERCOMP-TYPE                 CL*17
01041          MOVE LOW-VALUES             TO  ERCOMP-ACCOUNT.             CL*17
01042                                                                      CL**7
01043      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP                         CL**7
01044         MOVE ZEROS                   TO  ERCOMP-CARRIER.             CL*17
01045                                                                      CL**7
01046      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP                        CL**7
01047         MOVE ZEROS                   TO  ERCOMP-GROUPING.            CL*17
01048                                                                      CL**7
01049      EXEC CICS HANDLE CONDITION                                      CL**7
01050           NOTFND    (1320-ERCOMP-NOTFND)                             CL*17
01051      END-EXEC.                                                       CL**7
01052                                                                      CL**7
01053      EXEC CICS  READ                                                 CL**7
01054           SET      (ADDRESS OF COMPENSATION-MASTER)                  CL*20
01055           DATASET  (ERCOMP-FILE-ID)                                  CL*17
01056           RIDFLD   (ERCOMP-KEY)                                      CL*17
01057      END-EXEC.                                                       CL**7
01058                                                                      CL**7
01059      MOVE 'C'                        TO  WS-ADDR-SW.                 CL*17
01060      GO TO 1500-CONTINUE-SHOW.                                       CL*17
01061                                                                      CL**7
01062  1320-ERCOMP-NOTFND.                                                 CL*17
01063                                                                      CL**7
01064      MOVE LOW-VALUES                 TO  EL141AI.                    CL*17
01065      MOVE ER-0278                    TO  EMI-ERROR.                  CL*17
01066      MOVE -1                         TO  MADDRTPL.                   CL*17
01067      MOVE AL-UABON                   TO  MADDRTPA.                   CL*17
01068      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.                   CL*17
01069      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
01070      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
01071      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
01072                                                                      CL**7
01073      EJECT                                                           CL*20
01074  1500-CONTINUE-SHOW.                                                 CL*17
01075                                                                      CL**7
01076      MOVE SPACES                     TO  WS-ZIP-CODE.                CL*17
01077                                                                      CL*13
01078      IF WS-ADDR-SW EQUAL 'C'                                         CL**7
01079         MOVE CO-ACCT-NAME            TO  MAPNAMEO                    CL*17
01080         IF CO-ACCT-NAME EQUAL SPACES                                 CL**7
01081            MOVE CO-MAIL-NAME         TO  MAPNAMEO.                   CL*17
01082                                                                      CL**7
01083      IF WS-ADDR-SW EQUAL 'C'                                         CL**7
01084         MOVE CO-ADDR-1               TO  MADDRL1O                    CL*17
01085         MOVE CO-ADDR-2               TO  MADDRL2O                    CL*17
              MOVE CO-ADDR-CITY            TO  MCITYO
              MOVE CO-ADDR-STATE           TO  MSTATEO
01087         MOVE +0                      TO  WS-SEQ-CONVERT              CL*17
01088         MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O                    CL*17
01089         MOVE CO-AREA-CODE            TO  WS-PH-ED-1                  CL*17
01090         MOVE CO-PREFIX               TO  WS-PH-ED-2                  CL*17
01091         MOVE CO-PHONE                TO  WS-PH-ED-3                  CL*17
01092         MOVE WS-PHONE-EDIT           TO  MAPHONEO                    CL*17
01093         MOVE SPACES                  TO  MAPSEQ2O                    CL*17
01094         MOVE '*'                     TO  MAPNOT1O                    CL*17
01095         MOVE 'ABOVE ADDRESS FROM COMPENSATION MASTER'                CL**7
01096                                      TO  MAPNOT2O.                   CL*17
01097                                                                      CL**7
01098      IF WS-ADDR-SW EQUAL 'C'                                         CL*13
01099         IF CO-CANADIAN-POST-CODE                                     CL*13
01100             MOVE CO-CAN-POSTAL-1     TO  WS-ZIP-CAN-2-POST1          CL*17
01101             MOVE CO-CAN-POSTAL-2     TO  WS-ZIP-CAN-2-POST2          CL*17
01102         ELSE                                                         CL*13
01103             MOVE CO-ZIP-PRIME        TO  WS-ZIP-AM-2-CODE            CL*17
01104             IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 CL*13
01105                 MOVE '-'             TO  WS-ZIP-AM-2-DASH            CL*17
01106                 MOVE CO-ZIP-PLUS4    TO  WS-ZIP-AM-2-PLUS4.          CL*17
01107                                                                      CL*13
01108      IF WS-ADDR-SW EQUAL 'A'                                         CL**7
01109         MOVE AM-NAME                 TO  MAPNAMEO                    CL*17
01110         MOVE AM-PERSON               TO  MADDRL1O                    CL*17
01111         MOVE AM-ADDRS                TO  MADDRL2O                    CL*17
              MOVE AM-ADDR-CITY            TO  MCITYO
              MOVE AM-ADDR-STATE           TO  MSTATEO
01113         MOVE +0                      TO  WS-SEQ-CONVERT              CL*17
01114         MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O                    CL*17
01115         MOVE AM-AREA-CODE            TO  WS-PH-ED-1                  CL*17
01116         MOVE AM-TEL-PRE              TO  WS-PH-ED-2                  CL*17
01117         MOVE AM-TEL-NBR              TO  WS-PH-ED-3                  CL*17
01118         MOVE WS-PHONE-EDIT           TO  MAPHONEO                    CL*17
01119         MOVE SPACES                  TO  MAPSEQ2O                    CL*17
01120         MOVE '*'                     TO  MAPNOT1O                    CL*17
01121         MOVE 'ABOVE ADDRESS FROM ACCOUNT MASTER'                     CL**7
01122                                      TO  MAPNOT2O.                   CL*17
01123                                                                      CL**7
01124      IF WS-ADDR-SW EQUAL 'A'                                         CL*13
01125         IF AM-CANADIAN-POST-CODE                                     CL*13
01126             MOVE AM-CAN-POSTAL-1     TO  WS-ZIP-CAN-2-POST1          CL*17
01127             MOVE AM-CAN-POSTAL-2     TO  WS-ZIP-CAN-2-POST2          CL*17
01128         ELSE                                                         CL*13
01129             MOVE AM-ZIP-PRIME        TO  WS-ZIP-AM-2-CODE            CL*17
01130             IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 CL*13
01131                 MOVE '-'             TO  WS-ZIP-AM-2-DASH            CL*17
01132                 MOVE AM-ZIP-PLUS4    TO  WS-ZIP-AM-2-PLUS4.          CL*17
01133                                                                      CL*13
01134      IF WS-ADDR-SW EQUAL 'P'                                         CL*18
01135         MOVE PD-NAME                 TO  MAPNAMEO                    CL*18
01136         MOVE PD-PERSON               TO  MADDRL1O                    CL*18
01137         MOVE PD-ADDRS                TO  MADDRL2O                    CL*18
01138         MOVE PD-CITY                 TO  MCITYO
01139         MOVE +0                      TO  WS-SEQ-CONVERT              CL*18
01140         MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O                    CL*18
01141         MOVE PD-AREA-CODE            TO  WS-PH-ED-1                  CL*18
01142         MOVE PD-TEL-PRE              TO  WS-PH-ED-2                  CL*18
01143         MOVE PD-TEL-NBR              TO  WS-PH-ED-3                  CL*18
01144         MOVE WS-PHONE-EDIT           TO  MAPHONEO                    CL*18
01145         MOVE SPACES                  TO  MAPSEQ2O                    CL*18
01146         MOVE '*'                     TO  MAPNOT1O                    CL*18
01147         MOVE 'ABOVE ADDRESS FROM PRODUCER MASTER'                    CL*18
01148                                      TO  MAPNOT2O.                   CL*18
01149                                                                      CL*18
01150      IF WS-ADDR-SW EQUAL 'P'                                         CL*18
01151          MOVE PD-ZIP-PRIME           TO  WS-ZIP-AM-2-CODE            CL*18
01152          IF (PD-ZIP-PLUS4 NOT = SPACES  AND  ZEROS)                  CL*18
01153              MOVE '-'                TO  WS-ZIP-AM-2-DASH            CL*18
01154              MOVE PD-ZIP-PLUS4       TO  WS-ZIP-AM-2-PLUS4.          CL*18
01155                                                                      CL*18
01156      IF WS-ADDR-SW EQUAL 'T'                                         CL**7
01157         MOVE AT-MAIL-TO-NAME         TO  MAPNAMEO                    CL*17
01158         MOVE AT-ADDRESS-LINE-1       TO  MADDRL1O                    CL*17
01159         MOVE AT-ADDRESS-LINE-2       TO  MADDRL2O                    CL*17
              MOVE AT-CITY                 TO  MCITYO
              MOVE AT-STATE                TO  MSTATEO
01161 *       MOVE AT-ADDRESS-TYPE         TO  MADDRTPO                    CL*17
01162         MOVE ELTRLR-SEQUENCE-NO      TO  WS-SEQ-CONVERT              CL*17
01163         MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O                    CL*17
01164         MOVE AT-PHONE-NO             TO  WS-PHONE-BRKDN              CL*17
01165         MOVE WS-PH-1                 TO  WS-PH-ED-1                  CL*17
01166         MOVE WS-PH-2                 TO  WS-PH-ED-2                  CL*17
01167         MOVE WS-PH-3                 TO  WS-PH-ED-3                  CL*17
061511        MOVE WS-PHONE-EDIT           TO  MAPHONEO                    CL*17
061511        MOVE AT-VFY-2ND-BENE-SSN     TO  BENESSNO
061511        MOVE AT-VFY-2ND-BENE-VERIFIED TO BENEVFYO.
01169                                                                      CL*13
01170      IF WS-ADDR-SW EQUAL 'T'                                         CL*13
01171         IF AT-CANADIAN-POST-CODE                                     CL*13
01172             MOVE AT-CAN-POSTAL-1     TO  WS-ZIP-CAN-2-POST1          CL*17
01173             MOVE AT-CAN-POSTAL-2     TO  WS-ZIP-CAN-2-POST2          CL*17
01174         ELSE                                                         CL*13
01175             MOVE AT-ZIP-CODE         TO  WS-ZIP-AM-2-CODE            CL*17
01176             IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 CL*13
01177                 MOVE '-'             TO  WS-ZIP-AM-2-DASH            CL*17
01178                 MOVE AT-ZIP-PLUS4    TO  WS-ZIP-AM-2-PLUS4.          CL*17
01179                                                                      CL**7
01180      IF WS-ADDR-SW EQUAL 'B'                                         CL**7
01181          IF WS-ADDR-SEQ NOT EQUAL '9'                                CL*20
01182              MOVE BE-MAIL-TO-NAME        TO  MAPNAMEO                CL*20
01183              MOVE BE-ADDRESS-LINE-1      TO  MADDRL1O                CL*20
01184              MOVE BE-ADDRESS-LINE-2      TO  MADDRL2O                CL*20
                   MOVE BE-CITY                TO  MCITYO
                   MOVE BE-STATE               TO  MSTATEO
01186              MOVE +0                     TO  WS-SEQ-CONVERT          CL*20
01187              MOVE WS-SEQ-CONVERT         TO  MAPSEQ2O                CL*20
01188              MOVE BE-PHONE-NO            TO  WS-PHONE-BRKDN          CL*20
01189              MOVE WS-PH-1                TO  WS-PH-ED-1              CL*20
01190              MOVE WS-PH-2                TO  WS-PH-ED-2              CL*20
01191              MOVE WS-PH-3                TO  WS-PH-ED-3              CL*20
01192              MOVE WS-PHONE-EDIT          TO  MAPHONEO                CL*20
01193              MOVE SPACES                 TO  MAPSEQ2O                CL*20
01194              MOVE '*'                    TO  MAPNOT1O                CL*20
01195              MOVE 'ABOVE ADDRESS FROM BENEFICIARY MASTER'            CL*20
01196                                          TO  MAPNOT2O                CL*20
01197              IF BE-CANADIAN-POST-CODE                                CL*20
01198                 MOVE BE-CAN-POSTAL-1     TO  WS-ZIP-CAN-2-POST1      CL*20
01199                 MOVE BE-CAN-POSTAL-2     TO  WS-ZIP-CAN-2-POST2      CL*20
01200              ELSE                                                    CL*20
01201                 MOVE BE-ZIP-PRIME        TO  WS-ZIP-AM-2-CODE        CL*20
01202                 IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS             CL*20
01203                     MOVE '-'             TO  WS-ZIP-AM-2-DASH        CL*20
01204                     MOVE BE-ZIP-PLUS4    TO  WS-ZIP-AM-2-PLUS4       CL*20
01205                 ELSE                                                 CL*20
01206                     NEXT SENTENCE                                    CL*20
01207          ELSE                                                        CL*20
01208             MOVE BE-MAIL-TO-NAME2        TO  MAPNAMEO                CL*20
01209             MOVE BE-ADDRESS-LINE-12      TO  MADDRL1O                CL*20
01210             MOVE BE-ADDRESS-LINE-22      TO  MADDRL2O                CL*20
                  MOVE BE-CITY2                TO  MCITYO
                  MOVE BE-STATE2               TO  MSTATEO
01212             MOVE +0                      TO  WS-SEQ-CONVERT          CL*20
01213             MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O                CL*20
01214             MOVE BE-PHONE-NO2            TO  WS-PHONE-BRKDN          CL*20
01215             MOVE WS-PH-1                 TO  WS-PH-ED-1              CL*20
01216             MOVE WS-PH-2                 TO  WS-PH-ED-2              CL*20
01217             MOVE WS-PH-3                 TO  WS-PH-ED-3              CL*20
01218             MOVE WS-PHONE-EDIT           TO  MAPHONEO                CL*20
01219             MOVE SPACES                  TO  MAPSEQ2O                CL*20
01220             MOVE '*'                     TO  MAPNOT1O                CL*20
01221             MOVE 'ABOVE ADDRESS FROM BENEFICIARY ADDR2 '             CL*20
01222                                      TO  MAPNOT2O                    CL*20
01223             IF BE-CANADIAN-POST-CODE2                                CL*20
01224                 MOVE BE-CAN-POSTAL-12    TO  WS-ZIP-CAN-2-POST1      CL*20
01225                 MOVE BE-CAN-POSTAL-22    TO  WS-ZIP-CAN-2-POST2      CL*20
01226             ELSE                                                     CL*20
01227                 MOVE BE-ZIP-PRIME2       TO  WS-ZIP-AM-2-CODE        CL*20
01228                 IF BE-ZIP-PLUS42 NOT = SPACES  AND  ZEROS            CL*20
01229                     MOVE '-'             TO  WS-ZIP-AM-2-DASH        CL*20
01230                     MOVE BE-ZIP-PLUS42   TO  WS-ZIP-AM-2-PLUS4.      CL*20
01231                                                                      CL*13
01232      MOVE WS-ZIP-CODE                TO  MZIPCODO.                   CL*17
01233                                                                      CL*20
01234      IF WS-ADDR-TYPE EQUAL 'B' AND                                   CL*20
01235         WS-ADDR-SEQ EQUAL '9' OR '0'                                 CL*20
01236            MOVE AL-PABON                TO MAPNAMEA                  CL*20
01237                                            MADDRL1A                  CL*20
01238                                            MADDRL2A                  CL*20
01239                                            MCITYA
                                                 MSTATEA
01240                                            MAPHONEA                  CL*20
01241                                            MZIPCODA.                 CL*20
01242                                                                      CL*13
01243      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')       CL*18
01244          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')    CL*18
01245              IF PI-MAINT IS EQUAL TO 'A'                             CL*17
01246                  MOVE ER-0265        TO  EMI-ERROR                   CL*17
01247                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*17
01248                  MOVE PI-CLAIM-NO    TO  EMI-TEXT-VARIABLE (1).      CL*17
01249                                                                      CL*17
01250      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')       CL*18
01251          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')    CL*18
01252              IF PI-MAINT IS EQUAL TO 'A'                             CL*17
01253                  MOVE 'S'            TO  PI-MAINT                    CL*17
01254                  IF PI-LETTER-ERROR-CODE IS NOT EQUAL TO ZEROS       CL*17
01255                      MOVE PI-LETTER-ERROR-CODE   TO  EMI-ERROR       CL*17
01256                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.       CL*17
01257                                                                      CL**7
01258      IF ADD-REQUIRED OR CHANGE-REQUIRED                              CL*11
01259          MOVE ER-0000                TO  EMI-ERROR                   CL*17
01260          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*17
01261                                                                      CL*11
01262      MOVE -1                         TO  MFMAINTL.                   CL*17
01263                                                                      CL**9
01264      MOVE AL-UANON                   TO  MADDRTPA   MFMAINTA         CL*17
01265                                          MAPNAMEA   MADDRL1A         CL*17
01266                                          MADDRL2A   MCITYA
                                               MSTATEA
01267                                          MZIPCODA   MAPHONEA.        CL*17
01268                                                                      CL**9
01269      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
01270                                                                      CL**7
01271      MOVE 'S'                        TO  PI-PREV-MAINT-CODE.         CL*19
01272      MOVE MADDRTPI                   TO  PI-PREV-ADDR-TYPE.          CL*19
01273                                                                      CL*19
01274      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
01275      EJECT                                                           CL**7
01276  2000-CHANGE-ADDRESS.                                                CL*17
01277                                                                      CL*19
01278      IF PI-COMPANY-ID EQUAL 'DMD' AND                                CL*20
01279         WS-ADDR-SEQ   EQUAL '9'   AND                                CL*20
01280         WS-ADDR-TYPE  EQUAL 'B'                                      CL*20
01281         MOVE ER-8131                TO  EMI-ERROR                    CL*20
01282         MOVE -1                     TO  MFMAINTL                     CL*20
01283         MOVE AL-UABON               TO  MFMAINTA                     CL*20
01284                                         MADDRTPA                     CL*20
01285         PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT               CL*20
01286         GO TO 8200-SEND-DATAONLY.                                    CL*20
01287                                                                      CL*20
01288      IF PI-PREV-MAINT-CODE IS EQUAL TO 'S' AND                       CL*19
01289         PI-PREV-ADDR-TYPE IS EQUAL TO MADDRTPI                       CL*19
01290          NEXT SENTENCE                                               CL*19
01291      ELSE                                                            CL*19
01292          MOVE ER-0145                TO  EMI-ERROR                   CL*19
01293          MOVE -1                     TO  MFMAINTL                    CL*19
01294          MOVE AL-UABON               TO  MFMAINTA                    CL*19
01295                                          MADDRTPA                    CL*19
01296          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*19
01297          GO TO 8100-SEND-INITIAL-MAP.                                CL*19
01298                                                                      CL**7
01299      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.          CL*17
01300      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.             CL*17
01301      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.            CL*17
01302      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.             CL*17
01303                                                                      CL**7
01304      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.                        CL*17
01305                                                                      CL**7
01306      PERFORM 6000-EDIT-SCREEN THRU 6000-EXIT.                        CL*17
01307                                                                      CL**7
01308      IF NOT EMI-NO-ERRORS                                            CL*17
01309          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT              CL*17
01310          GO TO 8200-SEND-DATAONLY.                                   CL*17
01311                                                                      CL**7
01312      IF WS-ADDR-TYPE EQUAL 'I'                                       CL**7
01313         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01314      ELSE                                                            CL**7
01315      IF WS-ADDR-TYPE EQUAL 'B'                                       CL**7
01316         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01317         ADD +10                      TO  ELTRLR-SEQUENCE-NO          CL*17
01318      ELSE                                                            CL**7
01319      IF WS-ADDR-TYPE EQUAL 'A'                                       CL**7
01320         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01321         ADD +20                      TO  ELTRLR-SEQUENCE-NO          CL*17
01322      ELSE                                                            CL**7
01323      IF WS-ADDR-TYPE EQUAL 'P'                                       CL**7
01324         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01325         ADD +30                      TO  ELTRLR-SEQUENCE-NO          CL*17
01326      ELSE                                                            CL**7
01327      IF WS-ADDR-TYPE EQUAL 'E'                                       CL**7
01328         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01329         ADD +40                      TO  ELTRLR-SEQUENCE-NO          CL*17
01330      ELSE                                                            CL**7
01331      IF WS-ADDR-TYPE EQUAL 'O'                                       CL**7
01332         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01333         ADD +50                      TO  ELTRLR-SEQUENCE-NO          CL*17
01334      ELSE                                                            CL**7
01335      IF WS-ADDR-TYPE EQUAL 'Q'                                       CL**7
01336         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01337         ADD +60                      TO  ELTRLR-SEQUENCE-NO.         CL*17
01338                                                                      CL**7
01339      EXEC CICS HANDLE CONDITION                                      CL*17
01340          NOTFND   (9050-NOTFND-ELTRLR)                               CL*20
01341          NOTOPEN  (9060-NOTOPEN-ELTRLR)                              CL*20
01342      END-EXEC.                                                       CL**7
01343                                                                      CL**7
01344      PERFORM 7055-READ-ELTRLR-UPDATE THRU 7055-EXIT.                 CL*17
01345                                                                      CL**7
01346      GO TO 2050-CONTINUE-CHANGE.                                     CL*17
01347                                                                      CL**7
01348  2050-CONTINUE-CHANGE.                                               CL*17
01349                                                                      CL**7
01350      IF  MAPNAMEI  NOT = LOW-VALUES                                  CL**7
01351          MOVE MAPNAMEI               TO  AT-MAIL-TO-NAME.            CL*17
01352      IF  MADDRL1I  NOT = LOW-VALUES                                  CL**7
01353          MOVE MADDRL1I               TO  AT-ADDRESS-LINE-1.          CL*17
01354      IF  MADDRL2I  NOT = LOW-VALUES                                  CL**7
01355          MOVE MADDRL2I               TO  AT-ADDRESS-LINE-2.          CL*17
           IF  MCITYI NOT = LOW-VALUES
               MOVE MCITYI                 TO  AT-CITY.
           IF  MSTATEI NOT = LOW-VALUES
               MOVE MSTATEI                TO  AT-STATE.
01358      IF  WS-PHONE-NUM  NOT = LOW-VALUES                              CL**7
01359          MOVE WS-PHONE-NUM           TO  AT-PHONE-NO.                CL*17
01360      IF  MADDRTPI   NOT = LOW-VALUES                                 CL**7
01361          MOVE MADDRTPI               TO  AT-ADDRESS-TYPE.            CL*17
061511     IF  BENESSNI NOT = LOW-VALUES
061511         MOVE BENESSNI               TO  AT-VFY-2ND-BENE-SSN
061511     END-IF.
061511     IF  BENEVFYI NOT = LOW-VALUES
061511         MOVE BENEVFYI               TO  AT-VFY-2ND-BENE-VERIFIED
061511     END-IF.
01362                                                                      CL*13
01363      IF  MZIPCODL  =  ZEROS                                          CL*13
01364          GO TO 2060-CONTINUE-CHANGE.                                 CL*17
01365                                                                      CL*13
01366      MOVE MZIPCODI                   TO  WS-ZIP-CODE.                CL*17
01367      MOVE SPACES                     TO  AT-ZIP.                     CL*17
01368                                                                      CL*13
01369      IF WS-CANADIAN-ZIP                                              CL*13
01370          IF WS-ZIP-4 = SPACE  OR  '-'                                CL*13
01371              MOVE WS-ZIP-CAN-2-POST1 TO  AT-CAN-POSTAL-1             CL*17
01372              MOVE WS-ZIP-CAN-2-POST2 TO  AT-CAN-POSTAL-2             CL*17
01373          ELSE                                                        CL*13
01374              MOVE WS-ZIP-CAN-1-POST1 TO  AT-CAN-POSTAL-1             CL*17
01375              MOVE WS-ZIP-CAN-1-POST2 TO  AT-CAN-POSTAL-2             CL*17
01376      ELSE                                                            CL*13
01377          IF WS-ZIP-6 = SPACE  OR  '-'                                CL*13
01378              MOVE WS-ZIP-AM-2-CODE   TO  AT-ZIP-CODE                 CL*17
01379              MOVE WS-ZIP-AM-2-PLUS4  TO  AT-ZIP-PLUS4                CL*17
01380          ELSE                                                        CL*13
01381              MOVE WS-ZIP-AM-1-CODE   TO  AT-ZIP-CODE                 CL*17
01382              MOVE WS-ZIP-AM-1-PLUS4  TO  AT-ZIP-PLUS4.               CL*17
01383                                                                      CL**7
01384  2060-CONTINUE-CHANGE.                                               CL*17
01385                                                                      CL**7
01386      MOVE PI-PROCESSOR-ID        TO  AT-ADDRESS-LAST-UPDATED-BY.     CL*17
01387      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.           CL*17
01388      MOVE WS-CURR-DATE-BIN       TO  AT-ADDRESS-LAST-MAINT-DT.       CL*17
01389                                                                      CL*17
01390      PERFORM 7060-REWRITE-ELTRLR THRU 7060-EXIT.                     CL*17
01391                                                                      CL*17
01392      GO TO 1000-SHOW-ADDRESS.                                        CL*17
01393                                                                      CL*17
01394      EJECT                                                           CL*17
01395  3000-ADD-ADDRESS.                                                   CL*17
01396                                                                      CL*17
01397      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.          CL*17
01398      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.             CL*17
01399      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.            CL*17
01400      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.             CL*17
01401                                                                      CL*17
01402      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.                        CL*17
01403                                                                      CL**7
01404      PERFORM 6000-EDIT-SCREEN THRU 6000-EXIT.                        CL*17
01405                                                                      CL**7
01406      IF NOT EMI-NO-ERRORS                                            CL*17
01407          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT              CL*17
01408          GO TO 8200-SEND-DATAONLY.                                   CL*17
01409                                                                      CL**7
01410      IF WS-ADDR-TYPE EQUAL 'I'                                       CL**7
01411         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01412      ELSE                                                            CL**7
01413      IF WS-ADDR-TYPE EQUAL 'B'                                       CL**7
01414         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01415         ADD +10                      TO  ELTRLR-SEQUENCE-NO          CL*17
01416      ELSE                                                            CL**7
01417      IF WS-ADDR-TYPE EQUAL 'A'                                       CL**7
01418         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01419         ADD +20                      TO  ELTRLR-SEQUENCE-NO          CL*17
01420      ELSE                                                            CL**7
01421      IF WS-ADDR-TYPE EQUAL 'P'                                       CL**7
01422         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01423         ADD +30                      TO  ELTRLR-SEQUENCE-NO          CL*17
01424      ELSE                                                            CL**7
01425      IF WS-ADDR-TYPE EQUAL 'E'                                       CL**7
01426         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01427         ADD +40                      TO  ELTRLR-SEQUENCE-NO          CL*17
01428      ELSE                                                            CL**7
01429      IF WS-ADDR-TYPE EQUAL 'O'                                       CL**7
01430         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01431         ADD +50                      TO  ELTRLR-SEQUENCE-NO          CL*17
01432      ELSE                                                            CL**7
01433      IF WS-ADDR-TYPE EQUAL 'Q'                                       CL**7
01434         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01435         ADD +60                      TO  ELTRLR-SEQUENCE-NO.         CL*17
01436                                                                      CL**7
01437      PERFORM 7005-READ-ELMSTR-UPDATE THRU 7005-EXIT.                 CL*17
01438                                                                      CL**7
01439  3010-CONTINUE-ADD.                                                  CL*17
01440                                                                      CL**7
01441      PERFORM 7080-GETMAIN-ELTRLR THRU 7080-EXIT.                     CL*17
01442                                                                      CL**7
01443      MOVE ELTRLR-KEY                 TO  AT-CONTROL-PRIMARY.         CL*17
01444      MOVE 'AT'                       TO  AT-RECORD-ID.               CL*17
01445      MOVE '5'                        TO  AT-TRAILER-TYPE.            CL*17
01446      MOVE MAPNAMEI                   TO  AT-MAIL-TO-NAME.            CL*17
01447                                                                      CL**7
01448      IF MADDRL1L GREATER THAN ZEROS                                  CL**7
01449         MOVE MADDRL1I                TO  AT-ADDRESS-LINE-1           CL*17
01450      ELSE                                                            CL**7
01451         MOVE SPACES                  TO  AT-ADDRESS-LINE-1.          CL*17
01452                                                                      CL*17
01453      IF MADDRL2L GREATER THAN ZEROS                                  CL**7
01454          MOVE MADDRL2I               TO  AT-ADDRESS-LINE-2           CL*17
01455      ELSE                                                            CL**7
01456          MOVE SPACES                 TO  AT-ADDRESS-LINE-2.          CL*17
01457                                                                      CL**7
01458      IF MCITYL > 0
01459         MOVE MCITYI                  TO  AT-CITY
01460      ELSE                                                            CL**7
01461         MOVE SPACES                  TO  AT-CITY.
01458      IF MSTATEL > 0
01459         MOVE MSTATEI                 TO  AT-STATE
01460      ELSE                                                            CL**7
01461         MOVE SPACES                  TO  AT-STATE.
01462                                                                      CL**7
01463      MOVE WS-PHONE-NUM               TO  AT-PHONE-NO.                CL*17
01464      MOVE MADDRTPI                   TO  AT-ADDRESS-TYPE.            CL*17
01465      MOVE PI-PROCESSOR-ID            TO  AT-RECORDED-BY              CL*17
01466                                        AT-ADDRESS-LAST-UPDATED-BY.   CL*17
061511     IF  BENESSNL GREATER THAN ZEROS
061511         MOVE BENESSNI               TO  AT-VFY-2ND-BENE-SSN
061511     END-IF.
032514     IF  BENEVFYL GREATER THAN ZEROS
061511         MOVE BENEVFYI               TO  AT-VFY-2ND-BENE-VERIFIED
061511     END-IF.
01467                                                                      CL*13
01468      IF MZIPCODL  =  ZEROS                                           CL*13
01469          MOVE SPACES                 TO  AT-ZIP                      CL*17
01470          GO TO 3020-CONTINUE-ADD.                                    CL*17
01471                                                                      CL*13
01472      MOVE MZIPCODI                   TO  WS-ZIP-CODE.                CL*17
01473      MOVE SPACES                     TO  AT-ZIP.                     CL*17
01474                                                                      CL*13
01475      IF WS-CANADIAN-ZIP                                              CL*13
01476          IF WS-ZIP-4 = SPACE  OR  '-'                                CL*13
01477              MOVE WS-ZIP-CAN-2-POST1 TO  AT-CAN-POSTAL-1             CL*17
01478              MOVE WS-ZIP-CAN-2-POST2 TO  AT-CAN-POSTAL-2             CL*17
01479          ELSE                                                        CL*13
01480              MOVE WS-ZIP-CAN-1-POST1 TO  AT-CAN-POSTAL-1             CL*17
01481              MOVE WS-ZIP-CAN-1-POST2 TO  AT-CAN-POSTAL-2             CL*17
01482      ELSE                                                            CL*13
01483          IF WS-ZIP-6 = SPACE  OR  '-'                                CL*13
01484              MOVE WS-ZIP-AM-2-CODE   TO  AT-ZIP-CODE                 CL*17
01485              MOVE WS-ZIP-AM-2-PLUS4  TO  AT-ZIP-PLUS4                CL*17
01486          ELSE                                                        CL*13
01487              MOVE WS-ZIP-AM-1-CODE   TO  AT-ZIP-CODE                 CL*17
01488              MOVE WS-ZIP-AM-1-PLUS4  TO  AT-ZIP-PLUS4.               CL*17
01489                                                                      CL*13
01490  3020-CONTINUE-ADD.                                                  CL*17
01491                                                                      CL**7
01492      MOVE EIBTIME                    TO  AT-LAST-MAINT-HHMMSS.       CL*17
01493      MOVE WS-CURR-DATE-BIN           TO  AT-RECORDED-DT              CL*17
01494                                          AT-ADDRESS-LAST-MAINT-DT.   CL*17
01495                                                                      CL*17
01496      EXEC CICS  HANDLE CONDITION                                     CL*17
01497           DUPREC   (3030-TRLR-DUPREC)                                CL*17
01498      END-EXEC.                                                       CL*17
01499                                                                      CL*17
01500      PERFORM 7065-WRITE-ELTRLR THRU 7065-EXIT.                       CL*17
01501                                                                      CL**7
01502      GO TO 3040-CONTINUE-ADD.                                        CL*17
01503                                                                      CL**7
01504  3030-TRLR-DUPREC.                                                   CL*17
01505                                                                      CL**7
01506      PERFORM 7015-UNLOCK-ELMSTR THRU 7015-EXIT.                      CL*17
01507                                                                      CL**7
01508      GO TO 1000-SHOW-ADDRESS.                                        CL*17
01509                                                                      CL**7
01510  3040-CONTINUE-ADD.                                                  CL*17
01511                                                                      CL**7
01512      MOVE PI-PROCESSOR-ID            TO  CL-LAST-MAINT-USER.         CL*17
01513      MOVE '3'                        TO  CL-LAST-MAINT-TYPE.         CL*17
01514      MOVE EIBTIME                    TO  CL-LAST-MAINT-HHMMSS.       CL*17
01515      MOVE WS-CURR-DATE-BIN           TO  CL-LAST-MAINT-DT.           CL*17
01516                                                                      CL**7
01517      IF WS-ADDR-TYPE EQUAL 'I'                                       CL**7
01518         ADD +1                       TO  CL-INSURED-ADDR-CNT         CL*17
01519      ELSE                                                            CL**7
01520      IF WS-ADDR-TYPE EQUAL 'B'  AND                                  CL*20
01521         WS-ADDR-SEQ NOT EQUAL '9'                                    CL*20
01522         ADD +1                       TO  CL-BENIF-ADDR-CNT           CL*17
01523      ELSE                                                            CL**7
01524      IF WS-ADDR-TYPE EQUAL 'A' AND                                   CL**7
01525         WS-ADDR-SEQ NOT EQUAL '9'                                    CL**7
01526         ADD +1                       TO  CL-ACCOUNT-ADDR-CNT         CL*17
01527      ELSE                                                            CL**7
01528      IF WS-ADDR-TYPE EQUAL 'P'                                       CL**7
01529         ADD +1                       TO  CL-DOCTOR-ADDR-CNT          CL*17
01530      ELSE                                                            CL**7
01531      IF WS-ADDR-TYPE EQUAL 'E'                                       CL**7
01532         ADD +1                       TO  CL-EMPLOYER-ADDR-CNT        CL*17
01533      ELSE                                                            CL**7
01534      IF WS-ADDR-TYPE EQUAL 'O'                                       CL**7
01535         ADD +1                       TO  CL-OTHER-1-ADDR-CNT         CL*17
01536      ELSE                                                            CL**7
01537      IF WS-ADDR-TYPE EQUAL 'Q'                                       CL**7
01538         ADD +1                       TO  CL-OTHER-2-ADDR-CNT.        CL*17
01539                                                                      CL*17
01540      PERFORM 7010-REWRITE-ELMSTR THRU 7010-EXIT.                     CL*17
01541                                                                      CL**7
01542      GO TO 1000-SHOW-ADDRESS.                                        CL*17
01543                                                                      CL**7
01544  4000-DELETE-ADDRESS.                                                CL*17
01545                                                                      CL*19
01546      IF PI-PREV-MAINT-CODE IS EQUAL TO 'S' AND                       CL*19
01547         PI-PREV-ADDR-TYPE IS EQUAL TO MADDRTPI                       CL*19
01548          NEXT SENTENCE                                               CL*19
01549      ELSE                                                            CL*19
01550          MOVE ER-0145                TO  EMI-ERROR                   CL*19
01551          MOVE -1                     TO  MFMAINTL                    CL*19
01552          MOVE AL-UABON               TO  MFMAINTA                    CL*19
01553                                          MADDRTPA                    CL*19
01554          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*19
01555          GO TO 8100-SEND-INITIAL-MAP.                                CL*19
01556                                                                      CL**7
01557      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.          CL*17
01558      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.             CL*17
01559      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.            CL*17
01560      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.             CL*17
01561                                                                      CL**7
01562      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.                        CL*17
01563                                                                      CL*17
01564      IF ((WS-ADDR-TYPE EQUAL 'I') AND                                CL**7
01565         (CL-INSURED-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))             CL**7
01566       OR                                                             CL**7
01567         ((WS-ADDR-TYPE EQUAL 'A') AND                                CL**7
01568         (CL-ACCOUNT-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM) AND          CL**7
01569         (WS-ADDR-SEQ NOT EQUAL '9'))                                 CL**7
01570       OR                                                             CL**7
01571         ((WS-ADDR-TYPE EQUAL 'B') AND                                CL**7
01572         (CL-BENIF-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM)  AND           CL*20
01573         (WS-ADDR-SEQ NOT EQUAL '9'))                                 CL*20
01574       OR                                                             CL**7
01575         ((WS-ADDR-TYPE EQUAL 'E') AND                                CL**7
01576         (CL-EMPLOYER-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))            CL**7
01577       OR                                                             CL**7
01578         ((WS-ADDR-TYPE EQUAL 'P') AND                                CL**7
01579         (CL-DOCTOR-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))              CL**7
01580       OR                                                             CL**7
01581         ((WS-ADDR-TYPE EQUAL 'O') AND                                CL**7
01582         (CL-OTHER-1-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))             CL**7
01583       OR                                                             CL**7
01584         ((WS-ADDR-TYPE EQUAL 'Q') AND                                CL**7
01585         (CL-OTHER-2-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))             CL**7
01586          MOVE ER-1879                TO  EMI-ERROR                   CL*17
01587          MOVE -1                     TO  MADDRTPL                    CL*17
01588          MOVE AL-UABON               TO  MADDRTPA                    CL*17
01589          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*17
01590          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT              CL*17
01591          GO TO 8200-SEND-DATAONLY.                                   CL*17
01592                                                                      CL**7
01593      IF WS-ADDR-TYPE EQUAL 'I'                                       CL**7
01594         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01595      ELSE                                                            CL**7
01596      IF WS-ADDR-TYPE EQUAL 'B'                                       CL**7
01597         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01598         ADD +10                      TO  ELTRLR-SEQUENCE-NO          CL*17
01599      ELSE                                                            CL**7
01600      IF WS-ADDR-TYPE EQUAL 'A'                                       CL**7
01601         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01602         ADD +20                      TO  ELTRLR-SEQUENCE-NO          CL*17
01603      ELSE                                                            CL**7
01604      IF WS-ADDR-TYPE EQUAL 'P'                                       CL**7
01605         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01606         ADD +30                      TO  ELTRLR-SEQUENCE-NO          CL*17
01607      ELSE                                                            CL**7
01608      IF WS-ADDR-TYPE EQUAL 'E'                                       CL**7
01609         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01610         ADD +40                      TO  ELTRLR-SEQUENCE-NO          CL*17
01611      ELSE                                                            CL**7
01612      IF WS-ADDR-TYPE EQUAL 'O'                                       CL**7
01613         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01614         ADD +50                      TO  ELTRLR-SEQUENCE-NO          CL*17
01615      ELSE                                                            CL**7
01616      IF WS-ADDR-TYPE EQUAL 'Q'                                       CL**7
01617         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO          CL*17
01618         ADD +60                      TO  ELTRLR-SEQUENCE-NO.         CL*17
01619                                                                      CL**7
01620      PERFORM 7005-READ-ELMSTR-UPDATE THRU 7005-EXIT.                 CL*17
01621                                                                      CL**7
01622      EXEC CICS HANDLE CONDITION                                      CL*17
01623          NOTFND   (4010-NOTFND-ELTRLR)                               CL*17
01624          NOTOPEN  (4020-NOTOPEN-ELTRLR)                              CL*17
01625      END-EXEC.                                                       CL*17
01626                                                                      CL*17
01627      PERFORM 7055-READ-ELTRLR-UPDATE THRU 7055-EXIT.                 CL*17
01628                                                                      CL**7
01629      GO TO 4050-CONTINUE-DELETE.                                     CL*17
01630                                                                      CL**7
01631  4010-NOTFND-ELTRLR.                                                 CL*17
01632                                                                      CL**7
01633      PERFORM 7075-UNLOCK-ELTRLR THRU 7075-EXIT.                      CL*17
01634                                                                      CL**7
01635      MOVE LOW-VALUES                 TO  EL141AI.                    CL*17
01636      MOVE ER-0135                    TO  EMI-ERROR.                  CL*17
01637      MOVE -1                         TO  MADDRTPL.                   CL*17
01638      MOVE AL-UABON                   TO  MADDRTPA                    CL*17
01639                                          MFMAINTA.                   CL*17
01640      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.                   CL*17
01641      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.                   CL*17
01642      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
01643      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
01644      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
01645                                                                      CL**7
01646  4020-NOTOPEN-ELTRLR.                                                CL*17
01647                                                                      CL**7
01648      MOVE LOW-VALUES                 TO  EL141AI.                    CL*17
01649      MOVE ER-0172                    TO  EMI-ERROR.                  CL*17
01650      MOVE -1                         TO  MFMAINTL.                   CL*17
01651      MOVE AL-UABON                   TO  MFMAINTA.                   CL*17
01652      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.                   CL*17
01653      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
01654      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
01655      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
01656                                                                      CL**7
01657  4050-CONTINUE-DELETE.                                               CL*17
01658                                                                      CL**7
01659      PERFORM 7070-DELETE-ELTRLR THRU 7070-EXIT.                      CL*17
01660                                                                      CL**7
01661      IF WS-ADDR-TYPE EQUAL 'I'                                       CL**7
01662         SUBTRACT +1   FROM CL-INSURED-ADDR-CNT                       CL**7
01663      ELSE                                                            CL**7
01664      IF WS-ADDR-TYPE EQUAL 'B' AND                                   CL*20
01665         WS-ADDR-SEQ NOT EQUAL '9'                                    CL*20
01666         SUBTRACT +1   FROM CL-BENIF-ADDR-CNT                         CL**7
01667      ELSE                                                            CL**7
01668      IF WS-ADDR-TYPE EQUAL 'A' AND                                   CL**7
01669         WS-ADDR-SEQ NOT EQUAL '9'                                    CL**7
01670         SUBTRACT +1   FROM CL-ACCOUNT-ADDR-CNT                       CL**7
01671      ELSE                                                            CL**7
01672      IF WS-ADDR-TYPE EQUAL 'P'                                       CL**7
01673         SUBTRACT +1    FROM CL-DOCTOR-ADDR-CNT                       CL**7
01674      ELSE                                                            CL**7
01675      IF WS-ADDR-TYPE EQUAL 'E'                                       CL**7
01676         SUBTRACT +1    FROM CL-EMPLOYER-ADDR-CNT                     CL**7
01677      ELSE                                                            CL**7
01678      IF WS-ADDR-TYPE EQUAL 'O'                                       CL**7
01679         SUBTRACT +1    FROM CL-OTHER-1-ADDR-CNT                      CL**7
01680      ELSE                                                            CL**7
01681      IF WS-ADDR-TYPE EQUAL 'Q'                                       CL**7
01682         SUBTRACT +1    FROM CL-OTHER-2-ADDR-CNT.                     CL**7
01683                                                                      CL**7
01684      MOVE PI-PROCESSOR-ID            TO  CL-LAST-MAINT-USER.         CL*17
01685      MOVE '3'                        TO  CL-LAST-MAINT-TYPE.         CL*17
01686      MOVE EIBTIME                    TO  CL-LAST-MAINT-HHMMSS.       CL*17
01687      MOVE WS-CURR-DATE-BIN           TO  CL-LAST-MAINT-DT.           CL*17
01688                                                                      CL*17
01689      PERFORM 7010-REWRITE-ELMSTR THRU 7010-EXIT.                     CL*17
01690                                                                      CL*17
01691      MOVE SPACES                     TO  MAPNAMEO                    CL*17
01692                                          MADDRL1O                    CL*17
01693                                          MADDRL2O                    CL*17
01694                                          MCITYO
                                               MSTATEO
01695                                          MAPHONEO                    CL*17
01696                                          MZIPCODO                    CL*17
01697                                          MAPNOT1O                    CL*17
01698                                          MAPNOT2O                    CL*17
01699                                          MAPSEQ2O.                   CL*17
01700                                                                      CL**7
01701      MOVE ER-0000                    TO  EMI-ERROR.                  CL*17
01702      MOVE -1                         TO  MFMAINTL.                   CL*17
01703      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
01704                                                                      CL**7
01705      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.          CL*17
01706      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.             CL*17
01707      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.            CL*17
01708      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.             CL*17
01709                                                                      CL**7
01710      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.                        CL*17
01711                                                                      CL**4
01712      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*17
01713                                                                      CL*20
01714      MOVE 'D'                    TO  PI-PREV-MAINT-CODE.             CL*20
01715      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
01716                                                                   EL141
01717      EJECT                                                           CL*20
01718  5000-UPDATE-VIA-PF2.                                                CL*20
01719 *****                                                                CL*20
01720 * 5000 PARA FOR READING OF MASTER FOR CERT LEVEL TO BE CHANGED       CL*20
01721 *****                                                                CL*20
01722      MOVE +1                     TO  NDX.                            CL*20
01723      PERFORM 5900-CLEAR-AT-TABLE  THRU  5900-EXIT.                   CL*20
01724                                                                      CL*20
01725      MOVE PI-COMPANY-CD          TO  ELTRLR-COMPANY-CD.              CL*20
01726      MOVE PI-CARRIER             TO  ELTRLR-CARRIER.                 CL*20
01727      MOVE PI-CLAIM-NO            TO  ELTRLR-CLAIM-NO.                CL*20
01728      MOVE PI-CERT-NO             TO  ELTRLR-CERT-NO.                 CL*20
01729                                                                      CL*20
01730      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.                        CL*20
01731                                                                      CL*20
01732      MOVE CL-CONTROL-PRIMARY     TO  WS-SAVE-MSTR-KEY.               CL*20
01733      MOVE +1                     TO  ELTRLR-SEQUENCE-NO.             CL*20
01734      MOVE CL-ADDRESS-TRAILER-CNT TO  WS-ADDR-TRAILER-CNT.            CL*20
01735                                                                      CL*20
01736      IF WS-ADDR-TYPE EQUAL 'B'                                       CL*20
01737         ADD +10                  TO  ELTRLR-SEQUENCE-NO              CL*20
01738      ELSE                                                            CL*20
01739      IF WS-ADDR-TYPE EQUAL 'A'                                       CL*20
01740         ADD +20                  TO  ELTRLR-SEQUENCE-NO              CL*20
01741      ELSE                                                            CL*20
01742      IF WS-ADDR-TYPE EQUAL 'P'                                       CL*20
01743         ADD +30                  TO  ELTRLR-SEQUENCE-NO              CL*20
01744      ELSE                                                            CL*20
01745      IF WS-ADDR-TYPE EQUAL 'E'                                       CL*20
01746         ADD +40                  TO  ELTRLR-SEQUENCE-NO              CL*20
01747      ELSE                                                            CL*20
01748      IF WS-ADDR-TYPE EQUAL 'O'                                       CL*20
01749         ADD +50                  TO  ELTRLR-SEQUENCE-NO              CL*20
01750      ELSE                                                            CL*20
01751      IF WS-ADDR-TYPE EQUAL 'Q'                                       CL*20
01752         ADD +60                  TO  ELTRLR-SEQUENCE-NO.             CL*20
01753                                                                      CL*20
01754      MOVE +1                     TO  NDX.                            CL*20
01755                                                                      CL*20
01756      MOVE PI-COMPANY-CD          TO  ELTRLR-COMPANY-CD.              CL*20
01757      MOVE PI-CARRIER             TO  ELTRLR-CARRIER.                 CL*20
01758      MOVE PI-CLAIM-NO            TO  ELTRLR-CLAIM-NO.                CL*20
01759      MOVE PI-CERT-NO             TO  ELTRLR-CERT-NO.                 CL*20
01760                                                                      CL*20
01761  5100-READ-TRLR-TO-STORE.                                            CL*20
01762 *****                                                                CL*20
01763 * 5100 PARA TO READ TRAILER TO STORE TRAILERS OF CERT CHANGED        CL*20
01764 *****                                                                CL*20
01765                                                                      CL*20
01766      PERFORM 7090-START-ELTRLR THRU 7090-EXIT.                       CL*20
01767                                                                      CL*20
01768  5100-READ-LOOP.                                                     CL*20
01769                                                                      CL*20
01770      PERFORM 7095-READNEXT-ELTRLR THRU 7095-EXIT.                    CL*20
01771                                                                      CL*20
01772      IF AT-COMPANY-CD  =  PI-COMPANY-CD  AND                         CL*20
01773         AT-CARRIER     =  PI-CARRIER     AND                         CL*20
01774         AT-CLAIM-NO    =  PI-CLAIM-NO    AND                         CL*20
01775         AT-CERT-NO     =  PI-CERT-NO                                 CL*20
01776          NEXT SENTENCE                                               CL*20
01777      ELSE                                                            CL*20
01778          GO TO 5200-START-MSTR.                                      CL*20
01779                                                                      CL*20
01780      IF AT-ADDRESS-TYPE NOT = WS-ADDR-TYPE                           CL*20
01781          GO TO 5200-START-MSTR.                                      CL*20
01782                                                                      CL*20
01783      IF NOT ADDRESS-TR                                               CL*20
01784          GO TO 5100-READ-LOOP.                                       CL*20
01785                                                                      CL*20
01786      PERFORM 5800-BUILD-AT-TABLE THRU 5800-EXIT.                     CL*20
01787                                                                      CL*20
01788      ADD +1 TO NDX                                                   CL*20
01789      GO TO 5100-READ-LOOP.                                           CL*20
01790                                                                      CL*20
01791  5200-START-MSTR.                                                    CL*20
01792 *****                                                                CL*20
01793 * 5200 PARA FOR READING OF MASTERS FOR OTHER CERT LEVELS             CL*20
01794 *****                                                                CL*20
01795      EXEC CICS  ENDBR                                                CL*20
01796          DATASET  (ELTRLR-FILE-ID)                                   CL*20
01797      END-EXEC.                                                       CL*20
01798                                                                      CL*20
01799      MOVE 'N'                    TO  WS-REWRITE-MSTR-SW.             CL*20
01800                                                                      CL*20
01801      MOVE PI-COMPANY-CD          TO  ELTRLR-COMPANY-CD.              CL*20
01802      MOVE PI-CARRIER             TO  ELTRLR-CARRIER.                 CL*20
01803      MOVE PI-CLAIM-NO            TO  ELTRLR-CLAIM-NO.                CL*20
01804      MOVE LOW-VALUES             TO  ELTRLR-CERT-NO.                 CL*20
01805                                                                      CL*20
01806      PERFORM 7020-START-ELMSTR THRU 7020-EXIT.                       CL*20
01807                                                                      CL*20
01808  5200-READNEXT-ELMSTR.                                               CL*20
01809                                                                      CL*20
01810      PERFORM 7025-READNEXT-ELMSTR THRU 7025-EXIT.                    CL*20
01811                                                                      CL*20
01812      IF CL-CONTROL-PRIMARY = WS-SAVE-MSTR-KEY                        CL*20
01813          GO TO 5200-READNEXT-ELMSTR.                                 CL*20
01814                                                                      CL*20
01815      IF PI-COMPANY-CD  = ELTRLR-COMPANY-CD   AND                     CL*20
01816         PI-CARRIER     = ELTRLR-CARRIER      AND                     CL*20
01817         PI-CLAIM-NO    = ELTRLR-CLAIM-NO                             CL*20
01818          NEXT SENTENCE                                               CL*20
01819      ELSE                                                            CL*20
01820          GO TO 5200-END-MSTR-BROWSE.                                 CL*20
01821                                                                      CL*20
01822      IF WS-ADDR-TYPE EQUAL 'I'                                       CL*20
01823          MOVE CL-INSURED-ADDR-CNT                                    CL*20
01824                                  TO  WS-BEGIN-CNT                    CL*20
01825          MOVE ZEROS              TO  WS-INCREMENT-NO                 CL*20
01826      ELSE                                                            CL*20
01827          IF WS-ADDR-TYPE EQUAL 'B'                                   CL*20
01828              MOVE CL-BENIF-ADDR-CNT                                  CL*20
01829                                  TO  WS-BEGIN-CNT                    CL*20
01830              MOVE +10            TO  WS-INCREMENT-NO                 CL*20
01831          ELSE                                                        CL*20
01832              IF WS-ADDR-TYPE EQUAL 'A'                               CL*20
01833                  MOVE CL-ACCOUNT-ADDR-CNT                            CL*20
01834                                  TO  WS-BEGIN-CNT                    CL*20
01835                  MOVE +20        TO  WS-INCREMENT-NO                 CL*20
01836              ELSE                                                    CL*20
01837                  IF WS-ADDR-TYPE EQUAL 'P'                           CL*20
01838                      MOVE CL-DOCTOR-ADDR-CNT                         CL*20
01839                                  TO  WS-BEGIN-CNT                    CL*20
01840                      MOVE +30    TO  WS-INCREMENT-NO                 CL*20
01841                  ELSE                                                CL*20
01842                      IF WS-ADDR-TYPE EQUAL 'E'                       CL*20
01843                          MOVE CL-EMPLOYER-ADDR-CNT                   CL*20
01844                                  TO  WS-BEGIN-CNT                    CL*20
01845                          MOVE +40                                    CL*20
01846                                  TO  WS-INCREMENT-NO                 CL*20
01847                      ELSE                                            CL*20
01848                          IF WS-ADDR-TYPE EQUAL 'O'                   CL*20
01849                              MOVE CL-OTHER-1-ADDR-CNT                CL*20
01850                                  TO  WS-BEGIN-CNT                    CL*20
01851                              MOVE +50                                CL*20
01852                                  TO  WS-INCREMENT-NO                 CL*20
01853                          ELSE                                        CL*20
01854                              MOVE CL-OTHER-2-ADDR-CNT                CL*20
01855                                  TO  WS-BEGIN-CNT                    CL*20
01856                              MOVE +60                                CL*20
01857                                  TO  WS-INCREMENT-NO.                CL*20
01858                                                                      CL*20
01859      MOVE WS-BEGIN-CNT           TO  WS-NEW-SEQ.                     CL*20
01860                                                                      CL*20
01861      PERFORM 5500-TRLR-UPDATE  THRU  5500-EXIT.                      CL*20
01862                                                                      CL*20
01863      IF REWRITE-MSTR                                                 CL*20
01864          MOVE CL-CONTROL-PRIMARY TO  WS-START-MSTR-KEY               CL*20
01865          EXEC CICS  ENDBR                                            CL*20
01866              DATASET  (ELMSTR-FILE-ID)                               CL*20
01867          END-EXEC                                                    CL*20
01868          PERFORM 7005-READ-ELMSTR-UPDATE  THRU  7005-EXIT            CL*20
01869          PERFORM 5300-MOVE-NEW-CNT  THRU  5300-EXIT                  CL*20
01870          PERFORM 7010-REWRITE-ELMSTR  THRU  7010-EXIT                CL*20
01871          MOVE WS-START-MSTR-KEY  TO  ELMSTR-KEY                      CL*20
01872          PERFORM 7020-START-ELMSTR THRU 7020-EXIT                    CL*20
01873          PERFORM 7025-READNEXT-ELMSTR THRU 7025-EXIT                 CL*20
01874          MOVE 'N'                TO  WS-REWRITE-MSTR-SW.             CL*20
01875                                                                      CL*20
01876      GO TO 5200-READNEXT-ELMSTR.                                     CL*20
01877                                                                      CL*20
01878  5200-END-MSTR-BROWSE.                                               CL*20
01879      EXEC CICS  ENDBR                                                CL*20
01880          DATASET  (ELMSTR-FILE-ID)                                   CL*20
01881      END-EXEC.                                                       CL*20
01882                                                                      CL*20
01883      MOVE ER-0000                TO  EMI-ERROR                       CL*20
01884      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*20
01885                                                                      CL*20
01886      MOVE -1                     TO  MFMAINTL.                       CL*20
01887                                                                      CL*20
01888      IF WS-ADDR-TYPE EQUAL 'I'                                       CL*20
01889          MOVE AL-PABOF           TO  MTYPE1A                         CL*20
01890      ELSE                                                            CL*20
01891          IF WS-ADDR-TYPE EQUAL 'B'                                   CL*20
01892              MOVE AL-PABOF       TO  MTYPE2A                         CL*20
01893          ELSE                                                        CL*20
01894              IF WS-ADDR-TYPE EQUAL 'A'                               CL*20
01895                  MOVE AL-PABOF   TO  MTYPE3A                         CL*20
01896              ELSE                                                    CL*20
01897                  IF WS-ADDR-TYPE EQUAL 'P'                           CL*20
01898                      MOVE AL-PABOF                                   CL*20
01899                                  TO  MTYPE4A                         CL*20
01900                  ELSE                                                CL*20
01901                       IF WS-ADDR-TYPE EQUAL 'E'                      CL*20
01902                           MOVE AL-PABOF                              CL*20
01903                                  TO  MTYPE5A                         CL*20
01904                       ELSE                                           CL*20
01905                           IF WS-ADDR-TYPE EQUAL 'O'                  CL*20
01906                               MOVE AL-PABOF                          CL*20
01907                                  TO  MTYPE6A                         CL*20
01908                           ELSE                                       CL*20
01909                               MOVE AL-PABOF                          CL*20
01910                                  TO  MTYPE7A.                        CL*20
01911                                                                      CL*20
01912      MOVE WS-ADDR-I-CNT          TO  ICNTO.                          CL*20
01913      MOVE WS-ADDR-B-CNT          TO  BCNTO.                          CL*20
01914      MOVE WS-ADDR-A-CNT          TO  ACNTO.                          CL*20
01915      MOVE WS-ADDR-P-CNT          TO  PCNTO.                          CL*20
01916      MOVE WS-ADDR-E-CNT          TO  ECNTO.                          CL*20
01917      MOVE WS-ADDR-O-CNT          TO  OCNTO.                          CL*20
01918      MOVE WS-ADDR-Q-CNT          TO  QCNTO.                          CL*20
01919                                                                      CL*20
01920      GO TO 8100-SEND-INITIAL-MAP.                                    CL*20
01921                                                                      CL*20
01922      EJECT                                                           CL*20
01923                                                                      CL*20
01924  5300-MOVE-NEW-CNT.                                                  CL*20
01925                                                                      CL*20
01926      IF WS-ADDR-TYPE EQUAL 'I'                                       CL*20
01927          MOVE WS-NEW-SEQ        TO CL-INSURED-ADDR-CNT               CL*20
01928      ELSE                                                            CL*20
01929          IF WS-ADDR-TYPE EQUAL 'B'                                   CL*20
01930              MOVE WS-NEW-SEQ                                         CL*20
01931                                 TO  CL-BENIF-ADDR-CNT                CL*20
01932          ELSE                                                        CL*20
01933              IF WS-ADDR-TYPE EQUAL 'A'                               CL*20
01934                  MOVE WS-NEW-SEQ                                     CL*20
01935                                 TO  CL-ACCOUNT-ADDR-CNT              CL*20
01936              ELSE                                                    CL*20
01937                  IF WS-ADDR-TYPE EQUAL 'P'                           CL*20
01938                      MOVE WS-NEW-SEQ                                 CL*20
01939                                 TO  CL-DOCTOR-ADDR-CNT               CL*20
01940                  ELSE                                                CL*20
01941                      IF WS-ADDR-TYPE EQUAL 'E'                       CL*20
01942                          MOVE WS-NEW-SEQ                             CL*20
01943                                 TO  CL-EMPLOYER-ADDR-CNT             CL*20
01944                      ELSE                                            CL*20
01945                          IF WS-ADDR-TYPE EQUAL 'O'                   CL*20
01946                              MOVE WS-NEW-SEQ                         CL*20
01947                                 TO  CL-OTHER-1-ADDR-CNT              CL*20
01948                          ELSE                                        CL*20
01949                              MOVE WS-NEW-SEQ                         CL*20
01950                                 TO  CL-OTHER-2-ADDR-CNT.             CL*20
01951                                                                      CL*20
01952  5300-EXIT.                                                          CL*20
01953       EXIT.                                                          CL*20
01954      EJECT                                                           CL*20
01955                                                                      CL*20
01956  5500-TRLR-UPDATE.                                                   CL*20
01957 *****                                                                CL*20
01958 * 5500 PARA FOR UPDATING OF TRAILERS FOR OTHER CERT LEVELS           CL*20
01959 *****                                                                CL*20
01960                                                                      CL*20
01961      MOVE +1                     TO  WS-WORK-SEQ.                    CL*20
01962      ADD WS-INCREMENT-NO  TO  WS-WORK-SEQ.                           CL*20
01963      MOVE +1                     TO  NDX.                            CL*20
01964                                                                      CL*20
01965      MOVE CL-COMPANY-CD          TO  ELTRLR-COMPANY-CD.              CL*20
01966      MOVE CL-CARRIER             TO  ELTRLR-CARRIER.                 CL*20
01967      MOVE CL-CLAIM-NO            TO  ELTRLR-CLAIM-NO.                CL*20
01968      MOVE CL-CERT-NO             TO  ELTRLR-CERT-NO.                 CL*20
01969                                                                      CL*20
01970  5500-TRLR-UPDATE-LOOP.                                              CL*20
01971                                                                      CL*20
01972      IF NDX GREATER THAN WS-BEGIN-CNT                                CL*20
01973          IF AT-SAVE-STATUS (NDX) = 'Y'                               CL*20
01974              PERFORM 5600-ADD-TRAILER  THRU  5600-EXIT               CL*20
01975              GO TO 5500-CHECK-END                                    CL*20
01976          ELSE                                                        CL*20
01977              MOVE +9             TO  NDX                             CL*20
01978              GO TO 5500-CHECK-END.                                   CL*20
01979                                                                      CL*20
01980      IF (NDX NOT GREATER THAN WS-BEGIN-CNT) AND                      CL*20
01981         (AT-SAVE-STATUS (NDX) NOT = 'Y')                             CL*20
01982          PERFORM 5700-DELETE-TRAILER  THRU  5700-EXIT                CL*20
01983          GO TO 5500-CHECK-END.                                       CL*20
01984                                                                      CL*20
01985      MOVE WS-WORK-SEQ            TO  ELTRLR-SEQUENCE-NO.             CL*20
01986                                                                      CL*20
01987      PERFORM 7055-READ-ELTRLR-UPDATE THRU 7055-EXIT.                 CL*20
01988                                                                      CL*20
01989      IF AT-SAVE-STATUS (NDX) = 'Y'                                   CL*20
01990          PERFORM 5650-MOVE-SAVED  THRU  5650-EXIT                    CL*20
01991          PERFORM 7060-REWRITE-ELTRLR  THRU  7060-EXIT                CL*20
01992          ADD +1 TO WS-WORK-SEQ                                       CL*20
01993          GO TO 5500-CHECK-END.                                       CL*20
01994                                                                      CL*20
01995      PERFORM 7070-DELETE-ELTRLR  THRU  7070-EXIT.                    CL*20
01996                                                                      CL*20
01997      MOVE 'Y'                    TO  WS-REWRITE-MSTR-SW.             CL*20
01998      ADD +1 TO WS-WORK-SEQ.                                          CL*20
01999                                                                      CL*20
02000  5500-CHECK-END.                                                     CL*20
02001                                                                      CL*20
02002      IF NDX NOT = +9                                                 CL*20
02003          ADD +1 TO NDX                                               CL*20
02004          GO TO 5500-TRLR-UPDATE-LOOP.                                CL*20
02005                                                                      CL*20
02006  5500-EXIT.                                                          CL*20
02007       EXIT.                                                          CL*20
02008      EJECT                                                           CL*20
02009                                                                      CL*20
02010  5600-ADD-TRAILER.                                                   CL*20
02011                                                                      CL*20
02012      PERFORM 7080-GETMAIN-ELTRLR  THRU  7080-EXIT.                   CL*20
02013                                                                      CL*20
02014      PERFORM 5650-MOVE-SAVED  THRU  5650-EXIT.                       CL*20
02015                                                                      CL*20
02016      MOVE AT-CONTROL-PRIMARY     TO  ELTRLR-KEY.                     CL*20
02017                                                                      CL*20
02018      PERFORM 7065-WRITE-ELTRLR  THRU  7065-EXIT.                     CL*20
02019                                                                      CL*20
02020      MOVE 'Y'                    TO  WS-REWRITE-MSTR-SW.             CL*20
02021      ADD +1 TO WS-WORK-SEQ.                                          CL*20
02022      ADD +1 TO WS-NEW-SEQ.                                           CL*20
02023                                                                      CL*20
02024  5600-EXIT.                                                          CL*20
02025       EXIT.                                                          CL*20
02026  5650-MOVE-SAVED.                                                    CL*20
02027                                                                      CL*20
02028      MOVE 'AT'                   TO  AT-RECORD-ID.                   CL*20
02029      MOVE '5'                    TO  AT-TRAILER-TYPE.                CL*20
02030      MOVE AT-SAVE-COMPANY-CD (NDX)                                   CL*20
02031                                  TO  AT-COMPANY-CD.                  CL*20
02032      MOVE AT-SAVE-CARRIER (NDX)  TO  AT-CARRIER.                     CL*20
02033      MOVE AT-SAVE-CLAIM-NO (NDX) TO  AT-CLAIM-NO.                    CL*20
02034      MOVE CL-CERT-NO             TO  AT-CERT-NO.                     CL*20
02035      MOVE WS-WORK-SEQ            TO  AT-SEQUENCE-NO.                 CL*20
02036      MOVE AT-SAVE-RECORDED-DT (NDX)                                  CL*20
02037                                  TO  AT-RECORDED-DT.                 CL*20
02038      MOVE AT-SAVE-RECORDED-BY (NDX)                                  CL*20
02039                                  TO  AT-RECORDED-BY.                 CL*20
02040      MOVE AT-SAVE-LAST-MAINT-HHMMSS (NDX)                            CL*20
02041                                  TO  AT-LAST-MAINT-HHMMSS.           CL*20
02042      MOVE AT-SAVE-ADDRESS-TYPE (NDX)                                 CL*20
02043                                  TO  AT-ADDRESS-TYPE.                CL*20
02044      MOVE AT-SAVE-MAIL-TO-NAME (NDX)                                 CL*20
02045                                  TO  AT-MAIL-TO-NAME.                CL*20
02046      MOVE AT-SAVE-ADDRESS-LINE-1 (NDX)                               CL*20
02047                                  TO  AT-ADDRESS-LINE-1.              CL*20
02048      MOVE AT-SAVE-ADDRESS-LINE-2 (NDX)                               CL*20
02049                                  TO  AT-ADDRESS-LINE-2.              CL*20
02050      MOVE AT-SAVE-CITY (NDX)     TO  AT-CITY
           MOVE AT-SAVE-STATE (NDX)    TO  AT-STATE
02052      MOVE AT-SAVE-ZIP (NDX)      TO  AT-ZIP.                         CL*20
02053      MOVE AT-SAVE-PHONE-NO (NDX) TO  AT-PHONE-NO.                    CL*20
02054      MOVE AT-SAVE-ADDRESS-LAST-MAINT-DT (NDX)                        CL*20
02055                                  TO  AT-ADDRESS-LAST-MAINT-DT.       CL*20
02056      MOVE AT-SAVE-ADDRESS-LST-UPDATED-BY (NDX)                       CL*20
02057                                  TO  AT-ADDRESS-LAST-UPDATED-BY.     CL*20
061511     MOVE AT-SAVE-VFY-2ND-BENE-SSN (NDX)
061511                                 TO  AT-VFY-2ND-BENE-SSN.
061511     MOVE AT-SAVE-VFY-2ND-BENE-VERIFIED (NDX)
061511                                 TO  AT-VFY-2ND-BENE-VERIFIED.
02058                                                                      CL*20
02059  5650-EXIT.                                                          CL*20
02060       EXIT.                                                          CL*20
02061      EJECT                                                           CL*20
02062                                                                      CL*20
02063  5700-DELETE-TRAILER.                                                CL*20
02064                                                                      CL*20
02065      MOVE CL-COMPANY-CD          TO  ELTRLR-COMPANY-CD.              CL*20
02066      MOVE CL-CARRIER             TO  ELTRLR-CARRIER.                 CL*20
02067      MOVE CL-CLAIM-NO            TO  ELTRLR-CLAIM-NO.                CL*20
02068      MOVE CL-CERT-NO             TO  ELTRLR-CERT-NO.                 CL*20
02069      MOVE WS-WORK-SEQ            TO  ELTRLR-SEQUENCE-NO.             CL*20
02070                                                                      CL*20
02071      PERFORM 7055-READ-ELTRLR-UPDATE THRU 7055-EXIT.                 CL*20
02072                                                                      CL*20
02073      PERFORM 7070-DELETE-ELTRLR THRU 7070-EXIT.                      CL*20
02074                                                                      CL*20
02075      MOVE 'Y'                    TO  WS-REWRITE-MSTR-SW.             CL*20
02076      SUBTRACT +1 FROM WS-NEW-SEQ.                                    CL*20
02077      ADD +1 TO WS-WORK-SEQ.                                          CL*20
02078                                                                      CL*20
02079  5700-EXIT.                                                          CL*20
02080       EXIT.                                                          CL*20
02081      EJECT                                                           CL*20
02082                                                                      CL*20
02083  5800-BUILD-AT-TABLE.                                                CL*20
02084                                                                      CL*20
02085      MOVE AT-COMPANY-CD          TO  AT-SAVE-COMPANY-CD (NDX).       CL*20
02086      MOVE AT-CARRIER             TO  AT-SAVE-CARRIER (NDX).          CL*20
02087      MOVE AT-CLAIM-NO            TO  AT-SAVE-CLAIM-NO (NDX).         CL*20
02088      MOVE AT-CERT-NO             TO  AT-SAVE-CERT-NO (NDX).          CL*20
02089      MOVE AT-SEQUENCE-NO         TO  AT-SAVE-SEQUENCE-NO (NDX).      CL*20
02090      MOVE AT-TRAILER-TYPE        TO  AT-SAVE-TRAILER-TYPE (NDX).     CL*20
02091      MOVE AT-RECORDED-DT         TO  AT-SAVE-RECORDED-DT (NDX).      CL*20
02092      MOVE AT-RECORDED-BY         TO  AT-SAVE-RECORDED-BY (NDX).      CL*20
02093      MOVE AT-LAST-MAINT-HHMMSS                                       CL*20
02094                           TO  AT-SAVE-LAST-MAINT-HHMMSS (NDX).       CL*20
02095      MOVE AT-ADDRESS-TYPE        TO  AT-SAVE-ADDRESS-TYPE (NDX).     CL*20
02096      MOVE AT-MAIL-TO-NAME        TO  AT-SAVE-MAIL-TO-NAME (NDX).     CL*20
02097      MOVE AT-ADDRESS-LINE-1      TO  AT-SAVE-ADDRESS-LINE-1 (NDX).   CL*20
02098      MOVE AT-ADDRESS-LINE-2      TO  AT-SAVE-ADDRESS-LINE-2 (NDX).   CL*20
02099      MOVE AT-CITY                TO  AT-SAVE-CITY (NDX)
           MOVE AT-STATE               TO  AT-SAVE-STATE (NDX)
02100      MOVE AT-ZIP                 TO  AT-SAVE-ZIP (NDX).              CL*20
02101      MOVE AT-PHONE-NO            TO  AT-SAVE-PHONE-NO (NDX).         CL*20
02102      MOVE AT-ADDRESS-LAST-MAINT-DT                                   CL*20
02103                           TO  AT-SAVE-ADDRESS-LAST-MAINT-DT (NDX).   CL*20
02104      MOVE AT-ADDRESS-LAST-UPDATED-BY                                 CL*20
02105                           TO  AT-SAVE-ADDRESS-LST-UPDATED-BY (NDX)   CL*20
02106      MOVE AT-CERT-NO             TO  AT-SAVE-CERT-NO (NDX).          CL*20
02107      MOVE AT-SEQUENCE-NO         TO  AT-SAVE-SEQUENCE-NO (NDX).      CL*20
02108                                                                      CL*20
02109      IF AT-SAVE-MAIL-TO-NAME (NDX)   = SPACES AND                    CL*20
02110         AT-SAVE-ADDRESS-LINE-1 (NDX) = SPACES AND                    CL*20
02111         AT-SAVE-ADDRESS-LINE-2 (NDX) = SPACES AND                    CL*20
02112         AT-SAVE-CITY (NDX)           = SPACES AND
              AT-SAVE-STATE (NDX)          = SPACES
02113          MOVE 'N'                TO  AT-SAVE-STATUS (NDX)            CL*20
02114      ELSE                                                            CL*20
02115          MOVE 'Y'                TO  AT-SAVE-STATUS (NDX).           CL*20
061511     MOVE AT-VFY-2ND-BENE-SSN TO AT-SAVE-VFY-2ND-BENE-SSN (NDX).
061511     MOVE AT-VFY-2ND-BENE-VERIFIED 
061511                       TO AT-SAVE-VFY-2ND-BENE-VERIFIED (NDX).
02116  5800-EXIT.                                                          CL*20
02117       EXIT.                                                          CL*20
02118      EJECT                                                           CL*20
02119  5900-CLEAR-AT-TABLE.                                                CL*20
02120                                                                      CL*20
02121      MOVE SPACES                 TO  AT-SAVE-CONTROL-PRIMARY (NDX)   CL*20
02122      MOVE ZEROS                  TO  AT-SAVE-SEQUENCE-NO (NDX).      CL*20
02123      MOVE SPACES                 TO  AT-SAVE-TRAILER-TYPE (NDX).     CL*20
02124      MOVE LOW-VALUES             TO  AT-SAVE-RECORDED-DT (NDX).      CL*20
02125      MOVE LOW-VALUES             TO  AT-SAVE-RECORDED-BY (NDX).      CL*20
02126      MOVE ZEROS                                                      CL*20
02127                           TO  AT-SAVE-LAST-MAINT-HHMMSS (NDX).       CL*20
02128      MOVE SPACES                 TO  AT-SAVE-ADDRESS-TYPE (NDX).     CL*20
02129      MOVE SPACES                 TO  AT-SAVE-MAIL-TO-NAME (NDX).     CL*20
02130      MOVE SPACES                 TO  AT-SAVE-ADDRESS-LINE-1 (NDX).   CL*20
02131      MOVE SPACES                 TO  AT-SAVE-ADDRESS-LINE-2 (NDX).   CL*20
02132      MOVE SPACES                 TO  AT-SAVE-CITY (NDX)
                                           AT-SAVE-STATE (NDX)
02133      MOVE SPACES                 TO  AT-SAVE-ZIP (NDX).              CL*20
02134      MOVE ZEROS                  TO  AT-SAVE-PHONE-NO (NDX).         CL*20
02135      MOVE LOW-VALUES                                                 CL*20
02136                           TO  AT-SAVE-ADDRESS-LAST-MAINT-DT (NDX).   CL*20
02137      MOVE SPACES                                                     CL*20
02138                           TO  AT-SAVE-ADDRESS-LST-UPDATED-BY (NDX)   CL*20
02139      MOVE SPACES                 TO  AT-SAVE-STATUS (NDX).           CL*20
061511     MOVE SPACES          TO  AT-SAVE-VFY-2ND-BENE-SSN (NDX).
061511     MOVE SPACES          TO  AT-SAVE-VFY-2ND-BENE-VERIFIED (NDX).
02140                                                                      CL*20
02141      IF NDX = +9                                                     CL*20
02142          MOVE +1                 TO NDX                              CL*20
02143          GO TO 5900-EXIT                                             CL*20
02144      ELSE                                                            CL*20
02145          ADD +1 TO  NDX                                              CL*20
02146          GO TO 5900-CLEAR-AT-TABLE.                                  CL*20
02147                                                                      CL*20
02148  5900-EXIT.                                                          CL*20
02149       EXIT.                                                          CL*20
02150      EJECT                                                           CL*20
02151                                                                      CL*20
02152  5999-EXIT. EXIT.                                                    CL*20
02153      EJECT                                                        EL141
02154  6000-EDIT-SCREEN.                                                   CL*17
02155                                                                      CL**4
02156      IF MAPNAMEI = LOW-VALUES OR SPACES                           EL141
02157         MOVE ER-0315                 TO  EMI-ERROR                   CL*17
02158         MOVE -1                      TO  MFMAINTL                    CL*17
02159         MOVE AL-UABON                TO  MFMAINTA                    CL*17
02160         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL*17
02161         GO TO 6000-EXIT.                                             CL*17
02162                                                                   EL141
02163      MOVE MAPHONEI                   TO  WS-PHONE.                   CL*17
02164      EXEC CICS BIF DEEDIT                                            CL*17
02165          FIELD (WS-PHONE)                                            CL*17
02166          LENGTH (12)                                                 CL*17
02167      END-EXEC.                                                    EL141
02168                                                                   EL141
02169      IF WS-PHONE  NOT NUMERIC                                     EL141
02170         MOVE ER-0053                 TO  EMI-ERROR                   CL*17
02171         MOVE -1                      TO  MAPHONEL                    CL*17
02172         MOVE AL-UNBON                TO  MAPHONEA                    CL*17
02173         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    CL*17
02174                                                                   EL141
02175      IF MZIPCODL = ZEROS                                          EL141
02176         MOVE ER-0052                 TO  EMI-ERROR                   CL*17
02177         MOVE -1                      TO  MZIPCODL                    CL*17
02178         MOVE AL-UABON                TO  MZIPCODA                    CL*17
02179         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    CL*17
02180                                                                      CL**4
02181      IF ADD-REQUIRED                                                 CL**4
02182         IF ((WS-ADDR-TYPE EQUAL 'I') AND                             CL**4
02183            ((CL-INSURED-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))    CL**4
02184            OR                                                        CL**4
02185            ((WS-ADDR-TYPE EQUAL 'A') AND                             CL**4
02186            ((CL-ACCOUNT-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM)     CL**7
02187                AND                                                   CL**7
02188             (WS-ADDR-SEQ NOT EQUAL '9'))                             CL**7
02189            OR                                                        CL**4
02190            ((WS-ADDR-TYPE EQUAL 'B') AND                             CL**4
02191            ((CL-BENIF-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM)       CL*20
02192                AND                                                   CL*20
02193             (WS-ADDR-SEQ NOT EQUAL '9'))                             CL*20
02194            OR                                                        CL**4
02195            ((WS-ADDR-TYPE EQUAL 'E') AND                             CL**4
02196            ((CL-EMPLOYER-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))   CL**4
02197            OR                                                        CL**4
02198            ((WS-ADDR-TYPE EQUAL 'P') AND                             CL**4
02199            ((CL-DOCTOR-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))     CL**4
02200            OR                                                        CL**4
02201            ((WS-ADDR-TYPE EQUAL 'O') AND                             CL**4
02202            ((CL-OTHER-1-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))    CL**4
02203            OR                                                        CL**4
02204            ((WS-ADDR-TYPE EQUAL 'Q') AND                             CL**4
02205            ((CL-OTHER-2-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))    CL**4
02206              MOVE ER-1878            TO  EMI-ERROR                   CL*17
02207              MOVE -1                 TO  MADDRTPL                    CL*17
02208              MOVE AL-UABON           TO  MADDRTPA                    CL*17
02209              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*17
02210                                                                      CL*21
02211      IF PI-COMPANY-ID NOT = 'DMD'                                    CL*21
02212          GO TO 6000-EDIT-CONTINUE
           END-IF
02213                                                                      CL*21
02214      IF MCITYL GREATER ZERO
              AND MSTATEL > 0
02215         AND MZIPCODL GREATER ZERO
02216 *DLO004                                                              CL*21
               MOVE SPACES TO DL04-ADDR-LINE
               STRING MCITYI ' ' MSTATEI DELIMITED BY '  '
                  INTO DL04-ADDR-LINE
               END-STRING
02218          MOVE MZIPCODI           TO DL04-ZIP-CODE                    CL*21
02219          EXEC CICS LINK                                              CL*21
02220              PROGRAM    ('DLO004')                                   CL*21
02221              COMMAREA   (WS-DLO-VALIDATE-STATE-ZIP)                  CL*21
02222              LENGTH     (DL04-COMM-LENGTH)                           CL*21
02223          END-EXEC                                                    CL*21
02224          IF DL04-RETURN-CODE = 'OK'                                  CL*21
02225              MOVE AL-UANON       TO MCITYA
                                          MSTATEA
02226                                     MZIPCODA                         CL*21
02227           ELSE                                                       CL*21
02228              MOVE AL-UABON       TO MCITYA
                                          MSTATEA
02229              MOVE -1             TO MCITYL
02230              IF DL04-RETURN-CODE = '01'                              CL*22
02231                  MOVE ER-0853            TO EMI-ERROR                CL*22
02232                  PERFORM 9900-ERROR-FORMAT                           CL*22
02233                ELSE                                                  CL*22
02234              IF DL04-RETURN-CODE = '02'                              CL*22
02235                  MOVE ER-0854            TO EMI-ERROR                CL*22
02236                  PERFORM 9900-ERROR-FORMAT                           CL*22
02237                ELSE                                                  CL*22
02238              IF DL04-RETURN-CODE = '03'                              CL*22
02239                  MOVE ER-0855            TO EMI-ERROR                CL*22
02240                  PERFORM 9900-ERROR-FORMAT                           CL*22
02241                ELSE                                                  CL*22
02242              IF DL04-RETURN-CODE = '04'                              CL*22
02243                  MOVE ER-0856            TO EMI-ERROR                CL*22
02244                  PERFORM 9900-ERROR-FORMAT                           CL*22
02245                ELSE                                                  CL*22
02246              IF DL04-RETURN-CODE = '05'                              CL*22
02247                  MOVE ER-0857            TO EMI-ERROR                CL*22
02248                  PERFORM 9900-ERROR-FORMAT                           CL*22
02249                ELSE                                                  CL*22
02250              IF DL04-RETURN-CODE = '06'                              CL*22
02251                  MOVE ER-0858            TO EMI-ERROR                CL*22
02252                  PERFORM 9900-ERROR-FORMAT                           CL*22
02253                ELSE                                                  CL*22
02254              IF DL04-RETURN-CODE = '07'                              CL*22
02255                  MOVE ER-0859            TO EMI-ERROR                CL*22
02256                  PERFORM 9900-ERROR-FORMAT                           CL*22
02257                ELSE                                                  CL*22
02258              IF DL04-RETURN-CODE = '08'                              CL*22
02259                  MOVE ER-0860            TO EMI-ERROR                CL*22
02260                  PERFORM 9900-ERROR-FORMAT                           CL*23
02261                ELSE                                                  CL*23
02262              IF DL04-RETURN-CODE = 'N1'                              CL*23
02263                  MOVE ER-0862            TO EMI-ERROR                CL*23
02264                  PERFORM 9900-ERROR-FORMAT                           CL*23
02265                ELSE                                                  CL*23
02266              IF DL04-RETURN-CODE = 'E1'                              CL*23
02267                  MOVE ER-0863            TO EMI-ERROR                CL*23
02268                  PERFORM 9900-ERROR-FORMAT                           CL*22
02269                ELSE                                                  CL*22
02270                  MOVE ER-0886            TO EMI-ERROR                CL*22
02271                  PERFORM 9900-ERROR-FORMAT.                          CL*22
           .
       6000-EDIT-CONTINUE.

           IF MSTATEL > 0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE MSTATEI             TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              EXEC CICS READ
                 DATASET   (FILE-ID-ELCNTL)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO MSTATEL
                 MOVE AL-UABON         TO MSTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           ELSE
              IF MFMAINTI = 'A'
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO MSTATEL
                 MOVE AL-UABON         TO MSTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

061511     IF BENEVFYL > ZERO
061511        IF BENEVFYI NOT EQUAL 'Y' AND 'N' AND SPACES
032514            MOVE ER-7584         TO EMI-ERROR
061511            MOVE -1              TO BENEVFYL
061511            MOVE AL-UABON        TO BENEVFYA
061511            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061511        END-IF
061511     END-IF.
061511
           .
02273  6000-EXIT.                                                          CL*17
02274       EXIT.                                                       EL141
02275                                                                   EL141
02276      EJECT                                                        EL141
02277  6500-HIGHLIGHT-EXISTING.                                            CL*17
02278                                                                      CL*20
02279      IF CL-INSURED-ADDR-CNT NOT NUMERIC                              CL*20
02280         MOVE ZEROS            TO CL-INSURED-ADDR-CNT.                CL*20
02281      IF CL-BENIF-ADDR-CNT   NOT NUMERIC                              CL*20
02282         MOVE ZEROS            TO CL-BENIF-ADDR-CNT.                  CL*20
02283      IF CL-ACCOUNT-ADDR-CNT NOT NUMERIC                              CL*20
02284         MOVE ZEROS            TO CL-ACCOUNT-ADDR-CNT.                CL*20
02285      IF CL-DOCTOR-ADDR-CNT  NOT NUMERIC                              CL*20
02286         MOVE ZEROS            TO CL-DOCTOR-ADDR-CNT.                 CL*20
02287      IF CL-EMPLOYER-ADDR-CNT  NOT NUMERIC                            CL*20
02288         MOVE ZEROS            TO CL-EMPLOYER-ADDR-CNT.               CL*20
02289      IF CL-OTHER-1-ADDR-CNT NOT NUMERIC                              CL*20
02290         MOVE ZEROS            TO CL-OTHER-1-ADDR-CNT.                CL*20
02291      IF CL-OTHER-2-ADDR-CNT NOT NUMERIC                              CL*20
02292         MOVE ZEROS            TO CL-OTHER-2-ADDR-CNT.                CL*20
02293                                                                   EL141
02294      IF CL-INSURED-ADDR-CNT IS NOT EQUAL TO +0                       CL*17
02295          MOVE AL-PABOF               TO  MTYPE1A                     CL*17
02296      ELSE                                                            CL*17
02297          MOVE AL-PANOF               TO  MTYPE1A.                    CL*17
02298                                                                   EL141
02299      IF CL-BENIF-ADDR-CNT IS NOT EQUAL TO +0                         CL*17
02300          MOVE AL-PABOF               TO  MTYPE2A                     CL*17
02301      ELSE                                                            CL*17
02302          MOVE AL-PANOF               TO  MTYPE2A.                    CL*17
02303                                                                      CL**4
02304      IF CL-ACCOUNT-ADDR-CNT IS NOT EQUAL TO +0                       CL*17
02305          MOVE AL-PABOF               TO  MTYPE3A                     CL*17
02306      ELSE                                                            CL*17
02307          MOVE AL-PANOF               TO  MTYPE3A.                    CL*17
02308                                                                      CL**4
02309      IF CL-DOCTOR-ADDR-CNT IS NOT EQUAL TO +0                        CL*17
02310          MOVE AL-PABOF               TO  MTYPE4A                     CL*17
02311      ELSE                                                            CL*17
02312          MOVE AL-PANOF               TO  MTYPE4A.                    CL*17
02313                                                                   EL141
02314      IF CL-EMPLOYER-ADDR-CNT IS NOT EQUAL TO +0                      CL*17
02315          MOVE AL-PABOF               TO  MTYPE5A                     CL*17
02316      ELSE                                                            CL*17
02317          MOVE AL-PANOF               TO  MTYPE5A.                    CL*17
02318                                                                   EL141
02319      IF CL-OTHER-1-ADDR-CNT IS NOT EQUAL TO +0                       CL*17
02320          MOVE AL-PABOF               TO  MTYPE6A                     CL*17
02321      ELSE                                                            CL*17
02322          MOVE AL-PANOF               TO  MTYPE6A.                    CL*17
02323                                                                   EL141
02324      IF CL-OTHER-2-ADDR-CNT IS NOT EQUAL TO +0                       CL*17
02325          MOVE AL-PABOF               TO  MTYPE7A                     CL*17
02326      ELSE                                                            CL**4
02327          MOVE AL-PANOF               TO  MTYPE7A.                    CL*17
02328                                                                   EL141
02329      MOVE CL-INSURED-ADDR-CNT        TO  ICNTO.                      CL*17
02330      MOVE CL-BENIF-ADDR-CNT          TO  BCNTO.                      CL*17
02331      MOVE CL-ACCOUNT-ADDR-CNT        TO  ACNTO.                      CL*17
02332      MOVE CL-DOCTOR-ADDR-CNT         TO  PCNTO.                      CL*17
02333      MOVE CL-EMPLOYER-ADDR-CNT       TO  ECNTO.                      CL*17
02334      MOVE CL-OTHER-1-ADDR-CNT        TO  OCNTO.                      CL*17
02335      MOVE CL-OTHER-2-ADDR-CNT        TO  QCNTO.                      CL*17
061511
032514     IF PI-ST-VFY-2ND-BENE = 'L' OR 'A' OR 'B'
061511         MOVE AL-SANOF               TO LBLSSNA
061511         MOVE AL-UANOF               TO BENESSNA
061511         MOVE AL-SANOF               TO LBLVFYA
100713         IF PI-APPROVAL-LEVEL = '4' OR '5'
061511            MOVE AL-UANOF            TO BENEVFYA
061511         ELSE
061511            MOVE AL-SANOF            TO BENEVFYA
061511         END-IF
061511     ELSE
061511         MOVE AL-SADOF               TO LBLSSNA
061511         MOVE AL-SADOF               TO BENESSNA
061511         MOVE AL-SADOF               TO LBLVFYA
061511         MOVE AL-SADOF               TO BENEVFYA
061511     END-IF.
02336                                                                   EL141
02337  6500-EXIT.                                                          CL*17
02338      EXIT.                                                           CL*17
02339                                                                      CL*13
02340      EJECT                                                           CL*17
061511
061511 6700-GET-VFY-BENE-IND.
061511       MOVE SPACES              TO ELCNTL-KEY
061511       MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
061511       MOVE '3'                 TO ELCNTL-REC-TYPE
061511       MOVE PI-STATE            TO ELCNTL-ACCESS
061511       MOVE +0                  TO ELCNTL-SEQ
061511       EXEC CICS READ
061511          DATASET   (FILE-ID-ELCNTL)
061511          SET       (ADDRESS OF CONTROL-FILE)
061511          RIDFLD    (ELCNTL-KEY)
061511          RESP      (WS-RESPONSE)
061511       END-EXEC
061511       IF RESP-NORMAL
061511          CONTINUE
061511       ELSE
061511          MOVE ER-1563          TO EMI-ERROR
061511          PERFORM 9900-ERROR-FORMAT
061511                                THRU 9900-EXIT
061511          GO TO 6700-EXIT
061511       END-IF.
061511
061511       MOVE CF-ST-VFY-2ND-BENE TO PI-ST-VFY-2ND-BENE.
061511
061511 6700-EXIT.
061511     EXIT.
061511  
061511     EJECT
061511 6750-GET-APPROVAL-LEVEL.
061511       MOVE SPACES              TO ELCNTL-KEY
061511       MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
061511       MOVE '2'                 TO ELCNTL-REC-TYPE
061511       MOVE PI-PROCESSOR-ID     TO ELCNTL-ACCESS
061511       MOVE +0                  TO ELCNTL-SEQ
061511       EXEC CICS READ
061511          DATASET   (FILE-ID-ELCNTL)
061511          SET       (ADDRESS OF CONTROL-FILE)
061511          RIDFLD    (ELCNTL-KEY)
061511          RESP      (WS-RESPONSE)
061511       END-EXEC
061511       IF RESP-NORMAL
061511          CONTINUE
061511       ELSE
061511          MOVE ER-1563          TO EMI-ERROR
061511          PERFORM 9900-ERROR-FORMAT
061511                                THRU 9900-EXIT
061511          GO TO 6750-EXIT
061511       END-IF.
061511
061511       MOVE CF-APPROVAL-LEVEL TO PI-APPROVAL-LEVEL.
061511
061511 6750-EXIT.
061511     EXIT.
061511  
061511     EJECT
02341 ******************************************************************   CL*17
02342 *       I/O REQUESTS AGAINST THE CLAIM MASTER FILE.              *   CL*17
02343 ******************************************************************   CL*17
02344  7000-READ-ELMSTR.                                                   CL*17
02345                                                                      CL*13
02346      EXEC CICS HANDLE CONDITION                                      CL*17
02347          NOTFND   (9040-NOTFND-ELMSTR)                               CL*17
02348          NOTOPEN  (9030-NOTOPEN-ELMSTR)                              CL*17
02349      END-EXEC.                                                       CL*17
02350                                                                   EL141
02351      EXEC CICS READ                                                  CL*17
02352          DATASET   (ELMSTR-FILE-ID)                                  CL*17
02353          RIDFLD    (ELMSTR-KEY)                                      CL*17
02354          SET       (ADDRESS OF CLAIM-MASTER)                         CL*20
02355      END-EXEC.                                                       CL*17
02356                                                                   EL141
02357  7000-EXIT.                                                          CL*17
02358      EXIT.                                                           CL*17
02359                                                                   EL141
02360  7005-READ-ELMSTR-UPDATE.                                            CL*17
02361                                                                   EL141
02362      EXEC CICS HANDLE CONDITION                                      CL*17
02363          NOTFND   (9040-NOTFND-ELMSTR)                               CL*17
02364          NOTOPEN  (9030-NOTOPEN-ELMSTR)                              CL*17
02365      END-EXEC.                                                       CL*17
02366                                                                      CL**4
02367      EXEC CICS READ                                                  CL*17
02368          DATASET   (ELMSTR-FILE-ID)                                  CL*17
02369          RIDFLD    (ELMSTR-KEY)                                      CL*17
02370          SET       (ADDRESS OF CLAIM-MASTER)                         CL*20
02371          UPDATE                                                      CL*17
02372      END-EXEC.                                                       CL*17
02373                                                                      CL*13
02374  7005-EXIT.                                                          CL*17
02375      EXIT.                                                           CL*17
02376                                                                      CL*13
02377  7020-START-ELMSTR.                                                  CL*20
02378                                                                      CL*20
02379      EXEC CICS HANDLE CONDITION                                      CL*20
02380          NOTFND   (9040-NOTFND-ELMSTR)                               CL*20
02381          NOTOPEN  (9030-NOTOPEN-ELMSTR)                              CL*20
02382      END-EXEC.                                                       CL*20
02383                                                                      CL*20
02384      EXEC CICS STARTBR                                               CL*20
02385          DATASET   (ELMSTR-FILE-ID)                                  CL*20
02386          RIDFLD    (ELMSTR-KEY)                                      CL*20
02387      END-EXEC.                                                       CL*20
02388                                                                      CL*20
02389  7020-EXIT.                                                          CL*20
02390      EXIT.                                                           CL*20
02391                                                                      CL*20
02392  7025-READNEXT-ELMSTR.                                               CL*20
02393                                                                      CL*20
02394      EXEC CICS HANDLE CONDITION                                      CL*20
02395          NOTFND   (9040-NOTFND-ELMSTR)                               CL*20
02396          NOTOPEN  (9030-NOTOPEN-ELMSTR)                              CL*20
02397      END-EXEC.                                                       CL*20
02398                                                                      CL*20
02399      EXEC CICS READNEXT                                              CL*20
02400          DATASET   (ELMSTR-FILE-ID)                                  CL*20
02401          RIDFLD    (ELMSTR-KEY)                                      CL*20
02402          SET       (ADDRESS OF CLAIM-MASTER)                         CL*20
02403      END-EXEC.                                                       CL*20
02404                                                                      CL*20
02405  7025-EXIT.                                                          CL*20
02406      EXIT.                                                           CL*20
02407      EJECT                                                           CL*20
02408  7010-REWRITE-ELMSTR.                                                CL*17
02409                                                                      CL*13
02410      EXEC CICS REWRITE                                               CL*17
02411          DATASET   (ELMSTR-FILE-ID)                                  CL*17
02412          FROM      (CLAIM-MASTER)                                    CL*17
02413      END-EXEC.                                                       CL*17
02414                                                                   EL141
02415  7010-EXIT.                                                          CL*17
02416      EXIT.                                                           CL*17
02417                                                                   EL141
02418  7015-UNLOCK-ELMSTR.                                                 CL*17
02419                                                                      CL*17
02420      EXEC CICS UNLOCK                                                CL*17
02421          DATASET   (ELMSTR-FILE-ID)                                  CL*17
02422      END-EXEC.                                                       CL*17
02423                                                                      CL*17
02424  7015-EXIT.                                                          CL*17
02425      EXIT.                                                           CL*17
02426                                                                      CL*17
02427      EJECT                                                           CL*17
02428 ******************************************************************   CL*17
02429 *       I/O REQUESTS AGAINST THE ACTIVITY TRAILER FILE           *   CL*17
02430 ******************************************************************   CL*17
02431  7050-READ-ELTRLR.                                                   CL*17
02432                                                                      CL*17
PEMMOD*    EXEC CICS HANDLE CONDITION                                      CL*20
PEMMOD*        NOTFND   (9050-NOTFND-ELTRLR)                               CL*20
PEMMOD*        NOTOPEN  (9060-NOTOPEN-ELTRLR)                              CL*20
PEMMOD*    END-EXEC.                                                       CL*20
02437                                                                      CL*20
02438      EXEC CICS READ                                                  CL*17
02439          DATASET   (ELTRLR-FILE-ID)                                  CL*17
02440          RIDFLD    (ELTRLR-KEY)                                      CL*17
02441          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*20
02442      END-EXEC.                                                       CL*17
02443                                                                      CL*17
02444  7050-EXIT.                                                          CL*17
02445      EXIT.                                                           CL*17
02446                                                                      CL*17
02447  7055-READ-ELTRLR-UPDATE.                                            CL*17
02448                                                                      CL*17
02449      EXEC CICS HANDLE CONDITION                                      CL*20
02450          NOTFND   (9050-NOTFND-ELTRLR)                               CL*20
02451          NOTOPEN  (9060-NOTOPEN-ELTRLR)                              CL*20
02452      END-EXEC.                                                       CL*20
02453                                                                      CL*20
02454      EXEC CICS READ                                                  CL*17
02455          DATASET   (ELTRLR-FILE-ID)                                  CL*17
02456          RIDFLD    (ELTRLR-KEY)                                      CL*17
02457          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*20
02458          UPDATE                                                      CL*17
02459      END-EXEC.                                                       CL*17
02460                                                                      CL*17
02461  7055-EXIT.                                                          CL*17
02462      EXIT.                                                           CL*17
02463                                                                      CL*17
02464  7060-REWRITE-ELTRLR.                                                CL*17
02465                                                                      CL*17
02466      EXEC CICS REWRITE                                               CL*17
02467          DATASET   (ELTRLR-FILE-ID)                                  CL*17
02468          FROM      (ACTIVITY-TRAILERS)                               CL*17
02469      END-EXEC.                                                       CL*17
02470                                                                      CL*17
02471  7060-EXIT.                                                          CL*17
02472      EXIT.                                                           CL*17
02473                                                                      CL*17
02474  7065-WRITE-ELTRLR.                                                  CL*17
02475                                                                      CL*17
02476      EXEC CICS HANDLE CONDITION                                      CL*20
02477          DUPREC   (7065-DUP)                                         CL*20
02478      END-EXEC.                                                       CL*20
02479                                                                      CL*20
02480      EXEC CICS WRITE                                                 CL*17
02481          DATASET   (ELTRLR-FILE-ID)                                  CL*17
02482          FROM      (ACTIVITY-TRAILERS)                               CL*17
02483          RIDFLD    (ELTRLR-KEY)                                      CL*17
02484      END-EXEC.                                                       CL*17
02485                                                                      CL*20
02486      GO TO 7065-EXIT.                                                CL*20
02487                                                                      CL*20
02488  7065-DUP.                                                           CL*20
02489                                                                      CL*17
02490  7065-EXIT.                                                          CL*17
02491      EXIT.                                                           CL*17
02492                                                                      CL*17
02493  7070-DELETE-ELTRLR.                                                 CL*17
02494                                                                      CL*17
02495      EXEC CICS DELETE                                                CL*17
02496         DATASET   (ELTRLR-FILE-ID)                                   CL*17
02497      END-EXEC.                                                       CL*17
02498                                                                      CL*17
02499  7070-EXIT.                                                          CL*17
02500      EXIT.                                                           CL*17
02501                                                                      CL*17
02502  7075-UNLOCK-ELTRLR.                                                 CL*17
02503                                                                      CL*17
02504      EXEC CICS UNLOCK                                                CL*17
02505          DATASET   (ELTRLR-FILE-ID)                                  CL*17
02506      END-EXEC.                                                       CL*17
02507                                                                      CL*17
02508  7075-EXIT.                                                          CL*17
02509      EXIT.                                                           CL*17
02510                                                                      CL*17
02511  7080-GETMAIN-ELTRLR.                                                CL*17
02512                                                                      CL*17
02513      EXEC CICS  GETMAIN                                              CL*17
02514             SET      (ADDRESS OF ACTIVITY-TRAILERS)                  CL*20
02515             INITIMG  (GETMAIN-SPACE)                                 CL*17
02516             LENGTH   (WS-TRLR-LENGTH)                                CL*17
02517      END-EXEC.                                                       CL*17
02518                                                                      CL*17
02519  7080-EXIT.                                                          CL*17
02520                                                                      CL*20
02521  7090-START-ELTRLR.                                                  CL*20
02522                                                                      CL*20
02523      EXEC CICS HANDLE CONDITION                                      CL*20
02524          NOTFND   (9050-NOTFND-ELTRLR)                               CL*20
02525          NOTOPEN  (9060-NOTOPEN-ELTRLR)                              CL*20
02526      END-EXEC.                                                       CL*20
02527                                                                      CL*20
02528      EXEC CICS STARTBR                                               CL*20
02529          DATASET   (ELTRLR-FILE-ID)                                  CL*20
02530          RIDFLD    (ELTRLR-KEY)                                      CL*20
02531      END-EXEC.                                                       CL*20
02532                                                                      CL*20
02533  7090-EXIT.                                                          CL*20
02534      EXIT.                                                           CL*20
02535                                                                      CL*20
02536  7095-READNEXT-ELTRLR.                                               CL*20
02537                                                                      CL*20
02538      EXEC CICS HANDLE CONDITION                                      CL*20
02539          NOTFND   (9050-NOTFND-ELTRLR)                               CL*20
02540          NOTOPEN  (9060-NOTOPEN-ELTRLR)                              CL*20
02541      END-EXEC.                                                       CL*20
02542                                                                      CL*20
02543      EXEC CICS READNEXT                                              CL*20
02544          DATASET   (ELTRLR-FILE-ID)                                  CL*20
02545          RIDFLD    (ELTRLR-KEY)                                      CL*20
02546          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*20
02547      END-EXEC.                                                       CL*20
02548                                                                      CL*20
02549  7095-EXIT.                                                          CL*20
02550      EXIT.                                                           CL*17
02551      EJECT                                                        EL141
02552  8000-LOAD-ERROR-MESSAGES.                                           CL*17
02553                                                                   EL141
02554      MOVE SPACES                 TO  MERMSG1O                        CL*17
02555                                      MERMSG2O.                       CL*17
02556                                                                   EL141
02557      IF EMI-NO-ERRORS                                                CL*17
02558          GO TO 8000-EXIT.                                            CL*17
02559                                                                   EL141
02560      IF EMI-NUMBER-OF-LINES IS EQUAL TO 1                            CL*17
02561          MOVE EMI-LINE1          TO  MERMSG1O                        CL*17
02562          GO TO 8000-EXIT.                                            CL*17
02563                                                                      CL*13
02564      IF EMI-NUMBER-OF-LINES IS EQUAL TO 2                            CL*17
02565          MOVE EMI-LINE1          TO  MERMSG1O                        CL*17
02566          MOVE EMI-LINE2          TO  MERMSG2O                        CL*17
02567          GO TO 8000-EXIT.                                            CL*17
02568                                                                      CL*13
02569      MOVE EMI-LINE1              TO  MERMSG1O.                       CL*17
02570                                                                      CL*13
02571  8000-EXIT.                                                          CL*17
02572      EXIT.                                                           CL*17
02573                                                                      CL*13
02574  8100-SEND-INITIAL-MAP.                                              CL*17
02575      MOVE EIBTIME                TO  TIME-IN.                        CL*17
02576      MOVE WS-HOUR                TO  WS-TRANS-HOUR.                  CL*17
02577      MOVE WS-MINUTE              TO  WS-TRANS-MINUTE.                CL*17
02578                                                                      CL**4
02579      MOVE TIME-OUT               TO  MRNTIMEO.                       CL*17
02580      MOVE SAVE-DATE              TO  MRNDATEO.
031102     MOVE PI-CERT-NO             TO  MCERTO.                         CL*17
02581                                                                   EL141
02582      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.                CL*17
02583                                                                   EL141
02584      EXEC CICS  SEND                                                 CL*17
02585           MAPSET   (WS-MAPSET-NAME)                                  CL*17
02586           MAP      (WS-MAP-NAME)                                     CL*17
02587           FROM     (EL141AO)                                         CL*17
02588           FREEKB                                                     CL*17
02589           ERASE                                                      CL*17
02590           CURSOR                                                     CL*17
02591      END-EXEC.                                                    EL141
02592                                                                      CL*17
02593      GO TO 9100-RETURN-TRAN.                                         CL*17
02594                                                                   EL141
02595  8100-EXIT.                                                       EL141
02596       EXIT.                                                       EL141
02597                                                                   EL141
02598  8200-SEND-DATAONLY.                                                 CL*17
02599      MOVE EIBTIME                    TO  TIME-IN.                    CL*17
02600      MOVE WS-HOUR                    TO  WS-TRANS-HOUR.              CL*17
02601      MOVE WS-MINUTE                  TO  WS-TRANS-MINUTE.            CL*17
02602                                                                      CL*17
02603      MOVE TIME-OUT                   TO  MRNTIMEO.                   CL*17
02604      MOVE SAVE-DATE                  TO  MRNDATEO.
031102     MOVE PI-CERT-NO                 TO  MCERTO.                     CL*17
02605                                                                      CL*17
02606      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.                CL*17
02607                                                                      CL*17
02608      EXEC CICS  SEND                                                 CL*17
02609           MAPSET   (WS-MAPSET-NAME)                                  CL*17
02610           MAP      (WS-MAP-NAME)                                     CL*17
02611           FROM     (EL141AO)                                         CL*17
02612           FREEKB                                                     CL*17
02613           DATAONLY                                                   CL*17
02614           CURSOR                                                     CL*17
02615      END-EXEC.                                                    EL141
02616                                                                   EL141
02617      GO TO 9100-RETURN-TRAN.                                         CL*17
02618                                                                   EL141
02619  8200-EXIT.                                                       EL141
02620       EXIT.                                                       EL141
02621                                                                   EL141
02622  8300-SEND-TEXT.                                                     CL*17
02623      EXEC CICS SEND TEXT                                             CL*17
02624          FROM     (LOGOFF-TEXT)                                      CL*17
02625          ERASE                                                       CL*17
02626          FREEKB                                                      CL*17
02627          LENGTH   (LOGOFF-LENGTH)                                    CL*17
02628      END-EXEC.                                                    EL141
02629                                                                   EL141
02630      EXEC CICS RETURN                                                CL*17
02631          END-EXEC.                                                   CL*17
02632                                                                   EL141
02633  8300-EXIT.                                                          CL*17
02634       EXIT.                                                       EL141
02635                                                                   EL141
02636  8500-DATE-CONVERSION.                                               CL*17
02637                                                                   EL141
02638      MOVE LINK-ELDATCV               TO  PGM-NAME.                   CL*17
02639      EXEC CICS LINK                                                  CL*17
02640             PROGRAM  (PGM-NAME)                                      CL*17
02641             COMMAREA (DATE-CONVERSION-DATA)                          CL*17
02642             LENGTH   (DC-COMM-LENGTH)                                CL*17
02643      END-EXEC.                                                       CL*17
02644                                                                      CL*17
02645  8500-EXIT.                                                          CL*17
02646       EXIT.                                                       EL141
02647                                                                   EL141
02648      EJECT                                                           CL*17
02649  9000-UNAUTHERR.                                                     CL*17
02650      MOVE UNACCESS-MSG               TO  LOGOFF-MSG.                 CL*17
02651      GO TO 8300-SEND-TEXT.                                           CL*17
02652                                                                   EL141
02653  9030-NOTOPEN-ELMSTR.                                                CL*17
02654                                                                   EL141
02655      MOVE LOW-VALUES                 TO  EL141AI.                    CL*17
02656      MOVE ER-0154                    TO  EMI-ERROR.                  CL*17
02657      MOVE -1                         TO  MFMAINTL.                   CL*17
02658      MOVE AL-UABON                   TO  MFMAINTA.                   CL*17
02659      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.                   CL*17
02660      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
02661      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
02662                                                                   EL141
02663  9040-NOTFND-ELMSTR.                                                 CL*17
02664                                                                   EL141
02665      MOVE LOW-VALUES                 TO  EL141AI.                    CL*17
02666      MOVE ER-0133                    TO  EMI-ERROR.                  CL*17
02667      MOVE -1                         TO  MFMAINTL.                   CL*17
02668      MOVE AL-UABON                   TO  MFMAINTA.                   CL*17
02669      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.                   CL*17
02670      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
02671      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
02672                                                                   EL141
02673  9050-NOTFND-ELTRLR.                                                 CL*20
02674                                                                      CL*20
02675      MOVE LOW-VALUES                 TO  EL141AI.                    CL*20
02676      MOVE ER-0135                    TO  EMI-ERROR.                  CL*20
02677      MOVE -1                         TO  MADDRTPL.                   CL*20
02678      MOVE AL-UABON                   TO  MADDRTPA                    CL*20
02679                                          MFMAINTA.                   CL*20
02680      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.                   CL*20
02681      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.                   CL*20
02682      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*20
02683      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*20
02684      GO TO 8100-SEND-INITIAL-MAP.                                    CL*20
02685                                                                      CL*20
02686  9060-NOTOPEN-ELTRLR.                                                CL*20
02687                                                                      CL*20
02688      MOVE LOW-VALUES                 TO  EL141AI.                    CL*20
02689      MOVE ER-0172                    TO  EMI-ERROR.                  CL*20
02690      MOVE -1                         TO  MFMAINTL.                   CL*20
02691      MOVE AL-UABON                   TO  MFMAINTA.                   CL*20
02692      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.                   CL*20
02693      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*20
02694      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.                 CL*20
02695      GO TO 8100-SEND-INITIAL-MAP.                                    CL*20
02696                                                                      CL*20
02697      EJECT                                                           CL*20
02698  9100-RETURN-TRAN.                                                   CL*17
02699      EXEC CICS RETURN                                                CL*17
02700          TRANSID    (WS-TRANS-ID)                                    CL*17
02701          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                        CL*17
02702          LENGTH     (PI-COMM-LENGTH)                                 CL*17
02703      END-EXEC.                                                       CL*17
02704                                                                      CL*17
02705  9300-XCTL.                                                          CL*17
02706      EXEC CICS XCTL                                               EL141
02707          PROGRAM  (PGM-NAME)                                         CL*17
02708          COMMAREA (PROGRAM-INTERFACE-BLOCK)                          CL*17
02709          LENGTH   (PI-COMM-LENGTH)                                   CL*17
02710      END-EXEC.                                                    EL141
02711                                                                   EL141
02712  9300-EXIT.                                                          CL*17
02713       EXIT.                                                       EL141
02714                                                                   EL141
02715  9400-CLEAR.                                                         CL*17
02716                                                                   EL141
02717      MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.                   CL*17
02718      GO TO 9300-XCTL.                                                CL*17
02719                                                                   EL141
02720  9400-EXIT.                                                          CL*17
02721      EXIT.                                                           CL*17
02722                                                                   EL141
02723      EJECT                                                        EL141
02724  9600-PGMIDERR.                                                      CL*17
02725      EXEC CICS HANDLE CONDITION                                      CL*17
02726          PGMIDERR (8300-SEND-TEXT)                                   CL*17
02727      END-EXEC.                                                       CL*17
02728                                                                      CL*17
02729      MOVE THIS-PGM                   TO  LOGOFF-PGM                  CL*17
02730                                          PI-CALLING-PROGRAM.         CL*17
02731      MOVE SPACES                     TO  PI-ENTRY-CD-1.              CL*17
02732      MOVE 'EL005'                    TO  PGM-NAME.                   CL*17
02733      MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.                CL*17
02734      GO TO 9300-XCTL.                                                CL*17
02735                                                                      CL*17
02736  9600-EXIT.                                                          CL*17
02737       EXIT.                                                          CL*17
02738                                                                      CL*17
02739  9900-ERROR-FORMAT.                                                  CL*17
02740      MOVE LINK-001                   TO  PGM-NAME.                   CL*17
02741      EXEC CICS LINK                                                  CL*17
02742             PROGRAM  (PGM-NAME)                                      CL*17
02743             COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 CL*17
02744             LENGTH   (EMI-COMM-LENGTH)                               CL*17
02745      END-EXEC.                                                       CL*17
02746                                                                      CL*17
02747 *    MOVE EMI-LINE1                  TO  MERMSG1O.                   CL*17
02748 *    MOVE EMI-LINE2                  TO  MERMSG2O.                   CL*17
02749                                                                   EL141
02750  9900-EXIT.                                                       EL141
02751       EXIT.                                                       EL141
02752                                                                   EL141
02753  9990-ERROR.                                                         CL*17
02754                                                                   EL141
02755      MOVE LINK-004                   TO  PGM-NAME.                   CL*17
02756      MOVE -1                         TO  MFMAINTL.                   CL*17
02757      MOVE DFHEIBLK                   TO  EMI-LINE1.                  CL*17
02758      EXEC CICS LINK                                                  CL*17
02759          PROGRAM   (PGM-NAME)                                        CL*17
02760          COMMAREA  (EMI-LINE1)                                       CL*17
02761          LENGTH    (72)                                              CL*17
02762      END-EXEC.                                                       CL*17
02763                                                                   EL141
02764      GO TO 8200-SEND-DATAONLY.                                       CL*17
02765                                                                   EL141
02766  9995-SECURITY-VIOLATION.                                         EL141
02767                              COPY ELCSCTP.                        EL141
02768                                                                   EL141
02769  9995-EXIT.                                                       EL141
02770      EXIT.                                                        EL141
02771                                                                   EL141
02772  9999-LAST-PARAGRAPH.                                                CL*17
02773      GOBACK.                                                         CL*17
