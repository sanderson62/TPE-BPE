00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL141 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 07/15/94 10:58:41.
00007 *                            VMOD=2.023
00008 *
00008 *
00009 *AUTHOR.    LOGIC, INC.
00010 *           DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *REMARKS.   TRANSACTION - EX18
00024
00025 *    SCREENS     - EL141S - ADDRESS MAINTENANCE
00026
00027 *    ENTERED BY  - EL132 - NEW CLAIM SETUP
00028 *                - EL156 - PAYMENTS
00029 *                - EL162 - MAIL RECORDING
00030 *                - EL150 - STATUS AND DISPOSITION
00031 *                - EL130 - NEW CLAIM SET-UP
00032 *                - EM130 - CONVENIENCE NEW CLAIM SET-UP
00033
00034 *    EXIT TO     - EL101 - MAINTENANCE MENU
00035
00036 *    INPUT FILE  - ELMSTR - CLAIM MASTER
00037 *                - ELTRLR - ACTIVITY TRAILERS
00038 *                - ERACCT - ACCOUNT MASTER
00039 *                - MPPROD - CONVENIENCE PRODUCER MASTER
00040
00041 *    OUTPUT FILE - ELMSTR - CLAIM MASTER
00042 *                - ELTRLR - ACTIVITY TRAILERS
00043
00044 *    COMMAREA    - PASSED CLAIM NUMBER FROM PROG INTERFACE BLK
00045
00046 *    ERROR-CODES ACCESSED - 23, 135, 270, 20, 277, 278, 132, 52
00047 *                           133, 136, 137, 50, 29, 08
00048 *    NARRATIVE   - PROVIDE COMPLETE MAINTENANCE FUNCTIONS,
00049 *                  (ADD,CHANGE,DELETE,SHOW) FOR THE ADDRESS-
00050 *                  TRAILER RECORDS OF THE CLAIM MASTER FILE/
00051 *                  FOR MAINTENANCE FUNCTIONS(A, C, D), THE
00052 *                  CL-TRAILER-SEQ-CNT AND CL-ADDRESS-TRAILER-SEQ,
00053 *                  FIELDS OF THE CLAIM MASTER ALONG WITH
00054 *                  MODIFIED ( TIME, DATE, PERSON) ARE UPDATED/
00055 *                  ACCOUNT MASTER FILE IS ONLY  SCANNED FOR
00056 *                  ACCOUNT ADDRESS INFORMATION WHEN ACCT ADDRESS
00057 *                  SEQ NO OF CLMSTR FOR TRAILER TYPE (5) IS ZEROS.
00058 *                  ON ENTRY, SHOW EXISTING ADDRESS TRAILERS
00059 *                  BY HIGHLIGHTING THE 1-7 FIELDS ON MAP
00060
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
00061      EJECT
00062  ENVIRONMENT DIVISION.
00063
00064  DATA DIVISION.
00065
00066  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00067
00068  77  FILLER  PIC X(32)  VALUE '********************************'.
00069  77  FILLER  PIC X(32)  VALUE '*   EL141  WORKING STORAGE     *'.
00070  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.023 *********'.
00071
00072 *    COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00073
00074 *    COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
00075
00076      EJECT
00077  01  WS-DATE-AREA.
00078      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00079      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00080
00081 * DLO004
00082  01  DL04-COMM-LENGTH              PIC S9(4)  COMP VALUE +43.
00083  01  WS-DLO-VALIDATE-STATE-ZIP.
00084      12  DL04-ADDR-LINE            PIC X(30).
00085      12  DL04-ZIP-CODE             PIC X(9).
00086      12  DL04-RETURN-CODE          PIC XX.
00087      12  DL04-STATE-FOUND          PIC XX.
00088
00089  01  MISC-WORK-AREAS.
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
00091      12  WS-ZIP-CODE.
00092          16  WS-ZIP-1            PIC X.
00093              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
00094          16  WS-ZIP-2-3          PIC XX.
00095          16  WS-ZIP-4            PIC X.
00096          16  WS-ZIP-5            PIC X.
00097          16  WS-ZIP-6            PIC X.
00098          16  FILLER              PIC X(4).
00099      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
00100          16  WS-ZIP-AM-1-CODE    PIC X(5).
00101          16  WS-ZIP-AM-1-PLUS4   PIC X(4).
00102          16  FILLER              PIC X.
00103      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
00104          16  WS-ZIP-AM-2-CODE    PIC X(5).
00105          16  WS-ZIP-AM-2-DASH    PIC X.
00106          16  WS-ZIP-AM-2-PLUS4   PIC X(4).
00107      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
00108          16  WS-ZIP-CAN-1-POST1  PIC XXX.
00109          16  WS-ZIP-CAN-1-POST2  PIC XXX.
00110          16  FILLER              PIC X(4).
00111      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
00112          16  WS-ZIP-CAN-2-POST1  PIC XXX.
00113          16  FILLER              PIC X.
00114          16  WS-ZIP-CAN-2-POST2  PIC XXX.
00115          16  FILLER              PIC XXX.
00116
00117      EJECT
00118  01  AT-SAVE-TABLE.
00119    03  AT-SAVE-TABLE-OCCURS OCCURS 9 TIMES.
00120      12  AT-SAVE-CONTROL-PRIMARY.
00121          16  AT-SAVE-COMPANY-CD          PIC X.
00122          16  AT-SAVE-CARRIER             PIC X.
00123          16  AT-SAVE-CLAIM-NO            PIC X(7).
00124          16  AT-SAVE-CERT-NO.
00125              20  AT-SAVE-CERT-PRIME      PIC X(10).
00126              20  AT-SAVE-CERT-SFX        PIC X.
00127          16  AT-SAVE-SEQUENCE-NO         PIC S9(4)     COMP.
00128      12  AT-SAVE-TRAILER-TYPE            PIC X.
00129      12  AT-SAVE-RECORDED-DT             PIC XX.
00130      12  AT-SAVE-RECORDED-BY             PIC X(4).
00131      12  AT-SAVE-LAST-MAINT-HHMMSS       PIC S9(6)     COMP-3.
00132      12  AT-SAVE-ADDRESS-TYPE            PIC X.
00133      12  AT-SAVE-MAIL-TO-NAME            PIC X(30).
00134      12  AT-SAVE-ADDRESS-LINE-1          PIC X(30).
00135      12  AT-SAVE-ADDRESS-LINE-2          PIC X(30).
00136      12  AT-SAVE-CITY                    PIC X(28).
           12  AT-SAVE-STATE                   PIC XX.
00137      12  AT-SAVE-ZIP.
00138          16  AT-SAVE-ZIP-CODE.
00139              20  AT-SAVE-ZIP-1ST         PIC X.
00140              20  FILLER                  PIC X(4).
00141          16  AT-SAVE-ZIP-PLUS4           PIC X(4).
00142      12  AT-SAVE-PHONE-NO                PIC 9(11)     COMP-3.
00143      12  AT-SAVE-ADDRESS-LAST-MAINT-DT   PIC XX.
00144      12  AT-SAVE-ADDRESS-LST-UPDATED-BY  PIC X(4).
00145      12  AT-SAVE-STATUS                  PIC X.
061511     12  AT-SAVE-VFY-2ND-BENE-SSN        PIC X(9).
061511     12  AT-SAVE-VFY-2ND-BENE-VERIFIED   PIC X.
00146
00147      EJECT
00148  01  ERROR-MESSAGES.
00149      12  ER-0000                 PIC X(4)  VALUE '0000'.
00150      12  ER-0004                 PIC X(4)  VALUE '0004'.
00151      12  ER-0008                 PIC X(4)  VALUE '0008'.
00152      12  ER-0023                 PIC X(4)  VALUE '0023'.
00153      12  ER-0029                 PIC X(4)  VALUE '0029'.
00154      12  ER-0050                 PIC X(4)  VALUE '0050'.
00155      12  ER-0052                 PIC X(4)  VALUE '0052'.
00156      12  ER-0053                 PIC X(4)  VALUE '0053'.
00157      12  ER-0070                 PIC X(4)  VALUE '0070'.
00158      12  ER-0132                 PIC X(4)  VALUE '0132'.
00159      12  ER-0133                 PIC X(4)  VALUE '0133'.
00160      12  ER-0135                 PIC X(4)  VALUE '0135'.
00161      12  ER-0136                 PIC X(4)  VALUE '0136'.
00162      12  ER-0137                 PIC X(4)  VALUE '0137'.
00163      12  ER-0145                 PIC X(4)  VALUE '0145'.
00164      12  ER-0154                 PIC X(4)  VALUE '0154'.
00165      12  ER-0168                 PIC X(4)  VALUE '0168'.
00166      12  ER-0172                 PIC X(4)  VALUE '0172'.
00167      12  ER-0265                 PIC X(4)  VALUE '0265'.
00168      12  ER-0270                 PIC X(4)  VALUE '0270'.
00169      12  ER-0277                 PIC X(4)  VALUE '0277'.
00170      12  ER-0278                 PIC X(4)  VALUE '0278'.
00171      12  ER-0315                 PIC X(4)  VALUE '0315'.
00172      12  ER-0799                 PIC X(4)  VALUE '0799'.
00173      12  ER-0853                 PIC X(4)  VALUE '0853'.
00174      12  ER-0854                 PIC X(4)  VALUE '0854'.
00175      12  ER-0855                 PIC X(4)  VALUE '0855'.
00176      12  ER-0856                 PIC X(4)  VALUE '0856'.
00177      12  ER-0857                 PIC X(4)  VALUE '0857'.
00178      12  ER-0858                 PIC X(4)  VALUE '0858'.
00179      12  ER-0859                 PIC X(4)  VALUE '0859'.
00180      12  ER-0860                 PIC X(4)  VALUE '0860'.
00181      12  ER-0886                 PIC X(4)  VALUE '0886'.
00182      12  ER-0862                 PIC X(4)  VALUE '0862'.
00183      12  ER-0863                 PIC X(4)  VALUE '0863'.
061511     12  ER-1563                 PIC X(4)  VALUE '1563'.
00184      12  ER-1878                 PIC X(4)  VALUE '1878'.
00185      12  ER-1879                 PIC X(4)  VALUE '1879'.
00186      12  ER-2166                 PIC X(4)  VALUE '2166'.
           12  ER-2209                 PIC X(4)  VALUE '2209'.
00187      12  ER-2599                 PIC X(4)  VALUE '2599'.
061511     12  ER-3040                 PIC X(4)  VALUE '3040'.
00188      12  ER-7008                 PIC X(4)  VALUE '7008'.
032514     12  ER-7584                 PIC X(4)  VALUE '7584'.
00189      12  ER-7675                 PIC X(4)  VALUE '7675'.
00190      12  ER-8131                 PIC X(4)  VALUE '8131'.
00191      12  ER-8133                 PIC X(4)  VALUE '8133'.
00192      12  ER-8134                 PIC X(4)  VALUE '8134'.
00193      12  ER-9616                 PIC X(4)  VALUE '9616'.
00194      12  ER-9886                 PIC X(4)  VALUE '9886'.
00195
00196      EJECT
00197  01  WS-SCRATCH-AREA.
00198      05  WS-COMP-MSTR                PIC X VALUE SPACES.
00199      05  SC-ITEM                     PIC S9(4) VALUE +0001 COMP.
00200      05  GETMAIN-SPACE               PIC X     VALUE SPACE.
00201      05  WS-TRLR-LENGTH              PIC S9(4) VALUE +200 COMP.
00202
00203      05  WS-MAP-NAME                 PIC X(08)   VALUE 'EL141A'.
00204      05  WS-MAPSET-NAME              PIC X(08)   VALUE 'EL141S'.
00205
00206      05  ELBENE-FILE-ID              PIC X(08)   VALUE 'ELBENE'.
00207      05  ELMSTR-FILE-ID              PIC X(08)   VALUE 'ELMSTR'.
00208      05  ELTRLR-FILE-ID              PIC X(08)   VALUE 'ELTRLR'.
00209      05  ERACCT-FILE-ID              PIC X(08)   VALUE 'ERACCT'.
00210      05  ERCOMP-FILE-ID              PIC X(08)   VALUE 'ERCOMP'.
00211      05  EMPROD-FILE-ID              PIC X(08)   VALUE 'MPPROD'.
00212
00213      05  LINK-001                    PIC X(08)   VALUE 'EL001'.
00214      05  LINK-004                    PIC X(08)   VALUE 'EL004'.
00215      05  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'.
00216
00217      05  XCTL-005                    PIC X(08)   VALUE 'EL005'.
00218      05  XCTL-010                    PIC X(08)   VALUE 'EL010'.
00219      05  XCTL-114                    PIC X(08)   VALUE 'EL114'.
00220      05  XCTL-126                    PIC X(08)   VALUE 'EL126'.
00221
00222      05  WS-TRANS-ID                 PIC X(4)   VALUE 'EX18'.
00223
00224      05  WS-SEQ-CONVERT              PIC S9(4)   VALUE ZEROS.
00225      05  NDX                         PIC S99     COMP-3.
00226
00227      05  THIS-PGM                    PIC X(08)   VALUE 'EL141'.
00228      05  PGM-NAME                    PIC X(08)   VALUE SPACES.
00229
00230      05  WS-PHONE                    PIC X(12).
00231      05  FILLER        REDEFINES WS-PHONE.
00232          10  FILLER                  PIC 9.
00233          10  WS-PHONE-NUM            PIC 9(11).
00234
00235      05  WS-PHONE-BRKDN              PIC 9(11).
00236      05  FILLE    REDEFINES WS-PHONE-BRKDN.
00237          10  FILLER                  PIC 9.
00238          10  WS-PH-1                 PIC 999.
00239          10  WS-PH-2                 PIC 999.
00240          10  WS-PH-3                 PIC 9999.
00241
00242      05  WS-PHONE-EDIT.
00243          10  WS-PH-ED-1              PIC XXX.
00244          10  FILLER                  PIC X   VALUE '-'.
00245          10  WS-PH-ED-2              PIC XXX.
00246          10  FILLER                  PIC X   VALUE '-'.
00247          10  WS-PH-ED-3              PIC XXXX.
00248
00249      05  WS-CURR-DATE-BIN            PIC XX      VALUE LOW-VALUES.
00250      05  WS-SAVE-CERT-NO             PIC X(10).
00251      05  WS-SAVE-SEQUENCE-NO         PIC S9(4) COMP.
00252      05  WS-SAVE-TRLR-KEY            PIC X(22).
00253      05  WS-SAVE-MSTR-KEY            PIC X(20).
00254      05  WS-START-MSTR-KEY           PIC X(20).
00255      05  WS-SAVE-TRLR-SEQ            PIC S9(1).
00256      05  WS-BEGIN-CNT                PIC S9(2).
00257      05  WS-INCREMENT-NO             PIC 99.
00258      05  WS-WORK-SEQ                 PIC 99.
00259      05  WS-NEW-SEQ                  PIC 99.
00260
00261      05  WS-REWRITE-MSTR-SW          PIC X  VALUE 'N'.
00262          88  REWRITE-MSTR                   VALUE 'Y'.
00263
00264      05  WS-TRAILER-SW               PIC X  VALUE 'N'.
00265          88  TRLR-FOUND                     VALUE 'Y'.
00266          88  TRLR-NOT-FOUND                 VALUE 'Y'.
00267
00268      05  WS-MAINT-FUNC-SW            PIC X  VALUE SPACE.
00269          88  VALID-FUNCTION-ENTERED  VALUES ARE 'A' 'S' 'C' 'D'.
00270          88  CHANGE-REQUIRED         VALUE 'C'.
00271          88  DELETE-REQUIRED         VALUE 'D'.
00272          88  ADD-REQUIRED            VALUE 'A'.
00273          88  LOOKUP-REQUIRED         VALUE 'S'.
00274
00275      05  WS-ADDR-TYPE-SW.
00276          10  WS-ADDR-TYPE            PIC X VALUE SPACES.
00277              88  VALID-TYPE-ENTERED
00278                   VALUES ARE 'I' 'B' 'A' 'P' 'E' 'O' 'Q'.
00279              88  BENIF-ADDRESS
00280                       VALUE      'B'.
00281              88  ACCT-ADDRESS
00282                       VALUE      'A'.
00283          10  WS-ADDR-SEQ             PIC X VALUE SPACES.
00284          10  WS-ADDR-SEQ-NUM REDEFINES
00285              WS-ADDR-SEQ             PIC S9.
00286
00287      05  WS-ADDR-TRAILER-CNT.
00288          10  WS-ADDR-I-CNT          PIC S9.
00289          10  WS-ADDR-A-CNT          PIC S9.
00290          10  WS-ADDR-B-CNT          PIC S9.
00291          10  WS-ADDR-E-CNT          PIC S9.
00292          10  WS-ADDR-P-CNT          PIC S9.
00293          10  WS-ADDR-O-CNT          PIC S9.
00294          10  WS-ADDR-Q-CNT          PIC S9.
00295
00296      05  WS-ADDR-SW                  PIC X VALUE SPACES.
00297      05  WS-INPUT-ERROR-SW           PIC X  VALUE SPACE.
00298          88  ZIP-ERROR               VALUE 'Z'.
00299          88  PHON-ERROR              VALUE 'P'.
00300          88  NODATA-ERROR            VALUE 'N'.
00301
00302      05  WS-TRAILER-SW               PIC X  VALUE SPACE.
00303          88  NO-TRAILER              VALUE 'N'.
00304
00305 ******************************************************************
00306 ***  DATE AND TIME WORK AREAS
00307 ******************************************************************
00308      05  TIME-IN                     PIC 9(7).
00309      05  WS-TIME  REDEFINES TIME-IN.
00310          10  FILLER                  PIC 9.
00311          10  WS-HOUR                 PIC 99.
00312          10  WS-MINUTE               PIC 99.
00313          10  FILLER                  PIC 99.
00314
00315      05  TIME-OUT.
00316          10  WS-TRANS-HOUR           PIC XX  VALUE SPACE.
00317          10  FILLER                  PIC X   VALUE '.'.
00318          10  WS-TRANS-MINUTE         PIC XX  VALUE SPACE.
00319
00320      EJECT
00321 ***  KEY TO DATASETS
00322 ******************************************************************
00323
00324      05  ERCOMP-KEY.
00325          10  ERCOMP-COMPANY-CD           PIC X.
00326          10  ERCOMP-CARRIER              PIC X.
00327          10  ERCOMP-GROUPING             PIC X(6).
00328          10  ERCOMP-RESP-NO              PIC X(10).
00329          10  ERCOMP-ACCOUNT              PIC X(10).
00330          10  ERCOMP-TYPE                 PIC X.
00331
00332      05  ELTRLR-KEY.
00333          10  ELMSTR-KEY.
00334              15  ELTRLR-COMPANY-CD       PIC X.
00335              15  ELTRLR-CARRIER          PIC X.
00336              15  ELTRLR-CLAIM-NO         PIC X(7).
00337              15  ELTRLR-CERT-NO.
00338                  20  ELTRLR-CERT-PRIME   PIC X(10).
00339                  20  ELTRLR-CERT-SFX     PIC X.
00340          10  ELTRLR-SEQUENCE-NO      PIC   S9(4) COMP.
00341
00342      05  ERACCT-KEY.
00343        07  ERACCT-PARTIAL-KEY.
00344          10  ERACCT-COMPANY-CD           PIC X.
00345          10  ERACCT-CARRIER              PIC X.
00346          10  ERACCT-GROUPING             PIC X(6).
00347          10  ERACCT-STATE                PIC XX.
00348          10  ERACCT-ACCOUNT              PIC X(10).
00349        07  ERACCT-EFF-DT                 PIC XX.
00350        07  FILLER                        PIC S9(9) VALUE +0 COMP.
00351      05  ERACCT-KEY-SAVE                PIC X(20).
00352      05  WS-HOLD-ERACCT-RECORD          PIC X(2000).
00353
00354      05  EMPROD-KEY.
00355        07  EMPROD-PARTIAL-KEY.
00356          10  EMPROD-COMPANY-CD           PIC X(01).
00357          10  EMPROD-CARRIER              PIC X(01).
00358          10  EMPROD-GROUPING             PIC X(06).
00359          10  EMPROD-STATE                PIC X(02).
00360          10  EMPROD-PRODUCER             PIC X(10).
00361        07  EMPROD-EXP-DT                 PIC X(02).
00362
00363      05  ELBENE-KEY.
00364          10  ELBENE-COMPANY-CD    PIC X.
00365          10  ELBENE-RECORD-TYPE  PIC X.
00366          10  ELBENE-BENEFICIARY  PIC X(10).
00367      EJECT
00368 ******************************************************************
00369 *** STANDARD COPY AREAS
00370 ******************************************************************
00371 *    COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00372  01  PF-AID REDEFINES DFHAID.
00373      05  FILLER                      PIC X(8).
00374      05  PF-VALUES  OCCURS 24        PIC X.
00375      EJECT
00376 *    COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
00377      12  PI-REDEFINES REDEFINES PI-PROGRAM-WORK-AREA.
00378          16  FILLER                  PIC X(571).
00379          16  PI-MAINT                PIC X(01).
00380          16  PI-AIG-ADD-SW           PIC X(01).
00381          16  PI-LETTER-ERROR-CODE    PIC 9(04).
00382          16  PI-PREV-MAINT-CODE      PIC X(01).
00383          16  PI-PREV-ADDR-TYPE       PIC X(02).
061511*         16  PI-FILLER               PIC X(60).
061511         16  PI-ST-VFY-2ND-BENE      PIC X.
061511         16  PI-APPROVAL-LEVEL       PIC X.
061511         16  PI-FILLER               PIC X(58).
00385
00386      EJECT
00387 *    COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00388      EJECT
00389 *    COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00390      EJECT
00391 *    COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00392      EJECT
00393 *    COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00394      EJECT
00395 *    COPY EL141S.
       01  EL141AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  MRNDATEL PIC S9(0004) COMP.
           05  MRNDATEF PIC  X(0001).
           05  FILLER REDEFINES MRNDATEF.
               10  MRNDATEA PIC  X(0001).
           05  MRNDATEI PIC  X(0008).
      *    -------------------------------
           05  MRNTIMEL PIC S9(0004) COMP.
           05  MRNTIMEF PIC  X(0001).
           05  FILLER REDEFINES MRNTIMEF.
               10  MRNTIMEA PIC  X(0001).
           05  MRNTIMEI PIC  X(0005).
      *    -------------------------------
           05  MCERTL PIC S9(0004) COMP.
           05  MCERTF PIC  X(0001).
           05  FILLER REDEFINES MCERTF.
               10  MCERTA PIC  X(0001).
           05  MCERTI PIC  X(0010).
      *    -------------------------------
           05  MFMAINTL PIC S9(0004) COMP.
           05  MFMAINTF PIC  X(0001).
           05  FILLER REDEFINES MFMAINTF.
               10  MFMAINTA PIC  X(0001).
           05  MFMAINTI PIC  X(0001).
      *    -------------------------------
           05  MADDRTPL PIC S9(0004) COMP.
           05  MADDRTPF PIC  X(0001).
           05  FILLER REDEFINES MADDRTPF.
               10  MADDRTPA PIC  X(0001).
           05  MADDRTPI PIC  X(0002).
      *    -------------------------------
           05  MTYPE1L PIC S9(0004) COMP.
           05  MTYPE1F PIC  X(0001).
           05  FILLER REDEFINES MTYPE1F.
               10  MTYPE1A PIC  X(0001).
           05  MTYPE1I PIC  X(0013).
      *    -------------------------------
           05  ICNTL PIC S9(0004) COMP.
           05  ICNTF PIC  X(0001).
           05  FILLER REDEFINES ICNTF.
               10  ICNTA PIC  X(0001).
           05  ICNTI PIC  X(0001).
      *    -------------------------------
           05  MTYPE2L PIC S9(0004) COMP.
           05  MTYPE2F PIC  X(0001).
           05  FILLER REDEFINES MTYPE2F.
               10  MTYPE2A PIC  X(0001).
           05  MTYPE2I PIC  X(0013).
      *    -------------------------------
           05  BCNTL PIC S9(0004) COMP.
           05  BCNTF PIC  X(0001).
           05  FILLER REDEFINES BCNTF.
               10  BCNTA PIC  X(0001).
           05  BCNTI PIC  X(0001).
      *    -------------------------------
           05  MTYPE3L PIC S9(0004) COMP.
           05  MTYPE3F PIC  X(0001).
           05  FILLER REDEFINES MTYPE3F.
               10  MTYPE3A PIC  X(0001).
           05  MTYPE3I PIC  X(0013).
      *    -------------------------------
           05  ACNTL PIC S9(0004) COMP.
           05  ACNTF PIC  X(0001).
           05  FILLER REDEFINES ACNTF.
               10  ACNTA PIC  X(0001).
           05  ACNTI PIC  X(0001).
      *    -------------------------------
           05  MTYPE4L PIC S9(0004) COMP.
           05  MTYPE4F PIC  X(0001).
           05  FILLER REDEFINES MTYPE4F.
               10  MTYPE4A PIC  X(0001).
           05  MTYPE4I PIC  X(0013).
      *    -------------------------------
           05  PCNTL PIC S9(0004) COMP.
           05  PCNTF PIC  X(0001).
           05  FILLER REDEFINES PCNTF.
               10  PCNTA PIC  X(0001).
           05  PCNTI PIC  X(0001).
      *    -------------------------------
           05  LBLSSNL PIC S9(0004) COMP.
           05  LBLSSNF PIC  X(0001).
           05  FILLER REDEFINES LBLSSNF.
               10  LBLSSNA PIC  X(0001).
           05  LBLSSNI PIC  X(0012).
      *    -------------------------------
           05  BENESSNL PIC S9(0004) COMP.
           05  BENESSNF PIC  X(0001).
           05  FILLER REDEFINES BENESSNF.
               10  BENESSNA PIC  X(0001).
           05  BENESSNI PIC  X(0009).
      *    -------------------------------
           05  MTYPE5L PIC S9(0004) COMP.
           05  MTYPE5F PIC  X(0001).
           05  FILLER REDEFINES MTYPE5F.
               10  MTYPE5A PIC  X(0001).
           05  MTYPE5I PIC  X(0013).
      *    -------------------------------
           05  ECNTL PIC S9(0004) COMP.
           05  ECNTF PIC  X(0001).
           05  FILLER REDEFINES ECNTF.
               10  ECNTA PIC  X(0001).
           05  ECNTI PIC  X(0001).
      *    -------------------------------
           05  LBLVFYL PIC S9(0004) COMP.
           05  LBLVFYF PIC  X(0001).
           05  FILLER REDEFINES LBLVFYF.
               10  LBLVFYA PIC  X(0001).
           05  LBLVFYI PIC  X(0012).
      *    -------------------------------
           05  BENEVFYL PIC S9(0004) COMP.
           05  BENEVFYF PIC  X(0001).
           05  FILLER REDEFINES BENEVFYF.
               10  BENEVFYA PIC  X(0001).
           05  BENEVFYI PIC  X(0001).
      *    -------------------------------
           05  MTYPE6L PIC S9(0004) COMP.
           05  MTYPE6F PIC  X(0001).
           05  FILLER REDEFINES MTYPE6F.
               10  MTYPE6A PIC  X(0001).
           05  MTYPE6I PIC  X(0013).
      *    -------------------------------
           05  OCNTL PIC S9(0004) COMP.
           05  OCNTF PIC  X(0001).
           05  FILLER REDEFINES OCNTF.
               10  OCNTA PIC  X(0001).
           05  OCNTI PIC  X(0001).
      *    -------------------------------
           05  MTYPE7L PIC S9(0004) COMP.
           05  MTYPE7F PIC  X(0001).
           05  FILLER REDEFINES MTYPE7F.
               10  MTYPE7A PIC  X(0001).
           05  MTYPE7I PIC  X(0016).
      *    -------------------------------
           05  QCNTL PIC S9(0004) COMP.
           05  QCNTF PIC  X(0001).
           05  FILLER REDEFINES QCNTF.
               10  QCNTA PIC  X(0001).
           05  QCNTI PIC  X(0001).
      *    -------------------------------
           05  MAPNAMEL PIC S9(0004) COMP.
           05  MAPNAMEF PIC  X(0001).
           05  FILLER REDEFINES MAPNAMEF.
               10  MAPNAMEA PIC  X(0001).
           05  MAPNAMEI PIC  X(0030).
      *    -------------------------------
           05  MADDRL1L PIC S9(0004) COMP.
           05  MADDRL1F PIC  X(0001).
           05  FILLER REDEFINES MADDRL1F.
               10  MADDRL1A PIC  X(0001).
           05  MADDRL1I PIC  X(0030).
      *    -------------------------------
           05  MADDRL2L PIC S9(0004) COMP.
           05  MADDRL2F PIC  X(0001).
           05  FILLER REDEFINES MADDRL2F.
               10  MADDRL2A PIC  X(0001).
           05  MADDRL2I PIC  X(0030).
      *    -------------------------------
           05  MCITYL PIC S9(0004) COMP.
           05  MCITYF PIC  X(0001).
           05  FILLER REDEFINES MCITYF.
               10  MCITYA PIC  X(0001).
           05  MCITYI PIC  X(0028).
      *    -------------------------------
           05  MSTATEL PIC S9(0004) COMP.
           05  MSTATEF PIC  X(0001).
           05  FILLER REDEFINES MSTATEF.
               10  MSTATEA PIC  X(0001).
           05  MSTATEI PIC  X(0002).
      *    -------------------------------
           05  MZIPCODL PIC S9(0004) COMP.
           05  MZIPCODF PIC  X(0001).
           05  FILLER REDEFINES MZIPCODF.
               10  MZIPCODA PIC  X(0001).
           05  MZIPCODI PIC  X(0010).
      *    -------------------------------
           05  MAPHONEL PIC S9(0004) COMP.
           05  MAPHONEF PIC  X(0001).
           05  FILLER REDEFINES MAPHONEF.
               10  MAPHONEA PIC  X(0001).
           05  MAPHONEI PIC  X(0012).
      *    -------------------------------
           05  MAPNOT1L PIC S9(0004) COMP.
           05  MAPNOT1F PIC  X(0001).
           05  FILLER REDEFINES MAPNOT1F.
               10  MAPNOT1A PIC  X(0001).
           05  MAPNOT1I PIC  X(0001).
      *    -------------------------------
           05  MAPNOT2L PIC S9(0004) COMP.
           05  MAPNOT2F PIC  X(0001).
           05  FILLER REDEFINES MAPNOT2F.
               10  MAPNOT2A PIC  X(0001).
           05  MAPNOT2I PIC  X(0038).
      *    -------------------------------
           05  MAPSEQ2L PIC S9(0004) COMP.
           05  MAPSEQ2F PIC  X(0001).
           05  FILLER REDEFINES MAPSEQ2F.
               10  MAPSEQ2A PIC  X(0001).
           05  MAPSEQ2I PIC  X(0004).
      *    -------------------------------
           05  MERMSG1L PIC S9(0004) COMP.
           05  MERMSG1F PIC  X(0001).
           05  FILLER REDEFINES MERMSG1F.
               10  MERMSG1A PIC  X(0001).
           05  MERMSG1I PIC  X(0079).
      *    -------------------------------
           05  MERMSG2L PIC S9(0004) COMP.
           05  MERMSG2F PIC  X(0001).
           05  FILLER REDEFINES MERMSG2F.
               10  MERMSG2A PIC  X(0001).
           05  MERMSG2I PIC  X(0079).
      *    -------------------------------
           05  MPFNUMBL PIC S9(0004) COMP.
           05  MPFNUMBF PIC  X(0001).
           05  FILLER REDEFINES MPFNUMBF.
               10  MPFNUMBA PIC  X(0001).
           05  MPFNUMBI PIC  99.
       01  EL141AO REDEFINES EL141AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MRNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MRNTIMEO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MFMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MADDRTPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTYPE1O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ICNTO PIC  Z.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTYPE2O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTO PIC  Z.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTYPE3O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACNTO PIC  Z.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTYPE4O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCNTO PIC  Z.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LBLSSNO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENESSNO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTYPE5O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECNTO PIC  Z.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LBLVFYO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENEVFYO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTYPE6O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OCNTO PIC  Z.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTYPE7O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  QCNTO PIC  Z.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAPNAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MADDRL1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MADDRL2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MCITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MZIPCODO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAPHONEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAPNOT1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAPNOT2O PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAPSEQ2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MERMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MERMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MPFNUMBO PIC  X(0002).
      *    -------------------------------
00396      EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00398  01  DFHCOMMAREA                     PICTURE X(1024).
00399      EJECT
00400 *    COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
00401      EJECT
00402 *    COPY ELCTRLR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCTRLR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
050506* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
062806* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
080106* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
041807* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
101807* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
070909* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
071910* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
102610* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
061511* 061511    2011042000002  AJRA  ADD VFY 2ND BENE TO ADDRESS TRAIL
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
021213* 021213    2012092400007  AJRA  CAUSAL STATE SEQUENCE NO
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
102413* 102413  CR2013100800001  AJRA  ADD SPECIAL RELEASE IND
022614* 022614    2013050100003  AJRA  ADD CERT CANCELLED NOTE TYPE - T
040814* 040814    2014030500002  AJRA  ADD ICD CODES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
102418* 102418  CR2018083000001  TANA  ADD ADD NEW CALL TYPE
00017 ******************************************************************
00018  01  ACTIVITY-TRAILERS.
00019      12  AT-RECORD-ID                    PIC XX.
00020          88  VALID-AT-ID                       VALUE 'AT'.
00021
00022      12  AT-CONTROL-PRIMARY.
00023          16  AT-COMPANY-CD               PIC X.
00024          16  AT-CARRIER                  PIC X.
00025          16  AT-CLAIM-NO                 PIC X(7).
00026          16  AT-CERT-NO.
00027              20  AT-CERT-PRIME           PIC X(10).
00028              20  AT-CERT-SFX             PIC X.
00029          16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
00030              88  AT-1ST-TRL-AVAIL             VALUE +4095.
00031              88  AT-LAST-TRL-AVAIL            VALUE +100.
00032              88  AT-RESV-EXP-HIST-TRL         VALUE +0.
00033              88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
00034              88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
00035              88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
00036              88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
00037              88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
00038              88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
00039              88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
00040              88  AT-DIAGNOSIS-TRL             VALUE +90.
022106             88  AT-BENEFICIARY-TRL           VALUE +91.
022106             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
061511             88  AT-VFY-2ND-BENE-NOTE-TRL     VALUE +93.
021213             88  AT-VFY-CAUSAL-STATE          VALUE +94.
                   88  AT-ERROR-MSGS-TRL            VALUE +95.
00041
00042      12  AT-TRAILER-TYPE                 PIC X.
00043          88  RESERVE-EXPENSE-TR               VALUE '1'.
00044          88  PAYMENT-TR                       VALUE '2'.
00045          88  AUTO-PAY-TR                      VALUE '3'.
00046          88  CORRESPONDENCE-TR                VALUE '4'.
00047          88  ADDRESS-TR                       VALUE '5'.
00048          88  GENERAL-INFO-TR                  VALUE '6'.
00049          88  AUTO-PROMPT-TR                   VALUE '7'.
00050          88  DENIAL-TR                        VALUE '8'.
00051          88  INCURRED-CHG-TR                  VALUE '9'.
00052          88  FORM-CONTROL-TR                  VALUE 'A'.
00053
00054      12  AT-RECORDED-DT                  PIC XX.
00055      12  AT-RECORDED-BY                  PIC X(4).
00056      12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
00057
00058      12  AT-TRAILER-BODY                 PIC X(165).
00059
00060      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
00061          16  AT-RESERVE-CONTROLS.
00062              20  AT-MANUAL-SW            PIC X.
00063                  88  AT-MANUAL-RESERVES-USED VALUE '1'.
00064              20  AT-FUTURE-SW            PIC X.
00065                  88  AT-FUTURE-RESERVES-USED VALUE '1'.
00066              20  AT-PTC-SW               PIC X.
00067                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
00068              20  AT-IBNR-SW              PIC X.
00069                  88  AT-IBNR-RESERVES-USED   VALUE '1'.
00070              20  AT-PTC-LF-SW            PIC X.
00071                  88  AT-LF-PTC-USED          VALUE '1'.
00072              20  AT-CDT-ACCESS-METHOD    PIC X.
00073                  88  AT-CDT-ROUND-NEAR       VALUE '1'.
00074                  88  AT-CDT-ROUND-HIGH       VALUE '2'.
00075                  88  AT-CDT-INTERPOLATED     VALUE '3'.
00076              20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
00077          16  AT-LAST-COMPUTED-DT         PIC XX.
101807         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
101807         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
101807         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
101807         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
00084          16  AT-EXPENSE-CONTROLS.
00085              20  AT-EXPENSE-METHOD       PIC X.
00086                  88  NO-EXPENSE-CALCULATED    VALUE '1'.
00087                  88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
00088                  88  PERCENT-OF-PMT           VALUE '3'.
00089                  88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
00090              20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
00091              20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
00092          16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
00093          16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
00094
00095          16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
00096          16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
00097
101807*        16  FILLER                      PIC X(53).
101807         16  FILLER                      PIC X(47).
00099
00100          16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
00101          16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
00102
00103          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
00104              20  AT-OPEN-CLOSE-DATE      PIC XX.
00105              20  AT-OPEN-CLOSE-TYPE      PIC X.
00106 *                    C = CLOSED
00107 *                    O = OPEN
00108              20  AT-OPEN-CLOSE-REASON    PIC X(5).
00109 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
00110
00111      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
00112          16  AT-PAYMENT-TYPE             PIC X.
00113              88  PARTIAL-PAYMENT                VALUE '1'.
00114              88  FINAL-PAYMENT                  VALUE '2'.
00115              88  LUMP-SUM-PAYMENT               VALUE '3'.
00116              88  ADDITIONAL-PAYMENT             VALUE '4'.
00117              88  CHARGEABLE-EXPENSE             VALUE '5'.
00118              88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
00119              88  VOIDED-PAYMENT                 VALUE '9'.
00120              88  TRANSFER                       VALUE 'T'.
022106             88  LIFE-INTEREST                  VALUE 'I'.
00121
00122          16  AT-CLAIM-TYPE               PIC X.
00123              88  PAID-FOR-AH                    VALUE 'A'.
00124              88  PAID-FOR-LIFE                  VALUE 'L'.
00124              88  PAID-FOR-IUI                   VALUE 'I'.
120503             88  PAID-FOR-GAP                   VALUE 'G'.
052614             88  PAID-FOR-FAM                   VALUE 'F'.
100518             88  PAID-FOR-OTH                   VALUE 'O'.
00125          16  AT-CLAIM-PREM-TYPE          PIC X.
00126              88  AT-SINGLE-PREMIUM              VALUE '1'.
00127              88  AT-O-B-COVERAGE                VALUE '2'.
00128              88  AT-OPEN-END-COVERAGE           VALUE '3'.
00129          16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
00130          16  AT-CHECK-NO                 PIC X(7).
00131          16  AT-PAID-FROM-DT             PIC XX.
00132          16  AT-PAID-THRU-DT             PIC XX.
00133          16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
013017         16  AT-ACH-PAYMENT              PIC X.
013017*        16  FILLER                      PIC X.
00135          16  AT-PAYEES-NAME              PIC X(30).
00136          16  AT-PAYMENT-ORIGIN           PIC X.
00137              88  ONLINE-MANUAL-PMT              VALUE '1'.
00138              88  ONLINE-AUTO-PMT                VALUE '2'.
00139              88  OFFLINE-PMT                    VALUE '3'.
00140          16  AT-CHECK-WRITTEN-DT         PIC XX.
00141          16  AT-TO-BE-WRITTEN-DT         PIC XX.
00142          16  AT-VOID-DATA.
00143              20  AT-VOID-DT              PIC XX.
041807*00144       20  AT-VOID-REASON          PIC X(30).
041807             20  AT-VOID-REASON          PIC X(26).
041807         16  AT-PMT-APPROVED-BY          PIC X(04).
00145          16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
00146          16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
082807         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
082807                                         PIC S99V9(5)  COMP-3.
00147          16  AT-CREDIT-INTERFACE.
00148              20  AT-PMT-SELECT-DT        PIC XX.
00149                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
00150              20  AT-PMT-ACCEPT-DT        PIC XX.
00151                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
00152              20  AT-VOID-SELECT-DT       PIC XX.
00153                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
00154              20  AT-VOID-ACCEPT-DT       PIC XX.
00155                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
00156
00157          16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
00158                  88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00159                  88  CONVERSION-PAYMENT           VALUE +99999999.
00160          16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
00161
00162          16  AT-FORCE-CONTROL            PIC X.
00163              88  PAYMENT-WAS-FORCED           VALUE '1'.
00164          16  AT-PREV-LAST-PMT-DT         PIC XX.
00165          16  AT-PREV-PAID-THRU-DT        PIC XX.
00166          16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
00167          16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
00168          16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
00169          16  AT-BENEFIT-TYPE             PIC X.
00170
00171          16  AT-EXPENSE-TYPE             PIC X.
00172          16  AT-PAYMENT-APPROVAL-SW      PIC X.
00173
00174          16  AT-PAYEE-TYPE-CD.
00175              20  AT-PAYEE-TYPE           PIC X.
00176                  88  INSURED-PAID           VALUE 'I'.
00177                  88  BENEFICIARY-PAID       VALUE 'B'.
00178                  88  ACCOUNT-PAID           VALUE 'A'.
00179                  88  OTHER-1-PAID           VALUE 'O'.
00180                  88  OTHER-2-PAID           VALUE 'Q'.
00181                  88  DOCTOR-PAID            VALUE 'P'.
00182                  88  EMPLOYER-PAID          VALUE 'E'.
00183              20  AT-PAYEE-SEQ            PIC X.
00184
00185          16  AT-CASH-PAYMENT             PIC X.
00186          16  AT-GROUPED-PAYMENT          PIC X.
00187          16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
00188          16  AT-APPROVAL-LEVEL-REQD      PIC X.
00189          16  AT-APPROVED-LEVEL           PIC X.
00190          16  AT-VOID-TYPE                PIC X.
00191              88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
00192              88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
00193          16  AT-AIG-UNEMP-IND            PIC X.
00194              88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
00195          16  AT-ASSOCIATES               PIC X.
00196              88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
00197              88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
00198
00199          16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
00200          16  AT-CV-PMT-CODE              PIC X.
00201              88  FULL-DEATH-PAYMENT         VALUE '1'.
00202              88  HALF-DEATH-PAYMENT         VALUE '2'.
00203              88  FULL-ADD-PAYMENT           VALUE '3'.
00204              88  HALF-ADD-PAYMENT           VALUE '4'.
00205              88  FULL-RIDER-PAYMENT         VALUE '5'.
00206              88  HALF-RIDER-PAYMENT         VALUE '6'.
00207              88  NON-CHG-EXP-PAYMENT        VALUE '7'.
00208              88  ADDL-PAYMENT               VALUE '8'.
00209
00210          16  AT-EOB-CODE1                PIC XXX.
00211          16  AT-EOB-CODE2                PIC XXX.
00212          16  AT-EOB-CODE3                PIC XXX.
020413         16  FILLER REDEFINES AT-EOB-CODE3.
020413             20  AT-PRINT-CLM-FORM       PIC X.
020413             20  AT-PRINT-SURVEY         PIC X.
102413             20  AT-SPECIAL-RELEASE      PIC X.
00213          16  AT-EOB-CODE4                PIC XXX.
               16  FILLER REDEFINES AT-EOB-CODE4.
                   20  AT-INT-PMT-SELECT-DT    PIC XX.
                   20  FILLER                  PIC X.
00214          16  AT-EOB-CODE5                PIC XXX.
062806         16  FILLER REDEFINES AT-EOB-CODE5.
062806             20  AT-PMT-PROOF-DT         PIC XX.
062806             20  FILLER                  PIC X.
00215
071910         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
071910             88  AT-PRINT-EOB            VALUE 'Y'.
00217
00218          16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
00219          16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
00220
00221      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
00222          16  AT-SCHEDULE-START-DT        PIC XX.
00223          16  AT-SCHEDULE-END-DT          PIC XX.
00224          16  AT-TERMINATED-DT            PIC XX.
00225          16  AT-LAST-PMT-TYPE            PIC X.
00226              88  LAST-PMT-IS-FINAL              VALUE 'F'.
00227              88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
00228          16  AT-FIRST-PMT-DATA.
00229              20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
00230              20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
00231              20  AT-1ST-PAY-THRU-DT      PIC XX.
00232          16  AT-REGULAR-PMT-DATA.
00233              20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
00234              20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
00235              20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
00236          16  AT-AUTO-PAYEE-CD.
00237              20  AT-AUTO-PAYEE-TYPE      PIC X.
00238                  88  INSURED-PAID-AUTO      VALUE 'I'.
00239                  88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
00240                  88  ACCOUNT-PAID-AUTO      VALUE 'A'.
00241                  88  OTHER-1-PAID-AUTO      VALUE 'O'.
00242                  88  OTHER-2-PAID-AUTO      VALUE 'Q'.
00243              20  AT-AUTO-PAYEE-SEQ       PIC X.
00244          16  AT-AUTO-PAY-DAY             PIC 99.
00245          16  AT-AUTO-CASH                PIC X.
00246              88  AT-CASH                      VALUE 'Y'.
00247              88  AT-NON-CASH                  VALUE 'N'.
070909*        16  FILLER                      PIC X(129).
070909         16  AT-AUTO-END-LETTER          PIC X(4).
070909         16  FILLER                      PIC X(125).
00249
00250          16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
00251          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
00252
00253      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
00254          16  AT-LETTER-SENT-DT           PIC XX.
00255          16  AT-RECEIPT-FOLLOW-UP        PIC XX.
00256          16  AT-AUTO-RE-SEND-DT          PIC XX.
00257          16  AT-LETTER-ANSWERED-DT       PIC XX.
00258          16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
00259          16  AT-LETTER-ORIGIN            PIC X.
00260              88  ONLINE-CREATION              VALUE '1' '3'.
00261              88  OFFLINE-CREATION             VALUE '2' '4'.
                   88  NAPER-ONLINE-CREATION        VALUE '3'.
                   88  NAPER-OFFLINE-CREATION       VALUE '4'.
00262          16  AT-STD-LETTER-FORM          PIC X(4).
00263          16  AT-REASON-TEXT              PIC X(70).
00264          16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
00265          16  AT-ADDRESEE-TYPE            PIC X.
00266               88  INSURED-ADDRESEE            VALUE 'I'.
00267               88  BENEFICIARY-ADDRESEE        VALUE 'B'.
00268               88  ACCOUNT-ADDRESEE            VALUE 'A'.
00269               88  PHYSICIAN-ADDRESEE          VALUE 'P'.
00270               88  EMPLOYER-ADDRESEE           VALUE 'E'.
00271               88  OTHER-ADDRESEE-1            VALUE 'O'.
00272               88  OTHER-ADDRESEE-2            VALUE 'Q'.
00273          16  AT-ADDRESSEE-NAME           PIC X(30).
00274          16  AT-INITIAL-PRINT-DATE       PIC XX.
00275          16  AT-RESEND-PRINT-DATE        PIC XX.
00276          16  AT-CORR-SOL-UNSOL           PIC X.
00277          16  AT-LETTER-PURGED-DT         PIC XX.
CIDMOD*
CIDMOD*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
CIDMOD*
CIDMOD         16  AT-CSO-REDEFINITION.
040110             20  AT-RESEND-LETTER-FORM   PIC X(4).
040110             20  AT-AUTO-CLOSE-IND       PIC X(1).
040110             20  AT-LETTER-TO-BENE       PIC X(1).
102610             20  AT-STOP-LETTER-DT       PIC X(2).
062217             20  AT-AUTH-RCVD            PIC X(1).
062217             20  FILLER                  PIC X(18).
040110*             20  FILLER                  PIC X(27).
CIDMOD             20  AT-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
CIDMOD                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
CIDMOD                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
CIDMOD             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
CIDMOD             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
CIDMOD*
CIDMOD*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
CIDMOD*
CIDMOD*        16  FILLER                      PIC X(26).
CIDMOD*
CIDMOD*        16  AT-DMD-BSR-CODE             PIC X.
CIDMOD*            88  AT-AUTOMATED-BSR              VALUE 'A'.
CIDMOD*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
CIDMOD*
CIDMOD*        16  AT-DMD-LETTER-STATUS        PIC X.
CIDMOD*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
CIDMOD*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
CIDMOD*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
CIDMOD*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
CIDMOD*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
00290
00291          16  AT-CORR-LAST-MAINT-DT       PIC XX.
00292          16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
00293
00294      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
00295          16  AT-ADDRESS-TYPE             PIC X.
00296              88  INSURED-ADDRESS               VALUE 'I'.
00297              88  BENEFICIARY-ADDRESS           VALUE 'B'.
00298              88  ACCOUNT-ADDRESS               VALUE 'A'.
00299              88  PHYSICIAN-ADDRESS             VALUE 'P'.
00300              88  EMPLOYER-ADDRESS              VALUE 'E'.
00301              88  OTHER-ADDRESS-1               VALUE 'O'.
00302              88  OTHER-ADDRESS-2               VALUE 'Q'.
00303          16  AT-MAIL-TO-NAME             PIC X(30).
00304          16  AT-ADDRESS-LINE-1           PIC X(30).
00305          16  AT-ADDRESS-LINE-2           PIC X(30).
00306          16  AT-CITY-STATE.
                   20  AT-CITY                 PIC X(28).
                   20  AT-STATE                PIC XX.
00307          16  AT-ZIP.
00308              20  AT-ZIP-CODE.
00309                  24  AT-ZIP-1ST          PIC X.
00310                      88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00311                  24  FILLER              PIC X(4).
00312              20  AT-ZIP-PLUS4            PIC X(4).
00313          16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
00314              20  AT-CAN-POSTAL-1         PIC XXX.
00315              20  AT-CAN-POSTAL-2         PIC XXX.
00316              20  FILLER                  PIC XXX.
00317          16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
061511*         16  FILLER                      PIC X(23).
061511         16  AT-VFY-2ND-BENE-SSN         PIC X(9).
061511         16  AT-VFY-2ND-BENE-VERIFIED    PIC X.
061511         16  FILLER                      PIC X(13).
00319          16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
00320          16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
00321
00322      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00323          16  AT-INFO-LINE-1              PIC X(60).
061013         16  FILLER REDEFINES AT-INFO-LINE-1.
061013             20  AT-NOTE-ERROR-NO OCCURS 15
061013                                         PIC X(4).
00324          16  AT-INFO-LINE-2              PIC X(60).
040814         16  FILLER REDEFINES AT-INFO-LINE-2.
040814             20  AT-ICD-CODE-1           PIC X(8).
040814             20  AT-ICD-CODE-2           PIC X(8).
040814             20  FILLER                  PIC X(44).
00325          16  AT-INFO-TRAILER-TYPE        PIC X.
061013             88  AT-ERRORS-NOTE          VALUE 'E'.
00326              88  AT-PAYMENT-NOTE         VALUE 'P'.
00327              88  AT-CALL-NOTE            VALUE 'C'.
00328              88  AT-MAINT-NOTE           VALUE 'M'.
00329              88  AT-CERT-CHANGE          VALUE 'X'.
080106             88  AT-APPROVAL-NOTE        VALUE 'R'.
080106             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
022614             88  AT-CERT-CANCELLED       VALUE 'T'.
00330          16  AT-CALL-TYPE                PIC X.
00331              88  AT-PHONE-CALL-IN        VALUE 'I'.
102418             88  AT-PHONE-CALL-NEW       VALUE 'N'.
00332              88  AT-PHONE-CALL-OUT       VALUE 'O'.
00333          16  AT-NOTE-CONTINUATION        PIC X.
00334              88  AT-CONTINUED-NOTE       VALUE 'X'.
071910         16  AT-EOB-CODES-EXIST          PIC X.
071910             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
00335          16  FILLER                      PIC X(35).
00336          16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
00337          16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
00338
00339      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
00340          16  AT-PROMPT-LINE-1            PIC X(60).
00341          16  AT-PROMPT-LINE-2            PIC X(60).
00342          16  AT-PROMPT-START-DT          PIC XX.
00343          16  AT-PROMPT-END-DT            PIC XX.
00344          16  FILLER                      PIC X(35).
00345          16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
00346          16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
00347
00348      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00349          16  AT-DENIAL-INFO-1            PIC X(60).
00350          16  AT-DENIAL-INFO-2            PIC X(60).
00351          16  AT-DENIAL-DT                PIC XX.
00352          16  AT-RETRACTION-DT            PIC XX.
00353          16  AT-DENIAL-REASON-CODE       PIC X(4).
050506*         16  FILLER                      PIC X(31).
050506         16  AT-DENIAL-PROOF-DT          PIC XX.
050506         16  FILLER                      PIC X(29).
00355          16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
00356          16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
00357
00358      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
00359          16  AT-OLD-INCURRED-DT          PIC XX.
00360          16  AT-OLD-REPORTED-DT          PIC XX.
00361          16  AT-OLD-ESTABLISHED-DT       PIC XX.
00362          16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
00363          16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
00364          16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
00365          16  AT-OLD-PAID-THRU-DT         PIC XX.
00366          16  AT-LAST-PMT-MADE-DT         PIC XX.
00367          16  FILLER                      PIC X(26).
00368          16  AT-OLD-DIAG-CODE            PIC X(6).
00369          16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
00370          16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
00371          16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
00372          16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
00373          16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
00374          16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
00375          16  AT-OLD-DIAG-DESCRIP         PIC X(60).
040814         16  AT-OLD-ICD-CODE-1           PIC X(8).
040814         16  AT-OLD-ICD-CODE-2           PIC X(8).
040814         16  FILLER                      PIC X(9).
00377          16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
00378
00379      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
00380          16  AT-FORM-SEND-ON-DT          PIC XX.
00381          16  AT-FORM-FOLLOW-UP-DT        PIC XX.
00382          16  AT-FORM-RE-SEND-DT          PIC XX.
00383          16  AT-FORM-ANSWERED-DT         PIC XX.
00384          16  AT-FORM-PRINTED-DT          PIC XX.
00385          16  AT-FORM-REPRINT-DT          PIC XX.
00386          16  AT-FORM-TYPE                PIC X.
00387              88  INITIAL-FORM                  VALUE '1'.
00388              88  PROGRESS-FORM                 VALUE '2'.
00389          16  AT-INSTRUCT-LN-1            PIC X(28).
00390          16  AT-INSTRUCT-LN-2            PIC X(28).
00391          16  AT-INSTRUCT-LN-3            PIC X(28).
00392          16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
00393          16  AT-FORM-ADDRESS             PIC X.
00394              88  FORM-TO-INSURED              VALUE 'I'.
00395              88  FORM-TO-ACCOUNT              VALUE 'A'.
00396              88  FORM-TO-OTHER-1              VALUE 'O'.
00397              88  FORM-TO-OTHER-2              VALUE 'Q'.
00398          16  AT-RELATED-1.
00399              20 AT-REL-CARR-1            PIC X.
00400              20 AT-REL-CLAIM-1           PIC X(7).
00401              20 AT-REL-CERT-1            PIC X(11).
00402          16  AT-RELATED-2.
00403              20 AT-REL-CARR-2            PIC X.
00404              20 AT-REL-CLAIM-2           PIC X(7).
00405              20 AT-REL-CERT-2            PIC X(11).
00406          16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
00407          16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
00408          16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
00409          16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
00410          16  AT-FORM-REM-PRINT-DT        PIC XX.
102610         16  AT-STOP-FORM-DT             PIC X(2).
00411
102610         16  FILLER                      PIC X(09).
00413          16  AT-FORM-LAST-MAINT-DT       PIC XX.
00414          16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
00415 ******************************************************************
00403      EJECT
00404 *    COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00405      EJECT
00406 *    COPY ELCBENE.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCBENE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.006                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BENEFICIARY FILE                          *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 500   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELBENE                   RKP=2,LEN=12    *
00013 *     ALTERNATE PATH1 = ELBENE2 (ALT BY NAME)    RKP=14,LEN=42   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 *                                                                *
CIDMOD*  NO  CID  MODS  TO  COPYBOOK  ELCBENE                          *
00018 ******************************************************************
013017*                   C H A N G E   L O G
013017*
013017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
013017*-----------------------------------------------------------------
013017*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
013017* EFFECTIVE    NUMBER
013017*-----------------------------------------------------------------
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
082317* 082317  CR2017082100003  PEMA  Add sub type
032019* 032019  CR2019011400002  PEMA  Add email address for ach report
013017******************************************************************
00019
00020  01  BENEFICIARY-MASTER.
00021      12  BE-RECORD-ID                PIC XX.
00022          88  VALID-BE-ID                VALUE 'BE'.
00023
00024      12  BE-CONTROL-PRIMARY.
00025          16  BE-COMPANY-CD           PIC X.
00026          16  BE-RECORD-TYPE          PIC X.
00027              88  BENEFICIARY-RECORD  VALUE 'B'.
00028              88  ADJUSTOR-RECORD     VALUE 'A'.
00029          16  BE-BENEFICIARY          PIC X(10).
00030      12  BE-CONTROL-BY-NAME.
00031          16  BE-COMPANY-CD-A1        PIC X.
00032          16  BE-RECORD-TYPE-A1       PIC X.
00033          16  BE-MAIL-TO-NAME-A1      PIC X(30).
00034          16  BE-ALTERNATE-PRIME-A1   PIC X(10).
00035
00036      12  BE-LAST-MAINT-DT            PIC XX.
00037      12  BE-LAST-MAINT-BY            PIC X(4).
00038      12  BE-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
00039
00040      12  BE-ADDRESS-DATA.
00041          16  BE-MAIL-TO-NAME         PIC X(30).
00042          16  BE-ADDRESS-LINE-1       PIC X(30).
00043          16  BE-ADDRESS-LINE-2       PIC X(30).
00044          16  BE-ADDRESS-LINE-3       PIC X(30).
00045          16  BE-CITY-STATE.
051810             20  BE-CITY             PIC X(28).
051810             20  BE-STATE            PIC XX.
00046          16  BE-ZIP-CODE.
00047              20  BE-ZIP-PRIME.
00048                  24  BE-ZIP-1ST      PIC X.
00049                      88  BE-CANADIAN-POST-CODE
00050                                          VALUE 'A' THRU 'Z'.
00051                  24  FILLER          PIC X(4).
00052              20  BE-ZIP-PLUS4        PIC X(4).
00053          16  BE-CANADIAN-POSTAL-CODE  REDEFINES  BE-ZIP-CODE.
00054              20  BE-CAN-POSTAL-1     PIC XXX.
00055              20  BE-CAN-POSTAL-2     PIC XXX.
00056              20  FILLER              PIC XXX.
00057          16  BE-PHONE-NO             PIC 9(11)     COMP-3.
00058          16  BE-GROUP-CHECKS-Y-N     PIC X.
00059
00060 ******************************************************************
00061 *    THE BE-CARRIER FIELD IS USED BY 'AIG' TO DETERMINE HOW TO   *
00062 *    SET THE CARRIER CODE IN THE PENDING CLAIM FILE.             *
00063 ******************************************************************
00064      12  BE-CARRIER                  PIC X.
00065
00066      12  BE-ADDRESS-DATA2.
00067          16  BE-MAIL-TO-NAME2        PIC X(30).
00068          16  BE-ADDRESS-LINE-12      PIC X(30).
00069          16  BE-ADDRESS-LINE-22      PIC X(30).
00070          16  BE-ADDRESS-LINE-32      PIC X(30).
00071          16  BE-CITY-STATE2.
051810             20  BE-CITY2            PIC X(28).
051810             20  BE-STATE2           PIC XX.
00072          16  BE-ZIP-CODE2.
00073              20  BE-ZIP-PRIME2.
00074                  24  BE-ZIP-1ST2     PIC X.
00075                      88  BE-CANADIAN-POST-CODE2
00076                                          VALUE 'A' THRU 'Z'.
00077                  24  FILLER          PIC X(4).
00078              20  BE-ZIP-PLUS42       PIC X(4).
00079          16  BE-CANADIAN-POSTAL-CODE2 REDEFINES  BE-ZIP-CODE2.
00080              20  BE-CAN-POSTAL-12    PIC XXX.
00081              20  BE-CAN-POSTAL-22    PIC XXX.
00082              20  FILLER              PIC XXX.
00083          16  BE-PHONE-NO2            PIC 9(11)     COMP-3.
               16  BE-ACH-DATA.
                   20  BE-ACH-YES-OR-NO    PIC X.
                       88  BE-ON-ACH       VALUE 'Y'.
                       88  BE-NOT-ON-ACH   VALUE 'N' ' '.
                   20  BE-ACH-ABA-ROUTING-NUMBER
                                           PIC X(15).
                   20  BE-ACH-BANK-ACCOUNT-NUMBER
                                           PIC X(20).
                   20  BE-ACH-SUB-TYPE     PIC XX.
032019             20  BE-ACH-EMAIL-YN     PIC X.
032019                 88  BE-EMAIL-ACH-RPT  VALUE 'Y'.
032019             20  be-ach-email-addr   PIC X(40).
00084          16  BE-BILLING-STMT-DATA.
032019*            20  BE-BSR-PHONE-NUM    PIC 9(11)     COMP-3.
00091              20  BE-BSR-FAX-NUM      PIC 9(11)     COMP-3.
00092              20  BE-OUTPUT-TYPE      PIC X.
00093                  88  BE-FAX-OUTPUT         VALUE 'F'.
00094                  88  BE-PRINT-OUTPUT       VALUE 'P' ' '.
00095
032019     12  filler                      PIC X(16).
00097 ******************************************************************
00408 *    COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
071712* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
071712     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
071712         16  CO-OV120                      PIC S9(7)V99   COMP-3.
071712         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
00410 *    COPY MPCPROD.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCPROD                             *
00004 *                            VMOD=1.010                          *
00005 *                                                                *
00006 *   MORTGAGE SYSTEM PRODUCER MASTER FILE                         *
00007 *                                                                *
00008 *   THIS COPYBOOK IS USED FOR THE ONLINE                         *
00009 *   VSAM PRODUCER MASTER FILE.                                   *
00010 *                                                                *
00011 *   FILE DESCRIPTION = PRODUCER MASTER FILE                      *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 2000 RECFORM = FIXED                           *
00015 *                                                                *
00016 *   BASE CLUSTER NAME = MPPROD                    RKP=02,LEN=22  *
00017 *       ALTERNATE PATH1 = MPPROD2 (ALT GROUPING)  RKP=48,LEN=22  *
00018 *       ALTERNATE PATH2 = MPPROD3 (PRODUCER NAME) RKP=90,LEN=56  *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
00025
00026  01  PRODUCER-MASTER.
00027      12  PD-RECORD-ID                 PIC  X(02).
00028          88  PD-VALID-ID                   VALUE 'PD'.
00029
00030 ******************************************************************
00031 *   BASE CLUSTER NAME = MPPROD                    RKP=2,LEN=22   *
00032 ******************************************************************
00033
00034      12  PD-CONTROL-PRIMARY-BATCH.
00035          16  FILLER                   PIC  X(20).
00036          16  PD-EXPIRE-DT.
00037              20  PD-EXPIRE-DT-YY      PIC  9(02).
00038              20  PD-EXPIRE-DT-MM      PIC  9(02).
00039              20  PD-EXPIRE-DT-DD      PIC  9(02).
00040      12  FILLER REDEFINES PD-CONTROL-PRIMARY-BATCH.
00041          16  PD-CONTROL-PRIMARY.
00042              20  PD-COMPANY-CD        PIC  X(01).
00043              20  PD-MSTR-CNTRL.
00044                  24  PD-CONTROL-A.
00045                      28  PD-CARRIER   PIC  X(01).
00046                      28  PD-GROUPING.
00047                          32 PD-GROUPING-PREFIX
00048                                       PIC  X(03).
00049                          32 PD-GROUPING-PRIME
00050                                       PIC  X(03).
00051                      28  PD-STATE     PIC  X(02).
00052                      28  PD-PRODUCER.
00053                          32  PD-PRODUCER-PREFIX
00054                                       PIC  X(04).
00055                          32  PD-PRODUCER-PRIME
00056                                       PIC  X(06).
00057                  24  PD-CNTRL-B.
00058                      28  PD-EXPIRE-DATE
00059                                       PIC  X(02).
00060          16  FILLER REDEFINES PD-CONTROL-PRIMARY.
00061              20  FILLER               PIC  X(01).
00062              20  PD-CGSPE-KEY         PIC  X(21).
00063          16  FILLER                   PIC  X(04).
00064      12  FILLER                       PIC  X(20).
00065
00066 ******************************************************************
00067 *      ALTERNATE PATH1 = MPPROD2 (ALT GROUPING) RKP=48,LEN=22    *
00068 ******************************************************************
00069
00070      12  PD-CONTROL-BY-VAR-GRP.
00071          16  PD-VG-CCGSP-KEYLET.
00072              20  PD-COMPANY-CD-A1     PIC  X(01).
00073              20  PD-VG-CARRIER        PIC  X(01).
00074              20  PD-VG-GROUPING       PIC  X(06).
00075              20  PD-VG-STATE          PIC  X(02).
00076              20  PD-VG-PRODUCER       PIC  X(10).
00077          16  PD-VG-DATE.
00078              24  PD-VG-EXPIRE-DATE    PIC  X(02).
00079      12  FILLER                       PIC  X(20).
00080
00081
00082 ******************************************************************
00083 *      ALTERNATE PATH2 = MPPROD3 (NAME)         RKP=90,LEN=56    *
00084 ******************************************************************
00085
00086      12  PD-CONTROL-BY-NAME.
00087          16  PD-COMPANY-CD-A2         PIC  X(01).
00088          16  PD-NAME-A2               PIC  X(30).
00089          16  PD-CGSPE-KEY-A2.
00090              20  PD-CARRIER-A2        PIC  X(01).
00091              20  PD-GROUPING-A2       PIC  X(06).
00092              20  PD-STATE-A2          PIC  X(02).
00093              20  PD-PRODUCER-A2       PIC  X(10).
00094              20  PD-EXPIRE-DATE-A2    PIC  X(02).
00095          16  PD-CURRENT-DATE-BIN-A2   PIC  X(02).
00096          16  PD-CURRENT-TIME-BIN-A2   PIC S9(04) COMP.
00097      12  FILLER                       PIC  X(20).
00098
00099 ******************************************************************
00100 *                FILE SYNCHRONIZATION DATA                       *
00101 ******************************************************************
00102
00103      12  PD-MAINT-INFORMATION.
00104          16  PD-LAST-MAINT-DATE       PIC  X(02).
00105          16  PD-LAST-MAINT-HHMMSS     PIC S9(07) COMP-3.
00106          16  PD-LAST-MAINT-USER       PIC  X(04).
00107
00108 ******************************************************************
00109 *                PRODUCER SECURITY DATA                          *
00110 ******************************************************************
00111
00112      12  PD-SECURITY-ACCESS-CODE      PIC  X(01).
00113
00114 ******************************************************************
00115 *                DATES                                           *
00116 ******************************************************************
00117
00118      12  PD-ANNIVERSARY-DATE          PIC  X(02).
00119
00120      12  PD-AR-HI-DATE.
00121          16  PD-AR-HI-POLICY-DATE     PIC  X(02).
00122          16  FILLER                   PIC  X(04).
00123      12  PD-AR-HI-POLICY-DT REDEFINES PD-AR-HI-DATE.
00124          16  PD-AR-HI-POLICY-DT-YY    PIC  9(02).
00125          16  PD-AR-HI-POLICY-DT-MM    PIC  9(02).
00126          16  PD-AR-HI-POLICY-DT-DD    PIC  9(02).
00127
00128      12  PD-ENTRY-DATE                PIC  X(02).
00129
00130      12  PD-EFFECT-DTE.
00131          16  PD-EFFECT-DATE           PIC  X(02).
00132          16  FILLER                   PIC  X(04).
00133      12  PD-EFFECT-DT REDEFINES PD-EFFECT-DTE.
00134          16  PD-EFFECT-DT-YY          PIC  9(02).
00135          16  PD-EFFECT-DT-MM          PIC  9(02).
00136          16  PD-EFFECT-DT-DD          PIC  9(02).
00137
00138      12  PD-HI-DATE.
00139          16  PD-HI-POLICY-DATE        PIC  X(02).
00140          16  FILLER                   PIC  X(04).
00141      12  PD-HI-POLICY-DT REDEFINES PD-HI-DATE.
00142          16  PD-HI-POLICY-DT-YY       PIC  9(02).
00143          16  PD-HI-POLICY-DT-MM       PIC  9(02).
00144          16  PD-HI-POLICY-DT-DD       PIC  9(02).
00145
00146      12  PD-INACTIVE-DATE             PIC  X(02).
00147
00148      12  PD-LO-DATE.
00149          16  PD-LO-POLICY-DATE        PIC  X(02).
00150          16  FILLER                   PIC  X(04).
00151      12  PD-LO-POLICY-DT REDEFINES PD-LO-DATE.
00152          16  PD-LO-POLICY-DT-YY       PIC  9(02).
00153          16  PD-LO-POLICY-DT-MM       PIC  9(02).
00154          16  PD-LO-POLICY-DT-DD       PIC  9(02).
00155
00156      12  PD-POLICIES-PURGED-DATE      PIC  X(02).
00157
00158      12  PD-PREV-DATES.
00159          16  PD-PREV-EFF-DATE         PIC  X(02).
00160          16  FILLER                   PIC  X(04).
00161          16  PD-PREV-EXP-DATE         PIC  X(02).
00162          16  FILLER                   PIC  X(04).
00163      12  PD-PREV-DTS REDEFINES PD-PREV-DATES.
00164          16  PD-PREV-EFF-DT.
00165              20  PD-PREV-EFF-DT-YY    PIC  9(02).
00166              20  PD-PREV-EFF-DT-MM    PIC  9(02).
00167              20  PD-PREV-EFF-DT-DD    PIC  9(02).
00168          16  PD-PREV-EXP-DT.
00169              20  PD-PREV-EXP-DT-YY    PIC  9(02).
00170              20  PD-PREV-EXP-DT-MM    PIC  9(02).
00171              20  PD-PREV-EXP-DT-DD    PIC  9(02).
00172
00173      12  PD-1ST-PROD-DATE             PIC  X(02).
00174
00175      12  FILLER                       PIC  X(20).
00176
00177 ******************************************************************
00178 *                MORTGAGE BILLING DATA                           *
00179 ******************************************************************
00180
00181      12  PD-CONTACT                   PIC  X(30).
00182      12  PD-BILLING-MONTHS.
00183          16  PD-BILLING-MONTH-ANNUAL  PIC  9(02).
00184          16  PD-BILLING-MONTH-SEMIANN PIC  9(02).
00185      12  PD-BILLING-ADVANCE-ARREARS   PIC  X(01).
00186          88  PD-BILL-ADVANCE              VALUE '1'.
00187          88  PD-BILL-ARREARS              VALUE '2'.
00188      12  PD-BILLING-MODE              PIC  X(01).
00189          88  PD-ANNUAL-BILL               VALUE '1'.
00190          88  PD-SEMI-ANNUAL-BILL          VALUE '2'.
00191          88  PD-QUARTERLY-BILL            VALUE '3'.
00192          88  PD-MONTHLY-BILL              VALUE '4'.
00193          88  PD-BI-MONTHLY-BILL           VALUE '5'.
00194          88  PD-SINGLE-PREM-BILL          VALUE '6'.
00195      12  PD-BILLING-GROUPING-CODE     PIC  X(06).
00196      12  PD-BILLING-SCHEDULE          PIC  X(01).
00197          88  PD-BILL-1ST-WEEK             VALUE '1'.
00198          88  PD-BILL-2ND-WEEK             VALUE '2'.
00199          88  PD-BILL-3RD-WEEK             VALUE '3'.
00200          88  PD-BILL-4TH-WEEK             VALUE '4'.
00201          88  PD-BILL-5TH-WEEK             VALUE '5'.
00202          88  PD-HOLD-BILL                 VALUE '6'.
00203          88  PD-NO-BILL                   VALUE '7'.
00204      12  PD-BILLING-SEQUENCE          PIC  X(01).
00205          88  PD-BILL-NAME-SEQU            VALUE '1'.
00206          88  PD-BILL-LOAN-SEQU            VALUE '2'.
00207          88  PD-BILL-PLCY-SEQU            VALUE '3'.
00208      12  PD-BILLING-TYPE              PIC  X(01).
00209          88  PD-LIST-BILL                 VALUE '1'.
00210          88  PD-TAPE-BILL                 VALUE '2'.
00211          88  PD-TAPE-LIST-BILL            VALUE '3'.
00212          88  PD-GROUP-BILL            VALUES ARE '1' '2' '3'.
00213          88  PD-DIRECT-BILL               VALUE '4'.
00214          88  PD-PAC                   VALUES ARE '5' 'C' 'S'.
00215          88  PD-CREDIT-CARD               VALUE '6'.
00216          88  PD-INDIV-BILL
00217                               VALUES ARE '4' '5' '6' 'C' 'S'.
00218          88  PD-GROUP-BY-POLICY           VALUE '7'.
00219          88  PD-GROUP-BY-POLICY-PAC       VALUE '8'.
00220          88  PD-GROUP-BY-POLICY-CRDC      VALUE '9'.
00221          88  PD-GROUP-BY-BILL             VALUE '7' '8' '9'.
00222          88  PD-GROUP-BY-PROD             VALUE 'A'.
00223          88  PD-EFT-CHECKING              VALUE 'C'.
00224          88  PD-EFT-SAVINGS               VALUE 'S'.
00225      12  PD-DATE-PAID                 PIC  X(02).
00226      12  PD-LAST-BILLING-DATE         PIC  X(02).
00227      12  PD-LAST-BILL-TO-DATE         PIC  X(02).
00228      12  PD-MAX-MONTHS-BILL           PIC S9(03)  COMP-3.
00229      12  PD-PAID-TO-DATE              PIC  X(02).
00230      12  PD-PREV-BILLING-DATE         PIC  X(02).
00231      12  PD-PREV-BILL-TO-DATE         PIC  X(02).
00232
00233      12  FILLER                       PIC  X(20).
00234
00235 ******************************************************************
00236 *                PERSONAL DATA                                   *
00237 ******************************************************************
00238
00239      12  PD-ADDRS                     PIC  X(30).
00240      12  PD-CITY                      PIC  X(30).
00241      12  PD-CITY-CODE                 PIC  X(04).
00242      12  PD-COUNTY-CODE               PIC  X(03).
00243      12  PD-NAME                      PIC  X(30).
00244      12  PD-PARRISH-CODE              PIC  X(03).
00245      12  PD-PERSON                    PIC  X(30).
00246      12  PD-TEL-NO.
00247          16  PD-AREA-CODE             PIC  9(03).
00248          16  PD-TEL-PRE               PIC  9(03).
00249          16  PD-TEL-NBR               PIC  9(04).
00250      12  PD-ZIP.
00251          16  PD-ZIP-PRIME             PIC  X(05).
00252          16  PD-ZIP-PLUS4             PIC  X(04).
00253      12  PD-LANGUAGE-IND              PIC  X(01).
00254          88  PD-ENGLISH                          VALUE 'E'.
00255          88  PD-FRENCH                           VALUE 'F'.
00256          88  PD-SPANISH                          VALUE 'S'.
00257
00258      12  FILLER                       PIC  X(19).
00259
00260 ******************************************************************
00261 *                REINSURANCE DATA                                *
00262 ******************************************************************
00263
00264      12  PD-REINS-TBL-CODE            PIC  X(03).
00265      12  PD-REIN-RECALC               PIC  X(01).
00266
00267      12  PD-REI-AH-FEE                PIC S9(01)V9(04) COMP-3.
00268      12  PD-REI-AH-PE                 PIC  X(01).
00269      12  PD-REI-AH-TAX                PIC S9(01)V9(04) COMP-3.
00270
00271      12  PD-REI-GROUP-A               PIC  X(06).
00272      12  PD-REI-GROUP-B               PIC  X(06).
00273
00274      12  PD-REI-LF-FEE                PIC S9(01)V9(04) COMP-3.
00275      12  PD-REI-LF-PE                 PIC  X(01).
00276      12  PD-REI-LF-TAX                PIC S9(01)V9(04) COMP-3.
00277
00278      12  PD-REI-MORT                  PIC  X(04).
00279      12  PD-REI-PRT-OW                PIC  X(01).
00280      12  PD-REI-PRT-ST                PIC  X(01).
00281
00282      12  PD-REI-ADD-FEE               PIC S9(01)V9(04) COMP-3.
00283      12  PD-REI-ADD-PE                PIC  X(01).
00284      12  PD-REI-ADD-TAX               PIC S9(01)V9(04) COMP-3.
00285
00286      12  PD-REI-DIS-FEE               PIC S9(01)V9(04) COMP-3.
00287      12  PD-REI-DIS-PE                PIC  X(01).
00288      12  PD-REI-DIS-TAX               PIC S9(01)V9(04) COMP-3.
00289
00290      12  FILLER                       PIC  X(10).
00291 ******************************************************************
00292 *                RETRO DATA                                      *
00293 ******************************************************************
00294
00295      12  PD-RET-AH                    PIC S9(01)V9(04) COMP-3.
00296      12  PD-RET-GRP                   PIC  X(06).
00297      12  PD-RET-LF                    PIC S9(01)V9(04) COMP-3.
00298      12  PD-RET-MIN-LOSS-A            PIC SV9(03)      COMP-3.
00299      12  PD-RET-MIN-LOSS-L            PIC SV9(03)      COMP-3.
00300      12  PD-RET-P-E                   PIC  X(01).
00301      12  PD-RET-ST-TAX-USE            PIC  X(01).
00302          88  PD-CHARGE-ST-TAXES-ON-RETRO      VALUE 'Y' 'E' 'P'.
00303          88  PD-TAXES-NOT-IN-RETRO            VALUE 'N' ' '.
00304      12  PD-RET-Y-N                   PIC  X(01).
00305      12  PD-RET-ADD                   PIC S9(01)V9(04) COMP-3.
00306      12  PD-RET-MIN-LOSS-ADD          PIC SV9(03)      COMP-3.
00307      12  PD-RET-DIS                   PIC S9(01)V9(04) COMP-3.
00308      12  PD-RET-MIN-LOSS-DIS          PIC SV9(03)      COMP-3.
00309
00310      12  FILLER                       PIC  X(10).
00311
00312 ******************************************************************
00313 *                     MANAGEMENT OPTIONS                         *
00314 ******************************************************************
00315
00316      12  PD-DEFAULT-UNWTR-CODE        PIC  X(03).
00317      12  PD-LAPSE-NOTICE-CNTL         PIC  X(01).
00318      12  PD-CORRESPONDENCE-CNTL       PIC  X(01).
00319      12  PD-RETAIN-BILLING-DATA-MTHS  PIC S9(03)  COMP-3.
00320      12  PD-RETAIN-CLAIM-DATA-MTHS    PIC S9(03)  COMP-3.
00321      12  PD-RETAIN-COMMISSION-MTHS    PIC S9(03)  COMP-3.
00322      12  PD-RETAIN-DELINQUENCY-MTHS   PIC S9(03)  COMP-3.
00323      12  PD-RETAIN-INSD-PROFILE-MTHS  PIC S9(03)  COMP-3.
00324      12  PD-RETAIN-INS-COVERAGE-MTHS  PIC S9(03)  COMP-3.
00325      12  PD-RETAIN-STATUS-DISP-MTHS   PIC S9(03)  COMP-3.
00326      12  PD-NUM-BILLING-CYCLES-RETAINED
00327                                       PIC S9(03)  COMP-3.
00328      12  PD-RETAIN-UNDERWRITER-HST-MTHS
00329                                       PIC S9(03)  COMP-3.
00330
00331      12  FILLER                       PIC X(098).
00332
00333
00334 ******************************************************************
00335 *                MISCELLANEOUS DATA                              *
00336 ******************************************************************
00337
00338      12  PD-AH-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
00339      12  PD-AUTO-REFUND-SW            PIC  X(01).
00340          88  PD-AUTO-REFUNDS-USED             VALUE 'Y'.
00341          88  PD-AUTO-REFUNDS-NOT-USED         VALUE 'N' ' '.
00342      12  PD-BUSINESS-TYPE             PIC  9(02).
00343      12  PD-CAL-TABLE                 PIC  X(02).
00344      12  PD-COMMENTS.
00345          16  PD-COMMENT-LINE          PIC  X(50)
00346                                            OCCURS 5 TIMES.
00347      12  PD-EMPLOYER-STMT-USED        PIC  X(01).
00348      12  PD-GROUPED-CHECKS-Y-N        PIC  X(01).
00349      12  PD-IG                        PIC  X(01).
00350          88  PD-HAS-INDIVIDUAL                VALUE 'I'
00351                                                     '1'.
00352          88  PD-HAS-GROUP                     VALUE 'G'
00353                                                     '2'.
00354      12  PD-LF-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
00355      12  PD-REPORT-CODE-1             PIC  X(10).
00356      12  PD-REPORT-CODE-2             PIC  X(10).
00357      12  PD-RPT045A-SWITCH            PIC  X(01).
00358          88  PD-RPT045A-OFF                VALUE 'N'.
00359      12  PD-SPECIAL-BILLING-FREQ      PIC  X(01).
00360          88  PD-HAS-SPECIAL-BILL-FREQ         VALUE 'Y'.
00361          88  PD-NO-SPECIAL-BILL-FREQ          VALUE 'N' ' '.
00362      12  PD-STATUS                    PIC  X(01).
00363          88  PD-STATUS-ACTIVE                 VALUE '0'.
00364          88  PD-STATUS-INACTIVE               VALUE '1'.
00365      12  PD-STD-AH-TYPE               PIC  X(02).
00366      12  PD-TAX-NUMBER                PIC  X(11).
00367      12  PD-TOL-CLM                   PIC S9(03)V9(02) COMP-3.
00368      12  PD-USER-FIELDS.
00369          16  PD-USER-FLD-1            PIC  X(02).
00370          16  PD-USER-FLD-2            PIC  X(02).
00371          16  PD-USER-FLD-3            PIC  X(02).
00372          16  PD-USER-FLD-4            PIC  X(02).
00373          16  PD-USER-FLD-5            PIC  X(02).
00374      12  PD-USER-SELECT-OPTIONS.
00375          16  PD-USER-SELECT-1         PIC  X(10).
00376          16  PD-USER-SELECT-2         PIC  X(10).
00377          16  PD-USER-SELECT-3         PIC  X(10).
00378          16  PD-USER-SELECT-4         PIC  X(10).
00379          16  PD-USER-SELECT-5         PIC  X(10).
00380      12  PD-DIS-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
00381      12  PD-ADD-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
00382      12  FILLER                       PIC  X(20).
00383
00384 ******************************************************************
00385 *                CLIENT USE AREAS                                *
00386 ******************************************************************
00387
00388      12  PD-CLIENT-USE-AREA-1         PIC  X(30).
00389      12  PD-CLIENT-USE-AREA-2         PIC  X(30).
00390      12  PD-CLIENT-USE-AREA-3         PIC  X(11).
00391      12  PD-CLIENT-USE-AREA-4         PIC  X(30).
00392      12  PD-CLIENT-USE-AREA-5         PIC  X(30).
00393      12  PD-CLIENT-USE-AREA-6         PIC  X(11).
00394      12  PD-CLIENT-USE-AREA-7         PIC  X(30).
00395      12  PD-CLIENT-USE-AREA-8         PIC  X(30).
00396      12  PD-CLIENT-USE-AREA-9         PIC  X(11).
00397
00398 ******************************************************************
00399 *                TRANSFER DATA                                   *
00400 ******************************************************************
00401      12  PD-TRANSFERRED-FROM.
00402          16  PD-TRNFROM-CARRIER       PIC  X(01).
00403          16  PD-TRNFROM-GROUPING.
00404              20  PD-TRNFROM-GRP-PREFIX
00405                                       PIC  X(03).
00406              20  PD-TRNFROM-GRP-PRIME PIC  X(03).
00407          16  PD-TRNFROM-STATE         PIC  X(02).
00408          16  PD-TRNFROM-PRODUCER.
00409              20  PD-TRNFROM-PROD-PREFIX
00410                                       PIC  X(04).
00411              20  PD-TRNFROM-PROD-PRIME
00412                                       PIC  X(06).
00413          16  PD-TRNFROM-DATE          PIC  X(02).
00414      12  PD-TRANSFERRED-TO.
00415          16  PD-TRNTO-CARRIER         PIC  X(01).
00416          16  PD-TRNTO-GROUPING.
00417              20  PD-TRNTO-GRP-PREFIX  PIC  X(03).
00418              20  PD-TRNTO-GRP-PRIME   PIC  X(03).
00419          16  PD-TRNTO-STATE           PIC  X(02).
00420          16  PD-TRNTO-PRODUCER.
00421              20  PD-TRNTO-PROD-PREFIX PIC  X(04).
00422              20  PD-TRNTO-PROD-PRIME  PIC  X(06).
00423          16  PD-TRNTO-DATE            PIC  X(02).
00424      12  FILLER                       PIC  X(20).
00425
00426 ******************************************************************
00427 *                MORTGAGE PLANS SOLD                             *
00428 ******************************************************************
00429
00430      12  PD-PLANS-SOLD.
00431          16  PD-PRODUCER-PLANS  OCCURS 40 TIMES
00432                                 INDEXED BY PD-PLAN-NDX
00433                                            PD-PLAN-NDX2.
00434              20  PD-INDIVIDUAL-PLAN.
00435                  24  PD-PLAN-CODE     PIC  X(02).
00436                  24  PD-PLAN-REVISION PIC  X(03).
00437              20  PD-IBNR-PERCENT      PIC S9(01)V9(04) COMP-3.
00438      12  FILLER                       PIC  X(54).
00439
00440 ******************************************************************
00441 *                 AGENT AND COMMISSION DATA                      *
00442 ******************************************************************
00443
00444      12  PD-COMMISSION-INFORMATION.
00445          16  PD-REMIT-TO              PIC S9(03)   COMP-3.
00446          16  PD-RECALCULATION-SW      PIC  X(01).
00447              88  PD-RECALC-DETAIL             VALUE 'Y'.
00448              88  PD-RECALC-NO-DETAIL          VALUE 'I'.
00449              88  PD-IGNORE-RECALC             VALUE 'N'.
00450              88  PD-VALID-RECALCULATION-SW    VALUE 'Y' 'I' 'N'.
00451          16  PD-AGENT-DATA.
00452              20  PD-AGENT-ENTRY       OCCURS 5 TIMES
00453                                     INDEXED BY PD-AGENT-NDX
00454                                                PD-AGENT-NDX2.
00455                  24  PD-AGENT-NUMBER  PIC  X(10).
00456                  24  PD-AGENT-TYPE    PIC  X(01).
00457                      88  PD-AGENT-TYPE-A      VALUE 'C' 'D'.
00458                      88  PD-AGENT-TYPE-G      VALUE 'O' 'R'
00459                                                     'P' 'T'
00460                                                     'W'.
00461                      88  PD-AGENT-GROSS       VALUE 'C'.
00462                      88  PD-AGENT-REINS       VALUE 'R'.
00463                      88  PD-AGENT-GROSS-REINS VALUE 'D'.
00464                      88  PD-OVERWRITE-GROSS   VALUE 'O'.
00465                      88  PD-OVERWRITE-GROSS-REINS
00466                                           VALUE 'P'.
00467                      88  PD-OVERWRITE-REINS   VALUE 'T'.
00468                      88  PD-REINS-ONLY        VALUE 'W'.
00469                      88  PD-VALID-AGENT-TYPE  VALUE 'C' 'R'
00470                                                 'D' 'O' 'P'
00471                                                 'T' 'W'.
00472                  24  PD-COMMISSION-BILLED-PAID
00473                                       PIC  X(01).
00474                      88  PD-AGENT-BILLED      VALUE 'B'.
00475                      88  PD-AGENT-PAID        VALUE 'P'.
00476                  24  PD-COMP-RECALC-FLAG
00477                                       PIC  X(01).
00478                      88  PD-BYPASS-RECALC     VALUE 'N'.
00479                      88  PD-VALID-RECALC-FLAG VALUE ' ' 'N'.
00480      12  FILLER                       PIC  X(55).
00481
00482 ******************************************************************
00483 *                BANK DATA                                       *
00484 ******************************************************************
00485
00486      12  PD-BANK-ACCOUNT-NUMBER       PIC  X(20).
00487      12  PD-BANK-TRANSIT-NUMBER.
00488          16  PD-FEDERAL-NUMBER        PIC  X(04).
00489          16  PD-BANK-NUMBER           PIC  X(04).
00490      12  PD-CHARGE-CARD-EXP-DT        PIC  X(02).
00491      12  PD-CHARGE-CARD-TYPE          PIC  X(02).
00492          88  PD-AMERICAN-EXPRESS                 VALUE 'AE'.
00493          88  PD-CARTE-BLANCHE                    VALUE 'CB'.
00494          88  PD-DINERS-CLUB                      VALUE 'DN'.
00495          88  PD-DISCOVER                         VALUE 'DS'.
00496          88  PD-MASTER-CARD                      VALUE 'MC'.
00497          88  PD-VISA                             VALUE 'VI'.
00498      12  PD-SIGNATURE-NAME            PIC  X(25).
00499      12  PD-AUTHORIZATION-SW          PIC  X(01).
00500 ******************************************************************
00501 *                GENERIC FILLER                                  *
00502 ******************************************************************
00503
00504      12  PD-DATE-TEST                 PIC S9(08) COMP.
00505      12  FILLER                       PIC  X(62).
00506
00507 ******************************************************************
      *                                COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                ACTIVITY-TRAILERS ACCOUNT-MASTER
                                BENEFICIARY-MASTER
                                COMPENSATION-MASTER PRODUCER-MASTER
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL141' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00413
00414      MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.
00415      MOVE '5'                        TO  DC-OPTION-CODE.
00416      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT.
00417      MOVE DC-GREG-DATE-1-EDIT        TO  SAVE-DATE.
00418      MOVE DC-BIN-DATE-1              TO  SAVE-BIN-DATE.
00419      MOVE +2                         TO  EMI-NUMBER-OF-LINES.
00420      MOVE ZEROS                      TO  NDX.
00421
00422  0001-PROCESSING-EXITS.
00423      MOVE DFHCOMMAREA                TO  PROGRAM-INTERFACE-BLOCK.
00424
00425      IF EIBCALEN NOT GREATER THAN ZEROS
00426          GO TO 9000-UNAUTHERR.
00427
00428      
      * EXEC CICS  HANDLE CONDITION
00429 *        ERROR      (9990-ERROR)
00430 *        PGMIDERR   (9600-PGMIDERR)
00431 *    END-EXEC.
      *    MOVE '"$.L                  ! " #00005367' TO DFHEIV0
           MOVE X'22242E4C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035333637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00432
00433      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00434          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00435              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00436              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00437              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00438              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00439              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00440              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00441              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00442              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00443          ELSE
00444              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00445              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00446              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00447              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00448              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00449              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00450              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00451              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00452
00453      IF EIBTRNID EQUAL WS-TRANS-ID
00454          IF EIBAID EQUAL DFHCLEAR
00455              GO TO 9400-CLEAR
00456          ELSE
00457              GO TO 0200-RECEIVE.
00458
00459  0100-FIRST-TIME-IN.
00460 ******************************************************************
00461 *    ON FIRST ENTRY INTO THE PROGRAM, THE CLAIM RECORD IS READ   *
00462 *    AND THE CURRENT ADDRESS RECORDS THAT EXIST FOR THE CLAIM    *
00463 *    ARE HIGHLIGHTED AND THE NUMBER OF ADDRESS RECORDS THAT      *
00464 *    EXIST FOR EACH ADDRESS TYPE ARE DISPLAYED.                  *
00465 ******************************************************************
00466
00467      MOVE LOW-VALUES                 TO  EL141AO.
00468      MOVE -1                         TO  MFMAINTL.
00469
00470      MOVE SPACES                     TO  PI-PREV-MAINT-CODE
00471                                          PI-PREV-ADDR-TYPE.
00472
00473      MOVE '141A'                     TO  PI-CURRENT-SCREEN-NO.
00474      MOVE SAVE-BIN-DATE              TO  WS-CURR-DATE-BIN.
00475
00476      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.
00477      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.
00478      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.
00479      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.
00480
00481      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')
00482          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')
00483              IF PI-MAINT IS EQUAL TO 'A'
00484                  MOVE 'S'            TO  MFMAINTI
00485                                          WS-MAINT-FUNC-SW
00486                  MOVE 'I1'           TO  MADDRTPI
00487                                          WS-ADDR-TYPE-SW
00488                  GO TO 1000-SHOW-ADDRESS.
00489
00490      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.
061511     PERFORM 6700-GET-VFY-BENE-IND THRU 6700-EXIT.
061511     PERFORM 6750-GET-APPROVAL-LEVEL THRU 6750-EXIT.
00491      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
00492
00493      GO TO 8100-SEND-INITIAL-MAP.
00494
00495      EJECT
00496  0200-RECEIVE.
00497
00498      MOVE LOW-VALUES                 TO  EL141AI.
00499
00500      IF PI-PROCESSOR-ID = 'LGXX'
00501          NEXT SENTENCE
00502      ELSE
00503          
      * EXEC CICS READQ TS
00504 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00505 *            INTO    (SECURITY-CONTROL)
00506 *            LENGTH  (SC-COMM-LENGTH)
00507 *            ITEM    (SC-ITEM)
00508 *        END-EXEC
      *    MOVE '*$II   L              ''   #00005444' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00509          MOVE SC-CLAIMS-DISPLAY (14) TO  PI-DISPLAY-CAP
00510          MOVE SC-CLAIMS-UPDATE  (14) TO  PI-MODIFY-CAP
00511          IF NOT DISPLAY-CAP
00512              MOVE 'READ'             TO  SM-READ
00513              PERFORM 9995-SECURITY-VIOLATION
00514              MOVE ER-0070            TO  EMI-ERROR
00515              MOVE -1                 TO  MFMAINTL
00516              MOVE AL-UABON           TO  MFMAINTA
00517              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00518              GO TO 8100-SEND-INITIAL-MAP.
00519
00520      IF EIBAID IS EQUAL TO DFHPA1 OR DFHPA2 OR DFHPA3
00521          MOVE LOW-VALUES             TO  EL141AO
00522          MOVE ER-0008                TO  EMI-ERROR
00523          MOVE -1                     TO  MPFNUMBL
00524          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00525          GO TO 8200-SEND-DATAONLY.
00526
00527      
      * EXEC CICS RECEIVE
00528 *        MAP      (WS-MAP-NAME)
00529 *        MAPSET   (WS-MAPSET-NAME)
00530 *        INTO     (EL141AI)
00531 *    END-EXEC.
           MOVE LENGTH OF
            EL141AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005468' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL141AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00532
00533      IF MPFNUMBL IS EQUAL TO +0
00534          GO TO 0300-CHECK-PFKEYS.
00535
00536      IF EIBAID IS NOT EQUAL TO DFHENTER
00537          MOVE ER-0004                TO  EMI-ERROR
00538          GO TO 0320-INPUT-ERROR.
00539
00540      IF (MPFNUMBO NUMERIC) AND (MPFNUMBO GREATER 0 AND LESS 25)
00541          MOVE PF-VALUES (MPFNUMBI)   TO  EIBAID
00542      ELSE
00543          MOVE ER-0029                TO  EMI-ERROR
00544          GO TO 0320-INPUT-ERROR.
00545
00546  0300-CHECK-PFKEYS.
00547
00548      IF EIBAID IS EQUAL TO DFHPF12
00549          MOVE XCTL-010               TO  PGM-NAME
00550          GO TO 9300-XCTL.
00551
00552      IF EIBAID IS EQUAL TO DFHPF23
00553          MOVE EIBAID                 TO  PI-ENTRY-CD-1
00554          MOVE XCTL-005               TO  PGM-NAME
00555          GO TO 9300-XCTL.
00556
00557      IF EIBAID IS EQUAL TO DFHPF24
00558          MOVE XCTL-126               TO  PGM-NAME
00559          GO TO 9300-XCTL.
00560
00561      IF EIBAID IS EQUAL TO DFHPF1
00562          MOVE XCTL-114               TO  PGM-NAME
00563          GO TO 9300-XCTL.
00564
00565      IF EIBAID IS EQUAL TO DFHPF2
00566         GO TO  0330-EDIT-DATA.
00567
00568      IF EIBAID IS EQUAL TO DFHENTER
00569          GO TO 0330-EDIT-DATA.
00570
00571      MOVE ER-0029                    TO  EMI-ERROR.
00572
00573  0320-INPUT-ERROR.
00574
00575      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00576      MOVE -1                         TO  MPFNUMBL.
00577
00578      GO TO 8200-SEND-DATAONLY.
00579
00580  0330-EDIT-DATA.
00581
00582       MOVE SPACES                    TO  MERMSG1O
00583                                          MAPNOT1O
00584                                          MAPNOT2O.
00585
00586      MOVE  MFMAINTI                  TO  WS-MAINT-FUNC-SW.
00587
00588      MOVE  MADDRTPI                  TO  WS-ADDR-TYPE-SW.
00589
00590      IF VALID-FUNCTION-ENTERED
00591          NEXT SENTENCE
00592      ELSE
00593          MOVE ER-0023                TO  EMI-ERROR
00594          MOVE -1                     TO  MFMAINTL
00595          MOVE AL-UABON               TO  MFMAINTA
00596          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00597          GO TO 8200-SEND-DATAONLY.
00598
00599      IF VALID-TYPE-ENTERED
00600          NEXT SENTENCE
00601      ELSE
00602          MOVE ER-0136                TO  EMI-ERROR
00603          MOVE -1                     TO  MADDRTPL
00604          MOVE AL-UABON               TO  MADDRTPA
00605          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00606          GO TO 8200-SEND-DATAONLY.
00607
00608      IF (WS-ADDR-SEQ NUMERIC)
00609        AND
00610         (WS-ADDR-SEQ NOT LESS THAN '0' AND
00611                      NOT GREATER THAN '9')
00612         NEXT SENTENCE
00613      ELSE
00614         MOVE ER-0136                 TO  EMI-ERROR
00615         MOVE -1                      TO  MADDRTPL
00616         MOVE AL-UABON                TO  MADDRTPA
00617         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00618         GO TO 8200-SEND-DATAONLY.
00619
00620      IF WS-ADDR-SEQ EQUAL '0'
00621         IF WS-ADDR-TYPE EQUAL 'A' OR 'B'
00622             NEXT SENTENCE
00623         ELSE
00624             MOVE ER-0136                 TO  EMI-ERROR
00625             MOVE -1                      TO  MADDRTPL
00626             MOVE AL-UABON                TO  MADDRTPA
00627             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00628             GO TO 8200-SEND-DATAONLY.
00629
00630  0330-EDIT-CONTINUE.
00631      IF LOOKUP-REQUIRED
00632          NEXT SENTENCE
00633      ELSE
00634          IF (WS-ADDR-TYPE EQUAL 'A' ) AND
00635             (WS-ADDR-SEQ EQUAL '0' OR '9')
00636              MOVE ER-0277       TO  EMI-ERROR
00637              MOVE -1            TO  MFMAINTL
00638              MOVE AL-UABON      TO  MFMAINTA
00639              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00640              GO TO 8200-SEND-DATAONLY
00641          ELSE
00642             IF (WS-ADDR-TYPE EQUAL 'B' ) AND
00643                (WS-ADDR-SEQ EQUAL '0' OR '9')
00644                  MOVE ER-0277   TO  EMI-ERROR
00645                  MOVE -1        TO  MFMAINTL
00646                  MOVE AL-UABON  TO  MFMAINTA
00647                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00648                  GO TO 8200-SEND-DATAONLY.
00649
00650      IF NOT MODIFY-CAP
00651         IF LOOKUP-REQUIRED
00652            NEXT SENTENCE
00653         ELSE
00654            MOVE 'UPDATE'                 TO  SM-READ
00655            PERFORM 9995-SECURITY-VIOLATION
00656            MOVE ER-0070                  TO  EMI-ERROR
00657            MOVE -1                       TO  MFMAINTL
00658            MOVE AL-UABON                 TO  MFMAINTA
00659            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00660            GO TO 8100-SEND-INITIAL-MAP.
00661
00662      IF EIBAID IS EQUAL TO DFHPF2
00663          GO TO 5000-UPDATE-VIA-PF2.
00664
00665      IF CHANGE-REQUIRED
00666          GO TO 2000-CHANGE-ADDRESS.
00667
00668      IF ADD-REQUIRED
00669          GO TO 3000-ADD-ADDRESS.
00670
00671      IF DELETE-REQUIRED
00672         GO TO 4000-DELETE-ADDRESS.
00673
00674      EJECT
00675  1000-SHOW-ADDRESS.
00676
00677      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.
00678      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.
00679      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.
00680      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.
00681
00682      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.
00683
00684      IF WS-ADDR-TYPE EQUAL 'B'
00685         IF WS-ADDR-SEQ EQUAL '0' OR '9'
00686            GO TO 1100-READ-BENEFICIARY-RECORD.
00687
00688      IF WS-ADDR-TYPE EQUAL 'A'
00689         IF WS-ADDR-SEQ EQUAL '0'
00690             IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
00691                 GO TO 1250-READ-PRODUCER
00692             ELSE
00693                 GO TO 1200-READ-ACCT.
00694
00695      IF WS-ADDR-TYPE EQUAL 'I'
00696         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
00697      ELSE
00698      IF WS-ADDR-TYPE EQUAL 'B'
00699         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
00700         ADD +10                      TO  ELTRLR-SEQUENCE-NO
00701      ELSE
00702      IF WS-ADDR-TYPE EQUAL 'A'
00703         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
00704         ADD +20                      TO  ELTRLR-SEQUENCE-NO
00705      ELSE
00706      IF WS-ADDR-TYPE EQUAL 'P'
00707         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
00708         ADD +30                      TO  ELTRLR-SEQUENCE-NO
00709      ELSE
00710      IF WS-ADDR-TYPE EQUAL 'E'
00711         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
00712         ADD +40                      TO  ELTRLR-SEQUENCE-NO
00713      ELSE
00714      IF WS-ADDR-TYPE EQUAL 'O'
00715         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
00716         ADD +50                      TO  ELTRLR-SEQUENCE-NO
00717      ELSE
00718      IF WS-ADDR-TYPE EQUAL 'Q'
00719         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
00720         ADD +60                      TO  ELTRLR-SEQUENCE-NO.
00721
00722      
      * EXEC CICS HANDLE CONDITION
00723 *        NOTFND   (1010-NOTFND-ELTRLR)
00724 *        NOTOPEN  (1020-NOTOPEN-ELTRLR)
00725 *    END-EXEC.
      *    MOVE '"$IJ                  ! # #00005663' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00726
00727      PERFORM 7050-READ-ELTRLR THRU 7050-EXIT.
00728
00729      MOVE 'T'                        TO  WS-ADDR-SW.
00730      GO TO 1500-CONTINUE-SHOW.
00731
00732  1010-NOTFND-ELTRLR.
00733
00734      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')
00735          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')
00736              IF PI-MAINT IS EQUAL TO 'A'
00737                  MOVE ER-0265        TO  EMI-ERROR
00738                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00739                  MOVE PI-CLAIM-NO    TO  EMI-TEXT-VARIABLE (1).
00740
00741      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')
00742          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')
00743              IF PI-MAINT IS EQUAL TO 'A'
00744                  MOVE 'A'            TO  MFMAINTI
00745                                          WS-MAINT-FUNC-SW
00746                  MOVE 'S'            TO  PI-MAINT
00747                  MOVE ER-0799        TO  EMI-ERROR
00748                  MOVE -1             TO  MFMAINTL
00749                  MOVE AL-UABON       TO  MFMAINTA
00750                                          MADDRTPA
00751                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00752                  GO TO 8100-SEND-INITIAL-MAP.
00753
00754      IF WS-ADDR-TYPE IS EQUAL TO 'A'
00755          IF WS-ADDR-SEQ IS EQUAL TO '9'
00756              IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
00757                  GO TO 1250-READ-PRODUCER
00758              ELSE
00759                  GO TO 1200-READ-ACCT.
00760
00761      MOVE LOW-VALUES                 TO  EL141AO.
00762      MOVE ER-0135                    TO  EMI-ERROR.
00763      MOVE -1                         TO  MADDRTPL.
00764      MOVE AL-UABON                   TO  MADDRTPA
00765                                          MFMAINTA.
00766      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.
00767      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.
00768      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00769      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
00770      GO TO 8100-SEND-INITIAL-MAP.
00771
00772  1020-NOTOPEN-ELTRLR.
00773
00774      MOVE LOW-VALUES                 TO  EL141AI.
00775      MOVE ER-0172                    TO  EMI-ERROR.
00776      MOVE -1                         TO  MFMAINTL.
00777      MOVE AL-UABON                   TO  MFMAINTA.
00778      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.
00779      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00780      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
00781      GO TO 8100-SEND-INITIAL-MAP.
00782
00783      EJECT
00784  1100-READ-BENEFICIARY-RECORD.
00785
00786      
      * EXEC CICS HANDLE CONDITION
00787 *        NOTFND    (1110-NO-BENE)
00788 *        NOTOPEN   (1120-BENE-NOTOPEN)
00789 *    END-EXEC.
      *    MOVE '"$IJ                  ! $ #00005727' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00790
00791      MOVE PI-COMPANY-CD              TO  ELBENE-COMPANY-CD.
00792      MOVE 'B'                        TO  ELBENE-RECORD-TYPE.
00793      MOVE CL-BENEFICIARY             TO  ELBENE-BENEFICIARY.
00794
00795      
      * EXEC CICS READ
00796 *        DATASET (ELBENE-FILE-ID)
00797 *        SET     (ADDRESS OF BENEFICIARY-MASTER)
00798 *        RIDFLD  (ELBENE-KEY)
00799 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005736' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00800
00801      MOVE 'B'                        TO  WS-ADDR-SW.
00802      GO TO 1500-CONTINUE-SHOW.
00803
00804  1110-NO-BENE.
00805
00806      MOVE LOW-VALUES                 TO  EL141AO.
00807      MOVE ER-2599                    TO  EMI-ERROR.
00808      MOVE -1                         TO  MADDRTPL.
00809      MOVE AL-UABON                   TO  MADDRTPA.
00810      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.
00811      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00812      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
00813      GO TO 8100-SEND-INITIAL-MAP.
00814
00815  1120-BENE-NOTOPEN.
00816      MOVE LOW-VALUES                 TO  EL141AO.
00817      MOVE ER-7675                    TO  EMI-ERROR.
00818      MOVE -1                         TO  MFMAINTL.
00819      MOVE AL-UABON                   TO  MFMAINTA.
00820      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.
00821      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00822      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
00823      GO TO 8100-SEND-INITIAL-MAP.
00824
00825      EJECT
00826  1200-READ-ACCT.
00827 ******************************************************************
00828 *    READ THE ACCOUNT MASTER RECORD IF THE ADDRESS TYPE          *
00829 *    SELECTED IS A0 (ZERO).                                      *
00830 ******************************************************************
00831
00832      MOVE PI-COMPANY-CD              TO  ERACCT-COMPANY-CD.
00833      MOVE PI-CARRIER                 TO  ERACCT-CARRIER.
00834      MOVE CL-CERT-GROUPING           TO  ERACCT-GROUPING.
00835      MOVE CL-CERT-STATE              TO  ERACCT-STATE.
00836      MOVE CL-CERT-ACCOUNT            TO  ERACCT-ACCOUNT.
00837      MOVE CL-CERT-EFF-DT             TO  ERACCT-EFF-DT.
00838      MOVE SPACES                     TO  WS-HOLD-ERACCT-RECORD.
00839
00840      
      * EXEC CICS  HANDLE CONDITION
00841 *        NOTFND  (1230-NO-ACCT)
00842 *        NOTOPEN (1240-ACCT-NOTOPEN)
00843 *    END-EXEC.
      *    MOVE '"$IJ                  ! % #00005781' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035373831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00844
00845      
      * EXEC CICS  STARTBR
00846 *        RIDFLD   (ERACCT-KEY)
00847 *        DATASET  (ERACCT-FILE-ID)
00848 *        GTEQ
00849 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005786' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00850
00851      MOVE ERACCT-PARTIAL-KEY         TO  ERACCT-KEY-SAVE.
00852
00853  1210-READNEXT-ACCT.
00854
00855      
      * EXEC CICS  HANDLE CONDITION
00856 *        ENDFILE (1230-NO-ACCT)
00857 *        NOTFND  (1210-READNEXT-ACCT)
00858 *    END-EXEC.
      *    MOVE '"$''I                  ! & #00005796' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035373936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00859
00860      
      * EXEC CICS  READNEXT
00861 *        SET      (ADDRESS OF ACCOUNT-MASTER)
00862 *        DATASET  (ERACCT-FILE-ID)
00863 *        RIDFLD   (ERACCT-KEY)
00864 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005801' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00865
00866      MOVE 'A'                        TO  WS-ADDR-SW.
00867
00868      IF ERACCT-PARTIAL-KEY NOT = ERACCT-KEY-SAVE
00869          IF WS-HOLD-ERACCT-RECORD IS EQUAL TO SPACES
00870              GO TO 1230-NO-ACCT
00871          ELSE
00872              MOVE WS-HOLD-ERACCT-RECORD  TO  ACCOUNT-MASTER
00873              GO TO 1300-CHECK-COMP.
00874
00875      IF ERACCT-EFF-DT IS EQUAL TO HIGH-VALUES
00876          GO TO 1300-CHECK-COMP
00877      ELSE
00878          MOVE ACCOUNT-MASTER         TO  WS-HOLD-ERACCT-RECORD
00879          GO TO 1210-READNEXT-ACCT.
00880
00881  1230-NO-ACCT.
00882
00883      MOVE LOW-VALUES                 TO  EL141AI.
00884      MOVE ER-0278                    TO  EMI-ERROR.
00885      MOVE -1                         TO  MADDRTPL.
00886      MOVE AL-UABON                   TO  MADDRTPA.
00887      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.
00888      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00889      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
00890      GO TO 8100-SEND-INITIAL-MAP.
00891
00892  1240-ACCT-NOTOPEN.
00893
00894      MOVE LOW-VALUES                 TO  EL141AI.
00895      MOVE ER-0168                    TO  EMI-ERROR.
00896      MOVE -1                         TO  MADDRTPL.
00897      MOVE AL-UABON                   TO  MADDRTPA.
00898      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.
00899      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00900      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
00901      GO TO 8100-SEND-INITIAL-MAP.
00902
00903      EJECT
00904  1250-READ-PRODUCER.
00905 ******************************************************************
00906 *    READ THE CONVENIENCE PRODUCER MASTER RECORD IF THE ADDRESS  *
00907 *    TYPE SELECTED IS A0 (ZERO).                                 *
00908 ******************************************************************
00909
00910      MOVE PI-COMPANY-CD              TO  EMPROD-COMPANY-CD.
00911      MOVE PI-CARRIER                 TO  EMPROD-CARRIER.
00912      MOVE CL-CERT-GROUPING           TO  EMPROD-GROUPING.
00913      MOVE CL-CERT-STATE              TO  EMPROD-STATE.
00914      MOVE CL-CERT-ACCOUNT            TO  EMPROD-PRODUCER.
00915      MOVE CL-CERT-EFF-DT             TO  EMPROD-EXP-DT.
00916      MOVE SPACES                     TO  WS-HOLD-ERACCT-RECORD.
00917
00918      
      * EXEC CICS  HANDLE CONDITION
00919 *        NOTFND  (1280-NO-PRODUCER)
00920 *        NOTOPEN (1290-PRODUCER-NOTOPEN)
00921 *    END-EXEC.
      *    MOVE '"$IJ                  ! '' #00005859' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035383539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00922
00923      
      * EXEC CICS  STARTBR
00924 *        RIDFLD   (EMPROD-KEY)
00925 *        DATASET  (EMPROD-FILE-ID)
00926 *        GTEQ
00927 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005864' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPROD-FILE-ID, 
                 EMPROD-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00928
00929      MOVE EMPROD-PARTIAL-KEY         TO  ERACCT-KEY-SAVE.
00930
00931  1260-READNEXT-PRODUCER.
00932
00933      
      * EXEC CICS  HANDLE CONDITION
00934 *        ENDFILE (1280-NO-PRODUCER)
00935 *        NOTFND  (1260-READNEXT-PRODUCER)
00936 *    END-EXEC.
      *    MOVE '"$''I                  ! ( #00005874' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035383734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00937
00938      
      * EXEC CICS  READNEXT
00939 *        SET      (ADDRESS OF PRODUCER-MASTER)
00940 *        DATASET  (EMPROD-FILE-ID)
00941 *        RIDFLD   (EMPROD-KEY)
00942 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005879' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPROD-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPROD-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00943
00944      MOVE 'P'                        TO  WS-ADDR-SW.
00945
00946      IF EMPROD-PARTIAL-KEY NOT = ERACCT-KEY-SAVE
00947          IF WS-HOLD-ERACCT-RECORD IS EQUAL TO SPACES
00948              GO TO 1230-NO-ACCT
00949          ELSE
00950              MOVE WS-HOLD-ERACCT-RECORD  TO  PRODUCER-MASTER
00951              GO TO 1500-CONTINUE-SHOW.
00952
00953      IF EMPROD-EXP-DT IS EQUAL TO HIGH-VALUES
00954          GO TO 1500-CONTINUE-SHOW
00955      ELSE
00956          MOVE PRODUCER-MASTER        TO  WS-HOLD-ERACCT-RECORD
00957          GO TO 1260-READNEXT-PRODUCER.
00958
00959  1280-NO-PRODUCER.
00960
00961      MOVE LOW-VALUES                 TO  EL141AI.
00962      MOVE ER-0278                    TO  EMI-ERROR.
00963      MOVE -1                         TO  MADDRTPL.
00964      MOVE AL-UABON                   TO  MADDRTPA.
00965      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.
00966      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00967      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
00968      GO TO 8100-SEND-INITIAL-MAP.
00969
00970  1290-PRODUCER-NOTOPEN.
00971
00972      MOVE LOW-VALUES                 TO  EL141AI.
00973      MOVE ER-9616                    TO  EMI-ERROR.
00974      MOVE -1                         TO  MADDRTPL.
00975      MOVE AL-UABON                   TO  MADDRTPA.
00976      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.
00977      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00978      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
00979      GO TO 8100-SEND-INITIAL-MAP.
00980
00981  1300-CHECK-COMP.
00982 ******************************************************************
00983 *    READ THE COMPENSATION MASTER IF THE ADDRESS TYPE SELECTED   *
00984 *    IS A9 AND NO ACTIVITY TRAILER (SEQ # 29) EXISTS.            *
00985 ******************************************************************
00986
00987      IF WS-ADDR-TYPE EQUAL 'A'
00988        AND
00989         WS-ADDR-SEQ EQUAL '0'
00990         GO TO 1500-CONTINUE-SHOW.
00991
00992      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00993          IF WS-HOLD-ERACCT-RECORD IS EQUAL TO SPACES
00994              GO TO 1230-NO-ACCT
00995          ELSE
00996              MOVE WS-HOLD-ERACCT-RECORD  TO  ACCOUNT-MASTER
00997              GO TO 1500-CONTINUE-SHOW.
00998
00999  1310-READ-COMP.
01000
01001      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
01002          MOVE ZEROS                  TO  AM-3RD-PARTY-NOTIF-LEVEL.
01003
01004      IF AM-3RD-PARTY-NOTIF-LEVEL EQUAL ZEROS
01005          MOVE LOW-VALUES             TO  EL141AO
01006          MOVE ER-0135                TO  EMI-ERROR
01007          MOVE -1                     TO  MADDRTPL
01008          MOVE AL-UABON               TO  MADDRTPA
01009          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01010          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT
01011          GO TO 8100-SEND-INITIAL-MAP.
01012
01013      IF AM-3RD-PARTY-NOTIF-LEVEL EQUAL ZEROS
01014         MOVE 01                      TO  AM-3RD-PARTY-NOTIF-LEVEL.
01015
01016      IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) EQUAL SPACES OR ZEROS
01017          MOVE ER-0135                TO  EMI-ERROR
01018          MOVE -1                     TO  MADDRTPL
01019          MOVE AL-UABON               TO  MADDRTPA
01020          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01021          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT
01022          GO TO 8100-SEND-INITIAL-MAP.
01023
01024      MOVE PI-COMPANY-CD              TO  ERCOMP-COMPANY-CD.
01025      MOVE AM-CARRIER                 TO  ERCOMP-CARRIER.
01026      MOVE AM-GROUPING                TO  ERCOMP-GROUPING.
01027      MOVE 'A'                        TO  ERCOMP-TYPE.
01028      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
01029                                      TO  ERCOMP-RESP-NO.
01030
01031      IF AM-3RD-PARTY-NOTIF-LEVEL IS EQUAL TO AM-REMIT-TO
01032          IF AM-COM-TYP (AM-REMIT-TO) EQUAL TO 'O' OR 'P' OR
01033                                               'G' OR 'B' or 'S'
01034              MOVE 'G'                TO  ERCOMP-TYPE
01035              MOVE LOW-VALUES         TO  ERCOMP-ACCOUNT
01036          ELSE
01037              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
01038                                      TO  ERCOMP-ACCOUNT
01039      ELSE
01040          MOVE 'G'                    TO  ERCOMP-TYPE
01041          MOVE LOW-VALUES             TO  ERCOMP-ACCOUNT.
01042
01043      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
01044         MOVE ZEROS                   TO  ERCOMP-CARRIER.
01045
01046      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
01047         MOVE ZEROS                   TO  ERCOMP-GROUPING.
01048
01049      
      * EXEC CICS HANDLE CONDITION
01050 *         NOTFND    (1320-ERCOMP-NOTFND)
01051 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00005990' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035393930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01052
01053      
      * EXEC CICS  READ
01054 *         SET      (ADDRESS OF COMPENSATION-MASTER)
01055 *         DATASET  (ERCOMP-FILE-ID)
01056 *         RIDFLD   (ERCOMP-KEY)
01057 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005994' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01058
01059      MOVE 'C'                        TO  WS-ADDR-SW.
01060      GO TO 1500-CONTINUE-SHOW.
01061
01062  1320-ERCOMP-NOTFND.
01063
01064      MOVE LOW-VALUES                 TO  EL141AI.
01065      MOVE ER-0278                    TO  EMI-ERROR.
01066      MOVE -1                         TO  MADDRTPL.
01067      MOVE AL-UABON                   TO  MADDRTPA.
01068      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.
01069      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01070      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
01071      GO TO 8100-SEND-INITIAL-MAP.
01072
01073      EJECT
01074  1500-CONTINUE-SHOW.
01075
01076      MOVE SPACES                     TO  WS-ZIP-CODE.
01077
01078      IF WS-ADDR-SW EQUAL 'C'
01079         MOVE CO-ACCT-NAME            TO  MAPNAMEO
01080         IF CO-ACCT-NAME EQUAL SPACES
01081            MOVE CO-MAIL-NAME         TO  MAPNAMEO.
01082
01083      IF WS-ADDR-SW EQUAL 'C'
01084         MOVE CO-ADDR-1               TO  MADDRL1O
01085         MOVE CO-ADDR-2               TO  MADDRL2O
              MOVE CO-ADDR-CITY            TO  MCITYO
              MOVE CO-ADDR-STATE           TO  MSTATEO
01087         MOVE +0                      TO  WS-SEQ-CONVERT
01088         MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O
01089         MOVE CO-AREA-CODE            TO  WS-PH-ED-1
01090         MOVE CO-PREFIX               TO  WS-PH-ED-2
01091         MOVE CO-PHONE                TO  WS-PH-ED-3
01092         MOVE WS-PHONE-EDIT           TO  MAPHONEO
01093         MOVE SPACES                  TO  MAPSEQ2O
01094         MOVE '*'                     TO  MAPNOT1O
01095         MOVE 'ABOVE ADDRESS FROM COMPENSATION MASTER'
01096                                      TO  MAPNOT2O.
01097
01098      IF WS-ADDR-SW EQUAL 'C'
01099         IF CO-CANADIAN-POST-CODE
01100             MOVE CO-CAN-POSTAL-1     TO  WS-ZIP-CAN-2-POST1
01101             MOVE CO-CAN-POSTAL-2     TO  WS-ZIP-CAN-2-POST2
01102         ELSE
01103             MOVE CO-ZIP-PRIME        TO  WS-ZIP-AM-2-CODE
01104             IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
01105                 MOVE '-'             TO  WS-ZIP-AM-2-DASH
01106                 MOVE CO-ZIP-PLUS4    TO  WS-ZIP-AM-2-PLUS4.
01107
01108      IF WS-ADDR-SW EQUAL 'A'
01109         MOVE AM-NAME                 TO  MAPNAMEO
01110         MOVE AM-PERSON               TO  MADDRL1O
01111         MOVE AM-ADDRS                TO  MADDRL2O
              MOVE AM-ADDR-CITY            TO  MCITYO
              MOVE AM-ADDR-STATE           TO  MSTATEO
01113         MOVE +0                      TO  WS-SEQ-CONVERT
01114         MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O
01115         MOVE AM-AREA-CODE            TO  WS-PH-ED-1
01116         MOVE AM-TEL-PRE              TO  WS-PH-ED-2
01117         MOVE AM-TEL-NBR              TO  WS-PH-ED-3
01118         MOVE WS-PHONE-EDIT           TO  MAPHONEO
01119         MOVE SPACES                  TO  MAPSEQ2O
01120         MOVE '*'                     TO  MAPNOT1O
01121         MOVE 'ABOVE ADDRESS FROM ACCOUNT MASTER'
01122                                      TO  MAPNOT2O.
01123
01124      IF WS-ADDR-SW EQUAL 'A'
01125         IF AM-CANADIAN-POST-CODE
01126             MOVE AM-CAN-POSTAL-1     TO  WS-ZIP-CAN-2-POST1
01127             MOVE AM-CAN-POSTAL-2     TO  WS-ZIP-CAN-2-POST2
01128         ELSE
01129             MOVE AM-ZIP-PRIME        TO  WS-ZIP-AM-2-CODE
01130             IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
01131                 MOVE '-'             TO  WS-ZIP-AM-2-DASH
01132                 MOVE AM-ZIP-PLUS4    TO  WS-ZIP-AM-2-PLUS4.
01133
01134      IF WS-ADDR-SW EQUAL 'P'
01135         MOVE PD-NAME                 TO  MAPNAMEO
01136         MOVE PD-PERSON               TO  MADDRL1O
01137         MOVE PD-ADDRS                TO  MADDRL2O
01138         MOVE PD-CITY                 TO  MCITYO
01139         MOVE +0                      TO  WS-SEQ-CONVERT
01140         MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O
01141         MOVE PD-AREA-CODE            TO  WS-PH-ED-1
01142         MOVE PD-TEL-PRE              TO  WS-PH-ED-2
01143         MOVE PD-TEL-NBR              TO  WS-PH-ED-3
01144         MOVE WS-PHONE-EDIT           TO  MAPHONEO
01145         MOVE SPACES                  TO  MAPSEQ2O
01146         MOVE '*'                     TO  MAPNOT1O
01147         MOVE 'ABOVE ADDRESS FROM PRODUCER MASTER'
01148                                      TO  MAPNOT2O.
01149
01150      IF WS-ADDR-SW EQUAL 'P'
01151          MOVE PD-ZIP-PRIME           TO  WS-ZIP-AM-2-CODE
01152          IF (PD-ZIP-PLUS4 NOT = SPACES  AND  ZEROS)
01153              MOVE '-'                TO  WS-ZIP-AM-2-DASH
01154              MOVE PD-ZIP-PLUS4       TO  WS-ZIP-AM-2-PLUS4.
01155
01156      IF WS-ADDR-SW EQUAL 'T'
01157         MOVE AT-MAIL-TO-NAME         TO  MAPNAMEO
01158         MOVE AT-ADDRESS-LINE-1       TO  MADDRL1O
01159         MOVE AT-ADDRESS-LINE-2       TO  MADDRL2O
              MOVE AT-CITY                 TO  MCITYO
              MOVE AT-STATE                TO  MSTATEO
01161 *       MOVE AT-ADDRESS-TYPE         TO  MADDRTPO
01162         MOVE ELTRLR-SEQUENCE-NO      TO  WS-SEQ-CONVERT
01163         MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O
01164         MOVE AT-PHONE-NO             TO  WS-PHONE-BRKDN
01165         MOVE WS-PH-1                 TO  WS-PH-ED-1
01166         MOVE WS-PH-2                 TO  WS-PH-ED-2
01167         MOVE WS-PH-3                 TO  WS-PH-ED-3
061511        MOVE WS-PHONE-EDIT           TO  MAPHONEO
061511        MOVE AT-VFY-2ND-BENE-SSN     TO  BENESSNO
061511        MOVE AT-VFY-2ND-BENE-VERIFIED TO BENEVFYO.
01169
01170      IF WS-ADDR-SW EQUAL 'T'
01171         IF AT-CANADIAN-POST-CODE
01172             MOVE AT-CAN-POSTAL-1     TO  WS-ZIP-CAN-2-POST1
01173             MOVE AT-CAN-POSTAL-2     TO  WS-ZIP-CAN-2-POST2
01174         ELSE
01175             MOVE AT-ZIP-CODE         TO  WS-ZIP-AM-2-CODE
01176             IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
01177                 MOVE '-'             TO  WS-ZIP-AM-2-DASH
01178                 MOVE AT-ZIP-PLUS4    TO  WS-ZIP-AM-2-PLUS4.
01179
01180      IF WS-ADDR-SW EQUAL 'B'
01181          IF WS-ADDR-SEQ NOT EQUAL '9'
01182              MOVE BE-MAIL-TO-NAME        TO  MAPNAMEO
01183              MOVE BE-ADDRESS-LINE-1      TO  MADDRL1O
01184              MOVE BE-ADDRESS-LINE-2      TO  MADDRL2O
                   MOVE BE-CITY                TO  MCITYO
                   MOVE BE-STATE               TO  MSTATEO
01186              MOVE +0                     TO  WS-SEQ-CONVERT
01187              MOVE WS-SEQ-CONVERT         TO  MAPSEQ2O
01188              MOVE BE-PHONE-NO            TO  WS-PHONE-BRKDN
01189              MOVE WS-PH-1                TO  WS-PH-ED-1
01190              MOVE WS-PH-2                TO  WS-PH-ED-2
01191              MOVE WS-PH-3                TO  WS-PH-ED-3
01192              MOVE WS-PHONE-EDIT          TO  MAPHONEO
01193              MOVE SPACES                 TO  MAPSEQ2O
01194              MOVE '*'                    TO  MAPNOT1O
01195              MOVE 'ABOVE ADDRESS FROM BENEFICIARY MASTER'
01196                                          TO  MAPNOT2O
01197              IF BE-CANADIAN-POST-CODE
01198                 MOVE BE-CAN-POSTAL-1     TO  WS-ZIP-CAN-2-POST1
01199                 MOVE BE-CAN-POSTAL-2     TO  WS-ZIP-CAN-2-POST2
01200              ELSE
01201                 MOVE BE-ZIP-PRIME        TO  WS-ZIP-AM-2-CODE
01202                 IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
01203                     MOVE '-'             TO  WS-ZIP-AM-2-DASH
01204                     MOVE BE-ZIP-PLUS4    TO  WS-ZIP-AM-2-PLUS4
01205                 ELSE
01206                     NEXT SENTENCE
01207          ELSE
01208             MOVE BE-MAIL-TO-NAME2        TO  MAPNAMEO
01209             MOVE BE-ADDRESS-LINE-12      TO  MADDRL1O
01210             MOVE BE-ADDRESS-LINE-22      TO  MADDRL2O
                  MOVE BE-CITY2                TO  MCITYO
                  MOVE BE-STATE2               TO  MSTATEO
01212             MOVE +0                      TO  WS-SEQ-CONVERT
01213             MOVE WS-SEQ-CONVERT          TO  MAPSEQ2O
01214             MOVE BE-PHONE-NO2            TO  WS-PHONE-BRKDN
01215             MOVE WS-PH-1                 TO  WS-PH-ED-1
01216             MOVE WS-PH-2                 TO  WS-PH-ED-2
01217             MOVE WS-PH-3                 TO  WS-PH-ED-3
01218             MOVE WS-PHONE-EDIT           TO  MAPHONEO
01219             MOVE SPACES                  TO  MAPSEQ2O
01220             MOVE '*'                     TO  MAPNOT1O
01221             MOVE 'ABOVE ADDRESS FROM BENEFICIARY ADDR2 '
01222                                      TO  MAPNOT2O
01223             IF BE-CANADIAN-POST-CODE2
01224                 MOVE BE-CAN-POSTAL-12    TO  WS-ZIP-CAN-2-POST1
01225                 MOVE BE-CAN-POSTAL-22    TO  WS-ZIP-CAN-2-POST2
01226             ELSE
01227                 MOVE BE-ZIP-PRIME2       TO  WS-ZIP-AM-2-CODE
01228                 IF BE-ZIP-PLUS42 NOT = SPACES  AND  ZEROS
01229                     MOVE '-'             TO  WS-ZIP-AM-2-DASH
01230                     MOVE BE-ZIP-PLUS42   TO  WS-ZIP-AM-2-PLUS4.
01231
01232      MOVE WS-ZIP-CODE                TO  MZIPCODO.
01233
01234      IF WS-ADDR-TYPE EQUAL 'B' AND
01235         WS-ADDR-SEQ EQUAL '9' OR '0'
01236            MOVE AL-PABON                TO MAPNAMEA
01237                                            MADDRL1A
01238                                            MADDRL2A
01239                                            MCITYA
                                                 MSTATEA
01240                                            MAPHONEA
01241                                            MZIPCODA.
01242
01243      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')
01244          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')
01245              IF PI-MAINT IS EQUAL TO 'A'
01246                  MOVE ER-0265        TO  EMI-ERROR
01247                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01248                  MOVE PI-CLAIM-NO    TO  EMI-TEXT-VARIABLE (1).
01249
01250      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')
01251          IF (PI-RETURN-TO-PROGRAM EQUAL 'EL130   ' OR 'EM130   ')
01252              IF PI-MAINT IS EQUAL TO 'A'
01253                  MOVE 'S'            TO  PI-MAINT
01254                  IF PI-LETTER-ERROR-CODE IS NOT EQUAL TO ZEROS
01255                      MOVE PI-LETTER-ERROR-CODE   TO  EMI-ERROR
01256                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01257
01258      IF ADD-REQUIRED OR CHANGE-REQUIRED
01259          MOVE ER-0000                TO  EMI-ERROR
01260          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01261
01262      MOVE -1                         TO  MFMAINTL.
01263
01264      MOVE AL-UANON                   TO  MADDRTPA   MFMAINTA
01265                                          MAPNAMEA   MADDRL1A
01266                                          MADDRL2A   MCITYA
                                               MSTATEA
01267                                          MZIPCODA   MAPHONEA.
01268
01269      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
01270
01271      MOVE 'S'                        TO  PI-PREV-MAINT-CODE.
01272      MOVE MADDRTPI                   TO  PI-PREV-ADDR-TYPE.
01273
01274      GO TO 8100-SEND-INITIAL-MAP.
01275      EJECT
01276  2000-CHANGE-ADDRESS.
01277
01278      IF PI-COMPANY-ID EQUAL 'DMD' AND
01279         WS-ADDR-SEQ   EQUAL '9'   AND
01280         WS-ADDR-TYPE  EQUAL 'B'
01281         MOVE ER-8131                TO  EMI-ERROR
01282         MOVE -1                     TO  MFMAINTL
01283         MOVE AL-UABON               TO  MFMAINTA
01284                                         MADDRTPA
01285         PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
01286         GO TO 8200-SEND-DATAONLY.
01287
01288      IF PI-PREV-MAINT-CODE IS EQUAL TO 'S' AND
01289         PI-PREV-ADDR-TYPE IS EQUAL TO MADDRTPI
01290          NEXT SENTENCE
01291      ELSE
01292          MOVE ER-0145                TO  EMI-ERROR
01293          MOVE -1                     TO  MFMAINTL
01294          MOVE AL-UABON               TO  MFMAINTA
01295                                          MADDRTPA
01296          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01297          GO TO 8100-SEND-INITIAL-MAP.
01298
01299      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.
01300      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.
01301      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.
01302      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.
01303
01304      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.
01305
01306      PERFORM 6000-EDIT-SCREEN THRU 6000-EXIT.
01307
01308      IF NOT EMI-NO-ERRORS
01309          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT
01310          GO TO 8200-SEND-DATAONLY.
01311
01312      IF WS-ADDR-TYPE EQUAL 'I'
01313         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01314      ELSE
01315      IF WS-ADDR-TYPE EQUAL 'B'
01316         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01317         ADD +10                      TO  ELTRLR-SEQUENCE-NO
01318      ELSE
01319      IF WS-ADDR-TYPE EQUAL 'A'
01320         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01321         ADD +20                      TO  ELTRLR-SEQUENCE-NO
01322      ELSE
01323      IF WS-ADDR-TYPE EQUAL 'P'
01324         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01325         ADD +30                      TO  ELTRLR-SEQUENCE-NO
01326      ELSE
01327      IF WS-ADDR-TYPE EQUAL 'E'
01328         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01329         ADD +40                      TO  ELTRLR-SEQUENCE-NO
01330      ELSE
01331      IF WS-ADDR-TYPE EQUAL 'O'
01332         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01333         ADD +50                      TO  ELTRLR-SEQUENCE-NO
01334      ELSE
01335      IF WS-ADDR-TYPE EQUAL 'Q'
01336         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01337         ADD +60                      TO  ELTRLR-SEQUENCE-NO.
01338
01339      
      * EXEC CICS HANDLE CONDITION
01340 *        NOTFND   (9050-NOTFND-ELTRLR)
01341 *        NOTOPEN  (9060-NOTOPEN-ELTRLR)
01342 *    END-EXEC.
      *    MOVE '"$IJ                  ! * #00006289' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303036323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01343
01344      PERFORM 7055-READ-ELTRLR-UPDATE THRU 7055-EXIT.
01345
01346      GO TO 2050-CONTINUE-CHANGE.
01347
01348  2050-CONTINUE-CHANGE.
01349
01350      IF  MAPNAMEI  NOT = LOW-VALUES
01351          MOVE MAPNAMEI               TO  AT-MAIL-TO-NAME.
01352      IF  MADDRL1I  NOT = LOW-VALUES
01353          MOVE MADDRL1I               TO  AT-ADDRESS-LINE-1.
01354      IF  MADDRL2I  NOT = LOW-VALUES
01355          MOVE MADDRL2I               TO  AT-ADDRESS-LINE-2.
           IF  MCITYI NOT = LOW-VALUES
               MOVE MCITYI                 TO  AT-CITY.
           IF  MSTATEI NOT = LOW-VALUES
               MOVE MSTATEI                TO  AT-STATE.
01358      IF  WS-PHONE-NUM  NOT = LOW-VALUES
01359          MOVE WS-PHONE-NUM           TO  AT-PHONE-NO.
01360      IF  MADDRTPI   NOT = LOW-VALUES
01361          MOVE MADDRTPI               TO  AT-ADDRESS-TYPE.
061511     IF  BENESSNI NOT = LOW-VALUES
061511         MOVE BENESSNI               TO  AT-VFY-2ND-BENE-SSN
061511     END-IF.
061511     IF  BENEVFYI NOT = LOW-VALUES
061511         MOVE BENEVFYI               TO  AT-VFY-2ND-BENE-VERIFIED
061511     END-IF.
01362
01363      IF  MZIPCODL  =  ZEROS
01364          GO TO 2060-CONTINUE-CHANGE.
01365
01366      MOVE MZIPCODI                   TO  WS-ZIP-CODE.
01367      MOVE SPACES                     TO  AT-ZIP.
01368
01369      IF WS-CANADIAN-ZIP
01370          IF WS-ZIP-4 = SPACE  OR  '-'
01371              MOVE WS-ZIP-CAN-2-POST1 TO  AT-CAN-POSTAL-1
01372              MOVE WS-ZIP-CAN-2-POST2 TO  AT-CAN-POSTAL-2
01373          ELSE
01374              MOVE WS-ZIP-CAN-1-POST1 TO  AT-CAN-POSTAL-1
01375              MOVE WS-ZIP-CAN-1-POST2 TO  AT-CAN-POSTAL-2
01376      ELSE
01377          IF WS-ZIP-6 = SPACE  OR  '-'
01378              MOVE WS-ZIP-AM-2-CODE   TO  AT-ZIP-CODE
01379              MOVE WS-ZIP-AM-2-PLUS4  TO  AT-ZIP-PLUS4
01380          ELSE
01381              MOVE WS-ZIP-AM-1-CODE   TO  AT-ZIP-CODE
01382              MOVE WS-ZIP-AM-1-PLUS4  TO  AT-ZIP-PLUS4.
01383
01384  2060-CONTINUE-CHANGE.
01385
01386      MOVE PI-PROCESSOR-ID        TO  AT-ADDRESS-LAST-UPDATED-BY.
01387      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.
01388      MOVE WS-CURR-DATE-BIN       TO  AT-ADDRESS-LAST-MAINT-DT.
01389
01390      PERFORM 7060-REWRITE-ELTRLR THRU 7060-EXIT.
01391
01392      GO TO 1000-SHOW-ADDRESS.
01393
01394      EJECT
01395  3000-ADD-ADDRESS.
01396
01397      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.
01398      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.
01399      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.
01400      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.
01401
01402      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.
01403
01404      PERFORM 6000-EDIT-SCREEN THRU 6000-EXIT.
01405
01406      IF NOT EMI-NO-ERRORS
01407          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT
01408          GO TO 8200-SEND-DATAONLY.
01409
01410      IF WS-ADDR-TYPE EQUAL 'I'
01411         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01412      ELSE
01413      IF WS-ADDR-TYPE EQUAL 'B'
01414         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01415         ADD +10                      TO  ELTRLR-SEQUENCE-NO
01416      ELSE
01417      IF WS-ADDR-TYPE EQUAL 'A'
01418         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01419         ADD +20                      TO  ELTRLR-SEQUENCE-NO
01420      ELSE
01421      IF WS-ADDR-TYPE EQUAL 'P'
01422         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01423         ADD +30                      TO  ELTRLR-SEQUENCE-NO
01424      ELSE
01425      IF WS-ADDR-TYPE EQUAL 'E'
01426         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01427         ADD +40                      TO  ELTRLR-SEQUENCE-NO
01428      ELSE
01429      IF WS-ADDR-TYPE EQUAL 'O'
01430         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01431         ADD +50                      TO  ELTRLR-SEQUENCE-NO
01432      ELSE
01433      IF WS-ADDR-TYPE EQUAL 'Q'
01434         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01435         ADD +60                      TO  ELTRLR-SEQUENCE-NO.
01436
01437      PERFORM 7005-READ-ELMSTR-UPDATE THRU 7005-EXIT.
01438
01439  3010-CONTINUE-ADD.
01440
01441      PERFORM 7080-GETMAIN-ELTRLR THRU 7080-EXIT.
01442
01443      MOVE ELTRLR-KEY                 TO  AT-CONTROL-PRIMARY.
01444      MOVE 'AT'                       TO  AT-RECORD-ID.
01445      MOVE '5'                        TO  AT-TRAILER-TYPE.
01446      MOVE MAPNAMEI                   TO  AT-MAIL-TO-NAME.
01447
01448      IF MADDRL1L GREATER THAN ZEROS
01449         MOVE MADDRL1I                TO  AT-ADDRESS-LINE-1
01450      ELSE
01451         MOVE SPACES                  TO  AT-ADDRESS-LINE-1.
01452
01453      IF MADDRL2L GREATER THAN ZEROS
01454          MOVE MADDRL2I               TO  AT-ADDRESS-LINE-2
01455      ELSE
01456          MOVE SPACES                 TO  AT-ADDRESS-LINE-2.
01457
01458      IF MCITYL > 0
01459         MOVE MCITYI                  TO  AT-CITY
01460      ELSE
01461         MOVE SPACES                  TO  AT-CITY.
01458      IF MSTATEL > 0
01459         MOVE MSTATEI                 TO  AT-STATE
01460      ELSE
01461         MOVE SPACES                  TO  AT-STATE.
01462
01463      MOVE WS-PHONE-NUM               TO  AT-PHONE-NO.
01464      MOVE MADDRTPI                   TO  AT-ADDRESS-TYPE.
01465      MOVE PI-PROCESSOR-ID            TO  AT-RECORDED-BY
01466                                        AT-ADDRESS-LAST-UPDATED-BY.
061511     IF  BENESSNL GREATER THAN ZEROS
061511         MOVE BENESSNI               TO  AT-VFY-2ND-BENE-SSN
061511     END-IF.
032514     IF  BENEVFYL GREATER THAN ZEROS
061511         MOVE BENEVFYI               TO  AT-VFY-2ND-BENE-VERIFIED
061511     END-IF.
01467
01468      IF MZIPCODL  =  ZEROS
01469          MOVE SPACES                 TO  AT-ZIP
01470          GO TO 3020-CONTINUE-ADD.
01471
01472      MOVE MZIPCODI                   TO  WS-ZIP-CODE.
01473      MOVE SPACES                     TO  AT-ZIP.
01474
01475      IF WS-CANADIAN-ZIP
01476          IF WS-ZIP-4 = SPACE  OR  '-'
01477              MOVE WS-ZIP-CAN-2-POST1 TO  AT-CAN-POSTAL-1
01478              MOVE WS-ZIP-CAN-2-POST2 TO  AT-CAN-POSTAL-2
01479          ELSE
01480              MOVE WS-ZIP-CAN-1-POST1 TO  AT-CAN-POSTAL-1
01481              MOVE WS-ZIP-CAN-1-POST2 TO  AT-CAN-POSTAL-2
01482      ELSE
01483          IF WS-ZIP-6 = SPACE  OR  '-'
01484              MOVE WS-ZIP-AM-2-CODE   TO  AT-ZIP-CODE
01485              MOVE WS-ZIP-AM-2-PLUS4  TO  AT-ZIP-PLUS4
01486          ELSE
01487              MOVE WS-ZIP-AM-1-CODE   TO  AT-ZIP-CODE
01488              MOVE WS-ZIP-AM-1-PLUS4  TO  AT-ZIP-PLUS4.
01489
01490  3020-CONTINUE-ADD.
01491
01492      MOVE EIBTIME                    TO  AT-LAST-MAINT-HHMMSS.
01493      MOVE WS-CURR-DATE-BIN           TO  AT-RECORDED-DT
01494                                          AT-ADDRESS-LAST-MAINT-DT.
01495
01496      
      * EXEC CICS  HANDLE CONDITION
01497 *         DUPREC   (3030-TRLR-DUPREC)
01498 *    END-EXEC.
      *    MOVE '"$%                   ! + #00006464' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303036343634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01499
01500      PERFORM 7065-WRITE-ELTRLR THRU 7065-EXIT.
01501
01502      GO TO 3040-CONTINUE-ADD.
01503
01504  3030-TRLR-DUPREC.
01505
01506      PERFORM 7015-UNLOCK-ELMSTR THRU 7015-EXIT.
01507
01508      GO TO 1000-SHOW-ADDRESS.
01509
01510  3040-CONTINUE-ADD.
01511
01512      MOVE PI-PROCESSOR-ID            TO  CL-LAST-MAINT-USER.
01513      MOVE '3'                        TO  CL-LAST-MAINT-TYPE.
01514      MOVE EIBTIME                    TO  CL-LAST-MAINT-HHMMSS.
01515      MOVE WS-CURR-DATE-BIN           TO  CL-LAST-MAINT-DT.
01516
01517      IF WS-ADDR-TYPE EQUAL 'I'
01518         ADD +1                       TO  CL-INSURED-ADDR-CNT
01519      ELSE
01520      IF WS-ADDR-TYPE EQUAL 'B'  AND
01521         WS-ADDR-SEQ NOT EQUAL '9'
01522         ADD +1                       TO  CL-BENIF-ADDR-CNT
01523      ELSE
01524      IF WS-ADDR-TYPE EQUAL 'A' AND
01525         WS-ADDR-SEQ NOT EQUAL '9'
01526         ADD +1                       TO  CL-ACCOUNT-ADDR-CNT
01527      ELSE
01528      IF WS-ADDR-TYPE EQUAL 'P'
01529         ADD +1                       TO  CL-DOCTOR-ADDR-CNT
01530      ELSE
01531      IF WS-ADDR-TYPE EQUAL 'E'
01532         ADD +1                       TO  CL-EMPLOYER-ADDR-CNT
01533      ELSE
01534      IF WS-ADDR-TYPE EQUAL 'O'
01535         ADD +1                       TO  CL-OTHER-1-ADDR-CNT
01536      ELSE
01537      IF WS-ADDR-TYPE EQUAL 'Q'
01538         ADD +1                       TO  CL-OTHER-2-ADDR-CNT.
01539
01540      PERFORM 7010-REWRITE-ELMSTR THRU 7010-EXIT.
01541
01542      GO TO 1000-SHOW-ADDRESS.
01543
01544  4000-DELETE-ADDRESS.
01545
01546      IF PI-PREV-MAINT-CODE IS EQUAL TO 'S' AND
01547         PI-PREV-ADDR-TYPE IS EQUAL TO MADDRTPI
01548          NEXT SENTENCE
01549      ELSE
01550          MOVE ER-0145                TO  EMI-ERROR
01551          MOVE -1                     TO  MFMAINTL
01552          MOVE AL-UABON               TO  MFMAINTA
01553                                          MADDRTPA
01554          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01555          GO TO 8100-SEND-INITIAL-MAP.
01556
01557      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.
01558      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.
01559      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.
01560      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.
01561
01562      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.
01563
01564      IF ((WS-ADDR-TYPE EQUAL 'I') AND
01565         (CL-INSURED-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))
01566       OR
01567         ((WS-ADDR-TYPE EQUAL 'A') AND
01568         (CL-ACCOUNT-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM) AND
01569         (WS-ADDR-SEQ NOT EQUAL '9'))
01570       OR
01571         ((WS-ADDR-TYPE EQUAL 'B') AND
01572         (CL-BENIF-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM)  AND
01573         (WS-ADDR-SEQ NOT EQUAL '9'))
01574       OR
01575         ((WS-ADDR-TYPE EQUAL 'E') AND
01576         (CL-EMPLOYER-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))
01577       OR
01578         ((WS-ADDR-TYPE EQUAL 'P') AND
01579         (CL-DOCTOR-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))
01580       OR
01581         ((WS-ADDR-TYPE EQUAL 'O') AND
01582         (CL-OTHER-1-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))
01583       OR
01584         ((WS-ADDR-TYPE EQUAL 'Q') AND
01585         (CL-OTHER-2-ADDR-CNT NOT EQUAL WS-ADDR-SEQ-NUM))
01586          MOVE ER-1879                TO  EMI-ERROR
01587          MOVE -1                     TO  MADDRTPL
01588          MOVE AL-UABON               TO  MADDRTPA
01589          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01590          PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT
01591          GO TO 8200-SEND-DATAONLY.
01592
01593      IF WS-ADDR-TYPE EQUAL 'I'
01594         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01595      ELSE
01596      IF WS-ADDR-TYPE EQUAL 'B'
01597         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01598         ADD +10                      TO  ELTRLR-SEQUENCE-NO
01599      ELSE
01600      IF WS-ADDR-TYPE EQUAL 'A'
01601         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01602         ADD +20                      TO  ELTRLR-SEQUENCE-NO
01603      ELSE
01604      IF WS-ADDR-TYPE EQUAL 'P'
01605         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01606         ADD +30                      TO  ELTRLR-SEQUENCE-NO
01607      ELSE
01608      IF WS-ADDR-TYPE EQUAL 'E'
01609         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01610         ADD +40                      TO  ELTRLR-SEQUENCE-NO
01611      ELSE
01612      IF WS-ADDR-TYPE EQUAL 'O'
01613         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01614         ADD +50                      TO  ELTRLR-SEQUENCE-NO
01615      ELSE
01616      IF WS-ADDR-TYPE EQUAL 'Q'
01617         MOVE WS-ADDR-SEQ-NUM         TO  ELTRLR-SEQUENCE-NO
01618         ADD +60                      TO  ELTRLR-SEQUENCE-NO.
01619
01620      PERFORM 7005-READ-ELMSTR-UPDATE THRU 7005-EXIT.
01621
01622      
      * EXEC CICS HANDLE CONDITION
01623 *        NOTFND   (4010-NOTFND-ELTRLR)
01624 *        NOTOPEN  (4020-NOTOPEN-ELTRLR)
01625 *    END-EXEC.
      *    MOVE '"$IJ                  ! , #00006590' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303036353930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01626
01627      PERFORM 7055-READ-ELTRLR-UPDATE THRU 7055-EXIT.
01628
01629      GO TO 4050-CONTINUE-DELETE.
01630
01631  4010-NOTFND-ELTRLR.
01632
01633      PERFORM 7075-UNLOCK-ELTRLR THRU 7075-EXIT.
01634
01635      MOVE LOW-VALUES                 TO  EL141AI.
01636      MOVE ER-0135                    TO  EMI-ERROR.
01637      MOVE -1                         TO  MADDRTPL.
01638      MOVE AL-UABON                   TO  MADDRTPA
01639                                          MFMAINTA.
01640      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.
01641      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.
01642      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01643      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
01644      GO TO 8100-SEND-INITIAL-MAP.
01645
01646  4020-NOTOPEN-ELTRLR.
01647
01648      MOVE LOW-VALUES                 TO  EL141AI.
01649      MOVE ER-0172                    TO  EMI-ERROR.
01650      MOVE -1                         TO  MFMAINTL.
01651      MOVE AL-UABON                   TO  MFMAINTA.
01652      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.
01653      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01654      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
01655      GO TO 8100-SEND-INITIAL-MAP.
01656
01657  4050-CONTINUE-DELETE.
01658
01659      PERFORM 7070-DELETE-ELTRLR THRU 7070-EXIT.
01660
01661      IF WS-ADDR-TYPE EQUAL 'I'
01662         SUBTRACT +1   FROM CL-INSURED-ADDR-CNT
01663      ELSE
01664      IF WS-ADDR-TYPE EQUAL 'B' AND
01665         WS-ADDR-SEQ NOT EQUAL '9'
01666         SUBTRACT +1   FROM CL-BENIF-ADDR-CNT
01667      ELSE
01668      IF WS-ADDR-TYPE EQUAL 'A' AND
01669         WS-ADDR-SEQ NOT EQUAL '9'
01670         SUBTRACT +1   FROM CL-ACCOUNT-ADDR-CNT
01671      ELSE
01672      IF WS-ADDR-TYPE EQUAL 'P'
01673         SUBTRACT +1    FROM CL-DOCTOR-ADDR-CNT
01674      ELSE
01675      IF WS-ADDR-TYPE EQUAL 'E'
01676         SUBTRACT +1    FROM CL-EMPLOYER-ADDR-CNT
01677      ELSE
01678      IF WS-ADDR-TYPE EQUAL 'O'
01679         SUBTRACT +1    FROM CL-OTHER-1-ADDR-CNT
01680      ELSE
01681      IF WS-ADDR-TYPE EQUAL 'Q'
01682         SUBTRACT +1    FROM CL-OTHER-2-ADDR-CNT.
01683
01684      MOVE PI-PROCESSOR-ID            TO  CL-LAST-MAINT-USER.
01685      MOVE '3'                        TO  CL-LAST-MAINT-TYPE.
01686      MOVE EIBTIME                    TO  CL-LAST-MAINT-HHMMSS.
01687      MOVE WS-CURR-DATE-BIN           TO  CL-LAST-MAINT-DT.
01688
01689      PERFORM 7010-REWRITE-ELMSTR THRU 7010-EXIT.
01690
01691      MOVE SPACES                     TO  MAPNAMEO
01692                                          MADDRL1O
01693                                          MADDRL2O
01694                                          MCITYO
                                               MSTATEO
01695                                          MAPHONEO
01696                                          MZIPCODO
01697                                          MAPNOT1O
01698                                          MAPNOT2O
01699                                          MAPSEQ2O.
01700
01701      MOVE ER-0000                    TO  EMI-ERROR.
01702      MOVE -1                         TO  MFMAINTL.
01703      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01704
01705      MOVE PI-COMPANY-CD              TO  ELTRLR-COMPANY-CD.
01706      MOVE PI-CARRIER                 TO  ELTRLR-CARRIER.
01707      MOVE PI-CLAIM-NO                TO  ELTRLR-CLAIM-NO.
01708      MOVE PI-CERT-NO                 TO  ELTRLR-CERT-NO.
01709
01710      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.
01711
01712      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
01713
01714      MOVE 'D'                    TO  PI-PREV-MAINT-CODE.
01715      GO TO 8100-SEND-INITIAL-MAP.
01716
01717      EJECT
01718  5000-UPDATE-VIA-PF2.
01719 *****
01720 * 5000 PARA FOR READING OF MASTER FOR CERT LEVEL TO BE CHANGED
01721 *****
01722      MOVE +1                     TO  NDX.
01723      PERFORM 5900-CLEAR-AT-TABLE  THRU  5900-EXIT.
01724
01725      MOVE PI-COMPANY-CD          TO  ELTRLR-COMPANY-CD.
01726      MOVE PI-CARRIER             TO  ELTRLR-CARRIER.
01727      MOVE PI-CLAIM-NO            TO  ELTRLR-CLAIM-NO.
01728      MOVE PI-CERT-NO             TO  ELTRLR-CERT-NO.
01729
01730      PERFORM 7000-READ-ELMSTR THRU 7000-EXIT.
01731
01732      MOVE CL-CONTROL-PRIMARY     TO  WS-SAVE-MSTR-KEY.
01733      MOVE +1                     TO  ELTRLR-SEQUENCE-NO.
01734      MOVE CL-ADDRESS-TRAILER-CNT TO  WS-ADDR-TRAILER-CNT.
01735
01736      IF WS-ADDR-TYPE EQUAL 'B'
01737         ADD +10                  TO  ELTRLR-SEQUENCE-NO
01738      ELSE
01739      IF WS-ADDR-TYPE EQUAL 'A'
01740         ADD +20                  TO  ELTRLR-SEQUENCE-NO
01741      ELSE
01742      IF WS-ADDR-TYPE EQUAL 'P'
01743         ADD +30                  TO  ELTRLR-SEQUENCE-NO
01744      ELSE
01745      IF WS-ADDR-TYPE EQUAL 'E'
01746         ADD +40                  TO  ELTRLR-SEQUENCE-NO
01747      ELSE
01748      IF WS-ADDR-TYPE EQUAL 'O'
01749         ADD +50                  TO  ELTRLR-SEQUENCE-NO
01750      ELSE
01751      IF WS-ADDR-TYPE EQUAL 'Q'
01752         ADD +60                  TO  ELTRLR-SEQUENCE-NO.
01753
01754      MOVE +1                     TO  NDX.
01755
01756      MOVE PI-COMPANY-CD          TO  ELTRLR-COMPANY-CD.
01757      MOVE PI-CARRIER             TO  ELTRLR-CARRIER.
01758      MOVE PI-CLAIM-NO            TO  ELTRLR-CLAIM-NO.
01759      MOVE PI-CERT-NO             TO  ELTRLR-CERT-NO.
01760
01761  5100-READ-TRLR-TO-STORE.
01762 *****
01763 * 5100 PARA TO READ TRAILER TO STORE TRAILERS OF CERT CHANGED
01764 *****
01765
01766      PERFORM 7090-START-ELTRLR THRU 7090-EXIT.
01767
01768  5100-READ-LOOP.
01769
01770      PERFORM 7095-READNEXT-ELTRLR THRU 7095-EXIT.
01771
01772      IF AT-COMPANY-CD  =  PI-COMPANY-CD  AND
01773         AT-CARRIER     =  PI-CARRIER     AND
01774         AT-CLAIM-NO    =  PI-CLAIM-NO    AND
01775         AT-CERT-NO     =  PI-CERT-NO
01776          NEXT SENTENCE
01777      ELSE
01778          GO TO 5200-START-MSTR.
01779
01780      IF AT-ADDRESS-TYPE NOT = WS-ADDR-TYPE
01781          GO TO 5200-START-MSTR.
01782
01783      IF NOT ADDRESS-TR
01784          GO TO 5100-READ-LOOP.
01785
01786      PERFORM 5800-BUILD-AT-TABLE THRU 5800-EXIT.
01787
01788      ADD +1 TO NDX
01789      GO TO 5100-READ-LOOP.
01790
01791  5200-START-MSTR.
01792 *****
01793 * 5200 PARA FOR READING OF MASTERS FOR OTHER CERT LEVELS
01794 *****
01795      
      * EXEC CICS  ENDBR
01796 *        DATASET  (ELTRLR-FILE-ID)
01797 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006764' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01798
01799      MOVE 'N'                    TO  WS-REWRITE-MSTR-SW.
01800
01801      MOVE PI-COMPANY-CD          TO  ELTRLR-COMPANY-CD.
01802      MOVE PI-CARRIER             TO  ELTRLR-CARRIER.
01803      MOVE PI-CLAIM-NO            TO  ELTRLR-CLAIM-NO.
01804      MOVE LOW-VALUES             TO  ELTRLR-CERT-NO.
01805
01806      PERFORM 7020-START-ELMSTR THRU 7020-EXIT.
01807
01808  5200-READNEXT-ELMSTR.
01809
01810      PERFORM 7025-READNEXT-ELMSTR THRU 7025-EXIT.
01811
01812      IF CL-CONTROL-PRIMARY = WS-SAVE-MSTR-KEY
01813          GO TO 5200-READNEXT-ELMSTR.
01814
01815      IF PI-COMPANY-CD  = ELTRLR-COMPANY-CD   AND
01816         PI-CARRIER     = ELTRLR-CARRIER      AND
01817         PI-CLAIM-NO    = ELTRLR-CLAIM-NO
01818          NEXT SENTENCE
01819      ELSE
01820          GO TO 5200-END-MSTR-BROWSE.
01821
01822      IF WS-ADDR-TYPE EQUAL 'I'
01823          MOVE CL-INSURED-ADDR-CNT
01824                                  TO  WS-BEGIN-CNT
01825          MOVE ZEROS              TO  WS-INCREMENT-NO
01826      ELSE
01827          IF WS-ADDR-TYPE EQUAL 'B'
01828              MOVE CL-BENIF-ADDR-CNT
01829                                  TO  WS-BEGIN-CNT
01830              MOVE +10            TO  WS-INCREMENT-NO
01831          ELSE
01832              IF WS-ADDR-TYPE EQUAL 'A'
01833                  MOVE CL-ACCOUNT-ADDR-CNT
01834                                  TO  WS-BEGIN-CNT
01835                  MOVE +20        TO  WS-INCREMENT-NO
01836              ELSE
01837                  IF WS-ADDR-TYPE EQUAL 'P'
01838                      MOVE CL-DOCTOR-ADDR-CNT
01839                                  TO  WS-BEGIN-CNT
01840                      MOVE +30    TO  WS-INCREMENT-NO
01841                  ELSE
01842                      IF WS-ADDR-TYPE EQUAL 'E'
01843                          MOVE CL-EMPLOYER-ADDR-CNT
01844                                  TO  WS-BEGIN-CNT
01845                          MOVE +40
01846                                  TO  WS-INCREMENT-NO
01847                      ELSE
01848                          IF WS-ADDR-TYPE EQUAL 'O'
01849                              MOVE CL-OTHER-1-ADDR-CNT
01850                                  TO  WS-BEGIN-CNT
01851                              MOVE +50
01852                                  TO  WS-INCREMENT-NO
01853                          ELSE
01854                              MOVE CL-OTHER-2-ADDR-CNT
01855                                  TO  WS-BEGIN-CNT
01856                              MOVE +60
01857                                  TO  WS-INCREMENT-NO.
01858
01859      MOVE WS-BEGIN-CNT           TO  WS-NEW-SEQ.
01860
01861      PERFORM 5500-TRLR-UPDATE  THRU  5500-EXIT.
01862
01863      IF REWRITE-MSTR
01864          MOVE CL-CONTROL-PRIMARY TO  WS-START-MSTR-KEY
01865          
      * EXEC CICS  ENDBR
01866 *            DATASET  (ELMSTR-FILE-ID)
01867 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006834' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01868          PERFORM 7005-READ-ELMSTR-UPDATE  THRU  7005-EXIT
01869          PERFORM 5300-MOVE-NEW-CNT  THRU  5300-EXIT
01870          PERFORM 7010-REWRITE-ELMSTR  THRU  7010-EXIT
01871          MOVE WS-START-MSTR-KEY  TO  ELMSTR-KEY
01872          PERFORM 7020-START-ELMSTR THRU 7020-EXIT
01873          PERFORM 7025-READNEXT-ELMSTR THRU 7025-EXIT
01874          MOVE 'N'                TO  WS-REWRITE-MSTR-SW.
01875
01876      GO TO 5200-READNEXT-ELMSTR.
01877
01878  5200-END-MSTR-BROWSE.
01879      
      * EXEC CICS  ENDBR
01880 *        DATASET  (ELMSTR-FILE-ID)
01881 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006848' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01882
01883      MOVE ER-0000                TO  EMI-ERROR
01884      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01885
01886      MOVE -1                     TO  MFMAINTL.
01887
01888      IF WS-ADDR-TYPE EQUAL 'I'
01889          MOVE AL-PABOF           TO  MTYPE1A
01890      ELSE
01891          IF WS-ADDR-TYPE EQUAL 'B'
01892              MOVE AL-PABOF       TO  MTYPE2A
01893          ELSE
01894              IF WS-ADDR-TYPE EQUAL 'A'
01895                  MOVE AL-PABOF   TO  MTYPE3A
01896              ELSE
01897                  IF WS-ADDR-TYPE EQUAL 'P'
01898                      MOVE AL-PABOF
01899                                  TO  MTYPE4A
01900                  ELSE
01901                       IF WS-ADDR-TYPE EQUAL 'E'
01902                           MOVE AL-PABOF
01903                                  TO  MTYPE5A
01904                       ELSE
01905                           IF WS-ADDR-TYPE EQUAL 'O'
01906                               MOVE AL-PABOF
01907                                  TO  MTYPE6A
01908                           ELSE
01909                               MOVE AL-PABOF
01910                                  TO  MTYPE7A.
01911
01912      MOVE WS-ADDR-I-CNT          TO  ICNTO.
01913      MOVE WS-ADDR-B-CNT          TO  BCNTO.
01914      MOVE WS-ADDR-A-CNT          TO  ACNTO.
01915      MOVE WS-ADDR-P-CNT          TO  PCNTO.
01916      MOVE WS-ADDR-E-CNT          TO  ECNTO.
01917      MOVE WS-ADDR-O-CNT          TO  OCNTO.
01918      MOVE WS-ADDR-Q-CNT          TO  QCNTO.
01919
01920      GO TO 8100-SEND-INITIAL-MAP.
01921
01922      EJECT
01923
01924  5300-MOVE-NEW-CNT.
01925
01926      IF WS-ADDR-TYPE EQUAL 'I'
01927          MOVE WS-NEW-SEQ        TO CL-INSURED-ADDR-CNT
01928      ELSE
01929          IF WS-ADDR-TYPE EQUAL 'B'
01930              MOVE WS-NEW-SEQ
01931                                 TO  CL-BENIF-ADDR-CNT
01932          ELSE
01933              IF WS-ADDR-TYPE EQUAL 'A'
01934                  MOVE WS-NEW-SEQ
01935                                 TO  CL-ACCOUNT-ADDR-CNT
01936              ELSE
01937                  IF WS-ADDR-TYPE EQUAL 'P'
01938                      MOVE WS-NEW-SEQ
01939                                 TO  CL-DOCTOR-ADDR-CNT
01940                  ELSE
01941                      IF WS-ADDR-TYPE EQUAL 'E'
01942                          MOVE WS-NEW-SEQ
01943                                 TO  CL-EMPLOYER-ADDR-CNT
01944                      ELSE
01945                          IF WS-ADDR-TYPE EQUAL 'O'
01946                              MOVE WS-NEW-SEQ
01947                                 TO  CL-OTHER-1-ADDR-CNT
01948                          ELSE
01949                              MOVE WS-NEW-SEQ
01950                                 TO  CL-OTHER-2-ADDR-CNT.
01951
01952  5300-EXIT.
01953       EXIT.
01954      EJECT
01955
01956  5500-TRLR-UPDATE.
01957 *****
01958 * 5500 PARA FOR UPDATING OF TRAILERS FOR OTHER CERT LEVELS
01959 *****
01960
01961      MOVE +1                     TO  WS-WORK-SEQ.
01962      ADD WS-INCREMENT-NO  TO  WS-WORK-SEQ.
01963      MOVE +1                     TO  NDX.
01964
01965      MOVE CL-COMPANY-CD          TO  ELTRLR-COMPANY-CD.
01966      MOVE CL-CARRIER             TO  ELTRLR-CARRIER.
01967      MOVE CL-CLAIM-NO            TO  ELTRLR-CLAIM-NO.
01968      MOVE CL-CERT-NO             TO  ELTRLR-CERT-NO.
01969
01970  5500-TRLR-UPDATE-LOOP.
01971
01972      IF NDX GREATER THAN WS-BEGIN-CNT
01973          IF AT-SAVE-STATUS (NDX) = 'Y'
01974              PERFORM 5600-ADD-TRAILER  THRU  5600-EXIT
01975              GO TO 5500-CHECK-END
01976          ELSE
01977              MOVE +9             TO  NDX
01978              GO TO 5500-CHECK-END.
01979
01980      IF (NDX NOT GREATER THAN WS-BEGIN-CNT) AND
01981         (AT-SAVE-STATUS (NDX) NOT = 'Y')
01982          PERFORM 5700-DELETE-TRAILER  THRU  5700-EXIT
01983          GO TO 5500-CHECK-END.
01984
01985      MOVE WS-WORK-SEQ            TO  ELTRLR-SEQUENCE-NO.
01986
01987      PERFORM 7055-READ-ELTRLR-UPDATE THRU 7055-EXIT.
01988
01989      IF AT-SAVE-STATUS (NDX) = 'Y'
01990          PERFORM 5650-MOVE-SAVED  THRU  5650-EXIT
01991          PERFORM 7060-REWRITE-ELTRLR  THRU  7060-EXIT
01992          ADD +1 TO WS-WORK-SEQ
01993          GO TO 5500-CHECK-END.
01994
01995      PERFORM 7070-DELETE-ELTRLR  THRU  7070-EXIT.
01996
01997      MOVE 'Y'                    TO  WS-REWRITE-MSTR-SW.
01998      ADD +1 TO WS-WORK-SEQ.
01999
02000  5500-CHECK-END.
02001
02002      IF NDX NOT = +9
02003          ADD +1 TO NDX
02004          GO TO 5500-TRLR-UPDATE-LOOP.
02005
02006  5500-EXIT.
02007       EXIT.
02008      EJECT
02009
02010  5600-ADD-TRAILER.
02011
02012      PERFORM 7080-GETMAIN-ELTRLR  THRU  7080-EXIT.
02013
02014      PERFORM 5650-MOVE-SAVED  THRU  5650-EXIT.
02015
02016      MOVE AT-CONTROL-PRIMARY     TO  ELTRLR-KEY.
02017
02018      PERFORM 7065-WRITE-ELTRLR  THRU  7065-EXIT.
02019
02020      MOVE 'Y'                    TO  WS-REWRITE-MSTR-SW.
02021      ADD +1 TO WS-WORK-SEQ.
02022      ADD +1 TO WS-NEW-SEQ.
02023
02024  5600-EXIT.
02025       EXIT.
02026  5650-MOVE-SAVED.
02027
02028      MOVE 'AT'                   TO  AT-RECORD-ID.
02029      MOVE '5'                    TO  AT-TRAILER-TYPE.
02030      MOVE AT-SAVE-COMPANY-CD (NDX)
02031                                  TO  AT-COMPANY-CD.
02032      MOVE AT-SAVE-CARRIER (NDX)  TO  AT-CARRIER.
02033      MOVE AT-SAVE-CLAIM-NO (NDX) TO  AT-CLAIM-NO.
02034      MOVE CL-CERT-NO             TO  AT-CERT-NO.
02035      MOVE WS-WORK-SEQ            TO  AT-SEQUENCE-NO.
02036      MOVE AT-SAVE-RECORDED-DT (NDX)
02037                                  TO  AT-RECORDED-DT.
02038      MOVE AT-SAVE-RECORDED-BY (NDX)
02039                                  TO  AT-RECORDED-BY.
02040      MOVE AT-SAVE-LAST-MAINT-HHMMSS (NDX)
02041                                  TO  AT-LAST-MAINT-HHMMSS.
02042      MOVE AT-SAVE-ADDRESS-TYPE (NDX)
02043                                  TO  AT-ADDRESS-TYPE.
02044      MOVE AT-SAVE-MAIL-TO-NAME (NDX)
02045                                  TO  AT-MAIL-TO-NAME.
02046      MOVE AT-SAVE-ADDRESS-LINE-1 (NDX)
02047                                  TO  AT-ADDRESS-LINE-1.
02048      MOVE AT-SAVE-ADDRESS-LINE-2 (NDX)
02049                                  TO  AT-ADDRESS-LINE-2.
02050      MOVE AT-SAVE-CITY (NDX)     TO  AT-CITY
           MOVE AT-SAVE-STATE (NDX)    TO  AT-STATE
02052      MOVE AT-SAVE-ZIP (NDX)      TO  AT-ZIP.
02053      MOVE AT-SAVE-PHONE-NO (NDX) TO  AT-PHONE-NO.
02054      MOVE AT-SAVE-ADDRESS-LAST-MAINT-DT (NDX)
02055                                  TO  AT-ADDRESS-LAST-MAINT-DT.
02056      MOVE AT-SAVE-ADDRESS-LST-UPDATED-BY (NDX)
02057                                  TO  AT-ADDRESS-LAST-UPDATED-BY.
061511     MOVE AT-SAVE-VFY-2ND-BENE-SSN (NDX)
061511                                 TO  AT-VFY-2ND-BENE-SSN.
061511     MOVE AT-SAVE-VFY-2ND-BENE-VERIFIED (NDX)
061511                                 TO  AT-VFY-2ND-BENE-VERIFIED.
02058
02059  5650-EXIT.
02060       EXIT.
02061      EJECT
02062
02063  5700-DELETE-TRAILER.
02064
02065      MOVE CL-COMPANY-CD          TO  ELTRLR-COMPANY-CD.
02066      MOVE CL-CARRIER             TO  ELTRLR-CARRIER.
02067      MOVE CL-CLAIM-NO            TO  ELTRLR-CLAIM-NO.
02068      MOVE CL-CERT-NO             TO  ELTRLR-CERT-NO.
02069      MOVE WS-WORK-SEQ            TO  ELTRLR-SEQUENCE-NO.
02070
02071      PERFORM 7055-READ-ELTRLR-UPDATE THRU 7055-EXIT.
02072
02073      PERFORM 7070-DELETE-ELTRLR THRU 7070-EXIT.
02074
02075      MOVE 'Y'                    TO  WS-REWRITE-MSTR-SW.
02076      SUBTRACT +1 FROM WS-NEW-SEQ.
02077      ADD +1 TO WS-WORK-SEQ.
02078
02079  5700-EXIT.
02080       EXIT.
02081      EJECT
02082
02083  5800-BUILD-AT-TABLE.
02084
02085      MOVE AT-COMPANY-CD          TO  AT-SAVE-COMPANY-CD (NDX).
02086      MOVE AT-CARRIER             TO  AT-SAVE-CARRIER (NDX).
02087      MOVE AT-CLAIM-NO            TO  AT-SAVE-CLAIM-NO (NDX).
02088      MOVE AT-CERT-NO             TO  AT-SAVE-CERT-NO (NDX).
02089      MOVE AT-SEQUENCE-NO         TO  AT-SAVE-SEQUENCE-NO (NDX).
02090      MOVE AT-TRAILER-TYPE        TO  AT-SAVE-TRAILER-TYPE (NDX).
02091      MOVE AT-RECORDED-DT         TO  AT-SAVE-RECORDED-DT (NDX).
02092      MOVE AT-RECORDED-BY         TO  AT-SAVE-RECORDED-BY (NDX).
02093      MOVE AT-LAST-MAINT-HHMMSS
02094                           TO  AT-SAVE-LAST-MAINT-HHMMSS (NDX).
02095      MOVE AT-ADDRESS-TYPE        TO  AT-SAVE-ADDRESS-TYPE (NDX).
02096      MOVE AT-MAIL-TO-NAME        TO  AT-SAVE-MAIL-TO-NAME (NDX).
02097      MOVE AT-ADDRESS-LINE-1      TO  AT-SAVE-ADDRESS-LINE-1 (NDX).
02098      MOVE AT-ADDRESS-LINE-2      TO  AT-SAVE-ADDRESS-LINE-2 (NDX).
02099      MOVE AT-CITY                TO  AT-SAVE-CITY (NDX)
           MOVE AT-STATE               TO  AT-SAVE-STATE (NDX)
02100      MOVE AT-ZIP                 TO  AT-SAVE-ZIP (NDX).
02101      MOVE AT-PHONE-NO            TO  AT-SAVE-PHONE-NO (NDX).
02102      MOVE AT-ADDRESS-LAST-MAINT-DT
02103                           TO  AT-SAVE-ADDRESS-LAST-MAINT-DT (NDX).
02104      MOVE AT-ADDRESS-LAST-UPDATED-BY
02105                           TO  AT-SAVE-ADDRESS-LST-UPDATED-BY (NDX)
02106      MOVE AT-CERT-NO             TO  AT-SAVE-CERT-NO (NDX).
02107      MOVE AT-SEQUENCE-NO         TO  AT-SAVE-SEQUENCE-NO (NDX).
02108
02109      IF AT-SAVE-MAIL-TO-NAME (NDX)   = SPACES AND
02110         AT-SAVE-ADDRESS-LINE-1 (NDX) = SPACES AND
02111         AT-SAVE-ADDRESS-LINE-2 (NDX) = SPACES AND
02112         AT-SAVE-CITY (NDX)           = SPACES AND
              AT-SAVE-STATE (NDX)          = SPACES
02113          MOVE 'N'                TO  AT-SAVE-STATUS (NDX)
02114      ELSE
02115          MOVE 'Y'                TO  AT-SAVE-STATUS (NDX).
061511     MOVE AT-VFY-2ND-BENE-SSN TO AT-SAVE-VFY-2ND-BENE-SSN (NDX).
061511     MOVE AT-VFY-2ND-BENE-VERIFIED
061511                       TO AT-SAVE-VFY-2ND-BENE-VERIFIED (NDX).
02116  5800-EXIT.
02117       EXIT.
02118      EJECT
02119  5900-CLEAR-AT-TABLE.
02120
02121      MOVE SPACES                 TO  AT-SAVE-CONTROL-PRIMARY (NDX)
02122      MOVE ZEROS                  TO  AT-SAVE-SEQUENCE-NO (NDX).
02123      MOVE SPACES                 TO  AT-SAVE-TRAILER-TYPE (NDX).
02124      MOVE LOW-VALUES             TO  AT-SAVE-RECORDED-DT (NDX).
02125      MOVE LOW-VALUES             TO  AT-SAVE-RECORDED-BY (NDX).
02126      MOVE ZEROS
02127                           TO  AT-SAVE-LAST-MAINT-HHMMSS (NDX).
02128      MOVE SPACES                 TO  AT-SAVE-ADDRESS-TYPE (NDX).
02129      MOVE SPACES                 TO  AT-SAVE-MAIL-TO-NAME (NDX).
02130      MOVE SPACES                 TO  AT-SAVE-ADDRESS-LINE-1 (NDX).
02131      MOVE SPACES                 TO  AT-SAVE-ADDRESS-LINE-2 (NDX).
02132      MOVE SPACES                 TO  AT-SAVE-CITY (NDX)
                                           AT-SAVE-STATE (NDX)
02133      MOVE SPACES                 TO  AT-SAVE-ZIP (NDX).
02134      MOVE ZEROS                  TO  AT-SAVE-PHONE-NO (NDX).
02135      MOVE LOW-VALUES
02136                           TO  AT-SAVE-ADDRESS-LAST-MAINT-DT (NDX).
02137      MOVE SPACES
02138                           TO  AT-SAVE-ADDRESS-LST-UPDATED-BY (NDX)
02139      MOVE SPACES                 TO  AT-SAVE-STATUS (NDX).
061511     MOVE SPACES          TO  AT-SAVE-VFY-2ND-BENE-SSN (NDX).
061511     MOVE SPACES          TO  AT-SAVE-VFY-2ND-BENE-VERIFIED (NDX).
02140
02141      IF NDX = +9
02142          MOVE +1                 TO NDX
02143          GO TO 5900-EXIT
02144      ELSE
02145          ADD +1 TO  NDX
02146          GO TO 5900-CLEAR-AT-TABLE.
02147
02148  5900-EXIT.
02149       EXIT.
02150      EJECT
02151
02152  5999-EXIT. EXIT.
02153      EJECT
02154  6000-EDIT-SCREEN.
02155
02156      IF MAPNAMEI = LOW-VALUES OR SPACES
02157         MOVE ER-0315                 TO  EMI-ERROR
02158         MOVE -1                      TO  MFMAINTL
02159         MOVE AL-UABON                TO  MFMAINTA
02160         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02161         GO TO 6000-EXIT.
02162
02163      MOVE MAPHONEI                   TO  WS-PHONE.
02164      
      * EXEC CICS BIF DEEDIT
02165 *        FIELD (WS-PHONE)
02166 *        LENGTH (12)
02167 *    END-EXEC.
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007145' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PHONE, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02168
02169      IF WS-PHONE  NOT NUMERIC
02170         MOVE ER-0053                 TO  EMI-ERROR
02171         MOVE -1                      TO  MAPHONEL
02172         MOVE AL-UNBON                TO  MAPHONEA
02173         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02174
02175      IF MZIPCODL = ZEROS
02176         MOVE ER-0052                 TO  EMI-ERROR
02177         MOVE -1                      TO  MZIPCODL
02178         MOVE AL-UABON                TO  MZIPCODA
02179         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02180
02181      IF ADD-REQUIRED
02182         IF ((WS-ADDR-TYPE EQUAL 'I') AND
02183            ((CL-INSURED-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))
02184            OR
02185            ((WS-ADDR-TYPE EQUAL 'A') AND
02186            ((CL-ACCOUNT-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM)
02187                AND
02188             (WS-ADDR-SEQ NOT EQUAL '9'))
02189            OR
02190            ((WS-ADDR-TYPE EQUAL 'B') AND
02191            ((CL-BENIF-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM)
02192                AND
02193             (WS-ADDR-SEQ NOT EQUAL '9'))
02194            OR
02195            ((WS-ADDR-TYPE EQUAL 'E') AND
02196            ((CL-EMPLOYER-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))
02197            OR
02198            ((WS-ADDR-TYPE EQUAL 'P') AND
02199            ((CL-DOCTOR-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))
02200            OR
02201            ((WS-ADDR-TYPE EQUAL 'O') AND
02202            ((CL-OTHER-1-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))
02203            OR
02204            ((WS-ADDR-TYPE EQUAL 'Q') AND
02205            ((CL-OTHER-2-ADDR-CNT + 1) NOT EQUAL WS-ADDR-SEQ-NUM))
02206              MOVE ER-1878            TO  EMI-ERROR
02207              MOVE -1                 TO  MADDRTPL
02208              MOVE AL-UABON           TO  MADDRTPA
02209              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02210
02211      IF PI-COMPANY-ID NOT = 'DMD'
02212          GO TO 6000-EDIT-CONTINUE
           END-IF
02213
02214      IF MCITYL GREATER ZERO
              AND MSTATEL > 0
02215         AND MZIPCODL GREATER ZERO
02216 *DLO004
               MOVE SPACES TO DL04-ADDR-LINE
               STRING MCITYI ' ' MSTATEI DELIMITED BY '  '
                  INTO DL04-ADDR-LINE
               END-STRING
02218          MOVE MZIPCODI           TO DL04-ZIP-CODE
02219          
      * EXEC CICS LINK
02220 *            PROGRAM    ('DLO004')
02221 *            COMMAREA   (WS-DLO-VALIDATE-STATE-ZIP)
02222 *            LENGTH     (DL04-COMM-LENGTH)
02223 *        END-EXEC
           MOVE 'DLO004' TO DFHEIV1
      *    MOVE '."C                   (   #00007205' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DLO-VALIDATE-STATE-ZIP, 
                 DL04-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02224          IF DL04-RETURN-CODE = 'OK'
02225              MOVE AL-UANON       TO MCITYA
                                          MSTATEA
02226                                     MZIPCODA
02227           ELSE
02228              MOVE AL-UABON       TO MCITYA
                                          MSTATEA
02229              MOVE -1             TO MCITYL
02230              IF DL04-RETURN-CODE = '01'
02231                  MOVE ER-0853            TO EMI-ERROR
02232                  PERFORM 9900-ERROR-FORMAT
02233                ELSE
02234              IF DL04-RETURN-CODE = '02'
02235                  MOVE ER-0854            TO EMI-ERROR
02236                  PERFORM 9900-ERROR-FORMAT
02237                ELSE
02238              IF DL04-RETURN-CODE = '03'
02239                  MOVE ER-0855            TO EMI-ERROR
02240                  PERFORM 9900-ERROR-FORMAT
02241                ELSE
02242              IF DL04-RETURN-CODE = '04'
02243                  MOVE ER-0856            TO EMI-ERROR
02244                  PERFORM 9900-ERROR-FORMAT
02245                ELSE
02246              IF DL04-RETURN-CODE = '05'
02247                  MOVE ER-0857            TO EMI-ERROR
02248                  PERFORM 9900-ERROR-FORMAT
02249                ELSE
02250              IF DL04-RETURN-CODE = '06'
02251                  MOVE ER-0858            TO EMI-ERROR
02252                  PERFORM 9900-ERROR-FORMAT
02253                ELSE
02254              IF DL04-RETURN-CODE = '07'
02255                  MOVE ER-0859            TO EMI-ERROR
02256                  PERFORM 9900-ERROR-FORMAT
02257                ELSE
02258              IF DL04-RETURN-CODE = '08'
02259                  MOVE ER-0860            TO EMI-ERROR
02260                  PERFORM 9900-ERROR-FORMAT
02261                ELSE
02262              IF DL04-RETURN-CODE = 'N1'
02263                  MOVE ER-0862            TO EMI-ERROR
02264                  PERFORM 9900-ERROR-FORMAT
02265                ELSE
02266              IF DL04-RETURN-CODE = 'E1'
02267                  MOVE ER-0863            TO EMI-ERROR
02268                  PERFORM 9900-ERROR-FORMAT
02269                ELSE
02270                  MOVE ER-0886            TO EMI-ERROR
02271                  PERFORM 9900-ERROR-FORMAT.
           .
       6000-EDIT-CONTINUE.
           IF MSTATEL > 0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE MSTATEI             TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              
      * EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        E          (  N#00007268' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037323638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
02273  6000-EXIT.
02274       EXIT.
02275
02276      EJECT
02277  6500-HIGHLIGHT-EXISTING.
02278
02279      IF CL-INSURED-ADDR-CNT NOT NUMERIC
02280         MOVE ZEROS            TO CL-INSURED-ADDR-CNT.
02281      IF CL-BENIF-ADDR-CNT   NOT NUMERIC
02282         MOVE ZEROS            TO CL-BENIF-ADDR-CNT.
02283      IF CL-ACCOUNT-ADDR-CNT NOT NUMERIC
02284         MOVE ZEROS            TO CL-ACCOUNT-ADDR-CNT.
02285      IF CL-DOCTOR-ADDR-CNT  NOT NUMERIC
02286         MOVE ZEROS            TO CL-DOCTOR-ADDR-CNT.
02287      IF CL-EMPLOYER-ADDR-CNT  NOT NUMERIC
02288         MOVE ZEROS            TO CL-EMPLOYER-ADDR-CNT.
02289      IF CL-OTHER-1-ADDR-CNT NOT NUMERIC
02290         MOVE ZEROS            TO CL-OTHER-1-ADDR-CNT.
02291      IF CL-OTHER-2-ADDR-CNT NOT NUMERIC
02292         MOVE ZEROS            TO CL-OTHER-2-ADDR-CNT.
02293
02294      IF CL-INSURED-ADDR-CNT IS NOT EQUAL TO +0
02295          MOVE AL-PABOF               TO  MTYPE1A
02296      ELSE
02297          MOVE AL-PANOF               TO  MTYPE1A.
02298
02299      IF CL-BENIF-ADDR-CNT IS NOT EQUAL TO +0
02300          MOVE AL-PABOF               TO  MTYPE2A
02301      ELSE
02302          MOVE AL-PANOF               TO  MTYPE2A.
02303
02304      IF CL-ACCOUNT-ADDR-CNT IS NOT EQUAL TO +0
02305          MOVE AL-PABOF               TO  MTYPE3A
02306      ELSE
02307          MOVE AL-PANOF               TO  MTYPE3A.
02308
02309      IF CL-DOCTOR-ADDR-CNT IS NOT EQUAL TO +0
02310          MOVE AL-PABOF               TO  MTYPE4A
02311      ELSE
02312          MOVE AL-PANOF               TO  MTYPE4A.
02313
02314      IF CL-EMPLOYER-ADDR-CNT IS NOT EQUAL TO +0
02315          MOVE AL-PABOF               TO  MTYPE5A
02316      ELSE
02317          MOVE AL-PANOF               TO  MTYPE5A.
02318
02319      IF CL-OTHER-1-ADDR-CNT IS NOT EQUAL TO +0
02320          MOVE AL-PABOF               TO  MTYPE6A
02321      ELSE
02322          MOVE AL-PANOF               TO  MTYPE6A.
02323
02324      IF CL-OTHER-2-ADDR-CNT IS NOT EQUAL TO +0
02325          MOVE AL-PABOF               TO  MTYPE7A
02326      ELSE
02327          MOVE AL-PANOF               TO  MTYPE7A.
02328
02329      MOVE CL-INSURED-ADDR-CNT        TO  ICNTO.
02330      MOVE CL-BENIF-ADDR-CNT          TO  BCNTO.
02331      MOVE CL-ACCOUNT-ADDR-CNT        TO  ACNTO.
02332      MOVE CL-DOCTOR-ADDR-CNT         TO  PCNTO.
02333      MOVE CL-EMPLOYER-ADDR-CNT       TO  ECNTO.
02334      MOVE CL-OTHER-1-ADDR-CNT        TO  OCNTO.
02335      MOVE CL-OTHER-2-ADDR-CNT        TO  QCNTO.
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
02336
02337  6500-EXIT.
02338      EXIT.
02339
02340      EJECT
061511
061511 6700-GET-VFY-BENE-IND.
061511       MOVE SPACES              TO ELCNTL-KEY
061511       MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
061511       MOVE '3'                 TO ELCNTL-REC-TYPE
061511       MOVE PI-STATE            TO ELCNTL-ACCESS
061511       MOVE +0                  TO ELCNTL-SEQ
061511       
      * EXEC CICS READ
061511*         DATASET   (FILE-ID-ELCNTL)
061511*         SET       (ADDRESS OF CONTROL-FILE)
061511*         RIDFLD    (ELCNTL-KEY)
061511*         RESP      (WS-RESPONSE)
061511*      END-EXEC
      *    MOVE '&"S        E          (  N#00007393' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
061511       
      * EXEC CICS READ
061511*         DATASET   (FILE-ID-ELCNTL)
061511*         SET       (ADDRESS OF CONTROL-FILE)
061511*         RIDFLD    (ELCNTL-KEY)
061511*         RESP      (WS-RESPONSE)
061511*      END-EXEC
      *    MOVE '&"S        E          (  N#00007420' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
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
02341 ******************************************************************
02342 *       I/O REQUESTS AGAINST THE CLAIM MASTER FILE.              *
02343 ******************************************************************
02344  7000-READ-ELMSTR.
02345
02346      
      * EXEC CICS HANDLE CONDITION
02347 *        NOTFND   (9040-NOTFND-ELMSTR)
02348 *        NOTOPEN  (9030-NOTOPEN-ELMSTR)
02349 *    END-EXEC.
      *    MOVE '"$IJ                  ! - #00007446' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303037343436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02350
02351      
      * EXEC CICS READ
02352 *        DATASET   (ELMSTR-FILE-ID)
02353 *        RIDFLD    (ELMSTR-KEY)
02354 *        SET       (ADDRESS OF CLAIM-MASTER)
02355 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007451' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02356
02357  7000-EXIT.
02358      EXIT.
02359
02360  7005-READ-ELMSTR-UPDATE.
02361
02362      
      * EXEC CICS HANDLE CONDITION
02363 *        NOTFND   (9040-NOTFND-ELMSTR)
02364 *        NOTOPEN  (9030-NOTOPEN-ELMSTR)
02365 *    END-EXEC.
      *    MOVE '"$IJ                  ! . #00007462' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303037343632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02366
02367      
      * EXEC CICS READ
02368 *        DATASET   (ELMSTR-FILE-ID)
02369 *        RIDFLD    (ELMSTR-KEY)
02370 *        SET       (ADDRESS OF CLAIM-MASTER)
02371 *        UPDATE
02372 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007467' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02373
02374  7005-EXIT.
02375      EXIT.
02376
02377  7020-START-ELMSTR.
02378
02379      
      * EXEC CICS HANDLE CONDITION
02380 *        NOTFND   (9040-NOTFND-ELMSTR)
02381 *        NOTOPEN  (9030-NOTOPEN-ELMSTR)
02382 *    END-EXEC.
      *    MOVE '"$IJ                  ! / #00007479' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303037343739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02383
02384      
      * EXEC CICS STARTBR
02385 *        DATASET   (ELMSTR-FILE-ID)
02386 *        RIDFLD    (ELMSTR-KEY)
02387 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007484' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02388
02389  7020-EXIT.
02390      EXIT.
02391
02392  7025-READNEXT-ELMSTR.
02393
02394      
      * EXEC CICS HANDLE CONDITION
02395 *        NOTFND   (9040-NOTFND-ELMSTR)
02396 *        NOTOPEN  (9030-NOTOPEN-ELMSTR)
02397 *    END-EXEC.
      *    MOVE '"$IJ                  ! 0 #00007494' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303037343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02398
02399      
      * EXEC CICS READNEXT
02400 *        DATASET   (ELMSTR-FILE-ID)
02401 *        RIDFLD    (ELMSTR-KEY)
02402 *        SET       (ADDRESS OF CLAIM-MASTER)
02403 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007499' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02404
02405  7025-EXIT.
02406      EXIT.
02407      EJECT
02408  7010-REWRITE-ELMSTR.
02409
02410      
      * EXEC CICS REWRITE
02411 *        DATASET   (ELMSTR-FILE-ID)
02412 *        FROM      (CLAIM-MASTER)
02413 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007510' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02414
02415  7010-EXIT.
02416      EXIT.
02417
02418  7015-UNLOCK-ELMSTR.
02419
02420      
      * EXEC CICS UNLOCK
02421 *        DATASET   (ELMSTR-FILE-ID)
02422 *    END-EXEC.
      *    MOVE '&*                    #   #00007520' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02423
02424  7015-EXIT.
02425      EXIT.
02426
02427      EJECT
02428 ******************************************************************
02429 *       I/O REQUESTS AGAINST THE ACTIVITY TRAILER FILE           *
02430 ******************************************************************
02431  7050-READ-ELTRLR.
02432
PEMMOD*    EXEC CICS HANDLE CONDITION
PEMMOD*        NOTFND   (9050-NOTFND-ELTRLR)
PEMMOD*        NOTOPEN  (9060-NOTOPEN-ELTRLR)
PEMMOD*    END-EXEC.
02437
02438      
      * EXEC CICS READ
02439 *        DATASET   (ELTRLR-FILE-ID)
02440 *        RIDFLD    (ELTRLR-KEY)
02441 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
02442 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007538' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02443
02444  7050-EXIT.
02445      EXIT.
02446
02447  7055-READ-ELTRLR-UPDATE.
02448
02449      
      * EXEC CICS HANDLE CONDITION
02450 *        NOTFND   (9050-NOTFND-ELTRLR)
02451 *        NOTOPEN  (9060-NOTOPEN-ELTRLR)
02452 *    END-EXEC.
      *    MOVE '"$IJ                  ! 1 #00007549' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303037353439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02453
02454      
      * EXEC CICS READ
02455 *        DATASET   (ELTRLR-FILE-ID)
02456 *        RIDFLD    (ELTRLR-KEY)
02457 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
02458 *        UPDATE
02459 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007554' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02460
02461  7055-EXIT.
02462      EXIT.
02463
02464  7060-REWRITE-ELTRLR.
02465
02466      
      * EXEC CICS REWRITE
02467 *        DATASET   (ELTRLR-FILE-ID)
02468 *        FROM      (ACTIVITY-TRAILERS)
02469 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007566' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02470
02471  7060-EXIT.
02472      EXIT.
02473
02474  7065-WRITE-ELTRLR.
02475
02476      
      * EXEC CICS HANDLE CONDITION
02477 *        DUPREC   (7065-DUP)
02478 *    END-EXEC.
      *    MOVE '"$%                   ! 2 #00007576' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303037353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02479
02480      
      * EXEC CICS WRITE
02481 *        DATASET   (ELTRLR-FILE-ID)
02482 *        FROM      (ACTIVITY-TRAILERS)
02483 *        RIDFLD    (ELTRLR-KEY)
02484 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007580' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02485
02486      GO TO 7065-EXIT.
02487
02488  7065-DUP.
02489
02490  7065-EXIT.
02491      EXIT.
02492
02493  7070-DELETE-ELTRLR.
02494
02495      
      * EXEC CICS DELETE
02496 *       DATASET   (ELTRLR-FILE-ID)
02497 *    END-EXEC.
      *    MOVE '&(                    &   #00007595' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02498
02499  7070-EXIT.
02500      EXIT.
02501
02502  7075-UNLOCK-ELTRLR.
02503
02504      
      * EXEC CICS UNLOCK
02505 *        DATASET   (ELTRLR-FILE-ID)
02506 *    END-EXEC.
      *    MOVE '&*                    #   #00007604' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02507
02508  7075-EXIT.
02509      EXIT.
02510
02511  7080-GETMAIN-ELTRLR.
02512
02513      
      * EXEC CICS  GETMAIN
02514 *           SET      (ADDRESS OF ACTIVITY-TRAILERS)
02515 *           INITIMG  (GETMAIN-SPACE)
02516 *           LENGTH   (WS-TRLR-LENGTH)
02517 *    END-EXEC.
      *    MOVE ',"IL                  $   #00007613' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02518
02519  7080-EXIT.
02520
02521  7090-START-ELTRLR.
02522
02523      
      * EXEC CICS HANDLE CONDITION
02524 *        NOTFND   (9050-NOTFND-ELTRLR)
02525 *        NOTOPEN  (9060-NOTOPEN-ELTRLR)
02526 *    END-EXEC.
      *    MOVE '"$IJ                  ! 3 #00007623' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303037363233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02527
02528      
      * EXEC CICS STARTBR
02529 *        DATASET   (ELTRLR-FILE-ID)
02530 *        RIDFLD    (ELTRLR-KEY)
02531 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007628' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02532
02533  7090-EXIT.
02534      EXIT.
02535
02536  7095-READNEXT-ELTRLR.
02537
02538      
      * EXEC CICS HANDLE CONDITION
02539 *        NOTFND   (9050-NOTFND-ELTRLR)
02540 *        NOTOPEN  (9060-NOTOPEN-ELTRLR)
02541 *    END-EXEC.
      *    MOVE '"$IJ                  ! 4 #00007638' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303037363338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02542
02543      
      * EXEC CICS READNEXT
02544 *        DATASET   (ELTRLR-FILE-ID)
02545 *        RIDFLD    (ELTRLR-KEY)
02546 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
02547 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007643' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02548
02549  7095-EXIT.
02550      EXIT.
02551      EJECT
02552  8000-LOAD-ERROR-MESSAGES.
02553
02554      MOVE SPACES                 TO  MERMSG1O
02555                                      MERMSG2O.
02556
02557      IF EMI-NO-ERRORS
02558          GO TO 8000-EXIT.
02559
02560      IF EMI-NUMBER-OF-LINES IS EQUAL TO 1
02561          MOVE EMI-LINE1          TO  MERMSG1O
02562          GO TO 8000-EXIT.
02563
02564      IF EMI-NUMBER-OF-LINES IS EQUAL TO 2
02565          MOVE EMI-LINE1          TO  MERMSG1O
02566          MOVE EMI-LINE2          TO  MERMSG2O
02567          GO TO 8000-EXIT.
02568
02569      MOVE EMI-LINE1              TO  MERMSG1O.
02570
02571  8000-EXIT.
02572      EXIT.
02573
02574  8100-SEND-INITIAL-MAP.
02575      MOVE EIBTIME                TO  TIME-IN.
02576      MOVE WS-HOUR                TO  WS-TRANS-HOUR.
02577      MOVE WS-MINUTE              TO  WS-TRANS-MINUTE.
02578
02579      MOVE TIME-OUT               TO  MRNTIMEO.
02580      MOVE SAVE-DATE              TO  MRNDATEO.
031102     MOVE PI-CERT-NO             TO  MCERTO.
02581
02582      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
02583
02584      
      * EXEC CICS  SEND
02585 *         MAPSET   (WS-MAPSET-NAME)
02586 *         MAP      (WS-MAP-NAME)
02587 *         FROM     (EL141AO)
02588 *         FREEKB
02589 *         ERASE
02590 *         CURSOR
02591 *    END-EXEC.
           MOVE LENGTH OF
            EL141AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E F  H L F ,   #00007685' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2046202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL141AO, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02592
02593      GO TO 9100-RETURN-TRAN.
02594
02595  8100-EXIT.
02596       EXIT.
02597
02598  8200-SEND-DATAONLY.
02599      MOVE EIBTIME                    TO  TIME-IN.
02600      MOVE WS-HOUR                    TO  WS-TRANS-HOUR.
02601      MOVE WS-MINUTE                  TO  WS-TRANS-MINUTE.
02602
02603      MOVE TIME-OUT                   TO  MRNTIMEO.
02604      MOVE SAVE-DATE                  TO  MRNDATEO.
031102     MOVE PI-CERT-NO                 TO  MCERTO.
02605
02606      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
02607
02608      
      * EXEC CICS  SEND
02609 *         MAPSET   (WS-MAPSET-NAME)
02610 *         MAP      (WS-MAP-NAME)
02611 *         FROM     (EL141AO)
02612 *         FREEKB
02613 *         DATAONLY
02614 *         CURSOR
02615 *    END-EXEC.
           MOVE LENGTH OF
            EL141AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT    F  H L F ,   #00007710' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2046202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL141AO, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02616
02617      GO TO 9100-RETURN-TRAN.
02618
02619  8200-EXIT.
02620       EXIT.
02621
02622  8300-SEND-TEXT.
02623      
      * EXEC CICS SEND TEXT
02624 *        FROM     (LOGOFF-TEXT)
02625 *        ERASE
02626 *        FREEKB
02627 *        LENGTH   (LOGOFF-LENGTH)
02628 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007725' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02629
02630      
      * EXEC CICS RETURN
02631 *        END-EXEC.
      *    MOVE '.(                    ''   #00007732' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02632
02633  8300-EXIT.
02634       EXIT.
02635
02636  8500-DATE-CONVERSION.
02637
02638      MOVE LINK-ELDATCV               TO  PGM-NAME.
02639      
      * EXEC CICS LINK
02640 *           PROGRAM  (PGM-NAME)
02641 *           COMMAREA (DATE-CONVERSION-DATA)
02642 *           LENGTH   (DC-COMM-LENGTH)
02643 *    END-EXEC.
      *    MOVE '."C                   (   #00007741' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02644
02645  8500-EXIT.
02646       EXIT.
02647
02648      EJECT
02649  9000-UNAUTHERR.
02650      MOVE UNACCESS-MSG               TO  LOGOFF-MSG.
02651      GO TO 8300-SEND-TEXT.
02652
02653  9030-NOTOPEN-ELMSTR.
02654
02655      MOVE LOW-VALUES                 TO  EL141AI.
02656      MOVE ER-0154                    TO  EMI-ERROR.
02657      MOVE -1                         TO  MFMAINTL.
02658      MOVE AL-UABON                   TO  MFMAINTA.
02659      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.
02660      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02661      GO TO 8100-SEND-INITIAL-MAP.
02662
02663  9040-NOTFND-ELMSTR.
02664
02665      MOVE LOW-VALUES                 TO  EL141AI.
02666      MOVE ER-0133                    TO  EMI-ERROR.
02667      MOVE -1                         TO  MFMAINTL.
02668      MOVE AL-UABON                   TO  MFMAINTA.
02669      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.
02670      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02671      GO TO 8100-SEND-INITIAL-MAP.
02672
02673  9050-NOTFND-ELTRLR.
02674
02675      MOVE LOW-VALUES                 TO  EL141AI.
02676      MOVE ER-0135                    TO  EMI-ERROR.
02677      MOVE -1                         TO  MADDRTPL.
02678      MOVE AL-UABON                   TO  MADDRTPA
02679                                          MFMAINTA.
02680      MOVE WS-ADDR-TYPE-SW            TO  MADDRTPI.
02681      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.
02682      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02683      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
02684      GO TO 8100-SEND-INITIAL-MAP.
02685
02686  9060-NOTOPEN-ELTRLR.
02687
02688      MOVE LOW-VALUES                 TO  EL141AI.
02689      MOVE ER-0172                    TO  EMI-ERROR.
02690      MOVE -1                         TO  MFMAINTL.
02691      MOVE AL-UABON                   TO  MFMAINTA.
02692      MOVE WS-MAINT-FUNC-SW           TO  MFMAINTI.
02693      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02694      PERFORM 6500-HIGHLIGHT-EXISTING THRU 6500-EXIT.
02695      GO TO 8100-SEND-INITIAL-MAP.
02696
02697      EJECT
02698  9100-RETURN-TRAN.
02699      
      * EXEC CICS RETURN
02700 *        TRANSID    (WS-TRANS-ID)
02701 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
02702 *        LENGTH     (PI-COMM-LENGTH)
02703 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00007801' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02704
02705  9300-XCTL.
02706      
      * EXEC CICS XCTL
02707 *        PROGRAM  (PGM-NAME)
02708 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02709 *        LENGTH   (PI-COMM-LENGTH)
02710 *    END-EXEC.
      *    MOVE '.$C                   %   #00007808' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02711
02712  9300-EXIT.
02713       EXIT.
02714
02715  9400-CLEAR.
02716
02717      MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.
02718      GO TO 9300-XCTL.
02719
02720  9400-EXIT.
02721      EXIT.
02722
02723      EJECT
02724  9600-PGMIDERR.
02725      
      * EXEC CICS HANDLE CONDITION
02726 *        PGMIDERR (8300-SEND-TEXT)
02727 *    END-EXEC.
      *    MOVE '"$L                   ! 5 #00007827' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303037383237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02728
02729      MOVE THIS-PGM                   TO  LOGOFF-PGM
02730                                          PI-CALLING-PROGRAM.
02731      MOVE SPACES                     TO  PI-ENTRY-CD-1.
02732      MOVE 'EL005'                    TO  PGM-NAME.
02733      MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.
02734      GO TO 9300-XCTL.
02735
02736  9600-EXIT.
02737       EXIT.
02738
02739  9900-ERROR-FORMAT.
02740      MOVE LINK-001                   TO  PGM-NAME.
02741      
      * EXEC CICS LINK
02742 *           PROGRAM  (PGM-NAME)
02743 *           COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
02744 *           LENGTH   (EMI-COMM-LENGTH)
02745 *    END-EXEC.
      *    MOVE '."C                   (   #00007843' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02746
02747 *    MOVE EMI-LINE1                  TO  MERMSG1O.
02748 *    MOVE EMI-LINE2                  TO  MERMSG2O.
02749
02750  9900-EXIT.
02751       EXIT.
02752
02753  9990-ERROR.
02754
02755      MOVE LINK-004                   TO  PGM-NAME.
02756      MOVE -1                         TO  MFMAINTL.
02757      MOVE DFHEIBLK                   TO  EMI-LINE1.
02758      
      * EXEC CICS LINK
02759 *        PROGRAM   (PGM-NAME)
02760 *        COMMAREA  (EMI-LINE1)
02761 *        LENGTH    (72)
02762 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007860' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02763
02764      GO TO 8200-SEND-DATAONLY.
02765
02766  9995-SECURITY-VIOLATION.
02767 *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00007886' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
02768
02769  9995-EXIT.
02770      EXIT.
02771
02772  9999-LAST-PARAGRAPH.
02773      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL141' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL141' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ERROR,
                     9600-PGMIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1010-NOTFND-ELTRLR,
                     1020-NOTOPEN-ELTRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1110-NO-BENE,
                     1120-BENE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1230-NO-ACCT,
                     1240-ACCT-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1230-NO-ACCT,
                     1210-READNEXT-ACCT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1280-NO-PRODUCER,
                     1290-PRODUCER-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1280-NO-PRODUCER,
                     1260-READNEXT-PRODUCER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1320-ERCOMP-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 9050-NOTFND-ELTRLR,
                     9060-NOTOPEN-ELTRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 3030-TRLR-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 4010-NOTFND-ELTRLR,
                     4020-NOTOPEN-ELTRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 9040-NOTFND-ELMSTR,
                     9030-NOTOPEN-ELMSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 9040-NOTFND-ELMSTR,
                     9030-NOTOPEN-ELMSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 9040-NOTFND-ELMSTR,
                     9030-NOTOPEN-ELMSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 9040-NOTFND-ELMSTR,
                     9030-NOTOPEN-ELMSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 9050-NOTFND-ELTRLR,
                     9060-NOTOPEN-ELTRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 7065-DUP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 9050-NOTFND-ELTRLR,
                     9060-NOTOPEN-ELTRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 9050-NOTFND-ELTRLR,
                     9060-NOTOPEN-ELTRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL141' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
