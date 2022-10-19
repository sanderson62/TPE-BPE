00001  IDENTIFICATION DIVISION.                                         06/18/97
00002                                                                   EL1272
00003  PROGRAM-ID.                 EL1272.                                 LV036
00004 *              PROGRAM CONVERTED BY                                  CL*32
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*32
00006 *              CONVERSION DATE 02/07/95 10:55:03.                    CL*32
00007 *                            VMOD=2.036.                             CL*36
00008 *                                                                 EL1272
00008 *                                                                 EL1272
00009 *AUTHOR.     LOGIC INC.                                              CL*32
00010 *            DALLAS, TEXAS.                                          CL*32
00011                                                                   EL1272
00012 *DATE-COMPILED.                                                      CL*32
00013                                                                      CL*11
00014 *SECURITY.   *****************************************************   CL*32
00015 *            *                                                   *   CL*32
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*32
00017 *            *                                                   *   CL*32
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*32
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*32
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*32
00021 *            *                                                   *   CL*32
00022 *            *****************************************************   CL*32
00023                                                                   EL1272
00024 *REMARKS.                                                            CL**3
00025 *        THIS PROGRAM PROVIDES THE BROWSE NECESSARY FOR              CL**3
00026 *    THE CERTIFICATE LOOK-UP.                                        CL**3
00027                                                                   EL1272
00028 *    SCREENS     - EL127B - CERTIFICATE LOOK-UP MATCH LIST           CL**3
00029                                                                   EL1272
00030 *    ENTERED BY  - EL127 - CERTIFICATE QUALIFICATION                 CL**3
00031                                                                   EL1272
00032 *    EXIT TO     - EL127                                             CL**3
00033 *                  EL130 - NEW CLAIM SETUP                           CL**3
00034                                                                   EL1272
00035 *    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE                 CL**3
00036                                                                   EL1272
00037 *    OUTPUT FILE - NONE                                              CL**3
00038                                                                   EL1272
00039 *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE        CL**3
00040 *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE      CL**3
00041 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR           CL**3
00042 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM    CL**3
00043 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE     CL**3
00044 *                  RECORD KEY INFORMATION NEEDED BY EL1272 TO        CL**3
00045 *                  LOCATE THE CERTIFICATE.                           CL**3
00046                                                                   EL1272
00047 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON        CL**3
00048 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE    CL**3
00049 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE   CL**3
00050 *                  ENTRIES (XCTL FROM CICS VIA EXX2) THE SCREEN      CL**3
00051 *                  WILL BE READ AND ACTION WILL BE BASED ON THE      CL**3
00052 *                  MAINTENANCE TYPE INDICATED.                       CL**3
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & COMPANY ID TO SCREEN
101501*                              ADJUSTED REDEFINES EL127BI FILLER
110106* 110106  CR2005050500006  PEMA  ADD STATE TO OPTION 3 
101501******************************************************************

00053      EJECT                                                        EL1272
00054  ENVIRONMENT DIVISION.                                            EL1272
00055                                                                   EL1272
00056  DATA DIVISION.                                                   EL1272
00057                                                                   EL1272
00058  WORKING-STORAGE SECTION.                                         EL1272
00059  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.      CL*32
00060  77  LCP-ONCTR-02                  PIC S9(8) COMP-3 VALUE ZERO.      CL*32
00061                                                                   EL1272
00062  77  FILLER  PIC X(32)  VALUE '********************************'. EL1272
00063  77  FILLER  PIC X(32)  VALUE '*   EL1272 WORKING STORAGE     *'. EL1272
00064  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.036 *********'.    CL*36
00065                                                                      CL*13
       01  filler.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-TERMIDERR           VALUE +11.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-DUPREC              VALUE +14.
               88  RESP-DUPKEY              VALUE +15.
               88  RESP-INVREQ              VALUE +16.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
               88  RESP-ILLOGIC             VALUE +21.
               88  RESP-LENGERR             VALUE +22.

00066  01  FLA-WS.                                                         CL*13
00067      05  SAVE-NAME                   PIC X(15).                      CL*13
00068      05  FILLER REDEFINES SAVE-NAME.                                 CL*13
00069          10 SV-BYTE                  PIC X OCCURS 15.                CL*13
00070      05  A-SUB                       PIC 99.                         CL*13
00071      05  B-SUB                       PIC 99.                         CL*13
00072                                                                   EL1272
00073                                      COPY ELCSCTM.                   CL*15
00074                                                                   EL1272
00075                                      COPY ELCSCRTY.                  CL*15
00076                                                                   EL1272
00077  01  WS-DATE-AREA.                                                EL1272
00078      05  SAVE-DATE                   PIC X(8)     VALUE SPACES.   EL1272
00079      05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.   EL1272
00080                                                                   EL1272
00081  01  FILLER                          COMP-3.                      EL1272
00082      05  WS-NOT-FOUND                PIC S9       VALUE ZERO.     EL1272
00083      05  WS-ST-REC-NOT-FOUND         PIC S9       VALUE ZERO.        CL*35
00084      05  TIME-IN                     PIC S9(7)    VALUE ZERO.     EL1272
00085      05  TIME-OUT                    REDEFINES                    EL1272
00086          TIME-IN                     PIC S9(3)V9(4).              EL1272
00087                                                                   EL1272
00088      05  WS-MONTH-WORK               PIC S9(3)    VALUE ZERO.     EL1272
00089      05  WS-YEAR-WORK                PIC S9(3)    VALUE ZERO.     EL1272
00090      05  WS-CERT-SW                  PIC S9       VALUE ZERO.     EL1272
00091         88  WS-NO-CERT-FOUND                      VALUE ZERO.     EL1272
00092      05  SUB                         PIC S99      VALUE ZERO.        CL**3
00093                                                                   EL1272
00094  01  FILLER                          COMP                         EL1272
00095                                      SYNC.                        EL1272
00096      05  WS-INDEX                    PIC S9(4)    VALUE ZERO.     EL1272
00097      05  WS-TS-LENGTH                PIC S9(4)    VALUE +1920.    EL1272
00098      05  WS-WORK-LENGTH              PIC S9(4)    VALUE +1024.       CL*22
00099      05  WS-AIX-RECORD-COUNT         PIC S9(4)    VALUE ZERO.     EL1272
00100                                                                   EL1272
00101  01  FILLER.                                                      EL1272
00102                                                                   EL1272
00103      05  WS-COMPARE-INDICATOR        PIC  X.                         CL*25
00104          88 NAME-FOUND                         VALUE SPACE.          CL*25
00105          88 NAME-NOT-FOUND                     VALUE 'X'.            CL*25
00106      05  WS-NAME-INDEX               PIC S9(4)  COMP.                CL*25
00107      05  WS-CM-NAME                  PIC X(15).                      CL*25
00108      05  WS-CM-NAME-CHAR REDEFINES                                   CL*25
00109          WS-CM-NAME                  PIC X                           CL*25
00110                                      OCCURS 15.                      CL*25
00111      05  WS-PI-NAME                  PIC  X(15).                     CL*25
00112      05  WS-PI-NAME-CHAR  REDEFINES                                  CL*25
00113          WS-PI-NAME                  PIC  X                          CL*25
00114                                      OCCURS 15.                      CL*25
00115                                                                      CL*25
00116      05  QID.                                                     EL1272
00117          10  QID-TERM                PIC X(4).                    EL1272
00118          10  FILLER                  PIC X(4)     VALUE '127A'.   EL1272
00119      05  QID-ITEM                    PIC S9(4)    VALUE +1  COMP. EL1272
00120                                                                   EL1272
00121      05  WS-CONTROL-FILE-KEY.                                     EL1272
00122          10  WS-CFK-COMPANY-ID           PIC X(3).                   CL*17
00123          10  WS-CFK-RECORD-TYPE          PIC X.                      CL*17
00124          10  WS-CFK-ACCESS-TYPE.                                  EL1272
00125              15  WS-CFK-BENE-ACCESS.                                 CL*17
00126                  20  FILLER              PIC XX.                     CL*17
00127                  20  WS-CFK-BENEFIT-NO   PIC XX.                     CL*17
00128              15  WS-CFK-PROC-ACCESS REDEFINES WS-CFK-BENE-ACCESS.    CL*17
00129                  20  WS-CFK-PROD-ID      PIC X(04).                  CL*17
00130          10  WS-CFK-SEQUENCE-NO          PIC S9(4)  COMP.            CL*17
00131                                                                   EL1272
00132      05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL127S'. EL1272
00133      05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127B'. EL1272
00134                                                                   EL1272
00135      05  FILLER                      REDEFINES                    EL1272
00136          WS-MAP-NAME.                                             EL1272
00137          10  FILLER                  PIC XX.                      EL1272
00138          10  WS-MAP-NUMBER           PIC X(4).                    EL1272
00139          10  FILLER                  PIC XX.                         CL*34
00140                                                                   EL1272
00141      05  THIS-PGM                    PIC X(8)     VALUE 'EL1272'. EL1272
00142      05  ELRTRM                      PIC X(8)     VALUE 'ELRTRM'. EL1272
00143                                                                   EL1272
00144      05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'. EL1272
00145                                                                   EL1272
00146      05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX2'.   EL1272
00147                                                                   EL1272
00148      05  WS-TEMP-STORAGE-KEY.                                     EL1272
00149          10  WS-TSK-TERM-ID          PIC X(4)     VALUE 'XXXX'.   EL1272
00150          10  FILLER                  PIC X(4)     VALUE '1272'.   EL1272
00151                                                                      CL*22
00152      05  WS-EL1273-TS.                                               CL*22
00153          10  WS-TS1-TERM-ID          PIC X(4)     VALUE 'XXXX'.      CL*22
00154          10  FILLER                  PIC X(4)     VALUE '1273'.      CL*22
00155                                                                   EL1272
00156      05  WS-INITIALS.                                             EL1272
00157          10  WS-INIT1                PIC X.                       EL1272
00158          10  WS-INIT2                PIC X.                       EL1272
00159                                                                   EL1272
00160      05  WS-CURRENT-DATE             PIC XX.                      EL1272
00161                                                                   EL1272
00162      05  WS-KEY-HOLD.                                             EL1272
00163          10  WS-KH-CHAR              PIC X                        EL1272
00164              OCCURS 33 TIMES         INDEXED BY KEY-INDEX.           CL*25
00165                                                                   EL1272
00166      05  WS-KEY-INPUT.                                            EL1272
00167          10  WS-KI-CHAR              PIC X                        EL1272
00168              OCCURS 33 TIMES         INDEXED BY KEY-INDEX2.          CL*25
00169                                                                   EL1272
00170      05  WS-CALC-RDNXT               PIC S9(8)    VALUE ZERO COMP.EL1272
00171      05  WS-CERTS-SELECTED           PIC X        VALUE 'N'.         CL**3
00172          88  NO-CERTS-SELECTED                    VALUE 'N'.         CL**3
00173      05  WS-SELECTED-SW              PIC X        VALUE 'N'.         CL**3
00174          88  PREVIOUSLY-SELECTED                  VALUE 'Y'.         CL**3
00175                                                                      CL**3
00176      05  WS-CNTL-REC-FOUND-SW        PIC X        VALUE SPACE.       CL*36
00177      05  WS-NEXT-COMPANY-ID          PIC XXX      VALUE SPACES.      CL*36
00178                                                                      CL*17
00179      05  WS-CERT-CONTROL.                                            CL**3
00180          10  WS-CARRIER              PIC X.                          CL**3
00181          10  WS-GROUPING             PIC X(6).                       CL**3
00182          10  WS-STATE                PIC XX.                         CL**3
00183          10  WS-ACCOUNT              PIC X(10).                      CL**3
00184          10  WS-CERT-NO              PIC X(11).                      CL**3
00185          10  WS-EFF-DT               PIC XX.                         CL**3
00186                                                                   EL1272
00187      EJECT                                                        EL1272
00188      05  ERROR-MESSAGES.                                          EL1272
00189          10  ER-0004                 PIC X(4)     VALUE '0004'.   EL1272
00190          10  ER-0008                 PIC X(4)     VALUE '0008'.   EL1272
00191          10  ER-0019                 PIC X(4)     VALUE '0019'.      CL*17
00192          10  ER-0022                 PIC X(4)     VALUE '0022'.      CL*17
00193          10  ER-0029                 PIC X(4)     VALUE '0029'.   EL1272
00194          10  ER-0070                 PIC X(4)     VALUE '0070'.   EL1272
00195          10  ER-0089                 PIC X(4)     VALUE '0089'.      CL*17
00196          10  ER-0130                 PIC X(4)     VALUE '0130'.   EL1272
00197          10  ER-0131                 PIC X(4)     VALUE '0131'.      CL*20
00198          10  ER-0200                 PIC X(4)     VALUE '0200'.   EL1272
00199          10  ER-0201                 PIC X(4)     VALUE '0201'.   EL1272
00200          10  ER-0228                 PIC X(4)     VALUE '0228'.      CL*17
00201          10  ER-0363                 PIC X(4)     VALUE '0363'.   EL1272
00202          10  ER-0659                 PIC X(4)     VALUE '0659'.      CL**3
00203          10  ER-0765                 PIC X(4)     VALUE '0765'.      CL*22
00204          10  ER-2848                 PIC X(4)     VALUE '2848'.      CL*35
00205                                                                   EL1272
00206      EJECT                                                        EL1272
00207                                      COPY ELCNWA.                    CL*32
00208                                                                   EL1272
00209      EJECT                                                        EL1272
00210                                      COPY ELCINTF.                   CL*15
00211                                                                   EL1272
00212      EJECT                                                        EL1272
00213                                      COPY ELC127PI.                  CL*15
00214          16  PI-SUB                       PIC S99.                   CL**3
00215          16  PI-FIRST-TIME-SW             PIC X.                     CL**3
00216          16  PI-EL127-TO-EL130-CNTRL.                                CL**3
00217              20  PI-CERT-SELECT-CNT       PIC S9(4)   COMP.          CL**3
00218              20  PI-CERT-PROCESSED-CNT    PIC S9(4)   COMP.          CL**3
00219              20  PI-CERT-CONTROLS-EL127 OCCURS 5 TIMES.              CL**3
00220                  24  PI-EL127-CARRIER     PIC X.                     CL**3
00221                  24  PI-EL127-GROUPING    PIC X(6).                  CL**3
00222                  24  PI-EL127-STATE       PIC XX.                    CL**3
00223                  24  PI-EL127-ACCOUNT     PIC X(10).                 CL**3
00224                  24  PI-EL127-CERT-NO     PIC X(11).                 CL**3
00225                  24  PI-EL127-EFF-DT      PIC XX.                    CL**3
00226          16  PI-PART-KEY-SW               PIC X.                     CL*32
00227          16  PI-PART-FIELD-SW             PIC X.                     CL*32
00228          16  PI-SAVE-KEY-LENGTH           PIC S9(4)    COMP.         CL*32
00229          16  FILLER                       PIC X(136).                CL*34
       01  filler pic x(500) value spaces.
00230                                                                   EL1272
00231      EJECT                                                        EL1272
00232                                      COPY EL127S.                    CL*15
00233                                                                   EL1272
00234  01  FILLER REDEFINES EL127BI.                                       CL*15
101501     05  FILLER                      PIC X(44).                   EL1272
00236                                                                   EL1272
00237      05  EL127B-MAP-LINE             OCCURS 8 TIMES               EL1272
00238          INDEXED BY EL127B-INDEX                                  EL1272
00239                     EL127B-INDEX2.                                EL1272
00240                                                                   EL1272
00241          10  EL127B-AST-LENGTH       PIC S9(4)    COMP.           EL1272
00242          10  EL127B-AST-ATTRB        PIC X.                       EL1272
00243          10  EL127B-AST              PIC X.                       EL1272
00244                                                                      CL**3
00245          10  EL127B-CERT-SEL-LENGTH  PIC S9(4)    COMP.              CL**3
00246          10  EL127B-CERT-SEL-ATTRB   PIC X.                          CL**3
00247          10  EL127B-CERT-SEL         PIC X.                          CL**3
00248                                                                   EL1272
00249          10  EL127B-NAME-LENGTH      PIC S9(4)    COMP.           EL1272
00250          10  EL127B-NAME-ATTRB       PIC X.                       EL1272
00251          10  EL127B-NAME-O           PIC X(18).                      CL*21
00252                                                                   EL1272
00253          10  EL127B-AGE-LENGTH       PIC S9(4)    COMP.           EL1272
00254          10  EL127B-AGE-ATTRB        PIC X.                       EL1272
00255          10  EL127B-AGE              PIC 99.                      EL1272
00256                                                                   EL1272
00257          10  EL127B-SEX-LENGTH       PIC S9(4)    COMP.           EL1272
00258          10  EL127B-SEX-ATTRB        PIC X.                       EL1272
00259          10  EL127B-SEX              PIC X.                       EL1272
00260                                                                   EL1272
00261          10  EL127B-CARRIER-LENGTH   PIC S9(4)    COMP.           EL1272
00262          10  EL127B-CARRIER-ATTRB    PIC X.                       EL1272
00263          10  EL127B-CARRIER          PIC X.                       EL1272
00264                                                                   EL1272
00265          10  EL127B-GROUP-LENGTH     PIC S9(4)    COMP.           EL1272
00266          10  EL127B-GROUP-ATTRB      PIC X.                       EL1272
00267          10  EL127B-GROUP            PIC X(6).                    EL1272
00268                                                                   EL1272
00269          10  EL127B-STATE-LENGTH     PIC S9(4)    COMP.           EL1272
00270          10  EL127B-STATE-ATTRB      PIC X.                       EL1272
00271          10  EL127B-STATE            PIC XX.                      EL1272
00272                                                                   EL1272
00273          10  EL127B-ACCOUNT-LENGTH   PIC S9(4)    COMP.           EL1272
00274          10  EL127B-ACCOUNT-ATTRB    PIC X.                       EL1272
00275          10  EL127B-ACCOUNT          PIC X(10).                   EL1272
00276                                                                   EL1272
00277          10  EL127B-CERT-NO-LENGTH   PIC S9(4)    COMP.           EL1272
00278          10  EL127B-CERT-NO-ATTRB    PIC X.                       EL1272
00279          10  EL127B-CERT-NO          PIC X(11).                   EL1272
00280                                                                   EL1272
00281          10  EL127B-EFF-DATE-LENGTH  PIC S9(4)    COMP.           EL1272
00282          10  EL127B-EFF-DATE-ATTRB   PIC X.                       EL1272
00283          10  EL127B-EFF-DATE         PIC X(8).                    EL1272
00284                                                                   EL1272
00285          10 EL127B-LIFE-INFO-LENGTH PIC S9(4) COMP.                  CL**4
00286          10 EL127B-LIFE-INFO-ATTRB PIC X.                            CL**4
00287          10 EL127B-LIFE-INFO.                                        CL**4
00288              15 EL127B-MEMB-LOAN     PIC X(21).                      CL**4
00289              15 EL127B-LI-ABVR       PIC X(4).                       CL**4
00290              15 FILLER               PIC X.                          CL**4
00291              15 EL127B-LI-DESC2      PIC X(3).                       CL**4
00292              15 FILLER               PIC X.                          CL**4
00293              15 EL127B-LI-AMT        PIC ZZZ,ZZZ,ZZ9.99-.            CL**4
00294              15 EL127B-LI-DATE       REDEFINES                       CL**4
00295                  EL127B-LI-AMT       PIC X(15).                   EL1272
00296                                                                   EL1272
00297          10 EL127B-AH-INFO-LENGTH PIC S9(4)       COMP.              CL**4
00298          10 EL127B-AH-INFO-ATTRB PIC X.                              CL**4
00299          10 EL127B-AH-INFO.                                          CL**4
00300              15 EL127B-AH-ABVR       PIC X(4).                       CL**4
00301              15 FILLER               PIC X.                          CL**4
00302              15 EL127B-AH-DESC2      PIC X(3).                       CL**4
00303              15 FILLER               PIC X.                          CL**4
00304              15 EL127B-AH-AMT        PIC ZZZZ,ZZ9.99-.               CL**4
00305              15 EL127B-AH-DATE       REDEFINES                       CL**4
00306                  EL127B-AH-AMT       PIC X(12).                   EL1272
00307              15 FILLER               PIC X.                          CL**4
00308                                                                      CL**4
00309                                                                   EL1272
00310      EJECT                                                        EL1272
00311                                      COPY ELCCALC.                   CL*15
00312                                                                   EL1272
00313      EJECT                                                        EL1272
00314                                      COPY ELCDATE.                   CL*15
00315                                                                   EL1272
00316      EJECT                                                           CL*15
00317                                      COPY ELCEMIB.                   CL*15
00318                                                                   EL1272
00319      EJECT                                                           CL*15
00320                                      COPY ELCLOGOF.                  CL*15
00321                                                                      CL*15
00322      EJECT                                                           CL*15
00323                                      COPY ELCATTR.                   CL*15
00324                                                                      CL*15
00325      EJECT                                                           CL*15
00326                                      COPY ELCAID.                    CL*15
00327                                                                      CL*15
00328  01  FILLER REDEFINES DFHAID.                                        CL*15
00329      05  FILLER                      PIC X(8).                    EL1272
00330                                                                   EL1272
00331      05  PF-VALUES                   PIC X                        EL1272
00332          OCCURS 24 TIMES.                                         EL1272
00333                                                                   EL1272
00334  LINKAGE SECTION.                                                 EL1272
00335                                                                   EL1272
00336  01  DFHCOMMAREA                     PIC X(1024).                 EL1272
00337                                                                   EL1272
00338 *01 PARMLIST   COMP SYNC.                                            CL*32
00339 *    05  FILLER                      PIC S9(9).                      CL*32
00340 *    05  ELCERT-POINTER              PIC S9(9).                      CL*32
00341 *    05  ELCNTL-POINTER              PIC S9(9).                      CL*32
00342                                                                   EL1272
00343      EJECT                                                        EL1272
00344                                      COPY ELCCERT.                   CL*15
00345      EJECT                                                        EL1272
00346                                      COPY ELCCNTL.                   CL*15
00347      EJECT                                                        EL1272
00348  PROCEDURE DIVISION.                                              EL1272
00349                                                                   EL1272
00350      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1272
00351      MOVE '5'                    TO DC-OPTION-CODE.               EL1272
00352      PERFORM 8500-DATE-CONVERSION.                                EL1272
00353      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL1272
00354      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL1272
00355                                                                   EL1272
00356      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL1272
00357                                                                   EL1272
00358      MOVE +2                     TO  EMI-NUMBER-OF-LINES             CL*25
00359                                      EMI-SWITCH2.                    CL*25
00360                                                                      CL*25
00361 *    NOTE ******************************************************* EL1272
00362 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL1272
00363 *         *  FROM ANOTHER MODULE.                               * EL1272
00364 *         *******************************************************.EL1272
00365                                                                   EL1272
00366      IF EIBCALEN NOT > ZERO                                          CL*36
00367          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL1272
00368          GO TO 8300-SEND-TEXT.                                       CL*17
00369                                                                   EL1272
00370      EXEC CICS HANDLE CONDITION                                   EL1272
00371          PGMIDERR (9600-PGMIDERR)                                 EL1272
00372          NOTFND   (8700-NOT-FOUND)                                EL1272
00373          ENDFILE  (4700-END-OF-BROWSE)                               CL*25
00374          DUPKEY   (4015-DUPKEY)                                   EL1272
00375          ITEMERR  (9400-CLEAR)                                       CL*26
00376          ERROR    (9990-ERROR)                                    EL1272
00377      END-EXEC.                                                    EL1272
00378                                                                   EL1272
00379      EJECT                                                        EL1272
           if pi-calling-program = 'EL1503'
              move +1                  to pi-ts-item
              MOVE +2                  TO PI-1ST-TIME-SW
              move dfhpf1              to eibaid
           END-IF
00380      IF PI-CALLING-PROGRAM = 'EL1273'
00381          MOVE EIBTRMID           TO  WS-TS1-TERM-ID                  CL*22
00382          EXEC CICS READQ TS                                          CL*22
00383              QUEUE  (WS-EL1273-TS)                                   CL*22
00384              INTO   (PI-PROGRAM-WORK-AREA)                           CL*22
00385              LENGTH (WS-WORK-LENGTH)                                 CL*22
00386          END-EXEC                                                    CL*22
00387          EXEC CICS DELETEQ TS                                        CL*22
00388              QUEUE  (WS-EL1273-TS)                                   CL*22
00389          END-EXEC                                                    CL*26
00390          MOVE +2                 TO  PI-1ST-TIME-SW.                 CL*26
00391                                                                      CL*22
00392      IF PI-CALLING-PROGRAM = 'EL127'                                 CL*22
00393          MOVE ZERO               TO PI-SCREEN-COUNT                  CL*36
00394          MOVE PI-CERTIFICATE-KEY TO PI-1ST-KEY                       CL*36
00395          MOVE PI-KEY-LENGTH      TO PI-SAVE-KEY-LENGTH.              CL*36
00396                                                                      CL*22
00397      IF PI-CALLING-PROGRAM = 'EL1272'                                CL*23
00398          GO TO 0100-CONTINUE-PROCESSING                              CL*25
00399      ELSE                                                            CL*22
00400          MOVE ZERO               TO PI-ALT-NAME-COUNT.               CL*25
00401                                                                   EL1272
00402      IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                       EL1272
00403          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6         EL1272
00404          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5         EL1272
00405          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4         EL1272
00406          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3         EL1272
00407          MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2         EL1272
00408          MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1         EL1272
00409          MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM       EL1272
00410          MOVE THIS-PGM             TO  PI-CALLING-PROGRAM         EL1272
00411        ELSE                                                       EL1272
00412          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM         EL1272
00413          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM       EL1272
00414          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1         EL1272
00415          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2         EL1272
00416          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3         EL1272
00417          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4         EL1272
00418          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5         EL1272
00419          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.        EL1272
00420                                                                   EL1272
00421      MOVE +0                     TO PI-SUB.                          CL**3
00422      MOVE LOW-VALUES             TO PI-CERT-CONTROLS-EL127 (1)       CL**3
00423                                     PI-CERT-CONTROLS-EL127 (2)       CL**3
00424                                     PI-CERT-CONTROLS-EL127 (3)       CL**3
00425                                     PI-CERT-CONTROLS-EL127 (4)       CL**3
00426                                     PI-CERT-CONTROLS-EL127 (5).      CL**3
00427                                                                   EL1272
00428      IF PI-1ST-TIME-SW = +2                                       EL1272
00429          MOVE EIBTRMID           TO  WS-TSK-TERM-ID               EL1272
00430          EXEC CICS READQ TS                                       EL1272
00431              QUEUE  (WS-TEMP-STORAGE-KEY)                         EL1272
00432              ITEM   (PI-TS-ITEM)                                  EL1272
00433              INTO   (EL127BI)                                     EL1272
00434              LENGTH (WS-TS-LENGTH)                                EL1272
00435          END-EXEC                                                    CL**6
               add +1 to pi-ts-item
00430          EXEC CICS READQ TS                                       EL1272
00431              QUEUE  (WS-TEMP-STORAGE-KEY)                         EL1272
00432              ITEM   (PI-TS-ITEM)                                  EL1272
00433              INTO   (PI-PROGRAM-WORK-AREA)
00434              LENGTH (Ws-work-length)                              EL1272
                   resp   (ws-response)
00435          END-EXEC                                                    CL**6
00436          EXEC CICS DELETEQ TS                                     EL1272
00437              QUEUE  (WS-TEMP-STORAGE-KEY)                         EL1272
00438          END-EXEC                                                    CL**6
00439          MOVE ZERO               TO  PI-1ST-TIME-SW               EL1272
00440          MOVE LOW-VALUES         TO  BSELO                        EL1272
00441                                      BPFKO                        EL1272
00442          PERFORM 6000-SET-ATTRB                                   EL1272
00443              VARYING EL127B-INDEX FROM PI-LINE-COUNT BY -1        EL1272
00444                UNTIL EL127B-INDEX NOT > ZERO                         CL*36
00445          GO TO 8100-SEND-INITIAL-MAP.                                CL*17
00446                                                                   EL1272
00447      PERFORM 4000-BROWSE-CERT-FILE.                               EL1272
00448                                                                   EL1272
00449      EJECT                                                        EL1272
00450  0100-CONTINUE-PROCESSING.                                           CL*25
00451 *    NOTE ******************************************************* EL1272
00452 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL1272
00453 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL1272
00454 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL1272
00455 *         *******************************************************.EL1272
00456                                                                   EL1272
00457      IF EIBAID = DFHCLEAR                                         EL1272
00458          GO TO 9400-CLEAR.                                           CL*17
00459                                                                   EL1272
00460      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL1272
00461          MOVE LOW-VALUES         TO  EL127BO                      EL1272
00462          MOVE -1                 TO  BPFKL                        EL1272
00463          MOVE ER-0008            TO  EMI-ERROR                    EL1272
00464          GO TO 8200-SEND-DATAONLY.                                   CL*17
00465                                                                   EL1272
00466      EXEC CICS RECEIVE                                            EL1272
00467          INTO   (EL127BI)                                         EL1272
00468          MAPSET (WS-MAPSET-NAME)                                  EL1272
00469          MAP    (WS-MAP-NAME)                                     EL1272
00470      END-EXEC.                                                    EL1272
00471                                                                   EL1272
00472      IF BPFKL > ZERO                                                 CL*36
00473          IF EIBAID NOT = DFHENTER                                 EL1272
00474              MOVE ER-0004        TO  EMI-ERROR                    EL1272
00475              MOVE AL-UNBOF       TO  BPFKA                        EL1272
00476              MOVE -1             TO  BPFKL                        EL1272
00477              GO TO 8200-SEND-DATAONLY                                CL*17
00478          ELSE                                                        CL**3
00479              IF (BPFKI NUMERIC) AND                                  CL**3
00480                  (BPFKI > ZERO AND < 25)                             CL*36
00481                      MOVE PF-VALUES (BPFKI)  TO  EIBAID              CL**3
00482                  ELSE                                                CL**3
00483                      MOVE ER-0029        TO  EMI-ERROR               CL**3
00484                      MOVE AL-UNBOF       TO  BPFKA                   CL**3
00485                      MOVE -1             TO  BPFKL                   CL**3
00486                      GO TO 8200-SEND-DATAONLY.                       CL*17
00487                                                                   EL1272
00488      EJECT                                                           CL*25
00489 *    NOTE ******************************************************* EL1272
00490 *         *      PF KEY USAGE:                                  *    CL*25
00491 *         *        PF1       SEARCH FORWARD                     * EL1272
00492 *         *        PF2       SEARCH BACKWARD                    * EL1272
00493 *         *        PF3       CANCELLATION QUOTE                 *    CL**8
00494 *         *        PF4       NEW CLAIM SETUP                    * EL1272
00495 *         *        PF5       CLAIM LOOK-UP                      * EL1272
00496 *         *        PF6       NEXT COMPANY                       * EL1272
00497 *         *        PF7       ORIGINAL COMPANY                   * EL1272
      *         *        PF8       CERT CLAIM HISTORY                 *
00498 *      (removed)   PF9       REVOLVING CREDIT INTERFACE (CRI)   *    CL**3
00499 *         *        PF12      HELP                               * EL1272
00500 *         *        PF23      LOGOFF                             * EL1272
00501 *         *        PF24      RETURN TO MASTER MENU              * EL1272
00502 *         *******************************************************.EL1272
00503                                                                   EL1272
00504      IF EIBAID = DFHPF12                                          EL1272
00505          MOVE 'EL010'         TO  THIS-PGM                        EL1272
00506          GO TO 9300-XCTL.                                         EL1272
00507                                                                   EL1272
00508      IF EIBAID = DFHPF23                                          EL1272
00509          GO TO 9000-RETURN-CICS.                                     CL*17
00510                                                                   EL1272
00511      IF EIBAID = DFHPF24                                          EL1272
00512          MOVE 'EL126'            TO  THIS-PGM                        CL**6
00513          GO TO 9300-XCTL.                                         EL1272
00514                                                                      CL**6
00515      IF BSELL > ZERO                                                 CL*36
00516          IF BSELI NOT NUMERIC                                        CL*36
00517              MOVE -1             TO  BSELL                           CL**9
00518              MOVE ER-0200        TO  EMI-ERROR                       CL**9
00519              GO TO 8200-SEND-DATAONLY.                               CL*17
00520                                                                      CL**9
00521      EJECT                                                           CL*25
00522      IF CLAIM-SESSION                                                CL**9
00523        IF EIBAID = DFHPF4                                            CL**9
00524          IF BSELL > ZERO  AND                                        CL*36
00525             BSELI NOT > PI-LINE-COUNT                                CL*36
00526              NEXT SENTENCE                                           CL*30
00527            ELSE                                                      CL*30
00528          IF BCRTSL1L > 0 OR                                          CL*36
00529             BCRTSL2L > 0 OR                                          CL*36
00530             BCRTSL3L > 0 OR                                          CL*36
00531             BCRTSL4L > 0 OR                                          CL*36
00532             BCRTSL5L > 0 OR                                          CL*36
00533             BCRTSL6L > 0 OR                                          CL*36
00534             BCRTSL7L > 0 OR                                          CL*36
00535             BCRTSL8L > 0                                             CL*36
00536              NEXT SENTENCE                                           CL*10
00537            ELSE                                                      CL*10
00538              MOVE -1             TO  BSELL                           CL**6
00539              MOVE ER-0200        TO  EMI-ERROR                       CL**6
00540              GO TO 8200-SEND-DATAONLY.                               CL*17
00541                                                                   EL1272
00542      IF CLAIM-SESSION                                                CL**5
00543        IF EIBAID = DFHPF4 OR                                         CL**5
00544          PI-SAVED-PROGRAM-1 = 'EL130' OR                             CL*26
00545            BCRTSL1L > 0 OR                                           CL*36
00546            BCRTSL2L > 0 OR                                           CL*36
00547            BCRTSL3L > 0 OR                                           CL*36
00548            BCRTSL4L > 0 OR                                           CL*36
00549            BCRTSL5L > 0 OR                                           CL*36
00550            BCRTSL6L > 0 OR                                           CL*36
00551            BCRTSL7L > 0 OR                                           CL*36
00552            BCRTSL8L > 0                                              CL*36
00553               GO TO 0115-BUILD-CERT-KEYS.                            CL*36
00554                                                                      CL**3
00555      IF EIBAID = DFHPF3 OR DFHPF4 OR DFHPF5                          CL*25
00556          OR BSELL > ZERO                                             CL*36
00557          NEXT SENTENCE                                            EL1272
00558      ELSE                                                         EL1272
00559          GO TO 0120-MAIN-LOGIC.                                   EL1272
00560                                                                   EL1272
00561      IF (CREDIT-SESSION  AND                                      EL1272
00562          EIBAID = (DFHPF4 OR DFHPF5))                             EL1272
00563        OR                                                         EL1272
00564          (PI-MODIFY-CAP = 'C' AND                                 EL1272
00565           EIBAID = (DFHPF4 OR DFHPF5))                            EL1272
00566              MOVE ER-0008        TO  EMI-ERROR                    EL1272
00567              MOVE -1             TO  BPFKL                        EL1272
00568              MOVE AL-UNBOF       TO  BPFKA                        EL1272
00569              GO TO 8200-SEND-DATAONLY.                               CL*17
00570                                                                   EL1272
00571      IF BSELL > ZERO        AND                                      CL*36
00572         BSELO > ZERO        AND                                      CL*36
00573         BSELO < '9'         AND                                      CL*36
00574         BSELI NOT > PI-LINE-COUNT                                    CL*36
00575          NEXT SENTENCE                                            EL1272
00576        ELSE                                                       EL1272
00577          MOVE -1                 TO  BSELL                        EL1272
00578          MOVE ER-0200            TO  EMI-ERROR                    EL1272
00579          GO TO 8200-SEND-DATAONLY.                                   CL*17
00580                                                                   EL1272
00581      IF EIBAID = DFHPF3                                           EL1272
00582        OR (BSELL > ZERO AND                                          CL*36
00583            PI-SAVED-PROGRAM-1 NOT = 'EL130')                         CL*26
00584             MOVE EIBTRMID       TO  WS-TSK-TERM-ID                   CL*33
00585             PERFORM 8620-WRITE-TEMP-STORAGE THRU 8630-EXIT.          CL*33
00586                                                                   EL1272
00587      IF CREDIT-SESSION                                               CL**3
00588          NEXT SENTENCE                                               CL**3
00589      ELSE                                                            CL**3
00590          IF (PI-SAVED-PROGRAM-1 NOT = 'EL130') AND                   CL*26
00591              (EIBAID = DFHENTER OR DFHPF3 OR DFHPF5                  CL**3
00592                                           OR DFHPF8)                 CL**3
00593                  NEXT SENTENCE                                       CL**3
00594              ELSE                                                    CL**3
00595                  GO TO 8200-SEND-DATAONLY.                           CL*17
00596                                                                      CL**3
00597  EJECT                                                               CL**3
00598      SET EL127B-INDEX TO BSELI.                                   EL1272
00599                                                                   EL1272
00600      IF EIBAID = DFHPF5                                           EL1272
00601         IF EL127B-AST (EL127B-INDEX) NOT = '*'                       CL*36
00602          MOVE -1                 TO  BSELL                        EL1272
00603          MOVE ER-0363            TO  EMI-ERROR                    EL1272
00604          GO TO 8200-SEND-DATAONLY.                                   CL*17
00605                                                                   EL1272
00606      MOVE EL127B-CARRIER (EL127B-INDEX)  TO  PI-CARRIER.          EL1272
00607      MOVE EL127B-GROUP   (EL127B-INDEX)  TO  PI-GROUPING.         EL1272
00608      MOVE EL127B-STATE   (EL127B-INDEX)  TO  PI-STATE.            EL1272
00609      MOVE EL127B-ACCOUNT (EL127B-INDEX)  TO  PI-ACCOUNT.          EL1272
00610      MOVE EL127B-CERT-NO (EL127B-INDEX)  TO  PI-CERT-NO.          EL1272
00611                                                                   EL1272
00612      MOVE EL127B-EFF-DATE (EL127B-INDEX)  TO  DC-GREG-DATE-1-EDIT.EL1272
00613      MOVE '2'                    TO  DC-OPTION-CODE.              EL1272
00614      PERFORM 8500-DATE-CONVERSION.                                EL1272
00615      MOVE DC-BIN-DATE-1          TO  PI-CERT-EFF-DT.              EL1272
00616                                                                   EL1272
00617      MOVE +2                     TO  PI-1ST-TIME-SW.              EL1272
00618                                                                   EL1272
00619      MOVE PI-SAVED-PROGRAM-1     TO  THIS-PGM.                    EL1272
00620                                                                   EL1272
00621      IF THIS-PGM  NOT = 'EL130'                                      CL*26
00622          MOVE 'EL1273'           TO  THIS-PGM.                       CL*26
00623                                                                   EL1272
00624      IF EIBAID = DFHPF3                                           EL1272
00625          IF PI-COMPANY-ID = 'AIG' OR 'AUK'                           CL*36
00626              MOVE ER-0029        TO  EMI-ERROR                       CL*20
00627              MOVE -1             TO  BPFKL                           CL*20
00628              GO TO 8200-SEND-DATAONLY                                CL*20
00629          ELSE                                                        CL*20
00630              MOVE 'EL1278'       TO  THIS-PGM                        CL*26
00631              GO TO 9300-XCTL.                                        CL*20
00632                                                                   EL1272
00633      IF EIBAID = DFHPF5                                           EL1272
00634          MOVE 'EL132'            TO  THIS-PGM.                       CL*26

           IF EIBAID = DFHPF8
              MOVE 'EL1503'            TO THIS-PGM
              MOVE EIBTRMID            TO WS-TSK-TERM-ID
      *       PERFORM 8620-WRITE-TEMP-STORAGE
      *                                THRU 8630-EXIT
              GO TO 9300-XCTL
           END-IF

00646      IF THIS-PGM NOT = 'EL1273'                                      CL*26
00647          MOVE PI-RETURN-TO-PROGRAM   TO  PI-CALLING-PROGRAM          CL*26
00648          MOVE PI-SAVED-PROGRAM-1     TO  PI-RETURN-TO-PROGRAM     EL1272
00649          MOVE PI-SAVED-PROGRAM-2     TO  PI-SAVED-PROGRAM-1       EL1272
00650          MOVE PI-SAVED-PROGRAM-3     TO  PI-SAVED-PROGRAM-2       EL1272
00651          MOVE PI-SAVED-PROGRAM-4     TO  PI-SAVED-PROGRAM-3       EL1272
00652          MOVE PI-SAVED-PROGRAM-5     TO  PI-SAVED-PROGRAM-4       EL1272
00653          MOVE PI-SAVED-PROGRAM-6     TO  PI-SAVED-PROGRAM-5       EL1272
00654          MOVE SPACES                 TO  PI-SAVED-PROGRAM-6.      EL1272
00655                                                                   EL1272
00656      GO TO 9300-XCTL.                                             EL1272
00657                                                                   EL1272
00658  EJECT                                                               CL**3
00659  0115-BUILD-CERT-KEYS.                                               CL*25
00660                                                                      CL**3
00661      IF (BCRTSL1L > 0) OR                                            CL*36
00662         (EIBAID = DFHPF4 AND BSELL > 0                               CL*36
00663         AND BSELI = 1)                                               CL**3
00664            MOVE +1                 TO  SUB                           CL**3
00665            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT   CL**3
00666            MOVE +0                 TO  SUB                           CL**3
00667            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT      CL**3
00668            IF PREVIOUSLY-SELECTED                                    CL**3
00669                NEXT SENTENCE                                         CL**3
00670            ELSE                                                      CL**3
00671                ADD +1              TO  PI-SUB                        CL*36
00672                IF PI-SUB > +5                                        CL*36
00673                    MOVE ER-0659    TO  EMI-ERROR                     CL**3
00674                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          CL**3
00675                ELSE                                                  CL**3
00676                    MOVE WS-CERT-CONTROL    TO                        CL**3
00677                                   PI-CERT-CONTROLS-EL127 (PI-SUB).   CL**3
00678                                                                      CL**3
00679      IF (BCRTSL2L > 0) OR                                            CL*36
00680         (EIBAID = DFHPF4 AND BSELL > 0                               CL*36
00681         AND BSELI = 2)                                               CL**3
00682            MOVE +2                 TO  SUB                           CL**3
00683            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT   CL**3
00684            MOVE +0                 TO  SUB                           CL**3
00685            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT      CL**3
00686            ADD +1                  TO  PI-SUB                        CL*36
00687            IF PREVIOUSLY-SELECTED                                    CL**3
00688                NEXT SENTENCE                                         CL**3
00689            ELSE                                                      CL**3
00690                IF PI-SUB > +5                                        CL*36
00691                    MOVE ER-0659    TO  EMI-ERROR                     CL**3
00692                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          CL**3
00693                ELSE                                                  CL**3
00694                    MOVE WS-CERT-CONTROL    TO                        CL**3
00695                                   PI-CERT-CONTROLS-EL127 (PI-SUB).   CL**3
00696                                                                      CL**3
00697      IF (BCRTSL3L > 0) OR                                            CL*36
00698         (EIBAID = DFHPF4 AND BSELL > 0                               CL*36
00699         AND BSELI = 3)                                               CL**3
00700            MOVE +3                 TO  SUB                           CL**3
00701            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT   CL**3
00702            MOVE +0                 TO  SUB                           CL**3
00703            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT      CL**3
00704            ADD +1                  TO  PI-SUB                        CL**3
00705            IF PREVIOUSLY-SELECTED                                    CL**3
00706                NEXT SENTENCE                                         CL**3
00707            ELSE                                                      CL**3
00708                IF PI-SUB > +5                                        CL*36
00709                    MOVE ER-0659    TO  EMI-ERROR                     CL**3
00710                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          CL**3
00711                ELSE                                                  CL**3
00712                    MOVE WS-CERT-CONTROL    TO                        CL**3
00713                                   PI-CERT-CONTROLS-EL127 (PI-SUB).   CL**3
00714                                                                      CL**3
00715      IF (BCRTSL4L > 0) OR                                            CL*36
00716         (EIBAID = DFHPF4 AND BSELL > 0                               CL*36
00717         AND BSELI = 4)                                               CL**3
00718            MOVE +4                 TO  SUB                           CL**3
00719            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT   CL**3
00720            MOVE +0                 TO  SUB                           CL**3
00721            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT      CL**3
00722            ADD +1                  TO  PI-SUB                        CL**3
00723            IF PREVIOUSLY-SELECTED                                    CL**3
00724                NEXT SENTENCE                                         CL**3
00725            ELSE                                                      CL**3
00726                IF PI-SUB > +5                                        CL*36
00727                    MOVE ER-0659    TO  EMI-ERROR                     CL**3
00728                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          CL**3
00729                ELSE                                                  CL**3
00730                    MOVE WS-CERT-CONTROL    TO                        CL**3
00731                                   PI-CERT-CONTROLS-EL127 (PI-SUB).   CL**3
00732                                                                      CL**3
00733      IF (BCRTSL5L > 0) OR                                            CL*36
00734         (EIBAID = DFHPF4 AND BSELL > 0                               CL*36
00735         AND BSELI = 5)                                               CL**3
00736            MOVE +5                 TO  SUB                           CL**3
00737            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT   CL**3
00738            MOVE +0                 TO  SUB                           CL**3
00739            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT      CL**3
00740            ADD +1                  TO  PI-SUB                        CL**3
00741            IF PREVIOUSLY-SELECTED                                    CL**3
00742                NEXT SENTENCE                                         CL**3
00743            ELSE                                                      CL**3
00744                IF PI-SUB > +5                                        CL*36
00745                    MOVE ER-0659    TO  EMI-ERROR                     CL**3
00746                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          CL**3
00747                ELSE                                                  CL**3
00748                    MOVE WS-CERT-CONTROL    TO                        CL**3
00749                                   PI-CERT-CONTROLS-EL127 (PI-SUB).   CL**3
00750                                                                      CL**3
00751      IF (BCRTSL6L > 0) OR                                            CL*36
00752         (EIBAID = DFHPF4 AND BSELL > 0                               CL*36
00753         AND BSELI = 6)                                               CL**3
00754            MOVE +6                 TO  SUB                           CL**3
00755            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT   CL**3
00756            MOVE +0                 TO  SUB                           CL**3
00757            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT      CL**3
00758            ADD +1                  TO  PI-SUB                        CL**3
00759            IF PREVIOUSLY-SELECTED                                    CL**3
00760                NEXT SENTENCE                                         CL**3
00761            ELSE                                                      CL**3
00762                IF PI-SUB > +5                                        CL*36
00763                    MOVE ER-0659    TO  EMI-ERROR                     CL**3
00764                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          CL**3
00765                ELSE                                                  CL**3
00766                    MOVE WS-CERT-CONTROL    TO                        CL**3
00767                                   PI-CERT-CONTROLS-EL127 (PI-SUB).   CL**3
00768                                                                      CL**3
00769      IF (BCRTSL7L > 0) OR                                            CL*36
00770         (EIBAID = DFHPF4 AND BSELL > 0                               CL*36
00771         AND BSELI = 7)                                               CL**3
00772            MOVE +7                 TO  SUB                           CL**3
00773            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT   CL**3
00774            MOVE +0                 TO  SUB                           CL**3
00775            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT      CL**3
00776            ADD +1                  TO  PI-SUB                        CL**3
00777            IF PREVIOUSLY-SELECTED                                    CL**3
00778                NEXT SENTENCE                                         CL**3
00779            ELSE                                                      CL**3
00780                IF PI-SUB > +5                                        CL*36
00781                    MOVE ER-0659    TO  EMI-ERROR                     CL**3
00782                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          CL**3
00783                ELSE                                                  CL**3
00784                    MOVE WS-CERT-CONTROL    TO                        CL**3
00785                                   PI-CERT-CONTROLS-EL127 (PI-SUB).   CL**3
00786                                                                      CL**3
00787      IF (BCRTSL8L > 0) OR                                            CL*36
00788         (EIBAID = DFHPF4 AND BSELL > 0                               CL*36
00789         AND BSELI = 8)                                               CL**3
00790            MOVE +8                 TO  SUB                           CL**3
00791            PERFORM 0116-BUILD-EL127-CERT-KEYS THRU 0116-BUILD-EXIT   CL**3
00792            MOVE +0                 TO  SUB                           CL**3
00793            PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT      CL**3
00794            ADD +1                  TO  PI-SUB                        CL**3
00795            IF PREVIOUSLY-SELECTED                                    CL**3
00796                NEXT SENTENCE                                         CL**3
00797            ELSE                                                      CL**3
00798                IF PI-SUB > +5                                        CL*36
00799                    MOVE ER-0659    TO  EMI-ERROR                     CL**3
00800                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          CL**3
00801                ELSE                                                  CL**3
00802                    MOVE WS-CERT-CONTROL    TO                        CL**3
00803                                   PI-CERT-CONTROLS-EL127 (PI-SUB).   CL**3
00804                                                                      CL**3
00805      IF EIBAID = DFHPF4                                              CL**3
00806          MOVE PI-SUB                 TO  PI-CERT-SELECT-CNT          CL*26
00807          MOVE +1                     TO  PI-CERT-PROCESSED-CNT       CL*26
00808          MOVE PI-EL127-CARRIER (1)   TO  PI-CARRIER                  CL**3
00809          MOVE PI-EL127-GROUPING (1)  TO  PI-GROUPING                 CL**3
00810          MOVE PI-EL127-STATE (1)     TO  PI-STATE                    CL**3
00811          MOVE PI-EL127-ACCOUNT (1)   TO  PI-ACCOUNT                  CL**3
00812          MOVE PI-EL127-CERT-NO (1)   TO  PI-CERT-NO                  CL**3
00813          MOVE PI-EL127-EFF-DT (1)    TO  PI-CERT-EFF-DT              CL**3
00814          MOVE 'EL130'                TO  THIS-PGM                    CL*26
00815          MOVE PI-RETURN-TO-PROGRAM   TO  PI-CALLING-PROGRAM          CL**3
00816          MOVE PI-SAVED-PROGRAM-1     TO  PI-RETURN-TO-PROGRAM        CL**3
00817          MOVE PI-SAVED-PROGRAM-2     TO  PI-SAVED-PROGRAM-1          CL**3
00818          MOVE PI-SAVED-PROGRAM-3     TO  PI-SAVED-PROGRAM-2          CL**3
00819          MOVE PI-SAVED-PROGRAM-4     TO  PI-SAVED-PROGRAM-3          CL**3
00820          MOVE PI-SAVED-PROGRAM-5     TO  PI-SAVED-PROGRAM-4          CL**3
00821          MOVE PI-SAVED-PROGRAM-6     TO  PI-SAVED-PROGRAM-5          CL**3
00822          MOVE SPACES                 TO  PI-SAVED-PROGRAM-6          CL**3
00823          GO TO 9300-XCTL.                                            CL**3
00824                                                                      CL**3
00825      GO TO 0120-MAIN-LOGIC.                                          CL**3
00826                                                                      CL**3
00827  0115-CK-FOR-DUP-SELECTION.                                          CL**3
00828                                                                      CL**3
00829      ADD +1                      TO  SUB.                            CL**3
00830      IF SUB > 5                                                      CL*36
00831          MOVE 'N'                TO  WS-SELECTED-SW                  CL**3
00832          GO TO 0115-DUP-EXIT.                                        CL**3
00833                                                                      CL**3
00834      IF WS-CERT-CONTROL = PI-CERT-CONTROLS-EL127 (SUB)               CL**3
00835          MOVE 'Y'                TO  WS-SELECTED-SW                  CL**3
00836          GO TO 0115-DUP-EXIT.                                        CL**3
00837                                                                      CL**3
00838      GO TO 0115-CK-FOR-DUP-SELECTION.                                CL**3
00839                                                                      CL**3
00840  0115-DUP-EXIT.                                                      CL**3
00841      EXIT.                                                           CL**3
00842                                                                      CL**3
00843  0116-BUILD-EL127-CERT-KEYS.                                         CL**3
00844                                                                      CL**3
00845      MOVE EL127B-CARRIER (SUB)   TO  WS-CARRIER.                     CL**3
00846      MOVE EL127B-GROUP (SUB)     TO  WS-GROUPING.                    CL**3
00847      MOVE EL127B-STATE (SUB)     TO  WS-STATE.                       CL**3
00848      MOVE EL127B-ACCOUNT (SUB)   TO  WS-ACCOUNT.                     CL**3
00849      MOVE EL127B-CERT-NO (SUB)   TO  WS-CERT-NO.                     CL**3
00850      MOVE EL127B-EFF-DATE (SUB)  TO  DC-GREG-DATE-1-EDIT.            CL**3
00851      MOVE '2'                    TO  DC-OPTION-CODE.                 CL**3
00852      PERFORM 8500-DATE-CONVERSION.                                   CL**3
00853      IF NO-CONVERSION-ERROR                                          CL**3
00854          MOVE DC-BIN-DATE-1      TO  WS-EFF-DT                       CL**3
00855      ELSE                                                            CL**3
00856          MOVE LOW-VALUES         TO  WS-EFF-DT.                      CL**3
00857                                                                      CL**3
00858  0116-BUILD-EXIT.                                                    CL**3
00859      EXIT.                                                           CL**3
00860                                                                      CL**3
00861  0120-MAIN-LOGIC.                                                 EL1272
00862      IF EIBAID = (DFHENTER OR DFHPF1 OR DFHPF6 OR DFHPF7)         EL1272
00863        OR                                                         EL1272
00864          ((EIBAID = DFHPF2) AND                                   EL1272
00865           (PI-SCREEN-COUNT > +1))                                    CL*36
00866              NEXT SENTENCE                                        EL1272
00867            ELSE                                                   EL1272
00868              MOVE ER-0131            TO  EMI-ERROR                   CL*20
00869              MOVE -1                 TO  BPFKL                    EL1272
00870              GO TO 8200-SEND-DATAONLY.                               CL*17
00871                                                                   EL1272
00872      IF EIBAID = DFHPF6                                           EL1272
00873         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                    EL1272
00874            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT               EL1272
00875            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT  EL1272
00876         ELSE                                                      EL1272
00877            MOVE ER-0008              TO  EMI-ERROR                   CL*17
00878            MOVE -1                   TO  BSELL                       CL*17
00879            GO TO 8200-SEND-DATAONLY.                                 CL*17
00880                                                                   EL1272
00881      IF EIBAID = DFHPF7                                           EL1272
00882         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                    EL1272
00883            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT               EL1272
00884            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT  EL1272
00885            MOVE PI-ORIGINAL-COMPANY-CD   TO PI-COMPANY-CD         EL1272
00886                                             PI-CK-COMPANY-CD         CL*17
00887         ELSE                                                         CL*17
00888            MOVE ER-0008              TO  EMI-ERROR                   CL*17
00889            MOVE -1                   TO  BSELL                       CL*17
00890            GO TO 8200-SEND-DATAONLY.                                 CL*17
00891                                                                   EL1272
00892      IF PI-END-OF-FILE NOT > ZERO                                    CL*36
00893          PERFORM 4000-BROWSE-CERT-FILE.                           EL1272
00894                                                                      CL**3
00895      IF PI-END-OF-FILE > ZERO                                        CL*36
00896          IF EIBAID = DFHPF2                                          CL**3
00897              NEXT SENTENCE                                           CL**3
00898          ELSE                                                        CL**3
00899              MOVE ER-0130        TO  EMI-ERROR                       CL*25
00900              MOVE -1             TO  BSELL                           CL**3
00901              GO TO 8200-SEND-DATAONLY.                               CL*17
00902                                                                   EL1272
00903      IF EIBAID  = DFHPF2                                          EL1272
00904          NEXT SENTENCE                                            EL1272
00905        ELSE                                                       EL1272
00906          IF PI-DSID NOT = 'ELCERT'                                   CL*26
00907              GO TO 9400-CLEAR.                                       CL*17
00908                                                                   EL1272
00909      IF (PI-PREV-AID = (DFHPF1 OR DFHENTER) AND                   EL1272
00910          EIBAID = DFHPF2)                                         EL1272
00911        OR                                                         EL1272
00912          (PI-PREV-AID = DFHPF2 AND                                EL1272
00913           EIBAID = (DFHPF1 OR DFHENTER))                          EL1272
00914              PERFORM 4000-BROWSE-CERT-FILE                        EL1272
00915            ELSE                                                   EL1272
00916              GO TO 9400-CLEAR.                                       CL*17
00917                                                                   EL1272
00918      EJECT                                                        EL1272
00919  4000-BROWSE-CERT-FILE SECTION.                                   EL1272
00920      MOVE PI-CERTIFICATE-KEY   TO PI-LIN1-CERTIFICATE-KEY.           CL*26
00921                                                                      CL*26
00922      EXEC CICS HANDLE CONDITION                                   EL1272
00923           NOTFND (8700-NOT-FOUND)                                 EL1272
00924           END-EXEC.                                               EL1272
00925                                                                   EL1272
00926      MOVE LOW-VALUES             TO  EL127BO.                     EL1272
00927                                                                   EL1272
00928      IF PI-BROWSE-SW = ZERO                                       EL1272
00929        AND PI-START-SW = +1                                       EL1272
00930          EXEC CICS STARTBR                                        EL1272
00931              DATASET   (PI-DSID)                                  EL1272
00932              RIDFLD    (PI-CERTIFICATE-KEY)                       EL1272
00933              GENERIC                                              EL1272
00934              GTEQ                                                    CL*32
00935              KEYLENGTH (PI-KEY-LENGTH)                            EL1272
00936              END-EXEC                                             EL1272
00937          GO TO 4005-NEXT-SENTENCE.                                EL1272
00938                                                                   EL1272
00939      IF EIBAID = DFHPF1                                              CL**7
00940        AND OPTION-FOUR-SELECTED                                      CL**7
00941        AND PI-1ST-KEY = PI-LAST-KEY                                  CL**7
00942          PERFORM 7000-PF2-POSITION                                   CL**7
00943          GO TO 4005-NEXT-SENTENCE.                                   CL**7
00944                                                                      CL**7
00945      IF EIBAID = DFHPF2                                           EL1272
00946          SUBTRACT 2 FROM PI-SCREEN-COUNT                          EL1272
00947          PERFORM 7000-PF2-POSITION                                EL1272
00948          GO TO 4005-NEXT-SENTENCE.                                EL1272
00949                                                                   EL1272
00950      EXEC CICS STARTBR                                            EL1272
00951          DATASET (PI-DSID)                                        EL1272
00952          RIDFLD  (PI-CERTIFICATE-KEY)                             EL1272
00953          EQUAL                                                    EL1272
00954      END-EXEC.                                                       CL*36
00955                                                                   EL1272
00956  4005-NEXT-SENTENCE.                                              EL1272
00957      MOVE +1                     TO  PI-BROWSE-SW.                EL1272
00958      MOVE ZERO                   TO  PI-LINE-COUNT.               EL1272
00959      MOVE LOW-VALUES             TO  EL127BO.                     EL1272
00960      MOVE PI-CERTIFICATE-KEY     TO  WS-KEY-HOLD.                 EL1272
00961      SET EL127B-INDEX TO +1.                                      EL1272
00962                                                                   EL1272
00963  4010-READNEXT.                                                   EL1272
00964      EXEC CICS READNEXT                                           EL1272
00965          DATASET (PI-DSID)                                        EL1272
00966          RIDFLD  (PI-CERTIFICATE-KEY)                             EL1272
00967          SET     (ADDRESS OF CERTIFICATE-MASTER)                     CL*32
00968      END-EXEC.                                                       CL*36
00969                                                                      CL*22
00970      IF PI-LINE-COUNT NOT = ZERO                                  EL1272
00971          MOVE ZERO               TO  PI-AIX-RECORD-COUNT.         EL1272
00972                                                                   EL1272
00973  4015-DUPKEY.                                                     EL1272
00974      IF LCP-ONCTR-01 =  0                                            CL*32
00975          ADD 1 TO LCP-ONCTR-01                                       CL*32
00976      ELSE                                                         EL1272
00977          GO TO 4016-NEXT-SENTENCE.                                EL1272
00978                                                                   EL1272
00979      IF PI-AIX-RECORD-COUNT > +8                                     CL*36
00980          SUBTRACT +1 FROM PI-AIX-RECORD-COUNT.                    EL1272
00981                                                                   EL1272
00982  4016-NEXT-SENTENCE.                                              EL1272
00983                                                                      CL*22
00984      ADD +1  TO  WS-AIX-RECORD-COUNT.                             EL1272
00985                                                                   EL1272
00986      IF EIBAID = DFHPF1 OR DFHENTER                                  CL**3
00987          IF PI-LINE-COUNT = ZERO                                  EL1272
00988              IF OPTION-THREE-SELECTED                             EL1272
00989                  PERFORM 5000-MOVE-NAME                           EL1272
00990                  IF CM-INSURED-LAST-NAME = PI-END-NAME               CL*21
00991                      IF PI-END-CERTIFICATE-KEY =                  EL1272
00992                              CM-CONTROL-PRIMARY                   EL1272
00993                          NEXT SENTENCE                            EL1272
00994                        ELSE                                       EL1272
00995                          GO TO 4010-READNEXT                      EL1272
00996                    ELSE                                           EL1272
00997                      NEXT SENTENCE                                EL1272
00998                ELSE                                               EL1272
00999                  IF OPTION-ONE-SELECTED                           EL1272
01000                      IF ((CM-COMPANY-CD = PI-END-COMPANY-ID)      EL1272
01001                          AND (CM-CERT-NO = PI-END-CERT-NO))       EL1272
01002                          IF PI-END-CERTIFICATE-KEY =              EL1272
01003                                  CM-CONTROL-PRIMARY               EL1272
01004                              NEXT SENTENCE                        EL1272
01005                            ELSE                                   EL1272
01006                              GO TO 4010-READNEXT.                 EL1272
01007                                                                   EL1272
01008      MOVE PI-CERTIFICATE-KEY     TO  WS-KEY-INPUT.                EL1272
01009                                                                   EL1272
01010      SET KEY-INDEX                                                EL1272
01011          KEY-INDEX2 TO +1.                                        EL1272
01012                                                                   EL1272
01013  4020-COMPARE-KEY.                                                EL1272
01014      IF PI-PART-FIELD-SW  NOT = ' '                                  CL*32
01015         MOVE +1  TO PI-KEY-LENGTH.                                   CL*32
01016                                                                      CL*32
01017      IF WS-KH-CHAR (KEY-INDEX) NOT = WS-KI-CHAR (KEY-INDEX2)      EL1272
01018          GO TO 4700-END-OF-BROWSE.                                EL1272
01019                                                                   EL1272
01020      IF KEY-INDEX < PI-KEY-LENGTH                                    CL*36
01021          SET KEY-INDEX                                            EL1272
01022              KEY-INDEX2 UP BY +1                                  EL1272
01023          GO TO 4020-COMPARE-KEY.                                  EL1272
01024                                                                   EL1272
01025 ******************************************************************EL1272
01026 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *EL1272
01027 *                        04/04/84                                *EL1272
01028 ******************************************************************EL1272
01029                                                                   EL1272
01030 ******************************************************************EL1272
01031 *    IF THE SECURITY CHECK ROUTINE IS CHANGED HERE, YOU MUST     *EL1272
01032 *        ALSO CHANGE THE SECURITY CHECK ROUTINE IN               *EL1272
01033 *        7100-READNEXT-PF2.           KER/080884                 *EL1272
01034 ******************************************************************EL1272
01035                                                                   EL1272
01036      IF PI-NO-CARRIER-SECURITY AND                                   CL*36
01037         PI-NO-ACCOUNT-SECURITY                                       CL*36
01038          GO TO 4030-CHECK-OPTION.                                 EL1272
01039                                                                   EL1272
01040      IF PI-CARRIER-SECURITY > SPACES                                 CL*36
01041          IF CM-CARRIER = PI-CARRIER-SECURITY                         CL*36
01042              NEXT SENTENCE                                           CL*36
01043             ELSE                                                  EL1272
01044              GO TO 4010-READNEXT.                                 EL1272
01045                                                                   EL1272
01046      IF PI-ACCOUNT-SECURITY > SPACES                                 CL*36
01047          IF CM-ACCOUNT = PI-ACCOUNT-SECURITY                         CL*36
01048              NEXT SENTENCE                                           CL*36
01049             ELSE                                                  EL1272
01050              GO TO 4010-READNEXT.                                 EL1272
01051                                                                   EL1272
01052  4030-CHECK-OPTION.                                               EL1272
01053      IF NOT OPTION-THREE-SELECTED
01054          GO TO 4090-MOVE-DATA.                                    EL1272
01055                                                                      CL*25
01056      IF (CM-INSURED-LAST-NAME NOT = PI-SC-LAST-NAME) OR              CL*36
01057         (CM-INSURED-INITIALS  NOT = PI-PREV-INITIALS)                CL*36
01058          MOVE +1         TO PI-ALT-NAME-COUNT.                       CL*25
01059                                                                      CL*25
01060      MOVE CM-INSURED-INITIALS    TO PI-PREV-INITIALS.                CL*25
01061                                                                      CL*25
01062 ******************************************************************   CL*25
01063 *   IF READING ELCERT (ALT BY NAME), ADD TO PI-ALT-NAME-COUNT.   *   CL*25
01064 *   ON EITHER OF TWO CONDITIONS: 1. END OF SEARCH.....(EL1272)   *   CL*25
01065 *                                2. NOT FOUND.........(EL127)    *   CL*25
01066 *  ......IF COUNT WAS OVER 140, DISPLAY MESSAGE ER-0765,         *   CL*25
01067 *           DIRECTING USER TO ECS052 FOR FULL LIST.              *   CL*25
01068 ******************************************************************   CL*25
01069                                                                      CL*25
01070      IF EIBAID = DFHPF2                                              CL*25
01071           SUBTRACT +1 FROM PI-ALT-NAME-COUNT                         CL*25
01072         ELSE                                                         CL*25
01073           ADD +1        TO PI-ALT-NAME-COUNT.                        CL*25
01074                                                                   EL1272
01075 ******************************************************************EL1272
01076 *        IF THE INITIAL CHECKING ROUTINE OR THE ACCOUNT CHECKING *EL1272
01077 *    ROUTINE IS CHANGED, YOU MUST ALSO CHANGE THE CORRESPONDING  *EL1272
01078 *    ROUTINE IN 7130-CHECK-INITIAL.        KER/080884            *   CL*25
01079 ******************************************************************EL1272
01080                                                                   EL1272
01081      IF PI-SC-INITIALS NOT = SPACES                               EL1272
01082         MOVE PI-SC-INITIALS      TO WS-INITIALS                   EL1272
01083         IF WS-INIT2 NOT = SPACE                                   EL1272
01084            IF PI-SC-INITIALS NOT = CM-INSURED-INITIALS            EL1272
01085               GO TO 4010-READNEXT                                 EL1272
01086              ELSE                                                    CL*36
01087               NEXT SENTENCE                                       EL1272
01088            ELSE                                                   EL1272
01089            IF WS-INIT1 NOT = CM-INSURED-INITIAL1                  EL1272
01090               GO TO 4010-READNEXT.                                EL1272
01091                                                                      CL*22
01092      IF PI-SC-FIRST-NAME = SPACES                                    CL*25
01093          GO TO 4040-CONTINUE.                                        CL*25
01094                                                                   EL1272
01095      MOVE PI-SC-FIRST-NAME       TO WS-PI-NAME.                      CL*25
01096      MOVE CM-INSURED-FIRST-NAME  TO WS-CM-NAME.                      CL*25
01097                                                                      CL*25
01098      MOVE SPACE                  TO WS-COMPARE-INDICATOR.            CL*25
01099      PERFORM 4035-CHECK-NAME THRU 4035-EXIT                          CL*25
01100          VARYING WS-NAME-INDEX FROM 15 BY -1                         CL*25
01101            UNTIL WS-NAME-INDEX = ZERO.                               CL*36
01102                                                                      CL*25
01103      IF NAME-NOT-FOUND                                               CL*25
01104          GO TO 4010-READNEXT                                         CL*25
01105        ELSE                                                          CL*25
01106          GO TO 4040-CONTINUE.                                        CL*25
01107                                                                      CL*25
01108  4035-CHECK-NAME.                                                    CL*25
01109      IF WS-PI-NAME-CHAR (WS-NAME-INDEX) NOT = ' ' AND                CL*25
01110         WS-CM-NAME-CHAR (WS-NAME-INDEX)                              CL*25
01111           MOVE 'X'               TO WS-COMPARE-INDICATOR.            CL*25
01112                                                                      CL*25
01113  4035-EXIT.                                                          CL*25
01114       EXIT.                                                          CL*25
01115                                                                      CL*25
01116  4040-CONTINUE.                                                      CL*25
01117      IF PI-SC-ACCT-NO = SPACES                                       CL*11
01118          GO TO 4050-CK-CARRIER.                                      CL*31
01119                                                                   EL1272
01120      IF PI-SC-ACCT-NO = CM-ACCOUNT                                EL1272
01121          NEXT SENTENCE                                               CL*31
01122        ELSE                                                          CL*31
01123          GO TO 4010-READNEXT.                                        CL*31
01124                                                                      CL*31
01125  4050-CK-CARRIER.                                                    CL*31
01126      IF PI-SC-CARR = SPACES
110106        GO TO 4060-CK-ST
           END-IF

01129      IF PI-SC-CARR = CM-CARRIER                                      CL*31
01130          NEXT SENTENCE                                            EL1272
01131        ELSE                                                       EL1272
01132          GO TO 4010-READNEXT.                                        CL*12
01133                                                                   EL1272
110106 4060-CK-ST.

110106     IF PI-SC-ST = SPACES
110106        GO TO 4070-CK-STATUS
110106     END-IF
           
110106     IF PI-SC-ST = CM-STATE
110106        CONTINUE
110106     ELSE
110106        GO TO 4010-READNEXT
110106     END-IF

110106     .
       4070-CK-STATUS.

           IF PI-SC-STATUS NOT = 'Y'
              GO TO 4090-MOVE-DATA
           END-IF
           
           IF (CM-LF-CURRENT-STATUS = '1' OR '2' OR '4')
              OR (CM-AH-CURRENT-STATUS = '1' OR '2' OR '4')
              CONTINUE
           ELSE
              GO TO 4010-READNEXT
           END-IF

           IF ((CM-LF-LOAN-EXPIRE-DT NOT = LOW-VALUES)
              AND (CM-LF-LOAN-EXPIRE-DT > SAVE-BIN-DATE))
                             OR
              ((CM-AH-LOAN-EXPIRE-DT NOT = LOW-VALUES)
              AND (CM-AH-LOAN-EXPIRE-DT > SAVE-BIN-DATE))
              CONTINUE
           ELSE
              GO TO 4010-READNEXT
           END-IF

           .
01134  4090-MOVE-DATA.                                                  EL1272
01135      IF LCP-ONCTR-02 =  0                                            CL*32
01136          ADD 1 TO LCP-ONCTR-02                                       CL*32
01137        ELSE                                                       EL1272
01138          GO TO 4095-MOVE-DATA.                                    EL1272
01139                                                                   EL1272
01140      MOVE +1                     TO WS-CERT-SW.                   EL1272
01141                                                                   EL1272
01142  4095-MOVE-DATA.                                                  EL1272
01143      MOVE WS-KEY-INPUT           TO  PI-LAST-KEY.                 EL1272
01144                                                                   EL1272
01145      ADD +1                      TO  PI-LINE-COUNT                EL1272
01146                                      PI-AIX-RECORD-COUNT.         EL1272
01147                                                                   EL1272
01148      IF CM-CLAIM-ATTACHED-COUNT > ZERO                               CL*36
01149          MOVE '*'                TO  EL127B-AST (EL127B-INDEX).   EL1272
01150                                                                   EL1272
01151      IF PI-COMPANY-ID = 'FLA'                                        CL*13
01152          PERFORM 9050-FLA-NAME THRU 9050-EXIT-FIX.                   CL*13
01153                                                                      CL*13
01154      PERFORM 5000-MOVE-NAME.                                      EL1272
01155                                                                   EL1272
01156      MOVE CM-CONTROL-PRIMARY   TO  PI-END-CERTIFICATE-KEY.           CL*36
01157      MOVE CM-INSURED-LAST-NAME TO  PI-END-NAME.                      CL*36
01158                                                                      CL*36
01159      MOVE WS-NAME-WORK         TO  EL127B-NAME-O   (EL127B-INDEX).EL1272
01160      MOVE CM-INSURED-ISSUE-AGE TO  EL127B-AGE      (EL127B-INDEX).EL1272
01161      MOVE CM-INSURED-SEX       TO  EL127B-SEX      (EL127B-INDEX).EL1272
01162      MOVE CM-CARRIER           TO  EL127B-CARRIER  (EL127B-INDEX).EL1272
01163      MOVE CM-GROUPING          TO  EL127B-GROUP    (EL127B-INDEX).EL1272
01164      MOVE CM-STATE             TO  EL127B-STATE    (EL127B-INDEX).EL1272
01165      MOVE CM-ACCOUNT           TO  EL127B-ACCOUNT  (EL127B-INDEX).EL1272
01166      MOVE CM-CERT-NO           TO  EL127B-CERT-NO  (EL127B-INDEX).EL1272
01167                                                                   EL1272
01168      IF CM-CERT-EFF-DT NOT = LOW-VALUES                           EL1272
01169          MOVE CM-CERT-EFF-DT       TO  DC-BIN-DATE-1              EL1272
01170          MOVE SPACES               TO  DC-OPTION-CODE             EL1272
01171          PERFORM 8500-DATE-CONVERSION                             EL1272
01172          MOVE DC-GREG-DATE-1-EDIT  TO  EL127B-EFF-DATE            EL1272
01173                                               (EL127B-INDEX).     EL1272
01174                                                                   EL1272
01175      PERFORM 6000-SET-ATTRB.                                      EL1272
01176                                                                   EL1272
01177      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL1272
01178      MOVE '5'                    TO  DC-OPTION-CODE.              EL1272
01179      PERFORM 8500-DATE-CONVERSION.                                EL1272
01180      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.             EL1272
01181                                                                   EL1272
01182      IF CM-LF-BENEFIT-CD = '00'                                      CL**3
01183          GO TO 4100-A-AND-H-INFO.                                 EL1272
01184                                                                   EL1272
01185      MOVE SPACES     TO    EL127B-LIFE-INFO (EL127B-INDEX).          CL**4
01186                                                                   EL1272
01187      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL1272
01188      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL1272
01189      MOVE '4'                    TO  WS-CFK-RECORD-TYPE.          EL1272
01190      MOVE CM-LF-BENEFIT-CD       TO  WS-CFK-BENEFIT-NO.           EL1272
01191                                                                   EL1272
01192      PERFORM 8000-READ-CONTROL-FILE.                              EL1272
01193                                                                   EL1272
01194      IF WS-NOT-FOUND = ZERO                                       EL1272
01195          MOVE CF-BENEFIT-ALPHA (WS-INDEX)                         EL1272
01196                                 TO  EL127B-LI-ABVR (EL127B-INDEX).EL1272
01197                                                                   EL1272
01198      IF CM-LF-CURRENT-STATUS = '8'                                EL1272
01199         IF CM-LF-CANCEL-DT NOT = LOW-VALUES                       EL1272
01200             MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1                 EL1272
01201             MOVE SPACES          TO DC-OPTION-CODE                EL1272
01202             PERFORM 8500-DATE-CONVERSION                          EL1272
01203             IF NOT DATE-CONVERSION-ERROR                          EL1272
01204                 MOVE DC-GREG-DATE-1-EDIT TO EL127B-LI-DATE        EL1272
01205                                                    (EL127B-INDEX).EL1272
01206                                                                   EL1272
01207      IF CM-LF-CURRENT-STATUS = '7'                                EL1272
01208         IF CM-LF-DEATH-DT NOT = LOW-VALUES                        EL1272
01209             MOVE CM-LF-DEATH-DT  TO DC-BIN-DATE-1                 EL1272
01210             MOVE SPACES          TO DC-OPTION-CODE                EL1272
01211             PERFORM 8500-DATE-CONVERSION                          EL1272
01212             IF NOT DATE-CONVERSION-ERROR                          EL1272
01213                 MOVE DC-GREG-DATE-1-EDIT TO EL127B-LI-DATE        EL1272
01214                                                   (EL127B-INDEX).    CL*22
01215                                                                   EL1272
01216 * READ STATE MASTER RECORD FOR FREE LOOK PERIOD *                    CL*35
01217                                                                      CL*35
01218      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.            CL*35
01219      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.              CL*35
01220      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.             CL*35
01221      MOVE CM-STATE               TO  WS-CFK-ACCESS-TYPE.             CL*35
01222                                                                      CL*35
01223      PERFORM 8000-READ-CONTROL-FILE.                                 CL*35
01224                                                                      CL*35
01225      IF WS-ST-REC-NOT-FOUND = ZERO                                   CL*35
01226         MOVE CF-ST-FREE-LOOK-PERIOD                                  CL*35
01227                                  TO CP-FREE-LOOK                     CL*35
01228      ELSE                                                            CL*35
01229         MOVE ZERO                TO CP-FREE-LOOK.                    CL*35
01230                                                                      CL*35
01231      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.               EL1272
01232      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.              EL1272
01233      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.              CL**2
01234      MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.             EL1272
01235      MOVE CM-LF-ORIG-TERM        TO  CP-ORIGINAL-TERM.            EL1272
01236      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.         CL*14
01237      MOVE '4'                    TO  CP-REM-TERM-METHOD.          EL1272
01238      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.               EL1272
01239                                                                      CL*35
01240      EXEC CICS LINK                                               EL1272
01241           PROGRAM  (ELRTRM)                                       EL1272
01242           COMMAREA (CALCULATION-PASS-AREA)                        EL1272
01243           LENGTH   (CP-COMM-LENGTH)                               EL1272
01244      END-EXEC.                                                       CL*36
01245                                                                   EL1272
01246      IF CM-LF-CURRENT-STATUS = '1' OR '4'                         EL1272
01247         IF CP-REMAINING-TERM-3 = ZEROS                            EL1272
01248            MOVE 'EXPIRED'        TO EL127B-LI-DESC2 (EL127B-INDEX)EL1272
01249            MOVE CM-LF-BENEFIT-AMT TO EL127B-LI-AMT  (EL127B-INDEX)EL1272
01250         ELSE                                                      EL1272
01251            MOVE 'ACTIVE'        TO EL127B-LI-DESC2 (EL127B-INDEX).EL1272
01252                                                                   EL1272
01253      IF CM-LF-CURRENT-STATUS = '2'                                EL1272
01254         MOVE 'PEND  '           TO EL127B-LI-DESC2 (EL127B-INDEX).EL1272
01255                                                                   EL1272
01256      IF CM-LF-CURRENT-STATUS = '3'                                EL1272
01257         MOVE 'RESTORE'          TO EL127B-LI-DESC2 (EL127B-INDEX).EL1272
01258                                                                   EL1272
01259      IF CM-LF-CURRENT-STATUS = '5'                                EL1272
01260         MOVE 'REISSUE'          TO EL127B-LI-DESC2 (EL127B-INDEX).EL1272
01261                                                                   EL1272
01262      IF CM-LF-CURRENT-STATUS = '6'                                EL1272
01263         MOVE 'LMP DIS'          TO EL127B-LI-DESC2 (EL127B-INDEX).EL1272
01264                                                                   EL1272
01265      IF CM-LF-CURRENT-STATUS = '7'                                EL1272
01266         MOVE 'DEATH  '          TO EL127B-LI-DESC2 (EL127B-INDEX).EL1272
01267                                                                   EL1272
01268      IF CM-LF-CURRENT-STATUS = '8'                                EL1272
01269         MOVE 'CANCEL '          TO EL127B-LI-DESC2 (EL127B-INDEX).EL1272
01270                                                                   EL1272
01271      IF CM-LF-CURRENT-STATUS = '9'                                EL1272
01272         MOVE 'RE-ONLY'          TO EL127B-LI-DESC2 (EL127B-INDEX).EL1272
01273                                                                   EL1272
01274      IF CM-LF-CURRENT-STATUS = 'D'                                   CL*15
01275         MOVE 'DECLINE'          TO EL127B-LI-DESC2 (EL127B-INDEX).   CL*15
01276                                                                      CL*15
01277      IF CM-LF-CURRENT-STATUS = 'V'                                   CL*15
01278         MOVE 'VOID'             TO EL127B-LI-DESC2 (EL127B-INDEX).   CL*15
01279                                                                      CL*15
01280      IF CM-LF-CURRENT-STATUS = '6' OR '7' OR '8'                  EL1272
01281         NEXT SENTENCE                                             EL1272
01282      ELSE                                                         EL1272
01283         MOVE CM-LF-BENEFIT-AMT   TO EL127B-LI-AMT  (EL127B-INDEX).EL1272
01284                                                                   EL1272
01285      IF PI-COMPANY-ID = 'CRI'  OR  'PEM'                             CL**4
01286         MOVE CM-MEMBER-NO     TO  EL127B-MEMB-LOAN (EL127B-INDEX)    CL**4
01287      ELSE                                                            CL**4
01288         MOVE CM-LOAN-NUMBER   TO  EL127B-MEMB-LOAN (EL127B-INDEX).   CL**4
01289                                                                      CL**4
01290  4100-A-AND-H-INFO.                                               EL1272
01291      IF CM-AH-BENEFIT-CD = '00'                                      CL**3
01292          GO TO 4200-CONTINUE.                                     EL1272
01293                                                                   EL1272
01294      MOVE SPACES               TO  EL127B-AH-INFO (EL127B-INDEX).    CL*36
01295                                                                   EL1272
01296      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL1272
01297      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL1272
01298      MOVE '5'                    TO  WS-CFK-RECORD-TYPE.          EL1272
01299      MOVE CM-AH-BENEFIT-CD       TO  WS-CFK-BENEFIT-NO.           EL1272
01300                                                                   EL1272
01301      PERFORM 8000-READ-CONTROL-FILE.                              EL1272
01302                                                                   EL1272
01303      IF WS-NOT-FOUND = ZERO                                       EL1272
01304          MOVE CF-BENEFIT-ALPHA (WS-INDEX)                         EL1272
01305                                 TO  EL127B-AH-ABVR (EL127B-INDEX).EL1272
01306                                                                      CL*35
01307 * READ STATE MASTER RECORD FOR FREE LOOK PERIOD *                    CL*35
01308                                                                      CL*35
01309      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.            CL*35
01310      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.              CL*35
01311      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.             CL*35
01312      MOVE CM-STATE               TO  WS-CFK-ACCESS-TYPE.             CL*35
01313                                                                      CL*35
01314      PERFORM 8000-READ-CONTROL-FILE.                                 CL*35
01315                                                                      CL*35
01316      IF WS-ST-REC-NOT-FOUND = ZERO                                   CL*35
01317         MOVE CF-ST-FREE-LOOK-PERIOD                                  CL*35
01318                                  TO CP-FREE-LOOK                     CL*35
01319      ELSE                                                            CL*35
01320         MOVE ZERO                TO CP-FREE-LOOK.                    CL*35
01321                                                                   EL1272
01322      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.              EL1272
01323      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.              CL**2
01324      MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.             EL1272
01325      MOVE CM-AH-ORIG-TERM        TO  CP-ORIGINAL-TERM.            EL1272
01326      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.         CL*14
01327      MOVE '4'                    TO  CP-REM-TERM-METHOD.          EL1272
01328      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.               EL1272
01329      EXEC CICS LINK                                               EL1272
01330           PROGRAM  (ELRTRM)                                       EL1272
01331           COMMAREA (CALCULATION-PASS-AREA)                        EL1272
01332           LENGTH   (CP-COMM-LENGTH)                               EL1272
01333      END-EXEC.                                                       CL*36
01334                                                                   EL1272
01335      IF CM-AH-CURRENT-STATUS = '8'                                EL1272
01336         IF CM-AH-CANCEL-DT NOT = LOW-VALUES                       EL1272
01337             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1                 EL1272
01338             MOVE SPACES          TO DC-OPTION-CODE                EL1272
01339             PERFORM 8500-DATE-CONVERSION                          EL1272
01340             IF NOT DATE-CONVERSION-ERROR                          EL1272
01341                 MOVE DC-GREG-DATE-1-EDIT TO EL127B-AH-DATE        EL1272
01342                                                    (EL127B-INDEX).EL1272
01343                                                                   EL1272
01344      IF CM-AH-CURRENT-STATUS = '6' OR '7'                         EL1272
01345         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES                   EL1272
01346             MOVE CM-AH-SETTLEMENT-DT TO DC-BIN-DATE-1             EL1272
01347             MOVE SPACES              TO DC-OPTION-CODE            EL1272
01348             PERFORM 8500-DATE-CONVERSION                          EL1272
01349             IF NOT DATE-CONVERSION-ERROR                          EL1272
01350                 MOVE DC-GREG-DATE-1-EDIT TO EL127B-AH-DATE        EL1272
01351                                                    (EL127B-INDEX).EL1272
01352                                                                   EL1272
01353      IF CM-AH-CURRENT-STATUS = '1' OR '4'                         EL1272
01354        IF CP-REMAINING-TERM-3 = ZEROS                             EL1272
01355           MOVE 'EXPIRED'         TO EL127B-AH-DESC2 (EL127B-INDEX)EL1272
01356           MOVE CM-AH-BENEFIT-AMT TO EL127B-AH-AMT   (EL127B-INDEX)EL1272
01357        ELSE                                                       EL1272
01358           MOVE 'ACTIVE'         TO EL127B-AH-DESC2 (EL127B-INDEX).EL1272
01359                                                                   EL1272
01360      IF CM-AH-CURRENT-STATUS = '2'                                EL1272
01361         MOVE 'PEND   '          TO EL127B-AH-DESC2 (EL127B-INDEX).EL1272
01362                                                                   EL1272
01363      IF CM-AH-CURRENT-STATUS = '3'                                EL1272
01364         MOVE 'RESTORE'          TO EL127B-AH-DESC2 (EL127B-INDEX).EL1272
01365                                                                   EL1272
01366      IF CM-AH-CURRENT-STATUS = '5'                                EL1272
01367         MOVE 'REISSUE'          TO EL127B-AH-DESC2 (EL127B-INDEX).EL1272
01368                                                                   EL1272
01369      IF CM-AH-CURRENT-STATUS = '6'                                EL1272
01370         MOVE 'LMP DIS'          TO EL127B-AH-DESC2 (EL127B-INDEX).EL1272
01371                                                                   EL1272
01372      IF CM-AH-CURRENT-STATUS = '7'                                EL1272
01373         MOVE 'DEATH  '          TO EL127B-AH-DESC2 (EL127B-INDEX).EL1272
01374                                                                   EL1272
01375      IF CM-AH-CURRENT-STATUS = '8'                                EL1272
01376         MOVE 'CANCEL '          TO EL127B-AH-DESC2 (EL127B-INDEX).EL1272
01377                                                                   EL1272
01378      IF CM-AH-CURRENT-STATUS = '9'                                EL1272
01379         MOVE 'RE-ONLY'          TO EL127B-AH-DESC2 (EL127B-INDEX).EL1272
01380                                                                      CL*15
01381      IF CM-AH-CURRENT-STATUS = 'D'                                   CL*15
01382         MOVE 'DECLINE'          TO EL127B-AH-DESC2 (EL127B-INDEX).   CL*15
01383                                                                      CL*15
01384      IF CM-AH-CURRENT-STATUS = 'V'                                   CL*15
01385         MOVE 'VOID'             TO EL127B-AH-DESC2 (EL127B-INDEX).   CL*15
01386                                                                   EL1272
01387      IF CM-AH-CURRENT-STATUS = '6' OR '7' OR '8'                  EL1272
01388         NEXT SENTENCE                                             EL1272
01389      ELSE                                                         EL1272
01390         MOVE CM-AH-BENEFIT-AMT   TO EL127B-AH-AMT  (EL127B-INDEX).EL1272
01391                                                                   EL1272
01392  4200-CONTINUE.                                                   EL1272
01393      IF EL127B-INDEX < +8                                            CL*36
01394          SET EL127B-INDEX UP BY +1                                EL1272
01395          GO TO 4010-READNEXT.                                     EL1272
01396                                                                   EL1272
01397      GO TO 4900-ENDBROWSE.                                        EL1272
01398                                                                   EL1272
01399  4700-END-OF-BROWSE.                                              EL1272
01400      MOVE ER-0130                 TO  EMI-ERROR.                     CL*26
01401      ADD +1  TO  PI-END-OF-FILE.                                  EL1272
01402                                                                   EL1272
01403  4900-ENDBROWSE.                                                  EL1272
01404                                                                      CL*22
01405      ADD 1 TO PI-SCREEN-COUNT.                                    EL1272
01406                                                                      CL*22
01407      EXEC CICS ENDBR                                              EL1272
01408          DATASET (PI-DSID)                                        EL1272
01409      END-EXEC.                                                       CL*36
01410                                                                   EL1272
01411      IF CREDIT-SESSION                                               CL*20
01412          IF (PI-LINE-COUNT  =  +1)  AND                              CL*20
01413             (PI-1ST-TIME-SW =  ZEROS)                                CL*20
01414              NEXT SENTENCE                                           CL*20
01415          ELSE                                                        CL*20
01416              GO TO 4910-SEND-MAP                                     CL*20
01417      ELSE                                                            CL*20
01418         GO TO 4910-SEND-MAP.                                         CL*20
01419                                                                      CL*20
01420      MOVE EL127B-CARRIER (1)        TO  PI-CARRIER.                  CL*20
01421      MOVE EL127B-GROUP   (1)        TO  PI-GROUPING.                 CL*20
01422      MOVE EL127B-STATE   (1)        TO  PI-STATE.                    CL*20
01423      MOVE EL127B-ACCOUNT (1)        TO  PI-ACCOUNT.                  CL*20
01424      MOVE EL127B-CERT-NO (1)        TO  PI-CERT-NO.                  CL*20
01425      MOVE EL127B-EFF-DATE(1)        TO  DC-GREG-DATE-1-EDIT.         CL*20
01426      MOVE '2'                       TO  DC-OPTION-CODE.              CL*22
01427      PERFORM 8500-DATE-CONVERSION.                                   CL*20
01428      MOVE DC-BIN-DATE-1             TO  PI-CERT-EFF-DT.              CL*22
01429                                                                      CL*20
01430      MOVE PI-RETURN-TO-PROGRAM    TO  PI-CALLING-PROGRAM.            CL*20
01431      MOVE PI-SAVED-PROGRAM-1      TO  PI-RETURN-TO-PROGRAM           CL*20
01432                                       THIS-PGM.                      CL*20
01433      MOVE PI-SAVED-PROGRAM-2      TO  PI-SAVED-PROGRAM-1.            CL*20
01434      MOVE PI-SAVED-PROGRAM-3      TO  PI-SAVED-PROGRAM-2.            CL*20
01435      MOVE PI-SAVED-PROGRAM-4      TO  PI-SAVED-PROGRAM-3.            CL*20
01436      MOVE PI-SAVED-PROGRAM-5      TO  PI-SAVED-PROGRAM-4.            CL*20
01437      MOVE PI-SAVED-PROGRAM-6      TO  PI-SAVED-PROGRAM-5.            CL*20
01438      MOVE SPACES                  TO  PI-SAVED-PROGRAM-6.            CL*20
01439                                                                      CL*20
01440      IF THIS-PGM = 'EL6311' OR 'EL626' OR 'EL126' OR                 CL*22
01441                    'EL1272' OR 'EL127'                               CL*22
01442          MOVE 'EL1273'            TO  THIS-PGM                       CL*22
01443          GO TO 9300-XCTL.                                            CL*20
01444                                                                      CL*20
01445  4910-SEND-MAP.                                                   EL1272
01446      IF WS-NO-CERT-FOUND                                             CL*36
01447          MOVE +9                 TO  PI-BROWSE-SW                 EL1272
01448          GO TO 9400-CLEAR.                                           CL*17
01449                                                                   EL1272
01450      MOVE +1                     TO  PI-1ST-TIME-SW.                 CL*28
01451      MOVE -1                     TO  BSELL.                       EL1272
01452      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
01453                                                                   EL1272
01454      EJECT                                                        EL1272
uktdel*5000-MOVE-NAME SECTION. COPY ELCMNS REPLACING                    EL1272
uktins 5000-MOVE-NAME SECTION.
uktins     COPY ELCMNS REPLACING
01456      CL-INSURED-LAST-NAME        BY  CM-INSURED-LAST-NAME         EL1272
01457      CL-INSURED-1ST-NAME         BY  CM-INSURED-FIRST-NAME           CL*20
01458      CL-INSURED-MID-INIT         BY  CM-INSURED-INITIAL2.            CL*24
01459                                                                   EL1272
01460      EJECT                                                        EL1272
01461  6000-SET-ATTRB SECTION.                                          EL1272
01462      MOVE AL-SANON      TO  EL127B-AST-ATTRB       (EL127B-INDEX) EL1272
01463                             EL127B-CARRIER-ATTRB   (EL127B-INDEX) EL1272
01464                             EL127B-GROUP-ATTRB     (EL127B-INDEX) EL1272
01465                             EL127B-STATE-ATTRB     (EL127B-INDEX) EL1272
01466                             EL127B-ACCOUNT-ATTRB   (EL127B-INDEX) EL1272
01467                             EL127B-CERT-NO-ATTRB   (EL127B-INDEX) EL1272
01468                             EL127B-NAME-ATTRB      (EL127B-INDEX) EL1272
01469                             EL127B-AGE-ATTRB       (EL127B-INDEX) EL1272
01470                             EL127B-SEX-ATTRB       (EL127B-INDEX) EL1272
01471                             EL127B-EFF-DATE-ATTRB  (EL127B-INDEX) EL1272
01472                             EL127B-LIFE-INFO-ATTRB (EL127B-INDEX) EL1272
01473                             EL127B-AH-INFO-ATTRB   (EL127B-INDEX).EL1272
01474                                                                   EL1272
01475      IF CREDIT-SESSION                                               CL**3
01476          MOVE AL-SANON  TO  EL127B-CERT-SEL-ATTRB  (EL127B-INDEX)    CL**3
01477          MOVE AL-UNNON  TO  BSELA                                    CL**3
01478      ELSE                                                            CL**3
01479          MOVE AL-UANON  TO  EL127B-CERT-SEL-ATTRB  (EL127B-INDEX).   CL**3
01480                                                                   EL1272
01481      IF PI-SAVED-PROGRAM-1 = 'EL130'                                 CL*36
01482          MOVE AL-SANOF  TO  BPFK3A   BPFK5A                          CL**6
01483          MOVE SPACES    TO  BPFK3O   BPFK5O                          CL**6
01484      ELSE                                                            CL**5
01485          MOVE AL-SANON  TO  BPFK3A   BPFK5A.                         CL**6
01486                                                                      CL**5
01487  6000-EXIT.                                                       EL1272
01488      EXIT.                                                        EL1272
01489                                                                   EL1272
01490      EJECT                                                        EL1272
01491  7000-PF2-POSITION         SECTION.                               EL1272
01492                                                                   EL1272
01493      EXEC CICS IGNORE CONDITION                                   EL1272
01494           DUPKEY                                                  EL1272
01495      END-EXEC.                                                       CL*36
01496                                                                      CL*36
01497      EXEC CICS HANDLE CONDITION                                   EL1272
01498          NOTFND (8700-NOT-FOUND)                                  EL1272
01499      END-EXEC.                                                       CL*36
01500                                                                   EL1272
01501      IF PI-PART-FIELD-SW  = 'C' OR 'S'                               CL*32
01502         MOVE PI-SAVE-KEY-LENGTH TO PI-KEY-LENGTH.                    CL*32
01503                                                                      CL*32
01504      COMPUTE WS-CALC-RDNXT = PI-SCREEN-COUNT * 7.                 EL1272
01505      MOVE PI-1ST-KEY             TO PI-CERTIFICATE-KEY.           EL1272
01506      MOVE ZERO                   TO PI-END-OF-FILE.               EL1272
01507                                                                      CL*36
01508      IF (OPTION-FOUR-SELECTED   AND PI-KEY-LENGTH = 12)              CL*25
01509                       OR                                             CL*25
01510         (OPTION-THREE-SELECTED  AND PI-KEY-LENGTH = 18)              CL*25
01511        EXEC CICS STARTBR                                             CL*16
01512           DATASET   (PI-DSID)                                        CL*16
01513           RIDFLD    (PI-CERTIFICATE-KEY)                             CL*16
01514           EQUAL                                                      CL*16
01515           KEYLENGTH (PI-KEY-LENGTH)                                  CL*16
01516        END-EXEC                                                      CL*36
01517       ELSE                                                           CL*25
01518        EXEC CICS STARTBR                                             CL*16
01519           DATASET   (PI-DSID)                                     EL1272
01520           RIDFLD    (PI-CERTIFICATE-KEY)                          EL1272
01521           GENERIC                                                 EL1272
01522           EQUAL                                                   EL1272
01523           KEYLENGTH (PI-KEY-LENGTH)                               EL1272
01524        END-EXEC.                                                     CL*36
01525                                                                   EL1272
01526  7100-READNEXT-PF2.                                               EL1272
01527      IF WS-CALC-RDNXT > ZERO                                         CL*36
01528          NEXT SENTENCE                                            EL1272
01529        ELSE                                                       EL1272
01530          GO TO 7999-EXIT.                                         EL1272
01531                                                                      CL*11
01532      EXEC CICS READNEXT                                           EL1272
01533          DATASET (PI-DSID)                                        EL1272
01534          RIDFLD  (PI-CERTIFICATE-KEY)                             EL1272
01535          SET     (ADDRESS OF CERTIFICATE-MASTER)                     CL*32
01536      END-EXEC.                                                       CL*36
01537                                                                   EL1272
01538 ******************************************************************EL1272
01539 *    IF THE SECURITY CHECKING ROUTINE, INITIAL COMPARE ROUTINE   *EL1272
01540 *        OR THE ACCOUNT COMPARE ROUTINE IS CHANGED HERE, YOU     *EL1272
01541 *        MUST ALSO CHANGE THE CORRESPONDING ROUTINE IN           *EL1272
01542 *        PARAGRAPH 4020-COMPARE-KEY OR 4030-CHECK-OPTION.        *EL1272
01543 *                                     KER/080884.                *EL1272
01544 ******************************************************************EL1272
01545                                                                   EL1272
01546      IF PI-NO-CARRIER-SECURITY AND                                   CL*36
01547         PI-NO-ACCOUNT-SECURITY                                       CL*36
01548          GO TO 7130-CHECK-INITIAL.                                   CL*25
01549                                                                   EL1272
01550      IF PI-CARRIER-SECURITY > SPACES                                 CL*36
01551          IF CM-CARRIER NOT = PI-CARRIER-SECURITY                     CL*36
01552              GO TO 7100-READNEXT-PF2.                             EL1272
01553                                                                   EL1272
01554      IF PI-ACCOUNT-SECURITY > SPACES                                 CL*36
01555          IF CM-ACCOUNT NOT = PI-ACCOUNT-SECURITY                     CL*36
01556              GO TO 7100-READNEXT-PF2.                             EL1272
01557                                                                   EL1272
01558  7130-CHECK-INITIAL.                                                 CL*25
01559      IF NOT OPTION-THREE-SELECTED                                    CL*25
01560          GO TO 7190-COMPUTE.                                         CL**7
01561                                                                      CL**7
01562      IF PI-SC-INITIALS NOT = SPACES                               EL1272
01563         MOVE PI-SC-INITIALS      TO WS-INITIALS                   EL1272
01564         IF WS-INIT2 NOT = SPACE                                   EL1272
01565            IF PI-SC-INITIALS NOT = CM-INSURED-INITIALS            EL1272
01566               GO TO 7100-READNEXT-PF2                             EL1272
01567              ELSE                                                    CL*36
01568               NEXT SENTENCE                                       EL1272
01569            ELSE                                                   EL1272
01570            IF WS-INIT1 NOT = CM-INSURED-INITIAL1                  EL1272
01571               GO TO 7100-READNEXT-PF2.                            EL1272
01572                                                                      CL*22
01573      IF PI-SC-FIRST-NAME = SPACE                                     CL*22
01574          GO TO 7150-CONTINUE.                                        CL*25
01575                                                                   EL1272
01576      MOVE PI-SC-FIRST-NAME       TO WS-PI-NAME.                      CL*25
01577      MOVE CM-INSURED-FIRST-NAME  TO WS-CM-NAME.                      CL*25
01578                                                                      CL*25
01579      MOVE SPACE                  TO WS-COMPARE-INDICATOR.            CL*25
01580      PERFORM 4035-CHECK-NAME THRU 4035-EXIT                          CL*25
01581          VARYING WS-NAME-INDEX FROM 15 BY -1                         CL*25
01582              UNTIL WS-NAME-INDEX = ZERO.                             CL*25
01583                                                                      CL*25
01584      IF NAME-NOT-FOUND                                               CL*25
01585          GO TO 7100-READNEXT-PF2.                                    CL*25
01586                                                                      CL*25
01587  7150-CONTINUE.                                                      CL*25
01588      IF PI-SC-ACCT-NO = SPACES                                       CL*11
01589          GO TO 7190-COMPUTE.                                         CL*11
01590                                                                      CL*11
01591      IF PI-SC-ACCT-NO NOT = CM-ACCOUNT                               CL*36
01592          GO TO 7100-READNEXT-PF2.                                    CL*12
01593                                                                   EL1272
01594  7190-COMPUTE.                                                    EL1272
01595      COMPUTE WS-CALC-RDNXT = WS-CALC-RDNXT - 1.                   EL1272
01596      GO TO 7100-READNEXT-PF2.                                     EL1272
01597                                                                   EL1272
01598  7999-EXIT.                                                       EL1272
01599      EXIT.                                                        EL1272
01600                                                                   EL1272
01601      EJECT                                                        EL1272
01602  8000-READ-CONTROL-FILE SECTION.                                  EL1272
01603      MOVE ZERO                   TO  WS-NOT-FOUND                    CL*36
01604                                      WS-ST-REC-NOT-FOUND.            CL*36
01605                                                                   EL1272
01606      EXEC CICS HANDLE CONDITION                                   EL1272
01607          NOTFND (8020-NOTFND)                                     EL1272
01608      END-EXEC.                                                       CL*36
01609                                                                   EL1272
01610      EXEC CICS READ                                               EL1272
01611          DATASET   (WS-CONTROL-FILE-DSID)                         EL1272
01612          RIDFLD    (WS-CONTROL-FILE-KEY)                          EL1272
01613          SET       (ADDRESS OF CONTROL-FILE)                         CL*32
01614          GENERIC                                                  EL1272
01615          GTEQ                                                     EL1272
01616          KEYLENGTH (8)                                            EL1272
01617      END-EXEC.                                                       CL*36
01618                                                                   EL1272
01619      IF CF-RECORD-TYPE NOT = WS-CFK-RECORD-TYPE                   EL1272
01620          GO TO 8020-NOTFND.                                       EL1272
01621                                                                   EL1272
01622      IF WS-CFK-RECORD-TYPE = '3'                                     CL*35
01623          GO TO 8090-EXIT.                                            CL*35
01624                                                                      CL*35
01625      MOVE +1                     TO WS-INDEX.                        CL*35
01626                                                                   EL1272
01627  8010-FIND-BENEFIT.                                               EL1272
01628      IF WS-CFK-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)               CL**3
01629          GO TO 8090-EXIT.                                         EL1272
01630                                                                   EL1272
01631      IF CF-BENEFIT-CODE (WS-INDEX) NOT < CF-HI-BEN-IN-REC            CL*36
01632          GO TO 8020-NOTFND.                                       EL1272
01633                                                                   EL1272
01634      IF WS-INDEX < +8                                                CL*36
01635          ADD +1  TO  WS-INDEX                                     EL1272
01636          GO TO 8010-FIND-BENEFIT.                                 EL1272
01637                                                                   EL1272
01638  8020-NOTFND.                                                     EL1272
01639      IF WS-CFK-RECORD-TYPE = '3'                                     CL*35
01640         MOVE +1                  TO  WS-ST-REC-NOT-FOUND             CL*35
01641         GO TO 8090-EXIT.                                             CL*35
01642                                                                      CL*35
01643      MOVE +1                     TO  WS-NOT-FOUND.                EL1272
01644                                                                   EL1272
01645  8090-EXIT.                                                       EL1272
01646      EXIT.                                                        EL1272
01647                                                                   EL1272
01648      EJECT                                                        EL1272
01649  8100-SEND-INITIAL-MAP SECTION.                                   EL1272
01650      MOVE SAVE-DATE              TO  ADATEO.                      EL1272
01651      MOVE EIBTIME                TO  TIME-IN.                     EL1272
01652      MOVE TIME-OUT               TO  ATIMEO.                      EL1272
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01653                                                                   EL1272
01654      IF CREDIT-SESSION                                            EL1272
01655         MOVE SPACES              TO BPFK4O                           CL**5
01656                                     BPFK5O.                          CL**5
01662                                                                      CL**3
01663      IF EMI-ERROR NOT = ZERO                                      EL1272
01664          PERFORM 9900-ERROR-FORMAT.                               EL1272
01665                                                                   EL1272
01666      IF PI-ALT-NAME-COUNT > 140                                      CL*36
01667          MOVE ER-0765             TO  EMI-ERROR                      CL*25
01668          PERFORM 9900-ERROR-FORMAT.                                  CL*25
01669                                                                      CL*25
01670      MOVE EMI-MESSAGE-AREA (1)    TO  BEMSG1O.                    EL1272
01671      MOVE EMI-MESSAGE-AREA (2)    TO  BEMSG2O.                       CL*25

101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                       EL1272
101501*       MOVE AL-PABON            TO BPFK6A BPFK7A                 EL1272
101501*       MOVE PI-COMPANY-ID       TO BCOMPO                        EL1272
101501*    ELSE                                                         EL1272
101501*       MOVE SPACES              TO BCOMPO                        EL1272
101501*       MOVE AL-PADOF            TO BPFK6A BPFK7A.                EL1272
01679                                                                   EL1272
01680      EXEC CICS SEND                                               EL1272
01681          FROM   (EL127BO)                                         EL1272
01682          MAPSET (WS-MAPSET-NAME)                                  EL1272
01683          MAP    (WS-MAP-NAME)                                     EL1272
01684          CURSOR                                                   EL1272
01685          ERASE                                                    EL1272
01686      END-EXEC.                                                       CL*36
01687                                                                   EL1272
01688      GO TO 9100-RETURN-TRAN.                                         CL*17
01689                                                                      CL*17
01690  8100-EXIT.                                                       EL1272
01691      EXIT.                                                        EL1272
01692                                                                   EL1272
01693      EJECT                                                        EL1272
01694  8200-SEND-DATAONLY SECTION.                                      EL1272
01695      MOVE SAVE-DATE              TO  ADATEO.                      EL1272
01696      MOVE EIBTIME                TO  TIME-IN.                     EL1272
01697      MOVE TIME-OUT               TO  ATIMEO.                      EL1272
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01698                                                                   EL1272
01699      IF CREDIT-SESSION                                            EL1272
01700         MOVE SPACES              TO BPFK4O                           CL**5
01701                                     BPFK5O.                          CL**5
01702                                                                   EL1272
01703      IF EMI-ERROR NOT = ZERO                                      EL1272
01704          PERFORM 9900-ERROR-FORMAT.                               EL1272
01705                                                                   EL1272
01706      IF PI-ALT-NAME-COUNT > 140                                      CL*36
01707          MOVE ER-0765             TO  EMI-ERROR                      CL*25
01708          PERFORM 9900-ERROR-FORMAT.                                  CL*25
01709                                                                      CL*25
01710      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.                     EL1272
01711      MOVE EMI-MESSAGE-AREA (2)   TO  BEMSG2O.                        CL*25
01712                                                                   EL1272
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                       EL1272
101501*       MOVE AL-PABON            TO BPFK6A BPFK7A                 EL1272
101501*       MOVE PI-COMPANY-ID       TO BCOMPO                        EL1272
101501*    ELSE                                                         EL1272
101501*       MOVE SPACES              TO BCOMPO                        EL1272
101501*       MOVE AL-PADOF            TO BPFK6A BPFK7A.                EL1272
01719                                                                   EL1272
01720      EXEC CICS SEND DATAONLY                                      EL1272
01721          FROM   (EL127BO)                                         EL1272
01722          MAPSET (WS-MAPSET-NAME)                                  EL1272
01723          MAP    (WS-MAP-NAME)                                     EL1272
01724          CURSOR                                                   EL1272
01725      END-EXEC.                                                       CL*36
01726                                                                      CL*17
01727      GO TO 9100-RETURN-TRAN.                                         CL*17
01728                                                                   EL1272
01729  8200-EXIT.                                                       EL1272
01730      EXIT.                                                        EL1272
01731                                                                   EL1272
01732      EJECT                                                        EL1272
01733  8300-SEND-TEXT SECTION.                                          EL1272
01734      EXEC CICS SEND TEXT                                          EL1272
01735          FROM   (LOGOFF-TEXT)                                     EL1272
01736          LENGTH (LOGOFF-LENGTH)                                   EL1272
01737          ERASE                                                    EL1272
01738          FREEKB                                                   EL1272
01739      END-EXEC.                                                       CL*36
01740                                                                   EL1272
01741      EXEC CICS RETURN                                             EL1272
01742      END-EXEC.                                                       CL*36
01743                                                                   EL1272
01744  8300-EXIT.                                                       EL1272
01745      EXIT.                                                        EL1272
01746                                                                   EL1272
01747  8500-DATE-CONVERSION SECTION.                                    EL1272
01748      EXEC CICS LINK                                               EL1272
01749          PROGRAM  ('ELDATCV')                                     EL1272
01750          COMMAREA (DATE-CONVERSION-DATA)                          EL1272
01751          LENGTH   (DC-COMM-LENGTH)                                EL1272
01752      END-EXEC.                                                       CL*36
01753                                                                   EL1272
01754  8500-EXIT.                                                       EL1272
01755      EXIT.                                                        EL1272
01756                                                                      CL*36
01757      EJECT                                                        EL1272
01758  8600-NEXT-COMPANY SECTION.                                       EL1272
01759 ******************************************************************   CL*17
01760 ****      READ THE CURRENT COMPANY RECORD TO OBTAIN THE       ****   CL*17
01761 ****      NEXT COMPANY ID.                                    ****   CL*17
01762 ******************************************************************   CL*17
01763                                                                   EL1272
01764      MOVE SPACES                     TO  WS-CONTROL-FILE-KEY.        CL*17
01765      MOVE PI-COMPANY-ID              TO  WS-CFK-COMPANY-ID.          CL*17
01766      MOVE '1'                        TO  WS-CFK-RECORD-TYPE.         CL*17
01767      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.         CL*17
01768                                                                   EL1272
01769      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                       CL*17
01770                                                                   EL1272
01771      IF WS-CNTL-REC-FOUND-SW = 'N'                                   CL*36
01772          PERFORM 8800-INITIALIZE-MAP VARYING EL127B-INDEX            CL*17
01773            FROM +1 BY +1 UNTIL EL127B-INDEX > +8                     CL*36
01774          MOVE ER-0022                TO  EMI-ERROR                   CL*17
01775          MOVE -1                     TO  BSELL                       CL*17
01776          GO TO 8100-SEND-INITIAL-MAP.                                CL*17
01777                                                                   EL1272
01778      IF EIBAID = DFHPF6                                           EL1272
01779          MOVE CF-NEXT-COMPANY-ID     TO  WS-NEXT-COMPANY-ID.         CL*17
01780                                                                   EL1272
01781      IF EIBAID = DFHPF7                                           EL1272
01782          MOVE PI-ORIGINAL-COMPANY-ID TO  WS-NEXT-COMPANY-ID.         CL*17
01783                                                                   EL1272
01784      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*36
01785          GO TO 8600-CONTINUE-NEXT-COMPANY.                           CL*17
01786                                                                   EL1272
01787 ******************************************************************   CL*17
01788 ****      READ THE CURRENT USER RECORD FOR UPDATE AND REMOVE  ****   CL*17
01789 ****      THE TERMINAL ID FROM THE RECORD.                    ****   CL*17
01790 ******************************************************************   CL*17
01791                                                                   EL1272
01792      MOVE PI-COMPANY-ID              TO  WS-CFK-COMPANY-ID.          CL*17
01793      MOVE '2'                        TO  WS-CFK-RECORD-TYPE.         CL*17
01794      MOVE PI-PROCESSOR-ID            TO  WS-CFK-ACCESS-TYPE.         CL*17
01795      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.         CL*17
01796                                                                   EL1272
01797      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.                CL*17
01798                                                                   EL1272
01799      IF WS-CNTL-REC-FOUND-SW = 'N'                                   CL*36
01800          MOVE ER-0019                TO  EMI-ERROR                   CL*17
01801          MOVE -1                     TO  BSELL                       CL*17
01802          GO TO 8200-SEND-DATAONLY.                                   CL*17
01803                                                                   EL1272
01804      MOVE SPACES                     TO  CF-CURRENT-TERM-ON.         CL*17
01805                                                                   EL1272
01806      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.                    CL*17
01807                                                                   EL1272
01808 ******************************************************************   CL*17
01809 ****      READ THE USER RECORD ON THE "NEXT" COMPANY TO       ****   CL*17
01810 ****      VERIFY THAT A VALID USER RECORD EXISTS:             ****   CL*17
01811 ****        1.  MOVE USER CARRIER/ACCOUNT SECURITY TO PI-AREA ****   CL*17
01812 ****        2.  MOVE USER SECURITY VALUES TO SECURITY CODES   ****   CL*17
01813 ****            IN WORKING STORAGE                                   CL*17
01814 ******************************************************************   CL*17
01815                                                                      CL*17
01816      MOVE WS-NEXT-COMPANY-ID         TO  WS-CFK-COMPANY-ID.          CL*17
01817      MOVE '2'                        TO  WS-CFK-RECORD-TYPE.         CL*17
01818      MOVE PI-PROCESSOR-ID            TO  WS-CFK-ACCESS-TYPE.         CL*17
01819      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.         CL*17
01820                                                                      CL*17
01821      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                       CL*17
01822                                                                      CL*17
01823      IF WS-CNTL-REC-FOUND-SW = 'N'                                   CL*36
01824          PERFORM 8800-INITIALIZE-MAP VARYING EL127B-INDEX            CL*17
01825            FROM +1 BY +1 UNTIL EL127B-INDEX > +8                     CL*36
01826          MOVE ER-0228                TO  EMI-ERROR                   CL*17
01827          MOVE -1                     TO  BSELL                       CL*17
01828          GO TO 8100-SEND-INITIAL-MAP.                                CL*17
01829                                                                      CL*17
01830      MOVE CF-PROCESSOR-CARRIER       TO  PI-CARRIER-SECURITY.        CL*17
01831      MOVE CF-PROCESSOR-ACCOUNT       TO  PI-ACCOUNT-SECURITY.        CL*17
01832      MOVE CF-INDIVIDUAL-APP(1)       TO  SC-CREDIT-CODES.            CL*17
01833      MOVE CF-INDIVIDUAL-APP(2)       TO  SC-CLAIMS-CODES.            CL*17
01834                                                                      CL*17
01835 ******************************************************************   CL*17
01836 ****      READ THE USER RECORD ON THE "NEXT" COMPANY FOR      ****   CL*17
01837 ****      UPDATE AND MOVE THE TERMINAL ID INTO THE RECORD.    ****   CL*17
01838 ******************************************************************   CL*17
01839                                                                      CL*17
01840      MOVE WS-NEXT-COMPANY-ID         TO  WS-CFK-COMPANY-ID.          CL*17
01841      MOVE '2'                        TO  WS-CFK-RECORD-TYPE.         CL*17
01842      MOVE PI-PROCESSOR-ID            TO  WS-CFK-ACCESS-TYPE.         CL*17
01843      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.         CL*17
01844                                                                      CL*17
01845      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.                CL*17
01846                                                                      CL*17
01847      IF WS-CNTL-REC-FOUND-SW = 'N'                                   CL*36
01848          MOVE ER-0228                TO  EMI-ERROR                   CL*17
01849          MOVE -1                     TO  BSELL                       CL*17
01850          GO TO 8200-SEND-DATAONLY.                                   CL*17
01851                                                                      CL*17
01852      MOVE EIBTRMID                   TO  CF-CURRENT-TERM-ON.         CL*17
01853                                                                      CL*17
01854      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.                    CL*17
01855                                                                      CL*17
01856  8600-CONTINUE-NEXT-COMPANY.                                         CL*17
01857 ******************************************************************   CL*17
01858 ****      READ THE NEW COMPANY RECORD TO VERIFY THAT IT       ****   CL*17
01859 ****      EXISTS AND THEN MOVE SPECIFIC DATA TO PI-AREA.      ****   CL*17
01860 ******************************************************************   CL*17
01861                                                                      CL*17
01862      MOVE SPACES                     TO  WS-CONTROL-FILE-KEY.        CL*17
01863      MOVE WS-NEXT-COMPANY-ID         TO  WS-CFK-COMPANY-ID.          CL*17
01864      MOVE '1'                        TO  WS-CFK-RECORD-TYPE.         CL*17
01865      MOVE +0                         TO  WS-CFK-SEQUENCE-NO.         CL*17
01866                                                                      CL*17
01867      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                       CL*17
01868                                                                      CL*17
01869      IF WS-CNTL-REC-FOUND-SW = 'N'                                   CL*36
01870          PERFORM 8800-INITIALIZE-MAP VARYING EL127B-INDEX            CL*17
01871             FROM +1 BY +1 UNTIL EL127B-INDEX > +8                    CL*36
01872          MOVE ER-0089                TO  EMI-ERROR                   CL*17
01873          MOVE -1                     TO  BSELL                       CL*17
01874          GO TO 8100-SEND-INITIAL-MAP.                                CL*17
01875                                                                      CL*17
01876      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.           CL*17
01877                                                                      CL*17
01878      MOVE +1                     TO  PI-START-SW                     CL*17
01879                                      PI-KEY-LENGTH.                  CL*17
01880                                                                      CL*17
01881      MOVE +0                     TO  PI-1ST-TIME-SW                  CL*17
01882                                      PI-LINE-COUNT                   CL*17
01883                                      PI-AIX-RECORD-COUNT             CL*17
01884                                      PI-BROWSE-SW                    CL*17
01885                                      PI-END-OF-FILE                  CL*17
01886                                      PI-TS-ITEM                      CL*17
01887                                      PI-SCREEN-COUNT                 CL*17
01888                                      PI-SUB                          CL*25
01889                                      PI-ALT-NAME-COUNT.              CL*25
01890                                                                      CL*17
01891      MOVE LOW-VALUES             TO  PI-CERT-CONTROLS-EL127 (1)      CL*17
01892                                      PI-CERT-CONTROLS-EL127 (2)      CL*17
01893                                      PI-CERT-CONTROLS-EL127 (3)      CL*17
01894                                      PI-CERT-CONTROLS-EL127 (4)      CL*17
01895                                      PI-CERT-CONTROLS-EL127 (5).     CL*17
01896                                                                      CL*17
01897      MOVE 'ELCERT'                   TO  PI-DSID.                    CL*36
01898      MOVE CF-COMPANY-CD              TO  PI-COMPANY-CD            EL1272
01899                                          PI-CK-COMPANY-CD.           CL*36
01900      MOVE CF-COMPANY-ID              TO  PI-COMPANY-ID.           EL1272
01901      MOVE CF-COMPANY-PASSWORD        TO  PI-COMPANY-PASSWORD.        CL*17
01902      MOVE CF-LGX-CREDIT-USER         TO  PI-CREDIT-USER.          EL1272
01903      MOVE CF-LGX-CLAIM-USER          TO  PI-CLAIM-USER.           EL1272
01904      MOVE CF-CERT-ACCESS-CONTROL     TO  PI-CERT-ACCESS-CONTROL.  EL1272
01905      MOVE CF-CARRIER-CONTROL-LEVEL   TO  PI-CARRIER-CONTROL-LEVEL.EL1272
01906      MOVE CF-JOURNAL-FILE-ID         TO  PI-JOURNAL-FILE-ID.      EL1272
01907      MOVE CF-LOWER-CASE-LETTERS      TO  PI-LOWER-CASE-LETTERS.      CL*17
01908      MOVE CF-CLAIM-PAID-THRU-TO      TO  PI-CLAIM-PAID-THRU-TO.      CL*17
01909                                                                   EL1272
01910      MOVE CF-LIFE-OVERRIDE-L1    TO  PI-LIFE-OVERRIDE-L1.         EL1272
01911      MOVE CF-LIFE-OVERRIDE-L2    TO  PI-LIFE-OVERRIDE-L2.         EL1272
01912      MOVE CF-LIFE-OVERRIDE-L6    TO  PI-LIFE-OVERRIDE-L6.         EL1272
01913      MOVE CF-LIFE-OVERRIDE-L12   TO  PI-LIFE-OVERRIDE-L12.        EL1272
01914                                                                   EL1272
01915      MOVE CF-AH-OVERRIDE-L1      TO  PI-AH-OVERRIDE-L1.           EL1272
01916      MOVE CF-AH-OVERRIDE-L2      TO  PI-AH-OVERRIDE-L2.           EL1272
01917      MOVE CF-AH-OVERRIDE-L6      TO  PI-AH-OVERRIDE-L6.           EL1272
01918      MOVE CF-AH-OVERRIDE-L12     TO  PI-AH-OVERRIDE-L12.          EL1272
01919                                                                   EL1272
01920      IF CREDIT-SESSION                                            EL1272
01921          MOVE CF-CURRENT-MONTH-END                                   CL*29
01922                                  TO  PI-CR-MONTH-END-DT              CL*29
01923          MOVE CF-CAR-GROUP-ACCESS-CNTL                               CL*29
01924                                  TO  PI-CAR-GROUP-ACCESS-CNTL        CL*29
01925          MOVE CF-CR-PRINT-ADDRESS-LABELS                             CL*29
01926                                  TO  PI-LABEL-CONTROL                CL*29
01927      ELSE                                                            CL*29
01928          MOVE CF-PRINT-ADDRESS-LABELS                                CL*29
01929                                  TO  PI-LABEL-CONTROL.               CL*29
01930                                                                   EL1272
01931  8600-EXIT.                                                       EL1272
01932      EXIT.                                                           CL*33
01933                                                                      CL*33
01934  8620-WRITE-TEMP-STORAGE   SECTION.                                  CL*33
01935                                                                      CL*33
01936      EXEC CICS HANDLE CONDITION                                      CL*33
01937          QIDERR   (8625-WRITE-TEMP-STORAGE)                          CL*33
01938      END-EXEC.                                                       CL*33
01939                                                                      CL*33
01940      EXEC CICS DELETEQ TS                                            CL*33
01941          QUEUE  (WS-TEMP-STORAGE-KEY)                                CL*33
01942      END-EXEC.                                                       CL*33
01943                                                                      CL*33
01944  8625-WRITE-TEMP-STORAGE.                                            CL*33
01945                                                                      CL*33
           move +1 to pi-ts-item
01946      MOVE -1             TO  BSELL.                                  CL*33
01947                                                                      CL*33
01948      EXEC CICS WRITEQ TS                                             CL*33
01949           FROM   (EL127BO)                                           CL*33
01950           LENGTH (WS-TS-LENGTH)                                      CL*33
                QUEUE  (WS-TEMP-STORAGE-KEY)                               CL*33
                ITEM   (PI-TS-ITEM)                                        CL*33
           END-EXEC

           if eibaid = DFHPF8
              add +1 to pi-ts-item
              EXEC CICS WRITEQ TS
                 FROM   (PI-PROGRAM-WORK-AREA)
                 LENGTH (WS-WORK-LENGTH)
                 QUEUE  (WS-TEMP-STORAGE-KEY)
                 ITEM   (PI-TS-ITEM)
              END-EXEC
           end-if

           .
01955  8630-EXIT.                                                          CL*33
01956      EXIT.                                                        EL1272
01957                                                                      CL*17
01958  8650-WRITE-SECURITY-TEMP-STORE   SECTION.                        EL1272
01959                                                                   EL1272
01960      EXEC CICS HANDLE CONDITION                                   EL1272
01961          QIDERR   (8651-WRITE-SECURITY)                           EL1272
01962      END-EXEC.                                                    EL1272
01963                                                                   EL1272
01964      MOVE EIBTRMID               TO QID.                          EL1272
01965                                                                   EL1272
01966  8651-WRITE-SECURITY.                                             EL1272
01967                                                                   EL1272
01968      EXEC CICS WRITEQ TS                                          EL1272
01969          QUEUE   (QID)                                            EL1272
01970          FROM    (SECURITY-CONTROL)                               EL1272
01971          LENGTH  (SC-COMM-LENGTH)                                 EL1272
01972          ITEM    (QID-ITEM)                                       EL1272
01973      END-EXEC.                                                    EL1272
01974                                                                   EL1272
01975      MOVE QID                    TO PI-SECURITY-TEMP-STORE-ID.    EL1272
01976                                                                   EL1272
01977      IF PI-PROCESSOR-ID = 'LGXX'                                     CL**3
01978          MOVE ALL 'Y'            TO SC-CREDIT-CODES               EL1272
01979                                     SC-CLAIMS-CODES               EL1272
01980                                     PI-PROCESSOR-USER-ALMIGHTY.   EL1272
01981                                                                   EL1272
01982  8650-EXIT.                                                       EL1272
01983      EXIT.                                                        EL1272
01984                                                                      CL*36
01985      EJECT                                                        EL1272
01986  8700-NOT-FOUND SECTION.                                          EL1272
01987      PERFORM 8800-INITIALIZE-MAP VARYING EL127B-INDEX             EL1272
01988        FROM +1 BY +1 UNTIL EL127B-INDEX > +8.                        CL*36
01989      MOVE -1                     TO BSELL.                        EL1272
01990      MOVE ER-0201                TO EMI-ERROR.                    EL1272
01991      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
01992                                                                   EL1272
01993  8700-EXIT.                                                       EL1272
01994      EXIT.                                                        EL1272
01995                                                                   EL1272
01996  8800-INITIALIZE-MAP SECTION.                                     EL1272
01997      MOVE LOW-VALUES            TO EL127B-MAP-LINE (EL127B-INDEX).EL1272
01998  8800-EXIT.                                                       EL1272
01999      EXIT.                                                        EL1272
02000                                                                      CL*17
02001      EJECT                                                        EL1272
02002  8900-READ-CONTROL SECTION.                                          CL*17
02003                                                                   EL1272
02004      EXEC CICS HANDLE CONDITION                                      CL*17
02005          NOTFND   (8900-NOTFND)                                      CL*17
02006      END-EXEC.                                                       CL*17
02007                                                                      CL*17
02008      EXEC CICS READ                                                  CL*17
02009          DATASET   (WS-CONTROL-FILE-DSID)                            CL*17
02010          RIDFLD    (WS-CONTROL-FILE-KEY)                             CL*17
02011          SET       (ADDRESS OF CONTROL-FILE)                         CL*32
02012      END-EXEC.                                                       CL*17
02013                                                                      CL*17
02014      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.           CL*17
02015      GO TO 8900-EXIT.                                                CL*17
02016                                                                      CL*17
02017  8900-NOTFND.                                                        CL*17
02018      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.           CL*17
02019                                                                      CL*17
02020  8900-EXIT.                                                          CL*17
02021      EXIT.                                                           CL*17
02022                                                                      CL*17
02023  8910-READ-CONTROL-UPDATE.                                           CL*17
02024                                                                      CL*17
02025      EXEC CICS HANDLE CONDITION                                      CL*17
02026          NOTFND   (8910-NOTFND)                                      CL*17
02027      END-EXEC.                                                       CL*17
02028                                                                      CL*17
02029      EXEC CICS READ                                                  CL*17
02030          DATASET   (WS-CONTROL-FILE-DSID)                            CL*17
02031          RIDFLD    (WS-CONTROL-FILE-KEY)                             CL*17
02032          SET       (ADDRESS OF CONTROL-FILE)                         CL*32
02033          UPDATE                                                      CL*17
02034      END-EXEC.                                                       CL*17
02035                                                                      CL*17
02036      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.           CL*17
02037      GO TO 8910-EXIT.                                                CL*17
02038                                                                      CL*17
02039  8910-NOTFND.                                                        CL*17
02040      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.           CL*17
02041                                                                      CL*17
02042  8910-EXIT.                                                          CL*17
02043      EXIT.                                                           CL*17
02044                                                                      CL*17
02045  8920-REWRITE-CONTROL.                                               CL*17
02046                                                                      CL*17
02047      EXEC CICS REWRITE                                               CL*17
02048          DATASET   (WS-CONTROL-FILE-DSID)                            CL*17
02049          FROM      (CONTROL-FILE)                                    CL*17
02050      END-EXEC.                                                       CL*17
02051                                                                      CL*17
02052  8920-EXIT.                                                          CL*17
02053      EXIT.                                                           CL*17
02054                                                                      CL*17
02055      EJECT                                                           CL*17
02056  9000-RETURN-CICS SECTION.                                        EL1272
02057      MOVE 'EL005'                TO  THIS-PGM.                    EL1272
02058      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL1272
02059      GO TO 9300-XCTL.                                             EL1272
02060                                                                   EL1272
02061  9000-EXIT.                                                       EL1272
02062      EXIT.                                                        EL1272
02063                                                                      CL*13
02064  9050-FLA-NAME.                                                      CL*13
02065      MOVE CM-INSURED-LAST-NAME   TO  SAVE-NAME.                      CL*13
02066      MOVE ZERO                   TO  A-SUB.                          CL*13
02067                                                                      CL*36
02068  9050-ADD.                                                           CL*13
02069      ADD 1 TO A-SUB.                                                 CL*13
02070      IF A-SUB > 15                                                   CL*36
02071          GO TO 9050-EXIT-FIX.                                        CL*13
02072                                                                      CL*36
02073      IF SV-BYTE (A-SUB) = ' '                                        CL*13
02074          MOVE A-SUB              TO B-SUB                            CL*13
02075          GO TO 9050-PASS.                                            CL*13
02076                                                                      CL*36
02077      GO TO 9050-ADD.                                                 CL*13
02078                                                                      CL*36
02079  9050-PASS.                                                          CL*13
02080      ADD 1 TO A-SUB.                                                 CL*13
02081      IF A-SUB > 15                                                   CL*36
02082          GO TO 9050-EXIT-FIX.                                        CL*13
02083                                                                      CL*36
02084      IF SV-BYTE (A-SUB) = '*'                                        CL*13
02085          MOVE '*'                TO SV-BYTE (B-SUB)                  CL*13
02086          MOVE ' '                TO SV-BYTE (A-SUB)                  CL*13
02087          GO TO 9050-EXIT-FIX.                                        CL*13
02088                                                                      CL*36
02089      GO TO 9050-PASS.                                                CL*13
02090                                                                      CL*13
02091  9050-EXIT-FIX.                                                      CL*13
02092      MOVE SAVE-NAME              TO CM-INSURED-LAST-NAME.            CL*13
02093                                                                   EL1272
02094  9100-RETURN-TRAN SECTION.                                        EL1272
02095      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL1272
02096      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL1272
02097      MOVE EIBAID                 TO  PI-PREV-AID.                 EL1272
02098                                                                   EL1272
02099      EXEC CICS RETURN                                             EL1272
02100          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1272
02101          LENGTH   (PI-COMM-LENGTH)                                EL1272
02102          TRANSID  (WS-TRANS-ID)                                   EL1272
02103          END-EXEC.                                                EL1272
02104                                                                   EL1272
02105  9100-EXIT.                                                       EL1272
02106      EXIT.                                                        EL1272
02107                                                                   EL1272
02108  9300-XCTL SECTION.                                               EL1272
02109      MOVE DFHENTER               TO  EIBAID.                      EL1272
02110                                                                      CL*22
02111      IF THIS-PGM = 'EL1273'                                          CL*22
02112          MOVE EIBTRMID       TO  WS-TS1-TERM-ID                      CL*22
02113          EXEC CICS WRITEQ TS                                         CL*22
02114              FROM   (PI-PROGRAM-WORK-AREA)                           CL*22
02115              LENGTH (WS-WORK-LENGTH)                                 CL*22
02116              QUEUE  (WS-EL1273-TS)                                   CL*22
02117          END-EXEC.                                                   CL*22
02118                                                                   EL1272
02119      EXEC CICS XCTL                                               EL1272
02120          PROGRAM  (THIS-PGM)                                      EL1272
02121          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1272
02122          LENGTH   (PI-COMM-LENGTH)                                EL1272
02123      END-EXEC.                                                       CL*36
02124                                                                   EL1272
02125  9300-EXIT.                                                       EL1272
02126      EXIT.                                                        EL1272
02127                                                                   EL1272
02128      EJECT                                                        EL1272
02129  9400-CLEAR SECTION.                                              EL1272
02130      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.                    EL1272
02131      GO TO 9300-XCTL.                                             EL1272
02132                                                                   EL1272
02133  9400-EXIT.                                                       EL1272
02134      EXIT.                                                        EL1272
02135                                                                   EL1272
02136  9600-PGMIDERR SECTION.                                           EL1272
02137      EXEC CICS HANDLE CONDITION                                   EL1272
02138          PGMIDERR (8300-SEND-TEXT)                                EL1272
02139      END-EXEC.                                                       CL*36
02140                                                                   EL1272
02141      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL1272
02142                                      LOGOFF-PGM.                  EL1272
02143      MOVE 'EL005'                TO  THIS-PGM.                    EL1272
02144      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL1272
02145      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL1272
02146      GO TO 9300-XCTL.                                             EL1272
02147                                                                   EL1272
02148  9600-EXIT.                                                       EL1272
02149      EXIT.                                                        EL1272
02150                                                                   EL1272
02151      EJECT                                                        EL1272
02152  9900-ERROR-FORMAT SECTION.                                       EL1272
02153                                                                   EL1272
02154      EXEC CICS LINK                                               EL1272
02155          PROGRAM  ('EL001')                                       EL1272
02156          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL1272
02157          LENGTH   (EMI-COMM-LENGTH)                               EL1272
02158      END-EXEC.                                                       CL*36
02159                                                                   EL1272
02160  9900-EXIT.                                                       EL1272
02161      EXIT.                                                        EL1272
02162                                                                   EL1272
02163  9990-ERROR SECTION.                                              EL1272
02164      MOVE DFHEIBLK               TO EMI-LINE1.                    EL1272
02165                                                                      CL*36
02166      EXEC CICS LINK                                               EL1272
02167          PROGRAM  ('EL004')                                       EL1272
02168          COMMAREA (EMI-LINE1)                                     EL1272
02169          LENGTH   (72)                                            EL1272
02170      END-EXEC.                                                       CL*36
02171                                                                      CL*36
02172      MOVE -1                     TO BSELL.                        EL1272
02173      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
02174                                                                   EL1272
