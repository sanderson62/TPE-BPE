00001  IDENTIFICATION DIVISION.                                         10/07/97
00002                                                                   EL107
00003  PROGRAM-ID.                 EL107 .                                 LV021
00004 *              PROGRAM CONVERTED BY                                  CL*18
************************
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*18
00006 *              CONVERSION DATE 09/27/95 10:01:12.                    CL*18
00007 *                            VMOD=2.021.                             CL*21
00008 *                                                                 EL107
00008 *                                                                 EL107
00009 *AUTHOR.    LOGIC, INC.                                              CL*18
00010 *           DALLAS, TEXAS.                                           CL*18
00011                                                                   EL107
00012 *DATE-COMPILED.                                                      CL*18
00013                                                                   EL107
00014 *SECURITY.   *****************************************************   CL*18
00015 *            *                                                   *   CL*18
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*18
00017 *            *                                                   *   CL*18
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*18
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*18
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*18
00021 *            *                                                   *   CL*18
00022 *            *****************************************************   CL*18
00023                                                                   EL107
00024 *REMARKS.                                                            CL**9
00025 *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED      CL**9
00026 *    FOR THE BENEFIT CONTROL RECORDS.                                CL**9
00027 *                                                                    CL**9
00028 *    SCREENS     - EL107A - BENEFIT CONTROLS                         CL**9
00029 *                                                                    CL**9
00030 *    ENTERED BY  - EL101 - MAINTENANCE MENU                          CL**9
00031 *                                                                    CL**9
00032 *    EXIT TO     - EL101 - MAINTENANCE MENU                          CL**9
00033 *                                                                    CL**9
00034 *    INPUT FILE  - ELCNTL - CONTROL FILE - BENEFIT RECORDS           CL**9
00035 *                                                                    CL**9
00036 *    OUTPUT FILE - ELCNTL - CONTROL FILE - BENEFIT RECORDS           CL**9
00037 *                                                                    CL**9
00038 *    COMMAREA    - PASSED                                            CL**9
00039 *                                                                    CL**9
00040 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON        CL**9
00041 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE    CL**9
00042 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE   CL**9
00043 *                  ENTRIES (XCTL FROM CICS VIA EX11) THE SCREEN      CL**9
00044 *                  WILL BE READ AND ACTION WILL BE BASED ON THE      CL**9
00045 *                  MAINTENANCE TYPE INDICATED.                       CL**9
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 200 TO 450
082503* 082503                   PEMA  ADD BENEFIT CATEGORY
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
051414* 051414    2013100100002  PEMA  Add maximum benefits/crit period
081214* 081214    2014081200001  PEMA  correct max bens init.
092602******************************************************************
00046      EJECT                                                        EL107
00047  ENVIRONMENT DIVISION.                                            EL107
00048  DATA DIVISION.                                                   EL107
00049  WORKING-STORAGE SECTION.                                         EL107
00050  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.      CL*18
00051                                                                   EL107
00052  77  FILLER  PIC X(32)  VALUE '********************************'. EL107
00053  77  FILLER  PIC X(32)  VALUE '*   EL107  WORKING STORAGE     *'. EL107
00054  77  FILLER  PIC X(32)  VALUE '***********VMOD=2.021 **********'.    CL*21
00055                                                                   EL107
00056      COPY ELCSCTM.                                                   CL*12
00057      COPY ELCSCRTY.                                                  CL*12
00058                                                                   EL107
00059  01  WS-DATE-AREA.                                                EL107
00060      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL107
00061      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL107
00062                                                                   EL107
00063  01  FILLER                          COMP-3.                      EL107
092602*    05  WS-MAX-BEN-CODES            PIC S999    VALUE +200.         CL*21
092602     05  WS-MAX-BEN-CODES            PIC S999    VALUE +450.         CL*21
00065      05  WS-RECORD-COUNT             PIC S9(3)   VALUE ZERO.      EL107
00066      05  WS-READNEXT-SW              PIC S9      VALUE ZERO.      EL107
00067      05  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.      EL107
00068      05  WS-UPDATE-SW                PIC S9      VALUE ZERO.      EL107
00069      05  WS-COMPLETED-SUCCESSFUL     PIC S9      VALUE ZERO.      EL107
00070        88  TRANSACTION-SUCCESSFUL                    VALUE +1.    EL107
00071        88  INITIAL-TRANSACTION                       VALUE +2.       CL**6
00072        88  CHANGE-SUCCESSFUL                         VALUE +3.       CL**6
00073                                                                   EL107
00074      05  TIME-IN                     PIC S9(7)   VALUE ZERO.      EL107
00075      05  TIME-OUT                    REDEFINES                    EL107
00076          TIME-IN                     PIC S9(3)V9(4).              EL107
00077                                                                   EL107
00078  01  FILLER                          COMP SYNC.                   EL107
00079      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL107
00080      05  WS-JOURNAL-FILE-ID          PIC S9(4)   VALUE +1.        EL107
00081      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)   VALUE +773.         CL*11
00082                                                                   EL107
00083  01  FILLER.                                                      EL107
00084      05  WS-CONTROL-FILE-KEY.                                     EL107
00085          10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.    EL107
00086          10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.    EL107
00087          10  FILLER                  PIC XX      VALUE SPACES.    EL107
00088          10  WS-CFK-BENEFIT-NO       PIC XX      VALUE SPACES.    EL107
00089          10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP. EL107
00090                                                                   EL107
00091      05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL107S'.    EL107
00092      05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL107A'.    EL107
00093                                                                   EL107
00094      05  FILLER                      REDEFINES                    EL107
00095          WS-MAP-NAME.                                             EL107
00096          10  FILLER                  PIC XX.                      EL107
00097          10  WS-MAP-NUMBER           PIC X(4).                    EL107
00098          10  FILLER                  PIC XX.                         CL*18
00099                                                                   EL107
00100      05  WS-EDIT-BENE-CODE           PIC XX    VALUE SPACE.          CL**3
00101          88  INVALID-BENEFIT-CODE    VALUE '  ' '00'                 CL**3
00102                                            '90' THRU '99'.           CL**3
00103                                                                      CL**3
00104      05  WS-EDIT-LF-OB               PIC X     VALUE SPACE.          CL*17
00105          88  VALID-LF-OB             VALUE ' ' 'A' 'C' 'D' 'E'       CL*17
00106                                            'F' 'G' 'I' 'J' 'K'    EL107
00107                                            'L' 'M' 'O' 'P' 'S'    EL107
00108                                            'T' 'U' 'V' 'W' 'X'    EL107
00109                                            'Z' 'H' '2' '3' '4'       CL*16
00110                                            'N'.                      CL*16
00111                                                                      CL*17
00112      05  WS-EDIT-AH-OB               PIC X     VALUE SPACE.          CL*17
00113          88  VALID-AH-OB             VALUE ' ' 'A' 'C' 'E' 'F'       CL*17
00114                                            'G' 'H' 'I' 'J' 'K'       CL*17
00115                                            'M' 'N' 'O' 'P' 'Q'       CL*17
00116                                            'S' 'Z' '2' '3' '4'.      CL*17
00117                                                                   EL107
00118      05  WS-EDIT-IG-CODE             PIC X     VALUE SPACE.       EL107
00119          88  VALID-IG-CODE           VALUE 'I' 'G' ' '.           EL107
00120                                                                   EL107
00121      05  WS-EDIT-REFUND              PIC X     VALUE SPACE.       EL107
00122          88  VALID-REFUND-METHOD     VALUE '1' '2' '3' '4'        EL107
00123                                            '5' '6' '7' '8' '9'       CL*20
033104                                           ' ' 'G' 'D' 'S'.          CL*20
00125                                                                   EL107
00126      05  WS-EDIT-OVRD-EARNINGS       PIC X     VALUE SPACE.       EL107
00127          88  VALID-OVRD-EARNINGS     VALUE '1' '2' '3' '4'        EL107
00128                                            '5' '6' '8' 'B' ' '.   EL107
00129                                                                   EL107
00130      05  THIS-PGM                    PIC X(8)  VALUE 'EL107'.     EL107
00131                                                                   EL107
00132      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.     EL107
00133                                                                   EL107
00134      05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.      EL107
00135                                                                   EL107
00136      05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      EL107
00137      05  WS-SPACE                    PIC X       VALUE SPACE.     EL107
00138                                                                   EL107
00139      05  WS-TRANS-ID                 PIC X(4)    VALUE 'EX11'.    EL107
00140                                                                   EL107
00141      05  WS-TEMP-STORAGE-KEY.                                     EL107
00142          10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.    EL107
00143          10  FILLER                  PIC X(4)    VALUE '107'.     EL107
00144                                                                   EL107
082503     05  WS-BENEFIT-CONTROLS-SAVE    PIC X(53).                      CL**3
00146                                                                      CL**3
00147      05  WS-BENEFIT-CONTROLS-WORK.                                EL107
00148          10  WS-BENEFIT-NUMBER           PIC XX.                  EL107
00149          10  WS-BENEFIT-ABBREVIATION     PIC X(3).                   CL*18
00150          10  WS-BENEFIT-DESCRIPTION      PIC X(10).               EL107
00151          10  WS-BENEFIT-COMMENT          PIC X(10).               EL107
00152          10  WS-BENEFIT-COVERAGE-TYPE    PIC X.                   EL107
00153          10  WS-BENEFIT-OUTSTANDING-BAL  PIC X.                   EL107
00154          10  WS-BENEFIT-JOINT-COVERAGE   PIC X.                   EL107
051414         10  WS-BENEFIT-MAX-BENS         PIC s999 comp-3.
051414         10  FILLER                      PIC X(09).               EL107
082503         10  WS-BENEFIT-CATEGORY         PIC X.
00156          10  WS-BENEFIT-LOAN-TYPE        PIC X(8).                EL107
00157          10  WS-BENEFIT-REMAIN-TERM      PIC X.                   EL107
00158          10  WS-BENEFIT-EARN-METHOD      PIC X.                   EL107
00159          10  WS-BENEFIT-REFUND-METHOD    PIC X.                   EL107
00160          10  FILLER                      PIC X.                      CL**2
00161          10  WS-BENEFIT-IG-CODE          PIC X.                   EL107
00162                                                                   EL107
00163      05  WS-BENEFIT-TABLE-AREA.                                   EL107
092602         10  WS-BENEFIT-TABLE-ENTRY  OCCURS 450 TIMES                CL**3
00165              INDEXED BY BENEFIT-INDEX.                            EL107
00166              15  WS-BTE-NUMBER       PIC XX.                      EL107
082503             15  FILLER              PIC X(53).                   EL107
00168                                                                   EL107
00169      EJECT                                                        EL107
00170      05  WS-ERROR-MESSAGE-AREA.                                   EL107
00171          10  ER-0000                 PIC 9(4)   VALUE 0000.       EL107
00172          10  ER-0004                 PIC 9(4)   VALUE 0004.       EL107
00173          10  ER-0006                 PIC 9(4)   VALUE 0006.       EL107
00174          10  ER-0008                 PIC 9(4)   VALUE 0008.       EL107
00175          10  ER-0022                 PIC 9(4)   VALUE 0022.       EL107
00176          10  ER-0023                 PIC 9(4)   VALUE 0023.       EL107
00177          10  ER-0024                 PIC 9(4)   VALUE 0024.       EL107
00178          10  ER-0025                 PIC 9(4)   VALUE 0025.       EL107
00179          10  ER-0026                 PIC 9(4)   VALUE 0026.       EL107
00180          10  ER-0027                 PIC 9(4)   VALUE 0027.       EL107
00181          10  ER-0028                 PIC 9(4)   VALUE 0028.       EL107
00182          10  ER-0029                 PIC 9(4)   VALUE 0029.       EL107
00183          10  ER-0036                 PIC 9(4)   VALUE 0036.       EL107
00184          10  ER-0037                 PIC 9(4)   VALUE 0037.       EL107
00185          10  ER-0038                 PIC 9(4)   VALUE 0038.       EL107
00186          10  ER-0039                 PIC 9(4)   VALUE 0039.       EL107
00187          10  ER-0040                 PIC 9(4)   VALUE 0040.       EL107
00188          10  ER-0068                 PIC 9(4)   VALUE 0068.       EL107
00189          10  ER-0070                 PIC 9(4)   VALUE 0070.       EL107
00190          10  ER-0127                 PIC 9(4)   VALUE 0127.       EL107
00191          10  ER-0128                 PIC 9(4)   VALUE 0128.       EL107
00192          10  ER-0129                 PIC 9(4)   VALUE 0129.       EL107
00193          10  ER-0138                 PIC 9(4)   VALUE 0138.       EL107
00194          10  ER-0139                 PIC 9(4)   VALUE 0139.       EL107
00195          10  ER-0582                 PIC 9(4)   VALUE 0582.       EL107
00196          10  ER-0592                 PIC 9(4)   VALUE 0592.          CL**3
00197          10  ER-0595                 PIC 9(4)   VALUE 0595.       EL107
00198          10  ER-0596                 PIC 9(4)   VALUE 0596.       EL107
00199          10  ER-0640                 PIC 9(4)   VALUE 0640.       EL107
00200          10  ER-0752                 PIC 9(4)   VALUE 0752.          CL*21
00201          10  ER-0847                 PIC 9(4)   VALUE 0847.          CL*17
00202          10  ER-7535                 PIC 9(4)   VALUE 7535.
051414         10  er-7537                 pic 9(4)   value 7537.
00203          10  ER-8318                 PIC 9(4)   VALUE 8318.          CL*18
00204                                                                   EL107
00205      05  WS-HEADING-0.                                            EL107
00206          10  FILLER                  PIC X      VALUE '('.        EL107
00207          10  WS-HD0-LF-L1            PIC X.                       EL107
00208          10  FILLER                  PIC X(4)   VALUE ' OR '.     EL107
00209          10  WS-HD0-AH-L1            PIC X.                       EL107
00210          10  FILLER                  PIC X(19)                    EL107
00211                                       VALUE ')   BENEFIT CODE  :'.EL107
00212      EJECT                                                        EL107
00213      COPY ELCINTF.                                                   CL*12
00214      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL107
00215          16  PI-1ST-TIME-SW          PIC S9     COMP-3.           EL107
00216          16  PI-MODE                 PIC X.                       EL107
00217          16  PI-BENEFIT-TYPE         PIC X.                       EL107
00218          16  PI-BENEFIT-NUMBER       PIC XX.                      EL107
00219          16  PI-LAST-BENEFIT-NUMBER  PIC XX.                      EL107
00220          16  PI-NEXT-BENEFIT-NUMBER  PIC XX.                      EL107
00221          16  PI-LINE-COUNT           PIC S9(3)  COMP-3.           EL107
00222          16  PI-BROWSE-SW            PIC S9     COMP-3.           EL107
00223          16  PI-LOGIC-CUSTOMER       PIC X.                       EL107
00224          16  PI-SHOW-SW              PIC S9     COMP-3.           EL107
00225          16  PI-CHANGE-SW            PIC S9     COMP-3.              CL**6
00226          16  PI-UPDATE-KEY           PIC X(10).                      CL**6
00227                                                                      CL*18
00228          16  FILLER                  PIC X(615).                     CL*18
00229                                                                   EL107
00230      EJECT                                                        EL107
00231                                                                   EL107
00232      COPY EL107S.                                                    CL*12
00233                                                                   EL107
00234  01  EL107AO-R REDEFINES EL107AI.                                 EL107
00235      05  FILLER                      PIC X(117).                  EL107
00236      05  WS-MAP-LINE                 OCCURS 8 TIMES               EL107
00237                                      INDEXED BY EL107A-INDEX.     EL107
00238          10  EL107A-CODE-LENGTH      PIC S9(4)  COMP.             EL107
00239          10  EL107A-CODE-ATTRB       PIC X.                       EL107
00240          10  EL107A-CODE-O           PIC X(2).                    EL107
00241          10  EL107A-CODE-I           REDEFINES                    EL107
00242              EL107A-CODE-O           PIC X(2).                    EL107
00243          10  EL107A-ABBR-LENGTH      PIC S9(4)   COMP.            EL107
00244          10  EL107A-ABBR-ATTRB       PIC X.                       EL107
00245          10  EL107A-ABBR-O           PIC X(3).                    EL107
00246          10  EL107A-ABBR-I           REDEFINES                    EL107
00247              EL107A-ABBR-O           PIC X(3).                    EL107
00248          10  FILLER                  REDEFINES                    EL107
00249              EL107A-ABBR-O.                                       EL107
00250              15  EL107A-ABBR-CHAR-12 PIC XX.                      EL107
00251              15  EL107A-ABBR-CHAR-3  PIC X.                       EL107
00252          10  EL107A-DESC-LENGTH      PIC S9(4)   COMP.            EL107
00253          10  EL107A-DESC-ATTRB       PIC X.                       EL107
00254          10  EL107A-DESC-O           PIC X(10).                   EL107
00255          10  EL107A-DESC-I           REDEFINES                    EL107
00256              EL107A-DESC-O           PIC X(10).                   EL107
00257          10  EL107A-COMMENT-LENGTH   PIC S9(4)   COMP.            EL107
00258          10  EL107A-COMMENT-ATTRB    PIC X.                       EL107
00259          10  EL107A-COMMENT-O        PIC X(10).                   EL107
00260          10  EL107A-COMMENT-I        REDEFINES                    EL107
00261              EL107A-COMMENT-O        PIC X(10).                   EL107
051414         10  EL107A-MAX-BENS-LENGTH  PIC S9(4)   COMP.
051414         10  EL107A-MAX-BENS-ATTRB   PIC X.
051414         10  EL107A-MAX-BENS-O       PIC 99.
051414         10  EL107A-MAX-BENS-I       REDEFINES
051414             EL107A-MAX-BENS-O       PIC XX.
00262          10  EL107A-LOAN-LENGTH      PIC S9(4)   COMP.            EL107
00263          10  EL107A-LOAN-ATTRB       PIC X.                       EL107
00264          10  EL107A-LOAN-O           PIC X(8).                    EL107
00265          10  EL107A-LOAN-I           REDEFINES                    EL107
00266              EL107A-LOAN-O           PIC X(8).                    EL107
00267          10  EL107A-EM-LENGTH        PIC S9(4)   COMP.            EL107
00268          10  EL107A-EM-ATTRB         PIC X.                       EL107
00269          10  EL107A-EM-O             PIC X.                       EL107
00270          10  EL107A-EM-I             REDEFINES                    EL107
00271              EL107A-EM-O             PIC X.                       EL107
00272          10  EL107A-JOINT-LENGTH     PIC S9(4)   COMP.            EL107
00273          10  EL107A-JOINT-ATTRB      PIC X.                       EL107
00274          10  EL107A-JOINT-O          PIC X.                       EL107
00275          10  EL107A-JOINT-I          REDEFINES                    EL107
00276              EL107A-JOINT-O          PIC X.                       EL107
00277          10  EL107A-OB-LENGTH        PIC S9(4)   COMP.            EL107
00278          10  EL107A-OB-ATTRB         PIC X.                       EL107
00279          10  EL107A-OB-O             PIC X.                       EL107
00280          10  EL107A-OB-I             REDEFINES                    EL107
00281              EL107A-OB-O             PIC X.                       EL107
00282          10  EL107A-LOD-LENGTH       PIC S9(4)   COMP.            EL107
00283          10  EL107A-LOD-ATTRB        PIC X.                       EL107
00284          10  EL107A-LOD-O            PIC X.                       EL107
00285          10  EL107A-LOD-I            REDEFINES                    EL107
00286              EL107A-LOD-O            PIC X.                       EL107
00287          10  EL107A-RTM-LENGTH       PIC S9(4)   COMP.            EL107
00288          10  EL107A-RTM-ATTRB        PIC X.                       EL107
00289          10  EL107A-RTM-O            PIC X.                       EL107
00290          10  EL107A-RTM-I            REDEFINES                    EL107
00291              EL107A-RTM-O            PIC X.                       EL107
00292          10  EL107A-RFM-LENGTH       PIC S9(4)   COMP.            EL107
00293          10  EL107A-RFM-ATTRB        PIC X.                       EL107
00294          10  EL107A-RFM-O            PIC X.                       EL107
00295          10  EL107A-RFM-I            REDEFINES                    EL107
00296              EL107A-RFM-O            PIC X.                       EL107
00297          10  EL107A-IGC-LENGTH       PIC S9(4)   COMP.            EL107
00298          10  EL107A-IGC-ATTRB        PIC X.                       EL107
00299          10  EL107A-IGC-O            PIC X.                       EL107
00300          10  EL107A-IGC-I            REDEFINES                    EL107
00301              EL107A-IGC-O            PIC X.                       EL107
00297          10  EL107A-CAC-LENGTH       PIC S9(4)   COMP.            EL107
00298          10  EL107A-CAC-ATTRB        PIC X.                       EL107
00299          10  EL107A-CAC-O            PIC X.                       EL107
00300          10  EL107A-CAC-I            REDEFINES                    EL107
00301              EL107A-CAC-O            PIC X.                       EL107
00302                                                                   EL107
00303      EJECT                                                        EL107
00304      COPY ELCJPFX.                                                   CL*12
00305                                      PIC X(750).                     CL*11
00306      EJECT                                                        EL107
00307      COPY ELCEMIB.                                                   CL*12
00308                                                                   EL107
00309      EJECT                                                        EL107
00310      COPY ELCDATE.                                                   CL*12
00311                                                                   EL107
00312      EJECT                                                        EL107
00313      COPY ELCLOGOF.                                                  CL*12
00314                                                                   EL107
00315      EJECT                                                        EL107
00316      COPY ELCATTR.                                                   CL*12
00317                                                                   EL107
00318      EJECT                                                        EL107
00319      COPY ELCAID.                                                    CL*12
00320                                                                   EL107
00321  01  FILLER    REDEFINES DFHAID.                                  EL107
00322      05  FILLER                      PIC X(8).                    EL107
00323      05  PF-VALUES                   PIC X                        EL107
00324          OCCURS 24 TIMES.                                         EL107
00325                                                                   EL107
00326      EJECT                                                        EL107
00327  LINKAGE SECTION.                                                 EL107
00328  01  DFHCOMMAREA                     PIC X(1024).                 EL107
00329                                                                   EL107
00330 *01 DFHBLLDS                         COMP SYNC.                      CL*18
00331 *    05  BLLCBAR                     PIC S9(8).                      CL*18
00332 *    05  CFCBAR                      PIC S9(8).                      CL*18
00333                                                                   EL107
00334      EJECT                                                        EL107
00335      COPY ELCCNTL.                                                   CL*12
00336                                                                   EL107
00337      EJECT                                                        EL107
00338  PROCEDURE DIVISION.                                              EL107
00339                                                                   EL107
00340      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL107
00341      MOVE '5'                   TO DC-OPTION-CODE.                EL107
00342      PERFORM 8500-DATE-CONVERSION.                                EL107
00343      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL107
00344      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL107
00345                                                                   EL107
00346      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.      EL107
00347                                                                   EL107
00348      MOVE +2                    TO  EMI-NUMBER-OF-LINES           EL107
00349                                     EMI-SWITCH2.                  EL107
00350                                                                   EL107
00351      IF EIBCALEN NOT GREATER ZERO                                 EL107
00352          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL107
00353          GO TO 8300-SEND-TEXT.                                    EL107
00354                                                                   EL107
00355      EXEC CICS HANDLE CONDITION                                   EL107
00356          PGMIDERR (9600-PGMIDERR)                                 EL107
00357          ERROR    (9990-ERROR)                                    EL107
00358      END-EXEC.                                                    EL107
00359                                                                   EL107
00360      EJECT                                                        EL107
00361      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL107
00362          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL107
00363              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL107
00364              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL107
00365              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL107
00366              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL107
00367              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL107
00368              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL107
00369              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL107
00370              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     EL107
00371            ELSE                                                   EL107
00372              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL107
00373              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL107
00374              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL107
00375              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL107
00376              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL107
00377              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL107
00378              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL107
00379              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.       CL**6
00380                                                                   EL107
00381      IF EIBTRNID NOT EQUAL WS-TRANS-ID                               CL**6
00382          GO TO 1000-INITIAL-SCREEN.                                  CL**6
00383                                                                   EL107
00384      IF EIBAID = DFHCLEAR                                         EL107
00385          GO TO 9400-CLEAR.                                        EL107
00386                                                                   EL107
00387      IF NOT SYSTEM-DISPLAY-CAP                                    EL107
00388          MOVE 'READ'         TO SM-READ                           EL107
00389          PERFORM 9995-SECURITY-VIOLATION                          EL107
00390          MOVE ER-0070        TO EMI-ERROR                         EL107
00391          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL107
00392          PERFORM 8100-SEND-INITIAL-MAP                            EL107
00393          GO TO 9100-RETURN-TRAN.                                  EL107
00394                                                                   EL107
00395      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL107
00396          MOVE LOW-VALUES         TO  EL107AO                      EL107
00397          MOVE -1                 TO  APFKL                        EL107
00398          MOVE ER-0008            TO  EMI-ERROR                    EL107
00399          PERFORM 8200-SEND-DATAONLY                               EL107
00400          GO TO 9100-RETURN-TRAN.                                  EL107
00401                                                                   EL107
00402      EXEC CICS RECEIVE                                            EL107
00403          INTO   (EL107AO)                                         EL107
00404          MAPSET (WS-MAPSET-NAME)                                  EL107
00405          MAP    (WS-MAP-NAME)                                     EL107
00406      END-EXEC.                                                    EL107
00407                                                                   EL107
00408      IF APFKL GREATER ZERO                                        EL107
00409          IF EIBAID NOT = DFHENTER                                 EL107
00410              MOVE ER-0004        TO  EMI-ERROR                    EL107
00411              MOVE AL-UNBOF       TO  APFKA                        EL107
00412              MOVE -1             TO  APFKL                        EL107
00413              PERFORM 8200-SEND-DATAONLY                           EL107
00414              GO TO 9100-RETURN-TRAN                               EL107
00415            ELSE                                                   EL107
00416              IF APFKO NUMERIC   AND                               EL107
00417                (APFKO GREATER ZERO AND LESS '25')                 EL107
00418                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL107
00419                ELSE                                               EL107
00420                  MOVE ER-0029        TO  EMI-ERROR                EL107
00421                  MOVE AL-UNBOF       TO  APFKA                    EL107
00422                  MOVE -1             TO  APFKL                    EL107
00423                  PERFORM 8200-SEND-DATAONLY                       EL107
00424                  GO TO 9100-RETURN-TRAN.                          EL107
00425                                                                   EL107
00426      IF EIBAID = DFHPF12                                          EL107
00427          MOVE 'EL010'         TO  THIS-PGM                        EL107
00428          GO TO 9300-XCTL.                                         EL107
00429                                                                   EL107
00430      IF EIBAID = DFHPF23                                          EL107
00431          GO TO 9000-RETURN-CICS.                                  EL107
00432                                                                   EL107
00433      IF EIBAID = DFHPF24                                          EL107
00434         IF CREDIT-SESSION                                         EL107
00435              MOVE 'EL626'     TO  THIS-PGM                        EL107
00436              GO TO 9300-XCTL                                      EL107
00437          ELSE                                                     EL107
00438              MOVE 'EL126'     TO  THIS-PGM                        EL107
00439              GO TO 9300-XCTL.                                     EL107
00440                                                                   EL107
00441      IF EIBAID = DFHENTER OR DFHPF1                               EL107
00442          NEXT SENTENCE                                            EL107
00443        ELSE                                                       EL107
00444          MOVE ER-0008            TO  EMI-ERROR                    EL107
00445          MOVE -1                 TO  APFKL                        EL107
00446          PERFORM 8200-SEND-DATAONLY                               EL107
00447          GO TO 9100-RETURN-TRAN.                                  EL107
00448                                                                   EL107
00449      EJECT                                                        EL107
00450      IF AMAINTL GREATER ZERO                                      EL107
00451          IF AMAINTI = 'A' OR 'C' OR 'D' OR 'S'                    EL107
00452              MOVE AL-UANON       TO  AMAINTA                      EL107
00453              MOVE AMAINTI        TO  PI-MODE                      EL107
00454            ELSE                                                   EL107
00455              MOVE AL-UABOF       TO  AMAINTA                      EL107
00456              MOVE -1             TO  AMAINTL                      EL107
00457              MOVE ER-0023        TO  EMI-ERROR                    EL107
00458              PERFORM 9900-ERROR-FORMAT                            EL107
00459        ELSE                                                       EL107
00460          IF EIBAID = DFHPF1                                       EL107
00461              MOVE 'S'            TO  PI-MODE                      EL107
00462            ELSE                                                   EL107
00463              IF PI-1ST-TIME-SW NOT = ZERO                         EL107
00464                  NEXT SENTENCE                                    EL107
00465                ELSE                                               EL107
00466                  MOVE AL-UABOF   TO  AMAINTA                      EL107
00467                  MOVE -1         TO  AMAINTL                      EL107
00468                  MOVE ER-0023    TO  EMI-ERROR                    EL107
00469                  PERFORM 9900-ERROR-FORMAT.                       EL107
00470                                                                   EL107
00471      IF SYSTEM-MODIFY-CAP                                         EL107
00472          NEXT SENTENCE                                            EL107
00473        ELSE                                                       EL107
00474          IF AMAINTI = 'A' OR 'C' OR 'D'                           EL107
00475          MOVE 'UPDATE'           TO SM-READ                       EL107
00476          PERFORM 9995-SECURITY-VIOLATION                          EL107
00477          MOVE ER-0070            TO EMI-ERROR                     EL107
00478          MOVE -1                 TO  AMAINTL                      EL107
00479          MOVE AL-UABON           TO  AMAINTA                      EL107
00480          PERFORM 8200-SEND-DATAONLY                               EL107
00481          GO TO 9100-RETURN-TRAN.                                  EL107
00482                                                                   EL107
00483      IF AKINDL GREATER ZERO                                       EL107
00484          IF AKINDI = PI-LIFE-OVERRIDE-L1  OR  PI-AH-OVERRIDE-L1   EL107
00485              MOVE AL-UANON       TO  AKINDA                       EL107
00486              IF (AKINDI = PI-LIFE-OVERRIDE-L1  AND                EL107
00487                                     PI-BENEFIT-TYPE NOT = '4') OR EL107
00488                 (AKINDI = PI-AH-OVERRIDE-L1  AND                  EL107
00489                                     PI-BENEFIT-TYPE NOT = '5')    EL107
00490                  MOVE SPACES     TO  PI-BENEFIT-NUMBER            EL107
00491                                      PI-NEXT-BENEFIT-NUMBER       EL107
00492                  MOVE ZERO       TO  PI-BROWSE-SW                 EL107
00493                  IF AKINDI = PI-LIFE-OVERRIDE-L1                  EL107
00494                      MOVE '4' TO PI-BENEFIT-TYPE                  EL107
00495                    ELSE                                           EL107
00496                      MOVE '5' TO PI-BENEFIT-TYPE                  EL107
00497                ELSE                                               EL107
00498                  IF AKINDI = PI-LIFE-OVERRIDE-L1                  EL107
00499                      MOVE '4' TO PI-BENEFIT-TYPE                  EL107
00500                    ELSE                                           EL107
00501                      MOVE '5' TO PI-BENEFIT-TYPE                  EL107
00502            ELSE                                                   EL107
00503              MOVE AL-UABOF       TO  AKINDA                       EL107
00504              MOVE -1             TO  AKINDL                       EL107
00505              MOVE ER-0024        TO  EMI-ERROR                    EL107
00506              PERFORM 9900-ERROR-FORMAT                            EL107
00507        ELSE                                                       EL107
00508          IF EIBAID = DFHPF1                                       EL107
00509              MOVE '4'            TO  PI-BENEFIT-TYPE              EL107
00510            ELSE                                                   EL107
00511              IF PI-1ST-TIME-SW NOT = ZERO                         EL107
00512                  NEXT SENTENCE                                    EL107
00513                ELSE                                               EL107
00514                  MOVE AL-UABOF   TO  AKINDA                       EL107
00515                  MOVE -1         TO  AKINDL                       EL107
00516                  MOVE ER-0024    TO  EMI-ERROR                    EL107
00517                  PERFORM 9900-ERROR-FORMAT.                       EL107
00518                                                                   EL107
00519      IF ABENEL GREATER ZERO  OR                                   EL107
00520         AMAINTI = 'D' OR 'C'                                         CL**6
00521          MOVE ABENEI             TO WS-EDIT-BENE-CODE                CL**3
00522          IF NOT INVALID-BENEFIT-CODE                                 CL**3
00523              MOVE AL-UANON       TO  ABENEA                          CL**5
00524              MOVE ABENEI         TO  PI-BENEFIT-NUMBER            EL107
00525                                      PI-NEXT-BENEFIT-NUMBER       EL107
00526              MOVE +1             TO  PI-BROWSE-SW                 EL107
00527            ELSE                                                   EL107
00528              MOVE AL-UABOF       TO  ABENEA                          CL**5
00529              MOVE -1             TO  ABENEL                       EL107
00530              MOVE ER-0025        TO  EMI-ERROR                    EL107
00531              PERFORM 9900-ERROR-FORMAT                            EL107
00532        ELSE                                                       EL107
00533          IF PI-1ST-TIME-SW NOT = ZERO                             EL107
00534              NEXT SENTENCE                                        EL107
00535            ELSE                                                   EL107
00536              MOVE SPACES         TO  PI-BENEFIT-NUMBER.              CL**3
00537                                                                   EL107
00538      IF EMI-FATAL-CTR GREATER ZERO                                EL107
00539          PERFORM 8200-SEND-DATAONLY                               EL107
00540          GO TO 9100-RETURN-TRAN.                                  EL107
00541                                                                   EL107
00542      MOVE +1                     TO  PI-1ST-TIME-SW.              EL107
00543                                                                   EL107
00544      IF PI-MODE NOT EQUAL 'C'                                        CL**6
00545          MOVE +0                 TO  PI-CHANGE-SW.                   CL**6
00546                                                                      CL**6
00547      IF PI-MODE NOT EQUAL 'S'                                        CL**6
00548          MOVE +1                 TO  PI-BROWSE-SW.                EL107
00549                                                                   EL107
00550      IF PI-MODE EQUAL 'S'                                            CL**6
00551          GO TO 2000-PROCESS-SHOW.                                    CL**6
00552                                                                      CL**6
00553      IF PI-MODE EQUAL 'C'                                            CL**6
00554          GO TO 3000-PROCESS-CHANGE.                                  CL**6
00555                                                                      CL**6
00556      IF PI-MODE EQUAL 'A'                                            CL**6
00557          GO TO 5000-PROCESS-ADD.                                     CL**6
00558                                                                      CL**6
00559      IF PI-MODE EQUAL 'D'                                            CL**6
00560          GO TO 6500-PROCESS-DELETE.                                  CL**6
00561                                                                      CL**6
00562      MOVE 'LOGIC ERROR HAS OCCURRED - PROGRAM EL107'                 CL**6
00563                                  TO  LOGOFF-MSG.                     CL**6
00564      GO TO 8300-SEND-TEXT.                                           CL**6
00565                                                                      CL**6
00566      EJECT                                                        EL107
00567 *****************************************************************    CL**6
00568  1000-INITIAL-SCREEN.                                                CL**6
00569 *****************************************************************    CL**6
00570                                                                      CL**6
00571      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.           CL**6
00572                                                                      CL**6
00573      MOVE ZERO                   TO  PI-1ST-TIME-SW                  CL**6
00574                                      PI-LINE-COUNT                   CL**6
00575                                      PI-BROWSE-SW                    CL**6
00576                                      PI-SHOW-SW                      CL**6
00577                                      PI-CHANGE-SW.                   CL**6
00578                                                                      CL**6
00579      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.              CL**6
00580      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.             CL**6
00581                                                                      CL**6
00582      EXEC CICS READ                                                  CL**6
00583          DATASET   (WS-CONTROL-FILE-DSID)                            CL**6
00584          RIDFLD    (WS-CONTROL-FILE-KEY)                             CL**6
00585          SET       (ADDRESS OF CONTROL-FILE)                         CL*18
00586          GENERIC   EQUAL                                             CL**6
00587          KEYLENGTH (4)                                               CL**6
00588      END-EXEC.                                                       CL**6
00589                                                                      CL**6
00590      MOVE CF-LGX-INTERFACE-CNTL  TO  PI-LOGIC-CUSTOMER.              CL**6
00591                                                                      CL**6
00592      MOVE LOW-VALUES             TO  EL107AO.                        CL**6
00593                                                                      CL**6
00594      MOVE -1      TO  AMAINTL.                                       CL**6
00595      MOVE +2      TO  WS-COMPLETED-SUCCESSFUL.                       CL**6
00596      PERFORM 8100-SEND-INITIAL-MAP.                                  CL**6
00597                                                                      CL**6
00598      GO TO 9100-RETURN-TRAN.                                         CL**6
00599                                                                      CL**6
00600      EJECT                                                           CL**6
00601 *****************************************************************    CL**6
00602  2000-PROCESS-SHOW.                                                  CL**6
00603 *****************************************************************    CL**6
00604                                                                   EL107
00605      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL107
00606                                                                   EL107
00607      MOVE PI-NEXT-BENEFIT-NUMBER TO  PI-BENEFIT-NUMBER.           EL107
00608                                                                   EL107
00609      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL107
00610      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.          EL107
00611      MOVE PI-BENEFIT-NUMBER      TO  WS-CFK-BENEFIT-NO.           EL107
00612      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL107
00613                                                                   EL107
00614      MOVE +1                     TO  PI-SHOW-SW.                  EL107
00615                                                                   EL107
00616      GO TO 8000-DISPLAY-RECORDS.                                  EL107
00617                                                                   EL107
00618      EJECT                                                        EL107
00619 *****************************************************************    CL**6
00620  3000-PROCESS-CHANGE.                                                CL**6
00621 *****************************************************************    CL**6
00622                                                                   EL107
00623      IF  PI-CHANGE-SW EQUAL +2                                       CL**6
00624           GO TO 3000-PHASE-TWO.                                      CL**6
00625                                                                      CL**6
00626      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.            CL**6
00627                                                                      CL**6
00628      MOVE PI-NEXT-BENEFIT-NUMBER TO  PI-BENEFIT-NUMBER.              CL**6
00629                                                                      CL**6
00630      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.              CL**6
00631      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.             CL**6
00632      MOVE PI-BENEFIT-NUMBER      TO  WS-CFK-BENEFIT-NO.              CL**6
00633      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.             CL**6
00634                                                                      CL**6
00635      MOVE +1                     TO  PI-CHANGE-SW.                   CL**6
00636                                                                      CL**6
00637      GO TO 8000-DISPLAY-RECORDS.                                     CL**6
00638                                                                      CL**6
00639 *****************************************************************    CL**6
00640  3000-PHASE-TWO.                                                     CL**6
00641 *****************************************************************    CL**6
00642                                                                      CL**6
00643      EXEC CICS READ                                                  CL**6
00644          UPDATE                                                      CL**6
00645          DATASET ('ELCNTL')                                          CL**6
00646          SET (ADDRESS OF CONTROL-FILE)                               CL*18
00647          RIDFLD (PI-UPDATE-KEY)                                      CL**6
00648      END-EXEC.                                                       CL**6
00649                                                                      CL**6
00650      IF CF-LAST-MAINT-BY NOT EQUAL PI-UPDATE-BY OR                   CL**6
00651         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS                  CL**6
00652           EXEC CICS UNLOCK                                           CL**6
00653              DATASET ('ELCNTL')                                      CL**6
00654           END-EXEC                                                   CL**6
00655           MOVE ER-0068 TO EMI-ERROR                                  CL**6
00656           PERFORM 8200-SEND-DATAONLY                                 CL**6
00657           GO TO 9100-RETURN-TRAN.                                    CL**6
00658                                                                      CL**6
00659                                                                      CL**6
00660      MOVE SPACES                 TO  WS-BENEFIT-TABLE-AREA.          CL**6
00661      MOVE +1 TO WS-INDEX.                                            CL**6
00662      SET BENEFIT-INDEX TO +1.                                        CL**6
00663                                                                      CL**6
00664  3020-MOVE-DATA.                                                     CL**6
00665      MOVE CF-BENEFIT-CONTROLS (WS-INDEX)                             CL**6
00666                                  TO  WS-BENEFIT-CONTROLS-WORK.       CL**6
00667                                                                      CL**6
00668      IF WS-BENEFIT-NUMBER NOT = ZEROS                                CL**6
00669          MOVE WS-BENEFIT-CONTROLS-WORK                               CL**6
00670                        TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)    CL**6
00671      ELSE                                                            CL**6
00672          GO TO 3030-CONTINUE.                                        CL**6
00673                                                                      CL**6
00674      SET BENEFIT-INDEX UP BY +1.                                     CL**6
00675                                                                      CL**6
00676      IF WS-INDEX LESS +8                                             CL**6
00677          ADD +1  TO  WS-INDEX                                        CL**6
00678          GO TO 3020-MOVE-DATA.                                       CL**6
00679                                                                      CL**6
00680  3030-CONTINUE.                                                      CL**6
00681                                                                      CL**6
00682      SET BENEFIT-INDEX TO +1.                                        CL**6
00683      SET EL107A-INDEX TO +1.                                         CL**6
00684                                                                      CL**6
00685      PERFORM 4000-EDIT-DATA THRU 4099-EXIT.                          CL**6
00686                                                                      CL**6
00687      IF EMI-FATAL-CTR GREATER ZERO                                   CL**6
00688          PERFORM 8200-SEND-DATAONLY                               EL107
00689          GO TO 9100-RETURN-TRAN.                                  EL107
00690                                                                   EL107
00691      MOVE 'B'                    TO  JP-RECORD-TYPE.                 CL**6
00692      MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.          CL**6
00693      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.                 CL**6
00694                                                                   EL107
00695      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.                 CL**6
00696                                                                   EL107
00697      SET BENEFIT-INDEX TO +1.                                        CL**3
00698      MOVE +1 TO WS-INDEX.                                            CL**6
00699                                                                      CL**6
00700  3000-UPDATE-LOOP.                                                   CL**6
00701                                                                      CL**3
00702      IF WS-BTE-NUMBER (BENEFIT-INDEX) = CF-BENEFIT-CODE (WS-INDEX)   CL**6
00703          MOVE WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)                 CL**6
00704                   TO  CF-BENEFIT-CONTROLS (WS-INDEX)                 CL**6
00705      ELSE                                                            CL**6
00706          IF WS-INDEX LESS THAN +8                                    CL**6
00707              ADD +1  TO  WS-INDEX                                    CL**6
00708              GO TO 3000-UPDATE-LOOP.                                 CL**6
00709                                                                      CL**6
00710      IF WS-INDEX LESS THAN +8     AND                                CL**6
00711              BENEFIT-INDEX LESS THAN +8                              CL**6
00712          ADD +1  TO  WS-INDEX                                        CL**6
00713          SET BENEFIT-INDEX UP BY +1                                  CL**6
00714          GO TO 3000-UPDATE-LOOP.                                     CL**6
00715                                                                      CL**6
00716      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY                CL**7
00717                                      PI-UPDATE-BY.                   CL**7
00718      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS            CL**7
00719                                      PI-UPDATE-HHMMSS.               CL**7
00720      MOVE SAVE-BIN-DATE          TO  CF-LAST-MAINT-DT.               CL**6
00721                                                                      CL**6
00722      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**6
00723      MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.          CL**6
00724      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.                 CL**6
00725                                                                      CL**6
00726      EXEC CICS REWRITE                                               CL**6
00727          DATASET ('ELCNTL')                                          CL**6
00728          FROM (CONTROL-FILE)                                         CL**6
00729      END-EXEC.                                                       CL**6
00730                                                                      CL**6
00731      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.                 CL**6
00732                                                                      CL**6
00733      MOVE ER-0000 TO EMI-ERROR.                                      CL**6
00734                                                                      CL**7
00735      PERFORM 7200-UPDATE-USER-TIME.                                  CL**7
00736                                                                      CL**6
00737      MOVE ZERO               TO  PI-1ST-TIME-SW                      CL**6
00738                                  PI-SHOW-SW                          CL**6
00739                                  PI-CHANGE-SW.                       CL**6
00740                                                                      CL**6
00741      MOVE SPACES                 TO  PI-UPDATE-KEY.                  CL**6
00742      MOVE PI-LAST-BENEFIT-NUMBER TO  PI-NEXT-BENEFIT-NUMBER.         CL**6
00743      MOVE +3      TO  WS-COMPLETED-SUCCESSFUL.                       CL**6
00744                                                                      CL**6
00745      GO TO 2000-PROCESS-SHOW.                                        CL**6
00746                                                                      CL**6
00747      EJECT                                                           CL**6
00748 *****************************************************************    CL**6
00749  4000-EDIT-DATA.                                                     CL**6
00750 *****************************************************************    CL**6
00751                                                                      CL**6
00752 *    NOTE ******************************************************* EL107
00753 *         *      CHECK TO SEE IF ANY DATA WAS ENTERED ON THIS   * EL107
00754 *         *  LINE.                                              * EL107
00755 *         ******************************************************* EL107
00756                                                                   EL107
00757      IF EL107A-ABBR-LENGTH  (EL107A-INDEX)   NOT GREATER ZERO AND EL107
00758         EL107A-DESC-LENGTH  (EL107A-INDEX)   NOT GREATER ZERO AND EL107
00759         EL107A-COMMENT-LENGTH (EL107A-INDEX) NOT GREATER ZERO AND EL107
051414        EL107A-MAX-BENS-LENGTH (EL107A-INDEX) NOT GREATER ZERO AND
00760         EL107A-LOAN-LENGTH  (EL107A-INDEX)   NOT GREATER ZERO AND EL107
00761         EL107A-EM-LENGTH    (EL107A-INDEX)   NOT GREATER ZERO AND EL107
00762         EL107A-JOINT-LENGTH (EL107A-INDEX)   NOT GREATER ZERO AND EL107
00763         EL107A-OB-LENGTH    (EL107A-INDEX)   NOT GREATER ZERO AND EL107
00764         EL107A-LOD-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO AND EL107
00765         EL107A-RTM-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO AND EL107
00766         EL107A-RFM-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO AND EL107
082503        EL107A-IGC-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO AND
082503        EL107A-CAC-LENGTH   (EL107A-INDEX)   NOT GREATER ZERO
00768          GO TO 4060-CONTINUE-EDIT.                                   CL**6
00769                                                                   EL107
00770 *    NOTE ******************************************************* EL107
00771 *         *      CLEAR THE WORK AREA AND SAVE THE NUMBER OF     * EL107
00772 *         *  ERRORS THAT OCCURRED BEFORE EDITING THIS LINE SO   *    CL*18
00773 *         *  YOU CAN TEST FOR ERRORS IN THIS LINE.              * EL107
00774 *         ******************************************************* EL107
00775                                                                   EL107
00776      MOVE EMI-FATAL-CTR          TO  WS-LAST-ERROR-COUNT.         EL107
00777                                                                   EL107
00778  4010-CONTINUE-EDIT.                                                 CL**6
00779                                                                   EL107
00780      IF EL107A-CODE-I (EL107A-INDEX) =                               CL**3
00781                              WS-BTE-NUMBER (BENEFIT-INDEX)           CL**3
00782          MOVE WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)                 CL**3
00783                                  TO WS-BENEFIT-CONTROLS-WORK         CL**3
00784      ELSE                                                            CL**3
00785          SET BENEFIT-INDEX UP BY +1                                  CL**3
00786          GO TO 4010-CONTINUE-EDIT.                                   CL**6
00787                                                                   EL107
00788 *    NOTE ******************************************************* EL107
00789 *         *              EDIT THE ABBREVIATION.                 *    CL*18
00790 *         ******************************************************* EL107
00791                                                                   EL107
00792      IF PI-BENEFIT-TYPE = '4'                                     EL107
00793          GO TO 4020-CONTINUE-EDIT.                                   CL**6
00794                                                                   EL107
00795      IF EL107A-ABBR-LENGTH (EL107A-INDEX) NOT GREATER ZERO        EL107
00796            GO TO 4030-CONTINUE-EDIT.                                 CL*18
00797                                                                   EL107
00798      MOVE EL107A-ABBR-I (EL107A-INDEX)                            EL107
00799                                  TO  WS-BENEFIT-ABBREVIATION.        CL*18
00800                                                                   EL107
00801      INSPECT EL107A-ABBR-I (EL107A-INDEX)                            CL*18
00802                     CONVERTING LOW-VALUES TO SPACES.                 CL*18
00803                                                                      CL**6
00804      IF EL107A-ABBR-I (EL107A-INDEX) = 'OB ' OR ' OB'             EL107
00805         GO TO 4030-CONTINUE-EDIT.                                    CL**6
00806                                                                   EL107
00807      IF EL107A-ABBR-CHAR-12 (EL107A-INDEX) NUMERIC AND            EL107
00808         EL107A-ABBR-CHAR-12 (EL107A-INDEX) GREATER ZERO           EL107
00809          MOVE AL-UANON TO EL107A-ABBR-ATTRB (EL107A-INDEX)        EL107
00810        ELSE                                                       EL107
00811          MOVE ER-0127            TO  EMI-ERROR                    EL107
00812          PERFORM 9900-ERROR-FORMAT                                EL107
00813          MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)       EL107
00814          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).      EL107
00815                                                                   EL107
00816      IF EL107A-ABBR-CHAR-3 (EL107A-INDEX) = 'R' OR 'E'            EL107
00817          MOVE AL-UANON TO EL107A-ABBR-ATTRB (EL107A-INDEX)        EL107
00818        ELSE                                                       EL107
00819          MOVE ER-0128            TO  EMI-ERROR                    EL107
00820          PERFORM 9900-ERROR-FORMAT                                EL107
00821          MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)       EL107
00822          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).      EL107
00823                                                                   EL107
00824      GO TO 4030-CONTINUE-EDIT.                                       CL**6
00825                                                                   EL107
00826  4020-CONTINUE-EDIT.                                                 CL**6
00827      IF EL107A-ABBR-LENGTH (EL107A-INDEX) GREATER ZERO            EL107
00828          MOVE EL107A-ABBR-I (EL107A-INDEX)                        EL107
00829                                  TO  WS-BENEFIT-ABBREVIATION.        CL*18
00830                                                                   EL107
00831      IF PI-COMPANY-ID = 'DMD' AND                                    CL*19
00832         WS-BENEFIT-ABBREVIATION = SPACES                             CL*19
00833           MOVE ER-8318            TO  EMI-ERROR                      CL*19
00834           PERFORM 9900-ERROR-FORMAT                                  CL*19
00835           MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)         CL*19
00836           MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).        CL*19
00837                                                                      CL*19
00838  4030-CONTINUE-EDIT.                                                 CL**6
00839 *    NOTE ******************************************************* EL107
00840 *         *              EDIT THE DESCRIPTION.                  * EL107
00841 *         ******************************************************* EL107
00842                                                                   EL107
00843      IF EL107A-DESC-LENGTH (EL107A-INDEX) GREATER ZERO            EL107
00844          MOVE EL107A-DESC-I (EL107A-INDEX)                        EL107
00845                                  TO  WS-BENEFIT-DESCRIPTION.      EL107
00846                                                                   EL107
00847 *    NOTE ******************************************************* EL107
00848 *         *              EDIT THE COMMENT.                      * EL107
00849 *         ******************************************************* EL107
00850                                                                   EL107
00851      IF EL107A-COMMENT-LENGTH (EL107A-INDEX) GREATER ZERO         EL107
00852          MOVE EL107A-COMMENT-I (EL107A-INDEX)                     EL107
00853                                  TO  WS-BENEFIT-COMMENT.          EL107
00854                                                                   EL107
00855 *    NOTE ******************************************************* EL107
00856 *         *              EDIT THE ABBREVIATION.                 *    CL*18
00857 *         ******************************************************* EL107
00858                                                                   EL107
00859      IF EL107A-LOAN-LENGTH (EL107A-INDEX) GREATER ZERO            EL107
00860          MOVE EL107A-LOAN-I (EL107A-INDEX)                        EL107
00861                                  TO  WS-BENEFIT-LOAN-TYPE.        EL107

051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414***                                                            ***
051414***   EDIT THE MAXIMUM BENEFITS/CRITICAL PERIOD                ***
051414***                                                            ***
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414
051414     IF EL107A-MAX-BENS-LENGTH (EL107A-INDEX) <> ZEROS
051414        IF EL107A-MAX-BENS-I (EL107A-INDEX) NUMERIC
051414           MOVE EL107A-MAX-BENS-O (EL107A-INDEX)
051414                                 TO WS-BENEFIT-MAX-BENS
051414        END-IF
051414     END-IF

00863 *    NOTE ******************************************************* EL107
00864 *         *              EDIT EARNINGS METHOD.                  * EL107
00865 *         ******************************************************* EL107
00866                                                                   EL107
00867      IF EL107A-EM-LENGTH (EL107A-INDEX) GREATER ZERO              EL107
00868           IF ((EL107A-EM-I (EL107A-INDEX) NUMERIC)                EL107
00869               OR (EL107A-EM-I (EL107A-INDEX) = 'B')) AND          EL107
00870              EL107A-EM-I (EL107A-INDEX) NOT = ZERO                EL107
00871              MOVE EL107A-EM-I (EL107A-INDEX)                      EL107
00872                                  TO  WS-BENEFIT-EARN-METHOD       EL107
00873              MOVE AL-UNNON  TO  EL107A-EM-ATTRB (EL107A-INDEX)    EL107
00874            ELSE                                                   EL107
00875              MOVE AL-UNBON  TO  EL107A-EM-ATTRB (EL107A-INDEX)    EL107
00876              MOVE -1        TO  EL107A-EM-LENGTH (EL107A-INDEX)   EL107
00877              MOVE ER-0036      TO  EMI-ERROR                      EL107
00878              PERFORM 9900-ERROR-FORMAT.                           EL107
00879                                                                   EL107
00880 *    NOTE ******************************************************* EL107
00881 *         *          EDIT THE JOINT COVERAGE INDICATOR.         * EL107
00882 *         ******************************************************* EL107
00883                                                                   EL107
00884      IF PI-BENEFIT-TYPE NOT = '4' AND '5'                            CL*14
00885          NEXT SENTENCE                                            EL107
00886      ELSE                                                         EL107
00887          IF EL107A-JOINT-LENGTH (EL107A-INDEX) GREATER ZERO       EL107
00888              IF EL107A-JOINT-I (EL107A-INDEX) = SPACES OR 'J'     EL107
00889                  MOVE EL107A-JOINT-I (EL107A-INDEX)               EL107
00890                             TO  WS-BENEFIT-JOINT-COVERAGE         EL107
00891                  MOVE AL-UANON                                    EL107
00892                             TO  EL107A-JOINT-ATTRB (EL107A-INDEX) EL107
00893              ELSE                                                 EL107
00894                  MOVE AL-UABON                                    EL107
00895                             TO  EL107A-JOINT-ATTRB (EL107A-INDEX) EL107
00896                  MOVE -1    TO  EL107A-JOINT-LENGTH (EL107A-INDEX)EL107
00897                  MOVE ER-0037  TO  EMI-ERROR                      EL107
00898                  PERFORM 9900-ERROR-FORMAT.                       EL107
00899                                                                   EL107
00900                                                                   EL107
00901 *    NOTE ******************************************************* EL107
00902 *         *      EDIT THE OUTSTANDING BALANCE INDICATOR.        * EL107
00903 *         ******************************************************* EL107
00904                                                                   EL107
00905      IF EL107A-OB-LENGTH (EL107A-INDEX) GREATER ZERO              EL107
00906        IF AKINDI = PI-LIFE-OVERRIDE-L1                               CL*21
00907          MOVE EL107A-OB-I (EL107A-INDEX) TO WS-EDIT-LF-OB            CL*17
00908          IF VALID-LF-OB                                              CL*17
00909            MOVE EL107A-OB-I (EL107A-INDEX)                           CL*17
00910                              TO  WS-BENEFIT-OUTSTANDING-BAL          CL*17
00911            MOVE AL-UANON     TO  EL107A-OB-ATTRB (EL107A-INDEX)      CL*17
00912          ELSE                                                        CL*17
00913            MOVE AL-UABON     TO  EL107A-OB-ATTRB (EL107A-INDEX)      CL*17
00914            MOVE -1           TO  EL107A-OB-LENGTH (EL107A-INDEX)     CL*17
00915            MOVE ER-0038      TO  EMI-ERROR                           CL*17
00916            PERFORM 9900-ERROR-FORMAT.                                CL*17
00917                                                                      CL*17
00918      IF EL107A-OB-LENGTH (EL107A-INDEX) GREATER ZERO                 CL*17
00919        IF AKINDI = PI-AH-OVERRIDE-L1                                 CL*21
00920          MOVE EL107A-OB-I (EL107A-INDEX) TO WS-EDIT-AH-OB            CL*17
00921          IF VALID-AH-OB                                              CL*17
00922            MOVE EL107A-OB-I (EL107A-INDEX)                           CL*17
00923                              TO  WS-BENEFIT-OUTSTANDING-BAL          CL*17
00924            MOVE AL-UANON     TO  EL107A-OB-ATTRB (EL107A-INDEX)      CL*17
00925          ELSE                                                        CL*17
00926            MOVE AL-UABON     TO  EL107A-OB-ATTRB (EL107A-INDEX)      CL*17
00927            MOVE -1           TO  EL107A-OB-LENGTH (EL107A-INDEX)     CL*17
00928            MOVE ER-0847      TO  EMI-ERROR                           CL*17
00929            PERFORM 9900-ERROR-FORMAT.                                CL*17
00930                                                                   EL107
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414***                                                            ***
051414***   Edit the maximum benefits against the special calc code  ***
051414***   If the maxben <> 0 then the spec calc s/b = 'C'          ***
051414***                                                            ***
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414
051414     if ws-benefit-max-bens not numeric
051414        move zeros               to ws-benefit-max-bens
051414     end-if
051414     if pi-benefit-type = '5'
051414        if ((ws-benefit-max-bens > zeros)
051414           and (ws-benefit-outstanding-bal <> 'C'))
051414                          or
051414           ((ws-benefit-max-bens = zeros)
051414           and (ws-benefit-outstanding-bal = 'C'))
051414           MOVE AL-UABON        TO EL107A-OB-ATTRB (EL107A-INDEX)
051414           MOVE -1              TO EL107A-OB-LENGTH (EL107A-INDEX)
051414           MOVE ER-7537         TO EMI-ERROR
051414           PERFORM 9900-ERROR-FORMAT
051414        end-if
051414     end-if

00931 *    NOTE ******************************************************* EL107
00932 *         IF FARM PLAN (F) SPECIAL CALC CODE IS ENTERED,          EL107
00933 *            EARNING METHOD MUST BE 4 - (TEXAS).                  EL107
00934 *         ******************************************************* EL107
00935                                                                   EL107
00936      IF  WS-BENEFIT-OUTSTANDING-BAL = 'F'    AND                  EL107
00937          WS-BENEFIT-EARN-METHOD NOT = '4'                         EL107
00938              MOVE SPACE     TO  WS-BENEFIT-OUTSTANDING-BAL        EL107
00939              MOVE AL-UABON  TO  EL107A-OB-ATTRB (EL107A-INDEX)    EL107
00940              MOVE -1        TO  EL107A-OB-LENGTH (EL107A-INDEX)   EL107
00941              MOVE ER-0640   TO  EMI-ERROR                         EL107
00942              PERFORM 9900-ERROR-FORMAT.                           EL107
00943                                                                   EL107
00944 *    NOTE ******************************************************* EL107
00945 *         *    EDIT THE LEVEL OR DECREASING COVERAGE METHOD     * EL107
00946 *         ******************************************************* EL107
00947                                                                   EL107
00948      IF PI-BENEFIT-TYPE NOT = '4'                                 EL107
00949          NEXT SENTENCE                                            EL107
00950      ELSE                                                         EL107
00951          IF EL107A-LOD-LENGTH (EL107A-INDEX) GREATER ZERO         EL107
00952              IF EL107A-LOD-I (EL107A-INDEX) = 'L' OR 'R' OR 'P'      CL**4
00953                  MOVE EL107A-LOD-I (EL107A-INDEX)                 EL107
00954                                TO  WS-BENEFIT-COVERAGE-TYPE       EL107
00955                  MOVE AL-UANON TO  EL107A-LOD-ATTRB (EL107A-INDEX)EL107
00956              ELSE                                                 EL107
00957                  MOVE AL-UABON TO  EL107A-LOD-ATTRB (EL107A-INDEX)EL107
00958                  MOVE -1       TO EL107A-LOD-LENGTH (EL107A-INDEX)EL107
00959                  MOVE ER-0039  TO  EMI-ERROR                      EL107
00960                  PERFORM 9900-ERROR-FORMAT.                       EL107
00961                                                                   EL107
00962 *    NOTE ******************************************************* EL107
00963 *         *          EDIT THE REMAINING TERM METHOD.            * EL107
00964 *         ******************************************************* EL107
00965                                                                   EL107
00966      IF EL107A-RTM-LENGTH (EL107A-INDEX) GREATER ZERO             EL107
00967          IF EL107A-RTM-I (EL107A-INDEX) GREATER ZERO AND LESS '8'    CL*15
00968          OR EL107A-RTM-I (EL107A-INDEX) = SPACES                  EL107
00969              MOVE EL107A-RTM-I (EL107A-INDEX)                     EL107
00970                                  TO  WS-BENEFIT-REMAIN-TERM       EL107
00971              MOVE AL-UNNON  TO  EL107A-RTM-ATTRB (EL107A-INDEX)   EL107
00972            ELSE                                                   EL107
00973              MOVE AL-UNBON  TO  EL107A-RTM-ATTRB (EL107A-INDEX)   EL107
00974              MOVE -1        TO  EL107A-RTM-LENGTH (EL107A-INDEX)  EL107
00975              MOVE ER-0040   TO  EMI-ERROR                         EL107
00976              PERFORM 9900-ERROR-FORMAT.                           EL107
00977                                                                   EL107
00978  4040-CONTINUE-EDIT.                                                 CL**6
00979 *    NOTE ******************************************************* EL107
00980 *         *          EDIT THE REFUND METHOD.                    * EL107
00981 *         ******************************************************* EL107
00982                                                                   EL107
00983      IF EL107A-RFM-LENGTH (EL107A-INDEX) GREATER ZERO             EL107
00984          MOVE EL107A-RFM-I (EL107A-INDEX) TO WS-EDIT-REFUND       EL107
00985          IF VALID-REFUND-METHOD                                   EL107
00986              MOVE EL107A-RFM-I (EL107A-INDEX)                     EL107
00987                                  TO  WS-BENEFIT-REFUND-METHOD     EL107
00988              MOVE AL-UANON  TO  EL107A-RFM-ATTRB (EL107A-INDEX)   EL107
00989            ELSE                                                   EL107
00990              MOVE AL-UABON  TO  EL107A-RFM-ATTRB (EL107A-INDEX)   EL107
00991              MOVE -1        TO  EL107A-RFM-LENGTH (EL107A-INDEX)  EL107
00992              MOVE ER-0582   TO  EMI-ERROR                         EL107
00993              PERFORM 9900-ERROR-FORMAT.                           EL107
00994                                                                   EL107
00995 *    NOTE ******************************************************* EL107
00996 *         *          EDIT THE INDIVIDUAL/GROUP CODE             * EL107
00997 *         ******************************************************* EL107
00998                                                                   EL107
00999      IF EL107A-IGC-LENGTH (EL107A-INDEX) GREATER ZERO             EL107
01000          MOVE EL107A-IGC-I (EL107A-INDEX) TO WS-EDIT-IG-CODE      EL107
01001          IF VALID-IG-CODE                                         EL107
01002              MOVE EL107A-IGC-I (EL107A-INDEX)                     EL107
01003                                  TO  WS-BENEFIT-IG-CODE           EL107
01004              MOVE AL-UANON  TO  EL107A-IGC-ATTRB (EL107A-INDEX)   EL107
01005            ELSE                                                   EL107
01006              MOVE AL-UABON  TO  EL107A-IGC-ATTRB (EL107A-INDEX)   EL107
01007              MOVE -1        TO  EL107A-IGC-LENGTH (EL107A-INDEX)  EL107
01008              MOVE ER-0596   TO  EMI-ERROR                         EL107
01009              PERFORM 9900-ERROR-FORMAT.                           EL107
01010                                                                   EL107
082503*    NOTE ******************************************************* EL107
082503*         *          EDIT THE BENEFIT CATEGORY                  * EL107
082503*         ******************************************************* EL107
082503                                                                  EL107
082503     IF EL107A-CAC-LENGTH (EL107A-INDEX) > ZERO
082503        MOVE EL107A-CAC-I (EL107A-INDEX)
082503                                 TO WS-BENEFIT-CATEGORY
082503        MOVE AL-UANON  
082503                        TO EL107A-CAC-ATTRB (EL107A-INDEX)
082503     END-IF
01010                                                                   EL107
01011      IF EMI-FATAL-CTR GREATER WS-LAST-ERROR-COUNT                 EL107
01012          GO TO 4060-CONTINUE-EDIT.                                   CL**6
01013                                                                   EL107
01014  4050-CONTINUE-EDIT.                                                 CL**6
01015      MOVE +1                     TO  WS-UPDATE-SW.                EL107
01016                                                                   EL107
01017      MOVE WS-BENEFIT-CONTROLS-WORK                                EL107
01018                  TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX).      EL107
01019                                                                   EL107
01020  4060-CONTINUE-EDIT.                                                 CL**6
01021 *    NOTE ******************************************************* EL107
01022 *         *      AFTER ALL OF THE MAP LINES HAVE BEEN PROCESSED * EL107
01023 *         *  CHECK TO SEE IF ANY ERRORS OCCURRED DURING EDITING. *   CL*18
01024 *         ******************************************************* EL107
01025                                                                   EL107
01026      IF EL107A-INDEX LESS PI-LINE-COUNT                           EL107
01027          SET EL107A-INDEX UP BY +1                                EL107
01028          GO TO 4000-EDIT-DATA.                                       CL**6
01029                                                                   EL107
01030  4099-EXIT.                                                          CL**6
01031      EXIT.                                                           CL**6
01032                                                                   EL107
01033      EJECT                                                        EL107
01034 *****************************************************************    CL**6
01035  5000-PROCESS-ADD.                                                   CL**6
01036 *****************************************************************    CL**6
01037                                                                   EL107
01038      SET EL107A-INDEX TO PI-LINE-COUNT.                           EL107
01039      SET EL107A-INDEX UP BY +1.                                   EL107
01040                                                                   EL107
01041      PERFORM 7000-LOAD-BENEFIT-RECORDS.                           EL107
01042                                                                   EL107
01043      PERFORM 6000-EDIT-DATA THRU 6099-EXIT.                          CL**6
01044                                                                   EL107
01045      IF EMI-FATAL-CTR GREATER ZERO                                   CL**6
01046          PERFORM 8200-SEND-DATAONLY                                  CL**6
01047          GO TO 9100-RETURN-TRAN.                                     CL**6
01048                                                                      CL**6
01049      PERFORM 7100-UPDATE-BENEFIT-TABLE.                              CL**6
01050                                                                      CL**6
01051      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.        CL**6
01052      MOVE LOW-VALUES             TO  EL107AO.                        CL**6
01053      PERFORM 8100-SEND-INITIAL-MAP.                                  CL**6
01054      MOVE ZERO                   TO  PI-1ST-TIME-SW.                 CL**6
01055                                                                      CL**6
01056      MOVE SPACES                 TO  PI-MODE                         CL**6
01057                                      PI-BENEFIT-TYPE                 CL**6
01058                                      PI-BENEFIT-NUMBER               CL**6
01059                                      PI-NEXT-BENEFIT-NUMBER.         CL**6
01060                                                                      CL**6
01061      GO TO 9100-RETURN-TRAN.                                         CL**6
01062                                                                      CL**6
01063      EJECT                                                           CL**6
01064 *****************************************************************    CL**6
01065  6000-EDIT-DATA.                                                     CL**6
01066 *****************************************************************    CL**6
01067                                                                      CL**6
01068 *    NOTE ******************************************************* EL107
01069 *         *      CHECK TO SEE IF ANY DATA WAS ENTERED ON THIS   * EL107
01070 *         *  LINE.                                              * EL107
01071 *         ******************************************************* EL107
01072                                                                   EL107
01073      IF EL107A-CODE-LENGTH    (EL107A-INDEX) GREATER ZERO    OR   EL107
01074         EL107A-ABBR-LENGTH    (EL107A-INDEX) GREATER ZERO    OR   EL107
01075         EL107A-DESC-LENGTH    (EL107A-INDEX) GREATER ZERO    OR   EL107
01076         EL107A-COMMENT-LENGTH (EL107A-INDEX) GREATER ZERO    OR
051414        EL107A-MAX-BENS-LENGTH (EL107A-INDEX) GREATER ZERO   OR
01077         EL107A-LOAN-LENGTH    (EL107A-INDEX) GREATER ZERO    OR   EL107
01078         EL107A-EM-LENGTH      (EL107A-INDEX) GREATER ZERO    OR   EL107
01079         EL107A-JOINT-LENGTH   (EL107A-INDEX) GREATER ZERO    OR   EL107
01080         EL107A-OB-LENGTH      (EL107A-INDEX) GREATER ZERO    OR   EL107
01081         EL107A-LOD-LENGTH     (EL107A-INDEX) GREATER ZERO    OR   EL107
01082         EL107A-RTM-LENGTH     (EL107A-INDEX) GREATER ZERO    OR   EL107
01083         EL107A-RFM-LENGTH     (EL107A-INDEX) GREATER ZERO    OR   EL107
082503        EL107A-IGC-LENGTH     (EL107A-INDEX) GREATER ZERO    OR
082503        EL107A-CAC-LENGTH     (EL107A-INDEX) GREATER ZERO
01085          NEXT SENTENCE                                            EL107
01086        ELSE                                                       EL107
01087          GO TO 6050-CONTINUE-EDIT.                                   CL**6
01088                                                                   EL107
01089 *    NOTE ******************************************************* EL107
01090 *         *      CLEAR THE WORK AREA AND SAVE THE NUMBER OF     * EL107
01091 *         *  ERRORS THAT OCCURRED BEFORE EDITING THIS LINE SO   *    CL*18
01092 *         *  YOU CAN TEST FOR ERRORS IN THIS LINE.              * EL107
01093 *         ******************************************************* EL107
01094                                                                   EL107
01095      MOVE SPACES                 TO  WS-BENEFIT-CONTROLS-WORK.    EL107
01096                                                                   EL107
01097      MOVE EMI-FATAL-CTR          TO  WS-LAST-ERROR-COUNT.         EL107
01098                                                                   EL107
01099 *    NOTE ******************************************************* EL107
01100 *         *              EDIT THE BENEFIT CODE.                 * EL107
01101 *         ******************************************************* EL107
01102                                                                   EL107
01103      IF EL107A-CODE-LENGTH (EL107A-INDEX) GREATER ZERO            EL107
01104          MOVE EL107A-CODE-I (EL107A-INDEX) TO WS-EDIT-BENE-CODE      CL**3
01105          IF NOT INVALID-BENEFIT-CODE                                 CL**3
01106              MOVE EL107A-CODE-I (EL107A-INDEX)                    EL107
01107                                  TO  WS-BENEFIT-NUMBER            EL107
01108            ELSE                                                   EL107
01109              MOVE ER-0025        TO  EMI-ERROR                    EL107
01110              PERFORM 9900-ERROR-FORMAT                            EL107
01111              MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)     CL**3
01112              MOVE -1  TO  EL107A-CODE-LENGTH (EL107A-INDEX)       EL107
01113        ELSE                                                       EL107
01114          MOVE ER-0026            TO  EMI-ERROR                    EL107
01115          PERFORM 9900-ERROR-FORMAT                                EL107
01116          MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)         CL**3
01117          MOVE -1        TO  EL107A-CODE-LENGTH (EL107A-INDEX).    EL107
01118                                                                   EL107
01119 *    NOTE ******************************************************* EL107
01120 *         *              EDIT THE ABBREVIATION.                 *    CL*18
01121 *         ******************************************************* EL107
01122                                                                   EL107
01123      IF PI-BENEFIT-TYPE = '4'                                     EL107
01124          GO TO 6010-CONTINUE-EDIT.                                   CL**6
01125                                                                   EL107
01126      IF EL107A-ABBR-LENGTH (EL107A-INDEX) NOT GREATER ZERO        EL107
01127          MOVE ER-0127            TO  EMI-ERROR                    EL107
01128          PERFORM 9900-ERROR-FORMAT                                EL107
01129          MOVE ER-0128            TO  EMI-ERROR                    EL107
01130          PERFORM 9900-ERROR-FORMAT                                EL107
01131          MOVE ER-0129            TO  EMI-ERROR                    EL107
01132          PERFORM 9900-ERROR-FORMAT                                EL107
01133          MOVE AL-UABON TO EL107A-ABBR-ATTRB (EL107A-INDEX)        EL107
01134          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX)       EL107
01135          GO TO 6020-CONTINUE-EDIT.                                   CL**6
01136                                                                   EL107
01137      MOVE EL107A-ABBR-I (EL107A-INDEX)                            EL107
01138                                  TO  WS-BENEFIT-ABBREVIATION.        CL*18
01139                                                                   EL107
01140      IF EL107A-ABBR-CHAR-12 (EL107A-INDEX) NUMERIC AND            EL107
01141         EL107A-ABBR-CHAR-12 (EL107A-INDEX) GREATER ZERO           EL107
01142          MOVE AL-UANON TO EL107A-ABBR-ATTRB (EL107A-INDEX)        EL107
01143        ELSE                                                       EL107
01144          MOVE ER-0127            TO  EMI-ERROR                    EL107
01145          PERFORM 9900-ERROR-FORMAT                                EL107
01146          MOVE AL-UABON TO EL107A-ABBR-ATTRB (EL107A-INDEX)        EL107
01147          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).      EL107
01148                                                                   EL107
01149      IF EL107A-ABBR-CHAR-3 (EL107A-INDEX) = 'R' OR 'E'            EL107
01150          MOVE AL-UANON TO EL107A-ABBR-ATTRB (EL107A-INDEX)        EL107
01151        ELSE                                                       EL107
01152          MOVE ER-0128            TO  EMI-ERROR                    EL107
01153          PERFORM 9900-ERROR-FORMAT                                EL107
01154          MOVE AL-UABON TO EL107A-ABBR-ATTRB (EL107A-INDEX)        EL107
01155          MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).      EL107
01156                                                                   EL107
01157      GO TO 6020-CONTINUE-EDIT.                                       CL**6
01158                                                                   EL107
01159  6010-CONTINUE-EDIT.                                                 CL**6
01160      IF EL107A-ABBR-LENGTH (EL107A-INDEX) GREATER ZERO               CL*18
01161          MOVE EL107A-ABBR-I (EL107A-INDEX)                           CL*18
01162                                  TO  WS-BENEFIT-ABBREVIATION         CL*18
01163        ELSE                                                          CL*18
01164          IF PI-COMPANY-ID = 'DMD'                                    CL*18
01165            MOVE ER-8318            TO  EMI-ERROR                     CL*18
01166            PERFORM 9900-ERROR-FORMAT                                 CL*18
01167            MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)        CL*18
01168            MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).       CL*18
01169                                                                      CL*19
01170      IF PI-COMPANY-ID = 'DMD' AND                                    CL*19
01171         WS-BENEFIT-ABBREVIATION = SPACES                             CL*19
01172           MOVE ER-8318            TO  EMI-ERROR                      CL*19
01173           PERFORM 9900-ERROR-FORMAT                                  CL*19
01174           MOVE AL-UABON TO EL107A-ABBR-ATTRB  (EL107A-INDEX)         CL*19
01175           MOVE -1       TO EL107A-ABBR-LENGTH (EL107A-INDEX).        CL*19
01176                                                                   EL107
01177  6020-CONTINUE-EDIT.                                                 CL**6
01178 *    NOTE ******************************************************* EL107
01179 *         *              EDIT THE DESCRIPTION.                  * EL107
01180 *         ******************************************************* EL107
01181                                                                   EL107
01182      IF EL107A-DESC-LENGTH (EL107A-INDEX) GREATER ZERO            EL107
01183          MOVE EL107A-DESC-I (EL107A-INDEX)                        EL107
01184                                  TO  WS-BENEFIT-DESCRIPTION.      EL107
01185                                                                   EL107
01186 *    NOTE ******************************************************* EL107
01187 *         *              EDIT THE COMMENT.                      * EL107
01188 *         ******************************************************* EL107
01189                                                                   EL107
01190      IF EL107A-COMMENT-LENGTH (EL107A-INDEX) GREATER ZERO         EL107
01191          MOVE EL107A-COMMENT-I (EL107A-INDEX)                     EL107
01192                                  TO  WS-BENEFIT-COMMENT.          EL107
01193                                                                   EL107
01194 *    NOTE ******************************************************* EL107
01195 *         *              EDIT THE LOAN TYPE.                    * EL107
01196 *         ******************************************************* EL107
01197                                                                   EL107
01198      IF EL107A-LOAN-LENGTH (EL107A-INDEX) GREATER ZERO            EL107
01199          MOVE EL107A-LOAN-I (EL107A-INDEX)                        EL107
01200                                  TO  WS-BENEFIT-LOAN-TYPE.        EL107
01201                                                                   EL107
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414***                                                            ***
051414***   EDIT THE MAXIMUM BENEFITS/CRITICAL PERIOD                ***
051414***                                                            ***
051414***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_-***
051414
081214     move zeros to ws-benefit-max-bens
051414     IF PI-BENEFIT-TYPE = '5'
051414        IF EL107A-MAX-BENS-LENGTH (EL107A-INDEX) <> ZEROS
051414           IF EL107A-MAX-BENS-I (EL107A-INDEX) NUMERIC
051414              MOVE EL107A-MAX-BENS-O (EL107A-INDEX)
051414                                 TO WS-BENEFIT-MAX-BENS
051414              MOVE AL-UANON      TO EL107A-MAX-BENS-ATTRB
051414                                                (EL107A-INDEX)
051414           ELSE
051414              MOVE AL-UNBON      TO EL107A-MAX-BENS-ATTRB
051414                                                (EL107A-INDEX)
051414              MOVE -1            TO EL107A-MAX-BENS-LENGTH
051414                                                (EL107A-INDEX)
051414              MOVE 9999          TO EMI-ERROR                      
051414              PERFORM 9900-ERROR-FORMAT
051414           END-IF
051414        END-IF
051414     END-IF

01202 *    NOTE ******************************************************* EL107
01203 *         *              EDIT THE EARNINGS METHOD.              * EL107
01204 *         *                   (REQUIRED FIELD)                  * EL107
01205 *         ******************************************************* EL107
01206                                                                   EL107
01207      IF EL107A-EM-LENGTH (EL107A-INDEX) GREATER ZERO              EL107
01208         IF ((EL107A-EM-I (EL107A-INDEX) NUMERIC)                  EL107
01209              OR (EL107A-EM-I (EL107A-INDEX) = 'B')) AND           EL107
01210              EL107A-EM-I (EL107A-INDEX) NOT = ZERO                EL107
01211              MOVE EL107A-EM-I (EL107A-INDEX)                      EL107
01212                             TO  WS-BENEFIT-EARN-METHOD            EL107
01213              MOVE AL-UNNON  TO  EL107A-EM-ATTRB (EL107A-INDEX)    EL107
01214          ELSE                                                     EL107
01215              MOVE AL-UNBON  TO  EL107A-EM-ATTRB (EL107A-INDEX)    EL107
01216              MOVE -1        TO  EL107A-EM-LENGTH (EL107A-INDEX)   EL107
01217              MOVE ER-0036   TO  EMI-ERROR                         EL107
01218              PERFORM 9900-ERROR-FORMAT                            EL107
01219        ELSE                                                       EL107
01220            MOVE AL-UNBOF    TO  EL107A-EM-ATTRB (EL107A-INDEX)    EL107
01221            MOVE -1          TO  EL107A-EM-LENGTH (EL107A-INDEX)   EL107
01222            MOVE ER-0036     TO  EMI-ERROR                         EL107
01223            PERFORM 9900-ERROR-FORMAT.                             EL107
01224                                                                   EL107
01225 *    NOTE ******************************************************* EL107
01226 *         *          EDIT THE JOINT COVERAGE INDICATOR.         * EL107
01227 *         ******************************************************* EL107
01228                                                                   EL107
01229      IF PI-BENEFIT-TYPE NOT = '4' AND '5'                            CL*14
01230          NEXT SENTENCE                                            EL107
01231      ELSE                                                         EL107
01232          IF EL107A-JOINT-LENGTH (EL107A-INDEX) GREATER ZERO       EL107
01233              IF EL107A-JOINT-I (EL107A-INDEX) = SPACES OR 'J'     EL107
01234                  MOVE EL107A-JOINT-I (EL107A-INDEX)               EL107
01235                             TO  WS-BENEFIT-JOINT-COVERAGE         EL107
01236                  MOVE AL-UANON                                    EL107
01237                             TO  EL107A-JOINT-ATTRB (EL107A-INDEX) EL107
01238              ELSE                                                 EL107
01239                  MOVE AL-UABON                                    EL107
01240                             TO  EL107A-JOINT-ATTRB (EL107A-INDEX) EL107
01241                  MOVE -1    TO  EL107A-JOINT-LENGTH (EL107A-INDEX)EL107
01242                  MOVE ER-0037  TO  EMI-ERROR                      EL107
01243                  PERFORM 9900-ERROR-FORMAT.                       EL107
01244                                                                   EL107
01245 *    NOTE ******************************************************* EL107
01246 *         *        EDIT THE OUTSTANDING BALANCE INDICATOR.      * EL107
01247 *         ******************************************************* EL107
01248                                                                   EL107
01249      IF EL107A-OB-LENGTH (EL107A-INDEX) GREATER ZERO              EL107
01250        IF AKINDI = PI-LIFE-OVERRIDE-L1                               CL*21
01251          MOVE EL107A-OB-I (EL107A-INDEX) TO  WS-EDIT-LF-OB           CL*17
01252          IF VALID-LF-OB                                              CL*17
01253            MOVE EL107A-OB-I (EL107A-INDEX)                           CL*17
01254                               TO  WS-BENEFIT-OUTSTANDING-BAL         CL*17
01255            MOVE AL-UANON      TO  EL107A-OB-ATTRB (EL107A-INDEX)     CL*17
01256          ELSE                                                        CL*17
01257            MOVE AL-UABON      TO  EL107A-OB-ATTRB (EL107A-INDEX)     CL*17
01258            MOVE -1            TO  EL107A-OB-LENGTH (EL107A-INDEX)    CL*17
01259            MOVE ER-0038       TO  EMI-ERROR                          CL*17
01260            PERFORM 9900-ERROR-FORMAT.                                CL*17
01261                                                                      CL*17
01262      IF EL107A-OB-LENGTH (EL107A-INDEX) GREATER ZERO                 CL*17
01263        IF AKINDI = PI-AH-OVERRIDE-L1                                 CL*21
01264          MOVE EL107A-OB-I (EL107A-INDEX) TO  WS-EDIT-AH-OB           CL*17
01265          IF VALID-AH-OB                                              CL*17
01266            MOVE EL107A-OB-I (EL107A-INDEX)                           CL*17
01267                               TO  WS-BENEFIT-OUTSTANDING-BAL         CL*17
01268            MOVE AL-UANON      TO  EL107A-OB-ATTRB (EL107A-INDEX)     CL*17
01269          ELSE                                                        CL*17
01270            MOVE AL-UABON      TO  EL107A-OB-ATTRB (EL107A-INDEX)     CL*17
01271            MOVE -1            TO  EL107A-OB-LENGTH (EL107A-INDEX)    CL*17
01272            MOVE ER-0847       TO  EMI-ERROR                          CL*17
01273            PERFORM 9900-ERROR-FORMAT.                                CL*17
01274                                                                   EL107
01275 *    NOTE ******************************************************* EL107
01276 *         *    EDIT THE LEVEL OR DECREASING COVERAGE METHOD     * EL107
01277 *         ******************************************************* EL107
01278                                                                   EL107
01279      IF PI-BENEFIT-TYPE NOT = '4'                                 EL107
01280          NEXT SENTENCE                                            EL107
01281      ELSE                                                         EL107
01282          IF EL107A-LOD-LENGTH (EL107A-INDEX) GREATER ZERO         EL107
01283              IF EL107A-LOD-I (EL107A-INDEX) = 'L' OR 'R' OR 'P'      CL**4
01284                  MOVE EL107A-LOD-I (EL107A-INDEX)                 EL107
01285                                TO  WS-BENEFIT-COVERAGE-TYPE       EL107
01286                  MOVE AL-UANON TO  EL107A-LOD-ATTRB (EL107A-INDEX)EL107
01287              ELSE                                                 EL107
01288                  MOVE AL-UABON TO  EL107A-LOD-ATTRB (EL107A-INDEX)EL107
01289                  MOVE -1       TO EL107A-LOD-LENGTH (EL107A-INDEX)EL107
01290                  MOVE ER-0039  TO  EMI-ERROR                      EL107
01291                  PERFORM 9900-ERROR-FORMAT.                       EL107
01292                                                                   EL107
01293 *    NOTE ******************************************************* EL107
01294 *         *            EDIT THE REMAINING TERM METHOD.          * EL107
01295 *         *                  (REQUIRED FIELD)                   * EL107
01296 *         ******************************************************* EL107
01297                                                                   EL107
01298         IF EL107A-RTM-LENGTH (EL107A-INDEX) GREATER ZERO          EL107
01299             IF EL107A-RTM-I (EL107A-INDEX) GREATER ZERO AND       EL107
01300                                            LESS '8'                  CL*15
01301                OR EL107A-RTM-I (EL107A-INDEX) = SPACES            EL107
01302                  MOVE EL107A-RTM-I (EL107A-INDEX)                 EL107
01303                                  TO  WS-BENEFIT-REMAIN-TERM       EL107
01304                  MOVE AL-UNNON TO EL107A-RTM-ATTRB (EL107A-INDEX) EL107
01305                ELSE                                               EL107
01306                  MOVE AL-UNBON TO EL107A-RTM-ATTRB (EL107A-INDEX) EL107
01307                  MOVE -1       TO EL107A-RTM-LENGTH (EL107A-INDEX)EL107
01308                  MOVE ER-0040  TO  EMI-ERROR                      EL107
01309                  PERFORM 9900-ERROR-FORMAT.                       EL107
01310                                                                   EL107
01311 *    NOTE ******************************************************* EL107
01312 *         *          EDIT THE REFUND METHOD.                    * EL107
01313 *         ******************************************************* EL107
01314                                                                   EL107
01315      IF EL107A-RFM-LENGTH (EL107A-INDEX) GREATER ZERO             EL107
01316          MOVE EL107A-RFM-I (EL107A-INDEX) TO WS-EDIT-REFUND          CL*20
01317          IF VALID-REFUND-METHOD                                      CL*20
01318              MOVE EL107A-RFM-I (EL107A-INDEX)                     EL107
01319                                  TO  WS-BENEFIT-REFUND-METHOD     EL107
01320              MOVE AL-UNNON  TO  EL107A-RFM-ATTRB (EL107A-INDEX)   EL107
01321          ELSE                                                        CL*20
01322              MOVE AL-UNBON  TO  EL107A-RFM-ATTRB (EL107A-INDEX)   EL107
01323              MOVE -1        TO  EL107A-RFM-LENGTH (EL107A-INDEX)  EL107
01324              MOVE ER-0582   TO  EMI-ERROR                         EL107
01325              PERFORM 9900-ERROR-FORMAT.                           EL107
01326                                                                   EL107
01327 *    NOTE ******************************************************* EL107
01328 *         *          EDIT THE INDIVIDUAL/GROUP CODE             * EL107
01329 *         ******************************************************* EL107
01330                                                                   EL107
01331      IF EL107A-IGC-LENGTH (EL107A-INDEX) GREATER ZERO             EL107
01332          MOVE EL107A-IGC-I (EL107A-INDEX) TO WS-EDIT-IG-CODE      EL107
01333          IF VALID-IG-CODE                                         EL107
01334              MOVE EL107A-IGC-I (EL107A-INDEX)                     EL107
01335                                  TO  WS-BENEFIT-IG-CODE           EL107
01336              MOVE AL-UANON  TO  EL107A-IGC-ATTRB (EL107A-INDEX)   EL107
01337            ELSE                                                   EL107
01338              MOVE AL-UABON  TO  EL107A-IGC-ATTRB (EL107A-INDEX)   EL107
01339              MOVE -1        TO  EL107A-IGC-LENGTH (EL107A-INDEX)  EL107
01340              MOVE ER-0596   TO  EMI-ERROR                         EL107
01341              PERFORM 9900-ERROR-FORMAT.                           EL107
01342                                                                   EL107
01327 *    NOTE ******************************************************* EL107
01328 *         *          EDIT THE BENEFIT GROUP                     * EL107
01329 *         ******************************************************* EL107
01330                                                                   EL107
082503     IF EL107A-CAC-LENGTH (EL107A-INDEX) > ZERO
082503        MOVE EL107A-CAC-I (EL107A-INDEX)
082503                                 TO  WS-BENEFIT-CATEGORY
082503        MOVE AL-UANON  TO  EL107A-CAC-ATTRB (EL107A-INDEX)
082503     END-IF
01342                                                                   EL107
01343      IF EMI-FATAL-CTR GREATER WS-LAST-ERROR-COUNT                 EL107
01344          GO TO 6050-CONTINUE-EDIT.                                   CL**6
01345                                                                   EL107
082503*    IF WS-BTE-NUMBER (200) NOT = SPACES                             CL**3
082503     IF WS-BTE-NUMBER (450) NOT = SPACES                             CL**3
01347          MOVE ER-0592         TO  EMI-ERROR                          CL**3
01348          PERFORM 9900-ERROR-FORMAT                                   CL**3
01349          MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)         CL**3
01350          MOVE -1        TO  EL107A-CODE-LENGTH (EL107A-INDEX)        CL**3
01351          GO TO 6050-CONTINUE-EDIT.                                   CL**6
01352                                                                      CL**3
01353      SET BENEFIT-INDEX TO +1.                                        CL**3
01354                                                                      CL**3
01355  6030-CONTINUE-EDIT.                                                 CL**6
01356                                                                      CL**3
01357      IF WS-BTE-NUMBER (BENEFIT-INDEX) = SPACES                       CL**3
01358          GO TO 6040-CONTINUE-EDIT.                                   CL**6
01359                                                                      CL**3
01360      IF BENEFIT-INDEX > WS-MAX-BEN-CODES                             CL*21
01361          MOVE ER-0752         TO  EMI-ERROR                          CL*21
01362          PERFORM 9900-ERROR-FORMAT                                   CL*21
01363          MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)         CL*21
01364          MOVE -1        TO  EL107A-CODE-LENGTH (EL107A-INDEX)        CL*21
01365          GO TO 6099-EXIT.                                            CL*21
01366                                                                      CL*21
01367      IF WS-BENEFIT-NUMBER  GREATER  WS-BTE-NUMBER (BENEFIT-INDEX)    CL**3
01368          SET BENEFIT-INDEX UP BY +1                                  CL**3
01369          GO TO 6030-CONTINUE-EDIT.                                   CL**6
01370                                                                      CL**3
01371      IF WS-BENEFIT-NUMBER =  WS-BTE-NUMBER (BENEFIT-INDEX)           CL**3
01372          MOVE ER-0027         TO  EMI-ERROR                          CL**3
01373          PERFORM 9900-ERROR-FORMAT                                   CL**3
01374          MOVE AL-UABOF  TO  EL107A-CODE-ATTRB (EL107A-INDEX)         CL**3
01375          MOVE -1        TO  EL107A-CODE-LENGTH (EL107A-INDEX)        CL**3
01376          GO TO 6050-CONTINUE-EDIT.                                   CL**6
01377                                                                      CL**3
01378  6040-CONTINUE-EDIT.                                                 CL**6
01379                                                                   EL107
01380      IF WS-BTE-NUMBER (BENEFIT-INDEX) = SPACES                    EL107
01381          MOVE WS-BENEFIT-CONTROLS-WORK                            EL107
01382                      TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)   EL107
01383          MOVE +1                 TO  WS-UPDATE-SW                 EL107
01384          GO TO 6050-CONTINUE-EDIT.                                   CL**6
01385                                                                      CL**3
01386      MOVE WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)                     CL**3
01387                                  TO WS-BENEFIT-CONTROLS-SAVE.        CL**3
01388      MOVE WS-BENEFIT-CONTROLS-WORK                                   CL**3
01389                      TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX).     CL**3
01390      MOVE WS-BENEFIT-CONTROLS-SAVE TO WS-BENEFIT-CONTROLS-WORK.      CL**3
01391                                                                      CL**3
01392      SET BENEFIT-INDEX UP BY +1.                                     CL**3
01393                                                                      CL**3
01394      GO TO 6040-CONTINUE-EDIT.                                       CL**6
01395                                                                   EL107
01396  6050-CONTINUE-EDIT.                                                 CL**6
01397                                                                      CL**3
01398 *    NOTE ******************************************************* EL107
01399 *         *      AFTER ALL OF THE MAP LINES HAVE BEEN PROCESSED * EL107
01400 *         *  CHECK TO SEE IF ANY ERRORS OCCURRED DURING EDITING. *   CL*18
01401 *         ******************************************************* EL107
01402                                                                   EL107
01403      IF EL107A-INDEX LESS +8                                      EL107
01404          SET EL107A-INDEX UP BY +1                                EL107
01405          GO TO 6000-EDIT-DATA.                                       CL**6
01406                                                                   EL107
01407  6099-EXIT.                                                          CL**6
01408      EXIT.                                                           CL**6
01409                                                                   EL107
01410      EJECT                                                        EL107
01411 *****************************************************************    CL**6
01412  6500-PROCESS-DELETE.                                                CL**6
01413 *****************************************************************    CL**6
01414                                                                      CL**7
01415      IF PI-SHOW-SW EQUAL ZERO                                        CL**6
01416          MOVE ER-0138            TO  EMI-ERROR                    EL107
01417          MOVE -1                 TO  AMAINTL                      EL107
01418          PERFORM 8200-SEND-DATAONLY                               EL107
01419          GO TO 9100-RETURN-TRAN.                                  EL107
01420                                                                   EL107
01421      IF PI-SHOW-SW = +2                                           EL107
01422          MOVE ER-0139            TO  EMI-ERROR                    EL107
01423          MOVE -1                 TO  AMAINTL                      EL107
01424          PERFORM 8200-SEND-DATAONLY                               EL107
01425          GO TO 9100-RETURN-TRAN.                                  EL107
01426                                                                   EL107
01427      PERFORM 7000-LOAD-BENEFIT-RECORDS.                           EL107
01428                                                                   EL107
01429      SET BENEFIT-INDEX TO +1.                                        CL**3
01430                                                                      CL**3
01431  6510-MAIN-LOGIC.                                                    CL**6
01432                                                                      CL**3
01433      IF PI-BENEFIT-NUMBER = WS-BTE-NUMBER (BENEFIT-INDEX)            CL**3
01434          GO TO 6520-MAIN-LOGIC.                                      CL**6
01435                                                                      CL**3
01436      IF WS-BTE-NUMBER (BENEFIT-INDEX) = SPACES  OR                   CL**3
01437         BENEFIT-INDEX = WS-MAX-BEN-CODES                             CL*21
01438          MOVE ER-0028            TO  EMI-ERROR                    EL107
01439          MOVE AL-UABOF           TO  ABENEA                          CL**5
01440          MOVE -1                 TO  ABENEL                       EL107
01441          PERFORM 8200-SEND-DATAONLY                               EL107
01442          GO TO 9100-RETURN-TRAN.                                  EL107
01443                                                                   EL107
01444      SET BENEFIT-INDEX UP BY +1.                                     CL**3
01445                                                                      CL**3
01446      GO TO 6510-MAIN-LOGIC.                                          CL**6
01447                                                                      CL**3
01448  6520-MAIN-LOGIC.                                                    CL**6
01449                                                                      CL**3
01450      MOVE SPACES  TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX).     EL107
01451                                                                      CL**3
01452      PERFORM 7100-UPDATE-BENEFIT-TABLE.                           EL107
01453                                                                   EL107
01454      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     EL107
01455      MOVE LOW-VALUES             TO  EL107AO.                     EL107
01456      PERFORM 8100-SEND-INITIAL-MAP.                               EL107
01457                                                                   EL107
01458      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL107
01459                                      PI-SHOW-SW.                  EL107
01460      MOVE SPACES                 TO  PI-MODE                      EL107
01461                                      PI-BENEFIT-TYPE              EL107
01462                                      PI-BENEFIT-NUMBER            EL107
01463                                      PI-NEXT-BENEFIT-NUMBER.      EL107
01464      GO TO 9100-RETURN-TRAN.                                      EL107
01465                                                                   EL107
01466      EJECT                                                        EL107
01467 *****************************************************************    CL**6
01468  7000-LOAD-BENEFIT-RECORDS SECTION.                               EL107
01469 *****************************************************************    CL**6
01470                                                                      CL**6
01471      EXEC CICS HANDLE CONDITION                                   EL107
01472          NOTFND  (7090-EXIT)                                      EL107
01473          ENDFILE (7030-ENDBROWSE)                                 EL107
01474      END-EXEC.                                                    EL107
01475                                                                      CL**3
01476      SET BENEFIT-INDEX TO +1.                                        CL**3
01477                                                                   EL107
01478      MOVE SPACES                 TO  WS-BENEFIT-TABLE-AREA        EL107
01479                                      WS-CONTROL-FILE-KEY.         EL107
01480                                                                   EL107
01481      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL107
01482      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.          EL107
01483      MOVE PI-BENEFIT-NUMBER      TO  WS-CFK-BENEFIT-NO.           EL107
01484      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL107
01485                                                                   EL107
01486      EXEC CICS STARTBR                                            EL107
01487          DATASET   (WS-CONTROL-FILE-DSID)                         EL107
01488          RIDFLD    (WS-CONTROL-FILE-KEY)                          EL107
01489          GENERIC   EQUAL                                          EL107
01490          KEYLENGTH (4)                                            EL107
01491      END-EXEC.                                                    EL107
01492                                                                   EL107
01493  7010-READNEXT.                                                   EL107
01494      EXEC CICS READNEXT                                           EL107
01495          SET     (ADDRESS OF CONTROL-FILE)                           CL*18
01496          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01497          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL107
01498      END-EXEC.                                                    EL107
01499                                                                   EL107
01500      IF PI-BENEFIT-TYPE NOT = CF-RECORD-TYPE                      EL107
01501          GO TO 7030-ENDBROWSE.                                    EL107
01502                                                                   EL107
01503      MOVE +1                     TO  WS-INDEX.                    EL107
01504                                                                   EL107
01505  7020-MOVE-DATA.                                                  EL107
01506      MOVE CF-BENEFIT-CONTROLS (WS-INDEX)                          EL107
01507                                  TO  WS-BENEFIT-CONTROLS-WORK.    EL107
01508                                                                   EL107
01509      IF WS-BENEFIT-NUMBER NOT = ZEROS                                CL**3
01510          MOVE WS-BENEFIT-CONTROLS-WORK                            EL107
01511                        TO  WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)    CL**3
01512        ELSE                                                       EL107
01513          GO TO 7030-ENDBROWSE.                                    EL107
01514                                                                      CL**3
01515      SET BENEFIT-INDEX UP BY +1.                                     CL**3
01516                                                                   EL107
01517      IF WS-INDEX LESS +8                                          EL107
01518          ADD +1  TO  WS-INDEX                                     EL107
01519          GO TO 7020-MOVE-DATA.                                    EL107
01520                                                                   EL107
01521      GO TO 7010-READNEXT.                                         EL107
01522                                                                   EL107
01523  7030-ENDBROWSE.                                                  EL107
01524      EXEC CICS ENDBR                                              EL107
01525          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01526      END-EXEC.                                                    EL107
01527                                                                   EL107
01528  7090-EXIT.                                                       EL107
01529       EXIT.                                                       EL107
01530                                                                   EL107
01531      EJECT                                                        EL107
01532 *****************************************************************    CL**6
01533  7100-UPDATE-BENEFIT-TABLE SECTION.                               EL107
01534 *****************************************************************    CL**6
01535                                                                      CL**6
01536      MOVE SAVE-DATE              TO  DC-GREG-DATE-1-EDIT.         EL107
01537      MOVE '2'                    TO  DC-OPTION-CODE.              EL107
01538                                                                   EL107
01539      PERFORM 8500-DATE-CONVERSION.                                EL107
01540                                                                   EL107
01541      MOVE EIBTIME                TO  TIME-IN.                     EL107
01542                                                                   EL107
01543      EXEC CICS HANDLE CONDITION                                   EL107
01544          NOTFND  (7125-WRITE-BENEFIT-RECORDS)                     EL107
01545          ENDFILE (7120-ENDBROWSE)                                 EL107
01546      END-EXEC.                                                    EL107
01547                                                                   EL107
01548      SET BENEFIT-INDEX TO +1.                                     EL107
01549                                                                   EL107
01550      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL107
01551                                                                   EL107
01552      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL107
01553      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.          EL107
01554      MOVE PI-BENEFIT-NUMBER      TO  WS-CFK-BENEFIT-NO.           EL107
01555      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL107
01556                                                                   EL107
01557      EXEC CICS STARTBR                                            EL107
01558          DATASET   (WS-CONTROL-FILE-DSID)                         EL107
01559          RIDFLD    (WS-CONTROL-FILE-KEY)                          EL107
01560          GENERIC   EQUAL                                          EL107
01561          KEYLENGTH (4)                                            EL107
01562      END-EXEC.                                                    EL107
01563                                                                   EL107
01564      MOVE 'D'                    TO  JP-RECORD-TYPE.              EL107
01565      MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.       EL107
01566                                                                   EL107
01567  7110-READNEXT.                                                   EL107
01568      EXEC CICS READNEXT                                           EL107
01569          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01570          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL107
01571          SET     (ADDRESS OF CONTROL-FILE)                           CL*18
01572      END-EXEC.                                                    EL107
01573                                                                   EL107
01574      IF PI-BENEFIT-TYPE NOT = CF-RECORD-TYPE                      EL107
01575          GO TO 7120-ENDBROWSE.                                    EL107
01576                                                                   EL107
01577      IF PI-UPDATE-BY     NOT = CF-LAST-MAINT-BY  OR               EL107
01578         PI-UPDATE-HHMMSS NOT = CF-LAST-MAINT-HHMMSS               EL107
01579          MOVE ER-0068            TO  EMI-ERROR                    EL107
01580          MOVE LOW-VALUES         TO  EL107AO                      EL107
01581          MOVE -1                 TO  AMAINTL                      EL107
01582          PERFORM 8100-SEND-INITIAL-MAP                            EL107
01583          GO TO 9100-RETURN-TRAN.                                  EL107
01584                                                                   EL107
01585      EXEC CICS ENDBR                                              EL107
01586          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01587      END-EXEC.                                                    EL107
01588                                                                   EL107
01589      EXEC CICS READ UPDATE                                        EL107
01590          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01591          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL107
01592          SET     (ADDRESS OF CONTROL-FILE)                           CL*18
01593      END-EXEC.                                                    EL107
01594                                                                   EL107
01595      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL107
01596                                                                   EL107
01597      EXEC CICS DELETE                                             EL107
01598          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01599      END-EXEC.                                                    EL107
01600                                                                   EL107
01601      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              EL107
01602                                                                   EL107
01603      EXEC CICS STARTBR                                            EL107
01604          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01605          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL107
01606      END-EXEC.                                                    EL107
01607                                                                   EL107
01608      GO TO 7110-READNEXT.                                         EL107
01609                                                                   EL107
01610  7120-ENDBROWSE.                                                  EL107
01611      EXEC CICS ENDBR                                              EL107
01612          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01613      END-EXEC.                                                    EL107
01614                                                                   EL107
01615  7125-WRITE-BENEFIT-RECORDS.                                      EL107
01616      EXEC CICS GETMAIN                                            EL107
01617          SET     (ADDRESS OF CONTROL-FILE)                           CL*18
01618          LENGTH  (750)                                               CL*11
01619          INITIMG (WS-SPACE)                                       EL107
01620      END-EXEC.                                                    EL107
01621                                                                   EL107
01622      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL107
01623                                                                   EL107
01624      SET BENEFIT-INDEX TO +1.                                     EL107
01625                                                                   EL107
01626      EJECT                                                        EL107
01627  7130-CREATE-BENEFIT-RECORD.                                      EL107
01628      MOVE SPACES                 TO  CONTROL-FILE.                EL107
01629                                                                   EL107
01630      MOVE 'CF'                   TO  CF-RECORD-ID.                EL107
01631      MOVE PI-COMPANY-ID          TO  CF-COMPANY-ID.               EL107
01632      MOVE PI-BENEFIT-TYPE        TO  CF-RECORD-TYPE.              EL107
01633      MOVE ZERO                   TO  CF-SEQUENCE-NO.              EL107
01634      MOVE DC-BIN-DATE-1          TO  CF-LAST-MAINT-DT.            EL107
01635      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.            EL107
01636      MOVE TIME-IN                TO  CF-LAST-MAINT-HHMMSS.        EL107
01637                                                                   EL107
01638      MOVE ZERO                   TO  CF-BENEFIT-CODE (1)             CL**3
01639                                      CF-BENEFIT-CODE (2)             CL**3
01640                                      CF-BENEFIT-CODE (3)             CL**3
01641                                      CF-BENEFIT-CODE (4)             CL**3
01642                                      CF-BENEFIT-CODE (5)             CL**3
01643                                      CF-BENEFIT-CODE (6)             CL**3
01644                                      CF-BENEFIT-CODE (7)             CL**3
01645                                      CF-BENEFIT-CODE (8).            CL**3
01646                                                                   EL107
01647      MOVE +1                     TO  WS-INDEX.                    EL107
01648                                                                   EL107
01649  7140-MOVE-TABLE-ENTRIES.                                         EL107
01650      IF WS-BTE-NUMBER (BENEFIT-INDEX) = SPACES                    EL107
01651          GO TO 7150-INCREMENT-BENEFIT-INDEX.                      EL107
01652                                                                   EL107
01653      MOVE WS-BTE-NUMBER (BENEFIT-INDEX)  TO  CF-HI-BEN-IN-REC.    EL107
01654                                                                   EL107
01655      MOVE WS-BENEFIT-TABLE-ENTRY (BENEFIT-INDEX)                  EL107
01656                  TO  CF-BENEFIT-CONTROLS (WS-INDEX).              EL107
01657                                                                   EL107
01658      IF WS-INDEX LESS +8                                          EL107
01659          ADD +1  TO  WS-INDEX                                     EL107
01660        ELSE                                                       EL107
01661          GO TO 7160-WRITE-BENEFIT-RECORD.                         EL107
01662                                                                   EL107
01663  7150-INCREMENT-BENEFIT-INDEX.                                    EL107
01664      IF BENEFIT-INDEX < WS-MAX-BEN-CODES                             CL*21
01665          SET BENEFIT-INDEX UP BY +1                               EL107
01666          GO TO 7140-MOVE-TABLE-ENTRIES.                           EL107
01667                                                                   EL107
01668      IF WS-INDEX NOT GREATER +1                                   EL107
01669          GO TO 7180-FREEMAIN.                                     EL107
01670                                                                   EL107
01671  7160-WRITE-BENEFIT-RECORD.                                       EL107
01672      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL107
01673                                                                   EL107
01674      EXEC CICS WRITE                                              EL107
01675          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01676          RIDFLD  (CF-CONTROL-PRIMARY)                             EL107
01677          FROM    (CONTROL-FILE)                                   EL107
01678      END-EXEC.                                                    EL107
01679                                                                   EL107
01680      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              EL107
01681                                                                   EL107
01682      IF BENEFIT-INDEX < WS-MAX-BEN-CODES                             CL*21
01683          SET BENEFIT-INDEX UP BY +1                               EL107
01684          GO TO 7130-CREATE-BENEFIT-RECORD.                        EL107
01685                                                                   EL107
01686  7180-FREEMAIN.                                                   EL107
01687      EXEC CICS FREEMAIN                                           EL107
01688          DATA (CONTROL-FILE)                                      EL107
01689      END-EXEC.                                                    EL107
01690                                                                   EL107
01691  7190-EXIT.                                                       EL107
01692       EXIT.                                                       EL107
01693                                                                      CL**7
01694      EJECT                                                           CL**7
01695 *****************************************************************    CL**7
01696  7200-UPDATE-USER-TIME SECTION.                                      CL**7
01697 *****************************************************************    CL**7
01698                                                                      CL**7
01699      EXEC CICS HANDLE CONDITION                                      CL**7
01700          NOTFND  (7220-ENDBROWSE)                                    CL**7
01701          ENDFILE (7220-ENDBROWSE)                                    CL**7
01702      END-EXEC.                                                       CL**7
01703                                                                      CL**7
01704      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.            CL**7
01705                                                                      CL**7
01706      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.              CL**7
01707      MOVE PI-BENEFIT-TYPE        TO  WS-CFK-RECORD-TYPE.             CL**7
01708      MOVE LOW-VALUES             TO  WS-CFK-BENEFIT-NO.              CL**7
01709      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.             CL**7
01710                                                                      CL**7
01711      EXEC CICS STARTBR                                               CL**7
01712          DATASET   (WS-CONTROL-FILE-DSID)                            CL**7
01713          RIDFLD    (WS-CONTROL-FILE-KEY)                             CL**7
01714          GENERIC   EQUAL                                             CL**7
01715          KEYLENGTH (4)                                               CL**7
01716      END-EXEC.                                                       CL**7
01717                                                                      CL**7
01718  7210-READNEXT.                                                      CL**7
01719      EXEC CICS READNEXT                                              CL**7
01720          DATASET (WS-CONTROL-FILE-DSID)                              CL**7
01721          RIDFLD  (WS-CONTROL-FILE-KEY)                               CL**7
01722          SET     (ADDRESS OF CONTROL-FILE)                           CL*18
01723      END-EXEC.                                                       CL**7
01724                                                                      CL**7
01725      IF PI-BENEFIT-TYPE NOT = CF-RECORD-TYPE                         CL**7
01726          GO TO 7220-ENDBROWSE.                                       CL**7
01727                                                                      CL**7
01728      IF PI-UPDATE-BY = CF-LAST-MAINT-BY  AND                         CL**7
01729         PI-UPDATE-HHMMSS = CF-LAST-MAINT-HHMMSS                      CL**7
01730          GO TO 7210-READNEXT.                                        CL**7
01731                                                                      CL**7
01732      EXEC CICS ENDBR                                                 CL**7
01733          DATASET (WS-CONTROL-FILE-DSID)                              CL**7
01734      END-EXEC.                                                       CL**7
01735                                                                      CL**7
01736      EXEC CICS READ UPDATE                                           CL**7
01737          DATASET (WS-CONTROL-FILE-DSID)                              CL**7
01738          RIDFLD  (WS-CONTROL-FILE-KEY)                               CL**7
01739          SET     (ADDRESS OF CONTROL-FILE)                           CL*18
01740      END-EXEC.                                                       CL**7
01741                                                                      CL**7
01742      MOVE PI-UPDATE-BY           TO  CF-LAST-MAINT-BY.               CL**7
01743      MOVE PI-UPDATE-HHMMSS       TO  CF-LAST-MAINT-HHMMSS.           CL**7
01744      MOVE SAVE-BIN-DATE          TO  CF-LAST-MAINT-DT.               CL**7
01745                                                                      CL**7
01746      EXEC CICS REWRITE                                               CL**7
01747          DATASET ('ELCNTL')                                          CL**7
01748          FROM (CONTROL-FILE)                                         CL**7
01749      END-EXEC.                                                       CL**7
01750                                                                      CL**7
01751      EXEC CICS STARTBR                                               CL**7
01752          DATASET (WS-CONTROL-FILE-DSID)                              CL**7
01753          RIDFLD  (WS-CONTROL-FILE-KEY)                               CL**7
01754      END-EXEC.                                                       CL**7
01755                                                                      CL**7
01756      GO TO 7210-READNEXT.                                            CL**7
01757                                                                      CL**7
01758  7220-ENDBROWSE.                                                     CL**7
01759      EXEC CICS ENDBR                                                 CL**7
01760          DATASET (WS-CONTROL-FILE-DSID)                              CL**7
01761      END-EXEC.                                                       CL**7
01762                                                                   EL107
01763      EJECT                                                        EL107
01764 *****************************************************************    CL**6
01765  8000-DISPLAY-RECORDS SECTION.                                    EL107
01766 *****************************************************************    CL**6
01767                                                                   EL107
01768      EXEC CICS HANDLE CONDITION                                   EL107
01769          NOTFND  (8060-DISPLAY-RECORDS)                           EL107
01770          ENDFILE (8040-DISPLAY-RECORDS)                           EL107
01771      END-EXEC.                                                    EL107
01772                                                                   EL107
01773      SET EL107A-INDEX TO +1.                                      EL107
01774      MOVE PI-BENEFIT-NUMBER      TO  PI-LAST-BENEFIT-NUMBER.      EL107
01775                                                                   EL107
01776      EXEC CICS STARTBR                                            EL107
01777          DATASET   (WS-CONTROL-FILE-DSID)                         EL107
01778          RIDFLD    (WS-CONTROL-FILE-KEY)                          EL107
01779          GENERIC   GTEQ                                           EL107
01780          KEYLENGTH (8)                                            EL107
01781      END-EXEC.                                                    EL107
01782                                                                   EL107
01783      MOVE LOW-VALUES             TO  EL107AO.                     EL107
01784      MOVE ZERO                   TO  PI-LINE-COUNT.               EL107
01785                                                                   EL107
01786      EJECT                                                        EL107
01787  8010-DISPLAY-RECORDS.                                            EL107
01788      EXEC CICS READNEXT                                           EL107
01789          SET     (ADDRESS OF CONTROL-FILE)                           CL*18
01790          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01791          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL107
01792      END-EXEC.                                                    EL107
01793                                                                   EL107
01794      ADD +1  TO  WS-RECORD-COUNT.                                 EL107
01795                                                                      CL**6
01796      IF PI-CHANGE-SW EQUAL +1                                        CL**6
01797          IF WS-RECORD-COUNT EQUAL +1                                 CL**6
01798              MOVE CF-CONTROL-PRIMARY TO PI-UPDATE-KEY.               CL**6
01799                                                                   EL107
01800      MOVE +1                     TO  WS-INDEX.                    EL107
01801                                                                   EL107
01802 *    NOTE ******************************************************* EL107
01803 *         *      CHECK IF ALL OF THE RECORDS OF THIS BENEFIT    * EL107
01804 *         *  TYPE HAVE BEEN PROCESSED.                          * EL107
01805 *         ******************************************************* EL107
01806                                                                   EL107
01807      IF WS-CFK-COMPANY-ID NOT = PI-COMPANY-ID  OR                 EL107
01808         CF-RECORD-TYPE    NOT = PI-BENEFIT-TYPE                   EL107
01809          GO TO 8015-DISPLAY-RECORDS.                              EL107
01810                                                                   EL107
01811 *    NOTE ******************************************************* EL107
01812 *         *      IF WS-READNEXT SW IS ON (+1), THIS READ IS     * EL107
01813 *         *  JUST TO FIND OUT THE KEY OF THE NEXT RECORD.       * EL107
01814 *         ******************************************************* EL107
01815                                                                   EL107
01816      IF WS-READNEXT-SW = +1                                       EL107
01817          MOVE CF-BENEFIT-CONTROLS (1) TO WS-BENEFIT-CONTROLS-WORK EL107
01818          MOVE WS-BENEFIT-NUMBER       TO PI-NEXT-BENEFIT-NUMBER   EL107
01819          GO TO 8050-DISPLAY-RECORDS.                              EL107
01820                                                                   EL107
01821      GO TO 8020-DISPLAY-RECORDS.                                  EL107
01822                                                                   EL107
01823  8015-DISPLAY-RECORDS.                                            EL107
01824      IF WS-READNEXT-SW = +1                                       EL107
01825          MOVE SPACES             TO  PI-NEXT-BENEFIT-NUMBER          CL**3
01826          MOVE ZERO               TO  PI-BROWSE-SW                    CL**3
01827          GO TO 8050-DISPLAY-RECORDS.                              EL107
01828                                                                   EL107
01829      IF PI-MODE = 'A'                                             EL107
01830          GO TO 8050-DISPLAY-RECORDS.                              EL107
01831                                                                   EL107
01832      MOVE SPACES                 TO  PI-NEXT-BENEFIT-NUMBER          CL**3
01833      MOVE ZERO                   TO  PI-BROWSE-SW.                   CL**3
01834                                                                   EL107
01835      IF PI-LINE-COUNT NOT GREATER ZERO                            EL107
01836          MOVE ER-0028            TO   EMI-ERROR                   EL107
01837          IF PI-MODE = 'C' OR 'D'                                  EL107
01838              MOVE -1             TO  ABENEL                       EL107
01839              MOVE AL-UABON       TO  ABENEA                          CL**5
01840              MOVE ZERO           TO  PI-1ST-TIME-SW.              EL107
01841                                                                   EL107
01842      GO TO 8040-DISPLAY-RECORDS.                                  EL107
01843                                                                   EL107
01844      EJECT                                                        EL107
01845  8020-DISPLAY-RECORDS.                                            EL107
01846      IF LCP-ONCTR-01 =  0                                            CL*18
01847          ADD 1 TO LCP-ONCTR-01                                       CL*18
01848          MOVE CF-LAST-MAINT-BY      TO  PI-UPDATE-BY              EL107
01849          MOVE CF-LAST-MAINT-HHMMSS  TO  PI-UPDATE-HHMMSS          EL107
01850        ELSE                                                       EL107
01851          GO TO 8030-DISPLAY-RECORDS.                              EL107
01852                                                                   EL107
01853 *    NOTE ******************************************************* EL107
01854 *         *      THE FOLLOWING LOGIC IS TO BYPASS ALL TABLE     * EL107
01855 *         *  ENTRIES UNTIL THE ENTRY INQUIRED IS THE 1ST ONE    * EL107
01856 *         *  TO BE PROCESSED.                                   * EL107
01857 *         ******************************************************* EL107
01858                                                                   EL107
01859      IF PI-BROWSE-SW = ZERO                                       EL107
01860          GO TO 8030-DISPLAY-RECORDS.                              EL107
01861                                                                   EL107
01862      MOVE ER-0028                TO  EMI-ERROR.                   EL107
01863                                                                   EL107
01864  8025-DISPLAY-RECORDS.                                            EL107
01865      MOVE CF-BENEFIT-CONTROLS (WS-INDEX)                          EL107
01866                                  TO  WS-BENEFIT-CONTROLS-WORK.    EL107
01867                                                                   EL107
01868      IF WS-BENEFIT-NUMBER NOT LESS PI-BENEFIT-NUMBER              EL107
01869          GO TO 8030-DISPLAY-RECORDS.                              EL107
01870                                                                   EL107
01871      IF WS-INDEX LESS +8                                          EL107
01872          ADD +1  TO  WS-INDEX                                     EL107
01873          GO TO 8025-DISPLAY-RECORDS.                              EL107
01874                                                                   EL107
01875      GO TO 8010-DISPLAY-RECORDS.                                  EL107
01876                                                                   EL107
01877      EJECT                                                        EL107
01878  8030-DISPLAY-RECORDS.                                            EL107
01879 *    NOTE ******************************************************* EL107
01880 *         *          MOVE THE TABLE ENTRY TO THE MAP.           * EL107
01881 *         ******************************************************* EL107
01882                                                                   EL107
01883      MOVE CF-BENEFIT-CONTROLS (WS-INDEX)                          EL107
01884                                  TO  WS-BENEFIT-CONTROLS-WORK.    EL107
01885                                                                   EL107
01886      IF WS-BENEFIT-NUMBER = ZERO                                  EL107
01887          GO TO 8015-DISPLAY-RECORDS.                              EL107
01888                                                                   EL107
01889      IF PI-BENEFIT-NUMBER = WS-BENEFIT-NUMBER                     EL107
01890          MOVE ZERO   TO  EMI-ERROR.                               EL107
01891                                                                   EL107
01892      ADD +1  TO  PI-LINE-COUNT.                                   EL107
01893                                                                   EL107
01894      MOVE WS-BENEFIT-NUMBER      TO EL107A-CODE-O (EL107A-INDEX). EL107
01895      MOVE WS-BENEFIT-ABBREVIATION TO EL107A-ABBR-O (EL107A-INDEX).   CL*18
01896      MOVE WS-BENEFIT-DESCRIPTION TO EL107A-DESC-O (EL107A-INDEX). EL107
01897      MOVE WS-BENEFIT-COMMENT   TO EL107A-COMMENT-O (EL107A-INDEX).EL107
051414     if pi-benefit-type not = '4'
051414        if ws-benefit-max-bens not = zeros
051414           MOVE WS-BENEFIT-MAX-BENS
051414                         TO EL107A-MAX-BENS-O (EL107A-INDEX)
051414        end-if
051414     end-if
01898      MOVE WS-BENEFIT-LOAN-TYPE   TO EL107A-LOAN-O (EL107A-INDEX). EL107
01899      MOVE WS-BENEFIT-EARN-METHOD TO EL107A-EM-O   (EL107A-INDEX). EL107
01900      MOVE WS-BENEFIT-JOINT-COVERAGE                               EL107
01901                                  TO EL107A-JOINT-O (EL107A-INDEX).EL107
01902      MOVE WS-BENEFIT-OUTSTANDING-BAL                              EL107
01903                                    TO EL107A-OB-O (EL107A-INDEX). EL107
01904      MOVE WS-BENEFIT-COVERAGE-TYPE TO EL107A-LOD-O (EL107A-INDEX).EL107
01905      MOVE WS-BENEFIT-REMAIN-TERM   TO EL107A-RTM-O (EL107A-INDEX).EL107
01906      MOVE WS-BENEFIT-REFUND-METHOD TO EL107A-RFM-O (EL107A-INDEX).EL107
01907      MOVE WS-BENEFIT-IG-CODE       TO EL107A-IGC-O (EL107A-INDEX).EL107
082503     MOVE WS-BENEFIT-CATEGORY      TO EL107A-CAC-O (EL107A-INDEX)
01908                                                                   EL107
01909      MOVE AL-SANON  TO  EL107A-CODE-ATTRB (EL107A-INDEX).         EL107
01910                                                                   EL107
01911      IF PI-BENEFIT-TYPE NOT = '4'                                 EL107
01912          MOVE AL-SADOF TO EL107A-LOD-ATTRB (EL107A-INDEX)         EL107
01913          MOVE 'E J S  '          TO  AHEAD1O                         CL*14
01914          MOVE 'M T P  '          TO  AHEAD2O.                        CL*14
01915                                                                   EL107
01916  8033-DISPLAY-RECORDS.                                            EL107
01917      IF EL107A-INDEX LESS +8                                      EL107
01918          SET EL107A-INDEX UP BY +1                                EL107
01919          GO TO 8035-DISPLAY-RECORDS.                              EL107
01920                                                                   EL107
01921      IF WS-INDEX NOT LESS +8                                      EL107
01922          MOVE +1                 TO  WS-READNEXT-SW               EL107
01923          GO TO 8010-DISPLAY-RECORDS                               EL107
01924        ELSE                                                       EL107
01925          GO TO 8050-DISPLAY-RECORDS.                              EL107
01926                                                                   EL107
01927  8035-DISPLAY-RECORDS.                                            EL107
01928      IF WS-INDEX LESS +8                                          EL107
01929          ADD +1  TO  WS-INDEX                                     EL107
01930          MOVE CF-BENEFIT-CONTROLS (WS-INDEX)                      EL107
01931                                  TO  WS-BENEFIT-CONTROLS-WORK     EL107
01932          MOVE WS-BENEFIT-NUMBER  TO  PI-NEXT-BENEFIT-NUMBER       EL107
01933          GO TO 8030-DISPLAY-RECORDS.                              EL107
01934                                                                   EL107
01935      IF PI-MODE = 'C' OR 'D'                                      EL107
01936          GO TO 8040-DISPLAY-RECORDS.                              EL107
01937                                                                   EL107
01938      IF PI-MODE = 'A'                                             EL107
01939          GO TO 8050-DISPLAY-RECORDS.                              EL107
01940                                                                   EL107
01941      GO TO 8010-DISPLAY-RECORDS.                                  EL107
01942                                                                   EL107
01943      EJECT                                                        EL107
01944  8040-DISPLAY-RECORDS.                                            EL107
01945      IF EL107A-INDEX GREATER +8                                   EL107
01946          GO TO 8050-DISPLAY-RECORDS.                              EL107
01947                                                                   EL107
01948      IF PI-BENEFIT-TYPE NOT = '4'                                 EL107
01949          MOVE AL-SADOF TO EL107A-LOD-ATTRB   (EL107A-INDEX)       EL107
01950          MOVE 'E J S  '          TO  AHEAD1O                         CL*14
01951          MOVE 'M T P  '          TO  AHEAD2O.                        CL*14
01952                                                                   EL107
01953      IF PI-MODE = 'C'                                                CL**6
01954             AND PI-CHANGE-SW = +1                                    CL**6
01955          MOVE AL-SADOF  TO  EL107A-CODE-ATTRB    (EL107A-INDEX)      CL**6
01956                             EL107A-ABBR-ATTRB    (EL107A-INDEX)      CL**6
01957                             EL107A-DESC-ATTRB    (EL107A-INDEX)      CL**6
01958                             EL107A-COMMENT-ATTRB (EL107A-INDEX)      CL**6
051414                            EL107A-MAX-BENS-ATTRB (EL107A-INDEX)
01959                             EL107A-LOAN-ATTRB    (EL107A-INDEX)      CL**6
01960                             EL107A-EM-ATTRB      (EL107A-INDEX)      CL**6
01961                             EL107A-JOINT-ATTRB   (EL107A-INDEX)      CL**6
01962                             EL107A-OB-ATTRB      (EL107A-INDEX)      CL**6
01963                             EL107A-LOD-ATTRB     (EL107A-INDEX)      CL**6
01964                             EL107A-RTM-ATTRB     (EL107A-INDEX)      CL**6
01965                             EL107A-RFM-ATTRB     (EL107A-INDEX)      CL**6
01966                             EL107A-IGC-ATTRB     (EL107A-INDEX)
082503                            EL107A-CAC-ATTRB     (EL107A-INDEX).
01967                                                                      CL**6
01968      SET EL107A-INDEX UP BY +1.                                      CL*21
01969      GO TO 8040-DISPLAY-RECORDS.                                     CL*21
01970                                                                   EL107
01971  8050-DISPLAY-RECORDS.                                            EL107
01972      EXEC CICS ENDBR                                              EL107
01973          DATASET (WS-CONTROL-FILE-DSID)                           EL107
01974      END-EXEC.                                                    EL107
01975                                                                   EL107
01976      IF PI-MODE = 'C'                                             EL107
01977          GO TO 8070-DISPLAY-RECORDS.                              EL107
01978                                                                   EL107
01979      IF PI-MODE = 'A'                                             EL107
01980          MOVE -1  TO  EL107A-CODE-LENGTH (EL107A-INDEX)           EL107
01981          GO TO 8070-DISPLAY-RECORDS.                              EL107
01982                                                                   EL107
01983      MOVE -1                     TO  AMAINTL.                     EL107
01984      GO TO 8070-DISPLAY-RECORDS.                                  EL107
01985                                                                   EL107
01986  8060-DISPLAY-RECORDS.                                            EL107
01987      MOVE ER-0006                TO  EMI-ERROR.                   EL107
01988      MOVE -1                     TO  ABENEL.                      EL107
01989      MOVE AL-UABOF               TO  ABENEA.                         CL**5
01990                                                                   EL107
01991  8070-DISPLAY-RECORDS.                                            EL107
01992      IF PI-MODE = 'A'                                             EL107
01993          NEXT SENTENCE                                            EL107
01994        ELSE                                                       EL107
01995          GO TO 8090-DISPLAY-RECORDS.                              EL107
01996                                                                   EL107
01997  8080-DISPLAY-RECORDS.                                            EL107
01998      IF PI-BENEFIT-TYPE NOT = '4'                                 EL107
01999          MOVE AL-SADOF  TO  EL107A-LOD-ATTRB (EL107A-INDEX).         CL*14
02000                                                                   EL107
02001      IF PI-MODE = 'C'                                                CL**6
02002             AND PI-CHANGE-SW = +1                                    CL**6
02003          MOVE AL-SADOF  TO  EL107A-CODE-ATTRB    (EL107A-INDEX)      CL**6
02004                             EL107A-ABBR-ATTRB    (EL107A-INDEX)      CL**6
02005                             EL107A-DESC-ATTRB    (EL107A-INDEX)      CL**6
02006                             EL107A-COMMENT-ATTRB (EL107A-INDEX)      CL**6
051414                            EL107A-MAX-BENS-ATTRB (EL107A-INDEX)
02007                             EL107A-LOAN-ATTRB    (EL107A-INDEX)      CL**6
02008                             EL107A-EM-ATTRB      (EL107A-INDEX)      CL**6
02009                             EL107A-JOINT-ATTRB   (EL107A-INDEX)      CL**6
02010                             EL107A-OB-ATTRB      (EL107A-INDEX)      CL**6
02011                             EL107A-LOD-ATTRB     (EL107A-INDEX)      CL**6
02012                             EL107A-RTM-ATTRB     (EL107A-INDEX)      CL**6
02013                             EL107A-RFM-ATTRB     (EL107A-INDEX)      CL**6
02014                             EL107A-IGC-ATTRB     (EL107A-INDEX)
082603                            EL107A-CAC-ATTRB     (EL107A-INDEX).
02015                                                                   EL107
02016      IF EL107A-INDEX LESS +8                                      EL107
02017          SET EL107A-INDEX UP BY +1                                EL107
02018          GO TO 8080-DISPLAY-RECORDS.                              EL107
02019                                                                   EL107
02020  8090-DISPLAY-RECORDS.                                            EL107
02021      IF PI-BENEFIT-TYPE = '4'                                     EL107
02022          MOVE PI-LIFE-OVERRIDE-L1 TO  AKINDO                      EL107
02023      ELSE                                                         EL107
02024          MOVE PI-AH-OVERRIDE-L1   TO  AKINDO.                        CL**6
02025                                                                      CL**6
02026      MOVE AL-UANON                TO  AKINDA.                        CL**6
02027                                                                      CL**6
02028      IF PI-MODE = 'C'                                                CL**6
02029          IF EMI-ERROR = ER-0028                                      CL**6
02030              MOVE -1                  TO  ABENEL                     CL**6
02031          ELSE                                                        CL**6
02032          IF EMI-ERROR = ZERO AND PI-CHANGE-SW = +1                   CL**6
02033              MOVE AL-PANON            TO  AKINDA                     CL**6
02034                                           ABENEA                     CL**6
02035              MOVE -1                  TO  EL107A-ABBR-LENGTH (1)     CL**6
02036              MOVE +2 TO  PI-CHANGE-SW                                CL**6
02037              MOVE ER-7535 TO  EMI-ERROR.                             CL**6
02038                                                                   EL107
02039      MOVE PI-MODE                TO  AMAINTO.                     EL107
02040      MOVE AL-UANON               TO  AMAINTA.                     EL107
02041                                                                   EL107
02042      IF EMI-ERROR NOT = ZERO AND ER-7535                             CL**6
02043          MOVE +2                 TO  PI-SHOW-SW.                  EL107
02044                                                                   EL107
02045      PERFORM 8100-SEND-INITIAL-MAP.                               EL107
02046                                                                   EL107
02047      IF PI-MODE = 'S'   AND                                       EL107
02048         PI-NEXT-BENEFIT-NUMBER NOT = SPACES                          CL**3
02049          MOVE +1                 TO  PI-BROWSE-SW.                EL107
02050                                                                   EL107
02051      GO TO 9100-RETURN-TRAN.                                      EL107
02052                                                                   EL107
02053      EJECT                                                        EL107
02054 *****************************************************************    CL**6
02055  8100-SEND-INITIAL-MAP SECTION.                                   EL107
02056 *****************************************************************    CL**6
02057                                                                      CL**6
02058      IF TRANSACTION-SUCCESSFUL OR                                    CL**6
02059         INITIAL-TRANSACTION OR                                       CL**6
02060         CHANGE-SUCCESSFUL                                            CL**6
02061          NEXT SENTENCE                                               CL**6
02062      ELSE                                                            CL**6
02063          GO TO 8100-SEND-MAP.                                     EL107
02064                                                                   EL107
02065      MOVE -1                     TO  AMAINTL.                     EL107
02066      MOVE ZERO                   TO  PI-BROWSE-SW.                EL107
02067                                                                      CL**6
02068      IF CHANGE-SUCCESSFUL                                            CL**6
02069          GO TO 8100-SEND-MAP.                                        CL**6
02070                                                                      CL**6
02071      SET EL107A-INDEX TO +1.                                      EL107
02072                                                                   EL107
02073  8100-INITALIZE-MAP-LINE.                                         EL107
02074      MOVE AL-SADOF  TO  EL107A-CODE-ATTRB    (EL107A-INDEX)       EL107
02075                         EL107A-ABBR-ATTRB    (EL107A-INDEX)       EL107
02076                         EL107A-DESC-ATTRB    (EL107A-INDEX)       EL107
02077                         EL107A-COMMENT-ATTRB (EL107A-INDEX)       EL107
051414                        EL107A-MAX-BENS-ATTRB (EL107A-INDEX)
02078                         EL107A-LOAN-ATTRB    (EL107A-INDEX)       EL107
02079                         EL107A-EM-ATTRB      (EL107A-INDEX)       EL107
02080                         EL107A-JOINT-ATTRB   (EL107A-INDEX)       EL107
02081                         EL107A-OB-ATTRB      (EL107A-INDEX)       EL107
02082                         EL107A-LOD-ATTRB     (EL107A-INDEX)       EL107
02083                         EL107A-RTM-ATTRB     (EL107A-INDEX)       EL107
02084                         EL107A-RFM-ATTRB     (EL107A-INDEX)       EL107
02085                         EL107A-IGC-ATTRB     (EL107A-INDEX)
082603                        EL107A-CAC-ATTRB     (EL107A-INDEX)
02086                                                                   EL107
02087      IF EL107A-INDEX LESS +8                                      EL107
02088          SET EL107A-INDEX UP BY +1                                EL107
02089          GO TO 8100-INITALIZE-MAP-LINE.                           EL107
02090                                                                   EL107
02091  8100-SEND-MAP.                                                   EL107
02092      MOVE SAVE-DATE              TO  ADATEO.                      EL107
02093      MOVE EIBTIME                TO  TIME-IN.                     EL107
02094      MOVE TIME-OUT               TO  ATIMEO.                      EL107
02095                                                                   EL107
02096      MOVE PI-LIFE-OVERRIDE-L1    TO  WS-HD0-LF-L1.                EL107
02097      MOVE PI-AH-OVERRIDE-L1      TO  WS-HD0-AH-L1.                EL107
02098      MOVE WS-HEADING-0           TO  AHEAD0O.                     EL107
02099                                                                   EL107
02100      IF EMI-ERROR NOT = ZERO                                      EL107
02101          PERFORM 9900-ERROR-FORMAT                                EL107
02102        ELSE                                                       EL107
02103          IF TRANSACTION-SUCCESSFUL                                EL107
02104              PERFORM 9900-ERROR-FORMAT                               CL**6
02105          ELSE                                                        CL**6
02106            IF CHANGE-SUCCESSFUL                                      CL**6
02107                PERFORM 9900-ERROR-FORMAT.                            CL**6
02108                                                                   EL107
02109      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    EL107
02110      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                    EL107
02111                                                                   EL107
02112      EXEC CICS SEND                                               EL107
02113          FROM   (EL107AO)                                         EL107
02114          MAPSET (WS-MAPSET-NAME)                                  EL107
02115          MAP    (WS-MAP-NAME)                                     EL107
02116          CURSOR ERASE                                             EL107
02117      END-EXEC.                                                    EL107
02118                                                                   EL107
02119  8100-EXIT.                                                       EL107
02120       EXIT.                                                       EL107
02121                                                                   EL107
02122      EJECT                                                        EL107
02123 *****************************************************************    CL**6
02124  8200-SEND-DATAONLY SECTION.                                      EL107
02125 *****************************************************************    CL**6
02126                                                                      CL**6
02127      MOVE SAVE-DATE              TO  ADATEO.                      EL107
02128      MOVE EIBTIME                TO  TIME-IN.                     EL107
02129      MOVE TIME-OUT               TO  ATIMEO.                      EL107
02130                                                                   EL107
02131      MOVE PI-LIFE-OVERRIDE-L1    TO  WS-HD0-LF-L1.                EL107
02132      MOVE PI-AH-OVERRIDE-L1      TO  WS-HD0-AH-L1.                EL107
02133      MOVE WS-HEADING-0           TO  AHEAD0O.                     EL107
02134                                                                   EL107
02135      IF EMI-ERROR NOT = ZERO                                      EL107
02136          PERFORM 9900-ERROR-FORMAT.                               EL107
02137                                                                   EL107
02138      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    EL107
02139      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                    EL107
02140                                                                   EL107
02141      EXEC CICS SEND DATAONLY                                      EL107
02142          FROM   (EL107AO)                                         EL107
02143          MAPSET (WS-MAPSET-NAME)                                  EL107
02144          MAP    (WS-MAP-NAME)                                     EL107
02145          CURSOR                                                   EL107
02146      END-EXEC.                                                    EL107
02147                                                                   EL107
02148  8200-EXIT.                                                       EL107
02149       EXIT.                                                       EL107
02150                                                                   EL107
02151      EJECT                                                        EL107
02152 *****************************************************************    CL**6
02153  8300-SEND-TEXT SECTION.                                          EL107
02154 *****************************************************************    CL**6
02155                                                                      CL**6
02156      EXEC CICS SEND TEXT                                          EL107
02157          FROM   (LOGOFF-TEXT)                                     EL107
02158          LENGTH (LOGOFF-LENGTH)                                   EL107
02159          ERASE  FREEKB                                            EL107
02160      END-EXEC.                                                    EL107
02161                                                                   EL107
02162      EXEC CICS RETURN                                             EL107
02163      END-EXEC.                                                    EL107
02164                                                                   EL107
02165  8300-EXIT.                                                       EL107
02166       EXIT.                                                       EL107
02167                                                                   EL107
02168      EJECT                                                        EL107
02169 *****************************************************************    CL**6
02170  8400-LOG-JOURNAL-RECORD SECTION.                                 EL107
02171 *****************************************************************    CL**6
02172                                                                   EL107
02173      IF PI-JOURNAL-FILE-ID = 0                                       CL*21
02174          GO TO 8400-EXIT.                                         EL107
02175                                                                   EL107
02176      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL107
02177      MOVE WS-CONTROL-FILE-DSID   TO  JP-FILE-ID.                  EL107
02178      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL107
02179                                                                   EL107
pemuni*    EXEC CICS JOURNAL                                            EL107
pemuni*        JFILEID (PI-JOURNAL-FILE-ID)                             EL107
pemuni*        JTYPEID (WS-JOURNAL-TYPE-ID)                             EL107
pemuni*        FROM    (JOURNAL-RECORD)                                 EL107
pemuni*        LENGTH  (WS-JOURNAL-RECORD-LENGTH)                       EL107
pemuni*    END-EXEC.                                                    EL107
02186                                                                   EL107
02187  8400-EXIT.                                                       EL107
02188       EXIT.                                                       EL107
02189                                                                   EL107
02190 *****************************************************************    CL**6
02191  8500-DATE-CONVERSION SECTION.                                    EL107
02192 *****************************************************************    CL**6
02193                                                                      CL**6
02194      EXEC CICS LINK                                               EL107
02195          PROGRAM  ('ELDATCV')                                     EL107
02196          COMMAREA (DATE-CONVERSION-DATA)                          EL107
02197          LENGTH   (DC-COMM-LENGTH)                                EL107
02198      END-EXEC.                                                    EL107
02199                                                                   EL107
02200  8500-EXIT.                                                       EL107
02201       EXIT.                                                       EL107
02202                                                                   EL107
02203      EJECT                                                        EL107
02204 *****************************************************************    CL**6
02205  9000-RETURN-CICS SECTION.                                        EL107
02206 *****************************************************************    CL**6
02207                                                                      CL**6
02208      MOVE 'EL005'                TO  THIS-PGM.                    EL107
02209      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL107
02210      PERFORM 9300-XCTL.                                           EL107
02211                                                                   EL107
02212  9000-EXIT.                                                       EL107
02213       EXIT.                                                       EL107
02214                                                                   EL107
02215 *****************************************************************    CL**6
02216  9100-RETURN-TRAN SECTION.                                        EL107
02217 *****************************************************************    CL**6
02218                                                                      CL**6
02219      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL107
02220      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL107
02221                                                                   EL107
02222      EXEC CICS RETURN                                             EL107
02223          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL107
02224          LENGTH   (PI-COMM-LENGTH)                                EL107
02225          TRANSID  (WS-TRANS-ID)                                   EL107
02226      END-EXEC.                                                    EL107
02227                                                                   EL107
02228  9100-EXIT.                                                       EL107
02229       EXIT.                                                       EL107
02230                                                                   EL107
02231 *****************************************************************    CL**6
02232  9300-XCTL SECTION.                                               EL107
02233 *****************************************************************    CL**6
02234                                                                      CL**6
02235      MOVE DFHENTER               TO  EIBAID.                      EL107
02236                                                                   EL107
02237      EXEC CICS XCTL                                               EL107
02238          PROGRAM  (THIS-PGM)                                      EL107
02239          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL107
02240          LENGTH   (PI-COMM-LENGTH)                                EL107
02241      END-EXEC.                                                    EL107
02242                                                                   EL107
02243  9300-EXIT.                                                       EL107
02244       EXIT.                                                       EL107
02245                                                                   EL107
02246      EJECT                                                        EL107
02247 *****************************************************************    CL**6
02248  9400-CLEAR SECTION.                                              EL107
02249 *****************************************************************    CL**6
02250                                                                      CL**6
02251      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                     EL107
02252      PERFORM 9300-XCTL.                                           EL107
02253                                                                   EL107
02254  9400-EXIT.                                                       EL107
02255       EXIT.                                                       EL107
02256                                                                   EL107
02257 *****************************************************************    CL**6
02258  9600-PGMIDERR SECTION.                                           EL107
02259 *****************************************************************    CL**6
02260                                                                      CL**6
02261      EXEC CICS HANDLE CONDITION                                   EL107
02262          PGMIDERR (8300-SEND-TEXT)                                EL107
02263      END-EXEC.                                                    EL107
02264                                                                   EL107
02265      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL107
02266                                      LOGOFF-PGM.                  EL107
02267                                                                   EL107
02268      MOVE 'EL005'                TO  THIS-PGM.                    EL107
02269      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL107
02270      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL107
02271      PERFORM 9300-XCTL.                                           EL107
02272                                                                   EL107
02273  9600-EXIT.                                                       EL107
02274       EXIT.                                                       EL107
02275                                                                   EL107
02276 *****************************************************************    CL**6
02277  9900-ERROR-FORMAT SECTION.                                       EL107
02278 *****************************************************************    CL**6
02279                                                                      CL**6
02280      IF EMI-ERRORS-COMPLETE                                       EL107
02281          ADD +1             TO  EMI-FATAL-CTR                     EL107
02282          MOVE ZERO          TO  EMI-ERROR                         EL107
02283          GO TO 9900-EXIT.                                         EL107
02284                                                                   EL107
02285      EXEC CICS LINK                                               EL107
02286          PROGRAM  ('EL001')                                       EL107
02287          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL107
02288          LENGTH   (EMI-COMM-LENGTH)                               EL107
02289      END-EXEC.                                                    EL107
02290                                                                   EL107
02291      MOVE ZERO              TO  EMI-ERROR.                        EL107
02292                                                                   EL107
02293  9900-EXIT.                                                       EL107
02294       EXIT.                                                       EL107
02295                                                                   EL107
02296      EJECT                                                        EL107
02297 *****************************************************************    CL**6
02298  9990-ERROR SECTION.                                              EL107
02299 *****************************************************************    CL**6
02300                                                                      CL**6
02301      MOVE DFHEIBLK               TO EMI-LINE1.                    EL107
02302      EXEC CICS LINK                                               EL107
02303          PROGRAM   ('EL004')                                      EL107
02304          COMMAREA  (EMI-LINE1)                                    EL107
02305          LENGTH    (72)                                           EL107
02306      END-EXEC.                                                    EL107
02307                                                                   EL107
02308      PERFORM 8200-SEND-DATAONLY.                                  EL107
02309      GO TO 9100-RETURN-TRAN.                                      EL107
02310                                                                   EL107
02311  9990-EXIT.                                                       EL107
02312       EXIT.                                                       EL107
02313                                                                   EL107
02314 *****************************************************************    CL**6
02315  9995-SECURITY-VIOLATION.                                         EL107
02316 *****************************************************************    CL**6
02317                                                                      CL**6
02318             COPY ELCSCTP.                                         EL107
02319                                                                   EL107
02320  9995-EXIT.                                                       EL107
02321       EXIT.                                                       EL107
02322                                                                   EL107
02323  9999-LAST-PARAGRAPH SECTION.                                     EL107
02324      GOBACK.                                                      EL107
