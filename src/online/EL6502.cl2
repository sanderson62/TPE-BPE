00001  ID DIVISION.                                                     05/16/95
00002                                                                   EL6502
00003  PROGRAM-ID.                 EL6502.                                 LV018
00004 *              PROGRAM CONVERTED BY                                  CL*17
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*17
00006 *              CONVERSION DATE 09/06/94 09:23:05.                    CL*17
00007 *                            VMOD=2.018                              CL*18
00008 *                                                                 EL6502
00009 *AUTHOR.     LOGIC,INC.                                              CL*17
00010 *            DALLAS, TEXAS.                                          CL*17
00011                                                                   EL6502
00012 *DATE-COMPILED.                                                      CL*17
00013 *SECURITY.   *****************************************************   CL*17
00014 *            *                                                   *   CL*17
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*17
00016 *            *                                                   *   CL*17
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*17
00018 *                                                                *   CL*17
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*17
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*17
00021 *            *                                                   *   CL*17
00022 *            *****************************************************   CL*17
00023 *                                                                    CL**6
00024 *REMARKS.    TRANSACTION - EXG6 - ACCOUNT MAINT (MISC. ACCT DATA).   CL**6
00025 *                                                                    CL**6
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL6502AI FILLER
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
092711* 092711  CR2011092100002  AJRA  ADD ORIG DEALER NO
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
101101******************************************************************

00026  ENVIRONMENT DIVISION.                                            EL6502
00027                                                                   EL6502
00028      EJECT                                                        EL6502
00029  DATA DIVISION.                                                   EL6502
00030  WORKING-STORAGE SECTION.                                         EL6502
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6502
00032  77  FILLER  PIC X(32)  VALUE '*    EL6502 WORKING STORAGE    *'. EL6502
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.018 *********'.    CL*18
00034                                                                   EL6502
00035  01  WS-DATE-AREA.                                                EL6502
00036      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL6502
00037      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.       CL**4
00038                                                                   EL6502
00039  01  STANDARD-AREAS.                                              EL6502
00040      12  GETMAIN-SPACE               PIC X       VALUE SPACE.        CL*11
00041      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.     CL**4
00042      12  COMP-REC-LEN                PIC S9(4) COMP VALUE +700.      CL*11
00043      12  MAP-NAME                    PIC X(8)    VALUE 'EL6502A'.    CL**4
00044      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6502S'.    CL**4
00045      12  SCREEN-NUMBER               PIC X(4)    VALUE '650C'.    EL6502
00046      12  TRANS-ID                    PIC X(4)    VALUE 'EXC6'.    EL6502
00047      12  THIS-PGM                    PIC X(8)    VALUE 'EL6502'.  EL6502
00048      12  PGM-NAME                    PIC X(8).                    EL6502
00049      12  TIME-IN                     PIC S9(7).                   EL6502
00050      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6502
00051          16  FILLER                  PIC X.                       EL6502
00052          16  TIME-OUT                PIC 99V99.                   EL6502
00053          16  FILLER                  PIC XX.                         CL**4
00054      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL6502
00055      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL6502
00056      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.   EL6502
00057      12  XCTL-650                    PIC X(8)    VALUE 'EL650'.   EL6502
00058      12  XCTL-6501                   PIC X(8)    VALUE 'EL6501'.  EL6502
00059      12  XCTL-6503                   PIC X(8)    VALUE 'EL6503'.  EL6502
00060      12  XCTL-6504                   PIC X(8)    VALUE 'EL6504'.  EL6502
00061      12  XCTL-6505                   PIC X(8)    VALUE 'EL6505'.     CL**2
00062      12  XCTL-6506                   PIC X(8)    VALUE 'EL6506'.  EL6502
00063      12  XCTL-6507                   PIC X(8)    VALUE 'EL6507'.     CL**6
00064      12  XCTL-6508                   PIC X(8)    VALUE 'EL6508'.     CL*13
00065      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL6502
00066      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL6502
00067      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL6502
00068      12  FILE-ID                     PIC X(8)    VALUE SPACES.    EL6502
00069      12  ERACCT-FILE                 PIC X(8)    VALUE 'ERACCT'.     CL**6
00070      12  ERRATE-FILE                 PIC X(8)    VALUE 'ERRATE'.     CL**6
00071      12  ELCNTL-FILE                 PIC X(8)    VALUE 'ELCNTL'.     CL**6
00072      12  ERCOMP-FILE                 PIC X(8)    VALUE 'ERCOMP'.     CL*11
00073      12  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.    EL6502
00074      12  YMD-CURRENT-SAVE            PIC X(6)    VALUE SPACES.    EL6502
00075                                                                   EL6502
00076      12  ERACCT-LENGTH               PIC S9(4)   VALUE +2023 COMP.EL6502
00077      12  ELCNTL-LENGTH               PIC S9(4)   VALUE +773  COMP.   CL*11
00078      12  ERCOMP-LENGTH               PIC S9(4)   VALUE +773  COMP.   CL*11
00079      12  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.   CL**6
00080      12  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.   CL**6
00081      12  SUB1                        PIC S9(4)   VALUE +0    COMP.EL6502
00082      12  SUB2                        PIC S9(4)   VALUE +0    COMP.EL6502
00083                                                                   EL6502
00084      12  DEEDIT-FIELD                PIC X(15).                      CL**6
00085      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6502
00086      12  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(13)V99.   EL6502
00087      12  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).    CL**6
00088      12  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).    CL**6
00089      12  DEEDIT-FIELD-V6  REDEFINES DEEDIT-FIELD PIC S9(9)V9(6).     CL**6
00090                                                                      CL**6
00091      12  WS-EDIT-FIELD-CONV          PIC S9(4)   VALUE +0.           CL**6
00092      12  WS-TOT-PERCENT              PIC S9V9999 VALUE +0.           CL**6
00093                                                                      CL**6
00094      12  RATE-KEY.                                                   CL**6
00095          16  RATE-COMP-CD            PIC X.                          CL**6
00096          16  RATE-STATE              PIC XX.                         CL**6
00097          16  RATE-CLASS              PIC XX.                         CL**6
00098          16  RATE-DEV                PIC XXX.                        CL**6
00099          16  FILLER                  PIC X(20).                      CL**6
00100                                                                      CL**6
00101      12  W-CANCEL-FEE-LONG           PIC S9(3)V99 VALUE ZEROS.       CL*17
00102      12  W-CANCEL-FEE                PIC S99      VALUE ZEROS.       CL*17
00103                                                                      CL*17
00104      12  W-EXEC1-DIS-PRCNT           PIC S9V9(4)  VALUE ZEROS.       CL*14
00105      12  W-EXEC1-LIF-PRCNT           PIC S9V9(4)  VALUE ZEROS.       CL*14
00106      12  W-EXEC2-DIS-PRCNT           PIC S9V9(4)  VALUE ZEROS.       CL*14
00107      12  W-EXEC2-LIF-PRCNT           PIC S9V9(4)  VALUE ZEROS.       CL*14
00108                                                                      CL*14
00109      12  W-CRDT-MOD-PCT              PIC S9V9(4)  VALUE ZEROS.       CL**6
00110      12  W-LIFE-IBNR-PCT             PIC S9V9(4)  VALUE ZEROS.       CL**6
00111      12  W-TARGET-LOSS-RATIO         PIC S9V9(4)  VALUE ZEROS.       CL**6
00112                                                                   EL6502
00113      12  WS-LF-DEV-PERCENT           PIC S9V9(6)  VALUE ZEROS.       CL**6
00114      12  WS-AH-DEV-PERCENT           PIC S9V9(6)  VALUE ZEROS.       CL**6
00115                                                                   EL6502
00116      12  WS-OB-LF-RATE               PIC S99V9(5) VALUE ZEROS.       CL**6
00117      12  WS-OB-JNTLF-RATE            PIC S99V9(5) VALUE ZEROS.       CL**6
00118      12  WS-OB-AH-RATE               PIC S99V9(5) VALUE ZEROS.       CL**6
00119                                                                   EL6502
00120      12  WS-TOL-PREM                 PIC S999V99    VALUE ZEROS.     CL**6
00121      12  WS-TOL-REF                  PIC S999V99    VALUE ZEROS.     CL**6
00122      12  WS-TOL-CLM                  PIC S999V99    VALUE ZEROS.     CL**6
00123      12  WS-LF-EXP-PERCENT           PIC S9(3)V9(4) VALUE ZEROS.     CL**6
00124      12  WS-AH-EXP-PERCENT           PIC S9(3)V9(4) VALUE ZEROS.     CL**6
00125      12  SV-MAX-MON-BEN              PIC S9(7)      VALUE ZEROS.     CL**6
00126      12  SV-MAX-TOT-BEN              PIC S9(7)      VALUE ZEROS.     CL**6
00127      12  WS-MAX-MON-BEN              PIC S9(7)      VALUE ZEROS.     CL**6
00128      12  WS-MAX-MON-BEN-Z  REDEFINES WS-MAX-MON-BEN                  CL**6
00129                                      PIC ZZZ,ZZ9.                    CL**6
00130      12  WS-MAX-TOT-BEN              PIC S9(7)      VALUE ZEROS.     CL**6
00131      12  WS-MAX-TOT-BEN-Z  REDEFINES WS-MAX-TOT-BEN                  CL**6
00132                                      PIC ZZZ,ZZ9.                    CL**6
00133      EJECT                                                        EL6502
00134      12  ERROR-MESSAGES.                                          EL6502
00135          16  ER-0000                 PIC X(4)    VALUE '0000'.    EL6502
00136          16  ER-0002                 PIC X(4)    VALUE '0002'.    EL6502
00137          16  ER-0004                 PIC X(4)    VALUE '0004'.    EL6502
00138          16  ER-0008                 PIC X(4)    VALUE '0008'.    EL6502
00139          16  ER-0029                 PIC X(4)    VALUE '0029'.    EL6502
00140          16  ER-0068                 PIC X(4)    VALUE '0068'.    EL6502
00141          16  ER-0070                 PIC X(4)    VALUE '0070'.    EL6502
00142          16  ER-0627                 PIC X(4)    VALUE '0627'.       CL**6
00143          16  ER-0906                 PIC X(4)    VALUE '0906'.       CL*10
00144          16  ER-2039                 PIC X(4)    VALUE '2039'.    EL6502
00145          16  ER-2114                 PIC X(4)    VALUE '2114'.       CL*18
00146          16  ER-2154                 PIC X(4)    VALUE '2154'.       CL**6
00147          16  ER-2165                 PIC X(4)    VALUE '2165'.       CL**6
00148          16  ER-2168                 PIC X(4)    VALUE '2168'.       CL**6
00149          16  ER-2169                 PIC X(4)    VALUE '2169'.       CL**6
00150          16  ER-2170                 PIC X(4)    VALUE '2170'.       CL**6
00151          16  ER-2572                 PIC X(4)    VALUE '2572'.    EL6502
00152          16  ER-3124                 PIC X(4)    VALUE '3124'.       CL**6
00153          16  ER-3125                 PIC X(4)    VALUE '3125'.       CL**6
00154          16  ER-3126                 PIC X(4)    VALUE '3126'.       CL**6
00155          16  ER-3127                 PIC X(4)    VALUE '3127'.       CL**6
00156          16  ER-3128                 PIC X(4)    VALUE '3128'.       CL**6
00157          16  ER-3257                 PIC X(4)    VALUE '3257'.       CL*14
00158          16  ER-3258                 PIC X(4)    VALUE '3258'.       CL*14
00159          16  ER-3267                 PIC X(4)    VALUE '3267'.       CL*17
00160          16  ER-4011                 PIC X(4)    VALUE '4011'.       CL*10
00161          16  ER-7320                 PIC X(4)    VALUE '7320'.       CL**6
00162          16  ER-7531                 PIC X(4)    VALUE '7531'.       CL**6
00163          16  ER-7717                 PIC X(4)    VALUE '7717'.       CL**6
00164          16  ER-7718                 PIC X(4)    VALUE '7718'.       CL**6
00165          16  ER-7719                 PIC X(4)    VALUE '7719'.       CL**6
00166                                                                   EL6502
00167      12  ELCNTL-KEY.                                              EL6502
00168          16  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.    EL6502
00169          16  CNTL-REC-TYPE           PIC X       VALUE SPACES.    EL6502
00170          16  CNTL-ACCESS             PIC X(4)    VALUE SPACES.    EL6502
00171          16  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.  EL6502
00172                                                                   EL6502
00173      12  ERCOMP-KEY.                                                 CL*11
00174          16  COMP-CO-ID              PIC X(1)    VALUE SPACES.       CL*11
00175          16  COMP-CARRIER            PIC X       VALUE SPACES.       CL*11
00176          16  COMP-GROUPING           PIC X(6)    VALUE SPACES.       CL*11
00177          16  COMP-FIN-RESP           PIC X(10)   VALUE SPACES.       CL*11
00178          16  COMP-ACCT-AGT           PIC X(10)   VALUE SPACES.       CL*11
00179          16  COMP-REC-TYPE           PIC X       VALUE SPACES.       CL*11
00180                                                                      CL*11
00181      12  WS-SAVE-REPORT-CODE1        PIC X(10)   VALUE SPACES.       CL**6
00182      12  WS-SAVE-REPORT-CODE2        PIC X(10)   VALUE SPACES.       CL**6
120706     12  WS-SAVE-REPORT-CODE3        PIC X(10)   VALUE SPACES.       CL**6
00183      12  WS-REPORT-CODE-CAPTION.                                     CL**6
00184          16  WS-REPORT-CD-CAPTION    PIC X(14)   VALUE SPACES.       CL**6
00185          16  FILLER                  PIC X       VALUE ':'.          CL**6
00186                                                                      CL**6
00187      12  WS-FLI-PFK-DESC             PIC X(25)   VALUE               CL*13
00188          '    PF10=CLIENT ADDL DATA'.                                CL*13
00189      12  WS-CANCEL-TEXT              PIC X(17)   VALUE               CL*17
00190          '  CANCEL FEE    :'.                                        CL*17
PEMMOD     12  WS-YYYYMMDD             PIC X(8).                           CL*54
PEMMOD     12  WS-COMP-DATE REDEFINES WS-YYYYMMDD                          CL*54
PEMMOD                                 PIC 9(8).                           CL*54
00191      EJECT                                                        EL6502
00192                            COPY ELCSCTM.                             CL**6
00193      EJECT                                                        EL6502
00194                            COPY ELCSCRTY.                            CL**6
00195      EJECT                                                        EL6502
00196                            COPY ELCLOGOF.                            CL**6
00197      EJECT                                                        EL6502
00198                            COPY ELCDATE.                             CL**6
00199      EJECT                                                        EL6502
00200                            COPY ELCATTR.                             CL**6
00201      EJECT                                                        EL6502
00202                            COPY ELCEMIB.                             CL**6
00203      EJECT                                                           CL**6
00204                            COPY ELCINTF.                             CL**6
00205                            COPY ELC650PI.                            CL**6
00206                                                                   EL6502
00207      EJECT                                                        EL6502
00208                            COPY ELCAID.                              CL**9
00209  01  FILLER    REDEFINES DFHAID.                                  EL6502
00210      12  FILLER                      PIC X(8).                    EL6502
00211      12  PF-VALUES                   PIC X       OCCURS 2.        EL6502
00212                                                                   EL6502
00213      EJECT                                                        EL6502
00214                            COPY EL6502S.                             CL**9
00215  01  FILLER REDEFINES EL6502AI.                                      CL*14
101101     12  FILLER                      PIC X(306).                     CL*14
00217      12  W-MAP-REST                  PIC X(199).                     CL*14
00218                                                                   EL6502
00219      EJECT                                                        EL6502
00220  LINKAGE SECTION.                                                 EL6502
00221  01  DFHCOMMAREA                     PIC X(1500).                    CL**4
00222                                                                   EL6502
00223      EJECT                                                        EL6502
00224 *01 PARMLIST .                                                       CL*17
00225 *    02  FILLER                      PIC S9(8)   COMP.               CL*17
00226 *    02  ERACCT-POINTER              PIC S9(8)   COMP.               CL*17
00227 *    02  ERRATE-POINTER              PIC S9(8)   COMP.               CL*17
00228 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.               CL*17
00229 *    02  ERCOMP-POINTER              PIC S9(8)   COMP.               CL*17
00230                                                                   EL6502
00231                            COPY ERCACCT.                             CL**9
00232      EJECT                                                        EL6502
00233                            COPY ERCRATE.                             CL**9
00234      EJECT                                                           CL**6
00235                            COPY ELCCNTL.                             CL**9
00236      EJECT                                                           CL*11
00237                            COPY ERCCOMP.                             CL*11
00238      EJECT                                                        EL6502
00239                                                                   EL6502
00240  PROCEDURE DIVISION.                                              EL6502
00241                                                                   EL6502
00242      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6502
00243      MOVE '5'                    TO  DC-OPTION-CODE.              EL6502
00244      PERFORM 9700-LINK-DATE-CONVERT                               EL6502
                                       THRU 9700-EXIT                   EL6502
00245      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL6502
00246      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL6502
00247      MOVE DC-GREG-DATE-1-YMD     TO  YMD-CURRENT-SAVE.            EL6502
00248                                                                   EL6502
00249      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6502
00250      MOVE +2                     TO  EMI-NUMBER-OF-LINES.            CL**6
00251                                                                      CL**6
00252      IF EIBCALEN = 0                                              EL6502
00253          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6502
00254                                                                   EL6502
00255      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6502
00256          IF PI-CALLING-PROGRAM = XCTL-6501                           CL*16
00257              MOVE PI-CALLING-PROGRAM                                 CL*16
00258                                  TO  PI-RETURN-TO-PROGRAM            CL*16
00259              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM              CL*16
00260          ELSE                                                        CL*16
00261              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM.             CL*16
00262                                                                   EL6502
00263      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.                   CL*15
00264                                                                      CL*15
00265      MOVE LOW-VALUES             TO  EL6502AI.                    EL6502
00266                                                                   EL6502
00267      IF EIBTRNID NOT = TRANS-ID                                   EL6502
00268          MOVE PI-MAINT           TO  MAINTYPO                     EL6502
00269          MOVE AL-UANON           TO  MAINTYPA                     EL6502
00270          MOVE -1                 TO  MAINTYPL                     EL6502
00271          IF PI-MAINT = 'S' OR 'C'                                 EL6502
00272              GO TO 4000-SHOW                                      EL6502
00273          ELSE                                                     EL6502
00274              IF PI-MAINT = 'A'                                    EL6502
00275                  MOVE 'C'            TO  PI-MAINT                    CL**6
00276                  GO TO 4000-SHOW                                  EL6502
00277              ELSE                                                 EL6502
00278                  GO TO 8100-SEND-INITIAL-MAP.                     EL6502
00279                                                                   EL6502
00280      EXEC CICS HANDLE CONDITION                                   EL6502
00281          PGMIDERR  (9600-PGMID-ERROR)                             EL6502
00282          ERROR     (9990-ABEND)                                   EL6502
00283      END-EXEC.                                                    EL6502
00284                                                                   EL6502
00285      IF EIBAID = DFHCLEAR                                         EL6502
00286          GO TO 9400-CLEAR.                                        EL6502
00287                                                                   EL6502
00288      EJECT                                                        EL6502
00289  0200-RECEIVE.                                                    EL6502
00290      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6502
00291          MOVE ER-0008            TO  EMI-ERROR                    EL6502
00292          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6502
00293          MOVE -1                 TO  PFENTERL                     EL6502
00294          GO TO 8200-SEND-DATAONLY.                                EL6502
00295                                                                   EL6502
00296      EXEC CICS RECEIVE                                            EL6502
00297          MAP      (MAP-NAME)                                      EL6502
00298          MAPSET   (MAPSET-NAME)                                   EL6502
00299          INTO     (EL6502AI)                                      EL6502
00300      END-EXEC.                                                    EL6502
00301                                                                   EL6502
00302      IF PFENTERL = 0                                              EL6502
00303          GO TO 0300-CHECK-PFKEYS.                                 EL6502
00304      IF EIBAID NOT = DFHENTER                                     EL6502
00305          MOVE ER-0004            TO  EMI-ERROR                    EL6502
00306          GO TO 0320-INPUT-ERROR.                                  EL6502
00307      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)      CL**4
00308          MOVE PF-VALUES (PFENTERI) TO  EIBAID                        CL**6
00309      ELSE                                                         EL6502
00310          MOVE ER-0029            TO  EMI-ERROR                       CL**6
00311          GO TO 0320-INPUT-ERROR.                                  EL6502
00312                                                                      CL**6
00313      EJECT                                                        EL6502
00314  0300-CHECK-PFKEYS.                                               EL6502
00315      IF EIBAID = DFHPF23                                          EL6502
00316          GO TO 8810-PF23.                                         EL6502
00317      IF EIBAID = DFHPF24                                          EL6502
00318          GO TO 9200-RETURN-MAIN-MENU.                             EL6502
00319      IF EIBAID = DFHPF12                                          EL6502
00320          GO TO 9500-PF12.                                         EL6502
00321      IF EIBAID = DFHPF5                                           EL6502
00322          MOVE XCTL-6501          TO  PGM-NAME                     EL6502
00323          GO TO 9300-XCTL.                                         EL6502
00324      IF EIBAID = DFHPF7                                           EL6502
00325          MOVE XCTL-6504          TO  PGM-NAME                     EL6502
00326          GO TO 9300-XCTL.                                         EL6502
00327      IF EIBAID = DFHPF8                                           EL6502
00328          MOVE XCTL-6506          TO  PGM-NAME                     EL6502
00329          GO TO 9300-XCTL.                                            CL**2
00330      IF EIBAID = DFHPF9                                              CL**2
00331          MOVE XCTL-6505          TO  PGM-NAME                        CL**2
00332          GO TO 9300-XCTL.                                            CL*13
00333      IF (EIBAID = DFHPF10)  AND                                      CL*13
00334         (PI-COMPANY-ID = 'FLI' OR 'FLU' OR 'LGX')                    CL*13
00335          MOVE XCTL-6508          TO  PGM-NAME                        CL*13
00336          GO TO 9300-XCTL.                                         EL6502
00337                                                                      CL**4
00338      IF EIBAID = DFHENTER                                         EL6502
00339          GO TO 0330-CHECK-MAINTYP.                                EL6502
00340                                                                      CL**4
00341      MOVE ER-0029                TO  EMI-ERROR.                   EL6502
00342  0320-INPUT-ERROR.                                                EL6502
00343      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6502
00344      MOVE AL-UNBON               TO  PFENTERA.                    EL6502
00345      MOVE -1                     TO  PFENTERL.                    EL6502
00346      GO TO 8200-SEND-DATAONLY.                                    EL6502
00347                                                                   EL6502
00348  0330-CHECK-MAINTYP.                                              EL6502
00349      IF MAINTYPL GREATER ZERO                                        CL**6
00350          IF MAINTYPI = 'S' OR 'C' OR 'A'                             CL**6
00351              MOVE AL-UANON       TO  MAINTYPA                     EL6502
00352              MOVE MAINTYPI       TO  PI-MAINT                     EL6502
00353          ELSE                                                     EL6502
00354              MOVE -1             TO  MAINTYPL                     EL6502
00355              MOVE AL-UABON       TO  MAINTYPA                     EL6502
00356              MOVE ER-2039        TO  EMI-ERROR                    EL6502
00357              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6502
00358              GO TO 8200-SEND-DATAONLY                             EL6502
00359      ELSE                                                         EL6502
00360          MOVE -1                 TO  MAINTYPL                     EL6502
00361          MOVE AL-UABON           TO  MAINTYPA                     EL6502
00362          MOVE ER-2039            TO  EMI-ERROR                    EL6502
00363          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6502
00364          GO TO 8200-SEND-DATAONLY.                                EL6502
00365                                                                   EL6502
00366      IF PI-MAINT = 'S'                                            EL6502
00367          GO TO 4000-SHOW.                                         EL6502
00368                                                                   EL6502
CIDMOD     PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.                     000
CIDMOD                                                                       000
00369      IF EMI-ERROR NOT = ZEROS                                     EL6502
00370          MOVE -1                 TO  MAINTYPL                     EL6502
00371          GO TO 8200-SEND-DATAONLY.                                EL6502
00372                                                                   EL6502
00373      GO TO 4200-MAINT.                                            EL6502
00374                                                                   EL6502
00375      EJECT                                                        EL6502
00376                                                                   EL6502
00377  4000-SHOW.                                                       EL6502
CIDMOD     PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.                     000
CIDMOD                                                                       000
00379      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6502
00380      MOVE LOW-VALUES             TO  EL6502AO.                    EL6502
00381      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL6502
00382                                                                   EL6502
00383      EJECT                                                        EL6502
00384  4200-MAINT.                                                      EL6502
00385      IF NOT MODIFY-CAP                                            EL6502
00386          MOVE 'UPDATE'       TO SM-READ                              CL**6
00387          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT              CL**6
00388          MOVE ER-0070             TO  EMI-ERROR                      CL**6
00389          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**6
00390          GO TO 8100-SEND-INITIAL-MAP.                                CL**6
00391                                                                   EL6502
00392      PERFORM 7000-EDIT THRU 7099-EXIT.                            EL6502
00393                                                                   EL6502
00394      IF EMI-NO-ERRORS                                             EL6502
00395          NEXT SENTENCE                                            EL6502
00396      ELSE                                                         EL6502
00397          IF EMI-FORCABLE OR EMI-FATAL                                CL**6
00398             GO TO 8200-SEND-DATAONLY.                                CL**6
00399                                                                   EL6502
00400      PERFORM 7300-READ-ERACCT-UPDATE THRU 7300-EXIT.              EL6502
00401                                                                   EL6502
00402      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT.              EL6502
00403                                                                   EL6502
00404      IF AM-LAST-MAINT-USER   = PI-UPDATE-BY OR                       CL**4
00405         AM-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6502
00406          NEXT SENTENCE                                            EL6502
00407      ELSE                                                         EL6502
00408          EXEC CICS UNLOCK                                         EL6502
00409               DATASET  (ERACCT-FILE)                                 CL**6
00410          END-EXEC                                                 EL6502
00411          MOVE ER-0068            TO  EMI-ERROR                    EL6502
00412          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6502
00413          PERFORM 7100-READ-ERACCT  THRU 7100-EXIT                    CL**6
00414          MOVE LOW-VALUES         TO  EL6502AO                     EL6502
00415          MOVE -1                 TO  MAINTYPL                     EL6502
00416          MOVE 'S'                TO  PI-MAINT                     EL6502
00417          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL6502
00418                                                                   EL6502
00419      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.          EL6502
00420      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.        EL6502
00421      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6502
00422      MOVE '5'                    TO  DC-OPTION-CODE.              EL6502
00423      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL6502
00424                                                                   EL6502
00425      EXEC CICS LINK                                               EL6502
00426          PROGRAM (PGM-NAME)                                          CL**4
00427          COMMAREA(DATE-CONVERSION-DATA)                           EL6502
00428          LENGTH  (DC-COMM-LENGTH)                                    CL**4
00429      END-EXEC.                                                    EL6502
00430                                                                   EL6502
00431      MOVE DC-BIN-DATE-1          TO  AM-LAST-MAINT-DT             EL6502
00432                                      BIN-CURRENT-SAVE.            EL6502
00433                                                                   EL6502
00434      EXEC CICS REWRITE                                            EL6502
00435          DATASET  (ERACCT-FILE)                                      CL**6
00436          FROM     (ACCOUNT-MASTER)                                EL6502
00437      END-EXEC.                                                    EL6502
00438                                                                   EL6502
00439      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6502
00440      MOVE ER-0000                TO  EMI-ERROR.                   EL6502
00441      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6502
00442                                                                   EL6502
00443      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6502
00444      MOVE LOW-VALUES             TO  EL6502AO.                    EL6502
00445      MOVE 'C'                    TO  PI-MAINT.                       CL**6
00446                                                                   EL6502
00447      EJECT                                                        EL6502
00448                                                                      CL**6
00449  5000-BUILD-INITIAL-SCREEN.                                       EL6502
092711     MOVE AM-ORIG-DEALER-NO      TO ODEALERO.
00450      MOVE AM-REPORT-CODE-1       TO RPTCD1O.                         CL**6
00451      MOVE AM-CITY-CODE           TO CTYCDO.                          CL**6
00452      MOVE AM-AUTO-REFUND-SW      TO AUTORFDO.                        CL**6
00453                                                                      CL**6
00454      MOVE AM-REPORT-CODE-2       TO RPTCD2O.                         CL**6
120706     MOVE AM-REPORT-CODE-3       TO RPTCD3O
00455      MOVE AM-COUNTY-PARISH       TO CNTYCDO.                         CL**6
00456                                                                      CL**6
00457      MOVE AM-USER-FIELDS         TO USERO.                           CL**6
00458      MOVE AM-TRUST-TYPE          TO TRUSTTYO.                        CL**9
00459                                                                      CL*10
00460      IF AM-AH-ONLY-INDICATOR = 'N'                                   CL**9
00461         MOVE 'N'                 TO AHONLYO                          CL**9
00462      ELSE                                                            CL**9
00463         MOVE 'Y'                 TO AHONLYO.                         CL**9
00464                                                                      CL**9
00465      IF AM-EDIT-LOAN-OFC = 'Y'                                       CL*10
00466          MOVE 'Y'                TO LOANOFCO                         CL**9
00467      ELSE                                                            CL**9
00468          MOVE 'N'                TO LOANOFCO.                        CL**9
00469                                                                      CL*10
00470      IF AM-DISMBR-COVERAGE-SW = 'Y'                                  CL*10
00471          MOVE 'Y'                TO DISMBRO                          CL*10
00472      ELSE                                                            CL*10
00473          MOVE 'N'                TO DISMBRO.                         CL*10
00474                                                                      CL**6
00475      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC                         CL**6
00476         MOVE ZEROS TO AM-3RD-PARTY-NOTIF-LEVEL.                      CL**6
00477                                                                      CL**6
00478      MOVE AM-3RD-PARTY-NOTIF-LEVEL                                   CL**6
00479                                  TO NLEVELO.                         CL**6
00480                                                                      CL*10
00481      IF AM-NOTIF-OF-LETTERS = 'Y'                                    CL**6
00482         MOVE 'Y'                 TO NCORRO                           CL**6
00483      ELSE                                                            CL**6
00484         MOVE 'N'                 TO NCORRO.                          CL**6
00485                                                                      CL**6
00486      IF AM-NOTIF-OF-PAYMENTS = 'Y'                                   CL**6
00487         MOVE 'Y'                 TO NPMTSO                           CL**6
00488      ELSE                                                            CL**6
00489         MOVE 'N'                 TO NPMTSO.                          CL**6
00490                                                                      CL**6
00491      IF AM-NOTIF-OF-REPORTS = 'Y'                                    CL**6
00492         MOVE 'Y'                 TO NRPTSO                           CL**6
00493      ELSE                                                            CL**6
00494         MOVE 'N'                 TO NRPTSO.                          CL**6
00495                                                                      CL**6
00496      IF AM-NOTIF-OF-STATUS = 'Y'                                     CL**6
00497         MOVE 'Y'                 TO NSTATO                           CL**6
00498      ELSE                                                            CL**6
00499         MOVE 'N'                 TO NSTATO.                          CL**6
00500                                                                      CL**6
00501      IF AM-GROUPED-CHECKS-Y-N = 'Y'                                  CL**6
00502         MOVE 'Y'                 TO GRPCHKO                          CL**6
00503      ELSE                                                            CL**6
00504         MOVE 'N'                 TO GRPCHKO.                         CL**6
00505                                                                      CL**6
00506      IF AM-EMPLOYER-STMT-USED = 'Y' OR '1' OR '2' OR '3'             CL**6
00507         MOVE AM-EMPLOYER-STMT-USED  TO EMPSTMTO                      CL**6
00508      ELSE                                                            CL**6
00509         MOVE 'N'                 TO EMPSTMTO.                        CL**6
00510                                                                      CL**6
00511      IF  AM-TARGET-LOSS-RATIO NUMERIC                                CL**6
00512          MOVE AM-TARGET-LOSS-RATIO                                   CL**6
00513                                  TO TARRATO                          CL**6
00514      ELSE                                                            CL**6
00515          MOVE ZEROS              TO TARRATO.                         CL**6
00516                                                                      CL**6
00517      IF  AM-LIFE-IBNR-PCT NUMERIC                                    CL**6
00518          MOVE AM-LIFE-IBNR-PCT                                       CL**6
00519                                  TO LIFBNRO                          CL**6
00520      ELSE                                                            CL**6
00521          MOVE ZEROS              TO LIFBNRO.                         CL**6
00522                                                                   EL6502
00523      IF  AM-CRDT-MODIFICATION-PCT NUMERIC                            CL**6
00524          MOVE AM-CRDT-MODIFICATION-PCT                               CL**6
00525                                  TO CDTMODO                          CL**6
00526      ELSE                                                            CL**6
00527          MOVE ZEROS              TO CDTMODO.                         CL**6
00528                                                                      CL*14
070109*  THE FOLLOWING IS FOR JHL ONLY AND IS NOT USED IN THE BATCH SYSTEM
00529      IF  AM-EXEC1-DIS-PERCENT NUMERIC                                CL*14
00530          MOVE AM-EXEC1-DIS-PERCENT                                   CL*14
00531                                  TO AXDISP1O.                        CL*14
00532                                                                      CL*14
00533      IF  AM-EXEC1-LIFE-PERCENT NUMERIC                               CL*14
00534          MOVE AM-EXEC1-LIFE-PERCENT                                  CL*14
00535                                  TO AXLIFP1O.                        CL*14
00536                                                                      CL*14
00537      IF  AM-EXEC2-DIS-PERCENT NUMERIC                                CL*14
00538          MOVE AM-EXEC2-DIS-PERCENT                                   CL*14
00539                                  TO AXDISP2O.                        CL*14
00540                                                                      CL*14
00541      IF  AM-EXEC2-LIFE-PERCENT NUMERIC                               CL*14
00542          MOVE AM-EXEC2-LIFE-PERCENT                                  CL*14
00543                                  TO AXLIFP2O.                        CL*14
00544                                                                      CL*14
00545      MOVE AM-EXEC1-NAME          TO AXNAME1O.                        CL*14
00546      MOVE AM-EXEC2-NAME          TO AXNAME2O.                        CL*14
00547      MOVE AM-CONTROL-NAME        TO CNTLTITO.                        CL*14
00548                                                                   EL6502
00549      MOVE AM-USER-SELECT-1       TO USER1O.                          CL**6
00550      MOVE AM-USER-SELECT-2       TO USER2O.                          CL**6
00551      MOVE AM-USER-SELECT-3       TO USER3O.                          CL**6
00552      MOVE AM-USER-SELECT-4       TO USER4O.                          CL**6
00553      MOVE AM-USER-SELECT-5       TO USER5O.                          CL**6
00554                                                                   EL6502
00555      MOVE PI-MAINT               TO  MAINTYPO.                       CL**6
00556      MOVE -1                     TO  MAINTYPL.                       CL**6
00557      MOVE AL-UANON               TO  MAINTYPA.                       CL**6
00558                                                                   EL6502
CIDMOD     IF WS-SAVE-REPORT-CODE1 NOT = SPACES AND LOW-VALUES               000
CIDMOD         MOVE WS-SAVE-REPORT-CODE1  TO WS-REPORT-CD-CAPTION            000
CIDMOD         MOVE WS-REPORT-CODE-CAPTION TO RCAP1O.                        000
CIDMOD                                                                       000
CIDMOD     IF WS-SAVE-REPORT-CODE2 NOT = SPACES AND LOW-VALUES               000
CIDMOD         MOVE WS-SAVE-REPORT-CODE2  TO WS-REPORT-CD-CAPTION            000
CIDMOD         MOVE WS-REPORT-CODE-CAPTION TO RCAP2O.                        000
CIDMOD                                                                       000
120706     IF WS-SAVE-REPORT-CODE3 NOT = SPACES AND LOW-VALUES
120706         MOVE WS-SAVE-REPORT-CODE3
120706                                 TO WS-REPORT-CD-CAPTION
120706         MOVE WS-REPORT-CODE-CAPTION
120706                                 TO RCAP3O
120706     END-IF

CIDMOD                                                                       000
00559      GO TO 8100-SEND-INITIAL-MAP.                                    CL**6
00560                                                                   EL6502
00561  5099-EXIT.                                                       EL6502
00562      EXIT.                                                        EL6502
00563      EJECT                                                        EL6502
00564  6000-CHECK-FOR-UPDATE.                                           EL6502
00565                                                                      CL**6
020816     IF PI-COMPANY-ID = 'ACE' OR 'LGX' or 'DCC' or 'VPP'
00567         IF CANFEEL NOT EQUAL ZEROS                                   CL*17
00568            MOVE W-CANCEL-FEE     TO AM-CANCEL-FEE.                   CL*17
092711
092711     IF ODEALERL GREATER ZEROS
092711        MOVE ODEALERI            TO AM-ORIG-DEALER-NO
092711     END-IF.
00569                                                                      CL*17
00570      IF RPTCD1L GREATER ZEROS                                        CL**6
00571         MOVE RPTCD1I             TO AM-REPORT-CODE-1.                CL**6
00572                                                                      CL**4
00573      IF CTYCDL GREATER ZEROS                                         CL**6
00574         MOVE CTYCDI              TO AM-CITY-CODE.                    CL**6
00575                                                                   EL6502
00576      IF AUTORFDL GREATER ZEROS                                       CL**6
00577          MOVE AUTORFDI           TO AM-AUTO-REFUND-SW.               CL**6
00578                                                                   EL6502
00579      IF RPTCD2L GREATER ZEROS                                        CL**6
00580         MOVE RPTCD2I             TO AM-REPORT-CODE-2                 CL*10
00581         IF PI-COMPANY-ID = 'NCL'                                     CL*11
00582             PERFORM 7400-UPDATE-COMP-MAST THRU 7400-EXIT             CL*18
00583         ELSE                                                         CL*11
00584             NEXT SENTENCE                                            CL*11
00585      ELSE                                                            CL*11
00586         IF PI-COMPANY-ID = 'NCL' AND                                 CL*10
00587            PI-SV-MAINT   = 'A'                                       CL*10
00588                 MOVE ER-4011         TO EMI-ERROR                    CL*10
00589                 MOVE -1              TO RPTCD2L                      CL*10
00590                 MOVE AL-UABON        TO RPTCD2A                      CL*10
00591                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            CL*10
00592                                                                   EL6502
120706     IF RPTCD3L > ZEROS
120706        MOVE RPTCD3I             TO AM-REPORT-CODE-3
120706     END-IF

00593      IF CNTYCDL GREATER ZEROS                                        CL**6
00594         MOVE CNTYCDI             TO AM-COUNTY-PARISH.                CL**6
00595                                                                   EL6502
00596      IF USERL GREATER ZEROS                                          CL**6
00597         MOVE USERI               TO AM-USER-FIELDS.                  CL**6
00598                                                                   EL6502
00599      IF TRUSTTYL GREATER ZEROS                                       CL**9
00600         MOVE TRUSTTYI            TO AM-TRUST-TYPE.                   CL**9
00601                                                                      CL**9
00602      IF AHONLYL GREATER ZEROS                                        CL**9
00603         MOVE AHONLYI             TO AM-AH-ONLY-INDICATOR.            CL**9
00604                                                                      CL*12
00605      IF PI-COMPANY-ID = 'NCL'                                        CL*12
00606         IF AM-AH-ONLY-INDICATOR NOT = 'Y'                            CL*12
00607            MOVE 'N'              TO AM-AH-ONLY-INDICATOR.            CL*12
00608                                                                      CL**9
00609      IF LOANOFCL GREATER ZEROS                                       CL**9
00610         MOVE LOANOFCI            TO AM-EDIT-LOAN-OFC.                CL**9
00611                                                                      CL**9
00612      IF DISMBRL  GREATER ZEROS                                       CL*10
00613         MOVE DISMBRI             TO AM-DISMBR-COVERAGE-SW.           CL*10
00614
00615      IF NLEVELL GREATER ZEROS                                        CL**6
00616         IF NLEVELI NUMERIC                                           CL**6
00617            MOVE NLEVELI          TO AM-3RD-PARTY-NOTIF-LEVEL.        CL**6
00618                                                                   EL6502
00619      IF NCORRL GREATER ZEROS                                         CL**6
00620         IF NCORRI = ' ' OR 'Y' OR 'N'                                CL**6
00621            MOVE NCORRI           TO AM-NOTIF-OF-LETTERS.             CL**6
00622                                                                   EL6502
00623      IF NPMTSL GREATER ZEROS                                         CL**6
00624         IF NPMTSI = ' ' OR 'Y' OR 'N'                                CL**6
00625            MOVE NPMTSI           TO AM-NOTIF-OF-PAYMENTS.            CL**6
00626                                                                      CL**6
00627      IF NRPTSL GREATER ZEROS                                         CL**6
00628         IF NRPTSI = ' ' OR 'Y' OR 'N'                                CL**6
00629            MOVE NRPTSI           TO AM-NOTIF-OF-REPORTS.             CL**6
00630                                                                      CL**6
00631      IF NSTATL GREATER ZEROS                                         CL**6
00632         IF NSTATI = ' ' OR 'Y' OR 'N'                                CL**6
00633            MOVE NSTATI           TO AM-NOTIF-OF-STATUS.              CL**6
00634                                                                      CL**6
00635      IF GRPCHKL GREATER ZEROS                                        CL**6
00636         MOVE GRPCHKI             TO AM-GROUPED-CHECKS-Y-N.           CL**6
00637                                                                      CL**6
00638      IF EMPSTMTL GREATER ZEROS                                       CL**6
00639         MOVE EMPSTMTI            TO AM-EMPLOYER-STMT-USED.           CL**6
00640                                                                      CL**6
00641      IF TARRATL GREATER ZEROS                                        CL**6
00642         MOVE W-TARGET-LOSS-RATIO TO AM-TARGET-LOSS-RATIO.            CL**6
00643                                                                      CL**6
00644      IF LIFBNRL GREATER ZEROS                                        CL**6
00645         MOVE W-LIFE-IBNR-PCT     TO AM-LIFE-IBNR-PCT.                CL**6
00646                                                                      CL**6
00647      IF CDTMODL GREATER ZEROS                                        CL**6
00648         MOVE W-CRDT-MOD-PCT      TO AM-CRDT-MODIFICATION-PCT.        CL**6
00649                                                                      CL*14
00650      IF CNTLTITL GREATER ZEROS                                       CL*14
00651         MOVE CNTLTITI            TO AM-CONTROL-NAME.                 CL*14
00652                                                                      CL*14
00653      IF AXNAME1L NOT EQUAL ZEROS                                     CL*14
00654         MOVE AXNAME1I            TO AM-EXEC1-NAME.                   CL*14
00655                                                                      CL*14
00656      IF AXDISP1L NOT EQUAL ZEROS                                     CL*14
00657         MOVE W-EXEC1-DIS-PRCNT   TO AM-EXEC1-DIS-PERCENT             CL*14
00658                                                                      CL*14
00659      ELSE                                                            CL*14
00660         IF  AM-EXEC1-DIS-PERCENT NOT NUMERIC                         CL*14
00661             MOVE ZEROS           TO AM-EXEC1-DIS-PERCENT.            CL*14
00662                                                                      CL*14
00663      IF AXLIFP1L NOT EQUAL ZEROS                                     CL*14
00664         MOVE W-EXEC1-LIF-PRCNT   TO AM-EXEC1-LIFE-PERCENT            CL*14
00665                                                                      CL*14
00666      ELSE                                                            CL*14
00667         IF  AM-EXEC1-LIFE-PERCENT NOT NUMERIC                        CL*14
00668             MOVE ZEROS           TO AM-EXEC1-LIFE-PERCENT.           CL*14
00669                                                                      CL*14
00670      IF AXNAME2L NOT EQUAL ZEROS                                     CL*14
00671         MOVE AXNAME2I            TO AM-EXEC2-NAME.                   CL*14
00672                                                                      CL*14
00673      IF AXDISP2L NOT EQUAL ZEROS                                     CL*14
00674         MOVE W-EXEC2-DIS-PRCNT   TO AM-EXEC2-DIS-PERCENT             CL*14
00675                                                                      CL*14
00676      ELSE                                                            CL*14
00677         IF  AM-EXEC2-DIS-PERCENT NOT NUMERIC                         CL*14
00678             MOVE ZEROS           TO AM-EXEC2-DIS-PERCENT.            CL*14
00679                                                                      CL*14
00680      IF AXLIFP2L NOT EQUAL ZEROS                                     CL*14
00681         MOVE W-EXEC2-LIF-PRCNT   TO AM-EXEC2-LIFE-PERCENT            CL*14
00682                                                                      CL*14
00683      ELSE                                                            CL*14
00684         IF  AM-EXEC2-LIFE-PERCENT NOT NUMERIC                        CL*14
00685             MOVE ZEROS           TO AM-EXEC2-LIFE-PERCENT.           CL*14
00686                                                                      CL**6
00687      IF USER1L GREATER ZEROS                                         CL**6
00688         MOVE USER1I              TO AM-USER-SELECT-1.                CL**6
00689                                                                      CL**6
00690      IF USER2L GREATER ZEROS                                         CL**6
00691         MOVE USER2I              TO AM-USER-SELECT-2.                CL**6
00692                                                                      CL**6
00693      IF USER3L GREATER ZEROS                                         CL**6
00694         MOVE USER3I              TO AM-USER-SELECT-3.                CL**6
00695                                                                      CL**6
00696      IF USER4L GREATER ZEROS                                         CL**6
00697         MOVE USER4I              TO AM-USER-SELECT-4.                CL**6
00698                                                                      CL**6
00699      IF USER5L GREATER ZEROS                                         CL**6
00700         MOVE USER5I              TO AM-USER-SELECT-5.                CL**6
00701                                                                   EL6502
00702  6049-EXIT.                                                       EL6502
00703      EXIT.                                                        EL6502
00704      EJECT                                                        EL6502
00705                                                                   EL6502
00706  7000-EDIT.                                                       EL6502
00707                                                                   EL6502
00708      IF GRPCHKL GREATER ZEROS                                        CL**6
00709         IF GRPCHKI = ' ' OR 'Y' OR 'N'                               CL**6
00710            NEXT SENTENCE                                             CL**6
00711          ELSE                                                     EL6502
00712             MOVE -1                      TO GRPCHKL                  CL**6
00713             MOVE AL-UABON                TO GRPCHKA                  CL**6
00714             MOVE ER-0627                 TO EMI-ERROR                CL**9
00715             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                CL**9
00716                                                                      CL**9
00717      IF AHONLYL GREATER ZEROS                                        CL**9
00718         IF AHONLYI = ' ' OR 'Y' OR 'N'                               CL**9
00719            NEXT SENTENCE                                             CL**9
00720          ELSE                                                        CL**9
00721             MOVE -1                      TO AHONLYL                  CL**9
00722             MOVE AL-UABON                TO AHONLYA                  CL**9
00723             MOVE ER-0627                 TO EMI-ERROR                CL**9
00724             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                CL**9
00725                                                                      CL**9
00726      IF LOANOFCL GREATER ZEROS                                       CL**9
00727         IF LOANOFCI = ' ' OR 'Y' OR 'N'                              CL*10
00728             NEXT SENTENCE                                            CL**9
00729         ELSE                                                         CL**9
00730             MOVE -1                      TO LOANOFCL                 CL**9
00731             MOVE AL-UABON                TO LOANOFCA                 CL**9
00732             MOVE ER-0627                 TO EMI-ERROR                CL**6
00733             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                CL*10
00734                                                                      CL*10
00735      IF DISMBRL GREATER ZEROS                                        CL*10
00736         IF DISMBRI = ' ' OR 'Y' OR 'N'                               CL*10
00737             NEXT SENTENCE                                            CL*10
00738         ELSE                                                         CL*10
00739             MOVE -1                      TO DISMBRL                  CL*10
00740             MOVE AL-UABON                TO DISMBRA                  CL*10
00741             MOVE ER-0906                 TO EMI-ERROR                CL*10
00742             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                CL**6
00743                                                                   EL6502
00744      IF EMPSTMTL GREATER ZEROS                                       CL**6
00745         IF EMPSTMTI = ' ' OR 'Y' OR 'N' OR '1' OR '2' OR '3'         CL**6
00746            NEXT SENTENCE                                             CL**6
00747          ELSE                                                     EL6502
00748             MOVE -1                      TO EMPSTMTL                 CL**6
00749             MOVE AL-UABON                TO EMPSTMTA                 CL**6
00750             MOVE ER-0627                 TO EMI-ERROR                CL**6
00751             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                CL**6
00752                                                                   EL6502
00753      IF AUTORFDL GREATER ZEROS                                       CL**6
00754          IF AUTORFDI = ' '  OR  'Y'  OR  'N'                         CL**6
00755              NEXT SENTENCE                                           CL**6
00756          ELSE                                                     EL6502
00757              MOVE -1                     TO AUTORFDL                 CL**6
00758              MOVE AL-UABON               TO AUTORFDA                 CL**6
00759              MOVE ER-3124                TO EMI-ERROR                CL**6
00760              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6502
00761                                                                   EL6502
00762      IF  TARRATL GREATER ZEROS                                       CL**6
00763          EXEC CICS BIF DEEDIT                                        CL**6
00764              FIELD   (TARRATI)                                       CL**6
00765              LENGTH  (6)                                             CL**6
00766              END-EXEC                                                CL**6
00767          IF  TARRATI NUMERIC                                         CL**6
00768              MOVE TARRATI        TO W-TARGET-LOSS-RATIO              CL**6
00769                                     TARRATO                          CL**6
00770          ELSE                                                        CL**6
00771              MOVE -1             TO TARRATL                          CL**6
00772              MOVE AL-UABON       TO TARRATA                          CL**6
00773              MOVE ER-7717        TO EMI-ERROR                        CL**6
00774              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**6
00775                                                                   EL6502
00776      IF  LIFBNRL GREATER ZEROS                                       CL**6
00777          EXEC CICS BIF DEEDIT                                        CL**6
00778              FIELD   (LIFBNRI)                                       CL**6
00779              LENGTH  (6)                                             CL**6
00780              END-EXEC                                                CL**6
00781          IF  LIFBNRI NUMERIC                                         CL**6
00782              MOVE LIFBNRI        TO W-LIFE-IBNR-PCT                  CL**6
00783                                     LIFBNRO                          CL**6
00784          ELSE                                                        CL**6
00785              MOVE -1             TO LIFBNRL                          CL**6
00786              MOVE AL-UABON       TO LIFBNRA                          CL**6
00787              MOVE ER-7718        TO EMI-ERROR                        CL**6
00788              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**6
00789                                                                      CL**6
00790      IF  CDTMODL GREATER ZEROS                                       CL**6
00791          EXEC CICS BIF DEEDIT                                        CL**6
00792              FIELD   (CDTMODI)                                       CL**6
00793              LENGTH  (6)                                             CL**6
00794              END-EXEC                                                CL**6
00795          IF  CDTMODI NUMERIC                                         CL**6
00796              MOVE CDTMODI        TO W-CRDT-MOD-PCT                   CL**6
00797                                     CDTMODO                          CL**6
00798          ELSE                                                        CL**6
00799              MOVE -1             TO CDTMODL                          CL**6
00800              MOVE AL-UABON       TO CDTMODA                          CL**6
00801              MOVE ER-7719        TO EMI-ERROR                        CL**6
00802              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*14
00803                                                                      CL*14
020816     IF PI-COMPANY-ID = 'ACE' OR 'LGX' or 'DCC' or 'VPP'
00805          IF  CANFEEL GREATER ZEROS                                   CL*17
00806                                                                      CL*17
00807              IF  CANFEEI NUMERIC                                     CL*17
00808                  MOVE CANFEEI    TO W-CANCEL-FEE                     CL*17
00809                                     CANFEEO                          CL*17
00810                                                                      CL*17
00811              ELSE                                                    CL*17
00812                  MOVE -1         TO CANFEEL                          CL*17
00813                  MOVE AL-UABON   TO CANFEEA                          CL*17
00814                  MOVE ER-3267    TO EMI-ERROR                        CL*17
00815                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL*17
00816                                                                      CL*17
00817                                                                      CL*17
00818      IF  PI-COMPANY-ID NOT EQUAL 'HAN'                               CL*14
00819              AND                                                     CL*14
00820          PI-COMPANY-ID NOT EQUAL 'JHL'                               CL*14
00821          GO TO 7099-EXIT.                                            CL*14
00822                                                                      CL*14
00823      IF  AXDISP1L GREATER ZEROS                                      CL*14
00824                                                                      CL*14
00825          EXEC CICS BIF DEEDIT                                        CL*14
00826              FIELD   (AXDISP1I)                                      CL*14
00827              LENGTH  (6)                                             CL*14
00828              END-EXEC                                                CL*14
00829                                                                      CL*14
00830          IF  AXDISP1I NUMERIC                                        CL*14
00831              MOVE AXDISP1I       TO W-EXEC1-DIS-PRCNT                CL*14
00832                                     AXDISP1O                         CL*14
00833                                                                      CL*14
00834          ELSE                                                        CL*14
00835              MOVE -1             TO AXDISP1L                         CL*14
00836              MOVE AL-UABON       TO AXDISP1A                         CL*14
00837              MOVE ER-3257        TO EMI-ERROR                        CL*14
00838              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*14
00839                                                                      CL*14
00840      IF  AXLIFP1L GREATER ZEROS                                      CL*14
00841                                                                      CL*14
00842          EXEC CICS BIF DEEDIT                                        CL*14
00843              FIELD   (AXLIFP1I)                                      CL*14
00844              LENGTH  (6)                                             CL*14
00845              END-EXEC                                                CL*14
00846                                                                      CL*14
00847          IF  AXLIFP1I NUMERIC                                        CL*14
00848              MOVE AXLIFP1I       TO W-EXEC1-LIF-PRCNT                CL*14
00849                                     AXLIFP1O                         CL*14
00850                                                                      CL*14
00851          ELSE                                                        CL*14
00852              MOVE -1             TO AXLIFP1L                         CL*14
00853              MOVE AL-UABON       TO AXLIFP1A                         CL*14
00854              MOVE ER-3258        TO EMI-ERROR                        CL*14
00855              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*14
00856                                                                      CL*14
00857                                                                      CL*14
00858      IF  AXDISP2L GREATER ZEROS                                      CL*14
00859                                                                      CL*14
00860          EXEC CICS BIF DEEDIT                                        CL*14
00861              FIELD   (AXDISP2I)                                      CL*14
00862              LENGTH  (6)                                             CL*14
00863              END-EXEC                                                CL*14
00864                                                                      CL*14
00865          IF  AXDISP2I NUMERIC                                        CL*14
00866              MOVE AXDISP2I       TO W-EXEC2-DIS-PRCNT                CL*14
00867                                     AXDISP2O                         CL*14
00868                                                                      CL*14
00869          ELSE                                                        CL*14
00870              MOVE -1             TO AXDISP2L                         CL*14
00871              MOVE AL-UABON       TO AXDISP2A                         CL*14
00872              MOVE ER-3257        TO EMI-ERROR                        CL*14
00873              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*14
00874                                                                      CL*14
00875      IF  AXLIFP2L GREATER ZEROS                                      CL*14
00876                                                                      CL*14
00877          EXEC CICS BIF DEEDIT                                        CL*14
00878              FIELD   (AXLIFP2I)                                      CL*14
00879              LENGTH  (6)                                             CL*14
00880              END-EXEC                                                CL*14
00881                                                                      CL*14
00882          IF  AXLIFP2I NUMERIC                                        CL*14
00883              MOVE AXLIFP2I       TO W-EXEC2-LIF-PRCNT                CL*14
00884                                     AXLIFP2O                         CL*14
00885                                                                      CL*14
00886          ELSE                                                        CL*14
00887              MOVE -1             TO AXLIFP2L                         CL*14
00888              MOVE AL-UABON       TO AXLIFP2A                         CL*14
00889              MOVE ER-3258        TO EMI-ERROR                        CL*14
00890              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**6
00891                                                                   EL6502
00892  7099-EXIT.                                                       EL6502
00893      EXIT.                                                        EL6502
00894      EJECT                                                        EL6502
00895                                                                      CL**6
00896  7100-READ-ERACCT.                                                EL6502
00897      EXEC CICS READ                                               EL6502
00898           DATASET  (ERACCT-FILE)                                     CL**6
00899           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*17
00900           RIDFLD   (PI-ACCT-KEY)                                  EL6502
00901      END-EXEC.                                                       CL**6
00902                                                                   EL6502
00903      MOVE AM-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL6502
00904      MOVE AM-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL6502
00905                                                                   EL6502
00906  7100-EXIT.                                                       EL6502
00907      EXIT.                                                        EL6502
00908      EJECT                                                        EL6502
00909                                                                      CL**6
00910  7300-READ-ERACCT-UPDATE.                                         EL6502
00911      EXEC CICS READ                                               EL6502
00912           DATASET  (ERACCT-FILE)                                     CL**6
00913           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*17
00914           RIDFLD   (PI-ACCT-KEY)                                  EL6502
00915           UPDATE                                                  EL6502
00916      END-EXEC.                                                    EL6502
00917                                                                   EL6502
00918  7300-EXIT.                                                       EL6502
00919      EXIT.                                                           CL*11
00920      EJECT                                                           CL*11
00921  7400-UPDATE-COMP-MAST.                                              CL*11
00922                                                                      CL*11
00923      EXEC CICS GETMAIN                                               CL*11
00924           LENGTH   (COMP-REC-LEN)                                    CL*11
00925           SET      (ADDRESS OF COMPENSATION-MASTER)                  CL*17
00926           INITIMG  (GETMAIN-SPACE)                                   CL*11
00927      END-EXEC.                                                       CL*11
00928                                                                      CL*11
00929      MOVE AM-COMPANY-CD          TO  COMP-CO-ID.                     CL*11
00930                                                                      CL*11
00931      IF PI-CAR-GROUP-ACCESS-CNTL = '1' OR '3'                        CL*11
00932          MOVE ZEROS              TO  COMP-CARRIER                    CL*11
00933      ELSE                                                            CL*11
00934          MOVE AM-CARRIER         TO  COMP-CARRIER.                   CL*11
00935                                                                      CL*11
00936      IF PI-CAR-GROUP-ACCESS-CNTL = '2' OR '3'                        CL*11
00937          MOVE ZEROS              TO  COMP-GROUPING                   CL*11
00938      ELSE                                                            CL*11
00939          MOVE AM-GROUPING        TO  COMP-GROUPING.                  CL*11
00940                                                                      CL*11
00941      MOVE 'A'                    TO  COMP-REC-TYPE.                  CL*11
00942      MOVE AM-AGT (AM-REMIT-TO)   TO  COMP-FIN-RESP.                  CL*11
00943      MOVE AM-AGT (1)             TO  COMP-ACCT-AGT.                  CL*11
00944                                                                      CL*11
00945                                                                      CL*11
00946      EXEC CICS HANDLE CONDITION                                      CL*11
00947          NOTFND   (7400-COMP-NOT-FOUND)                              CL*18
00948      END-EXEC.                                                       CL*11
00949                                                                      CL*11
00950      EXEC CICS READ                                                  CL*11
00951           DATASET  (ERCOMP-FILE)                                     CL*11
00952           SET      (ADDRESS OF COMPENSATION-MASTER)                  CL*17
00953           RIDFLD   (ERCOMP-KEY)                                      CL*11
00954           UPDATE                                                     CL*11
00955      END-EXEC.                                                       CL*11
00956                                                                      CL*11
00957      MOVE RPTCD2I                TO  CO-RPTCD2.                      CL*11
00958                                                                      CL*11
00959      EXEC CICS REWRITE                                               CL*11
00960           DATASET  (ERCOMP-FILE)                                     CL*11
00961           FROM     (COMPENSATION-MASTER)                             CL*11
00962      END-EXEC.                                                       CL*11
00963                                                                      CL*18
00964      GO TO 7400-EXIT.                                                CL*18
00965                                                                      CL*18
00966  7400-COMP-NOT-FOUND.                                                CL*18
00967      MOVE ER-2114                TO  EMI-ERROR.                      CL*18
00968      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*18
00969                                                                      CL*11
00970  7400-EXIT.                                                          CL*11
00971      EXIT.                                                        EL6502
00972      EJECT                                                        EL6502
00973  7800-COMPANY-REC-READ.                                           EL6502
00974      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6502
00975      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6502
00976      MOVE '1'                    TO  CNTL-REC-TYPE.               EL6502
00977      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6502
00978      EXEC CICS HANDLE CONDITION                                   EL6502
00979          NOTFND   (7880-NO-COMP)                                  EL6502
00980      END-EXEC.                                                    EL6502
00981                                                                   EL6502
00982      EXEC CICS READ                                               EL6502
00983          DATASET   (ELCNTL-FILE)                                     CL**6
00984          SET       (ADDRESS OF CONTROL-FILE)                         CL*17
00985          RIDFLD    (ELCNTL-KEY)                                   EL6502
00986      END-EXEC.                                                    EL6502
00987                                                                   EL6502
00988      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES                     EL6502
00989          MOVE ER-2572            TO  EMI-ERROR                    EL6502
00990          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6502
00991                                                                   EL6502
00992      MOVE CF-REPORT-CD1-CAPTION  TO WS-SAVE-REPORT-CODE1.            CL*10
00993      MOVE CF-REPORT-CD2-CAPTION  TO WS-SAVE-REPORT-CODE2
120706     MOVE 'RPT CODE 3'           TO WS-SAVE-REPORT-CODE3

00995      GO TO 7899-EXIT.                                             EL6502
00996                                                                   EL6502
00997  7880-NO-COMP.                                                    EL6502
00998      MOVE ER-0002                TO  EMI-ERROR.                      CL*10
00999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6502
01000                                                                   EL6502
01001  7899-EXIT.                                                       EL6502
01002      EXIT.                                                        EL6502
01003      EJECT                                                        EL6502
01004  8000-UPDATE-MAINT-DATE.                                          EL6502
01005      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6502
01006                                                                   EL6502
01007      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6502
01008      MOVE '1'                    TO  CNTL-REC-TYPE.               EL6502
01009      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6502
01010                                                                   EL6502
01011      EXEC CICS HANDLE CONDITION                                   EL6502
01012          NOTFND   (8000-EXIT)                                     EL6502
01013      END-EXEC.                                                    EL6502
01014                                                                   EL6502
01015      EXEC CICS READ                                               EL6502
01016          UPDATE                                                   EL6502
01017          DATASET   (ELCNTL-FILE)                                     CL**6
01018          SET       (ADDRESS OF CONTROL-FILE)                         CL*17
01019          RIDFLD    (ELCNTL-KEY)                                   EL6502
01020      END-EXEC.                                                    EL6502
01021                                                                   EL6502
01022      MOVE BIN-CURRENT-SAVE       TO  CF-ACCOUNT-MSTR-MAINT-DT.    EL6502
01023                                                                   EL6502
01024      EXEC CICS REWRITE                                            EL6502
01025          DATASET   (ELCNTL-FILE)                                     CL**6
01026          FROM      (CONTROL-FILE)                                    CL**4
01027      END-EXEC.                                                    EL6502
01028                                                                   EL6502
01029  8000-EXIT.                                                       EL6502
01030       EXIT.                                                       EL6502
01031      EJECT                                                        EL6502
01032                                                                      CL**6
01033  8100-SEND-INITIAL-MAP.                                           EL6502
01034      MOVE SAVE-DATE              TO  DATEO.                       EL6502
01035      MOVE EIBTIME                TO  TIME-IN.                     EL6502
01036      MOVE TIME-OUT               TO  TIMEO.                       EL6502
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01037      MOVE -1                     TO  PFENTERL.                       CL**6
01038      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6502
01039                                                                      CL*14
01040      IF WS-SAVE-REPORT-CODE1 NOT = SPACES AND LOW-VALUES             CL*15
01041          MOVE WS-SAVE-REPORT-CODE1  TO WS-REPORT-CD-CAPTION          CL*15
01042          MOVE WS-REPORT-CODE-CAPTION TO RCAP1O.                      CL*15
01043                                                                      CL*15
01044      IF WS-SAVE-REPORT-CODE2 NOT = SPACES AND LOW-VALUES             CL*15
01045          MOVE WS-SAVE-REPORT-CODE2  TO WS-REPORT-CD-CAPTION          CL*15
01046          MOVE WS-REPORT-CODE-CAPTION TO RCAP2O.                      CL*15
01047                                                                      CL*15
120706     IF WS-SAVE-REPORT-CODE3 NOT = SPACES AND LOW-VALUES
120706         MOVE WS-SAVE-REPORT-CODE3
120706                                 TO WS-REPORT-CD-CAPTION
120706         MOVE WS-REPORT-CODE-CAPTION
120706                                 TO RCAP3O
120706     END-IF

01048      IF  PI-COMPANY-ID NOT EQUAL 'JHL'                               CL*14
01049              AND                                                     CL*14
01050          PI-COMPANY-ID NOT EQUAL 'HAN'                               CL*14
01051          MOVE AL-SADOF           TO ACCEXTA                          CL*14
01052                                     CNTLTA                           CL*14
01053                                     CNTLTITA                         CL*14
01054                                     ACCEXT1A                         CL*14
01055                                     AXNAME1A                         CL*14
01056                                     AXDIST1A                         CL*14
01057                                     AXDISP1A                         CL*14
01058                                     AXLIFT1A                         CL*14
01059                                     AXLIFP1A                         CL*14
01060                                     ACCEXT2A                         CL*14
01061                                     AXNAME2A                         CL*14
01062                                     AXDIST2A                         CL*14
01063                                     AXDISP2A                         CL*14
01064                                     AXLIFT2A                         CL*14
01065                                     AXLIFP2A.                        CL*14
01066                                                                      CL*17
01067 *****     CANCELATION FEE IS CUSTOMIZED FOR CLIENT ACE ONLY      *   CL*17
020816     IF PI-COMPANY-ID = 'ACE' OR 'LGX' or 'DCC' or 'VPP'
01069         MOVE WS-CANCEL-TEXT      TO CANTXTO                          CL*17
01070         IF AM-CANCEL-FEE NUMERIC                                     CL*17
01071            MOVE AM-CANCEL-FEE    TO W-CANCEL-FEE-LONG                CL*17
01072            MOVE W-CANCEL-FEE-LONG  TO W-CANCEL-FEE                   CL*17
01073         ELSE                                                         CL*17
01074            MOVE ZEROES           TO W-CANCEL-FEE                     CL*17
01075         END-IF                                                       CL*17
01076         MOVE W-CANCEL-FEE        TO CANFEEO                          CL*17
01077      ELSE                                                            CL*17
01078         MOVE AL-SADOF            TO CANTXTA                          CL*17
01079         MOVE AL-SANOF            TO CANFEEA.                         CL*17
01080                                                                   EL6502
01081      IF PI-COMPANY-ID = 'LGX' OR 'CRI'                               CL**6
01082         MOVE AL-SANOF            TO  EMPCAPA                         CL**6
01083         MOVE AL-UANON            TO  EMPSTMTA.                       CL**6
01084                                                                   EL6502
01085      IF PI-COMPANY-ID = 'FLI' OR 'FLU' OR 'LGX'                      CL*13
01086         MOVE WS-FLI-PFK-DESC     TO  FLIPFKO.                        CL*13
01087                                                                      CL*13
PEMMOD     SET T-INDEX                 TO PI-LINE-SELECTED
PEMMOD
PEMMOD     IF PI-2ND-PAGE
PEMMOD        SET T-INDEX UP BY 8
PEMMOD     ELSE
PEMMOD        IF PI-3RD-PAGE
PEMMOD           SET T-INDEX UP BY 16
PEMMOD        ELSE
PEMMOD           IF PI-LST-PAGE
PEMMOD              SET T-INDEX UP BY 24
PEMMOD           END-IF
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     MOVE PI-ACCT-CARRIER        TO CARRI
PEMMOD     MOVE PI-ACCT-GROUPING       TO GROUPINI
PEMMOD     MOVE PI-ACCT-STATE          TO STATEI
PEMMOD     MOVE PI-ACCT-ACCOUNT        TO ACCOUNTI
PEMMOD
PEMMOD     IF PI-ACCT-EXP-DT = LOW-VALUES
PEMMOD        MOVE SPACES              TO EXPDTEI
PEMMOD     ELSE
PEMMOD        IF PI-ACCT-EXP-DT NOT = HIGH-VALUES
PEMMOD           MOVE PI-ACCT-EXP-DT   TO DC-BIN-DATE-1
PEMMOD           MOVE SPACE            TO DC-OPTION-CODE
PEMMOD           PERFORM 9700-LINK-DATE-CONVERT
PEMMOD                                 THRU 9700-EXIT
PEMMOD           MOVE DC-GREG-DATE-1-EDIT
PEMMOD                                 TO EXPDTEI
PEMMOD        ELSE
PEMMOD           MOVE 999999           TO EXPDTEO
PEMMOD           INSPECT EXPDTEI CONVERTING SPACES TO '/'
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     MOVE EXPDTEI (1:2)          TO WS-YYYYMMDD (5:2)
PEMMOD     MOVE EXPDTEI (4:2)          TO WS-YYYYMMDD (7:2)
PEMMOD     MOVE EXPDTEI (6:2)          TO WS-YYYYMMDD (3:2)
PEMMOD     IF EXPDTEI (7:1) NOT = '9'
PEMMOD        MOVE '20'                TO WS-YYYYMMDD (1:2)
PEMMOD     ELSE
PEMMOD        MOVE '19'                TO WS-YYYYMMDD (1:2)
PEMMOD     END-IF
PEMMOD
PEMMOD     MOVE PI-BIN-EFF-DT (T-INDEX)
PEMMOD                                 TO DC-BIN-DATE-1
PEMMOD     MOVE SPACE                  TO DC-OPTION-CODE
PEMMOD     PERFORM 9700-LINK-DATE-CONVERT
PEMMOD                                 THRU 9700-EXIT
PEMMOD     MOVE DC-GREG-DATE-1-EDIT    TO EFFDTEI
PEMMOD
01088      EXEC CICS SEND                                               EL6502
01089          MAP      (MAP-NAME)                                      EL6502
01090          MAPSET   (MAPSET-NAME)                                   EL6502
01091          FROM     (EL6502AO)                                      EL6502
01092          ERASE                                                    EL6502
01093          CURSOR                                                   EL6502
01094      END-EXEC.                                                    EL6502
01095                                                                   EL6502
01096      GO TO 9100-RETURN-TRAN.                                      EL6502
01097                                                                   EL6502
01098  8200-SEND-DATAONLY.                                              EL6502
01099      MOVE SAVE-DATE              TO  DATEO.                       EL6502
01100      MOVE EIBTIME                TO  TIME-IN.                     EL6502
01101      MOVE TIME-OUT               TO  TIMEO.                       EL6502
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01102      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                       CL**8
01103                                                                      CL*15
01104      IF WS-SAVE-REPORT-CODE1 NOT = SPACES AND LOW-VALUES             CL*15
01105          MOVE WS-SAVE-REPORT-CODE1  TO WS-REPORT-CD-CAPTION          CL*15
01106          MOVE WS-REPORT-CODE-CAPTION TO RCAP1O.                      CL*15
01107                                                                      CL*15
01108      IF WS-SAVE-REPORT-CODE2 NOT = SPACES AND LOW-VALUES             CL*15
01109          MOVE WS-SAVE-REPORT-CODE2  TO WS-REPORT-CD-CAPTION          CL*15
01110          MOVE WS-REPORT-CODE-CAPTION TO RCAP2O.                      CL*15
01111                                                                      CL*13
120706     IF WS-SAVE-REPORT-CODE3 NOT = SPACES AND LOW-VALUES
120706         MOVE WS-SAVE-REPORT-CODE3
120706                                 TO WS-REPORT-CD-CAPTION
120706         MOVE WS-REPORT-CODE-CAPTION
120706                                 TO RCAP3O
120706     END-IF

01112      IF PI-COMPANY-ID = 'FLI' OR 'FLU' OR 'LGX'                      CL*13
01113         MOVE WS-FLI-PFK-DESC     TO  FLIPFKO.                        CL*13
01114                                                                   EL6502
01115      IF  PI-COMPANY-ID NOT EQUAL 'JHL'                               CL*14
01116              AND                                                     CL*14
01117          PI-COMPANY-ID NOT EQUAL 'HAN'                               CL*14
01118          MOVE AL-SADOF           TO ACCEXTA                          CL*14
01119                                     CNTLTITA                         CL*14
01120                                     CNTLTA                           CL*14
01121                                     ACCEXT1A                         CL*14
01122                                     AXNAME1A                         CL*14
01123                                     AXDIST1A                         CL*14
01124                                     AXDISP1A                         CL*14
01125                                     AXLIFT1A                         CL*14
01126                                     AXLIFP1A                         CL*14
01127                                     ACCEXT2A                         CL*14
01128                                     AXNAME2A                         CL*14
01129                                     AXDIST2A                         CL*14
01130                                     AXDISP2A                         CL*14
01131                                     AXLIFT2A                         CL*14
01132                                     AXLIFP2A.                        CL*14
01133                                                                      CL*14
01134 *****     CANCELATION FEE IS CUSTOMIZED FOR CLIENT ACE ONLY      *   CL*17
020816     IF PI-COMPANY-ID = 'ACE' OR 'LGX' or 'DCC' or 'VPP'
01136         MOVE WS-CANCEL-TEXT      TO CANTXTO                          CL*17
01137         IF AM-CANCEL-FEE  NUMERIC                                    CL*17
01138            MOVE AM-CANCEL-FEE    TO W-CANCEL-FEE-LONG                CL*17
01139            MOVE W-CANCEL-FEE-LONG   TO W-CANCEL-FEE                  CL*17
01140         ELSE                                                         CL*17
01141            MOVE ZEROES           TO W-CANCEL-FEE                     CL*17
01142         END-IF                                                       CL*17
01143         MOVE W-CANCEL-FEE        TO CANFEEO                          CL*17
01144      ELSE                                                            CL*17
01145         MOVE AL-SADOF            TO CANTXTA                          CL*17
01146         MOVE AL-SANOF            TO CANFEEA.                         CL*17
01147                                                                      CL*17
01148      EXEC CICS SEND                                               EL6502
01149          MAP      (MAP-NAME)                                      EL6502
01150          MAPSET   (MAPSET-NAME)                                   EL6502
01151          FROM     (EL6502AO)                                      EL6502
01152          DATAONLY                                                 EL6502
01153          CURSOR                                                   EL6502
01154      END-EXEC.                                                    EL6502
01155                                                                   EL6502
01156      GO TO 9100-RETURN-TRAN.                                      EL6502
01157                                                                   EL6502
01158  8300-SEND-TEXT.                                                  EL6502
01159      EXEC CICS SEND TEXT                                          EL6502
01160          FROM     (LOGOFF-TEXT)                                   EL6502
01161          LENGTH   (LOGOFF-LENGTH)                                 EL6502
01162          ERASE                                                    EL6502
01163          FREEKB                                                   EL6502
01164      END-EXEC.                                                    EL6502
01165                                                                   EL6502
01166      EXEC CICS RETURN                                             EL6502
01167      END-EXEC.                                                    EL6502
01168                                                                      CL**6
01169  8600-DEEDIT.                                                        CL**6
01170      EXEC CICS BIF DEEDIT                                            CL**6
01171           FIELD(DEEDIT-FIELD)                                        CL**6
01172           LENGTH(15)                                                 CL**6
01173      END-EXEC.                                                       CL**6
01174                                                                   EL6502
01175  8800-UNAUTHORIZED-ACCESS.                                        EL6502
01176      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL6502
01177      GO TO 8300-SEND-TEXT.                                        EL6502
01178                                                                   EL6502
01179  8810-PF23.                                                       EL6502
01180      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6502
01181      MOVE XCTL-005               TO  PGM-NAME.                    EL6502
01182      GO TO 9300-XCTL.                                             EL6502
01183                                                                   EL6502
01184  9100-RETURN-TRAN.                                                EL6502
01185      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6502
01186      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL6502
01187      EXEC CICS RETURN                                             EL6502
01188          TRANSID    (TRANS-ID)                                    EL6502
01189          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6502
01190          LENGTH     (WS-COMM-LENGTH)                                 CL**4
01191      END-EXEC.                                                       CL**4
01192                                                                   EL6502
01193  9200-RETURN-MAIN-MENU.                                           EL6502
01194      MOVE XCTL-626               TO  PGM-NAME.                    EL6502
01195      GO TO 9300-XCTL.                                             EL6502
01196                                                                   EL6502
01197  9300-XCTL.                                                       EL6502
01198      EXEC CICS XCTL                                               EL6502
01199          PROGRAM    (PGM-NAME)                                    EL6502
01200          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6502
01201          LENGTH     (WS-COMM-LENGTH)                                 CL**4
01202      END-EXEC.                                                       CL**4
01203                                                                   EL6502
01204  9400-CLEAR.                                                      EL6502
01205      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL6502
01206      GO TO 9300-XCTL.                                             EL6502
01207                                                                   EL6502
01208  9500-PF12.                                                       EL6502
01209      MOVE XCTL-010               TO  PGM-NAME.                    EL6502
01210      GO TO 9300-XCTL.                                             EL6502
01211                                                                   EL6502
01212  9600-PGMID-ERROR.                                                EL6502
01213      EXEC CICS HANDLE CONDITION                                   EL6502
01214          PGMIDERR    (8300-SEND-TEXT)                             EL6502
01215      END-EXEC.                                                    EL6502
01216                                                                   EL6502
01217      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL6502
01218      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL6502
01219      MOVE XCTL-005               TO  PGM-NAME.                    EL6502
01220      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL6502
01221      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6502
01222      GO TO 9300-XCTL.                                             EL6502
01223                                                                   EL6502
01224  9700-LINK-DATE-CONVERT.                                          EL6502
01225      EXEC CICS LINK                                               EL6502
01226          PROGRAM    ('ELDATCV')                                   EL6502
01227          COMMAREA   (DATE-CONVERSION-DATA)                        EL6502
01228          LENGTH     (DC-COMM-LENGTH)                              EL6502
01229      END-EXEC.                                                       CL**4
01230                                                                      CL**4
01231  9700-EXIT.                                                       EL6502
01232      EXIT.                                                        EL6502
01233                                                                   EL6502
01234  9900-ERROR-FORMAT.                                               EL6502
01235      IF NOT EMI-ERRORS-COMPLETE                                   EL6502
01236          MOVE LINK-001           TO  PGM-NAME                     EL6502
01237          EXEC CICS LINK                                           EL6502
01238              PROGRAM    (PGM-NAME)                                EL6502
01239              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6502
01240              LENGTH     (EMI-COMM-LENGTH)                         EL6502
01241          END-EXEC.                                                EL6502
01242                                                                   EL6502
01243  9900-EXIT.                                                       EL6502
01244      EXIT.                                                        EL6502
01245                                                                   EL6502
01246  9990-ABEND.                                                      EL6502
01247      MOVE LINK-004               TO  PGM-NAME.                    EL6502
01248      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL6502
01249      EXEC CICS LINK                                               EL6502
01250          PROGRAM   (PGM-NAME)                                     EL6502
01251          COMMAREA  (EMI-LINE1)                                    EL6502
01252          LENGTH    (72)                                           EL6502
01253      END-EXEC.                                                    EL6502
01254                                                                   EL6502
01255      GO TO 8200-SEND-DATAONLY.                                    EL6502
01256                                                                   EL6502
01257  9995-SECURITY-VIOLATION.                                         EL6502
01258             COPY ELCSCTP.                                            CL**6
01259  9995-EXIT.                                                       EL6502
01260       EXIT.                                                          CL**6
