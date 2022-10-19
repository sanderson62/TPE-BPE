00001  IDENTIFICATION DIVISION.                                         08/19/98
00002                                                                   EL525
00003  PROGRAM-ID.                 EL525 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL525
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL525
00006 *              CONVERSION DATE 03/22/96 08:53:58.                 EL525
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL525
00008 *                           VMOD=2.010                            EL525
00009                                                                   EL525
00010 *AUTHOR.     LOGIC, INC.                                          EL525
00011 *            DALLAS, TEXAS.                                       EL525
00012                                                                   EL525
00013 *DATE-COMPILED.                                                   EL525
00014                                                                   EL525
00015 *SECURITY.   *****************************************************EL525
00016 *            *                                                   *EL525
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL525
00018 *            *                                                   *EL525
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL525
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL525
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL525
00022 *            *                                                   *EL525
00023 *            *****************************************************EL525
00024                                                                   EL525
00025 *REMARKS.                                                         EL525
00026 *        THIS PROGRAM IS RUN AT MONTH END TO UPDATE THE CREDIT    EL525
00027 *       ACCEPT DATES FOR ERPNDB, ERPNDC, ERPYAJ ERRQST FILES.     EL525
00028                                                                   EL525
00029                                                                   EL525
00030 *    INPUT FILES  - ELCNTL - CONTROL FILE                         EL525
00031 *                 - EREXTR - CREDIT EXTRACT INTERFACE             EL525
00032 *                 - ELDATE - DATE CARD FILE                       EL525
00033                                                                   EL525
00034 *    OUTPUT FILES - ERPNDB - PENDING BUSINESS FILE                EL525
00035 *                 - ERPNDC - PENDING CLAIMS FILE                  EL525
00036 *                 - ERPYAJ - PAYMENTS AND ADJUSTMENTS FILE        EL525
00037 *                 - ERCRTC - CERTIFICATE CHANGES FILE             EL525
00038 *                 - ERREPY - PENDING RETRO/REIN ADJUSTMENTS       EL525
00039 *                 - ERRQST - A/R REQUEST FILE                     EL525
062104******************************************************************
062104*                   C H A N G E   L O G
062104*
062104* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062104*-----------------------------------------------------------------
062104*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062104* EFFECTIVE    NUMBER
062104*-----------------------------------------------------------------
062104* 062104    2004050700001  SMVA  ADD NEW FILE TO AUTOMATE ME BALANCING
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
062104******************************************************************
00040                                                                   EL525
00041      EJECT                                                        EL525
00042  ENVIRONMENT DIVISION.                                            EL525
00043  CONFIGURATION SECTION.                                           EL525
00044  SPECIAL-NAMES.                                                   EL525
00045      C02 IS LCP-CH2                                               EL525
00046      C03 IS LCP-CH3                                               EL525
00047      C04 IS LCP-CH4                                               EL525
00048      C05 IS LCP-CH5                                               EL525
00049      C06 IS LCP-CH6                                               EL525
00050      C07 IS LCP-CH7                                               EL525
00051      C08 IS LCP-CH8                                               EL525
00052      C09 IS LCP-CH9                                               EL525
00053      C10 IS LCP-CH10                                              EL525
00054      C11 IS LCP-CH11                                              EL525
00055      C12 IS LCP-CH12                                              EL525
00056      S01 IS LCP-P01                                               EL525
00057      S02 IS LCP-P02.                                              EL525
00058                                                                   EL525
00059  INPUT-OUTPUT SECTION.                                            EL525
00060                                                                   EL525
00061  FILE-CONTROL.                                                    EL525
00062                                                                   EL525
00063      SELECT EXTRACT-INTERFACE-FILE                                EL525
00064                              ASSIGN TO SYS010.
00065                                                                   EL525
00066      SELECT DISK-DATE        ASSIGN TO SYS019.
00067                                                                   EL525
00068      SELECT PRNTR            ASSIGN TO SYS008.
00069                                                                   EL525
00070      SELECT ELCNTL           ASSIGN TO SYS021-FBA1-ELCNTL
00071                              ORGANIZATION INDEXED                 EL525
00072                              ACCESS       DYNAMIC                 EL525
00073                              RECORD KEY   CF-CONTROL-PRIMARY      EL525
00074                              FILE STATUS  ELCNTL-FILE-STATUS.     EL525
00075                                                                   EL525
00076      SELECT ERPNDB           ASSIGN TO SYS022-FBA1-ERPNDB
00077                              ORGANIZATION INDEXED                 EL525
00078                              ACCESS       DYNAMIC                 EL525
00079                              RECORD KEY   PB-CONTROL-PRIMARY      EL525
00080                              FILE STATUS  ERPNDB-FILE-STATUS.     EL525
00081                                                                   EL525
00082      SELECT ERPNDC           ASSIGN TO SYS023-FBA1-ERPNDC 
00083                              ORGANIZATION INDEXED                 EL525
00084                              ACCESS       DYNAMIC                 EL525
00085                              RECORD KEY   PC-CONTROL-PRIMARY      EL525
00086                              FILE STATUS  ERPNDC-FILE-STATUS.     EL525
00087                                                                   EL525
00088      SELECT ERPYAJ           ASSIGN TO SYS024-FBA1-ERPYAJ
00089                              ORGANIZATION INDEXED                 EL525
00090                              ACCESS       DYNAMIC                 EL525
00091                              RECORD KEY   PY-CONTROL-PRIMARY      EL525
00092                              FILE STATUS  ERPYAJ-FILE-STATUS.     EL525
00093                                                                   EL525
00094      SELECT ERCRTC           ASSIGN TO SYS025-FBA1-ERCRTC
00095                              ORGANIZATION INDEXED                 EL525
00096                              ACCESS       DYNAMIC                 EL525
00097                              RECORD KEY   CC-CONTROL-PRIMARY      EL525
00098                              FILE STATUS  ERCRTC-FILE-STATUS.     EL525
00099                                                                   EL525
00100      SELECT ELREPT           ASSIGN TO SYS026-FBA1-ELREPT
00101                              ORGANIZATION INDEXED                 EL525
00102                              ACCESS       DYNAMIC                 EL525
00103                              RECORD KEY   RF-CONTROL-PRIMARY      EL525
00104                              FILE STATUS  DTE-VSAM-FLAGS.         EL525
00105                                                                   EL525
00106      SELECT ERREPY           ASSIGN TO SYS027-FBA1-ERREPY
00107                              ORGANIZATION INDEXED                 EL525
00108                              ACCESS       DYNAMIC                 EL525
00109                              RECORD KEY   RP-CONTROL-PRIMARY      EL525
00110                              FILE STATUS  ERREPY-FILE-STATUS.     EL525
00111                                                                   EL525
00112      SELECT ERRQST           ASSIGN TO SYS028-FBA1-ERRQST
00113                              ORGANIZATION INDEXED                 EL525
00114                              ACCESS       DYNAMIC                 EL525
00115                              RECORD KEY   RQ-CONTROL-PRIMARY      EL525
00116                              FILE STATUS  ERRQST-FILE-STATUS.     EL525
00117                                                                   EL525
00118      SELECT JOURNAL-LOG-FILE                                      EL525
00119                              ASSIGN TO SYS011.
00120                                                                   EL525
00121      SELECT ERMEBL           ASSIGN TO SYS030-FBA1-ERMEBL
00122                              ORGANIZATION INDEXED                 EL525
00123                              ACCESS       DYNAMIC                 EL525
00124                              RECORD KEY   ME-CONTROL-PRIMARY      EL525
00125                              FILE STATUS  ERMEBL-FILE-STATUS.     EL525

062104     SELECT ME50-EL525-BALANCE
062104                           ASSIGN TO SYS012
062104                           ORGANIZATION IS LINE SEQUENTIAL.
00126                                                                   EL525
00127      EJECT                                                        EL525
00128  DATA DIVISION.                                                   EL525
00129                                                                   EL525
00130  FILE SECTION.                                                    EL525
00131                                                                   EL525
00132  FD  DISK-DATE               COPY ELCDTEFD SUPPRESS.              EL525
00133                                                                   EL525
00134  FD  ELREPT                  COPY ELCRPTFD.                       EL525
00135                                                                   EL525
00136                              COPY ELCREPT.                        EL525
00137                                                                   EL525
00138  FD  ELCNTL.                                                      EL525
00139                                      COPY ELCCNTL.                EL525
00140                                                                   EL525
00141      EJECT                                                        EL525
00142  FD  ERPNDB.                                                      EL525
00143                                      COPY ERCPNDB.                EL525
00144                                                                   EL525
00145      EJECT                                                        EL525
00146  FD  ERPNDC.                                                      EL525
00147                                      COPY ERCPNDC.                EL525
00148                                                                   EL525
00149      EJECT                                                        EL525
00150  FD  ERPYAJ.                                                      EL525
00151                                      COPY ERCPYAJ.                EL525
00152                                                                   EL525
00153      EJECT                                                        EL525
00154  FD  ERCRTC.                                                      EL525
00155                                      COPY ERCCRTC.                EL525
00156                                                                   EL525
00157      EJECT                                                        EL525
00158  FD  ERREPY.                                                      EL525
00159                                      COPY ERCREPY.                EL525
00160                                                                   EL525
00161      EJECT                                                        EL525
00162  FD  ERRQST.                                                      EL525
00163                                      COPY ERCRQST.                EL525
00164                                                                   EL525
00165      EJECT                                                        EL525
00166  FD  EXTRACT-INTERFACE-FILE          COPY ERCEXTFD.               EL525
00167                                      COPY ERCEXTR.                EL525
00168                                                                   EL525
00169  FD  PRNTR                           COPY ELCPRTFD.               EL525
00170                                                                   EL525
00171      EJECT                                                        EL525
00172  FD  JOURNAL-LOG-FILE                                             EL525
00173      RECORDING MODE IS V.                                         EL525
00174                                                                   EL525
00175                        COPY ELCSLR.                               EL525
00176                                                                   EL525
00177      EJECT                                                        EL525
00178  FD  ERMEBL.                                                      EL525
00179                                                                   EL525
00180                         COPY ERCMEBL.                             EL525

062104 FD  ME50-EL525-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME50-EL525-BALANCE-REC    PIC X(95).
00181                                                                   EL525
00182      EJECT                                                        EL525
00183                                                                   EL525
00184  WORKING-STORAGE SECTION.                                         EL525
00185  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL525
00186  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL525
00187  77  LCP-ASA                       PIC X.                         EL525
00188                                                                   EL525
00189  77  FILLER  PIC X(32)   VALUE '********************************'.EL525
00190  77  FILLER  PIC X(32)   VALUE '*     EL525  WORKING STORAGE   *'.EL525
00191  77  FILLER  PIC X(32)   VALUE '**********VMOD=2.010 ***********'.EL525
00192                                                                   EL525
00193  01  MONTH-END-DATA.                                              EL525
00194      12  ME-START-DATE.                                           EL525
00195          16  ME-START-MO         PIC 99.                          EL525
00196          16  FILLER              PIC X.                           EL525
00197          16  ME-START-DA         PIC 99.                          EL525
00198          16  FILLER              PIC X.                           EL525
00199          16  ME-START-YR         PIC 99.                          EL525
00200      12  ME-CNDS-DATE            PIC 9(6).                        EL525
00201      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   EL525
00202          16  ME-CNDS-MO          PIC 99.                          EL525
00203          16  ME-CNDS-DA          PIC 99.                          EL525
00204          16  ME-CNDS-YR          PIC 99.                          EL525
00205      12  ME-START-TIME           PIC 9(6).                        EL525
00206      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 EL525
00207          88  ME-DO-UPDATE        VALUE 'Y'.                       EL525
00208          88  ME-NO-UPDATE        VALUE 'N'.                       EL525
00209      12  ERMEBL-FILE-STATUS      PIC XX.                          EL525
00210      12  MONTH-END-MOYR          PIC 9999 COMP.                   EL525

062104 01  WS-BAL50-DESCRIPTION          PIC X(50)  VALUE
062104     'EL522 PB should be +/-15 of EL525 PB recs updated '.

062104 01  WS-ME50-BALANCE-REC.
062104     12  WS-ME50-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME50-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-STEP          PIC X(08)  VALUE 'EL525   '.
062104     12  WS-ME50-BAL-DELIM2        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-LOW       PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM3        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-HIGH      PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM4        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-DESCRIP       PIC X(50)  VALUE SPACES.

00212  01  FILLER                          COMP-3.                      EL525
00213      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL525
00214      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   EL525
00215      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL525
00216      05  WS-REPORT-SW                PIC S9          VALUE +1.    EL525
00217      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL525
00218      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL525
00219      05  WS-TIME-WRITTEN             PIC S9(7)       VALUE ZERO.  EL525
00220      05  WS-DATE-WRITTEN             PIC S9(7)       VALUE ZERO.  EL525
00221      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL525
00222      05  WS-MONTHS-WORK              PIC S9(3)       VALUE ZERO.  EL525
00223      05  WS-MONTH-END                PIC S9          VALUE ZERO.  EL525
00224          88  THIS-IS-NOT-MONTH-END                   VALUE ZERO.  EL525
00225          88  THIS-IS-MONTH-END                       VALUE +1.    EL525
00226      05  WS-CURRENT-TIME             PIC S9(7)       VALUE ZERO.  EL525
00227      05  WS-RECORDS-RELEASED         PIC S9(7)       VALUE ZERO.  EL525
00228      05  WS-RECORDS-RETURNED         PIC S9(7)       VALUE ZERO.  EL525
00229                                                                   EL525
00230                                                                   EL525
00231  01  FILLER                          COMP SYNC.                   EL525
00232      05  PGM-SUB                     PIC S9(4)       VALUE +525.  EL525
00233      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL525
00234      05  WS-LENGTH                   REDEFINES                    EL525
00235          WS-INDEX                    PIC S9(4).                   EL525
00236                                                                   EL525
00237  01  WS-BATCH-RECORD.                                             EL525
00238      05  FILLER                      PIC X(10).                   EL525
00239      05  WS-LF-ISS-PRM-REMITTED      PIC S9(9)V99 COMP-3.         EL525
00240      05  WS-LF-ISS-PRM-ENTERED       PIC S9(9)V99 COMP-3.         EL525
00241      05  WS-LF-ISS-PRM-COMPUTED      PIC S9(9)V99 COMP-3.         EL525
00242      05  WS-LF-CAN-PRM-REMITTED      PIC S9(9)V99 COMP-3.         EL525
00243      05  WS-LF-CAN-PRM-ENTERED       PIC S9(9)V99 COMP-3.         EL525
00244      05  WS-LF-CAN-PRM-COMPUTED      PIC S9(9)V99 COMP-3.         EL525
00245      05  WS-AH-ISS-PRM-REMITTED      PIC S9(9)V99 COMP-3.         EL525
00246      05  WS-AH-ISS-PRM-ENTERED       PIC S9(9)V99 COMP-3.         EL525
00247      05  WS-AH-ISS-PRM-COMPUTED      PIC S9(9)V99 COMP-3.         EL525
00248      05  WS-AH-CAN-PRM-REMITTED      PIC S9(9)V99 COMP-3.         EL525
00249      05  WS-AH-CAN-PRM-ENTERED       PIC S9(9)V99 COMP-3.         EL525
00250      05  WS-AH-CAN-PRM-COMPUTED      PIC S9(9)V99 COMP-3.         EL525
00251      05  WS-ISSUE-CNT-REMITTED       PIC S9(5)    COMP-3.         EL525
00252      05  WS-ISSUE-CNT-ENTERED        PIC S9(5)    COMP-3.         EL525
00253      05  WS-CANCEL-CNT-REMITTED      PIC S9(5)    COMP-3.         EL525
00254      05  WS-CANCEL-CNT-ENTERED       PIC S9(5)    COMP-3.         EL525
00255      05  WS-HIGHEST-SEQ-NO           PIC S9(4)    COMP.           EL525
00256      05  FILLER                      PIC X(139).                  EL525
00257                                                                   EL525
00258  01  WS-TOTAL-TABLE.                                              EL525
00259      05  WS-TOTAL-1.                                              EL525
00260          10  WS-ERPNDB-DESC          PIC X(46)       VALUE        EL525
00261              ' PENDING BUSINESS RECORDS UPDATED - '.              EL525
00262          10  WS-ERPNDB-COUNT         PIC S9(7)       VALUE ZERO.  EL525
00263      05  WS-TOTAL-1A.                                             EL525
00264          10  WS-ERPNDB-DESC-ISS      PIC X(46)       VALUE        EL525
00265              ' PENDING BUSINESS ISSUES UPDATED  - '.              EL525
00266          10  WS-ERPNDB-COUNT-ISS     PIC S9(7)       VALUE ZERO.  EL525
00267      05  WS-TOTAL-1B.                                             EL525
00268          10  WS-ERPNDB-DESC-CAN      PIC X(46)       VALUE        EL525
00269              ' PENDING BUSINESS CANCELS UPDATED - '.              EL525
00270          10  WS-ERPNDB-COUNT-CAN     PIC S9(7)       VALUE ZERO.  EL525
00271      05  WS-TOTAL-1C.                                             EL525
00272          10  WS-ERPNDB-DESC-BATCH    PIC X(46)       VALUE        EL525
00273              ' PENDING BUSINESS BATCHS UPDATED  - '.              EL525
00274          10  WS-ERPNDB-COUNT-BATCH   PIC S9(7)       VALUE ZERO.  EL525
00275      05  WS-TOTAL-2.                                              EL525
00276          10  WS-ERPNDC-DESC          PIC X(46)       VALUE        EL525
00277              ' PENDING CLAIMS RECORDS UPDATED   - '.              EL525
00278          10  WS-ERPNDC-COUNT         PIC S9(7)       VALUE ZERO.  EL525
00279      05  WS-TOTAL-2A.                                             EL525
00280          10  WS-ERPNDC-DESC-PMTS     PIC X(46)       VALUE        EL525
00281              ' PENDING CLAIMS PAYMENTS UPDATED  - '.              EL525
00282          10  WS-ERPNDC-COUNT-PMTS    PIC S9(7)       VALUE ZERO.  EL525
00283      05  WS-TOTAL-2B.                                             EL525
00284          10  WS-ERPNDC-DESC-RESV     PIC X(46)       VALUE        EL525
00285              ' PENDING CLAIMS RESERVES UPDATED  - '.              EL525
00286          10  WS-ERPNDC-COUNT-RESV    PIC S9(7)       VALUE ZERO.  EL525
00287      05  WS-TOTAL-3.                                              EL525
00288          10  WS-ERCRTC-DESC          PIC X(46)       VALUE        EL525
00289              ' CERT CHANGE RECORDS UPDATED      - '.              EL525
00290          10  WS-ERCRTC-COUNT         PIC S9(7)       VALUE ZERO.  EL525
00291      05  WS-TOTAL-4.                                              EL525
00292          10  WS-ERPYAJ-DESC          PIC X(46)       VALUE        EL525
00293              ' PAYMENT/ADJ RECORDS UPDATED      - '.              EL525
00294          10  WS-ERPYAJ-COUNT         PIC S9(7)       VALUE ZERO.  EL525
00295      05  WS-TOTAL-5.                                              EL525
00296          10  WS-ERREPY-DESC          PIC X(46)       VALUE        EL525
00297              ' RETRO/EPEC  RECORDS UPDATED      - '.              EL525
00298          10  WS-ERREPY-COUNT         PIC S9(7)       VALUE ZERO.  EL525
00299      05  WS-TOTAL-6.                                              EL525
00300          10  WS-ERRQST-DESC          PIC X(46)       VALUE        EL525
00301              ' REQUEST RECORDS UPDATED          - '.              EL525
00302          10  WS-ERRQST-COUNT         PIC S9(7)       VALUE ZERO.  EL525
00303      05  WS-TOTAL-7.                                              EL525
00304          10  WS-TOTAL-DESC           PIC X(46)       VALUE        EL525
00305              ' ** TOTAL RECORDS UPDATED **      - '.              EL525
00306          10  WS-TOTAL-COUNT          PIC S9(7)       VALUE ZERO.  EL525
00307                                                                   EL525
00308  01  WS-TOT-TABLE REDEFINES WS-TOTAL-TABLE.                       EL525
00309      05  WS-TOTALS OCCURS 12 TIMES                                EL525
00310                    INDEXED BY TOT-INDX.                           EL525
00311          10  WS-TOT-DESC             PIC X(46).                   EL525
00312          10  WS-TOT-COUNT            PIC S9(7).                   EL525
00313                                                                   EL525
00314  01  FILLER.                                                      EL525
00315      05  WS-DISPLAY-TIME             PIC 99B99B99.                EL525
00316      05  WS-DEL-BIN1                 PIC 9999-.                   EL525
00317                                                                   EL525
00318      05  X                           PIC X.                       EL525
00319      05  ABEND-CODE                  PIC X(4).                    EL525
00320      05  ABEND-OPTION                PIC X.                       EL525
00321      05  OLC-REPORT-NAME             PIC X(8) VALUE 'EL525'.      EL525
00322      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL525
00323      05  WS-LAST-COMPANY-CD          PIC X      VALUE LOW-VALUES. EL525
00324      05  WS-LAST-CARRIER             PIC X      VALUE LOW-VALUES. EL525
00325      05  WS-LAST-BATCH               PIC X(6).                    EL525
00326      05  WS-DISPLAY-AMOUNT           PIC Z,ZZZ,ZZ9.99-.           EL525
00327      05  WS-DISPLAY-COUNT            PIC Z,ZZZ,ZZ9-.              EL525
00328      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL525
00329      05  WS-PROCESSOR-ID             PIC X(4)        VALUE SPACES.EL525
00330                                                                   EL525
00331      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL525
00332      05  ELCNTL-FILE-STATUS          PIC XX          VALUE ZERO.  EL525
00333      05  ERPNDB-FILE-STATUS          PIC XX          VALUE ZERO.  EL525
00334      05  ERPNDC-FILE-STATUS          PIC XX          VALUE ZERO.  EL525
00335      05  ERCRTC-FILE-STATUS          PIC XX          VALUE ZERO.  EL525
00336      05  ERPYAJ-FILE-STATUS          PIC XX          VALUE ZERO.  EL525
00337      05  ERREPY-FILE-STATUS          PIC XX          VALUE ZERO.  EL525
00338      05  ERRQST-FILE-STATUS          PIC XX          VALUE ZERO.  EL525
00339                                                                   EL525
00340      05  WS-FILE-ERROR-MESSAGE.                                   EL525
00341          10  FILLER                  PIC X(24)       VALUE        EL525
00342              'ERROR OCCURED OPENING - '.                          EL525
00343          10  WS-FEM-FILE-NAME        PIC X(8).                    EL525
00344                                                                   EL525
00345      05  WS-BATCH-MISSING-SW         PIC XX VALUE LOW-VALUES.     EL525
00346          88  BATCH-MISSING                      VALUE HIGH-VALUES.EL525
00347                                                                   EL525
00348      05  WS-BATCH-POST-SW            PIC X      VALUE SPACE.      EL525
00349          88  ALL-RECORDS-POSTED                 VALUE SPACE.      EL525
00350                                                                   EL525
00351      05  WS-LAST-CERT-NO             PIC X(8).                    EL525
00352                                                                   EL525
00353      05  WS-LAST-TIME-WRITTEN        PIC S9(7) VALUE ZERO COMP-3. EL525
00354      05  WS-SAVE-TIME                PIC 9(6).                    EL525
00355      05  WS-POST-DATE                PIC XX      VALUE LOW-VALUES.EL525
00356      05  WS-THIS-DATE                PIC XX      VALUE LOW-VALUES.EL525
00357      05  WS-SAVE-LAST-DT             PIC XX      VALUE LOW-VALUES.EL525
00358      05  WS-SAVE-DATE                PIC XX      VALUE LOW-VALUES.EL525
00359      05  WS-WORK-DATE1               PIC 9(6).                    EL525
00360      05  WS-WORK-DATE1-X             REDEFINES                    EL525
00361          WS-WORK-DATE1.                                           EL525
00362          10  WS-WORK-MM1             PIC 99.                      EL525
00363          10  WS-WORK-DD1             PIC 99.                      EL525
00364          10  WS-WORK-YY1             PIC 99.                      EL525
00365                                                                   EL525
00366      05  WS-WORK-DATE2               PIC 9(6).                    EL525
00367      05  WS-WORK-DATE2-X             REDEFINES                    EL525
00368          WS-WORK-DATE2.                                           EL525
00369          10  WS-WORK-MM2             PIC 99.                      EL525
00370          10  WS-WORK-DD2             PIC 99.                      EL525
00371          10  WS-WORK-YY2             PIC 99.                      EL525
00372                                                                   EL525
00373      05  WS-MONTH-END-DATE           PIC XX      VALUE LOW-VALUES.EL525
00374                                                                   EL525
00375      05  NEW-PB-CERT.                                             EL525
00376          10  NEW-PB-CERT-EFF-DT      PIC XX.                      EL525
00377          10  NEW-PB-CERT-NO          PIC X(8).                    EL525
00378                                                                   EL525
00379      05  HELD-PB-CERT.                                            EL525
00380          10  HELD-PB-CERT-EFF-DT     PIC XX        VALUE SPACES.  EL525
00381          10  HELD-PB-CERT-NO         PIC X(8)      VALUE SPACES.  EL525
00382                                                                   EL525
00383      05  WS-COMPANY-ID               PIC X(3).                    EL525
00384      05  WS-COMPANY-CD               PIC X.                       EL525
00385                                                                   EL525
00386      05  WS-COMPANY-NAME.                                         EL525
00387          10  WS-CN-CHAR              PIC X                        EL525
00388              OCCURS 30 TIMES         INDEXED BY CN1.              EL525
00389                                                                   EL525
00390      05  WS-COMPANY-NAME2.                                        EL525
00391          10  WS-CN2-CHAR             PIC X                        EL525
00392              OCCURS 30 TIMES         INDEXED BY CN2.              EL525
00393      05  WS-INITIALS.                                             EL525
00394          10  WS-INITIAL1             PIC X.                       EL525
00395          10  WS-INITIAL2             PIC X.                       EL525
00396                                                                   EL525
00397      05  WS-PHONETIC-WORK-AREA.                                   EL525
00398          10  WS-PWA-PHONETIC-NAME    PIC X(4).                    EL525
00399          10  WS-PWA-NAME             PIC X(16).                   EL525
00400          10  WS-PWA-LANGUAGE         PIC X.                       EL525
00401                                                                   EL525
00402      05  WS-BIN-DATE-WORK-X.                                      EL525
00403          10  WS-BIN-DATE-WORK        PIC S9(4)                    EL525
00404                                      COMP.                        EL525
00405                                                                   EL525
00406      EJECT                                                        EL525
00407  01  WS-HEADING1.                                                 EL525
00408      05  FILLER                      PIC X(49)       VALUE '1'.   EL525
00409      05  WS-H1-TITLE                 PIC X(72)       VALUE        EL525
00410          'UPDATE INTERFACE ACCEPT TOTALS'.                        EL525
00411      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL525'.      EL525
00412                                                                   EL525
00413  01  WS-HEADING2.                                                 EL525
00414      05  FILLER                      PIC X           VALUE SPACES.EL525
00415      05  WS-H2-COMPANY-ID            PIC XXX         VALUE 'XXX'. EL525
00416      05  FILLER                      PIC X(42)       VALUE SPACES.EL525
00417      05  WS-H2-CLIENT-NAME           PIC X(30)       VALUE SPACES.EL525
00418      05  FILLER                      PIC X(38)       VALUE SPACES.EL525
00419      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL525
00420                                                                   EL525
00421  01  WS-HEADING3.                                                 EL525
00422      05  FILLER                      PIC X(54)       VALUE SPACES.EL525
00423      05  WS-H3-DATE                  PIC X(18)       VALUE SPACES.EL525
00424      05  FILLER                      PIC X(45)       VALUE SPACES.EL525
00425      05  FILLER                      PIC X(4)        VALUE 'PAGE'.EL525
00426      05  WS-H3-PAGE                  PIC ZZ,ZZ9      VALUE ZERO.  EL525
00427                                                                   EL525
00428  01  WS-HEADING4.                                                 EL525
00429      05  FILLER                      PIC X(133)      VALUE        EL525
00430          '0           FILE                           COUNTS '.    EL525
00431                                                                   EL525
00432  01  WS-DETAIL1.                                                  EL525
00433      05  FILLER                      PIC X.                       EL525
00434      05  WS-D1-DESC                  PIC X(36).                   EL525
00435      05  WS-D1-COUNT                 PIC Z,ZZZ,ZZZ,ZZ9-.          EL525
00436      05  FILLER                      PIC X(82).                   EL525
00437                                                                   EL525
00438  01  WS-DETAIL1A                     REDEFINES                    EL525
00439      WS-DETAIL1.                                                  EL525
00440      05  WS-KEY-DATA                 PIC X(133).                  EL525
00441                                                                   EL525
00442      05  WS-PB-KEY-DATA              REDEFINES                    EL525
00443          WS-KEY-DATA.                                             EL525
00444          10  FILLER                  PIC XX.                      EL525
00445          10  WS-PB-ENTRY-BATCH       PIC X(6).                    EL525
00446          10  FILLER                  PIC XX.                      EL525
00447          10  WS-PB-BATCH-SEQ-NO      PIC 9(4).                    EL525
00448          10  FILLER                  PIC XX.                      EL525
00449          10  WS-PB-BATCH-CHG-SEQ-NO  PIC 9(4).                    EL525
00450          10  FILLER                  PIC XX.                      EL525
00451          10  WS-PB-MESSAGE           PIC X(45).                   EL525
00452          10  FILLER                  PIC X(66).                   EL525
00453                                                                   EL525
00454      05  WS-PC-KEY-DATA              REDEFINES                    EL525
00455          WS-KEY-DATA.                                             EL525
00456          10  FILLER                  PIC XX.                      EL525
00457          10  WS-PC-CARRIER           PIC X.                       EL525
00458          10  FILLER                  PIC XX.                      EL525
00459          10  WS-PC-GROUPING          PIC X(6).                    EL525
00460          10  FILLER                  PIC XX.                      EL525
00461          10  WS-PC-STATE             PIC XX.                      EL525
00462          10  FILLER                  PIC XX.                      EL525
00463          10  WS-PC-ACCOUNT           PIC X(10).                   EL525
00464          10  FILLER                  PIC XX.                      EL525
00465          10  WS-PC-CERT-EFF-DT       PIC X(8).                    EL525
00466          10  FILLER                  PIC XX.                      EL525
00467          10  WS-PC-CERT-NO           PIC X(11).                   EL525
00468          10  FILLER                  PIC XX.                      EL525
00469          10  WS-PC-CLAIM-NO          PIC X(7).                    EL525
00470          10  FILLER                  PIC XX.                      EL525
00471          10  WS-PC-CHECK-NO          PIC X(7).                    EL525
00472          10  FILLER                  PIC XX.                      EL525
00473          10  WS-PC-RECORD-TYPE       PIC XX.                      EL525
00474          10  FILLER                  PIC XX.                      EL525
00475          10  WS-PC-RECORD-SEQUENCE   PIC 9(4).                    EL525
00476          10  FILLER                  PIC XX.                      EL525
00477          10  WS-PC-MESSAGE           PIC X(45).                   EL525
00478          10  FILLER                  PIC X(8).                    EL525
00479                                                                   EL525
00480      05  WS-CC-KEY-DATA              REDEFINES                    EL525
00481          WS-KEY-DATA.                                             EL525
00482          10  FILLER                  PIC XX.                      EL525
00483          10  WS-CC-CARRIER           PIC X.                       EL525
00484          10  FILLER                  PIC XX.                      EL525
00485          10  WS-CC-GROUPING          PIC X(6).                    EL525
00486          10  FILLER                  PIC XX.                      EL525
00487          10  WS-CC-STATE             PIC XX.                      EL525
00488          10  FILLER                  PIC XX.                      EL525
00489          10  WS-CC-ACCOUNT           PIC X(10).                   EL525
00490          10  FILLER                  PIC XX.                      EL525
00491          10  WS-CC-CERT-EFF-DT       PIC X(8).                    EL525
00492          10  FILLER                  PIC XX.                      EL525
00493          10  WS-CC-CERT-NO           PIC X(11).                   EL525
00494          10  FILLER                  PIC XX.                      EL525
00495          10  WS-CC-FILE-SEQ-NO       PIC 9(8).                    EL525
00496          10  FILLER                  PIC XX.                      EL525
00497          10  WS-CC-MESSAGE           PIC X(45).                   EL525
00498          10  FILLER                  PIC X(26).                   EL525
00499                                                                   EL525
00500      05  WS-PY-KEY-DATA              REDEFINES                    EL525
00501          WS-KEY-DATA.                                             EL525
00502          10  FILLER                  PIC XX.                      EL525
00503          10  WS-PY-CARRIER           PIC X.                       EL525
00504          10  FILLER                  PIC XX.                      EL525
00505          10  WS-PY-GROUPING          PIC X(6).                    EL525
00506          10  FILLER                  PIC XX.                      EL525
00507          10  WS-PY-FIN-RESP          PIC X(6).                    EL525
00508          10  FILLER                  PIC XX.                      EL525
00509          10  WS-PY-ACCOUNT           PIC X(10).                   EL525
00510          10  FILLER                  PIC XX.                      EL525
00511          10  WS-PY-FILE-SEQ-NO       PIC 9(8).                    EL525
00512          10  FILLER                  PIC XX.                      EL525
00513          10  WS-PY-RECORD-TYPE       PIC X.                       EL525
00514          10  FILLER                  PIC XX.                      EL525
00515          10  WS-PY-MESSAGE           PIC X(45).                   EL525
00516          10  FILLER                  PIC X(42).                   EL525
00517                                                                   EL525
00518      05  WS-RP-KEY-DATA              REDEFINES                    EL525
00519          WS-KEY-DATA.                                             EL525
00520          10  FILLER                  PIC XX.                      EL525
00521          10  WS-RP-CARRIER           PIC X.                       EL525
00522          10  FILLER                  PIC XX.                      EL525
00523          10  WS-RP-GROUPING          PIC X(6).                    EL525
00524          10  FILLER                  PIC XX.                      EL525
00525          10  WS-RP-STATE             PIC XX.                      EL525
00526          10  FILLER                  PIC XX.                      EL525
00527          10  WS-RP-ACCOUNT           PIC X(10).                   EL525
00528          10  FILLER                  PIC XX.                      EL525
00529          10  WS-RP-FILE-SEQ-NO       PIC 9(8).                    EL525
00530          10  FILLER                  PIC XX.                      EL525
00531          10  WS-RP-RECORD-TYPE       PIC X.                       EL525
00532          10  FILLER                  PIC XX.                      EL525
00533          10  WS-RP-MESSAGE           PIC X(45).                   EL525
00534          10  FILLER                  PIC X(46).                   EL525
00535                                                                   EL525
00536      EJECT                                                        EL525
00537                                      COPY ELCDATE.                   CL**2
00538                                                                   EL525
00539                                                                   EL525
00540                                      COPY ELCDTECX SUPPRESS.      EL525
00541                                                                   EL525
00542                                      COPY ELCDTEVR SUPPRESS.      EL525
00543                                                                   EL525
00544      EJECT                                                        EL525
00545  PROCEDURE DIVISION.                                              EL525
00546                                                                   EL525
00547  CAPTURE-START.                                                   EL525
00548                                                                   EL525
00549      OPEN I-O ERMEBL.                                             EL525
00550      IF ERMEBL-FILE-STATUS NOT = ZERO                             EL525
00551        AND ERMEBL-FILE-STATUS NOT = '97'                          EL525
00552          MOVE 'N' TO ME-UPDATE-FLAG.                              EL525
00553                                                                   EL525
00554  0000-LOAD-DATE-CARD.        COPY ELCDTERX SUPPRESS.              EL525
00555                                                                   EL525
00556      MOVE WS-TIME                TO ME-START-TIME.                EL525
00557      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                EL525
00558      MOVE ME-START-MO            TO ME-CNDS-MO.                   EL525
00559      MOVE ME-START-DA            TO ME-CNDS-DA.                   EL525
00560      MOVE ME-START-YR            TO ME-CNDS-YR.                   EL525
00561      MOVE DTE-CLIENT             TO ME-COMPANY.                   EL525
00562      COMPUTE MONTH-END-MOYR = RUN-CCYY * 12 + RUN-MO.             EL525
00563      MOVE MONTH-END-MOYR         TO ME-MOYR.                      EL525
00564      IF ME-DO-UPDATE                                              EL525
00565          READ ERMEBL INVALID KEY                                  EL525
00566          MOVE 'N' TO ME-UPDATE-FLAG                               EL525
00567          CLOSE ERMEBL.                                            EL525

062121     evaluate dte-client
062121        when 'CID'
062121           MOVE 'CILGM50'        TO  WS-ME50-BAL-JOB
062121        when 'AHL'
062121           MOVE 'AHLGM50'        TO  WS-ME50-BAL-JOB
062121        when 'FNL'
062121           MOVE 'FLLGM50'        TO  WS-ME50-BAL-JOB
062121        when other
062121           MOVE 'CIDCLGM50'      TO  WS-ME50-BAL-JOB
062121     end-evaluate

           .
00569  0000-MAIN-LOGIC SECTION.                                         EL525
00570      PERFORM OPEN-FILES.                                          EL525
00571                                                                   EL525
00572      PERFORM GET-DATE.                                            EL525
00573                                                                   EL525
00574      PERFORM 1000-PROCESS-EXTRACT.                                EL525
00575                                                                   EL525
00576      PERFORM CLOSE-FILES.                                         EL525
00577                                                                   EL525
00578      IF ME-DO-UPDATE                                              EL525
00579          MOVE 1                  TO ME-525-FLAG                   EL525
00580          MOVE ME-START-TIME      TO ME-525-START                  EL525
00581          MOVE ME-CNDS-DATE       TO ME-525-RUN-DT                 EL525
00582          ACCEPT WS-TIME-OF-DAY   FROM  TIME                       EL525
00583          MOVE WS-TIME            TO ME-525-END                    EL525
00584          ADD  1                  TO ME-525-RUN-CT                 EL525
00585          REWRITE MONTH-END-BALANCES                               EL525
00586          CLOSE ERMEBL.                                            EL525
00587                                                                   EL525
00588      MOVE ZEROS  TO RETURN-CODE.
00588      GOBACK.                                                      EL525
00589                                                                   EL525
00590      EJECT                                                        EL525
00591                                                                   EL525
00592  1000-PROCESS-EXTRACT SECTION.                                    EL525
00593      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL525
00594      MOVE DTE-CLIENT             TO  CF-COMPANY-ID                EL525
00595                                      WS-PROCESSOR-ID.             EL525
00596      MOVE '1'                    TO  CF-RECORD-TYPE.              EL525
00597      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           EL525
00598      MOVE +0                     TO  CF-SEQUENCE-NO.              EL525
00599                                                                   EL525
00600      READ ELCNTL.                                                 EL525
00601                                                                   EL525
00602      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL525
00603          MOVE 'ERROR OCCURED READ INITIAL - ELCNTL'               EL525
00604                                  TO  WS-ABEND-MESSAGE             EL525
00605          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
00606          GO TO ABEND-PGM.                                         EL525
00607                                                                   EL525
00608      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME              EL525
00609                                      WS-H2-CLIENT-NAME.           EL525
00610      MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID                EL525
00611                                      WS-H2-COMPANY-ID.            EL525
00612      MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.               EL525
00613      MOVE LOW-VALUES             TO  WS-LAST-CARRIER.             EL525
00614                                                                   EL525
00615      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL525
00616                                                                   EL525
00617      MOVE WS-TIME                TO  WS-DISPLAY-TIME.             EL525
00618      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL525
00619                                                                   EL525
00620      DISPLAY 'BEGIN PROCESSING ' WS-H2-CLIENT-NAME  ' AT '        EL525
00621              WS-DISPLAY-TIME UPON CONSOLE.                        EL525
00622                                                                   EL525
00623      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL525
00624      SET CN1 TO +30.                                              EL525
00625                                                                   EL525
00626      IF CF-AR-SYSTEM-USED                                         EL525
00627          OPEN I-O ERRQST                                          EL525
00628          IF ERRQST-FILE-STATUS  = '00' OR '97'                    EL525
00629              NEXT SENTENCE                                        EL525
00630            ELSE                                                   EL525
00631              MOVE 'ERRQST'              TO  WS-FEM-FILE-NAME      EL525
00632              MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE      EL525
00633              MOVE ERRQST-FILE-STATUS    TO  WS-ABEND-FILE-STATUS  EL525
00634              GO TO ABEND-PGM.                                     EL525
00635                                                                   EL525
00636  1020-PES.                                                        EL525
00637      IF  WS-CN-CHAR (CN1) = SPACES                                EL525
00638          IF CN1 GREATER THAN +1                                   EL525
00639              SET CN1 DOWN BY +1                                   EL525
00640              GO TO 1020-PES                                       EL525
00641          ELSE                                                     EL525
00642              GO TO 1100-READ-EXTRACT.                             EL525
00643                                                                   EL525
00644      SET WS-LENGTH TO CN1.                                        EL525
00645                                                                   EL525
00646      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL525
00647      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL525
00648                                                                   EL525
00649      IF WS-LENGTH NOT GREATER THAN ZERO                           EL525
00650          GO TO 1100-READ-EXTRACT.                                 EL525
00651                                                                   EL525
00652      SET CN2 TO CN1.                                              EL525
00653      SET CN2 UP BY WS-LENGTH.                                     EL525
00654                                                                   EL525
00655  1030-PES.                                                        EL525
00656      MOVE WS-CN-CHAR (CN1) TO WS-CN2-CHAR (CN2).                  EL525
00657                                                                   EL525
00658      IF CN1 GREATER THAN +1                                       EL525
00659          SET CN1                                                  EL525
00660              CN2 DOWN BY +1                                       EL525
00661          GO TO 1030-PES.                                          EL525
00662                                                                   EL525
00663      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL525
00664      EJECT                                                        EL525
00665  1100-READ-EXTRACT.                                               EL525
00666 *    NOTE ******************************************************* EL525
00667 *         *      POSITION THE EXTRACT INTERFACE FILE AT THE     * EL525
00668 *         *  BEGINNING OF THE COMPANY TO UPDATE THE CREDIT-     * EL525
00669 *         *  ACCEPT DATES ON THE ERPNDB, ERPNDC, ERPYAJ FILES   * EL525
00670 *         *******************************************************.EL525
00671                                                                   EL525
00672      READ EXTRACT-INTERFACE-FILE                                  EL525
00673          AT END                                                   EL525
00674              CLOSE EXTRACT-INTERFACE-FILE                         EL525
00675              GO TO 1900-FINAL-TOTS.                               EL525
00676                                                                   EL525
00677      IF EX-COMPANY-CD LESS WS-COMPANY-CD                          EL525
00678          GO TO 1100-READ-EXTRACT                                  EL525
00679      ELSE                                                         EL525
00680          IF EX-COMPANY-CD GREATER WS-COMPANY-CD                   EL525
00681              GO TO 1900-FINAL-TOTS.                               EL525
00682                                                                   EL525
00683      IF EX-EXTRACT-CODE NOT = 'A'                                 EL525
00684          GO TO 1900-FINAL-TOTS.                                   EL525
00685                                                                   EL525
00686      IF EX-RECORD-TYPE = 'A'                                      EL525
00687          PERFORM 4000-PROCESS-ERPNDB                              EL525
00688      ELSE                                                         EL525
00689          IF EX-RECORD-TYPE = 'B'                                  EL525
00690              IF CO-HAS-CLAS-IC-CLAIM                              EL525
00691                  NEXT SENTENCE                                    EL525
00692              ELSE                                                 EL525
00693                  PERFORM 4200-PROCESS-ERPNDC                      EL525
00694          ELSE                                                     EL525
00695              IF EX-RECORD-TYPE = 'C'                              EL525
00696                  PERFORM 4300-PROCESS-ERCRTC                      EL525
00697              ELSE                                                 EL525
00698                  IF EX-RECORD-TYPE = 'D'                          EL525
00699                      PERFORM 4400-PROCESS-ERPYAJ                  EL525
00700                  ELSE                                             EL525
00701                      IF EX-RECORD-TYPE = 'E'                      EL525
00702                          PERFORM 4500-PROCESS-ERREPY.             EL525
00703                                                                   EL525
00704      GO TO 1100-READ-EXTRACT.                                     EL525
00705                                                                   EL525
00706  1900-FINAL-TOTS.                                                 EL525
00707      MOVE SPACES                 TO  WS-H2-COMPANY-ID.            EL525
00708      MOVE '       FINAL TOTALS'  TO  WS-H2-CLIENT-NAME.           EL525
00709      ADD WS-LINE-COUNT-MAX       TO  WS-LINE-COUNT.               EL525
00710      SET TOT-INDX TO +1.                                          EL525
00711      SET TOT-INDX DOWN BY 1.                                      EL525
00712                                                                   EL525
00713  1920-DETAIL.                                                     EL525
00714      SET TOT-INDX UP BY +1.                                       EL525
00715                                                                   EL525
00716      IF TOT-INDX GREATER +12                                      EL525
00717          GO TO 1999-EXIT.                                         EL525
00718                                                                   EL525
00719      MOVE SPACES                 TO  WS-DETAIL1.                  EL525
00720                                                                   EL525
00721      MOVE WS-TOT-DESC (TOT-INDX) TO  WS-D1-DESC.                  EL525
00722      MOVE WS-TOT-COUNT (TOT-INDX) TO WS-D1-COUNT.                 EL525
00723                                                                   EL525
00724      MOVE WS-DETAIL1             TO  PRT.                         EL525
00725      PERFORM WRITE-A-LINE                                         EL525

062104     IF TOT-INDX = +1
062104         MOVE WS-TOT-COUNT (1)     TO WS-ME50-BAL-AMT-LOW
062104         MOVE WS-TOT-COUNT (1)     TO WS-ME50-BAL-AMT-HIGH
062104         MOVE WS-BAL50-DESCRIPTION TO WS-ME50-BAL-DESCRIP
062104         WRITE ME50-EL525-BALANCE-REC FROM WS-ME50-BALANCE-REC
062104     END-IF.

00727      GO TO 1920-DETAIL.                                           EL525
00728                                                                   EL525
00729  1999-EXIT.                                                       EL525
00730      EXIT.                                                        EL525
00731      EJECT                                                        EL525
00732                                                                   EL525
00733  4000-PROCESS-ERPNDB SECTION.                                     EL525
00734      MOVE EX-DATA-AREAS   TO  PENDING-BUSINESS.                   EL525
00735                                                                   EL525
00736      IF  PB-ENTRY-BATCH NOT = WS-LAST-BATCH                       EL525
00737          MOVE PB-ENTRY-BATCH     TO WS-LAST-BATCH                 EL525
00738          PERFORM 7000-SEARCH-ERPNDB.                              EL525
00739                                                                   EL525
00740      IF  PB-ALT-CHG-SEQ-NO NOT = ZEROS                            EL525
00741          IF  PB-CERT-NO = WS-LAST-CERT-NO                         EL525
00742              GO TO 4150-READ-ERPNDB                               EL525
00743          ELSE                                                     EL525
00744              GO TO 4000-EXIT.                                     EL525
00745                                                                   EL525
00746      MOVE PB-CERT-EFF-DT  TO  NEW-PB-CERT-EFF-DT.                 EL525
00747      MOVE PB-CERT-NO      TO  NEW-PB-CERT-NO.                     EL525
00748                                                                   EL525
00749      IF NEW-PB-CERT NOT = HELD-PB-CERT                            EL525
00750          MOVE SPACES  TO  HELD-PB-CERT.                           EL525
00751                                                                   EL525
00752      IF PB-ISSUE                                                  EL525
00753          IF PB-RECORD-ON-HOLD                                     EL525
00754              MOVE NEW-PB-CERT  TO  HELD-PB-CERT                   EL525
00755          ELSE                                                     EL525
00756              MOVE SPACES       TO  HELD-PB-CERT.                  EL525
00757                                                                   EL525
00758      IF  PB-RECORD-RETURNED                                       EL525
00759          NEXT SENTENCE                                            EL525
00760      ELSE                                                         EL525
00761         IF  PB-RECORD-ON-HOLD OR                                  EL525
00762             PB-FATAL-ERRORS   OR                                  EL525
00763             PB-UNFORCED-ERRORS                                    EL525
00764             GO TO 4000-EXIT.                                      EL525
00765                                                                   EL525
00766      IF  HELD-PB-CERT NOT = SPACES                                EL525
00767          IF  HELD-PB-CERT = NEW-PB-CERT                           EL525
00768              GO TO 4000-EXIT.                                     EL525
00769                                                                   EL525
00770      IF BATCH-MISSING                                             EL525
00771          GO TO 4000-EXIT.                                         EL525
00772                                                                   EL525
00773  4150-READ-ERPNDB.                                                EL525
00774      READ ERPNDB.                                                 EL525
00775                                                                   EL525
00776      IF ERPNDB-FILE-STATUS = '23'                                 EL525
00777          MOVE SPACES             TO  WS-DETAIL1                   EL525
00778          MOVE PB-ENTRY-BATCH     TO  WS-PB-ENTRY-BATCH            EL525
00779          MOVE PB-BATCH-SEQ-NO    TO  WS-PB-BATCH-SEQ-NO           EL525
00780          MOVE PB-BATCH-CHG-SEQ-NO TO WS-PB-BATCH-CHG-SEQ-NO       EL525
00781          MOVE 'PENDING BUSINESS RECORD MISSING'                   EL525
00782                                  TO  WS-PB-MESSAGE                EL525
00783          MOVE WS-DETAIL1A        TO  PRT                          EL525
00784          PERFORM WRITE-A-LINE                                     EL525
00785          GO TO 4000-EXIT.                                         EL525
00786                                                                   EL525
00787      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL525
00788          MOVE 'ERROR OCCURED READ - ERPNDB'                       EL525
00789                                  TO  WS-ABEND-MESSAGE             EL525
00790          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
00791          GO TO ABEND-PGM.                                         EL525
00792                                                                   EL525
00793      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL525
00794          MOVE PB-CERT-NO         TO  WS-LAST-CERT-NO              EL525
00795          GO TO 4000-EXIT.                                         EL525
00796                                                                   EL525
00797      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                             EL525
00798          GO TO 4160-CONTINUE.                                     EL525
00799                                                                   EL525
00800      IF PB-ISSUE                                                  EL525
00801          SUBTRACT PB-I-LF-PREMIUM-AMT FROM WS-LF-ISS-PRM-REMITTED EL525
00802          SUBTRACT PB-I-LF-PREMIUM-AMT FROM WS-LF-ISS-PRM-ENTERED  EL525
00803          SUBTRACT PB-I-LF-PREM-CALC   FROM WS-LF-ISS-PRM-COMPUTED EL525
00804          SUBTRACT PB-I-AH-PREMIUM-AMT FROM WS-AH-ISS-PRM-REMITTED EL525
00805          SUBTRACT PB-I-AH-PREMIUM-AMT FROM WS-AH-ISS-PRM-ENTERED  EL525
00806          SUBTRACT PB-I-AH-PREM-CALC   FROM WS-AH-ISS-PRM-COMPUTED EL525
00807          SUBTRACT +1                  FROM WS-ISSUE-CNT-REMITTED  EL525
00808          SUBTRACT +1                  FROM WS-ISSUE-CNT-ENTERED.  EL525
00809                                                                   EL525
00810      IF PB-CANCELLATION                                           EL525
00811          SUBTRACT PB-C-LF-CANCEL-AMT  FROM WS-LF-CAN-PRM-REMITTED EL525
00812          SUBTRACT PB-C-LF-CANCEL-AMT  FROM WS-LF-CAN-PRM-ENTERED  EL525
00813          SUBTRACT PB-C-LF-REF-CALC    FROM WS-LF-CAN-PRM-COMPUTED EL525
00814          SUBTRACT PB-C-AH-CANCEL-AMT  FROM WS-AH-CAN-PRM-REMITTED EL525
00815          SUBTRACT PB-C-AH-CANCEL-AMT  FROM WS-AH-CAN-PRM-ENTERED  EL525
00816          SUBTRACT PB-C-AH-REF-CALC    FROM WS-AH-CAN-PRM-COMPUTED EL525
00817          SUBTRACT +1                  FROM WS-CANCEL-CNT-REMITTED EL525
00818          SUBTRACT +1                  FROM WS-CANCEL-CNT-ENTERED. EL525
00819                                                                   EL525
00820  4160-CONTINUE.                                                   EL525
00821      IF  PB-BATCH-TRAILER                                         EL525
00822          MOVE WS-BATCH-RECORD    TO  PB-BATCH-RECORD              EL525
00823              PERFORM 5000-BROWSE-BATCH-FOR-POST                   EL525
00824      ELSE                                                         EL525
00825          MOVE WS-POST-DATE       TO  PB-CREDIT-ACCEPT-DT.         EL525
00826                                                                   EL525
00827      MOVE WS-SAVE-DATE           TO  PB-LAST-MAINT-DT.            EL525
00828      MOVE WS-SAVE-TIME           TO  PB-LAST-MAINT-HHMMSS.        EL525
00829      MOVE WS-PROCESSOR-ID        TO  PB-LAST-MAINT-BY.            EL525
00830                                                                   EL525
00831      MOVE 'C'                    TO  SLR-ACTION.                  EL525
00832      MOVE 'EL'                   TO  SLR-PREFIX.                  EL525
00833      MOVE ZERO                   TO  SLR-TASK-NUMBER.             EL525
00834      MOVE SPACES                 TO  SLR-TERM-ID.                 EL525
00835      MOVE 'E525'                 TO  SLR-TRAN-ID.                 EL525
00836                                                                   EL525
00837      MOVE 'ERPNDB'               TO  SLR-DSID.                    EL525
00838                                                                   EL525
00839      MOVE SPACES                 TO DC-ALPHA-CENTURY.             EL525
00840      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL525
00841      MOVE '2'                    TO  DC-OPTION-CODE.              EL525
00842      PERFORM 8500-DATE-CONVERSION.                                EL525
00843      MOVE DC-JULIAN-DATE-1       TO  SLR-DATE-WRITTEN.            EL525
00844                                                                   EL525
00845                                                                   EL525
00846      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL525
00847                                                                   EL525
00848      MOVE WS-TIME                TO  SLR-TIME-WRITTEN.            EL525
00849      MULTIPLY +10 BY SLR-TIME-WRITTEN.                            EL525
00850                                                                   EL525
00851      MOVE +11                    TO  SLR-KEY-LENGTH.              EL525
00852      MOVE SLR-KEY-LENGTH         TO  SLR-KEY-LENGTH.              EL525
00853      MOVE SPACES                 TO  SLR-KEY.                     EL525
00854                                                                   EL525
00855      MOVE +575                   TO  SLR-RECORD-LENGTH.           EL525
00856      MOVE SLR-RECORD-LENGTH      TO  SLR-RECORD-LENGTH.           EL525
00857      MOVE SPACES                 TO  SLR-RECORD-IMAGE.            EL525
00858                                                                   EL525
00859      MOVE PB-CONTROL-PRIMARY     TO  SLR-KEY.                     EL525
00860      MOVE PENDING-BUSINESS       TO  SLR-RECORD-IMAGE.            EL525
00861                                                                   EL525
00862      REWRITE PENDING-BUSINESS.                                    EL525
00863                                                                   EL525
00864      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL525
00865          MOVE 'ERROR OCCURED REWRITE - ERPNDB'                    EL525
00866                                  TO  WS-ABEND-MESSAGE             EL525
00867          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
00868          GO TO ABEND-PGM.                                         EL525
00869                                                                   EL525
00870      IF SLR-DATE-WRITTEN GREATER THAN WS-DATE-WRITTEN             EL525
00871          MOVE SLR-DATE-WRITTEN   TO  WS-DATE-WRITTEN              EL525
00872          MOVE ZERO               TO  WS-TIME-WRITTEN.             EL525
00873                                                                   EL525
00874  4000-CHECK-TIME-STAMP.                                           EL525
00875      IF SLR-TIME-WRITTEN NOT GREATER THAN WS-TIME-WRITTEN         EL525
00876          ADD +1  TO  SLR-TIME-WRITTEN                             EL525
00877          GO TO 4000-CHECK-TIME-STAMP.                             EL525
00878                                                                   EL525
00879      MOVE SLR-TIME-WRITTEN       TO  WS-TIME-WRITTEN.             EL525
00880                                                                   EL525
00881      WRITE SYSTEM-LOG-RECORD.                                     EL525
00882                                                                   EL525
00883      MOVE PB-CERT-NO             TO  WS-LAST-CERT-NO.             EL525
00884      ADD +1                      TO WS-ERPNDB-COUNT               EL525
00885                                     WS-TOTAL-COUNT.               EL525
00886                                                                   EL525
00887      IF PB-ISSUE                                                  EL525
00888          ADD +1  TO  WS-ERPNDB-COUNT-ISS.                         EL525
00889                                                                   EL525
00890      IF PB-CANCELLATION                                           EL525
00891          ADD +1  TO  WS-ERPNDB-COUNT-CAN.                         EL525
00892                                                                   EL525
00893      IF PB-BATCH-TRAILER                                          EL525
00894          ADD +1  TO  WS-ERPNDB-COUNT-BATCH.                       EL525
00895                                                                   EL525
00896  4000-EXIT.                                                       EL525
00897      EXIT.                                                        EL525
00898      EJECT                                                        EL525
00899                                                                   EL525
00900  4200-PROCESS-ERPNDC SECTION.                                     EL525
00901      MOVE EX-DATA-AREAS   TO  PENDING-CLAIMS.                     EL525
00902                                                                   EL525
00903      MOVE PC-CERT-EFF-DT  TO  NEW-PB-CERT-EFF-DT.                 EL525
00904      MOVE PC-CERT-NO      TO  NEW-PB-CERT-NO.                     EL525
00905                                                                   EL525
00906      IF NEW-PB-CERT NOT = HELD-PB-CERT                            EL525
00907          MOVE SPACES  TO  HELD-PB-CERT.                           EL525
00908                                                                   EL525
00909      IF PC-FATAL-ERRORS  OR                                       EL525
00910         PC-UNFORCED-ERRORS                                        EL525
00911          GO TO 4200-EXIT.                                         EL525
00912                                                                   EL525
00913      IF HELD-PB-CERT NOT = SPACES                                 EL525
00914          IF HELD-PB-CERT = NEW-PB-CERT                            EL525
00915              GO TO 4200-EXIT.                                     EL525
00916                                                                   EL525
00917      IF PC-PAYMENT-DT GREATER WS-POST-DATE                        EL525
00918          GO TO 4200-EXIT.                                         EL525
00919                                                                   EL525
00920      READ ERPNDC.                                                 EL525
00921                                                                   EL525
00922      IF ERPNDC-FILE-STATUS = '23'                                 EL525
00923          MOVE SPACES             TO  WS-DETAIL1                   EL525
00924          MOVE PC-CARRIER         TO  WS-PC-CARRIER                EL525
00925          MOVE PC-GROUPING        TO  WS-PC-GROUPING               EL525
00926          MOVE PC-STATE           TO  WS-PC-STATE                  EL525
00927          MOVE PC-ACCOUNT         TO  WS-PC-ACCOUNT                EL525
00928          MOVE PC-CERT-EFF-DT     TO  DC-BIN-DATE-1                EL525
00929          MOVE SPACE              TO  DC-OPTION-CODE               EL525
00930          PERFORM 8500-DATE-CONVERSION                             EL525
00931          MOVE DC-GREG-DATE-1-MDY TO  WS-PC-CERT-EFF-DT            EL525
00932          MOVE PC-CERT-NO         TO  WS-PC-CERT-NO                EL525
00933          MOVE PC-CLAIM-NO        TO  WS-PC-CLAIM-NO               EL525
00934          MOVE PC-CHECK-NO        TO  WS-PC-CHECK-NO               EL525
00935          MOVE PC-RECORD-TYPE     TO  WS-PC-RECORD-TYPE            EL525
00936          MOVE PC-RECORD-SEQUENCE TO  WS-PC-RECORD-SEQUENCE        EL525
00937          MOVE 'PENDING CLAIMS RECORD MISSING'                     EL525
00938                                  TO  WS-PC-MESSAGE                EL525
00939          MOVE WS-DETAIL1A        TO  PRT                          EL525
00940          PERFORM WRITE-A-LINE                                     EL525
00941          GO TO 4200-EXIT.                                         EL525
00942                                                                   EL525
00943      IF ERPNDC-FILE-STATUS NOT = ZERO                             EL525
00944          MOVE 'ERROR OCCURED READ - ERPNDC'                       EL525
00945                                  TO  WS-ABEND-MESSAGE             EL525
00946          MOVE ERPNDC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
00947          GO TO ABEND-PGM.                                         EL525
00948                                                                   EL525
00949      MOVE WS-POST-DATE           TO  PC-CREDIT-ACCEPT-DT.         EL525
00950      MOVE WS-SAVE-DATE           TO  PC-LAST-MAINT-DT.            EL525
00951      MOVE WS-SAVE-TIME           TO  PC-LAST-MAINT-HHMMSS.        EL525
00952      MOVE WS-PROCESSOR-ID        TO  PC-LAST-MAINT-BY.            EL525
00953                                                                   EL525
00954      MOVE 'C'                    TO  SLR-ACTION.                  EL525
00955      MOVE 'EL'                   TO  SLR-PREFIX.                  EL525
00956      MOVE ZERO                   TO  SLR-TASK-NUMBER.             EL525
00957      MOVE SPACES                 TO  SLR-TERM-ID.                 EL525
00958      MOVE 'E525'                 TO  SLR-TRAN-ID.                 EL525
00959      MOVE 'ERPNDC'               TO  SLR-DSID.                    EL525
00960      MOVE SPACES                 TO DC-ALPHA-CENTURY.             EL525
00961      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL525
00962      MOVE '2'                    TO  DC-OPTION-CODE.              EL525
00963      PERFORM  8500-DATE-CONVERSION.                               EL525
00964      MOVE DC-JULIAN-DATE-1       TO  SLR-DATE-WRITTEN.            EL525
00965                                                                   EL525
00966                                                                   EL525
00967      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL525
00968                                                                   EL525
00969      MOVE WS-TIME                TO  SLR-TIME-WRITTEN.            EL525
00970      MULTIPLY +10 BY SLR-TIME-WRITTEN.                            EL525
00971      MOVE +50                    TO  SLR-KEY-LENGTH.              EL525
00972      MOVE SLR-KEY-LENGTH         TO  SLR-KEY-LENGTH.              EL525
00973      MOVE SPACES                 TO  SLR-KEY.                     EL525
00974      MOVE +500                   TO  SLR-RECORD-LENGTH.           EL525
00975      MOVE SLR-RECORD-LENGTH      TO  SLR-RECORD-LENGTH.           EL525
00976      MOVE SPACES                 TO  SLR-RECORD-IMAGE.            EL525
00977      MOVE PC-CONTROL-PRIMARY     TO  SLR-KEY.                     EL525
00978      MOVE PENDING-CLAIMS         TO  SLR-RECORD-IMAGE.            EL525
00979                                                                   EL525
00980      REWRITE PENDING-CLAIMS.                                      EL525
00981                                                                   EL525
00982      IF ERPNDC-FILE-STATUS NOT = ZERO                             EL525
00983          MOVE 'ERROR OCCURED REWRITE - ERPNDC'                    EL525
00984                                  TO  WS-ABEND-MESSAGE             EL525
00985          MOVE ERPNDC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
00986          GO TO ABEND-PGM.                                         EL525
00987                                                                   EL525
00988      IF SLR-DATE-WRITTEN GREATER WS-DATE-WRITTEN                  EL525
00989          MOVE SLR-DATE-WRITTEN   TO  WS-DATE-WRITTEN              EL525
00990          MOVE ZERO               TO  WS-TIME-WRITTEN.             EL525
00991                                                                   EL525
00992  4200-CHECK-TIME-STAMP.                                           EL525
00993      IF SLR-TIME-WRITTEN NOT GREATER WS-TIME-WRITTEN              EL525
00994          ADD +1  TO  SLR-TIME-WRITTEN                             EL525
00995          GO TO 4200-CHECK-TIME-STAMP.                             EL525
00996                                                                   EL525
00997      MOVE SLR-TIME-WRITTEN       TO  WS-TIME-WRITTEN.             EL525
00998                                                                   EL525
00999      WRITE SYSTEM-LOG-RECORD.                                     EL525
01000                                                                   EL525
01001      ADD +1                      TO WS-ERPNDC-COUNT               EL525
01002                                     WS-TOTAL-COUNT.               EL525
01003                                                                   EL525
01004      IF PC-CLAIMS                                                 EL525
01005          ADD +1  TO WS-ERPNDC-COUNT-PMTS.                         EL525
01006                                                                   EL525
01007      IF PC-RESERVES                                               EL525
01008          ADD +1  TO WS-ERPNDC-COUNT-RESV.                         EL525
01009                                                                   EL525
01010  4200-EXIT.                                                       EL525
01011      EXIT.                                                        EL525
01012      EJECT                                                        EL525
01013                                                                   EL525
01014  4300-PROCESS-ERCRTC SECTION.                                     EL525
01015      MOVE EX-DATA-AREAS   TO  PENDING-MAINT-TO-CERT-FILE.         EL525
01016                                                                   EL525
01017      READ ERCRTC.                                                 EL525
01018                                                                   EL525
01019      IF ERCRTC-FILE-STATUS = '23'                                 EL525
01020          MOVE SPACES             TO  WS-DETAIL1                   EL525
01021          MOVE CC-CARRIER         TO  WS-CC-CARRIER                EL525
01022          MOVE CC-GROUPING        TO  WS-CC-GROUPING               EL525
01023          MOVE CC-STATE           TO  WS-CC-STATE                  EL525
01024          MOVE CC-ACCOUNT         TO  WS-CC-ACCOUNT                EL525
01025          MOVE CC-CERT-EFF-DT     TO  DC-BIN-DATE-1                EL525
01026          MOVE SPACE              TO  DC-OPTION-CODE               EL525
01027          PERFORM 8500-DATE-CONVERSION                             EL525
01028          MOVE DC-GREG-DATE-1-MDY TO  WS-CC-CERT-EFF-DT            EL525
01029          MOVE CC-CERT-NO         TO  WS-CC-CERT-NO                EL525
01030          MOVE CC-FILE-SEQ-NO     TO  WS-CC-FILE-SEQ-NO            EL525
01031          MOVE 'CERT CHANGE RECORD MISSING'                        EL525
01032                                  TO  WS-CC-MESSAGE                EL525
01033          MOVE WS-DETAIL1A        TO  PRT                          EL525
01034          PERFORM WRITE-A-LINE                                     EL525
01035          GO TO 4300-EXIT.                                         EL525
01036                                                                   EL525
01037      IF ERCRTC-FILE-STATUS NOT = ZERO                             EL525
01038          MOVE 'ERROR OCCURED READ - ERCRTC'                       EL525
01039                                  TO  WS-ABEND-MESSAGE             EL525
01040          MOVE ERCRTC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
01041          GO TO ABEND-PGM.                                         EL525
01042                                                                   EL525
01043      MOVE WS-POST-DATE           TO  CC-CREDIT-ACCEPT-DT.         EL525
01044      MOVE WS-SAVE-DATE           TO  CC-LAST-MAINT-DT.            EL525
01045      MOVE WS-SAVE-TIME           TO  CC-LAST-MAINT-HHMMSS.        EL525
01046      MOVE WS-PROCESSOR-ID        TO  CC-LAST-MAINT-BY.            EL525
01047                                                                   EL525
01048      MOVE 'C'                    TO  SLR-ACTION.                  EL525
01049      MOVE 'EL'                   TO  SLR-PREFIX.                  EL525
01050      MOVE ZERO                   TO  SLR-TASK-NUMBER.             EL525
01051      MOVE SPACES                 TO  SLR-TERM-ID.                 EL525
01052      MOVE 'E525'                 TO  SLR-TRAN-ID.                 EL525
01053                                                                   EL525
01054      MOVE 'ERCRTC'               TO  SLR-DSID.                    EL525
01055                                                                   EL525
01056      MOVE SPACES                 TO  DC-ALPHA-CENTURY.            EL525
01057      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL525
01058      MOVE '2'                    TO  DC-OPTION-CODE.              EL525
01059      PERFORM 8500-DATE-CONVERSION.                                EL525
01060      MOVE DC-JULIAN-DATE-1       TO  SLR-DATE-WRITTEN.            EL525
01061                                                                   EL525
01062                                                                   EL525
01063      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL525
01064                                                                   EL525
01065      MOVE WS-TIME                TO  SLR-TIME-WRITTEN.            EL525
01066      MULTIPLY +10 BY SLR-TIME-WRITTEN.                            EL525
01067                                                                   EL525
01068      MOVE +37                    TO  SLR-KEY-LENGTH.              EL525
01069      MOVE SLR-KEY-LENGTH         TO  SLR-KEY-LENGTH.              EL525
01070      MOVE SPACES                 TO  SLR-KEY.                     EL525
01071                                                                   EL525
01072      MOVE +200                   TO  SLR-RECORD-LENGTH.           EL525
01073      MOVE SLR-RECORD-LENGTH      TO  SLR-RECORD-LENGTH.           EL525
01074      MOVE SPACES                 TO  SLR-RECORD-IMAGE.            EL525
01075                                                                   EL525
01076      MOVE CC-CONTROL-PRIMARY     TO  SLR-KEY.                     EL525
01077      MOVE PENDING-MAINT-TO-CERT-FILE  TO  SLR-RECORD-IMAGE.       EL525
01078                                                                   EL525
01079      REWRITE PENDING-MAINT-TO-CERT-FILE.                          EL525
01080                                                                   EL525
01081      IF ERCRTC-FILE-STATUS NOT = ZERO                             EL525
01082          MOVE 'ERROR OCCURED REWRITE - ERCRTC'                    EL525
01083                                  TO  WS-ABEND-MESSAGE             EL525
01084          MOVE ERCRTC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
01085          GO TO ABEND-PGM.                                         EL525
01086                                                                   EL525
01087      IF SLR-DATE-WRITTEN GREATER WS-DATE-WRITTEN                  EL525
01088          MOVE SLR-DATE-WRITTEN   TO  WS-DATE-WRITTEN              EL525
01089          MOVE ZERO               TO  WS-TIME-WRITTEN.             EL525
01090                                                                   EL525
01091  4300-CHECK-TIME-STAMP.                                           EL525
01092      IF SLR-TIME-WRITTEN NOT GREATER WS-TIME-WRITTEN              EL525
01093          ADD +1  TO  SLR-TIME-WRITTEN                             EL525
01094          GO TO 4300-CHECK-TIME-STAMP.                             EL525
01095                                                                   EL525
01096      MOVE SLR-TIME-WRITTEN       TO  WS-TIME-WRITTEN.             EL525
01097                                                                   EL525
01098      WRITE SYSTEM-LOG-RECORD.                                     EL525
01099                                                                   EL525
01100      ADD +1                      TO WS-ERCRTC-COUNT               EL525
01101                                     WS-TOTAL-COUNT.               EL525
01102                                                                   EL525
01103  4300-EXIT.                                                       EL525
01104      EXIT.                                                        EL525
01105      EJECT                                                        EL525
01106                                                                   EL525
01107  4400-PROCESS-ERPYAJ SECTION.                                     EL525
01108      MOVE EX-DATA-AREAS          TO PENDING-PAY-ADJ.              EL525
01109                                                                   EL525
01110      READ ERPYAJ.                                                 EL525
01111                                                                   EL525
01112      IF ERPYAJ-FILE-STATUS = '23'                                 EL525
01113          MOVE SPACES             TO  WS-DETAIL1                   EL525
01114          MOVE PY-CARRIER         TO  WS-PY-CARRIER                EL525
01115          MOVE PY-GROUPING        TO  WS-PY-GROUPING               EL525
01116          MOVE PY-FIN-RESP        TO  WS-PY-FIN-RESP               EL525
01117          MOVE PY-ACCOUNT         TO  WS-PY-ACCOUNT                EL525
01118          MOVE PY-FILE-SEQ-NO     TO  WS-PY-FILE-SEQ-NO            EL525
01119          MOVE PY-RECORD-TYPE     TO  WS-PY-RECORD-TYPE            EL525
01120          MOVE 'PAYMENTS AND ADJUSTMENTS RECORD MISSING'           EL525
01121                                  TO  WS-PY-MESSAGE                EL525
01122          MOVE WS-DETAIL1A        TO  PRT                          EL525
01123          PERFORM WRITE-A-LINE                                     EL525
01124          GO TO 4400-EXIT.                                         EL525
01125                                                                   EL525
01126      IF ERPYAJ-FILE-STATUS NOT = ZERO                             EL525
01127          MOVE 'ERROR OCCURED READ - ERPYAJ'                       EL525
01128                                  TO  WS-ABEND-MESSAGE             EL525
01129          MOVE ERPYAJ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
01130          GO TO ABEND-PGM.                                         EL525
01131                                                                   EL525
01132      MOVE WS-POST-DATE           TO  PY-CREDIT-ACCEPT-DT.         EL525
01133      MOVE WS-SAVE-DATE           TO  PY-LAST-MAINT-DT.            EL525
01134      MOVE WS-SAVE-TIME           TO  PY-LAST-MAINT-HHMMSS.        EL525
01135      MOVE WS-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.            EL525
01136                                                                   EL525
01137      MOVE 'C'                    TO  SLR-ACTION.                  EL525
01138      MOVE 'EL'                   TO  SLR-PREFIX.                  EL525
01139      MOVE ZERO                   TO  SLR-TASK-NUMBER.             EL525
01140      MOVE SPACES                 TO  SLR-TERM-ID.                 EL525
01141      MOVE 'E525'                 TO  SLR-TRAN-ID.                 EL525
01142                                                                   EL525
01143      MOVE 'ERPYAJ'               TO  SLR-DSID.                    EL525
01144                                                                   EL525
01145      MOVE SPACES                 TO  DC-ALPHA-CENTURY.            EL525
01146      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL525
01147      MOVE '2'                    TO  DC-OPTION-CODE.              EL525
01148      PERFORM 8500-DATE-CONVERSION.                                EL525
01149      MOVE DC-JULIAN-DATE-1       TO  SLR-DATE-WRITTEN.            EL525
01150                                                                   EL525
01151                                                                   EL525
01152      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL525
01153                                                                   EL525
01154      MOVE WS-TIME                TO  SLR-TIME-WRITTEN.            EL525
01155      MULTIPLY +10 BY SLR-TIME-WRITTEN.                            EL525
01156                                                                   EL525
01157      MOVE +33                    TO  SLR-KEY-LENGTH.              EL525
01158      MOVE SLR-KEY-LENGTH         TO  SLR-KEY-LENGTH.              EL525
01159      MOVE SPACES                 TO  SLR-KEY.                     EL525
01160                                                                   EL525
01161      MOVE +150                   TO  SLR-RECORD-LENGTH.           EL525
01162      MOVE SLR-RECORD-LENGTH      TO  SLR-RECORD-LENGTH.           EL525
01163      MOVE SPACES                 TO  SLR-RECORD-IMAGE.            EL525
01164                                                                   EL525
01165      MOVE PY-CONTROL-PRIMARY     TO  SLR-KEY.                     EL525
01166      MOVE PENDING-PAY-ADJ        TO  SLR-RECORD-IMAGE.            EL525
01167                                                                   EL525
01168      REWRITE PENDING-PAY-ADJ.                                     EL525
01169                                                                   EL525
01170      IF ERPYAJ-FILE-STATUS NOT = ZERO                             EL525
01171          MOVE 'ERROR OCCURED REWRITE - ERPYAJ'                    EL525
01172                                  TO  WS-ABEND-MESSAGE             EL525
01173          MOVE ERPYAJ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
01174          GO TO ABEND-PGM.                                         EL525
01175                                                                   EL525
01176      IF SLR-DATE-WRITTEN GREATER WS-DATE-WRITTEN                  EL525
01177          MOVE SLR-DATE-WRITTEN   TO  WS-DATE-WRITTEN              EL525
01178          MOVE ZERO               TO  WS-TIME-WRITTEN.             EL525
01179                                                                   EL525
01180  4400-CHECK-TIME-STAMP.                                           EL525
01181      IF SLR-TIME-WRITTEN NOT GREATER WS-TIME-WRITTEN              EL525
01182          ADD +1  TO  SLR-TIME-WRITTEN                             EL525
01183          GO TO 4400-CHECK-TIME-STAMP.                             EL525
01184                                                                   EL525
01185      MOVE SLR-TIME-WRITTEN       TO  WS-TIME-WRITTEN.             EL525
01186                                                                   EL525
01187      WRITE SYSTEM-LOG-RECORD.                                     EL525
01188                                                                   EL525
01189      ADD +1                      TO WS-ERPYAJ-COUNT               EL525
01190                                     WS-TOTAL-COUNT.               EL525
01191                                                                   EL525
01192  4400-EXIT.                                                       EL525
01193      EXIT.                                                        EL525
01194      EJECT                                                        EL525
01195                                                                   EL525
01196  4500-PROCESS-ERREPY SECTION.                                     EL525
01197      MOVE EX-DATA-AREAS         TO PENDING-RETRO-REIN-ADJUSTMENTS.EL525
01198                                                                   EL525
01199      READ ERREPY.                                                 EL525
01200                                                                   EL525
01201      IF ERREPY-FILE-STATUS = '23'                                 EL525
01202          MOVE SPACES             TO  WS-DETAIL1                   EL525
01203          MOVE RP-CARRIER         TO  WS-RP-CARRIER                EL525
01204          MOVE RP-GROUPING        TO  WS-RP-GROUPING               EL525
01205          MOVE RP-STATE           TO  WS-RP-STATE                  EL525
01206          MOVE RP-ACCOUNT         TO  WS-RP-ACCOUNT                EL525
01207          MOVE RP-FILE-SEQ-NO     TO  WS-RP-FILE-SEQ-NO            EL525
01208          MOVE RP-RECORD-TYPE     TO  WS-RP-RECORD-TYPE            EL525
01209          MOVE 'PAYMENTS AND ADJUSTMENTS RECORD MISSING'           EL525
01210                                  TO  WS-RP-MESSAGE                EL525
01211          MOVE WS-DETAIL1A        TO  PRT                          EL525
01212          PERFORM WRITE-A-LINE                                     EL525
01213          GO TO 4500-EXIT.                                         EL525
01214                                                                   EL525
01215      IF ERREPY-FILE-STATUS NOT = ZERO                             EL525
01216          MOVE 'ERROR OCCURED READ - ERREPY'                       EL525
01217                                  TO  WS-ABEND-MESSAGE             EL525
01218          MOVE ERREPY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
01219          GO TO ABEND-PGM.                                         EL525
01220                                                                   EL525
01221      MOVE WS-POST-DATE           TO  RP-CREDIT-ACCEPT-DT.         EL525
01222      MOVE WS-SAVE-DATE           TO  RP-LAST-MAINT-DT.            EL525
01223      MOVE WS-SAVE-TIME           TO  RP-LAST-MAINT-HHMMSS.        EL525
01224      MOVE WS-PROCESSOR-ID        TO  RP-LAST-MAINT-BY.            EL525
01225                                                                   EL525
01226      MOVE 'C'                    TO  SLR-ACTION.                  EL525
01227      MOVE 'EL'                   TO  SLR-PREFIX.                  EL525
01228      MOVE ZERO                   TO  SLR-TASK-NUMBER.             EL525
01229      MOVE SPACES                 TO  SLR-TERM-ID.                 EL525
01230      MOVE 'E525'                 TO  SLR-TRAN-ID.                 EL525
01231      MOVE 'ERREPY'               TO  SLR-DSID.                    EL525
01232      MOVE SPACES                 TO  DC-ALPHA-CENTURY.            EL525
01233      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL525
01234      MOVE '2'                    TO  DC-OPTION-CODE.              EL525
01235      PERFORM  8500-DATE-CONVERSION.                               EL525
01236      MOVE DC-JULIAN-DATE-1       TO  SLR-DATE-WRITTEN.            EL525
01237                                                                   EL525
01238                                                                   EL525
01239      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL525
01240                                                                   EL525
01241      MOVE WS-TIME                TO  SLR-TIME-WRITTEN.            EL525
01242      MULTIPLY +10 BY SLR-TIME-WRITTEN.                            EL525
01243      MOVE +25                    TO  SLR-KEY-LENGTH.              EL525
01244      MOVE SLR-KEY-LENGTH         TO  SLR-KEY-LENGTH.              EL525
01245      MOVE SPACES                 TO  SLR-KEY.                     EL525
01246      MOVE +200                   TO  SLR-RECORD-LENGTH.           EL525
01247      MOVE SLR-RECORD-LENGTH      TO  SLR-RECORD-LENGTH.           EL525
01248      MOVE SPACES                 TO  SLR-RECORD-IMAGE.            EL525
01249      MOVE RP-CONTROL-PRIMARY     TO  SLR-KEY.                     EL525
01250      MOVE PENDING-RETRO-REIN-ADJUSTMENTS TO SLR-RECORD-IMAGE.     EL525
01251                                                                   EL525
01252      REWRITE  PENDING-RETRO-REIN-ADJUSTMENTS.                     EL525
01253                                                                   EL525
01254      IF ERREPY-FILE-STATUS NOT = ZERO                             EL525
01255          MOVE 'ERROR OCCURED REWRITE - ERREPY'                    EL525
01256                                  TO  WS-ABEND-MESSAGE             EL525
01257          MOVE ERREPY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL525
01258          GO TO ABEND-PGM.                                         EL525
01259                                                                   EL525
01260      IF SLR-DATE-WRITTEN GREATER WS-DATE-WRITTEN                  EL525
01261          MOVE SLR-DATE-WRITTEN   TO  WS-DATE-WRITTEN              EL525
01262          MOVE ZERO               TO  WS-TIME-WRITTEN.             EL525
01263                                                                   EL525
01264  4500-CHECK-TIME-STAMP.                                           EL525
01265      IF SLR-TIME-WRITTEN NOT GREATER WS-TIME-WRITTEN              EL525
01266          ADD +1  TO  SLR-TIME-WRITTEN                             EL525
01267          GO TO 4500-CHECK-TIME-STAMP.                             EL525
01268                                                                   EL525
01269      MOVE SLR-TIME-WRITTEN       TO  WS-TIME-WRITTEN.             EL525
01270                                                                   EL525
01271      WRITE SYSTEM-LOG-RECORD.                                     EL525
01272                                                                   EL525
01273      ADD +1                      TO WS-ERREPY-COUNT               EL525
01274                                     WS-TOTAL-COUNT.               EL525
01275                                                                   EL525
01276  4500-EXIT.                                                       EL525
01277      EXIT.                                                        EL525
01278      EJECT                                                        EL525
01279                                                                   EL525
01280  4600-POST-REQUEST SECTION.                                       EL525
01281                                                                   EL525
01282      MOVE DTE-CLASIC-COMPANY-CD  TO RQ-COMPANY-CD.                EL525
01283      MOVE PB-ENTRY-BATCH         TO RQ-ENTRY-BATCH.               EL525
01284                                                                   EL525
01285      READ ERRQST.                                                 EL525
01286                                                                   EL525
01287      IF ERRQST-FILE-STATUS = '23' OR '10'                         EL525
01288          GO TO 4600-EXIT.                                         EL525
01289                                                                   EL525
01290      IF ERRQST-FILE-STATUS NOT = ZERO                             EL525
01291          MOVE 'ERROR OCCURED READ - ERRQST'                       EL525
01292                                   TO WS-ABEND-MESSAGE             EL525
01293          MOVE ERRQST-FILE-STATUS  TO WS-ABEND-FILE-STATUS         EL525
01294          GO TO ABEND-PGM.                                         EL525
01295                                                                   EL525
01296      MOVE WS-POST-DATE            TO RQ-CREDIT-ACCEPT-DT.         EL525
01297                                                                   EL525
01298      REWRITE AR-REQUEST-RECORD.                                   EL525
01299                                                                   EL525
01300      IF ERRQST-FILE-STATUS NOT = ZERO                             EL525
01301          MOVE 'ERROR OCCURED REWRITE - ERRQST'                    EL525
01302                                   TO WS-ABEND-MESSAGE             EL525
01303          MOVE ERRQST-FILE-STATUS  TO WS-ABEND-FILE-STATUS         EL525
01304          GO TO ABEND-PGM.                                         EL525
01305                                                                   EL525
01306      ADD +1                      TO WS-ERRQST-COUNT               EL525
01307                                     WS-TOTAL-COUNT.               EL525
01308                                                                   EL525
01309  4600-EXIT.                                                       EL525
01310      EXIT.                                                        EL525
01311      EJECT                                                        EL525
01312                                                                   EL525
01313  5000-BROWSE-BATCH-FOR-POST SECTION.                              EL525
01314      MOVE ZEROS                  TO PB-BATCH-SEQ-NO               EL525
01315                                     PB-BATCH-CHG-SEQ-NO.          EL525
01316                                                                   EL525
01317      START ERPNDB                                                 EL525
01318          KEY IS GREATER THAN PB-CONTROL-PRIMARY.                  EL525
01319                                                                   EL525
01320      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL525
01321          MOVE 'ERROR OCCURED DURING START - ERPNDB '              EL525
01322                                  TO WS-ABEND-MESSAGE              EL525
01323          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL525
01324          GO TO ABEND-PGM.                                         EL525
01325                                                                   EL525
01326      MOVE SPACE                  TO WS-BATCH-POST-SW.             EL525
01327                                                                   EL525
01328  5010-READ-NEXT.                                                  EL525
01329      READ ERPNDB NEXT RECORD.                                     EL525
01330                                                                   EL525
01331      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL525
01332          MOVE 'ERROR OCCURED DURING READ - ERPNDB '               EL525
01333                                  TO WS-ABEND-MESSAGE              EL525
01334          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL525
01335          GO TO ABEND-PGM.                                         EL525
01336                                                                   EL525
01337      IF PB-BATCH-TRAILER                                          EL525
01338          MOVE WS-BATCH-RECORD    TO PB-BATCH-RECORD               EL525
01339          IF ALL-RECORDS-POSTED                                    EL525
01340              MOVE WS-POST-DATE   TO PB-CREDIT-ACCEPT-DT           EL525
01341              IF CF-AR-SYSTEM-USED                                 EL525
01342                  PERFORM 4600-POST-REQUEST                        EL525
01343                  GO TO 5000-EXIT                                  EL525
01344              ELSE                                                 EL525
01345                  GO TO 5000-EXIT                                  EL525
01346          ELSE                                                     EL525
01347              GO TO 5000-EXIT.                                     EL525
01348                                                                   EL525
01349      IF PB-CREDIT-ACCEPT-DT = LOW-VALUES                          EL525
01350          MOVE 'X'                TO WS-BATCH-POST-SW.             EL525
01351                                                                   EL525
01352      GO TO 5010-READ-NEXT.                                        EL525
01353                                                                   EL525
01354  5000-EXIT.                                                       EL525
01355      EXIT.                                                        EL525
01356      EJECT                                                        EL525
01357                                                                   EL525
01358  7000-SEARCH-ERPNDB SECTION.                                      EL525
01359      MOVE 9999                   TO  PB-BATCH-SEQ-NO.             EL525
01360      MOVE ZEROS                  TO  PB-BATCH-CHG-SEQ-NO.         EL525
01361                                                                   EL525
01362      READ ERPNDB.                                                 EL525
01363                                                                   EL525
01364      IF ERPNDB-FILE-STATUS = '23'                                 EL525
01365          MOVE SPACES             TO  WS-DETAIL1                   EL525
01366          MOVE PB-ENTRY-BATCH     TO  WS-PB-ENTRY-BATCH            EL525
01367          MOVE PB-BATCH-SEQ-NO    TO  WS-PB-BATCH-SEQ-NO           EL525
01368          MOVE PB-BATCH-CHG-SEQ-NO TO WS-PB-BATCH-CHG-SEQ-NO       EL525
01369          MOVE 'PENDING BUSINESS BATCH RECORD MISSING'             EL525
01370                                  TO  WS-PB-MESSAGE                EL525
01371          MOVE WS-DETAIL1A        TO  PRT                          EL525
01372          PERFORM WRITE-A-LINE                                     EL525
01373          MOVE EX-DATA-AREAS      TO  PENDING-BUSINESS             EL525
01374          MOVE ZEROS              TO  WS-BATCH-RECORD              EL525
01375          MOVE HIGH-VALUES        TO  WS-BATCH-MISSING-SW          EL525
01376          GO TO 7000-EXIT.                                         EL525
01377                                                                   EL525
01378      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL525
01379          MOVE 'EL525 BATCH RECORD MISSING - ERPNDB'               EL525
01380                                  TO WS-ABEND-MESSAGE              EL525
01381          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL525
01382          GO TO ABEND-PGM.                                         EL525
01383                                                                   EL525
01384      MOVE LOW-VALUES             TO WS-BATCH-MISSING-SW.          EL525
01385      MOVE PB-BATCH-RECORD        TO WS-BATCH-RECORD.              EL525
01386      MOVE EX-DATA-AREAS          TO PENDING-BUSINESS.             EL525
01387                                                                   EL525
01388  7000-EXIT.                                                       EL525
01389      EXIT.                                                        EL525
01390      EJECT                                                        EL525
01391                                                                   EL525
01392  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL525
01393                                                                   EL525
01394      EJECT                                                        EL525
01395                                                                   EL525
01396  GET-DATE SECTION.                                                EL525
01397                                                                   EL525
01398  GDS-010.                                                         EL525
01399      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL525
01400      MOVE '2'                    TO  DC-OPTION-CODE.              EL525
01401      PERFORM 8500-DATE-CONVERSION.                                EL525
01402      MOVE DC-BIN-DATE-1          TO  WS-SAVE-DATE.                EL525
01403      MOVE BIN-RUN-DATE           TO  WS-POST-DATE.                EL525
01404                                                                   EL525
01405      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL525
01406                                                                   EL525
01407      MOVE WS-TIME                TO  WS-SAVE-TIME.                EL525
01408                                                                   EL525
01409  GDS-EXIT.                                                        EL525
01410      EXIT.                                                        EL525
01411                                                                   EL525
01412  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL525
01413                                                                   EL525
01414                                                                   EL525
01415  WRITE-HEADINGS SECTION.                                          EL525
01416 ***************************************************************** EL525
01417 *                                                               * EL525
01418 *                            ELCWHS1.                           * EL525
01419 *                            VMOD=2.001                         * EL525
01420 *                                                               * EL525
01421 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL525
01422 *****************************************************************.EL525
01423  WHS-010.                                                         EL525
01424      IF  WS-H2-DATE EQUAL SPACES                                  EL525
01425          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL525
01426          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL525
01427          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL525
01428                                                                   EL525
01429      ADD +1  TO  WS-PAGE.                                         EL525
01430      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL525
01431      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL525
01432      MOVE ZERO                   TO  WS-LINE-COUNT.               EL525
01433                                                                   EL525
01434      MOVE WS-HEADING1            TO  PRT.                         EL525
01435      MOVE '1'                    TO  X.                           EL525
01436      PERFORM WRITE-PRINTER.                                       EL525
01437                                                                   EL525
01438      MOVE WS-HEADING2            TO  PRT.                         EL525
01439      MOVE ' '                    TO  X.                           EL525
01440      PERFORM WRITE-PRINTER.                                       EL525
01441                                                                   EL525
01442      MOVE WS-HEADING3            TO  PRT.                         EL525
01443      MOVE ' '                    TO  X.                           EL525
01444      PERFORM WRITE-PRINTER.                                       EL525
01445                                                                   EL525
01446      MOVE WS-HEADING4            TO  PRT.                         EL525
01447      MOVE ' '                    TO  X.                           EL525
01448      PERFORM WRITE-PRINTER.                                       EL525
01449                                                                   EL525
01450                                                                   EL525
01451       MOVE +10                   TO  WS-LINE-COUNT.               EL525
01452                                                                   EL525
01453  WHS-020. COPY ELCWHS2.                                           EL525
01454                                                                   EL525
01455  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL525
01456                                                                   EL525
01457      MOVE P-CTL TO LCP-ASA                                        EL525
01458      PERFORM LCP-WRITE-POS-PRT                                    EL525
01459          THRU LCP-WRITE-END-PRT.                                  EL525
01460                                                                   EL525
01461  WPS-EXIT.                                                        EL525
01462      EXIT.                                                        EL525
01463                                                                   EL525
01464      EJECT                                                        EL525
01465  OPEN-FILES SECTION.                                              EL525
01466  OFS-010.                                                         EL525
01467      OPEN INPUT  ELCNTL                                           EL525
01468                  EXTRACT-INTERFACE-FILE                           EL525
01469           I-O    ERPNDB                                           EL525
01470                  ERPNDC                                           EL525
01471                  ERCRTC                                           EL525
01472                  ERPYAJ                                           EL525
01473                  ERREPY                                           EL525
01474           OUTPUT JOURNAL-LOG-FILE                                 EL525
062104                 ME50-EL525-BALANCE
01475                  PRNTR.                                           EL525
01476                                                                   EL525
01477      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL525
01478          NEXT SENTENCE                                            EL525
01479        ELSE                                                       EL525
01480          MOVE 'ELCNTL'              TO  WS-FEM-FILE-NAME          EL525
01481          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01482          MOVE ELCNTL-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01483          GO TO ABEND-PGM.                                         EL525
01484                                                                   EL525
01485      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL525
01486          NEXT SENTENCE                                            EL525
01487        ELSE                                                       EL525
01488          MOVE 'ERPNDB'              TO  WS-FEM-FILE-NAME          EL525
01489          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01490          MOVE ERPNDB-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01491          GO TO ABEND-PGM.                                         EL525
01492                                                                   EL525
01493      IF ERPNDC-FILE-STATUS  = '00' OR '97'                        EL525
01494          NEXT SENTENCE                                            EL525
01495        ELSE                                                       EL525
01496          MOVE 'ERPNDC'              TO  WS-FEM-FILE-NAME          EL525
01497          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01498          MOVE ERPNDC-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01499          GO TO ABEND-PGM.                                         EL525
01500                                                                   EL525
01501      IF ERCRTC-FILE-STATUS  = '00' OR '97'                        EL525
01502          NEXT SENTENCE                                            EL525
01503        ELSE                                                       EL525
01504          MOVE 'ERCRTC'              TO  WS-FEM-FILE-NAME          EL525
01505          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01506          MOVE ERCRTC-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01507          GO TO ABEND-PGM.                                         EL525
01508                                                                   EL525
01509      IF ERPYAJ-FILE-STATUS  = '00' OR '97'                        EL525
01510          NEXT SENTENCE                                            EL525
01511        ELSE                                                       EL525
01512          MOVE 'ERPYAJ'              TO  WS-FEM-FILE-NAME          EL525
01513          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01514          MOVE ERPYAJ-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01515          GO TO ABEND-PGM.                                         EL525
01516                                                                   EL525
01517      IF ERREPY-FILE-STATUS  = '00' OR '97'                        EL525
01518          NEXT SENTENCE                                            EL525
01519        ELSE                                                       EL525
01520          MOVE 'ERREPY'              TO  WS-FEM-FILE-NAME          EL525
01521          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01522          MOVE ERREPY-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01523          GO TO ABEND-PGM.                                         EL525
01524                                                                   EL525
01525  OFS-EXIT.                                                        EL525
01526      EXIT.                                                        EL525
01527      EJECT                                                        EL525
01528  CLOSE-FILES SECTION.                                             EL525
01529  CFS-010.                                                         EL525
01530      CLOSE  ELCNTL                                                EL525
01531             ERPNDB                                                EL525
01532             ERPNDC                                                EL525
01533             ERCRTC                                                EL525
01534             ERPYAJ                                                EL525
01535             ERREPY                                                EL525
01536             JOURNAL-LOG-FILE                                      EL525
062104            ME50-EL525-BALANCE
01537             PRNTR.                                                EL525
01538                                                                   EL525
01539      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL525
01540          MOVE 'ELCNTL'              TO  WS-FEM-FILE-NAME          EL525
01541          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01542          MOVE ELCNTL-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01543          GO TO ABEND-PGM.                                         EL525
01544                                                                   EL525
01545      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL525
01546          MOVE 'ERPNDB'              TO  WS-FEM-FILE-NAME          EL525
01547          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01548          MOVE ERPNDB-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01549          GO TO ABEND-PGM.                                         EL525
01550                                                                   EL525
01551      IF ERPNDC-FILE-STATUS NOT = ZERO                             EL525
01552          MOVE 'ERPNDC'              TO  WS-FEM-FILE-NAME          EL525
01553          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01554          MOVE ERPNDC-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01555          GO TO ABEND-PGM.                                         EL525
01556                                                                   EL525
01557      IF ERCRTC-FILE-STATUS NOT = ZERO                             EL525
01558          MOVE 'ERCRTC'              TO  WS-FEM-FILE-NAME          EL525
01559          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01560          MOVE ERCRTC-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01561          GO TO ABEND-PGM.                                         EL525
01562                                                                   EL525
01563      IF ERPYAJ-FILE-STATUS NOT = ZERO                             EL525
01564          MOVE 'ERPYAJ'              TO  WS-FEM-FILE-NAME          EL525
01565          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01566          MOVE ERPYAJ-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01567          GO TO ABEND-PGM.                                         EL525
01568                                                                   EL525
01569      IF ERREPY-FILE-STATUS NOT = ZERO                             EL525
01570          MOVE 'ERREPY'              TO  WS-FEM-FILE-NAME          EL525
01571          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL525
01572          MOVE ERREPY-FILE-STATUS    TO  WS-ABEND-FILE-STATUS      EL525
01573          GO TO ABEND-PGM.                                         EL525
01574                                                                   EL525
01575  CFS-EXIT.                                                        EL525
01576      EXIT.                                                        EL525
01577                                                                   EL525
01578                                                                   EL525
01579  ABEND-PGM SECTION. COPY ELCABEND SUPPRESS.                       EL525
01580 /                                                                 EL525
01581  LCP-WRITE-POS-PRT SECTION.                                       EL525
01582      IF LCP-ASA = '+'                                             EL525
01583          WRITE PRT AFTER 0 LINE                                   EL525
01584      ELSE                                                         EL525
01585      IF LCP-ASA = ' '                                             EL525
01586          WRITE PRT AFTER ADVANCING 1 LINE                         EL525
01587      ELSE                                                         EL525
01588      IF LCP-ASA = '0'                                             EL525
01589          WRITE PRT AFTER ADVANCING 2 LINE                         EL525
01590      ELSE                                                         EL525
01591      IF LCP-ASA = '-'                                             EL525
01592          WRITE PRT AFTER ADVANCING 3 LINE                         EL525
01593      ELSE                                                         EL525
01594      IF LCP-ASA = '1'                                             EL525
01595          WRITE PRT AFTER ADVANCING PAGE                           EL525
01596      ELSE                                                         EL525
01597      IF LCP-ASA = '2'                                             EL525
01598          WRITE PRT AFTER ADVANCING LCP-CH2                        EL525
01599      ELSE                                                         EL525
01600      IF LCP-ASA = '3'                                             EL525
01601          WRITE PRT AFTER ADVANCING LCP-CH3                        EL525
01602      ELSE                                                         EL525
01603      IF LCP-ASA = '4'                                             EL525
01604          WRITE PRT AFTER ADVANCING LCP-CH4                        EL525
01605      ELSE                                                         EL525
01606      IF LCP-ASA = '5'                                             EL525
01607          WRITE PRT AFTER ADVANCING LCP-CH5                        EL525
01608      ELSE                                                         EL525
01609      IF LCP-ASA = '6'                                             EL525
01610          WRITE PRT AFTER ADVANCING LCP-CH6                        EL525
01611      ELSE                                                         EL525
01612      IF LCP-ASA = '7'                                             EL525
01613          WRITE PRT AFTER ADVANCING LCP-CH7                        EL525
01614      ELSE                                                         EL525
01615      IF LCP-ASA = '8'                                             EL525
01616          WRITE PRT AFTER ADVANCING LCP-CH8                        EL525
01617      ELSE                                                         EL525
01618      IF LCP-ASA = '9'                                             EL525
01619          WRITE PRT AFTER ADVANCING LCP-CH9                        EL525
01620      ELSE                                                         EL525
01621      IF LCP-ASA = 'A'                                             EL525
01622          WRITE PRT AFTER ADVANCING LCP-CH10                       EL525
01623      ELSE                                                         EL525
01624      IF LCP-ASA = 'B'                                             EL525
01625          WRITE PRT AFTER ADVANCING LCP-CH11                       EL525
01626      ELSE                                                         EL525
01627      IF LCP-ASA = 'C'                                             EL525
01628          WRITE PRT AFTER ADVANCING LCP-CH12                       EL525
01629      ELSE                                                         EL525
01630      IF LCP-ASA = 'V'                                             EL525
01631          WRITE PRT AFTER ADVANCING LCP-P01                        EL525
01632      ELSE                                                         EL525
01633      IF LCP-ASA = 'W'                                             EL525
01634          WRITE PRT AFTER ADVANCING LCP-P02                        EL525
01635      ELSE                                                         EL525
01636      DISPLAY 'ASA CODE ERROR'.                                    EL525
01637  LCP-WRITE-END-PRT.                                               EL525
01638      EXIT.                                                        EL525
01639                                                                   EL525
